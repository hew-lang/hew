use std::collections::{BTreeMap, BTreeSet, HashMap};

use hew_parser::ast::{
    ActorDecl, BinaryOp, Block as AstBlock, CallArg, CompoundAssignOp, ElseBlock, Expr, FnDecl,
    Item, Literal, MatchArm, Pattern, Program, ReceiveFnDecl, Spanned, Stmt, TypeBodyItem,
    TypeDeclKind, VariantKind,
};
use hew_types::check::{SpanKey, TypeDefKind, VariantDef};
use hew_types::{BuiltinType, Ty};
use serde_json::{json, Value};
use sha2::{Digest, Sha256};

use crate::bytecode::{
    ActorLayout, Block, Capability, EnumLayout, FieldLayout, Function, HandlerLayout, ImportEdge,
    Instruction, Layouts, Local, MachineLayout, MachineTransition, Module, ModuleGraph, Operand,
    RecordLayout, SandboxBytecodePackage, SourceFile, SourceMap, SourcePosition, SpanEntry,
    StdlibSymbol, SupervisorChildSpec, SupervisorLayout, SupervisorStartSpec, Terminator,
    TypeLayout, VariantLayout,
};
use crate::CompileError;

const SCHEMA_VERSION: &str = "hew.sandbox.bytecode.v0";
const ROOT_SOURCE_ID: &str = "src:main";
const ROOT_MODULE_ID: &str = "mod:main";

type MatchBlock = (usize, String);
type MatchGuardBlock = Option<MatchBlock>;
type MatchBlocks = (Vec<MatchBlock>, Vec<MatchGuardBlock>, Vec<MatchBlock>);

pub fn emit_package(
    source: &str,
    profile: &str,
    program: &Program,
    type_output: &hew_types::TypeCheckOutput,
) -> Result<SandboxBytecodePackage, CompileError> {
    let mut emitter = PackageEmitter::new(source, profile, type_output);
    emitter.emit(program)
}

#[derive(Debug)]
struct PackageEmitter<'a> {
    profile: &'a str,
    type_output: &'a hew_types::TypeCheckOutput,
    spans: SpanInterner<'a>,
    type_layouts: BTreeMap<String, TypeLayout>,
    record_layouts: BTreeMap<String, RecordLayout>,
    enum_layouts: BTreeMap<String, EnumLayout>,
    stdlib_symbols: BTreeMap<String, StdlibSymbol>,
    capabilities: BTreeMap<String, Capability>,
    record_field_indexes: BTreeMap<String, BTreeMap<String, usize>>,
    enum_variant_tags: BTreeMap<String, (String, usize, Vec<Ty>)>,
    const_literals: BTreeMap<String, Literal>,
    user_functions: BTreeSet<String>,
    /// Actor descriptor tables keyed by the actor's `type:` id, populated in
    /// `collect_layouts` and serialized into `layouts.actors`.
    actor_layouts: BTreeMap<String, ActorLayout>,
    /// Actor name -> declared state-field names in order, used to order
    /// `spawn Actor(field: value)` arguments by the actor's field layout.
    actor_field_order: BTreeMap<String, Vec<String>>,
    /// Supervisor descriptor tables keyed by the supervisor's `type:` id,
    /// serialized into `layouts.supervisors`.
    supervisor_layouts: BTreeMap<String, SupervisorLayout>,
    /// Declared supervisor names, used to route `spawn Name` to a
    /// `supervisor.spawn` and child field access to `supervisor.child`.
    supervisor_names: BTreeSet<String>,
    /// Machine descriptor tables keyed by the machine's `type:` id, serialized
    /// into `layouts.machines`.
    machine_layouts: BTreeMap<String, MachineLayout>,
    /// Declared machine names, used to route `Machine::State` literals and
    /// `.step`/`.state_name` method calls to the machine ops.
    machine_names: BTreeSet<String>,
}

impl<'a> PackageEmitter<'a> {
    fn new(source: &'a str, profile: &'a str, type_output: &'a hew_types::TypeCheckOutput) -> Self {
        Self {
            profile,
            type_output,
            spans: SpanInterner::new(source),
            type_layouts: BTreeMap::new(),
            record_layouts: BTreeMap::new(),
            enum_layouts: BTreeMap::new(),
            stdlib_symbols: BTreeMap::new(),
            capabilities: BTreeMap::new(),
            record_field_indexes: BTreeMap::new(),
            enum_variant_tags: BTreeMap::new(),
            const_literals: BTreeMap::new(),
            user_functions: BTreeSet::new(),
            actor_layouts: BTreeMap::new(),
            actor_field_order: BTreeMap::new(),
            supervisor_layouts: BTreeMap::new(),
            supervisor_names: BTreeSet::new(),
            machine_layouts: BTreeMap::new(),
            machine_names: BTreeSet::new(),
        }
    }

    fn emit(&mut self, program: &Program) -> Result<SandboxBytecodePackage, CompileError> {
        self.collect_user_functions(program);
        self.collect_const_literals(program);
        self.collect_layouts(program);
        self.ensure_core_types();

        let mut functions = Vec::new();
        for (item, span) in &program.items {
            match item {
                Item::Function(function) => {
                    functions.push(self.emit_function(function, span.clone())?);
                }
                Item::Actor(actor) => {
                    // Each receive handler becomes a callable bytecode function;
                    // the actor scheduler invokes it by the id recorded in the
                    // actor layout's handler table.
                    for receive_fn in &actor.receive_fns {
                        functions.push(self.emit_handler(actor, receive_fn, span.clone())?);
                    }
                }
                _ => {}
            }
        }

        if functions.is_empty() {
            return Err(CompileError {
                message: "sandbox bytecode packages require at least one function".to_string(),
            });
        }

        functions.sort_by(|left, right| left.id.cmp(&right.id));
        let function_ids = functions
            .iter()
            .map(|function| function.id.clone())
            .collect();
        let imports = collect_import_edges(program);
        let source_map = self.spans.source_map();
        let module_graph = ModuleGraph {
            entry: ROOT_MODULE_ID.to_string(),
            modules: vec![Module {
                id: ROOT_MODULE_ID.to_string(),
                path: "main".to_string(),
                source_id: ROOT_SOURCE_ID.to_string(),
                imports,
                functions: function_ids,
            }],
        };
        let layouts = Layouts {
            types: self.type_layouts.values().cloned().collect(),
            records: self.record_layouts.values().cloned().collect(),
            enums: self.enum_layouts.values().cloned().collect(),
            actors: self.actor_layouts.values().cloned().collect(),
            supervisors: self.supervisor_layouts.values().cloned().collect(),
            machines: self.machine_layouts.values().cloned().collect(),
        };
        let stdlib_symbols: Vec<_> = self.stdlib_symbols.values().cloned().collect();
        let capabilities: Vec<_> = self.capabilities.values().cloned().collect();
        let digest = package_digest(
            self.profile,
            &source_map,
            &module_graph,
            &layouts,
            &stdlib_symbols,
            &capabilities,
            &functions,
        )?;

        Ok(SandboxBytecodePackage {
            schema_version: SCHEMA_VERSION.to_string(),
            package_id: format!("pkg:{digest}"),
            hew_version: env!("CARGO_PKG_VERSION").to_string(),
            compiler_version: format!("hew-sandbox-wasm-{}", env!("CARGO_PKG_VERSION")),
            profile: self.profile.to_string(),
            source_map,
            module_graph,
            layouts,
            stdlib_symbols,
            capabilities,
            functions,
        })
    }

    fn collect_user_functions(&mut self, program: &Program) {
        for (item, _) in &program.items {
            if let Item::Function(function) = item {
                self.user_functions.insert(function.name.clone());
            }
        }
    }

    fn collect_const_literals(&mut self, program: &Program) {
        for (item, _) in &program.items {
            if let Item::Const(const_decl) = item {
                if let Some(literal) = const_literal(&const_decl.value.0) {
                    self.const_literals.insert(const_decl.name.clone(), literal);
                }
            }
        }
    }

    #[expect(
        clippy::too_many_lines,
        reason = "layout collection mirrors the bytecode schema families in one deterministic pass"
    )]
    fn collect_layouts(&mut self, program: &Program) {
        for ty in [
            Ty::Unit,
            Ty::Bool,
            Ty::I64,
            Ty::U64,
            Ty::F64,
            Ty::String,
            Ty::Char,
            Ty::Duration,
            Ty::Never,
        ] {
            self.type_id_for_ty(&ty);
        }

        for (item, _) in &program.items {
            if let Item::TypeDecl(type_decl) = item {
                let type_id = self.type_id_for_named(&type_decl.name, &[]);
                match type_decl.kind {
                    TypeDeclKind::Struct => {
                        let mut fields = Vec::new();
                        let mut indexes = BTreeMap::new();
                        for body_item in &type_decl.body {
                            if let TypeBodyItem::Field { name, ty, .. } = body_item {
                                let index = fields.len();
                                indexes.insert(name.clone(), index);
                                fields.push(FieldLayout {
                                    name: name.clone(),
                                    ty: self.type_id_for_type_expr(&ty.0),
                                    index,
                                });
                            }
                        }
                        self.record_field_indexes
                            .insert(type_decl.name.clone(), indexes);
                        self.record_layouts.insert(
                            type_id.clone(),
                            RecordLayout {
                                id: type_id.clone(),
                                name: type_decl.name.clone(),
                                fields,
                            },
                        );
                        self.type_layouts.insert(
                            type_id.clone(),
                            TypeLayout {
                                id: type_id,
                                kind: "record".to_string(),
                                name: type_decl.name.clone(),
                                parameters: Vec::new(),
                            },
                        );
                    }
                    TypeDeclKind::Enum => {
                        let mut variants = Vec::new();
                        for body_item in &type_decl.body {
                            if let TypeBodyItem::Variant(variant) = body_item {
                                let payload = match &variant.kind {
                                    VariantKind::Unit => Vec::new(),
                                    VariantKind::Tuple(items) => items
                                        .iter()
                                        .map(|(ty, _)| self.type_id_for_type_expr(ty))
                                        .collect(),
                                    VariantKind::Struct(fields) => fields
                                        .iter()
                                        .map(|(_, (ty, _))| self.type_id_for_type_expr(ty))
                                        .collect(),
                                };
                                let tag = variants.len();
                                let payload_tys = match self
                                    .type_output
                                    .type_defs
                                    .get(&type_decl.name)
                                    .and_then(|def| def.variants.get(&variant.name))
                                {
                                    Some(VariantDef::Tuple(items)) => items.clone(),
                                    Some(VariantDef::Struct(fields)) => {
                                        fields.iter().map(|(_, ty)| ty.clone()).collect()
                                    }
                                    Some(VariantDef::Unit) | None => Vec::new(),
                                };
                                self.enum_variant_tags.insert(
                                    variant.name.clone(),
                                    (type_id.clone(), tag, payload_tys.clone()),
                                );
                                // Also register the qualified form (`TypeName::VariantName`) so
                                // that source code using `Colour::Red` in expression position
                                // (parsed as `Expr::Identifier("Colour::Red")`) is recognised
                                // alongside the bare `Red` form used in pattern arms and
                                // same-namespace calls.
                                self.enum_variant_tags.insert(
                                    format!("{}::{}", type_decl.name, variant.name),
                                    (type_id.clone(), tag, payload_tys),
                                );
                                variants.push(VariantLayout {
                                    name: variant.name.clone(),
                                    tag,
                                    payload,
                                });
                            }
                        }
                        self.enum_layouts.insert(
                            type_id.clone(),
                            EnumLayout {
                                id: type_id.clone(),
                                name: type_decl.name.clone(),
                                variants,
                            },
                        );
                        self.type_layouts.insert(
                            type_id.clone(),
                            TypeLayout {
                                id: type_id,
                                kind: "enum".to_string(),
                                name: type_decl.name.clone(),
                                parameters: Vec::new(),
                            },
                        );
                    }
                }
            }
        }

        for (name, type_def) in &self.type_output.type_defs {
            // Suppress emission for monomorphic builtin enums (e.g. `LookupError`)
            // that were pre-registered into `type_defs` from `std/builtins.hew`
            // but were not authored by the user. Their machine layout is
            // registered out-of-band via MIR's
            // `register_builtin_monomorphic_enum_layouts` and codegen's
            // builtin-enum-layout pass, so they do not need to appear in
            // per-program bytecode descriptor tables. If the program actually
            // references the type, the on-demand `type_id_for_named` path
            // elsewhere in emit still emits a layout entry.
            if self.type_output.internal_builtin_enum_names.contains(name) {
                continue;
            }
            let type_id = self.type_id_for_named(name, &[]);
            match type_def.kind {
                TypeDefKind::Struct | TypeDefKind::Record => {
                    self.type_layouts
                        .entry(type_id.clone())
                        .or_insert(TypeLayout {
                            id: type_id,
                            kind: "record".to_string(),
                            name: name.clone(),
                            parameters: Vec::new(),
                        });
                }
                TypeDefKind::Enum => {
                    self.type_layouts
                        .entry(type_id.clone())
                        .or_insert(TypeLayout {
                            id: type_id,
                            kind: "enum".to_string(),
                            name: name.clone(),
                            parameters: Vec::new(),
                        });
                }
                TypeDefKind::Actor | TypeDefKind::Machine => {}
            }
        }

        for (item, _) in &program.items {
            if let Item::Actor(actor) = item {
                self.collect_actor_layout(actor);
            }
        }
        // Supervisors are collected after actors so their child specs can read
        // each actor's field order when baking initial state.
        for (item, _) in &program.items {
            if let Item::Supervisor(supervisor) = item {
                self.collect_supervisor_layout(supervisor);
            }
        }
        for (item, _) in &program.items {
            if let Item::Machine(machine) = item {
                self.collect_machine_layout(machine);
            }
        }
    }

    fn collect_machine_layout(&mut self, machine: &hew_parser::ast::MachineDecl) {
        let type_id = self.type_id_for_named(&machine.name, &[]);
        self.type_layouts.insert(
            type_id.clone(),
            TypeLayout {
                id: type_id.clone(),
                kind: "machine".to_string(),
                name: machine.name.clone(),
                parameters: Vec::new(),
            },
        );

        let states = machine.states.iter().map(|s| s.name.clone()).collect();
        let events = machine.events.iter().map(|e| e.name.clone()).collect();
        let transitions = machine
            .transitions
            .iter()
            .map(|t| MachineTransition {
                event: t.event_name.clone(),
                from: t.source_state.clone(),
                to: t.target_state.clone(),
                span: None,
            })
            .collect();

        self.machine_names.insert(machine.name.clone());
        self.machine_layouts.insert(
            type_id.clone(),
            MachineLayout {
                id: type_id,
                name: machine.name.clone(),
                states,
                events,
                transitions,
            },
        );
    }

    fn collect_supervisor_layout(&mut self, supervisor: &hew_parser::ast::SupervisorDecl) {
        use hew_parser::ast::{RestartPolicy, SupervisorStrategy};

        let type_id = self.type_id_for_named(&supervisor.name, &[]);
        self.type_layouts.insert(
            type_id.clone(),
            TypeLayout {
                id: type_id.clone(),
                kind: "supervisor".to_string(),
                name: supervisor.name.clone(),
                parameters: Vec::new(),
            },
        );

        let strategy = match supervisor.strategy {
            Some(SupervisorStrategy::OneForAll) => "one_for_all",
            Some(SupervisorStrategy::RestForOne) => "rest_for_one",
            // The educational scheduler models the three static strategies;
            // simple_one_for_one (dynamic pools) falls back to one_for_one.
            _ => "one_for_one",
        }
        .to_string();

        let (restart_intensity, restart_window_ms) =
            supervisor
                .intensity
                .as_ref()
                .map_or((1, 60_000), |intensity| {
                    (
                        u32::try_from(intensity.restarts.max(0)).unwrap_or(u32::MAX),
                        parse_duration_ms(&intensity.window),
                    )
                });

        let children = supervisor
            .children
            .iter()
            .map(|child| {
                let actor_type_id = self.type_id_for_named(&child.actor_type, &[]);
                let field_order = self
                    .actor_field_order
                    .get(&child.actor_type)
                    .cloned()
                    .unwrap_or_default();
                let mut value_by_field = BTreeMap::new();
                for (field_name, value) in &child.args {
                    if let Some(literal) = literal_json(&value.0) {
                        value_by_field.insert(field_name.clone(), literal);
                    }
                }
                let args = field_order
                    .iter()
                    .map(|field_name| {
                        value_by_field
                            .get(field_name)
                            .cloned()
                            .unwrap_or(Value::Null)
                    })
                    .collect();
                let restart = match child.restart {
                    Some(RestartPolicy::Transient) => "transient",
                    Some(RestartPolicy::Temporary) => "temporary",
                    _ => "permanent",
                }
                .to_string();
                SupervisorChildSpec {
                    id: child.name.clone(),
                    restart,
                    start_spec: SupervisorStartSpec {
                        actor: actor_type_id,
                        args,
                    },
                }
            })
            .collect();

        self.supervisor_names.insert(supervisor.name.clone());
        self.supervisor_layouts.insert(
            type_id.clone(),
            SupervisorLayout {
                id: type_id,
                name: supervisor.name.clone(),
                strategy,
                restart_intensity,
                restart_window_ms,
                children,
            },
        );
    }

    fn collect_actor_layout(&mut self, actor: &ActorDecl) {
        let type_id = self.type_id_for_named(&actor.name, &[]);
        // Register (or overwrite) the actor's type as kind "actor" so it is not
        // mistaken for a record by the on-demand `type_id_for_ty` path that runs
        // later when lowering `spawn` destinations.
        self.type_layouts.insert(
            type_id.clone(),
            TypeLayout {
                id: type_id.clone(),
                kind: "actor".to_string(),
                name: actor.name.clone(),
                parameters: Vec::new(),
            },
        );

        let mut state_fields = Vec::new();
        let mut field_order = Vec::new();
        for (index, field) in actor.fields.iter().enumerate() {
            let field_ty = ty_from_type_expr(&field.ty.0);
            state_fields.push(FieldLayout {
                name: field.name.clone(),
                ty: self.type_id_for_ty(&field_ty),
                index,
            });
            field_order.push(field.name.clone());
        }

        let handlers = actor
            .receive_fns
            .iter()
            .map(|receive_fn| HandlerLayout {
                name: receive_fn.name.clone(),
                function: handler_function_id(&actor.name, &receive_fn.name),
            })
            .collect();

        self.actor_field_order
            .insert(actor.name.clone(), field_order);
        self.actor_layouts.insert(
            type_id.clone(),
            ActorLayout {
                id: type_id,
                name: actor.name.clone(),
                state_fields,
                handlers,
            },
        );
    }

    fn ensure_core_types(&mut self) {
        // The reply token threaded into every handler as its first parameter.
        self.type_layouts
            .entry("type:reply".to_string())
            .or_insert(TypeLayout {
                id: "type:reply".to_string(),
                kind: "opaque".to_string(),
                name: "ReplyToken".to_string(),
                parameters: Vec::new(),
            });
        self.type_layouts
            .entry("type:regex".to_string())
            .or_insert(TypeLayout {
                id: "type:regex".to_string(),
                kind: "regex".to_string(),
                name: "regex".to_string(),
                parameters: Vec::new(),
            });
    }

    fn emit_function(
        &mut self,
        function: &FnDecl,
        span: std::ops::Range<usize>,
    ) -> Result<Function, CompileError> {
        let function_id = function_id(&function.name);
        let sig = self.type_output.fn_sigs.get(&function.name);
        let result_ty = sig
            .map(|sig| sig.return_type.clone())
            .or_else(|| {
                function
                    .return_type
                    .as_ref()
                    .map(|(ty, _)| ty_from_type_expr(ty))
            })
            .unwrap_or(Ty::Unit);
        let result = self.type_id_for_ty(&result_ty);

        let mut ctx = FunctionEmitter::new(self, &function.name, result_ty.clone());
        let mut params = Vec::new();
        for (idx, param) in function.params.iter().enumerate() {
            let param_ty = sig
                .and_then(|sig| sig.params.get(idx).cloned())
                .unwrap_or_else(|| ty_from_type_expr(&param.ty.0));
            let local = ctx.declare_local(
                Some(param.name.clone()),
                &param_ty,
                false,
                Some(param.ty.1.clone()),
            );
            ctx.bindings.insert(param.name.clone(), local.clone());
            params.push(local);
        }

        let trailing_val = ctx.lower_block(&function.body)?;
        if !ctx.current_is_terminated() {
            let span_ref = ctx.package.spans.span_ref(&span);
            if let Some(val) = trailing_val {
                ctx.terminate(Terminator::ret(vec![Operand::local(val)], span_ref));
            } else if result_ty == Ty::Unit {
                let unit = ctx.emit_const_unit(Some(span.clone()));
                ctx.terminate(Terminator::ret(vec![Operand::local(unit)], span_ref));
            } else {
                ctx.terminate(Terminator::trap("internal_error", span_ref));
            }
        }

        let locals = std::mem::take(&mut ctx.locals);
        let blocks = ctx.finish();

        Ok(Function {
            id: function_id,
            module: ROOT_MODULE_ID.to_string(),
            name: function.name.clone(),
            params,
            result,
            locals,
            blocks,
            span: self.spans.span_ref(&span),
        })
    }

    fn emit_handler(
        &mut self,
        actor: &ActorDecl,
        receive_fn: &ReceiveFnDecl,
        span: std::ops::Range<usize>,
    ) -> Result<Function, CompileError> {
        let function_name = format!("{}.{}", actor.name, receive_fn.name);
        let function_id = handler_function_id(&actor.name, &receive_fn.name);
        let result_ty = receive_fn
            .return_type
            .as_ref()
            .map_or(Ty::Unit, |(ty, _)| ty_from_type_expr(ty));
        let result = self.type_id_for_ty(&result_ty);

        let mut ctx = FunctionEmitter::new(self, &function_name, result_ty.clone());
        let mut params = Vec::new();

        // The actor scheduler invokes a handler with the argument vector
        // `[reply_token, ...state_fields, ...message_params]`. State fields are
        // mutable locals so `field = expr` assignments lower to `local.set`.
        let reply_local = ctx.declare_local_with_type_id(Some("reply".to_string()), "type:reply");
        params.push(reply_local.clone());

        let mut state_locals = Vec::new();
        for field in &actor.fields {
            let field_ty = ty_from_type_expr(&field.ty.0);
            let local = ctx.declare_local(
                Some(field.name.clone()),
                &field_ty,
                true,
                Some(field.ty.1.clone()),
            );
            ctx.bindings.insert(field.name.clone(), local.clone());
            params.push(local.clone());
            state_locals.push(local);
        }

        for param in &receive_fn.params {
            let param_ty = ty_from_type_expr(&param.ty.0);
            let local = ctx.declare_local(
                Some(param.name.clone()),
                &param_ty,
                false,
                Some(param.ty.1.clone()),
            );
            ctx.bindings.insert(param.name.clone(), local.clone());
            params.push(local);
        }

        // Thread reply token and state locals into the emitter context so that
        // early `return` statements (Stmt::Return) can mirror the normal-exit
        // lowering: emit actor.reply(token, expr) then ret(<state record>).
        ctx.receive_context = Some(ReceiveHandlerContext {
            reply_local: reply_local.clone(),
            state_locals: state_locals.clone(),
            actor_name: actor.name.clone(),
        });

        let reply_value = ctx.lower_block(&receive_fn.body)?;

        // A handler that already terminated (e.g. an early `return`) keeps its
        // own terminator; the implicit reply/return below is the common path for
        // the straight-line handlers the sandbox profile admits.
        if !ctx.current_is_terminated() {
            let span_ref = ctx.package.spans.span_ref(&span);
            if let Some(reply_value) = reply_value {
                ctx.emit_instruction(
                    "actor.reply",
                    None,
                    vec![
                        Operand::local(reply_local.clone()),
                        Operand::local(reply_value),
                    ],
                    None,
                    None,
                );
            }
            // The scheduler derives the actor's next state from this return:
            // the bare value for a single field, a record for several. Handlers
            // with no state return unit (a no-op state update).
            let return_operand = match state_locals.as_slice() {
                [] => Operand::local(ctx.emit_const_unit(Some(span.clone()))),
                [single] => Operand::local(single.clone()),
                fields => {
                    let record_ty = ctx.package.actor_state_record_type_id(&actor.name);
                    let mut operands = vec![Operand::ty(record_ty)];
                    operands.extend(fields.iter().map(|local| Operand::local(local.clone())));
                    let dst = ctx.temp_local(&Ty::Unit, Some(span.clone()));
                    ctx.emit_instruction("record.new", Some(dst.clone()), operands, None, None);
                    Operand::local(dst)
                }
            };
            ctx.terminate(Terminator::ret(vec![return_operand], span_ref));
        }

        let locals = std::mem::take(&mut ctx.locals);
        let blocks = ctx.finish();

        Ok(Function {
            id: function_id,
            module: ROOT_MODULE_ID.to_string(),
            name: function_name,
            params,
            result,
            locals,
            blocks,
            span: self.spans.span_ref(&span),
        })
    }

    /// Synthetic record type id used to package multi-field actor state into the
    /// single value the scheduler's `updateStateFromReturn` expects.
    fn actor_state_record_type_id(&mut self, actor_name: &str) -> String {
        let id = format!("type:{}.state", sanitize_id(actor_name));
        self.type_layouts.entry(id.clone()).or_insert(TypeLayout {
            id: id.clone(),
            kind: "record".to_string(),
            name: format!("{actor_name}.State"),
            parameters: Vec::new(),
        });
        id
    }

    fn type_id_for_type_expr(&mut self, ty: &hew_parser::ast::TypeExpr) -> String {
        let ty = ty_from_type_expr(ty);
        self.type_id_for_ty(&ty)
    }

    #[expect(
        clippy::too_many_lines,
        reason = "type interning maps every checker Ty variant into the bytecode schema"
    )]
    fn type_id_for_ty(&mut self, ty: &Ty) -> String {
        let ty = ty.materialize_literal_defaults();
        let (id, kind, name, parameters) = match &ty {
            Ty::Unit => (
                "type:unit".to_string(),
                "unit".to_string(),
                "unit".to_string(),
                Vec::new(),
            ),
            Ty::Never => (
                "type:never".to_string(),
                "never".to_string(),
                "never".to_string(),
                Vec::new(),
            ),
            Ty::Bool => (
                "type:bool".to_string(),
                "bool".to_string(),
                "bool".to_string(),
                Vec::new(),
            ),
            // Sandbox parity compares against `hew run` on the supported 64-bit
            // native hosts, not the wasm32 playground target. isize/usize
            // therefore intentionally use 64-bit storage and cast semantics.
            Ty::I8 | Ty::I16 | Ty::I32 | Ty::I64 | Ty::Isize | Ty::IntLiteral => (
                "type:i64".to_string(),
                "integer".to_string(),
                "i64".to_string(),
                Vec::new(),
            ),
            Ty::U8 | Ty::U16 | Ty::U32 | Ty::U64 | Ty::Usize => (
                "type:u64".to_string(),
                "integer".to_string(),
                "u64".to_string(),
                Vec::new(),
            ),
            Ty::F32 => (
                "type:f32".to_string(),
                "float".to_string(),
                "f32".to_string(),
                Vec::new(),
            ),
            Ty::F64 | Ty::FloatLiteral => (
                "type:f64".to_string(),
                "float".to_string(),
                "f64".to_string(),
                Vec::new(),
            ),
            Ty::String => (
                "type:string".to_string(),
                "string".to_string(),
                "string".to_string(),
                Vec::new(),
            ),
            Ty::Char => (
                "type:char".to_string(),
                "string".to_string(),
                "char".to_string(),
                Vec::new(),
            ),
            Ty::Duration => (
                "type:duration".to_string(),
                "integer".to_string(),
                "duration".to_string(),
                Vec::new(),
            ),
            Ty::Named { name, args, .. } => {
                let params: Vec<_> = args.iter().map(|arg| self.type_id_for_ty(arg)).collect();
                let named_id = self.type_id_for_named(name, args);
                let kind = if name == "Vec" {
                    "vector"
                } else if name.eq_ignore_ascii_case("Regex") || name.ends_with(".Regex") {
                    "regex"
                } else if self.enum_layouts.contains_key(&named_id) {
                    "enum"
                } else {
                    "record"
                };
                (named_id, kind.to_string(), name.clone(), params)
            }
            Ty::Tuple(items) => {
                let params: Vec<_> = items.iter().map(|item| self.type_id_for_ty(item)).collect();
                let id = format!(
                    "type:tuple.{}",
                    params
                        .iter()
                        .map(|p| sanitize_id(p))
                        .collect::<Vec<_>>()
                        .join(".")
                );
                // Register a RecordLayout with positional fields _0, _1, … so
                // the VM's `record.new` / `record.get` ops can handle tuples.
                // `or_insert` is idempotent — repeated calls for the same
                // tuple shape are no-ops.
                let fields: Vec<FieldLayout> = params
                    .iter()
                    .enumerate()
                    .map(|(i, ty_id)| FieldLayout {
                        name: format!("_{i}"),
                        ty: ty_id.clone(),
                        index: i,
                    })
                    .collect();
                let mut indexes = BTreeMap::new();
                for i in 0..items.len() {
                    indexes.insert(format!("_{i}"), i);
                }
                self.record_field_indexes
                    .entry(id.clone())
                    .or_insert(indexes);
                self.record_layouts
                    .entry(id.clone())
                    .or_insert(RecordLayout {
                        id: id.clone(),
                        name: ty.user_facing().to_string(),
                        fields,
                    });
                (
                    id,
                    "record".to_string(),
                    ty.user_facing().to_string(),
                    params,
                )
            }
            Ty::Array(element, _) | Ty::Slice(element) => {
                let element_id = self.type_id_for_ty(element);
                (
                    format!("type:vector.{}", sanitize_id(&element_id)),
                    "vector".to_string(),
                    "Vec".to_string(),
                    vec![element_id],
                )
            }
            Ty::Function { params, ret } | Ty::Closure { params, ret, .. } => {
                let mut ids: Vec<_> = params
                    .iter()
                    .map(|param| self.type_id_for_ty(param))
                    .collect();
                ids.push(self.type_id_for_ty(ret));
                (
                    format!(
                        "type:function.{}",
                        ids.iter()
                            .map(|id| sanitize_id(id))
                            .collect::<Vec<_>>()
                            .join(".")
                    ),
                    "function".to_string(),
                    ty.user_facing().to_string(),
                    ids,
                )
            }
            Ty::Bytes
            | Ty::CancellationToken
            | Ty::Var(_)
            | Ty::Pointer { .. }
            | Ty::Borrow { .. }
            | Ty::TraitObject { .. }
            | Ty::Task(_)
            | Ty::AssocType { .. }
            | Ty::Error => (
                format!("type:opaque.{}", sanitize_id(&ty.user_facing().to_string())),
                "opaque".to_string(),
                ty.user_facing().to_string(),
                Vec::new(),
            ),
        };
        self.type_layouts.entry(id.clone()).or_insert(TypeLayout {
            id: id.clone(),
            kind,
            name,
            parameters,
        });
        id
    }

    fn type_id_for_named(&mut self, name: &str, args: &[Ty]) -> String {
        if args.is_empty() {
            format!("type:{}", sanitize_id(name))
        } else {
            let args = args
                .iter()
                .map(|arg| sanitize_id(&self.type_id_for_ty(arg)))
                .collect::<Vec<_>>()
                .join(".");
            format!("type:{}.{}", sanitize_id(name), args)
        }
    }

    /// Register the `Result<T, E>` enum layout (Ok=0, Err=1) plus its variant
    /// tags so the match lowering and the VM's `enum.tag`/`enum.payload` ops can
    /// operate on an actor-ask result. Returns the bytecode type id.
    fn register_result_enum(&mut self, ok_ty: &Ty, err_ty: &Ty) -> String {
        let ok_payload = self.type_id_for_ty(ok_ty);
        let err_payload = self.type_id_for_ty(err_ty);
        let type_id = self.type_id_for_named("Result", &[ok_ty.clone(), err_ty.clone()]);
        self.enum_layouts
            .entry(type_id.clone())
            .or_insert(EnumLayout {
                id: type_id.clone(),
                name: "Result".to_string(),
                variants: vec![
                    VariantLayout {
                        name: "Ok".to_string(),
                        tag: 0,
                        payload: vec![ok_payload],
                    },
                    VariantLayout {
                        name: "Err".to_string(),
                        tag: 1,
                        payload: vec![err_payload],
                    },
                ],
            });
        // `type_id_for_named` recorded the arg layouts but not Result itself;
        // mark it as an enum so on-demand `type_id_for_ty` agrees.
        self.type_layouts.insert(
            type_id.clone(),
            TypeLayout {
                id: type_id.clone(),
                kind: "enum".to_string(),
                name: "Result".to_string(),
                parameters: Vec::new(),
            },
        );
        self.enum_variant_tags.entry("Ok".to_string()).or_insert((
            type_id.clone(),
            0,
            vec![ok_ty.clone()],
        ));
        self.enum_variant_tags.entry("Err".to_string()).or_insert((
            type_id.clone(),
            1,
            vec![err_ty.clone()],
        ));
        type_id
    }

    fn register_stdout(&mut self) -> String {
        let id = "sym:core.stdout.println".to_string();
        self.capabilities
            .entry("core.stdout".to_string())
            .or_insert(Capability {
                id: "core.stdout".to_string(),
                disposition: "allowed".to_string(),
                reason: "stdout is captured by the educational sandbox trace".to_string(),
                required_by: vec![id.clone()],
            });
        self.stdlib_symbols
            .entry(id.clone())
            .or_insert(StdlibSymbol {
                id: id.clone(),
                module: "core.stdout".to_string(),
                name: "println".to_string(),
                params: vec!["type:string".to_string()],
                result: "type:unit".to_string(),
                capability: Some("core.stdout".to_string()),
                admission: "allowed".to_string(),
            });
        id
    }

    fn register_stdout_print(&mut self) -> String {
        let id = "sym:core.stdout.print".to_string();
        let cap_id = "core.stdout".to_string();
        self.capabilities
            .entry(cap_id.clone())
            .or_insert(Capability {
                id: cap_id.clone(),
                disposition: "allowed".to_string(),
                reason: "stdout is captured by the educational sandbox trace".to_string(),
                required_by: Vec::new(),
            });
        if let Some(cap) = self.capabilities.get_mut(&cap_id) {
            if !cap.required_by.contains(&id) {
                cap.required_by.push(id.clone());
                cap.required_by.sort();
            }
        }
        self.stdlib_symbols
            .entry(id.clone())
            .or_insert(StdlibSymbol {
                id: id.clone(),
                module: "core.stdout".to_string(),
                name: "print".to_string(),
                params: vec!["type:string".to_string()],
                result: "type:unit".to_string(),
                capability: Some(cap_id),
                admission: "allowed".to_string(),
            });
        id
    }

    fn register_regex_symbol(&mut self, name: &str, result: &str) -> String {
        let id = format!("sym:std.text.regex.{name}");
        self.capabilities
            .entry("std.text.regex.compile".to_string())
            .or_insert(Capability {
                id: "std.text.regex.compile".to_string(),
                disposition: "reserved".to_string(),
                reason: "regex execution waits for the sandbox-safe regex shim".to_string(),
                required_by: Vec::new(),
            });
        if let Some(capability) = self.capabilities.get_mut("std.text.regex.compile") {
            if !capability.required_by.contains(&id) {
                capability.required_by.push(id.clone());
                capability.required_by.sort();
            }
        }
        self.stdlib_symbols
            .entry(id.clone())
            .or_insert(StdlibSymbol {
                id: id.clone(),
                module: "std.text.regex".to_string(),
                name: name.to_string(),
                params: Vec::new(),
                result: result.to_string(),
                capability: Some("std.text.regex.compile".to_string()),
                admission: "reserved".to_string(),
            });
        id
    }

    fn register_fmt_to_string(&mut self) -> String {
        let id = "sym:std.fmt.to_string".to_string();
        self.stdlib_symbols
            .entry(id.clone())
            .or_insert(StdlibSymbol {
                id: id.clone(),
                module: "std.fmt".to_string(),
                name: "to_string".to_string(),
                params: vec!["type:opaque.any".to_string()],
                result: "type:string".to_string(),
                capability: None,
                admission: "allowed".to_string(),
            });
        id
    }

    /// Ensure the Option<T> or Result<T,E> enum layout is registered in
    /// `enum_layouts` and `enum_variant_tags` so the VM can decode the value.
    ///
    /// Called whenever a `Some`, `None`, `Ok`, or `Err` constructor is emitted
    /// (i.e. not just at actor-ask result sites). The full variant layout is
    /// required so `enum.tag` and `enum.payload` work correctly.
    fn ensure_option_result_layout(&mut self, ty: &Ty) {
        let (type_name, type_id, some_ok_variant, none_err_variant) = match ty {
            Ty::Named {
                builtin: Some(BuiltinType::Option),
                args,
                ..
            } => {
                let inner = args.first().cloned().unwrap_or(Ty::Unit);
                let inner_id = self.type_id_for_ty(&inner);
                let type_id = self.type_id_for_named("Option", std::slice::from_ref(&inner));
                (
                    "Option",
                    type_id,
                    VariantLayout {
                        name: "Some".to_string(),
                        tag: 0,
                        payload: vec![inner_id.clone()],
                    },
                    VariantLayout {
                        name: "None".to_string(),
                        tag: 1,
                        payload: vec![],
                    },
                )
            }
            Ty::Named {
                builtin: Some(BuiltinType::Result),
                args,
                ..
            } => {
                let ok_ty = args.first().cloned().unwrap_or(Ty::Unit);
                let err_ty = args.get(1).cloned().unwrap_or(Ty::Unit);
                let ok_id = self.type_id_for_ty(&ok_ty);
                let err_id = self.type_id_for_ty(&err_ty);
                let type_id = self.type_id_for_named("Result", &[ok_ty.clone(), err_ty.clone()]);
                (
                    "Result",
                    type_id,
                    VariantLayout {
                        name: "Ok".to_string(),
                        tag: 0,
                        payload: vec![ok_id],
                    },
                    VariantLayout {
                        name: "Err".to_string(),
                        tag: 1,
                        payload: vec![err_id],
                    },
                )
            }
            _ => return,
        };
        self.enum_layouts
            .entry(type_id.clone())
            .or_insert(EnumLayout {
                id: type_id.clone(),
                name: type_name.to_string(),
                variants: vec![some_ok_variant, none_err_variant],
            });
        self.type_layouts
            .entry(type_id.clone())
            .or_insert(TypeLayout {
                id: type_id.clone(),
                kind: "enum".to_string(),
                name: type_name.to_string(),
                parameters: Vec::new(),
            });
    }
}
#[derive(Debug, Clone)]
struct LoopTargets {
    continue_id: String,
    exit_id: String,
    label: Option<String>,
}

/// Context threading information needed to lower early `return` inside a
/// receive handler correctly.  Without this, `Stmt::Return` can only emit
/// `ret([expr])`, which conflates the reply value with the next-actor-state
/// and silently corrupts stateful actors.
///
/// With this context, every early `return` mirrors the normal-exit path
/// (emit.rs:664-698): emit `actor.reply(reply_token, expr)` then
/// `ret(<state record>)`.
#[derive(Debug, Clone)]
struct ReceiveHandlerContext {
    /// The reply-token parameter local (first param of every handler).
    reply_local: String,
    /// The state-field locals in declaration order (empty for stateless actors).
    state_locals: Vec<String>,
    /// Actor name, used to look up / intern the state-record type.
    actor_name: String,
}

#[derive(Debug)]
struct FunctionEmitter<'pkg, 'src> {
    package: &'pkg mut PackageEmitter<'src>,
    function_name: String,
    return_ty: Ty,
    locals: Vec<Local>,
    blocks: Vec<BlockBuilder>,
    current: usize,
    next_local: usize,
    next_block: usize,
    bindings: HashMap<String, String>,
    loop_targets: Vec<LoopTargets>,
    /// Set only inside receive-handler emission; drives correct early-return
    /// lowering (see `ReceiveHandlerContext`).
    receive_context: Option<ReceiveHandlerContext>,
}

impl<'pkg, 'src> FunctionEmitter<'pkg, 'src> {
    fn new(package: &'pkg mut PackageEmitter<'src>, function_name: &str, return_ty: Ty) -> Self {
        Self {
            package,
            function_name: function_name.to_string(),
            return_ty,
            locals: Vec::new(),
            blocks: vec![BlockBuilder::new("block:entry".to_string(), None)],
            current: 0,
            next_local: 0,
            next_block: 0,
            bindings: HashMap::new(),
            loop_targets: Vec::new(),
            receive_context: None,
        }
    }

    fn finish(self) -> Vec<Block> {
        self.blocks.into_iter().map(BlockBuilder::finish).collect()
    }

    fn current_block(&mut self) -> &mut BlockBuilder {
        &mut self.blocks[self.current]
    }

    fn current_is_terminated(&self) -> bool {
        self.blocks[self.current].terminator.is_some()
    }

    fn terminate(&mut self, terminator: Terminator) {
        self.blocks[self.current].terminator = Some(terminator);
    }

    fn new_block(&mut self, label: &str, span: Option<String>) -> (usize, String) {
        self.next_block += 1;
        let id = format!(
            "block:{}.{}.{}",
            sanitize_id(&self.function_name),
            label,
            self.next_block
        );
        let idx = self.blocks.len();
        self.blocks.push(BlockBuilder::new(id.clone(), span));
        (idx, id)
    }

    fn switch_to(&mut self, block_idx: usize) {
        self.current = block_idx;
    }

    #[expect(
        clippy::needless_pass_by_value,
        reason = "callers hand ownership of optional spans to local metadata creation"
    )]
    fn declare_local(
        &mut self,
        name: Option<String>,
        ty: &Ty,
        mutable: bool,
        span: Option<std::ops::Range<usize>>,
    ) -> String {
        let id = format!(
            "local:{}.{}",
            sanitize_id(&self.function_name),
            self.next_local
        );
        self.next_local += 1;
        let span_ref = span
            .as_ref()
            .and_then(|span| self.package.spans.span_ref(span));
        let type_id = self.package.type_id_for_ty(ty);
        self.locals.push(Local {
            id: id.clone(),
            name,
            ty: type_id,
            mutable,
            span: span_ref,
        });
        id
    }

    fn temp_local(&mut self, ty: &Ty, span: Option<std::ops::Range<usize>>) -> String {
        self.declare_local(None, ty, false, span)
    }

    /// Declare a local with an explicit bytecode type id, for runtime-only types
    /// (e.g. the actor reply token) that have no `hew_types::Ty` representation.
    fn declare_local_with_type_id(&mut self, name: Option<String>, type_id: &str) -> String {
        let id = format!(
            "local:{}.{}",
            sanitize_id(&self.function_name),
            self.next_local
        );
        self.next_local += 1;
        self.locals.push(Local {
            id: id.clone(),
            name,
            ty: type_id.to_string(),
            mutable: false,
            span: None,
        });
        id
    }

    #[expect(
        clippy::needless_pass_by_value,
        reason = "instruction emission consumes source spans into the span interner"
    )]
    fn emit_instruction(
        &mut self,
        op: impl Into<String>,
        dst: Option<String>,
        args: Vec<Operand>,
        span: Option<std::ops::Range<usize>>,
        metadata: Option<Value>,
    ) {
        let span_ref = span
            .as_ref()
            .and_then(|span| self.package.spans.span_ref(span));
        self.current_block().instructions.push(Instruction {
            op: op.into(),
            dst,
            args,
            span: span_ref,
            metadata,
        });
    }

    fn lower_block(&mut self, block: &AstBlock) -> Result<Option<String>, CompileError> {
        for (stmt, span) in &block.stmts {
            self.lower_stmt(stmt, span.clone())?;
            if self.current_is_terminated() {
                return Ok(None);
            }
        }
        if let Some(expr) = &block.trailing_expr {
            return self.lower_expr(expr).map(Some);
        }
        Ok(None)
    }

    #[expect(
        clippy::too_many_lines,
        reason = "lower_stmt dispatches every admitted statement variant fail-closed; splitting would obscure the control-flow"
    )]
    fn lower_stmt(
        &mut self,
        stmt: &Stmt,
        span: std::ops::Range<usize>,
    ) -> Result<(), CompileError> {
        match stmt {
            Stmt::Let {
                pattern,
                value,
                else_block,
                ..
            } => {
                if else_block.is_some() {
                    self.emit_unsupported(Some(span));
                    return Ok(());
                }
                if let Some(value) = value {
                    let value_local = self.lower_expr(value)?;
                    let ty = self.ty_for_expr(value);
                    if self.pattern_is_unconditional(pattern, &ty) {
                        self.bind_pattern_payloads(pattern, &value_local, &ty);
                    } else {
                        self.emit_unsupported(Some(span));
                    }
                }
            }
            Stmt::Var {
                name, value, ty, ..
            } => {
                let declared_ty = value
                    .as_ref()
                    .map(|expr| self.ty_for_expr(expr))
                    .or_else(|| ty.as_ref().map(|(ty, _)| ty_from_type_expr(ty)))
                    .unwrap_or(Ty::Unit);
                let local =
                    self.declare_local(Some(name.clone()), &declared_ty, true, Some(span.clone()));
                self.bindings.insert(name.clone(), local.clone());
                if let Some(value) = value {
                    let value_local = self.lower_expr(value)?;
                    self.emit_instruction(
                        "local.set",
                        None,
                        vec![Operand::local(local), Operand::local(value_local)],
                        Some(span),
                        None,
                    );
                }
            }
            Stmt::Return(value) => {
                let value_local = if let Some(value) = value {
                    self.lower_expr(value)?
                } else {
                    self.emit_const_unit(Some(span.clone()))
                };
                self.emit_early_return(value_local, span);
            }
            Stmt::Expression(expr) => {
                self.lower_expr(expr)?;
            }
            Stmt::Assign { target, op, value } => {
                if let Expr::Identifier(name) = &target.0 {
                    if let Some(local) = self.bindings.get(name).cloned() {
                        let rhs_local = self.lower_expr(value)?;
                        let result_local = if let Some(compound_op) = op {
                            // Compound assignment (`x += v`, `x -= v`, …).
                            // Read the current binding, combine with rhs using the
                            // type-directed opcode family, then write the result back.
                            let target_ty = self.ty_for_expr(target);
                            let operand_is_f32 = target_ty == Ty::F32;
                            let operand_is_float = target_ty.is_float();
                            let opcode = match compound_op {
                                CompoundAssignOp::Add if operand_is_f32 => "f32.add",
                                CompoundAssignOp::Subtract if operand_is_f32 => "f32.sub",
                                CompoundAssignOp::Multiply if operand_is_f32 => "f32.mul",
                                CompoundAssignOp::Divide if operand_is_f32 => "f32.div",
                                CompoundAssignOp::Modulo if operand_is_f32 => "f32.rem",
                                CompoundAssignOp::Add if operand_is_float => "f64.add",
                                CompoundAssignOp::Subtract if operand_is_float => "f64.sub",
                                CompoundAssignOp::Multiply if operand_is_float => "f64.mul",
                                CompoundAssignOp::Divide if operand_is_float => "f64.div",
                                CompoundAssignOp::Modulo if operand_is_float => "f64.rem",
                                CompoundAssignOp::Add => "i64.checked_add",
                                CompoundAssignOp::Subtract => "i64.checked_sub",
                                CompoundAssignOp::Multiply => "i64.checked_mul",
                                CompoundAssignOp::Divide => "i64.checked_div",
                                CompoundAssignOp::Modulo => "i64.checked_rem",
                                CompoundAssignOp::BitAnd => "i64.and",
                                CompoundAssignOp::BitOr => "i64.or",
                                CompoundAssignOp::BitXor => "i64.xor",
                                CompoundAssignOp::Shl => "i64.shl",
                                CompoundAssignOp::Shr => "i64.shr",
                            };
                            // Emit `local.get current, binding` to read the current value.
                            let current_ty = self.ty_for_expr(target);
                            let current = self.temp_local(&current_ty, Some(span.clone()));
                            self.emit_instruction(
                                "local.get",
                                Some(current.clone()),
                                vec![Operand::local(local.clone())],
                                Some(span.clone()),
                                None,
                            );
                            // Emit the binary op: `result = current op rhs`.
                            let result_ty = self.ty_for_expr(target);
                            let result = self.temp_local(&result_ty, Some(span.clone()));
                            self.emit_instruction(
                                opcode,
                                Some(result.clone()),
                                vec![Operand::local(current), Operand::local(rhs_local)],
                                Some(span.clone()),
                                None,
                            );
                            result
                        } else {
                            // Plain assignment — value is already lowered.
                            rhs_local
                        };
                        self.emit_instruction(
                            "local.set",
                            None,
                            vec![Operand::local(local), Operand::local(result_local)],
                            Some(span),
                            None,
                        );
                    } else {
                        self.emit_unsupported(Some(span));
                    }
                } else {
                    self.emit_unsupported(Some(span));
                }
            }
            Stmt::If {
                condition,
                then_block,
                else_block,
            } => {
                self.lower_stmt_if(condition, then_block, else_block.as_ref(), span)?;
            }
            Stmt::Match { scrutinee, arms } => {
                self.lower_stmt_match(scrutinee, arms, span)?;
            }
            Stmt::IfLet {
                pattern,
                expr,
                body,
                else_body,
            } => {
                self.lower_stmt_if_let(pattern, expr, body, else_body.as_ref(), span)?;
            }
            Stmt::Defer(_) => {
                self.emit_unsupported(Some(span));
            }
            Stmt::Loop { label, body } => {
                let span_ref = self.package.spans.span_ref(&span);
                let (header_idx, header_id) = self.new_block("loop_header", span_ref.clone());
                let (exit_idx, exit_id) = self.new_block("loop_exit", span_ref.clone());
                self.terminate(Terminator::br(header_id.clone(), Vec::new(), span_ref));
                self.switch_to(header_idx);
                self.loop_targets.push(LoopTargets {
                    continue_id: header_id.clone(),
                    exit_id: exit_id.clone(),
                    label: label.clone(),
                });
                self.lower_block(body)?;
                if !self.current_is_terminated() {
                    self.terminate(Terminator::br(header_id, Vec::new(), None));
                }
                self.loop_targets.pop();
                self.switch_to(exit_idx);
            }
            Stmt::For {
                label,
                is_await,
                pattern,
                iterable,
                body,
            } => {
                if *is_await {
                    self.emit_unsupported(Some(span));
                } else {
                    self.emit_for_range(label.clone(), pattern, iterable, body, span)?;
                }
            }
            Stmt::While {
                label,
                condition,
                body,
            } => {
                let span_ref = self.package.spans.span_ref(&span);
                let (header_idx, header_id) = self.new_block("while_header", span_ref.clone());
                let (body_idx, body_id) = self.new_block("while_body", span_ref.clone());
                let (exit_idx, exit_id) = self.new_block("while_exit", span_ref.clone());
                self.terminate(Terminator::br(
                    header_id.clone(),
                    Vec::new(),
                    span_ref.clone(),
                ));
                self.switch_to(header_idx);
                let cond = self.lower_expr(condition)?;
                self.terminate(Terminator::br_if(
                    Operand::local(cond),
                    body_id,
                    exit_id.clone(),
                    Vec::new(),
                    span_ref,
                ));
                self.switch_to(body_idx);
                self.loop_targets.push(LoopTargets {
                    continue_id: header_id.clone(),
                    exit_id: exit_id.clone(),
                    label: label.clone(),
                });
                self.lower_block(body)?;
                if !self.current_is_terminated() {
                    self.terminate(Terminator::br(header_id, Vec::new(), None));
                }
                self.loop_targets.pop();
                self.switch_to(exit_idx);
            }
            Stmt::WhileLet {
                label,
                pattern,
                expr,
                body,
            } => {
                self.emit_while_let(label.clone(), pattern, expr, body, span)?;
            }
            Stmt::Break { label, value: None } => {
                if let Some(exit_id) = self.resolve_loop_exit(label.as_deref()) {
                    let span_ref = self.package.spans.span_ref(&span);
                    self.terminate(Terminator::br(exit_id, Vec::new(), span_ref));
                } else {
                    self.emit_unsupported(Some(span));
                }
            }
            Stmt::Break { .. } => {
                // break-with-value is rejected at the profile level; trap defensively
                self.emit_unsupported(Some(span));
            }
            Stmt::Continue { label } => {
                if let Some(continue_id) = self.resolve_loop_continue(label.as_deref()) {
                    let span_ref = self.package.spans.span_ref(&span);
                    self.terminate(Terminator::br(continue_id, Vec::new(), span_ref));
                } else {
                    self.emit_unsupported(Some(span));
                }
            }
        }
        Ok(())
    }

    #[expect(
        clippy::too_many_lines,
        reason = "expression lowering keeps AST coverage visible against opcode families"
    )]
    fn lower_expr(&mut self, expr: &Spanned<Expr>) -> Result<String, CompileError> {
        let (kind, span) = expr;
        match kind {
            Expr::Literal(literal) => Ok(self.lower_literal(literal, span.clone())),
            Expr::RegexLiteral(pattern) => {
                let pattern_local = self.temp_local(&Ty::String, Some(span.clone()));
                self.emit_instruction(
                    "const.string",
                    Some(pattern_local.clone()),
                    vec![Operand::literal(pattern.clone())],
                    Some(span.clone()),
                    None,
                );
                let regex_local = self.temp_local(
                    &Ty::Named {
                        name: "Regex".to_string(),
                        args: Vec::new(),
                        builtin: None,
                    },
                    Some(span.clone()),
                );
                self.emit_instruction(
                    "regex.compile",
                    Some(regex_local.clone()),
                    vec![Operand::local(pattern_local)],
                    Some(span.clone()),
                    None,
                );
                self.package.register_regex_symbol("compile", "type:regex");
                Ok(regex_local)
            }
            Expr::Identifier(name) => {
                if let Some(local) = self.bindings.get(name).cloned() {
                    let ty = self.ty_for_expr(expr);
                    let dst = self.temp_local(&ty, Some(span.clone()));
                    self.emit_instruction(
                        "local.get",
                        Some(dst.clone()),
                        vec![Operand::local(local)],
                        Some(span.clone()),
                        None,
                    );
                    Ok(dst)
                } else if let Some((type_id, tag, _)) =
                    self.package.enum_variant_tags.get(name).cloned()
                {
                    // Bare unit-variant identifier in expression position (e.g.
                    // `Double` in `apply(Double, 5)`).  Hew treats unit variants as
                    // zero-argument constructors; the parser may produce either
                    // `Expr::Identifier` or `Expr::Call { args: [] }` depending on
                    // whether call-parens are present.  Emit `enum.new` with no
                    // payload arguments, matching the `lower_call` path for
                    // `Expr::Call { function: Identifier(variant), args: [] }`.
                    let ty = self.ty_for_expr(expr);
                    let dst = self.temp_local(&ty, Some(span.clone()));
                    self.emit_instruction(
                        "enum.new",
                        Some(dst.clone()),
                        vec![Operand::ty(type_id), Operand::literal(tag as u64)],
                        Some(span.clone()),
                        None,
                    );
                    Ok(dst)
                } else if matches!(name.as_str(), "None" | "Some" | "Ok" | "Err") {
                    // Bare builtin enum constructors not registered in enum_variant_tags
                    // (Option/Result are builtins). Look up the resolved type via
                    // the type output to emit the correct enum.new.
                    let ty = self.ty_for_expr(expr);
                    self.package.ensure_option_result_layout(&ty);
                    let type_id = self.package.type_id_for_ty(&ty);
                    let tag: u64 = match name.as_str() {
                        "Some" | "Ok" => 0,
                        "None" | "Err" => 1,
                        _ => unreachable!(),
                    };
                    let dst = self.temp_local(&ty, Some(span.clone()));
                    self.emit_instruction(
                        "enum.new",
                        Some(dst.clone()),
                        vec![Operand::ty(type_id), Operand::literal(tag)],
                        Some(span.clone()),
                        None,
                    );
                    Ok(dst)
                } else if let Some((machine, state)) = self.machine_state_path(name) {
                    // `Machine::State` constructs a machine value in that state.
                    let type_id = self.package.type_id_for_named(&machine, &[]);
                    let machine_ty = Ty::Named {
                        name: machine,
                        args: Vec::new(),
                        builtin: None,
                    };
                    let dst = self.temp_local(&machine_ty, Some(span.clone()));
                    self.emit_instruction(
                        "machine.new",
                        Some(dst.clone()),
                        vec![Operand::ty(type_id), Operand::symbol(state)],
                        Some(span.clone()),
                        None,
                    );
                    Ok(dst)
                } else if self.package.user_functions.contains(name) {
                    // Function name used in value position (e.g. stored in a
                    // record field of function type).  Emit `const.function` which
                    // materialises a function-kind value in the VM; the value can
                    // be retrieved via `record.get` and invoked via `call.indirect`.
                    let ty = self.ty_for_expr(expr);
                    let dst = self.temp_local(&ty, Some(span.clone()));
                    self.emit_instruction(
                        "const.function",
                        Some(dst.clone()),
                        vec![Operand::function(function_id(name))],
                        Some(span.clone()),
                        None,
                    );
                    Ok(dst)
                } else if let Some(literal) = self.package.const_literals.get(name).cloned() {
                    Ok(self.lower_literal(&literal, span.clone()))
                } else {
                    self.emit_unsupported(Some(span.clone()));
                    Ok(self.emit_const_unit(Some(span.clone())))
                }
            }
            Expr::Binary { left, op, right } => self.lower_binary(left, *op, right, span.clone()),
            Expr::Unary { op, operand } => {
                let value = self.lower_expr(operand)?;
                if *op == hew_parser::ast::UnaryOp::Negate {
                    let ty = self.ty_for_expr(expr);
                    // Type-directed negation: f32 rounds to single precision,
                    // f64 stays double precision, and integer negation is checked.
                    let opcode = if ty == Ty::F32 {
                        "f32.neg"
                    } else if ty.is_float() {
                        "f64.neg"
                    } else {
                        "i64.neg"
                    };
                    let dst = self.temp_local(&ty, Some(span.clone()));
                    self.emit_instruction(
                        opcode,
                        Some(dst.clone()),
                        vec![Operand::local(value)],
                        Some(span.clone()),
                        None,
                    );
                    Ok(dst)
                } else if *op == hew_parser::ast::UnaryOp::Not {
                    let dst = self.temp_local(&Ty::Bool, Some(span.clone()));
                    self.emit_instruction(
                        "bool.not",
                        Some(dst.clone()),
                        vec![Operand::local(value)],
                        Some(span.clone()),
                        None,
                    );
                    Ok(dst)
                } else {
                    self.emit_unsupported(Some(span.clone()));
                    Ok(value)
                }
            }
            Expr::Call { function, args, .. } => self.lower_call(function, args, span.clone()),
            Expr::MethodCall {
                receiver,
                method,
                args,
            } => self.lower_method_call(receiver, method, args, span.clone()),
            Expr::StructInit {
                name, fields, base, ..
            } => self.lower_struct_init(expr, name, fields, base.as_deref(), span.clone()),
            Expr::FieldAccess { object, field } => {
                let object_ty = self.ty_for_expr(object);
                // `supervisor.childName` resolves a running child actor handle.
                // The supervisor handle is a `LocalPid<SupervisorName>`, so we
                // detect it by the pid's type argument.
                if self.supervisor_handle_arg(&object_ty).is_some() {
                    let supervisor_local = self.lower_expr(object)?;
                    let child_ty = self.ty_for_expr(expr);
                    let dst = self.temp_local(&child_ty, Some(span.clone()));
                    self.emit_instruction(
                        "supervisor.child",
                        Some(dst.clone()),
                        vec![
                            Operand::local(supervisor_local),
                            Operand::symbol(field.clone()),
                        ],
                        Some(span.clone()),
                        None,
                    );
                    return Ok(dst);
                }
                let Some(field_index) = self.resolve_record_field_index(&object_ty, field) else {
                    self.emit_unsupported(Some(span.clone()));
                    return Ok(self.emit_const_unit(Some(span.clone())));
                };
                let object_local = self.lower_expr(object)?;
                let ty = self.ty_for_expr(expr);
                let dst = self.temp_local(&ty, Some(span.clone()));
                self.emit_instruction(
                    "record.get",
                    Some(dst.clone()),
                    vec![
                        Operand::local(object_local),
                        Operand::literal(field_index as u64),
                    ],
                    Some(span.clone()),
                    None,
                );
                Ok(dst)
            }
            Expr::Match { scrutinee, arms } => self.lower_match(scrutinee, arms, span.clone()),
            Expr::Array(items) => {
                let vector_ty = self.ty_for_expr(expr);
                let dst = self.temp_local(&vector_ty, Some(span.clone()));
                let element_ty = match &vector_ty {
                    Ty::Named { args, .. } => args.first().cloned().unwrap_or(Ty::Unit),
                    _ => Ty::Unit,
                };
                let element_type_id = self.package.type_id_for_ty(&element_ty);
                self.emit_instruction(
                    "vector.new",
                    Some(dst.clone()),
                    vec![Operand::ty(element_type_id)],
                    Some(span.clone()),
                    None,
                );
                for item in items {
                    let item_local = self.lower_expr(item)?;
                    self.emit_instruction(
                        "vector.push",
                        None,
                        vec![Operand::local(dst.clone()), Operand::local(item_local)],
                        Some(item.1.clone()),
                        None,
                    );
                }
                Ok(dst)
            }
            Expr::ArrayRepeat { value, count } => {
                self.lower_array_repeat(expr, value, count, span.clone())
            }
            Expr::InterpolatedString(parts) => {
                let mut current = None;
                for part in parts {
                    let part_local = match part {
                        hew_parser::ast::StringPart::Literal(text) => {
                            let local = self.temp_local(&Ty::String, Some(span.clone()));
                            self.emit_instruction(
                                "const.string",
                                Some(local.clone()),
                                vec![Operand::literal(text.clone())],
                                Some(span.clone()),
                                None,
                            );
                            local
                        }
                        hew_parser::ast::StringPart::Expr(expr) => self.lower_expr(expr)?,
                    };
                    current = Some(if let Some(current) = current {
                        let local = self.temp_local(&Ty::String, Some(span.clone()));
                        self.emit_instruction(
                            "string.concat",
                            Some(local.clone()),
                            vec![Operand::local(current), Operand::local(part_local)],
                            Some(span.clone()),
                            None,
                        );
                        local
                    } else {
                        part_local
                    });
                }
                Ok(current.unwrap_or_else(|| {
                    self.lower_literal(&Literal::String(String::new()), span.clone())
                }))
            }
            Expr::Block(block) => Ok(self
                .lower_block(block)?
                .unwrap_or_else(|| self.emit_const_unit(Some(span.clone())))),
            Expr::Index { object, index } => {
                // `v[start..end]` — exclusive range slice. In the AST, `start..end` inside
                // brackets is an `Expr::Range { inclusive: false }`, not `Expr::Binary`.
                if let Expr::Range {
                    start: Some(start),
                    end: Some(end),
                    inclusive: false,
                } = &index.0
                {
                    let object_local = self.lower_expr(object)?;
                    let start_local = self.lower_expr(start)?;
                    let end_local = self.lower_expr(end)?;
                    let ty = self.ty_for_expr(expr);
                    let dst = self.temp_local(&ty, Some(span.clone()));
                    self.emit_instruction(
                        "vector.range_slice",
                        Some(dst.clone()),
                        vec![
                            Operand::local(object_local),
                            Operand::local(start_local),
                            Operand::local(end_local),
                        ],
                        Some(span.clone()),
                        None,
                    );
                    return Ok(dst);
                }
                // `v[start..=end]` — inclusive range slice. Lower by computing the
                // exclusive end (`end + 1`) and delegating to `vector.range_slice`,
                // which already bounds-checks `end <= len`.  `i64.checked_add` traps
                // on overflow (same as any checked integer path), so a callee with
                // `end == I64_MAX` is a programming error that surfaces a trap rather
                // than silent wrong output — matching the native out-of-bounds trap.
                if let Expr::Range {
                    start: Some(start),
                    end: Some(end),
                    inclusive: true,
                } = &index.0
                {
                    let object_local = self.lower_expr(object)?;
                    let start_local = self.lower_expr(start)?;
                    let end_local = self.lower_expr(end)?;
                    // exclusive_end = inclusive_end + 1
                    let one_local = self.temp_local(&Ty::I64, Some(span.clone()));
                    self.emit_instruction(
                        "const.i64",
                        Some(one_local.clone()),
                        vec![Operand::literal(1u64)],
                        Some(span.clone()),
                        None,
                    );
                    let exclusive_end = self.temp_local(&Ty::I64, Some(span.clone()));
                    self.emit_instruction(
                        "i64.checked_add",
                        Some(exclusive_end.clone()),
                        vec![Operand::local(end_local), Operand::local(one_local)],
                        Some(span.clone()),
                        None,
                    );
                    let ty = self.ty_for_expr(expr);
                    let dst = self.temp_local(&ty, Some(span.clone()));
                    self.emit_instruction(
                        "vector.range_slice",
                        Some(dst.clone()),
                        vec![
                            Operand::local(object_local),
                            Operand::local(start_local),
                            Operand::local(exclusive_end),
                        ],
                        Some(span.clone()),
                        None,
                    );
                    return Ok(dst);
                }
                let object_local = self.lower_expr(object)?;
                let index_local = self.lower_expr(index)?;
                let ty = self.ty_for_expr(expr);
                let dst = self.temp_local(&ty, Some(span.clone()));
                self.emit_instruction(
                    "vector.get",
                    Some(dst.clone()),
                    vec![Operand::local(object_local), Operand::local(index_local)],
                    Some(span.clone()),
                    None,
                );
                Ok(dst)
            }
            Expr::If {
                condition,
                then_block,
                else_block,
            } => {
                let cond_local = self.lower_expr(condition)?;
                let span_ref = self.package.spans.span_ref(span);
                let (then_idx, then_id) = self.new_block("if_then", span_ref.clone());
                let (exit_idx, exit_id) = self.new_block("if_exit", span_ref.clone());

                if let Some(else_expr) = else_block.as_deref() {
                    let (else_idx, else_id) = self.new_block("if_else", span_ref.clone());
                    let result_ty = self.ty_for_expr(expr);
                    let result_local =
                        self.declare_local(None, &result_ty, true, Some(span.clone()));
                    self.terminate(Terminator::br_if(
                        Operand::local(cond_local),
                        then_id,
                        else_id,
                        Vec::new(),
                        span_ref,
                    ));

                    self.switch_to(then_idx);
                    let then_val = self.lower_expr(then_block)?;
                    self.emit_instruction(
                        "local.set",
                        None,
                        vec![
                            Operand::local(result_local.clone()),
                            Operand::local(then_val),
                        ],
                        Some(then_block.1.clone()),
                        None,
                    );
                    if !self.current_is_terminated() {
                        let s = self.package.spans.span_ref(&then_block.1);
                        self.terminate(Terminator::br(exit_id.clone(), Vec::new(), s));
                    }

                    self.switch_to(else_idx);
                    let else_val = self.lower_expr(else_expr)?;
                    self.emit_instruction(
                        "local.set",
                        None,
                        vec![
                            Operand::local(result_local.clone()),
                            Operand::local(else_val),
                        ],
                        Some(else_expr.1.clone()),
                        None,
                    );
                    if !self.current_is_terminated() {
                        let s = self.package.spans.span_ref(&else_expr.1);
                        self.terminate(Terminator::br(exit_id, Vec::new(), s));
                    }

                    self.switch_to(exit_idx);
                    Ok(result_local)
                } else {
                    // No else branch: result is always unit.
                    self.terminate(Terminator::br_if(
                        Operand::local(cond_local),
                        then_id,
                        exit_id.clone(),
                        Vec::new(),
                        span_ref,
                    ));
                    self.switch_to(then_idx);
                    self.lower_expr(then_block)?;
                    if !self.current_is_terminated() {
                        let s = self.package.spans.span_ref(&then_block.1);
                        self.terminate(Terminator::br(exit_id, Vec::new(), s));
                    }
                    self.switch_to(exit_idx);
                    Ok(self.emit_const_unit(Some(span.clone())))
                }
            }
            Expr::Spawn { target, args, .. } => self.lower_spawn(target, args, span.clone()),
            Expr::Await(operand) => self.lower_await(operand, span.clone()),
            // `if let` in expression position (e.g. `let v = if let Some(x) =
            // opt { x } else { d }`): join both arm values on a result local so
            // value-position if-let runs at parity with native `hew run`,
            // mirroring the value-position `Expr::If` / `Expr::Match` lowering.
            Expr::IfLet {
                pattern,
                expr: scrutinee,
                body,
                else_body,
            } => self.lower_expr_if_let(
                expr,
                pattern,
                scrutinee,
                body,
                else_body.as_ref(),
                span.clone(),
            ),
            Expr::Clone(operand) => {
                // `clone expr` produces an independent deep copy of the value.
                // The sandbox VM's `local.set` always calls `cloneValue` (a deep
                // recursive copy), so writing the evaluated operand into a fresh
                // temp local via `local.set` is sufficient to produce an
                // independent copy. This correctly handles vector aliasing: a
                // cloned vector is a distinct object whose items are independent
                // from the original.
                let inner = self.lower_expr(operand)?;
                let ty = self.ty_for_expr(expr);
                let dst = self.temp_local(&ty, Some(span.clone()));
                // `local.set dst, inner` calls cloneValue(resolve(inner)) in the
                // VM, producing a deep copy stored in `dst`.
                self.emit_instruction(
                    "local.set",
                    None,
                    vec![Operand::local(dst.clone()), Operand::local(inner)],
                    Some(span.clone()),
                    None,
                );
                Ok(dst)
            }
            Expr::Tuple(items) => {
                // Tuples are lowered as anonymous records with positional fields
                // _0, _1, … so the VM's `record.new` / `record.get` ops handle
                // them the same way as named structs.  `type_id_for_ty` already
                // registers the RecordLayout and field-index map for every
                // Ty::Tuple it encounters.
                let ty = self.ty_for_expr(expr);
                let type_id = self.package.type_id_for_ty(&ty);
                let mut args = vec![Operand::ty(type_id)];
                for item in items {
                    let local = self.lower_expr(item)?;
                    args.push(Operand::local(local));
                }
                let dst = self.temp_local(&ty, Some(span.clone()));
                self.emit_instruction(
                    "record.new",
                    Some(dst.clone()),
                    args,
                    Some(span.clone()),
                    None,
                );
                Ok(dst)
            }
            // `await_restart` parks the actor on the supervisor restart observer
            // — a native-only cooperative-scheduler primitive with no sandbox-VM
            // analogue. Fail closed (unsupported) on the sandbox path, matching
            // the other suspension/concurrency constructs in this group.
            Expr::MapLiteral { .. }
            | Expr::Lambda { .. }
            | Expr::SpawnLambdaActor { .. }
            | Expr::Scope { .. }
            | Expr::ForkChild { .. }
            | Expr::ForkBlock { .. }
            | Expr::ScopeDeadline { .. }
            | Expr::Select { .. }
            | Expr::Join(_)
            | Expr::Timeout { .. }
            | Expr::UnsafeBlock(_)
            | Expr::Yield(_)
            | Expr::Return(_)
            | Expr::This
            | Expr::Range { .. }
            | Expr::ByteStringLiteral(_)
            | Expr::ByteArrayLiteral(_)
            | Expr::Is { .. }
            | Expr::MachineEmit { .. }
            | Expr::AwaitRestart(_)
            | Expr::GenBlock { .. } => {
                self.emit_unsupported(Some(span.clone()));
                Ok(self.emit_const_unit(Some(span.clone())))
            }
            Expr::Cast { expr: operand, .. } => self.lower_cast(operand, span.clone()),
            Expr::PostfixTry(operand) => self.lower_postfix_try(operand, span.clone()),
        }
    }

    #[expect(
        clippy::too_many_lines,
        reason = "array-repeat lowering builds a four-block counted loop whose evaluation and clone order must remain visible"
    )]
    fn lower_array_repeat(
        &mut self,
        expr: &Spanned<Expr>,
        value: &Spanned<Expr>,
        count: &Spanned<Expr>,
        span: std::ops::Range<usize>,
    ) -> Result<String, CompileError> {
        let vector_ty = self.ty_for_expr(expr);
        let element_ty = match &vector_ty {
            Ty::Named { args, .. } => args.first().cloned().unwrap_or(Ty::Unit),
            Ty::Array(element, _) | Ty::Slice(element) => (**element).clone(),
            _ => {
                self.emit_unsupported(Some(span.clone()));
                return Ok(self.emit_const_unit(Some(span)));
            }
        };
        let element_type_id = self.package.type_id_for_ty(&element_ty);
        let vector_local = self.temp_local(&vector_ty, Some(span.clone()));
        self.emit_instruction(
            "vector.new",
            Some(vector_local.clone()),
            vec![Operand::ty(element_type_id)],
            Some(span.clone()),
            None,
        );

        // Native lowering evaluates the repeated value once before the count,
        // then clones that value into each slot.
        let value_local = self.lower_expr(value)?;
        let count_local = self.lower_expr(count)?;
        let index_local = self.declare_local(None, &Ty::I64, true, Some(span.clone()));
        let zero_local = self.lower_literal(
            &Literal::Integer {
                value: 0,
                radix: hew_parser::ast::IntRadix::Decimal,
            },
            span.clone(),
        );
        self.emit_instruction(
            "local.set",
            None,
            vec![
                Operand::local(index_local.clone()),
                Operand::local(zero_local),
            ],
            Some(span.clone()),
            None,
        );

        let span_ref = self.package.spans.span_ref(&span);
        let (header_idx, header_id) = self.new_block("array_repeat_header", span_ref.clone());
        let (body_idx, body_id) = self.new_block("array_repeat_body", span_ref.clone());
        let (continue_idx, continue_id) = self.new_block("array_repeat_continue", span_ref.clone());
        let (exit_idx, exit_id) = self.new_block("array_repeat_exit", span_ref.clone());

        self.terminate(Terminator::br(
            header_id.clone(),
            Vec::new(),
            span_ref.clone(),
        ));
        self.switch_to(header_idx);
        let condition_local = self.temp_local(&Ty::Bool, Some(span.clone()));
        self.emit_instruction(
            "cmp.lt",
            Some(condition_local.clone()),
            vec![
                Operand::local(index_local.clone()),
                Operand::local(count_local),
            ],
            Some(span.clone()),
            None,
        );
        self.terminate(Terminator::br_if(
            Operand::local(condition_local),
            body_id,
            exit_id.clone(),
            Vec::new(),
            span_ref.clone(),
        ));

        self.switch_to(body_idx);
        self.emit_instruction(
            "vector.push",
            None,
            vec![
                Operand::local(vector_local.clone()),
                Operand::local(value_local),
            ],
            Some(value.1.clone()),
            None,
        );
        self.terminate(Terminator::br(continue_id, Vec::new(), span_ref.clone()));

        self.switch_to(continue_idx);
        let one_local = self.lower_literal(
            &Literal::Integer {
                value: 1,
                radix: hew_parser::ast::IntRadix::Decimal,
            },
            span.clone(),
        );
        let next_local = self.temp_local(&Ty::I64, Some(span.clone()));
        self.emit_instruction(
            "i64.checked_add",
            Some(next_local.clone()),
            vec![
                Operand::local(index_local.clone()),
                Operand::local(one_local),
            ],
            Some(span.clone()),
            None,
        );
        self.emit_instruction(
            "local.set",
            None,
            vec![Operand::local(index_local), Operand::local(next_local)],
            Some(span),
            None,
        );
        self.terminate(Terminator::br(header_id, Vec::new(), span_ref));

        self.switch_to(exit_idx);
        Ok(vector_local)
    }

    fn lower_cast(
        &mut self,
        operand: &Spanned<Expr>,
        span: std::ops::Range<usize>,
    ) -> Result<String, CompileError> {
        let from_ty = self.ty_for_expr(operand);
        let to_ty = self.ty_for_span(&span);
        let (Some(from_name), Some(to_name)) = (
            numeric_cast_type_name(&from_ty),
            numeric_cast_type_name(&to_ty),
        ) else {
            self.emit_unsupported(Some(span.clone()));
            return Ok(self.emit_const_unit(Some(span)));
        };
        let value_local = self.lower_expr(operand)?;
        let dst = self.temp_local(&to_ty, Some(span.clone()));
        self.emit_instruction(
            "numeric.cast",
            Some(dst.clone()),
            vec![
                Operand::local(value_local),
                Operand::symbol(from_name),
                Operand::symbol(to_name),
            ],
            Some(span),
            None,
        );
        Ok(dst)
    }

    #[expect(
        clippy::too_many_lines,
        reason = "postfix-try lowering keeps success extraction and fail-fast return CFG in one auditable unit"
    )]
    fn lower_postfix_try(
        &mut self,
        operand: &Spanned<Expr>,
        span: std::ops::Range<usize>,
    ) -> Result<String, CompileError> {
        let operand_ty = self.ty_for_expr(operand);
        let return_ty = self.return_ty.clone();
        let result_ty = self.ty_for_span(&span);
        let is_result = match (&operand_ty, &return_ty) {
            (
                Ty::Named {
                    builtin: Some(BuiltinType::Result),
                    args: operand_args,
                    ..
                },
                Ty::Named {
                    builtin: Some(BuiltinType::Result),
                    args: return_args,
                    ..
                },
            ) => operand_args.get(1) == return_args.get(1),
            (
                Ty::Named {
                    builtin: Some(BuiltinType::Option),
                    ..
                },
                Ty::Named {
                    builtin: Some(BuiltinType::Option),
                    ..
                },
            ) => false,
            _ => {
                self.emit_unsupported(Some(span.clone()));
                return Ok(self.emit_const_unit(Some(span)));
            }
        };
        let is_option = matches!(
            (&operand_ty, &return_ty),
            (
                Ty::Named {
                    builtin: Some(BuiltinType::Option),
                    ..
                },
                Ty::Named {
                    builtin: Some(BuiltinType::Option),
                    ..
                }
            )
        );
        if !is_result && !is_option {
            self.emit_unsupported(Some(span.clone()));
            return Ok(self.emit_const_unit(Some(span)));
        }

        self.package.ensure_option_result_layout(&operand_ty);
        self.package.ensure_option_result_layout(&return_ty);
        let operand_local = self.lower_expr(operand)?;
        let result_local = self.declare_local(None, &result_ty, true, Some(span.clone()));
        let span_ref = self.package.spans.span_ref(&span);
        let (success_idx, success_id) = self.new_block("postfix_try_success", span_ref.clone());
        let (error_idx, error_id) = self.new_block("postfix_try_error", span_ref.clone());
        let (exit_idx, exit_id) = self.new_block("postfix_try_exit", span_ref.clone());

        let tag_local = self.temp_local(&Ty::I64, Some(span.clone()));
        self.emit_instruction(
            "enum.tag",
            Some(tag_local.clone()),
            vec![Operand::local(operand_local.clone())],
            Some(span.clone()),
            None,
        );
        let zero_local = self.lower_literal(
            &Literal::Integer {
                value: 0,
                radix: hew_parser::ast::IntRadix::Decimal,
            },
            span.clone(),
        );
        let condition_local = self.temp_local(&Ty::Bool, Some(span.clone()));
        self.emit_instruction(
            "cmp.eq",
            Some(condition_local.clone()),
            vec![Operand::local(tag_local), Operand::local(zero_local)],
            Some(span.clone()),
            None,
        );
        self.terminate(Terminator::br_if(
            Operand::local(condition_local),
            success_id,
            error_id,
            Vec::new(),
            span_ref.clone(),
        ));

        self.switch_to(success_idx);
        let payload_local = self.temp_local(&result_ty, Some(span.clone()));
        self.emit_instruction(
            "enum.payload",
            Some(payload_local.clone()),
            vec![
                Operand::local(operand_local.clone()),
                Operand::literal(0_u64),
            ],
            Some(span.clone()),
            None,
        );
        self.emit_instruction(
            "local.set",
            None,
            vec![
                Operand::local(result_local.clone()),
                Operand::local(payload_local),
            ],
            Some(span.clone()),
            None,
        );
        self.terminate(Terminator::br(exit_id, Vec::new(), span_ref.clone()));

        self.switch_to(error_idx);
        let return_type_id = self.package.type_id_for_ty(&return_ty);
        let mut error_args = vec![Operand::ty(return_type_id), Operand::literal(1_u64)];
        if is_result {
            let error_ty = match &operand_ty {
                Ty::Named { args, .. } => args.get(1).cloned().unwrap_or(Ty::Unit),
                _ => Ty::Unit,
            };
            let error_payload = self.temp_local(&error_ty, Some(span.clone()));
            self.emit_instruction(
                "enum.payload",
                Some(error_payload.clone()),
                vec![Operand::local(operand_local), Operand::literal(0_u64)],
                Some(span.clone()),
                None,
            );
            error_args.push(Operand::local(error_payload));
        }
        let error_return = self.temp_local(&return_ty, Some(span.clone()));
        self.emit_instruction(
            "enum.new",
            Some(error_return.clone()),
            error_args,
            Some(span.clone()),
            None,
        );
        self.emit_early_return(error_return, span);

        self.switch_to(exit_idx);
        Ok(result_local)
    }

    fn emit_early_return(&mut self, value_local: String, span: std::ops::Range<usize>) {
        let span_ref = self.package.spans.span_ref(&span);
        if let Some(rctx) = self.receive_context.clone() {
            self.emit_instruction(
                "actor.reply",
                None,
                vec![
                    Operand::local(rctx.reply_local),
                    Operand::local(value_local),
                ],
                None,
                None,
            );
            let return_operand = match rctx.state_locals.as_slice() {
                [] => Operand::local(self.emit_const_unit(Some(span.clone()))),
                [single] => Operand::local(single.clone()),
                fields => {
                    let record_ty = self.package.actor_state_record_type_id(&rctx.actor_name);
                    let mut operands = vec![Operand::ty(record_ty)];
                    operands.extend(fields.iter().map(|local| Operand::local(local.clone())));
                    let dst = self.temp_local(&Ty::Unit, Some(span));
                    self.emit_instruction("record.new", Some(dst.clone()), operands, None, None);
                    Operand::local(dst)
                }
            };
            self.terminate(Terminator::ret(vec![return_operand], span_ref));
        } else {
            self.terminate(Terminator::ret(vec![Operand::local(value_local)], span_ref));
        }
    }

    fn lower_struct_init(
        &mut self,
        expr: &Spanned<Expr>,
        name: &str,
        fields: &[(String, Spanned<Expr>)],
        base: Option<&Spanned<Expr>>,
        span: std::ops::Range<usize>,
    ) -> Result<String, CompileError> {
        let mut field_values = BTreeMap::new();
        for (field, value) in fields {
            field_values.insert(field.clone(), self.lower_expr(value)?);
        }

        let base_local = base.map(|base| self.lower_expr(base)).transpose()?;
        let ty = self.ty_for_expr(expr);
        let type_id = self.package.type_id_for_named(name, &[]);
        let mut args = vec![Operand::ty(type_id.clone())];

        let ordered_fields: Vec<String> = self.package.record_layouts.get(&type_id).map_or_else(
            || field_values.keys().cloned().collect(),
            |layout| {
                let mut fields = layout.fields.clone();
                fields.sort_by_key(|field| field.index);
                fields.into_iter().map(|field| field.name).collect()
            },
        );

        for (index, field) in ordered_fields.iter().enumerate() {
            if let Some(local) = field_values.get(field) {
                args.push(Operand::local(local.clone()));
            } else if let Some(base_local) = &base_local {
                let field_ty = self
                    .package
                    .type_output
                    .type_defs
                    .get(name)
                    .and_then(|def| def.fields.get(field))
                    .cloned()
                    .unwrap_or(Ty::Unit);
                let copied = self.temp_local(&field_ty, Some(span.clone()));
                self.emit_instruction(
                    "record.get",
                    Some(copied.clone()),
                    vec![
                        Operand::local(base_local.clone()),
                        Operand::literal(index as u64),
                    ],
                    Some(span.clone()),
                    None,
                );
                args.push(Operand::local(copied));
            } else {
                self.emit_unsupported(Some(span.clone()));
                return Ok(self.emit_const_unit(Some(span)));
            }
        }

        let dst = self.temp_local(&ty, Some(span.clone()));
        self.emit_instruction(
            "record.new",
            Some(dst.clone()),
            args,
            Some(span.clone()),
            None,
        );
        Ok(dst)
    }

    fn lower_literal(&mut self, literal: &Literal, span: std::ops::Range<usize>) -> String {
        match literal {
            Literal::Integer { value, .. } => {
                let dst = self.temp_local(&Ty::I64, Some(span.clone()));
                self.emit_instruction(
                    "const.i64",
                    Some(dst.clone()),
                    vec![i64_literal_operand(*value)],
                    Some(span),
                    None,
                );
                dst
            }
            Literal::Float(value) => {
                let ty = self.ty_for_span(&span);
                let (literal_ty, opcode) = if ty == Ty::F32 {
                    (Ty::F32, "const.f32")
                } else {
                    (Ty::F64, "const.f64")
                };
                let dst = self.temp_local(&literal_ty, Some(span.clone()));
                self.emit_instruction(
                    opcode,
                    Some(dst.clone()),
                    vec![Operand::literal(*value)],
                    Some(span),
                    None,
                );
                dst
            }
            Literal::String(value) => {
                let dst = self.temp_local(&Ty::String, Some(span.clone()));
                self.emit_instruction(
                    "const.string",
                    Some(dst.clone()),
                    vec![Operand::literal(value.clone())],
                    Some(span),
                    None,
                );
                dst
            }
            Literal::Bool(value) => {
                let dst = self.temp_local(&Ty::Bool, Some(span.clone()));
                self.emit_instruction(
                    "const.bool",
                    Some(dst.clone()),
                    vec![Operand::literal(*value)],
                    Some(span),
                    None,
                );
                dst
            }
            Literal::Char(value) => {
                let dst = self.temp_local(&Ty::Char, Some(span.clone()));
                self.emit_instruction(
                    "const.string",
                    Some(dst.clone()),
                    vec![Operand::literal(value.to_string())],
                    Some(span),
                    Some(json!({ "literal_kind": "char" })),
                );
                dst
            }
            Literal::Duration(value) => {
                let dst = self.temp_local(&Ty::Duration, Some(span.clone()));
                self.emit_instruction(
                    "const.i64",
                    Some(dst.clone()),
                    vec![i64_literal_operand(*value)],
                    Some(span),
                    Some(json!({ "literal_kind": "duration_ns" })),
                );
                dst
            }
        }
    }

    fn emit_const_unit(&mut self, span: Option<std::ops::Range<usize>>) -> String {
        let dst = self.temp_local(&Ty::Unit, span.clone());
        self.emit_instruction("const.unit", Some(dst.clone()), Vec::new(), span, None);
        dst
    }

    fn lower_binary(
        &mut self,
        left: &Spanned<Expr>,
        op: BinaryOp,
        right: &Spanned<Expr>,
        span: std::ops::Range<usize>,
    ) -> Result<String, CompileError> {
        match op {
            BinaryOp::And => return self.lower_logical_and(left, right, span),
            BinaryOp::Or => return self.lower_logical_or(left, right, span),
            _ => {}
        }

        let left_local = self.lower_expr(left)?;
        let right_local = self.lower_expr(right)?;
        // Arithmetic opcodes are type-directed: the native semantics of
        // `+ - * / %` differ between integers (checked, traps on overflow /
        // divide-by-zero) and floats (IEEE-754, never traps). Dispatch the
        // opcode family on the resolved operand type rather than `BinaryOp`
        // alone. The result type of an arithmetic op equals its operand type,
        // but comparisons yield `bool`, so we read the operand type from the
        // left-hand expression (both operands share a numeric type by the
        // time the type checker accepts the expression).
        let operand_ty = self.ty_for_expr(left);
        let operand_is_f32 = operand_ty == Ty::F32;
        let operand_is_float = operand_ty.is_float();
        let opcode = match op {
            BinaryOp::Add if operand_is_f32 => "f32.add",
            BinaryOp::Subtract if operand_is_f32 => "f32.sub",
            BinaryOp::Multiply if operand_is_f32 => "f32.mul",
            BinaryOp::Divide if operand_is_f32 => "f32.div",
            BinaryOp::Modulo if operand_is_f32 => "f32.rem",
            BinaryOp::Add if operand_is_float => "f64.add",
            BinaryOp::Subtract if operand_is_float => "f64.sub",
            BinaryOp::Multiply if operand_is_float => "f64.mul",
            BinaryOp::Divide if operand_is_float => "f64.div",
            BinaryOp::Modulo if operand_is_float => "f64.rem",
            // WrappingAdd/Sub/Mul are integer-only operators; they cannot type
            // a float operand, so they remain i64.* regardless.
            BinaryOp::Add => "i64.checked_add",
            BinaryOp::Subtract => "i64.checked_sub",
            BinaryOp::Multiply => "i64.checked_mul",
            BinaryOp::Divide => "i64.checked_div",
            BinaryOp::Modulo => "i64.checked_rem",
            BinaryOp::WrappingAdd => "i64.add",
            BinaryOp::WrappingSub => "i64.sub",
            BinaryOp::WrappingMul => "i64.mul",
            BinaryOp::BitAnd => "i64.and",
            BinaryOp::BitOr => "i64.or",
            BinaryOp::BitXor => "i64.xor",
            BinaryOp::Shl => "i64.shl",
            BinaryOp::Shr => "i64.shr",
            // Comparisons are already type-polymorphic in the interpreter
            // (`compareScalar` dispatches on the runtime value kind), so a
            // single `cmp.*` opcode covers both i64 and f64 operands.
            BinaryOp::Equal => "cmp.eq",
            BinaryOp::NotEqual => "cmp.ne",
            BinaryOp::Less => "cmp.lt",
            BinaryOp::LessEqual => "cmp.le",
            BinaryOp::Greater => "cmp.gt",
            BinaryOp::GreaterEqual => "cmp.ge",
            _ => {
                self.emit_unsupported(Some(span.clone()));
                "trap"
            }
        };
        if opcode == "trap" {
            return Ok(self.emit_const_unit(Some(span)));
        }
        let ty = self.ty_for_span(&span);
        let dst = self.temp_local(&ty, Some(span.clone()));
        self.emit_instruction(
            opcode,
            Some(dst.clone()),
            vec![Operand::local(left_local), Operand::local(right_local)],
            Some(span),
            None,
        );
        Ok(dst)
    }

    fn lower_logical_and(
        &mut self,
        left: &Spanned<Expr>,
        right: &Spanned<Expr>,
        span: std::ops::Range<usize>,
    ) -> Result<String, CompileError> {
        self.lower_logical_binary(left, right, span, false, true)
    }

    fn lower_logical_or(
        &mut self,
        left: &Spanned<Expr>,
        right: &Spanned<Expr>,
        span: std::ops::Range<usize>,
    ) -> Result<String, CompileError> {
        self.lower_logical_binary(left, right, span, true, false)
    }

    fn lower_logical_binary(
        &mut self,
        left: &Spanned<Expr>,
        right: &Spanned<Expr>,
        span: std::ops::Range<usize>,
        short_circuit_value: bool,
        rhs_on_true: bool,
    ) -> Result<String, CompileError> {
        let result = self.declare_local(None, &Ty::Bool, true, Some(span.clone()));
        let default = self.lower_literal(&Literal::Bool(short_circuit_value), span.clone());
        self.emit_instruction(
            "local.set",
            None,
            vec![Operand::local(result.clone()), Operand::local(default)],
            Some(span.clone()),
            None,
        );

        let span_ref = self.package.spans.span_ref(&span);
        let (rhs_idx, rhs_id) = self.new_block("logical_rhs", span_ref.clone());
        let (exit_idx, exit_id) = self.new_block("logical_exit", span_ref.clone());
        let left_local = self.lower_expr(left)?;
        let (then_id, else_id) = if rhs_on_true {
            (rhs_id.clone(), exit_id.clone())
        } else {
            (exit_id.clone(), rhs_id.clone())
        };
        self.terminate(Terminator::br_if(
            Operand::local(left_local),
            then_id,
            else_id,
            Vec::new(),
            span_ref.clone(),
        ));

        self.switch_to(rhs_idx);
        let right_local = self.lower_expr(right)?;
        self.emit_instruction(
            "local.set",
            None,
            vec![Operand::local(result.clone()), Operand::local(right_local)],
            Some(right.1.clone()),
            None,
        );
        self.terminate(Terminator::br(exit_id, Vec::new(), span_ref));

        self.switch_to(exit_idx);
        Ok(result)
    }

    #[expect(
        clippy::too_many_lines,
        reason = "lower_call dispatches all call forms and built-in stdlib symbols fail-closed; splitting would obscure the dispatch table"
    )]
    fn lower_call(
        &mut self,
        function: &Spanned<Expr>,
        args: &[CallArg],
        span: std::ops::Range<usize>,
    ) -> Result<String, CompileError> {
        match &function.0 {
            Expr::Identifier(name) if name == "println" => {
                let symbol = self.package.register_stdout();
                let mut operands = vec![Operand::symbol(symbol)];
                for arg in args {
                    operands.push(Operand::local(self.lower_expr(arg.expr())?));
                }
                self.emit_instruction("call.stdlib", None, operands, Some(span.clone()), None);
                Ok(self.emit_const_unit(Some(span)))
            }
            Expr::Identifier(name) if name == "print" => {
                let symbol = self.package.register_stdout_print();
                let mut operands = vec![Operand::symbol(symbol)];
                for arg in args {
                    operands.push(Operand::local(self.lower_expr(arg.expr())?));
                }
                self.emit_instruction("call.stdlib", None, operands, Some(span.clone()), None);
                Ok(self.emit_const_unit(Some(span)))
            }
            Expr::Identifier(name) if name == "panic" => {
                let message = if let Some(arg) = args.first() {
                    self.lower_expr(arg.expr())?
                } else {
                    self.lower_literal(&Literal::String("panic".to_string()), span.clone())
                };
                self.emit_instruction(
                    "panic",
                    None,
                    vec![Operand::local(message)],
                    Some(span.clone()),
                    None,
                );
                Ok(self.emit_const_unit(Some(span)))
            }
            Expr::Identifier(name) if name == "Vec::new" => Ok(self.lower_vector_new(span)),
            // Builtin Option/Result constructors: Some, None, Ok, Err.
            // These are NOT registered in enum_variant_tags (Option/Result are
            // builtins, not user-declared) so they need explicit handling.
            // Tag convention: Some/Ok = 0, None/Err = 1 (matches native codegen).
            Expr::Identifier(name) if matches!(name.as_str(), "Some" | "None" | "Ok" | "Err") => {
                let result_ty = self.ty_for_span(&span);
                self.package.ensure_option_result_layout(&result_ty);
                let type_id = self.package.type_id_for_ty(&result_ty);
                let tag: u64 = match name.as_str() {
                    "Some" | "Ok" => 0,
                    "None" | "Err" => 1,
                    _ => unreachable!(),
                };
                let mut operands = vec![Operand::ty(type_id), Operand::literal(tag)];
                for arg in args {
                    operands.push(Operand::local(self.lower_expr(arg.expr())?));
                }
                let dst = self.temp_local(&result_ty, Some(span.clone()));
                self.emit_instruction("enum.new", Some(dst.clone()), operands, Some(span), None);
                Ok(dst)
            }
            Expr::Identifier(name) if self.package.enum_variant_tags.contains_key(name) => {
                let (type_id, tag, _) = self.package.enum_variant_tags.get(name).cloned().unwrap();
                let mut operands = vec![Operand::ty(type_id), Operand::literal(tag as u64)];
                for arg in args {
                    operands.push(Operand::local(self.lower_expr(arg.expr())?));
                }
                let call_expr = (
                    Expr::Call {
                        function: Box::new(function.clone()),
                        type_args: None,
                        args: args.to_vec(),
                        is_tail_call: false,
                    },
                    span.clone(),
                );
                let ty = self.ty_for_expr(&call_expr);
                let dst = self.temp_local(&ty, Some(span.clone()));
                self.emit_instruction("enum.new", Some(dst.clone()), operands, Some(span), None);
                Ok(dst)
            }
            Expr::Identifier(name) if self.package.user_functions.contains(name) => {
                let mut operands = vec![Operand::function(function_id(name))];
                for arg in args {
                    operands.push(Operand::local(self.lower_expr(arg.expr())?));
                }
                let ty = self.ty_for_span(&span);
                let dst = self.temp_local(&ty, Some(span.clone()));
                self.emit_instruction("call.direct", Some(dst.clone()), operands, Some(span), None);
                Ok(dst)
            }
            Expr::FieldAccess { object, field } => {
                if let Expr::Identifier(module) = &object.0 {
                    if module == "regex" && field == "new" {
                        let pattern = args
                            .first()
                            .map(|arg| self.lower_expr(arg.expr()))
                            .transpose()?;
                        let pattern = pattern.unwrap_or_else(|| {
                            self.lower_literal(&Literal::String(String::new()), span.clone())
                        });
                        let dst = self.temp_local(
                            &Ty::Named {
                                name: "Regex".to_string(),
                                args: Vec::new(),
                                builtin: None,
                            },
                            Some(span.clone()),
                        );
                        self.package.register_regex_symbol("compile", "type:regex");
                        self.emit_instruction(
                            "regex.compile",
                            Some(dst.clone()),
                            vec![Operand::local(pattern)],
                            Some(span),
                            None,
                        );
                        return Ok(dst);
                    }
                    if module == "Vec" && field == "new" {
                        return Ok(self.lower_vector_new(span));
                    }
                }
                // `(rec.f)(args)` — fn-field call. The object is a record whose
                // field `f` holds a function value (emitted as `const.function` when
                // the function name appears in value position, stored via `record.new`
                // and retrieved via `record.get`).  Emit `record.get` to load the
                // function value, then `call.indirect` to invoke it.
                let object_ty = self.ty_for_expr(object);
                let Some(field_index) = self.resolve_record_field_index(&object_ty, field) else {
                    self.emit_unsupported(Some(span.clone()));
                    return Ok(self.emit_const_unit(Some(span)));
                };
                let object_local = self.lower_expr(object)?;
                // Get the function-kind value out of the field.
                let callee_ty = self.ty_for_span(&function.1);
                let callee_local = self.temp_local(&callee_ty, Some(span.clone()));
                self.emit_instruction(
                    "record.get",
                    Some(callee_local.clone()),
                    vec![
                        Operand::local(object_local),
                        Operand::literal(field_index as u64),
                    ],
                    Some(span.clone()),
                    None,
                );
                let result_ty = self.ty_for_span(&span);
                let dst = self.temp_local(&result_ty, Some(span.clone()));
                let mut operands = vec![Operand::local(callee_local)];
                for arg in args {
                    operands.push(Operand::local(self.lower_expr(arg.expr())?));
                }
                self.emit_instruction(
                    "call.indirect",
                    Some(dst.clone()),
                    operands,
                    Some(span),
                    None,
                );
                Ok(dst)
            }
            _ => {
                self.emit_unsupported(Some(span.clone()));
                Ok(self.emit_const_unit(Some(span)))
            }
        }
    }

    fn lower_vector_new(&mut self, span: std::ops::Range<usize>) -> String {
        let vector_ty = self.ty_for_span(&span);
        let element_ty = match &vector_ty {
            Ty::Named { args, .. } => args.first().cloned().unwrap_or(Ty::Unit),
            Ty::Array(element, _) | Ty::Slice(element) => (**element).clone(),
            _ => Ty::Unit,
        };
        let element_type_id = self.package.type_id_for_ty(&element_ty);
        let dst = self.temp_local(&vector_ty, Some(span.clone()));
        self.emit_instruction(
            "vector.new",
            Some(dst.clone()),
            vec![Operand::ty(element_type_id)],
            Some(span),
            None,
        );
        dst
    }

    fn lower_spawn(
        &mut self,
        target: &Spanned<Expr>,
        args: &[(String, Spanned<Expr>)],
        span: std::ops::Range<usize>,
    ) -> Result<String, CompileError> {
        let Expr::Identifier(actor_name) = &target.0 else {
            self.emit_unsupported(Some(span.clone()));
            return Ok(self.emit_const_unit(Some(span)));
        };

        // `spawn Supervisor` boots the whole tree; its child init state is baked
        // into the layout, so the op carries only the layout id.
        if self.package.supervisor_names.contains(actor_name) {
            let type_id = self.package.type_id_for_named(actor_name, &[]);
            let supervisor_ty = Ty::Named {
                name: actor_name.clone(),
                args: Vec::new(),
                builtin: None,
            };
            let dst = self.temp_local(&supervisor_ty, Some(span.clone()));
            self.emit_instruction(
                "supervisor.spawn",
                Some(dst.clone()),
                vec![Operand::ty(type_id)],
                Some(span),
                None,
            );
            return Ok(dst);
        }

        let Some(field_order) = self.package.actor_field_order.get(actor_name).cloned() else {
            // Not an admitted actor (e.g. a supervisor handle); reject cleanly.
            self.emit_unsupported(Some(span.clone()));
            return Ok(self.emit_const_unit(Some(span)));
        };

        // Lower the field initializers, then order them by the actor's declared
        // field layout so they line up with `state_fields` at spawn time.
        let mut value_by_field = BTreeMap::new();
        for (field_name, value) in args {
            let local = self.lower_expr(value)?;
            value_by_field.insert(field_name.clone(), local);
        }
        let type_id = self.package.type_id_for_named(actor_name, &[]);
        let mut operands = vec![Operand::ty(type_id)];
        for field_name in &field_order {
            if let Some(local) = value_by_field.get(field_name) {
                operands.push(Operand::local(local.clone()));
            } else {
                // A field without an initializer would spawn with the wrong
                // arity; the profile admits actors whose fields are all set at
                // spawn, so this is a defensive trap rather than a silent unit.
                self.emit_unsupported(Some(span.clone()));
                return Ok(self.emit_const_unit(Some(span)));
            }
        }

        let actor_ty = Ty::Named {
            name: actor_name.clone(),
            args: Vec::new(),
            builtin: None,
        };
        let dst = self.temp_local(&actor_ty, Some(span.clone()));
        self.emit_instruction("actor.spawn", Some(dst.clone()), operands, Some(span), None);
        Ok(dst)
    }

    fn lower_await(
        &mut self,
        operand: &Spanned<Expr>,
        span: std::ops::Range<usize>,
    ) -> Result<String, CompileError> {
        // The admitted await form is the actor ask: `await actor.handler(args)`.
        let Expr::MethodCall {
            receiver,
            method,
            args,
        } = &operand.0
        else {
            self.emit_unsupported(Some(span.clone()));
            return Ok(self.emit_const_unit(Some(span)));
        };

        let actor_local = self.lower_expr(receiver)?;
        let mut operands = vec![Operand::local(actor_local), Operand::symbol(method.clone())];
        for arg in args {
            operands.push(Operand::local(self.lower_expr(arg.expr())?));
        }

        let result_ty = self.ty_for_span(&span);
        // An actor ask is statically `Result<T, E>`. The educational VM resolves
        // the ask to the raw reply value (an ask failure traps rather than
        // yielding `Err`), so wrap the reply in `Ok(value)` to honour the type
        // and let callers `match` on `Ok`/`Err`.
        if let Ty::Named {
            name,
            args: ty_args,
            ..
        } = &result_ty
        {
            if name == "Result" && ty_args.len() == 2 {
                let ok_ty = ty_args[0].clone();
                let err_ty = ty_args[1].clone();
                let result_type_id = self.package.register_result_enum(&ok_ty, &err_ty);
                let reply_local = self.temp_local(&ok_ty, Some(span.clone()));
                self.emit_instruction(
                    "actor.ask",
                    Some(reply_local.clone()),
                    operands,
                    Some(span.clone()),
                    None,
                );
                let dst = self.temp_local(&result_ty, Some(span.clone()));
                self.emit_instruction(
                    "enum.new",
                    Some(dst.clone()),
                    vec![
                        Operand::ty(result_type_id),
                        Operand::literal(0u64),
                        Operand::local(reply_local),
                    ],
                    Some(span),
                    None,
                );
                return Ok(dst);
            }
        }

        let dst = self.temp_local(&result_ty, Some(span.clone()));
        self.emit_instruction("actor.ask", Some(dst.clone()), operands, Some(span), None);
        Ok(dst)
    }

    fn lower_machine_method(
        &mut self,
        receiver: &Spanned<Expr>,
        method: &str,
        args: &[CallArg],
        span: std::ops::Range<usize>,
    ) -> Result<String, CompileError> {
        if method == "state_name" {
            let machine_local = self.lower_expr(receiver)?;
            let dst = self.temp_local(&Ty::String, Some(span.clone()));
            self.emit_instruction(
                "machine.state",
                Some(dst.clone()),
                vec![Operand::local(machine_local)],
                Some(span),
                None,
            );
            return Ok(dst);
        }

        // `machine.step(Event)` mutates the machine in place, so the receiver
        // must be a bound local we can write the next state back into.
        let Expr::Identifier(name) = &receiver.0 else {
            self.emit_unsupported(Some(span.clone()));
            return Ok(self.emit_const_unit(Some(span)));
        };
        let Some(local) = self.bindings.get(name).cloned() else {
            self.emit_unsupported(Some(span.clone()));
            return Ok(self.emit_const_unit(Some(span)));
        };
        let Some(Expr::Identifier(event)) = args.first().map(|arg| &arg.expr().0) else {
            self.emit_unsupported(Some(span.clone()));
            return Ok(self.emit_const_unit(Some(span)));
        };
        // Accept a bare `Event` or a `Machine::Event` path.
        let event_name = event.rsplit("::").next().unwrap_or(event).to_string();
        self.emit_instruction(
            "machine.step",
            Some(local.clone()),
            vec![Operand::local(local), Operand::symbol(event_name)],
            Some(span.clone()),
            None,
        );
        Ok(self.emit_const_unit(Some(span)))
    }

    #[expect(
        clippy::too_many_lines,
        reason = "method-call lowering maps each admitted sandbox opcode family in one dispatch point"
    )]
    fn lower_method_call(
        &mut self,
        receiver: &Spanned<Expr>,
        method: &str,
        args: &[CallArg],
        span: std::ops::Range<usize>,
    ) -> Result<String, CompileError> {
        if matches!((&receiver.0, method), (Expr::Identifier(module), "new") if module == "regex") {
            let pattern = args
                .first()
                .map(|arg| self.lower_expr(arg.expr()))
                .transpose()?
                .unwrap_or_else(|| {
                    self.lower_literal(&Literal::String(String::new()), span.clone())
                });
            let dst = self.temp_local(
                &Ty::Named {
                    name: "Regex".to_string(),
                    args: Vec::new(),
                    builtin: None,
                },
                Some(span.clone()),
            );
            self.package.register_regex_symbol("compile", "type:regex");
            self.emit_instruction(
                "regex.compile",
                Some(dst.clone()),
                vec![Operand::local(pattern)],
                Some(span),
                None,
            );
            return Ok(dst);
        }

        // Machine state queries / event steps on a declared machine handle.
        if matches!(method, "step" | "state_name") {
            if let Ty::Named { name, .. } = self.ty_for_expr(receiver) {
                if self.package.machine_names.contains(&name) {
                    return self.lower_machine_method(receiver, method, args, span);
                }
            }
        }

        let receiver_local = self.lower_expr(receiver)?;
        match method {
            "find" | "is_match" | "replace" => {
                let opcode = match method {
                    "find" => "regex.find",
                    "is_match" => "regex.is_match",
                    "replace" => "regex.replace",
                    _ => unreachable!(),
                };
                let result_ty = if method == "is_match" {
                    Ty::Bool
                } else {
                    Ty::String
                };
                let mut operands = vec![Operand::local(receiver_local)];
                for arg in args {
                    operands.push(Operand::local(self.lower_expr(arg.expr())?));
                }
                let dst = self.temp_local(&result_ty, Some(span.clone()));
                let result_type_id = self.package.type_id_for_ty(&result_ty);
                self.package.register_regex_symbol(method, &result_type_id);
                self.emit_instruction(opcode, Some(dst.clone()), operands, Some(span), None);
                Ok(dst)
            }
            // `Pattern.close()` is the surface resource-release method (Pattern
            // is a `#[resource]`, renamed from `free()` in the resource
            // migration). The sandbox VM models a compiled regex as a GC'd JS
            // `RegExp`, so the release lowers to the `regex.free` no-op marker op
            // and the `std.text.regex.free` handle-free symbol — the VM-layer
            // name for the same action. Keeping the op/symbol stable keeps the
            // golden bytecode fixtures byte-identical across the rename.
            "close" => {
                self.package.register_regex_symbol("free", "type:unit");
                self.emit_instruction(
                    "regex.free",
                    None,
                    vec![Operand::local(receiver_local)],
                    Some(span.clone()),
                    None,
                );
                Ok(self.emit_const_unit(Some(span)))
            }
            "len" => {
                let dst = self.temp_local(&Ty::I64, Some(span.clone()));
                let opcode = if self.ty_for_expr(receiver) == Ty::String {
                    "string.len"
                } else {
                    "vector.len"
                };
                self.emit_instruction(
                    opcode,
                    Some(dst.clone()),
                    vec![Operand::local(receiver_local)],
                    Some(span),
                    None,
                );
                Ok(dst)
            }
            "push" => {
                let mut operands = vec![Operand::local(receiver_local)];
                for arg in args {
                    operands.push(Operand::local(self.lower_expr(arg.expr())?));
                }
                self.emit_instruction("vector.push", None, operands, Some(span.clone()), None);
                Ok(self.emit_const_unit(Some(span)))
            }
            "get" => {
                let Some(index_arg) = args.first() else {
                    self.emit_unsupported(Some(span.clone()));
                    return Ok(self.emit_const_unit(Some(span)));
                };
                let index = self.lower_expr(index_arg.expr())?;
                let result_ty = self.ty_for_span(&span);
                let dst = self.temp_local(&result_ty, Some(span.clone()));
                self.emit_instruction(
                    "vector.get",
                    Some(dst.clone()),
                    vec![Operand::local(receiver_local), Operand::local(index)],
                    Some(span),
                    None,
                );
                Ok(dst)
            }
            "slice" => {
                let Some(start_arg) = args.first() else {
                    self.emit_unsupported(Some(span.clone()));
                    return Ok(self.emit_const_unit(Some(span)));
                };
                let Some(end_arg) = args.get(1) else {
                    self.emit_unsupported(Some(span.clone()));
                    return Ok(self.emit_const_unit(Some(span)));
                };
                let start = self.lower_expr(start_arg.expr())?;
                let end = self.lower_expr(end_arg.expr())?;
                let dst = self.temp_local(&Ty::String, Some(span.clone()));
                self.emit_instruction(
                    "string.slice",
                    Some(dst.clone()),
                    vec![
                        Operand::local(receiver_local),
                        Operand::local(start),
                        Operand::local(end),
                    ],
                    Some(span),
                    None,
                );
                Ok(dst)
            }
            // `vec.contains(elem)` — linear scan using canonical equality,
            // the same structural equality as `cmp.eq`. Emits `vector.contains`.
            "contains" => {
                let Some(elem_arg) = args.first() else {
                    self.emit_unsupported(Some(span.clone()));
                    return Ok(self.emit_const_unit(Some(span)));
                };
                let elem_local = self.lower_expr(elem_arg.expr())?;
                let dst = self.temp_local(&Ty::Bool, Some(span.clone()));
                self.emit_instruction(
                    "vector.contains",
                    Some(dst.clone()),
                    vec![Operand::local(receiver_local), Operand::local(elem_local)],
                    Some(span),
                    None,
                );
                Ok(dst)
            }
            // `x.to_string()` for scalar types — emit a `fmt.to_string` stdlib
            // call. The VM's `fmt.to_string` handler routes through `renderStdout`
            // which produces the same decimal/float representation as native.
            "to_string" => {
                let symbol_id = self.package.register_fmt_to_string();
                let dst = self.temp_local(&Ty::String, Some(span.clone()));
                self.emit_instruction(
                    "call.stdlib",
                    Some(dst.clone()),
                    vec![Operand::symbol(symbol_id), Operand::local(receiver_local)],
                    Some(span),
                    None,
                );
                Ok(dst)
            }
            // Option/Result predicate and extraction methods. Both types use the
            // same tag convention: Some/Ok = tag 0, None/Err = tag 1.
            "is_some" | "is_none" | "is_ok" | "is_err" => {
                let receiver_ty = self.ty_for_expr(receiver);
                let is_option = matches!(
                    receiver_ty,
                    Ty::Named {
                        builtin: Some(BuiltinType::Option),
                        ..
                    }
                );
                let is_result = matches!(
                    receiver_ty,
                    Ty::Named {
                        builtin: Some(BuiltinType::Result),
                        ..
                    }
                );
                if !is_option && !is_result {
                    self.emit_unsupported(Some(span.clone()));
                    return Ok(self.emit_const_unit(Some(span)));
                }
                self.package.ensure_option_result_layout(&receiver_ty);
                // Some = tag 0, None = tag 1. Ok = tag 0, Err = tag 1.
                let positive_tag_value: u64 = 0; // Some / Ok
                let positive = matches!(method, "is_some" | "is_ok");
                let tag_local = self.temp_local(&Ty::I64, Some(span.clone()));
                self.emit_instruction(
                    "enum.tag",
                    Some(tag_local.clone()),
                    vec![Operand::local(receiver_local)],
                    Some(span.clone()),
                    None,
                );
                let tag_check = self.temp_local(&Ty::I64, Some(span.clone()));
                self.emit_instruction(
                    "const.i64",
                    Some(tag_check.clone()),
                    vec![Operand::literal(positive_tag_value)],
                    Some(span.clone()),
                    None,
                );
                let dst = self.temp_local(&Ty::Bool, Some(span.clone()));
                let cmp_op = if positive { "cmp.eq" } else { "cmp.ne" };
                self.emit_instruction(
                    cmp_op,
                    Some(dst.clone()),
                    vec![Operand::local(tag_local), Operand::local(tag_check)],
                    Some(span),
                    None,
                );
                Ok(dst)
            }
            "unwrap" => {
                let receiver_ty = self.ty_for_expr(receiver);
                let is_option = matches!(
                    receiver_ty,
                    Ty::Named {
                        builtin: Some(BuiltinType::Option),
                        ..
                    }
                );
                let is_result = matches!(
                    receiver_ty,
                    Ty::Named {
                        builtin: Some(BuiltinType::Result),
                        ..
                    }
                );
                if !is_option && !is_result {
                    self.emit_unsupported(Some(span.clone()));
                    return Ok(self.emit_const_unit(Some(span)));
                }
                self.package.ensure_option_result_layout(&receiver_ty);
                Ok(self.lower_option_result_unwrap(receiver_local, is_option, span))
            }
            "unwrap_or" => {
                let receiver_ty = self.ty_for_expr(receiver);
                let is_option = matches!(
                    receiver_ty,
                    Ty::Named {
                        builtin: Some(BuiltinType::Option),
                        ..
                    }
                );
                let is_result = matches!(
                    receiver_ty,
                    Ty::Named {
                        builtin: Some(BuiltinType::Result),
                        ..
                    }
                );
                if !is_option && !is_result {
                    self.emit_unsupported(Some(span.clone()));
                    return Ok(self.emit_const_unit(Some(span)));
                }
                self.package.ensure_option_result_layout(&receiver_ty);
                let Some(default_arg) = args.first() else {
                    self.emit_unsupported(Some(span.clone()));
                    return Ok(self.emit_const_unit(Some(span)));
                };
                let default_local = self.lower_expr(default_arg.expr())?;
                Ok(self.lower_option_result_unwrap_or(receiver_local, default_local, span))
            }
            // `rec.clone()` produces an independent deep copy of any record
            // value.  The sandbox VM's `local.set` unconditionally calls
            // `cloneValue` (a deep recursive copy), so writing the receiver into
            // a fresh temp via `local.set` is exactly what is needed: the result
            // is a new record object with independently cloned fields — matching
            // native `__hew_record_clone_inplace_<R>` ownership semantics.
            "clone" => {
                let ty = self.ty_for_expr(receiver);
                let dst = self.temp_local(&ty, Some(span.clone()));
                self.emit_instruction(
                    "local.set",
                    None,
                    vec![Operand::local(dst.clone()), Operand::local(receiver_local)],
                    Some(span),
                    None,
                );
                Ok(dst)
            }
            _ => {
                self.emit_unsupported(Some(span.clone()));
                Ok(self.emit_const_unit(Some(span)))
            }
        }
    }

    /// Lower `opt.unwrap()` / `res.unwrap()` for Option and Result.
    ///
    /// Emits an enum-tag check: tag == 0 (Some/Ok) → extract and return
    /// payload[0]; tag != 0 (None/Err) → panic.
    fn lower_option_result_unwrap(
        &mut self,
        receiver_local: String,
        is_option: bool,
        span: std::ops::Range<usize>,
    ) -> String {
        let result_ty = self.ty_for_span(&span);
        let result_local = self.declare_local(None, &result_ty, true, Some(span.clone()));
        let span_ref = self.package.spans.span_ref(&span);
        let (some_idx, some_id) = self.new_block("unwrap_some", span_ref.clone());
        let (panic_idx, panic_id) = self.new_block("unwrap_panic", span_ref.clone());
        let (exit_idx, exit_id) = self.new_block("unwrap_exit", span_ref.clone());

        let tag_local = self.temp_local(&Ty::I64, Some(span.clone()));
        self.emit_instruction(
            "enum.tag",
            Some(tag_local.clone()),
            vec![Operand::local(receiver_local.clone())],
            Some(span.clone()),
            None,
        );
        let zero_local = self.temp_local(&Ty::I64, Some(span.clone()));
        self.emit_instruction(
            "const.i64",
            Some(zero_local.clone()),
            vec![Operand::literal(0_u64)],
            Some(span.clone()),
            None,
        );
        let cond = self.temp_local(&Ty::Bool, Some(span.clone()));
        self.emit_instruction(
            "cmp.eq",
            Some(cond.clone()),
            vec![Operand::local(tag_local), Operand::local(zero_local)],
            Some(span.clone()),
            None,
        );
        self.terminate(Terminator::br_if(
            Operand::local(cond),
            some_id,
            panic_id,
            vec![],
            span_ref.clone(),
        ));

        // Some/Ok branch: extract payload[0]
        self.switch_to(some_idx);
        let payload_local = self.temp_local(&result_ty, Some(span.clone()));
        self.emit_instruction(
            "enum.payload",
            Some(payload_local.clone()),
            vec![Operand::local(receiver_local), Operand::literal(0_u64)],
            Some(span.clone()),
            None,
        );
        self.emit_instruction(
            "local.set",
            None,
            vec![
                Operand::local(result_local.clone()),
                Operand::local(payload_local),
            ],
            Some(span.clone()),
            None,
        );
        self.terminate(Terminator::br(exit_id.clone(), vec![], span_ref.clone()));

        // None/Err branch: panic
        self.switch_to(panic_idx);
        let msg_text = if is_option {
            "unwrap called on None"
        } else {
            "unwrap called on Err"
        };
        let msg_local = self.lower_literal(&Literal::String(msg_text.to_string()), span.clone());
        self.emit_instruction(
            "panic",
            None,
            vec![Operand::local(msg_local)],
            Some(span.clone()),
            None,
        );
        self.terminate(Terminator::br(exit_id, vec![], span_ref));

        self.switch_to(exit_idx);
        result_local
    }

    /// Lower `opt.unwrap_or(default)` / `res.unwrap_or(default)`.
    ///
    /// Emits an enum-tag check: tag == 0 (Some/Ok) → extract payload[0];
    /// tag != 0 (None/Err) → return the pre-evaluated default.
    fn lower_option_result_unwrap_or(
        &mut self,
        receiver_local: String,
        default_local: String,
        span: std::ops::Range<usize>,
    ) -> String {
        let result_ty = self.ty_for_span(&span);
        let result_local = self.declare_local(None, &result_ty, true, Some(span.clone()));
        let span_ref = self.package.spans.span_ref(&span);
        let (some_idx, some_id) = self.new_block("unwrap_or_some", span_ref.clone());
        let (default_idx, default_id) = self.new_block("unwrap_or_default", span_ref.clone());
        let (exit_idx, exit_id) = self.new_block("unwrap_or_exit", span_ref.clone());

        let tag_local = self.temp_local(&Ty::I64, Some(span.clone()));
        self.emit_instruction(
            "enum.tag",
            Some(tag_local.clone()),
            vec![Operand::local(receiver_local.clone())],
            Some(span.clone()),
            None,
        );
        let zero_local = self.temp_local(&Ty::I64, Some(span.clone()));
        self.emit_instruction(
            "const.i64",
            Some(zero_local.clone()),
            vec![Operand::literal(0_u64)],
            Some(span.clone()),
            None,
        );
        let cond = self.temp_local(&Ty::Bool, Some(span.clone()));
        self.emit_instruction(
            "cmp.eq",
            Some(cond.clone()),
            vec![Operand::local(tag_local), Operand::local(zero_local)],
            Some(span.clone()),
            None,
        );
        self.terminate(Terminator::br_if(
            Operand::local(cond),
            some_id,
            default_id,
            vec![],
            span_ref.clone(),
        ));

        // Some/Ok branch: extract payload[0]
        self.switch_to(some_idx);
        let payload_local = self.temp_local(&result_ty, Some(span.clone()));
        self.emit_instruction(
            "enum.payload",
            Some(payload_local.clone()),
            vec![Operand::local(receiver_local), Operand::literal(0_u64)],
            Some(span.clone()),
            None,
        );
        self.emit_instruction(
            "local.set",
            None,
            vec![
                Operand::local(result_local.clone()),
                Operand::local(payload_local),
            ],
            Some(span.clone()),
            None,
        );
        self.terminate(Terminator::br(exit_id.clone(), vec![], span_ref.clone()));

        // None/Err branch: use the default
        self.switch_to(default_idx);
        self.emit_instruction(
            "local.set",
            None,
            vec![
                Operand::local(result_local.clone()),
                Operand::local(default_local),
            ],
            Some(span.clone()),
            None,
        );
        self.terminate(Terminator::br(exit_id, vec![], span_ref));

        self.switch_to(exit_idx);
        result_local
    }

    fn lower_match(
        &mut self,
        scrutinee: &Spanned<Expr>,
        arms: &[MatchArm],
        span: std::ops::Range<usize>,
    ) -> Result<String, CompileError> {
        let scrutinee_ty = self.ty_for_expr(scrutinee);
        self.package.ensure_option_result_layout(&scrutinee_ty);
        let scrutinee_local = self.lower_expr(scrutinee)?;
        let result_ty = self.ty_for_span(&span);
        let result_local = self.declare_local(None, &result_ty, true, Some(span.clone()));
        let tag_local = if self.match_uses_enum_tag(&scrutinee_ty) {
            let local = self.temp_local(&Ty::I64, Some(scrutinee.1.clone()));
            self.emit_instruction(
                "enum.tag",
                Some(local.clone()),
                vec![Operand::local(scrutinee_local.clone())],
                Some(scrutinee.1.clone()),
                None,
            );
            Some(local)
        } else {
            None
        };

        let exit_span = self.package.spans.span_ref(&span);
        let (exit_idx, exit_id) = self.new_block("match_exit", exit_span);
        let (check_blocks, guard_blocks, arm_blocks) = self.new_match_blocks(arms);

        if let Some((first_idx, first_id)) = check_blocks.first() {
            let span_ref = self.package.spans.span_ref(&span);
            self.terminate(Terminator::br(first_id.clone(), Vec::new(), span_ref));
            self.switch_to(*first_idx);
        }

        for (index, arm) in arms.iter().enumerate() {
            let (check_idx, _) = check_blocks[index].clone();
            self.switch_to(check_idx);
            let (_, arm_id) = &arm_blocks[index];
            let matched_id = guard_blocks[index]
                .as_ref()
                .map_or_else(|| arm_id.clone(), |(_, id)| id.clone());
            let pattern_is_catch_all = self.pattern_is_catch_all(&arm.pattern, &scrutinee_ty);
            let else_id = Self::next_match_arm_id(
                index,
                arm.guard.is_some(),
                pattern_is_catch_all,
                &check_blocks,
                &matched_id,
                arm_id,
                &exit_id,
            );
            let pattern_span = self.package.spans.span_ref(&arm.pattern.1);
            if self.pattern_is_unconditional(&arm.pattern, &scrutinee_ty) {
                self.terminate(Terminator::br(matched_id, Vec::new(), pattern_span));
            } else if let Some(cond) = self.lower_match_condition(
                &arm.pattern,
                &scrutinee_local,
                &scrutinee_ty,
                tag_local.as_deref(),
            ) {
                self.terminate(Terminator::br_if(
                    Operand::local(cond),
                    matched_id,
                    else_id.clone(),
                    Vec::new(),
                    pattern_span,
                ));
            }

            if let (Some(guard), Some((guard_idx, _))) = (&arm.guard, guard_blocks[index].clone()) {
                let guard_else_id = Self::match_guard_failure_id(index, &check_blocks, &exit_id);
                self.lower_match_guard_block(
                    guard,
                    guard_idx,
                    &arm.pattern,
                    &scrutinee_local,
                    &scrutinee_ty,
                    arm_id.clone(),
                    guard_else_id,
                )?;
            }

            let (arm_idx, _) = arm_blocks[index].clone();
            self.switch_to(arm_idx);
            let saved_bindings = self.bindings.clone();
            self.bind_pattern_payloads(&arm.pattern, &scrutinee_local, &scrutinee_ty);
            let value = self.lower_expr(&arm.body)?;
            self.emit_instruction(
                "local.set",
                None,
                vec![Operand::local(result_local.clone()), Operand::local(value)],
                Some(arm.body.1.clone()),
                None,
            );
            self.bindings = saved_bindings;
            let body_span = self.package.spans.span_ref(&arm.body.1);
            self.terminate(Terminator::br(exit_id.clone(), Vec::new(), body_span));
        }

        self.switch_to(exit_idx);
        Ok(result_local)
    }

    #[expect(
        clippy::too_many_lines,
        reason = "pattern binding handles every admitted pattern shell in one recursive dispatcher"
    )]
    fn bind_pattern_payloads(
        &mut self,
        pattern: &Spanned<Pattern>,
        scrutinee_local: &str,
        scrutinee_ty: &Ty,
    ) {
        match &pattern.0 {
            Pattern::Identifier(binding) if self.pattern_tag(pattern, scrutinee_ty).is_none() => {
                let local = self.declare_local(
                    Some(binding.clone()),
                    scrutinee_ty,
                    false,
                    Some(pattern.1.clone()),
                );
                self.emit_instruction(
                    "local.set",
                    None,
                    vec![
                        Operand::local(local.clone()),
                        Operand::local(scrutinee_local.to_string()),
                    ],
                    Some(pattern.1.clone()),
                    None,
                );
                self.bindings.insert(binding.clone(), local);
            }
            Pattern::Constructor { name, patterns } => {
                let payload_tys = self
                    .variant_info_for_name(name, scrutinee_ty)
                    .map(|(_, tys)| tys)
                    .unwrap_or_default();
                for (idx, payload_pattern) in patterns.iter().enumerate() {
                    if let Pattern::Identifier(binding) = &payload_pattern.0 {
                        let ty = payload_tys.get(idx).cloned().unwrap_or(Ty::Unit);
                        let local = self.declare_local(
                            Some(binding.clone()),
                            &ty,
                            false,
                            Some(payload_pattern.1.clone()),
                        );
                        self.emit_instruction(
                            "enum.payload",
                            Some(local.clone()),
                            vec![
                                Operand::local(scrutinee_local.to_string()),
                                Operand::literal(idx as u64),
                            ],
                            Some(payload_pattern.1.clone()),
                            None,
                        );
                        self.bindings.insert(binding.clone(), local);
                    }
                }
            }
            Pattern::Tuple(patterns) => {
                if let Ty::Tuple(elem_tys) = scrutinee_ty {
                    for (idx, subpattern) in patterns.iter().enumerate() {
                        if matches!(subpattern.0, Pattern::Wildcard) {
                            continue;
                        }
                        let ty = elem_tys.get(idx).cloned().unwrap_or(Ty::Unit);
                        let field = self.temp_local(&ty, Some(subpattern.1.clone()));
                        self.emit_instruction(
                            "record.get",
                            Some(field.clone()),
                            vec![
                                Operand::local(scrutinee_local.to_string()),
                                Operand::literal(idx as u64),
                            ],
                            Some(subpattern.1.clone()),
                            None,
                        );
                        self.bind_pattern_payloads(subpattern, &field, &ty);
                    }
                }
            }
            Pattern::Struct { fields, .. } | Pattern::RecordShorthand { fields } => {
                if let Some(layout) = self.struct_pattern_layout(scrutinee_ty) {
                    for field_pattern in fields {
                        let Some((field_idx, field_ty)) = layout
                            .iter()
                            .position(|(name, _)| name == &field_pattern.name)
                            .map(|idx| (idx, layout[idx].1.clone()))
                        else {
                            continue;
                        };
                        let field_local = match &field_pattern.pattern {
                            Some(pattern) if matches!(pattern.0, Pattern::Wildcard) => continue,
                            Some(pattern) => self.temp_local(&field_ty, Some(pattern.1.clone())),
                            None => self.declare_local(
                                Some(field_pattern.name.clone()),
                                &field_ty,
                                false,
                                Some(pattern.1.clone()),
                            ),
                        };
                        self.emit_instruction(
                            "record.get",
                            Some(field_local.clone()),
                            vec![
                                Operand::local(scrutinee_local.to_string()),
                                Operand::literal(field_idx as u64),
                            ],
                            Some(pattern.1.clone()),
                            None,
                        );
                        if let Some(subpattern) = &field_pattern.pattern {
                            self.bind_pattern_payloads(subpattern, &field_local, &field_ty);
                        } else {
                            self.bindings
                                .insert(field_pattern.name.clone(), field_local);
                        }
                    }
                }
            }
            _ => {}
        }
    }

    fn struct_pattern_layout(&self, scrutinee_ty: &Ty) -> Option<Vec<(String, Ty)>> {
        let Ty::Named { name, .. } = scrutinee_ty else {
            return None;
        };
        let type_def = self.package.type_output.type_defs.get(name)?;
        if !matches!(type_def.kind, TypeDefKind::Record | TypeDefKind::Struct) {
            return None;
        }
        Some(
            type_def
                .field_order
                .iter()
                .filter_map(|field_name| {
                    type_def
                        .fields
                        .get(field_name)
                        .cloned()
                        .map(|ty| (field_name.clone(), ty))
                })
                .collect(),
        )
    }

    fn new_match_blocks(&mut self, arms: &[MatchArm]) -> MatchBlocks {
        let mut check_blocks = Vec::new();
        let mut guard_blocks = Vec::new();
        let mut arm_blocks = Vec::new();
        for (index, arm) in arms.iter().enumerate() {
            let check_span = self.package.spans.span_ref(&arm.pattern.1);
            check_blocks.push(self.new_block(&format!("match_check_{index}"), check_span));
            guard_blocks.push(arm.guard.as_ref().map(|guard| {
                let guard_span = self.package.spans.span_ref(&guard.1);
                self.new_block(&format!("match_guard_{index}"), guard_span)
            }));
            let arm_span = self.package.spans.span_ref(&arm.body.1);
            arm_blocks.push(self.new_block(&format!("match_arm_{index}"), arm_span));
        }
        (check_blocks, guard_blocks, arm_blocks)
    }

    fn match_uses_enum_tag(&self, ty: &Ty) -> bool {
        match ty {
            Ty::Named {
                builtin: Some(BuiltinType::Option | BuiltinType::Result),
                ..
            } => true,
            Ty::Named { name, .. } => self
                .package
                .type_output
                .type_defs
                .get(name)
                .is_some_and(|def| def.kind == TypeDefKind::Enum),
            _ => false,
        }
    }

    fn variant_info_for_name(&self, name: &str, scrutinee_ty: &Ty) -> Option<(usize, Vec<Ty>)> {
        if let Some((_, tag, payload_tys)) = self.package.enum_variant_tags.get(name) {
            return Some((*tag, payload_tys.clone()));
        }
        let short_name = name.rsplit("::").next().unwrap_or(name);
        match scrutinee_ty {
            Ty::Named {
                builtin: Some(BuiltinType::Option),
                args,
                ..
            } => match short_name {
                "Some" => Some((0, vec![args.first().cloned().unwrap_or(Ty::Unit)])),
                "None" => Some((1, Vec::new())),
                _ => None,
            },
            Ty::Named {
                builtin: Some(BuiltinType::Result),
                args,
                ..
            } => match short_name {
                "Ok" => Some((0, vec![args.first().cloned().unwrap_or(Ty::Unit)])),
                "Err" => Some((1, vec![args.get(1).cloned().unwrap_or(Ty::Unit)])),
                _ => None,
            },
            _ => None,
        }
    }

    fn pattern_tag(&self, pattern: &Spanned<Pattern>, scrutinee_ty: &Ty) -> Option<usize> {
        match &pattern.0 {
            Pattern::Constructor { name, .. } | Pattern::Identifier(name) => self
                .variant_info_for_name(name, scrutinee_ty)
                .map(|(tag, _)| tag),
            _ => None,
        }
    }

    fn pattern_is_catch_all(&self, pattern: &Spanned<Pattern>, scrutinee_ty: &Ty) -> bool {
        match &pattern.0 {
            Pattern::Wildcard => true,
            Pattern::Identifier(_) => self.pattern_tag(pattern, scrutinee_ty).is_none(),
            Pattern::Struct { .. } | Pattern::RecordShorthand { .. } | Pattern::Tuple(_) => {
                !self.match_uses_enum_tag(scrutinee_ty)
            }
            _ => false,
        }
    }

    fn pattern_is_unconditional(&self, pattern: &Spanned<Pattern>, scrutinee_ty: &Ty) -> bool {
        self.pattern_is_catch_all(pattern, scrutinee_ty)
    }

    fn lower_match_condition(
        &mut self,
        pattern: &Spanned<Pattern>,
        scrutinee_local: &str,
        scrutinee_ty: &Ty,
        tag_local: Option<&str>,
    ) -> Option<String> {
        if let Some(tag_local) = tag_local {
            if let Some(tag) = self.pattern_tag(pattern, scrutinee_ty) {
                let tag_const = self.lower_literal(
                    &Literal::Integer {
                        value: i64::try_from(tag).unwrap_or(0),
                        radix: hew_parser::ast::IntRadix::Decimal,
                    },
                    pattern.1.clone(),
                );
                let cond = self.temp_local(&Ty::Bool, Some(pattern.1.clone()));
                self.emit_instruction(
                    "cmp.eq",
                    Some(cond.clone()),
                    vec![
                        Operand::local(tag_local.to_string()),
                        Operand::local(tag_const),
                    ],
                    Some(pattern.1.clone()),
                    None,
                );
                return Some(cond);
            }
        } else if let Pattern::Literal(literal) = &pattern.0 {
            let pattern_local = self.lower_literal(literal, pattern.1.clone());
            let cond = self.temp_local(&Ty::Bool, Some(pattern.1.clone()));
            self.emit_instruction(
                "cmp.eq",
                Some(cond.clone()),
                vec![
                    Operand::local(scrutinee_local.to_string()),
                    Operand::local(pattern_local),
                ],
                Some(pattern.1.clone()),
                None,
            );
            return Some(cond);
        }

        self.emit_unsupported(Some(pattern.1.clone()));
        None
    }

    fn next_match_arm_id(
        index: usize,
        has_guard: bool,
        pattern_is_catch_all: bool,
        check_blocks: &[MatchBlock],
        matched_id: &str,
        arm_id: &str,
        exit_id: &str,
    ) -> String {
        if pattern_is_catch_all {
            return matched_id.to_string();
        }
        check_blocks.get(index + 1).map_or_else(
            || {
                if has_guard {
                    exit_id.to_string()
                } else {
                    arm_id.to_string()
                }
            },
            |(_, id)| id.clone(),
        )
    }

    fn match_guard_failure_id(index: usize, check_blocks: &[MatchBlock], exit_id: &str) -> String {
        check_blocks
            .get(index + 1)
            .map_or_else(|| exit_id.to_string(), |(_, id)| id.clone())
    }

    #[allow(
        clippy::too_many_arguments,
        reason = "all 8 params are distinct per-call inputs (guard expr, \
                  block index, pattern, scrutinee, type, two branch target ids); \
                  no cohesive subset warrants a struct at one call site"
    )]
    fn lower_match_guard_block(
        &mut self,
        guard: &Spanned<Expr>,
        guard_idx: usize,
        pattern: &Spanned<Pattern>,
        scrutinee_local: &str,
        scrutinee_ty: &Ty,
        arm_id: String,
        else_id: String,
    ) -> Result<(), CompileError> {
        self.switch_to(guard_idx);
        let saved_bindings = self.bindings.clone();
        self.bind_pattern_payloads(pattern, scrutinee_local, scrutinee_ty);
        let guard_local = self.lower_expr(guard)?;
        self.bindings = saved_bindings;
        if self.current_is_terminated() {
            return Ok(());
        }
        let guard_span = self.package.spans.span_ref(&guard.1);
        self.terminate(Terminator::br_if(
            Operand::local(guard_local),
            arm_id,
            else_id,
            Vec::new(),
            guard_span,
        ));
        Ok(())
    }

    /// If `name` is a `Machine::State` path naming a declared machine, return
    /// `(machine_name, state_name)`.
    fn machine_state_path(&self, name: &str) -> Option<(String, String)> {
        let (machine, state) = name.split_once("::")?;
        if self.package.machine_names.contains(machine) {
            Some((machine.to_string(), state.to_string()))
        } else {
            None
        }
    }

    /// If `ty` is a pid handle whose type argument names a declared supervisor
    /// (e.g. `LocalPid<WorkerPool>`), return that supervisor's name.
    fn supervisor_handle_arg(&self, ty: &Ty) -> Option<String> {
        if let Ty::Named { args, .. } = ty {
            if let Some(Ty::Named { name, .. }) = args.first() {
                if self.package.supervisor_names.contains(name) {
                    return Some(name.clone());
                }
            }
        }
        None
    }

    fn ty_for_expr(&mut self, expr: &Spanned<Expr>) -> Ty {
        let key = SpanKey::from(&expr.1);
        self.package
            .type_output
            .expr_types
            .get(&key)
            .cloned()
            .unwrap_or(Ty::Unit)
            .materialize_literal_defaults()
    }

    fn ty_for_span(&mut self, span: &std::ops::Range<usize>) -> Ty {
        let key = SpanKey::from(span);
        self.package
            .type_output
            .expr_types
            .get(&key)
            .cloned()
            .unwrap_or(Ty::Unit)
            .materialize_literal_defaults()
    }

    /// Look up the numeric field index for a named-record field.
    ///
    /// Returns `Some(idx)` when the layout is known; `None` when the object
    /// type is not a named record or the field name is absent from the layout.
    /// Callers must fail-closed on `None` — never substitute index 0.
    fn resolve_record_field_index(&self, object_ty: &Ty, field: &str) -> Option<usize> {
        match object_ty {
            Ty::Named { name, .. } => self
                .package
                .record_field_indexes
                .get(name.as_str())
                .and_then(|indexes| indexes.get(field))
                .copied(),
            _ => None,
        }
    }

    fn resolve_loop_exit(&self, label: Option<&str>) -> Option<String> {
        if let Some(lbl) = label {
            self.loop_targets
                .iter()
                .rev()
                .find(|t| t.label.as_deref() == Some(lbl))
                .map(|t| t.exit_id.clone())
        } else {
            self.loop_targets.last().map(|t| t.exit_id.clone())
        }
    }

    fn resolve_loop_continue(&self, label: Option<&str>) -> Option<String> {
        if let Some(lbl) = label {
            self.loop_targets
                .iter()
                .rev()
                .find(|t| t.label.as_deref() == Some(lbl))
                .map(|t| t.continue_id.clone())
        } else {
            self.loop_targets.last().map(|t| t.continue_id.clone())
        }
    }

    #[expect(
        clippy::too_many_lines,
        reason = "emit_for_range lowers a range-based for loop through 4 CFG blocks; splitting by block would obscure the loop structure"
    )]
    fn emit_for_range(
        &mut self,
        label: Option<String>,
        pattern: &Spanned<Pattern>,
        iterable: &Spanned<Expr>,
        body: &AstBlock,
        span: std::ops::Range<usize>,
    ) -> Result<(), CompileError> {
        let (iterable_expr, _) = iterable;
        // `0..n` and `0..=n` parse as Expr::Binary with BinaryOp::Range / RangeInclusive.
        // The standalone Expr::Range variant covers open-ended forms like `..n` or `n..`.
        let (start_opt, end_opt, inclusive): (
            Option<&Spanned<Expr>>,
            Option<&Spanned<Expr>>,
            bool,
        ) = match iterable_expr {
            Expr::Binary {
                op: BinaryOp::Range,
                left,
                right,
            } => (Some(left.as_ref()), Some(right.as_ref()), false),
            Expr::Binary {
                op: BinaryOp::RangeInclusive,
                left,
                right,
            } => (Some(left.as_ref()), Some(right.as_ref()), true),
            Expr::Range {
                start,
                end,
                inclusive,
            } => (start.as_deref(), end.as_deref(), *inclusive),
            _ => {
                self.emit_unsupported(Some(span));
                return Ok(());
            }
        };
        let Pattern::Identifier(var_name) = &pattern.0 else {
            self.emit_unsupported(Some(span));
            return Ok(());
        };
        let var_name = var_name.clone();
        let end_val = if let Some(end_expr) = end_opt {
            self.lower_expr(end_expr)?
        } else {
            self.emit_unsupported(Some(span));
            return Ok(());
        };
        let (start_val, elem_ty) = if let Some(start_expr) = start_opt {
            let ty = self.ty_for_expr(start_expr);
            (self.lower_expr(start_expr)?, ty)
        } else {
            let zero = self.lower_literal(
                &Literal::Integer {
                    value: 0,
                    radix: hew_parser::ast::IntRadix::Decimal,
                },
                span.clone(),
            );
            (zero, Ty::I64)
        };

        let saved_bindings = self.bindings.clone();
        let loop_var = self.declare_local(
            Some(var_name.clone()),
            &elem_ty,
            true,
            Some(pattern.1.clone()),
        );
        self.bindings.insert(var_name, loop_var.clone());
        self.emit_instruction(
            "local.set",
            None,
            vec![Operand::local(loop_var.clone()), Operand::local(start_val)],
            Some(span.clone()),
            None,
        );

        let span_ref = self.package.spans.span_ref(&span);
        let (header_idx, header_id) = self.new_block("for_header", span_ref.clone());
        let (body_idx, body_id) = self.new_block("for_body", span_ref.clone());
        let (continue_idx, continue_id) = self.new_block("for_continue", span_ref.clone());
        let (exit_idx, exit_id) = self.new_block("for_exit", span_ref.clone());

        self.terminate(Terminator::br(
            header_id.clone(),
            Vec::new(),
            span_ref.clone(),
        ));
        self.switch_to(header_idx);

        let cmp_op = if inclusive { "cmp.le" } else { "cmp.lt" };
        let cond = self.temp_local(&Ty::Bool, Some(span.clone()));
        self.emit_instruction(
            cmp_op,
            Some(cond.clone()),
            vec![Operand::local(loop_var.clone()), Operand::local(end_val)],
            Some(span.clone()),
            None,
        );
        self.terminate(Terminator::br_if(
            Operand::local(cond),
            body_id,
            exit_id.clone(),
            Vec::new(),
            span_ref,
        ));

        self.switch_to(body_idx);
        self.loop_targets.push(LoopTargets {
            continue_id: continue_id.clone(),
            exit_id: exit_id.clone(),
            label,
        });
        self.lower_block(body)?;
        if !self.current_is_terminated() {
            self.terminate(Terminator::br(continue_id.clone(), Vec::new(), None));
        }
        self.loop_targets.pop();

        self.switch_to(continue_idx);
        let one = self.lower_literal(
            &Literal::Integer {
                value: 1,
                radix: hew_parser::ast::IntRadix::Decimal,
            },
            span.clone(),
        );
        let next_val = self.temp_local(&elem_ty, Some(span.clone()));
        self.emit_instruction(
            "i64.checked_add",
            Some(next_val.clone()),
            vec![Operand::local(loop_var.clone()), Operand::local(one)],
            Some(span.clone()),
            None,
        );
        self.emit_instruction(
            "local.set",
            None,
            vec![Operand::local(loop_var), Operand::local(next_val)],
            Some(span.clone()),
            None,
        );
        self.terminate(Terminator::br(header_id, Vec::new(), None));

        self.bindings = saved_bindings;
        self.switch_to(exit_idx);
        Ok(())
    }

    /// Lower a statement-position `if` to the same `block`/`br_if` sequences used
    /// by expression-position `if`, discarding the branch results (side effects
    /// only; result is always unit).
    fn lower_stmt_if(
        &mut self,
        condition: &Spanned<Expr>,
        then_block: &AstBlock,
        else_block: Option<&ElseBlock>,
        span: std::ops::Range<usize>,
    ) -> Result<(), CompileError> {
        let cond_local = self.lower_expr(condition)?;
        let span_ref = self.package.spans.span_ref(&span);
        let (then_idx, then_id) = self.new_block("if_then", span_ref.clone());
        let (exit_idx, exit_id) = self.new_block("if_exit", span_ref.clone());

        if let Some(else_block) = else_block {
            let (else_idx, else_id) = self.new_block("if_else", span_ref.clone());
            self.terminate(Terminator::br_if(
                Operand::local(cond_local),
                then_id,
                else_id,
                Vec::new(),
                span_ref,
            ));

            self.switch_to(then_idx);
            self.lower_block(then_block)?;
            if !self.current_is_terminated() {
                self.terminate(Terminator::br(exit_id.clone(), Vec::new(), None));
            }

            self.switch_to(else_idx);
            if let Some(else_body_block) = &else_block.block {
                self.lower_block(else_body_block)?;
            }
            if let Some(chained_if) = &else_block.if_stmt {
                // `else if` chains lower recursively as statement-position if.
                self.lower_stmt(&chained_if.0, chained_if.1.clone())?;
            }
            if !self.current_is_terminated() {
                self.terminate(Terminator::br(exit_id, Vec::new(), None));
            }
        } else {
            self.terminate(Terminator::br_if(
                Operand::local(cond_local),
                then_id,
                exit_id.clone(),
                Vec::new(),
                span_ref,
            ));
            self.switch_to(then_idx);
            self.lower_block(then_block)?;
            if !self.current_is_terminated() {
                self.terminate(Terminator::br(exit_id, Vec::new(), None));
            }
        }

        self.switch_to(exit_idx);
        Ok(())
    }

    /// Lower a statement-position `match` to the same tag-dispatch sequences
    /// used by expression-position `match`, discarding each arm's result
    /// (side effects only; result is always unit).
    fn lower_stmt_match(
        &mut self,
        scrutinee: &Spanned<Expr>,
        arms: &[MatchArm],
        span: std::ops::Range<usize>,
    ) -> Result<(), CompileError> {
        let scrutinee_ty = self.ty_for_expr(scrutinee);
        self.package.ensure_option_result_layout(&scrutinee_ty);
        let scrutinee_local = self.lower_expr(scrutinee)?;
        let tag_local = if self.match_uses_enum_tag(&scrutinee_ty) {
            let local = self.temp_local(&Ty::I64, Some(scrutinee.1.clone()));
            self.emit_instruction(
                "enum.tag",
                Some(local.clone()),
                vec![Operand::local(scrutinee_local.clone())],
                Some(scrutinee.1.clone()),
                None,
            );
            Some(local)
        } else {
            None
        };

        let exit_span = self.package.spans.span_ref(&span);
        let (exit_idx, exit_id) = self.new_block("match_exit", exit_span);
        let (check_blocks, guard_blocks, arm_blocks) = self.new_match_blocks(arms);

        if let Some((first_idx, first_id)) = check_blocks.first() {
            let span_ref = self.package.spans.span_ref(&span);
            self.terminate(Terminator::br(first_id.clone(), Vec::new(), span_ref));
            self.switch_to(*first_idx);
        }

        for (index, arm) in arms.iter().enumerate() {
            let (check_idx, _) = check_blocks[index].clone();
            self.switch_to(check_idx);
            let (_, arm_id) = &arm_blocks[index];
            let matched_id = guard_blocks[index]
                .as_ref()
                .map_or_else(|| arm_id.clone(), |(_, id)| id.clone());
            let pattern_is_catch_all = self.pattern_is_catch_all(&arm.pattern, &scrutinee_ty);
            let else_id = Self::next_match_arm_id(
                index,
                arm.guard.is_some(),
                pattern_is_catch_all,
                &check_blocks,
                &matched_id,
                arm_id,
                &exit_id,
            );
            let pattern_span = self.package.spans.span_ref(&arm.pattern.1);
            if self.pattern_is_unconditional(&arm.pattern, &scrutinee_ty) {
                self.terminate(Terminator::br(matched_id, Vec::new(), pattern_span));
            } else if let Some(cond) = self.lower_match_condition(
                &arm.pattern,
                &scrutinee_local,
                &scrutinee_ty,
                tag_local.as_deref(),
            ) {
                self.terminate(Terminator::br_if(
                    Operand::local(cond),
                    matched_id,
                    else_id.clone(),
                    Vec::new(),
                    pattern_span,
                ));
            }

            if let (Some(guard), Some((guard_idx, _))) = (&arm.guard, guard_blocks[index].clone()) {
                let guard_else_id = Self::match_guard_failure_id(index, &check_blocks, &exit_id);
                self.lower_match_guard_block(
                    guard,
                    guard_idx,
                    &arm.pattern,
                    &scrutinee_local,
                    &scrutinee_ty,
                    arm_id.clone(),
                    guard_else_id,
                )?;
            }

            let (arm_idx, _) = arm_blocks[index].clone();
            self.switch_to(arm_idx);
            let saved_bindings = self.bindings.clone();
            self.bind_pattern_payloads(&arm.pattern, &scrutinee_local, &scrutinee_ty);
            // Discard the arm result (statement-position: side effects only).
            self.lower_expr(&arm.body)?;
            self.bindings = saved_bindings;
            let body_span = self.package.spans.span_ref(&arm.body.1);
            self.terminate(Terminator::br(exit_id.clone(), Vec::new(), body_span));
        }

        self.switch_to(exit_idx);
        Ok(())
    }

    /// Lower a statement-position `if let` to enum-tag-check + conditional body
    /// execution, discarding the body result (side effects only). This is the
    /// non-looping analogue of `emit_while_let`.
    fn lower_stmt_if_let(
        &mut self,
        pattern: &Spanned<Pattern>,
        expr: &Spanned<Expr>,
        body: &AstBlock,
        else_body: Option<&AstBlock>,
        span: std::ops::Range<usize>,
    ) -> Result<(), CompileError> {
        let scrutinee_ty = self.ty_for_expr(expr);
        let scrutinee = self.lower_expr(expr)?;

        let Pattern::Constructor { name, .. } = &pattern.0 else {
            self.emit_unsupported(Some(span));
            return Ok(());
        };
        let constructor_name = name.clone();

        let span_ref = self.package.spans.span_ref(&span);
        let (then_idx, then_id) = self.new_block("ifl_then", span_ref.clone());
        let (exit_idx, exit_id) = self.new_block("ifl_exit", span_ref.clone());

        self.package.ensure_option_result_layout(&scrutinee_ty);
        let tag_local = self.temp_local(&Ty::I64, Some(expr.1.clone()));
        self.emit_instruction(
            "enum.tag",
            Some(tag_local.clone()),
            vec![Operand::local(scrutinee.clone())],
            Some(expr.1.clone()),
            None,
        );
        let expected_tag = self
            .variant_info_for_name(&constructor_name, &scrutinee_ty)
            .map_or(0, |(tag, _)| tag);
        let expected_local = self.lower_literal(
            &Literal::Integer {
                value: i64::try_from(expected_tag).unwrap_or(0),
                radix: hew_parser::ast::IntRadix::Decimal,
            },
            span.clone(),
        );
        let cond = self.temp_local(&Ty::Bool, Some(span.clone()));
        self.emit_instruction(
            "cmp.eq",
            Some(cond.clone()),
            vec![Operand::local(tag_local), Operand::local(expected_local)],
            Some(span.clone()),
            None,
        );

        if let Some(else_body) = else_body {
            let (else_idx, else_id) = self.new_block("ifl_else", span_ref.clone());
            self.terminate(Terminator::br_if(
                Operand::local(cond),
                then_id,
                else_id,
                Vec::new(),
                span_ref,
            ));

            self.switch_to(then_idx);
            let saved_bindings = self.bindings.clone();
            self.bind_pattern_payloads(pattern, &scrutinee, &scrutinee_ty);
            self.lower_block(body)?;
            self.bindings = saved_bindings;
            if !self.current_is_terminated() {
                self.terminate(Terminator::br(exit_id.clone(), Vec::new(), None));
            }

            self.switch_to(else_idx);
            let saved_else = self.bindings.clone();
            self.lower_block(else_body)?;
            self.bindings = saved_else;
            if !self.current_is_terminated() {
                self.terminate(Terminator::br(exit_id, Vec::new(), None));
            }
        } else {
            self.terminate(Terminator::br_if(
                Operand::local(cond),
                then_id,
                exit_id.clone(),
                Vec::new(),
                span_ref,
            ));

            self.switch_to(then_idx);
            let saved_bindings = self.bindings.clone();
            self.bind_pattern_payloads(pattern, &scrutinee, &scrutinee_ty);
            self.lower_block(body)?;
            self.bindings = saved_bindings;
            if !self.current_is_terminated() {
                self.terminate(Terminator::br(exit_id, Vec::new(), None));
            }
        }

        self.switch_to(exit_idx);
        Ok(())
    }

    /// Lower a value-position `if let` (`let v = if let Some(x) = opt { x }
    /// else { d }`). Joins the then/else arm values on a result local so the
    /// expression yields the matched arm value, mirroring the value-position
    /// `Expr::If` lowering. When the pattern is not a constructor (the only
    /// shape the statement form supports) or there is no else arm to join,
    /// it falls back to statement-form lowering and yields unit.
    fn lower_expr_if_let(
        &mut self,
        whole_expr: &Spanned<Expr>,
        pattern: &Spanned<Pattern>,
        scrutinee_expr: &Spanned<Expr>,
        body: &AstBlock,
        else_body: Option<&AstBlock>,
        span: std::ops::Range<usize>,
    ) -> Result<String, CompileError> {
        let scrutinee_ty = self.ty_for_expr(scrutinee_expr);

        // Value position requires a join: without an else arm there is no
        // second value to join, so fall back to the statement form (the result
        // is unit anyway). Non-constructor patterns are unsupported by the
        // statement form too; route through it so they trap consistently.
        let Pattern::Constructor { name, .. } = &pattern.0 else {
            self.lower_stmt_if_let(pattern, scrutinee_expr, body, else_body, span.clone())?;
            return Ok(self.emit_const_unit(Some(span)));
        };
        let constructor_name = name.clone();
        let Some(else_body) = else_body else {
            self.lower_stmt_if_let(pattern, scrutinee_expr, body, None, span.clone())?;
            return Ok(self.emit_const_unit(Some(span)));
        };

        let span_ref = self.package.spans.span_ref(&span);
        let (then_idx, then_id) = self.new_block("ifl_then", span_ref.clone());
        let (else_idx, else_id) = self.new_block("ifl_else", span_ref.clone());
        let (exit_idx, exit_id) = self.new_block("ifl_exit", span_ref.clone());

        let result_ty = self.ty_for_expr(whole_expr);
        let result_local = self.declare_local(None, &result_ty, true, Some(span.clone()));

        self.package.ensure_option_result_layout(&scrutinee_ty);
        let scrutinee = self.lower_expr(scrutinee_expr)?;
        let tag_local = self.temp_local(&Ty::I64, Some(scrutinee_expr.1.clone()));
        self.emit_instruction(
            "enum.tag",
            Some(tag_local.clone()),
            vec![Operand::local(scrutinee.clone())],
            Some(scrutinee_expr.1.clone()),
            None,
        );
        let expected_tag = self
            .variant_info_for_name(&constructor_name, &scrutinee_ty)
            .map_or(0, |(tag, _)| tag);
        let expected_local = self.lower_literal(
            &Literal::Integer {
                value: i64::try_from(expected_tag).unwrap_or(0),
                radix: hew_parser::ast::IntRadix::Decimal,
            },
            span.clone(),
        );
        let cond = self.temp_local(&Ty::Bool, Some(span.clone()));
        self.emit_instruction(
            "cmp.eq",
            Some(cond.clone()),
            vec![Operand::local(tag_local), Operand::local(expected_local)],
            Some(span.clone()),
            None,
        );
        self.terminate(Terminator::br_if(
            Operand::local(cond),
            then_id,
            else_id,
            Vec::new(),
            span_ref,
        ));

        // Then arm: bind the matched payload, lower the body, join its value.
        self.switch_to(then_idx);
        let saved_bindings = self.bindings.clone();
        self.bind_pattern_payloads(pattern, &scrutinee, &scrutinee_ty);
        let then_val = self
            .lower_block(body)?
            .unwrap_or_else(|| self.emit_const_unit(Some(span.clone())));
        if !self.current_is_terminated() {
            self.emit_instruction(
                "local.set",
                None,
                vec![
                    Operand::local(result_local.clone()),
                    Operand::local(then_val),
                ],
                Some(span.clone()),
                None,
            );
            let s = self.package.spans.span_ref(&span);
            self.terminate(Terminator::br(exit_id.clone(), Vec::new(), s));
        }
        self.bindings = saved_bindings;

        // Else arm: lower the else body, join its value.
        self.switch_to(else_idx);
        let saved_else = self.bindings.clone();
        let else_val = self
            .lower_block(else_body)?
            .unwrap_or_else(|| self.emit_const_unit(Some(span.clone())));
        if !self.current_is_terminated() {
            self.emit_instruction(
                "local.set",
                None,
                vec![
                    Operand::local(result_local.clone()),
                    Operand::local(else_val),
                ],
                Some(span.clone()),
                None,
            );
            let s = self.package.spans.span_ref(&span);
            self.terminate(Terminator::br(exit_id, Vec::new(), s));
        }
        self.bindings = saved_else;

        self.switch_to(exit_idx);
        Ok(result_local)
    }

    fn emit_while_let(
        &mut self,
        label: Option<String>,
        pattern: &Spanned<Pattern>,
        expr: &Spanned<Expr>,
        body: &AstBlock,
        span: std::ops::Range<usize>,
    ) -> Result<(), CompileError> {
        let span_ref = self.package.spans.span_ref(&span);
        let (header_idx, header_id) = self.new_block("wl_header", span_ref.clone());
        let (body_idx, body_id) = self.new_block("wl_body", span_ref.clone());
        let (exit_idx, exit_id) = self.new_block("wl_exit", span_ref.clone());

        self.terminate(Terminator::br(
            header_id.clone(),
            Vec::new(),
            span_ref.clone(),
        ));
        self.switch_to(header_idx);

        let scrutinee_ty = self.ty_for_expr(expr);
        let scrutinee = self.lower_expr(expr)?;

        let Pattern::Constructor { name, .. } = &pattern.0 else {
            self.emit_unsupported(Some(span));
            return Ok(());
        };
        let constructor_name = name.clone();

        let tag_local = self.temp_local(&Ty::I64, Some(expr.1.clone()));
        self.emit_instruction(
            "enum.tag",
            Some(tag_local.clone()),
            vec![Operand::local(scrutinee.clone())],
            Some(expr.1.clone()),
            None,
        );
        let expected_tag = self
            .package
            .enum_variant_tags
            .get(&constructor_name)
            .map_or(0, |(_, tag, _)| *tag);
        let expected_local = self.lower_literal(
            &Literal::Integer {
                value: i64::try_from(expected_tag).unwrap_or(0),
                radix: hew_parser::ast::IntRadix::Decimal,
            },
            span.clone(),
        );
        let cond = self.temp_local(&Ty::Bool, Some(span.clone()));
        self.emit_instruction(
            "cmp.eq",
            Some(cond.clone()),
            vec![Operand::local(tag_local), Operand::local(expected_local)],
            Some(span.clone()),
            None,
        );
        self.terminate(Terminator::br_if(
            Operand::local(cond),
            body_id,
            exit_id.clone(),
            Vec::new(),
            span_ref,
        ));

        self.switch_to(body_idx);
        let saved_bindings = self.bindings.clone();
        self.bind_pattern_payloads(pattern, &scrutinee, &scrutinee_ty);
        self.loop_targets.push(LoopTargets {
            continue_id: header_id.clone(),
            exit_id: exit_id.clone(),
            label,
        });
        self.lower_block(body)?;
        if !self.current_is_terminated() {
            self.terminate(Terminator::br(header_id, Vec::new(), None));
        }
        self.loop_targets.pop();
        self.bindings = saved_bindings;
        self.switch_to(exit_idx);
        Ok(())
    }

    #[expect(
        clippy::needless_pass_by_value,
        reason = "unsupported lowering consumes the optional source span at the trap boundary"
    )]
    fn emit_unsupported(&mut self, span: Option<std::ops::Range<usize>>) {
        let span_ref = span
            .as_ref()
            .and_then(|span| self.package.spans.span_ref(span));
        if !self.current_is_terminated() {
            self.terminate(Terminator::trap("unsupported_instruction", span_ref));
        }
    }
}

#[derive(Debug)]
struct BlockBuilder {
    id: String,
    instructions: Vec<Instruction>,
    terminator: Option<Terminator>,
    span: Option<String>,
}

impl BlockBuilder {
    fn new(id: String, span: Option<String>) -> Self {
        Self {
            id,
            instructions: Vec::new(),
            terminator: None,
            span,
        }
    }

    fn finish(self) -> Block {
        Block {
            id: self.id,
            params: Vec::new(),
            instructions: self.instructions,
            terminator: self
                .terminator
                .unwrap_or_else(|| Terminator::trap("internal_error", None)),
            span: self.span,
        }
    }
}

#[derive(Debug)]
struct SpanInterner<'a> {
    source: &'a str,
    spans: BTreeMap<(usize, usize), String>,
}

impl<'a> SpanInterner<'a> {
    fn new(source: &'a str) -> Self {
        Self {
            source,
            spans: BTreeMap::new(),
        }
    }

    fn span_ref(&mut self, span: &std::ops::Range<usize>) -> Option<String> {
        if span.start == span.end {
            return None;
        }
        let key = (span.start, span.end);
        let next_id = format!("span:{}-{}", span.start, span.end);
        Some(self.spans.entry(key).or_insert(next_id).clone())
    }

    fn source_map(&self) -> SourceMap {
        let spans = self
            .spans
            .iter()
            .map(|((start, end), id)| SpanEntry {
                id: id.clone(),
                source_id: ROOT_SOURCE_ID.to_string(),
                start: source_position(self.source, *start),
                end: source_position(self.source, *end),
            })
            .collect();
        SourceMap {
            sources: vec![SourceFile {
                id: ROOT_SOURCE_ID.to_string(),
                path: "main.hew".to_string(),
                content_sha256: sha256_hex(self.source.as_bytes()),
            }],
            spans,
        }
    }
}

fn source_position(source: &str, byte_offset: usize) -> SourcePosition {
    let mut line = 1;
    let mut column = 1;
    for (idx, ch) in source.char_indices() {
        if idx >= byte_offset {
            break;
        }
        if ch == '\n' {
            line += 1;
            column = 1;
        } else {
            column += 1;
        }
    }
    SourcePosition {
        line,
        column,
        byte_offset,
    }
}

fn collect_import_edges(program: &Program) -> Vec<ImportEdge> {
    let mut edges = Vec::new();
    for (item, _) in &program.items {
        if let Item::Import(import) = item {
            let path = import.path.join("::");
            edges.push(ImportEdge {
                path: path.clone(),
                resolved_module: format!("mod:{}", sanitize_id(&path.replace("::", "."))),
            });
        }
    }
    edges.sort_by(|left, right| left.path.cmp(&right.path));
    edges
}

fn ty_from_type_expr(ty: &hew_parser::ast::TypeExpr) -> Ty {
    match ty {
        hew_parser::ast::TypeExpr::Named { name, type_args } => {
            let args = type_args
                .as_ref()
                .map(|args| args.iter().map(|(ty, _)| ty_from_type_expr(ty)).collect())
                .unwrap_or_default();
            Ty::normalize_named(name.clone(), args)
        }
        hew_parser::ast::TypeExpr::Option(inner) => Ty::option(ty_from_type_expr(&inner.0)),
        hew_parser::ast::TypeExpr::Result { ok, err } => {
            Ty::result(ty_from_type_expr(&ok.0), ty_from_type_expr(&err.0))
        }
        hew_parser::ast::TypeExpr::Tuple(items) => {
            Ty::Tuple(items.iter().map(|(ty, _)| ty_from_type_expr(ty)).collect())
        }
        hew_parser::ast::TypeExpr::Array { element, size } => {
            Ty::Array(Box::new(ty_from_type_expr(&element.0)), *size)
        }
        hew_parser::ast::TypeExpr::Slice(inner) => Ty::Slice(Box::new(ty_from_type_expr(&inner.0))),
        hew_parser::ast::TypeExpr::Function {
            params,
            return_type,
        } => Ty::Function {
            params: params.iter().map(|(ty, _)| ty_from_type_expr(ty)).collect(),
            ret: Box::new(ty_from_type_expr(&return_type.0)),
        },
        hew_parser::ast::TypeExpr::Pointer {
            is_mutable,
            pointee,
        } => Ty::Pointer {
            is_mutable: *is_mutable,
            pointee: Box::new(ty_from_type_expr(&pointee.0)),
        },
        hew_parser::ast::TypeExpr::TraitObject(_) | hew_parser::ast::TypeExpr::Infer => Ty::Unit,
        // `&T` immutable borrow — first-class no-retain shared reference.
        hew_parser::ast::TypeExpr::Borrow(inner) => Ty::Borrow {
            pointee: Box::new(ty_from_type_expr(&inner.0)),
        },
    }
}

fn numeric_cast_type_name(ty: &Ty) -> Option<&'static str> {
    match ty {
        Ty::I8 => Some("i8"),
        Ty::I16 => Some("i16"),
        Ty::I32 => Some("i32"),
        Ty::I64 | Ty::IntLiteral => Some("i64"),
        Ty::U8 => Some("u8"),
        Ty::U16 => Some("u16"),
        Ty::U32 => Some("u32"),
        Ty::U64 => Some("u64"),
        Ty::Isize => Some("isize"),
        Ty::Usize => Some("usize"),
        Ty::F32 => Some("f32"),
        Ty::F64 | Ty::FloatLiteral => Some("f64"),
        Ty::Bool => Some("bool"),
        Ty::Char => Some("char"),
        _ => None,
    }
}

fn const_literal(expr: &Expr) -> Option<Literal> {
    match expr {
        Expr::Literal(literal) => Some(literal.clone()),
        Expr::Unary {
            op: hew_parser::ast::UnaryOp::Negate,
            operand,
        } => match &operand.0 {
            Expr::Literal(Literal::Integer { value, radix }) => Some(Literal::Integer {
                value: value.checked_neg()?,
                radix: *radix,
            }),
            Expr::Literal(Literal::Float(value)) => Some(Literal::Float(-value)),
            _ => None,
        },
        _ => None,
    }
}

fn package_digest(
    profile: &str,
    source_map: &SourceMap,
    module_graph: &ModuleGraph,
    layouts: &Layouts,
    stdlib_symbols: &[StdlibSymbol],
    capabilities: &[Capability],
    functions: &[Function],
) -> Result<String, CompileError> {
    let value = json!({
        "profile": profile,
        "source_map": source_map,
        "module_graph": module_graph,
        "layouts": layouts,
        "stdlib_symbols": stdlib_symbols,
        "capabilities": capabilities,
        "functions": functions,
    });
    let bytes = serde_json::to_vec(&value).map_err(|err| CompileError {
        message: format!("canonicalize package digest input: {err}"),
    })?;
    Ok(sha256_hex(&bytes)[0..16].to_string())
}

fn sha256_hex(bytes: &[u8]) -> String {
    let digest = Sha256::digest(bytes);
    let mut out = String::with_capacity(digest.len() * 2);
    for byte in digest {
        use std::fmt::Write as _;
        let _ = write!(&mut out, "{byte:02x}");
    }
    out
}

fn function_id(name: &str) -> String {
    format!("fn:{}", sanitize_id(name))
}

/// Bytecode function id for an actor receive handler. Kept distinct from the
/// user-function namespace so an actor method never collides with a free
/// function of the same name.
fn handler_function_id(actor: &str, handler: &str) -> String {
    format!("fn:actor.{}.{}", sanitize_id(actor), sanitize_id(handler))
}

/// Parse a supervisor intensity window (a raw `Token::Duration` source string
/// such as `"60s"` or `"5m"`) into milliseconds. Unrecognised forms fall back
/// to the runtime default of 60s.
fn parse_duration_ms(raw: &str) -> u64 {
    let raw = raw.trim();
    let split = raw.find(|c: char| !c.is_ascii_digit()).unwrap_or(raw.len());
    let (digits, unit) = raw.split_at(split);
    let Ok(value) = digits.parse::<u64>() else {
        return 60_000;
    };
    match unit.trim() {
        "ms" => value,
        "m" => value.saturating_mul(60_000),
        "h" => value.saturating_mul(3_600_000),
        // Seconds is both the explicit `"s"` unit and the fallback default.
        _ => value.saturating_mul(1_000),
    }
}

/// Build a `const.i64` literal operand that survives the VM's JSON transport.
///
/// The bytecode is serialized to JSON and re-parsed by the JS VM via
/// `JSON.parse`, whose numbers are IEEE-754 doubles. Any i64 outside the
/// safe-integer range (`|n| > 2^53 - 1`) would silently lose precision as a
/// JSON number (e.g. `i64::MAX` → `9223372036854775808`). The VM's `bigintArg`
/// reader already accepts a decimal *string* operand and parses it with
/// `BigInt(..)`, so we string-encode out-of-safe-range values to preserve the
/// exact 64-bit magnitude. In-range values stay numeric to keep the common-case
/// bytecode compact and diff-friendly.
fn i64_literal_operand(value: i64) -> Operand {
    if i64_requires_string_encoding(value) {
        Operand::literal(value.to_string())
    } else {
        Operand::literal(value)
    }
}

/// Build a supervisor child initial-state literal that preserves its i64 type.
///
/// Unlike a `const.i64` operand, supervisor initial-state values have no opcode
/// to provide their type. A bare string therefore denotes a Hew `String`, so an
/// out-of-safe-range i64 is encoded as the VM's tagged i64 JSON form.
fn supervisor_i64_literal(value: i64) -> Value {
    if i64_requires_string_encoding(value) {
        json!({ "kind": "i64", "value": value.to_string() })
    } else {
        Value::from(value)
    }
}

fn i64_requires_string_encoding(value: i64) -> bool {
    const SAFE_INT_MAX: i64 = 9_007_199_254_740_991; // 2^53 - 1
    value.unsigned_abs() > SAFE_INT_MAX as u64
}

/// Evaluate a constant literal expression into a JSON value for baking a
/// supervisor child's initial state. Returns `None` for non-literal initializers
/// (which the educational profile does not yet admit in child specs).
fn literal_json(expr: &Expr) -> Option<Value> {
    match expr {
        Expr::Literal(Literal::Integer { value, .. }) => Some(supervisor_i64_literal(*value)),
        Expr::Literal(Literal::Float(value)) => Some(Value::from(*value)),
        Expr::Literal(Literal::String(value)) => Some(Value::from(value.clone())),
        Expr::Literal(Literal::Bool(value)) => Some(Value::from(*value)),
        Expr::Literal(Literal::Char(value)) => Some(Value::from(value.to_string())),
        _ => None,
    }
}

fn sanitize_id(raw: &str) -> String {
    let mut out = String::new();
    for ch in raw.chars() {
        if ch.is_ascii_alphanumeric() || matches!(ch, '_' | '.' | '-') {
            out.push(ch);
        } else {
            out.push('.');
        }
    }
    while out.contains("..") {
        out = out.replace("..", ".");
    }
    out.trim_matches('.').to_string()
}
