use std::collections::{BTreeMap, BTreeSet, HashMap};

use hew_parser::ast::{
    BinaryOp, Block as AstBlock, CallArg, Expr, FnDecl, Item, Literal, MatchArm, Pattern, Program,
    Spanned, Stmt, TypeBodyItem, TypeDeclKind, VariantKind,
};
use hew_types::check::{SpanKey, TypeDefKind, VariantDef};
use hew_types::Ty;
use serde_json::{json, Value};
use sha2::{Digest, Sha256};

use crate::bytecode::{
    Block, Capability, EnumLayout, FieldLayout, Function, ImportEdge, Instruction, Layouts, Local,
    Module, ModuleGraph, Operand, RecordLayout, SandboxBytecodePackage, SourceFile, SourceMap,
    SourcePosition, SpanEntry, StdlibSymbol, Terminator, TypeLayout, VariantLayout,
};
use crate::CompileError;

const SCHEMA_VERSION: &str = "hew.sandbox.bytecode.v0";
const ROOT_SOURCE_ID: &str = "src:main";
const ROOT_MODULE_ID: &str = "mod:main";

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
    user_functions: BTreeSet<String>,
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
            user_functions: BTreeSet::new(),
        }
    }

    fn emit(&mut self, program: &Program) -> Result<SandboxBytecodePackage, CompileError> {
        self.collect_user_functions(program);
        self.collect_layouts(program);
        self.ensure_core_types();

        let mut functions = Vec::new();
        for (item, span) in &program.items {
            if let Item::Function(function) = item {
                functions.push(self.emit_function(function, span.clone())?);
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
            actors: Vec::new(),
            supervisors: Vec::new(),
            machines: Vec::new(),
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
    }

    fn ensure_core_types(&mut self) {
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

        let mut ctx = FunctionEmitter::new(self, &function.name);
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
            Ty::F32 | Ty::F64 | Ty::FloatLiteral => (
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
                (
                    id,
                    "opaque".to_string(),
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
}

#[derive(Debug, Clone)]
struct LoopTargets {
    continue_id: String,
    exit_id: String,
    label: Option<String>,
}

#[derive(Debug)]
struct FunctionEmitter<'pkg, 'src> {
    package: &'pkg mut PackageEmitter<'src>,
    function_name: String,
    locals: Vec<Local>,
    blocks: Vec<BlockBuilder>,
    current: usize,
    next_local: usize,
    next_block: usize,
    bindings: HashMap<String, String>,
    loop_targets: Vec<LoopTargets>,
}

impl<'pkg, 'src> FunctionEmitter<'pkg, 'src> {
    fn new(package: &'pkg mut PackageEmitter<'src>, function_name: &str) -> Self {
        Self {
            package,
            function_name: function_name.to_string(),
            locals: Vec::new(),
            blocks: vec![BlockBuilder::new("block:entry".to_string(), None)],
            current: 0,
            next_local: 0,
            next_block: 0,
            bindings: HashMap::new(),
            loop_targets: Vec::new(),
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
            Stmt::Let { pattern, value, .. } => {
                if let Pattern::Identifier(name) = &pattern.0 {
                    if let Some(value) = value {
                        let value_local = self.lower_expr(value)?;
                        let ty = self.ty_for_expr(value);
                        let local = self.declare_local(
                            Some(name.clone()),
                            &ty,
                            false,
                            Some(pattern.1.clone()),
                        );
                        self.emit_instruction(
                            "local.set",
                            None,
                            vec![Operand::local(local.clone()), Operand::local(value_local)],
                            Some(span),
                            None,
                        );
                        self.bindings.insert(name.clone(), local);
                    }
                } else {
                    self.emit_unsupported(Some(span));
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
                let span_ref = self.package.spans.span_ref(&span);
                let values = if let Some(value) = value {
                    vec![Operand::local(self.lower_expr(value)?)]
                } else {
                    vec![Operand::local(self.emit_const_unit(Some(span.clone())))]
                };
                self.terminate(Terminator::ret(values, span_ref));
            }
            Stmt::Expression(expr) => {
                self.lower_expr(expr)?;
            }
            Stmt::Assign { target, value, .. } => {
                let value_local = self.lower_expr(value)?;
                if let Expr::Identifier(name) = &target.0 {
                    if let Some(local) = self.bindings.get(name).cloned() {
                        self.emit_instruction(
                            "local.set",
                            None,
                            vec![Operand::local(local), Operand::local(value_local)],
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
            Stmt::If { .. } | Stmt::IfLet { .. } | Stmt::Match { .. } | Stmt::Defer(_) => {
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
                    let dst = self.temp_local(&ty, Some(span.clone()));
                    self.emit_instruction(
                        "i64.neg",
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
            Expr::StructInit { name, fields, .. } => {
                let mut field_values = BTreeMap::new();
                for (field, value) in fields {
                    field_values.insert(field.clone(), self.lower_expr(value)?);
                }
                let type_id = self.package.type_id_for_named(name, &[]);
                let mut args = vec![Operand::ty(type_id)];
                if let Some(indexes) = self.package.record_field_indexes.get(name) {
                    for field in indexes.keys() {
                        if let Some(local) = field_values.get(field) {
                            args.push(Operand::local(local.clone()));
                        }
                    }
                } else {
                    for (_, local) in field_values {
                        args.push(Operand::local(local));
                    }
                }
                let ty = self.ty_for_expr(expr);
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
            Expr::FieldAccess { object, field } => {
                let object_local = self.lower_expr(object)?;
                let object_ty = self.ty_for_expr(object);
                let field_index = match object_ty {
                    Ty::Named { name, .. } => self
                        .package
                        .record_field_indexes
                        .get(&name)
                        .and_then(|indexes| indexes.get(field))
                        .copied()
                        .unwrap_or(0),
                    _ => 0,
                };
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
            Expr::IfLet { .. }
            | Expr::Tuple(_)
            | Expr::ArrayRepeat { .. }
            | Expr::MapLiteral { .. }
            | Expr::Lambda { .. }
            | Expr::Spawn { .. }
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
            | Expr::Cooperate
            | Expr::This
            | Expr::Cast { .. }
            | Expr::PostfixTry(_)
            | Expr::Range { .. }
            | Expr::Await(_)
            | Expr::ByteStringLiteral(_)
            | Expr::ByteArrayLiteral(_)
            | Expr::Is { .. }
            | Expr::MachineEmit { .. }
            | Expr::GenBlock { .. } => {
                self.emit_unsupported(Some(span.clone()));
                Ok(self.emit_const_unit(Some(span.clone())))
            }
        }
    }

    fn lower_literal(&mut self, literal: &Literal, span: std::ops::Range<usize>) -> String {
        match literal {
            Literal::Integer { value, .. } => {
                let dst = self.temp_local(&Ty::I64, Some(span.clone()));
                self.emit_instruction(
                    "const.i64",
                    Some(dst.clone()),
                    vec![Operand::literal(*value)],
                    Some(span),
                    None,
                );
                dst
            }
            Literal::Float(value) => {
                let dst = self.temp_local(&Ty::F64, Some(span.clone()));
                self.emit_instruction(
                    "const.f64",
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
                    vec![Operand::literal(*value)],
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
        let left_local = self.lower_expr(left)?;
        let right_local = self.lower_expr(right)?;
        let opcode = match op {
            BinaryOp::Add => "i64.checked_add",
            BinaryOp::Subtract => "i64.checked_sub",
            BinaryOp::Multiply => "i64.checked_mul",
            BinaryOp::Divide => "i64.checked_div",
            BinaryOp::Modulo => "i64.checked_rem",
            BinaryOp::WrappingAdd => "i64.add",
            BinaryOp::WrappingSub => "i64.sub",
            BinaryOp::WrappingMul => "i64.mul",
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
                self.emit_unsupported(Some(span.clone()));
                Ok(self.emit_const_unit(Some(span)))
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
            "free" => {
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
            _ => {
                self.emit_unsupported(Some(span.clone()));
                Ok(self.emit_const_unit(Some(span)))
            }
        }
    }

    fn lower_match(
        &mut self,
        scrutinee: &Spanned<Expr>,
        arms: &[MatchArm],
        span: std::ops::Range<usize>,
    ) -> Result<String, CompileError> {
        if arms
            .iter()
            .any(|arm| matches!(arm.pattern.0, Pattern::Struct { .. } | Pattern::Tuple(_)))
        {
            self.emit_unsupported(Some(span.clone()));
            return Ok(self.emit_const_unit(Some(span)));
        }
        let scrutinee_local = self.lower_expr(scrutinee)?;
        let result_ty = self.ty_for_span(&span);
        let result_local = self.declare_local(None, &result_ty, true, Some(span.clone()));
        let tag_local = self.temp_local(&Ty::I64, Some(scrutinee.1.clone()));
        self.emit_instruction(
            "enum.tag",
            Some(tag_local.clone()),
            vec![Operand::local(scrutinee_local.clone())],
            Some(scrutinee.1.clone()),
            None,
        );

        let exit_span = self.package.spans.span_ref(&span);
        let (exit_idx, exit_id) = self.new_block("match_exit", exit_span);
        let mut check_blocks = Vec::new();
        let mut arm_blocks = Vec::new();
        for (index, arm) in arms.iter().enumerate() {
            let check_span = self.package.spans.span_ref(&arm.pattern.1);
            check_blocks.push(self.new_block(&format!("match_check_{index}"), check_span));
            let arm_span = self.package.spans.span_ref(&arm.body.1);
            arm_blocks.push(self.new_block(&format!("match_arm_{index}"), arm_span));
        }

        if let Some((first_idx, first_id)) = check_blocks.first() {
            let span_ref = self.package.spans.span_ref(&span);
            self.terminate(Terminator::br(first_id.clone(), Vec::new(), span_ref));
            self.switch_to(*first_idx);
        }

        for (index, arm) in arms.iter().enumerate() {
            let (check_idx, _) = check_blocks[index].clone();
            self.switch_to(check_idx);
            let (_, arm_id) = &arm_blocks[index];
            let tag = self.pattern_tag(&arm.pattern).unwrap_or(index);
            let tag_const = self.lower_literal(
                &Literal::Integer {
                    value: i64::try_from(tag).unwrap_or(0),
                    radix: hew_parser::ast::IntRadix::Decimal,
                },
                arm.pattern.1.clone(),
            );
            let cond = self.temp_local(&Ty::Bool, Some(arm.pattern.1.clone()));
            self.emit_instruction(
                "cmp.eq",
                Some(cond.clone()),
                vec![Operand::local(tag_local.clone()), Operand::local(tag_const)],
                Some(arm.pattern.1.clone()),
                None,
            );
            let else_id = check_blocks
                .get(index + 1)
                .map_or_else(|| arm_id.clone(), |(_, id)| id.clone());
            let pattern_span = self.package.spans.span_ref(&arm.pattern.1);
            self.terminate(Terminator::br_if(
                Operand::local(cond),
                arm_id.clone(),
                else_id,
                Vec::new(),
                pattern_span,
            ));

            let (arm_idx, _) = arm_blocks[index].clone();
            self.switch_to(arm_idx);
            let saved_bindings = self.bindings.clone();
            self.bind_pattern_payloads(&arm.pattern, &scrutinee_local);
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

    fn bind_pattern_payloads(&mut self, pattern: &Spanned<Pattern>, scrutinee_local: &str) {
        if let Pattern::Constructor { name, patterns } = &pattern.0 {
            let payload_tys = self
                .package
                .enum_variant_tags
                .get(name)
                .map(|(_, _, tys)| tys.clone())
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
    }

    fn pattern_tag(&self, pattern: &Spanned<Pattern>) -> Option<usize> {
        match &pattern.0 {
            Pattern::Constructor { name, .. } => self
                .package
                .enum_variant_tags
                .get(name)
                .map(|(_, tag, _)| *tag),
            Pattern::Identifier(name) => self
                .package
                .enum_variant_tags
                .get(name)
                .map(|(_, tag, _)| *tag),
            _ => None,
        }
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

    fn emit_while_let(
        &mut self,
        label: Option<String>,
        pattern: &Spanned<Pattern>,
        expr: &Spanned<Expr>,
        body: &AstBlock,
        span: std::ops::Range<usize>,
    ) -> Result<(), CompileError> {
        let Pattern::Constructor { name, patterns } = &pattern.0 else {
            self.emit_unsupported(Some(span));
            return Ok(());
        };
        let (constructor_name, payload_patterns) = (name.clone(), patterns.clone());

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

        let scrutinee = self.lower_expr(expr)?;
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
        let payload_tys = self
            .package
            .enum_variant_tags
            .get(&constructor_name)
            .map(|(_, _, tys)| tys.clone())
            .unwrap_or_default();
        for (idx, payload_pattern) in payload_patterns.iter().enumerate() {
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
                        Operand::local(scrutinee.clone()),
                        Operand::literal(idx as u64),
                    ],
                    Some(payload_pattern.1.clone()),
                    None,
                );
                self.bindings.insert(binding.clone(), local);
            }
        }
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
