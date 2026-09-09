//! Normalize machine evaluators into ordinary declarations before checking.
//!
//! Selection, output collection and candidate construction are local value
//! operations. The generated step's provenance requests staged receiver
//! replacement through the ordinary checked method-call contract.

#![expect(
    clippy::result_large_err,
    reason = "normalization uses the shared checker diagnostic value"
)]

use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;

use hew_parser::ast::{
    Block, CallArg, ContextVariantExpr, ContextVariantPattern, ContextVariantRecord,
    DeclarationOrigin, Expr, FnDecl, Item, Literal, MachineDecl, MachineEvent, MachineState,
    MachineTransition, MatchArm, NominalPatternPayload, Pattern, PatternField, Program,
    ResourceMarker, Span, Spanned, Stmt, StringPart, TypeBodyItem, TypeDecl, TypeDeclKind,
    TypeExpr, VariantDecl, VariantKind, Visibility,
};

use crate::error::{TypeError, TypeErrorKind};

/// One checked program authority and diagnostic provenance for its generated
/// nodes. Original top-level items keep their declaration ordinal and span.
#[derive(Debug, Clone)]
pub struct NormalizedMachines {
    pub program: Program,
    pub source_spans: HashMap<Span, Span>,
}

pub(super) fn normalize(
    program: &Program,
) -> Result<Option<Arc<NormalizedMachines>>, Vec<TypeError>> {
    let contains_machine = program
        .items
        .iter()
        .any(|(item, _)| matches!(item, Item::Machine(_)))
        || program.module_graph.as_ref().is_some_and(|graph| {
            graph.modules.values().any(|module| {
                module
                    .items
                    .iter()
                    .any(|(item, _)| matches!(item, Item::Machine(_)))
            })
        });
    if !contains_machine {
        return Ok(None);
    }
    let max_span = program
        .items
        .iter()
        .map(|(_, span)| span.end)
        .chain(
            program
                .module_graph
                .iter()
                .flat_map(|graph| graph.modules.values())
                .flat_map(|module| module.items.iter().map(|(_, span)| span.end)),
        )
        .max()
        .unwrap_or(0);
    let mut builder = Builder {
        next_span: max_span + 1,
        source_spans: HashMap::new(),
        origin: 0..0,
        expansions: HashMap::new(),
    };
    let mut normalized = program.clone();
    let root_sources = program
        .module_graph
        .as_ref()
        .map(|graph| item_sources(graph, &graph.root))
        .unwrap_or_default();
    normalized.items = builder.items(&program.items, &root_sources, "(root)")?;
    if let Some(graph) = &mut normalized.module_graph {
        for (id, module) in &mut graph.modules {
            let old_items = &program
                .module_graph
                .as_ref()
                .expect("graph cloned above")
                .modules[id]
                .items;
            let old_len = old_items.len();
            let sources = graph
                .item_sources
                .get(&id.path.join("."))
                .cloned()
                .unwrap_or_else(|| {
                    module
                        .items
                        .iter()
                        .filter_map(|_| module.source_paths.first().cloned())
                        .collect()
                });
            module.items = if *id == graph.root && *old_items == program.items {
                normalized.items.clone()
            } else {
                builder.items(old_items, &sources, &id.path.join("."))?
            };
            if let Some(sources) = graph.item_sources.get_mut(&id.path.join(".")) {
                // Each expansion appends its companions after all authored
                // items, in authored machine order.
                let old_sources = sources.clone();
                for (ordinal, (item, _)) in old_items.iter().enumerate() {
                    if matches!(item, Item::Machine(_)) {
                        let source = old_sources
                            .get(ordinal)
                            .or_else(|| module.source_paths.first());
                        if let Some(source) = source {
                            sources.extend(std::iter::repeat_n(source.clone(), 5));
                        }
                    }
                }
                debug_assert_eq!(
                    sources.len(),
                    module.items.len(),
                    "machine source provenance must remain parallel to items (original {old_len})"
                );
            }
        }
    }
    project_normalized_imports(&mut normalized);
    Ok(Some(Arc::new(NormalizedMachines {
        program: normalized,
        source_spans: builder.source_spans,
    })))
}

type ExpansionKey = (Option<PathBuf>, String, Span, usize);
type VariantFields = (String, Vec<(String, Spanned<TypeExpr>)>);

struct Builder {
    next_span: usize,
    source_spans: HashMap<Span, Span>,
    origin: Span,
    /// One expansion per physical declaration, shared by import projections.
    expansions: HashMap<ExpansionKey, Vec<Spanned<Item>>>,
}

const OUTPUTS: &str = "_$machine_outputs";
const DISPOSITION: &str = "_$machine_disposition";
const CANDIDATE: &str = "_$machine_candidate";

impl Builder {
    fn span(&mut self) -> Span {
        let span = self.next_span..self.next_span + 1;
        self.next_span += 2;
        self.source_spans.insert(span.clone(), self.origin.clone());
        span
    }

    fn expr(&mut self, expr: Expr) -> Spanned<Expr> {
        (expr, self.span())
    }
    fn stmt(&mut self, stmt: Stmt) -> Spanned<Stmt> {
        (stmt, self.span())
    }
    fn ident(&mut self, name: impl Into<String>) -> Spanned<Expr> {
        self.expr(Expr::Identifier(name.into()))
    }
    fn ty(&mut self, name: impl Into<String>) -> Spanned<TypeExpr> {
        (
            TypeExpr::Named {
                name: name.into(),
                type_args: None,
            },
            self.span(),
        )
    }
    fn block(&mut self, stmts: Vec<Spanned<Stmt>>, trailing: Spanned<Expr>) -> Spanned<Expr> {
        self.expr(Expr::Block(Block {
            stmts,
            trailing_expr: Some(Box::new(trailing)),
        }))
    }
    fn var(
        &mut self,
        name: impl Into<String>,
        ty: Option<Spanned<TypeExpr>>,
        value: Spanned<Expr>,
    ) -> Spanned<Stmt> {
        self.stmt(Stmt::Var {
            name: name.into(),
            ty,
            value: Some(value),
        })
    }
    fn let_value(
        &mut self,
        name: &str,
        ty: Option<Spanned<TypeExpr>>,
        value: Spanned<Expr>,
    ) -> Spanned<Stmt> {
        let pattern = (Pattern::Identifier(name.to_string()), self.span());
        self.stmt(Stmt::Let {
            pattern,
            ty,
            value: Some(value),
            else_block: None,
        })
    }
    fn variant(&mut self, name: &str, fields: Vec<(String, Spanned<Expr>)>) -> Spanned<Expr> {
        self.expr(Expr::ContextVariant(ContextVariantExpr {
            name: name.to_string(),
            record: (!fields.is_empty())
                .then_some(Box::new(ContextVariantRecord { fields, base: None })),
        }))
    }
    fn pattern(
        &mut self,
        name: &str,
        fields: &[(String, Spanned<TypeExpr>)],
        prefix: &str,
    ) -> Spanned<Pattern> {
        let fields: Vec<_> = fields
            .iter()
            .map(|(name, _)| PatternField {
                name: name.clone(),
                pattern: Some((Pattern::Identifier(format!("{prefix}{name}")), self.span())),
            })
            .collect();
        (
            Pattern::ContextVariant(ContextVariantPattern {
                name: name.to_string(),
                payload: (!fields.is_empty())
                    .then_some(NominalPatternPayload::Record { fields, rest: None }),
            }),
            self.span(),
        )
    }
    /// The staged machine value for `state`, spelled through the machine's own
    /// name. A contextual `.Variant` needs an expected type, and reading
    /// `state` or `self` in an unannotated `let` supplies none.
    fn state_value(&mut self, machine: &MachineDecl, state: &MachineState) -> Spanned<Expr> {
        let fields: Vec<_> = state
            .fields
            .iter()
            .map(|(name, _)| (name.clone(), self.ident(format!("_$machine_state_{name}"))))
            .collect();
        self.qualified_variant(machine, &state.name, fields)
    }

    fn qualified_variant(
        &mut self,
        machine: &MachineDecl,
        name: &str,
        fields: Vec<(String, Spanned<Expr>)>,
    ) -> Spanned<Expr> {
        if fields.is_empty() {
            let object = self.ident(&machine.name);
            return self.expr(Expr::FieldAccess {
                object: Box::new(object),
                field: name.to_string(),
            });
        }
        self.expr(Expr::StructInit {
            name: format!("{}.{name}", machine.name),
            fields,
            type_args: None,
            base: None,
        })
    }
    fn state_locals(&mut self, state: &MachineState) -> Vec<Spanned<Stmt>> {
        state
            .fields
            .iter()
            .map(|(name, _)| {
                let value = self.ident(format!("_$machine_in_{name}"));
                self.var(format!("_$machine_state_{name}"), None, value)
            })
            .collect()
    }
    fn error(&self, message: impl Into<String>) -> TypeError {
        TypeError::new(
            TypeErrorKind::MachineExhaustivenessError,
            self.origin.clone(),
            message.into(),
        )
    }

    fn items(
        &mut self,
        items: &[Spanned<Item>],
        sources: &[PathBuf],
        module: &str,
    ) -> Result<Vec<Spanned<Item>>, Vec<TypeError>> {
        let mut result = items.to_vec();
        let mut companions = Vec::new();
        let mut errors = Vec::new();
        for (ordinal, (item, span)) in items.iter().enumerate() {
            let Item::Machine(machine) = item else {
                continue;
            };
            self.origin = span.clone();
            let source = sources.get(ordinal).cloned();
            let context = if source.is_some() {
                String::new()
            } else {
                module.to_string()
            };
            let key = (
                source,
                context,
                span.clone(),
                if span.is_empty() { ordinal } else { 0 },
            );
            let expansion = self
                .expansions
                .get(&key)
                .cloned()
                .map_or_else(|| self.machine(machine), Ok);
            match expansion {
                Ok(mut generated) => {
                    self.expansions.insert(key, generated.clone());
                    // Replacing in place preserves every authored root's
                    // source occurrence, including the machine itself.
                    result[ordinal] = (generated.remove(0).0, span.clone());
                    companions.extend(generated);
                }
                Err(error) => errors.push(error),
            }
        }
        result.extend(companions);
        if errors.is_empty() {
            Ok(result)
        } else {
            Err(errors)
        }
    }

    #[expect(
        clippy::too_many_lines,
        reason = "build the six ordinary declarations from one machine"
    )]
    fn machine(&mut self, machine: &MachineDecl) -> Result<Vec<Spanned<Item>>, TypeError> {
        self.validate_relation(machine)?;
        let params = if machine.type_params.is_empty() {
            None
        } else {
            Some(machine.type_params.clone())
        };
        let mut state_enum = self.enum_decl(
            &machine.name,
            &machine
                .states
                .iter()
                .map(|state| (state.name.clone(), state.fields.clone()))
                .collect::<Vec<_>>(),
            machine.visibility,
        );
        state_enum.origin = DeclarationOrigin::MachineState;
        state_enum.type_params.clone_from(&params);
        state_enum.where_clause.clone_from(&machine.where_clause);
        let mut event_enum = self.enum_decl(
            &format!("{}Event", machine.name),
            &machine
                .events
                .iter()
                .map(|event| (event.name.clone(), event.fields.clone()))
                .collect::<Vec<_>>(),
            machine.visibility,
        );
        event_enum.type_params.clone_from(&params);
        event_enum.where_clause.clone_from(&machine.where_clause);
        let mut output_enum = self.enum_decl(
            &format!("{}Output", machine.name),
            &machine
                .emits
                .iter()
                .map(|event| (event.name.clone(), event.fields.clone()))
                .collect::<Vec<_>>(),
            machine.visibility,
        );
        output_enum.type_params.clone_from(&params);
        output_enum.where_clause.clone_from(&machine.where_clause);
        let disposition_enum = self.enum_decl(
            &format!("{}StepDisposition", machine.name),
            &[
                ("Taken".to_string(), Vec::new()),
                ("Ignored".to_string(), Vec::new()),
            ],
            machine.visibility,
        );
        let generic = if machine.type_params.is_empty() {
            String::new()
        } else {
            format!(
                "<{}>",
                machine
                    .type_params
                    .iter()
                    .map(|param| param.name.as_str())
                    .collect::<Vec<_>>()
                    .join(", ")
            )
        };
        // These shells contain only ordinary source syntax. All copied source
        // expressions are transformed below, with fresh node spans.
        let source = format!("type {name}Step{generic} {{ outputs: Vec<{name}Output{generic}>, disposition: {name}StepDisposition }} impl{generic} {name}{generic} {{ fn step(var self, event: {name}Event{generic}) -> {name}Step{generic} {{}} fn state_name(self) -> string {{}} }}", name = machine.name);
        let parsed = hew_parser::parse(&source);
        if !parsed.errors.is_empty() {
            return Err(self.error(format!(
                "could not normalize machine declarations: {:?}",
                parsed.errors
            )));
        }
        let mut shell = parsed.program.items.into_iter();
        let Item::TypeDecl(mut report) = shell.next().expect("report shell").0 else {
            unreachable!()
        };
        report.origin = DeclarationOrigin::MachineReport;
        report.visibility = machine.visibility;
        // The shell text spells the parameters without their bounds; the
        // declared bounds and where clause are the machine's, so the report
        // and the impl carry them verbatim.
        report.type_params.clone_from(&params);
        report.where_clause.clone_from(&machine.where_clause);
        self.refresh_type_decl(&mut report)?;
        let Item::Impl(mut implementation) = shell.next().expect("impl shell").0 else {
            unreachable!()
        };
        implementation.type_params.clone_from(&params);
        implementation
            .where_clause
            .clone_from(&machine.where_clause);
        self.refresh_type(&mut implementation.target_type);
        let step = &mut implementation.methods[0];
        step.origin = DeclarationOrigin::MachineStep;
        step.visibility = machine.visibility;
        step.body = self.step_body(machine)?;
        self.refresh_signature(step);
        let state_name = &mut implementation.methods[1];
        state_name.origin = DeclarationOrigin::MachineCompanion;
        state_name.visibility = machine.visibility;
        let receiver = self.ident("self");
        let arms = machine
            .states
            .iter()
            .map(|state| {
                let pattern = self.pattern(&state.name, &state.fields, "_$machine_in_");
                let body = self.expr(Expr::Literal(Literal::String(state.name.clone())));
                MatchArm {
                    pattern,
                    guard: None,
                    body,
                }
            })
            .collect();
        let expression = self.expr(Expr::Match {
            scrutinee: Box::new(receiver),
            arms,
        });
        state_name.body = Block {
            stmts: Vec::new(),
            trailing_expr: Some(Box::new(expression)),
        };
        self.refresh_signature(state_name);
        let mut generated = vec![
            Item::TypeDecl(state_enum),
            Item::TypeDecl(event_enum),
            Item::TypeDecl(output_enum),
            Item::TypeDecl(disposition_enum),
            Item::TypeDecl(report),
            Item::Impl(implementation),
        ];
        for item in &mut generated[..4] {
            if let Item::TypeDecl(decl) = item {
                self.refresh_type_decl(decl)?;
            }
        }
        Ok(generated
            .into_iter()
            .map(|item| (item, self.span()))
            .collect())
    }

    fn enum_decl(
        &mut self,
        name: &str,
        variants: &[VariantFields],
        visibility: Visibility,
    ) -> TypeDecl {
        TypeDecl {
            origin: DeclarationOrigin::MachineCompanion,
            visibility,
            kind: TypeDeclKind::Enum,
            name: name.to_string(),
            type_params: None,
            where_clause: None,
            body: variants
                .iter()
                .map(|(name, fields)| {
                    TypeBodyItem::Variant(VariantDecl {
                        name: name.clone(),
                        doc_comment: None,
                        span: self.span(),
                        kind: if fields.is_empty() {
                            VariantKind::Unit
                        } else {
                            VariantKind::Struct(fields.clone())
                        },
                    })
                })
                .collect(),
            doc_comment: None,
            wire: None,
            is_indirect: false,
            resource_marker: ResourceMarker::None,
            is_opaque: false,
            consuming_methods: Vec::new(),
            lang_item: None,
        }
    }

    fn validate_relation(&self, machine: &MachineDecl) -> Result<(), TypeError> {
        if machine.states.is_empty() || machine.events.is_empty() {
            return Err(self.error("a machine must declare a state and an input event"));
        }
        if !machine.const_params.is_empty() || !machine.composite_groups.is_empty() {
            return Err(self.error("ordinary machine evaluation does not yet admit const parameters or composite states"));
        }
        // The generated declarations carry the machine's where clause, so a
        // predicate naming anything but a declared parameter would reach HIR
        // as an unlowerable impl shape instead of a machine diagnostic.
        for predicate in machine
            .where_clause
            .iter()
            .flat_map(|clause| &clause.predicates)
        {
            let declared = matches!(
                &predicate.ty.0,
                TypeExpr::Named { name, type_args: None }
                    if machine.type_params.iter().any(|param| param.name == *name)
            );
            if !declared {
                return Err(self.error(format!(
                    "where-clause predicate on machine `{}` must name a declared type parameter",
                    machine.name
                )));
            }
        }
        for transition in &machine.transitions {
            if !machine
                .events
                .iter()
                .any(|event| event.name == transition.event_name)
                || (transition.source_state != "_"
                    && !machine
                        .states
                        .iter()
                        .any(|state| state.name == transition.source_state))
                || (transition.target_state != "_"
                    && !machine
                        .states
                        .iter()
                        .any(|state| state.name == transition.target_state))
            {
                return Err(
                    self.error("machine transition references an undeclared state or input event")
                );
            }
            if transition.target_state != "_"
                && !returns_variant(
                    &transition.body.0,
                    &transition.target_state,
                    &transition.source_state,
                    &machine.name,
                )
            {
                return Err(self.error(format!(
                    "transition to `{}` must produce that state on every normal path",
                    transition.target_state
                )));
            }
        }
        for state in &machine.states {
            for event in &machine.events {
                let rules = rules_for(machine, state, event);
                let mut covered = false;
                for rule in rules {
                    if covered {
                        return Err(self.error(format!(
                            "unreachable rule after the unconditional fallback for `{}` / `{}`",
                            state.name, event.name
                        )));
                    }
                    covered = rule.guard.is_none();
                }
                if !covered && !machine.has_default {
                    return Err(self.error(format!("machine `{}` needs an unconditional fallback for `{}` / `{}`; guards do not establish coverage", machine.name, state.name, event.name)));
                }
            }
        }
        Ok(())
    }

    fn step_body(&mut self, machine: &MachineDecl) -> Result<Block, TypeError> {
        let element_ty = self.machine_ty(machine, "Output");
        let outputs_ty = (
            TypeExpr::Named {
                name: "Vec".to_string(),
                type_args: Some(vec![element_ty]),
            },
            self.span(),
        );
        let empty = self.expr(Expr::Array(Vec::new()));
        let outputs = self.var(OUTPUTS, Some(outputs_ty), empty);
        let disposition_ty = self.ty(format!("{}StepDisposition", machine.name));
        let taken = self.variant("Taken", Vec::new());
        let disposition = self.var(DISPOSITION, Some(disposition_ty), taken);
        let mut state_arms = Vec::new();
        for state in &machine.states {
            let pattern = self.pattern(&state.name, &state.fields, "_$machine_in_");
            let locals = self.state_locals(state);
            let mut event_arms = Vec::new();
            for event in &machine.events {
                let event_pattern = self.pattern(&event.name, &event.fields, "_$machine_event_");
                let rules = rules_for(machine, state, event);
                let current = self.state_value(machine, state);
                let ignored = self.variant("Ignored", Vec::new());
                let target = self.ident(DISPOSITION);
                let set_ignored = self.stmt(Stmt::Assign {
                    target,
                    op: None,
                    value: ignored,
                });
                let mut selected = self.block(vec![set_ignored], current);
                for rule in rules.into_iter().rev() {
                    let taken = self.transition(machine, state, event, rule)?;
                    selected = if let Some(guard) = &rule.guard {
                        let guard = self.rewrite(guard, machine, state, event)?;
                        self.expr(Expr::If {
                            condition: Box::new(guard),
                            then_block: Box::new(taken),
                            else_block: Some(Box::new(selected)),
                        })
                    } else {
                        taken
                    };
                }
                event_arms.push(MatchArm {
                    pattern: event_pattern,
                    guard: None,
                    body: selected,
                });
            }
            let event = self.ident("event");
            let dispatch = self.expr(Expr::Match {
                scrutinee: Box::new(event),
                arms: event_arms,
            });
            let body = self.block(locals, dispatch);
            state_arms.push(MatchArm {
                pattern,
                guard: None,
                body,
            });
        }
        let receiver = self.ident("self");
        let dispatch = self.expr(Expr::Match {
            scrutinee: Box::new(receiver),
            arms: state_arms,
        });
        let state_ty = self.machine_ty(machine, "");
        let candidate = self.let_value(CANDIDATE, Some(state_ty), dispatch);
        // Construct the report before changing the candidate receiver, so
        // ordinary candidate/output failure cleanup precedes any commit.
        let output_value = self.ident(OUTPUTS);
        let disposition_value = self.ident(DISPOSITION);
        let report = self.expr(Expr::StructInit {
            name: format!("{}Step", machine.name),
            fields: vec![
                ("outputs".to_string(), output_value),
                ("disposition".to_string(), disposition_value),
            ],
            type_args: None,
            base: None,
        });
        let report_ty = self.machine_ty(machine, "Step");
        let report_binding = self.let_value("_$machine_report", Some(report_ty), report);
        let receiver = self.ident("self");
        let candidate_value = self.ident(CANDIDATE);
        let commit = self.stmt(Stmt::Assign {
            target: receiver,
            op: None,
            value: candidate_value,
        });
        let result = self.ident("_$machine_report");
        Ok(Block {
            stmts: vec![outputs, disposition, candidate, report_binding, commit],
            trailing_expr: Some(Box::new(result)),
        })
    }

    fn transition(
        &mut self,
        machine: &MachineDecl,
        state: &MachineState,
        event: &MachineEvent,
        rule: &MachineTransition,
    ) -> Result<Spanned<Expr>, TypeError> {
        // A wildcard target declares an external transition even if its body
        // returns the source tag. This makes exit/body/entry order independent
        // of the as-yet unevaluated body.
        let run_hooks = rule.reenter || rule.target_state != state.name;
        let mut stmts = Vec::new();
        if run_hooks {
            if let Some(exit) = &state.exit {
                let exit = self.rewrite_block(exit, machine, state, event)?;
                let exit = self.expr(Expr::Block(exit));
                stmts.push(self.stmt(Stmt::Expression(exit)));
            }
        }
        let body = self.rewrite(&rule.body, machine, state, event)?;
        if !run_hooks {
            return Ok(self.block(stmts, body));
        }
        let state_ty = self.machine_ty(machine, "");
        stmts.push(self.let_value("_$machine_next", Some(state_ty), body));
        let mut arms = Vec::new();
        for next in &machine.states {
            let pattern = self.pattern(&next.name, &next.fields, "_$machine_in_");
            let mut locals = self.state_locals(next);
            // A fixed target reaches exactly one arm, so only that arm runs its
            // entry hook. Rewriting the others would check a hook against an
            // input it can never observe.
            let reachable = rule.target_state == "_" || rule.target_state == next.name;
            if let (true, Some(entry)) = (reachable, &next.entry) {
                let entry = self.rewrite_block(entry, machine, next, event)?;
                let entry = self.expr(Expr::Block(entry));
                locals.push(self.stmt(Stmt::Expression(entry)));
            }
            let result = self.state_value(machine, next);
            let body = self.block(locals, result);
            arms.push(MatchArm {
                pattern,
                guard: None,
                body,
            });
        }
        let next = self.ident("_$machine_next");
        let entered = self.expr(Expr::Match {
            scrutinee: Box::new(next),
            arms,
        });
        Ok(self.block(stmts, entered))
    }

    fn machine_ty(&mut self, machine: &MachineDecl, suffix: &str) -> Spanned<TypeExpr> {
        let args: Vec<_> = machine
            .type_params
            .iter()
            .map(|param| self.ty(&param.name))
            .collect();
        (
            TypeExpr::Named {
                name: format!("{}{suffix}", machine.name),
                type_args: (!args.is_empty()).then_some(args),
            },
            self.span(),
        )
    }

    fn refresh_signature(&mut self, function: &mut FnDecl) {
        function.fn_span = self.span();
        function.decl_span = self.span();
        for param in &mut function.params {
            self.refresh_type(&mut param.ty);
        }
        if let Some(ty) = &mut function.return_type {
            self.refresh_type(ty);
        }
    }

    fn refresh_type_decl(&mut self, decl: &mut TypeDecl) -> Result<(), TypeError> {
        for item in &mut decl.body {
            match item {
                TypeBodyItem::Field { ty, span, .. } => {
                    self.refresh_type(ty);
                    *span = self.span();
                }
                TypeBodyItem::Variant(variant) => match &mut variant.kind {
                    VariantKind::Unit => {}
                    VariantKind::Tuple(types) => {
                        for ty in types {
                            self.refresh_type(ty);
                        }
                    }
                    VariantKind::Struct(fields) => {
                        for (_, ty) in fields {
                            self.refresh_type(ty);
                        }
                    }
                },
                TypeBodyItem::Method(_) => {
                    return Err(self.error(
                        "generated machine data declaration unexpectedly contains a method",
                    ))
                }
            }
        }
        Ok(())
    }

    fn refresh_type(&mut self, ty: &mut Spanned<TypeExpr>) {
        ty.1 = self.span();
        match &mut ty.0 {
            TypeExpr::Named { type_args, .. } => {
                if let Some(args) = type_args {
                    for ty in args {
                        self.refresh_type(ty);
                    }
                }
            }
            TypeExpr::Tuple(types) => {
                for ty in types {
                    self.refresh_type(ty);
                }
            }
            TypeExpr::Option(ty) | TypeExpr::Slice(ty) | TypeExpr::Borrow(ty) => {
                self.refresh_type(ty);
            }
            TypeExpr::Result { ok, err }
            | TypeExpr::Fallible {
                success: ok,
                error: err,
            } => {
                self.refresh_type(ok);
                self.refresh_type(err);
            }
            TypeExpr::Array { element, .. }
            | TypeExpr::Pointer {
                pointee: element, ..
            } => self.refresh_type(element),
            TypeExpr::Function {
                params,
                return_type,
                ..
            } => {
                for ty in params {
                    self.refresh_type(ty);
                }
                self.refresh_type(return_type);
            }
            TypeExpr::QualifiedAssocPath(_) | TypeExpr::TraitObject(_) | TypeExpr::Infer => {}
        }
    }

    fn rewrite(
        &mut self,
        source: &Spanned<Expr>,
        machine: &MachineDecl,
        state: &MachineState,
        event: &MachineEvent,
    ) -> Result<Spanned<Expr>, TypeError> {
        let saved = self.origin.clone();
        self.origin = source.1.clone();
        let mut expr = source.0.clone();
        self.rewrite_expr(&mut expr, machine, state, event)?;
        let result = self.expr(expr);
        self.origin = saved;
        Ok(result)
    }

    #[expect(
        clippy::too_many_lines,
        reason = "recursive AST expression rewrite with one arm per admitted shape"
    )]
    fn rewrite_expr(
        &mut self,
        expr: &mut Expr,
        machine: &MachineDecl,
        state: &MachineState,
        event: &MachineEvent,
    ) -> Result<(), TypeError> {
        match expr {
            Expr::Identifier(name) if name == "self" || name == "state" => {
                *expr = self.state_value(machine, state).0;
            }
            Expr::Identifier(name) if machine.states.iter().any(|state| state.name == *name) => {
                let name = name.clone();
                *expr = self.qualified_variant(machine, &name, Vec::new()).0;
            }
            Expr::FieldAccess { object, field } if matches!(&object.0, Expr::Identifier(name) if name == "self" || name == "state") =>
            {
                if !state.fields.iter().any(|(name, _)| name == field) {
                    return Err(
                        self.error(format!("state `{}` has no field `{field}`", state.name))
                    );
                }
                *expr = Expr::Identifier(format!("_$machine_state_{field}"));
            }
            Expr::FieldAccess { object, field } if matches!(&object.0, Expr::Identifier(name) if name == "event") =>
            {
                if !event.fields.iter().any(|(name, _)| name == field) {
                    return Err(self.error(format!(
                        "input event `{}` has no field `{field}`",
                        event.name
                    )));
                }
                *expr = Expr::Identifier(format!("_$machine_event_{field}"));
            }
            Expr::MachineEmit { event_name, fields } => {
                if !machine
                    .emits
                    .iter()
                    .any(|output| output.name == *event_name)
                {
                    return Err(self.error(format!(
                        "`{event_name}` is not declared in the machine's typed output vocabulary"
                    )));
                }
                for (_, value) in fields.iter_mut() {
                    *value = self.rewrite(value, machine, state, event)?;
                }
                let value = self.variant(event_name, fields.clone());
                let receiver = self.ident(OUTPUTS);
                *expr = Expr::MethodCall {
                    receiver: Box::new(receiver),
                    method: "push".to_string(),
                    args: vec![CallArg::Positional(value)],
                };
            }
            Expr::Binary { left, right, .. } | Expr::Coalesce { left, right } => {
                **left = self.rewrite(left, machine, state, event)?;
                **right = self.rewrite(right, machine, state, event)?;
            }
            Expr::Unary { operand, .. } | Expr::Clone(operand) | Expr::PostfixTry(operand) => {
                **operand = self.rewrite(operand, machine, state, event)?;
            }
            Expr::FieldAccess { object, .. } => {
                **object = self.rewrite(object, machine, state, event)?;
            }
            Expr::Index { object, index } => {
                **object = self.rewrite(object, machine, state, event)?;
                **index = self.rewrite(index, machine, state, event)?;
            }
            Expr::Tuple(values) | Expr::Array(values) => {
                for value in values {
                    *value = self.rewrite(value, machine, state, event)?;
                }
            }
            Expr::ArrayRepeat { value, count } => {
                **value = self.rewrite(value, machine, state, event)?;
                **count = self.rewrite(count, machine, state, event)?;
            }
            Expr::MapLiteral { entries } => {
                for (key, value) in entries {
                    *key = self.rewrite(key, machine, state, event)?;
                    *value = self.rewrite(value, machine, state, event)?;
                }
            }
            Expr::Block(block) => *block = self.rewrite_block(block, machine, state, event)?,
            Expr::If {
                condition,
                then_block,
                else_block,
            } => {
                **condition = self.rewrite(condition, machine, state, event)?;
                **then_block = self.rewrite(then_block, machine, state, event)?;
                if let Some(other) = else_block {
                    **other = self.rewrite(other, machine, state, event)?;
                }
            }
            Expr::Match { scrutinee, arms } => {
                **scrutinee = self.rewrite(scrutinee, machine, state, event)?;
                self.rewrite_arms(arms, machine, state, event)?;
            }
            Expr::Call {
                function,
                type_args,
                args,
                ..
            } => {
                **function = self.rewrite(function, machine, state, event)?;
                self.rewrite_args(args, machine, state, event)?;
                if let Some(args) = type_args {
                    for ty in args {
                        self.refresh_type(ty);
                    }
                }
            }
            Expr::MethodCall { receiver, args, .. } => {
                **receiver = self.rewrite(receiver, machine, state, event)?;
                self.rewrite_args(args, machine, state, event)?;
            }
            Expr::ContextVariant(context) => {
                if let Some(record) = &mut context.record {
                    for (_, value) in &mut record.fields {
                        *value = self.rewrite(value, machine, state, event)?;
                    }
                    if let Some(base) = &mut record.base {
                        **base = self.rewrite(base, machine, state, event)?;
                    }
                }
            }
            Expr::StructInit {
                name,
                fields,
                base,
                type_args,
            } => {
                for (_, value) in fields.iter_mut() {
                    *value = self.rewrite(value, machine, state, event)?;
                }
                if let Some(base) = base {
                    **base = self.rewrite(base, machine, state, event)?;
                }
                if let Some(args) = type_args {
                    for ty in args {
                        self.refresh_type(ty);
                    }
                }
                if machine.states.iter().any(|state| state.name == *name) {
                    *expr = self.variant(name, fields.clone()).0;
                }
            }
            Expr::Cast { expr, ty } => {
                **expr = self.rewrite(expr, machine, state, event)?;
                self.refresh_type(ty);
            }
            Expr::Range { start, end, .. } => {
                if let Some(value) = start {
                    **value = self.rewrite(value, machine, state, event)?;
                }
                if let Some(value) = end {
                    **value = self.rewrite(value, machine, state, event)?;
                }
            }
            Expr::InterpolatedString(parts) => {
                for part in parts {
                    if let StringPart::Expr(value) | StringPart::StructuralExpr(value) = part {
                        *value = self.rewrite(value, machine, state, event)?;
                    }
                }
            }
            Expr::Literal(_)
            | Expr::Identifier(_)
            | Expr::RegexLiteral(_)
            | Expr::ByteStringLiteral(_)
            | Expr::ByteArrayLiteral(_) => {}
            other => {
                let construct = match other {
                    Expr::Await(_) => "`await`",
                    Expr::GenBlock { .. } => "a `gen` block",
                    Expr::Spawn { .. } => "`spawn`",
                    _ => "this expression",
                };
                return Err(self.error(format!(
                    "{construct} is not admitted in the pure machine evaluator"
                )));
            }
        }
        Ok(())
    }

    fn rewrite_args(
        &mut self,
        args: &mut [CallArg],
        machine: &MachineDecl,
        state: &MachineState,
        event: &MachineEvent,
    ) -> Result<(), TypeError> {
        for arg in args {
            let value = match arg {
                CallArg::Positional(value) | CallArg::Named { value, .. } => value,
            };
            *value = self.rewrite(value, machine, state, event)?;
        }
        Ok(())
    }
    fn rewrite_arms(
        &mut self,
        arms: &mut [MatchArm],
        machine: &MachineDecl,
        state: &MachineState,
        event: &MachineEvent,
    ) -> Result<(), TypeError> {
        for arm in arms {
            self.refresh_pattern(&mut arm.pattern);
            if let Some(guard) = &mut arm.guard {
                *guard = self.rewrite(guard, machine, state, event)?;
            }
            arm.body = self.rewrite(&arm.body, machine, state, event)?;
        }
        Ok(())
    }
    fn refresh_pattern(&mut self, pattern: &mut Spanned<Pattern>) {
        pattern.1 = self.span();
        match &mut pattern.0 {
            Pattern::Constructor { patterns, .. } | Pattern::Tuple(patterns) => {
                for pattern in patterns {
                    self.refresh_pattern(pattern);
                }
            }
            Pattern::Or(left, right) => {
                self.refresh_pattern(left);
                self.refresh_pattern(right);
            }
            Pattern::Struct { fields, rest, .. } | Pattern::RecordShorthand { fields, rest } => {
                for field in fields {
                    if let Some(pattern) = &mut field.pattern {
                        self.refresh_pattern(pattern);
                    }
                }
                if let Some(rest) = rest {
                    *rest = self.span();
                }
            }
            Pattern::ContextVariant(ContextVariantPattern {
                payload: Some(payload),
                ..
            })
            | Pattern::NominalPath {
                payload: Some(payload),
                ..
            } => match payload {
                NominalPatternPayload::Tuple(patterns) => {
                    for pattern in patterns {
                        self.refresh_pattern(pattern);
                    }
                }
                NominalPatternPayload::Record { fields, rest } => {
                    for field in fields {
                        if let Some(pattern) = &mut field.pattern {
                            self.refresh_pattern(pattern);
                        }
                    }
                    if let Some(rest) = rest {
                        *rest = self.span();
                    }
                }
            },
            _ => {}
        }
    }
    fn rewrite_block(
        &mut self,
        block: &Block,
        machine: &MachineDecl,
        state: &MachineState,
        event: &MachineEvent,
    ) -> Result<Block, TypeError> {
        let mut block = block.clone();
        for (stmt, span) in &mut block.stmts {
            *span = self.span();
            match stmt {
                Stmt::Let {
                    pattern,
                    ty,
                    value,
                    else_block,
                } => {
                    self.refresh_pattern(pattern);
                    if let Some(ty) = ty {
                        self.refresh_type(ty);
                    }
                    if let Some(value) = value {
                        *value = self.rewrite(value, machine, state, event)?;
                    }
                    if let Some(other) = else_block {
                        *other = self.rewrite_block(other, machine, state, event)?;
                    }
                }
                Stmt::Var { ty, value, .. } => {
                    if let Some(ty) = ty {
                        self.refresh_type(ty);
                    }
                    if let Some(value) = value {
                        *value = self.rewrite(value, machine, state, event)?;
                    }
                }
                Stmt::Assign { target, value, .. } => {
                    *target = self.rewrite(target, machine, state, event)?;
                    *value = self.rewrite(value, machine, state, event)?;
                }
                Stmt::Expression(value) => *value = self.rewrite(value, machine, state, event)?,
                Stmt::Match { scrutinee, arms } => {
                    *scrutinee = self.rewrite(scrutinee, machine, state, event)?;
                    self.rewrite_arms(arms, machine, state, event)?;
                }
                Stmt::If {
                    condition,
                    then_block,
                    else_block,
                } => {
                    *condition = self.rewrite(condition, machine, state, event)?;
                    *then_block = self.rewrite_block(then_block, machine, state, event)?;
                    if let Some(other) = else_block {
                        if let Some(block) = &mut other.block {
                            *block = self.rewrite_block(block, machine, state, event)?;
                        }
                        if other.if_stmt.is_some() {
                            return Err(self.error(
                                "machine statement else-if must use an ordinary nested block",
                            ));
                        }
                    }
                }
                Stmt::For {
                    pattern,
                    iterable,
                    body,
                    ..
                } => {
                    self.refresh_pattern(pattern);
                    *iterable = self.rewrite(iterable, machine, state, event)?;
                    *body = self.rewrite_block(body, machine, state, event)?;
                }
                Stmt::While {
                    condition, body, ..
                } => {
                    *condition = self.rewrite(condition, machine, state, event)?;
                    *body = self.rewrite_block(body, machine, state, event)?;
                }
                Stmt::Loop { body, .. } => {
                    *body = self.rewrite_block(body, machine, state, event)?;
                }
                Stmt::Break { value, .. } => {
                    if let Some(value) = value {
                        *value = self.rewrite(value, machine, state, event)?;
                    }
                }
                Stmt::Continue { .. } => {}
                _ => {
                    return Err(
                        self.error("this statement is not admitted in the pure machine evaluator")
                    )
                }
            }
        }
        if let Some(value) = &mut block.trailing_expr {
            **value = self.rewrite(value, machine, state, event)?;
        }
        Ok(block)
    }
}

fn rules_for<'a>(
    machine: &'a MachineDecl,
    state: &MachineState,
    event: &MachineEvent,
) -> Vec<&'a MachineTransition> {
    // State-local rules precede wildcard ancestor/fallback rules.
    let mut rules: Vec<_> = machine
        .transitions
        .iter()
        .filter(|rule| rule.event_name == event.name && rule.source_state == state.name)
        .collect();
    if !rules.iter().any(|rule| rule.guard.is_none()) {
        rules.extend(
            machine
                .transitions
                .iter()
                .filter(|rule| rule.event_name == event.name && rule.source_state == "_"),
        );
    }
    rules
}

/// Does `expr` produce `target` on every normal path?
///
/// `source` is the rule's source state. `self` and `state` name the refined
/// source payload, so a rule whose source and target are the same state
/// produces that state by naming it, and one whose source is a wildcard or a
/// different state does not.
fn returns_variant(expr: &Expr, target: &str, source: &str, machine: &str) -> bool {
    match expr {
        Expr::ContextVariant(context) => context.name == target,
        Expr::Identifier(name) if name == "self" || name == "state" => source == target,
        Expr::Identifier(name) => name == target,
        Expr::StructInit { name, .. } => name == target || name == &format!("{machine}.{target}"),
        Expr::FieldAccess { object, field } => {
            field == target && matches!(&object.0, Expr::Identifier(name) if name == machine)
        }
        Expr::Block(block) => block
            .trailing_expr
            .as_ref()
            .is_some_and(|value| returns_variant(&value.0, target, source, machine)),
        Expr::If {
            then_block,
            else_block: Some(other),
            ..
        } => {
            returns_variant(&then_block.0, target, source, machine)
                && returns_variant(&other.0, target, source, machine)
        }
        Expr::Match { arms, .. } => {
            !arms.is_empty()
                && arms
                    .iter()
                    .all(|arm| returns_variant(&arm.body.0, target, source, machine))
        }
        _ => false,
    }
}

fn item_sources(
    graph: &hew_parser::module::ModuleGraph,
    id: &hew_parser::module::ModuleId,
) -> Vec<PathBuf> {
    let Some(module) = graph.modules.get(id) else {
        return Vec::new();
    };
    module
        .items
        .iter()
        .enumerate()
        .filter_map(|(ordinal, _)| {
            graph
                .item_source(id, ordinal)
                .or_else(|| module.source_paths.first())
                .cloned()
        })
        .collect()
}

/// Import inventories are projections of the normalized graph, never a second
/// normalization pass with different expression or method identities.
fn project_normalized_imports(program: &mut Program) {
    let Some(mut graph) = program.module_graph.take() else {
        return;
    };
    for id in graph.topo_order.clone() {
        let Some(module) = graph.modules.get(&id) else {
            continue;
        };
        let updates = import_updates(&module.items, &module.imports, &graph);
        if let Some(module) = graph.modules.get_mut(&id) {
            apply_import_updates(&mut module.items, updates);
        }
    }
    if let Some(root) = graph.modules.get(&graph.root) {
        let updates = import_updates(&program.items, &root.imports, &graph);
        apply_import_updates(&mut program.items, updates);
    }
    program.module_graph = Some(graph);
}

type ImportUpdate = (usize, Vec<Spanned<Item>>, Vec<PathBuf>);

fn import_updates(
    items: &[Spanned<Item>],
    imports: &[hew_parser::module::ModuleImport],
    graph: &hew_parser::module::ModuleGraph,
) -> Vec<ImportUpdate> {
    items
        .iter()
        .enumerate()
        .filter_map(|(ordinal, (item, span))| {
            let Item::Import(decl) = item else {
                return None;
            };
            let import = imports.iter().find(|import| {
                import.span == *span
                    && graph
                        .modules
                        .get(&import.target)
                        .is_some_and(|target| target.source_paths == decl.resolved_source_paths)
            })?;
            let target = graph.modules.get(&import.target)?;
            Some((
                ordinal,
                target.items.clone(),
                item_sources(graph, &import.target),
            ))
        })
        .collect()
}

fn apply_import_updates(items: &mut [Spanned<Item>], updates: Vec<ImportUpdate>) {
    for (ordinal, resolved_items, sources) in updates {
        let Item::Import(decl) = &mut items[ordinal].0 else {
            unreachable!("import projection preserves item kinds")
        };
        decl.resolved_items = Some(resolved_items.into());
        decl.resolved_item_source_paths = sources;
    }
}
