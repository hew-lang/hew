use std::collections::HashMap;

use hew_parser::ast::{
    BinaryOp, Block, Expr, FnDecl, Item, LambdaParam, Literal, MachineDecl, Pattern, Program,
    ResourceMarker, SelectArm, Span, Spanned, Stmt, TimeoutClause, TypeBodyItem, TypeDecl,
    TypeExpr,
};
use hew_types::{MethodCallRewrite, ResolvedTy, SpanKey, TypeCheckOutput};

use crate::builtin_type_classes::seed_builtin_type_classes;
use crate::diagnostic::{HirDiagnostic, HirDiagnosticKind};
use crate::ids::{BindingId, IdGen, ItemId, ResolvedRef};
use crate::node::{
    HirBinding, HirBlock, HirCaptureKind, HirExpr, HirExprKind, HirField, HirFn, HirItem,
    HirLambdaCapture, HirLiteral, HirMachineDecl, HirMachineEvent, HirMachineState,
    HirMachineTransition, HirModule, HirSelect, HirSelectArm, HirSelectArmKind, HirStmt,
    HirStmtKind, HirTypeDecl,
};
use crate::{IntentKind, ValueClass};

#[derive(Debug, Clone, Default)]
pub struct ResolutionCtx;

#[derive(Debug, Clone, PartialEq)]
pub struct LowerOutput {
    pub module: HirModule,
    pub diagnostics: Vec<HirDiagnostic>,
}

/// Pre-collected signature of a top-level function item.
#[derive(Debug)]
struct FnEntry {
    id: ItemId,
    return_ty: ResolvedTy,
    param_tys: Vec<ResolvedTy>,
}

#[must_use]
pub fn lower_program(
    program: &Program,
    type_check_output: &TypeCheckOutput,
    _ctx: &ResolutionCtx,
) -> LowerOutput {
    let mut ctx = LowerCtx::new(type_check_output);

    // First pass: collect all function signatures so that forward and mutual
    // references in call expressions resolve to the correct return type.
    // Diagnostics from this pass are discarded — the same types are re-lowered
    // in the second pass, which is where canonical diagnostics are emitted.
    for (item, _) in &program.items {
        if let Item::Function(func) = item {
            let id = ctx.ids.item();
            let return_ty = func
                .return_type
                .as_ref()
                .map_or(ResolvedTy::Unit, |ty| ctx.lower_type(ty));
            let param_tys = func.params.iter().map(|p| ctx.lower_type(&p.ty)).collect();
            ctx.fn_registry.insert(
                func.name.clone(),
                FnEntry {
                    id,
                    return_ty,
                    param_tys,
                },
            );
        }
    }
    // Discard first-pass diagnostics; the second pass will re-emit any real ones.
    ctx.diagnostics.clear();

    // Second pass: lower type declarations and populate the per-module
    // type-class registry. Stored here so the source-order pass can emit them
    // in program order without a second lowering call. Function bodies depend
    // on type markers (so `ValueClass::of_ty` resolves `Named` types
    // correctly), but type-decl bodies do not depend on function signatures,
    // so this pre-pass can safely run before the combined item pass below.
    let mut type_decl_cache: HashMap<*const hew_parser::ast::TypeDecl, HirTypeDecl> =
        HashMap::new();
    for (item, span) in &program.items {
        if let Item::TypeDecl(decl) = item {
            let hir_decl = ctx.lower_type_decl(decl, span.clone());
            let close_method = if hir_decl.marker == ResourceMarker::Resource {
                hir_decl
                    .consuming_methods
                    .iter()
                    .find(|m| m.as_str() == "close")
                    .cloned()
            } else {
                None
            };
            ctx.type_classes
                .insert(hir_decl.name.clone(), (hir_decl.marker, close_method));
            type_decl_cache.insert(decl as *const _, hir_decl);
        }
    }

    // Third pass: emit all items in source order now that both fn signatures
    // and type markers are fully resolved.
    let mut items: Vec<HirItem> = Vec::new();
    for (item, span) in &program.items {
        match item {
            Item::TypeDecl(decl) => {
                // Retrieve the already-lowered decl so diagnostics are not
                // emitted a second time.
                if let Some(hir_decl) = type_decl_cache.remove(&(decl as *const _)) {
                    items.push(HirItem::TypeDecl(hir_decl));
                }
            }
            Item::Function(func) => {
                items.push(HirItem::Function(ctx.lower_fn(func, span.clone())));
            }
            Item::Machine(machine) => {
                if let Some(hir_machine) = ctx.lower_machine(machine, span.clone()) {
                    items.push(HirItem::Machine(hir_machine));
                }
            }
            _ => ctx.unsupported(span.clone(), "top-level-item", "slice-2"),
        }
    }

    LowerOutput {
        module: HirModule {
            items,
            type_classes: ctx.type_classes,
        },
        diagnostics: ctx.diagnostics,
    }
}

#[derive(Debug)]
struct LowerCtx {
    ids: IdGen,
    scopes: Vec<HashMap<String, (BindingId, ResolvedTy)>>,
    /// Maps function name → pre-allocated `ItemId` + return type + param types.
    fn_registry: HashMap<String, FnEntry>,
    /// Per-named-type marker + close-method registry. Pre-populated from
    /// every `Item::TypeDecl` before function bodies lower so that
    /// `ValueClass::of_ty` can resolve `Named` types as the body is walked.
    /// Also seeded with M2 substrate types (Duplex, Sink, Stream, etc.) via
    /// `builtin_type_classes::seed_builtin_type_classes` before the `TypeDecl` loop.
    type_classes: crate::value_class::TypeClassTable,
    diagnostics: Vec<HirDiagnostic>,
    /// Checker-owned method-call lowering decisions. Keyed by the method-call
    /// expression span. `Expr::MethodCall` lowering looks up each call site
    /// here and rewrites to `HirExprKind::Call` with the runtime symbol.
    /// A missing entry is a fail-closed diagnostic (`MethodCallNoRewrite`).
    method_call_rewrites: HashMap<SpanKey, MethodCallRewrite>,
    /// Depth counter for nested `scope{}` bodies. When > 0, statement-expression
    /// calls are inferred as child-task spawns (TI-1); outside any scope body
    /// all calls are synchronous (TI-3). Using a depth counter rather than a
    /// bool supports nested `scope{}` blocks correctly.
    scope_depth: u32,
    /// Set to `true` immediately before lowering the expression of a
    /// `Stmt::Expression` statement. `lower_expr` consumes it via
    /// `mem::replace(…, false)` at entry, so all recursive calls see `false`.
    /// This lets `Expr::Await` check whether it is the direct statement, not
    /// a sub-expression of a return value, argument, binary operand, etc.
    /// (TI-4 position rule.)
    statement_position: bool,
    /// `Some((let_id, let_name))` while lowering the body of an actor-lambda
    /// that is the value of `let <let_name> = actor |..| { .. }`. The
    /// capture-strength classifier inside the body walk compares each
    /// resolved capture's `BindingId` to `let_id` to discriminate the
    /// self-reference (Weak, §5.9 ratification 2) from every other captured
    /// binding (Strong). Nested actor-lambdas restore the prior value via
    /// `mem::replace` so the outer self-binding doesn't leak into an inner
    /// lambda's classification.
    current_actor_self: Option<(BindingId, String)>,
}

impl LowerCtx {
    fn new(tc_output: &TypeCheckOutput) -> Self {
        let mut type_classes = crate::value_class::TypeClassTable::default();
        // Seed compiler-known M2 substrate types before source-order TypeDecls.
        // This ensures `ValueClass::of_ty` resolves Duplex/Sink/Stream as
        // AffineResource even though they are not user-declared TypeDecl items.
        seed_builtin_type_classes(&mut type_classes);
        Self {
            ids: IdGen::default(),
            scopes: Vec::new(),
            fn_registry: HashMap::new(),
            type_classes,
            diagnostics: Vec::new(),
            method_call_rewrites: tc_output.method_call_rewrites.clone(),
            scope_depth: 0,
            statement_position: false,
            current_actor_self: None,
        }
    }
}

impl LowerCtx {
    fn lower_fn(&mut self, func: &FnDecl, span: std::ops::Range<usize>) -> HirFn {
        // Use the stable ItemId pre-allocated during the first pass.
        let id = self
            .fn_registry
            .get(&func.name)
            .map_or_else(|| self.ids.item(), |entry| entry.id);

        self.push_scope();
        let mut params = Vec::new();
        for param in &func.params {
            let ty = self.lower_type(&param.ty);
            let binding = self.bind(param.name.clone(), ty, param.is_mutable, param.ty.1.clone());
            params.push(binding);
        }

        let return_ty = func
            .return_type
            .as_ref()
            .map_or(ResolvedTy::Unit, |ty| self.lower_type(ty));
        let body = self.lower_block(&func.body, &return_ty);
        self.pop_scope();

        HirFn {
            id,
            node: self.ids.node(),
            name: func.name.clone(),
            type_params: func
                .type_params
                .as_ref()
                .map(|params| params.iter().map(|param| param.name.clone()).collect())
                .unwrap_or_default(),
            params,
            return_ty,
            body,
            span,
        }
    }

    fn lower_type_decl(&mut self, decl: &TypeDecl, span: std::ops::Range<usize>) -> HirTypeDecl {
        // Generic resource/linear types are rejected — the type→class map is
        // keyed by name, not by instantiation. This rule belongs at the
        // checker boundary (LESSONS `checker-output-boundary`); HIR is the
        // first place the marker is durable, so the check lands here.
        if decl.resource_marker != ResourceMarker::None && decl.type_params.is_some() {
            self.diagnostics.push(HirDiagnostic::new(
                HirDiagnosticKind::ResourceGenericUnsupported {
                    name: decl.name.clone(),
                },
                span.clone(),
                "`#[resource]` / `#[linear]` types cannot have type parameters in v0.5",
            ));
        }

        match decl.resource_marker {
            ResourceMarker::Resource => {
                // `#[resource]` must declare `close(consuming self)`.
                let has_close = decl
                    .consuming_methods
                    .iter()
                    .any(|name| name.as_str() == "close");
                if !has_close {
                    self.diagnostics.push(HirDiagnostic::new(
                        HirDiagnosticKind::ResourceMissingClose {
                            name: decl.name.clone(),
                        },
                        span.clone(),
                        "`#[resource]` type must declare `fn close(consuming self) -> ...` \
                         in its body; the implicit drop contract dispatches to this method",
                    ));
                }
            }
            ResourceMarker::Linear => {
                // `#[linear]` must declare at least one consuming method.
                if decl.consuming_methods.is_empty() {
                    self.diagnostics.push(HirDiagnostic::new(
                        HirDiagnosticKind::LinearNoConsumingMethods {
                            name: decl.name.clone(),
                        },
                        span.clone(),
                        "`#[linear]` type must declare at least one `consuming self` method; \
                         without one no exit path could exhaust a binding of this type",
                    ));
                }
            }
            ResourceMarker::None => {}
        }

        // Carry the field set so dump-hir and future analysis have something
        // to reason about; methods are out of scope for v0.5 MIR lowering
        // and stay on the parser-side `TypeBodyItem::Method`.
        let mut fields = Vec::new();
        for item in &decl.body {
            if let TypeBodyItem::Field {
                name,
                ty,
                span: field_span,
                ..
            } = item
            {
                fields.push(HirField {
                    name: name.clone(),
                    ty: self.lower_type(ty),
                    span: field_span.clone(),
                });
            }
            // `TypeBodyItem::Method` is not lowered into HIR in v0.5 — the
            // method-call expression form has no HIR/MIR lowering yet, so
            // method bodies cannot be exercised. Their *declared names* are
            // captured upstream as `TypeDecl.consuming_methods` and travel
            // on `HirTypeDecl.consuming_methods`. `TypeBodyItem::Variant`
            // belongs to enum bodies and is similarly out of slice scope.
        }

        HirTypeDecl {
            id: self.ids.item(),
            node: self.ids.node(),
            name: decl.name.clone(),
            marker: decl.resource_marker,
            consuming_methods: decl.consuming_methods.clone(),
            fields,
            span,
        }
    }

    #[expect(
        clippy::too_many_lines,
        reason = "machine lowering has three distinct phases (structure, checks, assembly) \
                  that read more clearly as a single function than as multiple helpers"
    )]
    fn lower_machine(
        &mut self,
        decl: &MachineDecl,
        span: std::ops::Range<usize>,
    ) -> Option<HirMachineDecl> {
        // Lower states.
        let mut hir_states = Vec::new();
        for state in &decl.states {
            let fields: Vec<HirField> = state
                .fields
                .iter()
                .map(|(name, ty)| HirField {
                    name: name.clone(),
                    ty: self.lower_type(ty),
                    span: ty.1.clone(),
                })
                .collect();

            // Shallow-scan the entry and exit blocks for field-assignment targets.
            // Lane A does not fully lower block bodies; this is enough for
            // effect-parity checking.
            let entry_writes = state
                .entry
                .as_ref()
                .map(collect_assigned_field_names)
                .unwrap_or_default();
            let exit_writes = state
                .exit
                .as_ref()
                .map(collect_assigned_field_names)
                .unwrap_or_default();

            hir_states.push(HirMachineState {
                name: state.name.clone(),
                fields,
                has_entry: state.entry.is_some(),
                has_exit: state.exit.is_some(),
                entry_writes,
                exit_writes,
                span: span.clone(),
            });
        }

        // Lower events.
        let hir_events: Vec<HirMachineEvent> = decl
            .events
            .iter()
            .map(|ev| {
                let fields = ev
                    .fields
                    .iter()
                    .map(|(name, ty)| HirField {
                        name: name.clone(),
                        ty: self.lower_type(ty),
                        span: ty.1.clone(),
                    })
                    .collect();
                HirMachineEvent {
                    name: ev.name.clone(),
                    fields,
                    span: span.clone(),
                }
            })
            .collect();

        // Lower transitions — shallow: record names, guard presence, body writes,
        // and emitted event names (for static checks). Body expressions are not
        // lowered to HirExpr in Lane A.
        let hir_transitions: Vec<HirMachineTransition> = decl
            .transitions
            .iter()
            .map(|tr| {
                let is_self_transition =
                    tr.source_state == tr.target_state && tr.source_state != "_";
                let body_writes = collect_assigned_field_names_expr(&tr.body.0);
                let body_emits = collect_emitted_events(&tr.body.0);
                HirMachineTransition {
                    event_name: tr.event_name.clone(),
                    source_state: tr.source_state.clone(),
                    target_state: tr.target_state.clone(),
                    has_guard: tr.guard.is_some(),
                    is_self_transition,
                    reenter: tr.reenter,
                    body_writes,
                    body_emits,
                    span: tr.body.1.clone(),
                }
            })
            .collect();

        // ── Static checks ────────────────────────────────────────────────────

        // 1. Exhaustiveness: every concrete (state, event) pair must have a
        //    transition, or a `default` arm must exist, or a wildcard source `_`
        //    covers it.
        if !decl.has_default {
            let mut missing: Vec<(String, String)> = Vec::new();
            for state in &decl.states {
                for event in &decl.events {
                    let covered = decl.transitions.iter().any(|tr| {
                        tr.event_name == event.name
                            && (tr.source_state == state.name || tr.source_state == "_")
                    });
                    if !covered {
                        missing.push((state.name.clone(), event.name.clone()));
                    }
                }
            }
            if !missing.is_empty() {
                self.diagnostics.push(HirDiagnostic::new(
                    HirDiagnosticKind::MachineExhaustivenessViolation {
                        machine_name: decl.name.clone(),
                        missing,
                    },
                    span.clone(),
                    format!(
                        "machine `{}` does not handle all (state, event) pairs; \
                         add the missing transitions or a `default` arm",
                        decl.name
                    ),
                ));
                return None;
            }
        }

        // 2. Self-transition @reenter rule: a non-empty self-loop body without
        //    @reenter is a compile error. Empty body OR @reenter are both OK.
        //    "Empty" means the body resolves to `Expr::Identifier(target_state)`
        //    (the no-body semicolon shorthand) or an `Expr::Block` with no stmts
        //    and no trailing expression.
        for tr in decl.transitions.iter().zip(hir_transitions.iter()) {
            let (ast_tr, hir_tr) = tr;
            if !hir_tr.is_self_transition || hir_tr.reenter {
                continue;
            }
            let body_is_empty = is_empty_self_body(&ast_tr.body.0, &hir_tr.target_state);
            if !body_is_empty {
                self.diagnostics.push(HirDiagnostic::new(
                    HirDiagnosticKind::MachineSelfTransitionNeedsReenter {
                        machine_name: decl.name.clone(),
                        state_name: hir_tr.source_state.clone(),
                        event_name: hir_tr.event_name.clone(),
                    },
                    hir_tr.span.clone(),
                    format!(
                        "self-transition `on {event}` in state `{state}` has a non-empty body \
                         but is not annotated `@reenter`; annotate with `@reenter` to opt in to \
                         Mealy re-entry semantics, or remove the body",
                        event = hir_tr.event_name,
                        state = hir_tr.source_state,
                    ),
                ));
            }
        }

        // 3. Effect-parity: a transition body that writes a field also written
        //    by the *target* state's `entry` block or the *source* state's `exit`
        //    block creates ambiguous initialization/teardown order.
        for tr in &hir_transitions {
            if tr.body_writes.is_empty() {
                continue;
            }
            // Check target entry conflict.
            if let Some(target) = hir_states.iter().find(|s| s.name == tr.target_state) {
                for field in &tr.body_writes {
                    if let Some((_, entry_assign_span)) =
                        target.entry_writes.iter().find(|(n, _)| n == field)
                    {
                        self.diagnostics.push(
                            HirDiagnostic::new(
                                HirDiagnosticKind::MachineEffectParityViolation {
                                    machine_name: decl.name.clone(),
                                    state_name: tr.target_state.clone(),
                                    field_name: field.clone(),
                                    transition_event: tr.event_name.clone(),
                                    is_entry_conflict: true,
                                },
                                tr.span.clone(),
                                format!(
                                    "transition `on {}` body and state `{}` entry block both \
                                     write field `{}`; remove the write from one site",
                                    tr.event_name, tr.target_state, field
                                ),
                            )
                            .with_secondary_spans(vec![(
                                entry_assign_span.clone(),
                                format!(
                                    "state `{}` entry block assigns `{}` here",
                                    tr.target_state, field
                                ),
                            )]),
                        );
                    }
                }
            }
            // Check source exit conflict.
            if let Some(source) = hir_states.iter().find(|s| s.name == tr.source_state) {
                for field in &tr.body_writes {
                    if let Some((_, exit_assign_span)) =
                        source.exit_writes.iter().find(|(n, _)| n == field)
                    {
                        self.diagnostics.push(
                            HirDiagnostic::new(
                                HirDiagnosticKind::MachineEffectParityViolation {
                                    machine_name: decl.name.clone(),
                                    state_name: tr.source_state.clone(),
                                    field_name: field.clone(),
                                    transition_event: tr.event_name.clone(),
                                    is_entry_conflict: false,
                                },
                                tr.span.clone(),
                                format!(
                                    "transition `on {}` body and state `{}` exit block both \
                                     write field `{}`; remove the write from one site",
                                    tr.event_name, tr.source_state, field
                                ),
                            )
                            .with_secondary_spans(vec![(
                                exit_assign_span.clone(),
                                format!(
                                    "state `{}` exit block assigns `{}` here",
                                    tr.source_state, field
                                ),
                            )]),
                        );
                    }
                }
            }
        }

        // 4. Emit-cycle: `on E` transition that directly emits `E` would
        //    immediately re-trigger its own handler.
        for tr in &hir_transitions {
            if tr.body_emits.contains(&tr.event_name) {
                self.diagnostics.push(HirDiagnostic::new(
                    HirDiagnosticKind::MachineEmitCycle {
                        machine_name: decl.name.clone(),
                        event_name: tr.event_name.clone(),
                    },
                    tr.span.clone(),
                    format!(
                        "transition `on {}` emits `{}` which immediately re-triggers itself; \
                         rename the emitted event or remove the emit",
                        tr.event_name, tr.event_name
                    ),
                ));
            }
        }

        // Fail-closed: if any diagnostic was pushed for this machine, abort.
        // (Effect-parity, self-transition, and emit-cycle diagnostics are not
        // return-None by themselves but we abort to avoid a partially-valid
        // machine in HIR.)
        let has_machine_errors = self.diagnostics.iter().any(|d| {
            matches!(
                &d.kind,
                HirDiagnosticKind::MachineSelfTransitionNeedsReenter { machine_name, .. }
                | HirDiagnosticKind::MachineEffectParityViolation { machine_name, .. }
                | HirDiagnosticKind::MachineEmitCycle { machine_name, .. }
                if machine_name == &decl.name
            )
        });
        if has_machine_errors {
            return None;
        }

        Some(HirMachineDecl {
            id: self.ids.item(),
            node: self.ids.node(),
            name: decl.name.clone(),
            states: hir_states,
            events: hir_events,
            transitions: hir_transitions,
            has_default: decl.has_default,
            span,
        })
    }

    fn lower_block(&mut self, block: &Block, expected_ty: &ResolvedTy) -> HirBlock {
        self.push_scope();
        let scope = self.ids.scope();
        let mut statements = Vec::new();
        for (stmt, span) in &block.stmts {
            statements.extend(self.lower_stmt_multi(stmt, span.clone(), expected_ty.clone()));
        }
        let tail = block
            .trailing_expr
            .as_ref()
            .map(|expr| Box::new(self.lower_expr(expr, IntentKind::Read)));
        let ty = tail
            .as_ref()
            .map_or(ResolvedTy::Unit, |expr| expr.ty.clone());
        self.pop_scope();

        HirBlock {
            node: self.ids.node(),
            scope,
            statements,
            tail,
            ty,
            span: 0..0,
        }
    }

    /// Lower a statement, returning zero or more `HirStmt`s.
    ///
    /// Most statements produce exactly one `HirStmt` (delegated to `lower_stmt`).
    /// `let (a, b) = expr;` (Q33 tuple-let) produces:
    ///   1. `let __tuple_N = expr;`    — binds the tuple value to a synthetic temp
    ///   2. `let a = __tuple_N.0;`     — per-element projection (as many as elements)
    ///   3. `let b = __tuple_N.1;`
    ///
    /// The element projections use a synthetic `HirExprKind::TupleIndex` node.
    /// Downstream MIR lowering handles tuple projections in the `Expr::Call` return
    /// path for `duplex_pair`.
    #[expect(
        clippy::too_many_lines,
        reason = "tuple-let expansion has three phases (validation, temp-bind, per-element loop) \
                  that read more clearly as a single function; splitting would obscure the \
                  invariant that temp-bind and per-element refs share the same temp_name"
    )]
    fn lower_stmt_multi(
        &mut self,
        stmt: &Stmt,
        span: std::ops::Range<usize>,
        return_ty: ResolvedTy,
    ) -> Vec<HirStmt> {
        // Tuple-let: `let (a, b, ...) = value_expr;`
        if let Stmt::Let {
            pattern,
            ty: annotation,
            value: Some(value_expr),
        } = stmt
        {
            if let Pattern::Tuple(element_patterns) = &pattern.0 {
                // Lower the tuple value once into a synthetic temp binding.
                let tuple_val = self.lower_expr(value_expr, IntentKind::Consume);
                let tuple_ty = tuple_val.ty.clone();

                // Validate the pattern element count against the inferred tuple type.
                let element_tys: Vec<ResolvedTy> = if let ResolvedTy::Tuple(elems) = &tuple_ty {
                    if elems.len() != element_patterns.len() {
                        self.diagnostics.push(HirDiagnostic::new(
                            HirDiagnosticKind::CutoverUnsupported {
                                construct: format!(
                                    "tuple pattern with {} elements for tuple with {} elements",
                                    element_patterns.len(),
                                    elems.len()
                                ),
                                slice_target: "type-checker".to_string(),
                            },
                            span.clone(),
                            "tuple pattern element count does not match tuple value arity",
                        ));
                        return vec![HirStmt {
                            node: self.ids.node(),
                            kind: HirStmtKind::Expr(
                                self.unsupported_expr(span, "tuple arity mismatch"),
                            ),
                            span: 0..0,
                        }];
                    }
                    elems.clone()
                } else if annotation.is_none() {
                    // Type not yet resolved — use Unit for each element (diagnostic
                    // already emitted by the checker; HIR does best-effort lowering).
                    vec![ResolvedTy::Unit; element_patterns.len()]
                } else {
                    self.diagnostics.push(HirDiagnostic::new(
                        HirDiagnosticKind::CutoverUnsupported {
                            construct: "tuple pattern on non-tuple value".to_string(),
                            slice_target: "type-checker".to_string(),
                        },
                        span.clone(),
                        "tuple-let pattern requires the right-hand side to have a tuple type",
                    ));
                    return vec![HirStmt {
                        node: self.ids.node(),
                        kind: HirStmtKind::Expr(
                            self.unsupported_expr(span, "tuple pattern on non-tuple"),
                        ),
                        span: 0..0,
                    }];
                };

                // Synthetic temp name — unlikely to collide with user identifiers.
                let temp_name = format!("__tuple_{}", self.ids.binding().0);
                let temp_binding =
                    self.bind(temp_name.clone(), tuple_ty.clone(), false, span.clone());
                let temp_stmt = HirStmt {
                    node: self.ids.node(),
                    kind: HirStmtKind::Let(temp_binding, Some(tuple_val)),
                    span: span.clone(),
                };

                let mut stmts = vec![temp_stmt];

                // Per-element projection lets.
                for (idx, (elem_pat, elem_ty)) in
                    element_patterns.iter().zip(element_tys).enumerate()
                {
                    let elem_name = match &elem_pat.0 {
                        Pattern::Identifier(n) => n.clone(),
                        Pattern::Wildcard => format!("_{idx}"),
                        _ => {
                            // Nested tuple / constructor patterns are out of scope.
                            self.diagnostics.push(HirDiagnostic::new(
                                HirDiagnosticKind::CutoverUnsupported {
                                    construct: "nested pattern in tuple-let".to_string(),
                                    slice_target: "pattern-matching".to_string(),
                                },
                                elem_pat.1.clone(),
                                "only identifier and wildcard patterns are supported \
                                 inside tuple-let in v0.5",
                            ));
                            format!("__unsupported_{idx}")
                        }
                    };

                    // Build a TupleIndex expression: `__tuple_N.<idx>`.
                    let temp_ref = HirExpr {
                        node: self.ids.node(),
                        site: self.ids.site(),
                        value_class: ValueClass::of_ty(&tuple_ty, &self.type_classes),
                        ty: tuple_ty.clone(),
                        intent: IntentKind::Read,
                        kind: HirExprKind::BindingRef {
                            name: temp_name.clone(),
                            // The temp was just bound so it is always resolved.
                            resolved: ResolvedRef::Unresolved,
                        },
                        span: span.clone(),
                    };
                    let projection = HirExpr {
                        node: self.ids.node(),
                        site: self.ids.site(),
                        value_class: ValueClass::of_ty(&elem_ty, &self.type_classes),
                        ty: elem_ty.clone(),
                        intent: IntentKind::Read,
                        kind: HirExprKind::TupleIndex {
                            tuple: Box::new(temp_ref),
                            index: idx,
                        },
                        span: elem_pat.1.clone(),
                    };

                    let elem_binding = self.bind(elem_name, elem_ty, false, elem_pat.1.clone());
                    stmts.push(HirStmt {
                        node: self.ids.node(),
                        kind: HirStmtKind::Let(elem_binding, Some(projection)),
                        span: elem_pat.1.clone(),
                    });
                }

                return stmts;
            }
        }

        // Non-tuple statements: delegate to the single-statement path.
        vec![self.lower_stmt(stmt, span, return_ty)]
    }

    #[allow(
        clippy::too_many_lines,
        reason = "single large match on stmt variants; splitting would hurt readability"
    )]
    fn lower_stmt(
        &mut self,
        stmt: &Stmt,
        span: std::ops::Range<usize>,
        return_ty: ResolvedTy,
    ) -> HirStmt {
        let kind = match stmt {
            Stmt::Let { pattern, ty, value } => {
                // `await expr` is only legal as a statement-expression inside a
                // scope{} body (TI-4). Using it as a let-value is always rejected —
                // the await result is consumed immediately and has no place to bind.
                if let Some(val_expr) = value {
                    if matches!(&val_expr.0, Expr::Await(_)) {
                        self.diagnostics.push(HirDiagnostic::new(
                            HirDiagnosticKind::AwaitOutOfPosition,
                            val_expr.1.clone(),
                            "`await` cannot be used as a let-value; \
                             it is only legal as a statement-expression inside a `scope{}` body",
                        ));
                        let name = self
                            .pattern_name(pattern)
                            .unwrap_or_else(|| "_".to_string());
                        let binding_ty = ty
                            .as_ref()
                            .map_or(ResolvedTy::Unit, |ty| self.lower_type(ty));
                        let binding = self.bind(name, binding_ty, false, pattern.1.clone());
                        let unsupported =
                            self.unsupported_expr(val_expr.1.clone(), "`await` in let-value");
                        return HirStmt {
                            node: self.ids.node(),
                            kind: HirStmtKind::Let(binding, Some(unsupported)),
                            span,
                        };
                    }
                }
                // Forward-bind for actor-lambda RHS. When the value is
                // `actor |params| { body }` and the let-pattern is a bare
                // identifier, the body may reference its own let-name for
                // recursive self-dispatch (HEW-SPEC §5.9 ratification 2).
                // Pre-bind the name BEFORE lowering the body so the body's
                // identifier reference resolves to a
                // `ResolvedRef::Binding(let_id)` rather than `Unresolved`.
                // The capture-strength classifier in `lower_expr`'s
                // `Expr::SpawnLambdaActor` arm checks the resolved id
                // against this let's id to discriminate Weak (self) from
                // Strong (every other free-variable capture).
                //
                // The pre-bind fires for both untyped and typed lets. The
                // binding type follows the annotation when present (the
                // type-checker layer reconciles the annotation against the
                // lambda's synthesised Duplex shape) and falls back to the
                // synthetic `Duplex<Msg, Reply>` derived from the lambda's
                // parameter / return annotations otherwise.
                if let (
                    Pattern::Identifier(name),
                    Some((
                        Expr::SpawnLambdaActor {
                            params: lambda_params,
                            return_type,
                            ..
                        },
                        _,
                    )),
                ) = (&pattern.0, value.as_ref())
                {
                    let binding_ty = match ty.as_ref() {
                        Some(annotation) => self.lower_type(annotation),
                        None => self.actor_lambda_duplex_ty(lambda_params, return_type.as_ref()),
                    };
                    // Pre-bind in the current scope; record the id so
                    // we can detect a self-reference inside the body
                    // walk via builder state.
                    let pre_binding = self.bind(name.clone(), binding_ty, false, pattern.1.clone());
                    let prior = self
                        .current_actor_self
                        .replace((pre_binding.id, name.clone()));
                    let lowered_value = self.lower_expr(
                        value.as_ref().expect("value Some checked above"),
                        IntentKind::Consume,
                    );
                    self.current_actor_self = prior;
                    return HirStmt {
                        node: self.ids.node(),
                        kind: HirStmtKind::Let(pre_binding, Some(lowered_value)),
                        span,
                    };
                }
                let value = value
                    .as_ref()
                    .map(|expr| self.lower_expr(expr, IntentKind::Consume));
                let binding_ty = ty.as_ref().map_or_else(
                    || {
                        value
                            .as_ref()
                            .map_or(ResolvedTy::Unit, |expr| expr.ty.clone())
                    },
                    |ty| self.lower_type(ty),
                );
                let name = self
                    .pattern_name(pattern)
                    .unwrap_or_else(|| "_".to_string());
                let binding = self.bind(name, binding_ty, false, pattern.1.clone());
                HirStmtKind::Let(binding, value)
            }
            Stmt::Var { name, ty, value } => {
                let value = value
                    .as_ref()
                    .map(|expr| self.lower_expr(expr, IntentKind::Consume));
                let binding_ty = ty.as_ref().map_or_else(
                    || {
                        value
                            .as_ref()
                            .map_or(ResolvedTy::Unit, |expr| expr.ty.clone())
                    },
                    |ty| self.lower_type(ty),
                );
                let binding = self.bind(name.clone(), binding_ty, true, span.clone());
                HirStmtKind::Let(binding, value)
            }
            Stmt::Expression(expr) => {
                // Inside a scope{} body, statement-expression calls are child-task
                // spawns (TI-1). Outside scope{} bodies all calls are synchronous
                // (TI-3). The TI-1 rewrite only applies when the expression is a
                // direct call — nested calls inside sub-expressions remain sync.
                //
                // Mark this as statement position before lowering so that
                // `lower_expr`'s `Expr::Await` arm can enforce TI-4 (await is
                // only legal in statement-expression position, not as a
                // sub-expression). The flag is consumed by `mem::replace` at the
                // top of `lower_expr`, so recursive calls see `false`.
                self.statement_position = true;
                if self.scope_depth > 0 {
                    if let Expr::Call { .. } = &expr.0 {
                        let spawned = self.lower_spawned_call(expr);
                        HirStmtKind::Expr(spawned)
                    } else {
                        HirStmtKind::Expr(self.lower_expr(expr, IntentKind::Read))
                    }
                } else {
                    HirStmtKind::Expr(self.lower_expr(expr, IntentKind::Read))
                }
            }
            Stmt::Return(value) => {
                if let Some(value) = value {
                    let expr = self.lower_expr(value, IntentKind::Consume);
                    // TI-5 escape check: a `Task<T>` value must not escape via
                    // return, whether the type was user-written or inferred. The
                    // `lower_type` wall blocks user-written `Task<T>` annotations;
                    // this check closes the inferred-escape path.
                    if matches!(expr.ty, ResolvedTy::Task(_)) {
                        self.diagnostics.push(HirDiagnostic::new(
                            HirDiagnosticKind::TaskCannotEscape,
                            value.1.clone(),
                            "a `Task<T>` handle cannot escape via `return`; \
                             await it inside the `scope{}` body with `await name`",
                        ));
                    } else if expr.ty != return_ty && return_ty != ResolvedTy::Unit {
                        self.diagnostics.push(HirDiagnostic::new(
                            HirDiagnosticKind::ReturnTypeMismatch {
                                expected: return_ty,
                                actual: expr.ty.clone(),
                            },
                            value.1.clone(),
                            "return expression type differs from function return annotation",
                        ));
                    }
                    HirStmtKind::Return(Some(expr))
                } else {
                    HirStmtKind::Return(None)
                }
            }
            _ => {
                self.unsupported(span.clone(), "statement", "slice-2");
                HirStmtKind::Expr(self.unsupported_expr(span.clone(), "unsupported statement"))
            }
        };
        HirStmt {
            node: self.ids.node(),
            kind,
            span,
        }
    }

    #[allow(
        clippy::too_many_lines,
        reason = "single large match on expr variants; splitting would hurt readability"
    )]
    fn lower_expr(&mut self, expr: &Spanned<Expr>, intent: IntentKind) -> HirExpr {
        // Consume the statement-position flag atomically. Every recursive call
        // to `lower_expr` (for arguments, operands, return values, block tails,
        // etc.) therefore sees `false`. Only the `Stmt::Expression` arm in
        // `lower_stmt` sets this to `true` immediately before calling us.
        let in_stmt_position = std::mem::replace(&mut self.statement_position, false);
        let span = expr.1.clone();
        let (kind, ty) = match &expr.0 {
            Expr::Literal(lit) => Self::lower_literal(lit),
            Expr::Identifier(name) => self.lower_identifier(name, span.clone()),
            Expr::Binary { left, op, right } => {
                let left = self.lower_expr(left, IntentKind::Read);
                let right = self.lower_expr(right, IntentKind::Read);
                let ty = Self::binary_ty(*op, &left.ty, &right.ty);
                (
                    HirExprKind::Binary {
                        op: *op,
                        left: Box::new(left),
                        right: Box::new(right),
                    },
                    ty,
                )
            }
            Expr::Call { function, args, .. } => {
                let callee = self.lower_expr(function, IntentKind::Read);
                let args = args
                    .iter()
                    .map(|arg| self.lower_expr(arg.expr(), IntentKind::Read))
                    .collect();
                // Determine the call result type from the callee's function type.
                // If the callee is unresolved, the result type is an inference hole.
                let result_ty = if let ResolvedTy::Function { ret, .. } = &callee.ty {
                    *ret.clone()
                } else {
                    if matches!(
                        callee.kind,
                        HirExprKind::BindingRef {
                            resolved: ResolvedRef::Unresolved,
                            ..
                        }
                    ) {
                        self.diagnostics.push(HirDiagnostic::new(
                            HirDiagnosticKind::UnresolvedInferenceVar,
                            span.clone(),
                            "call result type cannot be determined: callee is unresolved",
                        ));
                    }
                    ResolvedTy::Unit
                };
                (
                    HirExprKind::Call {
                        callee: Box::new(callee),
                        args,
                    },
                    result_ty,
                )
            }
            Expr::Block(block) => {
                let block = self.lower_block(block, &ResolvedTy::Unit);
                let ty = block.ty.clone();
                (HirExprKind::Block(block), ty)
            }
            Expr::If {
                condition,
                then_block,
                else_block,
            } => {
                let condition = self.lower_expr(condition, IntentKind::Read);
                let then_expr = self.lower_expr(then_block, IntentKind::Read);
                let else_expr = else_block
                    .as_ref()
                    .map(|expr| Box::new(self.lower_expr(expr, IntentKind::Read)));
                let ty = else_expr
                    .as_ref()
                    .map_or(ResolvedTy::Unit, |expr| expr.ty.clone());
                (
                    HirExprKind::If {
                        condition: Box::new(condition),
                        then_expr: Box::new(then_expr),
                        else_expr,
                    },
                    ty,
                )
            }
            Expr::StructInit {
                name,
                fields,
                // The v0.5 vertical-slice HIR lowering does not yet honour
                // explicit type arguments at struct literal sites; the
                // slice rejects user types at the MIR boundary anyway, so
                // dropping the args here cannot widen an accepted program.
                type_args: _,
            } => {
                let fields = fields
                    .iter()
                    .map(|(name, expr)| (name.clone(), self.lower_expr(expr, IntentKind::Read)))
                    .collect();
                (
                    HirExprKind::StructInit {
                        name: name.clone(),
                        type_args: Vec::new(),
                        fields,
                    },
                    ResolvedTy::Named {
                        name: name.clone(),
                        args: Vec::new(),
                    },
                )
            }
            Expr::Scope { body } => {
                // A `scope{}` block lowers to `HirExprKind::Scope`. Inside the
                // body, statement-calls become spawned-call nodes (TI-1) and
                // `fork name = call(...)` statements introduce `Task<T>` bindings
                // (TI-2). The scope block's type is `Unit` — it is a lifetime
                // boundary, not a value-producing expression.
                self.scope_depth += 1;
                let hir_body = self.lower_scope_block(body);
                self.scope_depth -= 1;
                (HirExprKind::Scope { body: hir_body }, ResolvedTy::Unit)
            }
            Expr::ForkChild { binding, expr } => {
                // `fork name = expr` outside a `scope{}` body: no spawn context,
                // so this is malformed. Emit CutoverUnsupported — the grammar
                // accepts this form but HIR-lowering requires scope context.
                // (Inside scope{} bodies this variant is handled by lower_scope_block,
                // not by lower_expr directly.)
                if self.scope_depth == 0 {
                    self.diagnostics.push(HirDiagnostic::new(
                        HirDiagnosticKind::AwaitOutOfPosition,
                        span.clone(),
                        "`fork name = expr` is only valid inside a `scope{}` body",
                    ));
                    (
                        HirExprKind::Unsupported(
                            "`fork name = expr` outside scope body".to_string(),
                        ),
                        ResolvedTy::Unit,
                    )
                } else {
                    // Inside a scope body, lower_scope_block handles this case;
                    // reaching here means the expression appeared in a non-statement
                    // position (e.g. tail expression). Reject: task handles cannot
                    // be used as values.
                    let _ = binding;
                    let _ = expr;
                    self.diagnostics.push(HirDiagnostic::new(
                        HirDiagnosticKind::AwaitOutOfPosition,
                        span.clone(),
                        "`fork name = expr` must be a statement, not an expression value",
                    ));
                    (
                        HirExprKind::Unsupported("`fork name = expr` as expression".to_string()),
                        ResolvedTy::Unit,
                    )
                }
            }
            Expr::Await(inner) => {
                // `await expr` — only legal as the direct statement-expression
                // inside a `scope{}` body in v0.5 (TI-4). Sub-expression positions
                // (return value, function argument, binary operand, let value, block
                // tail, etc.) are rejected with `AwaitOutOfPosition`.
                // `in_stmt_position` is set by `Stmt::Expression` in `lower_stmt`
                // and consumed by `mem::replace` at the top of this function, so
                // recursive calls always see `false`.
                if self.scope_depth == 0 || !in_stmt_position {
                    self.diagnostics.push(HirDiagnostic::new(
                        HirDiagnosticKind::AwaitOutOfPosition,
                        span.clone(),
                        "`await` is only legal as a statement-expression inside a `scope{}` body \
                         in v0.5. It cannot be used as a return value, function argument, \
                         binary operand, or let-value. Move the await to its own statement.",
                    ));
                    return HirExpr {
                        node: self.ids.node(),
                        site: self.ids.site(),
                        value_class: ValueClass::BitCopy,
                        ty: ResolvedTy::Unit,
                        intent,
                        kind: HirExprKind::Unsupported("`await` out of position".to_string()),
                        span,
                    };
                }
                // Resolve the inner expression. It must be a binding-ref with a
                // `Task<T>` type to be awaitable.
                let inner_hir = self.lower_expr(inner, IntentKind::Consume);
                match &inner_hir.ty {
                    ResolvedTy::Task(output_ty) => {
                        let output_ty = *output_ty.clone();
                        // Extract the binding name and id for the AwaitTask node.
                        // The inner expression must be a direct binding-ref; await
                        // on a complex expression is not supported in v0.5.
                        if let HirExprKind::BindingRef {
                            name: binding_name,
                            resolved: ResolvedRef::Binding(binding_id),
                        } = &inner_hir.kind
                        {
                            let (binding_name, binding_id) = (binding_name.clone(), *binding_id);
                            let value_class = ValueClass::of_ty(&output_ty, &self.type_classes);
                            return HirExpr {
                                node: self.ids.node(),
                                site: self.ids.site(),
                                value_class,
                                ty: output_ty.clone(),
                                intent,
                                kind: HirExprKind::AwaitTask {
                                    binding_name,
                                    binding_id,
                                    output_ty,
                                },
                                span,
                            };
                        }
                        // Await on a non-binding-ref Task<T>: reject. The form
                        // `await (some_expr)` where the expr is not a name is not
                        // supported — only named bindings can be awaited.
                        self.diagnostics.push(HirDiagnostic::new(
                            HirDiagnosticKind::AwaitOutOfPosition,
                            span.clone(),
                            "`await` requires a named task binding, not an expression",
                        ));
                        (
                            HirExprKind::Unsupported("`await` on non-binding-ref task".to_string()),
                            ResolvedTy::Unit,
                        )
                    }
                    found_ty => {
                        // The operand is not a Task<T> — reject with AwaitNonTask.
                        let found_ty = found_ty.clone();
                        self.diagnostics.push(HirDiagnostic::new(
                            HirDiagnosticKind::AwaitNonTask {
                                found_ty: found_ty.clone(),
                            },
                            span.clone(),
                            "`await` requires a task handle (`Task<T>`). \
                             Hint: did you mean to bind a task with `fork name = call(...)` first?",
                        ));
                        (
                            HirExprKind::Unsupported("`await` on non-task".to_string()),
                            ResolvedTy::Unit,
                        )
                    }
                }
            }
            Expr::Select { arms, timeout } => {
                self.lower_select(arms, timeout.as_deref(), span.clone())
            }
            Expr::SpawnLambdaActor {
                params,
                return_type,
                body,
                ..
            } => self.lower_spawn_lambda_actor(params, return_type.as_ref(), body),
            Expr::MethodCall {
                receiver,
                method,
                args,
            } => self.lower_method_call(receiver, method, args, span.clone()),
            _ => {
                self.unsupported(span.clone(), "expression", "slice-2");
                (
                    HirExprKind::Unsupported("unsupported expression".into()),
                    ResolvedTy::Unit,
                )
            }
        };
        HirExpr {
            node: self.ids.node(),
            site: self.ids.site(),
            value_class: ValueClass::of_ty(&ty, &self.type_classes),
            ty,
            intent,
            kind,
            span,
        }
    }

    /// Lower a parsed `select { ... }` expression to HIR.
    ///
    /// Per HEW-SPEC-2026 §4.11.1 the four arm forms are exhaustive:
    ///   1. `pat from next(<stream-expr>) => body`
    ///   2. `pat from <actor-expr>.<method>(<args>) => body`   (actor ask)
    ///   3. `pat from await <task-expr> => body`
    ///   4. `after <duration-expr> => body`                    (timer)
    ///
    /// Any other arm source shape is rejected with
    /// `SelectArmNotSealedForm`. Body-type disagreement is rejected
    /// with `SelectArmTypeMismatch`. Empty selects and multiple-after
    /// arms are rejected with `SelectNoArms` and
    /// `SelectMultipleAfterArms` respectively.
    fn lower_select(
        &mut self,
        arms: &[SelectArm],
        timeout: Option<&TimeoutClause>,
        span: std::ops::Range<usize>,
    ) -> (HirExprKind, ResolvedTy) {
        // Empty select — neither arms nor a timer — fires nothing.
        if arms.is_empty() && timeout.is_none() {
            self.diagnostics.push(HirDiagnostic::new(
                HirDiagnosticKind::SelectNoArms,
                span.clone(),
                "select expression contains no arms",
            ));
            return (
                HirExprKind::Unsupported("empty select".into()),
                ResolvedTy::Unit,
            );
        }

        // Multiple `after` arms have no meaningful join semantics and are
        // rejected. An `Expr::Timeout`-sourced arm in the `arms` vec is
        // treated as an `AfterTimer` arm; if that is combined with the
        // dedicated `timeout` field (or if two appear in `arms`) the
        // second one triggers `SelectMultipleAfterArms`.

        let mut hir_arms: Vec<HirSelectArm> = Vec::with_capacity(arms.len() + 1);
        let mut expected_ty: Option<ResolvedTy> = None;
        let mut first_after_span: Option<std::ops::Range<usize>> = None;

        for arm in arms {
            let binding_name = self.pattern_name(&arm.binding);
            let kind = self.recognize_sealed_arm_source(&arm.source);
            if matches!(kind, HirSelectArmKind::AfterTimer { .. }) {
                if first_after_span.is_some() {
                    self.diagnostics.push(HirDiagnostic::new(
                        HirDiagnosticKind::SelectMultipleAfterArms,
                        arm.source.1.clone(),
                        "select may have at most one `after` arm",
                    ));
                } else {
                    first_after_span = Some(arm.source.1.clone());
                }
            }
            let body = self.lower_expr(&arm.body, IntentKind::Read);
            if let Some(expected) = expected_ty.as_ref() {
                if &body.ty != expected {
                    self.diagnostics.push(HirDiagnostic::new(
                        HirDiagnosticKind::SelectArmTypeMismatch {
                            arm_index: hir_arms.len(),
                            expected: expected.clone(),
                            actual: body.ty.clone(),
                        },
                        arm.body.1.clone(),
                        "select arm body type differs from the first arm body type",
                    ));
                }
            } else {
                expected_ty = Some(body.ty.clone());
            }
            hir_arms.push(HirSelectArm {
                kind,
                binding_name,
                body,
            });
        }

        if let Some(timeout) = timeout {
            if first_after_span.is_some() {
                self.diagnostics.push(HirDiagnostic::new(
                    HirDiagnosticKind::SelectMultipleAfterArms,
                    timeout.duration.1.clone(),
                    "select may have at most one `after` arm",
                ));
            }
            let duration = self.lower_expr(&timeout.duration, IntentKind::Read);
            let body = self.lower_expr(&timeout.body, IntentKind::Read);
            if let Some(expected) = expected_ty.as_ref() {
                if &body.ty != expected {
                    self.diagnostics.push(HirDiagnostic::new(
                        HirDiagnosticKind::SelectArmTypeMismatch {
                            arm_index: hir_arms.len(),
                            expected: expected.clone(),
                            actual: body.ty.clone(),
                        },
                        timeout.body.1.clone(),
                        "select after-arm body type differs from earlier arm body types",
                    ));
                }
            } else {
                expected_ty = Some(body.ty.clone());
            }
            hir_arms.push(HirSelectArm {
                kind: HirSelectArmKind::AfterTimer {
                    duration: Box::new(duration),
                },
                binding_name: None,
                body,
            });
        }

        let result_ty = expected_ty.unwrap_or(ResolvedTy::Unit);
        (HirExprKind::Select(HirSelect { arms: hir_arms }), result_ty)
    }

    /// Build the `Duplex<Msg, Reply>` `ResolvedTy` for an actor-lambda
    /// from its parameter list and optional return-type annotation.
    /// Mirrors the forward-bind logic in `hew-types::check::statements`:
    /// zero params → Unit message; one param → that param's type;
    /// multiple params → tuple of param types. The HIR layer only needs
    /// a placeholder shape so the forward-bind succeeds and capture
    /// resolution sees the let-name.
    ///
    /// SHIM — `ResolvedTy::Unit` for missing param / return annotations.
    ///
    /// - WHY: HIR has no type-inference machinery; the slice-2 type
    ///   checker uses `TypeVar::fresh()` to allocate inference variables
    ///   which HIR cannot represent. The forward-bind only needs a
    ///   syntactic placeholder so the body's recursive self-reference
    ///   resolves to a `BindingId`; the actual type identity is
    ///   reconstructed downstream.
    /// - WHEN OBSOLETE: either HIR gains a placeholder type variant
    ///   (an explicit `ResolvedTy::Hole` or equivalent) for forward-bind
    ///   sites, or the post-typecheck pipeline rewrites HIR binding
    ///   types from the slice-2 unifier's substitution table via a
    ///   side-table keyed on `BindingId`.
    /// - REAL SOLUTION: lift the slice-2 unifier's substitution into a
    ///   `BindingId → Ty` side-table emitted alongside HIR and consumed
    ///   by MIR lowering, so HIR never has to invent a stand-in.
    fn actor_lambda_duplex_ty(
        &mut self,
        params: &[LambdaParam],
        return_type: Option<&Spanned<TypeExpr>>,
    ) -> ResolvedTy {
        let param_tys: Vec<ResolvedTy> = params
            .iter()
            .map(|p| {
                p.ty.as_ref()
                    .map_or(ResolvedTy::Unit, |annotation| self.lower_type(annotation))
            })
            .collect();
        let msg_ty = match param_tys.len() {
            0 => ResolvedTy::Unit,
            1 => param_tys.into_iter().next().unwrap(),
            _ => ResolvedTy::Tuple(param_tys),
        };
        let reply_ty = return_type
            .as_ref()
            .map_or(ResolvedTy::Unit, |ann| self.lower_type(ann));
        ResolvedTy::Named {
            name: "Duplex".to_string(),
            args: vec![msg_ty, reply_ty],
        }
    }

    /// Lower an `Expr::SpawnLambdaActor` to an
    /// `HirExprKind::SpawnLambdaActor` with a resolved capture set.
    ///
    /// The lambda body lowers inside a fresh scope so the parameter
    /// bindings shadow outer names; after the body is built the
    /// `current_actor_self` field tells us whether the body lives
    /// under a `let <name> = actor |..| { .. }` forward-bind. The
    /// capture walker then collects every `BindingRef { resolved:
    /// Binding(id) }` whose `id` refers to a binding from an outer
    /// scope (not a parameter introduced by this lambda) and
    /// classifies the strength: `id == current_actor_self.0` → Weak
    /// (recursive self-dispatch, §5.9 ratification 2), else → Strong.
    ///
    /// The HIR `expr.ty` is the Duplex<Msg, Reply> handle type. The
    /// MIR producer wires this directly into a
    /// `Place::LambdaActorHandle` whose drop selects
    /// `DropKind::LambdaActorRelease`.
    fn lower_spawn_lambda_actor(
        &mut self,
        params: &[LambdaParam],
        return_type: Option<&Spanned<TypeExpr>>,
        body: &Spanned<Expr>,
    ) -> (HirExprKind, ResolvedTy) {
        let actor_ty = self.actor_lambda_duplex_ty(params, return_type);
        let reply_ty = match &actor_ty {
            ResolvedTy::Named { args, .. } if args.len() == 2 => args[1].clone(),
            _ => ResolvedTy::Unit,
        };
        // Lower params + body inside a new scope. Track the parameter
        // BindingIds so the capture walker can exclude them (params
        // are intra-lambda bindings, not captures from the enclosing
        // scope).
        self.push_scope();
        let mut hir_params: Vec<HirBinding> = Vec::with_capacity(params.len());
        let mut param_ids: std::collections::HashSet<BindingId> =
            std::collections::HashSet::with_capacity(params.len());
        for param in params {
            let ty = param
                .ty
                .as_ref()
                .map_or(ResolvedTy::Unit, |ann| self.lower_type(ann));
            let binding = self.bind(param.name.clone(), ty, false, 0..0);
            param_ids.insert(binding.id);
            hir_params.push(binding);
        }
        // Lexically scope `current_actor_self` to THIS lambda body. If the
        // caller (`lower_stmt`'s let-pre-bind path) set it before invoking
        // `lower_expr`, that value is this lambda's self-id; otherwise this
        // lambda is in expression position (anonymous) and has no self-id.
        // Take the value out for the duration of the body walk so any
        // nested actor-lambda lowered from within `body` does not inherit
        // it — nested anonymous lambdas would otherwise misclassify
        // captures of THIS lambda's enclosing-scope bindings as Weak.
        // Restored before `collect_lambda_captures` so the capture-strength
        // classifier sees the correct self-id, and restored to the caller's
        // prior value on exit.
        let my_self_id = self.current_actor_self.take();
        let lowered_body = self.lower_expr(body, IntentKind::Read);
        self.current_actor_self = my_self_id;
        self.pop_scope();
        let captures = self.collect_lambda_captures(&lowered_body, &param_ids);
        (
            HirExprKind::SpawnLambdaActor {
                params: hir_params,
                reply_ty,
                body: Box::new(lowered_body),
                captures,
            },
            actor_ty,
        )
    }

    /// Walk a lowered lambda body collecting `BindingRef`s that resolve
    /// to bindings from the enclosing scope. A reference is a capture
    /// when its resolved binding id is not in `param_ids` (the lambda's
    /// own parameters). Each unique binding is classified Weak when
    /// its id matches `current_actor_self.0` (the let-name pre-bound
    /// before body lowering) and Strong otherwise.
    ///
    /// Duplicate references to the same binding produce a single
    /// capture entry — codegen needs the runtime to register the
    /// captured handle once per binding, not once per use site.
    fn collect_lambda_captures(
        &self,
        body: &HirExpr,
        param_ids: &std::collections::HashSet<BindingId>,
    ) -> Vec<HirLambdaCapture> {
        let mut seen: std::collections::HashSet<BindingId> = std::collections::HashSet::new();
        let mut captures: Vec<HirLambdaCapture> = Vec::new();
        let self_id = self.current_actor_self.as_ref().map(|(id, _)| *id);
        collect_captures_walk(body, param_ids, &mut seen, &mut captures, self_id);
        captures
    }

    /// Recognise the sealed-form discriminator for a `select` arm
    /// source expression. Emits a `SelectArmNotSealedForm` /
    /// `SelectStreamNextSurface` / `SelectStreamNextArity` diagnostic
    /// on miss and returns a placeholder `AfterTimer` arm kind (the
    /// callers tolerate the placeholder because the diagnostic has
    /// already been emitted; MIR lowering treats any select with HIR
    /// diagnostics as fail-closed downstream).
    fn recognize_sealed_arm_source(&mut self, source: &Spanned<Expr>) -> HirSelectArmKind {
        let span = source.1.clone();
        match &source.0 {
            // Form 1: `next(<stream-expr>)` — a call where the callee
            // is the bare identifier `next`. `next` is not a lexer
            // keyword; the sealed-form discriminator is the callee
            // name.
            Expr::Call { function, args, .. } => {
                if let Expr::Identifier(name) = &function.0 {
                    if name == "next" {
                        if args.len() != 1 {
                            self.diagnostics.push(HirDiagnostic::new(
                                HirDiagnosticKind::SelectStreamNextArity {
                                    arg_count: args.len(),
                                },
                                span.clone(),
                                "next(<stream>) takes exactly one argument",
                            ));
                            return HirSelectArmKind::StreamNext {
                                stream: Box::new(
                                    self.unsupported_expr(span, "stream-next arity mismatch"),
                                ),
                            };
                        }
                        let stream = self.lower_expr(args[0].expr(), IntentKind::Read);
                        return HirSelectArmKind::StreamNext {
                            stream: Box::new(stream),
                        };
                    }
                }
                // Some other function call — not a sealed form.
                self.diagnostics.push(HirDiagnostic::new(
                    HirDiagnosticKind::SelectArmNotSealedForm {
                        source_shape: "function call".into(),
                    },
                    span.clone(),
                    "select arm source must be next(...), an actor method call, or `await <task>`",
                ));
                HirSelectArmKind::StreamNext {
                    stream: Box::new(self.unsupported_expr(span, "non-sealed arm source")),
                }
            }
            // Form 2: `<actor>.<method>(<args>)` — method call on an
            // actor expression. Per HEW-SPEC-2026 §4.11.1 this is the
            // actor-ask arm. `ask` is reserved as a future syntactic
            // marker (see HEW-FUTURE) but is not lexer-recognised in
            // edition 2026; the sealed-form discriminator is the
            // method-call surface.
            Expr::MethodCall {
                receiver,
                method,
                args,
            } => {
                let actor = self.lower_expr(receiver, IntentKind::Read);
                let lowered_args: Vec<HirExpr> = args
                    .iter()
                    .map(|arg| self.lower_expr(arg.expr(), IntentKind::Read))
                    .collect();
                HirSelectArmKind::ActorAsk {
                    actor: Box::new(actor),
                    method: method.clone(),
                    args: lowered_args,
                }
            }
            // Form 3: `await <task-expr>` — explicit await keyword.
            Expr::Await(task_expr) => {
                let task = self.lower_expr(task_expr, IntentKind::Read);
                HirSelectArmKind::TaskAwait {
                    task: Box::new(task),
                }
            }
            // Form 4 (arm-position): `after <duration>` written as an
            // arm source rather than the dedicated `timeout` field.
            // Recognised here so the `lower_select` multiple-after check
            // can fire; the duplicate check in `lower_select` emits the
            // diagnostic when this arm coexists with another after arm.
            Expr::Timeout { duration, .. } => {
                let dur = self.lower_expr(duration, IntentKind::Read);
                HirSelectArmKind::AfterTimer {
                    duration: Box::new(dur),
                }
            }
            // Method-call dressed up as `stream.next()` — sealed
            // surface is `next(stream)`. Diagnose specifically so the
            // user can fix the form.
            // (Already handled by the MethodCall arm above as a
            // generic actor-ask. The dedicated diagnostic for the
            // `.next()` shape would shadow the actor-ask recognition;
            // we keep the more general actor-ask interpretation and
            // rely on the SelectStreamNextSurface diagnostic only if
            // we later choose to special-case it. For now, `s.next()`
            // is recognised as an actor-ask of the `next` method on
            // `s`, which is the lower-noise default.)
            other => {
                let shape = describe_select_source_shape(other);
                self.diagnostics.push(HirDiagnostic::new(
                    HirDiagnosticKind::SelectArmNotSealedForm {
                        source_shape: shape,
                    },
                    span.clone(),
                    "select arm source must be next(...), an actor method call, or `await <task>`",
                ));
                HirSelectArmKind::StreamNext {
                    stream: Box::new(self.unsupported_expr(span, "non-sealed arm source")),
                }
            }
        }
    }

    fn lower_literal(lit: &Literal) -> (HirExprKind, ResolvedTy) {
        match lit {
            Literal::Integer { value, .. } => (
                HirExprKind::Literal(HirLiteral::Integer(*value)),
                ResolvedTy::I64,
            ),
            Literal::Float(value) => (
                HirExprKind::Literal(HirLiteral::Float(*value)),
                ResolvedTy::F64,
            ),
            Literal::String(value) => (
                HirExprKind::Literal(HirLiteral::String(value.clone())),
                ResolvedTy::String,
            ),
            Literal::Bool(value) => (
                HirExprKind::Literal(HirLiteral::Bool(*value)),
                ResolvedTy::Bool,
            ),
            Literal::Char(value) => (
                HirExprKind::Literal(HirLiteral::Char(*value)),
                ResolvedTy::Char,
            ),
            Literal::Duration(value) => (
                HirExprKind::Literal(HirLiteral::Duration(*value)),
                ResolvedTy::Duration,
            ),
        }
    }

    fn lower_identifier(
        &mut self,
        name: &str,
        span: std::ops::Range<usize>,
    ) -> (HirExprKind, ResolvedTy) {
        if let Some((id, ty)) = self.lookup(name) {
            (
                HirExprKind::BindingRef {
                    name: name.to_string(),
                    resolved: ResolvedRef::Binding(id),
                },
                ty,
            )
        } else if let Some(entry) = self.fn_registry.get(name) {
            // Known function item — expose as a function-typed reference so
            // callers can extract the return type from the call expression.
            let fn_ty = ResolvedTy::Function {
                params: entry.param_tys.clone(),
                ret: Box::new(entry.return_ty.clone()),
            };
            let id = entry.id;
            (
                HirExprKind::BindingRef {
                    name: name.to_string(),
                    resolved: ResolvedRef::Item(id),
                },
                fn_ty,
            )
        } else {
            self.diagnostics.push(HirDiagnostic::new(
                HirDiagnosticKind::UnresolvedSymbol {
                    name: name.to_string(),
                },
                span,
                "identifier has no binding in resolved HIR",
            ));
            (
                HirExprKind::BindingRef {
                    name: name.to_string(),
                    resolved: ResolvedRef::Unresolved,
                },
                ResolvedTy::Unit,
            )
        }
    }

    fn lower_type(&mut self, ty: &Spanned<TypeExpr>) -> ResolvedTy {
        match &ty.0 {
            TypeExpr::Named { name, type_args } => {
                let args = type_args
                    .as_ref()
                    .map(|args| args.iter().map(|arg| self.lower_type(arg)).collect())
                    .unwrap_or_default();
                match name.as_str() {
                    "i8" => ResolvedTy::I8,
                    "i16" => ResolvedTy::I16,
                    "i32" => ResolvedTy::I32,
                    // `int` is the user-facing alias for `i64` across the
                    // type checker (hew-types::stdlib_loader maps `int` to
                    // Ty::I64; ty.rs's alias table lists `int` as a synonym
                    // for `i64`). The HIR lowering must match — otherwise
                    // integer literals (which lower to I64) cannot be
                    // returned through a function typed `int` without an
                    // explicit cast. Aligns with hew-types ground truth.
                    "i64" | "int" => ResolvedTy::I64,
                    "u8" => ResolvedTy::U8,
                    "u16" => ResolvedTy::U16,
                    "u32" => ResolvedTy::U32,
                    "u64" => ResolvedTy::U64,
                    "f32" => ResolvedTy::F32,
                    "f64" | "float" => ResolvedTy::F64,
                    "bool" | "Bool" => ResolvedTy::Bool,
                    "char" | "Char" => ResolvedTy::Char,
                    "String" | "str" => ResolvedTy::String,
                    "Bytes" => ResolvedTy::Bytes,
                    "Duration" => ResolvedTy::Duration,
                    "Unit" | "()" => ResolvedTy::Unit,
                    // `Task` is a compiler-internal value class with no user-source
                    // syntax. Writing `Task<T>` in any annotation position is a
                    // compile error. Use `fork name = call(...)` to obtain a task
                    // handle. (TI-5 structural enforcement.)
                    "Task" => {
                        self.diagnostics.push(HirDiagnostic::new(
                            HirDiagnosticKind::TaskNotNameable,
                            ty.1.clone(),
                            "`Task<T>` is a compiler-internal type and cannot be written \
                             in source. Use `fork name = call(...)` to create a task handle.",
                        ));
                        ResolvedTy::Unit
                    }
                    _ => ResolvedTy::Named {
                        name: name.clone(),
                        args,
                    },
                }
            }
            TypeExpr::Infer => {
                self.diagnostics.push(HirDiagnostic::new(
                    HirDiagnosticKind::UnresolvedInferenceVar,
                    ty.1.clone(),
                    "inferred type reached resolved HIR boundary",
                ));
                ResolvedTy::Unit
            }
            TypeExpr::Tuple(elems) => {
                ResolvedTy::Tuple(elems.iter().map(|elem| self.lower_type(elem)).collect())
            }
            TypeExpr::Array { element, size } => {
                ResolvedTy::Array(Box::new(self.lower_type(element)), *size)
            }
            TypeExpr::Slice(elem) => ResolvedTy::Slice(Box::new(self.lower_type(elem))),
            TypeExpr::Function {
                params,
                return_type,
            } => ResolvedTy::Function {
                params: params.iter().map(|param| self.lower_type(param)).collect(),
                ret: Box::new(self.lower_type(return_type)),
            },
            TypeExpr::Pointer {
                is_mutable,
                pointee,
            } => ResolvedTy::Pointer {
                is_mutable: *is_mutable,
                pointee: Box::new(self.lower_type(pointee)),
            },
            _ => {
                self.unsupported(ty.1.clone(), "type-expression", "slice-2");
                ResolvedTy::Unit
            }
        }
    }

    fn binary_ty(op: BinaryOp, left: &ResolvedTy, right: &ResolvedTy) -> ResolvedTy {
        match op {
            BinaryOp::Equal
            | BinaryOp::NotEqual
            | BinaryOp::Less
            | BinaryOp::LessEqual
            | BinaryOp::Greater
            | BinaryOp::GreaterEqual
            | BinaryOp::And
            | BinaryOp::Or => ResolvedTy::Bool,
            BinaryOp::Add if left == &ResolvedTy::String || right == &ResolvedTy::String => {
                ResolvedTy::String
            }
            _ => left.clone(),
        }
    }

    /// Lower `receiver.method(args)` using the checker's `method_call_rewrites` side-table.
    ///
    /// Fail-closed per `checker-output-boundary` (LESSONS P0): a missing entry for
    /// this call site's span is a hard diagnostic — HIR never re-infers the runtime
    /// symbol from the receiver type.  Only `RewriteToFunction` is recognised here;
    /// other rewrite variants are rejected as unsupported (they target the C++/MLIR
    /// pipeline, not the Rust MIR pipeline).
    fn lower_method_call(
        &mut self,
        receiver: &Spanned<Expr>,
        method: &str,
        args: &[hew_parser::ast::CallArg],
        span: Span,
    ) -> (HirExprKind, ResolvedTy) {
        let key = SpanKey::from(&span);
        let rewrite = self.method_call_rewrites.get(&key).cloned();
        match rewrite {
            Some(MethodCallRewrite::RewriteToFunction { c_symbol }) => {
                // Lower receiver + args, then prepend receiver as first argument.
                let lowered_receiver = self.lower_expr(receiver, IntentKind::Read);
                let mut lowered_args = vec![lowered_receiver];
                for arg in args {
                    lowered_args.push(self.lower_expr(arg.expr(), IntentKind::Read));
                }
                // Synthetic callee: a runtime-symbol reference.  The function type
                // uses `Unit` return (runtime send/recv return unit in the Rust MIR
                // pipeline; future slices thread expr_types for richer return types).
                // `params` is empty — the call arg list carries the real args.
                let callee_ty = ResolvedTy::Function {
                    params: Vec::new(),
                    ret: Box::new(ResolvedTy::Unit),
                };
                let callee = HirExpr {
                    node: self.ids.node(),
                    site: self.ids.site(),
                    value_class: ValueClass::PersistentShare,
                    ty: callee_ty,
                    intent: IntentKind::Read,
                    kind: HirExprKind::BindingRef {
                        name: c_symbol,
                        resolved: ResolvedRef::Unresolved,
                    },
                    span: span.clone(),
                };
                (
                    HirExprKind::Call {
                        callee: Box::new(callee),
                        args: lowered_args,
                    },
                    ResolvedTy::Unit,
                )
            }
            Some(
                MethodCallRewrite::RewriteModuleQualifiedToFunction { .. }
                | MethodCallRewrite::DeferToLowering,
            ) => {
                // These rewrite variants target the C++/MLIR pipeline and are
                // not consumed by the Rust MIR pipeline.  Fail-closed.
                self.diagnostics.push(HirDiagnostic::new(
                    HirDiagnosticKind::CutoverUnsupported {
                        construct: format!("method-call rewrite variant for `.{method}`"),
                        slice_target: "mir-pipeline".to_string(),
                    },
                    span,
                    "this method-call rewrite variant is not supported in the Rust MIR pipeline",
                ));
                (
                    HirExprKind::Unsupported(format!(
                        "unsupported rewrite variant for method `{method}`"
                    )),
                    ResolvedTy::Unit,
                )
            }
            None => {
                // No rewrite entry — fail closed.  Do not re-infer from the receiver type.
                self.diagnostics.push(HirDiagnostic::new(
                    HirDiagnosticKind::MethodCallNoRewrite {
                        method: method.to_string(),
                    },
                    span,
                    "no checker-produced rewrite entry for this method call; \
                     typecheck must record a rewrite before HIR lowering",
                ));
                (
                    HirExprKind::Unsupported(format!(
                        "method call `.{method}` has no rewrite entry"
                    )),
                    ResolvedTy::Unit,
                )
            }
        }
    }

    fn pattern_name(&mut self, pattern: &Spanned<Pattern>) -> Option<String> {
        if let Pattern::Identifier(name) = &pattern.0 {
            Some(name.clone())
        } else {
            self.unsupported(pattern.1.clone(), "pattern", "slice-2");
            None
        }
    }

    fn bind(
        &mut self,
        name: String,
        ty: ResolvedTy,
        mutable: bool,
        span: std::ops::Range<usize>,
    ) -> HirBinding {
        let id = self.ids.binding();
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.clone(), (id, ty.clone()));
        }
        HirBinding {
            id,
            name,
            ty,
            mutable,
            span,
        }
    }

    fn lookup(&self, name: &str) -> Option<(BindingId, ResolvedTy)> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.get(name).map(|(id, ty)| (*id, ty.clone())))
    }

    fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn unsupported(
        &mut self,
        span: std::ops::Range<usize>,
        construct: impl Into<String>,
        slice_target: impl Into<String>,
    ) {
        self.diagnostics.push(HirDiagnostic::new(
            HirDiagnosticKind::CutoverUnsupported {
                construct: construct.into(),
                slice_target: slice_target.into(),
            },
            span,
            "",
        ));
    }

    fn unsupported_expr(
        &mut self,
        span: std::ops::Range<usize>,
        note: impl Into<String>,
    ) -> HirExpr {
        HirExpr {
            node: self.ids.node(),
            site: self.ids.site(),
            ty: ResolvedTy::Unit,
            value_class: ValueClass::BitCopy,
            intent: IntentKind::Unknown,
            kind: HirExprKind::Unsupported(note.into()),
            span,
        }
    }

    /// Lower the body block of a `scope{}` expression. This is separate from
    /// `lower_block` because statements inside a scope body follow different
    /// rules:
    ///
    /// - `Stmt::Expression(Expr::Call{..})` → `SpawnedCall` (TI-1)
    /// - `Stmt::Expression(Expr::ForkChild { binding: Some(name), expr })` →
    ///   `HirStmtKind::Let` with a `Task<T>` typed binding (TI-2)
    /// - `Stmt::Expression(Expr::Await(..))` → `AwaitTask` (TI-4)
    /// - All other statements lower normally, including nested `scope{}` blocks.
    ///
    /// The caller is responsible for setting `scope_depth` before calling this
    /// function and restoring it after.
    #[allow(
        clippy::too_many_lines,
        reason = "single match on scope-body statement variants; splitting would hurt readability"
    )]
    fn lower_scope_block(&mut self, block: &Block) -> HirBlock {
        self.push_scope();
        let scope = self.ids.scope();
        let mut statements = Vec::new();

        for (stmt, span) in &block.stmts {
            let hir_stmt = match stmt {
                // `fork name = call(...)` inside a scope body → TI-2: typed Task<T> binding.
                Stmt::Expression(expr)
                    if matches!(
                        &expr.0,
                        Expr::ForkChild {
                            binding: Some(_),
                            ..
                        }
                    ) =>
                {
                    if let Expr::ForkChild {
                        binding: Some(binding_name),
                        expr: child_expr,
                    } = &expr.0
                    {
                        // The child expression must be a call; any other form is rejected.
                        if matches!(&child_expr.0, Expr::Call { .. }) {
                            // Lower the call synchronously first to get the return type,
                            // then wrap in SpawnedCall + Task<T>.
                            let call_hir = self.lower_expr(child_expr, IntentKind::Consume);
                            let call_ret_ty = call_hir.ty.clone();
                            let task_ty = ResolvedTy::Task(Box::new(call_ret_ty));

                            // Destructure call_hir.kind once to extract callee + args.
                            let HirExprKind::Call { callee, args } = call_hir.kind else {
                                unreachable!("just verified Call shape above")
                            };

                            // Re-wrap as a SpawnedCall node with Task<T> type.
                            let spawned = HirExpr {
                                node: self.ids.node(),
                                site: self.ids.site(),
                                value_class: ValueClass::Linear, // Task handles are linear (consume-once).
                                ty: task_ty.clone(),
                                intent: IntentKind::Consume,
                                kind: HirExprKind::SpawnedCall {
                                    callee,
                                    args,
                                    task_ty: task_ty.clone(),
                                },
                                span: child_expr.1.clone(),
                            };

                            // Bind the name with Task<T> type in the current scope.
                            let binding =
                                self.bind(binding_name.clone(), task_ty, false, span.clone());
                            HirStmt {
                                node: self.ids.node(),
                                kind: HirStmtKind::Let(binding, Some(spawned)),
                                span: span.clone(),
                            }
                        } else {
                            self.diagnostics.push(HirDiagnostic::new(
                                HirDiagnosticKind::ForkChildNotACall,
                                child_expr.1.clone(),
                                "`fork name = expr` requires a call expression as the \
                                 right-hand side; other expression forms cannot be spawned as tasks",
                            ));
                            // Emit an unsupported stmt and continue rather than stopping.
                            self.unsupported(span.clone(), "fork-child-non-call", "slice-2");
                            HirStmt {
                                node: self.ids.node(),
                                kind: HirStmtKind::Expr(
                                    self.unsupported_expr(span.clone(), "fork child non-call"),
                                ),
                                span: span.clone(),
                            }
                        }
                    } else {
                        unreachable!("pattern guard ensures this branch")
                    }
                }

                // `fork name = call(...)` without a binding name (bare ForkChild with no
                // name, e.g. `scope { fork = expr }` — the grammar produces binding: None).
                // Lower the child expression as a plain SpawnedCall; the result is not bound.
                Stmt::Expression(expr)
                    if matches!(&expr.0, Expr::ForkChild { binding: None, .. }) =>
                {
                    if let Expr::ForkChild {
                        binding: None,
                        expr: child_expr,
                    } = &expr.0
                    {
                        if matches!(&child_expr.0, Expr::Call { .. }) {
                            let spawned = self.lower_spawned_call(child_expr);
                            HirStmt {
                                node: self.ids.node(),
                                kind: HirStmtKind::Expr(spawned),
                                span: span.clone(),
                            }
                        } else {
                            self.diagnostics.push(HirDiagnostic::new(
                                HirDiagnosticKind::ForkChildNotACall,
                                child_expr.1.clone(),
                                "`fork = expr` requires a call expression",
                            ));
                            self.unsupported(span.clone(), "fork-child-non-call", "slice-2");
                            HirStmt {
                                node: self.ids.node(),
                                kind: HirStmtKind::Expr(
                                    self.unsupported_expr(span.clone(), "fork child non-call"),
                                ),
                                span: span.clone(),
                            }
                        }
                    } else {
                        unreachable!("pattern guard ensures this branch")
                    }
                }

                // All other statements lower normally (including regular calls,
                // let bindings, nested scope{} blocks, etc.). Inside scope depth,
                // `lower_stmt` will already handle statement-expression calls as
                // SpawnedCall nodes via TI-1 (the scope_depth > 0 path in lower_stmt).
                _ => self.lower_stmt(stmt, span.clone(), ResolvedTy::Unit),
            };
            statements.push(hir_stmt);
        }

        let tail = block
            .trailing_expr
            .as_ref()
            .map(|expr| Box::new(self.lower_expr(expr, IntentKind::Read)));
        let ty = tail
            .as_ref()
            .map_or(ResolvedTy::Unit, |expr| expr.ty.clone());
        self.pop_scope();

        HirBlock {
            node: self.ids.node(),
            scope,
            statements,
            tail,
            ty,
            span: 0..0,
        }
    }

    /// Lower a call expression appearing as a statement inside a `scope{}` body
    /// as a child-task spawn (TI-1). The resulting `HirExpr` has kind
    /// `SpawnedCall` and type `Task<call_return_ty>`.
    fn lower_spawned_call(&mut self, expr: &Spanned<Expr>) -> HirExpr {
        let span = expr.1.clone();
        // Lower the call normally to resolve the callee and argument types.
        let call_hir = self.lower_expr(expr, IntentKind::Consume);
        let call_ret_ty = call_hir.ty.clone();
        let task_ty = ResolvedTy::Task(Box::new(call_ret_ty));

        let HirExprKind::Call { callee, args } = call_hir.kind else {
            // Should not happen: caller verified the expression is a Call.
            return self.unsupported_expr(span, "lower_spawned_call on non-call");
        };

        HirExpr {
            node: self.ids.node(),
            site: self.ids.site(),
            value_class: ValueClass::Linear, // Task handles are linear (consume-once).
            ty: task_ty.clone(),
            intent: IntentKind::Consume,
            kind: HirExprKind::SpawnedCall {
                callee,
                args,
                task_ty,
            },
            span,
        }
    }
}

// ── Lambda-actor capture walker ─────────────────────────────────────────────

/// Recursive walk over a lowered actor-lambda body collecting captures.
///
/// A capture is any `HirExprKind::BindingRef { resolved: Binding(id) }`
/// whose `id` is NOT one of the lambda's own parameter bindings. The
/// first occurrence of each capture is recorded with a strength
/// classifier — `Weak` iff the id matches the lambda's let-binding id
/// passed in `self_id` (the forward-bound recursive-self case, §5.9
/// ratification 2), `Strong` otherwise. Subsequent references to the
/// same binding are skipped — codegen wires one runtime capture per
/// binding, not one per use site.
///
/// The walk is exhaustive over `HirExprKind` variants; nested lambdas
/// (an actor lambda inside another actor lambda's body) are NOT
/// descended into — their captures belong to the inner lambda's
/// frame and are reported separately when that lambda's own
/// `lower_spawn_lambda_actor` call ran. The nested lambda's appearance
/// in the outer body's capture set, if any, would come from the
/// outer body referencing a name that the inner lambda also referenced;
/// but `BindingRef` lives only in the outer body's expression tree, so
/// this falls out naturally from not descending into the inner body.
fn collect_captures_walk(
    expr: &HirExpr,
    param_ids: &std::collections::HashSet<BindingId>,
    seen: &mut std::collections::HashSet<BindingId>,
    captures: &mut Vec<HirLambdaCapture>,
    self_id: Option<BindingId>,
) {
    match &expr.kind {
        HirExprKind::BindingRef {
            name,
            resolved: ResolvedRef::Binding(id),
        } => {
            // Parameters of the current lambda are intra-frame
            // bindings, never captures from the enclosing scope.
            if param_ids.contains(id) {
                return;
            }
            if !seen.insert(*id) {
                return;
            }
            let kind = if Some(*id) == self_id {
                HirCaptureKind::Weak
            } else {
                HirCaptureKind::Strong
            };
            captures.push(HirLambdaCapture {
                binding: *id,
                name: name.clone(),
                kind,
            });
        }
        // Empty-body terminals: nothing to walk.
        //   - BindingRef without a resolved binding (Item / Unresolved):
        //     does not produce a capture from the enclosing scope.
        //   - Literal: no sub-expressions.
        //   - SpawnLambdaActor (nested): its captures belong to its own
        //     frame and were classified when the inner lambda lowered.
        //   - Unsupported: nothing to walk.
        HirExprKind::BindingRef { .. }
        | HirExprKind::Literal(_)
        | HirExprKind::SpawnLambdaActor { .. }
        | HirExprKind::Unsupported(_) => {}
        HirExprKind::Binary { left, right, .. } => {
            collect_captures_walk(left, param_ids, seen, captures, self_id);
            collect_captures_walk(right, param_ids, seen, captures, self_id);
        }
        HirExprKind::Call { callee, args } | HirExprKind::SpawnedCall { callee, args, .. } => {
            collect_captures_walk(callee, param_ids, seen, captures, self_id);
            for arg in args {
                collect_captures_walk(arg, param_ids, seen, captures, self_id);
            }
        }
        HirExprKind::Block(block) | HirExprKind::Scope { body: block } => {
            collect_captures_walk_block(block, param_ids, seen, captures, self_id);
        }
        HirExprKind::If {
            condition,
            then_expr,
            else_expr,
        } => {
            collect_captures_walk(condition, param_ids, seen, captures, self_id);
            collect_captures_walk(then_expr, param_ids, seen, captures, self_id);
            if let Some(else_expr) = else_expr {
                collect_captures_walk(else_expr, param_ids, seen, captures, self_id);
            }
        }
        HirExprKind::StructInit { fields, .. } => {
            for (_, field) in fields {
                collect_captures_walk(field, param_ids, seen, captures, self_id);
            }
        }
        HirExprKind::AwaitTask { binding_id, .. } => {
            // The awaited task handle is captured from the enclosing
            // scope unless it is one of the lambda's own params.
            if param_ids.contains(binding_id) || !seen.insert(*binding_id) {
                return;
            }
            let kind = if Some(*binding_id) == self_id {
                HirCaptureKind::Weak
            } else {
                HirCaptureKind::Strong
            };
            captures.push(HirLambdaCapture {
                binding: *binding_id,
                // The await arm doesn't carry the binding's surface
                // name on its own — reach for the binding name via
                // the binding_name slot.
                name: String::new(),
                kind,
            });
        }
        HirExprKind::Select(select) => {
            for arm in &select.arms {
                match &arm.kind {
                    HirSelectArmKind::StreamNext { stream } => {
                        collect_captures_walk(stream, param_ids, seen, captures, self_id);
                    }
                    HirSelectArmKind::ActorAsk { actor, args, .. } => {
                        collect_captures_walk(actor, param_ids, seen, captures, self_id);
                        for arg in args {
                            collect_captures_walk(arg, param_ids, seen, captures, self_id);
                        }
                    }
                    HirSelectArmKind::TaskAwait { task } => {
                        collect_captures_walk(task, param_ids, seen, captures, self_id);
                    }
                    HirSelectArmKind::AfterTimer { duration } => {
                        collect_captures_walk(duration, param_ids, seen, captures, self_id);
                    }
                }
                collect_captures_walk(&arm.body, param_ids, seen, captures, self_id);
            }
        }
        HirExprKind::TupleIndex { tuple, .. } => {
            collect_captures_walk(tuple, param_ids, seen, captures, self_id);
        }
    }
}

fn collect_captures_walk_block(
    block: &HirBlock,
    param_ids: &std::collections::HashSet<BindingId>,
    seen: &mut std::collections::HashSet<BindingId>,
    captures: &mut Vec<HirLambdaCapture>,
    self_id: Option<BindingId>,
) {
    for stmt in &block.statements {
        match &stmt.kind {
            HirStmtKind::Let(_, Some(value)) => {
                collect_captures_walk(value, param_ids, seen, captures, self_id);
            }
            HirStmtKind::Expr(expr) | HirStmtKind::Return(Some(expr)) => {
                collect_captures_walk(expr, param_ids, seen, captures, self_id);
            }
            HirStmtKind::Let(_, None) | HirStmtKind::Return(None) => {}
        }
    }
    if let Some(tail) = &block.tail {
        collect_captures_walk(tail, param_ids, seen, captures, self_id);
    }
}

// ── Machine static-check helpers ────────────────────────────────────────────

/// Determine whether a self-transition body is "empty" for the `@reenter` rule.
///
/// A body is considered empty when:
/// - It is `Expr::Identifier(target_state)` — the no-body semicolon shorthand
///   that the parser synthesises for `on E: S -> S;`.
/// - It is `Expr::Block` with no statements and no trailing expression.
///
/// Any other form (statements, expressions) is non-empty and requires `@reenter`.
fn is_empty_self_body(body: &Expr, target_state: &str) -> bool {
    match body {
        Expr::Identifier(name) => name == target_state,
        Expr::Block(block) => block.stmts.is_empty() && block.trailing_expr.is_none(),
        _ => false,
    }
}

/// Shallow-scan a `Block` for field names appearing as the left-hand side of
/// an assignment statement (`self.field = ...`). Used for effect-parity checking
/// in entry blocks — the scan is intentionally shallow (depth = 1) since a
/// full walk would require type information we don't have in Lane A.
fn collect_assigned_field_names(block: &Block) -> Vec<(String, Span)> {
    let mut names = Vec::new();
    for (stmt, _) in &block.stmts {
        if let Stmt::Assign { target, .. } = stmt {
            if let Expr::FieldAccess { object, field } = &target.0 {
                if matches!(object.0, Expr::This) {
                    names.push((field.clone(), target.1.clone()));
                }
            }
        }
    }
    names
}

/// Shallow-scan an `Expr` (transition body) for `self.field = ...` assignments.
fn collect_assigned_field_names_expr(expr: &Expr) -> Vec<String> {
    if let Expr::Block(block) = expr {
        collect_assigned_field_names(block)
            .into_iter()
            .map(|(name, _)| name)
            .collect()
    } else {
        Vec::new()
    }
}

/// Collect event names directly emitted by `emit EventName` expressions within
/// an expression (transition body). Only direct emits are tracked; deeper nesting
/// is deferred to runtime (per the plan's "direct cycles only" rule).
fn collect_emitted_events(expr: &Expr) -> Vec<String> {
    let mut events = Vec::new();
    collect_emitted_events_inner(expr, &mut events);
    events
}

fn collect_emitted_events_inner(expr: &Expr, out: &mut Vec<String>) {
    match expr {
        Expr::MachineEmit { event_name, .. } => out.push(event_name.clone()),
        Expr::Block(block) => {
            for (stmt, _) in &block.stmts {
                if let Stmt::Expression((e, _)) = stmt {
                    collect_emitted_events_inner(e, out);
                }
            }
            if let Some(tail) = &block.trailing_expr {
                collect_emitted_events_inner(&tail.0, out);
            }
        }
        _ => {}
    }
}

/// One-token description of a parser `Expr` shape, used by
/// `SelectArmNotSealedForm` diagnostic notes. Intentionally coarse — the
/// goal is to tell the user "you wrote a literal where a sealed form
/// belongs", not to echo the expression back at them.
fn describe_select_source_shape(expr: &Expr) -> String {
    match expr {
        Expr::Literal(_) => "literal".into(),
        Expr::Identifier(_) => "identifier".into(),
        Expr::Binary { .. } => "binary expression".into(),
        Expr::Block(_) => "block".into(),
        Expr::If { .. } => "if expression".into(),
        Expr::Select { .. } => "nested select".into(),
        Expr::Join(_) => "join expression".into(),
        Expr::FieldAccess { .. } => "field access".into(),
        Expr::Index { .. } => "index expression".into(),
        Expr::Range { .. } => "range expression".into(),
        Expr::Cast { .. } => "cast expression".into(),
        Expr::Timeout { .. } => "timeout expression".into(),
        Expr::Unsafe(_) => "unsafe block".into(),
        Expr::Yield(_) => "yield expression".into(),
        Expr::This => "this".into(),
        _ => "expression".into(),
    }
}
