// The verify pass walks `HirExprKind` exhaustively, which includes the
// `#[deprecated]` `CallTraitMethodStatic` variant. The deprecation
// enforcement is structural (allowlist test on construction sites),
// not lint-driven.
#![allow(
    deprecated,
    reason = "legacy CallTraitMethodStatic variant is allowlist-gated; \
              see hew-hir/tests/call_trait_method_static_creation_allowlist.rs"
)]

use std::collections::{HashMap, HashSet};
use std::ops::Range;

use crate::diagnostic::{HirDiagnostic, HirDiagnosticKind};
use crate::ids::{BindingId, HirNodeId, ResolvedRef, SiteId};
use crate::node::{
    HirBlock, HirExpr, HirExprKind, HirGenCaptureSource, HirItem, HirLiteral, HirMatchArmPredicate,
    HirModule, HirStmtKind,
};
use hew_types::{BuiltinType, ResolvedTy};

#[must_use]
pub fn verify_hir(module: &HirModule) -> Vec<HirDiagnostic> {
    let mut verifier = Verifier::default();
    verifier.module(module);
    verifier.diagnostics
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirSiteSource {
    pub span: Range<usize>,
    pub source_module: Option<String>,
}

#[must_use]
pub fn collect_site_spans(module: &HirModule) -> HashMap<SiteId, HirSiteSource> {
    let mut verifier = Verifier::default();
    verifier.module(module);
    verifier.site_spans
}

#[derive(Debug, Default)]
struct Verifier {
    bindings: HashSet<BindingId>,
    sites: HashSet<SiteId>,
    nodes: HashSet<HirNodeId>,
    diagnostics: Vec<HirDiagnostic>,
    current_source_module: Option<String>,
    site_spans: HashMap<SiteId, HirSiteSource>,
}

impl Verifier {
    fn module(&mut self, module: &HirModule) {
        for item in &module.items {
            self.current_source_module = Self::item_source_module(module, item);
            match item {
                HirItem::Function(func) => {
                    self.node(func.node, func.span.clone());
                    for param in &func.params {
                        self.binding(param.id, param.span.clone());
                    }
                    self.block(&func.body);
                }
                HirItem::TypeDecl(decl) => {
                    // Type declarations contribute only their HirNodeId
                    // uniqueness to the verifier — they carry no bindings,
                    // sites, or expressions to validate. The marker /
                    // consuming-method validations fire upstream in
                    // `lower_type_decl`.
                    self.node(decl.node, decl.span.clone());
                }
                HirItem::Machine(machine) => {
                    // Machine declarations are structurally verified here.
                    // Body expressions are not lowered to HirExpr in Lane A,
                    // so there are no binding or site IDs to verify beyond
                    // the machine's own node ID.
                    self.node(machine.node, machine.span.clone());
                }
                HirItem::Record(record) => {
                    // Record declarations contribute only their HirNodeId
                    // uniqueness to the verifier — they carry no bindings,
                    // sites, or expressions to validate. The @linear-field
                    // guard fires upstream in `lower_record_decl`.
                    self.node(record.node, record.span.clone());
                }
                HirItem::Actor(actor) => {
                    self.node(actor.node, actor.span.clone());
                    if let Some(init) = &actor.init {
                        for param in &init.params {
                            self.binding(param.id, param.span.clone());
                        }
                        self.block(&init.body);
                    }
                    for receive in &actor.receive_handlers {
                        for param in &receive.params {
                            self.binding(param.id, param.span.clone());
                        }
                        self.block(&receive.body);
                    }
                    for method in &actor.methods {
                        for param in &method.params {
                            self.binding(param.id, param.span.clone());
                        }
                        self.block(&method.body);
                    }
                    for hook in &actor.lifecycle_hooks {
                        for param in &hook.params {
                            self.binding(param.id, param.span.clone());
                        }
                        self.block(&hook.body);
                    }
                }
                HirItem::Supervisor(sup) => {
                    // Supervisor declarations contribute only their HirNodeId
                    // uniqueness in S-A; children-list resolution and
                    // wired_to validation are S-B's job.
                    self.node(sup.node, sup.span.clone());
                }
                HirItem::Impl(block) => {
                    // V0b: impl-block metadata only contributes its own
                    // HirNodeId. The per-method bodies are emitted as
                    // sibling `HirItem::Function` entries and are walked
                    // through the `Function` arm above, so no recursion
                    // into `block.method_symbols` is needed here.
                    self.node(block.node, block.span.clone());
                }
                HirItem::ExternFn(ef) => {
                    // Extern fns have no body, no parameter bindings — only
                    // their own HirNodeId contributes to uniqueness. The
                    // signature is verified by the checker before lowering.
                    self.node(ef.node, ef.span.clone());
                }
                HirItem::Const(c) => {
                    // Const declarations contribute only their HirNodeId
                    // uniqueness — the initializer was constant-folded into a
                    // value at lowering time, so there are no bindings, sites,
                    // or expressions to verify.
                    self.node(c.node, c.span.clone());
                }
            }
            self.current_source_module = None;
        }
    }

    fn diagnostic(
        &self,
        kind: HirDiagnosticKind,
        span: std::ops::Range<usize>,
        note: impl Into<String>,
    ) -> HirDiagnostic {
        HirDiagnostic::new(kind, span, note).with_source_module(self.current_source_module.clone())
    }

    fn block(&mut self, block: &HirBlock) {
        self.node(block.node, 0..0);
        for stmt in &block.statements {
            self.node(stmt.node, stmt.span.clone());
            match &stmt.kind {
                HirStmtKind::Let(binding, value) => {
                    self.binding(binding.id, binding.span.clone());
                    if let Some(value) = value {
                        self.expr(value);
                    }
                }
                HirStmtKind::Assign { target, value } => {
                    self.expr(target);
                    self.expr(value);
                }
                HirStmtKind::Expr(expr) | HirStmtKind::Return(Some(expr)) => self.expr(expr),
                HirStmtKind::Return(None) => {}
                HirStmtKind::Defer { body, .. } => self.expr(body),
                HirStmtKind::LetElse {
                    scrutinee,
                    bindings,
                    success_prelude,
                    else_body,
                    ..
                } => {
                    self.expr(scrutinee);
                    // The Ok-path bindings escape into the enclosing scope —
                    // register them here so later references resolve.
                    for binding in bindings {
                        self.binding(binding.binding, scrutinee.span.clone());
                    }
                    // Aggregate payload destructure (e.g. `Ok((n, s))`): the
                    // prelude's `Let` statements introduce the leaf binders
                    // (`n`, `s`) that also escape into the enclosing scope.
                    // Register them and verify their projection values so a
                    // later reference resolves and is not flagged unresolved.
                    for prelude_stmt in success_prelude {
                        if let HirStmtKind::Let(binding, value) = &prelude_stmt.kind {
                            self.binding(binding.id, binding.span.clone());
                            if let Some(value) = value {
                                self.expr(value);
                            }
                        }
                    }
                    self.block(else_body);
                }
            }
        }
        if let Some(tail) = &block.tail {
            self.expr(tail);
        }
    }

    #[expect(
        clippy::too_many_lines,
        reason = "exhaustive match on all HirExprKind variants; splitting would \
                  obscure the exhaustiveness requirement and scatter the fail-closed \
                  Unsupported arm away from the variants it guards"
    )]
    fn expr(&mut self, expr: &HirExpr) {
        self.node(expr.node, expr.span.clone());
        self.site(expr.site, expr.span.clone());
        match &expr.kind {
            HirExprKind::BindingRef { resolved, name } => {
                if *resolved == ResolvedRef::Unresolved {
                    self.diagnostics.push(self.diagnostic(
                        HirDiagnosticKind::UnresolvedSymbol { name: name.clone() },
                        expr.span.clone(),
                        "resolved HIR contains an unresolved binding reference",
                    ));
                }
            }
            HirExprKind::Binary { left, right, .. }
            | HirExprKind::IdentityCompare { left, right } => {
                self.expr(left);
                self.expr(right);
            }
            HirExprKind::Unary { operand, .. } | HirExprKind::WireCodec { operand, .. } => {
                self.expr(operand);
            }
            HirExprKind::ConnAwaitRead { conn, .. } => self.expr(conn),
            HirExprKind::AwaitRestart { child } => self.expr(child),
            HirExprKind::ListenerAwaitAccept { listener, .. } => self.expr(listener),
            HirExprKind::ChannelRecvAwait { receiver, .. } => self.expr(receiver),
            HirExprKind::StreamRecvAwait { stream, .. } => self.expr(stream),
            HirExprKind::NumericCast {
                value,
                from_ty,
                to_ty,
            }
            | HirExprKind::SaturatingWidthCast {
                value,
                from_ty,
                to_ty,
            }
            | HirExprKind::TryWidthCast {
                value,
                from_ty,
                to_ty,
                ..
            } => {
                let node_name = match &expr.kind {
                    HirExprKind::SaturatingWidthCast { .. } => "saturating width cast",
                    HirExprKind::TryWidthCast { .. } => "try width cast",
                    _ => "numeric cast",
                };
                self.expr(value);
                if value.ty != *from_ty {
                    self.diagnostics.push(self.diagnostic(
                        HirDiagnosticKind::CheckerBoundaryViolation {
                            name: node_name.to_string(),
                            reason: format!(
                                "cast source metadata {} disagrees with value type {}",
                                from_ty.user_facing(),
                                value.ty.user_facing()
                            ),
                        },
                        expr.span.clone(),
                        "cast source type metadata must match the lowered operand",
                    ));
                }
                let expected_expr_ty = if matches!(expr.kind, HirExprKind::TryWidthCast { .. }) {
                    ResolvedTy::Named {
                        name: "Option".to_string(),
                        args: vec![to_ty.clone()],
                        builtin: Some(BuiltinType::Option),
                        is_opaque: false,
                    }
                } else {
                    to_ty.clone()
                };
                if expr.ty != expected_expr_ty {
                    self.diagnostics.push(self.diagnostic(
                        HirDiagnosticKind::CheckerBoundaryViolation {
                            name: node_name.to_string(),
                            reason: format!(
                                "cast result metadata {} disagrees with expression type {}",
                                expected_expr_ty.user_facing(),
                                expr.ty.user_facing()
                            ),
                        },
                        expr.span.clone(),
                        "cast result type metadata must match the expression type",
                    ));
                }
                if !from_ty.can_explicitly_numeric_cast_to(to_ty) {
                    self.diagnostics.push(self.diagnostic(
                        HirDiagnosticKind::CheckerBoundaryViolation {
                            name: node_name.to_string(),
                            reason: format!(
                                "cast from {} to {} is outside the checker-admitted numeric matrix",
                                from_ty.user_facing(),
                                to_ty.user_facing()
                            ),
                        },
                        expr.span.clone(),
                        "cast HIR node carries a non-numeric cast",
                    ));
                }
            }
            HirExprKind::TupleLiteral { elements } => {
                // Arity check: expr.ty must be ResolvedTy::Tuple with width
                // matching elements.len(). Checker-authoritative invariant: the
                // lowering pass reads tuple types from TypeCheckOutput.expr_types,
                // never re-derives them. A mismatch here surfaces a checker/HIR
                // boundary violation (poison in the side-table), not a user error.
                match &expr.ty {
                    ResolvedTy::Tuple(fields) => {
                        if fields.len() != elements.len() {
                            self.diagnostics.push(self.diagnostic(
                                HirDiagnosticKind::CheckerBoundaryViolation {
                                    name: "tuple literal".to_string(),
                                    reason: format!(
                                        "tuple type has arity {} but literal has {} elements",
                                        fields.len(),
                                        elements.len()
                                    ),
                                },
                                expr.span.clone(),
                                "tuple literal element count does not match declared type width",
                            ));
                        }
                    }
                    // A zero-element tuple literal `()` whose resolved type is
                    // `ResolvedTy::Unit` is structurally valid: the checker
                    // accepts `Ok(())` and `Result<(),E>` returns (fixture
                    // `result_constructors_accept_unit_payloads` passes), but
                    // `()` resolves to `Unit` rather than `Tuple([])`, so it
                    // must be admitted here rather than rejected as a
                    // CheckerBoundaryViolation. Non-empty literals with a
                    // non-tuple type still fail closed below.
                    ResolvedTy::Unit if elements.is_empty() => {}
                    other => {
                        self.diagnostics.push(self.diagnostic(
                            HirDiagnosticKind::CheckerBoundaryViolation {
                                name: "tuple literal".to_string(),
                                reason: format!("expected tuple type, got {}", other.user_facing()),
                            },
                            expr.span.clone(),
                            "tuple literal must have tuple type",
                        ));
                    }
                }
                for elem in elements {
                    self.expr(elem);
                }
            }
            HirExprKind::Call { callee, args } | HirExprKind::SpawnedCall { callee, args, .. } => {
                self.expr(callee);
                for arg in args {
                    self.expr(arg);
                }
            }
            HirExprKind::Spawn { args, .. } => {
                for (_, arg) in args {
                    self.expr(arg);
                }
            }
            HirExprKind::ActorSend { receiver, args, .. }
            | HirExprKind::ActorAsk { receiver, args, .. }
            | HirExprKind::CallDynMethod { receiver, args, .. }
            | HirExprKind::ResolvedImplCall { receiver, args, .. }
            | HirExprKind::CallTraitMethodStatic { receiver, args, .. }
            | HirExprKind::VarSelfMethodCall { receiver, args, .. } => {
                self.expr(receiver);
                for arg in args {
                    self.expr(arg);
                }
            }
            HirExprKind::RemoteActorAsk {
                receiver,
                msg,
                timeout_ms,
                ..
            } => {
                self.expr(receiver);
                self.expr(msg);
                self.expr(timeout_ms);
            }
            HirExprKind::NumericMethod { receiver, arg, .. } => {
                self.expr(receiver);
                self.expr(arg);
            }
            HirExprKind::Block(block) => self.block(block),
            HirExprKind::If {
                condition,
                then_expr,
                else_expr,
            } => {
                self.expr(condition);
                self.expr(then_expr);
                if let Some(else_expr) = else_expr {
                    self.expr(else_expr);
                }
            }
            HirExprKind::StructInit { fields, base, .. } => {
                for (_, field) in fields {
                    self.expr(field);
                }
                if let Some(base) = base {
                    self.expr(base);
                }
            }
            HirExprKind::FieldAccess { object, .. } => {
                self.expr(object);
            }
            HirExprKind::ContextReader { .. }
            | HirExprKind::Literal(_)
            | HirExprKind::RegexLiteralRef { .. }
            | HirExprKind::MachineFieldAccess { .. }
            | HirExprKind::Continue { .. }
            | HirExprKind::ActorSelf
            | HirExprKind::MachineEventFieldAccess { .. } => {}
            HirExprKind::Scope { body }
            | HirExprKind::ForkBlock { body, .. }
            | HirExprKind::Loop { body, .. } => self.block(body),
            HirExprKind::ScopeDeadline { duration, body } => {
                self.expr(duration);
                self.block(body);
            }
            HirExprKind::AwaitTask { binding_id, .. } => {
                // Verify the binding-id referenced by the await is known to the verifier.
                // If it's not in `self.bindings`, that indicates a dangling reference.
                if !self.bindings.contains(binding_id) {
                    self.diagnostics.push(self.diagnostic(
                        HirDiagnosticKind::DanglingRef {
                            resolved: ResolvedRef::Binding(*binding_id),
                        },
                        expr.span.clone(),
                        "await-task references a binding that was not declared in resolved HIR",
                    ));
                }
            }
            HirExprKind::Select(select) => {
                for arm in &select.arms {
                    match &arm.kind {
                        crate::node::HirSelectArmKind::StreamNext { stream } => {
                            self.expr(stream);
                        }
                        crate::node::HirSelectArmKind::ActorAsk { actor, args, .. } => {
                            self.expr(actor);
                            for arg in args {
                                self.expr(arg);
                            }
                        }
                        crate::node::HirSelectArmKind::TaskAwait { task } => {
                            self.expr(task);
                        }
                        crate::node::HirSelectArmKind::ChannelRecv { receiver, .. } => {
                            self.expr(receiver);
                        }
                        crate::node::HirSelectArmKind::AfterTimer { duration } => {
                            self.expr(duration);
                        }
                    }
                    self.expr(&arm.body);
                }
            }
            HirExprKind::Join(join) => {
                for branch in &join.branches {
                    self.expr(&branch.actor);
                    for arg in &branch.args {
                        self.expr(arg);
                    }
                }
            }
            HirExprKind::SpawnLambdaActor { body, captures, .. } => {
                // The lambda body is a child expression — recurse for
                // node/site/diagnostic coverage. The capture set is
                // metadata produced by the resolver; verify each
                // captured binding id was declared somewhere in the
                // HIR (catches a resolver bug that records a freed
                // binding id).
                self.expr(body);
                for capture in captures {
                    if !self.bindings.contains(&capture.binding) {
                        self.diagnostics.push(self.diagnostic(
                            HirDiagnosticKind::DanglingRef {
                                resolved: ResolvedRef::Binding(capture.binding),
                            },
                            expr.span.clone(),
                            "lambda-actor capture references a binding not declared in resolved HIR",
                        ));
                    }
                }
            }
            HirExprKind::Closure {
                params,
                body,
                captures,
                ..
            } => {
                // Register the closure's own parameters BEFORE walking the
                // body: a nested closure in the body may capture one of these
                // params (`|x| { |y| x + y }`), and the capture-declared check
                // below resolves `capture.binding` against `self.bindings`.
                // Without this the inner capture of an outer-closure param is
                // a spurious DanglingRef — exactly the path actor methods and
                // named-fn bodies already register (see the actor-method and
                // Function arms). Mirrors that registration here.
                for param in params {
                    self.binding(param.id, param.span.clone());
                }
                self.expr(body);
                let mut seen = std::collections::HashSet::new();
                for capture in captures {
                    if !self.bindings.contains(&capture.binding) {
                        self.diagnostics.push(self.diagnostic(
                            HirDiagnosticKind::DanglingRef {
                                resolved: ResolvedRef::Binding(capture.binding),
                            },
                            expr.span.clone(),
                            "closure capture references a binding not declared in resolved HIR",
                        ));
                    }
                    if !seen.insert(capture.binding) {
                        self.diagnostics.push(self.diagnostic(
                            HirDiagnosticKind::DuplicateBindingId {
                                id: capture.binding,
                            },
                            expr.span.clone(),
                            "closure capture list contains the same binding more than once",
                        ));
                    }
                }
            }
            HirExprKind::GenBlock {
                body,
                yield_ty,
                return_ty,
                captures,
            } => {
                // Each gen capture must reference a binding that exists in the
                // resolved HIR, and no binding may appear twice (the env field
                // layout is keyed by binding — duplicates would collide). Mirrors
                // the closure/lambda capture verification above.
                //
                // The declared-binding requirement applies only to `Local`
                // captures (a `gen fn`'s params, a `gen {}` block's outer
                // locals, or a `receive gen fn` handler param) — those resolve
                // to a real `HirBinding` declaration and a dangling one is a
                // resolver bug. An `ActorStateField` capture is intentionally
                // synthetic: `lower_actor_generator_body` mints its binding id
                // while binding the actor's state fields into scope, and no
                // `HirBinding` declaration node carries that id, so it will
                // never be in `self.bindings`. Trust the HIR-authority source
                // tag (`type-info-survival` — do not re-derive) and exempt it
                // from the DanglingRef gate. The duplicate-id guard still
                // covers ALL captures (the env layout keys on binding id
                // regardless of source).
                let mut seen_captures = std::collections::HashSet::new();
                for capture in captures {
                    let checked_local = matches!(capture.source, HirGenCaptureSource::Local);
                    if checked_local && !self.bindings.contains(&capture.binding) {
                        self.diagnostics.push(self.diagnostic(
                            HirDiagnosticKind::DanglingRef {
                                resolved: ResolvedRef::Binding(capture.binding),
                            },
                            expr.span.clone(),
                            "generator capture references a binding not declared in resolved HIR",
                        ));
                    }
                    if !seen_captures.insert(capture.binding) {
                        self.diagnostics.push(self.diagnostic(
                            HirDiagnosticKind::DuplicateBindingId {
                                id: capture.binding,
                            },
                            expr.span.clone(),
                            "generator capture list contains the same binding more than once",
                        ));
                    }
                }
                match &expr.ty {
                    ResolvedTy::Named { name, args, .. }
                        if name == "Generator" && args.len() == 2 =>
                    {
                        if args[0] != *yield_ty || args[1] != *return_ty {
                            self.diagnostics.push(self.diagnostic(
                                HirDiagnosticKind::CheckerBoundaryViolation {
                                    name: "gen block".to_string(),
                                    reason: format!(
                                        "GenBlock carries Yield={}, Return={} but expr type is {}",
                                        yield_ty.user_facing(),
                                        return_ty.user_facing(),
                                        expr.ty.user_facing()
                                    ),
                                },
                                expr.span.clone(),
                                "gen block HIR metadata disagrees with its expression type",
                            ));
                        }
                    }
                    other => {
                        self.diagnostics.push(self.diagnostic(
                            HirDiagnosticKind::CheckerBoundaryViolation {
                                name: "gen block".to_string(),
                                reason: format!(
                                    "expected Generator<Yield, Return>, got {}",
                                    other.user_facing()
                                ),
                            },
                            expr.span.clone(),
                            "gen block HIR expression does not have Generator type",
                        ));
                    }
                }
                self.block(body);
            }
            HirExprKind::Yield { value, yield_ty } => {
                if expr.ty != ResolvedTy::Unit {
                    self.diagnostics.push(self.diagnostic(
                        HirDiagnosticKind::CheckerBoundaryViolation {
                            name: "yield".to_string(),
                            reason: format!(
                                "yield expression has non-unit result type {}",
                                expr.ty.user_facing()
                            ),
                        },
                        expr.span.clone(),
                        "yield HIR expression result type must be unit",
                    ));
                }
                if let Some(value) = value {
                    self.expr(value);
                    if value.ty != *yield_ty {
                        self.diagnostics.push(self.diagnostic(
                            HirDiagnosticKind::CheckerBoundaryViolation {
                                name: "yield".to_string(),
                                reason: format!(
                                    "yield value type {} disagrees with enclosing Yield {}",
                                    value.ty.user_facing(),
                                    yield_ty.user_facing()
                                ),
                            },
                            value.span.clone(),
                            "yield value type does not match enclosing generator Yield type",
                        ));
                    }
                }
            }
            HirExprKind::TupleIndex { tuple, .. } => {
                self.expr(tuple);
            }
            HirExprKind::Index { container, index } => {
                self.expr(container);
                self.expr(index);
            }
            HirExprKind::Slice {
                container,
                start,
                end,
                inclusive: _,
            } => {
                self.expr(container);
                if let Some(s) = start {
                    self.expr(s);
                }
                if let Some(e) = end {
                    self.expr(e);
                }
            }
            HirExprKind::CoerceToDynTrait { value, .. } => {
                self.expr(value);
            }
            HirExprKind::MachineEmit { fields, .. } => {
                for (_, field_val) in fields {
                    self.expr(field_val);
                }
            }
            HirExprKind::MachineStep {
                receiver, event, ..
            }
            | HirExprKind::MachineTakeEmits {
                receiver, event, ..
            } => {
                self.expr(receiver);
                self.expr(event);
            }
            HirExprKind::CancellationTokenIsCancelled { receiver }
            | HirExprKind::GeneratorNext { receiver, .. }
            | HirExprKind::MachineStateName { receiver, .. }
            | HirExprKind::RecordCloneCall { src: receiver, .. } => {
                self.expr(receiver);
            }
            HirExprKind::MachineVariantCtor { payload, .. } => {
                if let Some(fields) = payload {
                    for (_, val) in fields {
                        self.expr(val);
                    }
                }
            }
            HirExprKind::While {
                condition, body, ..
            } => {
                self.expr(condition);
                self.block(body);
            }
            HirExprKind::Match { scrutinee, arms } => {
                self.expr(scrutinee);
                for arm in arms {
                    match &arm.predicate {
                        HirMatchArmPredicate::Literal { lit, ty } => {
                            self.match_literal_predicate(lit, ty, arm.span.clone());
                        }
                        HirMatchArmPredicate::RecordProject { ty } => {
                            self.match_record_project_predicate(
                                ty,
                                &scrutinee.ty,
                                arm.span.clone(),
                            );
                        }
                        HirMatchArmPredicate::TupleProject { arity } => {
                            self.match_tuple_project_predicate(
                                *arity,
                                &scrutinee.ty,
                                &arm.bindings,
                                arm.span.clone(),
                            );
                        }
                        HirMatchArmPredicate::Wildcard
                        | HirMatchArmPredicate::Binding { .. }
                        | HirMatchArmPredicate::EnumVariant { .. }
                        | HirMatchArmPredicate::Regex { .. } => {}
                    }
                    for binding in &arm.bindings {
                        self.binding(binding.binding, arm.span.clone());
                    }
                    if let Some(guard) = &arm.guard {
                        self.expr(guard);
                    }
                    self.expr(&arm.body);
                }
            }
            HirExprKind::ForRange {
                binding,
                start,
                end,
                step,
                body,
                ..
            } => {
                self.binding(binding.id, binding.span.clone());
                self.expr(start);
                self.expr(end);
                self.expr(step);
                self.block(body);
            }
            HirExprKind::WhileLet {
                scrutinee,
                bindings,
                body,
                ..
            } => {
                self.expr(scrutinee);
                for binding in bindings {
                    // While-let payload bindings are scoped to the body
                    // (one fresh BindingId allocated by HIR lowering);
                    // register them here so the duplicate-binding check
                    // covers the new shape, mirroring `Match` arm bindings.
                    self.binding(binding.binding, expr.span.clone());
                }
                self.block(body);
            }
            HirExprKind::IfLet {
                scrutinee,
                bindings,
                body,
                else_body,
                ..
            } => {
                self.expr(scrutinee);
                for binding in bindings {
                    // If-let payload bindings are scoped to the then-body;
                    // register them here mirroring `WhileLet` and `Match`.
                    self.binding(binding.binding, expr.span.clone());
                }
                self.block(body);
                if let Some(eb) = else_body {
                    self.block(eb);
                }
            }
            HirExprKind::Break { value, .. } | HirExprKind::Return { value } => {
                if let Some(value) = value {
                    self.expr(value);
                }
            }
            HirExprKind::Unsupported(reason) => {
                if !self.diagnostics.iter().any(|diag| {
                    diag.span == expr.span
                        && matches!(diag.kind, HirDiagnosticKind::NotYetImplemented { .. })
                }) {
                    self.diagnostics.push(self.diagnostic(
                        HirDiagnosticKind::NotYetImplemented {
                            construct: reason.clone(),
                            owning_pass: "hir-lowering".to_string(),
                        },
                        expr.span.clone(),
                        "verifier: Unsupported HIR node reached verification without a prior diagnostic",
                    ));
                }
            }
        }
    }

    fn binding(&mut self, id: BindingId, span: std::ops::Range<usize>) {
        if !self.bindings.insert(id) {
            self.diagnostics.push(self.diagnostic(
                HirDiagnosticKind::DuplicateBindingId { id },
                span,
                "binding id reused inside resolved HIR",
            ));
        }
    }

    fn match_literal_predicate(&mut self, lit: &HirLiteral, ty: &ResolvedTy, span: Range<usize>) {
        let valid = match (lit, ty) {
            (HirLiteral::Integer(_), ty) => ty.is_integer_literal_match_scrutinee(),
            (HirLiteral::Float(_), ResolvedTy::F32 | ResolvedTy::F64)
            | (HirLiteral::Bool(_), ResolvedTy::Bool)
            | (HirLiteral::Char(_), ResolvedTy::Char)
            | (HirLiteral::String(_), ResolvedTy::String) => true,
            _ => false,
        };
        if !valid {
            self.diagnostics.push(self.diagnostic(
                HirDiagnosticKind::NotYetImplemented {
                    construct: format!(
                        "unsupported top-level literal match predicate {lit:?}: {ty:?}"
                    ),
                    owning_pass: "match-literal".to_string(),
                },
                span,
                "literal match predicates are currently limited to integers, floats, bool, char, and string",
            ));
        }
    }

    fn match_record_project_predicate(
        &mut self,
        predicate_ty: &ResolvedTy,
        scrutinee_ty: &ResolvedTy,
        span: Range<usize>,
    ) {
        if predicate_ty != scrutinee_ty || !matches!(predicate_ty, ResolvedTy::Named { .. }) {
            self.diagnostics.push(self.diagnostic(
                HirDiagnosticKind::CheckerBoundaryViolation {
                    name: "record match project".to_string(),
                    reason: format!(
                        "predicate type {} disagrees with scrutinee type {}",
                        predicate_ty.user_facing(),
                        scrutinee_ty.user_facing()
                    ),
                },
                span,
                "record project predicates must carry the resolved record scrutinee type",
            ));
        }
    }

    fn match_tuple_project_predicate(
        &mut self,
        arity: u32,
        scrutinee_ty: &ResolvedTy,
        bindings: &[crate::node::HirMatchArmBinding],
        span: Range<usize>,
    ) {
        let ResolvedTy::Tuple(items) = scrutinee_ty else {
            self.diagnostics.push(self.diagnostic(
                HirDiagnosticKind::CheckerBoundaryViolation {
                    name: "tuple match project".to_string(),
                    reason: format!(
                        "tuple project predicate on non-tuple scrutinee {}",
                        scrutinee_ty.user_facing()
                    ),
                },
                span,
                "tuple project predicates require a tuple-typed scrutinee",
            ));
            return;
        };
        if usize::try_from(arity).ok() != Some(items.len()) {
            self.diagnostics.push(self.diagnostic(
                HirDiagnosticKind::CheckerBoundaryViolation {
                    name: "tuple match project".to_string(),
                    reason: format!(
                        "predicate arity {arity} disagrees with scrutinee arity {}",
                        items.len()
                    ),
                },
                span.clone(),
                "tuple project arity must match the scrutinee tuple width",
            ));
        }
        if let Some(binding) = bindings.iter().find(|binding| {
            usize::try_from(binding.field_idx).map_or(true, |idx| idx >= items.len())
        }) {
            self.diagnostics.push(self.diagnostic(
                HirDiagnosticKind::CheckerBoundaryViolation {
                    name: "tuple match project".to_string(),
                    reason: format!(
                        "binding `{}` projects field {} from arity {} tuple",
                        binding.name,
                        binding.field_idx,
                        items.len()
                    ),
                },
                span,
                "tuple project binding indices must be within tuple arity",
            ));
        }
    }

    fn site(&mut self, id: SiteId, span: Range<usize>) {
        self.site_spans.entry(id).or_insert_with(|| HirSiteSource {
            span: span.clone(),
            source_module: self.current_source_module.clone(),
        });
        if !self.sites.insert(id) {
            self.diagnostics.push(self.diagnostic(
                HirDiagnosticKind::DuplicateSiteId { id },
                span,
                "site id reused inside resolved HIR",
            ));
        }
    }

    fn node(&mut self, id: HirNodeId, span: std::ops::Range<usize>) {
        if !self.nodes.insert(id) {
            self.diagnostics.push(self.diagnostic(
                HirDiagnosticKind::DuplicateNodeId { id },
                span,
                "HIR node id reused inside resolved HIR",
            ));
        }
    }

    fn item_source_module(module: &HirModule, item: &HirItem) -> Option<String> {
        let id = match item {
            HirItem::Function(item) => item.id,
            HirItem::TypeDecl(item) => item.id,
            HirItem::Machine(item) => item.id,
            HirItem::Record(item) => item.id,
            HirItem::Actor(item) => item.id,
            HirItem::Supervisor(item) => item.id,
            HirItem::Impl(item) => item.id,
            HirItem::ExternFn(item) => item.id,
            HirItem::Const(item) => item.id,
        };
        module.diagnostic_source_modules.get(&id).cloned()
    }
}
