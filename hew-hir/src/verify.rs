use std::collections::{HashMap, HashSet};
use std::ops::Range;

use crate::diagnostic::{HirDiagnostic, HirDiagnosticKind};
use crate::ids::{BindingId, HirNodeId, ResolvedRef, SiteId};
use crate::node::{HirBlock, HirExpr, HirExprKind, HirItem, HirModule, HirStmtKind};
use hew_types::ResolvedTy;

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
            HirExprKind::Unary { operand, .. } => self.expr(operand),
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
            | HirExprKind::CallTraitMethodStatic { receiver, args, .. } => {
                self.expr(receiver);
                for arg in args {
                    self.expr(arg);
                }
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
            | HirExprKind::MachineEventFieldAccess { .. } => {}
            HirExprKind::Scope { body } | HirExprKind::ForkBlock { body, .. } => self.block(body),
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
                        crate::node::HirSelectArmKind::AfterTimer { duration } => {
                            self.expr(duration);
                        }
                    }
                    self.expr(&arm.body);
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
            HirExprKind::Closure { body, captures, .. } => {
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
            } => {
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
            } => {
                self.expr(receiver);
                self.expr(event);
            }
            HirExprKind::CancellationTokenIsCancelled { receiver }
            | HirExprKind::MachineStateName { receiver, .. } => {
                self.expr(receiver);
            }
            HirExprKind::MachineVariantCtor { payload, .. } => {
                if let Some(fields) = payload {
                    for (_, val) in fields {
                        self.expr(val);
                    }
                }
            }
            HirExprKind::While { condition, body } => {
                self.expr(condition);
                self.block(body);
            }
            HirExprKind::Match { scrutinee, arms } => {
                self.expr(scrutinee);
                for arm in arms {
                    for binding in &arm.bindings {
                        self.binding(binding.binding, arm.span.clone());
                    }
                    self.expr(&arm.body);
                }
            }
            HirExprKind::ForRange {
                binding,
                start,
                end,
                body,
                ..
            } => {
                self.binding(binding.id, binding.span.clone());
                self.expr(start);
                self.expr(end);
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
            HirExprKind::Unsupported(reason) => {
                // Defense-in-depth: an Unsupported node should never survive
                // to verification without a prior NotYetImplemented diagnostic.
                // This catches any path where unsupported_expr() was called
                // without a preceding unsupported() call.
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

    fn binding(&mut self, id: BindingId, span: std::ops::Range<usize>) {
        if !self.bindings.insert(id) {
            self.diagnostics.push(self.diagnostic(
                HirDiagnosticKind::DuplicateBindingId { id },
                span,
                "binding id reused inside resolved HIR",
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
        };
        module.diagnostic_source_modules.get(&id).cloned()
    }
}
