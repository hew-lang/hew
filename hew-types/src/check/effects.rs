//! Suspension effects of callable bodies and checked invocations.
//!
//! A body suspends when it awaits, forks, selects or calls something that
//! suspends. A call takes its effect from its target: a declaration or closure
//! contributes its body, a runtime family its static contract, and a call
//! through a value the callee's type. A written `fn` type suspends only when
//! spelled `fn[suspends]`; a closure or named function flowing into a written
//! type that never suspends is an obligation checked once every body is known.

use std::collections::{HashMap, HashSet};

use hew_parser::ast::{Expr, Span, Spanned};

use super::{CallTarget, Checker, MethodCallRewrite, SpanKey};
use crate::env::TypeBindingId;
pub use crate::ty::EffectBody;
use crate::Ty;

#[path = "effect_solver.rs"]
mod effect_solver;
#[path = "effect_instances.rs"]
mod instances;
use effect_solver::EffectSolver;

/// The checker-owned contract consumed by lowering.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub enum SuspensionEffect {
    #[default]
    Never,
    MaySuspend,
}

impl SuspensionEffect {
    fn from_bool(suspends: bool) -> Self {
        if suspends {
            Self::MaySuspend
        } else {
            Self::Never
        }
    }
}

/// Complete body summaries and invocation verdicts.
#[derive(Debug, Clone, Default)]
pub struct SuspensionEffects {
    pub bodies: HashMap<EffectBody, SuspensionEffect>,
    pub calls: HashMap<SpanKey, SuspensionEffect>,
    /// Checked constructors have no executable callee.
    pub construct_calls: HashSet<SpanKey>,
    pub fork_transfers: HashMap<SpanKey, ForkTransferFact>,
    /// Exact checker body for each visited expression, including implicit calls.
    pub call_owners: HashMap<SpanKey, EffectBody>,
    /// Checked receiver types for explicit and indexed value operations.
    pub receivers: HashMap<SpanKey, Ty>,
    /// Selected value-operation demands, including structural comparisons.
    pub value_operations: HashMap<SpanKey, (Ty, crate::ValueCapability)>,
    /// Interpolated values whose selected rendering can invoke user bodies.
    pub rendering_operations: HashMap<SpanKey, Ty>,
    /// Checked iterables whose `for` protocol invokes source methods.
    pub iteration_operations: HashMap<SpanKey, Ty>,
    pub unresolved_calls: HashMap<SpanKey, EffectBody>,
    /// Boundaries whose transitive external effects must be confined.
    pub confined: HashSet<crate::DefId>,
    /// Required source-visible release effects, including captured values.
    pub releases: Vec<super::cleanup_effects::CleanupEffect>,
    /// Existing actor-field binding identities, shared by every callback.
    pub actor_fields: HashMap<TypeBindingId, (crate::NominalId, u32)>,
    /// Deferred or forked bodies constructed by a checked body.
    pub constructed_bodies: HashMap<EffectBody, Vec<EffectBody>>,
}

/// Proven capture capabilities and acquisition at a fork operand boundary.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ForkTransferFact {
    pub is_send: bool,
    pub is_sync: bool,
    pub acquisition: crate::ClosureCaptureAcquisition,
}

#[derive(Debug, Clone)]
struct Invocation {
    owner: Option<EffectBody>,
    receiver: Option<Ty>,
    /// The callee value's type when the call dispatches through a value.
    callee: Option<Ty>,
    /// Source spelling of the callee for diagnostics.
    name: String,
    deferred: bool,
    source_module: Option<String>,
}

/// A loop performs two distinct calls at one iterable expression. Keep those
/// calls separate from an authored call at that same span.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum EffectCallSite {
    Authored(SpanKey),
    ForIntoIter(SpanKey),
    ForNext(SpanKey),
}

impl EffectCallSite {
    fn diagnostic_span(&self) -> &SpanKey {
        match self {
            Self::Authored(key) | Self::ForIntoIter(key) | Self::ForNext(key) => key,
        }
    }

    fn authored_span(&self) -> Option<&SpanKey> {
        match self {
            Self::Authored(key) => Some(key),
            Self::ForIntoIter(_) | Self::ForNext(_) => None,
        }
    }

    fn order(&self) -> u8 {
        match self {
            Self::Authored(_) => 0,
            Self::ForIntoIter(_) => 1,
            Self::ForNext(_) => 2,
        }
    }
}

#[derive(Debug, Clone)]
struct PendingForkTransfer {
    required: crate::traits::MarkerTrait,
    context: &'static str,
    key: SpanKey,
    ty: Ty,
    candidates: super::IndirectCallCandidates,
    source_module: Option<String>,
    acquisition: crate::ClosureCaptureAcquisition,
}

/// A closure or named function flowing into a written callable type that
/// never suspends.
#[derive(Debug, Clone)]
struct SuspensionObligation {
    body: EffectBody,
    owner: Option<EffectBody>,
    once: bool,
    key: SpanKey,
    slot: ObligationSlot,
    source_module: Option<String>,
}

/// The written boundary a body is supplied to.
#[derive(Debug, Clone, PartialEq, Eq)]
enum ObligationSlot {
    /// A callable type that never suspends, as the user wrote it.
    Callable(String),
    /// A trait-object slot whose trait method is not `fn[suspends]` (A421):
    /// the trait method and the trait object, as the user wrote them.
    Dyn {
        method: String,
        target: String,
        receiver: Ty,
    },
}

#[derive(Debug, Clone, Default)]
pub(super) struct EffectGraph {
    pub builtin_suspensions: HashSet<CallTarget>,
    pub current_body: Option<EffectBody>,
    expression_owners: HashMap<SpanKey, EffectBody>,
    receivers: HashMap<SpanKey, Ty>,
    value_operations: HashMap<SpanKey, (Ty, crate::ValueCapability)>,
    rendering_operations: HashMap<SpanKey, Ty>,
    iteration_operations: HashMap<SpanKey, Ty>,
    pub constructed_bodies: HashMap<EffectBody, Vec<EffectBody>>,
    pub confined: HashSet<crate::DefId>,
    /// Bodies and whether a construct inside them suspends on its own.
    pub bodies: HashMap<EffectBody, bool>,
    /// The construct or call that first made each body suspend.
    witnesses: HashMap<EffectBody, String>,
    calls: HashMap<EffectCallSite, Invocation>,
    /// Every authored call the body checker visited, including unresolved
    /// calls. Imported-body eligibility refuses a body missing a selected
    /// target instead of inferring one from the callee spelling.
    seen_calls: HashMap<SpanKey, Option<EffectBody>>,
    construct_calls: HashSet<SpanKey>,
    submission_effects: HashMap<SpanKey, bool>,
    fork_transfers: Vec<PendingForkTransfer>,
    obligations: Vec<SuspensionObligation>,
    pub(super) releases: Vec<super::cleanup_effects::CleanupEffect>,
    pub(super) once_releases: HashMap<EffectBody, Vec<super::cleanup_effects::CleanupValue>>,
}

impl Checker {
    fn effect_for_method(&self, site: &EffectCallSite) -> Option<&super::types::CheckedForMethod> {
        let plan = self.for_loop_plans.get(site.diagnostic_span())?;
        match site {
            EffectCallSite::ForIntoIter(_) => plan.into_iter.as_ref(),
            EffectCallSite::ForNext(_) => Some(&plan.next),
            EffectCallSite::Authored(_) => None,
        }
    }

    fn effect_call_target(&self, site: &EffectCallSite) -> Option<&CallTarget> {
        site.authored_span()
            .and_then(|key| self.call_target(key))
            .or_else(|| self.effect_for_method(site).map(|method| &method.target))
    }

    fn effect_trait_args(&self, site: &EffectCallSite) -> Option<&[Ty]> {
        if let Some(method) = self.effect_for_method(site) {
            return Some(&method.trait_args);
        }
        let key = site.authored_span()?;
        match self.method_call_rewrites.get(key)? {
            MethodCallRewrite::StaticTraitDispatch { trait_args, .. } => Some(trait_args),
            _ => None,
        }
    }

    pub(super) fn record_iteration_effect(
        &mut self,
        span: &Span,
        ty: &Ty,
        candidates: super::IndirectCallCandidates,
    ) {
        self.generic_trait_call_arguments
            .entry(SpanKey::in_module(span, self.current_module_idx))
            .or_insert_with(|| {
                vec![super::CallableDispatchActual {
                    slot: 0,
                    candidates,
                }]
            });
        self.effect_graph.iteration_operations.insert(
            SpanKey::in_module(span, self.current_module_idx),
            ty.clone(),
        );
        let key = SpanKey::in_module(span, self.current_module_idx);
        if let Some(plan) = self.for_loop_plans.get(&key).cloned() {
            let owner = self.effect_graph.current_body.clone();
            let deferred = self.deferred_body.is_some();
            let source_module = self.current_module.clone();
            if plan.into_iter.is_some() {
                self.effect_graph.calls.insert(
                    EffectCallSite::ForIntoIter(key.clone()),
                    Invocation {
                        owner: owner.clone(),
                        receiver: Some(ty.clone()),
                        callee: None,
                        name: "IntoIterator.into_iter(...)".to_string(),
                        deferred,
                        source_module: source_module.clone(),
                    },
                );
            }
            self.effect_graph.calls.insert(
                EffectCallSite::ForNext(key),
                Invocation {
                    owner,
                    receiver: Some(plan.cursor_type),
                    callee: None,
                    name: "Iterator.next(...)".to_string(),
                    deferred,
                    source_module,
                },
            );
        }
    }

    pub(super) fn record_rendering_effect(&mut self, span: &Span, ty: &Ty) {
        self.effect_graph.rendering_operations.insert(
            SpanKey::in_module(span, self.current_module_idx),
            ty.clone(),
        );
    }

    pub(super) fn record_constructed_effect_body(&mut self, body: &EffectBody) {
        if let Some(owner) = &self.effect_graph.current_body {
            self.effect_graph
                .constructed_bodies
                .entry(owner.clone())
                .or_default()
                .push(body.clone());
        }
    }

    pub(super) fn enter_named_effect_body(&mut self, name: &str) -> Option<EffectBody> {
        let body = self.lookup_declaration(name).map(EffectBody::Declaration);
        if let Some(body) = &body {
            self.effect_graph.bodies.entry(body.clone()).or_default();
        }
        std::mem::replace(&mut self.effect_graph.current_body, body)
    }

    /// Record owning task inputs after invocation checking has resolved their
    /// types and consumption. Rechecking expressions here would repeat moves.
    pub(super) fn record_fork_call_inputs(&mut self, branch: &Spanned<Expr>) {
        match &branch.0 {
            Expr::Call { function, args, .. } => {
                if let Some(ty) = self.callee_value_type(function) {
                    self.check_fork_transfer(&function.0, &function.1, &ty);
                }
                for arg in args {
                    let (expr, span) = arg.expr();
                    if let Some(ty) = self
                        .expr_types
                        .get(&SpanKey::in_module(span, self.current_module_idx))
                        .cloned()
                    {
                        self.check_fork_transfer(expr, span, &ty);
                    }
                }
            }
            Expr::MethodCall {
                receiver,
                method,
                args,
            } => {
                let key = SpanKey::in_module(&branch.1, self.current_module_idx);
                if matches!(
                    self.method_call_rewrites.get(&key),
                    Some(MethodCallRewrite::RecordFnFieldCall { .. })
                ) {
                    let field = Expr::FieldAccess {
                        object: receiver.clone(),
                        field: method.clone(),
                    };
                    if let Some(ty) = self.record_fn_field_type(receiver, method.0.name.as_str()) {
                        let candidates = self.callable_field_candidates(receiver, method);
                        self.check_fork_value_transfer(&field, &branch.1, &ty, candidates);
                    }
                } else if !matches!(
                    self.method_call_receiver_kinds.get(&key),
                    Some(
                        super::MethodCallReceiverKind::ModuleBinding { .. }
                            | super::MethodCallReceiverKind::EnumConstructorPath { .. }
                    )
                ) {
                    if let Some(ty) = self
                        .expr_types
                        .get(&SpanKey::in_module(&receiver.1, self.current_module_idx))
                        .cloned()
                    {
                        self.check_fork_transfer(&receiver.0, &receiver.1, &ty);
                    }
                }
                for arg in args {
                    let (expr, span) = arg.expr();
                    if let Some(ty) = self
                        .expr_types
                        .get(&SpanKey::in_module(span, self.current_module_idx))
                        .cloned()
                    {
                        self.check_fork_transfer(expr, span, &ty);
                    }
                }
            }
            _ => {}
        }
    }

    pub(super) fn check_select_call_transfer(
        &mut self,
        expr: &Expr,
        span: &Span,
        shared: bool,
        loan: Option<bool>,
    ) {
        let key = SpanKey::in_module(span, self.current_module_idx);
        let ty = self.expr_types.get(&key).cloned().or_else(|| match expr {
            Expr::Ident(name) => self.env.lookup_ref(name).map(|binding| binding.ty.clone()),
            _ => None,
        });
        let Some(ty) = ty else {
            self.report_error(
                crate::error::TypeErrorKind::InvalidOperation,
                span,
                "select call input lacks its checked type".into(),
            );
            return;
        };
        if let Some(exclusive) = loan {
            self.record_select_input_loan(expr, span, &ty, exclusive);
        }
        self.check_select_call_transfer_typed(
            span,
            ty,
            self.callable_candidates_for_expr(expr, span),
            shared,
        );
    }

    pub(super) fn record_select_input_loan(
        &mut self,
        expr: &Expr,
        span: &Span,
        ty: &Ty,
        exclusive: bool,
    ) {
        if self.type_has_marker(ty, crate::traits::MarkerTrait::Copy) {
            return;
        }
        let Some((root, path)) = self.expr_place(expr) else {
            return;
        };
        self.reject_prepared_input_access(
            root,
            &path,
            span,
            crate::error::TypeErrorKind::InvalidOperation,
            !exclusive,
        );
        let Some(binding) = self.env.lookup_ref(root) else {
            return;
        };
        self.prepared_select_inputs
            .push(super::types::PreparedSelectInput {
                binding: binding.id,
                path,
                span: span.clone(),
                access: if exclusive {
                    super::types::PreparedSelectAccess::Exclusive
                } else {
                    super::types::PreparedSelectAccess::Shared
                },
            });
    }

    pub(super) fn check_select_call_transfer_typed(
        &mut self,
        span: &Span,
        ty: Ty,
        candidates: super::IndirectCallCandidates,
        shared: bool,
    ) {
        let key = SpanKey::in_module(span, self.current_module_idx);
        self.effect_graph.fork_transfers.push(PendingForkTransfer {
            required: if shared {
                crate::traits::MarkerTrait::Sync
            } else {
                crate::traits::MarkerTrait::Send
            },
            context: "select call",
            key,
            ty,
            candidates,
            source_module: self.current_module.clone(),
            acquisition: crate::ClosureCaptureAcquisition::Snapshot,
        });
    }

    pub(super) fn record_actor_marker_transfer(
        &mut self,
        expr: &Expr,
        span: &Span,
        error_span: &Span,
        ty: Ty,
    ) {
        self.effect_graph.fork_transfers.push(PendingForkTransfer {
            required: crate::traits::MarkerTrait::Send,
            context: "actor send",
            key: SpanKey::in_module(error_span, self.current_module_idx),
            ty,
            candidates: self.callable_candidates_for_expr(expr, span),
            source_module: self.current_module.clone(),
            acquisition: crate::ClosureCaptureAcquisition::Move,
        });
    }

    pub(super) fn check_fork_transfer(&mut self, expr: &Expr, span: &Span, ty: &Ty) {
        let candidates = self.callable_candidates_for_expr(expr, span);
        self.check_fork_value_transfer(expr, span, ty, candidates);
    }

    fn check_fork_value_transfer(
        &mut self,
        expr: &Expr,
        span: &Span,
        ty: &Ty,
        candidates: super::IndirectCallCandidates,
    ) {
        let ty = self.subst.resolve(ty);
        let key = SpanKey::in_module(span, self.current_module_idx);
        // Invocation checking has already recorded explicit consuming
        // parameters/receivers. Ordinary value operands acquire snapshots.
        let place = self.expr_place(expr);
        let consumed = place.as_ref().is_some_and(|(root, path)| {
            self.env
                .lookup_ref(root)
                .is_some_and(|binding| binding.is_moved)
                || self.env.place_move_conflict(root, path).is_some()
        });
        let snapshot = place.is_some()
            && !consumed
            && !matches!(ty, Ty::Borrow { .. })
            && self.parameter_has_independent_clone(&ty);
        self.effect_graph.fork_transfers.push(PendingForkTransfer {
            required: crate::traits::MarkerTrait::Send,
            context: "fork",
            key,
            ty: ty.clone(),
            candidates,
            source_module: self.current_module.clone(),
            acquisition: if snapshot {
                crate::ClosureCaptureAcquisition::Snapshot
            } else {
                crate::ClosureCaptureAcquisition::Move
            },
        });
        if matches!(ty, Ty::Borrow { .. }) {
            self.report_error(
                crate::error::TypeErrorKind::InvalidSend,
                span,
                "fork cannot retain a borrowed view in its owning task environment".to_string(),
            );
        } else if !snapshot
            && !self
                .registry
                .implements_marker(&ty, crate::traits::MarkerTrait::Copy)
            && !self.reject_borrowed_consumption(expr, span)
        {
            self.mark_expr_moved(expr, span);
        }
    }

    /// The current body suspends on its own through `witness`.
    pub(super) fn mark_body_suspends(&mut self, witness: &str) {
        if let Some(owner) = self.effect_graph.current_body.clone() {
            self.effect_graph.bodies.insert(owner.clone(), true);
            self.effect_graph
                .witnesses
                .entry(owner)
                .or_insert_with(|| witness.to_string());
        }
    }

    /// Enter the declaration-owned effect body of a crash hook.
    pub(super) fn enter_crash_hook_body(&mut self, qualified_name: &str) -> Option<EffectBody> {
        let body = self
            .lookup_declaration(qualified_name)
            .map(EffectBody::Declaration);
        if let Some(body) = &body {
            self.effect_graph.bodies.entry(body.clone()).or_default();
        }
        std::mem::replace(&mut self.effect_graph.current_body, body)
    }

    /// Consume the actor policy checker's immutable submission verdict.
    pub(super) fn record_submission_suspension(&mut self, span: &Span, may_suspend: bool) {
        let key = SpanKey::in_module(span, self.current_module_idx);
        self.effect_graph
            .submission_effects
            .insert(key, may_suspend);
        if may_suspend {
            self.mark_body_suspends("actor delivery");
        }
    }

    /// A closure or named function coerced into a written callable type must
    /// not suspend unless that type says `fn[suspends]`. Bodies are known only
    /// after the fixed point, so the check is deferred to it.
    ///
    /// An erasure into a trait object supplies each impl method to a slot;
    /// a slot whose trait method is not `fn[suspends]` obliges that method's
    /// body not to suspend (A421).
    pub(super) fn record_dyn_slot_obligations(
        &mut self,
        traits: &[crate::ty::TraitObjectBound],
        fillers: &[(crate::DefId, crate::DefId)],
        receiver: &Ty,
        span: &Span,
    ) {
        let Some(layout) = self.trait_object_layouts.get(traits) else {
            return;
        };
        let target = Ty::TraitObject {
            traits: traits.to_vec(),
        }
        .user_facing()
        .to_string();
        let mut obligations = Vec::new();
        for &(method, impl_method) in fillers {
            let plain = layout
                .slots
                .iter()
                .any(|slot| slot.method == method && slot.effect == super::SlotEffect::Plain);
            if plain {
                obligations.push(SuspensionObligation {
                    body: EffectBody::Declaration(impl_method),
                    owner: self.effect_graph.current_body.clone(),
                    once: false,
                    key: SpanKey::in_module(span, self.current_module_idx),
                    slot: ObligationSlot::Dyn {
                        method: self.defs.display(method).to_string(),
                        target: target.clone(),
                        receiver: receiver.clone(),
                    },
                    source_module: self.current_module.clone(),
                });
            }
        }
        self.effect_graph.obligations.extend(obligations);
    }

    pub(super) fn record_suspension_obligations(
        &mut self,
        expected: &Ty,
        actual: &Ty,
        span: &Span,
    ) {
        let expected_resolved = self.subst.resolve(expected);
        let actual_resolved = self.subst.resolve(actual);
        match (
            expected_resolved.callable_body(),
            actual_resolved.callable_body(),
        ) {
            (Ty::Function { capabilities, .. }, Ty::Closure { identity, .. }) => {
                if !capabilities.suspends {
                    self.record_plain_callable_obligation(
                        identity.clone(),
                        expected,
                        capabilities.call == crate::CallableCallMode::Once,
                        span,
                    );
                }
            }
            (Ty::Function { capabilities, .. }, Ty::Function { .. }) if !capabilities.suspends => {
                let key = SpanKey::in_module(span, self.current_module_idx);
                let candidates = self
                    .expression_value_candidates
                    .get(&key)
                    .cloned()
                    .unwrap_or_else(|| self.resolved_callable_candidate(span));
                for candidate in candidates.known {
                    let body = match candidate {
                        super::CallableCandidate::Declaration(id) => EffectBody::Declaration(id),
                        super::CallableCandidate::Closure(key) => EffectBody::Closure(key),
                        _ => continue,
                    };
                    self.record_plain_callable_obligation(
                        body,
                        expected,
                        capabilities.call == crate::CallableCallMode::Once,
                        span,
                    );
                }
            }
            (Ty::Named { args: expected, .. }, Ty::Named { args: actual, .. })
            | (Ty::Tuple(expected), Ty::Tuple(actual)) => {
                for (expected, actual) in expected.iter().zip(actual) {
                    self.record_suspension_obligations(expected, actual, span);
                }
            }
            (Ty::Array(expected, _), Ty::Array(actual, _)) => {
                self.record_suspension_obligations(expected, actual, span);
            }
            _ => {}
        }
    }

    fn record_plain_callable_obligation(
        &mut self,
        body: EffectBody,
        expected: &Ty,
        once: bool,
        span: &Span,
    ) {
        let slot = ObligationSlot::Callable(expected.user_facing().to_string());
        if self
            .effect_graph
            .obligations
            .iter()
            .any(|owed| owed.body == body && owed.slot == slot)
        {
            return;
        }
        self.effect_graph.obligations.push(SuspensionObligation {
            body,
            owner: self.effect_graph.current_body.clone(),
            once,
            key: SpanKey::in_module(span, self.current_module_idx),
            slot,
            source_module: self.current_module.clone(),
        });
    }

    pub(super) fn callee_value_type(&self, callee: &Spanned<Expr>) -> Option<Ty> {
        self.expr_types
            .get(&SpanKey::in_module(&callee.1, self.current_module_idx))
            .cloned()
            .or_else(|| match &callee.0 {
                Expr::Ident(name) => self
                    .env
                    .lookup_ref(name.name.as_str())
                    .map(|binding| binding.ty.clone()),
                _ => None,
            })
    }

    /// The instantiated type of a callable record field selected as a method.
    pub(super) fn record_fn_field_type(&self, receiver: &Spanned<Expr>, field: &str) -> Option<Ty> {
        let receiver_ty = self
            .expr_types
            .get(&SpanKey::in_module(&receiver.1, self.current_module_idx))?;
        let Ty::Named { head, args } = self.subst.resolve(receiver_ty) else {
            return None;
        };
        let definition = self.head_type_def(head)?;
        Some(Self::instantiate_type_def_member(
            definition.fields.get(field)?,
            &definition.type_params,
            &args,
        ))
    }

    fn record_value_operation_effects(&mut self, expr: &Expr, span: &Span) {
        let key = SpanKey::in_module(span, self.current_module_idx);
        let receiver = match expr {
            Expr::MethodCall { receiver, .. } => Some(receiver.as_ref()),
            Expr::Index { object, .. } => Some(object.as_ref()),
            _ => None,
        };
        if let Some(receiver) = receiver.and_then(|receiver| self.callee_value_type(receiver)) {
            if matches!(
                self.method_call_rewrites.get(&key),
                Some(MethodCallRewrite::GeneratorNext { .. })
            ) {
                let candidates = match expr {
                    Expr::MethodCall { receiver, .. } => {
                        self.callable_candidates_for_expr(&receiver.0, &receiver.1)
                    }
                    _ => super::IndirectCallCandidates::unknown(),
                };
                self.record_iteration_effect(span, &receiver, candidates);
            }
            self.effect_graph.receivers.insert(key.clone(), receiver);
        }
        if let Expr::Binary { left, op, .. } = expr {
            use hew_parser::ast::BinaryOp;
            let capability = match op {
                BinaryOp::Equal | BinaryOp::NotEqual => Some(crate::ValueCapability::Eq),
                BinaryOp::Less => Some(crate::ValueCapability::Less),
                BinaryOp::LessEqual => Some(crate::ValueCapability::LessEqual),
                BinaryOp::Greater => Some(crate::ValueCapability::Greater),
                BinaryOp::GreaterEqual => Some(crate::ValueCapability::GreaterEqual),
                _ => None,
            };
            if let Some(capability) = capability {
                if let Some(ty) = self.callee_value_type(left) {
                    self.effect_graph
                        .value_operations
                        .insert(key.clone(), (ty, capability));
                }
            }
        }
        if let Expr::InterpolatedString(parts) = expr {
            for part in parts {
                if let hew_parser::ast::StringPart::Expr(value)
                | hew_parser::ast::StringPart::StructuralExpr(value) = part
                {
                    if let Some(ty) = self.callee_value_type(value) {
                        self.record_rendering_effect(&value.1, &ty);
                    }
                }
            }
        }
    }

    pub(super) fn record_expression_effect(&mut self, expr: &Expr, span: &Span) {
        let key = SpanKey::in_module(span, self.current_module_idx);
        if let Some(owner) = &self.effect_graph.current_body {
            self.effect_graph
                .expression_owners
                .insert(key.clone(), owner.clone());
        }
        self.record_value_operation_effects(expr, span);
        self.record_receiver_release_effect(&key);
        let checked_invocation = self.selected_callable_target(span).is_some()
            || self.method_call_rewrites.contains_key(&key)
            || self.actor_method_dispatch.contains_key(&key)
            || self.actor_delivery_calls.contains_key(&key);
        if matches!(
            expr,
            Expr::Call { .. } | Expr::MethodCall { .. } | Expr::Spawn { .. }
        ) || (checked_invocation && matches!(expr, Expr::Index { .. }))
        {
            self.effect_graph
                .seen_calls
                .insert(key.clone(), self.effect_graph.current_body.clone());
        }
        if matches!(expr, Expr::Spawn { .. }) {
            self.effect_graph.construct_calls.insert(key.clone());
        }
        if checked_invocation
            && matches!(
                expr,
                Expr::Call { .. } | Expr::MethodCall { .. } | Expr::Index { .. }
            )
        {
            let (callee, name) = match expr {
                Expr::Call { function, .. } => (
                    self.callee_value_type(function),
                    match &function.0 {
                        Expr::Ident(name) => name.to_string(),
                        Expr::FieldAccess { field, .. } => field.0.to_string(),
                        _ => "callee".to_string(),
                    },
                ),
                Expr::MethodCall {
                    receiver,
                    method,
                    args,
                } => {
                    let callee = match self.method_call_rewrites.get(&key) {
                        Some(MethodCallRewrite::BuiltinVecHigherOrder { .. }) => {
                            args.first().and_then(|arg| {
                                let (_, span) = arg.expr();
                                self.expr_types
                                    .get(&SpanKey::in_module(span, self.current_module_idx))
                                    .cloned()
                            })
                        }
                        Some(MethodCallRewrite::RecordFnFieldCall { .. }) => {
                            self.record_fn_field_type(receiver, method.0.name.as_str())
                        }
                        _ if self.direct_call_targets.get(&key)
                            == Some(&CallTarget::IndirectFunctionValue) =>
                        {
                            self.record_fn_field_type(receiver, method.0.name.as_str())
                        }
                        _ => None,
                    };
                    (callee, method.0.to_string())
                }
                Expr::Index { .. } => (None, "Index.at".to_string()),
                _ => (None, "send".to_string()),
            };
            self.effect_graph.calls.insert(
                EffectCallSite::Authored(key),
                Invocation {
                    owner: self.effect_graph.current_body.clone(),
                    receiver: match expr {
                        Expr::MethodCall { receiver, .. } => self.callee_value_type(receiver),
                        Expr::Index { object, .. } => self.callee_value_type(object),
                        _ => None,
                    },
                    callee,
                    name: format!("{name}(...)"),
                    deferred: self.deferred_body.is_some(),
                    source_module: self.current_module.clone(),
                },
            );
        }
        self.record_intrinsic_suspension(expr);
    }

    fn record_receiver_release_effect(&mut self, key: &SpanKey) {
        let releases_receiver = self.call_target(key).is_some_and(|target| {
            let family = match target {
                CallTarget::Runtime(family) | CallTarget::DeclaredRuntime { family, .. } => {
                    Some(*family)
                }
                CallTarget::RuntimeCollection(method) => method.runtime_family(),
                _ => None,
            };
            family.is_some_and(|family| {
                family.consumes_receiver() || family.releases_receiver_contents()
            })
        });
        if releases_receiver {
            if let Some(receiver) = self.effect_graph.receivers.get(key).cloned() {
                self.record_owned_cleanup_effect(&receiver);
            }
        }
    }

    pub(super) fn is_construct_call(&self, span: &Span) -> bool {
        self.effect_graph
            .construct_calls
            .contains(&SpanKey::in_module(span, self.current_module_idx))
    }

    /// A checked variant constructor is a call-form expression with no
    /// executable callee. Mark it at the selection site so body eligibility
    /// does not mistake it for an unresolved function call.
    pub(super) fn record_construct_call(&mut self, span: &Span) {
        self.effect_graph
            .construct_calls
            .insert(SpanKey::in_module(span, self.current_module_idx));
    }

    /// Task joins and structured child teardown suspend independently of the
    /// child callable's own effect. Awaiting an ordinary call instead takes
    /// its effect from the invocation edge.
    fn record_intrinsic_suspension(&mut self, expr: &Expr) {
        let witness = match expr {
            Expr::Await(inner)
                if !matches!(inner.0, Expr::Call { .. } | Expr::MethodCall { .. }) =>
            {
                "await"
            }
            Expr::Race(_) => "race",
            Expr::Select { .. } => "select",
            Expr::ForkChild { .. } | Expr::ForkBlock { .. } => "fork",
            Expr::ScopeDeadline { .. } => "scope within",
            _ => return,
        };
        self.mark_body_suspends(witness);
    }

    fn call_target(&self, key: &SpanKey) -> Option<&CallTarget> {
        self.direct_call_targets
            .get(key)
            .or_else(|| self.resolved_calls.get(key).map(|call| &call.target))
            .or_else(|| {
                self.dyn_trait_method_calls
                    .get(key)
                    .map(|call| &call.target)
            })
            .or_else(|| match self.method_call_rewrites.get(key) {
                Some(
                    MethodCallRewrite::RewriteToFunction { target, .. }
                    | MethodCallRewrite::RewriteModuleQualifiedToFunction { target, .. }
                    | MethodCallRewrite::StaticTraitDispatch { target, .. }
                    | MethodCallRewrite::BinderStaticCall(super::types::BinderTraitCall {
                        target,
                        ..
                    }),
                ) => Some(target),
                _ => None,
            })
    }

    /// Direct calls made by checked impl bodies, grouped by the body's exact
    /// declaration. An unresolved or indirect edge has no declaration to
    /// propagate; its own HIR lowering still verifies the checked call fact.
    pub(super) fn checked_impl_body_callees(&self) -> HashMap<crate::DefId, Vec<crate::DefId>> {
        let mut result: HashMap<crate::DefId, Vec<crate::DefId>> = HashMap::new();
        for body in self.effect_graph.bodies.keys() {
            if let EffectBody::Declaration(id) = body {
                if self.defs.kind(*id) == crate::DeclarationKind::ImplMethod {
                    result.entry(*id).or_default();
                }
            }
        }
        for (key, owner) in &self.effect_graph.seen_calls {
            if self
                .effect_graph
                .calls
                .contains_key(&EffectCallSite::Authored(key.clone()))
                || self.effect_graph.construct_calls.contains(key)
            {
                continue;
            }
            if let Some(EffectBody::Declaration(declaration)) = owner {
                result.remove(declaration);
            }
        }
        for (key, invocation) in &self.effect_graph.calls {
            let Some(EffectBody::Declaration(owner)) = invocation.owner.as_ref() else {
                continue;
            };
            let Some(callees) = result.get_mut(owner) else {
                continue;
            };
            if let Some(CallTarget::User(id) | CallTarget::ImplMethod(id)) =
                self.effect_call_target(key)
            {
                callees.push(*id);
            }
        }
        for callees in result.values_mut() {
            callees.sort_unstable();
            callees.dedup();
        }
        result
    }

    /// Instantiate impl-owned cleanup parameters from the selected receiver.
    /// Both ordinary calls and trait-object fillers use this same binding.
    fn receiver_cleanup_substitutions(
        &self,
        declaration: crate::DefId,
        receiver: &Ty,
        enclosing: &HashMap<crate::ParamHead, Ty>,
    ) -> HashMap<crate::ParamHead, Ty> {
        let Some(method) = self
            .fn_sigs
            .get(&declaration)
            .and_then(|signature| signature.impl_method.as_ref())
        else {
            return HashMap::new();
        };
        let Ty::Named { args, .. } = self
            .subst
            .resolve(receiver)
            .substitute_type_params_parallel(enclosing)
        else {
            return HashMap::new();
        };
        crate::method_resolution::impl_receiver_substitution(
            &method.receiver_parameters,
            &method.receiver_args,
            &args,
        )
        .unwrap_or_default()
    }

    fn call_cleanup_substitutions(
        &self,
        declaration: crate::DefId,
        site: &EffectCallSite,
        receiver: Option<&Ty>,
        enclosing: &HashMap<crate::ParamHead, Ty>,
    ) -> HashMap<crate::ParamHead, Ty> {
        let Some(signature) = self.fn_sigs.get(&declaration) else {
            return HashMap::new();
        };
        let mut substitutions = receiver.map_or_else(HashMap::new, |receiver| {
            self.receiver_cleanup_substitutions(declaration, receiver, enclosing)
        });
        let parameters: Vec<_> = signature
            .type_params
            .iter()
            .copied()
            .filter(|parameter| !substitutions.contains_key(parameter))
            .collect();
        substitutions.extend(
            parameters.into_iter().zip(
                site.authored_span()
                    .and_then(|key| self.call_type_args.get(key))
                    .into_iter()
                    .flatten()
                    .map(|ty| {
                        self.subst
                            .resolve(ty)
                            .substitute_type_params_parallel(enclosing)
                    }),
            ),
        );
        // A checked `for` can call `next` on an associated cursor such as
        // `I.IntoIter`. Its binder belongs to the same generic declaration;
        // instantiate it from the actual `I` before resolving that call.
        self.complete_associated_schema_substitutions(&mut substitutions);
        substitutions
    }

    fn finish_fork_transfers(
        &mut self,
        graph: &EffectGraph,
        output: &mut SuspensionEffects,
        flows: &HashMap<SpanKey, Vec<super::CallableArgumentFlow>>,
    ) {
        for PendingForkTransfer {
            key,
            ty,
            candidates,
            source_module,
            acquisition,
            required,
            context,
        } in &graph.fork_transfers
        {
            let ty = self.subst.resolve(ty);
            let proven = |marker| {
                self.type_has_marker(&ty, marker)
                    || self.value_candidates_have_marker(&ty, candidates, marker, flows)
            };
            let is_send = proven(crate::traits::MarkerTrait::Send);
            let is_sync = proven(crate::traits::MarkerTrait::Sync);
            output.fork_transfers.insert(
                key.clone(),
                ForkTransferFact {
                    is_send,
                    is_sync,
                    acquisition: *acquisition,
                },
            );
            if !proven(*required) {
                let mut error = crate::error::TypeError::new(
                    crate::error::TypeErrorKind::InvalidSend,
                    key.start..key.end,
                    if *context == "actor send" {
                        format!(
                            "cannot send `{}` to actor: type is not Send",
                            ty.user_facing()
                        )
                    } else if *context == "fork" {
                        format!(
                            "fork cannot transfer `{}`: the value is not proven Send",
                            ty.user_facing()
                        )
                    } else {
                        format!(
                            "{context} cannot schedule `{}`: the value is not proven {required}",
                            ty.user_facing()
                        )
                    },
                );
                error.source_module.clone_from(source_module);
                if *context == "actor send" {
                    error.suggestions.push("keep the resource inside one owning actor and send that actor a message instead — see the language guide, 'Own a resource with an actor'".into());
                }
                self.errors.push(error);
            }
        }
    }

    #[allow(
        clippy::too_many_lines,
        reason = "Resolve the effect fixed point and report its source obligations in one pass"
    )]
    pub(super) fn finish_suspension_effects(
        &mut self,
        flows: &HashMap<SpanKey, Vec<super::CallableArgumentFlow>>,
    ) -> SuspensionEffects {
        let graph = std::mem::take(&mut self.effect_graph);
        let mut solver = EffectSolver::new(self, &graph, flows);
        let mut conservative = HashMap::new();
        for body in graph.bodies.keys() {
            if let Some(witness) = solver.body(body, true) {
                conservative.insert(body.clone(), witness);
            }
        }
        let calls: HashMap<_, _> = graph
            .calls
            .iter()
            .map(|(key, invocation)| (key.clone(), solver.invocation(key, invocation)))
            .collect();
        let failures: Vec<_> = graph
            .obligations
            .iter()
            .map(|obligation| solver.obligation(obligation))
            .collect();
        drop(solver);
        let mut output = SuspensionEffects {
            construct_calls: graph.construct_calls.clone(),
            call_owners: graph.expression_owners.clone(),
            value_operations: graph
                .value_operations
                .iter()
                .map(|(key, (ty, capability))| (key.clone(), (self.subst.resolve(ty), *capability)))
                .collect(),
            rendering_operations: graph
                .rendering_operations
                .iter()
                .map(|(key, ty)| (key.clone(), self.subst.resolve(ty)))
                .collect(),
            iteration_operations: graph
                .iteration_operations
                .iter()
                .map(|(key, ty)| (key.clone(), self.subst.resolve(ty)))
                .collect(),
            receivers: graph
                .receivers
                .iter()
                .map(|(key, ty)| (key.clone(), self.subst.resolve(ty)))
                .collect(),
            unresolved_calls: graph
                .seen_calls
                .iter()
                .filter(|(key, _)| {
                    !graph
                        .calls
                        .contains_key(&EffectCallSite::Authored((*key).clone()))
                        && !graph.construct_calls.contains(*key)
                })
                .filter_map(|(key, owner)| Some((key.clone(), owner.clone()?)))
                .collect(),
            confined: graph.confined.clone(),
            releases: self.published_cleanup_effects(&graph),
            actor_fields: self.actor_field_binding_ids.clone(),
            constructed_bodies: graph.constructed_bodies.clone(),
            ..SuspensionEffects::default()
        };
        for body in graph.bodies.keys() {
            output.bodies.insert(
                body.clone(),
                SuspensionEffect::from_bool(conservative.contains_key(body)),
            );
        }
        for (site, invocation) in &graph.calls {
            let key = site.diagnostic_span();
            let suspends = calls[site];
            if let Some(key) = site.authored_span() {
                output
                    .calls
                    .insert(key.clone(), SuspensionEffect::from_bool(suspends));
            }
            if suspends && invocation.deferred {
                let mut error = crate::error::TypeError::new(
                    crate::error::TypeErrorKind::InvalidOperation,
                    key.start..key.end,
                    format!(
                        "a deferred body cannot suspend: `{}` may suspend",
                        invocation.name
                    ),
                );
                error.source_module.clone_from(&invocation.source_module);
                self.errors.push(error);
            }
        }
        let mut reported_obligations = HashSet::new();
        for (obligation, failures) in graph.obligations.iter().zip(failures) {
            if matches!(obligation.body, EffectBody::Declaration(_))
                && !graph.bodies.contains_key(&obligation.body)
            {
                continue;
            }
            if failures.is_empty() {
                // A pure concrete dyn filler does not establish purity for
                // every instantiation of its generic impl method.
                if matches!(obligation.slot, ObligationSlot::Callable(_)) {
                    output
                        .bodies
                        .insert(obligation.body.clone(), SuspensionEffect::Never);
                }
                continue;
            }
            let witness = failures
                .first()
                .map(|(_, _, witness)| witness)
                .expect("suspending instance");
            let subject = match &obligation.body {
                EffectBody::Declaration(id) => format!("function `{}`", self.defs.path(*id)),
                _ => "closure".to_string(),
            };
            let message = match &obligation.slot {
                ObligationSlot::Callable(slot) => format!(
                    "{subject} suspends via `{witness}`; `{slot}` never suspends, write `fn[suspends]`"
                ),
                ObligationSlot::Dyn { method, target, .. } => format!(
                    "E_DYN_SLOT_SUSPENDS: {subject} suspends via `{witness}`, so it cannot fill \
                     `{method}` in `{target}`; that trait method never suspends unless it is \
                     declared `fn[suspends]`"
                ),
            };
            for (key, source_module, _) in failures {
                // Distinct coercion sites in one generic body can discharge at
                // the same failing call instance. They are separate solver
                // obligations but describe one source diagnostic there.
                if !reported_obligations.insert((
                    key.clone(),
                    source_module.clone(),
                    message.clone(),
                )) {
                    continue;
                }
                let mut error = crate::error::TypeError::new(
                    crate::error::TypeErrorKind::InvalidOperation,
                    key.start..key.end,
                    message.clone(),
                );
                error.source_module = source_module;
                self.errors.push(error);
            }
        }
        self.finish_fork_transfers(&graph, &mut output, flows);
        output
    }
}
