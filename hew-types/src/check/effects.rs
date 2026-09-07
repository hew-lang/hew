//! Suspension effects of callable bodies and checked invocations.
//!
//! A body suspends when it awaits, forks, selects or calls something that
//! suspends. A call takes its effect from its target: a declaration or closure
//! contributes its body, a runtime family its static contract, and a call
//! through a value the callee's type. A written `fn` type suspends only when
//! spelled `fn[suspends]`; a closure or named function flowing into a written
//! type that never suspends is an obligation checked once every body is known.

use std::collections::{BTreeMap, HashMap, HashSet};

use hew_parser::ast::{Expr, Pattern, Span, Spanned, Stmt};

use super::{CallTarget, Checker, MethodCallRewrite, SpanKey};
use crate::env::TypeBindingId;
pub use crate::ty::EffectBody;
use crate::Ty;

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
    pub fork_transfers: HashMap<SpanKey, ForkTransferFact>,
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
    /// The callee value's type when the call dispatches through a value.
    callee: Option<Ty>,
    /// Source spelling of the callee for diagnostics.
    name: String,
    deferred: bool,
    source_module: Option<String>,
}

/// The concrete callable behind a fork operand. A written callable slot
/// erases its captures, so the Send proof for a fork through a record field
/// or tuple element is not in the slot's type; this ledger keeps the concrete
/// callable types stored through local bindings for fork transfers only.
#[derive(Debug, Clone, PartialEq, Eq)]
enum CallableOrigin {
    Unknown,
    Typed(Ty),
    Aggregate(BTreeMap<String, CallableOrigin>),
    Binding {
        binding: TypeBindingId,
        path: Vec<String>,
    },
}

impl CallableOrigin {
    /// Replace the value at `path`; a conflicting replacement is unknown.
    fn merge_at(&mut self, path: &[String], value: Self) {
        if let Some((field, rest)) = path.split_first() {
            if let Self::Aggregate(fields) = self {
                fields
                    .entry(field.clone())
                    .or_insert(Self::Unknown)
                    .merge_at(rest, value);
            } else {
                *self = Self::Unknown;
            }
        } else if *self != value {
            *self = Self::Unknown;
        }
    }

    fn project(&self, field: &str) -> Self {
        match self {
            Self::Aggregate(fields) => fields.get(field).cloned().unwrap_or(Self::Unknown),
            Self::Binding { binding, path } => {
                let mut path = path.clone();
                path.push(field.to_string());
                Self::Binding {
                    binding: *binding,
                    path,
                }
            }
            _ => Self::Unknown,
        }
    }
}

#[derive(Debug)]
struct PendingForkTransfer {
    key: SpanKey,
    ty: Ty,
    origin: CallableOrigin,
    source_module: Option<String>,
    acquisition: crate::ClosureCaptureAcquisition,
}

/// A closure or named function flowing into a written callable type that
/// never suspends.
#[derive(Debug)]
struct SuspensionObligation {
    body: EffectBody,
    key: SpanKey,
    slot: String,
    source_module: Option<String>,
}

#[derive(Debug, Default)]
pub(super) struct EffectGraph {
    pub builtin_suspensions: HashSet<CallTarget>,
    pub current_body: Option<EffectBody>,
    /// Bodies and whether a construct inside them suspends on its own.
    pub bodies: HashMap<EffectBody, bool>,
    /// The construct or call that first made each body suspend.
    witnesses: HashMap<EffectBody, String>,
    calls: HashMap<SpanKey, Invocation>,
    submission_effects: HashMap<SpanKey, bool>,
    bindings: HashMap<TypeBindingId, CallableOrigin>,
    fork_transfers: Vec<PendingForkTransfer>,
    obligations: Vec<SuspensionObligation>,
}

impl Checker {
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
                    if let Some(ty) = self.record_fn_field_type(receiver, method) {
                        self.check_fork_transfer(&field, &branch.1, &ty);
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

    pub(super) fn check_fork_transfer(&mut self, expr: &Expr, span: &Span, ty: &Ty) {
        let ty = self.subst.resolve(ty);
        let origin = self.expression_callable_origin(expr, span);
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
            key,
            ty: ty.clone(),
            origin,
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

    /// Whether `span` is a checked actor delivery that suspends the sender.
    pub(super) fn submission_suspends(&self, span: &Span) -> bool {
        self.effect_graph
            .submission_effects
            .get(&SpanKey::in_module(span, self.current_module_idx))
            .copied()
            .unwrap_or(false)
    }

    /// A closure or named function coerced into a written callable type must
    /// not suspend unless that type says `fn[suspends]`. Bodies are known only
    /// after the fixed point, so the check is deferred to it.
    pub(super) fn record_suspension_obligations(
        &mut self,
        expected: &Ty,
        actual: &Ty,
        span: &Span,
    ) {
        match (&self.subst.resolve(expected), &self.subst.resolve(actual)) {
            (Ty::Function { capabilities, .. }, Ty::Closure { identity, .. }) => {
                if !capabilities.suspends {
                    self.effect_graph.obligations.push(SuspensionObligation {
                        body: identity.clone(),
                        key: SpanKey::in_module(span, self.current_module_idx),
                        slot: expected.user_facing().to_string(),
                        source_module: self.current_module.clone(),
                    });
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

    fn callee_value_type(&self, callee: &Spanned<Expr>) -> Option<Ty> {
        self.expr_types
            .get(&SpanKey::in_module(&callee.1, self.current_module_idx))
            .cloned()
            .or_else(|| match &callee.0 {
                Expr::Identifier(name) => {
                    self.env.lookup_ref(name).map(|binding| binding.ty.clone())
                }
                _ => None,
            })
    }

    /// The instantiated type of a callable record field selected as a method.
    fn record_fn_field_type(&self, receiver: &Spanned<Expr>, field: &str) -> Option<Ty> {
        let receiver_ty = self
            .expr_types
            .get(&SpanKey::in_module(&receiver.1, self.current_module_idx))?;
        let Ty::Named { name, args, .. } = self.subst.resolve(receiver_ty) else {
            return None;
        };
        let definition = self.lookup_type_def(&name)?;
        Some(Self::instantiate_type_def_member(
            definition.fields.get(field)?,
            &definition.type_params,
            &args,
        ))
    }

    fn expression_callable_origin(&self, expr: &Expr, span: &Span) -> CallableOrigin {
        let typed = |span: &Span| {
            self.expr_types
                .get(&SpanKey::in_module(span, self.current_module_idx))
                .map_or(CallableOrigin::Unknown, |ty| {
                    CallableOrigin::Typed(ty.clone())
                })
        };
        match expr {
            Expr::Identifier(name) => match self.env.lookup_ref(name) {
                Some(binding) if self.effect_graph.bindings.contains_key(&binding.id) => {
                    CallableOrigin::Binding {
                        binding: binding.id,
                        path: Vec::new(),
                    }
                }
                Some(binding) => CallableOrigin::Typed(binding.ty.clone()),
                None => typed(span),
            },
            Expr::Tuple(elements) => CallableOrigin::Aggregate(
                elements
                    .iter()
                    .enumerate()
                    .map(|(index, value)| {
                        (
                            index.to_string(),
                            self.expression_callable_origin(&value.0, &value.1),
                        )
                    })
                    .collect(),
            ),
            // A spread can carry callables whose origin is not visible here.
            Expr::StructInit {
                fields, base: None, ..
            } => CallableOrigin::Aggregate(
                fields
                    .iter()
                    .map(|(name, value)| {
                        (
                            name.clone(),
                            self.expression_callable_origin(&value.0, &value.1),
                        )
                    })
                    .collect(),
            ),
            Expr::FieldAccess { object, field } => self
                .expression_callable_origin(&object.0, &object.1)
                .project(field),
            _ => typed(span),
        }
    }

    pub(super) fn record_statement_effect_binding(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Assign { target, value, .. } => {
                let Some((root, path)) = self.expr_place(&target.0) else {
                    return;
                };
                let Some(binding) = self.env.lookup_ref(&root).map(|binding| binding.id) else {
                    return;
                };
                let origin = self.expression_callable_origin(&value.0, &value.1);
                self.effect_graph
                    .bindings
                    .entry(binding)
                    .or_insert(CallableOrigin::Unknown)
                    .merge_at(&path, origin);
            }
            Stmt::Let {
                pattern: (Pattern::Identifier(name), _),
                value: Some(value),
                ..
            }
            | Stmt::Var {
                name,
                value: Some(value),
                ..
            } => {
                let Some((binding, ty)) = self
                    .env
                    .lookup_ref(name)
                    .map(|binding| (binding.id, binding.ty.clone()))
                else {
                    return;
                };
                let origin = self.expression_callable_origin(&value.0, &value.1);
                if matches!(origin, CallableOrigin::Typed(_))
                    && !self.subst.resolve(&ty).contains_callable()
                {
                    return;
                }
                self.effect_graph
                    .bindings
                    .entry(binding)
                    .and_modify(|existing| existing.merge_at(&[], origin.clone()))
                    .or_insert(origin);
            }
            _ => {}
        }
    }

    fn origin_has_marker(
        &self,
        origin: &CallableOrigin,
        bindings: &HashMap<TypeBindingId, CallableOrigin>,
        seen: &mut HashSet<TypeBindingId>,
        marker: crate::traits::MarkerTrait,
    ) -> bool {
        match origin {
            CallableOrigin::Unknown => false,
            CallableOrigin::Typed(ty) => match self.subst.resolve(ty) {
                Ty::Closure {
                    identity: EffectBody::Closure(key),
                    ..
                } => self.closure_capture_facts[&key].iter().all(|capture| {
                    (if marker == crate::traits::MarkerTrait::Sync {
                        capture.is_sync
                    } else {
                        capture.is_send
                    }) || bindings.get(&capture.binding_id).is_some_and(|origin| {
                        self.origin_has_marker(origin, bindings, seen, marker)
                    })
                }),
                ty => self.registry.implements_marker(&ty, marker),
            },
            CallableOrigin::Aggregate(fields) => fields
                .values()
                .all(|origin| self.origin_has_marker(origin, bindings, seen, marker)),
            CallableOrigin::Binding { binding, path } => {
                if !seen.insert(*binding) {
                    return false;
                }
                let mut origin = bindings
                    .get(binding)
                    .cloned()
                    .unwrap_or(CallableOrigin::Unknown);
                for field in path {
                    origin = origin.project(field);
                }
                let proven = self.origin_has_marker(&origin, bindings, seen, marker);
                seen.remove(binding);
                proven
            }
        }
    }

    pub(super) fn record_expression_effect(&mut self, expr: &Expr, span: &Span) {
        let key = SpanKey::in_module(span, self.current_module_idx);
        let checked_invocation = self.direct_call_targets.contains_key(&key)
            || self.resolved_calls.contains_key(&key)
            || self.method_call_rewrites.contains_key(&key)
            || self.dyn_trait_method_calls.contains_key(&key)
            || self.actor_method_dispatch.contains_key(&key)
            || self.actor_delivery_calls.contains_key(&key);
        if checked_invocation
            && matches!(
                expr,
                Expr::Call { .. } | Expr::MethodCall { .. } | Expr::Send(_)
            )
        {
            let (callee, name) = match expr {
                Expr::Call { function, .. } => (
                    self.callee_value_type(function),
                    match &function.0 {
                        Expr::Identifier(name) => name.clone(),
                        Expr::FieldAccess { field, .. } => field.clone(),
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
                            self.record_fn_field_type(receiver, method)
                        }
                        _ if self.direct_call_targets.get(&key)
                            == Some(&CallTarget::IndirectFunctionValue) =>
                        {
                            self.record_fn_field_type(receiver, method)
                        }
                        _ => None,
                    };
                    (callee, method.clone())
                }
                _ => (None, "send".to_string()),
            };
            self.effect_graph.calls.insert(
                key,
                Invocation {
                    owner: self.effect_graph.current_body.clone(),
                    callee,
                    name: format!("{name}(...)"),
                    deferred: self.deferred_body.is_some(),
                    source_module: self.current_module.clone(),
                },
            );
        }
        self.record_intrinsic_suspension(expr);
    }

    /// Task joins and structured child teardown suspend independently of the
    /// child callable's own effect. Awaiting an ordinary call instead takes
    /// its effect from the invocation edge.
    fn record_intrinsic_suspension(&mut self, expr: &Expr) {
        let witness = match expr {
            Expr::Await(inner)
                if !matches!(
                    inner.0,
                    Expr::Call { .. } | Expr::MethodCall { .. } | Expr::Send(_)
                ) =>
            {
                "await"
            }
            Expr::AwaitRestart(_) => "await_restart",
            Expr::Join(_) => "join",
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
                    | MethodCallRewrite::StaticTraitDispatch { target, .. },
                ) => Some(target),
                _ => None,
            })
    }

    fn callee_suspends(&self, invocation: &Invocation, bodies: &HashMap<EffectBody, bool>) -> bool {
        match invocation.callee.as_ref().map(|ty| self.subst.resolve(ty)) {
            Some(Ty::Function { capabilities, .. }) => capabilities.suspends,
            Some(Ty::Closure { identity, .. }) => bodies.get(&identity).copied().unwrap_or(true),
            _ => true,
        }
    }

    fn invocation_suspends(
        &self,
        key: &SpanKey,
        invocation: &Invocation,
        graph: &EffectGraph,
        bodies: &HashMap<EffectBody, bool>,
    ) -> bool {
        if let Some(suspends) = graph.submission_effects.get(key) {
            return *suspends;
        }
        match self.call_target(key) {
            Some(target) if graph.builtin_suspensions.contains(target) => true,
            Some(CallTarget::IndirectFunctionValue) => self.callee_suspends(invocation, bodies),
            Some(
                CallTarget::User(id)
                | CallTarget::ImplMethod(id)
                | CallTarget::StaticTraitMethod { method: id, .. },
            ) => bodies
                .get(&EffectBody::Declaration(id.clone()))
                .copied()
                .unwrap_or(true),
            Some(CallTarget::Runtime(family)) => family.is_async_suspending().is_some(),
            Some(
                CallTarget::Extern { .. }
                | CallTarget::Builtin { .. }
                | CallTarget::RuntimeCollection(_),
            ) => false,
            Some(CallTarget::DynamicVtable { .. } | CallTarget::Unsupported { .. }) => true,
            None => match self.method_call_rewrites.get(key) {
                Some(
                    MethodCallRewrite::RecordFnFieldCall { .. }
                    | MethodCallRewrite::BuiltinVecHigherOrder { .. },
                ) => self.callee_suspends(invocation, bodies),
                Some(
                    MethodCallRewrite::RemoteActorAsk | MethodCallRewrite::GeneratorNext { .. },
                ) => true,
                Some(_) => false,
                None => !matches!(
                    self.actor_method_dispatch.get(key),
                    Some(super::ActorMethodKind::Message { .. })
                ),
            },
        }
    }

    fn finish_fork_transfers(&mut self, graph: &EffectGraph, output: &mut SuspensionEffects) {
        for PendingForkTransfer {
            key,
            ty,
            origin,
            source_module,
            acquisition,
        } in &graph.fork_transfers
        {
            let ty = self.subst.resolve(ty);
            let proven = |marker| {
                self.registry.implements_marker(&ty, marker)
                    || self.origin_has_marker(origin, &graph.bindings, &mut HashSet::new(), marker)
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
            if !is_send {
                let mut error = crate::error::TypeError::new(
                    crate::error::TypeErrorKind::InvalidSend,
                    key.start..key.end,
                    format!(
                        "fork cannot transfer `{}`: the value is not proven Send",
                        ty.user_facing()
                    ),
                );
                error.source_module.clone_from(source_module);
                self.errors.push(error);
            }
        }
    }

    pub(super) fn finish_suspension_effects(&mut self) -> SuspensionEffects {
        let graph = std::mem::take(&mut self.effect_graph);
        let mut bodies = graph.bodies.clone();
        let mut witnesses = graph.witnesses.clone();
        // Monotone least fixed point handles recursive declarations.
        loop {
            let mut changed = false;
            for (key, invocation) in &graph.calls {
                let Some(owner) = &invocation.owner else {
                    continue;
                };
                if bodies.get(owner).copied().unwrap_or(false) {
                    continue;
                }
                if self.invocation_suspends(key, invocation, &graph, &bodies) {
                    bodies.insert(owner.clone(), true);
                    witnesses.insert(owner.clone(), invocation.name.clone());
                    changed = true;
                }
            }
            if !changed {
                break;
            }
        }
        let mut output = SuspensionEffects::default();
        for (body, suspends) in &bodies {
            output
                .bodies
                .insert(body.clone(), SuspensionEffect::from_bool(*suspends));
        }
        for (key, invocation) in &graph.calls {
            let suspends = self.invocation_suspends(key, invocation, &graph, &bodies);
            output
                .calls
                .insert(key.clone(), SuspensionEffect::from_bool(suspends));
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
        for obligation in &graph.obligations {
            if !bodies.get(&obligation.body).copied().unwrap_or(true) {
                continue;
            }
            let subject = match &obligation.body {
                EffectBody::Declaration(id) => format!("function `{}`", id.display_name()),
                _ => "closure".to_string(),
            };
            let witness = witnesses
                .get(&obligation.body)
                .map_or("an unchecked body", String::as_str);
            let mut error = crate::error::TypeError::new(
                crate::error::TypeErrorKind::InvalidOperation,
                obligation.key.start..obligation.key.end,
                format!(
                    "{subject} suspends via `{witness}`; `{}` never suspends, write `fn[suspends]`",
                    obligation.slot
                ),
            );
            error.source_module.clone_from(&obligation.source_module);
            self.errors.push(error);
        }
        self.finish_fork_transfers(&graph, &mut output);
        output
    }
}
