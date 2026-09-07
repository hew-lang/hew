//! Suspension contracts inferred from checked declaration and callable identities.

use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};

use hew_parser::ast::{Expr, Pattern, Span, Spanned, Stmt};

use super::{CallTarget, Checker, SpanKey};
use crate::{env::TypeBindingId, DefId};

/// A callable body, independent of its linker spelling.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum EffectBody {
    Declaration(DefId),
    /// Deferred execution of a named generator, separate from its creator.
    Generator(DefId),
    /// Deferred execution of a generator block.
    GeneratorBlock(SpanKey),
    /// Also identifies the lifted body of a fork block.
    Closure(SpanKey),
}

/// A projection selected against a checked receiver type.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum CallableProjection {
    Field { declaration: DefId, index: usize },
    TupleIndex(usize),
}

/// A callable reached through one function parameter.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct CallableParameter {
    pub index: usize,
    pub projection: Vec<CallableProjection>,
}

/// The checker-owned contract consumed by lowering.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub enum SuspensionEffect {
    #[default]
    Never,
    MaySuspend,
}

/// Complete body summaries and instantiated invocation verdicts.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct SuspensionContract {
    pub intrinsic: bool,
    pub parameters: BTreeSet<CallableParameter>,
}

impl SuspensionContract {
    fn unknown() -> Self {
        Self {
            intrinsic: true,
            parameters: BTreeSet::new(),
        }
    }
    fn merge(&mut self, other: Self) {
        self.intrinsic |= other.intrinsic;
        self.parameters.extend(other.parameters);
    }
    fn effect(&self) -> SuspensionEffect {
        if self.intrinsic || !self.parameters.is_empty() {
            SuspensionEffect::MaySuspend
        } else {
            SuspensionEffect::Never
        }
    }
}

/// Complete body summaries and invocation verdicts with callable-parameter dependencies.
#[derive(Debug, Clone, Default)]
pub struct SuspensionEffects {
    pub contracts: HashMap<EffectBody, SuspensionContract>,
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

#[derive(Debug, Clone, PartialEq, Eq)]
enum CallableOrigin {
    Unknown,
    Typed(crate::Ty),
    Synchronous,
    Suspending,
    Parameter {
        owner: EffectBody,
        parameter: CallableParameter,
    },
    Aggregate(BTreeMap<CallableProjection, CallableOrigin>),
    Binding {
        binding: TypeBindingId,
        projection: Vec<CallableProjection>,
    },
    Body(EffectBody),
    Target(CallTarget),
}

impl CallableOrigin {
    fn merge_at(&mut self, path: &[CallableProjection], value: Self) {
        if let Some((projection, rest)) = path.split_first() {
            if let Self::Aggregate(fields) = self {
                fields
                    .entry(projection.clone())
                    .or_insert(Self::Unknown)
                    .merge_at(rest, value);
            } else {
                *self = Self::Unknown;
            }
        } else if *self != value {
            *self = Self::Unknown;
        }
    }

    fn project(&self, projection: &CallableProjection) -> Option<Self> {
        match self {
            Self::Aggregate(fields) => fields.get(projection).cloned(),
            Self::Binding {
                binding,
                projection: path,
            } => {
                let mut path = path.clone();
                path.push(projection.clone());
                Some(Self::Binding {
                    binding: *binding,
                    projection: path,
                })
            }
            Self::Parameter { owner, parameter } => {
                let mut parameter = parameter.clone();
                parameter.projection.push(projection.clone());
                Some(Self::Parameter {
                    owner: owner.clone(),
                    parameter,
                })
            }
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
struct Invocation {
    owner: Option<EffectBody>,
    origin: Option<CallableOrigin>,
    arguments: Vec<Option<CallableOrigin>>,
    argument_names: Vec<Option<String>>,
    explicit: bool,
    deferred: bool,
    source_module: Option<String>,
}

#[derive(Debug)]
struct PendingForkTransfer {
    key: SpanKey,
    ty: crate::Ty,
    origin: Option<CallableOrigin>,
    source_module: Option<String>,
    acquisition: crate::ClosureCaptureAcquisition,
}

#[derive(Debug, Default)]
pub(super) struct EffectGraph {
    pub builtin_suspensions: HashSet<CallTarget>,
    pub current_body: Option<EffectBody>,
    pub bodies: HashMap<EffectBody, bool>,
    pub parameter_names: HashMap<EffectBody, Vec<String>>,
    calls: HashMap<SpanKey, Invocation>,
    values: HashMap<SpanKey, CallableOrigin>,
    bindings: HashMap<TypeBindingId, CallableOrigin>,
    submission_effects: HashMap<SpanKey, bool>,
    fork_transfers: Vec<PendingForkTransfer>,
}

impl Checker {
    /// Record owning task inputs after invocation checking has resolved their
    /// types and consumption. Rechecking expressions here would repeat moves.
    pub(super) fn record_fork_call_inputs(&mut self, branch: &Spanned<Expr>) {
        match &branch.0 {
            Expr::Call { function, args, .. } => {
                if let Some(ty) = self
                    .expr_types
                    .get(&SpanKey::in_module(&function.1, self.current_module_idx))
                    .cloned()
                    .or_else(|| match &function.0 {
                        Expr::Identifier(name) => {
                            self.env.lookup_ref(name).map(|binding| binding.ty.clone())
                        }
                        _ => None,
                    })
                {
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
                if let Some(super::MethodCallRewrite::RecordFnFieldCall { field_ty }) =
                    self.method_call_rewrites.get(&key).cloned()
                {
                    let field = Expr::FieldAccess {
                        object: receiver.clone(),
                        field: method.clone(),
                    };
                    self.check_fork_transfer(&field, &branch.1, &field_ty.to_ty());
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

    pub(super) fn check_fork_transfer(&mut self, expr: &Expr, span: &Span, ty: &crate::Ty) {
        let ty = self.subst.resolve(ty);
        let origin = self.infer_expression_callable_origin(expr, span);
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
            && !matches!(ty, crate::Ty::Borrow { .. })
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
        if matches!(ty, crate::Ty::Borrow { .. }) {
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

    pub(super) fn record_callable_parameter(&mut self, name: &str, index: usize) {
        let Some(owner) = self.effect_graph.current_body.clone() else {
            return;
        };
        let Some(binding) = self.env.lookup_ref(name) else {
            return;
        };
        self.effect_graph.bindings.insert(
            binding.id,
            CallableOrigin::Parameter {
                owner,
                parameter: CallableParameter {
                    index,
                    projection: Vec::new(),
                },
            },
        );
    }

    fn checked_callable_projection(
        &self,
        ty: &crate::Ty,
        field: &str,
    ) -> Option<CallableProjection> {
        match self.subst.resolve(ty) {
            crate::Ty::Tuple(elements) => {
                let index: usize = field.parse().ok()?;
                (index < elements.len()).then_some(CallableProjection::TupleIndex(index))
            }
            crate::Ty::Named { name, .. } => {
                let declaration = self.identity.declaration_by_path(&name)?.clone();
                let index = self
                    .type_defs
                    .get(&name)?
                    .field_order
                    .iter()
                    .position(|candidate| candidate == field)?;
                Some(CallableProjection::Field { declaration, index })
            }
            _ => None,
        }
    }

    fn checked_callable_place(
        &self,
        expr: &Expr,
    ) -> Option<(TypeBindingId, Vec<CallableProjection>)> {
        let (name, fields) = self.expr_place(expr)?;
        let binding = self.env.lookup_ref(&name)?;
        let mut ty = self.subst.resolve(&binding.ty);
        let mut path = Vec::new();
        for field in fields {
            path.push(self.checked_callable_projection(&ty, &field)?);
            ty = match ty {
                crate::Ty::Tuple(elements) => elements.get(field.parse::<usize>().ok()?)?.clone(),
                crate::Ty::Named { name, args, .. } => {
                    let definition = self.type_defs.get(&name)?;
                    Self::instantiate_type_def_member(
                        definition.fields.get(&field)?,
                        &definition.type_params,
                        &args,
                    )
                }
                _ => return None,
            };
            ty = self.subst.resolve(&ty);
        }
        Some((binding.id, path))
    }

    fn projected_callable_origin(
        &self,
        object: &Spanned<Expr>,
        field: &str,
    ) -> Option<CallableOrigin> {
        let ty = self
            .expr_types
            .get(&SpanKey::in_module(&object.1, self.current_module_idx))?;
        let projection = self.checked_callable_projection(ty, field)?;
        self.expression_callable_origin(&object.0, &object.1)?
            .project(&projection)
    }

    fn expression_callable_origin(&self, expr: &Expr, span: &Span) -> Option<CallableOrigin> {
        match expr {
            Expr::Identifier(name) => self
                .env
                .lookup_ref(name)
                .filter(|binding| self.effect_graph.bindings.contains_key(&binding.id))
                .map(|binding| CallableOrigin::Binding {
                    binding: binding.id,
                    projection: Vec::new(),
                }),
            _ => None,
        }
        .or_else(|| {
            self.effect_graph
                .values
                .get(&SpanKey::in_module(span, self.current_module_idx))
                .cloned()
        })
    }

    /// Consume the actor policy checker's immutable submission verdict.
    pub(super) fn record_submission_suspension(&mut self, span: &Span, may_suspend: bool) {
        let key = SpanKey::in_module(span, self.current_module_idx);
        self.suspension_operands.insert(key.clone());
        self.effect_graph
            .submission_effects
            .insert(key, may_suspend);
        if may_suspend {
            if let Some(owner) = &self.effect_graph.current_body {
                self.effect_graph.bodies.insert(owner.clone(), true);
            }
        }
    }

    fn infer_expression_callable_origin(&self, expr: &Expr, span: &Span) -> Option<CallableOrigin> {
        let key = SpanKey::in_module(span, self.current_module_idx);
        match expr {
            Expr::Identifier(name) => self
                .env
                .lookup_ref(name)
                .filter(|binding| self.effect_graph.bindings.contains_key(&binding.id))
                .map(|binding| CallableOrigin::Binding {
                    binding: binding.id,
                    projection: Vec::new(),
                }),
            Expr::Lambda { .. } | Expr::ForkBlock { .. } => {
                Some(CallableOrigin::Body(EffectBody::Closure(key.clone())))
            }
            Expr::Tuple(elements) => Some(CallableOrigin::Aggregate(
                elements
                    .iter()
                    .enumerate()
                    .map(|(index, value)| {
                        let origin = self
                            .expression_callable_origin(&value.0, &value.1)
                            .unwrap_or_else(|| {
                                CallableOrigin::Typed(
                                    self.expr_types
                                        .get(&SpanKey::in_module(&value.1, self.current_module_idx))
                                        .cloned()
                                        .unwrap_or(crate::Ty::Error),
                                )
                            });
                        (CallableProjection::TupleIndex(index), origin)
                    })
                    .collect(),
            )),
            Expr::StructInit { fields, base, .. } => {
                // A spread can contain fields whose callable or transfer facts
                // are not available at this expression. Keep that uncertainty.
                if base.is_some() {
                    return None;
                }
                let mut entries = BTreeMap::new();
                if let Some(ty) = self.expr_types.get(&key) {
                    for (name, value) in fields {
                        if let Some(projection) = self.checked_callable_projection(ty, name) {
                            entries.insert(
                                projection,
                                self.expression_callable_origin(&value.0, &value.1)
                                    .unwrap_or_else(|| {
                                        CallableOrigin::Typed(
                                            self.expr_types
                                                .get(&SpanKey::in_module(
                                                    &value.1,
                                                    self.current_module_idx,
                                                ))
                                                .cloned()
                                                .unwrap_or(crate::Ty::Error),
                                        )
                                    }),
                            );
                        }
                    }
                }
                Some(CallableOrigin::Aggregate(entries))
            }
            Expr::FieldAccess { object, field } => self.projected_callable_origin(object, field),
            _ => None,
        }
        .or_else(|| {
            (!matches!(
                expr,
                Expr::Call { .. } | Expr::MethodCall { .. } | Expr::Send(_)
            ))
            .then(|| self.direct_call_targets.get(&key))
            .flatten()
            .cloned()
            .map(CallableOrigin::Target)
        })
    }

    #[expect(
        clippy::too_many_lines,
        reason = "checked invocation and mutation effects share expression identity"
    )]
    pub(super) fn record_expression_effect(&mut self, expr: &Expr, span: &Span) {
        let key = SpanKey::in_module(span, self.current_module_idx);
        let origin = self.infer_expression_callable_origin(expr, span);
        if let Some(origin) = origin {
            self.effect_graph.values.insert(key.clone(), origin);
        }
        let checked_invocation = self.direct_call_targets.contains_key(&key)
            || self.resolved_calls.contains_key(&key)
            || self.method_call_rewrites.contains_key(&key)
            || self.dyn_trait_method_calls.contains_key(&key)
            || self.actor_method_dispatch.contains_key(&key)
            || self.actor_delivery_calls.contains_key(&key);
        if matches!(
            expr,
            Expr::Call { .. } | Expr::MethodCall { .. } | Expr::Send(_)
        ) && checked_invocation
        {
            let origin = match expr {
                Expr::Call { function, .. } => {
                    self.expression_callable_origin(&function.0, &function.1)
                }
                Expr::MethodCall {
                    receiver, method, ..
                } => self.projected_callable_origin(receiver, method),
                _ => None,
            };
            let mut arguments: Vec<_> = match expr {
                Expr::Call { args, .. } | Expr::MethodCall { args, .. } => args
                    .iter()
                    .map(|arg| {
                        let (expr, span) = arg.expr();
                        self.expression_callable_origin(expr, span)
                    })
                    .collect(),
                _ => Vec::new(),
            };
            let mut argument_names: Vec<_> = match expr {
                Expr::Call { args, .. } | Expr::MethodCall { args, .. } => args
                    .iter()
                    .map(|arg| arg.name().map(str::to_string))
                    .collect(),
                _ => Vec::new(),
            };
            if let Expr::MethodCall { receiver, .. } = expr {
                let has_receiver = matches!(
                    self.method_call_rewrites.get(&key),
                    Some(
                        super::MethodCallRewrite::RewriteToFunction { .. }
                            | super::MethodCallRewrite::StaticTraitDispatch { .. }
                    )
                ) || self.resolved_calls.get(&key).is_some_and(|call| {
                    matches!(call.target, CallTarget::User(_) | CallTarget::ImplMethod(_))
                });
                if has_receiver {
                    arguments.insert(0, self.expression_callable_origin(&receiver.0, &receiver.1));
                    argument_names.insert(0, None);
                }
            }
            self.effect_graph.calls.insert(
                key.clone(),
                Invocation {
                    owner: self.effect_graph.current_body.clone(),
                    origin,
                    arguments,
                    argument_names,
                    explicit: self.suspension_operands.contains(&key),
                    deferred: self.deferred_body.is_some(),
                    source_module: self.current_module.clone(),
                },
            );
        }
        if let Expr::MethodCall { receiver, .. } = expr {
            let mutates_receiver = matches!(
                self.method_call_rewrites.get(&key),
                Some(
                    super::MethodCallRewrite::RewriteToFunction {
                        requires_mutable_receiver: true,
                        ..
                    } | super::MethodCallRewrite::StaticTraitDispatch {
                        requires_mutable_receiver: true,
                        ..
                    }
                )
            );
            if mutates_receiver {
                if let Some((binding, path)) = self.checked_callable_place(&receiver.0) {
                    if let Some(origin) = self.effect_graph.bindings.get_mut(&binding) {
                        origin.merge_at(&path, CallableOrigin::Unknown);
                    }
                }
            }
        }
        // Task joins and structured child teardown suspend independently of
        // the child callable's own effect. Awaiting an ordinary call instead
        // takes its effect from the invocation edge below.
        let intrinsic = match expr {
            Expr::Await(inner) => !matches!(
                inner.0,
                Expr::Call { .. } | Expr::MethodCall { .. } | Expr::Send(_)
            ),
            Expr::AwaitRestart(_)
            | Expr::Join(_)
            | Expr::Select { .. }
            | Expr::ForkChild { .. }
            | Expr::ForkBlock { .. }
            | Expr::ScopeDeadline { .. } => true,
            _ => false,
        };
        if intrinsic {
            if let Some(owner) = &self.effect_graph.current_body {
                self.effect_graph.bodies.insert(owner.clone(), true);
            }
        }
    }

    pub(super) fn record_statement_effect_binding(&mut self, stmt: &Stmt) {
        if let Stmt::Assign { target, value, .. } = stmt {
            if let Some((binding, path)) = self.checked_callable_place(&target.0) {
                let origin = self
                    .expression_callable_origin(&value.0, &value.1)
                    .unwrap_or(CallableOrigin::Unknown);
                if let Some(existing) = self.effect_graph.bindings.get_mut(&binding) {
                    existing.merge_at(&path, origin);
                } else {
                    self.effect_graph
                        .bindings
                        .insert(binding, CallableOrigin::Unknown);
                }
            }
            return;
        }

        let pair = match stmt {
            Stmt::Let {
                pattern: (Pattern::Identifier(name), _),
                value: Some(value),
                ..
            }
            | Stmt::Var {
                name,
                value: Some(value),
                ..
            } => Some((name, value)),
            _ => None,
        };
        let Some((name, value)) = pair else { return };
        let Some(binding) = self.env.lookup_ref(name) else {
            return;
        };
        let key = SpanKey::in_module(&value.1, self.current_module_idx);
        if let Some(origin) = self.effect_graph.values.get(&key).cloned() {
            self.effect_graph
                .bindings
                .entry(binding.id)
                .and_modify(|existing| {
                    if *existing != origin {
                        *existing = CallableOrigin::Unknown;
                    }
                })
                .or_insert(origin);
        } else {
            self.effect_graph.bindings.remove(&binding.id);
        }
    }

    fn callable_origin_has_marker(
        &self,
        origin: Option<&CallableOrigin>,
        graph: &EffectGraph,
        seen: &mut HashSet<EffectBody>,
        marker: crate::traits::MarkerTrait,
    ) -> bool {
        let origin = resolve_binding_origin(origin, &graph.bindings, &mut HashSet::new());
        match origin {
            Some(CallableOrigin::Typed(ty)) => self
                .registry
                .implements_marker(&self.subst.resolve(&ty), marker),
            Some(CallableOrigin::Body(body @ EffectBody::Closure(_))) => {
                if !seen.insert(body.clone()) {
                    return false;
                }
                let EffectBody::Closure(key) = &body else {
                    unreachable!()
                };
                let send = self.closure_capture_facts.get(key).is_some_and(|captures| {
                    captures.iter().all(|capture| {
                        (if marker == crate::traits::MarkerTrait::Sync {
                            capture.is_sync
                        } else {
                            capture.is_send
                        }) || self.callable_origin_has_marker(
                            graph.bindings.get(&capture.binding_id),
                            graph,
                            seen,
                            marker,
                        )
                    })
                });
                seen.remove(&body);
                send
            }
            Some(
                CallableOrigin::Body(EffectBody::Declaration(_))
                | CallableOrigin::Target(
                    CallTarget::User(_)
                    | CallTarget::ImplMethod(_)
                    | CallTarget::Extern { .. }
                    | CallTarget::Builtin { .. }
                    | CallTarget::Runtime(_),
                ),
            ) => true,
            Some(CallableOrigin::Aggregate(fields)) => fields
                .values()
                .all(|origin| self.callable_origin_has_marker(Some(origin), graph, seen, marker)),
            _ => false,
        }
    }

    #[expect(
        clippy::too_many_lines,
        reason = "fixed point and source diagnostics share invocation contracts"
    )]
    pub(super) fn finish_suspension_effects(&mut self) -> SuspensionEffects {
        let mut graph = std::mem::take(&mut self.effect_graph);
        let mut contracts: HashMap<_, _> = graph
            .bodies
            .iter()
            .map(|(body, intrinsic)| {
                (
                    body.clone(),
                    SuspensionContract {
                        intrinsic: *intrinsic,
                        parameters: BTreeSet::new(),
                    },
                )
            })
            .collect();
        let targets: HashMap<_, _> = graph
            .calls
            .iter()
            .map(|(key, invocation)| {
                let target = self
                    .direct_call_targets
                    .get(key)
                    .or_else(|| self.resolved_calls.get(key).map(|call| &call.target))
                    .or_else(|| {
                        self.dyn_trait_method_calls
                            .get(key)
                            .map(|call| &call.target)
                    })
                    .or_else(|| match self.method_call_rewrites.get(key) {
                        Some(
                            super::MethodCallRewrite::RewriteToFunction { target, .. }
                            | super::MethodCallRewrite::RewriteModuleQualifiedToFunction {
                                target,
                                ..
                            },
                        ) => Some(target),
                        _ => None,
                    });
                let origin = match target {
                    Some(CallTarget::IndirectFunctionValue) => invocation.origin.clone(),
                    Some(target) if graph.builtin_suspensions.contains(target) => {
                        Some(CallableOrigin::Suspending)
                    }
                    Some(target) => Some(CallableOrigin::Target(target.clone())),
                    None => self.method_call_rewrites.get(key).map_or_else(
                        || invocation.origin.clone(),
                        |rewrite| rewrite_callable_origin(rewrite, invocation),
                    ),
                };
                (key.clone(), origin)
            })
            .collect();
        for (key, invocation) in &mut graph.calls {
            if !invocation.argument_names.iter().any(Option::is_some) {
                continue;
            }
            let body = match targets[key].as_ref() {
                Some(CallableOrigin::Body(body)) => Some(body.clone()),
                Some(CallableOrigin::Target(CallTarget::User(id) | CallTarget::ImplMethod(id))) => {
                    Some(EffectBody::Declaration(id.clone()))
                }
                _ => None,
            };
            if let Some(names) = body
                .as_ref()
                .and_then(|body| graph.parameter_names.get(body))
            {
                let mut arguments = vec![None; names.len()];
                let mut positional = 0;
                for (argument, name) in invocation.arguments.iter().zip(&invocation.argument_names)
                {
                    let index = name
                        .as_ref()
                        .and_then(|name| names.iter().position(|candidate| candidate == name))
                        .unwrap_or_else(|| {
                            let index = positional;
                            positional += 1;
                            index
                        });
                    if let Some(slot) = arguments.get_mut(index) {
                        slot.clone_from(argument);
                    }
                }
                invocation.arguments = arguments;
            }
        }
        // Monotone least fixed point handles recursive declarations and
        // substitutes callable argument effects at each checked invocation.
        loop {
            let mut changed = false;
            for (key, invocation) in &graph.calls {
                let contract = graph.submission_effects.get(key).map_or_else(
                    || {
                        invocation_contract(
                            targets[key].as_ref(),
                            &invocation.arguments,
                            invocation.owner.as_ref(),
                            &contracts,
                            &graph.bindings,
                        )
                    },
                    |intrinsic| SuspensionContract {
                        intrinsic: *intrinsic,
                        parameters: BTreeSet::new(),
                    },
                );
                if let Some(owner) = &invocation.owner {
                    let summary = contracts.entry(owner.clone()).or_default();
                    let before = summary.clone();
                    summary.merge(contract);
                    changed |= *summary != before;
                }
            }
            if !changed {
                break;
            }
        }
        let mut output = SuspensionEffects::default();
        for (body, contract) in &contracts {
            output.bodies.insert(body.clone(), contract.effect());
        }
        for (key, invocation) in &graph.calls {
            let contract = graph.submission_effects.get(key).map_or_else(
                || {
                    invocation_contract(
                        targets[key].as_ref(),
                        &invocation.arguments,
                        invocation.owner.as_ref(),
                        &contracts,
                        &graph.bindings,
                    )
                },
                |intrinsic| SuspensionContract {
                    intrinsic: *intrinsic,
                    parameters: BTreeSet::new(),
                },
            );
            let may_suspend = contract.effect() == SuspensionEffect::MaySuspend;
            output.calls.insert(key.clone(), contract.effect());
            if may_suspend && (!invocation.explicit || invocation.deferred) {
                let message = if invocation.deferred {
                    "a deferred body cannot call a suspending function"
                } else {
                    "this call may suspend; use await or fork on this call"
                };
                let mut error = crate::error::TypeError::new(
                    crate::error::TypeErrorKind::InvalidOperation,
                    key.start..key.end,
                    message,
                );
                error.source_module.clone_from(&invocation.source_module);
                self.errors.push(error);
            }
        }
        for PendingForkTransfer {
            key,
            ty,
            origin,
            source_module,
            acquisition,
        } in &graph.fork_transfers
        {
            let is_send = self
                .registry
                .implements_marker(&self.subst.resolve(ty), crate::traits::MarkerTrait::Send)
                || self.callable_origin_has_marker(
                    origin.as_ref(),
                    &graph,
                    &mut HashSet::new(),
                    crate::traits::MarkerTrait::Send,
                );
            let is_sync = self
                .registry
                .implements_marker(&self.subst.resolve(ty), crate::traits::MarkerTrait::Sync)
                || self.callable_origin_has_marker(
                    origin.as_ref(),
                    &graph,
                    &mut HashSet::new(),
                    crate::traits::MarkerTrait::Sync,
                );
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
        output.contracts = contracts;
        output
    }
}

fn rewrite_callable_origin(
    rewrite: &super::MethodCallRewrite,
    invocation: &Invocation,
) -> Option<CallableOrigin> {
    use super::MethodCallRewrite as R;
    match rewrite {
        R::RewriteToFunction { target, .. }
        | R::RewriteModuleQualifiedToFunction { target, .. }
        | R::StaticTraitDispatch { target, .. } => Some(CallableOrigin::Target(target.clone())),
        R::RecordFnFieldCall { .. } => invocation.origin.clone(),
        R::BuiltinVecHigherOrder { .. } => invocation.arguments.first().cloned().flatten(),
        R::RemoteActorAsk => Some(CallableOrigin::Suspending),
        R::GeneratorNext { .. } => Some(CallableOrigin::Unknown),
        R::RcIntrinsic { .. }
        | R::GenericMathIntrinsic { .. }
        | R::DeferToLowering
        | R::BuiltinOptionResult { .. }
        | R::CancellationTokenIsCancelled
        | R::BuiltinVecIntoIter
        | R::BuiltinVecIter
        | R::BuiltinHashMapIntoIter { .. }
        | R::BuiltinVecIterNext
        | R::VecFrom
        | R::WireCodec { .. }
        | R::GenericWireCodec { .. }
        | R::RecordCloneInplace { .. }
        | R::CopyCloneNoop => Some(CallableOrigin::Synchronous),
    }
}

fn resolve_binding_origin(
    origin: Option<&CallableOrigin>,
    bindings: &HashMap<TypeBindingId, CallableOrigin>,
    seen: &mut HashSet<TypeBindingId>,
) -> Option<CallableOrigin> {
    let origin = origin?;
    if let CallableOrigin::Binding {
        binding,
        projection,
    } = origin
    {
        if !seen.insert(*binding) {
            return Some(CallableOrigin::Unknown);
        }
        let mut value = resolve_binding_origin(bindings.get(binding), bindings, seen)?;
        for step in projection {
            value = resolve_binding_origin(value.project(step).as_ref(), bindings, seen)?;
        }
        Some(value)
    } else {
        Some(origin.clone())
    }
}

fn invocation_contract(
    origin: Option<&CallableOrigin>,
    arguments: &[Option<CallableOrigin>],
    owner: Option<&EffectBody>,
    contracts: &HashMap<EffectBody, SuspensionContract>,
    bindings: &HashMap<TypeBindingId, CallableOrigin>,
) -> SuspensionContract {
    let resolved = resolve_binding_origin(origin, bindings, &mut HashSet::new());
    let origin = resolved.as_ref();
    let body = match origin {
        Some(CallableOrigin::Parameter {
            owner: parameter_owner,
            parameter,
        }) => {
            return if owner == Some(parameter_owner) {
                SuspensionContract {
                    intrinsic: false,
                    parameters: BTreeSet::from([parameter.clone()]),
                }
            } else {
                SuspensionContract::unknown()
            };
        }
        Some(CallableOrigin::Body(body)) => body.clone(),
        Some(CallableOrigin::Target(CallTarget::User(id) | CallTarget::ImplMethod(id))) => {
            EffectBody::Declaration(id.clone())
        }
        Some(CallableOrigin::Target(CallTarget::Runtime(family))) => {
            return SuspensionContract {
                intrinsic: family.is_async_suspending().is_some()
                    || matches!(
                        family,
                        crate::RuntimeCallFamily::FileRead(
                            crate::runtime_call::FileReadOp::Open
                                | crate::runtime_call::FileReadOp::StreamOpen
                                | crate::runtime_call::FileReadOp::Collect
                                | crate::runtime_call::FileReadOp::StreamCollect
                        ) | crate::RuntimeCallFamily::Tcp(
                            crate::runtime_call::TcpOp::Connect
                                | crate::runtime_call::TcpOp::ConnectTimeout
                        )
                    ),
                parameters: BTreeSet::new(),
            }
        }
        Some(
            CallableOrigin::Synchronous
            | CallableOrigin::Target(
                CallTarget::Extern { .. }
                | CallTarget::Builtin { .. }
                | CallTarget::RuntimeCollection(_),
            ),
        ) => return SuspensionContract::default(),
        _ => return SuspensionContract::unknown(),
    };
    let Some(callee) = contracts.get(&body) else {
        return SuspensionContract::unknown();
    };
    let mut result = SuspensionContract {
        intrinsic: callee.intrinsic,
        parameters: BTreeSet::new(),
    };
    for parameter in &callee.parameters {
        let mut argument = arguments
            .get(parameter.index)
            .and_then(Option::as_ref)
            .cloned();
        for projection in &parameter.projection {
            argument = argument.and_then(|origin| origin.project(projection));
        }
        result.merge(invocation_contract(
            argument.as_ref(),
            &[],
            owner,
            contracts,
            bindings,
        ));
    }
    result
}
