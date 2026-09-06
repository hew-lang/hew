//! Suspension contracts inferred from checked declaration and callable identities.

use std::collections::{BTreeSet, HashMap, HashSet};

use hew_parser::ast::{Expr, Pattern, Span, Stmt};

use super::{CallTarget, Checker, SpanKey};
use crate::{env::TypeBindingId, DefId};

/// A callable body, independent of its linker spelling.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum EffectBody {
    Declaration(DefId),
    /// Also identifies the lifted body of a fork block.
    Closure(SpanKey),
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
    pub parameters: BTreeSet<usize>,
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
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum CallableOrigin {
    Unknown,
    Synchronous,
    Suspending,
    Parameter { owner: EffectBody, index: usize },
    Body(EffectBody),
    Target(CallTarget),
}

#[derive(Debug, Clone)]
struct Invocation {
    owner: Option<EffectBody>,
    origin: Option<CallableOrigin>,
    arguments: Vec<Option<CallableOrigin>>,
    explicit: bool,
    deferred: bool,
    source_module: Option<String>,
}

#[derive(Debug, Default)]
pub(super) struct EffectGraph {
    pub builtin_suspensions: HashSet<CallTarget>,
    pub current_body: Option<EffectBody>,
    pub bodies: HashMap<EffectBody, bool>,
    calls: HashMap<SpanKey, Invocation>,
    values: HashMap<SpanKey, CallableOrigin>,
    bindings: HashMap<TypeBindingId, CallableOrigin>,
    submission_effects: HashMap<SpanKey, bool>,
}

impl Checker {
    pub(super) fn record_callable_parameter(&mut self, name: &str, index: usize) {
        let Some(owner) = self.effect_graph.current_body.clone() else {
            return;
        };
        let Some(binding) = self.env.lookup_ref(name) else {
            return;
        };
        if matches!(
            self.subst.resolve(&binding.ty),
            crate::Ty::Function { .. } | crate::Ty::Closure { .. }
        ) {
            self.effect_graph
                .bindings
                .insert(binding.id, CallableOrigin::Parameter { owner, index });
        }
    }

    fn expression_callable_origin(&self, expr: &Expr, span: &Span) -> Option<CallableOrigin> {
        match expr {
            Expr::Identifier(name) => self
                .env
                .lookup_ref(name)
                .and_then(|binding| self.effect_graph.bindings.get(&binding.id))
                .cloned(),
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

    pub(super) fn record_expression_effect(&mut self, expr: &Expr, span: &Span) {
        let key = SpanKey::in_module(span, self.current_module_idx);
        let origin = match expr {
            Expr::Identifier(name) => self
                .env
                .lookup_ref(name)
                .and_then(|binding| self.effect_graph.bindings.get(&binding.id))
                .cloned(),
            Expr::Lambda { .. } | Expr::ForkBlock { .. } => {
                Some(CallableOrigin::Body(EffectBody::Closure(key.clone())))
            }
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
        });
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
                _ => None,
            };
            let arguments = match expr {
                Expr::Call { args, .. } | Expr::MethodCall { args, .. } => args
                    .iter()
                    .map(|arg| {
                        let (expr, span) = arg.expr();
                        self.expression_callable_origin(expr, span)
                    })
                    .collect(),
                _ => Vec::new(),
            };
            self.effect_graph.calls.insert(
                key.clone(),
                Invocation {
                    owner: self.effect_graph.current_body.clone(),
                    origin,
                    arguments,
                    explicit: self.suspension_operands.contains(&key),
                    deferred: self.deferred_body.is_some(),
                    source_module: self.current_module.clone(),
                },
            );
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
            Stmt::Assign { target, value, .. } => match &target.0 {
                Expr::Identifier(name) => Some((name, value)),
                _ => None,
            },
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

    #[expect(
        clippy::too_many_lines,
        reason = "fixed point and source diagnostics share invocation contracts"
    )]
    pub(super) fn finish_suspension_effects(&mut self) -> SuspensionEffects {
        let graph = std::mem::take(&mut self.effect_graph);
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
                    None if self.method_call_rewrites.contains_key(key) => {
                        Some(CallableOrigin::Synchronous)
                    }
                    Some(CallTarget::IndirectFunctionValue) | None => invocation.origin.clone(),
                    Some(target) if graph.builtin_suspensions.contains(target) => {
                        Some(CallableOrigin::Suspending)
                    }
                    Some(target) => Some(CallableOrigin::Target(target.clone())),
                };
                (key.clone(), origin)
            })
            .collect();
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
        for (key, invocation) in graph.calls {
            let contract = graph.submission_effects.get(&key).map_or_else(
                || {
                    invocation_contract(
                        targets[&key].as_ref(),
                        &invocation.arguments,
                        invocation.owner.as_ref(),
                        &contracts,
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
                error.source_module = invocation.source_module;
                self.errors.push(error);
            }
        }
        output.contracts = contracts;
        output
    }
}

fn invocation_contract(
    origin: Option<&CallableOrigin>,
    arguments: &[Option<CallableOrigin>],
    owner: Option<&EffectBody>,
    contracts: &HashMap<EffectBody, SuspensionContract>,
) -> SuspensionContract {
    let body = match origin {
        Some(CallableOrigin::Parameter {
            owner: parameter_owner,
            index,
        }) => {
            return if owner == Some(parameter_owner) {
                SuspensionContract {
                    intrinsic: false,
                    parameters: BTreeSet::from([*index]),
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
    for index in &callee.parameters {
        result.merge(invocation_contract(
            arguments.get(*index).and_then(Option::as_ref),
            &[],
            owner,
            contracts,
        ));
    }
    result
}
