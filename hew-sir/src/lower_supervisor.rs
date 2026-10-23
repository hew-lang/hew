//! Demanded supervisor declarations and their boundaries.
//!
//! Each declared child gets a spawn callable `fn(supervisor, config...) -> handle` whose
//! body is the child's `spawn` with the declared init arguments. The
//! supervisor runs it for the initial spawn and for every restart, so a
//! restarted child re-runs init and start against the same config.

use super::{
    function_source_origin, lower_initial_value_transfer, Builder, CallableId, CallableInstance,
    CallableState, HirBlock, HirExpr, HirExprKind, HirFn, HirItem, HirModule, InstanceService,
    IntentKind, OwnKind, OwnedBindingUse, Provenance, ResolvedTy, SemAbiParam, SemCallConv,
    SemCallable, SemCallableKind, SemOpKind, SemParamPassing, SemSignature, TypeSubstitution,
    ValueId,
};
use crate::{
    ActorOperation, SemRestartPolicy, SemRestartStrategy, SemStopDeadline, SemSupervisedRole,
    SemSupervisor, SemSupervisorChild, SupervisorId,
};

fn stop_deadline(
    child: &hew_hir::HirSupervisorChild,
    source: &hew_hir::HirSupervisorDecl,
    role: SemSupervisedRole,
) -> Result<SemStopDeadline, String> {
    let Some(stop) = &child.stop else {
        return Ok(if matches!(role, SemSupervisedRole::Actor(_)) {
            SemStopDeadline::DefaultActor
        } else {
            SemStopDeadline::Literal(-1)
        });
    };
    if matches!(role, SemSupervisedRole::Supervisor(_)) {
        return Err("a nested supervisor uses its children's stop deadlines".into());
    }
    match &stop.kind {
        HirExprKind::Literal(hew_hir::HirLiteral::Duration(ns)) if *ns >= 0 => {
            Ok(SemStopDeadline::Literal(*ns))
        }
        HirExprKind::BindingRef {
            resolved: hew_hir::ResolvedRef::Binding(binding),
            ..
        } => source
            .params
            .iter()
            .position(|param| param.id == *binding)
            .map(SemStopDeadline::Config)
            .ok_or_else(|| {
                "child stop deadline does not name a supervisor config parameter".into()
            }),
        _ => Err(
            "child stop deadline requires a duration literal or supervisor config parameter".into(),
        ),
    }
}

pub(super) fn declaration<'a>(
    module: &'a HirModule,
    ty: &ResolvedTy,
) -> Option<&'a hew_hir::HirSupervisorDecl> {
    let declaration = crate::supervisor::declared_handle(&module.defs, ty)?;
    module.items.iter().find_map(|item| match item {
        HirItem::Supervisor(supervisor) if supervisor.declaration == declaration => {
            Some(supervisor)
        }
        _ => None,
    })
}

/// The restart window as whole seconds; the spec default is `10 within 5s`.
fn restart_window_secs(window: Option<&str>) -> Result<u32, String> {
    const SECOND: i64 = 1_000_000_000;
    let Some(window) = window else {
        return Ok(5);
    };
    let ns = hew_parser::parse_duration_ns(window)
        .ok_or_else(|| format!("restart window `{window}` is not a duration"))?;
    if ns <= 0 || ns % SECOND != 0 {
        return Err(format!(
            "restart window `{window}` must be a whole number of seconds"
        ));
    }
    u32::try_from(ns / SECOND)
        .map_err(|_| format!("restart window `{window}` exceeds the supported range"))
}

impl InstanceService<'_> {
    pub(super) fn require_supervisor(&mut self, ty: &ResolvedTy) -> Result<SupervisorId, String> {
        self.require_supervisor_intercepted(ty, &[])
    }

    #[expect(
        clippy::too_many_lines,
        reason = "one supervisor instance owns its config, restart budget and child spawn contracts"
    )]
    pub(super) fn require_supervisor_intercepted(
        &mut self,
        ty: &ResolvedTy,
        chain: &[ResolvedTy],
    ) -> Result<SupervisorId, String> {
        if let Some(supervisor) = self
            .supervisors
            .iter()
            .find(|supervisor| supervisor.handle_ty == *ty && supervisor.interceptor_chain == chain)
        {
            return Ok(supervisor.id);
        }
        let source = declaration(self.module, ty)
            .ok_or("supervisor handle lacks its exact declaration")?
            .clone();
        let instance = crate::actor::local_actor_instance(ty)
            .ok_or("supervisor instance lacks its nominal identity")?;
        if instance.args.len() != source.type_params.len() {
            return Err("supervisor instance type arguments differ from its declaration".into());
        }
        let substitution = TypeSubstitution {
            params: source.type_params.clone(),
            args: instance.args,
        };
        let strategy = match source.strategy {
            None | Some(hew_hir::HirSupervisorStrategy::OneForOne) => SemRestartStrategy::OneForOne,
            Some(hew_hir::HirSupervisorStrategy::OneForAll) => SemRestartStrategy::OneForAll,
            Some(hew_hir::HirSupervisorStrategy::RestForOne) => SemRestartStrategy::RestForOne,
            Some(hew_hir::HirSupervisorStrategy::SimpleOneForOne) => {
                SemRestartStrategy::SimpleOneForOne
            }
        };
        let max_restarts = match source.max_restarts {
            None => 10,
            Some(count) => u32::try_from(count)
                .ok()
                .ok_or("restart intensity must be a non-negative count")?,
        };
        let window_secs = restart_window_secs(source.window.as_deref())?;
        let config: Vec<ResolvedTy> = source
            .params
            .iter()
            .map(|param| substitution.apply(&param.ty))
            .collect();
        self.require_type_facts(ty)?;
        for ty in &config {
            self.require_type_facts(ty)?;
        }
        for ty in &substitution.args {
            self.require_type_facts(ty)?;
        }
        let id = SupervisorId(
            u32::try_from(self.supervisors.len()).map_err(|_| "supervisor count exceeds u32")?,
        );
        // Publish the header first so a nested child resolves this identity.
        self.supervisors.push(SemSupervisor {
            id,
            declaration: source.declaration,
            handle_ty: ty.clone(),
            interceptor_chain: chain.to_vec(),
            config: config.clone(),
            strategy,
            max_restarts,
            window_secs,
            backoff: None,
            children: Vec::new(),
            startup_order: source.startup_order.clone(),
        });
        if let Some(expression) = &source.backoff {
            let callable = self.register_supervisor_expression(
                id,
                &source,
                expression.as_ref().clone(),
                CallableInstance::SupervisorBackoff { supervisor: id },
                "backoff",
                &substitution,
            )?;
            self.supervisors[id.0 as usize].backoff = Some(callable);
        }
        let mut child_chain = chain.to_vec();
        if source.interceptor.is_some() {
            child_chain.push(ty.clone());
        }
        let mut children = Vec::new();
        for (index, child) in source.children.iter().enumerate() {
            let handle = substitution.apply(&child.ty);
            let role = if super::actor::declaration(self.module, &handle).is_some() {
                SemSupervisedRole::Actor(self.require_actor_intercepted(
                    &handle,
                    None,
                    &child_chain,
                )?)
            } else if declaration(self.module, &handle).is_some() {
                if handle == *ty {
                    return Err("a supervisor cannot supervise itself".into());
                }
                SemSupervisedRole::Supervisor(
                    self.require_supervisor_intercepted(&handle, &child_chain)?,
                )
            } else {
                return Err(format!(
                    "supervisor child `{}` names no actor or supervisor declaration",
                    child.name
                ));
            };
            // A pool's slots are actor slots: the runtime keeps supervisor
            // children in their own space, which the member accessor does not
            // address.
            if child.is_pool && matches!(role, SemSupervisedRole::Supervisor(_)) {
                return Err("a pool of supervisors needs its nested-slot contract".into());
            }
            let restart = match child.restart_policy {
                None | Some(hew_hir::HirRestartPolicy::Permanent) => SemRestartPolicy::Permanent,
                Some(hew_hir::HirRestartPolicy::Transient) => SemRestartPolicy::Transient,
                Some(hew_hir::HirRestartPolicy::Temporary) => SemRestartPolicy::Temporary,
            };
            let spawn =
                self.register_child_spawn(id, index, &source, child, handle, &substitution)?;
            children.push(SemSupervisorChild {
                name: child.name.clone(),
                dependencies: child
                    .wiring
                    .iter()
                    .map(|(_, sibling)| *sibling)
                    .collect::<std::collections::BTreeSet<_>>()
                    .into_iter()
                    .collect(),
                role,
                restart,
                stop_deadline: stop_deadline(child, &source, role)?,
                pool_count: if child.is_pool {
                    let count = child
                        .pool_count
                        .as_ref()
                        .ok_or_else(|| format!("pool `{}` declares no count", child.name))?;
                    Some(
                        self.register_supervisor_expression(
                            id,
                            &source,
                            count.clone(),
                            CallableInstance::SupervisorPoolCount {
                                supervisor: id,
                                child: u32::try_from(index)
                                    .map_err(|_| "pool declaration index exceeds u32")?,
                            },
                            &format!("pool_{index}_count"),
                            &substitution,
                        )?,
                    )
                } else {
                    None
                },
                spawn,
            });
        }
        self.supervisors[id.0 as usize].children = children;
        Ok(id)
    }

    /// `fn(supervisor, config...) -> handle { spawn Child { init args } }`, lowered and
    /// verified like any function. The config parameters are lent, so an owned
    /// init argument is copied into each incarnation.
    fn register_child_spawn(
        &mut self,
        supervisor: SupervisorId,
        index: usize,
        source: &hew_hir::HirSupervisorDecl,
        child: &hew_hir::HirSupervisorChild,
        handle: ResolvedTy,
        substitution: &TypeSubstitution,
    ) -> Result<CallableId, String> {
        let actor_name = self
            .module
            .defs
            .path(
                crate::supervisor::declared_handle(&self.module.defs, &handle)
                    .ok_or("supervised child lacks its declared handle type")?,
            )
            .to_string();
        let spawn = HirExpr {
            node: source.node,
            site: child.site,
            ty: handle,
            intent: IntentKind::Consume,
            kind: HirExprKind::Spawn {
                actor_name,
                args: child.init_args.clone(),
                slots: child.init_slots.clone(),
            },
            span: child.span.clone(),
        };
        self.register_supervisor_expression(
            supervisor,
            source,
            spawn,
            CallableInstance::SupervisorChild {
                supervisor,
                child: u32::try_from(index).map_err(|_| "supervisor child count exceeds u32")?,
            },
            &format!("child_{index}"),
            substitution,
        )
    }

    fn register_supervisor_expression(
        &mut self,
        supervisor: SupervisorId,
        source: &hew_hir::HirSupervisorDecl,
        expression: HirExpr,
        instance: CallableInstance,
        suffix: &str,
        substitution: &TypeSubstitution,
    ) -> Result<CallableId, String> {
        let id = CallableId(
            u32::try_from(self.table.callables.len()).map_err(|_| "callable count exceeds u32")?,
        );
        let result_ty = substitution.apply(&expression.ty);
        let span = expression.span.clone();
        let function = HirFn {
            failure_domain: hew_types::CallableFailureDomain::HewFault,
            id: source.id,
            node: source.node,
            declaration: source.bootstrap_declaration,
            name: format!("{}::{suffix}", source.name),
            type_params: Vec::new(),
            params: if matches!(instance, CallableInstance::SupervisorChild { .. }) {
                std::iter::once(source.owner.clone())
                    .chain(source.params.iter().cloned())
                    .collect()
            } else {
                source.params.clone()
            },
            var_self_receiver: None,
            terminal_receiver: None,
            return_ty: result_ty.clone(),
            body: HirBlock {
                node: source.node,
                scope: hew_hir::ScopeId(0),
                statements: Vec::new(),
                tail: Some(Box::new(expression)),
                ty: result_ty.clone(),
                span: span.clone(),
            },
            span,
            is_generator: false,
            intrinsic_id: None,
        };
        let mut params = Vec::new();
        if matches!(instance, CallableInstance::SupervisorChild { .. }) {
            params.push(SemAbiParam {
                ty: substitution.apply(&source.owner.ty),
                passing: SemParamPassing::Consume,
                caller_visible_projection: false,
            });
        }
        for ty in &self.supervisors[supervisor.0 as usize].config {
            let owned = OwnKind::of_ty(ty, self.checked_facts.rows())? == OwnKind::Owned;
            params.push(SemAbiParam {
                ty: ty.clone(),
                passing: if owned {
                    SemParamPassing::Borrow
                } else {
                    SemParamPassing::ReadOnly
                },
                caller_visible_projection: false,
            });
        }
        let signature = SemSignature {
            failure_domain: hew_types::CallableFailureDomain::HewFault,
            params,
            return_ty: result_ty,
        };
        self.require_signature_shapes(&signature)?;
        self.table.callables.push(SemCallable {
            id,
            function: source.id,
            declaration: source.bootstrap_declaration,
            instance,
            symbol: format!("__hew_supervisor_{}_{suffix}", supervisor.0),
            source_origin: function_source_origin(self.module, &function),
            signature,
            call_conv: SemCallConv::Default,
            kind: SemCallableKind::HewDirect,
        });
        self.synthetic_sources
            .insert(id, (function, substitution.clone()));
        self.states.push(CallableState::Unreached);
        self.statuses.push(None);
        self.request_body(id);
        Ok(id)
    }
}

impl Builder<'_, '_> {
    /// `sup.child`: a declared child resolved through its supervisor on every
    /// use. `sup.pool` instead produces the pool's view.
    pub(super) fn lower_supervisor_child(
        &mut self,
        expression: &HirExpr,
        object: &HirExpr,
        slot: &hew_types::ChildSlot,
    ) -> Result<ValueId, String> {
        if slot.kind == hew_types::ChildKind::Pool {
            return self.lower_supervisor_pool_view(expression, object, slot);
        }
        self.lower_supervisor_role(expression, object, slot)
    }

    fn lower_supervisor_role(
        &mut self,
        expression: &HirExpr,
        object: &HirExpr,
        slot: &hew_types::ChildSlot,
    ) -> Result<ValueId, String> {
        let supervisor = self.service.require_supervisor(&self.ty(&object.ty))?;
        let child = declaration(self.service.module, &self.ty(&object.ty))
            .ok_or("supervisor lookup lacks its declaration")?
            .children
            .iter()
            .enumerate()
            .filter(|(_, child)| child.is_pool == (slot.kind == hew_types::ChildKind::Pool))
            .nth(slot.index as usize)
            .map(|(index, _)| index)
            .ok_or("supervisor child lookup names no declared child")?;
        let child = u32::try_from(child).map_err(|_| "supervisor child count exceeds u32")?;
        let operation = ActorOperation::SupervisorChild { supervisor, child };
        let signature = self.actor_signature(&operation)?;
        if signature.return_ty != self.ty(&expression.ty) {
            return Err("supervisor child lookup differs from its checked handle".into());
        }
        let handle =
            lower_initial_value_transfer(self, object, "supervisor handle", OwnedBindingUse::Copy)?;
        if !self.is_open() {
            return Err("supervisor handle evaluation diverged".into());
        }
        self.emit_actor_call(operation, signature, vec![handle])?
            .ok_or_else(|| "supervisor child lookup has no result".into())
    }

    /// `sup.pool`: the pool's view — its supervisor and the first of the
    /// pool's consecutive member slots. Every accessor works from that view
    /// and the declared member count.
    fn lower_supervisor_pool_view(
        &mut self,
        expression: &HirExpr,
        object: &HirExpr,
        slot: &hew_types::ChildSlot,
    ) -> Result<ValueId, String> {
        let supervisor = self.service.require_supervisor(&self.ty(&object.ty))?;
        let child = declaration(self.service.module, &self.ty(&object.ty))
            .ok_or("supervisor lookup lacks its declaration")?
            .children
            .iter()
            .enumerate()
            .filter(|(_, child)| child.is_pool == (slot.kind == hew_types::ChildKind::Pool))
            .nth(slot.index as usize)
            .map(|(index, _)| index)
            .ok_or("supervisor pool lookup names no declared child")?;
        let child = u32::try_from(child).map_err(|_| "supervisor child count exceeds u32")?;
        let operation = ActorOperation::SupervisorPoolView { supervisor, child };
        let signature = self.actor_signature(&operation)?;
        if signature.return_ty != self.ty(&expression.ty) {
            return Err("supervisor pool lookup differs from its checked view".into());
        }
        let handle =
            lower_initial_value_transfer(self, object, "supervisor handle", OwnedBindingUse::Copy)?;
        if !self.is_open() {
            return Err("supervisor handle evaluation diverged".into());
        }
        self.emit_actor_call(operation, signature, vec![handle])?
            .ok_or_else(|| "supervisor pool lookup has no result".into())
    }

    pub(super) fn lower_supervisor_configuration(
        &mut self,
        expression: &HirExpr,
        configure: CallableId,
        config: &[ValueId],
    ) -> Result<ValueId, String> {
        let signature = self
            .service
            .callable(configure)
            .ok_or("supervisor configuration lost its callable")?
            .signature
            .clone();
        if signature.params.len() != config.len() {
            return Err("supervisor configuration expression differs from its config".into());
        }
        let live = self.owned_live.keys().copied().collect();
        let mut loans = Vec::new();
        let mut args = Vec::new();
        for (value, parameter) in config.iter().zip(&signature.params) {
            let (value, decision) = match parameter.passing {
                SemParamPassing::Borrow => {
                    let borrowed = self.emit_typed(
                        Provenance::Site(expression.site),
                        &parameter.ty,
                        SemOpKind::BeginBorrow {
                            owner: crate::Operand { value: *value },
                        },
                    )?;
                    loans.push(borrowed);
                    (borrowed, crate::BoundaryDecision::Borrow)
                }
                SemParamPassing::ReadOnly => (*value, crate::BoundaryDecision::Copy),
                _ => {
                    return Err(
                        "supervisor configuration must borrow its construction config".into(),
                    )
                }
            };
            args.push(crate::BoundaryOperand {
                operand: crate::Operand { value },
                decision,
            });
        }
        self.finish_user_call(
            super::PreparedCallee::Direct {
                target: configure,
                site: None,
            },
            signature,
            args,
            &loans,
            &live,
            true,
            None,
        )?
        .ok_or_else(|| "supervisor configuration returned no value".into())
    }

    pub(super) fn lower_supervisor_pool_count(
        &mut self,
        expression: &HirExpr,
        count: CallableId,
        config: &[ValueId],
    ) -> Result<ValueId, String> {
        let value = self.lower_supervisor_configuration(expression, count, config)?;
        if !self.value_ty(value).is_some_and(|ty| ty.is_integer()) {
            return Err("pool count returned no integer".into());
        }
        // Widen signed counts and preserve all unsigned bits. Values above
        // i64::MAX become negative and fail the runtime's positive i32 range.
        if self.value_ty(value) == Some(ResolvedTy::I64) {
            Ok(value)
        } else {
            self.emit_typed(
                Provenance::Site(expression.site),
                &ResolvedTy::I64,
                SemOpKind::Cast {
                    value: crate::Operand { value },
                    to: ResolvedTy::I64,
                },
            )
        }
    }

    /// Pool accessors consume the constructed view's member extent.
    pub(super) fn lower_pool_accessor(
        &mut self,
        expression: &HirExpr,
        kind: hew_types::PoolAccessorKind,
    ) -> Result<ValueId, String> {
        let (receiver, index) = match &expression.kind {
            HirExprKind::Index { container, index } => (container.as_ref(), Some(index.as_ref())),
            HirExprKind::Call { args, .. } => match args.as_slice() {
                [receiver] => (receiver, None),
                [receiver, index] => (receiver, Some(index)),
                _ => return Err("pool accessor requires its view and optional index".into()),
            },
            _ => return Err("pool accessor has no view".into()),
        };
        let operation = match kind {
            hew_types::PoolAccessorKind::Len => hew_types::runtime_call::SupervisorPoolOp::Len,
            hew_types::PoolAccessorKind::Index => hew_types::runtime_call::SupervisorPoolOp::Member,
            hew_types::PoolAccessorKind::Get => hew_types::runtime_call::SupervisorPoolOp::Get,
        };
        let mut args = vec![receiver];
        if let Some(index) = index {
            args.push(index);
        }
        self.lower_runtime_operation_with(
            expression,
            hew_types::RuntimeCallFamily::SupervisorPool(operation),
            (&args, &[]),
            true,
            &[],
        )?
        .ok_or_else(|| "pool accessor produces no value".into())
    }
}
