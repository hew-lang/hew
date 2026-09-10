//! Demanded supervisor declarations and their boundaries.
//!
//! Each declared child gets a spawn callable `fn(config...) -> handle` whose
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
    ActorOperation, SemRestartPolicy, SemRestartStrategy, SemSupervisedRole, SemSupervisor,
    SemSupervisorChild, SupervisorId,
};

pub(super) fn declaration<'a>(
    module: &'a HirModule,
    ty: &ResolvedTy,
) -> Option<&'a hew_hir::HirSupervisorDecl> {
    let declaration = crate::supervisor::declared_handle(ty)?;
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

/// A pool's arity. The `count:` clause is a compile-time member count: the
/// declared slots are laid out at compile time, so a config-derived count has
/// no slot space to occupy and fails closed here.
fn pool_count(child: &hew_hir::HirSupervisorChild) -> Result<Option<u32>, String> {
    if !child.is_pool {
        return Ok(None);
    }
    let Some(count) = &child.pool_count else {
        return Err(format!("pool `{}` declares no `count:` clause", child.name));
    };
    let HirExprKind::Literal(hew_hir::HirLiteral::Integer(value)) = &count.kind else {
        return Err(format!(
            "pool `{}` requires a literal `count:`; a computed arity has no declared slots",
            child.name
        ));
    };
    let members = u32::try_from(*value)
        .ok()
        .filter(|members| *members > 0)
        .ok_or_else(|| format!("pool `{}` requires a positive `count:`", child.name))?;
    Ok(Some(members))
}

impl InstanceService<'_> {
    #[expect(
        clippy::too_many_lines,
        reason = "one supervisor instance owns its config, restart budget and child spawn contracts"
    )]
    pub(super) fn require_supervisor(&mut self, ty: &ResolvedTy) -> Result<SupervisorId, String> {
        if let ResolvedTy::Named {
            builtin: Some(hew_types::BuiltinType::ChildRef),
            args,
            ..
        } = ty
        {
            return self.require_supervisor(&ResolvedTy::named_builtin(
                hew_types::BuiltinType::LocalPid.canonical_name(),
                hew_types::BuiltinType::LocalPid,
                args.clone(),
            ));
        }
        if let Some(supervisor) = self
            .supervisors
            .iter()
            .find(|supervisor| supervisor.handle_ty == *ty)
        {
            return Ok(supervisor.id);
        }
        let source = declaration(self.module, ty)
            .ok_or("supervisor handle lacks its exact declaration")?
            .clone();
        let ResolvedTy::Named { args, .. } = ty else {
            return Err("supervisor instance requires its checked handle type".into());
        };
        let [inner] = args.as_slice() else {
            return Err("supervisor handle requires one concrete supervisor type".into());
        };
        let instance = inner
            .nominal_instance()
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
        if source.children.iter().any(|child| child.wired_to.is_some()) {
            return Err("`wired_to` needs its sibling-handle contract".into());
        }
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
            declaration: source.declaration.clone(),
            handle_ty: ty.clone(),
            config: config.clone(),
            strategy,
            max_restarts,
            window_secs,
            children: Vec::new(),
        });
        let mut children = Vec::new();
        for (index, child) in source.children.iter().enumerate() {
            let handle = substitution.apply(&child.ty);
            let role = if super::actor::declaration(self.module, &handle).is_some() {
                SemSupervisedRole::Actor(self.require_actor(&handle)?)
            } else if declaration(self.module, &handle).is_some() {
                if handle == *ty {
                    return Err("a supervisor cannot supervise itself".into());
                }
                SemSupervisedRole::Supervisor(self.require_supervisor(&handle)?)
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
                role,
                restart,
                pool_count: pool_count(child)?,
                spawn,
            });
        }
        self.supervisors[id.0 as usize].children = children;
        Ok(id)
    }

    /// `fn(config...) -> handle { spawn Child(init args) }`, lowered and
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
        let id = CallableId(
            u32::try_from(self.table.callables.len()).map_err(|_| "callable count exceeds u32")?,
        );
        let spawn = HirExpr {
            node: source.node,
            site: child.site,
            ty: handle.clone(),
            value_class: hew_hir::ValueClass::BitCopy,
            intent: IntentKind::Consume,
            kind: HirExprKind::Spawn {
                actor_name: crate::supervisor::declared_handle(&handle)
                    .ok_or("supervised child lacks its declared handle type")?
                    .full_path()
                    .to_string(),
                args: child.init_args.clone(),
            },
            span: child.span.clone(),
        };
        let function = HirFn {
            id: source.id,
            node: source.node,
            declaration: source.bootstrap_declaration.clone(),
            name: format!("{}::{}", source.name, child.name),
            type_params: Vec::new(),
            params: source.params.clone(),
            var_self_receiver: None,
            return_ty: handle.clone(),
            body: HirBlock {
                node: source.node,
                scope: hew_hir::ScopeId(0),
                statements: Vec::new(),
                tail: Some(Box::new(spawn)),
                ty: handle.clone(),
                span: child.span.clone(),
            },
            span: child.span.clone(),
            is_generator: false,
            intrinsic_id: None,
        };
        let mut params = Vec::new();
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
            params,
            return_ty: handle,
        };
        self.require_signature_shapes(&signature)?;
        self.table.callables.push(SemCallable {
            id,
            function: source.id,
            declaration: source.bootstrap_declaration.clone(),
            instance: CallableInstance::SupervisorChild {
                supervisor,
                child: u32::try_from(index).map_err(|_| "supervisor child count exceeds u32")?,
            },
            symbol: format!("__hew_supervisor_{}_child_{index}", supervisor.0),
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
    /// `await_restart sup.child`: the same role, produced only once the child
    /// is Live again or permanently gone.
    pub(super) fn lower_supervisor_await_restart(
        &mut self,
        expression: &HirExpr,
        child: &HirExpr,
    ) -> Result<ValueId, String> {
        // The barrier blocks the calling thread. In `main` that thread is the
        // program; inside an actor it is a scheduler worker the restart needs
        // in order to finish, so the cooperative form has to suspend instead.
        if self
            .service
            .actors
            .iter()
            .any(|actor| actor.bodies().any(|body| body == self.callable.id))
        {
            return Err("`await_restart` inside an actor needs its suspend contract".into());
        }
        // `await_restart sup.pool[i]` waits on that one member's slot; the
        // accessor's own bounds check runs first, exactly as `sup.pool[i]` does.
        if matches!(
            self.service
                .module
                .pool_accessor_sites
                .get(&child.site)
                .map(|accessor| accessor.kind),
            Some(hew_types::PoolAccessorKind::Index)
        ) {
            return self.lower_pool_accessor(child, hew_types::PoolAccessorKind::Index, true);
        }
        let HirExprKind::FieldAccess { object, .. } = &child.kind else {
            return Err("`await_restart` operand is not a supervised child".into());
        };
        let slot = self
            .service
            .module
            .supervisor_child_slots
            .get(&child.site)
            .ok_or("`await_restart` operand names no supervised child")?
            .clone();
        self.lower_supervisor_role(expression, object, &slot, true)
    }

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
        self.lower_supervisor_role(expression, object, slot, false)
    }

    fn lower_supervisor_role(
        &mut self,
        expression: &HirExpr,
        object: &HirExpr,
        slot: &hew_types::ChildSlot,
        await_restart: bool,
    ) -> Result<ValueId, String> {
        let supervisor = self.service.require_supervisor(&self.ty(&object.ty))?;
        let child = self.service.supervisors[supervisor.0 as usize]
            .children
            .iter()
            .position(|child| child.name == slot.child_name)
            .ok_or("supervisor child lookup names no declared child")?;
        let child = u32::try_from(child).map_err(|_| "supervisor child count exceeds u32")?;
        let owner_is_role = self
            .ty(&object.ty)
            .is_builtin(hew_types::BuiltinType::ChildRef);
        let operation = if await_restart {
            ActorOperation::SupervisorAwaitRestart {
                supervisor,
                child,
                owner_is_role,
            }
        } else {
            ActorOperation::SupervisorChild {
                supervisor,
                child,
                owner_is_role,
            }
        };
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
        let child = self.service.supervisors[supervisor.0 as usize]
            .children
            .iter()
            .position(|child| child.name == slot.child_name)
            .ok_or("supervisor pool lookup names no declared child")?;
        let child = u32::try_from(child).map_err(|_| "supervisor child count exceeds u32")?;
        let owner_is_role = self
            .ty(&object.ty)
            .is_builtin(hew_types::BuiltinType::ChildRef);
        let operation = ActorOperation::SupervisorPoolView {
            supervisor,
            child,
            owner_is_role,
        };
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

    /// `pool.len()`, `pool[i]` and `pool.get(i)`: the fungible-slot contract.
    /// Members are identical children addressed by index, so `len` is the
    /// declared count, `[i]` traps outside it and `get` reports absence. The
    /// count travels as an argument, so the accessor needs no runtime lookup.
    pub(super) fn lower_pool_accessor(
        &mut self,
        expression: &HirExpr,
        kind: hew_types::PoolAccessorKind,
        await_restart: bool,
    ) -> Result<ValueId, String> {
        let (receiver, index) = match &expression.kind {
            HirExprKind::Index { container, index } => (container.as_ref(), Some(index.as_ref())),
            HirExprKind::Call { args, .. } => match args.as_slice() {
                [receiver] => (receiver, None),
                [receiver, index] => (receiver, Some(index)),
                _ => {
                    return Err("supervisor pool accessor takes its pool and one index".into());
                }
            },
            _ => return Err("supervisor pool accessor has no pool receiver".into()),
        };
        let count = self.pool_member_count(&self.ty(&receiver.ty))?;
        if kind == hew_types::PoolAccessorKind::Len {
            // The receiver still runs: it may be the expression that starts the
            // supervisor. Its view is then the count's only other use.
            lower_initial_value_transfer(self, receiver, "supervisor pool", OwnedBindingUse::Copy)?;
            if !self.is_open() {
                return Err("supervisor pool evaluation diverged".into());
            }
            return self.emit(expression, SemOpKind::ConstInteger(i128::from(count)));
        }
        let index = index.ok_or("supervisor pool member access takes an index")?;
        // The member count is a declaration fact, so it is produced here rather
        // than lowered from a source expression. Only its type is read from the
        // stand-in operand; the call transfers the constant itself.
        let members = self.emit_typed(
            Provenance::Site(expression.site),
            &ResolvedTy::I64,
            SemOpKind::ConstInteger(i128::from(count)),
        )?;
        let mut bound = index.clone();
        bound.ty = ResolvedTy::I64;
        let operation = match (kind, await_restart) {
            (hew_types::PoolAccessorKind::Index, false) => {
                hew_types::runtime_call::SupervisorPoolOp::Member
            }
            (hew_types::PoolAccessorKind::Index, true) => {
                hew_types::runtime_call::SupervisorPoolOp::AwaitRestartMember
            }
            (hew_types::PoolAccessorKind::Get, false) => {
                hew_types::runtime_call::SupervisorPoolOp::Get
            }
            (hew_types::PoolAccessorKind::Get, true) => {
                return Err("`await_restart` waits on one member, not a `get`".into())
            }
            (hew_types::PoolAccessorKind::Len, _) => {
                unreachable!("the member count returned above")
            }
        };
        self.lower_runtime_operation_with(
            expression,
            hew_types::RuntimeCallFamily::SupervisorPool(operation),
            &[receiver, index, &bound],
            true,
            &[(2, members)],
        )?
        .ok_or_else(|| "supervisor pool accessor produces no value".to_string())
    }

    /// The declared member count of the pool a `SupervisorPool<S, T>` value
    /// names. `simple_one_for_one` admits exactly one pool child, so the
    /// view's supervisor identifies the declaration.
    fn pool_member_count(&mut self, view_ty: &ResolvedTy) -> Result<u32, String> {
        let ResolvedTy::Named {
            builtin: Some(hew_types::BuiltinType::SupervisorPool),
            args,
            ..
        } = view_ty
        else {
            return Err("supervisor pool accessor has no pool receiver".into());
        };
        let [supervisor_ty, _] = args.as_slice() else {
            return Err("supervisor pool requires its supervisor and member types".into());
        };
        let supervisor = self.service.require_supervisor(&ResolvedTy::named_builtin(
            hew_types::BuiltinType::LocalPid.canonical_name(),
            hew_types::BuiltinType::LocalPid,
            vec![supervisor_ty.clone()],
        ))?;
        let mut pools = self.service.supervisors[supervisor.0 as usize]
            .children
            .iter()
            .filter_map(|child| child.pool_count);
        let count = pools
            .next()
            .ok_or("supervisor pool view names no pool declaration")?;
        if pools.next().is_some() {
            return Err("supervisor declares more than one pool".into());
        }
        Ok(count)
    }

    /// `supervisor_stop(sup)` / `close(sup)`: stop every child and the supervisor.
    pub(super) fn lower_supervisor_stop(
        &mut self,
        handle: &HirExpr,
    ) -> Result<Option<ValueId>, String> {
        let supervisor = self.service.require_supervisor(&self.ty(&handle.ty))?;
        let operation = if self
            .ty(&handle.ty)
            .is_builtin(hew_types::BuiltinType::ChildRef)
        {
            ActorOperation::SupervisorRoleAwaitClosed {
                supervisor,
                closing: true,
            }
        } else {
            ActorOperation::SupervisorStop(supervisor)
        };
        let signature = self.actor_signature(&operation)?;
        let value =
            lower_initial_value_transfer(self, handle, "supervisor handle", OwnedBindingUse::Copy)?;
        if !self.is_open() {
            return Ok(None);
        }
        self.emit_actor_call(operation, signature, vec![value])
    }
}
