//! Demanded supervisor declarations and their boundaries.
//!
//! Each declared child gets a spawn callable `fn(config...) -> handle` whose
//! body is the child's `spawn` with the declared init arguments. The
//! supervisor runs it for the initial spawn and for every restart, so a
//! restarted child re-runs init and start against the same config.

use super::{
    function_source_origin, lower_initial_value_transfer, Builder, CallableId, CallableInstance,
    CallableState, HirBlock, HirExpr, HirExprKind, HirFn, HirItem, HirModule, InstanceService,
    IntentKind, OwnKind, OwnedBindingUse, ResolvedTy, SemAbiParam, SemCallConv, SemCallable,
    SemCallableKind, SemParamPassing, SemSignature, ValueId,
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

fn local_pid(nominal: String) -> ResolvedTy {
    ResolvedTy::named_builtin(
        hew_types::BuiltinType::LocalPid.canonical_name(),
        hew_types::BuiltinType::LocalPid,
        vec![ResolvedTy::Named {
            name: nominal,
            args: Vec::new(),
            builtin: None,
            is_opaque: false,
        }],
    )
}

impl InstanceService<'_> {
    pub(super) fn require_supervisor(&mut self, ty: &ResolvedTy) -> Result<SupervisorId, String> {
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
        let strategy = match source.strategy {
            None | Some(hew_hir::HirSupervisorStrategy::OneForOne) => SemRestartStrategy::OneForOne,
            Some(hew_hir::HirSupervisorStrategy::OneForAll) => SemRestartStrategy::OneForAll,
            Some(hew_hir::HirSupervisorStrategy::RestForOne) => SemRestartStrategy::RestForOne,
            Some(hew_hir::HirSupervisorStrategy::SimpleOneForOne) => {
                return Err("pool supervision needs its fungible-slot contract".into())
            }
        };
        if source.children.iter().any(|child| child.is_pool) {
            return Err("pool children need their fungible-slot contract".into());
        }
        if source.children.iter().any(|child| child.wired_to.is_some()) {
            return Err("`wired_to` needs its sibling-handle contract".into());
        }
        let max_restarts = match source.max_restarts {
            None => 10,
            Some(count) => u32::try_from(count)
                .ok()
                .filter(|count| *count > 0)
                .ok_or("restart intensity must be a positive count")?,
        };
        let window_secs = restart_window_secs(source.window.as_deref())?;
        let config: Vec<ResolvedTy> = source.params.iter().map(|param| param.ty.clone()).collect();
        self.require_type_facts(ty)?;
        for ty in &config {
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
            let handle = local_pid(child.ty.clone());
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
            let restart = match child.restart_policy {
                None | Some(hew_hir::HirRestartPolicy::Permanent) => SemRestartPolicy::Permanent,
                Some(hew_hir::HirRestartPolicy::Transient) => SemRestartPolicy::Transient,
                Some(hew_hir::HirRestartPolicy::Temporary) => SemRestartPolicy::Temporary,
            };
            let spawn = self.register_child_spawn(id, index, &source, child, handle, &config)?;
            children.push(SemSupervisorChild {
                name: child.name.clone(),
                role,
                restart,
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
        config: &[ResolvedTy],
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
                actor_name: child.ty.clone(),
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
        for ty in config {
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
            instance: CallableInstance::Monomorphic,
            symbol: format!("__hew_supervisor_{}_child_{index}", supervisor.0),
            source_origin: function_source_origin(self.module, &function),
            signature,
            call_conv: SemCallConv::Default,
            kind: SemCallableKind::HewDirect,
        });
        self.synthetic_sources.insert(id, function);
        self.states.push(CallableState::Unreached);
        self.statuses.push(None);
        self.request_body(id);
        Ok(id)
    }
}

impl Builder<'_, '_> {
    /// `sup.child`: a declared child resolved through its supervisor on every use.
    pub(super) fn lower_supervisor_child(
        &mut self,
        expression: &HirExpr,
        object: &HirExpr,
        slot: &hew_types::ChildSlot,
    ) -> Result<ValueId, String> {
        if slot.kind == hew_types::ChildKind::Pool {
            return Err("pool children need their fungible-slot contract".into());
        }
        let supervisor = self.service.require_supervisor(&self.ty(&object.ty))?;
        let child = self.service.supervisors[supervisor.0 as usize]
            .children
            .iter()
            .position(|child| child.name == slot.child_name)
            .ok_or("supervisor child lookup names no declared child")?;
        let operation = ActorOperation::SupervisorChild {
            supervisor,
            child: u32::try_from(child).map_err(|_| "supervisor child count exceeds u32")?,
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

    /// `supervisor_stop(sup)` / `close(sup)`: stop every child and the supervisor.
    pub(super) fn lower_supervisor_stop(
        &mut self,
        handle: &HirExpr,
    ) -> Result<Option<ValueId>, String> {
        let supervisor = self.service.require_supervisor(&self.ty(&handle.ty))?;
        let operation = ActorOperation::SupervisorStop(supervisor);
        let signature = self.actor_signature(&operation)?;
        let value =
            lower_initial_value_transfer(self, handle, "supervisor handle", OwnedBindingUse::Copy)?;
        if !self.is_open() {
            return Ok(None);
        }
        self.emit_actor_call(operation, signature, vec![value])
    }
}
