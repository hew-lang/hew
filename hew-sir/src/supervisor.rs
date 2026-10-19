//! Supervisor declarations: restart policy, budget and the child roles the
//! supervisor owns.
//!
//! A supervisor is a nominal with no value members; values name its incarnation
//! through its own type `S`, including a stable declared role. Every child
//! is reconstructed from its declaration and the
//! supervisor's config on each incarnation through a verified spawn callable,
//! so restart never preserves mutable state.

use hew_types::{DefId, ResolvedTy};

use crate::{ActorId, CallableId, SemModule};

/// Identity of one demanded supervisor declaration in this semantic module.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SupervisorId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SemRestartStrategy {
    OneForOne,
    OneForAll,
    RestForOne,
    /// Pool supervision: only the crashed member's own slot is restarted.
    /// Required of every supervisor that declares a `pool` child.
    SimpleOneForOne,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SemRestartPolicy {
    Permanent,
    Transient,
    Temporary,
}

/// What occupies one supervised role.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SemSupervisedRole {
    Actor(ActorId),
    Supervisor(SupervisorId),
}

/// Checked source of a child's graceful-stop deadline.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SemStopDeadline {
    /// The runtime's ordinary actor stop budget.
    DefaultActor,
    Literal(i64),
    Config(usize),
}

/// One declared child. Its runtime slots are its position among the children
/// of the same role kind, in declaration order. A pool is the sole declaration
/// of a simple-one-for-one supervisor and fills its actor slots at construction.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SemSupervisorChild {
    pub name: String,
    pub role: SemSupervisedRole,
    pub restart: SemRestartPolicy,
    pub stop_deadline: SemStopDeadline,
    /// A checked count callable over borrowed config, evaluated once before
    /// bootstrap. `None` identifies an ordinary single child.
    pub pool_count: Option<CallableId>,
    /// `fn(config...) -> handle`: evaluates the declared init arguments against
    /// the supervisor's config and spawns one incarnation. The supervisor calls
    /// it for the initial spawn and for every restart.
    pub spawn: CallableId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SemSupervisor {
    pub id: SupervisorId,
    pub declaration: DefId,
    pub handle_ty: ResolvedTy,
    /// Outer-to-inner supervisor interceptors specialized into this instance.
    pub interceptor_chain: Vec<ResolvedTy>,
    /// Construction-time config parameters in declaration order; the spawn
    /// callables borrow them.
    pub config: Vec<ResolvedTy>,
    pub strategy: SemRestartStrategy,
    pub max_restarts: u32,
    pub window_secs: u32,
    pub children: Vec<SemSupervisorChild>,
}

impl SemModule {
    #[must_use]
    pub fn supervisor(&self, id: SupervisorId) -> Option<&SemSupervisor> {
        self.supervisors
            .get(id.0 as usize)
            .filter(|supervisor| supervisor.id == id)
    }
}

impl SemSupervisor {
    /// The first runtime slot of one child within its role kind. A pool's
    /// member `i` occupies `slot(child) + i`.
    #[must_use]
    pub fn slot(&self, child: usize) -> Option<u32> {
        let role = self.children.get(child)?.role;
        let kind = |candidate: &SemSupervisedRole| {
            matches!(
                (candidate, role),
                (SemSupervisedRole::Actor(_), SemSupervisedRole::Actor(_))
                    | (
                        SemSupervisedRole::Supervisor(_),
                        SemSupervisedRole::Supervisor(_)
                    )
            )
        };
        self.children[..child]
            .iter()
            .filter(|sibling| kind(&sibling.role))
            .try_fold(0u32, |base, _| base.checked_add(1))
    }

    /// The stable role a lookup produces, for either child kind.
    pub(crate) fn child_handle_ty(
        &self,
        child: usize,
        actors: &[crate::SemActor],
        supervisors: &[SemSupervisor],
    ) -> Result<ResolvedTy, String> {
        match self
            .children
            .get(child)
            .ok_or("supervisor child slot is out of range")?
            .role
        {
            SemSupervisedRole::Actor(id) => actors
                .get(id.0 as usize)
                .filter(|actor| actor.id == id)
                .map(|actor| actor.handle_ty.clone())
                .ok_or_else(|| "supervised actor role has no descriptor".into()),
            SemSupervisedRole::Supervisor(id) => supervisors
                .get(id.0 as usize)
                .filter(|supervisor| supervisor.id == id)
                .map(|supervisor| supervisor.handle_ty.clone())
                .ok_or_else(|| "nested supervisor role has no descriptor".into()),
        }
    }

    /// The first-class view a `pool` child's accessor produces:
    /// `SupervisorPool<S, T>` over the owning supervisor and its member type.
    pub(crate) fn pool_view_ty(
        &self,
        child: usize,
        actors: &[crate::SemActor],
        supervisors: &[Self],
    ) -> Result<ResolvedTy, String> {
        if self
            .children
            .get(child)
            .ok_or("supervisor child slot is out of range")?
            .pool_count
            .is_none()
        {
            return Err("a pool view names no pool declaration".into());
        }
        let member = self.child_handle_ty(child, actors, supervisors)?;
        let view = vec![self.handle_ty.clone(), member];
        Ok(ResolvedTy::named_builtin(
            hew_types::BuiltinType::SupervisorPool,
            view,
        ))
    }

    pub(crate) fn validate(&self, module: &SemModule) -> Result<(), String> {
        if module.supervisor(self.id) != Some(self) {
            return Err("supervisor descriptor is not at its canonical index".into());
        }
        if !self
            .handle_ty
            .is_builtin(hew_types::BuiltinType::ActorHandle)
            || declared_handle(&module.defs, &self.handle_ty) != Some(self.declaration)
        {
            return Err("supervisor handle refers to another declaration".into());
        }
        if self.window_secs == 0 {
            return Err("supervisor restart budget requires a positive window".into());
        }
        if self.strategy == SemRestartStrategy::SimpleOneForOne
            && (self.children.len() != 1 || self.children[0].pool_count.is_none())
        {
            return Err("simple_one_for_one requires exactly one pool".into());
        }
        let mut names = std::collections::BTreeSet::new();
        for (index, child) in self.children.iter().enumerate() {
            if !names.insert(child.name.as_str()) {
                return Err("supervisor child name is repeated".into());
            }
            if child.pool_count.is_some()
                && (self.strategy != SemRestartStrategy::SimpleOneForOne
                    || self.children.len() != 1
                    || !matches!(child.role, SemSupervisedRole::Actor(_)))
            {
                return Err("a pool requires one actor declaration and simple_one_for_one".into());
            }
            self.child_handle_ty(index, &module.actors, &module.supervisors)?;
            let spawned = match child.role {
                SemSupervisedRole::Actor(id) => module.actor(id).map(|actor| &actor.handle_ty),
                SemSupervisedRole::Supervisor(id) => {
                    if id == self.id {
                        return Err("a supervisor cannot supervise itself".into());
                    }
                    module
                        .supervisor(id)
                        .map(|supervisor| &supervisor.handle_ty)
                }
            }
            .ok_or("supervised role has no descriptor")?;
            if let Some(count) = child.pool_count {
                let callable = module.callable(count).ok_or("pool count has no callable")?;
                if callable.kind != crate::SemCallableKind::HewDirect
                    || !callable.signature.return_ty.is_integer()
                    || callable.signature.params.len() != self.config.len()
                    || callable
                        .signature
                        .params
                        .iter()
                        .zip(&self.config)
                        .any(|(param, ty)| {
                            param.ty != *ty
                                || !matches!(
                                    param.passing,
                                    crate::SemParamPassing::ReadOnly
                                        | crate::SemParamPassing::Borrow
                                )
                        })
                {
                    return Err(
                        "pool count callable differs from its config or integer result".into(),
                    );
                }
            }
            let callable = module
                .callables
                .iter()
                .find(|callable| callable.id == child.spawn)
                .ok_or("supervised child has no spawn callable")?;
            if callable.kind != crate::SemCallableKind::HewDirect
                || callable.signature.return_ty != *spawned
                || callable.signature.params.len() != self.config.len()
                || callable
                    .signature
                    .params
                    .iter()
                    .zip(&self.config)
                    .any(|(param, ty)| {
                        param.ty != *ty
                            || !matches!(
                                param.passing,
                                crate::SemParamPassing::ReadOnly | crate::SemParamPassing::Borrow
                            )
                    })
            {
                return Err(
                    "supervised child spawn callable differs from its config and role".into(),
                );
            }
        }
        Ok(())
    }
}

/// The declaration a direct handle or stable supervisor role names.
pub(crate) fn declared_handle(_defs: &hew_types::DefTable, ty: &ResolvedTy) -> Option<DefId> {
    let instance = crate::actor::local_actor_instance(ty)?;
    Some(instance.nominal.declaration())
}
