//! Supervisor declarations: restart policy, budget and the child roles the
//! supervisor owns.
//!
//! A supervisor is a nominal with no value members; values name its incarnation
//! through `LocalPid<S>` or its declared role through `ChildRef<S>`. Every child
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

/// One declared static child. Its runtime slot is its position among the
/// children of the same role kind, in declaration order.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SemSupervisorChild {
    pub name: String,
    pub role: SemSupervisedRole,
    pub restart: SemRestartPolicy,
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
    /// The runtime slot of one child within its role kind.
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
        u32::try_from(
            self.children[..child]
                .iter()
                .filter(|sibling| kind(&sibling.role))
                .count(),
        )
        .ok()
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
                .map(crate::SemActor::child_ref_ty)
                .ok_or_else(|| "supervised actor role has no descriptor".into()),
            SemSupervisedRole::Supervisor(id) => supervisors
                .get(id.0 as usize)
                .filter(|supervisor| supervisor.id == id)
                .map(Self::child_ref_ty)
                .ok_or_else(|| "nested supervisor role has no descriptor".into()),
        }
    }

    #[must_use]
    pub fn child_ref_ty(&self) -> ResolvedTy {
        let ResolvedTy::Named { args, .. } = &self.handle_ty else {
            unreachable!("supervisor handle is validated as LocalPid<S>");
        };
        ResolvedTy::named_builtin(
            hew_types::BuiltinType::ChildRef.canonical_name(),
            hew_types::BuiltinType::ChildRef,
            args.clone(),
        )
    }

    pub(crate) fn validate(&self, module: &SemModule) -> Result<(), String> {
        if module.supervisor(self.id) != Some(self) {
            return Err("supervisor descriptor is not at its canonical index".into());
        }
        if !self.handle_ty.is_builtin(hew_types::BuiltinType::LocalPid)
            || declared_handle(&self.handle_ty).as_ref() != Some(&self.declaration)
        {
            return Err("supervisor handle refers to another declaration".into());
        }
        if self.window_secs == 0 {
            return Err("supervisor restart budget requires a positive window".into());
        }
        let mut names = std::collections::BTreeSet::new();
        for (index, child) in self.children.iter().enumerate() {
            if !names.insert(child.name.as_str()) {
                return Err("supervisor child name is repeated".into());
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
pub(crate) fn declared_handle(ty: &ResolvedTy) -> Option<DefId> {
    let ResolvedTy::Named {
        builtin: Some(hew_types::BuiltinType::LocalPid | hew_types::BuiltinType::ChildRef),
        args,
        ..
    } = ty
    else {
        return None;
    };
    let [inner] = args.as_slice() else {
        return None;
    };
    let instance = inner.nominal_instance()?;
    Some(instance.nominal.declaration().clone())
}
