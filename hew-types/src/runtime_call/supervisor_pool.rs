//! Fungible supervisor pool members.
//!
//! A pool's members are identical children addressed by index. The view names
//! the owning supervisor and the first of the pool's consecutive slots; the
//! member count is a declaration fact the caller supplies, so an accessor is
//! arithmetic and a bounds test with no runtime lookup of its own.

use super::{
    runtime_semantic_contract, RuntimeArgumentContract, RuntimeArgumentEffect,
    RuntimeLogicalFailure, RuntimeResultEffect, RuntimeSemanticContract, RuntimeValueKind,
};
use serde::{Deserialize, Serialize};
use strum::EnumIter;

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash, EnumIter, Serialize, Deserialize)]
pub enum SupervisorPoolOp {
    /// `pool[i]` — the member's role, trapping outside the declared members
    /// exactly as `Vec[i]` does.
    #[default]
    Member,
    /// `pool.get(i)` — the member's role, or `None` outside them.
    Get,
    /// `await_restart pool[i]` — wait for that member's own slot to be Live
    /// again, or permanently gone, then its role.
    AwaitRestartMember,
}

impl SupervisorPoolOp {
    pub(super) const fn symbol(self) -> &'static str {
        match self {
            Self::Member => "supervisor.pool.member",
            Self::Get => "supervisor.pool.get",
            Self::AwaitRestartMember => "supervisor.pool.await_restart_member",
        }
    }

    pub(super) const fn contract(self) -> RuntimeSemanticContract {
        const VIEW: RuntimeArgumentContract = RuntimeArgumentContract {
            ty: RuntimeValueKind::PoolView,
            effect: RuntimeArgumentEffect::Borrow,
        };
        const INDEX: RuntimeArgumentContract = RuntimeArgumentContract {
            ty: RuntimeValueKind::I64,
            effect: RuntimeArgumentEffect::Copy,
        };
        const COUNT: RuntimeArgumentContract = RuntimeArgumentContract {
            ty: RuntimeValueKind::I64,
            effect: RuntimeArgumentEffect::Copy,
        };
        const BOUNDS: &[RuntimeLogicalFailure] = &[RuntimeLogicalFailure::IndexOutOfBounds];
        match self {
            Self::Member | Self::AwaitRestartMember => runtime_semantic_contract(
                &[VIEW, INDEX, COUNT],
                RuntimeResultEffect::IndependentValue(RuntimeValueKind::PoolMember),
                BOUNDS,
            ),
            Self::Get => runtime_semantic_contract(
                &[VIEW, INDEX, COUNT],
                RuntimeResultEffect::IndependentValue(RuntimeValueKind::Applied(
                    crate::BuiltinType::Option,
                    &[RuntimeValueKind::PoolMember],
                )),
                &[],
            ),
        }
    }
}
