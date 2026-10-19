//! The existing standard-library semaphore waits are owned suspension points.

use super::{
    RuntimeArgumentContract, RuntimeArgumentEffect, RuntimeCallFamily, RuntimeResultEffect,
    RuntimeSemanticContract, RuntimeValueKind,
};
use crate::ResolvedTy;
use serde::{Deserialize, Serialize};
use strum::EnumIter;

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    EnumIter,
    Default,
)]
pub enum SemaphoreOp {
    #[default]
    Acquire,
    AcquireTimeout,
}

impl SemaphoreOp {
    #[must_use]
    pub const fn c_symbol(self) -> &'static str {
        match self {
            Self::Acquire => "hew_semaphore_acquire",
            Self::AcquireTimeout => "hew_semaphore_acquire_timeout",
        }
    }

    #[must_use]
    pub const fn contract(self) -> RuntimeSemanticContract {
        const HANDLE: RuntimeArgumentContract = RuntimeArgumentContract {
            ty: RuntimeValueKind::NominalArgument("std.semaphore.SemaphoreHandle"),
            effect: RuntimeArgumentEffect::Borrow,
        };
        RuntimeSemanticContract {
            arguments: match self {
                Self::Acquire => &[HANDLE],
                Self::AcquireTimeout => &[
                    HANDLE,
                    RuntimeArgumentContract {
                        ty: RuntimeValueKind::I64,
                        effect: RuntimeArgumentEffect::Borrow,
                    },
                ],
            },
            result: match self {
                Self::Acquire => RuntimeResultEffect::Unit,
                Self::AcquireTimeout => RuntimeResultEffect::BitCopy(RuntimeValueKind::Bool),
            },
            failures: &[],
        }
    }
}

impl RuntimeCallFamily {
    #[must_use]
    pub fn matches_semaphore_extern(
        self,
        defs: &crate::DefTable,
        module: &str,
        declaration: &str,
        symbol: &str,
        params: &[ResolvedTy],
        result: &ResolvedTy,
        consuming: &[bool],
    ) -> bool {
        let Self::Semaphore(operation) = self else {
            return false;
        };
        module == "std.semaphore"
            && declaration == format!("{module}.{}", operation.c_symbol())
            && symbol == operation.c_symbol()
            && consuming.len() == params.len()
            && consuming.iter().all(|consume| !consume)
            && operation.contract().matches_signature(defs, params, result)
    }
}
