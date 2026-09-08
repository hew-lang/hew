//! Fixed-size array operations preserve their exact `[T; N]` receiver identity.

use super::{
    runtime_semantic_contract, RuntimeArgumentContract, RuntimeArgumentEffect,
    RuntimeLogicalFailure, RuntimeResultEffect, RuntimeSemanticContract, RuntimeValueKind,
};
use serde::{Deserialize, Serialize};
use strum::EnumIter;

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash, EnumIter, Serialize, Deserialize)]
pub enum ArrayValueOp {
    #[default]
    Len,
    Index,
    IndexBorrow,
    Set,
}

impl ArrayValueOp {
    pub(super) const fn symbol(self) -> &'static str {
        match self {
            Self::Len => "array.value.len",
            Self::Index => "array.value.index",
            Self::IndexBorrow => "array.value.index_borrow",
            Self::Set => "array.value.set",
        }
    }

    pub(super) const fn contract(self) -> RuntimeSemanticContract {
        const READ: RuntimeArgumentContract = RuntimeArgumentContract {
            ty: RuntimeValueKind::FixedArray,
            effect: RuntimeArgumentEffect::Borrow,
        };
        const WRITE: RuntimeArgumentContract = RuntimeArgumentContract {
            ty: RuntimeValueKind::FixedArray,
            effect: RuntimeArgumentEffect::Move,
        };
        const INDEX: RuntimeArgumentContract = RuntimeArgumentContract {
            ty: RuntimeValueKind::I64,
            effect: RuntimeArgumentEffect::Copy,
        };
        const ELEMENT: RuntimeArgumentContract = RuntimeArgumentContract {
            ty: RuntimeValueKind::ArrayElement,
            effect: RuntimeArgumentEffect::Value,
        };
        const BOUNDS: &[RuntimeLogicalFailure] = &[RuntimeLogicalFailure::IndexOutOfBounds];
        match self {
            Self::Len => runtime_semantic_contract(
                &[READ],
                RuntimeResultEffect::BitCopy(RuntimeValueKind::I64),
                &[],
            ),
            Self::Index => runtime_semantic_contract(
                &[READ, INDEX],
                RuntimeResultEffect::IndependentValue(RuntimeValueKind::ArrayElement),
                BOUNDS,
            ),
            Self::IndexBorrow => runtime_semantic_contract(
                &[READ, INDEX],
                RuntimeResultEffect::Borrowed(RuntimeValueKind::ArrayElement),
                BOUNDS,
            ),
            Self::Set => runtime_semantic_contract(
                &[WRITE, INDEX, ELEMENT],
                RuntimeResultEffect::UpdatedReceiver(RuntimeValueKind::FixedArray),
                BOUNDS,
            ),
        }
    }
}
