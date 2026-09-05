//! Checked target-realized storage and ABI for verified ownership SIR.

pub mod physical;

pub use physical::{
    lower_physical_module, ArgumentTransfer, CloneAction, DestroyAction, ParamCarrier,
    PhysicalBlock, PhysicalCallable, PhysicalCheckedFailure, PhysicalConst, PhysicalEdge,
    PhysicalError, PhysicalFunction, PhysicalLayout, PhysicalModule, PhysicalOp, PhysicalParam,
    PhysicalRepr, PhysicalRuntimeAction, PhysicalStorage, PhysicalTarget, PhysicalTerminator,
    ReturnTransfer, StorageId, StorageOrigin, VerifiedPhysicalModule,
};

/// Pointer width selected by a compilation host.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum PointerWidth {
    Bits32,
    #[default]
    Bits64,
}
