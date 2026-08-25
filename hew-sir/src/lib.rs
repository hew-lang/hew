//! Hew Semantic IR (SIR).
//!
//! SIR is the value-oriented SSA layer between resolved HIR and the existing
//! ownership/layout MIR ladder.  It deliberately contains no `Place`, alloca,
//! ABI carrier, byte-offset, or LLVM operation.  The initial shadow lane is a
//! conservative subset; unsupported HIR bodies report that fact to the driver,
//! which continues through the established HIR -> MIR path.

mod analysis;
mod dump;
mod lower;
mod model;
mod verify;

pub use analysis::{build_def_use, compute_dominators, DefUseIndex, Dominators};
pub use dump::dump_sir;
pub use lower::{lower_module, LoweredModule, SirLoweringStatus};
pub use model::{
    BlockArg, BlockId, Edge, OpId, Operand, Provenance, SemBlock, SemFunction, SemModule, SemOp,
    SemOpKind, SemTerminator, UseMode, ValueDef, ValueId,
};
pub use verify::{verify_module, SirDiagnostic, SirDiagnosticKind};
