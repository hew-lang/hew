//! Local v0.5 THIR/MIR/MLIR vertical slice.
//!
//! This crate deliberately models all ladder stages even while the first
//! executable subset is small. Later slices replace the placeholder lowering
//! with full CFG, borrow, drop, and dialect implementations without changing
//! the stage boundaries.

pub mod lower;
pub mod model;

pub use lower::lower_hir_module;
pub use model::{
    BasicBlock, DecisionFact, ElaboratedMirFunction, HewMlirFunction, HewMlirModule, Instr,
    IrPipeline, MirDiagnostic, MirDiagnosticKind, MirStatement, Place, RawMirFunction, Strategy,
    Terminator, ThirFunction,
};
