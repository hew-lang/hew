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
    DecisionFact, ElaboratedMirFunction, HewMlirFunction, HewMlirModule, IrPipeline, MirStatement,
    RawMirFunction, Strategy, ThirFunction,
};
