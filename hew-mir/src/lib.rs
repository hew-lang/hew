//! Local v0.5 THIR/MIR vertical slice.
//!
//! This crate deliberately models all ladder stages even while the first
//! executable subset is small. Later slices replace the placeholder lowering
//! with full CFG, borrow, and drop implementations without changing the
//! stage boundaries.

pub mod lower;
pub mod model;

pub use lower::lower_hir_module;
pub use model::{
    BasicBlock, BlockKind, BorrowKind, CheckedMirFunction, CmpPred, CoroutineSchema, DecisionFact,
    DropPlan, ElabBlock, ElabDrop, ElaboratedMirFunction, ExitPath, Instr, IrPipeline, MirCheck,
    MirDiagnostic, MirDiagnosticKind, MirStatement, Place, RawMirFunction, Strategy, Terminator,
    ThirFunction,
};
