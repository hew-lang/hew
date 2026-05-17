//! Local v0.5 THIR/MIR vertical slice.
//!
//! This crate deliberately models all ladder stages even while the first
//! executable subset is small. Later slices replace the placeholder lowering
//! with full CFG, borrow, and drop implementations without changing the
//! stage boundaries.

pub mod dataflow;
pub mod lower;
pub mod model;
pub mod runtime_symbols;

pub use lower::lower_hir_module;
pub use model::{
    BasicBlock, BlockKind, BorrowKind, CaptureKind, CheckedMirFunction, CmpPred, CoroutineSchema,
    DecisionFact, Direction, DropKind, DropPlan, ElabBlock, ElabDrop, ElaboratedMirFunction,
    ExitPath, FieldOffset, FloatWidth, Instr, IntArithOp, IntSignedness, IrPipeline, LambdaCapture,
    MirCheck, MirDiagnostic, MirDiagnosticKind, MirStatement, Place, RawMirFunction, RecordLayout,
    RuntimeCall, SelectArm, SelectArmKind, Strategy, Terminator, ThirFunction, TrapKind,
};
pub use runtime_symbols::UnknownRuntimeSymbol;
