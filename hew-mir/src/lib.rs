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

/// Test-only access to the per-Place + per-`ResolvedTy` drop-kind
/// dispatcher. Tests pin the boundary contract that codegen consumes
/// (e.g. `dyn Trait` locals → `DropKind::TraitObject`) without round-
/// tripping through a full pipeline. Not part of the public API; the
/// re-export sits in the crate root so tests in `tests/` can reach it.
#[doc(hidden)]
#[must_use]
pub fn drop_kind_for_test(place: Place, ty: &hew_types::ResolvedTy) -> DropKind {
    lower::drop_kind_for_test_only(place, ty)
}
pub use model::{
    BasicBlock, BlockKind, BorrowKind, CaptureKind, CheckedMirFunction, CmpPred, CooperateKind,
    CooperateSite, CoroutineSchema, DecisionFact, Direction, DropKind, DropPlan, ElabBlock,
    ElabDrop, ElaboratedMirFunction, ExitPath, FieldOffset, FloatWidth, Instr, IntArithOp,
    IntSignedness, IrPipeline, LambdaCapture, MirCheck, MirDiagnostic, MirDiagnosticKind,
    MirStatement, Place, RawMirFunction, RecordLayout, RuntimeCall, SelectArm, SelectArmKind,
    Strategy, Terminator, ThirFunction, TrapKind,
};
pub use runtime_symbols::UnknownRuntimeSymbol;
