//! Hew Semantic IR (SIR).
//!
//! SIR is the value-oriented SSA layer between resolved HIR and the existing
//! ownership/layout MIR ladder.  It deliberately contains no `Place`, alloca,
//! ABI carrier, byte-offset, or LLVM operation.  The strict `--sir-lower` lane
//! owns a conservative subset today; each supported family moves onto
//! SIR -> MIR and deletes its established HIR -> MIR body lowering.

mod analysis;
mod dump;
mod lower;
mod model;
mod optimize;
mod ownership;
mod verify;

pub use analysis::{
    build_cfg_index, build_def_use, compute_dominators, replace_all_uses, replace_use, CfgIndex,
    DefUseIndex, Dominators, EdgeRef, RewriteError,
};
pub use dump::{dump_lowering, dump_sir};
pub use lower::{
    lower_module, lower_module_with_demand, LoweredModule, SirLoweringDemand, SirLoweringStatus,
    SirSourceStatus,
};
pub use model::{
    BlockArg, BlockId, BoundaryOperand, CallableId, CallableInstance, Edge, EffectSet,
    FunctionSourceOrigin, GenericTemplateId, OpId, Operand, OperandSlot, Provenance, SemAbiParam,
    SemBlock, SemCallConv, SemCallable, SemCallableKind, SemFunction, SemFunctionIndex,
    SemGenericTemplate, SemModule, SemOp, SemOpKind, SemParamPassing, SemSignature, SemTerminator,
    SirInstanceKey, SuccessorSlot, UseSite, ValueDef, ValueId,
};
pub use optimize::{
    canonicalize_module_constant_cfg, CfgCanonicalizationReport, SirOptimizationError,
};
pub use ownership::{
    Binding, BindingId, BindingTarget, BoundaryDecision, BytesLiteralId, OwnKind, PlaceDecl,
    PlaceId, SnapshotDecision, StringLiteralId, SuspendKind, TrapKind,
};
pub use verify::{
    verify_function, verify_function_in_module, verify_module, CfgDiscardSafetyReason,
    SirDiagnostic, SirDiagnosticKind,
};
