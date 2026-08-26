//! Hew Semantic IR (SIR).
//!
//! SIR is the value-oriented SSA layer between resolved HIR and the existing
//! ownership/layout MIR ladder.  It deliberately contains no `Place`, alloca,
//! ABI carrier, byte-offset, or LLVM operation.  The initial shadow lane is a
//! temporary cutover proof for a conservative subset; each supported family
//! moves onto SIR -> MIR and deletes its established HIR -> MIR body lowering.

mod analysis;
mod dump;
mod lower;
mod model;
mod optimize;
mod verify;

pub use analysis::{
    build_cfg_index, build_def_use, compute_dominators, replace_all_uses, replace_use, CfgIndex,
    DefUseIndex, Dominators, EdgeRef, RewriteError,
};
pub use dump::dump_sir;
pub use lower::{lower_module, LoweredModule, SirLoweringStatus};
pub use model::{
    BlockArg, BlockId, CallableId, CallableInstance, Edge, EffectSet, EffectSummary,
    FunctionSourceOrigin, GenericTemplateId, OpId, Operand, OperandSlot, Provenance, SemAbiParam,
    SemBlock, SemCallConv, SemCallable, SemCallableKind, SemFunction, SemGenericTemplate,
    SemModule, SemOp, SemOpKind, SemParamPassing, SemSignature, SemTerminator, SirInstanceKey,
    SuccessorSlot, UseMode, UseSite, ValueDef, ValueId,
};
pub use optimize::{
    canonicalize_constant_cfg, canonicalize_module_constant_cfg, CfgCanonicalizationReport,
    SirOptimizationError,
};
pub use verify::{
    verify_function, verify_function_in_module, verify_module, SirDiagnostic, SirDiagnosticKind,
};
