//! Hew Semantic IR (SIR).
//!
//! SIR is the value-oriented SSA layer between resolved HIR and the existing
//! ownership/layout MIR ladder. Semantic places carry ownership and lifetime
//! contracts; SIR contains no machine allocation,
//! ABI carrier, byte-offset, or LLVM operation.  The strict `--sir-lower` lane
//! owns a conservative subset today; each supported family moves onto
//! SIR -> MIR and deletes its established HIR -> MIR body lowering.

mod actor;
mod analysis;
mod callable;
mod capability;
mod defer;
mod dump;
mod lifetime;
mod lower;
mod model;
mod optimize;
mod ownership;
mod projection;
mod resource;
mod task_scope;
mod verify;

pub use actor::{
    ActorId, ActorOperation, SemActor, SemActorField, SemActorHandler, SemActorOverflow,
};
pub use analysis::{
    build_cfg_index, build_def_use, compute_dominators, replace_all_uses, replace_use, CfgIndex,
    DefUseIndex, Dominators, EdgeRef, RewriteError,
};
pub use callable::{
    callable_parts, callable_value_signature, generator_parts, verify_callable_coercion, ClosureId,
    ClosureInstanceKey, SemCaptureField, SemClosure,
};
pub use capability::{derived_capability_components, SemValueMethodPlan};
pub use dump::{dump_lowering, dump_sir};
pub use lifetime::{CleanupMode, PlaceLifetimes};
pub use lower::{
    lower_module, lower_module_with_demand, lower_module_with_roots, LoweredModule,
    SirLoweringDemand, SirLoweringStatus, SirRootSelectionError, SirSourceStatus,
};
pub use model::{
    runtime_variant_shape_refs, AggregateShapeId, AggregateShapeRef, BlockArg, BlockId,
    BoundaryOperand, CallResult, CallUnwind, CallableId, CallableInstance, CheckedFailure, DeferId,
    DeferScopeId, Edge, EffectSet, FaultParkId, FunctionSourceOrigin, GenericTemplateId, OpId,
    Operand, OperandSlot, Provenance, RuntimeVariantShapeRefs, SemAbiParam, SemAggregateField,
    SemAggregateShape, SemBlock, SemCallConv, SemCallable, SemCallableKind, SemFunction,
    SemFunctionIndex, SemGenericTemplate, SemModule, SemOp, SemOpKind, SemParamPassing,
    SemSignature, SemTerminator, SemVariant, SemVariantArm, SemVariantField, SemVariantShape,
    SirInstanceKey, SuccessorSlot, UseSite, ValueDef, ValueId, VariantShapeId,
};
pub use optimize::{
    canonicalize_module_constant_cfg, CfgCanonicalizationReport, SirOptimizationError,
};
pub use ownership::{
    aggregate_field_recipes, aggregate_field_types, checked_binary_failure_kinds,
    checked_binary_types_match, pipe_parts, runtime_failure_trap_kind, sink_element,
    stream_element, variant_field_recipes, variant_field_types, AggregateFieldRecipe, Binding,
    BindingId, BindingTarget, BoundaryDecision, BytesLiteralId, OwnKind, OwnerRoot, PlaceBase,
    PlaceDecl, PlaceId, PlaceOrigin, SnapshotDecision, StringLiteralId, SuspendKind, TaskScopeId,
    TaskScopeJoinMode, TrapKind, ValueCloseSelection,
};
pub use projection::{place_plan, AggregateProjection, AggregateProjectionStep, PlacePlan};
pub use resource::{verify_resource_release, ResourceCarrier, ResourceExtern, ResourceRelease};
pub use verify::{
    check_module, place_lifetimes, verify_function, verify_function_in_module, verify_module,
    CfgDiscardSafetyReason, CheckedFunction, CheckedModule, SirDiagnostic, SirDiagnosticKind,
};

pub use hew_hir::HirSelectionOrder as TaskSelectionOrder;
