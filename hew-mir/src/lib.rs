//! Local v0.5 THIR/MIR vertical slice.
//!
//! This crate deliberately models all ladder stages even while the first
//! executable subset is small. Later slices replace the placeholder lowering
//! with full CFG, borrow, and drop implementations without changing the
//! stage boundaries.

pub mod closure_env;
pub mod dataflow;
pub mod drop_obligation;
pub mod dump;
pub mod dyn_vtable_registry;
pub mod faint;
pub mod ffi_contracts;
pub mod liveness;
pub mod lower;
pub mod model;
pub mod ownership;
pub mod raw_values;
pub mod return_provenance;
pub mod runtime_call;
pub mod runtime_symbols;
pub mod sir;
pub mod state_clone;
pub mod thunk_requirements;

pub use dataflow::local_is_written_in_body;
pub use lower::{
    bracket_actor_handler_blocks, build_const_descriptors, instr_source_places, is_string_const_ty,
    lower_hir_module, lower_hir_module_with_facts, terminator_source_places,
    validate_outbound_actor_modes,
};

/// Test-only access to the per-Place + per-`ResolvedTy` drop-kind
/// dispatcher. Tests pin the boundary contract that codegen consumes
/// (e.g. `dyn Trait` locals → `DropKind::TraitObject`) without round-
/// tripping through a full pipeline. Not part of the public API; the
/// re-export sits in the crate root so tests in `tests/` can reach it.
///
/// `dyn_storage` is consulted only when `(place, ty)` selects the
/// `DropKind::TraitObject` arm; for every other arm it is ignored.
/// Passing `None` for a `(Local, ResolvedTy::TraitObject)` pair is a
/// fail-closed boundary — the dispatcher panics so the test surfaces
/// the missing side-table population instead of silently picking a
/// default storage.
#[doc(hidden)]
#[must_use]
pub fn drop_kind_for_test(
    place: Place,
    ty: &hew_types::ResolvedTy,
    dyn_storage: Option<TraitObjectStorage>,
) -> DropKind {
    lower::drop_kind_for_test_only(place, ty, dyn_storage)
}
pub use dump::{dump_mir, DumpStage};
pub use hew_hir::sanitize_for_symbol;
pub use hew_types::short_name;
pub use model::{
    classify_extern_string_ownership, container_ingress_is_copy_in, indirect_closure_callee,
    is_indirect_enum, machine_enum_view, machine_enum_views, mangle_dyn_drop_in_place_symbol,
    mangle_dyn_thunk_symbol, mangle_dyn_vtable_symbol, ty_carries_drop_obligation,
    ty_carries_drop_obligation_mir, ty_contains_closure_value, ty_contains_heap_owning,
    ty_contains_unclonable_opaque, ty_contains_unclonable_opaque_with_names, ty_drop_obligation,
    ty_heap_ownership, ty_owns_heap, ty_owns_heap_mir, validate_context_markers, ActorHandlerKind,
    ActorHandlerLayout, ActorLayout, ActorStateLoadMode, ActorStateStoreHandoff, AggregateOwner,
    BasicBlock, BlockKind, BorrowKind, CallAuthority, CaptureKind, CheckedMirFunction,
    ChildInitArg, CloseObligationRegistry, ClosureEnvAllocation, ClosureEnvFieldInit,
    ClosureEnvFieldOwnership, ClosureEnvMode, ClosurePairVecKind, CmpPred,
    CollectionLayoutProbeKind, CompilerCallKind, CooperateKind, CooperateSite, CoroutineFacts,
    CoroutineSchema, DecisionFact, Direction, DropFnSpec, DropKind, DropObligation, DropPlan,
    DynVtableInstance, ElabBlock, ElabDrop, ElabDropGuard, ElaboratedMirFunction, EnumLayout,
    ExitPath, ExternDecl, ExternStringOwnership, FieldAddr, FieldOffset, FloatWidth,
    FunctionCallConv, GeneratorEnvFieldPlan, GeneratorEnvPlan, HeapOwnership, HeapOwnershipLayouts,
    IdentityAggregateKind, Instr, IntArithOp, IntSignedness, IrPipeline, JoinBranch,
    LambdaActorShape, LambdaCapture, LambdaEnvFieldDrop, MachineLayout, MachineVariantLayout,
    MirCheck, MirConst, MirConstValue, MirDiagnostic, MirDiagnosticKind, MirHeapLayouts, MirLint,
    MirScope, MirStatement, ModuleCapabilities, NeutralizeAuthority, OwnerId, OwnershipEvent,
    OwnershipGuardKind, ParamBoundaryFact, ParamBoundaryMode, ParamCrashCleanupKind,
    ParamLoanStorage, ParamRepresentationEffect, Place, PointerWidth, PolymorphicMirFunction,
    PoolCount, PreparedCarrierBoundary, ProjectedPayloadRejectReason, RawMirFunction, RawValueDef,
    RawValueId, RawValueOp, RecordLayout, RegexLiteral, RuntimeCall, SelectArm, SelectArmKind,
    SendAliasMode, SourceOrigin, SpawnEnvFieldOwnership, StableActorRole, Strategy,
    StringRetainCondition, SupervisorChildLayout, SupervisorConfigParam, SupervisorLayout,
    SuspendKind, Terminator, ThirFunction, TraitObjectStorage, TrapKind,
    ValueMaterializationReason, WitnessOperand, GEN_BODY_PREFIX, INDIRECT_CLOSURE_CALLEE,
};
pub use ownership::{
    AbiClass, CowHeapRelease, DropClass, FailClosedReason, HandleRole, HeapLeaf,
    InPlaceReleaseKind, LayoutClass, OwnershipCtx, OwnershipDecision, PlaceProvenance, Projection,
    ProvenanceOrigin, ValueOwnership, ValueProvenance,
};
pub use raw_values::{
    is_supported_raw_virtual_scalar_type, is_supported_raw_virtual_value_type,
    raw_uses_virtual_values, verify_raw_virtual_value_checked, verify_raw_virtual_value_elaborated,
    verify_raw_virtual_value_function, verify_raw_virtual_value_ladder, RawVirtualValueError,
    RawVirtualValueFacts,
};
pub use runtime_symbols::UnknownRuntimeSymbol;
pub use sir::{
    apply_sir_to_pipeline, lower_closed_scalar_component, SirMirComponent, SirMirLoweringError,
    SirMirLoweringReport, SirMirLoweringStatus,
};
pub use state_clone::{
    classify_actor_state_fields_with_lifecycle_registry, classify_owned_string_record_fields,
    classify_state_field_with_lifecycle_registry, mangle_actor_state_clone_fn,
    mangle_actor_state_drop_fn, ClassificationError, IoHandleKind, ResourceCloseAuthority,
    StateFieldCloneKind,
};
pub use thunk_requirements::ThunkSynthesisRequirements;
