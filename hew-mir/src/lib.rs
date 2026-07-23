//! Local v0.5 THIR/MIR vertical slice.
//!
//! This crate deliberately models all ladder stages even while the first
//! executable subset is small. Later slices replace the placeholder lowering
//! with full CFG, borrow, and drop implementations without changing the
//! stage boundaries.

pub mod closure_env;
pub mod dataflow;
pub mod dump;
pub mod dyn_vtable_registry;
pub mod ffi_contracts;
pub mod liveness;
pub mod lower;
pub mod model;
pub mod ownership;
pub mod return_provenance;
pub mod runtime_call;
pub mod runtime_symbols;
pub mod state_clone;
pub mod thunk_requirements;

pub use dataflow::local_is_written_in_body;
pub use lower::{
    bracket_actor_handler_blocks, build_const_descriptors, instr_source_places, lower_hir_module,
    lower_hir_module_with_facts, terminator_source_places, validate_outbound_actor_modes,
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
    classify_extern_string_ownership, container_ingress_is_copy_in, is_indirect_enum,
    machine_enum_view, machine_enum_views, mangle_dyn_drop_in_place_symbol,
    mangle_dyn_thunk_symbol, mangle_dyn_vtable_symbol, ty_contains_closure_value,
    ty_contains_heap_owning, ty_contains_unclonable_opaque,
    ty_contains_unclonable_opaque_with_names, ty_heap_ownership, ty_owns_heap, ty_owns_heap_mir,
    validate_context_markers, ActorHandlerKind, ActorHandlerLayout, ActorLayout,
    ActorStateLoadMode, AggregateOwner, BasicBlock, BlockKind, BorrowKind, CaptureKind,
    CheckedMirFunction, ChildInitArg, ClosureEnvAllocation, ClosureEnvFieldInit,
    ClosureEnvFieldOwnership, ClosureEnvMode, CmpPred, CooperateKind, CooperateSite,
    CoroutineFacts, CoroutineSchema, DecisionFact, Direction, DropFnSpec, DropKind, DropPlan,
    DynVtableInstance, ElabBlock, ElabDrop, ElaboratedMirFunction, EnumLayout, ExitPath,
    ExternDecl, ExternStringOwnership, FieldAddr, FieldOffset, FloatWidth, FunctionCallConv,
    GeneratorEnvFieldPlan, GeneratorEnvPlan, HeapOwnership, HeapOwnershipLayouts, Instr,
    IntArithOp, IntSignedness, IrPipeline, JoinBranch, LambdaActorShape, LambdaCapture,
    LambdaEnvFieldDrop, MachineLayout, MachineVariantLayout, MirCheck, MirConst, MirConstValue,
    MirDiagnostic, MirDiagnosticKind, MirHeapLayouts, MirLint, MirScope, MirStatement,
    ModuleCapabilities, Place, PointerWidth, PolymorphicMirFunction, PoolCount,
    PreparedCarrierBoundary, ProjectedPayloadRejectReason, RawMirFunction, RecordLayout,
    RegexLiteral, RuntimeCall, SelectArm, SelectArmKind, SendAliasMode, SourceOrigin,
    SpawnEnvFieldOwnership, StableActorRole, Strategy, SupervisorChildLayout,
    SupervisorConfigParam, SupervisorLayout, SuspendKind, Terminator, ThirFunction,
    TraitObjectStorage, TrapKind, WitnessOperand, GEN_BODY_PREFIX,
};
pub use ownership::{
    AbiClass, CowHeapRelease, DropClass, FailClosedReason, HandleRole, HeapLeaf,
    InPlaceReleaseKind, LayoutClass, OwnershipCtx, OwnershipDecision, PlaceProvenance, Projection,
    ProvenanceOrigin, ValueOwnership, ValueProvenance,
};
pub use runtime_symbols::UnknownRuntimeSymbol;
pub use state_clone::{
    classify_actor_state_fields, classify_actor_state_fields_with_enum_layouts,
    classify_actor_state_fields_with_opaque_handles,
    classify_actor_state_fields_with_resource_handles, classify_owned_string_record_fields,
    classify_state_field, classify_state_field_full, classify_state_field_with_enum_layouts,
    classify_state_field_with_resource_handles, mangle_actor_state_clone_fn,
    mangle_actor_state_drop_fn, ClassificationError, IoHandleKind, StateFieldCloneKind,
};
pub use thunk_requirements::ThunkSynthesisRequirements;
