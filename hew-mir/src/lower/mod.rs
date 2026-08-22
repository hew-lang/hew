// The MIR `lower_value` consumer fails closed on
// `HirExprKind::ResolvedImplCall` and the supporting walkers visit the
// `#[deprecated]` `CallTraitMethodStatic` exhaustively. Allowlist test on
// construction sites is the structural enforcement.
#![allow(
    deprecated,
    reason = "legacy CallTraitMethodStatic variant is allowlist-gated; \
              see hew-hir/tests/call_trait_method_static_creation_allowlist.rs"
)]

use std::{
    cell::RefCell,
    collections::{BTreeMap, HashMap, HashSet},
    rc::Rc,
};

use hew_hir::stdlib_catalog;
use hew_hir::{
    named_type_names, BindingId, HirActorDecl, HirBinding, HirBlock, HirConstValue, HirExpr,
    HirExprKind, HirFn, HirItem, HirJoin, HirLifecycleHookKind, HirLiteral, HirMachineDecl,
    HirMachineTransition, HirModule, HirNodeId, HirProducedValueFact, HirProducedValueRelation,
    HirSelect, HirSelectArmKind, HirStmt, HirStmtKind, HirSupervisorChild, HirSupervisorDecl,
    HirVarSelfMethodTarget, IntentKind, ResolvedRef, ResourceMarker, ScopeId, SiteId, ValueClass,
};
use hew_parser::ast::{BinaryOp, UnaryOp};
use hew_types::runtime_call::ConsumeVerdict;
use hew_types::{
    short_name, BuiltinType, ChildKind, ChildSlot, ExecutionContextReader, NumericMethodFamily,
    NumericMethodOp, NumericSignedness, ProducedValueOwnership, ResolvedTy,
};

use crate::dataflow;
use crate::model::{
    ActorHandlerLayout, ActorLayout, ActorStateLoadMode, AggregateOwner, BasicBlock, BlockKind,
    CheckedMirFunction, ClosureEnvAllocation, ClosureEnvFieldInit, ClosureEnvFieldOwnership,
    CmpPred, CoalesceKeyEntry, CoalesceKeyKind, CoalesceKeyPlan, DecisionFact, DropKind, DropPlan,
    ElabBlock, ElabDrop, ElaboratedMirFunction, ExitPath, FieldOffset, FloatWidth,
    FunctionCallConv, Instr, IntArithOp, IntSignedness, IrPipeline, JoinBranch, LambdaCapture,
    MirCheck, MirConst, MirConstValue, MirDiagnostic, MirDiagnosticKind, MirStatement,
    ParamBoundaryFact, ParamBoundaryMode, ParamCrashCleanupKind, ParamLoanStorage,
    ParamRepresentationEffect, Place, PointerWidth, ProjectedPayloadRejectReason, RawMirFunction,
    RecordLayout, SelectArm, SelectArmKind, SendAliasMode, SourceOrigin, SpawnEnvFieldOwnership,
    StableActorRole, Strategy, StringRetainCondition, SuspendKind, Terminator, ThirFunction,
    TraitObjectStorage, TrapKind,
};
use crate::ownership::FailClosedReason;
use crate::ownership::LayoutClass;
use crate::ownership::OwnershipCtx;
use crate::ownership::OwnershipDecision;
use crate::ownership::PlaceProvenance;
use crate::ownership::Projection;
use crate::ownership::ReleaseSymbolVerdict;
use crate::ownership::ValueOwnership;
use crate::ownership::ValueProvenance;
use crate::ownership::VecElementRelease;
use crate::state_clone::StateFieldCloneKind as SnapshotFieldKind;

mod actor;
mod actor_state_handle;
mod assign;
mod borrowed_argument_owner;
mod cfg_util;
mod closure_gen;
mod composite_own;
mod consts;
mod control_flow;
mod drop_plan;
mod expr;
mod facts;
mod for_await_drop_plan;
mod machine_own;
mod machine_synth;
mod move_value;
mod owner_mint;
mod ownership;
mod pattern;
mod rc_intrinsic;
mod scope;
mod split_consume;
mod suspend_places;
mod task;
mod temp_drop;
mod vec_index;

use self::pattern::{project_match_ownership_mode, ProjectMatchOwnershipMode};

/// The owner-mint warrant. Re-exported here so every lowering submodule reaches
/// it through `super::`, exactly as it reaches [`Builder`] — while the type's
/// fields, and therefore its construction, stay private to
/// [`self::owner_mint`]. See that module for why that is the whole close.
pub(crate) use self::owner_mint::OwnerMintWarrant;

/// Crate-visible re-export of the layout-key mangler for the MIR-owned
/// thunk-synthesis registry (`crate::thunk_requirements`), which must resolve
/// generic-instantiation seed keys under the SAME bare-normalised spine the
/// layout registration uses.
pub(crate) use self::facts::mangle_layout_key;

#[cfg(not(test))]
use self::actor_state_handle::{
    detect_actor_state_handle_consume, detect_actor_state_resource_overwrite,
};
#[cfg(not(test))]
use self::cfg_util::{
    block_by_id, block_dominators, blocks_reachable_from, call_terminator_next,
    local_is_rewritten_after_current_iteration, local_is_used_after, shift_instr_spans_on_insert,
};
#[cfg(not(test))]
use self::composite_own::{
    apply_escaped_record_sibling_field_drops,
    derive_borrowed_builtin_handle_projection_alias_bindings,
    derive_consumed_local_aggregate_member_bindings, derive_enum_composite_drop_allowed,
    derive_local_bytes_drop_allowed, derive_local_collection_drop_allowed,
    derive_owned_record_drop_allowed, derive_owned_tuple_handle_projection_bindings,
    derive_returned_aggregate_member_bindings, derive_returned_member_transfer_blocks,
    derive_spawn_consumed_handle_bindings, derive_tuple_composite_drop_allowed,
    detect_builtin_handle_record_field_overwrite, detect_opaque_resource_field_misuse,
    detect_unproven_aggregate_handle_double_free,
};
#[cfg(not(test))]
use self::consts::{
    actor_name_from_handle_ty, actor_name_from_remote_pid_ty, build_down_hook_body,
    build_exit_hook_body, check_function, cmp_select_by_signedness, context_reader_offset,
    crash_action_return_ty, float_width, integer_bit_width, integer_signedness,
    is_canonical_lifecycle_named_ty, is_crash_info_payload_ty, is_self_expr,
    is_unit_close_error_result, is_unit_send_error_result, literal_match_scrutinee_ty,
    method_name_from_id, named_type_marker, numeric_method_op, numeric_method_signedness,
    recv_result_payload_ty, register_builtin_monomorphic_enum_layouts,
    register_builtin_record_layouts, register_lifecycle_hook_layouts, runtime_symbol_for_call_expr,
    signed_min_value, unary_op_label, unresolved_fn_sig_reason,
};
pub use self::consts::{build_const_descriptors, is_string_const_ty};
#[cfg(not(test))]
use self::drop_plan::{
    affine_release_needs_drop_flag, attribute_payload_binder_root,
    binder_read_is_borrow_safe_instr, binder_read_is_borrow_safe_terminator,
    builtin_method_arg_is_move_ingress, check_to_diagnostic, classify_closure_pair_rhs,
    classify_dyn_trait_storage, cow_value_leaf_drop_symbol, describe_vec_element,
    dyn_rebind_source_binding, elaborate, exit_block_id, field_override_uses_record_field_drop,
    is_borrowing_call_abi, is_handle_borrowing_call_abi, note_payload_escape,
    propagate_payload_binder_root, render_owned_handle_ty, resource_drop_fn,
    stream_handle_drop_descriptor, string_binder_read_is_user_fn_borrow, ty_is_closure_pair,
    ty_is_generator_handle, ty_is_heap_owning_enum_composite, ty_is_heap_owning_tuple,
    ty_is_indirect_enum, ty_is_local_collection_handle, ty_is_nonowning_handle_leaf,
    ty_is_owned_handle_leaf, ty_is_stream_handle, ty_is_vec, validate_discharge_authority,
    validate_discharge_authority_corroboration, validate_drop_plan, validate_field_drop_in_place,
    validate_obligation_balance, vec_iter_init_vec_source_expr, vec_iter_let_cursor_owns_handle,
    vec_iter_yield_abandonment_diagnostics, PayloadBinderRoot,
};
pub use self::drop_plan::{crash_only_cleanup_drop, drop_kind_for_test_only};
pub(crate) use self::facts::*;
#[cfg(not(test))]
use self::machine_synth::{
    actor_symbol_base, build_machine_layout, build_supervisor_layout, lower_actor_body_handlers,
    lower_actor_handler_layouts, lower_supervisor_bootstrap, machine_emit_type_id,
    mangle_actor_crash_handler, mangle_actor_down_handler, mangle_actor_exit_handler,
    mangle_actor_init_handler, mangle_actor_lifecycle_wrapper, mangle_actor_start_handler,
    mangle_actor_stop_handler_indexed, mangle_machine_step, mangle_supervisor_bootstrap,
    synthesize_machine_step_fn, ProtocolDescriptorMissing,
};
#[cfg(not(test))]
use self::split_consume::{
    alias_projection_chain_owner_seeds, attribute_field_binder_provenance, base_local,
    binding_ref_target, check_duplex_split_state, close_alias_binders_forward,
    collect_record_field_binders, descend_match_bound_hop_alias_chain,
    descend_match_bound_hop_aliases, local_is_byte_copy_aggregate, place_is_interior_projection,
    place_is_tag_read, propagate_seeded_whole_value_alias_roots_excluding_moves,
    propagate_whole_value_alias_roots, propagate_whole_value_alias_roots_excluding_moves,
    validate_cross_block_split_consume,
};
pub use self::suspend_places::instr_source_places;
pub use self::suspend_places::suspend_kind_source_places;
pub use self::suspend_places::terminator_is_suspend_carrier;
pub use self::suspend_places::terminator_source_places;
#[cfg(not(test))]
use self::suspend_places::{
    generator_yield_instr_escapes, generator_yield_terminator_escapes,
    hir_expr_contains_synthetic_vec_get_clone, instr_escape_places, option_payload_ty,
    place_refs_local, retained_string_terminator_drop_safe, terminator_escape_places,
};
#[cfg(not(test))]
use self::temp_drop::{
    aggregate_borrowed_ingress_clone_sites, aggregate_projection_transfer_dests,
    apply_nested_fresh_bytes_temp_drops, apply_nested_fresh_string_temp_drops,
    bytes_interior_producer_dest, bytes_place_is_typed, bytes_runtime_arg_is_borrow,
    bytes_share_sink_places, classify_actor_state_load_modes,
    close_obligated_borrow_alias_violations, collection_borrow_getter_alias_locals,
    compute_collection_interior_alias_taint, compute_projection_alias_taint,
    derive_bytes_actor_transfer_blocks, derive_cow_fresh_borrowed_owner, derive_cow_sole_owner,
    finalize_bytes_ownership, finalize_string_local_share_intents, finalize_string_ownership,
    forward_move_closure, readmit_retained_bytes_tuple_roots, string_call_borrows,
    string_field_load_producer_dest,
};

/// Maps each original (unsanitized) callee symbol to the adapter symbol
/// generated for it. `mir_sanitize_symbol` maps every non-alphanumeric byte to
/// `'_'`, which is lossy and non-injective (e.g. a generic monomorphization
/// mangled as `worker$$i64` and an unrelated concrete function literally named
/// `worker__i64` both sanitize to the same `worker__i64` name). Naively
/// generating `__hew_task_entry_<sanitized>` per callee and deduping on either
/// the sanitized name (wrong: two distinct callees silently share one
/// adapter, so the second callee's task body never runs) or the plain
/// sanitized name with dedup keyed on the original symbol instead (wrong: two
/// distinct adapters would still collide on the same generated function
/// name) both break. Keying this map by the original callee symbol and
/// disambiguating with a numeric suffix on collision keeps every adapter
/// symbol both stable-per-callee and unique-per-distinct-callee.
type TaskEntryAdapterSymbols = Rc<RefCell<HashMap<String, String>>>;

#[derive(Debug, Clone, Copy)]
struct GeneratorShellCallGate;

// These literals mirror the runtime's offset_of!-derived authorities. MIR
// cannot depend on the runtime in production because the dependency points
// from the runtime toward compiler-independent crates; native tests compare
// this copy with the runtime so layout changes fail at the boundary.
const HEW_CTX_OFFSET_ACTOR_ID: usize = 8;
const HEW_CTX_OFFSET_PARENT_SUPERVISOR: usize = 16;
const HEW_CTX_OFFSET_TRACE: usize = 56;
const HEW_TRACE_OFFSET_SPAN_ID: usize = 16;
const HEW_CTX_OFFSET_TRACE_SPAN: usize = HEW_CTX_OFFSET_TRACE + HEW_TRACE_OFFSET_SPAN_ID;

/// Synthetic MIR record type name for the `ChildLookupResult` C-ABI struct
/// returned by `hew_supervisor_child_get`. Registered unconditionally in every
/// module so the `FieldAccess` intercept arm can use `RecordFieldLoad` on the
/// struct-return place. S3 codegen recognises this name and emits the correct
/// LLVM struct ABI (`{ i8, i8, [6 x i8], ptr }` at the wire level).
///
/// WHY a synthetic name rather than a user-visible record: `ChildLookupResult`
/// is a runtime-internal type; user programs never name or construct it directly.
/// The double-underscore prefix (`__`) is outside the user-identifier namespace.
const CHILD_LOOKUP_RESULT_TY_NAME: &str = "__HewChildLookupResult";

/// Per-call `receive gen fn` stream channel capacity
/// (`lower_actor_gen_stream`). A laziness/throughput tunable: delivery is
/// eager producer drive with bounded-channel backpressure over strict
/// pull-per-element dispatch; a small buffer keeps the consumer experience
/// pull-equivalent without a user-facing capacity-tuning surface.
const RECEIVE_GEN_STREAM_CAPACITY: i64 = 8;

/// Sentinel HIR IDs for the synthetic `__crash_code: i64` ABI binding injected
/// into `#[on(crash)]` handler prologues.
///
/// Checker-allocated IDs count upward from 0; these live at `u32::MAX` to avoid
/// collision with any real binding, site, or node emitted during type-checking.
/// One sentinel value per kind suffices because the injected binding is local to
/// the synthetic function scope and is never referenced outside it.
const SENTINEL_CRASH_CODE_BINDING: BindingId = BindingId(u32::MAX);
const SENTINEL_CRASH_CODE_SITE: SiteId = SiteId(u32::MAX);
const SENTINEL_CRASH_CODE_NODE: HirNodeId = HirNodeId(u32::MAX);

/// Sentinel HIR binding ID for the synthetic `__crash_message: string` ABI
/// param injected into `#[on(crash)]` handler prologues (M-3 ABI reshape).
///
/// The `HewOnCrashFn` ABI carries the crash message as a `*const c_char` (typed
/// `ResolvedTy::String` here, which lowers to an LLVM `ptr`). It is a BORROW per
/// the P0 `by-value-heap-params-are-borrows` invariant — the supervisor owns the
/// underlying Hew header-aware buffer and frees it after the call — so M-5's
/// prologue CLONES it (`hew_string_clone`, a refcount bump) into the owned
/// `CrashInfo.message` field (a fresh `+1` owner the function frame drops once via
/// `hew_string_drop`), rather than moving the borrow (a move would double-free
/// with the supervisor's release). Because the prologue uses the Hew string
/// primitives (which read a 16-byte refcount header), the supervisor MUST hand in
/// a Hew header-aware allocation, not a bare Rust `CString` — see
/// `supervisor.rs::invoke_on_crash_handler`, which allocates via `str_to_malloc`
/// and releases via `free_cstring`.
///
/// Known follow-up (#2252): when the hook body reads `info.message` via a
/// borrowing call, the `hew_string_clone` retain temp in the synthetic prologue
/// is not yet released by drop-elaboration, leaking ~32 B per crash. The narrow
/// fix regressed the generic record-clone drop path; both must be fixed together.
/// Lives one below `__crash_code`'s sentinel to stay clear of real bindings.
const SENTINEL_CRASH_MESSAGE_BINDING: BindingId = BindingId(u32::MAX - 1);

/// Sentinel HIR binding IDs for the synthetic `#[on(exit)]` (M-7-R) ABI params:
/// `__exit_actor_id: u64` and `__exit_kind_tag: i32`. The runtime delivers a
/// linked actor's `CrashNotification` as these two raw fields; the hook prologue
/// rebuilds `note = CrashNotification { actor_id, kind }` from them.
const SENTINEL_EXIT_ACTOR_ID_BINDING: BindingId = BindingId(u32::MAX - 2);
const SENTINEL_EXIT_KIND_TAG_BINDING: BindingId = BindingId(u32::MAX - 3);

/// Sentinel HIR binding ID for the synthetic generator-companion local the
/// `receive gen fn` stream-producer pump drives. The pump nests an inner
/// generator handle (`gen_place`) that has NO real HIR binding — a standalone
/// `gen fn` returns its handle to a caller who owns and drops it, but the pump
/// consumes the handle in place and had no owner for it, so its coroutine frame
/// and heap companion leaked on every pump exit. Registering `gen_place` under
/// this sentinel gives the drop-elaboration authority an owner, so
/// `hew_gen_coro_destroy` fires on every pump exit (Return, Panic, Cancel).
///
/// Must be distinct from the sink param's `BindingId(u32::MAX)` — BOTH live in
/// the pump function's `binding_locals` at once — and from the crash/exit
/// sentinels above (those are local to their own synthetic functions, but a
/// dedicated value keeps the pump self-contained and future-proof).
const SENTINEL_RECV_GEN_COMPANION_BINDING: BindingId = BindingId(u32::MAX - 4);

/// Sentinel HIR binding IDs for the synthetic `#[on(down)]` ABI params.
const SENTINEL_DOWN_MONITOR_ID_BINDING: BindingId = BindingId(u32::MAX - 5);
const SENTINEL_DOWN_TARGET_KIND_BINDING: BindingId = BindingId(u32::MAX - 6);
const SENTINEL_DOWN_REASON_KIND_BINDING: BindingId = BindingId(u32::MAX - 7);
const SENTINEL_DOWN_LOCATION_BINDING: BindingId = BindingId(u32::MAX - 8);
const SENTINEL_DOWN_LOCAL_SLOT_BINDING: BindingId = BindingId(u32::MAX - 9);
const SENTINEL_DOWN_CRASH_KIND_BINDING: BindingId = BindingId(u32::MAX - 10);

/// Base of the per-function synthetic binding-id range for anonymous
/// caller-owned temps. From-call match/while-let scrutinees and discarded
/// caller-owned `Option<T>` results all materialise into MIR locals without a
/// user binding; without a `BindingId` they are invisible to the ordinary
/// sole-owner/drop-plan machinery.
///
/// The range DESCENDS from here (`base`, `base - 1`, …), one id per anonymous
/// owner in the function, so it can never collide with the fixed
/// sentinels above (`u32::MAX ..= u32::MAX - 4`). Collision with real HIR
/// binding ids would need a function with ~4.29 billion bindings — outside
/// any representable source. If HIR ever exposes a per-function binding-id
/// ceiling, mint above it instead and retire this reserved range.
const SYNTHETIC_OWNED_TEMP_BINDING_BASE: u32 = u32::MAX - 64;

const SYNTHETIC_CALL_SCRUTINEE_NAME: &str = "__hew_call_scrutinee";
const SYNTHETIC_PROJECTED_SCRUTINEE_NAME: &str = "__hew_projected_scrutinee";
const SYNTHETIC_WHILE_LET_ITERATION_NAME: &str = "__hew_while_let_iteration";
const SYNTHETIC_DISCARDED_CALL_RESULT_NAME: &str = "__hew_discarded_call_result";
/// Name for the #2743 synthetic owner minted over a fresh owned composite/string
/// argument TEMPORARY (`g(Row{..})`, `f(E::A(..))`, `tf((..,..))`, `h("a"+"b")`)
/// passed to a proven-BORROW parameter. The temporary has no user `let`, so no
/// `BindingId` and no scope-exit drop; #2735's preserve-the-drop exemption is a
/// no-op for it (nothing to preserve). Minting a synthetic owner routes it
/// through the SAME `owned_locals` + `proven_borrow_whole_arg_locals` machinery
/// as the named `let x = Row{..}; g(x)` shape, so the fresh value is dropped
/// once at caller scope exit. Gated on the target param being BORROW (a CONSUME
/// target's temporary is the callee's obligation — no caller drop).
const SYNTHETIC_TEMP_ARG_NAME: &str = "__hew_temp_arg";
/// Name for a provisional owned result completed when a resolved method borrows
/// it as an anonymous receiver. Named receivers keep their ordinary binding;
/// specialised cursor rewrites keep their dedicated transfer authority.
const SYNTHETIC_TEMP_RECEIVER_NAME: &str = "__hew_temp_receiver";
/// Name for the owner minted over a fresh Vec COPY-IN element temporary when
/// every whole by-value parameter embedded in it is a retained string share.
/// The binding owns the temporary's retained share only; the parameter remains
/// caller-owned and keeps its natural drop.
const SYNTHETIC_COPY_IN_PARAM_TEMP_NAME: &str = "__hew_copy_in_param_temp";

/// Prefix of the synthetic for-iteration cursor binding minted by the HIR
/// for-loop desugar (`hew-hir/src/lower.rs`, `lower_for_iter_desugar`:
/// `format!("__hew_for_iter_{}", …)`). The scope-exit `Stream`/`Receiver` close
/// (3b-1) is scoped to THIS cursor only: it is internal to the loop and never
/// returned or consumed elsewhere, so closing it at block-scope exit is always
/// safe. A user `let s = <stream>` binding — which may be RETURNED (e.g.
/// `std/stream.hew`'s `bytes_pipe` returns its `let input = …` stream) or moved
/// into another binding — must keep its move-checked function-exit drop-plan
/// close instead, or the inline close would free a handle that was moved out.
const FOR_ITER_CURSOR_NAME_PREFIX: &str = "__hew_for_iter_";

/// `CrashKind` variants in declaration order (`std/failure.hew`). An
/// `#[on(exit)]` prologue builds the `kind` field by matching the runtime
/// `__exit_kind_tag` against these (Crashed=0, HeapExceeded=1,
/// PartitionDetected=2).
const CRASH_KIND_VARIANTS: &[&str] = &["Crashed", "HeapExceeded", "PartitionDetected"];

#[derive(Debug, Clone)]
struct ActorMethodInfo {
    msg_type: i32,
    param_tys: Vec<ResolvedTy>,
    return_ty: ResolvedTy,
}

#[derive(Debug, Clone)]
struct PendingOutboundArg {
    source: Place,
    ty: ResolvedTy,
    site: SiteId,
}

#[derive(Debug, Clone)]
struct ResolvedOutboundArg {
    source: Place,
    ty: ResolvedTy,
    site: SiteId,
    mode: SendAliasMode,
}

#[derive(Debug, Clone, Copy, Default)]
pub(crate) enum PendingOutboundTarget {
    #[default]
    Direct,
    SelectArm(usize),
    JoinBranch(usize),
}

#[derive(Debug, Clone, Default)]
struct PendingOutboundSite {
    target: PendingOutboundTarget,
    args: Vec<PendingOutboundArg>,
}

#[derive(Debug, Clone)]
struct PendingOwnedCallArg {
    index: usize,
    source: Place,
    ty: ResolvedTy,
    site: SiteId,
    source_is_prepared_owner: bool,
}

#[derive(Debug, Clone, Default)]
struct PendingOwnedCallSite {
    args: Vec<PendingOwnedCallArg>,
}

#[derive(Debug, Clone)]
enum OwnedCarrierNeutralizeTarget {
    Whole(Place),
    ScopeExitTuple {
        root: Place,
        owner: (BindingId, String, SiteId),
    },
    Projection {
        root: Place,
        fields: Vec<u32>,
        scope_exit_owner: Option<(BindingId, String, SiteId)>,
    },
}

#[derive(Debug, Clone)]
struct OwnedCarrierParam {
    value: Place,
    guard: Place,
    ty: ResolvedTy,
    plan: crate::state_clone::ValueSnapshotPlan,
}

/// Snapshot-plan roots that never enter the owned call-carrier protocol.
///
/// `BitCopy` carries no release authority. Whole-`string`/`bytes` values
/// stay on the `CoW` borrow spine: the caller owns the buffer and every
/// co-owner mint (let-share, return-of-param, copy-in embed) is balanced by
/// that spine's retain-site derivation, which classifies parameter locals
/// as borrowed sources. This predicate is the single home for the decision
/// — both admission halves consult it (`register_owned_call_carrier_param`
/// on the callee, `prepare_owned_call_carriers` on the caller) so the two
/// sides cannot drift apart. `lower_direct_call_args` also consults it only
/// when forwarding an already-tracked carrier projection or payload binder:
/// String/Bytes still discharge the enclosing carrier through the funnel, but
/// the binder/projection local keeps the transferred count's elaborated drop
/// while the callee borrows (copy-in sinks mint independent owners) — verified
/// leak- and scribble-clean in both liveness directions.
pub(crate) fn snapshot_root_outside_carrier_protocol(root: &SnapshotFieldKind) -> bool {
    matches!(
        root,
        SnapshotFieldKind::BitCopy { .. } | SnapshotFieldKind::String | SnapshotFieldKind::Bytes
    )
}

#[derive(Debug, Clone)]
struct ResolvedOutboundSite {
    target: PendingOutboundTarget,
    args: Vec<ResolvedOutboundArg>,
}

#[derive(Debug, Default)]
#[allow(
    clippy::struct_excessive_bools,
    reason = "Builder is the MIR-lowering state accumulator; each bool is an \
              independent, orthogonal lowering-mode flag (e.g. the \
              fallthrough-match-guard scope) that distinct call sites set and \
              query on their own — collapsing them into an enum would force a \
              single active-mode invariant the lowering does not have"
)]
struct Builder {
    /// Checker-authority stream for the *current* basic block. Drained
    /// into a `BasicBlock` when the cursor moves (`finish_current_block`)
    /// or at function-body finalisation. Once a block is sealed it lives
    /// in `pending_blocks` until the function's body walk completes.
    pub(crate) statements: Vec<MirStatement>,
    /// Total append-only declaration order for every binding introduced in
    /// this function, including parameters and bindings that deliberately do
    /// not enter `owned_locals` (for example tuple-derived channel handles and
    /// their synthetic `for await` cursors). Drop-plan merges use this as the
    /// LIFO rank authority; the ownership ledger is only an admission ledger
    /// and therefore cannot supply a total lexical order.
    pub(crate) binding_declaration_order: Vec<BindingId>,
    /// Deduplication sidecar for `binding_declaration_order`. A binding may be
    /// observed through a specialised lowering path more than once, but its
    /// declaration ordinal is fixed by the first observation.
    binding_declaration_seen: HashSet<BindingId>,
    /// Backend-authority stream for the *current* basic block. Populated
    /// in lock-step with `statements` by `lower_value` so the checker
    /// and the emitter agree on what each `SiteId` resolves to. Drained
    /// at the same cursor-move site as `statements`.
    pub(crate) instructions: Vec<Instr>,
    /// Raw-only per-block actor-boundary arguments. Lowering records each
    /// source independently before packing; the post-CFG outbound pass resolves
    /// modes and clears this map before checked MIR is built.
    pending_outbound_actor_args: HashMap<u32, Vec<PendingOutboundSite>>,
    /// Direct-call arguments whose callee body is proven to carry the value
    /// into an owning sink. Resolved after CFG construction so liveness and
    /// projection taint choose snapshot versus last-use transfer.
    pending_owned_call_args: HashMap<u32, PendingOwnedCallSite>,
    /// Callee-side parameters admitted by the same carrier summary. Every
    /// terminal path receives the inverse snapshot drop.
    owned_carrier_params: Vec<OwnedCarrierParam>,
    owned_carrier_param_ids: HashSet<BindingId>,
    /// Blocks in which a consuming record/tuple project match took over a
    /// whole owned call-carrier's release authority, keyed by the carrier
    /// slot. `append_owned_carrier_param_drops` classifies each function
    /// exit against these blocks: an exit the consumption cannot reach
    /// keeps the terminal snapshot drop, an exit only reachable through
    /// the consumption skips it, and a return reachable both ways fails
    /// closed — cancelling the registration globally instead would leak
    /// every owned field on a guard / early-return path that branches
    /// around the match.
    owned_carrier_consumed: HashMap<Place, Vec<(u32, SiteId)>>,
    /// Raw byte-copy sources that must be neutralized if their loaded value is
    /// moved onward by the callee.
    owned_carrier_neutralize: HashMap<Place, OwnedCarrierNeutralizeTarget>,
    /// Basic blocks that have consumed each carrier authority. Lowering walks
    /// sibling CFG arms sequentially, so the authority registry itself remains
    /// stable; a transfer is rejected only when an earlier transfer can reach
    /// the current block on the same runtime path.
    owned_carrier_transferred_at: HashMap<Place, Vec<u32>>,
    /// Parameter slots this function does NOT own: by-value params that are
    /// neither consume-classified, nor registered owned carriers, nor
    /// mailbox-delivered (`ActorHandler`), nor #2732 enum-composite consumes.
    /// The original caller keeps ownership and drops them, so the post-CFG
    /// owned-call and outbound preparation passes must never LAST-USE
    /// TRANSFER one into an owning callee or send — the callee/mailbox would
    /// free a buffer the real owner still releases (the `Version::to_string`
    /// borrowed-receiver double-free). Snapshot-clone remains the correct
    /// preparation for these sources.
    borrowed_value_param_locals: HashSet<u32>,
    /// Locals whose original carrier slot has already been neutralized. These
    /// are sole owners, not projection aliases, so a later consuming direct
    /// call transfers them exactly once instead of preparing another clone.
    prepared_owned_call_sources: HashSet<Place>,
    /// Send block -> (pre-liveness-branch preparation block, recover block) for
    /// fungible child tells.
    fungible_outbound_edges: HashMap<u32, (u32, u32)>,
    /// Completed basic blocks in construction order. Block id `0` is the
    /// function's entry block; subsequent ids are monotone in allocation
    /// order. The currently-being-built block (`current_block_id` /
    /// `statements` / `instructions`) is appended at function-body
    /// finalisation. Slice 1 leaves this empty for every function (the
    /// cursor never moves under the CFG-flat lowering); Slice 2's `If`
    /// lowering is the first writer.
    pub(crate) pending_blocks: Vec<BasicBlock>,
    /// Monotone counter for fresh `BasicBlock` ids. `alloc_block` returns
    /// the next id without switching the cursor — the caller is
    /// responsible for `finish_current_block(...)` + `start_block(id)`
    /// at the right point in the lowering sequence.
    pub(crate) next_block_id: u32,
    /// Id of the block currently receiving `statements` / `instructions`.
    /// Initialised to `0` (the entry block). Updated by
    /// `start_block(id)` after a `finish_current_block(...)` seals the
    /// previous block into `pending_blocks`.
    pub(crate) current_block_id: u32,
    /// Set when the most recent `finish_current_block` was followed by
    /// `start_block` for a block that has no predecessor in the CFG —
    /// typically the synthetic continuation block opened after an
    /// explicit early `return` seals its block with `Terminator::Return`.
    /// `seal_body_blocks` drops this block from the function's CFG when
    /// it is observed to be empty, so the dead-end does not appear as
    /// an additional `Return` exit in `drop_plans`.
    pub(crate) cursor_unreachable: bool,
    /// Set alongside `cursor_unreachable` ONLY when the current dead cursor
    /// was opened as the `next` of an already-sealed `Terminator::Call`
    /// (a `Never`-typed callee's continuation) rather than from an
    /// explicit `return`'s own seal. `seal_body_blocks`'s empty-dead-cursor
    /// drop is safe for the `return` case because nothing else references
    /// that block's id (`Terminator::Return` has no successor field), but
    /// a `Terminator::Call { next, .. }` DOES reference this id — dropping
    /// an empty block here would leave the already-committed `Call`
    /// pointing at a missing block (hew-lang/hew#2425:
    /// `E_CODEGEN_FRONT_FAIL_CLOSED: Call next bb<N> missing`, hit whenever
    /// the function-tail defer drain or a plain live-cursor fall-through
    /// reached a `defer exit(..)`/`defer panic(..)` and nothing else wrote
    /// into the resulting dead continuation before finalisation). When
    /// this flag is set, `seal_body_blocks` seals the empty block with
    /// `Terminator::Trap { kind: UnreachableCallContinuation }` instead of
    /// dropping it, so the id stays valid. Cleared by `start_block` and by
    /// every `seal_body_blocks` call, same as `cursor_unreachable`.
    pub(crate) dead_cursor_is_call_continuation: bool,
    /// Type-indexed local registers. `locals[i]` is the `ResolvedTy` of
    /// `Place::Local(i as u32)`.
    pub(crate) locals: Vec<ResolvedTy>,
    /// Source-level binding name for each local slot, parallel to `locals`
    /// (`local_names[i]` is the name of `Place::Local(i)`, or `None` for an
    /// anonymous temporary). Populated at the param prologue and `let`
    /// lowering sites from the HIR `HirBinding.name`; every other
    /// `alloc_local` pushes `None`. Drained into `RawMirFunction.local_names`
    /// for codegen's `-g` variable DIEs (`create_auto_variable` /
    /// `create_parameter_variable`). Best-effort and fail-closed: a `None`
    /// entry means codegen emits no DIE for that slot rather than fabricating
    /// a name. A side-table-shaped `Vec` (not a field on every binding-insert
    /// site) keeps the many `binding_locals.insert` call sites unchanged.
    pub(crate) local_names: Vec<Option<String>>,
    /// gdb `-g` lexical scoping, parallel to `local_names`. `local_scopes[i]` is
    /// the HIR `ScopeId` the binding occupying `Place::Local(i)` was declared in
    /// (or `None` for params / anonymous temporaries / unscoped slots). Resolved
    /// in `resolve_local_names_from_binds` from `binding_scope`. Codegen scopes
    /// each variable DIE to the matching `DILexicalBlock`, so a shadowed inner
    /// `first` does not share the outer's function-wide scope.
    pub(crate) local_scopes: Vec<Option<ScopeId>>,
    /// gdb `-g` declaration line, parallel to `local_names`. `local_decl_bytes[i]`
    /// is the source start-byte of the `let` that introduced `Place::Local(i)`
    /// (or `None`). Codegen maps it to a line so each local DIE carries its own
    /// declaration line rather than the function-declaration line.
    pub(crate) local_decl_bytes: Vec<Option<u32>>,
    /// Maps `BindingId` to the `Local(N)` slot that holds the binding's
    /// initialiser. Cluster 1 reads the slot directly; later clusters add
    /// drop-cleanup and rebinding semantics.
    pub(crate) binding_locals: HashMap<BindingId, Place>,
    /// Owned match-like payload bindings whose lexical mapping is restored
    /// when their body ends, but whose concrete backend place must remain
    /// available to scope-exit drop elaboration. Ownership finalization runs
    /// against the lexical map first; this side table is merged immediately
    /// afterward so it cannot create a spurious live alias or retain.
    pub(crate) deferred_drop_binding_locals: HashMap<BindingId, Place>,
    /// Count of anonymous caller-owned temp bindings minted so far in this
    /// function. The next mint is
    /// `BindingId(SYNTHETIC_OWNED_TEMP_BINDING_BASE - count)` — a
    /// descending per-function range that stays clear of both the fixed
    /// `u32::MAX ..= u32::MAX - 4` sentinels and real HIR binding ids.
    pub(crate) synthetic_owned_temp_bindings: u32,
    /// Producer `SiteId` attached to each synthetic owner generation. This is
    /// identity metadata for the existing owner ledger—not a second cleanup
    /// registry—and rejects a future rewrite of one local as a different
    /// owned publication unless that rewrite has explicitly retired the old
    /// generation first.
    synthetic_owner_publication_sites: HashMap<BindingId, SiteId>,
    /// Source site and user-facing label for direct-call scrutinee
    /// publications, keyed by their MIR local. The MIR binding keeps its
    /// structural synthetic name; only obligation diagnostics read this
    /// identity.
    call_scrutinee_diagnostics: HashMap<u32, (SiteId, String)>,
    /// Synthetic generations minted directly by the checker/HIR produced-value
    /// carrier.  Post-CFG inline releases consume only this subset; legacy
    /// sink owners keep their existing release protocol.
    typed_produced_value_owner_bindings: HashSet<BindingId>,
    /// Exact MIR moves that transfer a typed publication generation into a
    /// named binding. String/bytes ownership finalization uses this evidence to
    /// avoid retaining the moved value as though the source stayed live.
    typed_produced_value_handoffs: HashSet<(Place, Place)>,
    /// Exact successful expression publication places, keyed by HIR `SiteId`.
    /// This is identity-only metadata used for receiver-owner transfer; it
    /// neither schedules nor owns cleanup.
    published_value_places: HashMap<SiteId, Place>,
    /// Narrow lowering context for the `for await` desugar's cursor
    /// initializer. Its existing cursor owner is recorded after the `let`
    /// bind, once the destination scope is known; publishing a second generic
    /// owner for the transient source local would duplicate that authority.
    /// This is not a cleanup registry: it contains only the currently-lowered
    /// expression `SiteId` and is removed immediately after lowering.
    suppress_typed_produced_owner_sites: HashSet<SiteId>,
    /// Per-function destructive-funcupdate base provenance. Maps a `BindingId`
    /// to whether `{ ..<binding>, f: new }` over it is a PROVEN unique owner of
    /// its heap fields — i.e. consuming it leaves no live alias, so the
    /// override-drop's in-place field release is sound. Populated once per
    /// function by `compute_funcupdate_base_provenance` (a flow-insensitive
    /// prescan of the body) BEFORE the body is lowered, so the base allowlist
    /// gate sees every reassignment. A binding is proven iff EVERY definition
    /// (its `let` initialiser, every `=` reassignment, or a by-value parameter
    /// origin) is a materialised owner (`expr_is_materialized_owner`) or a
    /// move-chain of such; a binding bound from a projection of a still-live
    /// owner (`let b = o.inner`), or introduced by any non-`let`/non-param form
    /// (match-arm payload, let-else, loop var), is ABSENT or `false` — the gate
    /// then fails closed. See `base_is_safe_for_destructive_funcupdate`.
    pub(crate) funcupdate_base_proven: HashMap<BindingId, bool>,
    /// Module-global return-provenance context (#2648) for alias and foreign-
    /// value safety checks that are not represented by produced-value facts.
    /// Maps each module-fn `ItemId` to its precise three-state return provenance,
    /// the declared-extern id set, and the audited extern contract table. `Rc` so
    /// child builders share it cheaply; the empty default classifies every
    /// callee as unknown and therefore grants no legacy safety warrant. See
    /// `crate::return_provenance::CallScrutineeProvenance`.
    ///
    /// Its `fresh_owner_verdicts` field is retained for foreign-provenance,
    /// destructive-update, and alias-safety consumers. Produced-value owner
    /// publication does not consult it; HIR's typed fact is authoritative for
    /// that path. The verdict is the coarse interprocedural freshness fixpoint
    /// (`compute_fn_returns_fresh_owner`) CONJOINED with the opaque-extern
    /// laundering veto and the direct-extern name veto. The builder deliberately
    /// keeps NO second copy: one holder, one authority. The COARSE map is never
    /// threaded here at all — it is a local in `lower_module` consumed only by
    /// the authority builder, so a laundering wrapper cannot be read as fresh at
    /// any consumer, and an un-threaded (default) context grants nothing because
    /// the authority fails closed on every absent row.
    pub(crate) call_scrutinee_provenance: Rc<crate::return_provenance::CallScrutineeProvenance>,
    /// Bindings this function `let`-bound to a value that PROVABLY carries a
    /// declared, non-audited extern's handle, and which were therefore refused a
    /// scope-exit owner (`let_binder_owns_proven_foreign_value`).
    ///
    /// The authority's value queries walk an expression's own structure, and a
    /// `BindingRef` is a leaf to them — so without this ledger a foreign handle
    /// laundered through one `let` would re-enter every container mint clean
    /// (`let h = unsafe { host_record() }; Outer { inner: h }`). It is the
    /// per-function carrier of the same fact, consulted by
    /// `Builder::value_is_free_of_opaque_foreign_provenance` beside the
    /// module-level authority. Reset per function, like every other local ledger.
    pub(crate) proven_foreign_bindings: std::collections::HashSet<hew_hir::BindingId>,
    /// Per-function local-binding freshness facts (#2648 S2b) consumed by the
    /// caller-side argument scan: which of the CURRENT function's locals are
    /// provably solely-owned fresh values (S1 bits `∅`, plain `let`, not
    /// aliased, single read). Computed once per lowered function in
    /// `lower_function`; the empty default (synthetic machine-step builders,
    /// child builders, tests) admits NO local argument — fail-closed.
    pub(crate) call_scrutinee_local_freshness: crate::return_provenance::LocalBindingFreshness,
    /// Module-global RAII-2 param-ownership facts: which affine
    /// `#[resource]` free-fn params are CONSUME (callee owns + drops) vs
    /// BORROW (caller keeps + drops), and the call-arg `SiteId`s whose
    /// over-stamped `Consume` intent is downgraded to a borrowing `Read`.
    /// `Rc` so child builders share it cheaply; empty default leaves every
    /// arg `Consume` and drops no param (sound: pre-RAII-2 behaviour).
    pub(crate) param_ownership: Rc<ParamOwnershipFacts>,
    /// Per-call direct-function argument positions proven borrow-only by the
    /// module parameter-body summary. Keyed by the block containing the
    /// `Terminator::Call`; consumed by collection drop escape analysis so a
    /// caller-owned Vec survives a helper that only reads it.
    pub(crate) proven_borrow_call_args: HashMap<u32, HashSet<usize>>,
    /// Binding ids of the CURRENT function's by-value parameters, captured in
    /// `lower_params`. A funcupdate base that is (or embeds in a construction) a
    /// WHOLE by-value parameter is NOT a unique owner — the parameter is a
    /// borrow (LESSONS `by-value-heap-params-are-borrows`) stored without a
    /// refcount bump (`{ ..Wrap { s: p, .. }, s: new }` frees the caller's `p`
    /// at the override-drop). Consulted by `expr_is_materialized_owner` to reject
    /// such bases. `Rc` so child builders (closures) inherit the enclosing
    /// parameters cheaply; the empty default is sound (admits nothing extra).
    /// Projections of a parameter (`p.inner`) and bare locals are NOT listed —
    /// a field read refcount-bumps and a local move consumes, both empirically
    /// owner-preserving.
    pub(crate) funcupdate_param_ids: Rc<HashSet<BindingId>>,
    /// Exactly one initial typed boundary mode per source parameter. A
    /// module-wide MIR effect pass refines borrowed modes after every function
    /// body and monomorphisation is available.
    pub(crate) param_boundary_modes: Vec<ParamBoundaryMode>,
    /// MIR locals for by-value `bytes` parameters that remain caller-owned
    /// borrows. Returning or storing one mints a co-owner and therefore needs
    /// an explicit `BytesRetain`; ordinary calls continue to borrow it.
    pub(crate) borrowed_bytes_param_locals: HashSet<u32>,
    /// MIR local ids for every function parameter. Used to distinguish a named
    /// parameter slot from a consumed owned-local slot when classifying a plain
    /// `Move` as a retained co-owner share.
    pub(crate) parameter_locals: HashSet<u32>,
    /// MIR locals for by-value `string` parameters that remain caller-owned
    /// borrows. A genuine co-owner mint retains through `hew_string_clone`;
    /// ordinary calls continue to borrow the caller's reference.
    pub(crate) borrowed_string_param_locals: HashSet<u32>,
    /// MIR locals that RECEIVED a variant payload's sole ownership through an
    /// emitted `NeutralizePayloadSlot { transferee: Some(..) }` on a
    /// `MachineVariant`/`EnumVariant` slot (the uniform owned-carrier payload
    /// move-out, D185). The neutralize nulls the carrier's slot, so the
    /// transferee already holds the one live share — a typed-join branch
    /// retain on such a local would strand a `+1` nothing discharges.
    pub(crate) variant_payload_transferee_locals: HashSet<u32>,
    /// Exact join publications whose HIR result fact is a borrowed string.
    /// Return and owning-sink planning may mint a share from this typed row;
    /// ordinary reads keep borrowing the selected source owner.
    pub(crate) typed_borrowed_string_publication_locals: HashSet<u32>,
    /// Bytes counterpart of `typed_borrowed_string_publication_locals`.
    pub(crate) typed_borrowed_bytes_publication_locals: HashSet<u32>,
    /// Binding-reference sites used as the RHS of `let next = current` for
    /// `bytes`. Stage S1 treats these as retained shares, so their checker use
    /// intent is downgraded from `Consume` to `Read`.
    pub(crate) bytes_local_share_sites: HashSet<SiteId>,
    /// Candidate `let next = current` string copies, keyed by the RHS site and
    /// carrying `(source, destination)` bindings. Finalized MIR decides whether
    /// the source is used after the copy (genuine co-owner) or handed off.
    pub(crate) string_local_share_sites: HashMap<SiteId, (BindingId, BindingId)>,
    /// F-04 fungible supervisor-child reference table. Maps the handle local id
    /// produced by `lower_supervisor_child_get` (`Place::ActorHandle(N)`) to the
    /// stable `(supervisor, slot)` reference it stands for.
    ///
    /// A supervised child handle is FUNGIBLE: it names a ROLE (the slot), not a
    /// specific actor instance. The supervisor frees and replaces the underlying
    /// actor on restart, so a snapshotted `*mut HewActor` dangles across a yield.
    /// Rather than snapshot the resolved pointer, the handle carries this
    /// reference and each use re-resolves the current live child. Legacy
    /// single sends/asks use `hew_supervisor_child_get(sup, slot)`; select/join
    /// carriers retain the stable supervisor token plus slot and codegen uses
    /// `hew_local_pid_supervisor_child_get` immediately before submission. Both
    /// resolvers are race-free under `children_lock`. The reference never owns
    /// a child pointer, so the stale-handle-across-yield UAF dissolves by
    /// construction and a use of a not-live child fail-closes as a recoverable
    /// error rather than a program-killing trap.
    ///
    /// Keyed by the handle local id because `let a = sup.w` binds `a` directly to
    /// the same `Place::ActorHandle(N)` (no copy — see the `Let` arm), so both
    /// `sup.w.tick()` and `let a = sup.w; …; a.tick()` resolve their receiver to
    /// the same local and find the same ref here. Function-scoped (fresh per
    /// builder via `Default`), matching `binding_locals`.
    pub(crate) fungible_child_refs: HashMap<u32, FungibleChildRef>,
    pub(crate) decisions: Vec<DecisionFact>,
    /// NEW-6b: maps the id of a basic block that ends in `Terminator::SuspendingAsk`
    /// to the constant deadline (nanoseconds) of an `await … | after d` combinator.
    /// Populated by `lower_actor_ask` when the HIR `ActorAsk` carries a
    /// `deadline_ns`; consumed by codegen to schedule
    /// `hew_await_cancel_schedule_deadline_ms` against the suspend's cancel
    /// registration. Carried as a side-table (not a carrier/Terminator field) so
    /// the eight `Suspending*` carriers stay unchanged (codegen-locals shape).
    pub(crate) await_deadline_ns: HashMap<u32, i64>,
    /// Result-carrier locals minted by actor asks. Pattern lowering uses this
    /// identity to apply the ask carrier's payload-transfer cleanup without
    /// extending that protocol to unrelated enum-producing calls.
    pub(crate) actor_ask_result_locals: HashSet<u32>,
    /// Maps the id of a basic block that ends in one of the ten collapsed
    /// suspension carriers to the carrier's distinguishing payload
    /// ([`SuspendKind`]). Populated at each `finish_current_block(Suspending*)`
    /// site for a PURE-`{resume, cleanup}` carrier; copied into
    /// [`RawMirFunction::suspend_kinds`] at finalize. A side-table (not a
    /// carrier/Terminator field) so the carriers collapse onto one
    /// `Terminator::Suspend` while the emitted IR stays byte-identical.
    pub(crate) suspend_kinds: HashMap<u32, SuspendKind>,
    /// Abandon-edge drops for a suspend's escape-poisoned values that the
    /// generic `drops_for_exit` `BindingState` filter cannot see. These include
    /// a `SuspendKind::StreamSend` in-flight yield and fresh string arguments to
    /// a suspending `SuspendKind::CallClosure`: neither has a competing
    /// scope-exit drop, and each has an inline normal-completion drop. Keyed by
    /// the id of the block carrying the `Terminator::Suspend` (the SAME key
    /// `suspend_kinds` uses). Appended to the matching `ExitPath::Suspend` plan
    /// AFTER `enumerate_exits`, so each value is freed exactly once on the
    /// destroy-while-parked edge — mutually exclusive with its resume-edge drop
    /// (abandon XOR resume).
    pub(crate) suspend_abandon_extra_drops: HashMap<u32, Vec<ElabDrop>>,
    pub(crate) owned_locals: Vec<OwnedLocalEntry>,
    /// Generator/`AsyncGenerator` owned bindings tagged with the HIR scope they
    /// were declared in, recorded so a per-scope-exit `hew_gen_coro_destroy`
    /// fires when that scope closes — INCLUDING when the scope is re-executed
    /// by an enclosing loop. The function-exit LIFO drop only releases the
    /// final content of each binding's slot, so a generator declared inside a
    /// loop body (e.g. the `__hew_for_iter_*` binding of a `for x in gen()`
    /// nested in a `while`) leaks one coro frame + heap companion per outer
    /// iteration without this per-scope-exit release. Entries are removed from
    /// `owned_locals` once
    /// the scope-exit drop is emitted so the function-exit pass cannot
    /// double-free (the drop also null-stores the slot as defence-in-depth).
    pub(crate) scope_generator_bindings: Vec<(ScopeId, hew_hir::BindingId, ResolvedTy)>,
    /// First-class and synthetic `VecIter<T>` bindings tagged with their
    /// declaration scope. Every entry has a parallel runtime bit in
    /// `vec_iter_drop_flags`; the scope/exit edge releases `cursor.vec` only
    /// when that bit says the binding is the current owner. Borrowing
    /// topologies remain in the lexical ledger with a moved bit, while rvalue
    /// snapshots start owned. Keeping both sides of a transfer registered is
    /// essential: only the executed branch updates their bits.
    ///
    /// `VecIter` bindings are deliberately excluded from `owned_locals`; this
    /// guarded inline-field mechanism is their sole drop authority, so no
    /// unconditional `RecordInPlace` exit-plan drop can compete.
    pub(crate) scope_vec_iter_bindings: Vec<(ScopeId, hew_hir::BindingId, ResolvedTy)>,
    /// Persistent declaration ledger for every registered first-class
    /// `VecIter<T>` binding.
    ///
    /// `scope_vec_iter_bindings` is drained when lexical lowering emits the
    /// normal-flow field release. Drop elaboration runs after that drain, so it
    /// needs this append-only twin to recover cursor owners live at
    /// cancellation/panic/yield/suspend abandonment exits. Each entry has a
    /// parallel runtime bit in `vec_iter_drop_flags`; abandonment plans carry
    /// that bit as `ElabDrop::guard`, preserving conditional moves and the
    /// normal-resume/lexical-cleanup path exactly once.
    pub(crate) vec_iter_scope_owner_ledger: Vec<(hew_hir::BindingId, ResolvedTy)>,
    /// Source Vec bindings borrowed by synthetic `for x in source` cursors,
    /// paired with the desugar scope that owns the cursor. A whole-value move
    /// or reassignment of the source while this entry is active would
    /// invalidate `cursor.vec`; those ownership boundaries reject until the
    /// cursor scope closes.
    pub(crate) vec_iter_borrowed_sources: Vec<(ScopeId, hew_hir::BindingId)>,
    /// Full field paths (root binding + field-name chain, root→leaf) of
    /// `for x in root.f…` cursors whose `vec` handle BORROWS a record-field
    /// projection. `vec_iter_borrowed_sources` holds the ROOT for the
    /// whole-value move/reassign guards; this twin holds the PATH for the
    /// record-field-store guard. While an entry is active, a store whose
    /// target path is the projection itself or ANY prefix of it
    /// (`root.f = …`; `root.mid = …` while iterating `root.mid.f`) would
    /// overwrite the slot holding the borrowed handle — the cursor's own
    /// handle copy (cursor field 0) then dereferences released storage — so
    /// those stores reject until the cursor scope closes. Element-level
    /// mutation through the SAME handle (`root.f[i] = …`, `root.f.push(…)`,
    /// `root.f.clear()`) stays allowed: every `next` re-loads the handle,
    /// re-probes `hew_vec_len`, and clones the element out, so in-place
    /// mutation is a memory-safe live view.
    pub(crate) vec_iter_borrowed_projections: Vec<(ScopeId, hew_hir::BindingId, Vec<String>)>,
    /// `RecordInit` destination places of `for x in root.field` cursor inits
    /// whose `vec` handle BORROWS a field projection rooted at a live
    /// local/param binding
    /// ([`Builder::vec_iter_source_live_binding_record_field_root`]). The
    /// root's own drop authority — a by-value parameter's guarded carrier drop
    /// (`append_owned_carrier_param_drops`) or a local record's `RecordInPlace`
    /// composite — releases the field exactly once, so the composite-drop
    /// provers must not classify the cursor-init read of the field binder as an
    /// ownership escape (which would exclude the root and leak every field).
    /// Recorded at cursor registration, where the HIR borrow verdict is known;
    /// the provers cannot re-derive it structurally because an owning
    /// rvalue-rooted projection produces the identical MIR shape.
    pub(crate) vec_iter_projection_borrow_inits: HashSet<Place>,
    /// Runtime ownership bit for every first-class `VecIter<T>` local.
    ///
    /// `0` means this binding currently owns its `vec` snapshot and must release
    /// it; `1` means ownership moved elsewhere (or the cursor was born as a
    /// borrowing topology). Unlike the declaration ledger above, these bits are
    /// mutated by instructions on the actual CFG path, so a transfer in one
    /// branch cannot erase the source obligation on an untaken sibling path.
    /// A consuming `BindingRef` marks the source moved on that path; a let/var or
    /// assignment destination then starts owned. Fresh cursor reassignment
    /// likewise re-arms the destination after releasing its old snapshot.
    pub(crate) vec_iter_drop_flags: HashMap<hew_hir::BindingId, Place>,
    /// Runtime ownership sidecar currently receiving a `VecIter<T>` expression
    /// result.
    ///
    /// The outer ownership boundary allocates the bit; recursively lowered
    /// if/match arms and block tails reuse it, so each executed value-producing
    /// path writes whether its result owns the cursor snapshot. Unrelated reads
    /// never write it because only `lower_value_for_move` result expressions
    /// participate.
    pub(crate) vec_iter_move_result_flags: Vec<Place>,
    /// Whether the parallel result-sidecar frame crosses a proven ownership
    /// sink. `true` is reserved for let/assignment/return or another explicit
    /// owning destination. Composite value-security lowering and unknown
    /// closure/borrow calls use `false`, preserving a binding source while
    /// still describing whether a fresh expression result owns a snapshot.
    pub(crate) vec_iter_move_result_transfers: Vec<bool>,
    /// Direct binding-reference sites currently crossing a `VecIter<T>`
    /// ownership boundary. HIR may retain `Read` intent for a match arm even
    /// though its value moves; this exact-site stack distinguishes that result
    /// transfer from unrelated cursor reads nested in a composite expression.
    pub(crate) vec_iter_direct_move_sites: Vec<hew_hir::SiteId>,
    /// Direct projected user-resource binding sites currently crossing an
    /// ownership boundary. Match-arm tails retain HIR `Read` intent even when
    /// the selected value becomes the owning result of the match. This stack
    /// lets binding lowering record that exact handoff as `Consume`, which
    /// disarms the arm binder and neutralizes its source payload slot.
    pub(crate) projected_resource_direct_move_sites: Vec<hew_hir::SiteId>,
    /// Ownership sidecar produced for each lowered `VecIter<T>` expression.
    /// Let/var and assignment destinations copy this bit after moving the value
    /// bytes, preserving both owning and borrowing topologies through composite
    /// if/match/block results.
    pub(crate) vec_iter_value_drop_flags: HashMap<hew_hir::SiteId, Place>,
    /// `Stream<T>` / `Receiver<T>` for-await cursor bindings tagged with the
    /// block scope they were declared in, so a per-scope-exit close
    /// (`hew_stream_close` / `hew_channel_receiver_close`) fires when that scope
    /// closes. The `Generator`/`VecIter` analogue of #1949 for the general
    /// `for await` consumption path: a `for await v in <stream>` desugars to a
    /// `__hew_for_iter_*` cursor whose close was otherwise deferred to the
    /// ENCLOSING FUNCTION's exit-LIFO plan, so `break`/early `return`/exhaustion
    /// left the stream open — deadlocking any function that abandons a live
    /// stream then does more work before returning (the producer stays parked
    /// on backpressure, its peer never observed as closed). Closing at each
    /// exit edge wakes the parked producer promptly. Mirrors
    /// `scope_generator_bindings`: entries are dispositioned `ScopeReleased`
    /// once the inline close is emitted so the function-exit LIFO cannot fire a
    /// second close, and the inline `Instr::Drop` null-stores the slot
    /// (`raii-null-after-move`; the runtime close symbols also null-guard).
    pub(crate) scope_stream_bindings: Vec<(ScopeId, hew_hir::BindingId, ResolvedTy)>,
    /// Append-only provenance for a user-owned stream/receiver moved directly
    /// into a synthetic `for await` cursor. The lexical cursor close is emitted
    /// inline and may be unreachable when a coroutine is destroyed while
    /// suspended, so elaboration uses this hand-off to select the live owner on
    /// each terminal edge.
    pub(crate) for_await_handle_handoffs: Vec<ForAwaitHandleHandoff>,
    /// Active per-iteration generator-yielded heap value bindings, recorded
    /// while lowering a `for v in gen()` (or `match g.next()`) consuming body so
    /// a `break`/`continue` inside that body frees the current iteration's
    /// yielded value before the back/exit edge — symmetric to
    /// `emit_generator_drops_for_break_continue` freeing the generator HANDLE.
    /// Without this, the break/continue ITERATION's yielded `Vec`/`string`/map
    /// leaks: the body-end drop (`emit_generator_yield_binding_drop`) is emitted
    /// AFTER the body lowers, so a `break`/`continue` jumps past it.
    ///
    /// Each entry is `(active_scopes_len_at_registration, Place, ResolvedTy,
    /// drop_symbol)`. The `active_scopes` length captured when the value is
    /// bound is compared against a break/continue site's `loop_scope_depth`: an
    /// entry registered at depth >= the breaking loop's depth is in-loop and is
    /// freed on that edge. Entries are CLONE-freed on the break/continue edge
    /// (not removed) — the normal fall-through path's body-end drop still frees
    /// the value for that mutually-exclusive CFG path, and the inline drop's
    /// null-after-free makes a structurally-reachable second free a no-op
    /// (`raii-null-after-move`; the runtime also null-guards). Drained when the
    /// consuming body finishes lowering.
    /// Per-iteration generator-yielded heap value bindings active during a
    /// consuming body's lowering. Each entry is `(active_scopes_len, Place,
    /// ResolvedTy, drop_fn, start_block_id, start_instr_len)`:
    /// - `active_scopes_len`: depth marker used by break/continue at the
    ///   matching loop scope to know which entries lie inside the breaking
    ///   loop;
    /// - `Place`/`ResolvedTy`/`drop_fn`: what to drop and how (a cow-heap
    ///   `Release` symbol, or the composite `InPlace` thunk route);
    /// - `start_block_id`/`start_instr_len`: the binding's site, used by
    ///   `generator_yield_binding_drop_safe` to scan the body for an
    ///   ownership-transferring escape (a `Move` of the binding's slot into
    ///   another local, a store into a surviving aggregate). When the value
    ///   escapes between its bind site and a break/continue point, the
    ///   break/continue-edge drop MUST be skipped — the consumer (the
    ///   move-destination) owns the release, so an unconditional break-edge
    ///   drop would double-free the buffer at the move-out site. Without the
    ///   scan, a `for await item in rx { carry = item; break; }` pattern
    ///   reliably produces a use-after-free at `println(carry)` after the
    ///   loop. (`scope_generator_bindings` for the generator HANDLE has no
    ///   such case because handles do not get re-bound to another local
    ///   mid-body — only their content is consumed via `next`.)
    pub(crate) active_generator_yield_values: Vec<(
        usize,
        Place,
        ResolvedTy,
        crate::model::DropFnSpec,
        u32,
        usize,
    )>,
    /// Fresh `Some(x)` payload owners produced by the synthetic `VecIter`
    /// `next()` match. Their normal body/explicit-edge release is emitted
    /// inline; this bounded ledger restores authority only for abandoning
    /// exits reached while the consuming body still owns the payload.
    pub(crate) vec_iter_yield_exit_drops: Vec<VecIterYieldExitDrop>,
    /// MIR locals of every fresh per-frame yield/recv payload binder
    /// (`for x in vec`, generator drive, receiver read — the `Some(x)` arms
    /// that schedule a body-end release and retract the binding to
    /// `Disposition::BodyEndReleased`). This is the binder class whose value
    /// IS frame-owned but whose release authority is the per-iteration
    /// body-end drop, not the function-scope LIFO. The projection-alias taint
    /// seed exempts these dests: their `Move` out of the synthetic `Option`'s
    /// variant slot takes a fresh clone/frame the shell no longer releases,
    /// so a downstream retained share is a genuine co-owner, not an interior
    /// alias of a still-live aggregate.
    pub(crate) yield_binder_locals: HashSet<u32>,
    /// Escape-scan exemptions for RETAIN-BACKED shares of an active yield
    /// binder, recorded at lowering time as `(block, instr index)` of the
    /// share `Move`. The body-shape drop-safety scan treats such a Move as a
    /// borrow (the `+1` retain mints the destination's count), so the
    /// binder's per-iteration body-end drop is still emitted — suppressing it
    /// would leak the binder's own count on every path.
    pub(crate) yield_share_instr_exempt: HashSet<(u32, usize)>,
    /// Terminator analogue of `yield_share_instr_exempt`: `(block, local)`
    /// pairs whose consuming collection-ingress `Call` operand was
    /// pre-retained, making the call a borrow of the binder's count.
    pub(crate) yield_share_term_exempt: HashSet<(u32, u32)>,
    /// Header-defined while-let scrutinee owners active while their body is
    /// lowered. Break/continue edges consume these owners and record an
    /// explicit edge drop; returns/panic/cancellation leave them Live so the
    /// ordinary exit planner releases them.
    pub(crate) active_iteration_owners: Vec<ActiveIterationOwner>,
    /// Owning enum bindings whose current generation was byte-copied into the
    /// dedicated `while let` iteration snapshot before the loop body began.
    /// Reassigning one of these bindings must not run the ordinary enum
    /// overwrite release: the snapshot's back-edge/exit drop is the sole
    /// authority for that old generation.
    pub(crate) active_while_let_snapshot_parents: Vec<BindingId>,
    /// Map from each MIR-bound HIR `BindingId` to the HIR `ScopeId` it was
    /// declared in. Populated at every `MirStatement::Bind` push site (let
    /// statements, match-arm payload bindings, function parameters, for-range
    /// counter binds). Consulted by the elaborator (`enumerate_exits`) to
    /// scope-filter back-edge `Goto` drops: a binding declared in a loop body's
    /// scope must be released before the back-edge re-enters the body and
    /// overwrites the slot with the next iteration's value.
    ///
    /// This is the missing piece that distinguishes per-iteration drops from
    /// function-exit drops. Without scope tracking, the elaborator's existing
    /// `drops_for_exit` (currently called only on `Return`/`Cancel`/`Panic`)
    /// would either fire ALL live bindings on a back-edge (double-freeing
    /// outer-scope values like the receiver itself) or fire NONE (the current
    /// behaviour, which leaks the per-iteration heap-owning let-bindings).
    /// Restricting the back-edge plan to bindings whose `binding_scope` matches
    /// the loop body's scope (recorded in `loop_back_edge_blocks`) makes the
    /// drop set per-iteration and CFG-correct: outer-scope bindings keep their
    /// function-exit drop, inner-scope bindings get one drop per iteration.
    pub(crate) binding_scope: HashMap<BindingId, ScopeId>,
    /// Scope facts for transient payload-binding locals whose `binding_locals`
    /// entry is restored after a match-like body lowers. The enum sole-owner
    /// prover still needs their real body lifetime to distinguish an in-body
    /// handoff from an escape into a surviving outer binding.
    pub(crate) transient_local_scopes: HashMap<u32, ScopeId>,
    /// gdb `-g` lexical-block scoping. Accumulates, per HIR `ScopeId` ever active
    /// while lowering this function's body, the scope's parent `ScopeId` (the
    /// scope directly below it on `active_scopes` when first observed) and the
    /// source byte-extent `[min_start, max_end)` of every statement/instruction
    /// span lowered under it. Updated incrementally in `push_instr` and
    /// `record_binding_scope` (both fire while the scope's frame is on the stack
    /// top) — so no instrumentation of the 11 `active_scopes.push` sites is
    /// needed. Drained at finalize into `RawMirFunction.scope_table` for codegen
    /// to build one `DILexicalBlock` per scope, parented per `parent`, with PC
    /// ranges derived from byte-extent → line. A shadowed inner `let first` is
    /// recorded under its own (inner) scope, so its variable DIE and the inner
    /// block's instruction `DILocation`s land in a distinct lexical block — the
    /// outer `first` no longer leaks into the inner breakpoint's frame.
    pub(crate) scope_info: HashMap<ScopeId, ScopeInfoEntry>,
    /// gdb `-g`: source start-byte of each `let`-binding's declaration, captured
    /// at `record_binding_scope` from the live lowering cursor. Resolved to the
    /// binding's slot in `resolve_local_names_from_binds` so each local DIE gets
    /// its OWN declaration line — two same-named shadowed locals on different
    /// lines stay distinct even before lexical-block scoping. Absent → codegen
    /// falls back to the function-declaration line.
    pub(crate) binding_decl_byte: HashMap<BindingId, u32>,
    /// Map from each loop-body back-edge `Goto`'s emitting block id to the HIR
    /// `ScopeId` of the loop body that closes there. Populated immediately
    /// before each loop's body→header (or body→inc) `Terminator::Goto` is
    /// emitted, in `lower_while`/`lower_while_let`/`lower_for_range`/
    /// `lower_loop`. Consulted by `enumerate_exits` to populate the back-edge's
    /// `DropPlan` with drops for bindings whose `binding_scope` matches this
    /// scope — releasing exactly the per-iteration heap-owning let-bindings
    /// (`Option<T>` recv results, owned strings/Vecs/maps bound inside the body)
    /// before the next iteration overwrites their slots.
    ///
    /// The dataflow's `BindingState` filter in `drops_for_exit` automatically
    /// excludes bindings that are `Consumed` (moved out) or `Uninit` (not yet
    /// assigned on the first iteration's first reach), so escape via `break x`
    /// / pass-by-value / first-iteration-uninit double-free corner cases are
    /// handled by the existing dataflow without bespoke logic.
    pub(crate) loop_back_edge_blocks: HashMap<u32, ScopeId>,
    /// Goto-edge blocks that must release header-defined iteration owners.
    /// Each binding is also marked consumed in the source block's statement
    /// stream, so the target's later function exit cannot release it again.
    pub(crate) iteration_owner_drop_blocks: HashMap<u32, Vec<BindingId>>,
    /// Header snapshot owners whose drop template is valid only on a recorded
    /// loop back-edge. Ordinary exit plans exclude these bindings because the
    /// source binding still owns the same value on tag-false and pre-reassign
    /// break/return edges.
    pub(crate) back_edge_only_iteration_owners: HashSet<BindingId>,
    /// Fresh string publications that remain live across a borrowing operation
    /// spanning multiple blocks. They need the ordinary exit-plan template,
    /// unlike synthetic publications already discharged inline or transferred
    /// into another owner.
    pub(crate) synthetic_borrowed_temp_drop_bindings: HashSet<BindingId>,
    /// Diagnostics collected during MIR building (e.g., Unsupported HIR nodes).
    pub(crate) diagnostics: Vec<MirDiagnostic>,
    /// Per-function de-duplication for W3.029 user-aggregate value-class
    /// diagnostics. Keyed by the same bare/mangled record-layout key used by
    /// `record_field_orders`.
    pub(crate) unsupported_user_record_value_classes: HashSet<String>,
    /// owned-string-record allow-list of expression sites that may classify a user record as
    /// `CowValue` without opening the general user-record value-class surface.
    /// Populated only for the RHS `StructInit` of a let-bound, monomorphic,
    /// direct-string record and for direct field-access object `BindingRef`
    /// sites against such bindings.
    pub(crate) owned_string_record_value_sites: HashSet<SiteId>,
    /// Let bindings whose value was introduced by the owned-string record-construction
    /// gate. Field access and drop elaboration consult this binding-scoped proof
    /// instead of re-opening arbitrary user-record reads.
    pub(crate) owned_string_record_bindings: HashSet<BindingId>,
    /// W5.016 owned-Vec-element allow-list of record/enum layout KEYS that are
    /// used as the element of an owned-Vec anywhere in the current function. A
    /// value of such a type classifies as `CowValue` (the runtime deep-clones
    /// it in and drops it via the seeded per-type thunks) instead of tripping
    /// the W3.029 wholesale `Unknown` reject. Pre-computed by `function_body`
    /// before any `decide` runs (so it is robust to the `lower_value`-decides-
    /// before-arm ordering). This is a bounded allow-list keyed on the exact
    /// types codegen seeds thunks for — it does NOT open the general
    /// user-record value-class surface (a record with a `Vec`/`HashMap` field
    /// is never an owned-Vec element, so it never enters this set).
    pub(crate) vec_owned_element_keys: HashSet<String>,
    /// Per-named-type marker registry, cloned from the parent `HirModule` at
    /// builder construction. Read by every `ValueClass::of_ty` call site in
    /// MIR lowering so the marker is the single fact about whether a Named
    /// type participates in the ownership-discipline surface.
    pub(crate) type_classes: hew_hir::TypeClassTable,
    /// Lambda-actor capture ledger collected across every
    /// `HirExprKind::SpawnLambdaActor` literal in the function body.
    /// Drained into `ElaboratedMirFunction.lambda_captures` at the
    /// elaboration boundary; the structural fail-closed checker
    /// `validate_lambda_captures` runs against the drained list.
    pub(crate) lambda_captures: Vec<LambdaCapture>,
    /// `Some(LambdaActorHandle)` while the producer is lowering the
    /// value of `let <name> = actor |..| { .. }`. The `HirStmtKind::Let`
    /// arm pre-allocates the actor's local and records the
    /// `LambdaActorHandle(N)` here BEFORE lowering the value, so
    /// `lower_spawn_lambda_actor` reuses the slot the binding already
    /// owns instead of allocating a second local. The HIR forward-bind
    /// already routed the binding's resolved name to the lambda's own
    /// `BindingId`; this mirror at MIR keeps the binding's `Place`
    /// alignment to that same handle so a Weak self-capture's slot
    /// resolves correctly.
    pub(crate) pending_lambda_actor_handle: Option<Place>,
    /// Tuple decomposition map for runtime-call results that produce multiple
    /// output Places (e.g. `hew_duplex_pair` → two `DuplexHandle` slots).
    ///
    /// Key: the `u32` local index of the "tuple proxy" `Place::Local(N)` that
    /// `lower_runtime_call` returns for a multi-output call.  Value: the
    /// ordered slice of output Places in source-declaration order (e.g.
    /// `[DuplexHandle(N0), DuplexHandle(N1)]` for `duplex_pair`).
    ///
    /// `TupleIndex` lowering looks this map up when the tuple sub-expression
    /// resolves to a proxy local: index `i` into the value vec to obtain the
    /// concrete output Place without emitting additional instructions.
    ///
    /// SHIM(E2→E3): only `hew_duplex_pair` populates this map today.
    /// WHY: MIR has no multi-return instruction; a proxy local threads the
    ///   output Places through the existing single-`Place` `BindingRef` lookup.
    /// WHEN obsolete: when a dedicated MIR multi-return or projection surface
    ///   lands and `TupleIndex` lowering is rewritten to use it directly.
    /// WHAT: replace with `Place::Projection { base, index }` variant or a
    ///   `Terminator::Call`-style multi-dest encoding.
    pub(crate) tuple_decomp: HashMap<u32, Vec<Place>>,
    /// Declaration-order field descriptors for every `record` type in the module.
    ///
    /// Key: record type name (e.g. `"Point"`).
    /// Value: `(field_name, field_ty)` pairs in declaration order.
    ///
    /// Used by `StructInit` and `FieldAccess` lowering to resolve a field
    /// name to its 0-based `FieldOffset` and to look up the field type when
    /// allocating intermediate places for functional-update base reads.
    /// Built from `HirItem::Record` items in `lower_hir_module` and threaded
    /// through to the builder.
    ///
    /// Tuple records use synthetic ordinal field names paired with the
    /// authoritative `HirRecordDecl.positional_field_tys`. Their constructor
    /// remains a `Call`, not a `StructInit`, and the checker still exposes no
    /// named/indexed field access; the entries support structural ownership
    /// classification only.
    pub(crate) record_field_orders: HashMap<String, Vec<(String, ResolvedTy)>>,
    pub(crate) actor_layouts: HashMap<String, ActorLayout>,
    /// Supervisor-layout map, mirroring `actor_layouts` for supervisor types.
    /// Used by `lower_spawn_actor` to route `spawn Sup` to the supervisor
    /// bootstrap call and by `push_unknown_type_diagnostics` to recognise
    /// supervisor names inside `LocalPid<Sup>` type args as known. Child
    /// builders (closure shims, lambda-actor bodies, task-entry adapters)
    /// inherit it via `child_builder_tables`.
    pub(crate) supervisor_layout_map: HashMap<String, crate::model::SupervisorLayout>,
    /// Set of recognised tagged-union canonical classification keys — every
    /// machine type plus the synthesised `<Machine>Event` companion enum for
    /// each, and every ordinary enum, all projected through
    /// `machine_layout_key`. Used by
    /// `push_unknown_type_diagnostics` to silence `UnknownType` on these
    /// types and by `is_known_actor_runtime_ty` to classify their values
    /// as `BitCopy` so the decision-map check accepts the site. Populated
    /// from `module.machine_layouts` in `lower_hir_module` and threaded
    /// through every Builder construction site.
    pub(crate) machine_layout_names: HashSet<String>,
    /// Monomorphised tagged-union enum layouts, cloned from the pipeline-wide
    /// `enum_layouts` at builder construction. The drop elaborator consults
    /// these through the single record-aware heap-ownership authority
    /// `ty_owns_heap` (via `ty_owns_heap_mir`) to decide whether an
    /// enum-composite binding owns a heap payload and so earns a tag-aware
    /// `DropKind::EnumInPlace` scope-exit drop (W5.020).
    /// Empty for builders constructed before layout collection (synthetic
    /// test pipelines) — such bodies simply never elaborate the enum-in-place
    /// drop, matching the pre-W5.020 leak-not-double-free posture.
    pub(crate) enum_layouts: Vec<crate::model::EnumLayout>,
    /// Names of every `#[opaque]` type declared in the module. Threaded into
    /// `classify_state_field_full` so opaque handles (e.g. `json.Value`,
    /// `cron.Expr`) appearing in owned-aggregate records classify as
    /// `StateFieldCloneKind::OpaqueHandle` rather than `MissingRecordLayout`.
    /// Populated from `module.items` in `lower_hir_module` and passed through
    /// every Builder construction site that may reach
    /// `owned_aggregate_record_field_kinds_for_key`.
    pub(crate) opaque_handle_names: Vec<String>,
    /// RAII-1 opaque-resource close registry — `(opaque_type, "<Type>::<close>")`
    /// for every single-slot `#[resource] #[opaque]` handle (see
    /// `resource_opaque_close_registry`). Threaded into
    /// `classify_actor_state_fields_with_lifecycle_registry` at the owned-aggregate
    /// admission gate so a resource-bearing record field classifies as
    /// `StateFieldCloneKind::Resource` (RAII drop spine) instead of the
    /// no-op-drop `OpaqueHandle` (the W3.029 leak). Built from
    /// `opaque_handle_names` + `type_classes` in the Builder ctor; `IrPipeline`
    /// rebuilds the identical registry for codegen so the two never drift.
    pub(crate) lifecycle_registry: hew_hir::LifecycleRegistry,
    pub(crate) current_actor_state_fields: HashMap<String, (FieldOffset, ResolvedTy)>,
    /// Names of every user-defined function declared in the module. Used by
    /// `lower_value` `HirExprKind::Call` to distinguish user-fn callees
    /// (→ `Terminator::Call`) from runtime-ABI callees (→
    /// `Instr::CallRuntimeAbi`) and from indirect/closure callees
    /// (→ `NotYetImplemented`). Name-string matching is the reliable
    /// discriminator here because the HIR bridge does not yet emit
    /// `ResolvedRef::Item` for function-item callees (see the SHIM comment
    /// at the Call lowering arm). The set is populated once per module by
    /// `lower_hir_module` before any function body is lowered, so forward
    /// references (calling a function declared later in the file) are
    /// handled correctly.
    pub(crate) module_fn_names: HashSet<String>,
    pub(crate) module_generic_fn_names: HashSet<String>,
    /// Structured static-dispatch registry — `(declaring_trait,
    /// self_type_name, method_name) → impl method symbol + impl type
    /// params`. Built once from `HirItem::Impl` metadata in
    /// `lower_program_with_*` and cloned into every builder so the
    /// `CallTraitMethodStatic` arm can resolve through structured HIR
    /// facts rather than reconstructing the impl symbol from a display
    /// name. Empty for builders constructed via `Builder::default()`
    /// (machine step shells, actor handler shims) — those bodies do
    /// not host generic-bounded trait method calls.
    #[allow(
        dead_code,
        reason = "prepared for structured dispatch; MIR uses type-name derivation in this slice"
    )]
    trait_impl_index:
        HashMap<hew_hir::dispatch::TraitImplKey, hew_hir::dispatch::TraitImplMethodEntry>,
    /// Exact HIR-boundary projection from a checker-owned direct declaration
    /// ID to an emitted endpoint symbol.  It covers impl methods and source
    /// externs; this is the only route by which those `CallTarget::User` /
    /// `CallTarget::ImplMethod` edges obtain linker labels.
    pub(crate) direct_call_symbols: HashMap<hew_types::DefId, String>,
    /// Substitution map from origin-fn type-parameter symbols to
    /// concrete `ResolvedTy`s, populated only when this Builder is
    /// lowering a generic function under a specific monomorphisation.
    /// Empty for non-generic fn bodies.
    ///
    /// Every type observed during body lowering is substituted via
    /// `subst_ty` before reaching the backend Instr stream so symbolic
    /// type-parameter `Named` types never escape into MIR/codegen.
    pub(crate) subst: HashMap<String, ResolvedTy>,
    /// Per-call-site `Vec<ResolvedTy>` recorded by HIR lowering for
    /// generic top-level user-fn callees. Cloned from
    /// `HirModule.call_site_type_args`. The Call lowering arm
    /// substitutes these via `subst_ty` and dispatches to the
    /// per-monomorphisation mangled symbol.
    pub(crate) call_site_type_args: HashMap<hew_hir::SiteId, Vec<ResolvedTy>>,
    /// Per-concrete-element `Vec<T>` runtime-ABI verdict table, cloned from
    /// `HirModule.vec_generic_element_abi`. The `ResolvedImplCall` arm consults
    /// it to re-resolve an element-typed `Vec<T>` method (`push`/`get`/`set`/
    /// `pop`) whose checker dispatch left a `hew_vec_*_FAMILY` placeholder
    /// because the element was a type parameter: the substituted concrete
    /// element is looked up here per monomorphisation and `(method, token)` is
    /// mapped to the concrete runtime symbol. Empty for lowering contexts that
    /// never observe a polymorphic element (the placeholder then fails closed).
    pub(crate) vec_generic_element_abi: HashMap<hew_types::Ty, hew_types::VecElementToken>,
    /// Per-`FieldAccess` site-id → `ChildSlot` side-table, populated by HIR
    /// lowering from the checker's `supervisor_child_slots`. The `FieldAccess`
    /// arm checks this map BEFORE the `record_field_orders` lookup so that
    /// supervisor-typed LHS is intercepted and routed to
    /// `hew_supervisor_child_get` rather than a record-field load.
    ///
    /// Cloned from `HirModule.supervisor_child_slots`. Empty for functions
    /// (actor-handler shims, closure shims) whose bodies cannot contain
    /// supervisor field accesses — the empty map causes the intercept arm to
    /// skip immediately, adding zero overhead for the common case.
    pub(crate) supervisor_child_slots: HashMap<hew_hir::SiteId, hew_types::ChildSlot>,
    /// Resolved static-pool accessor sites (`sup.pool[i]` / `.get(i)` /
    /// `.len()`), cloned from `HirModule.pool_accessor_sites` and keyed by the
    /// `SiteId` of the `Index`/`MethodCall` expression. The lowering arms consult
    /// this to emit the pool ABI call instead of the generic container/method
    /// path. Empty for shim functions.
    pub(crate) pool_accessor_sites: HashMap<hew_hir::SiteId, hew_types::PoolAccessor>,
    pub(crate) current_task_scope: Option<Place>,
    pub(crate) current_function_symbol: String,
    pub(crate) current_function_call_conv: crate::model::FunctionCallConv,
    /// True only on a source generator shell builder. Its fn-typed formal
    /// parameters are admitted into the shell's generator env because every
    /// standalone `gen fn` call crosses `lower_direct_call`'s provenance gate
    /// and every `receive gen fn` call crosses `lower_actor_gen_stream`'s
    /// equivalent gate. Anonymous `gen {}` blocks in ordinary functions have
    /// no such call boundary and reject unproven fn-valued captures at env
    /// materialisation.
    pub(crate) generator_shell_call_gate: Option<GeneratorShellCallGate>,
    pub(crate) task_entry_adapter_symbols: TaskEntryAdapterSymbols,
    pub(crate) next_closure_id: u32,
    pub(crate) generated_functions: Vec<LoweredFunction>,
    pub(crate) closure_record_layouts: Vec<crate::model::RecordLayout>,
    pub(crate) capture_env_sources: HashMap<BindingId, CaptureEnvSource>,
    /// Whole-value loads from owned capture-env fields. Every `Instr::Move`
    /// source is checked against this table at emission and again before a
    /// block is sealed, so no result-position lowering path can move the
    /// environment's still-owned alias into a second owning place.
    capture_env_owned_loads: HashMap<Place, CaptureEnvOwnedLoad>,
    /// Suppresses duplicate diagnostics if malformed lowering tries to move
    /// the same captured load more than once; every such move is still dropped.
    rejected_capture_env_owned_loads: std::collections::HashSet<Place>,
    /// Body-side set of capture bindings whose env field holds a WEAK
    /// lambda-actor handle (`CaptureKind::Weak` — the forward-bound self
    /// reference, §5.9 ratification 2). `lower_lambda_actor_call` consults
    /// this to dispatch the self-send through `hew_lambda_actor_weak_send`
    /// (the weak handle's ABI) instead of `hew_lambda_actor_send`.
    pub(crate) weak_lambda_capture_bindings: std::collections::HashSet<BindingId>,
    /// Base locals (the `u32` slot index from `Place::Local`) of pattern
    /// bindings introduced by a non-BitCopy `match` record/tuple destructure
    /// (`lower_match_project`) **whose scrutinee was consume-marked at the
    /// destructure site**. Consulted by `derive_cow_sole_owner` to SUPPRESS
    /// the projection-alias taint seed for these binders.
    ///
    /// Why this exists. The pattern binder is loaded via `RecordFieldLoad` /
    /// `TupleFieldLoad`, both of which `projection_alias_dest` seeds as
    /// tainted (interior alias of the parent aggregate). Tainted
    /// leaf-`string`/`Vec`/`bytes` locals are excluded from
    /// `cow_drop_allowed`, so `build_lifo_drops` silently skips their drop
    /// (the leaf-CoW arm tolerates a missing place rather than panicking).
    /// The taint is correct when the parent aggregate's composite drop still
    /// fires (otherwise the same buffer frees twice), but a consume-marked
    /// scrutinee emits a follow-up `Use { intent: Consume }` for the
    /// `BindingRef`, so the parent's drop is suppressed by the dataflow
    /// exit-state filter. With the parent consumed, the binder is the SOLE
    /// owner of its loaded payload; the taint would otherwise become a leak.
    ///
    /// Membership is restricted to the binders whose source we actually
    /// consume — the `match_project_scrutinee_reject` gate guarantees this is
    /// always a non-captured `BindingRef`. Other destructure shapes
    /// (let-pattern, enum-tag, while-let) keep their existing taint
    /// behaviour. The dataflow `Consumed`/`MaybeConsumed` exit-state
    /// post-filter at the `derive_cow_sole_owner` call site is still the
    /// final authority: a binder consumed by `=> y` is removed from the
    /// allow-set, so the drop won't double-free a moved-out payload.
    pub(crate) match_project_consumed_binder_locals: HashSet<u32>,
    /// Match-arm payload binders whose active `(variant, field)` was proven a
    /// fresh transferred return. This includes mixed-return wrappers with no
    /// whole shell owner and recursive record payloads moved directly out as
    /// the result of a proved-fresh call-scrutinee match. In the latter case
    /// the shell's active payload drop is suppressed on that arm, so the binder
    /// is the one and only owner of the selected field. `CoW` payloads retain
    /// their separate retain-aware shell/binder authority. These locals are
    /// exempt from the ordinary interior-projection alias seed.
    pub(crate) fresh_variant_payload_binder_locals: HashSet<u32>,
    /// Binding ids paired with `fresh_variant_payload_binder_locals`. Record
    /// drop admission is keyed by binding rather than local, so this is the
    /// matching active-variant proof for recursive record payload teardown.
    pub(crate) fresh_variant_payload_bindings: HashSet<BindingId>,
    /// Bindings that hold a closure value whose resolved invoke-shim carries a
    /// suspend terminator (the suspendable-callee discriminator). Populated by
    /// the `Let` handler from the shim's lowered MIR carriers — the SAME
    /// structural fact codegen's `is_coroutine` reads — so a call to such a
    /// binding lowers to `Terminator::SuspendingCallClosure` (the driver) rather
    /// than the direct `Instr::CallClosure`. A non-suspending closure is never
    /// inserted, keeping it on the direct path (no spurious coroutine driving).
    pub(crate) suspending_closure_bindings: HashSet<BindingId>,
    /// `let` bindings whose initializer failed to lower (returned `None`) AND
    /// emitted at least one diagnostic of its own. A later `BindingRef` to such
    /// a binding has no `binding_locals` Place, which would otherwise raise a
    /// follow-on `UnresolvedPlace` ("could not resolve binding `u`") — pure
    /// cascade noise stacked on the root error the user must actually fix (e.g.
    /// a fail-closed functional-update base/carry reject on `let u = { ..base }`).
    /// The `BindingRef` resolver consults this set and returns `None` silently
    /// for a poisoned binding, so only the root diagnostic surfaces. Guarded on
    /// "a diagnostic was emitted" so a genuinely-unresolved binding (a real bug
    /// with no prior error) still reports.
    pub(crate) poisoned_let_bindings: HashSet<BindingId>,
    /// Generator/`gen fn` capture bindings the enclosing `lower_gen_block`
    /// rejected with a root `NotYetImplemented` (an inadmissible opaque/owned
    /// value that cannot be admitted into the generator's flat-copied env
    /// record). The synthetic body still names the capture as a free variable,
    /// but it was never
    /// materialised into the env record — so its body-side `BindingRef` would
    /// otherwise stack two cascade secondaries on the root: a
    /// `MirStatement::Use` of an un-`Bind`-ed binding (→ dataflow
    /// `InitialisedBeforeUse`) and an `UnresolvedPlace` (no backend slot). The
    /// `BindingRef` resolver consults this set and returns `None` silently for a
    /// poisoned capture, so only the actionable root rejection surfaces. Scoped
    /// to the body sub-builder: it carries only the specific failing capture
    /// ids, never the enclosing frame's bindings, and the body's own `let`
    /// bindings have distinct ids so they are unaffected.
    pub(crate) poisoned_capture_ids: HashSet<BindingId>,
    /// Transient: set by `lower_closure_literal` to the just-lowered closure's
    /// body-suspends verdict (derived from its shim's MIR carriers) so the `Let`
    /// handler can attribute it to the bound binding. Reset around each closure
    /// lowering; only read immediately after lowering a `HirExprKind::Closure`.
    pub(crate) pending_closure_literal_suspends: Option<bool>,
    /// Transient: set by `lower_closure_literal` to `true` when the
    /// just-lowered closure literal's escape class selected the `Heap`
    /// allocation strategy (its pair owns — or, capture-free, may own
    /// nothing behind — a heap env box). Read by the `Let` handler to admit
    /// the bound pair into `closure_pair_owned`. Same reset discipline as
    /// `pending_closure_literal_suspends`.
    pub(crate) pending_closure_literal_heap: Option<bool>,
    /// Bindings that own an escaping-closure pair's env-box free obligation
    /// (the sole-owner affine model). Admitted at the introducing
    /// `HirStmtKind::Let` for three RHS shapes only: a closure literal whose
    /// strategy was `Heap`, a fn-typed call result (the producer's pair is
    /// always heap-or-null by construction), and a rebind of an
    /// already-admitted binding (ownership transfers; the source is marked
    /// moved). Every other producing shape leaks rather than risks freeing
    /// a stack env (`boundary-fail-closed`: over-exclude, never re-admit).
    /// `elaborate` narrows this set further (returned pairs, captured
    /// pairs, consumed exits) into `closure_pair_drop_allowed`.
    pub(crate) closure_pair_owned: HashSet<BindingId>,
    /// Bindings holding a named-function pair (`let f = double;`) — the pair's
    /// `env_ptr` is null by construction (the named-fn shim never reads it),
    /// so the pair is freely byte-copyable: no env exists to double-free.
    /// These bindings are exempt from the closure-pair ingress discipline
    /// (`enforce_closure_pair_ingress`). Populated at the introducing `Let`
    /// for an `Item`-resolved fn reference RHS and propagated through
    /// rebinds of an already-exempt binding.
    pub(crate) closure_pair_null_env: HashSet<BindingId>,
    /// Fn-typed (`ty_is_closure_pair`) bindings whose pair may carry a NON-NULL
    /// heap closure-env word even when its static type is a plain `fn(..)`.
    /// Sources are:
    ///   * fn-typed parameters (the caller may pass a capturing closure);
    ///   * fn-typed call results (the callee may return one);
    ///   * capturing-closure literals structurally unified with `fn(..)`;
    ///   * copies/merges/reassignments of any binding already in this set.
    ///
    /// Let/reassignment provenance is collected flow-INSENSITIVELY by the
    /// `collect_vec_owned_element_keys_from_block` pre-pass so a loop back-edge
    /// assignment taints the binding before any call site lowers. Parameters
    /// are seeded by `lower_params` before that pre-pass runs.
    ///
    /// Consulted by generator call-argument gates, anonymous-generator env
    /// materialisation, and child-builder provenance inheritance. Deliberately
    /// NOT a drop ledger and NOT consulted by closure-pair ingress/drop
    /// machinery: taint answers "may this `fn(..)` value hide a heap env?",
    /// never "who frees the env box".
    pub(crate) closure_pair_env_may_be_nonnull: HashSet<BindingId>,
    /// Closure-pair bindings whose ownership has already left them via a
    /// rebind (`ClosurePairRhs::TransferFrom`). The dataflow checker flags
    /// any later use of these on its own: the rebind's RHS read carries
    /// HIR's `IntentKind::Consume`, so the source binding is `Consumed` in
    /// the lattice and every subsequent read is `UseAfterConsume`. The
    /// ingress gate consults this set only to avoid stacking a second
    /// `ClosurePairBorrowedStore` diagnostic on a use the move checker
    /// already rejects.
    pub(crate) closure_pair_moved: HashSet<BindingId>,
    /// Fn-typed (`ty_is_closure_pair`) PARAMETER bindings whose closure env is
    /// provably heap-allocated, and which may therefore transfer env ownership
    /// into an owning container (a record field, Vec element, tuple, or
    /// machine payload) when stored — the same sole-owner transfer the closure-
    /// *literal* and fn-call-*result* ingress paths already perform.
    ///
    /// SOUNDNESS PREMISE (the one fact this whole set rests on): a closure that
    /// reaches a parameter has crossed a call boundary as an argument, so the
    /// checker classified it `AnonContext::PassedToHigherOrder` →
    /// `ClosureEscapeKind::Escapes` → `AllocationStrategy::Heap`
    /// (`hew-types/src/check/mod.rs`; `hew-mir/src/closure_env.rs`). Its env was
    /// heap-boxed by the CALLER before the call, never a stack/frame address.
    /// So the conservative "a param pair may carry a stack env, freeing one
    /// would over-free a frame address" assumption that keeps a bare parameter
    /// in the `Borrowed` (refuse) class does NOT hold for a fn-typed param — it
    /// is ownable exactly like a fn-typed call result. If a future change ever
    /// admits a `Local`/stack-env closure into a parameter position, this
    /// premise breaks; the field-store would over-free. The leak oracle
    /// (`hew-cli/tests/closure_in_struct_leak_oracle.rs`) pins it with a
    /// poisoned-allocator floor-equality / no-double-free check.
    ///
    /// This set drives ONLY the ingress classification in
    /// `classify_closure_pair_ingress` (`Borrowed` → `OwnedBinding`). It is
    /// deliberately DISJOINT from `closure_pair_owned`: it is NOT a drop
    /// ledger. Merging it into `closure_pair_owned` would feed
    /// `derive_closure_pair_drop_allowed` a param that is read only by
    /// `CallClosure` (the benign callee read), which the aliasing scan does not
    /// mark, so an UNSTORED-but-invoked param would gain an erroneous scope-exit
    /// drop — a double-free of an env the param never owned. Two sets, two
    /// purposes (ingress classification vs `Let`-binding drop admission).
    pub(crate) closure_pair_param_owned: HashSet<BindingId>,
    /// Reserved context for the later transition-body lowering slice. When
    /// set, the `BindingId` identifies the step function's `self` parameter
    /// slot so `HirExprKind::MachineFieldAccess` can address payload reads
    /// via `Place::MachineVariant`.
    ///
    /// `None` outside a machine step body. This is the MIR analogue of the
    /// HIR-side `current_machine_self_binding` / `current_machine_source_state`
    /// context — HIR already resolves the field's `state_idx` and `field_idx`
    /// at lowering time, so MIR only needs the addressing binding.
    ///
    /// Slice 4a's step shell leaves this unset because transition bodies are
    /// not lowered. Slice 4b sets it while walking transition bodies; Slice
    /// 4c reads the emitted `Place::MachineVariant` places.
    pub(crate) current_machine_self_binding: Option<BindingId>,
    pub(crate) current_machine_event_binding: Option<BindingId>,
    /// Stable machine-type id (`machine_emit_type_id`) for the machine step
    /// function currently being lowered. Set alongside
    /// `current_machine_self_binding` / `current_machine_event_binding` by
    /// `emit_machine_step_transition_return` (transition bodies) and
    /// `lower_machine_lifecycle_block` (HIR admits `emit` inside `entry {}`
    /// / `exit {}` too — both call sites must set this so
    /// `HirExprKind::MachineEmit` can stamp `Instr::MachineEmitPlaceholder`'s
    /// `machine_emit_id` regardless of which machine body context it
    /// appears in). `None` outside a machine step/lifecycle body.
    pub(crate) current_machine_emit_type_id: Option<u64>,
    /// Set to `true` inside a gen-block body builder to enable
    /// `HirExprKind::Yield` → `Terminator::Yield` construction.
    /// The parent builder's field stays `false` (the `Default`).
    /// A `Yield` node encountered when `in_gen_body` is `false` is a
    /// checker invariant violation (HIR should never surface `yield`
    /// outside a gen block) — fail-closed with `UnsupportedNode`.
    /// S3b will extend this context with the cross-yield live-set
    /// accumulator once liveness analysis lands.
    pub(crate) in_gen_body: bool,
    /// Set on the SHELL builder of a `receive gen fn` handler (a `HirFn`
    /// with `is_generator: true` lowered under `FunctionCallConv::ActorHandler`
    /// — derived in `lower_function`, never threaded as a separate parameter).
    /// When present, the `HirExprKind::GenBlock` dispatch arm in `lower_value`
    /// reshapes the shell into a stream-producer PUMP instead of returning the
    /// freshly-constructed generator handle: `gen_place` is driven with
    /// `Instr::GeneratorNext` in a loop, each yielded value is forwarded via
    /// `Terminator::Suspend`/`SuspendKind::StreamSend { sink, value }`, and a
    /// `None` result closes `sink` and falls off the end (Unit return). `None`
    /// for every other function, including a standalone `gen fn`/`gen {}`
    /// shell (`Default` call conv), which still returns the generator handle
    /// to its caller unchanged.
    pub(crate) stream_producer_pump: Option<StreamProducerPumpCtx>,
    /// Per-scope deferred bodies collected during statement lowering.
    /// Key: the `ScopeId` of the HIR scope that owns the defer.
    /// Value: deferred body expressions in registration order (FIFO).
    /// `emit_pending_defers(scope_id)` drains and lowers them in LIFO
    /// (reverse registration) order at every exit from that scope.
    ///
    /// Q205-B: bindings inside defer bodies resolve by lexical reference
    /// at execution time — mutable vars observe their final value;
    /// moved/consumed bindings are rejected by the move-checker at the
    /// materialization site.
    pub(crate) pending_defers: HashMap<ScopeId, Vec<HirExpr>>,
    /// W3.031 Stage 1: per-binding `TraitObjectStorage` ledger for
    /// `dyn Trait` locals. Populated at the binding's introducing
    /// `HirStmtKind::Let` statement when the resolved type is
    /// `ResolvedTy::TraitObject`. Current production lowering uses
    /// `HeapBoxed` for coercion-site RHS, owned parameter bindings, and
    /// call-result RHS (`HirExprKind::Call`, `CallTraitMethodStatic`,
    /// `CallDynMethod`) returning `dyn Trait`. Binding rebinds inherit the
    /// source binding's recorded storage. The ledger is consumed by
    /// `build_lifo_drops` to construct the
    /// `DropKind::TraitObject { storage }` discriminator.
    ///
    /// Keys are the `BindingId` of the owning `let`-binding (the same
    /// key used by `binding_locals` / `owned_locals`). A binding that
    /// the classifier could not resolve to one of the two storage
    /// shapes emits a `MirDiagnosticKind::TraitObjectStorageUndetermined`
    /// diagnostic and is not added to `owned_locals` — drop elaboration
    /// then skips the binding, and the pipeline aborts at the MIR
    /// boundary instead of fabricating a default storage.
    pub(crate) dyn_trait_storage: HashMap<BindingId, TraitObjectStorage>,
    /// Path-sensitive drop-flag for each non-idempotent user `#[resource]`
    /// binding (#1933 / #1941). Keyed by the resource's `BindingId` (same
    /// key as `binding_locals` / `owned_locals`); the value is a fresh
    /// `i64` `Place::Local` initialised to 0 at the binding's introduction
    /// and set to 1 at each `IntentKind::Consume` use site.
    ///
    /// Populated only for bindings that satisfy
    /// `affine_release_needs_drop_flag`: Rc/Weak owners and user resources
    /// whose ritual is a `DropFnSpec::UserClose`.
    /// A binding present here is KEPT in `owned_locals` across its consume
    /// (we do NOT call `mark_binding_moved` for it), so the per-exit
    /// `drops_for_exit` dataflow filter narrows the drop per control-flow
    /// path and codegen gates the surviving close on `flag == 0` — exactly
    /// once on a `MaybeConsumed` join. A user resource absent here (no flag
    /// allocated) falls back to the path-insensitive
    /// `mark_binding_moved` removal: fail-closed to no-double-close (it may
    /// leak on a not-consumed branch, but never
    /// double-frees the non-idempotent close).
    pub(crate) affine_release_flags: HashMap<BindingId, Place>,
    /// #2301 (extends #53): runtime drop-flags for an owned
    /// `var`-local that is BOTH genuinely consumed (`intent=Consume`, a
    /// move-out such as `let m = r`) on one control-flow path AND reassigned
    /// (`r = <rhs>`) on another. The path-insensitive `owned_locals` removal at
    /// the consume would otherwise make the overwrite-release static gate skip
    /// the release on the NON-consuming path, leaking the still-owned old value
    /// (~1 block/iteration in a loop). The flag is zero-init at the binding's
    /// `let` (so it dominates every consume and overwrite, including loop
    /// back-edges), set to 1 at each `mark_binding_moved`, gates the
    /// overwrite-release on `flag == 0`, and is reset to 0 after the overwrite
    /// stores a fresh value. The same flag guards any surviving scope-exit drop,
    /// including an inline enum's tag-aware drop, so the moved generation is
    /// skipped while a fresh post-store generation is released.
    pub(crate) overwrite_guard_flags: HashMap<BindingId, Place>,
    /// #2523: provenance for projected enum/machine payload binders, keyed on
    /// the binder's `BindingId`. Populated in `lower_match_enum_tag`'s binder
    /// loop for `MachineVariant`/`EnumVariant` sources; consulted at the
    /// binder's `Consume`-intent move-out to emit `NeutralizePayloadSlot` for
    /// the source slot and `AggregateAlias` for the scrutinee.
    pub(crate) projected_payload_provenance: HashMap<BindingId, ProjectedPayloadProvenance>,
    /// Path-sensitive delayed-release flags for direct `string` payload
    /// binders projected from a mutable inline-enum owner. The flag starts at
    /// one (the parent still owns the payload) and flips to zero when that
    /// parent slot is overwritten while the binder remains lexically live.
    /// Drop elaboration then promotes the binder to the sole release authority,
    /// guarded on `flag == 0`, so the old payload survives every in-arm read
    /// but is released on the arm's normal or early-exit edge. A later consume
    /// of the binder flips the flag back to one on that runtime path: release
    /// authority has moved onward, so a shared arm-close plan must skip it.
    pub(crate) projected_payload_overwrite_flags: HashMap<BindingId, Place>,
    /// Projected payload binders that can become the delayed sole release
    /// authority on at least one runtime path. Parent overwrite is one such
    /// transfer; forwarding a payload from an owned call carrier into a
    /// borrow-spine callee is another. Kept separate from the flag map so a
    /// binder whose parent remains the owner on every path stays a plain
    /// non-owning projection alias.
    pub(crate) projected_payload_delayed_releases: HashSet<BindingId>,
    /// Set while lowering a match-arm guard expression that
    /// can fall through to a later arm. A projected heap-payload binder consumed
    /// with this flag set is rejected fail-closed (`GuardedConsume`): its
    /// `NeutralizePayloadSlot` would run before the guard outcome is known, so a
    /// false guard would fall through to a later arm re-destructuring the
    /// now-null payload (null-fault / abort). Borrow-only guards never reach the
    /// consume hook and are unaffected. Saved/restored around each guard lowering
    /// so nested guards compose.
    pub(crate) in_fallthrough_match_guard: bool,
    /// #2418: runtime drop-flags for an owned collection local (owned-element
    /// `Vec`, plain `Vec`, `HashMap`/`HashSet` handle) that the pre-pass saw
    /// genuinely consumed (`intent=Consume`, a move-out such as `let ys = xs`)
    /// somewhere in the body. The path-insensitive `mark_binding_moved`
    /// retraction at the consume site removed the binding from the scope-exit
    /// set entirely, so a binding moved out on only SOME control-flow paths
    /// (`MaybeConsumed` at the converging join) leaked on the not-moved path —
    /// the whole-value `Move` lowering does NOT null the source slot, so the
    /// moved path cannot be discriminated statically at a shared exit.
    ///
    /// A flagged binding is KEPT in `owned_locals` at its consume sites
    /// (mirroring the affine release-flag discipline): the flag is an
    /// `i64` local zero-initialised at the binding's `let` (dominating every
    /// consume, including loop back-edges) and set to 1 at each consume-use.
    /// `build_lifo_drops` attaches it as the [`ElabDrop::guard`], so codegen
    /// gates the release on `flag == 0` — exactly once on a `MaybeConsumed`
    /// join: skipped where the value moved to a new owner, fired where it is
    /// still owned. The per-exit `drops_for_exit` dataflow filter still
    /// excludes exits where the binding is `Consumed` on every reaching path.
    ///
    /// Mutually exclusive with `overwrite_guard_flags` (a mutable binding both
    /// consumed and reassigned keeps the #2301 overwrite path and today's
    /// scope-exit retraction — fail-closed, no flag interplay). Bindings
    /// outside the collection classes keep the legacy retraction (leak on the
    /// not-moved path, never double-free).
    pub(crate) collection_drop_flags: HashMap<BindingId, Place>,
    /// Runtime drop-flags for actor-message `CowValue` leaf parameters that
    /// are consumed on at least one body path. Mailbox delivery makes the
    /// handler frame an owner, but a shared exit can see the parameter as
    /// `MaybeConsumed`; the flag preserves its scope-exit drop on the live
    /// path and suppresses it after a state/send transfer.
    ///
    /// Admission is structural: leaf `string` values use
    /// `cow_value_leaf_drop_symbol`, while `bytes` uses its dedicated
    /// `BytesTriple` admission/drop authority. The consume hook and
    /// `build_lifo_drops` consult this same map, so allocation, move marking,
    /// and guarded release cannot drift.
    pub(crate) actor_message_cow_drop_flags: HashMap<BindingId, Place>,
    /// Direct actor-call argument bindings whose finalized outbound mode may
    /// become `TransferLastUse` even when HIR intent remains borrow-like until
    /// CFG analysis. This pre-pass fact lets `ActorHandler` `bytes` allocate its
    /// path flag in the dominating parameter prologue.
    pub(crate) prepass_actor_message_transfer_bindings: HashSet<BindingId>,
    /// Path-sensitive drop flags for fresh monomorphic records whose admitted
    /// fields are String/BitCopy and which are consumed on at least one body
    /// path. W60.108 makes every String field in such a construction an
    /// independent owner; the flag preserves the local `RecordInPlace` drop on
    /// a sibling non-consuming path and suppresses it after ownership moves to
    /// a state/send/aggregate sink.
    ///
    /// These records are kept in `owned_locals` across consume sites. The flag
    /// is zero-initialised at the binding's `let`, set to one at every consume,
    /// and attached to its scope-exit drop. The narrow String/BitCopy
    /// construction gate excludes borrowed collection/opaque fields whose
    /// source ownership is not independently materialised.
    pub(crate) conditional_record_drop_flags: HashMap<BindingId, Place>,
    /// #2301 per-function pre-pass scratch: `BindingId`s used with
    /// `intent=Consume` anywhere in the body. Populated by
    /// `collect_vec_owned_element_keys_from_expr` before lowering; intersected
    /// with `prepass_reassigned_bindings` + `owned_locals` membership to decide
    /// which bindings get an `overwrite_guard_flags` entry.
    pub(crate) prepass_consumed_bindings: HashSet<BindingId>,
    /// #2418 per-function pre-pass scratch: `BindingId`s with a
    /// `intent=Consume` use in any position OTHER than a direct `let`-rebind
    /// initializer (`let y = xs;`) — a by-value call argument, an
    /// aggregate-literal field (`Holder { items: xs }`), a `return xs`, an
    /// assignment RHS, a match scrutinee, and every nested-expression read.
    /// A binding in this set never gets a `collection_drop_flags` entry:
    /// those consume shapes are owning-sink ESCAPES to the allow-set provers,
    /// and keeping the source registered (flagged) would leave it a candidate
    /// whose escape taints its whole whole-value alias group — excluding a
    /// destination the unflagged (retract-at-consume) path admits. Fail
    /// closed to the legacy retraction instead: behaviour byte-identical to
    /// the pre-flag compiler for every non-rebind consume shape.
    pub(crate) prepass_nonrebind_consumed: HashSet<BindingId>,
    /// #2301 per-function pre-pass scratch: `BindingId`s that are the target of an
    /// `Assign` (`r = <rhs>`) anywhere in the body.
    pub(crate) prepass_reassigned_bindings: HashSet<BindingId>,
    /// Let-bound aliases whose only parent-body use is a nested generator
    /// capture. Generator construction semantically clones these values.
    pub(crate) prepass_generator_capture_bindings: HashSet<BindingId>,
    /// Ordinary parent-body binding references, excluding direct let-rebind
    /// initializers and nested generator bodies.
    pub(crate) prepass_binding_ref_uses: HashSet<BindingId>,
    /// Stack of active scope IDs in nesting order (outermost at index 0,
    /// innermost at the end). Pushed when entering a `Block` expression or
    /// `function_body`; popped on exit. Read by `emit_defers_for_return` to
    /// walk the full scope chain on early-return paths, emitting defers for
    /// every enclosing scope that has registered bodies.
    pub(crate) active_scopes: Vec<ScopeId>,
    /// Stack of enclosing loop control targets, innermost at the end.
    ///
    /// `continue_target_bb` is the block a `continue` jumps to (the header for
    /// `while`/`while let`, the increment block for `for`, the body for bare
    /// `loop`). `exit_bb` is the block a `break` jumps to (always allocated,
    /// even for a `loop` with no `break`, so the post-loop cursor has a home).
    /// `scopes_depth_at_entry` is `active_scopes.len()` captured *before* the
    /// loop body scope is pushed; `emit_defers_for_break_continue` flushes
    /// `active_scopes[scopes_depth_at_entry..]` so break/continue run the
    /// defers of every scope opened inside the loop body (LIFO) without
    /// touching the loop's enclosing scopes.
    ///
    /// Pushed when entering a loop body, popped after the body walk. Labeled
    /// break/continue scans this stack from inner to outer and uses the matched
    /// frame's `scope_depth` as the bounded defer/drop flush window. Unknown
    /// labels are a type-checker error; MIR still reports a diagnostic instead
    /// of panicking if malformed HIR reaches this boundary.
    pub(crate) loop_stack: Vec<LoopFrame>,
    /// Stage 2 (gdb `-g`): byte-offset span `(start, end)` of the HIR
    /// statement / tail expression currently being lowered. Set at each
    /// statement boundary (`stmt`) and at the function tail (`function_body`);
    /// read by `push_instr` to attribute every emitted `Instr` to its
    /// originating source line. `None` outside any statement (synthesised
    /// prologue/epilogue work) — those instructions get NO side-table entry
    /// and inherit the nearest enclosing `DILocation` at codegen (fail-closed:
    /// a real but coarser line, never a fabricated one).
    pub(crate) current_span: Option<(u32, u32)>,
    /// Stage 2 (gdb `-g`): per-instruction source spans for THIS function,
    /// keyed by `(block_id, instruction_index)` and valued by the enclosing
    /// statement/expression byte span. Populated incrementally by `push_instr`
    /// (the index is the live `instructions.len()` before the push — the
    /// instruction's final position in its block, because the per-block buffer
    /// is moved out whole by `finish_current_block` / `seal_body_blocks`).
    /// Transferred into `RawMirFunction::instr_spans` at function finalisation.
    pub(crate) instr_spans: BTreeMap<(u32, u32), (u32, u32)>,
    /// Target pointer width (32 on wasm32, 64 native), threaded from the
    /// compile target so the `isize`/`usize` div/rem signed-MIN and shift-range
    /// trap guards emit the correct per-target constant. Derived from
    /// `TargetArch`, never a host `cfg!` (a cross-compile would otherwise emit
    /// host-width guards — a fail-open hole). Defaults to `Bits64` (every native
    /// target); the host-defaulting `lower_hir_module` wrapper keeps the default.
    pub(crate) pointer_width: PointerWidth,
}

#[must_use]
#[allow(
    clippy::too_many_lines,
    reason = "two-phase orchestration: per-fn lowering + per-monomorphisation lowering live \
              in the same function so the producers share record_field_orders, type_classes, \
              and module_fn_names construction"
)]
pub fn lower_hir_module(module: &HirModule) -> IrPipeline {
    lower_hir_module_with_facts(module, PointerWidth::default())
}

/// Resolve the proven source origin of a lowered function body for codegen
/// caret attribution.
///
/// Positive-membership only: a function is [`SourceOrigin::RootUnit`] (its span
/// indexes the root compilation unit's source text) ONLY when the HIR lowering
/// pass recorded its `ItemId` in `module.root_item_ids`. Everything else
/// degrades to [`SourceOrigin::Foreign`] (when the diagnostic source-module
/// table names its origin module) or [`SourceOrigin::Unknown`].
///
/// This never infers root by absence-from-a-foreign-set: a producer the foreign
/// table happens to miss resolves to `Unknown` (a bare plain-line at the render
/// site), never a false caret against the root source. Generated trait
/// default-method bodies are excluded from `root_item_ids` at the HIR site, so
/// their trait-indexed spans resolve away from `RootUnit`.
fn resolve_source_origin(id: hew_hir::ItemId, module: &HirModule) -> SourceOrigin {
    if module.root_item_ids.contains(&id) {
        SourceOrigin::RootUnit
    } else if let Some(src) = module.diagnostic_source_modules.get(&id) {
        SourceOrigin::Foreign(src.clone())
    } else {
        SourceOrigin::Unknown
    }
}

/// Lower a HIR module to MIR with target-specific facts.
///
/// `pointer_width` is the target pointer width (32 on wasm32, 64 native),
/// derived from the compile target so the `isize`/`usize` div/rem signed-MIN
/// and shift-range trap guards emit the correct per-target constant. The CLI
/// passes the `--target`-derived width; the host-defaulting `lower_hir_module`
/// wrapper passes `PointerWidth::default()` (`Bits64`).
#[must_use]
#[allow(
    clippy::too_many_lines,
    reason = "two-phase orchestration: per-fn lowering + per-monomorphisation lowering live \
              in the same function so the producers share record_field_orders, type_classes, \
              and module_fn_names construction"
)]
pub fn lower_hir_module_with_facts(module: &HirModule, pointer_width: PointerWidth) -> IrPipeline {
    fn resolve_coalesce_key_plan(
        actor: &HirActorDecl,
        handlers: &[ActorHandlerLayout],
        diagnostics: &mut Vec<MirDiagnostic>,
    ) -> Option<CoalesceKeyPlan> {
        let Some(hew_parser::ast::OverflowPolicy::Coalesce {
            key_field,
            fallback,
        }) = &actor.overflow_policy
        else {
            return None;
        };

        let mut entries = Vec::new();
        for handler in &actor.receive_handlers {
            let Some((param_index, param)) = handler
                .params
                .iter()
                .enumerate()
                .find(|(_, param)| param.name == *key_field)
            else {
                // Only coalescing message types require `key_field`; mixed handlers are valid.
                // This least-surprise rule intentionally avoids a blanket annotation burden on
                // unrelated handlers.
                continue;
            };
            let Some(kind) = coalesce_key_kind(&param.ty) else {
                diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::MailboxOverflowCoalesceKeyFieldInvalid {
                        actor: actor.name.clone(),
                        key_field: key_field.clone(),
                        reason: format!(
                            "handler `{}` parameter `{key_field}` has unsupported type `{}`; \
                             rc1 coalesce keys must be integer, bool, or string",
                            handler.name,
                            param.ty.user_facing()
                        ),
                    },
                    note: format!(
                        "actor `{}` cannot generate a coalesce key for handler `{}`",
                        actor.name, handler.name
                    ),
                });
                return None;
            };
            let Some(layout) = handlers.iter().find(|layout| layout.name == handler.name) else {
                diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::MailboxOverflowCoalesceKeyFieldInvalid {
                        actor: actor.name.clone(),
                        key_field: key_field.clone(),
                        reason: format!(
                            "handler `{}` has no MIR handler layout for key extraction",
                            handler.name
                        ),
                    },
                    note: format!(
                        "actor `{}` cannot generate a total coalesce key plan",
                        actor.name
                    ),
                });
                return None;
            };
            let Ok(param_index) = u32::try_from(param_index) else {
                diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::MailboxOverflowCoalesceKeyFieldInvalid {
                        actor: actor.name.clone(),
                        key_field: key_field.clone(),
                        reason: format!(
                            "handler `{}` parameter index exceeds the codegen ABI limit",
                            handler.name
                        ),
                    },
                    note: format!(
                        "actor `{}` cannot generate a coalesce key for handler `{}`",
                        actor.name, handler.name
                    ),
                });
                return None;
            };
            entries.push(CoalesceKeyEntry {
                msg_type: layout.msg_type,
                param_index,
                kind,
            });
        }

        if entries.is_empty() {
            diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::MailboxOverflowCoalesceKeyFieldInvalid {
                    actor: actor.name.clone(),
                    key_field: key_field.clone(),
                    reason: "no receive handler declares that parameter".to_string(),
                },
                note: format!(
                    "actor `{}` cannot generate a coalesce key function for `{key_field}`",
                    actor.name
                ),
            });
            return None;
        }

        Some(CoalesceKeyPlan {
            entries,
            fallback: overflow_fallback_to_i32(fallback.as_ref()),
        })
    }

    fn coalesce_key_kind(ty: &ResolvedTy) -> Option<CoalesceKeyKind> {
        let width = match ty {
            ResolvedTy::I8 | ResolvedTy::U8 => Some(8),
            ResolvedTy::I16 | ResolvedTy::U16 => Some(16),
            ResolvedTy::I32 | ResolvedTy::U32 => Some(32),
            ResolvedTy::I64 | ResolvedTy::U64 => Some(64),
            ResolvedTy::Isize | ResolvedTy::Usize => u8::try_from(usize::BITS).ok(),
            _ => None,
        };
        if let Some(width) = width {
            return Some(CoalesceKeyKind::IntZext { width });
        }
        match ty {
            ResolvedTy::Bool => Some(CoalesceKeyKind::BoolZext),
            ResolvedTy::String => Some(CoalesceKeyKind::StringHash),
            _ => None,
        }
    }

    fn overflow_fallback_to_i32(fallback: Option<&hew_parser::ast::OverflowFallback>) -> i32 {
        use hew_parser::ast::OverflowFallback;
        match fallback {
            Some(OverflowFallback::Block) => 0,
            None | Some(OverflowFallback::DropNew) => 1,
            Some(OverflowFallback::DropOld) => 2,
            Some(OverflowFallback::Fail) => 3,
        }
    }

    let mut thir = Vec::new();
    let mut raw_mir = Vec::new();
    let mut checked_mir = Vec::new();
    let mut elaborated_mir = Vec::new();
    let mut diagnostics = Vec::new();
    let (user_consts, const_diagnostics) = build_const_descriptors(module);
    diagnostics.extend(const_diagnostics);
    // W5.007a: abstract (un-monomorphised) MIR for generic origins. Routed
    // to `IrPipeline::polymorphic_mir`; never fed to codegen.
    let mut polymorphic_mir: Vec<crate::model::PolymorphicMirFunction> = Vec::new();

    // Build the declaration-order field descriptor table once for the whole module.
    // Keys are record type names; values are (field_name, field_ty) pairs in
    // declaration order. Used by StructInit and FieldAccess lowering to resolve
    // a field name to its 0-based FieldOffset and to look up field types for
    // intermediate place allocation during functional-update desugaring. Tuple
    // records contribute synthetic ordinal names for structural ownership
    // classification; their constructor remains a Call, not a StructInit.
    let mut record_field_orders: HashMap<String, Vec<(String, ResolvedTy)>> = HashMap::new();
    let mut record_layouts: Vec<crate::model::RecordLayout> = Vec::new();
    let mut actor_layouts: Vec<crate::model::ActorLayout> = Vec::new();
    let mut supervisor_layouts: Vec<crate::model::SupervisorLayout> = Vec::new();
    let mut machine_layouts: Vec<crate::model::MachineLayout> = Vec::new();
    let mut enum_layouts: Vec<crate::model::EnumLayout> = Vec::new();
    // Canonical classification keys for ordinary enums.  The Builder's
    // `machine_layout_names` registry uses the machine class-tagged projection
    // for every tagged union, regardless of which layout vector owns its
    // physical descriptor.  Keeping this set beside enum construction avoids
    // trying to reverse a generic enum's emitted symbol back into its nominal
    // owner and type-argument spine later.
    let mut enum_classification_keys: HashSet<String> = HashSet::new();
    // Named-fn invoke shims are synthesised per-use-site inside each
    // `lower_function` builder, so the same shim can appear in
    // `lowered.generated` for every function that references the same
    // named function as a value.  Guard the module-level collection with
    // a seen-set keyed on shim name so the body is emitted exactly once.
    let mut emitted_named_fn_shims: HashSet<String> = HashSet::new();
    // Actor state-field classification is deferred to a second pass (below,
    // after generic record/enum instantiations and builtin enums are merged
    // into `record_layouts`/`enum_layouts`). The classifier must see the
    // FULLY-populated layout registries so a state field typed as a generic
    // enum instantiation (`Option<string>`, `Result<i64, string>` → mangled
    // `Option$$string`, `Result$$i64$string`) resolves to its tagged-union
    // layout rather than falling to the paired-`None` fail-closed path.
    // Collected in source order so the resulting `actor_layouts` order is
    // unchanged from the single-pass form.
    let mut deferred_actors: Vec<&HirActorDecl> = Vec::new();
    // Collect supervisor child init_args from HIR during the item loop.
    // The post-loop pass converts these to `ChildInitArg` literals using the
    // actor-layout map (which isn't fully built until after the item loop).
    // Key: (supervisor_name, child_name) → Vec<(field_name, HirExpr)>.
    let mut supervisor_child_hir_init_args: HashMap<(String, String), Vec<(String, HirExpr)>> =
        HashMap::new();
    // Collect pool children's reserved `count:` expr (the static pool size).
    // Key: (supervisor_name, child_name) → the count HirExpr. Lowered to
    // `PoolCount` in the post-loop pass once config params are resolvable.
    let mut supervisor_child_hir_pool_count: HashMap<(String, String), HirExpr> = HashMap::new();
    // The registry key for a monomorphic module type: its qualified identity
    // Declaration ownership is semantic identity, including when only one
    // module currently exports a given leaf name. The checker puts that full
    // owner in every imported `ResolvedTy`, so all declaration-derived layout
    // tables must use the same full key rather than a collision-triggered leaf
    // spelling. Native symbol emission applies `mangle_dotted_name` later.
    let type_layout_key = |_bare: &str, qualified: String| -> String { qualified };
    for item in &module.items {
        match item {
            HirItem::Record(decl) => {
                // Generic record decls (`record Box<T> { ... }`) emit zero
                // bare-name layouts: their per-instantiation layouts come
                // from `module.record_layouts` under mangled names. Only
                // monomorphic records register a bare-name field order.
                if !decl.type_params.is_empty() {
                    continue;
                }
                let fields: Vec<(String, ResolvedTy)> = if decl.fields.is_empty() {
                    // Tuple records deliberately expose no named fields in
                    // HIR, but their positional payload is real stored value
                    // state. Synthetic numeric names preserve declaration
                    // order for layout/classification without enabling field
                    // access (the checker continues to reject `.0`/`.1`).
                    decl.positional_field_tys
                        .iter()
                        .enumerate()
                        .map(|(index, ty)| (index.to_string(), ty.clone()))
                        .collect()
                } else {
                    decl.fields
                        .iter()
                        .map(|f| (f.name.clone(), f.ty.clone()))
                        .collect()
                };
                let layout_key = type_layout_key(&decl.name, decl.qualified_name());
                if !fields.is_empty() {
                    record_layouts.push(crate::model::RecordLayout {
                        name: layout_key.clone(),
                        field_tys: fields.iter().map(|(_, ty)| ty.clone()).collect(),
                        field_names: fields.iter().map(|(name, _)| name.clone()).collect(),
                    });
                }
                record_field_orders.insert(layout_key, fields);
            }
            HirItem::TypeDecl(decl) => {
                // `pub type Foo { ... }` and `pub type Foo<T> { ... }` are
                // structurally the named-record substrate that
                // `Expr::StructInit` targets. Generic `pub type Foo<T>`
                // declarations emit zero bare-name layouts (same rule as
                // generic records); their per-instantiation layouts come
                // from `module.record_layouts` under mangled names.
                //
                // Generic enums (`enum Maybe<T> { Just(T); Nothing }`) are
                // handled via the HIR EnumLayoutRegistry: `module.enum_layouts`
                // carries one substituted entry per instantiation discovered
                // during HIR mono-pass. Those entries are emitted into
                // `enum_layouts` below the item loop (mirroring the
                // `module.record_layouts` precedent for generic records).
                // The bare-name generic decl itself emits no layout here;
                // an unused generic enum declaration is a non-event.
                if !decl.type_params.is_empty() {
                    continue;
                }
                // Same collision-aware keying as the record arm: a same-bare
                // `pub type` (struct- OR enum-shaped) from two packages keys by
                // its qualified identity; a unique bare name keys bare.
                let layout_key = type_layout_key(&decl.name, decl.qualified_name());
                let fields: Vec<(String, ResolvedTy)> = decl
                    .fields
                    .iter()
                    .map(|f| (f.name.clone(), f.ty.clone()))
                    .collect();
                // A zero-field STRUCT declaration still owns a concrete
                // layout. The bundled lambda-actor handles deliberately use
                // this shape: their actual representation is supplied by the
                // runtime ABI, but HIR/MIR must retain the declaration so a
                // source-owned constructor never falls through as unknown.
                // Enums use their distinct tagged-union layout below.
                if decl.variants.is_empty() {
                    record_layouts.push(crate::model::RecordLayout {
                        name: layout_key.clone(),
                        field_tys: fields.iter().map(|(_, ty)| ty.clone()).collect(),
                        field_names: fields.iter().map(|(name, _)| name.clone()).collect(),
                    });
                    record_field_orders.insert(layout_key.clone(), fields);
                }
                // Enum-kind type decls: register a tagged-union layout for
                // every monomorphic enum, walking ALL variants (unit, tuple,
                // struct) in declaration order. Variant index in the layout
                // matches the index assigned by the HIR ctor pre-pass
                // (`hew_hir::lower` walks `td.body` in the same order), so
                // ctor sites and match-arm dispatch agree on tag values.
                if !decl.variants.is_empty() {
                    let variant_count =
                        u32::try_from(decl.variants.len().max(1)).unwrap_or(u32::MAX);
                    let tag_width = u32::max(1, variant_count.next_power_of_two().trailing_zeros());
                    let variants: Vec<crate::model::MachineVariantLayout> = decl
                        .variants
                        .iter()
                        .map(|v| crate::model::MachineVariantLayout {
                            name: v.name.clone(),
                            field_tys: v.field_tys(),
                            field_names: v.field_names(),
                        })
                        .collect();
                    // Enum-shaped pub types collide the same way (R6): a
                    // same-bare-name enum from two packages keys by its
                    // qualified identity, a unique one stays bare.
                    enum_classification_keys.insert(hew_hir::machine_layout_key(&layout_key, &[]));
                    enum_layouts.push(crate::model::EnumLayout {
                        name: layout_key,
                        tag_width,
                        variants,
                        is_indirect: decl.is_indirect,
                    });
                }
            }
            HirItem::Actor(actor) => {
                if !actor.state_fields.is_empty() {
                    // State-record key = the actor's qualified identity, the
                    // same key the spawn-site `RecordInit` type and the
                    // actor-layout registry use, so two same-named module
                    // actors get distinct state structs.
                    record_layouts.push(crate::model::RecordLayout {
                        name: actor.qualified_name(),
                        field_tys: actor
                            .state_fields
                            .iter()
                            .map(|field| field.ty.clone())
                            .collect(),
                        field_names: actor
                            .state_fields
                            .iter()
                            .map(|field| field.name.clone())
                            .collect(),
                    });
                }
                // W2.002 Stage 1 — per-actor state-field clone
                // classification is DEFERRED to the second pass below.
                // Collecting the actor here (rather than classifying inline)
                // lets the classifier run against the fully-merged
                // `record_layouts`/`enum_layouts` — including generic record
                // and enum instantiations from `module.record_layouts` /
                // `module.enum_layouts` and the builtin monomorphic enums —
                // so a state field typed as a generic enum instantiation
                // resolves to its tagged-union layout instead of falling to
                // the paired-`None` fail-closed path. The actor's own state
                // `RecordLayout` is still registered inline above so other
                // records declared later can resolve it.
                deferred_actors.push(actor);
            }
            HirItem::Supervisor(sup) => {
                // Supervisors compile to a layout (consumed by codegen for
                // the per-supervisor restart/registration table) plus a
                // bootstrap function (emitted in the second-pass loop below).
                // The layout's `children` vector is ordered by topological
                // spawn order so dependents observe their `wired_to:` deps
                // already spawned.
                if let Some(layout) = build_supervisor_layout(sup, &mut diagnostics) {
                    supervisor_layouts.push(layout);
                }
                // Stash each child's HIR init_args keyed by (sup_name, child_name).
                // These are lowered to ChildInitArg literals in the post-loop pass
                // once the actor-layout map is available for field validation.
                for child in &sup.children {
                    if !child.init_args.is_empty() {
                        supervisor_child_hir_init_args.insert(
                            (sup.name.clone(), child.name.clone()),
                            child.init_args.clone(),
                        );
                    }
                    if let Some(count_expr) = &child.pool_count {
                        supervisor_child_hir_pool_count
                            .insert((sup.name.clone(), child.name.clone()), count_expr.clone());
                    }
                }
            }
            _ => {}
        }
    }

    // Emit one MIR `RecordLayout` per HIR `record_layouts` entry under the
    // mangled symbol name. The HIR registry has already substituted the
    // type-parameter symbols with concrete `ResolvedTy`s, so the MIR layer
    // can read the field list verbatim. Insertion-ordered to keep codegen
    // deterministic; per-instantiation field_orders are keyed by the
    // mangled name so `StructInit` / `FieldAccess` lowering can find them
    // by mangling `(record_name, type_args)` from the expression's type.
    for layout in &module.record_layouts {
        let fields: Vec<(String, ResolvedTy)> = layout.fields.clone();
        record_layouts.push(crate::model::RecordLayout {
            name: layout.mangled_name.clone(),
            field_tys: fields.iter().map(|(_, ty)| ty.clone()).collect(),
            field_names: fields.iter().map(|(name, _)| name.clone()).collect(),
        });
        record_field_orders.insert(layout.mangled_name.clone(), fields);
    }
    // Emit one MIR `EnumLayout` per HIR `enum_layouts` entry under the
    // mangled symbol name. The HIR mono pass has already substituted
    // type-parameter symbols with concrete `ResolvedTy`s in each variant's
    // `field_tys`, so the MIR layer reads the variant list verbatim.
    // Variant index ordering is HIR-ctor-pre-pass authoritative (declaration
    // order, matching `machine_ctor_registry` assignments). Insertion order
    // is preserved from the HIR registry for codegen determinism.
    // LESSONS: `end-to-end-before-layer-thickening` (P1) — both layout
    // emission (this loop) and value-class resolution (Slice 4) must land
    // in the same cluster; the HIR registry is unconsumed scaffolding without
    // this consumer.
    // Register layouts for monomorphic builtin enums declared in
    // stdlib authority sources (for example `std.builtins.LookupError`). These never appear in
    // `module.items` (builtins.hew is consumed for signature wiring, not
    // emitted into the items list) and never appear in
    // `module.enum_layouts` (they have no type params, so
    // `EnumLayoutRegistry` produces no per-instantiation entry). Without
    // this hook MIR/codegen could not see their tagged-union layout.
    // The returned name list is folded into `machine_layout_names` below
    // so `Builder::is_known_actor_runtime_ty` classifies sites typed as
    // the builtin enum.
    //
    // ORDER: must run BEFORE the generic-enum instantiation loop below.
    // `register_enum_layouts` in codegen processes `pipeline.enum_layouts`
    // in order; an entry's `build_tagged_union_layout` call resolves each
    // variant's `field_tys` against the layouts registered so far. Generic
    // instantiations like `Result<RemotePid<T>, LookupError>` reference the
    // canonical `Named { name: "std.builtins.LookupError" }` in their Err
    // variant — that exact layout must be registered first.
    enum_classification_keys.extend(
        register_builtin_monomorphic_enum_layouts(&mut enum_layouts)
            .into_iter()
            .map(|name| hew_hir::machine_layout_key(&name, &[])),
    );

    for hir_layout in &module.enum_layouts {
        let variant_count = u32::try_from(hir_layout.variants.len().max(1)).unwrap_or(u32::MAX);
        let tag_width = u32::max(1, variant_count.next_power_of_two().trailing_zeros());
        let variants: Vec<crate::model::MachineVariantLayout> = hir_layout
            .variants
            .iter()
            .map(|v| crate::model::MachineVariantLayout {
                name: v.name.clone(),
                field_tys: v.field_tys.clone(),
                // Generic-enum instantiation payloads carry no field names in
                // the HIR mono layout (names are not load-bearing for value
                // codegen); `-g` falls back to positional `field_N` here.
                field_names: Vec::new(),
            })
            .collect();
        enum_classification_keys.insert(hew_hir::machine_layout_key(
            &hir_layout.key.origin_name,
            &hir_layout.key.type_args,
        ));
        enum_layouts.push(crate::model::EnumLayout {
            name: hir_layout.mangled_name.clone(),
            tag_width,
            variants,
            // Generic enum instantiations (e.g. `Option<i64>`) are never
            // `indirect` — the `indirect` modifier is only allowed on
            // monomorphic user-declared enums.
            is_indirect: false,
        });
    }
    // Register the synthetic `__HewChildLookupResult` record layout used by the
    // `FieldAccess` supervisor intercept arm (S2). The runtime struct is:
    //   { u8 tag, u8 reason, [u8;6] _pad, *mut HewActor handle }  (16 bytes, C ABI)
    // For MIR purposes we flatten this to two fields that S3 codegen maps to
    // `extractvalue` indices on the struct-return LLVM value:
    //   field 0 "tag"    : i64  (zero-extended from u8; tag 0=Live, 1=Transient, 2=Dead)
    //   field 1 "handle" : i64  (pointer-width integer; cast to actor pointer at codegen)
    // Using i64 for both avoids introducing a pointer-or-u8 type into MIR.
    // S3 emits the correct LLVM ABI types (i8 and ptr) when it lowers the
    // `CallRuntimeAbi`+`RecordFieldLoad` sequence into LLVM IR.
    //
    // Decision: option (b) from the plan — scratch-alloca + RecordFieldLoad.
    // WHY: reusing existing `CallRuntimeAbi` (dest = struct local) and
    //   `RecordFieldLoad` avoids any new `Instr` variant and keeps the S3
    //   match-arm cascade at zero lines for S2.
    // WHEN obsolete: when S3 wires LLVM emission for `hew_supervisor_child_get`.
    // WHAT: S3 interprets `CallRuntimeAbi` whose `dest` local is typed
    //   `__HewChildLookupResult` as a struct-return call and emits `extractvalue`.
    let child_lookup_fields: Vec<(String, ResolvedTy)> = vec![
        ("tag".to_string(), ResolvedTy::I64),
        ("handle".to_string(), ResolvedTy::I64),
    ];
    record_layouts.push(crate::model::RecordLayout {
        name: CHILD_LOOKUP_RESULT_TY_NAME.to_string(),
        field_tys: child_lookup_fields
            .iter()
            .map(|(_, ty)| ty.clone())
            .collect(),
        field_names: child_lookup_fields
            .iter()
            .map(|(name, _)| name.clone())
            .collect(),
    });
    record_field_orders.insert(CHILD_LOOKUP_RESULT_TY_NAME.to_string(), child_lookup_fields);

    // Compiler-known struct-shaped builtins are registered from the HIR builtin
    // type registry. The normal item loop above remains authoritative when a
    // module-graph-loaded stdlib TypeDecl is present; this fills only missing
    // substrate records needed by synthetic MIR construction.
    register_builtin_record_layouts(&mut record_layouts, &mut record_field_orders);
    let lifecycle_enum_start = enum_layouts.len();
    register_lifecycle_hook_layouts(
        &mut record_layouts,
        &mut record_field_orders,
        &mut enum_layouts,
    );
    enum_classification_keys.extend(
        enum_layouts[lifecycle_enum_start..]
            .iter()
            .map(|layout| hew_hir::machine_layout_key(&layout.name, &[])),
    );

    // Pre-compute the opaque handle names and resource-close registry before the
    // state-field classification pass so the classifier can distinguish plain
    // `#[opaque]` handles from `#[resource] #[opaque]` handles. The same registry
    // is moved into the `IrPipeline` below, keeping MIR admission and codegen
    // drop synthesis on one classification authority.
    let opaque_handle_names: Vec<String> = module
        .items
        .iter()
        .filter_map(|item| match item {
            HirItem::TypeDecl(decl) if decl.is_opaque => Some(decl.qualified_name()),
            _ => None,
        })
        .collect();
    let lifecycle_registry = module.type_classes.lifecycle_registry().clone();

    // Machines are enums at the value-classification layer.  Materialise one
    // layout for every HIR machine-mono entry, not one bare layout per source
    // declaration.  A machine's nominal owner and its concrete arguments are
    // semantic identity: `left.Lifecycle<i64>` and `right.Lifecycle<i64>` are
    // distinct, as are `Lifecycle<i64>` and `Lifecycle<string>`.
    let machine_decls_by_id: HashMap<hew_hir::ItemId, &HirMachineDecl> = module
        .items
        .iter()
        .filter_map(|item| match item {
            HirItem::Machine(machine) => Some((machine.id, machine)),
            _ => None,
        })
        .collect();
    let mut seen_machine_layout_keys: HashSet<String> = HashSet::new();
    for entry in &module.machine_instantiations {
        let Some(machine) = machine_decls_by_id.get(&entry.key.origin).copied() else {
            diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::UnsupportedNode {
                    reason: format!(
                        "machine monomorphisation `{}` references missing machine item {:?}",
                        entry.key.origin_name, entry.key.origin
                    ),
                },
                note: "the HIR machine-mono registry must reference an item in this module"
                    .to_string(),
            });
            continue;
        };
        let canonical_origin = machine.qualified_name();
        if entry.key.origin_name != canonical_origin {
            diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::UnsupportedNode {
                    reason: format!(
                        "machine monomorphisation origin `{}` does not match declaration identity `{canonical_origin}`",
                        entry.key.origin_name
                    ),
                },
                note: "machine layout identity is exact; MIR does not recover a declaration by leaf name"
                    .to_string(),
            });
            continue;
        }
        if entry.key.type_args.len() != machine.type_params.len()
            || !entry.key.const_args.is_empty()
        {
            diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::UnsupportedNode {
                    reason: format!(
                        "machine monomorphisation `{canonical_origin}` has {} type args and {} const args, but its declaration has {} type params",
                        entry.key.type_args.len(),
                        entry.key.const_args.len(),
                        machine.type_params.len(),
                    ),
                },
                note: "MIR requires a complete concrete argument spine for every machine layout"
                    .to_string(),
            });
            continue;
        }
        let layout_name = entry.key.mangle();
        debug_assert_eq!(
            layout_name,
            hew_hir::machine_layout_key(&canonical_origin, &entry.key.type_args),
            "MachineMonoKey and ResolvedTy-derived machine layout keys must agree"
        );
        if !seen_machine_layout_keys.insert(layout_name.clone()) {
            diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::UnsupportedNode {
                    reason: format!(
                        "duplicate concrete machine layout key `{layout_name}` in HIR machine-mono registry"
                    ),
                },
                note: "machine-mono keys must be unique by declaration identity and concrete arguments"
                    .to_string(),
            });
            continue;
        }
        let event_origin = format!("{canonical_origin}Event");
        let event_name = hew_hir::machine_layout_key(&event_origin, &entry.key.type_args);
        let subst: HashMap<String, ResolvedTy> = machine
            .type_params
            .iter()
            .cloned()
            .zip(entry.key.type_args.iter().cloned())
            .collect();
        machine_layouts.push(build_machine_layout(
            machine,
            layout_name,
            event_name,
            &subst,
        ));
    }
    let machine_enum_views: Vec<crate::model::EnumLayout> =
        crate::model::machine_enum_views(&machine_layouts);

    // Second pass — per-actor state-field clone/drop classification.
    // Deferred from the item loop so the classifier sees the fully-merged
    // `record_layouts`/`enum_layouts`: monomorphic user records/enums (item
    // loop), generic record instantiations (`module.record_layouts`), generic
    // enum instantiations (`module.enum_layouts`), builtin monomorphic enums,
    // and builtin record layouts are all registered by now. A state field
    // typed as a generic enum instantiation (e.g. `Option<string>` →
    // `Option$$string`, `Result<i64, string>` → `Result$$i64$$string`) thus
    // resolves to its tagged-union layout and routes through the enum
    // clone/drop helpers rather than the paired-`None` fail-closed path.
    // Single classification authority (`state_clone::*`); no duplicate
    // divergent classifier call sites. `deferred_actors` is in source order,
    // so `actor_layouts` order matches the pre-refactor single-pass form.
    //
    // The classification view = real enum layouts + machine-as-enum
    // projections, so machine-typed fields take the Enum arm.
    //
    // This same view is threaded into every function-body builder below
    // (user/generic functions, actor handlers, supervisor bootstrap, and
    // machine `__step` fns) as their `enum_layouts`, so a machine held as a
    // field of a user `type`/record classifies through the one enum-view
    // authority — `owned_aggregate_record_field_kinds_for_key` resolves the
    // machine field via `classify_enum` exactly as it does an enum field,
    // payload-free and payload-bearing alike. The bare `enum_layouts`
    // (machine-free) still flows to codegen's `register_enum_layouts` and
    // the layout diagnostics, which register machines separately via
    // `machine_layouts`. Without the augmented view the user-record
    // classifier missed the machine layout and the record was rejected with
    // `UnsupportedUserRecordValueClass` ("value class Unknown").
    let classification_enum_layouts: Vec<crate::model::EnumLayout> = enum_layouts
        .iter()
        .cloned()
        .chain(machine_enum_views.iter().cloned())
        .collect();
    for &actor in &deferred_actors {
        // Run BEFORE constructing the `ActorLayout` so the classifier outcome
        // decides whether to populate the `state_clone_fn_symbol` /
        // `state_drop_fn_symbol` pair (paired Some/None per the model.rs field
        // doc; substrate-first per dispatch-invariant #1).
        let state_field_tys: Vec<ResolvedTy> = actor
            .state_fields
            .iter()
            .map(|field| field.ty.clone())
            .collect();
        // Closure-valued actor state is rejected before classification: the
        // classifier now admits `fn(...)` fields for the RECORD value-class
        // (drop-only), but actor state additionally needs the CLONE direction
        // for supervisor restart, and a closure pair's environment has a sole
        // owner with no retain/deep-copy path. Transitive ty-level walk (a
        // direct fn field, a record-with-closure field, a Vec<fn> field, and
        // nestings all reject through one decision) so the refusal lands at
        // MIR compile time, never as a runtime restart-clone failure.
        let closure_field = actor.state_fields.iter().enumerate().find(|(_, field)| {
            crate::model::ty_contains_closure_value(&field.ty, &record_layouts, &enum_layouts)
        });
        let classification =
            crate::state_clone::classify_actor_state_fields_with_lifecycle_registry(
                &state_field_tys,
                &record_layouts,
                &classification_enum_layouts,
                &opaque_handle_names,
                &lifecycle_registry,
            );
        let (clone_sym, drop_sym, clone_kinds) = if let Some((field_index, field)) = closure_field {
            let reason = format!(
                "field `{}` holds (or transitively contains) a function value; \
                 a closure's environment has a sole owner and no clone path, so \
                 closure-valued actor state is not supported — store the closure \
                 in a local or a record instead",
                field.name
            );
            diagnostics.push(crate::model::MirDiagnostic {
                kind: crate::model::MirDiagnosticKind::ActorStateCloneClassificationFailed {
                    actor: actor.name.clone(),
                    field_index,
                    field_name: field.name.clone(),
                    reason: reason.clone(),
                },
                note: format!(
                    "actor `{}` state-field classifier rejected closure-valued state: {}",
                    actor.name, reason
                ),
            });
            (None, None, None)
        } else {
            match classification {
                Ok(kinds) => (
                    Some(crate::state_clone::mangle_actor_state_clone_fn(
                        &actor_symbol_base(actor),
                    )),
                    Some(crate::state_clone::mangle_actor_state_drop_fn(
                        &actor_symbol_base(actor),
                    )),
                    Some(kinds),
                ),
                Err(err) => {
                    // Locate the failing field index by re-running the classifier
                    // per-field with a fresh visited set. This is O(n) on the
                    // field count (always small — actor state has at most a
                    // handful of fields in the corpus) and produces a precise
                    // diagnostic anchor rather than blaming the whole actor. If
                    // every per-field run somehow succeeds (impossible given the
                    // collected error, but defensive), fall back to index 0 with a
                    // marker reason rather than `unreachable!`.
                    let mut field_index = 0usize;
                    let mut field_name = actor
                        .state_fields
                        .first()
                        .map(|f| f.name.clone())
                        .unwrap_or_default();
                    let mut found = false;
                    for (idx, field) in actor.state_fields.iter().enumerate() {
                        let mut v = std::collections::HashSet::new();
                        if crate::state_clone::classify_state_field_with_lifecycle_registry(
                            &field.ty,
                            &record_layouts,
                            &classification_enum_layouts,
                            &opaque_handle_names,
                            &lifecycle_registry,
                            &mut v,
                        )
                        .is_err()
                        {
                            field_index = idx;
                            field_name.clone_from(&field.name);
                            found = true;
                            break;
                        }
                    }
                    let reason = if found {
                        format!("{err}")
                    } else {
                        format!(
                            "{err} (per-field re-run could not localise; \
                         classifier saw an aggregate error)"
                        )
                    };
                    diagnostics.push(crate::model::MirDiagnostic {
                        kind:
                            crate::model::MirDiagnosticKind::ActorStateCloneClassificationFailed {
                                actor: actor.name.clone(),
                                field_index,
                                field_name,
                                reason: reason.clone(),
                            },
                        note: format!(
                            "actor `{}` state-field classifier failed: {}",
                            actor.name, reason
                        ),
                    });
                    (None, None, None)
                }
            }
        };
        // Fail closed when a `receive fn` has no protocol-issued
        // discriminant. `lower_actor_handler_layouts` cannot invent one — a
        // fabricated `msg_type` is a duplicate-switch-case LLVM verify reject
        // at two-plus handlers and silent wire-protocol corruption (wrong
        // discriminant, in-process-only dispatch) at exactly one — so it
        // refuses the whole layout instead. The `ActorLayout` is still
        // published, with zero handler rows: downstream lookups
        // (`actor_method_info`, the coalesce key plan) already fail closed on
        // an absent row, and the hard error below is what stops the compile.
        let handlers = match lower_actor_handler_layouts(actor) {
            Ok(handlers) => handlers,
            Err(reason) => {
                let detail = match reason {
                    ProtocolDescriptorMissing::NoDescriptor => {
                        "carries no protocol descriptor".to_string()
                    }
                    ProtocolDescriptorMissing::NoHandlerId { handler } => format!(
                        "carries a protocol descriptor that issues no msg_id for handler `{handler}`"
                    ),
                };
                diagnostics.push(crate::model::MirDiagnostic {
                    kind: crate::model::MirDiagnosticKind::ActorProtocolDescriptorMissing {
                        actor: actor.qualified_name(),
                        handler_count: actor.receive_handlers.len(),
                    },
                    note: format!(
                        "actor `{}` declares {} `receive fn` handler(s) but {detail}; \
                         message-kind discriminants cannot be assigned, so lowering \
                         refuses instead of fabricating one",
                        actor.qualified_name(),
                        actor.receive_handlers.len()
                    ),
                });
                Vec::new()
            }
        };
        let coalesce_key_plan = resolve_coalesce_key_plan(actor, &handlers, &mut diagnostics);
        actor_layouts.push(crate::model::ActorLayout {
            // The registry key is the actor's qualified identity: dotted
            // `module.Name` for module actors, bare for root actors. It
            // matches the checker's `LocalPid<T>` inner name, so ask-site
            // dispatch (`actor_method_info`) and spawn resolution read the
            // right layout even when two modules export the same bare name.
            name: actor.qualified_name(),
            defining_module: actor.defining_module.clone(),
            state_field_names: actor
                .state_fields
                .iter()
                .map(|field| field.name.clone())
                .collect(),
            state_field_tys: actor
                .state_fields
                .iter()
                .map(|field| field.ty.clone())
                .collect(),
            state_field_defaults: actor
                .state_fields
                .iter()
                .map(|field| field.default.clone())
                .collect(),
            init_param_names: actor
                .init
                .as_ref()
                .map(|init| init.params.iter().map(|param| param.name.clone()).collect())
                .unwrap_or_default(),
            init_param_tys: actor
                .init
                .as_ref()
                .map(|init| init.params.iter().map(|param| param.ty.clone()).collect())
                .unwrap_or_default(),
            init_symbol: actor
                .init
                .as_ref()
                .map(|_| mangle_actor_init_handler(&actor_symbol_base(actor))),
            on_start_symbol: actor
                .lifecycle_hooks
                .iter()
                .find(|hook| hook.kind == HirLifecycleHookKind::Start)
                .map(|_| mangle_actor_start_handler(&actor_symbol_base(actor))),
            on_stop_symbols: actor
                .lifecycle_hooks
                .iter()
                .enumerate()
                .filter(|(_, hook)| hook.kind == HirLifecycleHookKind::Stop)
                .map(|(idx, _)| mangle_actor_stop_handler_indexed(&actor_symbol_base(actor), idx))
                .collect(),
            on_crash_symbol: actor
                .lifecycle_hooks
                .iter()
                .find(|hook| hook.kind == HirLifecycleHookKind::Crash)
                .map(|_| mangle_actor_crash_handler(&actor_symbol_base(actor))),
            on_exit_symbol: actor
                .lifecycle_hooks
                .iter()
                .find(|hook| hook.kind == HirLifecycleHookKind::Exit)
                .map(|_| mangle_actor_exit_handler(&actor_symbol_base(actor))),
            on_down_symbol: actor
                .lifecycle_hooks
                .iter()
                .find(|hook| hook.kind == HirLifecycleHookKind::Down)
                .map(|_| mangle_actor_down_handler(&actor_symbol_base(actor))),
            max_heap_bytes: actor.max_heap_bytes,
            cycle_capable: actor.cycle_capable,
            mailbox_capacity: actor.mailbox_capacity,
            overflow_policy: actor.overflow_policy.clone(),
            coalesce_key_plan,
            handlers,
            state_clone_fn_symbol: clone_sym,
            state_drop_fn_symbol: drop_sym,
            state_field_clone_kinds: clone_kinds,
        });
    }

    let actor_layout_map: HashMap<String, ActorLayout> = actor_layouts
        .iter()
        .cloned()
        .map(|layout| (layout.name.clone(), layout))
        .collect();

    // Post-loop pass: populate on_crash_symbol, max_heap_bytes, cycle_capable,
    // init_state_fields, and nested_bootstrap_symbol on each
    // SupervisorChildLayout using the now-complete actor_layout_map.
    // build_supervisor_layout runs inside the single-pass item loop, so actor
    // declarations that follow a supervisor in source order would not have been
    // visible yet. Deferring the lookup here makes ordering irrelevant.
    //
    // The set of declared supervisor names lets the loop distinguish a
    // nested-supervisor child (`child api: AuthSupervisor;`) from an actor
    // child: a child whose declared type names another supervisor is registered
    // through the child-supervisor seam, not the actor `HewChildSpec` path.
    let supervisor_names: std::collections::HashSet<String> =
        supervisor_layouts.iter().map(|l| l.name.clone()).collect();
    for sup_layout in &mut supervisor_layouts {
        for child in &mut sup_layout.children {
            // A nested-supervisor child carries the child supervisor's bootstrap
            // symbol; codegen routes it through
            // `hew_supervisor_add_child_supervisor_with_init` instead of the
            // actor `HewChildSpec` path. Mark it before the actor-layout lookups
            // below — those all resolve to None/empty for a supervisor type, so
            // the remaining per-child fields stay correctly null for a nested
            // child.
            if supervisor_names.contains(&child.actor_name) {
                child.nested_bootstrap_symbol =
                    Some(mangle_supervisor_bootstrap(&child.actor_name));
            }
            let al = actor_layout_map.get(&child.actor_name);
            child.on_crash_symbol = al.and_then(|al| al.on_crash_symbol.clone());
            // Lifecycle wrapper: emit a wrapper symbol only when the actor has
            // an `init` or a `#[on(start)]` hook to run. An actor with neither
            // needs no supervised-spawn lifecycle call (matching direct-spawn's
            // no-hook early-return), so the field stays None and codegen leaves
            // the HewChildSpec.lifecycle_fn pointer null.
            child.lifecycle_symbol = al.and_then(|al| {
                if al.init_symbol.is_some() || al.on_start_symbol.is_some() {
                    Some(mangle_actor_lifecycle_wrapper(&child.actor_name))
                } else {
                    None
                }
            });
            child.max_heap_bytes = al.and_then(|al| al.max_heap_bytes);
            child.cycle_capable = al.is_some_and(|al| al.cycle_capable);
            child.mailbox_capacity = al.and_then(|al| al.mailbox_capacity);
            child.overflow_policy = al.and_then(|al| al.overflow_policy.clone());

            // Build the complete init_state_fields plan for this child's actor.
            //
            // Strategy: for every state field on the actor (in declaration order),
            // apply the priority chain:
            //   1. explicit named arg from the child declaration
            //   2. declared field default (from the actor's `state_field_defaults`)
            //   3. fail-closed diagnostic — required field not supplied
            //
            // This mirrors the canonical `lower_spawn_actor_state` chain and ensures
            // `init_state_fields` is always the COMPLETE ordered field set.  Codegen's
            // uniform loop then writes every field; no alloca is left unwritten.
            //
            // The previous code only iterated the explicitly-supplied args, leaving
            // defaulted-but-omitted fields as stack garbage in the template alloca.
            // The fix re-converges with the plain-spawn path so both paths implement
            // the same priority chain from one authoritative place (MIR).
            let key = (sup_layout.name.clone(), child.name.clone());
            let explicit_hir_args: &[(String, HirExpr)] = supervisor_child_hir_init_args
                .get(&key)
                .map_or(&[], Vec::as_slice);

            // Validate every explicit init-arg NAME against the actor's
            // accepted field/init surface BEFORE any missing-field planning.
            // This must run first: the field-plan loop below walks every
            // state field unconditionally and reports a field as "missing"
            // whenever no explicit arg matches its name, which is exactly
            // what happens when the arg that WAS meant to supply it has a
            // typo'd name. Running name validation first and skipping the
            // field plan entirely on an invalid name keeps a bad name to
            // exactly one diagnostic (InvalidActorSpawnArgument) instead of
            // that plus a misleading MissingActorSpawnArgument for the field
            // the bad name was presumably meant to supply.
            let mut has_invalid_arg_name = false;
            if let Some(actor_layout) = al {
                let explicit_init = actor_layout.init_symbol.is_some();
                for (arg_name, arg_expr) in explicit_hir_args {
                    let is_valid = if explicit_init {
                        actor_layout.init_param_names.iter().any(|n| n == arg_name)
                            || actor_layout.state_field_names.iter().any(|n| n == arg_name)
                    } else {
                        actor_layout.state_field_names.iter().any(|n| n == arg_name)
                    };
                    if !is_valid {
                        let note = Builder::invalid_spawn_arg_note(
                            &child.actor_name,
                            arg_name,
                            explicit_init,
                            &actor_layout.init_param_names,
                            &actor_layout.state_field_names,
                        );
                        diagnostics.push(MirDiagnostic {
                            kind: MirDiagnosticKind::InvalidActorSpawnArgument {
                                actor: child.actor_name.clone(),
                                argument: arg_name.clone(),
                                site: arg_expr.site,
                            },
                            note,
                        });
                        has_invalid_arg_name = true;
                        break;
                    }
                }
            }

            // Only build the complete field plan when the actor layout is known,
            // has state fields, and every explicit arg name was valid.  Missing
            // actor layout → silently empty (codegen will fail-closed for
            // stateful actors via its existing gate).
            if let Some(actor_layout) = al {
                if !has_invalid_arg_name && !actor_layout.state_field_names.is_empty() {
                    // Index explicit args by field name for O(1) lookup.
                    let explicit_by_name: HashMap<&str, &HirExpr> = explicit_hir_args
                        .iter()
                        .map(|(name, expr)| (name.as_str(), expr))
                        .collect();

                    let mut init_fields: Vec<(String, crate::model::ChildInitArg)> = Vec::new();
                    let mut all_ok = true;

                    'fields: for (field_idx, field_name) in
                        actor_layout.state_field_names.iter().enumerate()
                    {
                        let field_ty = actor_layout.state_field_tys.get(field_idx);

                        // Resolve the source expression: explicit arg wins over
                        // declared default.
                        let source_expr: &HirExpr =
                            if let Some(expr) = explicit_by_name.get(field_name.as_str()) {
                                expr
                            } else if let Some(default_expr) = actor_layout
                                .state_field_defaults
                                .get(field_idx)
                                .and_then(Option::as_ref)
                            {
                                default_expr
                            } else {
                                // Required field not supplied and no declared default →
                                // fail-closed diagnostic. `child.site` is the real
                                // per-child-declaration site registered in
                                // `hew_hir::verify::collect_site_spans` — never a
                                // sentinel — so the rendered caret lands on the
                                // child declaration.
                                diagnostics.push(MirDiagnostic {
                                    kind: MirDiagnosticKind::MissingActorSpawnArgument {
                                        actor: child.actor_name.clone(),
                                        field: field_name.clone(),
                                        site: child.site,
                                    },
                                    note: format!(
                                        "supply a value: `child {}: {}({field_name}: <value>)`",
                                        child.name, child.actor_name
                                    ),
                                });
                                all_ok = false;
                                continue 'fields;
                            };

                        // Lower the resolved expression to a ChildInitArg,
                        // respecting the declared field width.
                        //
                        // Resolve the declared field type — required to choose the
                        // correct ChildInitArg width for integer literals.  Without
                        // this, every integer would be emitted as I64, causing an
                        // 8-byte store into a sub-i64 field slot that clobbers the
                        // adjacent field and writes past the end of the struct.
                        let init_arg = match &source_expr.kind {
                            HirExprKind::Literal(HirLiteral::Integer(n)) => {
                                // Materialise the literal at the declared field's
                                // exact width. Each integer width gets its own
                                // `ChildInitArg` variant so codegen emits a store
                                // of exactly `sizeof(field_ty)` bytes — a wider
                                // store would clobber the adjacent field and run
                                // past the end of the state template. The literal
                                // is range-checked against the field's bounds so an
                                // out-of-range constant is a compile error rather
                                // than a silent truncation. An unresolved field type
                                // (`None`) falls back to I64, preserving the prior
                                // behaviour when the field type is unavailable.
                                //
                                // The literal carrier is `i64`, so a negative value
                                // can never satisfy an unsigned field's `try_from`,
                                // and a value beyond `i64::MAX` cannot be expressed
                                // as a literal at all; `u64` therefore widens from
                                // the non-negative `i64` range only.
                                let overflow = |target: &str| MirDiagnostic {
                                    kind: MirDiagnosticKind::NotYetImplemented {
                                        construct: format!(
                                            "supervisor `{}` child `{}` field `{field_name}` \
                                             value {n} does not fit in `{target}`",
                                            sup_layout.name, child.name
                                        ),
                                        site: source_expr.site,
                                    },
                                    note: "integer literal must fit in the declared field type"
                                        .to_string(),
                                };
                                match field_ty {
                                    Some(ResolvedTy::I8) => {
                                        let Ok(v) = i8::try_from(*n) else {
                                            diagnostics.push(overflow("i8"));
                                            all_ok = false;
                                            continue 'fields;
                                        };
                                        crate::model::ChildInitArg::I8(v)
                                    }
                                    Some(ResolvedTy::I16) => {
                                        let Ok(v) = i16::try_from(*n) else {
                                            diagnostics.push(overflow("i16"));
                                            all_ok = false;
                                            continue 'fields;
                                        };
                                        crate::model::ChildInitArg::I16(v)
                                    }
                                    Some(ResolvedTy::I32) => {
                                        let Ok(v) = i32::try_from(*n) else {
                                            diagnostics.push(overflow("i32"));
                                            all_ok = false;
                                            continue 'fields;
                                        };
                                        crate::model::ChildInitArg::I32(v)
                                    }
                                    Some(ResolvedTy::U8) => {
                                        let Ok(v) = u8::try_from(*n) else {
                                            diagnostics.push(overflow("u8"));
                                            all_ok = false;
                                            continue 'fields;
                                        };
                                        crate::model::ChildInitArg::U8(v)
                                    }
                                    Some(ResolvedTy::U16) => {
                                        let Ok(v) = u16::try_from(*n) else {
                                            diagnostics.push(overflow("u16"));
                                            all_ok = false;
                                            continue 'fields;
                                        };
                                        crate::model::ChildInitArg::U16(v)
                                    }
                                    Some(ResolvedTy::U32) => {
                                        let Ok(v) = u32::try_from(*n) else {
                                            diagnostics.push(overflow("u32"));
                                            all_ok = false;
                                            continue 'fields;
                                        };
                                        crate::model::ChildInitArg::U32(v)
                                    }
                                    Some(ResolvedTy::U64) => {
                                        let Ok(v) = u64::try_from(*n) else {
                                            diagnostics.push(overflow("u64"));
                                            all_ok = false;
                                            continue 'fields;
                                        };
                                        crate::model::ChildInitArg::U64(v)
                                    }
                                    // 64-bit signed (or unresolved — fall back to
                                    // I64 preserving the original behaviour when
                                    // the field type is unavailable).
                                    Some(ResolvedTy::I64) | None => {
                                        crate::model::ChildInitArg::I64(*n)
                                    }
                                    // Non-integer field types are handled by the
                                    // bool/float arms below or rejected by the
                                    // checker before reaching MIR; an integer
                                    // literal against a non-integer field is a
                                    // type error the checker already caught, so
                                    // fail closed here.
                                    Some(other_ty) => {
                                        diagnostics.push(MirDiagnostic {
                                            kind: MirDiagnosticKind::NotYetImplemented {
                                                construct: format!(
                                                    "supervisor `{}` child `{}` field \
                                                     `{field_name}` has integer literal value \
                                                     {n} for non-integer type `{other_ty}`",
                                                    sup_layout.name, child.name
                                                ),
                                                site: source_expr.site,
                                            },
                                            note: "integer literal supplied for a non-integer \
                                                   child init field"
                                                .to_string(),
                                        });
                                        all_ok = false;
                                        continue 'fields;
                                    }
                                }
                            }
                            HirExprKind::Literal(HirLiteral::Bool(b)) => {
                                crate::model::ChildInitArg::Bool(*b)
                            }
                            HirExprKind::Literal(HirLiteral::Float(f)) => {
                                crate::model::ChildInitArg::F64(*f)
                            }
                            // The v0.6 init-closure restart model: a child init
                            // arg may read the supervisor's construction-time
                            // config (`config.<field>`). The codegen-emitted init
                            // thunk loads (scalar) or deep-clones (owned) the
                            // config field into the fresh actor state, re-run on
                            // every restart. Match `FieldAccess` on a config-param
                            // binding and record a `ConfigField` ChildInitArg.
                            HirExprKind::FieldAccess { object, field } => {
                                if let HirExprKind::BindingRef {
                                    name: config_param_name,
                                    ..
                                } = &object.kind
                                {
                                    // The object's resolved type names the config
                                    // struct (for record-layout lookup at codegen).
                                    let ResolvedTy::Named {
                                        name: config_ty_name,
                                        ..
                                    } = &object.ty
                                    else {
                                        diagnostics.push(MirDiagnostic {
                                            kind: MirDiagnosticKind::NotYetImplemented {
                                                construct: format!(
                                                    "supervisor `{}` child `{}` field \
                                                     `{field_name}` reads `{config_param_name}.\
                                                     {field}` but the config binding is not a \
                                                     named struct type ({:?})",
                                                    sup_layout.name, child.name, object.ty
                                                ),
                                                site: source_expr.site,
                                            },
                                            note: "supervisor config init args read a field \
                                                   of a named config struct parameter"
                                                .to_string(),
                                        });
                                        all_ok = false;
                                        continue 'fields;
                                    };
                                    let config_ty_name = config_ty_name.clone();
                                    let owned = self::expr::binding_seeds_drop_elaboration(
                                        &source_expr.ty,
                                        &module.type_classes,
                                    );
                                    // #2238 item 2: keep the MIR self-consistent
                                    // with the checker wall
                                    // (`ty_is_supervisor_init_reproducible`) and
                                    // the codegen init thunk
                                    // (`emit_owned_config_field_clone`). The
                                    // checker admits owned supervised init args
                                    // for `string`/`bytes` ONLY (the two types
                                    // the thunk has a clone symbol for); owned
                                    // collections are rejected before MIR. An
                                    // owned `ConfigField` that is neither
                                    // `String` nor `Bytes` means the checker wall
                                    // regressed and codegen would fail closed —
                                    // assert the invariant here so a wall
                                    // regression is a named MIR failure rather
                                    // than an ordering-dependent codegen error.
                                    debug_assert!(
                                        !owned
                                            || matches!(
                                                &source_expr.ty,
                                                ResolvedTy::String | ResolvedTy::Bytes
                                            ),
                                        "supervisor `{}` child `{}` owned config-init field \
                                         `{field_name}` has type {:?}; the checker wall admits \
                                         owned supervised init args for `string`/`bytes` only",
                                        sup_layout.name,
                                        child.name,
                                        source_expr.ty
                                    );
                                    // Non-BitCopy config-field init
                                    // lowers to a `ConfigField { owned: true }`:
                                    // the codegen init thunk deep-clones the
                                    // config field per incarnation
                                    // (`emit_owned_config_field_clone`), so a
                                    // restart produces fresh, unaliased owned
                                    // state. Scalar fields keep `owned: false`
                                    // (a plain load). The checker admits owned
                                    // config init for clone-able, non-resource
                                    // types; this records the discriminator the
                                    // thunk reads.
                                    crate::model::ChildInitArg::ConfigField {
                                        config_param_name: config_param_name.clone(),
                                        config_ty_name,
                                        field_name: field.clone(),
                                        field_ty: source_expr.ty.clone(),
                                        owned,
                                    }
                                } else {
                                    diagnostics.push(MirDiagnostic {
                                        kind: MirDiagnosticKind::NotYetImplemented {
                                            construct: format!(
                                                "supervisor `{}` child `{}` field `{field_name}` \
                                                 reads a field of a non-binding expression; only \
                                                 `config.<field>` config reads are supported",
                                                sup_layout.name, child.name
                                            ),
                                            site: source_expr.site,
                                        },
                                        note: "supervisor config init args read a field of a \
                                               config struct parameter (`config.field`)"
                                            .to_string(),
                                    });
                                    all_ok = false;
                                    continue 'fields;
                                }
                            }
                            other => {
                                diagnostics.push(MirDiagnostic {
                                    kind: MirDiagnosticKind::NotYetImplemented {
                                        construct: format!(
                                            "supervisor `{}` child `{}` field `{field_name}` \
                                             resolves to a non-literal expression ({other:?}); \
                                             supported child init values are literals and \
                                             config-field reads (`config.<field>`)",
                                            sup_layout.name, child.name
                                        ),
                                        site: source_expr.site,
                                    },
                                    note: "use a literal or a config-field read (`config.field`)"
                                        .to_string(),
                                });
                                all_ok = false;
                                continue 'fields;
                            }
                        };
                        init_fields.push((field_name.clone(), init_arg));
                    }

                    // Only assign the complete plan when every field resolved
                    // cleanly; partial plans would silently omit fields.
                    if all_ok {
                        child.init_state_fields = init_fields;
                    }
                }
            }

            // Lower the pool child's reserved `count:` expr to a `PoolCount`.
            // A literal count is a compile-time constant; a `config.field` count
            // is loaded from the config buffer in the bootstrap. Mirrors the
            // ConfigField recognition for init args above.
            if child.is_pool {
                let key = (sup_layout.name.clone(), child.name.clone());
                if let Some(count_expr) = supervisor_child_hir_pool_count.get(&key) {
                    match &count_expr.kind {
                        HirExprKind::Literal(HirLiteral::Integer(n)) => {
                            child.pool_count = Some(crate::model::PoolCount::Literal(*n));
                        }
                        HirExprKind::FieldAccess { object, field } => {
                            if let HirExprKind::BindingRef {
                                name: config_param_name,
                                ..
                            } = &object.kind
                            {
                                if let ResolvedTy::Named {
                                    name: config_ty_name,
                                    ..
                                } = &object.ty
                                {
                                    child.pool_count = Some(crate::model::PoolCount::ConfigField {
                                        config_param_name: config_param_name.clone(),
                                        config_ty_name: config_ty_name.clone(),
                                        field_name: field.clone(),
                                        field_ty: count_expr.ty.clone(),
                                    });
                                } else {
                                    diagnostics.push(MirDiagnostic {
                                        kind: MirDiagnosticKind::NotYetImplemented {
                                            construct: format!(
                                                "supervisor `{}` pool `{}` count reads \
                                                 `{config_param_name}.{field}` but the config \
                                                 binding is not a named struct type",
                                                sup_layout.name, child.name
                                            ),
                                            site: count_expr.site,
                                        },
                                        note: "a pool count config read names a field of a \
                                               named config struct parameter"
                                            .to_string(),
                                    });
                                }
                            } else {
                                diagnostics.push(MirDiagnostic {
                                    kind: MirDiagnosticKind::NotYetImplemented {
                                        construct: format!(
                                            "supervisor `{}` pool `{}` count reads a field of a \
                                             non-binding expression; only a literal or \
                                             `config.<field>` is supported",
                                            sup_layout.name, child.name
                                        ),
                                        site: count_expr.site,
                                    },
                                    note: "use a literal `count: N` or `count: config.field`"
                                        .to_string(),
                                });
                            }
                        }
                        other => {
                            diagnostics.push(MirDiagnostic {
                                kind: MirDiagnosticKind::NotYetImplemented {
                                    construct: format!(
                                        "supervisor `{}` pool `{}` count is a non-literal, \
                                         non-config expression ({other:?}); supported pool \
                                         counts are an integer literal or `config.<field>`",
                                        sup_layout.name, child.name
                                    ),
                                    site: count_expr.site,
                                },
                                note: "use a literal `count: N` or `count: config.field`"
                                    .to_string(),
                            });
                        }
                    }
                }
            }
        }
    }

    let supervisor_layout_map: HashMap<String, crate::model::SupervisorLayout> = supervisor_layouts
        .iter()
        .cloned()
        .map(|layout| (layout.name.clone(), layout))
        .collect();
    let task_entry_adapter_symbols = TaskEntryAdapterSymbols::default();

    // Exact tagged-union layout keys.  A `ResolvedTy::Named` derives the same
    // key from its full owner plus concrete argument spine before consulting
    // this set; it is never matched by final path segment.
    // Machines ONLY. `ty_is_machine` must match real machines and never an
    // inline user enum (which DOES enter the owned call-carrier protocol), so
    // it consults this set instead of the combined `machine_layout_names`
    // below. Derived from the same `machine_layouts` before the user-enum keys
    // are chained in.
    let machine_decl_layout_names: HashSet<String> = machine_layouts
        .iter()
        .flat_map(|layout| [layout.name.clone(), layout.event_name.clone()])
        .collect();
    let machine_layout_names: HashSet<String> = machine_layouts
        .iter()
        .flat_map(|layout| [layout.name.clone(), layout.event_name.clone()])
        // Ordinary enums use the same class-tagged key family for readiness
        // classification even though their physical descriptors remain in
        // `enum_layouts` under codegen's enum-layout symbols.
        .chain(enum_classification_keys)
        .collect();
    debug_assert!(machine_layout_names
        .iter()
        .all(|key| key.starts_with("mc$$")));

    // Collect the names every user-defined function will use as its
    // emitted MIR symbol. For non-generic functions this is the
    // source-declared name. For generic functions this is the set of
    // mangled per-monomorphisation names; the unspecialised generic
    // body is not emitted (it would carry symbolic type-parameter
    // names through to LLVM and could not be linked).
    //
    // The Call lowering arm dispatches on this set: a callee name in
    // this set → `Terminator::Call`; otherwise the runtime-ABI/indirect
    // fail-closed paths apply.
    let mut module_fn_names: HashSet<String> = HashSet::new();
    for entry in stdlib_catalog::entries() {
        if !matches!(
            entry.linkage,
            stdlib_catalog::BuiltinLinkage::CompilerIntrinsic { .. }
                | stdlib_catalog::BuiltinLinkage::LayoutDescriptorSymbol { .. }
        ) {
            module_fn_names.insert(entry.name.to_string());
            if let Some(symbol) = entry.linkage.runtime_symbol() {
                module_fn_names.insert(symbol.to_string());
            }
        }
    }
    for item in &module.items {
        if let HirItem::Function(f) = item {
            if f.type_params.is_empty() {
                module_fn_names.insert(f.name.clone());
            }
        }
        if let HirItem::ExternFn(ef) = item {
            // Extern fns participate in direct-call dispatch: `Expr::Call` of
            // an extern symbol lowers through the `Terminator::Call` arm,
            // not the runtime-ABI fail-closed path.
            module_fn_names.insert(ef.name.clone());
        }
    }
    for mono in &module.monomorphisations {
        module_fn_names.insert(mono.mangled_name.clone());
    }
    // Channel/stream layout-witness recv/send symbols: registered by the
    // checker (`registration.rs::register_channel_recv_builtins`) and in
    // `fn_registry` (via `seed_stdlib_fn_registry`) but absent from the
    // stdlib catalog and from `extern "C"` blocks in `.hew` source (their
    // real ABI carries an out-parameter and/or an element-layout witness
    // pointer). Adding them here routes a
    // `BindingRef { name: "hew_channel_recv_layout", resolved: Builtin(family) }`
    // call through `lower_direct_call` → `Terminator::Call` (not through
    // `lower_runtime_call` / `Instr::CallRuntimeAbi`, which would require
    // these to be in `runtime_symbols::known_runtime_symbols`).
    // Codegen intercepts the `Terminator::Call` by callee name and emits
    // the element-layout-witness ABI.
    for name in [
        "hew_channel_recv_layout",
        "hew_channel_try_recv_layout",
        "hew_channel_send_layout",
        "hew_stream_next_layout",
        "hew_stream_try_next_layout",
        "hew_stream_send_layout",
    ] {
        module_fn_names.insert(name.to_string());
    }
    let module_generic_fn_names: HashSet<String> = module
        .items
        .iter()
        .filter_map(|item| match item {
            HirItem::Function(func) if !func.type_params.is_empty() => Some(func.name.clone()),
            _ => None,
        })
        .collect();
    // Structured static-trait-dispatch registry. Built once from
    // `HirItem::Impl` metadata and threaded into every user-fn builder.
    let trait_impl_index = hew_hir::dispatch::build_trait_impl_method_index(&module.items);
    let direct_call_symbols = hew_hir::dispatch::build_direct_call_symbol_index(&module.items);
    let mut emitted_actor_handler_symbols: HashMap<String, String> = module_fn_names
        .iter()
        .map(|name| (name.clone(), format!("function `{name}`")))
        .collect();

    // Build origin lookup: ItemId → &HirFn. Each monomorphisation's
    // `key.origin` resolves to the generic origin fn whose body is
    // lowered under per-monomorphisation substitution.
    let mut origin_fns: HashMap<hew_hir::ItemId, &HirFn> = HashMap::new();
    for item in &module.items {
        if let HirItem::Function(f) = item {
            origin_fns.insert(f.id, f);
        }
    }

    // Module-global interprocedural COARSE freshness summary, computed ONCE over
    // every function item (a least-fixpoint). It answers the narrow
    // may-alias-a-by-value-parameter question and CANNOT see an extern, so it is
    // deliberately a LOCAL: its only consumer is the authority builder below.
    // Nothing downstream — no builder, no consumer — is ever handed this map,
    // which is what makes the opaque-extern taint unbypassable.
    let coarse_fn_returns_fresh: HashMap<hew_hir::ItemId, bool> =
        compute_fn_returns_fresh_owner(&origin_fns);
    // Module-global call-scrutinee return-provenance context (#2648): the precise
    // three-state provenance fixpoint (+ the interprocedural mutation summary),
    // the declared-extern id set, the audited extern contract table, and the
    // table-aware fresh-owner authority. Computed ONCE and threaded (as `Rc`)
    // into every body-lowering builder so the preflight admission classifier
    // rejects a forwarded borrowed-parameter scrutinee before the from-call owner
    // mint fires.
    let call_scrutinee_provenance: Rc<crate::return_provenance::CallScrutineeProvenance> =
        Rc::new(crate::return_provenance::build_call_scrutinee_provenance(
            module,
            &origin_fns,
            &coarse_fn_returns_fresh,
        ));
    // Module-global RAII-2 (#1295) param-ownership facts: which affine
    // `#[resource]` free-fn value params are CONSUME vs BORROW, and the
    // call-arg `SiteId`s whose over-stamped `Consume` intent is downgraded to a
    // borrowing `Read`. Computed ONCE over every function item (a monotone
    // least-fixpoint, keyed by origin `ItemId` so monomorphisations share one
    // verdict) and threaded into every body-lowering builder. `Rc` so child
    // builders share it without re-cloning.
    let mut param_ownership = compute_param_ownership(
        &origin_fns,
        &module.items,
        &module.type_classes,
        &module.caller_visible_param_projections,
    );
    param_ownership
        .produced_value_facts
        .clone_from(&module.produced_value_facts);
    // Machine layout keys only exist post-lowering, so inject the machines-only
    // name authority here (not in the HIR-scoped fact pass). `ty_is_machine`
    // reads it via the Rc-shared facts to exclude real machines — never inline
    // user enums — from the owned call-carrier protocol.
    param_ownership
        .machine_decl_layout_names
        .clone_from(&machine_decl_layout_names);
    let param_ownership: Rc<ParamOwnershipFacts> = Rc::new(param_ownership);
    for item in &module.items {
        match item {
            HirItem::Function(func) => {
                // Generic origins are not monomorphic and never reach
                // codegen directly — their concrete instances are emitted
                // via the monomorphisations loop below. W5.007a additionally
                // lowers the origin body ONCE against abstract
                // `ResolvedTy::TypeParam` operands to populate the
                // representation-substrate `polymorphic_mir` bucket. This is
                // strictly additive: only the raw MIR is captured, and any
                // diagnostics produced while lowering the abstract body are
                // discarded so programs that compile today are unaffected.
                if !func.type_params.is_empty() {
                    if module
                        .diagnostic_source_modules
                        .get(&func.id)
                        .is_some_and(|source| source == "std.builtins")
                    {
                        continue;
                    }
                    let abstract_subst: HashMap<String, ResolvedTy> = func
                        .type_params
                        .iter()
                        .map(|name| (name.clone(), ResolvedTy::TypeParam { name: name.clone() }))
                        .collect();
                    let lowered = lower_function(
                        func,
                        func.name.clone(),
                        abstract_subst,
                        &module.type_classes,
                        &record_field_orders,
                        &actor_layout_map,
                        &supervisor_layout_map,
                        &machine_layout_names,
                        &classification_enum_layouts,
                        &opaque_handle_names,
                        None,
                        &module_fn_names,
                        &module_generic_fn_names,
                        &call_scrutinee_provenance,
                        &param_ownership,
                        &trait_impl_index,
                        &direct_call_symbols,
                        &module.call_site_type_args,
                        Some(&module.vec_generic_element_abi),
                        &module.supervisor_child_slots,
                        &module.pool_accessor_sites,
                        pointer_width,
                        crate::model::FunctionCallConv::Default,
                        task_entry_adapter_symbols.clone(),
                    );
                    // F3 fail-closed guard: an abstract origin declares every
                    // type parameter it can mention, so the witness verifier
                    // must find no out-of-scope `TypeParam` operand. Any
                    // `WitnessOperandUnresolved` here is a lowering bug of the
                    // exact A622/DI-019 class (a `TypeParam` left un-scoped
                    // under a composite). The bucket otherwise discards
                    // diagnostics, so assert on the checks directly rather
                    // than surfacing a user-facing error for this gated,
                    // non-codegen body.
                    debug_assert!(
                        !lowered
                            .checked
                            .checks
                            .iter()
                            .any(|c| matches!(c, MirCheck::WitnessOperandUnresolved { .. })),
                        "abstract generic origin `{}` produced an unresolved \
                         witness operand: {:?}",
                        func.name,
                        lowered.checked.checks
                    );
                    polymorphic_mir.push(crate::model::PolymorphicMirFunction {
                        type_params: func.type_params.clone(),
                        raw: lowered.raw,
                    });
                    continue;
                }
                let mut lowered = lower_function(
                    func,
                    func.name.clone(),
                    HashMap::new(),
                    &module.type_classes,
                    &record_field_orders,
                    &actor_layout_map,
                    &supervisor_layout_map,
                    &machine_layout_names,
                    &classification_enum_layouts,
                    &opaque_handle_names,
                    None,
                    &module_fn_names,
                    &module_generic_fn_names,
                    &call_scrutinee_provenance,
                    &param_ownership,
                    &trait_impl_index,
                    &direct_call_symbols,
                    &module.call_site_type_args,
                    Some(&module.vec_generic_element_abi),
                    &module.supervisor_child_slots,
                    &module.pool_accessor_sites,
                    pointer_width,
                    crate::model::FunctionCallConv::Default,
                    task_entry_adapter_symbols.clone(),
                );
                thir.push(lowered.thir);
                lowered.raw.source_origin = resolve_source_origin(func.id, module);
                raw_mir.push(lowered.raw);
                checked_mir.push(lowered.checked);
                elaborated_mir.push(lowered.elaborated);
                record_layouts.extend(lowered.record_layouts);
                flatten_generated_functions(
                    lowered.generated,
                    &mut thir,
                    &mut raw_mir,
                    &mut checked_mir,
                    &mut elaborated_mir,
                    &mut diagnostics,
                    &mut record_layouts,
                    &mut emitted_named_fn_shims,
                );
                diagnostics.extend(lowered.diagnostics);
            }
            HirItem::Actor(actor) => {
                let lowered_handlers = lower_actor_body_handlers(
                    actor,
                    &module.type_classes,
                    &record_field_orders,
                    &actor_layout_map,
                    &supervisor_layout_map,
                    &machine_layout_names,
                    &classification_enum_layouts,
                    &opaque_handle_names,
                    &module_fn_names,
                    &module_generic_fn_names,
                    &direct_call_symbols,
                    &call_scrutinee_provenance,
                    &param_ownership,
                    &module.call_site_type_args,
                    &module.supervisor_child_slots,
                    &module.pool_accessor_sites,
                    pointer_width,
                    &mut emitted_actor_handler_symbols,
                    &task_entry_adapter_symbols,
                    &mut diagnostics,
                );
                for lowered in lowered_handlers {
                    thir.push(lowered.thir);
                    raw_mir.push(lowered.raw);
                    checked_mir.push(lowered.checked);
                    elaborated_mir.push(lowered.elaborated);
                    record_layouts.extend(lowered.record_layouts);
                    flatten_generated_functions(
                        lowered.generated,
                        &mut thir,
                        &mut raw_mir,
                        &mut checked_mir,
                        &mut elaborated_mir,
                        &mut diagnostics,
                        &mut record_layouts,
                        &mut emitted_named_fn_shims,
                    );
                    diagnostics.extend(lowered.diagnostics);
                }
            }
            HirItem::Supervisor(sup) => {
                if let Some(lowered) = lower_supervisor_bootstrap(
                    sup,
                    &supervisor_layout_map,
                    &module.type_classes,
                    &record_field_orders,
                    &actor_layout_map,
                    &machine_layout_names,
                    &classification_enum_layouts,
                    &opaque_handle_names,
                    &module_fn_names,
                    &module_generic_fn_names,
                    &direct_call_symbols,
                    &call_scrutinee_provenance,
                    &param_ownership,
                    &module.call_site_type_args,
                    &module.supervisor_child_slots,
                    &module.pool_accessor_sites,
                    pointer_width,
                    &mut emitted_actor_handler_symbols,
                    &task_entry_adapter_symbols,
                    &mut diagnostics,
                ) {
                    thir.push(lowered.thir);
                    raw_mir.push(lowered.raw);
                    checked_mir.push(lowered.checked);
                    elaborated_mir.push(lowered.elaborated);
                    record_layouts.extend(lowered.record_layouts);
                    flatten_generated_functions(
                        lowered.generated,
                        &mut thir,
                        &mut raw_mir,
                        &mut checked_mir,
                        &mut elaborated_mir,
                        &mut diagnostics,
                        &mut record_layouts,
                        &mut emitted_named_fn_shims,
                    );
                    // `lowered.diagnostics` carries the synthetic body's own
                    // Builder-accumulated diagnostics (from lowering the
                    // per-child `spawn ChildActor(...)` statements). A
                    // missing required field on a NON-config child is
                    // authoritatively diagnosed once already, above, by the
                    // post-loop pass that builds `init_state_fields` (it
                    // walks every state field unconditionally, so it always
                    // fires first for the identical (actor, field) pair the
                    // synthetic spawn would also flag). Strip the redundant
                    // copy here rather than teaching the shared
                    // `lower_spawn_actor_state_arg` helper about a
                    // supervisor-synthetic-body special case that would also
                    // suppress the diagnostic real direct-spawn users need.
                    diagnostics.extend(lowered.diagnostics.into_iter().filter(|d| {
                        !matches!(d.kind, MirDiagnosticKind::MissingActorSpawnArgument { .. })
                    }));
                }
            }
            HirItem::Record(_)
            | HirItem::TypeDecl(_)
            | HirItem::Impl(_)
            | HirItem::ExternFn(_)
            | HirItem::Const(_) => {
                // Type declarations have no executable MIR bodies. TypeDecl
                // markers are consumed via `HirModule.type_classes`.
                //
                // V0b: `HirItem::Impl` is metadata-only — its method bodies
                // are emitted as sibling `HirItem::Function` entries
                // (named `<SelfType>::<method>`) by the HIR lowering pass
                // and are picked up through the `HirItem::Function` arm
                // above. Nothing to do here.
                //
                // Extern fns have no body to lower; they are registered
                // into `module_fn_names` above so `Call` lowering dispatches
                // them as `Terminator::Call`, and the
                // `IrPipeline.extern_decls` table populated below carries
                // the signature to codegen for symbol predeclaration.
                //
                // Module-level consts have no executable body either: the
                // folded value is carried structurally on `HirItem::Const`
                // and the codegen-facing descriptor is built in a later slice.
                // A const *reference* fails closed at `lower_value` until the
                // codegen global-load seam lands.
            }
            HirItem::Machine(md) => {
                // One helper per registered concrete machine instance.  The
                // call producer derives this same instance key from the
                // receiver's resolved type, so no generic helper can silently
                // service a different argument spine or same-leaf owner.
                for entry in module
                    .machine_instantiations
                    .iter()
                    .filter(|entry| entry.key.origin == md.id)
                {
                    let canonical_origin = md.qualified_name();
                    if entry.key.origin_name != canonical_origin {
                        // The earlier layout pass emitted the structural
                        // diagnostic; do not create a guessed helper here.
                        continue;
                    }
                    let layout_name = entry.key.mangle();
                    if !machine_layouts
                        .iter()
                        .any(|layout| layout.name == layout_name)
                    {
                        // The table may be malformed (arity/const mismatch),
                        // in which case the layout pass has already declined
                        // it and a helper would have no LLVM type to target.
                        continue;
                    }
                    let lowered = synthesize_machine_step_fn(
                        md,
                        layout_name,
                        &entry.key.type_args,
                        &module.type_classes,
                        &record_field_orders,
                        &actor_layout_map,
                        &supervisor_layout_map,
                        &machine_layout_names,
                        &classification_enum_layouts,
                        &module_fn_names,
                        &module_generic_fn_names,
                        &call_scrutinee_provenance,
                        &param_ownership,
                        &module.call_site_type_args,
                        &module.supervisor_child_slots,
                        pointer_width,
                    );
                    thir.push(lowered.thir);
                    raw_mir.push(lowered.raw);
                    checked_mir.push(lowered.checked);
                    elaborated_mir.push(lowered.elaborated);
                    diagnostics.extend(lowered.diagnostics);
                }
            }
        }
    }

    // Emit one MIR function per monomorphisation. Each instantiation
    // re-lowers the generic origin's body with a substitution map
    // mapping origin type-parameter symbols to the concrete arg types
    // declared by the MonoKey. The emitted function name is the
    // mangled symbol the registry assigned.
    for mono in &module.monomorphisations {
        let Some(origin) = origin_fns.get(&mono.key.origin).copied() else {
            // Origin fn missing from module.items. The registry was
            // populated against a fn that does not appear here —
            // fail-closed by emitting a diagnostic and skipping.
            diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::UnsupportedNode {
                    reason: format!(
                        "monomorphisation `{}` references unknown origin fn id {:?}",
                        mono.mangled_name, mono.key.origin
                    ),
                },
                note: "the HIR monomorphisation registry referenced an origin \
                       ItemId not present in module.items"
                    .to_string(),
            });
            continue;
        };
        // Substitution map: type-parameter name → concrete ResolvedTy.
        let subst: HashMap<String, ResolvedTy> = origin
            .type_params
            .iter()
            .cloned()
            .zip(mono.key.type_args.iter().cloned())
            .collect();
        let mut lowered = lower_function(
            origin,
            mono.mangled_name.clone(),
            subst,
            &module.type_classes,
            &record_field_orders,
            &actor_layout_map,
            &supervisor_layout_map,
            &machine_layout_names,
            &classification_enum_layouts,
            &opaque_handle_names,
            None,
            &module_fn_names,
            &module_generic_fn_names,
            &call_scrutinee_provenance,
            &param_ownership,
            &trait_impl_index,
            &direct_call_symbols,
            &module.call_site_type_args,
            Some(&module.vec_generic_element_abi),
            &module.supervisor_child_slots,
            &module.pool_accessor_sites,
            pointer_width,
            crate::model::FunctionCallConv::Default,
            task_entry_adapter_symbols.clone(),
        );
        thir.push(lowered.thir);
        lowered.raw.source_origin = resolve_source_origin(mono.key.origin, module);
        raw_mir.push(lowered.raw);
        checked_mir.push(lowered.checked);
        elaborated_mir.push(lowered.elaborated);
        record_layouts.extend(lowered.record_layouts);
        flatten_generated_functions(
            lowered.generated,
            &mut thir,
            &mut raw_mir,
            &mut checked_mir,
            &mut elaborated_mir,
            &mut diagnostics,
            &mut record_layouts,
            &mut emitted_named_fn_shims,
        );
        diagnostics.extend(lowered.diagnostics);
    }

    collect_layout_field_diagnostics(
        &record_layouts,
        &enum_layouts,
        &machine_layouts,
        &LayoutReadiness {
            record_field_orders: &record_field_orders,
            actor_layouts: &actor_layout_map,
            supervisor_layout_map: &supervisor_layout_map,
            machine_layout_names: &machine_layout_names,
            type_classes: &module.type_classes,
        },
        &mut diagnostics,
    );

    // Mirror HirModule::regex_literals into the IrPipeline so codegen can
    // emit module-init globals without re-reading HIR. Each entry's literal_id
    // is the 0-based index into this Vec AND into the global handle array that
    // codegen emits. The pattern has already been validated by the type-checker.
    let regex_literals: Vec<crate::model::RegexLiteral> = module
        .regex_literals
        .iter()
        .map(|rl| crate::model::RegexLiteral {
            literal_id: rl.literal_id,
            pattern: rl.pattern.clone(),
        })
        .collect();

    // Build one `ExternDecl` per HIR `extern` fn, deriving each C-ABI string
    // return's ownership from the extern's carried defining-module provenance —
    // NOT by absence from `diagnostic_source_modules`, which conflates a root
    // user extern with a std extern whose attribution was lost. A wrong
    // classification corrupts memory in either direction, so an unresolvable
    // provenance fails closed with a diagnostic rather than guessing.
    let mut extern_decls: Vec<crate::model::ExternDecl> = Vec::new();
    for item in &module.items {
        let HirItem::ExternFn(ef) = item else {
            continue;
        };
        // Ownership classification only applies to C-ABI `-> string` returns;
        // every other extern keeps `malloc_string_return = false`.
        let malloc_string_return = if ef.abi == "C" && matches!(ef.return_ty, ResolvedTy::String) {
            match crate::model::classify_extern_string_ownership(&ef.provenance, &ef.name) {
                crate::model::ExternStringOwnership::ForeignAdopt => true,
                crate::model::ExternStringOwnership::HeaderAware => false,
                crate::model::ExternStringOwnership::Unresolved => {
                    // Fail closed: refuse to guess a memory-unsafe adoption
                    // direction. The diagnostic gates codegen at the CLI, so
                    // no binary is emitted; the inert `false` here never
                    // reaches a real call edge.
                    diagnostics.push(MirDiagnostic {
                        kind: MirDiagnosticKind::ExternStringOwnershipUnresolved {
                            symbol: ef.name.clone(),
                        },
                        note: format!(
                            "extern \"C\" fn `{}` returns a string but its defining-module \
                                 provenance carries no module identity, so its return ownership \
                                 (header-aware Hew string vs. adopted foreign C string) cannot be \
                                 classified; neither direction is memory safe by default",
                            ef.name
                        ),
                    });
                    false
                }
            }
        } else {
            false
        };
        extern_decls.push(crate::model::ExternDecl {
            name: ef.name.clone(),
            abi: ef.abi.clone(),
            param_tys: ef.param_tys.clone(),
            return_ty: ef.return_ty.clone(),
            provenance: ef.provenance.clone(),
            runtime_capability: ef.runtime_capability,
            malloc_string_return,
        });
    }

    // OPM-1: all ordinary, generated, and monomorphised functions are now
    // present. Refine the initial typed parameter modes through the complete
    // module call graph before any checked-MIR consumer or backend capability
    // snapshot observes them.
    facts::finalize_param_boundary_modes(&mut raw_mir, &mut checked_mir, &mut elaborated_mir);

    // W3.031 Stage 2 — build the deduplicated `dyn Trait` vtable
    // registry from every `Instr::CoerceToDynTrait` reached by any
    // lowered function. This collapses the program's
    // `(trait_name, concrete_type, vtable_entries)` triples into
    // stable `DynVtableInstance` entries with reproducible
    // `vtable_id`s; later codegen stages (drop-in-place synthesis,
    // erased thunk emission, vtable static emission) consume this
    // registry without re-walking MIR. Empty when the module has no
    // `dyn Trait` usage.
    let dyn_vtable_registry = crate::dyn_vtable_registry::build_dyn_vtable_registry(&raw_mir);

    // MIR-stage lint pass (liveness-driven `dead_store`). Records raw,
    // level-agnostic findings; the CLI applies the user's `LintLevels` and
    // `// hew:allow(...)` policy at render time. CLI front end only — the LSP /
    // wasm front ends stop at HIR and never reach MIR (issue #2176).
    //
    // Runs on `raw_mir` (pre drop-elaboration) deliberately: this is sound ONLY
    // because `dead_store` is restricted to no-drop scalars (see
    // `liveness::is_no_drop_scalar`). Widening the lint past scalars would have
    // to run after drop elaboration so a `Drop`'s read of the local is modelled.
    let lint_warnings = crate::liveness::run_mir_lints(&raw_mir);

    // Field-bearing `#[resource]` record → `<Type>::close` symbol registry
    // (spec §3.7.3). A `#[resource]` record that owns a heap/aggregate field
    // is admitted to `owned_record_drop_allowed` and routes to the recursive
    // `__hew_record_drop_inplace_<R>` thunk, which frees heap leaves but would
    // otherwise skip the RAII `close()` contract. Seed the thunk synthesis with
    // each such record's user close symbol so codegen calls `close(self)` as
    // the first step of the record's (and any nested resource field's) drop.
    //
    // Keyed off `record_layouts` so only records with a real layout (the ones
    // the thunk is synthesised for) are listed; a single-handle `#[resource]`
    // with no record layout never reaches the thunk and is excluded. The close
    // method name comes from the SAME `type_classes` entry `resource_drop_fn`
    // reads, and the `<Type>::close` symbol matches `declare_function`'s
    // flattened `<Self>::<method>` mangling, so MIR and codegen agree without
    // translation glue.
    let capabilities = crate::model::ModuleCapabilities::from_raw_mir(&raw_mir, &extern_decls);

    IrPipeline {
        thir,
        raw_mir,
        checked_mir,
        elaborated_mir,
        diagnostics,
        capabilities,
        wire_layouts: module.wire_layouts.clone(),
        opaque_handle_names: module
            .items
            .iter()
            .filter_map(|item| match item {
                // `#[opaque]` runtime handles lower to ptr-sized slots.
                HirItem::TypeDecl(decl) if decl.is_opaque => Some(decl.qualified_name()),
                // `indirect enum` values are heap-allocated; every variable
                // of the type holds a `ptr` to the heap struct.  Adding the
                // name here causes `resolve_ty` in codegen to return `ptr`
                // for all alloca slots, function parameters, and field types
                // that reference this enum — including the self-referential
                // variant payloads that would otherwise produce an oversized
                // inline struct (the root cause of the recursive-enum crash).
                HirItem::TypeDecl(decl) if decl.is_indirect => Some(decl.name.clone()),
                _ => None,
            })
            .collect(),
        record_layouts,
        actor_layouts,
        supervisor_layouts,
        machine_layouts,
        enum_layouts,
        regex_literals,
        user_consts,
        extern_decls,
        dyn_vtable_registry,
        // C-3b: checker-authored layout facts are routed through a
        // separate seam (`attach_lowering_facts`) so `lower_hir_module`
        // — which only consumes HIR — remains a pure structural
        // lowering.  Driver glue (frontend / `hew_compile`) calls
        // `pipeline.attach_lowering_facts(&tco)` between type-check
        // and codegen.  Tests that build pipelines by hand without a
        // TypeCheckOutput leave these empty.
        hashmap_lowering_facts: Vec::new(),
        hashset_lowering_facts: Vec::new(),
        polymorphic_mir,
        // Populated by `attach_lowering_facts` from `TypeCheckOutput`; empty
        // here so the lowerer does not depend on `TypeCheckOutput` directly.
        user_clone_record_seeds: Vec::new(),
        lint_warnings,
        lifecycle_registry,
    }
}

/// Module-global RAII-2 (#1295) param-ownership facts. See
/// [`compute_param_ownership`].
#[derive(Debug, Default)]
pub(crate) struct ParamOwnershipFacts {
    /// Typed ownership facts keyed by the stable HIR producer `SiteId`.  This
    /// travels with the existing module-wide ownership authority so every MIR
    /// Builder consults the checker carrier, never a target symbol spelling.
    produced_value_facts: HashMap<SiteId, HirProducedValueFact>,
    /// `(origin fn ItemId, param index) -> true` iff that affine `#[resource]`
    /// parameter is CONSUMED — callers move it in, the callee owns it and either
    /// drops it at scope exit or forwards it onward. `false` = BORROW — callers
    /// keep ownership and auto-drop at caller scope, and the callee must NOT
    /// drop. Only affine `Named { builtin: None }` resource params ever appear;
    /// non-resource and builtin-handle params are absent.
    param_consume: HashMap<(hew_hir::ItemId, usize), bool>,
    /// `SiteId`s of free-call arguments whose target parameter is a resource
    /// BORROW. At the single `MirStatement::Use` emission point such an arg's
    /// over-stamped `Consume` intent is downgraded to a borrowing `Read`, so the
    /// caller keeps ownership and the resource is dropped exactly once (at the
    /// caller's scope exit) instead of being moved into a borrowing callee.
    borrow_arg_sites: HashSet<hew_hir::SiteId>,
    /// `SiteId`s of direct free-function arguments whose target parameter is
    /// proven borrow-only by the same body scan used for resource parameters.
    ///
    /// This broader set does not change MIR move intent or callee-side drops. It
    /// lets owned collection drop elaboration distinguish a genuine handoff from
    /// a call-boundary borrow such as `first<T>(v: Vec<T>) { v.get(0) }`.
    proven_borrow_arg_sites: HashSet<hew_hir::SiteId>,
    /// `(origin fn ItemId, param index) -> true` iff that parameter (of ANY
    /// type, not just resources) is CONSUMED by the callee body — returned,
    /// stored, sent, captured, forwarded to a consuming param, or destructured
    /// by a `match` scrutinee. The all-parameter body summary that seeds
    /// `proven_borrow_arg_sites`; unlike `param_consume` it covers non-resource
    /// composites (a by-value `Result<string, string>` / user enum), which
    /// `lower_params` needs to decide whether a heap-owning enum-composite param
    /// is the CALLEE's drop obligation (CONSUME) or the caller's (BORROW). A
    /// A BORROW param is `ConsumeVerdict::ProvenBorrow`; a param absent from the
    /// map is treated as BORROW (fail-safe: the caller keeps and drops it, no
    /// callee double-free). The two consume flavours
    /// (`ProvenConsume`/`ConservativeConsume`) both mean "callee owns/drops" and
    /// are byte-identical at every consumer via `is_consume()` — the finer label
    /// records WHY the param flipped (positive escape proof vs the fail-closed
    /// forward-to-unproven disjunct) so a later pass can act on the proven-borrow
    /// tail without re-deriving it.
    call_param_consume: HashMap<(hew_hir::ItemId, usize), ConsumeVerdict>,
    /// A separate caller/callee contract for parameters whose body carries the
    /// value into an owning sink. Callers prepare an independent owner (or
    /// transfer a proven dead whole local); callees install the inverse
    /// structural drop. Method receiver slot zero is excluded so an `Arena`
    /// value receiver retains its established borrowed-self semantics.
    call_param_owned_carrier: HashMap<(hew_hir::ItemId, usize), bool>,
    /// Positive checker/HIR authority that the parameter type exposes
    /// caller-visible storage.
    caller_visible_param_projections: HashSet<(hew_hir::ItemId, usize)>,
    /// Machines-ONLY tagged-union layout keys — every machine layout name plus
    /// its event-companion key, WITHOUT the user-enum keys carried in the
    /// Builder's combined `machine_layout_names`. `ty_is_machine` consults this
    /// so an ordinary inline user enum is never misread as a machine and denied
    /// the owned call-carrier protocol. Populated from `module.machine_layouts`
    /// after `compute_param_ownership` (the HIR pass has no layout keys); empty
    /// until then. Travels on the Rc-shared facts so every Builder — including
    /// the machine-`__step` synth builders — reads the same authority.
    machine_decl_layout_names: HashSet<String>,
}

/// Shared context for the consume-detection and borrow-site walkers. Bundles
/// the in-progress ownership verdict (`consume`) with the method-item set
/// (`methods`) so the `Call` arm can tell an accurate-intent method receiver
/// slot apart from an over-stamped free-call argument. Threaded by reference;
/// `consume` is re-borrowed fresh on each fixpoint pass (it mutates between
/// passes, never during a walk).
struct ScanCtx<'a> {
    consume: &'a HashMap<(hew_hir::ItemId, usize), bool>,
    methods: &'a HashSet<hew_hir::ItemId>,
    /// Symbols declared by a source `extern` block. Only these bodyless ABI
    /// boundaries may preserve HIR's generated-contract `Read` intent for an
    /// affine resource argument; ordinary Hew calls stay fail-closed.
    extern_fn_names: &'a HashSet<String>,
    /// The subset of `methods` whose parameter zero is a true receiver. An
    /// associated/static impl function has no receiver, so its first argument
    /// retains ordinary carrier/consume inference.
    true_receiver_methods: &'a HashSet<hew_hir::ItemId>,
    /// Whether consume-intent projections count as owning carriers of their
    /// root parameter. This is enabled only for the deep call-carrier summary,
    /// never for the legacy borrow/consume table.
    owned_projection_sinks: bool,
    /// Whether a bare-ref argument forwarded to a free-function parameter is
    /// treated OPTIMISTICALLY as a borrow, regardless of the target's verdict.
    /// The normal scan flips such a forward to CONSUME when the target is
    /// consuming or unproven (the fail-closed disjunct); this flag suppresses
    /// exactly that disjunct so the scan reports ONLY the positively-proven
    /// escapes (returned/stored/sent/captured). Used solely to split a converged
    /// consume verdict into `ProvenConsume` vs `ConservativeConsume` (see
    /// `refine_call_param_verdicts`); never enabled for the consume/borrow or
    /// carrier tables that drive move intent.
    assume_forward_borrows: bool,
    /// The subset of `methods` whose PARAM 0 is a by-value `self` receiver of a
    /// NON-resource type (`collect_borrow_receiver_methods`). Shape A of #2753:
    /// the borrow-site collector records such a receiver's `SiteId` so the
    /// caller keeps the value-receiver record/collection's scope-exit drop.
    /// Empty of effect except in the `proven_borrow_arg_sites` walk (a
    /// non-resource composite receiver is absent from the resource-only
    /// `param_consume` map, so the borrow-pass gate never fires for it).
    receiver_methods: &'a HashSet<hew_hir::ItemId>,
}

#[derive(Debug)]
pub(crate) struct LoweredFunction {
    thir: ThirFunction,
    raw: RawMirFunction,
    checked: CheckedMirFunction,
    elaborated: ElaboratedMirFunction,
    diagnostics: Vec<MirDiagnostic>,
    generated: Vec<LoweredFunction>,
    record_layouts: Vec<crate::model::RecordLayout>,
}

/// Recursively drain a `LoweredFunction::generated` tree into the module
/// output vectors.
///
/// A generated function (a closure invoke shim, a named-fn reference shim, an
/// ask-reply thunk, …) can itself produce generated functions: a *nested*
/// closure literal's invoke shim is lowered while the *parent* shim is being
/// built, so it lands in the parent's `builder.generated_functions` — i.e.
/// `generated.generated`, depth 2. A single-level `for g in lowered.generated`
/// loop (the historical shape at every dispatch site) surfaces only depth 1,
/// so a `MakeClosure` for the inner literal references a shim symbol codegen
/// never emitted (`E_NOT_YET_IMPLEMENTED: missing closure invoke shim`).
///
/// Walk the whole tree so EVERY generated function at EVERY depth is surfaced
/// exactly once (`exhaustive-coverage`). The `__hew_named_fn_invoke_` dedup is
/// preserved: a duplicate shim (and the subtree below it, which is the same
/// already-emitted bodies) is skipped wholesale.
#[allow(
    clippy::too_many_arguments,
    reason = "threads the same module-output sinks the four dispatch sites already hold as locals"
)]
fn flatten_generated_functions(
    generated: Vec<LoweredFunction>,
    thir: &mut Vec<ThirFunction>,
    raw_mir: &mut Vec<RawMirFunction>,
    checked_mir: &mut Vec<CheckedMirFunction>,
    elaborated_mir: &mut Vec<ElaboratedMirFunction>,
    diagnostics: &mut Vec<MirDiagnostic>,
    record_layouts: &mut Vec<crate::model::RecordLayout>,
    emitted_named_fn_shims: &mut HashSet<String>,
) {
    for generated in generated {
        if generated.raw.name.starts_with("__hew_named_fn_invoke_")
            && !emitted_named_fn_shims.insert(generated.raw.name.clone())
        {
            continue; // duplicate shim from a second use-site — body (and its subtree) already emitted
        }
        let nested = generated.generated;
        thir.push(generated.thir);
        raw_mir.push(generated.raw);
        checked_mir.push(generated.checked);
        elaborated_mir.push(generated.elaborated);
        diagnostics.extend(generated.diagnostics);
        record_layouts.extend(generated.record_layouts);
        flatten_generated_functions(
            nested,
            thir,
            raw_mir,
            checked_mir,
            elaborated_mir,
            diagnostics,
            record_layouts,
            emitted_named_fn_shims,
        );
    }
}

/// Insert execution-context carrier markers into a context-bearing CFG.
///
/// The entry block starts with `EnterContext`; every terminal block (`Return`
/// and `Trap` in today's MIR) ends with `ExitContext` immediately before the
/// terminator. The helper is idempotent so synthetic tests and future
/// context-bearing producers can call it before validation without
/// double-inserting markers.
pub fn bracket_actor_handler_blocks(blocks: &mut [BasicBlock]) {
    if let Some(entry) = blocks.first_mut() {
        if !matches!(entry.instructions.first(), Some(Instr::EnterContext)) {
            entry.instructions.insert(0, Instr::EnterContext);
        }
    }

    for block in blocks {
        if matches!(
            block.terminator,
            Terminator::Return | Terminator::Trap { .. }
        ) && !matches!(block.instructions.last(), Some(Instr::ExitContext))
        {
            block.instructions.push(Instr::ExitContext);
        }
    }
}

fn outbound_live_out(
    blocks: &[BasicBlock],
    suspend_kinds: &HashMap<u32, SuspendKind>,
) -> HashMap<u32, HashSet<u32>> {
    let mut live_in: HashMap<u32, HashSet<u32>> = blocks
        .iter()
        .map(|block| (block.id, HashSet::new()))
        .collect();
    let mut live_out = live_in.clone();

    loop {
        let mut changed = false;
        for block in blocks.iter().rev() {
            let out: HashSet<u32> = block
                .successors()
                .into_iter()
                .filter_map(|succ| live_in.get(&succ))
                .flat_map(|set| set.iter().copied())
                .collect();
            let mut live = out.clone();
            // A terminator's dest slots (a `Call`'s result temp, an `Ask`'s
            // reply dests, …) are defined on the edge into the successor, so
            // the definition kills the local here. Without the kill, a
            // call-result temp consumed by a later call terminator circulates a
            // loop back edge through its own defining block and reads as
            // live-out of its single use site — rejecting a by-construction
            // unique last-use carrier. Only whole-local dests kill: an interior
            // projection write leaves its base live (conservative, and today's
            // terminator dests are freshly allocated whole locals).
            for place in dataflow::terminator_write_places(&block.terminator) {
                if let Place::Local(local) = place {
                    live.remove(&local);
                }
            }
            for place in terminator_source_places(&block.terminator, suspend_kinds.get(&block.id)) {
                if let Some(local) = base_local(place) {
                    live.insert(local);
                }
            }
            // A record-field release immediately before a later whole-local
            // overwrite is a discharge-only read of the OLD value. For owned
            // call carriers the predecessor may transfer that old value and
            // neutralize its source slot; the field release then observes null
            // and safely no-ops before the call result overwrites the slot.
            //
            // Treating that null-tolerant release as an ordinary live read
            // makes the carrier look live after its sole call use, forcing a
            // structural snapshot clone. Besides unnecessary churn, the clone
            // creates a second refcount authority for each COW field and
            // defeats the overwrite release this liveness query is supposed to
            // enable. Track whole-local definitions already seen in the
            // backward walk so ONLY a RecordFieldDrop dominated by a later
            // overwrite gets this transfer-compatible classification. Any
            // intervening or earlier ordinary read still inserts the local and
            // keeps the fail-closed clone path.
            let mut overwritten_later = HashSet::new();
            for instr in block.instructions.iter().rev() {
                let (reads, writes, _) = dataflow::instr_reads_writes(instr);
                for place in writes {
                    if let Place::Local(local) = place {
                        live.remove(&local);
                        overwritten_later.insert(local);
                    } else if let Some(local) = base_local(place) {
                        live.remove(&local);
                    }
                }
                for place in reads {
                    if let Some(local) = base_local(place) {
                        if matches!(
                            instr,
                            Instr::RecordFieldDrop {
                                record: Place::Local(record),
                                ..
                            } if *record == local && overwritten_later.contains(&local)
                        ) {
                            continue;
                        }
                        live.insert(local);
                    }
                }
            }
            if live_out.get(&block.id) != Some(&out) {
                live_out.insert(block.id, out);
                changed = true;
            }
            if live_in.get(&block.id) != Some(&live) {
                live_in.insert(block.id, live);
                changed = true;
            }
        }
        if !changed {
            break;
        }
    }
    live_out
}

fn ty_contains_channel_handle(ty: &ResolvedTy) -> bool {
    match ty {
        ResolvedTy::Named { args, builtin, .. } => {
            matches!(
                builtin,
                Some(
                    BuiltinType::Sender
                        | BuiltinType::Receiver
                        | BuiltinType::Stream
                        | BuiltinType::Sink
                        | BuiltinType::Duplex
                        | BuiltinType::SendHalf
                        | BuiltinType::RecvHalf
                        | BuiltinType::Generator
                        | BuiltinType::AsyncGenerator
                        | BuiltinType::CancellationToken
                )
            ) || args.iter().any(ty_contains_channel_handle)
        }
        ResolvedTy::Tuple(elems) => elems.iter().any(ty_contains_channel_handle),
        ResolvedTy::Array(elem, _) | ResolvedTy::Slice(elem) => ty_contains_channel_handle(elem),
        _ => false,
    }
}

fn outbound_record_layouts(builder: &Builder) -> Vec<RecordLayout> {
    builder
        .record_field_orders
        .iter()
        .map(|(name, fields)| RecordLayout {
            name: name.clone(),
            field_names: fields.iter().map(|(field, _)| field.clone()).collect(),
            field_tys: fields.iter().map(|(_, ty)| ty.clone()).collect(),
        })
        .chain(builder.closure_record_layouts.iter().cloned())
        .collect()
}

#[allow(
    clippy::too_many_lines,
    reason = "the post-CFG carrier boundary keeps classification, liveness, transfer, and fail-closed diagnostics together"
)]
fn prepare_owned_call_carriers(
    blocks: &mut [BasicBlock],
    builder: &mut Builder,
    projection_tainted: &HashSet<u32>,
) {
    let live_out = outbound_live_out(blocks, &builder.suspend_kinds);
    let record_layouts = outbound_record_layouts(builder);
    let pending = std::mem::take(&mut builder.pending_owned_call_args);

    for block in blocks.iter_mut() {
        let Some(site) = pending.get(&block.id) else {
            continue;
        };
        let Terminator::Call { args, .. } = &mut block.terminator else {
            builder.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "owned call-carrier facts without call terminator".to_string(),
                    site: site.args.first().map_or(SiteId(0), |arg| arg.site),
                },
                note: "raw call-carrier facts must be discharged before checked MIR".to_string(),
            });
            continue;
        };
        let mut local_counts: HashMap<u32, usize> = HashMap::new();
        for source in args.iter().copied() {
            if let Some(local) = base_local(source) {
                *local_counts.entry(local).or_default() += 1;
            }
        }

        for arg in &site.args {
            let plan =
                match crate::state_clone::classify_value_snapshot_plan_with_lifecycle_registry(
                    &arg.ty,
                    &record_layouts,
                    &builder.enum_layouts,
                    &builder.opaque_handle_names,
                    &builder.lifecycle_registry,
                ) {
                    Ok(plan) => plan,
                    Err(error) => {
                        builder.diagnostics.push(MirDiagnostic {
                            kind: MirDiagnosticKind::NotYetImplemented {
                                construct: format!(
                                    "owned call-carrier plan for `{}`",
                                    arg.ty.user_facing()
                                ),
                                site: arg.site,
                            },
                            note: error.to_string(),
                        });
                        continue;
                    }
                };
            // Caller half of the admission predicate: the callee half never
            // registers these roots (`register_owned_call_carrier_param`),
            // so preparing a transfer or clone here would strand an owner.
            if snapshot_root_outside_carrier_protocol(plan.root()) {
                continue;
            }
            let local = base_local(arg.source);
            if arg.source_is_prepared_owner {
                let unique_terminal_use = matches!(arg.source, Place::Local(_))
                    && local.is_some_and(|local| {
                        !live_out
                            .get(&block.id)
                            .is_some_and(|live| live.contains(&local))
                            && local_counts.get(&local) == Some(&1)
                    })
                    && args.get(arg.index) == Some(&arg.source);
                if !unique_terminal_use {
                    builder.diagnostics.push(MirDiagnostic {
                        kind: MirDiagnosticKind::NotYetImplemented {
                            construct: format!(
                                "non-unique prepared owned call-carrier `{}`",
                                arg.ty.user_facing()
                            ),
                            site: arg.site,
                        },
                        note: "a carrier projection whose source slot is already neutralized must have exactly one dead-after-call direct consumer"
                            .to_string(),
                    });
                }
                // The callee's consume/carrier contract owns this value now.
                // Its original root-relative slot was already neutralized by
                // lower_value_for_move, so another Move/clone would either leak
                // this sole owner or create a second release authority.
                continue;
            }
            let transferable = matches!(arg.source, Place::Local(_))
                && local.is_some_and(|local| {
                    !live_out
                        .get(&block.id)
                        .is_some_and(|live| live.contains(&local))
                        && !projection_tainted.contains(&local)
                        && !builder.borrowed_value_param_locals.contains(&local)
                        && local_counts.get(&local) == Some(&1)
                });
            if transferable {
                // The instruction stream is about to transfer this source and
                // neutralize its slot. Mirror that ownership hand-off in the
                // checker stream when the source is a named owned binding.
                // A following assignment emits `Bind` in the call successor
                // and restores the redefined binding to Live; without a rebind,
                // the Consume keeps the transferred value out of every
                // scope-exit drop plan. This is the path-sensitive fact the
                // record sole-owner prover cannot recover from a flow-insensitive
                // scan of `NeutralizePayloadSlot` alone.
                let transferred_binding = local.and_then(|source_local| {
                    builder.owned_locals.iter().find_map(|entry| {
                        (builder.binding_locals.get(&entry.binding)
                            == Some(&Place::Local(source_local)))
                        .then(|| {
                            (
                                entry.binding,
                                entry.name.clone(),
                                entry.ty.clone(),
                                entry.disposition,
                            )
                        })
                    })
                });
                let dest = builder.alloc_local(arg.ty.clone());
                builder.transfer_typed_produced_value_owner(arg.site, arg.source, dest);
                block.instructions.push(Instr::Move {
                    dest,
                    src: arg.source,
                });
                block.instructions.push(Instr::NeutralizePayloadSlot {
                    place: arg.source,
                    transferee: Some(dest),
                    authority: crate::model::NeutralizeAuthority::SendTransferLastUse,
                });
                if let Some(slot) = args.get_mut(arg.index) {
                    *slot = dest;
                }
                if let Some((binding, name, ty, Disposition::ScopeExit)) = transferred_binding {
                    // The HIR use may already carry Consume intent (for an
                    // ordinary consuming parameter). Do not author a second
                    // transition at the same call site: the checker would
                    // correctly read that as use-after-consume. Carrier
                    // preparation only supplies the missing transition for
                    // borrow-stamped calls whose physical last use transfers.
                    let already_consumed = block.statements.iter().any(|statement| {
                        matches!(
                            statement,
                            MirStatement::Use {
                                binding: seen_binding,
                                site,
                                intent: IntentKind::Consume,
                                ..
                            } if *seen_binding == binding && *site == arg.site
                        )
                    });
                    if !already_consumed {
                        block.statements.push(MirStatement::Use {
                            binding,
                            name,
                            site: arg.site,
                            ty,
                            intent: IntentKind::Consume,
                        });
                    }
                }
                continue;
            }
            match plan.is_clone_total(
                &record_layouts,
                &builder.enum_layouts,
                &builder.opaque_handle_names,
                &builder.lifecycle_registry,
            ) {
                Ok(true) => {
                    let dest = builder.alloc_local(arg.ty.clone());
                    block.instructions.push(Instr::ValueSnapshotClone {
                        dest,
                        src: arg.source,
                        ty: arg.ty.clone(),
                        plan,
                        boundary: crate::model::PreparedCarrierBoundary::LocalCall,
                    });
                    if let Some(slot) = args.get_mut(arg.index) {
                        *slot = dest;
                    }
                }
                Ok(false) => builder.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::NotYetImplemented {
                        construct: format!(
                            "live owned call-carrier `{}`",
                            arg.ty.user_facing()
                        ),
                        site: arg.site,
                    },
                    note: "the value has no total structural clone and is not a proven unique whole-local last use"
                        .to_string(),
                }),
                Err(error) => builder.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::NotYetImplemented {
                        construct: format!(
                            "owned call-carrier clone-total proof for `{}`",
                            arg.ty.user_facing()
                        ),
                        site: arg.site,
                    },
                    note: error.to_string(),
                }),
            }
        }
    }

    for (block, site) in pending {
        if !blocks.iter().any(|candidate| candidate.id == block) {
            builder.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: format!("pending owned call-carrier block bb{block} is missing"),
                    site: site.args.first().map_or(SiteId(0), |arg| arg.site),
                },
                note: "raw call-carrier facts must be discharged before checked MIR".to_string(),
            });
        }
    }
}

/// Per-exit release plan for one owned call-carrier parameter whose
/// release authority a consuming record/tuple project match took over.
struct CarrierConsumePlan {
    /// Blocks reachable through (or containing) a consumption — the match
    /// discharge already released every owned field on paths into these.
    reach: HashSet<u32>,
    /// Blocks reachable from the entry without passing through any
    /// consumption — the carrier is still whole and owned on paths into
    /// these.
    avoid: HashSet<u32>,
    /// Scrutinee site of the first recorded consumption, for diagnostics.
    site: SiteId,
}

/// Forward CFG reachability from `starts`, never expanding into a block in
/// `blocked` (a `starts` member is only visited if not blocked).
fn cfg_reachable_over(
    successors: &HashMap<u32, Vec<u32>>,
    starts: impl IntoIterator<Item = u32>,
    blocked: &HashSet<u32>,
) -> HashSet<u32> {
    let mut seen: HashSet<u32> = HashSet::new();
    let mut stack: Vec<u32> = starts
        .into_iter()
        .filter(|block| !blocked.contains(block))
        .collect();
    while let Some(block) = stack.pop() {
        if seen.insert(block) {
            stack.extend(
                successors
                    .get(&block)
                    .into_iter()
                    .flatten()
                    .copied()
                    .filter(|succ| !blocked.contains(succ)),
            );
        }
    }
    seen
}

/// Append the callee-side terminal snapshot drop for every owned
/// call-carrier parameter to the function's exits, honouring per-path
/// release-authority transfer.
///
/// A consuming record/tuple project match takes over a carrier's release
/// authority at a recorded block (`Builder::owned_carrier_consumed`): the
/// selected arm's binder discharge and in-place field drops release every
/// owned field on paths that flow through the match. Exits classify per
/// carrier:
///
/// * an exit the consumption cannot reach keeps the terminal drop — a
///   guard / early `return` that branches around the match must still
///   release the untouched carrier (skipping here is the silent leak the
///   global cancellation shipped);
/// * an exit only reachable through a consumption skips the drop — the
///   match already released every field, so a second release double-frees;
/// * a RETURN exit reachable both through and around a consumption has no
///   single release authority — fail closed with a diagnostic rather than
///   pick between a leak and a double-free;
/// * a TRAP exit reachable through a consumption skips the drop even when
///   also reachable around it: the process is aborting, and a possible
///   leak at abort is safe where a possible double-free is not;
/// * a consumption reachable from itself (a consuming match on a loop
///   path) would discharge once per iteration — fail closed.
fn append_owned_carrier_param_drops(blocks: &mut [BasicBlock], builder: &mut Builder) {
    if builder.owned_carrier_params.is_empty() {
        return;
    }
    let successors: HashMap<u32, Vec<u32>> = blocks
        .iter()
        .map(|block| (block.id, block.successors()))
        .collect();
    let entry = blocks.first().map(|block| block.id);
    let consumed = std::mem::take(&mut builder.owned_carrier_consumed);
    let params = std::mem::take(&mut builder.owned_carrier_params);
    let plans: Vec<Option<CarrierConsumePlan>> = params
        .iter()
        .map(|param| {
            let sites = consumed.get(&param.value)?;
            let consume_blocks: HashSet<u32> = sites.iter().map(|(block, _)| *block).collect();
            let site = sites.first().map_or(SiteId(0), |(_, site)| *site);
            // Blocks strictly after a consumption.
            let mut reach = cfg_reachable_over(
                &successors,
                consume_blocks
                    .iter()
                    .filter_map(|block| successors.get(block))
                    .flatten()
                    .copied(),
                &HashSet::new(),
            );
            if consume_blocks.iter().any(|block| reach.contains(block)) {
                builder.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::NotYetImplemented {
                        construct: format!(
                            "owned call-carrier `{}` consumed on a loop path",
                            param.ty.user_facing()
                        ),
                        site,
                    },
                    note: "a consuming record/tuple match on a loop path would \
                           discharge the carrier's fields once per iteration while \
                           the value enters the function once; move the match out \
                           of the loop or destructure a per-iteration clone"
                        .to_string(),
                });
            }
            reach.extend(consume_blocks.iter().copied());
            // Blocks reachable from the entry without touching a consumption.
            let avoid = cfg_reachable_over(&successors, entry, &consume_blocks);
            Some(CarrierConsumePlan { reach, avoid, site })
        })
        .collect();
    let mut ambiguous_reported: HashSet<usize> = HashSet::new();
    for block in blocks {
        let is_return = matches!(block.terminator, Terminator::Return);
        if !is_return && !matches!(block.terminator, Terminator::Trap { .. }) {
            continue;
        }
        for (index, (param, plan)) in params.iter().zip(&plans).enumerate().rev() {
            if let Some(plan) = plan {
                if plan.reach.contains(&block.id) {
                    if is_return
                        && plan.avoid.contains(&block.id)
                        && ambiguous_reported.insert(index)
                    {
                        builder.diagnostics.push(MirDiagnostic {
                            kind: MirDiagnosticKind::NotYetImplemented {
                                construct: format!(
                                    "owned call-carrier `{}` conditionally consumed \
                                     before a shared exit",
                                    param.ty.user_facing()
                                ),
                                site: plan.site,
                            },
                            note: "a consuming record/tuple match transferred the \
                                   carrier's release authority on one path to this \
                                   exit while another path bypasses the match and \
                                   still owns the whole value, so no single release \
                                   authority covers the exit; destructure the value \
                                   on every path, or return before the paths join"
                                .to_string(),
                        });
                    }
                    continue;
                }
            }
            block.instructions.push(Instr::ValueSnapshotDrop {
                value: param.value,
                ty: param.ty.clone(),
                plan: param.plan.clone(),
                boundary: crate::model::PreparedCarrierBoundary::LocalCall,
                guard: Some(param.guard),
            });
        }
    }
    builder.owned_carrier_params = params;
}

#[allow(
    clippy::too_many_lines,
    reason = "the exhaustive structural-kind match is the fail-closed outbound policy boundary"
)]
fn resolve_outbound_actor_modes(
    blocks: &mut [BasicBlock],
    builder: &mut Builder,
    projection_tainted: &HashSet<u32>,
) -> HashMap<u32, Vec<ResolvedOutboundSite>> {
    let live_out = outbound_live_out(blocks, &builder.suspend_kinds);
    let record_layouts = outbound_record_layouts(builder);
    let pending = std::mem::take(&mut builder.pending_outbound_actor_args);
    let mut resolved = HashMap::new();

    for block in blocks.iter_mut() {
        let Some(sites) = pending.get(&block.id) else {
            continue;
        };
        let mut local_counts: HashMap<u32, usize> = HashMap::new();
        for site in sites {
            for arg in &site.args {
                if let Some(local) = base_local(arg.source) {
                    *local_counts.entry(local).or_default() += 1;
                }
            }
        }
        let mut resolved_sites = Vec::with_capacity(sites.len());
        for site in sites {
            let modes: Vec<SendAliasMode> = site.args.iter().map(|arg| {
                let local = base_local(arg.source);
                let transferable = matches!(arg.source, Place::Local(_))
                    && local.is_some_and(|local| {
                        !live_out
                            .get(&block.id)
                            .is_some_and(|live| live.contains(&local))
                            && !projection_tainted.contains(&local)
                            && !builder.borrowed_value_param_locals.contains(&local)
                            && local_counts.get(&local) == Some(&1)
                    });

                if ty_contains_channel_handle(&arg.ty) {
                    return SendAliasMode::TransferLastUse;
                }

                match crate::state_clone::classify_value_snapshot_plan_with_lifecycle_registry(
                    &arg.ty,
                    &record_layouts,
                    &builder.enum_layouts,
                    &builder.opaque_handle_names,
                    &builder.lifecycle_registry,
                ) {
                    Ok(plan) => match plan.root() {
                        SnapshotFieldKind::BitCopy { .. } => SendAliasMode::SnapshotBitCopy,
                        SnapshotFieldKind::String | SnapshotFieldKind::Bytes => {
                            if transferable {
                                SendAliasMode::TransferLastUse
                            } else {
                                SendAliasMode::SnapshotRetain
                            }
                        }
                        SnapshotFieldKind::Rc | SnapshotFieldKind::Weak => {
                            builder.diagnostics.push(MirDiagnostic {
                                kind: MirDiagnosticKind::NotYetImplemented {
                                    construct: format!(
                                        "actor send admitted cloneable non-Send payload `{}`",
                                        arg.ty.user_facing()
                                    ),
                                    site: arg.site,
                                },
                                note: "Rc/Weak structural clone support never implies Send; checker/MIR authority drift must fail closed"
                                    .to_string(),
                            });
                            SendAliasMode::SnapshotMaterialize
                        }
                        SnapshotFieldKind::Tuple { .. }
                        | SnapshotFieldKind::Array { .. }
                        | SnapshotFieldKind::Vec { .. }
                        | SnapshotFieldKind::HashMap { .. }
                        | SnapshotFieldKind::HashSet { .. }
                        | SnapshotFieldKind::UserRecord { .. }
                        | SnapshotFieldKind::Enum { .. } => {
                            if transferable {
                                SendAliasMode::TransferLastUse
                            } else {
                                SendAliasMode::SnapshotMaterialize
                            }
                        }
                        SnapshotFieldKind::IoHandle { .. }
                        | SnapshotFieldKind::ChannelSender
                        | SnapshotFieldKind::OpaqueHandle { .. }
                        | SnapshotFieldKind::ClosurePair
                        | SnapshotFieldKind::TraitObject
                        | SnapshotFieldKind::Resource { .. } => {
                            if transferable {
                                SendAliasMode::TransferLastUse
                            } else {
                                builder.diagnostics.push(MirDiagnostic {
                                    kind: MirDiagnosticKind::NotYetImplemented {
                                        construct: format!(
                                            "snapshot unavailable for actor-send payload `{}`",
                                            arg.ty.user_facing()
                                        ),
                                        site: arg.site,
                                    },
                                    note: "the value has no structural clone path and is not proven last-use transferable"
                                        .to_string(),
                                });
                                SendAliasMode::SnapshotMaterialize
                            }
                        }
                    },
                    Err(error) => {
                        if transferable {
                            SendAliasMode::TransferLastUse
                        } else {
                            builder.diagnostics.push(MirDiagnostic {
                                kind: MirDiagnosticKind::NotYetImplemented {
                                    construct: format!(
                                        "snapshot plan for actor-send payload `{}`",
                                        arg.ty.user_facing()
                                    ),
                                    site: arg.site,
                                },
                                note: error.to_string(),
                            });
                            SendAliasMode::SnapshotMaterialize
                        }
                    }
                }
            }).collect();
            let resolved_args = site
                .args
                .iter()
                .zip(modes.iter().copied())
                .map(|(arg, mode)| ResolvedOutboundArg {
                    source: arg.source,
                    ty: arg.ty.clone(),
                    site: arg.site,
                    mode,
                })
                .collect();
            resolved_sites.push(ResolvedOutboundSite {
                target: site.target,
                args: resolved_args,
            });

            if matches!(site.target, PendingOutboundTarget::Direct) {
                match &mut block.terminator {
                    Terminator::Send { arg_modes, .. } | Terminator::Ask { arg_modes, .. } => {
                        *arg_modes = modes;
                    }
                    Terminator::Suspend { .. } => {
                        let Some(SuspendKind::Ask { arg_modes, .. }) =
                            builder.suspend_kinds.get_mut(&block.id)
                        else {
                            builder.diagnostics.push(MirDiagnostic {
                                kind: MirDiagnosticKind::NotYetImplemented {
                                    construct: "pending outbound arguments on non-ask suspend"
                                        .to_string(),
                                    site: site.args.first().map_or(SiteId(0), |arg| arg.site),
                                },
                                note: "outbound preparation facts and suspend carrier drifted"
                                    .to_string(),
                            });
                            continue;
                        };
                        *arg_modes = modes;
                    }
                    _ => {
                        builder.diagnostics.push(MirDiagnostic {
                            kind: MirDiagnosticKind::NotYetImplemented {
                                construct:
                                    "pending direct outbound arguments without send/ask terminator"
                                        .to_string(),
                                site: site.args.first().map_or(SiteId(0), |arg| arg.site),
                            },
                            note: "raw outbound facts must be discharged before checked MIR"
                                .to_string(),
                        });
                    }
                }
            }
        }
        resolved.insert(block.id, resolved_sites);
    }

    for (block, sites) in pending {
        if !blocks.iter().any(|candidate| candidate.id == block) {
            builder.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: format!("pending outbound block bb{block} is missing"),
                    site: sites
                        .first()
                        .and_then(|site| site.args.first())
                        .map_or(SiteId(0), |arg| arg.site),
                },
                note: "raw outbound facts must be discharged before checked MIR".to_string(),
            });
        }
    }
    resolved
}

#[allow(
    clippy::too_many_lines,
    reason = "preparation keeps mode execution, pack rewriting, and suspend-carrier rewriting in one ownership-critical pass"
)]
fn prepare_outbound_actor_payloads(
    blocks: &mut [BasicBlock],
    builder: &mut Builder,
    resolved: &HashMap<u32, Vec<ResolvedOutboundSite>>,
    projection_tainted: &HashSet<u32>,
) {
    let record_layouts = outbound_record_layouts(builder);
    let mut edge_insertions: Vec<(u32, u32, Vec<Instr>, Vec<Instr>)> = Vec::new();
    for block in blocks.iter_mut() {
        let Some(sites) = resolved.get(&block.id) else {
            continue;
        };
        for site in sites {
            let args = &site.args;
            let payload = match site.target {
                PendingOutboundTarget::Direct => match &block.terminator {
                    Terminator::Send { value, .. } | Terminator::Ask { value, .. } => *value,
                    Terminator::Suspend { .. } => {
                        let Some(SuspendKind::Ask { value, .. }) =
                            builder.suspend_kinds.get(&block.id)
                        else {
                            continue;
                        };
                        *value
                    }
                    _ => continue,
                },
                PendingOutboundTarget::SelectArm(index) => match &block.terminator {
                    Terminator::Select { arms, .. } | Terminator::SuspendingSelect { arms, .. } => {
                        let Some(SelectArmKind::ActorAsk { value, .. }) =
                            arms.get(index).map(|arm| &arm.kind)
                        else {
                            continue;
                        };
                        *value
                    }
                    _ => continue,
                },
                PendingOutboundTarget::JoinBranch(index) => match &block.terminator {
                    Terminator::Join { branches, .. } => {
                        let Some(branch) = branches.get(index) else {
                            continue;
                        };
                        branch.value
                    }
                    _ => continue,
                },
            };

            let mut prep = Vec::new();
            let mut recover_drops = Vec::new();
            let mut prepared = Vec::with_capacity(args.len());
            let fungible_edges = matches!(site.target, PendingOutboundTarget::Direct)
                .then(|| builder.fungible_outbound_edges.get(&block.id).copied())
                .flatten()
                .filter(|_| args.iter().all(|arg| !ty_contains_channel_handle(&arg.ty)));
            for arg in args {
                match arg.mode {
                    SendAliasMode::SnapshotBitCopy => prepared.push(arg.source),
                    SendAliasMode::TransferLastUse => {
                        // Outbound mode resolution can discover a physical
                        // last-use transfer after HIR lowering stamped the
                        // source read-only. If this is any guarded owned leaf
                        // (including a non-idempotent user resource), mark the
                        // same path-local hand-off here, at the authority that
                        // selected `TransferLastUse`, before moving and
                        // neutralising the source slot.
                        let guarded_transfer = builder.owned_locals.iter().find_map(|entry| {
                            if builder.binding_locals.get(&entry.binding) != Some(&arg.source) {
                                return None;
                            }
                            builder
                                .affine_release_flags
                                .get(&entry.binding)
                                .or_else(|| {
                                    builder.actor_message_cow_drop_flags.get(&entry.binding)
                                })
                                .or_else(|| builder.collection_drop_flags.get(&entry.binding))
                                .or_else(|| {
                                    builder.conditional_record_drop_flags.get(&entry.binding)
                                })
                                .copied()
                                .map(|flag| {
                                    (entry.binding, flag, entry.name.clone(), entry.ty.clone())
                                })
                        });
                        if let Some((binding, flag, name, ty)) = guarded_transfer {
                            let already_set = block.instructions.iter().any(|instr| {
                                matches!(
                                    instr,
                                    Instr::ConstI64 {
                                        dest,
                                        value: 1
                                    } if *dest == flag
                                )
                            });
                            if !already_set {
                                prep.push(Instr::ConstI64 {
                                    dest: flag,
                                    value: 1,
                                });
                            }
                            let already_consumed = block.statements.iter().any(|statement| {
                                matches!(
                                    statement,
                                    MirStatement::Use {
                                        binding: seen_binding,
                                        site,
                                        intent: IntentKind::Consume,
                                        ..
                                    } if *seen_binding == binding && *site == arg.site
                                )
                            });
                            if !already_consumed {
                                block.statements.push(MirStatement::Use {
                                    binding,
                                    name,
                                    site: arg.site,
                                    ty,
                                    intent: IntentKind::Consume,
                                });
                            }
                        }
                        let dest = builder.alloc_local(arg.ty.clone());
                        builder.transfer_typed_produced_value_owner(arg.site, arg.source, dest);
                        prep.push(Instr::Move {
                            dest,
                            src: arg.source,
                        });
                        prep.push(Instr::NeutralizePayloadSlot {
                            place: arg.source,
                            transferee: Some(dest),
                            authority: crate::model::NeutralizeAuthority::SendTransferLastUse,
                        });
                        if let Ok(plan) =
                            crate::state_clone::classify_value_snapshot_plan_with_lifecycle_registry(
                                &arg.ty,
                                &record_layouts,
                                &builder.enum_layouts,
                                &builder.opaque_handle_names,
                                &builder.lifecycle_registry,
                            )
                        {
                            if !matches!(plan.root(), SnapshotFieldKind::BitCopy { .. }) {
                                recover_drops.push(Instr::ValueSnapshotDrop {
                                    value: dest,
                                    ty: arg.ty.clone(),
                                    plan,
                                    boundary: crate::model::PreparedCarrierBoundary::Actor,
                                    guard: None,
                                });
                            }
                        }
                        prepared.push(dest);
                    }
                    SendAliasMode::SnapshotRetain | SendAliasMode::SnapshotMaterialize => {
                        let Ok(plan) =
                            crate::state_clone::classify_value_snapshot_plan_with_lifecycle_registry(
                                &arg.ty,
                                &record_layouts,
                                &builder.enum_layouts,
                                &builder.opaque_handle_names,
                                &builder.lifecycle_registry,
                            )
                        else {
                            prepared.push(arg.source);
                            continue;
                        };
                        let dest = builder.alloc_local(arg.ty.clone());
                        prep.push(Instr::ValueSnapshotClone {
                            dest,
                            src: arg.source,
                            ty: arg.ty.clone(),
                            plan: plan.clone(),
                            boundary: crate::model::PreparedCarrierBoundary::Actor,
                        });
                        let source_is_fresh_whole_local = match arg.source {
                            Place::Local(local) => {
                                !projection_tainted.contains(&local)
                                    && !builder
                                        .binding_locals
                                        .values()
                                        .any(|place| *place == arg.source)
                            }
                            _ => false,
                        };
                        if source_is_fresh_whole_local {
                            prep.push(Instr::ValueSnapshotDrop {
                                value: arg.source,
                                ty: arg.ty.clone(),
                                plan: plan.clone(),
                                boundary: crate::model::PreparedCarrierBoundary::Actor,
                                guard: None,
                            });
                        }
                        recover_drops.push(Instr::ValueSnapshotDrop {
                            value: dest,
                            ty: arg.ty.clone(),
                            plan,
                            boundary: crate::model::PreparedCarrierBoundary::Actor,
                            guard: None,
                        });
                        prepared.push(dest);
                    }
                }
            }

            let insert_at = if prepared.len() > 1 {
                block
                    .instructions
                    .iter()
                    .position(
                        |instr| matches!(instr, Instr::RecordInit { dest, .. } if *dest == payload),
                    )
                    .unwrap_or(block.instructions.len())
            } else {
                block.instructions.len()
            };
            if fungible_edges.is_none() {
                for (offset, instr) in prep.iter().cloned().enumerate() {
                    let at = insert_at + offset;
                    cfg_util::shift_instr_spans_on_insert(
                        &mut builder.instr_spans,
                        block.id,
                        u32::try_from(at).unwrap_or(u32::MAX),
                    );
                    block.instructions.insert(at, instr);
                }
            }

            let prepared_payload = match prepared.as_slice() {
                [] => payload,
                [single] => *single,
                _ => {
                    let Some(Instr::RecordInit { fields, .. }) = block.instructions.iter_mut().find(
                    |instr| matches!(instr, Instr::RecordInit { dest, .. } if *dest == payload),
                ) else {
                    continue;
                };
                    for ((_, field), prepared) in fields.iter_mut().zip(&prepared) {
                        *field = *prepared;
                    }
                    payload
                }
            };
            let cleanup_plan = base_local(prepared_payload)
                .and_then(|local| builder.locals.get(local as usize))
                .and_then(|ty| {
                    crate::state_clone::classify_value_snapshot_plan_with_lifecycle_registry(
                        ty,
                        &record_layouts,
                        &builder.enum_layouts,
                        &builder.opaque_handle_names,
                        &builder.lifecycle_registry,
                    )
                    .ok()
                })
                .filter(|plan| !matches!(plan.root(), SnapshotFieldKind::BitCopy { .. }));

            match site.target {
                PendingOutboundTarget::Direct => match &mut block.terminator {
                    Terminator::Send {
                        value,
                        cleanup_plan: carrier_cleanup,
                        ..
                    }
                    | Terminator::Ask {
                        value,
                        cleanup_plan: carrier_cleanup,
                        ..
                    } => {
                        *value = prepared_payload;
                        *carrier_cleanup = cleanup_plan;
                    }
                    Terminator::Suspend { .. } => {
                        if let Some(SuspendKind::Ask {
                            value,
                            cleanup_plan: carrier_cleanup,
                            ..
                        }) = builder.suspend_kinds.get_mut(&block.id)
                        {
                            *value = prepared_payload;
                            *carrier_cleanup = cleanup_plan;
                        }
                    }
                    _ => {}
                },
                PendingOutboundTarget::SelectArm(index) => match &mut block.terminator {
                    Terminator::Select { arms, .. } | Terminator::SuspendingSelect { arms, .. } => {
                        if let Some(SelectArmKind::ActorAsk {
                            value,
                            cleanup_plan: carrier_cleanup,
                            ..
                        }) = arms.get_mut(index).map(|arm| &mut arm.kind)
                        {
                            *value = prepared_payload;
                            *carrier_cleanup = cleanup_plan;
                        }
                    }
                    _ => {}
                },
                PendingOutboundTarget::JoinBranch(index) => {
                    if let Terminator::Join { branches, .. } = &mut block.terminator {
                        if let Some(branch) = branches.get_mut(index) {
                            branch.value = prepared_payload;
                            branch.cleanup_plan = cleanup_plan;
                        }
                    }
                }
            }
            if let Some((prepare_block, recover_block)) = fungible_edges {
                edge_insertions.push((prepare_block, recover_block, prep, recover_drops));
            }
        }
    }

    for (prepare_block, recover_block, prep, drops) in edge_insertions {
        if let Some(block) = blocks.iter_mut().find(|block| block.id == prepare_block) {
            let insert_at = block.instructions.len();
            for (offset, instr) in prep.into_iter().enumerate() {
                let at = insert_at + offset;
                cfg_util::shift_instr_spans_on_insert(
                    &mut builder.instr_spans,
                    block.id,
                    u32::try_from(at).unwrap_or(u32::MAX),
                );
                block.instructions.insert(at, instr);
            }
        }
        if let Some(block) = blocks.iter_mut().find(|block| block.id == recover_block) {
            let insert_at = block.instructions.len();
            for (offset, instr) in drops.into_iter().enumerate() {
                let at = insert_at + offset;
                cfg_util::shift_instr_spans_on_insert(
                    &mut builder.instr_spans,
                    block.id,
                    u32::try_from(at).unwrap_or(u32::MAX),
                );
                block.instructions.insert(at, instr);
            }
        }
    }
}

/// Maximum single-writer copy hops walked back from a join slot's direct source
/// to the owned local that produced it. The `if`/`else` value lowering inserts
/// exactly one temp; the bound keeps the walk terminating on any future shape
/// without special-casing a depth.
const SELECTION_SOURCE_CHAIN_LIMIT: usize = 8;

/// Resolve a join-slot move site to the `(block, instr_index, owned_local,
/// transferee)` at which an owned local's value leaves its slot.
///
/// Returns the site itself when the move's source is already an owned local.
/// Otherwise the source is a temp; the walk follows it back while it has
/// EXACTLY ONE writer and that writer is a whole-local `Move`. A temp with
/// several writers, or one produced by a call/constructor rather than a copy,
/// resolves to `None` (fail-closed: the shape keeps its pre-existing posture).
fn resolve_selection_source(
    blocks: &[BasicBlock],
    candidate_locals: &HashSet<u32>,
    block_id: u32,
    instr_index: usize,
    src_local: u32,
    dest_local: u32,
) -> Option<(u32, usize, u32, u32)> {
    let mut site = (block_id, instr_index, src_local, dest_local);
    for _ in 0..SELECTION_SOURCE_CHAIN_LIMIT {
        if candidate_locals.contains(&site.2) {
            return Some(site);
        }
        let (writer_block, writer_index, inner) = sole_whole_local_move_writer(blocks, site.2)?;
        site = (writer_block, writer_index, inner, site.2);
    }
    None
}

/// The single defining site of `local` when it is written exactly once, by a
/// whole-local `Move`, and never by a terminator. Returns
/// `(block_id, instr_index, source_local)`.
fn sole_whole_local_move_writer(blocks: &[BasicBlock], local: u32) -> Option<(u32, usize, u32)> {
    let mut found: Option<(u32, usize, u32)> = None;
    for block in blocks {
        for (instr_index, instr) in block.instructions.iter().enumerate() {
            if !dataflow::instr_write_places(instr)
                .into_iter()
                .any(|place| base_local(place) == Some(local))
            {
                continue;
            }
            let Instr::Move {
                dest: Place::Local(dest_local),
                src: Place::Local(source_local),
            } = instr
            else {
                return None;
            };
            if *dest_local != local {
                return None;
            }
            if found.is_some() {
                return None;
            }
            found = Some((block.id, instr_index, *source_local));
        }
        if terminator_mint_writes_local(&block.terminator, local) {
            return None;
        }
    }
    found
}

/// Whether a terminator defines `local` (a call result, ask reply slot, select
/// arm binding, …). Such a local is a fresh producer, not a copy of an owned
/// local, so the selection-source walk stops there.
fn terminator_mint_writes_local(terminator: &Terminator, local: u32) -> bool {
    drop_plan::terminator_mint_places(terminator)
        .into_iter()
        .any(|place| base_local(place) == Some(local))
}

/// Backing locals of this frame's MOVE-SEMANTICS owned locals — the only slots
/// a selection transfer may null.
///
/// The classes admitted here are the ones whose every share lowers as a bare
/// pointer bitcopy with NO retain (the M-COW spine invariant documented on
/// `derive_local_collection_drop_allowed`): plain and owned-element `Vec`,
/// `HashMap` / `HashSet` handles, and owned aggregate records. For those,
/// exactly one live slot owns a given allocation, so nulling the source at a
/// transfer leaves exactly one owner and the source's release becomes a
/// null-tolerant no-op.
///
/// `string` and `bytes` are deliberately NOT admitted. They mint co-owners
/// through explicit `StringRetain` / `BytesRetain` markers, so an arm selection
/// over them already leaves TWO owners each holding a reference and each owing
/// one release. Nulling the source there would destroy a live reference's
/// release and leak the retained generation — the opposite of the defect being
/// fixed (measured: `std$net$http$host_of` lost five releases, `if_key` /
/// `range_key` / `map_str` lost both of theirs).
///
/// By-value parameters are excluded in every class: they are caller-retained
/// borrows and the caller owns their release (A278).
fn frame_owned_heap_locals(builder: &Builder) -> HashSet<u32> {
    let mut candidate_locals: HashSet<u32> = HashSet::new();
    for (binding, _name, ty) in builder.owned_locals_exit_candidates() {
        let move_semantics = builder.binding_ty_is_owned_element_vec(&ty)
            || builder.binding_ty_is_plain_vec(&ty)
            || drop_plan::ty_is_local_collection_handle(&ty)
            || builder.is_owned_aggregate_record_ty(&ty);
        if !move_semantics {
            continue;
        }
        if !crate::model::ty_owns_heap_mir(&ty, &builder.record_field_orders, &builder.enum_layouts)
        {
            continue;
        }
        let Some(local) = builder
            .binding_locals
            .get(&binding)
            .and_then(|p| base_local(*p))
        else {
            continue;
        };
        if builder.parameter_locals.contains(&local) {
            continue;
        }
        candidate_locals.insert(local);
    }
    candidate_locals
}

/// Null a whole owned local after a DIVERGENT-ARM VALUE SELECTION moved it into
/// a branch-join result slot.
///
/// `let out = match c { true => a, false => b };` (and the if/else, nested, and
/// return-position forms) lowers to one join slot written by a whole-value
/// `Move` on each arm. The join slot ends up holding exactly ONE of the
/// sources; every other source still owns a live allocation nothing releases.
///
/// Before this pass the join slot was reachable from two distinct alias roots,
/// so `propagate_whole_value_alias_roots` evicted it as conflicted and the
/// escape scans read each arm's `Move` as an ownership escape — which excluded
/// EVERY arm source from the scope-exit drop set. The join slot's own owner
/// then freed the selected value and the losing arm's value leaked, once per
/// call (`Vec<i64>`: 2 leak nodes / 144 B per frame).
///
/// The rewrite makes the transfer physical instead of inferred: after the arm's
/// `Move`, the source slot is zeroed. Ownership is then unambiguous on every
/// path — the join slot owns what it received, and each source owns its own
/// value on the paths where its `Move` never ran. The source keeps its ordinary
/// scope-exit release, which walks a nulled slot (a null-tolerant no-op) on the
/// transferring path and frees normally everywhere else. No runtime flag, no
/// per-exit path reconstruction: the LESSON is `raii-null-after-move`.
///
/// FAIL-CLOSED admission. A source is rewritten only when
///   * the transfer is CONDITIONAL — its block does not dominate every return
///     exit, so some path reaches an exit without executing it. That is the
///     arm-selection signature, and it also covers a selection whose sibling
///     arm diverges (one `Move`, still conditional) and a rebind sitting behind
///     an earlier guard return. A move that dominates every return hands
///     ownership over on ALL paths; it is an ordinary rebind, already resolved
///     by the alias machinery, and is left byte-identical;
///   * the source is a MOVE-SEMANTICS owned local of this frame — never a
///     retain-on-share leaf, never an interior alias, never a parameter (see
///     `frame_owned_heap_locals`); and
///   * the source local has NO READ on any path after the transfer site. A
///     read after the selection (`let out = match c { true => a, .. }; a.len()`)
///     is accepted today as an aliasing read of the still-live source slot;
///     nulling under it would turn a leak into a use-after-move fault. Those
///     shapes keep the pre-existing leak posture until the surface decides the
///     read is an error.
///
/// The paired `NeutralizePayloadSlot` carries
/// [`NeutralizeAuthority::DivergentSelectionTransfer`], and the alias / escape /
/// hand-off scans read that authority off the MIR
/// (`split_consume::is_divergent_selection_transfer_move`) rather than
/// re-deriving the shape.
fn neutralize_divergent_selection_sources(
    blocks: &mut [BasicBlock],
    builder: &mut Builder,
    projection_tainted: &HashSet<u32>,
) {
    let mut candidate_locals = frame_owned_heap_locals(builder);
    // A slot holding an INTERIOR ALIAS of a still-live parent — a match-arm
    // payload binder over an enum/machine carrier, a record/tuple field load,
    // a collection element getter — is not a whole-value owner, and its
    // ownership is governed by the projected-payload provenance machinery
    // (`NeutralizeAuthority::MoveOutArmConsume` and the alias-binder scans),
    // not by whole-local move semantics. Nulling such a binder makes its own
    // release a no-op while the parent's release stays suppressed, so the
    // payload leaks. Leave the whole alias class to its own authority.
    let interior_alias = temp_drop::compute_collection_interior_alias_taint(blocks);
    candidate_locals
        .retain(|local| !projection_tainted.contains(local) && !interior_alias.contains(local));
    if candidate_locals.is_empty() {
        return;
    }

    // A transfer is CONDITIONAL when its block does not dominate every return
    // exit: some path reaches an exit WITHOUT executing the move, and on that
    // path the source still owns its own value and owes its own release. That
    // is the arm-selection signature — `match`/`if` value arms, nested arms, a
    // selection whose sibling arm diverges, and a rebind sitting behind an
    // earlier guard return. A move that dominates every return hands ownership
    // over on ALL paths; the existing alias machinery already resolves it and
    // it is left byte-identical here.
    let dominators = block_dominators(blocks);
    let return_exits: Vec<u32> = blocks
        .iter()
        .filter(|block| matches!(block.terminator, Terminator::Return))
        .map(|block| block.id)
        .filter(|id| dominators.contains_key(id))
        .collect();
    if return_exits.is_empty() {
        return;
    }

    let mut inserts: Vec<(u32, usize, u32, u32)> = Vec::new();
    for block in blocks.iter() {
        for (instr_index, instr) in block.instructions.iter().enumerate() {
            let Instr::Move {
                dest: Place::Local(dest_local),
                src: Place::Local(src_local),
            } = instr
            else {
                continue;
            };
            if dest_local == src_local {
                continue;
            }
            let conditional = return_exits.iter().any(|exit| {
                dominators
                    .get(exit)
                    .is_some_and(|doms| !doms.contains(&block.id))
            });
            if !conditional {
                continue;
            }
            // `if`/`else` value lowering copies each arm's value through a
            // per-arm temp before the join slot (`_6 = move _4; _5 = move _6`),
            // so the join slot's direct source is a temp. Walk back through
            // that single-writer copy chain to the owned local it started at
            // and neutralize THERE — the temp holds the value from that point
            // on, exactly as in the direct `match` form.
            let Some((site_block, site_index, owned_local, transferee)) = resolve_selection_source(
                blocks,
                &candidate_locals,
                block.id,
                instr_index,
                *src_local,
                *dest_local,
            ) else {
                continue;
            };
            if local_read_after_site(
                blocks,
                &builder.suspend_kinds,
                owned_local,
                site_block,
                site_index,
            ) {
                continue;
            }
            // Another authority already nulls this slot at this move (a
            // whole-carrier or send-transfer consume). Its neutralize is the
            // same store; a second one is dead weight in the emitted IR.
            if source_slot_already_neutralized(blocks, site_block, site_index, owned_local) {
                continue;
            }
            inserts.push((site_block, site_index, owned_local, transferee));
        }
    }
    inserts.sort_unstable();
    inserts.dedup();
    if inserts.is_empty() {
        return;
    }

    // Insert back-to-front per block so earlier recorded indices stay valid.
    inserts.sort_unstable_by(|a, b| b.0.cmp(&a.0).then(b.1.cmp(&a.1)));
    for (block_id, instr_index, owned_local, transferee) in inserts {
        let Some(block) = blocks.iter_mut().find(|block| block.id == block_id) else {
            continue;
        };
        let at = instr_index + 1;
        cfg_util::shift_instr_spans_on_insert(
            &mut builder.instr_spans,
            block_id,
            u32::try_from(at).unwrap_or(u32::MAX),
        );
        block.instructions.insert(
            at,
            Instr::NeutralizePayloadSlot {
                place: Place::Local(owned_local),
                transferee: Some(Place::Local(transferee)),
                authority: crate::model::NeutralizeAuthority::DivergentSelectionTransfer,
            },
        );
    }
}

/// Whether the instruction directly after `(site_block, site_index)` already
/// nulls `local` under some other neutralize authority.
fn source_slot_already_neutralized(
    blocks: &[BasicBlock],
    site_block: u32,
    site_index: usize,
    local: u32,
) -> bool {
    blocks
        .iter()
        .find(|block| block.id == site_block)
        .and_then(|block| block.instructions.get(site_index + 1))
        .is_some_and(|instr| {
            matches!(
                instr,
                Instr::NeutralizePayloadSlot { place, .. }
                    if base_local(*place) == Some(local)
            )
        })
}

/// Whether `local` is READ anywhere reachable from just after the instruction
/// at `(site_block, site_index)`.
///
/// Scans the remainder of the site block plus every block reachable from it. A
/// site block that is reachable from itself (a loop body) is scanned in full,
/// because its earlier instructions execute again. Over-approximates reads
/// (every source place of every instruction and terminator counts), so the
/// caller's admission can only be too conservative, never too permissive.
fn local_read_after_site(
    blocks: &[BasicBlock],
    suspend_kinds: &HashMap<u32, SuspendKind>,
    local: u32,
    site_block: u32,
    site_index: usize,
) -> bool {
    let reachable = cfg_util::blocks_reachable_from(blocks, site_block);
    let site_on_cycle = reachable.contains(&site_block);
    let reads = |place: Place| base_local(place) == Some(local);
    for block in blocks {
        let is_site = block.id == site_block;
        if !is_site && !reachable.contains(&block.id) {
            continue;
        }
        // The transfer move itself reads the source — that is the read this
        // neutralize pairs with, never an observation of the nulled slot. On a
        // loop back-edge the rest of the site block IS re-entered, so only the
        // transfer instruction is exempt there; everything else in the block
        // still counts.
        let start = if is_site && !site_on_cycle {
            site_index + 1
        } else {
            0
        };
        for (instr_index, instr) in block.instructions.iter().enumerate().skip(start) {
            if is_site && instr_index == site_index {
                continue;
            }
            if instr_source_places(instr).into_iter().any(reads) {
                return true;
            }
        }
        if terminator_source_places(&block.terminator, suspend_kinds.get(&block.id))
            .into_iter()
            .any(reads)
        {
            return true;
        }
    }
    false
}

/// Recover every local forwarded into persistent actor state.
fn actor_state_ingress_locals(blocks: &[BasicBlock]) -> HashSet<u32> {
    let mut ingress: HashSet<u32> = blocks
        .iter()
        .flat_map(|block| &block.instructions)
        .filter_map(|instruction| match instruction {
            Instr::ActorStateFieldStore { src, .. } => base_local(*src),
            _ => None,
        })
        .collect();
    loop {
        let mut changed = false;
        for instruction in blocks.iter().flat_map(|block| &block.instructions) {
            let Instr::Move { dest, src } = instruction else {
                continue;
            };
            if base_local(*dest).is_some_and(|local| ingress.contains(&local)) {
                if let Some(source) = base_local(*src) {
                    changed |= ingress.insert(source);
                }
            }
        }
        if !changed {
            return ingress;
        }
    }
}

/// Null moved-from owners only after their aggregate sink has completed.
fn neutralize_aggregate_member_sources(blocks: &mut [BasicBlock], builder: &mut Builder) {
    let candidates = builder.owned_locals_exit_candidates();
    let transfer_bindings: HashSet<BindingId> = blocks
        .iter()
        .flat_map(|block| &block.statements)
        .filter_map(|statement| match statement {
            MirStatement::AggregateAlias { binding, .. }
            | MirStatement::Use {
                binding,
                intent: IntentKind::Consume | IntentKind::Discharge,
                ..
            } => Some(*binding),
            _ => None,
        })
        .collect();
    let candidate_by_root: HashMap<u32, BindingId> = candidates
        .iter()
        .filter(|(binding, _name, ty)| {
            transfer_bindings.contains(binding)
                && !matches!(ty, ResolvedTy::String | ResolvedTy::Bytes)
                && !drop_plan::ty_is_owned_handle_leaf(ty)
        })
        .filter_map(|(binding, _name, _ty)| {
            builder
                .binding_locals
                .get(binding)
                .and_then(|place| base_local(*place))
                .map(|local| (local, *binding))
        })
        .collect();
    if candidate_by_root.is_empty() {
        return;
    }
    let alias_to_root =
        propagate_whole_value_alias_roots(blocks, candidate_by_root.keys().copied());
    let actor_state_ingress_locals = actor_state_ingress_locals(blocks);

    for block in blocks {
        let mut index = 0;
        while index < block.instructions.len() {
            let sinks: Vec<(Place, Place)> = match &block.instructions[index] {
                Instr::TupleConstruct { elements, dest } => {
                    elements.iter().map(|source| (*source, *dest)).collect()
                }
                Instr::RecordInit { fields, dest, .. } => fields
                    .iter()
                    .map(|(_offset, source)| (*source, *dest))
                    .collect(),
                Instr::RecordFieldStore { record, src, .. } => vec![(*src, *record)],
                Instr::Move {
                    dest: Place::MachineVariant { local, .. } | Place::EnumVariant { local, .. },
                    src,
                } => vec![(*src, Place::Local(*local))],
                _ => Vec::new(),
            };
            index += 1;
            let mut roots = HashSet::new();
            for (source, destination) in sinks {
                if base_local(destination)
                    .is_some_and(|local| actor_state_ingress_locals.contains(&local))
                {
                    continue;
                }
                let Some(source_local) = base_local(source) else {
                    continue;
                };
                let root = alias_to_root
                    .get(&source_local)
                    .copied()
                    .unwrap_or(source_local);
                if !candidate_by_root.contains_key(&root) || !roots.insert(root) {
                    continue;
                }
                shift_instr_spans_on_insert(
                    &mut builder.instr_spans,
                    block.id,
                    u32::try_from(index).unwrap_or(u32::MAX),
                );
                block.instructions.insert(
                    index,
                    Instr::NeutralizePayloadSlot {
                        place: Place::Local(root),
                        transferee: Some(destination),
                        authority: crate::model::NeutralizeAuthority::AggregateMemberConsume,
                    },
                );
                index += 1;
            }
        }
    }
}

/// Release an owned element cloned from a collection after its sole borrowing read.
#[allow(
    clippy::too_many_lines,
    reason = "one proof pass keeps clone provenance, use classification, and release insertion together"
)]
fn release_cloned_collection_result_temps(blocks: &mut [BasicBlock], builder: &mut Builder) {
    let cloned_locals: HashMap<u32, HashSet<u32>> = blocks
        .iter()
        .filter_map(|block| match &block.terminator {
            Terminator::Call {
                authority:
                    crate::model::CallAuthority::Runtime(
                        hew_types::runtime_call::RuntimeCallFamily::VecGet(
                            hew_types::runtime_call::VecGetElem::Clone,
                        ),
                    ),
                dest: Some(place),
                ..
            } => base_local(*place).map(|local| (local, block.id)),
            _ => None,
        })
        .fold(HashMap::new(), |mut by_local, (local, mint_block)| {
            by_local
                .entry(local)
                .or_insert_with(HashSet::new)
                .insert(mint_block);
            by_local
        });
    let record_layouts = outbound_record_layouts(builder);
    let mut inserts = Vec::new();

    for (local, mint_blocks) in cloned_locals {
        let Some(ty) = builder.locals.get(local as usize).cloned() else {
            continue;
        };
        let mut reads = Vec::new();
        let mut admissible = true;
        for block in blocks.iter() {
            for (index, instruction) in block.instructions.iter().enumerate() {
                let (instruction_reads, _writes, _) =
                    crate::dataflow::instr_reads_writes(instruction);
                if !instruction_reads
                    .iter()
                    .any(|read| base_local(*read) == Some(local))
                {
                    continue;
                }
                let borrowed_field = matches!(
                    instruction,
                    Instr::RecordFieldLoad { record, dest, .. }
                        | Instr::TupleFieldLoad { tuple: record, dest, .. }
                        if base_local(*record) == Some(local)
                            && base_local(*dest)
                                .and_then(|dest| builder.locals.get(dest as usize))
                                .is_some_and(|field_ty| {
                                    ValueClass::of_ty(field_ty, &builder.type_classes)
                                        == ValueClass::BitCopy
                                })
                );
                let borrowed_clone = matches!(
                    instruction,
                    Instr::RecordCloneInplace { src, .. }
                        | Instr::EnumCloneInplace { src, .. }
                        | Instr::ValueSnapshotClone { src, .. }
                        if base_local(*src) == Some(local)
                );
                if !borrowed_field && !borrowed_clone {
                    admissible = false;
                    break;
                }
                reads.push((block.id, index));
            }
            if !admissible
                || terminator_source_places(&block.terminator, builder.suspend_kinds.get(&block.id))
                    .iter()
                    .any(|read| base_local(*read) == Some(local))
            {
                admissible = false;
                break;
            }
        }
        let Some((read_block, last_index)) = reads.last().copied() else {
            continue;
        };
        if !admissible || reads.iter().any(|(block, _)| *block != read_block) {
            continue;
        }
        if mint_blocks
            .iter()
            .any(|mint| !block_postdominates_owner_mint(blocks, *mint, read_block))
            || !block_executes_at_most_once_per_owner_mint(blocks, &mint_blocks, read_block)
        {
            continue;
        }
        let release = match crate::state_clone::classify_value_snapshot_plan_with_lifecycle_registry(
            &ty,
            &record_layouts,
            &builder.enum_layouts,
            &builder.opaque_handle_names,
            &builder.lifecycle_registry,
        ) {
            Ok(plan) => Instr::ValueSnapshotDrop {
                value: Place::Local(local),
                ty: ty.clone(),
                plan,
                boundary: crate::model::PreparedCarrierBoundary::LocalCall,
                guard: None,
            },
            Err(_)
                if builder.is_owned_aggregate_record_ty(&ty)
                    && builder.field_drop_in_place_admissible(&ty) =>
            {
                Instr::Drop {
                    place: Place::Local(local),
                    ty,
                    drop_fn: Some(crate::model::DropFnSpec::InPlace(
                        crate::ownership::InPlaceReleaseKind::Record,
                    )),
                }
            }
            Err(_) => continue,
        };
        inserts.push((read_block, last_index + 1, local, release));
    }

    inserts.sort_unstable_by(|left, right| right.0.cmp(&left.0).then_with(|| right.1.cmp(&left.1)));
    for (block_id, insert_at, local, release) in inserts {
        let Some(block) = blocks.iter_mut().find(|block| block.id == block_id) else {
            continue;
        };
        shift_instr_spans_on_insert(
            &mut builder.instr_spans,
            block_id,
            u32::try_from(insert_at).unwrap_or(u32::MAX),
        );
        block.instructions.insert(insert_at, release);
        let bindings: Vec<BindingId> = builder
            .binding_locals
            .iter()
            .filter_map(|(binding, place)| (base_local(*place) == Some(local)).then_some(*binding))
            .collect();
        for binding in bindings {
            builder.set_owned_local_disposition(binding, Disposition::ScopeReleased);
        }
    }
}

/// Whether every terminating path from `start` executes `postdominator`.
fn block_postdominates_owner_mint(blocks: &[BasicBlock], start: u32, postdominator: u32) -> bool {
    if start == postdominator {
        return true;
    }
    let by_id: HashMap<u32, &BasicBlock> = blocks.iter().map(|block| (block.id, block)).collect();
    let mut reachable = HashSet::new();
    let mut worklist = vec![start];
    while let Some(block_id) = worklist.pop() {
        if block_id == postdominator || !reachable.insert(block_id) {
            continue;
        }
        if let Some(block) = by_id.get(&block_id) {
            worklist.extend(block.successors());
        }
    }
    !blocks
        .iter()
        .any(|block| reachable.contains(&block.id) && block.successors().is_empty())
}

/// Whether each execution of `release_block` follows a new owner generation.
fn block_executes_at_most_once_per_owner_mint(
    blocks: &[BasicBlock],
    mint_blocks: &HashSet<u32>,
    release_block: u32,
) -> bool {
    if mint_blocks.is_empty() {
        return false;
    }
    if mint_blocks.contains(&release_block) {
        return true;
    }
    let by_id: HashMap<u32, &BasicBlock> = blocks.iter().map(|block| (block.id, block)).collect();
    if !by_id.contains_key(&release_block)
        || mint_blocks.iter().any(|mint| !by_id.contains_key(mint))
    {
        return false;
    }

    let mut seen = HashSet::new();
    let mut worklist = by_id[&release_block].successors();
    while let Some(block_id) = worklist.pop() {
        if mint_blocks.contains(&block_id) || !seen.insert(block_id) {
            continue;
        }
        if block_id == release_block {
            return false;
        }
        if let Some(block) = by_id.get(&block_id) {
            worklist.extend(block.successors());
        }
    }
    true
}

/// Release a typed owner immediately after its final proven borrowing read.
#[allow(
    clippy::too_many_lines,
    reason = "one proof pass keeps owner eligibility, postdominance, and release insertion together"
)]
fn release_last_borrowed_typed_owners(blocks: &mut [BasicBlock], builder: &mut Builder) {
    let mut candidates = Vec::new();
    let mut candidate_locals = HashSet::new();
    for (binding, _name, ty) in builder.owned_locals_exit_candidates() {
        if !builder
            .typed_produced_value_owner_bindings
            .contains(&binding)
        {
            continue;
        }
        if matches!(ty, ResolvedTy::Bytes) {
            continue;
        }
        if matches!(
            ValueClass::of_ty(&ty, &builder.type_classes),
            ValueClass::AffineResource | ValueClass::Linear
        ) {
            continue;
        }
        let Some(place) = builder.binding_locals.get(&binding).copied() else {
            continue;
        };
        let Some(local) = base_local(place) else {
            continue;
        };
        if !candidate_locals.insert(local) {
            continue;
        }
        candidates.push((binding, local, place, ty));
    }

    for (binding, local, place, ty) in candidates {
        let record_layouts = outbound_record_layouts(builder);
        let mut reads = Vec::new();
        let mut admissible = true;
        for block in blocks.iter() {
            for (index, instruction) in block.instructions.iter().enumerate() {
                let (instruction_reads, _writes, _) =
                    crate::dataflow::instr_reads_writes(instruction);
                if !instruction_reads
                    .iter()
                    .any(|read| base_local(*read) == Some(local))
                {
                    continue;
                }
                let borrowed_field = matches!(
                    instruction,
                    Instr::RecordFieldLoad { record, dest, .. }
                        | Instr::TupleFieldLoad { tuple: record, dest, .. }
                        if base_local(*record) == Some(local)
                            && base_local(*dest)
                                .and_then(|dest| builder.locals.get(dest as usize))
                                .is_some_and(|field_ty| {
                                    ValueClass::of_ty(field_ty, &builder.type_classes)
                                        == ValueClass::BitCopy
                                })
                );
                let borrowed_clone = matches!(
                    instruction,
                    Instr::RecordCloneInplace { src, .. }
                        | Instr::EnumCloneInplace { src, .. }
                        | Instr::ValueSnapshotClone { src, .. }
                        if base_local(*src) == Some(local)
                );
                if !borrowed_field
                    && !borrowed_clone
                    && !drop_plan::binder_read_is_borrow_safe_instr(instruction, local)
                {
                    admissible = false;
                    break;
                }
                reads.push((block.id, index));
            }
            if !admissible
                || terminator_source_places(&block.terminator, builder.suspend_kinds.get(&block.id))
                    .iter()
                    .any(|read| base_local(*read) == Some(local))
            {
                admissible = false;
                break;
            }
        }
        let Some((read_block, last_index)) = reads.last().copied() else {
            continue;
        };
        if !admissible || reads.iter().any(|(block, _)| *block != read_block) {
            continue;
        }
        let already_released = blocks.iter().any(|block| {
            block.instructions.iter().any(|instruction| {
                matches!(instruction, Instr::Drop { place, .. } if base_local(*place) == Some(local))
                    || matches!(instruction, Instr::ValueSnapshotDrop { value, .. } if base_local(*value) == Some(local))
            })
        });
        if already_released {
            continue;
        }
        let mut mint_blocks = HashSet::new();
        for block in blocks.iter() {
            for instruction in &block.instructions {
                let (_reads, writes, _mutates) = crate::dataflow::instr_reads_writes(instruction);
                if writes.iter().any(|write| base_local(*write) == Some(local)) {
                    mint_blocks.insert(block.id);
                }
            }
            if drop_plan::terminator_mint_places(&block.terminator)
                .iter()
                .any(|write| base_local(*write) == Some(local))
            {
                mint_blocks.insert(block.id);
            }
        }
        if mint_blocks.is_empty()
            || mint_blocks
                .iter()
                .any(|mint| !block_postdominates_owner_mint(blocks, *mint, read_block))
            || !block_executes_at_most_once_per_owner_mint(blocks, &mint_blocks, read_block)
        {
            continue;
        }
        let release = if matches!(ty, ResolvedTy::String) {
            Instr::Drop {
                place,
                ty: ty.clone(),
                drop_fn: Some(crate::model::DropFnSpec::Release("hew_string_drop")),
            }
        } else {
            match crate::state_clone::classify_value_snapshot_plan_with_lifecycle_registry(
                &ty,
                &record_layouts,
                &builder.enum_layouts,
                &builder.opaque_handle_names,
                &builder.lifecycle_registry,
            ) {
                Ok(plan) => Instr::ValueSnapshotDrop {
                    value: place,
                    ty: ty.clone(),
                    plan,
                    boundary: crate::model::PreparedCarrierBoundary::LocalCall,
                    guard: None,
                },
                Err(_)
                    if builder.is_owned_aggregate_record_ty(&ty)
                        && builder.field_drop_in_place_admissible(&ty) =>
                {
                    Instr::Drop {
                        place,
                        ty: ty.clone(),
                        drop_fn: Some(crate::model::DropFnSpec::InPlace(
                            crate::ownership::InPlaceReleaseKind::Record,
                        )),
                    }
                }
                Err(_) => continue,
            }
        };
        let Some(block) = blocks.iter_mut().find(|block| block.id == read_block) else {
            continue;
        };
        let insert_at = last_index + 1;
        shift_instr_spans_on_insert(
            &mut builder.instr_spans,
            block.id,
            u32::try_from(insert_at).unwrap_or(u32::MAX),
        );
        block.instructions.insert(insert_at, release);
        builder.set_owned_local_disposition(binding, Disposition::ScopeReleased);
    }
}

/// Null scalar handle sources after an exact tuple/record constructor has
/// moved them into a value that reaches the return slot.
///
/// Aggregate construction byte-copies handle words. The returned-member drop
/// proof already makes the constructor destination the caller's owner and
/// suppresses the source close on that path; this companion rewrite clears the
/// stale source slot so crash-cleanup escrow cannot close the selected handle
/// again during the normal return sweep. Unselected siblings are untouched and
/// keep their path-sensitive exit closes.
fn neutralize_returned_aggregate_handle_sources(blocks: &mut [BasicBlock], builder: &mut Builder) {
    let candidates = builder.owned_locals_exit_candidates();
    let returned_members =
        derive_returned_aggregate_member_bindings(blocks, &candidates, &builder.binding_locals);
    if returned_members.is_empty() {
        return;
    }

    let candidate_places: HashSet<Place> = candidates
        .iter()
        .filter(|(binding, _name, ty)| {
            returned_members.contains(binding) && drop_plan::ty_is_owned_handle_leaf(ty)
        })
        .filter_map(|(binding, _name, _ty)| builder.binding_locals.get(binding).copied())
        .collect();
    if candidate_places.is_empty() {
        return;
    }

    // Reverse the exact whole-value move graph from the return slot. A
    // constructor qualifies only when its own destination is in this closure;
    // another aggregate built from the same source in the same block cannot
    // trigger an early neutralize.
    let mut returned_value_locals: HashSet<u32> = blocks
        .iter()
        .flat_map(|block| &block.instructions)
        .filter_map(|instr| match instr {
            Instr::Move {
                dest: Place::ReturnSlot,
                src,
            } => base_local(*src),
            _ => None,
        })
        .collect();
    loop {
        let mut changed = false;
        for instr in blocks.iter().flat_map(|block| &block.instructions) {
            let Instr::Move { dest, src } = instr else {
                continue;
            };
            if base_local(*dest).is_some_and(|dest| returned_value_locals.contains(&dest)) {
                if let Some(source) = base_local(*src) {
                    changed |= returned_value_locals.insert(source);
                }
            }
        }
        if !changed {
            break;
        }
    }

    for block in blocks {
        let mut index = 0;
        while index < block.instructions.len() {
            let (dest, sources): (Place, Vec<Place>) = match &block.instructions[index] {
                Instr::TupleConstruct { elements, dest } => (*dest, elements.clone()),
                Instr::RecordInit { fields, dest, .. } => (
                    *dest,
                    fields.iter().map(|(_offset, source)| *source).collect(),
                ),
                _ => {
                    index += 1;
                    continue;
                }
            };
            if !base_local(dest).is_some_and(|local| returned_value_locals.contains(&local)) {
                index += 1;
                continue;
            }

            let mut neutralized = HashSet::new();
            let sources: Vec<Place> = sources
                .into_iter()
                .filter(|source| candidate_places.contains(source) && neutralized.insert(*source))
                .collect();
            index += 1;
            for source in sources {
                shift_instr_spans_on_insert(
                    &mut builder.instr_spans,
                    block.id,
                    u32::try_from(index).unwrap_or(u32::MAX),
                );
                block.instructions.insert(
                    index,
                    Instr::NeutralizePayloadSlot {
                        place: source,
                        transferee: Some(dest),
                        authority:
                            crate::model::NeutralizeAuthority::ReturnedAggregateMemberConsume,
                    },
                );
                index += 1;
            }
        }
    }
}

#[must_use]
pub fn validate_outbound_actor_modes(raw: &RawMirFunction) -> Vec<MirCheck> {
    let payload_requires_mode = |value: Place| {
        base_local(value)
            .and_then(|local| raw.locals.get(local as usize))
            .is_none_or(|ty| !matches!(ty, ResolvedTy::Unit))
    };
    raw.blocks
        .iter()
        .filter_map(|block| {
            let unresolved = match &block.terminator {
                Terminator::Send {
                    value, arg_modes, ..
                }
                | Terminator::Ask {
                    value, arg_modes, ..
                } => arg_modes.is_empty() && payload_requires_mode(*value),
                Terminator::Suspend { .. } => {
                    raw.suspend_kinds
                        .get(&block.id)
                        .is_some_and(|kind| match kind {
                            SuspendKind::Ask {
                                value, arg_modes, ..
                            } => arg_modes.is_empty() && payload_requires_mode(*value),
                            _ => false,
                        })
                }
                _ => false,
            };
            unresolved.then_some(MirCheck::OutboundModeUnresolved { block: block.id })
        })
        .collect()
}

/// Locals released by an inline `Instr::Drop` naming the string or bytes
/// release symbol. Used to take a before/after reading around the nested-temp
/// splices so publication-owner retirement pairs with the releases those
/// splices placed rather than with any release that happens to be present.
fn inline_string_or_bytes_release_locals(blocks: &[BasicBlock]) -> HashSet<u32> {
    blocks
        .iter()
        .flat_map(|block| block.instructions.iter())
        .filter_map(|instr| match instr {
            Instr::Drop {
                place: Place::Local(local),
                drop_fn: Some(crate::model::DropFnSpec::Release(symbol)),
                ..
            } if matches!(*symbol, "hew_string_drop" | "hew_bytes_drop") => Some(*local),
            _ => None,
        })
        .collect()
}

/// Phase one of [`finalize_body`]: splice the RELEASES the per-binding drop
/// provers cannot see — nested fresh string/bytes temporaries, the share-intent
/// finalisation, and the non-escaped sibling fields of an escaped record.
///
/// These run before the checker statement snapshot because each adds a
/// checker-visible read of the value it discharges.
fn splice_body_ownership_releases(
    blocks: &mut Vec<BasicBlock>,
    builder: &mut Builder,
    nested_fresh_temp_releases: bool,
) {
    if nested_fresh_temp_releases {
        // Inline string/bytes releases this body's own lowering already
        // emitted. The retirement below pairs with the releases the nested-temp
        // splices are about to add, so anything already present is excluded.
        let released_before_splices = inline_string_or_bytes_release_locals(blocks);
        let nested_temp_binding_locals: HashMap<BindingId, Place> = builder
            .binding_locals
            .iter()
            .filter(|(binding, _)| {
                !builder
                    .synthetic_owner_publication_sites
                    .contains_key(binding)
            })
            .map(|(binding, place)| (*binding, *place))
            .collect();
        apply_nested_fresh_string_temp_drops(
            &mut *blocks,
            &mut builder.suspend_kinds,
            &builder.locals,
            &nested_temp_binding_locals,
            &builder
                .call_scrutinee_provenance
                .owned_string_return_carrier_symbols,
            &mut builder.suspend_abandon_extra_drops,
            &mut builder.instr_spans,
        );
        // #2542 — release nested fresh-owned `bytes` user-call-result temporaries
        // (`mk().len()`, `mk().to_string()`, discarded `mk();`) that
        // `derive_local_bytes_drop_allowed` (binding-scoped) cannot see. Same
        // pre-`check_function` ordering rationale as the string splice above.
        apply_nested_fresh_bytes_temp_drops(
            &mut *blocks,
            &builder.suspend_kinds,
            &builder.locals,
            &nested_temp_binding_locals,
            &mut builder.instr_spans,
        );
        // Inline string/bytes releases consume the exact typed-publication
        // generation for their local.  This keeps concat/f-string and bytes
        // temporaries on the checker-owned carrier without a second scope-exit
        // drop authority, while persistent publications remain ledger-owned.
        builder
            .consume_typed_publication_owners_at_inline_release(&*blocks, &released_before_splices);
    }
    finalize_string_local_share_intents(&mut *blocks, builder);
    splice_escaped_record_sibling_field_drops(blocks, builder);
    splice_pretransfer_record_exit_drops(&mut *blocks, builder);
}

/// Release an escaped record on exits reached before its field transfer.
#[allow(
    clippy::too_many_lines,
    reason = "one proof pass keeps transfer reachability, dominance, and exit splicing together"
)]
fn splice_pretransfer_record_exit_drops(blocks: &mut [BasicBlock], builder: &mut Builder) {
    let candidates: HashMap<u32, (BindingId, ResolvedTy)> = builder
        .owned_locals_exit_candidates()
        .into_iter()
        .filter(|(_binding, _name, ty)| {
            builder.is_owned_aggregate_record_ty(ty) && builder.field_drop_in_place_admissible(ty)
        })
        .filter_map(|(binding, _name, ty)| {
            builder
                .binding_locals
                .get(&binding)
                .and_then(|place| base_local(*place))
                .map(|local| (local, (binding, ty)))
        })
        .collect();
    if candidates.is_empty() {
        return;
    }

    let mut transfer_blocks: HashMap<u32, HashSet<u32>> = HashMap::new();
    let mut mint_blocks: HashMap<u32, HashSet<u32>> = HashMap::new();
    for block in blocks.iter() {
        for instruction in &block.instructions {
            if let Instr::FieldDropInPlace { base, .. } = instruction {
                if let Some(local) =
                    base_local(*base).filter(|local| candidates.contains_key(local))
                {
                    transfer_blocks.entry(local).or_default().insert(block.id);
                }
            }
            let (_reads, writes, _mutates) = crate::dataflow::instr_reads_writes(instruction);
            for local in writes.into_iter().filter_map(base_local) {
                if candidates.contains_key(&local) {
                    mint_blocks.entry(local).or_default().insert(block.id);
                }
            }
        }
        for local in drop_plan::terminator_mint_places(&block.terminator)
            .into_iter()
            .filter_map(base_local)
        {
            if candidates.contains_key(&local) {
                mint_blocks.entry(local).or_default().insert(block.id);
            }
        }
    }
    let dominators = block_dominators(blocks);
    let mut inserts = Vec::new();
    for block in blocks
        .iter()
        .filter(|block| matches!(block.terminator, Terminator::Return))
    {
        for (local, (_binding, ty)) in &candidates {
            let Some(transfers) = transfer_blocks.get(local) else {
                continue;
            };
            let Some(mints) = mint_blocks.get(local) else {
                continue;
            };
            if !mints.iter().any(|mint| {
                dominators
                    .get(&block.id)
                    .is_some_and(|exit_dominators| exit_dominators.contains(mint))
            }) {
                continue;
            }
            let transfer_can_reach_exit = transfers.iter().any(|transfer| {
                *transfer == block.id
                    || cfg_util::blocks_reachable_from(blocks, *transfer).contains(&block.id)
            });
            if transfer_can_reach_exit {
                continue;
            }
            let already_released = block.instructions.iter().any(|instruction| {
                matches!(instruction, Instr::Drop { place, .. } if base_local(*place) == Some(*local))
                    || matches!(instruction, Instr::ValueSnapshotDrop { value, .. } if base_local(*value) == Some(*local))
            });
            if already_released {
                continue;
            }
            inserts.push((block.id, *local, ty.clone()));
        }
    }

    for (block_id, local, ty) in inserts {
        let Some(block) = blocks.iter_mut().find(|block| block.id == block_id) else {
            continue;
        };
        let insert_at = block.instructions.len();
        shift_instr_spans_on_insert(
            &mut builder.instr_spans,
            block_id,
            u32::try_from(insert_at).unwrap_or(u32::MAX),
        );
        block.instructions.insert(
            insert_at,
            Instr::Drop {
                place: Place::Local(local),
                ty,
                drop_fn: Some(crate::model::DropFnSpec::InPlace(
                    crate::ownership::InPlaceReleaseKind::Record,
                )),
            },
        );
    }
}

/// #2212 — an owned record whose field escapes through a binder loses its
/// composite scope-exit drop (the sole-owner prover excludes it); the record's
/// NON-escaped owned sibling fields still need their release. Where the value
/// flow proves the escape's root, field, and last-use position, splice one
/// `FieldDropInPlace` per dischargeable sibling right after the escape.
fn splice_escaped_record_sibling_field_drops(blocks: &mut Vec<BasicBlock>, builder: &mut Builder) {
    let owned_locals_snapshot = builder.owned_locals_snapshot();
    // #2212 — an owned record whose field escapes through a binder loses its
    // composite scope-exit drop (the sole-owner prover excludes it); the
    // record's NON-escaped owned sibling fields still need their release.
    // Where the value flow proves the escape's root, field, and last-use
    // position, splice one `FieldDropInPlace` per dischargeable sibling
    // right after the escape. Runs BEFORE `check_function` / elaboration so
    // the dataflow observes the discharges and codegen emits them.
    {
        let mut instr_spans = std::mem::take(&mut builder.instr_spans);
        // The immediate-parent chain of every recorded byte-copy alias, so the
        // sibling-discharge emitter can walk a MULTI-HOP escape (`let mid = o.mid;
        // let leaf = mid.leaf; return leaf`) and compensate the non-escaped
        // siblings at every level — the reach `close_alias_binders_forward` gave
        // the composite-drop prover's exclusion.
        let alias_chain = builder.alias_projection_chain();
        let aggregate_clone_sites = aggregate_borrowed_ingress_clone_sites(&*blocks, builder);
        let is_owned_record = |ty: &ResolvedTy| builder.is_owned_aggregate_record_ty(ty);
        let owned_field_list = |ty: &ResolvedTy| builder.project_record_owned_field_list(ty);
        let owned_tuple_field_list = |ty: &ResolvedTy| builder.project_tuple_owned_field_list(ty);
        let field_dischargeable = |ty: &ResolvedTy| {
            matches!(ty, ResolvedTy::String) || builder.field_drop_in_place_admissible(ty)
        };
        apply_escaped_record_sibling_field_drops(
            &mut *blocks,
            &builder.suspend_kinds,
            &owned_locals_snapshot,
            &builder.binding_locals,
            &builder.locals,
            &builder.record_field_orders,
            &builder.enum_layouts,
            builder.type_classes.lifecycle_registry(),
            &alias_chain,
            &aggregate_clone_sites,
            &builder.vec_iter_projection_borrow_inits,
            &is_owned_record,
            &owned_field_list,
            &owned_tuple_field_list,
            &field_dischargeable,
            &mut instr_spans,
        );
        builder.instr_spans = instr_spans;
    }
}

/// Phase two of [`finalize_body`]: prepare the OPERANDS — name locals for the
/// debug tables, classify actor-state loads, materialise owned call carriers
/// and outbound actor payloads, and null every slot whose ownership physically
/// transferred (returned aggregate members, divergent-arm selection sources).
///
/// These run after the checker statement snapshot because they rewrite operands
/// rather than discharge values.
fn prepare_body_transfers(blocks: &mut Vec<BasicBlock>, builder: &mut Builder) {
    builder.resolve_local_names_from_binds(&*blocks);
    // P0 #2432 — classify every `ActorStateFieldLoad`'s own/borrow `mode`
    // over the fully finalised blocks (every splice pass above has already
    // run). Must run on THIS `blocks` local, before it moves into `raw`
    // below: codegen's per-instruction lowering loop reads
    // `RawMirFunction.blocks`, not `CheckedMirFunction.blocks` (a clone
    // taken afterward), so classifying any later would be a no-op against
    // codegen. A no-op here for every non-actor-handler lowering entry point
    // (`load_locals` is empty whenever `current_actor_state_fields` was
    // never populated).
    classify_actor_state_load_modes(
        &mut *blocks,
        &builder.suspend_kinds,
        &builder.locals,
        &builder.module_fn_names,
        &builder.module_generic_fn_names,
        &builder.call_scrutinee_provenance.extern_table,
    );
    let projection_tainted = temp_drop::compute_projection_alias_taint(
        &*blocks,
        &builder.match_project_consumed_binder_locals,
        &builder.fresh_variant_payload_binder_locals,
        &builder.locals,
    );
    prepare_owned_call_carriers(&mut *blocks, builder, &projection_tainted);
    let resolved_outbound =
        resolve_outbound_actor_modes(&mut *blocks, builder, &projection_tainted);
    prepare_outbound_actor_payloads(
        &mut *blocks,
        builder,
        &resolved_outbound,
        &projection_tainted,
    );
    neutralize_aggregate_member_sources(&mut *blocks, builder);
    neutralize_returned_aggregate_handle_sources(&mut *blocks, builder);
    neutralize_divergent_selection_sources(&mut *blocks, builder, &projection_tainted);
    release_partially_transferred_return_carriers(&mut *blocks, builder);
    release_cloned_collection_result_temps(&mut *blocks, builder);
    release_last_borrowed_typed_owners(&mut *blocks, builder);
}

/// Release the remaining slots of an owned enum carrier after one selected
/// payload transfers into the function return value.
///
/// A `NeutralizePayloadSlot` rooted in a `MachineVariant` is a measured partial
/// transfer: the returned payload owns the cleared slot, while the carrier
/// still owns its shell and every untouched slot. Scope-exit elaboration can
/// conservatively classify the same move as a whole-owner escape and omit the
/// carrier from an early-return plan. Close that gap directly in the primitive
/// stream, using the neutralize authority itself as the transfer fact and the
/// structural local type as the release authority. Parameters remain
/// caller-owned and keep their separate terminal snapshot protocol.
fn release_partially_transferred_return_carriers(blocks: &mut [BasicBlock], builder: &mut Builder) {
    for block in blocks {
        if !matches!(block.terminator, Terminator::Return) {
            continue;
        }
        let transferred_roots: Vec<(u32, ResolvedTy)> = block
            .instructions
            .iter()
            .filter_map(|instr| match instr {
                Instr::NeutralizePayloadSlot {
                    place: Place::MachineVariant { local, .. },
                    ..
                } if !builder.parameter_locals.contains(local)
                    && builder.actor_ask_result_locals.contains(local) =>
                {
                    let ty = builder.locals.get(*local as usize)?;
                    let owns_heap = crate::model::ty_owns_heap_mir(
                        ty,
                        &builder.record_field_orders,
                        &builder.enum_layouts,
                    );
                    let is_enum = matches!(ty, ResolvedTy::Named { name, args, .. }
                        if crate::model::find_enum_layout(name, args, &builder.enum_layouts)
                            .is_some());
                    (owns_heap && is_enum).then(|| (*local, ty.clone()))
                }
                _ => None,
            })
            .collect();
        for (local, ty) in transferred_roots {
            let already_released = block.instructions.iter().any(|instr| {
                matches!(
                    instr,
                    Instr::Drop {
                        place: Place::Local(drop_local),
                        ..
                    } if *drop_local == local
                ) || matches!(
                    instr,
                    Instr::ValueSnapshotDrop {
                        value: Place::Local(drop_local),
                        ..
                    } if *drop_local == local
                )
            });
            if already_released {
                continue;
            }
            let insert_at = block.instructions.len();
            shift_instr_spans_on_insert(
                &mut builder.instr_spans,
                block.id,
                u32::try_from(insert_at).unwrap_or(u32::MAX),
            );
            block.instructions.push(Instr::Drop {
                place: Place::Local(local),
                ty,
                drop_fn: Some(crate::model::DropFnSpec::InPlace(
                    crate::ownership::InPlaceReleaseKind::Enum,
                )),
            });
        }
    }
}

/// Which body-kind-specific splices [`finalize_body`] runs on top of the shared
/// ownership pipeline.
///
/// Everything NOT named here is shared by construction: a pass added to
/// `finalize_body` reaches every body kind — free functions, closure shims,
/// lambda-actor handler bodies, generator bodies, the synthesized invoke /
/// fork trampolines, the task-entry adapter, and the machine-`step` dispatch —
/// without a second registration. Keeping this struct small is the point: each
/// field is a documented exception, and a new pass must argue its way into
/// becoming one rather than defaulting to it.
#[derive(Clone, Copy)]
pub(in crate::lower) struct BodyFinalizeSpec {
    /// Append releases for owned by-value carrier PARAMETERS. Only a body
    /// lowered from a `HirFn` signature carries the parameter ledger this
    /// reads; the shim and trampoline ramps bind their operands as locals.
    pub owned_carrier_param_drops: bool,
    /// Bracket the body with `EnterContext` / `ExitContext` — only for a call
    /// convention that carries the hidden execution-context argument.
    pub bracket_execution_context: bool,
    /// Splice inline releases for nested fresh string/bytes temporaries and
    /// retire the publication owners those releases discharge.
    ///
    /// This pair has a PRECONDITION the lambda-actor handler body does not
    /// meet. That body arrives with its own message and reply teardown spine
    /// and derives its string ownership separately afterwards
    /// (`finalize_string_ownership`); running the pair over it withdraws
    /// releases the body still owes. Measured on `examples/lambda_actors.hew`:
    /// `__hew_lambda_body_main_6` emitted 28 `hew_string_drop` calls without
    /// the pair and 12 with it, and every `__hew_lambda_body_*` in the example
    /// corpus lost its full scope-exit release count. Fail closed — that body
    /// keeps the posture it had, and the seam still carries every other pass to
    /// it.
    pub nested_fresh_temp_releases: bool,
}

impl BodyFinalizeSpec {
    /// The spec for every body not lowered from a `HirFn` signature: closure
    /// shims, generator bodies, the synthesized invoke / fork trampolines, and
    /// the machine-`step` dispatch. The lambda-actor handler body and the
    /// task-entry adapter each narrow one field below.
    pub(in crate::lower) fn nested_body() -> Self {
        Self {
            owned_carrier_param_drops: false,
            bracket_execution_context: false,
            nested_fresh_temp_releases: true,
        }
    }

    /// The lambda-actor handler body: everything except the nested fresh-temp
    /// release pair, whose precondition it does not meet (see the field).
    pub(in crate::lower) fn lambda_actor_body() -> Self {
        Self {
            nested_fresh_temp_releases: false,
            ..Self::nested_body()
        }
    }

    /// The synthesized `TaskEntry` adapter. Its two blocks are constructed
    /// rather than lowered through a cursor, but the `TaskEntry` call
    /// convention carries the hidden execution-context argument
    /// (`FunctionCallConv::carries_execution_context`), so the body needs the
    /// `EnterContext` / `ExitContext` bracket. It used to call the bracketing
    /// helper directly and never reached the pipeline at all.
    pub(in crate::lower) fn task_entry_adapter() -> Self {
        Self {
            bracket_execution_context: true,
            ..Self::nested_body()
        }
    }
}

/// How [`finalize_body`] obtains the sealed block list.
///
/// Almost every body ends with a live cursor that needs a terminator. The
/// synthesised machine-`step` dispatch is the exception: it already finished
/// its last block with a `Trap`, and sealing again would append a phantom
/// cursor block. Naming that as a seal STRATEGY keeps it inside the one seam
/// instead of justifying a hand-rolled drain beside it.
#[derive(Clone)]
pub(in crate::lower) enum BodySeal {
    /// Seal the in-flight cursor with this terminator.
    Cursor(Terminator),
    /// The last block is already terminated; drain the pending list as-is.
    AlreadyTerminated,
}

/// A finished body: the fully spliced blocks plus the two facts every caller
/// needs from the pipeline that produced them.
pub(in crate::lower) struct FinalizedBody {
    pub blocks: Vec<BasicBlock>,
    /// The checker statement stream, snapshotted at the canonical point — after
    /// the ownership splices that add checker-visible reads, before the carrier
    /// and outbound preparation that rewrites operands. Callers use THIS rather
    /// than re-flattening `blocks`, or the snapshot drifts between body kinds.
    pub thir_statements: Vec<MirStatement>,
}

/// THE body-finalization seam. Every body kind finishes here.
///
/// Before this existed, `lower_function` ran a long ownership pipeline after
/// sealing its blocks while `lower_closure_shim`, the lambda-actor handler
/// ramp, `lower_gen_block`, `lower_named_fn_invoke_shim` and the fork
/// trampoline each hand-rolled a PARTIAL copy of it — several carrying a
/// comment explaining that they "mirror `lower_function`'s call site". Every
/// copy is a place a later pass can be forgotten, and the omission is silent:
/// the body compiles, and the missing splice surfaces only as a runtime leak.
/// The divergent-arm selection release was added to `lower_function` alone, and
/// a selection inside a closure or a generator kept leaking a whole `Vec` per
/// call for exactly that reason.
///
/// [`Builder::seal_body_blocks`] is called from here and nowhere else, and the
/// synthesized task-entry adapter — which constructs its blocks rather than
/// lowering them through a cursor — arrives here through
/// [`BodySeal::AlreadyTerminated`] rather than bracketing itself on the side.
/// `hew-mir/tests/body_finalization_seam.rs` resolves the ENCLOSING FUNCTION of
/// every call to both names, so a ninth ramp that grows its own pipeline, or a
/// seal moved out of this function, fails there.
pub(in crate::lower) fn finalize_body(
    builder: &mut Builder,
    seal: BodySeal,
    spec: BodyFinalizeSpec,
) -> FinalizedBody {
    let mut blocks = match seal {
        BodySeal::Cursor(terminator) => builder.seal_body_blocks(terminator),
        BodySeal::AlreadyTerminated => {
            let mut pending = std::mem::take(&mut builder.pending_blocks);
            pending.sort_by_key(|block| block.id);
            pending
        }
    };
    if spec.owned_carrier_param_drops {
        append_owned_carrier_param_drops(&mut blocks, builder);
    }
    if spec.bracket_execution_context {
        // `bracket_actor_handler_blocks` splices `EnterContext` at index 0 of
        // the entry block when it is not already present. That shifts the
        // Stage 2 side-table indices for the entry block — realign them. (A
        // ctx-bearing body that is also a suspend coroutine is skipped by
        // codegen's debug path anyway, but keeping the table correct here is
        // cheap and fail-closed for any non-suspend ctx-bearing `fn`.)
        let entry_had_enter = matches!(
            blocks.first().and_then(|b| b.instructions.first()),
            Some(Instr::EnterContext)
        );
        bracket_actor_handler_blocks(&mut blocks);
        if !entry_had_enter {
            if let Some(entry_id) = blocks.first().map(|b| b.id) {
                shift_instr_spans_on_insert(&mut builder.instr_spans, entry_id, 0);
            }
        }
    }
    splice_body_ownership_releases(&mut blocks, builder, spec.nested_fresh_temp_releases);
    let thir_statements: Vec<MirStatement> = blocks
        .iter()
        .flat_map(|b| b.statements.iter().cloned())
        .collect();
    // Name the `let`-bound locals from the emitted `Bind` stream before the
    // blocks are moved into the raw function — params were already named in
    // `lower_params`. Feeds the `-g` variable DIEs.
    prepare_body_transfers(&mut blocks, builder);
    FinalizedBody {
        blocks,
        thir_statements,
    }
}

#[allow(
    clippy::too_many_arguments,
    clippy::too_many_lines,
    reason = "lowering threads shared module tables plus the call-convention discriminator"
)]
pub(crate) fn lower_function(
    func: &HirFn,
    emit_name: String,
    subst: HashMap<String, ResolvedTy>,
    type_classes: &hew_hir::TypeClassTable,
    record_field_orders: &HashMap<String, Vec<(String, ResolvedTy)>>,
    actor_layouts: &HashMap<String, ActorLayout>,
    supervisor_layout_map: &HashMap<String, crate::model::SupervisorLayout>,
    machine_layout_names: &HashSet<String>,
    enum_layouts: &[crate::model::EnumLayout],
    opaque_handle_names: &[String],
    current_actor_name: Option<&str>,
    module_fn_names: &HashSet<String>,
    module_generic_fn_names: &HashSet<String>,
    call_scrutinee_provenance: &Rc<crate::return_provenance::CallScrutineeProvenance>,
    param_ownership: &Rc<ParamOwnershipFacts>,
    trait_impl_index: &HashMap<
        hew_hir::dispatch::TraitImplKey,
        hew_hir::dispatch::TraitImplMethodEntry,
    >,
    direct_call_symbols: &HashMap<hew_types::DefId, String>,
    call_site_type_args: &HashMap<hew_hir::SiteId, Vec<ResolvedTy>>,
    vec_generic_element_abi: Option<&HashMap<hew_types::Ty, hew_types::VecElementToken>>,
    supervisor_child_slots: &HashMap<hew_hir::SiteId, ChildSlot>,
    pool_accessor_sites: &HashMap<hew_hir::SiteId, hew_types::PoolAccessor>,
    pointer_width: PointerWidth,
    call_conv: crate::model::FunctionCallConv,
    task_entry_adapter_symbols: TaskEntryAdapterSymbols,
) -> LoweredFunction {
    let mut builder = Builder {
        type_classes: type_classes.clone(),
        record_field_orders: record_field_orders.clone(),
        actor_layouts: actor_layouts.clone(),
        machine_layout_names: machine_layout_names.clone(),
        enum_layouts: enum_layouts.to_vec(),
        opaque_handle_names: opaque_handle_names.to_vec(),
        lifecycle_registry: type_classes.lifecycle_registry().clone(),
        supervisor_layout_map: supervisor_layout_map.clone(),
        current_actor_state_fields: current_actor_name
            .and_then(|name| actor_layouts.get(name))
            .map(|layout| {
                layout
                    .state_field_names
                    .iter()
                    .cloned()
                    .zip(layout.state_field_tys.iter().cloned())
                    .enumerate()
                    .map(|(idx, (name, ty))| {
                        (
                            name,
                            (
                                FieldOffset(
                                    u32::try_from(idx).expect("actor field count exceeds u32::MAX"),
                                ),
                                ty,
                            ),
                        )
                    })
                    .collect()
            })
            .unwrap_or_default(),
        module_fn_names: module_fn_names.clone(),
        module_generic_fn_names: module_generic_fn_names.clone(),
        call_scrutinee_provenance: call_scrutinee_provenance.clone(),
        proven_foreign_bindings: std::collections::HashSet::new(),
        param_ownership: param_ownership.clone(),
        trait_impl_index: trait_impl_index.clone(),
        direct_call_symbols: direct_call_symbols.clone(),
        subst,
        call_site_type_args: call_site_type_args.clone(),
        vec_generic_element_abi: vec_generic_element_abi.cloned().unwrap_or_default(),
        supervisor_child_slots: supervisor_child_slots.clone(),
        pool_accessor_sites: pool_accessor_sites.clone(),
        pointer_width,
        current_function_symbol: emit_name.clone(),
        current_function_call_conv: call_conv,
        generator_shell_call_gate: func.is_generator.then_some(GeneratorShellCallGate),
        task_entry_adapter_symbols,
        ..Builder::default()
    };
    // Allocate parameter locals BEFORE lowering the function body so
    // that `BindingRef` expressions that reference parameters resolve
    // to real `Place::Local(i)` slots instead of hitting `UnresolvedPlace`.
    //
    // Each parameter gets its own `Place::Local` in declaration order;
    // subsequent body-local allocations are appended after these slots
    // (enforced by `alloc_local`'s monotone `locals.len()` counter).
    // Codegen emits a parameter-prologue that stores each LLVM function
    // argument into the corresponding alloca slot before the first instruction.
    // Derive the stream-producer pump context from information `lower_function`
    // already carries — `func.is_generator` and `call_conv` — rather than
    // threading a new parameter through this (already heavily-shared) function
    // and its dozen call sites. Only a `receive gen fn` handler shell (built by
    // `lower_actor_receive_handlers`, which appends a synthetic trailing sink
    // param and lowers with `FunctionCallConv::ActorHandler`) matches both
    // conditions; a standalone `gen fn`/`gen {}` shell lowers `is_generator`
    // under `FunctionCallConv::Default` and is untouched.
    if func.is_generator && call_conv == crate::model::FunctionCallConv::ActorHandler {
        let yield_ty = func.body.tail.as_ref().and_then(|tail| match &tail.kind {
            HirExprKind::GenBlock { yield_ty, .. } => Some(yield_ty.clone()),
            _ => None,
        });
        if let Some(yield_ty) = yield_ty {
            let sink_idx = u32::try_from(func.params.len().checked_sub(1).expect(
                "receive-gen-fn shell must carry at least the synthetic trailing sink param",
            ))
            .expect("stream-producer handler param count exceeds u32::MAX");
            builder.stream_producer_pump = Some(StreamProducerPumpCtx {
                sink: Place::Local(sink_idx),
                yield_ty,
            });
        }
    }
    builder.lower_params(func);
    builder.funcupdate_base_proven =
        compute_funcupdate_base_provenance(func, &call_scrutinee_provenance.fresh_owner_verdicts);
    // #2648 S2b — the caller arg-scan's per-function local freshness facts,
    // computed under the SAME module tables the S1 fixpoint used.
    builder.call_scrutinee_local_freshness =
        crate::return_provenance::compute_local_binding_freshness(
            func,
            &call_scrutinee_provenance.provenance,
            &call_scrutinee_provenance.extern_table,
            &call_scrutinee_provenance.may_mutate,
        );
    builder.function_body(func);

    // Effective return type after type-parameter substitution.
    let return_ty = builder.subst_ty(&func.return_ty);

    // Drain the in-flight current block into a sealed `BasicBlock` with
    // a `Terminator::Return`. Slice 1's flat lowering always produces a
    // singleton blocks vector; Slice 2+ may surface multiple blocks
    // when `If` (and later `Match` / loops) split the CFG. The order is
    // monotone in block id.
    let FinalizedBody {
        blocks,
        thir_statements,
    } = finalize_body(
        &mut builder,
        BodySeal::Cursor(Terminator::Return),
        BodyFinalizeSpec {
            owned_carrier_param_drops: true,
            bracket_execution_context: call_conv.carries_execution_context(),
            nested_fresh_temp_releases: true,
        },
    );
    let thir = ThirFunction {
        name: emit_name.clone(),
        return_ty: return_ty.clone(),
        statements: thir_statements,
    };
    // Re-read the scope-exit-live owned-locals view after the splice pipeline:
    // the carrier and outbound preparation inside `finalize_body` can retire a
    // binding from the ledger, so the double-free gate below must see the
    // post-pipeline ledger rather than a snapshot taken ahead of it.
    let owned_locals_snapshot = builder.owned_locals_snapshot();
    debug_assert!(
        builder.pending_outbound_actor_args.is_empty(),
        "checked MIR cannot retain unresolved outbound actor arguments"
    );
    debug_assert!(
        builder.pending_owned_call_args.is_empty(),
        "checked MIR cannot retain unresolved owned call-carrier arguments"
    );
    assert_eq!(
        builder.param_boundary_modes.len(),
        func.params.len(),
        "every lowered source parameter must have exactly one typed boundary mode"
    );
    for (param_index, (param, mode)) in func
        .params
        .iter()
        .zip(builder.param_boundary_modes.iter().copied())
        .enumerate()
    {
        let param_index =
            u32::try_from(param_index).expect("function parameter count exceeds u32::MAX");
        let ty = builder.subst_ty(&param.ty);
        builder.decisions.push(DecisionFact {
            site: SiteId(param_index),
            value_class: ValueClass::of_ty(&ty, &builder.type_classes),
            ty,
            intent: IntentKind::Unknown,
            strategy: Strategy::ParamBoundary(ParamBoundaryFact {
                param_index,
                param_count: u32::try_from(func.params.len())
                    .expect("function parameter count exceeds u32::MAX"),
                caller_visible_projection: builder
                    .param_ownership
                    .caller_visible_param_projections
                    .contains(&(func.id, param_index as usize)),
                mode,
            }),
            why: "checker-authoritative parameter boundary classification".to_string(),
        });
    }
    // `CheckedMirFunction` mirrors `RawMirFunction.blocks` directly
    // (widened in Slice 2 from a single-block field to a vec). The
    // elaborator + check_function consume the block vec; legacy
    // single-block tests still see `blocks[0]` as the entry block.
    let mut raw = RawMirFunction {
        name: emit_name.clone(),
        return_ty: return_ty.clone(),
        call_conv,
        params: func
            .params
            .iter()
            .map(|p| builder.subst_ty(&p.ty))
            .collect(),
        locals: builder.locals.clone(),
        local_names: builder.local_names.clone(),
        local_scopes: builder
            .local_scopes
            .iter()
            .map(|s| s.map(|sc| sc.0))
            .collect(),
        local_decl_bytes: builder.local_decl_bytes.clone(),
        scope_table: builder.build_scope_table(),
        blocks,
        decisions: builder.decisions.clone(),
        // W5.005 / F1b: carry the floor-intrinsic catalog id from HIR so
        // codegen's `lower_fn` synthesizes the trampoline body instead of
        // emitting the bodyless placeholder (the D343 fail-OPEN no-op).
        intrinsic_id: func.intrinsic_id.clone(),
        await_deadline_ns: builder.await_deadline_ns.clone(),
        suspend_kinds: builder.suspend_kinds.clone(),
        lambda_actor_user_param_locals: Vec::new(),
        span: Some((
            u32::try_from(func.span.start).unwrap_or(u32::MAX),
            u32::try_from(func.span.end).unwrap_or(u32::MAX),
        )),
        // Stage 2 (gdb `-g`): the per-instruction line table threaded from the
        // lowering cursor (`push_instr`), already realigned for the post-seal
        // splices above. Cloned (not moved) — `builder` is still read by
        // `check_function` below.
        instr_spans: builder.instr_spans.clone(),
        source_origin: SourceOrigin::Unknown,
    };
    // Checked MIR's `checks` field is populated by `check_function`
    // from real dataflow over the checker-authority `MirStatement`
    // stream. The `MirDiagnostic` surface that the CLI rejects on is
    // projected from these checks — there is one source of truth for
    // move/borrow/init legality.
    let mut dataflow_result = check_function(&builder, &raw.blocks, func);
    dataflow_result
        .checks
        .extend(crate::model::validate_context_markers(&raw));
    dataflow_result
        .checks
        .extend(validate_outbound_actor_modes(&raw));
    let mut diagnostics: Vec<MirDiagnostic> = dataflow_result
        .checks
        .iter()
        .filter_map(check_to_diagnostic)
        .collect();
    diagnostics.extend(move_value::ordinary_projection_transfer_diagnostics(
        &raw.blocks,
        &builder.suspend_kinds,
    ));

    // Collect diagnostics emitted by the builder (e.g., Unsupported HIR nodes).
    diagnostics.append(&mut builder.diagnostics);

    // Reject any owned local holding a `Vec<E>` with no wired per-element
    // release (`Vec<bytes>` / `Vec<indirect_enum>`) at compile, rather than
    // constructing it and silently leaking the element nodes at scope exit. The
    // outer `Vec` owns heap unconditionally, but no release bucket claims such an
    // element, so without this reject it would fall through every scope-exit drop
    // set and leak. Consuming the typed
    // `VecElementRelease::Unsupported(NoReleaseProtocol)` disposition here moves
    // that "no" up to compile time (the leak-safe fail-closed direction — a
    // defined, actionable error the author can act on, over a silent runtime
    // leak). The release itself stays a tracked follow-up; this is the
    // fail-closed half. Bind sites come from the finalized blocks so the error
    // points at the real construction site.
    let bind_sites: HashMap<BindingId, SiteId> = raw
        .blocks
        .iter()
        .flat_map(|block| block.statements.iter())
        .filter_map(|stmt| match stmt {
            MirStatement::Bind { binding, site, .. } => Some((*binding, *site)),
            _ => None,
        })
        .collect();
    diagnostics.extend(builder.unsupported_vec_element_diagnostics(&bind_sites));

    collect_unknown_type_diagnostics(func, &builder, &mut diagnostics);

    let string_derivation = finalize_string_ownership(&mut raw, &mut builder, &dataflow_result);
    let bytes_derivation = finalize_bytes_ownership(&mut raw, &mut builder, &dataflow_result);
    let deferred_drop_binding_locals = std::mem::take(&mut builder.deferred_drop_binding_locals);
    builder.binding_locals.extend(deferred_drop_binding_locals);

    // Compute cooperate-check sites from the CFG. Empty for leaf functions
    // (< 10 MIR statements, no calls, no loops). Codegen reads
    // `cooperate_sites` to inject `call @hew_actor_cooperate()`.
    let cooperate_sites = dataflow::compute_cooperate_sites(&raw.blocks);

    let checked = CheckedMirFunction {
        name: emit_name,
        return_ty: return_ty.clone(),
        blocks: raw.blocks.clone(),
        decisions: builder.decisions.clone(),
        checks: dataflow_result.checks.clone(),
        cooperate_sites,
    };
    diagnostics.extend(vec_iter_yield_abandonment_diagnostics(
        &checked,
        &builder,
        &dataflow_result,
    ));
    // Drop-elaboration pass. Consumes the CheckedMirFunction we just
    // built; emits an ElaboratedMirFunction whose `blocks` + `drop_plans`
    // are the authoritative description of what fires on every exit.
    //
    // `owned_locals` is the per-function ownership ledger. Every binding
    // whose type obliges a scope-exit drop (`binding_seeds_drop_elaboration`
    // — `string`, `Vec<E>`, records, collection handles, generators, closure
    // pairs, resource fields) is registered once at its defining write
    // through `Builder::register_owned_local`, carrying its classified
    // `ValueOwnership`, any interior-alias `ValueProvenance`, and a
    // `Disposition`. The elaborator reads the scope-exit-live view of that
    // ledger (a consumed / body-end-released binding carries a
    // non-`ScopeExit` disposition rather than being physically removed), and
    // the carried provenance keeps a byte-copy field alias from being
    // mistaken for an independent owner. The pass runs on every function
    // body and is exercised end-to-end by the compiled leak-oracle
    // fixtures — real programs whose native binaries are proven leak- and
    // double-free-clean under `leaks --atExit` and the poisoned allocator —
    // as well as by hew-mir's hand-constructed CheckedMirFunction unit inputs.
    let (elaborated, elaboration_diagnostics) = elaborate(
        &checked,
        &builder,
        &thir.statements,
        &dataflow_result,
        Some(&string_derivation.allowed),
        Some(&bytes_derivation.allowed),
    );
    diagnostics.extend(elaboration_diagnostics);

    // Fail-closed validation of the elaborated drop plan. Surfaces a
    // `MirCheck::DropPlanUndetermined` for any Return-block whose
    // per-exit live-set decision the elaborator could not commit to.
    // No partial drops escape: a `DropPlanUndetermined` finding
    // upgrades into a `MirDiagnostic` via `check_to_diagnostic`, and
    // the CLI rejects the program before codegen runs. LESSONS:
    // cleanup-all-exits, boundary-fail-closed.
    for check in validate_drop_plan(&elaborated) {
        if let Some(diag) = check_to_diagnostic(&check) {
            diagnostics.push(diag);
        }
    }
    // S1 obligation-balance gate: every heap-owning owned local must be
    // released exactly once on every reachable Return path. The discharge
    // set is re-derived from the primitive Instr stream + CFG (never from
    // the Disposition ledger — the ledger is the component under test).
    //
    // SEVERITY SPLIT (close-by-construction, no forgeable allowlist):
    // - Over-release (double-free) is memory-unsafe — an unconditional HARD
    //   error with no escape.
    // - An unverifiable fixpoint (ObligationBalanceUnverified) fails CLOSED as
    //   a HARD error.
    // - Under-release (leak) is surfaced as an ADVISORY diagnostic
    //   (`MirDiagnosticKind::is_advisory`) — a compile-time warning that does
    //   NOT fail the build. The lite MIR tier has no un-forgeable defining-
    //   module provenance at this gate, so it cannot SOUNDLY suppress a leak: a
    //   per-function allowlist keyed on the mangled symbol is forgeable (the
    //   compiler mangles a user module `base64`'s `decode` to the same
    //   `base64$decode` a stdlib entry would use, silently swallowing a genuine
    //   user leak). Rather than hard-gate leaks behind that forgeable key, the
    //   gate warns on EVERY under-release — nothing to forge, tracked stdlib
    //   holes warn honestly. Promoting under-release to a sound hard gate is the
    //   OWN-V1 follow-up (frontend module provenance threaded into this gate).
    //
    // All three route through `check_to_diagnostic` unconditionally; the
    // advisory-vs-blocking severity is a property of the diagnostic kind, read
    // by the CLI at render time (LESSONS boundary-fail-closed).
    for check in validate_obligation_balance(&elaborated, &raw, &builder) {
        if let Some(diag) = check_to_diagnostic(&check) {
            diagnostics.push(diag);
        }
    }
    // Discharge-authority carriage gates (A / D159): (1) the fail-closed
    // backstop — a NeutralizePayloadSlot whose authority owns a destination must
    // carry its transferee; (2) the corroboration pin — the carried transferee
    // fact must agree with the independently re-derived routing. S1 above does
    // NOT read these facts (independence preserved); the corroboration is a
    // separate third pass comparing the two carriers (LESSONS boundary-fail-closed,
    // duplicated-boundary-fact-needs-a-pin-test).
    for check in validate_discharge_authority(&elaborated, &raw)
        .into_iter()
        .chain(validate_discharge_authority_corroboration(
            &elaborated,
            &raw,
        ))
    {
        if let Some(diag) = check_to_diagnostic(&check) {
            diagnostics.push(diag);
        }
    }
    // Fail-closed legality rules for the field-addressed in-place drop op:
    // the op's `ty` must be a shape the shared classifier (or the `string`
    // reroute) admits, its base must be a record/tuple local matching the
    // field address, and an inline-composite field release requires the
    // base's composite drop to be suppressed (exactly-once pairing — see
    // `validate_field_drop_in_place`).
    let field_drop_admissible = |ty: &ResolvedTy| builder.field_drop_in_place_admissible(ty);
    for check in validate_field_drop_in_place(
        &raw.blocks,
        &elaborated,
        &builder.locals,
        &builder.enum_layouts,
        &field_drop_admissible,
    ) {
        if let Some(diag) = check_to_diagnostic(&check) {
            diagnostics.push(diag);
        }
    }
    // Cross-block stale-handle detection. Walks the backend Instr
    // stream across the function's CFG and rejects drop plans that
    // fire `Place::DuplexHandle(N)` on a block whose reaching paths
    // have already moved the unified handle into a `SendHalf` /
    // `RecvHalf` split. Catches the case the slice-3 structural
    // checker cannot — a same-direction close emitted by codegen on
    // a handle whose previous owner has been moved out. LESSONS:
    // cleanup-all-exits, raii-null-after-move,
    // boundary-fail-closed.
    for check in validate_cross_block_split_consume(&raw.blocks, &elaborated) {
        if let Some(diag) = check_to_diagnostic(&check) {
            diagnostics.push(diag);
        }
    }
    // W3.053 catch-all fail-closed gate: refuse any owned handle that would
    // reach codegen with more than one live free path for the same runtime
    // context (the combinatorial aggregate-extraction double-free class). The
    // gate reconstructs the per-context free-site count the elaborator + codegen
    // will emit from the same drop-eligibility derivations `elaborate` uses:
    //   - `source_excluded` = the source-binding drops `elaborate` REMOVES
    //     (returned-aggregate handoff + consumed-local extraction); a source
    //     binding NOT in this set still fires its own standalone LIFO drop.
    //   - `composite_drop_allowed` = the owned-aggregate bindings whose in-place
    //     member drop `elaborate` KEEPS (tuple + owned-record). When such a
    //     member drop AND the source's own drop both free one context, that is
    //     the double-free this gate refuses.
    let returned_aggregate_members = derive_returned_aggregate_member_bindings(
        &raw.blocks,
        &owned_locals_snapshot,
        &builder.binding_locals,
    );
    let consumed_local_aggregate_members = derive_consumed_local_aggregate_member_bindings(
        &raw.blocks,
        &owned_locals_snapshot,
        &builder.binding_locals,
        &builder.locals,
        &builder.record_field_orders,
        &builder.enum_layouts,
        builder.type_classes.lifecycle_registry(),
    );
    // Owned handle-leaf bindings moved into an actor initial-state record
    // consumed by `SpawnActor`: the actor's `state_drop_fn` is the single free
    // site, so the source binding's standalone drop is removed. Mirrors the
    // `build_lifo_drops` skip in `elaborate` so the gate's free-count model
    // matches the drops the elaborator actually emits.
    let spawn_consumed_handle_members = derive_spawn_consumed_handle_bindings(
        &raw.blocks,
        &owned_locals_snapshot,
        &builder.binding_locals,
        &builder.locals,
    );
    let alias_field_binders = builder.alias_owner_field_binders();
    let tuple_composite_drop_allowed = derive_tuple_composite_drop_allowed(
        &raw.blocks,
        &raw.suspend_kinds,
        &owned_locals_snapshot,
        &builder.binding_locals,
        &builder.locals,
        &builder.record_field_orders,
        &builder.enum_layouts,
        builder.type_classes.lifecycle_registry(),
        &alias_field_binders,
        &builder.proven_borrow_call_args,
    );
    let mut source_excluded = returned_aggregate_members;
    source_excluded.extend(consumed_local_aggregate_members);
    source_excluded.extend(spawn_consumed_handle_members);
    // A no-retain payload/field projection of a builtin handle is a borrow of
    // the still-live aggregate owner, not a fresh close authority. The same
    // typed set suppresses its affine drop during elaboration. A real move-out
    // carries `NeutralizePayloadSlot`, which removes projection taint and keeps
    // the destination as the sole owner instead.
    let projection_alias_tainted = compute_projection_alias_taint(
        &raw.blocks,
        &builder.match_project_consumed_binder_locals,
        &builder.fresh_variant_payload_binder_locals,
        &builder.locals,
    );
    let mut borrowed_builtin_handle_projection_aliases =
        derive_borrowed_builtin_handle_projection_alias_bindings(
            &builder.binding_locals,
            &builder.locals,
            &projection_alias_tainted,
        );
    let owned_tuple_handle_projections = derive_owned_tuple_handle_projection_bindings(
        &raw.blocks,
        &owned_locals_snapshot,
        &builder.binding_locals,
        &builder.locals,
        &tuple_composite_drop_allowed,
    );
    borrowed_builtin_handle_projection_aliases
        .retain(|binding| !owned_tuple_handle_projections.contains(binding));
    source_excluded.extend(borrowed_builtin_handle_projection_aliases);
    let is_owned_record = |ty: &ResolvedTy| builder.is_owned_aggregate_record_ty(ty);
    let record_field_store_preserves_owner = |record, field_offset| {
        builder.record_field_store_preserves_record_owner(record, field_offset)
    };
    let owned_record_drop_allowed = derive_owned_record_drop_allowed(
        &raw.blocks,
        &raw.suspend_kinds,
        &owned_locals_snapshot,
        &builder.binding_locals,
        &builder.locals,
        &is_owned_record,
        &record_field_store_preserves_owner,
        &builder.record_field_orders,
        &builder.enum_layouts,
        builder.type_classes.lifecycle_registry(),
        &alias_field_binders,
        &builder.proven_borrow_call_args,
        &builder.vec_iter_projection_borrow_inits,
    );
    let mut composite_drop_allowed = tuple_composite_drop_allowed;
    composite_drop_allowed.extend(owned_record_drop_allowed);
    for check in detect_unproven_aggregate_handle_double_free(
        &raw.blocks,
        &raw.suspend_kinds,
        &owned_locals_snapshot,
        &builder.binding_locals,
        &builder.locals,
        &builder.record_field_orders,
        &builder.enum_layouts,
        &source_excluded,
        &composite_drop_allowed,
    ) {
        if let Some(diag) = check_to_diagnostic(&check) {
            diagnostics.push(diag);
        }
    }
    // RAII-1 fail-closed gate: refuse the two unsupported aggregate operations on
    // a `#[resource] #[opaque]` field — projecting it OUT of the record
    // (`let d = h.dq`, `h.dq.close()`, `f(h.dq)`) and OVERWRITING it in place
    // (`h.dq = src`). The recursive `__hew_record_drop_inplace_<R>` thunk frees
    // the field's runtime context exactly once on every exit path; an extraction
    // byte-copies the pointer-width handle with NO null-after-move so the thunk
    // AND the extracted consumer both free it (double-free), and an overwrite
    // raw-stores over the slot so the OLD handle leaks (no `close`) while `src`
    // becomes a second owner. Until overwrite-release + source-slot
    // null-after-move land (RAII-2), the compiler refuses both rather than emit
    // the leak / double-free (LESSONS boundary-fail-closed, raii-null-after-move).
    let opaque_resource_names: HashSet<String> = builder
        .lifecycle_registry
        .opaque_resources()
        .map(|lifecycle| lifecycle.resource_declaration.full_path().to_string())
        .collect();
    for check in detect_opaque_resource_field_misuse(
        &raw.blocks,
        &builder.locals,
        &builder.binding_locals,
        &opaque_resource_names,
    ) {
        if let Some(diag) = check_to_diagnostic(&check) {
            diagnostics.push(diag);
        }
    }
    // Builtin Stream/Sink/Generator/CancellationToken record fields have the
    // same overwrite hazard as opaque resources.  Their drop direction is
    // wired, but the replacement store has no source-slot neutralisation, so a
    // release-before-store alone would merely trade the old-value leak for a
    // double-close of the new handle. Refuse until a move/null protocol exists.
    for check in detect_builtin_handle_record_field_overwrite(
        &raw.blocks,
        &builder.locals,
        &builder.binding_locals,
        &builder.record_field_orders,
    ) {
        if let Some(diag) = check_to_diagnostic(&check) {
            diagnostics.push(diag);
        }
    }
    // #2654 fail-closed gate: the actor-state sibling of the record overwrite arm
    // above. An `ActorStateFieldStore` (`self.dq = src`) over a field whose drop
    // runs a real close (`Resource`, or a leaking `IoHandle`) has no
    // release-before-store in codegen, so the old handle leaks and the actor's
    // shutdown drop double-owns the new one. Reuse the actor's authoritative
    // per-field classification (`state_field_clone_kinds`) so the refusal fences
    // exactly the kinds whose drop it names. Runs for actor-handler builds only
    // (the sole functions with a `current_actor_name` + populated layout).
    if let Some(layout) = current_actor_name.and_then(|name| actor_layouts.get(name)) {
        if let Some(kinds) = layout.state_field_clone_kinds.as_deref() {
            for check in detect_actor_state_resource_overwrite(
                &raw.blocks,
                kinds,
                &layout.state_field_names,
                &layout.state_field_tys,
            ) {
                if let Some(diag) = check_to_diagnostic(&check) {
                    diagnostics.push(diag);
                }
            }
            // CAP-08 consume/extraction sibling: refuse an explicit close of a
            // handle held in actor state (the double-free the new state-held
            // Stream/Sink surface would otherwise reach). The state_drop_fn is
            // the single owner.
            for check in detect_actor_state_handle_consume(
                &raw.blocks,
                kinds,
                &layout.state_field_names,
                &layout.state_field_tys,
            ) {
                if let Some(diag) = check_to_diagnostic(&check) {
                    diagnostics.push(diag);
                }
            }
        }
    }

    LoweredFunction {
        thir,
        raw,
        checked,
        elaborated,
        diagnostics,
        generated: builder.generated_functions,
        record_layouts: builder.closure_record_layouts,
    }
}

/// The module-scoped readiness tables `is_codegen_ready_user_name` consults to
/// decide whether a user `Named` type is resolvable at the MIR boundary.
/// Bundled so the readiness-diagnostic path threads one reference instead of
/// five parallel maps (record / actor / supervisor / machine / value-class).
struct LayoutReadiness<'a> {
    record_field_orders: &'a HashMap<String, Vec<(String, ResolvedTy)>>,
    actor_layouts: &'a HashMap<String, ActorLayout>,
    supervisor_layout_map: &'a HashMap<String, crate::model::SupervisorLayout>,
    machine_layout_names: &'a HashSet<String>,
    type_classes: &'a hew_hir::TypeClassTable,
}

#[derive(Debug, Clone)]
struct LoopFrame {
    label: Option<String>,
    continue_target: u32,
    exit_target: u32,
    scope_depth: usize,
    /// Body scope of the loop — the scope id `let opt = recv()` style
    /// per-iteration bindings live in. Captured so a `continue` lowering
    /// can register its terminator block in `loop_back_edge_blocks` with
    /// the same scope filter the fall-through back-edge uses, releasing
    /// body-scope heap-owning bindings before jumping to the loop header
    /// (otherwise `let opt = rx.try_recv(); ... continue;` would leak the
    /// live `Option<string>` every iteration that took the `continue`
    /// arm — the body-end Drop sits past the `continue` and is skipped).
    body_scope: ScopeId,
}

#[derive(Debug, Clone)]
struct ActiveIterationOwner {
    scope_depth: usize,
    binding: BindingId,
    name: String,
    site: SiteId,
    ty: ResolvedTy,
}

#[derive(Debug, Clone)]
struct VecIterYieldExitDrop {
    binding: BindingId,
    place: Place,
    ty: ResolvedTy,
    kind: DropKind,
    body_start_block: u32,
    /// The body-end block already receives the normal inline drop. It is a
    /// region boundary, not part of the abandonment window.
    body_end_block: u32,
    site: SiteId,
}

/// Accumulated lexical-scope facts for one HIR `ScopeId`, built incrementally
/// while lowering a function body (see `Builder::scope_info`). `parent` is the
/// enclosing scope (the frame below this one on `active_scopes` at first
/// observation); `None` for the function-body root scope. `min_start`/`max_end`
/// widen to cover every source byte-span lowered under this scope, giving
/// codegen a source byte-range it maps to a `DILexicalBlock`'s PC extent.
#[derive(Debug, Clone, Copy)]
struct ScopeInfoEntry {
    parent: Option<ScopeId>,
    min_start: u32,
    max_end: u32,
}

/// A stable, fungible supervisor-child reference: `(supervisor, slot)`.
///
/// Stands in for a specific child INSTANCE the way OTP's `{via, Sup, Name}`
/// does — it identifies the slot/role, and the current occupant is re-resolved
/// at each use. Carries no actor pointer, so it is yield-safe to hold: after a
/// restart the next use re-resolves to the fresh child, and there is never a
/// dangling child pointer to dereference. Select/join retain only the stable
/// supervisor token and slot across their suspension boundary; `sup_place` is
/// kept solely for the existing immediate single send/ask path.
#[derive(Debug, Clone, Copy)]
struct FungibleChildRef {
    /// The supervisor handle place (`Place::ActorHandle(M)` for the
    /// `LocalPid<Supervisor>`). Supervisors are not restarted under the caller,
    /// so this place stays valid for the binding's lifetime — re-loadable at
    /// each send site.
    sup_place: Place,
    /// Stable target-word supervisor identity captured while `sup_place` is
    /// known live. Select/join carry this token, never `sup_place`, across the
    /// lowering/codegen boundary.
    supervisor_token: Place,
    /// The static child slot index within `HewSupervisor.children[]`.
    slot_index: u32,
}

/// How an owned local's release obligation is dispositioned within the
/// per-function ledger. The live drop-elaboration view
/// ([`Builder::owned_locals_snapshot`]) is exactly the `ScopeExit` entries — a
/// binding whose release is handled elsewhere (consumed, released at the end of
/// a consuming body, closed at an inner scope) is dispositioned OFF the
/// scope-exit set rather than physically removed. The whole ledger
/// ([`Builder::owned_locals_ledger`]) still carries the retracted entry, so an
/// end-of-pass scan can observe a binding whose release was handled
/// mid-lowering — the retraction-invisible class that a physical
/// `owned_locals.retain(...)` removal used to make unobservable.
/// WHY a [`Disposition::ConsumedAt`] retraction fired — the discharge authority
/// for a consume, carried on the ledger entry as data (D159/U229). Compact and
/// `Copy` so [`Disposition`] keeps its `Copy` derive (a non-`Copy` payload would
/// ripple through every by-value `Disposition` read). The match on this enum is
/// exhaustive at every consumer (no wildcard — `exhaustive-coverage`, L125).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum DischargeSite {
    /// The general consume retraction [`Builder::mark_binding_moved`] performs
    /// when a heap-owning owned local is moved out (returned, sent, or stored
    /// into a longer-lived owner). This is the only production origin today; the
    /// destination owner is not a nameable `Place` at this seam.
    BindingMoved,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Disposition {
    /// Released by the function-exit LIFO drop pass, narrowed per exit edge —
    /// the default for every entry the registration authority mints.
    ScopeExit,
    /// Released at the end of the consuming body on every exit edge rather than
    /// at function scope exit: a generator-yielded / channel-received `Some(x)`
    /// payload, or a `for x in vec` string cursor, whose per-iteration heap
    /// reference the body owns and releases each frame. Dispositioned off the
    /// scope-exit set so the function-exit LIFO pass cannot fire a second
    /// release on the same slot (double-free guard).
    BodyEndReleased,
    /// Consumed (moved out) before scope exit — the value's new owner drops it,
    /// so the scope-exit release is suppressed. A later overwrite on a different
    /// control-flow path is gated by the path-sensitive drop flag, independent
    /// of this disposition.
    ///
    /// Carries the discharge authority as data (D159/U229): `site` names WHY the
    /// consume retraction fired, and `transferee` names the new owner when it is
    /// a nameable `Place` at the retraction seam. The sole production writer
    /// ([`Builder::mark_binding_moved`]) is a general consume seam (return / send
    /// / store) with no destination local in hand, so `transferee` is `None`
    /// there; the field exists so a future consume site that DOES know its
    /// destination records it without erasing the fact. The variant is not
    /// constructible without both fields, so a caller cannot retract to
    /// `ConsumedAt` while dropping the authority (close-by-construction, U221).
    ConsumedAt {
        transferee: Option<Place>,
        site: DischargeSite,
    },
    /// Released inline when an INNER scope closes (a generator coro frame or a
    /// `VecIter` cursor handle declared in a nested block), so the release fires
    /// once per outer-loop iteration instead of accumulating to function exit.
    ScopeReleased,
    /// A byte-copy interior ALIAS of a still-live owner: the binder of a
    /// `let mid = o.mid` / `let inner = t.0` field projection whose field type
    /// is an inline aggregate (record / tuple / inline-enum). Codegen byte-copies
    /// such a field with no retain, so the binder does not own the copied heap —
    /// the projected root's composite drop frees every original exactly once.
    /// Dispositioned off the scope-exit-live set so (a) the function-exit LIFO
    /// pass emits no composite drop for the alias (a re-walk of heap the root
    /// still owns would double-free), and (b) the alias's base local is excluded
    /// from the record/tuple provers' `release_owner_bases` derivation, so an
    /// alias binder no longer trips their Defect-1 blanket every-root exclusion.
    /// The owner it aliases is named on the entry's `provenance`
    /// ([`OwnershipDecision::InteriorAlias`]-shaped). This is the recorded twin
    /// of the whole-local alias classifier
    /// [`Builder::local_storage_is_interior_alias`].
    AliasOf,
}

/// The three-way ownership class of a `let`-bound field load — the frozen
/// classification [`Builder::classify_field_load`] returns, mirroring exactly
/// what codegen emits for a `RecordFieldLoad` / `TupleFieldLoad`. Only
/// [`ByteCopyAlias`](FieldLoadClass::ByteCopyAlias) changes drop behaviour (it
/// dispositions the binder [`Disposition::AliasOf`]); the other two keep today's
/// `ScopeExit` ownership. Misassigning a class is the load-bearing double-free
/// risk, so it keys on the same facts codegen implements.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum FieldLoadClass {
    /// A `string` field: codegen `hew_string_clone`s the load, so the binder
    /// owns a fresh `+1` released by its own drop; the root keeps the original.
    Retained,
    /// An inline aggregate field (record / tuple / array / inline-enum): the load
    /// byte-copies the member with no retain, so the binder is an interior ALIAS
    /// — the projected root's composite drop frees every original exactly once.
    ByteCopyAlias,
    /// A single-pointer heap leaf (`Vec` / `bytes` / `HashMap` / `HashSet` /
    /// `Generator` / indirect-enum node): the load transfers the one owned
    /// handle, so the binder becomes the owner and the root's whole-root
    /// exclusion posture is correct.
    HandleTransfer,
}

/// One entry in the per-function owned-locals ledger: the binding whose
/// scope-exit release drop elaboration owns, recorded ONCE at its defining write
/// through [`Builder::register_owned_local`] (the single registration
/// authority) instead of pushed ad hoc at each lowering seam.
///
/// The tracked unit is the whole binding. The `(binding, name, ty)` triple is
/// the shape every downstream allow-set prover, `build_lifo_drops`, and the
/// unwired-`Vec`-element diagnostic already consume (via
/// [`Builder::owned_locals_snapshot`]); the richer facts (`ownership`,
/// `provenance`, `disposition`) are carried on the value so later drop stages
/// read a written-down fact rather than re-deriving ownership from the
/// instruction stream at each pass.
#[derive(Debug, Clone)]
pub(crate) struct ForAwaitHandleHandoff {
    pub(crate) source_binding: BindingId,
    pub(crate) cursor_binding: BindingId,
    pub(crate) handoff_block: u32,
    #[allow(
        dead_code,
        reason = "the ledger retains the source witness for diagnostics and future provenance validation"
    )]
    pub(crate) site: SiteId,
    pub(crate) ty: ResolvedTy,
}

#[derive(Debug, Clone)]
struct OwnedLocalEntry {
    /// The HIR binding this owned local backs.
    binding: BindingId,
    /// Source-level binding name (drop-statement rendering + diagnostics).
    name: String,
    /// The binding's (substituted) resolved type — the drop-shape input.
    ty: ResolvedTy,
    /// The rich ownership/drop/ABI decision classified once at registration.
    ///
    /// Recorded here so ownership is a fact written at the defining write; the
    /// provenance-aware provers and the type-directed drop-table selection in
    /// later drop-elaboration stages read it instead of re-classifying the type
    /// per pass. Not yet consumed at this stage — the drop passes still read the
    /// `(binding, name, ty)` view.
    #[allow(
        dead_code,
        reason = "written-once ownership fact; consumed by the provenance-aware \
                  provers and the type-directed drop-table selection in later \
                  drop-elaboration stages"
    )]
    ownership: ValueOwnership,
    /// Where the value's ownership traces to, when trivially proven at the
    /// defining write (an interior-alias projection of a still-live owner names
    /// its root). `None` = the value owns itself / provenance not recorded, which
    /// is today's semantics for every entry this stage mints.
    ///
    /// Not yet consumed at this stage — the interior-alias suppression that reads
    /// it is a later drop stage.
    #[allow(
        dead_code,
        reason = "carried provenance consumed by the interior-alias drop \
                  suppression in a later drop-elaboration stage"
    )]
    provenance: Option<ValueProvenance>,
    /// How this entry's release obligation is dispositioned. Minted `ScopeExit`
    /// (the function-exit LIFO pass owns it) and retracted off that set by a
    /// [`Builder::set_owned_local_disposition`] write when its release is handled
    /// mid-lowering (consumed, body-end-released, inner-scope-released).
    disposition: Disposition,
}

/// Pump context for a `receive gen fn` handler shell, set on `Builder`
/// only when `lower_function` derives `func.is_generator &&
/// call_conv == FunctionCallConv::ActorHandler` (see `Builder::stream_producer_pump`).
#[derive(Debug, Clone)]
struct StreamProducerPumpCtx {
    /// The trailing pointer-word sink parameter's local slot — always the
    /// LAST param local (`Place::Local(func.params.len() - 1)`), because
    /// `lower_actor_receive_handlers` appends the synthetic sink param after
    /// the handler's real params before calling `lower_function`.
    sink: Place,
    /// The generator's yield element type (the handler's declared `-> T`),
    /// read off the `HirExprKind::GenBlock` tail's own `yield_ty` field.
    yield_ty: ResolvedTy,
}

/// #2523 — provenance for a `match`-arm binder projected out of an enum/machine
/// payload (`lower_match_enum_tag`). The binder's storage is a byte-copy ALIAS
/// of the scrutinee's payload slot; when the binder is moved into a new owner
/// its heap ownership transfers, and the source slot must be neutralized so the
/// scrutinee's own drop no-ops on it. Recorded strictly for
/// `Place::MachineVariant`/`Place::EnumVariant` sources at the destructure site.
#[derive(Debug, Clone)]
struct ProjectedPayloadProvenance {
    /// The interior projection the binder aliases; nulled by
    /// `Instr::NeutralizePayloadSlot` at the binder's move-out.
    source_place: Place,
    /// The binder's own name, used to author the fail-closed diagnostic when
    /// the scrutinee is a re-readable place (see [`ProjectedPayloadOrigin`]).
    binder_name: String,
    /// How the scrutinee was materialised — decides whether a projected-payload
    /// move-out is soundly neutralizable at the temp, needs a consume-mark, or
    /// must be rejected fail-closed.
    origin: ProjectedPayloadOrigin,
}

/// #2523 — classification of a projected-payload binder's scrutinee, which
/// decides how a move-out of the (heap-owning) binder is made sound.
///
/// The match lowers the scrutinee into a temp before destructuring. Whether
/// nulling that temp (`Instr::NeutralizePayloadSlot`) actually neutralizes the
/// payload's *sole surviving owner* depends on how the scrutinee reached the
/// temp:
#[derive(Debug, Clone)]
enum ProjectedPayloadOrigin {
    /// The scrutinee is a bare owning binding (`match b { … }`): the match
    /// MOVES it into the temp, so the temp is the sole live copy. Neutralize
    /// the temp AND consume-mark the binding (via `MirStatement::AggregateAlias`)
    /// so a later re-read is a compile-time use-after-move rather than a read of
    /// the nulled slot.
    OwnedBinding(ProjectedScrutinee),
    /// The scrutinee is an ephemeral producer (`match f() { … }`,
    /// `match Box::Full(x) { … }`): the temp is a fresh, sole-owner value with
    /// no re-readable origin, so neutralizing the temp transfers ownership
    /// soundly with no consume-mark needed.
    EphemeralTemp,
    /// The FAIL-CLOSED default (#2523 F1/F1b/F2): the scrutinee is anything NOT
    /// proven a fresh sole owner — a re-readable *place* projection (`match h.b`,
    /// `match arr[i]`, `match self.field`), a `Block`/`If`/`Scope` wrapper whose
    /// value is a sub-expression (`match { h.b }`), a closure-CAPTURED binding
    /// (read from the env by copy, not moved into the temp), a NESTED-pattern
    /// binder (extracted through a transient copy the move cannot neutralize),
    /// or any un-enumerated / future HIR shape. A direct projection from an
    /// owned tuple (`match pair.0`) is the one explicit exception: enum-match
    /// lowering can neutralize that exact tuple field, consume-mark the tuple,
    /// and mint the match temp as its sole replacement owner. Other move-outs
    /// are REJECTED before codegen; `reason` selects the precise diagnostic.
    /// Rejecting a wrapper-hidden but otherwise-safe producer is acceptable —
    /// the safe default is to reject rather than risk aliasing. Borrow-only
    /// matches never reach this arm (they do not consume the binder).
    Reject(ProjectedPayloadRejectReason),
}

/// The re-readable scrutinee binding behind a [`ProjectedPayloadOrigin::OwnedBinding`],
/// consume-marked (via `MirStatement::AggregateAlias`) at the binder's move-out
/// so the checker rejects a later re-read as a use-after-move.
#[derive(Debug, Clone)]
struct ProjectedScrutinee {
    binding: BindingId,
    name: String,
    ty: ResolvedTy,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum RuntimeCallContext {
    Discarded,
    ValueNeeded,
}

#[derive(Debug, Clone)]
struct CaptureEnvSource {
    env: Place,
    env_ty: ResolvedTy,
    field_offset: FieldOffset,
    ty: ResolvedTy,
}

#[derive(Debug, Clone)]
struct CaptureEnvOwnedLoad {
    name: String,
    ty: ResolvedTy,
    site: SiteId,
}

impl Builder {
    /// Record a binding's declaration ordinal once.
    ///
    /// Parameters call this directly because they do not emit
    /// `MirStatement::Bind`; every statement-backed binding flows through
    /// `push_bind_statement` below.
    fn note_binding_declaration(&mut self, binding: BindingId) {
        if self.binding_declaration_seen.insert(binding) {
            self.binding_declaration_order.push(binding);
        }
    }

    /// Append a checker-stream binding declaration and update the total
    /// declaration-order ledger at the same seam.
    pub(crate) fn push_bind_statement(
        &mut self,
        binding: BindingId,
        name: String,
        site: SiteId,
        ty: ResolvedTy,
    ) {
        self.note_binding_declaration(binding);
        self.statements.push(MirStatement::Bind {
            binding,
            name,
            site,
            ty,
        });
    }

    /// Bundle this builder's module-scoped readiness tables for the
    /// codegen-readiness diagnostic gate.
    fn layout_readiness(&self) -> LayoutReadiness<'_> {
        LayoutReadiness {
            record_field_orders: &self.record_field_orders,
            actor_layouts: &self.actor_layouts,
            supervisor_layout_map: &self.supervisor_layout_map,
            machine_layout_names: &self.machine_layout_names,
            type_classes: &self.type_classes,
        }
    }

    /// Apply the per-monomorphisation substitution map to a type.
    /// Returns the input unchanged when `subst` is empty (the
    /// non-generic-function case).
    pub(crate) fn subst_ty(&self, ty: &ResolvedTy) -> ResolvedTy {
        if self.subst.is_empty() {
            return ty.clone();
        }
        hew_hir::lower::substitute_ty(ty, &self.subst)
    }

    pub(crate) fn record_layouts_for_classification(&self) -> Vec<crate::model::RecordLayout> {
        self.record_field_orders
            .iter()
            .map(|(name, fields)| crate::model::RecordLayout {
                name: name.clone(),
                field_tys: fields.iter().map(|(_, ty)| ty.clone()).collect(),
                field_names: fields.iter().map(|(fname, _)| fname.clone()).collect(),
            })
            .collect()
    }

    /// Return the type unchanged.  This compatibility seam remains at the
    /// aggregate-ownership call sites, but canonical machine instances are
    /// now registered under their exact concrete keys, so stripping generic
    /// arguments would destroy identity and is forbidden.
    #[expect(
        clippy::unused_self,
        reason = "this compatibility seam remains a Builder query at aggregate-ownership call sites"
    )]
    pub(crate) fn normalize_machine_field_ty(&self, ty: &ResolvedTy) -> ResolvedTy {
        ty.clone()
    }

    /// Allocate one `Place::Local` per function parameter and register each
    /// in `binding_locals` so that `BindingRef` expressions in the function
    /// body resolve to a real slot.
    ///
    /// Must be called BEFORE `function_body`. The allocated locals occupy
    /// `locals[0..params.len()]`; all subsequent `alloc_local` calls
    /// produce indices ≥ `params.len()`, maintaining the invariant documented
    /// on `RawMirFunction.params`.
    #[allow(
        clippy::too_many_lines,
        reason = "keeps parameter allocation, ownership registration, and typed boundary mode in one auditable pass"
    )]
    fn lower_params(&mut self, func: &HirFn) {
        // Record the by-value parameter binding ids for the destructive-
        // funcupdate base gate: a base that embeds a WHOLE parameter is a borrow,
        // not a unique owner. Captured here so every entry point that lowers a
        // parameterised body through `lower_params` participates.
        self.funcupdate_param_ids = Rc::new(func.params.iter().map(|p| p.id).collect());
        // Reserve the complete parameter-local prefix before any per-parameter
        // ownership setup allocates guard locals. A user `#[resource]`
        // parameter can allocate an affine release flag; doing that inside the
        // old single loop displaced every trailing parameter from
        // `locals[0..params.len()]`. Codegen still stored LLVM argument `i`
        // into local `i`, so a trailing bool could initialize the resource
        // flag while its actual binding remained uninitialized. Keep the
        // prefix invariant structural: all parameter slots first, all helper
        // locals second.
        for param in &func.params {
            self.note_binding_declaration(param.id);
            let slot = self.alloc_local(param.ty.clone());
            self.binding_locals.insert(param.id, slot);
            if let Place::Local(local) = slot {
                self.parameter_locals.insert(local);
            }
            self.record_local_name(slot, &param.name);
        }
        for (i, param) in func.params.iter().enumerate() {
            let slot = self.binding_locals[&param.id];
            // A fn-typed parameter's closure env is provably heap-or-null (the checker
            // `Escapes`-classifies any closure crossing a call boundary as an
            // argument), so it may transfer env ownership into an owning
            // container on store — see `closure_pair_param_owned`. Check the
            // substituted type so a monomorphised type parameter that resolves
            // to a fn type is also admitted.
            self.seed_fn_param_provenance(param);
            // RAII-2 (#1295) callee-side drop: a CONSUME affine `#[resource]`
            // parameter is OWNED by the callee (the caller moved it in and does
            // not drop it), so register it for the scope-exit drop — closing the
            // f9 leak where a by-value resource param the body did not forward
            // was never freed. The per-exit `drops_for_exit` dataflow narrowing
            // removes it on paths where the body already moved it out (returned,
            // forwarded to another consume, or a `self`-consuming method), so it
            // is dropped exactly once and never double-freed. Keyed by the
            // ORIGIN `(ItemId, index)`; a BORROW param is absent and never
            // registered (the caller keeps ownership and drops it).
            let param_is_consumed = self
                .param_ownership
                .param_consume
                .get(&(func.id, i))
                .copied()
                == Some(true);
            let owned_ty = self.subst_ty(&param.ty);
            // The authored `#[resource] close(self)` receiver is a consuming
            // ABI boundary even though its body is the close ritual itself and
            // therefore must not register a recursive callee-side scope drop.
            // Carry that distinction in the typed parameter mode so callers
            // transfer dynamic crash-cleanup authority before entry.
            let param_is_close_receiver = i == 0
                && matches!(
                    resource_drop_fn(&owned_ty, &self.type_classes),
                    Some(crate::model::DropFnSpec::UserClose(ref symbol))
                        if symbol == &self.current_function_symbol
                );
            // A generic Iterator parameter can specialise to the compiler's
            // first-class `VecIter<T>` cursor.  Its sole release authority is
            // the guarded cursor-field protocol (`scope_vec_iter_bindings`),
            // not the generic owned-carrier snapshot/drop path: treating the
            // synthetic cursor as a user record would later request an invalid
            // `RecordInPlace` drop.  It still crosses the call boundary by
            // value; only the callee-side cleanup representation is special.
            let param_is_vec_iter_cursor = self.vec_iter_cursor_release_symbol(&owned_ty).is_some();
            let param_is_owned_carrier = if param_is_vec_iter_cursor {
                false
            } else {
                self.register_owned_call_carrier_param(func.id, i, param, slot, param_is_consumed)
            };
            if param_is_vec_iter_cursor && !self.vec_iter_drop_flags.contains_key(&param.id) {
                let flag = self.alloc_local(ResolvedTy::I64);
                self.vec_iter_drop_flags.insert(param.id, flag);
                self.vec_iter_scope_owner_ledger
                    .push((param.id, owned_ty.clone()));
                self.scope_vec_iter_bindings
                    .push((func.body.scope, param.id, owned_ty.clone()));
                // A by-value parameter is the callee's cursor snapshot from
                // entry. The caller's transfer preparation has already made
                // the source unavailable on its executed path; this flag is
                // the callee-side authority for normal and abandoning exits.
                self.push_instr(Instr::ConstI64 {
                    dest: flag,
                    value: 0,
                });
            }
            if matches!(owned_ty, ResolvedTy::TraitObject { .. }) {
                // Every dyn value that crosses the function ABI owns persistent
                // heap-box storage: coercions transfer into a dyn box, dyn
                // returns preserve that box, and rebinds move the fat pointer.
                // Seed the same discriminator for a callee-owned parameter so
                // its eventual scope-exit drop runs slot 0 plus box release.
                // Borrowed dyn parameters never enter `owned_locals`, making
                // this side-table entry inert for them.
                self.dyn_trait_storage
                    .insert(param.id, TraitObjectStorage::HeapBoxed);
            }
            // A summary-owned param is one whose CALLERS consult the same
            // `call_param_owned_carrier` verdict and therefore move ownership
            // in (transfer, clone, or fail closed) — the callee owns it even
            // when registration declined (e.g. a non-clone-total plan keeps
            // the legacy transfer path). Two caller populations do NOT move
            // in despite a true summary: method callers of a stripped
            // true-receiver slot (summary removed — absent here), and
            // String/Bytes args, which the caller preparation skips onto the
            // CoW borrow spine.
            let param_summary_owned = self
                .param_ownership
                .call_param_owned_carrier
                .get(&(func.id, i))
                .copied()
                == Some(true)
                && !matches!(
                    self.subst_ty(&param.ty),
                    ResolvedTy::String | ResolvedTy::Bytes
                )
                && !self.ty_is_machine(&self.subst_ty(&param.ty));
            let mut callee_owns_param = param_is_consumed
                || param_is_close_receiver
                || param_is_owned_carrier
                || param_summary_owned
                || self.current_function_call_conv == crate::model::FunctionCallConv::ActorHandler;
            if matches!(self.subst_ty(&param.ty), ResolvedTy::Bytes)
                && !param_is_consumed
                && !param_is_owned_carrier
            {
                // Record every by-value `bytes` parameter for return/share
                // retain derivation. Ordinary calls borrow the caller's
                // reference. Actor handlers own the mailbox reference through
                // the generic `actor_message_param` registration below, but a
                // byte-copy returned from one branch still needs a fresh
                // reference because the guarded source slot drops on that
                // branch. This alias registry supplies that retain; it does not
                // itself register a scope-exit owner.
                if let Place::Local(local) = slot {
                    self.borrowed_bytes_param_locals.insert(local);
                }
            }
            if matches!(self.subst_ty(&param.ty), ResolvedTy::String)
                && !param_is_consumed
                && !param_is_owned_carrier
                && self.current_function_call_conv != crate::model::FunctionCallConv::ActorHandler
            {
                if let Place::Local(local) = slot {
                    self.borrowed_string_param_locals.insert(local);
                }
            }
            if param_is_consumed {
                // U3 — see `owner_warrant_for_owned_parameter`.
                let warrant = self.owner_warrant_for_owned_parameter(param.id, &owned_ty);
                self.register_owned_local(param.id, param.name.clone(), owned_ty.clone(), warrant);
                // Register the param in the function's top body scope so it
                // participates in the elaborator's path-sensitive drop passes
                // (forward-`Goto` scope-close + per-exit narrowing) exactly like
                // a `let`-bound resource local. Without a `binding_scope` entry
                // a param consumed on ONE branch is dropped on NO branch (the
                // scope-close pass skips unscoped bindings), leaking it on the
                // borrow path. `lower_params` runs before the body scope is
                // pushed, so set it directly rather than via the active-scope
                // cursor. Borrowed params are absent from `owned_locals` and
                // need no scope entry (the caller owns and drops them).
                self.binding_scope.insert(param.id, func.body.scope);
                // Allocate the path-sensitive runtime drop-flag for a
                // non-idempotent user `#[resource]` param consumed on SOME but
                // not all paths (a `MaybeConsumed` join at the exit). The
                // zero-init emitted here, in the param prologue, dominates every
                // later `Consume` use and every scope-exit drop; codegen gates
                // the close on `flag == 0`, so a param closed on one branch is
                // not re-closed at the merge while one left live on another
                // branch still drops exactly once. A no-op for a binding whose
                // close is idempotent/refcounted (`affine_release_needs_drop_flag`).
            }
            if param_is_consumed {
                self.maybe_alloc_affine_release_flag(param.id, &owned_ty);
            }
            let actor_message_param = self.register_actor_message_param(
                param,
                slot,
                &owned_ty,
                func.body.scope,
                param_is_consumed,
                param_is_owned_carrier,
            );
            // #2732 — callee-side drop for a by-value heap-owning ENUM COMPOSITE
            // param (`Result<T, string>`, `Option<string>`, a user enum with an
            // owned-payload variant) the body-summary classifies CONSUME: a
            // `match e { .. }` scrutinee destructures the param, so the caller
            // moved it in and does NOT drop it. `param_consume` is resource-only,
            // so such a param was never registered into `owned_locals` and its
            // heap payload leaked on every consuming path — the enum twin of the
            // record match-drain callee-drop.
            //
            // Register it into `owned_locals` + the body scope, exactly like a
            // `let`-bound local enum, so `derive_enum_composite_drop_allowed`
            // picks it up and emits the tag-aware `DropKind::EnumInPlace` shell
            // drop on every consuming path. That prover's escape scan already
            // excludes the composite when a match arm MOVES its payload out into
            // an owning sink (return / store / send / owning call), so a move-out
            // arm never double-frees the payload the binder now owns — it
            // fail-closed leaks the sibling remainder instead. Gated on
            // `call_param_consume` = CONSUME: a BORROW enum param is absent and
            // stays the caller's drop (#2735 named / #2743 temporary), mutually
            // exclusive with this callee drop.
            //
            // Records/tuples are deliberately NOT registered here — their owned
            // fields drain through per-field match binders that each own and drop
            // their extracted value, so a whole-composite drop would double-free.
            // The enum is unique in that its payload binders are non-owning
            // aliases, making the `EnumInPlace` shell drop the one balancing
            // release. `!param_is_consumed` keeps the resource path (already
            // registered above) from double-registering.
            if !param_is_consumed
                && !param_is_owned_carrier
                && !actor_message_param
                && self
                    .param_ownership
                    .call_param_consume
                    .get(&(func.id, i))
                    .is_some_and(|v| v.is_consume())
                && ty_is_heap_owning_enum_composite(
                    &owned_ty,
                    &self.record_field_orders,
                    &self.enum_layouts,
                    self.type_classes.lifecycle_registry(),
                )
            {
                self.register_owned_param(param, owned_ty.clone(), func.body.scope);
                callee_owns_param = true;
            }
            if !callee_owns_param {
                if let Place::Local(local) = slot {
                    self.borrowed_value_param_locals.insert(local);
                }
            }
            let mode = if self.current_function_call_conv
                == crate::model::FunctionCallConv::ActorHandler
                && param.id != SENTINEL_CRASH_MESSAGE_BINDING
                && !self
                    .stream_producer_pump
                    .as_ref()
                    .is_some_and(|pump| pump.sink == slot)
            {
                ParamBoundaryMode::OwnedMessage
            } else if param_is_consumed || param_is_close_receiver {
                ParamBoundaryMode::TransferResource
            } else if param_is_owned_carrier
                || ((param_summary_owned || callee_owns_param)
                    && self.binding_seeds_drop_elaboration(&owned_ty))
            {
                ParamBoundaryMode::OwnedCarrier
            } else {
                ParamBoundaryMode::BorrowReadOnly
            };
            self.param_boundary_modes.push(mode);
        }
        debug_assert_eq!(
            self.param_boundary_modes.len(),
            func.params.len(),
            "lowering must assign exactly one initial boundary mode per parameter"
        );
    }

    /// Register a heap-owning mailbox delivery as an actor-frame owner.
    ///
    /// Resource consumes and admitted call carriers are already registered and
    /// remain mutually exclusive. Synthetic runtime ABI borrows are not mailbox
    /// deliveries: a receive-gen shell's trailing sink is pump infrastructure
    /// that the pump closes explicitly, while `__crash_message` remains owned
    /// and released by the supervisor after the hook returns.
    fn register_actor_message_param(
        &mut self,
        param: &hew_hir::HirBinding,
        slot: Place,
        owned_ty: &ResolvedTy,
        scope: hew_hir::ScopeId,
        param_is_consumed: bool,
        param_is_owned_carrier: bool,
    ) -> bool {
        let actor_message_param = !param_is_consumed
            && !param_is_owned_carrier
            && self.current_function_call_conv == crate::model::FunctionCallConv::ActorHandler
            && param.id != SENTINEL_CRASH_MESSAGE_BINDING
            && !self
                .stream_producer_pump
                .as_ref()
                .is_some_and(|pump| pump.sink == slot)
            && self.binding_seeds_drop_elaboration(owned_ty);
        if actor_message_param {
            self.register_owned_param(param, owned_ty.clone(), scope);
        }
        actor_message_param
    }

    /// Register a parameter this frame's callee-side rules say the callee OWNS.
    ///
    /// U3 — the warrant is built by
    /// [`Builder::owner_warrant_for_owned_parameter`], which explains why the
    /// provenance question for a parameter is answered at the CALLER and what
    /// enforces that.
    fn register_owned_param(
        &mut self,
        param: &hew_hir::HirBinding,
        owned_ty: ResolvedTy,
        scope: hew_hir::ScopeId,
    ) {
        let warrant = self.owner_warrant_for_owned_parameter(param.id, &owned_ty);
        self.register_owned_local(param.id, param.name.clone(), owned_ty, warrant);
        self.binding_scope.insert(param.id, scope);
    }

    fn seed_fn_param_provenance(&mut self, param: &hew_hir::HirBinding) {
        if ty_is_closure_pair(&self.subst_ty(&param.ty)) {
            self.closure_pair_param_owned.insert(param.id);
            // CAP-11 provenance: the parameter's env word is not provably
            // null. If it is forwarded into a generator constructor, the
            // flat-copied env cannot release it, so the call-site gate must
            // reject that crossing.
            self.closure_pair_env_may_be_nonnull.insert(param.id);
        }
    }

    #[allow(
        clippy::needless_pass_by_value,
        reason = "alloc_local takes ty by value historically; substitution \
                  applies via subst_ty(&ty) without forcing every call site to \
                  introduce a borrow"
    )]
    pub(crate) fn alloc_local(&mut self, ty: ResolvedTy) -> Place {
        // u32::MAX locals per function is well beyond any realistic Hew
        // function size; the cast is bounded by `locals.len()` growing one
        // entry at a time within a single function-body walk.
        let id = u32::try_from(self.locals.len())
            .expect("function exceeds u32::MAX locals — impossible in Hew");
        // Substitute origin type-parameter symbols via the
        // per-monomorphisation substitution map before recording the
        // local's `ResolvedTy`. A no-op when `subst` is empty (non-
        // generic origin functions take this path with an identity
        // map).
        let resolved = self.subst_ty(&ty);
        self.locals.push(resolved);
        // Keep the name side-table in lockstep with `locals`. Anonymous
        // temporaries push `None`; named bindings overwrite it via
        // `record_local_name` after registering the slot in `binding_locals`.
        self.local_names.push(None);
        // gdb `-g` scope/decl-line side-tables stay parallel to `local_names`;
        // filled (alongside the name) in `resolve_local_names_from_binds`.
        self.local_scopes.push(None);
        self.local_decl_bytes.push(None);
        Place::Local(id)
    }

    /// Materialize a checker-selected common numeric type before a CFG value
    /// crosses into a destination slot of that type.
    ///
    /// HIR preserves each source expression's concrete type even when the
    /// checker assigns a wider/common type to the enclosing expression. A
    /// plain `Move` carries no signedness or numeric-conversion authority, so
    /// branch joins and other common-type boundaries must emit `NumericCast`
    /// explicitly. The backend then has enough type information to choose
    /// sign extension, zero extension, or an integer/float conversion.
    pub(crate) fn normalize_checker_numeric_value(
        &mut self,
        src: Place,
        source_ty: &ResolvedTy,
        target_ty: &ResolvedTy,
        role: &str,
        site: hew_hir::SiteId,
    ) -> Option<Place> {
        let source_ty = self.subst_ty(source_ty);
        let target_ty = self.subst_ty(target_ty);
        if source_ty == target_ty {
            return Some(src);
        }
        if !source_ty.can_implicitly_numeric_normalize_to(&target_ty) {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: format!(
                        "{role} type `{}` cannot normalize to checker-selected type `{}`",
                        source_ty.user_facing(),
                        target_ty.user_facing()
                    ),
                    site,
                },
                note: "checker-selected numeric joins must carry an admitted typed conversion into MIR"
                    .to_string(),
            });
            return None;
        }

        let dest = self.alloc_local(target_ty.clone());
        self.push_instr(Instr::NumericCast {
            dest,
            src,
            from_ty: source_ty,
            to_ty: target_ty,
        });
        Some(dest)
    }

    /// Record a source-level binding name for an already-allocated local
    /// slot, for `-g` variable DIEs. A no-op for any place that is not a
    /// `Place::Local` (e.g. a `Place::LambdaActorHandle` wrapping a handle id
    /// has no plain alloca to attach a `DILocalVariable` to) or whose index
    /// is out of range. Best-effort and fail-closed: an unnameable slot keeps
    /// its `None` and simply gets no DIE.
    fn record_local_name(&mut self, place: Place, name: &str) {
        if let Place::Local(id) = place {
            if let Some(slot) = self.local_names.get_mut(id as usize) {
                *slot = Some(name.to_string());
            }
        }
    }

    /// Build the `-g` lexical-block table from the incrementally-accumulated
    /// `scope_info`. Emits one [`MirScope`] per scope that is referenced by a
    /// named local OR appears on any such scope's parent chain, so codegen can
    /// parent every block up to the function-body root. A scope with no recorded
    /// byte-extent (never observed a span) is skipped — it has no instructions
    /// to scope and no local to host. Deterministic order (sorted by id) keeps
    /// the emitted DWARF stable.
    fn build_scope_table(&self) -> Vec<crate::model::MirScope> {
        use std::collections::BTreeSet;
        // Seed with every scope that hosts a named local, then walk parents.
        let mut wanted: BTreeSet<ScopeId> = self
            .local_scopes
            .iter()
            .filter_map(|s| *s)
            .filter(|s| self.scope_info.contains_key(s))
            .collect();
        let mut frontier: Vec<ScopeId> = wanted.iter().copied().collect();
        while let Some(sc) = frontier.pop() {
            if let Some(entry) = self.scope_info.get(&sc) {
                if let Some(parent) = entry.parent {
                    if self.scope_info.contains_key(&parent) && wanted.insert(parent) {
                        frontier.push(parent);
                    }
                }
            }
        }
        wanted
            .into_iter()
            .filter_map(|sc| {
                let e = self.scope_info.get(&sc)?;
                Some(crate::model::MirScope {
                    id: sc.0,
                    // Drop a parent that is not itself in the emitted set
                    // (defensive: an unobserved parent → root-level block).
                    parent: e
                        .parent
                        .filter(|p| self.scope_info.contains_key(p))
                        .map(|p| p.0),
                    start: e.min_start,
                    end: e.max_end,
                })
            })
            .collect()
    }

    /// Populate `local_names` for `let`-bound locals from the emitted
    /// `MirStatement::Bind` stream. Each `Bind` carries the source binding id
    /// and name; `binding_locals` maps that id to the slot the initialiser
    /// landed in. Resolving the two together names every `let` binding that
    /// occupies a plain `Place::Local`, uniformly, without instrumenting the
    /// many per-RHS-shape `binding_locals.insert` sites in the `Let` arm.
    /// Parameters are named directly in `lower_params`. Also fills the parallel
    /// `local_scopes` / `local_decl_bytes` side-tables for `-g` lexical scoping.
    /// Fail-closed: a binding whose final place is not a `Place::Local` (handle
    /// places) keeps its `None` and gets no DIE. Called once at finalize, before
    /// the `RawMirFunction` is built.
    fn resolve_local_names_from_binds(&mut self, blocks: &[BasicBlock]) {
        for block in blocks {
            for stmt in &block.statements {
                if let MirStatement::Bind { binding, name, .. } = stmt {
                    if let Some(place) = self.binding_locals.get(binding).copied() {
                        self.record_local_name(place, name);
                        // gdb `-g`: a shadowed inner `let first` and its outer
                        // sibling occupy DISTINCT slots (each `let` allocs a
                        // fresh local), so naming both is correct; the loss of
                        // the inner DIE happened downstream when both shared the
                        // function-wide DI scope and decl line. Carry the per-
                        // binding scope + decl-byte so codegen scopes each to
                        // its own lexical block on its own line.
                        if let Place::Local(id) = place {
                            let id = id as usize;
                            if let Some(slot) = self.local_scopes.get_mut(id) {
                                *slot = self.binding_scope.get(binding).copied();
                            }
                            if let Some(slot) = self.local_decl_bytes.get_mut(id) {
                                *slot = self.binding_decl_byte.get(binding).copied();
                            }
                        }
                    }
                }
            }
        }
    }

    /// Allocate a fresh `BasicBlock` id without switching the cursor.
    /// The caller invokes `finish_current_block(terminator)` to seal
    /// the current block, then `start_block(id)` to route subsequent
    /// `statements` / `instructions` into the new block.
    ///
    /// The very first `alloc_block` call returns id `1` because id `0`
    /// is reserved for the function's entry block (the cursor starts
    /// there at `Builder::default()`-time).
    #[allow(
        dead_code,
        reason = "Slice 1 declares cursor helpers; Slice 2 is the first caller"
    )]
    pub(crate) fn alloc_block(&mut self) -> u32 {
        // `next_block_id` starts at 0; bump to 1 the first time
        // `alloc_block` is called (id 0 is the entry block, allocated by
        // construction). After that, monotone increment.
        if self.next_block_id == 0 {
            self.next_block_id = 1;
        }
        let id = self.next_block_id;
        self.next_block_id = self
            .next_block_id
            .checked_add(1)
            .expect("function exceeds u32::MAX blocks — impossible in Hew");
        id
    }

    /// Append `instr` to the current block's backend-authority stream,
    /// recording its originating source span (when the lowering cursor is
    /// inside a statement) into the per-function `instr_spans` side-table.
    ///
    /// The key is `(current_block_id, instruction_index)`, where the index is
    /// the live `instructions.len()` BEFORE the push — which is the
    /// instruction's final position in its block, because the per-block buffer
    /// is moved out whole by `finish_current_block` / `seal_body_blocks` (order
    /// preserved). Recording at push time means the index is self-correcting:
    /// it always reflects the true position regardless of how earlier
    /// instructions in the same block were appended. Stage 2 (gdb `-g`): this
    /// is what threads the per-statement line table to codegen WITHOUT
    /// reshaping the `Instr` enum or its codegen match sites.
    pub(crate) fn push_instr(&mut self, instr: Instr) {
        if self.reject_capture_env_owned_move_instr(&instr) {
            return;
        }
        if let Instr::Move { dest, src } = &instr {
            if self.prepared_owned_call_sources.remove(src) {
                self.prepared_owned_call_sources.insert(*dest);
            }
        }
        if let Some(span) = self.current_span {
            let idx = u32::try_from(self.instructions.len()).unwrap_or(u32::MAX);
            self.instr_spans.insert((self.current_block_id, idx), span);
            self.note_scope_span(span);
        }
        self.instructions.push(instr);
    }

    fn reject_capture_env_owned_move_instr(&mut self, instr: &Instr) -> bool {
        let Instr::Move { src, .. } = instr else {
            return false;
        };
        let Some(load) = self.capture_env_owned_loads.get(src).cloned() else {
            return false;
        };
        if self.rejected_capture_env_owned_loads.insert(*src) {
            self.reject_capture_env_whole_escape(&load.name, &load.ty, load.site);
        }
        true
    }

    fn reject_buffered_capture_env_owned_moves(&mut self) {
        let instructions = std::mem::take(&mut self.instructions);
        for instr in instructions {
            if !self.reject_capture_env_owned_move_instr(&instr) {
                self.instructions.push(instr);
            }
        }
    }

    /// Seal the current basic block with `terminator` and move its
    /// statements + instructions into `pending_blocks`. The cursor is
    /// left at the just-sealed block's id; `start_block(new_id)` must
    /// be called before any further `statements.push` /
    /// `instructions.push` routes into the new block.
    #[allow(
        dead_code,
        reason = "Slice 1 declares cursor helpers; Slice 2 is the first caller"
    )]
    /// Record the [`SuspendKind`] payload of a collapsed suspension carrier in
    /// the per-function side-table, keyed by the block currently being built —
    /// the block whose terminator is (or will collapse to) the bare
    /// [`Terminator::Suspend`]. Called immediately before the matching
    /// `finish_current_block(Terminator::Suspending*)`, mirroring the
    /// `await_deadline_ns.insert(self.current_block_id, ns)` precedent.
    ///
    /// Active consuming-body values have a normal body-end release, but frame
    /// destruction while parked bypasses that edge. Mirror each safe active
    /// release into the abandon-only plan, deduplicated by storage place so a
    /// carrier and its repeated registration retain one release authority.
    fn record_suspend_kind(&mut self, kind: SuspendKind) {
        let mut recorded_places = self
            .suspend_abandon_extra_drops
            .get(&self.current_block_id)
            .into_iter()
            .flat_map(|drops| drops.iter().map(|drop| drop.place))
            .collect::<HashSet<_>>();
        let active_drops: Vec<ElabDrop> = self
            .active_generator_yield_values
            .iter()
            .filter_map(|(_, place, ty, drop_fn, start_block_id, start_instr_len)| {
                if recorded_places.contains(place) {
                    return None;
                }
                let local = base_local(*place)?;
                if !self.generator_yield_binding_drop_safe(*start_block_id, *start_instr_len, local)
                {
                    return None;
                }
                recorded_places.insert(*place);
                let kind = match drop_fn {
                    crate::model::DropFnSpec::Release("hew_rc_drop") => DropKind::RcRelease,
                    crate::model::DropFnSpec::Release("hew_weak_drop_rc") => DropKind::WeakRelease,
                    crate::model::DropFnSpec::Release(symbol) => DropKind::CowHeap {
                        release: crate::ownership::CowHeapRelease::from_symbol(symbol)?,
                    },
                    crate::model::DropFnSpec::InPlace(
                        crate::ownership::InPlaceReleaseKind::Record,
                    ) => DropKind::RecordInPlace,
                    crate::model::DropFnSpec::InPlace(
                        crate::ownership::InPlaceReleaseKind::Enum,
                    ) => DropKind::EnumInPlace,
                    crate::model::DropFnSpec::InPlace(
                        crate::ownership::InPlaceReleaseKind::AggregateRecursive,
                    ) => DropKind::AggregateRecursive,
                    crate::model::DropFnSpec::Runtime(_)
                    | crate::model::DropFnSpec::UserClose(_) => DropKind::Resource,
                };
                Some(ElabDrop {
                    place: *place,
                    ty: ty.clone(),
                    drop_fn: matches!(
                        drop_fn,
                        crate::model::DropFnSpec::Runtime(_)
                            | crate::model::DropFnSpec::UserClose(_)
                    )
                    .then(|| drop_fn.clone()),
                    kind,
                    guard: None,
                })
            })
            .collect();
        if !active_drops.is_empty() {
            self.suspend_abandon_extra_drops
                .entry(self.current_block_id)
                .or_default()
                .extend(active_drops);
        }
        self.suspend_kinds.insert(self.current_block_id, kind);
    }

    pub(crate) fn finish_current_block(&mut self, terminator: Terminator) {
        // Backstop direct buffer writes as well as `push_instr`: every block is
        // swept before sealing, so a future result-position cannot bypass the
        // guard by appending `Instr::Move` to `instructions` directly.
        self.reject_buffered_capture_env_owned_moves();
        self.record_terminator_span();
        let block = BasicBlock {
            id: self.current_block_id,
            statements: std::mem::take(&mut self.statements),
            instructions: std::mem::take(&mut self.instructions),
            terminator,
        };
        self.pending_blocks.push(block);
    }

    /// Stage 2 (gdb `-g`): attribute the span of the statement that seals the
    /// current block to the block's TERMINATOR, keyed one slot past the last
    /// instruction (`instructions.len()`). Calls and control-flow exits lower
    /// to a `Terminator`, not an `Instr`, so without this a call statement
    /// (e.g. `println(r)`) would inherit the previous instruction's line. The
    /// post-seal `shift_instr_spans_on_insert` keeps this key aligned to the
    /// final `block.instructions.len()` — exactly the key codegen looks up
    /// before lowering the terminator. No span recorded when the cursor is
    /// outside any statement (fail-closed: terminator inherits nearest loc).
    fn record_terminator_span(&mut self) {
        if let Some(span) = self.current_span {
            let idx = u32::try_from(self.instructions.len()).unwrap_or(u32::MAX);
            self.instr_spans.insert((self.current_block_id, idx), span);
            // gdb `-g`: a call statement (`println(first)`) lowers to a
            // TERMINATOR, so its span must also widen the active scope's
            // byte-extent — otherwise the inner block's range stops just short
            // of the call and the call's `DILocation` falls back to the outer
            // scope, hiding a shadowed inner binding at the call's breakpoint.
            self.note_scope_span(span);
        }
    }

    /// Move the cursor to `id`. `statements` and `instructions` must be
    /// empty before this call — typically reached by following a
    /// `finish_current_block(...)` which drains both. The new id is
    /// recorded for the next `finish_current_block` call's
    /// `BasicBlock.id` payload.
    #[allow(
        dead_code,
        reason = "Slice 1 declares cursor helpers; Slice 2 is the first caller"
    )]
    pub(crate) fn start_block(&mut self, id: u32) {
        debug_assert!(
            self.statements.is_empty() && self.instructions.is_empty(),
            "start_block must follow finish_current_block; \
             current block has {} statements / {} instructions buffered",
            self.statements.len(),
            self.instructions.len(),
        );
        self.current_block_id = id;
        // A fresh `start_block` always opens a normally-reachable cursor.
        // The early-return path that needs a dead block calls
        // `start_dead_block` instead to flag the dead-end.
        self.cursor_unreachable = false;
        self.dead_cursor_is_call_continuation = false;
    }

    /// Open a fresh continuation block whose only predecessor would be
    /// post-return source code. The block is flagged `cursor_unreachable`
    /// so `seal_body_blocks` can drop it if it remains empty when the
    /// function body finishes — preventing a synthetic Return exit with
    /// an empty drop plan from polluting `drop_plans`. Source code that
    /// lexically follows the `return` still lowers cleanly into this
    /// block; the block is kept (with no predecessor) and LLVM DCEs it.
    fn start_dead_block(&mut self, id: u32) {
        self.start_block(id);
        self.cursor_unreachable = true;
    }

    /// Seal the in-flight current block with the provided terminator and
    /// return the full `Vec<BasicBlock>` in id order.
    ///
    /// This is only HALF of finishing a body: sealed blocks still owe the
    /// ownership splice pipeline (nested temp releases, escaped sibling field
    /// discharges, carrier and outbound preparation, the null-after-move
    /// neutralizes). [`finalize_body`] is the sole caller for that reason — it
    /// seals and then runs that pipeline, so every body kind gets the same one.
    /// Calling this directly produces blocks that look finished and silently
    /// leak; `hew-mir/tests/body_finalization_seam.rs` holds the single-caller
    /// property by resolving the enclosing function of every call to this name.
    fn seal_body_blocks(&mut self, terminator: Terminator) -> Vec<BasicBlock> {
        self.reject_buffered_capture_env_owned_moves();
        let mut blocks = std::mem::take(&mut self.pending_blocks);
        // Drop a synthetic dead-end cursor (from an early-return seal)
        // when no producer has written into it — keeping the empty
        // block would surface a spurious `Return` exit in `drop_plans`
        // with an Uninit-filtered (empty) drop list, masking real
        // `cleanup-all-exits` violations on the reachable Return paths.
        // When the dead cursor accumulated content (e.g. a function-end
        // `emit_pending_defers` drained the function-scope defers into
        // it), keep the block — the content is unreachable at runtime
        // but the block must exist for the producer-side invariants
        // that drained into it to remain self-consistent.
        //
        // This drop is safe ONLY when nothing outside this dead cursor
        // references its block id — true for the `return`-seeded case
        // (`Terminator::Return` has no successor field to dangle). A dead
        // cursor seeded as a `Terminator::Call { next, .. }` continuation
        // (`dead_cursor_is_call_continuation`, hew-lang/hew#2425) is
        // different: that `Call` is already sealed into `blocks` and its
        // `next` field names this exact id, so dropping an empty block
        // here would leave it dangling. Seal it instead with a
        // compiler-proven-unreachable trap so the id stays valid — the
        // block still can never execute at runtime (a `Never`-typed
        // callee always diverges), it just can't be silently erased.
        let drop_empty_dead_cursor = self.cursor_unreachable
            && !self.dead_cursor_is_call_continuation
            && self.statements.is_empty()
            && self.instructions.is_empty();
        let seal_call_continuation_trap = self.cursor_unreachable
            && self.dead_cursor_is_call_continuation
            && self.statements.is_empty()
            && self.instructions.is_empty();
        if drop_empty_dead_cursor {
            // Clear the buffers defensively — they should already be
            // empty per the `drop_empty_dead_cursor` predicate above,
            // but keeping the state-reset explicit guards future
            // callers from observing stale `take`-leftover state.
            self.statements.clear();
            self.instructions.clear();
        } else if seal_call_continuation_trap {
            self.record_terminator_span();
            blocks.push(BasicBlock {
                id: self.current_block_id,
                statements: std::mem::take(&mut self.statements),
                instructions: std::mem::take(&mut self.instructions),
                terminator: Terminator::Trap {
                    kind: TrapKind::UnreachableCallContinuation,
                },
            });
        } else {
            self.record_terminator_span();
            let last = BasicBlock {
                id: self.current_block_id,
                statements: std::mem::take(&mut self.statements),
                instructions: std::mem::take(&mut self.instructions),
                terminator,
            };
            blocks.push(last);
        }
        self.cursor_unreachable = false;
        self.dead_cursor_is_call_continuation = false;
        // Sort by id so consumers can index by position when they want
        // RPO-ish iteration. Construction order is already monotone
        // because `alloc_block` is monotone, so this is a no-op in
        // every Slice 1 callsite (single-block) and a stable order in
        // Slice 2+.
        blocks.sort_by_key(|b| b.id);
        blocks
    }

    fn function_body(&mut self, func: &HirFn) {
        // W5.016 pre-pass: collect the record/enum layout keys used as the
        // element of an owned-Vec anywhere in this function BEFORE any `decide`
        // runs. `lower_value` calls `decide` at its entry (before the producing
        // arm), so a site-based seed would race; a function-scoped key set is
        // order-independent. Only owned-Vec element types (which carry
        // synthesizable clone/drop thunks) enter the set, so the W3.029 reject
        // stays fail-closed for every non-owned-Vec record.
        // Closure-env provenance is a transitive, flow-insensitive may-fact.
        // Repeat the shared pre-pass to a fixed point so reverse-order
        // assignments and nested-block producer chains cannot evade the
        // generator gate. Every other fact this walk records is set-valued and
        // idempotent, so repeating it adds no duplicate semantic effects.
        self.collect_prepass_facts(&func.body);
        if self.current_function_call_conv == crate::model::FunctionCallConv::ActorHandler {
            for param in &func.params {
                let ty = self.subst_ty(&param.ty);
                self.maybe_alloc_actor_message_cow_drop_flag(param.id, &ty);
            }
        }

        self.active_scopes.push(func.body.scope);
        for stmt in &func.body.statements {
            self.stmt(stmt);
        }
        if let Some(tail) = &func.body.tail {
            let returned_binding = match &tail.kind {
                HirExprKind::BindingRef {
                    resolved: ResolvedRef::Binding(binding),
                    ..
                } => Some(*binding),
                _ => None,
            };
            // Stage 2 (gdb `-g`): attribute the tail expression's instructions
            // (including the `Move` into the return slot below) to its own line
            // so a tail value-expression is a distinct step.
            self.current_span = Some((
                u32::try_from(tail.span.start).unwrap_or(u32::MAX),
                u32::try_from(tail.span.end).unwrap_or(u32::MAX),
            ));
            let value_place = self.lower_value_for_move(tail);
            self.decide(tail);
            self.mark_returned_binding_moved(tail);
            // A divergent tail leaves the cursor at an unreachable join block.
            // The canonical shape is a tail `match` whose arms ALL diverge
            // (every arm `return`s or `panic`s): each arm body already emitted
            // its own `Move { ReturnSlot <- value }` and `Return` before
            // control reached the join, and the match-lowering helpers flag
            // the join unreachable (`cursor_unreachable`) because no arm fell
            // through with a value. Such a `match` is checker-typed `Unit`
            // (each diverging arm-block is `Unit`), so its result place is the
            // `Unit` i8 stand-in — moving THAT into a non-scalar return slot
            // (ptr/struct) is the `Move type mismatch` the codegen verifier
            // rejects (#1907).
            //
            // Skip ONLY the secure Move when the cursor is unreachable. The
            // implicit `Return` marker + terminator must still be emitted: the
            // join block has live predecessors (the arm-end `Goto`s, dead at
            // runtime but present in the CFG), so it must keep a terminator —
            // dropping it would dangle those gotos. The bare `Return` over an
            // unwritten return slot is unreachable at runtime and DCE'd. A
            // value-yielding tail `match` (at least one arm falls through with
            // a value) leaves the cursor reachable and secures its result into
            // the return slot exactly as before.
            if !self.cursor_unreachable {
                // Secure the tail value into the return slot BEFORE running
                // function-scope defers. Q205-B: defer bodies observe their
                // referenced bindings at scope-exit execution time — a defer
                // that mutates a `var` named by the tail expression must not
                // be able to corrupt the returned value. Materialising the
                // Move first locks in the value the caller observes.
                if let Some(src) = value_place {
                    self.push_instr(Instr::Move {
                        dest: Place::ReturnSlot,
                        src,
                    });
                }
            }
            self.emit_pending_defers(func.body.scope);
            if !self.cursor_unreachable {
                self.emit_vec_iter_drops_for_exit_edge_except(0, returned_binding);
            }
            self.statements.push(MirStatement::Return {
                site: Some(tail.site),
                ty: self.subst_ty(&tail.ty),
            });
        } else {
            // No tail expression. The normal case is an implicit unit return
            // falling off the end of the statement list with a still-live
            // cursor — that path still needs `func.body.scope`'s defers
            // drained here, since nothing else will.
            //
            // But when the LAST statement was itself a diverging exit
            // (`return`, or any other statement-form divergence that calls
            // `start_dead_block` on its way out), the cursor is already
            // `cursor_unreachable` at this point, and that statement's own
            // handling already ran `emit_defers_for_return()` for every
            // enclosing scope including this one (see e.g. `Return(Some)`).
            // `emit_defers_for_return` deliberately CLONES rather than
            // drains `pending_defers` (a `return` inside a branch must not
            // consume defers a sibling branch still needs), so the entry is
            // still present here. Calling `emit_pending_defers` again would
            // re-lower the exact same deferred body a second time onto the
            // already-dead trailing cursor -- for a defer that itself calls
            // a `Never`-typed function (`panic()`/`exit()`), that second
            // lowering opens its own dead continuation block via
            // `start_dead_block`, which is never sealed (nothing follows the
            // function's real exit) and gets silently dropped by
            // `seal_body_blocks`' empty-dead-cursor cleanup -- while the
            // duplicate `Call` terminator that seeded it still points at the
            // now-missing block id, aborting codegen with
            // `E_CODEGEN_FRONT_FAIL_CLOSED: Call next bb<N> missing`
            // (hew-lang/hew#2426, `defer exit(42); return 0;`). Skip the
            // redundant re-drain in that case; the live-cursor drain already
            // covers every other implicit-unit-return shape.
            if !self.cursor_unreachable {
                self.emit_pending_defers(func.body.scope);
                self.emit_vec_iter_drops_for_exit_edge(0);
            }
        }
        self.active_scopes.pop();
    }

    fn push_runtime_call(&mut self, symbol: &str, args: Vec<Place>, dest: Option<Place>) {
        self.push_instr(Instr::CallRuntimeAbi(
            crate::model::RuntimeCall::new(symbol, args, dest)
                .unwrap_or_else(|_| panic!("{symbol} is an allowlisted runtime symbol")),
        ));
    }
}

/// Ownership verdict for a closure-pair operand at an owning container
/// ingress site. See `Builder::classify_closure_pair_ingress`.
enum ClosurePairIngress {
    /// A fresh pair the container may own outright (literal, call result,
    /// named-fn null-env pair, null-env binding).
    Fresh,
    /// An owned binding — the store is its move; later uses are rejected.
    OwnedBinding { id: BindingId, name: String },
    /// A binding whose ownership already transferred away; the move checker
    /// flags this read independently.
    AlreadyMoved,
    /// A borrow or unproven shape — refused (fail closed).
    Borrowed { name: Option<String> },
}

#[derive(Debug, Default)]
struct MatchBoundHopAliasFacts {
    owner_of: HashMap<u32, u32>,
    chain: Vec<(u32, u32, u32)>,
}

/// Where a record field binder's value came from, as far as the
/// intraprocedural value flow proves it. Consumed by the record
/// composite-drop prover (an escape of a provably-attributed binder excludes
/// ONLY its root instead of every record root) and by the #2212
/// sibling-discharge emitter (which additionally needs the FIELD, so only
/// `Unique` admits a discharge).
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum FieldBinderProvenance {
    /// Every defining write traces to exactly one (record root, field offset).
    Unique { root: u32, field: u32 },
    /// Every defining write traces to one record root, but to more than one
    /// of its fields.
    RootOnly { root: u32 },
    /// The binder mixes roots, or is written by something other than a
    /// member field load / binder-to-binder whole-value move. Fail-closed:
    /// consumers treat its escape as an escape of EVERY root.
    Ambiguous,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct StringRetainSite {
    block: u32,
    instr_index: usize,
    value: Place,
    condition: StringRetainCondition,
    required_bindings: Vec<BindingId>,
}

#[derive(Debug, Default)]
struct StringDropDerivation {
    allowed: HashSet<BindingId>,
    retain_sites: Vec<StringRetainSite>,
}

/// The single MIR site that produces a fresh-`string` temporary, used by
/// [`collect_nested_fresh_string_temp_drops`]. Either an `Instr` (a
/// `CallRuntimeAbi` producer like `hew_string_concat`) or the block's
/// `Terminator::Call` (a transform like `to_uppercase` / the `Vec<string>`
/// getter `hew_vec_get_str`).
#[derive(Clone, Copy)]
enum NestedDefSite {
    Instr { block: u32, idx: usize },
    Term { block: u32 },
}

/// A source-operand use site of a fresh-`string` temporary (deduplicated per
/// instruction/terminator, so `f(t, t)` is one use site, not two). The `Instr`
/// arm carries `block`/`idx` so the admission can locate the using instruction
/// and prove the def dominates it (a straight-line same-block use).
#[derive(Clone, Copy)]
enum NestedUseSite {
    Instr { block: u32, idx: usize },
    Term { block: u32 },
}

/// Per-root event ledger for [`apply_escaped_record_sibling_field_drops`]'
/// classification walk.
#[derive(Default)]
struct RootScan {
    poisoned: bool,
    /// Escape events: (block id, instruction index, field, binder). `None`
    /// marks a call terminator whose unique continuation is the post-call
    /// insertion point.
    escapes: Vec<(u32, Option<usize>, u32, u32)>,
    /// Non-escape uses of the root or its binders; `None` index = the
    /// block's terminator.
    sites: Vec<(u32, Option<usize>)>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum BytesRetainPlacement {
    Before,
    After,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct BytesRetainSite {
    block: u32,
    instr_index: usize,
    placement: BytesRetainPlacement,
    value: Place,
    required_bindings: Vec<BindingId>,
}

#[derive(Debug, Default)]
struct BytesDropDerivation {
    allowed: HashSet<BindingId>,
    retain_sites: Vec<BytesRetainSite>,
}

/// RHS-shape verdict for closure-pair env-box ownership admission at a
/// fn-typed `let` binding. See the `HirStmtKind::Let` arm for the
/// soundness argument per shape.
enum ClosurePairRhs {
    /// The binding owns a heap-or-null pair (heap-mode literal or call
    /// result) — admit into `closure_pair_owned`.
    Owned,
    /// The RHS rebinds an already-admitted binding; ownership transfers
    /// (admit the new binding, mark the carried source moved).
    TransferFrom(BindingId),
    /// Unrecognised or stack-pair shape — never admit (leak over over-free).
    NotOwned,
}

/// The function entry block id. `dataflow::analyze` seeds parameter live-state
/// at block 0 and `dataflow::compute_cooperate_sites` pins the `FunctionEntry`
/// cooperate site to block 0; the MIR builder's `seal_body_blocks` constructs the
/// entry block as id 0. This single constant keeps the elaborator's
/// FunctionEntry-cancel handling aligned with those producers.
const ENTRY_BLOCK_ID: u32 = 0;

#[cfg(test)]
use self::{
    actor_state_handle::*, cfg_util::*, composite_own::*, consts::*, drop_plan::*,
    machine_synth::*, split_consume::*, suspend_places::*, temp_drop::*,
};
