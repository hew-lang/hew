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
    HirMachineTransition, HirModule, HirNodeId, HirSelect, HirSelectArmKind, HirStmt, HirStmtKind,
    HirSupervisorChild, HirSupervisorDecl, HirVarSelfMethodTarget, IntentKind, ResolvedRef,
    ResourceMarker, ScopeId, SiteId, ValueClass,
};
use hew_parser::ast::{BinaryOp, UnaryOp};
use hew_types::{
    BuiltinType, ChildKind, ChildSlot, ExecutionContextReader, NumericMethodFamily,
    NumericMethodOp, NumericSignedness, ResolvedTy,
};

use crate::dataflow;
use crate::model::{
    ActorHandlerLayout, ActorLayout, ActorStateLoadMode, AggregateOwner, BasicBlock, BlockKind,
    CheckedMirFunction, ClosureEnvAllocation, ClosureEnvFieldInit, ClosureEnvFieldOwnership,
    CmpPred, CoalesceKeyEntry, CoalesceKeyKind, CoalesceKeyPlan, DecisionFact, DropKind, DropPlan,
    ElabBlock, ElabDrop, ElaboratedMirFunction, ExitPath, FieldOffset, FloatWidth, Instr,
    IntArithOp, IntSignedness, IrPipeline, JoinBranch, LambdaCapture, MirCheck, MirConst,
    MirConstValue, MirDiagnostic, MirDiagnosticKind, MirStatement, Place, PointerWidth,
    ProjectedPayloadRejectReason, RawMirFunction, SelectArm, SelectArmKind, SourceOrigin,
    SpawnEnvFieldOwnership, Strategy, SuspendKind, Terminator, ThirFunction, TraitObjectStorage,
    TrapKind,
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

mod actor;
mod cfg_util;
mod composite_own;
mod consts;
mod drop_plan;
mod expr;
mod facts;
mod machine_synth;
mod ownership;
mod pattern;
mod scope;
mod split_consume;
mod suspend_places;
mod temp_drop;

#[cfg(not(test))]
use self::cfg_util::{
    block_by_id, blocks_reachable_from, call_terminator_next, local_is_used_after,
    shift_instr_spans_on_insert,
};
#[cfg(not(test))]
use self::composite_own::{
    apply_escaped_record_sibling_field_drops, derive_consumed_local_aggregate_member_bindings,
    derive_enum_composite_drop_allowed, derive_local_bytes_drop_allowed,
    derive_local_collection_drop_allowed, derive_owned_record_drop_allowed,
    derive_returned_aggregate_member_bindings, derive_spawn_consumed_handle_bindings,
    derive_tuple_composite_drop_allowed, detect_actor_state_handle_consume,
    detect_actor_state_resource_overwrite, detect_opaque_resource_field_misuse,
    detect_unproven_aggregate_handle_double_free,
};
pub use self::consts::build_const_descriptors;
#[cfg(not(test))]
use self::consts::{
    actor_name_from_handle_ty, actor_name_from_remote_pid_ty, build_exit_hook_body, check_function,
    cmp_select_by_signedness, context_reader_offset, crash_action_return_ty, float_width,
    integer_bit_width, integer_signedness, is_crash_info_payload_ty, is_self_expr,
    is_string_const_ty, is_unit_close_error_result, is_unit_send_error_result,
    literal_match_scrutinee_ty, method_name_from_id, named_type_marker, numeric_method_op,
    numeric_method_signedness, recv_result_payload_ty, register_builtin_monomorphic_enum_layouts,
    register_builtin_record_layouts, runtime_symbol_for_call_expr, signed_min_value,
    unary_op_label, unresolved_fn_sig_reason,
};
pub use self::drop_plan::drop_kind_for_test_only;
#[cfg(not(test))]
use self::drop_plan::{
    binder_read_is_borrow_safe_instr, binder_read_is_borrow_safe_terminator,
    builtin_method_arg_is_move_ingress, check_to_diagnostic, classify_closure_pair_rhs,
    classify_dyn_trait_storage, cow_value_leaf_drop_symbol, describe_vec_element,
    dyn_rebind_source_binding, elaborate, exit_block_id, field_override_uses_record_field_drop,
    is_borrowing_call_abi, is_handle_borrowing_call_abi, note_payload_escape,
    render_owned_handle_ty, resource_drop_fn, resource_needs_drop_flag,
    resource_opaque_close_registry, stream_handle_drop_descriptor, ty_is_closure_pair,
    ty_is_generator_handle, ty_is_heap_owning_enum_composite, ty_is_heap_owning_tuple,
    ty_is_indirect_enum, ty_is_local_collection_handle, ty_is_nonowning_handle_leaf,
    ty_is_owned_handle_leaf, ty_is_stream_handle, ty_is_vec, validate_drop_plan,
    validate_field_drop_in_place, vec_iter_init_vec_source_expr, vec_iter_let_cursor_owns_handle,
};
pub(crate) use self::facts::*;
#[cfg(not(test))]
use self::machine_synth::{
    actor_symbol_base, build_machine_layout, build_supervisor_layout, child_init_field_is_owned,
    lower_actor_body_handlers, lower_actor_handler_layouts, lower_supervisor_bootstrap,
    machine_emit_type_id, mangle_actor_crash_handler, mangle_actor_exit_handler,
    mangle_actor_init_handler, mangle_actor_lifecycle_wrapper, mangle_actor_start_handler,
    mangle_actor_stop_handler_indexed, mangle_machine_step, mangle_supervisor_bootstrap,
    synthesize_machine_step_fn,
};
#[cfg(not(test))]
use self::split_consume::{
    alias_projection_chain_owner_seeds, attribute_field_binder_provenance, base_local,
    binding_ref_target, check_duplex_split_state, close_alias_binders_forward,
    collect_record_field_binders, descend_match_bound_hop_alias_chain,
    descend_match_bound_hop_aliases, local_is_byte_copy_aggregate, place_is_interior_projection,
    place_is_tag_read, propagate_whole_value_alias_roots, validate_cross_block_split_consume,
};
pub use self::suspend_places::instr_source_places;
pub use self::suspend_places::suspend_kind_source_places;
pub use self::suspend_places::terminator_is_suspend_carrier;
pub use self::suspend_places::terminator_source_places;
#[cfg(not(test))]
use self::suspend_places::{
    generator_yield_instr_escapes, generator_yield_terminator_escapes,
    hir_expr_contains_synthetic_vec_index, hir_expr_contains_synthetic_vec_string_index,
    instr_escape_places, option_payload_ty, place_refs_local, retained_string_terminator_drop_safe,
    terminator_escape_places,
};
#[cfg(not(test))]
use self::temp_drop::{
    apply_nested_fresh_bytes_temp_drops, apply_nested_fresh_string_temp_drops,
    bytes_interior_producer_dest, bytes_place_is_typed, bytes_runtime_arg_is_borrow,
    bytes_share_sink_places, classify_actor_state_load_modes,
    compute_collection_interior_alias_taint, compute_projection_alias_taint,
    derive_cow_fresh_borrowed_owner, derive_cow_sole_owner, finalize_bytes_ownership,
    finalize_string_local_share_intents, finalize_string_ownership,
    readmit_retained_bytes_tuple_roots, string_field_load_producer_dest,
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
const SYNTHETIC_DISCARDED_CALL_RESULT_NAME: &str = "__hew_discarded_call_result";

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
    /// Backend-authority stream for the *current* basic block. Populated
    /// in lock-step with `statements` by `lower_value` so the checker
    /// and the emitter agree on what each `SiteId` resolves to. Drained
    /// at the same cursor-move site as `statements`.
    pub(crate) instructions: Vec<Instr>,
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
    /// `finalize_blocks` drops this block from the function's CFG when
    /// it is observed to be empty, so the dead-end does not appear as
    /// an additional `Return` exit in `drop_plans`.
    pub(crate) cursor_unreachable: bool,
    /// Set alongside `cursor_unreachable` ONLY when the current dead cursor
    /// was opened as the `next` of an already-sealed `Terminator::Call`
    /// (a `Never`-typed callee's continuation) rather than from an
    /// explicit `return`'s own seal. `finalize_blocks`'s empty-dead-cursor
    /// drop is safe for the `return` case because nothing else references
    /// that block's id (`Terminator::Return` has no successor field), but
    /// a `Terminator::Call { next, .. }` DOES reference this id — dropping
    /// an empty block here would leave the already-committed `Call`
    /// pointing at a missing block (hew-lang/hew#2425:
    /// `E_CODEGEN_FRONT_FAIL_CLOSED: Call next bb<N> missing`, hit whenever
    /// the function-tail defer drain or a plain live-cursor fall-through
    /// reached a `defer exit(..)`/`defer panic(..)` and nothing else wrote
    /// into the resulting dead continuation before finalisation). When
    /// this flag is set, `finalize_blocks` seals the empty block with
    /// `Terminator::Trap { kind: UnreachableCallContinuation }` instead of
    /// dropping it, so the id stays valid. Cleared by `start_block` and by
    /// every `finalize_blocks` call, same as `cursor_unreachable`.
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
    /// Count of anonymous caller-owned temp bindings minted so far in this
    /// function. The next mint is
    /// `BindingId(SYNTHETIC_OWNED_TEMP_BINDING_BASE - count)` — a
    /// descending per-function range that stays clear of both the fixed
    /// `u32::MAX ..= u32::MAX - 4` sentinels and real HIR binding ids.
    pub(crate) synthetic_owned_temp_bindings: u32,
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
    /// Module-global interprocedural freshness summary: maps each free-function
    /// `ItemId` to whether it provably returns a FRESH MATERIALISED owner on
    /// every return path (`compute_fn_returns_fresh_owner`). Consulted by
    /// `expr_is_materialized_owner` so a `..f(args)` funcupdate base is admitted
    /// ONLY when `f` cannot launder a borrowed by-value parameter through its
    /// return (the call-returns-borrowed-param use-after-free). `Rc` so child
    /// builders share it cheaply; the empty default fails every call-base
    /// closed, which is sound. See `compute_fn_returns_fresh_owner`.
    pub(crate) funcupdate_fn_returns_fresh: Rc<HashMap<hew_hir::ItemId, bool>>,
    /// Module-global call-scrutinee return-provenance context (#2648) for the
    /// preflight admission classifier (`classify_call_scrutinee_admission`). Maps
    /// each module-fn `ItemId` to its precise three-state return provenance, the
    /// declared-extern id set, and the audited extern contract table. `Rc` so
    /// child builders share it cheaply; the empty default classifies every callee
    /// as an unknown item → interim `LegacyModuleCall` fail-open (sound —
    /// preserves today's mint, never a wrongly-Fresh admit). See
    /// `crate::return_provenance::CallScrutineeProvenance`.
    pub(crate) call_scrutinee_provenance: Rc<crate::return_provenance::CallScrutineeProvenance>,
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
    /// reference and EACH send/ask re-resolves the current live child via
    /// `hew_supervisor_child_get(sup, slot)` (the existing slot-table resolver,
    /// race-free under `children_lock`). The reference never owns a child
    /// pointer, so the stale-handle-across-yield UAF dissolves by construction
    /// and a send to a not-live child fail-closes as a recoverable error rather
    /// than a program-killing trap.
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
    /// Maps the id of a basic block that ends in one of the ten collapsed
    /// suspension carriers to the carrier's distinguishing payload
    /// ([`SuspendKind`]). Populated at each `finish_current_block(Suspending*)`
    /// site for a PURE-`{resume, cleanup}` carrier; copied into
    /// [`RawMirFunction::suspend_kinds`] at finalize. A side-table (not a
    /// carrier/Terminator field) so the carriers collapse onto one
    /// `Terminator::Suspend` while the emitted IR stays byte-identical.
    pub(crate) suspend_kinds: HashMap<u32, SuspendKind>,
    /// #2395 decision 2 — abandon-edge drops for a suspend's escape-poisoned
    /// value that the generic `drops_for_exit` `BindingState` filter cannot see.
    /// Today the sole member is the `SuspendKind::StreamSend` in-flight yield
    /// value: it is escape-poisoned (so no scope-exit drop competes on the
    /// resume path) and its resume-edge release is the pump's inline `after_send`
    /// `Instr::Drop`. Keyed by the id of the block carrying the
    /// `Terminator::Suspend` (the SAME key `suspend_kinds` uses). Appended to the
    /// matching `ExitPath::Suspend` plan AFTER `enumerate_exits`, so the value is
    /// freed exactly once on the destroy-while-parked edge — mutually exclusive
    /// with the resume-edge drop (abandon XOR resume).
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
    /// Sole-owner `for x in …` cursor (`VecIter<T>`) bindings tagged with the
    /// for-in block scope they were declared in, so a per-scope-exit
    /// `__hew_record_drop_inplace_VecIter$$T` (freeing the cursor's `vec` field
    /// via `hew_vec_free`) fires when the scope closes — releasing the handle on
    /// every outer iteration of an enclosing loop, the case the function-exit
    /// LIFO drop misses. Mirrors `scope_generator_bindings`. Registered ONLY for
    /// cursors that solely own their handle (rvalue / `to_vec()` / consumed
    /// `into_iter()` source — see `vec_iter_let_cursor_owns_handle`); a `CowShare`
    /// place source (`for x in v`) is NOT registered because the source binding
    /// keeps its own drop and the cursor only borrows (freeing here would
    /// double-free and dangle a post-loop `v` read). Entries are removed from
    /// `owned_locals` once the scope-exit drop is emitted so the function-exit
    /// pass cannot double-free; the inline `Instr::Drop` null-stores the slot as
    /// defence in depth (`raii-null-after-move`).
    pub(crate) scope_vec_iter_bindings: Vec<(ScopeId, hew_hir::BindingId, ResolvedTy)>,
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
    /// Header-defined while-let scrutinee owners active while their body is
    /// lowered. Break/continue edges consume these owners and record an
    /// explicit edge drop; returns/panic/cancellation leave them Live so the
    /// ordinary exit planner releases them.
    pub(crate) active_iteration_owners: Vec<ActiveIterationOwner>,
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
    /// Tuple records have an empty field list by design (`HirRecordDecl.fields`
    /// is empty for tuple records — their constructor is a `Call`, not a
    /// `StructInit`). They will never be looked up here.
    pub(crate) record_field_orders: HashMap<String, Vec<(String, ResolvedTy)>>,
    pub(crate) actor_layouts: HashMap<String, ActorLayout>,
    /// Supervisor-layout map, mirroring `actor_layouts` for supervisor types.
    /// Used by `lower_spawn_actor` to route `spawn Sup` to the supervisor
    /// bootstrap call and by `push_unknown_type_diagnostics` to recognise
    /// supervisor names inside `LocalPid<Sup>` type args as known. Child
    /// builders (closure shims, lambda-actor bodies, task-entry adapters)
    /// inherit it via `child_builder_tables`.
    pub(crate) supervisor_layout_map: HashMap<String, crate::model::SupervisorLayout>,
    /// Set of recognised tagged-union type names — every machine type plus
    /// the synthesised `<Machine>Event` companion enum for each. Used by
    /// `push_unknown_type_diagnostics` to silence `UnknownType` on these
    /// names and by `is_known_actor_runtime_ty` to classify their values
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
    /// `classify_actor_state_fields_with_resource_handles` at the owned-aggregate
    /// admission gate so a resource-bearing record field classifies as
    /// `StateFieldCloneKind::Resource` (RAII drop spine) instead of the
    /// no-op-drop `OpaqueHandle` (the W3.029 leak). Built from
    /// `opaque_handle_names` + `type_classes` in the Builder ctor; `IrPipeline`
    /// rebuilds the identical registry for codegen so the two never drift.
    pub(crate) resource_opaque_close: Vec<(String, String)>,
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
    /// `ResolvedTy::TraitObject` — `FrameOwned` for coercion-site
    /// RHS (`HirExprKind::CoerceToDynTrait`) and direct binding
    /// rebinds, `HeapBoxed` for call-result RHS (`HirExprKind::Call`,
    /// `CallTraitMethodStatic`, `CallDynMethod`) returning `dyn Trait`
    /// — and consumed by `build_lifo_drops` to construct the
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
    /// Populated ONLY for bindings that satisfy `resource_needs_drop_flag`
    /// (a `DropKind::Resource` whose ritual is a `DropFnSpec::UserClose`).
    /// A binding present here is KEPT in `owned_locals` across its consume
    /// (we do NOT call `mark_binding_moved` for it), so the per-exit
    /// `drops_for_exit` dataflow filter narrows the drop per control-flow
    /// path and codegen gates the surviving close on `flag == 0` — exactly
    /// once on a `MaybeConsumed` join. A user resource absent here (no flag
    /// allocated) falls back to the legacy path-insensitive
    /// `mark_binding_moved` removal: fail-closed to no-double-close (it may
    /// leak on a not-consumed branch, the pre-#1933 posture, but never
    /// double-frees the non-idempotent close).
    pub(crate) resource_drop_flags: HashMap<BindingId, Place>,
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
    /// stores a fresh value. Scope-exit drops are unaffected: owned
    /// record/string locals are released through the `elaborate` allow-set
    /// prover (`CowValue` arm), not `owned_locals` / this flag.
    pub(crate) overwrite_guard_flags: HashMap<BindingId, Place>,
    /// #2523: provenance for projected enum/machine payload binders, keyed on
    /// the binder's `BindingId`. Populated in `lower_match_enum_tag`'s binder
    /// loop for `MachineVariant`/`EnumVariant` sources; consulted at the
    /// binder's `Consume`-intent move-out to emit `NeutralizePayloadSlot` for
    /// the source slot and `AggregateAlias` for the scrutinee.
    pub(crate) projected_payload_provenance: HashMap<BindingId, ProjectedPayloadProvenance>,
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
    /// somewhere in the body. The legacy path-insensitive `mark_binding_moved`
    /// retraction at the consume site removed the binding from the scope-exit
    /// set entirely, so a binding moved out on only SOME control-flow paths
    /// (`MaybeConsumed` at the converging join) leaked on the not-moved path —
    /// the whole-value `Move` lowering does NOT null the source slot, so the
    /// moved path cannot be discriminated statically at a shared exit.
    ///
    /// A flagged binding is KEPT in `owned_locals` at its consume sites
    /// (mirroring the #1933 `resource_drop_flags` discipline): the flag is an
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
    /// Checker's per-send-site alias classification, keyed by the source span
    /// of each actor-send argument expression (same key as
    /// `TypeCheckOutput::actor_send_aliasing`). Populated by the caller
    /// (`lower_function` / `lower_hir_module_with_facts`); empty for the
    /// backward-compat `lower_hir_module` path.
    ///
    /// `lower_actor_send` looks up `SpanKey::from(&args[0].span)` here and
    /// stamps the resulting `SendAliasMode` onto `Terminator::Send.alias_mode`.
    /// **Missing entry → `SendAliasMode::Copy`** (fail-closed).
    ///
    /// LESSONS: `serializer-fail-closed` (P0) — default MUST be Copy.
    /// `copy ⊥ sendable` (P0) — derived SOLELY from this table, never from
    /// a `Copy`-marker check.
    pub(crate) actor_send_aliasing: HashMap<hew_types::SpanKey, hew_types::ActorSendAliasing>,
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
    /// is moved out whole by `finish_current_block` / `finalize_blocks`).
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
    lower_hir_module_with_facts(module, &HashMap::new(), PointerWidth::default())
}

/// Collapse a per-module-indexed `actor_send_aliasing` map to the `module_idx
/// = 0` keys MIR uses for send-site lookups (`SpanKey::from`). A byte range is
/// emitted as `Alias` only when every module's entry for it agrees on `Alias`;
/// any `Copy` classification or cross-file conflict at the same byte range is
/// dropped so the lookup misses and defaults to the safe `Copy` path.
pub(crate) fn collapse_actor_send_aliasing_to_idx0(
    map: &HashMap<hew_types::SpanKey, hew_types::ActorSendAliasing>,
) -> HashMap<hew_types::SpanKey, hew_types::ActorSendAliasing> {
    let mut all_alias: HashMap<(usize, usize), bool> = HashMap::new();
    for (k, v) in map {
        let entry = all_alias.entry((k.start, k.end)).or_insert(true);
        if !matches!(v, hew_types::ActorSendAliasing::Alias) {
            *entry = false;
        }
    }
    all_alias
        .into_iter()
        .filter(|&(_, is_alias)| is_alias)
        .map(|((start, end), _)| {
            (
                hew_types::SpanKey {
                    start,
                    end,
                    module_idx: 0,
                },
                hew_types::ActorSendAliasing::Alias,
            )
        })
        .collect()
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

/// Lower a HIR module to MIR, threading the checker's per-send-site alias
/// classification so each [`Terminator::Send`] carries the correct
/// [`crate::model::SendAliasMode`] discriminant at construction time.
///
/// This is the preferred entry point for driver glue that has access to a
/// `TypeCheckOutput`. Pass `tco.actor_send_aliasing` (or the equivalent map
/// from `CompileOutput`). The returned [`IrPipeline`] will have every
/// `Terminator::Send.alias_mode` set from the map; sites absent from the map
/// default to `SendAliasMode::Copy` (fail-closed).
///
/// `lower_hir_module` is the backward-compatible wrapper that passes an
/// empty map and is used by all existing tests; it remains correct because
/// `Copy` is the safe fallback for every send site.
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
#[allow(
    clippy::implicit_hasher,
    reason = "callers always supply a standard RandomState HashMap; \
              generalising S would require threading the type parameter through all \
              internal free functions which would be more disruptive than the lint value"
)]
pub fn lower_hir_module_with_facts(
    module: &HirModule,
    actor_send_aliasing: &HashMap<hew_types::SpanKey, hew_types::ActorSendAliasing>,
    pointer_width: PointerWidth,
) -> IrPipeline {
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

    // The checker stamps `actor_send_aliasing` with a per-module `SpanKey`
    // index (0 = root, N = N-th non-root module), but MIR looks each send up
    // by `SpanKey::from(&arg.span)` (module_idx = 0) because post-flatten HIR
    // spans carry no module identity. Without re-keying, every send inside a
    // non-root (imported/file-import) actor or fn body misses its checker fact
    // and silently degrades to the `Copy` deep-copy path. Collapse to idx-0
    // keys here, keeping `Alias` only where every module's entry for a byte
    // range agrees — any `Copy` or cross-file conflict falls back to the safe
    // `Copy` default, which is collision-safe and strictly safer than the
    // pre-module-idx idx-0 scheme.
    let collapsed_actor_send_aliasing = collapse_actor_send_aliasing_to_idx0(actor_send_aliasing);
    let actor_send_aliasing = &collapsed_actor_send_aliasing;
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
    // records have an empty field list and their constructor is a Call, not a
    // StructInit, so they never appear here.
    let mut record_field_orders: HashMap<String, Vec<(String, ResolvedTy)>> = HashMap::new();
    let mut record_layouts: Vec<crate::model::RecordLayout> = Vec::new();
    let mut actor_layouts: Vec<crate::model::ActorLayout> = Vec::new();
    let mut supervisor_layouts: Vec<crate::model::SupervisorLayout> = Vec::new();
    let mut machine_layouts: Vec<crate::model::MachineLayout> = Vec::new();
    let mut enum_layouts: Vec<crate::model::EnumLayout> = Vec::new();
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
    // Pre-scan for same-bare-name TYPE collisions across modules. Two imported
    // packages that each export a `Widget` with a divergent layout collide if
    // the layout registry keys by the bare name (last-write-wins). Only such a
    // genuine collision earns the module-qualified registry key; a type whose
    // bare name is unique in the module keeps the bare key, byte-identical to
    // the pre-qualification behaviour (so a single `pub type Ctx` constructed
    // and returned within its own module — qualified at the import boundary but
    // bare internally — is untouched and never grows a second LLVM struct).
    //
    // A name collides when two non-generic record/type decls share a bare name
    // but have distinct qualified identities (`{module}.{name}`). Generic decls
    // are excluded (they emit no bare-name layout here; their per-instantiation
    // layouts are mangled). Built once, consulted by every layout arm below.
    let collided_type_names: HashSet<String> = {
        let mut bare_to_qualified: HashMap<&str, HashSet<String>> = HashMap::new();
        for item in &module.items {
            let (bare, qualified) = match item {
                HirItem::Record(decl) if decl.type_params.is_empty() => {
                    (decl.name.as_str(), decl.qualified_name())
                }
                HirItem::TypeDecl(decl) if decl.type_params.is_empty() => {
                    (decl.name.as_str(), decl.qualified_name())
                }
                _ => continue,
            };
            bare_to_qualified.entry(bare).or_default().insert(qualified);
        }
        bare_to_qualified
            .into_iter()
            .filter(|(_, quals)| quals.len() > 1)
            .map(|(bare, _)| bare.to_string())
            .collect()
    };
    // The registry key for a monomorphic module type: its qualified identity
    // (`{module}.{name}`) ONLY when its bare name collides with another module's
    // same-bare type; otherwise the bare name (byte-identical to the pre-C1
    // behaviour). A colliding type's construction sites carry the same qualified
    // name (the Stage-2 checker stamps it), so they resolve to the right layout.
    let type_layout_key = |bare: &str, qualified: String| -> String {
        if collided_type_names.contains(bare) {
            qualified
        } else {
            bare.to_string()
        }
    };
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
                let fields: Vec<(String, ResolvedTy)> = decl
                    .fields
                    .iter()
                    .map(|f| (f.name.clone(), f.ty.clone()))
                    .collect();
                // Named-form records have a non-empty `fields` list;
                // tuple-form records have an empty list (their positional
                // layout lives on the parser's `RecordKind::Tuple`
                // discriminator and is not promoted into HIR fields). Tuple
                // records construct via `Expr::Call`, never via
                // `StructInit`, so they need no layout descriptor in this
                // slice — codegen will fail-closed on any
                // `ResolvedTy::Named` reach-through that names a tuple
                // record.
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
                if !fields.is_empty() {
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
    // `std/builtins.hew` (today: `LookupError`). These never appear in
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
    // instantiations like `Result<RemotePid<T>, LookupError>` reference
    // `Named { name: "LookupError" }` in their Err variant — that lookup
    // requires the `LookupError` layout to be registered first.
    let builtin_monomorphic_enum_names =
        register_builtin_monomorphic_enum_layouts(&mut enum_layouts);

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

    // Pre-compute the opaque handle names and resource-close registry before the
    // state-field classification pass so the classifier can distinguish plain
    // `#[opaque]` handles from `#[resource] #[opaque]` handles. The same registry
    // is moved into the `IrPipeline` below, keeping MIR admission and codegen
    // drop synthesis on one classification authority.
    let opaque_handle_names: Vec<String> = module
        .items
        .iter()
        .filter_map(|item| match item {
            HirItem::TypeDecl(decl) if decl.is_opaque => Some(decl.name.clone()),
            _ => None,
        })
        .collect();
    let resource_opaque_close =
        resource_opaque_close_registry(&opaque_handle_names, &module.type_classes);

    // Machines are enums at the value-classification layer: build every
    // machine's layout descriptor BEFORE the actor classification pass
    // (the main item loop below runs after it) and project the layouts
    // into enum-layout views, so a machine-typed actor state field
    // classifies through the same enum clone/drop authority — same
    // admissions, same refusals, no wider surface.
    for item in &module.items {
        if let HirItem::Machine(md) = item {
            machine_layouts.push(build_machine_layout(md));
        }
    }
    let machine_enum_views: Vec<crate::model::EnumLayout> =
        crate::model::machine_enum_views(&machine_layouts);
    // Generic machine instantiations canonicalize to the BARE decl name
    // across the whole substrate: one i64-defaulted layout per decl, one
    // `<Name>__step` symbol, one LLVM struct (`%Lifecycle`, never
    // `%Lifecycle$$i64`). Classification follows the same canon: an
    // all-`i64` instantiation field type is normalized to the bare name so
    // it resolves the decl-level view; any other instantiation keeps its
    // args, misses the (mangled) enum lookup, and fails closed with the
    // named classification diagnostic — matching the Move-type refusal
    // such programs already hit at codegen.
    let machine_decl_short_names: std::collections::HashSet<String> = machine_layouts
        .iter()
        .map(|m| short_name(&m.name).to_string())
        .collect();
    let normalize_machine_field_ty = |ty: &ResolvedTy| -> ResolvedTy {
        if let ResolvedTy::Named {
            name,
            args,
            builtin,
            is_opaque,
        } = ty
        {
            if !args.is_empty()
                && machine_decl_short_names.contains(short_name(name))
                && args.iter().all(|a| matches!(a, ResolvedTy::I64))
            {
                return ResolvedTy::Named {
                    name: name.clone(),
                    args: vec![],
                    builtin: *builtin,
                    is_opaque: *is_opaque,
                };
            }
        }
        ty.clone()
    };

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
            .map(|field| normalize_machine_field_ty(&field.ty))
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
        let classification = crate::state_clone::classify_actor_state_fields_with_resource_handles(
            &state_field_tys,
            &record_layouts,
            &classification_enum_layouts,
            &opaque_handle_names,
            &resource_opaque_close,
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
                        if crate::state_clone::classify_state_field_full(
                            &normalize_machine_field_ty(&field.ty),
                            &record_layouts,
                            &classification_enum_layouts,
                            &opaque_handle_names,
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
        let handlers = lower_actor_handler_layouts(actor);
        let coalesce_key_plan = resolve_coalesce_key_plan(actor, &handlers, &mut diagnostics);
        // Fail closed when receive handlers exist but no protocol descriptor
        // was attached: `lower_actor_handler_layouts` would fall back to the
        // `i32::MAX` sentinel for EVERY handler — a duplicate-switch-case
        // LLVM verify reject at two-plus handlers, and silent wire-protocol
        // corruption (wrong discriminant, in-process-only dispatch) at
        // exactly one. A known handler must never ride the sentinel; the
        // sentinel rows below exist only to keep the MIR shape well-formed
        // behind this hard error.
        if !actor.receive_handlers.is_empty() && actor.protocol_descriptor.is_none() {
            diagnostics.push(crate::model::MirDiagnostic {
                kind: crate::model::MirDiagnosticKind::ActorProtocolDescriptorMissing {
                    actor: actor.qualified_name(),
                    handler_count: actor.receive_handlers.len(),
                },
                note: format!(
                    "actor `{}` declares {} `receive fn` handler(s) but carries no \
                     protocol descriptor; message-kind discriminants cannot be \
                     assigned, so lowering refuses instead of emitting the \
                     unknown-message sentinel for known handlers",
                    actor.qualified_name(),
                    actor.receive_handlers.len()
                ),
            });
        }
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

    let mut actor_layout_map: HashMap<String, ActorLayout> = actor_layouts
        .iter()
        .cloned()
        .map(|layout| (layout.name.clone(), layout))
        .collect();
    // Bare short-name aliases for UNAMBIGUOUS module actors. An annotation-
    // derived handle type written inside the defining module
    // (`fn use(p: LocalPid<Account>)`) carries the bare inner name; alias it
    // to the module actor's layout when exactly one actor of that short name
    // exists. Root actors own the bare key outright (local-first, never
    // overwritten), and an ambiguous short name gets NO alias — a bare
    // lookup on it fails closed with the existing unknown-actor diagnostic
    // rather than silently picking a module.
    let mut module_short_name_counts: HashMap<&str, usize> = HashMap::new();
    for layout in &actor_layouts {
        if layout.defining_module.is_some() {
            *module_short_name_counts
                .entry(crate::model::short_name(&layout.name))
                .or_insert(0) += 1;
        }
    }
    for layout in &actor_layouts {
        if layout.defining_module.is_none() {
            continue;
        }
        let short = crate::model::short_name(&layout.name);
        if module_short_name_counts.get(short) == Some(&1) && !actor_layout_map.contains_key(short)
        {
            actor_layout_map.insert(short.to_string(), layout.clone());
        }
    }
    let actor_layout_map = actor_layout_map;

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
                                    let owned = child_init_field_is_owned(&source_expr.ty);
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
                                    // Owned config-field init (`string`/`Vec`/…)
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

    // Recognised tagged-union type names: every machine plus its synthesised
    // `<Machine>Event` companion. Walks `module.items` directly because the
    // `machine_layouts` Vec is not populated until the second item loop
    // below (the `HirItem::Machine` arm); we need this set ready BEFORE any
    // Builder is constructed for a fn/actor that may reference a machine
    // type. Also includes user-defined enum type names so `push_unknown_type_diagnostics`
    // and `is_known_actor_runtime_ty` accept enum-typed sites. Threaded into
    // every Builder construction site. See `Builder::machine_layout_names` doc.
    //
    // Generic enum origin names are added from `module.enum_layouts` so that
    // `is_known_actor_runtime_ty` classifies `Named { name: "Option", args: [I64] }`
    // (and the bare-name `Named { name: "Option", args: [] }` at the decl site)
    // as `BitCopy`, resolving `ValueClass::Unknown → Strategy::UnknownBlocked` for
    // generic-enum-typed locals. LESSONS: `boundary-fail-closed` (P0) — post-mono,
    // no `ResolvedTy::Named` referring to a registered generic enum may produce
    // `ValueClass::Unknown` at the MIR decision boundary.
    let machine_layout_names: HashSet<String> = module
        .items
        .iter()
        .flat_map(|item| match item {
            HirItem::Machine(md) => {
                vec![md.name.clone(), format!("{}Event", md.name)]
            }
            HirItem::TypeDecl(decl) if !decl.variants.is_empty() && decl.type_params.is_empty() => {
                vec![decl.name.clone()]
            }
            _ => vec![],
        })
        .chain(
            // Include the origin name of every generic enum instantiation so that
            // Builder::is_known_actor_runtime_ty resolves `Named { name, args }` for
            // generic enums to BitCopy regardless of the concrete type-arg list.
            module
                .enum_layouts
                .iter()
                .map(|el| el.key.origin_name.clone()),
        )
        .chain(
            // Include monomorphic builtin enums registered out-of-band above
            // (e.g. `LookupError`). Their `HirItem::TypeDecl` is intentionally
            // absent from `module.items` to keep the items list a faithful
            // mirror of user source — without this chain entry,
            // `is_known_actor_runtime_ty` would classify the builtin as
            // `ValueClass::Unknown` and the MIR fail-closed boundary would
            // reject any local typed `Named { name: "LookupError", args: [] }`.
            builtin_monomorphic_enum_names,
        )
        .collect();

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

    // Module-global interprocedural freshness summary, computed ONCE over every
    // function item (a least-fixpoint). Threaded into every body-lowering
    // builder so the destructive-funcupdate base gate can admit a `..f(args)`
    // base ONLY when `f` provably returns a fresh owner — closing the
    // call-returns-borrowed-param use-after-free. `Rc` so child builders share
    // it without re-cloning the map.
    let funcupdate_fn_returns_fresh: Rc<HashMap<hew_hir::ItemId, bool>> =
        Rc::new(compute_fn_returns_fresh_owner(&origin_fns));
    // Module-global call-scrutinee return-provenance context (#2648): the precise
    // three-state provenance fixpoint (+ the interprocedural mutation summary),
    // the declared-extern id set, and the audited extern contract table. Computed
    // ONCE and threaded (as `Rc`) into every body-lowering builder so the
    // preflight admission classifier rejects a forwarded borrowed-parameter
    // scrutinee before the from-call owner mint fires.
    let call_scrutinee_provenance: Rc<crate::return_provenance::CallScrutineeProvenance> = Rc::new(
        crate::return_provenance::build_call_scrutinee_provenance(module, &origin_fns),
    );
    // Module-global RAII-2 (#1295) param-ownership facts: which affine
    // `#[resource]` free-fn value params are CONSUME vs BORROW, and the
    // call-arg `SiteId`s whose over-stamped `Consume` intent is downgraded to a
    // borrowing `Read`. Computed ONCE over every function item (a monotone
    // least-fixpoint, keyed by origin `ItemId` so monomorphisations share one
    // verdict) and threaded into every body-lowering builder. `Rc` so child
    // builders share it without re-cloning.
    let param_ownership: Rc<ParamOwnershipFacts> = Rc::new(compute_param_ownership(
        &origin_fns,
        &module.items,
        &module.type_classes,
    ));
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
                        &funcupdate_fn_returns_fresh,
                        &call_scrutinee_provenance,
                        &param_ownership,
                        &trait_impl_index,
                        &module.call_site_type_args,
                        Some(&module.vec_generic_element_abi),
                        &module.supervisor_child_slots,
                        &module.pool_accessor_sites,
                        actor_send_aliasing,
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
                    &funcupdate_fn_returns_fresh,
                    &call_scrutinee_provenance,
                    &param_ownership,
                    &trait_impl_index,
                    &module.call_site_type_args,
                    Some(&module.vec_generic_element_abi),
                    &module.supervisor_child_slots,
                    &module.pool_accessor_sites,
                    actor_send_aliasing,
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
                    &machine_layout_names,
                    &classification_enum_layouts,
                    &opaque_handle_names,
                    &module_fn_names,
                    &module_generic_fn_names,
                    &funcupdate_fn_returns_fresh,
                    &call_scrutinee_provenance,
                    &param_ownership,
                    &module.call_site_type_args,
                    &module.supervisor_child_slots,
                    &module.pool_accessor_sites,
                    actor_send_aliasing,
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
                    &funcupdate_fn_returns_fresh,
                    &call_scrutinee_provenance,
                    &param_ownership,
                    &module.call_site_type_args,
                    &module.supervisor_child_slots,
                    &module.pool_accessor_sites,
                    actor_send_aliasing,
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
                // Synthesise the public `<Name>__step(self, event) -> Name`
                // MIR function for this machine declaration. The synthesised
                // body is a single block that fail-closes on dispatch — the
                // state×event switch tree is grown into this seam by the
                // codegen-side tagged-union layout work (plan §5). Until
                // then, every reachable call traps with a typed exit code
                // rather than silently no-op'ing or fabricating a state.
                let lowered = synthesize_machine_step_fn(
                    md,
                    &module.type_classes,
                    &record_field_orders,
                    &actor_layout_map,
                    &supervisor_layout_map,
                    &machine_layout_names,
                    &classification_enum_layouts,
                    &module_fn_names,
                    &module_generic_fn_names,
                    &funcupdate_fn_returns_fresh,
                    &call_scrutinee_provenance,
                    &param_ownership,
                    &module.call_site_type_args,
                    &module.supervisor_child_slots,
                    actor_send_aliasing,
                    pointer_width,
                );
                thir.push(lowered.thir);
                raw_mir.push(lowered.raw);
                checked_mir.push(lowered.checked);
                elaborated_mir.push(lowered.elaborated);
                diagnostics.extend(lowered.diagnostics);

                // The per-machine layout descriptor was already built by the
                // pre-classification machine-layout pass (see
                // `build_machine_layout` and the loop ahead of the actor
                // state-field classification) so machine-typed actor state
                // fields classify through the enum clone/drop path.
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
            &funcupdate_fn_returns_fresh,
            &call_scrutinee_provenance,
            &param_ownership,
            &trait_impl_index,
            &module.call_site_type_args,
            Some(&module.vec_generic_element_abi),
            &module.supervisor_child_slots,
            &module.pool_accessor_sites,
            actor_send_aliasing,
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
            malloc_string_return,
        });
    }

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
    let colliding_record_shorts: HashSet<&str> = record_layouts
        .iter()
        .filter_map(|layout| {
            let short = short_name(&layout.name);
            (record_layouts
                .iter()
                .filter(|other| short_name(&other.name) == short)
                .count()
                > 1)
            .then_some(short)
        })
        .collect();
    let resource_record_close: Vec<(String, String)> = record_layouts
        .iter()
        .filter_map(|layout| {
            let short = short_name(&layout.name);
            let entry = module
                .type_classes
                .get(layout.name.as_str())
                .or_else(|| module.type_classes.get(short))?;
            let (marker, close) = entry;
            if !matches!(marker, ResourceMarker::Resource) {
                return None;
            }
            let close_method = close.as_ref()?;
            // Colliding imported record types carry a qualified layout name and
            // a matching qualified impl symbol; unique types retain the legacy
            // bare spelling.
            let symbol_type = if colliding_record_shorts.contains(short) {
                layout.name.as_str()
            } else {
                short
            };
            let symbol = format!("{symbol_type}::{close_method}");
            Some((layout.name.clone(), symbol))
        })
        .collect();

    IrPipeline {
        thir,
        raw_mir,
        checked_mir,
        elaborated_mir,
        diagnostics,
        wire_layouts: module.wire_layouts.clone(),
        opaque_handle_names: module
            .items
            .iter()
            .filter_map(|item| match item {
                // `#[opaque]` runtime handles lower to ptr-sized slots.
                HirItem::TypeDecl(decl) if decl.is_opaque => Some(decl.name.clone()),
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
        // The caller-supplied alias classification is stored on the pipeline
        // for future codegen use (Phase P5.2). The same map was used by
        // `lower_actor_send` during HIR-to-MIR lowering above to stamp each
        // `Terminator::Send.alias_mode`; storing it here lets the P5.2
        // codegen branch consult the flat map without re-walking the MIR.
        actor_send_aliasing: actor_send_aliasing.clone(),
        polymorphic_mir,
        // Populated by `attach_lowering_facts` from `TypeCheckOutput`; empty
        // here so the lowerer does not depend on `TypeCheckOutput` directly.
        user_clone_record_seeds: Vec::new(),
        lint_warnings,
        resource_record_close,
        resource_opaque_close,
    }
}

/// Module-global RAII-2 (#1295) param-ownership facts. See
/// [`compute_param_ownership`].
#[derive(Debug, Default)]
pub(crate) struct ParamOwnershipFacts {
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
    funcupdate_fn_returns_fresh: &Rc<HashMap<hew_hir::ItemId, bool>>,
    call_scrutinee_provenance: &Rc<crate::return_provenance::CallScrutineeProvenance>,
    param_ownership: &Rc<ParamOwnershipFacts>,
    trait_impl_index: &HashMap<
        hew_hir::dispatch::TraitImplKey,
        hew_hir::dispatch::TraitImplMethodEntry,
    >,
    call_site_type_args: &HashMap<hew_hir::SiteId, Vec<ResolvedTy>>,
    vec_generic_element_abi: Option<&HashMap<hew_types::Ty, hew_types::VecElementToken>>,
    supervisor_child_slots: &HashMap<hew_hir::SiteId, ChildSlot>,
    pool_accessor_sites: &HashMap<hew_hir::SiteId, hew_types::PoolAccessor>,
    actor_send_aliasing: &HashMap<hew_types::SpanKey, hew_types::ActorSendAliasing>,
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
        resource_opaque_close: resource_opaque_close_registry(opaque_handle_names, type_classes),
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
        funcupdate_fn_returns_fresh: funcupdate_fn_returns_fresh.clone(),
        call_scrutinee_provenance: call_scrutinee_provenance.clone(),
        param_ownership: param_ownership.clone(),
        trait_impl_index: trait_impl_index.clone(),
        subst,
        call_site_type_args: call_site_type_args.clone(),
        vec_generic_element_abi: vec_generic_element_abi.cloned().unwrap_or_default(),
        supervisor_child_slots: supervisor_child_slots.clone(),
        pool_accessor_sites: pool_accessor_sites.clone(),
        actor_send_aliasing: actor_send_aliasing.clone(),
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
        compute_funcupdate_base_provenance(func, funcupdate_fn_returns_fresh);
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
    let mut blocks = builder.finalize_blocks(Terminator::Return);
    if call_conv.carries_execution_context() {
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
    // W5.011 P3 — release nested fresh-`string` temporaries (the bare-temp
    // shapes `(a + b).len()`, `s.to_uppercase().len()`, `xs[i].len()`, and the
    // discarded `a + b;`) that `derive_cow_fresh_borrowed_owner` (binding-scoped)
    // cannot see. Runs BEFORE `check_function`/elaboration so the dataflow
    // observes each inline drop as a read of its temp and codegen emits the
    // release. Fail-closed: only provably fresh, borrow-only/discarded,
    // single-predecessor-dominated temps earn an inline `hew_string_drop`.
    apply_nested_fresh_string_temp_drops(
        &mut blocks,
        &builder.suspend_kinds,
        &builder.locals,
        &builder.binding_locals,
        &mut builder.instr_spans,
    );
    // #2542 — release nested fresh-owned `bytes` user-call-result temporaries
    // (`mk().len()`, `mk().to_string()`, discarded `mk();`) that
    // `derive_local_bytes_drop_allowed` (binding-scoped) cannot see. Same
    // pre-`check_function` ordering rationale as the string splice above.
    apply_nested_fresh_bytes_temp_drops(
        &mut blocks,
        &builder.suspend_kinds,
        &builder.locals,
        &builder.binding_locals,
        &mut builder.instr_spans,
    );
    finalize_string_local_share_intents(&mut blocks, &mut builder);
    // The scope-exit-live owned-locals view, materialised once for the
    // escaped-sibling emitter and the double-free gate below — the same
    // `(binding, name, ty)` tuples they read before the ledger carried richer
    // facts.
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
        let is_owned_record = |ty: &ResolvedTy| builder.is_owned_aggregate_record_ty(ty);
        let owned_field_list = |ty: &ResolvedTy| builder.project_record_owned_field_list(ty);
        let owned_tuple_field_list = |ty: &ResolvedTy| builder.project_tuple_owned_field_list(ty);
        let field_dischargeable = |ty: &ResolvedTy| {
            matches!(ty, ResolvedTy::String) || builder.field_drop_in_place_admissible(ty)
        };
        apply_escaped_record_sibling_field_drops(
            &mut blocks,
            &builder.suspend_kinds,
            &owned_locals_snapshot,
            &builder.binding_locals,
            &builder.locals,
            &builder.record_field_orders,
            &builder.enum_layouts,
            &alias_chain,
            &is_owned_record,
            &owned_field_list,
            &owned_tuple_field_list,
            &field_dischargeable,
            &mut instr_spans,
        );
        builder.instr_spans = instr_spans;
    }
    // THIR's `statements` is the union of every block's checker stream
    // in CFG-construction order — the THIR snapshot's job is preserving
    // the pre-CFG flat-stream shape for diagnostic readers that haven't
    // been ported to block-aware iteration yet. Slice 3's per-block
    // dataflow consumes `RawMirFunction.blocks` directly and doesn't
    // touch this snapshot.
    let thir_statements: Vec<MirStatement> = blocks
        .iter()
        .flat_map(|b| b.statements.iter().cloned())
        .collect();
    let thir = ThirFunction {
        name: emit_name.clone(),
        return_ty: return_ty.clone(),
        statements: thir_statements,
    };
    // Name the `let`-bound locals from the emitted `Bind` stream before the
    // blocks are moved into the raw function — params were already named in
    // `lower_params`. Feeds the `-g` variable DIEs.
    builder.resolve_local_names_from_binds(&blocks);
    // P0 #2432 — classify every `ActorStateFieldLoad`'s own/borrow `mode`
    // over the fully finalised blocks (every splice pass above has already
    // run). Must run on THIS `blocks` local, before it moves into `raw`
    // below: codegen's per-instruction lowering loop reads
    // `RawMirFunction.blocks`, not `CheckedMirFunction.blocks` (a clone
    // taken afterward), so classifying any later would be a no-op against
    // codegen. A no-op here for every non-actor-handler lowering entry point
    // (`load_locals` is empty whenever `current_actor_state_fields` was
    // never populated).
    classify_actor_state_load_modes(&mut blocks, &builder.suspend_kinds, &builder.locals);
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
    let mut diagnostics: Vec<MirDiagnostic> = dataflow_result
        .checks
        .iter()
        .filter_map(check_to_diagnostic)
        .collect();

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

    let string_derivation = finalize_string_ownership(&mut raw, &builder, &dataflow_result);
    let bytes_derivation = finalize_bytes_ownership(&mut raw, &builder, &dataflow_result);

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
    let elaborated = elaborate(
        &checked,
        &builder,
        &thir.statements,
        &dataflow_result,
        Some(&string_derivation.allowed),
        Some(&bytes_derivation.allowed),
    );

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
    let mut source_excluded = returned_aggregate_members;
    source_excluded.extend(consumed_local_aggregate_members);
    source_excluded.extend(spawn_consumed_handle_members);
    let alias_field_binders = builder.alias_owner_field_binders();
    let tuple_composite_drop_allowed = derive_tuple_composite_drop_allowed(
        &raw.blocks,
        &raw.suspend_kinds,
        &owned_locals_snapshot,
        &builder.binding_locals,
        &builder.locals,
        &builder.record_field_orders,
        &builder.enum_layouts,
        &alias_field_binders,
    );
    let is_owned_record = |ty: &ResolvedTy| builder.is_owned_aggregate_record_ty(ty);
    let owned_record_drop_allowed = derive_owned_record_drop_allowed(
        &raw.blocks,
        &raw.suspend_kinds,
        &owned_locals_snapshot,
        &builder.binding_locals,
        &builder.locals,
        &is_owned_record,
        &builder.record_field_orders,
        &builder.enum_layouts,
        &alias_field_binders,
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
        .resource_opaque_close
        .iter()
        .map(|(name, _)| name.clone())
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
/// restart the next send re-resolves to the fresh child, and there is never a
/// dangling child pointer to dereference.
#[derive(Debug, Clone, Copy)]
struct FungibleChildRef {
    /// The supervisor handle place (`Place::ActorHandle(M)` for the
    /// `LocalPid<Supervisor>`). Supervisors are not restarted under the caller,
    /// so this place stays valid for the binding's lifetime — re-loadable at
    /// each send site.
    sup_place: Place,
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
    ConsumedAt,
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
    /// `match pair.0`, `match arr[i]`, `match self.field`), a `Block`/`If`/
    /// `Scope` wrapper whose value is a sub-expression (`match { h.b }`), a
    /// closure-CAPTURED binding (read from the env by copy, not moved into the
    /// temp), a NESTED-pattern binder (extracted through a transient copy the
    /// move cannot neutralize), or any un-enumerated / future HIR shape. The
    /// move-out is REJECTED before codegen; `reason` selects the precise
    /// diagnostic. Rejecting a wrapper-hidden but otherwise-safe producer is
    /// acceptable — the safe default is to reject rather than risk aliasing.
    /// Borrow-only matches never reach this arm (they do not consume the binder).
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

impl Builder {
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

    /// Strip type args from a machine-typed field when the outer name is a
    /// known machine decl and every arg is `i64`.  This mirrors the
    /// `normalize_machine_field_ty` closure used in the actor-state
    /// classification path (lower.rs ~1513): generic machine instantiations
    /// are canonicalised to the bare decl name because the machine layout is
    /// always registered under that bare name, never mangled.
    ///
    /// Non-machine types and non-all-i64 instantiations are returned unchanged
    /// so they fail closed through the normal classification miss.
    pub(crate) fn normalize_machine_field_ty(&self, ty: &ResolvedTy) -> ResolvedTy {
        if let ResolvedTy::Named {
            name,
            args,
            builtin,
            is_opaque,
        } = ty
        {
            // Strip type args only for machine decl types, never for generic
            // enums (Option, Result, etc.).  Both sets are present in
            // `machine_layout_names` — machine names are added directly from
            // HirItem::Machine, and generic enum origin names are added from
            // module.enum_layouts so that `is_known_actor_runtime_ty` resolves
            // them as BitCopy.  The distinguishing property: every generic enum
            // instantiation has at least one EnumLayout whose name begins with
            // `{short_name}$$` (the mangled form), while machine types never
            // produce a `$$`-mangled layout entry (machines always register
            // under the bare decl name).  If we stripped args from a generic
            // enum, the bare-name lookup would miss the mangled layout (e.g.
            // "Option" instead of "Option$$i64") and produce MissingRecordLayout
            // on any record with an Option<i64> field.
            let sname = short_name(name);
            let is_generic_enum_origin = self
                .enum_layouts
                .iter()
                .any(|el| el.name.starts_with(&format!("{sname}$$")));
            if !args.is_empty()
                && args.iter().all(|a| matches!(a, ResolvedTy::I64))
                && !is_generic_enum_origin
                && machine_layout_name_matches(&self.machine_layout_names, name)
            {
                return ResolvedTy::Named {
                    name: name.clone(),
                    args: vec![],
                    builtin: *builtin,
                    is_opaque: *is_opaque,
                };
            }
        }
        ty.clone()
    }

    /// True when `ty` may be admitted as a generator-env capture field: a
    /// plain, recursively-copyable value — every leaf `BitCopy` AND the whole
    /// transitively free of `#[opaque]` runtime handles.
    ///
    /// The generator's env record is heap-copied once, at construction
    /// (`Terminator::MakeGenerator`'s `hew_cont_frame_alloc` + flat `memcpy`),
    /// and the coro ramp reads that heap copy by address across every suspend
    /// — no per-field clone or drop protocol exists for it. An admitted value
    /// must therefore be safe to bit-copy with no ownership consequence.
    ///
    /// Three shapes are admitted:
    ///   1. `ValueClass::BitCopy` scalars (i64/f64/bool/char/…), actor pids, and
    ///      `#[copy]` records — provided they are transitively free of `#[opaque]`
    ///      runtime handles (gate 2 below).
    ///   2. `ResolvedTy::Function { .. }` — a fn-typed value. Safety rests on
    ///      four properties: (a) `Terminator::MakeGenerator` flat-`memcpy`s
    ///      the whole env record once at construction, and the companion's
    ///      release (`hew_gen_coro_destroy`) only frees that flat buffer —
    ///      neither recurses into inner pointers, so a non-null env-box is not
    ///      freed by the generator; (b) `Function` is `PersistentShare`, so
    ///      the body side never drops the fn value; (c) a named fn literal
    ///      produced by the compiler has a null env word; and (d) a capturing
    ///      closure that structurally unifies with `fn(..)` (via `unify.rs`)
    ///      is REFUSED at the generator-constructor call boundary
    ///      (`generator_arg_laundered_closure` in `lower_direct_call`). That
    ///      gate tracks local producer provenance and fails closed for fn-typed
    ///      parameters and call results whose env word is not provably null.
    ///   3. `ResolvedTy::Closure { captures, .. }` where `captures.is_empty()` —
    ///      an empty-capture closure. No heap env box exists to alias. Closures
    ///      with non-empty captures carry a heap-boxed env; flat-copying them would
    ///      shallow-alias the caller's heap → double-free / UAF at generator
    ///      teardown. That case is REJECTED here; admitting it takes the
    ///      clone-into-env protocol (`genfn-owned-captures`).
    ///
    /// Rejected shapes (fail closed):
    ///   * `ResolvedTy::Closure { captures, .. }` with non-empty `captures`.
    ///   * `ResolvedTy::TraitObject` — a `{data, vtable}` fat pointer with a heap
    ///     data box; NOT null-env-safe.
    ///   * owned / non-`BitCopy` values — `string`/`Bytes`/`Vec`/`HashMap`,
    ///     owned records, `Generator`, `CancellationToken`, etc.
    ///   * `#[opaque]`-only handles (classifies as `BitCopy` but aliases a
    ///     runtime resource on copy).
    ///
    /// A scalar (`i64`/`f64`/`bool`/`char`/…), an actor pid (a non-owning
    /// by-value reference), a `#[copy]` record of such fields, a bare named fn,
    /// and an empty-capture closure all answer `true` (admissible). Every other
    /// shape answers `false` (fail closed).
    fn gen_env_capture_admissible(&self, ty: &ResolvedTy) -> bool {
        // Fast-path: admit a fn-typed value (`fn(..)->R`). Safety is guaranteed
        // by flat-free semantics (generator runtime never recurses into inner
        // pointers), PersistentShare no-drop on the body side, and the
        // generator-constructor argument gate (`generator_arg_laundered_closure`)
        // refusing a capturing closure laundered through `fn(..)` at the call
        // boundary. Its provenance ledger rejects fn-typed parameters and call
        // results whose env word is not provably null.
        if matches!(ty, ResolvedTy::Function { .. }) {
            return true;
        }
        // Admit an empty-capture closure: same null-env guarantee.
        // A closure with non-empty captures carries a heap-boxed env; reject it
        // (admitting it takes the `genfn-owned-captures` clone-into-env protocol).
        if let ResolvedTy::Closure { captures, .. } = ty {
            return captures.is_empty();
        }
        // For all other types, require BitCopy AND transitively opaque-free.
        if ValueClass::of_ty(ty, &self.type_classes) != ValueClass::BitCopy {
            return false;
        }
        let record_layouts = self.record_layouts_for_classification();
        !crate::model::ty_contains_unclonable_opaque_with_names(
            ty,
            &record_layouts,
            &self.enum_layouts,
            &self.opaque_handle_names,
        )
    }

    /// Allocate one `Place::Local` per function parameter and register each
    /// in `binding_locals` so that `BindingRef` expressions in the function
    /// body resolve to a real slot.
    ///
    /// Must be called BEFORE `function_body`. The allocated locals occupy
    /// `locals[0..params.len()]`; all subsequent `alloc_local` calls
    /// produce indices ≥ `params.len()`, maintaining the invariant documented
    /// on `RawMirFunction.params`.
    fn lower_params(&mut self, func: &HirFn) {
        // Record the by-value parameter binding ids for the destructive-
        // funcupdate base gate: a base that embeds a WHOLE parameter is a borrow,
        // not a unique owner. Captured here so every entry point that lowers a
        // parameterised body through `lower_params` participates.
        self.funcupdate_param_ids = Rc::new(func.params.iter().map(|p| p.id).collect());
        for (i, param) in func.params.iter().enumerate() {
            let slot = self.alloc_local(param.ty.clone());
            self.binding_locals.insert(param.id, slot);
            if let Place::Local(local) = slot {
                self.parameter_locals.insert(local);
            }
            // Record the parameter name for `-g` `DW_TAG_formal_parameter`
            // DIEs (codegen's `create_parameter_variable`).
            self.record_local_name(slot, &param.name);
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
            if matches!(self.subst_ty(&param.ty), ResolvedTy::Bytes) && !param_is_consumed {
                if let Place::Local(local) = slot {
                    self.borrowed_bytes_param_locals.insert(local);
                }
            }
            if matches!(self.subst_ty(&param.ty), ResolvedTy::String)
                && !param_is_consumed
                && self.current_function_call_conv != crate::model::FunctionCallConv::ActorHandler
            {
                if let Place::Local(local) = slot {
                    self.borrowed_string_param_locals.insert(local);
                }
            }
            if param_is_consumed {
                let owned_ty = self.subst_ty(&param.ty);
                self.register_owned_local(param.id, param.name.clone(), owned_ty.clone());
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
                // close is idempotent/refcounted (`resource_needs_drop_flag`).
                self.maybe_alloc_resource_drop_flag(param.id, &owned_ty);
            }
        }
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
    /// is moved out whole by `finish_current_block` / `finalize_blocks` (order
    /// preserved). Recording at push time means the index is self-correcting:
    /// it always reflects the true position regardless of how earlier
    /// instructions in the same block were appended. Stage 2 (gdb `-g`): this
    /// is what threads the per-statement line table to codegen WITHOUT
    /// reshaping the `Instr` enum or its codegen match sites.
    pub(crate) fn push_instr(&mut self, instr: Instr) {
        if let Some(span) = self.current_span {
            let idx = u32::try_from(self.instructions.len()).unwrap_or(u32::MAX);
            self.instr_spans.insert((self.current_block_id, idx), span);
            self.note_scope_span(span);
        }
        self.instructions.push(instr);
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
    fn record_suspend_kind(&mut self, kind: SuspendKind) {
        self.suspend_kinds.insert(self.current_block_id, kind);
    }

    pub(crate) fn finish_current_block(&mut self, terminator: Terminator) {
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
    /// so `finalize_blocks` can drop it if it remains empty when the
    /// function body finishes — preventing a synthetic Return exit with
    /// an empty drop plan from polluting `drop_plans`. Source code that
    /// lexically follows the `return` still lowers cleanly into this
    /// block; the block is kept (with no predecessor) and LLVM DCEs it.
    fn start_dead_block(&mut self, id: u32) {
        self.start_block(id);
        self.cursor_unreachable = true;
    }

    /// Finalise the function's CFG by sealing the in-flight current
    /// block with the provided terminator. Returns the full
    /// `Vec<BasicBlock>` in id order. Slice 1 always returns a singleton
    /// because no caller invokes `finish_current_block`/`start_block`
    /// during the function-body walk; Slice 2's `If` lowering is the
    /// first writer to produce a non-trivial CFG here.
    fn finalize_blocks(&mut self, terminator: Terminator) -> Vec<BasicBlock> {
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

        self.active_scopes.push(func.body.scope);
        for stmt in &func.body.statements {
            self.stmt(stmt);
        }
        if let Some(tail) = &func.body.tail {
            // Stage 2 (gdb `-g`): attribute the tail expression's instructions
            // (including the `Move` into the return slot below) to its own line
            // so a tail value-expression is a distinct step.
            self.current_span = Some((
                u32::try_from(tail.span.start).unwrap_or(u32::MAX),
                u32::try_from(tail.span.end).unwrap_or(u32::MAX),
            ));
            let value_place = self.lower_value(tail);
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
            // `finalize_blocks`' empty-dead-cursor cleanup -- while the
            // duplicate `Call` terminator that seeded it still points at the
            // now-missing block id, aborting codegen with
            // `E_CODEGEN_FRONT_FAIL_CLOSED: Call next bb<N> missing`
            // (hew-lang/hew#2426, `defer exit(42); return 0;`). Skip the
            // redundant re-drain in that case; the live-cursor drain already
            // covers every other implicit-unit-return shape.
            if !self.cursor_unreachable {
                self.emit_pending_defers(func.body.scope);
            }
        }
        self.active_scopes.pop();
    }

    /// Lower `let PAT = scrutinee else { <divergent block> };`.
    ///
    /// Mirrors `lower_if_let`'s tag-test CFG, with two deliberate differences:
    ///   1. The success-path payload bindings are inserted into
    ///      `binding_locals` and NEVER restored — they escape into the
    ///      enclosing scope so the rest of the block can read them.
    ///   2. There is no join/result place. On a match, control flows straight
    ///      into the continuation block (the cursor) with the binders live. On
    ///      a mismatch, the else block runs; it is divergent (the checker
    ///      proved `Ty::Never`), so it seals its own block with a diverging
    ///      terminator and the continuation is reached only via the match edge.
    #[allow(
        clippy::too_many_lines,
        reason = "mirrors lower_if_let's tag-test CFG builder; splitting would require threading many intermediate block ids across helper boundaries"
    )]
    fn lower_let_else_stmt(
        &mut self,
        scrutinee: &HirExpr,
        variant_idx: u32,
        bindings: &[hew_hir::HirMatchArmBinding],
        success_prelude: &[hew_hir::HirStmt],
        payload_variant_predicates: &[hew_hir::HirPayloadVariantPredicate],
        else_body: &hew_hir::HirBlock,
    ) {
        // #2648 preflight — run BEFORE any allocation or scrutinee lowering. A
        // reject pushes one diagnostic and returns with no partial MIR; the
        // admission token also gates the #2429 from-call owner mint below.
        let scrutinee_admission = match self.classify_call_scrutinee_admission(scrutinee) {
            Ok(admission) => admission,
            Err(diag) => {
                self.diagnostics.push(*diag);
                return;
            }
        };
        // Entry: evaluate scrutinee, load tag, branch.
        let Some(scrutinee_place) = self.lower_value(scrutinee) else {
            return;
        };
        let scrutinee_local = match scrutinee_place {
            Place::Local(n) => n,
            other => {
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::NotYetImplemented {
                        construct: "let-else scrutinee place shape".to_string(),
                        site: scrutinee.site,
                    },
                    note: format!(
                        "let-else scrutinee must lower to Place::Local; got {other:?}. \
                         The HIR producer should only emit LetElse for enum-typed \
                         scrutinees backed by a local slot"
                    ),
                });
                return;
            }
        };

        // #2429 — give a FROM-CALL enum-composite let-else scrutinee an owner,
        // symmetric with `lower_match_enum_tag`/`lower_if_let`. Registered BEFORE
        // the branch and the payload-predicate checks that route to `else_bb`, so
        // the synthetic owned local is live on BOTH paths: the match path (payload
        // moved out into the escaping binders, shell composite drop) and the
        // divergent else path (`return`/`break`/`continue`/`panic` — no move-out,
        // so the FULL temp drops once on that edge). The scope-exit drop
        // elaboration frees it on whichever edge leaves the enclosing scope,
        // including the divergent-else edges. No-op for the non-Call / carrier
        // shapes per `register_from_call_scrutinee_owner`.
        self.register_from_call_scrutinee_owner(scrutinee_admission, scrutinee, scrutinee_local);

        let tag_local = self.alloc_local(ResolvedTy::I64);
        self.push_instr(Instr::Move {
            dest: tag_local,
            src: Place::EnumTag(scrutinee_local),
        });
        let k_local = self.alloc_local(ResolvedTy::I64);
        self.push_instr(Instr::ConstI64 {
            dest: k_local,
            value: i64::from(variant_idx),
        });
        let cond_local = self.alloc_local(ResolvedTy::Bool);
        self.push_instr(Instr::IntCmp {
            pred: crate::model::CmpPred::Eq,
            lhs: tag_local,
            rhs: k_local,
            dest: cond_local,
        });

        let bind_bb = self.alloc_block();
        let else_bb = self.alloc_block();
        let cont_bb = self.alloc_block();

        self.finish_current_block(Terminator::Branch {
            cond: cond_local,
            then_target: bind_bb,
            else_target: else_bb,
        });

        // Match path: bind the payload fields into the ENCLOSING scope's
        // binding_locals and DO NOT restore — they escape the statement. Then
        // Goto the continuation, where subsequent statements lower with the
        // binders live.
        self.start_block(bind_bb);
        // #2523 — classify the scrutinee once for the whole let-else so every
        // heap-owning payload binder routes its move-out through the shared
        // default-deny consume policy (see `classify_scrutinee_origin`).
        let scrutinee_origin = self.classify_scrutinee_origin(scrutinee);
        let mut nested_binding_jobs: Vec<(u32, u32, hew_hir::HirMatchArmBinding)> = Vec::new();
        for pvp in payload_variant_predicates {
            // A failed nested check routes to the else block, same as a
            // top-level tag mismatch.
            if self
                .emit_payload_variant_predicate_checks(
                    pvp,
                    scrutinee_local,
                    variant_idx,
                    else_bb,
                    scrutinee.site,
                    &mut nested_binding_jobs,
                )
                .is_none()
            {
                return;
            }
        }

        for binding in bindings {
            let binding_ty = self.subst_ty(&binding.ty);
            self.statements.push(MirStatement::Bind {
                binding: binding.binding,
                name: binding.name.clone(),
                site: scrutinee.site,
                ty: binding_ty.clone(),
            });
            self.record_binding_scope(binding.binding);
            let keep_for_drop_elab = self.binding_seeds_drop_elaboration(&binding_ty);
            if keep_for_drop_elab {
                self.register_owned_local(
                    binding.binding,
                    binding.name.clone(),
                    binding_ty.clone(),
                );
            }
            let dest = self.alloc_local(binding.ty.clone());
            self.push_instr(Instr::Move {
                dest,
                src: Place::MachineVariant {
                    local: scrutinee_local,
                    variant_idx,
                    field_idx: binding.field_idx,
                },
            });
            // Escape: insert into binding_locals and never restore.
            self.binding_locals.insert(binding.binding, dest);
            // #2523 — record provenance for a heap-owning TOP-LEVEL let-else
            // payload binder so its move-out routes through default-deny.
            self.record_projected_payload_provenance(
                binding.binding,
                &binding.name,
                Place::MachineVariant {
                    local: scrutinee_local,
                    variant_idx,
                    field_idx: binding.field_idx,
                },
                scrutinee_origin.clone(),
                keep_for_drop_elab,
            );
        }
        for (src_local, src_variant_idx, binding) in nested_binding_jobs {
            let binding_ty = self.subst_ty(&binding.ty);
            self.statements.push(MirStatement::Bind {
                binding: binding.binding,
                name: binding.name.clone(),
                site: scrutinee.site,
                ty: binding_ty.clone(),
            });
            self.record_binding_scope(binding.binding);
            let keep_for_drop_elab = self.binding_seeds_drop_elaboration(&binding_ty);
            if keep_for_drop_elab {
                self.register_owned_local(
                    binding.binding,
                    binding.name.clone(),
                    binding_ty.clone(),
                );
            }
            let dest = self.alloc_local(binding.ty.clone());
            self.push_instr(Instr::Move {
                dest,
                src: Place::MachineVariant {
                    local: src_local,
                    variant_idx: src_variant_idx,
                    field_idx: binding.field_idx,
                },
            });
            self.binding_locals.insert(binding.binding, dest);
            // #2523 F2 — nested let-else binder bound from a transient predicate
            // copy; reject a heap-owning move-out fail-closed.
            self.record_projected_payload_provenance(
                binding.binding,
                &binding.name,
                Place::MachineVariant {
                    local: src_local,
                    variant_idx: src_variant_idx,
                    field_idx: binding.field_idx,
                },
                ProjectedPayloadOrigin::Reject(ProjectedPayloadRejectReason::NestedDestructure),
                keep_for_drop_elab,
            );
        }
        // Aggregate-payload destructure (`Ok((n, s))`): the prelude `Let`
        // statements project the synthetic `__payload_*` temp into the leaf
        // binders (`n`, `s`). They run on the SUCCESS path after the top-level
        // payload fields bind, and like those fields their binding_locals are
        // inserted (by the normal `Let` lowering) and never restored, so the
        // leaf binders escape into the enclosing scope for the continuation.
        for stmt in success_prelude {
            self.stmt(stmt);
        }
        self.finish_current_block(Terminator::Goto { target: cont_bb });

        // No-match path: run the divergent else block. The checker proved it
        // has type `Ty::Never`, so its body seals the block with a diverging
        // terminator (Return/Trap). Defensive Goto for malformed HIR where the
        // else somehow falls through — the cursor is unreachable in the
        // well-formed case.
        self.start_block(else_bb);
        self.active_scopes.push(else_body.scope);
        for stmt in &else_body.statements {
            self.stmt(stmt);
        }
        if let Some(tail) = &else_body.tail {
            let _ = self.lower_value(tail);
        }
        self.emit_pending_defers(else_body.scope);
        self.active_scopes.pop();
        self.finish_current_block(Terminator::Goto { target: cont_bb });

        // Continuation: subsequent statements lower here, with the escaped
        // binders live in binding_locals.
        self.start_block(cont_bb);
    }

    fn actor_state_field_for_target(&self, expr: &HirExpr) -> Option<(FieldOffset, ResolvedTy)> {
        match &expr.kind {
            HirExprKind::BindingRef { name, .. } => {
                self.current_actor_state_fields.get(name).cloned()
            }
            HirExprKind::FieldAccess { object, field } if is_self_expr(object) => {
                self.current_actor_state_fields.get(field).cloned()
            }
            _ => None,
        }
    }

    /// Lower `while cond { body }` to a three-block CFG:
    ///
    /// ```text
    /// entry_bb (current):
    ///   Goto header_bb
    ///
    /// header_bb:
    ///   cond_place = lower(condition)
    ///   Branch { cond: cond_place, then: body_bb, else: exit_bb }
    ///
    /// body_bb:
    ///   lower(body statements)
    ///   Goto header_bb          ← back-edge
    ///
    /// exit_bb:
    ///   (subsequent lowering continues here)
    /// ```
    ///
    /// The while expression always has type Unit; `None` is returned
    /// (no value Place) matching the semantics of a statement-level loop.
    fn lower_while(
        &mut self,
        label: Option<&str>,
        condition: &HirExpr,
        body: &hew_hir::HirBlock,
    ) -> Option<Place> {
        let header_bb = self.alloc_block();
        let body_bb = self.alloc_block();
        let exit_bb = self.alloc_block();

        // Entry: unconditional jump to the header (condition check).
        self.finish_current_block(Terminator::Goto { target: header_bb });

        // Header: evaluate condition and branch.
        self.start_block(header_bb);
        let cond_place = self.lower_value(condition)?;
        self.finish_current_block(Terminator::Branch {
            cond: cond_place,
            then_target: body_bb,
            else_target: exit_bb,
        });

        // Body: lower statements then loop back.
        self.start_block(body_bb);
        // continue → header (re-checks the condition); break → exit.
        let loop_scope_depth = self.active_scopes.len();
        self.loop_stack.push(LoopFrame {
            label: label.map(str::to_string),
            continue_target: header_bb,
            exit_target: exit_bb,
            scope_depth: loop_scope_depth,
            body_scope: body.scope,
        });
        self.active_scopes.push(body.scope);
        for stmt in &body.statements {
            self.stmt(stmt);
        }
        if let Some(tail) = &body.tail {
            let _ = self.lower_value(tail);
        }
        self.emit_pending_defers(body.scope);
        // Release generators declared in the loop body before the back-edge so
        // a `let g = gen()` (consumed or not) frees its coro frame + heap
        // companion every iteration rather than accumulating one per pass (see
        // `emit_scope_generator_drops`).
        self.emit_scope_generator_drops(body.scope);
        // #1949 — release sole-owner `for x in …` cursors (`VecIter`) declared
        // directly in this loop body before the back-edge, the cursor analogue of
        // the generator release above (see `emit_scope_vec_iter_drops`).
        self.emit_scope_vec_iter_drops(body.scope);
        self.emit_scope_stream_drops(body.scope);
        // Record this block as a loop-body back-edge so `enumerate_exits`
        // populates its `Goto` `DropPlan` with per-iteration releases for
        // heap-owning bindings declared in `body.scope`. Without this, an
        // `Option<T>` (or other heap-owning) let-binding bound inside the body
        // gets overwritten on the next iteration with no preceding drop — the
        // memory leak this fix closes (Stream<T>/Receiver<T> recv loops).
        // The scope captured is the body's, not any nested block's: nested
        // block-scope bindings already self-drop when their block closes via
        // the existing scope-exit pass.
        self.loop_back_edge_blocks
            .insert(self.current_block_id, body.scope);
        self.active_scopes.pop();
        self.loop_stack.pop();
        self.finish_current_block(Terminator::Goto { target: header_bb });

        // Exit: subsequent lowering continues here.
        self.start_block(exit_bb);
        None
    }

    /// Lower `while let <Ctor>(bindings) = scrutinee { body }` to a four-block
    /// CFG that re-evaluates the scrutinee each iteration and dispatches on
    /// the resulting enum tag:
    ///
    /// ```text
    /// entry_bb (current):
    ///   Goto header_bb
    ///
    /// header_bb:
    ///   scrutinee_place = lower(scrutinee)        ← re-evaluated each iter
    ///   tag_local = Move { src: Place::EnumTag(scrutinee_local) }
    ///   k_local = ConstI64(variant_idx)
    ///   cond_local = IntCmp(Eq, tag_local, k_local)
    ///   Branch { cond: cond_local, then: body_bb, else: exit_bb }
    ///
    /// body_bb:
    ///   for each binding:
    ///     dest = Move { src: Place::MachineVariant { scrutinee_local,
    ///                                                variant_idx, field_idx } }
    ///     register binding_locals[binding.id] = dest
    ///   lower(body)
    ///   Goto header_bb                            ← back-edge
    ///
    /// exit_bb:
    ///   (subsequent lowering continues here)
    /// ```
    ///
    /// The CFG is a hybrid of `lower_while` (header/body/exit shape) and
    /// `lower_match_enum_tag` (tag-compare + payload extraction). The
    /// scrutinee is evaluated inside the header block so each loop iteration
    /// produces a fresh enum value — matching the surface semantics of
    /// `while let` re-checking the pattern on every pass.
    ///
    /// The expression always has type `Unit`; `None` is returned (no value
    /// Place) matching `lower_while`.
    fn lower_while_let(
        &mut self,
        label: Option<&str>,
        scrutinee: &HirExpr,
        variant_idx: u32,
        bindings: &[hew_hir::HirMatchArmBinding],
        payload_variant_predicates: &[hew_hir::HirPayloadVariantPredicate],
        body: &hew_hir::HirBlock,
    ) -> Option<Place> {
        // Two-line addition for back-edge `DropPlan` plumbing tips this fn
        // past clippy's 100-line bar; the algorithm is one coherent unit and
        // factoring it would obscure the loop CFG.
        #![allow(
            clippy::too_many_lines,
            reason = "back-edge DropPlan plumbing tipped this past clippy's 100-line bar; \
                      the algorithm is one coherent unit and factoring it would obscure \
                      the loop CFG"
        )]
        // #2648 preflight — run BEFORE any block allocation or scrutinee lowering.
        // A reject leaves no half-built loop CFG.
        let scrutinee_admission = match self.classify_call_scrutinee_admission(scrutinee) {
            Ok(admission) => admission,
            Err(diag) => {
                self.diagnostics.push(*diag);
                return None;
            }
        };
        // Fail-closed: a `while let` over an enum variant that leaves an OWNED
        // payload field unaccounted for (a `..` rest or a bare `_` sibling)
        // cannot release that field's heap on the loop back-edge. The header
        // re-evaluates the scrutinee every iteration, and the skipped owner has
        // no per-iteration release site — a reassigned scrutinee leaks the
        // prior iteration's skipped payload. The bound and nested-matched
        // siblings ride the composite/back-edge drop; a skipped owned field
        // does not. `match`, `if let`, `let ... else`, and record/tuple
        // destructure DO discharge this shape (the enum composite drop / the
        // field-drop safety loop), so this refusal is `while let`-specific.
        // Run before any block allocation so a reject leaves no partial CFG.
        if let Some((idx, field_ty)) = self.while_let_skipped_owned_payload_field(
            scrutinee,
            variant_idx,
            bindings,
            payload_variant_predicates,
        ) {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "while-let skipped owned enum payload field".to_string(),
                    site: scrutinee.site,
                },
                note: format!(
                    "payload field {idx} (`{}`) of this variant is owned but neither \
                     bound nor matched, so `while let` cannot release it on the loop \
                     back-edge — each re-entry would leak the prior iteration's value. \
                     Bind every owned payload field explicitly (use a name instead of \
                     `_`, and list every field instead of `..`), or destructure with \
                     `match` / `if let` / `let ... else`, which release the skipped \
                     owned sibling through the enum composite drop",
                    field_ty.user_facing(),
                ),
            });
            return None;
        }
        let header_bb = self.alloc_block();
        let body_bb = self.alloc_block();
        let exit_bb = self.alloc_block();

        // Entry → header.
        self.finish_current_block(Terminator::Goto { target: header_bb });

        // Header: re-evaluate scrutinee, load enum tag, compare to variant_idx,
        // branch to body or exit.
        self.start_block(header_bb);
        let scrutinee_place = self.lower_value(scrutinee)?;
        let scrutinee_local = match scrutinee_place {
            Place::Local(n) => n,
            other => {
                // Fail closed: a poisoned scrutinee shape leaves no half-built
                // CFG (same fail-closed pattern as `lower_match_enum_tag`).
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::NotYetImplemented {
                        construct: "while-let scrutinee place shape".to_string(),
                        site: scrutinee.site,
                    },
                    note: format!(
                        "while-let scrutinee must lower to Place::Local; got {other:?}. \
                         The HIR producer should only emit WhileLet for enum-typed scrutinees \
                         backed by a local slot"
                    ),
                });
                return None;
            }
        };
        let scrutinee_owner = self.register_from_call_scrutinee_owner(
            scrutinee_admission,
            scrutinee,
            scrutinee_local,
        );
        let false_cleanup_bb = scrutinee_owner.as_ref().map(|_| self.alloc_block());

        // Load the variant tag into a fresh i64 local, mirroring
        // `lower_match_enum_tag`. `Place::EnumTag(local)` is the substrate
        // primitive that codegen GEPs to outer-struct field 0; widening from
        // the per-enum tag width to i64 happens inside the Move arm.
        let tag_local = self.alloc_local(ResolvedTy::I64);
        self.push_instr(Instr::Move {
            dest: tag_local,
            src: Place::EnumTag(scrutinee_local),
        });

        // Compare tag against the continue-arm variant index.
        let k_local = self.alloc_local(ResolvedTy::I64);
        self.push_instr(Instr::ConstI64 {
            dest: k_local,
            value: i64::from(variant_idx),
        });
        let cond_local = self.alloc_local(ResolvedTy::Bool);
        self.push_instr(Instr::IntCmp {
            pred: crate::model::CmpPred::Eq,
            lhs: tag_local,
            rhs: k_local,
            dest: cond_local,
        });
        self.finish_current_block(Terminator::Branch {
            cond: cond_local,
            then_target: body_bb,
            else_target: false_cleanup_bb.unwrap_or(exit_bb),
        });

        // Body: bind payload fields, lower body, loop back to header.
        // The binding writes use `Place::MachineVariant` (same primitive used
        // by `lower_match_enum_tag` arm-body entry); MIR codegen GEPs to the
        // variant payload field.
        //
        // Bindings live for the body block only — we save and restore any
        // pre-existing entries in `binding_locals` so nested while-let loops
        // can shadow the same name without confusion.
        self.start_block(body_bb);
        let mut nested_binding_jobs: Vec<(u32, u32, hew_hir::HirMatchArmBinding)> = Vec::new();
        for pvp in payload_variant_predicates {
            self.emit_payload_variant_predicate_checks(
                pvp,
                scrutinee_local,
                variant_idx,
                false_cleanup_bb.unwrap_or(exit_bb),
                scrutinee.site,
                &mut nested_binding_jobs,
            )?;
        }

        let mut overwritten_bindings =
            Vec::with_capacity(bindings.len() + nested_binding_jobs.len());
        // #2523 — classify the while-let scrutinee once so every heap-owning
        // payload binder routes its move-out through the shared default-deny
        // consume policy. The loop back-edge re-reads the scrutinee every
        // iteration, so an owning-binding move-out MUST become a compile-time
        // use-after-move (the canonical #2523 shape).
        let scrutinee_origin = self.classify_scrutinee_origin(scrutinee);
        for binding in bindings {
            let binding_ty = self.subst_ty(&binding.ty);
            self.statements.push(MirStatement::Bind {
                binding: binding.binding,
                name: binding.name.clone(),
                site: scrutinee.site,
                ty: binding_ty.clone(),
            });
            self.record_binding_scope(binding.binding);
            let keep_for_drop_elab = self.binding_seeds_drop_elaboration(&binding_ty);
            if keep_for_drop_elab {
                self.register_owned_local(
                    binding.binding,
                    binding.name.clone(),
                    binding_ty.clone(),
                );
            }
            let dest = self.alloc_local(binding.ty.clone());
            self.push_instr(Instr::Move {
                dest,
                src: Place::MachineVariant {
                    local: scrutinee_local,
                    variant_idx,
                    field_idx: binding.field_idx,
                },
            });
            let previous = self.binding_locals.insert(binding.binding, dest);
            if let Some(local) = base_local(dest) {
                self.transient_local_scopes.insert(local, body.scope);
            }
            overwritten_bindings.push((binding.binding, previous));
            // #2523 — record provenance for a heap-owning TOP-LEVEL while-let
            // payload binder so its move-out routes through default-deny.
            self.record_projected_payload_provenance(
                binding.binding,
                &binding.name,
                Place::MachineVariant {
                    local: scrutinee_local,
                    variant_idx,
                    field_idx: binding.field_idx,
                },
                scrutinee_origin.clone(),
                keep_for_drop_elab,
            );
        }
        for (src_local, src_variant_idx, binding) in nested_binding_jobs {
            let binding_ty = self.subst_ty(&binding.ty);
            self.statements.push(MirStatement::Bind {
                binding: binding.binding,
                name: binding.name.clone(),
                site: scrutinee.site,
                ty: binding_ty.clone(),
            });
            self.record_binding_scope(binding.binding);
            let keep_for_drop_elab = self.binding_seeds_drop_elaboration(&binding_ty);
            if keep_for_drop_elab {
                self.register_owned_local(
                    binding.binding,
                    binding.name.clone(),
                    binding_ty.clone(),
                );
            }
            let dest = self.alloc_local(binding.ty.clone());
            self.push_instr(Instr::Move {
                dest,
                src: Place::MachineVariant {
                    local: src_local,
                    variant_idx: src_variant_idx,
                    field_idx: binding.field_idx,
                },
            });
            let previous = self.binding_locals.insert(binding.binding, dest);
            if let Some(local) = base_local(dest) {
                self.transient_local_scopes.insert(local, body.scope);
            }
            overwritten_bindings.push((binding.binding, previous));
            // #2523 F2 — nested while-let binder bound from a transient predicate
            // copy; reject a heap-owning move-out fail-closed.
            self.record_projected_payload_provenance(
                binding.binding,
                &binding.name,
                Place::MachineVariant {
                    local: src_local,
                    variant_idx: src_variant_idx,
                    field_idx: binding.field_idx,
                },
                ProjectedPayloadOrigin::Reject(ProjectedPayloadRejectReason::NestedDestructure),
                keep_for_drop_elab,
            );
        }

        self.active_scopes.push(body.scope);
        // continue → header (re-evaluates scrutinee + tag); break → exit.
        let loop_scope_depth = self.active_scopes.len() - 1;
        self.loop_stack.push(LoopFrame {
            label: label.map(str::to_string),
            continue_target: header_bb,
            exit_target: exit_bb,
            scope_depth: loop_scope_depth,
            body_scope: body.scope,
        });
        let active_iteration_owner_mark = self.active_iteration_owners.len();
        if let Some((binding, ty)) = &scrutinee_owner {
            self.active_iteration_owners.push(ActiveIterationOwner {
                scope_depth: self.active_scopes.len(),
                binding: *binding,
                name: SYNTHETIC_CALL_SCRUTINEE_NAME.to_string(),
                site: scrutinee.site,
                ty: ty.clone(),
            });
        }
        for stmt in &body.statements {
            self.stmt(stmt);
        }
        if let Some(tail) = &body.tail {
            let _ = self.lower_value(tail);
        }
        self.emit_pending_defers(body.scope);
        // Release generators declared in the loop body before the back-edge
        // (per-iteration `hew_gen_coro_destroy`; see `emit_scope_generator_drops`).
        self.emit_scope_generator_drops(body.scope);
        // #1949 — release sole-owner `for x in …` cursors (`VecIter`) declared
        // directly in this loop body before the back-edge, the cursor analogue of
        // the generator release above (see `emit_scope_vec_iter_drops`).
        self.emit_scope_vec_iter_drops(body.scope);
        self.emit_scope_stream_drops(body.scope);
        if let Some((binding, ty)) = &scrutinee_owner {
            self.record_iteration_owner_drop(
                *binding,
                SYNTHETIC_CALL_SCRUTINEE_NAME,
                scrutinee.site,
                ty,
            );
        }
        // Record this block as a loop-body back-edge so `enumerate_exits`
        // populates its `Goto` `DropPlan` with per-iteration releases for
        // heap-owning bindings declared in `body.scope` (including the
        // match-arm payload bindings the `while let` itself introduces).
        self.loop_back_edge_blocks
            .insert(self.current_block_id, body.scope);
        self.active_iteration_owners
            .truncate(active_iteration_owner_mark);
        self.active_scopes.pop();
        self.loop_stack.pop();
        // Restore the prior `binding_locals` entries so the binding scope
        // ends at the body's end — matches Match-arm body-block semantics.
        for (binding, previous) in overwritten_bindings.into_iter().rev() {
            if let Some(previous) = previous {
                self.binding_locals.insert(binding, previous);
            } else {
                self.binding_locals.remove(&binding);
            }
        }

        self.finish_current_block(Terminator::Goto { target: header_bb });

        if let (Some(false_cleanup_bb), Some((binding, ty))) = (false_cleanup_bb, &scrutinee_owner)
        {
            self.start_block(false_cleanup_bb);
            self.record_iteration_owner_drop(
                *binding,
                SYNTHETIC_CALL_SCRUTINEE_NAME,
                scrutinee.site,
                ty,
            );
            self.finish_current_block(Terminator::Goto { target: exit_bb });
        }

        // Exit: subsequent lowering continues here.
        self.start_block(exit_bb);
        None
    }

    /// Detect a `while let` enum-variant pattern that leaves an OWNED payload
    /// field unaccounted for — skipped by a `..` rest or a bare `_` sibling.
    /// Returns the first such `(field_idx, field_ty)`, or `None` when every
    /// owned payload field is bound or nested-matched (`BitCopy` skips are
    /// safe: there is no heap to leak). Drives the `while let`-specific
    /// fail-closed refusal in [`Self::lower_while_let`]. "Owned" is the same
    /// non-`BitCopy` drop-seed predicate the record/tuple discharge uses
    /// ([`Self::binding_seeds_drop_elaboration`]); a field is accounted for
    /// when a top-level binding or a nested payload predicate covers it.
    fn while_let_skipped_owned_payload_field(
        &self,
        scrutinee: &HirExpr,
        variant_idx: u32,
        bindings: &[hew_hir::HirMatchArmBinding],
        payload_variant_predicates: &[hew_hir::HirPayloadVariantPredicate],
    ) -> Option<(u32, ResolvedTy)> {
        use crate::model::HeapOwnershipLayouts as _;
        let subst = self.subst_ty(&scrutinee.ty);
        let ResolvedTy::Named { name, args, .. } = &subst else {
            return None;
        };
        let layouts = crate::model::MirHeapLayouts {
            record_field_orders: &self.record_field_orders,
            enum_layouts: &self.enum_layouts,
        };
        let variants = layouts.enum_variant_field_tys(name, args)?;
        let payload = variants.get(variant_idx as usize)?;
        let mut accounted: HashSet<u32> = bindings.iter().map(|b| b.field_idx).collect();
        accounted.extend(payload_variant_predicates.iter().map(|p| p.field_idx));
        for (idx, field_ty) in payload.iter().enumerate() {
            let Ok(idx) = u32::try_from(idx) else {
                continue;
            };
            if accounted.contains(&idx) {
                continue;
            }
            let substituted = self.subst_ty(field_ty);
            if self.binding_seeds_drop_elaboration(&substituted) {
                return Some((idx, substituted));
            }
        }
        None
    }

    /// Lower `if let PAT = scrutinee { body } else { else_body }` to a
    /// three-block CFG that mirrors `lower_if` (for the branch shape) plus the
    /// payload-binding emit from `lower_while_let` (for the then arm):
    ///
    /// ```text
    /// entry_bb (current):
    ///   result_local = alloc(result_ty)
    ///   scrutinee_local = lower(scrutinee)
    ///   tag_local = Move from Place::EnumTag(scrutinee_local)
    ///   k = ConstI64(variant_idx)
    ///   cond = IntCmp(Eq, tag, k)
    ///   Branch { cond, then: then_bb, else: else_bb }
    ///
    /// then_bb:
    ///   [payload bindings via Place::MachineVariant]
    ///   result_local = lower(body)
    ///   Goto join_bb
    ///
    /// else_bb:
    ///   [result_local = lower(else_body)]   (or no-op when else absent)
    ///   Goto join_bb
    ///
    /// join_bb:
    ///   (subsequent lowering continues here; value = result_local)
    /// ```
    ///
    /// Returns the result `Place::Local` (which both branches write). For a
    /// Unit-typed `if let` the result local is allocated but never read.
    #[allow(
        clippy::too_many_arguments,
        clippy::too_many_lines,
        reason = "single coherent CFG builder for if-let; mirrors lower_while_let binding \
                  setup, nested predicate checks, and lower_if branch shape — splitting would \
                  require passing many intermediate block IDs and binding-restore state across \
                  helper boundaries"
    )]
    fn lower_if_let(
        &mut self,
        scrutinee: &HirExpr,
        variant_idx: u32,
        bindings: &[hew_hir::HirMatchArmBinding],
        payload_variant_predicates: &[hew_hir::HirPayloadVariantPredicate],
        body: &hew_hir::HirBlock,
        else_body: Option<&hew_hir::HirBlock>,
        result_ty: &ResolvedTy,
    ) -> Option<Place> {
        // #2648 preflight — run BEFORE any allocation or scrutinee lowering. A
        // reject short-circuits with no partial MIR; the admission token also
        // gates the #2429 from-call owner mint below (symmetric with
        // `lower_match_enum_tag`/`lower_while_let`), so it is threaded through
        // rather than discarded.
        let scrutinee_admission = match self.classify_call_scrutinee_admission(scrutinee) {
            Ok(admission) => admission,
            Err(diag) => {
                self.diagnostics.push(*diag);
                return None;
            }
        };
        let result_place = self.alloc_local(self.subst_ty(result_ty));

        // Entry: evaluate scrutinee, load tag, branch.
        let scrutinee_place = self.lower_value(scrutinee)?;
        let scrutinee_local = match scrutinee_place {
            Place::Local(n) => n,
            other => {
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::NotYetImplemented {
                        construct: "if-let scrutinee place shape".to_string(),
                        site: scrutinee.site,
                    },
                    note: format!(
                        "if-let scrutinee must lower to Place::Local; got {other:?}. \
                         The HIR producer should only emit IfLet for enum-typed scrutinees \
                         backed by a local slot"
                    ),
                });
                return None;
            }
        };

        // #2429 — give a FROM-CALL enum-composite if-let scrutinee an owner so
        // its payload is released on EVERY exit edge, exactly as
        // `lower_match_enum_tag`/`lower_while_let` already do. Registered here,
        // BEFORE the branch and the mid-then `emit_payload_variant_predicate_checks`
        // that route to `else_bb`, so the synthetic owned local is live on the
        // matched edge (payload moved out, shell composite drop), the refuted
        // `else_bb` edge, AND any nested-predicate fallthrough — the scope-exit
        // drop elaboration then frees it once on whichever edge leaves the
        // enclosing scope. No-op for binding-ref scrutinees, runtime-symbol
        // producers, and the recv/iter-next shapes carrying their own release.
        // Non-loop: the return is discarded (mirroring `lower_match_enum_tag`);
        // the scope-exit machinery handles every edge, so no explicit
        // per-iteration owner-drop plumbing is needed (that is `lower_while_let`'s
        // loop-only concern).
        self.register_from_call_scrutinee_owner(scrutinee_admission, scrutinee, scrutinee_local);

        let tag_local = self.alloc_local(ResolvedTy::I64);
        self.push_instr(Instr::Move {
            dest: tag_local,
            src: Place::EnumTag(scrutinee_local),
        });
        let k_local = self.alloc_local(ResolvedTy::I64);
        self.push_instr(Instr::ConstI64 {
            dest: k_local,
            value: i64::from(variant_idx),
        });
        let cond_local = self.alloc_local(ResolvedTy::Bool);
        self.push_instr(Instr::IntCmp {
            pred: crate::model::CmpPred::Eq,
            lhs: tag_local,
            rhs: k_local,
            dest: cond_local,
        });

        let then_bb = self.alloc_block();
        let else_bb = self.alloc_block();
        let join_bb = self.alloc_block();

        // Track whether either arm falls through to the join with a value.
        // When BOTH arms diverge (each `return`s/`panic`s, possibly through a
        // further nested CFG expression) the join has no live predecessor and
        // `result_place` is never written; the cursor must stay unreachable so
        // a tail `if let` does not feed the dead `Unit` i8 stand-in into a
        // non-scalar return slot (the #1907 `Move type mismatch` abort). The
        // reachability flag — not the value `Option` — is the load-bearing
        // signal (see `lower_if`/`lower_match_enum_tag` for the rationale).
        let mut join_reachable = false;

        self.finish_current_block(Terminator::Branch {
            cond: cond_local,
            then_target: then_bb,
            else_target: else_bb,
        });

        // Then arm: bind payload fields (same as lower_while_let body entry),
        // lower body, move result into result_place.
        self.start_block(then_bb);
        let mut nested_binding_jobs: Vec<(u32, u32, hew_hir::HirMatchArmBinding)> = Vec::new();
        for pvp in payload_variant_predicates {
            self.emit_payload_variant_predicate_checks(
                pvp,
                scrutinee_local,
                variant_idx,
                else_bb,
                scrutinee.site,
                &mut nested_binding_jobs,
            )?;
        }

        let mut overwritten_bindings =
            Vec::with_capacity(bindings.len() + nested_binding_jobs.len());
        // #2523 — classify the if-let scrutinee once so every heap-owning
        // payload binder routes its move-out through the shared default-deny
        // consume policy.
        let scrutinee_origin = self.classify_scrutinee_origin(scrutinee);
        for binding in bindings {
            let binding_ty = self.subst_ty(&binding.ty);
            self.statements.push(MirStatement::Bind {
                binding: binding.binding,
                name: binding.name.clone(),
                site: scrutinee.site,
                ty: binding_ty.clone(),
            });
            self.record_binding_scope(binding.binding);
            let keep_for_drop_elab = self.binding_seeds_drop_elaboration(&binding_ty);
            if keep_for_drop_elab {
                self.register_owned_local(
                    binding.binding,
                    binding.name.clone(),
                    binding_ty.clone(),
                );
            }
            let dest = self.alloc_local(binding.ty.clone());
            self.push_instr(Instr::Move {
                dest,
                src: Place::MachineVariant {
                    local: scrutinee_local,
                    variant_idx,
                    field_idx: binding.field_idx,
                },
            });
            let previous = self.binding_locals.insert(binding.binding, dest);
            overwritten_bindings.push((binding.binding, previous));
            // #2523 — record provenance for a heap-owning TOP-LEVEL if-let
            // payload binder so its move-out routes through default-deny.
            self.record_projected_payload_provenance(
                binding.binding,
                &binding.name,
                Place::MachineVariant {
                    local: scrutinee_local,
                    variant_idx,
                    field_idx: binding.field_idx,
                },
                scrutinee_origin.clone(),
                keep_for_drop_elab,
            );
        }
        for (src_local, src_variant_idx, binding) in nested_binding_jobs {
            let binding_ty = self.subst_ty(&binding.ty);
            self.statements.push(MirStatement::Bind {
                binding: binding.binding,
                name: binding.name.clone(),
                site: scrutinee.site,
                ty: binding_ty.clone(),
            });
            self.record_binding_scope(binding.binding);
            let keep_for_drop_elab = self.binding_seeds_drop_elaboration(&binding_ty);
            if keep_for_drop_elab {
                self.register_owned_local(
                    binding.binding,
                    binding.name.clone(),
                    binding_ty.clone(),
                );
            }
            let dest = self.alloc_local(binding.ty.clone());
            self.push_instr(Instr::Move {
                dest,
                src: Place::MachineVariant {
                    local: src_local,
                    variant_idx: src_variant_idx,
                    field_idx: binding.field_idx,
                },
            });
            let previous = self.binding_locals.insert(binding.binding, dest);
            overwritten_bindings.push((binding.binding, previous));
            // #2523 F2 — nested if-let binder bound from a transient predicate
            // copy; reject a heap-owning move-out fail-closed.
            self.record_projected_payload_provenance(
                binding.binding,
                &binding.name,
                Place::MachineVariant {
                    local: src_local,
                    variant_idx: src_variant_idx,
                    field_idx: binding.field_idx,
                },
                ProjectedPayloadOrigin::Reject(ProjectedPayloadRejectReason::NestedDestructure),
                keep_for_drop_elab,
            );
        }

        self.active_scopes.push(body.scope);
        for stmt in &body.statements {
            self.stmt(stmt);
        }
        let then_value = if let Some(tail) = &body.tail {
            self.lower_value(tail)
        } else {
            None
        };
        if let Some(src) = then_value {
            self.push_instr(Instr::Move {
                dest: result_place,
                src,
            });
        }
        self.emit_pending_defers(body.scope);
        self.active_scopes.pop();

        // Restore binding_locals after then-arm scope ends.
        for (binding, previous) in overwritten_bindings.into_iter().rev() {
            if let Some(previous) = previous {
                self.binding_locals.insert(binding, previous);
            } else {
                self.binding_locals.remove(&binding);
            }
        }
        // Binding restore does not touch `cursor_unreachable`; the flag still
        // reflects whether the then-body diverged.
        if !self.cursor_unreachable {
            join_reachable = true;
        }
        self.finish_current_block(Terminator::Goto { target: join_bb });

        // Else arm: lower else_body (if present), move result into result_place.
        // `else_body: None` emits a Goto-only block that always falls through,
        // so a one-armed `if let ... { return }` keeps the join reachable.
        self.start_block(else_bb);
        if let Some(eb) = else_body {
            self.active_scopes.push(eb.scope);
            for stmt in &eb.statements {
                self.stmt(stmt);
            }
            let else_value = if let Some(tail) = &eb.tail {
                self.lower_value(tail)
            } else {
                None
            };
            if let Some(src) = else_value {
                self.push_instr(Instr::Move {
                    dest: result_place,
                    src,
                });
            }
            self.emit_pending_defers(eb.scope);
            self.active_scopes.pop();
        }
        if !self.cursor_unreachable {
            join_reachable = true;
        }
        self.finish_current_block(Terminator::Goto { target: join_bb });

        // Join: subsequent lowering continues here. `start_block` resets
        // `cursor_unreachable`, so re-flag the dead join AFTER opening it when
        // both arms diverged.
        self.start_block(join_bb);
        if !join_reachable {
            self.cursor_unreachable = true;
        }
        Some(result_place)
    }

    #[allow(
        clippy::too_many_lines,
        reason = "single coherent CFG builder for the for-range loop; splitting would require passing many intermediate block IDs across helper boundaries"
    )]
    /// Lower `for binding in start..end { body }` (or `start..=end`) to a
    /// four-block CFG:
    ///
    /// ```text
    /// entry_bb (current):
    ///   counter = lower(start)
    ///   end_val = lower(end)
    ///   [inclusive?]  end_val = IntArithChecked(Add, end_val, 1)
    ///                   → on overflow → trap_bb { IntegerOverflow }
    ///                   → on success → header_bb
    ///   [exclusive?]  Goto header_bb
    ///
    /// header_bb:
    ///   cond = IntCmp(SignedLess, counter, end_val)
    ///   Branch { cond, then: body_bb, else: exit_bb }
    ///
    /// body_bb:
    ///   binding ← counter       (Move)
    ///   lower(body statements)
    ///   Goto inc_bb              ← fall-through (also the `continue` target)
    ///
    /// inc_bb:
    ///   counter = IntArithChecked(Add, counter, 1) → trap on overflow
    ///   Goto header_bb           ← back-edge
    ///
    /// exit_bb:
    ///   (subsequent lowering continues here)
    /// ```
    ///
    /// The inclusive form `start..=end` adjusts `end_val` by +1 before the
    /// loop header so the header's strict-less-than predicate covers the
    /// inclusive endpoint.  Overflow on +1 traps as `IntegerOverflow`,
    /// matching B-2 discipline.
    ///
    /// The counter increment also uses `IntArithChecked` with a
    /// `TrapKind::IntegerOverflow` guard.  A loop iterating to `i64::MAX`
    /// would try to increment past it and trap rather than silently wrapping
    /// — fail-closed per the reliability tenet.
    ///
    /// ## Adapters: `step` and `descending`
    ///
    /// `step` is the per-iteration stride (default literal `1`, set by
    /// `.step_by(k)`); `descending` is set by `.rev()`.  The two compose:
    ///
    ///   - **Ascending** (`descending == false`): counter starts at `start`,
    ///     the header tests `counter < end_val` (with `end_val = end (+1 if
    ///     inclusive)`), and each iteration adds `step` (checked → trap on
    ///     overflow).
    ///   - **Descending** (`descending == true`): counter starts at the high
    ///     element (`end` if inclusive, `end - 1` if exclusive), the header
    ///     tests `counter >= start`, and each iteration subtracts `step`.  A
    ///     checked subtract that underflows the counter's type (e.g.
    ///     `0u32 - 1`) is the natural loop terminus and branches to the exit
    ///     rather than trapping, so a descending unsigned range to `0` does
    ///     not wrap.  This makes `(0..5).rev()` yield `4 3 2 1 0` and
    ///     `(0..=10).rev().step_by(3)` yield `10 7 4 1`.
    ///
    /// A statically-zero step is rejected by the checker; a runtime-zero step
    /// (`step_by(n)` with `n == 0`) traps as `DivideByZero` before the loop
    /// header so the loop can never spin forever — fail-closed.
    #[allow(
        clippy::too_many_arguments,
        reason = "the ForRange node's fields (label, binding, start, end, inclusive, step, descending, body) are threaded individually; bundling them into a struct would only re-spread them here"
    )]
    fn lower_for_range(
        &mut self,
        label: Option<&str>,
        binding: &hew_hir::HirBinding,
        start: &HirExpr,
        end: &HirExpr,
        inclusive: bool,
        step: &HirExpr,
        descending: bool,
        body: &hew_hir::HirBlock,
    ) -> Option<Place> {
        // The loop counter and bound use the checker-resolved element type from
        // the HIR binding.  The HIR lowers this from the range bounds, so a
        // `for i in 2..n` with `n: i32` produces an `i32` counter, matching the
        // widths of any `Vec<i32>` elements or other `i32` operands computed
        // from `i` inside the loop body.  Falls back to I64 when the binding
        // type is not a concrete integer (e.g. unconstrained literal range
        // `0..8` that was never narrowed by use — those still default to i64).
        let elem_ty = self.subst_ty(&binding.ty);
        let counter_ty = if integer_bit_width(&elem_ty, self.pointer_width).is_some() {
            elem_ty.clone()
        } else {
            ResolvedTy::I64
        };
        // Signedness drives the checked-arithmetic intrinsic family for the
        // counter advance.  Falls back to Signed for a non-integer counter_ty
        // (already canonicalised to I64 above), matching the historical
        // ascending behaviour.
        let counter_signedness = integer_signedness(&counter_ty).unwrap_or(IntSignedness::Signed);

        let counter = self.alloc_local(counter_ty.clone());
        // For an ascending loop `bound` is the exclusive upper bound the header
        // compares the counter against; for a descending loop it is the low
        // bound (the `start` of the range) the counter must stay `>=`.
        let bound = self.alloc_local(counter_ty.clone());

        // Loop structure blocks, allocated up front (before the
        // start/bound setup below) so the descending-exclusive emptiness
        // gate (#1948) can jump straight to `exit_bb`, bypassing the header
        // entirely for a statically-known-empty range without needing the
        // header's `counter >= bound` comparison to independently discover
        // the same fact from a synthesized counter value. `alloc_block` only
        // allocates a numeric id (no emission side effect — every block
        // below is forward-referenced by id well before `start_block` is
        // called on it, matching the existing header/body/inc/exit pattern
        // already used throughout this function), so hoisting the four ids
        // here changes nothing about where each block is actually emitted.
        // `inc_bb` is a dedicated advance block: the body falls through to it
        // AND `continue` jumps to it, so the counter advance happens on
        // every path that re-enters the header. Threading `continue` straight
        // to the header would skip the advance and spin forever (Risk 1).
        let header_bb = self.alloc_block();
        let body_bb = self.alloc_block();
        let inc_bb = self.alloc_block();
        let exit_bb = self.alloc_block();

        // Lower the stride into a local once; both directions reuse it. The
        // step expression is user source (the `step_by(n)` argument), so its
        // setup Move carries the for-loop statement span — gdb steps onto the
        // for line, not the prior statement.
        let step_place = self.lower_value(step)?;
        let step_val = self.alloc_local(counter_ty.clone());
        self.push_instr(Instr::Move {
            dest: step_val,
            src: step_place,
        });
        // Runtime fail-closed guard: a zero step would never advance the
        // counter and spin forever.  The checker rejects a statically-zero
        // literal step; this covers a dynamic `step_by(n)` with `n == 0`.
        // (A negative step is impossible for an unsigned width and is rejected
        // by the checker for a signed literal; a dynamic negative signed step
        // is caught by the same `<= 0` test against zero below.)
        //
        // SYNTHETIC step-validation guard — no user source statement. The
        // zero compare and its trap are compiler-inserted fail-closed
        // infrastructure, not anything the programmer wrote, so they stay
        // span-less (`instructions.push`); attributing them to the for line
        // would make gdb stop on a check the user never typed.
        {
            let zero = self.alloc_local(counter_ty.clone());
            self.instructions.push(Instr::ConstI64 {
                dest: zero,
                value: 0,
            });
            let bad_step = self.alloc_local(ResolvedTy::Bool);
            // For an unsigned counter the step is unsigned: only zero is
            // degenerate (`UnsignedLessEq(step, 0)` ≡ `step == 0`).  For a
            // signed counter, `SignedLessEq` also rejects negative steps.
            let step_guard_pred = match counter_signedness {
                IntSignedness::Unsigned => CmpPred::UnsignedLessEq,
                IntSignedness::Signed => CmpPred::SignedLessEq,
            };
            self.instructions.push(Instr::IntCmp {
                dest: bad_step,
                pred: step_guard_pred,
                lhs: step_val,
                rhs: zero,
            });
            let bad_step_trap = self.alloc_block();
            let step_ok_bb = self.alloc_block();
            self.finish_current_block(Terminator::Branch {
                cond: bad_step,
                then_target: bad_step_trap,
                else_target: step_ok_bb,
            });
            self.start_block(bad_step_trap);
            self.finish_current_block(Terminator::Trap {
                kind: TrapKind::DivideByZero,
            });
            self.start_block(step_ok_bb);
        }

        let raw_start = self.lower_value(start)?;
        let raw_end = self.lower_value(end)?;

        if descending {
            // Descending: counter starts at the high element and the header
            // gates on `counter >= start`.  `bound` holds `start`.  The
            // counter/bound init computes the user's range bounds, so it
            // carries the for-loop statement span (`push_instr`) — gdb steps
            // onto the for line for the loop setup.
            self.push_instr(Instr::Move {
                dest: bound,
                src: raw_start,
            });
            if inclusive {
                // `a..=b` reversed starts at `b`.
                self.push_instr(Instr::Move {
                    dest: counter,
                    src: raw_end,
                });
            } else {
                // `a..b` reversed starts at `b - 1` (checked; trap on
                // underflow so `a..MIN` fails closed rather than wrapping).
                //
                // #1948 — an EMPTY exclusive descending range (`raw_start >=
                // raw_end`, e.g. `(0u32..0).rev()`, `(i32::MIN..i32::MIN).rev()`,
                // or the a>b shapes `(5u32..0).rev()` / `(0i32..i32::MIN).rev()`)
                // must still yield zero iterations rather than trap. The `b - 1`
                // decrement below is only meaningful once a real high element
                // exists to start from; for an empty range there is no such
                // element, and when `raw_end` sits at the counter type's
                // representable minimum, `raw_end - 1` unconditionally
                // underflows/overflows the counter width in exactly this case —
                // independent of whether the loop itself has any iterations to
                // run.
                //
                // Gate the decrement on emptiness first: an exclusive range is
                // empty iff `raw_start >= raw_end`, not merely `raw_start ==
                // raw_end` (equality only catches the boundary case; `a > b`
                // is just as empty and can land on the same underflowing `b -
                // 1`). Skip the header/body cycle entirely (`Goto exit_bb`
                // directly) without ever computing `b - 1` — this is NOT the
                // same as routing `counter` to a value the header would
                // independently reject, because the header tests `counter >=
                // bound` (`bound == raw_start`) and any candidate `counter`
                // equal to `raw_start` trivially SATISFIES that predicate
                // (entering the loop once), while any value strictly less than
                // `raw_start` needs its own underflow-safe construction —
                // there is no single representable sentinel that solves this
                // for every width, so bypassing the header outright is the
                // correct fix, not an easier substitute for one. A non-empty
                // range takes the existing decrement-with-trap path unchanged,
                // still routed through `pre_header_bb` → `header_bb` as before.
                let is_empty = self.alloc_local(ResolvedTy::Bool);
                // Same signedness-keyed predicate selection as the header
                // comparison below (`header_pred`): an unsigned counter needs
                // `UnsignedGreaterEq` so a high-bit-set bound still compares
                // correctly, mirroring why the header itself doesn't use a
                // single signed/unsigned-agnostic predicate.
                let empty_pred = match counter_signedness {
                    IntSignedness::Signed => CmpPred::SignedGreaterEq,
                    IntSignedness::Unsigned => CmpPred::UnsignedGreaterEq,
                };
                self.push_instr(Instr::IntCmp {
                    dest: is_empty,
                    pred: empty_pred,
                    lhs: raw_start,
                    rhs: raw_end,
                });
                let nonempty_bb = self.alloc_block();
                let pre_header_bb = self.alloc_block();
                self.finish_current_block(Terminator::Branch {
                    cond: is_empty,
                    then_target: exit_bb,
                    else_target: nonempty_bb,
                });

                self.start_block(nonempty_bb);
                let one = self.alloc_local(counter_ty.clone());
                self.push_instr(Instr::ConstI64 {
                    dest: one,
                    value: 1,
                });
                let overflow_flag = self.alloc_local(ResolvedTy::Bool);
                self.push_instr(Instr::IntArithChecked {
                    op: IntArithOp::Sub,
                    signed: counter_signedness,
                    dest: counter,
                    lhs: raw_end,
                    rhs: one,
                    overflow_flag,
                });
                let trap_bb = self.alloc_block();
                self.finish_current_block(Terminator::Branch {
                    cond: overflow_flag,
                    then_target: trap_bb,
                    else_target: pre_header_bb,
                });
                self.start_block(trap_bb);
                self.finish_current_block(Terminator::Trap {
                    kind: TrapKind::IntegerOverflow,
                });

                self.start_block(pre_header_bb);
            }
        } else {
            // Ascending: counter starts at `start`; `bound` is the exclusive
            // upper bound.  The counter/bound init computes the user's range
            // bounds, so it carries the for-loop statement span (`push_instr`).
            self.push_instr(Instr::Move {
                dest: counter,
                src: raw_start,
            });
            // Exclusive (`a..b`)  → bound = raw_end (simple move).
            // Inclusive (`a..=b`) → bound = raw_end + 1 (checked; trap on
            //                       overflow so `a..=i64::MAX` fails closed).
            if inclusive {
                let one = self.alloc_local(counter_ty.clone());
                self.push_instr(Instr::ConstI64 {
                    dest: one,
                    value: 1,
                });
                let overflow_flag = self.alloc_local(ResolvedTy::Bool);
                self.push_instr(Instr::IntArithChecked {
                    op: IntArithOp::Add,
                    signed: counter_signedness,
                    dest: bound,
                    lhs: raw_end,
                    rhs: one,
                    overflow_flag,
                });
                // On overflow: trap.  On success: fall through to header.
                let trap_bb = self.alloc_block();
                let pre_header_bb = self.alloc_block();
                self.finish_current_block(Terminator::Branch {
                    cond: overflow_flag,
                    then_target: trap_bb,
                    else_target: pre_header_bb,
                });
                self.start_block(trap_bb);
                self.finish_current_block(Terminator::Trap {
                    kind: TrapKind::IntegerOverflow,
                });
                // Continue building in pre_header_bb; the loop header is
                // allocated below and we Goto it from here.
                self.start_block(pre_header_bb);
            } else {
                self.push_instr(Instr::Move {
                    dest: bound,
                    src: raw_end,
                });
            }
        }

        // Jump from entry (or post-bound-adjust-overflow-check) to the header.
        self.finish_current_block(Terminator::Goto { target: header_bb });

        // Header: ascending tests `counter < bound`; descending tests
        // `counter >= bound` (bound == start).  Either way, false → exit.
        self.start_block(header_bb);
        let cond = self.alloc_local(ResolvedTy::Bool);
        // Select ordering predicate by both direction AND signedness so that
        // unsigned ranges with high-bit-set bounds (e.g. crossing the sign
        // bit) compare correctly.  Signed ICmp on an unsigned counter treats
        // high-bit values as negative, producing 0 iterations for ranges such
        // as `0x7FFF_FFFF_FFFF_FFFEu64 .. 0x8000_0000_0000_0001u64`.
        let header_pred = match (descending, counter_signedness) {
            (true, IntSignedness::Signed) => CmpPred::SignedGreaterEq,
            (true, IntSignedness::Unsigned) => CmpPred::UnsignedGreaterEq,
            (false, IntSignedness::Signed) => CmpPred::SignedLess,
            (false, IntSignedness::Unsigned) => CmpPred::UnsignedLess,
        };
        self.push_instr(Instr::IntCmp {
            dest: cond,
            pred: header_pred,
            lhs: counter,
            rhs: bound,
        });
        self.finish_current_block(Terminator::Branch {
            cond,
            then_target: body_bb,
            else_target: exit_bb,
        });

        // Body: expose binding → counter, lower body statements, then
        // increment counter and loop back to the header.
        self.start_block(body_bb);

        // Register the loop variable so body BindingRef nodes resolve to
        // `counter`.  The Bind statement is required by MIR move-check
        // bookkeeping; it carries a sentinel SiteId(0) because the loop
        // variable has no checker-recorded call site.
        self.binding_locals.insert(binding.id, counter);
        self.statements.push(MirStatement::Bind {
            binding: binding.id,
            name: binding.name.clone(),
            site: hew_hir::SiteId(0),
            ty: counter_ty.clone(),
        });
        self.record_binding_scope(binding.id);

        self.active_scopes.push(body.scope);
        // continue → inc_bb (advances the counter, then re-checks the header);
        // break → exit. Depth captured before the body scope push so the
        // in-loop defer window covers body.scope and any nested block scopes.
        let loop_scope_depth = self.active_scopes.len() - 1;
        self.loop_stack.push(LoopFrame {
            label: label.map(str::to_string),
            continue_target: inc_bb,
            exit_target: exit_bb,
            scope_depth: loop_scope_depth,
            body_scope: body.scope,
        });
        for stmt in &body.statements {
            self.stmt(stmt);
        }
        if let Some(tail) = &body.tail {
            let _ = self.lower_value(tail);
        }
        self.emit_pending_defers(body.scope);
        // Release generators declared in the loop body before the increment +
        // back-edge (per-iteration `hew_gen_coro_destroy`; see
        // `emit_scope_generator_drops`).
        self.emit_scope_generator_drops(body.scope);
        // #1949 — release sole-owner `for x in …` cursors (`VecIter`) declared
        // directly in this loop body before the back-edge, the cursor analogue of
        // the generator release above (see `emit_scope_vec_iter_drops`).
        self.emit_scope_vec_iter_drops(body.scope);
        self.emit_scope_stream_drops(body.scope);
        // Record body→inc as the per-iteration back-edge for body-scope
        // bindings. body.scope closes here (active_scopes.pop next), so any
        // heap-owning let-binding declared inside the body must be released
        // before fall-through into the inc/header re-evaluation overwrites it
        // on the next iteration. The counter binding itself lives in the
        // outer scope (registered before active_scopes.push(body.scope)) and
        // is i64 — no drop — so the back-edge plan never touches it.
        self.loop_back_edge_blocks
            .insert(self.current_block_id, body.scope);
        self.active_scopes.pop();
        self.loop_stack.pop();
        // Body fall-through → increment block.
        self.finish_current_block(Terminator::Goto { target: inc_bb });

        // Advance the counter by `step` (checked).
        //
        // Ascending: `counter += step`.  Overflow past the type max traps as
        // IntegerOverflow rather than silently wrapping and running forever
        // — fail-closed per the reliability tenet.
        //
        // Descending: `counter -= step`.  An underflow past the type min is the
        // natural loop terminus for a descending range that reaches its low
        // bound (e.g. `(0u32..5).rev()` decrementing past `0`), so it branches
        // to the loop exit instead of trapping.  The header's `counter >= start`
        // guard handles every non-underflowing terminus.
        self.start_block(inc_bb);
        // The counter advance is user-visible loop mechanics (the per-iteration
        // step), so it carries the for-loop statement span (`push_instr`).
        let advance_flag = self.alloc_local(ResolvedTy::Bool);
        self.push_instr(Instr::IntArithChecked {
            op: if descending {
                IntArithOp::Sub
            } else {
                IntArithOp::Add
            },
            signed: counter_signedness,
            dest: counter,
            lhs: counter,
            rhs: step_val,
            overflow_flag: advance_flag,
        });
        if descending {
            // Underflow on the descending decrement → loop is exhausted; exit.
            self.finish_current_block(Terminator::Branch {
                cond: advance_flag,
                then_target: exit_bb,
                else_target: header_bb,
            });
        } else {
            let overflow_trap = self.alloc_block();
            // On overflow → trap; otherwise loop back to the header (re-check bound).
            self.finish_current_block(Terminator::Branch {
                cond: advance_flag,
                then_target: overflow_trap,
                else_target: header_bb,
            });
            self.start_block(overflow_trap);
            self.finish_current_block(Terminator::Trap {
                kind: TrapKind::IntegerOverflow,
            });
        }

        // Exit: subsequent lowering continues here.
        self.start_block(exit_bb);
        None
    }

    /// Lower a bare `loop { body }` to a two-block CFG:
    ///
    /// ```text
    /// entry_bb (current):
    ///   Goto body_bb
    ///
    /// body_bb:
    ///   lower(body statements)
    ///   Goto body_bb             ← unconditional back-edge (also `continue`)
    ///
    /// exit_bb:
    ///   (only reachable via `break`; subsequent lowering continues here)
    /// ```
    ///
    /// A bare `loop` has no condition: the sole way out is `break`, so
    /// `exit_bb` has no predecessor unless the body contains one. We start it
    /// unconditionally anyway (Risk 4) so the post-loop cursor always has a
    /// home and `finalize_blocks` can drop it if it stays empty/unreachable.
    /// `continue` targets `body_bb` directly — there is no header to re-check.
    ///
    /// Always returns `None`: `loop {}` is `Unit`-typed at the MIR boundary
    /// (a `break value` carries its operand for side effects only in this
    /// slice; loop-as-expression is out of scope — see the plan).
    fn lower_loop(&mut self, label: Option<&str>, body: &hew_hir::HirBlock) -> Option<Place> {
        let body_bb = self.alloc_block();
        let exit_bb = self.alloc_block();

        // Entry → body.
        self.finish_current_block(Terminator::Goto { target: body_bb });

        // Body: lower statements then loop back unconditionally.
        self.start_block(body_bb);
        // continue → body_bb (re-enter the top); break → exit.
        let loop_scope_depth = self.active_scopes.len();
        self.loop_stack.push(LoopFrame {
            label: label.map(str::to_string),
            continue_target: body_bb,
            exit_target: exit_bb,
            scope_depth: loop_scope_depth,
            body_scope: body.scope,
        });
        self.active_scopes.push(body.scope);
        for stmt in &body.statements {
            self.stmt(stmt);
        }
        if let Some(tail) = &body.tail {
            let _ = self.lower_value(tail);
        }
        self.emit_pending_defers(body.scope);
        // Release generators declared in the loop body before the back-edge
        // (per-iteration `hew_gen_coro_destroy`; see `emit_scope_generator_drops`).
        self.emit_scope_generator_drops(body.scope);
        // #1949 — release sole-owner `for x in …` cursors (`VecIter`) declared
        // directly in this loop body before the back-edge, the cursor analogue of
        // the generator release above (see `emit_scope_vec_iter_drops`).
        self.emit_scope_vec_iter_drops(body.scope);
        self.emit_scope_stream_drops(body.scope);
        // Record this block as a loop-body back-edge so `enumerate_exits`
        // populates its `Goto` `DropPlan` with per-iteration releases for
        // heap-owning bindings declared in `body.scope`. `loop { ... }` has
        // no separate header — `body_bb` is both entry and re-entry — so
        // this back-edge is the one place per-iteration drops can fire.
        self.loop_back_edge_blocks
            .insert(self.current_block_id, body.scope);
        self.active_scopes.pop();
        self.loop_stack.pop();
        self.finish_current_block(Terminator::Goto { target: body_bb });

        // Exit: only reached via `break`. Always started so the post-loop
        // cursor has a home (Risk 4).
        self.start_block(exit_bb);
        None
    }
    ///
    /// CFG shape:
    ///
    /// ```text
    /// entry_bb (current):
    ///   result_place = false          // pessimistic default
    ///   lhs_place = lower(lhs)
    ///   Branch { cond: lhs_place, then: rhs_bb, else: join_bb }
    ///
    /// rhs_bb:
    ///   rhs_place = lower(rhs)
    ///   Move { dest: result_place, src: rhs_place }
    ///   Goto join_bb
    ///
    /// join_bb:
    ///   -- result_place holds the final bool --
    /// ```
    ///
    /// The rhs block is only entered when lhs is true, so rhs side effects
    /// are correctly guarded. On the false path, `result_place` retains the
    /// `false` constant written in the entry block.
    fn lower_logical_and(
        &mut self,
        lhs_expr: &HirExpr,
        rhs_expr: &HirExpr,
        result_ty: &ResolvedTy,
    ) -> Option<Place> {
        let result_place = self.alloc_local(result_ty.clone());
        // Write `false` as the pessimistic default (the join block reads
        // result_place, and the else path never writes to it).
        self.push_instr(Instr::ConstI64 {
            dest: result_place,
            value: 0,
        });

        let lhs_place = self.lower_value(lhs_expr)?;

        let rhs_bb = self.alloc_block();
        let join_bb = self.alloc_block();

        self.finish_current_block(Terminator::Branch {
            cond: lhs_place,
            then_target: rhs_bb,
            else_target: join_bb,
        });

        // rhs_bb: lhs was true, evaluate rhs and move into result.
        self.start_block(rhs_bb);
        if let Some(rhs_place) = self.lower_value(rhs_expr) {
            self.push_instr(Instr::Move {
                dest: result_place,
                src: rhs_place,
            });
        }
        self.finish_current_block(Terminator::Goto { target: join_bb });

        self.start_block(join_bb);
        Some(result_place)
    }

    /// Lower `lhs || rhs` with short-circuit semantics.
    ///
    /// CFG shape:
    ///
    /// ```text
    /// entry_bb (current):
    ///   result_place = true           // optimistic default
    ///   lhs_place = lower(lhs)
    ///   Branch { cond: lhs_place, then: join_bb, else: rhs_bb }
    ///
    /// rhs_bb:
    ///   rhs_place = lower(rhs)
    ///   Move { dest: result_place, src: rhs_place }
    ///   Goto join_bb
    ///
    /// join_bb:
    ///   -- result_place holds the final bool --
    /// ```
    ///
    /// The rhs block is only entered when lhs is false, so rhs side effects
    /// are correctly guarded. On the true path, `result_place` retains the
    /// `true` constant written in the entry block.
    fn lower_logical_or(
        &mut self,
        lhs_expr: &HirExpr,
        rhs_expr: &HirExpr,
        result_ty: &ResolvedTy,
    ) -> Option<Place> {
        let result_place = self.alloc_local(result_ty.clone());
        // Write `true` as the optimistic default (the then path never writes
        // to result_place; the else path writes the rhs value into it).
        self.push_instr(Instr::ConstI64 {
            dest: result_place,
            value: 1,
        });

        let lhs_place = self.lower_value(lhs_expr)?;

        let rhs_bb = self.alloc_block();
        let join_bb = self.alloc_block();

        self.finish_current_block(Terminator::Branch {
            cond: lhs_place,
            then_target: join_bb,
            else_target: rhs_bb,
        });

        // rhs_bb: lhs was false, evaluate rhs and move into result.
        self.start_block(rhs_bb);
        if let Some(rhs_place) = self.lower_value(rhs_expr) {
            self.push_instr(Instr::Move {
                dest: result_place,
                src: rhs_place,
            });
        }
        self.finish_current_block(Terminator::Goto { target: join_bb });

        self.start_block(join_bb);
        Some(result_place)
    }

    /// Lower a recognised `hew_*` runtime-ABI call to
    /// `Instr::CallRuntimeAbi`.
    ///
    /// Called from `lower_value`'s `HirExprKind::Call` arm when the
    /// callee is a `BindingRef` whose name passes
    /// `runtime_symbols::is_known_runtime_symbol`.  The HIR args have
    /// already been validated by the HIR pipeline; this method lower
    /// each arg via `lower_value`, then emits the appropriate
    /// instruction sequence.
    ///
    /// # `hew_duplex_pair` encoding
    ///
    /// The runtime C-ABI takes `(s_cap, r_cap, *mut *mut HewDuplexHandle,
    /// *mut *mut HewDuplexHandle)`.  The user surface is `duplex_pair(N)`
    /// with one symmetric capacity arg.  E2 duplicates `args[0]` into
    /// both cap slots and passes two fresh `Place::DuplexHandle(N0/N1)`
    /// in the out-param positions (`args[2..=3]`).  Codegen (E4) takes
    /// the address of each `DuplexHandle` local and passes it as the
    /// actual pointer.  A "tuple proxy" `Place::Local(M)` is returned
    /// so that subsequent `TupleIndex` projections can recover the
    /// individual `DuplexHandle` Places via `self.tuple_decomp`.
    ///
    /// # `hew_duplex_send` encoding
    ///
    /// The runtime C-ABI takes `(*mut HewDuplexHandle, *const u8, usize)`.
    /// For an integer payload `42`, this method emits a prefatory
    /// `Instr::ConstI64 { value: 8 }` as the byte-length constant
    /// before the `CallRuntimeAbi`.  The message value Place is passed
    /// as-is; codegen (E4) stores it to a stack alloca and passes its
    /// address as the `*const u8`.
    ///
    /// # SHIM(E4) convention
    ///
    // SHIM(E4): codegen interprets Place::DuplexHandle(N) per-symbol convention:
    //   hew_duplex_send/recv/close: load the raw ptr from local-N's alloca, pass as *mut HewDuplexHandle.
    //   hew_duplex_pair out-params (args[2], args[3]): take address of local-N's alloca, pass as *mut *mut HewDuplexHandle.
    // The message-value Place::Local(N) in args[1] of hew_duplex_send is store-to-alloca + address-cast by E4.
    // The length Place::Local(N) in args[2] of hew_duplex_send carries the ConstI64(8) emitted above.
    // WHY: MIR names semantics; address materialisation is a codegen-target concern.
    // WHEN obsolete: when E4's lower_instr arm is wired and tested for each of these conventions.
    // WHAT: replace with direct LLVMBuildCall emission for each symbol group.
    fn task_scope_ty() -> ResolvedTy {
        ResolvedTy::Named {
            name: "HewTaskScope".to_string(),
            args: vec![],
            builtin: None,
            is_opaque: false,
        }
    }

    fn push_runtime_call(&mut self, symbol: &str, args: Vec<Place>, dest: Option<Place>) {
        self.push_instr(Instr::CallRuntimeAbi(
            crate::model::RuntimeCall::new(symbol, args, dest)
                .unwrap_or_else(|_| panic!("{symbol} is an allowlisted runtime symbol")),
        ));
    }

    fn lower_task_scope(&mut self, body: &HirBlock) -> Place {
        let scope_place = self.alloc_local(Self::task_scope_ty());
        self.push_runtime_call("hew_task_scope_new", vec![], Some(scope_place));

        let previous_scope_place = self.alloc_local(Self::task_scope_ty());
        self.push_runtime_call(
            "hew_task_scope_set_current",
            vec![scope_place],
            Some(previous_scope_place),
        );

        let saved_scope = self.current_task_scope.replace(scope_place);
        self.active_scopes.push(body.scope);
        for stmt in &body.statements {
            self.stmt(stmt);
        }
        if let Some(tail) = &body.tail {
            let _ = self.lower_value(tail);
        }
        self.emit_pending_defers(body.scope);
        self.active_scopes.pop();
        self.current_task_scope = saved_scope;

        self.push_runtime_call("hew_task_scope_join_all", vec![scope_place], None);
        self.push_runtime_call("hew_task_scope_destroy", vec![scope_place], None);
        let restored_scope_place = self.alloc_local(Self::task_scope_ty());
        self.push_runtime_call(
            "hew_task_scope_set_current",
            vec![previous_scope_place],
            Some(restored_scope_place),
        );

        let unit_place = self.alloc_local(ResolvedTy::Unit);
        self.push_instr(Instr::UnitLit { dest: unit_place });
        unit_place
    }

    fn direct_no_arg_unit_callee(
        &mut self,
        callee: &HirExpr,
        args: &[HirExpr],
        ret_ty: &ResolvedTy,
        site: hew_hir::SiteId,
        construct: &str,
    ) -> Option<String> {
        // For a generic call site, resolve the monomorphized callee symbol by
        // mangling the origin name with the per-site concrete type arguments
        // that HIR recorded. This mirrors the direct-call resolution in the
        // lower_value Call arm (which dispatches to the mangled symbol for
        // generic top-level user functions).
        if let Some(type_args) = self.call_site_type_args.get(&site).cloned() {
            if !type_args.is_empty() {
                let HirExprKind::BindingRef { name, .. } = &callee.kind else {
                    self.diagnostics.push(MirDiagnostic {
                        kind: MirDiagnosticKind::NotYetImplemented {
                            construct: construct.to_string(),
                            site,
                        },
                        note: "generic task spawn requires a direct function binding \
                               as callee"
                            .to_string(),
                    });
                    return None;
                };
                let substituted: Vec<ResolvedTy> =
                    type_args.iter().map(|t| self.subst_ty(t)).collect();
                let mangled = hew_hir::monomorph::mangle(name, &substituted);
                if !self.module_fn_names.contains(&mangled) {
                    self.diagnostics.push(MirDiagnostic {
                        kind: MirDiagnosticKind::NotYetImplemented {
                            construct: construct.to_string(),
                            site,
                        },
                        note: format!(
                            "generic spawn of `{name}` has no registered \
                             monomorphization `{mangled}`; HIR must emit this \
                             instantiation before task spawning can proceed"
                        ),
                    });
                    return None;
                }
                // Apply the same no-arg/unit-return gate as the concrete path.
                if !args.is_empty() || !matches!(ret_ty, ResolvedTy::Unit) {
                    for arg in args {
                        let _ = self.lower_value(arg);
                    }
                    self.diagnostics.push(MirDiagnostic {
                        kind: MirDiagnosticKind::NotYetImplemented {
                            construct: construct.to_string(),
                            site,
                        },
                        note: "cancellation-token task lowering currently supports only \
                               no-argument functions returning unit; value/result task \
                               propagation remains fail-closed"
                            .to_string(),
                    });
                    return None;
                }
                return Some(mangled);
            }
        }
        // Safety guard: a generic function without recorded type arguments
        // has no resolvable monomorphization and cannot be spawned.
        if matches!(
            &callee.kind,
            HirExprKind::BindingRef { name, .. } if self.module_generic_fn_names.contains(name)
        ) {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: construct.to_string(),
                    site,
                },
                note: "generic function spawned without a resolved concrete \
                       instantiation; provide explicit type arguments"
                    .to_string(),
            });
            return None;
        }
        // A no-argument callee returning UNIT is supported via this function.
        // Value-returning (`T != ()`) no-arg spawns are allowed only through
        // the `fork t = callee()` bound form — the task handle is then awaited
        // to retrieve `T`. An UNBOUND implicit spawn of a non-unit callee
        // (bare `callee()` inside `scope { }` with `T != ()`) is rejected here:
        // the result would be silently discarded and the value channel unused,
        // which is a miscompile not a runtime leak.
        //
        // Arg-bearing spawns route through `lower_spawned_args_call_task` before
        // this point; value+args spawns remain fail-closed there.
        if !args.is_empty() || !matches!(ret_ty, ResolvedTy::Unit) {
            for arg in args {
                let _ = self.lower_value(arg);
            }
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: construct.to_string(),
                    site,
                },
                note: "cancellation-token task lowering currently supports only \
                       no-argument functions returning unit; value/result task \
                       propagation remains fail-closed"
                    .to_string(),
            });
            return None;
        }
        match &callee.kind {
            HirExprKind::BindingRef { name, .. } if self.module_fn_names.contains(name) => {
                Some(name.clone())
            }
            _ => {
                let _ = self.lower_value(callee);
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::NotYetImplemented {
                        construct: construct.to_string(),
                        site,
                    },
                    note: "fork cancellation lowering requires a direct module function callee"
                        .to_string(),
                });
                None
            }
        }
    }

    fn mir_sanitize_symbol(symbol: &str) -> String {
        symbol
            .chars()
            .map(|ch| if ch.is_ascii_alphanumeric() { ch } else { '_' })
            .collect()
    }

    fn task_entry_adapter_symbol(callee_symbol: &str) -> String {
        format!(
            "__hew_task_entry_{}",
            Self::mir_sanitize_symbol(callee_symbol)
        )
    }

    /// Resolve (and, on first use, allocate) the task-entry adapter symbol
    /// for `callee_symbol`. Stable across repeated calls with the same
    /// `callee_symbol` (returns the previously allocated adapter symbol
    /// without regenerating the adapter body). If the naive sanitized name
    /// is already owned by a *different* original callee symbol, appends a
    /// numeric disambiguation suffix so every distinct callee still gets its
    /// own, uniquely named adapter.
    fn ensure_task_entry_adapter(&mut self, callee_symbol: &str, result_ty: &ResolvedTy) -> String {
        if let Some(existing) = self.task_entry_adapter_symbols.borrow().get(callee_symbol) {
            return existing.clone();
        }
        let base_symbol = Self::task_entry_adapter_symbol(callee_symbol);
        let already_used = self
            .task_entry_adapter_symbols
            .borrow()
            .values()
            .any(|used| used == &base_symbol);
        let adapter_symbol = if already_used {
            let mut candidate;
            let mut suffix = 2usize;
            loop {
                candidate = format!("{base_symbol}__dup{suffix}");
                let taken = self
                    .task_entry_adapter_symbols
                    .borrow()
                    .values()
                    .any(|used| used == &candidate);
                if !taken {
                    break candidate;
                }
                suffix += 1;
            }
        } else {
            base_symbol
        };
        self.task_entry_adapter_symbols
            .borrow_mut()
            .insert(callee_symbol.to_string(), adapter_symbol.clone());
        let lowered = self.synthesize_task_entry_adapter(callee_symbol, &adapter_symbol, result_ty);
        self.generated_functions.push(lowered);
        adapter_symbol
    }

    /// Synthesize the per-callee task-entry adapter (`__hew_task_entry_<fn>`,
    /// `TaskEntry` call-conv). The adapter is the body the codegen task wrapper
    /// invokes on the spawned worker; the wrapper publishes the adapter's return
    /// value through `hew_task_set_result` and then `hew_task_complete_threaded`.
    ///
    /// For a value-returning task (`result_ty != ()`) the adapter calls the body
    /// into a typed `dest` local and RETURNS it, so the wrapper captures the
    /// child's `T` and writes it into the task result buffer. For a unit task
    /// the adapter calls the body with `dest: None` and returns unit — the
    /// wrapper publishes nothing.
    fn synthesize_task_entry_adapter(
        &self,
        callee_symbol: &str,
        adapter_symbol: &str,
        result_ty: &ResolvedTy,
    ) -> LoweredFunction {
        let is_value_task = !matches!(result_ty, ResolvedTy::Unit);
        // Value task: the body call writes its `T` directly into the function
        // return slot so `Terminator::Return` hands it back; the codegen wrapper
        // then publishes that return value through `hew_task_set_result`. Unit
        // task: discard the body result (`dest: None`).
        let call_dest = if is_value_task {
            Some(Place::ReturnSlot)
        } else {
            None
        };
        let mut blocks = vec![
            BasicBlock {
                id: 0,
                statements: vec![],
                instructions: vec![],
                terminator: Terminator::Call {
                    callee: callee_symbol.to_string(),
                    builtin: None,
                    args: vec![],
                    dest: call_dest,
                    next: 1,
                },
            },
            BasicBlock {
                id: 1,
                statements: vec![],
                instructions: vec![],
                terminator: Terminator::Return,
            },
        ];
        bracket_actor_handler_blocks(&mut blocks);

        let adapter_return_ty = result_ty.clone();
        let thir = ThirFunction {
            name: adapter_symbol.to_string(),
            return_ty: adapter_return_ty.clone(),
            statements: vec![],
        };
        let mut raw = RawMirFunction {
            name: adapter_symbol.to_string(),
            return_ty: adapter_return_ty.clone(),
            call_conv: crate::model::FunctionCallConv::TaskEntry,
            params: vec![],
            locals: vec![],
            // Synthesised task-entry adapter: no user bindings, no `-g` DIEs.
            local_names: Vec::new(),
            local_scopes: Vec::new(),
            local_decl_bytes: Vec::new(),
            scope_table: Vec::new(),
            blocks,
            decisions: vec![],
            intrinsic_id: None,
            await_deadline_ns: std::collections::HashMap::new(),
            suspend_kinds: std::collections::HashMap::new(),
            lambda_actor_user_param_locals: Vec::new(),
            span: None,
            instr_spans: ::std::collections::BTreeMap::new(),
            source_origin: SourceOrigin::Unknown,
        };
        let builder = Builder {
            current_function_symbol: adapter_symbol.to_string(),
            current_function_call_conv: crate::model::FunctionCallConv::TaskEntry,
            ..self.child_builder_tables()
        };
        let mut dataflow_result = dataflow::analyze(&raw.blocks, &builder.type_classes, &[]);
        dataflow_result
            .checks
            .extend(crate::model::validate_context_markers(&raw));
        let diagnostics: Vec<MirDiagnostic> = dataflow_result
            .checks
            .iter()
            .filter_map(check_to_diagnostic)
            .collect();
        let string_derivation = finalize_string_ownership(&mut raw, &builder, &dataflow_result);
        let bytes_derivation = finalize_bytes_ownership(&mut raw, &builder, &dataflow_result);
        let cooperate_sites = dataflow::compute_cooperate_sites(&raw.blocks);
        let checked = CheckedMirFunction {
            name: adapter_symbol.to_string(),
            return_ty: adapter_return_ty,
            blocks: raw.blocks.clone(),
            decisions: vec![],
            checks: dataflow_result.checks.clone(),
            cooperate_sites,
        };
        let elaborated = elaborate(
            &checked,
            &builder,
            &[],
            &dataflow_result,
            Some(&string_derivation.allowed),
            Some(&bytes_derivation.allowed),
        );
        LoweredFunction {
            thir,
            raw,
            checked,
            elaborated,
            diagnostics,
            generated: vec![],
            record_layouts: vec![],
        }
    }

    fn lower_spawned_call_task(
        &mut self,
        callee: &HirExpr,
        args: &[HirExpr],
        task_ty: &ResolvedTy,
        // `true` for a bound `fork t = callee()` spawn (value-task allowed);
        // `false` for an implicit/unbound spawn (non-unit callee must reject).
        bound: bool,
        site: hew_hir::SiteId,
    ) -> Option<Place> {
        let Some(scope_place) = self.current_task_scope else {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "spawned call without current task scope".to_string(),
                    site,
                },
                note: "task spawn reached MIR without a scope-owned cancellation token; \
                       refusing to emit an unobservable cancellation edge"
                    .to_string(),
            });
            return None;
        };
        let ResolvedTy::Task(inner) = task_ty else {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "spawned call with non-Task type".to_string(),
                    site,
                },
                note: "HIR SpawnedCall must carry Task<T>".to_string(),
            });
            return None;
        };
        if !self.current_function_call_conv.carries_execution_context() {
            let callee_name = match &callee.kind {
                HirExprKind::BindingRef { name, .. } => name.as_str(),
                HirExprKind::Closure { .. } => "<closure>",
                _ => "<callee>",
            };
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: format!(
                        "cannot spawn `{callee_name}` from `{}`",
                        self.current_function_symbol
                    ),
                    site,
                },
                note: format!(
                    "`scope {{ fork ... }}` requires an enclosing ctx-bearing execution context, \
                     but `{}` has Default call-conv (no execution-context parameter). Move this \
                     spawn into an actor handler body or an actor-internal helper. Caller-side \
                     ctx-routing for top-level `fn main` is tracked under \
                     W4.010-followup-caller-ctx-routing.",
                    self.current_function_symbol
                ),
            });
            return None;
        }
        if matches!(callee.kind, HirExprKind::Closure { .. }) {
            return self.lower_spawned_closure_task(
                callee,
                args,
                task_ty,
                inner,
                scope_place,
                site,
            );
        }
        if !args.is_empty() {
            return self.lower_spawned_args_call_task(
                callee,
                args,
                task_ty,
                inner,
                scope_place,
                site,
            );
        }
        // Bound value-returning task (`fork t = callee()` where callee returns T ≠ ()):
        // route through the value-callee path — no unit-return restriction, the task handle
        // is awaited to retrieve `T`. Unbound implicit spawns (`callee()` as a bare scope
        // statement, `bound = false`) fall through to `direct_no_arg_unit_callee` where the
        // unit-return gate rejects the non-unit callee fail-closed.
        if bound && !matches!(&**inner, ResolvedTy::Unit) {
            return self.lower_no_arg_value_callee_task(callee, inner, task_ty, scope_place, site);
        }
        let user_callee_symbol =
            self.direct_no_arg_unit_callee(callee, args, inner, site, "spawned call")?;
        let callee_symbol = self.ensure_task_entry_adapter(&user_callee_symbol, inner);

        let task_place = self.alloc_local(task_ty.clone());
        self.push_runtime_call("hew_task_new", vec![], Some(task_place));
        self.push_runtime_call("hew_task_scope_spawn", vec![scope_place, task_place], None);
        self.push_instr(Instr::SpawnTaskDirect {
            task: task_place,
            callee_symbol,
        });
        Some(task_place)
    }

    /// Lower a no-argument value-returning task spawn: `fork t = callee()` where
    /// `callee` returns `T ≠ ()`. The task-entry adapter is synthesized with
    /// `result_ty = T` so the child's `T` is published via `hew_task_set_result`
    /// and the awaiting handler reads it on the resume edge.
    ///
    /// This path does NOT enforce a unit-return restriction (unlike
    /// `direct_no_arg_unit_callee`). All other gates (non-module-fn callee,
    /// arg-bearing callee) remain fail-closed. Generic callees are resolved to
    /// their monomorphized symbol via HIR's per-site type arguments.
    fn lower_no_arg_value_callee_task(
        &mut self,
        callee: &HirExpr,
        inner: &ResolvedTy,
        task_ty: &ResolvedTy,
        scope_place: Place,
        site: hew_hir::SiteId,
    ) -> Option<Place> {
        // Require a direct BindingRef callee.
        let HirExprKind::BindingRef { name, .. } = &callee.kind else {
            let _ = self.lower_value(callee);
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "value-task spawn".to_string(),
                    site,
                },
                note: "value-task spawn requires a direct module function callee".to_string(),
            });
            return None;
        };
        // For a generic call site, resolve the monomorphized callee symbol by
        // mangling the origin name with the per-site concrete type arguments.
        let user_callee_symbol =
            if let Some(type_args) = self.call_site_type_args.get(&site).cloned() {
                if type_args.is_empty() {
                    name.clone()
                } else {
                    let substituted: Vec<ResolvedTy> =
                        type_args.iter().map(|t| self.subst_ty(t)).collect();
                    let mangled = hew_hir::monomorph::mangle(name, &substituted);
                    if !self.module_fn_names.contains(&mangled) {
                        self.diagnostics.push(MirDiagnostic {
                            kind: MirDiagnosticKind::NotYetImplemented {
                                construct: "value-task spawn".to_string(),
                                site,
                            },
                            note: format!(
                                "generic spawn of `{name}` has no registered \
                                 monomorphization `{mangled}`; HIR must emit this \
                                 instantiation before task spawning can proceed"
                            ),
                        });
                        return None;
                    }
                    mangled
                }
            } else {
                name.clone()
            };
        // Safety guard: a generic function without recorded type arguments
        // has no resolvable monomorphization and cannot be spawned.
        if user_callee_symbol == *name && self.module_generic_fn_names.contains(name) {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "value-task spawn".to_string(),
                    site,
                },
                note: "generic function spawned without a resolved concrete \
                       instantiation; provide explicit type arguments"
                    .to_string(),
            });
            return None;
        }
        if !self.module_fn_names.contains(&user_callee_symbol) {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "value-task spawn".to_string(),
                    site,
                },
                note: format!(
                    "value-task spawn callee `{user_callee_symbol}` is not a registered \
                     module function"
                ),
            });
            return None;
        }
        let callee_symbol = self.ensure_task_entry_adapter(&user_callee_symbol, inner);
        let task_place = self.alloc_local(task_ty.clone());
        self.push_runtime_call("hew_task_new", vec![], Some(task_place));
        self.push_runtime_call("hew_task_scope_spawn", vec![scope_place, task_place], None);
        self.push_instr(Instr::SpawnTaskDirect {
            task: task_place,
            callee_symbol,
        });
        Some(task_place)
    }

    /// Lower a spawned call with arguments: `fork t = worker(a, b);` or the
    /// fork-block / implicit-spawn forms carrying args. The args are lowered
    /// in the parent (consuming owned bindings exactly as a direct call
    /// would), packed into a fork-env record, and the spawn dispatches via
    /// `SpawnTaskClosure` to a synthesized fork-entry shim that loads the
    /// env fields back out and calls the target.
    ///
    /// Ownership contract (drop-allowset rules, verified against the emitted
    /// drop plans): the parent emits NO drop for moved-in args — the consume
    /// facts from arg lowering remove them from the parent's plan; the env
    /// rc-box frees bytes only (codegen passes a null drop fn); the shim
    /// emits no drops for the env-loaded temps. This is byte-for-byte the
    /// same plan shape as the direct-call baseline (`shout(greeting)`):
    /// under the current M-COW move-only spine the by-value string param is
    /// read `CowShare` by the callee and released by no one, so the fork form
    /// leaks exactly where the direct call already leaks — never a
    /// double-free. WHEN-OBSOLETE: M-COW retain-on-share; the env transfer
    /// then needs a real retain/release pair in lockstep with call args.
    ///
    /// First slice restricts arg types to `BitCopy` scalars + `string`;
    /// anything else refuses with a diagnostic (never a miscompile). The
    /// callee must return unit — value-bearing forks are gated at the
    /// await site and remain fail-closed until result propagation lands.
    #[expect(
        clippy::too_many_lines,
        reason = "single fail-closed boundary sequence — generic/callee/return/arg-class \
                  gates, then env pack + shim + spawn; splitting would scatter the refusal \
                  conditions away from the spawn they guard"
    )]
    fn lower_spawned_args_call_task(
        &mut self,
        callee: &HirExpr,
        args: &[HirExpr],
        task_ty: &ResolvedTy,
        inner: &ResolvedTy,
        scope_place: Place,
        site: hew_hir::SiteId,
    ) -> Option<Place> {
        let HirExprKind::BindingRef { name, .. } = &callee.kind else {
            let _ = self.lower_value(callee);
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "spawned call".to_string(),
                    site,
                },
                note: "arg-bearing task spawn requires a direct module function callee".to_string(),
            });
            return None;
        };
        // For a generic call site, resolve the monomorphized callee symbol by
        // mangling the origin name with the per-site concrete type arguments.
        let callee_sym: String =
            if let Some(type_args) = self.call_site_type_args.get(&site).cloned() {
                if type_args.is_empty() {
                    name.clone()
                } else {
                    let substituted: Vec<ResolvedTy> =
                        type_args.iter().map(|t| self.subst_ty(t)).collect();
                    let mangled = hew_hir::monomorph::mangle(name, &substituted);
                    if !self.module_fn_names.contains(&mangled) {
                        self.diagnostics.push(MirDiagnostic {
                            kind: MirDiagnosticKind::NotYetImplemented {
                                construct: "spawned call".to_string(),
                                site,
                            },
                            note: format!(
                                "generic spawn of `{name}` has no registered \
                                 monomorphization `{mangled}`; HIR must emit this \
                                 instantiation before task spawning can proceed"
                            ),
                        });
                        return None;
                    }
                    mangled
                }
            } else {
                name.clone()
            };
        // Safety guard: a generic function without recorded type arguments
        // has no resolvable monomorphization and cannot be spawned.
        if callee_sym == *name && self.module_generic_fn_names.contains(name) {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "spawned call".to_string(),
                    site,
                },
                note: "generic function spawned without a resolved concrete \
                       instantiation; provide explicit type arguments"
                    .to_string(),
            });
            return None;
        }
        if !self.module_fn_names.contains(&callee_sym) {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "spawned call".to_string(),
                    site,
                },
                note: format!(
                    "arg-bearing task spawn callee `{callee_sym}` is not a registered module function"
                ),
            });
            return None;
        }
        if !matches!(inner, ResolvedTy::Unit) {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "spawned call".to_string(),
                    site,
                },
                note: "arg-bearing task spawn currently requires a unit-returning callee; \
                       value/result task propagation remains fail-closed"
                    .to_string(),
            });
            return None;
        }
        // Per-arg type restriction (this slice): BitCopy scalars + string.
        // Anything with non-trivial drop glue or interior ownership refuses
        // here — transferring it through the byte-copied env without a
        // retain/drop story would miscompile, and we fail closed instead.
        let mut arg_tys = Vec::with_capacity(args.len());
        for arg in args {
            let arg_ty = self.subst_ty(&arg.ty);
            let class = ValueClass::of_ty(&arg_ty, &self.type_classes);
            let allowed = class == ValueClass::BitCopy || matches!(arg_ty, ResolvedTy::String);
            if !allowed {
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::NotYetImplemented {
                        construct: "spawned call argument".to_string(),
                        site,
                    },
                    note: format!(
                        "task spawn argument of type `{}` is not yet supported; \
                         this slice transfers BitCopy scalars and `string` only — \
                         richer owned types need an env retain/drop plan first",
                        arg_ty.user_facing()
                    ),
                });
                return None;
            }
            arg_tys.push(arg_ty);
        }
        // Lower args left-to-right. Owned bindings (e.g. a string) record
        // their consume fact here, which removes them from the parent's
        // drop plan — ownership rides the env bytes into the child.
        let mut arg_places = Vec::with_capacity(args.len());
        for arg in args {
            arg_places.push(self.lower_value(arg)?);
        }

        // Pack the lowered args into a fork-env record (RecordInit memcpys
        // the fields; the layout registers alongside closure env records).
        let fork_id = self.next_closure_id;
        self.next_closure_id = self
            .next_closure_id
            .checked_add(1)
            .expect("closure id overflow");
        let owner = Self::sanitize_symbol_component(&self.current_function_symbol);
        let env_name = format!("__hew_fork_env_{owner}_{fork_id}");
        let shim_name = format!("__hew_fork_entry_{owner}_{fork_id}");
        let env_ty = ResolvedTy::Named {
            name: env_name.clone(),
            args: vec![],
            builtin: None,
            is_opaque: false,
        };
        self.closure_record_layouts
            .push(crate::model::RecordLayout {
                name: env_name,
                field_tys: arg_tys.clone(),
                // Compiler-internal fork-env record: positional `-g` names.
                field_names: Vec::new(),
            });
        let field_pairs: Vec<(FieldOffset, Place)> = arg_places
            .iter()
            .enumerate()
            .map(|(idx, place)| {
                (
                    FieldOffset(u32::try_from(idx).expect("fork arg count exceeds u32::MAX")),
                    *place,
                )
            })
            .collect();
        let env_place = self.alloc_local(env_ty.clone());
        self.push_instr(Instr::RecordInit {
            ty: env_ty.clone(),
            fields: field_pairs,
            dest: env_place,
        });

        let lowered = self.synthesize_fork_entry_shim(&callee_sym, &shim_name, &arg_tys, &env_ty);
        self.generated_functions.push(lowered);

        let task_place = self.alloc_local(task_ty.clone());
        self.push_runtime_call("hew_task_new", vec![], Some(task_place));
        self.push_runtime_call("hew_task_scope_spawn", vec![scope_place, task_place], None);
        self.push_instr(Instr::SpawnTaskClosure {
            task: task_place,
            fn_symbol: shim_name,
            env: env_place,
            env_ty,
            env_ownership: arg_tys
                .iter()
                .map(|ty| {
                    if ValueClass::of_ty(ty, &self.type_classes) == ValueClass::BitCopy {
                        SpawnEnvFieldOwnership::BorrowsOnly
                    } else {
                        SpawnEnvFieldOwnership::OwnsMoved
                    }
                })
                .collect(),
        });
        Some(task_place)
    }

    /// Synthesize the fork-entry shim for an arg-bearing task spawn: a
    /// `ClosureInvoke`-ABI function `(ctx, env_ptr)` that loads each arg
    /// back out of the env record and calls the target function with them.
    ///
    /// Loading an owned string capture retains it so the environment remains
    /// valid while the call runs. The shim balances that temporary retain after
    /// the callee returns; this does not release the moved environment owner.
    #[expect(
        clippy::too_many_lines,
        reason = "shim construction keeps raw/checked/elaborated MIR snapshots aligned, \
                  mirroring lower_closure_shim / lower_named_fn_invoke_shim"
    )]
    fn synthesize_fork_entry_shim(
        &self,
        callee_symbol: &str,
        shim_name: &str,
        arg_tys: &[ResolvedTy],
        env_ty: &ResolvedTy,
    ) -> LoweredFunction {
        let env_ptr_ty = Self::closure_env_pointer_ty(env_ty);
        let mut builder = Builder {
            type_classes: self.type_classes.clone(),
            record_field_orders: self.record_field_orders.clone(),
            actor_layouts: self.actor_layouts.clone(),
            supervisor_layout_map: self.supervisor_layout_map.clone(),
            machine_layout_names: self.machine_layout_names.clone(),
            module_fn_names: self.module_fn_names.clone(),
            module_generic_fn_names: self.module_generic_fn_names.clone(),
            funcupdate_fn_returns_fresh: self.funcupdate_fn_returns_fresh.clone(),
            param_ownership: self.param_ownership.clone(),
            subst: self.subst.clone(),
            call_site_type_args: self.call_site_type_args.clone(),
            supervisor_child_slots: self.supervisor_child_slots.clone(),
            actor_send_aliasing: self.actor_send_aliasing.clone(),
            pointer_width: self.pointer_width,
            current_function_symbol: shim_name.to_string(),
            current_function_call_conv: crate::model::FunctionCallConv::ClosureInvoke,
            task_entry_adapter_symbols: self.task_entry_adapter_symbols.clone(),
            // #2648 — synthetic call wrapper (no user match scrutinees), but the
            // provenance context is threaded uniformly: no child builder falls
            // back to the legacy fail-open default.
            call_scrutinee_provenance: self.call_scrutinee_provenance.clone(),
            ..Builder::default()
        };

        // locals[0] = env_ptr (ClosureInvoke ABI). Each arg is loaded out of
        // the env record into a fresh temp and forwarded to the callee.
        let env_place = builder.alloc_local(env_ptr_ty.clone());
        let mut arg_places = Vec::with_capacity(arg_tys.len());
        for (idx, ty) in arg_tys.iter().enumerate() {
            let dest = builder.alloc_local(ty.clone());
            builder.instructions.push(Instr::ClosureEnvFieldLoad {
                env: env_place,
                env_ty: env_ty.clone(),
                field_offset: FieldOffset(
                    u32::try_from(idx).expect("fork arg count exceeds u32::MAX"),
                ),
                dest,
            });
            arg_places.push(dest);
        }

        let ret_block_id = builder.alloc_block();
        builder.finish_current_block(Terminator::Call {
            callee: callee_symbol.to_string(),
            builtin: None,
            args: arg_places.clone(),
            dest: None,
            next: ret_block_id,
        });
        builder.start_block(ret_block_id);
        for (place, ty) in arg_places.iter().zip(arg_tys) {
            if matches!(ty, ResolvedTy::String) {
                builder.instructions.push(Instr::Drop {
                    place: *place,
                    ty: ty.clone(),
                    drop_fn: Some(crate::model::DropFnSpec::Release("hew_string_drop")),
                });
            }
        }
        let blocks = builder.finalize_blocks(Terminator::Return);

        let thir_statements: Vec<MirStatement> = blocks
            .iter()
            .flat_map(|b| b.statements.iter().cloned())
            .collect();
        let thir = ThirFunction {
            name: shim_name.to_string(),
            return_ty: ResolvedTy::Unit,
            statements: thir_statements,
        };
        let mut raw = RawMirFunction {
            name: shim_name.to_string(),
            return_ty: ResolvedTy::Unit,
            call_conv: crate::model::FunctionCallConv::ClosureInvoke,
            params: vec![env_ptr_ty],
            locals: builder.locals.clone(),
            // Synthesised closure-invoke shim: no faithful user bindings.
            local_names: Vec::new(),
            local_scopes: Vec::new(),
            local_decl_bytes: Vec::new(),
            scope_table: Vec::new(),
            blocks: blocks.clone(),
            decisions: builder.decisions.clone(),
            intrinsic_id: None,
            await_deadline_ns: builder.await_deadline_ns.clone(),
            suspend_kinds: builder.suspend_kinds.clone(),
            lambda_actor_user_param_locals: Vec::new(),
            span: None,
            instr_spans: ::std::collections::BTreeMap::new(),
            source_origin: SourceOrigin::Unknown,
        };
        // Synthetic HirFn for dataflow checking — no HIR params (the shim
        // locals are positional, not HIR bindings).
        let synthetic_func = HirFn {
            id: hew_hir::ItemId(0),
            node: hew_hir::HirNodeId(0),
            name: shim_name.to_string(),
            type_params: Vec::new(),
            is_generator: false,
            intrinsic_id: None,
            params: Vec::new(),
            return_ty: ResolvedTy::Unit,
            body: hew_hir::HirBlock {
                node: hew_hir::HirNodeId(0),
                scope: hew_hir::ScopeId(0),
                statements: Vec::new(),
                tail: None,
                ty: ResolvedTy::Unit,
                span: 0..0,
            },
            span: 0..0,
        };
        let dataflow_result = check_function(&builder, &raw.blocks, &synthetic_func);
        let mut diagnostics: Vec<MirDiagnostic> = dataflow_result
            .checks
            .iter()
            .filter_map(check_to_diagnostic)
            .collect();
        diagnostics.append(&mut builder.diagnostics);
        collect_unknown_type_diagnostics(&synthetic_func, &builder, &mut diagnostics);
        let string_derivation = finalize_string_ownership(&mut raw, &builder, &dataflow_result);
        let bytes_derivation = finalize_bytes_ownership(&mut raw, &builder, &dataflow_result);
        let cooperate_sites = dataflow::compute_cooperate_sites(&raw.blocks);
        let checked = CheckedMirFunction {
            name: shim_name.to_string(),
            return_ty: ResolvedTy::Unit,
            blocks: raw.blocks.clone(),
            decisions: builder.decisions.clone(),
            checks: dataflow_result.checks.clone(),
            cooperate_sites,
        };
        let elaborated = elaborate(
            &checked,
            &builder,
            &thir.statements,
            &dataflow_result,
            Some(&string_derivation.allowed),
            Some(&bytes_derivation.allowed),
        );

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

    fn lower_spawned_closure_task(
        &mut self,
        callee: &HirExpr,
        args: &[HirExpr],
        task_ty: &ResolvedTy,
        inner: &ResolvedTy,
        scope_place: Place,
        site: hew_hir::SiteId,
    ) -> Option<Place> {
        let HirExprKind::Closure {
            params,
            ret_ty,
            body,
            captures,
            escape_kind: _,
        } = &callee.kind
        else {
            unreachable!("caller checked closure callee");
        };
        if !args.is_empty()
            || !params.is_empty()
            || !matches!(inner, ResolvedTy::Unit)
            || !matches!(ret_ty, ResolvedTy::Unit)
        {
            for arg in args {
                let _ = self.lower_value(arg);
            }
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "spawned closure".to_string(),
                    site,
                },
                note: "spawned-closure task lowering supports only zero-argument closures \
                       returning unit; value/result task propagation remains fail-closed"
                    .to_string(),
            });
            return None;
        }
        if let Some(capture) = captures.iter().find(|capture| !capture.is_send) {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "spawned closure non-Send capture".to_string(),
                    site,
                },
                note: format!(
                    "closure capture `{}` is not Send; refusing to transfer its environment \
                     across the spawned task boundary",
                    capture.name
                ),
            });
            return None;
        }
        let (fn_symbol, env_ty, env_place, _suspends) = self.materialize_closure_env(
            callee,
            params,
            ret_ty,
            body,
            captures,
            crate::closure_env::AllocationStrategy::ScopeOwned,
        )?;
        let task_place = self.alloc_local(task_ty.clone());
        self.push_runtime_call("hew_task_new", vec![], Some(task_place));
        self.push_runtime_call("hew_task_scope_spawn", vec![scope_place, task_place], None);
        self.push_instr(Instr::SpawnTaskClosure {
            task: task_place,
            fn_symbol,
            env: env_place,
            env_ty,
            env_ownership: vec![SpawnEnvFieldOwnership::BorrowsOnly; captures.len()],
        });
        Some(task_place)
    }

    fn lower_fork_block_task(&mut self, body: &HirBlock, site: hew_hir::SiteId) -> Option<Place> {
        let expr = if body.statements.len() == 1 && body.tail.is_none() {
            let HirStmtKind::Expr(expr) = &body.statements[0].kind else {
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::NotYetImplemented {
                        construct: "fork block cancellation child".to_string(),
                        site,
                    },
                    note: "fork block task lowering currently supports expression-call statements only"
                        .to_string(),
                });
                return None;
            };
            expr
        } else if body.statements.is_empty() {
            let Some(tail) = &body.tail else {
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::NotYetImplemented {
                        construct: "fork block cancellation child".to_string(),
                        site,
                    },
                    note: "fork block task lowering requires a no-argument unit function call"
                        .to_string(),
                });
                return None;
            };
            tail
        } else {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "fork block cancellation child".to_string(),
                    site,
                },
                note: "fork block task lowering currently supports exactly one \
                       statement: a no-argument unit function call"
                    .to_string(),
            });
            return None;
        };
        let HirExprKind::Call { callee, args } = &expr.kind else {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "fork block cancellation child".to_string(),
                    site,
                },
                note: "fork block task lowering currently supports a direct function call body"
                    .to_string(),
            });
            return None;
        };
        let task_ty = ResolvedTy::Task(Box::new(ResolvedTy::Unit));
        // Use the inner call's site (expr.site) so that generic call-site type
        // arguments recorded by HIR for the inner call are visible to the
        // spawn lowering path. The outer ForkBlock site differs from the inner
        // Call site, and call_site_type_args is keyed on the inner site.
        self.lower_spawned_call_task(callee, args, &task_ty, false, expr.site)
    }

    fn lower_scope_deadline(
        &mut self,
        duration: &HirExpr,
        body: &HirBlock,
        site: hew_hir::SiteId,
    ) -> Option<Place> {
        let Some(scope_place) = self.current_task_scope else {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "scope deadline cancellation edge".to_string(),
                    site,
                },
                note: "deadline reached MIR without an active task scope token".to_string(),
            });
            return None;
        };
        // Deadline bodies lower later on this same Builder, after the outer
        // function pre-pass has already completed. Refresh the fixed-point
        // facts here so body-local fn producers cannot bypass the generator
        // provenance gate.
        self.collect_prepass_facts(body);
        let has_body = !body.statements.is_empty() || body.tail.is_some();

        // Empty `after(d) {}` keeps the legacy per-deadline-thread cancel on BOTH
        // call-convs — there is no body to route to, so nothing suspends. The
        // deadline simply cancels the scope's children when it fires.
        if !has_body {
            let duration_place = self.lower_value(duration)?;
            self.push_runtime_call(
                "hew_task_scope_cancel_after_ns",
                vec![scope_place, duration_place],
                None,
            );
            let unit_place = self.alloc_local(ResolvedTy::Unit);
            self.push_instr(Instr::UnitLit { dest: unit_place });
            return Some(unit_place);
        }

        // A NON-EMPTY `after(d) { body }` from a SUSPENDABLE caller (actor handler
        // / closure / task entry) lowers onto the `SuspendingScopeDeadline`
        // carrier: codegen arms a deadline on the global timer wheel carrying the
        // parked continuation, the scope's children run, and the FIRST of {all
        // children joined, deadline fired} wins the shared one-shot arbiter. The
        // deadline-wins edge routes to `timeout_body_block` (the lowered `after`
        // body); the join-wins edge skips it. A `FunctionCallConv::Default` caller
        // has no parkable continuation, so the non-empty timeout body stays
        // fail-closed there (mirrors the suspending await / sleep / select flips).
        if !self.current_function_call_conv.carries_execution_context() {
            // Lower the duration + body operands for diagnostics coherence, then
            // fail closed: a contextless caller cannot park to run the body.
            let _ = self.lower_value(duration);
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "scope deadline body".to_string(),
                    site,
                },
                note: "a non-empty after(...) timeout body is only lowered from an \
                       execution-context caller (actor handler / closure / task \
                       entry); a contextless caller has no parkable continuation to \
                       run the body on the deadline edge"
                    .to_string(),
            });
            return None;
        }

        let duration_place = self.lower_value(duration)?;

        // Allocate the timer-fired body block + the convergence (resume) block.
        // The carrier's `resume` is the scope-complete path AND the point the
        // timeout body falls through to; `cleanup` reuses `resume` exactly as the
        // await / sleep / select carriers do (the coro `cleanup` outline is the
        // abandon teardown owner, not this MIR block).
        let timeout_body_block = self.alloc_block();
        let resume = self.alloc_block();

        self.finish_current_block(Terminator::SuspendingScopeDeadline {
            scope: scope_place,
            duration_ms: duration_place,
            timeout_body_block,
            resume,
            cleanup: resume,
        });

        // The deadline-fired edge: lower the `after(...)` body, then converge on
        // `resume`. The body runs ONLY on the timeout edge; the join-wins resume
        // edge branches straight into `resume` from the coro switch.
        self.start_block(timeout_body_block);
        for stmt in &body.statements {
            self.stmt(stmt);
        }
        if let Some(tail) = &body.tail {
            let _ = self.lower_value(tail);
        }
        self.finish_current_block(Terminator::Goto { target: resume });

        self.start_block(resume);
        let unit_place = self.alloc_local(ResolvedTy::Unit);
        self.push_instr(Instr::UnitLit { dest: unit_place });
        Some(unit_place)
    }

    fn lower_await_task(
        &mut self,
        binding_name: &str,
        binding_id: BindingId,
        output_ty: &ResolvedTy,
        site: hew_hir::SiteId,
    ) -> Option<Place> {
        let is_value_task = !matches!(output_ty, ResolvedTy::Unit);
        let Some(task_place) = self.binding_locals.get(&binding_id).copied() else {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::UnresolvedPlace {
                    binding: binding_id,
                    name: "<await-task>".to_string(),
                    site,
                },
                note: "await task binding has no backend task handle slot".to_string(),
            });
            return None;
        };
        // `await t` consumes the linear task handle. Record the consume fact
        // so the dataflow MustConsume exit check sees the handle as used and
        // a second `await t` reports UseAfterConsume. Mirrors the
        // `BindingRef { intent: Consume }` path in `lower_value`.
        self.statements.push(MirStatement::Use {
            binding: binding_id,
            name: binding_name.to_string(),
            site,
            ty: ResolvedTy::Task(Box::new(output_ty.clone())),
            intent: IntentKind::Consume,
        });
        self.mark_binding_moved(binding_id);

        // Suspendable-caller flip: in a caller that carries the execution
        // context (actor handler / closure / task entry) `await t` SUSPENDS on
        // the child task's completion instead of blocking the worker in
        // `hew_task_await_blocking` (a condvar). The completion observer
        // (`hew_task_await_suspend`) re-enqueues the parked continuation on the
        // child's `Done`. A `FunctionCallConv::Default` caller (`main`, free fn)
        // has no parkable continuation and keeps the blocking call. Reuses the
        // same `carries_execution_context` discriminator as the recv/ask flips.
        if self.current_function_call_conv.carries_execution_context() {
            if let Some(scope_place) = self.current_task_scope {
                // Value task: allocate the slot the child's `T` is read into on
                // the resume edge (codegen reads `hew_task_get_result` into it,
                // copying the result-buffer bytes at the `T` element width). A
                // unit task binds nothing.
                let result_dest = if is_value_task {
                    Some(self.alloc_local(output_ty.clone()))
                } else {
                    None
                };
                let next = self.alloc_block();
                // The carrier rides the multi-suspend epilogue, so `cleanup`
                // reuses `next` exactly as the recv/ask carriers do.
                self.record_suspend_kind(SuspendKind::TaskAwait {
                    scope: scope_place,
                    task: task_place,
                    result_dest,
                });
                self.finish_current_block(Terminator::Suspend {
                    resume: next,
                    cleanup: next,
                    is_final: false,
                });
                self.start_block(next);
                if let Some(result_dest) = result_dest {
                    // The resume edge bound the child's `T` into `result_dest`.
                    return Some(result_dest);
                }
                let unit_place = self.alloc_local(ResolvedTy::Unit);
                self.push_instr(Instr::UnitLit { dest: unit_place });
                return Some(unit_place);
            }
        }

        // Contextless keep path: a `FunctionCallConv::Default` caller has no
        // parkable continuation and blocks the foreign thread on the condvar.
        // Value tasks cannot reach this path: `lower_spawned_call_task` refuses
        // to spawn from a Default-callconv caller, so a value `Task<T>` handle
        // never exists in a contextless awaiter. Fail closed rather than emit a
        // blocking read whose result-width copy was never proven on this path.
        if is_value_task {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "await task result".to_string(),
                    site,
                },
                note: "awaiting a value-returning task is only lowered from an \
                       execution-context caller (actor handler / closure / task \
                       entry); a `Task<T>` handle cannot reach a Default-callconv \
                       awaiter because value tasks cannot be spawned there"
                    .to_string(),
            });
            return None;
        }
        self.push_runtime_call("hew_task_await_blocking", vec![task_place], None);
        let unit_place = self.alloc_local(ResolvedTy::Unit);
        self.push_instr(Instr::UnitLit { dest: unit_place });
        Some(unit_place)
    }

    /// Lower `link(target)` / `monitor(target)` to `Instr::CallRuntimeAbi`,
    /// constructing the composite return value (`Result<(), LinkError>` or
    /// `MonitorRef`) when the call is in value-needed context.
    ///
    /// **Statement-position** (`context == Discarded`): emit the ABI call with
    /// `dest = None`. The codegen handler calls `hew_actor_link` / discards the
    /// `hew_actor_monitor` u64 return — no composite needed.
    ///
    /// **Value-needed** (`context == ValueNeeded`): two shapes:
    ///
    /// - `link` (void runtime ABI): allocate a `Result<(), LinkError>` dest
    ///   local from the checker-authoritative `result_ty`, emit the ABI call
    ///   with `dest = Some(result_local)`. The codegen handler (`runtime_abi.rs`
    ///   `ActorLink` arm) detects the dest and calls `emit_result_ok(dest, None)`
    ///   to write `tag = 0` (Ok, no payload) — the only shape because
    ///   `hew_actor_link` is void/infallible at the runtime today.
    ///
    /// - `monitor` (i64 runtime ABI → `MonitorRef` struct): allocate a raw `i64`
    ///   dest for the runtime return, emit the ABI call storing the `ref_id` into
    ///   it, then emit `Instr::RecordInit` to assemble `MonitorRef { ref_id }`
    ///   into a freshly-allocated `MonitorRef` local from `result_ty`. Returns
    ///   the `MonitorRef` local.
    ///
    /// **Null-self window (INTERIM):** `hew_actor_self()` returns null outside an
    /// actor dispatch context. The "only valid in actor context" fail-closed gate
    /// belongs to the actor-messaging lane and is not installed here. Calls from
    /// `main` / free functions are an out-of-context window; the runtime handles
    /// null self gracefully (`hew_actor_monitor` returns `ref_id = 0`).
    fn lower_actor_link_or_monitor(
        &mut self,
        symbol: &str,
        hir_args: &[hew_hir::HirExpr],
        site: hew_hir::SiteId,
        context: RuntimeCallContext,
        result_ty: Option<&ResolvedTy>,
    ) -> Option<Place> {
        // ARITY: the user-facing surface is 1-arg — `link(target)` /
        // `monitor(target)`. The linking/monitoring subject is the implicit
        // calling actor (`self`), matching Erlang/OTP `link(Pid)` /
        // `monitor(process, Pid)`. The 2-arg runtime ABI
        // (`hew_actor_link(parent, child)` / `hew_actor_monitor(watcher,
        // target)`) is satisfied by synthesizing `hew_actor_self()` as arg0
        // and the user target as arg1.
        if hir_args.len() != 1 {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: format!("runtime call `{symbol}` arity"),
                    site,
                },
                note: format!(
                    "`{symbol}` expects exactly 1 argument (target), got {}",
                    hir_args.len()
                ),
            });
            return None;
        }

        // Cross-node monitor: a `monitor(RemotePid<T>)` receiver routes
        // to the node-monitor ABI instead of the in-process actor-monitor ABI.
        // The remote receiver has no `HewActor*` in this address space, so the
        // 2-arg (watcher_ptr, target_ptr) shape does not apply: `hew_node_monitor`
        // takes the packed remote pid (an i64) and registers a distributed-table
        // entry keyed by (node_id, serial). Detected from the target argument's
        // checker-authoritative resolved type. Only `hew_actor_monitor` reaches
        // the cross-node route; `hew_actor_link` of a remote target is deferred
        // (no cross-node link surface), so it keeps the local 2-arg shape.
        let target_ty = self.subst_ty(&hir_args[0].ty);
        if symbol == "hew_actor_monitor" && actor_name_from_remote_pid_ty(&target_ty).is_some() {
            return self.lower_node_monitor(hir_args, site, context, result_ty);
        }

        // arg1: the user-provided target handle. Lower it first so a failure
        // to lower the target is reported before we emit the self-handle call.
        let target = self.lower_value(&hir_args[0])?;

        // arg0: synthesize the implicit `self` subject via `hew_actor_self()`.
        let self_handle = self.emit_actor_self_handle();

        if context != RuntimeCallContext::ValueNeeded {
            // Statement-position (Discarded): emit with dest=None. The codegen
            // handler calls the C ABI and ignores the return.
            self.push_runtime_call(symbol, vec![self_handle, target], None);
            return None;
        }

        // Value-needed: construct the composite return.
        // `result_ty` carries the checker-authoritative return type:
        //   hew_actor_link   → Result<(), LinkError>
        //   hew_actor_monitor → MonitorRef
        // Fail closed if the checker did not record a return type — a
        // checker-boundary violation that the HIR→MIR handoff must not
        // propagate silently.
        let composite_ty = if let Some(ty) = result_ty {
            ty.clone()
        } else {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: format!("runtime call `{symbol}` value result"),
                    site,
                },
                note: format!(
                    "`{symbol}` value-needed path requires a checker-authoritative \
                     result type; result_ty was None (checker did not record a type \
                     for this call site — boundary violation)"
                ),
            });
            return None;
        };

        match symbol {
            "hew_actor_link" => {
                // hew_actor_link is void/infallible. Allocate the
                // Result<(), LinkError> dest and pass it to the ABI call.
                // The codegen ActorLink handler sees the dest and emits
                // `emit_result_ok(dest, None)` to write tag=0 (Ok, no payload).
                let result_local = self.alloc_local(composite_ty);
                self.push_runtime_call(symbol, vec![self_handle, target], Some(result_local));
                Some(result_local)
            }
            "hew_actor_monitor" => {
                // hew_actor_monitor returns a u64 ref_id. Store the raw i64
                // into a temp local, then assemble MonitorRef { ref_id } via
                // RecordInit. The codegen ActorMonitor handler stores the i64
                // call result into the raw dest; RecordInit copies it into the
                // struct field.
                let ref_id_local = self.alloc_local(ResolvedTy::I64);
                self.push_runtime_call(symbol, vec![self_handle, target], Some(ref_id_local));
                // Assemble MonitorRef { ref_id: i64 } from the raw ref_id.
                // FieldOffset(0) is the declaration-order index of `ref_id`
                // in `MonitorRef { ref_id: i64 }`.
                let monitor_ref_local = self.alloc_local(composite_ty.clone());
                self.push_instr(Instr::RecordInit {
                    ty: composite_ty,
                    fields: vec![(FieldOffset(0), ref_id_local)],
                    dest: monitor_ref_local,
                });
                Some(monitor_ref_local)
            }
            _ => unreachable!("only hew_actor_link / hew_actor_monitor reach this helper"),
        }
    }

    /// Lower a cross-node `monitor(RemotePid<T>)` to the node-monitor ABI.
    ///
    /// Unlike the local `hew_actor_monitor(watcher_ptr, target_ptr)` (which keys
    /// on `HewActor*` pointers), the remote target has no pointer in this
    /// address space. `hew_node_monitor(target_remote_pid: i64) -> i64` takes the
    /// packed remote pid and registers a distributed-table entry keyed by
    /// `(node_id, serial)`, sending a `CTRL_MONITOR_REQ` to the owning peer.
    /// Positive returns are the `ref_id` keying the registration; negative
    /// returns encode `MonitorError` as `-(variant + 1)`. Codegen assembles the
    /// checker-authoritative `Result<MonitorRef, MonitorError>` directly.
    fn lower_node_monitor(
        &mut self,
        hir_args: &[hew_hir::HirExpr],
        site: hew_hir::SiteId,
        context: RuntimeCallContext,
        result_ty: Option<&ResolvedTy>,
    ) -> Option<Place> {
        // The target is the remote pid; it lowers to a bare i64 (node<<48|serial).
        let target = self.lower_value(&hir_args[0])?;

        if context != RuntimeCallContext::ValueNeeded {
            // Statement-position monitor: register but discard the ref. The
            // codegen handler calls the ABI and ignores the signed setup return.
            self.push_runtime_call("hew_node_monitor", vec![target], None);
            return None;
        }

        // Value-needed: the checker records
        // `Result<MonitorRef, MonitorError>` as the result type.
        // Fail closed if it did not — a HIR→MIR boundary violation.
        let composite_ty = if let Some(ty) = result_ty {
            ty.clone()
        } else {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "runtime call `hew_node_monitor` value result".to_string(),
                    site,
                },
                note: "`hew_node_monitor` value-needed path requires a \
                       checker-authoritative result type; result_ty was None \
                       (checker did not record a type for this call site — \
                       boundary violation)"
                    .to_string(),
            });
            return None;
        };

        // Codegen decodes the signed setup return and writes either
        // Ok(MonitorRef { ref_id }) or Err(MonitorError::<variant>) in place.
        let result_local = self.alloc_local(composite_ty);
        self.push_runtime_call("hew_node_monitor", vec![target], Some(result_local));
        Some(result_local)
    }

    /// Lower a cross-node `link_remote(RemotePid<T>, PartitionPolicy)` to the
    /// node-link ABI.
    ///
    /// The user surface is 2-arg: the remote target pid and the `PartitionPolicy`
    /// governing what happens to the LOCAL linked actor when the remote dies. The
    /// linking subject (self) is resolved inside the runtime (like
    /// `hew_node_monitor` / `hew_actor_self`), so the ABI takes only
    /// `(target_pid: i64, policy_tag: i64)`. The target lowers to a bare i64
    /// (node<<48|serial); the policy is a fieldless enum whose discriminant tag is
    /// extracted via `Place::EnumTag` and passed as the `policy_tag` i64.
    ///
    /// The return is `Result<(), LinkError>` (matching the local `link`). Positive
    /// ABI returns signal registration success; negative returns encode
    /// `LinkError` as `-(variant + 1)`. The EXIT arrives asynchronously when the
    /// remote dies.
    fn lower_node_link_remote(
        &mut self,
        hir_args: &[hew_hir::HirExpr],
        site: hew_hir::SiteId,
        context: RuntimeCallContext,
        result_ty: Option<&ResolvedTy>,
    ) -> Option<Place> {
        if hir_args.len() != 2 {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "runtime call `hew_node_link_remote` arity".to_string(),
                    site,
                },
                note: format!(
                    "`link_remote` expects exactly 2 arguments (remote pid, policy), got {}",
                    hir_args.len()
                ),
            });
            return None;
        }

        // arg0: the remote pid (a bare i64 node<<48|serial).
        let target = self.lower_value(&hir_args[0])?;
        // arg1: the PartitionPolicy fieldless enum value; extract its discriminant
        // tag into an i64 the ABI consumes as the policy selector.
        let policy_value = self.lower_value(&hir_args[1])?;
        let Place::Local(policy_local) = policy_value else {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "runtime call `hew_node_link_remote` policy operand".to_string(),
                    site,
                },
                note: "`link_remote` lowers the PartitionPolicy through the \
                       tagged-union discriminant; the policy operand must first \
                       materialise as an enum local"
                    .to_string(),
            });
            return None;
        };
        let policy_tag = self.alloc_local(ResolvedTy::I64);
        self.push_instr(Instr::Move {
            dest: policy_tag,
            src: Place::EnumTag(policy_local),
        });

        if context != RuntimeCallContext::ValueNeeded {
            // Statement-position: register the link, discard the ref_id.
            self.push_runtime_call("hew_node_link_remote", vec![target, policy_tag], None);
            return None;
        }

        // Value-needed: the checker records `Result<(), LinkError>` as the result.
        // Fail closed if it did not — a HIR→MIR boundary violation.
        let composite_ty = if let Some(ty) = result_ty {
            ty.clone()
        } else {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "runtime call `hew_node_link_remote` value result".to_string(),
                    site,
                },
                note: "`hew_node_link_remote` value-needed path requires a \
                       checker-authoritative result type; result_ty was None \
                       (checker did not record a type for this call site — \
                       boundary violation)"
                    .to_string(),
            });
            return None;
        };
        // Allocate the Result<(), LinkError> dest and pass it to the ABI call.
        // Codegen decodes the signed setup return into Ok(()) or the precise Err.
        let result_local = self.alloc_local(composite_ty);
        self.push_runtime_call(
            "hew_node_link_remote",
            vec![target, policy_tag],
            Some(result_local),
        );
        Some(result_local)
    }

    /// Emit `Instr::CallRuntimeAbi` for discarded `unlink` calls.
    ///
    /// The user-facing surface is `unlink(target)` — 1 arg. The runtime ABI
    /// is `hew_actor_unlink(self_ptr, target_ptr)` — 2 args. The calling
    /// actor is the implicit first argument, synthesized via
    /// `hew_actor_self()`, mirroring `lower_actor_link_or_monitor`.
    ///
    /// `unlink` is a statement-position-only builtin in v0.5: it returns
    /// `Unit` and has no value-needed path, so value-needed context fails
    /// closed here rather than silently discarding the call.
    fn lower_actor_unlink(
        &mut self,
        hir_args: &[hew_hir::HirExpr],
        site: hew_hir::SiteId,
        context: RuntimeCallContext,
    ) -> Option<Place> {
        if hir_args.len() != 1 {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "runtime call `hew_actor_unlink` arity".to_string(),
                    site,
                },
                note: format!(
                    "`hew_actor_unlink` expects exactly 1 argument (target), got {}",
                    hir_args.len()
                ),
            });
            return None;
        }

        if context == RuntimeCallContext::ValueNeeded {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "runtime call `hew_actor_unlink` value result".to_string(),
                    site,
                },
                note: "`hew_actor_unlink` returns unit; use it in statement position only"
                    .to_string(),
            });
            return None;
        }

        // arg1: the user-provided target handle. Lower it first so a failure
        // to lower the target is reported before emitting the self-handle call.
        let target = self.lower_value(&hir_args[0])?;

        // arg0: synthesize the implicit `self` subject via `hew_actor_self()`.
        let self_handle = self.emit_actor_self_handle();

        self.push_runtime_call("hew_actor_unlink", vec![self_handle, target], None);
        None
    }

    /// Pass-through handler for void-returning runtime symbols that carry no
    /// MIR-level composite-return semantics. The sole consumer today is
    /// `hew_actor_demonitor`, called directly from the body of the stdlib
    /// `impl MonitorRef { fn close(self) }` inherent method (lowered when a
    /// program imports `std::link_monitor`); the caller is responsible for
    /// lowering args correctly.
    ///
    /// Returns `None` (unit) in both statement and value-needed position — a
    /// void call appearing as the last expression in a block is valid Hew; the
    /// block yields unit. The `context` parameter is accepted but unused
    /// because the void→unit semantics are uniform across both positions.
    fn lower_simple_void_runtime_call(
        &mut self,
        symbol: &str,
        hir_args: &[hew_hir::HirExpr],
        _site: hew_hir::SiteId,
        _context: RuntimeCallContext,
    ) -> Option<Place> {
        let mut arg_places = Vec::with_capacity(hir_args.len());
        for arg in hir_args {
            let p = self.lower_value(arg)?;
            arg_places.push(p);
        }
        self.push_runtime_call(symbol, arg_places, None);
        None
    }

    /// Emit `Instr::CallRuntimeAbi` for a simple `(...) -> i64` runtime call.
    ///
    /// Lowers each argument to a Place and emits the call. In value position the
    /// i64 return is stored into a fresh i64 dest; in statement position the
    /// return is discarded. Used by the cross-node monitor extern surface
    /// (`hew_node_monitor` / `hew_node_monitor_recv`), whose returns are plain
    /// scalar reason / ref-id codes (no composite spine).
    fn lower_simple_int_runtime_call(
        &mut self,
        symbol: &str,
        hir_args: &[hew_hir::HirExpr],
        _site: hew_hir::SiteId,
        context: RuntimeCallContext,
        _result_ty: Option<&ResolvedTy>,
    ) -> Option<Place> {
        let mut arg_places = Vec::with_capacity(hir_args.len());
        for arg in hir_args {
            let p = self.lower_value(arg)?;
            arg_places.push(p);
        }
        if context != RuntimeCallContext::ValueNeeded {
            self.push_runtime_call(symbol, arg_places, None);
            return None;
        }
        let result_local = self.alloc_local(ResolvedTy::I64);
        self.push_runtime_call(symbol, arg_places, Some(result_local));
        Some(result_local)
    }

    /// Emit `Instr::CallRuntimeAbi` for `hew_duplex_pair`.
    ///
    /// HIR shape (from E1 bridge): `Call { callee: BindingRef("hew_duplex_pair"),
    /// args: [cap_expr] }` — one symmetric capacity arg.
    ///
    /// MIR emission:
    ///   1. Lower `cap_expr` → `cap_place`.
    ///   2. Allocate two fresh `DuplexHandle` locals (N0, N1).
    ///   3. Emit `CallRuntimeAbi { args: [cap, cap, DuplexHandle(N0), DuplexHandle(N1)], dest: None }`.
    ///   4. Allocate a "tuple proxy" `Place::Local(M)` to thread the two
    ///      output Places through the existing `BindingRef` lookup.
    ///   5. Register `tuple_decomp[M] = [DuplexHandle(N0), DuplexHandle(N1)]`.
    ///   6. Return `Some(Local(M))`.
    ///
    /// `TupleIndex` lowering recovers the individual `DuplexHandle` Places from
    /// `tuple_decomp`.  `owned_locals` registration for `a` and `b` happens
    /// naturally in `stmt()` when `let a = __tuple_N.0` stores
    /// `DuplexHandle(N0)` directly into `binding_locals` (see the handle-typed
    /// branch in the `stmt` Let arm).
    fn lower_duplex_pair(
        &mut self,
        hir_args: &[hew_hir::HirExpr],
        site: hew_hir::SiteId,
    ) -> Option<Place> {
        // E1 registers duplex_pair<S, R>(i64) — one symmetric capacity arg.
        // If E1 ever expands to two args (s_cap, r_cap), skip the duplication.
        let cap_place = if hir_args.len() == 1 {
            self.lower_value(&hir_args[0])
        } else if hir_args.len() >= 2 {
            // Future: two-arg form — just lower both and use the first two.
            self.lower_value(&hir_args[0])
        } else {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "hew_duplex_pair with zero args".to_string(),
                    site,
                },
                note: "hew_duplex_pair requires at least one capacity argument".to_string(),
            });
            return None;
        };
        let Some(cap_place) = cap_place else {
            // Capacity expression failed to lower (e.g. nested Unsupported).
            // Diagnostic already recorded; propagate the failure.
            return None;
        };

        // If E1 emits two args, lower the second capacity independently.
        // For the one-arg case, duplicate the single capacity for both slots.
        let r_cap_place = if hir_args.len() >= 2 {
            self.lower_value(&hir_args[1]).unwrap_or(cap_place)
        } else {
            cap_place // symmetric capacity: s_cap == r_cap
        };

        // Allocate two DuplexHandle locals.  The local index is shared
        // between `Place::Local(N)` (for type bookkeeping in `self.locals`)
        // and `Place::DuplexHandle(N)` (for semantic kind tracking in the
        // instruction and drop streams).
        let local0 = self.alloc_local(ResolvedTy::Named {
            name: "Duplex".to_string(),
            args: vec![],
            builtin: Some(hew_types::BuiltinType::Duplex),
            is_opaque: false,
        });
        let Place::Local(n0) = local0 else {
            unreachable!("alloc_local returns Place::Local");
        };
        let dh0 = Place::DuplexHandle(n0);

        let local1 = self.alloc_local(ResolvedTy::Named {
            name: "Duplex".to_string(),
            args: vec![],
            builtin: Some(hew_types::BuiltinType::Duplex),
            is_opaque: false,
        });
        let Place::Local(n1) = local1 else {
            unreachable!("alloc_local returns Place::Local");
        };
        let dh1 = Place::DuplexHandle(n1);

        // Emit the runtime call.  The i32 return (error code) is discarded
        // (`dest: None`); the two DuplexHandle out-params are in args[2..=3].
        // Codegen (E4) interprets DuplexHandle places in args[2..=3] as
        // "pass the address of this local's alloca as *mut *mut DuplexHandle".
        self.push_instr(Instr::CallRuntimeAbi(
            crate::model::RuntimeCall::new(
                "hew_duplex_pair",
                vec![cap_place, r_cap_place, dh0, dh1],
                None,
            )
            .expect("hew_duplex_pair is an allowlisted runtime symbol"),
        ));

        // Create a "tuple proxy" local so TupleIndex lowering can recover dh0/dh1.
        // The proxy carries no runtime value; its index is the key into tuple_decomp.
        // Using `ResolvedTy::Unit` for the proxy type so no spurious UnknownType
        // diagnostic fires for it.
        let proxy = self.alloc_local(ResolvedTy::Unit);
        let Place::Local(proxy_idx) = proxy else {
            unreachable!("alloc_local returns Place::Local");
        };
        self.tuple_decomp.insert(proxy_idx, vec![dh0, dh1]);

        Some(proxy)
    }

    /// Emit `Instr::CallRuntimeAbi` for `hew_supervisor_stop`.
    ///
    /// HIR shape: `Call { callee: BindingRef("supervisor_stop"), args: [sup_expr] }`.
    /// The checker registers `supervisor_stop(sup)` returning `Ty::Unit`, so
    /// this producer returns `None` — Unit is zero-sized and callers handle
    /// `None` as "no destination place" (see `CallClosure` and `CallTraitMethod`
    /// patterns in `lower_value`).
    ///
    /// MIR emission:
    ///   1. Lower `sup_expr` → `sup_place` (a `LocalPid<S>` — opaque ptr).
    ///   2. Emit `CallRuntimeAbi { "hew_supervisor_stop", args: [sup_place], dest: None }`.
    ///   3. Return `None` (Unit result).
    fn lower_supervisor_stop(
        &mut self,
        hir_args: &[hew_hir::HirExpr],
        site: hew_hir::SiteId,
    ) -> Option<Place> {
        if hir_args.len() != 1 {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "supervisor_stop".to_string(),
                    site,
                },
                note: format!(
                    "supervisor_stop expects 1 argument (sup), got {}",
                    hir_args.len()
                ),
            });
            return None;
        }
        let sup_place = self.lower_value(&hir_args[0])?;
        self.push_instr(Instr::CallRuntimeAbi(
            crate::model::RuntimeCall::new("hew_supervisor_stop", vec![sup_place], None)
                .expect("hew_supervisor_stop is an allowlisted runtime symbol"),
        ));
        // Unit return — no destination place.
        None
    }

    /// Emit the MIR sequence for a static supervisor child-slot access (F-04
    /// fungible reference).
    ///
    /// Called from the `HirExprKind::FieldAccess` intercept arm after the
    /// checker has confirmed the LHS is a supervisor with a static child at
    /// `slot_index`. The result is a FUNGIBLE child reference: it names the
    /// `(supervisor, slot)` role, not a specific actor instance, and the current
    /// occupant is RE-RESOLVED at each send/ask (see `emit_fungible_reresolve`).
    /// The accessor itself no longer traps on a not-live slot — liveness is the
    /// send's concern, surfaced there as a recoverable error.
    ///
    /// Produces:
    ///
    /// ```text
    /// entry_bb (current)
    ///   [lower object → sup_place]
    ///   ConstI64 { dest: idx_place, value: slot_index }
    ///   CallRuntimeAbi { "hew_supervisor_child_get",
    ///                    args: [sup_place, idx_place],
    ///                    dest: result_place }
    ///   RecordFieldLoad { record: result_place, field_offset: 1, dest: raw_handle }  -- i64 handle
    ///   Move { dest: handle_place (ActorHandle(N)), src: raw_handle }
    ///   [cursor stays here for subsequent lowering]
    /// ```
    ///
    /// The initial child-get seeds the alloca so an immediate same-expression use
    /// (`sup.w.tick()`) and the eventual binding (`let a = sup.w`) both have a
    /// well-formed handle. The crucial change vs. the snapshot model is that the
    /// handle local is recorded in `fungible_child_refs`, so a later send through
    /// it re-resolves rather than reusing this seed pointer. On a not-live slot at
    /// the accessor the handle is seeded with the (null) wire value; the send
    /// re-resolve is what decides liveness, so the seed value is never trusted as
    /// the send target.
    ///
    /// Returns `Some(handle_place)`. `handle_place` is `Place::ActorHandle(N)`
    /// where N is the backing local index of a freshly allocated
    /// `LocalPid<ChildActor>` local (typed as `result_ty` from the checker — the
    /// checker is the authority on the child actor type).
    ///
    /// S3 codegen interprets `CallRuntimeAbi` with a `__HewChildLookupResult`-typed
    /// dest as a struct-return call, emitting `{ i64, ptr }` in LLVM IR and storing
    /// the struct into the alloca slot. `RecordFieldLoad` at index 1 extracts the
    /// handle pointer (reinterpreted as i64 at the MIR layer; S3 emits `ptrtoint`
    /// when writing to the handle alloca).
    fn lower_supervisor_child_get(
        &mut self,
        object: &HirExpr,
        slot_index: u32,
        result_ty: &ResolvedTy,
        _site: hew_hir::SiteId,
    ) -> Option<Place> {
        // Lower the supervisor object expression to get the supervisor PID place.
        let sup_place = self.lower_value(object)?;

        // Allocate the final ActorHandle place typed as `result_ty`
        // (the checker-authority `LocalPid<ChildActor>` type for this site).
        let handle_local = self.alloc_local(result_ty.clone());
        let Place::Local(handle_id) = handle_local else {
            unreachable!("alloc_local always returns Place::Local");
        };
        let handle_place = Place::ActorHandle(handle_id);

        // Record the fungible reference BEFORE seeding so a re-resolve at any
        // send through `handle_place` knows the `(sup, slot)` to re-fetch.
        self.fungible_child_refs.insert(
            handle_id,
            FungibleChildRef {
                sup_place,
                slot_index,
            },
        );

        // Seed the alloca with one resolve (no trap, no liveness branch — the
        // send re-resolves and is the sole liveness authority).
        self.emit_child_get_into(sup_place, slot_index, handle_place);

        // The `instr_places` function in lower.rs surfaces `handle_place` to the
        // dataflow seed pass, maintaining the same bookkeeping invariant as
        // `lower_spawn_actor`.

        Some(handle_place)
    }

    /// Lower `await_restart sup.child` to `SuspendKind::RestartWait`.
    ///
    /// The supervisor analogue of `lower_await_task`: park the current actor on
    /// the supervisor restart observer until the static child slot is Live again,
    /// then resume re-fetching the now-Live `LocalPid<ChildType>`. Modelled on
    /// the `TaskAwait` suspend ramp; the resume edge re-resolves the slot through
    /// the SAME fungible-reference machinery a static child send uses
    /// (`emit_child_get_into` + `fungible_child_refs`), so the resumed handle is
    /// never a pointer cached across the suspend (LESSONS
    /// `replaceable-resource-handle-is-fungible-reference`, R6).
    ///
    /// `child` is the inner supervised-child accessor (a `FieldAccess`); its
    /// `site` keys `supervisor_child_slots` with the `(supervisor, slot)`
    /// discriminator. The checker already proved the operand is a static child,
    /// so a missing slot here is a lowering invariant break — fail closed.
    ///
    /// A `FunctionCallConv::Default` caller (`main`, free fn) has no parkable
    /// continuation, so it lowers the accessor seed only (the runtime observer's
    /// READY/immediate-Dead path resumes it through the same edge once codegen's
    /// blocking fallback lands); the suspend flip fires only for a caller that
    /// carries an execution context, exactly like `lower_await_task`.
    fn lower_await_restart(
        &mut self,
        child: &HirExpr,
        result_ty: &ResolvedTy,
        site: hew_hir::SiteId,
    ) -> Option<Place> {
        // The inner accessor's site carries the (supervisor, slot) discriminator.
        let Some(slot) = self.supervisor_child_slots.get(&child.site).cloned() else {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "await_restart on a non-supervised-child operand".to_string(),
                    site,
                },
                note: "await_restart lowering found no supervisor child slot for its operand; \
                       the checker should have rejected this"
                    .to_string(),
            });
            return None;
        };

        // The accessor's object is the supervisor PID. Extract + lower it (NOT the
        // whole FieldAccess, which would emit a child_get of its own).
        let HirExprKind::FieldAccess { object, .. } = &child.kind else {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "await_restart operand is not a field access".to_string(),
                    site,
                },
                note:
                    "await_restart expects `sup.child`; its operand lowered to a non-FieldAccess \
                       HIR node"
                        .to_string(),
            });
            return None;
        };
        let sup_place = self.lower_value(object)?;

        // Translate the checker's combined-static index to the kind-partitioned
        // runtime slot index (the accessor reads the same partitioned space).
        let slot_index =
            self.partitioned_static_slot_index(&slot.supervisor, &slot.child_name, false);

        // Allocate the re-fetched handle local, typed as the child's
        // `LocalPid<ChildType>` (the checker authority on this expr's type).
        let handle_local = self.alloc_local(result_ty.clone());
        let Place::Local(handle_id) = handle_local else {
            unreachable!("alloc_local always returns Place::Local");
        };
        let handle_place = Place::ActorHandle(handle_id);

        // Record the fungible reference BEFORE the suspend so a send through the
        // resumed handle re-resolves the `(sup, slot)` at send time rather than
        // trusting any pointer captured across the suspension point.
        self.fungible_child_refs.insert(
            handle_id,
            FungibleChildRef {
                sup_place,
                slot_index,
            },
        );

        // Suspendable-caller flip: a caller that carries the execution context
        // (actor handler / closure / task entry) SUSPENDS on the restart observer
        // instead of spinning. The resume edge re-fetches the now-Live handle.
        if self.current_function_call_conv.carries_execution_context() {
            let next = self.alloc_block();
            self.record_suspend_kind(SuspendKind::RestartWait {
                sup_place,
                slot_index,
                result_dest: handle_place,
                deadline_result_dest: None,
            });
            self.finish_current_block(Terminator::Suspend {
                resume: next,
                cleanup: next,
                is_final: false,
            });
            self.start_block(next);
            // On resume the child slot is Live (notify_restart fires AFTER
            // store_child_slot); re-fetch it through the fungible re-resolve so
            // the handle reflects the new incarnation, never a stale snapshot.
            self.emit_child_get_into(sup_place, slot_index, handle_place);
            return Some(handle_place);
        }

        // Contextless caller (`main` / free fn): no parkable continuation, so
        // BLOCK the calling thread on the supervisor restart Condvar until the
        // child is Live or permanently Dead, then re-fetch. Safe to thread-block
        // here: `main` runs off the cooperative scheduler that fires the restart
        // (no self-deadlock), exactly as a contextless `await` blocks on its ask.
        // The R4 fail-closed contract holds: a permanent-Dead slot returns from
        // the blocking wait and the re-fetch surfaces the dead slot recoverably.
        let idx_place = self.alloc_local(ResolvedTy::I64);
        self.push_instr(Instr::ConstI64 {
            dest: idx_place,
            value: i64::from(slot_index),
        });
        self.push_instr(Instr::CallRuntimeAbi(
            crate::model::RuntimeCall::new(
                "hew_supervisor_restart_await_blocking",
                vec![sup_place, idx_place],
                None,
            )
            .expect("hew_supervisor_restart_await_blocking is an allowlisted runtime symbol"),
        ));
        self.emit_child_get_into(sup_place, slot_index, handle_place);
        Some(handle_place)
    }

    /// Translate a supervisor child's combined-static checker index into its
    /// kind-partitioned runtime slot index.
    ///
    /// The runtime keeps actor children and nested supervisors in two separate
    /// 0-based tables. Codegen registers each kind into its own table while
    /// iterating `SupervisorLayout.children` (topological spawn order), so a
    /// child's runtime index is its position among same-kind children in that
    /// same iteration order. Count the same-kind children that precede this one
    /// in the layout to recover that index.
    ///
    /// Falls back to a 0 index if the supervisor or child is not found in the
    /// layout map (an upstream diagnostic already covers the unknown-supervisor
    /// case); the accessor's runtime null-guard then fail-closes the lookup.
    fn partitioned_static_slot_index(
        &self,
        supervisor: &str,
        child_name: &str,
        want_nested: bool,
    ) -> u32 {
        let Some(layout) = self.supervisor_layout_map.get(supervisor) else {
            return 0;
        };
        let mut index = 0u32;
        for child in &layout.children {
            // Pools live in a disjoint `pool_slots[]` space and occupy NEITHER
            // the static actor-child table nor the nested-supervisor table — skip
            // them on BOTH axes. This is the shared truth `occupies_static_child_slot`
            // encodes for the actor axis; the codegen bootstrap loop must skip
            // pools identically or a post-pool static accessor mis-routes.
            if child.is_pool {
                continue;
            }
            let child_is_nested = child.nested_bootstrap_symbol.is_some();
            if child_is_nested != want_nested {
                continue;
            }
            if child.name == child_name {
                // Invariant: a non-pool actor child (want_nested == false) is
                // exactly what `occupies_static_child_slot` admits; keep the two
                // iterations provably in lock-step.
                debug_assert!(
                    want_nested || child.occupies_static_child_slot(),
                    "partitioned_static_slot_index: actor-axis child `{child_name}` of \
                     supervisor `{supervisor}` must occupy a static child slot — the \
                     codegen bootstrap loop and this accessor lookup have diverged"
                );
                return index;
            }
            index += 1;
        }
        // Child not found among same-kind siblings — return the running count as
        // a best-effort index; an upstream diagnostic covers the genuine
        // unknown-child case.
        index
    }

    /// Lower a nested-supervisor child accessor (`app.api` where `api` is itself
    /// a supervisor) to a `hew_supervisor_nested_get(sup, slot)` call.
    ///
    /// Distinct from `lower_supervisor_child_get`: the parent resolves a nested
    /// child through its `child_supervisors` table (not its actor `children`
    /// table), so the runtime symbol is `hew_supervisor_nested_get`. The runtime
    /// returns the child supervisor pointer in field 1 of the same
    /// `__HewChildLookupResult` struct; the result type is
    /// `LocalPid<NestedSupervisor>`, so a subsequent dotted segment
    /// (`app.api.auth`) routes back through this intercept against the nested
    /// supervisor pointer.
    ///
    /// The handle is a plain `*mut HewSupervisor` reinterpreted as a PID — it is
    /// NOT registered as a fungible child ref. A fungible ref drives per-send
    /// re-resolution of an *actor* handle; a supervisor handle is only ever used
    /// as the receiver of a further child-get, which itself re-resolves the leaf
    /// actor at the moment of the send. Resolving the nested supervisor once per
    /// accessor expression is sufficient: the child supervisor's identity is
    /// stable across its own children's restarts, and a nested-supervisor
    /// escalation restart is re-resolved on the next `app.api` access.
    fn lower_supervisor_nested_get(
        &mut self,
        object: &HirExpr,
        slot_index: u32,
        result_ty: &ResolvedTy,
    ) -> Option<Place> {
        let sup_place = self.lower_value(object)?;

        // Allocate the handle typed as the checker-authority
        // `LocalPid<NestedSupervisor>` result type for this site.
        let handle_local = self.alloc_local(result_ty.clone());
        let Place::Local(handle_id) = handle_local else {
            unreachable!("alloc_local always returns Place::Local");
        };
        let handle_place = Place::ActorHandle(handle_id);

        // Emit a constant for the static nested slot index.
        let idx_place = self.alloc_local(ResolvedTy::I64);
        self.push_instr(Instr::ConstI64 {
            dest: idx_place,
            value: i64::from(slot_index),
        });

        // Allocate a local typed as the opaque `__HewChildLookupResult` record.
        // Codegen recognises this type name and emits a struct-return LLVM call.
        let result_place = self.alloc_local(ResolvedTy::Named {
            name: CHILD_LOOKUP_RESULT_TY_NAME.to_string(),
            args: vec![],
            builtin: None,
            is_opaque: false,
        });

        // Emit the nested-get runtime call. The dest carries the 16-byte struct.
        self.push_instr(Instr::CallRuntimeAbi(
            crate::model::RuntimeCall::new(
                "hew_supervisor_nested_get",
                vec![sup_place, idx_place],
                Some(result_place),
            )
            .expect("hew_supervisor_nested_get is an allowlisted runtime symbol"),
        ));

        // Extract the child supervisor pointer (field 1, i64 at MIR level).
        let raw_handle = self.alloc_local(ResolvedTy::I64);
        self.push_instr(Instr::RecordFieldLoad {
            record: result_place,
            field_offset: FieldOffset(1),
            dest: raw_handle,
        });

        // Move the i64 wire value into the typed supervisor handle slot.
        self.push_instr(Instr::Move {
            dest: handle_place,
            src: raw_handle,
        });

        Some(handle_place)
    }

    /// Lower a static-pool accessor through a first-class
    /// `SupervisorPool<S, T>` receiver.
    ///
    /// - `Index` → `hew_supervisor_pool_child_get(sup, key, i)`; tag != 0 (not
    ///   Live) traps `SupervisorChildUnavailable` (`Vec[i]` OOB parity).
    /// - `Len`   → `hew_supervisor_pool_len(sup, key)` → `i64`.
    /// - `Get`   → same `child_get`; tag 0 → `Some(handle)`, tag != 0 → `None`
    ///   (a drop-safe `Option<LocalPid<T>>`, never a sentinel dressed as live).
    fn lower_pool_accessor(
        &mut self,
        expr: &HirExpr,
        accessor: &hew_types::PoolAccessor,
    ) -> Option<Place> {
        use hew_types::PoolAccessorKind;

        let (pool_expr, index_expr): (&HirExpr, Option<&HirExpr>) = match &expr.kind {
            HirExprKind::Index { container, index } => (container.as_ref(), Some(index.as_ref())),
            HirExprKind::Call { args, .. } => {
                let Some(receiver) = args.first() else {
                    self.pool_accessor_shape_error(expr.site, "pool method call has no receiver");
                    return None;
                };
                (receiver, args.get(1))
            }
            _ => {
                self.pool_accessor_shape_error(expr.site, "accessor is neither index nor call");
                return None;
            }
        };

        let pool_ty = self.subst_ty(&pool_expr.ty);
        let supervisor_ty = match &pool_ty {
            ResolvedTy::Named {
                builtin: Some(hew_types::BuiltinType::SupervisorPool),
                args,
                ..
            } if args.len() == 2 => args[0].clone(),
            _ => {
                self.pool_accessor_shape_error(expr.site, "receiver is not `SupervisorPool<S, T>`");
                return None;
            }
        };
        let pool_place = self.lower_value(pool_expr)?;
        let sup_place = self.alloc_local(ResolvedTy::Named {
            name: "LocalPid".to_string(),
            args: vec![supervisor_ty],
            builtin: Some(hew_types::BuiltinType::LocalPid),
            is_opaque: false,
        });
        self.push_instr(Instr::RecordFieldLoad {
            record: pool_place,
            field_offset: FieldOffset(0),
            dest: sup_place,
        });
        let key_place = self.alloc_local(ResolvedTy::I64);
        self.push_instr(Instr::RecordFieldLoad {
            record: pool_place,
            field_offset: FieldOffset(1),
            dest: key_place,
        });

        match accessor.kind {
            PoolAccessorKind::Len => {
                let dest = self.alloc_local(ResolvedTy::I64);
                self.push_instr(Instr::CallRuntimeAbi(
                    crate::model::RuntimeCall::new(
                        "hew_supervisor_pool_len",
                        vec![sup_place, key_place],
                        Some(dest),
                    )
                    .expect("hew_supervisor_pool_len is an allowlisted runtime symbol"),
                ));
                Some(dest)
            }
            PoolAccessorKind::Index => {
                let index_expr = index_expr?;
                let idx_place = self.lower_value(index_expr)?;
                Some(self.lower_pool_index(
                    sup_place,
                    key_place,
                    idx_place,
                    &self.subst_ty(&expr.ty),
                ))
            }
            PoolAccessorKind::Get => {
                let Some(index_expr) = index_expr else {
                    self.pool_accessor_shape_error(expr.site, "pool get call has no index");
                    return None;
                };
                let idx_place = self.lower_value(index_expr)?;
                let lookup = self.emit_pool_child_get(sup_place, key_place, idx_place);
                let result = self.alloc_local(self.subst_ty(&expr.ty));
                let next = self.alloc_block();
                self.finish_current_block(Terminator::Call {
                    callee: "hew_supervisor_pool_get_option".to_string(),
                    builtin: None,
                    args: vec![lookup],
                    dest: Some(result),
                    next,
                });
                self.start_block(next);
                Some(result)
            }
        }
    }

    /// Emit `hew_supervisor_pool_child_get(sup, pool_key, index)` and return the
    /// `__HewChildLookupResult` result place (tag in field 0, handle in field 1).
    fn emit_pool_child_get(
        &mut self,
        sup_place: Place,
        key_place: Place,
        idx_place: Place,
    ) -> Place {
        let result_place = self.alloc_local(ResolvedTy::Named {
            name: CHILD_LOOKUP_RESULT_TY_NAME.to_string(),
            args: vec![],
            builtin: None,
            is_opaque: false,
        });
        self.push_instr(Instr::CallRuntimeAbi(
            crate::model::RuntimeCall::new(
                "hew_supervisor_pool_child_get",
                vec![sup_place, key_place, idx_place],
                Some(result_place),
            )
            .expect("hew_supervisor_pool_child_get is an allowlisted runtime symbol"),
        ));
        result_place
    }

    /// Lower `sup.pool[i]` (the trapping member accessor): call
    /// `pool_child_get`, then trap `SupervisorChildUnavailable` on a not-Live
    /// tag (OOB / mid-restart) and bind the Live handle into a `LocalPid<T>`
    /// (`Vec[i]` OOB parity — the index access is the sole liveness authority).
    fn lower_pool_index(
        &mut self,
        sup_place: Place,
        key_place: Place,
        idx_place: Place,
        result_ty: &ResolvedTy,
    ) -> Place {
        let result_place = self.emit_pool_child_get(sup_place, key_place, idx_place);
        let tag = self.alloc_local(ResolvedTy::I64);
        self.push_instr(Instr::RecordFieldLoad {
            record: result_place,
            field_offset: FieldOffset(0),
            dest: tag,
        });
        let zero = self.alloc_local(ResolvedTy::I64);
        self.push_instr(Instr::ConstI64 {
            dest: zero,
            value: 0,
        });
        let is_live = self.alloc_local(ResolvedTy::Bool);
        self.push_instr(Instr::IntCmp {
            dest: is_live,
            pred: CmpPred::Eq,
            lhs: tag,
            rhs: zero,
        });
        let live_bb = self.alloc_block();
        let trap_bb = self.alloc_block();
        self.finish_current_block(Terminator::Branch {
            cond: is_live,
            then_target: live_bb,
            else_target: trap_bb,
        });
        // OOB / not-live → trap (fail-closed, mirrors the static accessor).
        self.start_block(trap_bb);
        self.finish_current_block(Terminator::Trap {
            kind: crate::model::TrapKind::SupervisorChildUnavailable,
        });
        // Live → extract the handle into a typed LocalPid local.
        self.start_block(live_bb);
        let handle_local = self.alloc_local(result_ty.clone());
        let Place::Local(handle_id) = handle_local else {
            unreachable!("alloc_local always returns Place::Local");
        };
        let handle_place = Place::ActorHandle(handle_id);
        let raw_handle = self.alloc_local(ResolvedTy::I64);
        self.push_instr(Instr::RecordFieldLoad {
            record: result_place,
            field_offset: FieldOffset(1),
            dest: raw_handle,
        });
        self.push_instr(Instr::Move {
            dest: handle_place,
            src: raw_handle,
        });
        handle_place
    }

    /// Record a fail-closed shape error for a malformed pool accessor (the
    /// checker should have rejected these; this is the MIR backstop).
    fn pool_accessor_shape_error(&mut self, site: hew_hir::SiteId, why: &str) {
        self.diagnostics.push(MirDiagnostic {
            kind: MirDiagnosticKind::NotYetImplemented {
                construct: format!("malformed static-pool accessor: {why}"),
                site,
            },
            note: "a static-pool accessor must be `sup.pool[i]`, `sup.pool.get(i)`, \
                   or `sup.pool.len()`"
                .to_string(),
        });
    }

    /// Emit a `hew_supervisor_child_get(sup, slot)` call and store the resolved
    /// `*mut HewActor` (field 1) into `handle_place`, with NO liveness branch.
    ///
    /// On a not-live slot the runtime returns a null handle (field 1 == null);
    /// the caller (the accessor seed, or `emit_fungible_reresolve`) is responsible
    /// for whatever liveness handling is required. This is the shared "resolve the
    /// current child pointer into the handle slot" primitive.
    fn emit_child_get_into(&mut self, sup_place: Place, slot_index: u32, handle_place: Place) {
        // Emit a constant for the static slot index.
        let idx_place = self.alloc_local(ResolvedTy::I64);
        self.push_instr(Instr::ConstI64 {
            dest: idx_place,
            value: i64::from(slot_index),
        });

        // Allocate a local typed as the opaque `__HewChildLookupResult` record.
        // S3 codegen recognises this type name and emits a struct-return LLVM call.
        let result_place = self.alloc_local(ResolvedTy::Named {
            name: CHILD_LOOKUP_RESULT_TY_NAME.to_string(),
            args: vec![],
            builtin: None,
            is_opaque: false,
        });

        // Emit the runtime call. The dest carries the 16-byte struct return value.
        self.push_instr(Instr::CallRuntimeAbi(
            crate::model::RuntimeCall::new(
                "hew_supervisor_child_get",
                vec![sup_place, idx_place],
                Some(result_place),
            )
            .expect("hew_supervisor_child_get is an allowlisted runtime symbol"),
        ));

        // Extract the handle pointer (field 1, i64 at MIR level).
        // S3 emits a `ptrtoint`/`inttoptr` as needed for the wire representation.
        let raw_handle = self.alloc_local(ResolvedTy::I64);
        self.push_instr(Instr::RecordFieldLoad {
            record: result_place,
            field_offset: FieldOffset(1),
            dest: raw_handle,
        });

        // Move the i64 wire value into the typed ActorHandle slot.
        // S3 emits the appropriate cast; at MIR level they are the same storage.
        self.push_instr(Instr::Move {
            dest: handle_place,
            src: raw_handle,
        });
    }

    /// Emit `Instr::CallRuntimeAbi` for a `.send` on an actor/duplex handle.
    ///
    /// HIR shape (from E1 bridge): `Call { callee: BindingRef("hew_duplex_send"),
    /// args: [receiver_expr, msg_expr] }` — receiver prepended by E1. The
    /// checker records the `hew_duplex_send` rewrite for `.send` on every
    /// `Duplex<S, R>` receiver, including lambda-actor handles (which type as
    /// `Duplex<Msg, Reply>`).
    ///
    /// MIR emission:
    ///   1. Lower `receiver_expr` → `recv_place`. A lambda-actor binding
    ///      lowers to `Place::LambdaActorHandle(N)`; a raw `Duplex::pair()`
    ///      handle lowers to `Place::DuplexHandle(N)`.
    ///   2. Lower `msg_expr` → `msg_place` (the integer value's `Local(K)`).
    ///   3. Emit `ConstI64 { dest: len_place, value: 8 }` — the byte-length.
    ///   4. Select the runtime symbol by the receiver `Place` variant
    ///      (`codegen-abi-authority`): `LambdaActorHandle` → the
    ///      lambda-actor ABI `hew_lambda_actor_send`; anything else → the
    ///      raw-duplex ABI `hew_duplex_send`. Routing each `Place` to exactly
    ///      one correct symbol is load-bearing: passing a `LambdaActorHandle`
    ///      to `hew_duplex_send` type-puns the handle and silently mis-delivers
    ///      (`no-fail-open-fallback-after-authority`).
    ///   5. Emit `CallRuntimeAbi { symbol, args: [recv, msg, len], dest }`.
    ///
    /// The receiver is NOT consumed (non-move send semantics); `owned_locals`
    /// for the receiver handle must persist across multiple sends and the
    /// scope-exit drop (LESSONS `raii-null-after-move`).
    fn lower_duplex_send(
        &mut self,
        hir_args: &[hew_hir::HirExpr],
        site: hew_hir::SiteId,
        context: RuntimeCallContext,
        result_ty: Option<&ResolvedTy>,
    ) -> Option<Place> {
        if hir_args.len() < 2 {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "hew_duplex_send with fewer than 2 args".to_string(),
                    site,
                },
                note: "hew_duplex_send requires receiver + message arguments".to_string(),
            });
            return None;
        }
        // args[0] = receiver (DuplexHandle — non-consuming borrow; no Move emitted).
        let recv_place = self.lower_value(&hir_args[0]);
        // args[1] = message value.
        let msg_place = self.lower_value(&hir_args[1]);

        let (Some(recv_place), Some(msg_place)) = (recv_place, msg_place) else {
            // Argument lowering failed; diagnostic already recorded.
            return None;
        };

        // Route by receiver Place variant. A lambda-actor handle must use the
        // lambda-actor ABI; a raw `Duplex` handle uses the duplex ABI. The two
        // runtime symbols are distinct authorities for one `.send` surface, and
        // a `LambdaActorHandle` routed through `hew_duplex_send` type-puns the
        // handle and silently drops the message (Evidence #2). The `Place`
        // variant is the canonical "which handle is this" signal — selecting on
        // it (not on the receiver's type, which is `Duplex<Msg, Reply>` for
        // both) keeps ABI selection on an explicit authority
        // (`codegen-abi-authority`, `no-fail-open-fallback-after-authority`).
        let symbol = match recv_place {
            Place::LambdaActorHandle(_) => "hew_lambda_actor_send",
            _ => "hew_duplex_send",
        };

        // Materialize the runtime's i32 send status into a user-visible
        // `Result<(), SendError>`, or fail closed. The decision is on the result
        // SHAPE, not the call context, so an ask-shaped `.send` fails closed in
        // BOTH statement and value position — it must never lower as a
        // fire-and-forget tell that silently drops the reply the user's type
        // says they receive.
        //
        // Shape is the checker-recorded result type (`checker-authority`: the
        // producer consumes the recorded type, it does not re-infer it):
        //   - tell-shaped `Result<(), SendError>` -> supported. Value context
        //     binds a dest local that codegen fills from the rc (the D1
        //     discriminant mapping lives in codegen, the single authority);
        //     statement context discards the status (`dest: None`,
        //     fire-and-forget delivery).
        //   - ask-shaped `Result<R, AskError>` (a non-unit `Duplex` reply) or
        //     any other type -> a separate lowering this change does not own. Fail
        //     closed with a stable NYI diagnostic BEFORE emitting any send
        //     instruction (`no-fail-open-fallback-after-authority`), rather than
        //     bind nothing (the misleading `UnresolvedPlace`, Evidence #1),
        //     mis-size the slot, or — in statement context — drop the reply
        //     silently. The check runs before the length const below so the
        //     rejected path emits neither the const nor the call.
        let resolved_result = result_ty.map(|ty| self.subst_ty(ty));
        if !resolved_result
            .as_ref()
            .is_some_and(is_unit_send_error_result)
        {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "`.send` whose result is not Result<(), SendError>".to_string(),
                    site,
                },
                note: format!(
                    "`.send` materializes only the tell-shaped `Result<(), \
                     SendError>`; got {resolved_result:?}. Ask-shaped `.send` (a \
                     non-unit `Duplex` reply yielding `Result<R, AskError>`) is a \
                     separate lowering and fails closed in both statement and \
                     value context rather than lower as a fire-and-forget tell \
                     that drops the reply."
                ),
            });
            return None;
        }
        // Tell-shaped: bind the Result in value context; discard the status in
        // statement context (fire-and-forget delivery).
        let dest = match context {
            RuntimeCallContext::ValueNeeded => Some(self.alloc_local(
                resolved_result.expect("tell-shaped result is Some after the guard above"),
            )),
            RuntimeCallContext::Discarded => None,
        };

        // Emit the byte-length constant.  The runtime ABI takes `*const u8 + usize`;
        // E4 codegen stores `msg_place`'s value to a stack alloca and passes its
        // address.  The length constant here encodes the fixed 8-byte integer size.
        //
        // SHIM(E4): the ConstI64(8) encodes the integer payload byte-length.
        // WHY: MIR has no "sizeof" expression; the integer spine always uses 8-byte
        //   i64 values, so the length is a compile-time constant for this skeleton.
        // WHEN obsolete: when the type system can express the payload size directly,
        //   or when hew_duplex_send uses a typed message rather than a byte slice.
        // WHAT: replace with a proper sizeof/alignof expression or a typed ABI.
        let len_place = self.alloc_local(ResolvedTy::I64);
        self.push_instr(Instr::ConstI64 {
            dest: len_place,
            value: 8,
        });

        // Emit the runtime call.  `recv_place` is used as a borrow (not consumed);
        // the receiver's `owned_locals` entry survives for subsequent sends and the
        // scope-exit drop (LESSONS `raii-null-after-move`, `cleanup-all-exits`).
        // `dest` is `Some` only in value context; codegen materializes the Result
        // there and leaves the rc unobserved when it is `None`.
        self.push_instr(Instr::CallRuntimeAbi(
            crate::model::RuntimeCall::new(symbol, vec![recv_place, msg_place, len_place], dest)
                .expect("send symbol is an allowlisted runtime symbol"),
        ));

        dest
    }

    /// Lower an explicit `.close()` call rewritten to `hew_duplex_close`.
    ///
    /// The checker records `"hew_duplex_close"` for every `.close()` call on a
    /// `Duplex<S,R>`-typed receiver, including `LambdaPid<M,R>` handles (which
    /// surface as `Duplex<M,R>`).  The `Place` variant on the receiver is the
    /// authority for which runtime symbol to invoke:
    ///
    ///   - `Place::LambdaActorHandle(N)` → `hew_lambda_actor_release`.
    ///     The release ABI takes only the handle pointer; codegen rejects
    ///     `dest: Some(...)` for this symbol, so the call is always `dest: None`.
    ///     The checker records `Unit` as the return type (the release never
    ///     fails), so in value context a unit literal is produced.
    ///
    ///   - Any other `Place` → raw `Duplex` explicit close; not yet lowered.
    ///     Fails closed so the pipeline rejects the program before codegen runs.
    ///
    /// The checker marks the receiver as consumed (`method_call_consumes_receiver`
    /// + `mark_expr_moved_if_non_copy`), so drop elaboration at scope exit will
    ///   not re-drop the released handle.
    fn lower_duplex_close(
        &mut self,
        hir_args: &[hew_hir::HirExpr],
        site: hew_hir::SiteId,
        context: RuntimeCallContext,
        _result_ty: Option<&ResolvedTy>,
    ) -> Option<Place> {
        let Some(receiver_expr) = hir_args.first() else {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "hew_duplex_close with no receiver arg".to_string(),
                    site,
                },
                note: "hew_duplex_close requires at least one argument (the handle)".to_string(),
            });
            return None;
        };
        let recv_place = self.lower_value(receiver_expr)?;

        if let Place::LambdaActorHandle(_) = recv_place {
            // Explicit LambdaPid::close() → hew_lambda_actor_release.
            //
            // The release ABI is: hew_lambda_actor_release(handle: *mut HewLambdaActorHandle)
            // Codegen rejects dest: Some(_) for this symbol (the i32 rc is always
            // discarded), so we never pass a dest — the handle is released and the
            // result, if needed in value context, is a unit literal (the checker
            // records `()` for LambdaPid::close() since the release is
            // unconditionally successful).
            self.push_instr(Instr::CallRuntimeAbi(
                crate::model::RuntimeCall::new("hew_lambda_actor_release", vec![recv_place], None)
                    .expect("hew_lambda_actor_release is an allowlisted runtime symbol"),
            ));

            // The checker records `Unit` as the return type for LambdaPid::close()
            // (the release never fails). In value context, bind a unit literal so
            // any `let _ = handle.close()` binding has a well-typed Place.
            if context == RuntimeCallContext::ValueNeeded {
                let unit_place = self.alloc_local(ResolvedTy::Unit);
                self.push_instr(Instr::UnitLit { dest: unit_place });
                Some(unit_place)
            } else {
                None
            }
        } else {
            // Explicit `handle.close()` on a raw `Duplex<S,R>` binding routes
            // through drop elaboration, not a value-position call: codegen's
            // close arm is the drop ritual's responsibility (it pairs the
            // `hew_duplex_close` call with the alloca-zero so a later drop
            // cannot re-close). The unified handle's scope-exit drop already
            // emits exactly that ritual, so an explicit raw close is redundant
            // with the RAII close and fails closed here rather than emit a
            // value-position call codegen refuses. The half-handle close
            // (`hew_duplex_close_half`) has a dedicated value-position arm
            // because its direction discriminant is not a drop-ritual concern.
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: format!("explicit Duplex::close() on a raw handle ({recv_place:?})"),
                    site,
                },
                note: "explicit `Duplex::close()` on a raw `Duplex<S,R>` binding is closed \
                       implicitly at scope exit by drop elaboration; remove the explicit \
                       `.close()` call. Split the handle and close a half \
                       (`SendHalf`/`RecvHalf`) for an explicit per-direction close."
                    .to_string(),
            });
            None
        }
    }

    /// Lower `.send_half()` / `.recv_half()` on a unified `Duplex<S, R>`.
    ///
    /// HIR shape (E1 bridge): `Call { callee: BindingRef(symbol), args:
    /// [receiver_expr] }`. The checker marks the call consuming, so the
    /// `receiver_expr` carries `IntentKind::Consume`; `lower_value` emits the
    /// `Use { Consume }` that transitions the unified `DuplexHandle` binding to
    /// `Consumed` and drops it out of `owned_locals`, so its scope-exit close is
    /// suppressed and the extracted half is the only live handle on that
    /// direction (`raii-null-after-move`, `cleanup-all-exits`).
    ///
    /// MIR emission:
    ///   1. Lower `receiver_expr` → the unified `DuplexHandle` Place (consuming).
    ///   2. Allocate a fresh backing local typed `SendHalf<S>` / `RecvHalf<R>`
    ///      (the checker-recorded result type) and wrap it as the matching
    ///      half `Place`. The `Place` variant is the authority codegen reads to
    ///      pick the per-direction close ABI at drop, so it must match the
    ///      extracted direction (`codegen-abi-authority`).
    ///   3. Emit `CallRuntimeAbi { symbol, args: [duplex], dest: Some(half) }` —
    ///      the half ABIs RETURN the new handle pointer (unlike `hew_duplex_pair`,
    ///      which writes out-params), so the half Place is the call dest.
    ///   4. Return the half Place; the enclosing `Let` registers it in
    ///      `binding_locals` + `owned_locals` for its own scope-exit close.
    fn lower_duplex_half_extract(
        &mut self,
        symbol: &str,
        hir_args: &[hew_hir::HirExpr],
        site: hew_hir::SiteId,
        result_ty: Option<&ResolvedTy>,
    ) -> Option<Place> {
        let Some(receiver_expr) = hir_args.first() else {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: format!("{symbol} with no receiver arg"),
                    site,
                },
                note: format!("`{symbol}` requires the Duplex receiver argument"),
            });
            return None;
        };
        // Consuming receiver: the recorded `IntentKind::Consume` drives the
        // move-mark inside `lower_value`.
        let recv_place = self.lower_value(receiver_expr)?;

        // The result type is the half handle type (`SendHalf<S>` / `RecvHalf<R>`);
        // consume it as the backing local's type so the handle's element layout is
        // recorded for codegen (`checker-authority`).
        let half_ty = result_ty.map_or(
            ResolvedTy::Named {
                name: if symbol == "hew_duplex_send_half" {
                    "SendHalf".to_string()
                } else {
                    "RecvHalf".to_string()
                },
                args: vec![],
                builtin: None,
                is_opaque: true,
            },
            |ty| self.subst_ty(ty),
        );
        let backing = self.alloc_local(half_ty);
        let Place::Local(n) = backing else {
            unreachable!("alloc_local returns Place::Local");
        };
        let half_place = if symbol == "hew_duplex_send_half" {
            Place::SendHalf(n)
        } else {
            Place::RecvHalf(n)
        };

        self.push_instr(Instr::CallRuntimeAbi(
            crate::model::RuntimeCall::new(symbol, vec![recv_place], Some(half_place))
                .expect("half-extract symbol is an allowlisted runtime symbol"),
        ));

        Some(half_place)
    }

    /// Lower `.send(msg)` / `.try_send(msg)` on a `SendHalf<S>`.
    ///
    /// Mirrors `lower_duplex_send`'s `(handle, msg_ptr, len)` ABI: the receiver
    /// is a borrowing `SendHalf` Place (NON-consuming — a half is sent on many
    /// times before its own close), the message spills into the fixed 8-byte
    /// integer slot, and the runtime's i32 status materialises into the
    /// checker-recorded `Result<(), SendError>` in value context (discarded in
    /// statement context). Any other recorded shape fails closed
    /// (`checker-authority`, `boundary-fail-closed`).
    fn lower_half_send(
        &mut self,
        symbol: &str,
        hir_args: &[hew_hir::HirExpr],
        site: hew_hir::SiteId,
        context: RuntimeCallContext,
        result_ty: Option<&ResolvedTy>,
    ) -> Option<Place> {
        if hir_args.len() < 2 {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: format!("{symbol} with fewer than 2 args"),
                    site,
                },
                note: format!("`{symbol}` requires receiver + message arguments"),
            });
            return None;
        }
        // args[0] = SendHalf receiver (borrow; no Move emitted).
        let recv_place = self.lower_value(&hir_args[0]);
        // args[1] = message value.
        let msg_place = self.lower_value(&hir_args[1]);
        let (Some(recv_place), Some(msg_place)) = (recv_place, msg_place) else {
            return None;
        };

        let resolved_result = result_ty.map(|ty| self.subst_ty(ty));
        if !resolved_result
            .as_ref()
            .is_some_and(is_unit_send_error_result)
        {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: format!("`{symbol}` whose result is not Result<(), SendError>"),
                    site,
                },
                note: format!(
                    "`{symbol}` materializes only `Result<(), SendError>`; got \
                     {resolved_result:?}"
                ),
            });
            return None;
        }
        let dest = match context {
            RuntimeCallContext::ValueNeeded => Some(self.alloc_local(
                resolved_result.expect("tell-shaped result is Some after the guard above"),
            )),
            RuntimeCallContext::Discarded => None,
        };

        // The fixed 8-byte integer payload length (mirrors `lower_duplex_send`).
        // SHIM: the integer spine always uses 8-byte i64 values, so the length
        // is a compile-time constant for the scalar message shape.
        // WHEN obsolete: when the typed-message ABI lands a per-type payload size.
        // WHAT: replace with a sizeof/alignof expression or a typed ABI.
        let len_place = self.alloc_local(ResolvedTy::I64);
        self.push_instr(Instr::ConstI64 {
            dest: len_place,
            value: 8,
        });

        self.push_instr(Instr::CallRuntimeAbi(
            crate::model::RuntimeCall::new(symbol, vec![recv_place, msg_place, len_place], dest)
                .expect("half-send symbol is an allowlisted runtime symbol"),
        ));

        dest
    }

    /// Lower the recv family: `.recv()` / `.try_recv()` on a `RecvHalf<R>` and
    /// `.recv()` / `.try_recv()` on a unified `Duplex<S, R>` (channel mode).
    ///
    /// Every symbol shares the `(handle, out_ptr, out_len) -> i32` runtime ABI:
    /// the runtime writes the received payload into caller out-params and returns
    /// a `RecvError` status. The receiver is a borrowing handle Place
    /// (NON-consuming — recv is called repeatedly until close). The checker
    /// records `Result<R, RecvError>`; this producer allocates the dest typed
    /// from that recorded shape and lets codegen materialise the tagged union
    /// from the out-params + status (the recv codegen arm owns the payload copy
    /// and `hew_duplex_payload_free`). Fails closed on any non-recv-Result shape
    /// (`checker-authority`, `boundary-fail-closed`).
    fn lower_duplex_recv(
        &mut self,
        symbol: &str,
        hir_args: &[hew_hir::HirExpr],
        site: hew_hir::SiteId,
        context: RuntimeCallContext,
        result_ty: Option<&ResolvedTy>,
    ) -> Option<Place> {
        let Some(receiver_expr) = hir_args.first() else {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: format!("{symbol} with no receiver arg"),
                    site,
                },
                note: format!("`{symbol}` requires the receiver handle argument"),
            });
            return None;
        };
        // Borrowing receiver (recv is non-consuming).
        let recv_place = self.lower_value(receiver_expr)?;

        let resolved_result = result_ty.map(|ty| self.subst_ty(ty));
        let Some(resolved_result) = resolved_result else {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: format!("{symbol} with no recorded result type"),
                    site,
                },
                note: format!("`{symbol}` requires a checker-recorded `Result<R, RecvError>`"),
            });
            return None;
        };
        if recv_result_payload_ty(&resolved_result).is_none() {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: format!("`{symbol}` whose result is not Result<R, RecvError>"),
                    site,
                },
                note: format!(
                    "`{symbol}` materializes only `Result<R, RecvError>`; got \
                     {resolved_result:?}"
                ),
            });
            return None;
        }

        // The recv runtime ABI always writes its payload out-params and returns a
        // status; codegen materialises the Result from those, so a dest is
        // required even in statement context (the status carries the closed/empty
        // signal the user discards but the codegen arm still consumes). Allocate
        // the dest from the checker-recorded shape unconditionally.
        let dest = self.alloc_local(resolved_result);
        self.push_instr(Instr::CallRuntimeAbi(
            crate::model::RuntimeCall::new(symbol, vec![recv_place], Some(dest))
                .expect("recv symbol is an allowlisted runtime symbol"),
        ));

        match context {
            RuntimeCallContext::ValueNeeded => Some(dest),
            RuntimeCallContext::Discarded => None,
        }
    }

    /// Lower `.close()` on a `SendHalf<S>` / `RecvHalf<R>`.
    ///
    /// The checker rewrites half-close to `hew_duplex_close_half` and marks the
    /// receiver consuming, so `lower_value` transitions the half binding to
    /// `Consumed` and suppresses its scope-exit close — this call is the single
    /// close on the consuming path (the runtime's `AtomicBool` guard keeps a
    /// re-entrant close safe). The runtime ABI is `(half_ptr, direction) -> i32`
    /// with `direction` SendHalf=0 / RecvHalf=1; the direction is carried by the
    /// receiver's `Place` variant (`codegen-abi-authority`), and codegen
    /// materialises the discriminant at the call from that variant exactly as
    /// the drop-elaboration close already does. The i32 status materialises into
    /// the checker-recorded `Result<(), CloseError>`; any other shape fails
    /// closed.
    fn lower_half_close(
        &mut self,
        hir_args: &[hew_hir::HirExpr],
        site: hew_hir::SiteId,
        context: RuntimeCallContext,
        result_ty: Option<&ResolvedTy>,
    ) -> Option<Place> {
        let Some(receiver_expr) = hir_args.first() else {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "hew_duplex_close_half with no receiver arg".to_string(),
                    site,
                },
                note: "`hew_duplex_close_half` requires the half-handle argument".to_string(),
            });
            return None;
        };
        // Consuming receiver: recorded `IntentKind::Consume` drives the move-mark.
        let recv_place = self.lower_value(receiver_expr)?;

        // The receiver must be a half Place — that variant is the direction
        // authority codegen reads. A non-half Place is a producer-contract
        // violation; fail closed rather than emit a mis-directed close.
        if !matches!(recv_place, Place::SendHalf(_) | Place::RecvHalf(_)) {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: format!("hew_duplex_close_half on non-half handle ({recv_place:?})"),
                    site,
                },
                note: "`hew_duplex_close_half` requires a SendHalf/RecvHalf receiver; the \
                       direction discriminant is selected from the Place variant"
                    .to_string(),
            });
            return None;
        }

        let resolved_result = result_ty.map(|ty| self.subst_ty(ty));
        if !resolved_result
            .as_ref()
            .is_some_and(is_unit_close_error_result)
        {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "`half.close` whose result is not Result<(), CloseError>"
                        .to_string(),
                    site,
                },
                note: format!(
                    "half-handle `.close()` materializes only `Result<(), CloseError>`; \
                     got {resolved_result:?}"
                ),
            });
            return None;
        }
        let dest =
            match context {
                RuntimeCallContext::ValueNeeded => Some(self.alloc_local(
                    resolved_result.expect("close result is Some after the guard above"),
                )),
                RuntimeCallContext::Discarded => None,
            };

        // The direction discriminant is a codegen-side projection of the
        // receiver `Place` variant (SendHalf=0 / RecvHalf=1), mirroring the
        // drop-elaboration half-close. The producer passes only the handle; the
        // codegen arm appends the discriminant from the Place.
        self.push_instr(Instr::CallRuntimeAbi(
            crate::model::RuntimeCall::new("hew_duplex_close_half", vec![recv_place], dest)
                .expect("hew_duplex_close_half is an allowlisted runtime symbol"),
        ));

        dest
    }

    fn lower_actor_payload(
        &mut self,
        args: &[hew_hir::HirExpr],
        site: hew_hir::SiteId,
    ) -> Option<Place> {
        match args {
            [] => Some(self.alloc_local(ResolvedTy::Unit)),
            [arg] => self.lower_value(arg),
            _ => self.lower_packed_args_payload(args, site),
        }
    }

    /// Pack a multi-argument actor payload into a synthetic anonymous-record
    /// Place — the single shared payload mechanism for every multi-arg actor
    /// send shape (tell, ask, select arm, join branch, and the lambda-actor
    /// multi-param message).
    ///
    /// Wire contract: the packed record is a stack local in the caller's
    /// frame, filled field-by-field via `RecordInit` (struct GEP + store,
    /// each field written at exactly `sizeof(field_ty)`). Codegen derives
    /// `(ptr, sizeof(packed_record))` from the Place through the existing
    /// `actor_payload_ptr_size` path and the mailbox deep-copies that many
    /// bytes — the same bounded-copy invariant the single-arg wire carries.
    /// The receive side (`emit_actor_dispatch_trampoline`) reconstructs the
    /// identical non-packed LLVM struct from the handler's `param_tys` and
    /// unpacks each param at its natural field offset, so both ends agree on
    /// the layout by construction (same field types, same order, same
    /// `packed = false` struct rules).
    ///
    /// Ownership: the checker (`enforce_actor_method_send_args`) marks every
    /// arg moved at the boundary, so a heap-owning field's caller binding is
    /// dead after the send; the field store into the packed record IS the
    /// move. The packed temp itself is a non-binding local — no scope-exit
    /// drop is scheduled for it, so the bytes (including any heap pointers)
    /// have exactly one consumer: the mailbox node the runtime deep-copies
    /// them into.
    ///
    /// The minted type name is module-unique (`owner` symbol + per-function
    /// monotonic id), so two handlers with identical field types can never
    /// collide in `record_layouts` / the codegen struct map.
    fn lower_packed_args_payload(
        &mut self,
        args: &[hew_hir::HirExpr],
        _site: hew_hir::SiteId,
    ) -> Option<Place> {
        let mut field_places: Vec<Place> = Vec::with_capacity(args.len());
        let mut field_tys: Vec<ResolvedTy> = Vec::with_capacity(args.len());
        for arg in args {
            let place = self.lower_value(arg)?;
            field_places.push(place);
            field_tys.push(self.subst_ty(&arg.ty));
        }
        Some(self.pack_actor_payload_from_places(field_places, field_tys))
    }

    /// The pack half of [`Self::lower_packed_args_payload`]: mint the
    /// module-unique packed-record layout and emit the `RecordInit` over
    /// ALREADY-LOWERED argument places. Split out so the fungible-child tell
    /// path can evaluate arguments before its liveness branch (argument
    /// effects are user-visible and must not depend on child liveness) while
    /// building the packed temp — whose bytes the `Send` alone consumes — on
    /// the delivery edge only. Pure MIR construction: no user effect runs
    /// here, so the emission point is free to move across control flow.
    fn pack_actor_payload_from_places(
        &mut self,
        field_places: Vec<Place>,
        field_tys: Vec<ResolvedTy>,
    ) -> Place {
        let packed_id = self.next_closure_id;
        self.next_closure_id = self
            .next_closure_id
            .checked_add(1)
            .expect("packed-args id overflow — closure id counter exhausted");
        let owner = Self::sanitize_symbol_component(&self.current_function_symbol);
        let packed_name = format!("__hew_packed_args_{owner}_{packed_id}");
        let packed_ty = ResolvedTy::Named {
            name: packed_name.clone(),
            args: vec![],
            builtin: None,
            is_opaque: false,
        };
        self.closure_record_layouts
            .push(crate::model::RecordLayout {
                name: packed_name,
                field_tys,
                // Compiler-internal packed-args record: positional `-g` names.
                field_names: Vec::new(),
            });

        let fields: Vec<(FieldOffset, Place)> = field_places
            .into_iter()
            .enumerate()
            .map(|(idx, place)| {
                (
                    FieldOffset(
                        u32::try_from(idx)
                            .expect("packed-args field count exceeds u32::MAX — impossible in Hew"),
                    ),
                    place,
                )
            })
            .collect();
        let dest = self.alloc_local(packed_ty.clone());
        self.push_instr(Instr::RecordInit {
            ty: packed_ty,
            fields,
            dest,
        });
        dest
    }

    fn actor_method_info(
        &mut self,
        receiver_ty: &ResolvedTy,
        method_id: &str,
        site: hew_hir::SiteId,
    ) -> Option<ActorMethodInfo> {
        let actor_name = actor_name_from_handle_ty(receiver_ty)?;
        let method_name = method_name_from_id(method_id);
        let Some(layout) = self.actor_layouts.get(actor_name) else {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: format!("actor call on unknown actor `{actor_name}`"),
                    site,
                },
                note: "receiver type named an actor with no MIR actor layout".to_string(),
            });
            return None;
        };
        let Some(handler) = layout
            .handlers
            .iter()
            .find(|handler| handler.name == method_name)
        else {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: format!("unknown actor handler `{method_id}` on `{actor_name}`"),
                    site,
                },
                note:
                    "actor method dispatch side table named a handler absent from the actor layout"
                        .to_string(),
            });
            return None;
        };
        Some(ActorMethodInfo {
            msg_type: handler.msg_type,
            param_tys: handler.param_tys.clone(),
            return_ty: handler.return_ty.clone(),
        })
    }

    /// Returns the `(sup, slot)` fungible reference for `place` if it is a
    /// supervisor-child handle, else `None`.
    fn fungible_child_ref_of(&self, place: Place) -> Option<FungibleChildRef> {
        match place {
            Place::ActorHandle(id) => self.fungible_child_refs.get(&id).copied(),
            _ => None,
        }
    }

    /// F-04: re-resolve a fungible child reference into its handle alloca at the
    /// send site, branching on the current liveness of the slot.
    ///
    /// Emits `hew_supervisor_child_get(sup, slot)`, stores the fresh `*mut
    /// HewActor` into `handle_place`, and branches:
    /// - Live (`tag == 0`)  → continues in the returned `live_bb` (cursor parked
    ///   there) with `handle_place` holding the CURRENT live child pointer.
    /// - not-Live (`tag != 0`) → branches to `recover_bb` (the caller wires the
    ///   recoverable fail-closed path there — the tell releases its undelivered
    ///   payload values and skips the Send).
    ///
    /// Because the pointer is fetched fresh under the slot lock at the instant of
    /// the send and used immediately in the same turn, the "valid only within the
    /// current scheduler turn" borrow contract is honoured structurally rather
    /// than by user discipline — the stale-handle UAF cannot arise.
    ///
    /// Returns `(live_bb, recover_bb)`. The current block is finished with the
    /// liveness branch; NEITHER block is started — the caller fills `recover_bb`
    /// first, then `start_block(live_bb)` to continue with the send (this mirrors
    /// the trap-block-then-success-block ordering the old accessor used, keeping
    /// the `start_block` empty-buffer invariant satisfied).
    fn emit_fungible_reresolve(
        &mut self,
        child_ref: FungibleChildRef,
        handle_place: Place,
    ) -> (u32, u32) {
        let FungibleChildRef {
            sup_place,
            slot_index,
        } = child_ref;

        // Re-fetch into the same opaque struct so we can read BOTH the tag and
        // the handle. We reuse the result place for both extracts.
        let idx_place = self.alloc_local(ResolvedTy::I64);
        self.push_instr(Instr::ConstI64 {
            dest: idx_place,
            value: i64::from(slot_index),
        });
        let result_place = self.alloc_local(ResolvedTy::Named {
            name: CHILD_LOOKUP_RESULT_TY_NAME.to_string(),
            args: vec![],
            builtin: None,
            is_opaque: false,
        });
        self.push_instr(Instr::CallRuntimeAbi(
            crate::model::RuntimeCall::new(
                "hew_supervisor_child_get",
                vec![sup_place, idx_place],
                Some(result_place),
            )
            .expect("hew_supervisor_child_get is an allowlisted runtime symbol"),
        ));

        // Store the fresh handle pointer (field 1) into the handle alloca so the
        // Send terminator delivers to the CURRENT child.
        let raw_handle = self.alloc_local(ResolvedTy::I64);
        self.push_instr(Instr::RecordFieldLoad {
            record: result_place,
            field_offset: FieldOffset(1),
            dest: raw_handle,
        });
        self.push_instr(Instr::Move {
            dest: handle_place,
            src: raw_handle,
        });

        // Extract the tag (field 0) and branch on liveness.
        let tag_place = self.alloc_local(ResolvedTy::I64);
        self.push_instr(Instr::RecordFieldLoad {
            record: result_place,
            field_offset: FieldOffset(0),
            dest: tag_place,
        });
        let zero_place = self.alloc_local(ResolvedTy::I64);
        self.push_instr(Instr::ConstI64 {
            dest: zero_place,
            value: 0,
        });
        let is_live_flag = self.alloc_local(ResolvedTy::Bool);
        self.push_instr(Instr::IntCmp {
            pred: CmpPred::Eq,
            lhs: tag_place,
            rhs: zero_place,
            dest: is_live_flag,
        });

        let live_bb = self.alloc_block();
        let recover_bb = self.alloc_block();
        self.finish_current_block(Terminator::Branch {
            cond: is_live_flag,
            then_target: live_bb,
            else_target: recover_bb,
        });
        (live_bb, recover_bb)
    }

    /// Release one undelivered actor-tell payload value on the not-live
    /// recover edge of a fungible supervisor-child send (#2126).
    ///
    /// The delivered edge's `Terminator::Send` is the payload's one consumer:
    /// the mailbox takes ownership of the value's bytes (heap pointers
    /// included), and the checker marks every send argument moved at the
    /// boundary, so no scope-exit drop covers the value. When the liveness
    /// branch takes the recover edge instead, the Send never runs — this
    /// helper stands in for it, releasing exactly the ownership the delivered
    /// edge would have consumed. The two edges are exclusive per send
    /// execution, so the release runs exactly once.
    ///
    /// Shape dispatch, routed through the drop authorities:
    /// - a type that does not seed drop elaboration
    ///   (`binding_seeds_drop_elaboration` — the `BitCopy` spine) owns nothing;
    ///   no instruction is emitted.
    /// - a Wired leaf (`project_field_inline_drop_symbol`: `string`, `bytes`,
    ///   `Vec` with a wired element release, `HashMap`, `HashSet`, generator
    ///   handles) releases through one whole-value `Instr::Drop` — the same
    ///   inline release the overwrite path emits for these shapes. A static
    ///   string literal is safe here: `hew_string_drop` skips read-only
    ///   segment pointers via its `is_static_string` guard.
    /// - an Unwired `Vec` (element release protocol unwired) is refused fail
    ///   closed, per the picker contract ("a `Wired`-gated pre-flight can no
    ///   longer admit the buffer-only free"). Unreachable today: a receive
    ///   handler parameter of such a type is already rejected by the
    ///   scope-exit element scan in the handler's own body, so no tell can
    ///   target one — defence in depth, not a live diagnostic.
    /// - everything else returns with no instruction. The seed gate already
    ///   excluded `BitCopy`, so this arm is a View/handle shape with no release
    ///   obligation at this seam (e.g. an actor pid) — or an owned aggregate:
    ///   SHIM(F-04 recover-path aggregate payload): an owned-aggregate
    ///   payload (user record / tuple / enum with heap fields) has no inline
    ///   whole-value release — its drop kinds (`RecordInPlace` /
    ///   `TupleInPlace` / `EnumInPlace`) are function-scope drop-plan
    ///   entries, not inline `Instr::Drop`s — so an undelivered aggregate
    ///   payload still leaks its heap fields on the not-live edge. WHY: the
    ///   leak is bounded to restart/shutdown windows and refusing the shape
    ///   would reject every record tell through a supervisor child. WHEN
    ///   obsolete: when the type-directed drop-table consolidation gives
    ///   aggregates a whole-value release emittable at an arbitrary
    ///   instruction position. WHAT: route this arm through that whole-value
    ///   drop instead of returning empty-handed.
    fn emit_undelivered_send_payload_release(
        &mut self,
        place: Place,
        ty: &ResolvedTy,
        site: hew_hir::SiteId,
    ) -> Option<()> {
        let ty = self.subst_ty(ty);
        if !self.binding_seeds_drop_elaboration(&ty) {
            return Some(());
        }
        match self.project_field_inline_drop_symbol(&ty) {
            ReleaseSymbolVerdict::Wired(symbol) => {
                self.push_instr(Instr::Drop {
                    place,
                    ty,
                    drop_fn: Some(crate::model::DropFnSpec::Release(symbol)),
                });
                Some(())
            }
            ReleaseSymbolVerdict::Unwired(_) => {
                let elem = self
                    .unsupported_vec_element_in_ty(&ty)
                    .unwrap_or_else(|| format!("`{}`", ty.user_facing()));
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::NotYetImplemented {
                        construct: format!(
                            "actor tell payload: a `Vec` whose element is {elem} has no \
                             per-element release protocol, so a send skipped at a \
                             not-live supervisor child would leak its heap nodes"
                        ),
                        site,
                    },
                    note: "a `Vec` of `bytes` or of an indirect-enum element cannot yet \
                           be released element-by-element, and a fungible-child tell \
                           must free an undelivered payload on the not-live recover \
                           edge. This construction is rejected at compile rather than \
                           silently leaked, and becomes available once the per-element \
                           release is wired."
                        .to_string(),
                });
                None
            }
            // `WiredInPlace` is the yield/recv picker's composite verdict;
            // `project_field_inline_drop_symbol` never returns it (owned
            // aggregates stay `NoDropPath` here — the F-04 SHIM above). Kept
            // as an explicit arm so admitting composites at THIS seam is a
            // deliberate decision, not an accidental fall-through.
            ReleaseSymbolVerdict::WiredInPlace(_) | ReleaseSymbolVerdict::NoDropPath => Some(()),
        }
    }

    fn lower_actor_send(
        &mut self,
        receiver: &HirExpr,
        method_id: &str,
        args: &[hew_hir::HirExpr],
        site: hew_hir::SiteId,
    ) -> Option<Place> {
        let info = self.actor_method_info(&receiver.ty, method_id, site)?;
        // `ActorMethodKind::Fire` is only produced by `record_actor_method_dispatch`
        // when `reply_ty == Ty::Unit`, so every `ActorSend` HIR node refers to a
        // unit-returning handler by construction.  The arity check below is the
        // remaining structural guard.
        if info.param_tys.len() != args.len() {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: format!("actor send arity mismatch for `{method_id}`"),
                    site,
                },
                note: format!(
                    "handler expects {} argument(s), call supplied {}",
                    info.param_tys.len(),
                    args.len()
                ),
            });
            return None;
        }
        let actor = self.lower_value(receiver)?;
        let child_ref = self.fungible_child_ref_of(actor);
        // Argument evaluation stays HERE, in the pre-branch block: an argument
        // expression's effects are user-visible and must run whether or not a
        // fungible child is live — the liveness branch below decides DELIVERY,
        // never evaluation.
        let mut lowered: Vec<(Place, ResolvedTy)> = Vec::with_capacity(args.len());
        for arg in args {
            let place = self.lower_value(arg)?;
            let ty = self.subst_ty(&arg.ty);
            lowered.push((place, ty));
        }
        // The payload Place. Zero args: a unit local (owns nothing). One arg:
        // the argument's own place — no pack exists. Multi-arg: the packed
        // anonymous record; through a FUNGIBLE child reference the pack is
        // deferred into `live_bb` below, so the packed temp — whose bytes the
        // `Send` alone consumes — is never built on the discard path.
        let mut value = match &lowered[..] {
            [] => Some(self.alloc_local(ResolvedTy::Unit)),
            [(place, _)] => Some(*place),
            _ => None,
        };
        if value.is_none() && child_ref.is_none() {
            let (field_places, field_tys): (Vec<Place>, Vec<ResolvedTy>) =
                lowered.iter().cloned().unzip();
            value = Some(self.pack_actor_payload_from_places(field_places, field_tys));
        }
        let next = self.alloc_block();
        // F-04: a fire-and-forget send through a FUNGIBLE supervisor-child
        // reference re-resolves the current live child at the send site. On a
        // not-live slot (mid-restart or permanently down) the send fail-closes as
        // a recoverable no-op (the message is dropped, NOT a program-killing
        // trap) — the tell's contract is best-effort delivery, so dropping into a
        // restart window is the correct recoverable behaviour. The `recover_bb`
        // releases the undelivered payload values (#2126) and joins straight to
        // `next` so control flow continues normally; `live_bb` becomes the
        // current cursor where the multi-arg pack (if any) is built and the Send
        // terminator is emitted with the freshly-resolved child pointer.
        if let Some(child_ref) = child_ref {
            let (live_bb, recover_bb) = self.emit_fungible_reresolve(child_ref, actor);
            // recover_bb: not-live → the Send is skipped, so nothing consumes
            // the already-evaluated argument values. Release each one exactly
            // as the delivered edge would have consumed it (the two edges are
            // exclusive, so the release runs exactly once), then continue.
            self.start_block(recover_bb);
            for (place, ty) in &lowered {
                self.emit_undelivered_send_payload_release(*place, ty, site)?;
            }
            self.finish_current_block(Terminator::Goto { target: next });
            // live_bb: the freshly-resolved current child; the Send below
            // targets it. The multi-arg pack is built here, on the delivery
            // edge only (pure MIR construction over the pre-branch argument
            // places — no user effect moves across the branch).
            self.start_block(live_bb);
            if value.is_none() {
                let (field_places, field_tys): (Vec<Place>, Vec<ResolvedTy>) =
                    lowered.into_iter().unzip();
                value = Some(self.pack_actor_payload_from_places(field_places, field_tys));
            }
        }
        let value = value.expect("payload place is populated for every arity above");
        // Determine alias mode: look up the first argument's span in the
        // checker's `actor_send_aliasing` map.  Only an explicit `Alias`
        // classification promotes the mode; every `Copy(reason)` variant and
        // every absent entry defaults to `Copy` (fail-closed).
        let alias_mode = if args.len() == 1 {
            let key = hew_types::SpanKey::from(&args[0].span);
            match self.actor_send_aliasing.get(&key).copied() {
                Some(hew_types::ActorSendAliasing::Alias) => crate::model::SendAliasMode::Alias,
                // All Copy(reason) variants and missing entries → Copy (fail-closed).
                _ => crate::model::SendAliasMode::Copy,
            }
        } else {
            // Zero-arg send and the multi-arg packed-record payload: the
            // payload Place is unit / a fresh packed temp, never the first
            // arg's binding, so the per-arg alias classification does not
            // transfer — fail-closed Copy.
            crate::model::SendAliasMode::Copy
        };
        self.finish_current_block(Terminator::Send {
            actor,
            msg_type: info.msg_type,
            value,
            next,
            alias_mode,
        });
        self.start_block(next);
        None
    }

    /// Lower a `receive gen fn` call (`e.ticks()`, `t.stream(3)`) — decision 4
    /// for receive-gen-fn. Constructs a per-call bounded channel, then
    /// tell-sends a "start" message (the call's args plus the sink half) to
    /// the actor's stream-producer pump; the expression value is the stream
    /// half. No `await` here — the checker's ask-without-await guard already
    /// exempts generator methods.
    ///
    /// `ActorHandlerLayout`'s producer row (`lower_actor_handler_layouts`)
    /// carries `param_tys` = the handler's own params PLUS one trailing
    /// sink — the single pack/unpack authority shared with the dispatch
    /// trampoline's generic per-handler arg-unpack loop. Packing here mirrors
    /// `lower_actor_send`'s convention exactly: a single total field rides
    /// bare (no pack); two or more use `pack_actor_payload_from_places`.
    fn lower_actor_gen_stream(
        &mut self,
        receiver: &HirExpr,
        method: &str,
        args: &[hew_hir::HirExpr],
        expr: &HirExpr,
    ) -> Option<Place> {
        let site = expr.site;
        let info = self.actor_method_info(&receiver.ty, method, site)?;
        let Some((sink_ty, real_param_tys)) = info
            .param_tys
            .split_last()
            .map(|(sink, rest)| (sink.clone(), rest.to_vec()))
        else {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: format!("actor stream-producer `{method}` has no sink param"),
                    site,
                },
                note: "the producer row must carry at least the synthetic trailing sink \
                       (lower_actor_handler_layouts)"
                    .to_string(),
            });
            return None;
        };
        if real_param_tys.len() != args.len() {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: format!("actor stream-producer arity mismatch for `{method}`"),
                    site,
                },
                note: format!(
                    "handler expects {} argument(s), call supplied {}",
                    real_param_tys.len(),
                    args.len()
                ),
            });
            return None;
        }

        // `receive gen fn` calls do not route through `lower_direct_call`, so
        // enforce the same closure-env provenance boundary here before packing
        // the start message.
        self.reject_unproven_generator_fn_args(args);

        let actor = self.lower_value(receiver)?;
        let mut lowered: Vec<(Place, ResolvedTy)> = Vec::with_capacity(args.len() + 1);
        for arg in args {
            let place = self.lower_value(arg)?;
            let ty = self.subst_ty(&arg.ty);
            lowered.push((place, ty));
        }

        let stream_ty = self.subst_ty(&expr.ty);
        let (sink, stream) = self.build_receive_gen_channel(&sink_ty, stream_ty);

        lowered.push((sink, sink_ty));
        let value = if let [(place, _)] = &lowered[..] {
            *place
        } else {
            let (field_places, field_tys): (Vec<Place>, Vec<ResolvedTy>) =
                lowered.into_iter().unzip();
            self.pack_actor_payload_from_places(field_places, field_tys)
        };

        let next = self.alloc_block();
        self.finish_current_block(Terminator::Send {
            actor,
            msg_type: info.msg_type,
            value,
            next,
            // Args crossing into a stream-producer start message use the same
            // fail-closed default as the zero/multi-arg `ActorSend` cases;
            // per-arg alias classification for stream calls is a follow-on.
            alias_mode: crate::model::SendAliasMode::Copy,
        });
        self.start_block(next);
        Some(stream)
    }

    /// Construct the per-call bounded channel for a `receive gen fn` dispatch
    /// and extract both halves, returning `(sink, stream)`.
    ///
    /// The pair box (`hew_stream_channel`) is a transient two-pointer carrier
    /// whose lifetime is exactly this sequence: `hew_stream_pair_sink` and
    /// `hew_stream_pair_stream` each null-store their field on extraction
    /// (ownership transfer), so once both halves are out the box owns neither
    /// half. `hew_stream_pair_free` then releases ONLY the empty carrier —
    /// never the sink or stream — and is null-guarded, so a fail-closed null
    /// `pair` is a no-op rather than a fault. Skipping the free leaks the
    /// carrier box once per call.
    ///
    /// The capacity is a laziness/throughput tunable (delivery is eager
    /// producer drive with bounded-channel backpressure; a small buffer keeps
    /// it pull-equivalent).
    fn build_receive_gen_channel(
        &mut self,
        sink_ty: &ResolvedTy,
        stream_ty: ResolvedTy,
    ) -> (Place, Place) {
        let capacity = self.alloc_local(ResolvedTy::I64);
        self.push_instr(Instr::ConstI64 {
            dest: capacity,
            value: RECEIVE_GEN_STREAM_CAPACITY,
        });
        let opaque_ptr_ty = ResolvedTy::Pointer {
            is_mutable: true,
            pointee: Box::new(ResolvedTy::Unit),
        };
        let pair = self.alloc_local(opaque_ptr_ty);
        let after_channel = self.alloc_block();
        self.finish_current_block(Terminator::Call {
            callee: "hew_stream_channel".to_string(),
            builtin: None,
            args: vec![capacity],
            dest: Some(pair),
            next: after_channel,
        });
        self.start_block(after_channel);

        let sink = self.alloc_local(sink_ty.clone());
        let after_sink = self.alloc_block();
        self.finish_current_block(Terminator::Call {
            callee: "hew_stream_pair_sink".to_string(),
            builtin: None,
            args: vec![pair],
            dest: Some(sink),
            next: after_sink,
        });
        self.start_block(after_sink);

        let stream = self.alloc_local(stream_ty);
        let after_stream = self.alloc_block();
        self.finish_current_block(Terminator::Call {
            callee: "hew_stream_pair_stream".to_string(),
            builtin: None,
            args: vec![pair],
            dest: Some(stream),
            next: after_stream,
        });
        self.start_block(after_stream);

        // Free the now-empty carrier (see the doc comment above): both halves
        // are extracted, so this releases only the two-pointer box.
        let after_free = self.alloc_block();
        self.finish_current_block(Terminator::Call {
            callee: "hew_stream_pair_free".to_string(),
            builtin: None,
            args: vec![pair],
            dest: None,
            next: after_free,
        });
        self.start_block(after_free);

        (sink, stream)
    }

    /// Lower a sealed `select{}` expression to MIR.
    ///
    /// ## Shape produced
    ///
    /// ```text
    /// originating_bb:
    ///   <lower each arm's actor receiver + args / duration into Places>
    ///   Terminator::Select { arms, next: join_bb }
    ///
    /// arm_body_bb[i]:                    // entered when arm i wins
    ///   <body lowers; binding (if any) resolves through binding_locals
    ///    to the per-arm reply slot codegen writes via hew_reply_wait>
    ///   Move { dest: result_place, src: <arm body value> }
    ///   Terminator::Goto { target: join_bb }
    ///
    /// join_bb:                           // single convergence point
    ///   <subsequent function lowering continues here; the select's
    ///    value is result_place, written by exactly one arm body>
    /// ```
    ///
    /// ## Producer-bridge contract (consumed by codegen / slice 3)
    ///
    /// Codegen reads `Terminator::Select { arms, next }` and, for each
    /// currently-supported arm:
    ///   * `SelectArmKind::ActorAsk { actor, method, args }` — emits
    ///     `hew_reply_channel_new` + `hew_actor_ask_with_channel` per
    ///     arm in the originating block; calls `hew_select_first` to
    ///     pick a winner; on win, calls `hew_reply_wait` and writes the
    ///     reply into `arm.binding` (the reply slot MIR allocated),
    ///     then jumps to `arm.body_block`; on loss, calls
    ///     `hew_reply_channel_cancel` + `hew_reply_channel_free`.
    ///   * `SelectArmKind::AfterTimer { duration }` — wins when the
    ///     deadline elapses; jumps to `arm.body_block` with no binding.
    ///
    /// ## Out-of-scope consumer arm kinds (fail-closed)
    ///
    /// `StreamNext` and `TaskAwait` are produced in MIR so the producer
    /// boundary carries the sealed HIR shape forward. Backend target-specific
    /// fail-closed checks live in codegen, where the requested target is known.
    ///
    /// ## Cleanup-CFG composition (D24-2 / `ExitPath::Select`)
    ///
    /// The select terminator emits a `Terminator::Select`; the
    /// elaboration pass (`enumerate_exits` at lower.rs:6450) wires
    /// `ExitPath::Select { block: originating_bb, next: join_bb }`
    /// into `drop_plans` automatically — the function-wide LIFO drop
    /// plan is empty for this exit (per-arm loser cleanup happens at
    /// the codegen dispatch site, not at function exit).
    #[allow(
        clippy::too_many_lines,
        reason = "lower_select threads four phases — arm-kind rejection, \
                  block allocation, per-arm Place lowering + binding \
                  registration, and per-arm body emit — that don't \
                  factor cleanly into helpers without re-threading \
                  Builder state (binding_locals, statements buffer, \
                  current block cursor)"
    )]
    fn lower_select(
        &mut self,
        select: &HirSelect,
        expected_ty: &ResolvedTy,
        site: hew_hir::SiteId,
    ) -> Option<Place> {
        // Result local first so it dominates every arm-body's Move.
        // For Unit-typed selects the placeholder write is benign — no
        // load occurs in the join block. Mirrors the `lower_if` pattern.
        let result_place = self.alloc_local(expected_ty.clone());

        // Allocate body blocks for every arm and the single join block
        // up front so each `SelectArm.body_block` is known before the
        // originating block seals with `Terminator::Select`.
        let body_bbs: Vec<u32> = (0..select.arms.len()).map(|_| self.alloc_block()).collect();
        let join_bb = self.alloc_block();

        // Lower per-arm operands and allocate per-arm value slots in the
        // ORIGINATING block.
        // Codegen consumes the SelectArm payload to emit the per-arm
        // setup (channel alloc + ask issue) in the same originating
        // block before the `hew_select_first` dispatch.
        let mut mir_arms: Vec<SelectArm> = Vec::with_capacity(select.arms.len());
        for (arm_index, arm) in select.arms.iter().enumerate() {
            let (kind, binding_place) = match &arm.kind {
                HirSelectArmKind::ActorAsk {
                    actor,
                    method,
                    args,
                } => {
                    // Resolve the actor handler's reply type so the
                    // reply slot is typed correctly. Mirrors the
                    // single-arm `lower_actor_ask` path; differs only
                    // in that the wait + bind happen across the
                    // Terminator::Select boundary, not Terminator::Ask.
                    let info = self.actor_method_info(&actor.ty, method, site)?;
                    if info.param_tys.len() != args.len() {
                        self.diagnostics.push(MirDiagnostic {
                            kind: MirDiagnosticKind::NotYetImplemented {
                                construct: format!(
                                    "select actor-ask arm arity mismatch for `{method}`"
                                ),
                                site,
                            },
                            note: format!(
                                "handler expects {} argument(s), arm supplied {}",
                                info.param_tys.len(),
                                args.len()
                            ),
                        });
                        return None;
                    }
                    let actor_place = self.lower_value(actor)?;
                    // F-04: a select arm asking a FUNGIBLE supervisor-child
                    // reference re-resolves the current child into the handle
                    // alloca before the select dispatch. Like the single-shot
                    // ask, no liveness branch is needed here: a not-live slot
                    // resolves to a null handle, and the runtime ask path
                    // fail-closes a null/stale actor to an ask error
                    // (`actor_send_result_internal_reply` null-guard) rather than
                    // a UAF or trap.
                    if let Some(child_ref) = self.fungible_child_ref_of(actor_place) {
                        self.emit_child_get_into(
                            child_ref.sup_place,
                            child_ref.slot_index,
                            actor_place,
                        );
                    }
                    let arg_places: Option<Vec<Place>> =
                        args.iter().map(|a| self.lower_value(a)).collect();
                    let arg_places = arg_places?;
                    // Pack args into a single payload Place using the
                    // same helper single-shot ask lowering uses. This
                    // is the codegen-side ABI shape: one payload ptr
                    // + size threads through `hew_actor_ask_with_channel`.
                    let payload_place = self.lower_actor_payload(args, site)?;
                    // Per-arm reply slot. Codegen writes
                    // `hew_reply_wait`'s result here on win before
                    // jumping into the arm body. Register against the
                    // HIR binding so the body's BindingRef resolves to
                    // this slot.
                    let reply_dest = self.alloc_local(info.return_ty.clone());
                    if let Some(binding_id) = arm.binding_id {
                        self.binding_locals.insert(binding_id, reply_dest);
                    }
                    (
                        SelectArmKind::ActorAsk {
                            actor: actor_place,
                            method: method.clone(),
                            args: arg_places,
                            msg_type: info.msg_type,
                            value: payload_place,
                        },
                        Some(reply_dest),
                    )
                }
                HirSelectArmKind::StreamNext { stream } => {
                    let stream_place = self.lower_value(stream)?;
                    let stream_ty = self.subst_ty(&stream.ty);
                    let item_ty = match stream_ty {
                        ResolvedTy::Named { name, mut args, .. }
                            if name == "Stream" && args.len() == 1 =>
                        {
                            args.remove(0)
                        }
                        _ => ResolvedTy::Unit,
                    };
                    let item_dest = self.alloc_local(item_ty);
                    if let Some(binding_id) = arm.binding_id {
                        self.binding_locals.insert(binding_id, item_dest);
                    }
                    (
                        SelectArmKind::StreamNext {
                            stream: stream_place,
                        },
                        Some(item_dest),
                    )
                }
                HirSelectArmKind::TaskAwait { task } => {
                    let task_place = self.lower_value(task)?;
                    let task_ty = self.subst_ty(&task.ty);
                    let await_ty = match task_ty {
                        ResolvedTy::Task(inner) => *inner,
                        _ => ResolvedTy::Unit,
                    };
                    let await_dest = self.alloc_local(await_ty);
                    if let Some(binding_id) = arm.binding_id {
                        self.binding_locals.insert(binding_id, await_dest);
                    }
                    (
                        SelectArmKind::TaskAwait { task: task_place },
                        Some(await_dest),
                    )
                }
                HirSelectArmKind::ChannelRecv { receiver } => {
                    // The arm binds `Option<T>` — the same shape an awaited
                    // `rx.recv()` produces; the winner edge pops the queued item
                    // via the non-blocking layout-witness try_recv and
                    // materialises it. The element type comes from the
                    // checker-resolved `Receiver<T>` handle type (mirror of the
                    // `Stream<T>` extraction on the StreamNext arm above), never
                    // from a runtime symbol name.
                    let recv_place = self.lower_value(receiver)?;
                    let receiver_ty = self.subst_ty(&receiver.ty);
                    // Dispatch on the typed builtin discriminator (with the
                    // short-name fallback): the name string is `"Receiver"`
                    // for a locally constructed handle but module-qualified
                    // (`"channel.Receiver"`) for an annotated parameter, and
                    // the bare-name compare silently fell through to the Unit
                    // witness. Mirrors `select_arm_binding_ty` in
                    // `hew-hir/src/lower.rs`.
                    let elem = match receiver_ty {
                        ResolvedTy::Named {
                            name,
                            mut args,
                            builtin,
                            ..
                        } if args.len() == 1
                            && (matches!(builtin, Some(hew_types::BuiltinType::Receiver))
                                || name.rsplit_once('.').map_or(name.as_str(), |(_, s)| s)
                                    == "Receiver") =>
                        {
                            args.remove(0)
                        }
                        // A malformed receiver type cannot supply a witness;
                        // Unit produces a zero-size witness the runtime
                        // aborts on fail-closed (mirrors the StreamNext arm's
                        // Unit fallback).
                        _ => ResolvedTy::Unit,
                    };
                    let option_ty = ResolvedTy::Named {
                        name: "Option".to_string(),
                        args: vec![elem.clone()],
                        builtin: None,
                        is_opaque: false,
                    };
                    let item_dest = self.alloc_local(option_ty);
                    if let Some(binding_id) = arm.binding_id {
                        self.binding_locals.insert(binding_id, item_dest);
                    }
                    (
                        SelectArmKind::ChannelRecv {
                            receiver: recv_place,
                            elem_ty: elem,
                        },
                        Some(item_dest),
                    )
                }
                HirSelectArmKind::AfterTimer { duration } => {
                    let duration_place = self.lower_value(duration)?;
                    // AfterTimer arms bind no value — `binding_id` is
                    // None by construction (HIR forbids `<name> from
                    // after ...` patterns). Defensive: even if a
                    // future HIR shape attached a binding, we'd skip
                    // registration since codegen has no value to write.
                    debug_assert!(
                        arm.binding_id.is_none(),
                        "AfterTimer arms must not carry a binding_id"
                    );
                    (
                        SelectArmKind::AfterTimer {
                            duration: duration_place,
                        },
                        None,
                    )
                }
            };
            mir_arms.push(SelectArm {
                kind,
                body_block: body_bbs[arm_index],
                binding: binding_place,
            });
        }

        // Seal the originating block with the select terminator.
        //
        // Suspendable-caller flip: in a caller that carries the execution
        // context (actor handler / closure / task entry) the `select{}`
        // SUSPENDS on the first-ready of its arms instead of busy-polling the
        // worker in `hew_select_first`. The `SuspendingSelect` carrier rides the
        // SAME `arms` payload (identical per-arm body blocks + bindings) but
        // codegen builds the readiness waitset + arms the deadline on the global
        // timer wheel + `coro.suspend`s, resuming on the first-ready wake and
        // cancelling the losers. `cleanup` reuses `join_bb` (the resume edge)
        // exactly as the recv / ask / sleep carriers do — the coro `cleanup`
        // outline is the abandon teardown owner, not this MIR block. A
        // `FunctionCallConv::Default` caller (`main`, free fn) has no parkable
        // continuation and keeps the blocking `Terminator::Select` /
        // `hew_select_first` path. Reuses the same `carries_execution_context`
        // discriminator as the recv / ask / await / sleep flips.
        if self.current_function_call_conv.carries_execution_context() {
            self.finish_current_block(Terminator::SuspendingSelect {
                arms: mir_arms,
                resume: join_bb,
                cleanup: join_bb,
            });
        } else {
            self.finish_current_block(Terminator::Select {
                arms: mir_arms,
                next: join_bb,
            });
        }

        // Per-arm body blocks. Each lowers the arm body; the body's
        // BindingRef (for ActorAsk arms with a binding) resolves
        // through `binding_locals` to the per-arm reply slot codegen
        // populated. AfterTimer arms have no binding by construction.
        // Every body block terminates with Goto join_bb so the join
        // converges (single-predecessor-per-arm CFG; the converging
        // result_place plays the role of an SSA phi for the join).
        for (arm_index, arm) in select.arms.iter().enumerate() {
            self.start_block(body_bbs[arm_index]);
            // ActorAsk arms with a value-bearing binding: emit a
            // `MirStatement::Bind` at the body-block entry so the
            // dataflow pass sees the binding initialised before the
            // body's `BindingRef` reads. Codegen writes
            // `hew_reply_wait`'s result into `SelectArm.binding` on
            // win, then jumps into this body block — the Bind here
            // mirrors that runtime initialisation in the MIR
            // statement stream.
            let mut arm_binding: Option<(BindingId, Place)> = None;
            if let (Some(binding_id), Some(binding_name)) =
                (arm.binding_id, arm.binding_name.as_ref())
            {
                let binding_ty = self.subst_ty(&arm.body.ty);
                // The arm body's HIR type matches the bound reply
                // type (HIR's `select_arm_binding_ty` for ActorAsk
                // arms reads the same `actor_method_dispatch` reply
                // type the body's `BindingRef.ty` carries).
                // Use the arm-binding's resolved type (not the body
                // expression's) by consulting binding_locals' Place
                // which we populated earlier.
                let _ = binding_ty;
                let binding_place = self
                    .binding_locals
                    .get(&binding_id)
                    .copied()
                    .expect("ActorAsk arm registered its reply slot before body lowering");
                let ty_of_place: ResolvedTy = match binding_place {
                    Place::Local(n) => self.locals[n as usize].clone(),
                    _ => arm.body.ty.clone(),
                };
                self.statements.push(MirStatement::Bind {
                    binding: binding_id,
                    name: binding_name.clone(),
                    site: arm.body.site,
                    ty: ty_of_place.clone(),
                });
                self.record_binding_scope(binding_id);
                // The select-arm binding owns the value the runtime
                // materialises into its slot on the win edge (the reply
                // channel / channel reaps only NON-consumed legs), so it
                // enters `owned_locals` exactly like a `let`-bound owned
                // local. Win-edge-only release is the DATAFLOW's
                // invariant, not this Bind's scope placement: the binding
                // is `Live` only in its own body block, the join-entry
                // meet demotes it to absent (`Uninit ⊔ X = Uninit`,
                // dataflow.rs), and the scope-close forward-`Goto` pass
                // releases it on the winning body-block→join edge — the
                // last point it is provably the live sole owner. Loser
                // arms' slots are never `Live`, so no drop can fire on
                // them. `ValueOwnership::classify` filters BitCopy types
                // to no-op drops, and the move-out mark below suppresses
                // the drop when the arm body moves the value out.
                // Registering here — the one site shared by every
                // value-bearing arm kind — covers ActorAsk, StreamNext,
                // TaskAwait, and ChannelRecv uniformly; AfterTimer arms
                // bind nothing and fall outside this block.
                self.register_owned_local(binding_id, binding_name.clone(), ty_of_place);
                arm_binding = Some((binding_id, binding_place));
            }
            let body_value = self.lower_value(&arm.body);
            if let Some(src) = body_value {
                // Direct escape (`=> r`): the arm body's value IS the arm
                // binding's own slot, and the Move below transfers it into
                // the select result. Mark the binding consumed so its
                // scope-exit drop is suppressed and the destination
                // (`let x = select { ... }`) is the sole owner — otherwise
                // two owned locals alias one heap pointer and the drop
                // provers fail closed to a leak on both.
                if let Some((binding_id, binding_place)) = arm_binding {
                    if src == binding_place {
                        self.mark_binding_moved(binding_id);
                    }
                }
                self.push_instr(Instr::Move {
                    dest: result_place,
                    src,
                });
            }
            self.finish_current_block(Terminator::Goto { target: join_bb });
        }

        // Join block — subsequent lowering continues here.
        self.start_block(join_bb);
        Some(result_place)
    }

    /// Lower a `join { ... }` expression — STAGE 1 fail-closed placeholder.
    ///
    /// Stage 2 replaces this with the real `Terminator::Join` producer (the
    /// wait-ALL sibling of `lower_select`). Until then, fail closed with a
    /// `NotYetImplemented` diagnostic so `hew check` reports a precise
    /// limitation rather than silently mis-lowering the construct.
    /// Lower a `join { ... }` expression — the wait-ALL sibling of
    /// `lower_select`. Allocates the result-tuple local, issues every
    /// branch ask (resolving the handler reply type + packing the
    /// payload exactly as the `select` `ActorAsk` arm does), and seals the
    /// originating block with `Terminator::Join`. Codegen waits for ALL
    /// replies and materialises the tuple; per HEW-SPEC-2026 §4.11.2 a
    /// branch trap cancels the remaining branches and propagates.
    fn lower_join(
        &mut self,
        join: &HirJoin,
        expected_ty: &ResolvedTy,
        site: hew_hir::SiteId,
    ) -> Option<Place> {
        // Result tuple local first so it dominates the next block.
        let result_place = self.alloc_local(expected_ty.clone());
        let next_bb = self.alloc_block();

        let mut mir_branches: Vec<JoinBranch> = Vec::with_capacity(join.branches.len());
        for branch in &join.branches {
            // Resolve the actor handler's reply type so the per-branch
            // reply slot is typed correctly — identical to the single-arm
            // `lower_actor_ask` / select ActorAsk path.
            let info = self.actor_method_info(&branch.actor.ty, &branch.method, site)?;
            if info.param_tys.len() != branch.args.len() {
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::NotYetImplemented {
                        construct: format!(
                            "join actor-ask branch arity mismatch for `{}`",
                            branch.method
                        ),
                        site,
                    },
                    note: format!(
                        "handler expects {} argument(s), branch supplied {}",
                        info.param_tys.len(),
                        branch.args.len()
                    ),
                });
                return None;
            }
            let actor_place = self.lower_value(&branch.actor)?;
            let arg_places: Option<Vec<Place>> =
                branch.args.iter().map(|a| self.lower_value(a)).collect();
            let arg_places = arg_places?;
            // Pack args into a single payload Place using the same helper
            // single-shot ask + select lowering use (codegen-side ABI:
            // one payload ptr + size through `hew_actor_ask_with_channel`).
            let payload_place = self.lower_actor_payload(&branch.args, site)?;
            // Per-branch reply slot. Codegen writes `hew_reply_wait`'s
            // result here, then composes it into `result_place`'s tuple
            // element at this branch's index.
            let reply_dest = self.alloc_local(info.return_ty.clone());
            mir_branches.push(JoinBranch {
                actor: actor_place,
                method: branch.method.clone(),
                args: arg_places,
                msg_type: info.msg_type,
                value: payload_place,
                reply_dest,
                reply_ty: info.return_ty.clone(),
            });
        }

        // Seal the originating block with the join terminator.
        self.finish_current_block(Terminator::Join {
            branches: mir_branches,
            result: result_place,
            next: next_bb,
        });

        // Continuation — subsequent lowering resumes after all replies
        // landed and the result tuple is bound.
        self.start_block(next_bb);
        Some(result_place)
    }

    fn lower_actor_ask(
        &mut self,
        receiver: &HirExpr,
        method_id: &str,
        args: &[hew_hir::HirExpr],
        reply_ty: &ResolvedTy,
        deadline_ns: Option<i64>,
        expr: &HirExpr,
    ) -> Option<Place> {
        let site = expr.site;
        let info = self.actor_method_info(&receiver.ty, method_id, site)?;
        if info.return_ty != *reply_ty {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: format!("actor ask reply type mismatch for `{method_id}`"),
                    site,
                },
                note: format!(
                    "handler returns {}, ask expression expects {}",
                    info.return_ty.user_facing(),
                    reply_ty.user_facing()
                ),
            });
            return None;
        }
        if info.param_tys.len() != args.len() {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: format!("actor ask arity mismatch for `{method_id}`"),
                    site,
                },
                note: format!(
                    "handler expects {} argument(s), call supplied {}",
                    info.param_tys.len(),
                    args.len()
                ),
            });
            return None;
        }
        let actor = self.lower_value(receiver)?;
        // F-04: an ask through a FUNGIBLE supervisor-child reference re-resolves
        // the current child at the ask site. Unlike the tell path, the ask needs
        // NO liveness branch: a not-live slot resolves to a null handle, and the
        // existing ask err path fail-closes a null/stale actor to
        // `Err(AskError::ActorStopped)` (`actor_send_result_internal_reply`
        // null-guards at actor.rs:4078 → `ErrActorStopped` →
        // `hew_actor_ask_take_last_error` → `Err`). So storing the freshly
        // resolved pointer (null when not-live) into the handle alloca is
        // sufficient: a live child is asked, a not-live one yields a recoverable
        // `Err` rather than a UAF or trap.
        if let Some(child_ref) = self.fungible_child_ref_of(actor) {
            self.emit_child_get_into(child_ref.sup_place, child_ref.slot_index, actor);
        }
        let value = self.lower_actor_payload(args, site)?;
        // `result_dest` holds `Result<R, AskError>` — the R-ASK unified return type.
        // Its type comes from the HIR expression's checker-assigned type, which is
        // `Result<reply_ty, AskError>` after the unification fix in the type checker.
        let result_dest = self.alloc_local(self.subst_ty(&expr.ty));
        let reply_dest = self.alloc_local(reply_ty.clone());
        let error_dest = self.alloc_local(ResolvedTy::Named {
            name: "AskError".to_string(),
            args: Vec::new(),
            builtin: Some(BuiltinType::AskError),
            is_opaque: false,
        });
        let next = self.alloc_block();
        // Suspendable-caller flip (W6.010, E2/D-W2). A caller that carries the
        // execution context (an actor handler / closure / task entry) runs on
        // the scheduler as a coroutine and can PARK its continuation: emit the
        // non-blocking `SuspendingAsk` so the ask suspends (freeing the worker)
        // and resumes on the reply, binding the value on the resume edge. A
        // `FunctionCallConv::Default` caller (`main`, a free function) runs on a
        // foreign/main thread with no parkable continuation, so it keeps the
        // blocking `Terminator::Ask` (the condvar path, E6). The resume edge IS
        // `next` — lowering continues in the same block where the ask result is
        // already bound; `cleanup` routes to the codegen single-teardown
        // epilogue (`SuspendingAsk` carries no separate MIR cleanup block, so it
        // reuses `next` as the drop-elaboration cleanup seam, exactly as the
        // codegen `Terminator::Suspend` case-1 edge routes to the shared
        // epilogue).
        if self.current_function_call_conv.carries_execution_context() {
            // NEW-6b: record an `await … | after d` deadline for THIS block (the
            // one finished with `SuspendingAsk`). Only a suspendable caller carries
            // the deadline — a blocking `Terminator::Ask` (foreign/main thread) has
            // no parkable continuation to time out, so a deadline there fails
            // closed below. Literal-only ns (constant side-table, no Place
            // threaded into the IR).
            if let Some(ns) = deadline_ns {
                self.await_deadline_ns.insert(self.current_block_id, ns);
            }
            self.record_suspend_kind(SuspendKind::Ask {
                actor,
                msg_type: info.msg_type,
                value,
                result_dest,
                reply_dest,
                error_dest,
            });
            self.finish_current_block(Terminator::Suspend {
                resume: next,
                cleanup: next,
                is_final: false,
            });
        } else {
            // A blocking caller (`main`, a free function) has no parkable
            // continuation to time out: the deadline cannot be armed on the
            // condvar path, and silently dropping it would turn `| after d`
            // into an unbounded wait. Refuse instead of miscompiling.
            if deadline_ns.is_some() {
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::NotYetImplemented {
                        construct: format!(
                            "`await {method_id}(...) | after d` from a blocking caller \
                             (`main` or a free function)"
                        ),
                        site,
                    },
                    note: "the deadline timer arms against a suspended continuation; \
                           only actor handlers, closures, and task entries suspend — \
                           move the deadline ask into an actor handler"
                        .to_string(),
                });
                return None;
            }
            self.finish_current_block(Terminator::Ask {
                actor,
                msg_type: info.msg_type,
                value,
                result_dest,
                reply_dest,
                error_dest,
                next,
            });
        }
        self.start_block(next);
        Some(result_dest)
    }

    /// Lower `await conn.read()` / `await conn.read_string()` (NEW-1). Mirrors
    /// `lower_actor_ask`'s suspendable-caller flip:
    ///
    /// - A caller that carries the execution context (actor handler / closure /
    ///   task entry) lowers to `Terminator::SuspendingRead`: the read suspends
    ///   (freeing the worker) and resumes when the reactor reports the fd ready,
    ///   binding the bytes on the resume edge.
    /// - A `FunctionCallConv::Default` caller (`main`, free fn) runs on a
    ///   foreign/main thread with no parkable continuation, so it keeps the
    ///   blocking `hew_tcp_read` FFI call (E8).
    ///
    /// `read_string` reads `bytes` then converts via `hew_bytes_to_string`; the
    /// suspend carrier always binds `bytes` (the value-routing destination), and
    /// the string conversion wraps the bound bytes on the resume edge.
    fn lower_conn_await_read(
        &mut self,
        conn: &HirExpr,
        to_string: bool,
        deadline_ns: Option<i64>,
        expr: &HirExpr,
    ) -> Option<Place> {
        let conn_place = self.lower_value(conn)?;
        let bytes_ty = ResolvedTy::Bytes;

        // The bytes slot the read binds (the SuspendingRead `result_dest` / the
        // blocking `hew_tcp_read` return).
        let bytes_dest = self.alloc_local(bytes_ty.clone());

        if self.current_function_call_conv.carries_execution_context() {
            let deadline_result_dest =
                deadline_ns.map(|_| self.alloc_local(self.subst_ty(&expr.ty)));
            let error_dest = deadline_ns.map(|_| {
                self.alloc_local(ResolvedTy::Named {
                    name: "NetError".to_string(),
                    args: Vec::new(),
                    builtin: None,
                    is_opaque: false,
                })
            });
            let next = self.alloc_block();
            if let Some(ns) = deadline_ns {
                self.await_deadline_ns.insert(self.current_block_id, ns);
            }
            // `SuspendingRead` carries no separate MIR cleanup block — it rides
            // the multi-suspend epilogue, so `cleanup` reuses `next` (exactly as
            // `SuspendingAsk` does).
            self.record_suspend_kind(SuspendKind::Read {
                conn: conn_place,
                result_dest: bytes_dest,
                deadline_result_dest,
                error_dest,
                to_string: to_string && deadline_result_dest.is_some(),
            });
            self.finish_current_block(Terminator::Suspend {
                resume: next,
                cleanup: next,
                is_final: false,
            });
            self.start_block(next);
            if let Some(result_dest) = deadline_result_dest {
                if to_string {
                    // `read_string | after d` success path: codegen already
                    // converted bytes → string and packed Ok(string) into
                    // `result_dest` (via the `to_string` flag on the terminator).
                    // MIR skips the bytes-to-string call here — the conversion
                    // is codegen-side on the resume edge.
                    return Some(result_dest);
                }
                return Some(result_dest);
            }
        } else {
            if deadline_ns.is_some() {
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::NotYetImplemented {
                        construct: "`await conn.read() | after d` in a non-suspendable context"
                            .to_string(),
                        site: expr.site,
                    },
                    note: "read deadlines require a suspendable actor/closure/task context; \
                           default-call-convention functions have no parkable continuation to \
                           resume on timeout"
                        .to_string(),
                });
                return None;
            }
            // Default callers use the blocking read FFI: they run on a foreign/main
            // thread with no parkable continuation. Closure shims above fail closed
            // for captured Connection awaits until closure invocations can suspend.
            let next = self.alloc_block();
            self.finish_current_block(Terminator::Call {
                callee: "hew_tcp_read".to_string(),
                builtin: None,
                args: vec![conn_place],
                dest: Some(bytes_dest),
                next,
            });
            self.start_block(next);
        }

        if !to_string {
            return Some(bytes_dest);
        }
        // `read_string`: convert the bound bytes to a string.
        let string_dest = self.alloc_local(self.subst_ty(&expr.ty));
        let next = self.alloc_block();
        self.finish_current_block(Terminator::Call {
            callee: "hew_bytes_to_string".to_string(),
            builtin: None,
            args: vec![bytes_dest],
            dest: Some(string_dest),
            next,
        });
        self.start_block(next);
        Some(string_dest)
    }

    /// Lower `await rx.recv() | after d` (L4 phase 2, channel form). Suspending
    /// channel-recv with a deadline attached. When `deadline_ns` is present:
    ///
    /// - `expr.ty` is `Result<Option<T>, TimeoutError>` (set by HIR).
    /// - `result_dest` is allocated for the raw `Option<T>` slot; codegen wraps
    ///   it into `Ok(_)` or emits `Err(TimeoutError::Timeout)` via the
    ///   `deadline_result_dest` slot.
    ///
    /// In a non-suspendable context with a deadline, fail closed with a
    /// diagnostic (no parkable continuation → no deadline semantics possible).
    fn lower_channel_recv_await(
        &mut self,
        receiver: &HirExpr,
        deadline_ns: Option<i64>,
        expr: &HirExpr,
    ) -> Option<Place> {
        let receiver_place = self.lower_value(receiver)?;

        // When deadline is active, `expr.ty` is `Result<Option<T>, TimeoutError>`.
        // Extract `Option<T>` for result_dest; allocate outer slot for deadline_result_dest.
        let option_ty = if deadline_ns.is_some() {
            match &expr.ty {
                hew_types::ResolvedTy::Named { args, .. } if !args.is_empty() => {
                    self.subst_ty(&args[0])
                }
                other => self.subst_ty(other),
            }
        } else {
            self.subst_ty(&expr.ty)
        };

        // Extract the element type from `Option<T>`.
        let elem_ty = match &option_ty {
            hew_types::ResolvedTy::Named { args, .. } if !args.is_empty() => {
                self.subst_ty(&args[0])
            }
            other => other.clone(),
        };

        let result_dest = self.alloc_local(option_ty);

        if self.current_function_call_conv.carries_execution_context() {
            let deadline_result_dest =
                deadline_ns.map(|_| self.alloc_local(self.subst_ty(&expr.ty)));
            let error_dest = deadline_ns.map(|_| {
                self.alloc_local(hew_types::ResolvedTy::Named {
                    name: "TimeoutError".to_string(),
                    args: Vec::new(),
                    builtin: Some(hew_types::BuiltinType::TimeoutError),
                    is_opaque: false,
                })
            });
            let next = self.alloc_block();
            if let Some(ns) = deadline_ns {
                self.await_deadline_ns.insert(self.current_block_id, ns);
            }
            self.record_suspend_kind(SuspendKind::ChannelRecv {
                receiver: receiver_place,
                result_dest,
                elem_ty: elem_ty.clone(),
                deadline_result_dest,
                error_dest,
            });
            self.finish_current_block(Terminator::Suspend {
                resume: next,
                cleanup: next,
                is_final: false,
            });
            self.start_block(next);
            if let Some(outer_dest) = deadline_result_dest {
                return Some(outer_dest);
            }
        } else {
            if deadline_ns.is_some() {
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::NotYetImplemented {
                        construct: "`await rx.recv() | after d` in a non-suspendable context"
                            .to_string(),
                        site: expr.site,
                    },
                    note: "channel recv deadlines require a suspendable actor/closure/task \
                           context; default-call-convention functions have no parkable \
                           continuation to resume on timeout"
                        .to_string(),
                });
                return None;
            }
            // Default callers keep the blocking hew_channel_recv_layout FFI call.
            let next = self.alloc_block();
            self.finish_current_block(Terminator::Call {
                callee: "hew_channel_recv_layout".to_string(),
                builtin: hew_types::runtime_call::RuntimeCallFamily::from_c_symbol(
                    "hew_channel_recv_layout",
                ),
                args: vec![receiver_place],
                dest: Some(result_dest),
                next,
            });
            self.start_block(next);
        }

        Some(result_dest)
    }

    /// Lower `await stream.recv() | after d` (L4 phase 2, stream form). Suspending
    /// stream-recv with a deadline attached. When `deadline_ns` is present:
    ///
    /// - `expr.ty` is `Result<Option<T>, TimeoutError>` (set by HIR).
    /// - `result_dest` is allocated for the raw `Option<T>` slot; codegen wraps
    ///   it into `Ok(_)` or emits `Err(TimeoutError::Timeout)` via the
    ///   `deadline_result_dest` slot.
    ///
    /// In a non-suspendable context with a deadline, fail closed with a
    /// diagnostic (no parkable continuation → no deadline semantics possible).
    fn lower_stream_recv_await(
        &mut self,
        stream: &HirExpr,
        deadline_ns: Option<i64>,
        expr: &HirExpr,
    ) -> Option<Place> {
        let stream_place = self.lower_value(stream)?;

        // When deadline is active, `expr.ty` is `Result<Option<T>, TimeoutError>`.
        // Extract `Option<T>` for result_dest; allocate outer slot for deadline_result_dest.
        let option_ty = if deadline_ns.is_some() {
            match &expr.ty {
                hew_types::ResolvedTy::Named { args, .. } if !args.is_empty() => {
                    self.subst_ty(&args[0])
                }
                other => self.subst_ty(other),
            }
        } else {
            self.subst_ty(&expr.ty)
        };

        // Extract the element type from `Option<T>`.
        let elem_ty = match &option_ty {
            hew_types::ResolvedTy::Named { args, .. } if !args.is_empty() => {
                self.subst_ty(&args[0])
            }
            other => other.clone(),
        };

        let result_dest = self.alloc_local(option_ty);

        if self.current_function_call_conv.carries_execution_context() {
            let deadline_result_dest =
                deadline_ns.map(|_| self.alloc_local(self.subst_ty(&expr.ty)));
            let error_dest = deadline_ns.map(|_| {
                self.alloc_local(hew_types::ResolvedTy::Named {
                    name: "TimeoutError".to_string(),
                    args: Vec::new(),
                    builtin: Some(hew_types::BuiltinType::TimeoutError),
                    is_opaque: false,
                })
            });
            let next = self.alloc_block();
            if let Some(ns) = deadline_ns {
                self.await_deadline_ns.insert(self.current_block_id, ns);
            }
            self.record_suspend_kind(SuspendKind::StreamNext {
                stream: stream_place,
                result_dest,
                elem_ty: elem_ty.clone(),
                deadline_result_dest,
                error_dest,
            });
            self.finish_current_block(Terminator::Suspend {
                resume: next,
                cleanup: next,
                is_final: false,
            });
            self.start_block(next);
            if let Some(outer_dest) = deadline_result_dest {
                return Some(outer_dest);
            }
        } else {
            if deadline_ns.is_some() {
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::NotYetImplemented {
                        construct: "`await stream.recv() | after d` in a non-suspendable context"
                            .to_string(),
                        site: expr.site,
                    },
                    note: "stream recv deadlines require a suspendable actor/closure/task \
                           context; default-call-convention functions have no parkable \
                           continuation to resume on timeout"
                        .to_string(),
                });
                return None;
            }
            // Default callers keep the blocking hew_stream_next_layout FFI call.
            let next = self.alloc_block();
            self.finish_current_block(Terminator::Call {
                callee: "hew_stream_next_layout".to_string(),
                builtin: hew_types::runtime_call::RuntimeCallFamily::from_c_symbol(
                    "hew_stream_next_layout",
                ),
                args: vec![stream_place],
                dest: Some(result_dest),
                next,
            });
            self.start_block(next);
        }

        Some(result_dest)
    }

    /// Lower `await listener.accept()` (NEW-2). The listener-readiness sibling of
    /// [`Self::lower_conn_await_read`]:
    ///
    /// - A caller that carries the execution context (actor handler / closure /
    ///   task entry) lowers to `Terminator::SuspendingAccept`: the accept
    ///   suspends (freeing the worker) and resumes when the reactor reports the
    ///   listener ready, binding the accepted `Connection` on the resume edge.
    /// - A `FunctionCallConv::Default` caller (`main`, free fn) runs on a
    ///   foreign/main thread with no parkable continuation, so it keeps the
    ///   blocking `hew_tcp_accept` FFI call (the caller-conv flip mirrors
    ///   `lower_conn_await_read`).
    fn lower_listener_await_accept(
        &mut self,
        listener: &HirExpr,
        deadline_ns: Option<i64>,
        expr: &HirExpr,
    ) -> Option<Place> {
        let listener_place = self.lower_value(listener)?;

        // When a deadline is active, `expr.ty` is `Result<Connection, NetError>` (set
        // by HIR). The raw `Connection` slot is the `result_dest`; codegen wraps it
        // into `Ok(_)` or binds `Err(NetError::TimedOut)` into `deadline_result_dest`.
        // Without a deadline, `expr.ty` is `Connection` directly.
        let conn_ty = if deadline_ns.is_some() {
            // Extract the Ok arm type (Connection) from Result<Connection, NetError>.
            match &expr.ty {
                hew_types::ResolvedTy::Named { args, .. } if !args.is_empty() => {
                    self.subst_ty(&args[0])
                }
                other => self.subst_ty(other),
            }
        } else {
            self.subst_ty(&expr.ty)
        };

        // The `Connection` slot the accept binds (the SuspendingAccept
        // `result_dest` / the blocking `hew_tcp_accept` return).
        let conn_dest = self.alloc_local(conn_ty);

        if self.current_function_call_conv.carries_execution_context() {
            let deadline_result_dest =
                deadline_ns.map(|_| self.alloc_local(self.subst_ty(&expr.ty)));
            let error_dest = deadline_ns.map(|_| {
                self.alloc_local(hew_types::ResolvedTy::Named {
                    name: "NetError".to_string(),
                    args: Vec::new(),
                    builtin: None,
                    is_opaque: false,
                })
            });
            let next = self.alloc_block();
            if let Some(ns) = deadline_ns {
                self.await_deadline_ns.insert(self.current_block_id, ns);
            }
            // `SuspendingAccept` carries no separate MIR cleanup block — it rides
            // the multi-suspend epilogue, so `cleanup` reuses `next` (exactly as
            // `SuspendingRead` does).
            self.record_suspend_kind(SuspendKind::Accept {
                listener: listener_place,
                result_dest: conn_dest,
                deadline_result_dest,
                error_dest,
            });
            self.finish_current_block(Terminator::Suspend {
                resume: next,
                cleanup: next,
                is_final: false,
            });
            self.start_block(next);
            if let Some(result_dest) = deadline_result_dest {
                return Some(result_dest);
            }
        } else {
            if deadline_ns.is_some() {
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::NotYetImplemented {
                        construct: "`await ln.accept() | after d` in a non-suspendable context"
                            .to_string(),
                        site: expr.site,
                    },
                    note: "accept deadlines require a suspendable actor/closure/task context; \
                           default-call-convention functions have no parkable continuation to \
                           resume on timeout"
                        .to_string(),
                });
                return None;
            }
            // Default callers use the blocking accept FFI: they run on a
            // foreign/main thread with no parkable continuation.
            let next = self.alloc_block();
            self.finish_current_block(Terminator::Call {
                callee: "hew_tcp_accept".to_string(),
                builtin: None,
                args: vec![listener_place],
                dest: Some(conn_dest),
                next,
            });
            self.start_block(next);
        }

        Some(conn_dest)
    }

    fn remote_actor_method_info(
        &mut self,
        receiver_ty: &ResolvedTy,
        msg_ty: &ResolvedTy,
        reply_ty: &ResolvedTy,
        site: hew_hir::SiteId,
    ) -> Option<ActorMethodInfo> {
        let actor_name = actor_name_from_remote_pid_ty(receiver_ty)?;
        let Some(layout) = self.actor_layouts.get(actor_name) else {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: format!("remote actor ask on unknown actor `{actor_name}`"),
                    site,
                },
                note: "RemotePid<T> named an actor with no MIR actor layout".to_string(),
            });
            return None;
        };
        // The remote wire carries exactly ONE message value, so only
        // single-parameter handlers are remote-dispatchable. A multi-arg
        // handler whose first param matches the sent message must NOT be
        // selected: the local wire for that handler is the packed-args
        // anonymous record, and a single-value payload delivered to a
        // body that unpacks the full record reads out of bounds on the
        // receiving node. Fail closed with a distinct diagnostic
        // (`serializer-fail-closed`); the cross-node payload
        // serialization lane lands the positive path.
        let Some(handler) = layout.handlers.iter().find(|handler| {
            handler.param_tys.len() == 1
                && handler
                    .param_tys
                    .first()
                    .is_some_and(|param| param == msg_ty)
                && handler.return_ty == *reply_ty
        }) else {
            if let Some(multi) = layout.handlers.iter().find(|handler| {
                handler.param_tys.len() > 1
                    && handler
                        .param_tys
                        .first()
                        .is_some_and(|param| param == msg_ty)
                    && handler.return_ty == *reply_ty
            }) {
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::RemotePayloadUnsupported {
                        actor: actor_name.to_string(),
                        handler: multi.name.clone(),
                        site,
                    },
                    note: format!(
                        "receive fn `{}` takes {} parameters; a remote ask carries one \
                         message value and the cross-node codec is not seeded for \
                         packed multi-arg payloads. Declare a single-parameter handler \
                         (e.g. wrapping the fields in a record) for remote dispatch.",
                        multi.name,
                        multi.param_tys.len()
                    ),
                });
                return None;
            }
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: format!("remote actor ask handler lookup for `{actor_name}`"),
                    site,
                },
                note: format!(
                    "no receive handler accepts {} and returns {}",
                    msg_ty.user_facing(),
                    reply_ty.user_facing()
                ),
            });
            return None;
        };
        Some(ActorMethodInfo {
            msg_type: handler.msg_type,
            param_tys: handler.param_tys.clone(),
            return_ty: handler.return_ty.clone(),
        })
    }

    fn lower_remote_actor_ask(
        &mut self,
        receiver: &HirExpr,
        msg: &HirExpr,
        timeout_ms: &HirExpr,
        reply_ty: &ResolvedTy,
        expr: &HirExpr,
    ) -> Option<Place> {
        let msg_ty = self.subst_ty(&msg.ty);
        let info = self.remote_actor_method_info(&receiver.ty, &msg_ty, reply_ty, expr.site)?;
        let actor = self.lower_value(receiver)?;
        let value = self.lower_value(msg)?;
        let timeout_ms = self.lower_value(timeout_ms)?;
        let result_dest = self.alloc_local(self.subst_ty(&expr.ty));
        let reply_dest = self.alloc_local(reply_ty.clone());
        let error_dest = self.alloc_local(ResolvedTy::Named {
            name: "AskError".to_string(),
            args: Vec::new(),
            builtin: Some(BuiltinType::AskError),
            is_opaque: false,
        });
        let next = self.alloc_block();
        // Suspendable-caller flip (NEW-5). A caller that carries the execution
        // context (an actor handler / closure / task entry) runs on the
        // scheduler as a coroutine and can PARK its continuation across the
        // cross-node wire round-trip: emit the non-blocking
        // `SuspendingRemoteAsk` so the ask suspends (freeing the worker) and
        // resumes when the wire reply / peer-drop / timeout lands, binding the
        // `Result<Reply, AskError>` on the resume edge. A
        // `FunctionCallConv::Default` caller (`main`, a free function) runs on a
        // foreign/main thread with no parkable continuation, so it keeps the
        // blocking `Terminator::RemoteAsk` (`hew_node_api_ask`). The resume edge
        // IS `next` (lowering continues in the block where the result is bound);
        // `cleanup` reuses `next` as the multi-suspend drop-elaboration seam,
        // exactly as `SuspendingAsk` does.
        if self.current_function_call_conv.carries_execution_context() {
            self.record_suspend_kind(SuspendKind::RemoteAsk {
                actor,
                msg_type: info.msg_type,
                value,
                timeout_ms,
                result_dest,
                reply_dest,
                error_dest,
                reply_ty: reply_ty.clone(),
            });
            self.finish_current_block(Terminator::Suspend {
                resume: next,
                cleanup: next,
                is_final: false,
            });
        } else {
            self.finish_current_block(Terminator::RemoteAsk {
                actor,
                msg_type: info.msg_type,
                value,
                timeout_ms,
                result_dest,
                reply_dest,
                error_dest,
                reply_ty: reply_ty.clone(),
                next,
            });
        }
        self.start_block(next);
        Some(result_dest)
    }

    fn invalid_spawn_arg_note(
        actor_name: &str,
        name: &str,
        explicit_init: bool,
        init_param_names: &[String],
        state_field_names: &[String],
    ) -> String {
        if explicit_init {
            // Both init params and state fields are valid spawn args when an
            // init() block is present (COEXIST model). Report the full valid set.
            let params = format!(
                "[{}]",
                init_param_names
                    .iter()
                    .map(|n| format!("`{n}`"))
                    .collect::<Vec<_>>()
                    .join(", ")
            );
            let fields = format!(
                "[{}]",
                state_field_names
                    .iter()
                    .map(|n| format!("`{n}`"))
                    .collect::<Vec<_>>()
                    .join(", ")
            );
            format!(
                "actor `{actor_name}` has no init() parameter or state field named `{name}`; \
                 init() parameters are: {params}; state fields are: {fields}"
            )
        } else {
            let fields = format!(
                "[{}]",
                state_field_names
                    .iter()
                    .map(|n| format!("`{n}`"))
                    .collect::<Vec<_>>()
                    .join(", ")
            );
            format!(
                "actor `{actor_name}` has no state field named `{name}`; \
                 fields are: {fields}"
            )
        }
    }

    #[allow(
        clippy::too_many_lines,
        reason = "one coherent dispatcher: supervisor-spawn routing (config-arg \
                  threading + no-config gate) followed by the actor-spawn arg \
                  validation + state lowering; splitting would scatter the spawn \
                  contract across helpers"
    )]
    fn lower_spawn_actor(
        &mut self,
        actor_name: &str,
        args: &[(String, HirExpr)],
        expr: &HirExpr,
    ) -> Option<Place> {
        // ── Supervisor dispatch ───────────────────────────────────────────
        // Check if `actor_name` names a supervisor before falling through to
        // the actor-layout path. Supervisors are not in `actor_layouts`; their
        // spawn is routed to the synthesised bootstrap function via a
        // `Terminator::Call` rather than `Instr::SpawnActor`.
        if let Some(sup_layout) = self.supervisor_layout_map.get(actor_name).cloned() {
            // A config supervisor (`supervisor App(config: T)`) is spawned as
            // `spawn App(config: cfg)`: the single config value is threaded to
            // the bootstrap's config parameter, which codegen reads to build the
            // supervisor-owned config buffer for the init thunks. A no-config
            // supervisor takes no args. The arg count must match whether the
            // layout carries a config param (the HIR gate is the user-facing
            // diagnostic; this is the MIR backstop).
            let expected_args = usize::from(sup_layout.config_param.is_some());
            if args.len() != expected_args {
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::NotYetImplemented {
                        construct: format!(
                            "supervisor `{actor_name}` spawn expected {expected_args} \
                             argument(s) (config param {}), got {}",
                            if expected_args == 0 {
                                "absent"
                            } else {
                                "present"
                            },
                            args.len()
                        ),
                        site: expr.site,
                    },
                    note: "a config supervisor is spawned `spawn App(config: value)`; a \
                           no-config supervisor `spawn App`"
                        .to_string(),
                });
                return None;
            }
            let bootstrap_args: Vec<hew_hir::HirExpr> =
                args.iter().map(|(_, expr)| expr.clone()).collect();
            // Route `spawn Sup` → `Terminator::Call { bootstrap_symbol }`.
            // `lower_direct_call` allocates the destination local (typed
            // `LocalPid<Sup>` from `expr.ty`) and emits the call terminator,
            // passing the config value (if any) as the bootstrap's config arg.
            return self.lower_direct_call(
                &sup_layout.bootstrap_symbol,
                None,
                &bootstrap_args,
                &expr.ty,
                expr.site,
            );
        }
        // ── Actor dispatch (existing path) ───────────────────────────────
        let Some(layout) = self.actor_layouts.get(actor_name).cloned() else {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: format!("spawn of unknown actor `{actor_name}`"),
                    site: expr.site,
                },
                note: "named actor spawn requires a MIR actor layout".to_string(),
            });
            return None;
        };
        let explicit_init = layout.init_symbol.is_some();
        let mut explicit: HashMap<&str, &HirExpr> = HashMap::new();
        for (name, arg) in args {
            // COEXIST model (A155): when an init() block is present, spawn args
            // may name EITHER init() parameters (routed to the init call) OR
            // state fields (routed to the initial state record). Without init(),
            // only state fields are valid.
            let is_valid = if explicit_init {
                layout.init_param_names.iter().any(|n| n == name)
                    || layout.state_field_names.iter().any(|n| n == name)
            } else {
                layout.state_field_names.iter().any(|n| n == name)
            };
            if !is_valid {
                let note = Self::invalid_spawn_arg_note(
                    actor_name,
                    name,
                    explicit_init,
                    &layout.init_param_names,
                    &layout.state_field_names,
                );
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::InvalidActorSpawnArgument {
                        actor: actor_name.to_string(),
                        argument: name.clone(),
                        site: expr.site,
                    },
                    note,
                });
                return None;
            }
            if explicit.contains_key(name.as_str()) {
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::NotYetImplemented {
                        construct: format!("spawn `{actor_name}` duplicate argument `{name}`"),
                        site: expr.site,
                    },
                    note: "actor spawn arguments are named; each name may be supplied at most once"
                        .to_string(),
                });
                return None;
            }
            explicit.insert(name.as_str(), arg);
        }
        let init_args =
            self.lower_spawn_actor_init_args(actor_name, &layout, explicit_init, &explicit, expr)?;
        let Ok(state) = self.lower_spawn_actor_state_or_diag(
            actor_name,
            &layout,
            explicit_init,
            &explicit,
            expr,
        ) else {
            return None;
        };
        let slot = self.alloc_local(expr.ty.clone());
        let Place::Local(local_id) = slot else {
            unreachable!("alloc_local returns Place::Local");
        };
        let dest = Place::ActorHandle(local_id);
        self.push_instr(Instr::SpawnActor {
            actor_name: actor_name.to_string(),
            state,
            init_args,
            dest,
            max_heap_bytes: layout.max_heap_bytes,
            cycle_capable: layout.cycle_capable,
            mailbox_capacity: layout.mailbox_capacity,
            overflow_policy: layout.overflow_policy.clone(),
        });
        Some(dest)
    }

    fn lower_spawn_actor_state_or_diag(
        &mut self,
        actor_name: &str,
        layout: &ActorLayout,
        explicit_init: bool,
        explicit: &HashMap<&str, &HirExpr>,
        expr: &HirExpr,
    ) -> Result<Option<Place>, ()> {
        let diagnostics_before_state = self.diagnostics.len();
        let Ok(state) =
            self.lower_spawn_actor_state(actor_name, layout, explicit_init, explicit, expr)
        else {
            if self.diagnostics.len() == diagnostics_before_state {
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::NotYetImplemented {
                        construct: format!("spawn `{actor_name}` state lowering failed"),
                        site: expr.site,
                    },
                    note: "actor spawn state lowering failed before a field/value diagnostic was recorded"
                        .to_string(),
                });
            }
            return Err(());
        };
        Ok(state)
    }

    fn lower_spawn_actor_init_args(
        &mut self,
        actor_name: &str,
        layout: &ActorLayout,
        explicit_init: bool,
        explicit: &HashMap<&str, &HirExpr>,
        expr: &HirExpr,
    ) -> Option<Vec<Place>> {
        let mut init_args = Vec::new();
        if !explicit_init {
            return Some(init_args);
        }
        for param_name in &layout.init_param_names {
            let Some(arg) = explicit.get(param_name.as_str()) else {
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::NotYetImplemented {
                        construct: format!(
                            "spawn `{actor_name}` missing init parameter `{param_name}`"
                        ),
                        site: expr.site,
                    },
                    note: "actor spawn with an explicit init block requires every init parameter by name"
                        .to_string(),
                });
                return None;
            };
            init_args.push(self.lower_value(arg)?);
        }
        Some(init_args)
    }

    fn lower_spawn_actor_state(
        &mut self,
        actor_name: &str,
        layout: &ActorLayout,
        explicit_init: bool,
        explicit: &HashMap<&str, &HirExpr>,
        expr: &HirExpr,
    ) -> Result<Option<Place>, ()> {
        if layout.state_field_names.is_empty() {
            return Ok(None);
        }
        let state_ty = ResolvedTy::Named {
            name: actor_name.to_string(),
            args: Vec::new(),
            builtin: None,
            is_opaque: false,
        };
        let dest = self.alloc_local(state_ty.clone());
        let mut fields = Vec::new();
        for (idx, field_name) in layout.state_field_names.iter().enumerate() {
            let src = if let Some(arg) = explicit.get(field_name.as_str()) {
                self.lower_value(arg).ok_or(())?
            } else if let Some(default) = layout
                .state_field_defaults
                .get(idx)
                .and_then(std::option::Option::as_ref)
            {
                self.lower_value(default).ok_or(())?
            } else if explicit_init {
                self.default_actor_state_field_value(
                    actor_name,
                    field_name,
                    &layout.state_field_tys[idx],
                    expr.site,
                )
                .ok_or(())?
            } else {
                self.lower_spawn_actor_state_arg(actor_name, field_name, explicit, expr)
                    .ok_or(())?
            };
            fields.push((
                FieldOffset(
                    u32::try_from(idx)
                        .expect("actor state field count exceeds u32::MAX — impossible"),
                ),
                src,
            ));
        }
        self.push_instr(Instr::RecordInit {
            ty: state_ty,
            fields,
            dest,
        });
        Ok(Some(dest))
    }

    fn lower_spawn_actor_state_arg(
        &mut self,
        actor_name: &str,
        field_name: &str,
        explicit: &HashMap<&str, &HirExpr>,
        expr: &HirExpr,
    ) -> Option<Place> {
        let Some(arg) = explicit.get(field_name) else {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::MissingActorSpawnArgument {
                    actor: actor_name.to_string(),
                    field: field_name.to_string(),
                    site: expr.site,
                },
                note: "actor spawn without an init block requires every state field by declaration name"
                    .to_string(),
            });
            return None;
        };
        self.lower_value(arg)
    }

    fn default_actor_state_field_value(
        &mut self,
        actor_name: &str,
        field_name: &str,
        ty: &ResolvedTy,
        site: hew_hir::SiteId,
    ) -> Option<Place> {
        let dest = self.alloc_local(ty.clone());
        match ty {
            ResolvedTy::I8
            | ResolvedTy::I16
            | ResolvedTy::I32
            | ResolvedTy::I64
            | ResolvedTy::U8
            | ResolvedTy::U16
            | ResolvedTy::U32
            | ResolvedTy::U64
            | ResolvedTy::Isize
            | ResolvedTy::Usize
            | ResolvedTy::Bool
            | ResolvedTy::Char
            | ResolvedTy::Duration => {
                self.push_instr(Instr::ConstI64 { dest, value: 0 });
                Some(dest)
            }
            ResolvedTy::F32 => {
                self.push_instr(Instr::FloatLit {
                    dest,
                    value_bits: 0.0f32.to_bits().into(),
                    width: FloatWidth::F32,
                });
                Some(dest)
            }
            ResolvedTy::F64 => {
                self.push_instr(Instr::FloatLit {
                    dest,
                    value_bits: 0.0f64.to_bits(),
                    width: FloatWidth::F64,
                });
                Some(dest)
            }
            ResolvedTy::Unit => {
                self.push_instr(Instr::UnitLit { dest });
                Some(dest)
            }
            other => {
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::NotYetImplemented {
                        construct: format!(
                            "actor init default state value for field `{actor_name}.{field_name}`"
                        ),
                        site,
                    },
                    note: format!(
                        "state field `{field_name}` has type `{other:?}`; spawn-time init currently only zero-initializes scalar state before calling `__init`"
                    ),
                });
                None
            }
        }
    }

    fn sanitize_symbol_component(input: &str) -> String {
        input
            .chars()
            .map(|ch| if ch.is_ascii_alphanumeric() { ch } else { '_' })
            .collect()
    }

    fn closure_env_pointer_ty(env_ty: &ResolvedTy) -> ResolvedTy {
        ResolvedTy::Pointer {
            is_mutable: false,
            pointee: Box::new(env_ty.clone()),
        }
    }

    fn lower_closure_literal(
        &mut self,
        expr: &HirExpr,
        params: &[hew_hir::HirBinding],
        ret_ty: &ResolvedTy,
        body: &HirExpr,
        captures: &[hew_hir::HirClosureCapture],
        escape_kind: hew_types::ClosureEscapeKind,
    ) -> Option<Place> {
        // Build the foundation-API layout (plan §15.1). Stored on the
        // builder so downstream consumers (generator codegen, auto-lock
        // injection) can query `lock_slot_for()` / `has_suspend_in_body()`
        // without re-deriving facts. Lock-slot construction is gated
        // off here — the follow-on consumer flips the gate; the layout
        // API is the single source of truth for the slot tail offset
        // (plan §15.3 risk 8 mitigation).
        let capture_fields: Vec<crate::closure_env::CaptureField> = captures
            .iter()
            .enumerate()
            .map(|(idx, cap)| crate::closure_env::CaptureField {
                id: crate::closure_env::CaptureId(
                    u32::try_from(idx).expect("closure capture count exceeds u32::MAX"),
                ),
                ty: self.subst_ty(&cap.ty),
                mode: cap.mode,
                is_sync: cap.is_sync,
            })
            .collect();
        let layout = crate::closure_env::ClosureEnvLayout::build(
            capture_fields,
            body,
            escape_kind,
            /* enable_lock_slots = */ false,
        );
        // The layout's allocation strategy commits the env storage the
        // MakeClosure emit uses. `Stack` (Local escape class) keeps the
        // frame alloca address — the closure provably never outlives the
        // introducing scope. `Heap` (Escapes) promotes: with captures the
        // env is copied into a `hew_dyn_box_alloc` box so the pair stays
        // valid after this frame unwinds; capture-free escaping closures
        // store a null env instead (zero loads in the shim, nothing owned,
        // and null is the pair-drop protocol's "skip" signal). `ScopeOwned`
        // (Forked) closures never reach this lowering arm (they ride
        // `lower_spawned_closure_task`); if one ever does, the stack emit
        // preserves the pre-promotion behaviour rather than fabricating an
        // unowned heap box.
        let strategy = layout.allocation_strategy();
        let (shim_name, _env_ty, env_place, suspends) =
            self.materialize_closure_env(expr, params, ret_ty, body, captures, strategy)?;
        // Record the body-suspends verdict so the enclosing `Let` handler can
        // attribute it to the bound binding (the suspendable-callee
        // discriminator the closure-call site reads).
        self.pending_closure_literal_suspends = Some(suspends);

        let env_mode = match strategy {
            crate::closure_env::AllocationStrategy::Heap => {
                if captures.is_empty() {
                    crate::model::ClosureEnvMode::Null
                } else {
                    crate::model::ClosureEnvMode::HeapBox
                }
            }
            crate::closure_env::AllocationStrategy::Stack
            | crate::closure_env::AllocationStrategy::ScopeOwned => {
                crate::model::ClosureEnvMode::Stack
            }
        };
        // Record the escape verdict so the enclosing `Let` handler can admit
        // the bound pair into the closure-pair drop set (the same
        // pending-flag pattern `pending_closure_literal_suspends` uses).
        self.pending_closure_literal_heap =
            Some(strategy == crate::closure_env::AllocationStrategy::Heap);

        let closure_place = self.alloc_local(expr.ty.clone());
        self.push_instr(Instr::MakeClosure {
            fn_symbol: shim_name,
            env: env_place,
            dest: closure_place,
            env_mode,
        });

        Some(closure_place)
    }

    fn closure_env_capture_ownership(
        &self,
        strategy: crate::closure_env::AllocationStrategy,
        ty: &ResolvedTy,
    ) -> ClosureEnvFieldOwnership {
        match strategy {
            crate::closure_env::AllocationStrategy::Stack
            | crate::closure_env::AllocationStrategy::ScopeOwned => {
                ClosureEnvFieldOwnership::BorrowsOnly
            }
            crate::closure_env::AllocationStrategy::Heap => {
                let ty = self.subst_ty(ty);
                if ValueClass::of_ty(&ty, &self.type_classes) == ValueClass::BitCopy {
                    ClosureEnvFieldOwnership::BorrowsOnly
                } else {
                    ClosureEnvFieldOwnership::OwnsMoved
                }
            }
        }
    }

    fn closure_env_allocation_manifest(
        strategy: crate::closure_env::AllocationStrategy,
    ) -> ClosureEnvAllocation {
        match strategy {
            crate::closure_env::AllocationStrategy::Stack => ClosureEnvAllocation::Stack,
            crate::closure_env::AllocationStrategy::Heap => ClosureEnvAllocation::Heap,
            crate::closure_env::AllocationStrategy::ScopeOwned => ClosureEnvAllocation::ScopeOwned,
        }
    }

    #[allow(
        clippy::too_many_lines,
        reason = "closure env materialization keeps env layout registration, \
                  ownership manifest construction, and shim generation aligned"
    )]
    fn materialize_closure_env(
        &mut self,
        expr: &HirExpr,
        params: &[hew_hir::HirBinding],
        ret_ty: &ResolvedTy,
        body: &HirExpr,
        captures: &[hew_hir::HirClosureCapture],
        strategy: crate::closure_env::AllocationStrategy,
    ) -> Option<(String, ResolvedTy, Place, bool)> {
        let closure_id = self.next_closure_id;
        self.next_closure_id = self
            .next_closure_id
            .checked_add(1)
            .expect("closure id overflow");
        // A NESTED closure literal is lowered while `current_function_symbol`
        // is the PARENT closure's invoke shim (`__hew_closure_invoke_<path>`).
        // Using it verbatim as the owner re-prefixes the symbol on every level
        // (`__hew_closure_invoke___hew_closure_invoke_main_0_0`) — redundant and
        // unbounded in nesting depth. Strip the shim prefix so the owner is the
        // stable nesting PATH (`main_0`): child builders reset `next_closure_id`
        // to 0, so the parent path is what keeps sibling/cross-level shim names
        // unique. The `MakeClosure` and the shim both derive their symbol from
        // this single site, so they stay in lockstep.
        let owner_src = self
            .current_function_symbol
            .strip_prefix("__hew_closure_invoke_")
            .unwrap_or(&self.current_function_symbol);
        let owner = Self::sanitize_symbol_component(owner_src);
        let env_name = format!("__hew_closure_env_{owner}_{closure_id}");
        let shim_name = format!("__hew_closure_invoke_{owner}_{closure_id}");
        let env_ty = ResolvedTy::Named {
            name: env_name.clone(),
            args: vec![],
            builtin: None,
            is_opaque: false,
        };

        // Each capture field's `ResolvedTy` is substituted through the
        // per-monomorphisation map: a closure inside `fn f<T>(x: T)` that
        // captures `x` records the env field as the CONCRETE argument (`i64`,
        // `string`, …), never the bare type-parameter `T`. The env record
        // layout is walked verbatim by the codegen-readiness diagnostic
        // (`collect_layout_field_diagnostics`) with no subst map of its own, so
        // an un-substituted `T` here surfaces as `E_MIR UnknownType T`; it is
        // also the struct codegen lays out, so the field MUST be concrete for a
        // correct ABI. A non-generic origin takes the identity-map fast path.
        let env_field_tys: Vec<ResolvedTy> = captures
            .iter()
            .map(|capture| self.subst_ty(&capture.ty))
            .collect();
        self.closure_record_layouts
            .push(crate::model::RecordLayout {
                name: env_name,
                field_tys: env_field_tys,
                // Compiler-internal closure-env record: positional `-g` names.
                field_names: Vec::new(),
            });

        let mut field_pairs = Vec::with_capacity(captures.len());
        let mut failed = false;
        let allocation = Self::closure_env_allocation_manifest(strategy);
        for (idx, capture) in captures.iter().enumerate() {
            let offset =
                FieldOffset(u32::try_from(idx).expect("closure capture count exceeds u32::MAX"));

            // Defence-in-depth gate: a `LambdaPid<M,R>` (lambda-actor handle) must
            // never appear as a fn-closure capture env field. The authoritative
            // rejection is `TypeErrorKind::ClosureCapturesDuplexHandle` in the
            // checker's `check_call`; if MIR sees one here, the checker gate
            // was bypassed by a new source form. Fail closed rather than
            // misrouting to `hew_duplex_send` (wrong runtime ABI).
            if matches!(
                &capture.ty,
                ResolvedTy::Named { name, .. } if name == "LambdaPid"
            ) {
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::ClosureCapturesDuplexHandle {
                        name: capture.name.clone(),
                        site: expr.site,
                    },
                    note: format!(
                        "closure capture `{}` has type LambdaPid<_,_>; no env-materialization \
                         protocol exists — checker gate in `check_call` must have been bypassed",
                        capture.name
                    ),
                });
                failed = true;
                continue;
            }

            let (src, source_binding) = if let Some(place) =
                self.binding_locals.get(&capture.binding).copied()
            {
                (place, Some(capture.binding))
            } else if let Some(source) = self.capture_env_sources.get(&capture.binding).cloned() {
                let temp = self.alloc_local(source.ty.clone());
                self.push_instr(Instr::ClosureEnvFieldLoad {
                    env: source.env,
                    env_ty: source.env_ty,
                    field_offset: source.field_offset,
                    dest: temp,
                });
                (temp, None)
            } else {
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::CannotMaterializeClosureCapture {
                        binding: capture.binding,
                        name: capture.name.clone(),
                        site: expr.site,
                    },
                    note: format!(
                        "closure capture `{}` has no MIR backend slot or enclosing closure env field",
                        capture.name
                    ),
                });
                failed = true;
                continue;
            };
            let field_ty = self.subst_ty(&capture.ty);
            let ownership = self.closure_env_capture_ownership(strategy, &field_ty);
            if ownership == ClosureEnvFieldOwnership::OwnsMoved {
                if let Some(binding) = source_binding {
                    self.statements.push(MirStatement::Use {
                        binding,
                        name: capture.name.clone(),
                        site: expr.site,
                        ty: field_ty.clone(),
                        intent: IntentKind::Consume,
                    });
                }
            }
            field_pairs.push(ClosureEnvFieldInit {
                field_offset: offset,
                src,
                source_binding,
                capture_mode: capture.mode,
                allocation,
                ownership,
                source_is_parameter: self.funcupdate_param_ids.contains(&capture.binding),
            });
        }
        if failed {
            return None;
        }

        let env_place = self.alloc_local(env_ty.clone());
        self.push_instr(Instr::ClosureEnvInit {
            ty: env_ty.clone(),
            fields: field_pairs,
            dest: env_place,
        });

        let lowered = self.lower_closure_shim(&shim_name, &env_ty, params, ret_ty, body, captures);
        // The suspendable-callee discriminator: the closure's invoke shim is a
        // coroutine iff its lowered MIR carries a suspend terminator — the
        // IDENTICAL structural fact codegen's `is_coroutine` reads off the same
        // shim (`hew-codegen-rs` `declare_function`/`lower_function`). Deriving
        // the call-site driver decision from the same carriers makes the two
        // sites agree by construction (container-abi-ctor-op-agreement) — there
        // is no second suspends-tracker to drift.
        let suspends = lowered
            .raw
            .blocks
            .iter()
            .any(|b| terminator_is_suspend_carrier(&b.terminator));
        self.generated_functions.push(lowered);

        Some((shim_name, env_ty, env_place, suspends))
    }

    /// The module-level lookup tables every child `Builder` (closure shim,
    /// lambda-actor body, task-entry adapter) inherits from its parent, plus
    /// `Builder::default()` for everything per-function. A nested body must
    /// resolve the same module facts the parent resolves — in particular
    /// `actor_layouts` drives `actor_method_info` (sends to a captured pid)
    /// and the `UnknownType` silencing in the codegen-readiness walk, so a
    /// site that misses it diagnoses `actor call on unknown actor` for a
    /// perfectly well-formed send. Construction sites override only the
    /// per-function identity fields (`current_function_symbol`,
    /// `current_function_call_conv`, body flags) via struct-update syntax;
    /// any future shared table belongs HERE so it cannot silently miss one
    /// of the construction sites.
    fn child_builder_tables(&self) -> Builder {
        Builder {
            type_classes: self.type_classes.clone(),
            record_field_orders: self.record_field_orders.clone(),
            actor_layouts: self.actor_layouts.clone(),
            supervisor_layout_map: self.supervisor_layout_map.clone(),
            enum_layouts: self.enum_layouts.clone(),
            opaque_handle_names: self.opaque_handle_names.clone(),
            // Inherit the resource registry so a resource-bearing record dropped
            // inside a closure shim / lambda-actor / gen body classifies the
            // handle as `Resource` (runs its close), not the empty-registry
            // `OpaqueHandle` no-op that would leak it.
            resource_opaque_close: self.resource_opaque_close.clone(),
            machine_layout_names: self.machine_layout_names.clone(),
            module_fn_names: self.module_fn_names.clone(),
            module_generic_fn_names: self.module_generic_fn_names.clone(),
            funcupdate_fn_returns_fresh: self.funcupdate_fn_returns_fresh.clone(),
            param_ownership: self.param_ownership.clone(),
            subst: self.subst.clone(),
            call_site_type_args: self.call_site_type_args.clone(),
            supervisor_child_slots: self.supervisor_child_slots.clone(),
            actor_send_aliasing: self.actor_send_aliasing.clone(),
            task_entry_adapter_symbols: self.task_entry_adapter_symbols.clone(),
            // Child builders (closure shims, lambda-actor bodies, gen bodies)
            // inherit the parent's target pointer width so an isize/usize
            // div/shift lowered inside a closure emits the same per-target guard.
            pointer_width: self.pointer_width,
            // Destructive-funcupdate base provenance is computed once per
            // top-level function over the WHOLE body (the prescan recurses into
            // closure/gen bodies), and `BindingId`s are globally unique, so the
            // parent map already classifies this child's bindings. Inherit it so
            // a `{ ..base, f }` inside a closure is gated by the same proof
            // instead of failing closed for want of the map.
            funcupdate_base_proven: self.funcupdate_base_proven.clone(),
            // Same rationale: the enclosing function's by-value parameters are
            // globally-unique bindings a closure can capture and embed in a
            // funcupdate base, so the child must see them to reject the borrow.
            funcupdate_param_ids: self.funcupdate_param_ids.clone(),
            // #2648 — the module return-provenance context MUST reach every
            // child builder: without it the preflight classifies a resolved
            // module fn as an unknown item → interim `LegacyModuleCall`
            // fail-open → a `match wrap(captured)` INSIDE a closure minted an
            // owner the enclosing frame also releases (a reproduced
            // double-free). Never `Builder::default()` for this field on a
            // user-body child.
            call_scrutinee_provenance: self.call_scrutinee_provenance.clone(),
            // `call_scrutinee_local_freshness` is deliberately NOT inherited
            // (the spread leaves it the empty fail-closed default): the
            // parent's single-read/unaliased facts are computed for ONE
            // execution of the parent body, while a child body (closure /
            // lambda-actor / generator) can run any number of times per
            // parent execution — a captured local that looks single-read in
            // the parent may be re-read on every invocation. An empty map
            // admits NO local argument inside child bodies; literal /
            // fresh-ctor / Fresh-call arguments still admit.
            ..Builder::default()
        }
    }

    #[allow(
        clippy::too_many_lines,
        reason = "closure shim construction keeps raw/checked/elaborated MIR snapshots aligned"
    )]
    fn lower_closure_shim(
        &self,
        shim_name: &str,
        env_ty: &ResolvedTy,
        params: &[hew_hir::HirBinding],
        ret_ty: &ResolvedTy,
        body: &HirExpr,
        captures: &[hew_hir::HirClosureCapture],
    ) -> LoweredFunction {
        // Resolve the shim's return type through the per-monomorphisation subst
        // map: a closure in `fn f<T>() -> T` lowers its invoke shim with the
        // CONCRETE return ABI, never the bare `T`. The child builder inherits
        // `subst`, so the body's locals already substitute via `alloc_local`;
        // the shim's raw `return_ty`/`params` are built directly here and must
        // be substituted explicitly so codegen sees a concrete ABI.
        let ret_ty_subst = self.subst_ty(ret_ty);
        let ret_ty = &ret_ty_subst;
        let env_ptr_ty = Self::closure_env_pointer_ty(env_ty);
        // `child_builder_tables` carries the full shared-table list — notably
        // `actor_layouts`, so a closure body that sends to a captured pid
        // resolves `actor_method_info` exactly as the parent would, and
        // `supervisor_child_slots`, so the FieldAccess intercept arm fires for
        // a closure body that reads a child slot off a captured supervisor PID.
        let mut builder = Builder {
            current_function_symbol: shim_name.to_string(),
            current_function_call_conv: crate::model::FunctionCallConv::ClosureInvoke,
            ..self.child_builder_tables()
        };

        let env_place = builder.alloc_local(env_ptr_ty.clone());
        for (idx, capture) in captures.iter().enumerate() {
            builder.capture_env_sources.insert(
                capture.binding,
                CaptureEnvSource {
                    env: env_place,
                    env_ty: env_ty.clone(),
                    field_offset: FieldOffset(
                        u32::try_from(idx).expect("closure capture count exceeds u32::MAX"),
                    ),
                    ty: self.subst_ty(&capture.ty),
                },
            );
            if self
                .closure_pair_env_may_be_nonnull
                .contains(&capture.binding)
            {
                builder
                    .closure_pair_env_may_be_nonnull
                    .insert(capture.binding);
            }
            if self.closure_pair_null_env.contains(&capture.binding) {
                builder.closure_pair_null_env.insert(capture.binding);
            }
        }
        for param in params {
            let place = builder.alloc_local(param.ty.clone());
            builder.binding_locals.insert(param.id, place);
            builder.seed_fn_param_provenance(param);
        }

        // #2301 -- run the same owned-Vec-key / consumed-and-reassigned-binding
        // pre-pass on the closure body that `function_body` runs for a
        // top-level function. `lower_closure_shim` builds a brand-new child
        // `Builder` and lowers `body` directly via `lower_value` below; without
        // this call, `prepass_consumed_bindings`/`prepass_reassigned_bindings`
        // stay empty for every closure-local binding, so
        // `maybe_alloc_overwrite_guard_flag` never fires inside a closure body
        // and a `var` consumed on one control-flow arm and overwritten on a
        // sibling arm silently leaks its prior value (no guard flag, no
        // release before the overwrite) instead of getting the path-sensitive
        // release a byte-identical top-level function body would.
        builder.collect_expr_prepass_facts(body);

        if let Some(src) = builder.lower_value(body) {
            builder.instructions.push(Instr::Move {
                dest: Place::ReturnSlot,
                src,
            });
        }
        builder.statements.push(MirStatement::Return {
            site: Some(body.site),
            ty: ret_ty.clone(),
        });

        let mut blocks = builder.finalize_blocks(Terminator::Return);
        apply_nested_fresh_string_temp_drops(
            &mut blocks,
            &builder.suspend_kinds,
            &builder.locals,
            &builder.binding_locals,
            &mut builder.instr_spans,
        );
        // #2542 — mirror the closure-shim ramp's string splice for the bytes
        // user-call-result temp class (see `lower_function`'s call site).
        apply_nested_fresh_bytes_temp_drops(
            &mut blocks,
            &builder.suspend_kinds,
            &builder.locals,
            &builder.binding_locals,
            &mut builder.instr_spans,
        );
        let thir_statements: Vec<MirStatement> = blocks
            .iter()
            .flat_map(|b| b.statements.iter().cloned())
            .collect();
        let thir = ThirFunction {
            name: shim_name.to_string(),
            return_ty: ret_ty.clone(),
            statements: thir_statements,
        };
        let mut raw_params = Vec::with_capacity(params.len() + 1);
        raw_params.push(env_ptr_ty);
        raw_params.extend(params.iter().map(|param| self.subst_ty(&param.ty)));
        let mut raw = RawMirFunction {
            name: shim_name.to_string(),
            return_ty: ret_ty.clone(),
            call_conv: crate::model::FunctionCallConv::ClosureInvoke,
            params: raw_params,
            locals: builder.locals.clone(),
            // Synthesised closure-invoke shim: no faithful user bindings.
            local_names: Vec::new(),
            local_scopes: Vec::new(),
            local_decl_bytes: Vec::new(),
            scope_table: Vec::new(),
            blocks,
            decisions: builder.decisions.clone(),
            intrinsic_id: None,
            await_deadline_ns: builder.await_deadline_ns.clone(),
            suspend_kinds: builder.suspend_kinds.clone(),
            lambda_actor_user_param_locals: Vec::new(),
            span: None,
            instr_spans: ::std::collections::BTreeMap::new(),
            source_origin: SourceOrigin::Unknown,
        };
        let synthetic_func = HirFn {
            id: hew_hir::ItemId(0),
            node: hew_hir::HirNodeId(0),
            name: shim_name.to_string(),
            type_params: Vec::new(),
            is_generator: false,
            intrinsic_id: None,
            params: params.to_vec(),
            return_ty: ret_ty.clone(),
            body: hew_hir::HirBlock {
                node: hew_hir::HirNodeId(0),
                scope: hew_hir::ScopeId(0),
                statements: Vec::new(),
                tail: None,
                ty: ret_ty.clone(),
                span: body.span.clone(),
            },
            span: body.span.clone(),
        };
        let dataflow_result = check_function(&builder, &raw.blocks, &synthetic_func);
        let mut diagnostics: Vec<MirDiagnostic> = dataflow_result
            .checks
            .iter()
            .filter_map(check_to_diagnostic)
            .collect();
        diagnostics.append(&mut builder.diagnostics);
        collect_unknown_type_diagnostics(&synthetic_func, &builder, &mut diagnostics);
        let string_derivation = finalize_string_ownership(&mut raw, &builder, &dataflow_result);
        let bytes_derivation = finalize_bytes_ownership(&mut raw, &builder, &dataflow_result);
        let cooperate_sites = dataflow::compute_cooperate_sites(&raw.blocks);
        let checked = CheckedMirFunction {
            name: shim_name.to_string(),
            return_ty: ret_ty.clone(),
            blocks: raw.blocks.clone(),
            decisions: builder.decisions.clone(),
            checks: dataflow_result.checks.clone(),
            cooperate_sites,
        };
        let elaborated = elaborate(
            &checked,
            &builder,
            &thir.statements,
            &dataflow_result,
            Some(&string_derivation.allowed),
            Some(&bytes_derivation.allowed),
        );

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

    /// Synthesise a `ClosureInvoke`-ABI shim that forwards its user
    /// arguments to the named top-level function `fn_symbol` and stores
    /// the result into `ReturnSlot`. The shim is the bridge between the
    /// closure-pair calling convention (`ctx_ptr`, `env_ptr`, `...user_args`)
    /// and the plain calling convention of the target function.
    ///
    /// The `env_ptr` parameter (`locals[0]`) is **never loaded** — the named
    /// function has no captures. This is the core safety invariant: a
    /// null or garbage `env_ptr` is safe because the shim body contains
    /// zero `ClosureEnvFieldLoad` instructions.
    ///
    /// WHY: Named functions used as first-class values need a
    /// `FunctionCallConv::ClosureInvoke` wrapper so the uniform closure-call
    /// path (`lower_call_closure`) can invoke them through the closure pair.
    /// WHEN-OBSOLETE: if the runtime gains a separate fn-pointer ABI.
    /// WHAT-REAL: a native fn-pointer type that doesn't pretend to be a closure.
    #[allow(
        clippy::too_many_lines,
        reason = "exhaustive Terminator match arm in the shim's body \
                  (most recently the `MakeLambdaActor` arm) edges past \
                  the 100-line ceiling without changing the shim's \
                  single responsibility — synthesise a callable wrapper \
                  for the named fn"
    )]
    fn lower_named_fn_invoke_shim(
        &self,
        fn_symbol: &str,
        shim_name: &str,
        param_tys: &[ResolvedTy],
        ret_ty: &ResolvedTy,
    ) -> LoweredFunction {
        let env_ptr_ty = Self::closure_env_pointer_ty(&ResolvedTy::Unit);
        let mut builder = Builder {
            type_classes: self.type_classes.clone(),
            record_field_orders: self.record_field_orders.clone(),
            machine_layout_names: self.machine_layout_names.clone(),
            module_fn_names: self.module_fn_names.clone(),
            module_generic_fn_names: self.module_generic_fn_names.clone(),
            funcupdate_fn_returns_fresh: self.funcupdate_fn_returns_fresh.clone(),
            param_ownership: self.param_ownership.clone(),
            subst: self.subst.clone(),
            call_site_type_args: self.call_site_type_args.clone(),
            supervisor_child_slots: self.supervisor_child_slots.clone(),
            actor_send_aliasing: self.actor_send_aliasing.clone(),
            pointer_width: self.pointer_width,
            current_function_symbol: shim_name.to_string(),
            current_function_call_conv: crate::model::FunctionCallConv::ClosureInvoke,
            task_entry_adapter_symbols: self.task_entry_adapter_symbols.clone(),
            // #2648 — synthetic call wrapper (no user match scrutinees), but the
            // provenance context is threaded uniformly: no child builder falls
            // back to the legacy fail-open default.
            call_scrutinee_provenance: self.call_scrutinee_provenance.clone(),
            ..Builder::default()
        };

        // Allocate locals for env_ptr (ignored) and each user argument.
        // locals[0] = env_ptr (ClosureInvoke ABI; never loaded)
        // locals[1..n] = user arguments forwarded to fn_symbol
        let _env_place = builder.alloc_local(env_ptr_ty.clone());
        let mut arg_places = Vec::with_capacity(param_tys.len());
        for ty in param_tys {
            arg_places.push(builder.alloc_local(ty.clone()));
        }

        // Block 0: call the original function, storing return value into ReturnSlot.
        let ret_block_id = builder.alloc_block();
        builder.finish_current_block(Terminator::Call {
            callee: fn_symbol.to_string(),
            builtin: None,
            args: arg_places.clone(),
            dest: Some(Place::ReturnSlot),
            next: ret_block_id,
        });

        // Block 1: return.
        builder.start_block(ret_block_id);
        let blocks = builder.finalize_blocks(Terminator::Return);

        let thir_statements: Vec<MirStatement> = blocks
            .iter()
            .flat_map(|b| b.statements.iter().cloned())
            .collect();
        let thir = ThirFunction {
            name: shim_name.to_string(),
            return_ty: ret_ty.clone(),
            statements: thir_statements,
        };

        // Build params list: env_ptr_ty first (ClosureInvoke ABI), then user params.
        let mut raw_params = Vec::with_capacity(param_tys.len() + 1);
        raw_params.push(env_ptr_ty);
        raw_params.extend_from_slice(param_tys);
        let mut raw = RawMirFunction {
            name: shim_name.to_string(),
            return_ty: ret_ty.clone(),
            call_conv: crate::model::FunctionCallConv::ClosureInvoke,
            params: raw_params,
            locals: builder.locals.clone(),
            // Synthesised closure-invoke shim: no faithful user bindings.
            local_names: Vec::new(),
            local_scopes: Vec::new(),
            local_decl_bytes: Vec::new(),
            scope_table: Vec::new(),
            blocks: blocks.clone(),
            decisions: builder.decisions.clone(),
            intrinsic_id: None,
            await_deadline_ns: builder.await_deadline_ns.clone(),
            suspend_kinds: builder.suspend_kinds.clone(),
            lambda_actor_user_param_locals: Vec::new(),
            span: None,
            instr_spans: ::std::collections::BTreeMap::new(),
            source_origin: SourceOrigin::Unknown,
        };

        // Synthetic HirFn for dataflow checking — no HIR params (the shim
        // params are positional locals, not HIR bindings). An empty param list
        // is conservative: no param is pre-seeded as Live, so the checker
        // only sees the Bind/Move generated by the Terminator::Call dest.
        let synthetic_func = HirFn {
            id: hew_hir::ItemId(0),
            node: hew_hir::HirNodeId(0),
            name: shim_name.to_string(),
            type_params: Vec::new(),
            is_generator: false,
            intrinsic_id: None,
            params: Vec::new(),
            return_ty: ret_ty.clone(),
            body: hew_hir::HirBlock {
                node: hew_hir::HirNodeId(0),
                scope: hew_hir::ScopeId(0),
                statements: Vec::new(),
                tail: None,
                ty: ret_ty.clone(),
                span: 0..0,
            },
            span: 0..0,
        };
        let dataflow_result = check_function(&builder, &raw.blocks, &synthetic_func);
        let mut diagnostics: Vec<MirDiagnostic> = dataflow_result
            .checks
            .iter()
            .filter_map(check_to_diagnostic)
            .collect();
        diagnostics.append(&mut builder.diagnostics);
        collect_unknown_type_diagnostics(&synthetic_func, &builder, &mut diagnostics);
        let string_derivation = finalize_string_ownership(&mut raw, &builder, &dataflow_result);
        let bytes_derivation = finalize_bytes_ownership(&mut raw, &builder, &dataflow_result);
        let cooperate_sites = dataflow::compute_cooperate_sites(&raw.blocks);
        let checked = CheckedMirFunction {
            name: shim_name.to_string(),
            return_ty: ret_ty.clone(),
            blocks: raw.blocks.clone(),
            decisions: builder.decisions.clone(),
            checks: dataflow_result.checks.clone(),
            cooperate_sites,
        };
        let elaborated = elaborate(
            &checked,
            &builder,
            &thir.statements,
            &dataflow_result,
            Some(&string_derivation.allowed),
            Some(&bytes_derivation.allowed),
        );

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

    /// Lower an `HirExprKind::SpawnLambdaActor` literal to a MIR
    /// `Place::LambdaActorHandle`. The literal allocates a fresh
    /// local (typed as the actor's `Duplex<Msg, Reply>`) and emits a
    /// `Place::LambdaActorHandle(local_id)` so drop elaboration
    /// selects `DropKind::LambdaActorRelease` — the
    /// stop-on-last-handle-drop protocol with weak-ref body capture
    /// (§5.9 ratification 2).
    ///
    /// Every HIR-resolved capture is forwarded into the function's
    /// `lambda_captures` ledger after proving that the source binding has a MIR
    /// `Place` in `binding_locals`. A capture whose source binding has no backend
    /// slot is a lowering error, never a silently smaller capture set.
    ///
    /// Body lowering (the actor's per-message dispatch) is a
    /// follow-up slice; the MIR shape only needs the handle Place plus
    /// the capture metadata. Codegen rejects `Place::LambdaActorHandle`
    /// today (fail-closed) so a runtime substrate is not required for
    /// the static checks to land.
    #[allow(
        clippy::too_many_lines,
        reason = "the function carries the full spawn-side wiring — \
                  handle alloc, capture validation, body Builder \
                  construction, body HIR lowering, return-shape \
                  rewriting, and `Terminator::MakeLambdaActor` emission. \
                  Splitting it would scatter the body-fn synthesis \
                  contract across helpers without reducing the total \
                  surface area; the structural responsibility is one \
                  spawn site → one MIR body → one make-lambda-actor \
                  terminator and the function's shape mirrors that \
                  responsibility 1:1"
    )]
    fn lower_spawn_lambda_actor(&mut self, expr: &HirExpr) -> Place {
        let HirExprKind::SpawnLambdaActor {
            params,
            reply_ty,
            body,
            captures,
        } = &expr.kind
        else {
            unreachable!("lower_spawn_lambda_actor called on non-SpawnLambdaActor kind");
        };
        // Two paths produce the handle:
        //   - `let <name> = actor |..| { .. }`: the `stmt` Let arm
        //     pre-allocates the binding's slot and stashes its
        //     `LambdaActorHandle` in `pending_lambda_actor_handle`
        //     so the body's Weak self-capture finds a backend slot
        //     for the let-binding. Reuse the pre-allocated handle.
        //   - any non-let position (return-position literal, an
        //     argument, etc.): allocate a fresh local on the fly.
        let handle = if let Some(handle) = self.pending_lambda_actor_handle {
            handle
        } else {
            let local = self.alloc_local(expr.ty.clone());
            let Place::Local(local_id) = local else {
                unreachable!("alloc_local returns Place::Local");
            };
            Place::LambdaActorHandle(local_id)
        };
        for capture in captures {
            // Each captured binding must already have a backend slot in the
            // enclosing function. The forward-bound recursive self capture is
            // the let-binding itself, whose `binding_locals` entry was populated
            // by the `stmt` Let arm before this producer ran.
            if !self.binding_locals.contains_key(&capture.binding) {
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::CannotMaterializeClosureCapture {
                        binding: capture.binding,
                        name: capture.name.clone(),
                        site: expr.site,
                    },
                    note: format!(
                        "closure capture `{}` was resolved by HIR but has no MIR backend slot; \
                         refusing to drop the capture from the environment",
                        capture.name
                    ),
                });
                continue;
            }
            let capture_kind = match capture.kind {
                hew_hir::HirCaptureKind::Strong => crate::model::CaptureKind::Strong,
                hew_hir::HirCaptureKind::Weak => crate::model::CaptureKind::Weak,
            };
            self.lambda_captures.push(LambdaCapture {
                actor_handle: handle,
                captured: capture.binding,
                name: capture.name.clone(),
                capture_kind,
            });
        }

        // ── Body fn synthesis (M2 spawn-side) ──
        //
        // Mirrors `lower_gen_block`: mint a deterministic body-fn symbol,
        // synthesize a child `Builder` that lowers the lambda body under the
        // runtime ABI signature, push the LoweredFunction into
        // `self.generated_functions`, and emit `Terminator::MakeLambdaActor`
        // so codegen calls `hew_lambda_actor_new(...)` and stores the
        // returned handle into the enclosing function's
        // `LambdaActorHandle` slot.
        //
        // Scope:
        //   - Captures: materialised as a heap-boxed env record passed as
        //     the runtime state pointer (see the capture-env synthesis
        //     below). A capture-free lambda passes null state and the
        //     shared no-op state-drop stub.
        //   - Single user param (0 or 1): the codegen prologue copies
        //     `sizeof(user_ty)` bytes from `msg_ptr` into the param alloca.
        //     Multi-param bodies unpack each param at its packed-record
        //     field offset (see the LambdaActorBody prologue in codegen).
        //   - Tell shape (`reply_ty == Unit`): codegen returns i32 0
        //     unconditionally.
        //   - Ask shape: codegen serialises the body's ReturnSlot value into a
        //     fresh reply buffer via `hew_lambda_body_alloc_reply_buf`, stores
        //     the buf pointer + length into `*reply_out` / `*reply_len_out`,
        //     and returns i32 0.
        //
        // Drop safety (CLAUDE.md §1): the handle's release is scheduled at
        // scope exit by the enclosing function's LIFO drop plan via
        // `place_aware_drop_fn` → `hew_lambda_actor_release`. The state-drop
        // no-op stub is invoked exactly once by the runtime at actor
        // shutdown, releasing nothing for the no-capture MVP.
        // Multi-param bodies ride the packed-args anonymous-record wire: the
        // call site (`lower_lambda_actor_call`) packs the N args into one
        // record and the body prologue unpacks each param at its natural
        // field offset from `msg_ptr` (the codegen LambdaActorBody prologue
        // rebuilds the identical struct from the user-param local types).

        let lambda_id = self.next_closure_id;
        self.next_closure_id = self
            .next_closure_id
            .checked_add(1)
            .expect("lambda id overflow — closure id counter exhausted");
        let owner = Self::sanitize_symbol_component(&self.current_function_symbol);
        let body_name = format!("__hew_lambda_body_{owner}_{lambda_id}");

        // ── Capture-env synthesis ──
        //
        // Captures materialise as a synthetic env record in the enclosing
        // frame: one field per capture, in capture order. The supported
        // field classes are explicit and everything else fails closed:
        //
        //   - `Weak` self-handle (the forward-bound let-binding for
        //     recursion, §5.9 ratification 2): the field is OMITTED from
        //     the `RecordInit` — the handle does not exist until
        //     `hew_lambda_actor_new` returns. Codegen nulls the field at
        //     box time and back-fills it with the DOWNGRADED weak handle
        //     after construction; the env drop releases it via
        //     `hew_lambda_actor_weak_drop`. Holding a weak (not strong)
        //     self reference preserves stop-on-last-external-handle-drop.
        //   - Strong BitCopy scalar: the field store copies the value;
        //     the caller's binding stays live and untouched.
        //   - Strong pid (`LocalPid`): a BitCopy alias of an
        //     opaque identity reference with no drop glue. The field store
        //     copies the handle word; the caller's binding stays live and
        //     the env field gets no retain and no release — the actor's
        //     lifetime is runtime-owned.
        //   - Strong `string`: the field store copies the HANDLE bytes
        //     (an alias); codegen replaces the heap-env field with an
        //     independent `hew_string_clone` at box time, so the env owns
        //     its copy and the caller's binding remains the owner of the
        //     original. The env drop releases the clone via
        //     `hew_string_drop` exactly once at actor shutdown.
        //   - Anything else (Vec, HashMap, records, owned handles):
        //     `CannotMaterializeClosureCapture` — no silent shallow copy
        //     of an owned aggregate across the actor boundary.
        //
        // The env record outlives the spawning frame: codegen heap-boxes
        // it (`malloc(sizeof(env))` + `memcpy`) and passes the heap
        // pointer as `hew_lambda_actor_new`'s `state` arg. The body reads
        // captures back through the state pointer (`ClosureEnvFieldLoad`
        // on Local(0)); the synthesized `state_drop_fn` is the single
        // teardown owner (field drops, then `free`), called exactly once
        // by the runtime after the dispatch loop stops.
        let mut env_place: Option<Place> = None;
        let mut env_ty: Option<ResolvedTy> = None;
        let mut env_field_drops: Vec<crate::model::LambdaEnvFieldDrop> = Vec::new();
        let mut env_capture_field_tys: Vec<ResolvedTy> = Vec::new();
        let mut weak_capture_bindings: std::collections::HashSet<hew_hir::BindingId> =
            std::collections::HashSet::new();
        if !captures.is_empty() {
            let mut field_tys: Vec<ResolvedTy> = Vec::with_capacity(captures.len());
            let mut init_fields: Vec<(FieldOffset, Place)> = Vec::new();
            for (idx, capture) in captures.iter().enumerate() {
                let offset =
                    FieldOffset(u32::try_from(idx).expect("lambda capture count exceeds u32::MAX"));
                // `HirLambdaCapture` carries no type; the captured binding's
                // MIR slot type is the authority (for the weak self-capture
                // the slot is the handle local, typed `Duplex<Msg, Reply>`).
                let Some(capture_ty) = self
                    .binding_locals
                    .get(&capture.binding)
                    .and_then(|place| base_local(*place))
                    .and_then(|local| self.locals.get(local as usize))
                    .cloned()
                else {
                    // Already diagnosed by the backend-slot loop above.
                    return handle;
                };
                let drop_class = match (&capture.kind, &capture_ty) {
                    (hew_hir::HirCaptureKind::Weak, _) => {
                        weak_capture_bindings.insert(capture.binding);
                        crate::model::LambdaEnvFieldDrop::WeakSelfHandle
                    }
                    (hew_hir::HirCaptureKind::Strong, ResolvedTy::String) => {
                        crate::model::LambdaEnvFieldDrop::String
                    }
                    // BitCopy scalars and pids share the no-drop class. A pid
                    // is an opaque identity reference with no drop glue (its
                    // drop is a codegen no-op — see the double-free origin
                    // analysis on `validate_lambda_captures`): capturing one
                    // is a BitCopy alias, ownership-identical to passing it
                    // to a fn by value. The actor's lifetime is runtime-owned,
                    // so the env field needs no retain and no release; a send
                    // after the target stops is the same pre-existing hazard
                    // class as any post-stop pid use — `hew_actor_send`'s
                    // liveness contract treats a dead target as a normal
                    // non-fault outcome.
                    (
                        hew_hir::HirCaptureKind::Strong,
                        ResolvedTy::I64
                        | ResolvedTy::I32
                        | ResolvedTy::I16
                        | ResolvedTy::I8
                        | ResolvedTy::U64
                        | ResolvedTy::U32
                        | ResolvedTy::U16
                        | ResolvedTy::U8
                        | ResolvedTy::F64
                        | ResolvedTy::F32
                        | ResolvedTy::Bool
                        | ResolvedTy::Char
                        | ResolvedTy::Named {
                            builtin: Some(BuiltinType::LocalPid),
                            ..
                        },
                    ) => crate::model::LambdaEnvFieldDrop::None,
                    (hew_hir::HirCaptureKind::Strong, other) => {
                        self.diagnostics.push(MirDiagnostic {
                            kind: MirDiagnosticKind::CannotMaterializeClosureCapture {
                                binding: capture.binding,
                                name: capture.name.clone(),
                                site: expr.site,
                            },
                            note: format!(
                                "lambda-actor capture `{}` has type `{}`, which the \
                                 capture env cannot carry yet: only BitCopy scalars, \
                                 `string`, actor pids, and the weak self-handle have \
                                 an ownership protocol across the actor boundary. A \
                                 shallow byte copy of an owned aggregate would alias \
                                 its heap and double-free at shutdown — fail closed \
                                 instead.",
                                capture.name,
                                other.user_facing()
                            ),
                        });
                        return handle;
                    }
                };
                if !matches!(drop_class, crate::model::LambdaEnvFieldDrop::WeakSelfHandle) {
                    let Some(&src) = self.binding_locals.get(&capture.binding) else {
                        // Already diagnosed by the backend-slot loop above.
                        return handle;
                    };
                    init_fields.push((offset, src));
                }
                field_tys.push(capture_ty);
                env_field_drops.push(drop_class);
            }
            let env_name = format!("__hew_lambda_env_{owner}_{lambda_id}");
            let env_resolved_ty = ResolvedTy::Named {
                name: env_name.clone(),
                args: vec![],
                builtin: None,
                is_opaque: false,
            };
            env_capture_field_tys.clone_from(&field_tys);
            self.closure_record_layouts
                .push(crate::model::RecordLayout {
                    name: env_name,
                    field_tys,
                    // Compiler-internal lambda-env record: positional `-g` names.
                    field_names: Vec::new(),
                });
            let dest = self.alloc_local(env_resolved_ty.clone());
            self.push_instr(Instr::RecordInit {
                ty: env_resolved_ty.clone(),
                fields: init_fields,
                dest,
            });
            env_place = Some(dest);
            env_ty = Some(env_resolved_ty);
        }
        let state_drop_name = if env_place.is_some() {
            format!("__hew_lambda_env_drop_{owner}_{lambda_id}")
        } else {
            "__hew_lambda_state_drop_noop".to_string()
        };

        let shape = if matches!(reply_ty, ResolvedTy::Unit) {
            crate::model::LambdaActorShape::Tell
        } else {
            crate::model::LambdaActorShape::Ask
        };
        let shape_disc: i32 = match shape {
            crate::model::LambdaActorShape::Tell => 0,
            crate::model::LambdaActorShape::Ask => 1,
        };

        // ── Build child Builder for the body ──
        // Shares the parent's module tables (`child_builder_tables`) so a
        // lambda body that sends to a captured pid resolves the target
        // actor's `actor_method_info` exactly as the parent would.
        let mut body_builder = Builder {
            current_function_symbol: body_name.clone(),
            current_function_call_conv: crate::model::FunctionCallConv::LambdaActorBody(shape),
            ..self.child_builder_tables()
        };

        // Locals 0..=4: the five runtime ABI parameter slots. Their types
        // mirror `HewLambdaActorBody`'s C signature: (state ptr, msg ptr,
        // msg_len i64, reply_out ptr-of-ptr, reply_len_out ptr-of-usize).
        // The codegen parameter prologue stores LLVM args into these
        // allocas in order.
        let ptr_ty = ResolvedTy::Pointer {
            is_mutable: true,
            pointee: Box::new(ResolvedTy::Unit),
        };
        body_builder.locals.push(ptr_ty.clone()); // Local(0): state
        body_builder.locals.push(ptr_ty.clone()); // Local(1): msg ptr
        body_builder.locals.push(ResolvedTy::I64); // Local(2): msg_len
        body_builder.locals.push(ptr_ty.clone()); // Local(3): reply_out
        body_builder.locals.push(ptr_ty.clone()); // Local(4): reply_len_out

        // Allocate user-param locals AFTER the ABI slots and register them
        // in `binding_locals` so body HIR `BindingRef`s resolve. Track the
        // Local ids in the side-channel so codegen knows which slots need
        // the msg-deserialise prologue fragment.
        let mut user_param_local_ids: Vec<u32> = Vec::with_capacity(params.len());
        for param in params {
            let slot = body_builder.alloc_local(param.ty.clone());
            let Place::Local(slot_id) = slot else {
                unreachable!("alloc_local returns Place::Local");
            };
            body_builder.binding_locals.insert(param.id, slot);
            body_builder.seed_fn_param_provenance(param);
            user_param_local_ids.push(slot_id);
        }

        // Register each capture as a body-side env-field source: the body's
        // `BindingRef`s to a captured binding lower to `ClosureEnvFieldLoad`
        // through Local(0) — the runtime state pointer, which IS the boxed
        // env pointer when captures are present. Mirrors the closure-shim
        // env discipline at `lower_closure_shim`. Loads are read-only views
        // into the env; only the synthesized `state_drop_fn` frees env
        // fields (`ffi-ownership-contracts`).
        if let Some(env_resolved_ty) = &env_ty {
            for (idx, capture) in captures.iter().enumerate() {
                body_builder.capture_env_sources.insert(
                    capture.binding,
                    CaptureEnvSource {
                        env: Place::Local(0),
                        env_ty: env_resolved_ty.clone(),
                        field_offset: FieldOffset(
                            u32::try_from(idx).expect("lambda capture count exceeds u32::MAX"),
                        ),
                        ty: env_capture_field_tys[idx].clone(),
                    },
                );
                if self
                    .closure_pair_env_may_be_nonnull
                    .contains(&capture.binding)
                {
                    body_builder
                        .closure_pair_env_may_be_nonnull
                        .insert(capture.binding);
                }
                if self.closure_pair_null_env.contains(&capture.binding) {
                    body_builder.closure_pair_null_env.insert(capture.binding);
                }
            }
            body_builder.weak_lambda_capture_bindings = weak_capture_bindings;
        }

        // Lower the lambda body. The body is a single HirExpr (an arrow
        // body or block); reuse `lower_value` so all expression shapes —
        // BlockExpr, Call, BinaryOp, etc. — go through the standard path.
        //
        // #2301 -- same pre-pass gap as `lower_closure_shim`/`lower_gen_block`:
        // this lambda-actor body lowers via its own fresh `body_builder`
        // (built via `child_builder_tables`, which does not carry
        // `prepass_consumed_bindings`/`prepass_reassigned_bindings` either), so
        // without this call a `var` local to the lambda body that is consumed
        // on one control-flow arm and overwritten on a sibling arm silently
        // leaks its prior value instead of getting the path-sensitive release
        // a byte-identical top-level function body would.
        body_builder.collect_expr_prepass_facts(body);
        let tail_place = body_builder.lower_value(body);

        // Shape-driven return slot wiring:
        //   - Ask: move the body's tail value into ReturnSlot so codegen's
        //     LambdaActorBody Return epilogue can serialise it into the
        //     reply buffer.
        //   - Tell: discard the tail value; codegen returns i32 0
        //     unconditionally.
        if matches!(shape, crate::model::LambdaActorShape::Ask) {
            if let Some(src) = tail_place {
                body_builder.instructions.push(Instr::Move {
                    dest: Place::ReturnSlot,
                    src,
                });
            }
        }

        let body_blocks = body_builder.finalize_blocks(Terminator::Return);
        let body_locals = body_builder.locals.clone();
        let body_user_return_ty = if matches!(shape, crate::model::LambdaActorShape::Ask) {
            reply_ty.clone()
        } else {
            ResolvedTy::Unit
        };

        // The body's MIR `return_ty` carries the USER reply type (Ask) or
        // `Unit` (Tell). Codegen consults this to pick the reply
        // serialisation width; the LLVM return type is always `i32`
        // (the status code) — see codegen's LambdaActorBody arm.
        let mut raw = RawMirFunction {
            name: body_name.clone(),
            return_ty: body_user_return_ty.clone(),
            call_conv: crate::model::FunctionCallConv::LambdaActorBody(shape),
            params: vec![
                ptr_ty.clone(),  // state
                ptr_ty.clone(),  // msg ptr
                ResolvedTy::I64, // msg_len
                ptr_ty.clone(),  // reply_out
                ptr_ty.clone(),  // reply_len_out
            ],
            locals: body_locals.clone(),
            // Synthesised lambda-actor body (runtime ABI shape): no `-g` DIEs.
            local_names: Vec::new(),
            local_scopes: Vec::new(),
            local_decl_bytes: Vec::new(),
            scope_table: Vec::new(),
            blocks: body_blocks.clone(),
            decisions: body_builder.decisions.clone(),
            intrinsic_id: None,
            await_deadline_ns: body_builder.await_deadline_ns.clone(),
            suspend_kinds: body_builder.suspend_kinds.clone(),
            lambda_actor_user_param_locals: user_param_local_ids.clone(),
            span: None,
            instr_spans: ::std::collections::BTreeMap::new(),
            source_origin: SourceOrigin::Unknown,
        };

        let thir_statements: Vec<MirStatement> = body_blocks
            .iter()
            .flat_map(|b| b.statements.iter().cloned())
            .collect();
        let thir = ThirFunction {
            name: body_name.clone(),
            return_ty: body_user_return_ty.clone(),
            statements: thir_statements,
        };

        // Synthetic HirFn shell for `check_function` (mirrors `lower_gen_block`).
        let synthetic_fn = HirFn {
            id: hew_hir::ItemId(0),
            node: hew_hir::HirNodeId(0),
            name: body_name.clone(),
            type_params: Vec::new(),
            is_generator: false,
            intrinsic_id: None,
            // Seed the user-visible params into the synthetic HirFn so the
            // dataflow checker treats them as Live at body entry — without
            // this seed every `BindingRef` to a user param reads as
            // "uninitialised before use" (the body has no `let` for the
            // param; it arrives via the runtime ABI msg buffer). The MIR
            // user-param locals were inserted into `binding_locals` above,
            // so this seed lines up the HIR-binding-keyed checker with the
            // MIR-local-keyed value sources.
            params: params.clone(),
            return_ty: body_user_return_ty.clone(),
            body: hew_hir::HirBlock {
                node: hew_hir::HirNodeId(0),
                scope: hew_hir::ScopeId(0),
                statements: Vec::new(),
                tail: None,
                ty: body_user_return_ty.clone(),
                span: expr.span.clone(),
            },
            span: expr.span.clone(),
        };

        let dataflow_result = check_function(&body_builder, &raw.blocks, &synthetic_fn);
        let mut body_diagnostics: Vec<MirDiagnostic> = dataflow_result
            .checks
            .iter()
            .filter_map(check_to_diagnostic)
            .collect();
        body_diagnostics.append(&mut body_builder.diagnostics);
        collect_unknown_type_diagnostics(&synthetic_fn, &body_builder, &mut body_diagnostics);
        let string_derivation =
            finalize_string_ownership(&mut raw, &body_builder, &dataflow_result);
        let bytes_derivation = finalize_bytes_ownership(&mut raw, &body_builder, &dataflow_result);

        let cooperate_sites = dataflow::compute_cooperate_sites(&raw.blocks);
        let checked = CheckedMirFunction {
            name: body_name.clone(),
            return_ty: body_user_return_ty.clone(),
            blocks: raw.blocks.clone(),
            decisions: body_builder.decisions.clone(),
            checks: dataflow_result.checks.clone(),
            cooperate_sites,
        };
        let elaborated = elaborate(
            &checked,
            &body_builder,
            &thir.statements,
            &dataflow_result,
            Some(&string_derivation.allowed),
            Some(&bytes_derivation.allowed),
        );

        let body_lowered = LoweredFunction {
            thir,
            raw,
            checked,
            elaborated,
            diagnostics: body_diagnostics,
            generated: body_builder.generated_functions,
            record_layouts: body_builder.closure_record_layouts,
        };
        self.generated_functions.push(body_lowered);

        // Emit `Terminator::MakeLambdaActor` so codegen wires
        // `hew_lambda_actor_new(64, shape, &body_fn, state, &state_drop_fn)`
        // and stores the resulting `*mut HewLambdaActorHandle` into
        // `handle`. `state` is null for a capture-free lambda; with
        // captures it is the heap-boxed env codegen builds from `env`.
        // The 64-slot default mailbox is a substrate constant
        // (mirrors the cooperate-site default in `hew-runtime`); a future
        // surface knob (`actor capacity 128 |..| { ... }`) can override
        // this without touching codegen.
        let next = self.alloc_block();
        self.finish_current_block(Terminator::MakeLambdaActor {
            dest: handle,
            body_fn: body_name,
            state_drop_fn: state_drop_name,
            shape: shape_disc,
            mailbox_capacity: 64,
            next,
            env: env_place,
            env_field_drops,
        });
        self.start_block(next);

        handle
    }

    /// Lower a call-syntax dispatch through a lambda-actor handle:
    /// `let h = actor |...| { ... }; h(msg)`.
    ///
    /// The intercept in `HirExprKind::Call` routes here when the callee
    /// is a `BindingRef` whose MIR Place is `Place::LambdaActorHandle(N)`.
    /// This is the M2 substrate seam that replaces both the
    /// `module_fn_names` collision miscompile (a user binding named `log`
    /// shadowing the `f64 -> f64` math builtin) and the indirect-call NYI
    /// arm (`dbl(5)`, `fib(10)`) — see the comment at the call site.
    ///
    /// Shape dispatch (tell vs ask) is driven by the callee's HIR type
    /// `Duplex<Msg, Reply>`: `Reply = Unit` → tell (`hew_lambda_actor_send`),
    /// otherwise → ask (`hew_lambda_actor_ask`). Mirrors the runtime
    /// `LambdaShape::{Tell,Ask}` enum (`hew-runtime/src/lambda_actor.rs`).
    ///
    /// Scope: a single argument rides the single-vertebra wire (8-byte
    /// zero-padded scalar spill, or the aggregate `(ptr, sizeof)` path
    /// codegen selects for struct-typed messages — mirrors the
    /// `hew_duplex_send` shape); two or more arguments pack into the
    /// same anonymous-record payload declared-actor multi-arg sends use
    /// (`lower_packed_args_payload`). Body-side self-sends through the
    /// captured weak handle dispatch via `hew_lambda_actor_weak_send`;
    /// a weak self-ask fails closed (no weak-ask runtime entry).
    /// LESSONS: substrate-over-surface, boundary-fail-closed.
    #[allow(
        clippy::too_many_lines,
        reason = "tell/ask × strong/weak-capture dispatch keeps the wire selection and \
                  its fail-closed arms in one body; splitting would scatter the single \
                  branch authority over the payload mechanism"
    )]
    fn lower_lambda_actor_call(
        &mut self,
        callee: &HirExpr,
        args: &[HirExpr],
        expr_ty: &ResolvedTy,
        site: hew_hir::SiteId,
    ) -> Option<Place> {
        // Weak-capture dispatch: a body-side self-send through the captured
        // weak handle uses the weak ABI (`hew_lambda_actor_weak_send`). The
        // discriminator is the callee binding's membership in the body
        // builder's weak-capture set — the env field holds a
        // `*mut HewLambdaActorWeakHandle`, which the strong-send entry must
        // never receive (distinct repr; the runtime upgrade lives behind
        // the weak entry).
        let via_weak_capture = matches!(
            &callee.kind,
            HirExprKind::BindingRef {
                resolved: ResolvedRef::Binding(binding_id),
                ..
            } if self.weak_lambda_capture_bindings.contains(binding_id)
        );
        let handle = self.lower_value(callee)?;
        let handle_is_env_load = matches!(
            &callee.kind,
            HirExprKind::BindingRef {
                resolved: ResolvedRef::Binding(binding_id),
                ..
            } if self.capture_env_sources.contains_key(binding_id)
        );
        if !matches!(handle, Place::LambdaActorHandle(_)) && !handle_is_env_load {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::UnsupportedNode {
                    reason: format!(
                        "lambda-actor call routed to non-LambdaActorHandle place {handle:?}"
                    ),
                },
                note: "lower_lambda_actor_call expects the callee binding's Place to be \
                       Place::LambdaActorHandle or a captured-env Duplex field load; the \
                       gate in HirExprKind::Call must have drifted from the place_aware \
                       lookup"
                    .to_string(),
            });
            return None;
        }

        if args.is_empty() {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "lambda-actor call with arity 0".to_string(),
                    site,
                },
                note: "a zero-param lambda actor is dispatched with a unit message \
                       (`handle(())`); bare zero-argument dispatch is a follow-on slice"
                    .to_string(),
            });
            return None;
        }

        let reply_ty = match &callee.ty {
            ResolvedTy::Named { args: ty_args, .. } if ty_args.len() == 2 => ty_args[1].clone(),
            other => {
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::UnsupportedNode {
                        reason: format!("lambda-actor callee has non-Duplex<S,R> type `{other:?}`"),
                    },
                    note: "expected `Duplex<Msg, Reply>` for lambda-actor dispatch".to_string(),
                });
                return None;
            }
        };
        let is_ask = !matches!(reply_ty, ResolvedTy::Unit);

        // Payload mechanism convergence: a single argument rides the existing
        // single-vertebra wire (8-byte zero-padded spill for scalars, or the
        // aggregate (ptr, sizeof) path codegen selects for struct-typed
        // messages); two or more arguments pack into the same anonymous-record
        // payload declared-actor multi-arg sends use.
        let msg_place = if args.len() == 1 {
            self.lower_value(&args[0])?
        } else {
            self.lower_packed_args_payload(args, site)?
        };

        // Wire byte-length operand for the SCALAR single-vertebra wire only:
        // the fixed 8-byte zero-padded slot (mirrors hew_duplex_send). MIR has
        // no sizeof expression, so for an aggregate payload (the packed-args
        // record, or a struct-typed single message) codegen is the single size
        // authority: it branches on the message Place's LLVM type and derives
        // (ptr, sizeof(aggregate)) itself, never consuming this operand on
        // that path. There is exactly one branch predicate (codegen's
        // struct-type check), so the two ends cannot drift.
        let len_local = self.alloc_local(ResolvedTy::I64);
        self.push_instr(Instr::ConstI64 {
            dest: len_local,
            value: 8,
        });

        // The call's HIR type is `Result<Reply, SendError>` (tell) or
        // `Result<Reply, AskError>` (ask). Allocate the dest so let-
        // binding consumers see a Place; the slot is left uninitialised
        // in this MVP (the LLVM verifier accepts uninit allocas; `hew
        // run` of a real result-bearing call needs a follow-on slice
        // that materialises Ok(_) from the runtime i32 status).
        let dest = self.alloc_local(expr_ty.clone());

        // A self-ask through the weak capture has no weak-ask runtime entry
        // (`hew_lambda_actor_weak_send` is tell-only); refuse rather than
        // upgrade-and-strong-ask behind the caller's back, which would keep
        // the actor alive past external refcount zero for the ask duration.
        if via_weak_capture && is_ask {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "recursive self-ask through a lambda-actor weak capture".to_string(),
                    site,
                },
                note: "the weak self-handle supports fire-and-forget self-sends \
                       (`hew_lambda_actor_weak_send`); an ask-shaped self-dispatch \
                       needs a weak-ask runtime entry with upgrade semantics"
                    .to_string(),
            });
            return None;
        }

        let call = if is_ask {
            // Out-params: reply_out is `*mut *mut u8` (pointer-sized
            // slot), reply_len_out is `*mut usize`. We allocate a Pointer
            // local for reply_out (alloca holds a ptr value, 8 bytes on
            // 64-bit) and an I64 local for reply_len_out (alloca holds an
            // i64). Codegen passes the alloca addresses as the first 5
            // args to the runtime call. The 6th arg is codegen-only:
            // an `AskError` local that the codegen branches into on the
            // call's non-zero (Err) status — the AskError tag is stored
            // here and `emit_result_err(dest, error_dest)` materialises
            // `Result::Err(askerror)` without the unconditional null
            // reply-pointer deref the previous codegen did. Mirrors the
            // `SuspendingAsk` precedent at `lower.rs:15340`.
            // After the ask completes:
            //   - On Ok (status 0): codegen loads the reply bytes into
            //     `dest`'s Result::Ok variant and frees the libc-
            //     allocated reply buffer with `hew_reply_payload_free`.
            //   - On Err (status != 0): codegen maps the SendError
            //     discriminant to an AskError variant, stores into
            //     `error_dest`'s MachineTag slot, and binds
            //     `Result::Err(error_dest)` into `dest`.
            // Marking the runtime call's `dest` as `Some(dest)` keeps
            // the dataflow ledger consistent (the dest binding becomes
            // Live at the call), so the LIFO drop plan releases it on
            // scope exit if it carries owned-handle resources.
            let reply_ptr_slot = self.alloc_local(ResolvedTy::Pointer {
                is_mutable: true,
                pointee: Box::new(ResolvedTy::Unit),
            });
            let reply_len_slot = self.alloc_local(ResolvedTy::I64);
            let error_dest = self.alloc_local(ResolvedTy::Named {
                name: "AskError".to_string(),
                args: Vec::new(),
                builtin: Some(BuiltinType::AskError),
                is_opaque: false,
            });
            crate::model::RuntimeCall::new(
                "hew_lambda_actor_ask",
                vec![
                    handle,
                    msg_place,
                    len_local,
                    reply_ptr_slot,
                    reply_len_slot,
                    error_dest,
                ],
                Some(dest),
            )
        } else {
            crate::model::RuntimeCall::new(
                if via_weak_capture {
                    "hew_lambda_actor_weak_send"
                } else {
                    "hew_lambda_actor_send"
                },
                vec![handle, msg_place, len_local],
                None,
            )
        }
        .expect("hew_lambda_actor_{send,weak_send,ask} are on the M2 runtime allowlist");

        self.push_instr(Instr::CallRuntimeAbi(call));
        Some(dest)
    }

    /// Lower `HirExprKind::GenBlock { body, yield_ty, return_ty, captures }`
    /// to a MIR generator shell.
    ///
    /// The enclosing function receives a `Place::Local` typed as
    /// `Generator<Y, R>` — this is a placeholder for S3b, which synthesises
    /// the state-record struct and fills in cross-yield live fields.
    ///
    /// The gen-block body is lowered into a separate synthetic function
    /// (name: `__hew_gen_body_{owner}_{id}`) and registered in
    /// `self.generated_functions` so `lower_hir_module` surfaces it in the
    /// `IrPipeline`. Inside the body, `HirExprKind::Yield { value }` lowers
    /// to `Terminator::Yield { value: <place>, next: <resume_block> }`.
    ///
    /// # S3b cross-yield liveness stub
    /// This function does NOT compute which locals are live across yield sites.
    /// S3b adds the cross-yield liveness pass that lifts live locals to
    /// state-record fields. The generated body's blocks contain the correct
    /// CFG shape (yield terminators, resume blocks) for S3b to consume without
    /// any re-lowering.
    ///
    /// # Fail-closed invariant
    /// If the HIR `yield_ty` or `return_ty` is unreachable in the current
    /// checker state, the lowering succeeds structurally (the placeholder
    /// place still has type `Generator<Y, R>`) and S3b/S4 detect the
    /// inconsistency when they interrogate the state-record.
    #[allow(
        clippy::too_many_lines,
        reason = "gen-block lowering keeps body-builder construction, S3b synthesis-pass \
                  invocation, raw/checked/elaborated triple assembly, and dataflow \
                  dispatch in one routine so the surface a regression touches is \
                  contiguous; helper-splitting would scatter the gen-body invariants \
                  across functions that each only see half of the contract"
    )]
    fn lower_gen_block(
        &mut self,
        expr: &HirExpr,
        body: &HirBlock,
        _yield_ty: &ResolvedTy,
        return_ty: &ResolvedTy,
        captures: &[hew_hir::HirGenCapture],
    ) -> Place {
        // Mint a unique generator-body function name via the shared closure
        // id counter so multiple gen blocks in one function do not collide.
        let gen_id = self.next_closure_id;
        self.next_closure_id = self
            .next_closure_id
            .checked_add(1)
            .expect("generator id overflow — closure id counter exhausted");
        let owner = Self::sanitize_symbol_component(&self.current_function_symbol);
        let body_name = format!("__hew_gen_body_{owner}_{gen_id}");

        // Allocate a place in the ENCLOSING function typed as
        // `Generator<yield_ty, return_ty>`.  S3b will replace this with the
        // real state-record type; for S3a it is purely a checker-authority
        // token so the binding in the enclosing scope has the right type.
        let gen_place = self.alloc_local(expr.ty.clone());

        // ── Capture-env synthesis (mirrors `lower_spawn_lambda_actor`) ──
        //
        // The generator body is a coro ramp (`__hew_gen_body_*`); its only
        // window onto the enclosing frame is the env record
        // `Terminator::MakeGenerator` heap-copies at construction (so it
        // outlives this constructing frame — the body reads it across
        // suspends). Each free variable — a `gen fn`'s formal parameters, a
        // `gen { }` block's captured outer locals (HIR computed this set, in
        // `captures`) — becomes one field of a synthetic env record built HERE
        // in the enclosing frame, in capture order. The body reads them back
        // through `Local(1)` (the env-pointer param; `Local(0)` is the
        // out-pointer) via `ClosureEnvFieldLoad` (registered below).
        //
        // SCOPE / FAIL-CLOSED: `gen_env_capture_admissible` governs what may
        // be admitted to the flat-copied env. `Terminator::MakeGenerator`
        // copies the env's bytes flat once, at construction, and its release
        // (`hew_gen_coro_destroy`, `hew-runtime/src/cont.rs`) frees only that
        // flat buffer. Admitted shapes:
        //   * `BitCopy` scalars transitively free of `#[opaque]` handles.
        //   * `ResolvedTy::Function` — a bare named-fn reference whose runtime
        //     env word is null; safe to flat-copy.
        //   * `ResolvedTy::Closure` with no captures — same null-env guarantee.
        // Rejected shapes (fail closed):
        //   * owned / non-`BitCopy` (string/Vec/record): flat-copying
        //     shallow-aliases the caller's heap → double-free / UAF; and
        //   * `#[opaque]`-only handles: classifies as `BitCopy` (pointer-width,
        //     no implicit drop) yet is a runtime handle — bit-copying it into
        //     the env aliases the caller's handle → the same UAF; and
        //   * `Closure` with non-empty `captures`: heap-boxed env — same
        //     shallow-alias hazard. Admitting it takes the `genfn-owned-captures`
        //     clone-into-env + env-field-drop-on-destroy protocol.
        let mut env_place: Option<Place> = None;
        let mut env_ty: Option<ResolvedTy> = None;
        let mut env_capture_field_tys: Vec<ResolvedTy> = Vec::new();
        // Capture bindings rejected below as inadmissible to the flat env. Each
        // gets a root `NotYetImplemented`; the body sub-builder reads this set
        // to suppress the downstream `InitialisedBeforeUse`/`UnresolvedPlace`
        // cascade for the same bindings (only the root rejection is actionable).
        let mut poisoned_captures: HashSet<BindingId> = HashSet::new();
        if !captures.is_empty() {
            let mut field_tys: Vec<ResolvedTy> = Vec::with_capacity(captures.len());
            let mut init_fields: Vec<(FieldOffset, Place)> = Vec::new();
            let mut all_materialisable = true;
            for (idx, capture) in captures.iter().enumerate() {
                let offset =
                    FieldOffset(u32::try_from(idx).expect("gen capture count exceeds u32::MAX"));
                // The captured binding's MIR slot in the ENCLOSING frame is the
                // authority for both the value source and the field type. A
                // `Local` capture (a `gen fn`'s own params, a `gen {}`
                // block's outer locals, or a `receive gen fn` handler param)
                // already has a slot in `binding_locals`. An `ActorStateField`
                // capture has none — actor state fields resolve by NAME
                // through `current_actor_state_fields`, not by binding id —
                // so its value is snapshotted into a fresh shell local via
                // `ActorStateFieldLoad` first (state is snapshot-isolated: a
                // point-in-time copy taken now, in the enclosing actor-handler frame where state
                // is addressable, never a live reference); the resulting slot
                // then feeds `RecordInit` exactly like a `Local` capture's.
                let (slot, capture_ty) = match capture.source {
                    hew_hir::HirGenCaptureSource::Local => {
                        let slot = self.binding_locals.get(&capture.binding).copied();
                        let ty = slot
                            .and_then(base_local)
                            .and_then(|local| self.locals.get(local as usize))
                            .cloned();
                        (slot, ty)
                    }
                    hew_hir::HirGenCaptureSource::ActorStateField => {
                        match self.current_actor_state_fields.get(&capture.name).cloned() {
                            Some((field_offset, ty)) => {
                                let dest = self.alloc_local(ty.clone());
                                // P0 #2432 — fail-closed default; this snapshot is a
                                // RecordInit field a moment later (whole-value escape into
                                // the gen-body's captured env), so the classifier keeps it
                                // `Owned` regardless — recorded explicitly for consistency
                                // with the other two construct sites.
                                self.instructions.push(Instr::ActorStateFieldLoad {
                                    field_offset,
                                    dest,
                                    mode: ActorStateLoadMode::Owned,
                                });
                                (Some(dest), Some(ty))
                            }
                            None => (None, None),
                        }
                    }
                };
                let fn_env_provenance_unproven = matches!(capture_ty, Some(ResolvedTy::Function { .. }))
                    && self
                        .closure_pair_env_may_be_nonnull
                        .contains(&capture.binding)
                    // A source `gen fn` shell's own formal parameters are
                    // checked at every generator-constructor call. No other
                    // anonymous-gen capture has that external provenance gate.
                    && !(self.generator_shell_call_gate.is_some()
                        && self.closure_pair_param_owned.contains(&capture.binding));
                match (slot, capture_ty) {
                    (Some(src), Some(ty))
                        if self.gen_env_capture_admissible(&ty) && !fn_env_provenance_unproven =>
                    {
                        init_fields.push((offset, src));
                        field_tys.push(ty);
                    }
                    (Some(_), Some(ty)) => {
                        // Not admissible to the flat-`memcpy`'d generator env.
                        // Four fail-closed shapes reach this arm; name the reason
                        // precisely so the diagnostic is actionable:
                        //   * `Function` with unproven env provenance — an
                        //     anonymous gen block captured a parameter/call
                        //     result/aggregate read without a direct gen-fn call
                        //     boundary proving the pair's env word null; OR
                        //   * `Closure` with non-empty `captures` — a heap-boxed
                        //     env; flat-copying it shallow-aliases the caller's env
                        //     → double-free / UAF at generator teardown (admitting
                        //     it takes a clone-into-env protocol); OR
                        //   * owned / non-`BitCopy` (string/Vec/owned record) —
                        //     no clone-into-env protocol exists yet; OR
                        //   * `BitCopy`-but-opaque — an `#[opaque]` runtime
                        //     handle (or an aggregate transitively containing
                        //     one) classifies as `BitCopy` yet aliases a runtime
                        //     resource.
                        // `Terminator::MakeGenerator` deep-copies the env's flat
                        // bytes once, at construction, so shallow-copying any of
                        // these would alias the caller's resource → double-free /
                        // UAF at generator teardown.
                        let reason = if fn_env_provenance_unproven {
                            "its fn value may carry a heap closure environment, and this \
                             anonymous generator construction has no call-boundary proof \
                             that the env word is null"
                        } else {
                            match &ty {
                                ResolvedTy::Closure { captures, .. } if !captures.is_empty() => {
                                    "a closure with a captured environment cannot yet be admitted \
                                 into the generator's flat-copied env; its heap env would be \
                                 shallow-aliased (double-free / UAF at teardown). \
                                 Owned/closure-env captures need the clone-into-env + \
                                 env-field-drop-on-destroy protocol"
                                }
                                _ if ValueClass::of_ty(&ty, &self.type_classes)
                                    == ValueClass::BitCopy =>
                                {
                                    "it transitively contains an `#[opaque]` runtime handle; an \
                                 opaque handle is a pointer-width value with no clone helper, \
                                 so flat-copying it into the generator's env would alias the \
                                 caller's handle and double-free / use-after-free at teardown"
                                }
                                _ => {
                                    "it is an owned / non-BitCopy value; the generator's env is a \
                                 flat heap copy taken once at construction and can carry only \
                                 plain copyable values"
                                }
                            }
                        };
                        let construct = if fn_env_provenance_unproven {
                            format!(
                                "the capture of fn value `{}` with unproven env provenance \
                                 into a generator",
                                capture.name
                            )
                        } else {
                            format!(
                                "the capture of opaque/owned value `{}` into a generator",
                                capture.name
                            )
                        };
                        self.diagnostics.push(MirDiagnostic {
                            kind: MirDiagnosticKind::NotYetImplemented {
                                construct,
                                site: expr.site,
                            },
                            note: format!(
                                "cannot capture `{}` (type `{}`) into a generator: {reason}. \
                                 Only plain copyable values and null-env fn references may be \
                                 admitted into the generator's flat-copied env. DROP-TODO: a \
                                 per-field clone-into-env + env-field-drop-on-destroy protocol \
                                 (mirroring the lambda `state_drop_fn`) would admit \
                                 owned/closure-env captures safely.",
                                capture.name,
                                ty.user_facing()
                            ),
                        });
                        // Poison this capture's binding id: the root
                        // `NotYetImplemented` above is the one actionable
                        // diagnostic. The body sub-builder's `BindingRef`
                        // resolution consults `poisoned_capture_ids` and stays
                        // silent, suppressing the `InitialisedBeforeUse` +
                        // `UnresolvedPlace` cascade for this binding.
                        poisoned_captures.insert(capture.binding);
                        all_materialisable = false;
                    }
                    _ => {
                        // No backend slot for the capture in the enclosing
                        // frame: the body-side read fail-closes at
                        // `UnresolvedPlace` with the canonical note. Do not
                        // fabricate an env field.
                        all_materialisable = false;
                    }
                }
            }
            // Env-record synthesis is all-or-nothing (see `all_materialisable`
            // below): if ANY capture is inadmissible, the whole generator
            // construction fails closed with a root `NotYetImplemented`, and
            // NO env record is built at all — not even for the otherwise-
            // admissible captures collected into `init_fields`/`field_tys`
            // above. Their would-be `capture_env_sources` entries are
            // therefore never registered either (that registration loop below
            // only runs when `env_ty.is_some()`).
            //
            // Without this poison-everything step, an admissible capture in
            // that same abandoned set would be neither in
            // `poisoned_captures` (only the explicitly-rejected binding was
            // added above) nor in `capture_env_sources` (no env exists to
            // source it from) — so its body-side `BindingRef` falls through
            // to the ordinary unresolved-binding path and cascades a spurious
            // `InitialisedBeforeUse` + `UnresolvedPlace` on top of the one
            // actionable root diagnostic. Poison every capture in the
            // abandoned set so the body stays silent for all of them: the
            // root rejection above is already the single actionable
            // diagnostic for the whole construction.
            if !all_materialisable {
                poisoned_captures.extend(captures.iter().map(|capture| capture.binding));
            }
            if all_materialisable {
                let env_name = format!("__hew_gen_env_{owner}_{gen_id}");
                let env_resolved_ty = ResolvedTy::Named {
                    name: env_name.clone(),
                    args: vec![],
                    builtin: None,
                    is_opaque: false,
                };
                env_capture_field_tys.clone_from(&field_tys);
                self.closure_record_layouts
                    .push(crate::model::RecordLayout {
                        name: env_name,
                        field_tys,
                        // Compiler-internal generator-env record: positional names.
                        field_names: Vec::new(),
                    });
                let dest = self.alloc_local(env_resolved_ty.clone());
                self.instructions.push(Instr::RecordInit {
                    ty: env_resolved_ty.clone(),
                    fields: init_fields,
                    dest,
                });
                env_place = Some(dest);
                env_ty = Some(env_resolved_ty);
            }
        }

        // Build a child Builder that lowers the gen-block body.
        // `in_gen_body: true` enables `HirExprKind::Yield` → `Terminator::Yield`
        // construction inside the body.
        let mut body_builder = Builder {
            type_classes: self.type_classes.clone(),
            record_field_orders: self.record_field_orders.clone(),
            machine_layout_names: self.machine_layout_names.clone(),
            module_fn_names: self.module_fn_names.clone(),
            module_generic_fn_names: self.module_generic_fn_names.clone(),
            funcupdate_fn_returns_fresh: self.funcupdate_fn_returns_fresh.clone(),
            param_ownership: self.param_ownership.clone(),
            subst: self.subst.clone(),
            call_site_type_args: self.call_site_type_args.clone(),
            supervisor_child_slots: self.supervisor_child_slots.clone(),
            actor_send_aliasing: self.actor_send_aliasing.clone(),
            pointer_width: self.pointer_width,
            current_function_symbol: body_name.clone(),
            current_function_call_conv: crate::model::FunctionCallConv::Default,
            task_entry_adapter_symbols: self.task_entry_adapter_symbols.clone(),
            in_gen_body: true,
            // #2648 — the generator body is USER code: the preflight needs the
            // module provenance context or a `match wrap(x)` inside a gen body
            // silently takes the unknown-item legacy fail-open mint (the same
            // closure-shim double-free class). Local-freshness facts stay the
            // fail-closed empty default — see `child_builder_tables`.
            call_scrutinee_provenance: self.call_scrutinee_provenance.clone(),
            ..Builder::default()
        };
        // Propagate the inadmissible-capture poison set into the body builder so
        // its `BindingRef` resolution stays silent for those bindings — the root
        // rejection already fired in the enclosing frame, and the un-materialised
        // capture would otherwise cascade into `InitialisedBeforeUse` +
        // `UnresolvedPlace`.
        body_builder.poisoned_capture_ids = poisoned_captures;

        // Prepend the coroutine-ramp parameters. The generator body is lowered
        // as an `llvm.coro.*` switched-resume coroutine ramp (the same substrate
        // the await-family uses, `hew-codegen-rs/src/coro.rs`); its leading
        // formal parameters are:
        //   Local(0) — `out_ptr: *mut Y`. The value channel. Before every
        //              `yield` the codegen `Terminator::Yield` arm stores the
        //              yielded value to `*out_ptr`; the consumer
        //              (`Instr::GeneratorNext`) reads it back from the same slot
        //              after each resume. This is the explicit out-pointer the
        //              `HewCont` substrate threads through the frame (cont.rs),
        //              NOT the forbidden non-null `coro.id` promise.
        //   Local(1) — `env_ptr: *const Env` (ONLY when the body has captures).
        //              The capture env record; the body reads each captured free
        //              variable through it via `ClosureEnvFieldLoad`. A
        //              capture-free generator has no env param — its single
        //              leading param is `out_ptr`.
        // Subsequent user-statement local allocations naturally start after the
        // leading params because `alloc_local` indexes from `locals.len()`.
        let gen_ptr_ty = ResolvedTy::Pointer {
            is_mutable: true,
            pointee: Box::new(ResolvedTy::Unit),
        };
        // Local(0): the out-pointer (always present).
        body_builder.locals.push(gen_ptr_ty.clone());
        let has_env = env_ty.is_some();
        if has_env {
            // Local(1): the capture-env pointer (only when captures materialised).
            body_builder.locals.push(gen_ptr_ty.clone());
        }

        // Register each materialised capture as a body-side env-field source:
        // the body's `BindingRef`s to a captured binding (a `gen fn` param or a
        // `gen { }` captured outer local) lower to `ClosureEnvFieldLoad` through
        // `Local(1)` — the env pointer (out_ptr occupies Local(0)). Mirrors the
        // lambda-actor-body env discipline: loads are read-only views into the
        // env. The coro frame is single-owner (no body thread), so the env copy
        // lifetime is bounded by the frame, not a runtime thread.
        if let Some(env_resolved_ty) = &env_ty {
            for (idx, capture) in captures.iter().enumerate() {
                body_builder.capture_env_sources.insert(
                    capture.binding,
                    CaptureEnvSource {
                        env: Place::Local(1),
                        env_ty: env_resolved_ty.clone(),
                        field_offset: FieldOffset(
                            u32::try_from(idx).expect("gen capture count exceeds u32::MAX"),
                        ),
                        ty: env_capture_field_tys[idx].clone(),
                    },
                );
            }
        }

        // #2301 -- same pre-pass gap as `lower_closure_shim`: this gen body
        // lowers via its own fresh `body_builder` (built by field list above,
        // not `child_builder_tables`), so without this call
        // `prepass_consumed_bindings`/`prepass_reassigned_bindings` stay empty
        // for every binding local to the `gen fn`/`gen {}` body and a `var`
        // consumed on one control-flow arm and overwritten on a sibling arm
        // silently leaks its prior value instead of getting the path-sensitive
        // release a byte-identical top-level function body would.
        body_builder.collect_prepass_facts(body);

        // Lower all statements in the gen-block body. Yields inside the body
        // call `lower_yield_expr` which emits `Terminator::Yield` and advances
        // the cursor to a fresh resume block.
        body_builder.active_scopes.push(body.scope);
        for stmt in &body.statements {
            body_builder.stmt(stmt);
        }
        // Lower the tail expression (the implicit return value of the block).
        if let Some(tail) = &body.tail {
            if let Some(src) = body_builder.lower_value(tail) {
                body_builder.instructions.push(Instr::Move {
                    dest: Place::ReturnSlot,
                    src,
                });
            }
        }
        body_builder.emit_pending_defers(body.scope);
        body_builder.active_scopes.pop();

        // Seal the last block with `Terminator::Return`. For a gen body
        // this represents the generator completing (returns `return_ty` which
        // S5 maps to `None` on the Iterator impl side).
        let mut blocks = body_builder.finalize_blocks(Terminator::Return);

        // W5.011 P3 — release nested fresh-`string` temporaries (f-string
        // interpolation's `to_string_*`/`string_concat` chain, `(a + b).len()`,
        // `s.to_uppercase().len()`, discarded `a + b;`) that `derive_cow_fresh_
        // borrowed_owner` (binding-scoped) cannot see. `lower_function`'s
        // ordinary-fn path runs this splice unconditionally right after
        // `finalize_blocks` (see its own call site); this gen-body ramp builds
        // its `RawMirFunction` through its own hand-rolled pipeline below
        // instead of going through `lower_function`, so it needs the identical
        // call — omitting it left every fresh-string temp inside a generator
        // body (most visibly an f-string per `yield`) leaking unconditionally,
        // independent of the `string_concat` catalog-name contract fix, since
        // this pass never ran on gen-body blocks at all. Must run BEFORE
        // `check_function`/`elaborate` below so the dataflow observes each
        // inline drop as a read of its temp and codegen emits the release.
        apply_nested_fresh_string_temp_drops(
            &mut blocks,
            &body_builder.suspend_kinds,
            &body_builder.locals,
            &body_builder.binding_locals,
            &mut body_builder.instr_spans,
        );
        // #2542 — the gen-body ramp needs the identical bytes user-call-result
        // temp splice as `lower_function` (a `mk().len()` inside a `yield`
        // expression would otherwise leak per iteration). Must run BEFORE
        // `check_function`/elaborate below, same as the string splice.
        apply_nested_fresh_bytes_temp_drops(
            &mut blocks,
            &body_builder.suspend_kinds,
            &body_builder.locals,
            &body_builder.binding_locals,
            &mut body_builder.instr_spans,
        );

        // Cross-suspend state is owned by LLVM's CoroSplit. The generator body
        // lowers to an `llvm.coro.*` switched-resume coroutine; CoroSplit
        // automatically spills every local live across a `coro.suspend` into the
        // single heap frame and re-derives the drop manifest from the `cleanup`
        // outline. There is therefore NO separate cross-yield-liveness /
        // state-record synthesis pass: the prior `gen_state::synthesise` machinery
        // (the spike-era stand-in for the never-landed hand-rolled state machine)
        // is subsumed by the coro frame and removed (RC14).
        let body_locals_with_state = body_builder.locals.clone();

        // Build the THIR/raw/checked/elaborated triple for the body function.
        let thir_stmts: Vec<MirStatement> = blocks
            .iter()
            .flat_map(|b| b.statements.iter().cloned())
            .collect();
        let thir = ThirFunction {
            name: body_name.clone(),
            return_ty: return_ty.clone(),
            statements: thir_stmts,
        };

        // The gen-body coroutine ramp's formal parameters mirror the leading
        // locals prepended above: `params[0]` is the out-pointer (`*mut Y`, the
        // value channel) and — only when the body has captures — `params[1]` is
        // the capture-env pointer. The `Terminator::Yield` codegen arm loads
        // `Local(0)` to store the yielded value before each `coro.suspend`; the
        // body reads env fields through `Local(1)` via `ClosureEnvFieldLoad`.
        //
        // WHY FunctionCallConv::Default: the body IS a coroutine ramp, detected
        // by codegen's `is_coroutine`/`has_suspend` carrier scan (it carries
        // `Terminator::Yield`), which overrides the LLVM return type to the
        // `coro.begin` handle (`ptr`) regardless of the call conv. No dedicated
        // call conv is needed; the carrier presence is the signal.
        let mut gen_body_params = vec![ResolvedTy::Pointer {
            is_mutable: true,
            pointee: Box::new(ResolvedTy::Unit),
        }];
        if has_env {
            gen_body_params.push(ResolvedTy::Pointer {
                is_mutable: true,
                pointee: Box::new(ResolvedTy::Unit),
            });
        }
        let mut raw = RawMirFunction {
            name: body_name.clone(),
            return_ty: return_ty.clone(),
            call_conv: crate::model::FunctionCallConv::Default,
            params: gen_body_params,
            locals: body_locals_with_state.clone(),
            // Synthesised generator body: no faithful user bindings, no DIEs.
            local_names: Vec::new(),
            local_scopes: Vec::new(),
            local_decl_bytes: Vec::new(),
            scope_table: Vec::new(),
            blocks: blocks.clone(),
            decisions: body_builder.decisions.clone(),
            intrinsic_id: None,
            await_deadline_ns: body_builder.await_deadline_ns.clone(),
            suspend_kinds: body_builder.suspend_kinds.clone(),
            lambda_actor_user_param_locals: Vec::new(),
            span: None,
            instr_spans: ::std::collections::BTreeMap::new(),
            source_origin: SourceOrigin::Unknown,
        };

        // A synthetic HirFn shell so `check_function` has a valid fn descriptor.
        // The body is empty — dataflow runs on the raw blocks directly.
        let synthetic_fn = HirFn {
            id: hew_hir::ItemId(0),
            node: hew_hir::HirNodeId(0),
            name: body_name.clone(),
            type_params: Vec::new(),
            is_generator: false,
            intrinsic_id: None,
            params: Vec::new(),
            return_ty: return_ty.clone(),
            body: hew_hir::HirBlock {
                node: hew_hir::HirNodeId(0),
                scope: hew_hir::ScopeId(0),
                statements: Vec::new(),
                tail: None,
                ty: return_ty.clone(),
                span: expr.span.clone(),
            },
            span: expr.span.clone(),
        };

        let dataflow_result = check_function(&body_builder, &raw.blocks, &synthetic_fn);
        let mut body_diagnostics: Vec<MirDiagnostic> = dataflow_result
            .checks
            .iter()
            .filter_map(check_to_diagnostic)
            .collect();
        body_diagnostics.append(&mut body_builder.diagnostics);
        collect_unknown_type_diagnostics(&synthetic_fn, &body_builder, &mut body_diagnostics);
        let string_derivation =
            finalize_string_ownership(&mut raw, &body_builder, &dataflow_result);
        let bytes_derivation = finalize_bytes_ownership(&mut raw, &body_builder, &dataflow_result);

        let cooperate_sites = dataflow::compute_cooperate_sites(&raw.blocks);
        let checked = CheckedMirFunction {
            name: body_name.clone(),
            return_ty: return_ty.clone(),
            blocks: raw.blocks.clone(),
            decisions: body_builder.decisions.clone(),
            checks: dataflow_result.checks.clone(),
            cooperate_sites,
        };
        let elaborated = elaborate(
            &checked,
            &body_builder,
            &thir.statements,
            &dataflow_result,
            Some(&string_derivation.allowed),
            Some(&bytes_derivation.allowed),
        );

        let body_lowered = LoweredFunction {
            thir,
            raw,
            checked,
            elaborated,
            diagnostics: body_diagnostics,
            generated: body_builder.generated_functions,
            record_layouts: body_builder.closure_record_layouts,
            // The state-record layout this body owns sits on the
        };
        self.generated_functions.push(body_lowered);

        // Materialize the generator value at the construction site: emit
        // `Terminator::MakeGenerator`. When the body captures BitCopy free
        // variables, `env` is the freshly-built env record place in this
        // (enclosing) frame: codegen heap-copies it (`hew_cont_frame_alloc` +
        // `memcpy`) and passes the copy's address to the `__hew_gen_body_*`
        // coro ramp. When there are no captures `env` is `None` and codegen
        // passes a null env to the ramp. The heap companion pointer (holding
        // the ramp's returned `llvm.coro.begin` handle) is stored into
        // `gen_place`; the gen-block expression evaluates to that place.
        let next = self.alloc_block();
        self.finish_current_block(Terminator::MakeGenerator {
            dest: gen_place,
            body_fn: body_name.clone(),
            next,
            env: env_place,
        });
        self.start_block(next);
        gen_place
    }

    /// Reshape a `receive gen fn` handler shell into a stream-producer PUMP
    /// (decision 3). Called immediately after
    /// `lower_gen_block` constructs `gen_place` (the freshly-made generator
    /// handle) for a shell whose `Builder::stream_producer_pump` is `Some`.
    ///
    /// Emits, in the current (post-`MakeGenerator`) block:
    /// ```text
    /// loop_head:
    ///   opt = GeneratorNext(gen_place)          ; Option<Yield>
    ///   branch tag(opt) == 0 (Some) ? send : close
    /// send:
    ///   value = opt.0                            ; MachineVariant payload
    ///   suspend StreamSend { sink, value } -> after_send
    /// after_send:
    ///   goto loop_head
    /// close:
    ///   call hew_sink_close(sink) -> close_next
    /// close_next:
    ///   <cursor left open — `lower_function`'s `finalize_blocks(Terminator::Return)`
    ///    seals it as the implicit unit return>
    /// ```
    ///
    /// The `Option<Yield>` tag convention (`Some` = 0, `None` = 1) mirrors
    /// `Instr::GeneratorNext`'s own documented codegen contract; the tag-test +
    /// payload-extract shape mirrors the general enum-match lowering's
    /// `arm_is_generator_some` arm (`lower_match_enum_tag`) — both read a
    /// `GeneratorNext` dest via `Place::EnumTag`/`Place::MachineVariant { local,
    /// variant_idx: 0, field_idx: 0 }`, the same substrate an ordinary `for v in
    /// gen()` loop already exercises.
    ///
    /// The pump's fault-close and cancellation wiring (decisions 6+7): a
    /// PROLOGUE registers the pump's sink with its own actor
    /// (`hew_actor_gen_sink_register`) so a terminal teardown reaching this
    /// actor while the pump is live can find and fault-close it; each loop
    /// iteration checks the consumer peer (`hew_sink_peer_closed`) BEFORE
    /// resuming the generator, breaking WITHOUT resuming further once the
    /// peer has closed (cancellation — an infinite generator plus a consumer
    /// `break` must not livelock the actor); the shared close path (generator
    /// exhausted OR peer closed) deregisters + frees the sink via
    /// `hew_actor_gen_sink_complete` (replacing the shell's earlier bare
    /// `hew_sink_close`).
    ///
    /// Emits the peer-closed check (decision 6): calls `hew_sink_peer_closed`,
    /// compares its C-ABI `i32` result against zero, and branches on it.
    /// Returns `(resume_bb, close_bb)` — the caller starts `resume_bb` to
    /// drive the generator, and starts (or shares) `close_bb` for the
    /// registered-complete close. Factored out of `build_stream_producer_pump`
    /// to keep that function under the `too_many_lines` threshold.
    fn emit_pump_peer_closed_check(&mut self, sink: Place) -> (u32, u32) {
        use hew_types::runtime_call::RuntimeCallFamily;

        let peer_closed = self.alloc_local(ResolvedTy::I32);
        let after_peer_check = self.alloc_block();
        self.finish_current_block(Terminator::Call {
            callee: "hew_sink_peer_closed".to_string(),
            builtin: Some(RuntimeCallFamily::SinkPeerClosed),
            args: vec![sink],
            dest: Some(peer_closed),
            next: after_peer_check,
        });
        self.start_block(after_peer_check);

        let zero_i32 = self.alloc_local(ResolvedTy::I32);
        self.push_instr(Instr::ConstI64 {
            dest: zero_i32,
            value: 0,
        });
        let is_peer_closed = self.alloc_local(ResolvedTy::Bool);
        self.push_instr(Instr::IntCmp {
            pred: crate::model::CmpPred::NotEq,
            lhs: peer_closed,
            rhs: zero_i32,
            dest: is_peer_closed,
        });

        let resume_bb = self.alloc_block();
        let close_bb = self.alloc_block();
        self.finish_current_block(Terminator::Branch {
            cond: is_peer_closed,
            then_target: close_bb,
            else_target: resume_bb,
        });
        (resume_bb, close_bb)
    }

    /// #2395 decision 2 — record the abandon-edge drop for a `StreamSend`
    /// in-flight yield value. The value is escape-poisoned (so the generic
    /// `drops_for_exit` filter misses it) and its resume-edge release (the
    /// pump's `after_send` inline `Instr::Drop`) never fires on
    /// destroy-while-parked. Stash a congruent `CowHeap` drop keyed by
    /// `suspend_block` (the block carrying the `Terminator::Suspend`); the
    /// post-`enumerate_exits` pass appends it to that suspend's plan so codegen
    /// fires it on the case-1 destroy edge only. Exactly-once holds: the abandon
    /// and resume edges are mutually exclusive and this drop lives only on the
    /// abandon plan. The release protocol is the SAME `generator_yield_drop_symbol`
    /// verdict the resume drop uses, routed through the canonical
    /// `CowHeapRelease::from_symbol` inverse (no picker drift); the resulting kind
    /// passes `validate_drop_plan` (string/bytes via the Place-driven dispatcher,
    /// `Vec*` via its dedicated owned/plain arms). A symbol outside the wired set
    /// (`from_symbol` → `None`) skips the drop — leak-not-wrong-free.
    fn record_stream_send_abandon_drop(
        &mut self,
        suspend_block: u32,
        value: Place,
        yield_ty: &ResolvedTy,
        symbol: &'static str,
    ) {
        if let Some(release) = crate::ownership::CowHeapRelease::from_symbol(symbol) {
            self.suspend_abandon_extra_drops
                .entry(suspend_block)
                .or_default()
                .push(ElabDrop {
                    place: value,
                    ty: yield_ty.clone(),
                    drop_fn: None,
                    kind: DropKind::CowHeap { release },
                    guard: None,
                });
        }
    }

    /// Composite twin of [`Builder::record_stream_send_abandon_drop`]: stash
    /// the in-flight record/enum yield value's abandon-edge release as the
    /// same `DropKind::RecordInPlace` / `DropKind::EnumInPlace` plan drop the
    /// function-scope spine uses (`drop_fn = None`; the helper resolves from
    /// `ElabDrop::ty`). Exactly-once holds identically: the abandon and
    /// resume edges are mutually exclusive, and this drop lives only on the
    /// abandon plan. `validate_drop_plan` accepts the dedicated kinds on a
    /// `Place::Local` composite, so the congruence gate is unchanged.
    fn record_stream_send_abandon_composite_drop(
        &mut self,
        suspend_block: u32,
        value: Place,
        yield_ty: &ResolvedTy,
        kind: crate::ownership::InPlaceReleaseKind,
    ) {
        let drop_kind = match kind {
            crate::ownership::InPlaceReleaseKind::Record => DropKind::RecordInPlace,
            crate::ownership::InPlaceReleaseKind::Enum => DropKind::EnumInPlace,
        };
        self.suspend_abandon_extra_drops
            .entry(suspend_block)
            .or_default()
            .push(ElabDrop {
                place: value,
                ty: yield_ty.clone(),
                drop_fn: None,
                kind: drop_kind,
                guard: None,
            });
    }

    /// Release the pump's per-yield value copy on the stream-send RESUME edge
    /// (and register its abandon-edge twin on the suspend plan).
    ///
    /// The stream send BORROWS the yielded value: `hew_stream_await_send`
    /// and its layout sibling both copy the content out of the slot and
    /// document the argument as borrowed, so the pump stays the sole owner
    /// of the original and must release it exactly once per yield. Emit that
    /// release on the resume edge, before the `Goto` loops back to
    /// overwrite `value_local` on the next iteration. That edge is reached
    /// ONLY when the send resumes (delivered/ready); the abandon-while-parked
    /// path fires the suspend plan's twin drop on the destroy edge instead —
    /// the two edges are mutually exclusive, so the value is never
    /// double-released. `SuspendKind::StreamSend`'s value is escape-poisoned
    /// (`terminator_escape_places`), so no scope-exit drop competes with this
    /// one — it is the sole dropper. The release protocol comes from the same
    /// authority the consumer-side yield-binding drop uses
    /// (`generator_yield_drop_symbol`), so it stays congruent with the codegen
    /// inline-drop validator. `string`/`bytes`/`Vec` release through their
    /// wired symbol; a heap-owning record/enum composite releases through its
    /// synthesised in-place thunk (`DropFnSpec::InPlace`) — the send deep-
    /// cloned the envelope's copy (`encode_elem_envelope` `LayoutManaged`), so
    /// the pump's copy is released without reaching the consumer's. `BitCopy`
    /// scalars carry no inline release (`NoDropPath`); an `Unwired` element
    /// is rejected upstream and never reaches the pump.
    fn emit_pump_yield_value_release(
        &mut self,
        send_bb: u32,
        value_local: Place,
        yield_ty: &ResolvedTy,
    ) {
        match self.generator_yield_drop_symbol(yield_ty) {
            ReleaseSymbolVerdict::Wired(symbol) => {
                // Resume-edge release (delivered/ready): the pump is the sole
                // owner of its copy and frees it here before the loop
                // overwrites the slot.
                self.push_instr(Instr::Drop {
                    place: value_local,
                    ty: yield_ty.clone(),
                    drop_fn: Some(crate::model::DropFnSpec::Release(symbol)),
                });
                // Abandon-edge release for the SAME in-flight value.
                self.record_stream_send_abandon_drop(send_bb, value_local, yield_ty, symbol);
            }
            ReleaseSymbolVerdict::WiredInPlace(kind) => {
                // Composite twin of the Wired arm: same resume-edge inline
                // release, routed through the in-place thunk instead of a
                // C-ABI symbol.
                self.push_instr(Instr::Drop {
                    place: value_local,
                    ty: yield_ty.clone(),
                    drop_fn: Some(crate::model::DropFnSpec::InPlace(kind)),
                });
                self.record_stream_send_abandon_composite_drop(
                    send_bb,
                    value_local,
                    yield_ty,
                    kind,
                );
            }
            ReleaseSymbolVerdict::NoDropPath | ReleaseSymbolVerdict::Unwired(_) => {}
        }
    }

    fn build_stream_producer_pump(&mut self, gen_place: Place, pump: &StreamProducerPumpCtx) {
        use hew_types::runtime_call::RuntimeCallFamily;

        // Prologue: register this pump's own sink with its own actor.
        let actor_self = self.emit_actor_self_handle();
        let after_register = self.alloc_block();
        self.finish_current_block(Terminator::Call {
            callee: "hew_actor_gen_sink_register".to_string(),
            builtin: Some(RuntimeCallFamily::ActorGenSinkRegister),
            args: vec![actor_self, pump.sink],
            dest: None,
            next: after_register,
        });
        self.start_block(after_register);

        let loop_head = self.alloc_block();
        self.finish_current_block(Terminator::Goto { target: loop_head });
        self.start_block(loop_head);

        // Peer-closed check (decision 6): before every resume, not just the
        // first. Branches to `close_bb` without resuming when the consumer
        // has closed, else falls through into the caller's `resume_bb`.
        let (resume_bb, close_bb) = self.emit_pump_peer_closed_check(pump.sink);

        self.start_block(resume_bb);
        let option_ty = ResolvedTy::Named {
            name: "Option".to_string(),
            args: vec![pump.yield_ty.clone()],
            builtin: None,
            is_opaque: false,
        };
        let opt_dest = self.alloc_local(option_ty);
        self.push_instr(Instr::GeneratorNext {
            dest: opt_dest,
            ctx: gen_place,
            yield_ty: pump.yield_ty.clone(),
        });
        let Place::Local(opt_local) = opt_dest else {
            unreachable!("Builder::alloc_local always returns Place::Local")
        };

        let tag_local = self.alloc_local(ResolvedTy::I64);
        self.push_instr(Instr::Move {
            dest: tag_local,
            src: Place::EnumTag(opt_local),
        });
        let some_tag = self.alloc_local(ResolvedTy::I64);
        self.push_instr(Instr::ConstI64 {
            dest: some_tag,
            value: 0,
        });
        let is_some = self.alloc_local(ResolvedTy::Bool);
        self.push_instr(Instr::IntCmp {
            pred: crate::model::CmpPred::Eq,
            lhs: tag_local,
            rhs: some_tag,
            dest: is_some,
        });

        let send_bb = self.alloc_block();
        self.finish_current_block(Terminator::Branch {
            cond: is_some,
            then_target: send_bb,
            else_target: close_bb,
        });

        self.start_block(send_bb);
        let value_local = self.alloc_local(pump.yield_ty.clone());
        self.push_instr(Instr::Move {
            dest: value_local,
            src: Place::MachineVariant {
                local: opt_local,
                variant_idx: 0,
                field_idx: 0,
            },
        });
        let after_send = self.alloc_block();
        self.record_suspend_kind(SuspendKind::StreamSend {
            sink: pump.sink,
            value: value_local,
        });
        self.finish_current_block(Terminator::Suspend {
            resume: after_send,
            cleanup: after_send,
            is_final: false,
        });
        self.start_block(after_send);
        self.emit_pump_yield_value_release(send_bb, value_local, &pump.yield_ty);
        self.finish_current_block(Terminator::Goto { target: loop_head });

        // Shared close path: reached with the generator exhausted (`None`)
        // OR the consumer peer already closed. Either way the sink is done;
        // deregister + free it exactly once.
        self.start_block(close_bb);
        let close_next = self.alloc_block();
        self.finish_current_block(Terminator::Call {
            callee: "hew_actor_gen_sink_complete".to_string(),
            builtin: Some(RuntimeCallFamily::ActorGenSinkComplete),
            args: vec![actor_self, pump.sink],
            dest: None,
            next: close_next,
        });
        self.start_block(close_next);
    }

    /// Lower `HirExprKind::Yield { value, yield_ty }` inside a gen-block body.
    ///
    /// Must only be called from a `Builder` with `in_gen_body = true`. If
    /// `in_gen_body` is `false`, a `yield` expression appeared outside a
    /// generator body — this is a checker invariant violation. Fail-closed:
    /// emit `UnsupportedNode` and return `None` rather than fabricating a
    /// value.
    ///
    /// Emits `Terminator::Yield { value: <place>, next: <resume_block_id> }` on
    /// the CURRENT block, then advances the cursor to the fresh resume block.
    /// The yield expression evaluates to unit in the body (the caller ignores
    /// the `None` return).
    ///
    /// # S3b cross-yield liveness stub
    /// After yielding, locally-defined values that are used again after the
    /// resume point must be stored into (and reloaded from) the generator state
    /// record. S3b's liveness pass identifies those locals and emits
    /// store-before-yield / load-after-resume instructions. This function
    /// intentionally leaves that gap as a comment at the yield site so S3b
    /// has a clean insertion point.
    fn lower_yield_expr(&mut self, expr: &HirExpr, value: Option<&HirExpr>) -> Option<Place> {
        // Fail-closed: `yield` outside a gen-block body is a checker
        // invariant violation. Emit a clear diagnostic rather than
        // fabricating a value.
        if !self.in_gen_body {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "HirExprKind::Yield outside gen-block body".to_string(),
                    site: expr.site,
                },
                note: "yield is only valid inside a gen{} block; \
                       the HIR checker should have rejected this program before MIR"
                    .to_string(),
            });
            return None;
        }

        // Lower the yielded value to a Place.  If the value is absent (bare
        // `yield;` — unit-typed generator), allocate a unit constant.
        let value_place = if let Some(val_expr) = value {
            self.decide(val_expr);
            match self.lower_value(val_expr) {
                Some(p) => p,
                None => {
                    // The value sub-expression failed to lower.  The child
                    // diagnostic has already been pushed; propagate failure
                    // by not emitting the Yield terminator.
                    return None;
                }
            }
        } else {
            // `yield;` — allocate a unit local as the value carrier.
            self.alloc_local(ResolvedTy::Unit)
        };

        // S3b insertion point: store cross-yield live locals to the state
        // record HERE, before the Terminator::Yield. S3b's liveness pass
        // identifies which locals are used after this resume point and emits
        // the store instructions at this site.
        // TODO(S3b): emit store-before-yield for cross-yield live locals.

        // Allocate the resume block id and seal the current block with the
        // Yield terminator.
        let resume_block = self.alloc_block();
        self.finish_current_block(Terminator::Yield {
            value: value_place,
            next: resume_block,
        });
        self.start_block(resume_block);

        // S3b insertion point: reload cross-yield live locals from the state
        // record HERE, at the top of the resume block.
        // TODO(S3b): emit load-after-resume for cross-yield live locals.

        // `yield` evaluates to unit in the gen body.
        None
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
    /// Instruction escape events: (block id, instruction index, field).
    escapes: Vec<(u32, usize, u32)>,
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
/// cooperate site to block 0; the MIR builder's `finalize_blocks` constructs the
/// entry block as id 0. This single constant keeps the elaborator's
/// FunctionEntry-cancel handling aligned with those producers.
const ENTRY_BLOCK_ID: u32 = 0;

#[cfg(test)]
use self::{
    cfg_util::*, composite_own::*, consts::*, drop_plan::*, machine_synth::*, split_consume::*,
    suspend_places::*, temp_drop::*,
};
