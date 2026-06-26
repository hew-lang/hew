//! Typed cross-layer descriptor for compiler-known runtime / builtin calls.
//!
//! # Why this exists
//!
//! Today `Instr::CallRuntimeAbi` carries a validated `String` symbol and
//! `Terminator::Call` dispatches several compiler-magic ABI paths by
//! string-matching the callee name. The checker / HIR resolved a typed
//! identity for every one of those calls (`MethodCallRewrite`,
//! `ResolvedImplCall`, etc.) and stringified it; later layers then have
//! to re-match the string to recover the family. A new variant of a
//! closed family — added without updating every string-match site —
//! silently mis-lowers (LESSONS P0 `boundary-fail-closed`,
//! `exhaustive-traversal-and-lowering`, `match-fail-closed`).
//!
//! This module introduces the closed-set typed substrate that the
//! checker, HIR await-classifier, MIR carrier, and codegen will all
//! migrate onto over the follow-up commits. The substrate itself is
//! purely additive — this commit does not change `MethodCallRewrite`,
//! `RuntimeCall`, codegen, HIR, or any match site. It is
//! dead-code-but-compiled until the first producer wires onto it.
//!
//! # Correctness anchor
//!
//! The bijection enforced by [`RuntimeCallFamily::c_symbol`] and
//! [`RuntimeCallFamily::from_c_symbol`] is the load-bearing invariant.
//! Round-trip tests in `hew-mir/tests/runtime_call_allowlist.rs` pin
//! the substrate against the MIR-side `MIR_EMITTER_RUNTIME_SYMBOLS`
//! allowlist; the local tests in this module pin everything that is
//! intrinsic to the substrate itself (uniqueness of `c_symbol()`,
//! constructor fail-closed behaviour, `consumes_receiver` /
//! `is_async_suspending` parity).
//!
//! # Crate placement
//!
//! Lives in `hew-types` because the checker (`MethodCallRewrite`,
//! `MethodTarget`) is the producer of typed descriptors and must be
//! able to construct one without a circular dependency. `hew-mir`
//! re-exports the substrate as `hew_mir::runtime_call` so the existing
//! MIR/codegen call sites keep their import paths.
//!
//! # Asymmetric split discipline
//!
//! [`RuntimeCallFamily`] is closed. The sole legitimate open-set string
//! in the cross-layer descriptor world is
//! `MethodCallRewrite::RewriteModuleQualifiedToFunction.c_symbol`, which
//! carries a user-module-qualified dotted-name. That string is NOT
//! covered by this substrate by design — it is structurally open-set
//! and clippy-gated.

use crate::ResolvedTy;
use strum::{EnumIter, IntoEnumIterator};

// =============================================================================
// Element-type discriminators
// =============================================================================

/// Element-type discriminator for `Vec<T>::get` runtime entries
/// (`hew_vec_get_*`). One variant per monomorphic-element C-ABI symbol
/// emitted today by `hew-runtime/src/vec.rs`.
///
/// `Layout` covers the layout-descriptor path for `BitCopy` Named records
/// and tuples; `Owned` is the W5.016 borrow-getter for non-Copy owned
/// elements. The pending Vec genericisation work will collapse most of
/// these onto a single `Generic` variant that reads the descriptor's
/// `elem` field — out of scope here.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default, EnumIter)]
pub enum VecGetElem {
    #[default]
    Bool,
    F32,
    F64,
    I8,
    I16,
    I32,
    I64,
    Layout,
    Owned,
    Ptr,
    Str,
    U8,
    U16,
}

/// Element-type discriminator for `Vec<T>::slice_range` runtime entries
/// (`hew_vec_slice_range_*`). Narrower than [`VecGetElem`] because slice
/// does not have a bool path today; descriptor-backed record/tuple elements
/// route through the `Layout`/`Owned` substrate variants.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default, EnumIter)]
pub enum VecSliceElem {
    #[default]
    Bytesize,
    F64,
    I32,
    I64,
    Layout,
    Owned,
    Ptr,
    Str,
}

/// Element-kind discriminator for the Sink write families
/// (`hew_sink_write_bytes` vs `hew_sink_write_string`). Stream/channel
/// recv retired their per-element symbols in favour of the
/// element-layout-witness `*_layout` entries, which bypass
/// `RuntimeCallFamily` entirely (codegen `Terminator::Call` intercept).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default, EnumIter)]
pub enum StreamElementKind {
    #[default]
    Bytes,
    String,
}

/// Math-intrinsic family discriminator. Mirrors the user-visible
/// callee names dispatched in `hew-codegen-rs::llvm::math_builtin_intrinsic`
/// today (the names appear as `BindingRef.name` in HIR — `"sqrt"`, `"sin"`,
/// …). Pre-staged: a follow-up migrates the callee-name match in codegen
/// onto a typed `RuntimeCallFamily::MathIntrinsic(...)`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default, EnumIter)]
pub enum MathIntrinsic {
    #[default]
    Sqrt,
    Exp,
    Log,
    Sin,
    Cos,
    AbsI64,
    MinI64,
    MaxI64,
    AbsF64,
    MinF64,
    MaxF64,
    Pow,
    Floor,
    Ceil,
    Round,
}

// =============================================================================
// RuntimeCallFamily — closed-set typed catalog
// =============================================================================

/// Closed-set discriminator for every compiler-known runtime / builtin
/// call. One variant per `(method, generic-arity)` tuple. Adding a new
/// runtime symbol that MIR producers can emit means adding a variant
/// here AND adding the round-trip arm in [`RuntimeCallFamily::c_symbol`]
/// AND adding the symbol to `MIR_EMITTER_RUNTIME_SYMBOLS` (allowlist-
/// covered families only). The bijection test catches every drift.
///
/// Variants are grouped by surface family; ordering within a group is
/// alphabetical by C-symbol leaf to ease diffing against the allowlist.
///
/// `non_exhaustive` is INTENTIONALLY OMITTED — this enum's whole purpose
/// is fail-closed exhaustiveness across consumer match sites (LESSONS P0
/// `match-fail-closed`). A future contributor cannot silently extend it
/// behind a wildcard arm; every consumer must add the new arm at the
/// next slice.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, EnumIter)]
pub enum RuntimeCallFamily {
    // --- Actor cooperate/link/monitor/unlink/spawn surface ------------------
    ActorAsk,
    ActorAskWithChannel,
    ActorCooperate,
    /// `MonitorRef::close` → `hew_actor_demonitor(ref_id: u64) -> void`.
    /// In the drop path, codegen extracts `ref_id` from the struct alloca
    /// via `build_struct_gep` + `build_load` and passes it directly.
    /// Present as a `RuntimeCallFamily` variant for allowlist parity only;
    /// the canonical path is `RuntimeDropDescriptor::MonitorRefClose`.
    ActorDemonitor,
    ActorLink,
    ActorMonitor,
    ActorSelf,
    ActorSendById,
    ActorSpawn,
    ActorUnlink,

    // --- Auto-injected mutex substrate (closure-env / generator-state) ------
    AutoMutexAlloc,
    AutoMutexFree,
    AutoMutexLock,
    AutoMutexUnlock,

    // --- Bytes value get/index/length/slice/push ----------------------------
    BytesGet,
    BytesIndex,
    BytesLen,
    BytesPush,
    BytesSlice,

    // --- CancellationToken retain/release/poll ------------------------------
    CancelTokenIsRequested,
    CancelTokenRelease,
    CancelTokenRetain,

    // --- Channel<T> (std::channel) ------------------------------------------
    // recv/try_recv/send ride the element-layout-witness `*_layout`
    // entries (one symbol per operation for every describable element
    // type; the elem identity travels on the checker-resolved
    // `Option<T>` / value type, never on the symbol). They are
    // pre-staged: codegen intercepts the `Terminator::Call` by callee
    // identity, so they are not in `MIR_EMITTER_RUNTIME_SYMBOLS`.
    ChannelRecvLayout,
    ChannelSendLayout,
    ChannelTryRecvLayout,
    ChannelSenderClose,
    ChannelReceiverClose,

    // --- Duplex<S, R> dual-queue substrate ----------------------------------
    DuplexClone,
    DuplexClose,
    DuplexCloseHalf,
    DuplexPair,
    DuplexPayloadFree,
    DuplexRecv,
    DuplexRecvHalf,
    DuplexSend,
    DuplexSendHalf,
    DuplexTryRecv,
    DuplexTrySend,

    // --- Duration accessors (monomorphic time canaries) ---------------------
    DurationAbs,
    DurationHours,
    DurationIsZero,
    DurationMicros,
    DurationMillis,
    DurationMins,
    DurationNanos,
    DurationSecs,

    // --- Trait-object heap-box storage --------------------------------------
    DynBoxAlloc,
    DynBoxFree,

    // --- Layout-backed HashMap ---------------------------------------------
    HashMapContainsKeyLayout,
    HashMapFreeLayout,
    HashMapGetLayout,
    HashMapInsertLayout,
    /// `m.keys()` / `m.values()` projection ops. Pre-staged: they ride
    /// the `Terminator::Call` route (checker `MethodTarget.symbol_name`),
    /// not `Instr::CallRuntimeAbi`, so their symbols are not in
    /// `MIR_EMITTER_RUNTIME_SYMBOLS`. Catalogued so the codegen
    /// layout-fact walker classifies them by family, not symbol prefix.
    HashMapKeysLayout,
    HashMapLenLayout,
    /// The `HashMap::new` constructor surface form. A distinct callee
    /// identity from [`Self::HashMapNewWithLayout`]: the catalog row
    /// `"HashMap::new"` (`BuiltinLinkage::CalleeNameDispatchOnly`)
    /// survives to codegen as the literal callee name when the checker
    /// did not rewrite the construction to the synthesized
    /// `hew_hashmap_new_with_layout` form. Both identities are real at
    /// the `Terminator::Call` intercept; the bijection demands one
    /// variant per callee identity.
    HashMapNew,
    HashMapNewWithLayout,
    HashMapRemoveLayout,
    HashMapValuesLayout,

    // --- Layout-backed HashSet ---------------------------------------------
    HashSetContainsLayout,
    HashSetFreeLayout,
    HashSetInsertLayout,
    HashSetIsEmptyLayout,
    HashSetLenLayout,
    /// The `HashSet::new` constructor surface form; see
    /// [`Self::HashMapNew`] for the two-identity rationale.
    HashSetNew,
    HashSetNewWithLayout,
    HashSetRemoveLayout,

    // --- Instant accessors --------------------------------------------------
    InstantDurationSince,
    InstantElapsed,
    InstantNow,

    // --- Lambda-actor surface (overlays Duplex<Msg, Reply>) -----------------
    LambdaActorAsk,
    /// Body-side reply-buffer allocator: lambda-actor body fns publish
    /// their reply payload via a Box-allocated `*mut u8` that the runtime
    /// (`hew_reply`) copies into a libc-allocated buffer before publishing
    /// to the waiter; the body's Box-allocated original is then freed
    /// runtime-internally. Distinct from the waiter-side `hew_reply_payload_free`.
    LambdaBodyAllocReplyBuf,
    LambdaActorClone,
    LambdaActorDowngrade,
    /// Process-exit drain for detached lambda-actor dispatch threads
    /// (`hew-runtime/src/lambda_actor.rs`). Lambda actors run on
    /// dedicated OS threads NOT the work-stealing scheduler, so
    /// `hew_shutdown_wait` cannot drain them; codegen emits
    /// `hew_lambda_drain_all(0)` in main's Return epilogue so any
    /// in-flight body work completes before process exit.
    LambdaDrainAll,
    LambdaActorNew,
    /// Lambda-actor handle release; consumes receiver (mirrors
    /// `runtime_symbol_consumes_receiver` in `hew-types/src/builtin_names.rs`).
    LambdaActorRelease,
    LambdaActorSend,
    LambdaActorWeakClone,
    LambdaActorWeakDrop,
    LambdaActorWeakSend,

    // --- Math intrinsics ---------------------------------------------------
    // Pre-staged: dispatched today by user-visible callee name
    // (`"sqrt"`, `"sin"`, …) via codegen's `math_builtin_intrinsic`
    // lookup; not in `MIR_EMITTER_RUNTIME_SYMBOLS`.
    MathIntrinsic(MathIntrinsic),

    // --- Node operations ---------------------------------------------------
    // Pre-staged: `Node::lookup` is a Terminator::Call callee-name
    // intercept today (`hew-codegen-rs/src/llvm.rs:25631`); `Node::register_pid`
    // already has a typed `FnSymbol::NodeRegisterPid` so it does NOT appear
    // here as a runtime-call family.
    NodeLookup,

    // --- User metrics (#1862) -----------------------------------------------
    // `std::metrics` emit path: register-or-get + mutate developer-defined
    // counters/gauges/histograms (and their labelled `*Vec` forms). Non-
    // suspending, non-consuming (handles are Copy index IDs).
    MetricCounterRegister,
    MetricCounterInc,
    MetricCounterAdd,
    MetricGaugeRegister,
    MetricGaugeSet,
    MetricGaugeInc,
    MetricGaugeDec,
    MetricGaugeAdd,
    MetricHistogramRegister,
    /// Bucketless histogram register (name-only ABI). The bucketed
    /// `MetricHistogramRegister` takes a raw `(*const i64, len)` array that a
    /// Hew `extern "C"` declaration cannot express, so the stdlib reaches this
    /// scalar entry point instead; it registers a histogram with no buckets
    /// (just the running observation count).
    MetricHistogramRegisterSimple,
    MetricHistogramRecord,
    MetricVecRegister,
    MetricVecWith,

    // --- Observe ------------------------------------------------------------
    ObserveReadU64,
    ObserveScrape,
    ObserveSeries,
    ObserveBarrier,

    // --- Rc allocation for task-owned closure environments ------------------
    RcNew,

    // --- RecvHalf<T> --------------------------------------------------------
    RecvHalfRecv,
    RecvHalfTryRecv,

    // --- Regex runtime ABI --------------------------------------------------
    RegexCapture,
    RegexCompile,
    RegexFreeCapture,
    RegexMatch,

    // --- RemotePid<T>::tell intercept --------------------------------------
    // Pre-staged for codegen-intercept consumers: `pid.tell(msg)` on a
    // `RemotePid<T>` receiver dispatches via the `hew_remote_pid_tell`
    // callee-name intercept (`hew-codegen-rs/src/llvm.rs:25649`); it
    // does NOT call that symbol directly (codegen emits the
    // `hew_actor_send_by_id` sequence + `Result<(), SendError>` wrapping
    // in-place). The catalog declares `hew_remote_pid_tell` with linkage
    // `BuiltinLinkage::CalleeNameDispatchOnly` so the symbol is a real
    // callee identity but has no extern body.
    //
    // `RemoteActorAsk` is INTENTIONALLY ABSENT: the checker's
    // `MethodCallRewrite::RemoteActorAsk` is a fieldless structured
    // marker that HIR lowers to `HirExprKind::RemoteActorAsk` and MIR
    // lowers to `Terminator::RemoteAsk` — there is no callee-name string
    // anywhere in that path, so it does not belong in a runtime-call
    // descriptor catalog.
    RemotePidTell,

    // --- Reply channel surface (select{} actor-ask arm) ---------------------
    ReplyChannelCancel,
    ReplyChannelFree,
    ReplyChannelNew,
    /// Waiter-side payload free — frees the libc-allocated reply
    /// buffer that the runtime publishes back to the ask call site.
    /// Distinct from `LambdaBodyAllocReplyBuf` (body-side allocator)
    /// and from `ReplyChannelFree` (handle-level cleanup).
    ReplyPayloadFree,
    ReplyWait,

    // --- Select winner-picker ----------------------------------------------
    SelectFirst,

    // --- SendHalf<T> -------------------------------------------------------
    SendHalfSend,
    SendHalfTrySend,

    // --- Sink<T> -----------------------------------------------------------
    // Pre-staged consumers: today the bytes path is producer-emitted via
    // `Terminator::Call` intercept; symbols flow through the codegen
    // callee-name match, not `Instr::CallRuntimeAbi`. Both element kinds
    // exist in the runtime (`hew-runtime/src/sink.rs`) and in the checker
    // `BuiltinMethodRuntime::ElementOverload` table at
    // `hew-types/src/builtin_names.rs:253-265`. `SinkTryWrite` mirrors
    // `Sink::try_send` from the same table.
    SinkClose,
    SinkWrite(StreamElementKind),
    SinkTryWrite(StreamElementKind),

    // --- Stream<T> ---------------------------------------------------------
    // recv/try_recv/send ride the element-layout-witness `*_layout`
    // entries (see the Channel note above). `consumes_receiver()` is
    // `true` for `StreamClose`/`SinkClose` to mirror
    // `runtime_symbol_consumes_receiver`.
    StreamClose,
    StreamNextLayout,
    StreamSendLayout,
    StreamTryNextLayout,

    // --- String runtime helpers --------------------------------------------
    StringCharCount,
    StringConcat,
    StringGet,
    StringIndex,
    StringSliceCodepoints,

    // --- Supervisor --------------------------------------------------------
    SupervisorChildGet,
    SupervisorNestedGet,
    /// `hew_supervisor_pool_child_get(sup, pool_key, index) -> ChildLookupResult`
    /// — resolve a static-pool member through its live static slot. Emitted by
    /// the MIR static-pool accessor (`sup.pool[i]` / `.get(i)`).
    SupervisorPoolChildGet,
    /// `hew_supervisor_pool_len(sup, pool_key) -> i64` — the static-pool member
    /// count (`sup.pool.len()`).
    SupervisorPoolLen,
    SupervisorStop,
    /// `hew_supervisor_restart_await_blocking(sup, key) -> void` — the
    /// contextless `await_restart` path (`main` / free fn). Blocks the calling
    /// thread until the child slot is Live or permanently Dead.
    SupervisorRestartAwaitBlocking,

    // --- TCP attach (network actor binding) --------------------------------
    // Pre-staged: `conn.attach(handler)` dispatches via the
    // `hew_tcp_attach_local` callee-name intercept.
    TcpAttachLocal,

    // --- Task ABI (scope{}/spawn/await) ------------------------------------
    TaskAwaitBlocking,
    TaskCompleteThreaded,
    TaskCompletionObserve,
    TaskCompletionUnobserve,
    TaskFree,
    TaskGetEnv,
    TaskGetError,
    TaskGetResult,
    TaskNew,
    TaskScopeCancelAfterNs,
    TaskScopeDestroy,
    TaskScopeJoinAll,
    TaskScopeNew,
    TaskScopeSetCurrent,
    TaskScopeSpawn,
    TaskSetEnv,
    TaskSetResult,
    TaskSpawnThread,

    // --- Vec<T> ------------------------------------------------------------
    VecGet(VecGetElem),
    VecLen,
    VecSliceRange(VecSliceElem),

    // --- Trait-object dispatch diagnostics ---------------------------------
    VtableDispatchPanicOnOob,
}

impl RuntimeCallFamily {
    /// Resolve the C-ABI symbol the family lowers to. Total function;
    /// every variant has exactly one symbol (the bijection guarantee).
    ///
    /// For families whose symbols are in `MIR_EMITTER_RUNTIME_SYMBOLS`,
    /// the returned string is allowlist-recognised (consumer
    /// invariant). For pre-staged families (Channel, Stream, Sink,
    /// Node, Math, `RemotePidTell`, `RemoteActorAsk`, `TcpAttachLocal`), the
    /// returned string is the codegen `Terminator::Call` intercept
    /// callee name; a follow-up wires those producers and folds them into the
    /// allowlist.
    #[must_use]
    #[allow(
        clippy::too_many_lines,
        reason = "bijection enumeration IS the substrate; one arm per family"
    )]
    pub fn c_symbol(self) -> &'static str {
        match self {
            // Actor
            Self::ActorAsk => "hew_actor_ask",
            Self::ActorAskWithChannel => "hew_actor_ask_with_channel",
            Self::ActorCooperate => "hew_actor_cooperate",
            Self::ActorDemonitor => "hew_actor_demonitor",
            Self::ActorLink => "hew_actor_link",
            Self::ActorMonitor => "hew_actor_monitor",
            Self::ActorSelf => "hew_actor_self",
            Self::ActorSendById => "hew_actor_send_by_id",
            Self::ActorSpawn => "hew_actor_spawn",
            Self::ActorUnlink => "hew_actor_unlink",
            // Auto-mutex
            Self::AutoMutexAlloc => "hew_auto_mutex_alloc",
            Self::AutoMutexFree => "hew_auto_mutex_free",
            Self::AutoMutexLock => "hew_auto_mutex_lock",
            Self::AutoMutexUnlock => "hew_auto_mutex_unlock",
            // Bytes
            Self::BytesGet => "hew_bytes_get",
            Self::BytesIndex => "hew_bytes_index",
            Self::BytesLen => "hew_bytes_len",
            Self::BytesPush => "hew_bytes_push",
            Self::BytesSlice => "hew_bytes_slice",
            // CancellationToken
            Self::CancelTokenIsRequested => "hew_cancel_token_is_requested",
            Self::CancelTokenRelease => "hew_cancel_token_release",
            Self::CancelTokenRetain => "hew_cancel_token_retain",
            // Channel (pre-staged)
            Self::ChannelRecvLayout => "hew_channel_recv_layout",
            Self::ChannelSendLayout => "hew_channel_send_layout",
            Self::ChannelTryRecvLayout => "hew_channel_try_recv_layout",
            Self::ChannelSenderClose => "hew_channel_sender_close",
            Self::ChannelReceiverClose => "hew_channel_receiver_close",
            // Duplex
            Self::DuplexClone => "hew_duplex_clone",
            Self::DuplexClose => "hew_duplex_close",
            Self::DuplexCloseHalf => "hew_duplex_close_half",
            Self::DuplexPair => "hew_duplex_pair",
            Self::DuplexPayloadFree => "hew_duplex_payload_free",
            Self::DuplexRecv => "hew_duplex_recv",
            Self::DuplexRecvHalf => "hew_duplex_recv_half",
            Self::DuplexSend => "hew_duplex_send",
            Self::DuplexSendHalf => "hew_duplex_send_half",
            Self::DuplexTryRecv => "hew_duplex_try_recv",
            Self::DuplexTrySend => "hew_duplex_try_send",
            // Duration
            Self::DurationAbs => "hew_duration_abs",
            Self::DurationHours => "hew_duration_hours",
            Self::DurationIsZero => "hew_duration_is_zero",
            Self::DurationMicros => "hew_duration_micros",
            Self::DurationMillis => "hew_duration_millis",
            Self::DurationMins => "hew_duration_mins",
            Self::DurationNanos => "hew_duration_nanos",
            Self::DurationSecs => "hew_duration_secs",
            // Dyn box
            Self::DynBoxAlloc => "hew_dyn_box_alloc",
            Self::DynBoxFree => "hew_dyn_box_free",
            // HashMap
            Self::HashMapContainsKeyLayout => "hew_hashmap_contains_key_layout",
            Self::HashMapFreeLayout => "hew_hashmap_free_layout",
            Self::HashMapGetLayout => "hew_hashmap_get_layout",
            Self::HashMapInsertLayout => "hew_hashmap_insert_layout",
            Self::HashMapKeysLayout => "hew_hashmap_keys_layout",
            Self::HashMapLenLayout => "hew_hashmap_len_layout",
            Self::HashMapNew => "HashMap::new",
            Self::HashMapNewWithLayout => "hew_hashmap_new_with_layout",
            Self::HashMapRemoveLayout => "hew_hashmap_remove_layout",
            Self::HashMapValuesLayout => "hew_hashmap_values_layout",
            // HashSet
            Self::HashSetContainsLayout => "hew_hashset_contains_layout",
            Self::HashSetFreeLayout => "hew_hashset_free_layout",
            Self::HashSetInsertLayout => "hew_hashset_insert_layout",
            Self::HashSetIsEmptyLayout => "hew_hashset_is_empty_layout",
            Self::HashSetLenLayout => "hew_hashset_len_layout",
            Self::HashSetNew => "HashSet::new",
            Self::HashSetNewWithLayout => "hew_hashset_new_with_layout",
            Self::HashSetRemoveLayout => "hew_hashset_remove_layout",
            // Instant
            Self::InstantDurationSince => "hew_instant_duration_since",
            Self::InstantElapsed => "hew_instant_elapsed",
            Self::InstantNow => "hew_instant_now",
            // Lambda actor
            Self::LambdaActorAsk => "hew_lambda_actor_ask",
            Self::LambdaBodyAllocReplyBuf => "hew_lambda_body_alloc_reply_buf",
            Self::LambdaActorClone => "hew_lambda_actor_clone",
            Self::LambdaActorDowngrade => "hew_lambda_actor_downgrade",
            Self::LambdaDrainAll => "hew_lambda_drain_all",
            Self::LambdaActorNew => "hew_lambda_actor_new",
            Self::LambdaActorRelease => "hew_lambda_actor_release",
            Self::LambdaActorSend => "hew_lambda_actor_send",
            Self::LambdaActorWeakClone => "hew_lambda_actor_weak_clone",
            Self::LambdaActorWeakDrop => "hew_lambda_actor_weak_drop",
            Self::LambdaActorWeakSend => "hew_lambda_actor_weak_send",
            // Math intrinsics (pre-staged; user-visible callee names)
            Self::MathIntrinsic(MathIntrinsic::Sqrt) => "sqrt",
            Self::MathIntrinsic(MathIntrinsic::Exp) => "exp",
            Self::MathIntrinsic(MathIntrinsic::Log) => "log",
            Self::MathIntrinsic(MathIntrinsic::Sin) => "sin",
            Self::MathIntrinsic(MathIntrinsic::Cos) => "cos",
            Self::MathIntrinsic(MathIntrinsic::AbsI64) => "abs",
            Self::MathIntrinsic(MathIntrinsic::MinI64) => "min",
            Self::MathIntrinsic(MathIntrinsic::MaxI64) => "max",
            Self::MathIntrinsic(MathIntrinsic::AbsF64) => "abs_f",
            Self::MathIntrinsic(MathIntrinsic::MinF64) => "min_f",
            Self::MathIntrinsic(MathIntrinsic::MaxF64) => "max_f",
            Self::MathIntrinsic(MathIntrinsic::Pow) => "pow",
            Self::MathIntrinsic(MathIntrinsic::Floor) => "floor",
            Self::MathIntrinsic(MathIntrinsic::Ceil) => "ceil",
            Self::MathIntrinsic(MathIntrinsic::Round) => "round",
            // Node (pre-staged)
            Self::NodeLookup => "Node::lookup",
            // User metrics (#1862)
            Self::MetricCounterRegister => "hew_metric_counter_register",
            Self::MetricCounterInc => "hew_metric_counter_inc",
            Self::MetricCounterAdd => "hew_metric_counter_add",
            Self::MetricGaugeRegister => "hew_metric_gauge_register",
            Self::MetricGaugeSet => "hew_metric_gauge_set",
            Self::MetricGaugeInc => "hew_metric_gauge_inc",
            Self::MetricGaugeDec => "hew_metric_gauge_dec",
            Self::MetricGaugeAdd => "hew_metric_gauge_add",
            Self::MetricHistogramRegister => "hew_metric_histogram_register",
            Self::MetricHistogramRegisterSimple => "hew_metric_histogram_register_simple",
            Self::MetricHistogramRecord => "hew_metric_histogram_record",
            Self::MetricVecRegister => "hew_metric_vec_register",
            Self::MetricVecWith => "hew_metric_vec_with",
            // Observe
            Self::ObserveReadU64 => "hew_observe_read_u64",
            Self::ObserveScrape => "hew_observe_scrape",
            Self::ObserveSeries => "hew_observe_series",
            Self::ObserveBarrier => "hew_observe_barrier",
            // Rc
            Self::RcNew => "hew_rc_new",
            // RecvHalf
            Self::RecvHalfRecv => "hew_recv_half_recv",
            Self::RecvHalfTryRecv => "hew_recv_half_try_recv",
            // Regex
            Self::RegexCapture => "hew_regex_capture",
            Self::RegexCompile => "hew_regex_compile",
            Self::RegexFreeCapture => "hew_regex_free_capture",
            Self::RegexMatch => "hew_regex_match",
            // RemotePid<T>::tell intercept
            Self::RemotePidTell => "hew_remote_pid_tell",
            // Reply channel
            Self::ReplyChannelCancel => "hew_reply_channel_cancel",
            Self::ReplyChannelFree => "hew_reply_channel_free",
            Self::ReplyChannelNew => "hew_reply_channel_new",
            Self::ReplyPayloadFree => "hew_reply_payload_free",
            Self::ReplyWait => "hew_reply_wait",
            // Select
            Self::SelectFirst => "hew_select_first",
            // SendHalf
            Self::SendHalfSend => "hew_send_half_send",
            Self::SendHalfTrySend => "hew_send_half_try_send",
            // Sink (pre-staged consumers; both element kinds real per
            // hew-types/src/builtin_names.rs:253-265)
            Self::SinkClose => "hew_sink_close",
            Self::SinkWrite(StreamElementKind::Bytes) => "hew_sink_write_bytes",
            Self::SinkWrite(StreamElementKind::String) => "hew_sink_write_string",
            Self::SinkTryWrite(StreamElementKind::Bytes) => "hew_sink_try_write_bytes",
            Self::SinkTryWrite(StreamElementKind::String) => "hew_sink_try_write_string",
            // Stream
            Self::StreamClose => "hew_stream_close",
            Self::StreamNextLayout => "hew_stream_next_layout",
            Self::StreamSendLayout => "hew_stream_send_layout",
            Self::StreamTryNextLayout => "hew_stream_try_next_layout",
            // String
            Self::StringCharCount => "hew_string_char_count",
            Self::StringConcat => "hew_string_concat",
            Self::StringGet => "hew_string_get",
            Self::StringIndex => "hew_string_index",
            Self::StringSliceCodepoints => "hew_string_slice_codepoints",
            // Supervisor
            Self::SupervisorChildGet => "hew_supervisor_child_get",
            Self::SupervisorNestedGet => "hew_supervisor_nested_get",
            Self::SupervisorPoolChildGet => "hew_supervisor_pool_child_get",
            Self::SupervisorPoolLen => "hew_supervisor_pool_len",
            Self::SupervisorStop => "hew_supervisor_stop",
            Self::SupervisorRestartAwaitBlocking => "hew_supervisor_restart_await_blocking",
            // TCP attach (pre-staged)
            Self::TcpAttachLocal => "hew_tcp_attach_local",
            // Task
            Self::TaskAwaitBlocking => "hew_task_await_blocking",
            Self::TaskCompleteThreaded => "hew_task_complete_threaded",
            Self::TaskCompletionObserve => "hew_task_completion_observe",
            Self::TaskCompletionUnobserve => "hew_task_completion_unobserve",
            Self::TaskFree => "hew_task_free",
            Self::TaskGetEnv => "hew_task_get_env",
            Self::TaskGetError => "hew_task_get_error",
            Self::TaskGetResult => "hew_task_get_result",
            Self::TaskNew => "hew_task_new",
            Self::TaskScopeCancelAfterNs => "hew_task_scope_cancel_after_ns",
            Self::TaskScopeDestroy => "hew_task_scope_destroy",
            Self::TaskScopeJoinAll => "hew_task_scope_join_all",
            Self::TaskScopeNew => "hew_task_scope_new",
            Self::TaskScopeSetCurrent => "hew_task_scope_set_current",
            Self::TaskScopeSpawn => "hew_task_scope_spawn",
            Self::TaskSetEnv => "hew_task_set_env",
            Self::TaskSetResult => "hew_task_set_result",
            Self::TaskSpawnThread => "hew_task_spawn_thread",
            // Vec
            Self::VecGet(VecGetElem::Bool) => "hew_vec_get_bool",
            Self::VecGet(VecGetElem::F32) => "hew_vec_get_f32",
            Self::VecGet(VecGetElem::F64) => "hew_vec_get_f64",
            Self::VecGet(VecGetElem::I8) => "hew_vec_get_i8",
            Self::VecGet(VecGetElem::I16) => "hew_vec_get_i16",
            Self::VecGet(VecGetElem::I32) => "hew_vec_get_i32",
            Self::VecGet(VecGetElem::I64) => "hew_vec_get_i64",
            Self::VecGet(VecGetElem::Layout) => "hew_vec_get_layout",
            Self::VecGet(VecGetElem::Owned) => "hew_vec_get_owned",
            Self::VecGet(VecGetElem::Ptr) => "hew_vec_get_ptr",
            Self::VecGet(VecGetElem::Str) => "hew_vec_get_str",
            Self::VecGet(VecGetElem::U8) => "hew_vec_get_u8",
            Self::VecGet(VecGetElem::U16) => "hew_vec_get_u16",
            Self::VecLen => "hew_vec_len",
            Self::VecSliceRange(VecSliceElem::Bytesize) => "hew_vec_slice_range_bytesize",
            Self::VecSliceRange(VecSliceElem::F64) => "hew_vec_slice_range_f64",
            Self::VecSliceRange(VecSliceElem::I32) => "hew_vec_slice_range_i32",
            Self::VecSliceRange(VecSliceElem::I64) => "hew_vec_slice_range_i64",
            Self::VecSliceRange(VecSliceElem::Layout) => "hew_vec_slice_range_layout",
            Self::VecSliceRange(VecSliceElem::Owned) => "hew_vec_slice_range_owned",
            Self::VecSliceRange(VecSliceElem::Ptr) => "hew_vec_slice_range_ptr",
            Self::VecSliceRange(VecSliceElem::Str) => "hew_vec_slice_range_str",
            // Vtable
            Self::VtableDispatchPanicOnOob => "hew_vtable_dispatch_panic_on_oob",
        }
    }

    /// Inverse of [`RuntimeCallFamily::c_symbol`]: recover the typed
    /// family from its C-ABI symbol. Returns `None` for any string the
    /// substrate does not recognise — including user-defined extern
    /// FFI symbols routed via `#[extern_symbol]`, which are open-set
    /// by design and never need to be in this catalog.
    ///
    /// Bijection with `c_symbol()` is the load-bearing invariant; the
    /// round-trip is unit-tested below. Adding a new variant requires
    /// updating both arms in lock-step, which the bijection test
    /// catches at build time.
    #[must_use]
    #[allow(
        clippy::too_many_lines,
        reason = "inverse of the c_symbol enumeration; one arm per symbol"
    )]
    pub fn from_c_symbol(sym: &str) -> Option<Self> {
        let family = match sym {
            // Actor
            "hew_actor_ask" => Self::ActorAsk,
            "hew_actor_ask_with_channel" => Self::ActorAskWithChannel,
            "hew_actor_cooperate" => Self::ActorCooperate,
            "hew_actor_demonitor" => Self::ActorDemonitor,
            "hew_actor_link" => Self::ActorLink,
            "hew_actor_monitor" => Self::ActorMonitor,
            "hew_actor_self" => Self::ActorSelf,
            "hew_actor_send_by_id" => Self::ActorSendById,
            "hew_actor_unlink" => Self::ActorUnlink,
            "hew_actor_spawn" => Self::ActorSpawn,
            // Auto-mutex
            "hew_auto_mutex_alloc" => Self::AutoMutexAlloc,
            "hew_auto_mutex_free" => Self::AutoMutexFree,
            "hew_auto_mutex_lock" => Self::AutoMutexLock,
            "hew_auto_mutex_unlock" => Self::AutoMutexUnlock,
            // Bytes
            "hew_bytes_get" => Self::BytesGet,
            "hew_bytes_index" => Self::BytesIndex,
            "hew_bytes_len" => Self::BytesLen,
            "hew_bytes_push" => Self::BytesPush,
            "hew_bytes_slice" => Self::BytesSlice,
            // CancellationToken
            "hew_cancel_token_is_requested" => Self::CancelTokenIsRequested,
            "hew_cancel_token_release" => Self::CancelTokenRelease,
            "hew_cancel_token_retain" => Self::CancelTokenRetain,
            // Channel
            "hew_channel_recv_layout" => Self::ChannelRecvLayout,
            "hew_channel_send_layout" => Self::ChannelSendLayout,
            "hew_channel_try_recv_layout" => Self::ChannelTryRecvLayout,
            "hew_channel_sender_close" => Self::ChannelSenderClose,
            "hew_channel_receiver_close" => Self::ChannelReceiverClose,
            // Duplex
            "hew_duplex_clone" => Self::DuplexClone,
            "hew_duplex_close" => Self::DuplexClose,
            "hew_duplex_close_half" => Self::DuplexCloseHalf,
            "hew_duplex_pair" => Self::DuplexPair,
            "hew_duplex_payload_free" => Self::DuplexPayloadFree,
            "hew_duplex_recv" => Self::DuplexRecv,
            "hew_duplex_recv_half" => Self::DuplexRecvHalf,
            "hew_duplex_send" => Self::DuplexSend,
            "hew_duplex_send_half" => Self::DuplexSendHalf,
            "hew_duplex_try_recv" => Self::DuplexTryRecv,
            "hew_duplex_try_send" => Self::DuplexTrySend,
            // Duration
            "hew_duration_abs" => Self::DurationAbs,
            "hew_duration_hours" => Self::DurationHours,
            "hew_duration_is_zero" => Self::DurationIsZero,
            "hew_duration_micros" => Self::DurationMicros,
            "hew_duration_millis" => Self::DurationMillis,
            "hew_duration_mins" => Self::DurationMins,
            "hew_duration_nanos" => Self::DurationNanos,
            "hew_duration_secs" => Self::DurationSecs,
            // Dyn box
            "hew_dyn_box_alloc" => Self::DynBoxAlloc,
            "hew_dyn_box_free" => Self::DynBoxFree,
            // HashMap
            "hew_hashmap_contains_key_layout" => Self::HashMapContainsKeyLayout,
            "hew_hashmap_free_layout" => Self::HashMapFreeLayout,
            "hew_hashmap_get_layout" => Self::HashMapGetLayout,
            "hew_hashmap_insert_layout" => Self::HashMapInsertLayout,
            "hew_hashmap_keys_layout" => Self::HashMapKeysLayout,
            "hew_hashmap_len_layout" => Self::HashMapLenLayout,
            "HashMap::new" => Self::HashMapNew,
            "hew_hashmap_new_with_layout" => Self::HashMapNewWithLayout,
            "hew_hashmap_remove_layout" => Self::HashMapRemoveLayout,
            "hew_hashmap_values_layout" => Self::HashMapValuesLayout,
            // HashSet
            "hew_hashset_contains_layout" => Self::HashSetContainsLayout,
            "hew_hashset_free_layout" => Self::HashSetFreeLayout,
            "hew_hashset_insert_layout" => Self::HashSetInsertLayout,
            "hew_hashset_is_empty_layout" => Self::HashSetIsEmptyLayout,
            "hew_hashset_len_layout" => Self::HashSetLenLayout,
            "HashSet::new" => Self::HashSetNew,
            "hew_hashset_new_with_layout" => Self::HashSetNewWithLayout,
            "hew_hashset_remove_layout" => Self::HashSetRemoveLayout,
            // Instant
            "hew_instant_duration_since" => Self::InstantDurationSince,
            "hew_instant_elapsed" => Self::InstantElapsed,
            "hew_instant_now" => Self::InstantNow,
            // Lambda actor
            "hew_lambda_actor_ask" => Self::LambdaActorAsk,
            "hew_lambda_body_alloc_reply_buf" => Self::LambdaBodyAllocReplyBuf,
            "hew_lambda_actor_clone" => Self::LambdaActorClone,
            "hew_lambda_actor_downgrade" => Self::LambdaActorDowngrade,
            "hew_lambda_drain_all" => Self::LambdaDrainAll,
            "hew_lambda_actor_new" => Self::LambdaActorNew,
            "hew_lambda_actor_release" => Self::LambdaActorRelease,
            "hew_lambda_actor_send" => Self::LambdaActorSend,
            "hew_lambda_actor_weak_clone" => Self::LambdaActorWeakClone,
            "hew_lambda_actor_weak_drop" => Self::LambdaActorWeakDrop,
            "hew_lambda_actor_weak_send" => Self::LambdaActorWeakSend,
            // Math intrinsics
            "sqrt" => Self::MathIntrinsic(MathIntrinsic::Sqrt),
            "exp" => Self::MathIntrinsic(MathIntrinsic::Exp),
            "log" => Self::MathIntrinsic(MathIntrinsic::Log),
            "sin" => Self::MathIntrinsic(MathIntrinsic::Sin),
            "cos" => Self::MathIntrinsic(MathIntrinsic::Cos),
            "abs" => Self::MathIntrinsic(MathIntrinsic::AbsI64),
            "min" => Self::MathIntrinsic(MathIntrinsic::MinI64),
            "max" => Self::MathIntrinsic(MathIntrinsic::MaxI64),
            "abs_f" => Self::MathIntrinsic(MathIntrinsic::AbsF64),
            "min_f" => Self::MathIntrinsic(MathIntrinsic::MinF64),
            "max_f" => Self::MathIntrinsic(MathIntrinsic::MaxF64),
            "pow" => Self::MathIntrinsic(MathIntrinsic::Pow),
            "floor" => Self::MathIntrinsic(MathIntrinsic::Floor),
            "ceil" => Self::MathIntrinsic(MathIntrinsic::Ceil),
            "round" => Self::MathIntrinsic(MathIntrinsic::Round),
            // Node
            "Node::lookup" => Self::NodeLookup,
            // User metrics (#1862)
            "hew_metric_counter_register" => Self::MetricCounterRegister,
            "hew_metric_counter_inc" => Self::MetricCounterInc,
            "hew_metric_counter_add" => Self::MetricCounterAdd,
            "hew_metric_gauge_register" => Self::MetricGaugeRegister,
            "hew_metric_gauge_set" => Self::MetricGaugeSet,
            "hew_metric_gauge_inc" => Self::MetricGaugeInc,
            "hew_metric_gauge_dec" => Self::MetricGaugeDec,
            "hew_metric_gauge_add" => Self::MetricGaugeAdd,
            "hew_metric_histogram_register" => Self::MetricHistogramRegister,
            "hew_metric_histogram_register_simple" => Self::MetricHistogramRegisterSimple,
            "hew_metric_histogram_record" => Self::MetricHistogramRecord,
            "hew_metric_vec_register" => Self::MetricVecRegister,
            "hew_metric_vec_with" => Self::MetricVecWith,
            // Observe
            "hew_observe_read_u64" => Self::ObserveReadU64,
            "hew_observe_scrape" => Self::ObserveScrape,
            "hew_observe_series" => Self::ObserveSeries,
            "hew_observe_barrier" => Self::ObserveBarrier,
            // Rc
            "hew_rc_new" => Self::RcNew,
            // RecvHalf
            "hew_recv_half_recv" => Self::RecvHalfRecv,
            "hew_recv_half_try_recv" => Self::RecvHalfTryRecv,
            // Regex
            "hew_regex_capture" => Self::RegexCapture,
            "hew_regex_compile" => Self::RegexCompile,
            "hew_regex_free_capture" => Self::RegexFreeCapture,
            "hew_regex_match" => Self::RegexMatch,
            // RemotePid<T>::tell intercept
            "hew_remote_pid_tell" => Self::RemotePidTell,
            // Reply channel
            "hew_reply_channel_cancel" => Self::ReplyChannelCancel,
            "hew_reply_channel_free" => Self::ReplyChannelFree,
            "hew_reply_channel_new" => Self::ReplyChannelNew,
            "hew_reply_payload_free" => Self::ReplyPayloadFree,
            "hew_reply_wait" => Self::ReplyWait,
            // Select
            "hew_select_first" => Self::SelectFirst,
            // SendHalf
            "hew_send_half_send" => Self::SendHalfSend,
            "hew_send_half_try_send" => Self::SendHalfTrySend,
            // Sink
            "hew_sink_close" => Self::SinkClose,
            "hew_sink_write_bytes" => Self::SinkWrite(StreamElementKind::Bytes),
            "hew_sink_write_string" => Self::SinkWrite(StreamElementKind::String),
            "hew_sink_try_write_bytes" => Self::SinkTryWrite(StreamElementKind::Bytes),
            "hew_sink_try_write_string" => Self::SinkTryWrite(StreamElementKind::String),
            // Stream
            "hew_stream_close" => Self::StreamClose,
            "hew_stream_next_layout" => Self::StreamNextLayout,
            "hew_stream_send_layout" => Self::StreamSendLayout,
            "hew_stream_try_next_layout" => Self::StreamTryNextLayout,
            // String
            "hew_string_char_count" => Self::StringCharCount,
            "hew_string_concat" => Self::StringConcat,
            "hew_string_get" => Self::StringGet,
            "hew_string_index" => Self::StringIndex,
            "hew_string_slice_codepoints" => Self::StringSliceCodepoints,
            // Supervisor
            "hew_supervisor_child_get" => Self::SupervisorChildGet,
            "hew_supervisor_nested_get" => Self::SupervisorNestedGet,
            "hew_supervisor_pool_child_get" => Self::SupervisorPoolChildGet,
            "hew_supervisor_pool_len" => Self::SupervisorPoolLen,
            "hew_supervisor_stop" => Self::SupervisorStop,
            "hew_supervisor_restart_await_blocking" => Self::SupervisorRestartAwaitBlocking,
            // TCP attach
            "hew_tcp_attach_local" => Self::TcpAttachLocal,
            // Task
            "hew_task_await_blocking" => Self::TaskAwaitBlocking,
            "hew_task_complete_threaded" => Self::TaskCompleteThreaded,
            "hew_task_completion_observe" => Self::TaskCompletionObserve,
            "hew_task_completion_unobserve" => Self::TaskCompletionUnobserve,
            "hew_task_free" => Self::TaskFree,
            "hew_task_get_env" => Self::TaskGetEnv,
            "hew_task_get_error" => Self::TaskGetError,
            "hew_task_get_result" => Self::TaskGetResult,
            "hew_task_new" => Self::TaskNew,
            "hew_task_scope_cancel_after_ns" => Self::TaskScopeCancelAfterNs,
            "hew_task_scope_destroy" => Self::TaskScopeDestroy,
            "hew_task_scope_join_all" => Self::TaskScopeJoinAll,
            "hew_task_scope_new" => Self::TaskScopeNew,
            "hew_task_scope_set_current" => Self::TaskScopeSetCurrent,
            "hew_task_scope_spawn" => Self::TaskScopeSpawn,
            "hew_task_set_env" => Self::TaskSetEnv,
            "hew_task_set_result" => Self::TaskSetResult,
            "hew_task_spawn_thread" => Self::TaskSpawnThread,
            // Vec
            "hew_vec_get_bool" => Self::VecGet(VecGetElem::Bool),
            "hew_vec_get_f32" => Self::VecGet(VecGetElem::F32),
            "hew_vec_get_f64" => Self::VecGet(VecGetElem::F64),
            "hew_vec_get_i8" => Self::VecGet(VecGetElem::I8),
            "hew_vec_get_i16" => Self::VecGet(VecGetElem::I16),
            "hew_vec_get_i32" => Self::VecGet(VecGetElem::I32),
            "hew_vec_get_i64" => Self::VecGet(VecGetElem::I64),
            "hew_vec_get_layout" => Self::VecGet(VecGetElem::Layout),
            "hew_vec_get_owned" => Self::VecGet(VecGetElem::Owned),
            "hew_vec_get_ptr" => Self::VecGet(VecGetElem::Ptr),
            "hew_vec_get_str" => Self::VecGet(VecGetElem::Str),
            "hew_vec_get_u8" => Self::VecGet(VecGetElem::U8),
            "hew_vec_get_u16" => Self::VecGet(VecGetElem::U16),
            "hew_vec_len" => Self::VecLen,
            "hew_vec_slice_range_bytesize" => Self::VecSliceRange(VecSliceElem::Bytesize),
            "hew_vec_slice_range_f64" => Self::VecSliceRange(VecSliceElem::F64),
            "hew_vec_slice_range_i32" => Self::VecSliceRange(VecSliceElem::I32),
            "hew_vec_slice_range_i64" => Self::VecSliceRange(VecSliceElem::I64),
            "hew_vec_slice_range_layout" => Self::VecSliceRange(VecSliceElem::Layout),
            "hew_vec_slice_range_owned" => Self::VecSliceRange(VecSliceElem::Owned),
            "hew_vec_slice_range_ptr" => Self::VecSliceRange(VecSliceElem::Ptr),
            "hew_vec_slice_range_str" => Self::VecSliceRange(VecSliceElem::Str),
            // Vtable
            "hew_vtable_dispatch_panic_on_oob" => Self::VtableDispatchPanicOnOob,
            _ => return None,
        };
        Some(family)
    }

    /// True iff calling this family consumes the receiver handle (the
    /// runtime entry takes ownership; the caller MUST NOT drop the
    /// handle after the call). Mirrors `runtime_symbol_consumes_receiver`
    /// in `hew-types/src/builtin_names.rs` for the 7-symbol set:
    /// `hew_stream_close`, `hew_sink_close`, `hew_channel_sender_close`,
    /// `hew_channel_receiver_close`, `hew_duplex_close`,
    /// `hew_duplex_close_half`, `hew_lambda_actor_release`.
    ///
    /// LESSONS P0 `boundary-fail-closed`: a missed consume-mark leaks
    /// the handle (drop fires once on a still-live handle) — it never
    /// double-frees. So the safe default for unrecognised cases is
    /// `false` (borrowing), preserved here by the explicit closed-set
    /// listing.
    #[must_use]
    pub fn consumes_receiver(self) -> bool {
        matches!(
            self,
            Self::StreamClose
                | Self::SinkClose
                | Self::ChannelSenderClose
                | Self::ChannelReceiverClose
                | Self::DuplexClose
                | Self::DuplexCloseHalf
                | Self::LambdaActorRelease
        )
    }

    /// Classify the family's async-suspending behaviour, if any.
    ///
    /// Source of truth: the HIR await-classifier at
    /// `hew-hir/src/lower.rs` — `is_stream_send_await`
    /// (`c_symbol == "hew_sink_write_bytes"`) and the duplex-close
    /// await arm (`c_symbol == "hew_duplex_close"`). The suspending
    /// channel/stream recv path rides the element-layout-witness
    /// `*_layout` symbols, which bypass `RuntimeCallFamily` entirely
    /// (HIR string-matches `hew_channel_recv_layout` /
    /// `hew_stream_next_layout` directly).
    ///
    /// The match is exhaustive over [`RuntimeCallFamily`] — there is
    /// no `_ =>` wildcard arm — so a future family variant cannot be
    /// added without explicitly declaring its suspension behaviour
    /// (LESSONS P0 `match-fail-closed` + `exhaustive-traversal-and-lowering`).
    #[must_use]
    #[allow(
        clippy::too_many_lines,
        reason = "exhaustive closed-enum match by design; no `_ =>` arm"
    )]
    pub fn is_async_suspending(self) -> Option<AsyncSuspendKind> {
        use RuntimeCallFamily as F;
        match self {
            // The suspending symbols (HIR await-classifier source of truth).
            F::SinkWrite(StreamElementKind::Bytes) => Some(AsyncSuspendKind::SinkSendBytes),
            F::DuplexClose => Some(AsyncSuspendKind::DuplexClose),
            F::ChannelRecvLayout => Some(AsyncSuspendKind::ChannelRecv),
            F::StreamNextLayout => Some(AsyncSuspendKind::StreamRecv),

            // Everything else: NOT suspending today. Exhaustively listed
            // so adding a new variant requires an explicit decision.
            F::StreamClose
            | F::StreamSendLayout
            | F::StreamTryNextLayout
            | F::SinkWrite(StreamElementKind::String)
            | F::SinkTryWrite(_)
            | F::SinkClose
            | F::ChannelSendLayout
            | F::ChannelTryRecvLayout
            | F::ChannelSenderClose
            | F::ChannelReceiverClose
            | F::DuplexClone
            | F::DuplexCloseHalf
            | F::DuplexPair
            | F::DuplexPayloadFree
            | F::DuplexRecv
            | F::DuplexRecvHalf
            | F::DuplexSend
            | F::DuplexSendHalf
            | F::DuplexTryRecv
            | F::DuplexTrySend
            | F::ActorAsk
            | F::ActorAskWithChannel
            | F::ActorCooperate
            | F::ActorDemonitor
            | F::ActorLink
            | F::ActorMonitor
            | F::ActorSelf
            | F::ActorSendById
            | F::ActorSpawn
            | F::ActorUnlink
            | F::AutoMutexAlloc
            | F::AutoMutexFree
            | F::AutoMutexLock
            | F::AutoMutexUnlock
            | F::BytesGet
            | F::BytesIndex
            | F::BytesLen
            | F::BytesPush
            | F::BytesSlice
            | F::CancelTokenIsRequested
            | F::CancelTokenRelease
            | F::CancelTokenRetain
            | F::DurationAbs
            | F::DurationHours
            | F::DurationIsZero
            | F::DurationMicros
            | F::DurationMillis
            | F::DurationMins
            | F::DurationNanos
            | F::DurationSecs
            | F::DynBoxAlloc
            | F::DynBoxFree
            | F::HashMapContainsKeyLayout
            | F::HashMapFreeLayout
            | F::HashMapGetLayout
            | F::HashMapInsertLayout
            | F::HashMapKeysLayout
            | F::HashMapLenLayout
            | F::HashMapNew
            | F::HashMapNewWithLayout
            | F::HashMapRemoveLayout
            | F::HashMapValuesLayout
            | F::HashSetContainsLayout
            | F::HashSetFreeLayout
            | F::HashSetInsertLayout
            | F::HashSetIsEmptyLayout
            | F::HashSetLenLayout
            | F::HashSetNew
            | F::HashSetNewWithLayout
            | F::HashSetRemoveLayout
            | F::InstantDurationSince
            | F::InstantElapsed
            | F::InstantNow
            | F::LambdaActorAsk
            | F::LambdaBodyAllocReplyBuf
            | F::LambdaActorClone
            | F::LambdaActorDowngrade
            | F::LambdaDrainAll
            | F::LambdaActorNew
            | F::LambdaActorRelease
            | F::LambdaActorSend
            | F::LambdaActorWeakClone
            | F::LambdaActorWeakDrop
            | F::LambdaActorWeakSend
            | F::MathIntrinsic(_)
            | F::MetricCounterRegister
            | F::MetricCounterInc
            | F::MetricCounterAdd
            | F::MetricGaugeRegister
            | F::MetricGaugeSet
            | F::MetricGaugeInc
            | F::MetricGaugeDec
            | F::MetricGaugeAdd
            | F::MetricHistogramRegister
            | F::MetricHistogramRegisterSimple
            | F::MetricHistogramRecord
            | F::MetricVecRegister
            | F::MetricVecWith
            | F::NodeLookup
            | F::ObserveReadU64
            | F::ObserveScrape
            | F::ObserveSeries
            | F::ObserveBarrier
            | F::RcNew
            | F::RecvHalfRecv
            | F::RecvHalfTryRecv
            | F::RegexCapture
            | F::RegexCompile
            | F::RegexFreeCapture
            | F::RegexMatch
            | F::RemotePidTell
            | F::ReplyChannelCancel
            | F::ReplyChannelFree
            | F::ReplyChannelNew
            | F::ReplyPayloadFree
            | F::ReplyWait
            | F::SelectFirst
            | F::SendHalfSend
            | F::SendHalfTrySend
            | F::StringCharCount
            | F::StringConcat
            | F::StringGet
            | F::StringIndex
            | F::StringSliceCodepoints
            | F::SupervisorChildGet
            | F::SupervisorNestedGet
            | F::SupervisorPoolChildGet
            | F::SupervisorPoolLen
            | F::SupervisorStop
            | F::SupervisorRestartAwaitBlocking
            | F::TcpAttachLocal
            | F::TaskAwaitBlocking
            | F::TaskCompleteThreaded
            | F::TaskCompletionObserve
            | F::TaskCompletionUnobserve
            | F::TaskFree
            | F::TaskGetEnv
            | F::TaskGetError
            | F::TaskGetResult
            | F::TaskNew
            | F::TaskScopeCancelAfterNs
            | F::TaskScopeDestroy
            | F::TaskScopeJoinAll
            | F::TaskScopeNew
            | F::TaskScopeSetCurrent
            | F::TaskScopeSpawn
            | F::TaskSetEnv
            | F::TaskSetResult
            | F::TaskSpawnThread
            | F::VecGet(_)
            | F::VecLen
            | F::VecSliceRange(_)
            | F::VtableDispatchPanicOnOob => None,
        }
    }

    /// True iff the family's variant carries no element-type degree of
    /// freedom beyond the variant itself (i.e. the descriptor's `elem`
    /// field MUST be `None` for this family). Used by
    /// [`RuntimeCallDescriptor::new`] to enforce fail-closed
    /// construction.
    ///
    /// Today, EVERY variant returns `false`-meaning-"forbids-elem"
    /// because every element-type discriminant is embedded in the
    /// variant payload (the descriptor's optional `elem` field is
    /// pre-staged for the pending Vec genericisation work). When a
    /// `VecGetGeneric` variant lands, that one variant returns `true`
    /// here.
    #[must_use]
    #[allow(
        clippy::unused_self,
        reason = "substrate: every variant currently forbids a separate \
                                                `ResolvedTy` elem. When the pending genericisation work's first generic variant \
                                                lands (e.g. `VecGetGeneric`), this becomes a real per-variant match."
    )]
    fn accepts_elem(self) -> bool {
        // Closed-set fail-closed: no current variant accepts a separate
        // `ResolvedTy` elem; the pending genericisation work is where
        // this opens up. The future genericisation is additive:
        // a new `VecGetGeneric` variant returns `true` here and the
        // constructor admits `Some(elem)` for it without changing the
        // bijection invariant for the closed-set variants.
        false
    }
}

/// Async-suspending behaviour classification. One variant per HIR
/// await-classifier flavour in `hew-hir/src/lower.rs` that still flows
/// through `RuntimeCallFamily`: `is_stream_send_await` and the
/// duplex-close await arm. The channel/stream recv awaits ride the
/// element-layout-witness `*_layout` symbols outside this enum.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AsyncSuspendKind {
    /// `await sink.send(x)` over `Sink<bytes>` → `hew_sink_write_bytes`.
    SinkSendBytes,
    /// `await actor.close()` over a lambda-actor `Duplex` →
    /// `hew_duplex_close`.
    DuplexClose,
    /// `await rx.recv()` over a `std::channel` `Receiver<T>` →
    /// `hew_channel_recv_layout`. Suspends only in execution-context
    /// callers; a context-free caller keeps the blocking call.
    ChannelRecv,
    /// `await stream.recv()` over a `Stream<T>` →
    /// `hew_stream_next_layout`. Same context gating as `ChannelRecv`.
    StreamRecv,
}

// =============================================================================
// RuntimeCallDescriptor — typed cross-layer carrier
// =============================================================================

/// Typed cross-layer carrier for a compiler-known runtime / builtin call.
///
/// Fields are private and access is mediated by accessor methods so
/// construction is only possible via [`RuntimeCallDescriptor::new`],
/// which enforces the family↔elem consistency invariant. LESSONS P0
/// `boundary-fail-closed`: invalid combinations refuse to construct in
/// every build profile.
///
/// The `elem` field is pre-staged for the pending Vec/HashMap
/// genericisation work; today every legal `(family, elem)`
/// has `elem = None` and the embedded discriminator in the variant
/// payload (e.g. [`VecGetElem`]) carries the element identity.
///
/// Equality is value-equality (derive); hashing is fine because all
/// fields are hashable. Cloning is cheap (`Copy` on the family enum;
/// `ResolvedTy::Clone` for the elem if populated).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RuntimeCallDescriptor {
    family: RuntimeCallFamily,
    elem: Option<ResolvedTy>,
}

impl RuntimeCallDescriptor {
    /// Construct a validated descriptor.
    ///
    /// # Errors
    ///
    /// Returns [`DescriptorError::UnexpectedElem`] when `elem` is `Some`
    /// for a family whose variant payload already encodes the element
    /// identity. This is the substrate's fail-closed guarantee: a future
    /// producer that incorrectly threads a `ResolvedTy` through to a
    /// non-generic family will hard-fail at construction rather than
    /// silently store the unused elem.
    ///
    /// When the pending genericisation work lands its first
    /// element-type-generic variant (e.g. `VecGetGeneric`), that
    /// variant will return `true` from
    /// `RuntimeCallFamily::accepts_elem` and the constructor will
    /// require `Some(elem)` for it (symmetric fail-closed).
    pub fn new(
        family: RuntimeCallFamily,
        elem: Option<ResolvedTy>,
    ) -> Result<Self, DescriptorError> {
        match (family.accepts_elem(), &elem) {
            (false, Some(_)) => Err(DescriptorError::UnexpectedElem { family }),
            (true, None) => Err(DescriptorError::MissingElem { family }),
            _ => Ok(Self { family, elem }),
        }
    }

    /// The family discriminator.
    #[must_use]
    pub fn family(&self) -> RuntimeCallFamily {
        self.family
    }

    /// The checker-resolved element type, if the family accepts one.
    #[must_use]
    pub fn elem(&self) -> Option<&ResolvedTy> {
        self.elem.as_ref()
    }

    /// The C-ABI symbol the family lowers to. Delegates to
    /// [`RuntimeCallFamily::c_symbol`].
    #[must_use]
    pub fn c_symbol(&self) -> &'static str {
        self.family.c_symbol()
    }

    /// True iff calling this descriptor consumes the receiver handle.
    /// Delegates to [`RuntimeCallFamily::consumes_receiver`].
    #[must_use]
    pub fn consumes_receiver(&self) -> bool {
        self.family.consumes_receiver()
    }

    /// Async-suspending classification. Delegates to
    /// [`RuntimeCallFamily::is_async_suspending`].
    #[must_use]
    pub fn is_async_suspending(&self) -> Option<AsyncSuspendKind> {
        self.family.is_async_suspending()
    }
}

/// Construction error for [`RuntimeCallDescriptor::new`].
///
/// Carries the offending family so callers can emit diagnostics naming
/// the exact misuse (`MirDiagnosticKind::NotYetImplemented`, codegen
/// assertions, unit-test assertions).
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DescriptorError {
    /// `elem` is `Some` but the family's variant payload already encodes
    /// the element identity (the elem field is meaningless / a fail-
    /// closed signal of misuse).
    UnexpectedElem { family: RuntimeCallFamily },
    /// `elem` is `None` but the family is element-type-generic and the
    /// elem MUST be supplied. Reserved for the pending genericisation
    /// work; no current family triggers this arm.
    MissingElem { family: RuntimeCallFamily },
}

impl std::fmt::Display for DescriptorError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnexpectedElem { family } => write!(
                f,
                "RuntimeCallDescriptor: family {family:?} does not accept an `elem`; \
                 the variant payload already encodes the element identity",
            ),
            Self::MissingElem { family } => write!(
                f,
                "RuntimeCallDescriptor: family {family:?} is element-type-generic and \
                 requires `Some(elem)`",
            ),
        }
    }
}

impl std::error::Error for DescriptorError {}

// =============================================================================
// RuntimeDropDescriptor — typed mirror of runtime_drop_symbol's table
// =============================================================================

/// Closed-set descriptor for compiler-known runtime drop entries. Mirrors
/// the `runtime_drop_symbol` table in `hew-codegen-rs/src/llvm.rs:18352`
/// (today: `Duplex::close`, `Stream::close`, `Sink::close`,
/// `Sender::close`, `Receiver::close`, `LambdaActorHandle::close`,
/// `SendHalf::close | RecvHalf::close`, `CancellationToken::release`).
///
/// `non_exhaustive` is INTENTIONALLY OMITTED — same exhaustiveness
/// argument as [`RuntimeCallFamily`]. The codegen-internal "literal
/// C-ABI symbol pass-through" arms in `runtime_drop_symbol` (for hand-
/// built test MIR that pre-dates elaborated-drop-plan consumption)
/// are NOT mirrored here; a follow-up migrates the test MIR sites to typed
/// variants and the pass-through dies with the string field.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RuntimeDropDescriptor {
    /// `Duplex::close` → `hew_duplex_close`. Two-way close.
    DuplexClose,
    /// `Stream::close` → `hew_stream_close`. Element-type-independent
    /// at the ABI level; the type checker's builtin-method table emits
    /// the same `drop_fn` regardless of element type.
    StreamClose,
    /// `Sink::close` → `hew_sink_close`.
    SinkClose,
    /// `Sender::close` → `hew_channel_sender_close`.
    SenderClose,
    /// `Receiver::close` → `hew_channel_receiver_close`.
    ReceiverClose,
    /// `LambdaActorHandle::close` → `hew_lambda_actor_release`. NB the
    /// type-class seeding calls the method "close" but the runtime
    /// C-ABI symbol is "release"; an explicit table entry is required.
    LambdaActorHandleClose,
    /// `SendHalf::close` → `hew_duplex_close_half`. Direction
    /// discriminant materialised at the call site from the Place
    /// variant (`SendHalf` vs `RecvHalf`), not encoded in the symbol.
    SendHalfClose,
    /// `RecvHalf::close` → `hew_duplex_close_half`. Shares C-symbol with
    /// `SendHalfClose`; the two descriptor variants exist to preserve
    /// the typed direction information cross-layer.
    RecvHalfClose,
    /// `CancellationToken::release` → `hew_cancel_token_release`.
    CancellationTokenRelease,
    /// `MonitorRef::close` → `hew_actor_demonitor`. Extracts `ref_id: i64`
    /// from the struct and passes it directly to the runtime.
    MonitorRefClose,
}

impl RuntimeDropDescriptor {
    /// The C-ABI runtime symbol the drop lowers to. NB
    /// [`RuntimeDropDescriptor::SendHalfClose`] and
    /// [`RuntimeDropDescriptor::RecvHalfClose`] share
    /// `hew_duplex_close_half`; this is intentional — the bijection is
    /// over (drop descriptor → symbol), not (symbol → descriptor).
    #[must_use]
    pub fn c_symbol(self) -> &'static str {
        match self {
            Self::DuplexClose => "hew_duplex_close",
            Self::StreamClose => "hew_stream_close",
            Self::SinkClose => "hew_sink_close",
            Self::SenderClose => "hew_channel_sender_close",
            Self::ReceiverClose => "hew_channel_receiver_close",
            Self::LambdaActorHandleClose => "hew_lambda_actor_release",
            Self::SendHalfClose | Self::RecvHalfClose => "hew_duplex_close_half",
            Self::CancellationTokenRelease => "hew_cancel_token_release",
            Self::MonitorRefClose => "hew_actor_demonitor",
        }
    }

    /// The producer-side method-name spelling (`<Type>::<method>`), the
    /// round-trip key of the descriptor: unlike `c_symbol()` (where the
    /// two half-close variants share a symbol), every variant has a
    /// unique name, so [`RuntimeDropDescriptor::from_drop_fn_name`] is a
    /// true inverse.
    #[must_use]
    pub fn drop_fn_name(self) -> &'static str {
        match self {
            Self::DuplexClose => "Duplex::close",
            Self::StreamClose => "Stream::close",
            Self::SinkClose => "Sink::close",
            Self::SenderClose => "Sender::close",
            Self::ReceiverClose => "Receiver::close",
            Self::LambdaActorHandleClose => "LambdaActorHandle::close",
            Self::SendHalfClose => "SendHalf::close",
            Self::RecvHalfClose => "RecvHalf::close",
            Self::CancellationTokenRelease => "CancellationToken::release",
            Self::MonitorRefClose => "MonitorRef::close",
        }
    }

    /// Inverse of [`RuntimeDropDescriptor::drop_fn_name`]: lift a
    /// type-class-derived `<Type>::<method>` close name into the typed
    /// descriptor. Returns `None` for user `#[resource]` close methods
    /// (`MyType::close`) — the open-set arm of the drop-dispatch split.
    /// MIR's drop elaboration uses this lift to classify every close
    /// ritual at production; the `c_symbol` is then born at codegen from
    /// the descriptor.
    #[must_use]
    pub fn from_drop_fn_name(name: &str) -> Option<Self> {
        match name {
            "Duplex::close" => Some(Self::DuplexClose),
            "Stream::close" => Some(Self::StreamClose),
            "Sink::close" => Some(Self::SinkClose),
            "Sender::close" => Some(Self::SenderClose),
            "Receiver::close" => Some(Self::ReceiverClose),
            "LambdaActorHandle::close" => Some(Self::LambdaActorHandleClose),
            "SendHalf::close" => Some(Self::SendHalfClose),
            "RecvHalf::close" => Some(Self::RecvHalfClose),
            "CancellationToken::release" => Some(Self::CancellationTokenRelease),
            "MonitorRef::close" => Some(Self::MonitorRefClose),
            _ => None,
        }
    }
}

// =============================================================================
// Test-only enumeration helpers
// =============================================================================

/// Enumerate every legal [`RuntimeCallFamily`] value — compiler-complete.
///
/// The outer variant set is sourced from the `EnumIter` derive, so a new
/// `RuntimeCallFamily` variant is enumerated automatically and cannot be
/// silently dropped from the catalog. The no-wildcard `match` below expands
/// each payload-bearing variant to its full cross-product via the inner
/// enum's own `EnumIter`; adding a payload variant makes that match
/// non-exhaustive (a compile error) until it is expanded here.
///
/// This is what closes focal-7's pre-staged-family gap: a family that rides
/// `Terminator::Call` (its `c_symbol()` absent from
/// `MIR_EMITTER_RUNTIME_SYMBOLS`, so untouched by
/// `every_allowlist_symbol_has_a_family`) is still counted by this catalog,
/// hence by the corpus-coverage and bijection tests below. No hand-maintained
/// row to forget.
///
/// The bijection / round-trip tests in `tests` below and in
/// `hew-mir/tests/runtime_call_allowlist.rs` iterate this list to assert
/// every variant has a unique `c_symbol()` and (where applicable) lies in
/// `MIR_EMITTER_RUNTIME_SYMBOLS`.
#[must_use]
pub fn all_runtime_call_families() -> Vec<RuntimeCallFamily> {
    use RuntimeCallFamily as F;
    let mut out = Vec::new();
    for repr in F::iter() {
        match repr {
            F::MathIntrinsic(_) => out.extend(MathIntrinsic::iter().map(F::MathIntrinsic)),
            F::SinkWrite(_) => out.extend(StreamElementKind::iter().map(F::SinkWrite)),
            F::SinkTryWrite(_) => out.extend(StreamElementKind::iter().map(F::SinkTryWrite)),
            F::VecGet(_) => out.extend(VecGetElem::iter().map(F::VecGet)),
            F::VecSliceRange(_) => out.extend(VecSliceElem::iter().map(F::VecSliceRange)),
            // Nullary variants carry no payload: emit `EnumIter`'s single
            // representative as-is.
            nullary => out.push(nullary),
        }
    }
    out
}

/// Enumerate every legal [`RuntimeDropDescriptor`] variant.
///
/// See the bijection / parity tests; same coverage discipline as
/// [`all_runtime_call_families`].
#[must_use]
pub fn all_runtime_drop_descriptors() -> [RuntimeDropDescriptor; 10] {
    [
        RuntimeDropDescriptor::DuplexClose,
        RuntimeDropDescriptor::StreamClose,
        RuntimeDropDescriptor::SinkClose,
        RuntimeDropDescriptor::SenderClose,
        RuntimeDropDescriptor::ReceiverClose,
        RuntimeDropDescriptor::LambdaActorHandleClose,
        RuntimeDropDescriptor::SendHalfClose,
        RuntimeDropDescriptor::RecvHalfClose,
        RuntimeDropDescriptor::CancellationTokenRelease,
        RuntimeDropDescriptor::MonitorRefClose,
    ]
}

/// Families pre-staged (Channel, Stream, Sink, `Node::lookup`,
/// math intrinsics, `RemotePidTell`, `RemoteActorAsk`, `TcpAttachLocal`,
/// the `HashMap::new` / `HashSet::new` constructor surface forms, and
/// the `keys()` / `values()` projection ops).
/// Their `c_symbol()` is NOT in `MIR_EMITTER_RUNTIME_SYMBOLS` today
/// because they go through `Terminator::Call` callee-name intercepts
/// (codegen callee-name intercept), not `Instr::CallRuntimeAbi`. The bijection test
/// explicitly excludes them from the allowlist-coverage assertion.
///
/// When the follow-up wires the producers, the symbols join the allowlist and
/// this list shrinks.
#[must_use]
pub fn is_pre_staged_family(family: RuntimeCallFamily) -> bool {
    use RuntimeCallFamily as F;
    matches!(
        family,
        F::ChannelRecvLayout
            | F::ChannelSendLayout
            | F::ChannelTryRecvLayout
            | F::ChannelSenderClose
            | F::ChannelReceiverClose
            | F::HashMapKeysLayout
            | F::HashMapNew
            | F::HashMapValuesLayout
            | F::HashSetNew
            | F::MathIntrinsic(_)
            | F::NodeLookup
            | F::RemotePidTell
            | F::SinkClose
            | F::SinkWrite(_)
            | F::SinkTryWrite(_)
            | F::StreamClose
            | F::StreamNextLayout
            | F::StreamSendLayout
            | F::StreamTryNextLayout
            | F::TcpAttachLocal
    )
}

// =============================================================================
// Tests
// =============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;

    /// Bijection (forward): every family variant produces a unique
    /// `c_symbol()`. No two variants collide. This is the substrate's
    /// load-bearing correctness anchor — adding two variants that
    /// happen to lower to the same symbol breaks the round-trip and
    /// fails the test loudly.
    ///
    /// One subtle exception is allowed at the drop-descriptor side
    /// (`SendHalfClose` / `RecvHalfClose` both → `hew_duplex_close_half`)
    /// — that bijection is the drop-side test below, where the round-
    /// trip key is `drop_fn_name()`, not `c_symbol()`.
    #[test]
    fn runtime_call_family_c_symbol_is_unique() {
        let mut seen: HashMap<&'static str, RuntimeCallFamily> = HashMap::new();
        for family in all_runtime_call_families() {
            let sym = family.c_symbol();
            if let Some(prev) = seen.insert(sym, family) {
                panic!(
                    "c_symbol collision: both {prev:?} and {family:?} \
                     produce {sym:?} — the bijection requires every variant \
                     to map to a unique symbol"
                );
            }
        }
    }

    /// Bijection (inverse): every family variant round-trips through
    /// `from_c_symbol(family.c_symbol()) == Some(family)`. A new
    /// variant that adds a `c_symbol` arm but forgets the matching
    /// inverse arm fires here. This is the load-bearing invariant for
    /// the seam-#1 producer: `record_runtime_method_call_rewrite` uses
    /// `from_c_symbol` to lift a checker-resolved C symbol back into a
    /// typed descriptor.
    #[test]
    fn runtime_call_family_round_trips_through_from_c_symbol() {
        for family in all_runtime_call_families() {
            let sym = family.c_symbol();
            let back = RuntimeCallFamily::from_c_symbol(sym);
            assert_eq!(
                back,
                Some(family),
                "from_c_symbol({sym:?}) returned {back:?}, expected Some({family:?}) \
                 — the inverse arm is missing or wrong",
            );
        }
    }

    /// `from_c_symbol` returns `None` for strings the catalog does
    /// not enumerate (open-set extern FFI symbols, user-trait method
    /// keys like `i64::fmt`, garbage).
    #[test]
    fn from_c_symbol_rejects_unknown_strings() {
        assert!(RuntimeCallFamily::from_c_symbol("not_a_runtime_symbol").is_none());
        assert!(RuntimeCallFamily::from_c_symbol("").is_none());
        // User-trait method keys live in `RewriteToFunction.c_symbol` as
        // `Type::method` strings; they are open-set and MUST be rejected
        // so the typed-descriptor path leaves them alone.
        assert!(RuntimeCallFamily::from_c_symbol("i64::fmt").is_none());
        assert!(RuntimeCallFamily::from_c_symbol("MyType::greet").is_none());
    }

    /// `consumes_receiver` is anchored by an INDEPENDENT hard-coded
    /// 7-symbol set. `builtin_names::runtime_symbol_consumes_receiver`
    /// now delegates to this catalog, so asserting against that function
    /// would be circular — the literal set below is the sole external
    /// anchor: a close family losing its consume mark (leak) or a
    /// non-close family gaining one (double-free) fails here.
    #[test]
    fn consumes_receiver_mirrors_builtin_names() {
        use std::collections::HashSet;
        // The canonical 7-set, written out literally (NOT derived from
        // any function under test).
        let expected: HashSet<&'static str> = [
            "hew_stream_close",
            "hew_sink_close",
            "hew_channel_sender_close",
            "hew_channel_receiver_close",
            "hew_duplex_close",
            "hew_duplex_close_half",
            "hew_lambda_actor_release",
        ]
        .into_iter()
        .collect();

        for family in all_runtime_call_families() {
            let sym = family.c_symbol();
            let consumes = family.consumes_receiver();
            let expected_consumes = expected.contains(sym);
            assert_eq!(
                consumes, expected_consumes,
                "consumes_receiver mismatch for {family:?} → {sym}: \
                 descriptor says {consumes}, the hard-coded anchor says \
                 {expected_consumes}"
            );
        }
    }

    /// `is_async_suspending` returns `Some(_)` for EXACTLY the symbols
    /// the HIR await-classifier discriminates through `RuntimeCallFamily`
    /// today: `hew_sink_write_bytes` and `hew_duplex_close`. The
    /// channel/stream recv awaits ride the layout-witness `*_layout`
    /// symbols outside this enum.
    /// Locks the consumer contract for the eventual migration.
    ///
    /// Positive: the symbols listed map to the matching
    /// `AsyncSuspendKind`. Negative: every other family returns `None`,
    /// pinned by enumeration via `all_runtime_call_families`. ESP. the
    /// historical mis-classifications (`DuplexRecv`, `DuplexSend`,
    /// `SinkWrite(String)`, every `try_*` peer) MUST be non-suspending.
    /// The channel/stream recv awaits ride the `*_layout` symbols
    /// outside `RuntimeCallFamily` and are pinned by the HIR
    /// await-classifier tests instead.
    #[test]
    fn is_async_suspending_pins_exact_classifier_set() {
        use RuntimeCallFamily as F;

        // Positive: exactly these (family, expected kind) tuples.
        let positives: &[(RuntimeCallFamily, AsyncSuspendKind)] = &[
            (
                F::SinkWrite(StreamElementKind::Bytes),
                AsyncSuspendKind::SinkSendBytes,
            ),
            (F::DuplexClose, AsyncSuspendKind::DuplexClose),
            (F::ChannelRecvLayout, AsyncSuspendKind::ChannelRecv),
            (F::StreamNextLayout, AsyncSuspendKind::StreamRecv),
        ];
        for (family, kind) in positives {
            assert_eq!(
                family.is_async_suspending(),
                Some(*kind),
                "{family:?} must suspend with {kind:?}",
            );
        }

        // Explicit negative regression set: the families a previous
        // revision wrongly marked as suspending (DuplexRecv/Send,
        // SinkWrite(String)) plus the try_* peers the classifier never
        // touches.
        let must_not_suspend: &[RuntimeCallFamily] = &[
            F::ChannelTryRecvLayout,
            F::ChannelSendLayout,
            F::StreamTryNextLayout,
            F::StreamSendLayout,
            F::DuplexRecv,
            F::DuplexSend,
            F::DuplexTryRecv,
            F::DuplexTrySend,
            F::SinkWrite(StreamElementKind::String),
            F::SinkTryWrite(StreamElementKind::Bytes),
            F::SinkTryWrite(StreamElementKind::String),
            F::StreamClose,
            F::SinkClose,
            F::DuplexCloseHalf,
        ];
        for family in must_not_suspend {
            assert_eq!(
                family.is_async_suspending(),
                None,
                "{family:?} must NOT suspend per the current HIR await-classifier",
            );
        }

        // Coverage: walking the full family enumeration, the size of
        // the suspending set is exactly 4 (sink bytes write, duplex
        // close, channel recv, stream recv). Adding a new family without a corresponding
        // decision is caught by the exhaustive match in
        // `is_async_suspending`; the count assertion below is the belt
        // to that suspenders.
        let suspending_count = all_runtime_call_families()
            .into_iter()
            .filter(|f| f.is_async_suspending().is_some())
            .count();
        assert_eq!(
            suspending_count, 4,
            "exactly 4 families should be suspending today (sink bytes \
             write, duplex close, channel recv, stream recv); declare any \
             new suspending family explicitly in is_async_suspending"
        );
    }

    /// Fail-closed constructor: passing `Some(elem)` to a family that
    /// does not accept an element-type degree of freedom returns
    /// `Err(UnexpectedElem)`. Pins the substrate's "no silent default"
    /// guarantee.
    #[test]
    fn descriptor_new_rejects_unexpected_elem() {
        let err = RuntimeCallDescriptor::new(RuntimeCallFamily::VecLen, Some(ResolvedTy::I64))
            .expect_err("VecLen must reject Some(elem)");
        assert!(matches!(err, DescriptorError::UnexpectedElem { .. }));

        let err =
            RuntimeCallDescriptor::new(RuntimeCallFamily::DuplexClose, Some(ResolvedTy::Bool))
                .expect_err("DuplexClose must reject Some(elem)");
        assert!(matches!(err, DescriptorError::UnexpectedElem { .. }));

        // Symmetric: every variant accepts `None`.
        for family in all_runtime_call_families() {
            RuntimeCallDescriptor::new(family, None).unwrap_or_else(|e| {
                panic!("descriptor with elem=None must construct for {family:?}: {e}")
            });
        }
    }

    /// The descriptor's accessor methods delegate to the family;
    /// confirm the wire works for one representative variant from
    /// each axis.
    #[test]
    fn descriptor_accessors_delegate_to_family() {
        let d = RuntimeCallDescriptor::new(RuntimeCallFamily::DuplexClose, None).unwrap();
        assert_eq!(d.family(), RuntimeCallFamily::DuplexClose);
        assert_eq!(d.elem(), None);
        assert_eq!(d.c_symbol(), "hew_duplex_close");
        assert!(d.consumes_receiver());
        // DuplexClose is one of the suspending classifier symbols.
        assert_eq!(d.is_async_suspending(), Some(AsyncSuspendKind::DuplexClose));

        // Non-suspending close peer: StreamClose / SinkClose are NOT
        // in the await-classifier set.
        let d = RuntimeCallDescriptor::new(RuntimeCallFamily::StreamClose, None).unwrap();
        assert_eq!(d.c_symbol(), "hew_stream_close");
        assert!(d.consumes_receiver());
        assert_eq!(d.is_async_suspending(), None);

        let d = RuntimeCallDescriptor::new(RuntimeCallFamily::VecLen, None).unwrap();
        assert_eq!(d.c_symbol(), "hew_vec_len");
        assert!(!d.consumes_receiver());
        assert_eq!(d.is_async_suspending(), None);
    }

    // -------------------------------------------------------------------------
    // RuntimeDropDescriptor
    // -------------------------------------------------------------------------

    /// Round-trip parity with `runtime_drop_symbol`'s table in
    /// `hew-codegen-rs/src/llvm.rs:18352`. Hard-coded mirror; a follow-up
    /// migration deletes the string-keyed table and reads
    /// `RuntimeDropDescriptor::c_symbol()` directly.
    #[test]
    fn drop_descriptor_c_symbols_match_codegen_table() {
        use std::collections::HashSet;
        // Mirror of the runtime_drop_symbol table (drop_fn_name → C symbol).
        // Listed here so a future change to either side fails this test
        // loudly (substrate-tests-the-substrate).
        let expected: &[(&str, &str)] = &[
            ("Duplex::close", "hew_duplex_close"),
            ("Stream::close", "hew_stream_close"),
            ("Sink::close", "hew_sink_close"),
            ("Sender::close", "hew_channel_sender_close"),
            ("Receiver::close", "hew_channel_receiver_close"),
            ("LambdaActorHandle::close", "hew_lambda_actor_release"),
            ("SendHalf::close", "hew_duplex_close_half"),
            ("RecvHalf::close", "hew_duplex_close_half"),
            ("CancellationToken::release", "hew_cancel_token_release"),
            ("MonitorRef::close", "hew_actor_demonitor"),
        ];
        let mut by_name: HashMap<&'static str, RuntimeDropDescriptor> = HashMap::new();
        for d in all_runtime_drop_descriptors() {
            by_name.insert(d.drop_fn_name(), d);
        }
        for (name, sym) in expected {
            let d = by_name
                .get(name)
                .unwrap_or_else(|| panic!("missing RuntimeDropDescriptor for {name}"));
            assert_eq!(
                d.c_symbol(),
                *sym,
                "drop descriptor {d:?} c_symbol mismatch"
            );
        }
        // No extra descriptors (the inverse direction): every variant we
        // enumerated must be in the expected table. If a future
        // contributor adds a variant without updating the expected table
        // (and the codegen `runtime_drop_symbol` table), this fires.
        let expected_names: HashSet<&'static str> = expected.iter().map(|(n, _)| *n).collect();
        for d in all_runtime_drop_descriptors() {
            assert!(
                expected_names.contains(d.drop_fn_name()),
                "RuntimeDropDescriptor {d:?} has no entry in the \
                 codegen `runtime_drop_symbol` parity table; add it to \
                 both or remove the variant"
            );
        }
    }

    /// `from_drop_fn_name` is a true inverse of `drop_fn_name` (every
    /// variant has a unique name), and rejects user `<Type>::close`
    /// spellings so the open-set arm stays open.
    #[test]
    fn drop_descriptor_name_round_trips() {
        for d in all_runtime_drop_descriptors() {
            assert_eq!(
                RuntimeDropDescriptor::from_drop_fn_name(d.drop_fn_name()),
                Some(d),
                "drop_fn_name round-trip failed for {d:?}"
            );
        }
        assert!(RuntimeDropDescriptor::from_drop_fn_name("MyType::close").is_none());
        assert!(RuntimeDropDescriptor::from_drop_fn_name("hew_duplex_close").is_none());
        assert!(RuntimeDropDescriptor::from_drop_fn_name("").is_none());
    }

    // The allowlist-coverage parity tests
    // (`allowlist_subset_round_trips`, `every_allowlist_symbol_has_a_family`,
    // `drop_descriptor_symbols_in_allowlist_or_pre_staged`, and
    // `every_c_symbol_resolves_to_a_real_symbol`) require
    // `hew_mir::runtime_symbols::is_known_runtime_symbol`, which lives
    // in `hew-mir`. They moved to `hew-mir/tests/runtime_call_allowlist.rs`
    // alongside the re-export shim and run as integration tests against
    // the same substrate.
}
