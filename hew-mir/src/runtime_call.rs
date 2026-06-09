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
//! The bijection test in [`tests`] is the load-bearing invariant: every
//! symbol in [`crate::runtime_symbols::known_runtime_symbols`] maps to
//! exactly one [`RuntimeCallDescriptor`] and back. Adding a new
//! `MIR_EMITTER_RUNTIME_SYMBOLS` entry without adding the corresponding
//! [`RuntimeCallFamily`] variant breaks the test; adding a descriptor
//! whose `c_symbol()` is not in the allowlist (and is not on the
//! explicit "pre-staged, codegen-intercept" list) also breaks it.
//! That is the entire point.
//!
//! # Scope-out
//!
//! - NO consumer wires the descriptor yet.
//! - NO producer emits it yet.
//! - NO field changes on `MethodCallRewrite`, `RuntimeCall`, `Instr::Drop`.
//! - NO `ResolvedRef::Builtin` variant.
//!
//! # Asymmetric split discipline
//!
//! [`RuntimeCallFamily`] is closed. The sole legitimate open-set string
//! in the cross-layer descriptor world is
//! `MethodCallRewrite::RewriteModuleQualifiedToFunction.c_symbol`, which
//! carries a user-module-qualified dotted-name. That string is NOT
//! covered by this substrate by design — it is structurally open-set
//! and clippy-gated.

use hew_types::ResolvedTy;

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
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum VecGetElem {
    Bool,
    F64,
    I32,
    I64,
    Layout,
    Owned,
    Ptr,
    Str,
}

/// Element-type discriminator for `Vec<T>::slice_range` runtime entries
/// (`hew_vec_slice_range_*`). Narrower than [`VecGetElem`] because slice
/// does not have a bool path today and the layout/owned paths are not
/// yet wired in the runtime.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum VecSliceElem {
    F64,
    I32,
    I64,
    Ptr,
    Str,
}

/// Numeric width discriminator for `Option<T>` / `Result<T, E>` unwrap
/// runtime helpers (`hew_option_unwrap_*`, `hew_result_unwrap_*`).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum NumericElem {
    F64,
    I32,
    I64,
}

/// Integer width discriminator for the `unwrap_or` family
/// (`hew_result_unwrap_or_*` is i32/i64 only today; `hew_option_unwrap_or_*`
/// is f64/i32/i64).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IntegerElem {
    I32,
    I64,
}

/// Element-kind discriminator for the Stream/Sink families
/// (`hew_stream_next_bytes` vs `hew_stream_next_string`,
/// `hew_sink_write_bytes` vs `hew_sink_write_string`). The substrate
/// admits both kinds; today only the bytes path is producer-emitted
/// via the `Terminator::Call` intercept; a follow-up commit migrates that path onto this descriptor.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum StreamElementKind {
    Bytes,
    String,
}

/// Element-kind discriminator for `std::channel` `recv`/`try_recv` variants
/// (`hew_channel_recv`, `hew_channel_recv_int`, `hew_channel_try_recv`,
/// `hew_channel_try_recv_int`). Today only `String` (`hew_channel_recv`)
/// and `Int` (`hew_channel_recv_int`) are wired in codegen's
/// `Terminator::Call` intercept; bytes is reserved for parity with
/// Stream/Sink.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ChannelElementKind {
    Int,
    String,
}

/// Math-intrinsic family discriminator. Mirrors the user-visible
/// callee names dispatched in `hew-codegen-rs::llvm::math_builtin_intrinsic`
/// today (the names appear as `BindingRef.name` in HIR — `"sqrt"`, `"sin"`,
/// …). Pre-staged: a follow-up migrates the callee-name match in codegen
/// onto a typed `RuntimeCallFamily::MathIntrinsic(...)`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MathIntrinsic {
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
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RuntimeCallFamily {
    // --- Actor cooperate/link/monitor/spawn surface -------------------------
    ActorAsk,
    ActorAskWithChannel,
    ActorCooperate,
    ActorLink,
    ActorMonitor,
    ActorSelf,
    ActorSendById,
    ActorSpawn,

    // --- Auto-injected mutex substrate (closure-env / generator-state) ------
    AutoMutexAlloc,
    AutoMutexFree,
    AutoMutexLock,
    AutoMutexUnlock,

    // --- Bytes value index/length/slice/push --------------------------------
    BytesIndex,
    BytesLen,
    BytesPush,
    BytesSlice,

    // --- CancellationToken retain/release/poll ------------------------------
    CancelTokenIsRequested,
    CancelTokenRelease,
    CancelTokenRetain,

    // --- Channel<T> (std::channel) ------------------------------------------
    // Pre-staged: today these dispatch via codegen's
    // `Terminator::Call` callee-name intercept. Their symbols
    // are NOT in `MIR_EMITTER_RUNTIME_SYMBOLS`; the bijection test
    // explicitly excludes them from the allowlist-coverage assertion.
    ChannelRecv(ChannelElementKind),
    ChannelTryRecv(ChannelElementKind),
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
    HashMapLenLayout,
    HashMapNewWithLayout,
    HashMapRemoveLayout,

    // --- Layout-backed HashSet ---------------------------------------------
    HashSetContainsLayout,
    HashSetFreeLayout,
    HashSetInsertLayout,
    HashSetIsEmptyLayout,
    HashSetLenLayout,
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

    // --- Observe ------------------------------------------------------------
    ObserveReadU64,
    ObserveScrape,
    ObserveSeries,

    // --- Option<T> helpers --------------------------------------------------
    OptionIsNone,
    OptionIsSome,
    OptionUnwrap(NumericElem),
    OptionUnwrapOr(NumericElem),

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

    // --- Result<T, E> helpers ----------------------------------------------
    ResultIsErr,
    ResultIsOk,
    ResultUnwrap(NumericElem),
    ResultUnwrapOr(IntegerElem),

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
    // Pre-staged consumers: see Sink note above. The string-element
    // recv/try-recv symbols are `hew_stream_next` / `hew_stream_try_next`
    // (NB no `_string` suffix — the bytes path is the "overload" added
    // later, so the no-suffix name is the original string path). Source
    // of truth: `hew-types/src/builtin_names.rs:208-220` ElementOverload
    // table. `consumes_receiver()` is `true` for `StreamClose`/`SinkClose`
    // to mirror `runtime_symbol_consumes_receiver`.
    StreamClose,
    StreamNext(StreamElementKind),
    StreamTryNext(StreamElementKind),

    // --- String runtime helpers --------------------------------------------
    StringCharCount,
    StringConcat,
    StringIndex,
    StringSliceCodepoints,

    // --- Supervisor --------------------------------------------------------
    SupervisorChildGet,
    SupervisorNestedGet,
    SupervisorStop,

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
            Self::ActorLink => "hew_actor_link",
            Self::ActorMonitor => "hew_actor_monitor",
            Self::ActorSelf => "hew_actor_self",
            Self::ActorSendById => "hew_actor_send_by_id",
            Self::ActorSpawn => "hew_actor_spawn",
            // Auto-mutex
            Self::AutoMutexAlloc => "hew_auto_mutex_alloc",
            Self::AutoMutexFree => "hew_auto_mutex_free",
            Self::AutoMutexLock => "hew_auto_mutex_lock",
            Self::AutoMutexUnlock => "hew_auto_mutex_unlock",
            // Bytes
            Self::BytesIndex => "hew_bytes_index",
            Self::BytesLen => "hew_bytes_len",
            Self::BytesPush => "hew_bytes_push",
            Self::BytesSlice => "hew_bytes_slice",
            // CancellationToken
            Self::CancelTokenIsRequested => "hew_cancel_token_is_requested",
            Self::CancelTokenRelease => "hew_cancel_token_release",
            Self::CancelTokenRetain => "hew_cancel_token_retain",
            // Channel (pre-staged)
            Self::ChannelRecv(ChannelElementKind::Int) => "hew_channel_recv_int",
            Self::ChannelRecv(ChannelElementKind::String) => "hew_channel_recv",
            Self::ChannelTryRecv(ChannelElementKind::Int) => "hew_channel_try_recv_int",
            Self::ChannelTryRecv(ChannelElementKind::String) => "hew_channel_try_recv",
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
            Self::HashMapLenLayout => "hew_hashmap_len_layout",
            Self::HashMapNewWithLayout => "hew_hashmap_new_with_layout",
            Self::HashMapRemoveLayout => "hew_hashmap_remove_layout",
            // HashSet
            Self::HashSetContainsLayout => "hew_hashset_contains_layout",
            Self::HashSetFreeLayout => "hew_hashset_free_layout",
            Self::HashSetInsertLayout => "hew_hashset_insert_layout",
            Self::HashSetIsEmptyLayout => "hew_hashset_is_empty_layout",
            Self::HashSetLenLayout => "hew_hashset_len_layout",
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
            // Observe
            Self::ObserveReadU64 => "hew_observe_read_u64",
            Self::ObserveScrape => "hew_observe_scrape",
            Self::ObserveSeries => "hew_observe_series",
            // Option helpers
            Self::OptionIsNone => "hew_option_is_none",
            Self::OptionIsSome => "hew_option_is_some",
            Self::OptionUnwrap(NumericElem::F64) => "hew_option_unwrap_f64",
            Self::OptionUnwrap(NumericElem::I32) => "hew_option_unwrap_i32",
            Self::OptionUnwrap(NumericElem::I64) => "hew_option_unwrap_i64",
            Self::OptionUnwrapOr(NumericElem::F64) => "hew_option_unwrap_or_f64",
            Self::OptionUnwrapOr(NumericElem::I32) => "hew_option_unwrap_or_i32",
            Self::OptionUnwrapOr(NumericElem::I64) => "hew_option_unwrap_or_i64",
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
            // Result helpers
            Self::ResultIsErr => "hew_result_is_err",
            Self::ResultIsOk => "hew_result_is_ok",
            Self::ResultUnwrap(NumericElem::F64) => "hew_result_unwrap_f64",
            Self::ResultUnwrap(NumericElem::I32) => "hew_result_unwrap_i32",
            Self::ResultUnwrap(NumericElem::I64) => "hew_result_unwrap_i64",
            Self::ResultUnwrapOr(IntegerElem::I32) => "hew_result_unwrap_or_i32",
            Self::ResultUnwrapOr(IntegerElem::I64) => "hew_result_unwrap_or_i64",
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
            // Stream (pre-staged consumers; string variants resolve to the
            // no-suffix names `hew_stream_next` / `hew_stream_try_next`
            // per hew-types/src/builtin_names.rs:208-220 — NOT the
            // invented `_string` suffix)
            Self::StreamClose => "hew_stream_close",
            Self::StreamNext(StreamElementKind::Bytes) => "hew_stream_next_bytes",
            Self::StreamNext(StreamElementKind::String) => "hew_stream_next",
            Self::StreamTryNext(StreamElementKind::Bytes) => "hew_stream_try_next_bytes",
            Self::StreamTryNext(StreamElementKind::String) => "hew_stream_try_next",
            // String
            Self::StringCharCount => "hew_string_char_count",
            Self::StringConcat => "hew_string_concat",
            Self::StringIndex => "hew_string_index",
            Self::StringSliceCodepoints => "hew_string_slice_codepoints",
            // Supervisor
            Self::SupervisorChildGet => "hew_supervisor_child_get",
            Self::SupervisorNestedGet => "hew_supervisor_nested_get",
            Self::SupervisorStop => "hew_supervisor_stop",
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
            Self::TaskSpawnThread => "hew_task_spawn_thread",
            // Vec
            Self::VecGet(VecGetElem::Bool) => "hew_vec_get_bool",
            Self::VecGet(VecGetElem::F64) => "hew_vec_get_f64",
            Self::VecGet(VecGetElem::I32) => "hew_vec_get_i32",
            Self::VecGet(VecGetElem::I64) => "hew_vec_get_i64",
            Self::VecGet(VecGetElem::Layout) => "hew_vec_get_layout",
            Self::VecGet(VecGetElem::Owned) => "hew_vec_get_owned",
            Self::VecGet(VecGetElem::Ptr) => "hew_vec_get_ptr",
            Self::VecGet(VecGetElem::Str) => "hew_vec_get_str",
            Self::VecLen => "hew_vec_len",
            Self::VecSliceRange(VecSliceElem::F64) => "hew_vec_slice_range_f64",
            Self::VecSliceRange(VecSliceElem::I32) => "hew_vec_slice_range_i32",
            Self::VecSliceRange(VecSliceElem::I64) => "hew_vec_slice_range_i64",
            Self::VecSliceRange(VecSliceElem::Ptr) => "hew_vec_slice_range_ptr",
            Self::VecSliceRange(VecSliceElem::Str) => "hew_vec_slice_range_str",
            // Vtable
            Self::VtableDispatchPanicOnOob => "hew_vtable_dispatch_panic_on_oob",
        }
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
    /// `hew-hir/src/lower.rs` — `is_stream_recv_await` (line 8341,
    /// `c_symbol == "hew_stream_next_bytes"`), `is_channel_recv_await`
    /// (line 8356, `c_symbol == "hew_channel_recv"` /
    /// `"hew_channel_recv_int"`), `is_stream_send_await` (line 8388,
    /// `c_symbol == "hew_sink_write_bytes"`), and the duplex-close
    /// await arm (line 10127, `c_symbol == "hew_duplex_close"`). The
    /// returned `Some` set is EXACTLY those 5 symbols; everything else
    /// is non-suspending today.
    ///
    /// The match is exhaustive over [`RuntimeCallFamily`] — there is
    /// no `_ =>` wildcard arm — so a future family variant cannot be
    /// added without explicitly declaring its suspension behaviour
    /// (LESSONS P0 `match-fail-closed` + `exhaustive-traversal-and-lowering`).
    ///
    /// coordinate with for-await Stage 2: when Stage 2 lands
    /// `Stream<string>` awaitability, the `StreamNext(StreamElementKind::String)`
    /// arm flips from `None` to `Some(AsyncSuspendKind::StreamRecvString)`
    /// (and a sibling `StreamTryNext(String)` test is added). Do not
    /// pre-flip here.
    #[must_use]
    #[allow(
        clippy::too_many_lines,
        reason = "exhaustive closed-enum match by design; no `_ =>` arm"
    )]
    pub fn is_async_suspending(self) -> Option<AsyncSuspendKind> {
        use RuntimeCallFamily as F;
        match self {
            // The 5 suspending symbols (HIR await-classifier source of truth).
            F::StreamNext(StreamElementKind::Bytes) => Some(AsyncSuspendKind::StreamRecvBytes),
            F::ChannelRecv(_) => Some(AsyncSuspendKind::ChannelRecv),
            F::SinkWrite(StreamElementKind::Bytes) => Some(AsyncSuspendKind::SinkSendBytes),
            F::DuplexClose => Some(AsyncSuspendKind::DuplexClose),

            // Everything else: NOT suspending today. Exhaustively listed
            // so adding a new variant requires an explicit decision.
            // coordinate with for-await Stage 2: StreamNext(String) +
            // StreamTryNext(String) will flip.
            F::StreamNext(StreamElementKind::String)
            | F::StreamTryNext(_)
            | F::StreamClose
            | F::SinkWrite(StreamElementKind::String)
            | F::SinkTryWrite(_)
            | F::SinkClose
            | F::ChannelTryRecv(_)
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
            | F::ActorLink
            | F::ActorMonitor
            | F::ActorSelf
            | F::ActorSendById
            | F::ActorSpawn
            | F::AutoMutexAlloc
            | F::AutoMutexFree
            | F::AutoMutexLock
            | F::AutoMutexUnlock
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
            | F::HashMapLenLayout
            | F::HashMapNewWithLayout
            | F::HashMapRemoveLayout
            | F::HashSetContainsLayout
            | F::HashSetFreeLayout
            | F::HashSetInsertLayout
            | F::HashSetIsEmptyLayout
            | F::HashSetLenLayout
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
            | F::NodeLookup
            | F::ObserveReadU64
            | F::ObserveScrape
            | F::ObserveSeries
            | F::OptionIsNone
            | F::OptionIsSome
            | F::OptionUnwrap(_)
            | F::OptionUnwrapOr(_)
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
            | F::ResultIsErr
            | F::ResultIsOk
            | F::ResultUnwrap(_)
            | F::ResultUnwrapOr(_)
            | F::SelectFirst
            | F::SendHalfSend
            | F::SendHalfTrySend
            | F::StringCharCount
            | F::StringConcat
            | F::StringIndex
            | F::StringSliceCodepoints
            | F::SupervisorChildGet
            | F::SupervisorNestedGet
            | F::SupervisorStop
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
/// await-classifier flavour in `hew-hir/src/lower.rs`:
/// `is_stream_recv_await` / `is_channel_recv_await` /
/// `is_stream_send_await` / the duplex-close await arm.
///
/// coordinate with for-await Stage 2: a `StreamRecvString` variant
/// lands when Stream<string> recv becomes awaitable.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AsyncSuspendKind {
    /// `await stream.recv()` over `Stream<bytes>` → `hew_stream_next_bytes`.
    StreamRecvBytes,
    /// `await rx.recv()` over `Receiver<T>` → `hew_channel_recv` /
    /// `hew_channel_recv_int`.
    ChannelRecv,
    /// `await sink.send(x)` over `Sink<bytes>` → `hew_sink_write_bytes`.
    SinkSendBytes,
    /// `await actor.close()` over a lambda-actor `Duplex` →
    /// `hew_duplex_close`.
    DuplexClose,
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
/// `LambdaActorHandle::close`, `SendHalf::close | RecvHalf::close`,
/// `CancellationToken::release`).
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
            Self::LambdaActorHandleClose => "hew_lambda_actor_release",
            Self::SendHalfClose | Self::RecvHalfClose => "hew_duplex_close_half",
            Self::CancellationTokenRelease => "hew_cancel_token_release",
        }
    }

    /// The producer-side method-name spelling consumed by today's
    /// `runtime_drop_symbol(&str)`. Round-trip key for the migration
    /// (a follow-up deletes the string-keyed lookup; until then this method
    /// lets the bijection test pin parity with the existing table).
    #[must_use]
    pub fn drop_fn_name(self) -> &'static str {
        match self {
            Self::DuplexClose => "Duplex::close",
            Self::StreamClose => "Stream::close",
            Self::SinkClose => "Sink::close",
            Self::LambdaActorHandleClose => "LambdaActorHandle::close",
            Self::SendHalfClose => "SendHalf::close",
            Self::RecvHalfClose => "RecvHalf::close",
            Self::CancellationTokenRelease => "CancellationToken::release",
        }
    }
}

// =============================================================================
// Test-only enumeration helpers
// =============================================================================

/// Enumerate every legal [`RuntimeCallFamily`] variant. Test-only — the
/// bijection test iterates this to assert every variant has a unique
/// `c_symbol()` and (where applicable) lies in
/// `MIR_EMITTER_RUNTIME_SYMBOLS`.
///
/// Adding a new variant to [`RuntimeCallFamily`] requires adding the
/// constructor row(s) here OR the bijection test fails (coverage
/// invariant). This is the substrate's correctness anchor.
#[cfg(test)]
#[allow(
    clippy::too_many_lines,
    reason = "coverage enumeration IS the substrate; one row per variant"
)]
fn all_runtime_call_families() -> Vec<RuntimeCallFamily> {
    use RuntimeCallFamily as F;
    vec![
        // Actor
        F::ActorAsk,
        F::ActorAskWithChannel,
        F::ActorCooperate,
        F::ActorLink,
        F::ActorMonitor,
        F::ActorSelf,
        F::ActorSendById,
        F::ActorSpawn,
        // Auto-mutex
        F::AutoMutexAlloc,
        F::AutoMutexFree,
        F::AutoMutexLock,
        F::AutoMutexUnlock,
        // Bytes
        F::BytesIndex,
        F::BytesLen,
        F::BytesPush,
        F::BytesSlice,
        // CancellationToken
        F::CancelTokenIsRequested,
        F::CancelTokenRelease,
        F::CancelTokenRetain,
        // Channel
        F::ChannelRecv(ChannelElementKind::Int),
        F::ChannelRecv(ChannelElementKind::String),
        F::ChannelTryRecv(ChannelElementKind::Int),
        F::ChannelTryRecv(ChannelElementKind::String),
        F::ChannelSenderClose,
        F::ChannelReceiverClose,
        // Duplex
        F::DuplexClone,
        F::DuplexClose,
        F::DuplexCloseHalf,
        F::DuplexPair,
        F::DuplexPayloadFree,
        F::DuplexRecv,
        F::DuplexRecvHalf,
        F::DuplexSend,
        F::DuplexSendHalf,
        F::DuplexTryRecv,
        F::DuplexTrySend,
        // Duration
        F::DurationAbs,
        F::DurationHours,
        F::DurationIsZero,
        F::DurationMicros,
        F::DurationMillis,
        F::DurationMins,
        F::DurationNanos,
        F::DurationSecs,
        // Dyn box
        F::DynBoxAlloc,
        F::DynBoxFree,
        // HashMap
        F::HashMapContainsKeyLayout,
        F::HashMapFreeLayout,
        F::HashMapGetLayout,
        F::HashMapInsertLayout,
        F::HashMapLenLayout,
        F::HashMapNewWithLayout,
        F::HashMapRemoveLayout,
        // HashSet
        F::HashSetContainsLayout,
        F::HashSetFreeLayout,
        F::HashSetInsertLayout,
        F::HashSetIsEmptyLayout,
        F::HashSetLenLayout,
        F::HashSetNewWithLayout,
        F::HashSetRemoveLayout,
        // Instant
        F::InstantDurationSince,
        F::InstantElapsed,
        F::InstantNow,
        // Lambda actor
        F::LambdaActorAsk,
        F::LambdaBodyAllocReplyBuf,
        F::LambdaActorClone,
        F::LambdaActorDowngrade,
        F::LambdaDrainAll,
        F::LambdaActorNew,
        F::LambdaActorRelease,
        F::LambdaActorSend,
        F::LambdaActorWeakClone,
        F::LambdaActorWeakDrop,
        F::LambdaActorWeakSend,
        // Math intrinsics
        F::MathIntrinsic(MathIntrinsic::Sqrt),
        F::MathIntrinsic(MathIntrinsic::Exp),
        F::MathIntrinsic(MathIntrinsic::Log),
        F::MathIntrinsic(MathIntrinsic::Sin),
        F::MathIntrinsic(MathIntrinsic::Cos),
        F::MathIntrinsic(MathIntrinsic::AbsI64),
        F::MathIntrinsic(MathIntrinsic::MinI64),
        F::MathIntrinsic(MathIntrinsic::MaxI64),
        F::MathIntrinsic(MathIntrinsic::AbsF64),
        F::MathIntrinsic(MathIntrinsic::MinF64),
        F::MathIntrinsic(MathIntrinsic::MaxF64),
        F::MathIntrinsic(MathIntrinsic::Pow),
        F::MathIntrinsic(MathIntrinsic::Floor),
        F::MathIntrinsic(MathIntrinsic::Ceil),
        F::MathIntrinsic(MathIntrinsic::Round),
        // Node
        F::NodeLookup,
        // Observe
        F::ObserveReadU64,
        F::ObserveScrape,
        F::ObserveSeries,
        // Option helpers
        F::OptionIsNone,
        F::OptionIsSome,
        F::OptionUnwrap(NumericElem::F64),
        F::OptionUnwrap(NumericElem::I32),
        F::OptionUnwrap(NumericElem::I64),
        F::OptionUnwrapOr(NumericElem::F64),
        F::OptionUnwrapOr(NumericElem::I32),
        F::OptionUnwrapOr(NumericElem::I64),
        // Rc
        F::RcNew,
        // RecvHalf
        F::RecvHalfRecv,
        F::RecvHalfTryRecv,
        // Regex
        F::RegexCapture,
        F::RegexCompile,
        F::RegexFreeCapture,
        F::RegexMatch,
        // RemotePid<T>::tell intercept (RemoteActorAsk is structurally
        // typed via `Terminator::RemoteAsk` — no callee string — and
        // intentionally absent from this catalog)
        F::RemotePidTell,
        // Reply channel
        F::ReplyChannelCancel,
        F::ReplyChannelFree,
        F::ReplyChannelNew,
        F::ReplyPayloadFree,
        F::ReplyWait,
        // Result helpers
        F::ResultIsErr,
        F::ResultIsOk,
        F::ResultUnwrap(NumericElem::F64),
        F::ResultUnwrap(NumericElem::I32),
        F::ResultUnwrap(NumericElem::I64),
        F::ResultUnwrapOr(IntegerElem::I32),
        F::ResultUnwrapOr(IntegerElem::I64),
        // Select
        F::SelectFirst,
        // SendHalf
        F::SendHalfSend,
        F::SendHalfTrySend,
        // Sink
        F::SinkClose,
        F::SinkWrite(StreamElementKind::Bytes),
        F::SinkWrite(StreamElementKind::String),
        F::SinkTryWrite(StreamElementKind::Bytes),
        F::SinkTryWrite(StreamElementKind::String),
        // Stream
        F::StreamClose,
        F::StreamNext(StreamElementKind::Bytes),
        F::StreamNext(StreamElementKind::String),
        F::StreamTryNext(StreamElementKind::Bytes),
        F::StreamTryNext(StreamElementKind::String),
        // String
        F::StringCharCount,
        F::StringConcat,
        F::StringIndex,
        F::StringSliceCodepoints,
        // Supervisor
        F::SupervisorChildGet,
        F::SupervisorNestedGet,
        F::SupervisorStop,
        // TCP attach
        F::TcpAttachLocal,
        // Task
        F::TaskAwaitBlocking,
        F::TaskCompleteThreaded,
        F::TaskCompletionObserve,
        F::TaskCompletionUnobserve,
        F::TaskFree,
        F::TaskGetEnv,
        F::TaskGetError,
        F::TaskGetResult,
        F::TaskNew,
        F::TaskScopeCancelAfterNs,
        F::TaskScopeDestroy,
        F::TaskScopeJoinAll,
        F::TaskScopeNew,
        F::TaskScopeSetCurrent,
        F::TaskScopeSpawn,
        F::TaskSetEnv,
        F::TaskSpawnThread,
        // Vec
        F::VecGet(VecGetElem::Bool),
        F::VecGet(VecGetElem::F64),
        F::VecGet(VecGetElem::I32),
        F::VecGet(VecGetElem::I64),
        F::VecGet(VecGetElem::Layout),
        F::VecGet(VecGetElem::Owned),
        F::VecGet(VecGetElem::Ptr),
        F::VecGet(VecGetElem::Str),
        F::VecLen,
        F::VecSliceRange(VecSliceElem::F64),
        F::VecSliceRange(VecSliceElem::I32),
        F::VecSliceRange(VecSliceElem::I64),
        F::VecSliceRange(VecSliceElem::Ptr),
        F::VecSliceRange(VecSliceElem::Str),
        // Vtable
        F::VtableDispatchPanicOnOob,
    ]
}

/// Enumerate every legal [`RuntimeDropDescriptor`] variant. Test-only.
#[cfg(test)]
fn all_runtime_drop_descriptors() -> [RuntimeDropDescriptor; 7] {
    [
        RuntimeDropDescriptor::DuplexClose,
        RuntimeDropDescriptor::StreamClose,
        RuntimeDropDescriptor::SinkClose,
        RuntimeDropDescriptor::LambdaActorHandleClose,
        RuntimeDropDescriptor::SendHalfClose,
        RuntimeDropDescriptor::RecvHalfClose,
        RuntimeDropDescriptor::CancellationTokenRelease,
    ]
}

/// Families pre-staged (Channel, Stream, Sink, `Node::lookup`,
/// math intrinsics, `RemotePidTell`, `RemoteActorAsk`, `TcpAttachLocal`).
/// Their `c_symbol()` is NOT in `MIR_EMITTER_RUNTIME_SYMBOLS` today
/// because they go through `Terminator::Call` callee-name intercepts
/// (codegen callee-name intercept), not `Instr::CallRuntimeAbi`. The bijection test
/// explicitly excludes them from the allowlist-coverage assertion.
///
/// When the follow-up wires the producers, the symbols join the allowlist and
/// this list shrinks.
#[cfg(test)]
fn is_pre_staged_family(family: RuntimeCallFamily) -> bool {
    use RuntimeCallFamily as F;
    matches!(
        family,
        F::ChannelRecv(_)
            | F::ChannelTryRecv(_)
            | F::ChannelSenderClose
            | F::ChannelReceiverClose
            | F::MathIntrinsic(_)
            | F::NodeLookup
            | F::RemotePidTell
            | F::SinkClose
            | F::SinkWrite(_)
            | F::SinkTryWrite(_)
            | F::StreamClose
            | F::StreamNext(_)
            | F::StreamTryNext(_)
            | F::TcpAttachLocal
    )
}

// =============================================================================
// Tests
// =============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use crate::runtime_symbols::{is_known_runtime_symbol, known_runtime_symbols};
    use std::collections::{HashMap, HashSet};

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

    /// Bijection (inverse): every symbol in
    /// `MIR_EMITTER_RUNTIME_SYMBOLS` is produced by exactly one
    /// family variant. A symbol added to the allowlist without a
    /// matching descriptor variant fails this test, forcing every
    /// future runtime-ABI addition to update both the allowlist and
    /// the typed catalog at the same time.
    #[test]
    fn every_allowlist_symbol_has_a_family() {
        let families = all_runtime_call_families();
        let produced: HashSet<&'static str> = families.iter().map(|f| f.c_symbol()).collect();
        let mut missing = Vec::new();
        for sym in known_runtime_symbols() {
            if !produced.contains(sym) {
                missing.push(*sym);
            }
        }
        assert!(
            missing.is_empty(),
            "MIR_EMITTER_RUNTIME_SYMBOLS entries with no RuntimeCallFamily \
             variant — every allowlist symbol must have a typed descriptor \
             so consumers can dispatch on the family rather than the string. \
             Missing: {missing:?}"
        );
    }

    /// Coverage (forward): every family that is NOT pre-staged
    /// produces a symbol that is in `MIR_EMITTER_RUNTIME_SYMBOLS`. A
    /// new variant added without the matching allowlist entry fails
    /// this test (unless the contributor also adds it to
    /// `is_pre_staged_family`, which the reviewer would catch).
    #[test]
    fn allowlist_subset_round_trips() {
        let mut violations = Vec::new();
        for family in all_runtime_call_families() {
            if is_pre_staged_family(family) {
                continue;
            }
            let sym = family.c_symbol();
            if !is_known_runtime_symbol(sym) {
                violations.push((family, sym));
            }
        }
        assert!(
            violations.is_empty(),
            "Family→c_symbol values not in MIR_EMITTER_RUNTIME_SYMBOLS — \
             either add the symbol to the allowlist (preferred) or \
             mark the family pre-staged in `is_pre_staged_family`. Offenders: \
             {violations:?}"
        );
    }

    /// `consumes_receiver` parity with
    /// `hew-types/src/builtin_names.rs::runtime_symbol_consumes_receiver`
    /// for the 7-symbol set. Hard-coded mirror so a future change to
    /// either side surfaces here.
    #[test]
    fn consumes_receiver_mirrors_builtin_names() {
        // The canonical 7-set from
        // `hew_types::builtin_names::runtime_symbol_consumes_receiver`.
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
                 descriptor says {consumes}, builtin_names says \
                 {expected_consumes}"
            );
        }
    }

    /// `is_async_suspending` returns `Some(_)` for EXACTLY the 5 symbols
    /// the HIR await-classifier discriminates today
    /// (`hew-hir/src/lower.rs:8341` + `:8356` + `:8388` + `:10127`):
    /// `hew_stream_next_bytes`, `hew_channel_recv`,
    /// `hew_channel_recv_int`, `hew_sink_write_bytes`, `hew_duplex_close`.
    /// Locks the consumer contract for the eventual migration.
    ///
    /// Positive: the 5 symbols listed map to the matching
    /// `AsyncSuspendKind`. Negative: every other family returns `None`,
    /// pinned by enumeration via `all_runtime_call_families`. ESP. the
    /// historical mis-classifications (`DuplexRecv`, `DuplexSend`,
    /// `StreamNext(String)`, `SinkWrite(String)`, every `try_*` peer)
    /// MUST be non-suspending.
    ///
    /// coordinate with for-await Stage 2: `StreamNext(String)` and
    /// `StreamTryNext(String)` will move to the positive set when
    /// Stage 2 lands Stream<string> awaitability — update both the
    /// match in `is_async_suspending` and this test together.
    #[test]
    fn is_async_suspending_pins_exact_classifier_set() {
        use RuntimeCallFamily as F;

        // Positive: exactly these 5 (family, expected kind) tuples.
        let positives: &[(RuntimeCallFamily, AsyncSuspendKind)] = &[
            (
                F::StreamNext(StreamElementKind::Bytes),
                AsyncSuspendKind::StreamRecvBytes,
            ),
            (
                F::ChannelRecv(ChannelElementKind::Int),
                AsyncSuspendKind::ChannelRecv,
            ),
            (
                F::ChannelRecv(ChannelElementKind::String),
                AsyncSuspendKind::ChannelRecv,
            ),
            (
                F::SinkWrite(StreamElementKind::Bytes),
                AsyncSuspendKind::SinkSendBytes,
            ),
            (F::DuplexClose, AsyncSuspendKind::DuplexClose),
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
        // StreamNext(String), SinkWrite(String)) plus the try_* peers
        // the classifier never touches.
        let must_not_suspend: &[RuntimeCallFamily] = &[
            F::DuplexRecv,
            F::DuplexSend,
            F::DuplexTryRecv,
            F::DuplexTrySend,
            F::StreamNext(StreamElementKind::String),
            F::StreamTryNext(StreamElementKind::Bytes),
            F::StreamTryNext(StreamElementKind::String),
            F::SinkWrite(StreamElementKind::String),
            F::SinkTryWrite(StreamElementKind::Bytes),
            F::SinkTryWrite(StreamElementKind::String),
            F::ChannelTryRecv(ChannelElementKind::Int),
            F::ChannelTryRecv(ChannelElementKind::String),
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
        // the suspending set is exactly 5. Adding a new family without
        // a corresponding decision is caught by the exhaustive match
        // in `is_async_suspending`; the count assertion below is the
        // belt to that suspenders.
        let suspending_count = all_runtime_call_families()
            .into_iter()
            .filter(|f| f.is_async_suspending().is_some())
            .count();
        assert_eq!(
            suspending_count, 5,
            "exactly 5 families should be suspending today; \
             coordinate with for-await Stage 2 before changing this"
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
        // DuplexClose is one of the 5 suspending classifier symbols.
        assert_eq!(d.is_async_suspending(), Some(AsyncSuspendKind::DuplexClose));

        // Non-suspending close peer: StreamClose / SinkClose are NOT
        // in the await-classifier set.
        let d = RuntimeCallDescriptor::new(RuntimeCallFamily::StreamClose, None).unwrap();
        assert_eq!(d.c_symbol(), "hew_stream_close");
        assert!(d.consumes_receiver());
        assert_eq!(d.is_async_suspending(), None);

        let d = RuntimeCallDescriptor::new(
            RuntimeCallFamily::StreamNext(StreamElementKind::Bytes),
            None,
        )
        .unwrap();
        assert_eq!(d.c_symbol(), "hew_stream_next_bytes");
        assert!(!d.consumes_receiver());
        assert_eq!(
            d.is_async_suspending(),
            Some(AsyncSuspendKind::StreamRecvBytes)
        );

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
        // Mirror of the runtime_drop_symbol table (drop_fn_name → C symbol).
        // Listed here so a future change to either side fails this test
        // loudly (substrate-tests-the-substrate).
        let expected: &[(&str, &str)] = &[
            ("Duplex::close", "hew_duplex_close"),
            ("Stream::close", "hew_stream_close"),
            ("Sink::close", "hew_sink_close"),
            ("LambdaActorHandle::close", "hew_lambda_actor_release"),
            ("SendHalf::close", "hew_duplex_close_half"),
            ("RecvHalf::close", "hew_duplex_close_half"),
            ("CancellationToken::release", "hew_cancel_token_release"),
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

    /// Allowlist coverage: every drop-descriptor's `c_symbol()` is in
    /// `MIR_EMITTER_RUNTIME_SYMBOLS`. (`hew_stream_close` /
    /// `hew_sink_close` are not in the allowlist today — the bijection
    /// excludes them with the same `is_pre_staged_*` rationale as the
    /// call-family side.)
    #[test]
    fn drop_descriptor_symbols_in_allowlist_or_pre_staged() {
        // Drop-side pre-staged set: Stream/Sink close symbols are not in
        // MIR_EMITTER_RUNTIME_SYMBOLS because the follow-up migration
        // wires their producers via the await-classifier ramp; the
        // underlying handles are closed via the codegen Terminator::Call
        // intercepts today, not a typed Instr::Drop.
        let pre_staged: HashSet<&'static str> =
            ["hew_stream_close", "hew_sink_close"].into_iter().collect();
        for d in all_runtime_drop_descriptors() {
            let sym = d.c_symbol();
            if pre_staged.contains(sym) {
                continue;
            }
            assert!(
                is_known_runtime_symbol(sym),
                "drop descriptor {d:?} → {sym} not in MIR_EMITTER_RUNTIME_SYMBOLS \
                 and not in the pre-staged set"
            );
        }
    }

    /// Symbol-existence parity: every family's `c_symbol()` resolves to
    /// a real, verifiable identifier — either an entry in the runtime
    /// allowlist (`MIR_EMITTER_RUNTIME_SYMBOLS`), a codegen-intercept
    /// name (callee-name dispatch in `hew-codegen-rs/src/llvm.rs`), a
    /// math intrinsic user name, or a Stream/Sink `ElementOverload`
    /// extension symbol from `hew-types/src/builtin_names.rs`. NO
    /// family is allowed to fabricate a string that exists nowhere
    /// else — that would let a typed catalog ship symbols that the
    /// runtime cannot resolve.
    ///
    /// The verified-real allowlists below are hand-curated mirrors of
    /// the real sources of truth. A symbol slipping through (e.g. a
    /// future fake `hew_remote_actor_ask` reappearing) fires here.
    #[test]
    fn every_c_symbol_resolves_to_a_real_symbol() {
        // Callee-name dispatch intercepts in codegen — these are real
        // identities used by `Terminator::Call` matching in
        // `hew-codegen-rs/src/llvm.rs` (Channel/Stream/Sink pre-staged,
        // RemotePidTell `CalleeNameDispatchOnly` linkage in
        // `hew-hir/src/stdlib_catalog.rs`, `Node::lookup`, TCP attach).
        let codegen_intercepts: HashSet<&'static str> = [
            "Node::lookup",
            "hew_remote_pid_tell",
            "hew_tcp_attach_local",
            "hew_stream_next_bytes",
            "hew_stream_try_next_bytes",
            "hew_channel_recv",
            "hew_channel_try_recv",
            "hew_channel_recv_int",
            "hew_channel_try_recv_int",
            "hew_sink_write_bytes",
            "hew_sink_try_write_bytes",
        ]
        .into_iter()
        .collect();

        // Math intrinsics — user-visible names from
        // `hew-codegen-rs/src/llvm.rs::math_builtin_intrinsic`.
        let math_intrinsics: HashSet<&'static str> = [
            "sqrt", "exp", "log", "sin", "cos", "abs", "min", "max", "abs_f", "min_f", "max_f",
            "pow", "floor", "ceil", "round",
        ]
        .into_iter()
        .collect();

        // Stream/Sink ElementOverload + close peers — real exports from
        // `hew-types/src/builtin_names.rs:208-265`. Hand-mirrored so a
        // typo here can't paper over a fabricated symbol in the family.
        let element_overload_extras: HashSet<&'static str> = [
            "hew_stream_next",
            "hew_stream_try_next",
            "hew_sink_write_string",
            "hew_sink_try_write_string",
            "hew_sink_try_write_bytes",
            "hew_channel_sender_close",
            "hew_channel_receiver_close",
            "hew_stream_close",
            "hew_sink_close",
        ]
        .into_iter()
        .collect();

        let mut fabricated = Vec::new();
        for family in all_runtime_call_families() {
            let sym = family.c_symbol();
            let resolved = is_known_runtime_symbol(sym)
                || codegen_intercepts.contains(sym)
                || math_intrinsics.contains(sym)
                || element_overload_extras.contains(sym);
            if !resolved {
                fabricated.push((family, sym));
            }
        }
        assert!(
            fabricated.is_empty(),
            "RuntimeCallFamily::c_symbol() produced strings that do not \
             resolve to any real runtime symbol, codegen intercept, math \
             intrinsic, or element-overload extra — these would be \
             fabricated identities the runtime cannot dispatch on. \
             Offenders: {fabricated:?}"
        );
    }
}
