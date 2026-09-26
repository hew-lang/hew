//! `RuntimeCallFamily` definition, capability/extern tables, and its checker-facing constructors.

#![allow(
    clippy::wildcard_imports,
    reason = "sibling split of one module; shares its item set"
)]

use super::value_kinds::*;
use super::{declared, ArrayValueOp, AsyncIoOp, FileReadOp, SupervisorPoolOp, TcpOp};
use crate::ResolvedTy;
use serde::{Deserialize, Serialize};
use strum::EnumIter;

/// Closed-set discriminator for every compiler-known runtime / builtin
/// call. One variant per `(method, generic-arity)` tuple. Adding a new
/// runtime operation means adding a variant here and classifying its emitter
/// route. Symbol-derived operations round-trip through `from_c_symbol`;
/// print operations also require their selected type and newline flag.
///
/// Variants are grouped by surface family; ordering within a group is
/// alphabetical by C-symbol leaf to ease diffing against the allowlist.
///
/// `non_exhaustive` is INTENTIONALLY OMITTED — this enum's whole purpose
/// is fail-closed exhaustiveness across consumer match sites (LESSONS P0
/// `match-fail-closed`). A future contributor cannot silently extend it
/// behind a wildcard arm; every consumer must add the new arm at the
/// next slice.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, EnumIter, Serialize, Deserialize)]
pub enum RuntimeCallFamily {
    AsyncIo(AsyncIoOp),
    FileRead(FileReadOp),
    Tcp(TcpOp),
    /// Semantic operations over a distinct, owning encoding value.
    Encoding {
        format: EncodingFormat,
        op: EncodingOp,
    },
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
    /// `hew_actor_gen_sink_complete(actor, sink) -> void` — a `receive gen
    /// fn` stream-producer pump's clean (generator-exhausted) exit:
    /// deregisters the actor's gen-sink slot and frees the sink (decision
    /// 7). Emitted only by `build_stream_producer_pump`
    /// (`hew-mir/src/lower.rs`); no user-facing Hew syntax reaches it.
    /// Pre-staged like `SinkClose`, which it replaces in the pump.
    ActorGenSinkComplete,
    /// `hew_actor_gen_sink_register(actor, sink) -> void` — a `receive gen
    /// fn` pump's prologue registration of its own producer sink, so a
    /// terminal actor teardown can find and fault-close it (decision
    /// 7). Emitted only by `build_stream_producer_pump`.
    ActorGenSinkRegister,
    ActorLink,
    /// `link_remote(RemotePid<T>, PartitionPolicy)` →
    /// `hew_node_link_remote_location(target, policy_tag) -> i64`. Establishes a
    /// cross-node link: a local actor links a remote actor so the remote's
    /// death (exit / crash / partition) fires the per-link `PartitionPolicy`
    /// (`CrashLinked` crashes the local actor). Distinct from `ActorLink` (the
    /// process-local pointer-keyed link); the cross-node form has no local
    /// `HewActor*` for the remote peer.
    LinkRemote,
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

    // --- Bytes value collection ops -----------------------------------------
    BytesAppend,
    BytesClear,
    BytesContains,
    BytesDecodeUtf8,
    BytesDecodeUtf8Lossy,
    BytesGet,
    BytesIndex,
    BytesIsEmpty,
    BytesLen,
    BytesPop,
    BytesPush,
    BytesSet,
    BytesSlice,
    /// `b[a..]` - the open-ended byte slice. The end bound is the receiver's
    /// own length, so the receiver expression is evaluated once.
    BytesSliceFrom,
    /// `bytes::new` constructor callee identity. Codegen materialises the
    /// runtime `hew_bytes_new` call from the destination type.
    BytesNew,

    // --- CancellationToken retain/release/poll ------------------------------
    CancelTokenIsRequested,
    CancelTokenRelease,
    CancelTokenRetain,

    ActorRequestRelease,
    ActorCallFree,
    ActorRequestTake,

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
    HashMapClearLayout,
    HashMapCloneLayout,
    HashMapEntriesLayout,
    HashMapFreeLayout,
    HashMapGetLayout,
    HashMapInsertLayout,
    /// `m.keys()` / `m.values()` projection ops. Pre-staged: they ride
    /// the `Terminator::Call` route (checker `MethodTarget.symbol_name`),
    /// not `Instr::CallRuntimeAbi`, so their symbols are not in
    /// `known_runtime_symbols`. Catalogued so the codegen
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
    HashSetClearLayout,
    HashSetCloneLayout,
    HashSetFreeLayout,
    HashSetInsertLayout,
    HashSetIsEmptyLayout,
    HashSetLenLayout,
    /// The `HashSet::new` constructor surface form; see
    /// [`Self::HashMapNew`] for the two-identity rationale.
    HashSetNew,
    HashSetNewWithLayout,
    HashSetRemoveLayout,
    HashSetToVecLayout,

    // --- Instant accessors --------------------------------------------------
    InstantDurationSince,
    InstantElapsed,
    InstantNow,

    // --- Math intrinsics ---------------------------------------------------
    // User-visible callee identities carried on MIR `Terminator::Call`; not
    // runtime C-ABI symbols and therefore absent from `known_runtime_symbols`.
    MathIntrinsic(MathIntrinsic),
    // Integer bit-manipulation methods (`x.count_ones()`, …). Same shape as
    // `MathIntrinsic`: a user-visible callee identity carried on MIR
    // `Terminator::Call`, not a runtime C-ABI symbol.
    IntMethod(IntBitOp, IntMethodWidth),
    // Non-trapping integer arithmetic (`x.wrapping_add(y)`, `x.saturating_sub(y)`).
    // Same shape as `IntMethod`.
    IntArith(IntArithKind, IntMethodWidth),
    // `f64` bit/classification methods (`x.to_bits()`, `x.is_nan()`, …).
    // Same shape as `IntMethod`.
    FloatMethod(FloatMethodOp),

    // --- Node operations ---------------------------------------------------
    // The `Node::*` variants are pre-staged Terminator::Call callee identities.
    // Their runtime FFI symbols are carried separately by the stdlib catalog;
    // the family records the source-level builtin identity used by checker,
    // MIR, and codegen dispatch.
    NodeAllowPeer,
    NodeConnect,
    NodeId,
    /// The identity and location accessors `std/builtins.hew` declares through
    /// `#[extern_symbol]`. Each borrows its receiver - a `NodeId`, `Location`
    /// or `RemotePid<T>` is a by-value identity snapshot whose drop frees
    /// nothing - and returns either a fresh owned string or a bit-copied field.
    NodeIdDisplay,
    LocationNodeId,
    LocationSlot,
    LocationIncarnation,
    LocationDisplay,
    RemotePidLocation,
    RemotePidNodeId,
    RemotePidSlot,
    RemotePidIncarnation,
    RemotePidDisplay,
    NodeIdentityKey,
    NodeLoadKeys,
    NodeLookup,
    /// `monitor(RemotePid<T>)` →
    /// `hew_node_monitor_location(target, out_monitor_id) -> i32`.
    /// Zero returns success and writes the distributed-monitor id; non-zero is
    /// one plus the `MonitorError` discriminant. Codegen assembles
    /// `Result<MonitorRef, MonitorError>`. The current node is resolved
    /// internally, so the single runtime argument is a pointer to the carried
    /// full `Location`; non-consuming.
    NodeMonitor,
    NodeRegister,
    NodeSetTransport,
    NodeShutdown,
    NodeStart,

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

    // --- Rc/Weak ownership --------------------------------------------------
    RcClone,
    RcDowngrade,
    RcDrop,
    RcGet,
    RcIsUnique,
    RcNew,
    RcSet,
    RcStrongCount,
    RcWeakCount,
    WeakCloneRc,
    WeakDropRc,
    WeakUpgradeRc,

    // --- Regex runtime ABI --------------------------------------------------
    RegexCapture,
    RegexCompile,
    RegexFreeCapture,
    /// Value-position regex literal materialisation (`let pat = re"..."`).
    /// Operand zero is the literal's slot index; the result is the handle SIR
    /// wraps in a `std.text.regex.Pattern`, exactly as `regex.new` wraps the
    /// handle `hew_regex_new` returns. Codegen GEP-loads the compiled
    /// `*HewRegex` from `@hew_regex_handles[literal_id]` (the same load
    /// `RegexMatch` performs) and clones it, because that pattern's scope exit
    /// frees its handle and the module slot outlives every value built from it.
    /// The C-symbol spelling `hew_regex_handle` names the synthetic family for
    /// the round-trip bijection only; the call it emits is `hew_regex_clone`.
    RegexHandle,
    RegexMatch,

    // Remote `RemotePid<T>` send and ask are not runtime families: the
    // checker records `MethodCallRewrite::RemoteActorSend` / `RemoteActorAsk`,
    // HIR lowers them to `HirExprKind::RemoteActorSend` / `RemoteActorAsk`,
    // and SIR addresses the actor's checked remote member.

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

    // --- Sink<T> -----------------------------------------------------------
    // `send` rides the element-layout-witness `StreamSendLayout` entry (see
    // the Stream note below); `try_send` is its non-parking peer.
    SinkClone,
    SinkClose,
    SinkFinish,
    /// `hew_sink_peer_closed(sink) -> i32` — a `receive gen fn` pump's
    /// per-iteration peer-closed check (decision 6): 1 once the
    /// consumer stream has closed/detached, so the pump breaks its loop
    /// WITHOUT resuming the generator further (cancellation; an infinite
    /// generator plus a consumer `break` must not livelock the actor).
    /// Emitted only by `build_stream_producer_pump`; pre-staged like
    /// `SinkClose`.
    SinkPeerClosed,

    // --- Stream<T> ---------------------------------------------------------
    // recv/try_recv/send/try_send ride the element-layout-witness `*_layout`
    // entries (one symbol per operation for every describable element type;
    // the element identity travels on the checker-resolved `Option<T>` /
    // value type, never on the symbol). They are pre-staged: codegen
    // intercepts the `Terminator::Call` by callee identity, so they are not
    // in `known_runtime_symbols`. `consumes_receiver()` is
    // `true` for `StreamClose`/`SinkClose` to mirror
    // `runtime_symbol_consumes_receiver`.
    StreamClose,
    /// The three lazy adaptors. Each consumes its source stream and returns a
    /// fresh one that closes the source cooperatively when the consumer stops.
    StreamChunks,
    /// `stream.forward(from, to)`: drain `from` into `to`, finishing `to` at
    /// EOF. Both halves are consumed.
    StreamForward,
    StreamLines,
    StreamNextLayout,
    /// The two halves of one `stream.pipe` allocation, extracted by the
    /// source intrinsics `std.stream.pair_sink` / `pair_stream`.
    StreamPairSink,
    StreamPairStream,
    StreamSendLayout,
    StreamTake,
    StreamTryNextLayout,
    StreamTrySendLayout,

    // --- String runtime helpers --------------------------------------------
    StringCharAt,
    StringCharAtUtf8,
    StringCharCount,
    StringByteLen,
    StringConcat,
    StringEquals,
    StringCompare,
    StringStartsWith,
    StringEndsWith,
    StringContains,
    StringIsEmpty,
    StringIsDigit,
    StringIsAlpha,
    StringIsAlphanumeric,
    StructuralFormat,
    StringFind,
    StringIndex,
    StringLen,
    /// `s.repeat(n)` - a fresh string of `n` concatenated copies.
    StringRepeat,
    StringReplace,
    StringClone,
    /// `s.split(sep)` - a fresh `Vec<string>` of the separated parts.
    StringSplit,
    /// `s.lines()` - a fresh `Vec<string>` split on line boundaries.
    StringLines,
    /// `s.chars()` - a fresh `Vec<char>` of the string's Unicode scalars.
    StringChars,
    StringSliceCodepoints,
    StringSliceCodepointsFrom,
    StringSlice,
    StringToLowercase,
    StringToBytes,
    StringToUppercase,
    StringTrim,
    U8ToString,
    I32ToString,
    I64ToString,
    U32ToString,
    U64ToString,
    F64ToString,
    CharToString,
    /// The selected type and newline choice are required in addition to the
    /// shared C symbol; they cannot be reconstructed from that symbol alone.
    Print {
        kind: PrintKind,
        newline: bool,
    },
    /// Flush the standard streams and terminate the process with a Hew exit
    /// code. The call never returns.
    ProcessExit,
    /// Write one borrowed string to the standard error stream.
    StderrWrite,
    BoolToString,

    // --- Supervisor --------------------------------------------------------
    /// Capture the stable direct identity for a live supervisor binding.
    SupervisorDirectId,
    SupervisorChildGet,
    /// Resolve a static child role through a stable supervisor identity and
    /// return the current incarnation's stable `LocalPid` token.
    LocalPidSupervisorChildGet,
    SupervisorNestedGet,
    /// `hew_supervisor_pool_child_get(sup, pool_key, index) -> ChildLookupResult`
    /// — resolve a static-pool member through its live static slot. Emitted by
    /// the MIR static-pool accessor (`sup.pool[i]` / `.get(i)`).
    SupervisorPoolChildGet,
    /// Resolve a static-pool index to its stable static-child slot without
    /// consulting the member incarnation or liveness state.
    LocalPidSupervisorPoolChildRefGet,
    /// `hew_supervisor_pool_len(sup, pool_key) -> i64` — the static-pool member
    /// count (`sup.pool.len()`).
    SupervisorPoolLen,
    SupervisorStop,
    /// `hew_supervisor_restart_await_blocking(sup, key) -> void` — the
    /// contextless `await_restart` path (`main` / free fn). Blocks the calling
    /// thread until the child slot has been restarted or is permanently Dead.
    SupervisorRestartAwaitBlocking,

    // --- Active transport attach (network actor binding) -------------------
    // Pre-staged method calls dispatch via callee-name intercepts that
    // synthesize concrete actor protocol IDs at codegen time.
    TcpAttachLocal,
    TlsAttachLocal,
    WebSocketAttachLocal,

    // --- Task ABI (scope{}/spawn/await) ------------------------------------
    TaskFree,
    GeneratorFree,

    // --- Vec<T> ------------------------------------------------------------
    /// Final semantic values; target lowering chooses layout-backed runtime entry points.
    Vector(VecValueOp),
    Array(ArrayValueOp),
    /// `sup.pool[i]`, `sup.pool.get(i)` and `await_restart sup.pool[i]`.
    SupervisorPool(SupervisorPoolOp),
    Map(MapValueOp),
    Set(SetValueOp),
    VecAppend,
    VecClear,
    VecClone,
    VecCloneLayout,
    VecCloneOwned,
    VecContainsLayout,
    /// Whole-buffer move: `hew_vec_take_all(v)` returns a fresh vec owning
    /// every element (no clone/drop thunk runs) and leaves `v` a valid empty
    /// vec with its element representation intact. The consuming-iteration
    /// choke for a Vec living in persistent storage (an actor state field).
    VecTakeAll,
    VecContainsOwned,
    VecContainsScalar(VecContainsScalarElem),
    VecGet(VecGetElem),
    VecIsEmpty,
    VecJoinStr,
    VecLen,
    VecNew,
    VecPopBool,
    VecPopLayout,
    VecPopOwned,
    VecPushBool,
    VecPushLayout,
    VecPushOwned,
    VecPushOwnedMove,
    /// Closed scalar Vec ABI matrix (`hew_vec_{push,pop,set,remove_at}_$elem`).
    VecScalar {
        op: VecScalarOp,
        elem: VecScalarElem,
    },
    VecRemoveAtBool,
    VecRemoveAtLayout,
    VecRemoveAtOwned,
    VecSetBool,
    VecSetLayout,
    VecSetOwned,
    VecSetOwnedMove,
    VecSliceRange(VecSliceElem),

    // --- Trait-object dispatch diagnostics ---------------------------------
    VtableDispatchPanicOnOob,
}

/// Module-level runtime authorities implied by typed runtime-call families.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RuntimeCapability {
    BlockingOffload,
    Metrics,
    Node,
}

/// Checker-owned surface type shapes for canonical stdlib extern methods.
///
/// This is intentionally narrower than the runtime's physical C ABI.  It
/// describes the checked Hew declaration that may mint a typed
/// [`RuntimeCallFamily`], so a linker spelling plus matching arity is never
/// enough to select a special lowering path.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CanonicalExternTy {
    Bytes,
    Char,
    U8,
    I64,
    Bool,
    String,
    Unit,
    OptionU8,
    OptionI64,
    OptionChar,
    VecString,
    VecChar,
    Duration,
    Instant,
}

impl CanonicalExternTy {
    const fn matches(self, ty: &crate::Ty) -> bool {
        match self {
            Self::Bytes => matches!(ty, crate::Ty::Bytes),
            Self::Char => matches!(ty, crate::Ty::Char),
            Self::U8 => matches!(ty, crate::Ty::U8),
            Self::I64 => matches!(ty, crate::Ty::I64),
            Self::Bool => matches!(ty, crate::Ty::Bool),
            Self::String => matches!(ty, crate::Ty::String),
            Self::Unit => matches!(ty, crate::Ty::Unit),
            Self::Duration => matches!(ty, crate::Ty::Duration),
            Self::Instant => matches!(
                ty,
                crate::Ty::Named { head: crate::TypeHead::Builtin(crate::BuiltinType::Instant), args, .. } if args.is_empty()
            ),
            Self::VecString => matches!(
                ty,
                crate::Ty::Named { head: crate::TypeHead::Builtin(crate::BuiltinType::Vec), args, .. } if matches!(args.as_slice(), [crate::Ty::String])
            ),
            Self::VecChar => matches!(
                ty,
                crate::Ty::Named { head: crate::TypeHead::Builtin(crate::BuiltinType::Vec), args, .. } if matches!(args.as_slice(), [crate::Ty::Char])
            ),
            Self::OptionU8 => matches!(
                ty,
                crate::Ty::Named { head: crate::TypeHead::Builtin(crate::BuiltinType::Option), args, .. } if matches!(args.as_slice(), [crate::Ty::U8])
            ),
            Self::OptionI64 => matches!(
                ty,
                crate::Ty::Named { head: crate::TypeHead::Builtin(crate::BuiltinType::Option), args, .. } if matches!(args.as_slice(), [crate::Ty::I64])
            ),
            Self::OptionChar => matches!(
                ty,
                crate::Ty::Named { head: crate::TypeHead::Builtin(crate::BuiltinType::Option), args, .. } if matches!(args.as_slice(), [crate::Ty::Char])
            ),
        }
    }
}

/// One exact stdlib extern declaration that is eligible for a typed runtime
/// carrier. `family: None` records a real canonical source method whose
/// dedicated codegen path remains an open-set extern call; it prevents that
/// row from being silently treated as an omitted declaration.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct CanonicalStdlibExternSignature {
    pub module: &'static str,
    pub signature_key: &'static str,
    pub symbol: &'static str,
    pub family: Option<RuntimeCallFamily>,
    pub params: &'static [CanonicalExternTy],
    pub result: CanonicalExternTy,
}

// `FnSig` omits an inherent method's receiver; `signature_key` provides that
// exact `bytes` receiver identity, so these are the remaining source args.
const EMPTY: &[CanonicalExternTy] = &[];
const U8: &[CanonicalExternTy] = &[CanonicalExternTy::U8];
const I64: &[CanonicalExternTy] = &[CanonicalExternTy::I64];
const I64_U8: &[CanonicalExternTy] = &[CanonicalExternTy::I64, CanonicalExternTy::U8];
const BYTES: &[CanonicalExternTy] = &[CanonicalExternTy::Bytes];
const STRING: &[CanonicalExternTy] = &[CanonicalExternTy::String];

/// Extern bridges awaiting migration to declaration-generated contracts.
/// Admission still requires exact source signatures and trusted modules.
const HANDWRITTEN_STD_IO_EXTERN_SIGNATURES: &[CanonicalStdlibExternSignature] = &[
    CanonicalStdlibExternSignature {
        module: "std.io",
        signature_key: "bytes::append",
        symbol: "hew_bytes_append",
        family: Some(RuntimeCallFamily::BytesAppend),
        params: BYTES,
        result: CanonicalExternTy::Unit,
    },
    CanonicalStdlibExternSignature {
        module: "std.io",
        signature_key: "bytes::clear",
        symbol: "hew_bytes_clear",
        family: Some(RuntimeCallFamily::BytesClear),
        params: EMPTY,
        result: CanonicalExternTy::Unit,
    },
    CanonicalStdlibExternSignature {
        module: "std.io",
        signature_key: "bytes::get",
        symbol: "hew_bytes_get",
        family: Some(RuntimeCallFamily::BytesGet),
        params: I64,
        result: CanonicalExternTy::OptionU8,
    },
    CanonicalStdlibExternSignature {
        module: "std.io",
        signature_key: "bytes::pop",
        symbol: "hew_bytes_pop",
        family: Some(RuntimeCallFamily::BytesPop),
        params: EMPTY,
        result: CanonicalExternTy::OptionU8,
    },
    CanonicalStdlibExternSignature {
        module: "std.io",
        signature_key: "bytes::push",
        symbol: "hew_bytes_push",
        family: Some(RuntimeCallFamily::BytesPush),
        params: U8,
        result: CanonicalExternTy::Unit,
    },
    CanonicalStdlibExternSignature {
        module: "std.io",
        signature_key: "bytes::set",
        symbol: "hew_bytes_set",
        family: Some(RuntimeCallFamily::BytesSet),
        params: I64_U8,
        result: CanonicalExternTy::Unit,
    },
    CanonicalStdlibExternSignature {
        module: "std.string",
        signature_key: "string::byte_len",
        symbol: "hew_string_byte_length",
        family: Some(RuntimeCallFamily::StringByteLen),
        params: EMPTY,
        result: CanonicalExternTy::I64,
    },
    CanonicalStdlibExternSignature {
        module: "std.string",
        signature_key: "string::contains",
        symbol: "hew_string_contains",
        family: Some(RuntimeCallFamily::StringContains),
        params: STRING,
        result: CanonicalExternTy::Bool,
    },
    CanonicalStdlibExternSignature {
        module: "std.string",
        signature_key: "string::starts_with",
        symbol: "hew_string_starts_with",
        family: Some(RuntimeCallFamily::StringStartsWith),
        params: STRING,
        result: CanonicalExternTy::Bool,
    },
    CanonicalStdlibExternSignature {
        module: "std.string",
        signature_key: "string::ends_with",
        symbol: "hew_string_ends_with",
        family: Some(RuntimeCallFamily::StringEndsWith),
        params: STRING,
        result: CanonicalExternTy::Bool,
    },
    CanonicalStdlibExternSignature {
        module: "std.string",
        signature_key: "string::is_empty",
        symbol: "hew_string_is_empty",
        family: Some(RuntimeCallFamily::StringIsEmpty),
        params: EMPTY,
        result: CanonicalExternTy::Bool,
    },
    CanonicalStdlibExternSignature {
        module: "std.string",
        signature_key: "string::is_digit",
        symbol: "hew_string_is_digit",
        family: Some(RuntimeCallFamily::StringIsDigit),
        params: EMPTY,
        result: CanonicalExternTy::Bool,
    },
    CanonicalStdlibExternSignature {
        module: "std.string",
        signature_key: "string::is_alpha",
        symbol: "hew_string_is_alpha",
        family: Some(RuntimeCallFamily::StringIsAlpha),
        params: EMPTY,
        result: CanonicalExternTy::Bool,
    },
    CanonicalStdlibExternSignature {
        module: "std.string",
        signature_key: "string::is_alphanumeric",
        symbol: "hew_string_is_alphanumeric",
        family: Some(RuntimeCallFamily::StringIsAlphanumeric),
        params: EMPTY,
        result: CanonicalExternTy::Bool,
    },
    CanonicalStdlibExternSignature {
        module: "std.string",
        signature_key: "string::find",
        symbol: "hew_string_find",
        family: Some(RuntimeCallFamily::StringFind),
        params: STRING,
        result: CanonicalExternTy::OptionI64,
    },
    CanonicalStdlibExternSignature {
        module: "std.string",
        signature_key: "string::char_at",
        symbol: "hew_string_char_at",
        family: Some(RuntimeCallFamily::StringCharAt),
        params: I64,
        result: CanonicalExternTy::OptionChar,
    },
    CanonicalStdlibExternSignature {
        module: "std.string",
        signature_key: "string::get",
        symbol: "hew_string_char_at",
        family: Some(RuntimeCallFamily::StringCharAt),
        params: I64,
        result: CanonicalExternTy::OptionChar,
    },
    CanonicalStdlibExternSignature {
        module: "std.string",
        signature_key: "string::codepoint_at_utf8",
        symbol: "hew_string_char_at_utf8",
        family: Some(RuntimeCallFamily::StringCharAtUtf8),
        params: I64,
        result: CanonicalExternTy::OptionI64,
    },
    CanonicalStdlibExternSignature {
        module: "std.string",
        signature_key: "string::len",
        symbol: "hew_string_length",
        family: Some(RuntimeCallFamily::StringLen),
        params: EMPTY,
        result: CanonicalExternTy::I64,
    },
    CanonicalStdlibExternSignature {
        module: "std.string",
        signature_key: "string::to_bytes",
        symbol: "hew_string_to_bytes",
        family: Some(RuntimeCallFamily::StringToBytes),
        params: EMPTY,
        result: CanonicalExternTy::Bytes,
    },
    CanonicalStdlibExternSignature {
        module: "std.string",
        signature_key: "string::to_upper",
        symbol: "hew_string_to_uppercase",
        family: Some(RuntimeCallFamily::StringToUppercase),
        params: EMPTY,
        result: CanonicalExternTy::String,
    },
    CanonicalStdlibExternSignature {
        module: "std.string",
        signature_key: "string::slice",
        symbol: "hew_string_slice",
        family: Some(RuntimeCallFamily::StringSlice),
        params: &[CanonicalExternTy::I64, CanonicalExternTy::I64],
        result: CanonicalExternTy::String,
    },
    CanonicalStdlibExternSignature {
        module: "std.string",
        signature_key: "string::to_lower",
        symbol: "hew_string_to_lowercase",
        family: Some(RuntimeCallFamily::StringToLowercase),
        params: EMPTY,
        result: CanonicalExternTy::String,
    },
    CanonicalStdlibExternSignature {
        module: "std.string",
        signature_key: "string::repeat",
        symbol: "hew_string_repeat",
        family: Some(RuntimeCallFamily::StringRepeat),
        params: &[CanonicalExternTy::I64],
        result: CanonicalExternTy::String,
    },
    CanonicalStdlibExternSignature {
        module: "std.string",
        signature_key: "string::split",
        symbol: "hew_string_split",
        family: Some(RuntimeCallFamily::StringSplit),
        params: &[CanonicalExternTy::String],
        result: CanonicalExternTy::VecString,
    },
    CanonicalStdlibExternSignature {
        module: "std.string",
        signature_key: "string::lines",
        symbol: "hew_string_lines",
        family: Some(RuntimeCallFamily::StringLines),
        params: EMPTY,
        result: CanonicalExternTy::VecString,
    },
    CanonicalStdlibExternSignature {
        module: "std.string",
        signature_key: "string::chars",
        symbol: "hew_string_chars",
        family: Some(RuntimeCallFamily::StringChars),
        params: EMPTY,
        result: CanonicalExternTy::VecChar,
    },
    CanonicalStdlibExternSignature {
        module: "std.string",
        signature_key: "string::replace",
        symbol: "hew_string_replace",
        family: Some(RuntimeCallFamily::StringReplace),
        params: &[CanonicalExternTy::String, CanonicalExternTy::String],
        result: CanonicalExternTy::String,
    },
    CanonicalStdlibExternSignature {
        module: "std.string",
        signature_key: "string::clone",
        symbol: "hew_string_clone",
        family: Some(RuntimeCallFamily::StringClone),
        params: EMPTY,
        result: CanonicalExternTy::String,
    },
    CanonicalStdlibExternSignature {
        module: "std.string",
        signature_key: "string::trim",
        symbol: "hew_string_trim",
        family: Some(RuntimeCallFamily::StringTrim),
        params: EMPTY,
        result: CanonicalExternTy::String,
    },
];

static CANONICAL_STD_IO_EXTERN_SIGNATURES: std::sync::LazyLock<
    Vec<CanonicalStdlibExternSignature>,
> = std::sync::LazyLock::new(|| {
    let mut entries = HANDWRITTEN_STD_IO_EXTERN_SIGNATURES.to_vec();
    entries.extend(
        declared::DECLARED_DIRECT_RUNTIME_METHODS
            .iter()
            .map(|method| method.signature),
    );
    entries
});

/// Return a canonical stdlib extern declaration when its source identity,
/// endpoint, parameter sequence, and result type all agree.
#[must_use]
pub fn canonical_std_io_extern_signature(
    signature_key: &str,
    symbol: &str,
    params: &[crate::Ty],
    result: &crate::Ty,
) -> Option<&'static CanonicalStdlibExternSignature> {
    CANONICAL_STD_IO_EXTERN_SIGNATURES.iter().find(|entry| {
        let params_match = entry.params.len() == params.len()
            && entry
                .params
                .iter()
                .zip(params)
                .all(|(expected, actual)| expected.matches(actual));
        // Most method signatures have their receiver removed during
        // registration. Directly checked shipped stdlib roots retain the
        // explicit first receiver parameter in a few registration paths; the
        // declaration remains exact after accounting for that source form.
        let explicit_receiver = entry
            .signature_key
            .split_once("::")
            .and_then(|(receiver, _)| match receiver {
                "bytes" => Some(CanonicalExternTy::Bytes),
                "string" => Some(CanonicalExternTy::String),
                _ => None,
            });
        let params_with_receiver_match = explicit_receiver.is_some_and(|receiver| {
            params.len() == entry.params.len() + 1
                && receiver.matches(&params[0])
                && entry
                    .params
                    .iter()
                    .zip(&params[1..])
                    .all(|(expected, actual)| expected.matches(actual))
        });
        entry.signature_key == signature_key
            && entry.symbol == symbol
            && (params_match || params_with_receiver_match)
            && entry.result.matches(result)
    })
}

#[must_use]
pub fn canonical_std_io_extern_signatures() -> &'static [CanonicalStdlibExternSignature] {
    &CANONICAL_STD_IO_EXTERN_SIGNATURES
}

impl RuntimeCallFamily {
    /// Selected value operations invoked by this collection kernel. Their
    /// concrete bodies contribute faults and suspension to the caller.
    #[must_use]
    pub const fn value_callback_capabilities(self) -> &'static [crate::ValueCapability] {
        use crate::ValueCapability::{Eq, Hash};
        match self {
            Self::Map(
                MapValueOp::Index
                | MapValueOp::Get
                | MapValueOp::GetBorrow
                | MapValueOp::ContainsKey
                | MapValueOp::Insert
                | MapValueOp::Remove,
            )
            | Self::Set(SetValueOp::Contains | SetValueOp::Insert | SetValueOp::Remove) => {
                &[Hash, Eq]
            }
            Self::Vector(VecValueOp::Contains) => &[Eq],
            _ => &[],
        }
    }

    #[must_use]
    pub const fn encoding_format(self) -> Option<EncodingFormat> {
        match self {
            Self::Encoding { format, .. } => Some(format),
            _ => None,
        }
    }

    /// Admit an encoding extern only at its exact declaring identity and ABI.
    /// The caller must additionally prove the module is the shipped source.
    /// A C-void mutation retains its semantic updated-owner result; only the
    /// child is consumed by the source ABI, while SIR also moves the receiver.
    #[expect(
        clippy::too_many_arguments,
        reason = "each part of the extern declaration is matched independently"
    )]
    #[must_use]
    pub fn matches_encoding_extern(
        self,
        defs: &crate::DefTable,
        module: &str,
        declaration: &str,
        symbol: &str,
        params: &[ResolvedTy],
        result: &ResolvedTy,
        consuming: &[bool],
    ) -> bool {
        let Some(format) = self.encoding_format() else {
            return false;
        };
        if module != format.module()
            || symbol != self.c_symbol()
            || declaration != format!("{module}.{symbol}")
        {
            return false;
        }
        let Some(contract) = self.semantic_contract() else {
            return false;
        };
        let updates_receiver = matches!(contract.result, RuntimeResultEffect::UpdatedReceiver(_));
        if consuming.len() != contract.arguments.len()
            || !consuming.iter().zip(contract.arguments).enumerate().all(
                |(index, (actual, argument))| {
                    *actual
                        == (argument.effect == RuntimeArgumentEffect::Move
                            && !(updates_receiver && index == 0))
                },
            )
        {
            return false;
        }
        if updates_receiver {
            *result == ResolvedTy::Unit
                && params
                    .first()
                    .is_some_and(|receiver| contract.matches_signature(defs, params, receiver))
        } else {
            contract.matches_signature(defs, params, result)
        }
    }

    /// Canonical checker signature for compiler-registered builtins whose
    /// source identity differs from their runtime ABI symbol.
    #[must_use]
    pub const fn checker_signature_key(self) -> Option<&'static str> {
        match self {
            Self::RcNew => Some("Rc::new"),
            _ => None,
        }
    }

    /// Resolve a checker-selected builtin signature to its typed identity.
    /// Dotted and namespaced syntax have already converged on this key.
    #[must_use]
    pub fn from_checker_signature(signature_key: &str) -> Option<Self> {
        if signature_key == Self::RcNew.checker_signature_key()? {
            return Some(Self::RcNew);
        }
        Self::from_c_symbol(signature_key)
    }

    /// Exact source declaration allowed to publish this floor operation.
    /// Import aliases retain this owner; a same-named function cannot claim it.
    #[must_use]
    pub fn source_intrinsic_declaration(self) -> Option<&'static str> {
        match self {
            Self::StreamPairSink => Some("std.stream.pair_sink"),
            Self::StreamPairStream => Some("std.stream.pair_stream"),
            Self::StreamForward => Some("std.stream.forward"),
            Self::BytesDecodeUtf8 => Some("std.encoding.utf8.decode"),
            Self::BytesDecodeUtf8Lossy => Some("std.encoding.utf8.decode_lossy"),
            _ => None,
        }
    }

    /// Generic parameters admitted by a source-owned floor operation.
    #[must_use]
    pub const fn source_intrinsic_type_params(self) -> &'static [&'static str] {
        match self {
            Self::StreamPairSink | Self::StreamPairStream | Self::StreamForward => &["T"],
            _ => &[],
        }
    }

    /// Resolve an exact compiler-owned stdlib catalogue endpoint to its
    /// runtime family. Catalogue endpoint identity is established before this
    /// call; arbitrary source names never reach it.
    #[must_use]
    pub fn from_catalog_endpoint(endpoint: &str) -> Option<Self> {
        match endpoint {
            "hew_node_id_display" => Some(Self::NodeIdDisplay),
            "hew_location_node_id" => Some(Self::LocationNodeId),
            "hew_location_slot" => Some(Self::LocationSlot),
            "hew_location_incarnation" => Some(Self::LocationIncarnation),
            "hew_location_display" => Some(Self::LocationDisplay),
            "hew_remote_pid_location" => Some(Self::RemotePidLocation),
            "hew_remote_pid_node_id" => Some(Self::RemotePidNodeId),
            "hew_remote_pid_slot" => Some(Self::RemotePidSlot),
            "hew_remote_pid_incarnation" => Some(Self::RemotePidIncarnation),
            "hew_remote_pid_display" => Some(Self::RemotePidDisplay),
            "stream.pair_sink" => Some(Self::StreamPairSink),
            "stream.pair_stream" => Some(Self::StreamPairStream),
            "stream.forward" => Some(Self::StreamForward),
            "println_i32" => Some(Self::Print {
                kind: PrintKind::I32,
                newline: true,
            }),
            "println_i64" => Some(Self::Print {
                kind: PrintKind::I64,
                newline: true,
            }),
            "println_u8" => Some(Self::Print {
                kind: PrintKind::U8,
                newline: true,
            }),
            "println_u32" => Some(Self::Print {
                kind: PrintKind::U32,
                newline: true,
            }),
            "println_u64" => Some(Self::Print {
                kind: PrintKind::U64,
                newline: true,
            }),
            "println_f64" => Some(Self::Print {
                kind: PrintKind::F64,
                newline: true,
            }),
            "println_bool" => Some(Self::Print {
                kind: PrintKind::Bool,
                newline: true,
            }),
            "println_str" => Some(Self::Print {
                kind: PrintKind::Str,
                newline: true,
            }),
            "print_i32" => Some(Self::Print {
                kind: PrintKind::I32,
                newline: false,
            }),
            "print_i64" => Some(Self::Print {
                kind: PrintKind::I64,
                newline: false,
            }),
            "print_u8" => Some(Self::Print {
                kind: PrintKind::U8,
                newline: false,
            }),
            "print_u32" => Some(Self::Print {
                kind: PrintKind::U32,
                newline: false,
            }),
            "print_u64" => Some(Self::Print {
                kind: PrintKind::U64,
                newline: false,
            }),
            "print_f64" => Some(Self::Print {
                kind: PrintKind::F64,
                newline: false,
            }),
            "print_bool" => Some(Self::Print {
                kind: PrintKind::Bool,
                newline: false,
            }),
            "print_str" => Some(Self::Print {
                kind: PrintKind::Str,
                newline: false,
            }),
            "to_string_u8" => Some(Self::U8ToString),
            "to_string_i32" => Some(Self::I32ToString),
            "to_string_i64" => Some(Self::I64ToString),
            "to_string_u32" => Some(Self::U32ToString),
            "to_string_u64" => Some(Self::U64ToString),
            "to_string_f64" => Some(Self::F64ToString),
            "to_string_char" => Some(Self::CharToString),
            "to_string_bool" => Some(Self::BoolToString),
            "string_concat" => Some(Self::StringConcat),
            "bytes::new" => Some(Self::BytesNew),
            "exit" => Some(Self::ProcessExit),
            "utf8.decode" => Some(Self::BytesDecodeUtf8),
            "utf8.decode_lossy" => Some(Self::BytesDecodeUtf8Lossy),
            "Node::start" => Some(Self::NodeStart),
            "Node::connect" => Some(Self::NodeConnect),
            "Node::register" => Some(Self::NodeRegister),
            "Node::lookup" => Some(Self::NodeLookup),
            "Node::shutdown" => Some(Self::NodeShutdown),
            "Node::identity_key" => Some(Self::NodeIdentityKey),
            "Node::id" => Some(Self::NodeId),
            _ => None,
        }
    }
}
