//! Compiler-known named type discriminator.
//!
//! `BuiltinType` is the neutral, checker-owned identity for named builtins.
//! The only string-to-builtin lookup lives in this module; dispatch sites
//! should consume the enum carried on `Ty::Named` instead of comparing names.

use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum BuiltinType {
    Option,
    Result,
    Vec,
    HashMap,
    HashSet,
    /// Compiler-synthesised owned snapshot cursor for `Vec<T>` iteration.
    VecIter,
    /// Compiler-synthesised parallel snapshot cursor for `HashMap<K, V>`.
    HashMapIter,
    Task,
    StreamPair,
    Generator,
    AsyncGenerator,
    Range,
    Rc,
    Weak,
    Sender,
    Receiver,
    Stream,
    Sink,
    Duplex,
    /// `SupervisorPool<S, T>` — compiler-produced view of pool `T` owned by
    /// supervisor `S`. Runtime representation is `{ LocalPid<S>, i64 pool_key }`.
    SupervisorPool,
    LocalPid,
    NodeId,
    Location,
    RemotePid,
    HewActor,
    HewDuplex,
    HewSendHalf,
    HewRecvHalf,
    BoxedActor,
    ActorState,
    MachineState,
    SendHalf,
    RecvHalf,
    LambdaActorHandle,
    /// `LambdaPid<M, R>` — the user-visible handle for a lambda actor
    /// (`actor |m: M| -> R { .. }`). PID-like: "a pid you ask, M in → R out".
    /// Unifies the conceptual model with `LocalPid`/`RemotePid` (the `Pid`
    /// family) rather than the `Duplex` channel substrate. `handle_family =
    /// ActorPid` (not `Duplex`) so the channel-only surface (`.recv()`,
    /// `.send_half()`, `.recv_half()`) is never exposed on an actor handle.
    /// Lowers to `*mut HewLambdaActorHandle`; the MIR routes it through
    /// `Place::LambdaActorHandle` to `hew_lambda_actor_send` / `_ask`.
    LambdaPid,
    CrashInfo,
    CrashAction,
    /// `std/failure.hew::CrashNotification { actor_id: u64, kind: CrashKind }`
    /// — the typed payload a linked actor's `#[on(exit)]` hook receives (M-7-R).
    CrashNotification,
    /// `std/failure.hew::CrashKind { Crashed; HeapExceeded; PartitionDetected }`
    /// — the crash-class enum delivered in a `CrashNotification` (M-7-R).
    CrashKind,
    /// `std/link_monitor.hew::MonitorId { value: u64 }`.
    MonitorId,
    /// `std/link_monitor.hew::DownTarget`.
    DownTarget,
    /// `std/link_monitor.hew::DownReason`.
    DownReason,
    /// Canonical payload accepted by `#[on(down)]`.
    DownNotification,
    SendError,
    AskError,
    LookupError,
    RecvError,
    LinkError,
    MonitorError,
    MonitorRef,
    CloseError,
    Iterator,
    Unit,
    Duration,
    /// `instant` — a monotonic timestamp in nanoseconds. ABI-identical to a
    /// bare `i64` (the runtime's `hew_instant_*` symbols take/return `i64`),
    /// so it lowers to `ResolvedTy::I64` at the MIR boundary. Kept distinct in
    /// the checker only so `instant::now()` / `.elapsed()` / `.duration_since()`
    /// dispatch to the `impl instant` block rather than the integer methods.
    Instant,
    Trap,
    CancellationToken,
    /// `TimeoutError` — the error arm of `await rx.recv() | after d` /
    /// `await stream.recv() | after d`.  A unit enum with one variant
    /// (`Timeout`) that distinguishes a deadline expiry from a closed channel
    /// (`Ok(None)`).
    TimeoutError,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct BuiltinTypeInfo {
    pub kind: BuiltinType,
    pub canonical_name: &'static str,
    pub marker: BuiltinTypeMarker,
    pub close_method: Option<&'static str>,
    pub handle_family: Option<BuiltinHandleFamily>,
    pub arity: usize,
    pub roles: &'static [BuiltinTypeRole],
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum BuiltinTypeMarker {
    None,
    BitCopy,
    Resource,
    Linear,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum BuiltinHandleFamily {
    ActorPid,
    ActorRuntime,
    Duplex,
    DuplexHalf,
    ActorState,
    MachineState,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum BuiltinTypeRole {
    ActorDispatchLocal,
    ActorDispatchRemote,
    SupervisorLocalPid,
    WasmNativeOnlyHandle,
    ActorStatePayload,
    MachineStatePayload,
    CrashInfoPayload,
}

macro_rules! builtin_types {
    ($($variant:ident => $canonical:literal),* $(,)?) => {
        const BUILTIN_TYPES: &[BuiltinTypeInfo] = &[
            $(
                BuiltinTypeInfo {
                    kind: BuiltinType::$variant,
                    canonical_name: $canonical,
                    marker: BuiltinType::$variant.marker(),
                    close_method: BuiltinType::$variant.close_method(),
                    handle_family: BuiltinType::$variant.handle_family(),
                    arity: BuiltinType::$variant.arity(),
                    roles: BuiltinType::$variant.roles(),
                },
            )*
        ];

        impl BuiltinType {
            #[must_use]
            pub const fn canonical_name(self) -> &'static str {
                match self {
                    $(Self::$variant => $canonical),*
                }
            }
        }
    };
}

builtin_types! {
    Option => "Option",
    Result => "Result",
    Vec => "Vec",
    HashMap => "HashMap",
    HashSet => "HashSet",
    VecIter => "VecIter",
    HashMapIter => "HashMapIter",
    Task => "Task",
    StreamPair => "StreamPair",
    Generator => "Generator",
    AsyncGenerator => "AsyncGenerator",
    Range => "Range",
    Rc => "Rc",
    Weak => "Weak",
    Sender => "Sender",
    Receiver => "Receiver",
    Stream => "Stream",
    Sink => "Sink",
    Duplex => "Duplex",
    SupervisorPool => "SupervisorPool",
    LocalPid => "LocalPid",
    NodeId => "NodeId",
    Location => "Location",
    RemotePid => "RemotePid",
    HewActor => "HewActor",
    HewDuplex => "HewDuplex",
    HewSendHalf => "HewSendHalf",
    HewRecvHalf => "HewRecvHalf",
    BoxedActor => "BoxedActor",
    ActorState => "ActorState",
    MachineState => "MachineState",
    SendHalf => "SendHalf",
    RecvHalf => "RecvHalf",
    LambdaActorHandle => "LambdaActorHandle",
    LambdaPid => "LambdaPid",
    CrashInfo => "CrashInfo",
    CrashAction => "CrashAction",
    CrashNotification => "CrashNotification",
    CrashKind => "CrashKind",
    MonitorId => "MonitorId",
    DownTarget => "DownTarget",
    DownReason => "DownReason",
    DownNotification => "DownNotification",
    SendError => "SendError",
    AskError => "AskError",
    LookupError => "LookupError",
    RecvError => "RecvError",
    LinkError => "LinkError",
    MonitorError => "MonitorError",
    MonitorRef => "MonitorRef",
    CloseError => "CloseError",
    Iterator => "Iterator",
    Unit => "Unit",
    Duration => "duration",
    Instant => "instant",
    Trap => "Trap",
    CancellationToken => "CancellationToken",
    TimeoutError => "TimeoutError",
}

impl BuiltinType {
    /// Whether cloning this builtin duplicates only its outer handle and treats
    /// type arguments as protocol/identity tags rather than stored payloads.
    ///
    /// This is the shared checker/MIR authority for the affine-marker walk.
    /// Actor references are bit-copied, `Rc`/`Weak` retain their shared
    /// allocation, and `Sender` clones its refcounted endpoint handle; none
    /// recursively clones a resource-bearing type argument. `Receiver` is
    /// deliberately absent: the single-consumer endpoint has no clone helper.
    #[must_use]
    pub const fn is_affine_clone_terminal(self) -> bool {
        matches!(
            self,
            Self::LocalPid
                | Self::RemotePid
                | Self::LambdaPid
                | Self::HewActor
                | Self::Rc
                | Self::Weak
                | Self::Sender
        )
    }

    #[must_use]
    pub const fn marker(self) -> BuiltinTypeMarker {
        match self {
            Self::Duplex
            | Self::Sink
            | Self::Stream
            | Self::Sender
            | Self::Receiver
            | Self::LocalPid
            | Self::HewActor
            | Self::HewDuplex
            | Self::HewSendHalf
            | Self::HewRecvHalf
            | Self::BoxedActor
            | Self::SendHalf
            | Self::RecvHalf
            | Self::LambdaActorHandle
            | Self::LambdaPid
            | Self::CancellationToken
            | Self::MonitorRef => BuiltinTypeMarker::Resource,
            Self::SupervisorPool
            | Self::NodeId
            | Self::Location
            | Self::RemotePid
            | Self::MonitorId
            | Self::DownTarget
            | Self::DownReason
            | Self::DownNotification => BuiltinTypeMarker::BitCopy,
            Self::ActorState | Self::MachineState => BuiltinTypeMarker::Linear,
            // `CrashInfo` carries an owned `message: string` (M-5), so it is no
            // longer a `BitCopy` aggregate. `None` lets the owned-aggregate
            // record machinery classify it as `CowValue` (field-wise clone/drop
            // via `__hew_record_{clone,drop}_inplace_CrashInfo`) rather than
            // forcing a marker-driven `BitCopy`.
            _ => BuiltinTypeMarker::None,
        }
    }

    #[must_use]
    pub const fn close_method(self) -> Option<&'static str> {
        match self {
            Self::Duplex
            | Self::Sink
            | Self::Stream
            | Self::Sender
            | Self::Receiver
            | Self::HewActor
            | Self::HewDuplex
            | Self::HewSendHalf
            | Self::HewRecvHalf
            | Self::BoxedActor
            | Self::SendHalf
            | Self::RecvHalf
            | Self::LambdaActorHandle
            | Self::LambdaPid
            | Self::MonitorRef => Some("close"),
            Self::CancellationToken => Some("release"),
            _ => None,
        }
    }

    /// Whether this is a stdlib declaration whose ABI identity is known to the
    /// compiler but whose lexical authority and field/variant layout remain
    /// source-owned.  Such a type is never admitted by its bare catalog name:
    /// a named/glob/aliased import must publish that spelling, or the program
    /// must use the imported owner's qualified spelling.  The no-search-path
    /// inline-test bootstrap is the sole prelude exception.
    #[must_use]
    pub const fn requires_source_import(self) -> bool {
        matches!(
            self,
            Self::CrashNotification
                | Self::CrashKind
                | Self::MonitorId
                | Self::DownTarget
                | Self::DownReason
                | Self::DownNotification
                | Self::MonitorError
                | Self::MonitorRef
        )
    }

    #[must_use]
    pub const fn handle_family(self) -> Option<BuiltinHandleFamily> {
        match self {
            Self::LocalPid | Self::RemotePid | Self::LambdaPid => {
                Some(BuiltinHandleFamily::ActorPid)
            }
            Self::HewActor | Self::BoxedActor => Some(BuiltinHandleFamily::ActorRuntime),
            Self::Duplex | Self::HewDuplex | Self::LambdaActorHandle => {
                Some(BuiltinHandleFamily::Duplex)
            }
            Self::SendHalf | Self::RecvHalf | Self::HewSendHalf | Self::HewRecvHalf => {
                Some(BuiltinHandleFamily::DuplexHalf)
            }
            Self::ActorState => Some(BuiltinHandleFamily::ActorState),
            Self::MachineState => Some(BuiltinHandleFamily::MachineState),
            _ => None,
        }
    }

    #[must_use]
    pub const fn arity(self) -> usize {
        match self {
            Self::Option
            | Self::Vec
            | Self::VecIter
            | Self::HashSet
            | Self::Task
            | Self::Generator
            | Self::AsyncGenerator
            | Self::Range
            | Self::Rc
            | Self::Weak
            | Self::Sender
            | Self::Receiver
            | Self::Stream
            | Self::Sink
            | Self::LocalPid
            | Self::RemotePid
            | Self::ActorState
            | Self::MachineState
            | Self::SendHalf
            | Self::RecvHalf => 1,
            Self::Result
            | Self::HashMap
            | Self::HashMapIter
            | Self::StreamPair
            | Self::Duplex
            | Self::SupervisorPool
            | Self::HewDuplex
            | Self::LambdaActorHandle
            | Self::LambdaPid => 2,
            Self::HewActor
            | Self::HewSendHalf
            | Self::HewRecvHalf
            | Self::BoxedActor
            | Self::NodeId
            | Self::Location
            | Self::CrashInfo
            | Self::CrashAction
            | Self::CrashNotification
            | Self::CrashKind
            | Self::MonitorId
            | Self::DownTarget
            | Self::DownReason
            | Self::DownNotification
            | Self::SendError
            | Self::AskError
            | Self::LookupError
            | Self::RecvError
            | Self::LinkError
            | Self::MonitorError
            | Self::MonitorRef
            | Self::CloseError
            | Self::Iterator
            | Self::Unit
            | Self::Duration
            | Self::Instant
            | Self::Trap
            | Self::CancellationToken
            | Self::TimeoutError => 0,
        }
    }

    #[must_use]
    pub const fn roles(self) -> &'static [BuiltinTypeRole] {
        match self {
            Self::LambdaPid => &[BuiltinTypeRole::ActorDispatchLocal],
            Self::LocalPid => &[
                BuiltinTypeRole::ActorDispatchLocal,
                BuiltinTypeRole::SupervisorLocalPid,
            ],
            Self::RemotePid => &[BuiltinTypeRole::ActorDispatchRemote],
            Self::HewActor
            | Self::HewDuplex
            | Self::HewSendHalf
            | Self::HewRecvHalf
            | Self::BoxedActor => &[BuiltinTypeRole::WasmNativeOnlyHandle],
            Self::ActorState => &[BuiltinTypeRole::ActorStatePayload],
            Self::MachineState => &[BuiltinTypeRole::MachineStatePayload],
            Self::CrashInfo => &[BuiltinTypeRole::CrashInfoPayload],
            _ => &[],
        }
    }

    #[must_use]
    pub fn has_role(self, role: BuiltinTypeRole) -> bool {
        self.roles().contains(&role)
    }

    #[must_use]
    pub const fn is_channel_handle(self) -> bool {
        matches!(self, Self::Sender | Self::Receiver)
    }

    #[must_use]
    pub const fn is_collection(self) -> bool {
        matches!(self, Self::Vec | Self::HashMap | Self::HashSet)
    }

    /// Whether a by-value copy of this builtin still refers to storage or
    /// process state visible through the caller's copy.
    ///
    /// This is the checker authority for projection-aware mutable-parameter
    /// diagnostics. The set is intentionally narrower than `Resource`: some
    /// resources are merely affine values, while `RemotePid` and
    /// `SupervisorPool` are bit-copy representations that still designate
    /// shared actor state. Unknown and future builtins fail closed by default.
    #[must_use]
    pub const fn is_caller_visible_shared_handle(self) -> bool {
        matches!(
            self,
            Self::Vec
                | Self::HashMap
                | Self::HashSet
                | Self::Rc
                | Self::Weak
                | Self::Sender
                | Self::Receiver
                | Self::LocalPid
                | Self::RemotePid
                | Self::Duplex
                | Self::Stream
                | Self::Sink
                | Self::SendHalf
                | Self::RecvHalf
                | Self::HewActor
                | Self::LambdaActorHandle
                | Self::SupervisorPool
        )
    }

    #[must_use]
    pub const fn is_substrate_handle(self) -> bool {
        matches!(
            self,
            Self::Duplex | Self::Sink | Self::Stream | Self::SendHalf | Self::RecvHalf
        )
    }

    /// True for the local actor-handle builtin that lowers to a single
    /// pointer-shaped runtime word (`*mut HewActor`) — `LocalPid<T>`.
    ///
    /// This is the builtin whose codegen `resolve_ty` arm produces an opaque
    /// `ptr` and whose `Vec<T>` constructor routes to `hew_vec_new_ptr` (see
    /// `resolve_ty` + `resolved_ty_is_plain_bitcopy` in `hew-codegen-rs`). The
    /// checker MUST classify it as the pointer-shaped (`"ptr"`) Vec-element
    /// ABI so `push`/`get`/`set`/`pop` route to the `hew_vec_*_ptr` family
    /// rather than the layout-descriptor family — otherwise the constructor and
    /// the element ops disagree (null-layout `hew_vec_new_ptr` + layout push),
    /// tripping the runtime "layout-aware operation is not implemented" abort.
    ///
    /// `RemotePid<T>` is intentionally excluded: it lowers to an inline
    /// aggregate, not a pointer, so it takes a different element ABI.
    /// Substrate handles (`Duplex`/`Stream`/`Sink`/channel halves) are affine
    /// move-only resources and are not admitted as Vec elements here.
    #[must_use]
    pub const fn lowers_as_pointer_vec_element(self) -> bool {
        matches!(self, Self::LocalPid)
    }

    /// True when the builtin's complete value ABI is one opaque pointer word.
    ///
    /// This is codegen representation authority, not an ownership or source-
    /// level handle classification. In particular, `RemotePid<T>` is excluded
    /// because its value is an inline location aggregate, while
    /// `SupervisorPool<S, T>` is a two-field aggregate. Consumers must use this
    /// discriminator instead of presentation names so renamed builtin spellings
    /// retain their ABI and same-spelling user nominals cannot acquire it.
    #[must_use]
    pub const fn lowers_as_opaque_pointer_abi(self) -> bool {
        matches!(
            self,
            Self::Vec
                | Self::HashMap
                | Self::HashSet
                | Self::Rc
                | Self::Weak
                | Self::Sender
                | Self::Receiver
                | Self::Duplex
                | Self::Stream
                | Self::Sink
                | Self::SendHalf
                | Self::RecvHalf
                | Self::LocalPid
                | Self::LambdaPid
                | Self::Generator
                | Self::AsyncGenerator
        )
    }
}

#[must_use]
pub const fn builtin_types() -> &'static [BuiltinTypeInfo] {
    BUILTIN_TYPES
}

#[must_use]
pub fn lookup_builtin_type(name: &str) -> Option<BuiltinType> {
    match name {
        "channel.Sender" | "std.channel.Sender" => {
            return Some(BuiltinType::Sender);
        }
        "channel.Receiver" | "std.channel.Receiver" => {
            return Some(BuiltinType::Receiver);
        }
        "stream.Stream" | "std.stream.Stream" => return Some(BuiltinType::Stream),
        "stream.Sink" | "std.stream.Sink" => return Some(BuiltinType::Sink),
        "duplex.Duplex" => return Some(BuiltinType::Duplex),
        "link_monitor.MonitorRef" | "std.link_monitor.MonitorRef" => {
            return Some(BuiltinType::MonitorRef);
        }
        "link_monitor.MonitorError" | "std.link_monitor.MonitorError" => {
            return Some(BuiltinType::MonitorError);
        }
        _ => {}
    }
    builtin_types()
        .iter()
        .find(|info| info.canonical_name == name)
        .map(|info| info.kind)
}

/// Look up a source-owned lifecycle builtin by either its bare name or its
/// canonical source identity (`std.failure.CrashNotification`, for example).
///
/// This is deliberately narrower than [`lookup_builtin_type`]: callers use it
/// to enforce source import authority, not to make arbitrary catalog names
/// globally visible.
#[must_use]
pub fn lookup_source_owned_lifecycle_type(name: &str) -> Option<BuiltinType> {
    let (owner, bare) = if let Some(bare) = name.strip_prefix("std.failure.") {
        ("std.failure", bare)
    } else if let Some(bare) = name.strip_prefix("std.link_monitor.") {
        ("std.link_monitor", bare)
    } else if let Some(bare) = name.strip_prefix("failure.") {
        // Retained only for reading older internal facts while the canonical
        // producer identity is the full module path.
        ("failure", bare)
    } else if let Some(bare) = name.strip_prefix("link_monitor.") {
        ("link_monitor", bare)
    } else if name.contains('.') {
        return None;
    } else {
        ("", name)
    };
    let builtin = lookup_builtin_type(bare)?;
    builtin
        .requires_source_import()
        .then_some(builtin)
        .filter(|_| {
            owner.is_empty()
                || matches!(
                    (owner, builtin),
                    (
                        "failure" | "std.failure",
                        BuiltinType::CrashNotification | BuiltinType::CrashKind
                    ) | (
                        "link_monitor" | "std.link_monitor",
                        BuiltinType::MonitorId
                            | BuiltinType::DownTarget
                            | BuiltinType::DownReason
                            | BuiltinType::DownNotification
                            | BuiltinType::MonitorError
                            | BuiltinType::MonitorRef
                    )
                )
        })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lookup_covers_registered_builtins() {
        for info in builtin_types() {
            assert_eq!(lookup_builtin_type(info.canonical_name), Some(info.kind));
            assert_eq!(info.kind.canonical_name(), info.canonical_name);
            assert_eq!(info.marker, info.kind.marker());
            assert_eq!(info.close_method, info.kind.close_method());
            assert_eq!(info.handle_family, info.kind.handle_family());
            assert_eq!(info.arity, info.kind.arity());
            assert_eq!(info.roles, info.kind.roles());
        }
    }

    #[test]
    fn lookup_rejects_user_names() {
        assert_eq!(lookup_builtin_type("UserOption"), None);
        assert_eq!(lookup_builtin_type("user.Option"), None);
        assert_eq!(lookup_builtin_type("user.MonitorError"), None);
    }

    #[test]
    fn lookup_accepts_exact_renamed_monitor_carriers() {
        assert_eq!(
            lookup_builtin_type("link_monitor.MonitorError"),
            Some(BuiltinType::MonitorError)
        );
        assert_eq!(
            lookup_builtin_type("std.link_monitor.MonitorError"),
            Some(BuiltinType::MonitorError)
        );
    }

    #[test]
    fn lookup_accepts_exact_channel_owners_without_leaf_fallback() {
        assert_eq!(
            lookup_builtin_type("std.channel.Sender"),
            Some(BuiltinType::Sender)
        );
        assert_eq!(
            lookup_builtin_type("std.channel.Receiver"),
            Some(BuiltinType::Receiver)
        );
        assert_eq!(lookup_builtin_type("std.channel.channel.Sender"), None);
        assert_eq!(lookup_builtin_type("std.channel.channel.Receiver"), None);
        assert_eq!(lookup_builtin_type("acme.channel.Sender"), None);
        assert_eq!(lookup_builtin_type("acme.channel.Receiver"), None);
    }

    #[test]
    fn caller_visible_shared_handle_facts_are_exact() {
        let expected = [
            BuiltinType::Vec,
            BuiltinType::HashMap,
            BuiltinType::HashSet,
            BuiltinType::Rc,
            BuiltinType::Weak,
            BuiltinType::Sender,
            BuiltinType::Receiver,
            BuiltinType::LocalPid,
            BuiltinType::RemotePid,
            BuiltinType::Duplex,
            BuiltinType::Stream,
            BuiltinType::Sink,
            BuiltinType::SendHalf,
            BuiltinType::RecvHalf,
            BuiltinType::HewActor,
            BuiltinType::LambdaActorHandle,
            BuiltinType::SupervisorPool,
        ];

        for info in builtin_types() {
            assert_eq!(
                info.kind.is_caller_visible_shared_handle(),
                expected.contains(&info.kind),
                "{:?} caller-visible shared-handle classification",
                info.kind
            );
        }
    }

    #[test]
    fn opaque_pointer_abi_facts_are_exact() {
        let expected = [
            BuiltinType::Vec,
            BuiltinType::HashMap,
            BuiltinType::HashSet,
            BuiltinType::Rc,
            BuiltinType::Weak,
            BuiltinType::Sender,
            BuiltinType::Receiver,
            BuiltinType::Duplex,
            BuiltinType::Stream,
            BuiltinType::Sink,
            BuiltinType::SendHalf,
            BuiltinType::RecvHalf,
            BuiltinType::LocalPid,
            BuiltinType::LambdaPid,
            BuiltinType::Generator,
            BuiltinType::AsyncGenerator,
        ];

        for info in builtin_types() {
            assert_eq!(
                info.kind.lowers_as_opaque_pointer_abi(),
                expected.contains(&info.kind),
                "{:?} opaque-pointer ABI classification",
                info.kind
            );
        }
    }

    #[test]
    fn builtin_type_clone_and_serde_round_trip() {
        let cloned = BuiltinType::Option;
        assert_eq!(cloned, BuiltinType::Option);
        let json = serde_json::to_string(&BuiltinType::Option).unwrap();
        assert_eq!(
            serde_json::from_str::<BuiltinType>(&json).unwrap(),
            BuiltinType::Option
        );
    }

    #[test]
    #[allow(clippy::too_many_lines, reason = "single builtin fact table")]
    fn handle_and_project_cap_facts_are_registered() {
        let expected = [
            (
                BuiltinType::LocalPid,
                BuiltinTypeMarker::Resource,
                None,
                Some(BuiltinHandleFamily::ActorPid),
                1,
                &[
                    BuiltinTypeRole::ActorDispatchLocal,
                    BuiltinTypeRole::SupervisorLocalPid,
                ][..],
            ),
            (
                BuiltinType::RemotePid,
                BuiltinTypeMarker::BitCopy,
                None,
                Some(BuiltinHandleFamily::ActorPid),
                1,
                &[BuiltinTypeRole::ActorDispatchRemote][..],
            ),
            (
                BuiltinType::LambdaPid,
                BuiltinTypeMarker::Resource,
                Some("close"),
                Some(BuiltinHandleFamily::ActorPid),
                2,
                &[BuiltinTypeRole::ActorDispatchLocal][..],
            ),
            (
                BuiltinType::Sender,
                BuiltinTypeMarker::Resource,
                Some("close"),
                None,
                1,
                &[][..],
            ),
            (
                BuiltinType::Receiver,
                BuiltinTypeMarker::Resource,
                Some("close"),
                None,
                1,
                &[][..],
            ),
            (
                BuiltinType::HewActor,
                BuiltinTypeMarker::Resource,
                Some("close"),
                Some(BuiltinHandleFamily::ActorRuntime),
                0,
                &[BuiltinTypeRole::WasmNativeOnlyHandle][..],
            ),
            (
                BuiltinType::HewDuplex,
                BuiltinTypeMarker::Resource,
                Some("close"),
                Some(BuiltinHandleFamily::Duplex),
                2,
                &[BuiltinTypeRole::WasmNativeOnlyHandle][..],
            ),
            (
                BuiltinType::HewSendHalf,
                BuiltinTypeMarker::Resource,
                Some("close"),
                Some(BuiltinHandleFamily::DuplexHalf),
                0,
                &[BuiltinTypeRole::WasmNativeOnlyHandle][..],
            ),
            (
                BuiltinType::HewRecvHalf,
                BuiltinTypeMarker::Resource,
                Some("close"),
                Some(BuiltinHandleFamily::DuplexHalf),
                0,
                &[BuiltinTypeRole::WasmNativeOnlyHandle][..],
            ),
            (
                BuiltinType::BoxedActor,
                BuiltinTypeMarker::Resource,
                Some("close"),
                Some(BuiltinHandleFamily::ActorRuntime),
                0,
                &[BuiltinTypeRole::WasmNativeOnlyHandle][..],
            ),
            (
                BuiltinType::ActorState,
                BuiltinTypeMarker::Linear,
                None,
                Some(BuiltinHandleFamily::ActorState),
                1,
                &[BuiltinTypeRole::ActorStatePayload][..],
            ),
            (
                BuiltinType::MachineState,
                BuiltinTypeMarker::Linear,
                None,
                Some(BuiltinHandleFamily::MachineState),
                1,
                &[BuiltinTypeRole::MachineStatePayload][..],
            ),
        ];

        for (kind, marker, close_method, family, arity, roles) in expected {
            let info = builtin_types()
                .iter()
                .find(|info| info.kind == kind)
                .unwrap_or_else(|| panic!("missing builtin registration for {kind:?}"));
            assert_eq!(info.marker, marker, "{kind:?} marker");
            assert_eq!(info.close_method, close_method, "{kind:?} close method");
            assert_eq!(info.handle_family, family, "{kind:?} handle family");
            assert_eq!(info.arity, arity, "{kind:?} arity");
            assert_eq!(info.roles, roles, "{kind:?} roles");
        }
    }
}
