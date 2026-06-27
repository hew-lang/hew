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
    ActorRef,
    Actor,
    Task,
    StreamPair,
    Generator,
    AsyncGenerator,
    Range,
    Rc,
    Sender,
    Receiver,
    Stream,
    Sink,
    Duplex,
    Pid,
    LocalPid,
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
    SendError,
    AskError,
    RecvError,
    LinkError,
    MonitorRef,
    NarrowError,
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
    ActorRef => "ActorRef",
    Actor => "Actor",
    Task => "Task",
    StreamPair => "StreamPair",
    Generator => "Generator",
    AsyncGenerator => "AsyncGenerator",
    Range => "Range",
    Rc => "Rc",
    Sender => "Sender",
    Receiver => "Receiver",
    Stream => "Stream",
    Sink => "Sink",
    Duplex => "Duplex",
    Pid => "Pid",
    LocalPid => "LocalPid",
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
    SendError => "SendError",
    AskError => "AskError",
    RecvError => "RecvError",
    LinkError => "LinkError",
    MonitorRef => "MonitorRef",
    NarrowError => "NarrowError",
    CloseError => "CloseError",
    Iterator => "Iterator",
    Unit => "Unit",
    Duration => "Duration",
    Instant => "instant",
    Trap => "Trap",
    CancellationToken => "CancellationToken",
    TimeoutError => "TimeoutError",
}

impl BuiltinType {
    #[must_use]
    pub const fn marker(self) -> BuiltinTypeMarker {
        match self {
            Self::Duplex
            | Self::Sink
            | Self::Stream
            | Self::Sender
            | Self::Receiver
            | Self::LocalPid
            | Self::RemotePid
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

    #[must_use]
    pub const fn handle_family(self) -> Option<BuiltinHandleFamily> {
        match self {
            Self::Pid | Self::LocalPid | Self::RemotePid | Self::LambdaPid => {
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
            | Self::HashSet
            | Self::ActorRef
            | Self::Actor
            | Self::Task
            | Self::Generator
            | Self::AsyncGenerator
            | Self::Range
            | Self::Rc
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
            | Self::StreamPair
            | Self::Duplex
            | Self::HewDuplex
            | Self::LambdaActorHandle
            | Self::LambdaPid => 2,
            Self::Pid
            | Self::HewActor
            | Self::HewSendHalf
            | Self::HewRecvHalf
            | Self::BoxedActor
            | Self::CrashInfo
            | Self::CrashAction
            | Self::SendError
            | Self::AskError
            | Self::RecvError
            | Self::LinkError
            | Self::MonitorRef
            | Self::NarrowError
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
            Self::ActorRef | Self::Actor | Self::LambdaPid => {
                &[BuiltinTypeRole::ActorDispatchLocal]
            }
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

    #[must_use]
    pub const fn is_substrate_handle(self) -> bool {
        matches!(
            self,
            Self::Duplex | Self::Sink | Self::Stream | Self::SendHalf | Self::RecvHalf
        )
    }

    /// True for the local actor-handle builtins that lower to a single
    /// pointer-shaped runtime word (`*mut HewActor`) — `LocalPid<T>`,
    /// `ActorRef<T>`, and `Actor<T>`.
    ///
    /// These are the builtins whose codegen `resolve_ty` arm produces an opaque
    /// `ptr` and whose `Vec<T>` constructor routes to `hew_vec_new_ptr` (see
    /// `resolve_ty` + `resolved_ty_is_plain_bitcopy` in `hew-codegen-rs`). The
    /// checker MUST classify them as the pointer-shaped (`"ptr"`) Vec-element
    /// ABI so `push`/`get`/`set`/`pop` route to the `hew_vec_*_ptr` family
    /// rather than the layout-descriptor family — otherwise the constructor and
    /// the element ops disagree (null-layout `hew_vec_new_ptr` + layout push),
    /// tripping the runtime "layout-aware operation is not implemented" abort.
    ///
    /// `RemotePid<T>` is intentionally excluded: it lowers to a bare `i64`
    /// packed PID, not a pointer, so it takes a different element ABI.
    /// Substrate handles (`Duplex`/`Stream`/`Sink`/channel halves) are affine
    /// move-only resources and are not admitted as Vec elements here.
    #[must_use]
    pub const fn lowers_as_pointer_vec_element(self) -> bool {
        matches!(self, Self::LocalPid | Self::ActorRef | Self::Actor)
    }
}

#[must_use]
pub const fn builtin_types() -> &'static [BuiltinTypeInfo] {
    BUILTIN_TYPES
}

#[must_use]
pub fn lookup_builtin_type(name: &str) -> Option<BuiltinType> {
    match name {
        "channel.Sender" => return Some(BuiltinType::Sender),
        "channel.Receiver" => return Some(BuiltinType::Receiver),
        "stream.Stream" => return Some(BuiltinType::Stream),
        "stream.Sink" => return Some(BuiltinType::Sink),
        "duplex.Duplex" => return Some(BuiltinType::Duplex),
        _ => {}
    }
    builtin_types()
        .iter()
        .find(|info| info.canonical_name == name)
        .map(|info| info.kind)
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
                BuiltinType::Pid,
                BuiltinTypeMarker::None,
                None,
                Some(BuiltinHandleFamily::ActorPid),
                0,
                &[][..],
            ),
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
                BuiltinTypeMarker::Resource,
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
