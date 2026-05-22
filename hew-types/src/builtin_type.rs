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
    LocalPid,
    RemotePid,
    SendHalf,
    RecvHalf,
    LambdaActorHandle,
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
    String,
    Map,
    Char,
    Unit,
    Duration,
    Float,
    Trap,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct BuiltinTypeInfo {
    pub kind: BuiltinType,
    pub canonical_name: &'static str,
}

macro_rules! builtin_types {
    ($($variant:ident => $canonical:literal),* $(,)?) => {
        const BUILTIN_TYPES: &[BuiltinTypeInfo] = &[
            $(
                BuiltinTypeInfo {
                    kind: BuiltinType::$variant,
                    canonical_name: $canonical,
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
    LocalPid => "LocalPid",
    RemotePid => "RemotePid",
    SendHalf => "SendHalf",
    RecvHalf => "RecvHalf",
    LambdaActorHandle => "LambdaActorHandle",
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
    String => "String",
    Map => "Map",
    Char => "Char",
    Unit => "Unit",
    Duration => "Duration",
    Float => "Float",
    Trap => "Trap",
}

impl BuiltinType {
    #[must_use]
    pub const fn is_channel_handle(self) -> bool {
        matches!(self, Self::Sender | Self::Receiver)
    }

    #[must_use]
    pub const fn is_collection(self) -> bool {
        matches!(self, Self::Vec | Self::HashMap | Self::HashSet)
    }

    #[must_use]
    pub const fn is_actor_handle(self) -> bool {
        matches!(self, Self::ActorRef | Self::Actor | Self::LocalPid)
    }

    #[must_use]
    pub const fn is_substrate_handle(self) -> bool {
        matches!(
            self,
            Self::Duplex | Self::Sink | Self::Stream | Self::SendHalf | Self::RecvHalf
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
}
