//! Canonical builtin type vocabulary and method metadata.
//!
//! These tables are shared by type normalization, builtin method resolution,
//! checker-owned rewrite selection, and analysis/LSP surfaces so that
//! qualified and unqualified spellings converge on one model.

use crate::check::{FnSig, TypeDef, TypeDefKind};
use crate::{BuiltinType, Ty};
use std::collections::HashMap;
use std::sync::OnceLock;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BuiltinMethodSigTemplate {
    ValueToUnit,
    CloneSelf,
    ReturnOptionT,
    ReturnString,
    ReturnUnit,
    ReturnContainerOfString,
    CountToSelf,
    MapperToSelf,
    PredicateToSelf,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BuiltinMethodRuntime {
    None,
    Fixed(&'static str),
    IntegerOverload {
        default_symbol: &'static str,
        integer_symbol: &'static str,
    },
    ElementOverload {
        string_symbol: &'static str,
        bytes_symbol: &'static str,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct BuiltinMethodInfo {
    pub name: &'static str,
    pub sig_template: BuiltinMethodSigTemplate,
    pub runtime: BuiltinMethodRuntime,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct BuiltinNamedTypeInfo {
    pub kind: BuiltinNamedType,
    pub canonical_name: &'static str,
    pub qualified_name: &'static str,
    pub methods: &'static [BuiltinMethodInfo],
}

macro_rules! builtin_named_types {
    (
        $(
            $variant:ident {
                consts: ($canonical_const:ident, $qualified_const:ident),
                methods_const: $methods_const:ident,
                canonical: $canonical:literal,
                qualified: $qualified:literal,
                methods: [
                    $(
                        $method_name:literal => {
                            signature: $signature:ident,
                            runtime: $runtime:expr
                        }
                    ),* $(,)?
                ]
            }
        ),* $(,)?
    ) => {
        $(
            pub const $canonical_const: &str = $canonical;
            pub const $qualified_const: &str = $qualified;

            const $methods_const: &[BuiltinMethodInfo] = &[
                $(
                    BuiltinMethodInfo {
                        name: $method_name,
                        sig_template: BuiltinMethodSigTemplate::$signature,
                        runtime: $runtime,
                    },
                )*
            ];
        )*

        /// Builtin named types whose resolution is intrinsic to the compiler.
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
        pub enum BuiltinNamedType {
            $($variant),*
        }

        const BUILTIN_NAMED_TYPES: &[BuiltinNamedTypeInfo] = &[
            $(
                BuiltinNamedTypeInfo {
                    kind: BuiltinNamedType::$variant,
                    canonical_name: $canonical_const,
                    qualified_name: $qualified_const,
                    methods: $methods_const,
                },
            )*
        ];

        impl BuiltinNamedType {
            #[must_use]
            pub const fn canonical_name(self) -> &'static str {
                match self {
                    $(Self::$variant => $canonical_const),*
                }
            }

            #[must_use]
            pub const fn qualified_name(self) -> &'static str {
                match self {
                    $(Self::$variant => $qualified_const),*
                }
            }

            #[must_use]
            pub const fn info(self) -> &'static BuiltinNamedTypeInfo {
                match self {
                    $(Self::$variant => &BUILTIN_NAMED_TYPES[builtin_named_type_index(Self::$variant)]),*
                }
            }

            #[must_use]
            pub const fn is_channel_handle(self) -> bool {
                matches!(self, Self::Sender | Self::Receiver)
            }
        }
    };
}

const fn builtin_named_type_index(kind: BuiltinNamedType) -> usize {
    match kind {
        BuiltinNamedType::Sender => 0,
        BuiltinNamedType::Receiver => 1,
        BuiltinNamedType::Stream => 2,
        BuiltinNamedType::Sink => 3,
        BuiltinNamedType::Duplex => 4,
        BuiltinNamedType::LocalPid => 5,
        BuiltinNamedType::RemotePid => 6,
    }
}

builtin_named_types! {
    Sender {
        consts: (SENDER, QUALIFIED_SENDER),
        methods_const: SENDER_METHODS,
        canonical: "Sender",
        qualified: "channel.Sender",
        methods: [
            "send" => {
                signature: ValueToUnit,
                runtime: BuiltinMethodRuntime::IntegerOverload {
                    default_symbol: "hew_channel_send",
                    integer_symbol: "hew_channel_send_int",
                }
            },
            "clone" => {
                signature: CloneSelf,
                runtime: BuiltinMethodRuntime::Fixed("hew_channel_sender_clone")
            },
            "close" => {
                signature: ReturnUnit,
                runtime: BuiltinMethodRuntime::Fixed("hew_channel_sender_close")
            },
        ]
    },
    Receiver {
        consts: (RECEIVER, QUALIFIED_RECEIVER),
        methods_const: RECEIVER_METHODS,
        canonical: "Receiver",
        qualified: "channel.Receiver",
        methods: [
            "recv" => {
                signature: ReturnOptionT,
                runtime: BuiltinMethodRuntime::IntegerOverload {
                    default_symbol: "hew_channel_recv",
                    integer_symbol: "hew_channel_recv_int",
                }
            },
            "try_recv" => {
                signature: ReturnOptionT,
                runtime: BuiltinMethodRuntime::IntegerOverload {
                    default_symbol: "hew_channel_try_recv",
                    integer_symbol: "hew_channel_try_recv_int",
                }
            },
            "close" => {
                signature: ReturnUnit,
                runtime: BuiltinMethodRuntime::Fixed("hew_channel_receiver_close")
            },
        ]
    },
    Stream {
        consts: (STREAM, QUALIFIED_STREAM),
        methods_const: STREAM_METHODS,
        canonical: "Stream",
        qualified: "stream.Stream",
        methods: [
            // Channel-family naming: recv/close mirror Duplex and RecvHalf.
            // Iterator-style aliases (.next, .lines, .collect) are removed from
            // the fundamental surface; they land via trait impls in stdlib work.
            "recv" => {
                signature: ReturnOptionT,
                runtime: BuiltinMethodRuntime::ElementOverload {
                    string_symbol: "hew_stream_next",
                    bytes_symbol: "hew_stream_next_bytes",
                }
            },
            "try_recv" => {
                signature: ReturnOptionT,
                runtime: BuiltinMethodRuntime::ElementOverload {
                    string_symbol: "hew_stream_try_next",
                    bytes_symbol: "hew_stream_try_next_bytes",
                }
            },
            "close" => {
                signature: ReturnUnit,
                runtime: BuiltinMethodRuntime::Fixed("hew_stream_close")
            },
            "chunks" => {
                signature: CountToSelf,
                runtime: BuiltinMethodRuntime::Fixed("hew_stream_chunks")
            },
            "take" => {
                signature: CountToSelf,
                runtime: BuiltinMethodRuntime::Fixed("hew_stream_take")
            },
            "map" => {
                signature: MapperToSelf,
                runtime: BuiltinMethodRuntime::None
            },
            "filter" => {
                signature: PredicateToSelf,
                runtime: BuiltinMethodRuntime::None
            },
        ]
    },
    Sink {
        consts: (SINK, QUALIFIED_SINK),
        methods_const: SINK_METHODS,
        canonical: "Sink",
        qualified: "stream.Sink",
        methods: [
            // Channel-family naming: send/close mirror Duplex and SendHalf.
            // .write and .flush are removed from the fundamental surface; they
            // may re-surface via an I/O-sink trait in stdlib work.
            "send" => {
                signature: ValueToUnit,
                runtime: BuiltinMethodRuntime::ElementOverload {
                    string_symbol: "hew_sink_write_string",
                    bytes_symbol: "hew_sink_write_bytes",
                }
            },
            "try_send" => {
                signature: ValueToUnit,
                runtime: BuiltinMethodRuntime::ElementOverload {
                    string_symbol: "hew_sink_try_write_string",
                    bytes_symbol: "hew_sink_try_write_bytes",
                }
            },
            "close" => {
                signature: ReturnUnit,
                runtime: BuiltinMethodRuntime::Fixed("hew_sink_close")
            },
        ]
    },
    // Duplex<S, R>: bidirectional lambda-actor handle.
    //
    // S = send direction (msg type), R = receive direction (reply type).
    // @resource: dropping the last handle closes both directions.
    // Send iff S: Send + R: Send (checked in traits.rs implements_marker).
    // Call-syntax `handle(msg)` is canonical for lambda-actor handles; `.send()` is
    // accepted as an allowed-secondary surface because lambda-actor handles are
    // `Duplex<Msg, Reply>` underneath — both surfaces route to the same runtime symbol
    // (`hew_duplex_send`).  The type system cannot distinguish them at the call site.
    Duplex {
        consts: (DUPLEX, QUALIFIED_DUPLEX),
        methods_const: DUPLEX_METHODS,
        canonical: "Duplex",
        qualified: "duplex.Duplex",
        methods: []
    },
    // LocalPid<T>: actor pid in this process, returned by `spawn`.
    //
    // A `LocalPid<T>` is process-local: it refers to an actor running in the current
    // node. Built-in actor functions (`close`, `link`, `monitor`, etc.) use
    // `LocalPid<T>` directly; it is nominally distinct from `ActorRef<T>`.
    //
    // Methods (`.tell`) are declared in `std/builtins.hew` as `impl LocalPid<T>` and
    // resolved via the normal user-type method dispatch path.
    LocalPid {
        consts: (LOCAL_PID, QUALIFIED_LOCAL_PID),
        methods_const: LOCAL_PID_METHODS,
        canonical: "LocalPid",
        qualified: "LocalPid",
        methods: []
    },
    // RemotePid<T>: actor pid on a remote node.
    //
    // A `RemotePid<T>` is produced by peer-discovery or explicit construction
    // (`RemotePid::from_raw`). It does NOT unify with `ActorRef<T>` or `LocalPid<T>`.
    // Coercion from local → remote is explicit: `local_pid.to_remote_via(node_handle)`.
    //
    // `.tell` returns Result<(), SendError> and fails closed until actual routing arrives.
    //
    // SHIM: `RemotePid::from_raw` is S1 scaffolding;
    //       remove / replace when `hew_actor_send_remote` ABI lands in S4.
    RemotePid {
        consts: (REMOTE_PID, QUALIFIED_REMOTE_PID),
        methods_const: REMOTE_PID_METHODS,
        canonical: "RemotePid",
        qualified: "RemotePid",
        methods: []
    },
}

#[must_use]
pub const fn builtin_named_types() -> &'static [BuiltinNamedTypeInfo] {
    BUILTIN_NAMED_TYPES
}

#[must_use]
pub fn builtin_named_type(name: &str) -> Option<BuiltinNamedType> {
    match crate::lookup_builtin_type(name) {
        Some(BuiltinType::Sender) => Some(BuiltinNamedType::Sender),
        Some(BuiltinType::Receiver) => Some(BuiltinNamedType::Receiver),
        Some(BuiltinType::Stream) => Some(BuiltinNamedType::Stream),
        Some(BuiltinType::Sink) => Some(BuiltinNamedType::Sink),
        Some(BuiltinType::Duplex) => Some(BuiltinNamedType::Duplex),
        Some(BuiltinType::LocalPid) => Some(BuiltinNamedType::LocalPid),
        Some(BuiltinType::RemotePid) => Some(BuiltinNamedType::RemotePid),
        Some(
            BuiltinType::Option
            | BuiltinType::Result
            | BuiltinType::Vec
            | BuiltinType::HashMap
            | BuiltinType::HashSet
            | BuiltinType::ActorRef
            | BuiltinType::Actor
            | BuiltinType::Task
            | BuiltinType::StreamPair
            | BuiltinType::Generator
            | BuiltinType::AsyncGenerator
            | BuiltinType::Range
            | BuiltinType::Rc
            | BuiltinType::SendHalf
            | BuiltinType::RecvHalf
            | BuiltinType::LambdaActorHandle
            | BuiltinType::CrashInfo
            | BuiltinType::CrashAction
            | BuiltinType::SendError
            | BuiltinType::AskError
            | BuiltinType::RecvError
            | BuiltinType::LinkError
            | BuiltinType::MonitorRef
            | BuiltinType::NarrowError
            | BuiltinType::CloseError
            | BuiltinType::Iterator
            | BuiltinType::String
            | BuiltinType::Map
            | BuiltinType::Char
            | BuiltinType::Unit
            | BuiltinType::Duration
            | BuiltinType::Float
            | BuiltinType::Trap,
        )
        | None => None,
    }
}

#[must_use]
pub fn builtin_named_type_info(kind: BuiltinNamedType) -> &'static BuiltinNamedTypeInfo {
    kind.info()
}

#[must_use]
pub fn builtin_method_info(
    kind: BuiltinNamedType,
    method: &str,
) -> Option<&'static BuiltinMethodInfo> {
    kind.info().methods.iter().find(|info| info.name == method)
}

fn type_param_ty() -> Ty {
    Ty::Named {
        builtin: None,
        name: "T".to_string(),
        args: vec![],
    }
}

fn self_container_ty(kind: BuiltinNamedType, inner: Ty) -> Ty {
    Ty::normalize_named(kind.canonical_name().to_string(), vec![inner])
}

impl BuiltinMethodSigTemplate {
    fn instantiate(self, owner: BuiltinNamedType) -> FnSig {
        let item_ty = type_param_ty();
        let item_fn = Ty::Function {
            params: vec![item_ty.clone()],
            ret: Box::new(item_ty.clone()),
        };
        let item_predicate = Ty::Function {
            params: vec![item_ty.clone()],
            ret: Box::new(Ty::Bool),
        };
        match self {
            Self::ValueToUnit => FnSig {
                param_names: vec!["value".to_string()],
                params: vec![item_ty],
                return_type: Ty::Unit,
                ..FnSig::default()
            },
            Self::CloneSelf => FnSig {
                return_type: self_container_ty(owner, item_ty),
                ..FnSig::default()
            },
            Self::ReturnOptionT => FnSig {
                return_type: Ty::option(item_ty),
                ..FnSig::default()
            },
            Self::ReturnString => FnSig {
                return_type: Ty::String,
                ..FnSig::default()
            },
            Self::ReturnUnit => FnSig {
                return_type: Ty::Unit,
                ..FnSig::default()
            },
            Self::ReturnContainerOfString => FnSig {
                return_type: self_container_ty(owner, Ty::String),
                ..FnSig::default()
            },
            Self::CountToSelf => FnSig {
                param_names: vec!["count".to_string()],
                params: vec![Ty::I64],
                return_type: self_container_ty(owner, item_ty),
                ..FnSig::default()
            },
            Self::MapperToSelf => FnSig {
                param_names: vec!["mapper".to_string()],
                params: vec![item_fn],
                return_type: self_container_ty(owner, item_ty),
                ..FnSig::default()
            },
            Self::PredicateToSelf => FnSig {
                param_names: vec!["predicate".to_string()],
                params: vec![item_predicate],
                return_type: self_container_ty(owner, item_ty),
                ..FnSig::default()
            },
        }
    }
}

impl BuiltinMethodRuntime {
    fn resolve(self, element_ty: Option<&Ty>, element_name: Option<&str>) -> Option<&'static str> {
        match self {
            Self::None => None,
            Self::Fixed(symbol) => Some(symbol),
            Self::IntegerOverload {
                default_symbol,
                integer_symbol,
            } => Some(if element_ty.is_some_and(Ty::is_integer) {
                integer_symbol
            } else {
                default_symbol
            }),
            Self::ElementOverload {
                string_symbol,
                bytes_symbol,
            } => match element_name {
                Some("string") => Some(string_symbol),
                Some("bytes") => Some(bytes_symbol),
                _ => None,
            },
        }
    }
}

#[must_use]
pub fn resolve_builtin_method_symbol(
    kind: BuiltinNamedType,
    method: &str,
    element_ty: Option<&Ty>,
    element_name: Option<&str>,
) -> Option<&'static str> {
    builtin_method_info(kind, method)
        .and_then(|info| info.runtime.resolve(element_ty, element_name))
}

static BUILTIN_METHOD_SIGS: OnceLock<HashMap<BuiltinNamedType, HashMap<String, FnSig>>> =
    OnceLock::new();
static BUILTIN_TYPE_DEFS: OnceLock<HashMap<BuiltinNamedType, TypeDef>> = OnceLock::new();

#[must_use]
pub fn builtin_method_sigs(kind: BuiltinNamedType) -> &'static HashMap<String, FnSig> {
    &BUILTIN_METHOD_SIGS.get_or_init(|| {
        builtin_named_types()
            .iter()
            .map(|info| {
                (
                    info.kind,
                    info.methods
                        .iter()
                        .map(|method| {
                            (
                                method.name.to_string(),
                                method.sig_template.instantiate(info.kind),
                            )
                        })
                        .collect(),
                )
            })
            .collect()
    })[&kind]
}

#[must_use]
pub fn builtin_type_def(kind: BuiltinNamedType) -> &'static TypeDef {
    &BUILTIN_TYPE_DEFS.get_or_init(|| {
        builtin_named_types()
            .iter()
            .map(|info| {
                (
                    info.kind,
                    TypeDef {
                        kind: TypeDefKind::Struct,
                        name: info.canonical_name.to_string(),
                        type_params: vec!["T".to_string()],
                        fields: HashMap::new(),
                        variants: HashMap::new(),
                        methods: builtin_method_sigs(info.kind).clone(),
                        doc_comment: None,
                        is_indirect: false,
                    },
                )
            })
            .collect()
    })[&kind]
}

/// Resolve a builtin named type spelling to its canonical short name.
#[must_use]
pub fn canonical_builtin_named_type_name(name: &str) -> Option<&'static str> {
    builtin_named_type(name).map(BuiltinNamedType::canonical_name)
}
