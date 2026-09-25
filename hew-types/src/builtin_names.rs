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
    ValueToSendResult,
    CloneSelf,
    ReturnOptionT,
    ReturnString,
    ReturnUnit,
    ReturnBool,
    ReturnContainerOfString,
    CountToSelf,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BuiltinMethodRuntime {
    None,
    Fixed(&'static str),
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

        }
    };
}

const fn builtin_named_type_index(kind: BuiltinNamedType) -> usize {
    match kind {
        BuiltinNamedType::Stream => 0,
        BuiltinNamedType::Sink => 1,
        BuiltinNamedType::RemotePid => 2,
        BuiltinNamedType::CancellationToken => 3,
    }
}

builtin_named_types! {
    Stream {
        consts: (STREAM, QUALIFIED_STREAM),
        methods_const: STREAM_METHODS,
        canonical: "Stream",
        qualified: "stream.Stream",
        methods: [
            // The read half of a pipe. `recv` parks until an item, EOF or a
            // fault is ready; `try_recv` never parks. Both ride the
            // element-layout witness entries, one symbol for every
            // describable element type. `lines`, `chunks` and `take` are the
            // lazy adaptors: each consumes its source stream and returns a
            // fresh one.
            "recv" => {
                signature: ReturnOptionT,
                runtime: BuiltinMethodRuntime::Fixed("hew_stream_next_layout")
            },
            "try_recv" => {
                signature: ReturnOptionT,
                runtime: BuiltinMethodRuntime::Fixed("hew_stream_try_next_layout")
            },
            "close" => {
                signature: ReturnUnit,
                runtime: BuiltinMethodRuntime::Fixed("hew_stream_close")
            },
            // `Stream<bytes>.lines()` is the `frames(Lines)` case of the
            // codec seam: one newline-terminated item at a time, as text.
            "lines" => {
                signature: ReturnContainerOfString,
                runtime: BuiltinMethodRuntime::Fixed("hew_stream_lines")
            },
            "chunks" => {
                signature: CountToSelf,
                runtime: BuiltinMethodRuntime::Fixed("hew_stream_chunks")
            },
            "take" => {
                signature: CountToSelf,
                runtime: BuiltinMethodRuntime::Fixed("hew_stream_take")
            },
            // String-specific collect: drains a Stream<string> into a single
            // string. Only string elements have a runtime symbol; other element
            // types are rejected by the checker's element-type gate before this
            // table is consulted.
            "collect" => {
                signature: ReturnString,
                runtime: BuiltinMethodRuntime::Fixed("hew_stream_collect_string")
            },
        ]
    },
    Sink {
        consts: (SINK, QUALIFIED_SINK),
        methods_const: SINK_METHODS,
        canonical: "Sink",
        qualified: "stream.Sink",
        methods: [
            // The write half of a pipe. `send` parks on a full pipe and
            // reports `SendError.Closed` once the reader is gone; `try_send`
            // never parks and adds `SendError.Full`. Both carry every
            // describable element through the layout witness. `clone` adds a
            // producer handle; `finish` publishes EOF and keeps the handle;
            // `close` finishes and releases it.
            "send" => {
                signature: ValueToSendResult,
                runtime: BuiltinMethodRuntime::Fixed("hew_stream_send_layout")
            },
            "try_send" => {
                signature: ValueToSendResult,
                runtime: BuiltinMethodRuntime::Fixed("hew_stream_try_send_layout")
            },
            "clone" => {
                signature: CloneSelf,
                runtime: BuiltinMethodRuntime::Fixed("hew_sink_clone")
            },
            "finish" => {
                signature: ReturnUnit,
                runtime: BuiltinMethodRuntime::Fixed("hew_sink_finish")
            },
            "close" => {
                signature: ReturnUnit,
                runtime: BuiltinMethodRuntime::Fixed("hew_sink_close")
            },
        ]
    },
    // RemotePid<T>: actor pid on a remote node.
    //
    // The constructor is `Node::lookup<T>(name) ->
    // Result<RemotePid<T>, LookupError>`, which snapshots the full registration
    // Location the StaleRef boundary tracks. `RemotePid<T>` does NOT unify with
    // an actor handle and has no provenance-free public constructor.
    //
    // `.send` returns Result<(), SendError>; a captured ref whose registration
    // was superseded fails closed with `SendError::StaleRef`.
    RemotePid {
        consts: (REMOTE_PID, QUALIFIED_REMOTE_PID),
        methods_const: REMOTE_PID_METHODS,
        canonical: "RemotePid",
        qualified: "RemotePid",
        methods: []
    },
    CancellationToken {
        consts: (CANCELLATION_TOKEN, QUALIFIED_CANCELLATION_TOKEN),
        methods_const: CANCELLATION_TOKEN_METHODS,
        canonical: "CancellationToken",
        qualified: "CancellationToken",
        methods: [
            "is_cancelled" => {
                signature: ReturnBool,
                runtime: BuiltinMethodRuntime::None
            },
        ]
    },
}

#[must_use]
pub const fn builtin_named_types() -> &'static [BuiltinNamedTypeInfo] {
    BUILTIN_NAMED_TYPES
}

#[must_use]
pub fn builtin_named_type(name: &str) -> Option<BuiltinNamedType> {
    match crate::lookup_builtin_type(name) {
        Some(BuiltinType::Stream) => Some(BuiltinNamedType::Stream),
        Some(BuiltinType::Sink) => Some(BuiltinNamedType::Sink),
        Some(BuiltinType::RemotePid) => Some(BuiltinNamedType::RemotePid),
        Some(BuiltinType::CancellationToken) => Some(BuiltinNamedType::CancellationToken),
        Some(
            BuiltinType::Option
            | BuiltinType::Result
            | BuiltinType::Vec
            | BuiltinType::HashMap
            | BuiltinType::HashSet
            | BuiltinType::VecIter
            | BuiltinType::HashMapIter
            | BuiltinType::Task
            | BuiltinType::ActorCall
            | BuiltinType::SupervisorPool
            | BuiltinType::ChildRef
            | BuiltinType::Generator
            | BuiltinType::Range
            | BuiltinType::Rc
            | BuiltinType::Weak
            | BuiltinType::NodeId
            | BuiltinType::Location
            | BuiltinType::HewActor
            | BuiltinType::BoxedActor
            | BuiltinType::ActorState
            | BuiltinType::MachineState
            | BuiltinType::ActorHandle
            | BuiltinType::ActorFn
            | BuiltinType::CrashInfo
            | BuiltinType::CrashAction
            | BuiltinType::CrashNotification
            | BuiltinType::CrashKind
            | BuiltinType::MonitorId
            | BuiltinType::DownTarget
            | BuiltinType::DownReason
            | BuiltinType::DownNotification
            | BuiltinType::SendError
            | BuiltinType::NodeError
            | BuiltinType::LookupError
            | BuiltinType::LinkError
            | BuiltinType::MonitorError
            | BuiltinType::MonitorRef
            | BuiltinType::Iterator
            | BuiltinType::Unit
            | BuiltinType::Duration
            | BuiltinType::Instant
            | BuiltinType::Trap
            | BuiltinType::TimeoutError
            | BuiltinType::JsonValue
            | BuiltinType::YamlValue,
        )
        | None => None,
    }
}

#[must_use]
pub fn builtin_method_info(
    kind: BuiltinNamedType,
    method: &str,
) -> Option<&'static BuiltinMethodInfo> {
    kind.info().methods.iter().find(|info| info.name == method)
}

fn type_param_ty() -> Ty {
    Ty::param("T")
}

fn self_container_ty(kind: BuiltinNamedType, inner: Ty) -> Ty {
    let builtin = crate::builtin_type::lookup_builtin_type(kind.canonical_name())
        .expect("every builtin named type is a builtin type");
    Ty::named_head(crate::TypeHead::Builtin(builtin), vec![inner])
}

impl BuiltinMethodSigTemplate {
    fn instantiate(self, owner: BuiltinNamedType) -> FnSig {
        let item_ty = type_param_ty();
        match self {
            Self::ValueToSendResult => FnSig {
                param_names: vec!["item".to_string()],
                params: vec![item_ty],
                return_type: Ty::result(Ty::Unit, Ty::send_error()),
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
            Self::ReturnBool => FnSig {
                return_type: Ty::Bool,
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
        }
    }
}

impl BuiltinMethodRuntime {
    const fn resolve(self) -> Option<&'static str> {
        match self {
            Self::None => None,
            Self::Fixed(symbol) => Some(symbol),
        }
    }
}

#[must_use]
pub fn resolve_builtin_method_symbol(kind: BuiltinNamedType, method: &str) -> Option<&'static str> {
    builtin_method_info(kind, method).and_then(|info| info.runtime.resolve())
}

/// True when the runtime symbol a `.method()` call rewrites to TAKES OWNERSHIP
/// of (consumes) its receiver handle.
///
/// These are the `@resource` handle-release builtins: dropping the last handle
/// closes the underlying resource (`Stream`/`Sink`). A consuming call moves the receiver out, so
/// the receiver's scope-exit drop must NOT fire again — a second `close` is a
/// double `Box::from_raw` / double-free. HIR lowers a consuming receiver with
/// `IntentKind::Consume` so the MIR move-checker excludes the handle from the
/// function-exit drop set (`raii-null-after-move`, `cleanup-all-exits`).
///
/// Keyed on the resolved runtime SYMBOL (the dispatch discriminant), never a
/// receiver type name, so a new handle family that routes through one of these
/// symbols is covered without a separate type-name allow-list, and a borrowing
/// method (`send`/`recv`/`try_send`/`try_recv`) is never mis-marked
/// (LESSONS: drop-allowset-from-value-flow). Any symbol the allow-set does not
/// name is treated as borrowing — fail-closed toward leak-not-double-free: a
/// missed consume-mark leaks the handle (drop fires once, on a still-live
/// handle), it never double-frees.
#[must_use]
pub fn runtime_symbol_consumes_receiver(c_symbol: &str) -> bool {
    // Family-keyed: the closed-set verdict lives on
    // `RuntimeCallFamily::consumes_receiver` (one authority, exhaustively
    // matchable). The TCP attach member of that catalogue reads its first-arg
    // consume fact from the generated FFI ownership table; the remaining
    // closed-set members are literal runtime families. A string outside the
    // catalog maps to `None` → borrowing, preserving the fail-closed
    // leak-not-double-free default for open-set `#[extern_symbol]` strings.
    crate::runtime_call::RuntimeCallFamily::from_c_symbol(c_symbol)
        .is_some_and(crate::runtime_call::RuntimeCallFamily::consumes_receiver)
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
                        type_params: if info.kind == BuiltinNamedType::CancellationToken {
                            Vec::new()
                        } else {
                            vec!["T".to_string()]
                        },
                        bounds: HashMap::new(),
                        fields: HashMap::new(),
                        variants: HashMap::new(),
                        methods: builtin_method_sigs(info.kind).clone(),
                        doc_comment: None,
                        field_order: vec![],
                        is_indirect: false,
                    },
                )
            })
            .collect()
    })[&kind]
}
