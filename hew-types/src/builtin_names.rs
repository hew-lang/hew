//! Canonical builtin type vocabulary and method metadata.
//!
//! These tables are shared by type normalization, builtin method resolution,
//! checker-owned rewrite selection, and analysis/LSP surfaces so that
//! qualified and unqualified spellings converge on one model.

use crate::check::{FnSig, TypeDef, TypeDefKind};
use crate::Ty;
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
            "next" => {
                signature: ReturnOptionT,
                runtime: BuiltinMethodRuntime::ElementOverload {
                    string_symbol: "hew_stream_next",
                    bytes_symbol: "hew_stream_next_bytes",
                }
            },
            "collect" => {
                signature: ReturnString,
                runtime: BuiltinMethodRuntime::Fixed("hew_stream_collect_string")
            },
            "close" => {
                signature: ReturnUnit,
                runtime: BuiltinMethodRuntime::Fixed("hew_stream_close")
            },
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
            "write" => {
                signature: ValueToUnit,
                runtime: BuiltinMethodRuntime::ElementOverload {
                    string_symbol: "hew_sink_write_string",
                    bytes_symbol: "hew_sink_write_bytes",
                }
            },
            "flush" => {
                signature: ReturnUnit,
                runtime: BuiltinMethodRuntime::Fixed("hew_sink_flush")
            },
            "close" => {
                signature: ReturnUnit,
                runtime: BuiltinMethodRuntime::Fixed("hew_sink_close")
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
    builtin_named_types()
        .iter()
        .find(|info| name == info.canonical_name || name == info.qualified_name)
        .map(|info| info.kind)
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
                Some("String") => Some(string_symbol),
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
