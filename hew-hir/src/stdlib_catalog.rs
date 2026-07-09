//! HIR-visible stdlib builtin catalog.
//!
//! This is the resolver-side catalog only. `RuntimeFfiShim` entries name real
//! exported runtime symbols; compiler/codegen magic uses distinct linkage
//! variants so the catalog never pretends a HIR shim name is a C ABI symbol.

use hew_types::{MathGenericOp, ResolvedTy};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BuiltinClass {
    ClassA,
    ClassB,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PrintKind {
    I32,
    I64,
    U8,
    U32,
    U64,
    F64,
    Bool,
    Str,
}

#[must_use]
pub fn generic_math_intrinsic_callee(
    op: MathGenericOp,
    operand_ty: &ResolvedTy,
) -> Option<(&'static str, ResolvedTy)> {
    match operand_ty {
        ResolvedTy::I64 => Some((
            match op {
                MathGenericOp::Abs => "abs",
                MathGenericOp::Min => "min",
                MathGenericOp::Max => "max",
            },
            ResolvedTy::I64,
        )),
        ResolvedTy::F64 => Some((
            match op {
                MathGenericOp::Abs => "abs_f",
                MathGenericOp::Min => "min_f",
                MathGenericOp::Max => "max_f",
            },
            ResolvedTy::F64,
        )),
        _ => None,
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BuiltinLinkage {
    RuntimeFfiShim {
        symbol: &'static str,
    },
    // NOTE: `PrintIntercept` and `ToStringShim` below are retained as transitional
    // linkage variants. The L3 Display + f-strings work routes interpolants
    // through the lang-item registry and user `impl Display for T` methods,
    // making these shims redundant at the surface level — but their actual
    // retirement (and the print-macro rewrites that depend on it) belongs to
    // the Phase-2/3 codegen-rs lane (#340γ follow-on). Deletion is intentionally
    // deferred to that work to keep this revision boundary surgical.
    PrintIntercept {
        runtime_symbol: &'static str,
        kind: PrintKind,
        newline: bool,
    },
    ToStringShim {
        symbol: &'static str,
    },
    StringCloneShim {
        symbol: &'static str,
    },
    CompilerIntrinsic {
        intrinsic: &'static str,
    },
    /// Catalog row whose call site lowers as `Terminator::Call` and is
    /// intercepted in codegen by callee-name dispatch, requiring no LLVM
    /// extern of its own. Unlike `CompilerIntrinsic` (which is excluded
    /// from MIR's `module_fn_names` because numeric intrinsics lower
    /// through method-call rewrites rather than direct calls), entries
    /// using this linkage MUST appear in `module_fn_names` so that
    /// `Expr::Call` lowering reaches `Terminator::Call`. Codegen never
    /// predeclares an LLVM function for these — the call is handled
    /// before the `FnSymbol::Real` arm runs — so the catalog inventory
    /// gate treats them like `CompilerIntrinsic` (no C-ABI symbol to
    /// classify).
    ///
    /// Used by S5 `hew_remote_pid_send`: the user-visible call expands
    /// into `hew_actor_send_by_id` (declared via `intern_runtime_decl`)
    /// followed by an in-place `Result<(), SendError>` construction; no
    /// separate stub symbol is needed.
    CalleeNameDispatchOnly,
    /// `Node::register<T>(name, pid)` — register an actor by bare PID.
    ///
    /// Per registry R81 (2026-05-23), `LocalPid<T>` lowers to a `u64` at
    /// the C-ABI boundary, not a `*mut HewActor`. Codegen must therefore
    /// synthesise a two-step call sequence:
    /// 1. `hew_actor_pid(actor_ptr: ptr) -> u64` — extract the numeric PID
    ///    from the `LocalPid<T>` alloca (which is a `ptr` in LLVM).
    /// 2. `hew_node_api_register_by_pid(name: ptr, pid: u64) -> i32` — the
    ///    actual C-ABI registration call.
    ///
    /// `RuntimeFfiShim` cannot be used here because the LLVM call sequence
    /// requires an intermediate PID-extraction step that the generic
    /// `Terminator::Call` handler cannot express.
    NodeRegisterByPid {
        register_symbol: &'static str,
        pid_accessor: &'static str,
    },
    /// Catalog row that *declares* a `#[no_mangle] pub static` runtime symbol
    /// (a `HewMapKeyLayout` or `HewMapValueLayout` instance) rather than a
    /// function. Used by W4.001 Stage C0b layout-descriptor entries.
    ///
    /// **Not a callable**: codegen does not predeclare an LLVM function for
    /// this linkage, and MIR `module_fn_names` does not include the symbol —
    /// the symbol is consulted at Stage C call-site materialisation, where
    /// codegen takes the address of the corresponding extern static through
    /// `hew-cabi::map`.
    ///
    /// **Why this lives in the catalog at all (C0b boundary, plan §4):** to
    /// give checker-visible code a single source of truth for which descriptor
    /// symbols the runtime exports, and to drive the
    /// `stdlib_catalog_layout_descriptor_coverage` coverage gate.
    LayoutDescriptorSymbol {
        symbol: &'static str,
        role: LayoutDescriptorRole,
    },
}

/// Which descriptor flavour a `LayoutDescriptorSymbol` row names.
///
/// Mirrors the two cabi structs (`HewMapKeyLayout`, `HewMapValueLayout`) so
/// the coverage test can enumerate K-vs-V scope separately.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LayoutDescriptorRole {
    /// `HewMapKeyLayout` static (`hew_layout_key_<type>`).
    Key,
    /// `HewMapValueLayout` static (`hew_layout_val_<type>`).
    Value,
}

impl BuiltinLinkage {
    #[must_use]
    pub const fn runtime_symbol(self) -> Option<&'static str> {
        match self {
            Self::RuntimeFfiShim { symbol }
            | Self::ToStringShim { symbol }
            | Self::StringCloneShim { symbol } => Some(symbol),
            Self::PrintIntercept { .. }
            | Self::CompilerIntrinsic { .. }
            | Self::CalleeNameDispatchOnly
            | Self::NodeRegisterByPid { .. }
            | Self::LayoutDescriptorSymbol { .. } => None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BuiltinTy {
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    F32,
    F64,
    Bool,
    Char,
    String,
    Bytes,
    Unit,
    Never,
    VecAny,
    /// Erased `HashMap<K, V>` receiver placeholder used by layout-backed
    /// catalog rows whose linkage is `CalleeNameDispatchOnly`.  These rows
    /// exist only to register the runtime symbol name with HIR's
    /// `fn_registry` and MIR's `module_fn_names`; their `params` are not
    /// consulted by typecheck (checker authority drives dispatch) and the
    /// concrete `K`/`V` substitution is recovered downstream from
    /// `HashMapLoweringFact`.
    HashMapAny,
    /// Erased `HashSet<T>` receiver placeholder; see `HashMapAny`.
    HashSetAny,
    /// Raw pointer placeholder (`*mut u8`) for the `mem.*` memory-intrinsic
    /// floor rows (W5.005 / F1b).  These rows use `CalleeNameDispatchOnly`
    /// linkage and are compiler-internal-only (A605); their concrete element
    /// type is recovered at the codegen interceptor from the MIR call-site
    /// operand types, so the catalog models the pointer monomorphically as
    /// `*mut u8`.  Typecheck of `mem.*` calls is driven by the floor module's
    /// surface declarations, not by these params.
    Pointer,
    /// `duration` — i64 nanoseconds at the ABI, distinct from bare `i64`.
    Duration,
    /// `instant` — i64 nanosecond monotonic timestamp; ABI-identical to `i64`.
    Instant,
}

impl BuiltinTy {
    #[must_use]
    pub fn to_resolved(self) -> ResolvedTy {
        match self {
            BuiltinTy::I8 => ResolvedTy::I8,
            BuiltinTy::I16 => ResolvedTy::I16,
            BuiltinTy::I32 => ResolvedTy::I32,
            // `instant` is an i64 nanosecond timestamp at the ABI boundary.
            BuiltinTy::I64 | BuiltinTy::Instant => ResolvedTy::I64,
            BuiltinTy::U8 => ResolvedTy::U8,
            BuiltinTy::U16 => ResolvedTy::U16,
            BuiltinTy::U32 => ResolvedTy::U32,
            BuiltinTy::U64 => ResolvedTy::U64,
            BuiltinTy::F32 => ResolvedTy::F32,
            BuiltinTy::F64 => ResolvedTy::F64,
            BuiltinTy::Bool => ResolvedTy::Bool,
            BuiltinTy::Char => ResolvedTy::Char,
            BuiltinTy::String => ResolvedTy::String,
            BuiltinTy::Bytes => ResolvedTy::Bytes,
            BuiltinTy::Unit => ResolvedTy::Unit,
            BuiltinTy::Never => ResolvedTy::Never,
            BuiltinTy::VecAny => ResolvedTy::named_builtin(
                "Vec",
                hew_types::BuiltinType::Vec,
                vec![ResolvedTy::named_user("T", vec![])],
            ),
            BuiltinTy::HashMapAny => ResolvedTy::named_builtin(
                "HashMap",
                hew_types::BuiltinType::HashMap,
                vec![
                    ResolvedTy::named_user("K", vec![]),
                    ResolvedTy::named_user("V", vec![]),
                ],
            ),
            BuiltinTy::HashSetAny => ResolvedTy::named_builtin(
                "HashSet",
                hew_types::BuiltinType::HashSet,
                vec![ResolvedTy::named_user("T", vec![])],
            ),
            BuiltinTy::Pointer => ResolvedTy::Pointer {
                is_mutable: true,
                pointee: Box::new(ResolvedTy::U8),
            },
            BuiltinTy::Duration => ResolvedTy::Duration,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct BuiltinEntry {
    pub name: &'static str,
    pub overload_of: Option<&'static str>,
    pub class: BuiltinClass,
    pub params: &'static [BuiltinTy],
    pub return_ty: BuiltinTy,
    pub linkage: BuiltinLinkage,
}

const PRINT_RUNTIME: &str = "hew_print_value";

const I32: &[BuiltinTy] = &[BuiltinTy::I32];
const I64: &[BuiltinTy] = &[BuiltinTy::I64];
const U8: &[BuiltinTy] = &[BuiltinTy::U8];
const U16: &[BuiltinTy] = &[BuiltinTy::U16];
const U32: &[BuiltinTy] = &[BuiltinTy::U32];
const U64: &[BuiltinTy] = &[BuiltinTy::U64];
const F64: &[BuiltinTy] = &[BuiltinTy::F64];
const BOOL: &[BuiltinTy] = &[BuiltinTy::Bool];
const CHAR: &[BuiltinTy] = &[BuiltinTy::Char];
const STRING: &[BuiltinTy] = &[BuiltinTy::String];
const BYTES_U8: &[BuiltinTy] = &[BuiltinTy::Bytes, BuiltinTy::U8];
const BYTES_I64: &[BuiltinTy] = &[BuiltinTy::Bytes, BuiltinTy::I64];
const BYTES_I64_U8: &[BuiltinTy] = &[BuiltinTy::Bytes, BuiltinTy::I64, BuiltinTy::U8];
const BYTES_BYTES: &[BuiltinTy] = &[BuiltinTy::Bytes, BuiltinTy::Bytes];
const U8_U8: &[BuiltinTy] = &[BuiltinTy::U8, BuiltinTy::U8];
const DURATION: &[BuiltinTy] = &[BuiltinTy::Duration];
const INSTANT: &[BuiltinTy] = &[BuiltinTy::Instant];
const BYTES: &[BuiltinTy] = &[BuiltinTy::Bytes];
const VEC_ANY: &[BuiltinTy] = &[BuiltinTy::VecAny];
const VEC_ANY_BOOL: &[BuiltinTy] = &[BuiltinTy::VecAny, BuiltinTy::Bool];
const VEC_ANY_I8: &[BuiltinTy] = &[BuiltinTy::VecAny, BuiltinTy::I8];
const VEC_ANY_U8: &[BuiltinTy] = &[BuiltinTy::VecAny, BuiltinTy::U8];
const VEC_ANY_I16: &[BuiltinTy] = &[BuiltinTy::VecAny, BuiltinTy::I16];
const VEC_ANY_U16: &[BuiltinTy] = &[BuiltinTy::VecAny, BuiltinTy::U16];
const VEC_ANY_I32: &[BuiltinTy] = &[BuiltinTy::VecAny, BuiltinTy::I32];
const VEC_ANY_I64: &[BuiltinTy] = &[BuiltinTy::VecAny, BuiltinTy::I64];
const VEC_ANY_F32: &[BuiltinTy] = &[BuiltinTy::VecAny, BuiltinTy::F32];
const VEC_ANY_F64: &[BuiltinTy] = &[BuiltinTy::VecAny, BuiltinTy::F64];
const VEC_ANY_STRING: &[BuiltinTy] = &[BuiltinTy::VecAny, BuiltinTy::String];
const VEC_ANY_PTR: &[BuiltinTy] = &[BuiltinTy::VecAny, BuiltinTy::Pointer];
const VEC_ANY_I64_PTR: &[BuiltinTy] = &[BuiltinTy::VecAny, BuiltinTy::I64, BuiltinTy::Pointer];
const VEC_ANY_I64_BOOL: &[BuiltinTy] = &[BuiltinTy::VecAny, BuiltinTy::I64, BuiltinTy::Bool];
const VEC_ANY_I64_I32: &[BuiltinTy] = &[BuiltinTy::VecAny, BuiltinTy::I64, BuiltinTy::I32];
const VEC_ANY_I64_I8: &[BuiltinTy] = &[BuiltinTy::VecAny, BuiltinTy::I64, BuiltinTy::I8];
const VEC_ANY_I64_U8: &[BuiltinTy] = &[BuiltinTy::VecAny, BuiltinTy::I64, BuiltinTy::U8];
const VEC_ANY_I64_I16: &[BuiltinTy] = &[BuiltinTy::VecAny, BuiltinTy::I64, BuiltinTy::I16];
const VEC_ANY_I64_U16: &[BuiltinTy] = &[BuiltinTy::VecAny, BuiltinTy::I64, BuiltinTy::U16];
const VEC_ANY_I64_I64: &[BuiltinTy] = &[BuiltinTy::VecAny, BuiltinTy::I64, BuiltinTy::I64];
const VEC_ANY_I64_F32: &[BuiltinTy] = &[BuiltinTy::VecAny, BuiltinTy::I64, BuiltinTy::F32];
const VEC_ANY_I64_F64: &[BuiltinTy] = &[BuiltinTy::VecAny, BuiltinTy::I64, BuiltinTy::F64];
const VEC_ANY_I64_STRING: &[BuiltinTy] = &[BuiltinTy::VecAny, BuiltinTy::I64, BuiltinTy::String];
const VEC_ANY_VEC_ANY: &[BuiltinTy] = &[BuiltinTy::VecAny, BuiltinTy::VecAny];
const I64_I64: &[BuiltinTy] = &[BuiltinTy::I64, BuiltinTy::I64];
const F64_F64: &[BuiltinTy] = &[BuiltinTy::F64, BuiltinTy::F64];
const BOOL_BOOL: &[BuiltinTy] = &[BuiltinTy::Bool, BuiltinTy::Bool];
const STRING_STRING: &[BuiltinTy] = &[BuiltinTy::String, BuiltinTy::String];
const STRING_I64: &[BuiltinTy] = &[BuiltinTy::String, BuiltinTy::I64];
const STRING_I64_I64: &[BuiltinTy] = &[BuiltinTy::String, BuiltinTy::I64, BuiltinTy::I64];
const STRING_STRING_STRING: &[BuiltinTy] =
    &[BuiltinTy::String, BuiltinTy::String, BuiltinTy::String];
const EMPTY: &[BuiltinTy] = &[];
const HASHMAP_ANY: &[BuiltinTy] = &[BuiltinTy::HashMapAny];
const HASHSET_ANY: &[BuiltinTy] = &[BuiltinTy::HashSetAny];

const fn direct(
    name: &'static str,
    class: BuiltinClass,
    params: &'static [BuiltinTy],
    return_ty: BuiltinTy,
    linkage: BuiltinLinkage,
) -> BuiltinEntry {
    BuiltinEntry {
        name,
        overload_of: None,
        class,
        params,
        return_ty,
        linkage,
    }
}

const fn overload(
    name: &'static str,
    overload_of: &'static str,
    params: &'static [BuiltinTy],
    return_ty: BuiltinTy,
    linkage: BuiltinLinkage,
) -> BuiltinEntry {
    BuiltinEntry {
        name,
        overload_of: Some(overload_of),
        class: BuiltinClass::ClassA,
        params,
        return_ty,
        linkage,
    }
}

macro_rules! print_entry {
    ($name:literal, $of:literal, $params:expr, $kind:ident, $newline:expr) => {
        overload(
            $name,
            $of,
            $params,
            BuiltinTy::Unit,
            BuiltinLinkage::PrintIntercept {
                runtime_symbol: PRINT_RUNTIME,
                kind: PrintKind::$kind,
                newline: $newline,
            },
        )
    };
}

macro_rules! tostring_entry {
    ($name:literal, $params:expr, $symbol:literal) => {
        overload(
            $name,
            "to_string",
            $params,
            BuiltinTy::String,
            BuiltinLinkage::ToStringShim { symbol: $symbol },
        )
    };
}

macro_rules! assert_entry {
    ($name:literal, $of:literal, $params:expr, $symbol:literal) => {
        overload(
            $name,
            $of,
            $params,
            BuiltinTy::Unit,
            BuiltinLinkage::RuntimeFfiShim { symbol: $symbol },
        )
    };
}

pub const CATALOG: &[BuiltinEntry] = &[
    // Class A: direct builtin calls.
    // `Vec::new()` is a compiler-lowered constructor. Codegen selects the
    // concrete runtime allocator from the checker-authoritative destination
    // type (`hew_vec_new_bool`, `hew_vec_new_i64`, `hew_vec_new_with_layout`,
    // etc.), so there is no single C ABI symbol to declare here.
    direct(
        "Vec::new",
        BuiltinClass::ClassA,
        EMPTY,
        BuiltinTy::VecAny,
        BuiltinLinkage::CalleeNameDispatchOnly,
    ),
    direct(
        "HashMap::new",
        BuiltinClass::ClassA,
        EMPTY,
        BuiltinTy::HashMapAny,
        BuiltinLinkage::CalleeNameDispatchOnly,
    ),
    direct(
        "HashSet::new",
        BuiltinClass::ClassA,
        EMPTY,
        BuiltinTy::HashSetAny,
        BuiltinLinkage::CalleeNameDispatchOnly,
    ),
    direct(
        "bytes::new",
        BuiltinClass::ClassA,
        EMPTY,
        BuiltinTy::Bytes,
        BuiltinLinkage::CalleeNameDispatchOnly,
    ),
    direct(
        "sleep",
        BuiltinClass::ClassA,
        DURATION,
        BuiltinTy::Unit,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_sleep_ns",
        },
    ),
    direct(
        "sleep_until",
        BuiltinClass::ClassA,
        INSTANT,
        BuiltinTy::Unit,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_sleep_until_ns",
        },
    ),
    direct(
        "exit",
        BuiltinClass::ClassA,
        I64,
        BuiltinTy::Never,
        BuiltinLinkage::RuntimeFfiShim { symbol: "hew_exit" },
    ),
    direct(
        "panic",
        BuiltinClass::ClassA,
        STRING,
        BuiltinTy::Never,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_panic_msg",
        },
    ),
    direct(
        "assert",
        BuiltinClass::ClassA,
        BOOL,
        BuiltinTy::Unit,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_assert",
        },
    ),
    // Class A: core math builtins lowered directly by codegen.
    direct(
        "sqrt",
        BuiltinClass::ClassA,
        F64,
        BuiltinTy::F64,
        BuiltinLinkage::CalleeNameDispatchOnly,
    ),
    direct(
        "abs",
        BuiltinClass::ClassA,
        I64,
        BuiltinTy::I64,
        BuiltinLinkage::CalleeNameDispatchOnly,
    ),
    direct(
        "min",
        BuiltinClass::ClassA,
        I64_I64,
        BuiltinTy::I64,
        BuiltinLinkage::CalleeNameDispatchOnly,
    ),
    direct(
        "max",
        BuiltinClass::ClassA,
        I64_I64,
        BuiltinTy::I64,
        BuiltinLinkage::CalleeNameDispatchOnly,
    ),
    direct(
        "abs_f",
        BuiltinClass::ClassA,
        F64,
        BuiltinTy::F64,
        BuiltinLinkage::CalleeNameDispatchOnly,
    ),
    direct(
        "min_f",
        BuiltinClass::ClassA,
        F64_F64,
        BuiltinTy::F64,
        BuiltinLinkage::CalleeNameDispatchOnly,
    ),
    direct(
        "max_f",
        BuiltinClass::ClassA,
        F64_F64,
        BuiltinTy::F64,
        BuiltinLinkage::CalleeNameDispatchOnly,
    ),
    direct(
        "pow",
        BuiltinClass::ClassA,
        F64_F64,
        BuiltinTy::F64,
        BuiltinLinkage::CalleeNameDispatchOnly,
    ),
    direct(
        "floor",
        BuiltinClass::ClassA,
        F64,
        BuiltinTy::F64,
        BuiltinLinkage::CalleeNameDispatchOnly,
    ),
    direct(
        "ceil",
        BuiltinClass::ClassA,
        F64,
        BuiltinTy::F64,
        BuiltinLinkage::CalleeNameDispatchOnly,
    ),
    direct(
        "round",
        BuiltinClass::ClassA,
        F64,
        BuiltinTy::F64,
        BuiltinLinkage::CalleeNameDispatchOnly,
    ),
    direct(
        "exp",
        BuiltinClass::ClassA,
        F64,
        BuiltinTy::F64,
        BuiltinLinkage::CalleeNameDispatchOnly,
    ),
    direct(
        "log",
        BuiltinClass::ClassA,
        F64,
        BuiltinTy::F64,
        BuiltinLinkage::CalleeNameDispatchOnly,
    ),
    direct(
        "sin",
        BuiltinClass::ClassA,
        F64,
        BuiltinTy::F64,
        BuiltinLinkage::CalleeNameDispatchOnly,
    ),
    direct(
        "cos",
        BuiltinClass::ClassA,
        F64,
        BuiltinTy::F64,
        BuiltinLinkage::CalleeNameDispatchOnly,
    ),
    // Class A: monomorphic print/println overloads.
    print_entry!("println_i32", "println", I32, I32, true),
    print_entry!("println_i64", "println", I64, I64, true),
    print_entry!("println_u8", "println", U8, U8, true),
    print_entry!("println_u32", "println", U32, U32, true),
    print_entry!("println_u64", "println", U64, U64, true),
    print_entry!("println_f64", "println", F64, F64, true),
    print_entry!("println_bool", "println", BOOL, Bool, true),
    print_entry!("println_str", "println", STRING, Str, true),
    print_entry!("print_i32", "print", I32, I32, false),
    print_entry!("print_i64", "print", I64, I64, false),
    print_entry!("print_u8", "print", U8, U8, false),
    print_entry!("print_u32", "print", U32, U32, false),
    print_entry!("print_u64", "print", U64, U64, false),
    print_entry!("print_f64", "print", F64, F64, false),
    print_entry!("print_bool", "print", BOOL, Bool, false),
    print_entry!("print_str", "print", STRING, Str, false),
    // Class A: monomorphic to_string overloads.
    tostring_entry!("to_string_i32", I32, "hew_int_to_string"),
    tostring_entry!("to_string_i64", I64, "hew_i64_to_string"),
    tostring_entry!("to_string_u8", U8, "hew_u8_to_string"),
    tostring_entry!("to_string_u16", U16, "hew_uint_to_string"),
    tostring_entry!("to_string_u32", U32, "hew_uint_to_string"),
    tostring_entry!("to_string_u64", U64, "hew_u64_to_string"),
    tostring_entry!("to_string_f64", F64, "hew_float_to_string"),
    tostring_entry!("to_string_bool", BOOL, "hew_bool_to_string"),
    tostring_entry!("to_string_char", CHAR, "hew_char_to_string"),
    overload(
        "to_string_str",
        "to_string",
        STRING,
        BuiltinTy::String,
        BuiltinLinkage::StringCloneShim {
            symbol: "hew_string_clone",
        },
    ),
    // Class A: string concatenation used by f-string interpolation lowering
    // to join Display::fmt segments.  Resolves by exact name (no overload set).
    direct(
        "string_concat",
        BuiltinClass::ClassA,
        STRING_STRING,
        BuiltinTy::String,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_string_concat",
        },
    ),
    // Class A: monomorphic assertion and len overloads.
    assert_entry!("assert_eq_i64", "assert_eq", I64_I64, "hew_assert_eq_i64"),
    assert_entry!("assert_eq_u8", "assert_eq", U8_U8, "hew_assert_eq_u8"),
    assert_entry!(
        "assert_eq_str",
        "assert_eq",
        STRING_STRING,
        "hew_assert_eq_str"
    ),
    assert_entry!("assert_eq_f64", "assert_eq", F64_F64, "hew_assert_eq_f64"),
    assert_entry!(
        "assert_eq_bool",
        "assert_eq",
        BOOL_BOOL,
        "hew_assert_eq_bool"
    ),
    assert_entry!("assert_ne_i64", "assert_ne", I64_I64, "hew_assert_ne_i64"),
    assert_entry!("assert_ne_u8", "assert_ne", U8_U8, "hew_assert_ne_u8"),
    assert_entry!(
        "assert_ne_str",
        "assert_ne",
        STRING_STRING,
        "hew_assert_ne_str"
    ),
    assert_entry!("assert_ne_f64", "assert_ne", F64_F64, "hew_assert_ne_f64"),
    assert_entry!(
        "assert_ne_bool",
        "assert_ne",
        BOOL_BOOL,
        "hew_assert_ne_bool"
    ),
    overload(
        "len_str",
        "len",
        STRING,
        BuiltinTy::I64,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_string_length",
        },
    ),
    overload(
        "len_vec",
        "len",
        VEC_ANY,
        BuiltinTy::I64,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_vec_len",
        },
    ),
    // Receiver-method rewrite targets for the `impl duration` methods declared
    // in `std/builtins.hew`. The checker's `Ty::Duration` dispatch arm records a
    // `RewriteToFunction { c_symbol: "hew_duration_*", descriptor: None }`; HIR
    // resolves that callee through the seeded `fn_registry`, so each runtime
    // symbol needs a catalog row here to be resolvable. `duration` is i64-backed:
    // the receiver is modelled as the single `I64` param so codegen's
    // `declare_catalog_ffi` declares the extern with the real `(i64) -> ...`
    // C ABI. `nanos`/`micros`/`millis`/`secs`/
    // `mins`/`hours`/`abs` return `I64`; `is_zero` returns `Bool`.
    direct(
        "hew_duration_nanos",
        BuiltinClass::ClassB,
        I64,
        BuiltinTy::I64,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_duration_nanos",
        },
    ),
    direct(
        "hew_duration_micros",
        BuiltinClass::ClassB,
        I64,
        BuiltinTy::I64,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_duration_micros",
        },
    ),
    direct(
        "hew_duration_millis",
        BuiltinClass::ClassB,
        I64,
        BuiltinTy::I64,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_duration_millis",
        },
    ),
    direct(
        "hew_duration_secs",
        BuiltinClass::ClassB,
        I64,
        BuiltinTy::I64,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_duration_secs",
        },
    ),
    direct(
        "hew_duration_mins",
        BuiltinClass::ClassB,
        I64,
        BuiltinTy::I64,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_duration_mins",
        },
    ),
    direct(
        "hew_duration_hours",
        BuiltinClass::ClassB,
        I64,
        BuiltinTy::I64,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_duration_hours",
        },
    ),
    direct(
        "hew_duration_abs",
        BuiltinClass::ClassB,
        I64,
        BuiltinTy::I64,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_duration_abs",
        },
    ),
    direct(
        "hew_duration_is_zero",
        BuiltinClass::ClassB,
        I64,
        BuiltinTy::Bool,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_duration_is_zero",
        },
    ),
    // Runtime targets for the `impl instant` methods declared in
    // `std/builtins.hew`. `instant` is i64-backed (a nanosecond timestamp),
    // mirroring the `hew_duration_*` rows above. `elapsed` and `duration_since`
    // are receiver-method rewrites whose `c_symbol` HIR resolves through the
    // seeded `fn_registry`. `hew_instant_now` carries no receiver: the static
    // `instant::now()` callee resolves by name through the typed registry seed
    // (`builtin_family = InstantNow`), but the symbol still needs a catalog row
    // here so codegen declares the extern with the real `() -> i64` C ABI and
    // resolves it to an `FnSymbol::Real` at the `Terminator::Call` boundary.
    direct(
        "hew_instant_now",
        BuiltinClass::ClassB,
        EMPTY,
        BuiltinTy::I64,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_instant_now",
        },
    ),
    direct(
        "hew_instant_elapsed",
        BuiltinClass::ClassB,
        I64,
        BuiltinTy::I64,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_instant_elapsed",
        },
    ),
    direct(
        "hew_instant_duration_since",
        BuiltinClass::ClassB,
        I64_I64,
        BuiltinTy::I64,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_instant_duration_since",
        },
    ),
    // Class A: string predicate overloads (ABI-safe: bool return, no i32/i64 conflict).
    overload(
        "starts_with_str",
        "starts_with",
        STRING_STRING,
        BuiltinTy::Bool,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_string_starts_with",
        },
    ),
    overload(
        "ends_with_str",
        "ends_with",
        STRING_STRING,
        BuiltinTy::Bool,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_string_ends_with",
        },
    ),
    overload(
        "contains_str",
        "contains",
        STRING_STRING,
        BuiltinTy::Bool,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_string_contains",
        },
    ),
    overload(
        "is_empty_str",
        "is_empty",
        STRING,
        BuiltinTy::Bool,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_string_is_empty",
        },
    ),
    overload(
        "is_digit_str",
        "is_digit",
        STRING,
        BuiltinTy::Bool,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_string_is_digit",
        },
    ),
    overload(
        "is_alpha_str",
        "is_alpha",
        STRING,
        BuiltinTy::Bool,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_string_is_alpha",
        },
    ),
    overload(
        "is_alphanumeric_str",
        "is_alphanumeric",
        STRING,
        BuiltinTy::Bool,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_string_is_alphanumeric",
        },
    ),
    // Class A: string transform overloads (return String via the C-string-clone path).
    overload(
        "trim_str",
        "trim",
        STRING,
        BuiltinTy::String,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_string_trim",
        },
    ),
    overload(
        "to_lower_str",
        "to_lower",
        STRING,
        BuiltinTy::String,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_string_to_lowercase",
        },
    ),
    overload(
        "to_upper_str",
        "to_upper",
        STRING,
        BuiltinTy::String,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_string_to_uppercase",
        },
    ),
    overload(
        "to_bytes_str",
        "to_bytes",
        STRING,
        BuiltinTy::Bytes,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_string_to_bytes",
        },
    ),
    overload(
        "clone_str",
        "clone",
        STRING,
        BuiltinTy::String,
        BuiltinLinkage::StringCloneShim {
            symbol: "hew_string_clone",
        },
    ),
    overload(
        "replace_str",
        "replace",
        STRING_STRING_STRING,
        BuiltinTy::String,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_string_replace",
        },
    ),
    overload(
        "split_str",
        "split",
        STRING_STRING,
        BuiltinTy::VecAny,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_string_split",
        },
    ),
    overload(
        "lines_str",
        "lines",
        STRING,
        BuiltinTy::VecAny,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_string_lines",
        },
    ),
    // `string.find(needle) -> Option<i64>` — sentinel->Option (D46). The row
    // registers the symbol; checker authority (the `impl string` source sig)
    // drives the `Option<i64>` result. Codegen intercepts the callee, calls
    // the unchanged runtime `hew_string_find` (i32, `-1` miss), and wraps the
    // sentinel as `None` / a hit as `Some(index)`. (`I64` below is the element
    // class, not the wrapped return — there is no `BuiltinTy::Option`.)
    direct(
        "hew_string_find",
        BuiltinClass::ClassA,
        STRING_STRING,
        BuiltinTy::I64,
        BuiltinLinkage::CalleeNameDispatchOnly,
    ),
    overload(
        "slice_str",
        "slice",
        STRING_I64_I64,
        BuiltinTy::String,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_string_slice",
        },
    ),
    overload(
        "repeat_str",
        "repeat",
        STRING_I64,
        BuiltinTy::String,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_string_repeat",
        },
    ),
    // `string.char_at(i) -> Option<char>` — sentinel->Option (D46), the
    // byte-indexed sibling of the codepoint-indexed `string.get`. Codegen
    // intercepts the callee, calls the unchanged runtime `hew_string_char_at`
    // (i32 byte, `-1` OOB), and wraps the sentinel as `None`.
    direct(
        "hew_string_char_at",
        BuiltinClass::ClassA,
        STRING_I64,
        BuiltinTy::Char,
        BuiltinLinkage::CalleeNameDispatchOnly,
    ),
    overload(
        "chars_str",
        "chars",
        STRING,
        BuiltinTy::VecAny,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_string_chars",
        },
    ),
    // UTF-8 codepoint helpers — bound by `std::string`'s `impl string` block
    // and used by `std::text::unicode` for rune_count / runes / codepoint_at.
    overload(
        "char_count_utf8_str",
        "char_count_utf8",
        STRING,
        BuiltinTy::I64,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_string_char_count",
        },
    ),
    // `string.codepoint_at_utf8(i) -> Option<i64>` — sentinel->Option (D46).
    // Codegen intercepts the callee, calls the unchanged runtime
    // `hew_string_char_at_utf8` (i32 codepoint, `-1` OOB/invalid), and wraps
    // the sentinel as `None`.
    direct(
        "hew_string_char_at_utf8",
        BuiltinClass::ClassA,
        STRING_I64,
        BuiltinTy::I64,
        BuiltinLinkage::CalleeNameDispatchOnly,
    ),
    // Class A: declarative bytes receiver bridge. Keep every symbol named by
    // `std/io.hew`'s `impl bytes` extern declarations in the HIR catalog so
    // method-call rewrites resolve at the HIR boundary. Every collection-like
    // bytes method names a dedicated `hew_bytes_*` runtime entry that operates
    // on the `BytesTriple` ABI directly.
    direct(
        "hew_bytes_push",
        BuiltinClass::ClassA,
        BYTES_U8,
        BuiltinTy::Unit,
        BuiltinLinkage::CalleeNameDispatchOnly,
    ),
    // `bytes.pop() -> u8` — removes and returns the last byte (CoW-aware).
    // `CalleeNameDispatchOnly`: checker authority drives the `u8` result; the
    // MIR producer arm emits the dedicated `hew_bytes_pop` runtime call.
    direct(
        "hew_bytes_pop",
        BuiltinClass::ClassA,
        BYTES,
        BuiltinTy::U8,
        BuiltinLinkage::CalleeNameDispatchOnly,
    ),
    // `bytes.set(index, byte)` — overwrites the byte at `index` (CoW-aware).
    direct(
        "hew_bytes_set",
        BuiltinClass::ClassA,
        BYTES_I64_U8,
        BuiltinTy::Unit,
        BuiltinLinkage::CalleeNameDispatchOnly,
    ),
    // `bytes.is_empty() -> bool`.
    direct(
        "hew_bytes_is_empty",
        BuiltinClass::ClassA,
        BYTES,
        BuiltinTy::Bool,
        BuiltinLinkage::CalleeNameDispatchOnly,
    ),
    // `bytes.contains(byte) -> bool` — linear scan.
    direct(
        "hew_bytes_contains",
        BuiltinClass::ClassA,
        BYTES_U8,
        BuiltinTy::Bool,
        BuiltinLinkage::CalleeNameDispatchOnly,
    ),
    // `bytes.clear()` — releases the receiver's buffer ref and resets to empty.
    direct(
        "hew_bytes_clear",
        BuiltinClass::ClassA,
        BYTES,
        BuiltinTy::Unit,
        BuiltinLinkage::CalleeNameDispatchOnly,
    ),
    // `bytes.append(other)` — extends the receiver with a copy of `other`.
    direct(
        "hew_bytes_append",
        BuiltinClass::ClassA,
        BYTES_BYTES,
        BuiltinTy::Unit,
        BuiltinLinkage::CalleeNameDispatchOnly,
    ),
    // `bytes.get(index) -> Option<u8>` — the non-trapping `Index::get` accessor,
    // de-aliased from the trapping `b[i]` sugar (`hew_bytes_index`). This row
    // exists only to REGISTER the `hew_bytes_get` symbol name so the HIR
    // method-call rewrite resolves at the import boundary; `CalleeNameDispatchOnly`
    // means the params/return here are NOT consulted by typecheck — checker
    // authority drives the `Option<u8>` result type (read from `expr_types` at
    // the `RewriteToFunction` lowering site). `hew_bytes_get` carries no runtime
    // export: the MIR producer arm emits a `Terminator::Call` and codegen
    // intercepts the callee, bounds-checks the `BytesTriple`, and materialises
    // `Some(byte)` / `None`. (`U8` below is the element class, not the wrapped
    // return — there is no `BuiltinTy::Option`.)
    direct(
        "hew_bytes_get",
        BuiltinClass::ClassA,
        BYTES_I64,
        BuiltinTy::U8,
        BuiltinLinkage::CalleeNameDispatchOnly,
    ),
    // `string.get(index) -> Option<char>` — the non-trapping `Index::get`
    // accessor, de-aliased from the trapping `s[i]` sugar (`hew_string_index`).
    // This row exists only to REGISTER the `hew_string_get` symbol name so the
    // HIR method-call rewrite resolves at the import boundary;
    // `CalleeNameDispatchOnly` means the params/return here are NOT consulted by
    // typecheck — checker authority drives the `Option<char>` result type (read
    // from `expr_types` at the `RewriteToFunction` lowering site).
    // `hew_string_get` carries no runtime export: the MIR producer arm emits a
    // `Terminator::Call` and codegen intercepts the callee, bounds-checks the
    // index against `hew_string_char_count`, and materialises `Some(char)` /
    // `None`. (`Char` below is the element class, not the wrapped return — there
    // is no `BuiltinTy::Option`.)
    direct(
        "hew_string_get",
        BuiltinClass::ClassA,
        STRING_I64,
        BuiltinTy::Char,
        BuiltinLinkage::CalleeNameDispatchOnly,
    ),
    direct(
        "hew_vec_push_bool",
        BuiltinClass::ClassA,
        VEC_ANY_BOOL,
        BuiltinTy::Unit,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_vec_push_bool",
        },
    ),
    direct(
        "hew_vec_push_i8",
        BuiltinClass::ClassA,
        VEC_ANY_I8,
        BuiltinTy::Unit,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_vec_push_i8",
        },
    ),
    direct(
        "hew_vec_push_u8",
        BuiltinClass::ClassA,
        VEC_ANY_U8,
        BuiltinTy::Unit,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_vec_push_u8",
        },
    ),
    direct(
        "hew_vec_push_i16",
        BuiltinClass::ClassA,
        VEC_ANY_I16,
        BuiltinTy::Unit,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_vec_push_i16",
        },
    ),
    direct(
        "hew_vec_push_u16",
        BuiltinClass::ClassA,
        VEC_ANY_U16,
        BuiltinTy::Unit,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_vec_push_u16",
        },
    ),
    direct(
        "hew_vec_push_i32",
        BuiltinClass::ClassA,
        VEC_ANY_I32,
        BuiltinTy::Unit,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_vec_push_i32",
        },
    ),
    direct(
        "hew_vec_push_i64",
        BuiltinClass::ClassA,
        VEC_ANY_I64,
        BuiltinTy::Unit,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_vec_push_i64",
        },
    ),
    direct(
        "hew_vec_push_f64",
        BuiltinClass::ClassA,
        VEC_ANY_F64,
        BuiltinTy::Unit,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_vec_push_f64",
        },
    ),
    direct(
        "hew_vec_push_f32",
        BuiltinClass::ClassA,
        VEC_ANY_F32,
        BuiltinTy::Unit,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_vec_push_f32",
        },
    ),
    direct(
        "hew_vec_push_str",
        BuiltinClass::ClassA,
        VEC_ANY_STRING,
        BuiltinTy::Unit,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_vec_push_str",
        },
    ),
    // Pointer-shaped element family (`Vec<LocalPid<T>>`): the local actor-handle
    // builtin lowers to a single pointer-sized word (`*mut HewActor`) and the
    // checker classifies it via
    // `BuiltinType::lowers_as_pointer_vec_element` → `"ptr"`. The runtime ABI
    // (`hew_vec_new_ptr` + `hew_vec_{push,get,set,pop}_ptr`) already exists;
    // these rows register the symbols with HIR's `fn_registry` and codegen's
    // `fn_symbols` so the constructor (`hew_vec_new_ptr`) and the element ops
    // agree on the pointer-shaped path instead of tripping the runtime
    // layout-aware abort.
    direct(
        "hew_vec_push_ptr",
        BuiltinClass::ClassA,
        VEC_ANY_PTR,
        BuiltinTy::Unit,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_vec_push_ptr",
        },
    ),
    direct(
        "hew_vec_pop_bool",
        BuiltinClass::ClassA,
        VEC_ANY,
        BuiltinTy::Bool,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_vec_pop_bool",
        },
    ),
    direct(
        "hew_vec_pop_i8",
        BuiltinClass::ClassA,
        VEC_ANY,
        BuiltinTy::I8,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_vec_pop_i8",
        },
    ),
    direct(
        "hew_vec_pop_u8",
        BuiltinClass::ClassA,
        VEC_ANY,
        BuiltinTy::U8,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_vec_pop_u8",
        },
    ),
    direct(
        "hew_vec_pop_i16",
        BuiltinClass::ClassA,
        VEC_ANY,
        BuiltinTy::I16,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_vec_pop_i16",
        },
    ),
    direct(
        "hew_vec_pop_u16",
        BuiltinClass::ClassA,
        VEC_ANY,
        BuiltinTy::U16,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_vec_pop_u16",
        },
    ),
    direct(
        "hew_vec_pop_i32",
        BuiltinClass::ClassA,
        VEC_ANY,
        BuiltinTy::I32,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_vec_pop_i32",
        },
    ),
    direct(
        "hew_vec_pop_i64",
        BuiltinClass::ClassA,
        VEC_ANY,
        BuiltinTy::I64,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_vec_pop_i64",
        },
    ),
    direct(
        "hew_vec_pop_f64",
        BuiltinClass::ClassA,
        VEC_ANY,
        BuiltinTy::F64,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_vec_pop_f64",
        },
    ),
    direct(
        "hew_vec_pop_f32",
        BuiltinClass::ClassA,
        VEC_ANY,
        BuiltinTy::F32,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_vec_pop_f32",
        },
    ),
    direct(
        "hew_vec_pop_str",
        BuiltinClass::ClassA,
        VEC_ANY,
        BuiltinTy::String,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_vec_pop_str",
        },
    ),
    direct(
        "hew_vec_pop_ptr",
        BuiltinClass::ClassA,
        VEC_ANY,
        BuiltinTy::Pointer,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_vec_pop_ptr",
        },
    ),
    direct(
        "hew_vec_get_bool",
        BuiltinClass::ClassA,
        VEC_ANY_I64,
        BuiltinTy::Bool,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_vec_get_bool",
        },
    ),
    direct(
        "hew_vec_get_i8",
        BuiltinClass::ClassA,
        VEC_ANY_I64,
        BuiltinTy::I8,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_vec_get_i8",
        },
    ),
    direct(
        "hew_vec_get_u8",
        BuiltinClass::ClassA,
        VEC_ANY_I64,
        BuiltinTy::U8,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_vec_get_u8",
        },
    ),
    direct(
        "hew_vec_get_i16",
        BuiltinClass::ClassA,
        VEC_ANY_I64,
        BuiltinTy::I16,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_vec_get_i16",
        },
    ),
    direct(
        "hew_vec_get_u16",
        BuiltinClass::ClassA,
        VEC_ANY_I64,
        BuiltinTy::U16,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_vec_get_u16",
        },
    ),
    direct(
        "hew_vec_get_i32",
        BuiltinClass::ClassA,
        VEC_ANY_I64,
        BuiltinTy::I32,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_vec_get_i32",
        },
    ),
    direct(
        "hew_vec_get_i64",
        BuiltinClass::ClassA,
        VEC_ANY_I64,
        BuiltinTy::I64,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_vec_get_i64",
        },
    ),
    direct(
        "hew_vec_get_f64",
        BuiltinClass::ClassA,
        VEC_ANY_I64,
        BuiltinTy::F64,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_vec_get_f64",
        },
    ),
    direct(
        "hew_vec_get_f32",
        BuiltinClass::ClassA,
        VEC_ANY_I64,
        BuiltinTy::F32,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_vec_get_f32",
        },
    ),
    direct(
        "hew_vec_get_str",
        BuiltinClass::ClassA,
        VEC_ANY_I64,
        BuiltinTy::String,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_vec_get_str",
        },
    ),
    direct(
        "hew_vec_get_ptr",
        BuiltinClass::ClassA,
        VEC_ANY_I64,
        BuiltinTy::Pointer,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_vec_get_ptr",
        },
    ),
    direct(
        "hew_vec_set_bool",
        BuiltinClass::ClassA,
        VEC_ANY_I64_BOOL,
        BuiltinTy::Unit,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_vec_set_bool",
        },
    ),
    direct(
        "hew_vec_set_i8",
        BuiltinClass::ClassA,
        VEC_ANY_I64_I8,
        BuiltinTy::Unit,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_vec_set_i8",
        },
    ),
    direct(
        "hew_vec_set_u8",
        BuiltinClass::ClassA,
        VEC_ANY_I64_U8,
        BuiltinTy::Unit,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_vec_set_u8",
        },
    ),
    direct(
        "hew_vec_set_i16",
        BuiltinClass::ClassA,
        VEC_ANY_I64_I16,
        BuiltinTy::Unit,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_vec_set_i16",
        },
    ),
    direct(
        "hew_vec_set_u16",
        BuiltinClass::ClassA,
        VEC_ANY_I64_U16,
        BuiltinTy::Unit,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_vec_set_u16",
        },
    ),
    direct(
        "hew_vec_set_i32",
        BuiltinClass::ClassA,
        VEC_ANY_I64_I32,
        BuiltinTy::Unit,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_vec_set_i32",
        },
    ),
    direct(
        "hew_vec_set_i64",
        BuiltinClass::ClassA,
        VEC_ANY_I64_I64,
        BuiltinTy::Unit,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_vec_set_i64",
        },
    ),
    direct(
        "hew_vec_set_f64",
        BuiltinClass::ClassA,
        VEC_ANY_I64_F64,
        BuiltinTy::Unit,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_vec_set_f64",
        },
    ),
    direct(
        "hew_vec_set_f32",
        BuiltinClass::ClassA,
        VEC_ANY_I64_F32,
        BuiltinTy::Unit,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_vec_set_f32",
        },
    ),
    direct(
        "hew_vec_set_str",
        BuiltinClass::ClassA,
        VEC_ANY_I64_STRING,
        BuiltinTy::Unit,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_vec_set_str",
        },
    ),
    direct(
        "hew_vec_set_ptr",
        BuiltinClass::ClassA,
        VEC_ANY_I64_PTR,
        BuiltinTy::Unit,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_vec_set_ptr",
        },
    ),
    // Layout-backed Vec<T> methods are checker-only rewrite targets for
    // Copy record/tuple elements.  Codegen intercepts these callee names and
    // synthesises the hidden `HewTypeLayout*` / data-pointer operands before
    // calling the real runtime ABI.  They are intentionally not declared as
    // `RuntimeFfiShim` rows because their source-level arity differs from the
    // C ABI.
    direct(
        "hew_vec_push_layout",
        BuiltinClass::ClassA,
        VEC_ANY,
        BuiltinTy::Unit,
        BuiltinLinkage::CalleeNameDispatchOnly,
    ),
    direct(
        "hew_vec_get_layout",
        BuiltinClass::ClassA,
        VEC_ANY,
        BuiltinTy::VecAny,
        BuiltinLinkage::CalleeNameDispatchOnly,
    ),
    direct(
        "hew_vec_set_layout",
        BuiltinClass::ClassA,
        VEC_ANY,
        BuiltinTy::Unit,
        BuiltinLinkage::CalleeNameDispatchOnly,
    ),
    direct(
        "hew_vec_pop_layout",
        BuiltinClass::ClassA,
        VEC_ANY,
        BuiltinTy::VecAny,
        BuiltinLinkage::CalleeNameDispatchOnly,
    ),
    // W3.032 Slice 3: `Vec<Record/Tuple>::contains` for equality-eligible Copy
    // record/tuple elements routes through `hew_vec_contains_thunk`.  The
    // hidden third operand (a codegen-emitted per-type `__hew_eq_thunk_*`
    // function pointer) is synthesized by `lower_layout_vec_direct_call`;
    // checker authority is the sole eligibility gate (see
    // `hew-types/src/eq_eligibility.rs` and `check_vec_method` `contains`).
    direct(
        "hew_vec_contains_thunk",
        BuiltinClass::ClassA,
        VEC_ANY,
        BuiltinTy::Bool,
        BuiltinLinkage::CalleeNameDispatchOnly,
    ),
    // `Vec<T>::remove(index) -> T` for BitCopy layout-backed elements. Routes
    // through `hew_vec_remove_at_layout`, which moves the removed element into
    // a codegen-supplied out slot (mirroring `pop_layout`) and shifts the tail;
    // the hidden `out` and `HewTypeLayout*` operands are synthesized by codegen
    // from the Vec element type. Arity: receiver Vec + explicit index = 2
    // source-level parameters. The return is the removed element, materialised
    // by the codegen dispatch (`CalleeNameDispatchOnly`) exactly like
    // `pop_layout`.
    direct(
        "hew_vec_remove_at_layout",
        BuiltinClass::ClassA,
        VEC_ANY_I64,
        BuiltinTy::VecAny,
        BuiltinLinkage::CalleeNameDispatchOnly,
    ),
    // W3.003: `Vec<T>::clone()` for BitCopy layout-backed elements.
    // Routes through `hew_vec_clone_layout`; the hidden `HewTypeLayout*`
    // operand is synthesized by codegen from the Vec element type.
    // Arity: receiver Vec only — returns a freshly allocated `*mut HewVec`.
    direct(
        "hew_vec_clone_layout",
        BuiltinClass::ClassA,
        VEC_ANY,
        BuiltinTy::VecAny,
        BuiltinLinkage::CalleeNameDispatchOnly,
    ),
    // ── Layout-backed HashMap<K, V> entries ──────────────────────────────────
    //
    // W3.041 sub-lane A: 7 layout HashMap rows for Copy named-record keys with
    // any admitted value type (scalar or layout).  All use
    // `CalleeNameDispatchOnly` because the runtime ABI takes hidden
    // `HewMapKeyLayout*` / `HewMapValueLayout*` operands synthesised by codegen
    // from the checker-authoritative `HashMapLoweringFact`.  The `params` here
    // are placeholder shape metadata only — typecheck routes through
    // `check_hashmap_method` which performs full K/V validation; HIR/MIR/codegen
    // consume `HashMapLoweringFact` for the real ABI.
    //
    // `_free_layout` is the drop-path entry: it appears in this catalog so the
    // symbol resolves through the same path as the other six operations, even
    // though user code never names it directly.  Drop machinery wires its
    // selection.
    direct(
        "hew_hashmap_new_with_layout",
        BuiltinClass::ClassA,
        EMPTY,
        BuiltinTy::HashMapAny,
        BuiltinLinkage::CalleeNameDispatchOnly,
    ),
    direct(
        "hew_hashmap_insert_layout",
        BuiltinClass::ClassA,
        HASHMAP_ANY,
        BuiltinTy::Bool,
        BuiltinLinkage::CalleeNameDispatchOnly,
    ),
    direct(
        "hew_hashmap_get_layout",
        BuiltinClass::ClassA,
        HASHMAP_ANY,
        BuiltinTy::Unit,
        BuiltinLinkage::CalleeNameDispatchOnly,
    ),
    direct(
        "hew_hashmap_contains_key_layout",
        BuiltinClass::ClassA,
        HASHMAP_ANY,
        BuiltinTy::Bool,
        BuiltinLinkage::CalleeNameDispatchOnly,
    ),
    direct(
        "hew_hashmap_remove_layout",
        BuiltinClass::ClassA,
        HASHMAP_ANY,
        BuiltinTy::Bool,
        BuiltinLinkage::CalleeNameDispatchOnly,
    ),
    // `HashMap::remove(k) -> Option<V>` (A233): the move-out remove. Mirrors
    // `get_layout` — the checker projects `Option<V>`; codegen builds the
    // Some/None from the runtime's bool + out-param (drop-K, move-V).
    direct(
        "hew_hashmap_remove_take_layout",
        BuiltinClass::ClassA,
        HASHMAP_ANY,
        BuiltinTy::Unit,
        BuiltinLinkage::CalleeNameDispatchOnly,
    ),
    direct(
        "hew_hashmap_len_layout",
        BuiltinClass::ClassA,
        HASHMAP_ANY,
        BuiltinTy::I64,
        BuiltinLinkage::CalleeNameDispatchOnly,
    ),
    direct(
        "hew_hashmap_free_layout",
        BuiltinClass::ClassA,
        HASHMAP_ANY,
        BuiltinTy::Unit,
        BuiltinLinkage::CalleeNameDispatchOnly,
    ),
    // ── Layout-backed HashSet<T> entries ─────────────────────────────────────
    //
    // 6 layout HashSet rows mirroring the HashMap shape.  HashSet<T> is
    // layout-equivalent to HashMap<T, ()> per the runtime substrate (C-1c);
    // the elem layout takes the same `HewMapKeyLayout*` descriptor that the
    // HashMap key path consumes.
    direct(
        "hew_hashset_new_with_layout",
        BuiltinClass::ClassA,
        EMPTY,
        BuiltinTy::HashSetAny,
        BuiltinLinkage::CalleeNameDispatchOnly,
    ),
    direct(
        "hew_hashset_insert_layout",
        BuiltinClass::ClassA,
        HASHSET_ANY,
        BuiltinTy::Bool,
        BuiltinLinkage::CalleeNameDispatchOnly,
    ),
    direct(
        "hew_hashset_contains_layout",
        BuiltinClass::ClassA,
        HASHSET_ANY,
        BuiltinTy::Bool,
        BuiltinLinkage::CalleeNameDispatchOnly,
    ),
    direct(
        "hew_hashset_remove_layout",
        BuiltinClass::ClassA,
        HASHSET_ANY,
        BuiltinTy::Bool,
        BuiltinLinkage::CalleeNameDispatchOnly,
    ),
    direct(
        "hew_hashset_len_layout",
        BuiltinClass::ClassA,
        HASHSET_ANY,
        BuiltinTy::I64,
        BuiltinLinkage::CalleeNameDispatchOnly,
    ),
    direct(
        "hew_hashset_is_empty_layout",
        BuiltinClass::ClassA,
        HASHSET_ANY,
        BuiltinTy::Bool,
        BuiltinLinkage::CalleeNameDispatchOnly,
    ),
    direct(
        "hew_hashset_free_layout",
        BuiltinClass::ClassA,
        HASHSET_ANY,
        BuiltinTy::Unit,
        BuiltinLinkage::CalleeNameDispatchOnly,
    ),
    // ── Layout descriptor symbols (W4.001 Stage C0b) ─────────────────────────
    //
    // `#[no_mangle] pub static` instances of `HewMapKeyLayout` /
    // `HewMapValueLayout` exported by `hew-runtime/src/layout_intrinsics.rs`
    // and re-declared by `hew-cabi/src/map.rs`. Catalog rows here are
    // checker-visible declarations only — Stage C is the first production
    // reader (plan §4 Stage C0b boundary).
    //
    // Non-callable: `LayoutDescriptorSymbol` linkage skips both LLVM
    // pre-declaration and MIR `module_fn_names` insertion. The `name` slot
    // mirrors the runtime symbol so the symbol-linkage gate
    // (`resolved_call_kernel_symbols`) and the coverage gate
    // (`stdlib_catalog_layout_descriptor_coverage`) can enumerate the
    // ABI surface from one place.
    //
    // Named-record descriptors are *not* listed: per plan §4 Stage C0b they
    // are materialised on demand by the existing Named-record Layout
    // machinery (Stage C-1c), not shipped as fixed statics.
    direct(
        "hew_layout_key_i32",
        BuiltinClass::ClassA,
        EMPTY,
        BuiltinTy::Unit,
        BuiltinLinkage::LayoutDescriptorSymbol {
            symbol: "hew_layout_key_i32",
            role: LayoutDescriptorRole::Key,
        },
    ),
    direct(
        "hew_layout_key_i64",
        BuiltinClass::ClassA,
        EMPTY,
        BuiltinTy::Unit,
        BuiltinLinkage::LayoutDescriptorSymbol {
            symbol: "hew_layout_key_i64",
            role: LayoutDescriptorRole::Key,
        },
    ),
    direct(
        "hew_layout_key_u32",
        BuiltinClass::ClassA,
        EMPTY,
        BuiltinTy::Unit,
        BuiltinLinkage::LayoutDescriptorSymbol {
            symbol: "hew_layout_key_u32",
            role: LayoutDescriptorRole::Key,
        },
    ),
    direct(
        "hew_layout_key_u64",
        BuiltinClass::ClassA,
        EMPTY,
        BuiltinTy::Unit,
        BuiltinLinkage::LayoutDescriptorSymbol {
            symbol: "hew_layout_key_u64",
            role: LayoutDescriptorRole::Key,
        },
    ),
    direct(
        "hew_layout_key_f32",
        BuiltinClass::ClassA,
        EMPTY,
        BuiltinTy::Unit,
        BuiltinLinkage::LayoutDescriptorSymbol {
            symbol: "hew_layout_key_f32",
            role: LayoutDescriptorRole::Key,
        },
    ),
    direct(
        "hew_layout_key_f64",
        BuiltinClass::ClassA,
        EMPTY,
        BuiltinTy::Unit,
        BuiltinLinkage::LayoutDescriptorSymbol {
            symbol: "hew_layout_key_f64",
            role: LayoutDescriptorRole::Key,
        },
    ),
    direct(
        "hew_layout_key_bool",
        BuiltinClass::ClassA,
        EMPTY,
        BuiltinTy::Unit,
        BuiltinLinkage::LayoutDescriptorSymbol {
            symbol: "hew_layout_key_bool",
            role: LayoutDescriptorRole::Key,
        },
    ),
    direct(
        "hew_layout_key_char",
        BuiltinClass::ClassA,
        EMPTY,
        BuiltinTy::Unit,
        BuiltinLinkage::LayoutDescriptorSymbol {
            symbol: "hew_layout_key_char",
            role: LayoutDescriptorRole::Key,
        },
    ),
    direct(
        "hew_layout_key_string",
        BuiltinClass::ClassA,
        EMPTY,
        BuiltinTy::Unit,
        BuiltinLinkage::LayoutDescriptorSymbol {
            symbol: "hew_layout_key_string",
            role: LayoutDescriptorRole::Key,
        },
    ),
    direct(
        "hew_layout_key_bytes",
        BuiltinClass::ClassA,
        EMPTY,
        BuiltinTy::Unit,
        BuiltinLinkage::LayoutDescriptorSymbol {
            symbol: "hew_layout_key_bytes",
            role: LayoutDescriptorRole::Key,
        },
    ),
    direct(
        "hew_layout_val_i32",
        BuiltinClass::ClassA,
        EMPTY,
        BuiltinTy::Unit,
        BuiltinLinkage::LayoutDescriptorSymbol {
            symbol: "hew_layout_val_i32",
            role: LayoutDescriptorRole::Value,
        },
    ),
    direct(
        "hew_layout_val_i64",
        BuiltinClass::ClassA,
        EMPTY,
        BuiltinTy::Unit,
        BuiltinLinkage::LayoutDescriptorSymbol {
            symbol: "hew_layout_val_i64",
            role: LayoutDescriptorRole::Value,
        },
    ),
    direct(
        "hew_layout_val_u32",
        BuiltinClass::ClassA,
        EMPTY,
        BuiltinTy::Unit,
        BuiltinLinkage::LayoutDescriptorSymbol {
            symbol: "hew_layout_val_u32",
            role: LayoutDescriptorRole::Value,
        },
    ),
    direct(
        "hew_layout_val_u64",
        BuiltinClass::ClassA,
        EMPTY,
        BuiltinTy::Unit,
        BuiltinLinkage::LayoutDescriptorSymbol {
            symbol: "hew_layout_val_u64",
            role: LayoutDescriptorRole::Value,
        },
    ),
    direct(
        "hew_layout_val_f32",
        BuiltinClass::ClassA,
        EMPTY,
        BuiltinTy::Unit,
        BuiltinLinkage::LayoutDescriptorSymbol {
            symbol: "hew_layout_val_f32",
            role: LayoutDescriptorRole::Value,
        },
    ),
    direct(
        "hew_layout_val_f64",
        BuiltinClass::ClassA,
        EMPTY,
        BuiltinTy::Unit,
        BuiltinLinkage::LayoutDescriptorSymbol {
            symbol: "hew_layout_val_f64",
            role: LayoutDescriptorRole::Value,
        },
    ),
    direct(
        "hew_layout_val_bool",
        BuiltinClass::ClassA,
        EMPTY,
        BuiltinTy::Unit,
        BuiltinLinkage::LayoutDescriptorSymbol {
            symbol: "hew_layout_val_bool",
            role: LayoutDescriptorRole::Value,
        },
    ),
    direct(
        "hew_layout_val_char",
        BuiltinClass::ClassA,
        EMPTY,
        BuiltinTy::Unit,
        BuiltinLinkage::LayoutDescriptorSymbol {
            symbol: "hew_layout_val_char",
            role: LayoutDescriptorRole::Value,
        },
    ),
    direct(
        "hew_layout_val_string",
        BuiltinClass::ClassA,
        EMPTY,
        BuiltinTy::Unit,
        BuiltinLinkage::LayoutDescriptorSymbol {
            symbol: "hew_layout_val_string",
            role: LayoutDescriptorRole::Value,
        },
    ),
    direct(
        "hew_layout_val_bytes",
        BuiltinClass::ClassA,
        EMPTY,
        BuiltinTy::Unit,
        BuiltinLinkage::LayoutDescriptorSymbol {
            symbol: "hew_layout_val_bytes",
            role: LayoutDescriptorRole::Value,
        },
    ),
    direct(
        "hew_layout_val_unit",
        BuiltinClass::ClassA,
        EMPTY,
        BuiltinTy::Unit,
        BuiltinLinkage::LayoutDescriptorSymbol {
            symbol: "hew_layout_val_unit",
            role: LayoutDescriptorRole::Value,
        },
    ),
    direct(
        "hew_vec_is_empty",
        BuiltinClass::ClassA,
        VEC_ANY,
        BuiltinTy::Bool,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_vec_is_empty",
        },
    ),
    direct(
        "hew_vec_clear",
        BuiltinClass::ClassA,
        VEC_ANY,
        BuiltinTy::Unit,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_vec_clear",
        },
    ),
    direct(
        "hew_vec_contains_i32",
        BuiltinClass::ClassA,
        VEC_ANY_I32,
        BuiltinTy::Bool,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_vec_contains_i32",
        },
    ),
    direct(
        "hew_vec_contains_i64",
        BuiltinClass::ClassA,
        VEC_ANY_I64,
        BuiltinTy::Bool,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_vec_contains_i64",
        },
    ),
    direct(
        "hew_vec_contains_f64",
        BuiltinClass::ClassA,
        VEC_ANY_F64,
        BuiltinTy::Bool,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_vec_contains_f64",
        },
    ),
    direct(
        "hew_vec_contains_str",
        BuiltinClass::ClassA,
        VEC_ANY_STRING,
        BuiltinTy::Bool,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_vec_contains_str",
        },
    ),
    // W4.039: bytes -> string canonicalisation. Single `hew_bytes_to_string`
    // runtime export consumes the BytesTriple value directly; the catalog
    // declares the LLVM extern with a single `bytes` parameter, which codegen
    // materialises as `{ptr, i32, i32}` (ABI-equivalent to a
    // `#[repr(C)] BytesTriple` passed by value).
    direct(
        "hew_bytes_to_string",
        BuiltinClass::ClassA,
        BYTES,
        BuiltinTy::String,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_bytes_to_string",
        },
    ),
    direct(
        "hew_vec_append",
        BuiltinClass::ClassA,
        VEC_ANY_VEC_ANY,
        BuiltinTy::Unit,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_vec_append",
        },
    ),
    // `Vec<T>::remove(index) -> T` — index-based move-out mirroring `pop`, one
    // symbol per element class. Params are receiver Vec + explicit index
    // (`VEC_ANY_I64`); the return is the removed element `T` (transferred to
    // the caller — no clone, no drop). The bool arm rides a named codegen
    // dispatch (i1 marshalling) like `pop_bool`; the scalar/str/ptr arms flow
    // through the generic FFI-shim path typed by their `BuiltinTy` return.
    direct(
        "hew_vec_remove_at_bool",
        BuiltinClass::ClassA,
        VEC_ANY_I64,
        BuiltinTy::Bool,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_vec_remove_at_bool",
        },
    ),
    direct(
        "hew_vec_remove_at_i8",
        BuiltinClass::ClassA,
        VEC_ANY_I64,
        BuiltinTy::I8,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_vec_remove_at_i8",
        },
    ),
    direct(
        "hew_vec_remove_at_u8",
        BuiltinClass::ClassA,
        VEC_ANY_I64,
        BuiltinTy::U8,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_vec_remove_at_u8",
        },
    ),
    direct(
        "hew_vec_remove_at_i16",
        BuiltinClass::ClassA,
        VEC_ANY_I64,
        BuiltinTy::I16,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_vec_remove_at_i16",
        },
    ),
    direct(
        "hew_vec_remove_at_u16",
        BuiltinClass::ClassA,
        VEC_ANY_I64,
        BuiltinTy::U16,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_vec_remove_at_u16",
        },
    ),
    direct(
        "hew_vec_remove_at_i32",
        BuiltinClass::ClassA,
        VEC_ANY_I64,
        BuiltinTy::I32,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_vec_remove_at_i32",
        },
    ),
    direct(
        "hew_vec_remove_at_i64",
        BuiltinClass::ClassA,
        VEC_ANY_I64,
        BuiltinTy::I64,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_vec_remove_at_i64",
        },
    ),
    direct(
        "hew_vec_remove_at_f32",
        BuiltinClass::ClassA,
        VEC_ANY_I64,
        BuiltinTy::F32,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_vec_remove_at_f32",
        },
    ),
    direct(
        "hew_vec_remove_at_f64",
        BuiltinClass::ClassA,
        VEC_ANY_I64,
        BuiltinTy::F64,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_vec_remove_at_f64",
        },
    ),
    direct(
        "hew_vec_remove_at_str",
        BuiltinClass::ClassA,
        VEC_ANY_I64,
        BuiltinTy::String,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_vec_remove_at_str",
        },
    ),
    direct(
        "hew_vec_remove_at_ptr",
        BuiltinClass::ClassA,
        VEC_ANY_I64,
        BuiltinTy::Pointer,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_vec_remove_at_ptr",
        },
    ),
    direct(
        "hew_vec_clone",
        BuiltinClass::ClassA,
        VEC_ANY,
        BuiltinTy::VecAny,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_vec_clone",
        },
    ),
    direct(
        "hew_vec_join_str",
        BuiltinClass::ClassA,
        VEC_ANY_STRING,
        BuiltinTy::String,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_vec_join_str",
        },
    ),
    // Class B: math module intrinsics.
    direct(
        "math.exp",
        BuiltinClass::ClassB,
        F64,
        BuiltinTy::F64,
        BuiltinLinkage::CompilerIntrinsic {
            intrinsic: "math.exp",
        },
    ),
    direct(
        "math.log",
        BuiltinClass::ClassB,
        F64,
        BuiltinTy::F64,
        BuiltinLinkage::CompilerIntrinsic {
            intrinsic: "math.log",
        },
    ),
    direct(
        "math.sqrt",
        BuiltinClass::ClassB,
        F64,
        BuiltinTy::F64,
        BuiltinLinkage::CompilerIntrinsic {
            intrinsic: "math.sqrt",
        },
    ),
    direct(
        "math.sin",
        BuiltinClass::ClassB,
        F64,
        BuiltinTy::F64,
        BuiltinLinkage::CompilerIntrinsic {
            intrinsic: "math.sin",
        },
    ),
    direct(
        "math.cos",
        BuiltinClass::ClassB,
        F64,
        BuiltinTy::F64,
        BuiltinLinkage::CompilerIntrinsic {
            intrinsic: "math.cos",
        },
    ),
    direct(
        "math.floor",
        BuiltinClass::ClassB,
        F64,
        BuiltinTy::F64,
        BuiltinLinkage::CompilerIntrinsic {
            intrinsic: "math.floor",
        },
    ),
    direct(
        "math.ceil",
        BuiltinClass::ClassB,
        F64,
        BuiltinTy::F64,
        BuiltinLinkage::CompilerIntrinsic {
            intrinsic: "math.ceil",
        },
    ),
    direct(
        "math.abs",
        BuiltinClass::ClassB,
        F64,
        BuiltinTy::F64,
        BuiltinLinkage::CompilerIntrinsic {
            intrinsic: "math.abs",
        },
    ),
    direct(
        "math.tanh",
        BuiltinClass::ClassB,
        F64,
        BuiltinTy::F64,
        BuiltinLinkage::CompilerIntrinsic {
            intrinsic: "math.tanh",
        },
    ),
    direct(
        "math.log2",
        BuiltinClass::ClassB,
        F64,
        BuiltinTy::F64,
        BuiltinLinkage::CompilerIntrinsic {
            intrinsic: "math.log2",
        },
    ),
    direct(
        "math.log10",
        BuiltinClass::ClassB,
        F64,
        BuiltinTy::F64,
        BuiltinLinkage::CompilerIntrinsic {
            intrinsic: "math.log10",
        },
    ),
    direct(
        "math.exp2",
        BuiltinClass::ClassB,
        F64,
        BuiltinTy::F64,
        BuiltinLinkage::CompilerIntrinsic {
            intrinsic: "math.exp2",
        },
    ),
    direct(
        "math.pow",
        BuiltinClass::ClassB,
        F64_F64,
        BuiltinTy::F64,
        BuiltinLinkage::CompilerIntrinsic {
            intrinsic: "math.pow",
        },
    ),
    direct(
        "math.max",
        BuiltinClass::ClassB,
        F64_F64,
        BuiltinTy::F64,
        BuiltinLinkage::CompilerIntrinsic {
            intrinsic: "math.max",
        },
    ),
    direct(
        "math.min",
        BuiltinClass::ClassB,
        F64_F64,
        BuiltinTy::F64,
        BuiltinLinkage::CompilerIntrinsic {
            intrinsic: "math.min",
        },
    ),
    direct(
        "math.pi",
        BuiltinClass::ClassB,
        EMPTY,
        BuiltinTy::F64,
        BuiltinLinkage::CompilerIntrinsic {
            intrinsic: "math.pi",
        },
    ),
    direct(
        "math.e",
        BuiltinClass::ClassB,
        EMPTY,
        BuiltinTy::F64,
        BuiltinLinkage::CompilerIntrinsic {
            intrinsic: "math.e",
        },
    ),
    // Class B: random module runtime shims.
    direct(
        "random.seed",
        BuiltinClass::ClassB,
        I64,
        BuiltinTy::Unit,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_random_seed",
        },
    ),
    direct(
        "random.random",
        BuiltinClass::ClassB,
        EMPTY,
        BuiltinTy::F64,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_random_random",
        },
    ),
    direct(
        "random.gauss",
        BuiltinClass::ClassB,
        F64_F64,
        BuiltinTy::F64,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_random_gauss",
        },
    ),
    direct(
        "random.randint",
        BuiltinClass::ClassB,
        I64_I64,
        BuiltinTy::I64,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_random_randint",
        },
    ),
    direct(
        "random.shuffle",
        BuiltinClass::ClassB,
        VEC_ANY,
        BuiltinTy::Unit,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_random_shuffle_i64",
        },
    ),
    direct(
        "random.choices",
        BuiltinClass::ClassB,
        &[BuiltinTy::VecAny, BuiltinTy::F64, BuiltinTy::I64],
        BuiltinTy::I64,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_random_choices_vec",
        },
    ),
    // Class B: Node namespace — distributed-node lifecycle builtins (A7 S1).
    //
    // Each entry maps the Hew `Node::X` call form to its `hew_node_api_X` C
    // extern in `hew-runtime/src/hew_node.rs`.  The C functions return `c_int`
    // (0 = success, -1 = error) but are declared `Unit` here so HIR/MIR treat
    // them as void-returning and the call is emitted without a destination slot.
    // On x86-64 and arm64 the callee's return value sits in rax/x0 and is
    // harmlessly discarded by the caller — this is the standard C idiom for
    // ignoring a return value.  The runtime still surfaces a peer-auth setup
    // failure even though the `-1` is discarded: `Node::load_keys` /
    // `Node::allow_peer` set `hew_last_error`, print a `hew:` stderr diagnostic,
    // and record a sticky failure so a later `Node::start` refuses to bind a
    // listener (fail-closed) rather than silently presenting an ephemeral
    // identity. See `node_peer_auth_setup_failed` in `hew_node.rs`.
    direct(
        "Node::set_transport",
        BuiltinClass::ClassB,
        STRING,
        BuiltinTy::Unit,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_node_api_set_transport",
        },
    ),
    direct(
        "Node::start",
        BuiltinClass::ClassB,
        STRING,
        BuiltinTy::Unit,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_node_api_start",
        },
    ),
    direct(
        "Node::connect",
        BuiltinClass::ClassB,
        STRING,
        BuiltinTy::Unit,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_node_api_connect",
        },
    ),
    direct(
        "Node::shutdown",
        BuiltinClass::ClassB,
        EMPTY,
        BuiltinTy::Unit,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_node_api_shutdown",
        },
    ),
    // `Node::load_keys(path: String)` — load/persist this node's stable mesh
    // TLS identity from a keyfile. Same FFI shim shape as set_transport: one
    // String in, c_int discarded as Unit. Native quic-mesh only.
    direct(
        "Node::load_keys",
        BuiltinClass::ClassB,
        STRING,
        BuiltinTy::Unit,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_node_api_load_keys",
        },
    ),
    // `Node::allow_peer(spki_hex: String)` — pin a peer's certificate SPKI in
    // the fail-closed mesh allowlist. String (lowercase hex) in, c_int as Unit.
    direct(
        "Node::allow_peer",
        BuiltinClass::ClassB,
        STRING,
        BuiltinTy::Unit,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_node_api_allow_peer",
        },
    ),
    // `Node::register<T>(name: String, pid: LocalPid<T>) -> i32`
    //
    // Per R81, `LocalPid<T>` lowers to a bare `u64` PID at the C-ABI
    // boundary.  The catalog param list `[String, U64]` is a placeholder
    // for HIR/MIR name-resolution purposes — codegen handles this linkage
    // variant specially and does not use the generic `declare_catalog_ffi`
    // path (which would construct the wrong LLVM function type).
    direct(
        "Node::register",
        BuiltinClass::ClassB,
        &[BuiltinTy::String, BuiltinTy::U64],
        BuiltinTy::I32,
        BuiltinLinkage::NodeRegisterByPid {
            register_symbol: "hew_node_api_register_by_pid",
            pid_accessor: "hew_actor_pid",
        },
    ),
    // `RemotePid::from_raw(node_id: u64, serial: u64) -> RemotePid<T>`
    //
    // Constructs a `RemotePid<T>` from a raw (node_id, serial) pair.  The
    // `RemotePid<T>` type lowers to a bare `u64` PID — identical encoding to
    // `LocalPid<T>` — so the return type is `U64` at the C-ABI boundary.
    // The runtime validates that `node_id` is non-zero and fits in u16 (the
    // packed encoding constraint).  See `hew-runtime/src/hew_node.rs`.
    direct(
        "RemotePid::from_raw",
        BuiltinClass::ClassB,
        &[BuiltinTy::U64, BuiltinTy::U64],
        BuiltinTy::U64,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_remote_pid_from_raw",
        },
    ),
    // `Node::lookup<T>(name: String) -> Result<RemotePid<T>, LookupError>`
    //
    // The C-ABI shape is `hew_node_api_lookup(name: ptr) -> u64`: a packed
    // RemotePid encoding on success, or `0` on not-found / no-node / null-name.
    // The catalog entry carries the FFI-level types (String → U64); the
    // codegen Terminator::Call branch for "Node::lookup" wraps the raw `u64`
    // into the user-visible `Result<RemotePid<T>, LookupError>` by storing
    // tag and payload directly into the dest enum slot. RuntimeFfiShim is the
    // correct linkage because the call is a single C extern — no new
    // `FnSymbol::*` variant is needed (codegen branches by callee name).
    direct(
        "Node::lookup",
        BuiltinClass::ClassB,
        STRING,
        BuiltinTy::U64,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_node_api_lookup",
        },
    ),
    // `RemotePid<T>::send(pid: RemotePid<T>, msg: T::Msg) -> Result<(), SendError>`
    //
    // Surface in std/builtins.hew. The checker records a
    // MethodCallRewrite::RewriteToFunction with c_symbol
    // "hew_remote_pid_send" at every `pid.send(msg)` call site on a
    // RemotePid<T> receiver (see hew-types::check::methods); the HIR
    // direct-call lowering produces a Terminator::Call("hew_remote_pid_send",
    // [pid, msg]). Codegen branches on callee name in
    // `emit_remote_pid_send_call` to emit the `hew_actor_send_by_id`
    // call sequence and construct the user-visible `Result<(), SendError>`
    // in place (mirrors the Node::lookup precedent).
    //
    // No new FnSymbol::*Pid* / FnSymbol::*RemoteSend* variant is
    // introduced — codegen dispatch is by callee name, matching the S4
    // Node::lookup precedent (R82-era).
    //
    // Linkage is `CalleeNameDispatchOnly` (not `RuntimeFfiShim`): there
    // is no dedicated `hew_remote_pid_send` runtime symbol — the real
    // underlying extern `hew_actor_send_by_id` has a different ABI
    // (`(u64, i32, ptr, usize) -> i32`) and is declared via
    // `intern_runtime_decl` directly inside `emit_remote_pid_send_call`,
    // not through the catalog FFI predeclare path. The catalog entry's
    // params/return shape only needs to satisfy HIR's fn_registry lookup
    // (BindingRef resolves to Item) and MIR's `module_fn_names`
    // membership (so the call lowers to `Terminator::Call`).
    direct(
        "hew_remote_pid_send",
        BuiltinClass::ClassB,
        &[BuiltinTy::U64, BuiltinTy::U64],
        BuiltinTy::U64,
        BuiltinLinkage::CalleeNameDispatchOnly,
    ),
    // Active-mode `conn.attach(handler)` dispatch symbol.
    //
    // The checker rewrites every `conn.attach(handler)` site on a
    // `net.Connection` receiver to a direct call to `hew_tcp_attach_local`
    // (see hew-types::check::methods); HIR's direct-call lowering produces a
    // `Terminator::Call("hew_tcp_attach_local", [conn, handler])`. Codegen
    // intercepts that call by name (`emit_tcp_attach_local_call`): it resolves
    // the concrete actor type from the `handler` arg's recorded
    // `LocalPid<Actor>` type, looks up the actor's `on_data` / `on_close`
    // handler `msg_id`s in its `ActorLayout`, and emits the real runtime ABI
    // `hew_tcp_attach_local(conn, actor_ptr, on_data_id, on_close_id)`.
    //
    // Linkage is `CalleeNameDispatchOnly`, mirroring `hew_remote_pid_send`:
    // the real runtime symbol has a 4-arg ABI declared via `intern_runtime_decl`
    // inside the codegen interceptor, not through the catalog FFI predeclare
    // path. The catalog entry's params/return shape only needs to satisfy HIR's
    // fn_registry lookup and MIR's `module_fn_names` membership so the call
    // lowers to `Terminator::Call`; the params are not consulted by typecheck.
    direct(
        "hew_tcp_attach_local",
        BuiltinClass::ClassB,
        &[BuiltinTy::I32, BuiltinTy::Pointer],
        BuiltinTy::Unit,
        BuiltinLinkage::CalleeNameDispatchOnly,
    ),
    // ── W5.005 (F1b): memory-intrinsic floor (`mem.*`) ────────────────────
    //
    // The general-purpose heap allocator + typed-pointer primitives that back
    // the (upcoming) generic containers.  These are compiler-internal-only
    // (A605): declarable/callable ONLY from the `std.mem` floor module (gated
    // by `INTRINSIC_FLOOR_MODULES`), never from user surface.
    //
    // Linkage is `CalleeNameDispatchOnly` (NOT `CompilerIntrinsic`): the call
    // must reach `Terminator::Call` so codegen can intercept it by callee name
    // at the `llvm.rs` interceptor chain.  `CompilerIntrinsic` rows are excluded
    // from MIR's `module_fn_names` (they lower through numeric method-rewrites,
    // not direct calls) and therefore never reach `Terminator::Call` — there is
    // no working codegen path for them.  Using `CalleeNameDispatchOnly` mirrors
    // the `hew_remote_pid_send` precedent above; the catalog inventory gate
    // treats both identically (no C-ABI symbol to classify).
    //
    // alloc/realloc/dealloc lower to `build_call` of the hew-runtime symbols
    // `hew_alloc`/`hew_realloc`/`hew_dealloc` (Slice 1); `ptr_offset` lowers to
    // a byte-level LLVM in-bounds GEP and `ptr_copy` to `@llvm.memcpy`.  The
    // pointer ops are byte-level monomorphic (A612): they model pointers as
    // `*mut u8` and count raw bytes — the caller (a compiler-authored
    // container) has already multiplied by the element size, so no `<T>`
    // element scaling happens in the floor.  Typecheck is driven by the
    // `std.mem` surface declarations.
    direct(
        "mem.alloc",
        BuiltinClass::ClassB,
        &[BuiltinTy::U64, BuiltinTy::U64],
        BuiltinTy::Pointer,
        BuiltinLinkage::CalleeNameDispatchOnly,
    ),
    direct(
        "mem.realloc",
        BuiltinClass::ClassB,
        &[
            BuiltinTy::Pointer,
            BuiltinTy::U64,
            BuiltinTy::U64,
            BuiltinTy::U64,
        ],
        BuiltinTy::Pointer,
        BuiltinLinkage::CalleeNameDispatchOnly,
    ),
    direct(
        "mem.dealloc",
        BuiltinClass::ClassB,
        &[BuiltinTy::Pointer, BuiltinTy::U64, BuiltinTy::U64],
        BuiltinTy::Unit,
        BuiltinLinkage::CalleeNameDispatchOnly,
    ),
    direct(
        "mem.ptr_offset",
        BuiltinClass::ClassB,
        &[BuiltinTy::Pointer, BuiltinTy::U64],
        BuiltinTy::Pointer,
        BuiltinLinkage::CalleeNameDispatchOnly,
    ),
    direct(
        "mem.ptr_copy",
        BuiltinClass::ClassB,
        &[BuiltinTy::Pointer, BuiltinTy::Pointer, BuiltinTy::U64],
        BuiltinTy::Unit,
        BuiltinLinkage::CalleeNameDispatchOnly,
    ),
];

#[must_use]
pub fn entries() -> &'static [BuiltinEntry] {
    CATALOG
}

#[must_use]
pub fn missing_import_module(short_name: &str) -> Option<&'static str> {
    match short_name {
        "fs" => Some("std::fs"),
        "json" => Some("std::encoding::json"),
        _ => None,
    }
}

#[must_use]
pub fn missing_import_hint(module: &str) -> String {
    format!("add 'import {module};' at the top of the file")
}

#[must_use]
pub fn is_overloaded_builtin(name: &str) -> bool {
    matches!(
        name,
        "println" | "print" | "to_string" | "assert_eq" | "assert_ne" | "len"
    )
}

#[must_use]
pub fn resolve_overload(name: &str, arg_tys: &[ResolvedTy]) -> Option<&'static BuiltinEntry> {
    let lowered_name = overload_lowered_name(name, arg_tys)?;
    CATALOG.iter().find(|entry| entry.name == lowered_name)
}

fn overload_lowered_name(name: &str, arg_tys: &[ResolvedTy]) -> Option<&'static str> {
    match name {
        "println" if arg_tys.len() == 1 => {
            print_suffix(&arg_tys[0]).and_then(println_name_for_suffix)
        }
        "print" if arg_tys.len() == 1 => print_suffix(&arg_tys[0]).and_then(print_name_for_suffix),
        "to_string" if arg_tys.len() == 1 => to_string_name_for_ty(&arg_tys[0]),
        "assert_eq" if arg_tys.len() == 2 && arg_tys[0] == arg_tys[1] => {
            assert_eq_name_for_ty(&arg_tys[0])
        }
        "assert_ne" if arg_tys.len() == 2 && arg_tys[0] == arg_tys[1] => {
            assert_ne_name_for_ty(&arg_tys[0])
        }
        "len" if arg_tys.len() == 1 => len_name_for_ty(&arg_tys[0]),
        _ => None,
    }
}

fn print_suffix(ty: &ResolvedTy) -> Option<&'static str> {
    match ty {
        ResolvedTy::I32 => Some("i32"),
        ResolvedTy::I64 => Some("i64"),
        ResolvedTy::U8 => Some("u8"),
        ResolvedTy::U32 => Some("u32"),
        ResolvedTy::U64 => Some("u64"),
        ResolvedTy::F64 => Some("f64"),
        ResolvedTy::Bool => Some("bool"),
        ResolvedTy::String => Some("string"),
        _ => None,
    }
}

fn println_name_for_suffix(suffix: &str) -> Option<&'static str> {
    match suffix {
        "i32" => Some("println_i32"),
        "i64" => Some("println_i64"),
        "u8" => Some("println_u8"),
        "u32" => Some("println_u32"),
        "u64" => Some("println_u64"),
        "f64" => Some("println_f64"),
        "bool" => Some("println_bool"),
        "string" => Some("println_str"),
        _ => None,
    }
}

fn print_name_for_suffix(suffix: &str) -> Option<&'static str> {
    match suffix {
        "i32" => Some("print_i32"),
        "i64" => Some("print_i64"),
        "u8" => Some("print_u8"),
        "u32" => Some("print_u32"),
        "u64" => Some("print_u64"),
        "f64" => Some("print_f64"),
        "bool" => Some("print_bool"),
        "string" => Some("print_str"),
        _ => None,
    }
}

fn to_string_name_for_ty(ty: &ResolvedTy) -> Option<&'static str> {
    match ty {
        ResolvedTy::I32 => Some("to_string_i32"),
        ResolvedTy::I64 => Some("to_string_i64"),
        ResolvedTy::U8 => Some("to_string_u8"),
        ResolvedTy::U16 => Some("to_string_u16"),
        ResolvedTy::U32 => Some("to_string_u32"),
        ResolvedTy::U64 => Some("to_string_u64"),
        ResolvedTy::F64 => Some("to_string_f64"),
        ResolvedTy::Bool => Some("to_string_bool"),
        ResolvedTy::Char => Some("to_string_char"),
        ResolvedTy::String => Some("to_string_str"),
        _ => None,
    }
}

fn assert_eq_name_for_ty(ty: &ResolvedTy) -> Option<&'static str> {
    match ty {
        ResolvedTy::I64 => Some("assert_eq_i64"),
        ResolvedTy::U8 => Some("assert_eq_u8"),
        ResolvedTy::String => Some("assert_eq_str"),
        ResolvedTy::F64 => Some("assert_eq_f64"),
        ResolvedTy::Bool => Some("assert_eq_bool"),
        _ => None,
    }
}

fn assert_ne_name_for_ty(ty: &ResolvedTy) -> Option<&'static str> {
    match ty {
        ResolvedTy::I64 => Some("assert_ne_i64"),
        ResolvedTy::U8 => Some("assert_ne_u8"),
        ResolvedTy::String => Some("assert_ne_str"),
        ResolvedTy::F64 => Some("assert_ne_f64"),
        ResolvedTy::Bool => Some("assert_ne_bool"),
        _ => None,
    }
}

fn len_name_for_ty(ty: &ResolvedTy) -> Option<&'static str> {
    match ty {
        ResolvedTy::String => Some("len_str"),
        ResolvedTy::Named { name, .. } if name == "Vec" => Some("len_vec"),
        _ => None,
    }
}
