//! HIR-visible stdlib builtin catalog.
//!
//! This is the resolver-side catalog only. `RuntimeFfiShim` entries name real
//! exported runtime symbols; compiler/codegen magic uses distinct linkage
//! variants so the catalog never pretends a HIR shim name is a C ABI symbol.

use hew_types::ResolvedTy;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BuiltinClass {
    ClassA,
    ClassB,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PrintKind {
    I32,
    I64,
    U32,
    U64,
    F64,
    Bool,
    Str,
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
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BuiltinTy {
    I32,
    I64,
    U32,
    U64,
    F64,
    Bool,
    Char,
    String,
    Unit,
    Never,
    VecAny,
}

impl BuiltinTy {
    #[must_use]
    pub fn to_resolved(self) -> ResolvedTy {
        match self {
            BuiltinTy::I32 => ResolvedTy::I32,
            BuiltinTy::I64 => ResolvedTy::I64,
            BuiltinTy::U32 => ResolvedTy::U32,
            BuiltinTy::U64 => ResolvedTy::U64,
            BuiltinTy::F64 => ResolvedTy::F64,
            BuiltinTy::Bool => ResolvedTy::Bool,
            BuiltinTy::Char => ResolvedTy::Char,
            BuiltinTy::String => ResolvedTy::String,
            BuiltinTy::Unit => ResolvedTy::Unit,
            BuiltinTy::Never => ResolvedTy::Never,
            BuiltinTy::VecAny => ResolvedTy::Named {
                name: "Vec".to_string(),
                args: vec![ResolvedTy::Named {
                    name: "T".to_string(),
                    args: vec![],
                }],
            },
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
const U32: &[BuiltinTy] = &[BuiltinTy::U32];
const U64: &[BuiltinTy] = &[BuiltinTy::U64];
const F64: &[BuiltinTy] = &[BuiltinTy::F64];
const BOOL: &[BuiltinTy] = &[BuiltinTy::Bool];
const CHAR: &[BuiltinTy] = &[BuiltinTy::Char];
const STRING: &[BuiltinTy] = &[BuiltinTy::String];
const VEC_ANY: &[BuiltinTy] = &[BuiltinTy::VecAny];
const I64_I64: &[BuiltinTy] = &[BuiltinTy::I64, BuiltinTy::I64];
const F64_F64: &[BuiltinTy] = &[BuiltinTy::F64, BuiltinTy::F64];
const BOOL_BOOL: &[BuiltinTy] = &[BuiltinTy::Bool, BuiltinTy::Bool];
const STRING_STRING: &[BuiltinTy] = &[BuiltinTy::String, BuiltinTy::String];
const STRING_I64: &[BuiltinTy] = &[BuiltinTy::String, BuiltinTy::I64];
const STRING_I64_I64: &[BuiltinTy] = &[BuiltinTy::String, BuiltinTy::I64, BuiltinTy::I64];
const STRING_STRING_STRING: &[BuiltinTy] =
    &[BuiltinTy::String, BuiltinTy::String, BuiltinTy::String];
const F64_F64_F64: &[BuiltinTy] = &[BuiltinTy::F64, BuiltinTy::F64, BuiltinTy::F64];
const EMPTY: &[BuiltinTy] = &[];

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
    direct(
        "sleep_ms",
        BuiltinClass::ClassA,
        I64,
        BuiltinTy::Unit,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_sleep_ms",
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
    // Class A: monomorphic print/println overloads.
    print_entry!("println_i32", "println", I32, I32, true),
    print_entry!("println_i64", "println", I64, I64, true),
    print_entry!("println_u32", "println", U32, U32, true),
    print_entry!("println_u64", "println", U64, U64, true),
    print_entry!("println_f64", "println", F64, F64, true),
    print_entry!("println_bool", "println", BOOL, Bool, true),
    print_entry!("println_str", "println", STRING, Str, true),
    print_entry!("print_i32", "print", I32, I32, false),
    print_entry!("print_i64", "print", I64, I64, false),
    print_entry!("print_u32", "print", U32, U32, false),
    print_entry!("print_u64", "print", U64, U64, false),
    print_entry!("print_f64", "print", F64, F64, false),
    print_entry!("print_bool", "print", BOOL, Bool, false),
    print_entry!("print_str", "print", STRING, Str, false),
    // Class A: monomorphic to_string overloads.
    tostring_entry!("to_string_i32", I32, "hew_int_to_string"),
    tostring_entry!("to_string_i64", I64, "hew_i64_to_string"),
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
        "to_lowercase_str",
        "to_lowercase",
        STRING,
        BuiltinTy::String,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_string_to_lowercase",
        },
    ),
    overload(
        "to_uppercase_str",
        "to_uppercase",
        STRING,
        BuiltinTy::String,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_string_to_uppercase",
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
    overload(
        "find_str",
        "find",
        STRING_STRING,
        BuiltinTy::I64,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_string_find",
        },
    ),
    overload(
        "index_of_str",
        "index_of",
        STRING_STRING,
        BuiltinTy::I64,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_string_index_of_start",
        },
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
    overload(
        "char_at_str",
        "char_at",
        STRING_I64,
        BuiltinTy::I64,
        BuiltinLinkage::RuntimeFfiShim {
            symbol: "hew_string_char_at",
        },
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
        "math.abs_f",
        BuiltinClass::ClassB,
        F64,
        BuiltinTy::F64,
        BuiltinLinkage::CompilerIntrinsic {
            intrinsic: "math.abs_f",
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
        "math.max_f",
        BuiltinClass::ClassB,
        F64_F64,
        BuiltinTy::F64,
        BuiltinLinkage::CompilerIntrinsic {
            intrinsic: "math.max_f",
        },
    ),
    direct(
        "math.min_f",
        BuiltinClass::ClassB,
        F64_F64,
        BuiltinTy::F64,
        BuiltinLinkage::CompilerIntrinsic {
            intrinsic: "math.min_f",
        },
    ),
    direct(
        "math.clamp_f",
        BuiltinClass::ClassB,
        F64_F64_F64,
        BuiltinTy::F64,
        BuiltinLinkage::CompilerIntrinsic {
            intrinsic: "math.clamp_f",
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
    // ignoring a return value.  Error surfacing is deferred to a follow-on lane.
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
        ResolvedTy::U32 => Some("u32"),
        ResolvedTy::U64 => Some("u64"),
        ResolvedTy::F64 => Some("f64"),
        ResolvedTy::Bool => Some("bool"),
        ResolvedTy::String => Some("str"),
        _ => None,
    }
}

fn println_name_for_suffix(suffix: &str) -> Option<&'static str> {
    match suffix {
        "i32" => Some("println_i32"),
        "i64" => Some("println_i64"),
        "u32" => Some("println_u32"),
        "u64" => Some("println_u64"),
        "f64" => Some("println_f64"),
        "bool" => Some("println_bool"),
        "str" => Some("println_str"),
        _ => None,
    }
}

fn print_name_for_suffix(suffix: &str) -> Option<&'static str> {
    match suffix {
        "i32" => Some("print_i32"),
        "i64" => Some("print_i64"),
        "u32" => Some("print_u32"),
        "u64" => Some("print_u64"),
        "f64" => Some("print_f64"),
        "bool" => Some("print_bool"),
        "str" => Some("print_str"),
        _ => None,
    }
}

fn to_string_name_for_ty(ty: &ResolvedTy) -> Option<&'static str> {
    match ty {
        ResolvedTy::I32 => Some("to_string_i32"),
        ResolvedTy::I64 => Some("to_string_i64"),
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
        ResolvedTy::String => Some("assert_eq_str"),
        ResolvedTy::F64 => Some("assert_eq_f64"),
        ResolvedTy::Bool => Some("assert_eq_bool"),
        _ => None,
    }
}

fn assert_ne_name_for_ty(ty: &ResolvedTy) -> Option<&'static str> {
    match ty {
        ResolvedTy::I64 => Some("assert_ne_i64"),
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
