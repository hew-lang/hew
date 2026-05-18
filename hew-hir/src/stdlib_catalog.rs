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
];

#[must_use]
pub fn entries() -> &'static [BuiltinEntry] {
    CATALOG
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
