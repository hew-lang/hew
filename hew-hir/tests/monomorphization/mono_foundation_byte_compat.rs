//! Symbol spelling, category separation and LLVM-safe origin names.
//!
//! Function symbols use the same projection as concrete layout symbols.
//! Distinct symbol classes remain disjoint for the same origin and arguments.

use hew_hir::mono::{mangle_instantiation, ConstValue, SymbolClass};
use hew_hir::monomorph::mangle as legacy_mangle;
use hew_types::ResolvedTy;

fn sample_resolved_tys() -> Vec<Vec<ResolvedTy>> {
    let int_named = ResolvedTy::named_for_test("Int", vec![]);
    let vec_int = ResolvedTy::named_for_test("Vec", vec![ResolvedTy::I32]);
    let nested = ResolvedTy::named_for_test(
        "Vec",
        vec![ResolvedTy::named_for_test("Vec", vec![ResolvedTy::Bool])],
    );
    let module_qualified = ResolvedTy::named_for_test(
        "std::collections::HashMap",
        vec![ResolvedTy::String, ResolvedTy::I64],
    );
    vec![
        vec![],
        vec![ResolvedTy::I64],
        vec![ResolvedTy::I32, ResolvedTy::Bool],
        vec![int_named],
        vec![vec_int],
        vec![nested],
        vec![module_qualified],
        vec![ResolvedTy::Tuple(vec![ResolvedTy::I8, ResolvedTy::U8])],
        vec![ResolvedTy::Array(Box::new(ResolvedTy::F32), 4)],
        vec![ResolvedTy::Slice(Box::new(ResolvedTy::Char))],
        vec![ResolvedTy::Pointer {
            is_mutable: false,
            pointee: Box::new(ResolvedTy::Bytes),
        }],
        vec![ResolvedTy::Pointer {
            is_mutable: true,
            pointee: Box::new(ResolvedTy::F64),
        }],
        vec![ResolvedTy::Function {
            capabilities: hew_parser::ast::CallableCapabilities::default(),
            params: vec![
                hew_types::CallableParameter::borrowed(ResolvedTy::I32),
                hew_types::CallableParameter::borrowed(ResolvedTy::I32),
            ],
            ret: Box::new(ResolvedTy::Bool),
        }],
        vec![ResolvedTy::Task(Box::new(ResolvedTy::Unit))],
    ]
}

fn sample_origin_names() -> &'static [&'static str] {
    &[
        "foo",
        "module_qualified::fn_name",
        "underscored_name",
        "a",
        "very_long_function_name_with_many_words",
    ]
}

/// Every legacy `mangle(name, args)` output equals
/// `mangle_instantiation(SymbolClass::Function, name, args, &[])` —
/// byte-for-byte, for every name and type-arg vector in the sample
/// fixture set.
#[test]
fn byte_compat_function_mono_mangle() {
    for name in sample_origin_names() {
        for type_args in sample_resolved_tys() {
            let legacy = legacy_mangle(name, &type_args);
            let parametric = mangle_instantiation(SymbolClass::Function, name, &type_args, &[]);
            assert_eq!(
                legacy, parametric,
                "function-mono mangle drift for origin `{name}` with type_args `{type_args:?}`: \
                 legacy=`{legacy}`, parametric=`{parametric}`",
            );
        }
    }
}

/// `SymbolClass::Function` produces no class prefix and with no
/// `const_args` produces no const segment — the output is exactly
/// `<name>$$<args>` for that case. Spot-check the exact format on a
/// representative input.
#[test]
fn function_mono_mangle_has_no_class_prefix_or_const_segment() {
    let out = mangle_instantiation(SymbolClass::Function, "foo", &[ResolvedTy::I64], &[]);
    assert_eq!(out, "foo$$i64");

    let two_args = mangle_instantiation(
        SymbolClass::Function,
        "swap",
        &[ResolvedTy::I32, ResolvedTy::Bool],
        &[],
    );
    assert_eq!(two_args, "swap$$i32$bool");
}

/// Two instantiations sharing origin name and identical type args but
/// differing only in `SymbolClass` mangle to distinct symbols. Covers
/// every cross-class pair to ensure no two tags are coincidentally
/// equal.
#[test]
fn class_tag_uniqueness() {
    let classes = [
        SymbolClass::Function,
        SymbolClass::Record,
        SymbolClass::Enum,
        SymbolClass::Actor,
        SymbolClass::TraitConcrete,
    ];

    for name in sample_origin_names() {
        for type_args in sample_resolved_tys() {
            let mut seen = std::collections::HashSet::new();
            for class in classes {
                let mangled = mangle_instantiation(class, name, &type_args, &[]);
                assert!(
                    seen.insert(mangled.clone()),
                    "class-tag collision: `{mangled}` produced by multiple classes for \
                     origin `{name}`, type_args `{type_args:?}`",
                );
            }
        }
    }
}

/// Every non-Function class emits a class prefix. Verify the exact format.
#[test]
fn non_function_class_emits_prefix() {
    let enum_mangled = mangle_instantiation(SymbolClass::Enum, "Counter", &[ResolvedTy::I64], &[]);
    assert_eq!(enum_mangled, "en$$Counter$$i64");

    let actor_mangled =
        mangle_instantiation(SymbolClass::Actor, "Worker", &[ResolvedTy::String], &[]);
    assert_eq!(actor_mangled, "ac$$Worker$$string");

    let trait_concrete_mangled = mangle_instantiation(
        SymbolClass::TraitConcrete,
        "Show_for_Int",
        &[ResolvedTy::I32],
        &[],
    );
    assert_eq!(trait_concrete_mangled, "tc$$Show_for_Int$$i32");
}

/// Non-empty `const_args` produces a `$$c$<...>` suffix; empty produces
/// no suffix.
#[test]
fn const_args_segment_format() {
    let empty = mangle_instantiation(SymbolClass::Actor, "M", &[ResolvedTy::I32], &[]);
    assert_eq!(empty, "ac$$M$$i32");

    let one = mangle_instantiation(
        SymbolClass::Actor,
        "M",
        &[ResolvedTy::I32],
        &[ConstValue::Usize(16)],
    );
    assert_eq!(one, "ac$$M$$i32$$c$u16");

    let two = mangle_instantiation(
        SymbolClass::Actor,
        "M",
        &[ResolvedTy::I32],
        &[ConstValue::Usize(16), ConstValue::Usize(32)],
    );
    assert_eq!(two, "ac$$M$$i32$$c$u16$u32");
}

// ── sanitize_for_symbol wiring tests ──────────────────────────────────────
//
// These tests guard that `mangle_instantiation` runs origin names through
// `hew_hir::sanitize_for_symbol` for every non-Function class, while
// leaving `SymbolClass::Function` byte-identical to the legacy output.
//
// The sanitizer turns any character that is not ASCII alphanumeric or `_`
// into `_`.  Module-qualified names with `::` are the primary motivating
// case: `My::Module::Foo` → `My__Module__Foo`.

/// Actor class: module-qualified origin name is sanitized identically.
#[test]
fn mangle_instantiation_actor_sanitizes_origin_name() {
    let out = mangle_instantiation(
        SymbolClass::Actor,
        "worker::ns::Pool",
        &[ResolvedTy::String],
        &[],
    );
    assert_eq!(out, "ac$$worker__ns__Pool$$string");
}

/// Record class: `::` in origin name is sanitized.
#[test]
fn mangle_instantiation_record_sanitizes_origin_name() {
    let out = mangle_instantiation(
        SymbolClass::Record,
        "std::data::Pair",
        &[ResolvedTy::I32, ResolvedTy::Bool],
        &[],
    );
    assert_eq!(out, "rc$$std__data__Pair$$i32$bool");
}

/// Enum class: `::` in origin name is sanitized.
#[test]
fn mangle_instantiation_enum_sanitizes_origin_name() {
    let out = mangle_instantiation(
        SymbolClass::Enum,
        "core::option::Option",
        &[ResolvedTy::I64],
        &[],
    );
    assert_eq!(out, "en$$core__option__Option$$i64");
}

/// Function class: origin names with `::` are preserved byte-for-byte,
/// matching the legacy `mangle` output. This is a regression guard — any
/// accidental sanitization of the Function class would break the 70-fixture
/// byte-compat snapshot and would be caught here first.
#[test]
fn mangle_instantiation_function_preserves_legacy_bytes() {
    // A qualified name that contains `::` — sanitize would change it, but
    // Function class must be exempt.
    let new_path = mangle_instantiation(
        SymbolClass::Function,
        "module_qualified::fn_name",
        &[ResolvedTy::I64],
        &[],
    );
    let legacy = legacy_mangle("module_qualified::fn_name", &[ResolvedTy::I64]);
    assert_eq!(
        new_path, legacy,
        "Function class must not sanitize origin_name — legacy bytes must be preserved"
    );
    // Explicit spot-check: the `::` survives verbatim.
    assert_eq!(new_path, "module_qualified::fn_name$$i64");
}

// ── end sanitize_for_symbol wiring tests ──────────────────────────────────
