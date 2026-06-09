//! Foundation lift validation tests.
//!
//! Covers two invariants the parametric `MonoKey<K>` foundation
//! promises to downstream subsystems:
//!
//! 1. **Byte compatibility** — every function-mono mangled name
//!    produced by the legacy [`hew_hir::monomorph::mangle`] helper
//!    equals the new
//!    [`hew_hir::mono::mangle_instantiation`]`(SymbolClass::Function,
//!    ..., &[])` output for the same inputs. This is the non-breaking-
//!    change guarantee: no callsite consuming the legacy helper's
//!    output observes any change in mangled symbols across this
//!    refactor.
//! 2. **Class-tag uniqueness** — two instantiations sharing origin
//!    name and identical type args but differing only in
//!    [`SymbolClass`] mangle to distinct symbols. This is the
//!    cross-category collision guarantee that makes
//!    `MachineMonoKey` / `MonoKey<Actor>` safe to dispatch to
//!    downstream codegen alongside the existing function-mono symbols.

use hew_hir::mono::{mangle_instantiation, ConstValue, SymbolClass};
use hew_hir::monomorph::mangle as legacy_mangle;
use hew_types::ResolvedTy;

fn sample_resolved_tys() -> Vec<Vec<ResolvedTy>> {
    let int_named = ResolvedTy::Named {
        name: "Int".to_string(),
        args: vec![],
        builtin: None,
    };
    let vec_int = ResolvedTy::Named {
        name: "Vec".to_string(),
        args: vec![ResolvedTy::I32],
        builtin: None,
    };
    let nested = ResolvedTy::Named {
        name: "Vec".to_string(),
        args: vec![ResolvedTy::Named {
            name: "Vec".to_string(),
            args: vec![ResolvedTy::Bool],
            builtin: None,
        }],
        builtin: None,
    };
    let module_qualified = ResolvedTy::Named {
        name: "std::collections::HashMap".to_string(),
        args: vec![ResolvedTy::String, ResolvedTy::I64],
        builtin: None,
    };
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
            params: vec![ResolvedTy::I32, ResolvedTy::I32],
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
        SymbolClass::Machine,
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

/// `SymbolClass::Machine` (and every non-Function class) emits a class
/// prefix. Verify the exact format.
#[test]
fn non_function_class_emits_prefix() {
    let machine_mangled =
        mangle_instantiation(SymbolClass::Machine, "Counter", &[ResolvedTy::I64], &[]);
    assert_eq!(machine_mangled, "mc$$Counter$$i64");

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
    let empty = mangle_instantiation(SymbolClass::Machine, "M", &[ResolvedTy::I32], &[]);
    assert_eq!(empty, "mc$$M$$i32");

    let one = mangle_instantiation(
        SymbolClass::Machine,
        "M",
        &[ResolvedTy::I32],
        &[ConstValue::Usize(16)],
    );
    assert_eq!(one, "mc$$M$$i32$$c$u16");

    let two = mangle_instantiation(
        SymbolClass::Machine,
        "M",
        &[ResolvedTy::I32],
        &[ConstValue::Usize(16), ConstValue::Usize(32)],
    );
    assert_eq!(two, "mc$$M$$i32$$c$u16$u32");
}

/// `MonoKey<Function>` constructed via the parametric helper and then
/// mangled equals the legacy `mangle` output for the same name/args.
/// Exercises the `mangle_no_const_args` specialisation.
#[test]
fn function_mono_key_mangle_matches_legacy() {
    use hew_hir::ids::ItemId;
    use hew_hir::mono::FunctionMonoKey;

    let origin = ItemId(7);
    for name in sample_origin_names() {
        for type_args in sample_resolved_tys() {
            let key = FunctionMonoKey::new(origin, (*name).to_string(), type_args.clone());
            let key_mangled = key.mangle_no_const_args();
            let legacy = legacy_mangle(name, &type_args);
            assert_eq!(key_mangled, legacy);
        }
    }
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

/// Machine class: a module-qualified origin name has `::` replaced by `__`.
#[test]
fn mangle_instantiation_machine_sanitizes_origin_name() {
    // `My::Module::Foo` → sanitized to `My__Module__Foo`
    let out = mangle_instantiation(
        SymbolClass::Machine,
        "My::Module::Foo",
        &[ResolvedTy::I64],
        &[],
    );
    assert_eq!(out, "mc$$My__Module__Foo$$i64");

    // A name with hyphens (unusual but should not produce unsafe chars).
    let hyphen = mangle_instantiation(
        SymbolClass::Machine,
        "some-machine",
        &[ResolvedTy::Bool],
        &[],
    );
    assert_eq!(hyphen, "mc$$some_machine$$bool");
}

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

/// `MachineMonoKey::mangle` routes through the class-tagged path and
/// produces the expected `mc$$<name>$$<args>` shape for the empty-
/// const-args common case.
#[test]
fn machine_mono_key_mangle_uses_machine_class() {
    use hew_hir::ids::ItemId;
    use hew_hir::mono::MachineMonoKey;

    let key = MachineMonoKey::new(
        ItemId(11),
        "Lifecycle".to_string(),
        vec![ResolvedTy::Named {
            name: "File".to_string(),
            args: vec![],
            builtin: None,
        }],
    );
    let mangled = key.mangle();
    assert_eq!(mangled, "mc$$Lifecycle$$File");
}
