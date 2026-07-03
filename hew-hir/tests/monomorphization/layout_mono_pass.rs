//! Post-function-mono record/enum layout discovery: the layout-mono pass
//! registers concrete generic-record / generic-enum layouts reachable only
//! through a substituted generic-function body, and fails closed on the
//! out-of-slice abstract cases.

use hew_hir::{lower_program, ResolutionCtx};
use hew_types::module_registry::ModuleRegistry;
use hew_types::Checker;

fn typecheck_and_lower(source: &str) -> hew_hir::LowerOutput {
    let parsed = hew_parser::parse(source);
    assert!(parsed.errors.is_empty(), "parse: {:#?}", parsed.errors);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tco = checker.check_program(&parsed.program);
    assert!(tco.errors.is_empty(), "typecheck: {:#?}", tco.errors);
    lower_program(
        &parsed.program,
        &tco,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    )
}

fn record_layout_names(output: &hew_hir::LowerOutput) -> Vec<String> {
    output
        .module
        .record_layouts
        .iter()
        .map(|r| r.mangled_name.clone())
        .collect()
}

fn enum_layout_names(output: &hew_hir::LowerOutput) -> Vec<String> {
    output
        .module
        .enum_layouts
        .iter()
        .map(|e| e.mangled_name.clone())
        .collect()
}

/// A generic record constructed inside a generic fn body (`Box { value: x }`
/// in `make<T>`) gets a concrete `Box$$i64` layout once `make` is instantiated
/// at i64 — even though the construction site at the origin is abstract.
#[test]
fn generic_record_in_generic_body_registers_concrete_layout() {
    let source = r"
        record Box<T> {
            value: T,
        }

        fn make<T>(x: T) -> Box<T> {
            Box { value: x }
        }

        fn main() {
            let b = make(7);
            print(b.value);
        }
    ";
    let output = typecheck_and_lower(source);
    assert!(
        output.diagnostics.is_empty(),
        "diagnostics: {:#?}",
        output.diagnostics
    );
    let names = record_layout_names(&output);
    assert!(
        names.iter().any(|n| n == "Box$$i64"),
        "Box$$i64 must be discovered post-function-mono: {names:?}"
    );
}

/// The abstract origin layout `Box$$T` must NOT be registered — the field typed
/// `T` would leak an unknown type to the MIR boundary. Only concrete
/// instantiations land.
#[test]
fn abstract_origin_layout_is_not_registered() {
    let source = r"
        record Box<T> {
            value: T,
        }

        fn make<T>(x: T) -> Box<T> {
            Box { value: x }
        }

        fn main() {
            let b = make(7);
            print(b.value);
        }
    ";
    let output = typecheck_and_lower(source);
    let names = record_layout_names(&output);
    assert!(
        !names.iter().any(|n| n == "Box$$T"),
        "abstract Box$$T must never be registered (would leak `T` to MIR): {names:?}"
    );
}

/// Two distinct instantiations of the same generic record from one program
/// each get their own concrete layout — proving the discovery is polymorphic.
#[test]
fn two_record_instantiations_register_distinct_layouts() {
    let source = r"
        record Box<T> {
            value: T,
        }

        fn make<T>(x: T) -> Box<T> {
            Box { value: x }
        }

        fn main() {
            let a = make(7);
            let b = make(true);
            print(a.value);
            print(b.value);
        }
    ";
    let output = typecheck_and_lower(source);
    let names = record_layout_names(&output);
    assert!(
        names.iter().any(|n| n == "Box$$i64"),
        "Box$$i64 missing: {names:?}"
    );
    assert!(
        names.iter().any(|n| n == "Box$$bool"),
        "Box$$bool missing: {names:?}"
    );
}

/// A generic enum constructed inside a generic fn body gets its concrete
/// tagged-union layout registered once the fn is instantiated.
#[test]
fn generic_enum_in_generic_body_registers_concrete_layout() {
    let source = r"
        enum Holder<T> {
            Present(T);
            Absent;
        }

        fn wrap<T>(x: T) -> Holder<T> {
            Holder::Present(x)
        }

        fn unwrap_or<T>(h: Holder<T>, fallback: T) -> T {
            match h {
                Holder::Present(v) => v,
                Holder::Absent => fallback,
            }
        }

        fn main() {
            let h = wrap(99);
            print(unwrap_or(h, 0));
        }
    ";
    let output = typecheck_and_lower(source);
    assert!(
        output.diagnostics.is_empty(),
        "diagnostics: {:#?}",
        output.diagnostics
    );
    let names = enum_layout_names(&output);
    assert!(
        names.iter().any(|n| n == "Holder$$i64"),
        "Holder$$i64 must be discovered post-function-mono: {names:?}"
    );
}

/// A generic record reachable only through an *uninstantiated* generic fn
/// (never called) produces NO layout — the abstract shape never leaks.
#[test]
fn uninstantiated_generic_produces_no_layout() {
    let source = r"
        record Box<T> {
            value: T,
        }

        fn make<T>(x: T) -> Box<T> {
            Box { value: x }
        }

        fn main() {
            print(1);
        }
    ";
    let output = typecheck_and_lower(source);
    let names = record_layout_names(&output);
    assert!(
        !names.iter().any(|n| n.starts_with("Box$$")),
        "no Box layout should exist when make is never instantiated: {names:?}"
    );
}

// The byte-exact congruence of a `layout_mono`-discovered layout carrying a
// module-QUALIFIED payload (`Holder<lmonobox.Box>` → registered as `Holder$$Box`,
// the bare key every codegen / MIR lookup probes) is proven two ways:
//   * end-to-end, through the real `hew run` pkg-path resolver, by
//     `tests/pkg-import/postmono_qualified_layout.hew` (both the record AND enum
//     paths); and
//   * deterministically, at the `register_record` / `register_enum` surface, by
//     the unit tests in `hew-hir/src/layout_mono.rs`
//     (`register_record_shortens_qualified_payload_spine` and its enum sibling),
//     which assert the exact mangle bytes without depending on the cross-module
//     checker (the in-harness `lower_through_checker_from_program` does not
//     resolve a qualified type-arg threaded through an explicit annotation).
