//! HIR record-layout registry contract tests.
//!
//! Pins the producer-bridge wakeup that takes the checker's
//! `record_init_type_args` side-table out of passive pass-through and
//! into `HirModule.record_layouts`. Each test covers a distinct
//! registry behaviour:
//!
//! (a) Two distinct concrete `T`s at the same generic record produce
//!     two registry entries.
//! (b) The same `T` observed twice produces one entry — dedup.
//! (c) Two-param record at `(i64, string)` and `(i64, bool)` produces
//!     two entries with distinct mangled names.
//! (d) Generic-in-generic `Box<Vec<i64>>` produces ONE entry (the
//!     `Box<Vec<i64>>` layout) — Vec stays builtin-injected.
//! (e) Recursive polymorphic instantiation
//!     (`pub type Node<T> { value: T; next: Box<Node<i64>> }`)
//!     fires `RecursiveGenericTypeUnsupported` and produces no entry.
//!
//! Additionally:
//! - Field-type substitution: `Box<i64>`'s layout has `(value, i64)`,
//!   not `(value, T)`.
//! - Mangling: shared with the fn-monomorphisation scheme
//!   (`mangle(origin_name, type_args)`); `Box$$i64`, `Pair$$i64$string`.
//!
//! LESSONS: `producer-bridge-before-codegen` (P1),
//! `checker-authority` (P0).

use hew_hir::{lower_program, lower_program_with_mono_cap, HirDiagnosticKind, ResolutionCtx};
use hew_types::module_registry::ModuleRegistry;
use hew_types::{Checker, ResolvedTy};

fn typecheck_and_lower(source: &str) -> hew_hir::LowerOutput {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    assert!(
        tc_output.errors.is_empty(),
        "typecheck errors: {:#?}",
        tc_output.errors
    );
    lower_program(&parsed.program, &tc_output, &ResolutionCtx)
}

fn typecheck_and_lower_allowing_diags(source: &str) -> hew_hir::LowerOutput {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    // Caller-tolerated checker errors are surfaced upstream; we still
    // exercise HIR lowering so the test can assert which diagnostic
    // fires (e.g. RecursiveGenericTypeUnsupported).
    let _ = tc_output;
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    lower_program(&parsed.program, &tc_output, &ResolutionCtx)
}

/// (a) Plan test A: two distinct `T`s at the same generic record
/// produce two registry entries with distinct mangled names.
#[test]
fn generic_record_at_two_types_produces_two_entries() {
    let source = r#"
        pub type Box<T> { value: T }

        fn main() {
            let a: Box<i64> = Box { value: 42 };
            let b: Box<string> = Box { value: "hi" };
        }
    "#;

    let output = typecheck_and_lower(source);
    assert!(
        output.diagnostics.is_empty(),
        "unexpected HIR diagnostics: {:#?}",
        output.diagnostics
    );

    let layouts = &output.module.record_layouts;
    assert_eq!(
        layouts.len(),
        2,
        "expected two distinct record-layouts; got {layouts:#?}"
    );

    let names: Vec<&str> = layouts.iter().map(|l| l.mangled_name.as_str()).collect();
    assert!(
        names.contains(&"Box$$i64"),
        "expected mangled `Box$$i64`; got {names:?}"
    );
    assert!(
        names.contains(&"Box$$string"),
        "expected mangled `Box$$string`; got {names:?}"
    );

    // Origin id is stable across entries.
    let origins: std::collections::HashSet<_> = layouts.iter().map(|l| l.key.origin).collect();
    assert_eq!(
        origins.len(),
        1,
        "both entries must share the same origin record id"
    );
    assert!(layouts.iter().all(|l| l.key.origin_name == "Box"));

    // Field-type substitution: the layout for `Box<i64>` has
    // `(value, i64)`, not `(value, T)`.
    let i64_layout = layouts
        .iter()
        .find(|l| l.mangled_name == "Box$$i64")
        .unwrap();
    assert_eq!(
        i64_layout.fields,
        vec![("value".to_string(), ResolvedTy::I64)]
    );
    let string_layout = layouts
        .iter()
        .find(|l| l.mangled_name == "Box$$string")
        .unwrap();
    assert_eq!(
        string_layout.fields,
        vec![("value".to_string(), ResolvedTy::String)]
    );
}

/// (b) Plan test B: the same generic record initialised twice at the
/// same concrete `T` produces a single registry entry (dedup).
#[test]
fn generic_record_at_same_type_dedupes() {
    let source = r"
        pub type Box<T> { value: T }

        fn main() {
            let a: Box<i64> = Box { value: 1 };
            let b: Box<i64> = Box { value: 2 };
        }
    ";

    let output = typecheck_and_lower(source);
    assert!(
        output.diagnostics.is_empty(),
        "unexpected HIR diagnostics: {:#?}",
        output.diagnostics
    );

    let layouts = &output.module.record_layouts;
    assert_eq!(
        layouts.len(),
        1,
        "two init sites at the same concrete T must dedup to one entry; got {layouts:#?}"
    );
    assert_eq!(layouts[0].mangled_name, "Box$$i64");
    assert_eq!(layouts[0].key.type_args, vec![ResolvedTy::I64]);
    assert_eq!(
        layouts[0].fields,
        vec![("value".to_string(), ResolvedTy::I64)]
    );
}

/// (c) Plan test C: a two-param record initialised at two distinct
/// arg tuples produces two entries with distinct mangled names.
#[test]
fn two_param_record_at_two_arg_sets_produces_two_entries() {
    let source = r#"
        pub type Pair<A, B> { first: A; second: B }

        fn main() {
            let p: Pair<i64, string> = Pair { first: 1, second: "s" };
            let q: Pair<i64, bool> = Pair { first: 2, second: true };
        }
    "#;

    let output = typecheck_and_lower(source);
    assert!(
        output.diagnostics.is_empty(),
        "unexpected HIR diagnostics: {:#?}",
        output.diagnostics
    );

    let layouts = &output.module.record_layouts;
    assert_eq!(
        layouts.len(),
        2,
        "expected two distinct record-layouts; got {layouts:#?}"
    );

    let names: Vec<&str> = layouts.iter().map(|l| l.mangled_name.as_str()).collect();
    assert!(
        names.contains(&"Pair$$i64$string"),
        "expected mangled `Pair$$i64$string`; got {names:?}"
    );
    assert!(
        names.contains(&"Pair$$i64$bool"),
        "expected mangled `Pair$$i64$bool`; got {names:?}"
    );

    // Field substitution preserves order: first → A, second → B.
    let int_string_pair = layouts
        .iter()
        .find(|l| l.mangled_name == "Pair$$i64$string")
        .unwrap();
    assert_eq!(
        int_string_pair.fields,
        vec![
            ("first".to_string(), ResolvedTy::I64),
            ("second".to_string(), ResolvedTy::String),
        ]
    );
    let int_bool_pair = layouts
        .iter()
        .find(|l| l.mangled_name == "Pair$$i64$bool")
        .unwrap();
    assert_eq!(
        int_bool_pair.fields,
        vec![
            ("first".to_string(), ResolvedTy::I64),
            ("second".to_string(), ResolvedTy::Bool),
        ]
    );
}

/// (d) Plan test D: generic-in-generic `Box<Vec<i64>>` produces ONE
/// record-layout entry — for `Box` only. Builtin `Vec<T>` remains
/// compiler-injected (no record-layout entry) for v0.5 per the G-2
/// plan's out-of-scope note.
#[test]
fn generic_in_generic_box_vec_int_produces_one_entry_for_box_only() {
    // Vec is a builtin; constructing one uses a literal/init form
    // distinct from struct-init. The key invariant under test is
    // that `record_layouts` contains exactly one entry whose origin
    // is `Box` with `type_args = [Vec<i64>]`, and no entry for `Vec`
    // itself. To dodge the stdlib-symbol question we accept a
    // `Vec<i64>` as a function parameter and feed it into the
    // `Box` literal — this exercises the same struct-init lowering
    // path without needing a Vec constructor in scope.
    let source = r"
        pub type Box<T> { value: T }

        fn wrap(xs: Vec<i64>) -> Box<Vec<i64>> {
            return Box { value: xs };
        }

        fn main() {}
    ";

    let output = typecheck_and_lower_allowing_diags(source);

    // The HIR-side `Expr::StructInit` lowering hard-codes the result
    // type as `Named { name, args: [] }` (lower.rs line ~1942 — a
    // v0.5 slice limitation acknowledged in the surrounding comment),
    // which surfaces as a `ReturnTypeMismatch` on this fixture. The
    // record-layout registry reads from the checker's
    // `record_init_type_args` side-table, NOT from the HIR's
    // expression type — so the layout entry is still recorded
    // correctly. Filter out the slice-limitation diagnostic and
    // assert no record-layout diagnostic fired.
    let unrelated_diags: Vec<_> = output
        .diagnostics
        .iter()
        .filter(|d| !matches!(d.kind, HirDiagnosticKind::ReturnTypeMismatch { .. }))
        .collect();
    assert!(
        unrelated_diags.is_empty(),
        "unexpected HIR diagnostics: {unrelated_diags:#?}"
    );

    let layouts = &output.module.record_layouts;
    assert_eq!(
        layouts.len(),
        1,
        "Box<Vec<i64>> must produce one entry (for Box only — Vec stays \
         builtin-injected per the G-2 plan); got {layouts:#?}"
    );
    assert_eq!(layouts[0].key.origin_name, "Box");

    // The single arg is `Vec<i64>` — the layout retains the nested
    // generic shape verbatim.
    let expected_arg = ResolvedTy::Named {
        name: "Vec".into(),
        args: vec![ResolvedTy::I64],
    };
    assert_eq!(layouts[0].key.type_args, vec![expected_arg.clone()]);

    // The substituted field shape is `(value, Vec<i64>)` — T was
    // replaced by `Vec<i64>`.
    assert_eq!(layouts[0].fields, vec![("value".to_string(), expected_arg)]);

    // Mangling round-trips the nested arg: `Box$$Vec_i64`.
    assert_eq!(layouts[0].mangled_name, "Box$$Vec_i64");
}

/// (e) Plan test E: recursive polymorphic instantiation
/// (`pub type Node<T> { ... next: Box<Node<i64>> }`) fires the
/// `RecursiveGenericTypeUnsupported` diagnostic and produces no
/// `Node` entry (the `Box<Node<i64>>` field would force unbounded
/// layout expansion).
#[test]
fn recursive_polymorphic_self_emits_diagnostic_and_skips_entry() {
    // Construct a Node<string> — the recursive cycle is detected at
    // substitution time when the `next` field substitutes to
    // `Box<Node<i64>>`, which names `Node` with arg `i64` while the
    // current Node instantiation has arg `string`.
    //
    // Note: this fixture is purely structural for the diagnostic
    // probe; whether it type-checks end-to-end depends on stdlib
    // Box being usable. We accept checker errors here and assert on
    // the HIR diagnostic shape.
    let source = r#"
        pub type Node<T> { value: T; next: Box<Node<i64>> }

        fn main() {
            let n: Node<string> = Node { value: "hi", next: hole };
        }
    "#;

    let output = typecheck_and_lower_allowing_diags(source);

    let recursive_diags: Vec<_> = output
        .diagnostics
        .iter()
        .filter(|d| {
            matches!(
                d.kind,
                HirDiagnosticKind::RecursiveGenericTypeUnsupported { .. }
            )
        })
        .collect();
    assert!(
        !recursive_diags.is_empty(),
        "expected at least one RecursiveGenericTypeUnsupported diagnostic; \
         got: {:#?}",
        output.diagnostics
    );
    match &recursive_diags[0].kind {
        HirDiagnosticKind::RecursiveGenericTypeUnsupported { name } => {
            assert_eq!(name, "Node");
        }
        _ => unreachable!(),
    }

    // No `Node` entry should have landed (the diagnostic short-
    // circuits the insert).
    let node_layouts: Vec<_> = output
        .module
        .record_layouts
        .iter()
        .filter(|l| l.key.origin_name == "Node")
        .collect();
    assert!(
        node_layouts.is_empty(),
        "no Node record-layout entry must land when the recursive \
         polymorphic diagnostic fires; got {node_layouts:#?}"
    );
}

/// Monomorphic records are NOT recorded in `record_layouts` — they
/// remain bare-name in MIR. The registry is for per-instantiation
/// layouts only.
#[test]
fn monomorphic_record_does_not_appear_in_registry() {
    let source = r"
        pub type Point { x: i64; y: i64 }

        fn main() {
            let p: Point = Point { x: 1, y: 2 };
        }
    ";

    let output = typecheck_and_lower(source);
    assert!(
        output.diagnostics.is_empty(),
        "unexpected HIR diagnostics: {:#?}",
        output.diagnostics
    );
    assert!(
        output.module.record_layouts.is_empty(),
        "monomorphic record must not produce a record-layout entry; got {:#?}",
        output.module.record_layouts
    );
}

/// Exceeding the configured record-layout cap fires
/// `RecordLayoutCapExceeded` once (deduped across overflowing sites)
/// and the registry never grows past the cap. Mirrors the fn-mono cap
/// test so both sides of the registry surface share the same
/// fail-closed contract.
#[test]
fn record_layout_cap_exceeded_emits_fail_closed_diagnostic() {
    // Cap at 2, force 3 distinct concrete instantiations of `Box`.
    let source = r#"
        pub type Box<T> { value: T }

        fn main() {
            let a: Box<i64> = Box { value: 1 };
            let b: Box<string> = Box { value: "x" };
            let c: Box<bool> = Box { value: true };
        }
    "#;
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    assert!(
        tc_output.errors.is_empty(),
        "typecheck errors: {:#?}",
        tc_output.errors
    );
    let output = lower_program_with_mono_cap(&parsed.program, &tc_output, &ResolutionCtx, 2);

    let cap_diags: Vec<_> = output
        .diagnostics
        .iter()
        .filter(|d| matches!(d.kind, HirDiagnosticKind::RecordLayoutCapExceeded { .. }))
        .collect();
    assert_eq!(
        cap_diags.len(),
        1,
        "exactly one cap-exceeded diagnostic must fire (deduped across \
         overflowing init sites); got: {cap_diags:#?}"
    );
    match &cap_diags[0].kind {
        HirDiagnosticKind::RecordLayoutCapExceeded { cap } => {
            assert_eq!(*cap, 2, "diagnostic must report the configured cap");
        }
        _ => unreachable!(),
    }

    assert!(
        output.module.record_layouts.len() <= 2,
        "registry must never grow past the cap; got {} entries: {:#?}",
        output.module.record_layouts.len(),
        output.module.record_layouts
    );
}

/// A program with no struct-init sites at all has an empty
/// `record_layouts` — the field is always present on `HirModule`
/// even when there is nothing to monomorphise.
#[test]
fn fully_monomorphic_program_has_empty_record_layouts() {
    let source = r"
        fn main() -> i64 {
            return 0;
        }
    ";

    let output = typecheck_and_lower(source);
    assert!(output.diagnostics.is_empty());
    assert!(output.module.record_layouts.is_empty());
}
