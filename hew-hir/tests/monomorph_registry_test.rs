//! G-1.a: HIR monomorphisation-registry contract tests.
//!
//! Pins the producer-bridge wakeup that takes the checker's
//! `call_type_args` side-table out of `#[expect(dead_code)]` and into
//! `HirModule.monomorphisations`. Each test mirrors a row of the plan
//! `.tmp/plans/g1-generics-monomorphization.md` G-1.a checklist:
//!
//! 1. Two distinct concrete `T`s at the same generic callee produce
//!    two registry entries (`generic_fn_called_at_two_types_*`).
//! 2. The same `T` observed twice produces one entry — dedup
//!    (`generic_fn_called_twice_at_same_type_*`).
//! 3. Polymorphic self-recursion does not blow up: the checker filters
//!    inner self-calls whose args are still abstract, so the registry
//!    sees exactly one entry per concrete outer `T`
//!    (`generic_self_recursion_*`).
//! 4. Cap exhaustion emits a fail-closed diagnostic, not a panic
//!    (`monomorphisation_cap_*`).
//! 5. Non-generic callees and builtin/runtime-symbol callees never
//!    appear in the registry (`non_generic_callee_*`).
//!
//! LESSONS: `producer-bridge-before-codegen` (P1),
//! `checker-authority` (P0).

use hew_hir::{
    lower_program, lower_program_with_mono_cap, HirDiagnosticKind, ResolutionCtx,
    MONOMORPHISATION_REGISTRY_CAP,
};
use hew_types::module_registry::ModuleRegistry;
use hew_types::{Checker, ResolvedTy};

fn typecheck_and_lower(source: &str) -> hew_hir::LowerOutput {
    typecheck_and_lower_with_cap(source, MONOMORPHISATION_REGISTRY_CAP)
}

fn typecheck_and_lower_with_cap(source: &str, cap: usize) -> hew_hir::LowerOutput {
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
    lower_program_with_mono_cap(
        &parsed.program,
        &tc_output,
        &ResolutionCtx,
        cap,
        hew_hir::TargetArch::host(),
    )
}

/// Test A from the plan: a generic top-level fn called with two
/// distinct concrete `T`s produces two registry entries with distinct
/// mangled names.
#[test]
fn generic_fn_called_at_two_types_produces_two_entries() {
    // Use `<T>(x:T) -> T` shape on a top-level fn. The producer
    // side-table records `(callsite_span, [Ty::I64])` and
    // `(callsite_span, [Ty::String])`; the registry should collapse
    // these to `(id, [I64])` and `(id, [String])`.
    let source = r#"
        pub fn id<T>(x: T) -> T {
            x
        }

        fn main() -> i64 {
            let a: i64 = id(42);
            let b: string = id("hello");
            return 0;
        }
    "#;

    let output = typecheck_and_lower(source);
    assert!(
        output.diagnostics.is_empty(),
        "unexpected HIR diagnostics: {:#?}",
        output.diagnostics
    );

    let mono = &output.module.monomorphisations;
    assert_eq!(
        mono.len(),
        2,
        "expected two distinct monomorphisations; got {mono:#?}"
    );

    let names: Vec<&str> = mono.iter().map(|m| m.mangled_name.as_str()).collect();
    assert!(
        names.contains(&"id$$i64"),
        "expected mangled `id$$i64`; got {names:?}"
    );
    assert!(
        names.contains(&"id$$string"),
        "expected mangled `id$$string`; got {names:?}"
    );

    // Origin name and origin id are stable across entries.
    let origins: std::collections::HashSet<_> = mono.iter().map(|m| m.key.origin).collect();
    assert_eq!(
        origins.len(),
        1,
        "both entries must share the same origin fn id"
    );
    assert!(mono.iter().all(|m| m.key.origin_name == "id"));

    // Each entry carries exactly one concrete arg.
    let arg_sets: std::collections::HashSet<Vec<ResolvedTy>> =
        mono.iter().map(|m| m.key.type_args.clone()).collect();
    assert!(arg_sets.contains(&vec![ResolvedTy::I64]));
    assert!(arg_sets.contains(&vec![ResolvedTy::String]));
}

/// Test B from the plan: the same generic fn called twice with the
/// same concrete `T` produces a single registry entry (dedup).
#[test]
fn generic_fn_called_twice_at_same_type_dedupes() {
    let source = r"
        pub fn id<T>(x: T) -> T {
            x
        }

        fn main() -> i64 {
            let a: i64 = id(42);
            let b: i64 = id(7);
            return 0;
        }
    ";

    let output = typecheck_and_lower(source);
    assert!(
        output.diagnostics.is_empty(),
        "unexpected HIR diagnostics: {:#?}",
        output.diagnostics
    );

    let mono = &output.module.monomorphisations;
    assert_eq!(
        mono.len(),
        1,
        "two callsites at the same concrete T must dedup to one entry; got {mono:#?}"
    );
    assert_eq!(mono[0].mangled_name, "id$$i64");
    assert_eq!(mono[0].key.type_args, vec![ResolvedTy::I64]);
}

/// Test C from the plan: a generic self-recursive fn produces exactly
/// one registry entry per concrete `T` observed at outer call sites.
///
/// This documents an invariant G-1.b will rely on: the checker's
/// `record_concrete_call_type_args` (`calls.rs:78`) filters out any
/// recorded args containing inference variables, so the inner
/// `id(x)` call inside `id`'s body — whose args are still abstract
/// `T` — never appears in `call_type_args`. G-1.a therefore cannot
/// see polymorphic-recursion cycles; only outer concrete instantiations.
/// True cycle detection lands in G-1.b after body substitution makes
/// inner callsites concrete.
#[test]
fn generic_self_recursion_produces_one_entry_per_outer_t() {
    // Note: a self-recursive `id<T>` is degenerate (it never
    // terminates), but the checker accepts it because the body type-
    // checks under the polymorphic `T`. We test only the registry
    // shape, not runtime semantics.
    let source = r"
        pub fn id<T>(x: T) -> T {
            id(x)
        }

        fn main() -> i64 {
            let a: i64 = id(42);
            return 0;
        }
    ";

    let output = typecheck_and_lower(source);
    // Type-checking and lowering must not error (the polymorphic
    // self-call typechecks under abstract `T`).
    let unexpected_errors: Vec<_> = output
        .diagnostics
        .iter()
        .filter(|d| {
            !matches!(
                d.kind,
                HirDiagnosticKind::UnresolvedSymbol { .. }
                    | HirDiagnosticKind::UnresolvedInferenceVar
            )
        })
        .collect();
    assert!(
        unexpected_errors.is_empty(),
        "unexpected HIR diagnostics: {unexpected_errors:#?}"
    );

    let mono = &output.module.monomorphisations;
    assert_eq!(
        mono.len(),
        1,
        "self-recursive generic at one concrete outer T must produce \
         exactly one entry — inner polymorphic-T self-call is filtered \
         by the checker's `has_inference_var` guard. Got: {mono:#?}"
    );
    assert_eq!(mono[0].key.type_args, vec![ResolvedTy::I64]);
    assert_eq!(mono[0].mangled_name, "id$$i64");
}

/// Test D from the plan: exceeding the configured cap emits a fail-
/// closed `MonomorphisationCapExceeded` diagnostic (at most one per
/// invocation), not a panic, and does not grow the registry beyond
/// the cap.
#[test]
fn monomorphisation_cap_exceeded_emits_fail_closed_diagnostic() {
    // Set the cap to 2 and force 3 distinct concrete instantiations
    // (i64, string, bool) of the same generic fn.
    let source = r#"
        pub fn id<T>(x: T) -> T {
            x
        }

        fn main() -> i64 {
            let a: i64 = id(42);
            let b: string = id("hi");
            let c: bool = id(true);
            return 0;
        }
    "#;

    let output = typecheck_and_lower_with_cap(source, 2);

    let cap_diags: Vec<_> = output
        .diagnostics
        .iter()
        .filter(|d| {
            matches!(
                d.kind,
                HirDiagnosticKind::MonomorphisationCapExceeded { .. }
            )
        })
        .collect();
    assert_eq!(
        cap_diags.len(),
        1,
        "exactly one cap-exceeded diagnostic must fire (deduped across \
         overflowing callsites); got: {cap_diags:#?}"
    );

    match &cap_diags[0].kind {
        HirDiagnosticKind::MonomorphisationCapExceeded { cap } => {
            assert_eq!(*cap, 2, "diagnostic must report the configured cap");
        }
        _ => unreachable!(),
    }

    let mono = &output.module.monomorphisations;
    assert!(
        mono.len() <= 2,
        "registry must never grow past the cap; got {} entries: {mono:#?}",
        mono.len()
    );
}

/// Non-generic top-level fns and runtime-symbol callees never appear
/// in the registry — the registry filter pivots on
/// `FnEntry.type_params` being non-empty, which excludes both classes.
#[test]
fn non_generic_callee_does_not_appear_in_registry() {
    let source = r"
        pub fn add(x: i64, y: i64) -> i64 {
            x + y
        }

        fn main() -> i64 {
            let a: i64 = add(1, 2);
            return 0;
        }
    ";

    let output = typecheck_and_lower(source);
    assert!(
        output.diagnostics.is_empty(),
        "unexpected HIR diagnostics: {:#?}",
        output.diagnostics
    );
    assert!(
        output.module.monomorphisations.is_empty(),
        "non-generic callee must not produce a monomorphisation entry; got {:#?}",
        output.module.monomorphisations
    );
}

/// A program with no callsites at all produces an empty registry —
/// the field is always present on `HirModule` even when there is
/// nothing to monomorphise.
#[test]
fn fully_monomorphic_program_has_empty_registry() {
    let source = r"
        fn main() -> i64 {
            return 0;
        }
    ";

    let output = typecheck_and_lower(source);
    assert!(output.diagnostics.is_empty());
    assert!(output.module.monomorphisations.is_empty());
}

/// `lower_program` (the production entry point) delegates to
/// `lower_program_with_mono_cap` using
/// `MONOMORPHISATION_REGISTRY_CAP`. Pin that contract — a regression
/// that loses the wiring would skip the registry entirely from every
/// crate that imports `lower_program` directly.
#[test]
fn lower_program_uses_default_cap_and_records_entries() {
    let source = r#"
        pub fn id<T>(x: T) -> T {
            x
        }

        fn main() -> i64 {
            let a: i64 = id(42);
            let b: string = id("hello");
            return 0;
        }
    "#;

    let parsed = hew_parser::parse(source);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    let output = lower_program(
        &parsed.program,
        &tc_output,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );

    assert_eq!(
        output.module.monomorphisations.len(),
        2,
        "production `lower_program` must record monomorphisations"
    );
}
