//! Per-monomorphisation MIR lowering contract tests.
//!
//! When a generic top-level user fn is called at distinct concrete
//! type arguments, MIR lowering emits one specialised function per
//! `(origin_fn, Vec<ResolvedTy>)` pair from the HIR's
//! `HirModule.monomorphisations` registry, and rewrites each callsite
//! to `Terminator::Call` with the mangled symbol name.

use hew_hir::{lower_program, ResolutionCtx};
use hew_mir::{lower_hir_module, IrPipeline, MirDiagnosticKind, Terminator};
use hew_types::module_registry::ModuleRegistry;
use hew_types::Checker;

fn pipeline(source: &str) -> IrPipeline {
    let parsed = hew_parser::parse(source);
    assert!(parsed.errors.is_empty(), "parse: {:#?}", parsed.errors);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tco = checker.check_program(&parsed.program);
    assert!(tco.errors.is_empty(), "typecheck: {:#?}", tco.errors);
    let lowered = lower_program(
        &parsed.program,
        &tco,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    assert!(
        lowered.diagnostics.is_empty(),
        "HIR: {:#?}",
        lowered.diagnostics
    );
    lower_hir_module(&lowered.module)
}

// ─── Generic enum variant constructor tests ────────────────────────────────

/// `Option::None` written as a qualified unit-variant constructor with an
/// explicit `Option<i64>` annotation reaches MIR without type errors or
/// blocking diagnostics.  The HIR `MachineVariantCtor` for the `None` arm
/// must carry `Named { "Option", [I64] }`, not the bare `Named { "Option",
/// [] }` that the pre-fix path produced.
#[test]
fn unit_variant_ctor_qualified_none_carries_concrete_type_args() {
    let pipeline = pipeline(
        r"
        enum Option<T> { Some(T); None }
        fn main() -> i64 {
            let x: Option<i64> = Option::None;
            match x {
                Option::Some(v) => v,
                Option::None => 99,
            }
        }
        ",
    );
    let bad: Vec<_> = pipeline
        .diagnostics
        .iter()
        .filter(|d| {
            matches!(
                d.kind,
                MirDiagnosticKind::NotYetImplemented { .. }
                    | MirDiagnosticKind::UnresolvedPlace { .. }
                    | MirDiagnosticKind::UnsupportedNode { .. }
            )
        })
        .collect();
    assert!(
        bad.is_empty(),
        "blocking MIR diagnostics for Option::None: {bad:#?}"
    );
    // The enum instantiation registry must contain Option$$i64.
    let names: Vec<String> = pipeline.raw_mir.iter().map(|f| f.name.clone()).collect();
    assert!(
        names.contains(&"main".to_string()),
        "`main` must be emitted; got: {names:?}"
    );
}

/// `Maybe::Just { value: 42 }` struct-variant constructor with `Maybe<i64>`
/// annotation reaches MIR without blocking diagnostics.  The result type on
/// the `MachineVariantCtor` HIR node must be `Named { "Maybe", [I64] }`.
#[test]
fn struct_variant_ctor_carries_concrete_type_args() {
    let pipeline = pipeline(
        r"
        enum Maybe<T> { Just { value: T }; Nothing }
        fn main() -> i64 {
            let x: Maybe<i64> = Maybe::Just { value: 42 };
            match x {
                Maybe::Just { value } => value,
                Maybe::Nothing => 0,
            }
        }
        ",
    );
    let bad: Vec<_> = pipeline
        .diagnostics
        .iter()
        .filter(|d| {
            matches!(
                d.kind,
                MirDiagnosticKind::NotYetImplemented { .. }
                    | MirDiagnosticKind::UnresolvedPlace { .. }
                    | MirDiagnosticKind::UnsupportedNode { .. }
            )
        })
        .collect();
    assert!(
        bad.is_empty(),
        "blocking MIR diagnostics for Maybe::Just: {bad:#?}"
    );
    let names: Vec<String> = pipeline.raw_mir.iter().map(|f| f.name.clone()).collect();
    assert!(
        names.contains(&"main".to_string()),
        "`main` must be emitted; got: {names:?}"
    );
}

// ─── Generic function monomorphisation tests ──────────────────────────────

/// Two distinct instantiations of `id<T>` produce two MIR functions
/// with distinct mangled names; the unspecialised `id` is not emitted.
#[test]
fn two_instantiations_produce_two_specialised_mir_functions() {
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
    let pipeline = pipeline(source);

    // Reject any blocking diagnostic.
    let bad: Vec<_> = pipeline
        .diagnostics
        .iter()
        .filter(|d| {
            matches!(
                d.kind,
                MirDiagnosticKind::NotYetImplemented { .. }
                    | MirDiagnosticKind::UnresolvedPlace { .. }
                    | MirDiagnosticKind::UnsupportedNode { .. }
            )
        })
        .collect();
    assert!(bad.is_empty(), "blocking MIR diagnostics: {bad:#?}");

    let names: Vec<String> = pipeline.raw_mir.iter().map(|f| f.name.clone()).collect();
    assert!(
        names.contains(&"id$$i64".to_string()),
        "expected id$$i64 in MIR; got: {names:?}"
    );
    assert!(
        names.contains(&"id$$string".to_string()),
        "expected id$$string in MIR; got: {names:?}"
    );
    assert!(
        !names.contains(&"id".to_string()),
        "unspecialised generic `id` must not be emitted; got: {names:?}"
    );
}

/// Each call site in `main` produces a call terminator with the
/// monomorphisation's mangled symbol.
#[test]
fn callsite_rewrites_to_mangled_call_terminator() {
    let source = r#"
        pub fn id<T>(x: T) -> T {
            x
        }

        fn main() -> i64 {
            let a: i64 = id(42);
            let b: string = id("hi");
            return 0;
        }
    "#;
    let pipeline = pipeline(source);

    let main = pipeline
        .raw_mir
        .iter()
        .find(|f| f.name == "main")
        .expect("main must be emitted");

    let mut callees: Vec<String> = Vec::new();
    for block in &main.blocks {
        if let Terminator::Call { callee, .. } = &block.terminator {
            callees.push(callee.clone());
        }
    }

    callees.sort();
    assert_eq!(
        callees,
        vec!["id$$i64".to_string(), "id$$string".to_string()],
        "main must emit two call terminators to mangled monomorphisations; got {callees:?}"
    );
}

/// Inner generic call inside a generic body resolves to the right
/// per-instantiation mangled symbol after substitution.
#[test]
fn inner_generic_call_dispatches_to_substituted_mangled_symbol() {
    let source = r"
        pub fn id<T>(x: T) -> T {
            x
        }

        pub fn outer<U>(y: U) -> U {
            id(y)
        }

        fn main() -> i64 {
            let a: i64 = outer(42);
            0
        }
    ";
    let pipeline = pipeline(source);

    let names: Vec<String> = pipeline.raw_mir.iter().map(|f| f.name.clone()).collect();
    assert!(
        names.contains(&"outer$$i64".to_string()),
        "outer$$i64 expected: {names:?}"
    );
    assert!(
        names.contains(&"id$$i64".to_string()),
        "id$$i64 must be discovered via closure-under-subst: {names:?}"
    );

    // outer$$i64's body should call id$$i64, not a bare `id`.
    let outer = pipeline
        .raw_mir
        .iter()
        .find(|f| f.name == "outer$$i64")
        .expect("outer$$i64 expected");
    let mut calls = Vec::new();
    for block in &outer.blocks {
        if let Terminator::Call { callee, .. } = &block.terminator {
            calls.push(callee.clone());
        }
    }
    assert_eq!(
        calls,
        vec!["id$$i64".to_string()],
        "outer$$i64 must dispatch to id$$i64; got {calls:?}"
    );
}

/// Substituted parameter types reach the MIR function signature.
#[test]
fn monomorphised_function_signature_uses_substituted_types() {
    let source = r#"
        pub fn id<T>(x: T) -> T {
            x
        }

        fn main() -> i64 {
            let a: i64 = id(42);
            let b: string = id("hi");
            return 0;
        }
    "#;
    let pipeline = pipeline(source);

    let id_i64 = pipeline
        .raw_mir
        .iter()
        .find(|f| f.name == "id$$i64")
        .expect("id$$i64 expected");
    let id_string = pipeline
        .raw_mir
        .iter()
        .find(|f| f.name == "id$$string")
        .expect("id$$string expected");

    assert_eq!(id_i64.params, vec![hew_types::ResolvedTy::I64]);
    assert_eq!(id_i64.return_ty, hew_types::ResolvedTy::I64);

    assert_eq!(id_string.params, vec![hew_types::ResolvedTy::String]);
    assert_eq!(id_string.return_ty, hew_types::ResolvedTy::String);
}

/// Same `T` observed at multiple callsites collapses to one MIR
/// function (dedup is HIR-side; MIR emits one per registry entry).
#[test]
fn same_type_dedupes_to_one_mir_function() {
    let source = r"
        pub fn id<T>(x: T) -> T {
            x
        }

        fn main() -> i64 {
            let a: i64 = id(1);
            let b: i64 = id(2);
            return 0;
        }
    ";
    let pipeline = pipeline(source);

    let id_count = pipeline
        .raw_mir
        .iter()
        .filter(|f| f.name == "id$$i64")
        .count();
    assert_eq!(
        id_count, 1,
        "two callsites at same T must produce one specialised MIR function"
    );
}
