//! W5.007a representation-substrate contract tests.
//!
//! Covers the three behaviour-preserving substrate slices:
//!   * the `WitnessOperand::resolve` construction boundary (fail-closed on a
//!     checker-internal `Ty`, abstract on a declared type parameter),
//!   * the gated `IrPipeline::polymorphic_mir` bucket produced for generic
//!     origins (without disturbing the codegen-bound `raw_mir`), and
//!   * the `ResolvedTy::TypeParam` operands threaded through the abstract
//!     body's signature.

use std::collections::HashSet;

use hew_hir::{lower_program, ResolutionCtx};
use hew_mir::{lower_hir_module, IrPipeline, WitnessOperand};
use hew_types::module_registry::ModuleRegistry;
use hew_types::{Checker, ResolvedTy, Ty};

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

// ─── Slice 2/3: witness-operand construction boundary ──────────────────────

/// A declared type parameter resolves to the abstract `ResolvedTy::TypeParam`
/// operand the witness ops carry.
#[test]
fn witness_operand_admits_declared_type_param() {
    let mut params = HashSet::new();
    params.insert("T".to_string());
    let ty = Ty::Named {
        name: "T".to_string(),
        args: vec![],
        builtin: None,
    };
    let resolved = WitnessOperand::resolve(&ty, &params).expect("declared param resolves");
    assert_eq!(
        resolved,
        ResolvedTy::TypeParam {
            name: "T".to_string()
        }
    );
}

/// A fully-resolved concrete type passes through unchanged.
#[test]
fn witness_operand_admits_concrete_type() {
    let resolved = WitnessOperand::resolve(&Ty::I64, &HashSet::new()).expect("i64 resolves");
    assert_eq!(resolved, ResolvedTy::I64);
}

/// A checker-internal unresolved inference variable fails closed rather than
/// fabricating a placeholder operand (LESSONS `boundary-fail-closed`).
#[test]
fn witness_operand_fails_closed_on_inference_var() {
    let err = WitnessOperand::resolve(&Ty::Var(hew_types::ty::TypeVar(0)), &HashSet::new())
        .expect_err("Ty::Var must fail closed");
    let rendered = format!("{err:?}");
    assert!(
        rendered.contains("WitnessOperandUnresolved"),
        "expected WitnessOperandUnresolved, got: {rendered}"
    );
}

/// An undeclared bare `Named` is NOT silently treated as a type parameter; it
/// falls through to the ordinary user-type conversion (a resolved `Named`).
#[test]
fn witness_operand_undeclared_name_is_not_a_type_param() {
    let ty = Ty::Named {
        name: "MyRecord".to_string(),
        args: vec![],
        builtin: None,
    };
    let resolved = WitnessOperand::resolve(&ty, &HashSet::new()).expect("user type resolves");
    assert!(
        matches!(resolved, ResolvedTy::Named { .. }),
        "undeclared name must stay a Named user type, got: {resolved:?}"
    );
}

// ─── Slice 4: gated polymorphic-MIR production ─────────────────────────────

const GENERIC_SOURCE: &str = r"
    pub fn id<T>(x: T) -> T {
        x
    }

    fn main() -> i64 {
        let a: i64 = id(42);
        return 0;
    }
";

/// The generic origin is lowered once against abstract operands into the gated
/// `polymorphic_mir` bucket, with `ResolvedTy::TypeParam` in its signature.
#[test]
fn generic_origin_populates_polymorphic_mir_with_type_param_operands() {
    let pipeline = pipeline(GENERIC_SOURCE);

    let poly = pipeline
        .polymorphic_mir
        .iter()
        .find(|f| f.raw.name == "id")
        .expect("generic origin `id` must appear in polymorphic_mir");

    assert_eq!(poly.type_params, vec!["T".to_string()], "binder preserved");

    let type_param = ResolvedTy::TypeParam {
        name: "T".to_string(),
    };
    assert_eq!(poly.raw.return_ty, type_param, "abstract return type");
    assert_eq!(
        poly.raw.params,
        vec![type_param.clone()],
        "abstract parameter type"
    );
    assert!(
        poly.raw.locals.contains(&type_param),
        "abstract body must carry a TypeParam local; got: {:?}",
        poly.raw.locals
    );
}

/// The polymorphic bucket is strictly additive: the codegen-bound `raw_mir`
/// still carries ONLY the monomorphic instance and never the abstract origin.
#[test]
fn polymorphic_bucket_does_not_disturb_codegen_bound_raw_mir() {
    let pipeline = pipeline(GENERIC_SOURCE);

    let raw_names: Vec<&str> = pipeline.raw_mir.iter().map(|f| f.name.as_str()).collect();
    assert!(
        raw_names.contains(&"id$$i64"),
        "monomorphic instance expected in raw_mir; got: {raw_names:?}"
    );
    assert!(
        !raw_names.contains(&"id"),
        "abstract origin must NOT leak into codegen-bound raw_mir; got: {raw_names:?}"
    );
    // No abstract operand ever reaches a codegen-bound function.
    for f in &pipeline.raw_mir {
        assert!(
            !f.locals
                .iter()
                .chain(f.params.iter())
                .any(|t| matches!(t, ResolvedTy::TypeParam { .. })),
            "raw_mir function `{}` leaked a TypeParam operand",
            f.name
        );
    }
}

/// A program with no generic functions produces an empty polymorphic bucket —
/// the substrate is inert for monomorphic code.
#[test]
fn monomorphic_program_has_empty_polymorphic_mir() {
    let pipeline = pipeline(
        r"
        fn main() -> i64 {
            return 0;
        }
    ",
    );
    assert!(
        pipeline.polymorphic_mir.is_empty(),
        "no generic origins ⇒ empty polymorphic_mir"
    );
}

// ─── Slice 4: behaviour-preservation on the newly-lowered surface ──────────
//
// Before this slice, an UNUSED generic origin (one with no concrete
// instantiation) was never lowered at all. It is now lowered once against
// abstract operands. These tests assert that varied generic body shapes lower
// without panicking and surface in the substrate, while never leaking into the
// codegen-bound `raw_mir`.

/// Assert every named generic origin is present in `polymorphic_mir` and absent
/// from the codegen-bound `raw_mir`, and that no abstract operand leaks into a
/// codegen-bound function. Returns the pipeline for further assertions.
fn assert_substrate_clean(pipeline: &IrPipeline, origins: &[&str]) {
    for origin in origins {
        assert!(
            pipeline
                .polymorphic_mir
                .iter()
                .any(|f| f.raw.name == *origin),
            "generic origin `{origin}` must appear in polymorphic_mir"
        );
        assert!(
            !pipeline.raw_mir.iter().any(|f| f.name == *origin),
            "generic origin `{origin}` must NOT leak into codegen-bound raw_mir"
        );
    }
    for f in &pipeline.raw_mir {
        assert!(
            !f.locals
                .iter()
                .chain(f.params.iter())
                .any(|t| matches!(t, ResolvedTy::TypeParam { .. })),
            "raw_mir function `{}` leaked a TypeParam operand",
            f.name
        );
    }
}

/// An unused generic origin (never instantiated) is lowered abstractly without
/// panicking — the previously-unlowered surface.
#[test]
fn unused_generic_origin_lowers_abstractly() {
    let pipeline = pipeline(
        r"
        fn id<T>(x: T) -> T {
            return x;
        }

        fn main() -> i64 {
            return 0;
        }
    ",
    );
    assert_substrate_clean(&pipeline, &["id"]);
}

/// A generic body that mixes a concrete-typed local with an abstract `T`
/// passthrough lowers without panicking.
#[test]
fn generic_origin_with_mixed_concrete_and_abstract_locals() {
    let pipeline = pipeline(
        r"
        fn mix<T>(x: T) -> T {
            let n: i64 = 1 + 2;
            let m: i64 = n * 4;
            return x;
        }

        fn main() -> i64 {
            return 0;
        }
    ",
    );
    assert_substrate_clean(&pipeline, &["mix"]);
}

/// A multi-parameter generic origin lowers abstractly, with both binders
/// recorded.
#[test]
fn multi_type_param_origin_records_all_binders() {
    let pipeline = pipeline(
        r"
        fn first<A, B>(a: A, b: B) -> A {
            return a;
        }

        fn main() -> i64 {
            return 0;
        }
    ",
    );
    assert_substrate_clean(&pipeline, &["first"]);
    let poly = pipeline
        .polymorphic_mir
        .iter()
        .find(|f| f.raw.name == "first")
        .expect("origin present");
    assert_eq!(poly.type_params, vec!["A".to_string(), "B".to_string()]);
    assert_eq!(
        poly.raw.return_ty,
        ResolvedTy::TypeParam {
            name: "A".to_string()
        }
    );
}

/// A generic origin that calls another generic function at the abstract type
/// lowers without panicking (the diagnostics of any unsupported abstract call
/// are discarded; behaviour is preserved).
#[test]
fn generic_to_generic_call_lowers_without_panic() {
    let pipeline = pipeline(
        r"
        fn id<T>(x: T) -> T {
            return x;
        }

        fn wrap<T>(x: T) -> T {
            return id(x);
        }

        fn main() -> i64 {
            return 0;
        }
    ",
    );
    // Both origins are lowered abstractly; neither leaks into raw_mir.
    assert_substrate_clean(&pipeline, &["id", "wrap"]);
}
