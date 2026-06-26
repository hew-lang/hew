#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
pub(super) use super::*;

// ── W4.042: builtin `None` checker-boundary type record ───────────────────────
//
// True root cause (re-plan, tip d81529ba): `synthesize_inner`'s
// `Expr::Identifier("None")` arm early-`return`s `Ty::option(Var)` and bypasses
// the universal `record_type(span, &ty)` tail every other arm reaches. With no
// `expr_types` entry the `check_program` boundary resolve has nothing to write
// back, so the post-unification concrete `Option<i64>` is never recorded at the
// `None` span. Downstream, HIR's unit-ctor fallback stamps a bare
// `Named{Option, args:[]}` → codegen D10. The v1 root cause (a no-op in the HIR
// walker `try_register_enum_instantiation`) was REFUTED: that walker works.

/// Bare builtin `None` under an `Option<i64>` return must leave a resolvable
/// `expr_types` entry that finalizes to the POST-SUBSTITUTION concrete
/// `Option<i64>` — not `Option<Var>` and not absent.
#[test]
fn builtin_none_records_option_type_at_span() {
    let src = "fn f() -> Option<i64> { None }\nfn main() { let _x = f(); }";
    let out = check_source(src);
    assert!(
        out.errors.is_empty(),
        "unexpected type errors: {:#?}",
        out.errors
    );
    let none_start = src.find("None").expect("source must contain `None`");
    // The AST identifier span keys the `expr_types` entry; match on its start
    // offset (the exact span end is a parser detail) and require exactly one
    // entry begins there.
    let mut matches = out.expr_types.iter().filter(|(k, _)| k.start == none_start);
    let (_, recorded) = matches.next().unwrap_or_else(|| {
        panic!(
            "no expr_types entry for the bare `None` at offset {none_start}; entries: {:#?}",
            out.expr_types
        )
    });
    assert!(
        matches.next().is_none(),
        "expected exactly one expr_types entry starting at the `None` offset"
    );
    match recorded {
        Ty::Named { name, args, .. } => {
            assert_eq!(
                name, "Option",
                "recorded type must be Option, got {recorded:?}"
            );
            assert_eq!(
                args.as_slice(),
                &[Ty::I64],
                "recorded `None` type must be finalized to the concrete Option<i64> \
                 (post-substitution), not Option<Var>; got {recorded:?}"
            );
        }
        other => panic!("expected Named Option<i64>, got {other:?}"),
    }
}

/// Fail-closed regression guard (must PASS on tip and stay passing): a
/// genuinely-unconstrained `None` must still surface an inference error — the
/// Stage 2 record change must NOT paper this over with a bogus literal default.
#[test]
fn unconstrained_none_is_inference_error() {
    let out = check_source("fn main() { let x = None; }");
    assert!(
        out.errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::InferenceFailed),
        "unconstrained `None` must remain a fail-closed inference error; got {:#?}",
        out.errors
    );
}

// ---------------------------------------------------------------------------
// W4.047 P1.3: typed `resolved_expr_types` handoff population invariants.
//
// These probe the substrate added in P1.1/P1.2 directly at the checker
// boundary: the typed map must be exactly the `ResolvedTy::from_ty` image of
// the concrete entries in `expr_types`, never a superset (no fabricated types)
// and never under-populated for concrete spans (totality for accepted
// programs). They are the unit-level half of the totality evidence.
// ---------------------------------------------------------------------------

/// For a real accepted program, `resolved_expr_types` is exactly the
/// `from_ty`-image of the *concrete* entries in `expr_types`: every typed entry
/// agrees with `from_ty(expr_types[k])`, and every `expr_types` entry that
/// `from_ty` accepts is present in the typed map. This is the population
/// invariant the HIR shadow assert relies on being tautological.
#[test]
fn resolved_expr_types_is_exact_from_ty_image_of_concrete_expr_types() {
    let out = check_source(
        "fn add(a: i64, b: i64) -> i64 { a + b }\n\
         fn main() { let x: i64 = add(40, 2); let y: bool = x > 0; }",
    );
    assert!(
        out.errors.is_empty(),
        "fixture must type-check cleanly; got {:#?}",
        out.errors
    );

    // No fabricated entries: every typed key exists in expr_types and matches.
    for (key, resolved) in &out.resolved_expr_types {
        let ty = out.expr_types.get(key).unwrap_or_else(|| {
            panic!("resolved_expr_types key {key:?} absent from expr_types (fabricated)")
        });
        let expected = ResolvedTy::from_ty(ty).unwrap_or_else(|e| {
            panic!("typed map holds {key:?} but expr_types type {ty:?} fails from_ty: {e}")
        });
        assert_eq!(
            resolved, &expected,
            "typed entry for {key:?} disagrees with from_ty(expr_types[{key:?}])"
        );
    }

    // Totality: every concrete expr_types entry is present in the typed map.
    for (key, ty) in &out.expr_types {
        if ResolvedTy::from_ty(ty).is_ok() {
            assert!(
                out.resolved_expr_types.contains_key(key),
                "concrete expr_types entry {key:?} ({ty:?}) missing from typed handoff map"
            );
        }
    }
}

/// A concrete accepted program populates a non-empty typed map (the handoff is
/// actually carrying data, not silently empty), and every entry is a
/// well-formed `ResolvedTy` with no residual boundary state.
#[test]
fn resolved_expr_types_populated_and_concrete_for_concrete_program() {
    let out = check_source("fn main() { let x: i64 = 7; let y: bool = x == 7; }");
    assert!(out.errors.is_empty(), "fixture must type-check cleanly");
    assert!(
        !out.resolved_expr_types.is_empty(),
        "concrete program must hand off at least one typed expr"
    );
}
