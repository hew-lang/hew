use hew_hir::{lower_program, verify_hir, ResolutionCtx};
use hew_mir::{lower_hir_module, MirCheck, MirDiagnosticKind, MirStatement};

fn pipeline(source: &str) -> hew_mir::IrPipeline {
    let parsed = hew_parser::parse(source);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let output = lower_program(&parsed.program, &ResolutionCtx);
    assert!(output.diagnostics.is_empty(), "{:?}", output.diagnostics);
    let verify = verify_hir(&output.module);
    assert!(verify.is_empty(), "{verify:?}");
    lower_hir_module(&output.module)
}

#[test]
fn elaborated_mir_makes_owned_string_drop_explicit() {
    let pipeline = pipeline(r#"fn main() { let s = "hello"; }"#);
    let func = &pipeline.elaborated_mir[0];
    assert!(func
        .statements
        .iter()
        .any(|stmt| { matches!(stmt, MirStatement::Drop { name, .. } if name == "s") }));
}

#[test]
fn bitcopy_arithmetic_has_no_drop() {
    let pipeline = pipeline("fn main() -> i64 { let x = 1 + 2; return x; }");
    let func = &pipeline.elaborated_mir[0];
    assert!(!func
        .statements
        .iter()
        .any(|stmt| matches!(stmt, MirStatement::Drop { .. })));
}

#[test]
fn checked_mir_rejects_use_after_consume() {
    let pipeline = pipeline(r#"fn main() -> String { let s = "hello"; let t = s; return s; }"#);

    assert!(pipeline.diagnostics.iter().any(|diagnostic| matches!(
        diagnostic.kind,
        MirDiagnosticKind::UseAfterConsume { ref name, .. } if name == "s"
    )));
}

#[test]
fn checked_mir_finding_carries_consume_and_use_sites() {
    // The payload-bearing `MirCheck::UseAfterConsume` shape projects
    // through to the diagnostic so a CLI consumer can point at both
    // ends of the bug, not just the binding name.
    let pipeline = pipeline(r#"fn main() -> String { let s = "hello"; let t = s; return s; }"#);
    let func = pipeline
        .checked_mir
        .iter()
        .find(|f| f.name == "main")
        .expect("main is in checked_mir");
    let finding = func
        .checks
        .iter()
        .find_map(|check| match check {
            MirCheck::UseAfterConsume {
                name,
                consumed_at,
                used_at,
                ..
            } if name == "s" => Some((*consumed_at, *used_at)),
            _ => None,
        })
        .expect("UseAfterConsume finding for s");
    assert_ne!(
        finding.0, finding.1,
        "consume and use sites are distinct: {finding:?}"
    );
}

#[test]
fn checked_mir_rejects_use_of_uninitialised_binding() {
    // `let x;` declares without initialising; the subsequent `let _y = x;`
    // reads `x` before any `Bind` for it. The check fires on the
    // statement stream, independent of whether the backend can lower the
    // value (the spine rejects this for unrelated reasons too — what we
    // care about here is that the MirCheck variant is populated and the
    // diagnostic surface carries the binding name).
    let pipeline = pipeline("fn f() { let x; let _y = x; }");

    let init_check = pipeline
        .checked_mir
        .iter()
        .flat_map(|f| f.checks.iter())
        .find(|check| matches!(check, MirCheck::InitialisedBeforeUse { name, .. } if name == "x"));
    assert!(
        init_check.is_some(),
        "InitialisedBeforeUse finding expected for x: {:?}",
        pipeline
            .checked_mir
            .iter()
            .flat_map(|f| f.checks.iter())
            .collect::<Vec<_>>()
    );
    assert!(
        pipeline.diagnostics.iter().any(|d| matches!(
            &d.kind,
            MirDiagnosticKind::InitialisedBeforeUse { name, .. } if name == "x"
        )),
        "MirDiagnostic projection must carry the InitialisedBeforeUse \
         finding to the CLI rejection channel: {:?}",
        pipeline.diagnostics
    );
}

#[test]
fn checked_mir_accepts_spine_integer_function() {
    // The v0.5 integer spine must not produce any MirCheck findings —
    // the move-checker is fail-closed only on real legality violations.
    let pipeline = pipeline("fn main() -> i64 { let x = 1 + 2; return x; }");
    for func in &pipeline.checked_mir {
        assert!(
            func.checks.is_empty(),
            "spine integer function {} has unexpected checks: {:?}",
            func.name,
            func.checks
        );
    }
}

#[test]
fn cross_function_call_types_typecheck_then_fail_closed_at_mir() {
    // With the function registry, calling add() returns i64, not Unit — so
    // HIR no longer produces a ReturnTypeMismatch. The MIR boundary is the
    // next gate: Cluster 1's spine subset does not yet lower Call
    // expressions, and function parameters do not bind to backend
    // `Place`s, so the program must fail closed at the MIR boundary
    // (LESSONS `boundary-fail-closed`). The pipeline() helper already
    // asserts HIR is clean; this test pins the MIR rejection shape.
    let pipeline = pipeline(
        "fn add(a: i64, b: i64) -> i64 { return a + b; } \
         fn main() -> i64 { return add(0, 1); }",
    );
    let names: Vec<&str> = pipeline.raw_mir.iter().map(|f| f.name.as_str()).collect();
    assert!(
        names.contains(&"add"),
        "raw_mir must include add: {names:?}"
    );
    assert!(
        names.contains(&"main"),
        "raw_mir must include main: {names:?}"
    );
    assert!(
        pipeline.diagnostics.iter().any(|d| matches!(
            &d.kind,
            MirDiagnosticKind::CutoverUnsupported { construct, .. }
                if construct == "function call"
        )),
        "call expressions must fail closed at the MIR boundary: {:?}",
        pipeline.diagnostics
    );
}

#[test]
fn unknown_user_type_rejected_at_mir_boundary() {
    // D10: Named user types with no known ValueClass must be rejected at the
    // MIR boundary so they cannot reach the backend.
    let parsed = hew_parser::parse("fn f(x: Foo) -> Foo { return x; }");
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let output = lower_program(&parsed.program, &ResolutionCtx);
    // HIR has no error — type annotations resolve to Named for any identifier.
    // The fail-closed boundary is MIR.
    assert!(
        output.diagnostics.is_empty(),
        "HIR should not error on undeclared type name alone: {:?}",
        output.diagnostics
    );
    let pipeline = lower_hir_module(&output.module);
    assert!(
        pipeline
            .diagnostics
            .iter()
            .any(|d| matches!(d.kind, MirDiagnosticKind::UnknownType { .. })),
        "unknown named type must produce UnknownType at MIR boundary: {:?}",
        pipeline.diagnostics
    );
}

#[test]
fn nested_tuple_user_type_rejected_at_mir_boundary() {
    let parsed = hew_parser::parse("fn f(x: (Foo, i64)) -> (Foo, i64) { return x; }");
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let output = lower_program(&parsed.program, &ResolutionCtx);
    assert!(output.diagnostics.is_empty(), "{:?}", output.diagnostics);

    let pipeline = lower_hir_module(&output.module);

    assert!(
        pipeline.diagnostics.iter().any(|d| {
            matches!(d.kind, MirDiagnosticKind::UnknownType { ref name } if name == "Foo")
        }),
        "nested tuple Foo must produce UnknownType at MIR boundary: {:?}",
        pipeline.diagnostics
    );
}

#[test]
fn nested_array_user_type_rejected_at_mir_boundary() {
    let parsed = hew_parser::parse("fn f(x: [Foo; 2]) -> [Foo; 2] { return x; }");
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let output = lower_program(&parsed.program, &ResolutionCtx);
    assert!(output.diagnostics.is_empty(), "{:?}", output.diagnostics);

    let pipeline = lower_hir_module(&output.module);

    assert!(
        pipeline.diagnostics.iter().any(|d| {
            matches!(d.kind, MirDiagnosticKind::UnknownType { ref name } if name == "Foo")
        }),
        "nested array Foo must produce UnknownType at MIR boundary: {:?}",
        pipeline.diagnostics
    );
}
