use hew_hir::{lower_program, verify_hir, HirDiagnosticKind, ResolutionCtx};
use hew_mir::{lower_hir_module, MirCheck, MirDiagnosticKind, MirStatement};

// ---------- @resource / @linear surface ----------
//
// These tests exercise the user-source surface for `#[resource]` and
// `#[linear]` ownership-discipline markers. The substrate they ride
// on:
//   - parser carries `TypeDecl.resource_marker` + `consuming_methods`
//   - HIR lowers `Item::TypeDecl` into `HirItem::TypeDecl` and emits
//     `ResourceMissingClose` / `LinearNoConsumingMethods` diagnostics
//     on structurally invalid declarations
//   - HIR populates `HirModule.type_classes` so that
//     `ValueClass::of_ty(Named{T})` resolves to `Linear` /
//     `AffineResource` when `T` carries the corresponding marker
//   - MIR's existing forward-scan `MustConsume` check fires when a
//     `Linear` binding reaches a `Return` site without being consumed
//
// What this surface CANNOT exercise today (deferred to a follow-on
// cluster — method-call HIR lowering, `?` operator, and runtime Drop
// dispatch):
//   - calling a consuming method (`t.commit()`) to satisfy a `Linear`
//     binding's must-consume obligation
//   - `?`-bearing fixtures (`resource_early_close_propagates_err`)
//   - emitting a binary that calls `close(consuming self)` on scope
//     exit — codegen `Instr::Drop { drop_fn: Some(_) }` is fail-closed
//     per the substrate-PR hardening

#[test]
fn linear_unconsumed_single_exit_fires_must_consume() {
    // A `#[linear]` binding live at the implicit return fires
    // `MirCheck::MustConsume`. No consuming method is needed in the
    // body to drive the check — only the marker registration that
    // makes `ValueClass::of_ty(Named{Txn})` resolve to `Linear`.
    let src = r"
        #[linear]
        type Txn {
            id: int
            fn commit(consuming self) -> int { 0 }
        }
        fn main() -> int {
            let t = Txn { id: 0 };
            42
        }
    ";
    let parsed = hew_parser::parse(src);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let output = lower_program(&parsed.program, &ResolutionCtx);
    // No HIR-validation diagnostics: `Txn` has a consuming method.
    assert!(
        !output.diagnostics.iter().any(|d| matches!(
            d.kind,
            HirDiagnosticKind::LinearNoConsumingMethods { .. }
                | HirDiagnosticKind::ResourceMissingClose { .. }
        )),
        "unexpected HIR diagnostics: {:?}",
        output.diagnostics
    );
    let verify = verify_hir(&output.module);
    assert!(verify.is_empty(), "verify diagnostics: {verify:?}");
    let pipeline = lower_hir_module(&output.module);

    let func = pipeline
        .checked_mir
        .iter()
        .find(|f| f.name == "main")
        .expect("main is in checked_mir");
    let must_consume = func.checks.iter().find_map(|check| match check {
        MirCheck::MustConsume { name, .. } if name == "t" => Some(()),
        _ => None,
    });
    assert!(
        must_consume.is_some(),
        "MustConsume should fire for unconsumed @linear binding `t`; checks: {:?}",
        func.checks
    );
}

#[test]
fn linear_no_consuming_methods_declared_fires_hir_diagnostic() {
    // `#[linear]` type whose body declares zero `consuming self`
    // methods is structurally invalid: no exit path can ever exhaust
    // a binding of this type. HIR lowering emits
    // `LinearNoConsumingMethods` at type registration.
    let src = r"
        #[linear]
        type Bad {
            x: int
        }
    ";
    let parsed = hew_parser::parse(src);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let output = lower_program(&parsed.program, &ResolutionCtx);
    let diag = output.diagnostics.iter().find(|d| {
        matches!(d.kind, HirDiagnosticKind::LinearNoConsumingMethods { ref name } if name == "Bad")
    });
    assert!(
        diag.is_some(),
        "LinearNoConsumingMethods should fire for `Bad`; diagnostics: {:?}",
        output.diagnostics
    );
}

#[test]
fn resource_missing_close_method_fires_hir_diagnostic() {
    // `#[resource]` type whose body has no method named `close`
    // declared with `consuming self`. The implicit-drop contract
    // requires this method; HIR lowering rejects.
    let src = r"
        #[resource]
        type Sock {
            fd: int
        }
    ";
    let parsed = hew_parser::parse(src);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let output = lower_program(&parsed.program, &ResolutionCtx);
    let diag = output.diagnostics.iter().find(|d| {
        matches!(d.kind, HirDiagnosticKind::ResourceMissingClose { ref name } if name == "Sock")
    });
    assert!(
        diag.is_some(),
        "ResourceMissingClose should fire for `Sock`; diagnostics: {:?}",
        output.diagnostics
    );
}

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
fn checked_mir_rejects_use_after_consume_inside_block_expression() {
    // Block expressions used to drop nested `HirStmtKind::Let` /
    // `HirStmtKind::Return` statements before they reached the
    // checker-authority stream; the move-checker only saw
    // `HirStmtKind::Expr` forwards. A real use-after-consume inside
    // `{ ... }` would compile cleanly and emit a binary. Pin the
    // recursive-lowering path here.
    let pipeline =
        pipeline(r#"fn main() -> i64 { { let s = "hello"; let t = s; let _u = s; } return 42; }"#);
    assert!(
        pipeline.diagnostics.iter().any(|d| matches!(
            &d.kind,
            MirDiagnosticKind::UseAfterConsume { name, .. } if name == "s"
        )),
        "block-nested use-after-consume must surface: {:?}",
        pipeline.diagnostics
    );
}

#[test]
fn checked_mir_rejects_use_of_uninitialised_binding_inside_block_expression() {
    // Companion to the use-after-consume case: a block-nested
    // `let x; let _y = x;` exercises the same lowering recursion and
    // must reach the move-checker.
    let pipeline = pipeline("fn main() -> i64 { { let x; let _y = x; } return 42; }");
    assert!(
        pipeline.diagnostics.iter().any(|d| matches!(
            &d.kind,
            MirDiagnosticKind::InitialisedBeforeUse { name, .. } if name == "x"
        )),
        "block-nested initialised-before-use must surface: {:?}",
        pipeline.diagnostics
    );
}

#[test]
fn checked_mir_rejects_use_after_consume_inside_if_arm() {
    // `if` arms are themselves `HirExpr`s — when an arm is a block
    // expression, the recursion path runs through `HirExprKind::If`
    // -> `lower_value(then_expr)` -> `HirExprKind::Block`. Pin that
    // the arm's nested `let` statements reach the move-checker.
    let pipeline = pipeline(
        "fn main() -> i64 { \
             let _r = if 1 + 1 { \
                 let s = \"hello\"; let t = s; let _u = s; 7 \
             } else { 8 }; \
             return 42; \
         }",
    );
    assert!(
        pipeline.diagnostics.iter().any(|d| matches!(
            &d.kind,
            MirDiagnosticKind::UseAfterConsume { name, .. } if name == "s"
        )),
        "if-arm-nested use-after-consume must surface: {:?}",
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
