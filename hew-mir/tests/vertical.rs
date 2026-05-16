use hew_hir::{lower_program, verify_hir, HirDiagnosticKind, ResolutionCtx};
use hew_mir::{
    lower_hir_module, CmpPred, Instr, MirCheck, MirDiagnosticKind, MirStatement, Terminator,
};
use hew_types::TypeCheckOutput;

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
    let output = lower_program(&parsed.program, &TypeCheckOutput::default(), &ResolutionCtx);
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
    // A declared `#[linear]` type must not produce a spurious UnknownType
    // diagnostic — the MIR layer must honour the HIR checker's type_classes
    // registry rather than treating every Named type as unknown.
    assert!(
        !pipeline.diagnostics.iter().any(|d| matches!(
            &d.kind,
            MirDiagnosticKind::UnknownType { name } if name == "Txn"
        )),
        "registered @linear type Txn must not produce UnknownType: {:?}",
        pipeline.diagnostics
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
    let output = lower_program(&parsed.program, &TypeCheckOutput::default(), &ResolutionCtx);
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
fn linear_with_consuming_method_emits_no_diagnostic() {
    // A `#[linear]` type that declares at least one `consuming self` method
    // is structurally valid; HIR must not emit `LinearNoConsumingMethods`.
    let src = r"
        #[linear]
        type Token {
            id: int
            fn consume(consuming self) -> int { 0 }
        }
    ";
    let parsed = hew_parser::parse(src);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let output = lower_program(&parsed.program, &TypeCheckOutput::default(), &ResolutionCtx);
    assert!(
        !output
            .diagnostics
            .iter()
            .any(|d| matches!(d.kind, HirDiagnosticKind::LinearNoConsumingMethods { .. })),
        "valid @linear type must not produce LinearNoConsumingMethods: {:?}",
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
    let output = lower_program(&parsed.program, &TypeCheckOutput::default(), &ResolutionCtx);
    let diag = output.diagnostics.iter().find(|d| {
        matches!(d.kind, HirDiagnosticKind::ResourceMissingClose { ref name } if name == "Sock")
    });
    assert!(
        diag.is_some(),
        "ResourceMissingClose should fire for `Sock`; diagnostics: {:?}",
        output.diagnostics
    );
}

#[test]
fn resource_with_close_method_emits_no_diagnostic() {
    // A `#[resource]` type that declares `close(consuming self)` satisfies
    // the implicit-drop contract; HIR must not emit `ResourceMissingClose`.
    let src = r"
        #[resource]
        type File {
            fd: int
            fn close(consuming self) -> int { 0 }
        }
    ";
    let parsed = hew_parser::parse(src);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let output = lower_program(&parsed.program, &TypeCheckOutput::default(), &ResolutionCtx);
    assert!(
        !output
            .diagnostics
            .iter()
            .any(|d| matches!(d.kind, HirDiagnosticKind::ResourceMissingClose { .. })),
        "valid @resource type must not produce ResourceMissingClose: {:?}",
        output.diagnostics
    );
}

fn pipeline(source: &str) -> hew_mir::IrPipeline {
    let parsed = hew_parser::parse(source);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let output = lower_program(&parsed.program, &TypeCheckOutput::default(), &ResolutionCtx);
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
    let output = lower_program(&parsed.program, &TypeCheckOutput::default(), &ResolutionCtx);
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
    let output = lower_program(&parsed.program, &TypeCheckOutput::default(), &ResolutionCtx);
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
    let output = lower_program(&parsed.program, &TypeCheckOutput::default(), &ResolutionCtx);
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

// ---------- Slice 0: bool literal + comparison binop lowering ----------
//
// CFG construction needs a constructible condition Place for `If`
// lowering. Without bool literals + comparison binops at MIR, every
// non-trivial `If` condition emits `CutoverUnsupported` and the
// fixture corpus collapses to "integer non-zero" conditions only.
// These tests pin the seam.

#[test]
fn lower_bool_literal_true_emits_consti64_one() {
    let pipeline = pipeline("fn main() -> i64 { let r = true; 42 }");
    assert!(
        pipeline.diagnostics.is_empty(),
        "bool literal must lower cleanly: {:?}",
        pipeline.diagnostics
    );
    let func = &pipeline.raw_mir[0];
    let has_const_one = func.blocks[0]
        .instructions
        .iter()
        .any(|i| matches!(i, Instr::ConstI64 { value: 1, .. }));
    assert!(
        has_const_one,
        "bool literal `true` must lower to ConstI64 {{ value: 1, .. }}: {:#?}",
        func.blocks[0].instructions
    );
}

#[test]
fn lower_bool_literal_false_emits_consti64_zero() {
    let pipeline = pipeline("fn main() -> i64 { let r = false; 42 }");
    let func = &pipeline.raw_mir[0];
    let has_const_zero = func.blocks[0]
        .instructions
        .iter()
        .filter(|i| matches!(i, Instr::ConstI64 { value: 0, .. }))
        .count()
        >= 1;
    assert!(
        has_const_zero,
        "bool literal `false` must lower to ConstI64 {{ value: 0, .. }}: {:#?}",
        func.blocks[0].instructions
    );
}

#[test]
fn lower_equality_cmp_emits_intcmp_eq() {
    let pipeline = pipeline("fn main() -> i64 { let r = 1 == 1; 42 }");
    assert!(
        pipeline.diagnostics.is_empty(),
        "comparison binop must lower cleanly: {:?}",
        pipeline.diagnostics
    );
    let func = &pipeline.raw_mir[0];
    let has_eq = func.blocks[0].instructions.iter().any(|i| {
        matches!(
            i,
            Instr::IntCmp {
                pred: CmpPred::Eq,
                ..
            }
        )
    });
    assert!(
        has_eq,
        "`1 == 1` must lower to IntCmp(Eq): {:#?}",
        func.blocks[0].instructions
    );
}

#[test]
fn lower_all_six_comparison_preds() {
    // One fixture per CmpPred variant. Pins that the BinaryOp ->
    // CmpPred routing in lower_binary is complete.
    let cases: &[(&str, CmpPred)] = &[
        ("fn f() -> i64 { let r = 1 == 2; 0 }", CmpPred::Eq),
        ("fn f() -> i64 { let r = 1 != 2; 0 }", CmpPred::NotEq),
        ("fn f() -> i64 { let r = 1 < 2; 0 }", CmpPred::SignedLess),
        ("fn f() -> i64 { let r = 1 <= 2; 0 }", CmpPred::SignedLessEq),
        ("fn f() -> i64 { let r = 1 > 2; 0 }", CmpPred::SignedGreater),
        (
            "fn f() -> i64 { let r = 1 >= 2; 0 }",
            CmpPred::SignedGreaterEq,
        ),
    ];
    for (src, expected) in cases {
        let pipeline = pipeline(src);
        let func = &pipeline.raw_mir[0];
        let got = func.blocks[0]
            .instructions
            .iter()
            .find_map(|i| match i {
                Instr::IntCmp { pred, .. } => Some(*pred),
                _ => None,
            })
            .unwrap_or_else(|| panic!("no IntCmp emitted for {src}"));
        assert_eq!(got, *expected, "wrong CmpPred for {src}");
    }
}

#[test]
fn lower_unsupported_binop_fails_closed_with_diagnostic() {
    // Previously `lower_binary` silently popped the dest local and
    // returned None for any non-{Add,Sub,Mul} binop, letting the
    // caller's `decide` run while the emitter produced no
    // instruction — a quiet fail-soft. Now the unsupported branch
    // emits a `CutoverUnsupported` so the CLI rejection surface
    // catches the construct.
    let parsed = hew_parser::parse("fn main() -> i64 { let r = 1 / 2; 0 }");
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let output = lower_program(&parsed.program, &TypeCheckOutput::default(), &ResolutionCtx);
    assert!(
        output.diagnostics.is_empty(),
        "Divide should parse + lower cleanly through HIR: {:?}",
        output.diagnostics
    );
    let pipeline = lower_hir_module(&output.module);
    assert!(
        pipeline.diagnostics.iter().any(|d| matches!(
            &d.kind,
            MirDiagnosticKind::CutoverUnsupported { construct, .. }
                if construct.contains("binary operator")
        )),
        "Divide must emit CutoverUnsupported at MIR: {:?}",
        pipeline.diagnostics
    );
}

// ---------- Slice 2: CFG construction for HirExprKind::If ----------
//
// Real If lowering builds a Branch on the entry block; then/else arm
// blocks each Goto a join block; the join block carries the result
// local that each arm Move'd into. The CFG shape is observable on
// RawMirFunction.blocks.

#[test]
fn if_expression_builds_four_blocks() {
    // entry (Branch) + then (Goto) + else (Goto) + join (Return) = 4
    let p = pipeline("fn main() -> i64 { let r = if 1 == 1 { 7 } else { 8 }; r }");
    assert!(
        p.diagnostics.is_empty(),
        "If expression must lower cleanly: {:?}",
        p.diagnostics
    );
    let func = &p.raw_mir[0];
    assert_eq!(
        func.blocks.len(),
        4,
        "If expression must produce four blocks (entry, then, else, join); got {} blocks: {:#?}",
        func.blocks.len(),
        func.blocks
            .iter()
            .map(|b| &b.terminator)
            .collect::<Vec<_>>()
    );
}

#[test]
fn if_expression_entry_block_terminates_with_branch() {
    let p = pipeline("fn main() -> i64 { let r = if 1 == 1 { 7 } else { 8 }; r }");
    let func = &p.raw_mir[0];
    match &func.blocks[0].terminator {
        Terminator::Branch {
            then_target,
            else_target,
            ..
        } => {
            assert_ne!(then_target, else_target, "branch targets must differ");
        }
        other => panic!("entry block must end in Branch; got {other:?}"),
    }
}

#[test]
fn if_expression_arm_blocks_goto_join() {
    let p = pipeline("fn main() -> i64 { let r = if 1 == 1 { 7 } else { 8 }; r }");
    let func = &p.raw_mir[0];
    let (then_target, else_target) = match &func.blocks[0].terminator {
        Terminator::Branch {
            then_target,
            else_target,
            ..
        } => (*then_target, *else_target),
        _ => panic!("expected Branch"),
    };
    let then_block = func.blocks.iter().find(|b| b.id == then_target).unwrap();
    let else_block = func.blocks.iter().find(|b| b.id == else_target).unwrap();
    let Terminator::Goto { target: then_goto } = then_block.terminator else {
        panic!("then arm must Goto join")
    };
    let Terminator::Goto { target: else_goto } = else_block.terminator else {
        panic!("else arm must Goto join")
    };
    assert_eq!(then_goto, else_goto, "both arms must Goto the same join");
}

#[test]
fn if_expression_arm_blocks_write_result_local() {
    // Each arm's tail value is Move'd into the result local; the join
    // block's value Place is the result local. Pin the alloca-result-
    // local pattern is in use (no phi at MIR layer).
    let p = pipeline("fn main() -> i64 { let r = if 1 == 1 { 7 } else { 8 }; r }");
    let func = &p.raw_mir[0];
    let (then_target, else_target) = match &func.blocks[0].terminator {
        Terminator::Branch {
            then_target,
            else_target,
            ..
        } => (*then_target, *else_target),
        _ => panic!(),
    };
    for target in [then_target, else_target] {
        let block = func.blocks.iter().find(|b| b.id == target).unwrap();
        let has_move = block
            .instructions
            .iter()
            .any(|i| matches!(i, Instr::Move { .. }));
        assert!(
            has_move,
            "arm block {} must emit Move into result local; got {:#?}",
            target, block.instructions
        );
    }
}

#[test]
fn if_expression_join_block_terminates_with_return() {
    // The trailing `r` in `... else { 8 }; r` reads the result local
    // in the join block; the function-tail Return terminator lives on
    // the join block (now the cursor's block at function_body
    // finalisation).
    let p = pipeline("fn main() -> i64 { let r = if 1 == 1 { 7 } else { 8 }; r }");
    let func = &p.raw_mir[0];
    let return_blocks: Vec<_> = func
        .blocks
        .iter()
        .filter(|b| matches!(b.terminator, Terminator::Return))
        .collect();
    assert_eq!(
        return_blocks.len(),
        1,
        "exactly one Return-terminated block per simple If: {:#?}",
        func.blocks
            .iter()
            .map(|b| &b.terminator)
            .collect::<Vec<_>>()
    );
}

#[test]
fn if_no_else_unit_typed_lowers_without_diagnostic() {
    // HIR types `if x { 7 }` (no else) as Unit. Our `lower_if` accepts
    // `else_expr: None` and emits an else block that Goto's join with
    // no Move. No CutoverUnsupported here.
    let p = pipeline("fn main() -> i64 { let _r = if 1 == 1 { 7 }; 42 }");
    assert!(
        p.diagnostics.is_empty(),
        "else-less If must lower cleanly: {:?}",
        p.diagnostics
    );
    let func = &p.raw_mir[0];
    assert_eq!(
        func.blocks.len(),
        4,
        "else-less If still produces entry/then/else/join CFG: {} blocks",
        func.blocks.len()
    );
}

#[test]
fn sequential_ifs_each_contribute_three_blocks() {
    // Two sequential let-init Ifs in the same function. Each
    // contributes a 3-block split (then + else + join); the second
    // If's entry block is the first If's join block. So the function
    // has: entry-block-0 (Branch of If1), then1, else1, join1 (=
    // entry of If2's Branch), then2, else2, join2. 7 blocks total.
    // Pins that the cursor correctly continues lowering into the
    // join block after one If and that a second alloc_block is
    // monotone past the first set.
    let p = pipeline(
        "fn main() -> i64 { \
            let a = if 1 == 1 { 7 } else { 8 }; \
            let b = if 1 == 0 { 9 } else { 10 }; \
            a + b \
        }",
    );
    assert!(
        p.diagnostics.is_empty(),
        "sequential Ifs must lower cleanly: {:?}",
        p.diagnostics
    );
    let func = &p.raw_mir[0];
    assert_eq!(
        func.blocks.len(),
        7,
        "sequential Ifs produce 7 blocks: {:#?}",
        func.blocks
            .iter()
            .map(|b| (b.id, &b.terminator))
            .collect::<Vec<_>>()
    );
    // Exactly one Return (the last join, which also holds the tail).
    let returns = func
        .blocks
        .iter()
        .filter(|b| matches!(b.terminator, Terminator::Return))
        .count();
    assert_eq!(returns, 1, "exactly one Return on linear If chain");
    // Two Branches (one per If).
    let branches = func
        .blocks
        .iter()
        .filter(|b| matches!(b.terminator, Terminator::Branch { .. }))
        .count();
    assert_eq!(branches, 2);
}

// ---------- Slice 3: per-block dataflow over the 4-state lattice ----------
//
// The new dataflow path makes branch-sensitive consume-tracking real.
// The flat-stream scan that worked under single-block MIR could not
// distinguish "consumed on one arm, used after join" from
// "consumed-then-used in the same arm" (both flatten the same way).
// Under the lattice, the former produces a MaybeConsumed state at
// the join, and a subsequent use is UseAfterConsume.

#[test]
fn cross_arm_consume_then_use_after_join_rejects() {
    // Consume `s` in the then arm only. After the join, `s`'s state
    // is MaybeConsumed (one path Consumed, one path Live). The next
    // use of `s` must be flagged as UseAfterConsume.
    let p = pipeline(
        r#"fn main() -> i64 {
            let s = "hello";
            let _r = if 1 == 1 { let _t = s; 7 } else { 8 };
            let _u = s;
            0
        }"#,
    );
    assert!(
        p.diagnostics.iter().any(|d| matches!(
            &d.kind,
            MirDiagnosticKind::UseAfterConsume { name, .. } if name == "s"
        )),
        "cross-arm consume + post-join use must surface UseAfterConsume: {:?}",
        p.diagnostics
    );
}

#[test]
fn consume_in_both_arms_then_use_after_join_rejects() {
    // Consume in BOTH arms: state at join is Consumed. Post-join
    // use rejects as plain UseAfterConsume (not the MaybeConsumed
    // variant).
    let p = pipeline(
        r#"fn main() -> i64 {
            let s = "hello";
            let _r = if 1 == 1 { let _a = s; 7 } else { let _b = s; 8 };
            let _u = s;
            0
        }"#,
    );
    assert!(
        p.diagnostics.iter().any(|d| matches!(
            &d.kind,
            MirDiagnosticKind::UseAfterConsume { name, .. } if name == "s"
        )),
        "both-arm consume + post-join use must reject: {:?}",
        p.diagnostics
    );
}

#[test]
fn consume_in_both_arms_without_post_join_use_is_accepted() {
    // Consumed on every reachable path, never used after the join.
    // The dataflow must NOT emit UseAfterConsume here.
    let p = pipeline(
        r#"fn main() -> i64 {
            let s = "hello";
            let _r = if 1 == 1 { let _a = s; 7 } else { let _b = s; 8 };
            0
        }"#,
    );
    assert!(
        !p.diagnostics.iter().any(|d| matches!(
            &d.kind,
            MirDiagnosticKind::UseAfterConsume { name, .. } if name == "s"
        )),
        "both-arm consume without post-join use must NOT fire UseAfterConsume: {:?}",
        p.diagnostics
    );
}

#[test]
fn maybe_consumed_at_join_without_post_join_use_is_accepted() {
    // `s` is consumed in the then arm, Live in the else arm. At the
    // join its state is MaybeConsumed. With no post-join use of `s`,
    // neither UseAfterConsume nor InitialisedBeforeUse should fire.
    // Pins the `Consumed ⊓ Live = MaybeConsumed` lattice cell
    // together with the "no diagnostic without a post-join use"
    // boundary.
    //
    // The `Uninit ⊓ Live = Uninit` cell (binding initialised only on
    // one arm, read after join) is not reachable from Hew source today:
    // HIR has no assignment statement (`HirStmtKind` is `Let | Expr |
    // Return`), so a binding declared before an `if` cannot be
    // conditionally initialised inside an arm. If an assignment surface
    // is added in a future version, add a companion reject test here.
    let p = pipeline(
        r#"fn main() -> i64 {
            let s = "hello";
            let _r = if 1 == 1 { let _a = s; 7 } else { 8 };
            0
        }"#,
    );
    // `s` is MaybeConsumed at the join; no post-join use → no diagnostic.
    assert!(
        !p.diagnostics.iter().any(|d| matches!(
            &d.kind,
            MirDiagnosticKind::UseAfterConsume { .. }
                | MirDiagnosticKind::InitialisedBeforeUse { .. }
        )),
        "MaybeConsumed-without-post-join-use must accept: {:?}",
        p.diagnostics
    );
}

#[test]
fn linear_consumed_in_both_branches_accepted() {
    // @linear binding consumed in BOTH arms via consuming method.
    // Without a method-call surface today, we approximate via
    // `let _t = txn; ...` in both arms (consume by move). Slice 5's
    // fixtures pin the method-consume form once the surface arrives.
    let src = r"
        #[linear]
        type Txn {
            id: int
            fn commit(consuming self) -> int { 0 }
        }
        fn main() -> int {
            let t = Txn { id: 0 };
            let _r = if 1 == 1 { let _a = t; 7 } else { let _b = t; 8 };
            42
        }
    ";
    let parsed = hew_parser::parse(src);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let output = lower_program(&parsed.program, &TypeCheckOutput::default(), &ResolutionCtx);
    assert!(
        !output.diagnostics.iter().any(|d| matches!(
            d.kind,
            HirDiagnosticKind::LinearNoConsumingMethods { .. }
                | HirDiagnosticKind::ResourceMissingClose { .. }
        )),
        "HIR must accept the type decl: {:?}",
        output.diagnostics
    );
    let p = lower_hir_module(&output.module);
    let func = p
        .checked_mir
        .iter()
        .find(|f| f.name == "main")
        .expect("main is in checked_mir");
    let has_must_consume = func.checks.iter().any(|c| {
        matches!(
            c,
            MirCheck::MustConsume { name, .. } if name == "t"
        )
    });
    assert!(
        !has_must_consume,
        "@linear binding consumed on every path must NOT fire MustConsume: {:?}",
        func.checks
    );
}

#[test]
fn linear_consumed_only_in_then_branch_rejects() {
    // The plan's canary fixture (linear_consumed_only_some_branches).
    // @linear binding consumed in then arm only; else arm leaves it
    // Live; at the Return-terminated join, state is MaybeConsumed.
    // The per-exit MustConsume check fires.
    let src = r"
        #[linear]
        type Txn {
            id: int
            fn commit(consuming self) -> int { 0 }
        }
        fn main() -> int {
            let t = Txn { id: 0 };
            let _r = if 1 == 1 { let _a = t; 7 } else { 8 };
            42
        }
    ";
    let parsed = hew_parser::parse(src);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let output = lower_program(&parsed.program, &TypeCheckOutput::default(), &ResolutionCtx);
    let p = lower_hir_module(&output.module);
    let func = p
        .checked_mir
        .iter()
        .find(|f| f.name == "main")
        .expect("main is in checked_mir");
    assert!(
        func.checks.iter().any(|c| matches!(
            c,
            MirCheck::MustConsume { name, .. } if name == "t"
        )),
        "@linear binding consumed on only one branch must fire MustConsume \
         from MaybeConsumed-at-Return: {:?}",
        func.checks
    );
}
