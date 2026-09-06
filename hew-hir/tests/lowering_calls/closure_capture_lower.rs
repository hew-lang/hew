//! HIR-level closure capture probe tests.
//!
//! These tests verify that the HIR lowering pass correctly materialises
//! binding-accurate capture facts from the checker's `closure_capture_facts`
//! side-table into `HirExprKind::Closure::captures`. The checker-level
//! equivalents live in `hew-types/src/check/tests.rs`; this file pins the
//! producer-to-HIR boundary so a regression in `lower_closure` or
//! `materialize_closure_captures` surfaces here before reaching MIR.
//!
//! LESSONS trigger: `feedback_verify_ast_carries_discriminator_before_codegen_fix`
//! — verify the HIR closure node carries non-empty captures for a closure that
//! references an outer binding before relying on capture facts downstream.

use hew_hir::{lower_program, HirExprKind, HirItem, HirStmtKind, ResolutionCtx};
use hew_types::Checker;
use hew_types::{module_registry::ModuleRegistry, ClosureCaptureAcquisition, ResolvedTy};

/// Run the full source → typecheck → HIR pipeline.  Panics on parse or type
/// errors so individual tests can assert on the resulting HIR structure.
fn typecheck_and_lower(source: &str) -> hew_hir::LowerOutput {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tco = checker.check_program(&parsed.program);
    assert!(tco.errors.is_empty(), "type errors: {:?}", tco.errors);
    lower_program(
        &parsed.program,
        &tco,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    )
}

/// Find the `HirExprKind::Closure` node inside the body of a top-level
/// function named `outer_fn`.  Returns `None` if no closure is found.
fn find_closure_in_fn<'a>(
    output: &'a hew_hir::LowerOutput,
    fn_name: &str,
) -> Option<&'a hew_hir::HirExpr> {
    let func = output.module.items.iter().find_map(|item| {
        if let HirItem::Function(f) = item {
            if f.name == fn_name {
                return Some(f);
            }
        }
        None
    })?;
    find_closure_in_stmts(&func.body.statements, func.body.tail.as_deref())
}

fn find_closure_in_stmts<'a>(
    stmts: &'a [hew_hir::HirStmt],
    tail: Option<&'a hew_hir::HirExpr>,
) -> Option<&'a hew_hir::HirExpr> {
    for stmt in stmts {
        if let HirStmtKind::Let(_, Some(expr)) = &stmt.kind {
            if let found @ Some(_) = find_closure_in_expr(expr) {
                return found;
            }
        }
        if let HirStmtKind::Expr(expr) = &stmt.kind {
            if let found @ Some(_) = find_closure_in_expr(expr) {
                return found;
            }
        }
    }
    if let Some(tail) = tail {
        find_closure_in_expr(tail)
    } else {
        None
    }
}

fn find_closure_in_expr(expr: &hew_hir::HirExpr) -> Option<&hew_hir::HirExpr> {
    if matches!(expr.kind, HirExprKind::Closure { .. }) {
        return Some(expr);
    }
    match &expr.kind {
        HirExprKind::Block(block) => {
            find_closure_in_stmts(&block.statements, block.tail.as_deref())
        }
        HirExprKind::If {
            condition,
            then_expr,
            else_expr,
        } => find_closure_in_expr(condition)
            .or_else(|| find_closure_in_expr(then_expr))
            .or_else(|| else_expr.as_deref().and_then(find_closure_in_expr)),
        HirExprKind::MachineVariantCtor {
            payload: Some(fields),
            ..
        } => fields
            .iter()
            .find_map(|(_, value)| find_closure_in_expr(value)),
        HirExprKind::Binary { left, right, .. } => {
            find_closure_in_expr(left).or_else(|| find_closure_in_expr(right))
        }
        _ => None,
    }
}

// ── Tests ────────────────────────────────────────────────────────────────────

#[test]
fn copy_capture_produces_one_hir_capture_entry() {
    // A closure that references one outer `i64` binding via Copy capture
    // must produce exactly one `HirClosureCapture` with the correct name,
    // type, and mode.  This exercises the `checker_facts → HIR captures`
    // materialisation path in `lower_closure` / `materialize_closure_captures`.
    let output = typecheck_and_lower(
        r"
        fn main() {
            let k: i64 = 42;
            let f = |n: i64| n + k;
        }
        ",
    );
    assert!(
        output.diagnostics.is_empty(),
        "unexpected HIR diagnostics: {:?}",
        output.diagnostics
    );

    let closure_expr =
        find_closure_in_fn(&output, "main").expect("HirExprKind::Closure must be present in main");

    let HirExprKind::Closure {
        captures,
        params,
        ret_ty,
        ..
    } = &closure_expr.kind
    else {
        panic!("expected Closure kind; got {:?}", closure_expr.kind);
    };

    assert_eq!(
        captures.len(),
        1,
        "one outer binding `k` must produce exactly one capture; got {captures:?}"
    );
    let cap = &captures[0];
    assert_eq!(cap.name, "k", "captured binding name must be `k`");
    assert_eq!(
        cap.ty,
        ResolvedTy::I64,
        "captured binding type must be i64; got {:?}",
        cap.ty
    );
    assert_eq!(
        cap.acquisition,
        ClosureCaptureAcquisition::Snapshot,
        "i64 is Copy; capture mode must be Copy; got {:?}",
        cap.acquisition
    );

    // Params and return type are sanity-checked as a secondary invariant.
    assert_eq!(params.len(), 1, "closure has one explicit param `n`");
    assert_eq!(*ret_ty, ResolvedTy::I64, "return type is i64");
}

#[test]
fn repeated_use_of_same_binding_is_deduplicated_to_one_capture() {
    // A closure body that references the same outer binding twice (e.g. `k + k`)
    // must produce exactly ONE `HirClosureCapture` entry for that binding — the
    // checker deduplicates by `binding_id` before populating `closure_capture_facts`,
    // and the HIR materialisation walk must honour that.
    //
    // This pins the deduplication path documented in the checker test at
    // `hew-types/src/check/tests.rs::closure_capture_facts_are_binding_accurate_and_deduplicated`.
    let output = typecheck_and_lower(
        r"
        fn main() {
            let k: i64 = 2;
            let f = |n: i64| n + k + k;
        }
        ",
    );
    assert!(
        output.diagnostics.is_empty(),
        "unexpected HIR diagnostics: {:?}",
        output.diagnostics
    );

    let closure_expr =
        find_closure_in_fn(&output, "main").expect("HirExprKind::Closure must be present in main");
    let HirExprKind::Closure { captures, .. } = &closure_expr.kind else {
        panic!("expected Closure kind");
    };

    let k_captures: Vec<_> = captures.iter().filter(|c| c.name == "k").collect();
    assert_eq!(
        k_captures.len(),
        1,
        "repeated use of `k` must deduplicate to one capture entry; got {k_captures:?}"
    );
    assert_eq!(
        k_captures[0].ty,
        ResolvedTy::I64,
        "deduplicated capture type must be i64"
    );
}

#[test]
fn non_capturing_closure_has_empty_capture_list() {
    // A closure that only references its own parameters and no outer bindings
    // must produce an empty captures list. This verifies the walker does NOT
    // include parameter bindings as captures (they're in scope within the
    // closure body's own scope, not from the outer closure scope).
    let output = typecheck_and_lower(
        r"
        fn main() {
            let f = |n: i64| n + 1;
        }
        ",
    );
    assert!(
        output.diagnostics.is_empty(),
        "unexpected HIR diagnostics: {:?}",
        output.diagnostics
    );

    let closure_expr =
        find_closure_in_fn(&output, "main").expect("HirExprKind::Closure must be present in main");
    let HirExprKind::Closure { captures, .. } = &closure_expr.kind else {
        panic!("expected Closure kind");
    };

    assert!(
        captures.is_empty(),
        "a closure that captures nothing must have an empty captures list; got {captures:?}"
    );
}

#[test]
fn closure_capture_binding_id_is_stable_across_lowering() {
    // The `HirClosureCapture::binding` id must match the `BindingId` assigned
    // to the outer `let k` binding.  This verifies that `lower_closure` threads
    // the binding identity from the enclosing scope through the capture, not a
    // freshly allocated id.
    let output = typecheck_and_lower(
        r"
        fn main() {
            let k: i64 = 42;
            let f = |n: i64| n + k;
        }
        ",
    );
    assert!(
        output.diagnostics.is_empty(),
        "unexpected HIR diagnostics: {:?}",
        output.diagnostics
    );

    let func = output
        .module
        .items
        .iter()
        .find_map(|item| {
            if let HirItem::Function(f) = item {
                if f.name == "main" {
                    return Some(f);
                }
            }
            None
        })
        .expect("`main` must exist");

    // Pull the BindingId for `let k` from the first Let statement.
    let k_binding_id = func
        .body
        .statements
        .iter()
        .find_map(|stmt| {
            if let HirStmtKind::Let(binding, _) = &stmt.kind {
                if binding.name == "k" {
                    return Some(binding.id);
                }
            }
            None
        })
        .expect("`let k` must be the first let statement in main");

    // Find the closure and retrieve its captures.
    let closure_expr = find_closure_in_fn(&output, "main").expect("closure must exist");
    let HirExprKind::Closure { captures, .. } = &closure_expr.kind else {
        panic!("expected Closure kind");
    };

    let cap = captures
        .iter()
        .find(|c| c.name == "k")
        .expect("capture for `k` must be present");

    assert_eq!(
        cap.binding, k_binding_id,
        "capture binding id must match the outer `let k` binding id"
    );
}

#[test]
fn callable_joins_preserve_checker_guarantees_in_either_order() {
    for choice in [
        "if flag { a } else { b }",
        "if flag { b } else { a }",
        "match flag { true => a, false => b }",
        "match flag { true => b, false => a }",
    ] {
        let source = format!("fn choose(a: fn[clone]() -> i64, b: fn[once, clone]() -> i64, flag: bool) {{ let f = {choice}; }}");
        let output = typecheck_and_lower(&source);
        assert!(
            output.diagnostics.is_empty(),
            "{choice}: {:?}",
            output.diagnostics
        );
        let function = output
            .module
            .items
            .iter()
            .find_map(|item| match item {
                HirItem::Function(f) if f.name == "choose" => Some(f),
                _ => None,
            })
            .unwrap();
        let value = function
            .body
            .statements
            .iter()
            .find_map(|stmt| match &stmt.kind {
                HirStmtKind::Let(_, Some(value)) => Some(value),
                _ => None,
            })
            .unwrap();
        assert!(
            matches!(value.ty, ResolvedTy::Function { capabilities, .. } if capabilities.call == hew_types::CallableCallMode::Once && capabilities.clone),
            "{choice}: {:?}",
            value.ty
        );
    }
}

#[test]
fn callable_return_erasure_preserves_concrete_closure_type() {
    let output =
        typecheck_and_lower("fn make() -> fn[once]() -> i64 { let n: i64 = 7; return || n; }");
    assert!(output.diagnostics.is_empty(), "{:?}", output.diagnostics);
    let function = output
        .module
        .items
        .iter()
        .find_map(|item| match item {
            HirItem::Function(f) if f.name == "make" => Some(f),
            _ => None,
        })
        .unwrap();
    let returned = function
        .body
        .statements
        .iter()
        .find_map(|stmt| match &stmt.kind {
            HirStmtKind::Return(Some(value)) => Some(value),
            _ => None,
        })
        .unwrap();
    assert!(
        matches!(returned.ty, ResolvedTy::Closure { capabilities, .. } if capabilities.call == hew_types::CallableCallMode::Read && capabilities.clone),
        "{:?}",
        returned.ty
    );
}

#[test]
fn contextual_option_result_preserves_nested_callable_storage() {
    let output = typecheck_and_lower("fn choose(flag: bool) -> Result<Option<fn[var, clone]() -> i64>, string> { let count: i64 = 0; if flag { .Ok(.Some(capture(var count) || { count = count + 1; count })) } else { .Ok(.None) } }");
    assert!(output.diagnostics.is_empty(), "{:?}", output.diagnostics);
    let closure = find_closure_in_fn(&output, "choose")
        .expect("closure retained in nested constructor payloads");
    assert!(
        matches!(closure.ty, ResolvedTy::Closure { capabilities, .. } if capabilities.call == hew_types::CallableCallMode::Var && capabilities.clone)
    );
    let HirExprKind::Closure { captures, .. } = &closure.kind else {
        panic!("closure")
    };
    assert_eq!(captures[0].access, hew_types::ClosureCaptureAccess::Var);
}

#[test]
fn generic_function_values_register_concrete_targets_and_site_arguments() {
    let output = typecheck_and_lower("fn id<T>(x: T) -> T { x } fn main() { let a: fn(i64) -> i64 = id; let b = id<string>; a(4); b(\"hew\"); }");
    assert!(output.diagnostics.is_empty(), "{:?}", output.diagnostics);
    let monos = &output.module.monomorphisations;
    assert_eq!(monos.len(), 2, "{monos:?}");
    let origin = monos[0].key.origin;
    assert!(monos
        .iter()
        .all(|mono| mono.key.origin == origin && mono.key.declaration.full_path() == "id"));
    for ty in [ResolvedTy::I64, ResolvedTy::String] {
        assert!(monos
            .iter()
            .any(|mono| mono.key.type_args == vec![ty.clone()]));
    }
    let main = output
        .module
        .items
        .iter()
        .find_map(|item| match item {
            HirItem::Function(function) if function.name == "main" => Some(function),
            _ => None,
        })
        .unwrap();
    for stmt in main.body.statements.iter().take(2) {
        let HirStmtKind::Let(_, Some(value)) = &stmt.kind else {
            panic!("expected binding");
        };
        assert!(
            matches!(value.kind, HirExprKind::BindingRef { resolved: hew_hir::ResolvedRef::Item(id), .. } if id == origin)
        );
        assert!(output.module.call_site_type_args.contains_key(&value.site));
        assert!(
            matches!(&value.ty, ResolvedTy::Function { capabilities, .. } if *capabilities == hew_types::CallableCapabilities::FUNCTION_ITEM)
        );
    }
}

#[test]
fn generic_function_values_close_under_substitution() {
    let output = typecheck_and_lower("fn id<T>(x: T) -> T { x } fn factory<T>() -> fn(T) -> T { id<T> } fn main() { let f = factory<i64>(); f(4); }");
    assert!(output.diagnostics.is_empty(), "{:?}", output.diagnostics);
    assert!(
        output
            .module
            .monomorphisations
            .iter()
            .any(|mono| mono.key.declaration.full_path() == "id"
                && mono.key.type_args == vec![ResolvedTy::I64]),
        "{:?}",
        output.module.monomorphisations
    );
}

#[test]
fn generic_function_value_requires_complete_checker_facts() {
    let parsed =
        hew_parser::parse("fn id<T>(x: T) -> T { x } fn main() { let f = id<i64>; f(4); }");
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let checked = Checker::new(ModuleRegistry::new(vec![])).check_program(&parsed.program);
    assert!(checked.errors.is_empty(), "{:?}", checked.errors);
    let key = checked
        .call_type_args
        .keys()
        .next()
        .expect("value instantiation")
        .clone();
    for defect in 0..4 {
        let mut checked = checked.clone();
        match defect {
            0 => {
                checked.call_type_args.remove(&key);
            }
            1 => {
                checked.direct_call_targets.remove(&key);
            }
            2 => {
                checked.expr_types.insert(key.clone(), hew_types::Ty::Error);
            }
            _ => {
                checked
                    .call_type_args
                    .insert(key.clone(), vec![hew_types::Ty::Error]);
            }
        }
        let output = lower_program(
            &parsed.program,
            &checked,
            &ResolutionCtx,
            hew_hir::TargetArch::host(),
        );
        assert!(
            output.into_result().is_err(),
            "accepted malformed value facts {defect}"
        );
    }
}

#[test]
fn mutable_callable_field_call_keeps_the_selected_projection() {
    let output = typecheck_and_lower("type Holder { next: fn[var, clone](i64) -> i64 } fn main() { let count: i64 = 0; var holder = Holder { next: capture(var count) |step: i64| { count = count + step; count } }; holder.next(1); }");
    assert!(output.diagnostics.is_empty(), "{:?}", output.diagnostics);
    let main = output
        .module
        .items
        .iter()
        .find_map(|item| match item {
            HirItem::Function(function) if function.name == "main" => Some(function),
            _ => None,
        })
        .unwrap();
    let HirStmtKind::Let(holder, _) = &main.body.statements[1].kind else {
        panic!("holder binding");
    };
    let HirStmtKind::Expr(call) = &main.body.statements[2].kind else {
        panic!("field call expression");
    };
    let HirExprKind::Call {
        target: hew_types::CallTarget::IndirectFunctionValue,
        callee,
        args,
    } = &call.kind
    else {
        panic!("direct field invocation: {:?}", call.kind);
    };
    let HirExprKind::FieldAccess { object, field } = &callee.kind else {
        panic!("selected field callee: {:?}", callee.kind);
    };
    assert_eq!(field, "next");
    assert!(
        matches!(object.kind, HirExprKind::BindingRef { resolved: hew_hir::ResolvedRef::Binding(id), .. } if id == holder.id)
    );
    assert_eq!(
        callee.ty,
        ResolvedTy::Function {
            capabilities: hew_types::CallableCapabilities {
                call: hew_types::CallableCallMode::Var,
                clone: true
            },
            params: vec![ResolvedTy::I64],
            ret: Box::new(ResolvedTy::I64),
        }
    );
    assert_eq!(call.ty, ResolvedTy::I64);
    assert_eq!(args.len(), 1);
    assert!(
        object.site.0 < args[0].site.0,
        "evaluate the receiver before the arguments"
    );
}

#[test]
fn zero_capture_literals_keep_concrete_closure_identity() {
    for source in [
        "fn main() { let f = || 7; f(); }",
        "fn main() { let f: fn[clone]() -> i64 = || 7; f(); }",
        "fn main() { let count = 90; let f = capture(var count) || { var count = 1; count += 1; count }; f(); }",
    ] {
        let output = typecheck_and_lower(source);
        assert!(output.diagnostics.is_empty(), "{:?}", output.diagnostics);
        let closure = find_closure_in_fn(&output, "main").expect("closure literal");
        assert!(matches!(&closure.ty, ResolvedTy::Closure { captures, .. } if captures.is_empty()), "{:?}", closure.ty);
        let HirExprKind::Closure { captures, .. } = &closure.kind else { unreachable!() };
        assert!(captures.is_empty());
    }
}
