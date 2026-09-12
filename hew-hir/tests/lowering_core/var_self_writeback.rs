use crate::support;

use hew_hir::{
    dump_hir, HirExpr, HirExprKind, HirFn, HirItem, HirStmtKind, IntentKind, ResolvedRef,
};
use hew_types::ResolvedTy;

fn assert_receiver_return(function: &HirFn, expression: &HirExpr) {
    let receiver = &function.params[0];
    assert_eq!(function.var_self_receiver, Some(receiver.id));
    assert!(receiver.mutable);
    assert!(
        !receiver.is_consume,
        "var self must not rewrite source consume metadata"
    );
    let HirExprKind::TupleLiteral { elements } = &expression.kind else {
        panic!("expected the explicit result/receiver return: {expression:#?}")
    };
    assert_eq!(elements.len(), 2);
    let ResolvedTy::Tuple(return_fields) = &function.return_ty else {
        panic!("expected result/receiver return type")
    };
    assert_eq!(elements[0].ty, return_fields[0]);
    assert_eq!(elements[1].ty, receiver.ty);
    assert_eq!(elements[1].intent, IntentKind::Consume);
    assert!(matches!(&elements[1].kind,
        HirExprKind::BindingRef { resolved: ResolvedRef::Binding(id), .. } if *id == receiver.id));
}

const COUNTDOWN_SOURCE: &str = r"
trait Stepper {
    type Item;
    fn next(var self) -> Option<Self.Item>;
}

type Countdown { n: i64, }

impl Stepper for Countdown {
    type Item = i64;

    fn next(var self) -> Option<i64> {
        if self.n <= 0 {
            None
        } else {
            let cur = self.n;
            self.n = self.n - 1;
            Some(cur)
        }
    }
}

fn step() -> Option<i64> {
    var cd = Countdown { n: 1 };
    cd.next()
}
fn main() {}
";

#[test]
fn concrete_var_self_next_lowers_to_writeback_call() {
    let output = support::checker_pipeline::lower_through_checker(COUNTDOWN_SOURCE);
    assert!(
        output.diagnostics.is_empty(),
        "unexpected HIR diagnostics: {:?}",
        output.diagnostics
    );
    let dump = dump_hir(&output.module);
    assert!(
        dump.contains("var-self-call Countdown::<impl Stepper for Countdown>::next"),
        "concrete next() must retain its full source impl identity through the write-back node; dump:\n{dump}"
    );
    assert!(
        !dump.contains("var-self-call Countdown::next"),
        "write-back dispatch must not regress to the ambiguous leaf method identity; dump:\n{dump}"
    );
    assert!(
        !dump.contains("resolved-impl-call"),
        "user impl next() must not route through ResolvedImplCall; dump:\n{dump}"
    );
    let method = output
        .module
        .items
        .iter()
        .find_map(|item| match item {
            HirItem::Function(function) if function.var_self_receiver.is_some() => Some(function),
            _ => None,
        })
        .expect("var self body must carry its receiver binding");
    assert_receiver_return(method, method.body.tail.as_ref().unwrap());
    assert!(output.module.items.iter().any(|item| matches!(item,
        HirItem::Function(function) if function.name == "step" && function.var_self_receiver.is_none())));
}

#[test]
fn generic_var_self_retains_receiver_on_explicit_and_fallthrough_returns() {
    let source = r#"
trait Advance {
    fn advance(var self, early: bool) -> i64;
}

type Holder<T> { payload: T }
impl<T> Advance for Holder<T> {
    fn advance(var self, early: bool) -> i64 {
        if early { return 1; }
        2
    }
}
fn main() -> i64 {
    var holder = Holder { payload: "owned" };
    holder.advance(false)
}
fn pair(value: Holder<string>) -> (i64, Holder<string>) { (0, value) }
"#;
    let output = support::checker_pipeline::lower_through_checker(source);
    assert!(output.diagnostics.is_empty(), "{:?}", output.diagnostics);
    let method = output
        .module
        .items
        .iter()
        .find_map(|item| match item {
            HirItem::Function(function) if function.var_self_receiver.is_some() => Some(function),
            _ => None,
        })
        .expect("generic var self method");
    assert_eq!(method.type_params, ["T"]);
    assert!(matches!(&method.return_ty, ResolvedTy::Tuple(fields)
        if fields[1] == method.params[0].ty));
    assert_receiver_return(method, method.body.tail.as_ref().unwrap());
    assert!(output.module.items.iter().any(|item| matches!(item,
        HirItem::Function(function) if function.name == "pair" && function.var_self_receiver.is_none())));
    let HirStmtKind::Expr(branch) = &method.body.statements[0].kind else {
        panic!("expected conditional early return")
    };
    let HirExprKind::If { then_expr, .. } = &branch.kind else {
        panic!("expected conditional early return")
    };
    let HirExprKind::Block(block) = &then_expr.kind else {
        panic!("expected early-return block")
    };
    let HirStmtKind::Return(Some(value)) = &block.statements[0].kind else {
        panic!("expected explicit return")
    };
    assert_receiver_return(method, value);
}

#[test]
fn var_self_verifier_rejects_stale_receiver_bindings_and_types() {
    let output = support::checker_pipeline::lower_through_checker(COUNTDOWN_SOURCE);
    assert!(output.diagnostics.is_empty(), "{:?}", output.diagnostics);
    assert!(hew_hir::verify_hir(&output.module).is_empty());
    for mutation in 0..4 {
        let mut module = output.module.clone();
        let method = module
            .items
            .iter_mut()
            .find_map(|item| match item {
                HirItem::Function(function) if function.var_self_receiver.is_some() => {
                    Some(function)
                }
                _ => None,
            })
            .unwrap();
        match mutation {
            0 => method.var_self_receiver = Some(hew_hir::BindingId(u32::MAX)),
            1 => method.params[0].mutable = false,
            2 => method.return_ty = ResolvedTy::Tuple(vec![ResolvedTy::I64, ResolvedTy::String]),
            3 => method.return_ty = ResolvedTy::I64,
            _ => unreachable!(),
        }
        assert!(
            hew_hir::verify_hir(&module)
                .iter()
                .any(|diagnostic| matches!(
            &diagnostic.kind, hew_hir::HirDiagnosticKind::CheckerBoundaryViolation { name, .. }
                if name == "var self receiver")),
            "mutation {mutation} must be rejected"
        );
    }
}

#[test]
fn projected_var_self_receiver_preserves_the_writeback_place() {
    let source = COUNTDOWN_SOURCE
        .replace("fn step()", "type Holder { counter: Countdown }\nfn step()")
        .replace(
            "var cd = Countdown { n: 1 };",
            "var owner = Holder { counter: Countdown { n: 1 } };",
        )
        .replace("cd.next()", "owner.counter.next()");
    let output = support::checker_pipeline::lower_through_checker(&source);
    assert!(output.diagnostics.is_empty(), "{:?}", output.diagnostics);
    let entry = output
        .module
        .items
        .iter()
        .find_map(|item| match item {
            HirItem::Function(function) if function.name == "step" => Some(function),
            _ => None,
        })
        .unwrap();
    let HirExprKind::VarSelfMethodCall { receiver, .. } = &entry.body.tail.as_ref().unwrap().kind
    else {
        panic!("expected receiver writeback call")
    };
    assert_eq!(receiver.intent, IntentKind::Consume);
    let HirExprKind::FieldAccess { object, field, .. } = &receiver.kind else {
        panic!("expected receiver field place: {receiver:#?}")
    };
    assert_eq!(field, "counter");
    assert!(matches!(
        object.kind,
        HirExprKind::BindingRef {
            resolved: ResolvedRef::Binding(_),
            ..
        }
    ));
    assert!(hew_hir::verify_hir(&output.module).is_empty());
}

#[test]
fn var_self_verifier_rejects_an_endpoint_without_a_declaration_target() {
    let mut output = support::checker_pipeline::lower_through_checker(COUNTDOWN_SOURCE);
    assert!(output.diagnostics.is_empty(), "{:?}", output.diagnostics);
    let entry = output
        .module
        .items
        .iter_mut()
        .find_map(|item| match item {
            HirItem::Function(function) if function.name == "step" => Some(function),
            _ => None,
        })
        .unwrap();
    let HirExprKind::VarSelfMethodCall { call_target, .. } =
        &mut entry.body.tail.as_mut().unwrap().kind
    else {
        panic!("expected structured var self call")
    };
    *call_target = hew_types::CallTarget::Builtin {
        endpoint: "Countdown::next".into(),
    };
    assert!(hew_hir::verify_hir(&output.module)
        .iter()
        .any(|diagnostic| matches!(
        &diagnostic.kind, hew_hir::HirDiagnosticKind::CheckerBoundaryViolation { name, .. }
            if name == "var self call target")));
}

#[test]
fn generic_unit_var_self_wraps_a_bare_return_in_its_tail_once() {
    let output = support::checker_pipeline::lower_through_checker(
        r#"
trait Touch { fn touch(var self, early: bool); }
type Holder<T> { payload: T }
impl<T> Touch for Holder<T> {
    fn touch(var self, early: bool) { if early { return; } }
}
fn main() -> i64 {
    var owned = Holder { payload: "kept" };
    owned.touch(true);
    0
}
"#,
    );
    assert!(output.diagnostics.is_empty(), "{:?}", output.diagnostics);
    let method = output
        .module
        .items
        .iter()
        .find_map(|item| match item {
            HirItem::Function(function) if function.var_self_receiver.is_some() => Some(function),
            _ => None,
        })
        .unwrap();
    let tail = method.body.tail.as_ref().unwrap();
    assert_receiver_return(method, tail);
    let HirExprKind::TupleLiteral { elements } = &tail.kind else {
        unreachable!()
    };
    let HirExprKind::If { then_expr, .. } = &elements[0].kind else {
        panic!("expected conditional in the source tail: {tail:#?}")
    };
    let HirExprKind::Block(block) = &then_expr.kind else {
        unreachable!()
    };
    let HirStmtKind::Return(Some(value)) = &block.statements[0].kind else {
        unreachable!()
    };
    assert_receiver_return(method, value);
}
