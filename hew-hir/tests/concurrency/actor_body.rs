use hew_hir::{
    dump_hir, lower_program, verify_hir, HirActorDecl, HirExpr, HirExprKind, HirItem, HirStmtKind,
    ResolutionCtx, ResolvedRef,
};
use hew_types::{module_registry::ModuleRegistry, Checker};

fn lower_checked(source: &str) -> hew_hir::LowerOutput {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    assert!(
        tc_output.errors.is_empty(),
        "type errors: {:?}",
        tc_output.errors
    );
    lower_program(
        &parsed.program,
        &tc_output,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    )
}

fn find_actor<'a>(output: &'a hew_hir::LowerOutput, name: &str) -> &'a HirActorDecl {
    output
        .module
        .items
        .iter()
        .find_map(|item| match item {
            HirItem::Actor(actor) if actor.name == name => Some(actor),
            _ => None,
        })
        .unwrap_or_else(|| panic!("expected actor `{name}` in lowered module"))
}

fn assert_binding_ref(expr: &HirExpr, expected: hew_hir::BindingId, name: &str) {
    let HirExprKind::BindingRef {
        resolved,
        name: got,
    } = &expr.kind
    else {
        panic!("expected binding ref to `{name}`, got {expr:?}");
    };
    assert_eq!(got, name);
    assert_eq!(*resolved, ResolvedRef::Binding(expected));
}

/// A `receive gen fn` body is wrapped in a `GenBlock` tail (mirroring a
/// standalone `gen fn`): the handler's outer block carries no statements, and
/// the real body — including the `let seen = <param>;` referencing the handler
/// param — lives inside the `GenBlock` body. This routes the handler through
/// the shared `Terminator::Yield` state-machine path in MIR. Asserts the
/// handler is a generator and the first `GenBlock`-body statement binds
/// `param_name`.
fn assert_generator_handler_param_bound(handler: &hew_hir::HirActorReceiveFn, param_name: &str) {
    assert!(handler.is_generator);
    assert!(
        handler.body.statements.is_empty(),
        "generator handler outer block should be a thin GenBlock wrapper"
    );
    let tail = handler
        .body
        .tail
        .as_ref()
        .expect("generator handler body must have a GenBlock tail");
    let HirExprKind::GenBlock { body, .. } = &tail.kind else {
        panic!("expected GenBlock tail, got {:?}", tail.kind);
    };
    let HirStmtKind::Let(_, Some(value)) = &body.statements[0].kind else {
        panic!("generator body should contain a let initialized from its param");
    };
    assert_binding_ref(value, handler.params[0].id, param_name);
}

#[test]
fn actor_body_params_are_bound_and_reachable() {
    let output = lower_checked(
        r"
actor Counter {
    let count: i32;

    init(start: i32) {
        let seed: i32 = start;
    }

    receive fn inc(n: i32) -> i32 {
        return n;
    }

    receive fn dec(n: i32) -> i32 {
        return n;
    }

    receive gen fn stream(limit: i32) -> i32 {
        let seen: i32 = limit;
    }

    fn echo(value: i32) -> i32 {
        return value;
    }

    #[on(start)]
    fn boot() {
        let ready = true;
    }
}

fn main() {}
",
    );
    assert!(
        output.diagnostics.is_empty(),
        "unexpected HIR diagnostics: {:?}",
        output.diagnostics
    );
    let verify = verify_hir(&output.module);
    assert!(verify.is_empty(), "verify diagnostics: {verify:?}");

    let actor = find_actor(&output, "Counter");
    let init = actor.init.as_ref().expect("init body should lower");
    assert_eq!(init.params.len(), 1);
    assert_eq!(init.body.statements.len(), 1);
    let HirStmtKind::Let(_, Some(init_value)) = &init.body.statements[0].kind else {
        panic!("init should contain a let initialized from its param");
    };
    assert_binding_ref(init_value, init.params[0].id, "start");

    let inc = actor
        .receive_handlers
        .iter()
        .find(|receive| receive.name == "inc")
        .expect("inc receive handler should lower");
    let dec = actor
        .receive_handlers
        .iter()
        .find(|receive| receive.name == "dec")
        .expect("dec receive handler should lower");
    assert_ne!(
        inc.params[0].id, dec.params[0].id,
        "same-named receive params must be rebound per handler"
    );
    let HirStmtKind::Return(Some(inc_value)) = &inc.body.statements[0].kind else {
        panic!("inc should return its param");
    };
    assert_binding_ref(inc_value, inc.params[0].id, "n");
    let HirStmtKind::Return(Some(dec_value)) = &dec.body.statements[0].kind else {
        panic!("dec should return its param");
    };
    assert_binding_ref(dec_value, dec.params[0].id, "n");

    let stream = actor
        .receive_handlers
        .iter()
        .find(|receive| receive.name == "stream")
        .expect("generator receive handler should lower");
    assert_generator_handler_param_bound(stream, "limit");

    let echo = actor
        .methods
        .iter()
        .find(|method| method.name == "echo")
        .expect("method should lower");
    let HirStmtKind::Return(Some(echo_value)) = &echo.body.statements[0].kind else {
        panic!("method should return its param");
    };
    assert_binding_ref(echo_value, echo.params[0].id, "value");

    let boot = actor
        .lifecycle_hooks
        .iter()
        .find(|hook| hook.name == "boot")
        .expect("lifecycle hook should lower");
    assert_eq!(boot.body.statements.len(), 1);

    let dump = dump_hir(&output.module);
    assert!(
        dump.contains(
            "receive stream params=1 -> i32 state_guard=Exclusive every_ns=None is_generator=true"
        ),
        "dump must surface generator receive body metadata:\n{dump}"
    );
    assert!(
        dump.contains("body_scope="),
        "dump must surface actor body scopes:\n{dump}"
    );
}
