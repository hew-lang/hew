use hew_hir::{lower_program, HirExpr, HirExprKind, HirItem, HirStmtKind, ResolutionCtx};
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
    lower_program(&parsed.program, &tc_output, &ResolutionCtx)
}

fn visit_expr<'a>(expr: &'a HirExpr, out: &mut Vec<&'a HirExpr>) {
    out.push(expr);
    match &expr.kind {
        HirExprKind::Call { callee, args } | HirExprKind::SpawnedCall { callee, args, .. } => {
            visit_expr(callee, out);
            for arg in args {
                visit_expr(arg, out);
            }
        }
        HirExprKind::Spawn { args, .. } => {
            for (_, arg) in args {
                visit_expr(arg, out);
            }
        }
        HirExprKind::ActorSend { receiver, args, .. }
        | HirExprKind::ActorAsk { receiver, args, .. }
        | HirExprKind::CallDynMethod { receiver, args, .. } => {
            visit_expr(receiver, out);
            for arg in args {
                visit_expr(arg, out);
            }
        }
        HirExprKind::Block(block)
        | HirExprKind::Scope { body: block }
        | HirExprKind::ForkBlock { body: block, .. } => visit_block(block, out),
        HirExprKind::Binary { left, right, .. } | HirExprKind::IdentityCompare { left, right } => {
            visit_expr(left, out);
            visit_expr(right, out);
        }
        HirExprKind::If {
            condition,
            then_expr,
            else_expr,
        } => {
            visit_expr(condition, out);
            visit_expr(then_expr, out);
            if let Some(else_expr) = else_expr {
                visit_expr(else_expr, out);
            }
        }
        HirExprKind::StructInit { fields, base, .. } => {
            for (_, field) in fields {
                visit_expr(field, out);
            }
            if let Some(base) = base {
                visit_expr(base, out);
            }
        }
        HirExprKind::FieldAccess { object, .. } => visit_expr(object, out),
        HirExprKind::ScopeDeadline { duration, body } => {
            visit_expr(duration, out);
            visit_block(body, out);
        }
        HirExprKind::SpawnLambdaActor { body, .. } | HirExprKind::Closure { body, .. } => {
            visit_expr(body, out);
        }
        HirExprKind::TupleIndex { tuple, .. } => visit_expr(tuple, out),
        HirExprKind::Index { container, index } => {
            visit_expr(container, out);
            visit_expr(index, out);
        }
        HirExprKind::Slice {
            container,
            start,
            end,
            ..
        } => {
            visit_expr(container, out);
            if let Some(start) = start {
                visit_expr(start, out);
            }
            if let Some(end) = end {
                visit_expr(end, out);
            }
        }
        HirExprKind::CoerceToDynTrait { value, .. } => visit_expr(value, out),
        HirExprKind::MachineEmit { fields, .. } => {
            for (_, field_val) in fields {
                visit_expr(field_val, out);
            }
        }
        HirExprKind::Select(_)
        | HirExprKind::AwaitTask { .. }
        | HirExprKind::BindingRef { .. }
        | HirExprKind::ContextReader { .. }
        | HirExprKind::Literal(_)
        | HirExprKind::Unsupported(_) => {}
    }
}

fn visit_block<'a>(block: &'a hew_hir::HirBlock, out: &mut Vec<&'a HirExpr>) {
    for stmt in &block.statements {
        match &stmt.kind {
            HirStmtKind::Let(_, Some(expr))
            | HirStmtKind::Expr(expr)
            | HirStmtKind::Return(Some(expr)) => visit_expr(expr, out),
            HirStmtKind::Assign { target, value } => {
                visit_expr(target, out);
                visit_expr(value, out);
            }
            HirStmtKind::Let(_, None) | HirStmtKind::Return(None) => {}
        }
    }
    if let Some(tail) = &block.tail {
        visit_expr(tail, out);
    }
}

#[test]
fn actor_spawn_send_and_ask_lower_to_explicit_hir_surface() {
    let output = lower_checked(
        r"
        actor Counter {
            let count: i64;

            receive fn increment(n: i64) {
            }

            receive fn print_total() -> i64 {
                return 1;
            }
        }

        fn main() -> i64 {
            let c = spawn Counter(count: 0);
            c.increment(10);
            await c.print_total();
            return 0;
        }
        ",
    );

    assert!(
        output.diagnostics.is_empty(),
        "HIR lowering should be diagnostic-free: {:?}",
        output.diagnostics
    );
    let main = output
        .module
        .items
        .iter()
        .find_map(|item| match item {
            HirItem::Function(func) if func.name == "main" => Some(func),
            _ => None,
        })
        .expect("main function should lower");
    let mut exprs = Vec::new();
    visit_block(&main.body, &mut exprs);

    assert!(
        exprs.iter().any(|expr| matches!(
            &expr.kind,
            HirExprKind::Spawn { actor_name, .. } if actor_name == "Counter"
        )),
        "spawn Counter site should lower to HirExprKind::Spawn: {:#?}",
        main.body
    );
    assert!(
        exprs.iter().any(|expr| matches!(
            &expr.kind,
            HirExprKind::ActorSend { method_id, .. } if method_id == "Counter::increment"
        )),
        "c.increment(10) should lower to HirExprKind::ActorSend: {:#?}",
        main.body
    );
    assert!(
        exprs.iter().any(|expr| matches!(
            &expr.kind,
            HirExprKind::ActorAsk { method_id, .. } if method_id == "Counter::print_total"
        )),
        "await c.print_total() should lower to HirExprKind::ActorAsk: {:#?}",
        main.body
    );
}
