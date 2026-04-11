//! AST traversal for call-site collection.
//!
//! Used by the LSP for call hierarchy (incoming/outgoing calls) and code lens.
//! Returns parser-level `Span` values so callers can convert to LSP ranges
//! without an extra allocation layer.

use hew_parser::ast::{Block, Expr, Item, Span, Stmt, StringPart, TraitItem, TypeBodyItem};
use hew_parser::ParseResult;

/// A single call site found in the AST.
#[derive(Debug, Clone)]
pub struct CallSite {
    /// The callee name: function name, method name, or `"<actor>.send"` for sends.
    pub name: String,
    /// Byte-offset span of the call expression (not just the callee identifier).
    pub span: Span,
}

/// Collect all call sites across every item body in the parse result.
#[must_use]
pub fn collect_calls_in_parse_result(parse_result: &ParseResult) -> Vec<CallSite> {
    let mut calls = Vec::new();
    for (item, _) in &parse_result.program.items {
        collect_calls_in_item(item, &mut calls);
    }
    calls
}

/// Collect all call sites reachable from a single top-level item.
pub fn collect_calls_in_item(item: &Item, calls: &mut Vec<CallSite>) {
    match item {
        Item::Function(f) => collect_calls_in_block(&f.body, calls),
        Item::Actor(a) => {
            if let Some(init) = &a.init {
                collect_calls_in_block(&init.body, calls);
            }
            if let Some(term) = &a.terminate {
                collect_calls_in_block(&term.body, calls);
            }
            for recv in &a.receive_fns {
                collect_calls_in_block(&recv.body, calls);
            }
            for method in &a.methods {
                collect_calls_in_block(&method.body, calls);
            }
        }
        Item::TypeDecl(td) => {
            for body_item in &td.body {
                if let TypeBodyItem::Method(m) = body_item {
                    collect_calls_in_block(&m.body, calls);
                }
            }
        }
        Item::Impl(i) => {
            for method in &i.methods {
                collect_calls_in_block(&method.body, calls);
            }
        }
        Item::Trait(t) => {
            for trait_item in &t.items {
                if let TraitItem::Method(m) = trait_item {
                    if let Some(body) = &m.body {
                        collect_calls_in_block(body, calls);
                    }
                }
            }
        }
        Item::Const(c) => collect_calls_in_expr(&c.value, calls),
        Item::Supervisor(s) => {
            for child in &s.children {
                for arg in &child.args {
                    collect_calls_in_expr(arg, calls);
                }
            }
        }
        Item::Import(_)
        | Item::ExternBlock(_)
        | Item::Wire(_)
        | Item::TypeAlias(_)
        | Item::Machine(_) => {}
    }
}

/// Collect all call sites reachable from `block`.
pub fn collect_calls_in_block(block: &Block, calls: &mut Vec<CallSite>) {
    for (stmt, _span) in &block.stmts {
        collect_calls_in_stmt(stmt, calls);
    }
}

fn collect_calls_in_stmt(stmt: &Stmt, calls: &mut Vec<CallSite>) {
    match stmt {
        Stmt::Let { value, .. } | Stmt::Var { value, .. } => {
            if let Some(spanned) = value {
                collect_calls_in_expr(spanned, calls);
            }
        }
        Stmt::Expression(spanned) | Stmt::Return(Some(spanned)) => {
            collect_calls_in_expr(spanned, calls);
        }
        Stmt::Defer(d) => {
            collect_calls_in_expr(d.as_ref(), calls);
        }
        Stmt::Assign { target, value, .. } => {
            collect_calls_in_expr(target, calls);
            collect_calls_in_expr(value, calls);
        }
        Stmt::For { iterable, body, .. } => {
            collect_calls_in_expr(iterable, calls);
            collect_calls_in_block(body, calls);
        }
        Stmt::While {
            condition, body, ..
        } => {
            collect_calls_in_expr(condition, calls);
            collect_calls_in_block(body, calls);
        }
        Stmt::WhileLet { expr, body, .. } => {
            collect_calls_in_expr(expr.as_ref(), calls);
            collect_calls_in_block(body, calls);
        }
        Stmt::If {
            condition,
            then_block,
            else_block,
            ..
        } => {
            collect_calls_in_expr(condition, calls);
            collect_calls_in_block(then_block, calls);
            if let Some(eb) = else_block {
                if let Some(if_box) = &eb.if_stmt {
                    let (if_stmt, _) = if_box.as_ref();
                    collect_calls_in_stmt(if_stmt, calls);
                }
                if let Some(b) = &eb.block {
                    collect_calls_in_block(b, calls);
                }
            }
        }
        Stmt::IfLet {
            expr,
            body,
            else_body,
            ..
        } => {
            collect_calls_in_expr(expr.as_ref(), calls);
            collect_calls_in_block(body, calls);
            if let Some(block) = else_body {
                collect_calls_in_block(block, calls);
            }
        }
        Stmt::Match { scrutinee, arms } => {
            collect_calls_in_expr(scrutinee, calls);
            for arm in arms {
                if let Some(g) = &arm.guard {
                    collect_calls_in_expr(g, calls);
                }
                collect_calls_in_expr(&arm.body, calls);
            }
        }
        Stmt::Loop { body, .. } => collect_calls_in_block(body, calls),
        Stmt::Return(None) | Stmt::Break { .. } | Stmt::Continue { .. } => {}
    }
}

#[expect(
    clippy::too_many_lines,
    reason = "exhaustive match over all Expr variants is clearest as one function"
)]
fn collect_calls_in_expr(spanned: &(Expr, Span), calls: &mut Vec<CallSite>) {
    let (expr, expr_span) = spanned;
    match expr {
        Expr::Call { function, args, .. } => {
            let (func, _) = function.as_ref();
            if let Expr::Identifier(name) = func {
                calls.push(CallSite {
                    name: name.clone(),
                    span: expr_span.clone(),
                });
            }
            collect_calls_in_expr(function.as_ref(), calls);
            for arg in args {
                collect_calls_in_expr(arg.expr(), calls);
            }
        }
        Expr::MethodCall {
            receiver,
            method,
            args,
            ..
        } => {
            calls.push(CallSite {
                name: method.clone(),
                span: expr_span.clone(),
            });
            collect_calls_in_expr(receiver.as_ref(), calls);
            for arg in args {
                collect_calls_in_expr(arg.expr(), calls);
            }
        }
        Expr::IfLet {
            expr,
            body,
            else_body,
            ..
        } => {
            collect_calls_in_expr(expr.as_ref(), calls);
            collect_calls_in_block(body, calls);
            if let Some(block) = else_body {
                collect_calls_in_block(block, calls);
            }
        }
        Expr::Send { target, message } => {
            // Actor message sends are treated as call edges.
            let send_name = match &target.as_ref().0 {
                Expr::Identifier(name) => format!("{name}.send"),
                _ => "send".to_string(),
            };
            calls.push(CallSite {
                name: send_name,
                span: expr_span.clone(),
            });
            collect_calls_in_expr(target.as_ref(), calls);
            collect_calls_in_expr(message.as_ref(), calls);
        }
        Expr::Binary { left, right, .. } => {
            collect_calls_in_expr(left.as_ref(), calls);
            collect_calls_in_expr(right.as_ref(), calls);
        }
        Expr::Unary { operand, .. } => {
            collect_calls_in_expr(operand.as_ref(), calls);
        }
        Expr::Await(a) => {
            collect_calls_in_expr(a.as_ref(), calls);
        }
        Expr::PostfixTry(p) => {
            collect_calls_in_expr(p.as_ref(), calls);
        }
        Expr::Cast { expr, .. } => {
            collect_calls_in_expr(expr.as_ref(), calls);
        }
        Expr::Yield(Some(y)) => {
            collect_calls_in_expr(y.as_ref(), calls);
        }
        Expr::If {
            condition,
            then_block,
            else_block,
            ..
        } => {
            collect_calls_in_expr(condition.as_ref(), calls);
            collect_calls_in_expr(then_block.as_ref(), calls);
            if let Some(eb) = else_block {
                collect_calls_in_expr(eb.as_ref(), calls);
            }
        }
        Expr::Match { scrutinee, arms } => {
            collect_calls_in_expr(scrutinee.as_ref(), calls);
            for arm in arms {
                if let Some(g) = &arm.guard {
                    collect_calls_in_expr(g, calls);
                }
                collect_calls_in_expr(&arm.body, calls);
            }
        }
        Expr::Block(b) | Expr::Unsafe(b) => collect_calls_in_block(b, calls),
        Expr::Index { object, index, .. } => {
            collect_calls_in_expr(object.as_ref(), calls);
            collect_calls_in_expr(index.as_ref(), calls);
        }
        Expr::FieldAccess { object, .. } => {
            collect_calls_in_expr(object.as_ref(), calls);
        }
        Expr::Lambda { body, .. } | Expr::SpawnLambdaActor { body, .. } => {
            collect_calls_in_expr(body.as_ref(), calls);
        }
        Expr::ArrayRepeat { value, count } => {
            collect_calls_in_expr(value.as_ref(), calls);
            collect_calls_in_expr(count.as_ref(), calls);
        }
        Expr::MapLiteral { entries } => {
            for (k, v) in entries {
                collect_calls_in_expr(k, calls);
                collect_calls_in_expr(v, calls);
            }
        }
        Expr::Tuple(exprs) | Expr::Array(exprs) | Expr::Join(exprs) => {
            for e in exprs {
                collect_calls_in_expr(e, calls);
            }
        }
        Expr::Range { start, end, .. } => {
            if let Some(s) = start {
                collect_calls_in_expr(s.as_ref(), calls);
            }
            if let Some(e) = end {
                collect_calls_in_expr(e.as_ref(), calls);
            }
        }
        Expr::InterpolatedString(parts) => {
            for part in parts {
                if let StringPart::Expr(e) = part {
                    collect_calls_in_expr(e, calls);
                }
            }
        }
        Expr::Scope { body, .. } | Expr::ScopeLaunch(body) | Expr::ScopeSpawn(body) => {
            collect_calls_in_block(body, calls);
        }
        Expr::Select { arms, timeout, .. } => {
            for arm in arms {
                collect_calls_in_expr(&arm.body, calls);
            }
            if let Some(tc) = timeout {
                collect_calls_in_expr(tc.duration.as_ref(), calls);
                collect_calls_in_expr(tc.body.as_ref(), calls);
            }
        }
        Expr::StructInit { fields, .. } => {
            for (_, v) in fields {
                collect_calls_in_expr(v, calls);
            }
        }
        Expr::Spawn { target, args } => {
            collect_calls_in_expr(target.as_ref(), calls);
            for (_, a) in args {
                collect_calls_in_expr(a, calls);
            }
        }
        Expr::Timeout { expr, duration } => {
            collect_calls_in_expr(expr.as_ref(), calls);
            collect_calls_in_expr(duration.as_ref(), calls);
        }
        Expr::Literal(_)
        | Expr::Identifier(_)
        | Expr::Cooperate
        | Expr::ScopeCancel
        | Expr::This
        | Expr::RegexLiteral(_)
        | Expr::ByteStringLiteral(_)
        | Expr::ByteArrayLiteral(_)
        | Expr::Yield(None) => {}
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse(source: &str) -> hew_parser::ParseResult {
        hew_parser::parse(source)
    }

    fn names(calls: &[CallSite]) -> Vec<&str> {
        calls.iter().map(|c| c.name.as_str()).collect()
    }

    #[test]
    fn direct_function_call() {
        let source = "fn f() { foo(); }";
        let pr = parse(source);
        let calls = collect_calls_in_parse_result(&pr);
        assert!(names(&calls).contains(&"foo"), "should find call to foo");
    }

    #[test]
    fn method_call_found() {
        let source = "fn f() { x.bar(); }";
        let pr = parse(source);
        let calls = collect_calls_in_parse_result(&pr);
        assert!(
            names(&calls).contains(&"bar"),
            "should find method call bar"
        );
    }

    #[test]
    fn actor_receive_fn_calls_found() {
        let source = "actor A { receive fn on_msg() { helper(); } }";
        let pr = parse(source);
        let calls = collect_calls_in_parse_result(&pr);
        assert!(
            names(&calls).contains(&"helper"),
            "should find calls inside actor receive fn"
        );
    }

    #[test]
    fn impl_method_calls_found() {
        let source = "impl Foo { fn bar() { baz(); } }";
        let pr = parse(source);
        let calls = collect_calls_in_parse_result(&pr);
        assert!(
            names(&calls).contains(&"baz"),
            "should find calls inside impl method"
        );
    }

    #[test]
    fn nested_call_in_condition_found() {
        let source = "fn f() { if check() { work(); } }";
        let pr = parse(source);
        let calls = collect_calls_in_parse_result(&pr);
        let ns = names(&calls);
        assert!(ns.contains(&"check"), "should find call in if condition");
        assert!(ns.contains(&"work"), "should find call in if body");
    }

    #[test]
    fn no_false_positives_for_identifiers() {
        let source = "fn f() { let x = 1; let y = x; }";
        let pr = parse(source);
        let calls = collect_calls_in_parse_result(&pr);
        // Identifier references are not calls
        assert!(
            !names(&calls).contains(&"x"),
            "plain identifier usage must not appear as a call"
        );
    }

    #[test]
    fn collect_calls_in_item_function() {
        let source = "fn f() { a(); b(); }";
        let pr = parse(source);
        let mut calls = Vec::new();
        collect_calls_in_item(&pr.program.items[0].0, &mut calls);
        let ns = names(&calls);
        assert!(ns.contains(&"a") && ns.contains(&"b"));
    }

    #[test]
    fn span_covers_call_expression() {
        let source = "fn f() { foo(); }";
        let pr = parse(source);
        let calls = collect_calls_in_parse_result(&pr);
        let foo_call = calls.iter().find(|c| c.name == "foo").expect("foo call");
        // Span should be non-empty and within source bounds
        assert!(foo_call.span.start < foo_call.span.end);
        assert!(foo_call.span.end <= source.len());
    }
}
