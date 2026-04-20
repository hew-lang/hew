//! AST traversal for call-site collection.
//!
//! Used by the LSP for call hierarchy (incoming/outgoing calls) and code lens.
//! Returns parser-level `Span` values so callers can convert to LSP ranges
//! without an extra allocation layer.

use crate::ast_visit::{self, AstVisitor};
use hew_parser::ast::{Block, Expr, Item, Span, Stmt, StringPart};
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
    let mut visitor = CallCollector::default();
    ast_visit::walk_parse_result(None, parse_result, &mut visitor);
    visitor.calls
}

/// Collect all call sites reachable from a single top-level item.
pub fn collect_calls_in_item(item: &Item, calls: &mut Vec<CallSite>) {
    let mut visitor = CallCollector::default();
    let parse_result = ParseResult {
        program: hew_parser::ast::Program {
            items: vec![(item.clone(), 0..0)],
            module_doc: None,
            module_graph: None,
        },
        errors: Vec::new(),
    };
    ast_visit::walk_parse_result(None, &parse_result, &mut visitor);
    calls.extend(visitor.calls);
}

/// Collect call sites from the specific named body within an item.
///
/// For single-body items (`Function`, `Machine`, `Supervisor`, `Const`) the
/// `body_name` must match the item's own name; if it does the whole item body
/// is walked via [`collect_calls_in_item`].
///
/// For multi-body items (`Actor`, `Impl`, `TypeDecl`, `Trait`) **only** the
/// sub-body whose name equals `body_name` is walked.  This prevents sibling
/// methods or receive-fns from appearing as spurious outgoing calls when the
/// LSP call-hierarchy queries a single method.
///
/// Returns `true` if a matching body was found (regardless of whether it
/// produced any calls).
pub fn collect_calls_in_named_body(
    item: &Item,
    body_name: &str,
    calls: &mut Vec<CallSite>,
) -> bool {
    let mut visitor = CallCollector::default();
    let found = ast_visit::walk_named_body(None, item, body_name, &mut visitor);
    calls.extend(visitor.calls);
    found
}

#[derive(Default)]
struct CallCollector {
    calls: Vec<CallSite>,
}

impl<'ast> AstVisitor<'ast> for CallCollector {
    fn visit_expr(
        &mut self,
        expr: &'ast Expr,
        span: &'ast Span,
        _ctx: crate::ast_visit::VisitContext<'ast>,
    ) {
        match expr {
            Expr::Call { function, .. } => {
                if let Expr::Identifier(name) = &function.0 {
                    self.calls.push(CallSite {
                        name: name.clone(),
                        span: span.clone(),
                    });
                }
            }
            Expr::MethodCall { method, .. } => {
                self.calls.push(CallSite {
                    name: method.clone(),
                    span: span.clone(),
                });
            }
            Expr::Send { target, .. } => {
                let send_name = match &target.0 {
                    Expr::Identifier(name) => format!("{name}.send"),
                    _ => "send".to_string(),
                };
                self.calls.push(CallSite {
                    name: send_name,
                    span: span.clone(),
                });
            }
            _ => {}
        }
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

    // ── collect_calls_in_named_body regression tests ─────────────────────────

    #[test]
    fn named_body_function_match() {
        let source = "fn f() { a(); }";
        let pr = parse(source);
        let mut calls = Vec::new();
        let found = collect_calls_in_named_body(&pr.program.items[0].0, "f", &mut calls);
        assert!(found, "should report a match for function f");
        assert!(names(&calls).contains(&"a"));
    }

    #[test]
    fn named_body_function_no_match() {
        let source = "fn f() { a(); }";
        let pr = parse(source);
        let mut calls = Vec::new();
        let found = collect_calls_in_named_body(&pr.program.items[0].0, "other", &mut calls);
        assert!(!found, "should not match for a different name");
        assert!(calls.is_empty());
    }

    /// Regression: actor `receive_fn` must not bleed into sibling `receive_fn` bodies.
    #[test]
    fn named_body_actor_receive_fn_no_sibling_bleed() {
        let source =
            "actor A { receive fn on_msg() { helper_a(); } receive fn on_other() { helper_b(); } }";
        let pr = parse(source);
        let mut calls = Vec::new();
        let found = collect_calls_in_named_body(&pr.program.items[0].0, "on_msg", &mut calls);
        assert!(found, "on_msg should be found");
        let ns = names(&calls);
        assert!(
            ns.contains(&"helper_a"),
            "on_msg body should contain helper_a"
        );
        assert!(
            !ns.contains(&"helper_b"),
            "sibling on_other's helper_b must NOT appear in on_msg outgoing calls"
        );
    }

    /// Regression: actor method must not bleed into sibling method bodies.
    #[test]
    fn named_body_actor_method_no_sibling_bleed() {
        let source = "actor A { fn do_work() { work_fn(); } fn idle() { idle_fn(); } }";
        let pr = parse(source);
        let mut calls = Vec::new();
        let found = collect_calls_in_named_body(&pr.program.items[0].0, "do_work", &mut calls);
        assert!(found, "do_work should be found");
        let ns = names(&calls);
        assert!(ns.contains(&"work_fn"), "do_work should call work_fn");
        assert!(
            !ns.contains(&"idle_fn"),
            "sibling idle's idle_fn must NOT appear in do_work outgoing calls"
        );
    }

    /// Regression: impl method must not bleed into sibling methods.
    #[test]
    fn named_body_impl_method_no_sibling_bleed() {
        let source = "impl Foo { fn alpha() { foo_a(); } fn beta() { foo_b(); } }";
        let pr = parse(source);
        let mut calls = Vec::new();
        let found = collect_calls_in_named_body(&pr.program.items[0].0, "alpha", &mut calls);
        assert!(found, "alpha should be found");
        let ns = names(&calls);
        assert!(ns.contains(&"foo_a"), "alpha should call foo_a");
        assert!(
            !ns.contains(&"foo_b"),
            "sibling beta's foo_b must NOT appear in alpha outgoing calls"
        );
    }

    /// Regression: `TypeDecl` method must not bleed into sibling methods.
    #[test]
    fn named_body_typedecl_method_no_sibling_bleed() {
        let source = "type Foo { fn alpha() { td_a(); } fn beta() { td_b(); } }";
        let pr = parse(source);
        let mut calls = Vec::new();
        let found = collect_calls_in_named_body(&pr.program.items[0].0, "alpha", &mut calls);
        assert!(found, "alpha should be found in TypeDecl");
        let ns = names(&calls);
        assert!(ns.contains(&"td_a"), "alpha should call td_a");
        assert!(
            !ns.contains(&"td_b"),
            "sibling beta's td_b must NOT appear in alpha outgoing calls"
        );
    }
}
