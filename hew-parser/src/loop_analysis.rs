/// AST-level loop analysis utilities.
///
/// Provides break-detection over the raw AST so both the type checker
/// (`hew-types`) and HIR lowering (`hew-hir`) can share the logic without
/// duplicating it.
use crate::ast::{Block, Expr, Stmt};

/// Returns `true` if the `body` of a `loop { … }` contains a `break`
/// statement that would exit THAT loop — i.e., a break that targets the
/// loop identified by `self_label`.
///
/// This is the public entry point; it calls the internal depth-aware helpers.
///
/// # Semantics
/// * An unlabeled `break` at depth 0 (direct body of the loop) exits this loop.
/// * A labeled `break @L` at any depth exits the loop whose label is `L`.
/// * Inner loops (`loop`, `for`, `while`, `while let`) increase the depth so
///   an unlabeled break inside them does NOT count as exiting the outer loop.
/// * Lambda bodies (`|…| …`, `spawn_lambda_actor`, generator blocks) are new
///   scope boundaries; `break` inside them cannot escape to an outer loop.
///
/// # Completeness guarantee
/// The walker is exhaustive over every [`Stmt`] and [`Expr`] variant.  Unknown
/// or unhandled expression forms default to **conservative** (`true`), so this
/// function only returns `false` when it can **prove** no reachable break
/// escapes to the outer loop.  The inverse (`_ => false`) is the unsound
/// direction: it would cause an infinite loop to be mis-typed as `Never`.
#[must_use]
pub fn loop_body_has_break(body: &Block, self_label: Option<&str>) -> bool {
    ast_block_has_break(body, self_label, 0)
}

fn ast_block_has_break(block: &Block, self_label: Option<&str>, depth: usize) -> bool {
    for (stmt, _) in &block.stmts {
        if ast_stmt_has_break(stmt, self_label, depth) {
            return true;
        }
    }
    if let Some(trailing) = &block.trailing_expr {
        return ast_expr_has_break(&trailing.0, self_label, depth);
    }
    false
}

#[allow(
    clippy::too_many_lines,
    reason = "exhaustive walker over every Stmt variant; splitting would hide completeness guarantees"
)]
fn ast_stmt_has_break(stmt: &Stmt, self_label: Option<&str>, depth: usize) -> bool {
    match stmt {
        Stmt::Break { label, value } => {
            // Does THIS break target the loop under analysis?
            let targets_self = if depth == 0 {
                // An unlabeled break, or a break with our own label, exits this loop.
                label.is_none() || label.as_deref() == self_label
            } else {
                // Inside an inner loop, only a break that explicitly names OUR
                // label escapes to the outer loop.
                label.as_deref() == self_label && self_label.is_some()
            };
            // The break VALUE (`break (… break @L …)`) is evaluated in the same
            // scope before the break fires, so a nested break targeting our loop
            // inside it still counts.
            targets_self
                || value
                    .as_ref()
                    .is_some_and(|v| ast_expr_has_break(&v.0, self_label, depth))
        }
        // Inner loop: increase depth so an unlabeled break inside the inner
        // loop's body does not count as exiting the outer loop.
        Stmt::Loop { body, .. } => ast_block_has_break(body, self_label, depth + 1),
        Stmt::For { iterable, body, .. } => {
            // A loop HEADER expression (`for` iterable, `while`/`while let`
            // condition) is evaluated in the ENCLOSING scope, not inside the
            // loop: Hew reports "break used outside of a loop" for a bare break
            // in a header with no outer loop, and binds it to the OUTER loop when
            // one exists. So a break in a block-expr header targets the enclosing
            // loop — analyse the header at the CURRENT depth; only the body gets
            // `depth + 1`.
            ast_expr_has_break(&iterable.0, self_label, depth)
                || ast_block_has_break(body, self_label, depth + 1)
        }
        Stmt::While {
            condition, body, ..
        } => {
            // Header (condition) at current depth, body at `depth + 1` — see the
            // `For` arm for the header-scoping rationale.
            ast_expr_has_break(&condition.0, self_label, depth)
                || ast_block_has_break(body, self_label, depth + 1)
        }
        Stmt::WhileLet { expr, body, .. } => {
            // Header (scrutinee) at current depth, body at `depth + 1` — see the
            // `For` arm for the header-scoping rationale.
            ast_expr_has_break(&expr.0, self_label, depth)
                || ast_block_has_break(body, self_label, depth + 1)
        }
        Stmt::If {
            then_block,
            else_block,
            condition,
        } => {
            // The condition is evaluated in the enclosing scope (an `if` is not a
            // loop), so a break inside a block-expr condition targets the outer
            // loop — check it at the current depth, before the branches.
            if ast_expr_has_break(&condition.0, self_label, depth) {
                return true;
            }
            if ast_block_has_break(then_block, self_label, depth) {
                return true;
            }
            if let Some(eb) = else_block {
                if let Some(block) = &eb.block {
                    if ast_block_has_break(block, self_label, depth) {
                        return true;
                    }
                }
                if let Some(if_stmt) = &eb.if_stmt {
                    if ast_stmt_has_break(&if_stmt.0, self_label, depth) {
                        return true;
                    }
                }
            }
            false
        }
        Stmt::IfLet {
            expr,
            body,
            else_body,
            ..
        } => {
            // Scrutinee is evaluated in the enclosing scope (not a loop).
            if ast_expr_has_break(&expr.0, self_label, depth) {
                return true;
            }
            if ast_block_has_break(body, self_label, depth) {
                return true;
            }
            if let Some(eb) = else_body {
                if ast_block_has_break(eb, self_label, depth) {
                    return true;
                }
            }
            false
        }
        Stmt::Match { scrutinee, arms } => {
            // Scrutinee and each arm's guard are evaluated in the enclosing
            // scope, alongside the arm bodies.
            ast_expr_has_break(&scrutinee.0, self_label, depth)
                || arms.iter().any(|arm| {
                    arm.guard
                        .as_ref()
                        .is_some_and(|g| ast_expr_has_break(&g.0, self_label, depth))
                        || ast_expr_has_break(&arm.body.0, self_label, depth)
                })
        }
        Stmt::Let {
            value, else_block, ..
        } => {
            if let Some(val) = value {
                if ast_expr_has_break(&val.0, self_label, depth) {
                    return true;
                }
            }
            if let Some(eb) = else_block {
                if ast_block_has_break(eb, self_label, depth) {
                    return true;
                }
            }
            false
        }
        Stmt::Var {
            value: Some(val), ..
        } => ast_expr_has_break(&val.0, self_label, depth),
        // `None`-value Var and `continue` have no nested expressions.
        Stmt::Var { value: None, .. } | Stmt::Continue { .. } => false,
        Stmt::Assign { target, value, .. } => {
            // Both the assignment target (e.g. `arr[idx]`, `obj.field`) and the
            // value are evaluated in the enclosing scope.
            ast_expr_has_break(&target.0, self_label, depth)
                || ast_expr_has_break(&value.0, self_label, depth)
        }
        Stmt::Defer(expr) => ast_expr_has_break(&expr.0, self_label, depth),
        Stmt::Expression(expr) => ast_expr_has_break(&expr.0, self_label, depth),
        // A `return` expression value is evaluated in the outer loop's scope;
        // a `break` inside `return unsafe { break; }` exits the outer loop.
        Stmt::Return(opt) => opt
            .as_ref()
            .is_some_and(|e| ast_expr_has_break(&e.0, self_label, depth)),
    }
}

#[allow(
    clippy::too_many_lines,
    reason = "exhaustive walker over every Expr variant; splitting would hide completeness guarantees"
)]
fn ast_expr_has_break(expr: &Expr, self_label: Option<&str>, depth: usize) -> bool {
    match expr {
        // ── Structural forms with direct block/statement children ─────────
        Expr::Block(block) => ast_block_has_break(block, self_label, depth),
        Expr::UnsafeBlock(block) => ast_block_has_break(block, self_label, depth),
        // `scope { }` and `fork { }` are conservatively propagated: a break
        // inside them counts for the outer loop (safe direction — types loop
        // Unit rather than Never).
        Expr::Scope { body } | Expr::ForkBlock { body } => {
            ast_block_has_break(body, self_label, depth)
        }
        Expr::ScopeDeadline { duration, body } => {
            ast_expr_has_break(&duration.0, self_label, depth)
                || ast_block_has_break(body, self_label, depth)
        }
        Expr::GenBlock { body } => {
            // Generator block: `gen { … }` introduces a coroutine scope that
            // is a break boundary (like a lambda).  A `break` inside cannot
            // exit an outer loop.
            let _ = body;
            false
        }

        // ── if / if-let / match ───────────────────────────────────────────
        Expr::If {
            condition,
            then_block,
            else_block,
        } => {
            // Condition is evaluated in the enclosing scope (an `if` is not a loop).
            if ast_expr_has_break(&condition.0, self_label, depth) {
                return true;
            }
            if ast_expr_has_break(&then_block.0, self_label, depth) {
                return true;
            }
            if let Some(eb) = else_block {
                return ast_expr_has_break(&eb.0, self_label, depth);
            }
            false
        }
        Expr::IfLet {
            expr,
            body,
            else_body,
            ..
        } => {
            // Scrutinee is evaluated in the enclosing scope (not a loop).
            if ast_expr_has_break(&expr.0, self_label, depth) {
                return true;
            }
            if ast_block_has_break(body, self_label, depth) {
                return true;
            }
            if let Some(eb) = else_body {
                return ast_block_has_break(eb, self_label, depth);
            }
            false
        }
        Expr::Match { scrutinee, arms } => {
            // Scrutinee and each arm's guard are evaluated in the enclosing
            // scope, alongside the arm bodies.
            ast_expr_has_break(&scrutinee.0, self_label, depth)
                || arms.iter().any(|arm| {
                    arm.guard
                        .as_ref()
                        .is_some_and(|g| ast_expr_has_break(&g.0, self_label, depth))
                        || ast_expr_has_break(&arm.body.0, self_label, depth)
                })
        }

        // ── Arithmetic / logical operators ────────────────────────────────
        Expr::Binary { left, right, .. } => {
            ast_expr_has_break(&left.0, self_label, depth)
                || ast_expr_has_break(&right.0, self_label, depth)
        }
        Expr::Unary { operand, .. } | Expr::Clone(operand) => {
            ast_expr_has_break(&operand.0, self_label, depth)
        }
        Expr::Is { lhs, rhs } => {
            ast_expr_has_break(&lhs.0, self_label, depth)
                || ast_expr_has_break(&rhs.0, self_label, depth)
        }

        // ── Aggregate constructors ────────────────────────────────────────
        Expr::Tuple(exprs) | Expr::Array(exprs) | Expr::Join(exprs) => exprs
            .iter()
            .any(|e| ast_expr_has_break(&e.0, self_label, depth)),
        Expr::ArrayRepeat { value, count } => {
            ast_expr_has_break(&value.0, self_label, depth)
                || ast_expr_has_break(&count.0, self_label, depth)
        }
        Expr::MapLiteral { entries } => entries.iter().any(|(k, v)| {
            ast_expr_has_break(&k.0, self_label, depth)
                || ast_expr_has_break(&v.0, self_label, depth)
        }),
        Expr::StructInit { fields, base, .. } => {
            if fields
                .iter()
                .any(|(_, v)| ast_expr_has_break(&v.0, self_label, depth))
            {
                return true;
            }
            if let Some(b) = base {
                return ast_expr_has_break(&b.0, self_label, depth);
            }
            false
        }
        Expr::MachineEmit { fields, .. } => fields
            .iter()
            .any(|(_, v)| ast_expr_has_break(&v.0, self_label, depth)),
        Expr::InterpolatedString(parts) => parts.iter().any(|p| match p {
            crate::ast::StringPart::Literal(_) => false,
            crate::ast::StringPart::Expr(e) => ast_expr_has_break(&e.0, self_label, depth),
        }),

        // ── Call / field / index ──────────────────────────────────────────
        Expr::Call { function, args, .. } => {
            if ast_expr_has_break(&function.0, self_label, depth) {
                return true;
            }
            args.iter()
                .any(|a| ast_expr_has_break(&a.expr().0, self_label, depth))
        }
        Expr::MethodCall { receiver, args, .. } => {
            if ast_expr_has_break(&receiver.0, self_label, depth) {
                return true;
            }
            args.iter()
                .any(|a| ast_expr_has_break(&a.expr().0, self_label, depth))
        }
        Expr::Spawn { target, args, .. } => {
            if ast_expr_has_break(&target.0, self_label, depth) {
                return true;
            }
            args.iter()
                .any(|(_, v)| ast_expr_has_break(&v.0, self_label, depth))
        }
        Expr::FieldAccess { object, .. } => ast_expr_has_break(&object.0, self_label, depth),
        Expr::Index { object, index } => {
            ast_expr_has_break(&object.0, self_label, depth)
                || ast_expr_has_break(&index.0, self_label, depth)
        }
        // Single-operand wrappers: cast, postfix-try, await, fork-child expr.
        Expr::Cast { expr, .. }
        | Expr::PostfixTry(expr)
        | Expr::Await(expr)
        | Expr::ForkChild { expr, .. } => ast_expr_has_break(&expr.0, self_label, depth),

        // ── Range ─────────────────────────────────────────────────────────
        Expr::Range { start, end, .. } => {
            start
                .as_ref()
                .is_some_and(|e| ast_expr_has_break(&e.0, self_label, depth))
                || end
                    .as_ref()
                    .is_some_and(|e| ast_expr_has_break(&e.0, self_label, depth))
        }

        // ── Concurrency selectors ─────────────────────────────────────────
        Expr::Select { arms, timeout } => {
            if arms.iter().any(|arm| {
                ast_expr_has_break(&arm.source.0, self_label, depth)
                    || ast_expr_has_break(&arm.body.0, self_label, depth)
            }) {
                return true;
            }
            if let Some(tc) = timeout {
                return ast_expr_has_break(&tc.duration.0, self_label, depth)
                    || ast_expr_has_break(&tc.body.0, self_label, depth);
            }
            false
        }
        Expr::Timeout { expr, duration } => {
            ast_expr_has_break(&expr.0, self_label, depth)
                || ast_expr_has_break(&duration.0, self_label, depth)
        }

        // ── Control-flow expressions ──────────────────────────────────────
        // `return` and `yield` both carry an optional value expression.
        Expr::Return(opt) | Expr::Yield(opt) => opt
            .as_ref()
            .is_some_and(|e| ast_expr_has_break(&e.0, self_label, depth)),

        // ── New scope boundaries and leaf forms (no break can escape) ────────
        //
        // Lambda/SpawnLambdaActor: independent closure/actor scope — break inside
        // cannot target an outer loop.  GenBlock is handled above.
        // Leaf expressions have no nested blocks or statements at all.
        Expr::Lambda { .. }
        | Expr::SpawnLambdaActor { .. }
        | Expr::Literal(_)
        | Expr::Identifier(_)
        | Expr::This
        | Expr::RegexLiteral(_)
        | Expr::ByteStringLiteral(_)
        | Expr::ByteArrayLiteral(_) => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{BinaryOp, Literal, Spanned};

    // Helpers ──────────────────────────────────────────────────────────────

    fn span() -> crate::ast::Span {
        0..0
    }

    fn sp<T>(val: T) -> Spanned<T> {
        (val, span())
    }

    fn bare_break() -> Spanned<Stmt> {
        sp(Stmt::Break {
            label: None,
            value: None,
        })
    }

    fn labeled_break(label: &str) -> Spanned<Stmt> {
        sp(Stmt::Break {
            label: Some(label.to_owned()),
            value: None,
        })
    }

    fn block_with_stmts(stmts: Vec<Spanned<Stmt>>) -> Block {
        Block {
            stmts,
            trailing_expr: None,
        }
    }

    fn block_with_trailing(expr: Spanned<Expr>) -> Block {
        Block {
            stmts: vec![],
            trailing_expr: Some(Box::new(expr)),
        }
    }

    fn empty_block() -> Block {
        Block {
            stmts: vec![],
            trailing_expr: None,
        }
    }

    // ── Basic break detection ──────────────────────────────────────────────

    #[test]
    fn unlabeled_break_at_depth_zero_detected() {
        // `loop { break }` — direct break, no nesting
        let body = block_with_stmts(vec![bare_break()]);
        assert!(loop_body_has_break(&body, None));
    }

    #[test]
    fn no_break_returns_false() {
        // `loop { }` — truly break-less
        let body = empty_block();
        assert!(!loop_body_has_break(&body, None));
    }

    // ── UnsafeBlock ───────────────────────────────────────────────────────

    #[test]
    fn break_inside_unsafe_block_detected() {
        // `loop { unsafe { break } }` — the B1 soundness repro case
        let unsafe_body = block_with_stmts(vec![bare_break()]);
        let unsafe_expr = sp(Expr::UnsafeBlock(Box::new(unsafe_body)));
        let body = block_with_stmts(vec![sp(Stmt::Expression(unsafe_expr))]);
        assert!(loop_body_has_break(&body, None));
    }

    #[test]
    fn unsafe_block_no_break_not_detected() {
        let unsafe_body = empty_block();
        let unsafe_expr = sp(Expr::UnsafeBlock(Box::new(unsafe_body)));
        let body = block_with_stmts(vec![sp(Stmt::Expression(unsafe_expr))]);
        assert!(!loop_body_has_break(&body, None));
    }

    // ── Expr::IfLet ───────────────────────────────────────────────────────

    #[test]
    fn break_inside_expr_iflet_then_detected() {
        // `loop { if let _ = v { break } }` — Expr::IfLet then-body
        use crate::ast::Pattern;
        let if_let_expr = sp(Expr::IfLet {
            pattern: Box::new(sp(Pattern::Wildcard)),
            expr: Box::new(sp(Expr::Identifier("v".into()))),
            body: block_with_stmts(vec![bare_break()]),
            else_body: None,
        });
        let body = block_with_stmts(vec![sp(Stmt::Expression(if_let_expr))]);
        assert!(loop_body_has_break(&body, None));
    }

    #[test]
    fn break_inside_expr_iflet_else_detected() {
        use crate::ast::Pattern;
        let if_let_expr = sp(Expr::IfLet {
            pattern: Box::new(sp(Pattern::Wildcard)),
            expr: Box::new(sp(Expr::Identifier("v".into()))),
            body: empty_block(),
            else_body: Some(block_with_stmts(vec![bare_break()])),
        });
        let body = block_with_stmts(vec![sp(Stmt::Expression(if_let_expr))]);
        assert!(loop_body_has_break(&body, None));
    }

    // ── Lambda/closure boundary ───────────────────────────────────────────

    #[test]
    fn break_inside_lambda_not_counted_for_outer_loop() {
        // `loop { let _ = || { break }; }` — break inside lambda does NOT
        // target the outer loop; the loop should appear break-less.
        let lambda_body = block_with_stmts(vec![bare_break()]);
        let lambda_expr = sp(Expr::Lambda {
            is_move: false,
            type_params: None,
            params: vec![],
            return_type: None,
            body: Box::new(sp(Expr::Block(lambda_body))),
        });
        let body = block_with_stmts(vec![sp(Stmt::Let {
            pattern: sp(crate::ast::Pattern::Wildcard),
            ty: None,
            value: Some(lambda_expr),
            else_block: None,
        })]);
        assert!(!loop_body_has_break(&body, None));
    }

    // ── Nested-loop depth handling ─────────────────────────────────────────

    #[test]
    fn break_in_inner_loop_not_counted_for_outer() {
        // `loop { loop { break } }` — the inner break targets the inner loop
        let inner_body = block_with_stmts(vec![bare_break()]);
        let inner_loop_stmt = sp(Stmt::Loop {
            label: None,
            body: inner_body,
        });
        let body = block_with_stmts(vec![inner_loop_stmt]);
        assert!(!loop_body_has_break(&body, None));
    }

    #[test]
    fn labeled_break_targeting_outer_loop_detected() {
        // `@outer: loop { loop { break @outer } }` — the labeled break in the
        // inner loop is directed at the outer loop and MUST count.
        let inner_body = block_with_stmts(vec![labeled_break("outer")]);
        let inner_loop = sp(Stmt::Loop {
            label: None,
            body: inner_body,
        });
        let body = block_with_stmts(vec![inner_loop]);
        assert!(loop_body_has_break(&body, Some("outer")));
    }

    #[test]
    fn labeled_break_targeting_inner_loop_not_counted_for_unlabeled_outer() {
        // `loop { @inner: loop { break @inner } }` — the labeled break exits
        // the inner loop, not the unlabeled outer loop.
        let inner_body = block_with_stmts(vec![labeled_break("inner")]);
        let inner_loop = sp(Stmt::Loop {
            label: Some("inner".into()),
            body: inner_body,
        });
        let body = block_with_stmts(vec![inner_loop]);
        assert!(!loop_body_has_break(&body, None));
    }

    // ── break in call-arg block ────────────────────────────────────────────

    #[test]
    fn break_inside_call_arg_block_detected() {
        // `loop { f({ break }) }` — break inside a block passed as a call arg
        let arg_block = sp(Expr::Block(block_with_stmts(vec![bare_break()])));
        let call_expr = sp(Expr::Call {
            function: Box::new(sp(Expr::Identifier("f".into()))),
            type_args: None,
            args: vec![crate::ast::CallArg::Positional(arg_block)],
            is_tail_call: false,
        });
        let body = block_with_stmts(vec![sp(Stmt::Expression(call_expr))]);
        assert!(loop_body_has_break(&body, None));
    }

    // ── break in if-branch ─────────────────────────────────────────────────

    #[test]
    fn loop_with_if_and_break_in_then_detected() {
        // `loop { if c { break } }` — break in then-block exits the loop
        let then_block = block_with_stmts(vec![bare_break()]);
        let if_stmt = sp(Stmt::If {
            condition: sp(Expr::Identifier("c".into())),
            then_block,
            else_block: None,
        });
        let body = block_with_stmts(vec![if_stmt]);
        assert!(loop_body_has_break(&body, None));
    }

    // ── break in trailing expression ───────────────────────────────────────

    #[test]
    fn break_in_trailing_expr_of_block_detected() {
        // `loop { { break } }` — break is the trailing expression of an inner block
        let inner = block_with_stmts(vec![bare_break()]);
        let outer = block_with_trailing(sp(Expr::Block(inner)));
        assert!(loop_body_has_break(&outer, None));
    }

    // ── GenBlock is a scope boundary ──────────────────────────────────────

    #[test]
    fn break_inside_gen_block_not_counted() {
        // `loop { gen { break } }` — GenBlock is a coroutine scope boundary;
        // the break does not escape to the outer loop.
        let gen_body = block_with_stmts(vec![bare_break()]);
        let gen_expr = sp(Expr::GenBlock { body: gen_body });
        let body = block_with_stmts(vec![sp(Stmt::Expression(gen_expr))]);
        assert!(!loop_body_has_break(&body, None));
    }

    // ── break with value ───────────────────────────────────────────────────

    #[test]
    fn break_with_value_detected() {
        // `loop { break 42 }` — a value-carrying break still exits the loop
        let body = block_with_stmts(vec![sp(Stmt::Break {
            label: None,
            value: Some(sp(Expr::Literal(Literal::Integer {
                value: 42,
                radix: crate::ast::IntRadix::Decimal,
            }))),
        })]);
        assert!(loop_body_has_break(&body, None));
    }

    // ── Binary / unary expressions ─────────────────────────────────────────

    #[test]
    fn break_inside_binary_rhs_detected() {
        // `loop { let _ = a + { break; 1 }; }` — break in rhs of binary
        let rhs_block = block_with_stmts(vec![
            bare_break(),
            sp(Stmt::Expression(sp(Expr::Literal(Literal::Integer {
                value: 1,
                radix: crate::ast::IntRadix::Decimal,
            })))),
        ]);
        let binary = sp(Expr::Binary {
            left: Box::new(sp(Expr::Identifier("a".into()))),
            op: BinaryOp::Add,
            right: Box::new(sp(Expr::Block(rhs_block))),
        });
        let body = block_with_stmts(vec![sp(Stmt::Let {
            pattern: sp(crate::ast::Pattern::Wildcard),
            ty: None,
            value: Some(binary),
            else_block: None,
        })]);
        assert!(loop_body_has_break(&body, None));
    }

    // ── Header / scrutinee / guard / target sub-expression positions ────────
    //
    // Every condition, scrutinee, iterable, assignment target, match-arm guard,
    // and break value is a sub-expression the walker must descend into. A break
    // hiding in any of them (via a block-expr) escapes to the enclosing loop;
    // missing one types a breakable loop as `Never` (the B1 soundness hole).

    fn int_lit(value: i64) -> Literal {
        Literal::Integer {
            value,
            radix: crate::ast::IntRadix::Decimal,
        }
    }

    /// `{ break }` in expression position — a block-expr containing a break.
    fn break_block_expr() -> Spanned<Expr> {
        sp(Expr::Block(block_with_stmts(vec![bare_break()])))
    }

    /// `{ break @label }` in expression position.
    fn labeled_break_block_expr(label: &str) -> Spanned<Expr> {
        sp(Expr::Block(block_with_stmts(vec![labeled_break(label)])))
    }

    fn let_wildcard(value: Spanned<Expr>) -> Spanned<Stmt> {
        sp(Stmt::Let {
            pattern: sp(crate::ast::Pattern::Wildcard),
            ty: None,
            value: Some(value),
            else_block: None,
        })
    }

    #[test]
    fn break_in_stmt_if_condition_detected() {
        // `loop { if { break } { } }` — break in the if CONDITION exits the loop.
        let if_stmt = sp(Stmt::If {
            condition: break_block_expr(),
            then_block: empty_block(),
            else_block: None,
        });
        assert!(loop_body_has_break(&block_with_stmts(vec![if_stmt]), None));
    }

    #[test]
    fn break_in_while_condition_detected() {
        // `loop { while { break } { } }` — the while CONDITION is a header
        // expression in the enclosing scope, so its break targets the outer loop.
        let while_stmt = sp(Stmt::While {
            label: None,
            condition: break_block_expr(),
            body: empty_block(),
        });
        assert!(loop_body_has_break(
            &block_with_stmts(vec![while_stmt]),
            None
        ));
    }

    #[test]
    fn while_condition_without_break_not_detected() {
        // `loop { while cond { } }` — a plain identifier condition has no break.
        let while_stmt = sp(Stmt::While {
            label: None,
            condition: sp(Expr::Identifier("cond".into())),
            body: empty_block(),
        });
        assert!(!loop_body_has_break(
            &block_with_stmts(vec![while_stmt]),
            None
        ));
    }

    #[test]
    fn break_in_for_iterable_detected() {
        use crate::ast::Pattern;
        // `loop { for x in { break } { } }` — iterable header break targets the
        // outer loop (the iterable is evaluated in the enclosing scope).
        let for_stmt = sp(Stmt::For {
            label: None,
            is_await: false,
            pattern: sp(Pattern::Identifier("x".into())),
            iterable: break_block_expr(),
            body: empty_block(),
        });
        assert!(loop_body_has_break(&block_with_stmts(vec![for_stmt]), None));
    }

    #[test]
    fn break_in_for_body_targets_for_not_outer() {
        use crate::ast::Pattern;
        // `loop { for x in v { break } }` — the body break targets the FOR loop,
        // not the outer loop, so the outer loop stays break-less.
        let for_stmt = sp(Stmt::For {
            label: None,
            is_await: false,
            pattern: sp(Pattern::Identifier("x".into())),
            iterable: sp(Expr::Identifier("v".into())),
            body: block_with_stmts(vec![bare_break()]),
        });
        assert!(!loop_body_has_break(
            &block_with_stmts(vec![for_stmt]),
            None
        ));
    }

    #[test]
    fn break_in_while_let_scrutinee_detected() {
        use crate::ast::Pattern;
        // `loop { while let _ = { break } { } }` — scrutinee header break.
        let wl = sp(Stmt::WhileLet {
            label: None,
            pattern: Box::new(sp(Pattern::Wildcard)),
            expr: Box::new(break_block_expr()),
            body: empty_block(),
        });
        assert!(loop_body_has_break(&block_with_stmts(vec![wl]), None));
    }

    #[test]
    fn break_in_stmt_match_scrutinee_detected() {
        // `loop { match { break } { } }` — break in the match SCRUTINEE.
        let m = sp(Stmt::Match {
            scrutinee: break_block_expr(),
            arms: vec![],
        });
        assert!(loop_body_has_break(&block_with_stmts(vec![m]), None));
    }

    #[test]
    fn break_in_stmt_match_arm_guard_detected() {
        use crate::ast::{MatchArm, Pattern};
        // `loop { match v { _ if { break } => 1 } }` — break in an arm GUARD.
        let arm = MatchArm {
            pattern: sp(Pattern::Wildcard),
            guard: Some(break_block_expr()),
            body: sp(Expr::Literal(int_lit(1))),
        };
        let m = sp(Stmt::Match {
            scrutinee: sp(Expr::Identifier("v".into())),
            arms: vec![arm],
        });
        assert!(loop_body_has_break(&block_with_stmts(vec![m]), None));
    }

    #[test]
    fn break_in_stmt_iflet_scrutinee_detected() {
        use crate::ast::Pattern;
        // `loop { if let _ = { break } { } }` — break in the if-let SCRUTINEE.
        let il = sp(Stmt::IfLet {
            pattern: Box::new(sp(Pattern::Wildcard)),
            expr: Box::new(break_block_expr()),
            body: empty_block(),
            else_body: None,
        });
        assert!(loop_body_has_break(&block_with_stmts(vec![il]), None));
    }

    #[test]
    fn break_in_assign_target_detected() {
        // `loop { ({ break })[0] = 1 }` — break in the assignment TARGET expr.
        let target = sp(Expr::Index {
            object: Box::new(break_block_expr()),
            index: Box::new(sp(Expr::Literal(int_lit(0)))),
        });
        let assign = sp(Stmt::Assign {
            target,
            op: None,
            value: sp(Expr::Literal(int_lit(1))),
        });
        assert!(loop_body_has_break(&block_with_stmts(vec![assign]), None));
    }

    #[test]
    fn break_in_expr_if_condition_detected() {
        // `loop { let _ = if { break } { 1 } else { 2 }; }` — Expr::If condition.
        let if_expr = sp(Expr::If {
            condition: Box::new(break_block_expr()),
            then_block: Box::new(sp(Expr::Literal(int_lit(1)))),
            else_block: Some(Box::new(sp(Expr::Literal(int_lit(2))))),
        });
        assert!(loop_body_has_break(
            &block_with_stmts(vec![let_wildcard(if_expr)]),
            None
        ));
    }

    #[test]
    fn break_in_expr_match_scrutinee_detected() {
        use crate::ast::{MatchArm, Pattern};
        // `loop { let _ = match { break } { _ => 1 }; }` — Expr::Match scrutinee.
        let arm = MatchArm {
            pattern: sp(Pattern::Wildcard),
            guard: None,
            body: sp(Expr::Literal(int_lit(1))),
        };
        let m = sp(Expr::Match {
            scrutinee: Box::new(break_block_expr()),
            arms: vec![arm],
        });
        assert!(loop_body_has_break(
            &block_with_stmts(vec![let_wildcard(m)]),
            None
        ));
    }

    #[test]
    fn break_in_expr_match_arm_guard_detected() {
        use crate::ast::{MatchArm, Pattern};
        // `loop { let _ = match v { _ if { break } => 1, _ => 2 }; }` — guard.
        let guarded = MatchArm {
            pattern: sp(Pattern::Wildcard),
            guard: Some(break_block_expr()),
            body: sp(Expr::Literal(int_lit(1))),
        };
        let fallback = MatchArm {
            pattern: sp(Pattern::Wildcard),
            guard: None,
            body: sp(Expr::Literal(int_lit(2))),
        };
        let m = sp(Expr::Match {
            scrutinee: Box::new(sp(Expr::Identifier("v".into()))),
            arms: vec![guarded, fallback],
        });
        assert!(loop_body_has_break(
            &block_with_stmts(vec![let_wildcard(m)]),
            None
        ));
    }

    #[test]
    fn break_in_expr_iflet_scrutinee_detected() {
        use crate::ast::Pattern;
        // `loop { let _ = if let _ = { break } { } ... }` — Expr::IfLet scrutinee.
        let il = sp(Expr::IfLet {
            pattern: Box::new(sp(Pattern::Wildcard)),
            expr: Box::new(break_block_expr()),
            body: empty_block(),
            else_body: None,
        });
        assert!(loop_body_has_break(
            &block_with_stmts(vec![let_wildcard(il)]),
            None
        ));
    }

    #[test]
    fn labeled_break_in_inner_while_header_targets_outer() {
        // `@outer: loop { while { break @outer } { } }` — a labeled header break
        // escapes to the named outer loop.
        let while_stmt = sp(Stmt::While {
            label: None,
            condition: labeled_break_block_expr("outer"),
            body: empty_block(),
        });
        assert!(loop_body_has_break(
            &block_with_stmts(vec![while_stmt]),
            Some("outer")
        ));
    }

    #[test]
    fn unlabeled_break_in_nested_loop_header_not_counted_for_outer() {
        // `loop { loop { while { break } { } } }` — the header break targets the
        // middle loop (its nearest enclosing loop), NOT the outermost loop.
        let inner_while = sp(Stmt::While {
            label: None,
            condition: break_block_expr(),
            body: empty_block(),
        });
        let middle_loop = sp(Stmt::Loop {
            label: None,
            body: block_with_stmts(vec![inner_while]),
        });
        assert!(!loop_body_has_break(
            &block_with_stmts(vec![middle_loop]),
            None
        ));
    }

    #[test]
    fn break_value_with_nested_labeled_break_detected() {
        // `@outer: loop { @inner: loop { break ({ break @outer }) } }` — the
        // inner break's VALUE carries a break targeting @outer.
        let inner_break = sp(Stmt::Break {
            label: None,
            value: Some(labeled_break_block_expr("outer")),
        });
        let inner_loop = sp(Stmt::Loop {
            label: Some("inner".into()),
            body: block_with_stmts(vec![inner_break]),
        });
        assert!(loop_body_has_break(
            &block_with_stmts(vec![inner_loop]),
            Some("outer")
        ));
    }
}
