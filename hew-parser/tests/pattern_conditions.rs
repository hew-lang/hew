//! Parsing and formatting of the pattern conditions of spec §12.5: `&&`-joined
//! `let` and boolean operands in `if` and `while`.

use hew_parser::ast::{Block, ConditionItem, Expr, Stmt};
use hew_parser::fmt::format_source;
use hew_parser::parse;

fn body_of(source: &str) -> Block {
    let parsed = parse(source);
    assert!(
        parsed.errors.is_empty(),
        "source must parse cleanly: {:#?}",
        parsed.errors
    );
    parsed
        .program
        .items
        .into_iter()
        .find_map(|(item, _)| match item {
            hew_parser::ast::Item::Function(decl) => Some(decl.body),
            _ => None,
        })
        .expect("source must declare a function")
}

/// The condition of the body's single control-flow construct. A lone `if` in a
/// function body is promoted to the block's trailing expression, so look there
/// as well as in the statement list.
fn condition_of(body: &Block) -> &[ConditionItem] {
    if let Some((Stmt::IfLet { conditions, .. } | Stmt::WhileLet { conditions, .. }, _)) =
        body.stmts.first()
    {
        return conditions;
    }
    match body.trailing_expr.as_deref() {
        Some((Expr::IfLet { conditions, .. }, _)) => conditions,
        other => panic!("expected a pattern condition, found {other:?}"),
    }
}

#[test]
fn let_chain_keeps_every_operand_in_source_order() {
    let body = body_of(
        r"
fn main() {
    if let .Some(n) = first && n > 10 && let .Ok(text) = second {
        println(text);
    }
}
",
    );
    let conditions = condition_of(&body);
    assert!(matches!(conditions[0], ConditionItem::Let { .. }));
    assert!(matches!(conditions[1], ConditionItem::Expr(_)));
    assert!(matches!(conditions[2], ConditionItem::Let { .. }));
    assert_eq!(conditions.len(), 3);
}

#[test]
fn a_boolean_operand_may_open_the_chain() {
    let body = body_of(
        r#"
fn main() {
    if flag && let .Some(n) = first {
        println(f"{n}");
    }
}
"#,
    );
    let conditions = condition_of(&body);
    assert!(matches!(conditions[0], ConditionItem::Expr(_)));
    assert!(matches!(conditions[1], ConditionItem::Let { .. }));
}

#[test]
fn a_plain_condition_stays_one_expression() {
    // Without a `let` operand the condition keeps ordinary precedence, so
    // `a || b && c` is one expression and the statement is a plain `if`.
    let body = body_of(
        r"
fn main() {
    if a || b && c {
        run();
    }
}
",
    );
    assert!(matches!(
        body.trailing_expr.as_deref(),
        Some((Expr::If { .. }, _))
    ));
}

#[test]
fn while_takes_the_same_chain() {
    let body = body_of(
        r#"
fn main() {
    while let .Some(v) = next() && v < 3 {
        println(f"{v}");
    }
}
"#,
    );
    let conditions = condition_of(&body);
    assert_eq!(conditions.len(), 2);
    assert!(matches!(conditions[0], ConditionItem::Let { .. }));
}

#[test]
fn or_cannot_join_a_let_operand() {
    let parsed = parse(
        r#"
fn main() {
    if let .Some(n) = first || n > 0 {
        println(f"{n}");
    }
}
"#,
    );
    assert!(
        parsed
            .errors
            .iter()
            .any(|error| error.message.contains("E_OR_JOINED_LET_CONDITION")),
        "`||` cannot join a `let` operand: {:#?}",
        parsed.errors
    );
}

#[test]
fn the_formatter_round_trips_a_chain() {
    let source = r#"fn main() {
    if let .Some(n) = first && n > 10 && let .Ok(text) = second {
        println(text);
    }
    if flag && let .Some(n) = first {
        println(f"{n}");
    }
    while let .Some(v) = next() && v < 3 {
        println(f"{v}");
    }
}
"#;
    let parsed = parse(source);
    assert!(
        parsed.errors.is_empty(),
        "source must parse cleanly: {:#?}",
        parsed.errors
    );
    let formatted = format_source(source, &parsed.program);
    assert_eq!(formatted, source, "a chain must format as written");
}

#[test]
fn a_pattern_condition_scrutinee_still_takes_a_struct_literal() {
    let body = body_of(
        r"
fn main() {
    if let Point { x, y } = Point { x: 1, y: 2 } {
        use_point(x, y);
    }
}
",
    );
    let conditions = condition_of(&body);
    let ConditionItem::Let { expr, .. } = &conditions[0] else {
        panic!("expected a `let` operand");
    };
    assert!(matches!(expr.0, Expr::StructInit { .. }));
}
