//! Binding of the `as` cast relative to prefix and binary operators (§12.2).
//!
//! Every prefix operator binds tighter than `as`, and `as` binds tighter than
//! every binary operator: `-x as u8` is `(-x) as u8`, `a * b as T` is
//! `a * (b as T)`, and `x as T as U` chains from the left. The formatter must
//! print exactly the parentheses the tree needs to reparse to itself.

use hew_parser::ast::{Expr, Item, Spanned, UnaryOp};

/// Parse `expr` as the trailing expression of a function and render its tree
/// fully parenthesised.
fn shape(expr: &str) -> String {
    let source = format!("fn f() {{ {expr} }}");
    let parsed = hew_parser::parse(&source);
    assert!(parsed.errors.is_empty(), "{expr}: {:?}", parsed.errors);
    let Item::Function(f) = &parsed.program.items[0].0 else {
        panic!("expected a function");
    };
    let tail = f.body.trailing_expr.as_ref().expect("trailing expression");
    render(&source, tail)
}

fn render(source: &str, expr: &Spanned<Expr>) -> String {
    match &expr.0 {
        Expr::Unary { op, operand } => {
            let op = match op {
                UnaryOp::Not => "!",
                UnaryOp::Negate => "-",
                UnaryOp::BitNot => "~",
                UnaryOp::RawDeref => "*",
            };
            format!("({op}{})", render(source, operand))
        }
        Expr::Cast { expr, ty } => {
            format!(
                "({} as {})",
                render(source, expr),
                source[ty.1.clone()].trim()
            )
        }
        Expr::Binary { left, right, .. } => {
            let op = source[left.1.end..right.1.start]
                .trim_matches(|c: char| c == '(' || c == ')' || c.is_whitespace());
            format!("({} {op} {})", render(source, left), render(source, right))
        }
        Expr::Await(operand) => format!("(await {})", render(source, operand)),
        Expr::Clone(operand) => format!("(clone {})", render(source, operand)),
        Expr::ForkChild { expr } => format!("(fork {})", render(source, expr)),
        Expr::PostfixTry(operand) => format!("({}?)", render(source, operand)),
        _ => source[expr.1.clone()].trim().to_string(),
    }
}

#[test]
fn every_prefix_operator_binds_tighter_than_as() {
    for (source, expected) in [
        ("-x as u8", "((-x) as u8)"),
        ("-1.5 as u32", "((-1.5) as u32)"),
        ("!x as bool", "((!x) as bool)"),
        ("~x as u16", "((~x) as u16)"),
        ("*p as i64", "((*p) as i64)"),
        ("clone x as T", "((clone x) as T)"),
        ("await t as i64", "((await t) as i64)"),
        ("fork work() as i64", "((fork work()) as i64)"),
    ] {
        assert_eq!(shape(source), expected, "{source}");
    }
}

#[test]
fn postfix_binds_into_the_prefix_operand_before_the_cast() {
    assert_eq!(shape("-x.y as i64"), "((-x.y) as i64)");
    assert_eq!(shape("-f()? as i64"), "((-(f()?)) as i64)");
    assert_eq!(shape("await t? as i64"), "(((await t)?) as i64)");
}

#[test]
fn as_binds_tighter_than_binary_operators() {
    assert_eq!(shape("a * b as T"), "(a * (b as T))");
    assert_eq!(shape("a as T * b"), "((a as T) * b)");
    assert_eq!(shape("a - b as T"), "(a - (b as T))");
    assert_eq!(shape("a << b as u32"), "(a << (b as u32))");
    assert_eq!(shape("-a * b as T"), "((-a) * (b as T))");
}

#[test]
fn as_chains_from_the_left() {
    assert_eq!(shape("x as T as U"), "((x as T) as U)");
    assert_eq!(shape("-x as T as U"), "(((-x) as T) as U)");
}

#[test]
fn explicit_parentheses_keep_the_cast_inside_the_prefix() {
    assert_eq!(shape("-(x as u8)"), "(-(x as u8))");
    assert_eq!(shape("!(x as bool)"), "(!(x as bool))");
    assert_eq!(shape("await (t as i64)"), "(await (t as i64))");
}

#[test]
fn borrow_prefix_is_refused_before_a_cast() {
    let parsed = hew_parser::parse("fn f() { &x as i64 }");
    assert!(
        parsed
            .errors
            .iter()
            .any(|e| e.message.contains("`&` is not a prefix operator")),
        "{:?}",
        parsed.errors
    );
}

/// The formatter prints `-(x as u8)` for a negated cast and no parentheses for
/// a cast of a negation, so each tree reprints to source that reparses to it.
#[test]
fn fmt_parenthesises_exactly_where_the_tree_needs() {
    for (source, printed) in [
        ("-(x as u8)", "-(x as u8)"),
        ("(-x) as u8", "-x as u8"),
        ("!(x as bool)", "!(x as bool)"),
        ("~(x as u16)", "~(x as u16)"),
        ("await (t as i64)", "await (t as i64)"),
        ("(await t) as i64", "await t as i64"),
        ("(a * b) as T", "(a * b) as T"),
        ("a * (b as T)", "a * b as T"),
        ("((-x) as T) as U", "-x as T as U"),
        ("(-x as T).m()", "(-x as T).m()"),
        ("(x as T)?", "(x as T)?"),
        ("-((x as T)?)", "-(x as T)?"),
        ("(clone x as T).m()", "(clone x as T).m()"),
    ] {
        let parsed = hew_parser::parse(&format!("fn f() {{ {source} }}"));
        assert!(parsed.errors.is_empty(), "{source}: {:?}", parsed.errors);
        let formatted = hew_parser::fmt::format_program(&parsed.program);
        assert!(
            formatted.contains(&format!("    {printed}\n")),
            "{source} printed as {formatted}"
        );
        assert_eq!(shape(printed), shape(source), "{source} reparses");
    }
}
