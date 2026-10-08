//! Field-init shorthand: `name` in a named field list stands for `name: name`.

use hew_parser::ast::{ConditionItem, Expr, Item, MachineTransitionBodyForm, Stmt};
use hew_parser::ast_eq::program_eq_ignoring_spans;

fn parse_clean(src: &str) -> hew_parser::ParseResult {
    let result = hew_parser::parse(src);
    assert!(
        result.errors.is_empty(),
        "source must parse cleanly:\n{src}\nerrors: {:?}",
        result.errors
    );
    result
}

/// The shorthand spelling parses to the same tree as the explicit one, so no
/// later stage sees a difference.
fn assert_desugars(shorthand: &str, explicit: &str) {
    let short = parse_clean(shorthand);
    let long = parse_clean(explicit);
    assert!(
        program_eq_ignoring_spans(&short.program, &long.program),
        "shorthand must desugar to the explicit form:\n{shorthand}\nvs\n{explicit}"
    );
}

/// `hew fmt` keeps each field as written: shorthand stays shorthand and an
/// explicit `name: name` stays explicit.
fn assert_fmt_preserves(src: &str) {
    let parsed = parse_clean(src);
    let formatted = hew_parser::fmt::format_source(src, &parsed.program);
    assert_eq!(formatted, src, "fmt must keep the spelling as written");
}

const TYPES: &str = "type Config {\n    host: string;\n    port: i64;\n}\n\nenum Shape {\n    Circle { radius: f64; }\n}\n\nactor Worker {\n    let port: i64;\n}\n\n";

#[test]
fn record_literal_shorthand_desugars() {
    assert_desugars(
        &format!("{TYPES}fn f(host: string, port: i64) -> Config {{\n    Config {{ host, port }}\n}}\n"),
        &format!("{TYPES}fn f(host: string, port: i64) -> Config {{\n    Config {{ host: host, port: port }}\n}}\n"),
    );
}

#[test]
fn mixed_shorthand_and_update_desugar() {
    assert_desugars(
        &format!("{TYPES}fn f(old: Config, port: i64) -> Config {{\n    Config {{ ..old, port }}\n}}\n"),
        &format!("{TYPES}fn f(old: Config, port: i64) -> Config {{\n    Config {{ ..old, port: port }}\n}}\n"),
    );
}

#[test]
fn variant_spawn_and_emit_shorthand_desugar() {
    let body = |radius: &str, port: &str| {
        format!(
            "{TYPES}fn f(radius: f64, port: i64) {{\n    let a = Shape.Circle {{ {radius} }};\n    let b: Shape = .Circle {{ {radius} }};\n    let w = spawn Worker({port});\n    emit Ping {{ {port} }};\n}}\n"
        )
    };
    assert_desugars(
        &body("radius", "port"),
        &body("radius: radius", "port: port"),
    );
}

#[test]
fn fmt_keeps_shorthand_and_explicit_as_written() {
    assert_fmt_preserves(&format!(
        "{TYPES}fn f(host: string, port: i64, old: Config, radius: f64) {{\n    let a = Config {{ host, port: port }};\n    let b = Config {{ ..old, port }};\n    let c: Shape = .Circle {{ radius }};\n    let d = Shape.Circle {{ radius: radius }};\n    let w = spawn Worker(port);\n    emit Ping {{ port, host: host }};\n}}\n"
    ));
}

fn main_body(result: &hew_parser::ParseResult) -> &hew_parser::ast::Block {
    result
        .program
        .items
        .iter()
        .find_map(|(item, _)| match item {
            Item::Function(f) => Some(&f.body),
            _ => None,
        })
        .expect("a function")
}

/// The scrutinee of an `if let` expression.
fn if_let_scrutinee(expr: &Expr) -> &Expr {
    let Expr::IfLet { conditions, .. } = expr else {
        panic!("expected if-let, got {expr:?}");
    };
    let Some(ConditionItem::Let { expr, .. }) = conditions.first() else {
        panic!("expected a let condition");
    };
    &expr.0
}

/// The trailing value of the first statement's `let` initializer.
fn let_value(result: &hew_parser::ParseResult) -> &Expr {
    let Some((Stmt::Let { value: Some(v), .. }, _)) = main_body(result).stmts.first() else {
        panic!("expected a let with an initializer");
    };
    &v.0
}

#[test]
fn lone_shorthand_after_if_let_scrutinee_is_the_body() {
    let result =
        parse_clean("fn main() {\n    let hit = if let .Some(v) = cached { v } else { 0 };\n}\n");
    let scrutinee = if_let_scrutinee(let_value(&result));
    assert!(
        matches!(scrutinee, Expr::Ident(_)),
        "`cached {{ v }}` must not be a record literal, got {scrutinee:?}"
    );
}

#[test]
fn record_literal_with_shorthand_list_in_if_let_scrutinee() {
    // Two fields (or one with `:`) cannot start a block, so the literal stays.
    let result = parse_clean(
        "fn main() {\n    let ok = if let Pair { a, b } = Pair { a, b } { a } else { 0 };\n}\n",
    );
    let scrutinee = if_let_scrutinee(let_value(&result));
    assert!(
        matches!(scrutinee, Expr::StructInit { .. }),
        "got {scrutinee:?}"
    );
}

#[test]
fn lone_shorthand_after_for_iterable_is_the_body() {
    let result = parse_clean("fn main() {\n    for i in 0..n { i }\n}\n");
    let Some((Stmt::For { iterable, .. }, _)) = main_body(&result).stmts.first() else {
        panic!("expected a for loop");
    };
    assert!(
        matches!(&iterable.0, Expr::Binary { right, .. } if matches!(right.0, Expr::Ident(_))),
        "`n {{ i }}` must not be a record literal, got {:?}",
        iterable.0
    );
}

#[test]
fn lone_shorthand_in_condition_is_the_body() {
    parse_clean(
        "fn main() {\n    if ready { done } else { pending }\n    while busy { idle }\n}\n",
    );
}

#[test]
fn machine_transition_lone_name_is_a_block_and_list_is_a_payload() {
    let result = parse_clean(
        "machine Meter {\n    events {\n        Tick;\n        Reset;\n    }\n    state Watching { peak: i64; seen: i64; }\n    on Tick: Watching => Watching { state }\n    on Reset: Watching => Watching { peak, seen: 0 }\n}\n",
    );
    let machine = result
        .program
        .items
        .iter()
        .find_map(|(item, _)| match item {
            Item::Machine(m) => Some(m),
            _ => None,
        })
        .expect("a machine");
    let forms: Vec<_> = machine.transitions.iter().map(|t| t.body_form).collect();
    assert_eq!(
        forms,
        [
            MachineTransitionBodyForm::Block,
            MachineTransitionBodyForm::PayloadShorthand
        ]
    );
}

#[test]
fn lone_shorthand_after_while_let_scrutinee_is_the_body() {
    let result = parse_clean("fn main() {\n    while let .Some(v) = next { v }\n}\n");
    let Some((Stmt::WhileLet { conditions, .. }, _)) = main_body(&result).stmts.first() else {
        panic!("expected a while-let loop");
    };
    let Some(ConditionItem::Let { expr, .. }) = conditions.first() else {
        panic!("expected a let condition");
    };
    assert!(
        matches!(expr.0, Expr::Ident(_)),
        "`next {{ v }}` must not be a record literal, got {:?}",
        expr.0
    );
}
