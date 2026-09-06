use hew_parser::ast::{CallableCallMode, Expr, Item, Stmt, TypeExpr};
use hew_parser::{ast_eq::program_eq_ignoring_spans, fmt::format_source, parse};

#[test]
fn private_capture_prefix_preserves_pipe_forms_and_round_trips() {
    let source = r"
fn main() {
    let count = 0;
    var next = capture(var count) || { count = count + 1; count };
    var add = move capture(var count) |x: i64| -> i64 { count = count + x; count };
    let values = [1, 2];
    let ordinary = |x| x;
    let other = capture(3);
}
fn capture(value: i64) -> i64 { value }
fn compose(flag: bool, left: fn[clone]() -> Option<i64>, right: fn[once, clone]() -> Result<Option<i64>, string>) {
    let chosen: Option<fn[clone]() -> Option<i64>> = if flag { .Some(left) } else { .None };
}
";
    let parsed = parse(source);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let Item::Function(main) = &parsed.program.items[0].0 else {
        panic!("function")
    };
    let Stmt::Var {
        value:
            Some((
                Expr::Lambda {
                    private_captures,
                    is_move,
                    ..
                },
                _,
            )),
        ..
    } = &main.body.stmts[1].0
    else {
        panic!("private closure")
    };
    assert!(!is_move);
    assert_eq!(private_captures[0].0, "count");
    let formatted = format_source(source, &parsed.program);
    assert!(formatted.contains("move capture(var count) |x: i64| -> i64"));
    let reparsed = parse(&formatted);
    assert!(reparsed.errors.is_empty(), "{:?}", reparsed.errors);
    assert!(program_eq_ignoring_spans(
        &parsed.program,
        &reparsed.program
    ));
}

#[test]
fn callable_qualifiers_preserve_nested_types_and_round_trip() {
    for (qualifiers, call, clone) in [
        ("", CallableCallMode::Read, false),
        ("[clone]", CallableCallMode::Read, true),
        ("[var]", CallableCallMode::Var, false),
        ("[var, clone]", CallableCallMode::Var, true),
        ("[once]", CallableCallMode::Once, false),
        ("[once, clone]", CallableCallMode::Once, true),
    ] {
        let source =
            format!("fn accept(f: fn{qualifiers}(i64) -> fn[clone]() -> i64) {{}} fn once() {{}}");
        let parsed = parse(&source);
        assert!(parsed.errors.is_empty(), "{source}: {:?}", parsed.errors);
        let Item::Function(accept) = &parsed.program.items[0].0 else {
            panic!("function")
        };
        let TypeExpr::Function {
            capabilities,
            return_type,
            ..
        } = &accept.params[0].ty.0
        else {
            panic!("callable type")
        };
        assert_eq!(capabilities.call, call);
        assert_eq!(capabilities.clone, clone);
        assert!(
            matches!(return_type.0, TypeExpr::Function { capabilities, .. } if capabilities.clone)
        );
        let formatted = format_source(&source, &parsed.program);
        let reparsed = parse(&formatted);
        assert!(reparsed.errors.is_empty(), "{:?}", reparsed.errors);
        assert!(program_eq_ignoring_spans(
            &parsed.program,
            &reparsed.program
        ));
    }
}

#[test]
fn malformed_capture_prefixes_and_qualifiers_are_rejected() {
    for expression in [
        "capture(var count, var count) || count",
        "capture(var count) |count| count",
        "capture(var count = 0) || count",
        "capture(var count as other) || other",
        "move capture(count) || count",
        "capture(var count) (x) => x",
        "[var count] || count",
        "move [var count] || count",
        "move capture() || count",
        "move capture(var count) [1, 2]",
    ] {
        let source = format!("fn main() {{ let count = 0; let f = {expression}; }}");
        assert!(!parse(&source).errors.is_empty(), "accepted {source}");
    }
    for qualifiers in [
        "[]",
        "[var, once]",
        "[once, var]",
        "[var, var]",
        "[once, once]",
        "[clone, clone]",
        "[Copy]",
        "[Clone]",
        "[var, Clone]",
        "[mut]",
        "[other]",
    ] {
        let source = format!("fn accept(f: fn{qualifiers}() -> i64) {{}}");
        assert!(!parse(&source).errors.is_empty(), "accepted {source}");
    }
}
