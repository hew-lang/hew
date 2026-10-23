use hew_parser::ast::{CallableCallMode, Item, TypeExpr};
use hew_parser::{ast_eq::program_eq_ignoring_spans, fmt::format_source, parse};

#[test]
fn closure_snapshots_preserve_pipe_forms_and_round_trip() {
    let source = r"
fn main() {
    let count = 0;
    var next = || { count = count + 1; count };
    var add = move |x: i64| -> i64 { count = count + x; count };
    let actor_counter = actor || -> i64 { count += 1; count };
    let actor_value = actor move |n: i64| { count += n; count };
    let ordinary = |x| x;
    let other = capture(3);
}
fn capture(value: i64) -> i64 { value }
";
    let parsed = parse(source);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let formatted = format_source(source, &parsed.program);
    let reparsed = parse(&formatted);
    assert!(reparsed.errors.is_empty(), "{:?}", reparsed.errors);
    assert!(program_eq_ignoring_spans(
        &parsed.program,
        &reparsed.program
    ));
    assert_eq!(format_source(&formatted, &reparsed.program), formatted);
}

#[test]
fn retired_capture_prefix_has_an_exact_migration_fix() {
    for expression in [
        "capture(var count) || { count += 1; count }",
        "move capture(var count) || { count += 1; count }",
        "actor capture(var count) || { count += 1; count }",
        "actor move capture(var count) || { count += 1; count }",
    ] {
        let source = format!("fn main() {{ let count = 0; var f = {expression}; }}");
        let parsed = parse(&source);
        let [error] = parsed.errors.as_slice() else {
            panic!("{:?}", parsed.errors)
        };
        assert_eq!(
            error.kind,
            hew_parser::ParseDiagnosticKind::LegacyPrivateCapture
        );
        let edit = &error.fixes[0].edits[0];
        assert_eq!(
            &source[edit.span.start..edit.span.end],
            "capture(var count)"
        );
        let mut migrated = source.clone();
        migrated.replace_range(edit.span.start..edit.span.end, &edit.new_text);
        assert!(parse(&migrated).errors.is_empty(), "{migrated}");
    }
}

#[test]
fn callable_qualifiers_preserve_nested_types_and_round_trip() {
    for (qualifiers, call, clone, suspends) in [
        ("", CallableCallMode::Read, false, false),
        ("[clone]", CallableCallMode::Read, true, false),
        ("[var]", CallableCallMode::Var, false, false),
        ("[var, clone]", CallableCallMode::Var, true, false),
        ("[once]", CallableCallMode::Once, false, false),
        ("[once, clone]", CallableCallMode::Once, true, false),
        ("[suspends]", CallableCallMode::Read, false, true),
        (
            "[once, clone, suspends]",
            CallableCallMode::Once,
            true,
            true,
        ),
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
        assert_eq!(capabilities.suspends, suspends);
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
        "[suspends, suspends]",
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
