//! Small-stack coverage for parsing, formatting, comparing and dropping
//! postfix chains, optional-chain runs and coalesce runs.

use hew_parser::ast::{Expr, Ident, Spanned, TypeExpr};
use hew_parser::ast_eq::program_eq_ignoring_spans;
use hew_parser::chain::Chain;
use hew_parser::fmt::format_checked;
use hew_parser::parse;

/// Run `f` on a thread with `stack` bytes of stack, failing the test if it
/// overflows or panics.
fn on_stack(stack: usize, f: impl FnOnce() + Send + 'static) {
    std::thread::Builder::new()
        .stack_size(stack)
        .spawn(f)
        .expect("spawn")
        .join()
        .expect("a long chain overflowed or failed");
}

/// Parse `source`, format it, and require the formatted text to equal the
/// source and to parse to the same program.
fn formats_unchanged(source: &str) {
    let parsed = parse(source);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors.first());
    let formatted = format_checked(source, &parsed.program).expect("faithful formatting");
    assert!(formatted == source, "formatting changed the chain");
    let reparsed = parse(&formatted);
    assert!(program_eq_ignoring_spans(
        &parsed.program,
        &reparsed.program
    ));
}

#[test]
fn long_postfix_chains_parse_and_format_on_a_small_stack() {
    // Every shape the postfix loop builds, at over 2,000 links, on a 1 MiB
    // stack in an unoptimized build.
    let shapes = [
        ".trim()".repeat(2_048),
        ".field".repeat(2_048),
        "[0]".repeat(2_048),
        "()".repeat(2_048),
        "[0]?".repeat(2_048),
        " as i64".repeat(2_048),
        "?.next()".repeat(2_048),
        ".kids[0]?.v".repeat(1_024),
    ];
    for links in shapes {
        on_stack(1024 * 1024, move || {
            formats_unchanged(&format!("fn main() {{\n    let value = x{links};\n}}\n"));
        });
    }
}

#[test]
fn a_long_coalesce_run_parses_and_formats_on_a_small_stack() {
    on_stack(1024 * 1024, || {
        let links = " ?? x".repeat(2_048);
        formats_unchanged(&format!(
            "fn main() {{\n    let value = x{links} ?? 0;\n}}\n"
        ));
    });
}

#[test]
fn an_optional_chain_run_nests_to_the_right() {
    // `a?.b?.c` is `a?.(b?.c)`, read in a loop and folded from the right.
    let parsed = parse("fn main() { let v = a?.b?.c; }");
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let formatted = hew_parser::fmt::format_program(&parsed.program);
    assert!(formatted.contains("a?.b?.c"), "{formatted}");
}

#[test]
fn a_try_before_a_member_keeps_its_space() {
    // `x? .f` is a try then a field: printed as `x?.f` it would become an
    // optional chain, and `x? ?` printed as `x??` a coalesce.
    for (source, expected) in [
        ("fn main() { let v = x? .f; }", "x? .f"),
        ("fn main() { let v = x? .f(); }", "x? .f()"),
        ("fn main() { let v = x? ?; }", "x? ?"),
        ("fn main() { let v = x? ?.f; }", "x? ?.f"),
        ("fn main() { let v = (x?).f; }", "(x?).f"),
        ("fn main() { let v = (x?).f(); }", "(x?).f()"),
        ("fn main() { let v = (x?)?; }", "(x?)?"),
    ] {
        let parsed = parse(source);
        assert!(parsed.errors.is_empty(), "{source}: {:?}", parsed.errors);
        let formatted = format_checked(source, &parsed.program).expect("faithful formatting");
        assert!(formatted.contains(expected), "{source} -> {formatted}");
        let reparsed = parse(&formatted);
        assert!(program_eq_ignoring_spans(
            &parsed.program,
            &reparsed.program
        ));
    }
}

fn leaf(name: &str) -> Spanned<Expr> {
    (Expr::Ident(Ident::new(name)), 0..1)
}

#[test]
fn a_long_ast_spine_drops_on_a_small_stack() {
    // Every chain edge kind, alternating, 20,000 links deep, with an
    // optional chain and a `??` run that nest through both of their edges.
    on_stack(256 * 1024, || {
        let mut spine = leaf("x");
        for link in 0..20_000_u32 {
            let edge = Chain::new(spine);
            let expr = match link % 9 {
                0 => Expr::MethodCall {
                    receiver: edge,
                    method: (Ident::new("f"), 0..1),
                    args: Vec::new(),
                },
                1 => Expr::FieldAccess {
                    object: edge,
                    field: (Ident::new("f"), 0..1),
                },
                2 => Expr::Index {
                    object: edge,
                    index: Box::new(leaf("i")),
                },
                3 => Expr::Call {
                    function: edge,
                    type_args: None,
                    args: Vec::new(),
                    is_tail_call: false,
                },
                4 => Expr::PostfixTry(edge),
                5 => Expr::Cast {
                    expr: edge,
                    ty: (TypeExpr::Tuple(Vec::new()), 0..1),
                },
                6 => Expr::OptionalChain {
                    receiver: Chain::new(leaf("r")),
                    body: edge,
                },
                7 => Expr::OptionalChain {
                    receiver: edge,
                    body: Chain::new(leaf("b")),
                },
                _ => Expr::Coalesce {
                    left: Chain::new(leaf("l")),
                    right: edge,
                },
            };
            spine = (expr, 0..1);
        }
        let copy = spine.clone();
        assert!(copy == spine);
        drop(copy);
        drop(spine);
    });
}
