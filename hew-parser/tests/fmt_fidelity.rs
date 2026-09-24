//! Formatter fidelity for each place a comment can sit, and negative
//! controls proving the oracle rejects moved comments, reordered members and
//! changed programs.

use hew_parser::fmt::fidelity::{check, FidelityError};
use hew_parser::fmt::format_source;
use hew_parser::parse;

/// Format `source` and require a faithful, fixed-point reprint.
fn assert_faithful(source: &str) {
    let parsed = parse(source);
    assert!(
        parsed.errors.is_empty(),
        "fixture must parse: {:?}",
        parsed.errors
    );
    let formatted = format_source(source, &parsed.program);
    if let Err(e) = check(source, &formatted) {
        panic!("{e}\n--- formatted ---\n{formatted}");
    }
    let again = format_source(&formatted, &parse(&formatted).program);
    assert_eq!(again, formatted, "formatting is not a fixed point");
}

#[test]
fn issue_repro_keeps_comment_in_main_and_hook_before_handler() {
    assert_faithful(
        "actor W { var n: i64, #[on(start)] fn started() { println(\"s\"); } receive fn boom() { panic(\"x\"); } }\n\
         fn main() { let w = spawn W(n: 1);\n    // note about the next line\n    let _ = w.boom(); }\n",
    );
}

#[test]
fn comments_between_call_arguments() {
    assert_faithful(
        r"fn f(a: i64, b: i64) -> i64 {
    a + b
}

fn main() {
    let x = f(
        1, // first
        // before second
        2,
    );
    println(x);
}
",
    );
}

#[test]
fn comments_between_array_elements() {
    assert_faithful(
        r"fn main() {
    let xs = [
        1, // one
        // two next
        2,
    ];
    println(xs.len());
}
",
    );
}

#[test]
fn comments_between_record_literal_fields() {
    assert_faithful(
        r"type P {
    x: i64,
    y: i64,
}

fn main() {
    let p = P {
        x: 1, // the x
        // the y
        y: 2,
    };
    println(p.x);
}
",
    );
}

#[test]
fn comments_around_match_arms() {
    assert_faithful(
        r#"fn main() {
    let v = 3;
    match v {
        // zero case
        0 => println("zero"), // trailing zero
        // other
        _ => {
            // inside arm
            println("other");
        }
    }
}
"#,
    );
}

#[test]
fn comments_before_closing_brace_and_between_items() {
    assert_faithful(
        r"fn main() {
    let a = 1;
    println(a);
    // dangling before close
}

// between items

fn other() {
    // only a comment
}
",
    );
}

#[test]
fn comment_inside_binary_expression() {
    assert_faithful(
        r"fn main() {
    let a = 1
        // continued
        + 2;
    println(a);
}
",
    );
}

#[test]
fn comments_around_else_branch() {
    assert_faithful(
        r#"fn main() {
    let a = 1;
    if a > 0 {
        println("pos");
    // about the else
    } else {
        // negative
        println("neg");
    }
}
"#,
    );
}

#[test]
fn comments_between_parameters() {
    assert_faithful(
        r"fn f(
    a: i64, // first param
    // second param
    b: i64,
) -> i64 {
    a + b
}

fn main() {
    println(f(1, 2));
}
",
    );
}

#[test]
fn comments_between_enum_variants() {
    assert_faithful(
        r#"enum Colour {
    // warm
    Red, // hot
    // cool
    Blue,
    // trailing inside
}

fn main() {
    let c = Colour.Red;
    match c {
        .Red => println("r"),
        .Blue => println("b"),
    }
}
"#,
    );
}

#[test]
fn comments_between_actor_members_and_attributes() {
    assert_faithful(
        r#"actor W {
    var n: i64, // count
    // hook next
    #[on(start)]
    // between attr and fn
    fn started() {
        println("s");
    }
    // handler
    receive fn boom() {
        panic("x");
    }
    // end of actor
}

fn main() {
    let w = spawn W(n: 1);
    // note about the next line
    let _ = w.boom();
}
"#,
    );
}

#[test]
fn comment_before_method_call() {
    assert_faithful(
        r"fn main() {
    let xs = [1, 2, 3];
    let n = xs
        // count them
        .len();
    println(n);
}
",
    );
}

#[test]
fn comment_inside_lambda_body() {
    assert_faithful(
        r"fn main() {
    let f = |x: i64| {
        // inside lambda
        x + 1
    };
    println(f(1));
}
",
    );
}

#[test]
fn comments_in_trait_and_impl_bodies() {
    assert_faithful(
        r#"trait Shape {
    // area first
    fn area(self) -> i64;
    // then name
    fn name(self) -> string;
    // end of trait
}

type Sq {
    s: i64,
}

impl Shape for Sq {
    // area impl
    fn area(self) -> i64 {
        self.s * self.s
    }

    fn name(self) -> string {
        "sq"
    }
    // end of impl
}

fn main() {
    let q = Sq { s: 2 };
    println(q.area());
}
"#,
    );
}

#[test]
fn block_comments_inside_expressions() {
    assert_faithful(
        r"fn main() {
    let a = /* inline */ 1;
    println(a /* after */);
}
",
    );
}

#[test]
fn comment_inside_let_else_block() {
    assert_faithful(
        r"fn main() {
    let x: Option<i64> = .Some(1);
    let .Some(v) = x else {
        // bail
        return;
    };
    println(v);
}
",
    );
}

#[test]
fn literal_and_attribute_spellings_survive() {
    assert_faithful(
        "#[max_heap(64 kb)]\nactor A {\n    receive fn go() {}\n}\n\n#[resource]\n#[opaque]\ntype H {}\n\n\
         fn main() {\n    let a = 0x04;\n    let b = 1_000;\n    let c = 3000ms;\n    let d = \"\\u{3000}\";\n    let e = 1.0e30;\n}\n",
    );
}

#[test]
fn bare_actor_field_and_bare_break_keep_their_spelling() {
    assert_faithful(
        "actor A {\n    name: string,\n    receive fn go() {}\n}\n\n\
         fn main() {\n    loop {\n        match 1 {\n            1 => break,\n            _ => continue,\n        }\n    }\n}\n",
    );
}

#[test]
fn doc_comments_keep_their_place_among_attributes() {
    assert_faithful(
        "//! Module doc.\n\n#[resource]\n/// Doc after the attribute.\ntype H {}\n\n\
         /// Doc before the attribute.\n#[test]\nfn t() {}\n\n\
         actor A {\n    #[on(start)]\n    /// Hook doc.\n    fn started() {}\n}\n",
    );
}

#[test]
fn comments_between_record_fields() {
    assert_faithful(
        "type Pair {\n    // the left side\n    left: i64,\n    /// The right side.\n    right: i64, // trailing\n    // end of record\n}\n\nfn main() {}\n",
    );
}

#[test]
fn comment_after_last_type_method() {
    assert_faithful(
        "type Counter {\n    n: i64,\n\n    fn get(c: Counter) -> i64 {\n        c.n\n    }\n    // after the last method\n}\n\nfn main() {}\n",
    );
}

#[test]
fn comments_inside_wire_declarations() {
    assert_faithful(
        "#[wire]\ntype Message {\n    // the id\n    id: i32 @1, // first\n    text: string @2,\n    // end of message\n}\n\n#[wire]\nenum Kind {\n    // plain\n    Plain,\n    Rich,\n    // end of kind\n}\n\nfn main() {}\n",
    );
}

// ── Negative controls ────────────────────────────────────────────────────

const ACTOR: &str = "actor W {\n    var n: i64,\n\n    // starts it\n    #[on(start)]\n    fn started() {}\n\n    receive fn boom() {}\n}\n";

#[test]
fn a_comment_moved_across_a_token_is_rejected() {
    let moved = "actor W {\n    var n: i64,\n\n    #[on(start)]\n    // starts it\n    fn started() {}\n\n    receive fn boom() {}\n}\n";
    assert!(matches!(
        check(ACTOR, moved),
        Err(FidelityError::TraceChanged { .. })
    ));
}

#[test]
fn reordered_members_are_rejected() {
    let reordered = "actor W {\n    var n: i64,\n\n    receive fn boom() {}\n\n    // starts it\n    #[on(start)]\n    fn started() {}\n}\n";
    assert!(matches!(
        check(ACTOR, reordered),
        Err(FidelityError::TraceChanged { .. })
    ));
}

#[test]
fn a_dropped_comment_is_rejected() {
    let dropped = "actor W {\n    var n: i64,\n\n    #[on(start)]\n    fn started() {}\n\n    receive fn boom() {}\n}\n";
    assert!(matches!(
        check(ACTOR, dropped),
        Err(FidelityError::TraceChanged { .. })
    ));
}

#[test]
fn a_trailing_comment_moved_to_its_own_line_is_rejected() {
    let source = "fn main() {\n    let a = 1; // one\n    let b = 2;\n}\n";
    let own_line = "fn main() {\n    let a = 1;\n    // one\n    let b = 2;\n}\n";
    assert!(matches!(
        check(source, own_line),
        Err(FidelityError::TraceChanged { .. })
    ));
}

#[test]
fn a_comment_moved_out_of_a_block_is_rejected() {
    let source = "fn a() {\n    f();\n    // last\n}\n\nfn b() {}\n";
    let moved = "fn a() {\n    f();\n}\n\n// last\nfn b() {}\n";
    assert!(matches!(
        check(source, moved),
        Err(FidelityError::TraceChanged { .. })
    ));
}

#[test]
fn a_respelled_literal_is_rejected() {
    let source = "fn main() {\n    let a = 0x04;\n}\n";
    let respelled = "fn main() {\n    let a = 4;\n}\n";
    assert!(matches!(
        check(source, respelled),
        Err(FidelityError::TraceChanged { .. })
    ));
}

#[test]
fn a_changed_program_is_rejected() {
    let source = "fn main() {\n    let a = 1;\n}\n";
    let changed = "fn main() {\n    let a = 2;\n}\n";
    assert_eq!(check(source, changed), Err(FidelityError::AstChanged));
}

#[test]
fn output_that_does_not_parse_is_rejected() {
    let source = "fn main() {}\n";
    assert!(matches!(
        check(source, "fn main( {}\n"),
        Err(FidelityError::Reparse(_))
    ));
}

#[test]
fn only_the_record_base_may_move() {
    // D488: base-first is the one written spelling of a record literal.
    let source =
        "type P {\n    x: i64,\n    y: i64,\n}\n\nfn f(b: P) -> P {\n    P { x: 1, ..b }\n}\n";
    let hoisted =
        "type P {\n    x: i64,\n    y: i64,\n}\n\nfn f(b: P) -> P {\n    P { ..b, x: 1 }\n}\n";
    assert_eq!(check(source, hoisted), Ok(()));
    let source =
        "type P {\n    x: i64,\n    y: i64,\n}\n\nfn f(b: P) -> P {\n    P { x: 1, y: 2 }\n}\n";
    let swapped =
        "type P {\n    x: i64,\n    y: i64,\n}\n\nfn f(b: P) -> P {\n    P { y: 2, x: 1 }\n}\n";
    assert!(check(source, swapped).is_err(), "only the base may move");
}
