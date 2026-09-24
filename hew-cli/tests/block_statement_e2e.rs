//! A block-like form at the start of a statement ends at its `}` (D552).
//!
//! Every row here once changed meaning silently: `unsafe { 7 } - 1` returned
//! `-1`, `unsafe { dbl }(4)` returned `4`, `{ vv }[0]` built an array. Each
//! is now refused, and the parenthesized spelling of the same program runs
//! with the value the source meant.

mod support;

use support::{run_hew_in, strip_ansi, tempdir};

fn check(name: &str, source: &str) -> (bool, String) {
    let dir = tempdir();
    let path = dir.path().join(name);
    std::fs::write(&path, source).expect("fixture must be writable");
    let output = run_hew_in(dir.path(), &["check", path.to_str().expect("UTF-8 path")]);
    let rendered = strip_ansi(&format!(
        "{}{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    ));
    (output.status.success(), rendered)
}

fn run(name: &str, source: &str) -> String {
    let dir = tempdir();
    let path = dir.path().join(name);
    std::fs::write(&path, source).expect("fixture must be writable");
    let output = run_hew_in(dir.path(), &["run", path.to_str().expect("UTF-8 path")]);
    assert!(
        output.status.success(),
        "{name} must run:\n{}",
        strip_ansi(&String::from_utf8_lossy(&output.stderr))
    );
    String::from_utf8_lossy(&output.stdout).into_owned()
}

/// Same-line continuations: the token after `}` would have been an operator,
/// call or index on the block. Each is refused with the parentheses fix-it.
#[test]
fn same_line_continuations_after_a_statement_block_are_refused() {
    for (name, body) in [
        ("unsafe_minus.hew", "unsafe { 7 } - 1"),
        ("scope_minus.hew", "scope { 7 } - 1"),
        ("block_minus.hew", "{ g() } - 1"),
        (
            "handle_minus.hew",
            "scope within 1000ms { 5 } handle failure { 0 } - 1",
        ),
        ("unsafe_call.hew", "unsafe { dbl }(4)"),
        ("block_index.hew", "{ vv }[0]"),
        ("unsafe_star.hew", "unsafe { 7 } * 2"),
        ("unsafe_amp.hew", "unsafe { 6 } & 3"),
        ("unsafe_shift.hew", "unsafe { 6 } << 1"),
        ("if_minus.hew", "if true { 1 } else { 2 } - 5"),
        ("block_method.hew", "{ g() }.abs()"),
    ] {
        let source = format!(
            "fn g() -> i64 {{ 10 }}\nfn dbl(x: i64) -> i64 {{ x * 2 }}\nfn f() -> i64 {{\n    let vv: Vec<i64> = [3];\n    {body}\n}}\nfn main() {{ println(f()); }}\n"
        );
        let (ok, rendered) = check(name, &source);
        assert!(!ok, "{name} must be refused:\n{rendered}");
        assert!(
            rendered.contains("E_BLOCK_STATEMENT_OPERAND")
                && rendered.contains("wrap the block in parentheses"),
            "{name} must name the rule and its fix-it:\n{rendered}"
        );
    }
}

/// Next-line continuations start a new statement, so the block before them
/// is a statement whose value would be dropped. A non-unit value without a
/// `;` is refused; a `;` or a unit block is accepted.
#[test]
fn a_dropped_statement_block_value_is_refused() {
    for (name, body) in [
        ("next_minus.hew", "unsafe { 7 }\n    - 1"),
        ("next_tail.hew", "{ n }\n    .Ok(n)"),
        ("next_gen.hew", "gen { yield 5; }\n    .Ok(n)"),
    ] {
        let source = format!(
            "fn f(n: i64) -> Result<i64, string> {{\n    {body}\n}}\nfn main() {{ println(f(1).is_ok()); }}\n"
        );
        let (ok, rendered) = check(name, &source);
        assert!(!ok, "{name} must be refused:\n{rendered}");
        assert!(
            rendered.contains("E_BLOCK_STATEMENT_VALUE"),
            "{name} must name the dropped value:\n{rendered}"
        );
    }
}

/// The parenthesized spellings keep the value the source meant, a `;`
/// discards a block value on purpose, and a unit block before a dotted tail
/// is its own statement.
#[test]
fn parenthesized_and_terminated_statement_blocks_run() {
    let source = r#"fn g() -> i64 { 10 }
fn dbl(x: i64) -> i64 { x * 2 }
fn minus() -> i64 {
    (unsafe { 7 }) - 1
}
fn call() -> i64 {
    (unsafe { dbl })(4)
}
fn index() -> i64 {
    let vv: Vec<i64> = [3, 4];
    ({ vv })[1]
}
fn handled() -> i64 {
    (scope within 1000ms { 5 } handle failure { 0 }) - 1
}
fn discarded() -> i64 {
    unsafe { g() };
    { g() };
    2
}
fn tail() -> Result<i64, string> {
    unsafe { println("unit"); }
    .Ok(3)
}
fn map_len() -> i64 {
    {"a": 1, "b": 2}.len()
}
fn operand(s: string) -> i64 {
    let n = { s }.len();
    n
}
fn main() {
    println(minus());
    println(call());
    println(index());
    println(handled());
    println(discarded());
    println(tail().is_ok());
    println(map_len());
    println(operand("abc"));
}
"#;
    assert_eq!(
        run("accepted.hew", source),
        "6\n8\n4\n4\n2\nunit\ntrue\n2\n3\n"
    );
}
