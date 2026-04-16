/// Formatter coverage tests — round-trip parse → format → verify output.
///
/// Each test parses Hew source, formats via `format_source`, and checks that
/// the result is idempotent (formatting already-formatted code is a no-op).
use hew_parser::ast::{Expr, Item, Stmt};
use hew_parser::fmt::{format_program, format_source};
use hew_parser::parse;

// -----------------------------------------------------------------------
// Helpers
// -----------------------------------------------------------------------

/// Parse `source`, assert no errors, format with comment preservation,
/// then re-parse and re-format to verify idempotency.  Returns the
/// formatted string so callers can make additional assertions.
fn roundtrip(source: &str) -> String {
    let r1 = parse(source);
    assert!(
        r1.errors.is_empty(),
        "Initial parse failed: {:?}",
        r1.errors
    );

    let formatted = format_source(source, &r1.program);

    // Re-parse the formatted output — it must still be valid.
    let r2 = parse(&formatted);
    assert!(
        r2.errors.is_empty(),
        "Re-parse of formatted output failed: {:?}\nFormatted:\n{formatted}",
        r2.errors,
    );

    // Idempotency: formatting the already-formatted output must be identical.
    let formatted2 = format_source(&formatted, &r2.program);
    assert_eq!(
        formatted, formatted2,
        "Formatter is not idempotent.\nFirst:\n{formatted}\nSecond:\n{formatted2}",
    );

    formatted
}

/// Like `roundtrip` but uses `format_program` (no comment preservation).
fn roundtrip_no_comments(source: &str) -> String {
    let r1 = parse(source);
    assert!(
        r1.errors.is_empty(),
        "Initial parse failed: {:?}",
        r1.errors
    );

    let formatted = format_program(&r1.program);

    let r2 = parse(&formatted);
    assert!(
        r2.errors.is_empty(),
        "Re-parse of formatted output failed: {:?}\nFormatted:\n{formatted}",
        r2.errors,
    );

    let formatted2 = format_program(&r2.program);
    assert_eq!(
        formatted, formatted2,
        "format_program is not idempotent.\nFirst:\n{formatted}\nSecond:\n{formatted2}",
    );

    formatted
}

fn exact_roundtrip(source: &str) {
    let formatted = roundtrip_no_comments(source);
    assert_eq!(
        formatted, source,
        "Formatter output changed.
Expected:
{source}
Actual:
{formatted}",
    );
}

// -----------------------------------------------------------------------
// Functions
// -----------------------------------------------------------------------

#[test]
fn fmt_function_multiple_statements() {
    let src = r"fn compute(x: i32) -> i32 {
    let y = x * 2;
    let z = y + 1;
    z
}";
    let out = roundtrip(src);
    assert!(out.contains("let y = x * 2;"), "output: {out}");
    assert!(out.contains("let z = y + 1;"), "output: {out}");
}

#[test]
fn fmt_generic_function_with_bounds() {
    let src = "fn print_it<T: Display>(x: T) { println(x); }";
    let out = roundtrip(src);
    assert!(out.contains("<T: Display>"), "output: {out}");
}

#[test]
fn fmt_pure_function() {
    let src = r"pure fn checksum(x: i32) -> i32 {
    x
}";
    let out = roundtrip(src);
    assert!(out.contains("pure fn checksum"), "output: {out}");
}

// -----------------------------------------------------------------------
// If / else
// -----------------------------------------------------------------------

#[test]
fn fmt_if_else() {
    let src = r"fn main() {
    if x > 0 {
        println(x);
    } else {
        println(0);
    }
}";
    let out = roundtrip(src);
    assert!(out.contains("} else {"), "else on same line. output: {out}");
}

#[test]
fn fmt_if_else_if_chain() {
    let src = r"fn check(x: i32) {
    if x > 10 {
        println(1);
    } else if x > 0 {
        println(2);
    } else {
        println(3);
    }
}";
    let out = roundtrip(src);
    assert!(out.contains("} else if x > 0 {"), "output: {out}");
    assert!(out.contains("} else {"), "output: {out}");
}

#[test]
fn fmt_if_expression() {
    let src = r"fn main() -> i32 {
    let val = if true { 1 } else { 0 };
    val
}";
    let out = roundtrip(src);
    assert!(out.contains("if true {"), "output: {out}");
}

#[test]
fn fmt_if_let() {
    let src = r"fn main() {
    if let Some(x) = maybe {
        println(x);
    }
}";
    let out = roundtrip(src);
    assert!(out.contains("if let Some(x) = maybe"), "output: {out}");
}

// -----------------------------------------------------------------------
// For loops
// -----------------------------------------------------------------------

#[test]
fn fmt_for_loop() {
    let src = r"fn main() {
    for i in 0 .. 10 {
        println(i);
    }
}";
    let out = roundtrip(src);
    assert!(out.contains("for i in 0 .. 10 {"), "output: {out}");
}

#[test]
fn fmt_for_with_pattern() {
    let src = r"fn main() {
    for (k, v) in items {
        println(k);
    }
}";
    let out = roundtrip(src);
    assert!(out.contains("for (k, v) in items"), "output: {out}");
}

// -----------------------------------------------------------------------
// While loops
// -----------------------------------------------------------------------

#[test]
fn fmt_while_loop() {
    let src = r"fn main() {
    var x = 10;
    while x > 0 {
        x = x - 1;
    }
}";
    let out = roundtrip(src);
    assert!(out.contains("while x > 0 {"), "output: {out}");
}

// -----------------------------------------------------------------------
// Loop / break / continue
// -----------------------------------------------------------------------

#[test]
fn fmt_loop_with_break() {
    let src = r"fn main() {
    var i = 0;
    loop {
        if i > 5 {
            break;
        }
        i = i + 1;
    }
}";
    let out = roundtrip(src);
    assert!(out.contains("loop {"), "output: {out}");
    assert!(out.contains("break;"), "output: {out}");
}

#[test]
fn fmt_continue_statement() {
    let src = r"fn main() {
    for i in 0..10 {
        if i == 3 {
            continue;
        }
        println(i);
    }
}";
    let out = roundtrip(src);
    assert!(out.contains("continue;"), "output: {out}");
}

// -----------------------------------------------------------------------
// Match expressions
// -----------------------------------------------------------------------

#[test]
fn fmt_match_statement() {
    let src = r"fn describe(x: i32) {
    match x {
        0 => println(0),
        1 => println(1),
        _ => println(x),
    }
}";
    let out = roundtrip(src);
    assert!(out.contains("match x {"), "output: {out}");
    assert!(out.contains("0 => println(0),"), "output: {out}");
    assert!(out.contains("_ => println(x),"), "output: {out}");
}

#[test]
fn fmt_match_with_enum_patterns() {
    let src = r"enum Colour {
    Red;
    Green;
    Blue;
}

fn name(c: Colour) -> i32 {
    match c {
        Colour::Red => 1,
        Colour::Green => 2,
        Colour::Blue => 3,
    }
}";
    let out = roundtrip(src);
    assert!(out.contains("Colour::Red => 1,"), "output: {out}");
}

#[test]
fn fmt_match_with_guard() {
    let src = r"fn classify(x: i32) -> i32 {
    match x {
        n if n > 100 => 3,
        n if n > 0 => 2,
        _ => 1,
    }
}";
    let out = roundtrip(src);
    assert!(out.contains("n if n > 100 => 3,"), "output: {out}");
}

// -----------------------------------------------------------------------
// Struct definitions (type keyword in Hew)
// -----------------------------------------------------------------------

#[test]
fn fmt_struct_definition() {
    let src = r"type Point {
    x: f64;
    y: f64;
}";
    let out = roundtrip(src);
    assert!(out.contains("type Point {"), "output: {out}");
    assert!(out.contains("    x: f64;"), "output: {out}");
    assert!(out.contains("    y: f64;"), "output: {out}");
}

#[test]
fn fmt_generic_struct() {
    let src = r"type Pair<A, B> {
    first: A;
    second: B;
}";
    let out = roundtrip(src);
    assert!(out.contains("type Pair<A, B> {"), "output: {out}");
}

// -----------------------------------------------------------------------
// Enum definitions
// -----------------------------------------------------------------------

#[test]
fn fmt_enum_definition() {
    let src = r"enum Direction {
    North;
    South;
    East;
    West;
}";
    let out = roundtrip(src);
    assert!(out.contains("enum Direction {"), "output: {out}");
    assert!(out.contains("    North;"), "output: {out}");
}

#[test]
fn fmt_enum_with_tuple_variant() {
    let src = r"enum Shape {
    Circle(f64);
    Rectangle(f64, f64);
}";
    let out = roundtrip(src);
    assert!(out.contains("Circle(f64);"), "output: {out}");
    assert!(out.contains("Rectangle(f64, f64);"), "output: {out}");
}

#[test]
fn fmt_enum_with_struct_variant() {
    let src = r"enum Event {
    Click { x: i32, y: i32 };
    KeyPress { code: i32 };
}";
    let out = roundtrip(src);
    assert!(out.contains("Click { x: i32, y: i32 }"), "output: {out}");
}

// -----------------------------------------------------------------------
// Trait definitions
// -----------------------------------------------------------------------

#[test]
fn fmt_trait_definition() {
    let src = r"trait Greet {
    fn hello() -> i32;
}";
    let out = roundtrip(src);
    assert!(out.contains("trait Greet {"), "output: {out}");
    assert!(out.contains("fn hello() -> i32;"), "output: {out}");
}

#[test]
fn fmt_trait_with_default_method() {
    let src = r"trait Describable {
    fn describe() -> i32 {
        42
    }
}";
    let out = roundtrip(src);
    assert!(out.contains("trait Describable {"), "output: {out}");
    assert!(out.contains("fn describe() -> i32 {"), "output: {out}");
}

#[test]
fn fmt_trait_with_supertrait() {
    let src = r"trait Printable: Display {
    fn output();
}";
    let out = roundtrip(src);
    assert!(out.contains("trait Printable: Display"), "output: {out}");
}

// -----------------------------------------------------------------------
// Impl blocks
// -----------------------------------------------------------------------

#[test]
fn fmt_impl_block() {
    let src = r"type Counter {
    value: i32;
}

impl Counter {
    fn new() -> Counter {
        Counter(value: 0)
    }

    fn get(c: Counter) -> i32 {
        c.value
    }
}";
    let out = roundtrip(src);
    assert!(out.contains("impl Counter {"), "output: {out}");
    assert!(out.contains("fn new() -> Counter"), "output: {out}");
    assert!(out.contains("fn get(c: Counter) -> i32"), "output: {out}");
}

#[test]
fn fmt_trait_impl() {
    let src = r#"type MyType {
    val: i32;
}

impl Display for MyType {
    fn to_string(t: MyType) -> String {
        f"{t.val}"
    }
}"#;
    let out = roundtrip(src);
    assert!(out.contains("impl Display for MyType"), "output: {out}");
}

#[test]
fn fmt_generic_impl() {
    let src = r"type Wrapper<T> {
    inner: T;
}

impl<T> Wrapper<T> {
    fn unwrap(w: Wrapper<T>) -> T {
        w.inner
    }
}";
    let out = roundtrip(src);
    assert!(out.contains("impl<T> Wrapper<T>"), "output: {out}");
}

// -----------------------------------------------------------------------
// Actors
// -----------------------------------------------------------------------

#[test]
fn fmt_simple_actor() {
    let src = r"actor Counter {
    let count: i32;

    receive fn increment(n: i32) -> i32 {
        count + n
    }
}";
    let out = roundtrip(src);
    assert!(out.contains("actor Counter {"), "output: {out}");
    assert!(out.contains("let count: i32;"), "output: {out}");
    assert!(out.contains("receive fn increment"), "output: {out}");
}

#[test]
fn fmt_actor_receive_without_preamble() {
    let src = r"actor Worker {
    receive fn work(n: i32) -> i32 {
        n
    }
}";
    let out = roundtrip(src);
    assert!(
        out.contains("actor Worker {\n    receive fn work(n: i32) -> i32 {"),
        "output: {out}"
    );
}

#[test]
fn fmt_actor_with_mailbox() {
    let src = r"actor Worker {
    let id: i32;

    mailbox 16;

    receive fn process(value: i32) {
        println(value);
    }
}";
    let out = roundtrip(src);
    assert!(out.contains("mailbox 16;"), "output: {out}");
}

#[test]
fn fmt_actor_pure_method_without_preamble() {
    let src = r"actor Counter {
    pure fn current() -> i32 {
        0
    }
}";
    let out = roundtrip(src);
    assert!(
        out.contains("actor Counter {\n    pure fn current() -> i32 {"),
        "output: {out}"
    );
}

// -----------------------------------------------------------------------
// Closures / lambdas
// -----------------------------------------------------------------------

#[test]
fn fmt_lambda_with_types() {
    let src = "fn main() { let f = (x: i32) -> i32 => x + 1; }";
    let out = roundtrip(src);
    assert!(out.contains("(x: i32) -> i32 => x + 1"), "output: {out}");
}

#[test]
fn fmt_multi_param_lambda() {
    let src = "fn main() { let add = (a, b) => a + b; }";
    let out = roundtrip(src);
    assert!(out.contains("(a, b) => a + b"), "output: {out}");
}

#[test]
fn fmt_generic_lambda_type_params() {
    let src = "fn main() { let id = <T: Display>(x: T) -> T => x; }";
    let out = roundtrip(src);
    assert!(
        out.contains("<T: Display>(x: T) -> T => x"),
        "output: {out}"
    );
}

// -----------------------------------------------------------------------
// Method calls and chaining
// -----------------------------------------------------------------------

#[test]
fn fmt_chained_method_calls() {
    let src = "fn main() { list.push(1).push(2).push(3); }";
    let out = roundtrip(src);
    assert!(
        out.contains("list.push(1).push(2).push(3)"),
        "output: {out}"
    );
}

// -----------------------------------------------------------------------
// String interpolation
// -----------------------------------------------------------------------

#[test]
fn fmt_string_interpolation() {
    let src = r#"fn main() { let msg = f"value is {x}"; }"#;
    let out = roundtrip(src);
    assert!(out.contains(r#"f"value is {x}""#), "output: {out}");
}

#[test]
fn fmt_string_interpolation_multiple() {
    let src = r#"fn main() { let msg = f"{a} plus {b} equals {c}"; }"#;
    let out = roundtrip(src);
    assert!(
        out.contains(r#"f"{a} plus {b} equals {c}""#),
        "output: {out}"
    );
}

// -----------------------------------------------------------------------
// Import statements
// -----------------------------------------------------------------------

#[test]
fn fmt_import_path() {
    let src = "import std::io;";
    let out = roundtrip(src);
    assert!(out.contains("import std::io;"), "output: {out}");
}

#[test]
fn fmt_import_glob() {
    let src = "import std::collections::*;";
    let out = roundtrip(src);
    assert!(out.contains("import std::collections::*;"), "output: {out}");
}

#[test]
fn fmt_import_named() {
    let src = "import std::{println, print};";
    let out = roundtrip(src);
    assert!(
        out.contains("import std::{println, print};"),
        "output: {out}"
    );
}

#[test]
fn fmt_import_file() {
    let src = r#"import "helpers.hew";"#;
    let out = roundtrip(src);
    assert!(out.contains(r#"import "helpers.hew";"#), "output: {out}");
}

// -----------------------------------------------------------------------
// Return statement
// -----------------------------------------------------------------------

#[test]
fn fmt_return_statement() {
    let src = "fn early(x: i32) -> i32 { if x < 0 { return 0; } x }";
    let out = roundtrip(src);
    assert!(out.contains("return 0;"), "output: {out}");
}

// -----------------------------------------------------------------------
// Assignment / compound assignment
// -----------------------------------------------------------------------

#[test]
fn fmt_assignment() {
    let src = "fn main() { var x = 1; x = 2; }";
    let out = roundtrip(src);
    assert!(out.contains("x = 2;"), "output: {out}");
}

#[test]
fn fmt_compound_assignment() {
    let src = "fn main() { var x = 0; x += 10; }";
    let out = roundtrip(src);
    assert!(out.contains("x += 10;"), "output: {out}");
}

// -----------------------------------------------------------------------
// Binary / unary operators
// -----------------------------------------------------------------------

#[test]
fn fmt_binary_operators() {
    let src = "fn main() { let r = a + b * c - d / e; }";
    let out = roundtrip(src);
    // Formatter should preserve operator precedence correctly.
    assert!(out.contains("a + b * c - d / e"), "output: {out}");
}

#[test]
fn fmt_comparison_operators() {
    let src = "fn main() { let r = x >= 0 && x < 100; }";
    let out = roundtrip(src);
    assert!(out.contains("x >= 0 && x < 100"), "output: {out}");
}

// -----------------------------------------------------------------------
// Array / tuple / map literals
// -----------------------------------------------------------------------

#[test]
fn fmt_array_literal() {
    let src = "fn main() { let arr = [1, 2, 3]; }";
    let out = roundtrip(src);
    assert!(out.contains("[1, 2, 3]"), "output: {out}");
}

#[test]
fn fmt_tuple_literal() {
    let src = "fn main() { let t = (1, 2, 3); }";
    let out = roundtrip(src);
    assert!(out.contains("(1, 2, 3)"), "output: {out}");
}

// -----------------------------------------------------------------------
// Struct initialisation
// -----------------------------------------------------------------------

#[test]
fn fmt_struct_init() {
    let src = r"type Point { x: i32; y: i32; }

fn main() {
    let p = Point(x: 1, y: 2);
}";
    let out = roundtrip(src);
    assert!(out.contains("Point(x: 1, y: 2)"), "output: {out}");
}

// -----------------------------------------------------------------------
// Index expressions
// -----------------------------------------------------------------------

#[test]
fn fmt_index_expr() {
    let src = "fn main() { let v = arr[0]; }";
    let out = roundtrip(src);
    assert!(out.contains("arr[0]"), "output: {out}");
}

// -----------------------------------------------------------------------
// Range expressions
// -----------------------------------------------------------------------

#[test]
fn fmt_range_expression() {
    let src = "fn main() { for i in 0 .. 10 { println(i); } }";
    let out = roundtrip(src);
    assert!(out.contains("0 .. 10"), "output: {out}");
}

// -----------------------------------------------------------------------
// Cast expressions
// -----------------------------------------------------------------------

#[test]
fn fmt_cast_expression() {
    let src = "fn main() { let x = 42 as f64; }";
    let out = roundtrip(src);
    assert!(out.contains("42 as f64"), "output: {out}");
}

// -----------------------------------------------------------------------
// Spawn / await
// -----------------------------------------------------------------------

#[test]
fn fmt_spawn_expression() {
    let src = r"actor Worker {
    receive fn work(n: i32) -> i32 { n }
}

fn main() {
    let w = spawn Worker;
}";
    let out = roundtrip(src);
    assert!(out.contains("spawn Worker"), "output: {out}");
}

#[test]
fn fmt_await_expression() {
    let src = r"async fn fetch() -> i32 { 42 }

fn main() {
    let f = fetch();
    let r = await f;
}";
    let out = roundtrip(src);
    assert!(out.contains("await f"), "output: {out}");
}

// -----------------------------------------------------------------------
// Comments preservation
// -----------------------------------------------------------------------

#[test]
fn fmt_preserves_line_comments() {
    let src = r"// This is a comment
fn main() {
    // Inner comment
    let x = 1;
}";
    let out = roundtrip(src);
    assert!(
        out.contains("// This is a comment"),
        "Line comment lost. output: {out}"
    );
    assert!(
        out.contains("// Inner comment"),
        "Inner comment lost. output: {out}"
    );
}

#[test]
fn fmt_preserves_block_comments() {
    let src = r"/* Block comment */
fn main() { let x = 1; }";
    let out = roundtrip(src);
    assert!(
        out.contains("/* Block comment */"),
        "Block comment lost. output: {out}"
    );
}

// -----------------------------------------------------------------------
// Indentation & nesting
// -----------------------------------------------------------------------

#[test]
fn fmt_nested_blocks_indentation() {
    let src = r"fn main() {
    if true {
        if false {
            println(1);
        }
    }
}";
    let out = roundtrip(src);
    // Inner println should be indented 12 spaces (3 levels × 4 spaces).
    assert!(
        out.contains("            println(1);"),
        "Nested indentation incorrect. output: {out}"
    );
}

#[test]
fn fmt_uses_four_space_indent() {
    let src = "fn main() { let x = 1; }";
    let out = roundtrip(src);
    // The let statement inside the block should be indented with 4 spaces.
    assert!(
        out.contains("    let x = 1;"),
        "Expected 4-space indent. output: {out}"
    );
}

// -----------------------------------------------------------------------
// Idempotency on well-formatted code
// -----------------------------------------------------------------------

#[test]
fn fmt_already_formatted_is_idempotent() {
    // This code is already well-formatted; the formatter should not change it.
    let src = r"fn fibonacci(n: i32) -> i32 {
    if n <= 1 {
        n
    } else {
        fibonacci(n - 1) + fibonacci(n - 2)
    }
}
";
    let out = roundtrip(src);
    assert_eq!(src, out, "Well-formatted code should be unchanged");
}

// -----------------------------------------------------------------------
// format_program vs format_source
// -----------------------------------------------------------------------

#[test]
fn fmt_format_program_strips_comments() {
    let src = r"// This comment should be stripped
fn main() {
    // So should this
    let x = 1;
}";
    let out = roundtrip_no_comments(src);
    assert!(
        !out.contains("// This comment"),
        "format_program should strip comments. output: {out}"
    );
    // But the code should still be there.
    assert!(out.contains("fn main()"), "output: {out}");
    assert!(out.contains("let x = 1;"), "output: {out}");
}

// -----------------------------------------------------------------------
// Defer statement
// -----------------------------------------------------------------------

#[test]
fn fmt_defer_statement() {
    let src = "fn main() { defer cleanup(); }";
    let out = roundtrip(src);
    assert!(out.contains("defer cleanup();"), "output: {out}");
}

// -----------------------------------------------------------------------
// Block expression (trailing expression)
// -----------------------------------------------------------------------

#[test]
fn fmt_block_trailing_expression() {
    let src = r"fn compute() -> i32 {
    let x = 1;
    let y = 2;
    x + y
}";
    let out = roundtrip(src);
    // The trailing expression (x + y) should appear without a semicolon.
    assert!(out.contains("    x + y\n"), "output: {out}");
}

// -----------------------------------------------------------------------
// Extern block
// -----------------------------------------------------------------------

#[test]
fn fmt_extern_block() {
    let src = r#"extern "C" {
    fn puts(s: CString) -> i32;
}"#;
    let out = roundtrip(src);
    assert!(out.contains(r#"extern "C" {"#), "output: {out}");
    assert!(out.contains("fn puts(s: CString) -> i32;"), "output: {out}");
}

// -----------------------------------------------------------------------
// Visibility modifiers
// -----------------------------------------------------------------------

#[test]
fn fmt_pub_function_in_impl() {
    // Visibility on impl methods is not supported inside impl bodies.
    // Test pub visibility on top-level functions instead.
    let src = r"pub fn helper() -> i32 {
    42
}

fn internal() -> i32 {
    0
}
";
    let out = roundtrip(src);
    assert!(out.contains("pub fn helper"), "output: {out}");
    assert!(out.contains("fn internal"), "output: {out}");
}

// -----------------------------------------------------------------------
// Named call arguments
// -----------------------------------------------------------------------

#[test]
fn fmt_named_arguments() {
    let src = r"type Pt { x: i32; y: i32; }

fn main() {
    let p = Pt(x: 10, y: 20);
}";
    let out = roundtrip(src);
    assert!(out.contains("Pt(x: 10, y: 20)"), "output: {out}");
}

// -----------------------------------------------------------------------
// Complex / combined constructs
// -----------------------------------------------------------------------

#[test]
fn fmt_full_program() {
    // A small but complete programme exercising multiple constructs.
    let src = r"const MAX: i32 = 100;

type Point {
    x: f64;
    y: f64;
}

impl Point {
    fn origin() -> Point {
        Point(x: 0.0, y: 0.0)
    }
}

fn main() {
    var p = Point::origin();
    for i in 0..MAX {
        println(i);
    }
}
";
    let out = roundtrip(src);
    assert!(out.contains("const MAX: i32 = 100;"), "output: {out}");
    assert!(out.contains("type Point {"), "output: {out}");
    assert!(out.contains("impl Point {"), "output: {out}");
    assert!(out.contains("fn main()"), "output: {out}");
}

#[test]
fn fmt_enum_and_match_combined() {
    let src = r"enum Colour {
    Red;
    Green;
    Blue;
}

fn to_hex(c: Colour) -> i32 {
    match c {
        Colour::Red => 1,
        Colour::Green => 2,
        Colour::Blue => 3,
    }
}
";
    let out = roundtrip(src);
    assert_eq!(src, out, "Well-formatted combined code should be unchanged");
}

// -----------------------------------------------------------------------
// Mutable parameter
// -----------------------------------------------------------------------

#[test]
fn fmt_mutable_parameter() {
    let src = "fn modify(var x: i32) -> i32 { x = x + 1; x }";
    let out = roundtrip(src);
    assert!(out.contains("var x: i32"), "output: {out}");
}

// -----------------------------------------------------------------------
// Attributes
// -----------------------------------------------------------------------

#[test]
fn fmt_function_attribute() {
    let src = r"#[inline]
fn fast() -> i32 {
    42
}";
    let out = roundtrip(src);
    assert!(out.contains("#[inline]"), "output: {out}");
}

// -----------------------------------------------------------------------
// Bool / string / char literals
// -----------------------------------------------------------------------

#[test]
fn fmt_bool_literals() {
    let src = "fn main() { let a = true; let b = false; }";
    let out = roundtrip(src);
    assert!(out.contains("true"), "output: {out}");
    assert!(out.contains("false"), "output: {out}");
}

#[test]
fn fmt_string_literal() {
    let src = r#"fn main() { let s = "hello world"; }"#;
    let out = roundtrip(src);
    assert!(out.contains(r#""hello world""#), "output: {out}");
}

#[test]
fn fmt_char_literal() {
    let src = "fn main() { let c = 'a'; }";
    let out = roundtrip(src);
    assert!(out.contains("'a'"), "output: {out}");
}

#[test]
fn fmt_byte_string_literal_escapes() {
    let src = r#"fn main() { let bytes = b"quote\"slash\\line\n"; }"#;
    let out = roundtrip(src);
    assert!(out.contains(r#"b"quote\"slash\\line\n""#), "output: {out}");
}

#[test]
fn fmt_regex_literal_escapes_delimiters() {
    let mut parsed = parse(r#"fn main() { let rx = re"ok"; }"#);
    assert!(
        parsed.errors.is_empty(),
        "Initial parse failed: {:?}",
        parsed.errors
    );

    let Item::Function(func) = &mut parsed.program.items[0].0 else {
        panic!("expected function");
    };
    let Stmt::Let {
        value: Some((Expr::RegexLiteral(pattern), _)),
        ..
    } = &mut func.body.stmts[0].0
    else {
        panic!("expected let with regex literal");
    };
    *pattern = "a\"b\\".to_string();

    let formatted = format_program(&parsed.program);
    assert!(formatted.contains(r#"re"a\"b\\""#), "output: {formatted}");

    let reparsed = parse(&formatted);
    assert!(
        reparsed.errors.is_empty(),
        "Re-parse of formatted output failed: {:?}\nFormatted:\n{formatted}",
        reparsed.errors,
    );

    let reformatted = format_program(&reparsed.program);
    assert_eq!(
        formatted, reformatted,
        "format_program is not idempotent.\nFirst:\n{formatted}\nSecond:\n{reformatted}",
    );
}

// -----------------------------------------------------------------------
// Numeric literals
// -----------------------------------------------------------------------

#[test]
fn fmt_integer_literal() {
    let src = "fn main() { let x = 42; }";
    let out = roundtrip(src);
    assert!(out.contains("42"), "output: {out}");
}

#[test]
fn fmt_float_literal() {
    let src = "fn main() { let x = 3.14; }";
    let out = roundtrip(src);
    assert!(out.contains("3.14"), "output: {out}");
}

// -----------------------------------------------------------------------
// Parenthesised expressions
// -----------------------------------------------------------------------

#[test]
fn fmt_parenthesised_expression() {
    let src = "fn main() { let x = (a + b) * c; }";
    let out = roundtrip(src);
    assert!(out.contains("(a + b) * c"), "output: {out}");
}

// -----------------------------------------------------------------------
// Multiple items separated by blank lines
// -----------------------------------------------------------------------

#[test]
fn fmt_items_separated_by_blank_lines() {
    let src = r"fn foo() {
}

fn bar() {
}
";
    let out = roundtrip(src);
    // Items should be separated by a blank line.
    assert!(out.contains("}\n\nfn bar"), "output: {out}");
}

// -----------------------------------------------------------------------
// Generator function
// -----------------------------------------------------------------------

#[test]
fn fmt_generator_function() {
    let src = r"gen fn counting(n: i32) -> i32 {
    for i in 0..n {
        yield i;
    }
}";
    let out = roundtrip(src);
    assert!(out.contains("gen fn counting"), "output: {out}");
    assert!(out.contains("yield i;"), "output: {out}");
}

// -----------------------------------------------------------------------
// Where clause
// -----------------------------------------------------------------------

#[test]
fn fmt_where_clause() {
    let src = r"fn process<T>(x: T) -> i32 where T: Display {
    42
}";
    let out = roundtrip(src);
    assert!(out.contains("where T: Display"), "output: {out}");
}

#[test]
fn fmt_map_literal_roundtrip() {
    exact_roundtrip("fn main() {\n    let m = {\"a\": 1, \"b\": 2};\n}\n");
}

#[test]
fn fmt_array_repeat_roundtrip() {
    exact_roundtrip("fn main() {\n    let values = [0; 10];\n}\n");
}

#[test]
fn fmt_scope_launch_roundtrip() {
    exact_roundtrip(
        "fn main() {\n    scope |s| {\n        let task = s.launch {\n            1\n        };\n    };\n}\n",
    );
}

#[test]
fn fmt_scope_spawn_roundtrip() {
    exact_roundtrip(
        "fn main() {\n    scope |s| {\n        s.spawn {\n            println(1);\n        };\n    };\n}\n",
    );
}

#[test]
fn fmt_scope_cancel_roundtrip() {
    exact_roundtrip("fn main() {\n    scope |s| {\n        s.cancel();\n    };\n}\n");
}

#[test]
fn fmt_cooperate_roundtrip() {
    exact_roundtrip("fn main() {\n    cooperate;\n}\n");
}

#[test]
fn fmt_spawn_lambda_actor_roundtrip() {
    exact_roundtrip("fn main() {\n    let worker = spawn (x: int) => x + 1;\n}\n");
}

#[test]
fn fmt_postfix_try_roundtrip() {
    exact_roundtrip("fn main() {\n    let value = result?;\n}\n");
}

#[test]
fn fmt_select_roundtrip() {
    exact_roundtrip(
        "fn main() {\n    let value = select {\n        msg from inbox.recv() => msg,\n        after 100ms => -1,\n    };\n}\n",
    );
}

#[test]
fn fmt_join_roundtrip() {
    exact_roundtrip(
        "fn main() {\n    let pair = join {\n        left(),\n        right(),\n    };\n}\n",
    );
}

#[test]
fn fmt_timeout_roundtrip() {
    exact_roundtrip("fn main() {\n    let value = await task | after 5s;\n}\n");
}

#[test]
fn fmt_send_roundtrip() {
    exact_roundtrip("fn main() {\n    worker <- 42;\n}\n");
}

#[test]
fn fmt_while_let_roundtrip() {
    exact_roundtrip(
        "fn main() {\n    while let Some(x) = iter.next() {\n        println(x);\n    }\n}\n",
    );
}

#[test]
fn fmt_type_alias_roundtrip() {
    exact_roundtrip("type Alias = i32;\n");
}

#[test]
fn fmt_wire_declarations_roundtrip() {
    exact_roundtrip(
        "#[wire]\nstruct Message {\n    id: i32 @1,\n}\n\nwire enum Command {\n    Start;\n    Stop;\n}\n",
    );
}

#[test]
fn fmt_machine_decl_roundtrip() {
    exact_roundtrip(
        "machine Light {\n    state Off;\n    state On;\n\n    event Toggle;\n\n    on Toggle: Off -> On;\n    on Toggle: On -> Off;\n}\n",
    );
}

#[test]
fn fmt_supervisor_decl_roundtrip() {
    exact_roundtrip(
        "supervisor Pool {\n    strategy: one_for_one;\n    max_restarts: 5;\n    window: 30;\n\n    child worker: Worker(1);\n}\n",
    );
}

#[test]
fn fmt_duration_literals_roundtrip() {
    exact_roundtrip(
        "fn main() {\n    let short = 100ms;\n    let medium = 5s;\n    let long = 1m;\n}\n",
    );
}

#[test]
fn fmt_integer_radix_literals_roundtrip() {
    exact_roundtrip(
        "fn main() {\n    let hex = 0xFF;\n    let octal = 0o77;\n    let binary = 0b1010;\n}\n",
    );
}

#[test]
fn fmt_type_infer_roundtrip() {
    exact_roundtrip("fn main() {\n    let x: _ = 42;\n}\n");
}

#[test]
fn fmt_pattern_or_roundtrip() {
    exact_roundtrip("fn main() {\n    match x {\n        1 | 2 => 3,\n        _ => 0,\n    }\n}\n");
}

#[test]
fn fmt_for_await_roundtrip() {
    exact_roundtrip("fn main() {\n    for await x in stream {\n        println(x);\n    }\n}\n");
}

#[test]
fn fmt_const_decl_roundtrip() {
    exact_roundtrip("const X: i32 = 42;\n");
}

#[test]
fn fmt_extern_immutable_pointer_type_roundtrip() {
    exact_roundtrip("extern \"C\" {\n    fn malloc(n: i32) -> *u8;\n}\n");
}

#[test]
fn fmt_extern_mutable_pointer_type_roundtrip() {
    exact_roundtrip("extern \"C\" {\n    fn free(ptr: *var u8);\n}\n");
}

#[test]
fn fmt_trait_multi_item_blank_lines_roundtrip() {
    exact_roundtrip(
        "trait Describable {\n    fn describe() -> i32 {\n        42\n    }\n\n    fn reset();\n}\n",
    );
}

#[test]
fn fmt_contextual_keywords_as_identifiers_roundtrip() {
    exact_roundtrip(
        "fn test_contextual_keywords_as_identifiers() {\n    let wire = 5;\n    let event = \"hello\";\n    let state = true;\n    let join = 42;\n    let after = 0;\n}\n",
    );
}
