//! Formatter coverage tests — round-trip parse → format → verify output.
//!
//! Each test parses Hew source, formats via `format_source` or `format_program`,
//! and checks that the result is idempotent and structurally sound.

use hew_parser::ast::{Expr, Item, Stmt, Visibility};
use hew_parser::ast_eq::program_eq_ignoring_spans;
use hew_parser::fmt::{format_program, format_source};
use hew_parser::parse;

// -----------------------------------------------------------------------
// Helpers
// -----------------------------------------------------------------------

/// Parse `source`, assert no errors, format with comment preservation,
/// then re-parse and re-format to verify idempotency.
fn roundtrip(source: &str) -> String {
    let r1 = parse(source);
    assert!(
        r1.errors.is_empty(),
        "Initial parse failed: {:?}",
        r1.errors
    );

    let formatted = format_source(source, &r1.program);

    let r2 = parse(&formatted);
    assert!(
        r2.errors.is_empty(),
        "Re-parse of formatted output failed: {:?}\nFormatted:\n{formatted}",
        r2.errors,
    );

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
        "Formatter output changed.\nExpected:\n{source}\nActual:\n{formatted}",
    );
}

fn assert_exact_roundtrips(sources: &[&str]) {
    for &source in sources {
        exact_roundtrip(source);
    }
}

// -----------------------------------------------------------------------
// Functions, signatures & control flow
// -----------------------------------------------------------------------

#[test]
fn fmt_functions_and_signatures_roundtrip() {
    let out = roundtrip(
        r"fn compute(x: i32) -> i32 {
    let y = x * 2;
    let z = y + 1;
    z
}",
    );
    assert!(out.contains("let y = x * 2;"), "output: {out}");
    assert!(out.contains("let z = y + 1;"), "output: {out}");

    let out = roundtrip("fn print_it<T: Display>(x: T) { println(x); }");
    assert!(out.contains("<T: Display>"), "output: {out}");

    let out = roundtrip("impl Counter {\n    fn next(var self) -> i64 {\n        1\n    }\n}\n");
    assert!(out.contains("fn next(var self) -> i64"), "output: {out}");

    let out = roundtrip("fn modify(var x: i32) -> i32 { x = x + 1; x }");
    assert!(out.contains("var x: i32"), "output: {out}");

    let out = roundtrip("#[test]\nfn fast() -> i32 {\n    42\n}");
    assert!(out.contains("#[test]"), "output: {out}");

    let out = roundtrip("fn process<T>(x: T) -> i32 where T: Display {\n    42\n}");
    assert!(out.contains("where T: Display"), "output: {out}");
}

#[test]
fn fmt_conditionals_and_loops_roundtrip() {
    let out = roundtrip(
        r"fn main() {
    if x > 0 {
        println(x);
    } else {
        println(0);
    }
}",
    );
    assert!(out.contains("} else {"), "else on same line. output: {out}");

    let out = roundtrip("fn main() -> i32 {\n    let val = if true { 1 } else { 0 };\n    val\n}");
    assert!(out.contains("if true {"), "output: {out}");

    let out = roundtrip("fn main() {\n    for i in 0..10 {\n        println(i);\n    }\n}");
    assert!(out.contains("for i in 0..10 {"), "output: {out}");

    let out = roundtrip("fn main() {\n    for (k, v) in items {\n        println(k);\n    }\n}");
    assert!(out.contains("for (k, v) in items"), "output: {out}");

    let out =
        roundtrip("fn main() {\n    var x = 10;\n    while x > 0 {\n        x = x - 1;\n    }\n}");
    assert!(out.contains("while x > 0 {"), "output: {out}");

    let out = roundtrip(
        "fn main() {\n    var i = 0;\n    loop {\n        if i > 5 {\n            break;\n        }\n        i = i + 1;\n    }\n}",
    );
    assert!(out.contains("loop {"), "output: {out}");
    assert!(out.contains("break;"), "output: {out}");

    let out = roundtrip(
        "fn main() {\n    for i in 0..10 {\n        if i == 3 {\n            continue;\n        }\n        println(i);\n    }\n}",
    );
    assert!(out.contains("continue;"), "output: {out}");
}

#[test]
fn fmt_if_let_while_let_and_labeled_loops_exact_roundtrip() {
    assert_exact_roundtrips(&[
        "fn check(x: i32) {\n    if x > 10 {\n        println(1);\n    } else if x > 0 {\n        println(2);\n    } else {\n        println(3);\n    }\n}\n",
        "fn check(maybe: Option<i32>, x: i32) {\n    if x > 10 {\n        println(1);\n    } else if let Some(y) = maybe {\n        println(y);\n    } else {\n        println(3);\n    }\n}\n",
        "fn main() {\n    if let Some(x) = maybe {\n        println(x);\n    }\n}\n",
        "fn main() {\n    if let Some(x) = maybe {\n        println(x);\n    } else {\n        println(0);\n    }\n}\n",
        "fn main() {\n    if let Point { x, y } = p {\n        println(x);\n    }\n}\n",
        "fn main() {\n    if let (a, b) = pair {\n        println(a);\n    }\n}\n",
        "fn main() {\n    if let A | B = val {\n        work();\n    }\n}\n",
        "fn main() {\n    while let Point { x, y } = next_point() {\n        work(x);\n    }\n}\n",
        "fn main() {\n    while let (k, v) = iter.next() {\n        use_pair(k, v);\n    }\n}\n",
        "fn main() {\n    while let Some(x) = iter.next() {\n        println(x);\n    }\n}\n",
        "fn main() {\n    let x = if let Some(v) = opt {\n        v\n    } else {\n        0\n    };\n}\n",
        "fn foo() -> int {\n    if let Some(x) = maybe {\n        x\n    } else {\n        0\n    }\n}\n",
        "fn main() {\n    @outer: loop {\n        break @outer;\n    }\n}\n",
        "fn main() {\n    @counter: while x > 0 {\n        x = x - 1;\n        if x == 3 {\n            continue @counter;\n        }\n    }\n}\n",
        "fn main() {\n    @rows: for i in 0..n {\n        for j in 0..m {\n            if cond {\n                break @rows;\n            }\n        }\n    }\n}\n",
        "fn main() {\n    @scan: while let Some(x) = iter.next() {\n        if x == 0 {\n            break @scan;\n        }\n    }\n}\n",
    ]);
}

#[test]
fn fmt_match_and_patterns_roundtrip() {
    let out = roundtrip(
        r"fn describe(x: i32) {
    match x {
        0 => println(0),
        1 => println(1),
        _ => println(x),
    }
}",
    );
    assert!(out.contains("match x {"), "output: {out}");
    assert!(out.contains("0 => println(0),"), "output: {out}");
    assert!(out.contains("_ => println(x),"), "output: {out}");

    let out = roundtrip(
        r"enum Colour {
    Red;
    Green;
    Blue;
}

fn name(c: Colour) -> i32 {
    match c {
        Colour.Red => 1,
        Colour.Green => 2,
        Colour.Blue => 3,
    }
}
",
    );
    assert!(out.contains("Colour.Red => 1,"), "output: {out}");

    let out = roundtrip(
        r"fn classify(x: i32) -> i32 {
    match x {
        n if n > 100 => 3,
        n if n > 0 => 2,
        _ => 1,
    }
}",
    );
    assert!(out.contains("n if n > 100 => 3,"), "output: {out}");

    assert_exact_roundtrips(&[
        "fn main() {\n    match p {\n        Point { x, y } => println(x),\n        _ => println(0),\n    }\n}\n",
        "fn main() {\n    match e {\n        Ev { x: Some(v), y } => println(v),\n        _ => println(0),\n    }\n}\n",
        "fn main() {\n    match x {\n        1 | 2 => 3,\n        _ => 0,\n    }\n}\n",
    ]);
}

// -----------------------------------------------------------------------
// Types, enums, traits, impls & actors
// -----------------------------------------------------------------------

#[test]
fn fmt_struct_and_enum_definitions_roundtrip() {
    let out = roundtrip("type Point {\n    x: f64;\n    y: f64;\n}\n");
    assert!(out.contains("type Point {"), "output: {out}");
    assert!(out.contains("    x: f64;"), "output: {out}");
    assert!(out.contains("    y: f64;"), "output: {out}");

    let out = roundtrip("type Pair<A, B> {\n    first: A;\n    second: B;\n}\n");
    assert!(out.contains("type Pair<A, B> {"), "output: {out}");

    let out = roundtrip("enum Direction {\n    North;\n    South;\n    East;\n    West;\n}\n");
    assert!(out.contains("enum Direction {"), "output: {out}");
    assert!(out.contains("    North;"), "output: {out}");

    let out = roundtrip("enum Shape {\n    Circle(f64);\n    Rectangle(f64, f64);\n}\n");
    assert!(out.contains("Circle(f64);"), "output: {out}");
    assert!(out.contains("Rectangle(f64, f64);"), "output: {out}");

    let out = roundtrip(
        "enum Event {\n    Click { x: i32; y: i32;  }\n    KeyPress { code: i32;  }\n}\n",
    );
    assert!(out.contains("Click { x: i32; y: i32; }"), "output: {out}");
}

#[test]
fn fmt_trait_and_impl_definitions_roundtrip() {
    let out = roundtrip("trait Greet {\n    fn hello() -> i32;\n}");
    assert!(out.contains("trait Greet {"), "output: {out}");
    assert!(out.contains("fn hello() -> i32;"), "output: {out}");

    let out = roundtrip("trait Describable {\n    fn describe() -> i32 {\n        42\n    }\n}");
    assert!(out.contains("trait Describable {"), "output: {out}");
    assert!(out.contains("fn describe() -> i32 {"), "output: {out}");

    let out = roundtrip("trait Printable: Display {\n    fn output();\n}");
    assert!(out.contains("trait Printable: Display"), "output: {out}");

    exact_roundtrip(
        "trait Describable {\n    fn describe() -> i32 {\n        42\n    }\n\n    fn reset();\n}\n",
    );

    let spaced = concat!(
        "// Spacing follows the source.\n",
        "trait Describable {\n",
        "    fn describe() -> i32 {\n",
        "        42\n",
        "    }\n",
        "\n",
        "    fn reset();\n",
        "}\n"
    );
    assert_eq!(roundtrip(spaced), spaced);
    let adjacent = spaced.replace("    }\n\n", "    }\n");
    assert_eq!(roundtrip(&adjacent), adjacent);

    let out = roundtrip(
        r"type Counter {
    value: i32;
}

impl Counter {
    fn new() -> Counter {
        Counter(value: 0)
    }

    fn get(self) -> i32 {
        self.value
    }
}
",
    );
    assert!(out.contains("impl Counter {"), "output: {out}");
    assert!(out.contains("fn new() -> Counter"), "output: {out}");
    assert!(out.contains("fn get(self) -> i32"), "output: {out}");

    let out = roundtrip(
        r#"type MyType {
    val: i32;
}

impl Display for MyType {
    fn to_string(self) -> String {
        f"{self.val}"
    }
}
"#,
    );
    assert!(out.contains("impl Display for MyType"), "output: {out}");

    let out = roundtrip(
        "type Wrapper<T> {\n    inner: T;\n}\n\nimpl<T> Wrapper<T> {\n    fn unwrap(self) -> T {\n        self.inner\n    }\n}\n",
    );
    assert!(out.contains("impl<T> Wrapper<T>"), "output: {out}");
}

#[test]
fn fmt_actor_declarations_and_hooks_roundtrip() {
    let out = roundtrip(
        "actor Counter {\n    let count: i32;\n\n    receive fn increment(n: i32) -> i32 {\n        count + n\n    }\n}\n",
    );
    assert!(out.contains("actor Counter {"), "output: {out}");
    assert!(out.contains("let count: i32;"), "output: {out}");
    assert!(out.contains("receive fn increment"), "output: {out}");

    let out =
        roundtrip("actor Worker {\n    receive fn work(n: i32) -> i32 {\n        n\n    }\n}");
    assert!(
        out.contains("actor Worker {\n    receive fn work(n: i32) -> i32 {"),
        "output: {out}"
    );

    let out = roundtrip(
        "actor Worker {\n    let id: i32;\n\n    mailbox 16;\n\n    receive fn process(value: i32) {\n        println(value);\n    }\n}\n",
    );
    assert!(out.contains("mailbox 16;"), "output: {out}");

    let out = roundtrip("actor Counter {\n    fn current() -> i32 {\n        0\n    }\n}");
    assert!(
        out.contains("actor Counter {\n    fn current() -> i32 {"),
        "output: {out}"
    );

    roundtrip(
        "actor Ticker {\n    let count: int;\n\n    #[every(50ms)]\n    receive fn tick() {\n        count = count + 1;\n    }\n}\n\nfn main() {}\n",
    );
    roundtrip(
        "actor Logger {\n    let label: string;\n\n    receive fn log(msg: string) {\n        println(f\"[{label}] {msg}\");\n    }\n\n    #[on(stop)]\n    fn shutdown() {\n        println(f\"[{label}] shutting down\");\n    }\n}\n\nfn main() {}\n",
    );

    assert_exact_roundtrips(&[
        "actor Counter {\n    receive fn current() {\n        let me = this;\n    }\n}\n",
        "actor Worker {\n    #[every(50ms)]\n    receive fn tick() {\n        work();\n    }\n\n    #[on(stop)]\n    fn cleanup() {\n        shutdown();\n    }\n}\n\nfn main() {\n}\n",
        "actor Logger {\n    let label: string;\n\n    receive fn log(msg: string) {\n        println(f\"[{label}] {msg}\");\n    }\n\n    #[on(start)]\n    fn init() {\n        println(f\"[{label}] started\");\n    }\n}\n\nfn main() {\n}\n",
        "actor Worker {\n    receive fn work() {\n        do_work();\n    }\n\n    #[on(restart)]\n    fn handle_restart() {\n        reset();\n    }\n}\n\nfn main() {\n}\n",
        "actor Guard {\n    receive fn run() {\n        execute();\n    }\n\n    #[on(crash)]\n    fn on_crash() {\n        log_error();\n    }\n}\n\nfn main() {\n}\n",
        "actor Service {\n    receive fn handle() {\n        process();\n    }\n\n    #[on(upgrade)]\n    fn migrate() {\n        migrate_state();\n    }\n}\n\nfn main() {\n}\n",
    ]);
}

#[test]
fn fmt_actor_field_mutability_preserved() {
    let var_src =
        "actor Counter {\n    var count: i64 = 0;\n\n    receive fn increment() {\n        count = count + 1;\n    }\n}\n";
    let out = roundtrip(var_src);
    assert!(
        out.contains("var count: i64 = 0;"),
        "formatter must not rewrite `var` field to `let`; output:\n{out}"
    );
    assert!(
        !out.contains("let count:"),
        "output must not contain `let count:`; output:\n{out}"
    );

    let p1 = parse(var_src);
    assert!(p1.errors.is_empty(), "parse errors: {:?}", p1.errors);
    let formatted = format_program(&p1.program);
    let p2 = parse(&formatted);
    assert!(p2.errors.is_empty(), "reparse errors: {:?}", p2.errors);
    assert!(
        program_eq_ignoring_spans(&p1.program, &p2.program),
        "AST mismatch: var-field mutability lost after format+reparse.\nFormatted:\n{formatted}"
    );

    let let_out = roundtrip("actor Frozen {\n    let x: i64 = 0;\n\n    receive fn noop() {}\n}\n");
    assert!(
        let_out.contains("let x: i64 = 0;"),
        "formatter must preserve `let` field; output:\n{let_out}"
    );
    assert!(
        !let_out.contains("var x:"),
        "formatter must not promote `let` field to `var`; output:\n{let_out}"
    );

    exact_roundtrip(
        "actor Mixed {\n    let id: i64 = 0;\n    var count: i64 = 0;\n\n    receive fn increment() {\n        count = count + 1;\n    }\n}\n",
    );
}

// -----------------------------------------------------------------------
// Closures, generators & concurrency surfaces
// -----------------------------------------------------------------------

#[test]
fn fmt_lambdas_and_actor_lambdas_roundtrip() {
    let out = roundtrip("fn main() { let f = |x: i32| -> i32 { x + 1 }; }");
    assert!(out.contains("|x: i32| -> i32 {"), "output: {out}");

    let out = roundtrip("fn main() { let add = |a, b| a + b; }");
    assert!(out.contains("|a, b| a + b"), "output: {out}");

    assert_exact_roundtrips(&[
        "fn main() {\n    let worker = actor |x: int| {\n        x + 1\n    };\n}\n",
        "fn main() {\n    let adder = actor |x: int, y: int| {\n        x + y\n    };\n}\n",
        "fn main() {\n    let f = move |x: i32| -> i32 {\n        x + 1\n    };\n}\n",
        "fn main() {\n    scope {\n        (move || {\n            let _ = r;\n        })();\n    };\n}\n",
        "fn main() {\n    let worker = actor move |x: int| {\n        x + 1\n    };\n}\n",
    ]);
}

#[test]
fn fmt_generators_and_yield_roundtrip() {
    let out = roundtrip(
        "fn main() {\n    let g = gen {\n        yield 1;\n        yield 2;\n    };\n}\n",
    );
    assert!(out.contains("gen {"), "output: {out}");
    assert!(out.contains("yield 1"), "output: {out}");

    assert_exact_roundtrips(&[
        "fn main() {\n    let yields = gen { yield 1; yield 2; };\n    let returns = gen { yield 1; 2 };\n    let explicit = gen { return 1; };\n}\n",
        "fn main() {\n    let g = gen {\n        yield 1;\n        yield 2;\n        3\n    };\n}\n",
        "gen fn counting(n: i32) -> i32 {\n    for i in 0..n {\n        yield i;\n    }\n}\n",
        "actor NumberStream {\n    receive gen fn numbers() -> i32 {\n        yield 1;\n    }\n}\n",
        "gen fn tick() -> i32 {\n    yield;\n}\n",
    ]);
}

#[test]
fn fmt_channels_and_surface_constructs_roundtrip() {
    assert_exact_roundtrips(&[
        "fn make_channel() -> Duplex<int, string> {\n    duplex_pair(16)\n}\n",
        "fn pipe(src: Stream<string>, dst: Sink<string>) {\n    forward(src, dst);\n}\n",
        "fn main() {\n    ch.send(42);\n    let v = ch.recv();\n    ch.try_send(1);\n    let r = ch.try_recv();\n    ch.close();\n}\n",
        "fn main() {\n    let sink = ch.send_half();\n    let stream = ch.recv_half();\n}\n",
        "fn main() {\n    scope {\n        let child = fork run();\n    };\n}\n",
        "fn main() {\n    scope {\n        let worker = fork compute();\n        let auditor = fork audit();\n        worker\n    };\n}\n",
        "fn main() {\n    let (tx, rx) = duplex_pair<int, string>(16);\n}\n",
        "fn main() {\n    let value = select {\n        msg from inbox.recv() => msg,\n        after 100ms => -1,\n    };\n}\n",
        "fn main() {\n    let value = result?;\n}\n",
        "fn main() {\n    unsafe {\n        do_thing();\n    };\n}\n",
        "fn main() {\n    defer {\n        cleanup();\n    }\n}\n",
    ]);

    let out = roundtrip("fn main() { defer cleanup(); }");
    assert!(out.contains("defer cleanup();"), "output: {out}");
}

// -----------------------------------------------------------------------
// Calls, imports, interpolation, contextual paths & spreads
// -----------------------------------------------------------------------

#[test]
fn fmt_calls_and_generic_values_roundtrip() {
    let out = roundtrip("fn main() { list.push(1).push(2).push(3); }");
    assert!(
        out.contains("list.push(1).push(2).push(3)"),
        "output: {out}"
    );

    let out = roundtrip(
        "type Pt {\n    x: i32;\n    y: i32;\n}\n\nfn main() {\n    let p = Pt(x: 10, y: 20);\n}\n",
    );
    assert!(out.contains("Pt(x: 10, y: 20)"), "output: {out}");

    assert_exact_roundtrips(&[
        "fn main() {\n    restarted(sup.worker);\n}\n",
        "fn main() {\n    let a = worker<Vec<Vec<i64>>>();\n    let b = service.compute<string>();\n    let c = HashMap<string, User>.new();\n    let d = Tag<bool> { value: true };\n}\n",
        "fn make() -> fn(i64) -> i64 {\n    let f = id<i64>;\n    let pair = (id<i64>, id<string>);\n    use_fn(id<i64>);\n    id<i64>\n}\n",
    ]);
}

#[test]
fn fmt_string_interpolation_roundtrip() {
    let out = roundtrip(r#"fn main() { let msg = f"value is {x}"; }"#);
    assert!(out.contains(r#"f"value is {x}""#), "output: {out}");

    let out = roundtrip(r#"fn main() { let msg = f"{a} plus {b} equals {c}"; }"#);
    assert!(
        out.contains(r#"f"{a} plus {b} equals {c}""#),
        "output: {out}"
    );
}

#[test]
fn fmt_import_forms_roundtrip() {
    for (src, expected) in [
        ("import std.io;", "import std.io;"),
        (
            "import std.collections.{HashMap, HashSet};",
            "import std.collections.{HashMap, HashSet};",
        ),
        (
            "import std.{println, print};",
            "import std.{println, print};",
        ),
    ] {
        let out = roundtrip(src);
        assert!(out.contains(expected), "output: {out}");
    }

    assert_exact_roundtrips(&[
        "import std.net.{http as h, websocket as ws};\n",
        "import std.net as n;\n",
        "import \"codecs/huffman.hew\" as huffman;\n",
        "import \"codecs/rle.hew\";\n",
        "import app.net.http as transport;\n",
        "import app.net.http.{self as transport, Client as C,};\n",
    ]);
}

#[test]
fn fmt_contextual_paths_and_spreads_roundtrip() {
    assert_exact_roundtrips(&[
        "fn main() {\n    let a = .None;\n    let b = .Some(value);\n    let c = .Ready { ..base, value: value };\n}\n",
        "fn f(value: module.E) {\n    match value {\n        module.E.Some(x) => x,\n        module.E.None => 0,\n        .Other => 1,\n    }\n}\n",
        "fn project<T>(x: <T as iter.Iterator>.Item) {\n    let make = <T as iter.Iterator>.make();\n}\n",
        "fn main() {\n    let a = [..xs, 1, ..ys];\n    let b = [..xs];\n    let c = [1, ..xs];\n}\n",
        "fn main() {\n    let data = wire.Message.Data { ..base, bytes: payload };\n}\n",
        "fn test_contextual_keywords_as_identifiers() {\n    let wire = 5;\n    let event = \"hello\";\n    let state = true;\n    let join = 42;\n    let after = 0;\n}\n",
    ]);

    let out = roundtrip("fn main() { let p = Point { x: 1, ..base }; }");
    assert!(
        out.contains("Point { ..base, x: 1 }"),
        "the base is written first: {out}"
    );
}

// -----------------------------------------------------------------------
// Operators, assignments, expressions & parenthesisation
// -----------------------------------------------------------------------

#[test]
fn fmt_statements_and_blocks_roundtrip() {
    let out = roundtrip("fn early(x: i32) -> i32 { if x < 0 { return 0; } x }");
    assert!(out.contains("return 0;"), "output: {out}");

    let out = roundtrip("fn compute() -> i32 {\n    let x = 1;\n    let y = 2;\n    x + y\n}");
    assert!(out.contains("    x + y\n"), "output: {out}");

    let out = roundtrip("actor Worker {\n    receive fn work(n: i32) -> i32 { n }\n}\n\nfn main() {\n    let w = spawn Worker;\n}");
    assert!(out.contains("spawn Worker"), "output: {out}");

    let out = roundtrip("fn fetch() -> i32 {\n    42\n}\n\nfn main() {\n    let f = fetch();\n    let r = await f;\n}");
    assert!(out.contains("await f"), "output: {out}");
}

#[test]
fn fmt_assignments_and_compound_ops_roundtrip() {
    let out = roundtrip("fn main() { var x = 1; x = 2; }");
    assert!(out.contains("x = 2;"), "output: {out}");

    let out = roundtrip("fn main() { var x = 0; x += 10; }");
    assert!(out.contains("x += 10;"), "output: {out}");

    assert_exact_roundtrips(&[
        "fn main() {\n    var x = 10;\n    x -= 1;\n}\n",
        "fn main() {\n    var x = 10;\n    x *= 2;\n}\n",
        "fn main() {\n    var x = 10;\n    x /= 2;\n}\n",
        "fn main() {\n    var x = 10;\n    x %= 3;\n}\n",
        "fn main() {\n    var x = 10;\n    x &= 0xFF;\n}\n",
        "fn main() {\n    var x = 10;\n    x |= 1;\n}\n",
        "fn main() {\n    var x = 10;\n    x ^= mask;\n}\n",
        "fn main() {\n    var x = 10;\n    x <<= 2;\n}\n",
        "fn main() {\n    var x = 10;\n    x >>= 1;\n}\n",
    ]);
}

#[test]
fn fmt_operators_and_unary_paren_roundtrip() {
    let out = roundtrip("fn main() { let r = a + b * c - d / e; }");
    assert!(out.contains("a + b * c - d / e"), "output: {out}");

    let out = roundtrip("fn main() { let r = x >= 0 && x < 100; }");
    assert!(out.contains("x >= 0 && x < 100"), "output: {out}");

    assert_exact_roundtrips(&[
        "fn main() {\n    let ok = re\"pat\".is_match(s);\n}\n",
        "fn main() {\n    let no = !re\"pat\".is_match(s);\n}\n",
        "fn main() {\n    let r = a & b;\n}\n",
        "fn main() {\n    let r = a | b;\n}\n",
        "fn main() {\n    let r = a ^ b;\n}\n",
        "fn main() {\n    let r = a << 2;\n}\n",
        "fn main() {\n    let r = a >> 1;\n}\n",
        "fn main() {\n    let ok = !flag;\n}\n",
        "fn main() {\n    let n = -x;\n}\n",
        "fn main() {\n    let m = ~mask;\n}\n",
        "fn main() {\n    let r = !(a && b);\n}\n",
        "fn main() {\n    let r = -(a + b);\n}\n",
        "fn main() {\n    let r = ~(a | b);\n}\n",
    ]);
}

#[test]
fn fmt_literals_collections_and_postfix_roundtrip() {
    for (src, needle) in [
        ("fn main() { let arr = [1, 2, 3]; }", "[1, 2, 3]"),
        ("fn main() { let t = (1, 2, 3); }", "(1, 2, 3)"),
        (
            "type Point {\n    x: i32;\n    y: i32;\n}\n\nfn main() {\n    let p = Point(x: 1, y: 2);\n}\n",
            "Point(x: 1, y: 2)",
        ),
        ("fn main() { let v = arr[0]; }", "arr[0]"),
        ("fn main() { let x = 42 as f64; }", "42 as f64"),
        ("fn main() { let x = (a + b) * c; }", "(a + b) * c"),
    ] {
        let out = roundtrip(src);
        assert!(out.contains(needle), "output: {out}");
    }

    assert_exact_roundtrips(&[
        "type Point {\n    x: i32;\n    y: i32;\n}\n\nfn main() {\n    let p = Point { x: 1, y: 2 };\n}\n",
        "fn main() {\n    let m = {\"a\": 1, \"b\": 2};\n}\n",
        "fn main() {\n    let values = [0; 10];\n}\n",
    ]);
}

#[test]
fn fmt_range_formatting_roundtrip() {
    let out =
        roundtrip("fn main() { for i in 0 .. n { f(i); } for j in a.b ..= c.len() { f(j); } }");
    assert!(out.contains("0..n"), "output: {out}");
    assert!(out.contains("a.b..=c.len()"), "output: {out}");

    let out = roundtrip(
        "fn main() { for i in n-1..m { f(i); } for j in 0..=(a + b) { f(j); } let s = v[a..b + 1]; let t = v[a .. b]; }",
    );
    assert!(out.contains("n - 1 .. m"), "output: {out}");
    assert!(out.contains("0 ..= (a + b)"), "output: {out}");
    assert!(out.contains("v[a .. b + 1]"), "output: {out}");
    assert!(out.contains("v[a..b]"), "output: {out}");

    exact_roundtrip("fn main() {\n    for i in 0..=9 {\n        println(i);\n    }\n}\n");
}

#[test]
fn fmt_receiver_parenthesisation_roundtrip() {
    for src in [
        "fn f() -> i64 {\n    let _b = (\"p-\" + \"q\").len();\n    0\n}\n",
        "fn f(a: Point, b: Point) -> i64 {\n    (a + b).x\n}\n",
        "fn f(x: i64) -> i64 {\n    (-x).abs()\n}\n",
    ] {
        exact_roundtrip(src);
        let p1 = parse(src);
        let formatted = format_program(&p1.program);
        let p2 = parse(&formatted);
        assert!(
            program_eq_ignoring_spans(&p1.program, &p2.program),
            "AST mismatch after format.\nFormatted:\n{formatted}"
        );
    }
}

#[test]
fn fmt_numeric_field_chains_preserve_parse_and_identity() {
    for expression in [
        "(outer.0).1",
        "((outer.0).1).2",
        "(outer.inner.10).11",
        "((outer.0).1).name",
        "(outer.0)[1].2",
        "(1).0",
        "outer.0.name",
        "outer.name.0",
    ] {
        let source = format!("fn f() {{ let _ = {expression}; }}");
        let formatted = roundtrip(&source);
        let original = parse(&source);
        let reparsed = parse(&formatted);
        assert!(
            program_eq_ignoring_spans(&original.program, &reparsed.program),
            "field expression changed: {expression}\n{formatted}"
        );
    }
}

// -----------------------------------------------------------------------
// Indentation, visibility & comment stripping
// -----------------------------------------------------------------------

#[test]
fn fmt_indentation_blank_lines_and_comment_stripping() {
    let out = roundtrip(
        r"fn main() {
    if true {
        if false {
            println(1);
        }
    }
}",
    );
    assert!(out.contains("            println(1);"), "output: {out}");

    let out = roundtrip("fn main() { let x = 1; }");
    assert!(out.contains("    let x = 1;"), "output: {out}");

    let src = "fn fibonacci(n: i32) -> i32 {\n    if n <= 1 { n } else { fibonacci(n - 1) + fibonacci(n - 2) }\n}\n";
    assert_eq!(src, roundtrip(src));

    let out =
        roundtrip_no_comments("// strip me\nfn main() {\n    // strip too\n    let x = 1;\n}");
    assert!(!out.contains("// strip"), "output: {out}");
    assert!(out.contains("fn main()"), "output: {out}");
    assert!(out.contains("let x = 1;"), "output: {out}");

    let out = roundtrip("fn foo() {\n}\n\nfn bar() {\n}\n");
    assert!(out.contains("}\n\nfn bar"), "output: {out}");
}

#[test]
fn fmt_pub_visibility_roundtrip() {
    let out =
        roundtrip("pub fn helper() -> i32 {\n    42\n}\n\nfn internal() -> i32 {\n    0\n}\n");
    assert!(out.contains("pub fn helper"), "output: {out}");
    assert!(out.contains("fn internal"), "output: {out}");

    let out = roundtrip(
        "type Foo {\n    x: int;\n}\n\nimpl Foo {\n    pub fn make(v: int) -> Foo {\n        Foo { x: v }\n    }\n\n    fn private_helper(self) -> int {\n        self.x\n    }\n}\n",
    );
    assert!(out.contains("pub fn make"), "output: {out}");
    assert!(out.contains("fn private_helper"), "output: {out}");
}

#[test]
fn fmt_package_visibility_roundtrip_preserves_visibility() {
    fn extract_vis(program: &hew_parser::ast::Program) -> Visibility {
        match program.items.first().map(|(i, _)| i) {
            Some(Item::Function(f)) => f.visibility,
            Some(Item::TypeDecl(t)) => t.visibility,
            Some(Item::Const(c)) => c.visibility,
            other => panic!("unexpected item: {other:?}"),
        }
    }

    for (src, expected_kw) in [
        ("package fn f() {}\n", "package fn f"),
        (
            "package type Point {\n    x: i64;\n    y: i64;\n}\n",
            "package type Point",
        ),
        ("package const MAX: i64 = 100;\n", "package const MAX"),
    ] {
        let r1 = parse(src);
        assert!(r1.errors.is_empty(), "parse failed: {:?}", r1.errors);
        assert_eq!(extract_vis(&r1.program), Visibility::Package);

        let formatted = format_source(src, &r1.program);
        assert!(
            formatted.contains(expected_kw),
            "formatter dropped `{expected_kw}`: {formatted:?}"
        );

        let r2 = parse(&formatted);
        assert!(
            r2.errors.is_empty(),
            "re-parse failed: {:?}\nFormatted:\n{formatted}",
            r2.errors
        );
        assert!(
            program_eq_ignoring_spans(&r1.program, &r2.program),
            "AST mismatch for {expected_kw}"
        );
        assert_eq!(extract_vis(&r2.program), Visibility::Package);

        let formatted2 = format_source(&formatted, &r2.program);
        assert_eq!(formatted, formatted2);
    }
}

// -----------------------------------------------------------------------
// Combined programs, extern pointers/borrows, assoc types, wire, machines, supervisors
// -----------------------------------------------------------------------

#[test]
fn fmt_full_programs_roundtrip() {
    let out = roundtrip(
        r"const MAX: i32 = 100;

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
    var p = Point.origin();
    for i in 0..MAX {
        println(i);
    }
}
",
    );
    for needle in [
        "const MAX: i32 = 100;",
        "type Point {",
        "impl Point {",
        "fn main()",
    ] {
        assert!(out.contains(needle), "output: {out}");
    }

    let src = r"enum Colour {
    Red;
    Green;
    Blue;
}

fn to_hex(c: Colour) -> i32 {
    match c {
        Colour.Red => 1,
        Colour.Green => 2,
        Colour.Blue => 3,
    }
}
";
    assert_eq!(src, roundtrip(src));
}

#[test]
fn fmt_extern_pointers_and_borrows_roundtrip() {
    let out = roundtrip("extern \"C\" {\n    fn puts(s: CString) -> i32;\n}");
    assert!(out.contains("extern \"C\" {"), "output: {out}");
    assert!(out.contains("fn puts(s: CString) -> i32;"), "output: {out}");

    assert_exact_roundtrips(&[
        "extern \"C\" {\n    fn malloc(n: i32) -> *const u8;\n}\n",
        "extern \"C\" {\n    fn free(ptr: *mut u8);\n}\n",
        "extern \"C\" {\n    fn foo(x: &string) -> i32;\n}\n",
        "extern \"C\" {\n    fn bar(x: &Vec<i32>) -> i32;\n}\n",
        "extern \"C\" {\n    fn baz() -> &string;\n}\n",
    ]);

    let borrow_fmt = roundtrip("extern \"C\" { fn f(x: &i32) -> i32; }");
    assert!(borrow_fmt.contains("&i32"), "got: {borrow_fmt}");
    assert!(!borrow_fmt.contains("*const"), "got: {borrow_fmt}");
}

#[test]
fn fmt_types_assoc_and_wire_decls_roundtrip() {
    assert_exact_roundtrips(&[
        "const X: i32 = 42;\n",
        "type Alias = i32;\n",
        "fn apply(f: fn(i32) -> bool) -> bool {\n    f(1)\n}\n",
        "fn main() {\n    let x: _ = 42;\n}\n",
        "type Showable = dyn (Display + Debug);\n",
        "fn use_iter(iter: dyn Iterator<Item = i32>) {\n}\n",
        "trait Iterator {\n    type Item;\n\n    fn next(self) -> Option<Self.Item>;\n}\n",
        "fn use_pair(pair: dyn Pair<Left = i32, Right = string>) {\n}\n",
        "fn use_mapper(mapper: dyn Mapper<i32, string, Key = i32, Value = string>) {\n}\n",
        "trait Container {\n    type Item;\n\n    fn get(self) -> Self.Item;\n}\n",
        "trait AssocForms {\n    type Plain;\n\n    fn make(self) -> Self.Plain;\n\n    type Bounded: Display;\n\n    type Defaulted = i32;\n\n    fn show(self) -> Self.Bounded;\n\n    type BoundedDefault: Display = string;\n}\n",
        "impl Container for Vec<i32> {\n    type Item = i32;\n\n    fn get(self) -> i32 {\n        self[0]\n    }\n}\n",
        "impl Container for Widget {\n    fn get(self) -> Self.Item {\n        1\n    }\n\n    type Item = i32;\n}\n",
        "fn collect<I: Iterator>(it: I) -> Vec<I.Item> where I.Item: Display {\n    empty()\n}\n",
        "fn collect_display<I>(it: I) -> Vec<I.Item> where I: Iterator<Item = string>, I.Item: Display {\n    empty()\n}\n",
        "#[wire]\ntype Message {\n    id: i32 @1;\n}\n",
        "#[wire]\ntype Message {\n    id: i32 @1;\n    reserved @2;\n    body: string;\n    labels: Vec<string> repeated;\n}\n",
        "#[wire]\nenum Command {\n    Start @0;\n    Push(i64) @1;\n    Move { x: i32; y: i32; } @2;\n}\n",
        "#[wire]\nenum Command {\n    Start;\n    Fixed @0;\n    reserved @2;\n    Push(i64);\n    Move { x: i32; y: i32; }\n}\n",
        "#[wire(version = 2, min_version = 1)]\nenum Packet {\n    V1 @0;\n    V2 @1;\n}\n",
    ]);
}

#[test]
fn fmt_machine_and_supervisor_syntax_roundtrip() {
    assert_exact_roundtrips(&[
        "machine Light {\n    events {\n        Toggle;\n    }\n\n    state Off;\n    state On;\n\n    on Toggle: Off => On;\n    on Toggle: On => Off;\n}\n",
        "machine Light {\n    events {\n        Toggle;\n    }\n\n    state Off;\n    state On;\n\n    on Toggle: Off => .On;\n    on Toggle: On => .Off;\n}\n",
        "machine Light {\n    events {\n        Toggle;\n    }\n\n    state Off;\n    state On;\n\n    on Toggle: Off => .On;\n    on Toggle: On => Off;\n}\n",
        "machine Light {\n    events {\n        Toggle;\n    }\n\n    state Off;\n    state On;\n\n    on Toggle: Off => .On;\n    on Toggle: _ => .Off;\n}\n",
        "machine Light {\n    events {\n        Toggle;\n    }\n\n    state Off;\n    state On;\n\n    on Toggle: Off => .On;\n    on Toggle: _ => _ {\n        state\n    }\n}\n",
        "machine Light {\n    events {\n        Toggle;\n    }\n\n    state Off;\n    state On;\n\n    on Toggle: Off => .On {\n        state\n    }\n    on Toggle: On => .Off {\n        state\n    }\n}\n",
        "machine Bank {\n    events {\n        Deposit { amount: Int; }\n    }\n\n    state Open { balance: Int; }\n\n    on Deposit: Open => .Open { balance: event.amount }\n}\n",
        "machine Gate {\n    events {\n        Try;\n    }\n\n    state Locked;\n    state Open;\n\n    on Try: Locked => .Locked when flag;\n    on Try: Locked => .Open reenter;\n}\n",
        "machine Bucket {\n    events {\n        Drain;\n    }\n\n    state Full { tokens: Int; }\n    state Empty;\n\n    on Drain: Full => Empty;\n    on Drain: Empty => Empty;\n}\n",
        "machine Bank {\n    events {\n        Deposit { amount: Int; }\n    }\n\n    state Open;\n\n    on Deposit: Open => Open;\n}\n",
        "machine Signal {\n    events {\n        Start;\n        Ready;\n    }\n\n    emits {\n        Ready;\n    }\n\n    state Idle;\n    state Active;\n\n    on Start: Idle => Active {\n        emit Ready;\n        Active\n    }\n    on Ready: Idle => Idle reenter;\n    on Ready: Active => Active reenter;\n    on Start: Active => Active reenter;\n}\n",
        "machine Meter {\n    events {\n        Sample { value: i64; }\n    }\n\n    emits {\n        Quiet;\n        Rule;\n        Alarm { value: i64; }\n    }\n\n    state Watching { peak: i64; }\n\n    on Sample: Watching => Watching {\n        emit Quiet;\n        emit Rule;\n        emit Alarm { value: event.value };\n        Watching { peak: event.value }\n    }\n}\n",
        "machine Gate {\n    events {\n        Try;\n    }\n\n    state Locked;\n    state Open;\n\n    on Try: Locked => Locked when flag;\n    on Try: Locked => Open;\n}\n",
        "machine Counter {\n    events {\n        Inc;\n    }\n\n    state Active { n: Int; }\n\n    on Inc: Active => Active when state.n < 10 { n: state.n + 1 }\n}\n",
        "machine Counter {\n    events {\n        Inc;\n    }\n\n    state Active { n: Int; }\n\n    on Inc: Active => Active when flag { n: state.n + 1 }\n}\n",
        "machine Counter {\n    events {\n        Inc;\n    }\n\n    state Active { n: Int; }\n\n    on Inc: Active => Active reenter { n: state.n + 1 }\n}\n",
        "machine Safe {\n    events {\n        Toggle;\n    }\n\n    state On;\n    state Off;\n\n    on Toggle: On => Off;\n\n    default { state }\n}\n",
        "machine Lifecycle<T> {\n    events {\n        Start;\n    }\n\n    state Created;\n    state Running;\n\n    on Start: Created => Running;\n    on Start: Running => Running;\n}\n",
        "machine Pair<K, V> {\n    events {\n        Insert;\n    }\n\n    state Empty;\n    state Filled;\n\n    on Insert: Empty => Filled;\n    on Insert: Filled => Filled;\n}\n",
        "pub machine Lifecycle<T> {\n    events {\n        Start;\n    }\n\n    state Created;\n    state Running;\n\n    on Start: Created => Running;\n    on Start: Running => Running;\n}\n",
        "supervisor Pool {\n    strategy: one_for_one;\n    intensity: 5 within 30s;\n\n    child worker: Worker { id: 1 };\n}\n",
        "supervisor Pool {\n    intensity: 5 within 30s;\n\n    child w: Worker { id: 1 };\n}\n",
        "supervisor ServiceStack {\n    strategy: rest_for_one;\n    intensity: 5 within 60s;\n\n    child db: Database { connections: 4 } restart: permanent stop: 10s;\n    child api: ApiHandler { port: 8080 } restart: transient stop: 0s wired_to: { backend: db };\n}\n",
        "supervisor ConnectionPool {\n    strategy: simple_one_for_one;\n    intensity: 20 within 60s;\n\n    pool handler: ApiHandler { port: 8080 };\n}\n",
        "supervisor Pool {\n    strategy: simple_one_for_one;\n    intensity: 5 within 60s;\n\n    pool workers: Worker { value: 7 } count: 3;\n}\n",
    ]);

    let formatted = roundtrip("machine Delivery{events{Send;Completion;}state Ready;state Running{initial state Waiting;final state Paid;on Send:Waiting=>Paid;}final state Finished;on Completion:Ready=>Running;on completion:Running=>Finished;always:Ready=>Finished when false;default{state}}");
    for needle in [
        "final state Paid;",
        "final state Finished;",
        "on completion: Running => Finished;",
        "always: Ready => Finished when false;",
        "on Completion: Ready => Running;",
    ] {
        assert!(formatted.contains(needle), "{formatted}");
    }
}

// -----------------------------------------------------------------------
// Literals & escape sequences
// -----------------------------------------------------------------------

#[test]
fn fmt_literals_roundtrip() {
    for (src, expected) in [
        (
            "fn main() { let a = true; let b = false; }",
            vec!["true", "false"],
        ),
        (
            r#"fn main() { let s = "hello world"; }"#,
            vec![r#""hello world""#],
        ),
        ("fn main() { let c = 'a'; }", vec!["'a'"]),
        ("fn main() { let x = 42; }", vec!["42"]),
        ("fn main() { let x = 3.14; }", vec!["3.14"]),
    ] {
        let out = roundtrip(src);
        for needle in expected {
            assert!(out.contains(needle), "output: {out}");
        }
    }

    assert_exact_roundtrips(&[
        "fn main() {\n    let data = bytes [0x48, 0x65, 0x77];\n}\n",
        "fn main() {\n    let short = 100ms;\n    let medium = 5s;\n    let long = 1m;\n}\n",
        "fn main() {\n    let hex = 0xFF;\n    let octal = 0o77;\n    let binary = 0b1010;\n}\n",
    ]);
}

#[test]
fn byte_literal_hex_escapes_preserve_every_byte_through_formatting() {
    use std::fmt::Write as _;

    let mut source = String::from("fn main() { let data = b\"");
    for byte in 0..=u8::MAX {
        write!(source, "\\x{byte:02x}").unwrap();
    }
    source.push_str("\"; }");
    let parsed = parse(&source);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let Item::Function(function) = &parsed.program.items[0].0 else {
        panic!("expected function");
    };
    let Stmt::Let {
        value: Some((Expr::ByteStringLiteral(data), _)),
        ..
    } = &function.body.stmts[0].0
    else {
        panic!("expected bytes binding");
    };
    assert_eq!(data, &(0..=u8::MAX).collect::<Vec<_>>());

    let formatted = roundtrip(&source);
    let reparsed = parse(&formatted);
    assert!(program_eq_ignoring_spans(
        &parsed.program,
        &reparsed.program
    ));
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
    assert_eq!(formatted, reformatted);
}

#[test]
fn fmt_string_and_fstring_escapes_roundtrip() {
    let out = roundtrip(r#"fn main() { let bytes = b"quote\"slash\\line\n"; }"#);
    assert!(out.contains(r#"b"quote\"slash\\line\n""#), "output: {out}");

    let out = roundtrip(r#"fn main() { let s = "a\nb\tc\\d\"e\rf"; }"#);
    assert!(out.contains(r#""a\nb\tc\\d\"e\rf""#), "output: {out}");
    let canonical = "fn main() {\n    let s = \"a\\nb\\tc\\\\d\\\"e\\rf\";\n}\n";
    assert_eq!(roundtrip(canonical), canonical);

    let out = roundtrip(r#"fn main() { let s = f"line1\nline2\ttab\\back\"quote\rcr"; }"#);
    for needle in ["\\n", "\\t", "\\\\", "\\\"", "\\r"] {
        assert!(out.contains(needle), "missing {needle}: {out}");
    }

    let out = roundtrip(r#"fn greet(name: string) { println(f"hello\t{name}\n"); }"#);
    for needle in ["\\t", "\\n", "{name}"] {
        assert!(out.contains(needle), "missing {needle}: {out}");
    }

    for src in [
        r#"fn main() { let s = "\u{00E9}"; }"#,
        r#"fn main() { let s = "\u{1F600}"; }"#,
        r#"fn f() { let s = "a\nb"; }"#,
        r#"fn f() { let s = "a\tb"; }"#,
        r#"fn f() { let s = "a\rb"; }"#,
        r#"fn f() { let s = "a\\b"; }"#,
        r#"fn f() { let s = "a\"b"; }"#,
        r#"fn f() { let s = "a\x07b"; }"#,
        r#"fn f() { let s = "a\u{200B}b"; }"#,
        r#"fn f() { let s = "a\0b"; }"#,
        r#"fn f() { let s = f"a\0{1}"; }"#,
        r#"fn f(x: i64) { let s = f"a\n{x}b"; }"#,
        r#"fn f(x: i64) { let s = f"a\t{x}b"; }"#,
        r#"fn f(x: i64) { let s = f"a\r{x}b"; }"#,
        r#"fn f(x: i64) { let s = f"a\\{x}b"; }"#,
        r#"fn f(x: i64) { let s = f"a\"{x}b"; }"#,
        r#"fn f(x: i64) { let s = f"a\x07{x}b"; }"#,
        r#"fn f(x: i64) { let s = f"a\u{200B}{x}b"; }"#,
    ] {
        let p1 = parse(src);
        assert!(
            p1.errors.is_empty(),
            "parse failed for {src:?}: {:?}",
            p1.errors
        );
        let formatted = format_program(&p1.program);
        let p2 = parse(&formatted);
        assert!(
            p2.errors.is_empty(),
            "reparse failed for {src:?}:\nFormatted: {formatted:?}\nErrors: {:?}",
            p2.errors
        );
        assert!(
            program_eq_ignoring_spans(&p1.program, &p2.program),
            "AST mismatch for {src:?}.\nFormatted: {formatted:?}"
        );
        assert_eq!(
            formatted,
            format_program(&p2.program),
            "format_program not idempotent for {src:?}"
        );
    }
}

#[test]
fn fmt_invisible_escapes_preserved_exact() {
    assert_exact_roundtrips(&[
        "fn main() {\n    let s = \"bell\\x07end\";\n}\n",
        "fn main() {\n    let s = \"a\\u{200B}b\";\n}\n",
        "fn main() {\n    let s = f\"bell\\x07{x}end\";\n}\n",
        "fn main() {\n    let s = f\"a\\u{200B}{x}b\";\n}\n",
    ]);
}
