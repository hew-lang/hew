//! Representative syntax must preserve its AST through formatting and reparsing.
use hew_parser::ast_eq::program_eq_ignoring_spans;
use hew_parser::fmt::format_program;
use hew_parser::parse;

/// Parse `src`, format via `format_program`, reparse, and assert structural
/// equality and fixed-point idempotency.
fn assert_roundtrip(label: &str, src: &str) -> String {
    let r1 = parse(src);
    assert!(
        r1.errors.is_empty(),
        "[{label}] parse failed for:\n{src}\nErrors: {:?}",
        r1.errors
    );

    let formatted = format_program(&r1.program);

    let r2 = parse(&formatted);
    assert!(
        r2.errors.is_empty(),
        "[{label}] reparse failed for:\n{src}\nFormatted:\n{formatted}\nErrors: {:?}",
        r2.errors
    );

    assert!(
        program_eq_ignoring_spans(&r1.program, &r2.program),
        "[{label}] AST mismatch after format+reparse.\nOriginal:\n{src}\nFormatted:\n{formatted}"
    );

    let formatted2 = format_program(&r2.program);
    assert_eq!(
        formatted, formatted2,
        "[{label}] Formatter not idempotent.\nFirst:\n{formatted}\nSecond:\n{formatted2}"
    );

    formatted
}

fn assert_roundtrips(cases: &[(&str, &str)]) {
    for &(label, src) in cases {
        let _ = assert_roundtrip(label, src);
    }
}

#[test]
fn fmt_totality_item_variants() {
    assert_roundtrips(&[
        ("Item::Import", "import std.io;\n"),
        ("Item::Const", "const LIMIT: i64 = 100;\n"),
        (
            "Item::TypeDecl struct",
            "type Point {\n    x: i64;\n    y: i64;\n}\n",
        ),
        (
            "Item::TypeDecl enum",
            "enum Colour {\n    Red;\n    Green;\n    Blue;\n}\n",
        ),
        ("Item::TypeAlias", "type Metres = i64;\n"),
        ("Item::Trait", "trait Greet {\n    fn hello() -> string;\n}\n"),
        (
            "Item::Impl",
            "type Counter {\n    val: i64;\n}\n\nimpl Counter {\n    fn get(self) -> i64 {\n        self.val\n    }\n}\n",
        ),
        (
            "Item::Function",
            "fn add(a: i64, b: i64) -> i64 {\n    a + b\n}\n",
        ),
        (
            "Item::ExternBlock",
            "extern \"C\" {\n    fn puts(s: *const i8) -> i64;\n}\n",
        ),
        (
            "Item::Actor",
            "actor Counter {\n    let count: i64;\n\n    receive fn inc(n: i64) -> i64 {\n        count + n\n    }\n}\n",
        ),
        (
            "Item::Supervisor no params",
            "supervisor App {\n    strategy: one_for_one;\n\n    child worker: Counter { id: 1 };\n}\n",
        ),
        (
            "Item::Machine",
            "machine Light {\n    events {\n        Toggle;\n    }\n\n    state Off;\n    state On;\n\n    on Toggle: Off => On {\n        On\n    }\n    on Toggle: On => Off {\n        Off\n    }\n}\n",
        ),
        ("Item::Record tuple", "type Point(i64, i64);\n"),
    ]);

    for (src, clause) in [
        (
            "supervisor App(config: string) {\n    strategy: one_for_one;\n\n    child worker: Counter { id: 1 };\n}\n",
            "(config: string)",
        ),
        (
            "supervisor App(cfg: string, port: i64) {\n    strategy: one_for_one;\n\n    child worker: Counter { id: 1 };\n}\n",
            "(cfg: string, port: i64)",
        ),
    ] {
        let formatted = assert_roundtrip("Item::Supervisor with params", src);
        assert!(
            formatted.contains(clause),
            "format_supervisor dropped `{clause}`:\n{formatted}"
        );
    }
}

const EXPR_VARIANT_CASES: &[(&str, &str)] = &[
    ("Expr::Binary", "fn f() -> i64 {\n    1 + 2\n}\n"),
    (
        "Expr::Unary",
        "fn f() {\n    let a = !true;\n    let b = -1;\n    let c = ~0;\n}\n",
    ),
    ("Expr::Clone", "fn f(s: string) -> string {\n    clone s\n}\n"),
    ("Expr::Literal", "fn f() -> i64 {\n    42\n}\n"),
    ("Expr::Ident", "fn f(x: i64) -> i64 {\n    x\n}\n"),
    ("Expr::Tuple", "fn f() -> (i64, i64) {\n    (1, 2)\n}\n"),
    ("Expr::Array", "fn f() {\n    let a = [1, 2, 3];\n}\n"),
    ("Expr::ArrayRepeat", "fn f() {\n    let a = [0; 8];\n}\n"),
    (
        "Expr::MapLiteral",
        "fn f() {\n    let m = {\"a\": 1, \"b\": 2};\n}\n",
    ),
    (
        "Expr::Block",
        "fn f() -> i64 {\n    let v = {\n        let x = 1;\n        x\n    };\n    v\n}\n",
    ),
    (
        "Expr::If",
        "fn f(x: i64) -> i64 {\n    if x > 0 { 1 } else { 0 }\n}\n",
    ),
    (
        "Expr::IfLet",
        "fn f() -> i64 {\n    if let Some(v) = maybe() {\n        v\n    } else {\n        0\n    }\n}\n",
    ),
    (
        "Expr::Match",
        "fn f(x: i64) -> i64 {\n    match x {\n        0 => 1,\n        _ => 0,\n    }\n}\n",
    ),
    (
        "Expr::Lambda",
        "fn f() {\n    let add = |a: i64, b: i64| -> i64 { a + b };\n}\n",
    ),
    (
        "Expr::Spawn",
        "actor Worker {\n    receive fn run() {}\n}\n\nfn f() {\n    let w = spawn Worker;\n}\n",
    ),
    (
        "Expr::SpawnLambdaActor",
        "fn f() {\n    let a = actor |x: i64| -> i64 { x };\n}\n",
    ),
    (
        "Expr::Scope",
        "fn f() {\n    scope {\n        do_work();\n    }\n}\n",
    ),
    (
        "Expr::ForkChild",
        "fn f() {\n    scope {\n        fork task();\n    }\n}\n",
    ),
    (
        "Expr::ForkBlock",
        "fn f() {\n    scope {\n        fork {\n            do_work();\n        }\n    }\n}\n",
    ),
    (
        "Expr::ScopeDeadline",
        "fn f() {\n    scope {\n        scope within 5s {\n            cancel();\n        }\n    }\n}\n",
    ),
    (
        "Expr::InterpolatedString",
        "fn f(x: i64) -> string {\n    f\"value is {x}\"\n}\n",
    ),
    ("Expr::Call", "fn f() {\n    println(42);\n}\n"),
    (
        "Expr::MethodCall",
        "fn f(s: string) -> i64 {\n    s.len()\n}\n",
    ),
    (
        "Expr::StructInit",
        "type Point {\n    x: i64;\n    y: i64;\n}\n\nfn f() -> Point {\n    Point { x: 1, y: 2 }\n}\n",
    ),
    (
        "Expr::Select",
        "fn f() {\n    select {\n        v from ch => println(v),\n    }\n}\n",
    ),
    (
        "Expr::UnsafeBlock",
        "fn f() {\n    unsafe {\n        do_unsafe_thing();\n    }\n}\n",
    ),
    (
        "Expr::Yield",
        "gen fn produce() {\n    yield 1;\n    yield 2;\n}\n",
    ),
    (
        "Expr::Return",
        "fn f(x: i64) -> i64 {\n    if x < 0 { return 0; }\n    x\n}\n",
    ),
    (
        "actor self",
        "actor Counter {\n    receive fn me() {\n        let me = self;\n    }\n}\n",
    ),
    (
        "Expr::FieldAccess",
        "type Point {\n    x: i64;\n}\n\nfn f(p: Point) -> i64 {\n    p.x\n}\n",
    ),
    (
        "Expr::Index",
        "fn f(arr: [i64; 4]) -> i64 {\n    arr[0]\n}\n",
    ),
    ("Expr::Cast", "fn f(x: i64) -> f64 {\n    x as f64\n}\n"),
    (
        "Expr::PostfixTry",
        "fn f() -> Result<i64, string> {\n    .Ok(try_op()?)\n}\n",
    ),
    (
        "Expr::Range",
        "fn f() {\n    for i in 0 .. 10 {\n        println(i);\n    }\n    for j in 0 ..= 9 {\n        println(j);\n    }\n}\n",
    ),
    ("Expr::Await", "fn f() {\n    let r = await task();\n}\n"),
    (
        "Expr::RegexLiteral",
        "fn f(s: string) {\n    let ok = re\"^[a-z]+$\".is_match(s);\n}\n",
    ),
    (
        "Expr::ByteStringLiteral",
        "fn f() {\n    let b = b\"hello\";\n}\n",
    ),
    (
        "Expr::ByteArrayLiteral",
        "fn f() {\n    let data = bytes [0x48, 0x65, 0x77];\n}\n",
    ),
    (
        "Expr::Is",
        "fn f(a: string, b: string) -> bool {\n    a is b\n}\n",
    ),
    (
        "Expr::MachineEmit",
        "machine Signal {\n    events {\n        Start;\n        Ready { code: i64; }\n    }\n\n    emits {\n        Ready;\n    }\n\n    state Idle;\n    state Active;\n\n    on Start: Idle => Active {\n        emit Ready { code: 0 };\n        Active\n    }\n    on Start: Active => Active reenter {\n        Active\n    }\n}\n",
    ),
    (
        "Expr::GenBlock",
        "fn f() {\n    let g = gen {\n        yield 1;\n        yield 2;\n    };\n}\n",
    ),
];

#[test]
fn fmt_totality_expr_variants() {
    assert_roundtrips(EXPR_VARIANT_CASES);
}

#[test]
fn fmt_totality_stmt_variants() {
    assert_roundtrips(&[
        ("Stmt::Let", "fn f() {\n    let x: i64 = 1;\n}\n"),
        (
            "Stmt::Let else",
            "fn f() -> i64 {\n    let Ok(v) = try_op() else {\n        return 0;\n    };\n    v\n}\n",
        ),
        (
            "Stmt::Var",
            "fn f() {\n    var x: i64 = 0;\n    x = x + 1;\n}\n",
        ),
        (
            "Stmt::Assign",
            "fn f() {\n    var x = 0;\n    x = 5;\n    x += 1;\n}\n",
        ),
        (
            "Stmt::If",
            "fn f(x: i64) {\n    if x > 0 {\n        println(x);\n    } else {\n        println(0);\n    }\n}\n",
        ),
        (
            "Stmt::IfLet",
            "fn f() {\n    if let Some(v) = maybe() {\n        println(v);\n    }\n}\n",
        ),
        (
            "Stmt::Match",
            "fn f(x: i64) {\n    match x {\n        0 => println(0),\n        _ => println(x),\n    }\n}\n",
        ),
        ("Stmt::Loop / Break", "fn f() {\n    loop {\n        break;\n    }\n}\n"),
        (
            "Stmt::For",
            "fn f() {\n    for i in 0 .. 10 {\n        println(i);\n    }\n}\n",
        ),
        (
            "Stmt::While",
            "fn f() {\n    var x = 10;\n    while x > 0 {\n        x -= 1;\n    }\n}\n",
        ),
        (
            "Stmt::WhileLet",
            "fn f() {\n    while let Some(v) = next() {\n        println(v);\n    }\n}\n",
        ),
        (
            "Stmt::Continue",
            "fn f() {\n    for i in 0 .. 10 {\n        if i == 3 {\n            continue;\n        }\n        println(i);\n    }\n}\n",
        ),
        ("Stmt::Return", "fn f(x: i64) -> i64 {\n    return x;\n}\n"),
        ("Stmt::Expression", "fn f() {\n    println(42);\n}\n"),
        ("Stmt::Defer", "fn f() {\n    defer cleanup();\n}\n"),
    ]);
}

#[test]
fn fmt_totality_pattern_variants() {
    assert_roundtrips(&[
        (
            "Pattern::Wildcard",
            "fn f(x: i64) {\n    match x {\n        _ => println(0),\n    }\n}\n",
        ),
        (
            "Pattern::Literal",
            "fn f(x: i64) {\n    match x {\n        0 => println(0),\n        1 => println(1),\n        _ => println(x),\n    }\n}\n",
        ),
        (
            "Pattern::Identifier",
            "fn f(x: i64) {\n    match x {\n        n => println(n),\n    }\n}\n",
        ),
        (
            "Pattern::Constructor",
            "fn f(x: Option<i64>) {\n    match x {\n        Some(v) => println(v),\n        None => println(0),\n    }\n}\n",
        ),
        (
            "Pattern::Struct",
            "type Point {\n    x: i64;\n    y: i64;\n}\n\nfn f(p: Point) {\n    match p {\n        Point { x, y } => println(x),\n    }\n}\n",
        ),
        (
            "Pattern::RecordShorthand",
            "type Point {\n    x: i64;\n    y: i64;\n}\n\nfn f(p: Point) {\n    let { x, y } = p;\n    println(x);\n}\n",
        ),
        (
            "Pattern::Tuple",
            "fn f(pair: (i64, i64)) {\n    match pair {\n        (a, b) => println(a),\n    }\n}\n",
        ),
        (
            "Pattern::Or",
            "fn f(x: i64) {\n    match x {\n        0 | 1 => println(0),\n        _ => println(x),\n    }\n}\n",
        ),
        (
            "Pattern::Regex",
            "fn f(s: string) {\n    match s {\n        re\"^hello\" => println(1),\n        _ => println(0),\n    }\n}\n",
        ),
    ]);
}

#[test]
fn fmt_totality_type_expr_variants() {
    assert_roundtrips(&[
        (
            "TypeExpr::Named",
            "fn f(x: i64) -> string {\n    f\"{x}\"\n}\n",
        ),
        (
            "TypeExpr::Result",
            "fn f() -> Result<i64, string> {\n    .Ok(1)\n}\n",
        ),
        (
            "TypeExpr::Option",
            "fn f(x: Option<i64>) -> i64 {\n    0\n}\n",
        ),
        (
            "TypeExpr::Tuple",
            "fn f() -> (i64, string) {\n    (1, \"ok\")\n}\n",
        ),
        (
            "TypeExpr::Array",
            "fn f(buf: [u8; 16]) -> i64 {\n    0\n}\n",
        ),
        ("TypeExpr::Slice", "fn f(items: [i64]) -> i64 {\n    0\n}\n"),
        (
            "TypeExpr::Function",
            "fn apply(f: fn(i64) -> i64, x: i64) -> i64 {\n    f(x)\n}\n",
        ),
        (
            "TypeExpr::Pointer",
            "extern \"C\" {\n    fn puts(s: *const i8) -> i64;\n}\n",
        ),
        (
            "TypeExpr::Borrow",
            "extern \"C\" {\n    fn f(x: &i64) -> i64;\n}\n",
        ),
        ("TypeExpr::TraitObject", "fn f(x: dyn Display) {}\n"),
        ("TypeExpr::Infer", "fn f() {\n    let x: _ = 1;\n}\n"),
    ]);
}
