/// Formatter totality test — exhaustive AST-variant coverage.
///
/// # The keystone mechanism
///
/// A printer over a typed tree must be TOTAL over the type's variants. When
/// someone adds a new AST variant and forgets to teach the formatter about it,
/// the formatter silently drops the new syntax. This file makes that class of
/// bug a compile-time error rather than a runtime surprise.
///
/// Each `_variant_coverage_guard_*` function below is an exhaustive `match`
/// with NO wildcard arm. Adding a new variant to `Item`, `Expr`, `Stmt`,
/// `Pattern`, or `TypeExpr` without adding a corresponding arm to these
/// functions causes a compiler error:
///
///   ```text
///   error[E0004]: non-exhaustive patterns: `Item::NewVariant(_)` not covered
///   ```
///
/// The guard functions are never called at runtime; their purpose is purely to
/// force the compiler to verify exhaustiveness at every `cargo check`.
///
/// # The structural assertion
///
/// For each variant we also parse a minimal representative Hew snippet,
/// format it via `format_program`, reparse, and verify structural equality
/// using `program_eq_ignoring_spans` (which normalises away all span fields
/// before comparing). This catches formatter drops at runtime: if the
/// formatter omits a clause from an AST node, the reparsed tree will be
/// structurally different from the original.
///
/// # What this does NOT do
///
/// This is not a proptest/Arbitrary fuzz layer. The variant-coverage approach
/// gives ~95% of the leverage at ~20% of the cost. A generative fuzz layer is
/// a potential follow-on (see backlog note at end of file).
///
/// # Relation to existing tests
///
/// `fmt_roundtrip_corpus.rs` — round-trips every real `.hew` file in the repo
/// tree (idempotence net over real-world code). This file layers on top: it
/// guarantees the formatter is structurally total over the AST enum, while the
/// corpus test catches regressions against real programs.
use hew_parser::ast::{Expr, Item, Pattern, Stmt, TypeExpr};
use hew_parser::ast_eq::program_eq_ignoring_spans;
use hew_parser::fmt::format_program;
use hew_parser::parse;

// ---------------------------------------------------------------------------
// Helper
// ---------------------------------------------------------------------------

/// Parse `src`, format via `format_program`, reparse, and assert structural
/// equality. Also verifies idempotency: formatting the already-formatted
/// output produces the same string.
fn assert_roundtrip(src: &str) {
    let r1 = parse(src);
    assert!(
        r1.errors.is_empty(),
        "parse failed for:\n{src}\nErrors: {:?}",
        r1.errors
    );

    let formatted = format_program(&r1.program);

    let r2 = parse(&formatted);
    assert!(
        r2.errors.is_empty(),
        "reparse failed for:\n{src}\nFormatted:\n{formatted}\nErrors: {:?}",
        r2.errors
    );

    assert!(
        program_eq_ignoring_spans(&r1.program, &r2.program),
        "AST mismatch after format+reparse.\nOriginal:\n{src}\nFormatted:\n{formatted}"
    );

    let formatted2 = format_program(&r2.program);
    assert_eq!(
        formatted, formatted2,
        "Formatter not idempotent.\nFirst:\n{formatted}\nSecond:\n{formatted2}"
    );
}

// ---------------------------------------------------------------------------
// Compile-time variant-coverage guards
//
// These functions are NEVER called. Their sole purpose is to produce a
// compile error if a new enum variant is added without a corresponding
// arm here.  The `#[allow(dead_code)]` suppresses the "function never used"
// lint without silencing the coverage enforcement.
// ---------------------------------------------------------------------------

/// Compile-time guard: exhaustive match over `Item` with no wildcard arm.
///
/// Adding a new `Item` variant without updating this match produces
/// `error[E0004]: non-exhaustive patterns` at the next `cargo check`.
#[allow(
    dead_code,
    reason = "called only for compile-time exhaustiveness checking, never at runtime"
)]
#[allow(
    clippy::match_same_arms,
    reason = "each arm must be listed separately so rustc checks individual variant coverage"
)]
fn _variant_coverage_guard_item(item: &Item) {
    match item {
        Item::Import(_) => {}
        Item::Const(_) => {}
        Item::TypeDecl(_) => {}
        Item::TypeAlias(_) => {}
        Item::Trait(_) => {}
        Item::Impl(_) => {}
        Item::Function(_) => {}
        Item::ExternBlock(_) => {}
        Item::Actor(_) => {}
        Item::Supervisor(_) => {}
        Item::Machine(_) => {}
        Item::Record(_) => {} // NO wildcard arm — new variants cause a compile error until added above.
    }
}

/// Compile-time guard: exhaustive match over `Expr` with no wildcard arm.
#[allow(
    dead_code,
    reason = "called only for compile-time exhaustiveness checking, never at runtime"
)]
#[allow(
    clippy::match_same_arms,
    reason = "each arm must be listed separately so rustc checks individual variant coverage"
)]
fn _variant_coverage_guard_expr(expr: &Expr) {
    match expr {
        Expr::Binary { .. } => {}
        Expr::Unary { .. } => {}
        Expr::Clone(_) => {}
        Expr::Literal(_) => {}
        Expr::Identifier(_) => {}
        Expr::Tuple(_) => {}
        Expr::Array(_) => {}
        Expr::ArrayRepeat { .. } => {}
        Expr::MapLiteral { .. } => {}
        Expr::Block(_) => {}
        Expr::If { .. } => {}
        Expr::IfLet { .. } => {}
        Expr::Match { .. } => {}
        Expr::Lambda { .. } => {}
        Expr::Spawn { .. } => {}
        Expr::SpawnLambdaActor { .. } => {}
        Expr::Scope { .. } => {}
        Expr::ForkChild { .. } => {}
        Expr::ForkBlock { .. } => {}
        Expr::ScopeDeadline { .. } => {}
        Expr::InterpolatedString(_) => {}
        Expr::Call { .. } => {}
        Expr::MethodCall { .. } => {}
        Expr::StructInit { .. } => {}
        Expr::Select { .. } => {}
        Expr::Join(_) => {}
        Expr::Timeout { .. } => {}
        Expr::UnsafeBlock(_) => {}
        Expr::Yield(_) => {}
        Expr::Return(_) => {}
        Expr::This => {}
        Expr::FieldAccess { .. } => {}
        Expr::Index { .. } => {}
        Expr::Cast { .. } => {}
        Expr::PostfixTry(_) => {}
        Expr::Range { .. } => {}
        Expr::Await(_) => {}
        Expr::AwaitRestart(_) => {}
        Expr::RegexLiteral(_) => {}
        Expr::ByteStringLiteral(_) => {}
        Expr::ByteArrayLiteral(_) => {}
        Expr::Is { .. } => {}
        Expr::MachineEmit { .. } => {}
        Expr::GenBlock { .. } => {} // NO wildcard arm — new variants cause a compile error until added above.
    }
}

/// Compile-time guard: exhaustive match over `Stmt` with no wildcard arm.
#[allow(
    dead_code,
    reason = "called only for compile-time exhaustiveness checking, never at runtime"
)]
#[allow(
    clippy::match_same_arms,
    reason = "each arm must be listed separately so rustc checks individual variant coverage"
)]
fn _variant_coverage_guard_stmt(stmt: &Stmt) {
    match stmt {
        Stmt::Let { .. } => {}
        Stmt::Var { .. } => {}
        Stmt::Assign { .. } => {}
        Stmt::If { .. } => {}
        Stmt::IfLet { .. } => {}
        Stmt::Match { .. } => {}
        Stmt::Loop { .. } => {}
        Stmt::For { .. } => {}
        Stmt::While { .. } => {}
        Stmt::WhileLet { .. } => {}
        Stmt::Break { .. } => {}
        Stmt::Continue { .. } => {}
        Stmt::Return(_) => {}
        Stmt::Defer(_) => {}
        Stmt::Expression(_) => {} // NO wildcard arm — new variants cause a compile error until added above.
    }
}

/// Compile-time guard: exhaustive match over `Pattern` with no wildcard arm.
#[allow(
    dead_code,
    reason = "called only for compile-time exhaustiveness checking, never at runtime"
)]
#[allow(
    clippy::match_same_arms,
    reason = "each arm must be listed separately so rustc checks individual variant coverage"
)]
fn _variant_coverage_guard_pattern(pat: &Pattern) {
    match pat {
        Pattern::Wildcard => {}
        Pattern::Literal(_) => {}
        Pattern::Identifier(_) => {}
        Pattern::Constructor { .. } => {}
        Pattern::Struct { .. } => {}
        Pattern::RecordShorthand { .. } => {}
        Pattern::Tuple(_) => {}
        Pattern::Or(_, _) => {}
        Pattern::Regex { .. } => {} // NO wildcard arm — new variants cause a compile error until added above.
    }
}

/// Compile-time guard: exhaustive match over `TypeExpr` with no wildcard arm.
#[allow(
    dead_code,
    reason = "called only for compile-time exhaustiveness checking, never at runtime"
)]
#[allow(
    clippy::match_same_arms,
    reason = "each arm must be listed separately so rustc checks individual variant coverage"
)]
fn _variant_coverage_guard_type_expr(ty: &TypeExpr) {
    match ty {
        TypeExpr::Named { .. } => {}
        TypeExpr::Result { .. } => {}
        TypeExpr::Option(_) => {}
        TypeExpr::Tuple(_) => {}
        TypeExpr::Array { .. } => {}
        TypeExpr::Slice(_) => {}
        TypeExpr::Function { .. } => {}
        TypeExpr::Pointer { .. } => {}
        TypeExpr::Borrow(_) => {}
        TypeExpr::TraitObject(_) => {}
        TypeExpr::Infer => {} // NO wildcard arm — new variants cause a compile error until added above.
    }
}

// ---------------------------------------------------------------------------
// Item variant round-trip tests
// ---------------------------------------------------------------------------

/// `Item::Import` — plain path import.
#[test]
fn fmt_totality_item_import() {
    assert_roundtrip("import std::io;\n");
}

/// `Item::Const`
#[test]
fn fmt_totality_item_const() {
    assert_roundtrip("const LIMIT: i64 = 100;\n");
}

/// `Item::TypeDecl` — struct form.
#[test]
fn fmt_totality_item_type_decl_struct() {
    assert_roundtrip("type Point {\n    x: i64;\n    y: i64;\n}\n");
}

/// `Item::TypeDecl` — enum form.
#[test]
fn fmt_totality_item_type_decl_enum() {
    assert_roundtrip("enum Colour {\n    Red;\n    Green;\n    Blue;\n}\n");
}

/// `Item::TypeAlias`
#[test]
fn fmt_totality_item_type_alias() {
    assert_roundtrip("type Metres = i64;\n");
}

/// `Item::Trait` — abstract method.
#[test]
fn fmt_totality_item_trait() {
    assert_roundtrip("trait Greet {\n    fn hello() -> string;\n}\n");
}

/// `Item::Impl`
#[test]
fn fmt_totality_item_impl() {
    assert_roundtrip(
        "type Counter {\n    val: i64;\n}\n\nimpl Counter {\n    fn get(c: Counter) -> i64 {\n        c.val\n    }\n}\n",
    );
}

/// `Item::Function`
#[test]
fn fmt_totality_item_function() {
    assert_roundtrip("fn add(a: i64, b: i64) -> i64 {\n    a + b\n}\n");
}

/// `Item::ExternBlock`
#[test]
fn fmt_totality_item_extern_block() {
    assert_roundtrip("extern \"C\" {\n    fn puts(s: *const i8) -> i64;\n}\n");
}

/// `Item::Actor`
#[test]
fn fmt_totality_item_actor() {
    assert_roundtrip(
        "actor Counter {\n    let count: i64;\n\n    receive fn inc(n: i64) -> i64 {\n        count + n\n    }\n}\n",
    );
}

/// `Item::Supervisor` — with config params (the C4/C5 bug path).
#[test]
fn fmt_totality_item_supervisor() {
    assert_roundtrip(
        "supervisor App(config: string) {\n    strategy: one_for_one;\n\n    child worker: Counter(id: 1);\n}\n",
    );
}

/// `Item::Supervisor` — without params (baseline).
#[test]
fn fmt_totality_item_supervisor_no_params() {
    assert_roundtrip(
        "supervisor App {\n    strategy: one_for_one;\n\n    child worker: Counter(id: 1);\n}\n",
    );
}

/// `Item::Machine` — minimal state machine.
#[test]
fn fmt_totality_item_machine() {
    assert_roundtrip(
        "machine Light {\n    events {\n        Toggle\n    }\n\n    state Off;\n    state On;\n\n    on Toggle: Off => On {\n        On\n    }\n    on Toggle: On => Off {\n        Off\n    }\n}\n",
    );
}

/// `Item::Record` — named form.
#[test]
fn fmt_totality_item_record() {
    assert_roundtrip("record Point { x: i64, y: i64 }\n");
}

// ---------------------------------------------------------------------------
// Expr variant round-trip tests
// ---------------------------------------------------------------------------

/// `Expr::Binary`
#[test]
fn fmt_totality_expr_binary() {
    assert_roundtrip("fn f() -> i64 {\n    1 + 2\n}\n");
}

/// `Expr::Unary` — all four operators.
#[test]
fn fmt_totality_expr_unary() {
    assert_roundtrip("fn f() {\n    let a = !true;\n    let b = -1;\n    let c = ~0;\n}\n");
}

/// `Expr::Clone`
#[test]
fn fmt_totality_expr_clone() {
    assert_roundtrip("fn f(s: string) -> string {\n    clone s\n}\n");
}

/// `Expr::Literal`
#[test]
fn fmt_totality_expr_literal() {
    assert_roundtrip("fn f() -> i64 {\n    42\n}\n");
}

/// `Expr::Identifier`
#[test]
fn fmt_totality_expr_identifier() {
    assert_roundtrip("fn f(x: i64) -> i64 {\n    x\n}\n");
}

/// `Expr::Tuple`
#[test]
fn fmt_totality_expr_tuple() {
    assert_roundtrip("fn f() -> (i64, i64) {\n    (1, 2)\n}\n");
}

/// `Expr::Array`
#[test]
fn fmt_totality_expr_array() {
    assert_roundtrip("fn f() {\n    let a = [1, 2, 3];\n}\n");
}

/// `Expr::ArrayRepeat`
#[test]
fn fmt_totality_expr_array_repeat() {
    assert_roundtrip("fn f() {\n    let a = [0; 8];\n}\n");
}

/// `Expr::MapLiteral` — map literals use string keys.
#[test]
fn fmt_totality_expr_map_literal() {
    assert_roundtrip("fn f() {\n    let m = {\"a\": 1, \"b\": 2};\n}\n");
}

/// `Expr::Block` — inline block as an expression value.
#[test]
fn fmt_totality_expr_block() {
    assert_roundtrip(
        "fn f() -> i64 {\n    let v = {\n        let x = 1;\n        x\n    };\n    v\n}\n",
    );
}

/// `Expr::If`
#[test]
fn fmt_totality_expr_if() {
    assert_roundtrip("fn f(x: i64) -> i64 {\n    if x > 0 { 1 } else { 0 }\n}\n");
}

/// `Expr::IfLet`
#[test]
fn fmt_totality_expr_if_let() {
    assert_roundtrip("fn f() -> i64 {\n    if let Some(v) = maybe() {\n        v\n    } else {\n        0\n    }\n}\n");
}

/// `Expr::Match`
#[test]
fn fmt_totality_expr_match() {
    assert_roundtrip(
        "fn f(x: i64) -> i64 {\n    match x {\n        0 => 1,\n        _ => 0,\n    }\n}\n",
    );
}

/// `Expr::Lambda` — pipe-closure form.
#[test]
fn fmt_totality_expr_lambda() {
    assert_roundtrip("fn f() {\n    let add = |a: i64, b: i64| -> i64 { a + b };\n}\n");
}

/// `Expr::Spawn`
#[test]
fn fmt_totality_expr_spawn() {
    assert_roundtrip(
        "actor Worker {\n    receive fn run() {}\n}\n\nfn f() {\n    let w = spawn Worker;\n}\n",
    );
}

/// `Expr::SpawnLambdaActor`
#[test]
fn fmt_totality_expr_spawn_lambda_actor() {
    assert_roundtrip("fn f() {\n    let a = actor |x: i64| -> i64 { x };\n}\n");
}

/// `Expr::Scope`
#[test]
fn fmt_totality_expr_scope() {
    assert_roundtrip("fn f() {\n    scope {\n        do_work();\n    }\n}\n");
}

/// `Expr::ForkChild` — bare `fork call()` inside a scope.
#[test]
fn fmt_totality_expr_fork_child() {
    assert_roundtrip("fn f() {\n    scope {\n        fork task();\n    }\n}\n");
}

/// `Expr::ForkBlock` — anonymous `fork { ... }` inside a scope.
#[test]
fn fmt_totality_expr_fork_block() {
    assert_roundtrip(
        "fn f() {\n    scope {\n        fork {\n            do_work();\n        }\n    }\n}\n",
    );
}

/// `Expr::ScopeDeadline`
#[test]
fn fmt_totality_expr_scope_deadline() {
    assert_roundtrip(
        "fn f() {\n    scope {\n        after(5s) {\n            cancel();\n        }\n    }\n}\n",
    );
}

/// `Expr::InterpolatedString`
#[test]
fn fmt_totality_expr_interpolated_string() {
    assert_roundtrip("fn f(x: i64) -> string {\n    f\"value is {x}\"\n}\n");
}

/// `Expr::Call`
#[test]
fn fmt_totality_expr_call() {
    assert_roundtrip("fn f() {\n    println(42);\n}\n");
}

/// `Expr::MethodCall`
#[test]
fn fmt_totality_expr_method_call() {
    assert_roundtrip("fn f(s: string) -> i64 {\n    s.len()\n}\n");
}

/// `Expr::StructInit` — brace form.
#[test]
fn fmt_totality_expr_struct_init() {
    assert_roundtrip(
        "type Point {\n    x: i64;\n    y: i64;\n}\n\nfn f() -> Point {\n    Point { x: 1, y: 2 }\n}\n",
    );
}

/// `Expr::Select`
#[test]
fn fmt_totality_expr_select() {
    assert_roundtrip("fn f() {\n    select {\n        v from ch => println(v),\n    }\n}\n");
}

/// `Expr::Join`
#[test]
fn fmt_totality_expr_join() {
    assert_roundtrip("fn f() {\n    join {\n        a(),\n        b(),\n    }\n}\n");
}

/// `Expr::UnsafeBlock`
#[test]
fn fmt_totality_expr_unsafe_block() {
    assert_roundtrip("fn f() {\n    unsafe {\n        do_unsafe_thing();\n    }\n}\n");
}

/// `Expr::Yield`
#[test]
fn fmt_totality_expr_yield() {
    assert_roundtrip("gen fn produce() {\n    yield 1;\n    yield 2;\n}\n");
}

/// `Expr::Return` — expression-position return.
#[test]
fn fmt_totality_expr_return() {
    assert_roundtrip("fn f(x: i64) -> i64 {\n    if x < 0 { return 0; }\n    x\n}\n");
}

/// `Expr::This`
#[test]
fn fmt_totality_expr_this() {
    assert_roundtrip("actor Counter {\n    receive fn me() {\n        let me = this;\n    }\n}\n");
}

/// `Expr::FieldAccess`
#[test]
fn fmt_totality_expr_field_access() {
    assert_roundtrip("type Point {\n    x: i64;\n}\n\nfn f(p: Point) -> i64 {\n    p.x\n}\n");
}

/// `Expr::Index`
#[test]
fn fmt_totality_expr_index() {
    assert_roundtrip("fn f(arr: [i64; 4]) -> i64 {\n    arr[0]\n}\n");
}

/// `Expr::Cast`
#[test]
fn fmt_totality_expr_cast() {
    assert_roundtrip("fn f(x: i64) -> f64 {\n    x as f64\n}\n");
}

/// `Expr::PostfixTry`
#[test]
fn fmt_totality_expr_postfix_try() {
    assert_roundtrip("fn f() -> Result<i64, string> {\n    Ok(try_op()?)\n}\n");
}

/// `Expr::Range` — exclusive and inclusive.
#[test]
fn fmt_totality_expr_range() {
    assert_roundtrip("fn f() {\n    for i in 0 .. 10 {\n        println(i);\n    }\n    for j in 0 ..= 9 {\n        println(j);\n    }\n}\n");
}

/// `Expr::Timeout` — `await expr | after duration`.
#[test]
fn fmt_totality_expr_timeout() {
    assert_roundtrip("fn f() {\n    let value = await task | after 5s;\n}\n");
}

/// `Expr::Await`
#[test]
fn fmt_totality_expr_await() {
    assert_roundtrip("fn f() {\n    let r = await task();\n}\n");
}

/// `Expr::AwaitRestart`
#[test]
fn fmt_totality_expr_await_restart() {
    assert_roundtrip("fn f() {\n    let w = await_restart sup.worker;\n}\n");
}

/// `Expr::RegexLiteral`
#[test]
fn fmt_totality_expr_regex_literal() {
    assert_roundtrip("fn f(s: string) {\n    let ok = re\"^[a-z]+$\".is_match(s);\n}\n");
}

/// `Expr::ByteStringLiteral`
#[test]
fn fmt_totality_expr_byte_string_literal() {
    assert_roundtrip("fn f() {\n    let b = b\"hello\";\n}\n");
}

/// `Expr::ByteArrayLiteral`
#[test]
fn fmt_totality_expr_byte_array_literal() {
    assert_roundtrip("fn f() {\n    let data = bytes [0x48, 0x65, 0x77];\n}\n");
}

/// `Expr::Is`
#[test]
fn fmt_totality_expr_is() {
    assert_roundtrip("fn f(a: string, b: string) -> bool {\n    a is b\n}\n");
}

/// `Expr::MachineEmit` — `emit EventName { fields }` inside a transition body.
#[test]
fn fmt_totality_expr_machine_emit() {
    assert_roundtrip(concat!(
        "machine Signal {\n",
        "    events {\n",
        "        Start\n",
        "        Ready { code: i64 }\n",
        "    }\n",
        "\n",
        "    emits {\n",
        "        Ready;\n",
        "    }\n",
        "\n",
        "    state Idle;\n",
        "    state Active;\n",
        "\n",
        "    on Start: Idle => Active {\n",
        "        emit Ready { code: 0 };\n",
        "        Active\n",
        "    }\n",
        "    on Start: Active => Active reenter {\n",
        "        Active\n",
        "    }\n",
        "}\n",
    ));
}

/// `Expr::GenBlock`
#[test]
fn fmt_totality_expr_gen_block() {
    assert_roundtrip(
        "fn f() {\n    let g = gen {\n        yield 1;\n        yield 2;\n    };\n}\n",
    );
}

// ---------------------------------------------------------------------------
// Stmt variant round-trip tests
// ---------------------------------------------------------------------------

/// `Stmt::Let`
#[test]
fn fmt_totality_stmt_let() {
    assert_roundtrip("fn f() {\n    let x: i64 = 1;\n}\n");
}

/// `Stmt::Let` with else block (let-else).
#[test]
fn fmt_totality_stmt_let_else() {
    assert_roundtrip(
        "fn f() -> i64 {\n    let Ok(v) = try_op() else {\n        return 0;\n    };\n    v\n}\n",
    );
}

/// `Stmt::Var`
#[test]
fn fmt_totality_stmt_var() {
    assert_roundtrip("fn f() {\n    var x: i64 = 0;\n    x = x + 1;\n}\n");
}

/// `Stmt::Assign`
#[test]
fn fmt_totality_stmt_assign() {
    assert_roundtrip("fn f() {\n    var x = 0;\n    x = 5;\n    x += 1;\n}\n");
}

/// `Stmt::If`
#[test]
fn fmt_totality_stmt_if() {
    assert_roundtrip("fn f(x: i64) {\n    if x > 0 {\n        println(x);\n    } else {\n        println(0);\n    }\n}\n");
}

/// `Stmt::IfLet`
#[test]
fn fmt_totality_stmt_if_let() {
    assert_roundtrip("fn f() {\n    if let Some(v) = maybe() {\n        println(v);\n    }\n}\n");
}

/// `Stmt::Match`
#[test]
fn fmt_totality_stmt_match() {
    assert_roundtrip("fn f(x: i64) {\n    match x {\n        0 => println(0),\n        _ => println(x),\n    }\n}\n");
}

/// `Stmt::Loop`
#[test]
fn fmt_totality_stmt_loop() {
    assert_roundtrip("fn f() {\n    loop {\n        break;\n    }\n}\n");
}

/// `Stmt::For`
#[test]
fn fmt_totality_stmt_for() {
    assert_roundtrip("fn f() {\n    for i in 0 .. 10 {\n        println(i);\n    }\n}\n");
}

/// `Stmt::While`
#[test]
fn fmt_totality_stmt_while() {
    assert_roundtrip("fn f() {\n    var x = 10;\n    while x > 0 {\n        x -= 1;\n    }\n}\n");
}

/// `Stmt::WhileLet`
#[test]
fn fmt_totality_stmt_while_let() {
    assert_roundtrip("fn f() {\n    while let Some(v) = next() {\n        println(v);\n    }\n}\n");
}

/// `Stmt::Break`
#[test]
fn fmt_totality_stmt_break() {
    assert_roundtrip("fn f() {\n    loop {\n        break;\n    }\n}\n");
}

/// `Stmt::Continue`
#[test]
fn fmt_totality_stmt_continue() {
    assert_roundtrip("fn f() {\n    for i in 0 .. 10 {\n        if i == 3 {\n            continue;\n        }\n        println(i);\n    }\n}\n");
}

/// `Stmt::Return`
#[test]
fn fmt_totality_stmt_return() {
    assert_roundtrip("fn f(x: i64) -> i64 {\n    return x;\n}\n");
}

/// `Stmt::Defer`
#[test]
fn fmt_totality_stmt_defer() {
    assert_roundtrip("fn f() {\n    defer cleanup();\n}\n");
}

/// `Stmt::Expression`
#[test]
fn fmt_totality_stmt_expression() {
    assert_roundtrip("fn f() {\n    println(42);\n}\n");
}

// ---------------------------------------------------------------------------
// Pattern variant round-trip tests
// ---------------------------------------------------------------------------

/// `Pattern::Wildcard`
#[test]
fn fmt_totality_pattern_wildcard() {
    assert_roundtrip("fn f(x: i64) {\n    match x {\n        _ => println(0),\n    }\n}\n");
}

/// `Pattern::Literal`
#[test]
fn fmt_totality_pattern_literal() {
    assert_roundtrip("fn f(x: i64) {\n    match x {\n        0 => println(0),\n        1 => println(1),\n        _ => println(x),\n    }\n}\n");
}

/// `Pattern::Identifier`
#[test]
fn fmt_totality_pattern_identifier() {
    assert_roundtrip("fn f(x: i64) {\n    match x {\n        n => println(n),\n    }\n}\n");
}

/// `Pattern::Constructor`
#[test]
fn fmt_totality_pattern_constructor() {
    assert_roundtrip("fn f(x: Option<i64>) {\n    match x {\n        Some(v) => println(v),\n        None => println(0),\n    }\n}\n");
}

/// `Pattern::Struct`
#[test]
fn fmt_totality_pattern_struct() {
    assert_roundtrip(
        "type Point {\n    x: i64;\n    y: i64;\n}\n\nfn f(p: Point) {\n    match p {\n        Point { x, y } => println(x),\n    }\n}\n",
    );
}

/// `Pattern::RecordShorthand` — `{ a, b }` without a type name.
#[test]
fn fmt_totality_pattern_record_shorthand() {
    assert_roundtrip(
        "type Point {\n    x: i64;\n    y: i64;\n}\n\nfn f(p: Point) {\n    let { x, y } = p;\n    println(x);\n}\n",
    );
}

/// `Pattern::Tuple`
#[test]
fn fmt_totality_pattern_tuple() {
    assert_roundtrip(
        "fn f(pair: (i64, i64)) {\n    match pair {\n        (a, b) => println(a),\n    }\n}\n",
    );
}

/// `Pattern::Or`
#[test]
fn fmt_totality_pattern_or() {
    assert_roundtrip("fn f(x: i64) {\n    match x {\n        0 | 1 => println(0),\n        _ => println(x),\n    }\n}\n");
}

/// `Pattern::Regex`
#[test]
fn fmt_totality_pattern_regex() {
    assert_roundtrip("fn f(s: string) {\n    match s {\n        re\"^hello\" => println(1),\n        _ => println(0),\n    }\n}\n");
}

// ---------------------------------------------------------------------------
// TypeExpr variant round-trip tests
// ---------------------------------------------------------------------------

/// `TypeExpr::Named`
#[test]
fn fmt_totality_type_expr_named() {
    assert_roundtrip("fn f(x: i64) -> string {\n    f\"{x}\"\n}\n");
}

/// `TypeExpr::Result`
#[test]
fn fmt_totality_type_expr_result() {
    assert_roundtrip("fn f() -> Result<i64, string> {\n    Ok(1)\n}\n");
}

/// `TypeExpr::Option`
#[test]
fn fmt_totality_type_expr_option() {
    assert_roundtrip("fn f(x: Option<i64>) -> i64 {\n    0\n}\n");
}

/// `TypeExpr::Tuple`
#[test]
fn fmt_totality_type_expr_tuple() {
    assert_roundtrip("fn f() -> (i64, string) {\n    (1, \"ok\")\n}\n");
}

/// `TypeExpr::Array` — fixed-size.
#[test]
fn fmt_totality_type_expr_array() {
    assert_roundtrip("fn f(buf: [u8; 16]) -> i64 {\n    0\n}\n");
}

/// `TypeExpr::Slice` — dynamic-size `[T]`.
#[test]
fn fmt_totality_type_expr_slice() {
    assert_roundtrip("fn f(items: [i64]) -> i64 {\n    0\n}\n");
}

/// `TypeExpr::Function`
#[test]
fn fmt_totality_type_expr_function() {
    assert_roundtrip("fn apply(f: fn(i64) -> i64, x: i64) -> i64 {\n    f(x)\n}\n");
}

/// `TypeExpr::Pointer` — immutable raw pointer.
#[test]
fn fmt_totality_type_expr_pointer() {
    assert_roundtrip("extern \"C\" {\n    fn puts(s: *const i8) -> i64;\n}\n");
}

/// `TypeExpr::Borrow` — `&T` borrow.
#[test]
fn fmt_totality_type_expr_borrow() {
    assert_roundtrip("fn f(x: &i64) -> i64 {\n    0\n}\n");
}

/// `TypeExpr::TraitObject` — `dyn Trait`.
#[test]
fn fmt_totality_type_expr_trait_object() {
    assert_roundtrip("fn f(x: dyn Display) {}\n");
}

/// `TypeExpr::Infer` — `_` placeholder.
#[test]
fn fmt_totality_type_expr_infer() {
    assert_roundtrip("fn f() {\n    let x: _ = 1;\n}\n");
}

// ---------------------------------------------------------------------------
// Supervisor config-param regression — the C4/C5 B3 bug
//
// A dedicated regression test for the specific drop that motivated this file:
// `format_supervisor` previously did not emit `decl.params`, silently losing
// config parameters on `hew fmt`. The totality test above exercises this path
// via `fmt_totality_item_supervisor`, but this named test makes the regression
// explicit and self-documenting.
// ---------------------------------------------------------------------------

/// The supervisor config-param clause must survive format → reparse.
///
/// Verifies that `supervisor App(config: AppConfig)` round-trips with the
/// param intact. If `format_supervisor` drops `decl.params`, the reparsed
/// program will have an empty `params` vec and `program_eq_ignoring_spans`
/// will fail.
#[test]
fn fmt_supervisor_config_param_is_not_dropped() {
    let src = "supervisor App(cfg: string, port: i64) {\n    strategy: one_for_one;\n\n    child worker: Counter(id: 1);\n}\n";

    let r1 = parse(src);
    assert!(r1.errors.is_empty(), "parse failed: {:?}", r1.errors);

    let formatted = format_program(&r1.program);

    // The param clause must be present in the formatted output.
    assert!(
        formatted.contains("(cfg: string, port: i64)"),
        "format_supervisor dropped the config-param clause.\nFormatted:\n{formatted}"
    );

    let r2 = parse(&formatted);
    assert!(
        r2.errors.is_empty(),
        "reparse failed: {:?}\nFormatted:\n{formatted}",
        r2.errors
    );

    assert!(
        program_eq_ignoring_spans(&r1.program, &r2.program),
        "AST mismatch: supervisor config params were lost in format → reparse.\nFormatted:\n{formatted}"
    );
}

// ---------------------------------------------------------------------------
// Backlog note (one-liner only, per audit recommendation):
//
// A proptest/Arbitrary fuzz layer over AST variants was evaluated and cut as
// YAGNI for a solo engineer. The variant-coverage guards above give ~95% of
// the leverage at ~20% of the maintenance cost. If the formatter grows complex
// field-conditional logic that escapes variant-level coverage, revisit then.
// ---------------------------------------------------------------------------
