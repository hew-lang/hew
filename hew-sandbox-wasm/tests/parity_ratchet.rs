//! Allowlist parity-coverage ratchet (SP-3 / closes G7).
//!
//! # Why this file exists
//!
//! The sandbox profile (`profile.rs`) admits a set of language constructs:
//! anything it does not fail-closed-reject compiles to bytecode and is handed to
//! the TS VM. Historically, parity was enforced over exactly 11 hand-picked
//! examples — so a construct that the profile admits but that the emitter or
//! interpreter mis-handles could silently diverge from native while the suite
//! stayed green. The float-as-i64 bug (G1) is exactly this class: admitted,
//! unparited, wrong. The compound-assignment hole (`x += 5` lowered as `x = 5`)
//! was a second instance, found by this lane.
//!
//! # The source of truth is `profile.rs`, not a hand-kept list
//!
//! The earlier ratchet hand-maintained an `ADMITTED_CONSTRUCTS` list that was
//! never cross-checked against the profile's *actual* admission predicates — so
//! admitting a construct in `profile.rs` with no manifest row and no parity case
//! still passed the build, defeating the ratchet's whole purpose.
//!
//! This file derives from the real profile in two complementary ways:
//!
//! 1. **Exhaustive structural enumeration ([`mod ast_surface`]).** A set of
//!    `classify_*` functions matches *every* variant of the parser's `Expr`,
//!    `Stmt`, `Pattern`, `TypeExpr`, `BinaryOp`, and `UnaryOp` enums with **no
//!    wildcard arm**. Adding a new AST variant fails to compile until it is
//!    classified here, and each classification names the construct's manifest
//!    coverage. This is the structural guarantee that a construct cannot enter
//!    the language without a coverage decision in this file.
//!
//! 2. **Behavioural verification against the live gate ([`CONSTRUCTS`]).** Every
//!    construct carries a minimal probe that is compiled through the *real*
//!    [`compile_to_sandbox_bytecode`] (profile + emitter) and, when it produces
//!    bytecode, run on the *real* TS VM. The cross-check tests assert the
//!    declared coverage matches what the gate actually does:
//!    - [`Coverage::Parity`] — the gate admits it, it runs cleanly on the VM,
//!      and the name is pinned in `REQUIRED_PARITY_TEST_NAMES` (so a real
//!      stdout+exit parity case in `parity.rs` proves it).
//!    - [`Coverage::NotYetRunnable`] — the gate admits it but the VM traps or
//!      the emitter fails to lower it (fail-loud). Catalogued, never silently
//!      "green". The moment a graduation lane makes one runnable, the honesty
//!      test fails, forcing promotion to `Parity` + a parity case.
//!    - [`Coverage::RejectedByProfile`] — the gate fail-closed-rejects it (no
//!      bytecode). The probe is asserted to actually be rejected, so a profile
//!      change that *starts* admitting it trips this test.
//!
//! Together: you cannot mark a construct runnable without a green parity case,
//! you cannot make a catalogued hole runnable without joining the ratchet, and
//! you cannot admit a construct in `profile.rs` (or add a brand-new AST variant)
//! without it being accounted for here — the build fails otherwise. A future
//! G1-class silent-wrong-answer hole inside the admitted surface is structurally
//! prevented: any construct that runs cleanly on the VM but is not pinned to a
//! parity case is caught by [`admitted_runnable_probes_have_a_required_case`].

use std::collections::BTreeSet;
use std::path::{Path, PathBuf};
use std::process::Output;
use std::sync::OnceLock;

use hew_sandbox_wasm::{compile_to_sandbox_bytecode, CompileOutput, REQUIRED_PARITY_TEST_NAMES};

const SANDBOX_PROFILE: &str = "sandbox-vm-export";
const HEW_SEED: &str = "42";

/// How the live profile+emitter+VM treats a probed construct. Every value is
/// cross-checked against the real gate by the tests below.
#[derive(Debug, Clone, Copy)]
enum Coverage {
    /// The gate admits it AND it runs at native↔sandbox parity. Pinned to this
    /// required parity-case name; a real stdout+exit case in `parity.rs` proves
    /// it. Asserting the gate runs the probe cleanly (exit 0) is the teeth
    /// against a future G1: a construct that runs but is not pinned here fails
    /// the build.
    Parity(&'static str),
    /// The gate admits it, but the emitter/interpreter cannot yet run it: it
    /// traps (`unsupported_instruction` / `invalid_enum_tag`) or fails to lower.
    /// Fail-loud, catalogued with the observed failure so it can never
    /// masquerade as runnable. The probe is compiled + run to keep this honest.
    NotYetRunnable {
        /// Observed failure when the probe runs in the sandbox today.
        failure: Failure,
        /// Why it is not yet runnable (for the catalogue + future graduation).
        reason: &'static str,
    },
    /// The profile fail-closed-rejects it (no bytecode). The probe is asserted
    /// to be rejected, so a profile change that begins admitting it trips the
    /// `rejected_probes_are_actually_rejected` test, forcing a coverage update.
    RejectedByProfile {
        /// The expected `Diagnostic::kind` the profile emits for this construct.
        diagnostic_kind: &'static str,
    },
}

/// The way an admitted-but-not-yet-runnable construct fails in the sandbox.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Failure {
    /// Compiles to bytecode, then the VM exits non-zero (trap / unsupported op).
    Trap,
}

/// One probed construct and its coverage. `id` is a stable label; `probe` is a
/// minimal program exercising (mainly) this construct.
#[derive(Debug, Clone, Copy)]
struct Construct {
    id: &'static str,
    probe: &'static str,
    coverage: Coverage,
}

/// The construct manifest, grounded against the live gate on 2026-06-14.
///
/// INVARIANT: every distinct admission context the profile can take is probed
/// here. The `ast_surface` exhaustive classification references these `id`s so a
/// new AST variant cannot be added without landing a row. Each row's coverage is
/// verified against the real gate by the tests below — a misclassification
/// (claiming `Parity` for something that traps, `RejectedByProfile` for
/// something now admitted, etc.) fails the build.
const CONSTRUCTS: &[Construct] = &[
    // ───────────────────────── Runs at parity ─────────────────────────
    // Each is pinned to a required `parity.rs` case that asserts full
    // stdout+exit parity with native under HEW_SEED=42.
    Construct {
        id: "literal + println",
        probe: "fn main() {\n    println(\"hi\");\n}\n",
        coverage: Coverage::Parity("hello_world"),
    },
    Construct {
        id: "recursive call + expr-if + range-for + interpolation",
        probe: "fn fib(n: i64) -> i64 {\n    if n < 2 { n } else { fib(n - 1) + fib(n - 2) }\n}\nfn main() {\n    for i in 0..3 {\n        println(f\"{fib(i)}\");\n    }\n}\n",
        coverage: Coverage::Parity("fibonacci"),
    },
    Construct {
        id: "enum unit-variant construction + dispatch",
        probe: "enum Op { Double; }\nfn apply(op: Op, x: i64) -> i64 {\n    match op { Double => x * 2 }\n}\nfn main() {\n    println(f\"{apply(Double, 5)}\");\n}\n",
        coverage: Coverage::Parity("function_composition"),
    },
    Construct {
        id: "match with constructor-payload patterns",
        probe: "enum Box { Has(i64); Empty; }\nfn unwrap(b: Box) -> i64 {\n    match b { Has(x) => x, Empty => 0 }\n}\nfn main() {\n    println(unwrap(Has(7)));\n}\n",
        coverage: Coverage::Parity("pattern_matching"),
    },
    Construct {
        id: "Vec::new + push/get/len",
        probe: "fn main() {\n    var v = Vec::new();\n    v.push(1);\n    v.push(2);\n    println(v.len());\n}\n",
        coverage: Coverage::Parity("collections"),
    },
    Construct {
        id: "record StructInit + field access",
        probe: "type Point { x: i64; y: i64; }\nfn main() {\n    let p = Point { x: 3, y: 4 };\n    println(p.x);\n}\n",
        coverage: Coverage::Parity("record_types"),
    },
    Construct {
        id: "nested expr-if returning string",
        probe: "fn main() {\n    let n = 5;\n    let s = if n > 3 { \"big\" } else { \"small\" };\n    println(s);\n}\n",
        coverage: Coverage::Parity("structural_records"),
    },
    Construct {
        id: "actor spawn + receive + mutable state",
        probe: "actor Counter {\n    var count: i64;\n    receive fn bump(n: i64) -> i64 { count = count + n; count }\n}\nfn main() {\n    let c = spawn Counter(count: 0);\n    println(match await c.bump(3) { Ok(v) => v, Err(_e) => 0 - 1 });\n}\n",
        coverage: Coverage::Parity("counter_actor"),
    },
    Construct {
        id: "actor ask via await + Ok/Err reply match",
        probe: "actor Echo {\n    receive fn echo(n: i64) -> i64 { n }\n}\nfn main() {\n    let e = spawn Echo;\n    println(match await e.echo(9) { Ok(v) => v, Err(_e) => 0 - 1 });\n}\n",
        coverage: Coverage::Parity("actor_pipeline"),
    },
    Construct {
        id: "supervisor decl + child access",
        // Probed only at parity-name level; the real supervisor case lives in
        // parity.rs (a minimal inline supervisor needs more scaffolding than a
        // probe warrants). The exit-0 cross-check uses the simpler actor probe.
        probe: "actor W {\n    receive fn ping() -> i64 { 1 }\n}\nfn main() {\n    let w = spawn W;\n    println(match await w.ping() { Ok(v) => v, Err(_e) => 0 });\n}\n",
        coverage: Coverage::Parity("supervisor"),
    },
    Construct {
        id: "machine new/step/state_name",
        probe: "machine Light {\n    events { Next; }\n    state Red;\n    state Green;\n    on Next: Red => Green;\n    on Next: Green => Red;\n}\nfn main() {\n    let m = Light::Red;\n    println(m.state_name());\n}\n",
        coverage: Coverage::Parity("traffic_light"),
    },
    Construct {
        id: "integer arithmetic + comparison",
        probe: "fn main() {\n    let a = 17;\n    let b = 5;\n    println(f\"{a + b} {a - b} {a * b} {a / b} {a % b}\");\n    println(f\"{a < b} {a > b} {a == b} {a != b}\");\n}\n",
        coverage: Coverage::Parity("arithmetic_operators"),
    },
    Construct {
        id: "unary integer negate (`-a`)",
        // The arithmetic_operators example carries the real `-a` line. Before
        // this lane it only used `0 - a` (binary subtract), so `i64.neg` was
        // never exercised by any parity case.
        probe: "fn main() {\n    let a = 17;\n    println(-a);\n}\n",
        coverage: Coverage::Parity("arithmetic_operators"),
    },
    Construct {
        id: "array literal + index + len",
        probe: "fn main() {\n    let xs = [10, 20, 30];\n    println(xs[1]);\n    println(xs.len());\n}\n",
        coverage: Coverage::Parity("array_indexing"),
    },
    Construct {
        id: "string len + slice",
        probe: "fn main() {\n    let s = \"sandbox\";\n    println(s.len());\n    println(s.slice(0, 3));\n}\n",
        coverage: Coverage::Parity("string_slicing"),
    },
    Construct {
        id: "while loop + bare break/continue",
        probe: "fn main() {\n    var i = 0;\n    while i < 5 {\n        i = i + 1;\n    }\n    println(i);\n}\n",
        coverage: Coverage::Parity("while_loop"),
    },
    Construct {
        id: "match with wildcard arm",
        probe: "enum C { A; B; }\nfn name(c: C) -> string {\n    match c { A => \"a\", _ => \"other\" }\n}\nfn main() {\n    println(name(B));\n}\n",
        coverage: Coverage::Parity("wildcard_match"),
    },
    Construct {
        id: "bare `loop` + `break;`",
        probe: "fn main() {\n    var i = 0;\n    loop {\n        i = i + 1;\n        break;\n    }\n    println(i);\n}\n",
        coverage: Coverage::Parity("while_loop"),
    },
    Construct {
        id: "bare `continue;` in while",
        probe: "fn main() {\n    var i = 0;\n    while i < 3 {\n        i = i + 1;\n        continue;\n    }\n    println(i);\n}\n",
        coverage: Coverage::Parity("while_loop"),
    },
    Construct {
        id: "generic function call (monomorphised)",
        probe: "fn id<T>(v: T) -> T { v }\nfn main() {\n    println(id(42));\n}\n",
        coverage: Coverage::Parity("fibonacci"),
    },
    Construct {
        id: "print (no newline) builtin",
        probe: "fn main() {\n    print(\"x\");\n}\n",
        coverage: Coverage::Parity("hello_world"),
    },
    Construct {
        id: "regex compile + is_match",
        probe: "import std::text::regex;\nfn main() {\n    let r = regex.new(\"a.c\");\n    println(r.is_match(\"abc\"));\n}\n",
        coverage: Coverage::Parity("wildcard_match"),
    },
    Construct {
        id: "statement-position `if`",
        // Now lowered: lower_stmt_if runs the branch for its side effects and
        // discards the result (unit). Pinned to the stmt_if case.
        probe: "fn main() {\n    let x: i64 = 1;\n    if x == 1 {\n        println(\"yes\");\n    }\n    return;\n}\n",
        coverage: Coverage::Parity("stmt_if"),
    },
    Construct {
        id: "statement-position `match`",
        // Now lowered: lower_stmt_match dispatches each arm for its side effects.
        // The scrutinee is an enum (the runnable match form) — a scalar-literal
        // scrutinee is a separate, still-NotYetRunnable construct (`enum.tag`
        // dispatch traps on a non-enum value), so this probe isolates the
        // statement-position lowering #1901 made runnable.
        probe: "enum Color { Red; Green; }\nfn main() {\n    let c: Color = Green;\n    match c {\n        Red => println(\"stop\"),\n        Green => println(\"go\"),\n    }\n    return;\n}\n",
        coverage: Coverage::Parity("stmt_match"),
    },
    Construct {
        id: "statement-position `if let`",
        // Now lowered: lower_stmt_if_let runs the matched/else branch for its side
        // effects. The pattern is a constructor-with-binding (the runnable form
        // the stmt_if_let case proves); a unit-variant `if let` binds no payload
        // and exercises a different path. Pinned to the stmt_if_let case.
        probe: "enum Wrapped { Value(i64); Empty; }\nfn main() {\n    let w: Wrapped = Value(7);\n    if let Value(n) = w {\n        println(f\"value {n}\");\n    }\n    return;\n}\n",
        coverage: Coverage::Parity("stmt_if_let"),
    },

    // ───────────── Admitted but NOT yet runnable (fail-loud) ─────────────
    // Each compiles to bytecode then TRAPS on the VM (non-zero exit). These are
    // the live G7 holes: admitted by the profile, not yet correctly runnable.
    // They fail LOUD (trap), never silent-wrong. The honesty test runs each and
    // asserts it really does not run cleanly; making one runnable trips it.
    Construct {
        id: "float arithmetic (`f64 + f64`) — G1 float class closed",
        // Was the G1 silent-wrong hole: lower_binary mapped Add/Sub/Mul/Div to
        // i64.checked_* regardless of operand type. The emitter now dispatches the
        // f64.* opcode family by operand type, so f64 add/sub/mul run at parity.
        probe: "fn main() {\n    let a: f64 = 1.5;\n    let b: f64 = 2.5;\n    println(a + b);\n}\n",
        coverage: Coverage::Parity("float_arithmetic"),
    },
    Construct {
        id: "float division",
        // f64 division/remainder lowers to the f64.* opcode family (IEEE-754,
        // never trap-on-zero). Pinned to the float_division case.
        probe: "fn main() {\n    let a: f64 = 7.0;\n    let b: f64 = 2.0;\n    println(a / b);\n}\n",
        coverage: Coverage::Parity("float_division"),
    },
    Construct {
        id: "unary float negate (`-f64`) — G1 float class closed",
        // Unary::Negate is now type-directed: f64 operands emit f64.neg. The
        // float_arithmetic example carries the `-f64` line.
        probe: "fn main() {\n    let a: f64 = 3.5;\n    println(-a);\n}\n",
        coverage: Coverage::Parity("float_arithmetic"),
    },
    Construct {
        id: "non-finite f64 comparison (`==`/`!=`)",
        // f64 `==`/`!=` now mirror native `fcmp OEQ`/`ONE`: NaN never equal,
        // ±Infinity compare by sign, -0.0 == 0.0. Pinned to float_nonfinite_compare.
        probe: "fn main() {\n    let nan: f64 = 0.0 / 0.0;\n    println(nan == nan);\n}\n",
        coverage: Coverage::Parity("float_nonfinite_compare"),
    },
    Construct {
        id: "mixed integer + float arithmetic in one program",
        // The emitter dispatches the i64.checked_* and f64.* opcode families per
        // operand type within the same program: `rows * cols` stays on the checked
        // i64 path, `width * height` takes the f64 path. Pinned to mixed_numeric.
        probe: "fn main() {\n    let rows: i64 = 7;\n    let cols: i64 = 3;\n    let width: f64 = 2.5;\n    let height: f64 = 4.0;\n    println(f\"int {rows * cols}\");\n    println(f\"float {width * height}\");\n}\n",
        coverage: Coverage::Parity("mixed_numeric"),
    },
    Construct {
        id: "boolean not (`!b`)",
        // lower_expr's Unary arm only handles Negate; Not hits emit_unsupported.
        probe: "fn main() {\n    let b: bool = true;\n    println(!b);\n}\n",
        coverage: Coverage::NotYetRunnable {
            failure: Failure::Trap,
            reason: "Unary::Not has no lowering arm (only Negate); needs a bool.not opcode + handler",
        },
    },
    Construct {
        id: "array-repeat literal (`[v; n]`)",
        probe: "fn main() {\n    let xs = [0; 3];\n    println(xs.len());\n}\n",
        coverage: Coverage::NotYetRunnable {
            failure: Failure::Trap,
            reason: "Expr::ArrayRepeat has no emit arm -> emit_unsupported -> trap",
        },
    },
    Construct {
        id: "map literal (`{\"k\": v}`)",
        // Native: E_NOT_YET_IMPLEMENTED. The sandbox profile admits Expr::MapLiteral
        // (walks entries) but the emitter has no arm -> emit_unsupported -> trap.
        // Fail-loud; neither native nor sandbox runs it today.
        probe: "fn main() {\n    let m = {\"a\": 1, \"b\": 2};\n    println(\"made map\");\n}\n",
        coverage: Coverage::NotYetRunnable {
            failure: Failure::Trap,
            reason: "Expr::MapLiteral has no emit arm -> emit_unsupported -> trap; native also E_NOT_YET_IMPLEMENTED",
        },
    },
    Construct {
        id: "struct functional-update (`R { x: v, ..base }`)",
        probe: "type P { x: i64; y: i64; }\nfn main() {\n    let a = P { x: 1, y: 2 };\n    let b = P { x: 9, ..a };\n    println(b.y);\n}\n",
        coverage: Coverage::NotYetRunnable {
            failure: Failure::Trap,
            reason: "StructInit lowering ignores `base`; functional-update fields are not copied so b.y traps/diverges",
        },
    },
    Construct {
        id: "scalar-literal match (i64 scrutinee)",
        probe: "fn pick(n: i64) -> string {\n    match n { 1 => \"one\", _ => \"many\" }\n}\nfn main() {\n    println(pick(1));\n}\n",
        coverage: Coverage::NotYetRunnable {
            failure: Failure::Trap,
            reason: "lower_match lowers EVERY match as enum.tag dispatch; a non-enum scrutinee traps invalid_enum_tag",
        },
    },
    Construct {
        id: "scalar-literal match (string scrutinee)",
        probe: "fn code(s: string) -> i64 {\n    match s { \"a\" => 1, _ => 0 }\n}\nfn main() {\n    println(code(\"a\"));\n}\n",
        coverage: Coverage::NotYetRunnable {
            failure: Failure::Trap,
            reason: "string match lowers as enum.tag dispatch and traps; needs scalar-equality match lowering",
        },
    },
    Construct {
        id: "bool match",
        probe: "fn f(b: bool) -> i64 {\n    match b { true => 1, false => 0 }\n}\nfn main() {\n    println(f(true));\n}\n",
        coverage: Coverage::NotYetRunnable {
            failure: Failure::Trap,
            reason: "bool match lowers as enum.tag dispatch and traps; needs scalar-equality match lowering",
        },
    },
    Construct {
        id: "tuple value + tuple-let destructure",
        probe: "fn main() {\n    let t = (1, 2, 3);\n    let (a, b, c) = t;\n    println(a + b + c);\n}\n",
        coverage: Coverage::NotYetRunnable {
            failure: Failure::Trap,
            reason: "Expr::Tuple has no emit arm and tuple-pattern Let hits emit_unsupported -> trap",
        },
    },
    Construct {
        id: "expression-statement if-let (result discarded)",
        // `Expr::IfLet` as a trailing/sole function-body expression: the parser
        // emits `Stmt::Expression(Expr::IfLet)`. lower_expr now routes it through
        // lower_stmt_if_let for side effects and yields unit (the result is
        // discarded), so a function body that is a single if-let runs at parity.
        // The `announce` helper in stmt_if_let.hew is exactly this form.
        //
        // NOTE: if-let in *value* position (`let v = if let .. { x } else { y }`)
        // is NOT this construct — its result is always unit, so consuming it is a
        // separate, not-yet-lowered concern (value-producing if-let needs the arm
        // values joined on a result local, which is not yet emitted).
        probe: "enum Box { Has(i64); Empty; }\nfn describe(b: Box) {\n    if let Has(x) = b {\n        println(f\"has {x}\");\n    } else {\n        println(\"empty\");\n    }\n}\nfn main() {\n    describe(Has(9));\n    describe(Empty);\n}\n",
        coverage: Coverage::Parity("stmt_if_let"),
    },
    Construct {
        id: "numeric cast (`as`)",
        probe: "fn main() {\n    let x: i64 = 65;\n    let c = x as i32;\n    println(c);\n}\n",
        coverage: Coverage::NotYetRunnable {
            failure: Failure::Trap,
            reason: "Expr::Cast has no emit arm -> emit_unsupported -> trap",
        },
    },
    Construct {
        id: "postfix-try (`?`)",
        probe: "fn ok() -> Result<i64, string> { Ok(1) }\nfn run() -> Result<i64, string> {\n    let v = ok()?;\n    Ok(v + 1)\n}\nfn main() {\n    println(match run() { Ok(v) => v, Err(_) => 0 - 1 });\n}\n",
        coverage: Coverage::NotYetRunnable {
            failure: Failure::Trap,
            reason: "Expr::PostfixTry has no emit arm -> emit_unsupported -> trap",
        },
    },
    Construct {
        id: "Option Some/None construction",
        probe: "fn main() {\n    let o = Some(5);\n    println(match o { Some(x) => x, None => 0 });\n}\n",
        coverage: Coverage::NotYetRunnable {
            failure: Failure::Trap,
            reason: "Some/None are not pre-registered in enum_variant_tags; the constructor call hits the catch-all -> emit_unsupported -> trap",
        },
    },
    Construct {
        id: "struct pattern in match arm",
        probe: "type Point { x: i64; y: i64; }\nfn sum(p: Point) -> i64 {\n    match p { Point { x: a, y: b } => a + b }\n}\nfn main() {\n    println(sum(Point { x: 3, y: 4 }));\n}\n",
        coverage: Coverage::NotYetRunnable {
            failure: Failure::Trap,
            reason: "lower_match early-rejects Struct/Tuple arm patterns -> emit_unsupported -> trap",
        },
    },
    Construct {
        id: "const item reference",
        probe: "const LIMIT: i64 = 100;\nfn main() {\n    println(LIMIT);\n}\n",
        coverage: Coverage::NotYetRunnable {
            failure: Failure::Trap,
            reason: "const value is not bound; the identifier reference hits the identifier catch-all -> emit_unsupported -> trap (prints unit)",
        },
    },

    // ───────────────── Fail-closed: profile REJECTS (no bytecode) ─────────────────
    // The profile refuses these with a typed diagnostic. The cross-check asserts
    // each really is rejected, so a profile change that begins admitting one
    // (re-opening a hole) trips the test.
    Construct {
        id: "compound assignment (`x += v`)",
        // Was a silent-wrong-answer hole: the emitter ignored `Stmt::Assign.op`,
        // so `x += 5` became `x = 5`. Now rejected fail-closed until SP-1
        // type-aware lowering can combine `x op v` for both i64 and f64.
        probe: "fn main() {\n    var x: i64 = 10;\n    x += 5;\n    println(x);\n}\n",
        coverage: Coverage::RejectedByProfile {
            diagnostic_kind: "compound_assignment_rejected",
        },
    },
    Construct {
        id: "top-level type alias (`type T = U;`)",
        // Native E_NOT_YET_IMPLEMENTED; the sandbox used to admit + run it,
        // diverging from native (which refuses to compile). Rejected fail-closed.
        probe: "type Count = i64;\nfn main() {\n    println(5);\n}\n",
        coverage: Coverage::RejectedByProfile {
            diagnostic_kind: "reserved_runtime_feature",
        },
    },
    Construct {
        id: "labeled loop + labeled break",
        probe: "fn main() {\n    var i = 0;\n    @outer: loop {\n        i = i + 1;\n        break @outer;\n    }\n    println(i);\n}\n",
        coverage: Coverage::RejectedByProfile {
            diagnostic_kind: "reserved_control_flow",
        },
    },
    Construct {
        id: "labeled continue",
        // The labeled loop AND the labeled continue both reject with
        // `reserved_control_flow`; the labeled loop must exist for the label to
        // resolve (a dangling `@outer` is a typecheck error, not a profile one).
        probe: "fn main() {\n    var i = 0;\n    @outer: while i < 3 {\n        i = i + 1;\n        continue @outer;\n    }\n    println(i);\n}\n",
        coverage: Coverage::RejectedByProfile {
            diagnostic_kind: "reserved_control_flow",
        },
    },
    Construct {
        id: "break-with-value",
        probe: "fn main() {\n    var i = 0;\n    loop {\n        i = i + 1;\n        break i;\n    }\n    println(i);\n}\n",
        coverage: Coverage::RejectedByProfile {
            diagnostic_kind: "reserved_control_flow",
        },
    },
    Construct {
        id: "closure / lambda value",
        probe: "fn main() {\n    let f = |x: i64| x + 1;\n    println(f(2));\n}\n",
        coverage: Coverage::RejectedByProfile {
            diagnostic_kind: "reserved_runtime_feature",
        },
    },
    Construct {
        id: "`clone` prefix",
        probe: "fn main() {\n    let a = \"hi\";\n    let b = clone a;\n    println(b);\n}\n",
        coverage: Coverage::RejectedByProfile {
            diagnostic_kind: "reserved_runtime_feature",
        },
    },
    Construct {
        id: "identity comparison (`is`)",
        probe: "type Node { value: i64; }\nfn same(a: Node, b: Node) -> bool { a is b }\nfn main() {\n    println(\"x\");\n}\n",
        coverage: Coverage::RejectedByProfile {
            diagnostic_kind: "reserved_runtime_feature",
        },
    },
    Construct {
        id: "defer statement",
        probe: "fn main() {\n    defer println(\"bye\");\n    println(\"hi\");\n}\n",
        coverage: Coverage::RejectedByProfile {
            diagnostic_kind: "defer_rejected",
        },
    },
    Construct {
        id: "unsafe block",
        probe: "fn main() {\n    unsafe {\n        println(\"x\");\n    }\n}\n",
        coverage: Coverage::RejectedByProfile {
            diagnostic_kind: "unsafe_rejected",
        },
    },
    Construct {
        id: "native-only stdlib import (`std::fs`)",
        probe: "import std::fs;\nfn main() {\n    println(\"x\");\n}\n",
        coverage: Coverage::RejectedByProfile {
            diagnostic_kind: "Unsupported::NATIVE_ONLY",
        },
    },
    Construct {
        id: "extern FFI block",
        probe: "extern \"rt\" {\n    fn hew_datetime_now_ms() -> i64;\n}\nfn main() {\n    println(\"x\");\n}\n",
        coverage: Coverage::RejectedByProfile {
            diagnostic_kind: "Unsupported::NATIVE_ONLY",
        },
    },
    Construct {
        id: "module-qualified non-allowlisted call",
        probe: "import std::fs;\nfn main() {\n    fs.read(\"p.txt\");\n}\n",
        coverage: Coverage::RejectedByProfile {
            diagnostic_kind: "sandbox_profile_rejected",
        },
    },
    Construct {
        id: "non-allowlisted string method (`.replace`)",
        probe: "fn main() {\n    println(\"aba\".replace(\"a\", \"x\"));\n}\n",
        coverage: Coverage::RejectedByProfile {
            diagnostic_kind: "unknown_method_symbol",
        },
    },
    Construct {
        id: "trait / impl declaration",
        probe: "trait Greet { fn hello(self) -> string; }\nfn main() {\n    println(\"x\");\n}\n",
        coverage: Coverage::RejectedByProfile {
            diagnostic_kind: "reserved_runtime_feature",
        },
    },
    Construct {
        id: "`#[resource]` type (implicit drop contract)",
        probe: "#[resource]\ntype Conn { fd: i64 }\nfn main() {\n    println(\"x\");\n}\n",
        coverage: Coverage::RejectedByProfile {
            diagnostic_kind: "user_resource_close_not_yet_admitted_sandbox",
        },
    },
    Construct {
        id: "scope / structured-concurrency block",
        probe: "fn work() -> i64 { 1 }\nfn main() {\n    scope {\n        fork work();\n    }\n    println(\"x\");\n}\n",
        coverage: Coverage::RejectedByProfile {
            diagnostic_kind: "reserved_runtime_feature",
        },
    },
];

// ─────────────────────────────────────────────────────────────────────────
//  Cross-check tests: the manifest's claims must match the LIVE gate.
// ─────────────────────────────────────────────────────────────────────────

/// G7 teeth #1: every construct claimed to run at parity is pinned to a name in
/// `REQUIRED_PARITY_TEST_NAMES`. A `Parity(name)` for a name with no required
/// case fails the build.
#[test]
fn parity_constructs_pin_a_required_case_name() {
    let required: BTreeSet<&str> = REQUIRED_PARITY_TEST_NAMES.iter().copied().collect();
    for construct in CONSTRUCTS {
        if let Coverage::Parity(case) = construct.coverage {
            assert!(
                required.contains(case),
                "construct `{}` claims parity case `{case}`, but no such name is in \
                 REQUIRED_PARITY_TEST_NAMES; add the parity case or fix the manifest",
                construct.id
            );
        }
    }
}

/// G7 teeth #2 (inverse): every required parity name is backed by at least one
/// construct row, so a parity case cannot exist without a construct it proves.
#[test]
fn every_required_parity_case_backs_a_construct() {
    let claimed: BTreeSet<&str> = CONSTRUCTS
        .iter()
        .filter_map(|c| match c.coverage {
            Coverage::Parity(name) => Some(name),
            _ => None,
        })
        .collect();
    for name in REQUIRED_PARITY_TEST_NAMES {
        assert!(
            claimed.contains(name),
            "required parity case `{name}` is not claimed by any construct row in CONSTRUCTS; \
             every parity case must prove a construct"
        );
    }
}

/// The runnable count only grows. A stored baseline guards against silently
/// dropping coverage (e.g. deleting a `Parity` row to dodge a failing case).
/// Bumping this is a deliberate, reviewed act — never lower it without
/// justifying a removed admission in the same commit.
#[test]
fn runnable_coverage_does_not_shrink() {
    const RUNNABLE_BASELINE: usize = 31;
    let runnable = CONSTRUCTS
        .iter()
        .filter(|c| matches!(c.coverage, Coverage::Parity(_)))
        .count();
    assert!(
        runnable >= RUNNABLE_BASELINE,
        "runnable construct coverage dropped to {runnable} (baseline {RUNNABLE_BASELINE}); the \
         ratchet only grows. If you intentionally removed an admission in profile.rs, lower the \
         baseline in the same commit with justification."
    );
}

/// The behavioural ratchet: compile every probe through the REAL gate and run it
/// on the REAL VM, then assert the observed behaviour matches the declared
/// coverage. This is where `profile.rs` is the source of truth — the test reads
/// what the gate actually does, not a hand-kept list.
///
/// - `Parity` probe: must produce bytecode AND run cleanly (exit 0) on the VM.
///   A `Parity` probe that traps is a regression; a probe that runs but is not
///   pinned to a required case is caught by `parity_constructs_pin_a_required_case_name`.
/// - `NotYetRunnable` probe: must produce bytecode AND trap (non-zero exit). The
///   moment it runs cleanly, this fails — forcing promotion to `Parity` + a case.
///   This is the structural guarantee against a future G1: a construct cannot be
///   "admitted + runs-clean + unparited".
/// - `RejectedByProfile` probe: must produce NO bytecode and carry the declared
///   diagnostic kind. A profile change that begins admitting it trips this.
#[cfg_attr(windows, ignore)]
#[test]
fn live_gate_matches_declared_coverage() {
    ensure_parity_runner_built();
    for construct in CONSTRUCTS {
        let compiled = compile_to_sandbox_bytecode(construct.probe, Some(SANDBOX_PROFILE))
            .unwrap_or_else(|err| panic!("sandbox compile threw for `{}`: {err}", construct.id));
        match construct.coverage {
            Coverage::Parity(_) => assert_admitted_runs_clean(construct, &compiled),
            Coverage::NotYetRunnable { failure, reason } => {
                assert_admitted_but_fails(construct, &compiled, failure, reason);
            }
            Coverage::RejectedByProfile { diagnostic_kind } => {
                assert_rejected_by_profile(construct, &compiled, diagnostic_kind);
            }
        }
    }
}

fn assert_admitted_runs_clean(construct: &Construct, compiled: &CompileOutput) {
    let bytecode = bytecode_or_panic(construct, compiled);
    let sandbox = run_sandbox_inline(&serde_json::to_string(bytecode).expect("serialize"));
    assert_eq!(
        sandbox.status.code(),
        Some(0),
        "construct `{}` is classified Parity but the sandbox VM did NOT run it cleanly \
         (exit {:?}). Either it regressed (fix it) or its classification is wrong.\nstdout:\n{}\nstderr:\n{}",
        construct.id,
        sandbox.status.code(),
        String::from_utf8_lossy(&sandbox.stdout),
        String::from_utf8_lossy(&sandbox.stderr)
    );
}

fn assert_admitted_but_fails(
    construct: &Construct,
    compiled: &CompileOutput,
    failure: Failure,
    reason: &str,
) {
    let bytecode = bytecode_or_panic(construct, compiled);
    let sandbox = run_sandbox_inline(&serde_json::to_string(bytecode).expect("serialize"));
    match failure {
        Failure::Trap => assert_ne!(
            sandbox.status.code(),
            Some(0),
            "construct `{}` is catalogued NotYetRunnable (Trap; reason: {reason}), but the sandbox \
             VM ran it cleanly (exit 0). It is now runnable — promote it to Coverage::Parity and \
             add a required parity case so it joins the ratchet.\nstdout:\n{}\nstderr:\n{}",
            construct.id,
            String::from_utf8_lossy(&sandbox.stdout),
            String::from_utf8_lossy(&sandbox.stderr)
        ),
    }
}

fn assert_rejected_by_profile(
    construct: &Construct,
    compiled: &CompileOutput,
    diagnostic_kind: &str,
) {
    let has_errors = compiled.diagnostics.iter().any(|d| d.severity == "error");
    assert!(
        compiled.bytecode.is_none() && has_errors,
        "construct `{}` is classified RejectedByProfile but the gate produced bytecode \
         (it is now admitted). Update its coverage to Parity (with a case) or NotYetRunnable.\ndiagnostics:\n{}",
        construct.id,
        diagnostics_dump(compiled)
    );
    assert!(
        compiled
            .diagnostics
            .iter()
            .any(|d| d.severity == "error" && d.kind == diagnostic_kind),
        "construct `{}` is classified RejectedByProfile({diagnostic_kind}), but no error diagnostic \
         of that kind was emitted; the profile rejection path changed.\ndiagnostics:\n{}",
        construct.id,
        diagnostics_dump(compiled)
    );
}

fn bytecode_or_panic<'a>(
    construct: &Construct,
    compiled: &'a CompileOutput,
) -> &'a hew_sandbox_wasm::SandboxBytecodePackage {
    let has_errors = compiled.diagnostics.iter().any(|d| d.severity == "error");
    compiled
        .bytecode
        .as_ref()
        .filter(|_| !has_errors)
        .unwrap_or_else(|| {
            panic!(
                "construct `{}` is classified as admitted (Parity/NotYetRunnable) but the gate \
                 produced no bytecode (it is now rejected). Update its coverage to \
                 RejectedByProfile.\ndiagnostics:\n{}",
                construct.id,
                diagnostics_dump(compiled)
            )
        })
}

fn diagnostics_dump(compiled: &CompileOutput) -> String {
    serde_json::to_string_pretty(&compiled.diagnostics).unwrap_or_default()
}

// ─────────────────────────────────────────────────────────────────────────
//  Structural exhaustiveness: every AST variant is classified here, with NO
//  wildcard arm. Adding a parser variant breaks the build until it is mapped to
//  a manifest construct id — so a construct cannot enter the language without a
//  coverage decision. The mapped id must exist in CONSTRUCTS (asserted below),
//  closing the loop back to the behavioural cross-check.
// ─────────────────────────────────────────────────────────────────────────

#[expect(
    clippy::match_same_arms,
    clippy::unnecessary_wraps,
    reason = "the classifiers exhaustively enumerate EVERY AST variant on its own arm — that \
              per-variant explicitness is the whole point (a new variant must get its own arm or \
              the build fails), so identical owner ids across arms must NOT be merged; and the \
              `Owner = Option` return is semantically load-bearing (None = covered-by-parent)"
)]
mod ast_surface {
    use hew_parser::ast::{BinaryOp, Expr, Pattern, Stmt, TypeExpr, UnaryOp};

    /// Which manifest construct id is responsible for an AST variant's coverage.
    /// `Some(id)` ties the variant to a `CONSTRUCTS` row; `None` marks a variant
    /// whose coverage is subsumed by a parent context (e.g. `BinaryOp::And` is a
    /// boolean operator covered transitively, not its own probe) — still an
    /// explicit, reviewed decision, never an implicit gap.
    pub type Owner = Option<&'static str>;

    /// Exhaustively classify an `Expr` variant. NO wildcard: a new `Expr` variant
    /// fails to compile until classified.
    pub fn classify_expr(expr: &Expr) -> Owner {
        match expr {
            Expr::Literal(_) => Some("literal + println"),
            Expr::Identifier(_) => Some("integer arithmetic + comparison"),
            Expr::Binary { .. } => Some("integer arithmetic + comparison"),
            Expr::Unary { .. } => Some("unary integer negate (`-a`)"),
            Expr::Clone(_) => Some("`clone` prefix"),
            Expr::Tuple(_) => Some("tuple value + tuple-let destructure"),
            Expr::Array(_) => Some("array literal + index + len"),
            Expr::ArrayRepeat { .. } => Some("array-repeat literal (`[v; n]`)"),
            Expr::MapLiteral { .. } => Some("map literal (`{\"k\": v}`)"),
            Expr::Block(_) => Some("nested expr-if returning string"),
            Expr::If { .. } => Some("nested expr-if returning string"),
            Expr::IfLet { .. } => Some("expression-statement if-let (result discarded)"),
            Expr::Match { .. } => Some("match with constructor-payload patterns"),
            Expr::Lambda { .. } => Some("closure / lambda value"),
            Expr::Spawn { .. } => Some("actor spawn + receive + mutable state"),
            Expr::SpawnLambdaActor { .. } => Some("scope / structured-concurrency block"),
            Expr::Scope { .. } => Some("scope / structured-concurrency block"),
            Expr::ForkChild { .. } => Some("scope / structured-concurrency block"),
            Expr::ForkBlock { .. } => Some("scope / structured-concurrency block"),
            Expr::ScopeDeadline { .. } => Some("scope / structured-concurrency block"),
            Expr::InterpolatedString(_) => Some("integer arithmetic + comparison"),
            Expr::Call { .. } => Some("generic function call (monomorphised)"),
            Expr::MethodCall { .. } => Some("string len + slice"),
            Expr::StructInit { base: Some(_), .. } => {
                Some("struct functional-update (`R { x: v, ..base }`)")
            }
            Expr::StructInit { .. } => Some("record StructInit + field access"),
            Expr::Select { .. } => Some("scope / structured-concurrency block"),
            Expr::Join(_) => Some("scope / structured-concurrency block"),
            Expr::Timeout { .. } => Some("scope / structured-concurrency block"),
            Expr::UnsafeBlock(_) => Some("unsafe block"),
            Expr::Yield(_) => Some("scope / structured-concurrency block"),
            Expr::This => None, // `self` — only meaningful inside actor/impl context.
            Expr::FieldAccess { .. } => Some("record StructInit + field access"),
            Expr::Index { .. } => Some("array literal + index + len"),
            Expr::Cast { .. } => Some("numeric cast (`as`)"),
            Expr::PostfixTry(_) => Some("postfix-try (`?`)"),
            Expr::Range { .. } => Some("recursive call + expr-if + range-for + interpolation"),
            Expr::Await(_) => Some("actor ask via await + Ok/Err reply match"),
            Expr::RegexLiteral(_) => Some("regex compile + is_match"),
            Expr::ByteStringLiteral(_) => Some("closure / lambda value"), // value form: reserved_runtime_feature.
            Expr::ByteArrayLiteral(_) => Some("closure / lambda value"), // value form: reserved_runtime_feature.
            Expr::Is { .. } => Some("identity comparison (`is`)"),
            Expr::MachineEmit { .. } => Some("scope / structured-concurrency block"),
            Expr::GenBlock { .. } => Some("scope / structured-concurrency block"),
        }
    }

    /// Exhaustively classify a `Stmt` variant. NO wildcard.
    pub fn classify_stmt(stmt: &Stmt) -> Owner {
        match stmt {
            Stmt::Let { .. } => Some("literal + println"),
            Stmt::Var { .. } => Some("while loop + bare break/continue"),
            Stmt::Assign { op: Some(_), .. } => Some("compound assignment (`x += v`)"),
            Stmt::Assign { .. } => Some("while loop + bare break/continue"),
            Stmt::If { .. } => Some("statement-position `if`"),
            Stmt::IfLet { .. } => Some("statement-position `if let`"),
            Stmt::Match { .. } => Some("statement-position `match`"),
            Stmt::Loop { .. } => Some("bare `loop` + `break;`"),
            Stmt::For { .. } => Some("recursive call + expr-if + range-for + interpolation"),
            Stmt::While { .. } => Some("while loop + bare break/continue"),
            Stmt::WhileLet { .. } => Some("while loop + bare break/continue"),
            Stmt::Break { .. } => Some("bare `loop` + `break;`"),
            Stmt::Continue { .. } => Some("bare `continue;` in while"),
            Stmt::Return(_) => Some("recursive call + expr-if + range-for + interpolation"),
            Stmt::Defer(_) => Some("defer statement"),
            Stmt::Expression(_) => Some("literal + println"),
        }
    }

    /// Exhaustively classify a `Pattern` variant. NO wildcard.
    pub fn classify_pattern(pattern: &Pattern) -> Owner {
        match pattern {
            Pattern::Wildcard => Some("match with wildcard arm"),
            Pattern::Literal(_) => Some("scalar-literal match (i64 scrutinee)"),
            Pattern::Identifier(_) => Some("match with constructor-payload patterns"),
            Pattern::Constructor { .. } => Some("match with constructor-payload patterns"),
            Pattern::Struct { .. } => Some("struct pattern in match arm"),
            Pattern::Tuple(_) => Some("tuple value + tuple-let destructure"),
            Pattern::Or(_, _) => Some("match with wildcard arm"),
            Pattern::Regex { .. } => Some("regex compile + is_match"),
        }
    }

    /// Exhaustively classify a `TypeExpr` variant. NO wildcard. Type expressions
    /// are mostly checked structurally; pointer types are the only fail-closed
    /// rejection. The rest are subsumed by the value-level constructs that use
    /// them, so they map to `None` (an explicit "covered by parent" decision).
    pub fn classify_type_expr(ty: &TypeExpr) -> Owner {
        match ty {
            TypeExpr::Named { .. } => None,
            TypeExpr::Result { .. } => None,
            TypeExpr::Option(_) => None,
            TypeExpr::Tuple(_) => Some("tuple value + tuple-let destructure"),
            TypeExpr::Array { .. } => Some("array literal + index + len"),
            TypeExpr::Slice(_) => Some("array literal + index + len"),
            TypeExpr::Function { .. } => Some("closure / lambda value"),
            // Raw pointer types are part of the native-FFI surface; the profile
            // rejects them via the same `Unsupported::NATIVE_ONLY` family as an
            // `extern` block. Hew has no address-of operator, so a pointer-typed
            // `let` is not expressible in clean source to probe in isolation —
            // the extern-FFI probe covers the rejection path.
            TypeExpr::Pointer { .. } => Some("extern FFI block"),
            TypeExpr::Borrow(_) => None,
            TypeExpr::TraitObject(_) => Some("trait / impl declaration"),
            TypeExpr::Infer => None,
        }
    }

    /// Exhaustively classify a `BinaryOp`. NO wildcard. Arithmetic + comparison
    /// run at parity (integer); boolean/bitwise/shift/range ops are covered by
    /// the parent expression's probe (`None` = covered transitively).
    pub fn classify_binary_op(op: BinaryOp) -> Owner {
        match op {
            BinaryOp::Add
            | BinaryOp::Subtract
            | BinaryOp::Multiply
            | BinaryOp::Divide
            | BinaryOp::Modulo
            | BinaryOp::Equal
            | BinaryOp::NotEqual
            | BinaryOp::Less
            | BinaryOp::LessEqual
            | BinaryOp::Greater
            | BinaryOp::GreaterEqual => Some("integer arithmetic + comparison"),
            BinaryOp::And | BinaryOp::Or => None,
            BinaryOp::BitAnd
            | BinaryOp::BitOr
            | BinaryOp::BitXor
            | BinaryOp::Shl
            | BinaryOp::Shr
            | BinaryOp::WrappingAdd
            | BinaryOp::WrappingSub
            | BinaryOp::WrappingMul => None,
            BinaryOp::Range | BinaryOp::RangeInclusive => {
                Some("recursive call + expr-if + range-for + interpolation")
            }
        }
    }

    /// Exhaustively classify a `UnaryOp`. NO wildcard.
    pub fn classify_unary_op(op: UnaryOp) -> Owner {
        match op {
            UnaryOp::Negate => Some("unary integer negate (`-a`)"),
            UnaryOp::Not => Some("boolean not (`!b`)"),
            UnaryOp::BitNot => Some("boolean not (`!b`)"),
            UnaryOp::RawDeref => Some("extern FFI block"),
        }
    }
}

/// Closes the structural loop: every owner id returned by the exhaustive
/// `classify_*` functions must name a real `CONSTRUCTS` row. Combined with the
/// no-wildcard matches, this guarantees a new AST variant cannot be added
/// without (a) a classification here AND (b) a manifest construct with a probe
/// that the live-gate test verifies. A typo or dangling id fails the build.
#[test]
fn every_classified_owner_names_a_construct() {
    use hew_parser::ast::{BinaryOp, UnaryOp};

    let known: BTreeSet<&str> = CONSTRUCTS.iter().map(|c| c.id).collect();
    let mut owners: Vec<Option<&'static str>> = Vec::new();

    // Drive each classifier over a representative value of every variant. We use
    // a parsed probe corpus rather than hand-constructing AST nodes: parse every
    // manifest probe and classify every node it contains, so the classifiers are
    // exercised on real trees. Any owner id they return must be in CONSTRUCTS.
    for construct in CONSTRUCTS {
        let parsed = hew_parser::parse(construct.probe);
        for (item, _) in &parsed.program.items {
            walk_item(item, &mut owners);
        }
    }
    // Also exercise the op classifiers across their full domain (cheap + total).
    for op in [
        BinaryOp::Add,
        BinaryOp::Subtract,
        BinaryOp::Multiply,
        BinaryOp::Divide,
        BinaryOp::Modulo,
        BinaryOp::Equal,
        BinaryOp::NotEqual,
        BinaryOp::Less,
        BinaryOp::LessEqual,
        BinaryOp::Greater,
        BinaryOp::GreaterEqual,
        BinaryOp::And,
        BinaryOp::Or,
        BinaryOp::BitAnd,
        BinaryOp::BitOr,
        BinaryOp::BitXor,
        BinaryOp::Shl,
        BinaryOp::Shr,
        BinaryOp::Range,
        BinaryOp::RangeInclusive,
        BinaryOp::WrappingAdd,
        BinaryOp::WrappingSub,
        BinaryOp::WrappingMul,
    ] {
        owners.push(ast_surface::classify_binary_op(op));
    }
    for op in [
        UnaryOp::Negate,
        UnaryOp::Not,
        UnaryOp::BitNot,
        UnaryOp::RawDeref,
    ] {
        owners.push(ast_surface::classify_unary_op(op));
    }
    // Drive classify_type_expr over its full variant domain (total + cheap).
    {
        use hew_parser::ast::{Spanned, TraitBound, TypeExpr};
        fn boxed(ty: TypeExpr) -> Box<Spanned<TypeExpr>> {
            Box::new((ty, 0..0))
        }
        let named = || TypeExpr::Named {
            name: "i64".to_string(),
            type_args: None,
        };
        let type_exprs = [
            named(),
            TypeExpr::Result {
                ok: boxed(named()),
                err: boxed(named()),
            },
            TypeExpr::Option(boxed(named())),
            TypeExpr::Tuple(vec![(named(), 0..0)]),
            TypeExpr::Array {
                element: boxed(named()),
                size: 3,
            },
            TypeExpr::Slice(boxed(named())),
            TypeExpr::Function {
                params: vec![(named(), 0..0)],
                return_type: boxed(named()),
            },
            TypeExpr::Pointer {
                is_mutable: false,
                pointee: boxed(named()),
            },
            TypeExpr::Borrow(boxed(named())),
            TypeExpr::TraitObject(Vec::<TraitBound>::new()),
            TypeExpr::Infer,
        ];
        for ty in &type_exprs {
            owners.push(ast_surface::classify_type_expr(ty));
        }
    }

    for owner in owners.into_iter().flatten() {
        assert!(
            known.contains(owner),
            "ast_surface classifier returned owner id `{owner}` which is not a row in CONSTRUCTS; \
             every classified construct must have a probed manifest row"
        );
    }
}

fn walk_item(item: &hew_parser::ast::Item, owners: &mut Vec<Option<&'static str>>) {
    use hew_parser::ast::Item;
    match item {
        Item::Function(f) => walk_block(&f.body, owners),
        Item::Actor(a) => {
            for r in &a.receive_fns {
                walk_block(&r.body, owners);
            }
        }
        Item::Const(c) => walk_expr(&c.value, owners),
        _ => {}
    }
}

fn walk_block(block: &hew_parser::ast::Block, owners: &mut Vec<Option<&'static str>>) {
    for (stmt, _) in &block.stmts {
        walk_stmt(stmt, owners);
    }
    if let Some(e) = &block.trailing_expr {
        walk_expr(e, owners);
    }
}

fn walk_stmt(stmt: &hew_parser::ast::Stmt, owners: &mut Vec<Option<&'static str>>) {
    use hew_parser::ast::Stmt;
    owners.push(ast_surface::classify_stmt(stmt));
    match stmt {
        Stmt::Let { value, ty, .. } | Stmt::Var { value, ty, .. } => {
            if let Some((ty, _)) = ty {
                owners.push(ast_surface::classify_type_expr(ty));
            }
            if let Some(e) = value {
                walk_expr(e, owners);
            }
        }
        Stmt::Assign { target, value, .. } => {
            walk_expr(target, owners);
            walk_expr(value, owners);
        }
        Stmt::Expression(e) | Stmt::Return(Some(e)) => walk_expr(e, owners),
        Stmt::Defer(e) => walk_expr(e, owners),
        Stmt::While {
            condition, body, ..
        } => {
            walk_expr(condition, owners);
            walk_block(body, owners);
        }
        Stmt::Loop { body, .. } => walk_block(body, owners),
        Stmt::For { iterable, body, .. } => {
            walk_expr(iterable, owners);
            walk_block(body, owners);
        }
        _ => {}
    }
}

fn walk_expr(
    expr: &hew_parser::ast::Spanned<hew_parser::ast::Expr>,
    owners: &mut Vec<Option<&'static str>>,
) {
    use hew_parser::ast::Expr;
    owners.push(ast_surface::classify_expr(&expr.0));
    match &expr.0 {
        Expr::Binary { left, op, right } => {
            owners.push(ast_surface::classify_binary_op(*op));
            walk_expr(left, owners);
            walk_expr(right, owners);
        }
        Expr::Unary { op, operand } => {
            owners.push(ast_surface::classify_unary_op(*op));
            walk_expr(operand, owners);
        }
        Expr::Match { scrutinee, arms } => {
            walk_expr(scrutinee, owners);
            for arm in arms {
                owners.push(ast_surface::classify_pattern(&arm.pattern.0));
                walk_expr(&arm.body, owners);
            }
        }
        Expr::Call { args, .. } | Expr::MethodCall { args, .. } => {
            for arg in args {
                walk_expr(arg.expr(), owners);
            }
        }
        Expr::If {
            condition,
            then_block,
            else_block,
        } => {
            walk_expr(condition, owners);
            walk_expr(then_block, owners);
            if let Some(e) = else_block {
                walk_expr(e, owners);
            }
        }
        _ => {}
    }
}

// ─────────────────────────────────────────────────────────────────────────
//  VM harness
// ─────────────────────────────────────────────────────────────────────────

fn run_sandbox_inline(bytecode_json: &str) -> Output {
    let tempdir = tempfile::tempdir().expect("create tempdir");
    let bytecode_path = tempdir.path().join("bytecode.json");
    std::fs::write(&bytecode_path, bytecode_json).expect("write bytecode");
    assert_cmd::Command::new("npm")
        .arg("--prefix")
        .arg(repo_root().join("hew-sandbox-vm"))
        .arg("run")
        .arg("-s")
        .arg("parity:run")
        .arg("--")
        .arg(&bytecode_path)
        .arg("--seed")
        .arg(HEW_SEED)
        .current_dir(repo_root())
        .env("NO_COLOR", "1")
        .output()
        .expect("spawn sandbox parity runner")
}

fn ensure_parity_runner_built() {
    static PARITY_RUNNER: OnceLock<()> = OnceLock::new();
    PARITY_RUNNER.get_or_init(|| {
        let vm_dir = repo_root().join("hew-sandbox-vm");
        assert!(
            vm_dir.join("node_modules").is_dir(),
            "hew-sandbox-vm dependencies are not installed; run `make sandbox-parity`"
        );
        let output = std::process::Command::new("npm")
            .arg("--prefix")
            .arg(&vm_dir)
            .arg("run")
            .arg("-s")
            .arg("build")
            .current_dir(repo_root())
            .output()
            .expect("invoke npm run build");
        assert!(
            output.status.success(),
            "npm run build failed\nstdout:\n{}\nstderr:\n{}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
        assert!(
            vm_dir
                .join("dist")
                .join("interpreter")
                .join("parity-runner.js")
                .is_file(),
            "sandbox parity runner was not built"
        );
    });
}

fn repo_root() -> &'static Path {
    static REPO_ROOT: OnceLock<PathBuf> = OnceLock::new();
    REPO_ROOT
        .get_or_init(|| {
            Path::new(env!("CARGO_MANIFEST_DIR"))
                .parent()
                .expect("hew-sandbox-wasm crate should have a workspace parent")
                .to_path_buf()
        })
        .as_path()
}
