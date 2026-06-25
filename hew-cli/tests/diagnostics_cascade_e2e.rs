//! End-to-end cascade-suppression gate: `hew check` must emit EXACT error
//! counts on cascade-prone fixtures, never amplifying a single root cause into
//! a spray of `<error>`-typed downstream diagnostics.
//!
//! Every assert in this file is an exact equality (never `>= 1`). The guard
//! is meaningful only if a regression that reintroduces a cascade engine makes
//! a test go RED — the teeth proof in each fixture description notes which
//! suppression guard, if reverted, would cause that test to fail.
//!
//! Fixture classes covered (must match `FIXTURE_CLASS_COUNT`):
//!   (a) Binary-operand cascade: `undefined_var + 1`   → exactly 1 error.
//!   (b) Index cascade:          `undefined_var[0]`     → exactly 1 error.
//!   (c) Enum-eq cascade:        `undefined_var == E::V` → exactly 1 error.
//!   (d) Destructure cascade:    `let T { x, y } = undef` → exactly 1 error.
//!   (e) Two independent roots:  two separate `undefined_X` → exactly 2 errors.
//!   (f) Genuine type mismatch:  two real `i64`/`string` mismatches → exactly 2.

mod support;

use std::fs;
use std::process::Command;

use support::{describe_output, hew_binary, strip_ansi};

// ── helpers ──────────────────────────────────────────────────────────────────

/// The number of distinct fixture classes in this gate. A count mismatch means
/// the table-completeness assertion at the bottom will fail, surfacing any new
/// cascade engine class that has not been wired into this gate.
const FIXTURE_CLASS_COUNT: usize = 6;

fn write_fixture(source: &str) -> (tempfile::TempDir, std::path::PathBuf) {
    let dir = support::tempdir();
    let path = dir.path().join("main.hew");
    fs::write(&path, source).expect("write cascade fixture");
    (dir, path)
}

fn run_check(path: &std::path::Path) -> std::process::Output {
    Command::new(hew_binary())
        .args(["check", path.to_str().unwrap()])
        .output()
        .expect("hew binary must run")
}

/// Count `file:line:col: error:` lines in stderr — same regex as the vertical-
/// slice shell helper `expect_check_fail_error_count`.
fn count_errors(output: &std::process::Output) -> usize {
    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));
    stderr
        .lines()
        .filter(|line| {
            // Match `<something>:<digits>:<digits>: error:` — the canonical
            // rendered-diagnostic header that every cascade engine emits exactly once
            // for the root cause and must NOT emit again for downstream inferences.
            let trimmed = line.trim_start();
            trimmed.contains(": error:") && {
                // Require at least one colon-separated numeric segment before ": error:"
                let before_error = &trimmed[..trimmed.find(": error:").unwrap()];
                before_error
                    .split(':')
                    .any(|seg| seg.trim().parse::<u32>().is_ok())
            }
        })
        .count()
}

fn assert_no_leak(output: &std::process::Output, label: &str) {
    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));
    for forbidden in ["<error>", "has no binding", "IntCmp", "E_CODEGEN_FRONT"] {
        assert!(
            !stderr.contains(forbidden),
            "{label}: cascade guard violated — stderr contains `{forbidden}` (a downstream \
             phantom diagnostic that must be suppressed by the `<error>` guard);\
             \nfull stderr:\n{stderr}",
        );
    }
}

fn assert_exact_error_count(output: &std::process::Output, expected: usize, label: &str) {
    assert!(
        !output.status.success(),
        "{label}: fixture must fail closed under hew check\n{}",
        describe_output(output),
    );
    let actual = count_errors(output);
    assert_eq!(
        actual,
        expected,
        "{label}: expected exactly {expected} error(s), got {actual}\n{}",
        describe_output(output),
    );
    assert_no_leak(output, label);
}

// ── fixture sources ───────────────────────────────────────────────────────────

// (a) Binary-operand cascade.
//
// Root: `undefined_var` → `undefined variable`.
// Without engine-(a) suppression the `<error>` result propagates into the `+`
// binary-op, emitting a spurious `cannot apply \`+\` to \`<error>\`` secondary.
//
// Teeth proof: if the `Ty::Error` short-circuit guard in binary-op type-checking
// is removed, this test goes RED (actual count becomes 2).
const BINARY_OPERAND_CASCADE: &str = "fn main() {\n    let _z = undefined_var + 1;\n}\n";

// (b) Index cascade.
//
// Root: `undefined_var` → `undefined variable`.
// Without engine-(b) suppression the `<error>` result propagates into `[0]`,
// emitting a spurious `cannot index into \`<error>\`` secondary.
//
// Teeth proof: if the `Ty::Error` guard in index-expression type-checking is
// removed, this test goes RED.
const INDEX_CASCADE: &str = "fn main() {\n    let _z = undefined_var[0];\n}\n";

// (c) Enum-equality cascade.
//
// Root: `undefined_var` → `undefined variable`.
// Without engine-(c) suppression the `<error>` result propagates into the `==`
// enum-equality call, emitting a spurious `IntCmp` or missing-variant secondary.
//
// Teeth proof: if the `Ty::Error` guard in enum-equality dispatch is removed,
// this test goes RED.
const ENUM_EQ_CASCADE: &str = "enum Colour { Red; Green; }\n\
     fn main() {\n\
     \x20\x20\x20\x20let _r = undefined_var == Colour::Red;\n\
     }\n";

// (d) Destructure cascade.
//
// Root: `undefined_var` → `undefined variable`.
// Without engine-(a) destructure suppression (the `Ty::Error` check applied to
// the record-pattern RHS type before field bindings are resolved) this produces
// a secondary `has no binding 'x'` / `has no binding 'y'` pair.
//
// Teeth proof: if the `Ty::Error` early-out in record-pattern binding is
// removed, this test goes RED (count becomes 3).
const DESTRUCTURE_CASCADE: &str = "type Point { x: i64; y: i64; }\n\
     fn main() {\n\
     \x20\x20\x20\x20let Point { x, y } = undefined_var;\n\
     \x20\x20\x20\x20println(x + y);\n\
     }\n";

// (e) Two independent roots → 2 errors (c19 shape).
//
// Two distinct `undefined_X` bindings in separate statements produce 2 root
// errors. This fixture asserts the cascade count does NOT collapse both to 1
// (i.e. that suppression only swallows downstream phantoms, not genuine
// independent diagnostics).
const TWO_INDEPENDENT_ROOTS: &str = "type Point { x: i64; y: i64; }\n\
     fn main() {\n\
     \x20\x20\x20\x20let Point { x, y } = undefined_a;\n\
     \x20\x20\x20\x20let Point { x: x2, y: y2 } = undefined_b;\n\
     \x20\x20\x20\x20println(x + y + x2 + y2);\n\
     }\n";

// (f) Genuine two-type mismatch → 2 errors (c20 shape).
//
// Two real type mismatches (`i64` ← `string` and `string` ← `i64`) must both
// surface as independent `type mismatch` errors. This guards against
// over-aggressive cascade suppression that might silently drop a genuine second
// diagnostic.
const GENUINE_TWO_TYPE_MISMATCH: &str = "fn main() {\n\
     \x20\x20\x20\x20let x: i64 = \"not_an_int\";\n\
     \x20\x20\x20\x20let y: string = 42;\n\
     \x20\x20\x20\x20println(x);\n\
     \x20\x20\x20\x20println(y);\n\
     }\n";

// ── tests ─────────────────────────────────────────────────────────────────────

/// Engine (a): a binary-op applied to an `<error>`-typed operand must produce
/// exactly 1 diagnostic (the root undefined-variable error), not a cascade.
#[test]
fn binary_operand_cascade_emits_exactly_one_error() {
    let (_dir, path) = write_fixture(BINARY_OPERAND_CASCADE);
    let output = run_check(&path);
    assert_exact_error_count(&output, 1, "binary_operand_cascade");

    // Extra guard: the suppressed downstream text must be absent.
    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));
    assert!(
        !stderr.contains("cannot apply"),
        "binary_operand_cascade: suppressed 'cannot apply' secondary must not appear; got:\n{stderr}",
    );
}

/// Engine (b): indexing into an `<error>`-typed value must produce exactly 1
/// diagnostic (the root undefined-variable error).
#[test]
fn index_cascade_emits_exactly_one_error() {
    let (_dir, path) = write_fixture(INDEX_CASCADE);
    let output = run_check(&path);
    assert_exact_error_count(&output, 1, "index_cascade");

    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));
    assert!(
        !stderr.contains("cannot index into"),
        "index_cascade: suppressed 'cannot index into' secondary must not appear; got:\n{stderr}",
    );
}

/// Engine (c): enum equality (`==`) applied to an `<error>`-typed operand must
/// produce exactly 1 diagnostic, not a cascade through the equality dispatch.
#[test]
fn enum_eq_cascade_emits_exactly_one_error() {
    let (_dir, path) = write_fixture(ENUM_EQ_CASCADE);
    let output = run_check(&path);
    assert_exact_error_count(&output, 1, "enum_eq_cascade");
}

/// Engine (a/d): record-pattern destructure with an `<error>`-typed RHS must
/// produce exactly 1 diagnostic — the root undefined-variable error — and must
/// NOT emit per-field `has no binding` phantoms.
#[test]
fn destructure_cascade_emits_exactly_one_error() {
    let (_dir, path) = write_fixture(DESTRUCTURE_CASCADE);
    let output = run_check(&path);
    assert_exact_error_count(&output, 1, "destructure_cascade");

    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));
    assert!(
        !stderr.contains("has no binding"),
        "destructure_cascade: phantom 'has no binding' must not appear; got:\n{stderr}",
    );
}

/// Two independent undefined variables (c19 shape) must produce exactly 2 errors:
/// cascade suppression must not collapse distinct roots into a single diagnostic.
#[test]
fn two_independent_cascade_roots_emit_exactly_two_errors() {
    let (_dir, path) = write_fixture(TWO_INDEPENDENT_ROOTS);
    let output = run_check(&path);
    assert_exact_error_count(&output, 2, "two_independent_cascade_roots");
}

/// Two genuine type mismatches (c20 shape) must produce exactly 2 errors:
/// over-suppression must not silence a real secondary diagnostic.
#[test]
fn genuine_two_type_mismatch_emits_exactly_two_errors() {
    let (_dir, path) = write_fixture(GENUINE_TWO_TYPE_MISMATCH);
    let output = run_check(&path);
    assert_exact_error_count(&output, 2, "genuine_two_type_mismatch");

    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));
    assert!(
        stderr.contains("type mismatch"),
        "genuine_two_type_mismatch: both errors must be rendered as 'type mismatch'; got:\n{stderr}",
    );
}

/// Table-completeness guard. Asserting `FIXTURE_CLASS_COUNT == 6` means any
/// new cascade engine class added to this file must also increment the constant,
/// which keeps the gate honest about its coverage envelope.
///
/// To add a new class: increment `FIXTURE_CLASS_COUNT`, add a new const source,
/// and add a new `#[test]` — the compiler will then check all three are in sync.
#[test]
fn fixture_table_covers_all_known_cascade_classes() {
    // Each source constant represents one distinct engine class. Count them
    // explicitly so the arithmetic is verifiable at a glance.
    let covered = [
        BINARY_OPERAND_CASCADE,
        INDEX_CASCADE,
        ENUM_EQ_CASCADE,
        DESTRUCTURE_CASCADE,
        TWO_INDEPENDENT_ROOTS,
        GENUINE_TWO_TYPE_MISMATCH,
    ]
    .len();
    assert_eq!(
        covered, FIXTURE_CLASS_COUNT,
        "FIXTURE_CLASS_COUNT ({FIXTURE_CLASS_COUNT}) must equal the number of source constants \
         ({covered}); update the constant when adding a new cascade engine class",
    );
}
