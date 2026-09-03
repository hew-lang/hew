//! End-to-end coverage for `is` on a value type (#3108, #3134).
//!
//! `is` is reference identity on heap handles. A `type Point { ... }`
//! declaration is a copy-on-write value under the v0.5 value model
//! (`docs/v05/ownership.md`); an `enum` is a tagged value and a machine is a
//! tagged state. None of them has an identity to compare. The checker owns
//! that answer: `hew check` must report `E_IS_VALUE_TYPE` at the `is`
//! expression.
//!
//! Before the fix the checker admitted the program and it died later in the
//! codegen front with a span-less
//! `E_CODEGEN_FRONT_FAIL_CLOSED: … IdentityCompare lhs must be a pointer or
//! integer value` — a compiler-invariant message, not a user diagnostic. This
//! file pins both halves: the user-facing rejection appears, and the
//! fail-closed backstop never surfaces for any `is` program in the file,
//! accepted or rejected.

mod support;

use std::process::{Command, Output};

use support::{hew_binary, repo_root, require_codegen, run_bounded_hew_run, strip_ansi, tempdir};

/// Two records compared with `is` — the repro from #3108.
const RECORD_IS: &str = "type Point {\n\
     x: i64;\n\
     y: i64;\n\
     }\n\
     \n\
     fn main() {\n\
     let p = Point { x: 1, y: 2 };\n\
     let q = Point { x: 1, y: 2 };\n\
     let same: bool = p is q;\n\
     println(same);\n\
     }\n";

/// The same program written with `==`, the operator the diagnostic points at.
const RECORD_EQ: &str = "type Point {\n\
     x: i64;\n\
     y: i64;\n\
     }\n\
     \n\
     fn main() {\n\
     let p = Point { x: 1, y: 2 };\n\
     let q = Point { x: 1, y: 2 };\n\
     let same: bool = p == q;\n\
     println(same);\n\
     }\n";

/// Negative control: `is` on a heap handle stays accepted end to end, so the
/// rejection above is about the value class and not about `is` itself.
const VEC_IS: &str = "fn main() {\n\
     let v1: Vec<i64> = Vec.new();\n\
     let v2: Vec<i64> = Vec.new();\n\
     let same: bool = v1 is v2;\n\
     println(same);\n\
     }\n";

/// A fieldless enum — the #3134 repro.
const ENUM_IS: &str = "enum Colour {\n\
     Red;\n\
     Green;\n\
     }\n\
     \n\
     fn main() {\n\
     let a = Colour.Red;\n\
     let b = Colour.Green;\n\
     let same: bool = a is b;\n\
     println(same);\n\
     }\n";

/// A payload enum: carrying fields does not give a tagged value an address.
const PAYLOAD_ENUM_IS: &str = "enum Shape {\n\
     Circle(f64);\n\
     Square(f64);\n\
     }\n\
     \n\
     fn main() {\n\
     let a = Shape.Circle(1.0);\n\
     let b = Shape.Square(2.0);\n\
     let same: bool = a is b;\n\
     println(same);\n\
     }\n";

/// An `indirect` enum: the box is real, but `indirect` is a layout annotation
/// (HEW-SPEC-2026 §3.7.4) and `is` must not turn it into a semantic one, so
/// this is rejected like every other enum rather than answering from the
/// box's address.
const INDIRECT_ENUM_IS: &str = "indirect enum Expr {\n\
     Lit(i64);\n\
     Neg(Expr);\n\
     }\n\
     \n\
     fn main() {\n\
     let a = Expr.Lit(1);\n\
     let b = Expr.Lit(2);\n\
     let same: bool = a is b;\n\
     println(same);\n\
     }\n";

/// A two-state machine value.
const MACHINE_IS: &str = "machine Tank {\n\
     events {\n\
     Fill;\n\
     }\n\
     \n\
     state Filling;\n\
     state Draining;\n\
     \n\
     on Fill: Filling => Draining {\n\
     Tank.Draining\n\
     }\n\
     \n\
     default { state }\n\
     }\n\
     \n\
     fn main() {\n\
     let t = Filling;\n\
     let u = Draining;\n\
     let same: bool = t is u;\n\
     println(same);\n\
     }\n";

/// Negative control for the `bytes` half of #3134: `bytes` is a heap handle
/// and must run, not merely check.
///
/// A `bytes` value is a `BytesTriple { ptr, offset, len }` and `bytes.new()`
/// stores the all-zero triple — it allocates nothing until the first write
/// (`lower_bytes_constructor_call`). So both handles here name the same
/// absent buffer and the answer is `true`. Distinct buffers are the case
/// below.
const BYTES_IS: &str = "fn main() {\n\
     let a = bytes.new();\n\
     let b = bytes.new();\n\
     let same: bool = a is b;\n\
     println(same);\n\
     }\n";

/// The discriminating pair: once each handle has pushed a byte it owns its
/// own buffer, so the two are distinct and each is identical to itself. This
/// is what proves the comparison reads the buffer pointer instead of
/// answering `true` for every `bytes`.
const BYTES_DISTINCT_BUFFERS_IS: &str = "fn main() {\n\
     var a = bytes.new();\n\
     a.push(1);\n\
     var b = bytes.new();\n\
     b.push(1);\n\
     let distinct: bool = a is b;\n\
     let self_same: bool = a is a;\n\
     println(distinct);\n\
     println(self_same);\n\
     }\n";

/// Every `is` program this file compiles, rejected and accepted alike. The
/// codegen-front backstop must be unreachable from all of them.
const ALL_IS_SOURCES: &[(&str, &str)] = &[
    ("record", RECORD_IS),
    ("enum", ENUM_IS),
    ("payload enum", PAYLOAD_ENUM_IS),
    ("indirect enum", INDIRECT_ENUM_IS),
    ("machine", MACHINE_IS),
    ("Vec", VEC_IS),
    ("empty bytes", BYTES_IS),
    ("bytes with distinct buffers", BYTES_DISTINCT_BUFFERS_IS),
];

fn run_check(source: &str) -> Output {
    let dir = tempdir();
    let path = dir.path().join("main.hew");
    std::fs::write(&path, source).unwrap();

    Command::new(hew_binary())
        .arg("check")
        .arg(&path)
        .current_dir(repo_root())
        .output()
        .expect("failed to spawn hew check")
}

#[test]
fn is_on_a_record_is_rejected_by_the_checker() {
    let output = run_check(RECORD_IS);
    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));
    assert!(
        stderr.contains("E_IS_VALUE_TYPE"),
        "expected `hew check` to reject `p is q` with E_IS_VALUE_TYPE; got:\n{stderr}"
    );
    assert!(!output.status.success(), "expected a non-zero exit");
}

#[test]
fn is_on_a_record_never_reaches_the_codegen_front_backstop() {
    // The regression under test: the checker used to pass this program
    // through, and the only thing the user saw was a span-less
    // compiler-invariant message from the codegen front.
    let output = run_check(RECORD_IS);
    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));
    assert!(
        !stderr.contains("E_CODEGEN_FRONT_FAIL_CLOSED"),
        "the codegen-front fail-closed must be unreachable for `is` on a record; got:\n{stderr}"
    );
}

#[test]
fn is_rejection_points_at_the_users_source_line() {
    // A user diagnostic carries a location; the fail-closed message it
    // replaces carried none.
    let output = run_check(RECORD_IS);
    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));
    assert!(
        stderr
            .lines()
            .any(|line| line.contains("main.hew:") && line.contains("E_IS_VALUE_TYPE")),
        "expected the E_IS_VALUE_TYPE diagnostic to be attributed to main.hew; got:\n{stderr}"
    );
}

#[test]
fn structural_equality_on_the_same_record_is_accepted() {
    // Negative control for the diagnostic's advice: `==` really is the way to
    // compare two records, so the suggestion is not a dead end.
    let output = run_check(RECORD_EQ);
    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));
    assert!(
        output.status.success(),
        "`==` on two records must check clean; got:\n{stderr}"
    );
}

#[test]
fn is_on_a_heap_handle_is_still_accepted() {
    // Negative control for the rejection: `is` on a `Vec` handle is identity
    // comparison with a real answer and must keep checking clean.
    let output = run_check(VEC_IS);
    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));
    assert!(
        output.status.success(),
        "`is` on two `Vec` handles must check clean; got:\n{stderr}"
    );
}

#[test]
fn is_on_an_enum_is_rejected_by_the_checker() {
    assert_rejected_with_e_is_value_type(ENUM_IS, "Colour");
}

#[test]
fn is_on_a_payload_enum_is_rejected_by_the_checker() {
    assert_rejected_with_e_is_value_type(PAYLOAD_ENUM_IS, "Shape");
}

#[test]
fn is_on_an_indirect_enum_is_rejected_by_the_checker() {
    // `indirect` gets no carve-out: before #3134 this program compiled and
    // printed `false`, answering from the heap box that `indirect` allocates
    // and so leaking a layout annotation into the language's semantics.
    assert_rejected_with_e_is_value_type(INDIRECT_ENUM_IS, "Expr");
}

#[test]
fn is_on_a_machine_value_is_rejected_by_the_checker() {
    assert_rejected_with_e_is_value_type(MACHINE_IS, "Tank");
}

#[test]
fn no_is_program_reaches_the_codegen_front_backstop() {
    // The invariant the whole file exists for, stated once over every `is`
    // shape here: whatever a program does with `is`, the user never sees the
    // span-less compiler-invariant message. Rejected shapes are stopped by
    // the checker; accepted shapes compile. A new operand shape added to
    // `ALL_IS_SOURCES` is covered by this without a new test.
    for (label, source) in ALL_IS_SOURCES {
        let output = run_check(source);
        let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));
        assert!(
            !stderr.contains("E_CODEGEN_FRONT_FAIL_CLOSED"),
            "`is` on {label} must never reach the codegen-front fail-closed; got:\n{stderr}"
        );
    }
}

#[test]
fn is_on_two_empty_bytes_runs_and_reports_the_same_absent_buffer() {
    // The `bytes` half of #3134: `bytes` is a heap handle, so `is` must
    // compile and execute, not just type-check. `bytes.new()` allocates
    // nothing, so both handles name the same absent buffer.
    assert_run_prints(BYTES_IS, &["true"]);
}

#[test]
fn is_on_bytes_answers_from_the_buffer_pointer() {
    // The counterfactual for the test above: once each handle owns a buffer
    // the comparison separates them. Without this, a comparison that always
    // answered `true` would pass.
    assert_run_prints(BYTES_DISTINCT_BUFFERS_IS, &["false", "true"]);
}

/// Compile and run `source`, asserting it exits clean and prints exactly
/// `expected`, one entry per line.
fn assert_run_prints(source: &str, expected: &[&str]) {
    require_codegen();
    let dir = tempdir();
    let path = dir.path().join("main.hew");
    std::fs::write(&path, source).unwrap();

    let output = run_bounded_hew_run(&path, repo_root());
    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));
    assert!(
        output.status.success(),
        "`is` on `bytes` handles must run; got:\n{stderr}"
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    let lines: Vec<&str> = stdout.lines().map(str::trim).collect();
    assert_eq!(
        lines, expected,
        "unexpected `is` result; stdout:\n{stdout}\nstderr:\n{stderr}"
    );
}

/// Shared shape for the #3134 rejections: the checker refuses the program,
/// names the offending type, and points at `==`.
fn assert_rejected_with_e_is_value_type(source: &str, type_name: &str) {
    let output = run_check(source);
    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));
    assert!(!output.status.success(), "expected a non-zero exit");
    assert!(
        stderr.lines().any(|line| line.contains("main.hew:")
            && line.contains("E_IS_VALUE_TYPE")
            && line.contains(type_name)
            && line.contains("`==`")),
        "expected an E_IS_VALUE_TYPE at a main.hew location naming `{type_name}` and `==`; \
         got:\n{stderr}"
    );
}
