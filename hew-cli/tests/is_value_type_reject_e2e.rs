//! End-to-end coverage for `is` on a value type (#3108).
//!
//! `is` is reference identity on heap handles. A `type Point { ... }`
//! declaration is a copy-on-write value under the v0.5 value model
//! (`docs/v05/ownership.md`), so it has no identity to compare. The checker
//! owns that answer: `hew check` must report `E_IS_VALUE_TYPE` at the `is`
//! expression.
//!
//! Before the fix the checker admitted the program and it died later in the
//! codegen front with a span-less
//! `E_CODEGEN_FRONT_FAIL_CLOSED: … IdentityCompare lhs must be a pointer or
//! integer value` — a compiler-invariant message, not a user diagnostic. This
//! file pins both halves: the user-facing rejection appears, and the
//! fail-closed backstop never surfaces.

mod support;

use std::process::{Command, Output};

use support::{hew_binary, repo_root, strip_ansi, tempdir};

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
