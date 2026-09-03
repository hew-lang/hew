//! End-to-end coverage for `is` on a value type (#3108, #3134, D340).
//!
//! `is` is handle identity, admitted only for actor references
//! (`is_identity_capable`, HEW-SPEC-2026 §3.4.3's pid handle row). A
//! `type Point { ... }` declaration is a copy-on-write value under the v0.5
//! value model (`docs/v05/ownership.md`); an `enum` is a tagged value, a
//! machine is a tagged state, and `Vec`/`HashMap`/`HashSet`/`bytes` are
//! copy-on-write values with structural `==` (§3.4.3's value row, D340).
//! None of them has an identity to compare. The checker owns that answer:
//! `hew check` must report `E_IS_VALUE_TYPE` at the `is` expression.
//!
//! Before the #3108/#3134 fixes the checker admitted these programs and they
//! died later in the codegen front with a span-less
//! `E_CODEGEN_FRONT_FAIL_CLOSED: … IdentityCompare lhs must be a pointer or
//! integer value` — a compiler-invariant message, not a user diagnostic. This
//! file pins both halves: the user-facing rejection appears, and the
//! fail-closed backstop never surfaces for any `is` program in the file,
//! accepted or rejected.

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

/// Negative control: `is` on an actor handle stays accepted end to end, so
/// the rejection above is about the value class and not about `is` itself.
const ACTOR_IS: &str = "actor Worker {\n\
     let _id: i64;\n\
     receive fn ping() {}\n\
     }\n\
     \n\
     fn main() {\n\
     let a = spawn Worker(_id: 1);\n\
     let b = spawn Worker(_id: 2);\n\
     let same: bool = a is b;\n\
     println(same);\n\
     }\n";

/// `Vec` is a copy-on-write value (D340), rejected like an enum or record.
const VEC_IS: &str = "fn main() {\n\
     let v1: Vec<i64> = Vec.new();\n\
     let v2: Vec<i64> = Vec.new();\n\
     let same: bool = v1 is v2;\n\
     println(same);\n\
     }\n";

/// `HashMap` is a copy-on-write value (D340), rejected like `Vec`.
const HASHMAP_IS: &str = "fn main() {\n\
     let m1: HashMap<string, i64> = HashMap.new();\n\
     let m2: HashMap<string, i64> = HashMap.new();\n\
     let same: bool = m1 is m2;\n\
     println(same);\n\
     }\n";

/// `HashSet` is a copy-on-write value (D340), rejected like `Vec`.
const HASHSET_IS: &str = "fn main() {\n\
     let s1: HashSet<i64> = HashSet.new();\n\
     let s2: HashSet<i64> = HashSet.new();\n\
     let same: bool = s1 is s2;\n\
     println(same);\n\
     }\n";

/// `bytes` is a copy-on-write value (D340), rejected like `Vec`. A
/// predecessor of this fixture's PR admitted `bytes` to `is` and added
/// codegen support for its identity word (`BytesTriple` field-0
/// `ptrtoint`); that support became unreachable and was deleted with it.
const BYTES_IS: &str = "fn main() {\n\
     let a = bytes.new();\n\
     let b = bytes.new();\n\
     let same: bool = a is b;\n\
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

/// An `is` inside a closure with inferred parameters. The closure body is
/// checked before the call site unifies `a` and `b` with `Colour`, so the
/// operand types are still inference variables at the `is` and the checker
/// has to re-run its decision once inference settles. Annotating the
/// parameters was the difference between a user diagnostic and the span-less
/// codegen-front message.
const INFERRED_LAMBDA_ENUM_IS: &str = "enum Colour {\n\
     Red;\n\
     Green;\n\
     }\n\
     \n\
     fn main() {\n\
     let same = |a, b| a is b;\n\
     println(same(Colour.Red, Colour.Green));\n\
     }\n";

/// The same inference shape over the #3108 record, so the record answer is
/// not reachable through a closure either.
const INFERRED_LAMBDA_RECORD_IS: &str = "type Point {\n\
     x: i64;\n\
     }\n\
     \n\
     fn main() {\n\
     let same = |a, b| a is b;\n\
     println(same(Point { x: 1 }, Point { x: 2 }));\n\
     }\n";

/// Negative control for the two above: the same inferred closure over actor
/// handles stays accepted, so the deferred decision rejects the value class
/// rather than every inferred operand.
const INFERRED_LAMBDA_ACTOR_IS: &str = "actor Worker {\n\
     let _id: i64;\n\
     receive fn ping() {}\n\
     }\n\
     \n\
     fn main() {\n\
     let same = |a, b| a is b;\n\
     println(same(spawn Worker(_id: 1), spawn Worker(_id: 2)));\n\
     }\n";

/// Every `is` program this file checks, rejected and accepted alike. The
/// codegen-front backstop must be unreachable from all of them.
const ALL_IS_SOURCES: &[(&str, &str)] = &[
    ("record", RECORD_IS),
    ("enum", ENUM_IS),
    ("payload enum", PAYLOAD_ENUM_IS),
    ("indirect enum", INDIRECT_ENUM_IS),
    ("machine", MACHINE_IS),
    ("actor", ACTOR_IS),
    ("Vec", VEC_IS),
    ("HashMap", HASHMAP_IS),
    ("HashSet", HASHSET_IS),
    ("bytes", BYTES_IS),
    ("enum through an inferred closure", INFERRED_LAMBDA_ENUM_IS),
    (
        "record through an inferred closure",
        INFERRED_LAMBDA_RECORD_IS,
    ),
    (
        "actor through an inferred closure",
        INFERRED_LAMBDA_ACTOR_IS,
    ),
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
fn is_on_an_actor_handle_is_still_accepted() {
    // Negative control for the rejections in this file: `is` on an actor
    // handle is identity comparison with a real answer and must keep
    // checking clean.
    let output = run_check(ACTOR_IS);
    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));
    assert!(
        output.status.success(),
        "`is` on two actor handles must check clean; got:\n{stderr}"
    );
}

#[test]
fn is_on_a_vec_is_rejected_by_the_checker() {
    assert_rejected_with_e_is_value_type(VEC_IS, "Vec<i64>");
}

#[test]
fn is_on_a_hashmap_is_rejected_by_the_checker() {
    assert_rejected_with_e_is_value_type(HASHMAP_IS, "HashMap<string, i64>");
}

#[test]
fn is_on_a_hashset_is_rejected_by_the_checker() {
    assert_rejected_with_e_is_value_type(HASHSET_IS, "HashSet<i64>");
}

#[test]
fn is_on_bytes_is_rejected_by_the_checker() {
    assert_rejected_with_e_is_value_type(BYTES_IS, "bytes");
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
fn is_on_an_enum_through_an_inferred_closure_is_rejected_by_the_checker() {
    // The operand types are inference variables while the closure body is
    // checked, so the decision has to be re-run after unification. Without
    // that, this program type-checked and the user saw only the codegen
    // front's span-less `IdentityCompare lhs must be a pointer or integer
    // value`.
    assert_rejected_with_e_is_value_type(INFERRED_LAMBDA_ENUM_IS, "Colour");
}

#[test]
fn is_on_a_record_through_an_inferred_closure_is_rejected_by_the_checker() {
    assert_rejected_with_e_is_value_type(INFERRED_LAMBDA_RECORD_IS, "Point");
}

#[test]
fn is_on_an_actor_handle_through_an_inferred_closure_is_accepted() {
    // Negative control for the two rejections above: an inferred operand is
    // not itself the fault, so the same closure over two actor handles
    // checks clean.
    let output = run_check(INFERRED_LAMBDA_ACTOR_IS);
    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));
    assert!(
        output.status.success(),
        "`is` on two actor handles through an inferred closure must check clean; got:\n{stderr}"
    );
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

/// The four value-type reject fixtures under `tests/vertical-slice/reject/`
/// (D340), checked directly rather than through an inline source string —
/// the corpus these belong to is read by other tooling that scans that
/// directory, so the file form is pinned alongside the inline-source form
/// above.
#[test]
fn vertical_slice_reject_fixtures_are_rejected_with_e_is_value_type() {
    let fixtures: &[(&str, &str)] = &[
        ("is_on_vec.hew", "Vec<i64>"),
        ("is_on_hashmap.hew", "HashMap<string, i64>"),
        ("is_on_hashset.hew", "HashSet<i64>"),
        ("is_on_bytes.hew", "bytes"),
    ];
    for (fixture, type_name) in fixtures {
        let path = repo_root()
            .join("tests/vertical-slice/reject")
            .join(fixture);
        let output = Command::new(hew_binary())
            .arg("check")
            .arg(&path)
            .current_dir(repo_root())
            .output()
            .expect("failed to spawn hew check");
        let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));
        assert!(
            !output.status.success(),
            "expected {fixture} to be rejected; got:\n{stderr}"
        );
        assert!(
            stderr.contains("E_IS_VALUE_TYPE")
                && stderr.contains(type_name)
                && stderr.contains("`==`"),
            "expected {fixture} to report E_IS_VALUE_TYPE naming `{type_name}` and `==`; \
             got:\n{stderr}"
        );
    }
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
