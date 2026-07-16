//! Sandbox profile pins for record rest-patterns.
//!
//! A rest-carrying record pattern (`{ x, .. }`) reaches the sandbox profile and
//! emitter through the same `Pattern::Struct`/`RecordShorthand` field recursion
//! as an explicit wildcard (`{ x, y: _ }`). The `..` omits fields at the surface
//! but carries no runtime semantics the VM must model — omitted fields are
//! simply not visited. These pins assert the profile does not silently
//! mis-handle the rest carrier: it makes the SAME admit/reject decision for the
//! rest form as for the explicit-wildcard form (erasure), never a divergent
//! silent path.

use hew_sandbox_wasm::compile_to_sandbox_bytecode;

const PROFILE: &str = "sandbox-vm-export";

fn admitted(source: &str) -> bool {
    let output = compile_to_sandbox_bytecode(source, Some(PROFILE))
        .expect("sandbox compilation should not hard-error");
    let has_error = output.diagnostics.iter().any(|d| d.severity == "error");
    output.bytecode.is_some() && !has_error
}

#[test]
fn record_rest_matches_wildcard_profile_decision() {
    // Rest form: `{ x, .. }` omits `y`.
    let rest = r"
type Point {
    x: i64;
    y: i64;
}

fn main() {
    let p = Point { x: 3, y: 4 };
    match p {
        Point { x, .. } => println(x),
    }
}";
    // Explicit-wildcard form: `{ x, y: _ }` — the erasure target.
    let wildcard = r"
type Point {
    x: i64;
    y: i64;
}

fn main() {
    let p = Point { x: 3, y: 4 };
    match p {
        Point { x, y: _ } => println(x),
    }
}";
    assert_eq!(
        admitted(rest),
        admitted(wildcard),
        "the sandbox profile must make the same decision for `..` as for the \
         explicit-wildcard erasure target (no divergent silent path)"
    );
}

#[test]
fn record_rest_bare_is_handled() {
    // `{ .. }` alone binds nothing — the profile must not panic or diverge.
    let source = r"
type Point {
    x: i64;
    y: i64;
}

fn main() {
    let p = Point { x: 3, y: 4 };
    match p {
        Point { .. } => println(0),
    }
}";
    // Whatever the decision, it is total (no panic) and matches the all-wildcard
    // equivalent's admission.
    let all_wild = r"
type Point {
    x: i64;
    y: i64;
}

fn main() {
    let p = Point { x: 3, y: 4 };
    match p {
        Point { x: _, y: _ } => println(0),
    }
}";
    assert_eq!(admitted(source), admitted(all_wild));
}
