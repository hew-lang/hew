//! Sandbox pins for `..operand` inside a bracket literal.
//!
//! A spread lowers the way `hew-hir` does on the native path: read the
//! operand's length, walk it by index, push each element onto the literal's
//! vector. These pins assert the sandbox admits the surface and actually
//! carries that walk into the package, rather than admitting the literal and
//! silently dropping the spliced elements.

use hew_wasm::sandbox::{compile_to_sandbox_bytecode, SandboxBytecodePackageV1};

const PROFILE: &str = "sandbox-vm-export";

const SPREAD: &str = r"
fn main() {
    let xs: Vec<i64> = [1, 2];
    let all = [..xs, 3];
    println(all.len());
}";

/// The hand-written program a spread stands for.
const WALK: &str = r"
fn main() {
    let xs: Vec<i64> = [1, 2];
    var all: Vec<i64> = Vec.new();
    for i in 0..xs.len() { all.push(xs[i]); }
    all.push(3);
    println(all.len());
}";

fn compile(source: &str) -> SandboxBytecodePackageV1 {
    let output = compile_to_sandbox_bytecode(source, Some(PROFILE))
        .expect("sandbox compilation should not hard-error");
    let errors: Vec<_> = output
        .diagnostics
        .iter()
        .filter(|diagnostic| diagnostic.severity == "error")
        .collect();
    assert!(errors.is_empty(), "unexpected sandbox errors: {errors:#?}");
    output.bytecode.expect("the source emits bytecode")
}

/// How many times the package calls one vector operation.
///
/// A `runtime.call` names its family by table index, so the count joins through
/// the package's own `runtime_families` rather than matching an opcode name.
fn vector_call_counts(package: &SandboxBytecodePackageV1, operation: &str) -> usize {
    let Some(family) = package.runtime_families.iter().find(|entry| {
        entry.family == "Vector"
            && entry.detail.as_ref().and_then(serde_json::Value::as_str) == Some(operation)
    }) else {
        return 0;
    };
    package
        .functions
        .iter()
        .flat_map(|function| &function.blocks)
        .filter(|block| block.term["op"] == "runtime.call" && block.term["family"] == family.id)
        .count()
}

#[test]
fn spread_emits_the_same_vector_walk_as_the_hand_written_loop() {
    let spread = compile(SPREAD);
    let walk = compile(WALK);

    for operation in ["Len", "Index", "Push"] {
        assert_eq!(
            vector_call_counts(&spread, operation),
            vector_call_counts(&walk, operation),
            "the spread makes the same number of Vector({operation}) calls as the walk it stands for"
        );
    }
}

#[test]
fn spread_does_not_silently_drop_its_operand() {
    // Negative control: without the emitter's spread arm the literal would
    // still be admitted, emitting only the plain element pushes and none of
    // the walk. Any one of these at zero means the elements went nowhere.
    let spread = compile(SPREAD);
    for operation in ["Len", "Index"] {
        assert!(
            vector_call_counts(&spread, operation) > 0,
            "the spread walks its operand: Vector({operation}) must appear"
        );
    }
}
