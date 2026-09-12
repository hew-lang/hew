//! Sandbox pins for `..operand` inside a bracket literal.
//!
//! The sandbox emitter lowers a spread the way `hew-hir` does on the native
//! path: read the operand's length, walk it by index, push each element onto
//! the literal's vector. These pins assert the sandbox admits the surface and
//! actually emits that walk, rather than admitting the literal and silently
//! dropping the spliced elements.

use hew_sandbox_wasm::{compile_to_sandbox_bytecode, SandboxBytecodePackage};

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

fn compile(source: &str) -> SandboxBytecodePackage {
    let output = compile_to_sandbox_bytecode(source, Some(PROFILE))
        .expect("sandbox compilation should not hard-error");
    let errors: Vec<_> = output
        .diagnostics
        .iter()
        .filter(|diagnostic| diagnostic.severity == "error")
        .collect();
    assert!(errors.is_empty(), "unexpected sandbox errors: {errors:#?}");
    output.bytecode.expect("admitted source emits bytecode")
}

fn op_counts(bytecode: &SandboxBytecodePackage, op: &str) -> usize {
    bytecode
        .functions
        .iter()
        .flat_map(|function| &function.blocks)
        .flat_map(|block| &block.instructions)
        .filter(|instruction| instruction.op == op)
        .count()
}

#[test]
fn spread_emits_the_same_vector_walk_as_the_hand_written_loop() {
    let spread = compile(SPREAD);
    let walk = compile(WALK);

    for op in ["vector.len", "vector.index", "vector.push"] {
        assert_eq!(
            op_counts(&spread, op),
            op_counts(&walk, op),
            "the spread emits the same number of `{op}` instructions as the walk it stands for"
        );
    }
}

#[test]
fn spread_does_not_silently_drop_its_operand() {
    // Negative control: without the emitter's spread arm the literal would
    // still be admitted, emitting only the plain element pushes and none of
    // the walk. Any one of these at zero means the elements went nowhere.
    let spread = compile(SPREAD);
    for op in ["vector.len", "vector.index"] {
        assert!(
            op_counts(&spread, op) > 0,
            "the spread walks its operand: `{op}` must appear"
        );
    }
}
