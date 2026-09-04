//! Sandbox capability boundary for primary-valid actor-handle identity.

use hew_sandbox_wasm::compile_to_sandbox_bytecode;

const PROFILE: &str = "sandbox-vm-export";
const ACTOR_HANDLE_IS: &str = r"
    actor Worker {
        let _id: i64;
        receive fn ping() {}
    }

    fn main() {
        let a = spawn Worker(_id: 1);
        let b = spawn Worker(_id: 2);
        let _same: bool = a is b;
    }
";

#[test]
fn actor_handle_is_reaches_capability_wall_without_bytecode() {
    let output = compile_to_sandbox_bytecode(ACTOR_HANDLE_IS, Some(PROFILE))
        .expect("sandbox compilation should return diagnostics, not an internal error");

    assert!(
        output
            .diagnostics
            .iter()
            .all(|diagnostic| !(diagnostic.severity == "error" && diagnostic.phase == "typecheck")),
        "actor-handle identity must pass the primary checker: {:#?}",
        output.diagnostics
    );
    assert!(
        output.bytecode.is_none(),
        "the sandbox must emit no bytecode for actor-handle identity"
    );
    assert!(
        output.diagnostics.iter().any(|diagnostic| {
            diagnostic.severity == "error"
                && diagnostic.phase == "profile"
                && diagnostic.kind == "reserved_runtime_feature"
        }),
        "actor-handle identity must reach the sandbox capability wall: {:#?}",
        output.diagnostics
    );
}
