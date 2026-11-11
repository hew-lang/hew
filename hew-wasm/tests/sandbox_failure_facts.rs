//! The browser frontend retains source declarations needed by actor errors.

#[test]
fn actor_call_retains_failure_declaration_facts() {
    let source = "actor Worker { receive fn run() -> i64 { 1 } } fn main() { let w = spawn Worker; let _ = w.run(); }";
    let output = hew_wasm::sandbox::compile_to_sandbox_bytecode(source, Some("sandbox-vm-export"))
        .expect("compile source");
    let Some(package) = output.bytecode else {
        panic!("{:?}", output.diagnostics);
    };
    let info = package
        .aggregates
        .iter()
        .find(|shape| shape.name.ends_with("CrashInfo"))
        .expect("the actor error retains its checked CrashInfo shape");
    assert!(package.managed_types.iter().any(|managed| matches!(
        &managed.layout,
        hew_wasm::sandbox::package::ManagedLayout::Aggregate { shape: Some(id), .. } if *id == info.id
    )), "managed layout names the same checked CrashInfo shape");
}
