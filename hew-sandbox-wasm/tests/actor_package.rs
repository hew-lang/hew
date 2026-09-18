//! The package carries what an actor program needs before the VM runs one.
//!
//! Actors still reach the AST emitter through the dispatch, so these pin the
//! walker directly: the tables and operations are what the VM will be fed, and
//! a gap here would otherwise only surface when the dispatch flips.

use hew_sandbox_wasm::sir_emit;

fn semantics(source: &str) -> hew_sir::LoweredModule {
    let state = hew_compile::run_source_frontend(
        source,
        "actor_package.hew",
        &hew_compile::FrontendOptions::default(),
    );
    let tco = state
        .typecheck_result
        .as_ref()
        .and_then(|result| result.tco.as_ref())
        .expect("the program type-checks");
    hew_compile::Session::new(
        hew_compile::SessionTarget::browser(),
        hew_compile::DiagnosticPolicy::default(),
    )
    .lower_program(&state.program, tco)
    .expect("the program lowers")
    .into_semantics()
}

const COUNTER: &str = r"
actor Counter {
    var count: i64,
    receive fn bump(n: i64) -> i64 { count = count + n; count }
}

fn main() {
    let c = spawn Counter(count: 0);
    println(match c.bump(3) { .Ok(v) => v, .Err(_e) => 0 - 1 });
}
";

#[test]
fn an_actor_declaration_reaches_the_package_with_its_handlers() {
    let module = semantics(COUNTER);
    assert!(
        sir_emit::uses_concurrency(&module.module),
        "an actor program is routed to the concurrency path"
    );
    let package = sir_emit::emit_package(&module.module, "sandbox-vm-export", "0", "test")
        .expect("an actor module projects into a package");

    let actor = package
        .actors
        .first()
        .expect("the actor reaches the package");
    assert_eq!(actor.state_fields.len(), 1, "one state seat");
    assert!(
        actor.state_fields[0].mutable,
        "`var count` is a mutable seat"
    );
    let handler = actor
        .handlers
        .first()
        .expect("the handler reaches the package");
    assert_eq!(handler.name, "bump");
    assert_eq!(handler.params, 1, "one message parameter");
    assert!(
        package.functions.get(handler.callable as usize).is_some(),
        "the handler names a function in this package"
    );
}

#[test]
fn an_actor_call_names_its_operation_rather_than_a_symbol() {
    let module = semantics(COUNTER);
    let package = sir_emit::emit_package(&module.module, "sandbox-vm-export", "0", "test")
        .expect("an actor module projects into a package");

    let operations: Vec<String> = package
        .functions
        .iter()
        .flat_map(|function| &function.blocks)
        .filter(|block| block.term["op"] == "actor.call")
        .filter_map(|block| block.term["operation"]["op"].as_str().map(str::to_string))
        .collect();
    assert!(
        operations.iter().any(|op| op == "spawn"),
        "spawning the actor is an operation the package names: {operations:?}"
    );
    assert!(
        !operations.is_empty(),
        "an actor program reaches at least one actor operation"
    );
}
