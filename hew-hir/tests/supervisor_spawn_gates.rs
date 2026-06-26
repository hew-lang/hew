//! FC-P1-A3: Supervisor spawn args gate tests.
//!
//! `spawn AppSupervisor(...)` with non-empty init args is a HIR-level fail-
//! closed gate per slepp A222: the checker already rejects supervisor
//! declarations that take init params, but this defense-in-depth HIR gate
//! catches any future surface that could reach MIR
//! (`hew-mir/src/lower.rs:8852`) before the checker guard does.

use hew_hir::HirDiagnosticKind;
use hew_parser::ast::{Item, Program};
use hew_parser::module::{Module, ModuleGraph, ModuleId};

#[path = "support/mod.rs"]
mod support;

fn lower(source: &str) -> hew_hir::LowerOutput {
    support::checker_pipeline::lower_through_checker(source)
}

fn lower_two_module(program: &Program) -> hew_hir::LowerOutput {
    support::checker_pipeline::lower_through_checker_from_program(program)
}

/// `spawn Root` (no args) on a host target is accepted — no spawn-gate
/// diagnostic is emitted. The coroutine gate is also inactive on the host's
/// native arch so the test isolates the new gate.
#[test]
fn supervisor_spawn_no_args_accepted() {
    let source = r"
        supervisor Root {
            strategy: one_for_one;
            child worker: Worker();
        }
        actor Worker {
            receive fn work() {}
        }
        fn main() {
            let s = spawn Root;
        }
    ";
    let output = lower(source);

    let spawn_gate_hits: Vec<_> = output
        .diagnostics
        .iter()
        .filter(|d| {
            matches!(
                d.kind,
                HirDiagnosticKind::SupervisorSpawnArgsUnsupported { .. }
            )
        })
        .collect();
    assert!(
        spawn_gate_hits.is_empty(),
        "Expected no SupervisorSpawnArgsUnsupported diagnostic for `spawn Root` with no args, got: {spawn_gate_hits:?}"
    );
}

/// A config supervisor (`supervisor App(config: T)`) spawned with its config
/// arg (`spawn App(config: cfg)`) is ADMITTED — the gate rejects only args on a
/// no-config supervisor. This is the v0.6 init-closure surface.
#[test]
fn config_supervisor_spawn_with_config_arg_accepted() {
    let source = r"
        record AppConfig { size: i64 }
        actor Cache {
            var capacity: i64;
            receive fn get_cap() -> i64 { capacity }
        }
        supervisor App(config: AppConfig) {
            strategy: one_for_one;
            child cache: Cache(capacity: config.size);
        }
        fn main() {
            let cfg = AppConfig { size: 7 };
            let s = spawn App(config: cfg);
        }
    ";
    let output = lower(source);

    let spawn_gate_hits: Vec<_> = output
        .diagnostics
        .iter()
        .filter(|d| {
            matches!(
                d.kind,
                HirDiagnosticKind::SupervisorSpawnArgsUnsupported { .. }
            )
        })
        .collect();
    assert!(
        spawn_gate_hits.is_empty(),
        "a config supervisor spawned with its config arg must be admitted, got: {spawn_gate_hits:?}"
    );
}

/// `spawn Root(value: 1)` triggers the gate and the diagnostic is fatal
/// (surfaces through `into_result()` as `Err`).
#[test]
fn supervisor_spawn_with_args_rejected() {
    let source = r"
        supervisor Root {
            strategy: one_for_one;
            child worker: Worker();
        }
        actor Worker {
            receive fn work() {}
        }
        fn main() {
            let s = spawn Root(value: 1);
        }
    ";
    let output = lower(source);

    let spawn_gate_hits: Vec<_> = output
        .diagnostics
        .iter()
        .filter_map(|d| match &d.kind {
            HirDiagnosticKind::SupervisorSpawnArgsUnsupported { supervisor_name } => {
                Some(supervisor_name.as_str())
            }
            _ => None,
        })
        .collect();
    assert_eq!(
        spawn_gate_hits,
        vec!["Root"],
        "Expected exactly one SupervisorSpawnArgsUnsupported diagnostic naming `Root`, all diagnostics: {:?}",
        output.diagnostics
    );

    let result = output.into_result();
    assert!(
        result.is_err(),
        "into_result() should fail when SupervisorSpawnArgsUnsupported fires"
    );
}

/// `spawn Root(value: 1)` placed inside a `machine` transition body must
/// trigger the gate. The walker must visit `MachineTransition::body` (per
/// A242: HIR pre-pass walkers must cover ALL FOUR `Item::Machine` positions —
/// state.entry, state.exit, transition.guard, transition.body).
#[test]
fn supervisor_spawn_with_args_in_machine_transition_body_rejected() {
    let source = r"
        supervisor Root {
            strategy: one_for_one;
            child worker: Worker();
        }
        actor Worker {
            receive fn work() {}
        }
        machine M {
            events {
                Tick;
            }

            state Active;
            on Tick: Active => Active reenter {
                let s = spawn Root(value: 1);
                Active
            }
        }
        fn main() {}
    ";
    let output = lower(source);

    let spawn_gate_hits: Vec<&str> = output
        .diagnostics
        .iter()
        .filter_map(|d| match &d.kind {
            HirDiagnosticKind::SupervisorSpawnArgsUnsupported { supervisor_name } => {
                Some(supervisor_name.as_str())
            }
            _ => None,
        })
        .collect();
    assert_eq!(
        spawn_gate_hits,
        vec!["Root"],
        "Expected SupervisorSpawnArgsUnsupported for `Root` in machine transition body, all diagnostics: {:?}",
        output.diagnostics
    );
}

/// `spawn Root(value: 1)` placed inside a `machine` state `entry` block must
/// trigger the gate. Covers the `state.entry` position from the A242 four-
/// position invariant. (A symmetric `exit` test is omitted because the
/// walker handles entry and exit through the same code path; the parallel is
/// asserted structurally in `scan_item_for_supervisor_spawn`.)
#[test]
fn supervisor_spawn_with_args_in_machine_state_entry_rejected() {
    let source = r"
        supervisor Root {
            strategy: one_for_one;
            child worker: Worker();
        }
        actor Worker {
            receive fn work() {}
        }
        machine M {
            events {
                Tick;
            }

            state Idle {
                entry {
                    let s = spawn Root(value: 1);
                }
            }
            on Tick: Idle => Idle reenter { Idle }
        }
        fn main() {}
    ";
    let output = lower(source);

    let spawn_gate_hits: Vec<&str> = output
        .diagnostics
        .iter()
        .filter_map(|d| match &d.kind {
            HirDiagnosticKind::SupervisorSpawnArgsUnsupported { supervisor_name } => {
                Some(supervisor_name.as_str())
            }
            _ => None,
        })
        .collect();
    assert_eq!(
        spawn_gate_hits,
        vec!["Root"],
        "Expected SupervisorSpawnArgsUnsupported for `Root` in machine state.entry block, all diagnostics: {:?}",
        output.diagnostics
    );
}

/// `spawn Worker(value: 1)` on a *regular actor* (not a supervisor) must NOT
/// trigger the supervisor-spawn gate. The gate is supervisor-specific —
/// actor spawn with init args is a separate, supported path.
#[test]
fn actor_spawn_with_args_accepted() {
    let source = r"
        actor Worker {
            var value: int = 0;
            receive fn work() {}
        }
        fn main() {
            let w = spawn Worker(value: 1);
        }
    ";
    let output = lower(source);

    let spawn_gate_hits: Vec<_> = output
        .diagnostics
        .iter()
        .filter(|d| {
            matches!(
                d.kind,
                HirDiagnosticKind::SupervisorSpawnArgsUnsupported { .. }
            )
        })
        .collect();
    assert!(
        spawn_gate_hits.is_empty(),
        "Expected no SupervisorSpawnArgsUnsupported diagnostic for actor spawn, got: {spawn_gate_hits:?}"
    );
}

// ── Module-context threading regression tests ───────────────────────────────
//
// Rev2 finding: prior to the `current_module` thread, `scan_expr_for_supervisor_spawn`
// always consulted `registry.root` for bare `Expr::Spawn { target: Identifier }`,
// regardless of which module's body the walker was visiting. That produced
// two distinct bug classes the tests below pin:
//
//   1. False-negative: module-local supervisor `LocalSup` declared in
//      module `other` is spawned with args by a function in `other`. The
//      bare `Identifier("LocalSup")` failed to match because `LocalSup`
//      isn't in `registry.root`. Caught by
//      `module_local_supervisor_spawn_with_args_rejected`.
//
//   2. False-positive: root supervisor `Root` is declared, and module
//      `other` also declares an unrelated *actor* named `Root`. Module
//      `other`'s function does `spawn Root(args)` — which the checker
//      resolves to `other`'s actor (a legal actor-with-init-args
//      spawn). The walker should NOT fire the supervisor gate here.
//      Caught by `root_supervisor_name_does_not_false_positive_in_module`.

/// Build a `Program` whose root has the given items and whose `module_graph`
/// contains exactly one non-root module at `<module_short_name>` with the
/// given items. Mirrors the construction in
/// `tests/cross_module_machine_binding.rs::build_cross_module_program`.
fn build_two_module_program(root_src: &str, module_short_name: &str, module_src: &str) -> Program {
    let root_parsed = hew_parser::parser::parse(root_src);
    assert!(
        root_parsed.errors.is_empty(),
        "root parse errors: {:?}",
        root_parsed.errors
    );
    let module_parsed = hew_parser::parser::parse(module_src);
    assert!(
        module_parsed.errors.is_empty(),
        "module parse errors: {:?}",
        module_parsed.errors
    );

    let module_id = ModuleId::new(vec![module_short_name.to_string()]);
    let root_id = ModuleId::root();

    let module_items: Vec<_> = module_parsed
        .program
        .items
        .iter()
        .filter(|(item, _)| !matches!(item, Item::Import(_)))
        .cloned()
        .collect();

    let module_module = Module {
        id: module_id.clone(),
        items: module_items,
        imports: Vec::new(),
        source_paths: Vec::new(),
        doc: None,
    };
    let root_module = Module {
        id: root_id.clone(),
        items: root_parsed.program.items.clone(),
        imports: Vec::new(),
        source_paths: Vec::new(),
        doc: None,
    };

    let mut graph = ModuleGraph::new(root_id.clone());
    graph.add_module(module_module).expect("add module");
    graph.add_module(root_module).expect("add root");
    graph.topo_order = vec![module_id, root_id];

    Program {
        items: root_parsed.program.items,
        module_graph: Some(graph),
        ..root_parsed.program
    }
}

/// REV2 REGRESSION (false-negative): a supervisor declared in module
/// `other` and spawned with args by a function in module `other` MUST
/// trigger the gate. Before the rev2 `current_module` thread, this slipped
/// through because `LocalSup` wasn't in `registry.root` and the walker
/// only consulted `registry.root` for bare identifiers.
#[test]
fn module_local_supervisor_spawn_with_args_rejected() {
    // Module `other` declares its own supervisor + worker + a function
    // that spawns the supervisor with args. The root has only `main`,
    // and `LocalSup` is NOT declared at root — so the rev1 walker would
    // have missed this entirely.
    let root_src = r"
        fn main() {}
    ";
    let module_src = r"
        pub supervisor LocalSup {
            strategy: one_for_one;
            child worker: Worker();
        }
        pub actor Worker {
            receive fn work() {}
        }
        pub fn make() {
            let s = spawn LocalSup(value: 1);
        }
    ";
    let program = build_two_module_program(root_src, "other", module_src);
    let output = lower_two_module(&program);

    let spawn_gate_hits: Vec<&str> = output
        .diagnostics
        .iter()
        .filter_map(|d| match &d.kind {
            HirDiagnosticKind::SupervisorSpawnArgsUnsupported { supervisor_name } => {
                Some(supervisor_name.as_str())
            }
            _ => None,
        })
        .collect();
    assert_eq!(
        spawn_gate_hits,
        vec!["LocalSup"],
        "Expected SupervisorSpawnArgsUnsupported for module-local `LocalSup` spawn, all diagnostics: {:?}",
        output.diagnostics
    );
}

/// REV2 REGRESSION (false-positive): root declares supervisor `Root`,
/// module `other` declares an unrelated *actor* named `Root`. Module
/// `other`'s function does `spawn Root(args)` — which under module-local
/// resolution names `other`'s actor, a legal actor-init spawn. The
/// supervisor gate MUST NOT fire here. Before the rev2 thread, the
/// walker consulted `registry.root` for the bare identifier and falsely
/// matched the root's `Root` supervisor, rejecting valid code.
#[test]
fn root_supervisor_name_does_not_false_positive_in_module() {
    let root_src = r"
        supervisor Root {
            strategy: one_for_one;
            child worker: Worker();
        }
        actor Worker {
            receive fn work() {}
        }
        fn main() {}
    ";
    let module_src = r"
        // `Root` here is a regular actor that legitimately takes init args,
        // NOT a supervisor. Module-local bare-name resolution selects this
        // actor, so the supervisor-spawn gate must not match against the
        // root program's `Root` supervisor.
        pub actor Root {
            var value: int = 0;
            receive fn ping() {}
        }
        pub fn make() {
            let r = spawn Root(value: 1);
        }
    ";
    let program = build_two_module_program(root_src, "other", module_src);
    let output = lower_two_module(&program);

    let spawn_gate_hits: Vec<_> = output
        .diagnostics
        .iter()
        .filter(|d| {
            matches!(
                d.kind,
                HirDiagnosticKind::SupervisorSpawnArgsUnsupported { .. }
            )
        })
        .collect();
    assert!(
        spawn_gate_hits.is_empty(),
        "Expected no SupervisorSpawnArgsUnsupported diagnostic — module `other`'s \
         bare `Root` resolves to its local actor, not the root supervisor. Got: \
         {spawn_gate_hits:?}"
    );
}
