//! Substrate test for HIR cross-module machine / enum binding.
//!
//! Verifies the HIR pre-pass walks `program.module_graph` (in addition to
//! `program.items`) so that qualified and bare-name references to a
//! machine declared in an imported module resolve to
//! `HirExprKind::MachineVariantCtor` rather than emitting
//! `HirDiagnosticKind::UnresolvedSymbol`.
//!
//! Companion to `examples/machine/run_cross_module_toggle.hew` — the
//! runnable fixture asserts the same property end-to-end; this test
//! asserts it at the substrate boundary so a regression is caught by a
//! cargo test before the run fixture is even executed.

use hew_hir::{HirExprKind, HirItem, HirStmtKind};
use hew_parser::ast::{Item, Program};
use hew_parser::module::{Module, ModuleGraph, ModuleId};

use crate::support;

/// Build a `Program` with a non-root module containing the imported
/// `Toggle` machine and a root module whose `main` references it via the
/// qualified `Toggle::Off` ctor and the bare `Flip` event.
fn build_cross_module_program() -> Program {
    let imported_src = r"
pub machine Toggle {
    events {
        Flip,
    }

    state Off,
    state On,
    on Flip: Off => On { On }
    on Flip: On => Off { Off }
}
";
    let root_src = r"
fn main() {
    var t = Toggle.Off;
    t.step(Flip);
}
";

    let imported = hew_parser::parse(imported_src);
    assert!(
        imported.errors.is_empty(),
        "imported parse errors: {:?}",
        imported.errors
    );
    let root = hew_parser::parse(root_src);
    assert!(
        root.errors.is_empty(),
        "root parse errors: {:?}",
        root.errors
    );

    // Build the module graph: root depends on `std::machines::toggle`.
    let imported_id = ModuleId::new(vec![
        "std".to_string(),
        "machines".to_string(),
        "toggle".to_string(),
    ]);
    let root_id = ModuleId::root();

    let imported_items: Vec<_> = imported
        .program
        .items
        .iter()
        .filter(|(item, _)| !matches!(item, Item::Import(_)))
        .cloned()
        .collect();

    let imported_module = Module {
        id: imported_id.clone(),
        items: imported_items,
        imports: Vec::new(),
        source_paths: Vec::new(),
        doc: None,
    };
    let root_module = Module {
        id: root_id.clone(),
        items: root.program.items.clone(),
        imports: Vec::new(),
        source_paths: Vec::new(),
        doc: None,
    };

    let mut graph = ModuleGraph::new(root_id.clone());
    graph.add_module(imported_module).expect("add imported");
    graph.add_module(root_module).expect("add root");
    // Topo order: dependency first, then root.
    graph.topo_order = vec![imported_id, root_id];

    Program {
        items: root.program.items,
        module_graph: Some(graph),
        ..root.program
    }
}

#[test]
fn cross_module_machine_ctor_resolves_to_machine_variant_ctor() {
    let program = build_cross_module_program();
    let output = support::checker_pipeline::lower_through_checker_from_program(&program);

    // No `UnresolvedSymbol` diagnostics for the imported ctor/event refs.
    let unresolved: Vec<_> = output
        .diagnostics
        .iter()
        .filter(|d| matches!(d.kind, hew_hir::HirDiagnosticKind::UnresolvedSymbol { .. }))
        .collect();
    assert!(
        unresolved.is_empty(),
        "expected no UnresolvedSymbol diagnostics; got: {unresolved:?}"
    );

    // The root `main` body should contain a `MachineVariantCtor` for
    // `Toggle::Off` — i.e. the qualified cross-module reference resolved
    // through `machine_ctor_registry` populated by the §4a module_graph
    // walk in `hew-hir/src/lower.rs`.
    let main_fn = output
        .module
        .items
        .iter()
        .find_map(|item| match item {
            HirItem::Function(f) if f.name == "main" => Some(f),
            _ => None,
        })
        .expect("root `main` function present in lowered module");

    let found = main_fn.body.statements.iter().any(|stmt| {
        if let HirStmtKind::Let(_, Some(expr)) = &stmt.kind {
            matches!(
                expr.kind,
                HirExprKind::MachineVariantCtor {
                    ref machine_name,
                    state_idx: 0,
                    ..
                } if machine_name == "std.machines.toggle.Toggle"
            )
        } else {
            false
        }
    });
    assert!(
        found,
        "expected exact imported `MachineVariantCtor {{ machine_name: \
         \"std.machines.toggle.Toggle\", state_idx: 0, .. }}` \
         binding in `main`; got items: {:#?}",
        main_fn.body.statements
    );

    // The imported `Toggle` machine itself must be re-emitted as
    // `HirItem::Machine` so MIR's `machine_layout_names` set includes it.
    let machine_present = output
        .module
        .items
        .iter()
        .any(|item| matches!(item, HirItem::Machine(m) if m.name == "Toggle"));
    assert!(
        machine_present,
        "expected imported `Toggle` machine re-emitted as `HirItem::Machine` \
         so MIR's machine_layout_names includes it"
    );
}

#[test]
fn imported_generic_machine_usage_discovers_a_concrete_layout() {
    let imported_src = r"
pub machine Lifecycle<T> {
    events { Stop, }
    state Created,
    state Stopped,
    on Stop: Created => Stopped { Stopped }
    on Stop: Stopped => Stopped,
}
";
    let root_src = r"
import m;

fn main() {
    var lifecycle: m.Lifecycle<i64> = m.Lifecycle.Created;
    lifecycle.step(m.LifecycleEvent.Stop);
}
";
    let program = support::checker_pipeline::program_with_imported_module(imported_src, root_src);
    let output = support::checker_pipeline::lower_through_checker_from_program(&program);
    assert!(output.diagnostics.is_empty(), "{:#?}", output.diagnostics);
    assert!(
        output.module.machine_instantiations.iter().any(|entry| {
            entry.key.origin_name == "m.Lifecycle"
                && entry.key.type_args == vec![hew_types::ResolvedTy::I64]
        }),
        "expected imported generic lifecycle layout: {:#?}",
        output.module.machine_instantiations
    );
    let main = output
        .module
        .items
        .iter()
        .find_map(|item| match item {
            HirItem::Function(function) if function.name == "main" => Some(function),
            _ => None,
        })
        .expect("root main must be lowered");
    let HirStmtKind::Expr(step) = &main.body.statements[1].kind else {
        panic!("expected lifecycle step expression")
    };
    let HirExprKind::MachineStep {
        receiver, event, ..
    } = &step.kind
    else {
        panic!("expected machine step, got {:#?}", step.kind)
    };
    assert!(matches!(
        receiver.ty,
        hew_types::ResolvedTy::Named { ref name, .. } if name == "m.Lifecycle"
    ));
    assert!(matches!(
        event.ty,
        hew_types::ResolvedTy::Named { ref name, .. } if name == "m.LifecycleEvent"
    ));
}

#[test]
fn private_imported_machine_retains_runtime_layout() {
    let imported_src = r"
machine RunLifecycle {
    events { Finish, }
    state Running,
    state Completed,
    on Finish: Running => Completed { Completed }
    default { state }
}

pub fn initial() -> RunLifecycle { RunLifecycle.Running }
";
    let root_src = "import m; fn main() {}";
    let program = support::checker_pipeline::program_with_imported_module(imported_src, root_src);
    let output = support::checker_pipeline::lower_through_checker_from_program(&program);
    assert!(output.diagnostics.is_empty(), "{:#?}", output.diagnostics);
    assert!(
        output.module.items.iter().any(
            |item| matches!(item, HirItem::Machine(machine) if machine.name == "RunLifecycle")
        ),
        "a private machine escaping through a public factory still needs its runtime layout"
    );
}

#[test]
fn dotted_imported_constructor_uses_the_source_owner() {
    let imported_src = r"
pub machine Toggle {
    events { Flip, }
    state Off,
    state On,
    on Flip: Off => On { On }
    on Flip: On => Off { Off }
}
";
    let root_src = r"
import m;

fn main() { let _value = m.Toggle.Off; }
";
    let program = support::checker_pipeline::program_with_imported_module(imported_src, root_src);
    let output = support::checker_pipeline::lower_through_checker_from_program(&program);
    assert!(output.diagnostics.is_empty(), "{:#?}", output.diagnostics);
    let main = output
        .module
        .items
        .iter()
        .find_map(|item| match item {
            HirItem::Function(function) if function.name == "main" => Some(function),
            _ => None,
        })
        .expect("root main must be lowered");
    assert!(matches!(
        &main.body.statements[0].kind,
        HirStmtKind::Let(
            _,
            Some(hew_hir::HirExpr {
                kind: HirExprKind::MachineVariantCtor {
                    machine_name,
                    state_idx: 0,
                    ..
                },
                ..
            })
        ) if machine_name == "m.Toggle"
    ));
}

#[test]
fn module_qualified_pattern_uses_the_source_owner() {
    let imported_src = r"
pub machine Toggle {
    events { Flip, }
    state Off,
    state On,
    on Flip: Off => On { On }
    on Flip: On => Off { Off }
}

pub fn initial() -> Toggle { Toggle.Off }
";
    let root_src = r"
import m;

fn tag(value: m.Toggle) -> i64 {
    match value {
        m.Toggle.Off => 0,
        m.Toggle.On => 1,
    }
}

fn main() { let _tag = tag(m.initial()); }
";
    let program = support::checker_pipeline::program_with_imported_module(imported_src, root_src);
    let output = support::checker_pipeline::lower_through_checker_from_program(&program);
    assert!(output.diagnostics.is_empty(), "{:#?}", output.diagnostics);
}
