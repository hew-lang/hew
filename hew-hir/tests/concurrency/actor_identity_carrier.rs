//! Tests for the actor defining-module identity carrier.
//!
//! Actor identity is `(defining-module, name)`: a `pub actor` lowered from a
//! package module carries `defining_module = Some(module_short)` on its
//! `HirActorDecl`, while a root-program actor carries `None`. The carrier is
//! what lets MIR layout keys and codegen symbol synthesis distinguish two
//! same-named actors from different modules; these tests pin the population
//! sites and the derived qualified key, including the root → bare mapping
//! that keeps single-module programs free of symbol churn.

use hew_hir::{lower_program_host_target, HirActorDecl, HirItem, ResolutionCtx};
use hew_parser::ast::{Item, Program};
use hew_parser::module::{Module, ModuleGraph, ModuleId};
use hew_types::{module_registry::ModuleRegistry, Checker, TypeCheckOutput};

/// Build a `Program` containing a non-root module `bank` with arbitrary
/// source. Mirrors the imported-free-fn harness shape.
fn build_program_with_imported_module(imported_src: &str, root_src: &str) -> Program {
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

    let imported_id = ModuleId::new(vec!["bank".to_string()]);
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
    graph.topo_order = vec![imported_id, root_id];

    Program {
        items: root.program.items,
        module_graph: Some(graph),
        ..root.program
    }
}

fn lower_with_checker(program: &Program) -> (hew_hir::LowerOutput, TypeCheckOutput) {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tco = checker.check_program(program);
    let output = lower_program_host_target(program, &tco, &ResolutionCtx);
    (output, tco)
}

fn find_actor<'a>(output: &'a hew_hir::LowerOutput, name: &str) -> &'a HirActorDecl {
    output
        .module
        .items
        .iter()
        .find_map(|item| {
            if let HirItem::Actor(actor) = item {
                (actor.name == name).then_some(actor)
            } else {
                None
            }
        })
        .unwrap_or_else(|| {
            let names: Vec<&str> = output
                .module
                .items
                .iter()
                .filter_map(|item| {
                    if let HirItem::Actor(actor) = item {
                        Some(actor.name.as_str())
                    } else {
                        None
                    }
                })
                .collect();
            panic!("missing actor `{name}`; actors present: {names:?}")
        })
}

/// A `pub actor` lowered from a package module carries its defining module;
/// a root-program actor carries `None`. The decl `name` stays bare in both
/// cases — qualification lives in the carrier, not the name.
#[test]
fn imported_actor_carries_defining_module_and_root_actor_carries_none() {
    let program = build_program_with_imported_module(
        "pub actor Worker {\n\
         \x20   var count: i64;\n\
         \x20   receive fn bump(n: i64) { count = count + n; }\n\
         }\n",
        "actor Local {\n\
         \x20   var total: i64;\n\
         \x20   receive fn poke() { total = total + 1; }\n\
         }\n\
         fn main() -> i64 { 0 }",
    );
    let (output, tco) = lower_with_checker(&program);
    assert!(tco.errors.is_empty(), "type errors: {:#?}", tco.errors);

    let imported = find_actor(&output, "Worker");
    assert_eq!(
        imported.defining_module.as_deref(),
        Some("bank"),
        "package-module actor must carry its defining module"
    );
    assert_eq!(imported.name, "Worker", "decl name stays bare");

    let local = find_actor(&output, "Local");
    assert_eq!(
        local.defining_module, None,
        "root-program actor identity is the root namespace"
    );
}

/// The qualified key derives dotted (`bank.Worker`) for module actors and
/// bare for root actors, and the symbol mangle of the bare form is the
/// identity mapping — the zero-churn guarantee for single-module programs.
#[test]
fn qualified_name_derives_dotted_for_module_and_bare_for_root() {
    let program = build_program_with_imported_module(
        "pub actor Worker {\n\
         \x20   var count: i64;\n\
         \x20   receive fn bump(n: i64) { count = count + n; }\n\
         }\n",
        "actor Local {\n\
         \x20   var total: i64;\n\
         \x20   receive fn poke() { total = total + 1; }\n\
         }\n\
         fn main() -> i64 { 0 }",
    );
    let (output, tco) = lower_with_checker(&program);
    assert!(tco.errors.is_empty(), "type errors: {:#?}", tco.errors);

    let imported = find_actor(&output, "Worker");
    assert_eq!(imported.qualified_name(), "bank.Worker");
    assert_eq!(
        hew_hir::mangle_dotted_name(&imported.qualified_name()),
        "bank$Worker",
        "module-actor symbols mangle through the dotted-name authority"
    );

    let local = find_actor(&output, "Local");
    assert_eq!(local.qualified_name(), "Local");
    assert_eq!(
        hew_hir::mangle_dotted_name(&local.qualified_name()),
        "Local",
        "root-actor qualified key mangles to the bare name unchanged"
    );
}
