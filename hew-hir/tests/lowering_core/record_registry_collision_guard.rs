use hew_hir::{lower_program_host_target, HirDiagnosticKind, ResolutionCtx};
use hew_parser::ast::{Item, Program};
use hew_parser::module::{Module, ModuleGraph, ModuleId};
use hew_types::{module_registry::ModuleRegistry, Checker};

fn parsed_module(id: ModuleId, source: &str) -> Module {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors for {}: {:?}",
        id.path.join("."),
        parsed.errors
    );

    Module {
        id,
        items: parsed
            .program
            .items
            .into_iter()
            .filter(|(item, _)| !matches!(item, Item::Import(_)))
            .collect(),
        imports: Vec::new(),
        source_paths: Vec::new(),
        doc: None,
    }
}

fn program_with_colliding_imports_and_local_record() -> Program {
    let root_source = r"
type Thing { x: i64 }

fn main() -> i64 {
    let Thing { x } = Thing { x: 1 };
    x
}
";
    let root = hew_parser::parse(root_source);
    assert!(
        root.errors.is_empty(),
        "root parse errors: {:?}",
        root.errors
    );

    let a_id = ModuleId::new(vec!["a".to_string()]);
    let b_id = ModuleId::new(vec!["b".to_string()]);
    let root_id = ModuleId::root();
    let root_module = Module {
        id: root_id.clone(),
        items: root.program.items.clone(),
        imports: Vec::new(),
        source_paths: Vec::new(),
        doc: None,
    };

    let mut graph = ModuleGraph::new(root_id.clone());
    graph
        .add_module(parsed_module(a_id.clone(), "pub type Thing { a: i64 }"))
        .expect("add module a");
    graph
        .add_module(parsed_module(b_id.clone(), "pub type Thing { b: i64 }"))
        .expect("add module b");
    graph.add_module(root_module).expect("add root module");
    graph.topo_order = vec![a_id, b_id, root_id];

    Program {
        items: root.program.items,
        module_graph: Some(graph),
        ..root.program
    }
}

#[test]
fn root_local_record_shadows_colliding_imported_record_names() {
    let program = program_with_colliding_imports_and_local_record();
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let typecheck = checker.check_program(&program);
    assert!(
        typecheck.errors.is_empty(),
        "local `Thing` must shadow imported collisions: {:#?}",
        typecheck.errors
    );

    let lowered = lower_program_host_target(&program, &typecheck, &ResolutionCtx);
    let boundary_violations: Vec<_> = lowered
        .diagnostics
        .iter()
        .filter(|diagnostic| {
            matches!(
                diagnostic.kind,
                HirDiagnosticKind::CheckerBoundaryViolation { .. }
            )
        })
        .collect();
    assert!(
        boundary_violations.is_empty(),
        "root-local `Thing` destructuring must lower without a registry boundary violation: \
         {boundary_violations:#?}"
    );
}
