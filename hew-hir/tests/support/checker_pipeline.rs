#![allow(
    dead_code,
    reason = "shared integration-test helper module exposes variants used by different test crates"
)]

use hew_hir::{lower_program, lower_program_host_target, LowerOutput, ResolutionCtx, TargetArch};
use hew_parser::{
    ast::{Item, Program},
    module::{Module, ModuleGraph, ModuleId},
    ParseResult,
};
use hew_types::{module_registry::ModuleRegistry, Checker, TypeCheckOutput};

pub fn parse_source(source: &str) -> ParseResult {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    parsed
}

pub fn typecheck_source(source: &str) -> (ParseResult, TypeCheckOutput) {
    let parsed = parse_source(source);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tco = checker.check_program(&parsed.program);
    (parsed, tco)
}

pub fn lower_through_checker(source: &str) -> LowerOutput {
    let (parsed, tco) = typecheck_source(source);
    lower_program_host_target(&parsed.program, &tco, &ResolutionCtx)
}

pub fn lower_through_checker_from_program(program: &Program) -> LowerOutput {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tco = checker.check_program(program);
    lower_program_host_target(program, &tco, &ResolutionCtx)
}

/// Build a root program with an imported `m` module whose import item carries
/// the module's resolved source items, matching package/module loading.
pub fn program_with_imported_module(imported_src: &str, root_src: &str) -> Program {
    let imported = parse_source(imported_src);
    let mut root = parse_source(root_src);

    let imported_id = ModuleId::new(vec!["m".to_string()]);
    let root_id = ModuleId::root();
    let imported_items: Vec<_> = imported
        .program
        .items
        .iter()
        .filter(|(item, _)| !matches!(item, Item::Import(_)))
        .cloned()
        .collect();
    for (item, _) in &mut root.program.items {
        if let Item::Import(import) = item {
            if import.path == ["m"] {
                import.resolved_items = Some(imported_items.clone());
            }
        }
    }

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
    graph
        .add_module(imported_module)
        .expect("add imported module");
    graph.add_module(root_module).expect("add root module");
    graph.topo_order = vec![imported_id, root_id];

    Program {
        items: root.program.items,
        module_graph: Some(graph),
        ..root.program
    }
}

pub fn lower_through_checker_for_target(source: &str, target: TargetArch) -> LowerOutput {
    let (parsed, tco) = typecheck_source(source);
    lower_program(&parsed.program, &tco, &ResolutionCtx, target)
}

#[allow(
    dead_code,
    reason = "module-backed gate tests use this helper in follow-up coverage lanes"
)]
pub fn lower_through_checker_with_modules(source: &str) -> LowerOutput {
    let parsed = parse_source(source);
    let repo_root = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("hew-hir crate must live below repo root")
        .to_path_buf();
    let mut checker = Checker::new(ModuleRegistry::new(vec![repo_root]));
    let tco = checker.check_program(&parsed.program);
    lower_program_host_target(&parsed.program, &tco, &ResolutionCtx)
}
