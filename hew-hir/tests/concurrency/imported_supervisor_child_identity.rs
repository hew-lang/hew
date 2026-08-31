//! File-imported actor identity at the supervisor boundary.

use std::path::PathBuf;

use hew_hir::{lower_program_host_target, HirActorDecl, HirItem, HirSupervisorDecl, ResolutionCtx};
use hew_parser::ast::{ImportDecl, ImportName, ImportSpec, Item, Program};
use hew_parser::module::{Module, ModuleGraph, ModuleId};
use hew_types::{module_registry::ModuleRegistry, Checker, TypeCheckOutput};

const WORKER_MODULE: &str = "imported_supervisor_child_support.worker";
const NAMED_WORKER_MODULE: &str = "services.workers";

fn file_import_program(
    imported_src: &str,
    root_src: &str,
) -> (Program, Vec<hew_parser::ast::Spanned<Item>>) {
    let imported = hew_parser::parse(imported_src);
    assert!(
        imported.errors.is_empty(),
        "imported parse errors: {:#?}",
        imported.errors
    );
    let root = hew_parser::parse(root_src);
    assert!(
        root.errors.is_empty(),
        "root parse errors: {:#?}",
        root.errors
    );

    let imported_items: Vec<_> = imported
        .program
        .items
        .into_iter()
        .filter(|(item, _)| !matches!(item, Item::Import(_)))
        .collect();
    let worker_path = PathBuf::from("/virtual/imported_supervisor_child_support/worker.hew");
    let mut root_items = root.program.items;
    let import = root_items
        .iter_mut()
        .find_map(|(item, _)| match item {
            Item::Import(import) if import.file_path.is_some() => Some(import),
            _ => None,
        })
        .expect("root file import");
    import.resolved_items = Some(imported_items.clone());
    import.resolved_item_source_paths = vec![worker_path.clone(); imported_items.len()];
    import.resolved_source_paths = vec![worker_path.clone()];

    let worker_id = ModuleId::new(WORKER_MODULE.split('.').map(str::to_string).collect());
    let root_id = ModuleId::root();
    let worker_module = Module {
        id: worker_id.clone(),
        items: imported_items.clone(),
        imports: Vec::new(),
        source_paths: vec![worker_path],
        doc: None,
    };
    let root_module = Module {
        id: root_id.clone(),
        items: root_items.clone(),
        imports: Vec::new(),
        source_paths: Vec::new(),
        doc: None,
    };
    let mut graph = ModuleGraph::new(root_id.clone());
    graph.add_module(worker_module).expect("add worker module");
    graph.add_module(root_module).expect("add root module");
    graph.topo_order = vec![worker_id, root_id];

    (
        Program {
            items: root_items,
            module_graph: Some(graph),
            module_doc: root.program.module_doc,
        },
        imported_items,
    )
}

fn lower_file_import(
    imported_src: &str,
    root_src: &str,
) -> (hew_hir::LowerOutput, TypeCheckOutput) {
    lower_file_import_with(imported_src, root_src, |_| {})
}

fn lower_file_import_with(
    imported_src: &str,
    root_src: &str,
    prepare_checked: impl FnOnce(&mut TypeCheckOutput),
) -> (hew_hir::LowerOutput, TypeCheckOutput) {
    let (mut program, imported_items) = file_import_program(imported_src, root_src);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let mut tc_output = checker.check_program(&program);
    prepare_checked(&mut tc_output);
    // `hew-compile` performs this source-order splice after checking. HIR sees
    // both the checker-authored module graph and the flattened tail entries.
    program.items.extend(imported_items);
    let lowered = lower_program_host_target(&program, &tc_output, &ResolutionCtx);
    (lowered, tc_output)
}

fn actor_import_program(
    spec: Option<ImportSpec>,
    module_alias: Option<&str>,
    root_tail: &str,
) -> Program {
    let imported = hew_parser::parse(
        "pub actor Worker {\n\
         \x20   let id: i64;\n\
         \x20   receive fn identify() -> i64 { id }\n\
         }\n",
    );
    assert!(imported.errors.is_empty(), "{:#?}", imported.errors);
    let root = hew_parser::parse(root_tail);
    assert!(root.errors.is_empty(), "{:#?}", root.errors);

    let import_item = (
        Item::Import(ImportDecl {
            path: NAMED_WORKER_MODULE.split('.').map(str::to_string).collect(),
            spec,
            selection_trailing_comma: false,
            module_alias: module_alias.map(str::to_string),
            file_path: None,
            resolved_items: Some(imported.program.items.clone()),
            resolved_item_source_paths: Vec::new(),
            resolved_source_paths: Vec::new(),
        }),
        0..0,
    );
    let worker_id = ModuleId::new(NAMED_WORKER_MODULE.split('.').map(str::to_string).collect());
    let root_id = ModuleId::root();
    let mut root_items = vec![import_item];
    root_items.extend(root.program.items.clone());
    let mut graph = ModuleGraph::new(root_id.clone());
    graph
        .add_module(Module {
            id: worker_id.clone(),
            items: imported.program.items,
            imports: Vec::new(),
            source_paths: Vec::new(),
            doc: None,
        })
        .expect("add named worker module");
    graph
        .add_module(Module {
            id: root_id.clone(),
            items: root_items.clone(),
            imports: Vec::new(),
            source_paths: Vec::new(),
            doc: None,
        })
        .expect("add root module");
    graph.topo_order = vec![worker_id, root_id];

    Program {
        items: root_items,
        module_graph: Some(graph),
        module_doc: root.program.module_doc,
    }
}

fn named_import_program(alias: Option<&str>, root_tail: &str) -> Program {
    actor_import_program(
        Some(ImportSpec::Names(vec![ImportName {
            name: "Worker".to_string(),
            alias: alias.map(str::to_string),
        }])),
        None,
        root_tail,
    )
}

fn lower_named_import(
    alias: Option<&str>,
    root_tail: &str,
) -> (hew_hir::LowerOutput, TypeCheckOutput) {
    let program = named_import_program(alias, root_tail);
    let mut type_checker = Checker::new(ModuleRegistry::new(vec![]));
    let checked = type_checker.check_program(&program);
    let lowered = lower_program_host_target(&program, &checked, &ResolutionCtx);
    (lowered, checked)
}

fn lower_whole_module_import(root_tail: &str) -> (hew_hir::LowerOutput, TypeCheckOutput) {
    let program = actor_import_program(None, None, root_tail);
    let mut type_checker = Checker::new(ModuleRegistry::new(vec![]));
    let checked = type_checker.check_program(&program);
    let lowered = lower_program_host_target(&program, &checked, &ResolutionCtx);
    (lowered, checked)
}

fn actor<'a>(output: &'a hew_hir::LowerOutput, name: &str) -> &'a HirActorDecl {
    output
        .module
        .items
        .iter()
        .find_map(|item| match item {
            HirItem::Actor(actor) if actor.name == name => Some(actor),
            _ => None,
        })
        .unwrap_or_else(|| panic!("missing actor `{name}`"))
}

fn supervisor<'a>(output: &'a hew_hir::LowerOutput, name: &str) -> &'a HirSupervisorDecl {
    output
        .module
        .items
        .iter()
        .find_map(|item| match item {
            HirItem::Supervisor(supervisor) if supervisor.name == name => Some(supervisor),
            _ => None,
        })
        .unwrap_or_else(|| panic!("missing supervisor `{name}`"))
}

#[test]
fn file_imported_supervisor_child_and_protocol_share_full_actor_identity() {
    let (output, checked) = lower_file_import(
        "pub actor ImportedWorker {\n\
         \x20   let id: i64;\n\
         \x20   receive fn identify() -> i64 { id }\n\
         }\n",
        "import \"imported_supervisor_child_support/worker.hew\";\n\
         supervisor ImportedWorkerPool {\n\
         \x20   child worker: ImportedWorker(id: 17);\n\
         }\n",
    );
    assert!(
        checked.errors.is_empty(),
        "type errors: {:#?}",
        checked.errors
    );
    assert!(
        output.diagnostics.is_empty(),
        "HIR diagnostics: {:#?}",
        output.diagnostics
    );

    let imported = actor(&output, "ImportedWorker");
    let expected = format!("{WORKER_MODULE}.ImportedWorker");
    assert_eq!(imported.qualified_name(), expected);
    let descriptor = imported
        .protocol_descriptor
        .as_ref()
        .expect("file-imported handler must retain its checker descriptor");
    assert_eq!(descriptor.actor_name, expected);

    let pool = supervisor(&output, "ImportedWorkerPool");
    assert_eq!(pool.children.len(), 1);
    assert_eq!(pool.children[0].ty, imported.qualified_name());
}

#[test]
fn file_imported_actor_cycle_capability_uses_full_checker_identity() {
    let expected = format!("{WORKER_MODULE}.CyclicImported");
    let (output, checked) = lower_file_import_with(
        "pub actor CyclicImported {\n\
         \x20   let peer: LocalPid<CyclicImported>;\n\
         }\n\
         pub actor AcyclicImported {\n\
         \x20   let value: i64;\n\
         }\n",
        "import \"imported_supervisor_child_support/worker.hew\";\n\
         actor RootAcyclic {\n\
         \x20   let value: i64;\n\
         }\n",
        |checked| {
            assert!(checked.cycle_capable_actors.contains(&expected));
            // File imports also expose a bare compatibility alias. Keep only
            // the checker's full nominal key so this oracle cannot pass through
            // the historical leaf/bare fallback in HIR lowering.
            checked
                .cycle_capable_actors
                .retain(|actor| actor == &expected);
        },
    );
    assert!(
        checked.errors.is_empty(),
        "type errors: {:#?}",
        checked.errors
    );
    assert!(
        output.diagnostics.is_empty(),
        "HIR diagnostics: {:#?}",
        output.diagnostics
    );

    assert!(checked.cycle_capable_actors.contains(&expected));
    assert_eq!(checked.cycle_capable_actors.len(), 1);

    let cyclic = actor(&output, "CyclicImported");
    assert_eq!(cyclic.qualified_name(), expected);
    assert!(cyclic.cycle_capable);
    assert!(!actor(&output, "AcyclicImported").cycle_capable);
    assert!(!actor(&output, "RootAcyclic").cycle_capable);
}

#[test]
fn named_and_aliased_supervisor_children_use_the_imported_actor_identity() {
    for (alias, binding) in [(None, "Worker"), (Some("Renamed"), "Renamed")] {
        let source =
            format!("supervisor App {{ child worker: {binding}(id: 17) restart: temporary; }}");
        let (output, checked) = lower_named_import(alias, &source);
        assert!(
            checked.errors.is_empty(),
            "`{binding}` type errors: {:#?}",
            checked.errors
        );
        assert!(
            output.diagnostics.is_empty(),
            "`{binding}` HIR diagnostics: {:#?}",
            output.diagnostics
        );
        assert_eq!(
            checked
                .import_type_name_aliases
                .iter()
                .find(|((_, _, published), _)| published == binding)
                .map(|(_, identity)| identity.as_str()),
            Some("services.workers.Worker")
        );
        assert_eq!(
            supervisor(&output, "App").children[0].ty,
            "services.workers.Worker"
        );
    }
}

#[test]
fn whole_module_supervisor_child_uses_the_imported_actor_identity() {
    let (output, checked) = lower_whole_module_import(
        "supervisor App { child worker: workers.Worker(id: 17) restart: temporary; }",
    );
    assert!(checked.errors.is_empty(), "{:#?}", checked.errors);
    assert!(output.diagnostics.is_empty(), "{:#?}", output.diagnostics);
    assert_eq!(
        supervisor(&output, "App").children[0].ty,
        "services.workers.Worker"
    );
}

#[test]
fn selected_alias_does_not_authorize_a_raw_canonical_actor_path_in_hir() {
    let (output, checked) = lower_named_import(
        Some("Renamed"),
        "supervisor App { child worker: services.workers.Worker restart: temporary; }",
    );
    assert!(checked.errors.iter().any(|error| {
        matches!(
            error.kind,
            hew_types::error::TypeErrorKind::SupervisorError {
                subkind: hew_types::error::SupervisorErrorKind::UnknownChildActor
            }
        )
    }));
    let diagnostic = output
        .diagnostics
        .iter()
        .find(|diagnostic| {
            matches!(
                &diagnostic.kind,
                hew_hir::HirDiagnosticKind::CheckerBoundaryViolation { name, reason }
                    if name == "services.workers.Worker"
                        && reason == "supervisor child has no lexical actor authority"
            )
        })
        .expect("forced HIR lowering must fail closed at the checker boundary");
    assert!(diagnostic.note.contains("exact module binding"));
}

#[test]
fn file_imported_supervisor_resolves_its_same_file_actor_by_exact_owner() {
    let (output, checked) = lower_file_import(
        "pub actor Worker {\n\
         \x20   let id: i64;\n\
         \x20   receive fn identify() -> i64 { id }\n\
         }\n\
         pub supervisor Inner {\n\
         \x20   child worker: Worker(id: 23) restart: temporary;\n\
         }\n",
        "import \"imported_supervisor_child_support/worker.hew\";",
    );
    assert!(checked.errors.is_empty(), "{:#?}", checked.errors);
    assert!(output.diagnostics.is_empty(), "{:#?}", output.diagnostics);
    assert_eq!(
        supervisor(&output, "Inner").children[0].ty,
        format!("{WORKER_MODULE}.Worker")
    );
}

#[test]
fn root_actor_keeps_authority_over_same_leaf_named_import_in_hir() {
    let (output, checked) = lower_named_import(
        None,
        "actor Worker { receive fn identify() -> i64 { 9 } }\n\
         supervisor App { child worker: Worker restart: temporary; }",
    );
    assert!(checked.errors.is_empty(), "{:#?}", checked.errors);
    assert!(output.diagnostics.is_empty(), "{:#?}", output.diagnostics);
    assert_eq!(supervisor(&output, "App").children[0].ty, "Worker");
    assert!(output.module.items.iter().any(|item| matches!(
        item,
        HirItem::Actor(actor) if actor.name == "Worker" && actor.defining_module.is_none()
    )));
}
