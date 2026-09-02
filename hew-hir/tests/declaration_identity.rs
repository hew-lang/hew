use hew_hir::{lower_program, HirDiagnosticKind, HirItem, ResolutionCtx, TargetArch};
use hew_parser::{
    ast::Program,
    module::{Module, ModuleGraph, ModuleId},
};
use hew_types::{module_registry::ModuleRegistry, Checker, TypeCheckOutput};

fn check_and_lower(source: &str) -> hew_hir::LowerOutput {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );
    let checked = Checker::new(ModuleRegistry::new(vec![])).check_program(&parsed.program);
    assert!(
        checked.errors.is_empty(),
        "type errors: {:#?}",
        checked.errors
    );
    lower_program(
        &parsed.program,
        &checked,
        &ResolutionCtx,
        TargetArch::host(),
    )
}

#[test]
fn source_and_child_artifacts_carry_checker_declaration_ids() {
    let output = check_and_lower(
        r#"
type Holder<T> { value: T; }

impl Holder<i64> {
    fn get(holder: Holder<i64>) -> i64 { holder.value }
}

extern "C" {
    fn raw_identity(value: i64) -> i64;
}

actor Counter {
    init() {}
    receive fn ping() {}
    fn status() -> i64 { 0 }
}

supervisor App {
    child counter: Counter
}

machine Toggle {
    events { Flip; }
    state Off;
    state On;
    on Flip: Off => .On;
    on Flip: On => .Off;
}

fn ordinary<T>(value: T) -> T { value }
"#,
    );
    assert!(
        output.diagnostics.is_empty(),
        "HIR diagnostics: {:#?}",
        output.diagnostics
    );

    let mut saw_impl = false;
    for item in &output.module.items {
        match item {
            HirItem::Function(function) if function.name == "ordinary" => {
                assert_eq!(function.declaration.full_path(), "ordinary");
            }
            HirItem::ExternFn(function) if function.name == "raw_identity" => {
                assert_eq!(function.declaration.full_path(), "raw_identity");
            }
            HirItem::TypeDecl(declaration) if declaration.name == "Holder" => {
                assert_eq!(declaration.declaration.full_path(), "Holder");
            }
            HirItem::Impl(implementation) if implementation.self_type_name == "Holder" => {
                saw_impl = true;
                let method = implementation.method_ids[0]
                    .as_ref()
                    .expect("language-declared impl method has an exact DefId");
                assert!(method
                    .full_path()
                    .contains("::<impl inherent for Holder<i64>>::get"));
            }
            HirItem::Actor(actor) if actor.name == "Counter" => {
                assert_eq!(actor.declaration.full_path(), "Counter");
                assert_eq!(
                    actor.init.as_ref().unwrap().declaration.full_path(),
                    "Counter::<init>"
                );
                assert_eq!(
                    actor.receive_handlers[0].declaration.full_path(),
                    "Counter::ping"
                );
                assert_eq!(actor.methods[0].declaration.full_path(), "Counter::status");
            }
            HirItem::Supervisor(supervisor) if supervisor.name == "App" => {
                assert_eq!(supervisor.declaration.full_path(), "App");
                assert_eq!(
                    supervisor.bootstrap_declaration.full_path(),
                    "App::<bootstrap>"
                );
            }
            HirItem::Machine(machine) if machine.name == "Toggle" => {
                assert_eq!(machine.declaration.full_path(), "Toggle");
                assert_eq!(
                    machine.states[0].declaration.full_path(),
                    "Toggle::state Off"
                );
                assert_eq!(
                    machine.events[0].declaration.full_path(),
                    "Toggle::event Flip"
                );
                assert_eq!(
                    machine.transitions[0].declaration.full_path(),
                    "Toggle::<transition#0>"
                );
            }
            _ => {}
        }
    }
    assert!(saw_impl, "impl metadata must survive lowering");
}

#[test]
fn missing_or_desynchronised_identity_emits_no_source_artifact() {
    let parsed = hew_parser::parse("fn stable() {}");
    assert!(parsed.errors.is_empty());
    let checked = Checker::new(ModuleRegistry::new(vec![])).check_program(&parsed.program);

    let missing = TypeCheckOutput {
        identity: hew_types::IdentityView::default(),
        ..checked.clone()
    };
    let missing_output = lower_program(
        &parsed.program,
        &missing,
        &ResolutionCtx,
        TargetArch::host(),
    );
    assert!(!missing_output
        .module
        .items
        .iter()
        .any(|item| matches!(item, HirItem::Function(function) if function.name == "stable")));
    assert!(missing_output.diagnostics.iter().any(|diagnostic| matches!(
        diagnostic.kind,
        HirDiagnosticKind::CheckerBoundaryViolation { .. }
    )));

    let mut desynchronised = parsed.program.clone();
    desynchronised.items[0].1 = 1000..1010;
    let desynchronised_output = lower_program(
        &desynchronised,
        &checked,
        &ResolutionCtx,
        TargetArch::host(),
    );
    assert!(!desynchronised_output
        .module
        .items
        .iter()
        .any(|item| matches!(item, HirItem::Function(function) if function.name == "stable")));
    assert!(desynchronised_output
        .diagnostics
        .iter()
        .any(|diagnostic| matches!(
            diagnostic.kind,
            HirDiagnosticKind::CheckerBoundaryViolation { .. }
        )));
}

#[test]
fn directory_peer_file_projects_its_exact_checker_module_id() {
    let parsed = hew_parser::parse("pub fn peer_value() -> i64 { 7 }");
    assert!(parsed.errors.is_empty());
    let root = ModuleId::root();
    let package = ModuleId::new(vec!["pkg".to_string()]);
    let primary = std::path::PathBuf::from("/nonexistent/pkg/pkg.hew");
    let peer = std::path::PathBuf::from("/nonexistent/pkg/peer.hew");
    let mut graph = ModuleGraph::new(root.clone());
    graph
        .add_module(Module {
            id: package.clone(),
            items: parsed.program.items,
            imports: Vec::new(),
            source_paths: vec![primary, peer.clone()],
            doc: None,
        })
        .unwrap();
    graph
        .add_module(Module {
            id: root.clone(),
            items: Vec::new(),
            imports: Vec::new(),
            source_paths: Vec::new(),
            doc: None,
        })
        .unwrap();
    graph.topo_order = vec![package, root];
    graph.item_sources.insert("pkg".to_string(), vec![peer]);
    let program = Program {
        module_graph: Some(graph),
        items: Vec::new(),
        module_doc: None,
    };

    let checked = Checker::new(ModuleRegistry::new(vec![])).check_program(&program);
    assert!(
        checked.errors.is_empty(),
        "type errors: {:#?}",
        checked.errors
    );
    let output = lower_program(&program, &checked, &ResolutionCtx, TargetArch::host());
    assert!(
        output.diagnostics.is_empty(),
        "HIR diagnostics: {:#?}",
        output.diagnostics
    );
    let function = output
        .module
        .items
        .iter()
        .find_map(|item| match item {
            HirItem::Function(function) if function.name.ends_with("peer_value") => Some(function),
            _ => None,
        })
        .expect("peer function lowers");
    assert_eq!(function.declaration.full_path(), "pkg.peer.peer_value");
}
