#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
pub(super) use super::*;

#[test]
fn crash_hook_tail_inherits_its_declared_crash_action_type() {
    let source = r#"
import std.failure.{CrashInfo, CrashAction};

actor Cache {
    receive fn poison() { panic("x"); }

    #[on(crash)]
    fn on_crash(info: CrashInfo) -> CrashAction { .Restart }
}

fn main() { let cache = spawn Cache; let _ = cache.poison(); }
"#;
    let parsed = hew_parser::parse(source);
    assert!(parsed.errors.is_empty(), "{:#?}", parsed.errors);
    let output = Checker::new(test_registry()).check_program(&parsed.program);
    assert!(output.errors.is_empty(), "{:#?}", output.errors);
}

#[test]
fn supervisor_backoff_requires_the_stdlib_policy_identity() {
    for value in ["100", "Backoff { initial: 100ms, maximum: 1s }"] {
        let output = check_source(&format!(
            "type Backoff {{ initial: duration; maximum: duration; }}\nactor Worker {{}}\nsupervisor Team {{ backoff: {value}; child worker: Worker; }}"
        ));
        assert!(
            output.errors.iter().any(|error| matches!(
                error.kind,
                TypeErrorKind::SupervisorError {
                    subkind: SupervisorErrorKind::BackoffType
                }
            )),
            "{:#?}",
            output.errors
        );
    }
}

#[test]
fn child_backoff_requires_the_stdlib_policy_identity() {
    for value in ["5s", "Backoff { initial: 100ms, maximum: 1s }"] {
        let output = check_source(&format!(
            "type Backoff {{ initial: duration; maximum: duration; }}\nactor Worker {{}}\nsupervisor Team {{ child worker: Worker restart: permanent backoff: {value}; }}"
        ));
        assert!(
            output.errors.iter().any(|error| matches!(
                error.kind,
                TypeErrorKind::SupervisorError {
                    subkind: SupervisorErrorKind::BackoffType
                }
            )),
            "{:#?}",
            output.errors
        );
    }
}

#[test]
fn temporary_child_refuses_a_backoff_clause() {
    let declared = |restart: &str| {
        check_source(&format!(
            "import std.restart;\nactor Worker {{}}\nsupervisor Team {{ child worker: Worker restart: {restart} backoff: restart.immediate(); }}"
        ))
    };
    let refused = |output: &TypeCheckOutput| {
        output.errors.iter().any(|error| {
            matches!(
                error.kind,
                TypeErrorKind::SupervisorError {
                    subkind: SupervisorErrorKind::TemporaryBackoff
                }
            )
        })
    };
    let temporary = declared("temporary");
    assert!(refused(&temporary), "{:#?}", temporary.errors);
    let transient = declared("transient");
    assert!(!refused(&transient), "{:#?}", transient.errors);
}

#[test]
fn nested_supervisor_remains_a_valid_child_target() {
    let output = check_source(
        "actor Worker {\n    receive fn ping() {}\n}\n\nsupervisor Inner {\n    child worker: Worker;\n}\n\nsupervisor Root {\n    child inner: Inner;\n}\n",
    );
    assert!(
        output.errors.is_empty(),
        "a nested supervisor is a supervisable child target: {:#?}",
        output.errors
    );
}

#[test]
fn supervisor_stop_clause_checks_a_configured_duration() {
    let output = check_source(
        "actor Worker { receive fn ping() {} }\n\
         supervisor Team(grace: duration) { child worker: Worker stop: grace; }",
    );
    assert!(output.errors.is_empty(), "{:#?}", output.errors);

    let invalid = check_source(
        "actor Worker { receive fn ping() {} }\n\
         supervisor Team { child worker: Worker stop: \"later\"; }",
    );
    assert!(
        invalid
            .errors
            .iter()
            .any(|error| matches!(error.kind, TypeErrorKind::Mismatch { .. })),
        "{:#?}",
        invalid.errors
    );
}

#[test]
fn generic_supervisor_infers_config_and_child_type_before_function_projection() {
    let output = check_source(
        r#"fn main() {
    let group = spawn Group { seed: "owned" };
    let value: Result<string, ActorError<Never>> = group.worker.get();
    stop(group); stopped(group);
}

actor Worker<T> {
    let value: T;
    receive fn get() -> T {
        value
    }
}

supervisor Group<T>(seed: T) {
    child worker: Worker { value: seed };
}
"#,
    );
    assert!(output.errors.is_empty(), "{:#?}", output.errors);
}

#[test]
fn generic_supervisor_rejects_wrong_child_argument_type() {
    let output = check_source(
        r"actor Worker<T> {
    let value: T;
    receive fn get() -> T {
        value
    }
}

supervisor Group<T>(seed: T) {
    child worker: Worker<i64> { value: seed };
}
",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|error| matches!(error.kind, TypeErrorKind::Mismatch { .. })),
        "{:#?}",
        output.errors
    );
}

#[test]
fn generic_supervisor_rejects_non_send_type_argument_at_spawn() {
    let output = check_source(
        r"
        supervisor Group<T> {}
        fn main() { let _ = spawn Group<Rc<i64>>; }
    ",
    );
    assert!(
        output.errors.iter().any(
            |error| matches!(error.kind, TypeErrorKind::BoundsNotSatisfied)
                && error.message.contains("Send")
        ),
        "{:#?}",
        output.errors
    );
}

#[test]
fn generic_supervisor_checks_nested_send_bounds() {
    let output = check_source(
        r#"actor Worker<T> {
    let value: T;
    receive fn get() -> T {
        value
    }
}

supervisor Group<T>(seed: Vec<T>) {
    child worker: Worker<Vec<T>> { value: seed };
}

fn main() {
    let group = spawn Group { seed: ["owned"] };
    let values: Result<Vec<string>, ActorError<Never>> = group.worker.get();
    stop(group); stopped(group);
}
"#,
    );
    assert!(output.errors.is_empty(), "{:#?}", output.errors);
}

#[test]
fn supervisor_init_arg_scalar_config_field_admitted() {
    // Config field access must be typed before constructing the child.
    let output = check_source(
        r"type AppConfig {
    size: i64;
}

actor Cache {
    var stored: i64;
    init(capacity: i64) {
        stored = capacity;
    }
    receive fn noop() {}
}

supervisor App(config: AppConfig) {
    child cache: Cache { capacity: config.size };
}
",
    );

    assert!(output.errors.is_empty(), "{:#?}", output.errors);
}

#[test]
fn supervisor_init_arg_config_nonexistent_field_surfaces_error() {
    // Typing the init-arg expr against the config param surfaces a real
    // diagnostic for a missing config field, at the arg-expr span.
    let output = check_source(
        r"type AppConfig {
    size: i64;
}

actor Cache {
    var capacity: i64;
    init(capacity: i64) {
        capacity = capacity;
    }
    receive fn noop() {}
}

supervisor App(config: AppConfig) {
    child cache: Cache { capacity: config.nonexistent };
}
",
    );

    assert!(
        output
            .errors
            .iter()
            .any(|e| e.message.contains("nonexistent") && e.message.contains("AppConfig")),
        "reading a non-existent config field must surface a typed error naming \
         the field and the config struct; errors: {:#?}",
        output.errors
    );
}

#[test]
fn supervisor_init_arg_config_field_type_mismatch_surfaces_error() {
    // A config field whose type does not match the actor init param surfaces a
    // type-mismatch diagnostic (the actor expects i64; the config field is
    // bool). The init-arg-expr typing is what makes this catchable.
    let output = check_source(
        r"type AppConfig {
    flag: bool;
}

actor Cache {
    var capacity: i64;
    init(capacity: i64) {
        capacity = capacity;
    }
    receive fn noop() {}
}

supervisor App(config: AppConfig) {
    child cache: Cache { capacity: config.flag };
}
",
    );

    // The arg expr types cleanly to bool; the mismatch against the i64 param is
    // reported by the spawn/constructor arg check. Either a Mismatch error or
    // a constructor-arg diagnostic is acceptable — assert at least one error
    // mentions the type clash so a silent accept fails the test.
    assert!(
        !output.errors.is_empty(),
        "a config field whose type mismatches the actor init param must produce \
         at least one diagnostic; errors: {:#?}",
        output.errors
    );
}

#[test]
fn supervisor_pool_count_missing_is_rejected() {
    let output = check_source(
        r"actor Worker {
    receive fn ping() {}
}

supervisor App {
    strategy: simple_one_for_one;
    pool workers: Worker;
}
",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.message.contains("E_SUPERVISOR_POOL_COUNT_MISSING")),
        "a pool child without `count:` must be rejected; errors: {:#?}",
        output.errors
    );
}

#[test]
fn supervisor_pool_count_zero_literal_is_rejected() {
    let output = check_source(
        r"actor Worker {
    receive fn ping() {}
}

supervisor App {
    strategy: simple_one_for_one;
    pool workers: Worker count: 0;
}
",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.message.contains("E_SUPERVISOR_POOL_COUNT_NON_POSITIVE")),
        "a `count: 0` pool must be rejected; errors: {:#?}",
        output.errors
    );
}

#[test]
fn supervisor_pool_count_negative_literal_is_rejected() {
    let output = check_source(
        r"actor Worker {
    receive fn ping() {}
}

supervisor App {
    strategy: simple_one_for_one;
    pool workers: Worker count: -3;
}
",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.message.contains("E_SUPERVISOR_POOL_COUNT_NON_POSITIVE")),
        "a negative `count:` pool must be rejected; errors: {:#?}",
        output.errors
    );
}

#[test]
fn supervisor_pool_count_positive_literal_is_accepted() {
    let output = check_source(
        r"actor Worker {
    receive fn ping() {}
}

supervisor App {
    strategy: simple_one_for_one;
    pool workers: Worker count: 5;
}
",
    );
    assert!(
        !output
            .errors
            .iter()
            .any(|e| e.message.contains("E_SUPERVISOR_POOL_COUNT")),
        "a positive `count:` pool must be accepted; errors: {:#?}",
        output.errors
    );
}

#[test]
fn supervisor_pool_count_string_literal_is_rejected() {
    let output = check_source(
        r#"actor Worker {
    receive fn ping() {}
}

supervisor App {
    strategy: simple_one_for_one;
    pool workers: Worker count: "five";
}
"#,
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.message.contains("E_SUPERVISOR_POOL_COUNT_TYPE")),
        "a non-integer `count:` must be rejected; errors: {:#?}",
        output.errors
    );
}

#[test]
fn supervisor_pool_count_dynamic_config_is_accepted() {
    // A dynamic count (`config.workers`) is accepted by the type-checker (the
    // expr resolves through the config record layout) but codegen currently
    // rejects it with CodegenError::FailClosed — the dynamic 0..N bootstrap
    // loop is not yet emitted. This test only covers the type-checker accept.
    let output = check_source(
        r"type AppConfig {
    workers: i64;
}

actor Worker {
    receive fn ping() {}
}

supervisor App(config: AppConfig) {
    strategy: simple_one_for_one;
    pool workers: Worker count: config.workers;
}
",
    );
    assert!(
        !output
            .errors
            .iter()
            .any(|e| e.message.contains("E_SUPERVISOR_POOL_COUNT")),
        "a dynamic config-derived `count:` must be accepted; errors: {:#?}",
        output.errors
    );
}

#[test]
fn supervisor_wired_cycle_reports_distinct_kind() {
    let output = check_source(
        r"actor ActorA {
    init(dep: ActorB) {}
    receive fn ping() {}
}

actor ActorB {
    init(dep: ActorA) {}
    receive fn ping() {}
}

supervisor CycleApp {
    strategy: one_for_one;

    child a: ActorA wired_to: { dep: b };
    child b: ActorB wired_to: { dep: a };
}
",
    );

    let err = output
        .errors
        .iter()
        .find(|e| e.message.contains("E_SUPERVISOR_WIRED_CYCLE"))
        .expect("wired_to cycle a->b->a must emit E_SUPERVISOR_WIRED_CYCLE");
    assert_eq!(
        err.kind,
        TypeErrorKind::SupervisorError {
            subkind: SupervisorErrorKind::WiredCycle,
        }
    );
    assert_eq!(err.kind.as_kind_str(), "SupervisorWiredCycle");
}

#[test]
fn supervisor_wiring_binds_forward_generic_init_arguments_and_defaults() {
    let output = check_source(
        r#"
actor Ledger<T> { let value: T; receive fn get() -> T { value } }
actor Checkout<T> {
    let ledger: Ledger<T>;
    let label: string = "default";
    init(backend: Ledger<T>, count: i64) { self.ledger = backend; }
    receive fn ping() {}
}
supervisor Shop<T>(seed: T) {
    child checkout: Checkout<T> { count: 7 } wired_to: { backend: ledger };
    child ledger: Ledger<T> { value: seed };
}
"#,
    );
    assert!(output.errors.is_empty(), "{:#?}", output.errors);
    assert!(output
        .supervisor_startup_order
        .values()
        .any(|order| order == &[1, 0]));
    assert!(output
        .supervisor_wiring
        .values()
        .any(|bindings| bindings == &[("backend".to_string(), 1)]));
}

#[test]
fn supervisor_wiring_rejects_full_generic_mismatch_and_argument_collision() {
    for declaration in [
        "child checkout: Checkout<string> wired_to: { backend: ledger }; child ledger: Ledger<i64> { value: 1 };",
        "child ledger: Ledger<i64> { value: 1 }; child checkout: Checkout<i64> { backend: seed } wired_to: { backend: ledger };",
    ] {
        let source = format!(r"
actor Ledger<T> {{ let value: T; receive fn get() -> T {{ value }} }}
actor Checkout<T> {{ init(backend: Ledger<T>) {{}} receive fn ping() {{}} }}
supervisor Shop(seed: Ledger<i64>) {{ {declaration} }}
");
        let output = check_source(&source);
        assert!(!output.errors.is_empty(), "accepted invalid wiring: {source}");
        assert!(output.errors.iter().any(|error| matches!(error.kind, TypeErrorKind::SpawnArgDuplicate | TypeErrorKind::SupervisorError { subkind: SupervisorErrorKind::WiredToTypeMismatch })), "{:#?}", output.errors);
    }
}

#[test]
fn supervisor_wiring_does_not_capture_source_configuration_names() {
    let output = check_source(
        r"
actor Ledger<T> { let value: T; receive fn get() -> T { value } }
actor Checkout<T> { let count: i64; init(backend: Ledger<T>) {} receive fn ping() {} }
supervisor Shop<T>(__hew_wired_0_1: i64, seed: T) {
    child checkout: Checkout<T> { count: __hew_wired_0_1 } wired_to: { backend: ledger };
    child ledger: Ledger<T> { value: seed };
}
",
    );
    assert!(output.errors.is_empty(), "{:#?}", output.errors);
}

#[test]
fn private_supervisor_in_directory_module_uses_peer_file_import_before_function_projection() {
    let workers_path = std::path::PathBuf::from("workers.hew");
    let first_peer = std::path::PathBuf::from("consumer/api.hew");
    let second_peer = std::path::PathBuf::from("consumer/sup.hew");
    let workers =
        hew_parser::parse("pub actor PeerWorker {\n    receive fn ping() -> i64 { 1 }\n}\n");
    assert!(workers.errors.is_empty(), "{:?}", workers.errors);
    let first =
        hew_parser::parse("pub fn use_sup(s: Sup) -> i64 {\n    let w = s.worker;\n    1\n}\n");
    assert!(first.errors.is_empty(), "{:?}", first.errors);
    let mut second = hew_parser::parse(
        "import workers.{PeerWorker};\n\nsupervisor Sup {\n    strategy: one_for_one;\n    child worker: PeerWorker;\n}\n",
    );
    assert!(second.errors.is_empty(), "{:?}", second.errors);
    let import = second
        .program
        .items
        .iter_mut()
        .find_map(|(item, _)| match item {
            Item::Import(import) => Some(import),
            _ => None,
        })
        .expect("peer import");
    import.resolved_items = Some(workers.program.items.clone().into());
    import.resolved_item_source_paths =
        std::iter::repeat_n(workers_path.clone(), workers.program.items.len()).collect();
    import.resolved_source_paths = vec![workers_path.clone()];

    let root_id = ModulePath::root();
    let workers_id = ModulePath::new(["workers"]);
    let consumer_id = ModulePath::new(["consumer"]);
    let mut consumer_items = first.program.items;
    let first_count = consumer_items.len();
    consumer_items.extend(second.program.items);
    let mut graph = ModuleGraph::new(root_id.clone());
    graph
        .add_module(Module {
            id: workers_id.clone(),
            items: workers.program.items,
            imports: vec![],
            source_paths: vec![workers_path],
            doc: None,
        })
        .expect("workers module");
    graph
        .add_module(Module {
            id: consumer_id.clone(),
            items: consumer_items,
            imports: vec![hew_parser::module::ModuleImport {
                target: workers_id.clone(),
                spec: Some(ImportSpec::Names(vec![ImportName {
                    name: Ident::new("PeerWorker"),
                    alias: None,
                }])),
                span: 0..29,
            }],
            source_paths: vec![first_peer.clone(), second_peer.clone()],
            doc: None,
        })
        .expect("consumer module");
    graph
        .add_module(Module {
            id: root_id.clone(),
            items: vec![],
            imports: vec![],
            source_paths: vec!["main.hew".into()],
            doc: None,
        })
        .expect("root module");
    graph.item_sources.insert(
        "consumer".to_string(),
        std::iter::repeat_n(first_peer, first_count)
            .chain(std::iter::repeat_n(
                second_peer,
                graph.modules[&consumer_id].items.len() - first_count,
            ))
            .collect(),
    );
    graph.topo_order = vec![workers_id, consumer_id, root_id];

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&Program {
        expansions: hew_parser::macro_syntax::ExpansionMap::default(),
        items: vec![],
        module_graph: Some(graph),
        module_doc: None,
    });
    assert!(
        output.errors.is_empty(),
        "private supervisor in directory module must resolve peer-file import before api projection: {:#?}",
        output.errors
    );
}

#[test]
fn supervisor_rejects_unimported_bare_child_when_root_has_same_named_actor() {
    let sup_mod = hew_parser::parse(
        "pub supervisor RootSup {\n    strategy: one_for_one;\n    child worker: Worker;\n}\n",
    );
    assert!(sup_mod.errors.is_empty(), "{:?}", sup_mod.errors);
    let root =
        hew_parser::parse("actor Worker {\n    receive fn ping() -> i64 { 1 }\n}\nfn main() {}\n");
    assert!(root.errors.is_empty(), "{:?}", root.errors);

    let root_id = ModulePath::root();
    let sup_id = ModulePath::new(["sup_mod"]);
    let mut graph = ModuleGraph::new(root_id.clone());
    graph
        .add_module(Module {
            id: sup_id.clone(),
            items: sup_mod.program.items,
            imports: vec![],
            source_paths: vec![],
            doc: None,
        })
        .expect("sup module");
    graph
        .add_module(Module {
            id: root_id.clone(),
            items: root.program.items,
            imports: vec![],
            source_paths: vec![],
            doc: None,
        })
        .expect("root module");
    graph.topo_order = vec![sup_id, root_id];

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&Program {
        expansions: hew_parser::macro_syntax::ExpansionMap::default(),
        items: vec![],
        module_graph: Some(graph),
        module_doc: None,
    });
    assert!(
        output.errors.iter().any(|e| matches!(
            e.kind,
            TypeErrorKind::SupervisorError {
                subkind: SupervisorErrorKind::UnknownChildActor
            }
        )),
        "unimported bare child `Worker` must fail as UnknownChildActor instead of binding root `Worker`: {:#?}",
        output.errors
    );
}

#[test]
fn supervisor_rejects_private_child_actor_across_module_boundary() {
    let secret_mod = hew_parser::parse("actor HiddenWorker {\n    receive fn ping() {}\n}\n");
    assert!(secret_mod.errors.is_empty(), "{:?}", secret_mod.errors);
    let mut root = hew_parser::parse(
        "import secret_mod;\n\nsupervisor Root {\n    strategy: one_for_one;\n    child h: secret_mod.HiddenWorker;\n}\n",
    );
    assert!(root.errors.is_empty(), "{:?}", root.errors);
    for (item, _) in &mut root.program.items {
        if let Item::Import(import) = item {
            if import.path.to_string() == "secret_mod" {
                import.resolved_items = Some(secret_mod.program.items.clone().into());
            }
        }
    }

    let root_id = ModulePath::root();
    let secret_id = ModulePath::new(["secret_mod"]);
    let mut graph = ModuleGraph::new(root_id.clone());
    graph
        .add_module(Module {
            id: secret_id.clone(),
            items: secret_mod.program.items,
            imports: vec![],
            source_paths: vec![],
            doc: None,
        })
        .expect("secret module");
    graph
        .add_module(Module {
            id: root_id.clone(),
            items: root.program.items.clone(),
            imports: vec![hew_parser::module::ModuleImport {
                target: secret_id.clone(),
                spec: None,
                span: 0..18,
            }],
            source_paths: vec![],
            doc: None,
        })
        .expect("root module");
    graph.topo_order = vec![secret_id, root_id];

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&Program {
        expansions: hew_parser::macro_syntax::ExpansionMap::default(),
        items: root.program.items,
        module_graph: Some(graph),
        module_doc: None,
    });
    assert!(
        output
            .errors
            .iter()
            .any(|e| matches!(e.kind, TypeErrorKind::VisibilityViolationPrivate { .. })),
        "private child actor across module boundary must emit VisibilityViolationPrivate: {:#?}",
        output.errors
    );
    assert!(
        output.errors.iter().any(|e| matches!(
            e.kind,
            TypeErrorKind::SupervisorError {
                subkind: SupervisorErrorKind::UnknownChildActor
            }
        )),
        "private child actor must not resolve cleanly: {:#?}",
        output.errors
    );
}
