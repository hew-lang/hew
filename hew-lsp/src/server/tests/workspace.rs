use super::*;

// ── Code lens tests ─────────────────────────────────────────────

#[test]
fn code_lenses_for_functions() {
    let source = "fn foo() -> i32 { 0 }\nfn bar() -> i32 { foo() }";
    let parse_result = hew_parser::parse(source);
    let lo = compute_line_offsets(source);
    let lenses = build_code_lenses(source, &lo, &parse_result, "sample.hew");
    assert!(
        lenses.len() >= 2,
        "expected at least 2 code lenses (one per function), got {}",
        lenses.len()
    );
    // All lenses should have a command
    for lens in &lenses {
        assert!(lens.command.is_some());
    }
}

#[test]
fn code_lens_reference_count_reflects_usage() {
    let source = "fn helper() -> i32 { 42 }\nfn main() { helper() }";
    let parse_result = hew_parser::parse(source);
    let lo = compute_line_offsets(source);
    let lenses = build_code_lenses(source, &lo, &parse_result, "sample.hew");
    let helper_lens = lenses
        .iter()
        .find(|l| {
            l.command
                .as_ref()
                .is_some_and(|c| c.title.contains("reference"))
                && l.range.start.line == 0
        })
        .expect("should find a reference lens for helper");
    assert!(
        helper_lens
            .command
            .as_ref()
            .unwrap()
            .title
            .contains("1 reference"),
        "helper should have 1 reference, got: {}",
        helper_lens.command.as_ref().unwrap().title
    );
}

#[test]
fn code_lens_test_attribute() {
    let source = "#[test]\nfn test_add() { 0 }";
    let parse_result = hew_parser::parse(source);
    let lo = compute_line_offsets(source);
    let lenses = build_code_lenses(source, &lo, &parse_result, "sample.hew");
    let run_test_lens = lenses.iter().find(|l| {
        l.command
            .as_ref()
            .is_some_and(|c| c.title.contains("Run test"))
    });
    assert!(
        run_test_lens.is_some(),
        "expected a 'Run test' code lens for #[test] function"
    );
    assert!(lenses
        .iter()
        .any(|lens| lens.command.as_ref().is_some_and(|command| {
            command.title.contains("Run file")
                && command
                    .arguments
                    .as_ref()
                    .is_some_and(|args| args[0] == "sample.hew")
        })));
    assert_eq!(
        run_test_lens
            .and_then(|lens| lens.command.as_ref())
            .and_then(|command| command.arguments.as_ref())
            .and_then(|args| args.first()),
        Some(&json!("sample.hew::test_add"))
    );
}

#[test]
fn failed_test_event_uses_fault_site_operands_and_seed_lens() {
    let source = "#[test]\nfn fails() { assert(1 == 2); }\n";
    let file = std::env::temp_dir().join("hew-lsp-fault-site.hew");
    let uri = Url::from_checked_file_path(&file).unwrap();
    let documents = DashMap::new();
    documents.insert(uri.clone(), make_doc(source));
    let event = json!({
        "event": "test_finished",
        "selector": format!("{}::fails", file.display()),
        "outcome": "failed",
        "kind": "assertion",
        "message": "assertion failed",
        "report": {
            "site": { "file": file.file_name().unwrap().to_str().unwrap(), "line": 2, "col": 14 },
            "seed": "18446744073709551615",
            "assertion": { "operator": "==", "left": "1", "right": "2" }
        }
    });
    let failure = TestFailureBatch::default()
        .failure_from_event(&event, &documents)
        .expect("test failure diagnostic");
    assert_eq!(failure.uri, uri);
    assert_eq!(failure.diagnostic.range.start.line, 1);
    assert_eq!(failure.diagnostic.range.start.character, 13);
    assert_eq!(failure.diagnostic.source.as_deref(), Some("hew test"));
    assert!(failure.diagnostic.message.contains("left: 1\nright: 2"));
    assert_eq!(failure.seed.as_deref(), Some("18446744073709551615"));

    let seeds = DashMap::new();
    seeds.insert(failure.selector.clone(), failure.seed.unwrap());
    let doc = documents.get(&uri).unwrap();
    let lenses = super::super::workspace::build_code_lenses_with_seeds(
        source,
        &doc.line_offsets,
        &doc.parse_result,
        doc.expanded_program.as_deref(),
        &file.display().to_string(),
        Some(&seeds),
    );
    let rerun = lenses
        .iter()
        .filter_map(|lens| lens.command.as_ref())
        .find(|command| command.title.contains("Rerun with seed"))
        .expect("seeded rerun lens");
    assert_eq!(
        rerun.arguments.as_ref().unwrap()[0]["seed"],
        "18446744073709551615"
    );
}

#[test]
fn test_inventory_uses_open_document_and_exact_identity() {
    let root = std::env::temp_dir().join("hew-lsp-inventory-project");
    let file = root.join("cart_test.hew");
    let uri = Url::from_checked_file_path(&file).unwrap();
    let documents = DashMap::new();
    documents.insert(uri.clone(), make_doc("#[test]\nfn totals() {}\n"));
    let entries = test_inventory(&documents, &[root], Some(&uri));
    assert_eq!(entries.len(), 1);
    assert_eq!(entries[0]["identity"], "cart_test.hew::totals");
    assert_eq!(
        entries[0]["selector"],
        format!("{}::totals", file.display())
    );
    assert_eq!(entries[0]["uri"], uri.as_str());
    assert_eq!(entries[0]["ignored"], false);
}

#[test]
fn test_inventory_keeps_same_named_tests_in_two_roots_distinct() {
    let first_root = std::env::temp_dir().join("hew-lsp-inventory-first");
    let second_root = std::env::temp_dir().join("hew-lsp-inventory-second");
    let first_file = first_root.join("cart_test.hew");
    let second_file = second_root.join("cart_test.hew");
    let first = Url::from_checked_file_path(&first_file).unwrap();
    let second = Url::from_checked_file_path(&second_file).unwrap();
    let documents = DashMap::new();
    documents.insert(first.clone(), make_doc("#[test]\nfn totals() {}\n"));
    documents.insert(second.clone(), make_doc("#[test]\nfn totals() {}\n"));
    let roots = [first_root, second_root];
    let left = test_inventory(&documents, &roots, Some(&first));
    let right = test_inventory(&documents, &roots, Some(&second));
    assert_eq!(
        left[0]["selector"],
        format!("{}::totals", first_file.display())
    );
    assert_eq!(
        right[0]["selector"],
        format!("{}::totals", second_file.display())
    );
    assert_ne!(left[0]["selector"], right[0]["selector"]);
    assert_eq!(left[0]["identity"], "cart_test.hew::totals");
    assert_eq!(
        right[0]["identity"],
        format!("{}::totals", second_file.display()).replace('\\', "/")
    );
}

#[test]
fn initialize_advertises_run_test_execute_command() {
    let capabilities = build_server_capabilities();
    let commands = capabilities
        .execute_command_provider
        .expect("execute command support should be advertised")
        .commands;
    assert_eq!(
        commands,
        vec![RUN_TEST_COMMAND.to_string(), GENERATE_COMMAND.to_string()]
    );
}

#[test]
fn initialize_surfaces_capability_decode_errors() {
    let err = build_initialize_result_from_caps_json(serde_json::json!(42))
        .expect_err("invalid capability payload should surface an initialize error");
    assert!(
        err.message
            .contains("failed to decode LSP server capabilities"),
        "expected decode error, got: {err:?}"
    );
}

#[test]
fn extract_run_test_name_accepts_legacy_and_object_args() {
    assert_eq!(
        extract_run_test_name(&[Value::String("test_add".to_string())]),
        Some("test_add".to_string())
    );
    assert_eq!(
        extract_run_test_name(&[json!({ "name": "test_subtract" })]),
        Some("test_subtract".to_string())
    );
}

#[test]
fn build_run_test_invocation_uses_cli_test_runner() {
    let (program, args) = build_run_test_invocation("/workspace-root/cart_test.hew::test_add");
    assert!(program.ends_with(Path::new(&format!("hew{}", std::env::consts::EXE_SUFFIX))));
    assert_eq!(
        args,
        vec![
            "test".to_string(),
            "/workspace-root/cart_test.hew::test_add".to_string(),
            "--format".to_string(),
            "json".to_string(),
        ]
    );
}

#[test]
fn code_action_capabilities_advertise_remove_unused_imports_kind() {
    let capabilities = build_server_capabilities();
    let kinds = match capabilities.code_action_provider {
        Some(tower_lsp_server::ls_types::CodeActionProviderCapability::Options(options)) => options
            .code_action_kinds
            .expect("code action kinds should be advertised"),
        _ => panic!("expected code action options"),
    };
    assert!(kinds.contains(&CodeActionKind::QUICKFIX));
    assert!(kinds.contains(&CodeActionKind::SOURCE));
    assert!(kinds.contains(&remove_unused_imports_kind()));
}

// ── Workspace symbol tests ──────────────────────────────────────

#[test]
fn workspace_symbols_finds_functions_and_types() {
    let source = "fn compute() -> i32 {\n    0\n}\n\ntype Widget {\n    w: i32;\n}\n\nconst MAX: i32 = 100;\n";
    let parse_result = hew_parser::parse(source);
    assert!(
        parse_result.errors.is_empty(),
        "parse errors: {:?}",
        parse_result.errors
    );
    let lo = compute_line_offsets(source);
    let uri = Url::parse("file:///test.hew").unwrap();
    let analysis_symbols = hew_analysis::symbols::build_document_symbols(source, &parse_result);
    let symbols = collect_workspace_symbols(&uri, source, &lo, &analysis_symbols, "");
    let names: Vec<&str> = symbols.iter().map(|s| s.name.as_str()).collect();
    assert!(
        names.contains(&"compute"),
        "should find function, got: {names:?}"
    );
    assert!(
        names.contains(&"Widget"),
        "should find type, got: {names:?}"
    );
    assert!(
        names.contains(&"MAX"),
        "should find constant, got: {names:?}"
    );
}

#[test]
fn workspace_symbols_filters_by_query() {
    let source = "fn compute() -> i32 {\n    0\n}\n\nfn render() -> i32 {\n    0\n}\n\ntype Widget {\n    w: i32;\n}\n";
    let parse_result = hew_parser::parse(source);
    let lo = compute_line_offsets(source);
    let uri = Url::parse("file:///test.hew").unwrap();
    let analysis_symbols = hew_analysis::symbols::build_document_symbols(source, &parse_result);
    let symbols = collect_workspace_symbols(&uri, source, &lo, &analysis_symbols, "comp");
    assert_eq!(symbols.len(), 1, "query 'comp' should match only compute");
    assert_eq!(symbols[0].name, "compute");
}

#[test]
fn workspace_symbols_case_insensitive() {
    let source = "fn ComputeValue() -> i32 { 0 }";
    let parse_result = hew_parser::parse(source);
    let lo = compute_line_offsets(source);
    let uri = Url::parse("file:///test.hew").unwrap();
    let analysis_symbols = hew_analysis::symbols::build_document_symbols(source, &parse_result);
    let symbols = collect_workspace_symbols(&uri, source, &lo, &analysis_symbols, "compute");
    assert_eq!(symbols.len(), 1, "case-insensitive match should work");
}

#[test]
fn workspace_symbols_includes_actor_and_receive_methods() {
    let source = "actor Counter { receive fn increment(n: i32) { n } }";
    let parse_result = hew_parser::parse(source);
    let lo = compute_line_offsets(source);
    let uri = Url::parse("file:///test.hew").unwrap();
    let analysis_symbols = hew_analysis::symbols::build_document_symbols(source, &parse_result);
    let symbols = collect_workspace_symbols(&uri, source, &lo, &analysis_symbols, "");
    let names: Vec<&str> = symbols.iter().map(|s| s.name.as_str()).collect();
    assert!(
        names.contains(&"Counter"),
        "should find actor, got: {names:?}"
    );
    assert!(
        names.contains(&"increment"),
        "should find receive method, got: {names:?}"
    );
    // The receive method should have the actor as container
    let recv_sym = symbols.iter().find(|s| s.name == "increment").unwrap();
    assert_eq!(recv_sym.container_name.as_deref(), Some("Counter"));
}

#[test]
fn workspace_symbols_includes_trait() {
    let source = "trait Drawable { fn draw() -> i32; }";
    let parse_result = hew_parser::parse(source);
    let lo = compute_line_offsets(source);
    let uri = Url::parse("file:///test.hew").unwrap();
    let analysis_symbols = hew_analysis::symbols::build_document_symbols(source, &parse_result);
    let symbols = collect_workspace_symbols(&uri, source, &lo, &analysis_symbols, "");
    let names: Vec<&str> = symbols.iter().map(|s| s.name.as_str()).collect();
    assert!(
        names.contains(&"Drawable"),
        "should find trait, got: {names:?}"
    );
    let trait_sym = symbols.iter().find(|s| s.name == "Drawable").unwrap();
    assert_eq!(trait_sym.kind, SymbolKind::INTERFACE);
}

#[test]
fn workspace_symbols_include_fields_states_and_events() {
    let source = r"type Point {
    x: i32;
}

machine Traffic {
    events {
        Start;
    }

    state Idle;
    on Start: Idle => .Idle;
}
";
    let parse_result = hew_parser::parse(source);
    let lo = compute_line_offsets(source);
    let uri = Url::parse("file:///test.hew").unwrap();
    let analysis_symbols = hew_analysis::symbols::build_document_symbols(source, &parse_result);
    let symbols = collect_workspace_symbols(&uri, source, &lo, &analysis_symbols, "");

    let field = symbols.iter().find(|symbol| symbol.name == "x").unwrap();
    assert_eq!(field.kind, SymbolKind::FIELD);
    assert_eq!(field.container_name.as_deref(), Some("Point"));

    let event = symbols
        .iter()
        .find(|symbol| symbol.name == "Start")
        .unwrap();
    assert_eq!(event.kind, SymbolKind::EVENT);
    assert_eq!(event.container_name.as_deref(), Some("Traffic"));

    let state = symbols.iter().find(|symbol| symbol.name == "Idle").unwrap();
    assert_eq!(state.kind, SymbolKind::ENUM_MEMBER);
    assert_eq!(state.container_name.as_deref(), Some("Traffic"));
}

#[test]
fn workspace_symbols_scan_workspace_roots() {
    let test_dir = TestDir::new("workspace-symbols");
    let root = test_dir.path();
    let nested = root.join("pkg");
    std::fs::create_dir_all(&nested).unwrap();
    std::fs::write(
        root.join("main.hew"),
        "fn open_buffer() -> i32 { 0 }\nconst ROOT: i32 = 1;\n",
    )
    .unwrap();
    std::fs::write(
        nested.join("worker.hew"),
        "fn hidden_worker() -> i32 { 0 }\n",
    )
    .unwrap();

    let documents = DashMap::new();
    let symbols = collect_project_workspace_symbols(&documents, &[root.to_path_buf()], "hidden");

    assert_eq!(symbols.len(), 1, "should find unopened workspace file");
    assert_eq!(symbols[0].name, "hidden_worker");
}

#[test]
fn workspace_symbols_scan_repository_style_std_and_examples() {
    let test_dir = TestDir::new("workspace-symbols-repo-layout");
    let root = test_dir.path();
    let std_dir = root.join("std").join("demo");
    let examples_dir = root.join("examples");
    std::fs::create_dir_all(&std_dir).unwrap();
    std::fs::create_dir_all(&examples_dir).unwrap();
    std::fs::write(
        std_dir.join("stdlib_probe.hew"),
        "fn stdlib_probe() -> i32 { 0 }\n",
    )
    .unwrap();
    std::fs::write(
        examples_dir.join("example_probe.hew"),
        "fn example_probe() -> i32 { 0 }\n",
    )
    .unwrap();

    let documents = DashMap::new();
    let symbols = collect_project_workspace_symbols(&documents, &[root.to_path_buf()], "probe");
    let names: Vec<&str> = symbols.iter().map(|symbol| symbol.name.as_str()).collect();

    assert!(names.contains(&"stdlib_probe"), "symbols: {names:?}");
    assert!(names.contains(&"example_probe"), "symbols: {names:?}");
}
