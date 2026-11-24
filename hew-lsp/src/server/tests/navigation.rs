use super::*;

#[test]
fn goto_def_receive_method() {
    let source = "actor Counter {\n    let count: i32;\n    receive fn increment(n: i32) {\n        count = count + n;\n    }\n}\n\nfn main() {\n    let c = spawn Counter { count: 0 };\n    c.increment(1);\n}\n";
    let parse_result = hew_parser::parse(source);
    let lo = compute_line_offsets(source);
    let call_offset = source.rfind("increment").unwrap();
    let word = word_at_offset(source, call_offset).unwrap();
    // word_at_offset returns the dot-qualified form "c.increment"
    assert_eq!(word, "c.increment");
    // find_definition_in_ast doesn't find the full dot-qualified name...
    assert!(find_definition_in_ast(source, &lo, &parse_result, &word).is_none());
    // ...but the method part alone finds the receive method definition.
    let method = hew_types::short_name(&word);
    let found = find_definition_in_ast(source, &lo, &parse_result, method);
    assert!(found.is_some(), "should find receive method definition");
}

#[test]
fn goto_def_resolves_checked_local_binding() {
    let source = "fn main() {\n    let result = 41;\n    result + 1\n}";
    let doc = make_typed_doc(source);
    let offset = source.rfind("result + 1").unwrap();
    let word = word_at_offset(source, offset).unwrap();

    assert!(find_definition_in_ast(source, &doc.line_offsets, &doc.parse_result, &word).is_none());

    let uri = make_test_uri("/identity-local.hew");
    let range =
        super::super::navigation::identity_definition_location(&uri, &doc, offset, &DashMap::new())
            .expect("checker should resolve local binding")
            .range;
    let expected_start = source.find("let result").unwrap() + 4;
    let expected = offset_range_to_lsp(
        source,
        &doc.line_offsets,
        expected_start,
        expected_start + "result".len(),
    );
    assert_eq!(range, expected);
}

#[test]
fn goto_def_resolves_checked_struct_field_access() {
    let source =
        "type Point {\n    x: i32;\n    y: i32;\n}\n\nfn main() {\n    let p = Point { x: 1, y: 2 };\n    p.x\n}\n";
    let doc = make_typed_doc(source);
    let offset = source.rfind("p.x").unwrap() + 2;
    let word = word_at_offset(source, offset).unwrap();

    assert_eq!(word, "p.x");
    assert!(find_definition_in_ast(source, &doc.line_offsets, &doc.parse_result, &word).is_none());

    let uri = make_test_uri("/identity-field.hew");
    let range =
        super::super::navigation::identity_definition_location(&uri, &doc, offset, &DashMap::new())
            .expect("checker should resolve field access")
            .range;
    let expected_start = source.find("x: i32").unwrap();
    let expected = offset_range_to_lsp(
        source,
        &doc.line_offsets,
        expected_start,
        expected_start + "x".len(),
    );
    assert_eq!(range, expected);
}

#[test]
fn checked_field_navigation_and_references_keep_nominal_owners_distinct() {
    let source = "type A {\n    x: i64;\n}\n\ntype B {\n    x: i64;\n}\n\nfn main() {\n    let a = A { x: 1 };\n    let b = B { x: 2 };\n    println(a.x);\n    println(b.x);\n}\n";
    let doc = make_typed_doc(source);
    let uri = make_test_uri("/identity-fields.hew");
    let a_use = source.find("a.x").unwrap() + 2;
    let b_use = source.find("b.x").unwrap() + 2;
    let expected_b = source.find("type B {\n    x:").unwrap() + "type B {\n    ".len();
    let location =
        super::super::navigation::identity_definition_location(&uri, &doc, b_use, &DashMap::new())
            .expect("B.x must resolve through checker identity");
    assert_eq!(location.uri, uri);
    assert_eq!(
        location.range,
        offset_range_to_lsp(source, &doc.line_offsets, expected_b, expected_b + 1)
    );
    let at_declaration = super::super::navigation::identity_definition_location(
        &uri,
        &doc,
        expected_b,
        &DashMap::new(),
    )
    .expect("B.x declaration keeps its checker owner");
    assert_eq!(at_declaration, location);
    let refs = super::super::navigation::identity_reference_locations(
        &uri,
        &doc,
        b_use,
        true,
        &DashMap::new(),
    )
    .expect("checked B.x references");
    assert!(refs.iter().any(|site| site.range == location.range));
    assert!(
        refs.iter()
            .any(|site| site.range
                == offset_range_to_lsp(source, &doc.line_offsets, b_use, b_use + 1))
    );
    assert!(
        !refs
            .iter()
            .any(|site| site.range
                == offset_range_to_lsp(source, &doc.line_offsets, a_use, a_use + 1))
    );
}

#[test]
fn checked_var_navigation_uses_only_the_name_token() {
    let source = "fn main() { var value = 1; println(value); }";
    let doc = make_typed_doc(source);
    let uri = make_test_uri("/identity-var.hew");
    let declared = source.find("var value").unwrap() + 4;
    let used = source.rfind("value").unwrap();
    let expected = offset_range_to_lsp(source, &doc.line_offsets, declared, declared + 5);
    for offset in [declared, used] {
        let location = super::super::navigation::identity_definition_location(
            &uri,
            &doc,
            offset,
            &DashMap::new(),
        )
        .expect("var binding should resolve to its name token");
        assert_eq!(location.range, expected);
        let refs = super::super::navigation::identity_reference_locations(
            &uri,
            &doc,
            offset,
            true,
            &DashMap::new(),
        )
        .expect("var references should use token spans");
        assert_eq!(refs.len(), 2);
        assert!(refs
            .iter()
            .all(|reference| reference.range.end.character - reference.range.start.character == 5));
    }
    let initializer = source.find("= 1").unwrap() + 2;
    assert!(
        super::super::navigation::identity_definition_location(
            &uri,
            &doc,
            initializer,
            &DashMap::new(),
        )
        .is_none(),
        "initializer is not the var binder"
    );
}

#[test]
fn checked_method_navigation_selects_second_impl() {
    let source = "type A {\n    x: i64;\n}\n\nimpl A {\n    fn get(self) -> i64 {\n        self.x\n    }\n}\n\ntype B {\n    x: i64;\n}\n\nimpl B {\n    fn get(self) -> i64 {\n        self.x\n    }\n}\n\nfn main() {\n    let b = B { x: 2 };\n    println(b.get());\n}\n";
    let doc = make_typed_doc(source);
    let uri = make_test_uri("/identity-methods.hew");
    let call = source.rfind("b.get").unwrap() + 2;
    let expected = source.rfind("fn get").unwrap() + 3;
    let location =
        super::super::navigation::identity_definition_location(&uri, &doc, call, &DashMap::new())
            .expect("B.get must resolve through checker identity");
    assert_eq!(
        location.range,
        offset_range_to_lsp(source, &doc.line_offsets, expected, expected + 3)
    );
}

#[test]
fn checked_trait_bound_call_navigates_to_trait_method() {
    let source = include_str!("../../../tests/fixtures/v05_trait_bounds.hew");
    let doc = make_typed_doc(source);
    let uri = make_test_uri("/identity-trait-bound.hew");
    let call = source.find("item.describe()").unwrap() + "item.".len();
    let declared = source.find("fn describe(self) -> string;").unwrap() + "fn ".len();
    let location =
        super::super::navigation::identity_definition_location(&uri, &doc, call, &DashMap::new())
            .expect("bounded call should resolve to its trait method");
    assert_eq!(
        location.range,
        offset_range_to_lsp(
            source,
            &doc.line_offsets,
            declared,
            declared + "describe".len()
        )
    );
}

#[test]
fn checked_imported_type_navigation_uses_its_source_module() {
    let main_source =
        "import ma;\nimport mb;\nfn main() { let shape = ma.Shape { x: 1 }; println(shape.x); }";
    let imported_source = "pub type Shape {\n    x: i64;\n}\n";
    let other_source = "pub type Shape {\n    x: bool;\n}\n";
    let main_uri = make_test_uri("/fake/identity/main.hew");
    let imported_uri = make_test_uri("/fake/identity/ma.hew");
    let other_uri = make_test_uri("/fake/identity/mb.hew");
    let documents = DashMap::new();
    documents.insert(imported_uri.clone(), make_doc(imported_source));
    documents.insert(other_uri.clone(), make_doc(other_source));
    let doc = analyze_document(&main_uri, main_source, &documents, &[]);
    let errors = published_errors(&doc, &main_uri);
    assert!(errors.is_empty(), "imported type should check: {errors:?}");
    let offset = main_source.find("ma.Shape").unwrap() + 3;
    let location =
        super::super::navigation::identity_definition_location(&main_uri, &doc, offset, &documents)
            .expect("ma.Shape must resolve through checker identity");
    let expected = imported_source.find("Shape").unwrap();
    assert_eq!(location.uri, imported_uri);
    assert_eq!(
        location.range,
        offset_range_to_lsp(
            imported_source,
            &compute_line_offsets(imported_source),
            expected,
            expected + 5
        )
    );
    let field_use = main_source.find("shape.x").unwrap() + "shape.".len();
    let refs = super::super::navigation::identity_reference_locations(
        &main_uri, &doc, field_use, true, &documents,
    )
    .expect("imported field references should be checker-owned");
    let field_decl = imported_source.find("x:").unwrap();
    assert!(refs.iter().any(|reference| reference.uri == imported_uri
        && reference.range
            == offset_range_to_lsp(
                imported_source,
                &compute_line_offsets(imported_source),
                field_decl,
                field_decl + 1
            )));
    assert!(refs.iter().any(|reference| reference.uri == main_uri
        && reference.range
            == offset_range_to_lsp(main_source, &doc.line_offsets, field_use, field_use + 1)));
    assert!(!refs.iter().any(|reference| reference.uri == other_uri));
}

#[test]
fn goto_def_resolves_nominal_type_name_to_declaration_span() {
    let source =
        "type Point {\n    x: i64;\n    y: i64;\n}\n\nfn origin() -> Point {\n    Point { x: 0, y: 0 }\n}\n";
    let doc = make_typed_doc(source);
    let offset = source.find("-> Point").unwrap() + "-> ".len();

    let uri = make_test_uri("/identity-nominal.hew");
    let range =
        super::super::navigation::identity_definition_location(&uri, &doc, offset, &DashMap::new())
            .expect("checker should resolve nominal type use")
            .range;
    let start = source.find("Point").unwrap();
    let expected = offset_range_to_lsp(source, &doc.line_offsets, start, start + "Point".len());
    assert_eq!(range, expected);
}

#[test]
fn goto_def_resolves_checked_param() {
    let source = "fn add(value: i32) -> i32 {\n    value + 1\n}";
    let doc = make_typed_doc(source);
    let offset = source.rfind("value + 1").unwrap();
    let word = word_at_offset(source, offset).unwrap();

    assert!(find_definition_in_ast(source, &doc.line_offsets, &doc.parse_result, &word).is_none());

    let uri = make_test_uri("/identity-param.hew");
    let range =
        super::super::navigation::identity_definition_location(&uri, &doc, offset, &DashMap::new())
            .expect("checker should resolve parameter")
            .range;
    let expected_start = source.find("value: i32").unwrap();
    let expected = offset_range_to_lsp(
        source,
        &doc.line_offsets,
        expected_start,
        expected_start + "value".len(),
    );
    assert_eq!(range, expected);
}

#[test]
fn find_definition_includes_extern_functions_and_associated_types() {
    let source =
        "extern \"C\" { fn c_abs(x: i32) -> i32; }\ntrait Iter { type Item; fn next() -> i32; }";
    let parse_result = hew_parser::parse(source);
    let lo = compute_line_offsets(source);
    assert!(
        parse_result.errors.is_empty(),
        "parse errors: {:?}",
        parse_result.errors
    );
    assert!(find_definition_in_ast(source, &lo, &parse_result, "c_abs").is_some());
    assert!(find_definition_in_ast(source, &lo, &parse_result, "Item").is_some());
}

#[test]
fn find_refs_scope_aware_separates_same_name_locals() {
    let source = "fn foo(x: i64) { println(x); }\nfn bar(x: i64) { println(x); }";
    let parse_result = hew_parser::parse(source);
    // Cursor on `x` in foo's println(x) — should only find refs in foo scope
    let foo_x_offset = source.find("println(x").unwrap() + 9;
    let result = hew_analysis::references::find_all_references(source, &parse_result, foo_x_offset);
    assert!(result.is_some());
    let (name, spans) = result.unwrap();
    assert_eq!(name, "x");
    // Should only find x usages within foo, not bar
    // foo has: `println(x)` — 1 usage reference (no param decl span in AST)
    assert_eq!(spans.len(), 1, "expected 1 ref in foo, got {}", spans.len());
}

#[test]
fn find_refs_top_level_finds_all() {
    let source = "fn helper() -> i64 { 42 }\nfn main() { helper(); }";
    let parse_result = hew_parser::parse(source);
    // Cursor on `helper` in main's call
    let call_offset = source.rfind("helper").unwrap();
    let result = hew_analysis::references::find_all_references(source, &parse_result, call_offset);
    assert!(result.is_some());
    let (name, spans) = result.unwrap();
    assert_eq!(name, "helper");
    // Should find the call in main (top-level name = global search)
    assert!(!spans.is_empty(), "expected refs for helper");
}

// ── count_all_references tests ──────────────────────────────────

#[test]
fn count_all_references_basic() {
    let source = "fn helper() -> i32 { 42 }\nfn main() { helper() }";
    let parse_result = hew_parser::parse(source);
    let counts = count_all_references(&parse_result);
    let helper_count = counts.get("helper").copied().unwrap_or(0);
    assert!(
        helper_count >= 1,
        "expected at least 1 reference to 'helper', got {helper_count}"
    );
}

#[test]
fn cross_file_references_include_named_importer_and_imported_open_document() {
    let main_source =
        "import util.{ greet };\nfn first() -> i32 { greet() }\nfn second() -> i32 { greet() }";
    let util_source = "pub fn greet() -> i32 { 1 }\nfn wrapper() -> i32 { greet() }";

    let main_uri = make_test_uri("/project/main.hew");
    let util_uri = make_test_uri("/project/util.hew");

    let documents: DashMap<Url, DocumentState> = DashMap::new();
    documents.insert(main_uri.clone(), make_doc(main_source));
    documents.insert(util_uri.clone(), make_doc(util_source));

    let util_doc = documents.get(&util_uri).unwrap();
    let offset = util_source.find("greet").unwrap();
    let locations = build_reference_locations(&util_uri, &util_doc, offset, true, &documents);

    assert_eq!(
        locations.len(),
        5,
        "expected importer + imported references"
    );
    assert_eq!(
        locations
            .iter()
            .filter(|location| location.uri == main_uri)
            .count(),
        3,
        "expected import site plus two main.hew call sites"
    );
    assert_eq!(
        locations
            .iter()
            .filter(|location| location.uri == util_uri)
            .count(),
        2,
        "expected greet definition plus wrapper() call in util.hew"
    );
}

#[test]
fn cross_file_references_from_named_import_usage_include_imported_open_document() {
    let main_source =
        "import util.{ greet };\nfn first() -> i32 { greet() }\nfn second() -> i32 { greet() }";
    let util_source = "pub fn greet() -> i32 { 1 }\nfn wrapper() -> i32 { greet() }";

    let main_uri = make_test_uri("/project/main.hew");
    let util_uri = make_test_uri("/project/util.hew");

    let documents: DashMap<Url, DocumentState> = DashMap::new();
    documents.insert(main_uri.clone(), make_doc(main_source));
    documents.insert(util_uri.clone(), make_doc(util_source));

    let main_doc = documents.get(&main_uri).unwrap();
    let offset = main_source.rfind("greet").unwrap();
    let locations = build_reference_locations(&main_uri, &main_doc, offset, true, &documents);

    assert_eq!(
        locations.len(),
        5,
        "expected importer + imported references"
    );
    assert_eq!(
        locations
            .iter()
            .filter(|location| location.uri == main_uri)
            .count(),
        3,
        "expected import site plus two main.hew call sites"
    );
    assert_eq!(
        locations
            .iter()
            .filter(|location| location.uri == util_uri)
            .count(),
        2,
        "expected greet definition plus wrapper() call in util.hew"
    );
}

#[test]
fn cross_file_references_from_named_import_usage_include_other_open_importers() {
    let main_source = "import util.{ greet };\nfn main() -> i32 { greet() }";
    let helper_source = "import util.{ greet };\nfn helper() -> i32 { greet() }";
    let util_source = "pub fn greet() -> i32 { 1 }\nfn wrapper() -> i32 { greet() }";

    let main_uri = make_test_uri("/project/main.hew");
    let helper_uri = make_test_uri("/project/helper.hew");
    let util_uri = make_test_uri("/project/util.hew");

    let documents: DashMap<Url, DocumentState> = DashMap::new();
    documents.insert(main_uri.clone(), make_doc(main_source));
    documents.insert(helper_uri.clone(), make_doc(helper_source));
    documents.insert(util_uri.clone(), make_doc(util_source));

    let main_doc = documents.get(&main_uri).unwrap();
    let offset = main_source.rfind("greet").unwrap();
    let locations = build_reference_locations(&main_uri, &main_doc, offset, true, &documents);

    assert_eq!(
        locations.len(),
        6,
        "expected both importers plus imported references"
    );
    assert_eq!(
        locations
            .iter()
            .filter(|location| location.uri == main_uri)
            .count(),
        2,
        "expected import site plus usage in main.hew"
    );
    assert_eq!(
        locations
            .iter()
            .filter(|location| location.uri == helper_uri)
            .count(),
        2,
        "expected import site plus usage in helper.hew"
    );
    assert_eq!(
        locations
            .iter()
            .filter(|location| location.uri == util_uri)
            .count(),
        2,
        "expected definition plus wrapper() call in util.hew"
    );
}

#[test]
fn cross_file_references_shadowed_local_usage_stays_local() {
    let main_source = "import util.{ greet };\nfn imported() -> i32 { greet() }\nfn shadowed() -> i32 { let greet = 0; greet }";
    let util_source = "pub fn greet() -> i32 { 1 }\nfn wrapper() -> i32 { greet() }";

    let main_uri = make_test_uri("/project/main.hew");
    let util_uri = make_test_uri("/project/util.hew");

    let documents: DashMap<Url, DocumentState> = DashMap::new();
    documents.insert(main_uri.clone(), make_doc(main_source));
    documents.insert(util_uri.clone(), make_doc(util_source));

    let main_doc = documents.get(&main_uri).unwrap();
    let offset = main_source.rfind("greet").unwrap();
    let locations = build_reference_locations(&main_uri, &main_doc, offset, true, &documents);

    assert!(
        locations.len() >= 2,
        "expected at least the local shadow binding references"
    );
    assert!(
        locations.iter().all(|location| location.uri == main_uri),
        "shadowed local references must stay within main.hew"
    );
}

#[test]
fn cross_file_references_top_level_collision_stays_local() {
    let main_source =
        "import util.{ greet };\nfn greet() -> i32 { 0 }\nfn main() -> i32 { greet() }";
    let util_source = "pub fn greet() -> i32 { 1 }\nfn wrapper() -> i32 { greet() }";

    let main_uri = make_test_uri("/project/main.hew");
    let util_uri = make_test_uri("/project/util.hew");

    let documents: DashMap<Url, DocumentState> = DashMap::new();
    documents.insert(main_uri.clone(), make_doc(main_source));
    documents.insert(util_uri.clone(), make_doc(util_source));

    let main_doc = documents.get(&main_uri).unwrap();
    let offset = main_source.rfind("greet").unwrap();
    let locations = build_reference_locations(&main_uri, &main_doc, offset, true, &documents);

    assert_eq!(
        locations.len(),
        2,
        "expected only the local top-level symbol"
    );
    assert!(locations.iter().all(|location| location.uri == main_uri));
}

#[test]
fn cross_file_goto_named_import_resolves_to_open_document() {
    // `main.hew` imports `Counter` from `counter.hew` (open in the editor).
    let main_source = "import counter.{ Counter };\nfn main() {}";
    let counter_source = "type Counter {\n    value: i32;\n}\n";

    let main_uri = make_test_uri("/project/main.hew");
    let counter_uri = make_test_uri("/project/counter.hew");

    let documents: DashMap<Url, DocumentState> = DashMap::new();
    documents.insert(main_uri.clone(), make_doc(main_source));
    documents.insert(counter_uri.clone(), make_doc(counter_source));

    let imports = collect_imports(main_source);
    let result = find_cross_file_definition(&main_uri, &imports, "Counter", &documents);

    assert!(result.is_some(), "should resolve Counter to counter.hew");
    let (uri, _range) = result.unwrap();
    assert_eq!(uri, counter_uri, "should point to counter.hew");
}

#[test]
fn goto_definition_handler_named_import_usage_jumps_to_imported_file_not_import_line() {
    // Regression test for the bug this helper-level test above cannot catch:
    // `cross_file_goto_named_import_resolves_to_open_document` calls
    // `find_cross_file_definition` directly, bypassing the actual
    // `goto_definition` LSP handler entirely. The handler used to return
    // any `Resolution::def_location()` immediately -- and
    // `Resolution::ImportedItem::def_location()` used to point at the
    // *import statement*, not the real definition -- so this path was
    // never exercised end-to-end and the regression shipped silently.
    //
    // Cursor sits on a *usage* of `Counter` inside `main()`, not on the
    // import line, so this only passes if `goto_definition` actually
    // chases the import across files instead of stopping at the import
    // span.
    let main_source = "import counter.{ Counter };\nfn main() -> Counter { Counter { value: 1 } }";
    let counter_source = "type Counter {\n    value: i32;\n}\n";

    let main_uri = make_test_uri("/project/main.hew");
    let counter_uri = make_test_uri("/project/counter.hew");

    // Build a real `HewLanguageServer` via `tower_lsp::LspService::new`,
    // the same construction path production code uses (`main.rs`). This
    // is the harness-free route the original bug report noted was
    // missing: `Client` has no public constructor of its own, but
    // `LspService::new(init)` hands a real one to `init` and `.inner()`
    // gives back the constructed server for direct, non-async calls into
    // the handler.
    let (service, _socket) = tower_lsp_server::LspService::new(|client| {
        HewLanguageServer::new_with_options(client, Vec::new())
    });
    let server = service.inner();
    server
        .documents
        .insert(main_uri.clone(), make_doc(main_source));
    server
        .documents
        .insert(counter_uri.clone(), make_doc(counter_source));

    // Cursor on the *usage* `Counter` in the return type, not the import.
    let usage_offset = main_source.rfind("-> Counter").unwrap() + "-> ".len();
    let doc = server.documents.get(&main_uri).unwrap();
    let position =
        offset_range_to_lsp(main_source, &doc.line_offsets, usage_offset, usage_offset).start;
    drop(doc);

    let params = GotoDefinitionParams {
        text_document_position_params: tower_lsp_server::ls_types::TextDocumentPositionParams {
            text_document: TextDocumentIdentifier {
                uri: main_uri.clone(),
            },
            position,
        },
        work_done_progress_params: WorkDoneProgressParams::default(),
        partial_result_params: PartialResultParams::default(),
    };

    let response = handlers::navigation::goto_definition(server, &params)
        .expect("goto_definition should resolve the imported Counter usage");
    let GotoDefinitionResponse::Scalar(location) = response else {
        panic!("expected a scalar goto-definition response, got {response:?}");
    };

    assert_eq!(
        location.uri, counter_uri,
        "goto-definition on a named-import usage must jump to the imported \
             file's real definition, not stay on the import statement in the \
             importing file (got uri={:?}, range={:?})",
        location.uri, location.range
    );
}

#[test]
fn cross_file_goto_aliased_import_resolves_by_alias() {
    // `import counter::{ Counter as Cnt }` — cursor on `Cnt` in usage.
    let main_source = "import counter.{ Counter as Cnt };\nfn main() {}";
    let counter_source = "type Counter {\n    value: i32;\n}\n";

    let main_uri = make_test_uri("/project/main.hew");
    let counter_uri = make_test_uri("/project/counter.hew");

    let documents: DashMap<Url, DocumentState> = DashMap::new();
    documents.insert(main_uri.clone(), make_doc(main_source));
    documents.insert(counter_uri.clone(), make_doc(counter_source));

    let imports = collect_imports(main_source);
    // The visible name in the current file is `Cnt` (the alias).
    let result = find_cross_file_definition(&main_uri, &imports, "Cnt", &documents);

    assert!(result.is_some(), "should resolve alias Cnt to counter.hew");
    let (uri, _range) = result.unwrap();
    assert_eq!(uri, counter_uri, "should point to counter.hew");
}

#[test]
fn cross_file_goto_selected_import_resolves_to_open_document() {
    let main_source = "import counter.{ Counter };\nfn main() {}";
    let counter_source = "type Counter {\n    value: i32;\n}\n";

    let main_uri = make_test_uri("/project/main.hew");
    let counter_uri = make_test_uri("/project/counter.hew");

    let documents: DashMap<Url, DocumentState> = DashMap::new();
    documents.insert(main_uri.clone(), make_doc(main_source));
    documents.insert(counter_uri.clone(), make_doc(counter_source));

    let imports = collect_imports(main_source);
    let result = find_cross_file_definition(&main_uri, &imports, "Counter", &documents);

    assert!(result.is_some(), "selected import should resolve Counter");
    let (uri, _range) = result.unwrap();
    assert_eq!(uri, counter_uri);
}

#[test]
fn cross_file_goto_absent_name_returns_none() {
    // `NotDefined` is not in counter.hew.
    let main_source = "import counter.{ Counter };\nfn main() {}";
    let counter_source = "type Counter {\n    value: i32;\n}\n";

    let main_uri = make_test_uri("/project/main.hew");
    let counter_uri = make_test_uri("/project/counter.hew");

    let documents: DashMap<Url, DocumentState> = DashMap::new();
    documents.insert(main_uri.clone(), make_doc(main_source));
    documents.insert(counter_uri.clone(), make_doc(counter_source));

    let imports = collect_imports(main_source);
    let result = find_cross_file_definition(&main_uri, &imports, "NotDefined", &documents);
    assert!(result.is_none(), "should return None for unknown name");
}

#[test]
fn cross_file_goto_name_not_in_explicit_imports_returns_none() {
    // `Bar` is not in the explicit import list even though counter.hew defines it.
    let main_source = "import counter.{ Counter };\nfn main() {}";
    let counter_source = "type Counter {\n    value: i32;\n}\n\ntype Bar {\n    x: i32;\n}\n";

    let main_uri = make_test_uri("/project/main.hew");
    let counter_uri = make_test_uri("/project/counter.hew");

    let documents: DashMap<Url, DocumentState> = DashMap::new();
    documents.insert(main_uri.clone(), make_doc(main_source));
    documents.insert(counter_uri.clone(), make_doc(counter_source));

    let imports = collect_imports(main_source);
    let result = find_cross_file_definition(&main_uri, &imports, "Bar", &documents);
    assert!(
        result.is_none(),
        "Bar was not imported, should not be found"
    );
}

#[test]
fn cross_file_goto_transitive_import_resolves_one_hop_deeper() {
    let main_source = "import middle.{ Counter };\nfn main() {}";
    let middle_source = "import leaf.{ Counter };\nfn helper() {}";
    let leaf_source = "type Counter {\n    value: i32;\n}\n";

    let main_uri = make_test_uri("/project/main.hew");
    let middle_uri = make_test_uri("/project/middle.hew");
    let leaf_uri = make_test_uri("/project/leaf.hew");

    let documents: DashMap<Url, DocumentState> = DashMap::new();
    documents.insert(main_uri.clone(), make_doc(main_source));
    documents.insert(middle_uri, make_doc(middle_source));
    documents.insert(leaf_uri.clone(), make_doc(leaf_source));

    let imports = collect_imports(main_source);
    let result = find_cross_file_definition(&main_uri, &imports, "Counter", &documents);

    assert!(
        result.is_some(),
        "transitive import should resolve Counter through middle.hew"
    );
    let (uri, _range) = result.unwrap();
    assert_eq!(uri, leaf_uri, "should point to leaf.hew");
}

#[test]
fn cross_file_goto_transitive_cycle_returns_none() {
    let main_source = "import middle.{ Counter };\nfn main() {}";
    let middle_source = "import main.{ Counter };\nfn helper() {}";

    let main_uri = make_test_uri("/project/main.hew");
    let middle_uri = make_test_uri("/project/middle.hew");

    let documents: DashMap<Url, DocumentState> = DashMap::new();
    documents.insert(main_uri.clone(), make_doc(main_source));
    documents.insert(middle_uri, make_doc(middle_source));

    let imports = collect_imports(main_source);
    let result = find_cross_file_definition(&main_uri, &imports, "Counter", &documents);

    assert!(
        result.is_none(),
        "cycle should terminate and report no definition"
    );
}

#[test]
fn cross_file_goto_multiple_imports_from_same_file_do_not_leak_seen() {
    let main_source = "import middle.{ Timer };\nimport middle.{ Counter };\nfn main() {}";
    let middle_source = "type Counter {\n    value: i32;\n}\n\ntype Timer {\n    ticks: i32;\n}\n";

    let main_uri = make_test_uri("/project/main.hew");
    let middle_uri = make_test_uri("/project/middle.hew");

    let documents: DashMap<Url, DocumentState> = DashMap::new();
    documents.insert(main_uri.clone(), make_doc(main_source));
    documents.insert(middle_uri.clone(), make_doc(middle_source));

    let imports = collect_imports(main_source);
    let result = find_cross_file_definition(&main_uri, &imports, "Counter", &documents);

    assert!(
        result.is_some(),
        "later imports from the same file should still be evaluated"
    );
    let (uri, _range) = result.unwrap();
    assert_eq!(uri, middle_uri, "should still point to middle.hew");
}
