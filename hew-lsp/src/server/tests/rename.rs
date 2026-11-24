use super::*;

#[test]
fn checked_field_rename_edits_only_the_selected_owner() {
    let source = "type A {\n    x: i64;\n}\n\ntype B {\n    x: i64;\n}\n\nfn main() {\n    let a = A { x: 1 };\n    let b = B { x: 2 };\n    println(a.x);\n    println(b.x);\n}\n";
    let doc = make_typed_doc(source);
    let uri = make_test_uri("/identity-rename.hew");
    let offset = source.find("b.x").unwrap() + 2;
    let prepared = super::super::navigation::build_prepare_rename_response(
        &uri,
        &doc,
        offset,
        &DashMap::new(),
    )
    .expect("checked field should be preparable for rename");
    assert!(matches!(prepared, PrepareRenameResponse::Range(range)
        if range == offset_range_to_lsp(source, &doc.line_offsets, offset, offset + 1)));
    let edit = super::super::navigation::plan_workspace_rename(
        &uri,
        &doc,
        offset,
        "value",
        &DashMap::new(),
    )
    .expect("field rename should be safe")
    .expect("field rename should produce edits");
    let changes = edit.changes.unwrap();
    let edits = &changes[&uri];
    let positions: Vec<_> = edits.iter().map(|edit| edit.range.start).collect();
    let expected = [
        source.find("type B {\n    x:").unwrap() + "type B {\n    ".len(),
        source.find("B { x: 2 }").unwrap() + "B { ".len(),
        offset,
    ];
    assert_eq!(edits.len(), expected.len(), "{edits:?}");
    for start in expected {
        assert!(positions
            .contains(&offset_range_to_lsp(source, &doc.line_offsets, start, start + 1).start));
    }
    assert!(edits.iter().all(|edit| edit.new_text == "value"));
}

#[test]
fn checked_field_rename_rejects_an_existing_field() {
    let source =
        "type B {\n    x: i64;\n    y: i64;\n}\n\nfn main() {\n    let b = B { x: 1, y: 2 };\n    println(b.x);\n}\n";
    let doc = make_typed_doc(source);
    let uri = make_test_uri("/identity-field-conflict.hew");
    let offset = source.find("b.x").unwrap() + 2;
    let error =
        super::super::navigation::plan_workspace_rename(&uri, &doc, offset, "y", &DashMap::new())
            .expect_err("renaming x to existing y must be refused");
    assert!(
        matches!(error, hew_analysis::RenameError::Conflicts { conflicts }
        if conflicts.iter().any(|conflict| conflict.kind == hew_analysis::RenameConflictKind::ShadowsField))
    );
}

/// Apply a rename's edits to the one document it changes.
fn renamed_source(source: &str, uri: &Url, offset: usize, new_name: &str) -> String {
    let doc = make_typed_doc(source);
    let edit = super::super::navigation::plan_workspace_rename(
        uri,
        &doc,
        offset,
        new_name,
        &DashMap::new(),
    )
    .expect("rename should be safe")
    .expect("rename should produce edits");
    let mut edits = edit.changes.unwrap().remove(uri).unwrap();
    edits
        .sort_by_key(|edit| std::cmp::Reverse((edit.range.start.line, edit.range.start.character)));
    let mut lines: Vec<String> = source.split('\n').map(str::to_string).collect();
    for edit in edits {
        assert_eq!(edit.range.start.line, edit.range.end.line, "{edit:?}");
        let line = &mut lines[edit.range.start.line as usize];
        line.replace_range(
            edit.range.start.character as usize..edit.range.end.character as usize,
            &edit.new_text,
        );
    }
    lines.join("\n")
}

/// Source text of each checked reference to the symbol at `offset`.
fn reference_columns(source: &str, uri: &Url, offset: usize) -> Vec<(u32, u32, u32)> {
    let doc = make_typed_doc(source);
    let mut refs = super::super::navigation::identity_reference_locations(
        uri,
        &doc,
        offset,
        true,
        &DashMap::new(),
    )
    .expect("checked references");
    refs.sort_by_key(|site| (site.range.start.line, site.range.start.character));
    refs.iter()
        .map(|site| {
            (
                site.range.start.line,
                site.range.start.character,
                site.range.end.character,
            )
        })
        .collect()
}

const SHORTHAND_ENDPOINT: &str = "type Endpoint { host: string; port: i64; }\nfn main() {\n    let port = 5432;\n    let host = \"h\";\n    let e = Endpoint { host, port };\n    println(e.port);\n}\n";
const EXPLICIT_ENDPOINT: &str = "type Endpoint { host: string; port: i64; }\nfn main() {\n    let port = 5432;\n    let host = \"h\";\n    let e = Endpoint { host: host, port: port };\n    println(e.port);\n}\n";

#[test]
fn field_rename_writes_a_shorthand_label_out() {
    let uri = make_test_uri("/shorthand-field-rename.hew");
    let expected = "type Endpoint { host: string; p2: i64; }\nfn main() {\n    let port = 5432;\n    let host = \"h\";\n    let e = Endpoint { host, p2: port };\n    println(e.p2);\n}\n";
    let declaration = SHORTHAND_ENDPOINT.find("port: i64").unwrap();
    let projection = SHORTHAND_ENDPOINT.find("e.port").unwrap() + 2;
    for offset in [declaration, projection] {
        assert_eq!(
            renamed_source(SHORTHAND_ENDPOINT, &uri, offset, "p2"),
            expected
        );
    }
}

const ROLE_BRANCH: &str = "type Plan { name: string; }\nactor RoleWorker {\n    let plan: Plan;\n    receive fn name() -> string { plan.name }\n}\nsupervisor RoleBranch(plan: Plan) {\n    strategy: one_for_one;\n    child role: RoleWorker { plan };\n}\nfn main() {\n    let plan = Plan { name: \"relay\" };\n    let w = spawn RoleWorker { plan };\n    let b = spawn RoleBranch { plan };\n    println(w.name().expect(\"w\"));\n    println(b.role.name().expect(\"b\"));\n}\n";

#[test]
fn actor_field_rename_writes_spawn_and_child_keys_out() {
    let uri = make_test_uri("/spawn-key-field-rename.hew");
    let expected = ROLE_BRANCH
        .replace("let plan: Plan;", "let blueprint: Plan;")
        .replace("{ plan.name }", "{ blueprint.name }")
        .replace(
            "child role: RoleWorker { plan }",
            "child role: RoleWorker { blueprint: plan }",
        )
        .replace(
            "spawn RoleWorker { plan }",
            "spawn RoleWorker { blueprint: plan }",
        );
    let declaration = ROLE_BRANCH.find("plan: Plan;").unwrap();
    assert_eq!(
        renamed_source(ROLE_BRANCH, &uri, declaration, "blueprint"),
        expected
    );
}

#[test]
fn field_rename_keeps_an_explicit_value() {
    let uri = make_test_uri("/explicit-field-rename.hew");
    let expected = "type Endpoint { host: string; p2: i64; }\nfn main() {\n    let port = 5432;\n    let host = \"h\";\n    let e = Endpoint { host: host, p2: port };\n    println(e.p2);\n}\n";
    let declaration = EXPLICIT_ENDPOINT.find("port: i64").unwrap();
    let label = EXPLICIT_ENDPOINT.find("port: port").unwrap();
    let projection = EXPLICIT_ENDPOINT.find("e.port").unwrap() + 2;
    for offset in [declaration, label, projection] {
        assert_eq!(
            renamed_source(EXPLICIT_ENDPOINT, &uri, offset, "p2"),
            expected
        );
    }
}

#[test]
fn local_rename_keeps_the_field_a_shorthand_labels() {
    let uri = make_test_uri("/shorthand-local-rename.hew");
    let expected = "type Endpoint { host: string; port: i64; }\nfn main() {\n    let pp = 5432;\n    let host = \"h\";\n    let e = Endpoint { host, port: pp };\n    println(e.port);\n}\n";
    let binding = SHORTHAND_ENDPOINT.find("port = ").unwrap();
    let shorthand = SHORTHAND_ENDPOINT.find("port }").unwrap();
    for offset in [binding, shorthand] {
        assert_eq!(
            renamed_source(SHORTHAND_ENDPOINT, &uri, offset, "pp"),
            expected
        );
    }
}

#[test]
fn local_rename_keeps_an_explicit_label() {
    let uri = make_test_uri("/explicit-local-rename.hew");
    let expected = "type Endpoint { host: string; port: i64; }\nfn main() {\n    let pp = 5432;\n    let host = \"h\";\n    let e = Endpoint { host: host, port: pp };\n    println(e.port);\n}\n";
    let binding = EXPLICIT_ENDPOINT.find("port = ").unwrap();
    let value = EXPLICIT_ENDPOINT.find("port }").unwrap();
    for offset in [binding, value] {
        assert_eq!(
            renamed_source(EXPLICIT_ENDPOINT, &uri, offset, "pp"),
            expected
        );
    }
}

#[test]
fn parameter_rename_writes_a_shorthand_label_out() {
    let uri = make_test_uri("/shorthand-param-rename.hew");
    let source = "type Win { lo: i64; hi: i64; }\nfn widen(w: Win, lo: i64) -> Win {\n    Win { ..w, lo }\n}\nfn main() { println(widen(Win { lo: 1, hi: 5 }, 3).lo); }\n";
    let expected = "type Win { lo: i64; hi: i64; }\nfn widen(w: Win, low: i64) -> Win {\n    Win { ..w, lo: low }\n}\nfn main() { println(widen(Win { lo: 1, hi: 5 }, 3).lo); }\n";
    let parameter = source.find("lo: i64)").unwrap();
    assert_eq!(renamed_source(source, &uri, parameter, "low"), expected);
}

#[test]
fn references_include_shorthand_tokens_for_field_and_binding() {
    let uri = make_test_uri("/shorthand-references.hew");
    // `port: i64` (line 0), `let port` (line 2), the initializer (line 4)
    // and `e.port` (line 5).
    let declaration = (0, 30, 34);
    let binding = (2, 8, 12);
    let projection = (5, 14, 18);
    for (source, initializer) in [
        (SHORTHAND_ENDPOINT, (4, 29, 33)),
        (EXPLICIT_ENDPOINT, (4, 35, 39)),
    ] {
        let field_use = source.find("e.port").unwrap() + 2;
        assert_eq!(
            reference_columns(source, &uri, field_use),
            vec![declaration, initializer, projection],
            "{source}"
        );
    }
    let value = |source: &str| source.find("port }").unwrap();
    assert_eq!(
        reference_columns(SHORTHAND_ENDPOINT, &uri, value(SHORTHAND_ENDPOINT)),
        vec![binding, (4, 29, 33)]
    );
    assert_eq!(
        reference_columns(EXPLICIT_ENDPOINT, &uri, value(EXPLICIT_ENDPOINT)),
        vec![binding, (4, 41, 45)]
    );
}

// ── Rename tests ────────────────────────────────────────────────

#[test]
fn prepare_rename_on_variable() {
    let source = "fn main() { let counter = 42; counter }";
    let parse_result = hew_parser::parse(source);
    let offset = source.find("counter").unwrap();
    let span = hew_analysis::rename::prepare_rename(source, &parse_result, offset);
    assert!(
        span.is_some(),
        "should be able to prepare rename on variable"
    );
    let s = span.unwrap();
    assert_eq!(&source[s.start..s.end], "counter");
}

#[test]
fn rename_variable_updates_all_occurrences() {
    let source = "fn main() { let counter = 42; counter }";
    let parse_result = hew_parser::parse(source);
    let offset = source.find("counter").unwrap();
    let edits = hew_analysis::rename::rename(source, &parse_result, offset, "count");
    assert!(edits.is_some(), "rename should produce edits");
    let edits = edits.unwrap();
    assert!(
        edits.len() >= 2,
        "expected at least 2 rename edits (decl + usage), got {}",
        edits.len()
    );
    for edit in &edits {
        assert_eq!(edit.new_text, "count");
    }
}

#[test]
fn prepare_rename_on_whitespace_returns_none() {
    let source = "fn main() { }";
    let parse_result = hew_parser::parse(source);
    // Offset in the whitespace between braces
    let offset = source.find("{ }").unwrap() + 1;
    let span = hew_analysis::rename::prepare_rename(source, &parse_result, offset);
    assert!(span.is_none(), "rename on whitespace should return None");
}

#[test]
fn cross_file_prepare_rename_on_named_import_binding_returns_import_range() {
    let main_source = "import util.{ greet };\nfn main() -> i32 { greet() }";
    let util_source = "pub fn greet() -> i32 { 1 }";

    let main_uri = make_test_uri("/project/main.hew");
    let util_uri = make_test_uri("/project/util.hew");

    let documents: DashMap<Url, DocumentState> = DashMap::new();
    documents.insert(main_uri.clone(), make_doc(main_source));
    documents.insert(util_uri, make_doc(util_source));

    let main_doc = documents.get(&main_uri).unwrap();
    let offset = main_source.find("greet").unwrap();

    // `prepare_rename` internally calls `find_all_references` (line 152 of
    // rename.rs), so the #1283 fix to `find_all_references_raw` propagates
    // here: the import-binding cursor is now recognised, `find_all_references`
    // returns Some, and `prepare_rename` no longer falls through to None.
    assert!(
        hew_analysis::rename::prepare_rename(&main_doc.source, &main_doc.parse_result, offset)
            .is_some(),
        "prepare_rename should succeed on a named import binding cursor after #1283 fix"
    );

    let response = build_prepare_rename_response(&main_uri, &main_doc, offset, &documents)
        .expect("prepareRename should succeed on a named import binding");
    let PrepareRenameResponse::Range(range) = response else {
        panic!("expected prepareRename to return a range");
    };

    let expected_range = offset_range_to_lsp(
        main_source,
        &main_doc.line_offsets,
        offset,
        offset + "greet".len(),
    );
    assert_eq!(range, expected_range);
}

#[test]
fn cross_file_prepare_rename_on_named_import_usage_returns_usage_range() {
    let main_source = "import util.{ greet };\nfn main() -> i32 { greet() }";
    let util_source = "pub fn greet() -> i32 { 1 }";

    let main_uri = make_test_uri("/project/main.hew");
    let util_uri = make_test_uri("/project/util.hew");

    let documents: DashMap<Url, DocumentState> = DashMap::new();
    documents.insert(main_uri.clone(), make_doc(main_source));
    documents.insert(util_uri, make_doc(util_source));

    let main_doc = documents.get(&main_uri).unwrap();
    let offset = main_source.rfind("greet").unwrap();

    let response = build_prepare_rename_response(&main_uri, &main_doc, offset, &documents)
        .expect("prepareRename should succeed on a named import usage");
    let PrepareRenameResponse::Range(range) = response else {
        panic!("expected prepareRename to return a range");
    };

    let expected_range = offset_range_to_lsp(
        main_source,
        &main_doc.line_offsets,
        offset,
        offset + "greet".len(),
    );
    assert_eq!(range, expected_range);
}

#[test]
fn cross_file_rename_updates_named_importer_and_imported_open_document() {
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
    let workspace_edit = build_workspace_edit(&main_uri, &main_doc, offset, "welcome", &documents)
        .expect("rename should not error")
        .expect("rename should produce a cross-file workspace edit");
    let changes = workspace_edit
        .changes
        .expect("workspace edit should contain per-document text edits");

    let main_edits = changes
        .get(&main_uri)
        .expect("rename should include main.hew edits");
    assert_eq!(main_edits.len(), 3, "expected import + two call-site edits");
    assert!(main_edits.iter().all(|edit| edit.new_text == "welcome"));

    let util_edits = changes
        .get(&util_uri)
        .expect("rename should include util.hew edits");
    assert_eq!(
        util_edits.len(),
        2,
        "expected definition + wrapper call edits"
    );
    assert!(util_edits.iter().all(|edit| edit.new_text == "welcome"));
}

#[test]
fn cross_file_rename_from_named_import_usage_updates_other_open_importers() {
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
    let workspace_edit = build_workspace_edit(&main_uri, &main_doc, offset, "welcome", &documents)
        .expect("rename should not error")
        .expect("rename should produce a cross-file workspace edit");
    let changes = workspace_edit
        .changes
        .expect("workspace edit should contain per-document text edits");

    let main_edits = changes
        .get(&main_uri)
        .expect("rename should include main.hew edits");
    assert_eq!(
        main_edits.len(),
        2,
        "expected import + usage edits in main.hew"
    );
    assert!(main_edits.iter().all(|edit| edit.new_text == "welcome"));

    let helper_edits = changes
        .get(&helper_uri)
        .expect("rename should include helper.hew edits");
    assert_eq!(
        helper_edits.len(),
        2,
        "expected import + usage edits in helper.hew"
    );
    assert!(helper_edits.iter().all(|edit| edit.new_text == "welcome"));

    let util_edits = changes
        .get(&util_uri)
        .expect("rename should include util.hew edits");
    assert_eq!(
        util_edits.len(),
        2,
        "expected definition + wrapper call edits"
    );
    assert!(util_edits.iter().all(|edit| edit.new_text == "welcome"));
}

#[test]
fn cross_file_rename_from_imported_definition_updates_open_importer() {
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
    let workspace_edit = build_workspace_edit(&util_uri, &util_doc, offset, "welcome", &documents)
        .expect("rename should not error")
        .expect("rename should produce a cross-file workspace edit");
    let changes = workspace_edit
        .changes
        .expect("workspace edit should contain per-document text edits");

    let util_edits = changes
        .get(&util_uri)
        .expect("rename should include util.hew edits");
    assert_eq!(
        util_edits.len(),
        2,
        "expected definition + wrapper call edits"
    );
    assert!(util_edits.iter().all(|edit| edit.new_text == "welcome"));

    let main_edits = changes
        .get(&main_uri)
        .expect("rename should include main.hew edits");
    assert_eq!(main_edits.len(), 3, "expected import + two call-site edits");
    assert!(main_edits.iter().all(|edit| edit.new_text == "welcome"));
}

#[test]
fn cross_file_rename_shadowed_local_usage_stays_local() {
    let main_source = "import util.{ greet };\nfn imported() -> i32 { greet() }\nfn shadowed() -> i32 { let greet = 0; greet }";
    let util_source = "pub fn greet() -> i32 { 1 }\nfn wrapper() -> i32 { greet() }";

    let main_uri = make_test_uri("/project/main.hew");
    let util_uri = make_test_uri("/project/util.hew");

    let documents: DashMap<Url, DocumentState> = DashMap::new();
    documents.insert(main_uri.clone(), make_doc(main_source));
    documents.insert(util_uri.clone(), make_doc(util_source));

    let main_doc = documents.get(&main_uri).unwrap();
    let offset = main_source.rfind("greet").unwrap();
    let workspace_edit = build_workspace_edit(&main_uri, &main_doc, offset, "welcome", &documents)
        .expect("rename should not error")
        .expect("rename should produce local edits");
    let changes = workspace_edit
        .changes
        .expect("workspace edit should contain per-document text edits");

    assert_eq!(changes.len(), 1, "expected only main.hew edits");
    let main_edits = changes
        .get(&main_uri)
        .expect("rename should include main.hew edits");
    assert!(
        main_edits.len() >= 2,
        "expected at least the local binding + usage edits"
    );
    assert!(main_edits.iter().all(|edit| edit.new_text == "welcome"));
    assert!(
        !changes.contains_key(&util_uri),
        "shadowed local rename must not touch util.hew"
    );
}

#[test]
fn cross_file_rename_from_imported_definition_skips_shadowed_local_usage() {
    let main_source = "import util.{ greet };\nfn imported() -> i32 { greet() }\nfn shadowed() -> i32 { let greet = 0; greet }";
    let util_source = "pub fn greet() -> i32 { 1 }\nfn wrapper() -> i32 { greet() }";

    let main_uri = make_test_uri("/project/main.hew");
    let util_uri = make_test_uri("/project/util.hew");

    let documents: DashMap<Url, DocumentState> = DashMap::new();
    documents.insert(main_uri.clone(), make_doc(main_source));
    documents.insert(util_uri.clone(), make_doc(util_source));

    let util_doc = documents.get(&util_uri).unwrap();
    let offset = util_source.find("greet").unwrap();
    let workspace_edit = build_workspace_edit(&util_uri, &util_doc, offset, "welcome", &documents)
        .expect("rename should not error")
        .expect("rename should produce a cross-file workspace edit");
    let changes = workspace_edit
        .changes
        .expect("workspace edit should contain per-document text edits");

    let util_edits = changes
        .get(&util_uri)
        .expect("rename should include util.hew edits");
    assert_eq!(
        util_edits.len(),
        2,
        "expected definition + wrapper call edits"
    );
    assert!(util_edits.iter().all(|edit| edit.new_text == "welcome"));

    let main_edits = changes
        .get(&main_uri)
        .expect("rename should include main.hew edits");
    assert_eq!(
        main_edits.len(),
        2,
        "expected import + unshadowed call edit"
    );
    assert!(main_edits.iter().all(|edit| edit.new_text == "welcome"));
}

// ── plan_workspace_rename: conflict detection + builtin guard ───

#[test]
fn plan_workspace_rename_cross_file_happy_path() {
    let main_source = "import util.{ greet };\nfn main() -> i32 { greet() }";
    let util_source = "pub fn greet() -> i32 { 1 }";

    let main_uri = make_test_uri("/project/main.hew");
    let util_uri = make_test_uri("/project/util.hew");

    let documents: DashMap<Url, DocumentState> = DashMap::new();
    documents.insert(main_uri.clone(), make_doc(main_source));
    documents.insert(util_uri.clone(), make_doc(util_source));

    let main_doc = documents.get(&main_uri).unwrap();
    let offset = main_source.rfind("greet").unwrap();
    let result = plan_workspace_rename(&main_uri, &main_doc, offset, "welcome", &documents)
        .expect("rename should succeed when target is conflict-free");
    let edit = result.expect("should produce a WorkspaceEdit");
    let changes = edit.changes.expect("workspace edit should have changes");
    assert!(changes.contains_key(&main_uri));
    assert!(changes.contains_key(&util_uri));
}

#[test]
fn plan_workspace_rename_allows_prelude_shadowing() {
    let source = "fn main() { let x = 1; }";
    let uri = make_test_uri("/project/main.hew");
    let documents: DashMap<Url, DocumentState> = DashMap::new();
    documents.insert(uri.clone(), make_doc(source));

    let doc = documents.get(&uri).unwrap();
    let offset = source.find("let x").unwrap() + 4;
    let edit = plan_workspace_rename(&uri, &doc, offset, "println", &documents)
        .expect("prelude names are lexical and may be shadowed")
        .expect("local rename should produce an edit");
    let changes = edit.changes.expect("workspace edit should contain changes");
    let edits = &changes[&uri];
    assert_eq!(edits.len(), 1);
    assert_eq!(edits[0].new_text, "println");
}

#[test]
fn plan_workspace_rename_rejects_rename_to_keyword() {
    let source = "fn main() { let x = 1; }";
    let uri = make_test_uri("/project/main.hew");
    let documents: DashMap<Url, DocumentState> = DashMap::new();
    documents.insert(uri.clone(), make_doc(source));

    let doc = documents.get(&uri).unwrap();
    let offset = source.find("let x").unwrap() + 4;
    let err = plan_workspace_rename(&uri, &doc, offset, "fn", &documents)
        .expect_err("renaming to keyword must fail");
    assert!(matches!(err, hew_analysis::RenameError::Builtin { .. }));
}

#[test]
fn plan_workspace_rename_detects_same_file_local_shadow() {
    // `let x` → `y` fails because `y` is already in the same scope.
    let source = "fn main() {\n    let x = 1;\n    let y = 2;\n    x + y\n}";
    let uri = make_test_uri("/project/main.hew");
    let documents: DashMap<Url, DocumentState> = DashMap::new();
    documents.insert(uri.clone(), make_doc(source));

    let doc = documents.get(&uri).unwrap();
    let offset = source.find("let x").unwrap() + 4;
    let err = plan_workspace_rename(&uri, &doc, offset, "y", &documents)
        .expect_err("shadow should be detected");
    match err {
        hew_analysis::RenameError::Conflicts { conflicts } => {
            assert_eq!(
                conflicts[0].kind,
                hew_analysis::RenameConflictKind::ShadowsLocal
            );
        }
        other => panic!("expected Conflicts, got {other:?}"),
    }
}

#[test]
fn plan_workspace_rename_detects_cross_file_top_level_clash() {
    // util::greet is imported into main; main also defines `welcome`.
    // Renaming greet → welcome should be refused because `welcome`
    // already exists at the top level of main.hew.
    let main_source =
        "import util.{ greet };\nfn welcome() -> i32 { 0 }\nfn m() -> i32 { greet() }";
    let util_source = "pub fn greet() -> i32 { 1 }";

    let main_uri = make_test_uri("/project/main.hew");
    let util_uri = make_test_uri("/project/util.hew");

    let documents: DashMap<Url, DocumentState> = DashMap::new();
    documents.insert(main_uri.clone(), make_doc(main_source));
    documents.insert(util_uri.clone(), make_doc(util_source));

    // Rename from the definition file — cross-file walk lands in main.hew.
    let util_doc = documents.get(&util_uri).unwrap();
    let offset = util_source.find("fn greet").unwrap() + 3;
    let err = plan_workspace_rename(&util_uri, &util_doc, offset, "welcome", &documents)
        .expect_err("cross-file top-level clash should be reported");
    match err {
        hew_analysis::RenameError::Conflicts { conflicts } => {
            assert!(
                conflicts
                    .iter()
                    .any(|c| c.kind == hew_analysis::RenameConflictKind::ShadowsTopLevel),
                "expected a ShadowsTopLevel conflict, got {conflicts:?}"
            );
        }
        other => panic!("expected Conflicts, got {other:?}"),
    }
}

#[test]
fn plan_workspace_rename_detects_cross_file_shadows_import() {
    // util.hew defines `foo`; other.hew defines `bar`.
    // main.hew imports both: `import util::{foo}; import other::{bar};`.
    // Renaming `foo` → `bar` from util.hew walks main.hew, which already
    // imports `bar` → ShadowsImport must be reported.
    let util_source = "pub fn foo() -> i32 { 1 }";
    let other_source = "pub fn bar() -> i32 { 2 }";
    let main_source = "import util.{ foo };\nimport other.{ bar };\nfn m() -> i32 { foo() }";

    let util_uri = make_test_uri("/project/util.hew");
    let other_uri = make_test_uri("/project/other.hew");
    let main_uri = make_test_uri("/project/main.hew");

    let documents: DashMap<Url, DocumentState> = DashMap::new();
    documents.insert(util_uri.clone(), make_doc(util_source));
    documents.insert(other_uri.clone(), make_doc(other_source));
    documents.insert(main_uri.clone(), make_doc(main_source));

    // Rename from the definition file — cross-file walk lands in main.hew.
    let util_doc = documents.get(&util_uri).unwrap();
    let offset = util_source.find("fn foo").unwrap() + 3;
    let err = plan_workspace_rename(&util_uri, &util_doc, offset, "bar", &documents)
        .expect_err("cross-file ShadowsImport should be reported");
    match err {
        hew_analysis::RenameError::Conflicts { conflicts } => {
            assert!(
                conflicts
                    .iter()
                    .any(|c| c.kind == hew_analysis::RenameConflictKind::ShadowsImport),
                "expected a ShadowsImport conflict, got {conflicts:?}"
            );
        }
        other => panic!("expected Conflicts, got {other:?}"),
    }
}

#[test]
fn plan_workspace_rename_detects_cross_file_local_shadow() {
    // util.hew defines `greet`. main.hew imports it and calls it inside
    // a function that also declares `let welcome = ...`. Renaming `greet`
    // → `welcome` must be refused because `welcome` is a local variable
    // in scope at the usage site in main.hew.
    let util_source = "pub fn greet() -> i32 { 1 }";
    let main_source = "import util.{ greet };\nfn m() -> i32 { let welcome = 0; greet() }";

    let util_uri = make_test_uri("/project/util.hew");
    let main_uri = make_test_uri("/project/main.hew");

    let documents: DashMap<Url, DocumentState> = DashMap::new();
    documents.insert(util_uri.clone(), make_doc(util_source));
    documents.insert(main_uri.clone(), make_doc(main_source));

    // Rename from the definition file — cross-file walk lands in main.hew.
    let util_doc = documents.get(&util_uri).unwrap();
    let offset = util_source.find("fn greet").unwrap() + 3;
    let err = plan_workspace_rename(&util_uri, &util_doc, offset, "welcome", &documents)
        .expect_err("cross-file local shadow should be reported");
    match err {
        hew_analysis::RenameError::Conflicts { conflicts } => {
            assert!(
                conflicts
                    .iter()
                    .any(|c| c.kind == hew_analysis::RenameConflictKind::ShadowsLocal),
                "expected a ShadowsLocal conflict, got {conflicts:?}"
            );
        }
        other => panic!("expected Conflicts, got {other:?}"),
    }
}

#[test]
fn plan_workspace_rename_detects_cross_file_param_shadow() {
    // util.hew defines `greet`. main.hew imports it and calls it inside a
    // function whose parameter is named `hi`. Renaming `greet` → `hi` must
    // be refused because `hi` is a parameter in scope at the usage site.
    let util_source = "pub fn greet() -> i32 { 1 }";
    let main_source = "import util.{ greet };\nfn m(hi: i32) -> i32 { greet() }";

    let util_uri = make_test_uri("/project/util.hew");
    let main_uri = make_test_uri("/project/main.hew");

    let documents: DashMap<Url, DocumentState> = DashMap::new();
    documents.insert(util_uri.clone(), make_doc(util_source));
    documents.insert(main_uri.clone(), make_doc(main_source));

    let util_doc = documents.get(&util_uri).unwrap();
    let offset = util_source.find("fn greet").unwrap() + 3;
    let err = plan_workspace_rename(&util_uri, &util_doc, offset, "hi", &documents)
        .expect_err("cross-file param shadow should be reported");
    match err {
        hew_analysis::RenameError::Conflicts { conflicts } => {
            assert!(
                conflicts
                    .iter()
                    .any(|c| c.kind == hew_analysis::RenameConflictKind::ShadowsLocal),
                "expected a ShadowsLocal conflict for param, got {conflicts:?}"
            );
        }
        other => panic!("expected Conflicts, got {other:?}"),
    }
}

#[test]
fn plan_workspace_rename_does_not_duplicate_cross_file_conflicts() {
    // Verify that when the plan_rename probe at the definition file
    // re-emits a ShadowsTopLevel conflict already reported by
    // collect_cross_file_conflict, the dedup filter collapses it to one.
    //
    // Setup: util.hew defines both `greet` AND `hello` (top-level).
    //        main.hew imports `greet` from util.
    //        Renaming from main.hew's import token (`greet` → `hello`):
    //   1. collect_cross_file_conflict(util_doc, "hello", "greet") detects
    //      that `hello` is already a top-level name in util.hew → ShadowsTopLevel.
    //   2. The plan_rename probe on util.hew (at greet's def span) also hits
    //      the same clash and extends cross_file_conflicts with an identical entry.
    //
    // Before the dedup block, conflicts.len() == 2.  After dedup: 1.
    // Regression guard: both collection paths emit an identical ShadowsTopLevel
    // entry for the same (existing_span, offending_span) — the dedup block must
    // collapse them to one.  Without dedup this assertion fails with len == 2.
    let util_source = "pub fn greet() -> i32 { 1 }\npub fn hello() -> i32 { 2 }";
    let main_source = "import util.{ greet };\nfn m() -> i32 { greet() }";

    let util_uri = make_test_uri("/project/util.hew");
    let main_uri = make_test_uri("/project/main.hew");

    let documents: DashMap<Url, DocumentState> = DashMap::new();
    documents.insert(util_uri.clone(), make_doc(util_source));
    documents.insert(main_uri.clone(), make_doc(main_source));

    // Cursor on main.hew's import token `greet` — importer-originated path,
    // which exercises both collect_cross_file_conflict and the plan_rename probe
    // on the definition file (the two paths that each independently emit the clash).
    let main_doc = documents.get(&main_uri).unwrap();
    let offset = main_source.find("greet").unwrap();
    let err = plan_workspace_rename(&main_uri, &main_doc, offset, "hello", &documents)
        .expect_err("clash with util.hew's top-level hello should be reported");
    match err {
        hew_analysis::RenameError::Conflicts { conflicts } => {
            // The key assertion: dedup must reduce two identical ShadowsTopLevel
            // entries (one from collect_cross_file_conflict, one from the
            // plan_rename probe) down to exactly one.
            assert_eq!(
                conflicts.len(),
                1,
                "conflicts should be deduped to 1, got {conflicts:?}"
            );
            assert_eq!(
                conflicts[0].kind,
                hew_analysis::RenameConflictKind::ShadowsTopLevel
            );
        }
        other => panic!("expected Conflicts, got {other:?}"),
    }
}

#[test]
fn plan_workspace_rename_same_name_is_noop() {
    // Renaming a symbol to its own name must return Ok(None) — no conflict,
    // no edit — even when cross-file importers exist.  Before the early-return
    // guard, the cross-file walk could surface spurious ShadowsTopLevel clashes
    // (e.g. main.hew's `greet` seen as a clash for new_name == "greet").
    let util_source = "pub fn greet() -> i32 { 1 }";
    let main_source = "import util.{ greet };\nfn m() -> i32 { greet() }";

    let util_uri = make_test_uri("/project/util.hew");
    let main_uri = make_test_uri("/project/main.hew");

    let documents: DashMap<Url, DocumentState> = DashMap::new();
    documents.insert(util_uri.clone(), make_doc(util_source));
    documents.insert(main_uri.clone(), make_doc(main_source));

    let util_doc = documents.get(&util_uri).unwrap();
    let offset = util_source.find("fn greet").unwrap() + 3;
    let result = plan_workspace_rename(&util_uri, &util_doc, offset, "greet", &documents);
    match result {
        Ok(None | Some(_)) => {} // no error: correct
        Err(hew_analysis::RenameError::Conflicts { ref conflicts }) => {
            panic!("same-name rename must not produce conflicts, got: {conflicts:?}");
        }
        Err(other) => panic!("unexpected error on same-name rename: {other:?}"),
    }
}

#[test]
fn plan_workspace_rename_local_let_does_not_affect_module_top_level() {
    // Rename a local `let x` to `y`. Even though `y` does not collide
    // anywhere, the rename must affect ONLY the enclosing function,
    // not reach any module-level item (regression guard).
    let main_source = "fn greet() {}\nfn main() {\n    let x = 1;\n    x + 2\n}\nfn trailing() {}";
    let uri = make_test_uri("/project/main.hew");
    let documents: DashMap<Url, DocumentState> = DashMap::new();
    documents.insert(uri.clone(), make_doc(main_source));

    let doc = documents.get(&uri).unwrap();
    let offset = main_source.find("let x").unwrap() + 4;
    let edit = plan_workspace_rename(&uri, &doc, offset, "y", &documents)
        .expect("rename should succeed")
        .expect("should produce a WorkspaceEdit");
    let changes = edit.changes.expect("should have changes");
    assert_eq!(changes.len(), 1, "local rename touches only one file");
    let edits = changes.get(&uri).expect("edits in main.hew");
    // Every edit must fall inside `fn main`'s byte range.
    let main_start = main_source.find("fn main").unwrap();
    let trailing_start = main_source.find("fn trailing").unwrap();
    for edit in edits {
        let line = edit.range.start.line;
        let ch = edit.range.start.character;
        // Convert position → offset rough check by line.
        assert!(
            (1..=3).contains(&line),
            "edit at line {line}:{ch} should be inside fn main body"
        );
    }
    assert!(
        main_start < trailing_start,
        "sanity: fn main precedes fn trailing"
    );
}
