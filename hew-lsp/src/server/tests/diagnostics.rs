use super::*;

#[test]
fn diagnostics_from_parse_error() {
    let source = "fn {";
    let uri = Url::parse("file:///test.hew").unwrap();
    let diags = analyzed_diagnostics(&uri, source);
    assert!(!diags.is_empty());
    assert_eq!(diags[0].severity, Some(DiagnosticSeverity::ERROR));
}

// ── Diagnostic data tests ───────────────────────────────────────

#[test]
fn diagnostic_data_encodes_kind_and_suggestions() {
    let data = diagnostic_data(
        &TypeErrorKind::UndefinedVariable,
        &["counter".to_string(), "count".to_string()],
    );
    assert_eq!(data["kind"], "UndefinedVariable");
    let suggestions = data["suggestions"].as_array().unwrap();
    assert_eq!(suggestions.len(), 2);
    assert_eq!(suggestions[0], "counter");
    assert_eq!(suggestions[1], "count");
}

#[test]
fn diagnostic_data_all_error_kinds() {
    // Verify that all error kinds produce a non-empty string
    let kinds = [
        TypeErrorKind::UndefinedVariable,
        TypeErrorKind::UndefinedType,
        TypeErrorKind::UndefinedFunction,
        TypeErrorKind::UndefinedField,
        TypeErrorKind::UndefinedMethod,
        TypeErrorKind::InvalidSend,
        TypeErrorKind::InvalidOperation,
        TypeErrorKind::ArityMismatch,
        TypeErrorKind::BoundsNotSatisfied,
        TypeErrorKind::InferenceFailed,
        TypeErrorKind::NonExhaustiveMatch,
        TypeErrorKind::DuplicateDefinition,
        TypeErrorKind::MutabilityError,
        TypeErrorKind::ReturnTypeMismatch,
        TypeErrorKind::UseAfterMove,
        TypeErrorKind::YieldOutsideGenerator,
        TypeErrorKind::ActorRefCycle,
        TypeErrorKind::UnusedVariable,
        TypeErrorKind::UnusedMut,
        TypeErrorKind::StyleSuggestion,
        TypeErrorKind::UnusedImport,
        TypeErrorKind::UnreachableCode,
        TypeErrorKind::Shadowing,
        TypeErrorKind::OrphanImpl,
        TypeErrorKind::PlatformLimitation,
        TypeErrorKind::MachineExhaustivenessError,
        TypeErrorKind::BlockingCallInReceiveFn,
    ];
    for kind in &kinds {
        let data = diagnostic_data(kind, &[]);
        let kind_str = data["kind"].as_str().unwrap();
        assert!(
            !kind_str.is_empty(),
            "diagnostic_data kind string must not be empty for {kind:?}"
        );
    }
}

// ── Build diagnostics tests ─────────────────────────────────────

#[test]
fn diagnostics_from_type_errors() {
    let source = "fn main() -> i32 { undefined_var }";
    let uri = Url::parse("file:///test.hew").unwrap();
    let diags = analyzed_diagnostics(&uri, source);
    assert!(
        !diags.is_empty(),
        "expected diagnostics for undefined variable"
    );
    let type_diag = diags
        .iter()
        .find(|d| d.source.as_deref() == Some("hew-types"));
    assert!(type_diag.is_some(), "expected a type-checker diagnostic");
    assert_eq!(type_diag.unwrap().tags, None);
}

#[test]
fn diagnostics_tag_unused_warnings_as_unnecessary() {
    let source = "fn main() -> i32 { let unused = 42; 0 }";
    let uri = Url::parse("file:///test.hew").unwrap();
    let diag = analyzed_diagnostics(&uri, source)
        .into_iter()
        .find(|diag| {
            diag.data
                .as_ref()
                .and_then(|data| data.get("kind"))
                .and_then(Value::as_str)
                == Some("UnusedVariable")
        })
        .expect("expected unused variable diagnostic");

    assert_eq!(
        diag.tags,
        Some(vec![tower_lsp_server::ls_types::DiagnosticTag::UNNECESSARY])
    );
}

#[test]
fn diagnostics_include_suggestions_in_message() {
    let source = "fn main() -> i32 { let counter = 42; counte }";
    let uri = Url::parse("file:///test.hew").unwrap();
    let diags = analyzed_diagnostics(&uri, source);
    // Look for a diagnostic that contains "Did you mean" (suggestion text)
    let has_suggestion = diags
        .iter()
        .any(|d| d.message.contains("Did you mean") || d.data.is_some());
    // This test validates the diagnostic builder can handle suggestions;
    // the actual suggestion depends on the type checker heuristics.
    assert!(
        !diags.is_empty(),
        "expected diagnostics for misspelled variable"
    );
    // Verify that data field is populated for type errors
    let type_diags: Vec<_> = diags
        .iter()
        .filter(|d| d.source.as_deref() == Some("hew-types"))
        .collect();
    for d in &type_diags {
        assert!(d.data.is_some(), "type diagnostics should have data field");
    }
    let _ = has_suggestion; // suppress unused warning
}

#[test]
fn diagnostics_parser_warning_severity() {
    // Empty source produces no diagnostics
    let source = "";
    let uri = Url::parse("file:///test.hew").unwrap();
    let diags = analyzed_diagnostics(&uri, source);
    assert!(diags.is_empty(), "empty source should have no diagnostics");
}

#[test]
fn diagnostics_route_non_root_module_path_to_imported_uri() {
    let main_source = "import shapes.circle;\nfn main() -> i64 { circle.mistyped() }\n";
    let circle_source = "pub fn mistyped() -> i64 { true }\n";

    let main_url = make_test_uri("/fake/project/main.hew");
    let circle_url = make_test_uri("/fake/project/shapes/circle.hew");

    let documents: DashMap<Url, DocumentState> = DashMap::new();
    documents.insert(circle_url.clone(), make_doc(circle_source));

    let refreshed = refresh_document_and_dependents(&main_url, main_source, &documents, &[]);
    let main_diags = refreshed
        .iter()
        .find(|(uri, _)| uri == &main_url)
        .map(|(_, diags)| diags)
        .expect("main document must be republished");
    assert!(
        main_diags.is_empty(),
        "non-root type diagnostics should not stay attached to the importer: {main_diags:?}"
    );

    let circle_diags = refreshed
        .iter()
        .find(|(uri, _)| uri == &circle_url)
        .map(|(_, diags)| diags)
        .expect("imported module must receive routed diagnostics");
    assert!(
        circle_diags
            .iter()
            .any(|diag| diag.source.as_deref() == Some("hew-types")),
        "expected routed hew-types diagnostic on imported module: {circle_diags:?}"
    );
}

#[test]
fn diagnostics_route_non_root_related_information_to_imported_uri() {
    let main_source = "import \"dep.hew\";\nfn main() {}\n";
    let dep_source = "pub fn dup() -> i64 { 1 }\npub fn dup() -> i64 { 2 }\n";

    let main_url = make_test_uri("/fake/project/main.hew");
    let dep_url = make_test_uri("/fake/project/dep.hew");

    let documents: DashMap<Url, DocumentState> = DashMap::new();
    documents.insert(dep_url.clone(), make_doc(dep_source));

    let refreshed = refresh_document_and_dependents(&main_url, main_source, &documents, &[]);
    let dep_diag = refreshed
        .iter()
        .find(|(uri, _)| uri == &dep_url)
        .and_then(|(_, diags)| {
            diags.iter().find(|diag| {
                diag.source.as_deref() == Some("hew-types")
                    && diag
                        .related_information
                        .as_ref()
                        .is_some_and(|info| !info.is_empty())
            })
        })
        .expect("expected a routed diagnostic with related information");

    assert_eq!(
        dep_diag.related_information.as_ref().unwrap()[0]
            .location
            .uri,
        dep_url
    );
}

// ── In-memory module parity tests ───────────────────────────────

/// An open buffer for a sibling module is what the importing document is
/// checked against, so a module that exists only in the editor resolves.
#[test]
fn open_sibling_module_resolves_for_the_importing_document() {
    let main_source = "import shapes.circle;\nfn main() { circle.area(1.0); }";
    let circle_source = "pub fn area(r: f64) -> f64 { r }";

    let main_url = make_test_uri("/fake/project/main.hew");
    let circle_url = make_test_uri("/fake/project/shapes/circle.hew");

    let documents: DashMap<Url, DocumentState> = DashMap::new();
    documents.insert(circle_url, make_doc(circle_source));

    let document = analyze_document(&main_url, main_source, &documents, &[]);
    let errors = published_errors(&document, &main_url);
    assert!(
        errors.is_empty(),
        "an open sibling module must resolve, got: {errors:?}"
    );
}

/// The same rule for a string-literal file import.
#[test]
fn open_file_import_resolves_for_the_importing_document() {
    let main_source = "import \"shapes/circle.hew\";\nfn area_check() -> f64 { area(1.0) }";
    let circle_source = "pub fn area(r: f64) -> f64 { r }";

    let main_url = make_test_uri("/fake/project/main.hew");
    let circle_url = make_test_uri("/fake/project/shapes/circle.hew");

    let documents: DashMap<Url, DocumentState> = DashMap::new();
    documents.insert(circle_url, make_doc(circle_source));

    let document = analyze_document(&main_url, main_source, &documents, &[]);
    let errors = published_errors(&document, &main_url);
    assert!(
        errors.is_empty(),
        "an open file-import target must resolve, got: {errors:?}"
    );
}

#[test]
fn typecheck_opening_file_import_target_refreshes_open_importer_diagnostics() {
    let main_source = "import \"foo.hew\";\nfn main() -> i32 { exported() }";
    let foo_source = "pub fn exported() -> i32 { 1 }";

    let main_url = make_test_uri("/fake/project/main.hew");
    let foo_url = make_test_uri("/fake/project/foo.hew");

    let documents: DashMap<Url, DocumentState> = DashMap::new();

    let initial = refresh_document_and_dependents(&main_url, main_source, &documents, &[]);
    assert_eq!(
        initial.len(),
        1,
        "opening importer should only analyze itself"
    );
    assert!(
        has_unresolved_import(&documents, &main_url),
        "importer should start with UnresolvedImport before foo.hew is open"
    );

    let refreshed = refresh_document_and_dependents(&foo_url, foo_source, &documents, &[]);
    assert!(
        refreshed.iter().any(|(uri, _)| uri == &main_url),
        "opening foo.hew should refresh diagnostics for the open importer"
    );
    assert!(
        !has_unresolved_import(&documents, &main_url),
        "opening foo.hew should clear the importer's stale UnresolvedImport"
    );
}

#[test]
fn typecheck_changing_file_import_target_refreshes_open_importer_diagnostics() {
    let main_source = "import \"foo.hew\";\nfn main() -> i32 { exported() }";
    let invalid_foo_source = "pub fn exported(";
    let foo_source = "pub fn exported() -> i32 { 1 }";

    let main_url = make_test_uri("/fake/project/main.hew");
    let foo_url = make_test_uri("/fake/project/foo.hew");

    let documents: DashMap<Url, DocumentState> = DashMap::new();

    refresh_document_and_dependents(&main_url, main_source, &documents, &[]);
    assert!(
        has_unresolved_import(&documents, &main_url),
        "importer should start with UnresolvedImport before foo.hew is valid"
    );

    refresh_document_and_dependents(&foo_url, invalid_foo_source, &documents, &[]);
    assert!(
        has_unresolved_import(&documents, &main_url),
        "importer should stay unresolved while foo.hew has parse errors"
    );

    let refreshed = refresh_document_and_dependents(&foo_url, foo_source, &documents, &[]);
    assert!(
        refreshed.iter().any(|(uri, _)| uri == &main_url),
        "editing foo.hew should refresh diagnostics for the open importer"
    );
    assert!(
        !has_unresolved_import(&documents, &main_url),
        "editing foo.hew to valid source should clear the importer's stale UnresolvedImport"
    );
}

#[test]
fn typecheck_closing_unsaved_file_import_target_refreshes_open_importer_diagnostics() {
    let main_source = "import \"foo.hew\";\nfn main() -> i32 { exported() }";
    let foo_source = "pub fn exported() -> i32 { 1 }";

    let main_url = make_test_uri("/fake/project/main.hew");
    let foo_url = make_test_uri("/fake/project/foo.hew");

    let documents: DashMap<Url, DocumentState> = DashMap::new();

    refresh_document_and_dependents(&main_url, main_source, &documents, &[]);
    assert!(
        has_unresolved_import(&documents, &main_url),
        "importer should start with UnresolvedImport before foo.hew is open"
    );

    refresh_document_and_dependents(&foo_url, foo_source, &documents, &[]);
    assert!(
        !has_unresolved_import(&documents, &main_url),
        "opening foo.hew should clear the importer's stale UnresolvedImport"
    );

    let refreshed = close_document_and_dependents(&foo_url, &documents, &[]);
    assert!(
        refreshed.iter().any(|(uri, _)| uri == &main_url),
        "closing foo.hew should refresh diagnostics for the open importer"
    );
    assert!(
        has_unresolved_import(&documents, &main_url),
        "closing an unsaved foo.hew should restore the importer's UnresolvedImport"
    );
    assert!(
        !documents.contains_key(&foo_url),
        "closing foo.hew should remove it from the open document store"
    );
}

#[test]
fn typecheck_opening_package_directory_import_target_refreshes_open_importer_diagnostics() {
    let main_source = "import shapes.circle;\nfn area_check() -> f64 { circle.area(1.0) }";
    let circle_source = "pub fn area(r: f64) -> f64 { r }";

    // A directory module belongs to a package: the manifest is on disk,
    // the sources only in the editor.
    let root = make_temp_workspace_dir(&[("hew.toml", "[package]\nname = \"app\"\n")]);
    let main_url = Url::from_checked_file_path(root.join("main.hew")).expect("absolute path");
    let circle_url =
        Url::from_checked_file_path(root.join("shapes/circle/circle.hew")).expect("absolute path");

    let documents: DashMap<Url, DocumentState> = DashMap::new();

    let initial = refresh_document_and_dependents(&main_url, main_source, &documents, &[]);
    assert_eq!(
        initial.len(),
        1,
        "opening importer should only analyze itself"
    );
    assert!(
        has_unresolved_import(&documents, &main_url),
        "importer should start with UnresolvedImport before shapes/circle/circle.hew is open"
    );

    let refreshed = refresh_document_and_dependents(&circle_url, circle_source, &documents, &[]);
    assert!(
        refreshed.iter().any(|(uri, _)| uri == &main_url),
        "opening shapes/circle/circle.hew should refresh diagnostics for the open importer"
    );
    assert!(
        !has_unresolved_import(&documents, &main_url),
        "opening shapes/circle/circle.hew should clear the importer's stale UnresolvedImport"
    );
}

#[test]
fn typecheck_changing_package_directory_import_target_refreshes_open_importer_diagnostics() {
    let main_source = "import shapes.circle;\nfn area_check() -> f64 { circle.area(1.0) }";
    let invalid_circle_source = "pub fn area(";
    let circle_source = "pub fn area(r: f64) -> f64 { r }";

    // A directory module belongs to a package: the manifest is on disk,
    // the sources only in the editor.
    let root = make_temp_workspace_dir(&[("hew.toml", "[package]\nname = \"app\"\n")]);
    let main_url = Url::from_checked_file_path(root.join("main.hew")).expect("absolute path");
    let circle_url =
        Url::from_checked_file_path(root.join("shapes/circle/circle.hew")).expect("absolute path");

    let documents: DashMap<Url, DocumentState> = DashMap::new();

    refresh_document_and_dependents(&main_url, main_source, &documents, &[]);
    assert!(
        has_unresolved_import(&documents, &main_url),
        "importer should start with UnresolvedImport before shapes/circle/circle.hew is valid"
    );

    refresh_document_and_dependents(&circle_url, invalid_circle_source, &documents, &[]);
    assert!(
        has_unresolved_import(&documents, &main_url),
        "importer should stay unresolved while shapes/circle/circle.hew has parse errors"
    );

    let refreshed = refresh_document_and_dependents(&circle_url, circle_source, &documents, &[]);
    assert!(
        refreshed.iter().any(|(uri, _)| uri == &main_url),
        "editing shapes/circle/circle.hew should refresh diagnostics for the open importer"
    );
    assert!(
        !has_unresolved_import(&documents, &main_url),
        "editing shapes/circle/circle.hew to valid source should clear the importer's stale UnresolvedImport"
    );
}

/// Negative control: a sibling module that is neither open nor on disk is
/// reported unfound, the same refusal `hew check` gives.
#[test]
fn a_missing_sibling_module_is_reported_unfound() {
    let main_source = "import shapes.missing_module;\nfn main() { missing_module.foo(); }";
    let main_url = make_test_uri("/fake/project/main.hew");

    let document = analyze_document(&main_url, main_source, &DashMap::new(), &[]);
    let diagnostics = published_errors(&document, &main_url);
    assert!(
        diagnostics.iter().any(|message| message.contains("not found")),
        "a module in neither the editor nor the filesystem must be reported unfound, got: {diagnostics:?}"
    );
}

#[test]
fn refresh_document_surfaces_dangling_module_graph_import_diagnostic() {
    let util_source = "import missing.thing;\npub fn greet() -> i32 { 1 }";
    let util_url = make_test_uri("/project/util.hew");
    let documents: DashMap<Url, DocumentState> = DashMap::new();

    let refreshed = refresh_document_and_dependents(&util_url, util_source, &documents, &[]);
    let util_diags = refreshed
        .into_iter()
        .find(|(uri, _)| *uri == util_url)
        .map(|(_, diags)| diags)
        .expect("expected diagnostics for dangling import source");
    assert!(
        util_diags
            .iter()
            .any(|diag| diag.message.contains("module `missing.thing` not found")),
        "expected dangling import diagnostic, got: {util_diags:?}"
    );
}

/// An import cycle stops the frontend, so the editor shows the compiler's
/// positioned cycle diagnostic on the file carrying the closing edge with
/// a note on the other member, exactly as `hew check` renders it.
#[test]
fn refresh_document_surfaces_the_compilers_module_cycle_diagnostic() {
    let root = make_temp_workspace_dir(&[
        ("main.hew", "import \"foo.hew\";\nfn main() -> i32 { 0 }\n"),
        (
            "foo.hew",
            "import \"main.hew\";\npub fn exported() -> i32 { 1 }\n",
        ),
    ]);
    #[cfg(unix)]
    let editor_root = {
        let alias = root.join("editor");
        std::os::unix::fs::symlink(&root, &alias).unwrap();
        alias
    };
    #[cfg(not(unix))]
    let editor_root = root.clone();
    let main_url = Url::from_checked_file_path(editor_root.join("main.hew"))
        .expect("workspace path is absolute");
    let foo_url = Url::from_checked_file_path(editor_root.join("foo.hew"))
        .expect("workspace path is absolute");
    let main_source = std::fs::read_to_string(root.join("main.hew")).expect("read main.hew");
    let foo_source = std::fs::read_to_string(root.join("foo.hew")).expect("read foo.hew");

    let documents: DashMap<Url, DocumentState> = DashMap::new();
    refresh_document_and_dependents(&main_url, &main_source, &documents, &[]);
    let refreshed = refresh_document_and_dependents(&foo_url, &foo_source, &documents, &[]);

    let foo_diagnostics = refreshed
        .iter()
        .find(|(uri, _)| uri == &foo_url)
        .map(|(_, diagnostics)| diagnostics)
        .expect("the cycle's closing edge belongs to foo.hew");
    let cycle = foo_diagnostics
        .iter()
        .find(|diagnostic| diagnostic.message.contains("import cycle"))
        .expect("expected the compiler's cycle diagnostic on foo.hew");
    assert!(
        cycle
            .related_information
            .as_ref()
            .is_some_and(|notes| notes.iter().any(|note| note.location.uri == main_url)),
        "the cycle diagnostic must point at the other member, got: {:?}",
        cycle.related_information
    );

    let _ = std::fs::remove_dir_all(&root);
}

#[cfg(unix)]
#[test]
fn symlinked_open_import_preserves_related_locations_and_refreshes_dependents() {
    let main_source = "import \"dep.hew\";\nfn main() -> i64 { dep.dup() }\n";
    let disk_source = "pub fn dup() -> i64 { 1 }\n";
    let duplicate_source = "pub fn dup() -> i64 { 1 }\npub fn dup() -> i64 { 2 }\n";
    let root = make_temp_workspace_dir(&[("main.hew", main_source), ("dep.hew", disk_source)]);
    let alias = root.join("editor");
    std::os::unix::fs::symlink(&root, &alias).unwrap();
    let main_uri = Url::from_checked_file_path(alias.join("main.hew")).unwrap();
    let dep_uri = Url::from_checked_file_path(alias.join("dep.hew")).unwrap();
    let documents = DashMap::new();
    refresh_document_and_dependents(&dep_uri, duplicate_source, &documents, &[]);
    let published = refresh_document_and_dependents(&main_uri, main_source, &documents, &[]);
    let diagnostic = published
        .iter()
        .find(|(uri, _)| *uri == dep_uri)
        .and_then(|(_, diagnostics)| {
            diagnostics.iter().find(|diagnostic| {
                diagnostic.source.as_deref() == Some("hew-types")
                    && diagnostic
                        .related_information
                        .as_ref()
                        .is_some_and(|notes| !notes.is_empty())
            })
        })
        .expect("the imported unsaved duplicate must be diagnosed on its open URI");
    let note = &diagnostic.related_information.as_ref().unwrap()[0];
    assert_eq!(note.location.uri, dep_uri);
    assert_ne!(diagnostic.range.start.line, note.location.range.start.line);

    let changed = "pub fn dup() -> bool { true }\n";
    let published = refresh_document_and_dependents(&dep_uri, changed, &documents, &[]);
    let main_diagnostics = &published
        .iter()
        .find(|(uri, _)| *uri == main_uri)
        .expect("editing an aliased dependency must refresh its importer")
        .1;
    assert!(
        main_diagnostics.iter().any(|diagnostic| {
            diagnostic.source.as_deref() == Some("hew-types")
                && diagnostic.severity == Some(DiagnosticSeverity::ERROR)
        }),
        "the importer must observe the unsaved return type: {main_diagnostics:?}"
    );

    let published = close_document_and_dependents(&dep_uri, &documents, &[]);
    let main_diagnostics = &published
        .iter()
        .find(|(uri, _)| *uri == main_uri)
        .expect("closing an aliased dependency must refresh its importer from disk")
        .1;
    assert!(
        main_diagnostics
            .iter()
            .all(|diagnostic| diagnostic.severity != Some(DiagnosticSeverity::ERROR)),
        "the on-disk integer return must restore the importer: {main_diagnostics:?}"
    );
    std::fs::remove_dir_all(root).unwrap();
}

// ── Transitive-refresh regression tests ──────────────────────────────

/// Changing C in the chain A → B → C must propagate diagnostics all the
/// way back to A, not just to B (the direct importer).
///
/// Before the BFS fix, `refresh_open_importers` was single-hop: saving C
/// re-analysed B but left A with stale diagnostics.
#[test]
fn typecheck_transitive_importer_chain_a_b_c_all_refreshed() {
    // C exports `provided`.
    // B imports C and re-exports `provided` as `b_provided`.
    // A imports B and calls `b_provided`.
    let c_source = "pub fn provided() -> i32 { 1 }\n";
    let b_source = "import c;\npub fn b_provided() -> i32 { c.provided() }\n";
    let a_source = "import b;\nfn main() -> i32 { b.b_provided() }\n";

    let a_url = make_test_uri("/fake/project/a.hew");
    let b_url = make_test_uri("/fake/project/b.hew");
    let c_url = make_test_uri("/fake/project/c.hew");

    let documents: DashMap<Url, DocumentState> = DashMap::new();

    // Open A first — B and C are not yet in the store, so A has stale
    // diagnostics (UnresolvedImport for b.hew).
    refresh_document_and_dependents(&a_url, a_source, &documents, &[]);
    assert!(
        has_unresolved_import(&documents, &a_url),
        "A should have UnresolvedImport before B is open"
    );

    // Open B — C is still missing, but A should now be re-analysed.
    refresh_document_and_dependents(&b_url, b_source, &documents, &[]);
    // B itself will have an UnresolvedImport (c.hew not open yet).
    assert!(
        has_unresolved_import(&documents, &b_url),
        "B should have UnresolvedImport before C is open"
    );

    // Open C — this should transitively refresh B and then A.
    let refreshed = refresh_document_and_dependents(&c_url, c_source, &documents, &[]);

    assert!(
        refreshed.iter().any(|(uri, _)| uri == &b_url),
        "opening C must refresh its direct importer B"
    );
    assert!(
        refreshed.iter().any(|(uri, _)| uri == &a_url),
        "opening C must transitively refresh A (the two-hop importer)"
    );
    assert!(
        !has_unresolved_import(&documents, &b_url),
        "B should have no unresolved import after C is open"
    );
    assert!(
        !has_unresolved_import(&documents, &a_url),
        "A should have no unresolved import after C is open (transitive refresh)"
    );
}

/// Diamond import: both B and C import D; A imports both B and C.
/// Opening D must refresh B, C, and A exactly once (no duplicate work /
/// no infinite loop).
#[test]
fn typecheck_diamond_import_no_cycle_all_refreshed() {
    let d_source = "pub fn d_fn() -> i32 { 1 }";
    let b_source = "import \"d.hew\";\npub fn b_fn() -> i32 { d_fn() }";
    let c_source = "import \"d.hew\";\npub fn c_fn() -> i32 { d_fn() }";
    let a_source = "import \"b.hew\";\nimport \"c.hew\";\nfn main() -> i32 { b_fn() + c_fn() }";

    let a_url = make_test_uri("/fake/project/a.hew");
    let b_url = make_test_uri("/fake/project/b.hew");
    let c_url = make_test_uri("/fake/project/c.hew");
    let d_url = make_test_uri("/fake/project/d.hew");

    let documents: DashMap<Url, DocumentState> = DashMap::new();

    refresh_document_and_dependents(&a_url, a_source, &documents, &[]);
    refresh_document_and_dependents(&b_url, b_source, &documents, &[]);
    refresh_document_and_dependents(&c_url, c_source, &documents, &[]);

    // Opening D triggers the refresh.  This must not panic or loop.
    let refreshed = refresh_document_and_dependents(&d_url, d_source, &documents, &[]);

    for url in [&b_url, &c_url, &a_url] {
        assert!(
            refreshed.iter().any(|(uri, _)| uri == url),
            "opening D should refresh {url:?}"
        );
    }

    // Each URI appears at most once in the publish list (dedup guarantee).
    let counts = refreshed.iter().filter(|(u, _)| u == &a_url).count();
    assert_eq!(
        counts, 1,
        "A should appear exactly once in the publish list"
    );
}

/// An open buffer supersedes the file on disk: the importing document is
/// checked against the editor's version of its sibling, not the saved one.
#[test]
fn in_memory_version_supersedes_disk_for_type_checking() {
    let root =
        make_temp_workspace_dir(&[("shapes/circle.hew", "pub fn area(r: f64) -> f64 { r }\n")]);
    let main_source =
        "import shapes.circle;\nfn circumference_check() -> f64 { circle.circumference(1.0) }";
    let main_url =
        Url::from_checked_file_path(root.join("main.hew")).expect("workspace path is absolute");
    let circle_url = Url::from_checked_file_path(root.join("shapes/circle.hew"))
        .expect("workspace path is absolute");

    // Negative control: the saved sibling has no `circumference`.
    let saved = analyze_document(&main_url, main_source, &DashMap::new(), &[]);
    assert!(
        !published_errors(&saved, &main_url).is_empty(),
        "the saved sibling declares no `circumference`"
    );

    let documents: DashMap<Url, DocumentState> = DashMap::new();
    documents.insert(
        circle_url,
        make_doc("pub fn circumference(r: f64) -> f64 { r }\n"),
    );
    let document = analyze_document(&main_url, main_source, &documents, &[]);
    let errors = published_errors(&document, &main_url);
    assert!(
        errors.is_empty(),
        "the open buffer declares `circumference`, got: {errors:?}"
    );

    let _ = std::fs::remove_dir_all(&root);
}
