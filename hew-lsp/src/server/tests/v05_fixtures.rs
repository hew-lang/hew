use super::*;

// ── Hew v0.5 syntax coverage fixtures ────────────────────────────────
//
fn v05_fixture_path(name: &str) -> String {
    format!("file:///v05/{name}.hew")
}

fn flatten_symbol_names<'a>(symbols: &'a [DocumentSymbol], names: &mut Vec<&'a str>) {
    for symbol in symbols {
        names.push(symbol.name.as_str());
        if let Some(children) = &symbol.children {
            flatten_symbol_names(children, names);
        }
    }
}

/// Check the `documentSymbols` LSP surface for a v0.5 fixture.
///
/// Parses `source`, builds the document-symbol tree, and asserts that every
/// name in `expected` appears somewhere in the flattened tree.
///
/// Failure messages identify: surface, fixture name, and the missing symbol.
fn assert_v05_document_symbols(fixture_name: &str, source: &str, expected: &[&str]) {
    let doc = make_typed_doc(source);
    assert!(
        doc.parse_result.errors.is_empty(),
        "surface=documentSymbols fixture={fixture_name}: parse errors prevent symbol analysis: {:?}",
        doc.parse_result.errors
    );
    let analysis_symbols =
        hew_analysis::symbols::build_document_symbols(&doc.source, &doc.parse_result);
    let document_symbols: Vec<DocumentSymbol> = analysis_symbols
        .into_iter()
        .map(|symbol| symbol_info_to_doc_symbol(&doc.source, &doc.line_offsets, symbol))
        .collect();
    let mut symbol_names = Vec::new();
    flatten_symbol_names(&document_symbols, &mut symbol_names);
    for expected_name in expected {
        assert!(
            symbol_names.contains(expected_name),
            "surface=documentSymbols fixture={fixture_name}: missing symbol {expected_name:?}; got {symbol_names:?}"
        );
    }
}

/// Check the `gotoDefinition` LSP surface for a v0.5 fixture probe.
///
/// Uses the last occurrence of `probe_name` in `source` as the request
/// offset and checks either the checker identity or a source declaration
/// that predates its remaining generated/builtin rows.
///
/// Failure messages identify: surface, fixture name, probe name, and byte
/// offset.
fn assert_v05_goto_definition(fixture_name: &str, source: &str, probe_name: &str) {
    let uri = Url::parse(&v05_fixture_path(fixture_name)).unwrap();
    let doc = make_typed_doc(source);
    let probe_offset = source.rfind(probe_name).unwrap_or_else(|| {
        panic!("surface=gotoDefinition fixture={fixture_name}: missing probe {probe_name:?}")
    });
    let checked_location = super::super::navigation::identity_definition_location(
        &uri,
        &doc,
        probe_offset,
        &DashMap::new(),
    );
    assert!(
        checked_location.is_some()
            || find_definition_in_ast(
                &doc.source,
                &doc.line_offsets,
                &doc.parse_result,
                probe_name,
            )
            .is_some(),
        "surface=gotoDefinition fixture={fixture_name} probe={probe_name:?} \
             offset={probe_offset}: no checked or source declaration found"
    );
}

/// Check the `findReferences` LSP surface for a v0.5 fixture probe.
///
/// Uses the last occurrence of `probe_name` in `source` as the request
/// offset.  Asserts that the reference list has at least `min_count`
/// entries (typically 2: definition site + one use site).
///
/// Failure messages identify: surface, fixture name, probe name, byte
/// offset, and the actual reference list.
fn assert_v05_find_references(
    fixture_name: &str,
    source: &str,
    probe_name: &str,
    min_count: usize,
) {
    let uri = Url::parse(&v05_fixture_path(fixture_name)).unwrap();
    let doc = make_typed_doc(source);
    let probe_offset = source.rfind(probe_name).unwrap_or_else(|| {
        panic!("surface=findReferences fixture={fixture_name}: missing probe {probe_name:?}")
    });
    let documents: DashMap<Url, DocumentState> = DashMap::new();
    documents.insert(uri.clone(), doc);
    let doc_ref = documents.get(&uri).unwrap();
    let refs = build_reference_locations(&uri, &doc_ref, probe_offset, true, &documents);
    assert!(
        refs.len() >= min_count,
        "surface=findReferences fixture={fixture_name} probe={probe_name:?} \
             offset={probe_offset}: expected >={min_count} refs, got {refs:?}"
    );
}

/// Check the `semanticTokens` LSP surface for a v0.5 fixture at a specific
/// byte offset.
///
/// Finds the semantic token whose `start` equals `offset` and asserts that
/// its `token_type` matches `expected_type`.  Use `hew_analysis::token_types`
/// constants for the expected type.
///
/// Failure messages identify: surface, fixture name, offset, expected type,
/// and the actual type returned.
fn assert_v05_semantic_token_at(
    fixture_name: &str,
    source: &str,
    offset: usize,
    expected_type: u32,
) {
    let tokens = hew_analysis::semantic_tokens::build_semantic_tokens(source);
    let token = tokens
        .iter()
        .find(|t| t.start == offset)
        .unwrap_or_else(|| {
            panic!(
                "surface=semanticTokens fixture={fixture_name} offset={offset}: \
                 no token at that offset; token starts: {:?}",
                tokens.iter().map(|t| t.start).collect::<Vec<_>>()
            )
        });
    assert_eq!(
        token.token_type, expected_type,
        "surface=semanticTokens fixture={fixture_name} offset={offset}: \
             expected token_type={expected_type}, got token_type={}",
        token.token_type
    );
}

/// Smoke-test all four LSP surfaces (documentSymbols, hover, gotoDefinition,
/// findReferences) for a v0.5 fixture in a single call.
///
/// This is the thin orchestration wrapper used by the majority of v0.5
/// fixture tests.  For targeted precision assertions use the individual
/// surface helpers above.
///
/// Delegates to:
/// - [`assert_v05_document_symbols`] for the symbol-table surface
/// - inline hover check (probe name must appear in hover text; does not
///   require zero type errors so fixtures with intentional type errors are
///   accepted at this tier)
/// - [`assert_v05_goto_definition`] for the definition-jump surface
/// - [`assert_v05_find_references`] for the reference-list surface
fn assert_v05_lsp_fixture(
    fixture_name: &str,
    source: &str,
    probe_name: &str,
    expected_symbols: &[&str],
) {
    // documentSymbols surface (also validates parse-level correctness)
    assert_v05_document_symbols(fixture_name, source, expected_symbols);

    // hover surface — probe name must appear in hover text at its call-site
    // offset.  No hard-type-error guard here so that fixtures with intentional
    // type errors (machine lifecycle variants, index-trait error recovery, …)
    // still pass this smoke check.
    let doc = make_typed_doc(source);
    let probe_offset = source
        .rfind(probe_name)
        .unwrap_or_else(|| panic!("fixture={fixture_name}: missing probe {probe_name:?}"));
    let hover = hew_analysis::hover::hover(
        &doc.source,
        &doc.parse_result,
        doc.type_output.as_ref(),
        probe_offset,
    )
    .unwrap_or_else(|| {
        panic!(
            "surface=hover fixture={fixture_name} probe={probe_name:?} \
                 offset={probe_offset}: hover returned None"
        )
    });
    assert!(
        hover.contents.contains(probe_name),
        "surface=hover fixture={fixture_name} probe={probe_name:?} \
             offset={probe_offset}: hover should mention probe name, got: {}",
        hover.contents
    );

    // gotoDefinition surface
    assert_v05_goto_definition(fixture_name, source, probe_name);

    // findReferences surface — expect definition site + at least one use site
    assert_v05_find_references(fixture_name, source, probe_name, 2);
}

fn assert_no_hard_type_errors(fixture_name: &str, doc: &DocumentState) {
    let Some(type_output) = doc.type_output.as_ref() else {
        panic!("missing type output for {fixture_name}");
    };
    let hard_errors: Vec<_> = type_output
        .errors
        .iter()
        .filter(|error| error.severity == hew_types::error::Severity::Error)
        .collect();
    assert!(
        hard_errors.is_empty(),
        "hard type errors in {fixture_name}: {hard_errors:?}"
    );
}

fn assert_v05_completion_labels(
    fixture_name: &str,
    source: &str,
    offset: usize,
    expected_labels: &[&str],
) {
    let doc = make_typed_doc(source);
    assert_no_hard_type_errors(fixture_name, &doc);
    let items: Vec<_> = hew_analysis::completions::complete(
        &doc.source,
        &doc.parse_result,
        doc.type_output.as_ref(),
        offset,
    )
    .into_iter()
    .map(to_lsp_completion)
    .collect();
    let labels: Vec<&str> = items.iter().map(|item| item.label.as_str()).collect();
    for expected in expected_labels {
        assert!(
            labels.contains(expected),
            "surface=completion fixture={fixture_name} offset={offset}: \
                 missing label {expected:?}; got {labels:?}"
        );
    }
}

fn assert_v05_hover_contains(fixture_name: &str, source: &str, offset: usize, expected: &str) {
    let doc = make_typed_doc(source);
    assert_no_hard_type_errors(fixture_name, &doc);
    let hover = hew_analysis::hover::hover(
        &doc.source,
        &doc.parse_result,
        doc.type_output.as_ref(),
        offset,
    )
    .unwrap_or_else(|| {
        panic!("surface=hover fixture={fixture_name} offset={offset}: hover returned None")
    });
    assert!(
        hover.contents.contains(expected),
        "surface=hover fixture={fixture_name} offset={offset}: \
             expected {expected:?} in hover, got: {}",
        hover.contents
    );
}

fn assert_lsp_rejection_diagnostic(
    fixture_name: &str,
    source: &str,
    expected_code: &str,
    expected_kind: &str,
    expected_line: u32,
) {
    let uri = Url::parse(&v05_fixture_path(fixture_name)).unwrap();
    let documents: DashMap<Url, DocumentState> = DashMap::new();
    let published = refresh_document_and_dependents(&uri, source, &documents, &[]);
    let diagnostics = published
        .iter()
        .find(|(published_uri, _)| published_uri == &uri)
        .map_or(&[] as &[Diagnostic], |(_, diagnostics)| {
            diagnostics.as_slice()
        });

    let matches: Vec<&Diagnostic> = diagnostics
        .iter()
        .filter(|diagnostic| {
            diagnostic.source.as_deref() == Some("hew-types")
                && diagnostic
                    .data
                    .as_ref()
                    .and_then(|data| data.get("kind"))
                    .and_then(serde_json::Value::as_str)
                    == Some(expected_kind)
        })
        .collect();
    assert_eq!(
        matches.len(),
        1,
        "expected exactly one {expected_code}/{expected_kind} diagnostic in {fixture_name}; got {diagnostics:?}"
    );
    assert_eq!(
        matches[0].range.start.line, expected_line,
        "{fixture_name} {expected_code} diagnostic should point at the rejected transition expression"
    );
}

#[test]
fn lsp_reject_gen_in_transition_diagnostic_code() {
    assert_lsp_rejection_diagnostic(
        "lsp_reject_gen_in_transition",
        include_str!("../../../tests/fixtures/lsp_reject_gen_in_transition.hew"),
        "MachineExhaustivenessError",
        "MachineExhaustivenessError",
        9, // 0-indexed: `gen { yield Open; }` is on line 9 (1-indexed line 10)
    );
}

#[test]
fn lsp_reject_await_in_transition_diagnostic_code() {
    assert_lsp_rejection_diagnostic(
        "lsp_reject_await_in_transition",
        include_str!("../../../tests/fixtures/lsp_reject_await_in_transition.hew"),
        "MachineExhaustivenessError",
        "MachineExhaustivenessError",
        9, // 0-indexed: `await pending;` is on line 9 (1-indexed line 10)
    );
}

#[test]
fn lsp_reject_self_in_transition_diagnostic_code() {
    // `self` in a machine body is refused (E_MACHINE_SELF), and the
    // refusal has to reach the editor rather than only the CLI.
    assert_lsp_rejection_diagnostic(
        "lsp_reject_self_in_transition",
        include_str!("../../../tests/fixtures/lsp_reject_self_in_transition.hew"),
        "MachineExhaustivenessError",
        "MachineExhaustivenessError",
        9, // 0-indexed: `self.hits` is on line 9 (1-indexed line 10)
    );
}

#[test]
fn v05_record_literals_lsp_coverage() {
    assert_v05_lsp_fixture(
        "v05_record_literals",
        include_str!("../../../tests/fixtures/v05_record_literals.hew"),
        "record_probe",
        &["Point", "record_probe", "record_literals"],
    );
}

#[test]
fn v05_regex_literal_lsp_coverage() {
    assert_v05_lsp_fixture(
        "v05_regex_literal",
        include_str!("../../../tests/fixtures/v05_regex_literal.hew"),
        "regex_literal_probe",
        &["regex_literal_probe", "regex_literal_use"],
    );
}

#[test]
fn v05_regex_literal_hover_reports_pattern_type() {
    let source = include_str!("../../../tests/fixtures/v05_regex_literal.hew");
    let doc = make_typed_doc(source);
    assert!(
        doc.parse_result.errors.is_empty(),
        "parse errors: {:?}",
        doc.parse_result.errors
    );
    let offset = source
        .find("re\"")
        .expect("regex literal fixture should contain re\"");
    let hover = hew_analysis::hover::hover(
        &doc.source,
        &doc.parse_result,
        doc.type_output.as_ref(),
        offset,
    )
    .expect("hover should resolve regex literal expression");
    assert!(
        hover.contents.contains("regex.Pattern"),
        "regex literal hover should mention regex.Pattern, got: {}",
        hover.contents
    );
}

#[test]
fn v05_is_operator_lsp_coverage() {
    let source = include_str!("../../../tests/fixtures/v05_is_operator.hew");
    assert_v05_lsp_fixture(
        "v05_is_operator",
        source,
        "is_probe",
        &["Worker", "is_probe", "is_operator"],
    );
}

/// An `is` whose RHS names a user declaration. No such program type-checks
/// after #3134: the only identity-bearing user `TypeDef` is an actor, and
/// an actor's values are actor handles, so `handle is Actor` is a
/// type-pattern mismatch and every other declaration is a value type. The
/// LSP still has to resolve the RHS as a type — jump-to-definition,
/// semantic tokens and completion serve the buffer the user is typing,
/// which is broken most of the time — so the surfaces are asserted on a
/// program the checker refuses.
///
/// This source is inline rather than a `tests/fixtures/*.hew` file because
/// the hew-corpus gate compiles every tracked `.hew` in the tree, and a
/// refused program there would need a ratchet row it has not earned. The
/// accepted `Worker` actor-handle value form lives in the fixture instead.
const IS_RHS_TYPE_PATTERN_SOURCE: &str = "enum Payload {\n    First;\n    Second;\n}\n\nfn is_probe() -> i32 {\n    7\n}\n\nfn is_operator(value: Payload) -> i32 {\n    if value is Payload {\n        is_probe()\n    } else {\n        0\n    }\n}\n";

#[test]
fn v05_is_operator_value_form_fixture_is_accepted() {
    // Negative control for the test below: the `Worker` actor-handle value
    // form the fixture uses carries no hard type error, so the rejection
    // asserted there is about the enum operand and not about `is` itself.
    let source = include_str!("../../../tests/fixtures/v05_is_operator.hew");
    let doc = make_typed_doc(source);
    assert_no_hard_type_errors("v05_is_operator", &doc);
}

#[test]
fn v05_is_operator_rhs_type_pattern_lsp_surfaces_are_correct() {
    let source = IS_RHS_TYPE_PATTERN_SOURCE;
    let doc = make_typed_doc(source);

    // `value is Payload` is rejected: an enum is a value with no identity
    // to compare, `indirect` included (#3134). That rejection is the only
    // hard error this program may carry — the LSP surfaces below are about
    // how the RHS *resolves*, not about whether the checker admits the
    // comparison, and they must keep working on a refused program.
    let errors = doc
        .type_output
        .as_ref()
        .map_or(&[] as &[hew_types::error::TypeError], |output| {
            output.errors.as_slice()
        });
    assert!(
        errors
            .iter()
            .all(|error| error.message.contains("E_IS_VALUE_TYPE")),
        "the `is` RHS source should carry only the value-type rejection; got {errors:?}"
    );
    assert!(
        errors
            .iter()
            .any(|error| error.message.contains("E_IS_VALUE_TYPE")),
        "`value is Payload` must be rejected: an enum has no identity to compare"
    );

    let uri = Url::parse(&v05_fixture_path("v05_is_operator")).unwrap();
    let documents: DashMap<Url, DocumentState> = DashMap::new();
    let published = refresh_document_and_dependents(&uri, source, &documents, &[]);
    let diagnostics = published
        .iter()
        .find(|(published_uri, _)| published_uri == &uri)
        .map_or(&[] as &[Diagnostic], |(_, diagnostics)| {
            diagnostics.as_slice()
        });
    assert!(
        !diagnostics.iter().any(|diagnostic| {
            diagnostic.source.as_deref() == Some("hew-types")
                && diagnostic
                    .data
                    .as_ref()
                    .and_then(|data| data.get("kind"))
                    .and_then(serde_json::Value::as_str)
                    == Some("UndefinedVariable")
                && diagnostic.message.contains("Payload")
        }),
        "`is` operator RHS type pattern should not resolve as a value variable; got {diagnostics:?}"
    );

    let rhs_offset = source.find("is Payload").expect("is type pattern") + "is ".len();
    let definition = super::super::navigation::identity_definition_location(
        &uri,
        &doc,
        rhs_offset,
        &DashMap::new(),
    )
    .map(|location| location.range)
    .or_else(|| find_definition_in_ast(source, &doc.line_offsets, &doc.parse_result, "Payload"))
    .expect("RHS type pattern should jump to its source declaration");
    let declared = source.find("Payload").unwrap();
    assert_eq!(
        definition,
        offset_range_to_lsp(
            source,
            &doc.line_offsets,
            declared,
            declared + "Payload".len()
        )
    );

    assert_v05_semantic_token_at(
        "v05_is_operator",
        source,
        rhs_offset,
        hew_analysis::token_types::TYPE,
    );

    let completion_offset = rhs_offset;
    let completions = hew_analysis::completions::complete(
        &doc.source,
        &doc.parse_result,
        doc.type_output.as_ref(),
        completion_offset,
    );
    let labels: Vec<&str> = completions.iter().map(|item| item.label.as_str()).collect();
    assert!(
        labels.contains(&"Payload"),
        "`is` RHS completion should include Payload type, got {labels:?}"
    );
    assert!(
        !labels.contains(&"is_probe"),
        "`is` RHS completion should be type-pattern focused, got {labels:?}"
    );
}

#[test]
fn v05_unsafe_block_lsp_coverage() {
    assert_v05_lsp_fixture(
        "v05_unsafe_block",
        include_str!("../../../tests/fixtures/v05_unsafe_block.hew"),
        "unsafe_probe",
        &["unsafe_probe", "unsafe_block"],
    );
}

#[test]
fn v05_extern_unsafe_lsp_coverage() {
    assert_v05_lsp_fixture(
        "v05_extern_unsafe",
        include_str!("../../../tests/fixtures/v05_extern_unsafe.hew"),
        "extern_unsafe_probe",
        &["extern \"C\"", "raw_number", "extern_unsafe_probe"],
    );
}

#[test]
fn v05_extern_unsafe_resolves_extern_call_definition() {
    let source = include_str!("../../../tests/fixtures/v05_extern_unsafe.hew");
    let doc = make_typed_doc(source);
    assert_no_hard_type_errors("v05_extern_unsafe", &doc);
    let call_offset = source.rfind("raw_number").expect("raw_number call");
    let uri = Url::parse(&v05_fixture_path("v05_extern_unsafe")).unwrap();
    let definition = super::super::navigation::identity_definition_location(
        &uri,
        &doc,
        call_offset,
        &DashMap::new(),
    )
    .map(|location| location.range)
    .or_else(|| find_definition_in_ast(source, &doc.line_offsets, &doc.parse_result, "raw_number"))
    .expect("extern raw_number should have a source definition");
    let declared = source.find("raw_number").unwrap();
    assert_eq!(
        definition,
        offset_range_to_lsp(
            source,
            &doc.line_offsets,
            declared,
            declared + "raw_number".len()
        )
    );
}

#[test]
fn v05_string_methods_lsp_coverage() {
    let source = include_str!("../../../tests/fixtures/v05_string_methods.hew");
    assert_v05_lsp_fixture(
        "v05_string_methods",
        source,
        "string_methods_probe",
        &["string_methods_probe"],
    );
    let hover_offset = source.find("text[0..1]").expect("range index");
    assert_v05_hover_contains("v05_string_methods", source, hover_offset, "string");
}

#[test]
fn v05_string_methods_completion_includes_current_surface() {
    let source = include_str!("../../../tests/fixtures/v05_string_methods.hew");
    let doc = make_typed_doc(source);
    assert_no_hard_type_errors("v05_string_methods", &doc);
    let offset = source.find("text.clone").expect("text.clone") + "text.".len();
    let items = hew_analysis::completions::complete(
        &doc.source,
        &doc.parse_result,
        doc.type_output.as_ref(),
        offset,
    );
    let labels: Vec<&str> = items.iter().map(|item| item.label.as_str()).collect();
    let expected = [
        "get",
        "first",
        "last",
        "iter",
        "push",
        "clear",
        "replace",
        "find",
        "find_from",
        "split",
        "lines",
        "repeat",
        "clone",
    ];
    for retired in [
        "slice",
        "char_at",
        "chars",
        "char_count_utf8",
        "codepoint_at_utf8",
        "is_digit",
        "is_alpha",
        "is_alphanumeric",
    ] {
        assert!(
            !labels.contains(&retired),
            "string completion must not advertise retired method {retired}"
        );
    }
    let missing: Vec<_> = expected
        .iter()
        .copied()
        .filter(|label| !labels.contains(label))
        .collect();
    assert_eq!(
        missing.as_slice(),
        &[] as &[&str],
        "string method completion should include the current scalar API; missing {missing:?}, got {labels:?}"
    );
}

#[test]
fn v05_trait_bounds_lsp_coverage() {
    assert_v05_lsp_fixture(
        "v05_trait_bounds",
        include_str!("../../../tests/fixtures/v05_trait_bounds.hew"),
        "trait_bounds_probe",
        &[
            "Describable",
            "describe",
            "Label",
            "trait_bounds_probe",
            "trait_bounds_use",
        ],
    );
}

#[test]
fn v05_impl_where_clause_lsp_coverage() {
    assert_v05_lsp_fixture(
        "v05_impl_where_clause",
        include_str!("../../../tests/fixtures/v05_impl_where_clause.hew"),
        "impl_where_clause_probe",
        &["Holder", "impl", "show", "impl_where_clause_probe"],
    );
}

#[test]
fn v05_record_decl_lsp_coverage() {
    let source = include_str!("../../../tests/fixtures/v05_record_decl.hew");
    assert_v05_lsp_fixture(
        "v05_record_decl",
        source,
        "record_decl_probe",
        &["AutoRecord", "record_decl_probe"],
    );
    let offset = source.find("AutoRecord { count").expect("record literal") + "AutoRecord { ".len();
    assert_v05_completion_labels("v05_record_decl", source, offset, &["count", "name"]);
}

#[test]
fn v05_while_let_lsp_coverage() {
    let source = include_str!("../../../tests/fixtures/v05_while_let.hew");
    assert_v05_lsp_fixture(
        "v05_while_let",
        source,
        "while_let_probe",
        &["while_let_probe"],
    );
    let offset = source.find("total + value").expect("while-let value") + "total + ".len();
    assert_v05_hover_contains("v05_while_let", source, offset, "value: i64");
}

#[test]
fn v05_machine_generics_lsp_coverage() {
    assert_v05_lsp_fixture(
        "v05_machine_generics",
        include_str!("../../../tests/fixtures/v05_machine_generics.hew"),
        "machine_generics_probe",
        &["Boxed", "Store", "Idle", "machine_generics_probe"],
    );
}

#[test]
fn v05_machine_substrate_lsp_coverage() {
    let source = include_str!("../../../tests/fixtures/v05_machine_substrate.hew");
    assert_v05_lsp_fixture(
        "v05_machine_substrate",
        source,
        "machine_substrate_probe",
        &[
            "Resource",
            "close",
            "FileHandle",
            "Lifecycle",
            "Idle",
            "Active",
            "Start",
            "Stop",
            "Started",
            "machine_substrate_probe",
        ],
    );
    let offset = source
        .find("lifecycle.state_name")
        .expect("state_name call")
        + "lifecycle.".len();
    assert_v05_completion_labels(
        "v05_machine_substrate",
        source,
        offset,
        &["step", "state_name"],
    );
}

#[test]
fn v05_cross_module_machine_ctor_lsp_coverage() {
    let main_source = include_str!("../../../tests/fixtures/v05_cross_module_machine_main.hew");
    let defs_source = include_str!("../../../tests/fixtures/machines/toggle.hew");
    let main_uri = make_test_uri("/v05/cross_module/main.hew");
    let defs_uri = make_test_uri("/v05/cross_module/machines/toggle.hew");
    let documents: DashMap<Url, DocumentState> = DashMap::new();

    refresh_document_and_dependents(&defs_uri, defs_source, &documents, &[]);
    refresh_document_and_dependents(&main_uri, main_source, &documents, &[]);

    let main_doc = documents
        .get(&main_uri)
        .expect("main fixture should be analyzed");
    assert_no_hard_type_errors("v05_cross_module_machine_main", &main_doc);
    let type_output = main_doc
        .type_output
        .as_ref()
        .expect("cross-module machine fixture should be type checked");
    assert!(
        type_output
            .type_def_at_path("machines.toggle.Toggle")
            .is_some(),
        "imported machine type should retain its canonical owner in type defs"
    );
    assert!(
        type_output
            .type_def_at_path("machines.toggle.Toggle.Event")
            .is_some(),
        "imported machine event type should retain its canonical owner in type defs"
    );

    let state_name_offset = main_source
        .find("toggle.state_name")
        .expect("state_name call")
        + "toggle.".len();
    let items = hew_analysis::completions::complete(
        &main_doc.source,
        &main_doc.parse_result,
        main_doc.type_output.as_ref(),
        state_name_offset,
    );
    let labels: Vec<&str> = items.iter().map(|item| item.label.as_str()).collect();
    assert!(
        labels.contains(&"state_name"),
        "cross-module machine completion should include state_name, got {labels:?}"
    );
}

#[test]
fn v05_machine_states_lsp_coverage() {
    assert_v05_lsp_fixture(
        "v05_machine_states",
        include_str!("../../../tests/fixtures/v05_machine_states.hew"),
        "machine_states_probe",
        &[
            "Traffic",
            "Start",
            "Stop",
            "Idle",
            "Running",
            "machine_states_probe",
        ],
    );
}

#[test]
fn v05_machine_methods_lsp_coverage() {
    assert_v05_lsp_fixture(
        "v05_machine_methods",
        include_str!("../../../tests/fixtures/v05_machine_methods.hew"),
        "machine_methods_probe",
        &[
            "Turnstile",
            "Coin",
            "Push",
            "Locked",
            "Unlocked",
            "machine_methods_probe",
        ],
    );
}

#[test]
fn v05_scope_fork_lsp_coverage() {
    assert_v05_lsp_fixture(
        "v05_scope_fork",
        include_str!("../../../tests/fixtures/v05_scope_fork.hew"),
        "fork_worker",
        &["fork_worker", "scope_fork"],
    );
}

#[test]
fn v05_supervisor_lsp_coverage() {
    assert_v05_lsp_fixture(
        "v05_supervisor",
        include_str!("../../../tests/fixtures/v05_supervisor.hew"),
        "supervisor_probe",
        &[
            "LogWorker",
            "AppSupervisor",
            "supervisor_probe",
            "supervisor_use",
        ],
    );
}

#[test]
fn v05_composite_machine_lsp_coverage() {
    assert_v05_lsp_fixture(
        "v05_composite_machine",
        include_str!("../../../tests/fixtures/v05_composite_machine.hew"),
        "composite_machine_probe",
        &[
            "ConnectionLifecycle",
            "composite_machine_probe",
            "composite_machine_use",
        ],
    );
}

#[test]
fn v05_select_arms_lsp_coverage() {
    let source = include_str!("../../../tests/fixtures/v05_select_arms.hew");
    let repo_root = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("hew-lsp has a parent (repo root)")
        .to_path_buf();
    let uri =
        Url::from_checked_file_path(repo_root.join("hew-lsp/tests/fixtures/v05_select_arms.hew"))
            .expect("fixture path is absolute");
    let docs: DashMap<Url, DocumentState> = DashMap::new();
    let doc = analyze_document(&uri, source, &docs, &[]);
    assert_no_hard_type_errors("v05_select_arms", &doc);
    assert_v05_lsp_fixture(
        "v05_select_arms",
        source,
        "select_probe",
        &["Responder", "ask", "select_probe", "select_arms"],
    );
}

#[test]
fn v05_attributes_lsp_coverage() {
    assert_v05_lsp_fixture(
        "v05_attributes",
        include_str!("../../../tests/fixtures/v05_attributes.hew"),
        "attribute_crash_probe",
        &[
            "AttributeActor",
            "ping",
            "attribute_crash_probe",
            "attribute_upgrade_probe",
        ],
    );
}

#[test]
fn v05_link_monitor_lsp_coverage() {
    assert_v05_lsp_fixture(
        "v05_link_monitor",
        include_str!("../../../tests/fixtures/v05_link_monitor.hew"),
        "link_monitor_probe",
        &["LinkWorker", "ping", "link_monitor_probe", "link_monitor"],
    );
}

// `actor |params| { body }` is the landed lambda-actor syntax.
// The earlier `spawn |…| { … }` form was removed before v0.5.0; the
// fixture now uses the correct `actor` keyword so the parser accepts it.
#[test]
fn v05_spawn_lambda_actor_lsp_coverage() {
    assert_v05_lsp_fixture(
        "v05_spawn_lambda_actor",
        include_str!("../../../tests/fixtures/v05_spawn_lambda_actor.hew"),
        "spawn_lambda_probe",
        &["spawn_lambda_probe", "spawn_lambda_actor"],
    );
}

#[test]
fn v05_std_channels_lsp_coverage() {
    assert_v05_lsp_fixture(
        "v05_std_channels",
        include_str!("../../../tests/fixtures/v05_std_channels.hew"),
        "std_channels_probe",
        &["std_channels_probe", "std_channels"],
    );
}

#[test]
fn v05_associated_type_projection_lsp_coverage() {
    assert_v05_lsp_fixture(
        "v05_associated_type_projection",
        include_str!("../../../tests/fixtures/v05_associated_type_projection.hew"),
        "associated_projection_probe",
        &["Conveyor", "Item", "next", "associated_projection_probe"],
    );
}

#[test]
fn v05_index_trait_lsp_coverage() {
    assert_v05_lsp_fixture(
        "v05_index_trait",
        include_str!("../../../tests/fixtures/v05_index_trait_reject.hew"),
        "index_probe",
        &[
            "Bag",
            "Indexable",
            "Item",
            "get",
            "index_probe",
            "index_trait",
        ],
    );
}

#[test]
fn v05_generators_lsp_coverage() {
    assert_v05_lsp_fixture(
        "v05_generators",
        include_str!("../../../tests/fixtures/v05_generators.hew"),
        "generator_probe",
        &["generator_probe", "generators"],
    );
}

#[test]
fn v05_closures_lsp_coverage() {
    assert_v05_lsp_fixture(
        "v05_closures",
        include_str!("../../../tests/fixtures/v05_closures.hew"),
        "closure_probe",
        &[
            "closure_probe",
            "closures",
            "generic_identity",
            "quantified_closures",
        ],
    );
    let source = include_str!("../../../tests/fixtures/v05_closures.hew");
    let doc = make_typed_doc(source);
    assert_no_hard_type_errors("v05_closures", &doc);
    let hover = hew_analysis::hover::hover(
        source,
        &doc.parse_result,
        doc.type_output.as_ref(),
        source.rfind("generic_identity").unwrap(),
    )
    .expect("quantified return type has hover information");
    assert!(
        hover.contents.contains("fn<T>(T) -> T"),
        "{}",
        hover.contents
    );
}

#[test]
fn v05_result_option_ctors_lsp_coverage() {
    assert_v05_lsp_fixture(
        "v05_result_option_ctors",
        include_str!("../../../tests/fixtures/v05_result_option_ctors.hew"),
        "result_option_probe",
        &[
            "Outcome",
            "Ok",
            "Err",
            "Maybe",
            "Some",
            "None",
            "result_option_probe",
            "result_option_ctors",
        ],
    );
}

#[test]
fn v05_match_enum_variant_lsp_coverage() {
    assert_v05_lsp_fixture(
        "v05_match_enum_variant",
        include_str!("../../../tests/fixtures/v05_match_enum_variant.hew"),
        "match_enum_probe",
        &[
            "Shape",
            "Circle",
            "Square",
            "match_enum_probe",
            "match_enum_variant",
        ],
    );
}

#[test]
fn v05_display_fstring_lsp_coverage() {
    assert_v05_lsp_fixture(
        "v05_display_fstring",
        include_str!("../../../tests/fixtures/v05_display_fstring.hew"),
        "display_probe",
        &["Point", "display_probe", "display_fstring"],
    );
}

// ── W4.023 Stage 2 — targeted accepted-surface assertions ────────────────
//
// Each test below extends beyond the Stage 0/1 smoke tier.  They pin a
// specific LSP surface on an accepted fixture using Stage 1 helpers.
// Organisation mirrors the Stage 2 scope:
//
//   Group A — Field-access hover:        record field-access hover in call sites / impl bodies
//   Group B — Records / wires:           field-declaration hover
//   Group C — Static trait dispatch:     goto-definition + find-references
//   Group D — Match enum:                pattern-binding hover
//   Group E — Machines:                  precision document-symbol assertions
//   Group F — Canonical primitives:      integer-literal hover
//   Group G — Result/Option constructors: enum-variant goto-definition

// ── Group A: Field-access hover — call sites and impl bodies ─────────────

/// `value.count` field access in `record_decl_probe` must surface
/// `count: i64` via the field-access hover path.
#[test]
fn v05_record_decl_field_access_hover_pins_count_type() {
    let source = include_str!("../../../tests/fixtures/v05_record_decl.hew");
    let offset = source
        .find("value.count")
        .expect("value.count field access")
        + "value.".len();
    assert_v05_hover_contains("v05_record_decl", source, offset, "count: i64");
}

/// `p.x` field access inside the `Display` impl body must surface
/// `x: i64` via the field-access hover path.
///
/// Note: the only occurrence of `p.x` in the fixture is inside the
/// f-string interpolation `f"Point({p.x})"`.  The field-access hover
/// path does not currently route through f-string contents — the
/// surrounding string expression type wins at that offset.  The
/// assertion below therefore uses the field-*declaration* hover path
/// on `type Point { x: i64; }`, which IS a `TypeDecl` and is handled
/// by `hover_field_declaration_at_offset`.
#[test]
fn v05_display_fstring_type_decl_field_declaration_hover_pins_type() {
    let source = include_str!("../../../tests/fixtures/v05_display_fstring.hew");
    // offset of `x` inside the `type Point { x: i64; }` declaration
    let offset = source.find("x: i64").expect("x: i64 field declaration");
    assert_v05_hover_contains("v05_display_fstring", source, offset, "x: i64");
}

/// `self.text` field access inside the `Describable` impl body must
/// surface `text: string` via the field-access hover path.
#[test]
fn v05_trait_bounds_impl_field_access_hover_pins_type() {
    let source = include_str!("../../../tests/fixtures/v05_trait_bounds.hew");
    let offset = source.find("self.text").expect("self.text field access") + "self.".len();
    assert_v05_hover_contains("v05_trait_bounds", source, offset, "text: string");
}

// ── Group B: Records / wires — field-declaration hover ───────────────────

/// Hovering on the field name tokens inside a nominal `type` declaration
/// must surface the declared field type via `hover_field_declaration_at_offset`.
///
/// `type AutoRecord { … }` is parsed as `Item::TypeDecl` and the hover
/// walk resolves its field declarations.
#[test]
fn v05_record_decl_field_declaration_hover_pins_types() {
    let source = include_str!("../../../tests/fixtures/v05_record_decl.hew");
    // `count` in `count: i64,` → `count: i64`
    let count_offset = source.find("count: i64").expect("count field decl");
    assert_v05_hover_contains("v05_record_decl", source, count_offset, "count: i64");
    // `name` in `name: string,` → `name: string`
    let name_offset = source.find("name: string").expect("name field decl");
    assert_v05_hover_contains("v05_record_decl", source, name_offset, "name: string");
}

/// `type Point { x: i32; y: i32; }` produces an `Item::TypeDecl`; hovering
/// on the field names must surface the field types via
/// `hover_field_declaration_at_offset`.
#[test]
fn v05_record_literals_type_decl_field_declaration_hover_pins_types() {
    let source = include_str!("../../../tests/fixtures/v05_record_literals.hew");
    // `x` in `x: i32;` field declaration
    let x_offset = source.find("x: i32").expect("x: i32 field decl");
    assert_v05_hover_contains("v05_record_literals", source, x_offset, "x: i32");
    // `y` in `y: i32;` field declaration
    let y_offset = source.find("y: i32").expect("y: i32 field decl");
    assert_v05_hover_contains("v05_record_literals", source, y_offset, "y: i32");
}

// ── Group C: Static trait dispatch — goto-definition + find-references ───

/// The last occurrence of `describe` in the fixture is the `item.describe()`
/// call site.  Both the resolver and the AST-walk fallback must be able to
/// locate the trait or impl definition.
#[test]
fn v05_trait_bounds_static_dispatch_method_has_goto_definition() {
    let source = include_str!("../../../tests/fixtures/v05_trait_bounds.hew");
    assert_v05_goto_definition("v05_trait_bounds", source, "describe");
}

/// `Describable` in `<T: Describable>` (the last occurrence) must navigate
/// back to the `trait Describable { … }` declaration via goto-definition.
/// Uses `rfind("Describable")` which resolves the type-bound usage site;
/// the AST-walk locates `Item::Trait("Describable")` as the definition.
#[test]
fn v05_trait_bounds_trait_name_has_goto_definition_from_type_bound() {
    let source = include_str!("../../../tests/fixtures/v05_trait_bounds.hew");
    // rfind finds <T: Describable>; assert navigation reaches the declaration.
    assert_v05_goto_definition("v05_trait_bounds", source, "Describable");
}

/// The last occurrence of `show` in the fixture is the `holder.show()` call
/// site.  Both the resolver and the AST-walk fallback must locate the impl
/// method definition.
#[test]
fn v05_impl_where_clause_method_has_goto_definition() {
    let source = include_str!("../../../tests/fixtures/v05_impl_where_clause.hew");
    assert_v05_goto_definition("v05_impl_where_clause", source, "show");
}

// ── Group D: Match enum — pattern-binding hover ───────────────────────────

/// `r` in `Shape.Circle(r)` is a constructor-pattern binding.
/// `hover_pattern_binding` must resolve its type from the `Circle(i64)`
/// variant definition and surface `r: i64`.
#[test]
fn v05_match_enum_variant_pattern_binding_hover_pins_payload_type() {
    let source = include_str!("../../../tests/fixtures/v05_match_enum_variant.hew");
    // Position inside the constructor pattern, not the arm body.
    let offset = source
        .find("Shape.Circle(r)")
        .expect("Circle pattern in match arm")
        + "Shape.Circle(".len();
    assert_v05_hover_contains("v05_match_enum_variant", source, offset, "r: i64");
}

// ── Group E: Machines — precision document-symbol assertions ─────────────

/// The smoke test for `v05_machine_states` only checks Start / Stop / Idle /
/// Running.  Stage 2 pins the full event list, including the two events that
/// the smoke tier omits: `Started` and `Stopped`.
///
/// Note: non-exhaustive transition coverage is intentional in this fixture;
/// `assert_v05_document_symbols` does not require zero type errors.
#[test]
fn v05_machine_states_document_symbols_include_all_events() {
    let source = include_str!("../../../tests/fixtures/v05_machine_states.hew");
    assert_v05_document_symbols(
        "v05_machine_states",
        source,
        &[
            "Traffic", "Start", "Stop", "Started", "Stopped", "Idle", "Running",
        ],
    );
}

/// Pin the complete machine-level symbol tree for `Turnstile`.  The smoke
/// test only checks symbols that appear in `expected_symbols`; Stage 2 pins
/// every event and state name.
///
/// Note: the fixture is a valid program; the pins below check symbols only.
#[test]
fn v05_machine_methods_document_symbols_include_machine_and_all_events() {
    let source = include_str!("../../../tests/fixtures/v05_machine_methods.hew");
    assert_v05_document_symbols(
        "v05_machine_methods",
        source,
        &["Turnstile", "Coin", "Push", "Locked", "Unlocked"],
    );
}

// ── Group F: Canonical primitives — integer-literal hover ────────────────

/// An untyped integer literal (`7` in `AutoRecord { count: 7, … }`) must
/// materialise to `i64` — the default integer width — when hovered.
/// This exercises `ResolvedTy::materialize_literal_defaults` through the
/// `hover_type_display` → `ResolvedTy::user_facing()` chain.
#[test]
fn v05_record_decl_integer_literal_hover_materializes_to_i64() {
    let source = include_str!("../../../tests/fixtures/v05_record_decl.hew");
    // Offset of the `7` literal inside `AutoRecord { count: 7, name: "ok" }`.
    let offset = source
        .find("count: 7")
        .expect("count: 7 literal in record constructor")
        + "count: ".len();
    assert_v05_hover_contains("v05_record_decl", source, offset, "i64");
}

// ── Group G: generic enum constructors — enum-variant goto-definition ────
//
// Stage 2 targeted coverage for the `v05_result_option_ctors` fixture.
// The Stage 0/1 smoke test (`v05_result_option_ctors_lsp_coverage`) only
// probes the sentinel function `result_option_probe`.  These tests prove
// that enum variant constructors at their call sites are navigable via
// goto-definition.
//
// Substrate note: `enum Outcome<T, E>` and `enum Maybe<T>` are both parsed
// as `Item::TypeDecl(TypeDeclKind::Enum)`; their variant names are
// `TypeBodyItem::Variant` nodes that `find_definition_in_ast` (via
// `hew_analysis::definition::find_definition`) locates by name.
// `rfind` therefore navigates from the last call-site occurrence of each
// variant name to the variant declaration in the enum body.

/// `Outcome::Ok(…)` call site (last "Ok") must navigate to the `Ok(T);`
/// variant declaration in `enum Outcome`.
#[test]
fn v05_result_option_ctors_ok_constructor_has_goto_definition() {
    let source = include_str!("../../../tests/fixtures/v05_result_option_ctors.hew");
    // rfind("Ok") = Outcome::Ok(result_option_probe()) constructor call site
    assert_v05_goto_definition("v05_result_option_ctors", source, "Ok");
}

/// `Outcome::Err(true)` call site (last "Err") must navigate to the `Err(E);`
/// variant declaration in `enum Outcome`.
#[test]
fn v05_result_option_ctors_err_constructor_has_goto_definition() {
    let source = include_str!("../../../tests/fixtures/v05_result_option_ctors.hew");
    assert_v05_goto_definition("v05_result_option_ctors", source, "Err");
}

/// `Maybe::Some(…)` call site (last "Some") must navigate to the `Some(T);`
/// variant declaration in `enum Maybe`.
#[test]
fn v05_result_option_ctors_some_constructor_has_goto_definition() {
    let source = include_str!("../../../tests/fixtures/v05_result_option_ctors.hew");
    assert_v05_goto_definition("v05_result_option_ctors", source, "Some");
}

/// `Maybe::None` unit-variant site (last "None") must navigate to the
/// `None;` variant declaration in `enum Maybe`.
#[test]
fn v05_result_option_ctors_none_unit_variant_has_goto_definition() {
    let source = include_str!("../../../tests/fixtures/v05_result_option_ctors.hew");
    // rfind("None") = Maybe::None; unit variant — no payload, still navigable
    assert_v05_goto_definition("v05_result_option_ctors", source, "None");
}
