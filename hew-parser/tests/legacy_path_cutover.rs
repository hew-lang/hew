use hew_parser::ParseDiagnosticKind;

#[test]
fn legacy_path_separator_fixture_has_its_structured_cutover_diagnostic() {
    assert_cutover_diagnostic(
        include_str!("fixtures/reject/legacy_path_separator.hew"),
        &ParseDiagnosticKind::LegacyPathSeparator,
        "E_PATH_LEGACY_SEPARATOR",
    );
}

#[test]
fn legacy_turbofish_fixture_has_its_structured_cutover_diagnostic() {
    assert_cutover_diagnostic(
        include_str!("fixtures/reject/legacy_turbofish.hew"),
        &ParseDiagnosticKind::LegacyTurbofish,
        "E_LEGACY_TURBOFISH",
    );
}

#[test]
fn legacy_glob_fixture_has_its_structured_cutover_diagnostic() {
    assert_cutover_diagnostic(
        include_str!("fixtures/reject/legacy_glob_import.hew"),
        &ParseDiagnosticKind::ImportGlobRemoved,
        "E_IMPORT_GLOB_REMOVED",
    );
}

fn assert_cutover_diagnostic(
    source: &str,
    expected_kind: &ParseDiagnosticKind,
    expected_code: &str,
) {
    let parsed = hew_parser::parse(source);
    let diagnostic = parsed
        .errors
        .iter()
        .find(|error| &error.kind == expected_kind)
        .unwrap_or_else(|| panic!("missing {expected_code}: {:?}", parsed.errors));

    assert_eq!(diagnostic.kind.as_kind_str(), expected_code);
    assert!(diagnostic.span.start < diagnostic.span.end);
    assert!(diagnostic.span.end <= source.len());
}
