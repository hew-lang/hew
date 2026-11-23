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
    let source = include_str!("fixtures/reject/legacy_glob_import.hew");
    let diagnostic = assert_cutover_diagnostic(
        source,
        &ParseDiagnosticKind::ImportGlobRemoved,
        "E_IMPORT_GLOB_REMOVED",
    );
    assert!(
        diagnostic.message.contains("import std.io.{ Name };"),
        "expected glob-before-separator rewrite in message: {}",
        diagnostic.message
    );
    assert!(
        diagnostic
            .hint
            .as_deref()
            .is_some_and(|hint| hint.contains("import std.io.{ Name };")),
        "expected glob-before-separator rewrite in hint: {:?}",
        diagnostic.hint
    );
}

fn assert_cutover_diagnostic(
    source: &str,
    expected_kind: &ParseDiagnosticKind,
    expected_code: &str,
) -> hew_parser::ParseError {
    let parsed = hew_parser::parse(source);
    let diagnostic = parsed
        .errors
        .iter()
        .find(|error| &error.kind == expected_kind)
        .unwrap_or_else(|| panic!("missing {expected_code}: {:?}", parsed.errors));

    assert_eq!(diagnostic.kind.as_kind_str(), expected_code);
    assert!(diagnostic.span.start < diagnostic.span.end);
    assert!(diagnostic.span.end <= source.len());
    diagnostic.clone()
}
