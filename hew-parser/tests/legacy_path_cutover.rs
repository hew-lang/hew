use hew_parser::ParseDiagnosticKind;

#[test]
fn legacy_path_separator_fixture_has_exact_diagnostic_and_fix() {
    assert_cutover_diagnostic(
        include_str!("fixtures/reject/legacy_path_separator.hew"),
        &ParseDiagnosticKind::LegacyPathSeparator,
        "E_PATH_LEGACY_SEPARATOR",
        "Option.Some(1)",
    );
}

#[test]
fn legacy_turbofish_fixture_has_exact_diagnostic_and_fix() {
    assert_cutover_diagnostic(
        include_str!("fixtures/reject/legacy_turbofish.hew"),
        &ParseDiagnosticKind::LegacyTurbofish,
        "E_LEGACY_TURBOFISH",
        "Vec<i64>.new()",
    );
}

#[test]
fn legacy_glob_fixture_has_exact_diagnostic_and_fix() {
    assert_cutover_diagnostic(
        include_str!("fixtures/reject/legacy_glob_import.hew"),
        &ParseDiagnosticKind::ImportGlobRemoved,
        "E_IMPORT_GLOB_REMOVED",
        "import std.io.{ Name };",
    );
}

fn assert_cutover_diagnostic(
    source: &str,
    expected_kind: &ParseDiagnosticKind,
    expected_code: &str,
    expected_fix: &str,
) {
    let parsed = hew_parser::parse(source);
    let diagnostic = parsed
        .errors
        .iter()
        .find(|error| &error.kind == expected_kind)
        .unwrap_or_else(|| panic!("missing {expected_code}: {:?}", parsed.errors));

    assert_eq!(diagnostic.kind.as_kind_str(), expected_code);
    assert!(
        diagnostic.message.contains(expected_fix),
        "diagnostic did not include migrated spelling `{expected_fix}`: {}",
        diagnostic.message
    );
    assert!(
        diagnostic
            .hint
            .as_deref()
            .is_some_and(|hint| hint.contains(expected_fix)),
        "hint did not include migrated spelling `{expected_fix}`: {:?}",
        diagnostic.hint
    );
}
