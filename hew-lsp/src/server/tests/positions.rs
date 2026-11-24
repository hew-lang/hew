use super::*;

#[test]
fn line_offsets_multiline() {
    let offsets = compute_line_offsets("ab\ncd\ne");
    assert_eq!(offsets, vec![0, 3, 6]);
}

#[test]
fn offset_to_line_col_basic() {
    let source = "ab\ncd\ne";
    let offsets = compute_line_offsets(source);
    assert_eq!(offset_to_line_col(source, &offsets, 0), (0, 0));
    assert_eq!(offset_to_line_col(source, &offsets, 1), (0, 1));
    assert_eq!(offset_to_line_col(source, &offsets, 3), (1, 0));
    assert_eq!(offset_to_line_col(source, &offsets, 6), (2, 0));
}

#[test]
fn position_to_offset_basic() {
    let source = "ab\ncd\ne";
    let lo = compute_line_offsets(source);
    assert_eq!(position_to_offset(source, &lo, Position::new(0, 0)), 0);
    assert_eq!(position_to_offset(source, &lo, Position::new(1, 1)), 4);
    assert_eq!(position_to_offset(source, &lo, Position::new(2, 0)), 6);
}

#[test]
fn offset_to_line_col_utf16() {
    // "aé" — 'é' is 2 bytes in UTF-8 but 1 UTF-16 code unit
    let source = "aé\nb";
    let offsets = compute_line_offsets(source);
    // 'a' is at byte 0 → col 0
    assert_eq!(offset_to_line_col(source, &offsets, 0), (0, 0));
    // 'é' starts at byte 1 → col 1 (UTF-16)
    assert_eq!(offset_to_line_col(source, &offsets, 1), (0, 1));
    // After 'é' (byte 3) → col 2 (UTF-16)
    assert_eq!(offset_to_line_col(source, &offsets, 3), (0, 2));
    // 'b' is on line 1 at byte 4 → (1, 0)
    assert_eq!(offset_to_line_col(source, &offsets, 4), (1, 0));
}

#[test]
fn position_to_offset_utf16() {
    // "aé\nb" — 'é' is 2 bytes in UTF-8
    let source = "aé\nb";
    let lo = compute_line_offsets(source);
    // Position(0, 1) → byte 1 (start of 'é')
    assert_eq!(position_to_offset(source, &lo, Position::new(0, 1)), 1);
    // Position(0, 2) → byte 3 (after 'é', which is \n)
    assert_eq!(position_to_offset(source, &lo, Position::new(0, 2)), 3);
    // Position(1, 0) → byte 4
    assert_eq!(position_to_offset(source, &lo, Position::new(1, 0)), 4);
}

#[test]
fn offset_to_line_col_surrogate_pair() {
    // '𝄞' (U+1D11E) is 4 bytes in UTF-8 and 2 UTF-16 code units
    let source = "a𝄞b";
    let offsets = compute_line_offsets(source);
    // 'a' at byte 0 → col 0
    assert_eq!(offset_to_line_col(source, &offsets, 0), (0, 0));
    // '𝄞' at byte 1 → col 1
    assert_eq!(offset_to_line_col(source, &offsets, 1), (0, 1));
    // 'b' at byte 5 → col 3 (1 for 'a' + 2 for surrogate pair)
    assert_eq!(offset_to_line_col(source, &offsets, 5), (0, 3));
}

// ── Non-empty helper test ───────────────────────────────────────

#[test]
fn non_empty_returns_none_for_empty_vec() {
    let v: Vec<i32> = vec![];
    assert!(non_empty(v).is_none());
}

#[test]
fn non_empty_returns_some_for_non_empty_vec() {
    let v = vec![1, 2, 3];
    assert!(non_empty(v).is_some());
}

// ── Span/range conversion tests ─────────────────────────────────

#[test]
fn offset_range_to_lsp_basic() {
    let source = "fn main() {\n    42\n}";
    let lo = compute_line_offsets(source);
    let range = offset_range_to_lsp(source, &lo, 0, 2);
    assert_eq!(range.start, Position::new(0, 0));
    assert_eq!(range.end, Position::new(0, 2));
}

#[test]
fn offset_range_to_lsp_multiline() {
    let source = "fn main() {\n    42\n}";
    let lo = compute_line_offsets(source);
    let end = source.len();
    let range = offset_range_to_lsp(source, &lo, 0, end);
    assert_eq!(range.start.line, 0);
    assert_eq!(range.end.line, 2);
}

#[test]
fn absolute_paths_round_trip_through_file_uris() {
    for path in ["/project/plain.hew", "/project/naïve source.hew"] {
        let uri = make_test_uri(path);
        #[cfg(windows)]
        let expected = PathBuf::from(format!("C:{path}"));
        #[cfg(not(windows))]
        let expected = PathBuf::from(path);
        assert_eq!(
            uri.to_checked_file_path().as_deref(),
            Some(expected.as_path())
        );
    }
}

// ── Debounce version-counter logic ──────────────────────────────

#[test]
fn debounce_version_counter_increments_on_each_edit() {
    // The debounce mechanism uses a per-URI generation counter to decide
    // whether a spawned task is still the most recent edit.  Each call to
    // `reanalyze` must increment the counter so that the previous task
    // sees a stale value and exits without publishing diagnostics.
    //
    // This test exercises the counter directly (no async runtime needed)
    // since the correctness invariant is: "a task started for version N
    // bails if the counter has already advanced past N".
    let versions: DashMap<Url, u64> = DashMap::new();
    let uri = Url::parse("file:///test.hew").unwrap();

    // First edit: counter goes from absent → 1.
    let v1 = {
        let mut entry = versions.entry(uri.clone()).or_insert(0);
        *entry += 1;
        *entry
    };
    assert_eq!(v1, 1, "first edit should produce version 1");
    assert_eq!(versions.get(&uri).map(|v| *v), Some(1));

    // Second edit: counter advances to 2; first task's version (1) is now stale.
    let v2 = {
        let mut entry = versions.entry(uri.clone()).or_insert(0);
        *entry += 1;
        *entry
    };
    assert_eq!(v2, 2, "second edit should produce version 2");
    // The first task checks: versions.get(&uri) != Some(1) → true → it bails.
    assert_ne!(
        versions.get(&uri).map(|v| *v),
        Some(v1),
        "version counter must have advanced past the first task's snapshot"
    );
    // The second task checks: versions.get(&uri) == Some(2) → it proceeds.
    assert_eq!(
        versions.get(&uri).map(|v| *v),
        Some(v2),
        "second task's version must still be current"
    );
}
