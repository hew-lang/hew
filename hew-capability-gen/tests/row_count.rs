//! Prose-matrix coverage checks for the typed WASM capability authority.
//!
//! The manifest is the sole authority. The generator owns the complete
//! feature-policy table; the remaining row-count checks cover the surrounding
//! tier and backlog prose tables.

use std::path::{Path, PathBuf};

/// Repo root relative to the crate directory.
fn repo_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("crate dir has a parent")
        .to_path_buf()
}

fn read(path: &Path) -> String {
    std::fs::read_to_string(path).unwrap_or_else(|err| panic!("read {}: {err}", path.display()))
}

/// Counts data rows in a markdown table whose content lives between
/// `start_heading` and `end_heading` (both matched against `line.starts_with`).
/// Subtracts the two header/separator rows.
fn count_markdown_table_rows(md: &str, start_heading: &str, end_heading: &str) -> usize {
    let mut in_section = false;
    let mut pipe_rows = 0usize;
    for line in md.lines() {
        if line.starts_with(start_heading) {
            in_section = true;
            continue;
        }
        if in_section && line.starts_with(end_heading) {
            break;
        }
        if in_section && line.starts_with('|') {
            pipe_rows += 1;
        }
    }
    assert!(
        pipe_rows >= 2,
        "expected at least header+separator rows in section starting {start_heading:?}; \
         found {pipe_rows}"
    );
    pipe_rows - 2
}

/// Returns one markdown table byte-for-byte, including its trailing newline.
fn markdown_table(md: &str, start_heading: &str, end_heading: &str) -> String {
    let mut in_section = false;
    let mut table = String::new();
    for line in md.lines() {
        if line.starts_with(start_heading) {
            in_section = true;
            continue;
        }
        if in_section && line.starts_with(end_heading) {
            break;
        }
        if in_section && line.starts_with('|') {
            table.push_str(line);
            table.push('\n');
        }
    }
    assert!(
        table.lines().count() >= 2,
        "expected a header, separator, and data rows in section starting {start_heading:?}"
    );
    table
}

fn delimited_contents(md: &str, begin_marker: &str, end_marker: &str) -> String {
    let mut in_section = false;
    let mut contents = String::new();
    for line in md.lines() {
        if line == begin_marker {
            assert!(!in_section, "duplicate begin marker {begin_marker:?}");
            in_section = true;
            continue;
        }
        if line == end_marker {
            assert!(in_section, "end marker precedes begin marker");
            return contents;
        }
        if in_section {
            contents.push_str(line);
            contents.push('\n');
        }
    }
    panic!("missing generated section markers {begin_marker:?} / {end_marker:?}");
}

#[test]
fn manifest_row_counts_match_prose_matrix() {
    let root = repo_root();
    let manifest_src = read(&root.join("wasm-capability-manifest.toml"));
    let matrix_src = read(&root.join("docs").join("wasm-capability-matrix.md"));

    let manifest = hew_capability_gen::Manifest::parse(&manifest_src)
        .expect("wasm-capability-manifest.toml parses as a Manifest");

    assert_eq!(manifest.manifest_version, 1, "unexpected manifest_version");

    let md_features = count_markdown_table_rows(
        &matrix_src,
        "## Feature disposition table",
        "## Disposition rationale",
    );
    let md_backlog = count_markdown_table_rows(
        &matrix_src,
        "## WASM-TODO backlog",
        "## Playground capability contract",
    );
    let md_tiers = count_markdown_table_rows(&matrix_src, "## Target tiers", "**Tier 1**");

    assert_eq!(
        manifest.features.len(),
        md_features,
        "TOML feature count ({}) does not match prose feature-disposition rows ({}).\n\
         Every row in docs/wasm-capability-matrix.md must have a [[feature]] entry \
         in wasm-capability-manifest.toml, and vice versa.",
        manifest.features.len(),
        md_features,
    );

    assert_eq!(
        manifest.backlog.len(),
        md_backlog,
        "TOML backlog count ({}) does not match prose WASM-TODO backlog rows ({}).",
        manifest.backlog.len(),
        md_backlog,
    );

    assert_eq!(
        manifest.tiers.len(),
        md_tiers,
        "TOML tier count ({}) does not match prose target-tier rows ({}).",
        manifest.tiers.len(),
        md_tiers,
    );
}

#[test]
fn manifest_feature_policy_table_matches_byte_for_byte() {
    let root = repo_root();
    let manifest_src = read(&root.join("wasm-capability-manifest.toml"));
    let matrix_src = read(&root.join("docs").join("wasm-capability-matrix.md"));
    let manifest = hew_capability_gen::Manifest::parse(&manifest_src)
        .expect("wasm-capability-manifest.toml parses");
    let table = markdown_table(
        &matrix_src,
        "## Feature disposition table",
        "## Disposition rationale",
    );
    assert_eq!(
        table,
        manifest.render_feature_policy_table(),
        "feature policy table drifted from the sole manifest authority"
    );
}

#[test]
fn manifest_playground_wasi_summary_matches_byte_for_byte() {
    const BEGIN: &str = "<!-- BEGIN GENERATED: playground-wasi-capability-summary -->";
    const END: &str = "<!-- END GENERATED: playground-wasi-capability-summary -->";

    let root = repo_root();
    let manifest_src = read(&root.join("wasm-capability-manifest.toml"));
    let playground_manifest = read(&root.join("examples/playground/manifest.json"));
    let matrix_src = read(&root.join("docs/wasm-capability-matrix.md"));
    let manifest = hew_capability_gen::Manifest::parse(&manifest_src)
        .expect("wasm-capability-manifest.toml parses");
    let table = delimited_contents(&matrix_src, BEGIN, END);
    assert_eq!(
        table,
        manifest
            .render_playground_wasi_summary(&playground_manifest)
            .expect("playground authorities agree"),
        "current WASI summary drifted from the typed exclusions and runnable playground truth"
    );
}

#[test]
fn manifest_feature_ids_are_unique() {
    let root = repo_root();
    let manifest_src = read(&root.join("wasm-capability-manifest.toml"));
    let manifest = hew_capability_gen::Manifest::parse(&manifest_src)
        .expect("wasm-capability-manifest.toml parses");

    let mut seen = std::collections::HashSet::new();
    for feature in &manifest.features {
        assert!(
            seen.insert(feature.id.as_str()),
            "duplicate feature id in wasm-capability-manifest.toml: {}",
            feature.id,
        );
    }
}

#[test]
fn manifest_backlog_ids_are_unique() {
    let root = repo_root();
    let manifest_src = read(&root.join("wasm-capability-manifest.toml"));
    let manifest = hew_capability_gen::Manifest::parse(&manifest_src)
        .expect("wasm-capability-manifest.toml parses");

    let mut seen = std::collections::HashSet::new();
    for entry in &manifest.backlog {
        assert!(
            seen.insert(entry.id.as_str()),
            "duplicate backlog id in wasm-capability-manifest.toml: {}",
            entry.id,
        );
    }
}
