//! Cardinality lock-step between the TOML manifest and the prose matrix.
//!
//! The initial revision of `wasm-capability-manifest.toml` is a transcription
//! of `docs/wasm-capability-matrix.md`. The byte-exact round-trip renderer
//! arrives in a subsequent change; until then the minimum invariant enforced
//! here is that row counts never drift between the two files.
//!
//! WHY row-count-only (shortcut):
//!   A full drift gate walks every field and asserts a byte-exact rerender
//!   equals the committed markdown. The renderer is deferred so the initial
//!   transcription can land quickly; the row-count assertion is strictly
//!   weaker but catches the most common drift class — a new row added to one
//!   file only.
//! WHEN to remove:
//!   When the byte-exact renderer lands, this test becomes redundant with the
//!   full diff check and should be deleted.
//! WHAT the real solution looks like:
//!   `cargo run -p hew-capability-gen -- --check` regenerates every output
//!   into a tempdir and unified-diffs each against the committed copy.

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
