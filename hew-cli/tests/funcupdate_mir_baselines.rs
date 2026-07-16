//! #2648 interface pin, MIR half — committed golden elaborated-MIR baselines
//! for the funcupdate/reassign consumers.
//!
//! The Coarse freshness summary feeds two UAF gates: the destructive-
//! funcupdate base gate (`expr_is_materialized_owner`) and the #2420
//! reassign-overwrite gate (`reassign_rhs_may_alias_binding`). The frozen-
//! reference differential (`hew-mir` `coarse_verdict_differential`) proves the
//! boolean verdicts byte-identical; this harness proves the EMITTED elaborated
//! MIR (including drop plans) of the fixtures that exercise those consumers is
//! identical to baselines generated at the branch base (`3683c2ac5`,
//! `hew compile --dump-mir elab`) under ONE normalization rule: the dump's
//! FUNCTION ORDER is nondeterministic (map iteration), so both sides are
//! split into per-function chunks and sorted by signature line before the
//! byte comparison; intra-function text must match exactly.
//!
//! Fail-closed manifest discipline:
//! - every manifest row's fixture AND baseline file must exist (missing →
//!   FAIL, never a silent skip);
//! - every `.elab.mir` file in the baseline directory must be mapped by the
//!   manifest (an orphan baseline → FAIL — no silent corpus shrinkage);
//! - any byte difference between the live dump and the baseline → FAIL. The
//!   baseline is regenerated only by an explicit, reviewed step (see the
//!   manifest header), never by this test.

mod support;

use std::collections::HashSet;
use std::path::Path;
use std::process::Command;

use support::{hew_binary, repo_root};

const BASELINE_DIR: &str = "tests/mir-baselines/funcupdate-reassign";

fn manifest_rows(root: &Path) -> Vec<(String, String)> {
    let manifest = root.join(BASELINE_DIR).join("manifest.tsv");
    let text = std::fs::read_to_string(&manifest)
        .unwrap_or_else(|e| panic!("manifest missing at {}: {e}", manifest.display()));
    let mut rows = Vec::new();
    for line in text.lines() {
        let line = line.trim();
        if line.is_empty() || line.starts_with('#') {
            continue;
        }
        let mut parts = line.split('\t');
        let fixture = parts.next().expect("manifest row has a fixture column");
        let baseline = parts
            .next()
            .unwrap_or_else(|| panic!("manifest row `{line}` is missing the baseline column"));
        rows.push((fixture.to_string(), baseline.to_string()));
    }
    assert!(
        !rows.is_empty(),
        "manifest has zero rows — corpus collapsed"
    );
    rows
}

#[test]
fn funcupdate_reassign_elab_mir_matches_committed_baselines() {
    support::require_codegen();
    let root = repo_root();
    let rows = manifest_rows(root);

    // Stale/missing detection half 1: every baseline file on disk is mapped.
    let mapped: HashSet<&str> = rows.iter().map(|(_, b)| b.as_str()).collect();
    for entry in std::fs::read_dir(root.join(BASELINE_DIR)).expect("baseline dir readable") {
        let entry = entry.expect("baseline dir entry readable");
        let name = entry.file_name();
        let name = name.to_str().expect("utf-8 baseline file name");
        if name.ends_with(".elab.mir") {
            assert!(
                mapped.contains(name),
                "orphan baseline `{name}` is not mapped by manifest.tsv — remove it or add \
                 its fixture row (no silent corpus shrinkage)"
            );
        }
    }

    for (fixture, baseline) in rows {
        let fixture_path = root.join(&fixture);
        // Stale/missing detection half 2: both sides of the row must exist.
        assert!(
            fixture_path.exists(),
            "manifest fixture `{fixture}` does not exist — the corpus input was moved or \
             deleted without a manifest update"
        );
        let baseline_path = root.join(BASELINE_DIR).join(&baseline);
        let expected = std::fs::read_to_string(&baseline_path).unwrap_or_else(|e| {
            panic!(
                "committed baseline `{}` unreadable: {e}",
                baseline_path.display()
            )
        });

        let output = Command::new(hew_binary())
            .current_dir(root)
            .args(["compile", "--dump-mir", "elab"])
            .arg(&fixture)
            .output()
            .expect("hew compile spawns");
        assert!(
            output.status.success(),
            "`hew compile --dump-mir elab {fixture}` failed (exit {:?}):\n{}",
            output.status.code(),
            String::from_utf8_lossy(&output.stderr)
        );
        let live = String::from_utf8(output.stdout).expect("utf-8 MIR dump");

        let expected_norm = normalize_fn_order(&expected);
        let live_norm = normalize_fn_order(&live);
        assert!(
            live_norm == expected_norm,
            "elaborated MIR for `{fixture}` diverged from the committed baseline \
             `{baseline}` (generated at 3683c2ac5). A funcupdate/reassign consumer's \
             lowering or drop plan CHANGED — this is the Coarse-drift signal the boolean \
             differential cannot see. If the change is intended and reviewed, regenerate \
             the baseline per the manifest header. First differing line (after \
             fn-order normalization):\n{}",
            first_diff(&expected_norm, &live_norm)
        );
    }
}

/// Split a dump into per-function chunks (a chunk starts at a column-0
/// `fn ` line), apply the two recorded normalizations (see the manifest
/// header), and rejoin: (1) sort chunks by signature line because dump
/// FUNCTION ORDER is nondeterministic (map iteration); (2) renumber
/// `BindingId(n)` / `SiteId(n)` values within each chunk in first-occurrence
/// order because they depend on module iteration order. Within-chunk content
/// order is preserved, so a reordered drop inside a chunk remains detectable;
/// everything else must match byte-for-byte — opcodes, drop plans, local
/// structure.
fn normalize_fn_order(dump: &str) -> String {
    let mut chunks: Vec<String> = Vec::new();
    let mut current = String::new();
    for line in dump.lines() {
        if line.starts_with("fn ") && !current.is_empty() {
            chunks.push(std::mem::take(&mut current));
        }
        current.push_str(line);
        current.push('\n');
    }
    if !current.is_empty() {
        chunks.push(current);
    }
    let mut chunks: Vec<String> = chunks
        .iter()
        .map(|c| canonicalize_ids(&canonicalize_ids(c, "BindingId("), "SiteId("))
        .collect();
    chunks.sort();
    chunks.concat()
}

/// Renumber every `<marker>N)` occurrence in first-occurrence order
/// (`<marker>#0)`, `<marker>#1)`, …) so run-varying id assignment cannot
/// produce a false diff while any structural change still does.
fn canonicalize_ids(chunk: &str, marker: &str) -> String {
    let mut out = String::with_capacity(chunk.len());
    let mut map: std::collections::HashMap<String, usize> = std::collections::HashMap::new();
    let mut rest = chunk;
    while let Some(pos) = rest.find(marker) {
        let start = pos + marker.len();
        let digits: String = rest[start..]
            .chars()
            .take_while(char::is_ascii_digit)
            .collect();
        if digits.is_empty() {
            out.push_str(&rest[..start]);
            rest = &rest[start..];
            continue;
        }
        let next = map.len();
        let id = *map.entry(digits.clone()).or_insert(next);
        out.push_str(&rest[..pos]);
        out.push_str(marker);
        out.push('#');
        out.push_str(&id.to_string());
        rest = &rest[start + digits.len()..];
    }
    out.push_str(rest);
    out
}

fn first_diff(expected: &str, live: &str) -> String {
    for (i, (e, l)) in expected.lines().zip(live.lines()).enumerate() {
        if e != l {
            return format!("line {}:\n  baseline: {e}\n  live:     {l}", i + 1);
        }
    }
    format!(
        "line-count mismatch: baseline {} lines, live {} lines",
        expected.lines().count(),
        live.lines().count()
    )
}
