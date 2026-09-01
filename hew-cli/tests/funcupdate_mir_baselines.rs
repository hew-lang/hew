//! #2648 interface pin, MIR half — committed golden elaborated-MIR baselines
//! for the funcupdate/reassign consumers.
//!
//! The Coarse freshness summary feeds two UAF gates: the destructive-
//! funcupdate base gate (`expr_is_materialized_owner`) and the #2420
//! reassign-overwrite gate (`reassign_rhs_may_alias_binding`). The frozen-
//! reference differential (`hew-mir` `coarse_verdict_differential`) proves the
//! boolean verdicts byte-identical; this harness proves the EMITTED elaborated
//! MIR (including drop plans) of the fixtures that exercise those consumers is
//! identical to baselines generated at the revision recorded in the manifest
//! (`hew compile --dump-mir elab`) under only four recorded normalizations:
//! function order is sorted by signature because map iteration is
//! nondeterministic; `BindingId`s are renumbered per function; statement
//! `SiteId`s share a source-site key with ordinary leading decision `site=sN`
//! labels while exact `ParamBoundary` rows use a synthetic key; and trailing
//! function separators are normalized. Every other intra-function byte must
//! match exactly.
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

use std::collections::{HashMap, HashSet};
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
             `{baseline}` (revision recorded in manifest.tsv). A funcupdate/reassign consumer's \
             lowering or drop plan CHANGED — this is the Coarse-drift signal the boolean \
             differential cannot see. If the change is intended and reviewed, regenerate \
             the baseline per the manifest header. First differing line (after \
             fn-order normalization):\n{}",
            first_diff(&expected_norm, &live_norm)
        );
    }
}

/// Split a dump into per-function chunks (a chunk starts at a column-0
/// `fn ` line), apply the three recorded normalizations (see the manifest
/// header), and rejoin: (1) sort chunks by signature line because dump
/// FUNCTION ORDER is nondeterministic (map iteration); (2) renumber
/// `BindingId(n)` values, plus the syntactic statement `SiteId(n)` tokens and
/// leading `decisions` `site=sN` labels within each chunk in their respective
/// rendered orders because they depend on module iteration order; and (3) normalize the
/// blank separators after each function because
/// the dump emitter's final empty line otherwise follows whichever function map
/// iteration emits last.
/// Within-function content order is preserved, so a reordered drop inside a
/// chunk remains detectable; everything else must match byte-for-byte —
/// opcodes, drop plans, local structure.
fn normalize_fn_order(dump: &str) -> String {
    let dump = dump.trim_end_matches('\n');
    let mut chunks: Vec<String> = Vec::new();
    let mut current = String::new();
    for line in dump.lines() {
        if line.starts_with("fn ") && !current.is_empty() {
            let chunk = std::mem::take(&mut current);
            chunks.push(format!("{}\n", chunk.trim_end_matches('\n')));
        }
        current.push_str(line);
        current.push('\n');
    }
    if !current.is_empty() {
        chunks.push(format!("{}\n", current.trim_end_matches('\n')));
    }
    let mut chunks: Vec<String> = chunks
        .iter()
        .map(|c| canonicalize_function_site_ids(&canonicalize_ids(c, "BindingId(")))
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

/// Normalize one function's site labels through a shared map.  The two dump
/// spellings deliberately have narrow syntax contracts: statement fields are
/// `site=SiteId(N)` or `site=Some(SiteId(N))`, while a decision fact starts
/// exactly with `    site=sN `.  In particular, do not treat a lookalike in a
/// quoted `why` field (or any other semantic payload) as an allocator label.
/// Ordinary expression decisions share their statement source-site key.  The
/// one rendered synthetic exception is a `ParamBoundary` fact, whose site is
/// an ABI parameter slot rather than a statement source site.
fn canonicalize_function_site_ids(chunk: &str) -> String {
    let mut out = String::with_capacity(chunk.len());
    let mut sites = HashMap::new();

    for line in chunk.split_inclusive('\n') {
        let label = if line.starts_with("    site=s") {
            leading_decision_site_label(line).map(|(range, raw)| {
                let key = if is_param_boundary_decision(line) {
                    SiteKey::SyntheticParamBoundary(raw.to_owned())
                } else {
                    SiteKey::SourceSite(raw.to_owned())
                };
                (range, key)
            })
        } else if line.starts_with("    ") {
            statement_site_label(line)
                .map(|(range, raw)| (range, SiteKey::SourceSite(raw.to_owned())))
        } else {
            None
        };

        if let Some((range, key)) = label {
            let next = sites.len();
            let canonical = *sites.entry(key).or_insert(next);
            out.push_str(&line[..range.start]);
            out.push('#');
            out.push_str(&canonical.to_string());
            out.push_str(&line[range.end..]);
        } else {
            out.push_str(line);
        }
    }
    out
}

#[derive(Hash, Eq, PartialEq)]
enum SiteKey {
    SourceSite(String),
    SyntheticParamBoundary(String),
}

/// The structured discriminator is a decision field before the `why` payload.
/// A lookalike inside quoted text must not turn an ordinary expression source
/// into a synthetic parameter-boundary site.
fn is_param_boundary_decision(line: &str) -> bool {
    let fields = line.split_once(" why=").map_or(line, |(fields, _)| fields);
    fields.contains(" strategy=ParamBoundary(")
}

/// Return the digits from only a leading decision-row label: `    site=sN `.
fn leading_decision_site_label(line: &str) -> Option<(std::ops::Range<usize>, &str)> {
    const PREFIX: &str = "    site=s";
    line.strip_prefix(PREFIX)?;
    let digits_start = PREFIX.len();
    let digits_len = line[digits_start..]
        .bytes()
        .take_while(u8::is_ascii_digit)
        .count();
    let digits_end = digits_start + digits_len;
    (digits_len > 0 && line.as_bytes().get(digits_end) == Some(&b' '))
        .then(|| (digits_start..digits_end, &line[digits_start..digits_end]))
}

/// Return the digits from a statement's actual `site` field, never a quoted
/// semantic field.  `Some` is checked first because it is the longer spelling.
fn statement_site_label(line: &str) -> Option<(std::ops::Range<usize>, &str)> {
    const OPTIONAL: &str = "site=Some(SiteId(";
    const DIRECT: &str = "site=SiteId(";
    let mut quoted = false;
    let mut index = 0;
    while index < line.len() {
        match line.as_bytes()[index] {
            b'\\' if quoted => index += 2,
            b'\"' => {
                quoted = !quoted;
                index += 1;
            }
            _ if !quoted => {
                let (prefix, suffix) = if line[index..].starts_with(OPTIONAL) {
                    (OPTIONAL, "))")
                } else if line[index..].starts_with(DIRECT) {
                    (DIRECT, ")")
                } else {
                    index += 1;
                    continue;
                };
                let digits_start = index + prefix.len();
                let digits_len = line[digits_start..]
                    .bytes()
                    .take_while(u8::is_ascii_digit)
                    .count();
                let digits_end = digits_start + digits_len;
                if digits_len > 0 && line[digits_end..].starts_with(suffix) {
                    return Some((digits_start..digits_end, &line[digits_start..digits_end]));
                }
                index += prefix.len();
            }
            _ => index += 1,
        }
    }
    None
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

#[test]
fn function_separator_and_site_normalization_preserve_intra_function_drift() {
    let baseline = "fn probe -> ()\n  statements:\n    bind BindingId(1) first site=SiteId(10) ty=i64\n    return site=Some(SiteId(20)) ty=string\n  decisions:\n    site=s10 ty=i64 value_class=BitCopy intent=Read strategy=BorrowRead why=\"origin site=s777\"\n    site=s20 ty=string value_class=CowValue intent=Consume strategy=Move why=\"return site=SiteId(777)\"\n  blocks:\n    id=0 kind=normal\n      successor: bb1\n  drop_plans:\n    return[bb1] ->\n      drop _0 ty=string authority=owner\n";
    assert_eq!(
        normalize_fn_order(baseline),
        normalize_fn_order(&format!("{baseline}\n")),
        "the optional final empty dump line is non-semantic"
    );
    assert_eq!(
        normalize_fn_order(&format!("{baseline}\nfn other -> ()\n")),
        normalize_fn_order(&format!("fn other -> ()\n\n{baseline}\n")),
        "the final empty line must not follow a function through order normalization"
    );
    assert_ne!(
        normalize_fn_order(baseline),
        normalize_fn_order(&baseline.replacen("  statements:", "\n  statements:", 1)),
        "an interior blank remains MIR drift"
    );
    assert_eq!(
        normalize_fn_order(baseline),
        normalize_fn_order(
            &baseline
                .replace("SiteId(10)", "SiteId(101)")
                .replace("SiteId(20)", "SiteId(202)")
                .replace("site=s10 ", "site=s101 ")
                .replace("site=s20 ", "site=s202 "),
        ),
        "the one per-function normalizer accepts a consistent whole-chunk renumber"
    );
    let synthetic_boundary = "fn boundary -> ()\n  statements:\n    eval site=SiteId(0) ty=()\n  decisions:\n    site=s0 ty=i64 value_class=BitCopy intent=Unknown strategy=ParamBoundary(ParamBoundaryFact { param_index: 0 }) why=\"parameter boundary\"\n";
    assert_eq!(
        normalize_fn_order(synthetic_boundary),
        normalize_fn_order(&synthetic_boundary.replace("SiteId(0)", "SiteId(100)")),
        "only the exact ParamBoundary strategy gives the ABI parameter slot its synthetic key"
    );
    let quoted_discriminator = "fn quoted -> ()\n  statements:\n    eval site=SiteId(10) ty=()\n  decisions:\n    site=s10 ty=() value_class=BitCopy intent=Read strategy=BorrowRead why=\"strategy=ParamBoundary( is prose\"\n";
    assert_ne!(
        normalize_fn_order(quoted_discriminator),
        normalize_fn_order(&quoted_discriminator.replace("SiteId(10)", "SiteId(30)")),
        "a ParamBoundary lookalike in why remains an ordinary shared source site"
    );
    assert_ne!(
        normalize_fn_order(baseline),
        normalize_fn_order(&baseline.replace("site=SiteId(10)", "site=SiteId(30)")),
        "a statement-only source-site mismatch remains visible across the decision section"
    );
    assert_ne!(
        normalize_fn_order(baseline),
        normalize_fn_order(&baseline.replace("site=s10 ty=i64", "site=s30 ty=i64")),
        "a decision-only source-site mismatch remains visible across the statement section"
    );
    assert_ne!(
        normalize_fn_order(baseline),
        normalize_fn_order(&baseline.replace("site=s10 ty=i64", "site=s10 ty=bool")),
        "decision types remain baseline-visible"
    );
    assert_ne!(
        normalize_fn_order(baseline),
        normalize_fn_order(&baseline.replacen("intent=Read", "intent=Consume", 1)),
        "decision intents remain baseline-visible"
    );
    assert_ne!(
        normalize_fn_order(baseline),
        normalize_fn_order(
            &baseline.replace("why=\"origin site=s777\"", "why=\"origin site=s888\"")
        ),
        "a lookalike site label in a why payload is not normalized"
    );
    assert_ne!(
        normalize_fn_order(baseline),
        normalize_fn_order(&baseline.replace("kind=normal", "kind=cleanup")),
        "block kind remains baseline-visible"
    );
    assert_ne!(
        normalize_fn_order(baseline),
        normalize_fn_order(&baseline.replace("successor: bb1", "successor: none")),
        "block successor remains baseline-visible"
    );
    assert_ne!(
        normalize_fn_order(baseline),
        normalize_fn_order(&baseline.replace("authority=owner", "authority=borrow")),
        "drop-plan rows remain baseline-visible"
    );
    assert_ne!(
        normalize_fn_order(baseline),
        normalize_fn_order(
            &baseline
                .replace(
                    "site=s10 ty=i64 value_class=BitCopy intent=Read strategy=BorrowRead why=\"origin site=s777\"",
                    "site=s10 ty=string value_class=CowValue intent=Consume strategy=Move why=\"return site=SiteId(777)\"",
                )
                .replace(
                    "site=s20 ty=string value_class=CowValue intent=Consume strategy=Move why=\"return site=SiteId(777)\"",
                    "site=s20 ty=i64 value_class=BitCopy intent=Read strategy=BorrowRead why=\"origin site=s777\"",
                ),
        ),
        "renderer-sorted decision rows expose a two-site statement/decision association permutation"
    );
}
