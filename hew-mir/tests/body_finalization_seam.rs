//! One body-finalization seam, held structurally.
//!
//! HIR-to-MIR lowering finishes SEVEN different body kinds: free functions,
//! closure invoke shims, named-fn invoke shims, lambda-actor handler bodies,
//! generator bodies, fork trampolines, and the synthesized machine-`step`
//! dispatch. Each one used to seal its own blocks and then run its own
//! hand-rolled subset of the ownership splice pipeline — several with a comment
//! saying they "mirror `lower_function`'s call site".
//!
//! That shape makes every new ownership pass a seven-way registration problem,
//! and a missed registration is SILENT: the body still compiles, and the absent
//! splice surfaces only as a runtime leak. The divergent-arm selection release
//! was added to `lower_function` alone, and a selection inside a closure or a
//! `gen fn` leaked a whole `Vec` per call until the ramps were routed through
//! the shared seam.
//!
//! `Builder::seal_body_blocks` is only half of finishing a body — it seals the
//! cursor and returns blocks that still owe the pipeline. `finalize_body` is
//! the one place that seals and then runs it. These tests hold the property
//! that makes "add the pass once" true: exactly one call site for the sealing
//! primitive, inside `finalize_body`, and every body ramp reaching it.
//!
//! A source-text check is the right instrument. The property is about WHERE a
//! function may be called from, which no signature expresses — the ramps are
//! child modules of `lower`, so Rust visibility cannot bar them — and it must
//! fail the moment a seventh ramp is written, not when a leak oracle happens to
//! cover its shape.

use std::path::{Path, PathBuf};

/// The sealing primitive that must have exactly one caller.
const SEAL: &str = "seal_body_blocks(";

/// The function that caller must be.
const SEAM: &str = "fn finalize_body(";

fn lower_sources() -> Vec<PathBuf> {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("src/lower");
    let mut sources = Vec::new();
    let mut pending = vec![root];
    while let Some(current) = pending.pop() {
        let entries = std::fs::read_dir(&current)
            .unwrap_or_else(|e| panic!("failed to read {}: {e}", current.display()));
        for entry in entries {
            let path = entry
                .unwrap_or_else(|e| panic!("failed to read entry under {}: {e}", current.display()))
                .path();
            if path.is_dir() {
                pending.push(path);
            } else if path.extension().is_some_and(|extension| extension == "rs") {
                sources.push(path);
            }
        }
    }
    sources.sort();
    sources
}

/// Whether a source line is a comment rather than code.
fn is_comment(line: &str) -> bool {
    let trimmed = line.trim_start();
    trimmed.starts_with("//")
}

/// Every `(file, line_number, line)` that CALLS the sealing primitive. Its
/// declaration is not a call and is excluded.
fn seal_call_sites() -> Vec<(PathBuf, usize, String)> {
    let mut sites = Vec::new();
    for path in lower_sources() {
        let text = std::fs::read_to_string(&path)
            .unwrap_or_else(|e| panic!("failed to read {}: {e}", path.display()));
        for (index, line) in text.lines().enumerate() {
            if is_comment(line) || !line.contains(SEAL) || line.contains(&format!("fn {SEAL}")) {
                continue;
            }
            sites.push((path.clone(), index + 1, line.trim().to_string()));
        }
    }
    sites
}

/// The sealing primitive has exactly one caller, and it is `finalize_body`.
///
/// A new body ramp that seals its own blocks trips this immediately, with the
/// remedy in the failure text: route it through the seam so every ownership
/// pass reaches it.
#[test]
fn seal_body_blocks_is_called_only_from_the_finalization_seam() {
    let sites = seal_call_sites();
    assert_eq!(
        sites.len(),
        1,
        "`{SEAL}` must have exactly ONE caller — `finalize_body`, which seals and then runs the \
         shared ownership splice pipeline. A ramp that seals its own blocks gets a body that \
         looks finished and silently skips every splice (the closure / generator divergent-arm \
         leak). Route the new ramp through `finalize_body` instead. Call sites found: {sites:#?}"
    );

    let (path, line, _) = &sites[0];
    let text = std::fs::read_to_string(path).expect("read seam file");
    let seam_line = text
        .lines()
        .position(|candidate| candidate.contains(SEAM))
        .map_or_else(
            || {
                panic!(
                    "`{SEAM}` must live in the same file as the sole `{SEAL}` call ({})",
                    path.display()
                )
            },
            |index| index + 1,
        );
    assert!(
        *line > seam_line,
        "the sole `{SEAL}` call at {}:{line} must sit inside `{SEAM}` (declared at line \
         {seam_line}), not ahead of it",
        path.display()
    );
}

/// Every body ramp reaches the seam.
///
/// The count is a ratchet in the same spirit as the sealing check. Lowering an
/// EIGHTH body kind is fine — routing it through the seam and updating this
/// number is the whole ask. Dropping below seven without deleting a ramp means
/// a ramp stopped finishing through the seam.
#[test]
fn every_body_ramp_finishes_through_the_seam() {
    let mut call_sites: Vec<(String, usize)> = Vec::new();
    for path in lower_sources() {
        let text = std::fs::read_to_string(&path)
            .unwrap_or_else(|e| panic!("failed to read {}: {e}", path.display()));
        let calls = text
            .lines()
            .filter(|line| {
                !is_comment(line)
                    && line.trim_end().ends_with("finalize_body(")
                    && !line.contains(SEAM)
            })
            .count();
        if calls > 0 {
            let name = path
                .file_name()
                .and_then(|n| n.to_str())
                .unwrap_or_default()
                .to_string();
            call_sites.push((name, calls));
        }
    }
    let total: usize = call_sites.iter().map(|(_, count)| count).sum();
    assert_eq!(
        total, 7,
        "seven body kinds finish through `finalize_body`: the free function, the closure invoke \
         shim, the named-fn invoke shim, the lambda-actor handler body, the generator body, the \
         fork trampoline, and the machine-`step` dispatch. Found {total}: {call_sites:?}"
    );
}
