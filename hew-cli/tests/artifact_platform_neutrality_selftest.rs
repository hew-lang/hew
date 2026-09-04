//! Registry selftest for platform-neutral byte-compare test artifacts.
//!
//! Every artifact class a byte-compare harness reads (MIR baselines,
//! checked-MIR goldens, and manifest TSVs) MUST
//! be pinned `text eol=lf` in `.gitattributes` and MUST contain no CRLF or
//! UTF-8 BOM bytes on disk. Without this, a Windows checkout (CRLF
//! `core.autocrlf`) silently corrupts the artifact and a byte-compare test
//! fails ONLY on Windows CI — the failure surfaces platforms and cycles
//! away from the change that caused it.
//!
//! This test walks the KNOWN artifact roots — the directories/files a
//! byte-compare or line-parsed harness actually reads today — rather than
//! the whole repository, so it stays fast and does not flag unrelated
//! source files. Walking whole directories (not a fixed extension list)
//! means a brand new file dropped into an existing artifact root — any
//! extension, any future class — is checked automatically: forgetting its
//! `.gitattributes` pin fails this test at commit time, not on Windows CI.
//! A genuinely new artifact root (a new top-level corpus directory outside
//! the list below) still needs a one-line addition to `ARTIFACT_ROOTS`.
//!
//! Kept deliberately as ONE test per the artifact-neutrality mandate: a
//! second selftest would just be a second place to forget to update.

use std::path::{Path, PathBuf};
use std::process::Command;

/// Directories walked recursively; every file inside must be LF-pinned and
/// CRLF/BOM-free.
///
/// - `tests/mir-baselines`: committed `--dump-mir elab` baselines
///   (`*.mir`) and their `manifest.tsv` (funcupdate/reassign interface pin,
///   `hew-cli/tests/funcupdate_mir_baselines.rs`).
/// - `hew-cli/tests/fixtures`: `.hew` compile fixtures and package fixtures.
/// - `examples/v05/checked-mir`: checked-MIR execution transcripts
///   (`*.expected`, `scripts/checked-mir-corpus.sh`).
const ARTIFACT_ROOTS: &[&str] = &[
    "tests/mir-baselines",
    "hew-cli/tests/fixtures",
    "examples/v05/checked-mir",
];

/// Individual files outside the roots above, compared/parsed byte-for-byte.
const ARTIFACT_FILES: &[&str] = &[
    "scripts/structural-authority-inventory.tsv",
    "wasm-capability-manifest.toml",
    "hew-types/src/wasm_capabilities_generated.rs",
    "examples/playground/wasm-capabilities.json",
];

fn repo_root() -> &'static Path {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("hew-cli crate should live under the repo root")
}

fn collect_files(dir: &Path, out: &mut Vec<PathBuf>) {
    let entries = std::fs::read_dir(dir)
        .unwrap_or_else(|e| panic!("artifact root {} unreadable: {e}", dir.display()));
    for entry in entries {
        let entry = entry.expect("dir entry readable");
        let path = entry.path();
        let file_type = entry.file_type().expect("file type readable");
        if file_type.is_dir() {
            collect_files(&path, out);
        } else if file_type.is_file() {
            out.push(path);
        }
    }
}

/// `git check-attr eol -- <path>` reports the effective `.gitattributes`
/// `eol` value for a committed path. Shelling out (rather than
/// hand-parsing `.gitattributes`) is the ground truth Git itself applies
/// on checkout — the same resolution a Windows clone gets.
fn eol_attr(root: &Path, rel: &Path) -> String {
    let output = Command::new("git")
        .current_dir(root)
        .arg("check-attr")
        .arg("eol")
        .arg("--")
        .arg(rel)
        .output()
        .expect("git check-attr spawns");
    assert!(
        output.status.success(),
        "git check-attr failed for {}: {}",
        rel.display(),
        String::from_utf8_lossy(&output.stderr)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    // Format: "<path>: eol: <value>"
    stdout
        .trim()
        .rsplit(':')
        .next()
        .unwrap_or("")
        .trim()
        .to_string()
}

#[test]
fn committed_artifacts_are_lf_pinned_and_byte_clean() {
    let root = repo_root();
    let mut files = Vec::new();
    for rel_root in ARTIFACT_ROOTS {
        let dir = root.join(rel_root);
        assert!(
            dir.is_dir(),
            "artifact root `{rel_root}` no longer exists — update ARTIFACT_ROOTS in \
             hew-cli/tests/artifact_platform_neutrality_selftest.rs (a moved/renamed \
             corpus must not silently drop out of this gate)"
        );
        collect_files(&dir, &mut files);
    }
    for rel_file in ARTIFACT_FILES {
        let path = root.join(rel_file);
        assert!(
            path.is_file(),
            "artifact file `{rel_file}` no longer exists — update ARTIFACT_FILES in \
             hew-cli/tests/artifact_platform_neutrality_selftest.rs"
        );
        files.push(path);
    }
    assert!(
        !files.is_empty(),
        "zero artifact files discovered — the corpus roots collapsed or the selftest's \
         own registry is wrong; either way this test must not pass silently"
    );

    let mut unpinned = Vec::new();
    let mut dirty = Vec::new();
    for path in &files {
        let rel = path
            .strip_prefix(root)
            .expect("artifact file lives under repo root");

        if eol_attr(root, rel) != "lf" {
            unpinned.push(rel.display().to_string());
        }

        let bytes = std::fs::read(path)
            .unwrap_or_else(|e| panic!("artifact file {} unreadable: {e}", rel.display()));
        let has_crlf = bytes.windows(2).any(|w| w == b"\r\n");
        let has_bom = bytes.starts_with(&[0xEF, 0xBB, 0xBF]);
        if has_crlf || has_bom {
            let what = match (has_crlf, has_bom) {
                (true, true) => "CRLF and a UTF-8 BOM",
                (true, false) => "CRLF",
                (false, true) => "a UTF-8 BOM",
                (false, false) => unreachable!(),
            };
            dirty.push(format!("{} ({what})", rel.display()));
        }
    }

    assert!(
        unpinned.is_empty() && dirty.is_empty(),
        "artifact-platform-neutrality gate failed (rule: committed test artifacts are \
         platform-neutral by construction).\n\
         Missing `.gitattributes` `text eol=lf` pin ({} file(s)): {unpinned:#?}\n\
         CRLF/BOM bytes on disk ({} file(s)): {dirty:#?}\n\
         Add the missing pattern to `.gitattributes` (or, for a stray CRLF/BOM, \
         renormalize the file: `git add --renormalize -- <path>`) before committing.",
        unpinned.len(),
        dirty.len(),
    );
}
