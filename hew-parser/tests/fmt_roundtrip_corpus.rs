//! Corpus-wide `parse(format(parse(src))) == parse(src)` property test.
//!
//! For every `.hew` file under the repo's real-world source roots, this test
//! parses, formats back to source, reparses, and asserts the two parsed
//! programs are AST-equivalent (ignoring spans).
//!
//! This replaces fixture-by-fixture formatter coverage with a structural
//! guarantee: a new AST variant, a missing formatter arm, or a formatter drop
//! of a field all surface immediately as a mismatch against a real file.
//!
//! Sister file: `fmt_roundtrip_known_fails.txt` — a reason-stamped allowlist
//! of files exempt from the property. The test enforces a hard budget on that
//! list (`MAX_KNOWN_FAILS`) so the allowlist cannot become a dumping ground.

use hew_parser::ast_eq::program_eq_ignoring_spans;
use hew_parser::{fmt, parse};
use std::collections::BTreeSet;
use std::path::{Path, PathBuf};
use walkdir::WalkDir;

/// Corpus roots, relative to the workspace root (the directory containing the
/// top-level `Cargo.toml`). This test walks each recursively for `.hew` files.
const CORPUS_ROOTS: &[&str] = &[
    "tests",
    "examples",
    "std",
    "hew-codegen/tests/examples",
    "hew-lsp/tests",
    "hew-parser/tests",
    "hew-cli/tests",
    "hew-lib/tests",
];

/// Minimum corpus size. If the walk produces fewer than this many files, the
/// test fails — catches accidental root deletion or misrouted relative paths.
const MIN_CORPUS_FILES: usize = 1000;

/// Hard cap on the known-fails allowlist. If this is exceeded, the list has
/// become a bug-drawer instead of a tracked exemption set; fail the test.
const MAX_KNOWN_FAILS: usize = 20;

fn workspace_root() -> PathBuf {
    // CARGO_MANIFEST_DIR points at hew-parser/; the workspace root is its parent.
    let crate_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    crate_dir
        .parent()
        .expect("hew-parser has a workspace parent")
        .to_path_buf()
}

fn known_fails_path() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("fmt_roundtrip_known_fails.txt")
}

/// Parse the known-fails allowlist. Returns the set of relative paths that
/// should be skipped. Panics if any non-comment line lacks a reason string,
/// or if the total entry count exceeds `MAX_KNOWN_FAILS`.
fn load_known_fails() -> BTreeSet<String> {
    let path = known_fails_path();
    let contents =
        std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("read {}: {e}", path.display()));

    let mut out = BTreeSet::new();
    for (lineno, raw) in contents.lines().enumerate() {
        let line = raw.trim();
        if line.is_empty() || line.starts_with('#') {
            continue;
        }
        let (rel, reason) = line.split_once(':').unwrap_or_else(|| {
            panic!(
                "{}:{}: expected '<path>: <reason>', got: {raw:?}",
                path.display(),
                lineno + 1
            )
        });
        let rel = rel.trim();
        let reason = reason.trim();
        assert!(
            !rel.is_empty(),
            "{}:{}: empty path before ':'",
            path.display(),
            lineno + 1
        );
        assert!(
            !reason.is_empty(),
            "{}:{}: empty reason after ':' — every entry must name the blocking invariant",
            path.display(),
            lineno + 1
        );
        out.insert(rel.to_string());
    }

    assert!(
        out.len() <= MAX_KNOWN_FAILS,
        "known-fails allowlist has {} entries, budget is {}: shrink it before adding more",
        out.len(),
        MAX_KNOWN_FAILS
    );
    out
}

/// Walk the corpus roots and yield all `.hew` files as paths relative to the
/// workspace root (slash-separated, stable across platforms).
fn corpus_files() -> Vec<(PathBuf, String)> {
    let root = workspace_root();
    let mut files = Vec::new();
    for sub in CORPUS_ROOTS {
        let abs_root = root.join(sub);
        if !abs_root.exists() {
            continue;
        }
        for entry in WalkDir::new(&abs_root).into_iter().filter_map(Result::ok) {
            let path = entry.path();
            if path.extension().is_some_and(|e| e == "hew") {
                let rel = path
                    .strip_prefix(&root)
                    .expect("corpus path rooted at workspace")
                    .to_string_lossy()
                    .replace('\\', "/");
                files.push((path.to_path_buf(), rel));
            }
        }
    }
    files.sort_by(|a, b| a.1.cmp(&b.1));
    files
}

#[test]
fn every_hew_file_roundtrips_through_formatter() {
    let known_fails = load_known_fails();
    let files = corpus_files();

    assert!(
        files.len() >= MIN_CORPUS_FILES,
        "corpus walk found only {} files — expected ≥ {}; a root may be missing \
         or a relative path may be wrong (walked roots: {:?})",
        files.len(),
        MIN_CORPUS_FILES,
        CORPUS_ROOTS
    );

    let mut reparse_errors: Vec<String> = Vec::new();
    let mut ast_mismatches: Vec<String> = Vec::new();
    let mut skipped_intentionally: usize = 0;
    let mut skipped_known_fail: usize = 0;

    for (abs, rel) in &files {
        if known_fails.contains(rel) {
            skipped_known_fail += 1;
            continue;
        }

        let src = match std::fs::read_to_string(abs) {
            Ok(s) => s,
            Err(e) => {
                reparse_errors.push(format!("read {rel}: {e}"));
                continue;
            }
        };

        // Some corpus files are intentional parse-reject fixtures. A program
        // that fails to parse cleanly is not a valid input to the round-trip
        // property — skip it.
        let parsed1 = parse(&src);
        if !parsed1.errors.is_empty() {
            skipped_intentionally += 1;
            continue;
        }

        let formatted = fmt::format_program(&parsed1.program);

        let parsed2 = parse(&formatted);
        if !parsed2.errors.is_empty() {
            let first = parsed2
                .errors
                .first()
                .map_or_else(|| "<no message>".to_string(), |e| format!("{e:?}"));
            reparse_errors.push(format!("{rel}: reformatted output fails to parse: {first}"));
            continue;
        }

        if !program_eq_ignoring_spans(&parsed1.program, &parsed2.program) {
            ast_mismatches.push(rel.clone());
        }
    }

    let ok = reparse_errors.is_empty() && ast_mismatches.is_empty();
    if !ok {
        use std::fmt::Write as _;
        let mut msg = String::new();
        let _ = writeln!(
            &mut msg,
            "corpus round-trip failed across {} file(s) \
             (reparse errors: {}, AST mismatches: {})",
            reparse_errors.len() + ast_mismatches.len(),
            reparse_errors.len(),
            ast_mismatches.len()
        );
        if !reparse_errors.is_empty() {
            msg.push_str("\n-- reparse errors --\n");
            for e in &reparse_errors {
                msg.push_str(e);
                msg.push('\n');
            }
        }
        if !ast_mismatches.is_empty() {
            msg.push_str("\n-- AST mismatches --\n");
            for e in &ast_mismatches {
                msg.push_str(e);
                msg.push('\n');
            }
        }
        panic!("{msg}");
    }

    eprintln!(
        "fmt roundtrip corpus: {} files walked, {} OK, {} skipped (parse-reject fixtures), \
         {} on known-fails allowlist (budget {})",
        files.len(),
        files.len() - skipped_intentionally - skipped_known_fail,
        skipped_intentionally,
        skipped_known_fail,
        MAX_KNOWN_FAILS,
    );
}

/// Regression guard: the allowlist parser rejects entries without a reason.
/// This test exercises the loader on the committed file — if the committed
/// file ever contains a malformed entry, this test surfaces it.
#[test]
fn known_fails_parses_cleanly() {
    let _ = load_known_fails();
}

/// Regression guard: make sure all known-fails paths actually exist in the
/// repo. A stale entry (file renamed or deleted) silently shrinks coverage.
#[test]
fn known_fails_entries_all_exist() {
    let root = workspace_root();
    let known = load_known_fails();
    let mut missing = Vec::new();
    for rel in &known {
        if !root.join(Path::new(rel)).exists() {
            missing.push(rel.clone());
        }
    }
    assert!(
        missing.is_empty(),
        "known-fails allowlist references {} missing file(s): {missing:?}",
        missing.len()
    );
}
