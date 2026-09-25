//! Corpus-wide formatter fidelity oracle.
//!
//! Every `.hew` file in the workspace that parses is formatted with its
//! comments and must pass [`fidelity::check`]: the output parses to the same
//! program, and every token and comment keeps its order and attachment. The
//! output must also be a fixed point of the formatter.
//!
//! `HEW_FMT_FIDELITY_ROOTS` adds directories outside the workspace, such as
//! sibling example and ecosystem checkouts, as a platform path list; each
//! named root must exist.
//!
//! A workspace file that does not parse is not formatter input. A path under
//! a `reject/` directory is exempt outright: those fixtures exist to prove a
//! refusal and are never meant to parse. Every other unparsable file is
//! listed in `fmt_unparsed_files.txt` (the ledger's `Expect` enum has no
//! `skip`/`unparsed` kind yet, so these rows cannot move into
//! `tests/expected-failures.tsv`); the list is checked for exact equality
//! against the observed unparsable files, so a new unparsable file or a
//! listed file that now parses fails the test.

use hew_parser::fmt::{fidelity, format_source};
use hew_parser::{parse, Severity};
use std::collections::BTreeSet;
use std::path::{Path, PathBuf};
use walkdir::WalkDir;

/// A walk that finds fewer files than this has lost a root.
const MIN_WORKSPACE_FILES: usize = 2000;

/// A path under a `reject/` directory is a deliberate parse-refusal fixture,
/// not formatter input; it needs no ledger row.
fn is_reject_fixture(shown: &str) -> bool {
    shown.split('/').any(|segment| segment == "reject")
}

/// Workspace files allowed to stay unparsable, outside `reject/` directories.
fn unparsed_list_path() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("fmt_unparsed_files.txt")
}

fn fmt_ledger_ids() -> BTreeSet<String> {
    std::fs::read_to_string(unparsed_list_path())
        .expect("read the unparsed-file list")
        .lines()
        .map(str::trim)
        .filter(|line| !line.is_empty() && !line.starts_with('#'))
        .map(str::to_string)
        .collect()
}

fn workspace_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("hew-parser has a workspace parent")
        .to_path_buf()
}

/// Every `.hew` file under `root`, skipping build output and hidden
/// directories.
fn hew_files(root: &Path) -> Vec<PathBuf> {
    let mut files: Vec<PathBuf> = WalkDir::new(root)
        .into_iter()
        .filter_entry(|entry| {
            let name = entry.file_name().to_string_lossy();
            entry.depth() == 0
                || !(name.starts_with('.') || name == "target" || name == "node_modules")
        })
        .filter_map(Result::ok)
        .map(walkdir::DirEntry::into_path)
        .filter(|path| path.extension().is_some_and(|e| e == "hew"))
        .collect();
    files.sort();
    files
}

enum Outcome {
    Faithful,
    /// Not formatter input: the file does not parse.
    Unparsed,
    Failed(String),
}

fn reprint(source: &str) -> Outcome {
    let parsed = parse(source);
    if parsed
        .errors
        .iter()
        .any(|e| matches!(e.severity, Severity::Error))
    {
        return Outcome::Unparsed;
    }
    let formatted = format_source(source, &parsed.program);
    if let Err(e) = fidelity::check(source, &formatted) {
        return Outcome::Failed(e.to_string());
    }
    if format_source(&formatted, &parse(&formatted).program) != formatted {
        return Outcome::Failed("a second format changed the output".to_string());
    }
    Outcome::Faithful
}

#[test]
fn every_hew_file_reprints_faithfully() {
    let root = workspace_root();
    let mut roots = vec![root.clone()];
    if let Some(extra) = std::env::var_os("HEW_FMT_FIDELITY_ROOTS") {
        roots.extend(std::env::split_paths(&extra).filter(|p| !p.as_os_str().is_empty()));
    }

    let mut failures = Vec::new();
    let mut workspace_unparsed = BTreeSet::new();
    for dir in &roots {
        assert!(
            dir.is_dir(),
            "fidelity root {} does not exist",
            dir.display()
        );
        let files = hew_files(dir);
        if dir == &root {
            assert!(
                files.len() >= MIN_WORKSPACE_FILES,
                "found only {} workspace .hew files, expected at least {MIN_WORKSPACE_FILES}",
                files.len()
            );
        } else {
            assert!(!files.is_empty(), "{} holds no .hew files", dir.display());
        }
        let (mut checked, mut unparsed) = (0usize, 0usize);
        for path in &files {
            let shown = path
                .strip_prefix(dir)
                .unwrap_or(path)
                .to_string_lossy()
                .replace('\\', "/");
            let source = match std::fs::read_to_string(path) {
                Ok(source) => source,
                Err(e) => {
                    failures.push(format!("{shown}: cannot read: {e}"));
                    continue;
                }
            };
            match reprint(&source) {
                Outcome::Faithful => checked += 1,
                Outcome::Unparsed => {
                    unparsed += 1;
                    if dir == &root && !is_reject_fixture(&shown) {
                        workspace_unparsed.insert(shown);
                    }
                }
                Outcome::Failed(why) => failures.push(format!("{shown}: {why}")),
            }
        }
        eprintln!(
            "fmt fidelity: {}: {} files, {checked} reprinted faithfully, {unparsed} do not parse",
            dir.display(),
            files.len(),
        );
    }

    assert!(
        failures.is_empty(),
        "{} file(s) do not reprint faithfully:\n{}",
        failures.len(),
        failures.join("\n")
    );

    let listed = fmt_ledger_ids();
    let new: Vec<_> = workspace_unparsed.difference(&listed).collect();
    let gone: Vec<_> = listed.difference(&workspace_unparsed).collect();
    assert!(
        new.is_empty() && gone.is_empty(),
        "fmt_unparsed_files.txt is out of date.\n\
         New files that do not parse (list them, or a reject/ path needs none): {new:?}\n\
         Listed files that now parse or no longer exist (remove them): {gone:?}"
    );
}
