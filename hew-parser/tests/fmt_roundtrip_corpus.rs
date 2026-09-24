//! Corpus-wide formatter fidelity oracle.
//!
//! Every `.hew` file in the workspace that parses is formatted with its
//! comments and must pass [`fidelity::check`]: the output parses to the same
//! program, and every token and comment keeps its order and attachment. The
//! output must also be a fixed point of the formatter.
//!
//! `HEW_FMT_FIDELITY_ROOTS` adds directories outside the workspace, such as
//! sibling example and ecosystem checkouts, as a platform path list.

use hew_parser::fmt::{fidelity, format_source};
use hew_parser::{parse, Severity};
use std::path::{Path, PathBuf};
use walkdir::WalkDir;

/// A walk that finds fewer files than this has lost a root.
const MIN_WORKSPACE_FILES: usize = 2000;

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
    for dir in &roots {
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
            let shown = path.strip_prefix(dir).unwrap_or(path).display().to_string();
            let source = match std::fs::read_to_string(path) {
                Ok(source) => source,
                Err(e) => {
                    failures.push(format!("{shown}: cannot read: {e}"));
                    continue;
                }
            };
            match reprint(&source) {
                Outcome::Faithful => checked += 1,
                Outcome::Unparsed => unparsed += 1,
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
}
