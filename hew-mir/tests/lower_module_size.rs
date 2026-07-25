//! Line-count ratchet for the `src/lower/` concern modules.
//!
//! HIR-to-MIR lowering is split by language concern so independent feature
//! work does not reconverge on a single god-module. This ratchet fails closed
//! if any concern file grows past the hard ceiling. When it trips, carve a
//! coherent concern into a sibling module and prove the move with the
//! `scripts/ll-corpus.sh` byte-identity oracle; do not raise the ceiling.
//!
//! The walk is recursive: a concern carved into `src/lower/<concern>/` is a
//! sibling module one directory deeper, not an escape hatch from the ceiling.

use std::path::{Path, PathBuf};

/// Hard ceiling for every Rust source file anywhere under `src/lower/`.
/// Ratchet this down as concern modules are split further.
const CEILING: usize = 10_000;

const _: () = assert!(CEILING <= 10_000);

/// Collect every `.rs` file under `dir`, descending into subdirectories so no
/// file can duck the ceiling by living one directory deeper.
fn collect_rust_sources(dir: &Path) -> Vec<PathBuf> {
    let mut sources = Vec::new();
    let mut pending = vec![dir.to_path_buf()];
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

fn lower_dir() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("src/lower")
}

#[test]
fn lower_modules_stay_under_line_ceiling() {
    let lower_dir = lower_dir();
    let sources = collect_rust_sources(&lower_dir);

    assert!(
        !sources.is_empty(),
        "{} contains no Rust sources",
        lower_dir.display()
    );
    for path in sources {
        let source = std::fs::read_to_string(&path)
            .unwrap_or_else(|e| panic!("failed to read {}: {e}", path.display()));
        let line_count = source.lines().count();
        assert!(
            line_count <= CEILING,
            "{} is {line_count} lines, over the {CEILING}-line ceiling. \
             Carve a coherent lowering concern into a sibling module; do not \
             raise the ceiling. The `scripts/ll-corpus.sh` byte-identity oracle \
             proves a pure-move carve emits identical IR.",
            path.display()
        );
    }
}

/// Pins the recursion itself. A non-recursive `read_dir` would silently stop
/// covering nested concern modules, which is the exact failure mode this
/// ratchet exists to prevent, so the walk is exercised against a hermetic
/// fixture tree rather than trusted to whatever the repo happens to contain.
#[test]
fn source_walk_descends_into_subdirectories() {
    let root = std::env::temp_dir().join(format!(
        "hew-mir-lower-ratchet-walk-{}-{:?}",
        std::process::id(),
        std::thread::current().id()
    ));
    let _ = std::fs::remove_dir_all(&root);
    let nested = root.join("concern").join("deeper");
    std::fs::create_dir_all(&nested).expect("failed to create fixture tree");

    let top = root.join("top.rs");
    let mid = root.join("concern").join("mid.rs");
    let deep = nested.join("deep.rs");
    let ignored = root.join("concern").join("notes.txt");
    for path in [&top, &mid, &deep] {
        std::fs::write(path, "fn main() {}\n").expect("failed to write fixture source");
    }
    std::fs::write(&ignored, "not rust\n").expect("failed to write fixture non-source");

    let found = collect_rust_sources(&root);
    std::fs::remove_dir_all(&root).expect("failed to clean fixture tree");

    let mut expected = vec![top, mid, deep.clone()];
    expected.sort();
    assert_eq!(
        found, expected,
        "the walk must yield every nested Rust source and nothing else"
    );
    assert!(
        found.contains(&deep),
        "the walk must reach {} — a non-recursive read_dir would miss it",
        deep.display()
    );
}
