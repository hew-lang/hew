//! Line-count ratchet for the `src/lower/` concern modules.
//!
//! HIR-to-MIR lowering is split by language concern so independent feature
//! work does not reconverge on a single god-module. This ratchet fails closed
//! if any concern file grows past the hard ceiling. When it trips, carve a
//! coherent concern into a sibling module and prove the move with the
//! `scripts/ll-corpus.sh` byte-identity oracle; do not raise the ceiling.

use std::path::Path;

/// Hard ceiling for every Rust source file directly under `src/lower/`.
/// Ratchet this down as concern modules are split further.
const CEILING: usize = 10_000;

const _: () = assert!(CEILING <= 10_000);

#[test]
fn lower_modules_stay_under_line_ceiling() {
    let lower_dir = Path::new(env!("CARGO_MANIFEST_DIR")).join("src/lower");
    let mut sources = std::fs::read_dir(&lower_dir)
        .unwrap_or_else(|e| panic!("failed to read {}: {e}", lower_dir.display()))
        .map(|entry| {
            entry
                .expect("failed to read lower module directory entry")
                .path()
        })
        .filter(|path| path.extension().is_some_and(|extension| extension == "rs"))
        .collect::<Vec<_>>();
    sources.sort();

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
