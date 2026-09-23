//! Every `extern "C"` function the standard library declares carries exactly
//! one FFI ownership row (#3228). The generated contract table is the sole
//! ownership authority for std externs, so a declaration without a row would
//! leave its parameters and result undecided at every call.

use std::path::{Path, PathBuf};

use hew_parser::ast::Item;
use hew_types::ffi_contracts::extern_ownership_contract;

fn hew_sources(dir: &Path, out: &mut Vec<PathBuf>) {
    for entry in std::fs::read_dir(dir).expect("read std directory") {
        let path = entry.expect("std directory entry").path();
        if path.is_dir() {
            hew_sources(&path, out);
        } else if path.extension().is_some_and(|ext| ext == "hew") {
            out.push(path);
        }
    }
}

fn declared_std_externs() -> Vec<(String, PathBuf)> {
    let std_root = Path::new(env!("CARGO_MANIFEST_DIR")).join("../std");
    let mut files = Vec::new();
    hew_sources(&std_root, &mut files);
    files.sort();
    let mut externs = Vec::new();
    for file in files {
        let source = std::fs::read_to_string(&file).expect("read std source");
        let parsed = hew_parser::parse(&source);
        for (item, _) in &parsed.program.items {
            if let Item::ExternBlock(block) = item {
                if block.abi != "C" {
                    continue;
                }
                for function in &block.functions {
                    externs.push((function.name.clone(), file.clone()));
                }
            }
        }
    }
    externs
}

#[test]
fn every_std_extern_has_an_ownership_row() {
    let externs = declared_std_externs();
    assert!(
        !externs.is_empty(),
        "the std walk found no extern declarations"
    );
    let missing: Vec<String> = externs
        .iter()
        .filter(|(symbol, _)| !extern_ownership_contract(symbol).is_contract())
        .map(|(symbol, file)| format!("{symbol} ({})", file.display()))
        .collect();
    assert!(
        missing.is_empty(),
        "std extern declarations with no ownership row in \
         scripts/runtime-export-classification.toml:\n{}",
        missing.join("\n")
    );
}
