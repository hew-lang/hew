//! Counterfactual source gate for the shared ownership classifier boundary.
//!
//! The Rust type system enforces the registry argument once a caller selects
//! the structured API. This test additionally inventories every production
//! call site so a parallel blind API or a default/empty registry cannot become
//! an accidental second authority.

use std::fs;
use std::path::{Path, PathBuf};

const CLASSIFIERS: &[&str] = &[
    "classify_actor_state_fields_with_lifecycle_registry",
    "classify_state_field_with_lifecycle_registry",
    "classify_value_snapshot_plan_with_lifecycle_registry",
];

fn rust_sources(root: &Path, files: &mut Vec<PathBuf>) {
    for entry in fs::read_dir(root).unwrap_or_else(|error| {
        panic!(
            "read production source directory {}: {error}",
            root.display()
        )
    }) {
        let path = entry.expect("read source entry").path();
        if path.is_dir() {
            rust_sources(&path, files);
        } else if path.extension().is_some_and(|extension| extension == "rs")
            && !path
                .file_name()
                .is_some_and(|name| name.to_string_lossy().ends_with("_tests.rs"))
        {
            files.push(path);
        }
    }
}

fn balanced_call(source: &str, open: usize) -> Option<&str> {
    let mut depth = 0_u32;
    for (offset, byte) in source.as_bytes()[open..].iter().enumerate() {
        match byte {
            b'(' => depth += 1,
            b')' => {
                depth = depth.checked_sub(1)?;
                if depth == 0 {
                    return Some(&source[open..=open + offset]);
                }
            }
            _ => {}
        }
    }
    None
}

fn is_definition(source: &str, name_start: usize) -> bool {
    let line_start = source[..name_start]
        .rfind('\n')
        .map_or(0, |index| index + 1);
    source[line_start..name_start].contains("fn ")
}

fn carries_registry_context(call: &str) -> bool {
    call.contains("lifecycle_registry")
        || call.contains("resource_close")
        || call.contains("LifecycleRegistry")
        || call.contains("&registry")
        || call.contains("&lifecycle,")
}

#[test]
fn every_production_classifier_call_carries_lifecycle_registry_context() {
    let workspace = Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("codegen crate lives in workspace root");
    let mut files = Vec::new();
    rust_sources(&workspace.join("hew-mir/src"), &mut files);
    rust_sources(&workspace.join("hew-codegen-rs/src"), &mut files);
    files.sort();

    let mut calls = Vec::new();
    for path in files {
        let source = fs::read_to_string(&path)
            .unwrap_or_else(|error| panic!("read {}: {error}", path.display()));
        let source = source.as_str();
        assert!(
            !source.contains("_with_resource_handles"),
            "{} retains the pre-registry classifier API spelling",
            path.display()
        );
        for classifier in CLASSIFIERS {
            let mut cursor = 0;
            while let Some(relative) = source[cursor..].find(classifier) {
                let start = cursor + relative;
                cursor = start + classifier.len();
                if is_definition(source, start) {
                    continue;
                }
                let whitespace = source[cursor..].len() - source[cursor..].trim_start().len();
                let open = cursor + whitespace;
                if source.as_bytes().get(open) != Some(&b'(') {
                    continue;
                }
                let call = balanced_call(source, open)
                    .unwrap_or_else(|| panic!("unbalanced classifier call in {}", path.display()));
                let line = source[..start]
                    .bytes()
                    .filter(|byte| *byte == b'\n')
                    .count()
                    + 1;
                assert!(
                    carries_registry_context(call),
                    "{}:{line}: `{classifier}` call omits lifecycle registry context: {call}",
                    path.display()
                );
                calls.push(format!("{}:{line}:{classifier}", path.display()));
            }
        }
    }

    assert!(
        calls.len() >= 45,
        "classifier call-site inventory unexpectedly shrank to {} entries; update the gate only after auditing the removed production paths:\n{}",
        calls.len(),
        calls.join("\n")
    );
    assert!(
        !carries_registry_context("(&ty, &records)"),
        "counterfactual missing-registry call must fail the structural predicate"
    );
}
