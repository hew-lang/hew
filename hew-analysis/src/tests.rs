//! Shared discovery of source test declarations.

use hew_parser::ast::{Item, Program};
use std::ops::Range;
use std::path::{Path, PathBuf};

/// A test declaration and its source selection facts.
#[derive(Debug, Clone)]
pub struct TestDeclaration {
    pub name: String,
    pub span: Range<usize>,
    pub item_ordinal: usize,
    pub ignored: bool,
    pub should_panic: bool,
    pub serial: bool,
    pub real_time: bool,
}

/// Return every top-level `#[test]` function in source order.
#[must_use]
pub fn discover_tests(program: &Program) -> Vec<TestDeclaration> {
    program
        .items
        .iter()
        .enumerate()
        .filter_map(|(item_ordinal, (item, span))| {
            let Item::Function(function) = item else {
                return None;
            };
            let has = |name: &str| {
                function
                    .attributes
                    .iter()
                    .any(|attribute| attribute.name == name)
            };
            has("test").then(|| TestDeclaration {
                name: function.name.to_string(),
                span: span.clone(),
                item_ordinal,
                ignored: has("ignore"),
                should_panic: has("should_panic"),
                serial: has("serial"),
                real_time: has("real_time"),
            })
        })
        .collect()
}

/// Find Hew sources beneath a selected file or directory in stable order.
///
/// # Errors
///
/// Returns the first directory read error.
pub fn source_files(path: &Path) -> std::io::Result<Vec<PathBuf>> {
    let mut files = Vec::new();
    collect_sources(path, &mut files)?;
    files.sort();
    Ok(files)
}

fn collect_sources(path: &Path, files: &mut Vec<PathBuf>) -> std::io::Result<()> {
    if path.is_file() {
        if path
            .extension()
            .is_some_and(|extension| extension.eq_ignore_ascii_case("hew"))
        {
            files.push(path.to_path_buf());
        }
        return Ok(());
    }
    for entry in std::fs::read_dir(path)? {
        let entry = entry?;
        let name = entry.file_name();
        let name = name.to_string_lossy();
        if name.starts_with('.') || matches!(name.as_ref(), "target" | "node_modules") {
            continue;
        }
        let kind = entry.file_type()?;
        if kind.is_dir() {
            collect_sources(&entry.path(), files)?;
        } else if kind.is_file() {
            collect_sources(&entry.path(), files)?;
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn discovers_only_test_functions_with_source_selection() {
        let source =
            "fn helper() {}\n#[test]\n#[ignore]\nfn a() {}\n#[test]\n#[real_time]\nfn b() {}\n";
        let parsed = hew_parser::parse(source);
        let tests = discover_tests(&parsed.program);
        assert_eq!(tests.len(), 2);
        assert_eq!(tests[0].name, "a");
        assert!(tests[0].ignored);
        assert!(source[tests[0].span.clone()].contains("fn a() {}"));
        assert_eq!(tests[1].name, "b");
        assert!(tests[1].real_time);
    }
}
