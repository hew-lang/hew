//! Discover `#[test]` functions in Hew source files.

use hew_parser::ast::{Item, Program};
use hew_parser::ParseError;
#[cfg(test)]
use hew_parser::Severity;

/// A discovered test case.
#[derive(Debug, Clone)]
pub struct TestCase {
    /// Test function name.
    pub name: String,
    /// Source file path.
    pub file: String,
    /// Whether the test has `#[ignore]`.
    pub ignored: bool,
    /// Whether the test has `#[should_panic]`.
    pub should_panic: bool,
}

/// The result of inspecting a single source file for tests.
#[derive(Debug)]
pub struct DiscoveredTestFile {
    /// Source file path.
    pub path: String,
    /// Source contents.
    pub source: String,
    /// Discovered `#[test]` functions.
    pub tests: Vec<TestCase>,
    /// Parser diagnostics found while reading the file.
    pub parse_errors: Vec<ParseError>,
}

impl DiscoveredTestFile {
    /// Whether the file has any parser errors that should fail the test run.
    #[must_use]
    #[cfg(test)]
    pub fn has_parse_errors(&self) -> bool {
        self.parse_errors
            .iter()
            .any(|error| error.severity == Severity::Error)
    }
}

/// Walk a parsed program's AST and collect all `#[test]` functions.
#[must_use]
pub fn discover_tests(program: &Program, file: &str) -> Vec<TestCase> {
    let mut tests = Vec::new();
    for (item, _span) in &program.items {
        if let Item::Function(f) = item {
            let is_test = f.attributes.iter().any(|a| a.name == "test");
            if is_test {
                let ignored = f.attributes.iter().any(|a| a.name == "ignore");
                let should_panic = f.attributes.iter().any(|a| a.name == "should_panic");
                tests.push(TestCase {
                    name: f.name.clone(),
                    file: file.to_string(),
                    ignored,
                    should_panic,
                });
            }
        }
    }
    tests
}

/// Parse a source file and discover tests.
///
/// # Errors
///
/// Returns an error string if the file cannot be read.
pub fn discover_tests_in_file(path: &str) -> Result<DiscoveredTestFile, String> {
    let source = std::fs::read_to_string(path).map_err(|e| format!("cannot read {path}: {e}"))?;
    let result = hew_parser::parse(&source);
    Ok(DiscoveredTestFile {
        path: path.to_string(),
        tests: discover_tests(&result.program, path),
        source,
        parse_errors: result.errors,
    })
}

/// Recursively discover test files in a directory.
///
/// A file is considered a test file if it ends with `_test.hew` or is inside
/// a `tests/` directory and ends with `.hew`.
///
/// # Errors
///
/// Returns an error string if directory traversal fails.
pub fn discover_test_files(dir: &str) -> Result<Vec<String>, String> {
    let mut files = Vec::new();
    collect_test_files(std::path::Path::new(dir), &mut files)
        .map_err(|e| format!("cannot scan {dir}: {e}"))?;
    files.sort();
    Ok(files)
}

fn collect_test_files(dir: &std::path::Path, out: &mut Vec<String>) -> Result<(), std::io::Error> {
    if !dir.is_dir() {
        // Single file.
        if let Some(s) = dir.to_str() {
            if std::path::Path::new(s)
                .extension()
                .is_some_and(|ext| ext.eq_ignore_ascii_case("hew"))
            {
                out.push(s.to_string());
            }
        }
        return Ok(());
    }
    for entry in std::fs::read_dir(dir)? {
        let entry = entry?;
        let path = entry.path();
        if path.is_dir() {
            collect_test_files(&path, out)?;
        } else if let Some(name) = path.file_name().and_then(|n| n.to_str()) {
            let is_hew = std::path::Path::new(name)
                .extension()
                .is_some_and(|ext| ext.eq_ignore_ascii_case("hew"));
            if name.ends_with("_test.hew") || is_hew {
                if let Some(s) = path.to_str() {
                    // Accept files ending with _test.hew, or .hew files inside tests/ dirs.
                    let in_tests_dir = path.components().any(|c| c.as_os_str() == "tests");
                    if name.ends_with("_test.hew") || in_tests_dir {
                        out.push(s.to_string());
                    }
                }
            }
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::tempdir;

    #[test]
    fn discover_test_functions() {
        let source = r"
fn helper() -> i32 { 42 }

#[test]
fn test_basic() {
    assert(true);
}

#[test]
#[ignore]
fn test_ignored() {
    assert(false);
}

#[test]
#[should_panic]
fn test_panic() {
    assert(false);
}
";
        let result = hew_parser::parse(source);
        let tests = discover_tests(&result.program, "test.hew");
        assert_eq!(tests.len(), 3);

        assert_eq!(tests[0].name, "test_basic");
        assert!(!tests[0].ignored);
        assert!(!tests[0].should_panic);

        assert_eq!(tests[1].name, "test_ignored");
        assert!(tests[1].ignored);

        assert_eq!(tests[2].name, "test_panic");
        assert!(tests[2].should_panic);
    }

    #[test]
    fn no_tests_in_plain_program() {
        let source = "fn main() -> i32 { 0 }";
        let result = hew_parser::parse(source);
        let tests = discover_tests(&result.program, "main.hew");
        assert!(tests.is_empty());
    }

    #[test]
    fn discover_test_files_respects_layout_rules() {
        let dir = tempdir().unwrap();
        std::fs::write(
            dir.path().join("alpha_test.hew"),
            "#[test]\nfn alpha() { assert(true); }\n",
        )
        .unwrap();
        std::fs::create_dir_all(dir.path().join("nested")).unwrap();
        std::fs::write(
            dir.path().join("nested").join("beta_test.hew"),
            "#[test]\nfn beta() { assert(true); }\n",
        )
        .unwrap();
        std::fs::create_dir_all(dir.path().join("tests")).unwrap();
        std::fs::write(
            dir.path().join("tests").join("gamma.hew"),
            "#[test]\nfn gamma() { assert(true); }\n",
        )
        .unwrap();
        std::fs::write(
            dir.path().join("tests").join("helper.hew"),
            "fn helper() {}\n",
        )
        .unwrap();
        std::fs::write(dir.path().join("plain.hew"), "fn helper() {}\n").unwrap();

        let mut files = discover_test_files(dir.path().to_str().unwrap()).unwrap();
        files.sort();

        assert_eq!(
            files,
            vec![
                dir.path().join("alpha_test.hew").display().to_string(),
                dir.path()
                    .join("nested")
                    .join("beta_test.hew")
                    .display()
                    .to_string(),
                dir.path()
                    .join("tests")
                    .join("gamma.hew")
                    .display()
                    .to_string(),
                dir.path()
                    .join("tests")
                    .join("helper.hew")
                    .display()
                    .to_string(),
            ]
        );
    }

    #[test]
    fn discover_tests_in_file_preserves_parse_errors() {
        let dir = tempdir().unwrap();
        let file = dir.path().join("broken_test.hew");
        std::fs::write(&file, "#[test]\nfn broken( {\n    assert(true);\n}\n").unwrap();

        let discovered = discover_tests_in_file(file.to_str().unwrap()).unwrap();

        assert!(discovered.tests.is_empty());
        assert!(discovered.has_parse_errors());
        assert!(!discovered.parse_errors.is_empty());
    }
}
