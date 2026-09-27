//! Discover `#[test]` functions in Hew source files.

use hew_parser::ast::Program;
use hew_parser::ParseError;
#[cfg(test)]
use hew_parser::Severity;
use hew_types::{DeclarationKind, DeclarationOccurrence};

/// A discovered test case.
#[derive(Debug, Clone)]
pub struct TestCase {
    /// Test function name.
    pub name: String,
    /// Source file path.
    pub file: String,
    /// Exact source declaration selected for process entry.
    pub occurrence: DeclarationOccurrence,
    /// Canonical production peer matched to this test root.
    pub companion: Option<String>,
    /// Whether the test has `#[ignore]`.
    pub ignored: bool,
    /// Optional reason attached to `#[ignore]`.
    pub ignore_reason: Option<String>,
    /// Whether the test has `#[should_panic]`.
    pub should_panic: bool,
    /// Optional text that the expected fault must contain.
    pub should_panic_message: Option<String>,
    /// Per-test timeout override from `#[timeout(D)]`.
    pub timeout_ns: Option<i64>,
    /// Whether the test must run exclusively with other serial tests.
    pub serial: bool,
    /// Where the test runs: the deterministic driver, or host threads and the
    /// host clock for a `#[real_time]` test.
    pub clock: TestClock,
    /// Original source identity and output contract for an executable doc fence.
    pub doc: Option<DocTest>,
}

/// Metadata retained while a doc fence runs from a generated source file.
#[derive(Debug, Clone)]
pub struct DocTest {
    pub identity: String,
    pub selector: String,
    pub origin: String,
    pub expected_stdout: Option<String>,
    pub no_run: bool,
    pub parse_error: Option<String>,
}

/// The runtime a test executes on.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TestClock {
    /// The single-thread driver with a seeded schedule and a virtual clock.
    Deterministic,
    /// The threaded scheduler and the host clock (`#[real_time]`).
    RealTime,
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
    let companion = matched_production_peer(std::path::Path::new(file));
    for declaration in hew_analysis::test_discovery::discover_tests(program) {
        tests.push(TestCase {
            name: declaration.name,
            file: file.to_string(),
            occurrence: DeclarationOccurrence::new_with_synthetic_ordinal(
                None,
                &declaration.span,
                declaration.item_ordinal,
                DeclarationKind::Function,
                0,
            ),
            companion: companion.clone(),
            ignored: declaration.ignored,
            ignore_reason: declaration.ignore_reason,
            should_panic: declaration.should_panic,
            should_panic_message: declaration.should_panic_message,
            timeout_ns: declaration.timeout_ns,
            serial: declaration.serial,
            clock: if declaration.real_time {
                TestClock::RealTime
            } else {
                TestClock::Deterministic
            },
            doc: None,
        });
    }
    tests
}

fn matched_production_peer(path: &std::path::Path) -> Option<String> {
    let stem = path.file_stem()?.to_str()?.strip_suffix("_test")?;
    let peer = path.with_file_name(format!("{stem}.hew"));
    peer.is_file()
        .then_some(peer)
        .and_then(|peer| peer.canonicalize().ok())
        .map(|peer| peer.display().to_string())
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

/// Recursively discover Hew sources so inline tests are never missed.
///
/// # Errors
///
/// Returns an error string if directory traversal fails.
pub fn discover_test_files(dir: &str) -> Result<Vec<String>, String> {
    hew_analysis::test_discovery::source_files(std::path::Path::new(dir))
        .map(|files| {
            files
                .iter()
                .map(|path| path.display().to_string())
                .collect()
        })
        .map_err(|error| format!("cannot scan {dir}: {error}"))
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
        assert!(!tests[0].serial);

        assert_eq!(tests[1].name, "test_ignored");
        assert!(tests[1].ignored);

        assert_eq!(tests[2].name, "test_panic");
        assert!(tests[2].should_panic);
    }

    #[test]
    fn discover_serial_test() {
        let source = "#[test]\n#[serial]\nfn touches_shared_state() {}\n";
        let result = hew_parser::parse(source);
        let tests = discover_tests(&result.program, "test.hew");

        assert_eq!(tests.len(), 1);
        assert!(tests[0].serial);
    }

    #[test]
    fn no_tests_in_plain_program() {
        let source = "fn main() -> i32 { 0 }";
        let result = hew_parser::parse(source);
        let tests = discover_tests(&result.program, "main.hew");
        assert!(tests.is_empty());
    }

    #[test]
    fn discover_test_files_finds_inline_tests_in_all_sources() {
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
                dir.path().join("plain.hew").display().to_string(),
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
