//! Execute discovered test cases via the native compilation pipeline.

use super::discovery::TestCase;
use std::process::Command;
use std::time::Duration;

/// Default per-test execution timeout.
#[cfg(test)]
const DEFAULT_TEST_TIMEOUT: Duration = Duration::from_secs(30);

/// Result of running a single test.
#[derive(Debug)]
pub enum TestOutcome {
    /// Test passed.
    Passed,
    /// Test failed with an error message.
    Failed(String),
    /// Test was ignored (not run).
    Ignored,
}

/// Result of a single test execution.
#[derive(Debug)]
pub struct TestResult {
    /// The test case that was run.
    pub test: TestCase,
    /// Outcome of the test.
    pub outcome: TestOutcome,
    /// Captured program output.
    pub output: String,
    /// Wall-clock duration of the test (compile + run).
    pub duration: Duration,
}

/// Summary of a full test run.
#[derive(Debug)]
pub struct TestSummary {
    /// Individual test results.
    pub results: Vec<TestResult>,
    /// Number of tests that passed.
    pub passed: usize,
    /// Number of tests that failed.
    pub failed: usize,
    /// Number of tests that were ignored.
    pub ignored: usize,
}

/// Run a set of test cases.
///
/// Each test is compiled to a native binary via the `hew build` pipeline and
/// executed as a child process for isolation.
#[must_use]
pub fn run_tests(
    tests: &[TestCase],
    filter: Option<&str>,
    include_ignored: bool,
    ffi_lib: Option<&str>,
    timeout: Duration,
) -> TestSummary {
    let mut results = Vec::new();
    let mut passed = 0;
    let mut failed = 0;
    let mut ignored = 0;

    // Group tests by file for efficiency while preserving discovery order.
    let mut by_file: Vec<(&str, Vec<&TestCase>)> = Vec::new();
    for test in tests {
        if let Some(pat) = filter {
            if !test.name.contains(pat) {
                continue;
            }
        }
        if let Some((_, grouped_tests)) = by_file
            .iter_mut()
            .find(|(file, _)| *file == test.file.as_str())
        {
            grouped_tests.push(test);
        } else {
            by_file.push((test.file.as_str(), vec![test]));
        }
    }

    for (file, file_tests) in by_file {
        let source = match std::fs::read_to_string(file) {
            Ok(s) => s,
            Err(e) => {
                for test in &file_tests {
                    failed += 1;
                    results.push(TestResult {
                        test: (*test).clone(),
                        outcome: TestOutcome::Failed(format!("cannot read {file}: {e}")),
                        output: String::new(),
                        duration: Duration::ZERO,
                    });
                }
                continue;
            }
        };

        for test in file_tests {
            if test.ignored && !include_ignored {
                ignored += 1;
                results.push(TestResult {
                    test: test.clone(),
                    outcome: TestOutcome::Ignored,
                    output: String::new(),
                    duration: Duration::ZERO,
                });
                continue;
            }

            let result = run_single_test(&source, test, ffi_lib, timeout);
            match &result.outcome {
                TestOutcome::Passed => passed += 1,
                TestOutcome::Failed(_) => failed += 1,
                TestOutcome::Ignored => ignored += 1,
            }
            results.push(result);
        }
    }

    TestSummary {
        results,
        passed,
        failed,
        ignored,
    }
}

/// Compile a synthetic test program to a native binary.
///
/// Returns the paths to the temp source and binary on success, or an error
/// message on failure.
fn compile_test(
    source: &str,
    test: &TestCase,
    ffi_lib: Option<&str>,
) -> Result<(tempfile::NamedTempFile, tempfile::TempPath), String> {
    let synthetic = format!(
        "{source}\n\nfn main() {{\n    {name}();\n}}\n",
        name = test.name,
    );

    let hew_binary = crate::util::find_hew_binary()?;

    let test_dir = std::path::Path::new(&test.file)
        .parent()
        .unwrap_or(std::path::Path::new("."));

    let tmp_source = tempfile::Builder::new()
        .prefix("hew_test_")
        .suffix(".hew")
        .tempfile_in(test_dir)
        .map_err(|e| format!("cannot create temp file: {e}"))?;

    std::fs::write(tmp_source.path(), &synthetic)
        .map_err(|e| format!("cannot write temp file: {e}"))?;

    let tmp_binary = tempfile::Builder::new()
        .prefix("hew_test_bin_")
        .suffix(crate::platform::exe_suffix())
        .tempfile_in(test_dir)
        .map_err(|e| format!("cannot create temp binary: {e}"))?
        .into_temp_path();

    let mut cmd = Command::new(&hew_binary);
    cmd.arg("build")
        .arg(tmp_source.path())
        .arg("-o")
        .arg(&tmp_binary);
    if let Some(lib) = ffi_lib {
        cmd.arg("--link-lib").arg(lib);
    }
    let compile_output = cmd
        .output()
        .map_err(|e| format!("cannot invoke hew build: {e}"))?;

    if !compile_output.status.success() {
        let stderr = String::from_utf8_lossy(&compile_output.stderr);
        let stdout = String::from_utf8_lossy(&compile_output.stdout);
        let msg = if stderr.is_empty() {
            stdout.to_string()
        } else {
            stderr.to_string()
        };
        return Err(msg);
    }

    Ok((tmp_source, tmp_binary))
}

/// Build a synthetic program that calls the test function, compile it natively,
/// and execute the resulting binary.
fn run_single_test(
    source: &str,
    test: &TestCase,
    ffi_lib: Option<&str>,
    timeout: Duration,
) -> TestResult {
    let start = std::time::Instant::now();

    let tmp_binary = match compile_test(source, test, ffi_lib) {
        Ok((_src, bin)) => bin,
        Err(msg) => {
            let outcome = if test.should_panic {
                TestOutcome::Failed(format!(
                    "compile error (expected panic, got compile error): {msg}"
                ))
            } else {
                TestOutcome::Failed(format!("compile error: {msg}"))
            };
            return TestResult {
                test: test.clone(),
                outcome,
                output: String::new(),
                duration: start.elapsed(),
            };
        }
    };

    // Execute the compiled binary with a timeout.
    let run_result = crate::process::run_binary_with_timeout(&tmp_binary, timeout);

    let duration = start.elapsed();
    match run_result {
        Ok(crate::process::BinaryRunOutcome::Success { stdout }) => {
            if test.should_panic {
                TestResult {
                    test: test.clone(),
                    outcome: TestOutcome::Failed(
                        "expected test to panic, but it completed successfully".into(),
                    ),
                    output: stdout,
                    duration,
                }
            } else {
                TestResult {
                    test: test.clone(),
                    outcome: TestOutcome::Passed,
                    output: stdout,
                    duration,
                }
            }
        }
        Ok(crate::process::BinaryRunOutcome::Failed { stdout, stderr, .. }) => {
            if test.should_panic {
                TestResult {
                    test: test.clone(),
                    outcome: TestOutcome::Passed,
                    output: stdout,
                    duration,
                }
            } else {
                let msg = if stderr.is_empty() {
                    "test exited with non-zero status".to_string()
                } else {
                    stderr
                };
                TestResult {
                    test: test.clone(),
                    outcome: TestOutcome::Failed(msg),
                    output: stdout,
                    duration,
                }
            }
        }
        Ok(crate::process::BinaryRunOutcome::Timeout) => TestResult {
            test: test.clone(),
            outcome: TestOutcome::Failed(format!(
                "test timed out after {}",
                crate::process::format_timeout(timeout)
            )),
            output: String::new(),
            duration,
        },
        Err(e) => TestResult {
            test: test.clone(),
            outcome: TestOutcome::Failed(format!("cannot execute test binary: {e}")),
            output: String::new(),
            duration,
        },
    }
}

#[cfg(test)]
mod tests {
    use super::super::discovery;
    use super::*;
    use std::sync::OnceLock;

    /// Skip tests that require the linked native backend when this crate was
    /// built without the embedded MLIR/LLVM bridge.
    fn require_codegen() -> bool {
        ensure_test_toolchain() && crate::util::find_hew_binary().is_ok()
    }

    /// Ensure the full native test toolchain is available before tests that
    /// exercise `hew test` end-to-end.
    fn ensure_test_toolchain() -> bool {
        static BUILD_OK: OnceLock<bool> = OnceLock::new();
        *BUILD_OK.get_or_init(|| {
            Command::new("make")
                .args(["hew", "codegen", "runtime", "stdlib"])
                .status()
                .is_ok_and(|status| status.success())
        })
    }

    /// Helper to run tests from inline source.
    fn run_inline(source: &str) -> TestSummary {
        run_inline_with_timeout(source, DEFAULT_TEST_TIMEOUT)
    }

    fn run_inline_with_timeout(source: &str, timeout: Duration) -> TestSummary {
        let result = hew_parser::parse(source);
        let tests = discovery::discover_tests(&result.program, "<inline>");
        // Write source to a unique temp file so the runner can read it.
        let thread_name = std::thread::current()
            .name()
            .unwrap_or("unknown")
            .replace("::", "_");
        let tmp = std::env::temp_dir().join(format!("hew_test_inline_{thread_name}.hew"));
        std::fs::write(&tmp, source).unwrap();
        let tests: Vec<TestCase> = tests
            .into_iter()
            .map(|mut t| {
                t.file = tmp.display().to_string();
                t
            })
            .collect();
        run_tests(&tests, None, false, None, timeout)
    }

    #[test]
    fn passing_test() {
        if !require_codegen() {
            return;
        }
        let summary = run_inline(
            r"
#[test]
fn test_pass() {
    assert(true);
}
",
        );
        assert_eq!(summary.passed, 1);
        assert_eq!(summary.failed, 0);
    }

    #[test]
    fn failing_test() {
        if !require_codegen() {
            return;
        }
        let summary = run_inline(
            r"
#[test]
fn test_fail() {
    assert(false);
}
",
        );
        assert_eq!(summary.passed, 0);
        assert_eq!(summary.failed, 1);
    }

    #[test]
    fn assert_eq_pass() {
        if !require_codegen() {
            return;
        }
        let summary = run_inline(
            r"
fn add(a: i64, b: i64) -> i64 { a + b }

#[test]
fn test_add() {
    assert_eq(add(1, 2), 3);
}
",
        );
        assert_eq!(summary.passed, 1);
    }

    #[test]
    fn assert_eq_fail() {
        if !require_codegen() {
            return;
        }
        let summary = run_inline(
            r"
#[test]
fn test_bad_eq() {
    assert_eq(1, 2);
}
",
        );
        assert_eq!(summary.failed, 1);
        if let TestOutcome::Failed(msg) = &summary.results[0].outcome {
            assert!(msg.contains("assert_eq"), "error message: {msg}");
        }
    }

    #[test]
    fn should_panic_pass() {
        if !require_codegen() {
            return;
        }
        let summary = run_inline(
            r"
#[test]
#[should_panic]
fn test_expected_panic() {
    assert(false);
}
",
        );
        assert_eq!(summary.passed, 1);
    }

    #[test]
    fn should_panic_fail_no_panic() {
        if !require_codegen() {
            return;
        }
        let summary = run_inline(
            r"
#[test]
#[should_panic]
fn test_no_panic() {
    assert(true);
}
",
        );
        assert_eq!(summary.failed, 1);
    }

    #[test]
    fn ignored_test() {
        if !require_codegen() {
            return;
        }
        let summary = run_inline(
            r"
#[test]
#[ignore]
fn test_skip() {
    assert(false);
}
",
        );
        assert_eq!(summary.ignored, 1);
        assert_eq!(summary.passed, 0);
        assert_eq!(summary.failed, 0);
    }

    #[test]
    fn timeout_test() {
        if !require_codegen() {
            return;
        }
        let summary = run_inline_with_timeout(
            r#"
#[test]
fn test_timeout() {
    while true {
        println("spin");
    }
}
"#,
            Duration::from_millis(100),
        );
        assert_eq!(summary.failed, 1);
        match &summary.results[0].outcome {
            TestOutcome::Failed(message) => assert!(message.contains("timed out after 100ms")),
            outcome => panic!("expected timeout failure, got {outcome:?}"),
        }
    }

    #[test]
    fn preserves_discovery_order_across_files() {
        let tests = vec![
            TestCase {
                name: "alpha".into(),
                file: "alpha_test.hew".into(),
                ignored: true,
                should_panic: false,
            },
            TestCase {
                name: "beta".into(),
                file: "nested/beta_test.hew".into(),
                ignored: true,
                should_panic: false,
            },
            TestCase {
                name: "gamma".into(),
                file: "tests/gamma.hew".into(),
                ignored: true,
                should_panic: false,
            },
        ];

        let summary = run_tests(&tests, None, false, None, DEFAULT_TEST_TIMEOUT);
        let names: Vec<_> = summary
            .results
            .iter()
            .map(|result| result.test.name.as_str())
            .collect();

        assert_eq!(names, vec!["alpha", "beta", "gamma"]);
    }
}
