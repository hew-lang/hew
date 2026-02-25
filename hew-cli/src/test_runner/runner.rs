//! Execute discovered test cases via the native compilation pipeline.

use super::discovery::TestCase;
use std::path::PathBuf;
use std::process::Command;
use std::time::Duration;

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
) -> TestSummary {
    let mut results = Vec::new();
    let mut passed = 0;
    let mut failed = 0;
    let mut ignored = 0;

    // Group tests by file for efficiency.
    let mut by_file: std::collections::HashMap<&str, Vec<&TestCase>> =
        std::collections::HashMap::new();
    for test in tests {
        if let Some(pat) = filter {
            if !test.name.contains(pat) {
                continue;
            }
        }
        by_file.entry(test.file.as_str()).or_default().push(test);
    }

    for (file, file_tests) in &by_file {
        let source = match std::fs::read_to_string(file) {
            Ok(s) => s,
            Err(e) => {
                for test in file_tests {
                    failed += 1;
                    results.push(TestResult {
                        test: (*test).clone(),
                        outcome: TestOutcome::Failed(format!("cannot read {file}: {e}")),
                        output: String::new(),
                    });
                }
                continue;
            }
        };

        for test in file_tests {
            if test.ignored && !include_ignored {
                ignored += 1;
                results.push(TestResult {
                    test: (*test).clone(),
                    outcome: TestOutcome::Ignored,
                    output: String::new(),
                });
                continue;
            }

            let result = run_single_test(&source, test, ffi_lib);
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

/// Locate the `hew` binary.
///
/// When running as `hew test`, `current_exe()` is the hew binary itself.
/// When running unit tests, the test binary is in `target/debug/deps/` and
/// the hew binary is at `target/debug/hew`.
fn find_hew_binary() -> Result<PathBuf, String> {
    let exe = std::env::current_exe().map_err(|e| format!("cannot locate self: {e}"))?;

    // If the current binary is named "hew" (or "hew.exe" on Windows), use it directly.
    if exe
        .file_name()
        .is_some_and(|n| n == "hew" || n == "hew.exe")
    {
        return Ok(exe);
    }

    // Otherwise, search relative to the current binary.
    let exe_dir = exe.parent().expect("exe should have a parent directory");
    let hew_name = if cfg!(target_os = "windows") {
        "hew.exe"
    } else {
        "hew"
    };
    let candidates = [
        exe_dir.join(format!("../{hew_name}")), // target/debug/deps/../hew
        exe_dir.join(hew_name),                 // same dir
        exe_dir.join(format!("../../debug/{hew_name}")), // fallback
    ];

    for c in &candidates {
        if c.exists() {
            return c
                .canonicalize()
                .map_err(|e| format!("cannot resolve hew binary path: {e}"));
        }
    }

    Err(format!(
        "cannot find hew binary (searched relative to {})",
        exe_dir.display()
    ))
}

/// Compile a synthetic test program to a native binary.
///
/// Returns the paths to the temp source and binary on success, or an error
/// message on failure.
fn compile_test(
    source: &str,
    test: &TestCase,
    ffi_lib: Option<&str>,
) -> Result<(tempfile::NamedTempFile, tempfile::NamedTempFile), String> {
    let synthetic = format!(
        "{source}\n\nfn main() {{\n    {name}();\n}}\n",
        name = test.name,
    );

    let hew_binary = find_hew_binary()?;

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

    let exe_suffix = if cfg!(target_os = "windows") {
        ".exe"
    } else {
        ""
    };
    let tmp_binary = tempfile::Builder::new()
        .prefix("hew_test_bin_")
        .suffix(exe_suffix)
        .tempfile_in(test_dir)
        .map_err(|e| format!("cannot create temp binary: {e}"))?;

    let mut cmd = Command::new(&hew_binary);
    cmd.arg("build")
        .arg(tmp_source.path())
        .arg("-o")
        .arg(tmp_binary.path());
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
fn run_single_test(source: &str, test: &TestCase, ffi_lib: Option<&str>) -> TestResult {
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
            };
        }
    };

    // Close the write fd before exec to avoid ETXTBSY ("Text file busy").
    // NamedTempFile keeps the file open O_RDWR; into_temp_path() closes
    // the fd while still auto-deleting on drop.
    let tmp_binary = tmp_binary.into_temp_path();

    // Execute the compiled binary with a timeout.
    let run_result = run_binary_with_timeout(&tmp_binary, Duration::from_secs(30));

    match run_result {
        RunOutcome::Success { stdout, .. } => {
            if test.should_panic {
                TestResult {
                    test: test.clone(),
                    outcome: TestOutcome::Failed(
                        "expected test to panic, but it completed successfully".into(),
                    ),
                    output: stdout,
                }
            } else {
                TestResult {
                    test: test.clone(),
                    outcome: TestOutcome::Passed,
                    output: stdout,
                }
            }
        }
        RunOutcome::Failed { stdout, stderr, .. } => {
            if test.should_panic {
                TestResult {
                    test: test.clone(),
                    outcome: TestOutcome::Passed,
                    output: stdout,
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
                }
            }
        }
        RunOutcome::Timeout => TestResult {
            test: test.clone(),
            outcome: TestOutcome::Failed("test timed out after 30s".into()),
            output: String::new(),
        },
        RunOutcome::Error(e) => TestResult {
            test: test.clone(),
            outcome: TestOutcome::Failed(format!("cannot execute test binary: {e}")),
            output: String::new(),
        },
    }
}

enum RunOutcome {
    Success { stdout: String },
    Failed { stdout: String, stderr: String },
    Timeout,
    Error(String),
}

fn run_binary_with_timeout(binary: &std::path::Path, timeout: Duration) -> RunOutcome {
    let mut child = match Command::new(binary)
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .spawn()
    {
        Ok(c) => c,
        Err(e) => return RunOutcome::Error(e.to_string()),
    };

    // Wait with timeout.
    let start = std::time::Instant::now();
    loop {
        match child.try_wait() {
            Ok(Some(status)) => {
                let stdout = child
                    .stdout
                    .take()
                    .map(|mut s| {
                        let mut buf = String::new();
                        std::io::Read::read_to_string(&mut s, &mut buf).ok();
                        buf
                    })
                    .unwrap_or_default();

                let stderr = child
                    .stderr
                    .take()
                    .map(|mut s| {
                        let mut buf = String::new();
                        std::io::Read::read_to_string(&mut s, &mut buf).ok();
                        buf
                    })
                    .unwrap_or_default();

                if status.success() {
                    return RunOutcome::Success { stdout };
                }
                return RunOutcome::Failed { stdout, stderr };
            }
            Ok(None) => {
                if start.elapsed() > timeout {
                    let _ = child.kill();
                    let _ = child.wait();
                    return RunOutcome::Timeout;
                }
                std::thread::sleep(Duration::from_millis(10));
            }
            Err(e) => return RunOutcome::Error(e.to_string()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::super::discovery;
    use super::*;

    /// Ensure the `hew` binary is built before tests that need it.
    ///
    /// `cargo test` only builds test harness binaries, not the regular
    /// `target/debug/hew` binary that `find_hew_binary()` resolves to.
    /// Build it once per test run so the runner tests work in a clean
    /// checkout (where `cargo build` hasn't been run separately).
    fn ensure_hew_binary() {
        use std::sync::Once;
        static BUILD: Once = Once::new();
        BUILD.call_once(|| {
            let status = Command::new(env!("CARGO"))
                .args(["build", "--bin", "hew"])
                .status()
                .expect("failed to invoke cargo build");
            assert!(status.success(), "cargo build --bin hew failed");
        });
    }

    /// Helper to run tests from inline source.
    fn run_inline(source: &str) -> TestSummary {
        ensure_hew_binary();
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
        run_tests(&tests, None, false, None)
    }

    #[test]
    fn passing_test() {
        let summary = run_inline(
            r#"
#[test]
fn test_pass() {
    assert(true);
}
"#,
        );
        assert_eq!(summary.passed, 1);
        assert_eq!(summary.failed, 0);
    }

    #[test]
    fn failing_test() {
        let summary = run_inline(
            r#"
#[test]
fn test_fail() {
    assert(false);
}
"#,
        );
        assert_eq!(summary.passed, 0);
        assert_eq!(summary.failed, 1);
    }

    #[test]
    fn assert_eq_pass() {
        let summary = run_inline(
            r#"
fn add(a: i64, b: i64) -> i64 { a + b }

#[test]
fn test_add() {
    assert_eq(add(1, 2), 3);
}
"#,
        );
        assert_eq!(summary.passed, 1);
    }

    #[test]
    fn assert_eq_fail() {
        let summary = run_inline(
            r#"
#[test]
fn test_bad_eq() {
    assert_eq(1, 2);
}
"#,
        );
        assert_eq!(summary.failed, 1);
        if let TestOutcome::Failed(msg) = &summary.results[0].outcome {
            assert!(msg.contains("assert_eq"), "error message: {msg}");
        }
    }

    #[test]
    fn should_panic_pass() {
        let summary = run_inline(
            r#"
#[test]
#[should_panic]
fn test_expected_panic() {
    assert(false);
}
"#,
        );
        assert_eq!(summary.passed, 1);
    }

    #[test]
    fn should_panic_fail_no_panic() {
        let summary = run_inline(
            r#"
#[test]
#[should_panic]
fn test_no_panic() {
    assert(true);
}
"#,
        );
        assert_eq!(summary.failed, 1);
    }

    #[test]
    fn ignored_test() {
        let summary = run_inline(
            r#"
#[test]
#[ignore]
fn test_skip() {
    assert(false);
}
"#,
        );
        assert_eq!(summary.ignored, 1);
        assert_eq!(summary.passed, 0);
        assert_eq!(summary.failed, 0);
    }
}
