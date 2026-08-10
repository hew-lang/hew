//! Execute discovered test cases via the native compilation pipeline.

use super::discovery::TestCase;
use std::path::PathBuf;
#[cfg(test)]
use std::process::Command;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{Arc, Mutex};
use std::time::Duration;

const MAX_DEFAULT_JOBS: usize = 8;

/// Choose a conservative host-aware default for concurrent compilation tasks.
#[must_use]
pub fn default_jobs() -> usize {
    physical_core_count()
        .unwrap_or_else(|| {
            std::thread::available_parallelism().map_or(1, std::num::NonZeroUsize::get)
        })
        .clamp(1, MAX_DEFAULT_JOBS)
}

#[cfg(target_os = "macos")]
fn physical_core_count() -> Option<usize> {
    use std::ffi::CString;

    let name = CString::new("hw.physicalcpu").ok()?;
    let mut cores: libc::c_uint = 0;
    let mut size = std::mem::size_of_val(&cores);
    // SAFETY: `cores` and `size` point to writable storage of the advertised
    // length, and the remaining sysctl arguments are null for a read-only query.
    let status = unsafe {
        libc::sysctlbyname(
            name.as_ptr(),
            std::ptr::from_mut(&mut cores).cast(),
            &raw mut size,
            std::ptr::null_mut(),
            0,
        )
    };
    (status == 0 && cores > 0).then_some(cores as usize)
}

#[cfg(target_os = "linux")]
fn physical_core_count() -> Option<usize> {
    use std::collections::HashSet;

    let cpuinfo = std::fs::read_to_string("/proc/cpuinfo").ok()?;
    let mut physical_id = None;
    let mut core_id = None;
    let mut cores = HashSet::new();
    for line in cpuinfo.lines().chain(std::iter::once("")) {
        if let Some((key, value)) = line.split_once(':') {
            match key.trim() {
                "physical id" => physical_id = value.trim().parse::<usize>().ok(),
                "core id" => core_id = value.trim().parse::<usize>().ok(),
                _ => {}
            }
        } else if line.is_empty() {
            if let (Some(package), Some(core)) = (physical_id.take(), core_id.take()) {
                cores.insert((package, core));
            }
        }
    }
    (!cores.is_empty()).then_some(cores.len())
}

#[cfg(not(any(target_os = "linux", target_os = "macos")))]
fn physical_core_count() -> Option<usize> {
    None
}

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
/// Each test is compiled to a native binary via the `hew compile` pipeline and
/// executed as a child process for isolation.
#[must_use]
pub fn run_tests(
    tests: &[TestCase],
    filter: Option<&str>,
    include_ignored: bool,
    ffi_lib: Option<&str>,
    timeout: Duration,
    jobs: usize,
) -> TestSummary {
    if jobs <= 1 {
        return run_tests_serial(tests, filter, include_ignored, ffi_lib, timeout);
    }

    run_tests_parallel(tests, filter, include_ignored, ffi_lib, timeout, jobs)
}

fn run_tests_serial(
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

struct TestTask {
    result_index: usize,
    source: Arc<str>,
    test: TestCase,
}

fn run_tests_parallel(
    tests: &[TestCase],
    filter: Option<&str>,
    include_ignored: bool,
    ffi_lib: Option<&str>,
    timeout: Duration,
    jobs: usize,
) -> TestSummary {
    let mut by_file: Vec<(&str, Vec<&TestCase>)> = Vec::new();
    for test in tests {
        if filter.is_some_and(|pattern| !test.name.contains(pattern)) {
            continue;
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

    let result_count = by_file.iter().map(|(_, tests)| tests.len()).sum();
    let mut result_slots: Vec<Option<TestResult>> =
        std::iter::repeat_with(|| None).take(result_count).collect();
    let mut tasks = Vec::new();
    let mut result_index = 0;

    for (file, file_tests) in by_file {
        let source = match std::fs::read_to_string(file) {
            Ok(source) => Some(Arc::<str>::from(source)),
            Err(error) => {
                for test in file_tests {
                    result_slots[result_index] = Some(TestResult {
                        test: test.clone(),
                        outcome: TestOutcome::Failed(format!("cannot read {file}: {error}")),
                        output: String::new(),
                        duration: Duration::ZERO,
                    });
                    result_index += 1;
                }
                continue;
            }
        };

        for test in file_tests {
            if test.ignored && !include_ignored {
                result_slots[result_index] = Some(TestResult {
                    test: test.clone(),
                    outcome: TestOutcome::Ignored,
                    output: String::new(),
                    duration: Duration::ZERO,
                });
            } else {
                tasks.push(TestTask {
                    result_index,
                    source: Arc::clone(source.as_ref().expect("source was read")),
                    test: test.clone(),
                });
            }
            result_index += 1;
        }
    }

    let next_task = AtomicUsize::new(0);
    let result_slots = Mutex::new(result_slots);
    let serial_gate = Mutex::new(());
    let worker_count = jobs.min(tasks.len().max(1));

    std::thread::scope(|scope| {
        for _ in 0..worker_count {
            scope.spawn(|| loop {
                let task_index = next_task.fetch_add(1, Ordering::Relaxed);
                let Some(task) = tasks.get(task_index) else {
                    break;
                };
                let result = if task.test.serial {
                    let _serial_guard = serial_gate
                        .lock()
                        .unwrap_or_else(std::sync::PoisonError::into_inner);
                    run_single_test(&task.source, &task.test, ffi_lib, timeout)
                } else {
                    run_single_test(&task.source, &task.test, ffi_lib, timeout)
                };
                result_slots
                    .lock()
                    .unwrap_or_else(std::sync::PoisonError::into_inner)[task.result_index] =
                    Some(result);
            });
        }
    });

    summarize(
        result_slots
            .into_inner()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .into_iter()
            .map(|result| result.expect("every scheduled test returns a result"))
            .collect(),
    )
}

fn summarize(results: Vec<TestResult>) -> TestSummary {
    let mut passed = 0;
    let mut failed = 0;
    let mut ignored = 0;
    for result in &results {
        match result.outcome {
            TestOutcome::Passed => passed += 1,
            TestOutcome::Failed(_) => failed += 1,
            TestOutcome::Ignored => ignored += 1,
        }
    }
    TestSummary {
        results,
        passed,
        failed,
        ignored,
    }
}

struct CompiledTestArtifact {
    _source: tempfile::NamedTempFile,
    _emit_dir: tempfile::TempDir,
    binary_path: PathBuf,
}

/// Compile a synthetic test program to a native binary.
fn compile_test(
    source: &str,
    test: &TestCase,
    ffi_lib: Option<&str>,
) -> Result<CompiledTestArtifact, String> {
    if ffi_lib.is_some() {
        return Err("hew test FFI libraries are unavailable on the v0.5 compile path".to_string());
    }

    let synthetic = format!(
        "{source}\n\nfn main() {{\n    {name}();\n}}\n",
        name = test.name,
    );

    // Write synthetic source and the emit dir to the system temp directory,
    // NOT to the test file's own parent.  If the process is killed mid-run,
    // a leftover hew_test_*.hew inside a tests/ directory would be picked up
    // by the next discovery scan and cause a spurious "main is defined multiple
    // times" compile error.  The OS temp dir is outside any scanned tree.
    let tmp_source = tempfile::Builder::new()
        .prefix("hew_test_")
        .suffix(".hew")
        .tempfile_in(std::env::temp_dir())
        .map_err(|e| format!("cannot create temp file: {e}"))?;

    std::fs::write(tmp_source.path(), &synthetic)
        .map_err(|e| format!("cannot write temp file: {e}"))?;

    let emit_dir = tempfile::Builder::new()
        .prefix("hew_test_emit_")
        .tempdir_in(std::env::temp_dir())
        .map_err(|e| format!("cannot create temp emit dir: {e}"))?;

    let binary_name = tmp_source
        .path()
        .file_stem()
        .ok_or_else(|| "temp source path has no file stem".to_string())?;
    let binary_path = emit_dir.path().join(binary_name);

    crate::diagnostic::start_diagnostic_capture();
    let compile_result = crate::compile_native_binary(tmp_source.path(), &binary_path);
    let diagnostics = crate::diagnostic::finish_diagnostic_capture();
    if compile_result.is_err() {
        return Err(if diagnostics.is_empty() {
            "in-process compilation failed".to_string()
        } else {
            diagnostics.trim_end().to_string()
        });
    }

    Ok(CompiledTestArtifact {
        _source: tmp_source,
        _emit_dir: emit_dir,
        binary_path,
    })
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

    let artifact = match compile_test(source, test, ffi_lib) {
        Ok(artifact) => artifact,
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
    let run_result = crate::process::run_binary_with_timeout(&artifact.binary_path, timeout);

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

    /// Skip tests that require the linked native execution substrate while
    /// `hew test` is still blocked by the v0.5 cutover guard.
    fn require_codegen() -> bool {
        ensure_test_toolchain()
    }

    /// Ensure the full native test toolchain is available before tests that
    /// exercise `hew test` end-to-end.
    fn ensure_test_toolchain() -> bool {
        static BUILD_OK: OnceLock<bool> = OnceLock::new();
        *BUILD_OK.get_or_init(|| {
            Command::new("make")
                .current_dir(
                    std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
                        .parent()
                        .expect("hew-cli should have a workspace parent"),
                )
                .arg("stdlib")
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
        run_tests(&tests, None, false, None, timeout, 1)
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
                serial: false,
            },
            TestCase {
                name: "beta".into(),
                file: "nested/beta_test.hew".into(),
                ignored: true,
                should_panic: false,
                serial: false,
            },
            TestCase {
                name: "gamma".into(),
                file: "tests/gamma.hew".into(),
                ignored: true,
                should_panic: false,
                serial: false,
            },
        ];

        let summary = run_tests(&tests, None, false, None, DEFAULT_TEST_TIMEOUT, 2);
        let names: Vec<_> = summary
            .results
            .iter()
            .map(|result| result.test.name.as_str())
            .collect();

        assert_eq!(names, vec!["alpha", "beta", "gamma"]);
    }
}
