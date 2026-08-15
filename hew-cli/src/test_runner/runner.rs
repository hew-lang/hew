//! Execute discovered test cases via the native compilation pipeline.

use super::discovery::TestCase;
use std::path::{Path, PathBuf};
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

/// Filesystem inputs needed by the in-process native compiler.
#[derive(Debug)]
pub struct TestCompilePaths {
    paths: crate::NativeBuildPaths,
}

impl TestCompilePaths {
    /// Resolve the standard library and runtime archive before scheduling work.
    pub fn resolve(project_dir: &Path) -> Result<Self, String> {
        let module_search_paths =
            hew_types::module_registry::build_module_search_paths_for(Some(project_dir));
        let target = crate::target::TargetSpec::from_requested(None)
            .map_err(|error| format!("cannot determine the host target: {error}"))?;
        let hew_lib = crate::link::find_hew_lib(
            target.hew_lib_name(),
            target.normalized_triple(),
            target.can_run_on_host(),
        )?;
        Self::from_explicit(
            project_dir.to_path_buf(),
            module_search_paths,
            PathBuf::from(hew_lib),
        )
    }

    fn from_explicit(
        project_dir: PathBuf,
        module_search_paths: Vec<PathBuf>,
        hew_lib: PathBuf,
    ) -> Result<Self, String> {
        let has_stdlib = module_search_paths
            .iter()
            .any(|root| root.join("std/builtins.hew").is_file());
        if !has_stdlib {
            let tried = module_search_paths
                .iter()
                .map(|root| root.join("std/builtins.hew").display().to_string())
                .collect::<Vec<_>>()
                .join(", ");
            return Err(format!(
                "Hew standard library is missing; looked for std/builtins.hew at: {}",
                if tried.is_empty() {
                    "<no search roots>"
                } else {
                    &tried
                }
            ));
        }
        if !hew_lib.is_file() {
            return Err(format!(
                "Hew runtime archive is missing at `{}`",
                hew_lib.display()
            ));
        }
        Ok(Self {
            paths: crate::NativeBuildPaths {
                project_dir,
                module_search_paths,
                hew_lib,
            },
        })
    }
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
    compile_paths: &TestCompilePaths,
    timeout: Duration,
    jobs: usize,
) -> TestSummary {
    if jobs <= 1 {
        return run_tests_serial(
            tests,
            filter,
            include_ignored,
            ffi_lib,
            compile_paths,
            timeout,
        );
    }

    run_tests_parallel(
        tests,
        filter,
        include_ignored,
        ffi_lib,
        compile_paths,
        timeout,
        jobs,
    )
}

fn run_tests_serial(
    tests: &[TestCase],
    filter: Option<&str>,
    include_ignored: bool,
    ffi_lib: Option<&str>,
    compile_paths: &TestCompilePaths,
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

            let result = run_single_test(&source, test, ffi_lib, compile_paths, timeout);
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
    compile_paths: &TestCompilePaths,
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
                    run_single_test(&task.source, &task.test, ffi_lib, compile_paths, timeout)
                } else {
                    run_single_test(&task.source, &task.test, ffi_lib, compile_paths, timeout)
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
    compile_paths: &TestCompilePaths,
) -> Result<CompiledTestArtifact, String> {
    let synthetic = format!(
        "{source}\n\nfn main() {{\n    {name}();\n}}\n",
        name = test.name,
    );

    // Keep the synthetic source beside the fixture so relative file imports
    // resolve from the same directory as the authored test. Discovery excludes
    // the `hew_test_*` prefix, so a process killed before TempPath cleanup cannot
    // poison a later scan with the generated `main`.
    let source_dir = Path::new(&test.file)
        .parent()
        .ok_or_else(|| "test source path has no parent directory".to_string())?;
    let tmp_source = tempfile::Builder::new()
        .prefix("hew_test_")
        .suffix(".hew")
        .tempfile_in(source_dir)
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
    let extra_libs = ffi_lib.into_iter().map(str::to_owned).collect::<Vec<_>>();

    crate::diagnostic::start_diagnostic_capture();
    let compile_result = crate::compile_test_binary_with_paths(
        tmp_source.path(),
        &binary_path,
        &compile_paths.paths,
        &extra_libs,
    );
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
    compile_paths: &TestCompilePaths,
    timeout: Duration,
) -> TestResult {
    let start = std::time::Instant::now();

    let artifact = match compile_test(source, test, ffi_lib, compile_paths) {
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

    fn cargo_test_compile_paths() -> &'static TestCompilePaths {
        static PATHS: OnceLock<TestCompilePaths> = OnceLock::new();
        PATHS.get_or_init(|| {
            let workspace_root = Path::new(env!("CARGO_MANIFEST_DIR"))
                .parent()
                .expect("hew-cli should have a workspace parent")
                .to_path_buf();
            let cargo_profile_dir = Path::new(env!("OUT_DIR"))
                .ancestors()
                .nth(3)
                .expect("OUT_DIR should be below the Cargo profile directory");
            let target = crate::target::TargetSpec::from_requested(None)
                .expect("the test host target should resolve");
            TestCompilePaths::from_explicit(
                workspace_root.clone(),
                vec![workspace_root],
                cargo_profile_dir.join(target.hew_lib_name()),
            )
            .expect("make stdlib should provide explicit test compiler paths")
        })
    }

    #[test]
    fn explicit_compile_paths_name_missing_standard_library() {
        let dir = tempfile::tempdir().expect("create path fixture");
        let archive = dir.path().join("libhew.a");
        std::fs::write(&archive, []).expect("create archive fixture");

        let error = TestCompilePaths::from_explicit(
            dir.path().to_path_buf(),
            vec![dir.path().to_path_buf()],
            archive,
        )
        .expect_err("a root without std/builtins.hew must fail");

        assert!(error.contains("standard library"), "error: {error}");
        assert!(error.contains("std/builtins.hew"), "error: {error}");
    }

    #[test]
    fn explicit_compile_paths_name_missing_runtime_archive() {
        let dir = tempfile::tempdir().expect("create path fixture");
        std::fs::create_dir(dir.path().join("std")).expect("create std fixture");
        std::fs::write(dir.path().join("std/builtins.hew"), []).expect("create builtins fixture");
        let archive = dir.path().join("missing-libhew.a");

        let error = TestCompilePaths::from_explicit(
            dir.path().to_path_buf(),
            vec![dir.path().to_path_buf()],
            archive.clone(),
        )
        .expect_err("a missing runtime archive must fail");

        assert!(error.contains("runtime archive"), "error: {error}");
        assert!(
            error.contains(&archive.display().to_string()),
            "error: {error}"
        );
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
        run_tests(
            &tests,
            None,
            false,
            None,
            cargo_test_compile_paths(),
            timeout,
            1,
        )
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

        let unused_paths = TestCompilePaths {
            paths: crate::NativeBuildPaths {
                project_dir: PathBuf::new(),
                module_search_paths: Vec::new(),
                hew_lib: PathBuf::new(),
            },
        };
        let summary = run_tests(
            &tests,
            None,
            false,
            None,
            &unused_paths,
            DEFAULT_TEST_TIMEOUT,
            2,
        );
        let names: Vec<_> = summary
            .results
            .iter()
            .map(|result| result.test.name.as_str())
            .collect();

        assert_eq!(names, vec!["alpha", "beta", "gamma"]);
    }
}
