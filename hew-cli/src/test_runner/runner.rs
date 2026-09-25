//! Execute discovered test cases via the native compilation pipeline.

use super::discovery::TestCase;
#[cfg(target_os = "linux")]
use std::collections::HashSet;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Mutex;
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
    /// Test failed with a semantic kind and diagnostic.
    Failed(TestFailure),
    /// Test was ignored (not run).
    Ignored,
}

impl TestOutcome {
    pub(crate) fn failed(kind: TestFailureKind, message: impl Into<String>) -> Self {
        Self::Failed(TestFailure {
            kind,
            message: message.into(),
        })
    }
}

/// Stable stage at which a compiled Hew test failed.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TestFailureKind {
    /// The selected test root could not be compiled.
    Compile,
    /// The compiled program ran and reported failure.
    Runtime,
    /// The compiled program exceeded its execution deadline.
    Timeout,
    /// The compiled program could not be started.
    Launch,
}

impl TestFailureKind {
    /// Stable `JUnit` value consumed by the compiled-test ratchets.
    #[must_use]
    pub const fn as_str(self) -> &'static str {
        match self {
            Self::Compile => "compile",
            Self::Runtime => "runtime",
            Self::Timeout => "timeout",
            Self::Launch => "launch",
        }
    }
}

/// Diagnostic attached to a failed compiled Hew test.
#[derive(Debug)]
pub struct TestFailure {
    /// Semantic stage at which the test failed.
    pub kind: TestFailureKind,
    /// Human-readable diagnostic; not part of the ratchet identity.
    pub message: String,
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
    target: crate::target::TargetSpec,
}

impl TestCompilePaths {
    /// Resolve the standard library and runtime archive before scheduling work.
    pub fn resolve(project_dir: &Path) -> Result<Self, String> {
        let module_search_paths = hew_types::module_registry::stdlib_search_paths();
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
        let target = crate::target::TargetSpec::from_requested(None)
            .map_err(|error| format!("cannot determine the host target: {error}"))?;
        Ok(Self {
            paths: crate::NativeBuildPaths {
                project_dir,
                module_search_paths,
                hew_lib,
            },
            target,
        })
    }
}

/// Execution policy and compiler inputs shared by every test in a run.
///
/// Keeping this as one value prevents test-runner entry points from gaining a
/// new positional argument every time the compiler pipeline gains a mode.
#[derive(Debug, Clone, Copy)]
pub struct TestRunOptions<'a> {
    pub filter: Option<&'a str>,
    pub include_ignored: bool,
    pub ffi_lib: Option<&'a str>,
    pub compile_paths: &'a TestCompilePaths,
    pub timeout: Duration,
    pub jobs: usize,
    pub schedules: ScheduleOptions,
    /// Directory test identities (`path::name`) are relative to.
    pub root: &'a Path,
}

/// How the single-thread driver orders a deterministic test.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Schedule {
    /// Participants run in the order they became ready.
    Fifo,
    /// Each pick is a seeded uniform choice over the ready participants.
    Random,
}

impl Schedule {
    const fn as_str(self) -> &'static str {
        match self {
            Self::Fifo => "fifo",
            Self::Random => "random",
        }
    }
}

/// Schedule selection for every deterministic test in a run.
#[derive(Debug, Clone, Copy)]
pub struct ScheduleOptions {
    /// The schedule of each test's first run.
    pub schedule: Schedule,
    /// Overrides the per-test seed (a stable hash of the test's identity).
    pub seed: Option<u64>,
    /// Additional `random` schedules explored after the first run.
    pub explore: u32,
}

/// One execution of a compiled test: a driver schedule and seed, or the
/// threaded runtime for a `#[real_time]` test.
#[derive(Debug, Clone, Copy)]
struct Execution {
    driver: Option<(Schedule, u64)>,
}

impl Execution {
    fn environment(self) -> Option<String> {
        self.driver
            .map(|(schedule, seed)| format!("schedule={},seed={seed:#x}", schedule.as_str()))
    }
}

/// The seed of the `index`th explored schedule: a splitmix64 output over the
/// test's base seed, so the sequence is fixed per test and independent of the
/// other tests in the run.
fn explored_seed(base: u64, index: u32) -> u64 {
    let mut z = base.wrapping_add(u64::from(index + 1).wrapping_mul(0x9e37_79b9_7f4a_7c15));
    z = (z ^ (z >> 30)).wrapping_mul(0xbf58_476d_1ce4_e5b9);
    z = (z ^ (z >> 27)).wrapping_mul(0x94d0_49bb_1331_11eb);
    z ^ (z >> 31)
}

/// Every execution one test gets: the first run under the selected schedule,
/// then the explored `random` schedules. A `#[real_time]` test runs once on
/// the threaded runtime.
fn executions(test: &TestCase, options: &TestRunOptions<'_>) -> Vec<Execution> {
    if test.real_time {
        return vec![Execution { driver: None }];
    }
    let schedules = options.schedules;
    let base = schedules
        .seed
        .unwrap_or_else(|| super::stable_hash(&super::test_identity(test, options.root)));
    let mut runs = vec![Execution {
        driver: Some((schedules.schedule, base)),
    }];
    runs.extend((0..schedules.explore).map(|index| Execution {
        driver: Some((Schedule::Random, explored_seed(base, index))),
    }));
    runs
}

/// Run a set of test cases.
///
/// Each test is compiled to a native binary via the `hew compile` pipeline and
/// executed as a child process for isolation.
#[must_use]
pub fn run_tests(tests: &[TestCase], options: TestRunOptions<'_>) -> TestSummary {
    if options.jobs <= 1 {
        return run_tests_serial(tests, &options);
    }

    run_tests_parallel(tests, &options)
}

fn run_tests_serial(tests: &[TestCase], options: &TestRunOptions<'_>) -> TestSummary {
    let mut results = Vec::new();
    let mut passed = 0;
    let mut failed = 0;
    let mut ignored = 0;

    // Group tests by file for efficiency while preserving discovery order.
    let mut by_file: Vec<(&str, Vec<&TestCase>)> = Vec::new();
    for test in tests {
        if let Some(pat) = options.filter {
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

    for (_, file_tests) in by_file {
        for test in file_tests {
            if test.ignored && !options.include_ignored {
                ignored += 1;
                results.push(TestResult {
                    test: test.clone(),
                    outcome: TestOutcome::Ignored,
                    output: String::new(),
                    duration: Duration::ZERO,
                });
                continue;
            }

            let result = run_single_test(test, options);
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
    test: TestCase,
}

#[allow(
    clippy::too_many_lines,
    reason = "parallel scheduling, stable result placement, and serial-test exclusion form one cohesive execution policy"
)]
fn run_tests_parallel(tests: &[TestCase], options: &TestRunOptions<'_>) -> TestSummary {
    let mut by_file: Vec<(&str, Vec<&TestCase>)> = Vec::new();
    for test in tests {
        if options
            .filter
            .is_some_and(|pattern| !test.name.contains(pattern))
        {
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

    for (_, file_tests) in by_file {
        for test in file_tests {
            if test.ignored && !options.include_ignored {
                result_slots[result_index] = Some(TestResult {
                    test: test.clone(),
                    outcome: TestOutcome::Ignored,
                    output: String::new(),
                    duration: Duration::ZERO,
                });
            } else {
                tasks.push(TestTask {
                    result_index,
                    test: test.clone(),
                });
            }
            result_index += 1;
        }
    }

    let next_task = AtomicUsize::new(0);
    let result_slots = Mutex::new(result_slots);
    let serial_gate = Mutex::new(());
    let worker_count = options.jobs.min(tasks.len().max(1));

    std::thread::scope(|scope| {
        for worker_index in 0..worker_count {
            std::thread::Builder::new()
                .name(format!("hew-test-worker-{worker_index}"))
                .stack_size(crate::COMPILER_STACK_SIZE)
                .spawn_scoped(scope, || loop {
                    let task_index = next_task.fetch_add(1, Ordering::Relaxed);
                    let Some(task) = tasks.get(task_index) else {
                        break;
                    };
                    let result = if task.test.serial {
                        let _serial_guard = serial_gate
                            .lock()
                            .unwrap_or_else(std::sync::PoisonError::into_inner);
                        run_single_test(&task.test, options)
                    } else {
                        run_single_test(&task.test, options)
                    };
                    result_slots
                        .lock()
                        .unwrap_or_else(std::sync::PoisonError::into_inner)[task.result_index] =
                        Some(result);
                })
                .expect("failed to spawn Hew test compiler worker");
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
    _emit_dir: tempfile::TempDir,
    binary_path: PathBuf,
}

fn compile_test(
    test: &TestCase,
    ffi_lib: Option<&str>,
    compile_paths: &TestCompilePaths,
) -> Result<CompiledTestArtifact, String> {
    let emit_dir = tempfile::Builder::new()
        .prefix("hew_test_emit_")
        .tempdir_in(std::env::temp_dir())
        .map_err(|e| format!("cannot create temp emit dir: {e}"))?;

    let source_path = Path::new(&test.file);
    let binary_name = source_path
        .file_stem()
        .and_then(std::ffi::OsStr::to_str)
        .ok_or_else(|| "test source path has no file stem".to_string())?;
    let binary_path = compile_paths
        .target
        .executable_path(emit_dir.path(), binary_name);
    let extra_libs = ffi_lib.into_iter().map(str::to_owned).collect::<Vec<_>>();

    let options = crate::compile::CompileOptions {
        project_dir: Some(compile_paths.paths.project_dir.clone()),
        module_search_paths: Some(compile_paths.paths.module_search_paths.clone()),
        entry_selection: Some(test.occurrence),
        companion: test.companion.as_deref().map(PathBuf::from),
        ..crate::compile::CompileOptions::default()
    };

    crate::diagnostic::start_diagnostic_capture();
    let compile_result = crate::compile_native_binary_with_paths(
        source_path,
        &binary_path,
        &options,
        Some(&compile_paths.paths),
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
        _emit_dir: emit_dir,
        binary_path,
    })
}

fn run_single_test(test: &TestCase, options: &TestRunOptions<'_>) -> TestResult {
    let start = std::time::Instant::now();

    let artifact = match compile_test(test, options.ffi_lib, options.compile_paths) {
        Ok(artifact) => artifact,
        Err(msg) => {
            let outcome = if test.should_panic {
                TestOutcome::failed(
                    TestFailureKind::Compile,
                    format!("compile error (expected panic, got compile error): {msg}"),
                )
            } else {
                TestOutcome::failed(TestFailureKind::Compile, format!("compile error: {msg}"))
            };
            return TestResult {
                test: test.clone(),
                outcome,
                output: String::new(),
                duration: start.elapsed(),
            };
        }
    };

    let runs = executions(test, options);
    let mut first_output = None;
    let mut first_failure: Option<(Execution, TestFailure, String)> = None;
    let mut failed_runs = 0usize;
    for execution in &runs {
        let (outcome, output) = judge_run(
            test,
            crate::process::run_binary_with_driver(
                &artifact.binary_path,
                options.timeout,
                execution.environment().as_deref(),
            ),
            options.timeout,
        );
        match outcome {
            TestOutcome::Failed(failure) => {
                failed_runs += 1;
                if first_failure.is_none() {
                    first_failure = Some((*execution, failure, output));
                }
            }
            TestOutcome::Passed | TestOutcome::Ignored => {
                first_output.get_or_insert(output);
            }
        }
    }

    let duration = start.elapsed();
    let Some((execution, failure, output)) = first_failure else {
        return TestResult {
            test: test.clone(),
            outcome: TestOutcome::Passed,
            output: first_output.unwrap_or_default(),
            duration,
        };
    };
    let message = match execution.driver {
        None => failure.message,
        Some((schedule, seed)) => {
            let mut message = failure.message.trim_end().to_string();
            if runs.len() > 1 {
                message.push_str(&format!(
                    "\nfailed on {failed_runs} of {} schedules",
                    runs.len()
                ));
            }
            message.push_str(&format!(
                "\nschedule {}, seed {seed:#x}\nreproduce: hew test {} --filter {} --schedule {} --seed {seed:#x}",
                schedule.as_str(),
                test.file,
                test.name,
                schedule.as_str(),
            ));
            message
        }
    };
    TestResult {
        test: test.clone(),
        outcome: TestOutcome::failed(failure.kind, message),
        output,
        duration,
    }
}

/// Operands shorter than this that fit on one line are compared by eye.
const DIFF_THRESHOLD: usize = 40;

/// Append a line diff to a failed comparison's report when an operand is too
/// long or spans lines to compare by eye. A one-line value is split after each
/// `, ` so a record or collection diffs field by field.
fn with_operand_diff(report: String) -> String {
    let Some((head, right)) = report.rsplit_once("\n right: ") else {
        return report;
    };
    let Some((_, left)) = head.rsplit_once("\n  left: ") else {
        return report;
    };
    let right = right.trim_end_matches('\n');
    if left.len().max(right.len()) < DIFF_THRESHOLD && !left.contains('\n') && !right.contains('\n')
    {
        return report;
    }
    let pieces = |value: &str| -> Vec<String> {
        if value.contains('\n') {
            value.lines().map(str::to_string).collect()
        } else {
            value.split_inclusive(", ").map(str::to_string).collect()
        }
    };
    let (left, right) = (pieces(left), pieces(right));
    // Longest common subsequence table, then walk it into -/+ lines.
    let mut common = vec![vec![0usize; right.len() + 1]; left.len() + 1];
    for i in (0..left.len()).rev() {
        for j in (0..right.len()).rev() {
            common[i][j] = if left[i] == right[j] {
                common[i + 1][j + 1] + 1
            } else {
                common[i + 1][j].max(common[i][j + 1])
            };
        }
    }
    let mut diff = String::from("\ndiff (-left +right):");
    let (mut i, mut j) = (0, 0);
    while i < left.len() || j < right.len() {
        let line = if i < left.len() && j < right.len() && left[i] == right[j] {
            i += 1;
            j += 1;
            format!("\n    {}", left[i - 1])
        } else if j < right.len() && (i == left.len() || common[i][j + 1] >= common[i + 1][j]) {
            j += 1;
            format!("\n  + {}", right[j - 1])
        } else {
            i += 1;
            format!("\n  - {}", left[i - 1])
        };
        diff.push_str(line.trim_end());
    }
    format!("{}{diff}\n", report.trim_end_matches('\n'))
}

/// Decide one execution's outcome, returning it with the captured stdout.
fn judge_run(
    test: &TestCase,
    run_result: Result<crate::process::BinaryRunOutcome, String>,
    timeout: Duration,
) -> (TestOutcome, String) {
    match run_result {
        Ok(crate::process::BinaryRunOutcome::Success { stdout }) => {
            if test.should_panic {
                (
                    TestOutcome::failed(
                        TestFailureKind::Runtime,
                        "expected test to panic, but it completed successfully",
                    ),
                    stdout,
                )
            } else {
                (TestOutcome::Passed, stdout)
            }
        }
        Ok(crate::process::BinaryRunOutcome::Failed { stdout, stderr, .. }) => {
            if test.should_panic {
                (TestOutcome::Passed, stdout)
            } else {
                let msg = if stderr.is_empty() {
                    "test exited with non-zero status".to_string()
                } else {
                    with_operand_diff(stderr)
                };
                (TestOutcome::failed(TestFailureKind::Runtime, msg), stdout)
            }
        }
        Ok(crate::process::BinaryRunOutcome::Timeout) => (
            TestOutcome::failed(
                TestFailureKind::Timeout,
                format!(
                    "test timed out after {}",
                    crate::process::format_timeout(timeout)
                ),
            ),
            String::new(),
        ),
        Err(e) => (
            TestOutcome::failed(
                TestFailureKind::Launch,
                format!("cannot execute test binary: {e}"),
            ),
            String::new(),
        ),
    }
}

#[cfg(test)]
mod tests {
    use super::super::discovery;
    use super::*;
    use std::sync::OnceLock;

    const FIFO_ONCE: ScheduleOptions = ScheduleOptions {
        schedule: Schedule::Fifo,
        seed: None,
        explore: 0,
    };

    fn require_codegen() -> bool {
        test_toolchain_lib().is_some()
    }

    /// Build the native test toolchain and resolve the `libhew.a` these tests
    /// link against, serialized across every concurrent test process.
    ///
    /// This MUST go through [`hew_testutil::ensure_hew_lib_built`] rather than
    /// shelling `make stdlib`. Under nextest each test is its own process, so a
    /// process-local `OnceLock` serializes nothing across the suite: an
    /// unlocked `cargo build -p hew-lib` here is simultaneously an unlocked
    /// WRITER (Cargo uplifts `libhew.a` non-atomically) and an unlocked READER
    /// (the link below `open()`s that same archive). Either role can land
    /// inside another participant's uplift window, and the link then fails with
    /// the `libhew.a`-absence signature -- which this module reports as a
    /// `compile error` and a 0-passed summary, i.e. a spurious failure of an
    /// `assert(true)` test. The shared file lock + `NEXTEST_RUN_ID` stamp is
    /// the project-wide authority for that window; see
    /// `hew-testutil/tests/libhew_link_race.rs` for the multi-process proof.
    ///
    /// Returning the archive path that was actually built (instead of
    /// re-deriving one from `OUT_DIR`) keeps "the archive we built" and "the
    /// archive we link" the same object by construction.
    fn test_toolchain_lib() -> Option<&'static PathBuf> {
        static HEW_LIB: OnceLock<Option<PathBuf>> = OnceLock::new();
        HEW_LIB
            .get_or_init(|| hew_testutil::ensure_hew_lib_built().ok())
            .as_ref()
    }

    fn cargo_test_compile_paths() -> &'static TestCompilePaths {
        static PATHS: OnceLock<TestCompilePaths> = OnceLock::new();
        PATHS.get_or_init(|| {
            let workspace_root = Path::new(env!("CARGO_MANIFEST_DIR"))
                .parent()
                .expect("hew-cli should have a workspace parent")
                .to_path_buf();
            let hew_lib = test_toolchain_lib()
                .expect("require_codegen must gate every caller of this helper")
                .clone();
            TestCompilePaths::from_explicit(workspace_root.clone(), vec![workspace_root], hew_lib)
                .expect("the serialized toolchain build should provide test compiler paths")
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

    /// Render every non-passing outcome so a count assertion reports WHY it
    /// failed, not just `left: 0, right: 1`.
    ///
    /// A toolchain-level fault (a link against a half-written `libhew.a`, a
    /// missing archive) surfaces here as a compile error on a test whose body
    /// is `assert(true)`. Without this the CI log carries the count and
    /// discards the cause, so the mechanism has to be re-derived by hand.
    fn describe(summary: &TestSummary) -> String {
        let detail = summary
            .results
            .iter()
            .filter_map(|result| match &result.outcome {
                TestOutcome::Failed(failure) => {
                    Some(format!("{} FAILED: {}", result.test.name, failure.message))
                }
                TestOutcome::Ignored => Some(format!("{} ignored", result.test.name)),
                TestOutcome::Passed => None,
            })
            .collect::<Vec<_>>()
            .join("; ");
        format!(
            "passed={} failed={} ignored={}{}",
            summary.passed,
            summary.failed,
            summary.ignored,
            if detail.is_empty() {
                String::new()
            } else {
                format!(" [{detail}]")
            }
        )
    }

    /// Helper to run tests from inline source.
    fn run_inline(source: &str) -> TestSummary {
        run_inline_with_timeout(source, DEFAULT_TEST_TIMEOUT)
    }

    fn run_inline_with_timeout(source: &str, timeout: Duration) -> TestSummary {
        run_inline_with(source, timeout, FIFO_ONCE)
    }

    fn run_inline_with(source: &str, timeout: Duration, schedules: ScheduleOptions) -> TestSummary {
        let result = hew_parser::parse(source);
        let tests = discovery::discover_tests(&result.program, "<inline>");
        // Keep each invocation's source isolated from concurrent test processes
        // and worktrees. The directory handle removes it after `run_tests`
        // finishes, while the stable `.hew` basename preserves source semantics.
        let source_dir = tempfile::Builder::new()
            .prefix("hew_test_inline_")
            .tempdir()
            .expect("create inline test source directory");
        let source_path = source_dir.path().join("inline.hew");
        std::fs::write(&source_path, source).expect("write inline test source");
        let tests: Vec<TestCase> = tests
            .into_iter()
            .map(|mut t| {
                t.file = source_path.display().to_string();
                t
            })
            .collect();
        let summary = run_tests(
            &tests,
            TestRunOptions {
                filter: None,
                include_ignored: false,
                ffi_lib: None,
                compile_paths: cargo_test_compile_paths(),
                timeout,
                jobs: 1,
                schedules,
                root: Path::new("/"),
            },
        );
        drop(source_dir);
        summary
    }

    fn run_discovered_file(path: &Path) -> TestSummary {
        let discovered =
            discovery::discover_tests_in_file(path.to_str().expect("test fixture path is utf-8"))
                .expect("discover test fixture");
        run_tests(
            &discovered.tests,
            TestRunOptions {
                filter: None,
                include_ignored: false,
                ffi_lib: None,
                compile_paths: cargo_test_compile_paths(),
                timeout: DEFAULT_TEST_TIMEOUT,
                jobs: 1,
                schedules: FIFO_ONCE,
                root: Path::new("/"),
            },
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
        assert_eq!(summary.passed, 1, "{}", describe(&summary));
        assert_eq!(summary.failed, 0, "{}", describe(&summary));
    }

    #[test]
    fn selected_test_runs_instead_of_authored_main() {
        if !require_codegen() {
            return;
        }
        let summary = run_inline(
            r"
fn main() {
    assert(false);
}

#[test]
fn selected_test() {
    assert(true);
}
",
        );
        assert_eq!(summary.passed, 1, "{}", describe(&summary));
    }

    #[test]
    fn selected_test_can_call_authored_main() {
        if !require_codegen() {
            return;
        }
        let summary = run_inline(
            r"
fn main() -> i64 {
    7
}

#[test]
fn selected_test() {
    assert(main() == 7);
}
",
        );
        assert_eq!(summary.passed, 1, "{}", describe(&summary));
        assert_eq!(summary.failed, 0, "{}", describe(&summary));
    }

    #[test]
    fn matched_production_peer_supplies_test_symbols() {
        if !require_codegen() {
            return;
        }
        let dir = tempfile::tempdir().expect("create peer fixture directory");
        std::fs::write(
            dir.path().join("peer.hew"),
            "pub fn peer_value() -> bool { true }\n",
        )
        .expect("write production peer");
        let test_path = dir.path().join("peer_test.hew");
        std::fs::write(
            &test_path,
            "#[test]\nfn uses_peer() { assert(peer_value()); }\n",
        )
        .expect("write test peer");

        let summary = run_discovered_file(&test_path);

        assert_eq!(summary.passed, 1, "{}", describe(&summary));
    }

    #[test]
    fn unrelated_sibling_is_not_loaded() {
        if !require_codegen() {
            return;
        }
        let dir = tempfile::tempdir().expect("create sibling fixture directory");
        std::fs::write(
            dir.path().join("unrelated.hew"),
            "fn invalid_helper() { missing_symbol(); }\n",
        )
        .expect("write unrelated sibling");
        let test_path = dir.path().join("isolated_test.hew");
        std::fs::write(&test_path, "#[test]\nfn isolated() { assert(true); }\n")
            .expect("write isolated test");

        let summary = run_discovered_file(&test_path);

        assert_eq!(summary.passed, 1, "{}", describe(&summary));
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
        assert_eq!(summary.passed, 0, "{}", describe(&summary));
        assert_eq!(summary.failed, 1, "{}", describe(&summary));
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
    assert(add(1, 2) == 3);
}
",
        );
        assert_eq!(summary.passed, 1, "{}", describe(&summary));
    }

    #[test]
    fn failed_comparison_reports_expression_and_operands() {
        if !require_codegen() {
            return;
        }
        let summary = run_inline(
            r"
#[test]
fn test_bad_eq() {
    assert(1 == 2);
}
",
        );
        assert_eq!(summary.failed, 1, "{}", describe(&summary));
        if let TestOutcome::Failed(failure) = &summary.results[0].outcome {
            assert_eq!(failure.kind, TestFailureKind::Runtime);
            // The desugar reports the condition's text and both rendered
            // operands, so a failure says what went wrong without a rerun.
            assert!(
                failure.message.contains("assertion failed: 1 == 2")
                    && failure.message.contains("  left: 1\n right: 2"),
                "error message: {}",
                failure.message
            );
        }
    }

    #[test]
    fn compile_failure_has_compile_kind() {
        if !require_codegen() {
            return;
        }
        let summary = run_inline(
            r#"
#[test]
fn test_compile_failure() {
    let value: i64 = "not an integer";
    assert(value == 0);
}
"#,
        );
        assert_eq!(summary.failed, 1, "{}", describe(&summary));
        match &summary.results[0].outcome {
            TestOutcome::Failed(failure) => {
                assert_eq!(failure.kind, TestFailureKind::Compile);
                assert!(!failure.message.is_empty());
            }
            outcome => panic!("expected compile failure, got {outcome:?}"),
        }
    }

    #[test]
    fn failure_kind_junit_values_are_stable() {
        assert_eq!(TestFailureKind::Compile.as_str(), "compile");
        assert_eq!(TestFailureKind::Runtime.as_str(), "runtime");
        assert_eq!(TestFailureKind::Timeout.as_str(), "timeout");
        assert_eq!(TestFailureKind::Launch.as_str(), "launch");
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
        assert_eq!(summary.passed, 1, "{}", describe(&summary));
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
        assert_eq!(summary.failed, 1, "{}", describe(&summary));
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
        assert_eq!(summary.ignored, 1, "{}", describe(&summary));
        assert_eq!(summary.passed, 0, "{}", describe(&summary));
        assert_eq!(summary.failed, 0, "{}", describe(&summary));
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
        assert_eq!(summary.failed, 1, "{}", describe(&summary));
        match &summary.results[0].outcome {
            TestOutcome::Failed(failure) => {
                assert_eq!(failure.kind, TestFailureKind::Timeout);
                assert!(failure.message.contains("timed out after 100ms"));
            }
            outcome => panic!("expected timeout failure, got {outcome:?}"),
        }
    }

    #[test]
    fn preserves_discovery_order_across_files() {
        let tests = vec![
            TestCase {
                name: "alpha".into(),
                file: "alpha_test.hew".into(),
                occurrence: hew_types::DeclarationOccurrence::new(
                    None,
                    &(0..0),
                    hew_types::DeclarationKind::Function,
                    0,
                ),
                companion: None,
                ignored: true,
                should_panic: false,
                serial: false,
                real_time: false,
            },
            TestCase {
                name: "beta".into(),
                file: "nested/beta_test.hew".into(),
                occurrence: hew_types::DeclarationOccurrence::new(
                    None,
                    &(0..0),
                    hew_types::DeclarationKind::Function,
                    0,
                ),
                companion: None,
                ignored: true,
                should_panic: false,
                serial: false,
                real_time: false,
            },
            TestCase {
                name: "gamma".into(),
                file: "tests/gamma.hew".into(),
                occurrence: hew_types::DeclarationOccurrence::new(
                    None,
                    &(0..0),
                    hew_types::DeclarationKind::Function,
                    0,
                ),
                companion: None,
                ignored: true,
                should_panic: false,
                serial: false,
                real_time: false,
            },
        ];

        let unused_paths = TestCompilePaths {
            paths: crate::NativeBuildPaths {
                project_dir: PathBuf::new(),
                module_search_paths: Vec::new(),
                hew_lib: PathBuf::new(),
            },
            target: crate::target::TargetSpec::from_requested(None)
                .expect("the test host target should resolve"),
        };
        let summary = run_tests(
            &tests,
            TestRunOptions {
                filter: None,
                include_ignored: false,
                ffi_lib: None,
                compile_paths: &unused_paths,
                timeout: DEFAULT_TEST_TIMEOUT,
                jobs: 2,
                schedules: FIFO_ONCE,
                root: Path::new("/"),
            },
        );
        let names: Vec<_> = summary
            .results
            .iter()
            .map(|result| result.test.name.as_str())
            .collect();

        assert_eq!(names, vec!["alpha", "beta", "gamma"]);
    }

    const LEDGER: &str = r"
actor Account {
    var balance: i64 = 0,
    receive fn balance() -> i64 { balance }
    receive fn set(amount: i64) { balance = amount; }
    receive fn deposit(amount: i64) { balance = balance + amount; }
}

fn read_then_write(account: Account, amount: i64) -> () fails ActorError {
    let current = account.balance()?;
    account.set(current + amount)?;
}

fn one_turn(account: Account, amount: i64) -> () fails ActorError {
    account.deposit(amount)?;
}

fn read(account: Account) -> i64 {
    match account.balance() {
        .Ok(value) => value,
        .Err(_) => -1,
    }
}
";

    fn ledger_test(deposit: &str) -> String {
        format!(
            "{LEDGER}
#[test]
fn concurrent_deposits_are_not_lost() {{
    let account = spawn Account(balance: 0);
    scope {{
        let first = fork {deposit}(account, 10);
        let second = fork {deposit}(account, 20);
        let _ = await first;
        let _ = await second;
    }}
    assert(read(account) == 30);
}}
"
        )
    }

    const EXPLORE: ScheduleOptions = ScheduleOptions {
        schedule: Schedule::Fifo,
        seed: None,
        explore: 64,
    };

    /// A read-then-write race loses an update on some schedules; exploring
    /// finds one and names a seed that reproduces it.
    #[test]
    fn exploration_finds_a_lost_update_and_names_its_seed() {
        if !require_codegen() {
            return;
        }
        let summary = run_inline_with(
            &ledger_test("read_then_write"),
            DEFAULT_TEST_TIMEOUT,
            EXPLORE,
        );
        assert_eq!(summary.failed, 1, "{}", describe(&summary));
        let TestOutcome::Failed(failure) = &summary.results[0].outcome else {
            unreachable!("counted as failed");
        };
        assert!(
            failure.message.contains("of 65 schedules")
                && failure.message.contains("--seed 0x")
                && failure
                    .message
                    .contains("assertion failed: read(account) == 30"),
            "{}",
            failure.message
        );
    }

    /// The control: one turn per deposit passes every explored schedule.
    #[test]
    fn exploration_passes_a_race_free_ledger() {
        if !require_codegen() {
            return;
        }
        let summary = run_inline_with(&ledger_test("one_turn"), DEFAULT_TEST_TIMEOUT, EXPLORE);
        assert_eq!(summary.passed, 1, "{}", describe(&summary));
    }

    /// A deterministic test sleeps on the virtual clock, so a minute-long
    /// sleep finishes inside a one-second hang guard; the same test on the
    /// host clock runs into the guard.
    #[test]
    fn deterministic_sleep_runs_on_the_virtual_clock() {
        if !require_codegen() {
            return;
        }
        let source = |attribute: &str| {
            format!("#[test]\n{attribute}fn waits() {{\n    sleep(60s);\n    assert(true);\n}}\n")
        };
        let guard = Duration::from_secs(1);
        let virtual_run = run_inline_with_timeout(&source(""), guard);
        assert_eq!(virtual_run.passed, 1, "{}", describe(&virtual_run));
        let host_run = run_inline_with_timeout(&source("#[real_time]\n"), guard);
        assert!(
            matches!(
                &host_run.results[0].outcome,
                TestOutcome::Failed(TestFailure {
                    kind: TestFailureKind::Timeout,
                    ..
                })
            ),
            "{}",
            describe(&host_run)
        );
    }

    #[test]
    fn operand_diff_marks_only_the_differing_field() {
        let report = "hew: failure: UserPanic (212): assertion failed: a == b\n  left: User { name: ada, age: 36, tags: [admin] }\n right: User { name: ada, age: 37, tags: [admin] }\n".to_string();
        let rendered = with_operand_diff(report);
        assert!(
            rendered.ends_with(
                "diff (-left +right):\n    User { name: ada,\n  - age: 36,\n  + age: 37,\n    tags: [admin] }\n"
            ),
            "{rendered}"
        );
    }

    #[test]
    fn short_operands_get_no_diff() {
        let report = "assertion failed: x == 2\n  left: 1\n right: 2\n".to_string();
        assert_eq!(with_operand_diff(report.clone()), report);
    }
}
