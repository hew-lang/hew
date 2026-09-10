use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, ExitStatus, Stdio};
use std::thread;
use std::time::{Duration, Instant};

use serde::Deserialize;

type Result<T> = std::result::Result<T, String>;

#[derive(Debug, Deserialize)]
struct Manifest {
    #[serde(rename = "case")]
    cases: Vec<Case>,
}

#[derive(Debug, Clone, Deserialize)]
struct Case {
    id: String,
    intent: String,
    source: PathBuf,
    /// Flat input directory copied independently for each execution profile.
    fixtures: Option<PathBuf>,
    suites: Vec<String>,
    timeout_seconds: u64,
    /// A case without a `kind` is `run`: compile and execute at O0 and O2.
    #[serde(default)]
    kind: CaseKind,
    expected: ExpectedOutcome,
}

/// What a case proves. `Run` (the default, and today's only behaviour)
/// compiles and executes the source at O0 and O2 against an exact
/// stdout/stderr/exit expectation. `Check` runs `hew check` once against the
/// source and asserts the exact set of diagnostics it reports; it never
/// builds or executes a binary. Safety (ASan/LSan) stays a suite selected by
/// `suites`, not a case kind — a `check` case is never sanitizer-compiled.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize, Default)]
#[serde(rename_all = "lowercase")]
enum CaseKind {
    #[default]
    Run,
    Check,
}

#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
struct ExpectedOutcome {
    /// `kind = "run"` only, and required there: the exact stdout the
    /// program must produce. `Option` (not a default-empty `String`) so a
    /// run case that forgets it is a validation error, not a silent pass
    /// against an empty expectation.
    #[serde(default)]
    stdout: Option<String>,
    #[serde(default)]
    stderr: String,
    /// `kind = "run"` only, and required there: see `stdout` for why this is
    /// `Option` rather than default-zero.
    #[serde(default)]
    exit: Option<i32>,
    /// `kind = "check"` only: the exact diagnostics `hew check --format json`
    /// must report, matched as a set on `(code, line, column)` with an
    /// optional message substring.
    #[serde(default)]
    diagnostics: Vec<ExpectedDiagnostic>,
}

impl ExpectedOutcome {
    /// The `(stdout, exit)` a `run` case expects. `validate_manifest`
    /// requires both to be present before a `run` case ever reaches
    /// `execute`, so the `expect`s here document an already-checked
    /// invariant rather than a fallible lookup.
    fn run_expectation(&self) -> (&str, i32) {
        (
            self.stdout
                .as_deref()
                .expect("run case validated to have expected stdout"),
            self.exit
                .expect("run case validated to have an expected exit"),
        )
    }
}

/// One expected diagnostic for a `check` case.
///
/// `code` is the JSON diagnostic's `code` field verbatim — the stable `kind`
/// discriminant `hew check --format json` emits (see
/// `hew-cli/src/diagnostic_json.rs`). For a diagnostic whose specific error
/// lives under a generic discriminant (`InvalidOperation` covers several
/// distinct `E_*` checks today), `code` is that generic string and `message`
/// is what actually pins down which one: the `E_*`/`W_*` token is embedded in
/// the message text, not the JSON code, until each such check earns its own
/// discriminant.
#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
struct ExpectedDiagnostic {
    code: String,
    line: usize,
    column: usize,
    #[serde(default)]
    message: Option<String>,
}

#[derive(Debug)]
struct Options {
    suite: String,
    cases: Vec<String>,
    hew_bin: PathBuf,
    timeout_seconds: Option<u64>,
}

#[derive(Debug, Clone, Copy)]
enum Profile {
    O0,
    O2,
}

impl Profile {
    fn cli_level(self) -> &'static str {
        match self {
            Self::O0 => "0",
            Self::O2 => "2",
        }
    }

    fn label(self) -> &'static str {
        match self {
            Self::O0 => "O0",
            Self::O2 => "O2",
        }
    }
}

#[derive(Debug)]
enum CommandResult {
    Completed {
        status: ExitStatus,
        stdout: String,
        stderr: String,
    },
    TimedOut {
        stdout: String,
        stderr: String,
    },
}

pub(crate) fn run(args: &[String]) -> Result<()> {
    if matches!(args, [flag] if flag == "--help" || flag == "-h") {
        println!("{}", usage());
        return Ok(());
    }
    let options = parse_options(args)?;
    let root = workspace_root()?;
    let manifest = load_manifest(&root)?;
    validate_manifest(&manifest, &root)?;
    let selected = select_cases(&manifest, &options.suite, &options.cases)?;
    let fingerprint = compiler_fingerprint(&options.hew_bin)?;
    let instrumentation_request = if options.suite == "safety" {
        if !cfg!(target_os = "linux") {
            return Err("environment failure: paired ASan/LSan safety requires Linux".to_string());
        }
        "address"
    } else {
        "none"
    };

    println!(
        "core-acceptance compiler={} version={fingerprint:?} host={}-{} instrumentation-requested={instrumentation_request}",
        options.hew_bin.display(),
        std::env::consts::OS,
        std::env::consts::ARCH,
    );
    println!(
        "core-acceptance suite={} selected={} build=prebuilt",
        options.suite,
        selected.len()
    );

    let run_dir = tempfile::tempdir()
        .map_err(|err| format!("create core acceptance temporary directory: {err}"))?;
    let runner = Runner {
        options: &options,
        root: &root,
        run_dir: run_dir.path(),
        instrumentation_request,
    };
    let mut failures = 0usize;
    for case in selected {
        if !runner.run_case(case) {
            failures += 1;
        }
    }

    if failures == 0 {
        println!("core-acceptance: PASS");
        Ok(())
    } else {
        Err(format!("core-acceptance: {failures} case(s) failed"))
    }
}

fn parse_options(args: &[String]) -> Result<Options> {
    let root = workspace_root()?;
    let mut suite = "acceptance".to_string();
    let mut cases = Vec::new();
    let mut hew_bin = root.join("target/debug/hew");
    let mut timeout_seconds = None;
    let mut index = 0;

    while index < args.len() {
        let value = &args[index];
        match value.as_str() {
            "--suite" => {
                suite = required_value(args, &mut index, "--suite")?.to_string();
            }
            "--case" => {
                cases.push(required_value(args, &mut index, "--case")?.to_string());
            }
            "--hew-bin" => {
                hew_bin = PathBuf::from(required_value(args, &mut index, "--hew-bin")?);
            }
            "--timeout-seconds" => {
                let value = required_value(args, &mut index, "--timeout-seconds")?;
                timeout_seconds = Some(value.parse().map_err(|_| {
                    format!("--timeout-seconds must be a positive integer, got {value:?}")
                })?);
            }
            "--help" | "-h" => return Err("--help must be used on its own".to_string()),
            _ => {
                return Err(format!(
                    "unknown core-acceptance option: {value}\n\n{}",
                    usage()
                ));
            }
        }
        index += 1;
    }

    if !matches!(suite.as_str(), "acceptance" | "safety") {
        return Err(format!(
            "unknown core-acceptance suite {suite:?}; expected acceptance or safety"
        ));
    }
    if timeout_seconds == Some(0) {
        return Err("--timeout-seconds must be positive".to_string());
    }
    Ok(Options {
        suite,
        cases,
        hew_bin,
        timeout_seconds,
    })
}

fn required_value<'a>(args: &'a [String], index: &mut usize, flag: &str) -> Result<&'a str> {
    *index += 1;
    args.get(*index)
        .map(String::as_str)
        .ok_or_else(|| format!("{flag} requires a value"))
}

fn usage() -> String {
    [
        "usage: cargo run -p xtask -- core-acceptance [options]",
        "",
        "options:",
        "  --suite acceptance|safety              select a manifest suite (default: acceptance)",
        "  --case ID                             run one named manifest case",
        "  --hew-bin PATH                        use a prebuilt compiler binary",
        "  --timeout-seconds N                   override each case timeout",
    ]
    .join("\n")
}

fn workspace_root() -> Result<PathBuf> {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .map(Path::to_path_buf)
        .ok_or_else(|| "xtask manifest should have a workspace parent".to_string())
}

fn load_manifest(root: &Path) -> Result<Manifest> {
    let cases_dir = root.join("tests/core-acceptance/cases");
    let mut paths: Vec<PathBuf> = fs::read_dir(&cases_dir)
        .map_err(|err| format!("read core acceptance cases {}: {err}", cases_dir.display()))?
        .map(|entry| entry.map(|entry| entry.path()))
        .collect::<std::result::Result<_, _>>()
        .map_err(|err| format!("read core acceptance case entry: {err}"))?;
    paths.retain(|path| path.extension().is_some_and(|ext| ext == "toml"));
    paths.sort();

    let mut cases = Vec::with_capacity(paths.len());
    for path in paths {
        let contents = fs::read_to_string(&path)
            .map_err(|err| format!("read core acceptance case {}: {err}", path.display()))?;
        let mut parsed: Manifest = toml::from_str(&contents)
            .map_err(|err| format!("parse core acceptance case {}: {err}", path.display()))?;
        if parsed.cases.len() != 1 {
            return Err(format!(
                "core acceptance case file must contain exactly one [[case]]: {}",
                path.display()
            ));
        }
        let case = parsed.cases.remove(0);
        let file_stem = path
            .file_stem()
            .map(|stem| stem.to_string_lossy().into_owned());
        if file_stem.as_deref() != Some(case.id.as_str()) {
            return Err(format!(
                "core acceptance case id {:?} does not match its file name {}",
                case.id,
                path.display()
            ));
        }
        cases.push(case);
    }
    Ok(Manifest { cases })
}

fn validate_manifest(manifest: &Manifest, root: &Path) -> Result<()> {
    if manifest.cases.is_empty() {
        return Err("core acceptance manifest has no cases".to_string());
    }
    let mut ids = std::collections::BTreeSet::new();
    for case in &manifest.cases {
        if case.id.is_empty() {
            return Err("core acceptance case id must not be empty".to_string());
        }
        if case.intent.is_empty() {
            return Err(format!("{} must state its semantic intent", case.id));
        }
        if !ids.insert(&case.id) {
            return Err(format!("duplicate core acceptance case id: {}", case.id));
        }
        if case.timeout_seconds == 0 {
            return Err(format!("{} has a zero timeout", case.id));
        }
        if case.suites.is_empty()
            || case
                .suites
                .iter()
                .any(|suite| !matches!(suite.as_str(), "acceptance" | "safety"))
        {
            return Err(format!("{} has invalid suite membership", case.id));
        }
        match case.kind {
            CaseKind::Run => {
                if !case.expected.diagnostics.is_empty() {
                    return Err(format!(
                        "{} has kind run but declares expected diagnostics (that shape is check-only)",
                        case.id
                    ));
                }
                if case.expected.stdout.is_none() || case.expected.exit.is_none() {
                    return Err(format!(
                        "{} has kind run and must declare both expected stdout and exit",
                        case.id
                    ));
                }
            }
            CaseKind::Check => {
                if case.expected.diagnostics.is_empty() {
                    return Err(format!(
                        "{} has kind check but declares no expected diagnostics",
                        case.id
                    ));
                }
                if case.suites.iter().any(|suite| suite == "safety") {
                    return Err(format!(
                        "{} has kind check but belongs to the safety suite; safety stays a suite, not a kind",
                        case.id
                    ));
                }
                if case.expected.stdout.is_some()
                    || case.expected.exit.is_some()
                    || !case.expected.stderr.is_empty()
                {
                    return Err(format!(
                        "{} has kind check but declares run-only expected stdout/stderr/exit",
                        case.id
                    ));
                }
            }
        }
        let source = root.join("tests/core-acceptance").join(&case.source);
        if !source.is_file() {
            return Err(format!(
                "{} source does not exist: {}",
                case.id,
                source.display()
            ));
        }
    }
    Ok(())
}

fn select_cases<'a>(
    manifest: &'a Manifest,
    suite: &str,
    selected_ids: &[String],
) -> Result<Vec<&'a Case>> {
    if !selected_ids.is_empty() {
        let mut selected = Vec::with_capacity(selected_ids.len());
        for selected_id in selected_ids {
            let case = manifest
                .cases
                .iter()
                .find(|case| case.id == *selected_id)
                .ok_or_else(|| format!("unknown core acceptance case: {selected_id}"))?;
            if !case.suites.iter().any(|member| member == suite) {
                return Err(format!(
                    "core acceptance case {selected_id:?} does not belong to suite {suite:?}"
                ));
            }
            selected.push(case);
        }
        return Ok(selected);
    }
    let selected = manifest
        .cases
        .iter()
        .filter(|case| case.suites.iter().any(|member| member == suite))
        .collect::<Vec<_>>();
    if selected.is_empty() {
        return Err(format!("core acceptance suite {suite:?} has no cases"));
    }
    Ok(selected)
}

fn compiler_fingerprint(hew_bin: &Path) -> Result<String> {
    if !hew_bin.is_file() {
        return Err(format!(
            "environment failure: compiler binary not found: {}",
            hew_bin.display()
        ));
    }
    match run_command(
        Command::new(hew_bin).arg("--version"),
        Duration::from_secs(10),
    )? {
        CommandResult::Completed {
            status,
            stdout,
            stderr: _,
        } if status.success() => Ok(stdout.trim().to_string()),
        CommandResult::Completed {
            status,
            stdout,
            stderr,
        } => Err(format!(
            "environment failure: compiler fingerprint exited {:?}: {}{}",
            status.code(),
            summarise(&stdout),
            summarise(&stderr)
        )),
        CommandResult::TimedOut { stdout, stderr } => Err(format!(
            "environment failure: compiler fingerprint timed out: {}{}",
            summarise(&stdout),
            summarise(&stderr)
        )),
    }
}

struct Runner<'a> {
    options: &'a Options,
    root: &'a Path,
    run_dir: &'a Path,
    instrumentation_request: &'a str,
}

impl Runner<'_> {
    fn run_case(&self, case: &Case) -> bool {
        match case.kind {
            CaseKind::Run => {
                let mut passed = true;
                for profile in [Profile::O0, Profile::O2] {
                    if !self.run_profile(case, profile) {
                        passed = false;
                    }
                }
                passed
            }
            CaseKind::Check => self.run_check(case),
        }
    }

    /// Run a `kind = "check"` case once: `hew check --format json` against
    /// the source, no build and no execution. The process must exit 1 and
    /// its stdout must be exactly the JSON diagnostic array the case expects.
    fn run_check(&self, case: &Case) -> bool {
        let source = self.root.join("tests/core-acceptance").join(&case.source);
        let mut command = Command::new(&self.options.hew_bin);
        command
            .arg("check")
            .arg(&source)
            .arg("--format")
            .arg("json")
            .current_dir(self.root);
        let result = match run_command(&mut command, self.timeout(case)) {
            Ok(result) => result,
            Err(err) => {
                println!(
                    "FAIL {} profile=check class=environment-failure detail={err}",
                    case.id
                );
                return false;
            }
        };
        match result {
            CommandResult::TimedOut { stdout, stderr } => {
                println!(
                    "FAIL {} profile=check class=timeout timeout_seconds={}{}{}",
                    case.id,
                    self.timeout(case).as_secs(),
                    summarise(&stdout),
                    summarise(&stderr)
                );
                false
            }
            CommandResult::Completed {
                status,
                stdout,
                stderr,
            } => {
                let Some(actual_exit) = status.code() else {
                    println!(
                        "FAIL {} profile=check class=compiler-crash{}{}",
                        case.id,
                        summarise(&stdout),
                        summarise(&stderr)
                    );
                    return false;
                };
                // A rejecting `check` always exits 1: that is the contract
                // this case kind verifies, not a per-case expectation.
                if actual_exit != 1 {
                    println!(
                        "FAIL {} profile=check class=wrong-exit expected=1 actual={actual_exit}{}{}",
                        case.id,
                        summarise(&stdout),
                        summarise(&stderr)
                    );
                    return false;
                }
                let actual: Vec<ActualDiagnostic> = match serde_json::from_str(&stdout) {
                    Ok(diagnostics) => diagnostics,
                    Err(err) => {
                        println!(
                            "FAIL {} profile=check class=environment-failure detail=parse diagnostics json: {err}{}",
                            case.id,
                            summarise(&stdout)
                        );
                        return false;
                    }
                };
                if let Err(detail) = diagnostics_match(&case.expected.diagnostics, &actual) {
                    println!(
                        "FAIL {} profile=check class=wrong-diagnostics detail={detail}{}",
                        case.id,
                        summarise(&stderr)
                    );
                    return false;
                }
                println!("PASS {} profile=check exit={actual_exit}", case.id);
                true
            }
        }
    }

    fn run_profile(&self, case: &Case, profile: Profile) -> bool {
        let source = self.root.join("tests/core-acceptance").join(&case.source);
        let emit_dir = self.run_dir.join(&case.id).join(profile.label());
        let Some(binary) = self.compile(case, profile, &source, &emit_dir) else {
            return false;
        };
        self.execute(case, profile, &binary)
    }

    fn timeout(&self, case: &Case) -> Duration {
        Duration::from_secs(self.options.timeout_seconds.unwrap_or(case.timeout_seconds))
    }

    fn compile(
        &self,
        case: &Case,
        profile: Profile,
        source: &Path,
        emit_dir: &Path,
    ) -> Option<PathBuf> {
        if let Err(err) = fs::create_dir_all(emit_dir) {
            println!(
                "FAIL {} profile={} class=environment-failure detail=create emit directory: {err}",
                case.id,
                profile.label()
            );
            return None;
        }
        let mut command = Command::new(&self.options.hew_bin);
        command
            .arg("compile")
            .arg("--emit-dir")
            .arg(emit_dir)
            .arg("--opt-level")
            .arg(profile.cli_level())
            .arg(source)
            .current_dir(self.root);
        if self.instrumentation_request == "address" {
            command.arg("--emit-llvm").env("HEW_SANITIZE_ADDRESS", "1");
            configure_safety_environment(&mut command);
        } else {
            command.env_remove("HEW_SANITIZE_ADDRESS");
        }
        let result = match run_command(&mut command, self.timeout(case)) {
            Ok(result) => result,
            Err(err) => {
                println!(
                    "FAIL {} profile={} class=environment-failure detail={err}",
                    case.id,
                    profile.label()
                );
                return None;
            }
        };
        match result {
            CommandResult::TimedOut { stdout, stderr } => {
                println!(
                    "FAIL {} profile={} class=timeout phase=compile timeout_seconds={}{}{}",
                    case.id,
                    profile.label(),
                    self.timeout(case).as_secs(),
                    summarise(&stdout),
                    summarise(&stderr)
                );
                None
            }
            CommandResult::Completed {
                status,
                stdout,
                stderr,
            } if !status.success() => {
                let class = if status.code().is_some() {
                    "source-diagnostic"
                } else {
                    "compiler-crash"
                };
                println!(
                    "FAIL {} profile={} class={} exit={:?}{}{}",
                    case.id,
                    profile.label(),
                    class,
                    status.code(),
                    summarise(&stdout),
                    summarise(&stderr)
                );
                None
            }
            CommandResult::Completed { .. } => {
                let binary = executable_path(emit_dir, source);
                if self.instrumentation_request == "address" {
                    if let Err(error) = verify_address_instrumentation(&binary.with_extension("ll"))
                    {
                        println!(
                            "FAIL {} profile={} class=environment-failure detail={error}",
                            case.id,
                            profile.label()
                        );
                        return None;
                    }
                }
                if binary.is_file() {
                    Some(binary)
                } else {
                    println!(
                        "FAIL {} profile={} class=environment-failure detail=compiler returned success without {}",
                        case.id,
                        profile.label(),
                        binary.display()
                    );
                    None
                }
            }
        }
    }

    fn prepare_inputs(&self, case: &Case, profile: Profile) -> Result<PathBuf> {
        let Some(fixtures) = &case.fixtures else {
            return Ok(self.root.to_path_buf());
        };
        let source = self.root.join("tests/core-acceptance").join(fixtures);
        let destination = self
            .run_dir
            .join(&case.id)
            .join(profile.label())
            .join("inputs");
        fs::create_dir_all(&destination)
            .map_err(|err| format!("create fixture directory: {err}"))?;
        for entry in fs::read_dir(&source)
            .map_err(|err| format!("read fixture directory {}: {err}", source.display()))?
        {
            let entry = entry.map_err(|err| format!("read fixture entry: {err}"))?;
            if !entry
                .file_type()
                .map_err(|err| format!("inspect fixture: {err}"))?
                .is_file()
            {
                return Err(format!(
                    "fixture must be a regular file: {}",
                    entry.path().display()
                ));
            }
            fs::copy(entry.path(), destination.join(entry.file_name()))
                .map_err(|err| format!("copy fixture {}: {err}", entry.path().display()))?;
        }
        Ok(destination)
    }

    fn execute(&self, case: &Case, profile: Profile, binary: &Path) -> bool {
        let mut command = Command::new(binary);
        let working_dir = match self.prepare_inputs(case, profile) {
            Ok(directory) => directory,
            Err(err) => {
                println!(
                    "FAIL {} profile={} class=environment-failure detail={err}",
                    case.id,
                    profile.label()
                );
                return false;
            }
        };
        command.current_dir(working_dir);
        if self.instrumentation_request == "address" {
            configure_safety_environment(&mut command);
        }
        let executed = match run_command(&mut command, self.timeout(case)) {
            Ok(result) => result,
            Err(err) => {
                println!(
                    "FAIL {} profile={} class=environment-failure detail={err}",
                    case.id,
                    profile.label()
                );
                return false;
            }
        };
        match executed {
            CommandResult::TimedOut { stdout, stderr } => {
                println!(
                    "FAIL {} profile={} class=timeout phase=run timeout_seconds={}{}{}",
                    case.id,
                    profile.label(),
                    self.timeout(case).as_secs(),
                    summarise(&stdout),
                    summarise(&stderr)
                );
                false
            }
            CommandResult::Completed {
                status,
                stdout,
                stderr,
            } if status.code().is_none() => {
                println!(
                    "FAIL {} profile={} class=program-crash{}{}",
                    case.id,
                    profile.label(),
                    summarise(&stdout),
                    summarise(&stderr)
                );
                false
            }
            CommandResult::Completed {
                status,
                stdout,
                stderr,
            } => {
                let actual_exit = status.code().expect("checked above");
                let (expected_stdout, expected_exit) = case.expected.run_expectation();
                if actual_exit != expected_exit {
                    println!(
                        "FAIL {} profile={} class=wrong-exit expected={} actual={}{}{}",
                        case.id,
                        profile.label(),
                        expected_exit,
                        actual_exit,
                        summarise(&stdout),
                        summarise(&stderr)
                    );
                    return false;
                }
                if stdout != expected_stdout || stderr != case.expected.stderr {
                    println!(
                        "FAIL {} profile={} class=wrong-output expected={:?} actual={:?}{}",
                        case.id,
                        profile.label(),
                        expected_stdout,
                        stdout,
                        summarise(&stderr)
                    );
                    return false;
                }
                println!(
                    "PASS {} profile={} exit={} instrumentation-requested={}",
                    case.id,
                    profile.label(),
                    actual_exit,
                    self.instrumentation_request
                );
                true
            }
        }
    }
}

fn executable_path(emit_dir: &Path, source: &Path) -> PathBuf {
    let stem = source
        .file_stem()
        .expect("manifest source has a filename")
        .to_string_lossy();
    let name = if cfg!(windows) {
        format!("{stem}.exe")
    } else {
        stem.into_owned()
    };
    emit_dir.join(name)
}

fn verify_address_instrumentation(path: &Path) -> Result<()> {
    let ir = fs::read_to_string(path).map_err(|error| {
        format!(
            "environment failure: cannot read generated safety IR {}: {error}",
            path.display()
        )
    })?;
    if !ir.contains("sanitize_address") || !ir.contains("call void @__asan_init()") {
        return Err(format!(
            "environment failure: generated IR has no AddressSanitizer instrumentation: {}",
            path.display()
        ));
    }
    Ok(())
}

fn configure_safety_environment(command: &mut Command) {
    command
        .env(
            "ASAN_OPTIONS",
            "detect_leaks=1:use_sigaltstack=0:halt_on_error=1:exitcode=99",
        )
        .env("LSAN_OPTIONS", "exitcode=99");
}

fn run_command(command: &mut Command, timeout: Duration) -> Result<CommandResult> {
    let output_dir =
        tempfile::tempdir().map_err(|err| format!("create command output directory: {err}"))?;
    let stdout_path = output_dir.path().join("stdout");
    let stderr_path = output_dir.path().join("stderr");
    let stdout_file = fs::File::create(&stdout_path)
        .map_err(|err| format!("create command stdout capture: {err}"))?;
    let stderr_file = fs::File::create(&stderr_path)
        .map_err(|err| format!("create command stderr capture: {err}"))?;
    command.stdout(Stdio::from(stdout_file));
    command.stderr(Stdio::from(stderr_file));
    command.stdin(Stdio::null());
    let mut child = command
        .spawn()
        .map_err(|err| format!("start command: {err}"))?;
    let started = Instant::now();
    let status = loop {
        if let Some(status) = child
            .try_wait()
            .map_err(|err| format!("wait for command: {err}"))?
        {
            break Some(status);
        }
        if started.elapsed() >= timeout {
            child
                .kill()
                .map_err(|err| format!("kill timed out command: {err}"))?;
            child
                .wait()
                .map_err(|err| format!("wait for timed out command: {err}"))?;
            break None;
        }
        thread::sleep(Duration::from_millis(10));
    };
    let stdout = read_capture(&stdout_path, "stdout")?;
    let stderr = read_capture(&stderr_path, "stderr")?;
    Ok(match status {
        Some(status) => CommandResult::Completed {
            status,
            stdout,
            stderr,
        },
        None => CommandResult::TimedOut { stdout, stderr },
    })
}

fn read_capture(path: &Path, stream: &str) -> Result<String> {
    fs::read_to_string(path)
        .map_err(|err| format!("read command {stream} capture {}: {err}", path.display()))
}

/// The slice of `hew check --format json`'s `JsonDiagnostic` this runner
/// needs. Serde ignores the rest of the object (severity, channel, source,
/// file, notes, fixes) — the JSON diagnostics array is the one authority,
/// read directly, never re-derived from the text renderer.
#[derive(Debug, Deserialize)]
struct ActualDiagnostic {
    code: String,
    span: ActualSpan,
    message: String,
}

#[derive(Debug, Deserialize)]
struct ActualSpan {
    start_line: usize,
    start_col: usize,
}

/// Match a case's expected diagnostics against what `hew check` actually
/// reported, as a set keyed on `(code, line, column)` with an optional
/// message substring per match. Extra or missing diagnostics both fail: a
/// `check` case proves the exact diagnostic set, not a subset of it.
fn diagnostics_match(expected: &[ExpectedDiagnostic], actual: &[ActualDiagnostic]) -> Result<()> {
    let mut remaining: Vec<&ActualDiagnostic> = actual.iter().collect();
    let mut missing = Vec::new();
    for want in expected {
        let position = remaining.iter().position(|got| {
            got.code == want.code
                && got.span.start_line == want.line
                && got.span.start_col == want.column
                && match &want.message {
                    Some(substring) => got.message.contains(substring.as_str()),
                    None => true,
                }
        });
        match position {
            Some(index) => {
                remaining.remove(index);
            }
            None => missing.push(format!(
                "{}@{}:{}{}",
                want.code,
                want.line,
                want.column,
                want.message
                    .as_deref()
                    .map(|m| format!(" ({m:?})"))
                    .unwrap_or_default()
            )),
        }
    }
    if missing.is_empty() && remaining.is_empty() {
        return Ok(());
    }
    let extra: Vec<String> = remaining
        .iter()
        .map(|got| {
            format!(
                "{}@{}:{} ({:?})",
                got.code, got.span.start_line, got.span.start_col, got.message
            )
        })
        .collect();
    Err(format!("missing={missing:?} extra={extra:?}"))
}

fn summarise(text: &str) -> String {
    if text.is_empty() {
        String::new()
    } else {
        let limit = 2_000;
        let mut end = text.len().min(limit);
        while !text.is_char_boundary(end) {
            end -= 1;
        }
        let truncated = &text[..end];
        format!(" output={truncated:?}")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn manifest() -> Manifest {
        toml::from_str(
            r#"
            [[case]]
            id = "acceptance-case"
            intent = "selection test"
            source = "cases/acceptance-case.hew"
            suites = ["acceptance"]
            timeout_seconds = 1
            [case.expected]
            stdout = ""
            exit = 0
            [[case]]
            id = "safety-case"
            intent = "selection test"
            source = "cases/safety-case.hew"
            suites = ["safety"]
            timeout_seconds = 1
            [case.expected]
            stdout = ""
            exit = 0
            "#,
        )
        .expect("test manifest parses")
    }

    #[test]
    fn repeated_case_flags_select_each_case_in_order() {
        let mut manifest = manifest();
        manifest.cases[1].suites = vec!["acceptance".into()];
        let selected = select_cases(
            &manifest,
            "acceptance",
            &["safety-case".to_string(), "acceptance-case".to_string()],
        )
        .expect("both cases belong to the acceptance suite");
        let selected_ids: Vec<&str> = selected.iter().map(|case| case.id.as_str()).collect();
        assert_eq!(selected_ids, ["safety-case", "acceptance-case"]);
    }

    #[test]
    fn selected_case_must_exist() {
        let error = select_cases(&manifest(), "acceptance", &["missing".to_string()])
            .expect_err("unknown focused case must fail rather than silently running a suite");
        assert!(error.contains("unknown core acceptance case"));
    }

    #[test]
    fn profile_inputs_are_isolated_and_missing_inputs_fail() {
        let directory = tempfile::tempdir().unwrap();
        let fixture = directory.path().join("tests/core-acceptance/fixtures");
        fs::create_dir_all(&fixture).unwrap();
        let bytes = b"a\0b\xff";
        fs::write(fixture.join("input.txt"), bytes).unwrap();
        let mut case = manifest().cases.remove(0);
        case.fixtures = Some("fixtures".into());
        let options = Options {
            suite: "acceptance".into(),
            cases: Vec::new(),
            hew_bin: directory.path().join("hew"),
            timeout_seconds: None,
        };
        let runner = Runner {
            options: &options,
            root: directory.path(),
            run_dir: directory.path(),
            instrumentation_request: "none",
        };
        let first = runner.prepare_inputs(&case, Profile::O0).unwrap();
        assert_eq!(fs::read(first.join("input.txt")).unwrap(), bytes);
        fs::write(first.join("input.txt"), "changed by program").unwrap();
        let second = runner.prepare_inputs(&case, Profile::O2).unwrap();
        assert_eq!(fs::read(second.join("input.txt")).unwrap(), bytes);
        assert_eq!(fs::read(fixture.join("input.txt")).unwrap(), bytes);
        case.fixtures = Some("missing".into());
        assert!(runner.prepare_inputs(&case, Profile::O0).is_err());
    }

    #[test]
    fn safety_requires_instrumented_ir_not_just_an_executable() {
        let directory = tempfile::tempdir().unwrap();
        let path = directory.path().join("program.ll");
        assert!(verify_address_instrumentation(&path).is_err());
        fs::write(&path, "define i32 @main() { ret i32 0 }").unwrap();
        assert!(verify_address_instrumentation(&path).is_err());
        fs::write(
            &path,
            "define void @body() sanitize_address { ret void }\n\
             declare void @__asan_init()\n\
             define void @asan.module_ctor() { call void @__asan_init()\nret void }",
        )
        .unwrap();
        verify_address_instrumentation(&path).unwrap();
    }

    #[cfg(unix)]
    #[test]
    fn sanitizer_report_fails_even_when_exit_matches_the_program() {
        use std::os::unix::fs::PermissionsExt;
        let directory = tempfile::tempdir().unwrap();
        let binary = directory.path().join("report");
        fs::write(
            &binary,
            "#!/bin/sh\nprintf 'ERROR: LeakSanitizer: detected memory leaks\\n' >&2\nexit 23\n",
        )
        .unwrap();
        fs::set_permissions(&binary, fs::Permissions::from_mode(0o700)).unwrap();
        let mut case = manifest().cases.remove(0);
        case.expected.exit = Some(23);
        let options = Options {
            suite: "safety".to_string(),
            cases: Vec::new(),
            hew_bin: binary.clone(),
            timeout_seconds: None,
        };
        let runner = Runner {
            options: &options,
            root: directory.path(),
            run_dir: directory.path(),
            instrumentation_request: "address",
        };
        assert!(!runner.execute(&case, Profile::O0, &binary));
    }

    #[test]
    fn a_case_can_require_both_native_acceptance_and_safety() {
        let manifest: Manifest = toml::from_str(
            r#"[[case]]
            id = "owned"
            intent = "independent value copy"
            source = "owned.hew"
            suites = ["acceptance", "safety"]
            timeout_seconds = 1
            [case.expected]
            stdout = ""
            exit = 0
            "#,
        )
        .unwrap();
        for suite in ["acceptance", "safety"] {
            assert_eq!(select_cases(&manifest, suite, &[]).unwrap()[0].id, "owned");
        }
    }

    #[test]
    fn focused_case_cannot_replace_the_requested_suite() {
        let manifest = manifest();
        let error = select_cases(&manifest, "safety", &["acceptance-case".to_string()])
            .expect_err("ordinary execution cannot substitute for safety validation");
        assert!(error.contains("does not belong to suite"));
        let selected = select_cases(&manifest, "safety", &["safety-case".to_string()])
            .expect("a focused case within its suite remains selectable");
        assert_eq!(selected[0].id, "safety-case");
    }

    #[test]
    fn unicode_output_summary_never_slices_a_character() {
        let text = format!("{}étail", "x".repeat(1_999));
        assert_eq!(summarise(&text), format!(" output={:?}", "x".repeat(1_999)));
    }

    fn write_case(dir: &Path, file_name: &str, id: &str) {
        let cases_dir = dir.join("tests/core-acceptance/cases");
        fs::create_dir_all(&cases_dir).unwrap();
        fs::write(
            cases_dir.join(file_name),
            format!(
                r#"[[case]]
                id = "{id}"
                intent = "selection test"
                source = "cases/{id}.hew"
                suites = ["acceptance"]
                timeout_seconds = 1
                [case.expected]
                stdout = ""
                exit = 0
                "#
            ),
        )
        .unwrap();
    }

    #[test]
    fn case_id_must_match_its_file_name() {
        let directory = tempfile::tempdir().unwrap();
        write_case(directory.path(), "wrong-name.toml", "actual-id");
        let error =
            load_manifest(directory.path()).expect_err("mismatched id and file name must fail");
        assert!(error.contains("does not match its file name"));
    }

    #[test]
    fn cases_load_in_sorted_file_order() {
        let directory = tempfile::tempdir().unwrap();
        write_case(directory.path(), "b-case.toml", "b-case");
        write_case(directory.path(), "a-case.toml", "a-case");
        let manifest = load_manifest(directory.path()).expect("both cases load");
        let ids: Vec<&str> = manifest.cases.iter().map(|case| case.id.as_str()).collect();
        assert_eq!(ids, ["a-case", "b-case"]);
    }

    #[test]
    fn missing_capture_is_an_environment_error() {
        let directory = tempfile::tempdir().expect("temporary directory");
        let error = read_capture(&directory.path().join("missing"), "stdout")
            .expect_err("missing command output must not masquerade as program output");
        assert!(error.contains("read command stdout capture"));
    }

    // -----------------------------------------------------------------
    // Expectation kind: `check`
    // -----------------------------------------------------------------

    fn make_case(
        kind: CaseKind,
        diagnostics: Vec<ExpectedDiagnostic>,
        suites: &[&str],
        source_rel: &str,
    ) -> Case {
        Case {
            id: "case-under-test".to_string(),
            intent: "unit test".to_string(),
            source: PathBuf::from(source_rel),
            fixtures: None,
            suites: suites.iter().map(ToString::to_string).collect(),
            timeout_seconds: 1,
            kind,
            expected: ExpectedOutcome {
                stdout: None,
                stderr: String::new(),
                exit: None,
                diagnostics,
            },
        }
    }

    fn manifest_root_with_source(source_rel: &str) -> tempfile::TempDir {
        let directory = tempfile::tempdir().expect("temporary directory");
        let source = directory
            .path()
            .join("tests/core-acceptance")
            .join(source_rel);
        fs::create_dir_all(source.parent().expect("source has a parent")).unwrap();
        fs::write(&source, "fn main() {}\n").unwrap();
        directory
    }

    #[test]
    fn case_without_kind_defaults_to_run() {
        assert_eq!(manifest().cases[0].kind, CaseKind::Run);
    }

    #[test]
    fn run_case_rejects_declared_expected_diagnostics() {
        let directory = manifest_root_with_source("cases/case.hew");
        let case = make_case(
            CaseKind::Run,
            vec![ExpectedDiagnostic {
                code: "InvalidOperation".to_string(),
                line: 1,
                column: 1,
                message: None,
            }],
            &["acceptance"],
            "cases/case.hew",
        );
        let manifest = Manifest { cases: vec![case] };
        let error = validate_manifest(&manifest, directory.path())
            .expect_err("a run case declaring diagnostics is a check-only shape");
        assert!(error.contains("check-only"));
    }

    #[test]
    fn run_case_requires_expected_stdout_and_exit() {
        // `stdout`/`exit` are `Option`, not default-empty/default-zero:
        // a run case that forgets one must fail validation rather than
        // silently comparing against "" / 0 and passing when the program
        // happens to match by coincidence.
        let directory = manifest_root_with_source("cases/case.hew");
        let case = make_case(CaseKind::Run, Vec::new(), &["acceptance"], "cases/case.hew");
        let manifest = Manifest { cases: vec![case] };
        let error = validate_manifest(&manifest, directory.path())
            .expect_err("a run case missing stdout/exit must fail validation");
        assert!(error.contains("must declare both expected stdout and exit"));
    }

    #[test]
    fn check_case_requires_expected_diagnostics() {
        let directory = manifest_root_with_source("cases/case.hew");
        let case = make_case(
            CaseKind::Check,
            Vec::new(),
            &["acceptance"],
            "cases/case.hew",
        );
        let manifest = Manifest { cases: vec![case] };
        let error = validate_manifest(&manifest, directory.path())
            .expect_err("a check case with no expected diagnostics must fail validation");
        assert!(error.contains("declares no expected diagnostics"));
    }

    #[test]
    fn check_case_cannot_join_the_safety_suite() {
        let directory = manifest_root_with_source("cases/case.hew");
        let case = make_case(
            CaseKind::Check,
            vec![ExpectedDiagnostic {
                code: "InvalidOperation".to_string(),
                line: 1,
                column: 1,
                message: None,
            }],
            &["acceptance", "safety"],
            "cases/case.hew",
        );
        let manifest = Manifest { cases: vec![case] };
        let error = validate_manifest(&manifest, directory.path())
            .expect_err("safety stays a suite, not a case kind");
        assert!(error.contains("safety stays a suite"));
    }

    #[test]
    fn diagnostics_match_accepts_an_exact_set() {
        let actual = vec![ActualDiagnostic {
            code: "InvalidOperation".to_string(),
            span: ActualSpan {
                start_line: 10,
                start_col: 9,
            },
            message: "a select needs at least one arm: a source arm".to_string(),
        }];
        let expected = vec![ExpectedDiagnostic {
            code: "InvalidOperation".to_string(),
            line: 10,
            column: 9,
            message: Some("needs at least one arm".to_string()),
        }];
        assert!(diagnostics_match(&expected, &actual).is_ok());
    }

    #[test]
    fn diagnostics_match_rejects_wrong_position() {
        let actual = vec![ActualDiagnostic {
            code: "InvalidOperation".to_string(),
            span: ActualSpan {
                start_line: 10,
                start_col: 9,
            },
            message: "a select needs at least one arm".to_string(),
        }];
        let wrong_line = vec![ExpectedDiagnostic {
            code: "InvalidOperation".to_string(),
            line: 11,
            column: 9,
            message: None,
        }];
        let error = diagnostics_match(&wrong_line, &actual)
            .expect_err("naming the wrong line must fail rather than silently pass");
        assert!(error.contains("missing"));
        assert!(error.contains("extra"));
    }

    #[test]
    fn diagnostics_match_rejects_an_unexpected_extra_diagnostic() {
        let actual = vec![ActualDiagnostic {
            code: "UnusedVariable".to_string(),
            span: ActualSpan {
                start_line: 4,
                start_col: 13,
            },
            message: "unused variable seen".to_string(),
        }];
        let error = diagnostics_match(&[], &actual)
            .expect_err("an undeclared extra diagnostic must fail the case");
        assert!(error.contains("extra"));
    }

    #[cfg(unix)]
    #[test]
    fn check_case_with_a_wrong_position_fails_the_runner() {
        use std::os::unix::fs::PermissionsExt;
        let directory = manifest_root_with_source("cases/probe.hew");
        let fake_hew = directory.path().join("fake-hew");
        let script = concat!(
            "#!/bin/sh\n",
            "cat <<'JSON'\n",
            "[{",
            r#""code":"InvalidOperation","severity":"error","channel":"user","#,
            r#""source":"hew-types","file":"probe.hew","#,
            r#""span":{"start_line":10,"start_col":9,"end_line":10,"end_col":18,"start_byte":0,"end_byte":0},"#,
            r#""message":"a select needs at least one arm","notes":[],"fixes":[]"#,
            "}]\n",
            "JSON\n",
            "exit 1\n",
        );
        fs::write(&fake_hew, script).unwrap();
        fs::set_permissions(&fake_hew, fs::Permissions::from_mode(0o700)).unwrap();

        let options = Options {
            suite: "acceptance".to_string(),
            cases: Vec::new(),
            hew_bin: fake_hew,
            timeout_seconds: None,
        };
        let runner = Runner {
            options: &options,
            root: directory.path(),
            run_dir: directory.path(),
            instrumentation_request: "none",
        };

        let correct_case = make_case(
            CaseKind::Check,
            vec![ExpectedDiagnostic {
                code: "InvalidOperation".to_string(),
                line: 10,
                column: 9,
                message: None,
            }],
            &["acceptance"],
            "cases/probe.hew",
        );
        assert!(
            runner.run_check(&correct_case),
            "a check case whose expectation matches the real diagnostic position must pass"
        );

        let wrong_position_case = make_case(
            CaseKind::Check,
            vec![ExpectedDiagnostic {
                code: "InvalidOperation".to_string(),
                line: 99,
                column: 9,
                message: None,
            }],
            &["acceptance"],
            "cases/probe.hew",
        );
        assert!(
            !runner.run_check(&wrong_position_case),
            "a check case naming the wrong position must fail the runner, not pass it"
        );
    }
}
