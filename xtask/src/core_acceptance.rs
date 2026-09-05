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
    suite: String,
    host_capabilities: Vec<String>,
    timeout_seconds: u64,
    expected: ExpectedOutcome,
}

#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
struct ExpectedOutcome {
    stdout: String,
    exit: i32,
}

#[derive(Debug)]
struct Options {
    suite: String,
    case: Option<String>,
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
    let selected = select_cases(&manifest, &options.suite, options.case.as_deref())?;
    let fingerprint = compiler_fingerprint(&options.hew_bin)?;
    let instrumentation_request = "none";

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
    let mut case = None;
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
                case = Some(required_value(args, &mut index, "--case")?.to_string());
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

    if !matches!(suite.as_str(), "acceptance" | "safety" | "native-smoke") {
        return Err(format!(
            "unknown core-acceptance suite {suite:?}; expected acceptance, safety, or native-smoke"
        ));
    }
    if timeout_seconds == Some(0) {
        return Err("--timeout-seconds must be positive".to_string());
    }
    Ok(Options {
        suite,
        case,
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
        "  --suite acceptance|safety|native-smoke  select a manifest suite (default: acceptance)",
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
    let path = root.join("tests/core-acceptance/manifest.toml");
    let contents = fs::read_to_string(&path)
        .map_err(|err| format!("read core acceptance manifest {}: {err}", path.display()))?;
    toml::from_str(&contents).map_err(|err| format!("parse core acceptance manifest: {err}"))
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
        if !case
            .host_capabilities
            .iter()
            .any(|capability| capability == "native")
        {
            return Err(format!(
                "{} does not declare the native host capability",
                case.id
            ));
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
    selected_id: Option<&str>,
) -> Result<Vec<&'a Case>> {
    if let Some(selected_id) = selected_id {
        let case = manifest
            .cases
            .iter()
            .find(|case| case.id == selected_id)
            .ok_or_else(|| format!("unknown core acceptance case: {selected_id}"))?;
        if case.suite != suite {
            return Err(format!(
                "core acceptance case {selected_id:?} does not belong to suite {suite:?}"
            ));
        }
        return Ok(vec![case]);
    }
    let selected = manifest
        .cases
        .iter()
        .filter(|case| case.suite == suite)
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
        let mut passed = true;
        for profile in [Profile::O0, Profile::O2] {
            if !self.run_profile(case, profile) {
                passed = false;
            }
        }
        passed
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

    fn execute(&self, case: &Case, profile: Profile, binary: &Path) -> bool {
        let mut command = Command::new(binary);
        command.current_dir(self.root);
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
                if actual_exit != case.expected.exit {
                    println!(
                        "FAIL {} profile={} class=wrong-exit expected={} actual={}{}{}",
                        case.id,
                        profile.label(),
                        case.expected.exit,
                        actual_exit,
                        summarise(&stdout),
                        summarise(&stderr)
                    );
                    return false;
                }
                if stdout != case.expected.stdout {
                    println!(
                        "FAIL {} profile={} class=wrong-output expected={:?} actual={:?}{}",
                        case.id,
                        profile.label(),
                        case.expected.stdout,
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
            suite = "acceptance"
            host_capabilities = ["native"]
            timeout_seconds = 1
            [case.expected]
            stdout = ""
            exit = 0
            [[case]]
            id = "safety-case"
            intent = "selection test"
            source = "cases/safety-case.hew"
            suite = "safety"
            host_capabilities = ["native"]
            timeout_seconds = 1
            [case.expected]
            stdout = ""
            exit = 0
            "#,
        )
        .expect("test manifest parses")
    }

    #[test]
    fn selected_case_must_exist() {
        let error = select_cases(&manifest(), "acceptance", Some("missing"))
            .expect_err("unknown focused case must fail rather than silently running a suite");
        assert!(error.contains("unknown core acceptance case"));
    }

    #[test]
    fn focused_case_cannot_replace_the_requested_suite() {
        let manifest = manifest();
        let error = select_cases(&manifest, "safety", Some("acceptance-case"))
            .expect_err("ordinary execution cannot substitute for safety validation");
        assert!(error.contains("does not belong to suite"));
        let selected = select_cases(&manifest, "safety", Some("safety-case"))
            .expect("a focused case within its suite remains selectable");
        assert_eq!(selected[0].id, "safety-case");
    }

    #[test]
    fn unicode_output_summary_never_slices_a_character() {
        let text = format!("{}étail", "x".repeat(1_999));
        assert_eq!(summarise(&text), format!(" output={:?}", "x".repeat(1_999)));
    }

    #[test]
    fn missing_capture_is_an_environment_error() {
        let directory = tempfile::tempdir().expect("temporary directory");
        let error = read_capture(&directory.path().join("missing"), "stdout")
            .expect_err("missing command output must not masquerade as program output");
        assert!(error.contains("read command stdout capture"));
    }
}
