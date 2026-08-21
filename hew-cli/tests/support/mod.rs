#![allow(
    dead_code,
    reason = "shared integration-test helpers are not used by every test target"
)]

pub mod leak_slope;

use hew_testutil::{BoundedExecError, DEFAULT_EXEC_TIMEOUT};
use std::ffi::OsStr;
use std::path::{Path, PathBuf};
use std::process::{Command, Output};
use std::sync::OnceLock;

static CODEGEN_STATUS: OnceLock<Result<(), String>> = OnceLock::new();
static WASI_RUNNER_STATUS: OnceLock<Result<(), String>> = OnceLock::new();

const WASI_STDLIB_ARCHIVES: &[(&str, &str)] = &[("hew-std", "libhew_std.a")];

pub fn repo_root() -> &'static Path {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("hew-cli crate should live under the repo root")
}

pub fn hew_binary() -> PathBuf {
    PathBuf::from(env!("CARGO_BIN_EXE_hew"))
}

pub fn require_codegen() {
    if let Err(error) = CODEGEN_STATUS.get_or_init(bootstrap_codegen) {
        panic!("{error}");
    }
}

/// Panics when the WASI toolchain or the `wasmtime` runtime is unavailable.
///
/// There is deliberately no non-panicking variant. A `try_require_wasi_runner`
/// used to exist and returned `false` with a stderr SKIP notice; the callers
/// early-returned green having asserted nothing, so on a runner without
/// wasmtime — which is what the Windows job was until it was given one — those
/// tests reported success while executing no WASM at all. A missing runner is a
/// provisioning failure, and it must be reported as a failure.
///
/// A caller that genuinely wants to skip on an unsupported host must say so at
/// its own site, where the condition is visible and reviewable, rather than
/// inheriting it from a shared helper.
pub fn require_wasi_runner() {
    if let Err(error) = WASI_RUNNER_STATUS.get_or_init(bootstrap_wasi_runner) {
        panic!("{error}");
    }
}

fn bootstrap_codegen() -> Result<(), String> {
    hew_testutil::ensure_hew_lib_built().map(|_lib_path| ())
}

fn bootstrap_wasi_runner() -> Result<(), String> {
    if find_wasmtime().is_none() {
        return Err(
            "failed to bootstrap WASI runner prerequisites: `wasmtime` not found \
             (checked PATH and ~/.wasmtime/bin)"
                .to_string(),
        );
    }

    // Every wasm32-wasip1 staticlib is a shared-target artifact that `wasm-ld`
    // reads while a sibling nextest process could be rewriting it. The build
    // authority is the single writer; this suite only names what it needs.
    hew_testutil::ensure_wasm_staticlib_built(
        "hew-runtime",
        "libhew_runtime.a",
        &["--no-default-features"],
    )
    .map_err(|error| format!("failed to bootstrap WASI runner runtime: {error}"))?;

    for (package, archive) in WASI_STDLIB_ARCHIVES {
        hew_testutil::ensure_wasm_staticlib_built(package, archive, &[]).map_err(|error| {
            format!("failed to bootstrap WASI stdlib archive {archive}: {error}")
        })?;
    }

    Ok(())
}

fn target_dir() -> Result<PathBuf, String> {
    hew_binary()
        .parent()
        .and_then(Path::parent)
        .map(Path::to_path_buf)
        .ok_or_else(|| {
            format!(
                "hew binary path {} is not under a Cargo target directory",
                hew_binary().display()
            )
        })
}

fn build_profile() -> &'static str {
    match hew_binary()
        .parent()
        .and_then(|dir| dir.file_name())
        .and_then(|name| name.to_str())
    {
        Some("release") => "release",
        _ => "debug",
    }
}

pub fn tempdir() -> tempfile::TempDir {
    tempfile::tempdir().expect("create hew-cli tempdir")
}

pub fn hew_command() -> Command {
    Command::new(hew_binary())
}

pub fn run_hew(args: &[&str]) -> Output {
    hew_command()
        .args(args)
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .output()
        .expect("failed to spawn hew binary")
}

pub fn run_bounded_command(command: Command, label: impl Into<String>) -> Output {
    try_run_bounded_command(command, label, DEFAULT_EXEC_TIMEOUT)
        .unwrap_or_else(|error| panic!("{error}"))
}

pub fn try_run_bounded_command(
    mut command: Command,
    label: impl Into<String>,
    timeout: std::time::Duration,
) -> Result<Output, BoundedExecError> {
    hew_testutil::run_command_bounded(&mut command, label, timeout)
}

pub fn run_bounded_command_with_stdin(
    mut command: Command,
    label: impl Into<String>,
    stdin: &[u8],
) -> Output {
    hew_testutil::run_command_bounded_with_stdin(&mut command, label, DEFAULT_EXEC_TIMEOUT, stdin)
        .unwrap_or_else(|error| panic!("{error}"))
}

pub fn run_bounded_hew_run(source: &Path, current_dir: &Path) -> Output {
    let mut command = hew_command();
    command.arg("run").arg(source).current_dir(current_dir);
    run_bounded_command(command, format!("hew run {}", source.display()))
}

pub fn bounded_hew_command<I, S>(args: I, current_dir: &Path, label: impl Into<String>) -> Output
where
    I: IntoIterator<Item = S>,
    S: AsRef<OsStr>,
{
    let mut command = hew_command();
    command.args(args).current_dir(current_dir);
    run_bounded_command(command, label)
}

pub fn run_hew_in(current_dir: &Path, args: &[&str]) -> Output {
    hew_command()
        .args(args)
        .current_dir(current_dir)
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .output()
        .expect("failed to spawn hew binary")
}

pub fn assert_success(output: &Output, context: &str) {
    assert!(
        output.status.success(),
        "{context}\n{}",
        describe_output(output)
    );
}

pub fn describe_output(output: &Output) -> String {
    format!(
        "stdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    )
}

fn tool_available(name: &str) -> bool {
    Command::new(name)
        .arg("--version")
        .output()
        .is_ok_and(|output| output.status.success())
}

fn find_wasmtime() -> Option<PathBuf> {
    if tool_available("wasmtime") {
        return Some(PathBuf::from("wasmtime"));
    }

    let binary_name = format!("wasmtime{}", std::env::consts::EXE_SUFFIX);
    [std::env::var_os("HOME"), std::env::var_os("USERPROFILE")]
        .into_iter()
        .flatten()
        .map(PathBuf::from)
        .map(|home| home.join(".wasmtime").join("bin").join(&binary_name))
        .find(|candidate| candidate.exists())
}

pub fn strip_ansi(input: &str) -> String {
    let mut out = String::with_capacity(input.len());
    let mut chars = input.chars().peekable();
    while let Some(ch) = chars.next() {
        if ch == '\u{1b}' && chars.next_if_eq(&'[').is_some() {
            for next in chars.by_ref() {
                if ('@'..='~').contains(&next) {
                    break;
                }
            }
            continue;
        }
        out.push(ch);
    }
    out
}
