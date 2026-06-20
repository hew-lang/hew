#![allow(
    dead_code,
    reason = "shared integration-test helpers are not used by every test target"
)]

use fd_lock::RwLock;
use hew_testutil::{BoundedExecError, DEFAULT_EXEC_TIMEOUT};
use std::ffi::OsStr;
use std::fs::{self, OpenOptions};
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

pub fn require_wasi_runner() {
    if let Err(error) = WASI_RUNNER_STATUS.get_or_init(bootstrap_wasi_runner) {
        panic!("{error}");
    }
}

/// Non-panicking variant of [`require_wasi_runner`].
///
/// Returns `false` (and emits a skip notice to stderr) when the WASI
/// toolchain or `wasmtime` runtime is unavailable, allowing tests to
/// early-return cleanly instead of panicking. Returns `true` when the
/// bootstrap succeeds and the test should proceed.
pub fn try_require_wasi_runner() -> bool {
    match WASI_RUNNER_STATUS.get_or_init(bootstrap_wasi_runner) {
        Ok(()) => true,
        Err(error) => {
            eprintln!("SKIP: WASI runner unavailable — {error}");
            false
        }
    }
}

fn bootstrap_codegen() -> Result<(), String> {
    let target_dir = target_dir()?;
    fs::create_dir_all(&target_dir).map_err(|error| {
        format!(
            "failed to create target dir {}: {error}",
            target_dir.display()
        )
    })?;

    let build_profile = build_profile();
    let lock_path = target_dir.join("hew-cli-codegen-bootstrap.lock");
    let stamp_path = target_dir.join(format!("hew-cli-codegen-bootstrap-{build_profile}.stamp"));
    let run_id =
        std::env::var("NEXTEST_RUN_ID").unwrap_or_else(|_| format!("pid:{}", std::process::id()));

    let lock_file = OpenOptions::new()
        .create(true)
        .read(true)
        .write(true)
        .truncate(false)
        .open(&lock_path)
        .map_err(|error| {
            format!(
                "failed to open codegen bootstrap lock {}: {error}",
                lock_path.display()
            )
        })?;
    let mut lock = RwLock::new(lock_file);
    let _guard = lock.write().map_err(|error| {
        format!(
            "failed to lock codegen bootstrap {}: {error}",
            lock_path.display()
        )
    })?;

    if fs::read_to_string(&stamp_path).is_ok_and(|stamp| stamp == run_id)
        && hew_library_path().is_file()
    {
        return Ok(());
    }

    let output = build_codegen_artifacts(&target_dir, build_profile)?;
    if !output.status.success() {
        return Err(format!(
            "failed to bootstrap hew-cli codegen artifacts with `cargo build -p hew-lib{}`\n{}",
            if build_profile == "release" {
                " --release"
            } else {
                ""
            },
            describe_output(&output)
        ));
    }

    let hew_library = hew_library_path();
    if !hew_library.is_file() {
        return Err(format!(
            "codegen bootstrap succeeded but {} was not created",
            hew_library.display()
        ));
    }

    fs::write(&stamp_path, run_id).map_err(|error| {
        format!(
            "failed to write codegen bootstrap stamp {}: {error}",
            stamp_path.display()
        )
    })?;

    Ok(())
}

fn bootstrap_wasi_runner() -> Result<(), String> {
    if find_wasmtime().is_none() {
        return Err(
            "failed to bootstrap WASI runner prerequisites: `wasmtime` not found \
             (checked PATH and ~/.wasmtime/bin)"
                .to_string(),
        );
    }

    let target_dir = target_dir()?;
    fs::create_dir_all(&target_dir).map_err(|error| {
        format!(
            "failed to create target dir {}: {error}",
            target_dir.display()
        )
    })?;

    let build_profile = build_profile();
    build_wasi_runtime_serialized(&target_dir, build_profile)?;

    for (package, archive) in WASI_STDLIB_ARCHIVES {
        build_wasi_stdlib_archive(&target_dir, build_profile, package, archive)?;
    }

    Ok(())
}

/// Build `libhew_runtime.a` for wasm32-wasip1, serialized across parallel
/// nextest test processes.
///
/// The fast path (all artifacts present) skips the lock entirely. Otherwise a
/// cross-process `fd_lock::RwLock` serializes the build: without it, multiple
/// test binaries each see the artifact absent and each launch `cargo build`
/// concurrently; Cargo writes the staticlib non-atomically, so races corrupt
/// or transiently delete the file and wasm-ld then fails with
/// "cannot open `libhew_runtime.a`". `bootstrap_codegen` uses the same pattern.
fn build_wasi_runtime_serialized(target_dir: &Path, build_profile: &str) -> Result<(), String> {
    let runtime_path = target_dir
        .join("wasm32-wasip1")
        .join(build_profile)
        .join("libhew_runtime.a");

    // Fast path: artifact already present (common case after the CI pre-build
    // step or after a sibling process completed the build).
    if runtime_path.is_file() {
        return Ok(());
    }

    let lock_path = target_dir.join("hew-cli-wasi-bootstrap.lock");
    let lock_file = OpenOptions::new()
        .create(true)
        .read(true)
        .write(true)
        .truncate(false)
        .open(&lock_path)
        .map_err(|error| {
            format!(
                "failed to open WASI bootstrap lock {}: {error}",
                lock_path.display()
            )
        })?;
    let mut lock = RwLock::new(lock_file);
    let _guard = lock.write().map_err(|error| {
        format!(
            "failed to lock WASI bootstrap {}: {error}",
            lock_path.display()
        )
    })?;

    // Re-check after acquiring the lock: a sibling may have built while we waited.
    if runtime_path.is_file() {
        return Ok(());
    }

    let mut command = Command::new("cargo");
    command
        .args([
            "build",
            "-q",
            "-p",
            "hew-runtime",
            "--target",
            "wasm32-wasip1",
            "--no-default-features",
        ])
        .env("CARGO_TARGET_DIR", target_dir)
        .current_dir(repo_root());
    if build_profile == "release" {
        command.arg("--release");
    }

    let output = command.output().map_err(|error| {
        format!(
            "failed to invoke `cargo build -p hew-runtime --target wasm32-wasip1 --no-default-features{}`: {error}",
            if build_profile == "release" { " --release" } else { "" }
        )
    })?;

    if !output.status.success() {
        return Err(format!(
            "failed to bootstrap WASI runner runtime with `cargo build -p hew-runtime --target wasm32-wasip1 --no-default-features{}`\n{}",
            if build_profile == "release" { " --release" } else { "" },
            describe_output(&output)
        ));
    }

    if !runtime_path.is_file() {
        // Cargo can exit 0 without producing the staticlib when a stale
        // fingerprint convinces it nothing needs rebuilding.  Clean and retry.
        wasi_runtime_clean_and_retry(target_dir, build_profile)?;

        if !runtime_path.is_file() {
            return Err(format!(
                "WASI runner bootstrap succeeded but {} was not created \
                 even after a clean-and-retry",
                runtime_path.display()
            ));
        }
    }

    Ok(())
}

fn build_wasi_stdlib_archive(
    target_dir: &std::path::Path,
    build_profile: &str,
    package: &str,
    archive: &str,
) -> Result<(), String> {
    let archive_path = target_dir
        .join("wasm32-wasip1")
        .join(build_profile)
        .join(archive);
    if archive_path.is_file() {
        return Ok(());
    }

    let mut command = Command::new("cargo");
    command
        .args(["build", "-q", "-p", package, "--target", "wasm32-wasip1"])
        .env("CARGO_TARGET_DIR", target_dir)
        .current_dir(repo_root());
    if build_profile == "release" {
        command.arg("--release");
    }

    let output = command.output().map_err(|error| {
        format!("failed to invoke `cargo build -p {package} --target wasm32-wasip1`: {error}")
    })?;

    if !output.status.success() {
        return Err(format!(
            "failed to bootstrap WASI stdlib archive with `cargo build -p {package} --target wasm32-wasip1{}`\n{}",
            if build_profile == "release" {
                " --release"
            } else {
                ""
            },
            describe_output(&output)
        ));
    }

    if !archive_path.is_file() {
        // Cargo can exit 0 without producing the staticlib when a stale
        // fingerprint (e.g. from a CI cache hit that only cached the rlib)
        // convinces it that nothing needs to be rebuilt.  Recover by cleaning
        // the wasm32-wasip1 artifacts for this package and retrying once.
        wasi_stdlib_archive_clean_and_retry(target_dir, build_profile, package, archive)?;

        if !archive_path.is_file() {
            return Err(format!(
                "WASI stdlib archive bootstrap succeeded but {} was not created \
                 even after a clean-and-retry",
                archive_path.display()
            ));
        }
    }

    Ok(())
}

/// Clean the wasm32-wasip1 artifacts for a stdlib archive package and rebuild.
/// Used to recover from Cargo's stale-fingerprint no-op when the rlib is
/// cached from a prior run but the staticlib was never produced.
fn wasi_stdlib_archive_clean_and_retry(
    target_dir: &std::path::Path,
    build_profile: &str,
    package: &str,
    archive: &str,
) -> Result<(), String> {
    let clean_output = Command::new("cargo")
        .args(["clean", "-p", package, "--target", "wasm32-wasip1"])
        .env("CARGO_TARGET_DIR", target_dir)
        .current_dir(repo_root())
        .output()
        .map_err(|error| {
            format!("failed to invoke `cargo clean -p {package} --target wasm32-wasip1`: {error}")
        })?;

    if !clean_output.status.success() {
        return Err(format!(
            "WASI stdlib archive clean failed for {archive}:\n{}",
            describe_output(&clean_output)
        ));
    }

    let mut retry_cmd = Command::new("cargo");
    retry_cmd
        .args(["build", "-q", "-p", package, "--target", "wasm32-wasip1"])
        .env("CARGO_TARGET_DIR", target_dir)
        .current_dir(repo_root());
    if build_profile == "release" {
        retry_cmd.arg("--release");
    }

    let retry_output = retry_cmd.output().map_err(|error| {
        format!("failed to invoke wasm32-wasip1 retry build for {package}: {error}")
    })?;

    if !retry_output.status.success() {
        return Err(format!(
            "WASI stdlib archive build failed on retry for {archive}:\n{}",
            describe_output(&retry_output)
        ));
    }

    Ok(())
}

/// Clean the wasm32-wasip1 artifacts for `hew-runtime` and rebuild.
/// Used to recover from Cargo's stale-fingerprint no-op when the rlib is
/// cached from a prior run but the staticlib was never produced.
fn wasi_runtime_clean_and_retry(
    target_dir: &std::path::Path,
    build_profile: &str,
) -> Result<(), String> {
    let clean_output = Command::new("cargo")
        .args(["clean", "-p", "hew-runtime", "--target", "wasm32-wasip1"])
        .env("CARGO_TARGET_DIR", target_dir)
        .current_dir(repo_root())
        .output()
        .map_err(|error| {
            format!("failed to invoke `cargo clean -p hew-runtime --target wasm32-wasip1`: {error}")
        })?;

    if !clean_output.status.success() {
        return Err(format!(
            "WASI runtime clean failed:\n{}",
            describe_output(&clean_output)
        ));
    }

    let mut retry_cmd = Command::new("cargo");
    retry_cmd
        .args([
            "build",
            "-q",
            "-p",
            "hew-runtime",
            "--target",
            "wasm32-wasip1",
            "--no-default-features",
        ])
        .env("CARGO_TARGET_DIR", target_dir)
        .current_dir(repo_root());
    if build_profile == "release" {
        retry_cmd.arg("--release");
    }

    let retry_output = retry_cmd
        .output()
        .map_err(|error| format!("failed to invoke wasm32-wasip1 retry build: {error}"))?;

    if !retry_output.status.success() {
        return Err(format!(
            "WASI runner runtime build failed on retry:\n{}",
            describe_output(&retry_output)
        ));
    }

    Ok(())
}

fn build_codegen_artifacts(target_dir: &Path, build_profile: &str) -> Result<Output, String> {
    let mut command = Command::new("cargo");
    command
        .args(["build", "-q", "-p", "hew-lib"])
        .env("CARGO_TARGET_DIR", target_dir)
        .current_dir(repo_root());
    if build_profile == "release" {
        command.arg("--release");
    }

    // On macOS, pin the deployment target so the Rust-compiled objects inside
    // libhew.a are tagged with the same minimum-OS version as the link step
    // (which uses MACOSX_DEPLOYMENT_TARGET or defaults to "13.0" via
    // `TargetSpec::linker_triple`).  Without this, objects compiled against a
    // newer Xcode SDK are tagged with a higher OS version, causing ld64.lld to
    // emit "has version X, which is newer than target minimum of 13.0.0"
    // warnings for every archive member.
    #[cfg(target_os = "macos")]
    {
        let deployment = std::env::var("MACOSX_DEPLOYMENT_TARGET")
            .ok()
            .filter(|v| !v.is_empty())
            .unwrap_or_else(|| "13.0".to_string());
        command.env("MACOSX_DEPLOYMENT_TARGET", deployment);
    }

    command.output().map_err(|error| {
        format!(
            "failed to invoke `cargo build -p hew-lib{}`: {error}",
            if build_profile == "release" {
                " --release"
            } else {
                ""
            }
        )
    })
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

fn hew_library_path() -> PathBuf {
    hew_binary()
        .parent()
        .expect("hew binary should have a parent directory")
        .join(hew_lib_name())
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

fn hew_lib_name() -> &'static str {
    if cfg!(windows) {
        "hew.lib"
    } else {
        "libhew.a"
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
