#![allow(
    dead_code,
    reason = "shared integration-test helpers are not used by every test target"
)]

use fd_lock::RwLock;
use std::fs::{self, OpenOptions};
use std::path::{Path, PathBuf};
use std::process::{Command, Output};
use std::sync::OnceLock;

static CODEGEN_STATUS: OnceLock<Result<(), String>> = OnceLock::new();
static WASI_RUNNER_STATUS: OnceLock<Result<(), String>> = OnceLock::new();

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
    let runtime_path = target_dir
        .join("wasm32-wasip1")
        .join(build_profile)
        .join("libhew_runtime.a");
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
        .env("CARGO_TARGET_DIR", &target_dir)
        .current_dir(repo_root());
    if build_profile == "release" {
        command.arg("--release");
    }

    let output = command.output().map_err(|error| {
        format!(
            "failed to invoke `cargo build -p hew-runtime --target wasm32-wasip1 --no-default-features{}`: {error}",
            if build_profile == "release" {
                " --release"
            } else {
                ""
            }
        )
    })?;

    if !output.status.success() {
        return Err(format!(
            "failed to bootstrap WASI runner runtime with `cargo build -p hew-runtime --target wasm32-wasip1 --no-default-features{}`\n{}",
            if build_profile == "release" {
                " --release"
            } else {
                ""
            },
            describe_output(&output)
        ));
    }

    if !runtime_path.is_file() {
        // Cargo can exit 0 without producing the staticlib when a stale
        // fingerprint (e.g. from a CI cache hit that only cached the rlib)
        // convinces it that nothing needs to be rebuilt.  Recover by cleaning
        // the wasm32-wasip1 artifacts for this package and retrying once.
        wasi_runtime_clean_and_retry(&target_dir, build_profile)?;

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
