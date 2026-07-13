//! wasm32 execution coverage for platform-width `try_to_X()` conversions (#2371).
//!
//! The `try_to_X()` conversion family's execution fixtures otherwise run on the
//! native (64-bit) target only. The `isize`/`usize` legs are width-generic in
//! codegen — their bounds derive from `get_bit_width()` — so 32-bit behaviour is
//! *likely* correct by construction but was previously unproven. This compiles a
//! source fixture that exercises the platform-width MAX boundary rows, links it
//! against the real wasm runtime archive, runs it under wasmtime (where `isize`
//! and `usize` are 32-bit), and asserts the width-correct outcome:
//!
//!   * `isize::MAX` on wasm32 == 2147483647, which fits `i32` exactly — so
//!     `(2147483647 as isize).try_to_i32()` must be `Some(2147483647)`. On the
//!     native 64-bit target the *same* boundary would be `9223372036854775807`
//!     and reject, so this row genuinely distinguishes the two widths.
//!   * `usize::MAX` on wasm32 == 4294967295, which does NOT fit `i32` but DOES
//!     fit `u64` — so `try_to_i32()` is `None` while `try_to_u64()` is `Some`.
//!
//! This reuses the same self-contained link/run scaffolding as
//! `wasm_generator_exec.rs`; it skips cleanly when wasm-ld / wasmtime / the
//! wasm32-wasip1 runtime archive / the rustup self-contained libc are absent.

#![cfg(not(target_arch = "wasm32"))]
#![cfg(unix)]

use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::OnceLock;
use std::time::Duration;

fn repo_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("hew-codegen-rs has a workspace parent")
        .to_path_buf()
}

fn target_dir(repo: &Path) -> PathBuf {
    std::env::var_os("CARGO_TARGET_DIR").map_or_else(
        || repo.join("target"),
        |dir| {
            let path = PathBuf::from(dir);
            if path.is_absolute() {
                path
            } else {
                repo.join(path)
            }
        },
    )
}

fn which(tool: &str) -> Option<PathBuf> {
    let out = Command::new("which").arg(tool).output().ok()?;
    if !out.status.success() {
        return None;
    }
    let path = PathBuf::from(String::from_utf8_lossy(&out.stdout).trim());
    path.is_file().then_some(path)
}

fn wasmtime() -> Option<PathBuf> {
    which("wasmtime").or_else(|| {
        let home = std::env::var_os("HOME")?;
        let candidate = PathBuf::from(home)
            .join(".wasmtime")
            .join("bin")
            .join("wasmtime");
        candidate.is_file().then_some(candidate)
    })
}

fn ensure_wasm_runtime(repo: &Path) -> Option<PathBuf> {
    static BUILT: OnceLock<Option<PathBuf>> = OnceLock::new();
    BUILT
        .get_or_init(|| {
            let lib = target_dir(repo)
                .join("wasm32-wasip1")
                .join("debug")
                .join("libhew_runtime.a");
            let cargo = std::env::var_os("CARGO").unwrap_or_else(|| "cargo".into());
            let status = Command::new(cargo)
                .current_dir(repo)
                .args([
                    "build",
                    "--quiet",
                    "-p",
                    "hew-runtime",
                    "--target",
                    "wasm32-wasip1",
                    "--no-default-features",
                ])
                .status()
                .ok()?;
            (status.success() && lib.exists()).then_some(lib)
        })
        .clone()
}

fn wasm_self_contained_libc() -> Option<PathBuf> {
    let sysroot = Command::new("rustc")
        .args(["--print", "sysroot"])
        .output()
        .ok()
        .filter(|o| o.status.success())
        .map(|o| PathBuf::from(String::from_utf8_lossy(&o.stdout).trim()))?;
    let libc = sysroot.join("lib/rustlib/wasm32-wasip1/lib/self-contained/libc.a");
    libc.exists().then_some(libc)
}

fn hew_command(repo: &Path) -> Command {
    let bin = target_dir(repo).join("debug").join("hew");
    if bin.exists() {
        return Command::new(bin);
    }
    let cargo = std::env::var_os("CARGO").unwrap_or_else(|| "cargo".into());
    let mut command = Command::new(cargo);
    command
        .current_dir(repo)
        .args(["run", "--quiet", "-p", "hew-cli", "--bin", "hew", "--"]);
    command
}

#[test]
fn wasm_try_to_platform_width_boundaries_are_width_correct() {
    let Some(wasm_ld) = which("wasm-ld") else {
        eprintln!("skip: wasm-ld not found");
        return;
    };
    let Some(wasmtime) = wasmtime() else {
        eprintln!("skip: wasmtime not found");
        return;
    };
    let repo = repo_root();
    let Some(runtime) = ensure_wasm_runtime(&repo) else {
        eprintln!("skip: wasm32-wasip1 runtime archive unavailable");
        return;
    };
    let Some(libc) = wasm_self_contained_libc() else {
        eprintln!("skip: rustup wasm32-wasip1 self-contained libc not installed");
        return;
    };

    let dir = tempfile::tempdir().expect("tempdir");
    let source_path = dir.path().join("try_to_platform_width.hew");
    std::fs::write(
        &source_path,
        r#"fn main() {
    // On wasm32 `isize` is 32-bit, so isize::MAX == 2147483647, which fits i32
    // exactly. On a 64-bit target the same boundary would be far larger and
    // reject, so this row proves the width-generic bound tracks get_bit_width().
    let smax: isize = 2147483647 as isize;
    match smax.try_to_i32() {
        Some(v) => {
            if v == 2147483647 { println("smax_i32=ok") } else { println("smax_i32=wrong") }
        }
        None => println("smax_i32=none"),
    }

    // On wasm32 `usize` is 32-bit, so usize::MAX == 4294967295: does NOT fit i32
    // but DOES fit u64.
    let umax: usize = 4294967295 as usize;
    match umax.try_to_i32() {
        Some(_) => println("umax_i32=some"),
        None => println("umax_i32=none"),
    }
    match umax.try_to_u64() {
        Some(v) => {
            if v == 4294967295 as u64 { println("umax_u64=ok") } else { println("umax_u64=wrong") }
        }
        None => println("umax_u64=none"),
    }
}
"#,
    )
    .expect("write Hew source");

    let mut compile = hew_command(&repo);
    compile
        .arg("compile")
        .arg("--target")
        .arg("wasm32-unknown-unknown")
        .arg("--emit-dir")
        .arg(dir.path())
        .arg(&source_path);
    let output = hew_testutil::run_command_bounded(
        &mut compile,
        "hew compile wasm try_to platform width",
        Duration::from_secs(60),
    )
    .unwrap_or_else(|e| panic!("{e}"));
    assert!(
        output.status.success(),
        "hew compile failed (status={:?}); stderr:\n{}",
        output.status,
        String::from_utf8_lossy(&output.stderr)
    );

    let obj = dir.path().join("try_to_platform_width.wasm.o");
    let wasm = dir.path().join("try_to_platform_width.linked.wasm");
    let link = Command::new(&wasm_ld)
        .arg("--no-entry")
        .arg("--export=_start")
        .arg(&obj)
        .arg(&runtime)
        .arg(&libc)
        .arg("-o")
        .arg(&wasm)
        .output()
        .expect("wasm-ld link");
    assert!(
        link.status.success(),
        "wasm-ld failed:\n{}",
        String::from_utf8_lossy(&link.stderr)
    );

    let run = Command::new(&wasmtime)
        .arg("run")
        .arg(&wasm)
        .output()
        .expect("wasmtime run");
    assert!(
        run.status.success(),
        "wasmtime failed:\n{}",
        String::from_utf8_lossy(&run.stderr)
    );
    assert_eq!(
        String::from_utf8(run.stdout).expect("stdout utf8"),
        "smax_i32=ok\numax_i32=none\numax_u64=ok\n"
    );
}
