//! wasm32 execution coverage for platform-width `try_to_X()` conversions (#2371).
//!
//! The `try_to_X()` conversion family's execution fixtures otherwise run on the
//! native (64-bit) target only. The `isize`/`usize` legs are width-generic in
//! codegen — their bounds derive from `get_bit_width()` — so 32-bit behaviour is
//! *likely* correct by construction but was previously unproven.
//!
//! [`HEW_SOURCE`] derives its boundary values instead of hard-coding them:
//! `(-1) as usize` is `usize::MAX` at whatever bit width the compiler actually
//! assigned `usize`, and clearing that pattern's sign bit with a logical shift
//! gives the matching `isize::MAX`. A fixed literal like `2147483647 as isize`
//! would carry the same numeric value regardless of the emitted width, so it
//! cannot tell a correct 32-bit emission from a regression that (mis)emits
//! `isize`/`usize` as 64-bit on wasm32 — the literal is exact either way. The
//! derivation is what makes the boundary itself width-sensitive.
//!
//! Two tests share [`HEW_SOURCE`] and assert opposite stdout:
//!
//!   * [`wasm_try_to_platform_width_boundaries_are_width_correct`] compiles it
//!     for wasm32, links against the real wasm runtime archive, and runs it
//!     under wasmtime (where `isize`/`usize` are genuinely 32-bit): `usize::MAX`
//!     is `4294967295`, which does not fit `i32` but does fit `u64`, and
//!     `isize::MAX` is `2147483647`, which fits `i32` exactly.
//!   * [`native_run_of_the_same_source_diverges_from_the_wasm32_result`] is the
//!     negative control: it runs the identical source natively (host is
//!     64-bit), where the same derivation instead produces a 64-bit
//!     `isize::MAX` that does *not* fit `i32`, and a 64-bit `usize::MAX` that
//!     does not equal the 32-bit literal the wasm leg checks against. The two
//!     tests assert genuinely different stdout for the same source — proving
//!     the fixture pair actually distinguishes platform width rather than
//!     passing coincidentally on both.
//!
//! The wasm leg reuses the same self-contained link/run scaffolding as
//! `wasm_generator_exec.rs`; both tests skip cleanly when their respective
//! toolchain (wasm-ld / wasmtime / the wasm32-wasip1 runtime archive / the
//! rustup self-contained libc, or `libhew.a`) is unavailable.

#![cfg(not(target_arch = "wasm32"))]
#![cfg(unix)]

use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::OnceLock;
use std::time::Duration;

/// Platform-width `try_to_X()` boundary probe, shared by the wasm32 leg and
/// its native negative control. Every boundary value is derived from the
/// compiler's actual `isize`/`usize` bit width, not hard-coded, so a width
/// regression changes the printed result instead of leaving it unchanged.
const HEW_SOURCE: &str = r#"fn main() {
    // usize::MAX at whatever bit width the compiler assigned usize: all-ones,
    // derived (not typed as a literal) via unsigned negation. On wasm32 this
    // is 4294967295; on a 64-bit target it is 18446744073709551615.
    let all_ones: usize = (-1) as usize;

    // isize::MAX at the same bit width: clear the sign bit of all_ones with a
    // logical (unsigned) shift. On wasm32 this is 2147483647; on a 64-bit
    // target it is 9223372036854775807.
    let smax: isize = (all_ones >> 1) as isize;

    match smax.try_to_i32() {
        Some(v) => {
            if v == 2147483647 { println("smax_i32=ok") } else { println("smax_i32=wrong") }
        }
        None => println("smax_i32=none"),
    }

    match all_ones.try_to_i32() {
        Some(_) => println("umax_i32=some"),
        None => println("umax_i32=none"),
    }
    match all_ones.try_to_u64() {
        Some(v) => {
            if v == 4294967295 as u64 { println("umax_u64=ok") } else { println("umax_u64=wrong") }
        }
        None => println("umax_u64=none"),
    }
}
"#;

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
    // Cold `target/`: build `hew` once under the shared serialized build
    // lock, OUTSIDE any per-test deadline, so a concurrent build-lock holder
    // cannot make a `cargo run` fallback burn the bounded budget and produce
    // a false timeout (hew-lang/hew#1887).
    Command::new(hew_testutil::ensure_hew_bin_built().expect("build hew binary"))
}

fn ensure_hew_runtime_lib(repo: &Path) -> bool {
    static BUILT: OnceLock<bool> = OnceLock::new();
    *BUILT.get_or_init(|| {
        let lib = target_dir(repo).join("debug").join("libhew.a");
        let cargo = std::env::var_os("CARGO").unwrap_or_else(|| "cargo".into());
        let status = Command::new(cargo)
            .current_dir(repo)
            .args(["build", "--quiet", "-p", "hew-lib"])
            .status();
        matches!(status, Ok(s) if s.success()) && lib.exists()
    })
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
    std::fs::write(&source_path, HEW_SOURCE).expect("write Hew source");

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

/// Negative control: the identical source run on the native (64-bit) target
/// must NOT reproduce the wasm32-correct stdout above. `isize`/`usize::MAX`
/// derive to their 64-bit values here, so `smax.try_to_i32()` rejects (the
/// 64-bit signed max does not fit `i32`) and `all_ones.try_to_u64()` no longer
/// equals the 32-bit literal the wasm leg checks — printing `umax_u64=wrong`.
/// If this ever printed the wasm32-shaped line, the derivation above would not
/// actually be tracking the compiler's real bit width, and neither test would
/// be proving anything about platform width.
#[test]
fn native_run_of_the_same_source_diverges_from_the_wasm32_result() {
    let repo = repo_root();
    if !ensure_hew_runtime_lib(&repo) {
        eprintln!("skip: libhew.a unavailable");
        return;
    }

    let dir = tempfile::tempdir().expect("tempdir");
    let source_path = dir.path().join("try_to_platform_width_native.hew");
    std::fs::write(&source_path, HEW_SOURCE).expect("write Hew source");

    let mut run = hew_command(&repo);
    run.arg("run").arg(&source_path);
    let output = hew_testutil::run_command_bounded(
        &mut run,
        "hew run try_to platform width (native negative control)",
        Duration::from_secs(60),
    )
    .unwrap_or_else(|e| panic!("{e}"));
    assert!(
        output.status.success(),
        "hew run failed (status={:?}); stderr:\n{}",
        output.status,
        String::from_utf8_lossy(&output.stderr)
    );
    let stdout = String::from_utf8(output.stdout).expect("stdout utf8");
    assert_eq!(stdout, "smax_i32=none\numax_i32=none\numax_u64=wrong\n");
    assert_ne!(
        stdout, "smax_i32=ok\numax_i32=none\numax_u64=ok\n",
        "native run must diverge from the wasm32-correct stdout, or the \
         boundary values are not actually tracking platform width"
    );
}
