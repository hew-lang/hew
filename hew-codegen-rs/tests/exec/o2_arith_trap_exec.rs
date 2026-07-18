//! O2-pipeline arithmetic-trap fixture (hew-lang/hew#2104, gate 1).
//!
//! Before the release default can flip from `-O0` to `-O2`, every runtime trap
//! the front-end wires must survive the LLVM `default<O2>` middle-end pipeline.
//! The `-O2` pipeline is free to reshape IR arbitrarily; it must never elide a
//! trap the language semantics require. #2103 verified overflow survives O2 by a
//! manual check, but there was no dedicated fixture that would go red if a future
//! O2 pass (aggressive value-range/`nsw`/`nuw` inference, a UB-based DCE, etc.)
//! silently dropped one of these guards.
//!
//! Each case forces its computed value through a real cross-function call so the
//! result is genuinely LIVE (consumed via `print`) — the trap cannot be removed
//! by legitimate dead-code elimination, only by an incorrect optimization that
//! this fixture is meant to catch. Every case is exercised twice over the SAME
//! `hew` binary: once at the default `-O0` and once with `HEW_OPT_LEVEL=2`
//! forcing the whole program through the O2 pipeline (the env FLOOR the CLI
//! honours; see `hew-cli/src/main.rs`). Both runs must trap identically.
//!
//! `hew run` catches the abort, prints `hew: trap in main context: <kind>` to
//! stderr, and exits with a clean non-zero code (not a propagated signal), so
//! the oracle asserts on BOTH the non-zero exit and the exact trap kind at each
//! optimization level.
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

fn hew_bin(repo: &Path) -> PathBuf {
    target_dir(repo).join("debug").join("hew")
}

fn hew_command(repo: &Path) -> Command {
    let bin = hew_bin(repo);
    if bin.exists() {
        return Command::new(bin);
    }
    // Cold `target/`: build `hew` once under the shared serialized build lock,
    // OUTSIDE any per-test deadline, so a concurrent build-lock holder cannot
    // make a `cargo run` fallback burn the bounded budget (hew-lang/hew#1887).
    Command::new(hew_testutil::ensure_hew_bin_built().expect("build hew binary"))
}

fn ensure_hew_runtime_lib() {
    static BUILT: OnceLock<()> = OnceLock::new();
    BUILT.get_or_init(|| {
        hew_testutil::ensure_hew_lib_built().expect("build libhew.a");
    });
}

/// Compile and run a Hew snippet expected to TRAP at runtime. Returns
/// `(exit_code, stderr)`. `opt2 = true` sets `HEW_OPT_LEVEL=2` so the whole
/// program is lowered through the LLVM `default<O2>` pipeline.
fn run_hew_source_trap(stem: &str, source: &str, opt2: bool) -> (i32, String) {
    let repo = repo_root();
    ensure_hew_runtime_lib();
    let level = if opt2 { "o2" } else { "o0" };
    let dir = std::env::temp_dir().join(format!(
        "hew-o2-arith-trap-{}-{}-{}",
        std::process::id(),
        stem,
        level
    ));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("create temp source dir");
    let path = dir.join(format!("{stem}.hew"));
    std::fs::write(&path, source).expect("write temp Hew source");

    let mut cmd = hew_command(&repo);
    cmd.arg("run").arg(&path);
    if opt2 {
        cmd.env("HEW_OPT_LEVEL", "2");
    }
    let output = hew_testutil::run_command_bounded(
        &mut cmd,
        format!("hew run {} (opt={level})", path.display()),
        Duration::from_secs(30),
    )
    .unwrap_or_else(|e| panic!("{e}"));

    let code = output.status.code().unwrap_or_else(|| {
        panic!(
            "hew run was killed by signal (expected a clean trap exit); stderr:\n{}",
            String::from_utf8_lossy(&output.stderr)
        )
    });
    let stderr = String::from_utf8_lossy(&output.stderr).into_owned();
    (code, stderr)
}

/// Assert that `source` traps with `trap_kind` at BOTH O0 and O2. The two levels
/// must agree: a divergence (trap at O0, silent at O2, or a different kind) is a
/// miscompile in the O2 pipeline and the exact regression this gate guards.
fn assert_traps_at_both_levels(stem: &str, source: &str, trap_kind: &str) {
    for (opt2, level) in [(false, "O0"), (true, "O2")] {
        let (code, stderr) = run_hew_source_trap(stem, source, opt2);
        assert_ne!(
            code, 0,
            "[{level}] {stem}: expected a non-zero trap exit, got 0; stderr:\n{stderr}"
        );
        assert!(
            stderr.contains(trap_kind),
            "[{level}] {stem}: expected trap `{trap_kind}`; exit={code}, stderr:\n{stderr}"
        );
    }
}

/// `i64::MAX + 1` must trap `IntegerOverflow` through both pipelines. The add is
/// hidden behind a cross-function call and the result is consumed by `print`, so
/// the overflow-checked add cannot be constant-folded away or DCE'd; only an
/// unsound O2 range/`nsw` transform could drop it.
#[test]
fn integer_overflow_traps_at_o0_and_o2() {
    let source = r#"
fn add(a: i64, b: i64) -> i64 {
    a + b
}

fn main() {
    let a: i64 = 9223372036854775807;
    let c = add(a, 1);
    print("${c}");
}
"#;
    assert_traps_at_both_levels("o2_overflow", source, "IntegerOverflow");
}

/// `n / 0` must trap `DivideByZero` through both pipelines. The divisor arrives
/// via a call so it is not a syntactic literal zero the front-end could reject
/// at compile time; the quotient is consumed by `print`.
#[test]
fn divide_by_zero_traps_at_o0_and_o2() {
    let source = r#"
fn div(a: i64, b: i64) -> i64 {
    a / b
}

fn main() {
    let a: i64 = 42;
    let c = div(a, 0);
    print("${c}");
}
"#;
    assert_traps_at_both_levels("o2_divzero", source, "DivideByZero");
}

/// A shift whose count is `>=` the bit width must trap `ShiftOutOfRange` through
/// both pipelines. The shift amount arrives via a call so it is a runtime value,
/// forcing the MIR-emitted range guard rather than a compile-time rejection; the
/// result is consumed by `print`.
#[test]
fn shift_out_of_range_traps_at_o0_and_o2() {
    let source = r#"
fn shl(a: i64, s: i64) -> i64 {
    a << s
}

fn main() {
    let a: i64 = 1;
    let c = shl(a, 64);
    print("${c}");
}
"#;
    assert_traps_at_both_levels("o2_shift", source, "ShiftOutOfRange");
}
