//! End-to-end execution tests pinning the default-arm reachability invariant
//! for literal-scrutinee `match` lowering.
//!
//! Regression coverage for a silent-wrong miscompile: a `match` mixing a
//! literal arm, a guarded `Binding` arm, and a `_` wildcard dropped every arm
//! after the first guarded binding arm — including the real `_` default — from
//! the emitted IR. `hew check` stayed green while `hew run` produced no output
//! and exited 1 on the fall-through path, because the dispatch trapped where the
//! default arm should have run.
//!
//! These tests exercise the full pipeline (checker → HIR → MIR literal-match
//! ordered chain → codegen-rs → native execution) and assert on stdout. A
//! `check`-only test cannot catch this class of bug; the value must be observed
//! at runtime.

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
    // Cold `target/`: build `hew` once under the shared serialized build
    // lock, OUTSIDE any per-test deadline, so a concurrent build-lock holder
    // cannot make a `cargo run` fallback burn the bounded budget and produce
    // a false timeout (hew-lang/hew#1887).
    Command::new(hew_testutil::ensure_hew_bin_built().expect("build hew binary"))
}

fn ensure_hew_runtime_lib(repo: &Path) {
    let _ = repo;
    static BUILT: OnceLock<()> = OnceLock::new();
    BUILT.get_or_init(|| {
        hew_testutil::ensure_hew_lib_built().expect("build libhew.a");
    });
}

/// Compile and run a Hew snippet; return trimmed stdout.
///
/// Panics if `hew run` exits non-zero or times out — exit-1-with-no-output is
/// precisely the symptom under test, so a non-zero exit is a hard failure.
fn run_hew_source(repo: &Path, stem: &str, source: &str) -> String {
    ensure_hew_runtime_lib(repo);
    let dir = std::env::temp_dir().join(format!(
        "hew-match-guard-default-exec-{}-{stem}",
        std::process::id()
    ));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("create temp source dir");
    let path = dir.join(format!("{stem}.hew"));
    std::fs::write(&path, source).expect("write temp Hew source");

    let mut cmd = hew_command(repo);
    cmd.arg("run").arg(&path);
    let output = hew_testutil::run_command_bounded(
        &mut cmd,
        format!("hew run {}", path.display()),
        Duration::from_secs(20),
    )
    .unwrap_or_else(|e| panic!("{e}"));
    assert!(
        output.status.success(),
        "hew run {} exited non-zero (status={:?}); stderr:\n{}",
        path.display(),
        output.status,
        String::from_utf8_lossy(&output.stderr)
    );
    String::from_utf8(output.stdout)
        .expect("stdout is utf-8")
        .trim()
        .to_string()
}

/// The reported shape: literal `0`, guarded binding `x if x > 100`, then `_`.
/// Drives every dispatch path of the same source program:
///   - `5`   matches neither the literal nor the guard → wildcard `"other"`
///   - `0`   matches the literal → `"zero"`
///   - `200` passes the guard → `"big"`
///
/// The fall-through case (`5`) is the regression: it previously dropped the
/// `_` arm and exited 1 with no output.
#[test]
fn literal_guard_wildcard_covers_every_dispatch_path() {
    let repo = repo_root();
    let stdout = run_hew_source(
        &repo,
        "literal_guard_wildcard",
        r#"fn classify(n: i64) -> string {
    match n {
        0 => "zero",
        x if x > 100 => "big",
        _ => "other",
    }
}

fn main() {
    println(classify(5));
    println(classify(0));
    println(classify(200));
}
"#,
    );
    assert_eq!(
        stdout, "other\nzero\nbig",
        "literal+guard+wildcard must reach each arm; got {stdout:?}"
    );
}

/// A guarded binding arm placed *before* the literal arm. The first arm is a
/// guarded `Binding`, so the previous implementation set it as the sole
/// catch-all and dropped both the literal and the `_` default. All three
/// dispatch paths must still resolve in source order.
#[test]
fn guarded_binding_before_literal_keeps_later_arms_reachable() {
    let repo = repo_root();
    let stdout = run_hew_source(
        &repo,
        "guard_before_literal",
        r#"fn classify(n: i64) -> string {
    match n {
        x if x > 100 => "big",
        0 => "zero",
        _ => "other",
    }
}

fn main() {
    println(classify(200));
    println(classify(0));
    println(classify(5));
}
"#,
    );
    assert_eq!(
        stdout, "big\nzero\nother",
        "guarded binding before literal must not shadow later arms; got {stdout:?}"
    );
}

/// Multiple guarded arms between a literal and the wildcard. The fall-through
/// path (no literal, all guards fail) must reach the final `_`.
#[test]
fn literal_then_multiple_guards_then_wildcard_falls_through() {
    let repo = repo_root();
    let stdout = run_hew_source(
        &repo,
        "literal_multi_guard_wildcard",
        r#"fn classify(n: i64) -> i64 {
    match n {
        0 => 0,
        x if x > 100 => 1,
        y if y < -100 => 2,
        _ => -1,
    }
}

fn main() {
    println(classify(5));
    println(classify(0));
    println(classify(200));
    println(classify(-200));
}
"#,
    );
    assert_eq!(
        stdout, "-1\n0\n1\n2",
        "literal + two guards + wildcard must reach each arm; got {stdout:?}"
    );
}

/// String scrutinee with a guarded binding before the wildcard. Confirms the
/// fix is type-agnostic (string literal compares via `hew_string_equals`), not
/// integer-only.
#[test]
fn string_literal_guard_wildcard_falls_through() {
    let repo = repo_root();
    let stdout = run_hew_source(
        &repo,
        "string_literal_guard_wildcard",
        r#"fn label(s: string) -> string {
    match s {
        "yes" => "affirmative",
        x if x.len() > 5 => "long",
        _ => "fallback",
    }
}

fn main() {
    println(label("yes"));
    println(label("absolutely"));
    println(label("no"));
}
"#,
    );
    assert_eq!(
        stdout, "affirmative\nlong\nfallback",
        "string literal + guard + wildcard must reach each arm; got {stdout:?}"
    );
}
