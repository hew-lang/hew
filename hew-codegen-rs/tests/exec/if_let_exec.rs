//! End-to-end execution tests for `if let` lowering.
//!
//! Exercises the full pipeline: checker wiring → HIR `IfLet` node →
//! MIR three-block CFG (tag-check → then_bb / else_bb → join_bb) →
//! codegen-rs → native execution.
//!
//! Three orthogonal cases:
//!   1. Statement-position `if let` on `Option<i64>` — payload on Some, else on None.
//!   2. Expression-position `if let` used as the RHS of a `let` binding.
//!   3. User-defined enum payload — discriminates by variant tag.

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
    let cargo = std::env::var_os("CARGO").unwrap_or_else(|| "cargo".into());
    let mut command = Command::new(cargo);
    command
        .current_dir(repo)
        .args(["run", "--quiet", "-p", "hew-cli", "--bin", "hew", "--"]);
    command
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
/// Panics if `hew run` exits non-zero or times out.
fn run_hew_source(repo: &Path, stem: &str, source: &str) -> String {
    ensure_hew_runtime_lib(repo);
    let dir = std::env::temp_dir().join(format!("hew-if-let-exec-{}-{stem}", std::process::id()));
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

/// Statement-position `if let Some(v) = opt` prints payload on Some,
/// prints the else value on None.
#[test]
fn if_let_stmt_some_prints_payload_none_prints_else() {
    let repo = repo_root();
    let stdout = run_hew_source(
        &repo,
        "if_let_stmt_option",
        r#"fn main() {
    let some_val: Option<i64> = Some(42);
    if let Some(v) = some_val {
        println(v);
    } else {
        println(-1);
    }
    let none_val: Option<i64> = None;
    if let Some(v) = none_val {
        println(v);
    } else {
        println(-1);
    }
}
"#,
    );
    assert_eq!(
        stdout, "42\n-1",
        "if-let statement: expected '42\\n-1', got {stdout:?}"
    );
}

/// Expression-position `let y = if let Some(v) = opt { v } else { 0 }`.
/// The result is bound to `y` and used downstream.
#[test]
fn if_let_expr_position_binds_result() {
    let repo = repo_root();
    let stdout = run_hew_source(
        &repo,
        "if_let_expr_position",
        r#"fn main() {
    let opt_some: Option<i64> = Some(7);
    let y = if let Some(v) = opt_some { v } else { 0 };
    println(y);
    let opt_none: Option<i64> = None;
    let z = if let Some(v) = opt_none { v } else { 0 };
    println(z);
}
"#,
    );
    assert_eq!(
        stdout, "7\n0",
        "if-let expression: expected '7\\n0', got {stdout:?}"
    );
}

/// User-defined enum payload: `if let Shape::Circle(r) = s` matches the
/// correct variant and binds the payload field; mismatched variant goes
/// to the else branch.
#[test]
fn if_let_user_enum_payload_matches_correct_variant() {
    let repo = repo_root();
    let stdout = run_hew_source(
        &repo,
        "if_let_user_enum",
        r#"enum Shape {
    Circle(i64);
    Square(i64);
}

fn main() {
    let circle: Shape = Shape::Circle(10);
    if let Shape::Circle(r) = circle {
        println(r);
    } else {
        println(-1);
    }
    let square: Shape = Shape::Square(5);
    if let Shape::Circle(r) = square {
        println(r);
    } else {
        println(-1);
    }
}
"#,
    );
    assert_eq!(
        stdout, "10\n-1",
        "if-let user-enum: expected '10\\n-1', got {stdout:?}"
    );
}
