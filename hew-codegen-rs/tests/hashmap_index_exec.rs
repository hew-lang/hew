//! End-to-end execution tests for `HashMap` key indexing: `m[k]` read and
//! `m[k] = v` write.
//!
//! Verifies the full pipeline: checker key-bound + Option/value-type wiring →
//! HIR routing (read → get ResolvedImplCall, write → insert) → MIR insert
//! lowering → codegen → runtime execution. Exit codes (mod-256) encode the
//! grounding oracle.
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
    static BUILT: OnceLock<()> = OnceLock::new();
    BUILT.get_or_init(|| {
        let lib = target_dir(repo).join("debug").join("libhew.a");
        // Always ask cargo to (re)build libhew.a rather than short-circuiting on
        // its mere presence. Cargo's own fingerprint makes this a fast no-op when
        // the archive is current and regenerates it when the toolchain (rustc /
        // bundled LLVM) or the runtime/stdlib sources changed. A bare
        // `lib.exists()` early-return reused a stale archive after a toolchain
        // upgrade, linking a freshly-rebuilt `hew` against old object code and
        // failing the link.
        let cargo = std::env::var_os("CARGO").unwrap_or_else(|| "cargo".into());
        let status = Command::new(cargo)
            .current_dir(repo)
            .args(["build", "--quiet", "-p", "hew-lib"])
            .status()
            .expect("spawn cargo build -p hew-lib");
        assert!(
            status.success(),
            "cargo build -p hew-lib failed: {status:?}"
        );
        assert!(
            lib.exists(),
            "libhew.a missing after build: {}",
            lib.display()
        );
    });
}

/// Compile and run a Hew snippet; return the exit code.
fn run_hew_source_exit_code(repo: &Path, stem: &str, source: &str) -> i32 {
    ensure_hew_runtime_lib(repo);
    let dir =
        std::env::temp_dir().join(format!("hew-hashmap-index-{}-{}", std::process::id(), stem));
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

    output.status.code().unwrap_or_else(|| {
        panic!(
            "hew run was killed by signal; stderr:\n{}",
            String::from_utf8_lossy(&output.stderr)
        )
    })
}

/// Write via `m[k] = v`, then read back via `m[k]`. Oracle: the written value
/// flows back out unchanged. `Some(42)` → exit 42; the `None` arm would exit 0,
/// so a non-zero exit also proves the key was present.
#[test]
fn string_key_write_then_read_returns_written_value() {
    let repo = repo_root();
    let code = run_hew_source_exit_code(
        &repo,
        "string_write_read",
        r#"fn main() -> i64 {
    var m: HashMap<string, i64> = HashMap::new();
    m["answer"] = 42;
    match m["answer"] {
        Some(v) => v,
        None => 0,
    }
}
"#,
    );
    assert_eq!(
        code, 42,
        "write-then-read oracle: expected exit 42, got {code}"
    );
}

/// A second `m[k] = v` on an existing key overwrites the prior value.
#[test]
fn string_key_write_overwrites_existing_value() {
    let repo = repo_root();
    let code = run_hew_source_exit_code(
        &repo,
        "string_overwrite",
        r#"fn main() -> i64 {
    var m: HashMap<string, i64> = HashMap::new();
    m["k"] = 10;
    m["k"] = 99;
    match m["k"] {
        Some(v) => v,
        None => 0,
    }
}
"#,
    );
    assert_eq!(code, 99, "overwrite oracle: expected exit 99, got {code}");
}

/// Reading an absent key yields `None`; the `None` arm encodes 7 so a stray
/// `Some` (or a present key) would not produce this exit code.
#[test]
fn read_absent_key_yields_none() {
    let repo = repo_root();
    let code = run_hew_source_exit_code(
        &repo,
        "absent_none",
        r#"fn main() -> i64 {
    var m: HashMap<string, i64> = HashMap::new();
    m["present"] = 1;
    match m["missing"] {
        Some(_) => 0,
        None => 7,
    }
}
"#,
    );
    assert_eq!(
        code, 7,
        "absent-key None oracle: expected exit 7, got {code}"
    );
}

/// The key bound generalises beyond `string`: an `i64` key (Hash + Eq) indexes
/// just as well. Oracle sums two written values, 11 + 22 = 33.
#[test]
fn i64_key_write_then_read() {
    let repo = repo_root();
    let code = run_hew_source_exit_code(
        &repo,
        "i64_write_read",
        r"fn main() -> i64 {
    var m: HashMap<i64, i64> = HashMap::new();
    m[1] = 11;
    m[2] = 22;
    let a = match m[1] {
        Some(v) => v,
        None => 0,
    };
    let b = match m[2] {
        Some(v) => v,
        None => 0,
    };
    a + b
}
",
    );
    assert_eq!(
        code, 33,
        "i64-key oracle: expected exit 33 (11+22), got {code}"
    );
}

/// `m[k]` read agrees with `m.get(k)`: both yield `Option<V>` from the same
/// runtime path, so indexing a key written via `.insert` reads back the value.
#[test]
fn index_read_agrees_with_method_insert() {
    let repo = repo_root();
    let code = run_hew_source_exit_code(
        &repo,
        "index_get_insert_parity",
        r#"fn main() -> i64 {
    var m: HashMap<string, i64> = HashMap::new();
    m.insert("v", 55);
    match m["v"] {
        Some(v) => v,
        None => 0,
    }
}
"#,
    );
    assert_eq!(
        code, 55,
        "index/insert parity oracle: expected exit 55, got {code}"
    );
}
