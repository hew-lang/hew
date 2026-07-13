//! End-to-end execution tests for `HashMap` key indexing: the trapping `m[k]`
//! read, the `m[k] = v` write, and the `m.get(k) -> Option<V>` sibling.
//!
//! Verifies the full pipeline for the indexed-accessor INVERSION. In read
//! position `m[k]` is the trapping `Index::at` accessor (`-> V`): an absent key
//! aborts with `IndexOutOfBounds` (the map analogue of `v[i]` OOB), NOT an
//! `Option` round-trip. The non-aborting `Option<V>` outcome lives on
//! `m.get(k)`, which is re-routed through the `Index` trait (runtime-identical).
//! The write `m[k] = v` lowers to
//! `hew_hashmap_insert_layout`.
//!
//! Pipeline under test: checker key-bound and value-type wiring; HIR routing
//! (read to a plain `Index` node, `get` to a `ResolvedImplCall`, write to
//! insert); MIR `lower_hashmap_index_trap` or insert; codegen; runtime
//! execution. Exit codes (mod-256) encode the grounding oracle; the trap test
//! asserts the `IndexOutOfBounds` abort.
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

/// Compile and run a Hew snippet expected to TRAP at runtime; return
/// `(exit_code, stderr)`. `hew run` catches the abort, prints
/// `hew: trap in main context: <kind>` to stderr, and exits non-zero (1) — a
/// clean exit code, not a propagated signal — so the oracle asserts on both the
/// non-zero code and the trap kind in stderr.
fn run_hew_source_trap(repo: &Path, stem: &str, source: &str) -> (i32, String) {
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

    let code = output.status.code().unwrap_or_else(|| {
        panic!(
            "hew run was killed by signal (expected a clean trap exit); stderr:\n{}",
            String::from_utf8_lossy(&output.stderr)
        )
    });
    let stderr = String::from_utf8_lossy(&output.stderr).into_owned();
    (code, stderr)
}

/// Write via `m[k] = v`, then read back via the trapping `m[k]` accessor. Oracle:
/// the written value flows back out unchanged as a BARE `V` (no `Option`). Exit
/// 42 proves the present key read the written value directly.
#[test]
fn string_key_write_then_read_returns_written_value() {
    let repo = repo_root();
    let code = run_hew_source_exit_code(
        &repo,
        "string_write_read",
        r#"fn main() -> i64 {
    var m: HashMap<string, i64> = HashMap::new();
    m["answer"] = 42;
    m["answer"]
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
    m["k"]
}
"#,
    );
    assert_eq!(code, 99, "overwrite oracle: expected exit 99, got {code}");
}

/// Reading an ABSENT key now TRAPS (`IndexOutOfBounds`) — the inverted accessor:
/// `m[k]` is the trapping `Index::at` (`-> V`), the map analogue of a `v[i]` OOB
/// abort. The present key read above proves the happy path, so a trap here proves
/// the miss path aborts rather than returning a sentinel. The non-aborting
/// `Option<V>` outcome is `m.get(k)` (see `method_get_returns_option_some_and_none`).
#[test]
fn read_absent_key_traps() {
    let repo = repo_root();
    let (code, stderr) = run_hew_source_trap(
        &repo,
        "absent_trap",
        r#"fn main() -> i64 {
    var m: HashMap<string, i64> = HashMap::new();
    m["present"] = 1;
    let r: i64 = m["missing"];
    r
}
"#,
    );
    assert_ne!(
        code, 0,
        "absent-key read must trap (non-zero exit), got {code}; stderr:\n{stderr}"
    );
    assert!(
        stderr.contains("IndexOutOfBounds"),
        "absent-key read must trap with IndexOutOfBounds; exit={code}, stderr:\n{stderr}"
    );
}

/// The key bound generalises beyond `string`: an `i64` key (Hash + Eq) indexes
/// just as well. Oracle sums two written values read back as bare `V`, 11 + 22 = 33.
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
    m[1] + m[2]
}
",
    );
    assert_eq!(
        code, 33,
        "i64-key oracle: expected exit 33 (11+22), got {code}"
    );
}

/// The trapping `m[k]` read agrees with `m.insert(k, v)`: indexing a key written
/// via `.insert` reads back the value as a bare `V` (same runtime table, same
/// clone choke).
#[test]
fn index_read_agrees_with_method_insert() {
    let repo = repo_root();
    let code = run_hew_source_exit_code(
        &repo,
        "index_get_insert_parity",
        r#"fn main() -> i64 {
    var m: HashMap<string, i64> = HashMap::new();
    m.insert("v", 55);
    m["v"]
}
"#,
    );
    assert_eq!(
        code, 55,
        "index/insert parity oracle: expected exit 55, got {code}"
    );
}

/// `m.get(k)` keeps the non-aborting `Option<V>` contract (re-routed through the
/// `Index` trait, runtime-identical to before): present → `Some`,
/// absent → `None`. Oracle: `Some(40)` hit + `None` miss-marker 2 = 42. This is
/// the sibling of the trapping `m[k]` read above.
#[test]
fn method_get_returns_option_some_and_none() {
    let repo = repo_root();
    let code = run_hew_source_exit_code(
        &repo,
        "method_get_option",
        r#"fn main() -> i64 {
    var m: HashMap<string, i64> = HashMap::new();
    m.insert("k", 40);
    let hit = match m.get("k") {
        Some(v) => v,
        None => 0,
    };
    let miss = match m.get("absent") {
        Some(_) => 0,
        None => 2,
    };
    hit + miss
}
"#,
    );
    assert_eq!(
        code, 42,
        "m.get Some/None oracle: expected 42 (40+2), got {code}"
    );
}
