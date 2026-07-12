//! End-to-end execution tests for `HashMap.keys()` and `HashMap.values()`.
//!
//! Verifies the full pipeline: checker wiring → MIR passthrough → codegen
//! → runtime execution.  The grounding oracle is the values-sum fixture:
//! three entries summing to 42 (10+20+12).
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

fn ensure_hew_cli(repo: &Path) {
    static BUILT: OnceLock<()> = OnceLock::new();
    BUILT.get_or_init(|| {
        let cargo = std::env::var_os("CARGO").unwrap_or_else(|| "cargo".into());
        let status = Command::new(cargo)
            .current_dir(repo)
            .args(["build", "--quiet", "-p", "hew-cli", "--bin", "hew"])
            .status()
            .expect("spawn cargo build -p hew-cli --bin hew");
        assert!(
            status.success(),
            "cargo build -p hew-cli --bin hew failed: {status:?}"
        );
        assert!(
            hew_bin(repo).exists(),
            "hew binary missing after build: {}",
            hew_bin(repo).display()
        );
    });
}

fn hew_command(repo: &Path) -> Command {
    ensure_hew_cli(repo);
    let bin = hew_bin(repo);
    Command::new(bin)
}

fn ensure_hew_runtime_lib(repo: &Path) {
    let _ = repo;
    static BUILT: OnceLock<()> = OnceLock::new();
    BUILT.get_or_init(|| {
        hew_testutil::ensure_hew_lib_built().expect("build libhew.a");
    });
}

/// Compile and run a Hew snippet; return the exit code.
///
/// Writing to a temp file so the hew binary gets a real path. A `timeout`
/// of 20 s bounds the run; the child is killed on expiry.
fn run_hew_source_exit_code(repo: &Path, stem: &str, source: &str) -> i32 {
    ensure_hew_runtime_lib(repo);
    let dir = std::env::temp_dir().join(format!("hew-hashmap-kv-{}-{}", std::process::id(), stem));
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

fn check_hew_file(repo: &Path, path: &Path) -> (i32, String) {
    let mut cmd = hew_command(repo);
    cmd.arg("check").arg(path);
    let output = hew_testutil::run_command_bounded(
        &mut cmd,
        format!("hew check {}", path.display()),
        Duration::from_secs(20),
    )
    .unwrap_or_else(|e| panic!("{e}"));
    let mut text = String::from_utf8_lossy(&output.stdout).into_owned();
    text.push_str(&String::from_utf8_lossy(&output.stderr));
    (output.status.code().unwrap_or(125), text)
}

fn assert_single_check_error_with_exact_diagnostic(output: &str, expected: &str) {
    let error_count = output.matches(": error:").count();
    assert_eq!(
        error_count, 1,
        "expected exactly one checker error, got {error_count}; output:\n{output}"
    );
    assert!(
        output.contains(expected),
        "expected exact diagnostic:\n{expected}\n\noutput:\n{output}"
    );
}

/// Grounding oracle: `HashMap<string, i64>.values()` iterated with `for`,
/// sum = 10+20+12 = 42.  Exit code encodes the return value of main.
#[test]
fn hashmap_values_sum_oracle_exits_42() {
    let repo = repo_root();
    let code = run_hew_source_exit_code(
        &repo,
        "hashmap_values_sum",
        r#"fn main() -> i64 {
    var m: HashMap<string, i64> = HashMap::new();
    m.insert("a", 10);
    m.insert("b", 20);
    m.insert("c", 12);
    let vs = m.values();
    var total: i64 = 0;
    for v in vs {
        total = total + v;
    }
    total
}
"#,
    );
    assert_eq!(
        code, 42,
        "values() sum oracle: expected exit 42, got {code}"
    );
}

/// `keys()` on a 3-entry map returns a Vec whose len is 3.
#[test]
fn hashmap_keys_len_is_map_size() {
    let repo = repo_root();
    let code = run_hew_source_exit_code(
        &repo,
        "hashmap_keys_len",
        r#"fn main() -> i64 {
    var m: HashMap<string, i64> = HashMap::new();
    m.insert("x", 1);
    m.insert("y", 2);
    m.insert("z", 3);
    let ks = m.keys();
    ks.len()
}
"#,
    );
    assert_eq!(code, 3, "keys() len oracle: expected exit 3, got {code}");
}

/// Regression: `values().get(i)` on a `HashMap<i64, Point>` where `Point` is a
/// Copy-record value type must return correct field values through the layout
/// pointer.
///
/// Previously `hew_hashmap_values_layout` built `HewTypeLayout` as a stack
/// local and passed its address to `hew_vec_new_with_layout`, which aliased the
/// pointer without copying.  After returning, the vec's `layout` field pointed
/// into the destroyed frame -> any layout-aware op (`.get()` calls
/// `validate_bitcopy_layout_operation`, which reads `align`) triggered
/// `PANIC: HewTypeLayout align must be a non-zero power of two`.
///
/// Oracle uses `x + y = 30`, which fits in a byte (exit codes are mod-256).
#[test]
fn hashmap_values_get_layout_path_copy_record_returns_correct_fields() {
    let repo = repo_root();
    let code = run_hew_source_exit_code(
        &repo,
        "hashmap_values_get_layout",
        r#"record Point { x: i64, y: i64 }

fn main() -> i64 {
    var m: HashMap<i64, Point> = HashMap::new();
    m.insert(1, Point { x: 10, y: 20 });
    let vs = m.values();
    let got = vs[0];
    got.x + got.y
}
"#,
    );
    assert_eq!(
        code, 30,
        "values().get(0) on Copy-record value: expected exit 30 (x=10+y=20), got {code}"
    );
}

/// Regression: `keys().get(i)` on a `HashMap<Point, i64>` where `Point` is a
/// Copy-record key type must return correct field values.
///
/// Mirrors the values() regression above; both `hew_hashmap_keys_layout` and
/// `hew_hashmap_values_layout` had the same stack-local layout pointer bug.
///
/// Oracle uses `x + y = 10`, which fits in a byte (exit codes are mod-256).
#[test]
fn hashmap_keys_get_layout_path_copy_record_returns_correct_fields() {
    let repo = repo_root();
    let code = run_hew_source_exit_code(
        &repo,
        "hashmap_keys_get_layout",
        r#"record Point { x: i64, y: i64 }

fn main() -> i64 {
    var m: HashMap<Point, i64> = HashMap::new();
    m.insert(Point { x: 3, y: 7 }, 99);
    let ks = m.keys();
    let got = ks[0];
    got.x + got.y
}
"#,
    );
    assert_eq!(
        code, 10,
        "keys().get(0) on Copy-record key: expected exit 10 (x=3+y=7), got {code}"
    );
}

#[test]
fn hashmap_values_heap_bearing_record_rejected_with_one_exact_diagnostic() {
    let repo = repo_root();
    let fixture = repo.join("tests/vertical-slice/reject/hashmap_values_managed_record.hew");
    let (code, output) = check_hew_file(&repo, &fixture);
    assert_eq!(
        code, 1,
        "heap-bearing values() projection must fail at check time; output:\n{output}"
    );
    assert_single_check_error_with_exact_diagnostic(
        &output,
        "`HashMap<i64, User>.values()` is not yet supported: projecting from a map with value type `User` into an owned `Vec` is not lowered; supported projection value types are scalar primitives, `string`, and Copy record/enum types",
    );
}

#[test]
fn hashmap_keys_bytes_rejected_by_key_branch_with_one_exact_diagnostic() {
    let repo = repo_root();
    let fixture = repo.join("tests/vertical-slice/reject/hashmap_keys_bytes.hew");
    let (code, output) = check_hew_file(&repo, &fixture);
    assert_eq!(
        code, 1,
        "bytes keys() projection must fail at check time; output:\n{output}"
    );
    assert_single_check_error_with_exact_diagnostic(
        &output,
        "`HashMap<bytes, i64>.keys()` is not yet supported: projecting key type `bytes` into an owned `Vec` is not lowered; supported projection key types are scalar primitives, `string`, and Copy record/enum types",
    );
}

/// Empty map: both `keys()` and `values()` return empty Vecs (len 0).
#[test]
fn hashmap_keys_values_empty_map_returns_empty_vecs() {
    let repo = repo_root();
    let code = run_hew_source_exit_code(
        &repo,
        "hashmap_kv_empty",
        r#"fn main() -> i64 {
    var m: HashMap<string, i64> = HashMap::new();
    let ks = m.keys();
    let vs = m.values();
    ks.len() + vs.len()
}
"#,
    );
    assert_eq!(
        code, 0,
        "empty-map keys()+values() len oracle: expected 0, got {code}"
    );
}
