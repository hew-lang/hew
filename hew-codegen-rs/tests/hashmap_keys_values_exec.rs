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

fn check_hew_source(repo: &Path, stem: &str, source: &str) -> (i32, String) {
    let dir = std::env::temp_dir().join(format!(
        "hew-hashmap-kv-check-{}-{stem}",
        std::process::id()
    ));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("create temp source dir");
    let path = dir.join(format!("{stem}.hew"));
    std::fs::write(&path, source).expect("write temp Hew source");

    let mut cmd = hew_command(repo);
    cmd.arg("check").arg(&path);
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

/// `values()` on a Copy-record value is not an admitted surface today: the
/// projection would produce an owned `Vec<Point>` whose element descriptor is
/// not seeded by the current lowering path. The checker must reject it instead
/// of letting execution reach a layout/runtime abort.
#[test]
fn hashmap_values_copy_record_rejected_at_check() {
    let repo = repo_root();
    let (code, stderr) = check_hew_source(
        &repo,
        "hashmap_values_copy_record_reject",
        r#"record Point { x: i64, y: i64 }

fn main() -> i64 {
    var m: HashMap<i64, Point> = HashMap::new();
    m.insert(1, Point { x: 10, y: 20 });
    let vs = m.values();
    let got = vs.get(0);
    got.x + got.y
}
"#,
    );
    assert_eq!(
        code, 1,
        "values() on Copy-record value must fail at check time; stderr:\n{stderr}"
    );
    assert!(
        stderr.contains("HashMap<i64, Point>.values()` is not yet supported"),
        "diagnostic must name the unsupported values() projection; stderr:\n{stderr}"
    );
}

/// `keys()` on a Copy-record key is likewise fail-closed: the projection would
/// produce an owned `Vec<Point>`, so the checker rejects it until the managed /
/// layout-backed projection path exists end-to-end.
#[test]
fn hashmap_keys_copy_record_rejected_at_check() {
    let repo = repo_root();
    let (code, stderr) = check_hew_source(
        &repo,
        "hashmap_keys_copy_record_reject",
        r#"record Point { x: i64, y: i64 }

fn main() -> i64 {
    var m: HashMap<Point, i64> = HashMap::new();
    m.insert(Point { x: 3, y: 7 }, 99);
    let ks = m.keys();
    let got = ks.get(0);
    got.x + got.y
}
"#,
    );
    assert_eq!(
        code, 1,
        "keys() on Copy-record key must fail at check time; stderr:\n{stderr}"
    );
    assert!(
        stderr.contains("HashMap<Point, i64>.keys()` is not yet supported"),
        "diagnostic must name the unsupported keys() projection; stderr:\n{stderr}"
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
