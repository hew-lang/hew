//! Execution tests for `bytes` element loads through `hew_bytes_index`.
//!
//! A `bytes` value is a stack-resident `BytesTriple { ptr, offset, len }`, NOT
//! a `*mut HewVec`, so the `b[i]` indexing sugar routes to the dedicated
//! `hew_bytes_index(ptr, offset, len, index) -> u8` runtime getter (not the
//! heap-Vec element getter `hew_vec_get_i32`, whose arg0 ABI is a Vec `ptr`),
//! and `bytes.get(i)` lowers through the `hew_bytes_get` codegen arm to an
//! in-bounds `Some(u8)` / out-of-bounds `None`. This was previously a
//! fail-closed codegen-front error (`hew_vec_get_i32 arg0 resolves to
//! non-pointer type {ptr,i32,i32}`); the `hew_bytes_index` codegen arm unpacks
//! the triple into the runtime's (ptr, offset, len) args.

#![cfg(not(target_arch = "wasm32"))]
#![cfg(unix)]

use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::OnceLock;

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

fn run_hew_source(repo: &Path, stem: &str, source: &str) -> String {
    ensure_hew_runtime_lib(repo);
    let dir = std::env::temp_dir().join(format!("hew-bytes-index-{}-{stem}", std::process::id()));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("create temp source dir");
    let path = dir.join(format!("{stem}.hew"));
    std::fs::write(&path, source).expect("write temp Hew source");

    let output = hew_command(repo)
        .arg("run")
        .arg(&path)
        .output()
        .expect("spawn hew run");
    assert!(
        output.status.success(),
        "hew run {} exited non-zero (status={:?}); stderr:\n{}",
        path.display(),
        output.status,
        String::from_utf8_lossy(&output.stderr)
    );
    String::from_utf8(output.stdout).expect("stdout is utf-8")
}

/// `b[i]` indexing sugar over a `bytes` value loads the byte at the offset and
/// must compile (no codegen-front `{ptr,i32,i32}` arg0 error) and read back the
/// pushed bytes. `b[i]` yields `u8`; compare against `u8` literals (there is no
/// `print(u8)` overload, so the assertion is encoded as a printed marker).
#[test]
fn bytes_index_sugar_reads_pushed_bytes() {
    let repo = repo_root();
    let stdout = run_hew_source(
        &repo,
        "bytes_index_sugar",
        r#"
        import std::io;

        fn main() {
            var b: bytes = bytes::new();
            b.push(65);
            b.push(66);
            b.push(67);
            let first = b[0];
            let last = b[2];
            if first == 65 {
                if last == 67 {
                    print("PASS");
                }
            }
        }
        "#,
    );

    assert_eq!(stdout, "PASS");
}

/// `bytes.get(i)` returns `Option<u8>`: `Some(byte)` in bounds, `None` out of
/// bounds (de-aliased from the trapping `b[i]`). The high byte 200 (>127) must
/// round-trip through the `Some` payload as 200, not a sign-extended negative.
#[test]
fn bytes_get_method_returns_option() {
    let repo = repo_root();
    let stdout = run_hew_source(
        &repo,
        "bytes_get_method",
        r#"
        fn main() {
            var b: bytes = bytes::new();
            b.push(200);
            b.push(1);
            match b.get(0) {
                Some(high) => {
                    if high == 200 {
                        print("A");
                    }
                },
                None => {
                    print("x");
                },
            }
            match b.get(1) {
                Some(low) => {
                    if low == 1 {
                        print("B");
                    }
                },
                None => {
                    print("x");
                },
            }
            match b.get(9) {
                Some(_) => {
                    print("x");
                },
                None => {
                    print("C");
                },
            }
        }
        "#,
    );

    // A = Some(200) with the high byte preserved (not sign-extended);
    // B = Some(1); C = None for the out-of-bounds index 9.
    assert_eq!(stdout, "ABC");
}
