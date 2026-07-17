//! End-to-end regression tests for Vec<record> element reads via subscript,
//! for-in, and `.get()`.
//!
//! These tests were added to catch the silent-wrong-result bug where
//! `xs[i]` and `for x in xs` used `hew_vec_get_ptr` (8-byte pointer stride)
//! for BitCopy Named record element types, returning garbage for any element
//! wider than 8 bytes. After the fix, all three read paths (`xs[i]`,
//! `for x in xs`, `.get(i)`) route through `hew_vec_get_layout` for
//! value-record element types, which applies the correct element stride.

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
    let dir = std::env::temp_dir().join(format!(
        "hew-vec-record-index-{}-{stem}",
        std::process::id()
    ));
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

/// `xs[i]` on a Vec<Point> must read the correct field values for a record
/// element type whose size exceeds 8 bytes (the pointer stride).
///
/// Before the fix, `lower_vec_index` emitted `hew_vec_get_ptr` for Named types
/// regardless of whether they were value records or heap handles. For a
/// 16-byte `Point{x:i64, y:i64}`, pointer stride returns the wrong `y` value.
#[test]
fn vec_record_subscript_reads_correct_field_values() {
    let repo = repo_root();
    let stdout = run_hew_source(
        &repo,
        "vec_record_subscript",
        r#"
        type Point { x: i64; y: i64 }

        fn main() {
            let pts = [Point{x:10, y:20}, Point{x:30, y:40}];
            let a = pts[0];
            let b = pts[1];
            print(a.x); print(" "); print(a.y); print(" ");
            print(b.x); print(" "); println(b.y);
        }
        "#,
    );
    assert_eq!(stdout.trim(), "10 20 30 40");
}

/// `for x in xs` on a Vec<Point> must yield the correct field values for each
/// element. Before the fix, the VecIter desugaring emitted `hew_vec_get_ptr`
/// via the Index path, corrupting y fields for wide records.
#[test]
fn vec_record_for_in_reads_correct_field_values() {
    let repo = repo_root();
    let stdout = run_hew_source(
        &repo,
        "vec_record_for_in",
        r#"
        type Point { x: i64; y: i64 }

        fn main() {
            let pts = [Point{x:1, y:2}, Point{x:3, y:4}, Point{x:5, y:6}];
            for p in pts {
                print(p.x); print(" "); println(p.y);
            }
        }
        "#,
    );
    assert_eq!(stdout.trim(), "1 2\n3 4\n5 6");
}

/// `.get(i)` on a Vec<Point> must also return the correct field values. This
/// path was already correct (routing through `hew_vec_get_layout` via the
/// checker's method-rewrite table). This test anchors it as a regression guard.
#[test]
fn vec_record_get_method_reads_correct_field_values() {
    let repo = repo_root();
    let stdout = run_hew_source(
        &repo,
        "vec_record_get_method",
        r#"
        type Point { x: i64; y: i64 }

        fn main() {
            let pts: Vec<Point> = Vec::new();
            pts.push(Point{x:100, y:200});
            pts.push(Point{x:300, y:400});
            let a = pts[0];
            let b = pts[1];
            print(a.x); print(" "); print(a.y); print(" ");
            print(b.x); print(" "); println(b.y);
        }
        "#,
    );
    assert_eq!(stdout.trim(), "100 200 300 400");
}

/// Vec<(i64, i64)> subscript must read the correct tuple fields. Tuples are
/// BitCopy aggregates stored inline at the full element stride; they also
/// route through `hew_vec_get_layout` after the fix.
#[test]
fn vec_tuple_subscript_reads_correct_field_values() {
    let repo = repo_root();
    let stdout = run_hew_source(
        &repo,
        "vec_tuple_subscript",
        r#"
        fn main() {
            let pairs = [(1, 2), (3, 4), (5, 6)];
            let first = pairs[0];
            let last = pairs[2];
            print(first.0); print(" "); print(first.1); print(" ");
            print(last.0); print(" "); println(last.1);
        }
        "#,
    );
    assert_eq!(stdout.trim(), "1 2 5 6");
}

/// `for x in xs` on a Vec<(i64, i64)> must yield the correct tuple fields for
/// each element. The for-in desugaring uses the same Index path as subscript,
/// so this anchors that the layout-getter routing is also in effect for the
/// iterator path on tuples.
#[test]
fn vec_tuple_for_in_reads_correct_field_values() {
    let repo = repo_root();
    let stdout = run_hew_source(
        &repo,
        "vec_tuple_for_in",
        r#"
        fn main() {
            let pairs = [(1, 2), (3, 4), (5, 6)];
            for p in pairs {
                print(p.0); print(" "); println(p.1);
            }
        }
        "#,
    );
    assert_eq!(stdout.trim(), "1 2\n3 4\n5 6");
}
