//! End-to-end regression tests for owned-vs-BitCopy `Vec<T>` element-ABI
//! routing.
//!
//! The owned-element Vec ABI (`hew_vec_*_owned`) and the BitCopy layout ABI
//! (`hew_vec_*_layout`) are mutually exclusive: a Vec constructed under one and
//! operated under the other trips the runtime layout-aware abort
//! (`Vec layout-aware operation is not implemented`). The owned-vs-BitCopy
//! decision is one structural authority (a heap-owning element is owned; a
//! payload-free / all-scalar element is BitCopy) threaded to the constructor
//! AND every push/get/free site.
//!
//! These tests pin both abort shapes that a constructor↔push authority
//! disagreement produced:
//!   1. A payload-free / all-scalar-payload enum element must route the BitCopy
//!      ABI on BOTH the constructor and the push (it owns no heap).
//!   2. An array-literal of a genuinely-owned element (`[a, b]`) must route the
//!      owned ABI on the push too — the array-literal desugar must agree with
//!      the owned constructor.

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
        "hew-vec-owned-routing-{}-{stem}",
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

/// `Vec<Color>` where `Color` is a payload-free enum owns no heap; both the
/// constructor and the push must take the BitCopy ABI. The pre-fix constructor
/// over-routed every enum to the owned ABI (its bitcopy probe had no enum arm),
/// while the push correctly stayed BitCopy — the mismatch aborted at runtime.
#[test]
fn vec_payload_free_enum_constructs_and_pushes_bitcopy() {
    let repo = repo_root();
    let stdout = run_hew_source(
        &repo,
        "payload_free_enum",
        r#"
        enum Color { Red; Green; Blue }

        fn main() {
            let v: Vec<Color> = Vec::new();
            v.push(Red);
            v.push(Green);
            v.push(Blue);
            println(v.len());
        }
        "#,
    );
    assert_eq!(stdout.trim(), "3");
}

/// `Vec<Tag>` where every variant payload is a scalar (`i64`) owns no heap, so
/// it must also stay on the BitCopy ABI on both sides — the same root cause as
/// the payload-free case (no enum arm in the bitcopy probe).
#[test]
fn vec_scalar_payload_enum_constructs_and_pushes_bitcopy() {
    let repo = repo_root();
    let stdout = run_hew_source(
        &repo,
        "scalar_payload_enum",
        r#"
        enum Tag { A(i64); B(i64) }

        fn main() {
            let v: Vec<Tag> = Vec::new();
            v.push(A(1));
            v.push(B(2));
            println(v.len());
        }
        "#,
    );
    assert_eq!(stdout.trim(), "2");
}

/// An array literal of a heap-owning enum (`[Circle(3), Named("box")]`) builds a
/// `Vec<Shape>`. The constructor routes the owned ABI; the array-literal push
/// MUST route the owned ABI too. Before the fix, the array-literal desugar
/// emitted the BitCopy `hew_vec_push_layout` unconditionally → abort.
#[test]
fn vec_array_literal_owned_enum_runs() {
    let repo = repo_root();
    let stdout = run_hew_source(
        &repo,
        "array_literal_owned_enum",
        r#"
        enum Shape { Circle(i64); Named(string) }

        fn main() {
            let v: Vec<Shape> = [Circle(3), Named("box")];
            for s in v {
                match s {
                    Circle(r) => println("circle"),
                    Named(n) => println(n),
                }
            }
        }
        "#,
    );
    assert_eq!(stdout.trim(), "circle\nbox");
}

/// An array literal of an owned tuple element (`[(string, string), ...]`) must
/// route the owned push. This is the `curl`-headers shape built via a literal.
#[test]
fn vec_array_literal_owned_tuple_runs() {
    let repo = repo_root();
    let stdout = run_hew_source(
        &repo,
        "array_literal_owned_tuple",
        r#"
        fn main() {
            let v: Vec<(string, string)> = [("Content-Type", "application/json"), ("Accept", "text/plain")];
            for pair in v {
                let (k, val) = pair;
                print(k); print(" "); println(val);
            }
        }
        "#,
    );
    assert_eq!(
        stdout.trim(),
        "Content-Type application/json\nAccept text/plain"
    );
}

/// An array literal of an owned record element (`[Person { ... }, ...]`) must
/// route the owned push and run clean.
#[test]
fn vec_array_literal_owned_record_runs() {
    let repo = repo_root();
    let stdout = run_hew_source(
        &repo,
        "array_literal_owned_record",
        r#"
        type Person { name: string; role: string }

        fn main() {
            let v: Vec<Person> = [Person { name: "Ada", role: "eng" }, Person { name: "Linus", role: "eng" }];
            for p in v {
                print(p.name); print(" "); println(p.role);
            }
        }
        "#,
    );
    assert_eq!(stdout.trim(), "Ada eng\nLinus eng");
}
