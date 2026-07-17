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
    let dir = std::env::temp_dir().join(format!("hew-prim-bound-{}-{stem}", std::process::id()));
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

fn check_hew_source_fails(repo: &Path, stem: &str, source: &str) -> String {
    ensure_hew_runtime_lib(repo);
    let dir =
        std::env::temp_dir().join(format!("hew-prim-bound-neg-{}-{stem}", std::process::id()));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("create temp source dir");
    let path = dir.join(format!("{stem}.hew"));
    std::fs::write(&path, source).expect("write temp Hew source");

    let output = hew_command(repo)
        .arg("check")
        .arg(&path)
        .output()
        .expect("spawn hew check");
    assert!(
        !output.status.success(),
        "hew check {} must fail (no impl for primitive), but succeeded",
        path.display()
    );
    String::from_utf8(output.stderr).expect("stderr is utf-8")
}

/// A generic function `fn display<T: Show>(x: T)` dispatches correctly when
/// `T = i64` and `impl Show for i64` exists — the monomorphised call returns
/// the trait method's value.
#[test]
fn generic_fn_with_i64_bound_dispatches_trait_method() {
    let repo = repo_root();
    let stdout = run_hew_source(
        &repo,
        "prim_bound_i64",
        r#"
        trait Show {
            fn show(val: Self) -> string;
        }

        impl Show for i64 {
            fn show(val: i64) -> string { "i64" }
        }

        fn display<T: Show>(x: T) -> string {
            x.show()
        }

        fn main() {
            print(display(5));
        }
        "#,
    );
    assert_eq!(stdout, "i64");
}

/// All four common primitive types (i64, f64, bool, string) satisfy a user
/// trait bound when each has an `impl`; each monomorphised call dispatches to
/// its own trait method body.
#[test]
fn generic_fn_with_primitive_bounds_dispatches_all_four_types() {
    let repo = repo_root();
    let stdout = run_hew_source(
        &repo,
        "prim_bound_all_four",
        r#"
        trait Label {
            fn label(val: Self) -> string;
        }

        impl Label for i64    { fn label(val: i64)    -> string { "i64"    } }
        impl Label for f64    { fn label(val: f64)    -> string { "f64"    } }
        impl Label for bool   { fn label(val: bool)   -> string { "bool"   } }
        impl Label for string { fn label(val: string) -> string { "string" } }

        fn tag<T: Label>(x: T) -> string {
            x.label()
        }

        fn main() {
            print(tag(5));
            print(tag(1.5));
            print(tag(true));
            print(tag("hi"));
        }
        "#,
    );
    assert_eq!(stdout, "i64f64boolstring");
}

/// A primitive without a matching `impl Trait for <primitive>` must still
/// produce a type error — the fix must not weaken the bound gate.
#[test]
fn primitive_without_impl_rejected_at_check_time() {
    let repo = repo_root();
    let stderr = check_hew_source_fails(
        &repo,
        "prim_bound_negative",
        r"
        trait Show {
            fn show(val: Self) -> string;
        }

        fn display<T: Show>(x: T) -> string {
            x.show()
        }

        fn main() {
            let _ = display(5);
        }
        ",
    );
    assert!(
        stderr.contains("does not implement trait"),
        "expected 'does not implement trait' in rejection, got:\n{stderr}"
    );
}
