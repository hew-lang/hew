//! End-to-end execution tests for `for line in vec_of_strings` over a
//! `Vec<string>`.
//!
//! `hew_vec_get_str` returns a FRESH, solely-owned retained owner of the element
//! (a `hew_string_clone` refcount bump — NOT a borrow of the Vec's live buffer
//! slot). The for-in lowering therefore owes the iteration binding exactly one
//! `hew_string_drop` on EVERY path out of the loop body: the fall-through
//! back-edge, each branch-arm join, and the `break`/`continue` edges. An early
//! `return line` moves the single retained reference to the caller, so the
//! body-end drop is suppressed on that path (leak-not-double-free; the move
//! checker / function-scope machinery owns the escaped reference).
//!
//! These tests prove the real program behaviour, not just the lowered shape:
//! they compile + link + run each body shape and assert the exact stdout. The
//! MIR-shape companions (drop count / escape suppression / no-NYI) live in
//! `hew-mir/tests/cstring_container_domain_canary.rs`; the no-double-free /
//! no-extra-leak property is validated out-of-band under the macOS malloc
//! debugger (`MallocScribble`) and `leaks`, where the for-in path is byte-for-
//! byte equivalent to the established `for i in 0..len { let line = xs.get(i) }`
//! path.

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

fn run_hew_source(repo: &Path, stem: &str, source: &str) -> String {
    ensure_hew_runtime_lib(repo);
    let dir = std::env::temp_dir().join(format!(
        "hew-for-in-vecstring-{}-{stem}",
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

/// The headline demo shape: a branched body that prefixes non-empty lines. Both
/// arms read `line` by-value via string concat (a borrow), so the retained
/// element is released once at the branch-join per iteration. `split("\n")` on
/// `"a\nb\n"` yields `["a", "b", ""]`, so the empty trailing segment takes the
/// else arm.
#[test]
fn for_in_branched_transform_body_runs_correct() {
    let repo = repo_root();
    let stdout = run_hew_source(
        &repo,
        "branched",
        r#"
        fn transform_body(body: string) -> string {
            var out = "";
            for line in body.split("\n") {
                if line.len() > 0 { out = out + "PROXY> " + line + "\n"; }
                else { out = out + "\n"; }
            }
            out
        }
        fn main() { print(transform_body("a\nb\n")); }
        "#,
    );
    assert_eq!(stdout, "PROXY> a\nPROXY> b\n\n");
}

/// A simple linear body that concatenates every element.
#[test]
fn for_in_simple_concat_runs_correct() {
    let repo = repo_root();
    let stdout = run_hew_source(
        &repo,
        "simple",
        r#"
        fn join_all(lines: Vec<string>) -> string {
            var out = "";
            for line in lines { out = out + line; }
            out
        }
        fn main() {
            let v: Vec<string> = Vec::new();
            v.push("foo"); v.push("bar"); v.push("baz");
            print(join_all(v));
        }
        "#,
    );
    assert_eq!(stdout, "foobarbaz");
}

/// `continue` skips empty elements; the continued iteration's retained element
/// is freed on the continue edge.
#[test]
fn for_in_continue_skips_empty_runs_correct() {
    let repo = repo_root();
    let stdout = run_hew_source(
        &repo,
        "continue",
        r#"
        fn keep_nonempty(lines: Vec<string>) -> string {
            var out = "";
            for line in lines {
                if line.len() == 0 { continue; }
                out = out + line + ";";
            }
            out
        }
        fn main() {
            let v: Vec<string> = Vec::new();
            v.push("a"); v.push(""); v.push("b"); v.push(""); v.push("c");
            print(keep_nonempty(v));
        }
        "#,
    );
    assert_eq!(stdout, "a;b;c;");
}

/// `break` stops at a sentinel; the breaking iteration's retained element is
/// freed on the break edge before the loop-exit goto.
#[test]
fn for_in_break_at_sentinel_runs_correct() {
    let repo = repo_root();
    let stdout = run_hew_source(
        &repo,
        "break",
        r#"
        fn until_stop(lines: Vec<string>) -> string {
            var out = "";
            for line in lines {
                if line == "STOP" { break; }
                out = out + line + ",";
            }
            out
        }
        fn main() {
            let v: Vec<string> = Vec::new();
            v.push("one"); v.push("two"); v.push("STOP"); v.push("three");
            print(until_stop(v));
        }
        "#,
    );
    assert_eq!(stdout, "one,two,");
}

/// An unused binding still drops each retained element; the count is correct.
#[test]
fn for_in_unused_binding_runs_correct() {
    let repo = repo_root();
    let stdout = run_hew_source(
        &repo,
        "unused",
        r#"
        fn count_iters(lines: Vec<string>) -> i64 {
            var n = 0;
            for line in lines { n = n + 1; }
            n
        }
        fn main() {
            let v: Vec<string> = Vec::new();
            v.push("x"); v.push("y"); v.push("z");
            print(f"{count_iters(v)}");
        }
        "#,
    );
    assert_eq!(stdout, "3");
}

/// Ownership escape: `return line` hands the retained reference to the caller.
/// The body-end drop is suppressed; the returned value is printed intact (no
/// use-after-free of a wrongly-dropped buffer).
#[test]
fn for_in_returned_binding_runs_correct() {
    let repo = repo_root();
    let stdout = run_hew_source(
        &repo,
        "return_escape",
        r#"
        fn first_match(lines: Vec<string>, needle: string) -> string {
            for line in lines {
                if line == needle { return line; }
            }
            "none"
        }
        fn main() {
            let v: Vec<string> = Vec::new();
            v.push("alpha"); v.push("beta"); v.push("gamma");
            print(first_match(v, "beta"));
        }
        "#,
    );
    assert_eq!(stdout, "beta");
}

/// High-iteration branched body (10000 retained-then-dropped elements). A
/// double-free of any element would corrupt the accumulated result or abort
/// under the malloc debugger; a leaked element would show as a growing RSS but
/// the result stays exact. 200 rounds x 50 lines x 7 chars per line = 70000.
#[test]
fn for_in_stress_branched_runs_correct() {
    let repo = repo_root();
    let stdout = run_hew_source(
        &repo,
        "stress",
        r#"
        fn build(n: i64) -> Vec<string> {
            var v: Vec<string> = Vec::new();
            for i in 0..n {
                if i % 3 == 0 { v.push("fizz"); } else { v.push("buzz"); }
            }
            v
        }
        fn transform(lines: Vec<string>) -> string {
            var out = "";
            for line in lines {
                if line.len() > 3 { out = out + "L:" + line + "\n"; }
                else { out = out + "S:" + line + "\n"; }
            }
            out
        }
        fn main() {
            var total = 0;
            for round in 0..200 {
                let lines = build(50);
                let s = transform(lines);
                total = total + s.len();
            }
            print(f"{total}");
        }
        "#,
    );
    assert_eq!(stdout, "70000");
}
