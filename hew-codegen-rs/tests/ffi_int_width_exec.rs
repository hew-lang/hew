//! Exec tests for the i32/i64 FFI width-reconciliation at the extern call
//! boundary.
//!
//! Several runtime C functions return (or accept) a narrower `i32` than the
//! Hew-facing stdlib catalog declares (`i64`). Codegen must declare the extern
//! at the runtime's true C-ABI width and reconcile against the Hew width at the
//! call site — sign-extending narrow results and truncating wide arguments.
//! Without this:
//!   * `string.from_char(code)` crashed every call (i64 argument handed to the
//!     `i32`-declared `hew_char_to_string`), and
//!   * `string.find` / `string.split` / `string.lines` silently corrupted data
//!     because the `-1` not-found sentinel from the `i32`-returning
//!     `hew_string_find` was read as `4294967295` (zero/garbage high bits).

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

fn hew_command(repo: &Path) -> Command {
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
        if lib.exists() {
            return;
        }
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
    });
}

fn run_hew_source(repo: &Path, stem: &str, source: &str) -> String {
    ensure_hew_runtime_lib(repo);
    let dir = std::env::temp_dir().join(format!("hew-ffi-width-{}-{stem}", std::process::id()));
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

/// `string.from_char` previously crashed at compile time: an `i64` argument was
/// handed to the `i32`-declared `hew_char_to_string`, so LLVM verification
/// rejected the module. The arg-narrowing reconciliation truncates the `i64`
/// down to the declared `i32`, so the call compiles and returns the right char.
#[test]
fn from_char_returns_char_without_crashing() {
    let repo = repo_root();
    let stdout = run_hew_source(
        &repo,
        "from_char",
        r#"
        import std::string;

        fn main() {
            let h = string.from_char(72);
            let i = string.from_char(105);
            print(f"{h}{i}");
        }
        "#,
    );

    assert_eq!(stdout, "Hi", "from_char produced wrong output: {stdout:?}");
}

/// `string.split` (module form) silently dropped/corrupted the LAST element
/// because its `rest.find(sep)` loop relied on the `-1` not-found sentinel,
/// which was read as `4294967295`. The result sign-extension restores `-1`, so
/// the final element survives. Indexed access avoids the unrelated
/// `Vec<String> for-in` NYI.
#[test]
fn split_preserves_last_element() {
    let repo = repo_root();
    let stdout = run_hew_source(
        &repo,
        "split_last",
        r#"
        import std::string;

        fn main() {
            let parts = string.split("a,b,c", ",");
            let n = parts.len();
            print(f"count={n};");
            for i in 0 .. n {
                print(f"[{parts.get(i)}]");
            }
        }
        "#,
    );

    assert_eq!(
        stdout, "count=3;[a][b][c]",
        "split corrupted the last element: {stdout:?}"
    );
}

/// `string.lines` previously emitted a phantom empty element and displaced the
/// final line for the same `-1`-sentinel reason. The middle element and the
/// last line must both be intact.
#[test]
fn lines_has_no_phantom_empty_and_keeps_last() {
    let repo = repo_root();
    let stdout = run_hew_source(
        &repo,
        "lines_no_phantom",
        r#"
        import std::string;

        fn main() {
            let ls = string.lines("l1\nl2\nl3");
            let n = ls.len();
            print(f"count={n};");
            for i in 0 .. n {
                print(f"[{ls.get(i)}]");
            }
        }
        "#,
    );

    assert_eq!(
        stdout, "count=3;[l1][l2][l3]",
        "lines produced phantom empties or lost the last line: {stdout:?}"
    );
}

/// Boundary case for the sentinel itself: a not-found `find` must round-trip the
/// `i32` `-1` as the signed `i64` `-1`, never the zero-extended `4294967295`.
/// `find` for a present needle stays a correct non-negative index.
#[test]
fn find_not_found_round_trips_negative_one() {
    let repo = repo_root();
    let stdout = run_hew_source(
        &repo,
        "find_sentinel",
        r#"
        import std::string;

        fn main() {
            let s = "hello";
            let found = s.find("ell");
            let missing = s.find("zzz");
            print(f"found={found};missing={missing};");
            if missing < 0 {
                print("guard_fires");
            } else {
                print("guard_broken");
            }
        }
        "#,
    );

    assert_eq!(
        stdout, "found=1;missing=-1;guard_fires",
        "find sentinel did not round-trip as signed -1: {stdout:?}"
    );
}

/// New UTF-8 codepoint helpers must also preserve the signed -1 sentinel across
/// the i32 C-ABI return → Hew-facing i64 boundary.
#[test]
fn unicode_codepoint_at_oob_round_trips_negative_one() {
    let repo = repo_root();
    let stdout = run_hew_source(
        &repo,
        "unicode_codepoint_oob",
        r#"
        import std::text::unicode;

        fn main() {
            let s = "café";
            let n = unicode.rune_count(s);
            let oob = unicode.codepoint_at(s, n);
            print(f"oob={oob};");
            if oob < 0 {
                print("guard_fires");
            } else {
                print("guard_broken");
            }
        }
        "#,
    );

    assert_eq!(
        stdout, "oob=-1;guard_fires",
        "unicode.codepoint_at OOB sentinel did not round-trip as signed -1: {stdout:?}",
    );
}
