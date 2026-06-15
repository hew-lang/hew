//! End-to-end regression test for `Vec<enum>` element reads via `v[i]` subscript.
//!
//! Root cause: `lower_vec_index` in `hew-mir/src/lower.rs` routed DIRECT
//! (non-indirect) enum element types to `hew_vec_get_ptr` (hard-coded 8-byte
//! pointer stride) via the `ResolvedTy::Named { .. }` catch-all. Direct enums
//! are stored inline at the tagged-union struct stride; the 8-byte stride
//! mis-strides the buffer and causes a runtime panic.
//!
//! Fix: direct enums (those present in `enum_layouts` with `is_indirect =
//! false`) now route to `hew_vec_get_layout` — the correct-stride layout-
//! descriptor getter — before the catch-all.  Indirect enums (heap pointers,
//! 8-byte stride) continue to fall through to `hew_vec_get_ptr`.

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

fn run_hew_source(repo: &Path, stem: &str, source: &str) -> String {
    ensure_hew_runtime_lib(repo);
    let dir =
        std::env::temp_dir().join(format!("hew-vec-enum-index-{}-{stem}", std::process::id()));
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

fn run_hew_fixture(repo: &Path, name: &str) -> String {
    ensure_hew_runtime_lib(repo);
    let fixture = repo
        .join("examples")
        .join("enums")
        .join(format!("{name}.hew"));
    let expected_path = fixture.with_extension("expected");
    let expected = std::fs::read_to_string(&expected_path)
        .unwrap_or_else(|e| panic!("read {}: {e}", expected_path.display()));

    let output = hew_command(repo)
        .arg("run")
        .arg(&fixture)
        .output()
        .expect("spawn hew run");
    assert!(
        output.status.success(),
        "hew run {} exited non-zero (status={:?}); stderr:\n{}",
        fixture.display(),
        output.status,
        String::from_utf8_lossy(&output.stderr)
    );
    let stdout = String::from_utf8(output.stdout).expect("stdout is utf-8");
    assert_eq!(stdout, expected, "{name} stdout mismatch");
    stdout
}

/// `v[i]` on a `Vec<Colour>` must return the correct variant for each element.
///
/// Before the fix, direct enums fell through to `hew_vec_get_ptr`, which uses
/// an 8-byte pointer stride.  Enums are stored inline at the tagged-union
/// struct stride; the 8-byte mis-stride causes a runtime abort.
#[test]
fn vec_direct_enum_subscript_returns_correct_variant() {
    let repo = repo_root();
    run_hew_fixture(&repo, "run_vec_enum_index");
}

/// `v[i]` on a `Vec<Colour>` reading unit, tuple, and struct variants all
/// produce correct payload values inline in the source.
///
/// This variant tests the same fix path with inline Hew source rather than a
/// fixture file, so the test is self-contained.
#[test]
fn vec_direct_enum_subscript_reads_unit_and_payload_variants() {
    let repo = repo_root();
    let stdout = run_hew_source(
        &repo,
        "vec_enum_unit_payload",
        r#"
enum Shape {
    Circle(i64);
    Square(i64, i64);
    Dot;
}

fn main() {
    let v: Vec<Shape> = Vec::new();
    v.push(Circle(7));
    v.push(Dot);
    v.push(Square(3, 4));

    let s0 = v[0];
    let s1 = v[1];
    let s2 = v[2];
    match s0 {
        Circle(r) => println(f"Circle({r})"),
        Square(w, h) => println(f"Square({w},{h})"),
        Dot => println("Dot"),
    }
    match s1 {
        Circle(r) => println(f"Circle({r})"),
        Square(w, h) => println(f"Square({w},{h})"),
        Dot => println("Dot"),
    }
    match s2 {
        Circle(r) => println(f"Circle({r})"),
        Square(w, h) => println(f"Square({w},{h})"),
        Dot => println("Dot"),
    }
}
        "#,
    );
    assert_eq!(stdout.trim(), "Circle(7)\nDot\nSquare(3,4)");
}
