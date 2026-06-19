//! End-to-end wasm32 generator execution and teardown proof.
//!
//! This is the regression for the former wasm generator fence: compile a source
//! `gen fn`, link the wasm object against the real wasm runtime archive, run it
//! under wasmtime, and assert both the yielded value and zero live coroutine
//! frame bytes after generator scope teardown.

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

fn which(tool: &str) -> Option<PathBuf> {
    let out = Command::new("which").arg(tool).output().ok()?;
    if !out.status.success() {
        return None;
    }
    let path = PathBuf::from(String::from_utf8_lossy(&out.stdout).trim());
    path.is_file().then_some(path)
}

fn wasmtime() -> Option<PathBuf> {
    which("wasmtime").or_else(|| {
        let home = std::env::var_os("HOME")?;
        let candidate = PathBuf::from(home)
            .join(".wasmtime")
            .join("bin")
            .join("wasmtime");
        candidate.is_file().then_some(candidate)
    })
}

fn ensure_wasm_runtime(repo: &Path) -> Option<PathBuf> {
    static BUILT: OnceLock<Option<PathBuf>> = OnceLock::new();
    BUILT
        .get_or_init(|| {
            let lib = target_dir(repo)
                .join("wasm32-wasip1")
                .join("debug")
                .join("libhew_runtime.a");
            let cargo = std::env::var_os("CARGO").unwrap_or_else(|| "cargo".into());
            let status = Command::new(cargo)
                .current_dir(repo)
                .args([
                    "build",
                    "--quiet",
                    "-p",
                    "hew-runtime",
                    "--target",
                    "wasm32-wasip1",
                    "--no-default-features",
                ])
                .status()
                .ok()?;
            (status.success() && lib.exists()).then_some(lib)
        })
        .clone()
}

fn wasm_self_contained_libc() -> Option<PathBuf> {
    let sysroot = Command::new("rustc")
        .args(["--print", "sysroot"])
        .output()
        .ok()
        .filter(|o| o.status.success())
        .map(|o| PathBuf::from(String::from_utf8_lossy(&o.stdout).trim()))?;
    let libc = sysroot.join("lib/rustlib/wasm32-wasip1/lib/self-contained/libc.a");
    libc.exists().then_some(libc)
}

fn hew_command(repo: &Path) -> Command {
    let bin = target_dir(repo).join("debug").join("hew");
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

#[test]
fn wasm_generator_teardown_runs_and_releases_frames() {
    let Some(wasm_ld) = which("wasm-ld") else {
        eprintln!("skip: wasm-ld not found");
        return;
    };
    let Some(wasmtime) = wasmtime() else {
        eprintln!("skip: wasmtime not found");
        return;
    };
    let repo = repo_root();
    let Some(runtime) = ensure_wasm_runtime(&repo) else {
        eprintln!("skip: wasm32-wasip1 runtime archive unavailable");
        return;
    };
    let Some(libc) = wasm_self_contained_libc() else {
        eprintln!("skip: rustup wasm32-wasip1 self-contained libc not installed");
        return;
    };

    let dir = tempfile::tempdir().expect("tempdir");
    let source_path = dir.path().join("gen_wasm_teardown.hew");
    std::fs::write(
        &source_path,
        r#"import std::observe;

gen fn rows() -> string {
    yield "row-" + "data";
    yield "done-" + "data";
}

fn main() {
    {
        let g = rows();
        match g.next() {
            Some(v) => println(v),
            None => println("none"),
        }
    }
    println(observe.read("coroutines.frame_bytes_live"));
}
"#,
    )
    .expect("write Hew source");

    let mut compile = hew_command(&repo);
    compile
        .arg("compile")
        .arg("--target")
        .arg("wasm32-unknown-unknown")
        .arg("--emit-dir")
        .arg(dir.path())
        .arg(&source_path);
    let output = hew_testutil::run_command_bounded(
        &mut compile,
        "hew compile wasm generator",
        Duration::from_secs(60),
    )
    .unwrap_or_else(|e| panic!("{e}"));
    assert!(
        output.status.success(),
        "hew compile failed (status={:?}); stderr:\n{}",
        output.status,
        String::from_utf8_lossy(&output.stderr)
    );

    let obj = dir.path().join("gen_wasm_teardown.wasm.o");
    let wasm = dir.path().join("gen_wasm_teardown.linked.wasm");
    let link = Command::new(&wasm_ld)
        .arg("--no-entry")
        .arg("--export=_start")
        .arg(&obj)
        .arg(&runtime)
        .arg(&libc)
        .arg("-o")
        .arg(&wasm)
        .output()
        .expect("wasm-ld link");
    assert!(
        link.status.success(),
        "wasm-ld failed:\n{}",
        String::from_utf8_lossy(&link.stderr)
    );

    let run = Command::new(&wasmtime)
        .arg("run")
        .arg(&wasm)
        .output()
        .expect("wasmtime run");
    assert!(
        run.status.success(),
        "wasmtime failed:\n{}",
        String::from_utf8_lossy(&run.stderr)
    );
    assert_eq!(
        String::from_utf8(run.stdout).expect("stdout utf8"),
        "row-data\n0\n"
    );
}
