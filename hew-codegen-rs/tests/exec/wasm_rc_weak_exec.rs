//! End-to-end wasm32 execution coverage for `Rc<T>` and `Weak<T>`.

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
    let output = Command::new("which").arg(tool).output().ok()?;
    if !output.status.success() {
        return None;
    }
    let path = PathBuf::from(String::from_utf8_lossy(&output.stdout).trim());
    path.is_file().then_some(path)
}

fn wasmtime() -> Option<PathBuf> {
    which("wasmtime").or_else(|| {
        let home = std::env::var_os("HOME")?;
        let candidate = PathBuf::from(home).join(".wasmtime/bin/wasmtime");
        candidate.is_file().then_some(candidate)
    })
}

fn ensure_wasm_runtime(repo: &Path) -> Option<PathBuf> {
    static BUILT: OnceLock<Option<PathBuf>> = OnceLock::new();
    BUILT
        .get_or_init(|| {
            let lib = target_dir(repo).join("wasm32-wasip1/debug/libhew_runtime.a");
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
    let output = Command::new("rustc")
        .args(["--print", "sysroot"])
        .output()
        .ok()?;
    if !output.status.success() {
        return None;
    }
    let sysroot = PathBuf::from(String::from_utf8_lossy(&output.stdout).trim());
    let libc = sysroot.join("lib/rustlib/wasm32-wasip1/lib/self-contained/libc.a");
    libc.exists().then_some(libc)
}

#[test]
fn wasm_rc_weak_set_upgrade_and_counts_are_exact() {
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
    let source = dir.path().join("rc_weak_wasm.hew");
    std::fs::write(
        &source,
        r#"
fn expired() -> Weak<i64> {
    let rc = Rc::new(1);
    rc.downgrade()
}

fn main() {
    let rc = Rc::new(7);
    print(rc.strong_count());
    print(rc.weak_count());
    let alias = rc.clone();
    print(rc.strong_count());
    let weak = rc.downgrade();
    let _weak_alias = weak.clone();
    print(rc.weak_count());
    match weak.upgrade() {
        Some(upgraded) => {
            print(upgraded.get());
            print(upgraded.strong_count());
        },
        None => print(99),
    }
    print(rc.strong_count());
    rc.set(9);
    print(alias.get());
    let dead = expired();
    print(match dead.upgrade() {
        Some(_) => 9,
        None => 0,
    });
}
"#,
    )
    .expect("write Hew source");

    let hew = target_dir(&repo).join("debug/hew");
    let hew = if hew.exists() {
        hew
    } else {
        hew_testutil::ensure_hew_bin_built().expect("build hew binary")
    };
    let output = hew_testutil::run_command_bounded(
        Command::new(hew)
            .arg("compile")
            .arg("--target")
            .arg("wasm32-unknown-unknown")
            .arg("--emit-dir")
            .arg(dir.path())
            .arg(&source),
        "hew compile wasm Rc/Weak lifecycle",
        Duration::from_secs(60),
    )
    .unwrap_or_else(|error| panic!("{error}"));
    assert!(
        output.status.success(),
        "hew compile failed:\n{}",
        String::from_utf8_lossy(&output.stderr)
    );

    let object = dir.path().join("rc_weak_wasm.wasm.o");
    let wasm = dir.path().join("rc_weak_wasm.linked.wasm");
    let link = Command::new(wasm_ld)
        .arg("--no-entry")
        .arg("--export=_start")
        .arg(object)
        .arg(runtime)
        .arg(libc)
        .arg("-o")
        .arg(&wasm)
        .output()
        .expect("wasm-ld link");
    assert!(
        link.status.success(),
        "wasm-ld failed:\n{}",
        String::from_utf8_lossy(&link.stderr)
    );

    let run = Command::new(wasmtime)
        .arg("run")
        .arg(wasm)
        .output()
        .expect("wasmtime run");
    assert!(
        run.status.success(),
        "wasmtime failed:\n{}",
        String::from_utf8_lossy(&run.stderr)
    );
    assert_eq!(String::from_utf8_lossy(&run.stdout), "102273290");
}
