//! WASM emission and native-vs-WASI parity for the shapes this file pins.
//!
//! Behaviours under test:
//!
//! 1. A plain arithmetic program still emits a `.wasm` artefact when WASM is
//!    requested with `--target wasm32-unknown-unknown`.
//!
//! 2. A bare compile of that same program skips WASM emission and produces
//!    only the native binary.
//!
//! 3. A lambda actor runs identically on both targets: `actor(M) -> R` lowers
//!    through the same `_native` family a declared actor does, so there is no
//!    lambda-actor substrate category to classify.
mod support;

use std::process::Command;

use support::{describe_output, hew_binary, repo_root, require_codegen};

// ---------------------------------------------------------------------------
// Plain WASM smoke: existing fixture must still emit wasm
// ---------------------------------------------------------------------------

/// A plain arithmetic program must still emit a `.wasm` artefact when WASM is
/// requested.
///
/// Uses the existing `01-arith.hew` fixture (`return 3 + 4`).
#[test]
fn arith_program_emits_wasm() {
    require_codegen();

    // wasm-ld is needed to link the .wasm; skip the test if it's not available.
    let wasm_ld_available = Command::new("wasm-ld")
        .arg("--version")
        .output()
        .is_ok_and(|o| o.status.success())
        || Command::new("rust-lld")
            .args(["--version"])
            .output()
            .is_ok_and(|o| o.status.success());
    if !wasm_ld_available {
        eprintln!("skip: wasm-ld / rust-lld not available");
        return;
    }

    let fixture = repo_root().join("tests/vertical-slice/accept/01-arith.hew");
    assert!(fixture.exists(), "fixture not found: {}", fixture.display());

    let emit_dir = tempfile::Builder::new()
        .prefix("compile-wasm-arith-")
        .tempdir()
        .expect("create temp dir");

    let fixture_str = fixture.to_str().expect("fixture path is valid UTF-8");
    let emit_dir_str = emit_dir.path().to_str().expect("emit dir is valid UTF-8");

    // WASM emission is requested explicitly through --target.
    let output = Command::new(hew_binary())
        .args([
            "compile",
            "--target",
            "wasm32-unknown-unknown",
            "--emit-dir",
            emit_dir_str,
            fixture_str,
        ])
        .current_dir(repo_root())
        .output()
        .expect("invoke hew compile");

    assert!(
        output.status.success(),
        "hew compile (arith, with WASM) failed:\n{}",
        describe_output(&output),
    );

    let stdout = String::from_utf8_lossy(&output.stdout);
    let wasm_line = stdout.lines().find(|l| l.starts_with("wasm:"));
    assert!(
        wasm_line.is_some(),
        "expected a `wasm:` line in stdout for arith WASM emission; got:\n{stdout}"
    );

    // The .wasm file must actually exist.
    let wasm_path = wasm_line
        .unwrap()
        .split_once(':')
        .map_or("", |x| x.1)
        .trim();
    assert!(
        !wasm_path.is_empty() && std::path::Path::new(wasm_path).exists(),
        "wasm artefact not found at `{wasm_path}`"
    );
}

// ---------------------------------------------------------------------------
// Bare compile: no wasm line, native binary present
// ---------------------------------------------------------------------------

/// A bare compile of the arithmetic program skips WASM emission; the native
/// binary is still produced.
#[test]
fn bare_compile_skips_wasm() {
    require_codegen();

    let fixture = repo_root().join("tests/vertical-slice/accept/01-arith.hew");
    assert!(fixture.exists(), "fixture not found: {}", fixture.display());

    let emit_dir = tempfile::Builder::new()
        .prefix("compile-native-default-")
        .tempdir()
        .expect("create temp dir");

    let fixture_str = fixture.to_str().expect("fixture path is valid UTF-8");
    let emit_dir_str = emit_dir.path().to_str().expect("emit dir is valid UTF-8");

    let output = Command::new(hew_binary())
        .args(["compile", "--emit-dir", emit_dir_str, fixture_str])
        .current_dir(repo_root())
        .output()
        .expect("invoke hew compile");

    assert!(
        output.status.success(),
        "hew compile (native default, arith) failed:\n{}",
        describe_output(&output),
    );

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.lines().any(|l| l.starts_with("native:")),
        "expected a `native:` line in stdout; got:\n{stdout}"
    );
    assert!(
        !stdout.lines().any(|l| l.starts_with("wasm:")),
        "expected no `wasm:` line for bare native compile; got:\n{stdout}"
    );
}

/// A lambda actor runs identically on both targets.
///
/// The oracle this replaces asserted a `CodegenError::WasmUnsupportedSubstrate`
/// diagnostic with the `lambda_actor` category. The retired backend produced
/// that from a MIR symbol scan; the physical backend has no such scan and no
/// lambda-actor symbol - `actor(M) -> R` lowers through the same `_native`
/// family a declared actor does - so the category has no producer and the
/// fixture is a parity case instead.
#[test]
fn lambda_actor_runs_identically_on_native_and_wasi() {
    require_codegen();
    if !wasi_runner_available() {
        eprintln!("skip: wasmtime not available");
        return;
    }

    let fixture = repo_root().join("tests/vertical-slice/accept/lambda_method_send.hew");
    assert!(fixture.exists(), "fixture not found: {}", fixture.display());

    let run = |target: Option<&str>| {
        let mut args = vec!["run"];
        if let Some(target) = target {
            args.extend_from_slice(&["--target", target]);
        }
        let fixture = fixture.to_str().expect("fixture path is valid UTF-8");
        args.push(fixture);
        Command::new(hew_binary())
            .args(&args)
            .current_dir(repo_root())
            .output()
            .expect("invoke hew run")
    };

    let native = run(None);
    let wasi = run(Some("wasm32-wasi"));

    assert!(
        native.status.success(),
        "native lambda-actor run failed:\n{}",
        describe_output(&native)
    );
    assert_eq!(
        String::from_utf8_lossy(&wasi.stdout),
        String::from_utf8_lossy(&native.stdout),
        "WASI stdout must match native:\n{}",
        describe_output(&wasi)
    );
    assert_eq!(
        wasi.status.code(),
        native.status.code(),
        "WASI exit status must match native:\n{}",
        describe_output(&wasi)
    );
}

/// Whether a WASI runner is on this host, using the same lookup `hew run` does.
fn wasi_runner_available() -> bool {
    Command::new("wasmtime")
        .arg("--version")
        .output()
        .is_ok_and(|output| output.status.success())
        || std::env::var_os("HOME").is_some_and(|home| {
            std::path::Path::new(&home)
                .join(".wasmtime/bin/wasmtime")
                .exists()
        })
}
