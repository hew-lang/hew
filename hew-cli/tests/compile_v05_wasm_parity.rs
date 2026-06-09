//! E5c integration tests: WASM parity classification for the duplex substrate.
//!
//! Three behaviours under test:
//!
//! 1. Non-duplex programs (`01-arith.hew`) still emit a `.wasm` artefact when
//!    WASM is requested (no `--no-wasm`). The duplex detection gate must not
//!    fire for programs that don't use the duplex substrate.
//!
//! 2. The `--no-wasm` flag on a non-duplex program silently skips WASM
//!    emission and produces only the native binary.  This verifies the
//!    existing flag continues to work as documented.
//!
//! 3. WASM-TODO(#1451): `hew_duplex_*` symbols are excluded from wasm32
//!    builds via `hew-runtime/src/duplex.rs:54`. The codegen layer returns
//!    `CodegenError::WasmUnsupportedSubstrate` before invoking `wasm-ld`,
//!    so the CLI surfaces a structured diagnostic pointing to `--no-wasm`
//!    rather than a raw linker error. This sub-test is `#[ignore]` because
//!    reaching it end-to-end requires the HIR→MIR duplex surface to be wired
//!    through the checker (E3), which has not yet landed on `v05-integration`.
//!    The codegen-layer gate is tested in
//!    `hew-codegen-rs/tests/wasm_duplex_classification.rs`.
mod support;

use std::process::Command;

use support::{describe_output, hew_binary, repo_root, require_codegen};

// ---------------------------------------------------------------------------
// Non-duplex WASM smoke: existing fixture must still emit wasm
// ---------------------------------------------------------------------------

/// Non-duplex programs must still emit a `.wasm` artefact when WASM is
/// requested.  This verifies that the duplex detection gate introduced in E5c
/// does not inadvertently block programs that have no duplex symbols.
///
/// Uses the existing `01-arith.hew` fixture (`return 3 + 4`).
#[test]
fn non_duplex_program_emits_wasm() {
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

    let fixture = repo_root().join("tests/v05-vertical-slice/accept/01-arith.hew");
    assert!(fixture.exists(), "fixture not found: {}", fixture.display());

    let emit_dir = tempfile::Builder::new()
        .prefix("compile-v05-wasm-non-duplex-")
        .tempdir()
        .expect("create temp dir");

    let fixture_str = fixture.to_str().expect("fixture path is valid UTF-8");
    let emit_dir_str = emit_dir.path().to_str().expect("emit dir is valid UTF-8");

    // No --no-wasm: WASM emission is requested.
    let output = Command::new(hew_binary())
        .args(["compile-v05", "--emit-dir", emit_dir_str, fixture_str])
        .current_dir(repo_root())
        .output()
        .expect("invoke hew compile-v05");

    assert!(
        output.status.success(),
        "hew compile-v05 (non-duplex, with WASM) failed:\n{}",
        describe_output(&output),
    );

    let stdout = String::from_utf8_lossy(&output.stdout);
    let wasm_line = stdout.lines().find(|l| l.starts_with("wasm:"));
    assert!(
        wasm_line.is_some(),
        "expected a `wasm:` line in stdout for non-duplex WASM emission; got:\n{stdout}"
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
// --no-wasm flag on non-duplex: no wasm line, native binary present
// ---------------------------------------------------------------------------

/// `--no-wasm` on a non-duplex program silently skips WASM emission; the
/// native binary is still produced.  Verifies the flag works as documented
/// and as an escape hatch.
#[test]
fn no_wasm_flag_skips_wasm_for_non_duplex() {
    require_codegen();

    let fixture = repo_root().join("tests/v05-vertical-slice/accept/01-arith.hew");
    assert!(fixture.exists(), "fixture not found: {}", fixture.display());

    let emit_dir = tempfile::Builder::new()
        .prefix("compile-v05-no-wasm-flag-")
        .tempdir()
        .expect("create temp dir");

    let fixture_str = fixture.to_str().expect("fixture path is valid UTF-8");
    let emit_dir_str = emit_dir.path().to_str().expect("emit dir is valid UTF-8");

    let output = Command::new(hew_binary())
        .args([
            "compile-v05",
            "--no-wasm",
            "--emit-dir",
            emit_dir_str,
            fixture_str,
        ])
        .current_dir(repo_root())
        .output()
        .expect("invoke hew compile-v05 --no-wasm");

    assert!(
        output.status.success(),
        "hew compile-v05 --no-wasm (non-duplex) failed:\n{}",
        describe_output(&output),
    );

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.lines().any(|l| l.starts_with("native:")),
        "expected a `native:` line in stdout; got:\n{stdout}"
    );
    assert!(
        !stdout.lines().any(|l| l.starts_with("wasm:")),
        "expected no `wasm:` line when --no-wasm is passed; got:\n{stdout}"
    );
}

// ---------------------------------------------------------------------------
// WASM-TODO(#1451): duplex program blocked at codegen, not at wasm-ld
// ---------------------------------------------------------------------------

/// WASM-TODO(#1451): `hew_duplex_*` symbols are excluded from the wasm32 build
/// (`hew-runtime/src/duplex.rs:54`, `#![cfg(not(target_arch = "wasm32"))]`).
/// `emit_module` returns `CodegenError::WasmUnsupportedSubstrate` before
/// invoking `wasm-ld`, and the CLI surfaces a structured diagnostic with a
/// `--no-wasm` hint instead of a raw linker failure.
///
/// This test is ignored because the HIR→MIR duplex surface requires E3
/// (checker-side `duplex_pair` resolution in the HIR bridge) which has not yet
/// landed on `v05-integration`. The codegen-layer classification is exercised
/// directly by `hew-codegen-rs/tests/wasm_duplex_classification.rs`.
#[test]
#[ignore = "WASM-TODO(#1451): hew_duplex_* excluded from wasm32 build; see \
            hew-runtime/src/duplex.rs:54. E3 (HIR duplex bridge) is required \
            to compile a duplex source fixture end-to-end."]
fn duplex_program_wasm_surfaces_structured_diagnostic() {
    require_codegen();

    // This fixture requires E3 to compile; it will fail at HIR with
    // `UnresolvedSymbol: duplex_pair` until E3 lands.
    let fixture = repo_root().join("tests/v05-vertical-slice/accept/duplex_pair_send_skeleton.hew");

    let emit_dir = tempfile::Builder::new()
        .prefix("compile-v05-duplex-wasm-")
        .tempdir()
        .expect("create temp dir");

    let fixture_str = fixture.to_str().expect("fixture path is valid UTF-8");
    let emit_dir_str = emit_dir.path().to_str().expect("emit dir is valid UTF-8");

    // Without --no-wasm: expect structured diagnostic referencing #1451.
    let output = Command::new(hew_binary())
        .args(["compile-v05", "--emit-dir", emit_dir_str, fixture_str])
        .current_dir(repo_root())
        .output()
        .expect("invoke hew compile-v05 (duplex, without --no-wasm)");

    assert!(
        !output.status.success(),
        "expected non-zero exit for duplex program without --no-wasm"
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("duplex") && stderr.contains("--no-wasm"),
        "expected diagnostic mentioning duplex substrate and --no-wasm hint; got:\n{stderr}"
    );

    // With --no-wasm: native binary produced, exits 0.
    let output_no_wasm = Command::new(hew_binary())
        .args([
            "compile-v05",
            "--no-wasm",
            "--emit-dir",
            emit_dir_str,
            fixture_str,
        ])
        .current_dir(repo_root())
        .output()
        .expect("invoke hew compile-v05 --no-wasm (duplex)");

    assert!(
        output_no_wasm.status.success(),
        "hew compile-v05 --no-wasm (duplex) must succeed; got:\n{}",
        describe_output(&output_no_wasm)
    );
    let stdout_no_wasm = String::from_utf8_lossy(&output_no_wasm.stdout);
    assert!(
        stdout_no_wasm.lines().any(|l| l.starts_with("native:")),
        "expected `native:` line with --no-wasm; got:\n{stdout_no_wasm}"
    );
}
