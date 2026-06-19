//! E5c integration tests: WASM parity classification for native-only substrates.
//!
//! Behaviours under test:
//!
//! 1. Non-duplex programs (`01-arith.hew`) still emit a `.wasm` artefact when
//!    WASM is requested with `--target wasm32-unknown-unknown`. The duplex
//!    detection gate must not fire for programs that don't use the duplex
//!    substrate.
//!
//! 2. A bare compile of a non-duplex program skips WASM emission and produces
//!    only the native binary.
//!
//! 3. `CodegenError::WasmUnsupportedSubstrate` diagnostics keep the category
//!    selected by the codegen display path: lambda-actor surfaces `lambda_actor`
//!    + `#1451`; duplex surfaces `duplex` + `#1451`.
//!      (Generators are now fully supported on wasm32 — see `wasm_generator_exec.rs`.)
//!
//! 4. WASM-TODO(#1451): `hew_duplex_*` symbols are excluded from wasm32
//!    builds via `hew-runtime/src/duplex.rs:54`. The codegen layer returns
//!    `CodegenError::WasmUnsupportedSubstrate` before invoking `wasm-ld`,
//!    so the CLI surfaces a structured diagnostic pointing to the WASM target
//!    rather than a raw linker error.
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

    let fixture = repo_root().join("tests/vertical-slice/accept/01-arith.hew");
    assert!(fixture.exists(), "fixture not found: {}", fixture.display());

    let emit_dir = tempfile::Builder::new()
        .prefix("compile-wasm-non-duplex-")
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
        "hew compile (non-duplex, with WASM) failed:\n{}",
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
// Bare non-duplex compile: no wasm line, native binary present
// ---------------------------------------------------------------------------

/// A bare compile of a non-duplex program skips WASM emission; the native
/// binary is still produced.
#[test]
fn bare_compile_skips_wasm_for_non_duplex() {
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
        "hew compile (native default, non-duplex) failed:\n{}",
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

#[test]
fn wasm_unsupported_substrate_diagnostics_preserve_symbol_category() {
    require_codegen();

    // `gen_block_outside_receive.hew` was previously asserted to fail on wasm32
    // (the generator fence, WASM-TODO(#1758)). Generators are now supported on
    // wasm32 via the in-module `hew_gen_coro_destroy` override emitted by
    // `emit_wasm_coro_runtime_overrides`; that fixture compiles and produces a
    // `.wasm` artefact on this target.  The generator-parity classification path
    // in `CodegenError::WasmUnsupportedSubstrate` is no longer reachable from a
    // `gen {}` expression, so the representative below is lambda-actor.

    assert_wasm_unsupported_category(
        "tests/vertical-slice/accept/lambda_method_send.hew",
        // Spawn-side wiring (Terminator::MakeLambdaActor → hew_lambda_actor_new)
        // surfaces the lambda-actor substrate symbol BEFORE the Duplex send symbol
        // because the spawn fail-closes first. Both `hew_lambda_actor_*` and the
        // underlying `hew_duplex_*` symbols are native-only on wasm32; the test's
        // intent is to assert the diagnostic preserves the symbol category in the
        // first surface, and "lambda_actor" satisfies that for this fixture.
        &["lambda_actor", "#1451"],
        &[],
    );
}

fn assert_wasm_unsupported_category(fixture: &str, expected: &[&str], forbidden: &[&str]) {
    let fixture = repo_root().join(fixture);
    assert!(fixture.exists(), "fixture not found: {}", fixture.display());

    let emit_dir = tempfile::Builder::new()
        .prefix("compile-wasm-unsupported-category-")
        .tempdir()
        .expect("create temp dir");

    let fixture_str = fixture.to_str().expect("fixture path is valid UTF-8");
    let emit_dir_str = emit_dir.path().to_str().expect("emit dir is valid UTF-8");

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
        !output.status.success(),
        "expected non-zero exit for wasm-unsupported fixture `{}`:\n{}",
        fixture.display(),
        describe_output(&output)
    );

    let stderr = String::from_utf8_lossy(&output.stderr);
    for needle in expected {
        assert!(
            stderr.contains(needle),
            "expected diagnostic for `{}` to contain `{needle}`; got:\n{stderr}",
            fixture.display()
        );
    }
    let stderr_lower = stderr.to_lowercase();
    for needle in forbidden {
        assert!(
            !stderr_lower.contains(&needle.to_lowercase()),
            "expected diagnostic for `{}` not to contain `{needle}`; got:\n{stderr}",
            fixture.display()
        );
    }
}

// ---------------------------------------------------------------------------
// WASM-TODO(#1451): duplex program blocked at codegen, not at wasm-ld
// ---------------------------------------------------------------------------

/// WASM-TODO(#1451): `hew_duplex_*` symbols are excluded from the wasm32 build
/// (`hew-runtime/src/duplex.rs:54`, `#![cfg(not(target_arch = "wasm32"))]`).
/// `emit_module` returns `CodegenError::WasmUnsupportedSubstrate` before
/// invoking `wasm-ld`, and the CLI surfaces a structured diagnostic with a
/// `--target wasm32-unknown-unknown` hint instead of a raw linker failure.
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
    let fixture = repo_root().join("tests/vertical-slice/accept/duplex_pair_send_skeleton.hew");

    let emit_dir = tempfile::Builder::new()
        .prefix("compile-duplex-wasm-")
        .tempdir()
        .expect("create temp dir");

    let fixture_str = fixture.to_str().expect("fixture path is valid UTF-8");
    let emit_dir_str = emit_dir.path().to_str().expect("emit dir is valid UTF-8");

    // With the WASM target: expect structured diagnostic referencing #1451.
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
        .expect("invoke hew compile (duplex, WASM target)");

    assert!(
        !output.status.success(),
        "expected non-zero exit for duplex program with WASM target"
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("duplex") && stderr.contains("--target wasm32-unknown-unknown"),
        "expected diagnostic mentioning duplex substrate and WASM target hint; got:\n{stderr}"
    );

    // Bare native compile: native binary produced, exits 0.
    let output_native = Command::new(hew_binary())
        .args(["compile", "--emit-dir", emit_dir_str, fixture_str])
        .current_dir(repo_root())
        .output()
        .expect("invoke hew compile (duplex, native default)");

    assert!(
        output_native.status.success(),
        "hew compile native default (duplex) must succeed; got:\n{}",
        describe_output(&output_native)
    );
    let stdout_native = String::from_utf8_lossy(&output_native.stdout);
    assert!(
        stdout_native.lines().any(|l| l.starts_with("native:")),
        "expected `native:` line with native default; got:\n{stdout_native}"
    );
}
