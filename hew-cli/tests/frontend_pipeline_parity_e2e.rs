//! Integration tests: `hew check` and `hew compile` share the frontend pipeline.
//!
//! These tests assert that both commands run the same frontend (parse →
//! import-graph → typecheck) and therefore produce identical frontend
//! diagnostics for every scenario below.  They are the durable invariant that
//! trips when someone reintroduces a bypass (e.g. a compile shortcut that
//! skips `run_file_frontend_to_typecheck`).
//!
//! ## Equality predicate
//!
//! `hew check` always appends `failure.message` (e.g. "type errors found",
//! "undeclared dependencies") to stderr after rendering span-attributed
//! diagnostics.  `hew compile` only appends the message when no span
//! diagnostics were emitted.  The load-bearing invariant is the *diagnostic
//! body* (the span-attributed lines), not the trailing summary.
//!
//! `diagnostic_body_from_check_stderr` strips the known trailing summary
//! lines so that both outputs can be compared byte-for-byte.
//!
//! ## Scenario map
//!
//! | # | Scenario                       | Expected common outcome  |
//! |---|--------------------------------|--------------------------|
//! | 1 | Manifest-undeclared import     | Both reject              |
//! | 2 | `re"..."` implicit-import      | Frontend accepts both    |
//! | 3 | Type error in imported module  | Both reject              |
//! | 4 | WASM target (frontend layer)   | Frontend accepts both    |

mod support;

use std::fs;
use std::process::Command;

use support::{describe_output, hew_binary, repo_root, strip_ansi};

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Run `hew check <path>` from the repo root and return the raw output.
fn run_check(path: &str) -> std::process::Output {
    Command::new(hew_binary())
        .args(["check", path])
        .current_dir(repo_root())
        .output()
        .expect("invoke hew check")
}

/// Run `hew compile <path>` with a temp emit-dir from the repo root.
fn run_compile(path: &str) -> (std::process::Output, tempfile::TempDir) {
    let emit_dir = tempfile::Builder::new()
        .prefix("frontend-parity-emit-")
        .tempdir()
        .expect("create temp emit dir");
    let output = Command::new(hew_binary())
        .args([
            "compile",
            "--emit-dir",
            emit_dir.path().to_str().expect("valid UTF-8 emit dir"),
            path,
        ])
        .current_dir(repo_root())
        .output()
        .expect("invoke hew compile");
    (output, emit_dir)
}

/// Run `hew compile <path> --target <triple>` with a temp emit-dir.
fn run_compile_with_target(path: &str, target: &str) -> (std::process::Output, tempfile::TempDir) {
    let emit_dir = tempfile::Builder::new()
        .prefix("frontend-parity-wasm-emit-")
        .tempdir()
        .expect("create temp emit dir");
    let output = Command::new(hew_binary())
        .args([
            "compile",
            "--target",
            target,
            "--emit-dir",
            emit_dir.path().to_str().expect("valid UTF-8 emit dir"),
            path,
        ])
        .current_dir(repo_root())
        .output()
        .expect("invoke hew compile --target");
    (output, emit_dir)
}

/// Extract the diagnostic body from `hew check` stderr.
///
/// `hew check` always appends the [`FrontendFailure::message`] (e.g.
/// "type errors found", "undeclared dependencies", "parsing failed") as a bare
/// line after the span-attributed diagnostics.  `hew compile` only appends
/// that line when no span diagnostics were emitted
/// (`lower_file_to_mir:main.rs` — the `if failure.diagnostics.is_empty()`
/// guard).  Strip the trailing summary so both outputs share the same body.
///
/// Specifically: remove the last non-empty trailing line when it contains no
/// `:` (span lines all contain `filename:line:col:`), after ANSI stripping.
fn diagnostic_body_from_check_stderr(stderr: &str) -> String {
    let stripped = strip_ansi(stderr);
    let lines: Vec<&str> = stripped.lines().collect();
    // The summary line is the final non-empty line that contains no `:`.
    // Span lines always contain at least one `:` (e.g. "file.hew:2:10: error:").
    // Source-context lines (` 2 | ...`) and hint lines (`  hint:`) do too.
    // Summary messages like "type errors found" or "undeclared dependencies" do not.
    let trimmed = match lines.iter().rposition(|l| !l.trim().is_empty()) {
        Some(last_idx) if !lines[last_idx].contains(':') => &lines[..last_idx],
        _ => &lines,
    };
    trimmed.join("\n")
}

// ---------------------------------------------------------------------------
// Scenario 1: manifest-undeclared import
// ---------------------------------------------------------------------------

/// A manifest-aware project (`hew.toml`) that imports a module not listed in
/// `[dependencies]` must be rejected by **both** `hew check` and `hew compile`
/// with identical diagnostic output.
///
/// This catches a bypass that skips `validate_imports_against_manifest` on the
/// compile path.
#[test]
fn manifest_undeclared_import_rejected_identically() {
    // Build an on-disk project: hew.toml with no deps, main.hew that imports
    // a non-std external module.
    let workspace = tempfile::Builder::new()
        .prefix("fp-parity-manifest-")
        .tempdir()
        .expect("create temp workspace");

    fs::write(
        workspace.path().join("hew.toml"),
        "[package]\nname = \"testpkg\"\n",
    )
    .expect("write hew.toml");

    let main_hew = workspace.path().join("main.hew");
    fs::write(
        &main_hew,
        "import some::external::lib;\n\nfn main() -> i64 { return 0; }\n",
    )
    .expect("write main.hew");

    let path = main_hew.to_str().expect("valid UTF-8 path");

    let check_out = run_check(path);
    let (compile_out, _emit_dir) = run_compile(path);

    // Both must fail.
    assert!(
        !check_out.status.success(),
        "hew check should reject undeclared import:\n{}",
        describe_output(&check_out),
    );
    assert!(
        !compile_out.status.success(),
        "hew compile should reject undeclared import:\n{}",
        describe_output(&compile_out),
    );

    let check_body = diagnostic_body_from_check_stderr(&String::from_utf8_lossy(&check_out.stderr));
    let compile_diag = strip_ansi(&String::from_utf8_lossy(&compile_out.stderr));

    // The diagnostic bodies must be identical — both pipelines go through
    // the same `validate_imports_against_manifest` call.
    assert_eq!(
        check_body.trim(),
        compile_diag.trim(),
        "hew check and hew compile produced different frontend diagnostics \
         for a manifest-undeclared import.\n\
         check body:\n{check_body}\n\
         compile stderr:\n{compile_diag}",
    );

    // Belt-and-suspenders: both mention the undeclared module.
    assert!(
        check_body.contains("some::external::lib"),
        "diagnostic should name the offending module: {check_body}",
    );
}

// ---------------------------------------------------------------------------
// Scenario 2: re"..." literal — implicit std::text::regex injection
// ---------------------------------------------------------------------------

/// Source using a `re"..."` regex literal triggers implicit injection of
/// `import std::text::regex;` by `inject_implicit_imports`.  The frontend
/// should accept the source on both paths (the implicit import is resolved
/// via `std::`, which is always builtin).
///
/// This test verifies that both `hew check` and `hew compile` share the same
/// frontend verdict (success) and produce the same (empty) frontend diagnostic
/// body.  `hew compile` may then fail at HIR/MIR for other reasons during the
/// v0.5 cutover; that is a post-frontend concern and is not the invariant
/// tested here.
#[test]
fn regex_literal_implicit_import_same_frontend_verdict() {
    let tmp = tempfile::Builder::new()
        .prefix("fp-parity-regex-")
        .suffix(".hew")
        .tempfile_in(repo_root())
        .expect("create temp .hew file");

    fs::write(
        tmp.path(),
        "fn main() -> i64 {\n    let _r = re\"hello\";\n    return 0;\n}\n",
    )
    .expect("write regex fixture");

    let path = tmp.path().to_str().expect("valid UTF-8 path");

    let check_out = run_check(path);

    // `hew check` must accept: the frontend resolves `re"..."` via the
    // implicit `std::text::regex` injection and the type-checker is happy.
    assert!(
        check_out.status.success(),
        "hew check should accept a re\"...\" literal (implicit std::text::regex import):\n{}",
        describe_output(&check_out),
    );

    // The frontend diagnostics emitted by `hew check` must be empty — no
    // span-attributed lines at all.  The `hew check` stdout/stderr must only
    // contain the `<file>: OK` status line.
    let check_stderr = strip_ansi(&String::from_utf8_lossy(&check_out.stderr));
    let check_diag_lines: Vec<&str> = check_stderr
        .lines()
        .filter(|l| l.contains(": error:") || l.contains(": warning:"))
        .collect();
    assert!(
        check_diag_lines.is_empty(),
        "hew check should emit no error/warning diagnostics for a re\"...\" literal; \
         got:\n{check_stderr}",
    );

    // `hew compile` runs the same frontend and must also not emit any
    // frontend-layer diagnostics (span lines that reference the source file)
    // before handing off to HIR.
    let (compile_out, _emit_dir) = run_compile(path);

    let compile_stderr = strip_ansi(&String::from_utf8_lossy(&compile_out.stderr));
    let compile_frontend_errors: Vec<&str> = compile_stderr
        .lines()
        .filter(|l| {
            let l = l.trim();
            (l.contains(": error:") || l.contains(": warning:"))
                // A genuine frontend diagnostic always references the source file
                && compile_stderr.contains(path)
        })
        .collect();
    assert!(
        compile_frontend_errors.is_empty(),
        "hew compile should emit no frontend diagnostics for a re\"...\" literal \
         (implicit std::text::regex injection must resolve on both paths);\n\
         frontend-matching lines in compile stderr:\n{}\n\
         full compile stderr:\n{compile_stderr}",
        compile_frontend_errors.join("\n"),
    );
}

// ---------------------------------------------------------------------------
// Scenario 3: type error in an imported module (multi-file project)
// ---------------------------------------------------------------------------

/// A type error in a module imported by the root file must be reported by
/// **both** `hew check` and `hew compile` with identical span-attributed
/// diagnostic output.
///
/// This catches a bypass that skips multi-file import resolution on the
/// compile path (e.g. a shortcut that only typechecks the root file in
/// isolation).
#[test]
fn type_error_in_imported_module_reported_identically() {
    let workspace = tempfile::Builder::new()
        .prefix("fp-parity-sibling-")
        .tempdir()
        .expect("create temp workspace");

    fs::write(
        workspace.path().join("hew.toml"),
        "[package]\nname = \"siblingpkg\"\n",
    )
    .expect("write hew.toml");

    fs::create_dir(workspace.path().join("src")).expect("create src directory");

    let main_hew = workspace.path().join("main.hew");
    fs::write(
        &main_hew,
        "import siblingpkg::helper;\n\nfn main() -> i64 {\n    return helper.value();\n}\n",
    )
    .expect("write main.hew");

    fs::write(
        workspace.path().join("src").join("helper.hew"),
        "pub fn value() -> i64 {\n    let x: i64 = \"not an integer\";\n    return x;\n}\n",
    )
    .expect("write src/helper.hew");

    let path = main_hew.to_str().expect("valid UTF-8 path");

    let check_out = run_check(path);
    let (compile_out, _emit_dir) = run_compile(path);

    // Both must fail.
    assert!(
        !check_out.status.success(),
        "hew check should reject a type error in a sibling module:\n{}",
        describe_output(&check_out),
    );
    assert!(
        !compile_out.status.success(),
        "hew compile should reject a type error in a sibling module:\n{}",
        describe_output(&compile_out),
    );

    let check_body = diagnostic_body_from_check_stderr(&String::from_utf8_lossy(&check_out.stderr));
    let compile_diag = strip_ansi(&String::from_utf8_lossy(&compile_out.stderr));

    // The span-attributed diagnostic lines must be identical.
    assert_eq!(
        check_body.trim(),
        compile_diag.trim(),
        "hew check and hew compile produced different frontend diagnostics \
         for a type error in an imported module.\n\
         check body:\n{check_body}\n\
         compile stderr:\n{compile_diag}",
    );

    // Belt-and-suspenders: the type mismatch must reference the sibling file.
    assert!(
        check_body.contains("helper.hew"),
        "diagnostic should point to helper.hew (not main.hew):\n{check_body}",
    );
    assert!(
        check_body.contains("type mismatch"),
        "diagnostic should mention 'type mismatch':\n{check_body}",
    );
}

// ---------------------------------------------------------------------------
// Scenario 4: WASM target — frontend pipeline consistency
// ---------------------------------------------------------------------------

/// `hew check` and `hew compile --target wasm32-unknown-unknown` both route
/// through `run_file_frontend_to_typecheck`.  For a source that is valid at the
/// frontend layer, both must accept it with no frontend-layer diagnostics.
///
/// `hew check` does not accept `--target` (it hardcodes
/// `enable_wasm_target: false`).  The invariant here is narrower: for a
/// program that has no WASM-gated symbols, both commands reach the frontend
/// successfully without rejecting the source.  Any codegen-layer rejection
/// (e.g. `WasmUnsupportedSubstrate` when duplex symbols are involved) is
/// post-frontend and is not part of the equivalence asserted here.
///
/// The duplex-specific WASM path is covered by the `#[ignore]`'d test in
/// `compile_wasm_parity.rs` (pending E3 HIR duplex bridge landing).
#[test]
fn wasm_target_compile_shares_frontend_with_check() {
    let fixture = repo_root().join("tests/vertical-slice/accept/01-arith.hew");
    assert!(fixture.exists(), "fixture not found: {}", fixture.display());
    let path = fixture.to_str().expect("fixture path is valid UTF-8");

    // `hew check` must accept the arithmetic fixture at the frontend.
    let check_out = run_check(path);
    assert!(
        check_out.status.success(),
        "hew check should accept 01-arith.hew:\n{}",
        describe_output(&check_out),
    );
    let check_stderr = strip_ansi(&String::from_utf8_lossy(&check_out.stderr));
    let check_has_errors = check_stderr
        .lines()
        .any(|l| l.contains(": error:") || l.contains(": warning:"));
    assert!(
        !check_has_errors,
        "hew check emitted frontend diagnostics for 01-arith.hew:\n{check_stderr}",
    );

    // `hew compile --target wasm32-unknown-unknown` must also not produce any
    // frontend-layer diagnostics for this fixture. We only inspect the absence
    // of `: error:` / `: warning:` lines that reference the fixture path.
    let (compile_out, _emit_dir) = run_compile_with_target(path, "wasm32-unknown-unknown");

    let compile_stderr = strip_ansi(&String::from_utf8_lossy(&compile_out.stderr));
    let compile_frontend_errors: Vec<&str> = compile_stderr
        .lines()
        .filter(|l| {
            (l.contains(": error:") || l.contains(": warning:")) && l.contains("01-arith.hew")
        })
        .collect();
    assert!(
        compile_frontend_errors.is_empty(),
        "hew compile --target wasm32-unknown-unknown emitted frontend diagnostics \
         for 01-arith.hew; this means the WASM target path diverges from hew check \
         at the frontend layer.\n\
         frontend lines:\n{}\n\
         full compile stderr:\n{compile_stderr}",
        compile_frontend_errors.join("\n"),
    );
}
