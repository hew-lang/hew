//! LLVM-IR emission teeth for the closure-env drop KEYSTONE: the per-closure
//! env free thunk (`__hew_closure_env_free_*`) must drop every OWNED captured
//! field before freeing the box, through the canonical per-field drop authority
//! (`emit_field_drop_step`).
//!
//! ## What this proves (and what it would have caught)
//!
//! Before the keystone the free thunk freed only the heap box
//! (`hew_dyn_box_free`) and never released the captured owning handle — an
//! escaping closure that captured a runtime-built `string`/`Vec` leaked it.
//! These tests assert the EXACT release symbol now appears inside the
//! per-closure free thunk:
//!   * a captured `string` → a `hew_string_drop` call inside
//!     `__hew_closure_env_free_*`;
//!   * a captured `Vec<i64>` → the managed Vec free inside the same thunk.
//!
//! A BitCopy-only capture (`i64`) must NOT gain any owned-field release — its
//! free thunk frees only the box, the pre-keystone posture for non-owning
//! captures. This is the negative control distinguishing a wired manifest from
//! a blanket "drop everything" (which would double-free a BitCopy alias).
//!
//! LESSONS applied:
//! - `drop-allowset-from-value-flow` (P0): the test asserts the exact emitted
//!   release symbol against a fix-disabled-equivalent negative control (the
//!   BitCopy capture), not a happy-path `> 0`.
//! - `borrow-classifier-completeness-or-leak` (P0): enumerates the emitted
//!   owned-field release per capture class.
//! - `boundary-fail-closed` (P0): absence of the release inside the free thunk
//!   is the gate condition.

use std::path::Path;

use hew_codegen_rs::{emit_module, EmitOptions};
use hew_types::module_registry::ModuleRegistry;
use hew_types::Checker;

/// Full pipeline (parse → typecheck → HIR → MIR → emit) returning the textual
/// LLVM IR. Type-checking is REQUIRED so closure capture facts and method-call
/// rewrites are produced — a `TypeCheckOutput::default()` would trip the HIR
/// `CheckerBoundaryViolation` guard that requires real typecheck output.
fn emit_ll(source: &str, module_name: &str) -> String {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    let output = hew_hir::lower_program(
        &parsed.program,
        &tc_output,
        &hew_hir::ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    assert!(
        output.diagnostics.is_empty(),
        "hir diagnostics: {:?}",
        output.diagnostics
    );
    let pipeline = hew_mir::lower_hir_module(&output.module);
    assert!(
        pipeline.diagnostics.is_empty(),
        "mir diagnostics: {:?}",
        pipeline.diagnostics
    );
    let tmp = std::env::temp_dir().join(format!("hew-closure-env-drop-{module_name}"));
    std::fs::create_dir_all(&tmp).expect("create out_dir");
    let options = EmitOptions {
        module_name,
        out_dir: &tmp,
        native: false,
        wasm: false,
        target_triple: None,
        debug: false,
        opt_level: hew_codegen_rs::OptLevel::O0,
        source_path: None,
    };
    let artefacts = emit_module(&pipeline, &options).expect("closure pipeline must emit");
    let ll_path: &Path = artefacts
        .ll_path
        .as_deref()
        .expect("emit_module must populate ll_path");
    std::fs::read_to_string(ll_path).expect("read emitted .ll")
}

/// Extract the body of the named function from emitted LLVM-IR text: every line
/// from `define ... @<name>(` through the matching closing `}`. Returns an
/// empty string if the function is not defined.
fn function_body(ll: &str, fn_name: &str) -> String {
    let needle = format!("@{fn_name}(");
    let mut body = String::new();
    let mut in_fn = false;
    for line in ll.lines() {
        if !in_fn && line.starts_with("define") && line.contains(&needle) {
            in_fn = true;
        }
        if in_fn {
            body.push_str(line);
            body.push('\n');
            if line.trim() == "}" {
                break;
            }
        }
    }
    body
}

/// The single per-closure env free thunk symbol emitted for the (only) closure
/// literal in these single-closure fixtures. The owner component is the
/// enclosing function symbol; the lone closure literal is id 0.
const FREE_THUNK: &str = "__hew_closure_env_free___hew_closure_invoke_make_0";

// ---------------------------------------------------------------------------
// String capture: the free thunk must release the captured string.
// ---------------------------------------------------------------------------

#[test]
fn captured_string_freed_inside_env_free_thunk() {
    let ll = emit_ll(
        "fn make(a: string, b: string) -> fn() -> i64 {\n\
        \x20   let label = a + b;\n\
        \x20   || label.len()\n\
        }\n\
        fn main() -> i64 { let f = make(\"x\", \"y\"); f() }\n",
        "string_capture",
    );
    let thunk = function_body(&ll, FREE_THUNK);
    assert!(
        !thunk.is_empty(),
        "no env free thunk `{FREE_THUNK}` emitted; the escaping string-capturing \
         closure must plant a per-closure free thunk. Emitted IR:\n{ll}"
    );
    assert!(
        thunk.contains("hew_string_drop"),
        "the env free thunk must release the captured `string` via `hew_string_drop` \
         BEFORE freeing the box -- without it the captured handle leaks. Thunk body:\n{thunk}"
    );
    assert!(
        thunk.contains("hew_dyn_box_free"),
        "the env free thunk must still free the box after dropping captures. \
         Thunk body:\n{thunk}"
    );
}

// ---------------------------------------------------------------------------
// Vec capture: the free thunk must release the captured Vec.
// ---------------------------------------------------------------------------

#[test]
fn captured_vec_freed_inside_env_free_thunk() {
    let ll = emit_ll(
        "fn make() -> fn() -> i64 {\n\
        \x20   var xs: Vec<i64> = Vec::new();\n\
        \x20   xs.push(10);\n\
        \x20   || xs.len()\n\
        }\n\
        fn main() -> i64 { let f = make(); f() }\n",
        "vec_capture",
    );
    let thunk = function_body(&ll, FREE_THUNK);
    assert!(
        !thunk.is_empty(),
        "no env free thunk `{FREE_THUNK}` emitted for the Vec-capturing closure. \
         Emitted IR:\n{ll}"
    );
    assert!(
        thunk.contains("hew_vec_free"),
        "the env free thunk must release the captured `Vec` via a `hew_vec_free*` call \
         before freeing the box. Thunk body:\n{thunk}"
    );
    assert!(
        thunk.contains("hew_dyn_box_free"),
        "the env free thunk must still free the box. Thunk body:\n{thunk}"
    );
}

// ---------------------------------------------------------------------------
// Negative control: a BitCopy (i64) capture must NOT gain an owned-field
// release. Its free thunk frees only the box -- a blanket "drop every field"
// would double-free the BitCopy alias of the caller's live binding.
// ---------------------------------------------------------------------------

#[test]
fn bitcopy_capture_has_no_owned_release_in_free_thunk() {
    let ll = emit_ll(
        "fn make(n: i64) -> fn() -> i64 {\n\
        \x20   || n\n\
        }\n\
        fn main() -> i64 { let f = make(7); f() }\n",
        "bitcopy_capture",
    );
    let thunk = function_body(&ll, FREE_THUNK);
    assert!(
        !thunk.is_empty(),
        "no env free thunk `{FREE_THUNK}` emitted for the i64-capturing closure. \
         Emitted IR:\n{ll}"
    );
    assert!(
        !thunk.contains("hew_string_drop") && !thunk.contains("hew_vec_free"),
        "a BitCopy (i64) capture must NOT emit any owned-field release in its free \
         thunk -- the value is a byte-copy, not an owned handle; releasing it would \
         corrupt unrelated memory. Thunk body:\n{thunk}"
    );
    assert!(
        thunk.contains("hew_dyn_box_free"),
        "the BitCopy-capture free thunk must still free the box. Thunk body:\n{thunk}"
    );
}
