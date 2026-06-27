//! End-to-end LLVM-IR emission tests for B-4's wrapping operator sugar.
//!
//! `&+` / `&-` / `&*` must emit plain LLVM `add` / `sub` / `mul`
//! instructions — no `llvm.*.with.overflow.*` intrinsic, no `@llvm.trap`
//! call, no `unreachable` terminator from the overflow path.
//!
//! LESSONS applied:
//! - `boundary-fail-closed` (P0): the *absence* of overflow intrinsics is
//!   the correctness invariant. A regression that accidentally routes `&+`
//!   through `IntArithChecked` would silently add a trap path the user
//!   explicitly opted out of by choosing `&+`.
//! - `exhaustive-coverage` (P0): one test per operator.

use std::path::Path;

use hew_codegen_rs::{emit_module, EmitOptions};
use hew_hir::{lower_program, verify_hir, ResolutionCtx};
use hew_types::TypeCheckOutput;

fn emit_ll(source: &str, module_name: &str) -> String {
    let parsed = hew_parser::parse(source);
    assert!(parsed.errors.is_empty(), "parse: {:?}", parsed.errors);
    let output = lower_program(
        &parsed.program,
        &TypeCheckOutput::default(),
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    let verify = verify_hir(&output.module);
    assert!(
        output.diagnostics.is_empty() && verify.is_empty(),
        "hir: {:?} verify: {:?}",
        output.diagnostics,
        verify
    );
    let pipeline = hew_mir::lower_hir_module(&output.module);
    assert!(
        pipeline.diagnostics.is_empty(),
        "mir diagnostics: {:?}",
        pipeline.diagnostics
    );
    let tmp = std::env::temp_dir().join(format!("hew-b4-{module_name}"));
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
    let artefacts =
        emit_module(&pipeline, &options).expect("wrapping pipeline must emit successfully");
    let ll_path: &Path = artefacts
        .ll_path
        .as_deref()
        .expect("emit_module must populate ll_path");
    std::fs::read_to_string(ll_path).expect("read emitted .ll")
}

/// Slice the emitted module down to the body of the `@main` function.
///
/// The negative assertions below (no `with.overflow`, no `@llvm.trap`) are
/// about the WRAPPING OPERATION under test, not the whole module: every module
/// now carries the always-spliced stdlib builtin impls, including the
/// `duration::from_*` constructors whose `n * 1<unit>` bodies legitimately use
/// `smul.with.overflow` + `@llvm.trap` (default trap-on-overflow arithmetic).
/// Scoping to `@main` keeps the assertion about `&+`/`&-`/`&*` precise.
fn main_body(ll: &str) -> String {
    let start = ll
        .find("define")
        .and_then(|defs| ll[defs..].find("@main").map(|m| defs + m))
        .map(|m| ll[..m].rfind("define").unwrap_or(m))
        .expect("emitted module must define @main");
    let rest = &ll[start..];
    let end = rest[1..]
        .find("\ndefine")
        .map_or(rest.len(), |next| next + 1);
    rest[..end].to_string()
}

// ---------------------------------------------------------------------------
// `&+` emits plain `add`, not a with.overflow intrinsic.
// ---------------------------------------------------------------------------

#[test]
fn wrapping_emission_add_emits_plain_add_not_intrinsic() {
    let ll = emit_ll(
        "fn main() -> i64 { let a: i64 = 1; let b: i64 = 2; a &+ b }",
        "wrap_add_i64",
    );
    let body = main_body(&ll);
    // Must contain a plain `add` instruction.
    assert!(
        body.contains(" = add "),
        "`&+` must emit a plain LLVM `add`; got:\n{body}"
    );
    // Must NOT call any with.overflow intrinsic.
    assert!(
        !body.contains("with.overflow"),
        "`&+` must not call any with.overflow intrinsic; got:\n{body}"
    );
    // Must NOT emit a trap call (no overflow path exists).
    assert!(
        !body.contains("@llvm.trap"),
        "`&+` must not emit @llvm.trap; got:\n{body}"
    );
}

// ---------------------------------------------------------------------------
// `&-` emits plain `sub`, not a with.overflow intrinsic.
// ---------------------------------------------------------------------------

#[test]
fn wrapping_emission_sub_emits_plain_sub_not_intrinsic() {
    let ll = emit_ll(
        "fn main() -> i64 { let a: i64 = 5; let b: i64 = 3; a &- b }",
        "wrap_sub_i64",
    );
    let body = main_body(&ll);
    assert!(
        body.contains(" = sub "),
        "`&-` must emit a plain LLVM `sub`; got:\n{body}"
    );
    assert!(
        !body.contains("with.overflow"),
        "`&-` must not call any with.overflow intrinsic; got:\n{body}"
    );
    assert!(
        !body.contains("@llvm.trap"),
        "`&-` must not emit @llvm.trap; got:\n{body}"
    );
}

// ---------------------------------------------------------------------------
// `&*` emits plain `mul`, not a with.overflow intrinsic.
// ---------------------------------------------------------------------------

#[test]
fn wrapping_emission_mul_emits_plain_mul_not_intrinsic() {
    let ll = emit_ll(
        "fn main() -> i64 { let a: i64 = 7; let b: i64 = 6; a &* b }",
        "wrap_mul_i64",
    );
    let body = main_body(&ll);
    assert!(
        body.contains(" = mul "),
        "`&*` must emit a plain LLVM `mul`; got:\n{body}"
    );
    assert!(
        !body.contains("with.overflow"),
        "`&*` must not call any with.overflow intrinsic; got:\n{body}"
    );
    assert!(
        !body.contains("@llvm.trap"),
        "`&*` must not emit @llvm.trap; got:\n{body}"
    );
}
