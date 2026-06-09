//! Slice 2 substrate: module-level `const` declarations folded in HIR are
//! converted to codegen-ready descriptors by `build_const_descriptors`.
//!
//! The descriptor table is the seam codegen consumes to back module globals
//! and resolve `ResolvedRef::Const(item_id)` references.

use hew_hir::{lower_program, verify_hir, HirModule, ResolutionCtx};
use hew_mir::{build_const_descriptors, lower_hir_module, Instr, MirConstValue};
use hew_types::TypeCheckOutput;

/// Lower source to a verified `HirModule` with no HIR diagnostics.
fn hir(source: &str) -> HirModule {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let output = lower_program(
        &parsed.program,
        &TypeCheckOutput::default(),
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    assert!(
        output.diagnostics.is_empty(),
        "HIR diagnostics: {:?}",
        output.diagnostics
    );
    let verify = verify_hir(&output.module);
    assert!(verify.is_empty(), "HIR verify: {verify:?}");
    output.module
}

#[test]
fn integer_const_builds_descriptor() {
    let module = hir(r"const X: i64 = 42; fn main() -> i64 { 0 }");
    let (consts, diags) = build_const_descriptors(&module);
    assert!(
        diags.is_empty(),
        "well-typed integer const must not raise diagnostics: {diags:?}"
    );
    assert_eq!(consts.len(), 1, "exactly one const descriptor expected");
    let c = &consts[0];
    assert_eq!(c.const_id, 0, "first const takes slot 0");
    assert_eq!(c.name, "X");
    assert_eq!(c.value, MirConstValue::Integer(42));
}

#[test]
fn folded_integer_arithmetic_builds_descriptor() {
    // `1 + 2 * 3` must be folded in HIR to a single Integer(7) descriptor.
    let module = hir(r"const N: i64 = 1 + 2 * 3; fn main() -> i64 { 0 }");
    let (consts, diags) = build_const_descriptors(&module);
    assert!(
        diags.is_empty(),
        "fold must not raise diagnostics: {diags:?}"
    );
    assert_eq!(consts.len(), 1);
    assert_eq!(consts[0].value, MirConstValue::Integer(7));
}

#[test]
fn signed_negative_integer_const_builds_descriptor() {
    let module = hir(r"const X: i64 = -13; fn main() -> i64 { 0 }");
    let (consts, diags) = build_const_descriptors(&module);
    assert!(diags.is_empty(), "diagnostics: {diags:?}");
    assert_eq!(consts.len(), 1);
    assert_eq!(consts[0].value, MirConstValue::Integer(-13));
}

#[test]
fn string_const_builds_descriptor() {
    let module = hir(r#"const NAME: String = "hew"; fn main() -> i64 { 0 }"#);
    let (consts, diags) = build_const_descriptors(&module);
    assert!(
        diags.is_empty(),
        "well-typed string const must not raise diagnostics: {diags:?}"
    );
    assert_eq!(consts.len(), 1);
    assert_eq!(consts[0].name, "NAME");
    assert_eq!(consts[0].value, MirConstValue::Str("hew".to_string()));
}

#[test]
fn multiple_consts_take_sequential_slots() {
    let module =
        hir(r"const A: i64 = 1; const B: i64 = 2; const C: i64 = 3; fn main() -> i64 { 0 }");
    let (consts, diags) = build_const_descriptors(&module);
    assert!(diags.is_empty(), "diagnostics: {diags:?}");
    assert_eq!(consts.len(), 3);
    assert_eq!(consts[0].const_id, 0);
    assert_eq!(consts[1].const_id, 1);
    assert_eq!(consts[2].const_id, 2);
    assert_eq!(consts[0].name, "A");
    assert_eq!(consts[1].name, "B");
    assert_eq!(consts[2].name, "C");
    // Each descriptor's item_id is distinct (ties references to their slot).
    assert_ne!(consts[0].item_id, consts[1].item_id);
    assert_ne!(consts[1].item_id, consts[2].item_id);
}

#[test]
fn module_without_consts_builds_empty_table() {
    let module = hir(r"fn main() -> i64 { 0 }");
    let (consts, diags) = build_const_descriptors(&module);
    assert!(consts.is_empty(), "no consts ⇒ empty descriptor table");
    assert!(diags.is_empty());
}

#[test]
fn lower_hir_module_wires_const_table_and_global_load() {
    let module = hir(r"const X: i64 = 42; fn main() -> i64 { return X; }");
    let pipeline = lower_hir_module(&module);
    assert!(
        pipeline.diagnostics.is_empty(),
        "const reference must lower without MIR diagnostics: {:?}",
        pipeline.diagnostics
    );
    assert_eq!(pipeline.user_consts.len(), 1);
    let item_id = pipeline.user_consts[0].item_id;
    let saw_load = pipeline.raw_mir.iter().any(|func| {
        func.blocks.iter().any(|block| {
            block.instructions.iter().any(|instr| {
                matches!(instr, Instr::ConstGlobalLoad { item_id: got, .. } if *got == item_id)
            })
        })
    });
    assert!(saw_load, "const reference must lower to ConstGlobalLoad");
}
