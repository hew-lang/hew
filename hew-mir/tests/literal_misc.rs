use hew_hir::{lower_program, verify_hir, ResolutionCtx};
use hew_mir::{lower_hir_module, Instr, Place};
use hew_types::TypeCheckOutput;

fn pipeline(source: &str) -> hew_mir::IrPipeline {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let output = lower_program(&parsed.program, &TypeCheckOutput::default(), &ResolutionCtx);
    assert!(
        output.diagnostics.is_empty(),
        "HIR diagnostics: {:?}",
        output.diagnostics
    );
    let verify = verify_hir(&output.module);
    assert!(verify.is_empty(), "HIR verify: {verify:?}");
    lower_hir_module(&output.module)
}

// ---------- char literal ----------

#[test]
fn lower_char_literal_emits_char_lit_instr() {
    // `let c = 'a'` must lower to `Instr::CharLit` with Unicode scalar value
    // 97 (U+0061, LATIN SMALL LETTER A).
    let pipeline = pipeline(r"fn main() -> i64 { let _c = 'a'; 0 }");
    assert!(
        pipeline.diagnostics.is_empty(),
        "char literal must lower without diagnostics: {:?}",
        pipeline.diagnostics
    );
    let func = &pipeline.raw_mir[0];
    let found = func.blocks[0].instructions.iter().find_map(|i| match i {
        Instr::CharLit { value, dest } => Some((*value, *dest)),
        _ => None,
    });
    let (value, dest) = found.expect("char literal `'a'` must produce Instr::CharLit in raw MIR");
    assert_eq!(
        value, 97u32,
        "Instr::CharLit value must be Unicode scalar 97 for 'a'"
    );
    assert!(
        matches!(dest, Place::Local(_)),
        "Instr::CharLit dest must be a Local place"
    );
}

#[test]
fn lower_char_literal_unicode_scalar_value_preserved() {
    // Non-ASCII char: U+00E9 LATIN SMALL LETTER E WITH ACUTE (é), scalar value 233.
    let pipeline = pipeline(r"fn main() -> i64 { let _c = 'é'; 0 }");
    assert!(
        pipeline.diagnostics.is_empty(),
        "non-ASCII char literal must lower without diagnostics: {:?}",
        pipeline.diagnostics
    );
    let func = &pipeline.raw_mir[0];
    let value = func.blocks[0]
        .instructions
        .iter()
        .find_map(|i| match i {
            Instr::CharLit { value, .. } => Some(*value),
            _ => None,
        })
        .expect("non-ASCII char literal must produce Instr::CharLit");
    assert_eq!(
        value, 0xE9u32,
        "Instr::CharLit must carry the Unicode scalar value (0xE9 = 233)"
    );
}

// ---------- duration literal ----------

#[test]
fn lower_duration_literal_emits_duration_lit_instr() {
    // `let d = 5s` — the parser converts `5s` to 5_000_000_000 nanoseconds.
    let pipeline = pipeline(r"fn main() -> i64 { let _d = 5s; 0 }");
    assert!(
        pipeline.diagnostics.is_empty(),
        "duration literal must lower without diagnostics: {:?}",
        pipeline.diagnostics
    );
    let func = &pipeline.raw_mir[0];
    let found = func.blocks[0].instructions.iter().find_map(|i| match i {
        Instr::DurationLit { nanos, dest } => Some((*nanos, *dest)),
        _ => None,
    });
    let (nanos, dest) =
        found.expect("duration literal `5s` must produce Instr::DurationLit in raw MIR");
    assert_eq!(
        nanos, 5_000_000_000i64,
        "Instr::DurationLit nanos must be 5_000_000_000 for `5s`"
    );
    assert!(
        matches!(dest, Place::Local(_)),
        "Instr::DurationLit dest must be a Local place"
    );
}

#[test]
fn lower_duration_literal_milliseconds_carries_nanos() {
    // `100ms` → 100_000_000 nanoseconds.
    let pipeline = pipeline(r"fn main() -> i64 { let _d = 100ms; 0 }");
    assert!(
        pipeline.diagnostics.is_empty(),
        "millisecond duration literal must lower without diagnostics: {:?}",
        pipeline.diagnostics
    );
    let func = &pipeline.raw_mir[0];
    let nanos = func.blocks[0]
        .instructions
        .iter()
        .find_map(|i| match i {
            Instr::DurationLit { nanos, .. } => Some(*nanos),
            _ => None,
        })
        .expect("100ms must produce Instr::DurationLit");
    assert_eq!(
        nanos, 100_000_000i64,
        "Instr::DurationLit nanos must be 100_000_000 for `100ms`"
    );
}

// ---------- unit literal — HirLiteral::Unit is currently unreachable ----------
//
// `HirLiteral::Unit` is never produced by the HIR lowerer (no parser
// `Literal::Unit` exists). Unit-typed expressions reach MIR via other HIR
// node kinds (empty-else If, void returns, etc.), so there is no Hew source
// that exercises `Instr::UnitLit` today.
//
// The `Instr::UnitLit` variant and the `HirLiteral::Unit` lowering arm exist
// for exhaustiveness: a future parser `Literal::Unit` production will have a
// corresponding MIR representation without a retrofit. Tests for `Instr::UnitLit`
// will be added in the slice that wires the upstream producer.
