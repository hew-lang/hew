//! MIR-level tests for `bytes[...]` and `b"..."` literal lowering.
//!
//! Both forms produce `HirLiteral::Bytes` in HIR and `Instr::BytesLit` in
//! the raw MIR instruction stream.

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
    lower_hir_module(&output.module)
}

// ---------- bytes[...] array literal ----------

#[test]
fn bytes_array_literal_emits_bytes_lit_instr() {
    // `bytes[0x68, 0x65, 0x77]` must produce a single `Instr::BytesLit`
    // whose byte content is exactly the three elements.
    let pl = pipeline(r"fn main() -> i64 { let _b = bytes [0x68, 0x65, 0x77]; 0 }");
    assert!(
        pl.diagnostics.is_empty(),
        "`bytes[...]` literal must lower without diagnostics: {:?}",
        pl.diagnostics
    );
    let func = &pl.raw_mir[0];
    let found = func.blocks[0].instructions.iter().find_map(|i| match i {
        Instr::BytesLit { bytes, dest } => Some((bytes.clone(), *dest)),
        _ => None,
    });
    let (bytes, dest) =
        found.expect("`bytes[0x68, 0x65, 0x77]` must produce Instr::BytesLit in raw MIR");
    assert_eq!(
        bytes,
        vec![0x68u8, 0x65, 0x77],
        "Instr::BytesLit must carry the exact element bytes"
    );
    assert!(
        matches!(dest, Place::Local(_)),
        "Instr::BytesLit dest must be a Local place"
    );
}

#[test]
fn bytes_array_literal_empty_emits_bytes_lit_instr() {
    // An empty `bytes[]` must still produce `Instr::BytesLit` with an
    // empty byte vector — not a `UnitLit` or `ConstI64`.
    let pl = pipeline(r"fn main() -> i64 { let _b = bytes []; 0 }");
    assert!(
        pl.diagnostics.is_empty(),
        "`bytes[]` empty literal must lower without diagnostics: {:?}",
        pl.diagnostics
    );
    let func = &pl.raw_mir[0];
    let found = func.blocks[0].instructions.iter().find_map(|i| match i {
        Instr::BytesLit { bytes, .. } => Some(bytes.clone()),
        _ => None,
    });
    let bytes = found.expect("`bytes[]` must produce Instr::BytesLit in raw MIR");
    assert!(
        bytes.is_empty(),
        "Instr::BytesLit for `bytes[]` must carry an empty byte vec, got {bytes:?}"
    );
}

#[test]
fn bytes_array_literal_single_element_max_value() {
    // `bytes[0xFF]` — single element at the maximum u8 value (255).
    let pl = pipeline(r"fn main() -> i64 { let _b = bytes [0xFF]; 0 }");
    assert!(
        pl.diagnostics.is_empty(),
        "`bytes[0xFF]` literal must lower without diagnostics: {:?}",
        pl.diagnostics
    );
    let func = &pl.raw_mir[0];
    let found = func.blocks[0].instructions.iter().find_map(|i| match i {
        Instr::BytesLit { bytes, .. } => Some(bytes.clone()),
        _ => None,
    });
    let bytes = found.expect("`bytes[0xFF]` must produce Instr::BytesLit in raw MIR");
    assert_eq!(
        bytes,
        vec![0xFFu8],
        "Instr::BytesLit must carry [0xFF] for max-range byte element"
    );
}

// ---------- b"..." byte-string literal ----------

#[test]
fn byte_string_literal_emits_bytes_lit_instr() {
    // `b"AB"` must produce `Instr::BytesLit` with bytes [0x41, 0x42].
    let pl = pipeline(r#"fn main() -> i64 { let _b = b"AB"; 0 }"#);
    assert!(
        pl.diagnostics.is_empty(),
        "`b\"AB\"` literal must lower without diagnostics: {:?}",
        pl.diagnostics
    );
    let func = &pl.raw_mir[0];
    let found = func.blocks[0].instructions.iter().find_map(|i| match i {
        Instr::BytesLit { bytes, dest } => Some((bytes.clone(), *dest)),
        _ => None,
    });
    let (bytes, dest) = found.expect("`b\"AB\"` must produce Instr::BytesLit in raw MIR");
    assert_eq!(
        bytes,
        vec![0x41u8, 0x42],
        "Instr::BytesLit for b\"AB\" must carry [0x41, 0x42]"
    );
    assert!(
        matches!(dest, Place::Local(_)),
        "Instr::BytesLit dest must be a Local place"
    );
}

#[test]
fn byte_string_literal_empty_emits_bytes_lit_instr() {
    // `b""` must produce `Instr::BytesLit` with an empty byte vector.
    let pl = pipeline(r#"fn main() -> i64 { let _b = b""; 0 }"#);
    assert!(
        pl.diagnostics.is_empty(),
        "`b\"\"` empty byte-string literal must lower without diagnostics: {:?}",
        pl.diagnostics
    );
    let func = &pl.raw_mir[0];
    let found = func.blocks[0].instructions.iter().find_map(|i| match i {
        Instr::BytesLit { bytes, .. } => Some(bytes.clone()),
        _ => None,
    });
    let bytes = found.expect("`b\"\"` must produce Instr::BytesLit in raw MIR");
    assert!(
        bytes.is_empty(),
        "Instr::BytesLit for `b\"\"` must carry an empty byte vec, got {bytes:?}"
    );
}
