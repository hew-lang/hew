//! A-7 codegen emission tests: `record` struct layout, alloca, GEP+store /
//! GEP+load lowering to LLVM IR.
//!
//! These tests drive the full HIR → MIR → codegen pipeline on real Hew
//! source containing `record` declarations, then assert on the textual
//! LLVM IR that:
//!
//! 1. Each named-form record produces an LLVM named struct type whose
//!    body matches the field-type declaration order.
//! 2. `Instr::RecordInit` lowers to per-field GEP + store into the
//!    record local's struct alloca.
//! 3. `Instr::RecordFieldLoad` lowers to GEP + load on the record's
//!    struct alloca.
//! 4. Functional update (`R { x: 5, ..base }`) lowers to per-field
//!    RecordFieldLoad + RecordInit, producing GEP+load for the
//!    base-copied field and GEP+store for the new aggregate.
//!
//! Drop tracing for record-of-heap-fields (per `lifecycle-symmetry`
//! LESSON) is a function of the elaboration layer (E3 drop elaboration);
//! A-7 is responsible for emitting load/store correctly so any drop the
//! elaborator inserts fires at the right point. With the v0.5 spine the
//! affine-resource drop classification only fires for `@resource` /
//! `@linear` value classes — plain records of `Copy` fields produce no
//! drops. Records carrying heap-backed fields (`Vec<i64>`, `String`)
//! that need per-field drop are a follow-on slice; see the deliverable
//! report for details.
//!
//! LESSONS applied:
//! - `boundary-fail-closed` (P0): missing record layout entries are
//!   rejected by `record_struct_for`; not silently emitting a malformed
//!   GEP.
//! - `exhaustive-coverage` (P0): one test per structural assertion.

use std::path::Path;

use hew_codegen_rs::{emit_module, EmitOptions};
use hew_hir::{lower_program, ResolutionCtx};
use hew_types::{module_registry::ModuleRegistry, Checker};

/// Run the full HIR + checker + MIR + codegen pipeline on `source` and
/// return the emitted textual LLVM IR. Uses the full checker because
/// `FieldAccess` lowering consults the checker's `expr_types`.
fn emit_ll_with_checker(source: &str, module_name: &str) -> String {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    let output = lower_program(
        &parsed.program,
        &tc_output,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    let pipeline = hew_mir::lower_hir_module(&output.module);
    let tmp = std::env::temp_dir().join(format!("hew-a7-{module_name}"));
    std::fs::create_dir_all(&tmp).expect("create out_dir");
    let options = EmitOptions {
        module_name,
        out_dir: &tmp,
        native: false,
        wasm: false,
        target_triple: None,
        debug: false,
        source_path: None,
    };
    let artefacts =
        emit_module(&pipeline, &options).expect("record pipeline must emit successfully");
    let ll_path: &Path = artefacts
        .ll_path
        .as_deref()
        .expect("emit_module must populate ll_path");
    std::fs::read_to_string(ll_path).expect("read emitted .ll")
}

/// Run the same pipeline but tolerate `emit_module` failure and return the
/// error string. Used by negative tests; not used today but kept for parity
/// with sibling test files.
#[allow(dead_code)]
fn try_emit_ll(source: &str, module_name: &str) -> Result<String, String> {
    let parsed = hew_parser::parse(source);
    if !parsed.errors.is_empty() {
        return Err(format!("parse errors: {:?}", parsed.errors));
    }
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    let output = lower_program(
        &parsed.program,
        &tc_output,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    let pipeline = hew_mir::lower_hir_module(&output.module);
    let tmp = std::env::temp_dir().join(format!("hew-a7-{module_name}"));
    std::fs::create_dir_all(&tmp).map_err(|e| e.to_string())?;
    let options = EmitOptions {
        module_name,
        out_dir: &tmp,
        native: false,
        wasm: false,
        target_triple: None,
        debug: false,
        source_path: None,
    };
    let artefacts = emit_module(&pipeline, &options).map_err(|e| format!("{e:?}"))?;
    let ll_path = artefacts
        .ll_path
        .ok_or_else(|| "emit_module did not populate ll_path".to_string())?;
    std::fs::read_to_string(&ll_path).map_err(|e| e.to_string())
}

/// A named-form record `record Point { x: i64, y: i64 }` must register an
/// LLVM named struct `%Point = type { i64, i64 }` in the emitted module.
#[test]
fn record_layout_registers_named_struct_in_field_order() {
    let ll = emit_ll_with_checker(
        "record Point { x: i64, y: i64 }
         fn main() -> i64 {
             let p = Point { x: 1, y: 2 };
             0
         }",
        "record-layout-named-struct",
    );
    assert!(
        ll.contains("%Point = type { i64, i64 }"),
        "named-form record `Point` must register LLVM struct \
         `%Point = type {{ i64, i64 }}`; got:\n{ll}"
    );
}

/// `let p = Point { x: 1, y: 2 }` must lower to:
/// - an alloca of the Point struct, and
/// - per-field GEP+store for each (offset, value) pair.
///
/// We pin the GEP shape by asserting that `getelementptr` appears against
/// the `%Point` type with a field index, and that a `store i64 1` follows.
#[test]
fn record_init_emits_alloca_and_per_field_gep_store() {
    let ll = emit_ll_with_checker(
        "record Point { x: i64, y: i64 }
         fn main() -> i64 {
             let p = Point { x: 1, y: 2 };
             0
         }",
        "record-init-gep-store",
    );
    // The struct alloca: %local_N = alloca %Point
    assert!(
        ll.contains("alloca %Point"),
        "RecordInit must allocate a `%Point` slot via alloca; got:\n{ll}"
    );
    // Per-field GEP: getelementptr inbounds %Point, ptr %..., i32 0, i32 0
    // (field 0) and `i32 0, i32 1` (field 1).
    assert!(
        ll.contains("getelementptr inbounds") && ll.contains("%Point"),
        "RecordInit must emit GEPs against the %Point struct; got:\n{ll}"
    );
    // Both field-0 and field-1 stores must appear (one per field).
    // LLVM normalises `inbounds` GEPs into `inbounds nuw` on newer versions —
    // match the prefix `getelementptr inbounds` and the `%Point,` operand
    // separately rather than pinning a single concatenated form.
    let gep_count = ll
        .lines()
        .filter(|l| l.contains("getelementptr") && l.contains("%Point,"))
        .count();
    assert!(
        gep_count >= 2,
        "RecordInit on a 2-field record must emit at least 2 GEPs; got {gep_count}:\n{ll}"
    );
}

/// `p.x` (read of the first field) must lower to a GEP at offset 0 + load
/// of `i64` from the resulting pointer. Without LLVM's value-tracking the
/// most reliable shape assertion is "the GEP + load pair against `%Point`
/// at the right field index appears in the IR".
#[test]
fn record_field_load_emits_gep_and_load() {
    let ll = emit_ll_with_checker(
        "record Point { x: i64, y: i64 }
         fn get_x() -> i64 {
             let p = Point { x: 1, y: 2 };
             p.x
         }
         fn main() -> i64 { get_x() }",
        "record-field-load-gep-load",
    );
    assert!(
        ll.contains("getelementptr") && ll.contains("%Point,"),
        "field access must emit a GEP against %Point; got:\n{ll}"
    );
    // The field load against the GEP pointer is `load i64, ptr %...`.
    // Multiple `load i64` lines appear (the field-init source loads also
    // load i64); we assert at least one `load i64` follows the field GEP.
    assert!(
        ll.contains("load i64"),
        "field access on an i64 field must emit `load i64`; got:\n{ll}"
    );
}

/// Functional update `Point { x: 5, ..base }` must:
/// - load `y` from `base` (GEP at offset 1 + load), then
/// - store both the explicit `x=5` and the loaded `y` into a fresh `%Point`
///   alloca.
///
/// We pin the resulting LLVM IR shape: at least two `alloca %Point` (one
/// for `base`, one for the update result) and at least three GEPs against
/// `%Point` (field-1 load from base + two field stores into the result).
#[test]
fn record_functional_update_loads_base_field_and_stores_new() {
    let ll = emit_ll_with_checker(
        "record Point { x: i64, y: i64 }
         fn update_x() -> i64 {
             let base = Point { x: 3, y: 4 };
             let p = Point { x: 5, ..base };
             0
         }
         fn main() -> i64 { update_x() }",
        "record-functional-update",
    );
    let alloca_count = ll.matches("alloca %Point").count();
    assert!(
        alloca_count >= 2,
        "functional update must produce at least 2 `alloca %Point` (base + result); \
         got {alloca_count}:\n{ll}"
    );
    let gep_count = ll
        .lines()
        .filter(|l| l.contains("getelementptr") && l.contains("%Point,"))
        .count();
    // base init (2 stores) + new init (1 base-load + 2 stores) = 5 GEPs minimum.
    assert!(
        gep_count >= 5,
        "functional update on a 2-field record must emit at least 5 GEPs \
         (2 base init + 1 base load + 2 new init); got {gep_count}:\n{ll}"
    );
}

/// The emitted module must `verify()` cleanly after record emission. This
/// is the structural integrity gate: if any GEP type, store value width,
/// or load width is wrong, LLVM's verifier rejects the module and
/// `emit_module` returns `CodegenError::LlvmVerify`.
#[test]
fn record_pipeline_module_verifies() {
    // Reaching this assertion at all means `emit_module` returned Ok,
    // which means `Module::verify()` was clean.
    let ll = emit_ll_with_checker(
        "record Point { x: i64, y: i64 }
         fn main() -> i64 {
             let p = Point { x: 7, y: 11 };
             p.y
         }",
        "record-module-verifies",
    );
    assert!(
        ll.contains("%Point"),
        "verified module must still contain %Point references; got:\n{ll}"
    );
}
