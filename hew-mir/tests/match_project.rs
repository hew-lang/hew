use hew_hir::{lower_program, ResolutionCtx};
use hew_mir::{lower_hir_module, FieldOffset, Instr, IrPipeline, MirDiagnosticKind};
use hew_types::{module_registry::ModuleRegistry, Checker};

fn pipeline_with_tc(source: &str) -> IrPipeline {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    assert!(
        tc_output.errors.is_empty(),
        "type errors: {:#?}",
        tc_output.errors
    );
    let output = lower_program(
        &parsed.program,
        &tc_output,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    assert!(
        output.diagnostics.is_empty(),
        "HIR diagnostics: {:#?}",
        output.diagnostics
    );
    let pipeline = lower_hir_module(&output.module);
    assert!(
        pipeline.diagnostics.is_empty(),
        "MIR diagnostics: {:#?}",
        pipeline.diagnostics
    );
    pipeline
}

/// Run the full pipeline with type-checking, but do NOT assert empty MIR
/// diagnostics. Used when the test expects fail-closed rejection at MIR.
fn pipeline_with_tc_allow_diags(source: &str) -> IrPipeline {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    assert!(
        tc_output.errors.is_empty(),
        "type errors: {:#?}",
        tc_output.errors
    );
    let output = lower_program(
        &parsed.program,
        &tc_output,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    // HIR diagnostics are still expected to be clean; MIR is the fail-closed boundary.
    assert!(
        output.diagnostics.is_empty(),
        "HIR diagnostics: {:#?}",
        output.diagnostics
    );
    lower_hir_module(&output.module)
}

fn find_fn<'a>(p: &'a IrPipeline, name: &str) -> &'a hew_mir::RawMirFunction {
    p.raw_mir
        .iter()
        .find(|f| f.name == name)
        .unwrap_or_else(|| panic!("function `{name}` not found in raw_mir"))
}

#[test]
fn record_project_lowers_to_record_field_loads() {
    let pipeline = pipeline_with_tc(
        r"
type Point {
    x: i64,
    y: i64,
}

fn sum(p: Point) -> i64 {
    match p {
        Point { x, y } => x + y,
    }
}",
    );
    let func = find_fn(&pipeline, "sum");
    let field_loads = func
        .blocks
        .iter()
        .flat_map(|block| block.instructions.iter())
        .filter(|instr| matches!(instr, Instr::RecordFieldLoad { .. }))
        .count();
    assert_eq!(field_loads, 2);
}

#[test]
fn tuple_project_lowers_to_tuple_field_loads() {
    let pipeline = pipeline_with_tc(
        r"
fn sum(t: (i64, i64, i64)) -> i64 {
    match t {
        (a, b, c) => a + b + c,
    }
}",
    );
    let func = find_fn(&pipeline, "sum");
    let field_loads = func
        .blocks
        .iter()
        .flat_map(|block| block.instructions.iter())
        .filter(|instr| matches!(instr, Instr::TupleFieldLoad { .. }))
        .count();
    assert_eq!(field_loads, 3);
}

/// Regression: a record with an owning (non-BitCopy)
/// field is lowered by `lower_match_project` via the `RecordFieldLoad`
/// path — the binder enters `owned_locals` and the drop spine
/// (`derive_owned_record_drop_allowed`) excludes the source aggregate from
/// composite drop via the field-binder release-owner rule. This pins the
/// new invariant: no `NotYetImplemented` for the common shape, and the
/// destructure actually emits `RecordFieldLoad` instructions.
#[test]
fn owning_field_destructure_lowers_to_record_field_loads() {
    let pipeline = pipeline_with_tc_allow_diags(
        r"
type Holder {
    name: string,
    n: i64,
}

fn project(h: Holder) -> i64 {
    match h {
        Holder { name, n } => n,
    }
}",
    );
    let has_nyi = pipeline.diagnostics.iter().any(|d| {
        matches!(
            &d.kind,
            MirDiagnosticKind::NotYetImplemented { construct, .. }
                if construct.contains("non-BitCopy") || construct.contains("owning")
        )
    });
    assert!(
        !has_nyi,
        "non-BitCopy record destructure must lower without NYI; got: {:#?}",
        pipeline.diagnostics
    );
    let func = find_fn(&pipeline, "project");
    let field_loads = func
        .blocks
        .iter()
        .flat_map(|block| block.instructions.iter())
        .filter(|instr| matches!(instr, Instr::RecordFieldLoad { .. }))
        .count();
    assert!(
        field_loads >= 2,
        "expected RecordFieldLoad for both bound fields; got {field_loads}"
    );
}

/// T-1: `type R {{ b: i64, a: i64 }}` matched as `R {{ a, b }}` — declaration
/// order is NOT alphabetical. `a` is declared at index 1, `b` at index 0.
/// The pattern binds `a` first then `b`; the checker must assign
/// `a → FieldOffset(1)`, `b → FieldOffset(0)` so the loaded values are correct.
///
/// This test specifically exercises the `td.field_order` authority chain: any
/// code that sorts fields alphabetically would produce the opposite offsets
/// (`a → 0, b → 1`) and load the wrong values.
#[test]
fn record_project_declaration_order_not_alphabetical() {
    let pipeline = pipeline_with_tc(
        r"
type R {
    b: i64,
    a: i64,
}

fn diff(r: R) -> i64 {
    match r {
        R { a, b } => a - b,
    }
}",
    );
    let func = find_fn(&pipeline, "diff");
    // Collect all RecordFieldLoad instructions in emit order.
    let loads: Vec<FieldOffset> = func
        .blocks
        .iter()
        .flat_map(|block| block.instructions.iter())
        .filter_map(|instr| match instr {
            Instr::RecordFieldLoad { field_offset, .. } => Some(*field_offset),
            _ => None,
        })
        .collect();

    // Pattern `R { a, b }`: `a` is declared second (index 1), `b` first (index 0).
    // The checker resolves field names to declaration-order offsets, so the
    // lowered loads must be: first load for binding `a` at FieldOffset(1),
    // second load for binding `b` at FieldOffset(0).
    assert_eq!(
        loads.len(),
        2,
        "expected exactly 2 RecordFieldLoad instructions; got: {loads:?}"
    );
    assert_eq!(
        loads[0],
        FieldOffset(1),
        "binding `a` must load from FieldOffset(1) — declaration position of `a` in `R {{ b, a }}`; got loads: {loads:?}"
    );
    assert_eq!(
        loads[1],
        FieldOffset(0),
        "binding `b` must load from FieldOffset(0) — declaration position of `b` in `R {{ b, a }}`; got loads: {loads:?}"
    );
}
