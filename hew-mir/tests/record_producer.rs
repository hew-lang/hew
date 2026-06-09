/// MIR lowering tests for `HirExprKind::StructInit` → `Instr::RecordInit`
/// and `HirExprKind::FieldAccess` → `Instr::RecordFieldLoad`.
///
/// These tests pin the MIR instruction shape produced by A-6c's producer wiring.
/// Codegen emission (alloca / GEP / store / load) is A-7's responsibility.
///
/// ## Pipeline notes
///
/// - `StructInit` lowering derives field types from `HirItem::Record` entries
///   (added here: HIR now handles `Item::Record` in its item loop). The checker
///   is not needed for the instruction-shape assertions.
/// - `FieldAccess` lowering reads `expr_types` from the checker's output to
///   determine the field's result type (checker-authority invariant). The full
///   `Checker` pipeline is used for the field-access test.
/// - MIR diagnostics such as `UnknownType { name: "Point" }` and
///   `DecisionMapTotal` are expected when user-defined record types are present:
///   the current MIR spine knows only builtin types. These diagnostics do not
///   prevent the producer from emitting `RecordInit` / `RecordFieldLoad`; tests
///   assert only on the instruction shape, not on an empty diagnostic list.
use hew_hir::{lower_program, ResolutionCtx};
use hew_mir::{lower_hir_module, FieldOffset, Instr, IrPipeline, MirDiagnosticKind};
use hew_types::{module_registry::ModuleRegistry, Checker, TypeCheckOutput};

/// Run the HIR→MIR pipeline without a type-checker. Suitable for `StructInit`
/// tests where HIR does not consult the checker side-table for field types.
fn pipeline_no_tc(source: &str) -> IrPipeline {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let output = lower_program(&parsed.program, &TypeCheckOutput::default(), &ResolutionCtx);
    lower_hir_module(&output.module)
}

/// Run the full pipeline with type-checking. Required for `FieldAccess` because
/// HIR's `FieldAccess` lowering reads `expr_types` from `TypeCheckOutput` to
/// determine the field's result type (checker-authority invariant).
fn pipeline_with_tc(source: &str) -> IrPipeline {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    let output = lower_program(&parsed.program, &tc_output, &ResolutionCtx);
    lower_hir_module(&output.module)
}

/// Flatten all instructions from all blocks of the named function's raw MIR.
fn all_instrs(pl: &IrPipeline, fn_name: &str) -> Vec<Instr> {
    pl.raw_mir
        .iter()
        .find(|f| f.name == fn_name)
        .unwrap_or_else(|| panic!("{fn_name} must be in raw_mir"))
        .blocks
        .iter()
        .flat_map(|b| b.instructions.iter().cloned())
        .collect()
}

/// `record Point { x: i64, y: i64 }; let p = Point { x: 1, y: 2 };`
/// must produce a single `Instr::RecordInit` with two (offset, place) pairs
/// at offsets 0 (`x`) and 1 (`y`). MIR diagnostics for the user-defined type
/// are expected (unknown `ValueClass`) and do not block instruction emission.
#[test]
fn struct_init_emits_record_init_with_two_fields() {
    let pl = pipeline_no_tc(
        "record Point { x: i64, y: i64 }
         fn make() -> i64 {
             let p = Point { x: 1, y: 2 };
             0
         }",
    );
    // `NotYetImplemented` for the Point type is expected from existing MIR
    // slice limitations. We assert only that no such diagnostic was produced
    // *by our new code* (i.e., no "not registered in field-order table" error).
    let unexpected: Vec<_> = pl
        .diagnostics
        .iter()
        .filter(|d| {
            matches!(&d.kind, MirDiagnosticKind::NotYetImplemented { construct, .. }
                if construct.contains("not registered in field-order table"))
        })
        .collect();
    assert!(
        unexpected.is_empty(),
        "record field-order table lookup must not fail: {unexpected:?}"
    );

    let instrs = all_instrs(&pl, "make");
    let init = instrs.iter().find_map(|i| match i {
        Instr::RecordInit { fields, dest, .. } => Some((fields.clone(), *dest)),
        _ => None,
    });
    let (fields, _dest) = init.expect("RecordInit instruction must be present");
    assert_eq!(
        fields.len(),
        2,
        "RecordInit must carry exactly 2 fields; got: {fields:?}"
    );
    assert_eq!(
        fields[0].0,
        FieldOffset(0),
        "first field offset must be 0 (x)"
    );
    assert_eq!(
        fields[1].0,
        FieldOffset(1),
        "second field offset must be 1 (y)"
    );
}

/// `let p = Point { x: 1, y: 2 }; p.x` must produce `Instr::RecordFieldLoad`
/// at offset 0 (declaration order of `x` in `Point { x: i64, y: i64 }`).
/// Uses the full checker pipeline because HIR's `FieldAccess` lowering reads
/// `expr_types` for the result type.
#[test]
fn field_access_emits_record_field_load_at_correct_offset() {
    let pl = pipeline_with_tc(
        "record Point { x: i64, y: i64 }
         fn get_x() -> i64 {
             let p = Point { x: 1, y: 2 };
             p.x
         }",
    );
    // Assert no field-order lookup failure from our new code.
    let unexpected: Vec<_> = pl
        .diagnostics
        .iter()
        .filter(|d| {
            matches!(&d.kind, MirDiagnosticKind::NotYetImplemented { construct, .. }
                if construct.contains("not registered in field-order table")
                    || construct.contains("unknown field"))
        })
        .collect();
    assert!(
        unexpected.is_empty(),
        "field-order or field-name lookup must not fail: {unexpected:?}"
    );

    let instrs = all_instrs(&pl, "get_x");
    let load = instrs.iter().find_map(|i| match i {
        Instr::RecordFieldLoad {
            field_offset, dest, ..
        } => Some((*field_offset, *dest)),
        _ => None,
    });
    let (offset, _dest) = load.expect("RecordFieldLoad instruction must be present");
    assert_eq!(
        offset,
        FieldOffset(0),
        "`p.x` must load field at offset 0; got: {offset:?}"
    );
}

/// Functional update `Point { x: 5, ..base }` (where `base` is a locally
/// constructed value) must desugar to:
/// - one `RecordFieldLoad` at offset 1 (`y` from base), then
/// - one `RecordInit` with explicit x=5 at offset 0 and base-loaded y at offset 1.
///
/// The base is constructed inline to avoid function-parameter lowering
/// limitations in the current MIR spine.
#[test]
fn struct_init_functional_update_loads_base_fields_and_emits_record_init() {
    let pl = pipeline_no_tc(
        "record Point { x: i64, y: i64 }
         fn update_x() -> i64 {
             let base = Point { x: 3, y: 4 };
             let p = Point { x: 5, ..base };
             0
         }",
    );
    // Assert no field-order lookup failure.
    let unexpected: Vec<_> = pl
        .diagnostics
        .iter()
        .filter(|d| {
            matches!(&d.kind, MirDiagnosticKind::NotYetImplemented { construct, .. }
                if construct.contains("not registered in field-order table")
                    || construct.contains("missing field"))
        })
        .collect();
    assert!(
        unexpected.is_empty(),
        "functional-update field resolution must not fail: {unexpected:?}"
    );

    let instrs = all_instrs(&pl, "update_x");

    // There must be at least one RecordFieldLoad (for `y` from `base`).
    // The base construction also produces a RecordInit, so filter for the one
    // that loads offset 1 (y).
    let field_loads_at_1: Vec<_> = instrs
        .iter()
        .filter(|i| matches!(i, Instr::RecordFieldLoad { field_offset, .. } if *field_offset == FieldOffset(1)))
        .collect();
    assert_eq!(
        field_loads_at_1.len(),
        1,
        "functional update must emit exactly one RecordFieldLoad at offset 1 (y); got: {field_loads_at_1:?}"
    );

    // There must be exactly two RecordInits: one for `base`, one for `p`.
    let record_inits: Vec<_> = instrs
        .iter()
        .filter_map(|i| match i {
            Instr::RecordInit { fields, .. } => Some(fields.clone()),
            _ => None,
        })
        .collect();
    assert_eq!(
        record_inits.len(),
        2,
        "must have 2 RecordInit instructions (base construction + functional update); got: {record_inits:?}"
    );

    // The second RecordInit (the functional update) must have 2 fields in
    // declaration order: x@0, y@1.
    let update_fields = &record_inits[1];
    assert_eq!(
        update_fields.len(),
        2,
        "functional-update RecordInit must carry 2 fields; got: {update_fields:?}"
    );
    assert_eq!(
        update_fields[0].0,
        FieldOffset(0),
        "first field in functional-update RecordInit must be x at offset 0"
    );
    assert_eq!(
        update_fields[1].0,
        FieldOffset(1),
        "second field in functional-update RecordInit must be y at offset 1"
    );
}
