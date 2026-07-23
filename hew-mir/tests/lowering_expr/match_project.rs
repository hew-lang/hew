use hew_hir::{lower_program, IntentKind, ResolutionCtx};
use hew_mir::{
    lower_hir_module, DropKind, ElabDrop, ExitPath, FieldOffset, Instr, IrPipeline,
    MirDiagnosticKind, MirStatement,
};
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

fn all_drops(pipeline: &IrPipeline, name: &str) -> Vec<ElabDrop> {
    pipeline
        .elaborated_mir
        .iter()
        .find(|function| function.name == name)
        .unwrap_or_else(|| panic!("function `{name}` not found in elaborated MIR"))
        .drop_plans
        .iter()
        .flat_map(|(_, plan)| plan.drops.iter().cloned())
        .collect()
}

fn return_drops(pipeline: &IrPipeline, name: &str) -> Vec<ElabDrop> {
    pipeline
        .elaborated_mir
        .iter()
        .find(|function| function.name == name)
        .unwrap_or_else(|| panic!("function `{name}` not found in elaborated MIR"))
        .drop_plans
        .iter()
        .filter(|(exit, _)| matches!(exit, ExitPath::Return { .. }))
        .flat_map(|(_, plan)| plan.drops.iter().cloned())
        .collect()
}

fn goto_drops(pipeline: &IrPipeline, name: &str) -> Vec<ElabDrop> {
    pipeline
        .elaborated_mir
        .iter()
        .find(|function| function.name == name)
        .unwrap_or_else(|| panic!("function `{name}` not found in elaborated MIR"))
        .drop_plans
        .iter()
        .filter(|(exit, _)| matches!(exit, ExitPath::Goto { .. }))
        .flat_map(|(_, plan)| plan.drops.iter().cloned())
        .collect()
}

fn drop_kind_counts(drops: &[ElabDrop]) -> (usize, usize, usize) {
    drops.iter().fold(
        (0, 0, 0),
        |(cow_heap, record_in_place, tuple_in_place), drop| match drop.kind {
            DropKind::CowHeap { .. } => (cow_heap + 1, record_in_place, tuple_in_place),
            DropKind::RecordInPlace => (cow_heap, record_in_place + 1, tuple_in_place),
            DropKind::TupleInPlace => (cow_heap, record_in_place, tuple_in_place + 1),
            _ => (cow_heap, record_in_place, tuple_in_place),
        },
    )
}

fn cow_drop_places(drops: &[ElabDrop]) -> std::collections::HashSet<hew_mir::Place> {
    drops
        .iter()
        .filter(|drop| matches!(drop.kind, DropKind::CowHeap { .. }))
        .map(|drop| drop.place)
        .collect()
}

fn source_consume_count(pipeline: &IrPipeline, name: &str, source_name: &str) -> usize {
    pipeline
        .thir
        .iter()
        .find(|function| function.name == name)
        .unwrap_or_else(|| panic!("function `{name}` not found in THIR"))
        .statements
        .iter()
        .filter(|statement| {
            matches!(
                statement,
                MirStatement::Use {
                    name,
                    intent: IntentKind::Consume,
                    ..
                } if name == source_name
            )
        })
        .count()
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

#[test]
fn owned_project_param_tail_and_nontail_have_identical_release_authority() {
    let pipeline = pipeline_with_tc(
        r"
type Packet {
    tag: string,
    body: string,
}

fn record_tail(p: Packet) -> i64 {
    match p {
        Packet { tag, body } => tag.len() + body.len(),
    }
}

fn record_nontail(p: Packet) -> i64 {
    let total = match p {
        Packet { tag, body } => tag.len() + body.len(),
    };
    total
}

fn tuple_tail(p: (string, string)) -> i64 {
    match p {
        (left, right) => left.len() + right.len(),
    }
}

fn tuple_nontail(p: (string, string)) -> i64 {
    let total = match p {
        (left, right) => left.len() + right.len(),
    };
    total
}
",
    );

    let record_tail = all_drops(&pipeline, "record_tail");
    let record_nontail = all_drops(&pipeline, "record_nontail");
    let tuple_tail = all_drops(&pipeline, "tuple_tail");
    let tuple_nontail = all_drops(&pipeline, "tuple_nontail");

    assert_eq!(drop_kind_counts(&record_tail), (6, 0, 0));
    assert_eq!(drop_kind_counts(&record_nontail), (6, 0, 0));
    assert_eq!(drop_kind_counts(&tuple_tail), (6, 0, 0));
    assert_eq!(drop_kind_counts(&tuple_nontail), (6, 0, 0));
    assert_eq!(cow_drop_places(&record_tail).len(), 2);
    assert_eq!(cow_drop_places(&record_nontail).len(), 2);
    assert_eq!(cow_drop_places(&tuple_tail).len(), 2);
    assert_eq!(cow_drop_places(&tuple_nontail).len(), 2);
    assert_eq!(
        drop_kind_counts(&return_drops(&pipeline, "record_tail")),
        (2, 0, 0)
    );
    assert_eq!(
        drop_kind_counts(&return_drops(&pipeline, "record_nontail")),
        (2, 0, 0)
    );
    assert_eq!(
        drop_kind_counts(&return_drops(&pipeline, "tuple_tail")),
        (2, 0, 0)
    );
    assert_eq!(
        drop_kind_counts(&return_drops(&pipeline, "tuple_nontail")),
        (2, 0, 0)
    );
    assert_eq!(
        drop_kind_counts(&record_tail),
        drop_kind_counts(&record_nontail)
    );
    assert_eq!(
        drop_kind_counts(&tuple_tail),
        drop_kind_counts(&tuple_nontail)
    );

    assert_eq!(source_consume_count(&pipeline, "record_tail", "p"), 1);
    assert_eq!(source_consume_count(&pipeline, "record_nontail", "p"), 1);
    assert_eq!(source_consume_count(&pipeline, "tuple_tail", "p"), 1);
    assert_eq!(source_consume_count(&pipeline, "tuple_nontail", "p"), 1);
}

#[test]
fn wildcard_project_param_borrows_and_keeps_one_caller_composite_drop() {
    let pipeline = pipeline_with_tc(
        r#"
type Packet {
    tag: string,
    body: string,
}

fn borrow_record(p: Packet) -> i64 {
    match p { _ => 1 }
}

fn borrow_tuple(p: (string, string)) -> i64 {
    match p { _ => 1 }
}

fn caller() -> i64 {
    let packet = Packet { tag: "o" + "k", body: "a" + "b" };
    let record_result = borrow_record(packet);
    let record_reuse = packet.tag.len();
    let tuple = ("c" + "d", "e" + "f");
    let tuple_result = borrow_tuple(tuple);
    let tuple_reuse = tuple.0.len();
    record_result + record_reuse + tuple_result + tuple_reuse
}
"#,
    );

    assert_eq!(source_consume_count(&pipeline, "borrow_record", "p"), 0);
    assert_eq!(source_consume_count(&pipeline, "borrow_tuple", "p"), 0);
    assert_eq!(
        drop_kind_counts(&all_drops(&pipeline, "borrow_record")),
        (0, 0, 0)
    );
    assert_eq!(
        drop_kind_counts(&all_drops(&pipeline, "borrow_tuple")),
        (0, 0, 0)
    );
    let caller_drops = all_drops(&pipeline, "caller");
    assert_eq!(drop_kind_counts(&caller_drops), (0, 4, 4));
    assert_eq!(
        drop_kind_counts(&return_drops(&pipeline, "caller")),
        (0, 1, 1)
    );
    let record_places = caller_drops
        .iter()
        .filter(|drop| matches!(drop.kind, DropKind::RecordInPlace))
        .map(|drop| drop.place)
        .collect::<std::collections::HashSet<_>>();
    let tuple_places = caller_drops
        .iter()
        .filter(|drop| matches!(drop.kind, DropKind::TupleInPlace))
        .map(|drop| drop.place)
        .collect::<std::collections::HashSet<_>>();
    assert_eq!(record_places.len(), 1);
    assert_eq!(tuple_places.len(), 1);
}

#[test]
fn predicate_project_arms_share_exact_selected_arm_shape() {
    let pipeline = pipeline_with_tc(
        r"
type R {
    b: i64,
    a: i64,
}

fn record_case(r: R) -> i64 {
    match r {
        R { a: 0, b } => 10,
        R { a, b } => 20,
    }
}

fn tuple(t: (i64, i64, i64)) -> i64 {
    match t {
        (0, y, z) => 30,
        (x, y, z) => 40,
    }
}
",
    );

    let record = find_fn(&pipeline, "record_case");
    let record_offsets = record
        .blocks
        .iter()
        .flat_map(|block| block.instructions.iter())
        .filter_map(|instr| match instr {
            Instr::RecordFieldLoad { field_offset, .. } => Some(*field_offset),
            _ => None,
        })
        .collect::<Vec<_>>();
    assert_eq!(
        record_offsets,
        vec![
            FieldOffset(1),
            FieldOffset(1),
            FieldOffset(0),
            FieldOffset(0)
        ]
    );
    assert_eq!(
        record
            .blocks
            .iter()
            .flat_map(|block| block.instructions.iter())
            .filter(|instr| matches!(instr, Instr::Move { .. }))
            .count(),
        3
    );

    let tuple = find_fn(&pipeline, "tuple");
    let tuple_indexes = tuple
        .blocks
        .iter()
        .flat_map(|block| block.instructions.iter())
        .filter_map(|instr| match instr {
            Instr::TupleFieldLoad { field_index, .. } => Some(*field_index),
            _ => None,
        })
        .collect::<Vec<_>>();
    assert_eq!(tuple_indexes, vec![0, 0, 1, 2, 1, 2]);
    assert_eq!(
        tuple
            .blocks
            .iter()
            .flat_map(|block| block.instructions.iter())
            .filter(|instr| matches!(instr, Instr::Move { .. }))
            .count(),
        3
    );

    let bind_counts = |name: &str| {
        pipeline
            .thir
            .iter()
            .find(|function| function.name == name)
            .unwrap_or_else(|| panic!("function `{name}` not found in THIR"))
            .statements
            .iter()
            .filter(|statement| matches!(statement, MirStatement::Bind { .. }))
            .count()
    };
    assert_eq!(bind_counts("record_case"), 3);
    assert_eq!(bind_counts("tuple"), 5);
    assert_eq!(source_consume_count(&pipeline, "record_case", "r"), 0);
    assert_eq!(source_consume_count(&pipeline, "tuple", "t"), 0);
    assert_eq!(
        drop_kind_counts(&all_drops(&pipeline, "record_case")),
        (0, 0, 0)
    );
    assert_eq!(drop_kind_counts(&all_drops(&pipeline, "tuple")), (0, 0, 0));
}

#[test]
fn skipped_owned_field_preflight_emits_no_partial_arm() {
    let pipeline = pipeline_with_tc_allow_diags(
        r#"
type Holder {
    label: string,
    op: fn(i64) -> i64,
}

fn make() -> Holder {
    let k = 3;
    Holder { label: "l-" + "x", op: |x| x * k }
}

fn main() -> i64 {
    let h = make();
    let _s = match h {
        Holder { label: l, op: _ } => l,
    };
    0
}
"#,
    );

    assert_eq!(pipeline.diagnostics.len(), 1);
    assert!(matches!(
        &pipeline.diagnostics[0].kind,
        MirDiagnosticKind::NotYetImplemented { construct, .. }
            if construct == "match-destructure wildcard on owned aggregate field"
    ));
    let main = find_fn(&pipeline, "main");
    assert_eq!(
        main.blocks
            .iter()
            .flat_map(|block| block.instructions.iter())
            .filter(|instr| matches!(instr, Instr::RecordFieldLoad { .. }))
            .count(),
        0
    );
    assert_eq!(
        pipeline
            .thir
            .iter()
            .find(|function| function.name == "main")
            .expect("main THIR")
            .statements
            .iter()
            .filter(|statement| matches!(statement, MirStatement::Bind { name, .. } if name == "l"))
            .count(),
        0
    );
}

#[test]
#[allow(
    clippy::too_many_lines,
    reason = "the test checks the complete record and tuple predicate-temp proof symmetrically"
)]
fn owned_predicate_only_chain_borrows_and_drops_string_temps_before_branch() {
    let pipeline = pipeline_with_tc(
        r#"
type Labels {
    first: string,
    code: i64,
    last: string,
}

fn record_case() -> i64 {
    let value = Labels { first: "a" + "b", code: 9, last: "y" + "z" };
    match value {
        Labels { first: "", code: 0, last: "z" } => 1,
        Labels { first: "a", code: -1, last: "" } => 2,
        Labels { first: _, code: _, last: _ } => 3,
    }
}

fn tuple_case() -> i64 {
    let value: (string, u8, string) = ("a" + "b", 9, "y" + "z");
    match value {
        ("", 0, "z") => 4,
        ("a", 255, "") => 5,
        (_, _, _) => 6,
    }
}
"#,
    );

    for (name, source_name, expected_loads) in [
        ("record_case", "value", 6usize),
        ("tuple_case", "value", 6usize),
    ] {
        let function = find_fn(&pipeline, name);
        assert_eq!(source_consume_count(&pipeline, name, source_name), 0);
        assert_eq!(
            function
                .blocks
                .iter()
                .flat_map(|block| block.instructions.iter())
                .filter(|instr| matches!(
                    instr,
                    Instr::RecordFieldLoad { .. } | Instr::TupleFieldLoad { .. }
                ))
                .count(),
            expected_loads
        );
        assert_eq!(
            function
                .blocks
                .iter()
                .flat_map(|block| block.instructions.iter())
                .filter(|instr| matches!(
                    instr,
                    Instr::Drop {
                        ty: hew_types::ResolvedTy::String,
                        drop_fn: Some(hew_mir::DropFnSpec::Release("hew_string_drop")),
                        ..
                    }
                ))
                .count(),
            4
        );
        assert_eq!(
            function
                .blocks
                .iter()
                .flat_map(|block| block.instructions.iter())
                .filter(|instr| matches!(instr, Instr::FieldDropInPlace { .. }))
                .count(),
            0
        );

        let mut ordered_string_temps = 0;
        for block in &function.blocks {
            for (load_idx, load) in block.instructions.iter().enumerate() {
                let loaded = match load {
                    Instr::RecordFieldLoad { dest, .. } | Instr::TupleFieldLoad { dest, .. }
                        if matches!(
                            dest,
                            hew_mir::Place::Local(local)
                                if matches!(
                                    function.locals.get(*local as usize),
                                    Some(hew_types::ResolvedTy::String)
                                )
                        ) =>
                    {
                        *dest
                    }
                    _ => continue,
                };
                let cmp_idx = block
                    .instructions
                    .iter()
                    .position(|instr| matches!(instr, Instr::IntCmp { lhs, .. } if *lhs == loaded))
                    .expect("string predicate comparison in load block");
                let drop_idx = block
                    .instructions
                    .iter()
                    .position(
                        |instr| matches!(instr, Instr::Drop { place, .. } if *place == loaded),
                    )
                    .expect("string predicate temporary drop in load block");
                assert!(load_idx < cmp_idx);
                assert!(cmp_idx < drop_idx);
                assert!(matches!(
                    block.terminator,
                    hew_mir::Terminator::Branch { .. }
                ));
                ordered_string_temps += 1;
            }
        }
        assert_eq!(ordered_string_temps, 4);
    }

    assert_eq!(
        drop_kind_counts(&return_drops(&pipeline, "record_case")),
        (0, 1, 0)
    );
    assert_eq!(
        drop_kind_counts(&return_drops(&pipeline, "tuple_case")),
        (0, 0, 1)
    );
}

#[test]
fn owned_predicate_parameter_tail_is_a_borrow() {
    let pipeline = pipeline_with_tc(
        r#"
type Packet { tag: string, body: string }

fn classify_record(p: Packet) -> i64 {
    match p {
        Packet { tag: "ok", body: _ } => 1,
        Packet { tag: _, body: _ } => 2,
    }
}

fn classify_tuple(p: (string, string)) -> i64 {
    match p {
        ("ok", _) => 3,
        (_, _) => 4,
    }
}

fn caller() -> i64 {
    let packet = Packet { tag: "o" + "k", body: "a" + "b" };
    let a = classify_record(packet) + packet.body.len();
    let tuple = ("o" + "k", "c" + "d");
    a + classify_tuple(tuple) + tuple.1.len()
}
"#,
    );

    assert_eq!(source_consume_count(&pipeline, "classify_record", "p"), 0);
    assert_eq!(source_consume_count(&pipeline, "classify_tuple", "p"), 0);
    assert_eq!(
        drop_kind_counts(&return_drops(&pipeline, "caller")),
        (0, 1, 1)
    );
}

fn selected_block_consume_counts(
    pipeline: &IrPipeline,
    name: &str,
    source_name: &str,
) -> Vec<usize> {
    find_fn(pipeline, name)
        .blocks
        .iter()
        .map(|block| {
            block
                .statements
                .iter()
                .filter(|statement| {
                    matches!(
                        statement,
                        MirStatement::Use {
                            name,
                            intent: IntentKind::Consume,
                            ..
                        } if name == source_name
                    )
                })
                .count()
        })
        .filter(|count| *count != 0)
        .collect()
}

#[test]
fn owned_predicate_extract_chain_consumes_and_discharges_every_selected_arm() {
    let pipeline = pipeline_with_tc(
        r#"
type Packet { tag: i64, left: string, right: string }

fn record_case(tag: i64) -> i64 {
    let value = Packet { tag: tag, left: "l" + "eft", right: "r" + "ight" };
    match value {
        Packet { tag: 0, left, right: _ } => left.len(),
        Packet { tag: _, left: _, right: _ } => 2,
    }
}

fn tuple_case(tag: i64) -> i64 {
    let value = (tag, "l" + "eft", "r" + "ight");
    match value {
        (0, left, _) => left.len(),
        (_, _, _) => 3,
    }
}

fn direct_return(tag: i64) -> i64 {
    let value = Packet { tag: tag, left: "l" + "eft", right: "r" + "ight" };
    match value {
        Packet { tag: 0, left, right: _ } => return left.len(),
        Packet { tag: _, left: _, right: _ } => 4,
    }
}
"#,
    );

    for (name, load_count, root_kind) in [
        ("record_case", 2usize, DropKind::RecordInPlace),
        ("tuple_case", 2usize, DropKind::TupleInPlace),
        ("direct_return", 2usize, DropKind::RecordInPlace),
    ] {
        let function = find_fn(&pipeline, name);
        assert_eq!(source_consume_count(&pipeline, name, "value"), 2);
        assert_eq!(
            selected_block_consume_counts(&pipeline, name, "value"),
            vec![1, 1]
        );
        assert!(function.blocks.iter().all(|block| {
            !matches!(block.terminator, hew_mir::Terminator::Branch { .. })
                || block.statements.iter().all(|statement| {
                    !matches!(
                        statement,
                        MirStatement::Use {
                            name,
                            intent: IntentKind::Consume,
                            ..
                        } if name == "value"
                    )
                })
        }));
        assert_eq!(
            function
                .blocks
                .iter()
                .flat_map(|block| block.instructions.iter())
                .filter(|instr| matches!(
                    instr,
                    Instr::RecordFieldLoad { .. } | Instr::TupleFieldLoad { .. }
                ))
                .count(),
            load_count
        );
        assert_eq!(
            function
                .blocks
                .iter()
                .flat_map(|block| block.instructions.iter())
                .filter(|instr| matches!(instr, Instr::FieldDropInPlace { .. }))
                .count(),
            4
        );
        assert_eq!(
            all_drops(&pipeline, name)
                .iter()
                .filter(|drop| drop.kind == root_kind)
                .count(),
            0
        );
    }
}

#[test]
fn owned_predicate_extract_binders_close_on_each_join_edge() {
    let pipeline = pipeline_with_tc(
        r"
#[resource]
type Token { id: i64 }

impl Token { fn close(self) {} }

type Packet { tag: i64, token: Token, text: string }

fn record_join(p: Packet) -> i64 {
    match p {
        Packet { tag: 0, token, text: _ } => token.id,
        Packet { tag: _, token, text: _ } => token.id,
    }
}

fn tuple_join(p: (i64, Token, string)) -> i64 {
    match p {
        (0, token, _) => token.id,
        (_, token, _) => token.id,
    }
}
",
    );

    for name in ["record_join", "tuple_join"] {
        let goto_resources = goto_drops(&pipeline, name)
            .into_iter()
            .filter(|drop| drop.kind == DropKind::Resource)
            .collect::<Vec<_>>();
        assert_eq!(
            goto_resources.len(),
            2,
            "each selected arm must close its transferred resource at the join edge"
        );
        assert_eq!(
            goto_resources
                .iter()
                .map(|drop| drop.place)
                .collect::<std::collections::HashSet<_>>()
                .len(),
            2,
            "the two exclusive selected arms must close distinct binder slots"
        );
        assert!(
            return_drops(&pipeline, name)
                .iter()
                .all(|drop| drop.kind != DropKind::Resource),
            "arm-local resource binders must not survive the join"
        );
    }
}

#[test]
fn owned_predicate_whole_binding_fallback_transfers_one_root_owner() {
    let pipeline = pipeline_with_tc(
        r#"
type Packet { tag: i64, left: string, right: string }

fn whole_case(tag: i64) -> i64 {
    let value = Packet { tag: tag, left: "l" + "eft", right: "r" + "ight" };
    match value {
        Packet { tag: 0, left, right: _ } => left.len(),
        whole => whole.right.len(),
    }
}
"#,
    );

    assert_eq!(source_consume_count(&pipeline, "whole_case", "value"), 2);
    assert_eq!(
        selected_block_consume_counts(&pipeline, "whole_case", "value"),
        vec![1, 1]
    );
    assert_eq!(
        pipeline
            .thir
            .iter()
            .find(|function| function.name == "whole_case")
            .expect("whole_case THIR")
            .statements
            .iter()
            .filter(
                |statement| matches!(statement, MirStatement::Bind { name, .. } if name == "whole")
            )
            .count(),
        1
    );
    let record_drop_places = all_drops(&pipeline, "whole_case")
        .iter()
        .filter(|drop| matches!(drop.kind, DropKind::RecordInPlace))
        .map(|drop| drop.place)
        .collect::<std::collections::HashSet<_>>();
    assert_eq!(record_drop_places.len(), 1);
}

#[test]
fn predicate_undischargeable_field_rejects_before_arm_cfg() {
    let pipeline = pipeline_with_tc_allow_diags(
        r#"
type Holder { tag: i64, label: string, op: fn(i64) -> i64 }

fn main() -> i64 {
    let k = 3;
    let value = Holder { tag: 0, label: "l" + "abel", op: |x| x * k };
    let _result = match value {
        Holder { tag: 0, label, op: _ } => label.len(),
        Holder { tag: _, label: _, op: _ } => 2,
    };
    0
}
"#,
    );

    assert_eq!(pipeline.diagnostics.len(), 1);
    assert!(matches!(
        &pipeline.diagnostics[0].kind,
        MirDiagnosticKind::NotYetImplemented { construct, .. }
            if construct == "match-destructure wildcard on owned aggregate field"
    ));
    assert_eq!(
        find_fn(&pipeline, "main")
            .blocks
            .iter()
            .flat_map(|block| block.instructions.iter())
            .filter(|instr| matches!(instr, Instr::RecordFieldLoad { .. }))
            .count(),
        0
    );
    assert_eq!(source_consume_count(&pipeline, "main", "value"), 0);
}

/// An owned call-carrier enum param gets a terminal `ValueSnapshotDrop` on
/// every exit, so a match arm that MOVES the payload out must neutralize the
/// variant slot ON THAT ARM — the returned binder becomes the sole owner and
/// the terminal drop observes null there, while a read-only sibling arm keeps
/// its slot live for the terminal drop. Path-sensitive by construction: the
/// neutralize sits in the arm's own block, never shared.
#[test]
fn carrier_param_move_out_arm_neutralizes_variant_slot() {
    let pipeline = pipeline_with_tc(
        r#"
fn ef(e: Result<string, string>) -> string {
    match e { Ok(x) => x, Err(y) => "e" + "rr" }
}

fn main() -> i64 {
    let s = ef(Ok("a" + "b"));
    s.len()
}
"#,
    );
    let ef = pipeline
        .raw_mir
        .iter()
        .find(|f| f.name == "ef")
        .expect("raw fn ef");

    // The move-out (Ok) arm neutralizes its variant payload slot; the
    // read-only (Err) arm must NOT — its payload is the terminal drop's.
    let neutralized_variants: Vec<u32> = ef
        .blocks
        .iter()
        .flat_map(|block| block.instructions.iter())
        .filter_map(|instr| match instr {
            Instr::NeutralizePayloadSlot {
                place:
                    hew_mir::Place::MachineVariant {
                        local: 0,
                        variant_idx,
                        ..
                    },
                ..
            } => Some(*variant_idx),
            _ => None,
        })
        .collect();
    assert_eq!(
        neutralized_variants,
        vec![0],
        "exactly the Ok arm's variant slot must be neutralized"
    );

    // The terminal carrier drop remains on the return path (it releases the
    // Err payload when that arm runs, and no-ops on the neutralized Ok path).
    assert!(
        ef.blocks.iter().any(|block| {
            block.instructions.iter().any(|instr| {
                matches!(
                    instr,
                    Instr::ValueSnapshotDrop {
                        value: hew_mir::Place::Local(0),
                        ..
                    }
                )
            })
        }),
        "the carrier param keeps its terminal snapshot drop"
    );
}
