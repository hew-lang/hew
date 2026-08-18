//! Drop-plan coverage for close-obligated contextual payload binders.

use hew_mir::{
    CallAuthority, DropFnSpec, DropKind, ElaboratedMirFunction, ExitPath, Instr, IrPipeline,
    RawMirFunction, Terminator,
};
use hew_types::module_registry::ModuleRegistry;
use hew_types::runtime_call::{RuntimeCallFamily, RuntimeDropDescriptor};
use hew_types::Checker;

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
    let output = hew_hir::lower_program(
        &parsed.program,
        &tc_output,
        &hew_hir::ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    assert!(
        output.diagnostics.is_empty(),
        "HIR diagnostics: {:#?}",
        output.diagnostics
    );
    hew_mir::lower_hir_module(&output.module)
}

fn pipeline_with_runtime_contracts(source: &str) -> IrPipeline {
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
    let output = hew_hir::lower_program(
        &parsed.program,
        &tc_output,
        &hew_hir::ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    assert!(
        output.diagnostics.is_empty(),
        "HIR diagnostics: {:#?}",
        output.diagnostics
    );
    hew_mir::lower_hir_module(&output.module)
}

const PRELUDE: &str = r"
#[resource]
type Handle { raw: i64 }

impl Handle {
    fn close(self) {}
}

fn make() -> Result<Handle, string> {
    Ok(Handle { raw: 1 })
}
";

#[test]
fn if_let_explicit_close_retains_drop_place_authority() {
    let pipeline = pipeline_with_tc(&format!(
        r"{PRELUDE}
fn probe() {{
    if let .Ok(handle) = make() {{
        handle.close();
    }}
}}
"
    ));
    assert!(
        pipeline.diagnostics.is_empty(),
        "a consumed if-let resource binder must elaborate without losing its place: {:#?}",
        pipeline.diagnostics
    );
    let probe = pipeline
        .elaborated_mir
        .iter()
        .find(|function| function.name == "probe")
        .expect("probe function must lower");
    assert!(
        probe.drop_plans.iter().all(|(exit, plan)| {
            !matches!(exit, ExitPath::Return { .. })
                || plan
                    .drops
                    .iter()
                    .all(|drop| drop.kind != DropKind::Resource)
        }),
        "the explicit close must consume the binder before every reachable return: {:#?}",
        probe.drop_plans
    );
}

const SINK_MATRIX: &str = r#"
fn make(path: string) -> Result<Sink<string>, string> {
    unsafe {
        let sink = hew_stream_from_file_write(path);
        if hew_sink_is_valid(sink) {
            Ok(sink)
        } else {
            Err("open failed")
        }
    }
}

extern "C" {
    fn hew_stream_from_file_write(path: string) -> Sink<string>;
    fn hew_sink_is_valid(sink: Sink<string>) -> bool;
    fn hew_sink_write_string(sink: Sink<string>, data: string);
    fn hew_sink_close(consume sink: Sink<string>);
}

fn if_implicit(path: string) {
    if let .Ok(sink) = make(path) {
        sink.write("if implicit");
    }
}

fn if_explicit(path: string) {
    if let .Ok(sink) = make(path) {
        sink.write("if explicit");
        sink.close();
    }
}

fn match_implicit(path: string) {
    match make(path) {
        .Ok(sink) => sink.write("match implicit"),
        _ => {},
    }
}

fn match_explicit(path: string) {
    match make(path) {
        .Ok(sink) => {
            sink.write("match explicit");
            sink.close();
        },
        _ => {},
    }
}

fn while_implicit(path: string) {
    while let .Ok(sink) = make(path) {
        sink.write("while implicit");
        break;
    }
}

fn while_explicit(path: string) {
    while let .Ok(sink) = make(path) {
        sink.write("while explicit");
        sink.close();
        break;
    }
}

fn let_else_implicit(path: string) {
    let .Ok(sink) = make(path) else { return; };
    sink.write("let else implicit");
}

fn let_else_explicit(path: string) {
    let .Ok(sink) = make(path) else { return; };
    sink.write("let else explicit");
    sink.close();
}

fn if_explicit_early(path: string, before: bool, after: bool) {
    if let .Ok(sink) = make(path) {
        if before { return; }
        sink.close();
        if after { return; }
    }
}

fn main() {}
"#;

fn raw_function<'a>(pipeline: &'a IrPipeline, name: &str) -> &'a RawMirFunction {
    pipeline
        .raw_mir
        .iter()
        .find(|function| function.name == name)
        .unwrap_or_else(|| panic!("raw MIR function {name}"))
}

fn elaborated_function<'a>(pipeline: &'a IrPipeline, name: &str) -> &'a ElaboratedMirFunction {
    pipeline
        .elaborated_mir
        .iter()
        .find(|function| function.name == name)
        .unwrap_or_else(|| panic!("elaborated MIR function {name}"))
}

fn source_sink_close_count(function: &RawMirFunction) -> usize {
    function
        .blocks
        .iter()
        .filter(|block| {
            matches!(
                block.terminator,
                Terminator::Call {
                    authority: CallAuthority::Runtime(RuntimeCallFamily::SinkClose),
                    ..
                }
            )
        })
        .count()
}

fn transfer_neutralize_count(function: &RawMirFunction) -> usize {
    function
        .blocks
        .iter()
        .flat_map(|block| &block.instructions)
        .filter(|instruction| {
            matches!(
                instruction,
                Instr::NeutralizePayloadSlot {
                    transferee: Some(_),
                    ..
                }
            )
        })
        .count()
}

fn assert_each_exit_has_unique_close_authorities(function: &ElaboratedMirFunction) {
    for (exit, plan) in &function.drop_plans {
        let sink_closes = plan
            .drops
            .iter()
            .filter(|drop| {
                drop.kind == DropKind::Resource
                    && drop.drop_fn == Some(DropFnSpec::Runtime(RuntimeDropDescriptor::SinkClose))
            })
            .count();
        let enum_closes = plan
            .drops
            .iter()
            .filter(|drop| drop.kind == DropKind::EnumInPlace)
            .count();
        assert!(
            sink_closes <= 1 && enum_closes <= 1,
            "{} exit {exit:?} duplicates a close authority: {:#?}",
            function.name,
            plan.drops
        );
        for (index, drop) in plan.drops.iter().enumerate() {
            assert!(
                !plan.drops[index + 1..]
                    .iter()
                    .any(|later| later.place == drop.place && later.kind == drop.kind),
                "{} exit {exit:?} repeats the same place/drop kind: {:#?}",
                function.name,
                plan.drops
            );
        }
    }
}

#[allow(
    clippy::needless_pass_by_value,
    reason = "call sites construct their exact expected exit discriminants inline"
)]
fn assert_exit_drop_counts(
    function: &ElaboratedMirFunction,
    expected_exit: ExitPath,
    expected_sink_closes: usize,
    expected_enum_drops: usize,
) {
    let matching = function
        .drop_plans
        .iter()
        .filter(|(exit, _)| *exit == expected_exit)
        .collect::<Vec<_>>();
    assert_eq!(
        matching.len(),
        1,
        "{} must have exactly one {expected_exit:?} plan: {:#?}",
        function.name,
        function.drop_plans
    );
    let drops = &matching[0].1.drops;
    let sink_closes = drops
        .iter()
        .filter(|drop| {
            drop.kind == DropKind::Resource
                && drop.drop_fn == Some(DropFnSpec::Runtime(RuntimeDropDescriptor::SinkClose))
        })
        .count();
    let enum_drops = drops
        .iter()
        .filter(|drop| drop.kind == DropKind::EnumInPlace)
        .count();
    assert_eq!(
        (sink_closes, enum_drops, drops.len()),
        (
            expected_sink_closes,
            expected_enum_drops,
            expected_sink_closes + expected_enum_drops
        ),
        "{} {expected_exit:?} has the wrong close authorities: {drops:#?}",
        function.name
    );
}

#[test]
#[allow(
    clippy::too_many_lines,
    reason = "the parity matrix keeps every syntax form's exact CFG exits together"
)]
fn contextual_sink_matrix_has_one_close_authority_per_exit() {
    let pipeline = pipeline_with_runtime_contracts(SINK_MATRIX);
    assert!(
        pipeline.diagnostics.is_empty(),
        "contextual Sink matrix diagnostics: {:#?}",
        pipeline.diagnostics
    );
    let siblings = [
        ("if_implicit", "if_explicit"),
        ("match_implicit", "match_explicit"),
        ("while_implicit", "while_explicit"),
        ("let_else_implicit", "let_else_explicit"),
    ];
    for (implicit, explicit) in siblings {
        let implicit_raw = raw_function(&pipeline, implicit);
        let explicit_raw = raw_function(&pipeline, explicit);

        assert_eq!(transfer_neutralize_count(implicit_raw), 1, "{implicit}");
        assert_eq!(transfer_neutralize_count(explicit_raw), 1, "{explicit}");
        assert_eq!(source_sink_close_count(implicit_raw), 0, "{implicit}");
        assert_eq!(source_sink_close_count(explicit_raw), 1, "{explicit}");
    }

    let if_implicit = elaborated_function(&pipeline, "if_implicit");
    assert_exit_drop_counts(if_implicit, ExitPath::Return { block: 4 }, 0, 1);
    assert_exit_drop_counts(
        if_implicit,
        ExitPath::Goto {
            block: 5,
            target: 4,
        },
        1,
        0,
    );
    assert_exit_drop_counts(if_implicit, ExitPath::Cancel { block: 5 }, 1, 1);

    let if_explicit = elaborated_function(&pipeline, "if_explicit");
    assert_exit_drop_counts(if_explicit, ExitPath::Return { block: 4 }, 0, 1);
    assert_exit_drop_counts(
        if_explicit,
        ExitPath::Goto {
            block: 6,
            target: 4,
        },
        0,
        0,
    );
    assert_exit_drop_counts(if_explicit, ExitPath::Cancel { block: 6 }, 0, 1);

    let match_implicit = elaborated_function(&pipeline, "match_implicit");
    assert_exit_drop_counts(match_implicit, ExitPath::Return { block: 2 }, 0, 1);
    assert_exit_drop_counts(
        match_implicit,
        ExitPath::Goto {
            block: 6,
            target: 2,
        },
        1,
        0,
    );
    assert_exit_drop_counts(match_implicit, ExitPath::Cancel { block: 6 }, 1, 1);

    let match_explicit = elaborated_function(&pipeline, "match_explicit");
    assert_exit_drop_counts(match_explicit, ExitPath::Return { block: 2 }, 0, 1);
    assert_exit_drop_counts(
        match_explicit,
        ExitPath::Goto {
            block: 7,
            target: 2,
        },
        0,
        0,
    );
    assert_exit_drop_counts(match_explicit, ExitPath::Cancel { block: 7 }, 0, 1);

    let while_implicit = elaborated_function(&pipeline, "while_implicit");
    assert_exit_drop_counts(
        while_implicit,
        ExitPath::Goto {
            block: 5,
            target: 3,
        },
        0,
        1,
    );
    assert_exit_drop_counts(
        while_implicit,
        ExitPath::Goto {
            block: 6,
            target: 3,
        },
        1,
        1,
    );
    assert_exit_drop_counts(while_implicit, ExitPath::Cancel { block: 6 }, 1, 0);

    let while_explicit = elaborated_function(&pipeline, "while_explicit");
    assert_exit_drop_counts(
        while_explicit,
        ExitPath::Goto {
            block: 5,
            target: 3,
        },
        0,
        1,
    );
    assert_exit_drop_counts(
        while_explicit,
        ExitPath::Goto {
            block: 7,
            target: 3,
        },
        0,
        1,
    );
    assert_exit_drop_counts(while_explicit, ExitPath::Cancel { block: 7 }, 0, 0);

    let let_else_implicit = elaborated_function(&pipeline, "let_else_implicit");
    assert_exit_drop_counts(let_else_implicit, ExitPath::Return { block: 3 }, 0, 1);
    assert_exit_drop_counts(let_else_implicit, ExitPath::Return { block: 6 }, 1, 1);

    let let_else_explicit = elaborated_function(&pipeline, "let_else_explicit");
    assert_exit_drop_counts(let_else_explicit, ExitPath::Return { block: 3 }, 0, 1);
    assert_exit_drop_counts(let_else_explicit, ExitPath::Return { block: 7 }, 0, 1);

    for name in [
        "if_implicit",
        "if_explicit",
        "match_implicit",
        "match_explicit",
        "while_implicit",
        "while_explicit",
        "let_else_implicit",
        "let_else_explicit",
    ] {
        assert_each_exit_has_unique_close_authorities(elaborated_function(&pipeline, name));
    }

    let early_raw = raw_function(&pipeline, "if_explicit_early");
    let early_elab = elaborated_function(&pipeline, "if_explicit_early");
    assert_eq!(transfer_neutralize_count(early_raw), 1);
    assert_eq!(source_sink_close_count(early_raw), 1);
    assert_exit_drop_counts(early_elab, ExitPath::Return { block: 4 }, 0, 1);
    assert_exit_drop_counts(early_elab, ExitPath::Return { block: 5 }, 1, 1);
    assert_exit_drop_counts(early_elab, ExitPath::Return { block: 10 }, 0, 1);
    assert_exit_drop_counts(early_elab, ExitPath::Cancel { block: 12 }, 0, 1);
    assert_each_exit_has_unique_close_authorities(early_elab);
}
