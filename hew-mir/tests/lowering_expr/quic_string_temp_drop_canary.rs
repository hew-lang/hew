//! Exact-drop MIR canaries for measured handle-scoped QUIC strings.
//!
//! Every admitted extern is borrowed directly and through a tail-return Hew
//! forwarder. The caller must own exactly one `hew_string_drop`; the forwarder
//! transfers the result onward and must not release it. An unknown same-family
//! extern remains fail-closed.

use std::fmt::Write as _;

use hew_hir::{lower_program, ResolutionCtx};
use hew_mir::{lower_hir_module, DropKind, ExitPath, Instr, IrPipeline};
use hew_types::module_registry::ModuleRegistry;
use hew_types::{Checker, ResolvedTy};

const MEASURED: &[&str] = &[
    "hew_quic_conn_last_error",
    "hew_quic_conn_local_addr",
    "hew_quic_conn_peer_addr",
    "hew_quic_endpoint_last_error",
    "hew_quic_endpoint_local_addr",
    "hew_quic_stream_last_error",
];

fn source() -> String {
    let mut source = String::from("extern \"C\" {\n");
    for symbol in MEASURED {
        writeln!(source, "    fn {symbol}(handle: i64) -> string;").unwrap();
    }
    writeln!(
        source,
        "    fn hew_quic_unmeasured_string_probe(handle: i64) -> string;\n}}\n"
    )
    .unwrap();

    for symbol in MEASURED {
        let suffix = symbol.strip_prefix("hew_").unwrap();
        writeln!(
            source,
            "fn wrap_{suffix}(handle: i64) -> string {{\n    unsafe {{ {symbol}(handle) }}\n}}\n"
        )
        .unwrap();
        writeln!(
            source,
            "fn direct_{suffix}(handle: i64) -> i64 {{\n    let value = unsafe {{ {symbol}(handle) }};\n    value.len()\n}}\n"
        )
        .unwrap();
        writeln!(
            source,
            "fn forwarded_{suffix}(handle: i64) -> i64 {{\n    wrap_{suffix}(handle).len()\n}}\n"
        )
        .unwrap();
    }
    source.push_str(
        "fn unmeasured_quic_string(handle: i64) -> i64 {\n\
         \x20   unsafe { hew_quic_unmeasured_string_probe(handle) }.len()\n\
         }\n",
    );
    source
}

fn pipeline_with_tc(source: &str) -> IrPipeline {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}\nsource:\n{source}",
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
    lower_hir_module(&output.module)
}

fn inline_string_drops(pipeline: &IrPipeline, fn_name: &str) -> usize {
    pipeline
        .raw_mir
        .iter()
        .filter(|function| function.name == fn_name)
        .flat_map(|function| function.blocks.iter())
        .flat_map(|block| block.instructions.iter())
        .filter(|instruction| {
            matches!(
                instruction,
                Instr::Drop {
                    ty: ResolvedTy::String,
                    drop_fn: Some(spec),
                    ..
                } if *spec == hew_mir::DropFnSpec::Release("hew_string_drop")
            )
        })
        .count()
}

fn return_exit_string_drops(pipeline: &IrPipeline, fn_name: &str) -> usize {
    let function = pipeline
        .elaborated_mir
        .iter()
        .find(|function| function.name == fn_name)
        .unwrap_or_else(|| panic!("missing elaborated MIR for {fn_name}"));
    function
        .drop_plans
        .iter()
        .filter(|(exit, _)| matches!(exit, ExitPath::Return { .. }))
        .map(|(_, plan)| {
            plan.drops
                .iter()
                .filter(|drop| {
                    matches!(
                        &drop.kind,
                        DropKind::CowHeap { release }
                            if release.release_symbol() == "hew_string_drop"
                    )
                })
                .count()
        })
        .max()
        .unwrap_or(0)
}

fn total_string_drops(pipeline: &IrPipeline, fn_name: &str) -> usize {
    inline_string_drops(pipeline, fn_name) + return_exit_string_drops(pipeline, fn_name)
}

#[test]
fn measured_quic_results_release_once_direct_and_through_forwarders() {
    let source = source();
    let pipeline = pipeline_with_tc(&source);
    assert!(
        pipeline.diagnostics.is_empty(),
        "QUIC canary must lower without diagnostics: {:#?}\nsource:\n{source}",
        pipeline.diagnostics
    );

    for symbol in MEASURED {
        let suffix = symbol.strip_prefix("hew_").unwrap();
        for caller in [format!("direct_{suffix}"), format!("forwarded_{suffix}")] {
            assert_eq!(
                total_string_drops(&pipeline, &caller),
                1,
                "{caller}: measured `{symbol}` result must carry exactly one \
                 caller-side hew_string_drop"
            );
        }
        assert_eq!(
            total_string_drops(&pipeline, &format!("wrap_{suffix}")),
            0,
            "wrap_{suffix}: forwarder transfers `{symbol}` onward"
        );
    }
}

#[test]
fn unknown_quic_string_export_remains_fail_closed() {
    let source = source();
    let pipeline = pipeline_with_tc(&source);
    assert_eq!(
        total_string_drops(&pipeline, "unmeasured_quic_string"),
        0,
        "an unknown QUIC string export must not gain a speculative release"
    );
}
