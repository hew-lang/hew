//! HTTP/WebSocket measured-string MIR canaries.
//!
//! This module is intentionally separate from the batch string-family canary.
//! Every newly measured symbol is exercised directly and through a Hew
//! forwarder. Borrowing the result with `.len()` must create exactly one
//! caller-side `hew_string_drop`; the forwarding function itself transfers the
//! result onward and must create none. An unmeasured HTTP string export remains
//! fail-closed with no synthetic release.

use std::fmt::Write as _;

use hew_hir::{lower_program, ResolutionCtx};
use hew_mir::{lower_hir_module, DropKind, ExitPath, Instr, IrPipeline};
use hew_types::module_registry::ModuleRegistry;
use hew_types::{Checker, ResolvedTy};

const MEASURED: &[(&str, usize)] = &[
    ("hew_http_request_body_string", 2),
    ("hew_http_request_header", 2),
    ("hew_http_request_method", 1),
    ("hew_http_request_path", 1),
    ("hew_http_response_body", 1),
    ("hew_http_response_content_type", 1),
    ("hew_http_response_header", 2),
    ("hew_ws_message_text", 1),
    ("hew_ws_last_error", 0),
];

fn params(arity: usize) -> String {
    (0..arity)
        .map(|index| format!("p{index}: i64"))
        .collect::<Vec<_>>()
        .join(", ")
}

fn args(arity: usize) -> String {
    (0..arity)
        .map(|index| format!("p{index}"))
        .collect::<Vec<_>>()
        .join(", ")
}

fn source() -> String {
    let mut source = String::from("extern \"C\" {\n");
    for (symbol, arity) in MEASURED {
        writeln!(source, "    fn {symbol}({}) -> string;", params(*arity)).unwrap();
    }
    // Same result type/family, deliberately unmeasured: it must stay refused.
    writeln!(
        source,
        "    fn hew_http_get_string(p0: i64) -> string;\n}}\n"
    )
    .unwrap();

    for (symbol, arity) in MEASURED {
        let suffix = symbol.strip_prefix("hew_").unwrap();
        let parameters = params(*arity);
        let arguments = args(*arity);
        writeln!(
            source,
            "fn wrap_{suffix}({parameters}) -> string {{\n    unsafe {{ {symbol}({arguments}) }}\n}}\n"
        )
        .unwrap();
        writeln!(
            source,
            "fn direct_{suffix}({parameters}) -> i64 {{\n    let value = unsafe {{ {symbol}({arguments}) }};\n    value.len()\n}}\n"
        )
        .unwrap();
        writeln!(
            source,
            "fn forwarded_{suffix}({parameters}) -> i64 {{\n    wrap_{suffix}({arguments}).len()\n}}\n"
        )
        .unwrap();
    }
    source.push_str(
        "fn unmeasured_http_get_string(p0: i64) -> i64 {\n\
         \x20   unsafe { hew_http_get_string(p0) }.len()\n\
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
fn measured_http_ws_results_release_once_direct_and_through_forwarders() {
    let source = source();
    let pipeline = pipeline_with_tc(&source);
    assert!(
        pipeline.diagnostics.is_empty(),
        "HTTP/WS canary must lower without diagnostics: {:#?}\nsource:\n{source}",
        pipeline.diagnostics
    );

    for (symbol, _) in MEASURED {
        let suffix = symbol.strip_prefix("hew_").unwrap();
        for caller in [format!("direct_{suffix}"), format!("forwarded_{suffix}")] {
            assert_eq!(
                total_string_drops(&pipeline, &caller),
                1,
                "{caller}: measured `{symbol}` result borrowed by `.len()` must \
                 carry exactly one caller-side hew_string_drop"
            );
        }
        assert_eq!(
            total_string_drops(&pipeline, &format!("wrap_{suffix}")),
            0,
            "wrap_{suffix}: forwarder transfers `{symbol}` result onward"
        );
    }
}

#[test]
fn unmeasured_http_string_export_remains_fail_closed() {
    let source = source();
    let pipeline = pipeline_with_tc(&source);
    assert_eq!(
        total_string_drops(&pipeline, "unmeasured_http_get_string"),
        0,
        "a fresh-looking HTTP string export with no measured retention row \
         must not gain a speculative caller-side release"
    );
}
