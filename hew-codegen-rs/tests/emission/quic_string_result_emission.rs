//! LLVM exact-one-drop canaries for measured handle-scoped QUIC strings.

use std::fmt::Write as _;
use std::path::Path;

use hew_codegen_rs::{emit_module, EmitOptions};
use hew_hir::{lower_program, ResolutionCtx};
use hew_types::{module_registry::ModuleRegistry, Checker};

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
    source.push_str("}\n");

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
    source
}

fn emit_ll(source: &str) -> String {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}\nsource:\n{source}",
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
    assert!(
        pipeline.diagnostics.is_empty(),
        "MIR diagnostics must be empty before codegen: {:#?}",
        pipeline.diagnostics
    );
    let out_dir = std::env::temp_dir().join("hew-quic-string-result-emission");
    std::fs::create_dir_all(&out_dir).expect("create codegen out_dir");
    let options = EmitOptions {
        module_name: "quic_string_result_emission",
        out_dir: &out_dir,
        native: false,
        wasm: false,
        target_triple: None,
        debug: false,
        opt_level: hew_codegen_rs::OptLevel::O0,
        source_path: None,
    };
    let artefacts = emit_module(&pipeline, &options).expect("QUIC canary must emit");
    let ll_path: &Path = artefacts
        .ll_path
        .as_deref()
        .expect("emit_module must populate ll_path");
    std::fs::read_to_string(ll_path).expect("read emitted LLVM IR")
}

fn function_ir<'a>(ll: &'a str, name: &str) -> &'a str {
    let symbol = format!("@{name}(");
    let header = ll
        .lines()
        .find(|line| line.starts_with("define ") && line.contains(&symbol))
        .unwrap_or_else(|| panic!("LLVM IR must define `{name}`:\n{ll}"));
    let start = ll.find(header).expect("selected LLVM line has an offset");
    let body = &ll[start..];
    let end = body
        .find("\n}")
        .unwrap_or_else(|| panic!("LLVM definition for `{name}` must close:\n{body}"));
    &body[..end + 2]
}

fn block_ir<'a>(function_ir: &'a str, label: &str) -> &'a str {
    let marker = format!("\n{label}:");
    let start = function_ir
        .find(&marker)
        .unwrap_or_else(|| panic!("LLVM function must contain block `{label}`:\n{function_ir}"))
        + 1;
    let body = &function_ir[start..];
    let end = body.find("\n\n").unwrap_or(body.len());
    &body[..end]
}

fn assert_consumed_string_result_cleanup(ir: &str, caller: &str, producer: &str) {
    assert!(
        ir.contains(&format!("invoke ptr @{producer}(")),
        "{caller}: must invoke the canonical string producer `{producer}`:\n{ir}"
    );
    let release_blocks = ir
        .split("\n\n")
        .filter(|block| block.contains("call void @hew_string_drop("))
        .collect::<Vec<_>>();
    assert_eq!(
        release_blocks.len(),
        2,
        "{caller}: must have one normal and one mutually-exclusive unwind release block:\n{ir}"
    );
    assert_eq!(
        release_blocks
            .iter()
            .filter(|block| block.contains("ret i64 "))
            .count(),
        1,
        "{caller}: exactly one release block must be the normal return path:\n{ir}"
    );
    assert_eq!(
        release_blocks
            .iter()
            .filter(|block| block.contains("resume "))
            .count(),
        1,
        "{caller}: exactly one release block must be the unwind path:\n{ir}"
    );
    for block in release_blocks {
        assert_eq!(
            block.matches("call void @hew_string_drop(").count(),
            1,
            "{caller}: an executable cleanup path must release exactly once:\n{block}"
        );
        assert!(
            block.contains("store ptr null"),
            "{caller}: each release path must neutralize the temporary slot:\n{block}"
        );
    }
    let producer_unwind = block_ir(ir, "invoke.cleanup");
    assert_eq!(
        producer_unwind
            .matches("call void @hew_string_drop(")
            .count(),
        0,
        "{caller}: producer unwind precedes result materialization and must not release:\n\
         {producer_unwind}"
    );
}

#[test]
fn measured_quic_results_emit_exactly_one_release_through_forwarders() {
    let source = source();
    let ll = emit_ll(&source);
    for symbol in MEASURED {
        let suffix = symbol.strip_prefix("hew_").unwrap();
        let direct = format!("direct_{suffix}");
        assert_consumed_string_result_cleanup(function_ir(&ll, &direct), &direct, symbol);
        let wrapper = format!("wrap_{suffix}");
        let forwarded = format!("forwarded_{suffix}");
        assert_consumed_string_result_cleanup(function_ir(&ll, &forwarded), &forwarded, &wrapper);

        let ir = function_ir(&ll, &wrapper);
        assert!(
            ir.contains(&format!("invoke ptr @{symbol}(")),
            "{wrapper}: must invoke the canonical runtime producer `{symbol}`:\n{ir}"
        );
        assert_eq!(
            ir.matches("call void @hew_string_drop(").count(),
            0,
            "{wrapper}: forwarding `{symbol}` must not release early:\n{ir}"
        );
    }
}
