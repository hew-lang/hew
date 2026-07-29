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

#[test]
fn measured_quic_results_emit_exactly_one_release_through_forwarders() {
    let source = source();
    let ll = emit_ll(&source);
    for symbol in MEASURED {
        let suffix = symbol.strip_prefix("hew_").unwrap();
        for caller in [format!("direct_{suffix}"), format!("forwarded_{suffix}")] {
            let ir = function_ir(&ll, &caller);
            assert_eq!(
                ir.matches("call void @hew_string_drop(").count(),
                1,
                "{caller}: `{symbol}` must reach exactly one LLVM release:\n{ir}"
            );
        }
        let wrapper = format!("wrap_{suffix}");
        let ir = function_ir(&ll, &wrapper);
        assert_eq!(
            ir.matches("call void @hew_string_drop(").count(),
            0,
            "{wrapper}: forwarding `{symbol}` must not release early:\n{ir}"
        );
    }
}
