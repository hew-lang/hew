use std::path::Path;

use hew_codegen_rs::{emit_module, EmitOptions};
use hew_hir::{lower_program, ResolutionCtx};
use hew_types::{module_registry::ModuleRegistry, Checker};

fn emit_ll(source: &str) -> String {
    let parsed = hew_parser::parse(source);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let checked = checker.check_program(&parsed.program);
    assert!(checked.errors.is_empty(), "{:#?}", checked.errors);
    let lowered = lower_program(
        &parsed.program,
        &checked,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    assert!(lowered.diagnostics.is_empty(), "{:#?}", lowered.diagnostics);
    let pipeline = hew_mir::lower_hir_module(&lowered.module);
    assert!(
        pipeline.diagnostics.is_empty(),
        "{:#?}",
        pipeline.diagnostics
    );
    let out = std::env::temp_dir().join("hew-snapshot-send-structural");
    std::fs::create_dir_all(&out).expect("create output directory");
    let artefacts = emit_module(
        &pipeline,
        &EmitOptions {
            module_name: "snapshot_send",
            out_dir: &out,
            native: false,
            wasm: false,
            target_triple: None,
            debug: false,
            opt_level: hew_codegen_rs::OptLevel::O0,
            source_path: None,
        },
    )
    .expect("emit snapshot-send module");
    let ll_path: &Path = artefacts.ll_path.as_deref().expect("LLVM path");
    std::fs::read_to_string(ll_path).expect("read LLVM")
}

#[test]
fn snapshot_send_materializes_independent_record_carriers() {
    let ll = emit_ll(
        r"
        type Boxed {
            payload: Vec<i64>,
        }

        actor Sink {
            receive fn take(value: Boxed, tag: i64) {}
        }

        fn main() {
            let a = spawn Sink;
            let b = spawn Sink;
            let value = Boxed { payload: [1, 2] };
            a.take(value, 7);
            b.take(value, 8);
            value.payload.push(9);
        }
        ",
    );

    assert!(
        ll.matches("call i32 @__hew_record_clone_inplace_Boxed")
            .count()
            >= 2,
        "{ll}"
    );
    assert!(ll.contains("call ptr @hew_vec_clone_owned"), "{ll}");
    assert_eq!(ll.matches("call i32 @hew_actor_send_by_id").count(), 2);
    assert!(
        ll.contains("call void @__hew_record_drop_inplace___hew_packed_args_main_0(ptr %local_10)"),
        "{ll}"
    );
    assert!(
        ll.contains("call void @__hew_record_drop_inplace___hew_packed_args_main_1(ptr %local_12)"),
        "{ll}"
    );
}
