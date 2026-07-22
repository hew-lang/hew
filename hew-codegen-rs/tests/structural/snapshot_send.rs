use std::path::Path;

use hew_codegen_rs::{emit_module, EmitOptions};
use hew_hir::{lower_program, ResolutionCtx};
use hew_types::{module_registry::ModuleRegistry, Checker};

fn emit_ll(source: &str) -> String {
    emit_ll_for_target(source, None)
}

fn emit_ll_for_target(source: &str, target_triple: Option<&'static str>) -> String {
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
            target_triple,
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
        ll.contains("define internal void @__hew_message_drop_Sink"),
        "{ll}"
    );
    assert_eq!(
        ll.matches("call void @hew_actor_set_message_drop").count(),
        2,
        "direct spawns must register the typed message drop callback:\n{ll}"
    );
    assert!(
        ll.contains("call void @__hew_record_drop_inplace___hew_packed_args_main_0(ptr %local_10)"),
        "{ll}"
    );
    assert!(
        ll.contains("call void @__hew_record_drop_inplace___hew_packed_args_main_1(ptr %local_12)"),
        "{ll}"
    );
}

#[test]
fn indirect_enum_actor_messages_use_the_pointer_value_abi() {
    let ll = emit_ll(
        r"
        indirect enum Tree {
            Leaf(i64);
            Node(Tree, Tree);
        }

        actor Sink {
            receive fn take(value: Tree) {}
            receive fn tagged(tag: i64, value: Tree) {}
        }

        fn main() {
            let sink = spawn Sink;
            sink.take(Node(Leaf(1), Leaf(2)));
            sink.tagged(10, Node(Leaf(3), Leaf(4)));
        }
        ",
    );

    assert!(
        ll.contains("define internal ptr @__hew_actor_dispatch_Sink"),
        "{ll}"
    );
    assert!(ll.contains("load ptr, ptr %payload_src_ptr"), "{ll}");
    assert!(
        ll.contains("call void @__hew_indirect_enum_free_Tree(ptr"),
        "{ll}"
    );
    assert!(
        !ll.contains("load %Tree, ptr %payload_src_ptr"),
        "actor dispatch must not load an inline Tree from mailbox storage:\n{ll}"
    );
}

#[test]
fn indirect_enum_local_request_carriers_use_pointer_fields_on_native_and_wasm() {
    let source = include_str!("../../../examples/v05/indirect_enum_actor_message_requests.hew");

    for (target, ll) in [
        ("native", emit_ll_for_target(source, None)),
        (
            "wasm32",
            emit_ll_for_target(source, Some("wasm32-unknown-unknown")),
        ),
    ] {
        let pointer_carriers = ll
            .lines()
            .filter(|line| {
                line.starts_with("%__hew_packed_args_") && line.ends_with("= type { i64, ptr }")
            })
            .count();
        assert_eq!(
            pointer_carriers, 8,
            "{target}: every Ask, suspended Ask, Select ActorAsk, suspended Select, and Join producer must give its unique carrier an i64-plus-pointer body:\n{ll}"
        );
        assert!(
            !ll.contains("= type { i64, %Tree }"),
            "{target}: no local actor-request carrier may embed an inline Tree:\n{ll}"
        );
    }
}
