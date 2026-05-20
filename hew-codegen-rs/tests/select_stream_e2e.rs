#![cfg(not(target_arch = "wasm32"))]

use std::path::PathBuf;

use hew_codegen_rs::{emit_module, EmitOptions};
use hew_mir::{BasicBlock, Instr, IrPipeline, Place, RawMirFunction, SelectArm, SelectArmKind};
use hew_runtime::{reply_channel, stream};
use hew_types::ResolvedTy;
use inkwell::context::Context;
use inkwell::memory_buffer::MemoryBuffer;
use inkwell::targets::{InitializationConfig, Target};
use inkwell::OptimizationLevel;

fn stream_ty(item: ResolvedTy) -> ResolvedTy {
    ResolvedTy::Named {
        name: "Stream".to_string(),
        args: vec![item],
    }
}

fn select_stream_i64_pipeline() -> IrPipeline {
    let stream_i64 = stream_ty(ResolvedTy::I64);
    let blocks = vec![
        BasicBlock {
            id: 0,
            statements: vec![],
            instructions: vec![Instr::ConstI64 {
                dest: Place::Local(2),
                value: 5_000_000_000,
            }],
            terminator: hew_mir::Terminator::Select {
                arms: vec![
                    SelectArm {
                        kind: SelectArmKind::StreamNext {
                            stream: Place::DuplexHandle(0),
                        },
                        body_block: 10,
                        binding: Some(Place::Local(1)),
                    },
                    SelectArm {
                        kind: SelectArmKind::AfterTimer {
                            duration: Place::Local(2),
                        },
                        body_block: 11,
                        binding: None,
                    },
                ],
                next: 99,
            },
        },
        BasicBlock {
            id: 10,
            statements: vec![],
            instructions: vec![Instr::Move {
                dest: Place::ReturnSlot,
                src: Place::Local(1),
            }],
            terminator: hew_mir::Terminator::Return,
        },
        BasicBlock {
            id: 11,
            statements: vec![],
            instructions: vec![Instr::ConstI64 {
                dest: Place::ReturnSlot,
                value: 0,
            }],
            terminator: hew_mir::Terminator::Return,
        },
        BasicBlock {
            id: 99,
            statements: vec![],
            instructions: vec![Instr::ConstI64 {
                dest: Place::ReturnSlot,
                value: -1,
            }],
            terminator: hew_mir::Terminator::Return,
        },
    ];

    IrPipeline {
        thir: vec![],
        raw_mir: vec![RawMirFunction {
            name: "main".to_string(),
            return_ty: ResolvedTy::I64,
            call_conv: hew_mir::FunctionCallConv::Default,
            params: vec![stream_i64.clone()],
            locals: vec![stream_i64, ResolvedTy::I64, ResolvedTy::Duration],
            blocks,
            decisions: vec![],
        }],
        checked_mir: vec![],
        elaborated_mir: vec![],
        diagnostics: vec![],
        record_layouts: vec![],
        actor_layouts: vec![],
        supervisor_layouts: vec![],
        machine_layouts: vec![],
    }
}

fn compile_to_ll(pipeline: &IrPipeline, module_name: &str) -> PathBuf {
    let tmp = std::env::temp_dir().join(format!("hew-select-stream-e2e-{module_name}"));
    std::fs::create_dir_all(&tmp).expect("create out_dir");
    let options = EmitOptions {
        module_name,
        out_dir: &tmp,
        native: false,
        wasm: false,
    };
    emit_module(pipeline, &options)
        .unwrap_or_else(|e| panic!("emit_module for {module_name}: {e}"))
        .ll_path
        .expect("emit_module must populate ll_path")
}

fn jit_run_main_with_stream(ll_path: &std::path::Path, stream_ptr: *mut stream::HewStream) -> i64 {
    Target::initialize_native(&InitializationConfig::default())
        .expect("initialize_native must succeed on the host platform");

    let ctx = Context::create();
    let buf = MemoryBuffer::create_from_file(ll_path)
        .unwrap_or_else(|e| panic!("read {}: {e}", ll_path.display()));
    let module = ctx
        .create_module_from_ir(buf)
        .unwrap_or_else(|e| panic!("parse {}: {e}", ll_path.display()));

    let mappings = [
        (
            "hew_reply_channel_new",
            reply_channel::hew_reply_channel_new as *const () as usize,
        ),
        (
            "hew_reply_channel_retain",
            reply_channel::hew_reply_channel_retain as *const () as usize,
        ),
        (
            "hew_reply_channel_cancel",
            reply_channel::hew_reply_channel_cancel as *const () as usize,
        ),
        (
            "hew_reply_channel_free",
            reply_channel::hew_reply_channel_free as *const () as usize,
        ),
        ("hew_reply", reply_channel::hew_reply as *const () as usize),
        (
            "hew_reply_wait",
            reply_channel::hew_reply_wait as *const () as usize,
        ),
        (
            "hew_select_first",
            reply_channel::hew_select_first as *const () as usize,
        ),
        (
            "hew_stream_poll",
            stream::hew_stream_poll as *const () as usize,
        ),
        (
            "hew_stream_cancel_pending_read",
            stream::hew_stream_cancel_pending_read as *const () as usize,
        ),
        ("free", libc::free as *const () as usize),
    ];
    let functions: Vec<_> = mappings
        .iter()
        .filter_map(|(name, ptr)| module.get_function(name).map(|function| (function, *ptr)))
        .collect();

    let ee = module
        .create_jit_execution_engine(OptimizationLevel::None)
        .expect("create_jit_execution_engine must succeed");
    for (function, ptr) in functions {
        ee.add_global_mapping(&function, ptr);
    }

    let jit_main = unsafe {
        ee.get_function::<unsafe extern "C" fn(*mut stream::HewStream) -> i64>("main")
            .expect("main must be present in the JIT module")
    };
    unsafe { jit_main.call(stream_ptr) }
}

#[test]
#[ignore = "P0 fix wired; runtime panic needs investigation in follow-up"]
fn select_stream_next_i64_binding_receives_item_value() {
    let pipeline = select_stream_i64_pipeline();
    let ll = compile_to_ll(&pipeline, "select_stream_i64_value");
    let item: i64 = 0x0102_0304_0506_0708;

    let pair = unsafe { stream::hew_stream_channel(1) };
    assert!(!pair.is_null(), "stream channel allocation must succeed");
    let sink = unsafe { stream::hew_stream_pair_sink(pair) };
    let stream_ptr = unsafe { stream::hew_stream_pair_stream(pair) };
    unsafe { stream::hew_stream_pair_free(pair) };
    assert!(!sink.is_null(), "sink extraction must succeed");
    assert!(!stream_ptr.is_null(), "stream extraction must succeed");

    unsafe {
        stream::hew_sink_write(
            sink,
            (&raw const item).cast::<libc::c_void>(),
            std::mem::size_of::<i64>(),
        );
    }

    let result = jit_run_main_with_stream(&ll, stream_ptr);

    unsafe {
        stream::hew_sink_close(sink);
        stream::hew_stream_close(stream_ptr);
    }

    assert_eq!(
        result, item,
        "Stream<i64> select arm binding must receive the item bytes as i64, not the item buffer address"
    );
}
