//! End-to-end execution coverage for the memory-intrinsic floor (W5.005 /
//! F1b, Decision 4 Option A).
//!
//! `mem_floor_intrinsic_emission.rs` proves the *static* shape: each of the
//! five `mem.*` floor functions emits a real body (`call @hew_alloc`, i8 GEP,
//! `@llvm.memcpy`, …) and an unknown id fails closed. This file proves the
//! *dynamic* contract by JIT-executing a `main` that threads pointers through
//! the synthesized bodies and observing — in Rust space, via mock allocator
//! stubs — that:
//!
//!   1. `mem.alloc` actually invokes `hew_alloc` with the caller's
//!      `(size, align)` and **returns the runtime allocator's real pointer**
//!      (the D343 fail-OPEN kill at execution level: an empty-body no-op
//!      would have returned `undef`, so `dealloc` would not receive the
//!      allocator's pointer);
//!   2. that real pointer threads through `main` into `mem.dealloc` with the
//!      matching `(size, align)`;
//!   3. `mem.ptr_offset` (i8 GEP) and `mem.ptr_copy` (`@llvm.memcpy`) execute
//!      on the live buffers without crashing.
//!
//! There is NO user surface for these primitives (A605 gates them to the
//! `std.mem` floor) and no container consumer yet, so the honest e2e is a
//! hand-built pipeline JIT-executed with the allocator mocked — never a
//! constant-folded fake-green.

#![cfg(not(target_arch = "wasm32"))]
#![cfg(unix)]

use std::alloc::{alloc, dealloc, Layout};
use std::path::Path;
use std::sync::Mutex;

use hew_codegen_rs::{emit_module, CodegenError, EmitOptions};
use hew_mir::{
    BasicBlock, CheckedMirFunction, ElaboratedMirFunction, FunctionCallConv, Instr, IrPipeline,
    Place, RawMirFunction, Terminator,
};
use hew_types::ResolvedTy;
use inkwell::context::Context;
use inkwell::memory_buffer::MemoryBuffer;
use inkwell::targets::{InitializationConfig, Target};
use inkwell::OptimizationLevel;

// ── mock runtime allocator (records what the emitted code actually passes) ──

/// `(ptr_addr, size, align)` recorded by `mock_alloc` in call order.
static ALLOC_LOG: Mutex<Vec<(usize, u64, u64)>> = Mutex::new(Vec::new());
/// `(ptr_addr, size, align)` recorded by `mock_dealloc` in call order.
static DEALLOC_LOG: Mutex<Vec<(usize, u64, u64)>> = Mutex::new(Vec::new());
/// Serialises the JIT-executing tests. Both clear and read the shared
/// `ALLOC_LOG`/`DEALLOC_LOG`, so they must not run concurrently (threaded
/// `cargo test` would otherwise interleave their allocation records and break
/// the index-keyed fill in `mock_alloc`).
static EXEC_GUARD: Mutex<()> = Mutex::new(());

extern "C" fn mock_alloc(size: u64, align: u64) -> *mut u8 {
    let layout = Layout::from_size_align(size as usize, align as usize)
        .expect("test uses a valid (size, align)");
    // SAFETY: layout is non-zero and valid; matched by `mock_dealloc`.
    let ptr = unsafe { alloc(layout) };
    let mut log = ALLOC_LOG.lock().expect("ALLOC_LOG poisoned");
    // Stamp each allocation with a distinct, deterministic byte pattern keyed
    // off the allocation index. This gives `mem.ptr_copy` an observable
    // before/after: the source buffer carries one fill, the destination
    // another, so a real `@llvm.memcpy` is provable by reading the bytes back
    // (a no-op copy would leave the destination's own fill intact).
    let fill = 0xA0u8.wrapping_add(log.len() as u8);
    // SAFETY: `ptr` owns `size` writable bytes from the layout above.
    unsafe { std::ptr::write_bytes(ptr, fill, size as usize) };
    log.push((ptr as usize, size, align));
    ptr
}

extern "C" fn mock_dealloc(ptr: *mut u8, size: u64, align: u64) {
    DEALLOC_LOG
        .lock()
        .expect("DEALLOC_LOG poisoned")
        .push((ptr as usize, size, align));
    let layout = Layout::from_size_align(size as usize, align as usize)
        .expect("test uses a valid (size, align)");
    // SAFETY: `ptr` came from `mock_alloc` with the same layout.
    unsafe { dealloc(ptr, layout) };
}

/// `mem.realloc` is never called by the driver, but `mem$realloc` is a defined
/// function in the module and MCJIT compiles the whole module eagerly — so its
/// call to `hew_realloc` must resolve to *something* at finalization or the
/// engine aborts. This mock satisfies that resolution; it is not exercised.
extern "C" fn mock_realloc(ptr: *mut u8, _old_size: u64, _new_size: u64, _align: u64) -> *mut u8 {
    ptr
}

// ── pipeline builders ──

fn mut_u8_ptr() -> ResolvedTy {
    ResolvedTy::Pointer {
        is_mutable: true,
        pointee: Box::new(ResolvedTy::U8),
    }
}

/// One bodyless floor-intrinsic function, tagged with its catalog id. The
/// blocks are a placeholder; codegen synthesizes the real body.
fn floor_fn(name: &str, id: &str, params: Vec<ResolvedTy>, ret: ResolvedTy) -> RawMirFunction {
    RawMirFunction {
        name: name.to_string(),
        return_ty: ret,
        call_conv: FunctionCallConv::Default,
        params: params.clone(),
        locals: params,
        blocks: vec![BasicBlock {
            id: 0,
            statements: vec![],
            instructions: vec![],
            terminator: Terminator::Return,
        }],
        decisions: vec![],
        intrinsic_id: Some(id.to_string()),
        await_deadline_ns: std::collections::HashMap::new(),

        lambda_actor_user_param_locals: Vec::new(),
        span: None,
        instr_spans: ::std::collections::BTreeMap::new(),
    }
}

/// `main() -> i64` that exercises every floor primitive and returns 0:
///   p = alloc(64, 8); r = alloc(64, 8);
///   ptr_copy(r, p, 32); q = ptr_offset(p, 16);
///   dealloc(p, 64, 8); dealloc(r, 64, 8); return 0
fn driver_main() -> RawMirFunction {
    // locals: 0=size(64) 1=align(8) 2=p 3=r 4=copy_count(32) 5=offset(16) 6=q
    let locals = vec![
        ResolvedTy::I64,
        ResolvedTy::I64,
        mut_u8_ptr(),
        mut_u8_ptr(),
        ResolvedTy::I64,
        ResolvedTy::I64,
        mut_u8_ptr(),
    ];
    let blocks = vec![
        BasicBlock {
            id: 0,
            statements: vec![],
            instructions: vec![
                Instr::ConstI64 {
                    dest: Place::Local(0),
                    value: 64,
                },
                Instr::ConstI64 {
                    dest: Place::Local(1),
                    value: 8,
                },
                Instr::ConstI64 {
                    dest: Place::Local(4),
                    value: 32,
                },
                Instr::ConstI64 {
                    dest: Place::Local(5),
                    value: 16,
                },
            ],
            terminator: Terminator::Call {
                callee: "mem$alloc".to_string(),
                builtin: hew_types::runtime_call::RuntimeCallFamily::from_c_symbol("mem$alloc"),
                args: vec![Place::Local(0), Place::Local(1)],
                dest: Some(Place::Local(2)),
                next: 1,
            },
        },
        BasicBlock {
            id: 1,
            statements: vec![],
            instructions: vec![],
            terminator: Terminator::Call {
                callee: "mem$alloc".to_string(),
                builtin: hew_types::runtime_call::RuntimeCallFamily::from_c_symbol("mem$alloc"),
                args: vec![Place::Local(0), Place::Local(1)],
                dest: Some(Place::Local(3)),
                next: 2,
            },
        },
        BasicBlock {
            id: 2,
            statements: vec![],
            instructions: vec![],
            terminator: Terminator::Call {
                callee: "mem$ptr_copy".to_string(),
                builtin: hew_types::runtime_call::RuntimeCallFamily::from_c_symbol("mem$ptr_copy"),
                // dst=r, src=p, byte_count=32
                args: vec![Place::Local(3), Place::Local(2), Place::Local(4)],
                dest: None,
                next: 3,
            },
        },
        BasicBlock {
            id: 3,
            statements: vec![],
            instructions: vec![],
            terminator: Terminator::Call {
                callee: "mem$ptr_offset".to_string(),
                builtin: hew_types::runtime_call::RuntimeCallFamily::from_c_symbol(
                    "mem$ptr_offset",
                ),
                // p, byte_offset=16 -> q
                args: vec![Place::Local(2), Place::Local(5)],
                dest: Some(Place::Local(6)),
                next: 4,
            },
        },
        BasicBlock {
            id: 4,
            statements: vec![],
            instructions: vec![],
            terminator: Terminator::Call {
                callee: "mem$dealloc".to_string(),
                builtin: hew_types::runtime_call::RuntimeCallFamily::from_c_symbol("mem$dealloc"),
                args: vec![Place::Local(2), Place::Local(0), Place::Local(1)],
                dest: None,
                next: 5,
            },
        },
        BasicBlock {
            id: 5,
            statements: vec![],
            instructions: vec![],
            terminator: Terminator::Call {
                callee: "mem$dealloc".to_string(),
                builtin: hew_types::runtime_call::RuntimeCallFamily::from_c_symbol("mem$dealloc"),
                args: vec![Place::Local(3), Place::Local(0), Place::Local(1)],
                dest: None,
                next: 6,
            },
        },
        BasicBlock {
            id: 6,
            statements: vec![],
            instructions: vec![Instr::ConstI64 {
                dest: Place::ReturnSlot,
                value: 0,
            }],
            terminator: Terminator::Return,
        },
    ];
    RawMirFunction {
        name: "main".to_string(),
        return_ty: ResolvedTy::I64,
        call_conv: FunctionCallConv::Default,
        params: vec![],
        locals,
        blocks,
        decisions: vec![],
        intrinsic_id: None,
        await_deadline_ns: std::collections::HashMap::new(),

        lambda_actor_user_param_locals: Vec::new(),
        span: None,
        instr_spans: ::std::collections::BTreeMap::new(),
    }
}

fn floor_exec_pipeline() -> IrPipeline {
    floor_pipeline_with_driver(driver_main())
}

/// `main() -> i64` that allocates two buffers and copies the leading 32 bytes
/// of the first into the second — and then **stops** (no `dealloc`), so the
/// backing buffers survive the JIT run and the test can read them back in Rust
/// space to prove the copy actually moved bytes (R2: `mem.ptr_copy` byte
/// semantics, not merely "did not crash"). The caller is responsible for
/// freeing the two leaked buffers via the `ALLOC_LOG` addresses.
///   p = alloc(64, 8); r = alloc(64, 8); ptr_copy(r, p, 32); return 0
fn driver_copy_no_free() -> RawMirFunction {
    // locals: 0=size(64) 1=align(8) 2=p 3=r 4=copy_count(32)
    let locals = vec![
        ResolvedTy::I64,
        ResolvedTy::I64,
        mut_u8_ptr(),
        mut_u8_ptr(),
        ResolvedTy::I64,
    ];
    let blocks = vec![
        BasicBlock {
            id: 0,
            statements: vec![],
            instructions: vec![
                Instr::ConstI64 {
                    dest: Place::Local(0),
                    value: 64,
                },
                Instr::ConstI64 {
                    dest: Place::Local(1),
                    value: 8,
                },
                Instr::ConstI64 {
                    dest: Place::Local(4),
                    value: 32,
                },
            ],
            terminator: Terminator::Call {
                callee: "mem$alloc".to_string(),
                builtin: hew_types::runtime_call::RuntimeCallFamily::from_c_symbol("mem$alloc"),
                args: vec![Place::Local(0), Place::Local(1)],
                dest: Some(Place::Local(2)),
                next: 1,
            },
        },
        BasicBlock {
            id: 1,
            statements: vec![],
            instructions: vec![],
            terminator: Terminator::Call {
                callee: "mem$alloc".to_string(),
                builtin: hew_types::runtime_call::RuntimeCallFamily::from_c_symbol("mem$alloc"),
                args: vec![Place::Local(0), Place::Local(1)],
                dest: Some(Place::Local(3)),
                next: 2,
            },
        },
        BasicBlock {
            id: 2,
            statements: vec![],
            instructions: vec![],
            terminator: Terminator::Call {
                callee: "mem$ptr_copy".to_string(),
                builtin: hew_types::runtime_call::RuntimeCallFamily::from_c_symbol("mem$ptr_copy"),
                // dst=r, src=p, byte_count=32
                args: vec![Place::Local(3), Place::Local(2), Place::Local(4)],
                dest: None,
                next: 3,
            },
        },
        BasicBlock {
            id: 3,
            statements: vec![],
            instructions: vec![Instr::ConstI64 {
                dest: Place::ReturnSlot,
                value: 0,
            }],
            terminator: Terminator::Return,
        },
    ];
    RawMirFunction {
        name: "main".to_string(),
        return_ty: ResolvedTy::I64,
        call_conv: FunctionCallConv::Default,
        params: vec![],
        locals,
        blocks,
        decisions: vec![],
        intrinsic_id: None,
        await_deadline_ns: std::collections::HashMap::new(),

        lambda_actor_user_param_locals: Vec::new(),
        span: None,
        instr_spans: ::std::collections::BTreeMap::new(),
    }
}

fn floor_pipeline_with_driver(driver: RawMirFunction) -> IrPipeline {
    IrPipeline {
        thir: vec![],
        raw_mir: vec![
            floor_fn(
                "mem$alloc",
                "mem.alloc",
                vec![ResolvedTy::U64, ResolvedTy::U64],
                mut_u8_ptr(),
            ),
            floor_fn(
                "mem$realloc",
                "mem.realloc",
                vec![
                    mut_u8_ptr(),
                    ResolvedTy::U64,
                    ResolvedTy::U64,
                    ResolvedTy::U64,
                ],
                mut_u8_ptr(),
            ),
            floor_fn(
                "mem$dealloc",
                "mem.dealloc",
                vec![mut_u8_ptr(), ResolvedTy::U64, ResolvedTy::U64],
                ResolvedTy::Unit,
            ),
            floor_fn(
                "mem$ptr_offset",
                "mem.ptr_offset",
                vec![mut_u8_ptr(), ResolvedTy::U64],
                mut_u8_ptr(),
            ),
            floor_fn(
                "mem$ptr_copy",
                "mem.ptr_copy",
                vec![mut_u8_ptr(), mut_u8_ptr(), ResolvedTy::U64],
                ResolvedTy::Unit,
            ),
            driver,
        ],
        // Empty checked/elaborated MIR: these functions carry no cooperate
        // sites and no drop plans, so the codegen loop's per-function lookup
        // resolves to `None` (the hand-built-pipeline fallback path).
        checked_mir: vec![] as Vec<CheckedMirFunction>,
        elaborated_mir: vec![] as Vec<ElaboratedMirFunction>,
        diagnostics: vec![],
        wire_layouts: std::sync::Arc::default(),
        opaque_handle_names: vec![],
        record_layouts: vec![],
        actor_layouts: vec![],
        supervisor_layouts: vec![],
        machine_layouts: vec![],
        enum_layouts: vec![],
        regex_literals: vec![],
        user_consts: Vec::new(),
        extern_decls: vec![],
        dyn_vtable_registry: vec![],
        hashmap_lowering_facts: vec![],
        hashset_lowering_facts: vec![],
        actor_send_aliasing: std::collections::HashMap::new(),
        polymorphic_mir: Vec::new(),
        user_clone_record_seeds: vec![],
    }
}

fn emit_ll(pipeline: &IrPipeline, module_name: &str) -> std::path::PathBuf {
    let tmp = std::env::temp_dir().join(format!("hew-mem-floor-exec-{module_name}"));
    std::fs::create_dir_all(&tmp).expect("create out_dir");
    let options = EmitOptions {
        module_name,
        out_dir: &tmp,
        native: false,
        wasm: false,
        target_triple: None,
        debug: false,
        source_path: None,
    };
    let artefacts =
        emit_module(pipeline, &options).expect("floor exec pipeline must emit successfully");
    artefacts
        .ll_path
        .expect("emit_module must populate ll_path")
}

/// JIT-execute `main`, mapping the runtime allocator externs to the recording
/// mocks. Returns `main`'s `i64` result.
fn jit_run_main(ll_path: &Path) -> i64 {
    Target::initialize_native(&InitializationConfig::default())
        .expect("native target initialisation must succeed");

    let ctx = Context::create();
    let buf = MemoryBuffer::create_from_file(ll_path)
        .unwrap_or_else(|e| panic!("read {}: {e}", ll_path.display()));
    let module = ctx
        .create_module_from_ir(buf)
        .unwrap_or_else(|e| panic!("parse IR {}: {e}", ll_path.display()));

    let alloc_fn = module.get_function("hew_alloc");
    let dealloc_fn = module.get_function("hew_dealloc");
    let realloc_fn = module.get_function("hew_realloc");

    let ee = module
        .create_jit_execution_engine(OptimizationLevel::None)
        .expect("create_jit_execution_engine must succeed");

    if let Some(f) = alloc_fn {
        ee.add_global_mapping(&f, mock_alloc as *const () as usize);
    } else {
        panic!("emitted module must declare hew_alloc — the mem.alloc body calls it");
    }
    if let Some(f) = dealloc_fn {
        ee.add_global_mapping(&f, mock_dealloc as *const () as usize);
    } else {
        panic!("emitted module must declare hew_dealloc — the mem.dealloc body calls it");
    }
    // `mem$realloc` is compiled (MCJIT is eager over the whole module) even
    // though `main` never calls it; its `hew_realloc` reference must resolve.
    if let Some(f) = realloc_fn {
        ee.add_global_mapping(&f, mock_realloc as *const () as usize);
    }

    // SAFETY: `main` is `fn() -> i64`.
    let jit_main = unsafe {
        ee.get_function::<unsafe extern "C" fn() -> i64>("main")
            .expect("main must be present in the JIT module")
    };
    unsafe { jit_main.call() }
}

/// The full alloc → copy → offset → dealloc round-trip must execute against
/// the real runtime allocator path, threading the allocator's pointer through
/// `main` into `dealloc`. This is the D343 fail-OPEN kill at execution level.
#[test]
#[ignore = "JIT/MCJIT execution deferred post-v0.5; native is the primary path (U26). Re-enable when the JIT runtime matures."]
fn mem_floor_round_trip_threads_real_runtime_pointer() {
    let _exec = EXEC_GUARD.lock().unwrap_or_else(|p| p.into_inner());
    ALLOC_LOG.lock().expect("ALLOC_LOG poisoned").clear();
    DEALLOC_LOG.lock().expect("DEALLOC_LOG poisoned").clear();

    let pipeline = floor_exec_pipeline();
    let ll = emit_ll(&pipeline, "round_trip");
    let rc = jit_run_main(&ll);

    assert_eq!(rc, 0, "main must return 0 after a clean round-trip");

    let allocs = ALLOC_LOG.lock().expect("ALLOC_LOG poisoned").clone();
    let deallocs = DEALLOC_LOG.lock().expect("DEALLOC_LOG poisoned").clone();

    assert_eq!(
        allocs.len(),
        2,
        "mem.alloc must invoke hew_alloc exactly twice; got {allocs:?}"
    );
    for (_, size, align) in &allocs {
        assert_eq!(
            (*size, *align),
            (64, 8),
            "alloc must forward the caller's (size, align); got ({size}, {align})"
        );
    }
    assert_eq!(
        deallocs.len(),
        2,
        "mem.dealloc must invoke hew_dealloc exactly twice; got {deallocs:?}"
    );

    // The D343 kill: the pointers `hew_alloc` returned must be exactly the
    // pointers `hew_dealloc` received (in the same order, p then r). An
    // empty-body no-op (the historic fail-OPEN bug) would have returned undef
    // from `mem.alloc`, so dealloc would NOT see the allocator's pointers.
    let alloc_ptrs: Vec<usize> = allocs.iter().map(|(p, _, _)| *p).collect();
    let dealloc_ptrs: Vec<usize> = deallocs.iter().map(|(p, _, _)| *p).collect();
    assert_eq!(
        alloc_ptrs, dealloc_ptrs,
        "the runtime allocator's real pointers must thread alloc -> main -> dealloc \
         (D343 fail-OPEN regression); alloc returned {alloc_ptrs:?}, dealloc saw {dealloc_ptrs:?}"
    );
    for (_, size, align) in &deallocs {
        assert_eq!(
            (*size, *align),
            (64, 8),
            "dealloc must forward the matching (size, align); got ({size}, {align})"
        );
    }
}

/// `mem.ptr_copy` must actually move bytes — not merely execute without
/// crashing. The two buffers are stamped with distinct fills by `mock_alloc`
/// (`p`=0xA0, `r`=0xA1); after `ptr_copy(r, p, 32)` the destination's leading
/// 32 bytes must read back as the *source* fill (0xA0) while its tail (bytes
/// 32..64) keeps its own fill (0xA1), and the source must be unchanged. The
/// driver omits `dealloc` so the buffers survive the JIT run; the test frees
/// them by hand after reading.
#[test]
#[ignore = "JIT/MCJIT execution deferred post-v0.5; native is the primary path (U26). Re-enable when the JIT runtime matures."]
fn mem_floor_ptr_copy_moves_bytes() {
    let _exec = EXEC_GUARD.lock().unwrap_or_else(|p| p.into_inner());
    ALLOC_LOG.lock().expect("ALLOC_LOG poisoned").clear();
    DEALLOC_LOG.lock().expect("DEALLOC_LOG poisoned").clear();

    let pipeline = floor_pipeline_with_driver(driver_copy_no_free());
    let ll = emit_ll(&pipeline, "ptr_copy_bytes");
    let rc = jit_run_main(&ll);
    assert_eq!(rc, 0, "copy-only main must return 0");

    let allocs = ALLOC_LOG.lock().expect("ALLOC_LOG poisoned").clone();
    assert_eq!(
        allocs.len(),
        2,
        "copy-only driver must allocate exactly two buffers; got {allocs:?}"
    );
    // alloc order in the driver: p (src, fill 0xA0) then r (dst, fill 0xA1).
    let (src_addr, _, _) = allocs[0];
    let (dst_addr, _, _) = allocs[1];
    let src = src_addr as *const u8;
    let dst = dst_addr as *const u8;

    // SAFETY: both buffers are live (the driver did not free them) and each
    // owns 64 writable/readable bytes from `mock_alloc`'s `(64, 8)` layout.
    let (src_bytes, dst_bytes) = unsafe {
        (
            std::slice::from_raw_parts(src, 64),
            std::slice::from_raw_parts(dst, 64),
        )
    };

    assert!(
        dst_bytes[..32].iter().all(|&b| b == 0xA0),
        "ptr_copy must overwrite dst[0..32] with the source fill 0xA0; got {:?}",
        &dst_bytes[..32]
    );
    assert!(
        dst_bytes[32..].iter().all(|&b| b == 0xA1),
        "ptr_copy must leave dst[32..64] (beyond byte_count) at its own fill 0xA1; got {:?}",
        &dst_bytes[32..]
    );
    assert!(
        src_bytes.iter().all(|&b| b == 0xA0),
        "ptr_copy must not mutate the source buffer; got {:?}",
        src_bytes
    );

    // Hand-free the two leaked buffers (the driver intentionally skipped
    // dealloc so the bytes stayed readable above).
    let layout = Layout::from_size_align(64, 8).expect("valid layout");
    // SAFETY: both came from `mock_alloc`/`std::alloc::alloc` with this layout.
    unsafe {
        dealloc(src_addr as *mut u8, layout);
        dealloc(dst_addr as *mut u8, layout);
    }
}

// ── WASM parity ────────────────────────────────────────────────────────────

/// The memory floor must be WASM-parity-clean: the runtime allocator
/// (`hew-runtime/src/mem.rs`) is NOT gated out on wasm32, and codegen lowers
/// the five floor bodies identically for the wasm32 target (the same
/// `FloorIntrinsic` emitter — `call @hew_alloc`, i8 GEP, `@llvm.memcpy`).
/// So a wasm emission of the floor pipeline must NOT be rejected as a
/// native-only substrate.
///
/// This assertion runs the codegen-front substrate gate (which fires *before*
/// any `wasm-ld` invocation). It tolerates a link-step failure when the wasm
/// toolchain is absent — that is an environment gap, not a parity gap — but
/// fails loudly if the floor is ever (incorrectly) classified as
/// wasm-unsupported.
#[test]
fn mem_floor_is_not_a_wasm_excluded_substrate() {
    let pipeline = floor_exec_pipeline();
    let tmp = std::env::temp_dir().join("hew-mem-floor-wasm-gate");
    std::fs::create_dir_all(&tmp).expect("create out_dir");
    let options = EmitOptions {
        module_name: "mem_floor_wasm_gate",
        out_dir: &tmp,
        native: false,
        wasm: true,
        target_triple: None,
        debug: false,
        source_path: None,
    };
    if let Err(CodegenError::WasmUnsupportedSubstrate { symbol }) = emit_module(&pipeline, &options)
    {
        panic!(
            "the memory floor must not be a wasm-excluded substrate — the runtime \
             allocator compiles for wasm32 and the bodies lower identically; \
             got WasmUnsupportedSubstrate({symbol})"
        );
    }
}

/// Full WASM artefact emission for the floor pipeline: lowers the bodies for
/// `wasm32-unknown-unknown`, runs `wasm-ld --no-entry --export=main`, and
/// asserts a standalone `.wasm` is produced with the runtime allocator left as
/// an import (the wasm runtime supplies `hew_alloc`/`hew_dealloc` the same way
/// the native build links them).
///
/// Requires the wasm toolchain (`wasm-ld`/`rust-lld`); verified passing on
/// this host (wasmtime + wasm32-wasip1 present). The active
/// `mem_floor_is_not_a_wasm_excluded_substrate` test guards the parity
/// contract on every run; this one is the link-level proof.
#[test]
fn mem_floor_emits_linkable_wasm_module() {
    let pipeline = floor_exec_pipeline();
    let tmp = std::env::temp_dir().join("hew-mem-floor-wasm-emit");
    std::fs::create_dir_all(&tmp).expect("create out_dir");
    let options = EmitOptions {
        module_name: "mem_floor_wasm_emit",
        out_dir: &tmp,
        native: false,
        wasm: true,
        target_triple: None,
        debug: false,
        source_path: None,
    };
    let artefacts = emit_module(&pipeline, &options)
        .expect("floor pipeline must emit a wasm module (wasm toolchain required)");
    let wasm_path = artefacts
        .wasm_path
        .expect("wasm_path must be populated when wasm: true");
    assert!(
        wasm_path.exists(),
        "the linked .wasm artefact must exist at {}",
        wasm_path.display()
    );
    let bytes = std::fs::read(&wasm_path).expect("read .wasm artefact");
    assert!(
        bytes.starts_with(b"\0asm"),
        "the emitted artefact must be a wasm module (magic \\0asm); got {:?}",
        &bytes[..bytes.len().min(8)]
    );
    // The runtime allocator symbol is left as a wasm import (the wasm runtime
    // supplies it). Its name survives in the import section as a byte string.
    assert!(
        bytes.windows(b"hew_alloc".len()).any(|w| w == b"hew_alloc"),
        "the wasm module must reference the runtime allocator import `hew_alloc`"
    );
}
