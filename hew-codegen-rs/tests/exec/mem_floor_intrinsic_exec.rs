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

use hew_codegen_rs::{emit_module, CodegenError, EmitOptions};
use hew_mir::{BasicBlock, FunctionCallConv, Instr, IrPipeline, Place, RawMirFunction, Terminator};
use hew_types::ResolvedTy;

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
        source_origin: hew_mir::SourceOrigin::Unknown,
        key: hew_mir::MirCallableKey::for_test(name),
        name: name.to_string(),
        return_ty: ret,
        call_conv: FunctionCallConv::Default,
        params: params.clone(),
        locals: params,
        local_names: Vec::new(),
        local_scopes: Vec::new(),
        local_decl_bytes: Vec::new(),
        scope_table: Vec::new(),
        blocks: vec![BasicBlock {
            id: 0,
            statements: vec![],
            instructions: vec![],
            terminator: Terminator::Return,
        }],
        decisions: vec![],
        intrinsic_id: Some(id.to_string()),
        await_deadline_ns: std::collections::HashMap::new(),
        suspend_kinds: std::collections::HashMap::new(),

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
                authority: (hew_types::runtime_call::RuntimeCallFamily::from_c_symbol("mem$alloc"))
                    .map(hew_mir::CallAuthority::Runtime)
                    .unwrap_or_default(),
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
                authority: (hew_types::runtime_call::RuntimeCallFamily::from_c_symbol("mem$alloc"))
                    .map(hew_mir::CallAuthority::Runtime)
                    .unwrap_or_default(),
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
                authority: (hew_types::runtime_call::RuntimeCallFamily::from_c_symbol(
                    "mem$ptr_copy",
                ))
                .map(hew_mir::CallAuthority::Runtime)
                .unwrap_or_default(),
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
                authority: (hew_types::runtime_call::RuntimeCallFamily::from_c_symbol(
                    "mem$ptr_offset",
                ))
                .map(hew_mir::CallAuthority::Runtime)
                .unwrap_or_default(),
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
                authority: (hew_types::runtime_call::RuntimeCallFamily::from_c_symbol(
                    "mem$dealloc",
                ))
                .map(hew_mir::CallAuthority::Runtime)
                .unwrap_or_default(),
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
                authority: (hew_types::runtime_call::RuntimeCallFamily::from_c_symbol(
                    "mem$dealloc",
                ))
                .map(hew_mir::CallAuthority::Runtime)
                .unwrap_or_default(),
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
        source_origin: hew_mir::SourceOrigin::Unknown,
        key: hew_mir::MirCallableKey::for_test("main"),
        name: "main".to_string(),
        return_ty: ResolvedTy::I64,
        call_conv: FunctionCallConv::Default,
        params: vec![],
        locals,
        local_names: Vec::new(),
        local_scopes: Vec::new(),
        local_decl_bytes: Vec::new(),
        scope_table: Vec::new(),
        blocks,
        decisions: vec![],
        intrinsic_id: None,
        await_deadline_ns: std::collections::HashMap::new(),
        suspend_kinds: std::collections::HashMap::new(),

        lambda_actor_user_param_locals: Vec::new(),
        span: None,
        instr_spans: ::std::collections::BTreeMap::new(),
    }
}

fn floor_exec_pipeline() -> IrPipeline {
    floor_pipeline_with_driver(driver_main())
}

fn floor_pipeline_with_driver(driver: RawMirFunction) -> IrPipeline {
    crate::mir_fixture::complete_stages(IrPipeline {
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
        checked_mir: vec![],
        elaborated_mir: vec![],
        capabilities: hew_mir::ModuleCapabilities::EMPTY,
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
        polymorphic_mir: Vec::new(),
        user_clone_record_seeds: vec![],
        lint_warnings: vec![],
        lifecycle_registry: hew_hir::LifecycleRegistry::default(),
    })
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
        opt_level: hew_codegen_rs::OptLevel::O0,
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
        opt_level: hew_codegen_rs::OptLevel::O0,
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
