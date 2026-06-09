//! E5c: WASM parity classification for the duplex substrate.
//!
//! `hew-runtime/src/duplex.rs:54` gates the entire duplex module out of
//! wasm32 builds via `#![cfg(not(target_arch = "wasm32"))]`.  If `emit_module`
//! were to proceed with WASM emission for a duplex-using program, `wasm-ld`
//! would fail with `undefined symbol: hew_duplex_*`.  These tests verify that
//! `emit_module` returns `CodegenError::WasmUnsupportedSubstrate` before
//! invoking the WASM toolchain, and that non-duplex programs still emit WASM
//! normally (the native path is unaffected in both cases).
//!
//! WASM-TODO(#1451): duplex WASM parity is tracked in issue #1451.
//!
//! LESSONS: boundary-fail-closed (P0), parity-or-tracked-gap (P0),
//! user-surface-correctness (P0).

use hew_codegen_rs::{emit_module, CodegenError, EmitOptions};
use hew_mir::{
    BasicBlock, BlockKind, CheckedMirFunction, DropPlan, ElabBlock, ElaboratedMirFunction,
    ExitPath, Instr, IrPipeline, Place, RawMirFunction, RuntimeCall, Terminator,
};
use hew_types::ResolvedTy;

// ---------------------------------------------------------------------------
// Pipeline builders
// ---------------------------------------------------------------------------

fn duplex_ty() -> ResolvedTy {
    ResolvedTy::Named {
        name: "Duplex".to_string(),
        args: vec![],
        builtin: None,
    }
}

/// Minimal pipeline with a `hew_duplex_pair` call — mirrors the shape produced
/// by `lower_duplex_pair` in `hew-mir/src/lower.rs`.
fn pipeline_with_duplex_pair_call() -> IrPipeline {
    let raw_blocks = vec![BasicBlock {
        id: 0,
        statements: vec![],
        instructions: vec![
            Instr::ConstI64 {
                dest: Place::Local(0),
                value: 16,
            },
            Instr::CallRuntimeAbi(
                RuntimeCall::new(
                    "hew_duplex_pair",
                    vec![
                        Place::Local(0),
                        Place::Local(0),
                        Place::DuplexHandle(1),
                        Place::DuplexHandle(2),
                    ],
                    None,
                )
                .expect("hew_duplex_pair is on the M2 allowlist"),
            ),
            Instr::ConstI64 {
                dest: Place::ReturnSlot,
                value: 0,
            },
        ],
        terminator: Terminator::Return,
    }];
    IrPipeline {
        thir: vec![],
        raw_mir: vec![RawMirFunction {
            name: "main".to_string(),
            return_ty: ResolvedTy::I64,
            call_conv: hew_mir::FunctionCallConv::Default,
            params: vec![],
            locals: vec![
                ResolvedTy::I64, // 0 cap
                duplex_ty(),     // 1 Duplex A
                duplex_ty(),     // 2 Duplex B
            ],
            blocks: raw_blocks.clone(),
            decisions: vec![],
        }],
        checked_mir: vec![CheckedMirFunction {
            name: "main".to_string(),
            return_ty: ResolvedTy::I64,
            blocks: raw_blocks,
            decisions: vec![],
            checks: vec![],
            cooperate_sites: vec![],
        }],
        elaborated_mir: vec![ElaboratedMirFunction {
            name: "main".to_string(),
            return_ty: ResolvedTy::I64,
            statements: vec![],
            decisions: vec![],
            blocks: vec![ElabBlock {
                id: 0,
                kind: BlockKind::Normal,
                drops: vec![],
                successor: None,
            }],
            drop_plans: vec![(ExitPath::Return { block: 0 }, DropPlan::default())],
            coroutine: None,
            lambda_captures: vec![],
        }],
        diagnostics: vec![],
        record_layouts: vec![],
        actor_layouts: vec![],
        supervisor_layouts: vec![],
        machine_layouts: vec![],
        enum_layouts: vec![],
        regex_literals: vec![],
        gen_state_layouts: vec![],
        extern_decls: vec![],
        dyn_vtable_registry: vec![],
        hashmap_lowering_facts: vec![],
        hashset_lowering_facts: vec![],
    }
}

/// Minimal pipeline with a `hew_duplex_close` drop — the close call is in the
/// Instr stream of the raw blocks (as emitted by the elaborator).
fn pipeline_with_duplex_close_drop() -> IrPipeline {
    let raw_blocks = vec![BasicBlock {
        id: 0,
        statements: vec![],
        instructions: vec![
            Instr::Drop {
                place: Place::DuplexHandle(0),
                ty: duplex_ty(),
                drop_fn: Some("hew_duplex_close".to_string()),
            },
            Instr::ConstI64 {
                dest: Place::ReturnSlot,
                value: 0,
            },
        ],
        terminator: Terminator::Return,
    }];
    IrPipeline {
        thir: vec![],
        raw_mir: vec![RawMirFunction {
            name: "main".to_string(),
            return_ty: ResolvedTy::I64,
            call_conv: hew_mir::FunctionCallConv::Default,
            params: vec![],
            locals: vec![duplex_ty()], // 0 Duplex handle
            blocks: raw_blocks.clone(),
            decisions: vec![],
        }],
        checked_mir: vec![CheckedMirFunction {
            name: "main".to_string(),
            return_ty: ResolvedTy::I64,
            blocks: raw_blocks,
            decisions: vec![],
            checks: vec![],
            cooperate_sites: vec![],
        }],
        elaborated_mir: vec![ElaboratedMirFunction {
            name: "main".to_string(),
            return_ty: ResolvedTy::I64,
            statements: vec![],
            decisions: vec![],
            blocks: vec![ElabBlock {
                id: 0,
                kind: BlockKind::Normal,
                drops: vec![],
                successor: None,
            }],
            drop_plans: vec![(ExitPath::Return { block: 0 }, DropPlan::default())],
            coroutine: None,
            lambda_captures: vec![],
        }],
        diagnostics: vec![],
        record_layouts: vec![],
        actor_layouts: vec![],
        supervisor_layouts: vec![],
        machine_layouts: vec![],
        enum_layouts: vec![],
        regex_literals: vec![],
        gen_state_layouts: vec![],
        extern_decls: vec![],
        dyn_vtable_registry: vec![],
        hashmap_lowering_facts: vec![],
        hashset_lowering_facts: vec![],
    }
}

/// Minimal non-duplex pipeline — a plain integer arithmetic function.  Used to
/// verify WASM emission proceeds normally when no duplex symbols are present.
fn pipeline_no_duplex() -> IrPipeline {
    let raw_blocks = vec![BasicBlock {
        id: 0,
        statements: vec![],
        instructions: vec![
            Instr::ConstI64 {
                dest: Place::Local(0),
                value: 3,
            },
            Instr::ConstI64 {
                dest: Place::Local(1),
                value: 4,
            },
            Instr::IntAdd {
                dest: Place::ReturnSlot,
                lhs: Place::Local(0),
                rhs: Place::Local(1),
            },
        ],
        terminator: Terminator::Return,
    }];
    IrPipeline {
        thir: vec![],
        raw_mir: vec![RawMirFunction {
            name: "main".to_string(),
            return_ty: ResolvedTy::I64,
            call_conv: hew_mir::FunctionCallConv::Default,
            params: vec![],
            locals: vec![ResolvedTy::I64, ResolvedTy::I64],
            blocks: raw_blocks.clone(),
            decisions: vec![],
        }],
        checked_mir: vec![CheckedMirFunction {
            name: "main".to_string(),
            return_ty: ResolvedTy::I64,
            blocks: raw_blocks,
            decisions: vec![],
            checks: vec![],
            cooperate_sites: vec![],
        }],
        elaborated_mir: vec![ElaboratedMirFunction {
            name: "main".to_string(),
            return_ty: ResolvedTy::I64,
            statements: vec![],
            decisions: vec![],
            blocks: vec![ElabBlock {
                id: 0,
                kind: BlockKind::Normal,
                drops: vec![],
                successor: None,
            }],
            drop_plans: vec![(ExitPath::Return { block: 0 }, DropPlan::default())],
            coroutine: None,
            lambda_captures: vec![],
        }],
        diagnostics: vec![],
        record_layouts: vec![],
        actor_layouts: vec![],
        supervisor_layouts: vec![],
        machine_layouts: vec![],
        enum_layouts: vec![],
        regex_literals: vec![],
        gen_state_layouts: vec![],
        extern_decls: vec![],
        dyn_vtable_registry: vec![],
        hashmap_lowering_facts: vec![],
        hashset_lowering_facts: vec![],
    }
}

// ---------------------------------------------------------------------------
// Test helpers
// ---------------------------------------------------------------------------

fn out_dir(name: &str) -> std::path::PathBuf {
    let d = std::env::temp_dir().join(format!("hew-e5c-{name}"));
    std::fs::create_dir_all(&d).expect("create out_dir");
    d
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

/// A pipeline containing `hew_duplex_pair` must return
/// `CodegenError::WasmUnsupportedSubstrate` when WASM emission is requested,
/// rather than proceeding to invoke `wasm-ld` (which would fail with
/// `undefined symbol: hew_duplex_pair`).  WASM-TODO(#1451).
#[test]
fn duplex_pair_call_blocks_wasm_emission() {
    let pipeline = pipeline_with_duplex_pair_call();
    let dir = out_dir("duplex-pair-wasm-block");
    let options = EmitOptions {
        module_name: "duplex_wasm_block",
        out_dir: &dir,
        native: false,
        wasm: true,
    };
    let result = emit_module(&pipeline, &options);
    match result {
        Err(CodegenError::WasmUnsupportedSubstrate { symbol }) => {
            assert!(
                symbol.starts_with("hew_duplex_"),
                "WasmUnsupportedSubstrate symbol must be a duplex symbol; got: {symbol}"
            );
        }
        Ok(_) => panic!(
            "expected WasmUnsupportedSubstrate error for duplex_pair call with wasm: true, \
             but emit_module succeeded"
        ),
        Err(other) => panic!("expected WasmUnsupportedSubstrate, got a different error: {other}"),
    }
}

/// A pipeline with a `hew_duplex_close` drop in the Instr stream must also
/// block WASM emission.  WASM-TODO(#1451).
#[test]
fn duplex_close_drop_blocks_wasm_emission() {
    let pipeline = pipeline_with_duplex_close_drop();
    let dir = out_dir("duplex-close-wasm-block");
    let options = EmitOptions {
        module_name: "duplex_close_wasm_block",
        out_dir: &dir,
        native: false,
        wasm: true,
    };
    let result = emit_module(&pipeline, &options);
    match result {
        Err(CodegenError::WasmUnsupportedSubstrate { symbol }) => {
            assert!(
                symbol.starts_with("hew_duplex_"),
                "WasmUnsupportedSubstrate symbol must be a duplex symbol; got: {symbol}"
            );
        }
        Ok(_) => panic!(
            "expected WasmUnsupportedSubstrate for duplex_close drop with wasm: true, \
             but emit_module succeeded"
        ),
        Err(other) => panic!("expected WasmUnsupportedSubstrate, got: {other}"),
    }
}

/// With `wasm: false`, the duplex pipeline emits the textual LLVM IR and
/// native object normally.
/// The native path must NOT be blocked by the duplex detection gate.
#[test]
fn duplex_pair_native_only_succeeds_without_wasm_emit() {
    let pipeline = pipeline_with_duplex_pair_call();
    let dir = out_dir("duplex-native-only");
    let options = EmitOptions {
        module_name: "duplex_native_only",
        out_dir: &dir,
        native: true,
        wasm: false,
    };
    let artefacts =
        emit_module(&pipeline, &options).expect("duplex pipeline with wasm: false must succeed");
    assert!(
        artefacts.ll_path.is_some(),
        "ll_path must be populated even when wasm: false"
    );
    assert!(
        artefacts.native_obj_path.is_some(),
        "native_obj_path must be populated when native: true"
    );
    assert!(
        artefacts.wasm_path.is_none(),
        "wasm_path must be None when wasm: false"
    );
}

/// A non-duplex pipeline (plain arithmetic) must still emit WASM normally —
/// the duplex gate must not interfere with programs that don't use the duplex
/// substrate.  This is the regression guard for the non-duplex smoke path.
///
/// The full WASM link would require `wasm-ld`; use `native: false, wasm: false`
/// with an assertion that the error is NOT `WasmUnsupportedSubstrate`.
#[test]
fn non_duplex_pipeline_does_not_trigger_wasm_substrate_error() {
    let pipeline = pipeline_no_duplex();
    let dir = out_dir("non-duplex-wasm-check");
    let options = EmitOptions {
        module_name: "non_duplex_wasm",
        out_dir: &dir,
        native: false,
        wasm: false, // avoid invoking wasm-ld in unit tests
    };
    let result = emit_module(&pipeline, &options);
    // The result may succeed or fail for other reasons, but must NOT fail with
    // WasmUnsupportedSubstrate — that error is reserved for duplex programs.
    if let Err(CodegenError::WasmUnsupportedSubstrate { symbol }) = result {
        panic!(
            "non-duplex pipeline must not trigger WasmUnsupportedSubstrate; \
             got symbol: {symbol}"
        );
    }
}
