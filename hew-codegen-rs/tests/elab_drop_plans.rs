//! Coverage for the elaborated-drop-plan codegen path introduced when
//! `lower_function` was wired to consume `elaborated_mir.drop_plans`.
//!
//! Every test here uses a pipeline where `raw_mir.blocks` carries NO
//! `Instr::Drop` instruction — the close calls must therefore originate
//! solely from `emit_elab_drops`, proving the new path fires end-to-end.
//!
//! Covered paths:
//! 1. `"Duplex::close"` resolves to `hew_duplex_close` and emits the
//!    close call + alloca null-store for each `ElabDrop` in the plan.
//! 2. A pipeline with elaborator-string drops in `drop_plans` blocks
//!    WASM emission via the extended `uses_wasm_excluded_symbol` scan.
//! 3. A pipeline with an unknown `drop_fn` string surfaces
//!    `CodegenError::FailClosed` — boundary-fail-closed (P0).
//!
//! LESSONS: cleanup-all-exits (P0), lifecycle-symmetry (P0),
//! boundary-fail-closed (P0), end-to-end-before-layer-thickening (P1).

use hew_codegen_rs::{emit_module, CodegenError, EmitOptions};
use hew_mir::{
    BasicBlock, BlockKind, CheckedMirFunction, DropKind, DropPlan, ElabBlock, ElabDrop,
    ElaboratedMirFunction, ExitPath, Instr, IrPipeline, Place, RawMirFunction, Terminator,
};
use hew_types::ResolvedTy;

fn duplex_ty() -> ResolvedTy {
    ResolvedTy::Named {
        name: "Duplex".to_string(),
        args: vec![],
        builtin: None,
        is_opaque: false,
    }
}

/// Build a pipeline whose close calls arrive exclusively via
/// `elaborated_mir.drop_plans` — no `Instr::Drop` in raw_mir.
///
/// Locals layout:
/// - 0: i64 (return value)
/// - 1: Duplex handle A
///
/// The drop plan for the single Return exit carries one `ElabDrop`
/// for handle A with `drop_fn = Some("Duplex::close")` and
/// `kind = DropKind::DuplexClose`, mirroring what the elaborator emits
/// for `@resource`-typed Duplex bindings.
fn pipeline_with_elab_drop_plan() -> IrPipeline {
    let raw_blocks = vec![BasicBlock {
        id: 0,
        statements: vec![],
        instructions: vec![
            // Return 0 — no Instr::Drop anywhere in the instruction stream.
            Instr::ConstI64 {
                dest: Place::ReturnSlot,
                value: 0,
            },
        ],
        terminator: Terminator::Return,
    }];
    let drop_plan = DropPlan {
        drops: vec![ElabDrop {
            place: Place::DuplexHandle(1),
            ty: duplex_ty(),
            drop_fn: Some(hew_mir::DropFnSpec::Runtime(
                hew_types::runtime_call::RuntimeDropDescriptor::DuplexClose,
            )),
            kind: DropKind::DuplexClose,
        }],
    };
    IrPipeline {
        thir: vec![],
        raw_mir: vec![RawMirFunction {
            name: "main".to_string(),
            return_ty: ResolvedTy::I64,
            call_conv: hew_mir::FunctionCallConv::Default,
            params: vec![],
            locals: vec![
                ResolvedTy::I64, // 0 return value
                duplex_ty(),     // 1 Duplex handle A
            ],
            blocks: raw_blocks.clone(),
            decisions: vec![],
            intrinsic_id: None,
            await_deadline_ns: std::collections::HashMap::new(),

            lambda_actor_user_param_locals: Vec::new(),
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
            drop_plans: vec![(ExitPath::Return { block: 0 }, drop_plan)],
            coroutine: None,
            lambda_captures: vec![],
        }],
        diagnostics: vec![],
        opaque_handle_names: vec![],
        record_layouts: vec![],
        actor_layouts: vec![],
        supervisor_layouts: vec![],
        machine_layouts: vec![],
        enum_layouts: vec![],
        regex_literals: vec![],
        user_consts: Vec::new(),
        gen_state_layouts: vec![],
        extern_decls: vec![],
        dyn_vtable_registry: vec![],
        hashmap_lowering_facts: vec![],
        hashset_lowering_facts: vec![],
        actor_send_aliasing: std::collections::HashMap::new(),
        polymorphic_mir: Vec::new(),
    }
}

fn out_dir(name: &str) -> std::path::PathBuf {
    let d = std::env::temp_dir().join(format!("hew-elab-drop-{name}"));
    std::fs::create_dir_all(&d).expect("create out_dir");
    d
}

fn emit_ll(pipeline: &IrPipeline, module_name: &str) -> String {
    let dir = out_dir(module_name);
    let options = EmitOptions {
        module_name,
        out_dir: &dir,
        native: false,
        wasm: false,
        target_triple: None,
    };
    let artefacts =
        emit_module(pipeline, &options).expect("elab drop plan pipeline must emit successfully");
    let ll_path = artefacts.ll_path.expect("ll_path must be populated");
    std::fs::read_to_string(&ll_path).expect("read emitted .ll")
}

/// `"Duplex::close"` in `drop_plans` (no `Instr::Drop`) must produce a
/// `call i32 @hew_duplex_close` in the emitted IR. This proves the new
/// `emit_elab_drops` path fires and resolves the elaborator-produced
/// string to the C-ABI symbol name.
#[test]
fn elab_drop_plan_duplex_close_emits_close_call() {
    let pipeline = pipeline_with_elab_drop_plan();
    let ll = emit_ll(&pipeline, "elab-drop-close-call");
    assert!(
        ll.contains("call i32 @hew_duplex_close"),
        "drop_plans Duplex::close must emit hew_duplex_close call via \
         emit_elab_drops; raw_mir has no Instr::Drop to confirm the \
         new path fired. Got:\n{ll}"
    );
}

/// After `hew_duplex_close` fires from `drop_plans`, the alloca must be
/// zeroed (`store ptr null`) — defence-in-depth for LIFO double-drop
/// per LESSONS `raii-null-after-move`. Confirmed independent of the
/// `Instr::Drop` path that the existing `emit_duplex_pair.rs` covers.
#[test]
fn elab_drop_plan_duplex_close_zeroes_alloca() {
    let pipeline = pipeline_with_elab_drop_plan();
    let ll = emit_ll(&pipeline, "elab-drop-null-store");
    assert!(
        ll.contains("store ptr null"),
        "emit_elab_drops must store ptr null after each close call \
         (LESSONS raii-null-after-move); got:\n{ll}"
    );
}

/// A pipeline whose `drop_plans` carries `"Duplex::close"` must block
/// WASM emission — the extended `uses_wasm_excluded_symbol` scan over
/// `elaborated_mir.drop_plans` must catch the duplex close string even
/// when no `hew_duplex_*` literal appears anywhere in `raw_mir`.
#[test]
fn elab_drop_plan_duplex_close_blocks_wasm_emission() {
    let pipeline = pipeline_with_elab_drop_plan();
    let dir = out_dir("elab-drop-wasm-block");
    let options = EmitOptions {
        module_name: "elab_drop_wasm",
        out_dir: &dir,
        native: false,
        wasm: true,
        target_triple: None,
    };
    let result = emit_module(&pipeline, &options);
    match result {
        Err(CodegenError::WasmUnsupportedSubstrate { symbol }) => {
            assert!(
                symbol.starts_with("hew_duplex_"),
                "WasmUnsupportedSubstrate symbol must be a duplex C-ABI \
                 symbol (resolved from Duplex::close); got: {symbol}"
            );
        }
        Ok(_) => panic!(
            "expected WasmUnsupportedSubstrate for pipeline with \
             Duplex::close in drop_plans and wasm: true, but got Ok"
        ),
        Err(other) => panic!("expected WasmUnsupportedSubstrate, got a different error: {other}"),
    }
}

/// A drop plan carrying an unknown `drop_fn` string surfaces
/// `CodegenError::FailClosed` naming the string — boundary-fail-closed
/// (P0). No wildcard, no silent no-op of a resource close.
#[test]
fn elab_drop_plan_unknown_drop_fn_fails_closed() {
    let unknown_drop_fn = "SomeUnwiredType::close";
    let raw_blocks = vec![BasicBlock {
        id: 0,
        statements: vec![],
        instructions: vec![Instr::ConstI64 {
            dest: Place::ReturnSlot,
            value: 0,
        }],
        terminator: Terminator::Return,
    }];
    let drop_plan = DropPlan {
        drops: vec![ElabDrop {
            place: Place::Local(0),
            ty: ResolvedTy::I64,
            drop_fn: Some(hew_mir::DropFnSpec::UserClose(unknown_drop_fn.to_string())),
            kind: DropKind::Resource,
        }],
    };
    let pipeline = IrPipeline {
        thir: vec![],
        raw_mir: vec![RawMirFunction {
            name: "main".to_string(),
            return_ty: ResolvedTy::I64,
            call_conv: hew_mir::FunctionCallConv::Default,
            params: vec![],
            locals: vec![ResolvedTy::I64],
            blocks: raw_blocks.clone(),
            decisions: vec![],
            intrinsic_id: None,
            await_deadline_ns: std::collections::HashMap::new(),

            lambda_actor_user_param_locals: Vec::new(),
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
            drop_plans: vec![(ExitPath::Return { block: 0 }, drop_plan)],
            coroutine: None,
            lambda_captures: vec![],
        }],
        diagnostics: vec![],
        opaque_handle_names: vec![],
        record_layouts: vec![],
        actor_layouts: vec![],
        supervisor_layouts: vec![],
        machine_layouts: vec![],
        enum_layouts: vec![],
        regex_literals: vec![],
        user_consts: Vec::new(),
        gen_state_layouts: vec![],
        extern_decls: vec![],
        dyn_vtable_registry: vec![],
        hashmap_lowering_facts: vec![],
        hashset_lowering_facts: vec![],
        actor_send_aliasing: std::collections::HashMap::new(),
        polymorphic_mir: Vec::new(),
    };
    let dir = out_dir("elab-drop-unknown-fail-closed");
    let options = EmitOptions {
        module_name: "elab_drop_unknown",
        out_dir: &dir,
        native: false,
        wasm: false,
        target_triple: None,
    };
    let result = emit_module(&pipeline, &options);
    match result {
        Err(CodegenError::FailClosed(msg)) => {
            assert!(
                msg.contains(unknown_drop_fn),
                "FailClosed message must name the unknown drop_fn string \
                 so the rejection points at the unwired seam; got: {msg}"
            );
        }
        Err(other) => {
            panic!("expected CodegenError::FailClosed for unknown drop_fn, got {other:?}")
        }
        Ok(_) => panic!(
            "expected codegen to fail closed on unknown drop_fn \
             in elaborated drop plan; got Ok(_)"
        ),
    }
}
