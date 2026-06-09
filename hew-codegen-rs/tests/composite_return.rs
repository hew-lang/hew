//! Substrate-level tests for composite-return spine (W3.028).
//!
//! Verifies that user functions returning `Option<i64>` and `Result<i64, i64>`
//! produce valid LLVM IR through the aggregate Move-to-ReturnSlot path, and
//! that heap-owning payloads (`Option<string>`) trigger the fail-closed
//! boundary diagnostic.
//!
//! Also exercises the generic-enum layout walk: `Envelope<i64>` where a
//! separate variant `Message(string)` carries a heap-owning field unrelated
//! to the bitcopy type argument `i64`.  That case must also fail closed.

use hew_codegen_rs::{emit_module, CodegenError, EmitOptions};
use hew_mir::{
    BasicBlock, EnumLayout, FunctionCallConv, Instr, IrPipeline, MachineVariantLayout, Place,
    RawMirFunction, Terminator,
};
use hew_types::ResolvedTy;

fn emit_ll(pipeline: &IrPipeline, module_name: &str) -> String {
    let tmp = std::env::temp_dir().join(format!("hew-composite-return-{module_name}"));
    std::fs::create_dir_all(&tmp).expect("create scratch dir");
    let options = EmitOptions {
        module_name,
        out_dir: &tmp,
        native: false,
        wasm: false,
    };
    let artefacts = emit_module(pipeline, &options)
        .expect("emit_module must succeed for composite-return fixture");
    let ll_path = artefacts
        .ll_path
        .expect("emit_module must produce an .ll when native=false");
    std::fs::read_to_string(&ll_path)
        .unwrap_or_else(|e| panic!("could not read {}: {e}", ll_path.display()))
}

/// Build a pipeline with `fn maybe() -> Option<i64> { Some(42) }`.
fn option_some_pipeline() -> IrPipeline {
    let option_ty = ResolvedTy::Named {
        name: "Option".to_string(),
        args: vec![ResolvedTy::I64],
        builtin: None,
    };
    let enum_layout = EnumLayout {
        name: "Option$$i64".to_string(),
        tag_width: 1,
        variants: vec![
            MachineVariantLayout {
                name: "Some".to_string(),
                field_tys: vec![ResolvedTy::I64],
            },
            MachineVariantLayout {
                name: "None".to_string(),
                field_tys: vec![],
            },
        ],
    };
    // MIR: local_0: Option<i64>, local_1: i64 (tag), local_2: i64 (payload)
    let maybe_fn = RawMirFunction {
        name: "main".to_string(),
        return_ty: option_ty.clone(),
        call_conv: FunctionCallConv::Default,
        params: vec![],
        locals: vec![option_ty, ResolvedTy::I64, ResolvedTy::I64],
        blocks: vec![BasicBlock {
            id: 0,
            statements: Vec::new(),
            instructions: vec![
                // tag = 0 (Some)
                Instr::ConstI64 {
                    dest: Place::Local(1),
                    value: 0,
                },
                Instr::Move {
                    dest: Place::MachineTag(0),
                    src: Place::Local(1),
                },
                // payload = 42
                Instr::ConstI64 {
                    dest: Place::Local(2),
                    value: 42,
                },
                Instr::Move {
                    dest: Place::MachineVariant {
                        local: 0,
                        variant_idx: 0,
                        field_idx: 0,
                    },
                    src: Place::Local(2),
                },
                // Move local_0 → ReturnSlot
                Instr::Move {
                    dest: Place::ReturnSlot,
                    src: Place::Local(0),
                },
            ],
            terminator: Terminator::Return,
        }],
        decisions: Vec::new(),
    };
    IrPipeline {
        thir: Vec::new(),
        raw_mir: vec![maybe_fn],
        checked_mir: Vec::new(),
        elaborated_mir: Vec::new(),
        diagnostics: Vec::new(),
        record_layouts: Vec::new(),
        actor_layouts: Vec::new(),
        supervisor_layouts: Vec::new(),
        machine_layouts: Vec::new(),
        enum_layouts: vec![enum_layout],
        regex_literals: vec![],
        gen_state_layouts: vec![],
        extern_decls: vec![],
    }
}

/// Build a pipeline with `fn greet() -> Option<string> { Some("hello") }`.
fn option_string_pipeline() -> IrPipeline {
    let option_ty = ResolvedTy::Named {
        name: "Option".to_string(),
        args: vec![ResolvedTy::String],
        builtin: None,
    };
    let enum_layout = EnumLayout {
        name: "Option$$string".to_string(),
        tag_width: 1,
        variants: vec![
            MachineVariantLayout {
                name: "Some".to_string(),
                field_tys: vec![ResolvedTy::String],
            },
            MachineVariantLayout {
                name: "None".to_string(),
                field_tys: vec![],
            },
        ],
    };
    let greet_fn = RawMirFunction {
        name: "main".to_string(),
        return_ty: option_ty.clone(),
        call_conv: FunctionCallConv::Default,
        params: vec![],
        locals: vec![option_ty, ResolvedTy::I64, ResolvedTy::String],
        blocks: vec![BasicBlock {
            id: 0,
            statements: Vec::new(),
            instructions: vec![
                Instr::ConstI64 {
                    dest: Place::Local(1),
                    value: 0,
                },
                Instr::Move {
                    dest: Place::MachineTag(0),
                    src: Place::Local(1),
                },
                Instr::StringLit {
                    dest: Place::Local(2),
                    bytes: "hello".as_bytes().to_vec(),
                },
                Instr::Move {
                    dest: Place::MachineVariant {
                        local: 0,
                        variant_idx: 0,
                        field_idx: 0,
                    },
                    src: Place::Local(2),
                },
                Instr::Move {
                    dest: Place::ReturnSlot,
                    src: Place::Local(0),
                },
            ],
            terminator: Terminator::Return,
        }],
        decisions: Vec::new(),
    };
    IrPipeline {
        thir: Vec::new(),
        raw_mir: vec![greet_fn],
        checked_mir: Vec::new(),
        elaborated_mir: Vec::new(),
        diagnostics: Vec::new(),
        record_layouts: Vec::new(),
        actor_layouts: Vec::new(),
        supervisor_layouts: Vec::new(),
        machine_layouts: Vec::new(),
        enum_layouts: vec![enum_layout],
        regex_literals: vec![],
        gen_state_layouts: vec![],
        extern_decls: vec![],
    }
}

/// Option<i64> composite return produces valid LLVM IR with aggregate ret.
#[test]
fn option_i64_return_emits_aggregate_ret() {
    let ll = emit_ll(&option_some_pipeline(), "option_i64_ret");
    assert!(
        ll.contains("ret %\"Option$$i64\""),
        "LLVM IR must contain aggregate ret of Option$$i64; got:\n{ll}"
    );
}

/// Option<i64> composite return passes LLVM module verification.
#[test]
fn option_i64_return_module_verifies() {
    let pipeline = option_some_pipeline();
    let tmp = std::env::temp_dir().join("hew-composite-return-verify");
    std::fs::create_dir_all(&tmp).expect("create scratch dir");
    let options = EmitOptions {
        module_name: "option_verify",
        out_dir: &tmp,
        native: false,
        wasm: false,
    };
    emit_module(&pipeline, &options)
        .expect("emit_module with Option<i64> return must verify cleanly");
}

/// Option<string> triggers the heap-owning composite return fail-closed boundary.
#[test]
fn option_string_return_fails_closed() {
    let pipeline = option_string_pipeline();
    let tmp = std::env::temp_dir().join("hew-composite-return-string-fail");
    std::fs::create_dir_all(&tmp).expect("create scratch dir");
    let options = EmitOptions {
        module_name: "option_string_fail",
        out_dir: &tmp,
        native: false,
        wasm: false,
    };
    let result = emit_module(&pipeline, &options);
    match result {
        Err(CodegenError::FailClosed(msg)) => {
            assert!(
                msg.contains("composite return of heap-owning payload"),
                "expected heap-owning diagnostic, got: {msg}"
            );
            assert!(
                msg.contains("tag-aware drop"),
                "expected tag-aware drop mention, got: {msg}"
            );
        }
        Err(other) => panic!("expected FailClosed, got: {other:?}"),
        Ok(_) => panic!("expected Option<string> return to fail closed, but it succeeded"),
    }
}

/// Build a pipeline with `fn send() -> Envelope<i64>` where
/// `enum Envelope<T> { Data(T), Message(string) }`.
///
/// The type argument `i64` is bitcopy — it would pass the args-only check.
/// But `Message(string)` is a non-param variant carrying a heap-owning field.
/// The fail-closed boundary must catch this even though the type arg is safe.
fn envelope_i64_pipeline() -> IrPipeline {
    let envelope_ty = ResolvedTy::Named {
        name: "Envelope".to_string(),
        args: vec![ResolvedTy::I64],
        builtin: None,
    };
    // Monomorphised layout name follows `mangle("Envelope", [i64])` → "Envelope$$i64".
    let enum_layout = EnumLayout {
        name: "Envelope$$i64".to_string(),
        tag_width: 1,
        variants: vec![
            MachineVariantLayout {
                // Data(T) instantiated as Data(i64) — bitcopy payload
                name: "Data".to_string(),
                field_tys: vec![ResolvedTy::I64],
            },
            MachineVariantLayout {
                // Message(string) — heap-owning, NOT derived from the type param
                name: "Message".to_string(),
                field_tys: vec![ResolvedTy::String],
            },
        ],
    };
    // Minimal MIR: fn send() -> Envelope<i64> { /* unreachable body — rejected before emit */ }
    let send_fn = RawMirFunction {
        name: "main".to_string(),
        return_ty: envelope_ty.clone(),
        call_conv: FunctionCallConv::Default,
        params: vec![],
        locals: vec![envelope_ty],
        blocks: vec![BasicBlock {
            id: 0,
            statements: Vec::new(),
            instructions: vec![Instr::Move {
                dest: Place::ReturnSlot,
                src: Place::Local(0),
            }],
            terminator: Terminator::Return,
        }],
        decisions: Vec::new(),
    };
    IrPipeline {
        thir: Vec::new(),
        raw_mir: vec![send_fn],
        checked_mir: Vec::new(),
        elaborated_mir: Vec::new(),
        diagnostics: Vec::new(),
        record_layouts: Vec::new(),
        actor_layouts: Vec::new(),
        supervisor_layouts: Vec::new(),
        machine_layouts: Vec::new(),
        enum_layouts: vec![enum_layout],
        regex_literals: vec![],
        gen_state_layouts: vec![],
        extern_decls: vec![],
    }
}

/// `Envelope<i64>` with a `Message(string)` variant must fail closed even
/// though the type argument `i64` is bitcopy.
///
/// Regression for the underbroad guard that only inspected enum layouts when
/// `args.is_empty()`, allowing generic enums with non-param heap-owning
/// variant fields to bypass the fail-closed boundary.
#[test]
fn generic_enum_bitcopy_arg_heap_variant_fails_closed() {
    let pipeline = envelope_i64_pipeline();
    let tmp = std::env::temp_dir().join("hew-composite-return-envelope-fail");
    std::fs::create_dir_all(&tmp).expect("create scratch dir");
    let options = EmitOptions {
        module_name: "envelope_fail",
        out_dir: &tmp,
        native: false,
        wasm: false,
    };
    let result = emit_module(&pipeline, &options);
    match result {
        Err(CodegenError::FailClosed(msg)) => {
            assert!(
                msg.contains("composite return of heap-owning payload"),
                "expected heap-owning diagnostic, got: {msg}"
            );
            assert!(
                msg.contains("tag-aware drop"),
                "expected tag-aware drop mention, got: {msg}"
            );
        }
        Err(other) => panic!("expected FailClosed, got: {other:?}"),
        Ok(_) => panic!(
            "Envelope<i64> with Message(string) variant must fail closed, but emit succeeded"
        ),
    }
}
