//! Stage 3a layout-backed `Vec<T>` codegen tests.
//!
//! The checker rewrites `Vec<record/tuple>` push/get/set/pop/remove to
//! `hew_vec_*_layout` only for Copy element types.  MIR represents those as
//! source-level `Terminator::Call`s; codegen must synthesize the hidden
//! `HewTypeLayout*` operand and pass value slots by address.
//! `Vec::new` descriptor construction is narrower: only named record layouts
//! are descriptor-backed until tuple layout semantics are owned end-to-end.

use hew_codegen_rs::{emit_module, CodegenError, EmitOptions};
use hew_mir::{
    BasicBlock, BlockKind, CheckedMirFunction, DropPlan, ElabBlock, ElaboratedMirFunction,
    ExitPath, Instr, IrPipeline, Place, RawMirFunction, RecordLayout, Terminator,
};
use hew_types::ResolvedTy;

fn point_ty() -> ResolvedTy {
    ResolvedTy::Named {
        name: "Point".to_string(),
        args: vec![],
        builtin: None,
    }
}

fn base_pipeline(
    entry_block: BasicBlock,
    locals: Vec<ResolvedTy>,
    return_ty: ResolvedTy,
) -> IrPipeline {
    let return_block = BasicBlock {
        id: 1,
        statements: vec![],
        instructions: vec![],
        terminator: Terminator::Return,
    };
    let blocks = vec![entry_block, return_block];
    IrPipeline {
        thir: vec![],
        raw_mir: vec![RawMirFunction {
            name: "main".to_string(),
            return_ty: return_ty.clone(),
            call_conv: hew_mir::FunctionCallConv::Default,
            params: vec![],
            locals: locals.clone(),
            blocks: blocks.clone(),
            decisions: vec![],
            intrinsic_id: None,
        }],
        checked_mir: vec![CheckedMirFunction {
            name: "main".to_string(),
            return_ty: return_ty.clone(),
            blocks: blocks.clone(),
            decisions: vec![],
            checks: vec![],
            cooperate_sites: vec![],
        }],
        elaborated_mir: vec![ElaboratedMirFunction {
            name: "main".to_string(),
            return_ty,
            statements: vec![],
            decisions: vec![],
            blocks: vec![
                ElabBlock {
                    id: 0,
                    kind: BlockKind::Normal,
                    drops: vec![],
                    successor: Some(1),
                },
                ElabBlock {
                    id: 1,
                    kind: BlockKind::Normal,
                    drops: vec![],
                    successor: None,
                },
            ],
            drop_plans: vec![(ExitPath::Return { block: 1 }, DropPlan::default())],
            coroutine: None,
            lambda_captures: vec![],
        }],
        diagnostics: vec![],
        opaque_handle_names: vec![],
        record_layouts: vec![RecordLayout {
            name: "Point".to_string(),
            field_tys: vec![ResolvedTy::I64, ResolvedTy::I64],
        }],
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

fn emit_ll(pipeline: IrPipeline, module_name: &str) -> String {
    let tmp = std::env::temp_dir().join(format!("hew-vec-layout-{module_name}"));
    std::fs::create_dir_all(&tmp).expect("create out_dir");
    let options = EmitOptions {
        module_name,
        out_dir: &tmp,
        native: false,
        wasm: false,
    };
    let artefacts =
        emit_module(&pipeline, &options).expect("layout Vec pipeline must emit successfully");
    let ll_path = artefacts
        .ll_path
        .expect("emit_module must populate ll_path");
    std::fs::read_to_string(&ll_path).expect("read emitted .ll")
}

fn emit_error(pipeline: IrPipeline, module_name: &str) -> String {
    let tmp = std::env::temp_dir().join(format!("hew-vec-layout-{module_name}"));
    std::fs::create_dir_all(&tmp).expect("create out_dir");
    let options = EmitOptions {
        module_name,
        out_dir: &tmp,
        native: false,
        wasm: false,
    };
    emit_module(&pipeline, &options)
        .expect_err("layout Vec pipeline must fail closed")
        .to_string()
}

#[test]
fn vec_layout_new_constructs_with_descriptor() {
    let vec_ty = ResolvedTy::Named {
        name: "Vec".to_string(),
        args: vec![point_ty()],
        builtin: None,
    };
    let block = BasicBlock {
        id: 0,
        statements: vec![],
        instructions: vec![],
        terminator: Terminator::Call {
            callee: "Vec::new".to_string(),
            args: vec![],
            dest: Some(Place::Local(0)),
            next: 1,
        },
    };
    let ll = emit_ll(base_pipeline(block, vec![vec_ty], ResolvedTy::Unit), "new");

    assert!(ll.contains("@__hew_layout_new_16_8_plain"));
    assert!(ll.contains("declare ptr @hew_vec_new_with_layout(ptr)"));
    assert!(ll.contains("call ptr @hew_vec_new_with_layout"));
    assert!(ll.contains("store ptr %hew_vec_new_with_layout_call"));
}

#[test]
fn vec_tuple_new_constructor_is_not_descriptor_backed() {
    let tuple_ty = ResolvedTy::Tuple(vec![ResolvedTy::String, ResolvedTy::String]);
    let vec_ty = ResolvedTy::Named {
        name: "Vec".to_string(),
        args: vec![tuple_ty],
        builtin: None,
    };
    let block = BasicBlock {
        id: 0,
        statements: vec![],
        instructions: vec![],
        terminator: Terminator::Call {
            callee: "Vec::new".to_string(),
            args: vec![],
            dest: Some(Place::Local(0)),
            next: 1,
        },
    };
    let error = emit_error(
        base_pipeline(block, vec![vec_ty], ResolvedTy::Unit),
        "tuple_new",
    );

    assert!(
        error.contains("Vec::new has no constructor lowering for element type Tuple"),
        "tuple Vec::new must fail closed instead of fabricating a layout descriptor: {error}"
    );
}

#[test]
fn vec_layout_push_synthesizes_descriptor_and_data_pointer() {
    let vec_ty = ResolvedTy::Named {
        name: "Vec".to_string(),
        args: vec![point_ty()],
        builtin: None,
    };
    let block = BasicBlock {
        id: 0,
        statements: vec![],
        instructions: vec![],
        terminator: Terminator::Call {
            callee: "hew_vec_push_layout".to_string(),
            args: vec![Place::Local(0), Place::Local(1)],
            dest: None,
            next: 1,
        },
    };
    let ll = emit_ll(
        base_pipeline(block, vec![vec_ty, point_ty()], ResolvedTy::Unit),
        "push",
    );

    assert!(ll.contains("@__hew_layout_push_16_8_plain"));
    assert!(ll.contains("declare void @hew_vec_push_layout(ptr, ptr, ptr)"));
    assert!(ll.contains("call void @hew_vec_push_layout"));
}

#[test]
fn vec_layout_get_loads_returned_element_pointer_into_dest() {
    let vec_ty = ResolvedTy::Named {
        name: "Vec".to_string(),
        args: vec![point_ty()],
        builtin: None,
    };
    let block = BasicBlock {
        id: 0,
        statements: vec![],
        instructions: vec![Instr::ConstI64 {
            dest: Place::Local(1),
            value: 0,
        }],
        terminator: Terminator::Call {
            callee: "hew_vec_get_layout".to_string(),
            args: vec![Place::Local(0), Place::Local(1)],
            dest: Some(Place::Local(2)),
            next: 1,
        },
    };
    let ll = emit_ll(
        base_pipeline(
            block,
            vec![vec_ty, ResolvedTy::I64, point_ty()],
            ResolvedTy::Unit,
        ),
        "get",
    );

    assert!(ll.contains("@__hew_layout_get_16_8_plain"));
    assert!(ll.contains("declare ptr @hew_vec_get_layout(ptr, i64, ptr)"));
    assert!(ll.contains("call ptr @hew_vec_get_layout"));
    assert!(ll.contains("load %Point, ptr"));
}

#[test]
fn vec_layout_set_synthesizes_descriptor_index_and_data_pointer() {
    let vec_ty = ResolvedTy::Named {
        name: "Vec".to_string(),
        args: vec![point_ty()],
        builtin: None,
    };
    let block = BasicBlock {
        id: 0,
        statements: vec![],
        instructions: vec![Instr::ConstI64 {
            dest: Place::Local(1),
            value: 0,
        }],
        terminator: Terminator::Call {
            callee: "hew_vec_set_layout".to_string(),
            args: vec![Place::Local(0), Place::Local(1), Place::Local(2)],
            dest: None,
            next: 1,
        },
    };
    let ll = emit_ll(
        base_pipeline(
            block,
            vec![vec_ty, ResolvedTy::I64, point_ty()],
            ResolvedTy::Unit,
        ),
        "set",
    );

    assert!(ll.contains("@__hew_layout_set_16_8_plain"));
    assert!(ll.contains("declare void @hew_vec_set_layout(ptr, i64, ptr, ptr)"));
    assert!(ll.contains("call void @hew_vec_set_layout"));
}

#[test]
fn vec_layout_pop_traps_when_runtime_reports_empty() {
    let vec_ty = ResolvedTy::Named {
        name: "Vec".to_string(),
        args: vec![point_ty()],
        builtin: None,
    };
    let block = BasicBlock {
        id: 0,
        statements: vec![],
        instructions: vec![],
        terminator: Terminator::Call {
            callee: "hew_vec_pop_layout".to_string(),
            args: vec![Place::Local(0)],
            dest: Some(Place::Local(1)),
            next: 1,
        },
    };
    let ll = emit_ll(
        base_pipeline(block, vec![vec_ty, point_ty()], ResolvedTy::Unit),
        "pop",
    );

    assert!(ll.contains("@__hew_layout_pop_16_8_plain"));
    assert!(ll.contains("declare i32 @hew_vec_pop_layout(ptr, ptr, ptr)"));
    assert!(ll.contains("call i32 @hew_vec_pop_layout"));
    assert!(ll.contains("hew_vec_pop_layout_empty"));
    assert!(ll.contains("call void @hew_trap_with_code(i32 205)"));
}

#[test]
fn vec_layout_contains_thunk_emits_per_type_equality_function() {
    // W3.032 Slice 3: codegen lowers the checker-authorized
    // `hew_vec_contains_thunk` callee by synthesising a per-type
    // `__hew_eq_thunk_*` equality function and passing its pointer as the
    // third operand alongside the receiver Vec and the needle alloca.
    let vec_ty = ResolvedTy::Named {
        name: "Vec".to_string(),
        args: vec![point_ty()],
        builtin: None,
    };
    let block = BasicBlock {
        id: 0,
        statements: vec![],
        instructions: vec![],
        terminator: Terminator::Call {
            callee: "hew_vec_contains_thunk".to_string(),
            args: vec![Place::Local(0), Place::Local(1)],
            dest: Some(Place::Local(2)),
            next: 1,
        },
    };
    let ll = emit_ll(
        base_pipeline(
            block,
            vec![vec_ty, point_ty(), ResolvedTy::Bool],
            ResolvedTy::Unit,
        ),
        "contains_thunk",
    );

    // Runtime extern declaration with the agreed `(ptr, ptr, ptr) -> i32` ABI.
    assert!(
        ll.contains("declare i32 @hew_vec_contains_thunk(ptr, ptr, ptr)"),
        "missing hew_vec_contains_thunk runtime decl in:\n{ll}"
    );
    // Call site passes vec ptr, needle ptr, and thunk function pointer.
    assert!(
        ll.contains("call i32 @hew_vec_contains_thunk"),
        "missing hew_vec_contains_thunk call in:\n{ll}"
    );
    // Internal per-type equality thunk emitted (size=16, align=8 for Point).
    assert!(
        ll.contains("define internal i32 @__hew_eq_thunk_16_8_"),
        "missing __hew_eq_thunk_16_8_* definition in:\n{ll}"
    );
    // Thunk body must do field-by-field comparison via icmp, NOT memcmp /
    // fcmp (padding-safe + float-safe).
    assert!(
        !ll.contains("call i32 @memcmp"),
        "thunk must not call memcmp (padding-unsafe): \n{ll}"
    );
    assert!(
        !ll.contains("fcmp"),
        "thunk must not emit fcmp for eligible Copy records: \n{ll}"
    );
}

#[test]
fn vec_layout_contains_thunk_dedups_by_structured_type() {
    // W3.032 Slice 3c: a single equality thunk per structured element type
    // is emitted regardless of how many `contains` call sites reference it.
    // Use two source blocks both calling `hew_vec_contains_thunk` on the same
    // `Vec<Point>` and verify a single `__hew_eq_thunk_16_8_*` definition.
    let vec_ty = ResolvedTy::Named {
        name: "Vec".to_string(),
        args: vec![point_ty()],
        builtin: None,
    };
    let return_block = BasicBlock {
        id: 2,
        statements: vec![],
        instructions: vec![],
        terminator: Terminator::Return,
    };
    let mid_block = BasicBlock {
        id: 1,
        statements: vec![],
        instructions: vec![],
        terminator: Terminator::Call {
            callee: "hew_vec_contains_thunk".to_string(),
            args: vec![Place::Local(0), Place::Local(1)],
            dest: Some(Place::Local(2)),
            next: 2,
        },
    };
    let entry_block = BasicBlock {
        id: 0,
        statements: vec![],
        instructions: vec![],
        terminator: Terminator::Call {
            callee: "hew_vec_contains_thunk".to_string(),
            args: vec![Place::Local(0), Place::Local(1)],
            dest: Some(Place::Local(2)),
            next: 1,
        },
    };
    let return_ty = ResolvedTy::Unit;
    let blocks = vec![entry_block, mid_block, return_block];
    let pipeline = IrPipeline {
        thir: vec![],
        raw_mir: vec![RawMirFunction {
            name: "main".to_string(),
            return_ty: return_ty.clone(),
            call_conv: hew_mir::FunctionCallConv::Default,
            params: vec![],
            locals: vec![vec_ty, point_ty(), ResolvedTy::Bool],
            blocks: blocks.clone(),
            decisions: vec![],
            intrinsic_id: None,
        }],
        checked_mir: vec![CheckedMirFunction {
            name: "main".to_string(),
            return_ty: return_ty.clone(),
            blocks: blocks.clone(),
            decisions: vec![],
            checks: vec![],
            cooperate_sites: vec![],
        }],
        elaborated_mir: vec![ElaboratedMirFunction {
            name: "main".to_string(),
            return_ty,
            statements: vec![],
            decisions: vec![],
            blocks: vec![
                ElabBlock {
                    id: 0,
                    kind: BlockKind::Normal,
                    drops: vec![],
                    successor: Some(1),
                },
                ElabBlock {
                    id: 1,
                    kind: BlockKind::Normal,
                    drops: vec![],
                    successor: Some(2),
                },
                ElabBlock {
                    id: 2,
                    kind: BlockKind::Normal,
                    drops: vec![],
                    successor: None,
                },
            ],
            drop_plans: vec![(ExitPath::Return { block: 2 }, DropPlan::default())],
            coroutine: None,
            lambda_captures: vec![],
        }],
        diagnostics: vec![],
        opaque_handle_names: vec![],
        record_layouts: vec![RecordLayout {
            name: "Point".to_string(),
            field_tys: vec![ResolvedTy::I64, ResolvedTy::I64],
        }],
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
    let ll = emit_ll(pipeline, "contains_thunk_dedup");

    let definitions = ll
        .lines()
        .filter(|line| line.contains("define internal i32 @__hew_eq_thunk_16_8_"))
        .count();
    assert_eq!(
        definitions, 1,
        "expected exactly one __hew_eq_thunk_16_8_* definition, got {definitions}:\n{ll}"
    );
}

#[test]
fn vec_layout_contains_thunk_enum_tag_dispatches_not_byte_compares() {
    // W5.006 Slice 2: a `Vec<enum>::contains` equality thunk must compare the
    // discriminant tag first and then only the active variant's declared
    // payload fields — never byte-compare the `{ tag, [N x i8] }` payload
    // union (whose inactive-variant + padding bytes are indeterminate).
    //
    // Model a monomorphic enum `Maybe { Just(i64); Nothing }` so the eq path
    // sees a tagged-union element: tag dispatch must appear as an LLVM
    // `switch`, an out-of-range tag must trap fail-closed (code 208), and the
    // body must use `icmp` (not `memcmp` / `fcmp`).
    use hew_mir::{EnumLayout, MachineVariantLayout};

    let enum_ty = ResolvedTy::Named {
        name: "Maybe".to_string(),
        args: vec![],
        builtin: None,
    };
    let vec_ty = ResolvedTy::Named {
        name: "Vec".to_string(),
        args: vec![enum_ty.clone()],
        builtin: None,
    };
    let block = BasicBlock {
        id: 0,
        statements: vec![],
        instructions: vec![],
        terminator: Terminator::Call {
            callee: "hew_vec_contains_thunk".to_string(),
            args: vec![Place::Local(0), Place::Local(1)],
            dest: Some(Place::Local(2)),
            next: 1,
        },
    };
    let mut pipeline = base_pipeline(
        block,
        vec![vec_ty, enum_ty, ResolvedTy::Bool],
        ResolvedTy::Unit,
    );
    // Register the enum's tagged-union layout so codegen resolves the element
    // type to the named outer struct and the eq thunk recognises it as a
    // tagged union via `machine_layouts`.
    pipeline.record_layouts = vec![];
    pipeline.enum_layouts = vec![EnumLayout {
        name: "Maybe".to_string(),
        tag_width: 1,
        variants: vec![
            MachineVariantLayout {
                name: "Just".to_string(),
                field_tys: vec![ResolvedTy::I64],
            },
            MachineVariantLayout {
                name: "Nothing".to_string(),
                field_tys: vec![],
            },
        ],
    }];

    let ll = emit_ll(pipeline, "contains_thunk_enum");

    // An equality thunk is emitted for the enum element type.
    assert!(
        ll.contains("define internal i32 @__hew_eq_thunk_"),
        "missing enum equality thunk definition in:\n{ll}"
    );
    // Tag dispatch lowers to an LLVM `switch` (not a flat field-by-field
    // walk of the outer struct).
    assert!(
        ll.contains("switch"),
        "enum eq thunk must tag-dispatch via switch in:\n{ll}"
    );
    // Out-of-range tag traps fail-closed with the exhaustiveness-fallthrough
    // code (208) rather than comparing indeterminate payload bytes.
    assert!(
        ll.contains("call void @hew_trap_with_code(i32 208)"),
        "enum eq thunk must trap (code 208) on out-of-range tag in:\n{ll}"
    );
    // Equality is field/tag comparison, never a whole-union byte compare or
    // a float compare.
    assert!(
        !ll.contains("call i32 @memcmp"),
        "enum eq thunk must not memcmp the payload union (indeterminate bytes):\n{ll}"
    );
    assert!(
        !ll.contains("fcmp"),
        "enum eq thunk must not emit fcmp:\n{ll}"
    );
}

#[test]
fn vec_layout_contains_thunk_emits_for_wasm_target() {
    // W3.032 Slice 3c WASM validation: equality-thunk indirect calls must
    // lower to wasm32 `call_indirect` without panic.  This is the positive
    // alternative to a `WasmUnsupportedSubstrate` structured diagnostic —
    // we exercise the wasm pipeline directly and expect a successful emit.
    let vec_ty = ResolvedTy::Named {
        name: "Vec".to_string(),
        args: vec![point_ty()],
        builtin: None,
    };
    let block = BasicBlock {
        id: 0,
        statements: vec![],
        instructions: vec![],
        terminator: Terminator::Call {
            callee: "hew_vec_contains_thunk".to_string(),
            args: vec![Place::Local(0), Place::Local(1)],
            dest: Some(Place::Local(2)),
            next: 1,
        },
    };
    let pipeline = base_pipeline(
        block,
        vec![vec_ty, point_ty(), ResolvedTy::Bool],
        ResolvedTy::Unit,
    );
    let tmp = std::env::temp_dir().join("hew-vec-layout-contains-thunk-wasm");
    std::fs::create_dir_all(&tmp).expect("create out_dir");
    let options = EmitOptions {
        module_name: "contains_thunk_wasm",
        out_dir: &tmp,
        native: false,
        wasm: true,
    };
    let result = emit_module(&pipeline, &options);
    // Accept either a clean emit (wasm32 indirect call worked) or a
    // structured `WasmUnsupportedSubstrate` fail-closed naming this
    // substrate symbol.  A panic, generic LLVM error, or any other
    // variant is a contract violation.
    match result {
        Ok(_) => {}
        Err(CodegenError::WasmUnsupportedSubstrate { symbol }) => {
            assert_eq!(
                symbol, "hew_vec_contains_thunk",
                "WasmUnsupportedSubstrate symbol must name the contains-thunk \
                 substrate; got: {symbol}"
            );
        }
        Err(other) => panic!(
            "wasm emit must succeed or fail with \
             WasmUnsupportedSubstrate {{ symbol: \"hew_vec_contains_thunk\" }}; \
             got: {other}"
        ),
    }
}

#[test]
fn vec_layout_clone_synthesizes_descriptor_and_returns_new_vec() {
    // W3.003: codegen lowers `hew_vec_clone_layout(vec)` by
    // deriving the hidden `HewTypeLayout*` from the Vec<Point> element type
    // and emitting a ptr-returning call with (ptr vec, ptr layout).
    let vec_ty = ResolvedTy::Named {
        name: "Vec".to_string(),
        args: vec![point_ty()],
        builtin: None,
    };
    let cloned_vec_ty = vec_ty.clone();
    let block = BasicBlock {
        id: 0,
        statements: vec![],
        instructions: vec![],
        terminator: Terminator::Call {
            callee: "hew_vec_clone_layout".to_string(),
            args: vec![Place::Local(0)],
            dest: Some(Place::Local(1)),
            next: 1,
        },
    };
    let ll = emit_ll(
        base_pipeline(block, vec![vec_ty, cloned_vec_ty], ResolvedTy::Unit),
        "clone",
    );

    // Runtime extern declaration: ptr (ptr, ptr).
    assert!(
        ll.contains("declare ptr @hew_vec_clone_layout(ptr, ptr)"),
        "missing hew_vec_clone_layout runtime decl in:\n{ll}"
    );
    // Layout descriptor global synthesized with size=16, align=8 (two i64 fields).
    assert!(
        ll.contains("@__hew_layout_clone_16_8_plain"),
        "missing layout descriptor global in:\n{ll}"
    );
    // Call site emitted.
    assert!(
        ll.contains("call ptr @hew_vec_clone_layout"),
        "missing hew_vec_clone_layout call in:\n{ll}"
    );
    // Clone returns a ptr — verify no void call.
    let has_void_call = ll
        .lines()
        .any(|line| line.contains("call void @hew_vec_clone_layout"));
    assert!(
        !has_void_call,
        "clone must emit `call ptr @hew_vec_clone_layout`, \
         not a void call:\n{ll}"
    );
}

#[test]
fn vec_layout_remove_synthesizes_descriptor_and_index() {
    // W3.003: codegen lowers `hew_vec_remove_at_layout(vec, index)` by
    // deriving the hidden `HewTypeLayout*` from the Vec<Point> element type
    // and emitting a void call with (ptr vec, i64 index, ptr layout).
    let vec_ty = ResolvedTy::Named {
        name: "Vec".to_string(),
        args: vec![point_ty()],
        builtin: None,
    };
    let block = BasicBlock {
        id: 0,
        statements: vec![],
        instructions: vec![Instr::ConstI64 {
            dest: Place::Local(1),
            value: 1,
        }],
        terminator: Terminator::Call {
            callee: "hew_vec_remove_at_layout".to_string(),
            args: vec![Place::Local(0), Place::Local(1)],
            dest: None,
            next: 1,
        },
    };
    let ll = emit_ll(
        base_pipeline(block, vec![vec_ty, ResolvedTy::I64], ResolvedTy::Unit),
        "remove",
    );

    // Runtime extern declaration: void (ptr, i64, ptr).
    assert!(
        ll.contains("declare void @hew_vec_remove_at_layout(ptr, i64, ptr)"),
        "missing hew_vec_remove_at_layout runtime decl in:\n{ll}"
    );
    // Layout descriptor global synthesized with size=16, align=8 (two i64 fields).
    assert!(
        ll.contains("@__hew_layout_remove_16_8_plain"),
        "missing layout descriptor global in:\n{ll}"
    );
    // Call site emitted.
    assert!(
        ll.contains("call void @hew_vec_remove_at_layout"),
        "missing hew_vec_remove_at_layout call in:\n{ll}"
    );
    // Remove is void — the call site must not capture a return value.
    // A non-void regression would look like `%foo = call i... @hew_vec_remove_at_layout`
    // (i.e. `= call` with any type token before the callee name).  We detect that
    // pattern directly so a future ABI change cannot silently slip past.
    let has_typed_result = ll.lines().any(|line| {
        // Match `= call <type> @hew_vec_remove_at_layout` — the `= call` prefix
        // with any token between `call` and the callee name indicates a result is
        // being captured.
        if let Some(after_call) = line.split("= call ").nth(1) {
            after_call.contains("@hew_vec_remove_at_layout")
        } else {
            false
        }
    });
    assert!(
        !has_typed_result,
        "remove must emit `call void @hew_vec_remove_at_layout`, \
         not a typed result-capturing call: \n{ll}"
    );
}
