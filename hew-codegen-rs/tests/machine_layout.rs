use hew_codegen_rs::{emit_module, CodegenError, EmitOptions};
use hew_mir::{
    BasicBlock, FunctionCallConv, Instr, IrPipeline, MachineLayout, MachineVariantLayout, Place,
    RawMirFunction, Terminator,
};
use hew_types::ResolvedTy;

fn empty_pipeline(machine_layouts: Vec<MachineLayout>) -> IrPipeline {
    IrPipeline {
        thir: Vec::new(),
        raw_mir: Vec::new(),
        checked_mir: Vec::new(),
        elaborated_mir: Vec::new(),
        diagnostics: Vec::new(),
        opaque_handle_names: vec![],
        record_layouts: Vec::new(),
        actor_layouts: Vec::new(),
        supervisor_layouts: Vec::new(),
        machine_layouts,
        enum_layouts: Vec::new(),
        regex_literals: Vec::new(),
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

fn emit_ll(pipeline: &IrPipeline, module_name: &str) -> Result<String, CodegenError> {
    let tmp = std::env::temp_dir().join(format!("hew-machine-layout-{module_name}"));
    std::fs::create_dir_all(&tmp).expect("create scratch dir");
    let options = EmitOptions {
        module_name,
        out_dir: &tmp,
        native: false,
        wasm: false,
        target_triple: None,
        debug: false,
        source_path: None,
    };
    let artefacts = emit_module(pipeline, &options)?;
    let ll_path = artefacts
        .ll_path
        .expect("emit_module must produce textual LLVM IR");
    Ok(std::fs::read_to_string(&ll_path)
        .unwrap_or_else(|e| panic!("read {}: {e}", ll_path.display())))
}

fn unit_variant(name: &str) -> MachineVariantLayout {
    MachineVariantLayout {
        name: name.to_string(),
        field_tys: Vec::new(),
    }
}

fn traffic_light_layout() -> MachineLayout {
    MachineLayout {
        name: "TrafficLight".to_string(),
        tag_width: 2,
        variants: vec![
            unit_variant("Red"),
            unit_variant("Yellow"),
            unit_variant("Green"),
        ],
        events: Vec::new(),
    }
}

#[test]
fn traffic_light_uses_i8_tagged_union_struct() {
    let machine_ty = ResolvedTy::Named {
        name: "TrafficLight".to_string(),
        args: Vec::new(),
        builtin: None,
        is_opaque: false,
    };
    let mut pipeline = empty_pipeline(vec![traffic_light_layout()]);
    pipeline.raw_mir = vec![RawMirFunction {
        name: "main".to_string(),
        return_ty: ResolvedTy::Unit,
        call_conv: FunctionCallConv::Default,
        params: Vec::new(),
        locals: vec![machine_ty],
        blocks: vec![BasicBlock {
            id: 0,
            statements: Vec::new(),
            instructions: Vec::new(),
            terminator: Terminator::Return,
        }],
        decisions: Vec::new(),
        intrinsic_id: None,
        await_deadline_ns: std::collections::HashMap::new(),

        lambda_actor_user_param_locals: Vec::new(),
        span: None,
        instr_spans: ::std::collections::BTreeMap::new(),
    }];
    let ll = emit_ll(&pipeline, "traffic_light_layout").expect("TrafficLight must emit");

    assert!(
        ll.contains("%TrafficLight = type { i8, [1 x i8] }"),
        "TrafficLight must use an i8 tag for three states:\n{ll}"
    );
}

#[test]
fn repeated_machine_uses_share_one_named_struct_definition() {
    let machine_ty = ResolvedTy::Named {
        name: "TrafficLight".to_string(),
        args: Vec::new(),
        builtin: None,
        is_opaque: false,
    };
    let mut pipeline = empty_pipeline(vec![traffic_light_layout()]);
    pipeline.raw_mir = vec![RawMirFunction {
        name: "main".to_string(),
        return_ty: ResolvedTy::Unit,
        call_conv: FunctionCallConv::Default,
        params: Vec::new(),
        locals: vec![machine_ty.clone(), machine_ty],
        blocks: vec![BasicBlock {
            id: 0,
            statements: Vec::new(),
            instructions: Vec::new(),
            terminator: Terminator::Return,
        }],
        decisions: Vec::new(),
        intrinsic_id: None,
        await_deadline_ns: std::collections::HashMap::new(),

        lambda_actor_user_param_locals: Vec::new(),
        span: None,
        instr_spans: ::std::collections::BTreeMap::new(),
    }];

    let ll = emit_ll(&pipeline, "traffic_light_cache").expect("TrafficLight must emit");
    assert_eq!(
        ll.matches("%TrafficLight = type").count(),
        1,
        "machine struct type must be cached/reused by machine name:\n{ll}"
    );
}

#[test]
fn two_hundred_fifty_seven_states_use_i16_tag() {
    let machine_ty = ResolvedTy::Named {
        name: "WideTags".to_string(),
        args: Vec::new(),
        builtin: None,
        is_opaque: false,
    };
    let variants = (0..257)
        .map(|idx| unit_variant(&format!("S{idx}")))
        .collect();
    let mut pipeline = empty_pipeline(vec![MachineLayout {
        name: "WideTags".to_string(),
        tag_width: 9,
        variants,
        events: Vec::new(),
    }]);
    pipeline.raw_mir = vec![RawMirFunction {
        name: "main".to_string(),
        return_ty: ResolvedTy::Unit,
        call_conv: FunctionCallConv::Default,
        params: Vec::new(),
        locals: vec![machine_ty],
        blocks: vec![BasicBlock {
            id: 0,
            statements: Vec::new(),
            instructions: Vec::new(),
            terminator: Terminator::Return,
        }],
        decisions: Vec::new(),
        intrinsic_id: None,
        await_deadline_ns: std::collections::HashMap::new(),

        lambda_actor_user_param_locals: Vec::new(),
        span: None,
        instr_spans: ::std::collections::BTreeMap::new(),
    }];

    let ll = emit_ll(&pipeline, "wide_tags").expect("WideTags must emit");
    assert!(
        ll.contains("%WideTags = type { i16, [1 x i8] }"),
        "257 states must use an i16 tag:\n{ll}"
    );
}

fn constructor_pipeline() -> IrPipeline {
    let machine_ty = ResolvedTy::Named {
        name: "Fiction".to_string(),
        args: Vec::new(),
        builtin: None,
        is_opaque: false,
    };
    let layout = MachineLayout {
        name: "Fiction".to_string(),
        tag_width: 1,
        variants: vec![
            unit_variant("Red"),
            MachineVariantLayout {
                name: "WithValue".to_string(),
                field_tys: vec![ResolvedTy::I64],
            },
        ],
        events: Vec::new(),
    };
    let main_fn = RawMirFunction {
        name: "main".to_string(),
        return_ty: ResolvedTy::Unit,
        call_conv: FunctionCallConv::Default,
        params: Vec::new(),
        locals: vec![machine_ty, ResolvedTy::I64, ResolvedTy::I64],
        blocks: vec![BasicBlock {
            id: 0,
            statements: Vec::new(),
            instructions: vec![
                Instr::ConstI64 {
                    dest: Place::Local(1),
                    value: 1,
                },
                Instr::ConstI64 {
                    dest: Place::Local(2),
                    value: 42,
                },
                Instr::Move {
                    dest: Place::MachineVariant {
                        local: 0,
                        variant_idx: 1,
                        field_idx: 0,
                    },
                    src: Place::Local(2),
                },
                Instr::Move {
                    dest: Place::MachineTag(0),
                    src: Place::Local(1),
                },
            ],
            terminator: Terminator::Return,
        }],
        decisions: Vec::new(),
        intrinsic_id: None,
        await_deadline_ns: std::collections::HashMap::new(),

        lambda_actor_user_param_locals: Vec::new(),
        span: None,
        instr_spans: ::std::collections::BTreeMap::new(),
    };

    let mut pipeline = empty_pipeline(vec![layout]);
    pipeline.raw_mir = vec![main_fn];
    pipeline
}

#[test]
fn variant_construction_emits_payload_gep_and_tag_store() {
    let ll = emit_ll(&constructor_pipeline(), "fiction_ctor").expect("constructor must emit");

    assert!(
        ll.contains("%Fiction = type { i8, [1 x i64] }"),
        "Fiction must use an i8 tag and max-sized payload storage:\n{ll}"
    );
    assert!(
        ll.contains("machine_payload_ptr"),
        "struct-variant construction must GEP to the payload field:\n{ll}"
    );
    assert!(
        ll.contains("machine_variant_field_ptr"),
        "struct-variant construction must GEP to the variant field:\n{ll}"
    );
    assert!(
        ll.contains("store i8 "),
        "variant construction must store the discriminant into the i8 tag:\n{ll}"
    );
}

#[test]
fn native_payload_size_and_alignment_use_target_data() {
    inkwell::targets::Target::initialize_native(&inkwell::targets::InitializationConfig {
        base: true,
        ..inkwell::targets::InitializationConfig::default()
    })
    .expect("native target must initialise");
    let triple = inkwell::targets::TargetMachine::get_default_triple();
    let target = inkwell::targets::Target::from_triple(&triple).expect("host target exists");
    let tm = target
        .create_target_machine(
            &triple,
            "generic",
            "",
            inkwell::OptimizationLevel::None,
            inkwell::targets::RelocMode::Default,
            inkwell::targets::CodeModel::Default,
        )
        .expect("target machine");
    let td = tm.get_target_data();
    let ctx = inkwell::context::Context::create();
    let payload_struct = ctx.struct_type(&[ctx.i64_type().into()], false);

    assert_eq!(
        td.get_abi_size(&payload_struct),
        8,
        "single-i64 variant payload size must come from TargetData"
    );
    assert_eq!(
        td.get_abi_alignment(&payload_struct),
        8,
        "single-i64 variant payload alignment must come from TargetData"
    );
}

#[test]
fn over_65536_states_returns_unsupported() {
    let variants = (0..65_537)
        .map(|idx| unit_variant(&format!("S{idx}")))
        .collect();
    let pipeline = empty_pipeline(vec![MachineLayout {
        name: "TooManyStates".to_string(),
        tag_width: 17,
        variants,
        events: Vec::new(),
    }]);

    let err = emit_ll(&pipeline, "too_many_states").expect_err("must fail closed");
    assert!(
        matches!(err, CodegenError::Unsupported(_)),
        "expected CodegenError::Unsupported for >65,536 states, got {err:?}"
    );
}
