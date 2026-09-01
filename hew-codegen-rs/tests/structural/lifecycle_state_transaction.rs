use hew_codegen_rs::{emit_module, EmitOptions};
use hew_hir::{lower_program, ResolutionCtx};
use hew_mir::{lower_hir_module, ActorHandlerKind, Instr, IrPipeline, SourceOrigin};
use hew_types::module_registry::ModuleRegistry;
use hew_types::Checker;

const SOURCE: &str =
    include_str!("../../../tests/vertical-slice/accept/actor_lifecycle_state_writes.hew");

fn pipeline() -> IrPipeline {
    let parsed = hew_parser::parse(SOURCE);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let checked = checker.check_program(&parsed.program);
    assert!(
        checked.errors.is_empty(),
        "type errors: {:#?}",
        checked.errors
    );
    let hir = lower_program(
        &parsed.program,
        &checked,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    let pipeline = lower_hir_module(&hir.module);
    assert!(
        pipeline.diagnostics.is_empty(),
        "MIR diagnostics: {:#?}",
        pipeline.diagnostics
    );
    pipeline
}

fn emit_ir(pipeline: &IrPipeline) -> String {
    let dir = tempfile::Builder::new()
        .prefix("lifecycle-state-transaction-")
        .tempdir()
        .expect("tempdir");
    let artifacts = emit_module(
        pipeline,
        &EmitOptions {
            module_name: "lifecycle_state_transaction",
            out_dir: dir.path(),
            native: false,
            wasm: false,
            target_triple: None,
            debug: false,
            opt_level: hew_codegen_rs::OptLevel::O0,
            source_path: None,
        },
    )
    .expect("emit lifecycle phase IR");
    std::fs::read_to_string(artifacts.ll_path.expect("LLVM IR path"))
        .expect("read lifecycle LLVM IR")
        .replace("\r\n", "\n")
}

fn with_required_system_store_counterfactuals(mut pipeline: IrPipeline) -> IrPipeline {
    let receive = pipeline
        .raw_mir
        .iter()
        .find(|function| {
            actor_phase(function) == Some(ActorHandlerKind::Receive) && has_state_store(function)
        })
        .expect("receive state-store MIR")
        .clone();
    let checked = pipeline
        .checked_mir
        .iter()
        .find(|function| function.key == receive.key)
        .expect("checked receive state-store MIR")
        .clone();
    let elaborated = pipeline
        .elaborated_mir
        .iter()
        .find(|function| function.key == receive.key)
        .expect("elaborated receive state-store MIR")
        .clone();
    let actor_layout_key = match &receive.source_origin {
        SourceOrigin::SynthesizedActorHandler {
            actor_layout_key, ..
        } => actor_layout_key.clone(),
        _ => unreachable!("receive is an actor handler"),
    };

    for (kind, name) in [
        (
            ActorHandlerKind::Exit,
            "LifecycleWriter__counterfactual_on_exit",
        ),
        (
            ActorHandlerKind::Down,
            "LifecycleWriter__counterfactual_on_down",
        ),
    ] {
        let key = hew_mir::MirCallableKey::for_test(name);
        let mut system = receive.clone();
        system.name = name.to_string();
        system.key = key.clone();
        system.source_origin = SourceOrigin::SynthesizedActorHandler {
            kind,
            actor_layout_key: actor_layout_key.clone(),
        };
        pipeline.raw_mir.push(system);

        let mut system_checked = checked.clone();
        system_checked.name = name.to_string();
        system_checked.key = key.clone();
        if let Some(ownership_elaboration) = &mut system_checked.ownership_elaboration {
            ownership_elaboration.name = name.to_string();
            ownership_elaboration.key = key.clone();
        }
        pipeline.checked_mir.push(system_checked);

        let mut system_elaborated = elaborated.clone();
        system_elaborated.name = name.to_string();
        system_elaborated.key = key;
        pipeline.elaborated_mir.push(system_elaborated);
    }
    pipeline
}

fn function_body<'a>(ir: &'a str, symbol: &str) -> &'a str {
    let needle = format!("@{symbol}(");
    let start = ir
        .match_indices("define ")
        .find_map(|(offset, _)| {
            let line_end = ir[offset..].find('\n').map_or(ir.len(), |n| offset + n);
            ir[offset..line_end].contains(&needle).then_some(offset)
        })
        .unwrap_or_else(|| panic!("missing definition for `{symbol}` in IR:\n{ir}"));
    let end = ir[start..]
        .find("\n}\n")
        .map(|offset| start + offset + 3)
        .unwrap_or_else(|| panic!("missing closing brace for `{symbol}`"));
    &ir[start..end]
}

fn actor_phase(function: &hew_mir::RawMirFunction) -> Option<ActorHandlerKind> {
    match &function.source_origin {
        SourceOrigin::SynthesizedActorHandler { kind, .. } => Some(*kind),
        _ => None,
    }
}

fn has_state_store(function: &hew_mir::RawMirFunction) -> bool {
    function.blocks.iter().any(|block| {
        block
            .instructions
            .iter()
            .any(|instruction| matches!(instruction, Instr::ActorStateFieldStore { .. }))
    })
}

fn vec_release_calls(body: &str) -> usize {
    vec_local_release_calls(body) + vec_owned_release_calls(body)
}

fn vec_local_release_calls(body: &str) -> usize {
    body.matches("call void @hew_vec_free(").count()
}

fn vec_owned_release_calls(body: &str) -> usize {
    body.matches("call void @hew_vec_free_owned(").count()
}

#[test]
fn mir_carries_every_state_store_phase_without_reclassifying_the_default() {
    let pipeline = pipeline();
    for phase in [
        ActorHandlerKind::Init,
        ActorHandlerKind::Start,
        ActorHandlerKind::Stop,
        ActorHandlerKind::Receive,
    ] {
        assert!(
            pipeline
                .raw_mir
                .iter()
                .any(|function| actor_phase(function) == Some(phase) && has_state_store(function)),
            "missing ActorStateFieldStore carrying {phase:?} MIR identity"
        );
    }

    let main = pipeline
        .raw_mir
        .iter()
        .find(|function| function.name == "main")
        .expect("main MIR");
    assert!(main.blocks.iter().any(|block| block
        .instructions
        .iter()
        .any(|instruction| matches!(instruction, Instr::RecordInit { .. }))));
    assert!(
        !has_state_store(main),
        "declared state defaults must initialize the spawn record, not masquerade as a handler store"
    );
}

#[test]
fn lifecycle_ir_skips_only_state_transaction_hooks_and_keeps_exact_releases() {
    let ir = emit_ir(&with_required_system_store_counterfactuals(pipeline()));
    for symbol in [
        "LifecycleWriter__init",
        "LifecycleWriter__on_start",
        "LifecycleWriter__on_stop__1",
    ] {
        let body = function_body(&ir, symbol);
        assert!(
            !body.contains("@hew_dispatch_state_cleanup_"),
            "{symbol} must not address an absent or foreign scheduler state domain\n{body}"
        );
        assert_eq!(
            vec_local_release_calls(body),
            1,
            "{symbol} must release its uncommitted replacement exactly once on unwind\n{body}"
        );
        assert_eq!(
            vec_owned_release_calls(body),
            1,
            "{symbol} must release the prior state-field Vec exactly once on the normal replacement path\n{body}"
        );
    }

    for symbol in [
        "LifecycleWriter__recv__replace",
        "LifecycleWriter__counterfactual_on_exit",
        "LifecycleWriter__counterfactual_on_down",
    ] {
        let body = function_body(&ir, symbol);
        assert!(
            body.contains("@hew_dispatch_state_cleanup_begin_replace("),
            "required receive/system store must enter its transactional finalizer phase before overwrite\n{body}"
        );
        assert!(
            body.contains("@hew_dispatch_state_cleanup_prepare_transfer(")
                || body.contains("@hew_dispatch_state_cleanup_prepare("),
            "required receive/system store must prepare transfer/publication before its live write\n{body}"
        );
        assert!(
            !body.contains("@hew_dispatch_state_cleanup_clear("),
            "state-store lowering must use the fatal begin-replace transaction, not a recoverable standalone clear\n{body}"
        );
        let begin = body
            .find("@hew_dispatch_state_cleanup_begin_replace(")
            .expect("begin replace call");
        let release = body
            .find("call void @hew_vec_free_owned(")
            .expect("old Vec release");
        let prepare = body
            .find("@hew_dispatch_state_cleanup_prepare_transfer(")
            .or_else(|| body.find("@hew_dispatch_state_cleanup_prepare("))
            .expect("replacement prepare call");
        let live_store = body
            .rfind("ptr %actor_state_field_0_ptr")
            .expect("final actor-state field store");
        assert!(
            begin < release && release < prepare && prepare < live_store,
            "required ordering is fatal begin < old release < replacement prepare < live store\n{body}"
        );
        assert_eq!(
            vec_local_release_calls(body),
            1,
            "required receive/system overwrite must release its uncommitted replacement exactly once on unwind\n{body}"
        );
        assert_eq!(
            vec_owned_release_calls(body),
            1,
            "required receive/system overwrite must release the prior Vec exactly once on the committed path\n{body}"
        );
    }

    let state_drop = function_body(&ir, "__hew_state_drop_LifecycleWriter");
    assert_eq!(
        vec_release_calls(state_drop),
        1,
        "final actor teardown must release the last Vec generation exactly once\n{state_drop}"
    );
}
