use hew_mir::model::NeutralizeAuthority;
use hew_mir::{Instr, Place, RawMirFunction, SendAliasMode, SuspendKind, Terminator};
use hew_types::module_registry::ModuleRegistry;
use hew_types::Checker;

fn pipeline_with_tc(source: &str) -> hew_mir::IrPipeline {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    assert!(
        tc_output.errors.is_empty(),
        "type errors: {:#?}",
        tc_output.errors
    );
    let output = hew_hir::lower_program(
        &parsed.program,
        &tc_output,
        &hew_hir::ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    hew_mir::lower_hir_module(&output.module)
}

fn forward_handler(pipeline: &hew_mir::IrPipeline) -> &RawMirFunction {
    pipeline
        .raw_mir
        .iter()
        .find(|function| function.name == "Forwarder__recv__forward")
        .expect("forward handler raw MIR")
}

fn ask_payload(function: &RawMirFunction) -> (u32, &[SendAliasMode], Place, bool) {
    let mut asks = function
        .suspend_kinds
        .iter()
        .filter_map(|(&block_id, kind)| match kind {
            SuspendKind::Ask {
                arg_modes,
                value,
                cleanup_plan,
                ..
            } => Some((
                block_id,
                arg_modes.as_slice(),
                *value,
                cleanup_plan.is_some(),
            )),
            _ => None,
        });
    let ask = asks.next().expect("forward handler ask suspension");
    assert!(
        asks.next().is_none(),
        "fixture must contain exactly one ask suspension"
    );
    ask
}

const CONDITIONAL_ASK_RETURN: &str = r"
actor Recipient {
    receive fn take(data: bytes) -> i64 { data.len() as i64 }
}
actor Forwarder {
    let recipient: LocalPid<Recipient>;
    receive fn forward(data: bytes, flag: bool) -> bytes {
        if flag {
            let _ = await recipient.take(data);
        }
        data
    }
}
fn main() -> i64 { 0 }
";

const SIBLING_ASK_OR_RETURN: &str = r#"
actor Recipient {
    receive fn take(data: bytes) -> i64 { data.len() as i64 }
}
actor Forwarder {
    let recipient: LocalPid<Recipient>;
    receive fn forward(data: bytes, flag: bool) -> bytes {
        if flag {
            let _ = await recipient.take(data);
            b"sent"
        } else {
            data
        }
    }
}
fn main() -> i64 { 0 }
"#;

#[test]
fn ask_before_shared_tail_snapshots_live_bytes() {
    let pipeline = pipeline_with_tc(CONDITIONAL_ASK_RETURN);
    assert!(
        pipeline.diagnostics.is_empty(),
        "{:#?}",
        pipeline.diagnostics
    );
    let function = forward_handler(&pipeline);
    let (ask_block, modes, prepared_payload, has_failure_cleanup) = ask_payload(function);

    assert_eq!(modes, [SendAliasMode::SnapshotRetain]);
    assert!(
        has_failure_cleanup,
        "a failed ask must release its prepared actor carrier"
    );

    let instructions = &function.blocks[ask_block as usize].instructions;
    assert!(
        instructions.iter().any(|instr| matches!(
            instr,
            Instr::ValueSnapshotClone {
                dest,
                src: Place::Local(0),
                ..
            } if *dest == prepared_payload
        )),
        "the ask must snapshot the still-live source into its prepared carrier: \
         {instructions:#?}"
    );
    assert!(
        !instructions.iter().any(|instr| matches!(
            instr,
            Instr::NeutralizePayloadSlot {
                place: Place::Local(0),
                ..
            }
        )),
        "the still-live source must not be consumed: {instructions:#?}"
    );
}

#[test]
fn sibling_ask_or_return_transfers_only_the_ask_path() {
    let pipeline = pipeline_with_tc(SIBLING_ASK_OR_RETURN);
    assert!(
        pipeline.diagnostics.is_empty(),
        "{:#?}",
        pipeline.diagnostics
    );
    let function = forward_handler(&pipeline);
    let (ask_block, modes, prepared_payload, has_failure_cleanup) = ask_payload(function);

    assert_eq!(modes, [SendAliasMode::TransferLastUse]);
    assert!(
        has_failure_cleanup,
        "a failed ask must release its prepared actor carrier"
    );

    let instructions = &function.blocks[ask_block as usize].instructions;
    let move_index = instructions
        .iter()
        .position(|instr| {
            matches!(
                instr,
                Instr::Move {
                    dest,
                    src: Place::Local(0),
                } if *dest == prepared_payload
            )
        })
        .expect("ask path must move the bytes parameter into the prepared carrier");
    let neutralize_index = instructions
        .iter()
        .position(|instr| {
            matches!(
                instr,
                Instr::NeutralizePayloadSlot {
                    place: Place::Local(0),
                    transferee: Some(dest),
                    authority: NeutralizeAuthority::SendTransferLastUse,
                } if *dest == prepared_payload
            )
        })
        .expect("ask-path transfer must null the source slot with explicit authority");
    assert!(
        move_index < neutralize_index,
        "the source must be neutralized after its ownership reaches the carrier"
    );

    let return_instructions = &function
        .blocks
        .iter()
        .find(|block| matches!(block.terminator, Terminator::Return))
        .expect("forward handler return block")
        .instructions;
    let returned = return_instructions
        .iter()
        .find_map(|instr| match instr {
            Instr::Move {
                dest: Place::ReturnSlot,
                src,
            } => Some(*src),
            _ => None,
        })
        .expect("return block must move the selected result into the return slot");
    assert!(
        return_instructions
            .iter()
            .any(|instr| matches!(instr, Instr::BytesRetain { value } if *value == returned)),
        "the exact bytes place moved into the return slot must be retained for the caller: \
         {return_instructions:#?}"
    );
}
