use std::collections::HashSet;

use hew_hir::{lower_program, BindingId, ResolutionCtx};
use hew_mir::{
    lower_hir_module, CheckedMirFunction, Instr, IrPipeline, MirStatement, OwnerId, OwnershipEvent,
    Place, Terminator,
};
use hew_types::module_registry::ModuleRegistry;
use hew_types::Checker;

const CONSUMING_EXTERN_SOURCE: &str = r#"
#[resource]
type Conn { id: i64 }

impl Conn {
    fn close(self) {}
}

extern "C" {
    fn consume_conn(consume conn: Conn);
}

fn run() {
    let conn = Conn { id: 7 };
    unsafe { consume_conn(conn) };
}

fn main() {
    run();
}
"#;

fn named_binding(function: &CheckedMirFunction, name: &str) -> BindingId {
    function
        .blocks
        .iter()
        .flat_map(|block| &block.statements)
        .find_map(|statement| match statement {
            MirStatement::Bind {
                binding,
                name: candidate,
                ..
            } if candidate == name => Some(*binding),
            _ => None,
        })
        .unwrap_or_else(|| panic!("the owned {name} binding must be present"))
}

fn guarded_owner(function: &CheckedMirFunction, binding: BindingId) -> OwnerId {
    function
        .blocks
        .iter()
        .flat_map(|block| &block.instructions)
        .find_map(|instruction| match instruction {
            Instr::OwnershipEvent(OwnershipEvent::Guard { owner, .. })
                if owner.binding == binding =>
            {
                Some(*owner)
            }
            _ => None,
        })
        .expect("the owned binding must publish a guarded owner generation")
}

fn consuming_call_continuation(function: &CheckedMirFunction, expected_callee: &str) -> u32 {
    function
        .blocks
        .iter()
        .find_map(|block| match &block.terminator {
            Terminator::Call {
                callee, args, next, ..
            } if callee == expected_callee => {
                assert_eq!(args.len(), 1, "consuming extern must have one argument");
                Some(*next)
            }
            _ => None,
        })
        .expect("run must contain the consuming extern call")
}

fn terminal_transfer_source(function: &CheckedMirFunction, expected_owner: OwnerId) -> Place {
    let transfers: Vec<Place> = function
        .blocks
        .iter()
        .flat_map(|block| &block.instructions)
        .filter_map(|instruction| match instruction {
            Instr::OwnershipEvent(OwnershipEvent::Transfer {
                owner,
                from,
                to: None,
                to_owner: None,
                to_ty: None,
            }) if *owner == expected_owner => Some(*from),
            _ => None,
        })
        .collect();
    assert_eq!(
        transfers.len(),
        1,
        "the consuming call must publish exactly one terminal owner transfer"
    );
    transfers[0]
}

fn assert_no_release_after(
    function: &CheckedMirFunction,
    continuation: u32,
    owner: OwnerId,
    owner_place: Place,
) {
    let mut pending = vec![continuation];
    let mut reachable = HashSet::new();
    while let Some(block_id) = pending.pop() {
        if !reachable.insert(block_id) {
            continue;
        }
        let block = function
            .blocks
            .iter()
            .find(|block| block.id == block_id)
            .unwrap_or_else(|| panic!("reachable block bb{block_id} must exist"));
        pending.extend(block.successors());
    }

    for block in function
        .blocks
        .iter()
        .filter(|block| reachable.contains(&block.id))
    {
        assert!(
            block.instructions.iter().all(|instruction| !matches!(
                instruction,
                Instr::OwnershipEvent(OwnershipEvent::Release {
                    owner: released,
                    ..
                }) if *released == owner
            )),
            "scope exit must not release the transferred owner in bb{}: {:#?}",
            block.id,
            block.instructions
        );
        assert!(
            block.instructions.iter().all(|instruction| !matches!(
                instruction,
                Instr::Drop { place, .. } if *place == owner_place
            )),
            "scope exit must not drop the transferred owner slot in bb{}: {:#?}",
            block.id,
            block.instructions
        );
    }
}

fn lower_clean_to_checked_mir(source: &str) -> IrPipeline {
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
    let hir = lower_program(
        &parsed.program,
        &tc_output,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    assert!(
        hir.diagnostics.is_empty(),
        "HIR diagnostics: {:#?}",
        hir.diagnostics
    );
    lower_hir_module(&hir.module)
}

#[test]
fn consuming_owned_call_publishes_terminal_transfer_before_scope_exit() {
    let pipeline = lower_clean_to_checked_mir(CONSUMING_EXTERN_SOURCE);
    assert!(
        pipeline.diagnostics.is_empty(),
        "the ownership transfer must pass MIR validation: {:#?}",
        pipeline.diagnostics
    );

    let run = pipeline
        .checked_mir
        .iter()
        .find(|function| function.name == "run")
        .expect("run must reach Checked MIR");
    assert!(
        run.checks.is_empty(),
        "the consuming call must pass Checked-MIR ownership validation: {:#?}",
        run.checks
    );

    let binding = named_binding(run, "conn");
    let owner = guarded_owner(run, binding);
    let continuation = consuming_call_continuation(run, "consume_conn");
    let owner_place = terminal_transfer_source(run, owner);
    assert_no_release_after(run, continuation, owner, owner_place);
}
