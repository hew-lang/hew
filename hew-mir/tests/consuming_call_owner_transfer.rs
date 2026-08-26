use hew_hir::{lower_program, BindingId, IntentKind, ResolutionCtx};
use hew_mir::{
    lower_hir_module, CheckedMirFunction, Instr, IrPipeline, MirStatement, NeutralizeAuthority,
    OwnerId, OwnershipEvent, Place, Terminator,
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

const CONSUMING_DIRECT_PANIC_SOURCE: &str = r#"
#[resource]
type Conn { id: i64 }

impl Conn {
    fn close(self) {}
}

fn explode() {
    panic("boom");
}

fn consume_then_panic(consume conn: Conn) {
    explode();
}

fn run() {
    let conn = Conn { id: 7 };
    consume_then_panic(conn);
}

fn run_fresh() {
    consume_then_panic(Conn { id: 8 });
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

fn guarded_owner(function: &CheckedMirFunction, binding: BindingId) -> (OwnerId, Place) {
    function
        .blocks
        .iter()
        .flat_map(|block| &block.instructions)
        .find_map(|instruction| match instruction {
            Instr::OwnershipEvent(OwnershipEvent::Guard { owner, flag, .. })
                if owner.binding == binding =>
            {
                Some((*owner, *flag))
            }
            _ => None,
        })
        .expect("the owned binding must publish a guarded owner generation")
}

fn owner_at_place(function: &CheckedMirFunction, expected_place: Place) -> OwnerId {
    function
        .blocks
        .iter()
        .flat_map(|block| &block.instructions)
        .find_map(|instruction| match instruction {
            Instr::OwnershipEvent(OwnershipEvent::Mint { owner, place, .. })
                if *place == expected_place =>
            {
                Some(*owner)
            }
            _ => None,
        })
        .unwrap_or_else(|| panic!("{expected_place:?} must mint an owner"))
}

fn guarded_owner_at_place(
    function: &CheckedMirFunction,
    expected_place: Place,
) -> (OwnerId, Place) {
    let owner = owner_at_place(function, expected_place);
    let flag = function
        .blocks
        .iter()
        .flat_map(|block| &block.instructions)
        .find_map(|instruction| match instruction {
            Instr::OwnershipEvent(OwnershipEvent::Guard {
                owner: guarded,
                flag,
                ..
            }) if *guarded == owner => Some(*flag),
            _ => None,
        })
        .unwrap_or_else(|| panic!("owner {owner:?} must publish a guard"));
    (owner, flag)
}

fn consuming_call_edge(function: &CheckedMirFunction, expected_callee: &str) -> (u32, Place, u32) {
    function
        .blocks
        .iter()
        .find_map(|block| match &block.terminator {
            Terminator::Call {
                callee, args, next, ..
            } if callee == expected_callee => {
                let [argument] = args.as_slice() else {
                    panic!("consuming extern must have one argument: {args:?}");
                };
                Some((block.id, *argument, *next))
            }
            _ => None,
        })
        .expect("run must contain the consuming extern call")
}

fn assert_call_block_retains_owner(
    function: &CheckedMirFunction,
    call_block_id: u32,
    binding: BindingId,
    owner: OwnerId,
    flag: Place,
) {
    let call_block = function
        .blocks
        .iter()
        .find(|block| block.id == call_block_id)
        .expect("consuming call block must exist");
    assert!(
        call_block.statements.iter().any(|statement| matches!(
            statement,
            MirStatement::Use {
                binding: used,
                intent: IntentKind::Consume,
                ..
            } if *used == binding
        )),
        "the source-level consuming argument must remain a checker Consume use: {:#?}",
        call_block.statements
    );
    assert!(
        call_block.instructions.iter().all(|instruction| !matches!(
            instruction,
            Instr::ConstI64 { dest, value: 1 } if *dest == flag
        ) && !matches!(
            instruction,
            Instr::OwnershipEvent(OwnershipEvent::Transfer {
                owner: transferred,
                ..
            }) if *transferred == owner
        )),
        "the call block must retain its guarded owner for the unwind edge: {:#?}",
        call_block.instructions
    );
}

fn unwind_drops<'a>(
    function: &'a CheckedMirFunction,
    expected_callee: &str,
) -> &'a [hew_mir::ElabDrop] {
    function
        .ownership_elaboration
        .as_ref()
        .expect("Checked MIR must retain ownership elaboration")
        .drop_plans
        .iter()
        .find_map(|(exit, plan)| {
            matches!(
                exit,
                hew_mir::ExitPath::Unwind { callee, .. } if callee == expected_callee
            )
            .then_some(plan.drops.as_slice())
        })
        .unwrap_or_else(|| panic!("{expected_callee} must publish an unwind cleanup plan"))
}

fn assert_call_block_transfers_owner(
    function: &CheckedMirFunction,
    call_block_id: u32,
    binding: BindingId,
    owner: OwnerId,
    owner_place: Place,
    flag: Place,
) {
    let call_block = function
        .blocks
        .iter()
        .find(|block| block.id == call_block_id)
        .expect("consuming call block must exist");
    assert!(
        call_block.statements.iter().any(|statement| matches!(
            statement,
            MirStatement::Use {
                binding: used,
                intent: IntentKind::Consume,
                ..
            } if *used == binding
        )),
        "the direct consuming argument must remain a checker Consume use: {:#?}",
        call_block.statements
    );
    let exact_transfers = call_block
        .instructions
        .windows(2)
        .filter(|window| {
            matches!(
                window,
                [
                    Instr::ConstI64 { dest, value: 1 },
                    Instr::OwnershipEvent(OwnershipEvent::Transfer {
                        owner: transferred,
                        from,
                        to: None,
                        to_owner: None,
                        to_ty: None,
                    }),
                ] if *dest == flag && *transferred == owner && *from == owner_place
            )
        })
        .count();
    assert_eq!(
        exact_transfers, 1,
        "a direct Hew consume must transfer caller ownership before invoke: {:#?}",
        call_block.instructions
    );
    assert!(
        call_block.instructions.iter().all(|instruction| !matches!(
            instruction,
            Instr::NeutralizePayloadSlot {
                authority: NeutralizeAuthority::CallDischargeConsume,
                ..
            }
        )),
        "a direct Hew parameter slot, not caller-side neutralization, owns the value"
    );
}

fn assert_exact_normal_edge_commit(
    function: &CheckedMirFunction,
    continuation: u32,
    owner: OwnerId,
    owner_place: Place,
    flag: Place,
) {
    let continuation_block = function
        .blocks
        .iter()
        .find(|block| block.id == continuation)
        .expect("consuming call continuation must exist");
    let exact_commits = continuation_block
        .instructions
        .windows(3)
        .filter(|window| {
            matches!(
                window,
                [
                    Instr::ConstI64 { dest, value: 1 },
                    Instr::NeutralizePayloadSlot {
                        place,
                        transferee: None,
                        authority: NeutralizeAuthority::CallDischargeConsume,
                    },
                    Instr::OwnershipEvent(OwnershipEvent::Transfer {
                        owner: transferred,
                        from,
                        to: None,
                        to_owner: None,
                        to_ty: None,
                    }),
                ] if *dest == flag
                    && *place == owner_place
                    && *transferred == owner
                    && *from == owner_place
            )
        })
        .count();
    assert_eq!(
        exact_commits, 1,
        "the normal edge must contain exactly one guard/neutralize/transfer commit: {:#?}",
        continuation_block.instructions
    );
}

fn assert_unwind_retains_owner(
    function: &CheckedMirFunction,
    expected_callee: &str,
    owner: OwnerId,
    owner_place: Place,
    flag: Place,
) {
    let unwind_drops = unwind_drops(function, expected_callee);
    assert!(
        unwind_drops.iter().any(|drop| {
            drop.place == owner_place
                && drop
                    .guard
                    .is_some_and(|guard| guard.owner == owner && guard.flag == flag)
        }),
        "the unwind edge must retain the exact guarded caller owner: {unwind_drops:#?}"
    );
}

fn assert_unwind_excludes_owner(
    function: &CheckedMirFunction,
    expected_callee: &str,
    owner: OwnerId,
    owner_place: Place,
) {
    let unwind_drops = unwind_drops(function, expected_callee);
    assert!(
        unwind_drops.iter().all(|drop| {
            drop.place != owner_place
                && drop.guard.is_none_or(|guard| guard.owner != owner)
        }),
        "the caller transferred ownership before entry and must not drop it on unwind: {unwind_drops:#?}"
    );
}

fn assert_callee_unwind_drops_param(
    function: &CheckedMirFunction,
    expected_callee: &str,
    owner: OwnerId,
    param_place: Place,
    flag: Place,
) {
    let unwind_drops = unwind_drops(function, expected_callee);
    assert!(
        unwind_drops.iter().any(|drop| {
            drop.place == param_place
                && drop
                    .guard
                    .is_some_and(|guard| guard.owner == owner && guard.flag == flag)
        }),
        "the consuming callee must drop its exact guarded parameter on unwind: {unwind_drops:#?}"
    );
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
fn declared_extern_consume_commits_only_on_normal_return() {
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
    let (owner, flag) = guarded_owner(run, binding);
    let (call_block_id, owner_place, continuation) = consuming_call_edge(run, "consume_conn");
    assert_call_block_retains_owner(run, call_block_id, binding, owner, flag);
    assert_exact_normal_edge_commit(run, continuation, owner, owner_place, flag);
    assert_unwind_retains_owner(run, "consume_conn", owner, owner_place, flag);
}

#[test]
fn direct_hew_consume_transfers_before_invoke_and_callee_owns_unwind() {
    let pipeline = lower_clean_to_checked_mir(CONSUMING_DIRECT_PANIC_SOURCE);
    assert!(
        pipeline.diagnostics.is_empty(),
        "direct consuming ownership must pass MIR validation: {:#?}",
        pipeline.diagnostics
    );

    let run = pipeline
        .checked_mir
        .iter()
        .find(|function| function.name == "run")
        .expect("run must reach Checked MIR");
    assert!(
        run.checks.is_empty(),
        "the direct consuming call must pass Checked-MIR ownership validation: {:#?}",
        run.checks
    );
    let binding = named_binding(run, "conn");
    let (transferred_owner, transfer_guard) = guarded_owner(run, binding);
    let (call_block_id, owner_place, _) = consuming_call_edge(run, "consume_then_panic");
    assert_call_block_transfers_owner(
        run,
        call_block_id,
        binding,
        transferred_owner,
        owner_place,
        transfer_guard,
    );
    assert_unwind_excludes_owner(run, "consume_then_panic", transferred_owner, owner_place);

    let run_fresh = pipeline
        .checked_mir
        .iter()
        .find(|function| function.name == "run_fresh")
        .expect("fresh-value caller must reach Checked MIR");
    assert!(
        run_fresh.checks.is_empty(),
        "the fresh consuming call must pass Checked-MIR ownership validation: {:#?}",
        run_fresh.checks
    );
    let (fresh_call_block_id, fresh_owner_place, _) =
        consuming_call_edge(run_fresh, "consume_then_panic");
    let fresh_owner = owner_at_place(run_fresh, fresh_owner_place);
    let fresh_call_block = run_fresh
        .blocks
        .iter()
        .find(|block| block.id == fresh_call_block_id)
        .expect("fresh consuming call block must exist");
    assert_eq!(
        fresh_call_block
            .instructions
            .iter()
            .filter(|instruction| matches!(
                instruction,
                Instr::OwnershipEvent(OwnershipEvent::Transfer {
                    owner,
                    from,
                    to: None,
                    to_owner: None,
                    to_ty: None,
                }) if *owner == fresh_owner && *from == fresh_owner_place
            ))
            .count(),
        1,
        "a fresh direct consume must terminal-transfer its produced owner before invoke: {:#?}",
        fresh_call_block.instructions
    );
    assert_unwind_excludes_owner(
        run_fresh,
        "consume_then_panic",
        fresh_owner,
        fresh_owner_place,
    );

    let callee = pipeline
        .checked_mir
        .iter()
        .find(|function| function.name == "consume_then_panic")
        .expect("consuming callee must reach Checked MIR");
    assert!(
        callee.checks.is_empty(),
        "the consuming callee must pass Checked-MIR ownership validation: {:#?}",
        callee.checks
    );
    let param_place = Place::Local(0);
    let (param_owner, entry_drop_guard) = guarded_owner_at_place(callee, param_place);
    assert_callee_unwind_drops_param(
        callee,
        "explode",
        param_owner,
        param_place,
        entry_drop_guard,
    );
}
