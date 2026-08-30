use hew_hir::{lower_program, BindingId, IntentKind, ResolutionCtx};
use hew_mir::{
    lower_hir_module, CheckedMirFunction, ExitPath, Instr, IrPipeline, MirDiagnosticKind,
    MirStatement, NeutralizeAuthority, OwnerId, OwnershipEvent, ParamBoundaryMode, Place, Strategy,
    Terminator,
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

const VEC_ITER_LATER_ARG_EXIT_SOURCE: &str = r"
fn take_cursor(it: VecIter<i64>, seed: i64, f: fn(i64, i64) -> i64) -> i64 {
    f(seed, 0)
}

fn call_or_return(k: i64, enter: bool) -> i64 {
    let xs: Vec<i64> = Vec.new();
    xs.push(k);
    xs.push(k + 1);
    take_cursor(
        xs.into_iter(),
        if enter { 0 } else { return -1 },
        |acc: i64, x: i64| acc + x,
    )
}

fn main() { let _ = call_or_return(1, true); }
";

const VEC_ITER_DUPLICATE_SOURCE: &str = r"
fn take_two(a: VecIter<i64>, b: VecIter<i64>) {}

fn main() {
    let xs: Vec<i64> = Vec.new();
    let cursor = xs.into_iter();
    take_two(cursor, cursor);
}
";

const VEC_ITER_REUSE_SOURCE: &str = r"
fn take_cursor(it: VecIter<i64>) {}

fn main() {
    let xs: Vec<i64> = Vec.new();
    var cursor = xs.into_iter();
    take_cursor(cursor);
    let _ = cursor.next();
}
";

const VEC_ITER_CLOSURE_FORWARD_SOURCE: &str = r"
fn take_cursor(it: VecIter<i64>) {}

fn main() {
    let forward = |it: VecIter<i64>| { take_cursor(it); };
    let xs: Vec<i64> = Vec.new();
    forward(xs.into_iter());
}
";

const VEC_ITER_NONINTRINSIC_VAR_SELF_SOURCE: &str = r"
trait Advance {
    fn advance(var self) -> i64;
}

impl Advance for VecIter<i64> {
    fn advance(var self) -> i64 { 0 }
}

fn main() -> i64 {
    let xs: Vec<i64> = Vec.new();
    var cursor = xs.into_iter();
    cursor.advance()
}
";

const VEC_ITER_TWO_CURSOR_SOURCE: &str = r"
fn take_two(a: VecIter<i64>, b: VecIter<i64>) -> i64 { 0 }

fn main() -> i64 {
    let xs: Vec<i64> = Vec.new();
    let ys: Vec<i64> = Vec.new();
    take_two(xs.into_iter(), ys.into_iter())
}
";

const VEC_ITER_FORWARD_SOURCE: &str = r"
fn leaf(it: VecIter<i64>) -> i64 { 0 }

fn forward(it: VecIter<i64>) -> i64 {
    leaf(it)
}

fn main() -> i64 {
    let xs: Vec<i64> = Vec.new();
    forward(xs.into_iter())
}
";

const VEC_ITER_MIXED_CARRIER_SOURCE: &str = r#"
type Payload { value: string }

fn return_payload(it: VecIter<i64>, payload: Payload) -> Payload {
    payload
}

fn mixed() -> i64 {
    let xs: Vec<i64> = Vec.new();
    let payload = Payload { value: "caller" };
    let returned = return_payload(xs.into_iter(), payload);
    returned.value.len() + payload.value.len()
}

fn main() -> i64 { mixed() }
"#;

const VEC_ITER_REASSIGN_SOURCE: &str = r"
fn take_cursor(it: VecIter<i64>) -> i64 { 0 }

fn main() -> i64 {
    let first: Vec<i64> = Vec.new();
    var cursor = first.into_iter();
    let second: Vec<i64> = Vec.new();
    cursor = second.into_iter();
    take_cursor(cursor)
}
";

const VEC_ITER_OWNED_ELEMENT_ASYMMETRY_SOURCE: &str = r#"
type Row { value: string }

fn callee_without_vec(it: VecIter<Row>) -> i64 { 1 }

fn callee_with_vec(it: VecIter<Row>) -> i64 {
    let local: Vec<Row> = Vec.new();
    local.push(Row { value: "callee" });
    2
}

fn caller_with_vec() -> i64 {
    let rows: Vec<Row> = Vec.new();
    rows.push(Row { value: "caller" });
    callee_without_vec(rows.into_iter())
}

fn caller_without_vec(it: VecIter<Row>) -> i64 {
    callee_with_vec(it)
}

fn main() -> i64 {
    let rows: Vec<Row> = Vec.new();
    rows.push(Row { value: "entry" });
    caller_with_vec() + caller_without_vec(rows.into_iter())
}
"#;

const VEC_ITER_EXTERN_AUTHORITY_SOURCE: &str = r#"
extern "C" {
    fn foreign_cursor(it: VecIter<i64>);
}

fn main() {
    let xs: Vec<i64> = Vec.new();
    unsafe { foreign_cursor(xs.into_iter()) };
}
"#;

const VEC_ITER_NAMED_FUNCTION_VALUE_SOURCE: &str = r"
fn take_cursor(it: VecIter<i64>) {}

fn main() {
    let invoke = take_cursor;
    let xs: Vec<i64> = Vec.new();
    let cursor = xs.into_iter();
    invoke(cursor);
}
";

const VEC_ITER_STATIC_TRAIT_BRIDGE_SOURCE: &str = r"
trait Drain {
    fn drain(value: Self) -> i64;
}

impl Drain for VecIter<i64> {
    fn drain(it: VecIter<i64>) -> i64 { 0 }
}

fn invoke<T: Drain>(value: T) -> i64 {
    value.drain()
}

fn main() -> i64 {
    let xs: Vec<i64> = Vec.new();
    invoke(xs.into_iter())
}
";

const VEC_ITER_STATIC_TRAIT_LATER_ARG_SOURCE: &str = r"
trait Inspect {
    fn inspect(value: Self, it: VecIter<i64>, seed: i64) -> i64;
}

type Marker { value: i64 }

impl Inspect for Marker {
    fn inspect(value: Marker, it: VecIter<i64>, seed: i64) -> i64 { seed }
}

fn invoke<T: Inspect>(value: T, it: VecIter<i64>, enter: bool) -> i64 {
    value.inspect(it, if enter { 0 } else { return -1 })
}

fn main() -> i64 {
    let xs: Vec<i64> = Vec.new();
    invoke(Marker { value: 1 }, xs.into_iter(), true)
}
";

const VEC_ITER_VAR_SELF_EXPLICIT_ARG_SOURCE: &str = r"
trait Inspect {
    fn inspect(var self, it: VecIter<i64>, seed: i64) -> i64;
}

type Counter { value: i64 }

impl Inspect for Counter {
    fn inspect(var self, it: VecIter<i64>, seed: i64) -> i64 { seed }
}

fn call_or_return(enter: bool) -> i64 {
    var counter = Counter { value: 0 };
    let xs: Vec<i64> = Vec.new();
    counter.inspect(xs.into_iter(), if enter { 0 } else { return -1 })
}

fn main() -> i64 { call_or_return(true) }
";

const VEC_ITER_SUSPENDING_CLOSURE_SOURCE: &str = r"
actor Driver {
    receive fn run() {
        let xs: Vec<i64> = Vec.new();
        let pause = |it: VecIter<i64>| { sleep(1ms); };
        pause(xs.into_iter());
    }
}

fn main() {
    let driver = spawn Driver();
    driver.run();
}
";

const VEC_ITER_UNSUPPORTED_RELEASE_SOURCE: &str = r#"
fn take_unsupported(it: VecIter<bytes>, later: i64) {}

fn later_sentinel() -> i64 { 7 }

fn call_unsupported(it: VecIter<bytes>) {
    take_unsupported(it, later_sentinel());
}

fn key_sentinel() -> string { "k" }

fn stash_unsupported(m: HashMap<string, VecIter<bytes>>, it: VecIter<bytes>) {
    m.insert(key_sentinel(), it);
}

fn supported_key_sentinel() -> string { "supported" }

fn stash_supported(m: HashMap<string, VecIter<i64>>, it: VecIter<i64>) {
    m.insert(supported_key_sentinel(), it);
}

fn main() {}
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

fn direct_call<'a>(
    function: &'a CheckedMirFunction,
    callee_fragment: &str,
) -> (&'a hew_mir::BasicBlock, &'a str, &'a [Place]) {
    function
        .blocks
        .iter()
        .find_map(|block| match &block.terminator {
            Terminator::Call { callee, args, .. } if callee.contains(callee_fragment) => {
                Some((block, callee.as_str(), args.as_slice()))
            }
            _ => None,
        })
        .unwrap_or_else(|| panic!("{} must call {callee_fragment}", function.name))
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

fn has_not_yet_implemented(pipeline: &IrPipeline, expected: &str) -> bool {
    pipeline.diagnostics.iter().any(|diagnostic| {
        matches!(
            &diagnostic.kind,
            MirDiagnosticKind::NotYetImplemented { construct, .. } if construct == expected
        )
    })
}

fn assert_owned_cursor_boundary(function: &CheckedMirFunction, param_index: u32) {
    assert!(
        function.decisions.iter().any(|decision| matches!(
            decision.strategy,
            Strategy::ParamBoundary(fact)
                if fact.param_index == param_index
                    && fact.mode == ParamBoundaryMode::OwnedCursor
        )),
        "{} parameter {param_index} must publish OwnedCursor: {:#?}",
        function.name,
        function.decisions
    );
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

fn temporary_cursor_owner(function: &CheckedMirFunction) -> (OwnerId, Place, Place) {
    let binding = named_binding(function, "__hew_vec_iter_value");
    let (owner, flag) = guarded_owner(function, binding);
    let source = function
        .blocks
        .iter()
        .flat_map(|block| &block.instructions)
        .find_map(|instruction| match instruction {
            Instr::OwnershipEvent(OwnershipEvent::Mint {
                owner: minted,
                place,
                ..
            }) if *minted == owner => Some(*place),
            _ => None,
        })
        .expect("the temporary cursor must mint at its produced place");
    (owner, flag, source)
}

fn assert_temporary_cursor_commit_is_last(
    caller: &CheckedMirFunction,
    owner: OwnerId,
    flag: Place,
    source: Place,
) {
    let (call_block, _callee_symbol, args) = direct_call(caller, "take_cursor");
    assert_eq!(
        args.first(),
        Some(&source),
        "the call must receive the exact temporary cursor place"
    );
    let transfer_blocks: Vec<_> = caller
        .blocks
        .iter()
        .filter(|block| {
            block.instructions.iter().any(|instruction| {
                matches!(
                    instruction,
                    Instr::OwnershipEvent(OwnershipEvent::Transfer {
                        owner: transferred,
                        from,
                        to: None,
                        ..
                    }) if *transferred == owner && *from == source
                )
            })
        })
        .map(|block| block.id)
        .collect();
    assert_eq!(transfer_blocks, [call_block.id]);
    let transfer_index = call_block
        .instructions
        .windows(2)
        .position(|window| matches!(
            window,
            [Instr::ConstI64 { dest, value: 1 }, Instr::OwnershipEvent(OwnershipEvent::Transfer { owner: transferred, from, to: None, .. })]
                if *dest == flag && *transferred == owner && *from == source
        ))
        .expect("the final call block must disarm and transfer the cursor once");
    assert_eq!(
        transfer_index + 2,
        call_block.instructions.len(),
        "the cursor commit must be the literal final instruction pair before invoke"
    );
    assert!(
        call_block.instructions[..transfer_index]
            .iter()
            .any(|instruction| matches!(instruction, Instr::MakeClosure { .. })),
        "the closure argument must be fully materialised before the cursor transfer"
    );
}

fn assert_temporary_cursor_preinvoke_cleanup(
    caller: &CheckedMirFunction,
    owner: OwnerId,
    flag: Place,
    source: Place,
) {
    let (argument_branch, branch_targets) = caller
        .blocks
        .iter()
        .find_map(|block| {
            let guards_cursor = block.instructions.iter().any(|instruction| {
                matches!(
                    instruction,
                    Instr::OwnershipEvent(OwnershipEvent::Guard { owner: guarded, .. })
                        if *guarded == owner
                )
            });
            match (&block.terminator, guards_cursor) {
                (
                    Terminator::Branch {
                        then_target,
                        else_target,
                        ..
                    },
                    true,
                ) => Some((block, [*then_target, *else_target])),
                _ => None,
            }
        })
        .expect("later argument evaluation must branch after the cursor mint");
    let early_return = branch_targets
        .into_iter()
        .find(|target| {
            caller
                .blocks
                .iter()
                .find(|block| block.id == *target)
                .is_some_and(|block| matches!(block.terminator, Terminator::Return))
        })
        .expect("one later-argument branch must return before the call");
    let (call_block, callee_symbol, _) = direct_call(caller, "take_cursor");
    assert_ne!(argument_branch.id, call_block.id);
    let cursor_drop_exits: Vec<_> = caller
        .ownership_elaboration
        .as_ref()
        .expect("Checked MIR must retain ownership elaboration")
        .drop_plans
        .iter()
        .filter(|(_, plan)| {
            plan.drops.iter().any(|drop| {
                drop.place == source
                    && drop
                        .guard
                        .is_some_and(|guard| guard.owner == owner && guard.flag == flag)
            })
        })
        .map(|(exit, _)| exit)
        .collect();
    assert!(matches!(
        cursor_drop_exits.as_slice(),
        [ExitPath::Return { block }] if *block == early_return
    ));
    assert_unwind_excludes_owner(caller, callee_symbol, owner, source);
}

fn assert_owned_cursor_callee_cleanup(pipeline: &IrPipeline) {
    let callee = pipeline
        .checked_mir
        .iter()
        .find(|function| function.name == "take_cursor")
        .expect("the cursor callee must reach Checked MIR");
    assert_owned_cursor_boundary(callee, 0);
    let param_place = Place::Local(0);
    let (owner, flag) = guarded_owner_at_place(callee, param_place);
    let drops_param = |drop: &hew_mir::ElabDrop| {
        drop.place == param_place
            && drop
                .guard
                .is_some_and(|guard| guard.owner == owner && guard.flag == flag)
    };
    let plans = &callee
        .ownership_elaboration
        .as_ref()
        .expect("Checked MIR must retain ownership elaboration")
        .drop_plans;
    assert_eq!(
        callee
            .blocks
            .iter()
            .flat_map(|block| &block.instructions)
            .filter(|instruction| matches!(
                instruction,
                Instr::RecordFieldDrop { record, .. } if *record == param_place
            ))
            .count(),
        1
    );
    assert_eq!(
        callee
            .blocks
            .iter()
            .flat_map(|block| &block.instructions)
            .filter(|instruction| matches!(
                instruction,
                Instr::OwnershipEvent(OwnershipEvent::GuardedRelease {
                    owner: released,
                    place,
                    flag: release_flag,
                }) if *released == owner && *place == param_place && *release_flag == flag
            ))
            .count(),
        1
    );
    assert!(plans.iter().any(|(exit, plan)| {
        matches!(exit, ExitPath::Unwind { .. }) && plan.drops.iter().any(drops_param)
    }));
}

#[test]
fn vec_iter_temporary_transfers_after_later_arguments_finish() {
    let pipeline = lower_clean_to_checked_mir(VEC_ITER_LATER_ARG_EXIT_SOURCE);
    assert!(
        pipeline.diagnostics.is_empty(),
        "{:#?}",
        pipeline.diagnostics
    );
    let caller = pipeline
        .checked_mir
        .iter()
        .find(|function| function.name == "call_or_return")
        .expect("the caller must reach Checked MIR");
    assert!(caller.checks.is_empty(), "{:#?}", caller.checks);
    let (owner, flag, source) = temporary_cursor_owner(caller);
    assert_temporary_cursor_commit_is_last(caller, owner, flag, source);
    assert_temporary_cursor_preinvoke_cleanup(caller, owner, flag, source);
    assert_owned_cursor_callee_cleanup(&pipeline);
}

#[test]
fn vec_iter_duplicate_and_post_call_reuse_fail_closed() {
    let duplicate = lower_clean_to_checked_mir(VEC_ITER_DUPLICATE_SOURCE);
    assert!(
        has_not_yet_implemented(&duplicate, "duplicate OwnedCursor argument"),
        "the same cursor owner cannot enter two parameters: {:#?}",
        duplicate.diagnostics
    );

    let reuse = lower_clean_to_checked_mir(VEC_ITER_REUSE_SOURCE);
    assert!(
        reuse.diagnostics.iter().any(|diagnostic| matches!(
            &diagnostic.kind,
            MirDiagnosticKind::UseAfterConsume { name, .. } if name == "cursor"
        )),
        "a direct OwnedCursor call must consume the named source before later reuse: {:#?}",
        reuse.diagnostics
    );
}

#[test]
fn borrowed_closure_cursor_cannot_forward_to_owned_cursor() {
    let pipeline = lower_clean_to_checked_mir(VEC_ITER_CLOSURE_FORWARD_SOURCE);
    assert!(
        has_not_yet_implemented(&pipeline, "OwnedCursor argument without one caller owner"),
        "a borrowed closure parameter must not mint authority when forwarded: {:#?}",
        pipeline.diagnostics
    );
}

#[test]
fn nonintrinsic_vec_iter_var_self_fails_before_invoke() {
    let pipeline = lower_clean_to_checked_mir(VEC_ITER_NONINTRINSIC_VAR_SELF_SOURCE);
    assert!(
        has_not_yet_implemented(&pipeline, "non-intrinsic VecIter var-self call"),
        "mutable cursor writeback needs a fresh normal-return owner rearm: {:#?}",
        pipeline.diagnostics
    );
}

#[test]
fn two_owned_cursors_commit_atomically_at_one_invoke() {
    let pipeline = lower_clean_to_checked_mir(VEC_ITER_TWO_CURSOR_SOURCE);
    assert!(
        pipeline.diagnostics.is_empty(),
        "two independent cursors must pass the direct ABI together: {:#?}",
        pipeline.diagnostics
    );
    let caller = pipeline
        .checked_mir
        .iter()
        .find(|function| function.name == "main")
        .expect("main must reach Checked MIR");
    assert!(caller.checks.is_empty(), "checks: {:#?}", caller.checks);
    let (call_block, callee_symbol, args) = caller
        .blocks
        .iter()
        .find_map(|block| match &block.terminator {
            Terminator::Call { callee, args, .. } if callee.contains("take_two") => {
                Some((block, callee.as_str(), args.as_slice()))
            }
            _ => None,
        })
        .expect("main must invoke take_two");
    let [first, second] = args else {
        panic!("take_two must receive two cursors: {args:?}");
    };
    let (first_owner, first_flag) = guarded_owner_at_place(caller, *first);
    let (second_owner, second_flag) = guarded_owner_at_place(caller, *second);
    let suffix = &call_block.instructions[call_block.instructions.len() - 4..];
    assert!(
        matches!(
            suffix,
            [
                Instr::ConstI64 { dest: first_dest, value: 1 },
                Instr::OwnershipEvent(OwnershipEvent::Transfer {
                    owner: first_transferred,
                    from: first_from,
                    to: None,
                    ..
                }),
                Instr::ConstI64 { dest: second_dest, value: 1 },
                Instr::OwnershipEvent(OwnershipEvent::Transfer {
                    owner: second_transferred,
                    from: second_from,
                    to: None,
                    ..
                }),
            ] if *first_dest == first_flag
                && *first_transferred == first_owner
                && *first_from == *first
                && *second_dest == second_flag
                && *second_transferred == second_owner
                && *second_from == *second
        ),
        "both cursor commits must form the final atomic suffix: {suffix:#?}"
    );
    assert_unwind_excludes_owner(caller, callee_symbol, first_owner, *first);
    assert_unwind_excludes_owner(caller, callee_symbol, second_owner, *second);

    let callee_function = pipeline
        .checked_mir
        .iter()
        .find(|function| function.name == "take_two")
        .expect("take_two must reach Checked MIR");
    let owned_cursor_modes = callee_function
        .decisions
        .iter()
        .filter(|decision| {
            matches!(
                decision.strategy,
                Strategy::ParamBoundary(fact) if fact.mode == ParamBoundaryMode::OwnedCursor
            )
        })
        .count();
    assert_eq!(
        owned_cursor_modes, 2,
        "both direct VecIter parameters must mint OwnedCursor"
    );
}

#[test]
fn owned_cursor_param_can_forward_its_exact_owner() {
    let pipeline = lower_clean_to_checked_mir(VEC_ITER_FORWARD_SOURCE);
    assert!(
        pipeline.diagnostics.is_empty(),
        "forwarding a callee-owned cursor must pass MIR validation: {:#?}",
        pipeline.diagnostics
    );
    let leaf = pipeline
        .checked_mir
        .iter()
        .find(|function| function.name == "leaf")
        .expect("leaf must reach Checked MIR");
    let forward = pipeline
        .checked_mir
        .iter()
        .find(|function| function.name == "forward")
        .expect("forward must reach Checked MIR");
    assert_owned_cursor_boundary(leaf, 0);
    assert_owned_cursor_boundary(forward, 0);

    let source_param = Place::Local(0);
    let (owner, flag) = guarded_owner_at_place(forward, source_param);
    let binding = owner.binding;
    let (call_block, callee, args) = direct_call(forward, "leaf");
    let [source] = args else {
        panic!("leaf must receive one cursor: {args:?}");
    };
    assert_eq!(*source, source_param);
    assert_call_block_transfers_owner(forward, call_block.id, binding, owner, *source, flag);
    assert_unwind_excludes_owner(forward, callee, owner, *source);
}

#[test]
fn owned_cursor_commit_follows_snapshot_clone_carrier_preparation() {
    let pipeline = lower_clean_to_checked_mir(VEC_ITER_MIXED_CARRIER_SOURCE);
    assert!(
        pipeline.diagnostics.is_empty(),
        "the mixed cursor/carrier call must pass MIR validation: {:#?}",
        pipeline.diagnostics
    );
    let caller = pipeline
        .checked_mir
        .iter()
        .find(|function| function.name == "mixed")
        .expect("mixed must reach Checked MIR");
    assert!(caller.checks.is_empty(), "checks: {:#?}", caller.checks);
    let (call_block, _, args) = direct_call(caller, "return_payload");
    let [cursor_source, _prepared_payload] = args else {
        panic!("return_payload must receive cursor and payload: {args:?}");
    };
    let (cursor_owner, cursor_flag) = guarded_owner_at_place(caller, *cursor_source);
    let clone_index = call_block
        .instructions
        .iter()
        .position(|instruction| matches!(instruction, Instr::ValueSnapshotClone { .. }))
        .expect("the live summary-owned payload must be snapshot-cloned");
    let transfer_index = call_block
        .instructions
        .windows(2)
        .position(|window| {
            matches!(
                window,
                [
                    Instr::ConstI64 { dest, value: 1 },
                    Instr::OwnershipEvent(OwnershipEvent::Transfer {
                        owner,
                        from,
                        to: None,
                        ..
                    }),
                ] if *dest == cursor_flag && *owner == cursor_owner && *from == *cursor_source
            )
        })
        .expect("the cursor must commit at the mixed call");
    assert!(
        clone_index < transfer_index,
        "the possibly-unwinding snapshot clone must precede cursor relinquishment: {:#?}",
        call_block.instructions
    );
    assert_eq!(
        transfer_index + 2,
        call_block.instructions.len(),
        "the cursor commit must remain the literal final pair after carrier preparation"
    );
}

#[test]
fn reassigned_cursor_transfers_only_its_latest_guarded_generation() {
    let pipeline = lower_clean_to_checked_mir(VEC_ITER_REASSIGN_SOURCE);
    assert!(
        pipeline.diagnostics.is_empty(),
        "a reassigned cursor must pass MIR validation: {:#?}",
        pipeline.diagnostics
    );
    let caller = pipeline
        .checked_mir
        .iter()
        .find(|function| function.name == "main")
        .expect("main must reach Checked MIR");
    let binding = named_binding(caller, "cursor");
    let (call_block, _, args) = direct_call(caller, "take_cursor");
    let [source] = args else {
        panic!("take_cursor must receive one cursor: {args:?}");
    };
    let transferred = call_block
        .instructions
        .iter()
        .find_map(|instruction| match instruction {
            Instr::OwnershipEvent(OwnershipEvent::Transfer {
                owner,
                from,
                to: None,
                ..
            }) if owner.binding == binding && *from == *source => Some(*owner),
            _ => None,
        })
        .expect("the reassigned cursor must transfer one owner");
    let mut guarded_generations = caller
        .blocks
        .iter()
        .flat_map(|block| &block.instructions)
        .filter_map(|instruction| match instruction {
            Instr::OwnershipEvent(OwnershipEvent::Guard { owner, flag, .. })
                if owner.binding == binding =>
            {
                Some((*owner, *flag))
            }
            _ => None,
        })
        .collect::<Vec<_>>();
    guarded_generations.sort_by_key(|(owner, _)| owner.generation);
    guarded_generations.dedup();
    assert!(
        guarded_generations.len() >= 2,
        "reassignment must publish a distinct owner generation: {guarded_generations:#?}"
    );
    let (latest_owner, latest_flag) = guarded_generations
        .last()
        .copied()
        .expect("at least one guarded generation");
    assert_eq!(
        transferred, latest_owner,
        "the call must select the exact latest owner, not an historical same-binding guard"
    );
    assert!(matches!(
        call_block.instructions.as_slice(),
        [.., Instr::ConstI64 { dest, value: 1 }, Instr::OwnershipEvent(OwnershipEvent::Transfer { owner, from, to: None, .. })]
            if *dest == latest_flag && *owner == latest_owner && *from == *source
    ));
}

#[test]
fn owned_record_element_cursor_boundary_is_function_independent() {
    let pipeline = lower_clean_to_checked_mir(VEC_ITER_OWNED_ELEMENT_ASYMMETRY_SOURCE);
    assert!(
        pipeline.diagnostics.is_empty(),
        "owned-record cursor ABI must not depend on local Vec harvests: {:#?}",
        pipeline.diagnostics
    );
    for name in [
        "callee_without_vec",
        "callee_with_vec",
        "caller_without_vec",
    ] {
        let function = pipeline
            .checked_mir
            .iter()
            .find(|function| function.name == name)
            .unwrap_or_else(|| panic!("{name} must reach Checked MIR"));
        assert_owned_cursor_boundary(function, 0);
    }

    for (caller_name, callee_name) in [
        ("caller_with_vec", "callee_without_vec"),
        ("caller_without_vec", "callee_with_vec"),
    ] {
        let caller = pipeline
            .checked_mir
            .iter()
            .find(|function| function.name == caller_name)
            .unwrap_or_else(|| panic!("{caller_name} must reach Checked MIR"));
        let (call_block, callee_symbol, args) = direct_call(caller, callee_name);
        let [source] = args else {
            panic!("{callee_name} must receive one cursor: {args:?}");
        };
        let (owner, flag) = guarded_owner_at_place(caller, *source);
        let transfer_count = call_block
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
                            ..
                        }),
                    ] if *dest == flag && *transferred == owner && *from == *source
                )
            })
            .count();
        assert_eq!(
            transfer_count, 1,
            "{caller_name} must hand off exactly one cursor owner to {callee_name}"
        );
        assert_unwind_excludes_owner(caller, callee_symbol, owner, *source);
    }
}

#[test]
fn extern_item_id_does_not_authorise_owned_cursor_transfer() {
    let pipeline = lower_clean_to_checked_mir(VEC_ITER_EXTERN_AUTHORITY_SOURCE);
    assert!(
        has_not_yet_implemented(
            &pipeline,
            "VecIter argument at an unsupported call authority"
        ),
        "an extern ItemId has no emitted OwnedCursor parameter boundary: {:#?}",
        pipeline.diagnostics
    );
}

#[test]
fn named_function_value_cannot_bridge_borrowed_and_owned_cursor_abis() {
    let pipeline = lower_clean_to_checked_mir(VEC_ITER_NAMED_FUNCTION_VALUE_SOURCE);
    assert!(
        has_not_yet_implemented(
            &pipeline,
            "named function `take_cursor` with a VecIter parameter used as a value"
        ),
        "the raw named-function shim cannot forward a borrowed ClosureInvoke cursor into an OwnedCursor callee: {:#?}",
        pipeline.diagnostics
    );
}

#[test]
fn generic_static_trait_cursor_receiver_uses_owned_cursor_funnel() {
    let pipeline = lower_clean_to_checked_mir(VEC_ITER_STATIC_TRAIT_BRIDGE_SOURCE);
    assert!(
        pipeline.diagnostics.is_empty(),
        "static trait dispatch must use the typed cursor funnel: {:#?}",
        pipeline.diagnostics
    );
    let caller = pipeline
        .checked_mir
        .iter()
        .find(|function| {
            function.name.contains("invoke")
                && function.blocks.iter().any(|block| {
                    matches!(
                        &block.terminator,
                        Terminator::Call { callee, .. } if callee.contains("drain")
                    )
                })
        })
        .expect("the monomorphised invoke body must reach Checked MIR");
    assert_owned_cursor_boundary(caller, 0);
    let (call_block, callee_symbol, args) = direct_call(caller, "drain");
    let [source] = args else {
        panic!("Drain::drain must receive one cursor: {args:?}");
    };
    let (owner, flag) = guarded_owner_at_place(caller, *source);
    assert!(matches!(
        call_block.instructions.as_slice(),
        [.., Instr::ConstI64 { dest, value: 1 }, Instr::OwnershipEvent(OwnershipEvent::Transfer { owner: transferred, from, to: None, .. })]
            if *dest == flag && *transferred == owner && *from == *source
    ));
    assert_unwind_excludes_owner(caller, callee_symbol, owner, *source);

    let impl_body = pipeline
        .checked_mir
        .iter()
        .find(|function| function.name.contains("VecIter") && function.name.contains("drain"))
        .expect("the concrete Drain impl must reach Checked MIR");
    assert_owned_cursor_boundary(impl_body, 0);
}

fn assert_static_trait_early_cursor_cleanup(
    caller: &CheckedMirFunction,
    owner: OwnerId,
    flag: Place,
    source: Place,
) {
    let (early_cleanup, early_release) = caller
        .blocks
        .iter()
        .find_map(|block| {
            let Terminator::Branch {
                then_target,
                else_target,
                ..
            } = block.terminator
            else {
                return None;
            };
            block
                .statements
                .iter()
                .any(|statement| matches!(statement, MirStatement::Return { .. }))
                .then_some((then_target, else_target))
        })
        .expect("the later argument's early return must retain its cleanup branch");
    let cleanup_block = &caller.blocks[early_cleanup as usize];
    assert!(
        cleanup_block
            .instructions
            .iter()
            .any(|instruction| matches!(
                instruction,
                Instr::RecordFieldDrop {
                    record,
                    drop_fn: hew_mir::DropFnSpec::Release(symbol),
                    ..
                } if *record == source && *symbol == "hew_vec_free"
            ))
            && cleanup_block
                .instructions
                .iter()
                .any(|instruction| matches!(
                    instruction,
                    Instr::ConstI64 { dest, value: 1 } if *dest == flag
                ))
            && matches!(cleanup_block.terminator, Terminator::Goto { target } if target == early_release),
        "the unentered route must physically release the cursor field and disarm its guard"
    );
    let release_block = &caller.blocks[early_release as usize];
    assert!(
        release_block
            .instructions
            .iter()
            .any(|instruction| matches!(
                instruction,
                Instr::OwnershipEvent(OwnershipEvent::GuardedRelease {
                    owner: released,
                    place,
                    flag: release_flag,
                }) if *released == owner && *place == source && *release_flag == flag
            ))
            && matches!(release_block.terminator, Terminator::Return),
        "the unentered route must close the exact guarded owner once"
    );
}

#[test]
fn static_trait_explicit_cursor_waits_for_later_argument() {
    let pipeline = lower_clean_to_checked_mir(VEC_ITER_STATIC_TRAIT_LATER_ARG_SOURCE);
    assert!(
        pipeline.diagnostics.is_empty(),
        "static trait explicit cursor handoff must pass MIR validation: {:#?}",
        pipeline.diagnostics
    );
    let caller = pipeline
        .checked_mir
        .iter()
        .find(|function| {
            function.name.contains("invoke")
                && function.blocks.iter().any(|block| {
                    matches!(
                        &block.terminator,
                        Terminator::Call { callee, .. } if callee.contains("inspect")
                    )
                })
        })
        .expect("the monomorphised invoke body must reach Checked MIR");
    assert_owned_cursor_boundary(caller, 1);
    let (call_block, callee_symbol, args) = direct_call(caller, "inspect");
    let [_receiver, source, _seed] = args else {
        panic!("Inspect::inspect must receive receiver, cursor, and seed: {args:?}");
    };
    let (owner, flag) = guarded_owner_at_place(caller, *source);
    let transfer_blocks = caller
        .blocks
        .iter()
        .filter(|block| {
            block.instructions.iter().any(|instruction| {
                matches!(
                    instruction,
                    Instr::OwnershipEvent(OwnershipEvent::Transfer {
                        owner: transferred,
                        from,
                        to: None,
                        ..
                    }) if *transferred == owner && *from == *source
                )
            })
        })
        .map(|block| block.id)
        .collect::<Vec<_>>();
    assert_eq!(
        transfer_blocks,
        [call_block.id],
        "the cursor must remain owned throughout later-argument control flow"
    );
    assert!(matches!(
        call_block.instructions.as_slice(),
        [.., Instr::ConstI64 { dest, value: 1 }, Instr::OwnershipEvent(OwnershipEvent::Transfer { owner: transferred, from, to: None, .. })]
            if *dest == flag && *transferred == owner && *from == *source
    ));
    assert_static_trait_early_cursor_cleanup(caller, owner, flag, *source);
    assert_unwind_excludes_owner(caller, callee_symbol, owner, *source);

    let impl_body = pipeline
        .checked_mir
        .iter()
        .find(|function| function.name.contains("Marker") && function.name.contains("inspect"))
        .expect("the concrete Inspect impl must reach Checked MIR");
    assert_owned_cursor_boundary(impl_body, 1);
}

#[test]
fn var_self_bridge_rejects_explicit_cursor_before_later_argument_lowering() {
    let pipeline = lower_clean_to_checked_mir(VEC_ITER_VAR_SELF_EXPLICIT_ARG_SOURCE);
    assert!(
        has_not_yet_implemented(&pipeline, "VecIter argument at a var-self call bridge"),
        "the dual-return var-self bridge must not bypass the late cursor handoff: {:#?}",
        pipeline.diagnostics
    );
    let caller = pipeline
        .raw_mir
        .iter()
        .find(|function| function.name == "call_or_return")
        .expect("the rejected caller must retain diagnostic raw MIR");
    assert!(
        caller.blocks.iter().all(|block| !matches!(
            &block.terminator,
            Terminator::Call { callee, .. } if callee.contains("inspect")
        )),
        "the unsupported bridge must reject before emitting a partial invoke"
    );
}

#[test]
fn suspending_closure_rejects_borrowed_cursor_before_emitting_suspend() {
    let pipeline = lower_clean_to_checked_mir(VEC_ITER_SUSPENDING_CLOSURE_SOURCE);
    assert!(
        has_not_yet_implemented(&pipeline, "VecIter argument at a suspending closure call"),
        "a borrowed cursor cannot survive closure suspension or cancellation: {:#?}",
        pipeline.diagnostics
    );
    assert!(
        pipeline.raw_mir.iter().all(|function| {
            function
                .suspend_kinds
                .values()
                .all(|kind| !matches!(kind, hew_mir::SuspendKind::CallClosure { .. }))
        }),
        "the unsupported closure call must reject before emitting a suspension carrier"
    );
}

fn assert_unsupported_cursor_param_is_inert(pipeline: &IrPipeline) {
    let take = pipeline
        .raw_mir
        .iter()
        .find(|function| function.name == "take_unsupported")
        .expect("the rejected callee must retain diagnostic Raw MIR");
    assert!(
        take.blocks
            .iter()
            .flat_map(|block| &block.instructions)
            .all(|instruction| !matches!(
                instruction,
                Instr::OwnershipEvent(
                    OwnershipEvent::Mint {
                        place: Place::Local(0),
                        ..
                    } | OwnershipEvent::Guard { .. }
                        | OwnershipEvent::Transfer {
                            from: Place::Local(0),
                            ..
                        }
                        | OwnershipEvent::GuardedRelease {
                            place: Place::Local(0),
                            ..
                        }
                ) | Instr::RecordFieldDrop {
                    record: Place::Local(0),
                    ..
                }
            )),
        "an unsupported parameter must not mint or release half of OwnedCursor: {:#?}",
        take.blocks
    );
    assert!(
        pipeline
            .checked_mir
            .iter()
            .filter(|function| function.name == "take_unsupported")
            .flat_map(|function| &function.decisions)
            .all(|decision| !matches!(
                decision.strategy,
                Strategy::ParamBoundary(fact) if fact.mode == ParamBoundaryMode::OwnedCursor
            )),
        "the unsupported parameter must publish no OwnedCursor boundary"
    );
}

fn assert_unsupported_direct_cursor_call_is_inert(pipeline: &IrPipeline) {
    let call = pipeline
        .raw_mir
        .iter()
        .find(|function| function.name == "call_unsupported")
        .expect("the rejected direct caller must retain diagnostic Raw MIR");
    assert!(
        call.blocks.iter().all(|block| !matches!(
            &block.terminator,
            Terminator::Call { callee, .. }
                if callee.contains("take_unsupported") || callee.contains("later_sentinel")
        )),
        "the direct preflight must reject before either the cursor or later operand is lowered: {:#?}",
        call.blocks
    );
    assert!(
        call.blocks
            .iter()
            .flat_map(|block| &block.instructions)
            .all(|instruction| !matches!(
                instruction,
                Instr::OwnershipEvent(
                    OwnershipEvent::Mint {
                        place: Place::Local(0),
                        ..
                    } | OwnershipEvent::Guard { .. }
                        | OwnershipEvent::Transfer {
                            from: Place::Local(0),
                            ..
                        }
                        | OwnershipEvent::GuardedRelease {
                            place: Place::Local(0),
                            ..
                        }
                )
            )),
        "the rejected direct call must leave the incoming cursor ownerless and untouched"
    );
}

fn assert_unsupported_collection_cursor_preflight(pipeline: &IrPipeline) {
    let stash = pipeline
        .raw_mir
        .iter()
        .find(|function| function.name == "stash_unsupported")
        .expect("the rejected collection caller must retain diagnostic Raw MIR");
    assert!(
        stash.blocks.iter().all(|block| !matches!(
            &block.terminator,
            Terminator::Call { callee, .. }
                if callee == "hew_hashmap_insert_layout" || callee.contains("key_sentinel")
        )),
        "HashMap insertion must reject before lowering its receiver, sentinel key, or cursor value: {:#?}",
        stash.blocks
    );
    assert!(
        stash
            .blocks
            .iter()
            .flat_map(|block| &block.instructions)
            .all(|instruction| !matches!(
                instruction,
                Instr::OwnershipEvent(
                    OwnershipEvent::Mint {
                        place: Place::Local(1),
                        ..
                    } | OwnershipEvent::Guard { .. }
                        | OwnershipEvent::Transfer {
                            from: Place::Local(1),
                            ..
                        }
                        | OwnershipEvent::GuardedRelease {
                            place: Place::Local(1),
                            ..
                        }
                )
            )),
        "the runtime-ingress refusal must not author a cursor owner transition"
    );
}

fn assert_supported_collection_cursor_preflight(pipeline: &IrPipeline) {
    let store = pipeline
        .raw_mir
        .iter()
        .find(|function| function.name == "stash_supported")
        .expect("the rejected supported-cursor store must retain diagnostic Raw MIR");
    assert!(
        store.blocks.iter().all(|block| !matches!(
            &block.terminator,
            Terminator::Call { callee, .. }
                if callee == "hew_hashmap_insert_layout"
                    || callee.contains("supported_key_sentinel")
        )),
        "HashMap insert must reject before lowering its receiver, earlier key operand, or cursor value: {:#?}",
        store.blocks
    );
    assert!(
        store
            .blocks
            .iter()
            .flat_map(|block| &block.instructions)
            .all(|instruction| !matches!(
                instruction,
                Instr::OwnershipEvent(OwnershipEvent::Transfer {
                    from: Place::Local(1),
                    ..
                })
            )),
        "the rejected collection store must leave its supported cursor parameter owned"
    );
}

#[test]
fn unsupported_exact_vec_iter_rejects_before_call_and_collection_mutation() {
    let pipeline = lower_clean_to_checked_mir(VEC_ITER_UNSUPPORTED_RELEASE_SOURCE);
    assert!(
        has_not_yet_implemented(
            &pipeline,
            "VecIter element without an owned cursor release protocol"
        ),
        "an exact cursor must not become an ordinary carrier merely because its release is unwired: {:#?}",
        pipeline.diagnostics
    );
    assert!(
        has_not_yet_implemented(
            &pipeline,
            "VecIter value at a runtime collection storage boundary"
        ),
        "a supported standalone cursor still has no collection-owned storage ABI: {:#?}",
        pipeline.diagnostics
    );
    assert_unsupported_cursor_param_is_inert(&pipeline);
    assert_unsupported_direct_cursor_call_is_inert(&pipeline);
    assert_unsupported_collection_cursor_preflight(&pipeline);
    assert_supported_collection_cursor_preflight(&pipeline);
}
