use hew_hir::{lower_program, ResolutionCtx};
use hew_mir::{
    lower_hir_module, ClosureEnvAllocation, ClosureEnvFieldOwnership, ClosureEnvMode, DropFnSpec,
    DropKind, Instr, IrPipeline, MirDiagnosticKind, OwnershipEvent, Place,
};
use hew_types::{module_registry::ModuleRegistry, Checker, ResolvedTy};

fn pipeline_with_tc(source: &str) -> IrPipeline {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    let output = lower_program(
        &parsed.program,
        &tc_output,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    lower_hir_module(&output.module)
}

fn raw_fn<'a>(pl: &'a IrPipeline, name: &str) -> &'a hew_mir::RawMirFunction {
    pl.raw_mir
        .iter()
        .find(|f| f.name == name)
        .unwrap_or_else(|| panic!("raw function `{name}` not found"))
}

fn raw_fn_prefix<'a>(pl: &'a IrPipeline, prefix: &str) -> &'a hew_mir::RawMirFunction {
    pl.raw_mir
        .iter()
        .find(|f| f.name.starts_with(prefix))
        .unwrap_or_else(|| panic!("raw function prefix `{prefix}` not found"))
}

#[test]
fn stack_env_capture_manifest_is_borrow_only_and_source_drop_remains() {
    let pl = pipeline_with_tc(
        r#"
type Holder {
    left: string;
    right: string;
}

fn run_loop(frames: i64) -> i64 {
    var total: i64 = 0;
    for i in 0..frames {
        let h = Holder {
            left: "left-payload".to_upper(),
            right: "right-payload".to_upper(),
        };
        let f = || h.left.len() + h.right.len() + i;
        total = total + f();
    }
    total
}
"#,
    );
    assert!(
        pl.diagnostics.is_empty(),
        "unexpected diagnostics: {:?}",
        pl.diagnostics
    );
    let run_loop = raw_fn(&pl, "run_loop");
    let env_inits: Vec<_> = run_loop
        .blocks
        .iter()
        .flat_map(|block| block.instructions.iter())
        .filter_map(|instr| match instr {
            Instr::ClosureEnvInit { fields, .. } => Some(fields),
            _ => None,
        })
        .collect();
    assert_eq!(env_inits.len(), 1, "expected one stack closure env init");
    assert!(
        env_inits[0].iter().all(|field| {
            field.allocation == ClosureEnvAllocation::Stack
                && field.ownership == ClosureEnvFieldOwnership::BorrowsOnly
        }),
        "stack env fields must be borrow-only: {:?}",
        env_inits[0]
    );

    let has_holder_drop = pl
        .elaborated_mir
        .iter()
        .find(|f| f.name == "run_loop")
        .expect("run_loop elaborated")
        .drop_plans
        .iter()
        .flat_map(|(_, plan)| plan.drops.iter())
        .any(|drop| matches!(drop.kind, DropKind::RecordInPlace));
    assert!(
        has_holder_drop,
        "stack-env source Holder must keep a RecordInPlace drop"
    );
    assert!(
        run_loop
            .blocks
            .iter()
            .flat_map(|block| block.instructions.iter())
            .all(|instr| !matches!(
                instr,
                Instr::OwnershipEvent(OwnershipEvent::Mint {
                    ty: ResolvedTy::Function { .. } | ResolvedTy::Closure { .. },
                    ..
                })
            )),
        "a stack-env closure pair must not mint heap-release authority"
    );
    assert!(
        pl.elaborated_mir
            .iter()
            .find(|f| f.name == "run_loop")
            .expect("run_loop elaborated")
            .drop_plans
            .iter()
            .flat_map(|(_, plan)| plan.drops.iter())
            .all(|drop| !matches!(drop.kind, DropKind::ClosurePair)),
        "a stack-env closure pair must never run the heap-env destructor"
    );
}

#[test]
fn captureless_local_closure_has_no_pair_release_authority() {
    let pl = pipeline_with_tc(
        r"
fn main() -> i64 {
    let f = || 42;
    f()
}
",
    );
    assert!(
        pl.diagnostics.is_empty(),
        "unexpected diagnostics: {:?}",
        pl.diagnostics
    );
    let main = raw_fn(&pl, "main");
    assert!(main
        .blocks
        .iter()
        .flat_map(|block| &block.instructions)
        .any(|instr| matches!(
            instr,
            Instr::MakeClosure {
                env_mode: ClosureEnvMode::Stack | ClosureEnvMode::Null,
                env_ownership,
                ..
            } if env_ownership.is_empty()
        )));
    assert!(
        pl.elaborated_mir
            .iter()
            .find(|f| f.name == "main")
            .expect("main elaborated")
            .drop_plans
            .iter()
            .flat_map(|(_, plan)| plan.drops.iter())
            .all(|drop| !matches!(drop.kind, DropKind::ClosurePair)),
        "a captureless local pair has no environment allocation to free"
    );
}

#[test]
fn heap_closure_call_result_keeps_pair_release_authority() {
    let pl = pipeline_with_tc(
        r#"
fn make() -> fn() -> i64 {
    let label = "heap-owned".to_upper();
    || label.len()
}

fn main() -> i64 {
    let f = make();
    f()
}
"#,
    );
    assert!(
        pl.diagnostics.is_empty(),
        "unexpected diagnostics: {:?}",
        pl.diagnostics
    );
    let make = raw_fn(&pl, "make");
    assert!(make
        .blocks
        .iter()
        .flat_map(|block| &block.instructions)
        .any(|instr| matches!(
            instr,
            Instr::MakeClosure {
                env_mode: ClosureEnvMode::HeapBox,
                ..
            }
        )));
    assert!(
        pl.elaborated_mir
            .iter()
            .find(|f| f.name == "main")
            .expect("main elaborated")
            .drop_plans
            .iter()
            .flat_map(|(_, plan)| plan.drops.iter())
            .any(|drop| matches!(drop.kind, DropKind::ClosurePair)),
        "a heap closure call result must retain one pair release authority"
    );
}

/// A checker-`Borrow` (read-only) heap capture of a `string` is a RETAINED
/// SHARE: the env field manifest records `OwnsClonedOrRetained`, an
/// unconditional `StringRetain` mints the env's co-owner before the env init,
/// and the source binding is NOT consumed (its own scope-exit owner survives).
/// The env free thunk releases the env's share — two owners, two releases.
#[test]
fn single_source_heap_borrow_capture_manifest_is_retained_share() {
    let pl = pipeline_with_tc(
        r#"
fn make_label(n: i64) -> fn() -> i64 {
    let label = "row-payload-seed".to_upper();
    || label.len() + n
}
"#,
    );
    assert!(
        pl.diagnostics.is_empty(),
        "unexpected diagnostics: {:?}",
        pl.diagnostics
    );
    let make_label = raw_fn(&pl, "make_label");
    let instructions: Vec<_> = make_label
        .blocks
        .iter()
        .flat_map(|block| block.instructions.iter())
        .collect();
    let (env_init_index, retained_src, retained_binding) = instructions
        .iter()
        .enumerate()
        .find_map(|(index, instr)| match instr {
            Instr::ClosureEnvInit { fields, .. } => fields
                .iter()
                .find(|field| {
                    field.allocation == ClosureEnvAllocation::Heap
                        && field.ownership == ClosureEnvFieldOwnership::OwnsClonedOrRetained
                })
                .and_then(|field| {
                    field
                        .source_binding
                        .map(|binding| (index, field.src, binding))
                }),
            _ => None,
        })
        .expect("heap Borrow string capture must carry a retained-share manifest");
    let retain_index = instructions
        .iter()
        .position(
            |instr| matches!(instr, Instr::StringRetain { value, .. } if *value == retained_src),
        )
        .expect("the env co-owner must be minted with a StringRetain");
    assert!(
        retain_index < env_init_index,
        "the retain must precede the env init that byte-copies the handle"
    );
    assert!(
        !make_label
            .blocks
            .iter()
            .flat_map(|block| block.statements.iter())
            .any(|stmt| matches!(
                stmt,
                hew_mir::MirStatement::Use {
                    binding,
                    intent: hew_hir::IntentKind::Consume,
                    ..
                } if *binding == retained_binding
            )),
        "a retained-share capture must not consume the source binding"
    );
}

/// A read-only (`Borrow`) capture of an owned call-carrier record parameter is
/// a RETAINED SHARE: the env field manifest records `OwnsClonedOrRetained`, an
/// `AggregateBorrowedIngress` string retain mints the env's co-owner over the
/// record's string leaves before the env init, and the parameter is NOT
/// moved-and-neutralized — its own terminal carrier drop survives as the
/// second, independent release.
#[test]
fn owned_carrier_parameter_borrow_capture_retains_and_keeps_terminal_drop() {
    let pl = pipeline_with_tc(
        r#"
type Holder {
    left: string;
    right: string;
}

fn make_reader(h: Holder, n: i64) -> fn() -> i64 {
    || h.left.len() + h.right.len() + n
}

fn run_once() -> i64 {
    let h = Holder {
        left: "left-payload".to_upper(),
        right: "right-payload".to_upper(),
    };
    let f = make_reader(h, 1);
    f()
}
"#,
    );
    assert!(
        pl.diagnostics.is_empty(),
        "unexpected diagnostics: {:?}",
        pl.diagnostics
    );

    let make_reader = raw_fn(&pl, "make_reader");
    let instructions: Vec<_> = make_reader
        .blocks
        .iter()
        .flat_map(|block| block.instructions.iter())
        .collect();
    let (env_init_index, env_source) = instructions
        .iter()
        .enumerate()
        .find_map(|(index, instr)| match instr {
            Instr::ClosureEnvInit { fields, .. } => fields
                .iter()
                .find(|field| {
                    field.source_is_parameter
                        && field.ownership == ClosureEnvFieldOwnership::OwnsClonedOrRetained
                })
                .map(|field| (index, field.src)),
            _ => None,
        })
        .expect("retained-share parameter capture manifest");

    let parameter = Place::Local(0);
    let retain_index = instructions
        .iter()
        .position(|instr| {
            matches!(
                instr,
                Instr::StringRetain {
                    value,
                    condition: hew_mir::StringRetainCondition::AggregateBorrowedIngress,
                } if *value == env_source
            )
        })
        .expect("the env co-owner must be minted with an aggregate string retain");
    assert!(
        retain_index < env_init_index,
        "the aggregate retain must precede the env init that byte-copies the record"
    );
    assert!(
        !instructions.iter().any(|instr| {
            matches!(instr, Instr::NeutralizePayloadSlot { place, .. } if *place == parameter)
        }),
        "a retained-share capture must not neutralize the parameter slot"
    );
    assert!(
        instructions.iter().any(|instr| {
            matches!(instr, Instr::ValueSnapshotDrop { value, .. } if *value == parameter)
        }),
        "the parameter's terminal owned-carrier drop must survive as the second release"
    );
}

/// A fresh call-result temp (`wrap(i)` feeding `make_handler` with no binding
/// in between) is a by-construction unique last use of the fn-carrier it
/// holds. Inside a loop its single terminator use flows around the back edge
/// through its own defining block, so a liveness that does not kill
/// call-terminator dests misclassifies it as live-out and rejects the shape
/// (`E_NOT_YET_IMPLEMENTED` "live owned call-carrier"). The carrier pass must
/// accept it and transfer through the move-and-neutralize funnel: the temp
/// moves into a fresh owner handed to the callee and the source slot is
/// neutralized so no second release authority survives.
#[test]
fn fresh_call_result_carrier_in_loop_transfers_and_neutralizes() {
    let pl = pipeline_with_tc(
        r"
type Handler {
    action: fn(i64) -> i64;
}

fn make_adder(n: i64) -> fn(i64) -> i64 {
    |x: i64| x + n
}

fn wrap(n: i64) -> fn(i64) -> i64 {
    make_adder(n)
}

fn make_handler(f: fn(i64) -> i64) -> Handler {
    Handler { action: f }
}

fn run_loop(frames: i64) -> i64 {
    var total: i64 = 0;
    for i in 0..frames {
        let h = make_handler(wrap(i));
        total = total + h.action(1);
    }
    total
}
",
    );
    assert!(
        pl.diagnostics.is_empty(),
        "the fresh call-result carrier is a unique last use and must lower: {:?}",
        pl.diagnostics
    );

    let run_loop = raw_fn(&pl, "run_loop");
    let carrier_block = run_loop
        .blocks
        .iter()
        .find(|block| {
            matches!(
                &block.terminator,
                hew_mir::Terminator::Call { callee, .. } if callee == "make_handler"
            )
        })
        .expect("make_handler call block");
    let hew_mir::Terminator::Call { args, .. } = &carrier_block.terminator else {
        unreachable!("guarded by the find above");
    };
    let transferred_arg = *args.first().expect("make_handler takes the carrier");
    let move_src = carrier_block
        .instructions
        .iter()
        .find_map(|instr| match instr {
            Instr::Move { dest, src } if *dest == transferred_arg => Some(*src),
            _ => None,
        })
        .expect("carrier temp must move into the fresh owner the callee receives");
    assert!(
        carrier_block.instructions.iter().any(|instr| {
            matches!(instr, Instr::NeutralizePayloadSlot { place, .. } if *place == move_src)
        }),
        "the source temp slot must be neutralized so only the callee holds release authority"
    );
}

/// The `label` sharing between the two closures is now legal (each env takes a
/// retained share), but storing a CAPTURING closure pair into a record field
/// still fails closed (`UseAfterConsume` on `a`/`b` at the `RecordInit`) — the
/// closure-pair-into-record transfer has no ownership protocol yet. This pin
/// holds the fail-closed line at that seam.
#[test]
fn shared_source_heap_capture_fails_closed_before_codegen() {
    let pl = pipeline_with_tc(
        r#"
type PairFns {
    a: fn() -> i64;
    b: fn() -> i64;
}

fn make_pair(n: i64) -> PairFns {
    let label: string = "row-payload-seed".to_upper();
    let a = || label.len() + n;
    let b = || label.len() + n;
    PairFns { a: a, b: b }
}
"#,
    );
    assert!(
        pl.diagnostics
            .iter()
            .any(|diag| matches!(diag.kind, MirDiagnosticKind::UseAfterConsume { .. })),
        "shared-source capture must fail closed with UseAfterConsume; diagnostics: {:?}",
        pl.diagnostics
    );
}

#[test]
fn closure_shim_record_field_retains_get_balancing_inline_drops() {
    let pl = pipeline_with_tc(
        r"
type Holder {
    left: string;
    right: string;
}

fn make_reader(h: Holder) -> fn() -> i64 {
    || h.left.len() + h.right.len()
}
",
    );
    assert!(
        pl.diagnostics.is_empty(),
        "unexpected diagnostics: {:?}",
        pl.diagnostics
    );
    let shim = raw_fn_prefix(&pl, "__hew_closure_invoke_make_reader_");
    let inline_string_drops = shim
        .blocks
        .iter()
        .flat_map(|block| block.instructions.iter())
        .filter(|instr| {
            matches!(
                instr,
                Instr::Drop {
                    ty: ResolvedTy::String,
                    drop_fn: Some(DropFnSpec::Release("hew_string_drop")),
                    ..
                }
            )
        })
        .count();
    assert_eq!(
        inline_string_drops, 2,
        "two env-loaded record string fields need two balancing inline drops"
    );
}

#[test]
fn closure_shim_string_capture_load_gets_balancing_inline_drop() {
    let pl = pipeline_with_tc(
        r"
fn make_reader(label: string) -> fn() -> i64 {
    || label.len()
}
",
    );
    assert!(
        pl.diagnostics.is_empty(),
        "unexpected diagnostics: {:?}",
        pl.diagnostics
    );
    let shim = raw_fn_prefix(&pl, "__hew_closure_invoke_make_reader_");
    let has_env_string_load = shim
        .blocks
        .iter()
        .flat_map(|block| block.instructions.iter())
        .any(|instr| matches!(instr, Instr::ClosureEnvFieldLoad { dest, .. } if matches!(dest, hew_mir::Place::Local(local) if shim.locals.get(*local as usize) == Some(&ResolvedTy::String))));
    assert!(has_env_string_load, "expected string ClosureEnvFieldLoad");
    let inline_string_drops = shim
        .blocks
        .iter()
        .flat_map(|block| block.instructions.iter())
        .filter(|instr| {
            matches!(
                instr,
                Instr::Drop {
                    ty: ResolvedTy::String,
                    drop_fn: Some(DropFnSpec::Release("hew_string_drop")),
                    ..
                }
            )
        })
        .count();
    assert_eq!(
        inline_string_drops, 1,
        "retained string capture load needs one balancing inline drop"
    );
}
