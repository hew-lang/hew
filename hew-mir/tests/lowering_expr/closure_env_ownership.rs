use hew_hir::{lower_program, ResolutionCtx};
use hew_mir::{
    lower_hir_module, ClosureEnvAllocation, ClosureEnvFieldOwnership, DropFnSpec, DropKind, Instr,
    IrPipeline, MirDiagnosticKind, Place,
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
}

#[test]
fn single_source_heap_capture_manifest_owns_moved_source() {
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
    let env_fields: Vec<_> = make_label
        .blocks
        .iter()
        .flat_map(|block| block.instructions.iter())
        .filter_map(|instr| match instr {
            Instr::ClosureEnvInit { fields, .. } => Some(fields),
            _ => None,
        })
        .flat_map(|fields| fields.iter())
        .collect();
    assert!(
        env_fields.iter().any(|field| {
            field.allocation == ClosureEnvAllocation::Heap
                && field.ownership == ClosureEnvFieldOwnership::OwnsMoved
                && field.source_binding.is_some()
        }),
        "heap string capture must own a moved source binding: {env_fields:?}"
    );
}

#[test]
fn owned_carrier_parameter_capture_transfers_before_terminal_drop() {
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
                        && field.ownership == ClosureEnvFieldOwnership::OwnsMoved
                })
                .map(|field| (index, field.src)),
            _ => None,
        })
        .expect("owned parameter capture manifest");

    let parameter = Place::Local(0);
    assert_ne!(
        env_source, parameter,
        "the heap env must receive the transferred owner, not alias the parameter slot"
    );
    let move_index = instructions
        .iter()
        .position(|instr| {
            matches!(instr, Instr::Move { dest, src } if *dest == env_source && *src == parameter)
        })
        .expect("carrier parameter move into the env source");
    let neutralize_indices: Vec<_> = instructions
        .iter()
        .enumerate()
        .filter_map(|(index, instr)| {
            matches!(instr, Instr::NeutralizePayloadSlot { place } if *place == parameter)
                .then_some(index)
        })
        .collect();
    assert_eq!(
        neutralize_indices.len(),
        1,
        "the captured carrier parameter must be neutralized exactly once"
    );
    let terminal_drop_index = instructions
        .iter()
        .position(
            |instr| matches!(instr, Instr::ValueSnapshotDrop { value, .. } if *value == parameter),
        )
        .expect("terminal owned-carrier parameter drop");
    assert!(
        move_index < neutralize_indices[0]
            && neutralize_indices[0] < env_init_index
            && env_init_index < terminal_drop_index,
        "the owner must move and neutralize before env initialization, while the terminal drop remains"
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
            matches!(instr, Instr::NeutralizePayloadSlot { place } if *place == move_src)
        }),
        "the source temp slot must be neutralized so only the callee holds release authority"
    );
}

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
