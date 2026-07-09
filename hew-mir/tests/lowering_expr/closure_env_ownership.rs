use hew_hir::{lower_program, ResolutionCtx};
use hew_mir::{
    lower_hir_module, ClosureEnvAllocation, ClosureEnvFieldOwnership, DropFnSpec, DropKind, Instr,
    IrPipeline, MirDiagnosticKind,
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
