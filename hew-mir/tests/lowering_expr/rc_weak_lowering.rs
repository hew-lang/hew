use hew_hir::{lower_program, ResolutionCtx};
use hew_mir::{DropKind, Instr, IrPipeline, MirCheck};
use hew_types::module_registry::ModuleRegistry;
use hew_types::{Checker, RcIntrinsicOp};

fn pipeline(source: &str) -> IrPipeline {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let type_output = checker.check_program(&parsed.program);
    assert!(
        type_output.errors.is_empty(),
        "type errors: {:#?}",
        type_output.errors
    );
    let hir = lower_program(
        &parsed.program,
        &type_output,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    assert!(
        hir.diagnostics.is_empty(),
        "HIR diagnostics: {:#?}",
        hir.diagnostics
    );
    hew_mir::lower_hir_module(&hir.module)
}

fn function<'a>(pipeline: &'a IrPipeline, name: &str) -> &'a hew_mir::RawMirFunction {
    pipeline
        .raw_mir
        .iter()
        .find(|function| function.name == name)
        .unwrap_or_else(|| panic!("missing MIR function {name}"))
}

#[test]
fn rc_and_weak_intrinsics_keep_typed_operands_and_drop_kinds() {
    let pipeline = pipeline(
        r"
        fn probe() -> i64 {
            let rc = Rc::new(7);
            let alias = rc.clone();
            let weak = rc.downgrade();
            let weak_alias = weak.clone();
            rc.set(9);
            let upgraded = weak.upgrade();
            alias.strong_count() + rc.weak_count()
        }
        ",
    );
    assert!(
        pipeline.diagnostics.is_empty(),
        "MIR diagnostics: {:#?}",
        pipeline.diagnostics
    );

    let function = function(&pipeline, "probe");
    let ops: Vec<_> = function
        .blocks
        .iter()
        .flat_map(|block| &block.instructions)
        .filter_map(|instr| match instr {
            Instr::RcIntrinsic { op, .. } => Some(op.to_owned()),
            _ => None,
        })
        .collect();
    assert_eq!(
        ops,
        vec![
            RcIntrinsicOp::New,
            RcIntrinsicOp::Clone,
            RcIntrinsicOp::Downgrade,
            RcIntrinsicOp::WeakClone,
            RcIntrinsicOp::Set,
            RcIntrinsicOp::WeakUpgrade,
            RcIntrinsicOp::StrongCount,
            RcIntrinsicOp::WeakCount,
        ]
    );

    let elaborated = pipeline
        .elaborated_mir
        .iter()
        .find(|function| function.name == "probe")
        .expect("missing elaborated probe");
    let kinds: Vec<_> = elaborated
        .drop_plans
        .iter()
        .flat_map(|(_, plan)| plan.drops.iter().map(|drop| drop.kind))
        .collect();
    assert!(kinds.contains(&DropKind::RcRelease), "drops: {kinds:#?}");
    assert!(kinds.contains(&DropKind::WeakRelease), "drops: {kinds:#?}");
}

#[test]
fn ordinary_rc_move_invalidates_source() {
    let pipeline = pipeline(
        r"
        fn probe() -> i64 {
            let rc = Rc::new(7);
            let moved = rc;
            rc.get()
        }
        ",
    );
    let checked = pipeline
        .checked_mir
        .iter()
        .find(|function| function.name == "probe")
        .expect("missing checked probe");
    assert!(
        checked
            .checks
            .iter()
            .any(|check| matches!(check, MirCheck::UseAfterConsume { name, .. } if name == "rc")),
        "checks: {:#?}",
        checked.checks
    );
}

#[test]
fn branch_move_keeps_guarded_rc_release() {
    let pipeline = pipeline(
        r"
        fn probe(take: bool) {
            let rc = Rc::new(7);
            if take {
                let moved = rc;
                moved.strong_count();
            }
        }
        ",
    );
    assert!(
        pipeline.diagnostics.is_empty(),
        "MIR diagnostics: {:#?}",
        pipeline.diagnostics
    );
    let elaborated = pipeline
        .elaborated_mir
        .iter()
        .find(|function| function.name == "probe")
        .expect("missing elaborated probe");
    assert!(
        elaborated.drop_plans.iter().any(|(_, plan)| plan
            .drops
            .iter()
            .any(|drop| drop.kind == DropKind::RcRelease && drop.guard.is_some())),
        "drop plans: {:#?}",
        elaborated.drop_plans
    );
}
