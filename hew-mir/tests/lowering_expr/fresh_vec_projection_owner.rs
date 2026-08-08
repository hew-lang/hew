//! Structural ownership pins for fresh owned records cloned by `Vec` indexing
//! and used immediately as record-projection bases.

use hew_mir::{DropKind, ExitPath, Instr, IrPipeline, Place, Terminator};
use hew_types::module_registry::ModuleRegistry;
use hew_types::runtime_call::{RuntimeCallFamily, VecGetElem};
use hew_types::Checker;

fn pipeline(source: &str) -> IrPipeline {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    let output = hew_hir::lower_program(
        &parsed.program,
        &tc_output,
        &hew_hir::ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    let pipeline = hew_mir::lower_hir_module(&output.module);
    assert!(
        pipeline.diagnostics.is_empty(),
        "MIR diagnostics: {:#?}",
        pipeline.diagnostics
    );
    pipeline
}

fn clone_destinations(pipeline: &IrPipeline, fn_name: &str) -> Vec<u32> {
    pipeline
        .raw_mir
        .iter()
        .find(|function| function.name == fn_name)
        .unwrap_or_else(|| panic!("function {fn_name} must be present"))
        .blocks
        .iter()
        .filter_map(|block| match &block.terminator {
            Terminator::Call {
                callee,
                dest: Some(Place::Local(local)),
                ..
            } if callee == "hew_vec_get_clone" => Some(*local),
            _ => None,
        })
        .collect()
}

fn call_count(pipeline: &IrPipeline, fn_name: &str, symbol: &str) -> usize {
    pipeline
        .raw_mir
        .iter()
        .find(|function| function.name == fn_name)
        .unwrap_or_else(|| panic!("function {fn_name} must be present"))
        .blocks
        .iter()
        .filter(|block| {
            matches!(
                &block.terminator,
                Terminator::Call { callee, .. } if callee == symbol
            )
        })
        .count()
}

fn call_builtin(pipeline: &IrPipeline, fn_name: &str, symbol: &str) -> Option<RuntimeCallFamily> {
    pipeline
        .raw_mir
        .iter()
        .find(|function| function.name == fn_name)
        .unwrap_or_else(|| panic!("function {fn_name} must be present"))
        .blocks
        .iter()
        .find_map(|block| match &block.terminator {
            Terminator::Call {
                callee, authority, ..
            } if callee == symbol => authority.runtime_family(),
            _ => None,
        })
}

fn runtime_call_count(pipeline: &IrPipeline, fn_name: &str, symbol: &str) -> usize {
    pipeline
        .raw_mir
        .iter()
        .find(|function| function.name == fn_name)
        .unwrap_or_else(|| panic!("function {fn_name} must be present"))
        .blocks
        .iter()
        .flat_map(|block| block.instructions.iter())
        .filter(|instruction| {
            matches!(instruction, Instr::CallRuntimeAbi(call) if call.symbol() == symbol)
        })
        .count()
}

fn aggregate_neutralize_count(pipeline: &IrPipeline, fn_name: &str) -> usize {
    pipeline
        .raw_mir
        .iter()
        .find(|function| function.name == fn_name)
        .unwrap_or_else(|| panic!("function {fn_name} must be present"))
        .blocks
        .iter()
        .flat_map(|block| block.instructions.iter())
        .filter(|instruction| matches!(instruction, Instr::AggregateProjectionNeutralize { .. }))
        .count()
}

fn clone_drop_exits<'a>(
    pipeline: &'a IrPipeline,
    fn_name: &str,
    local: u32,
) -> Vec<(&'a ExitPath, usize)> {
    pipeline
        .elaborated_mir
        .iter()
        .find(|function| function.name == fn_name)
        .unwrap_or_else(|| panic!("function {fn_name} must be present"))
        .drop_plans
        .iter()
        .filter_map(|(exit, plan)| {
            let count = plan
                .drops
                .iter()
                .filter(|drop| {
                    drop.place == Place::Local(local)
                        && matches!(drop.kind, DropKind::RecordInPlace)
                })
                .count();
            (count != 0).then_some((exit, count))
        })
        .collect()
}

#[test]
fn direct_projection_gets_exactly_one_synthetic_root_but_bound_control_does_not() {
    let p = pipeline(
        r#"
record Holder { items: Vec<string> }

fn direct() -> i64 {
    let v: Vec<Holder> = [];
    v.push(Holder { items: ["left", "right"] });
    v[0].items.len()
}

fn bound() -> i64 {
    let v: Vec<Holder> = [];
    v.push(Holder { items: ["left", "right"] });
    let h = v[0];
    h.items.len()
}

record Scalar { value: i64 }

fn bitcopy_control() -> i64 {
    let v: Vec<Scalar> = [];
    v.push(Scalar { value: 7 });
    v[0].value
}

fn borrowed_control() -> i64 {
    let v: Vec<HashMap<i64, string>> = [];
    let m: HashMap<i64, string> = HashMap::new();
    m.insert(1, "value");
    v.push(m);
    let got = v[0];
    got.len()
}
"#,
    );

    assert_eq!(call_count(&p, "direct", "hew_vec_get_clone"), 1);
    assert_eq!(
        call_builtin(&p, "direct", "hew_vec_get_clone"),
        Some(RuntimeCallFamily::VecGet(VecGetElem::Clone)),
        "ordinary owned indexing must carry its typed clone family on the emitted terminator"
    );
    let direct_locals = clone_destinations(&p, "direct");
    assert_eq!(direct_locals.len(), 1);
    let direct_exits = clone_drop_exits(&p, "direct", direct_locals[0]);
    assert_eq!(direct_exits.len(), 1);
    assert!(matches!(direct_exits[0], (ExitPath::Return { .. }, 1)));

    assert_eq!(call_count(&p, "bound", "hew_vec_get_clone"), 1);
    assert_eq!(
        call_builtin(&p, "bound", "hew_vec_get_clone"),
        Some(RuntimeCallFamily::VecGet(VecGetElem::Clone))
    );
    let bound_locals = clone_destinations(&p, "bound");
    assert_eq!(bound_locals.len(), 1);
    let bound_exits = clone_drop_exits(&p, "bound", bound_locals[0]);
    assert!(
        bound_exits.is_empty(),
        "the ordinary binder adopts the clone and owns its terminal drop"
    );

    assert_eq!(call_count(&p, "bitcopy_control", "hew_vec_get_clone"), 0);
    assert!(clone_destinations(&p, "bitcopy_control").is_empty());

    assert_eq!(
        runtime_call_count(&p, "borrowed_control", "hew_vec_get_owned"),
        1,
        "the nested collection must use the borrow getter, not the fresh composite clone route"
    );
    assert_eq!(call_count(&p, "borrowed_control", "hew_vec_get_clone"), 0);
    assert!(clone_destinations(&p, "borrowed_control").is_empty());
}

#[test]
fn copy_in_projection_assignment_does_not_neutralize_source_but_consuming_assignment_does() {
    let p = pipeline(
        r#"
record Holder { items: Vec<string> }

fn make() -> (Holder, i64) {
    (Holder { items: ["left", "right"] }, 7)
}

fn copy_in() -> i64 {
    var v: Vec<Holder> = [];
    v.push(Holder { items: ["seed"] });
    let pair = make();
    v[0] = pair.0;
    pair.0.items.len()
}

fn consuming() -> i64 {
    var owner = Holder { items: ["seed"] };
    let pair = make();
    owner = pair.0;
    owner.items.len()
}

fn forward() -> i64 {
    let pair = (Holder { items: ["transferred"] }, ["sibling"]);
    var owner = Holder { items: [] };
    owner = pair.0;
    let rebound = owner;
    rebound.items.len() + pair.1.len()
}
"#,
    );

    assert_eq!(
        aggregate_neutralize_count(&p, "copy_in"),
        0,
        "COPY-IN `v[0] = pair.0` must preserve pair.0 for later reads"
    );
    assert_eq!(
        aggregate_neutralize_count(&p, "consuming"),
        1,
        "consuming assignment must transfer pair.0 and neutralize its source slot"
    );
    assert_eq!(
        aggregate_neutralize_count(&p, "forward"),
        1,
        "forward assignment and rebind must neutralize pair.0 exactly once"
    );
}

#[test]
fn panic_edge_never_drops_an_uninitialised_projection_owner() {
    let p = pipeline(
        r#"
record Holder { items: Vec<string> }

fn indexed(i: i64) -> i64 {
    let v: Vec<Holder> = [];
    v.push(Holder { items: ["left", "right"] });
    v[i].items.len()
}
"#,
    );

    let locals = clone_destinations(&p, "indexed");
    assert_eq!(locals.len(), 1);
    let exits = clone_drop_exits(&p, "indexed", locals[0]);
    assert_eq!(exits.len(), 1);
    assert!(matches!(exits[0], (ExitPath::Return { .. }, 1)));
    assert!(
        !exits
            .iter()
            .any(|(exit, _)| matches!(exit, ExitPath::Panic { .. })),
        "the bounds-failure edge precedes the clone and must not drop its uninitialised result"
    );
}

#[test]
fn destructive_update_of_projected_member_transfers_out_of_the_synthetic_root() {
    let p = pipeline(
        r#"
record Inner { label: string, n: i64 }
record Mid { inner: Inner, k: i64 }

fn transfer() -> i64 {
    let v: Vec<Mid> = [];
    v.push(Mid { inner: Inner { label: "a".to_upper(), n: 7 }, k: 3 });
    let u = Inner { label: "b".to_upper(), ..v[0].inner };
    u.label.len()
}
"#,
    );

    assert_eq!(call_count(&p, "transfer", "hew_vec_get_clone"), 1);
    let locals = clone_destinations(&p, "transfer");
    assert_eq!(locals.len(), 1);
    assert!(
        clone_drop_exits(&p, "transfer", locals[0]).is_empty(),
        "the destructive update transfers the projected member and must exclude the parent root"
    );
}

#[test]
fn early_return_back_edge_and_break_each_release_the_live_root_once() {
    let p = pipeline(
        r#"
record Holder { items: Vec<string> }

fn early(flag: bool) -> i64 {
    let v: Vec<Holder> = [];
    v.push(Holder { items: ["left", "right"] });
    if flag {
        return v[0].items.len();
    }
    v[0].items.len()
}

fn loop_edges(stop: bool) -> i64 {
    let v: Vec<Holder> = [];
    v.push(Holder { items: ["left", "right"] });
    var i = 0;
    while i < 2 {
        let n = v[0].items.len();
        if stop {
            break;
        }
        i = i + n;
    }
    i
}
"#,
    );

    let early_locals = clone_destinations(&p, "early");
    assert_eq!(early_locals.len(), 2);
    for local in early_locals {
        let exits = clone_drop_exits(&p, "early", local);
        assert_eq!(exits.len(), 1, "each return-site root must drop once");
        assert!(matches!(exits[0], (ExitPath::Return { .. }, 1)));
    }

    let loop_locals = clone_destinations(&p, "loop_edges");
    assert_eq!(loop_locals.len(), 1);
    let exits = clone_drop_exits(&p, "loop_edges", loop_locals[0]);
    assert_eq!(
        exits
            .iter()
            .filter(|(exit, _)| matches!(exit, ExitPath::Goto { .. }))
            .count(),
        2,
        "the live root must drop once on the loop back-edge and once on break"
    );
    assert!(
        exits.iter().all(|(_, count)| *count == 1),
        "every exit from the live projection region must carry exactly one root drop: {exits:?}"
    );
}
