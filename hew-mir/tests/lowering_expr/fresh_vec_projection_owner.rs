//! Structural ownership pins for fresh owned records cloned by `Vec` indexing
//! and used immediately as record-projection bases.

use hew_mir::{DropKind, ExitPath, Instr, IrPipeline, MirStatement, Place, Terminator};
use hew_types::module_registry::ModuleRegistry;
use hew_types::Checker;

const OWNER_NAME: &str = "__hew_vec_get_clone_projection_base";

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

fn synthetic_binds(pipeline: &IrPipeline, fn_name: &str) -> usize {
    pipeline
        .raw_mir
        .iter()
        .find(|function| function.name == fn_name)
        .unwrap_or_else(|| panic!("function {fn_name} must be present"))
        .blocks
        .iter()
        .flat_map(|block| block.statements.iter())
        .filter(
            |statement| matches!(statement, MirStatement::Bind { name, .. } if name == OWNER_NAME),
        )
        .count()
}

fn synthetic_locals(pipeline: &IrPipeline, fn_name: &str) -> Vec<u32> {
    pipeline
        .raw_mir
        .iter()
        .find(|function| function.name == fn_name)
        .unwrap_or_else(|| panic!("function {fn_name} must be present"))
        .local_names
        .iter()
        .enumerate()
        .filter(|(_, name)| name.as_deref() == Some(OWNER_NAME))
        .map(|(local, _)| {
            u32::try_from(local).expect("MIR local index must fit its u32 identity domain")
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

fn synthetic_drop_exits<'a>(
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
    assert_eq!(synthetic_binds(&p, "direct"), 1);
    let direct_locals = synthetic_locals(&p, "direct");
    assert_eq!(direct_locals.len(), 1);
    let direct_exits = synthetic_drop_exits(&p, "direct", direct_locals[0]);
    assert_eq!(direct_exits.len(), 1);
    assert!(matches!(direct_exits[0], (ExitPath::Return { .. }, 1)));

    assert_eq!(call_count(&p, "bound", "hew_vec_get_clone"), 1);
    assert_eq!(synthetic_binds(&p, "bound"), 0);
    assert!(synthetic_locals(&p, "bound").is_empty());

    assert_eq!(call_count(&p, "bitcopy_control", "hew_vec_get_clone"), 0);
    assert_eq!(synthetic_binds(&p, "bitcopy_control"), 0);
    assert!(synthetic_locals(&p, "bitcopy_control").is_empty());

    assert_eq!(
        runtime_call_count(&p, "borrowed_control", "hew_vec_get_owned"),
        1,
        "the nested collection must use the borrow getter, not the fresh composite clone route"
    );
    assert_eq!(call_count(&p, "borrowed_control", "hew_vec_get_clone"), 0);
    assert_eq!(synthetic_binds(&p, "borrowed_control"), 0);
    assert!(synthetic_locals(&p, "borrowed_control").is_empty());
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

    let locals = synthetic_locals(&p, "indexed");
    assert_eq!(locals.len(), 1);
    let exits = synthetic_drop_exits(&p, "indexed", locals[0]);
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

    assert_eq!(synthetic_binds(&p, "early"), 2);
    for local in synthetic_locals(&p, "early") {
        let exits = synthetic_drop_exits(&p, "early", local);
        assert_eq!(exits.len(), 1, "each return-site root must drop once");
        assert!(matches!(exits[0], (ExitPath::Return { .. }, 1)));
    }

    assert_eq!(synthetic_binds(&p, "loop_edges"), 1);
    let loop_locals = synthetic_locals(&p, "loop_edges");
    assert_eq!(loop_locals.len(), 1);
    let exits = synthetic_drop_exits(&p, "loop_edges", loop_locals[0]);
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
