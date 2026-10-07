//! Fault exits share one cleanup ladder per body. A fault site jumps to the
//! rung for its remaining cleanup, so sites that owe the same releases reach
//! the same blocks and a body unwinds through one `resume_unwind`.

use hew_hir::{lower_program_host_target, ResolutionCtx};
use hew_sir::{
    verify_module, BlockId, SemBlock, SemFunction, SemModule, SemOpKind, SemTerminator,
    SirLoweringStatus, SuspendKind,
};
use hew_types::{module_registry::ModuleRegistry, Checker};

const PRELUDE: &str = r#"#[resource]
type Lease {
    name: string;
}

impl Lease {
    fn close(consume self) {
        println("closed " + self.name);
    }
}

actor Store {
    receive fn get(key: string) -> i64 {
        key.len()
    }
}

fn join(text: string, n: i64) -> i64 {
    text.len() + n
}
"#;

/// Lower `body` with a `main` that runs `call` against a spawned `store`, so
/// the function under test is demanded.
fn lower_source(body: &str, call: &str) -> SemModule {
    let source = format!(
        "{PRELUDE}\n{body}\n\nfn main() {{\n    let store = spawn Store;\n    {call};\n}}\n"
    );
    let parsed = hew_parser::parse(&source);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let mut checker = Checker::new(ModuleRegistry::new(Vec::new()));
    let facts = checker.check_program(&parsed.program);
    assert!(facts.errors.is_empty(), "{:?}", facts.errors);
    let hir = lower_program_host_target(&parsed.program, &facts, &ResolutionCtx);
    assert!(hir.diagnostics.is_empty(), "{:?}", hir.diagnostics);
    let lowered = hew_sir::lower_module(&hir.module, &facts);
    for status in &lowered.statuses {
        assert!(
            !matches!(status.status, SirLoweringStatus::Unsupported { .. }),
            "{}: {:?}",
            status.name,
            status.status
        );
    }
    let errors = verify_module(&lowered.module);
    assert!(errors.is_empty(), "{errors:#?}");
    lowered.module
}

fn function<'m>(module: &'m SemModule, suffix: &str) -> &'m SemFunction {
    module
        .functions
        .iter()
        .find(|function| function.name.ends_with(suffix))
        .unwrap_or_else(|| panic!("no function ends with {suffix}"))
}

fn block(function: &SemFunction, id: BlockId) -> &SemBlock {
    function
        .blocks
        .iter()
        .find(|block| block.id == id)
        .expect("block")
}

/// The block a fault landing jumps to: the top rung of its cleanup.
fn rung(function: &SemFunction, landing: BlockId) -> BlockId {
    match &block(function, landing).terminator {
        SemTerminator::Goto(edge) => edge.target,
        other => panic!("fault landing bb{} is not a jump: {other:?}", landing.0),
    }
}

/// `(cancel rung, unwind rung)` of every ask, in block order.
fn ask_rungs(function: &SemFunction) -> Vec<(BlockId, BlockId)> {
    function
        .blocks
        .iter()
        .filter_map(|block| match &block.terminator {
            SemTerminator::Suspend {
                kind: SuspendKind::Ask { .. },
                cancel,
                unwind,
                ..
            } => Some((rung(function, cancel.target), rung(function, unwind.target))),
            _ => None,
        })
        .collect()
}

fn resume_unwinds(function: &SemFunction) -> usize {
    function
        .blocks
        .iter()
        .filter(|block| matches!(block.terminator, SemTerminator::ResumeUnwind { .. }))
        .count()
}

/// The blocks a rung chain visits from `start`, following plain jumps.
fn chain(function: &SemFunction, start: BlockId) -> Vec<BlockId> {
    let mut visited = vec![start];
    let mut current = start;
    while let SemTerminator::Goto(edge) = &block(function, current).terminator {
        current = edge.target;
        visited.push(current);
    }
    visited
}

#[test]
fn fault_sites_with_the_same_live_owners_share_one_rung() {
    let module = lower_source(
        r#"fn pair(first: Store, second: Store) -> Result<i64, ActorError> {
    let lease = Lease { name: "a" };
    let one = first.get("k")?;
    let two = second.get("jj")?;
    .Ok(one + two)
}"#,
        "let _ = pair(store, store)",
    );
    let pair = function(&module, "pair");
    let rungs = ask_rungs(pair);
    assert_eq!(rungs.len(), 2);
    let shared = rungs[0].0;
    assert!(
        rungs
            .iter()
            .all(|&(cancel, unwind)| cancel == shared && unwind == shared),
        "both asks' cancel and unwind edges enter the same rung: {rungs:?}"
    );
    assert_eq!(resume_unwinds(pair), 1, "one body, one unwind");
    let ends = chain(pair, shared)
        .into_iter()
        .flat_map(|id| &block(pair, id).ops)
        .filter(|op| matches!(op.kind, SemOpKind::EndLifetime { .. }))
        .count();
    assert_eq!(ends, 1, "the shared chain ends the lease once");
}

#[test]
fn a_site_with_one_more_temporary_enters_one_rung_above_the_shared_suffix() {
    let module = lower_source(
        r#"fn temp(store: Store) -> Result<i64, ActorError> {
    let lease = Lease { name: "a" };
    let one = store.get("k")?;
    .Ok(join(f"t{one}", store.get("j")?))
}"#,
        "let _ = temp(store)",
    );
    let temp = function(&module, "temp");
    let rungs = ask_rungs(temp);
    assert_eq!(rungs.len(), 2);
    let (without, with) = (chain(temp, rungs[0].0), chain(temp, rungs[1].0));
    let releases = |chain: &[BlockId], kind: fn(&SemOpKind) -> bool| {
        chain
            .iter()
            .filter(|&&id| block(temp, id).ops.iter().any(|op| kind(&op.kind)))
            .copied()
            .collect::<Vec<_>>()
    };
    let destroys = |kind: &SemOpKind| matches!(kind, SemOpKind::DestroyValue { .. });
    let ends = |kind: &SemOpKind| matches!(kind, SemOpKind::EndLifetime { .. });
    assert!(releases(&without, destroys).is_empty());
    assert_eq!(
        releases(&with, destroys).len(),
        1,
        "only the second ask still owns the temporary: {with:?}"
    );
    let lease = releases(&without, ends);
    assert_eq!(lease.len(), 1);
    assert_eq!(
        releases(&with, ends),
        lease,
        "after the temporary, the second ask continues into the first ask's lease rung"
    );
    assert_eq!(resume_unwinds(temp), 1);
}

#[test]
fn a_defer_rung_dispatches_its_outcome_and_no_other_fault_rung_does() {
    let module = lower_source(
        r#"fn deferred(store: Store) -> Result<i64, ActorError> {
    let lease = Lease { name: "a" };
    defer println("deferred");
    let one = store.get("k")?;
    let two = store.get("j")?;
    .Ok(one + two)
}"#,
        "let _ = deferred(store)",
    );
    let deferred = function(&module, "deferred");
    let rungs = ask_rungs(deferred);
    assert!(rungs
        .iter()
        .all(|&(cancel, unwind)| cancel == rungs[0].0 && unwind == rungs[0].0));
    assert_eq!(resume_unwinds(deferred), 1);
    let after_finish = deferred
        .blocks
        .iter()
        .filter_map(|block| match &block.terminator {
            SemTerminator::FinishDefer { next, .. } => Some(next.target),
            _ => None,
        })
        .collect::<Vec<_>>();
    for block in &deferred.blocks {
        if let SemTerminator::CleanupDispatch { normal, fault } = &block.terminator {
            if normal.target == fault.target {
                assert!(
                    after_finish.contains(&block.id),
                    "bb{} dispatches a fault path that ran no defer",
                    block.id.0
                );
            }
        }
    }
}

#[test]
fn a_defer_reading_rewritten_places_keeps_one_rung() {
    // The deferred body reads `n` and `text` through their places, so every
    // ask runs the same deferred body whatever has been stored there since.
    let module = lower_source(
        r#"fn track(store: Store, label: string) -> Result<i64, ActorError> {
    var n = 1;
    var text = label;
    defer println(f"{text} {n}");
    let one = store.get("k")?;
    n = 2;
    let two = store.get("jj")?;
    text = "replaced";
    let three = store.get("jjj")?;
    .Ok(one + two + three + n)
}"#,
        "let _ = track(store, \"label\")",
    );
    let track = function(&module, "track");
    let rungs = ask_rungs(track);
    assert_eq!(rungs.len(), 3);
    assert!(
        rungs
            .iter()
            .all(|&(cancel, unwind)| cancel == rungs[0].0 && unwind == rungs[0].0),
        "{rungs:?}"
    );
    let defers = track
        .blocks
        .iter()
        .filter(|block| matches!(block.terminator, SemTerminator::EnterDefer { .. }))
        .count();
    // One deferred body for the shared fault rung, one per ordinary exit:
    // three `?` returns and the final value.
    assert_eq!(defers, 5);
    assert_eq!(resume_unwinds(track), 1);
}
