use super::effect_markers::MarkerSolver;
use super::{CallableOrigin, Checker, EffectBody, EffectGraph, SpanKey};
use crate::env::TypeBindingId;
use crate::module_registry::ModuleRegistry;
use crate::traits::MarkerTrait;
use crate::Ty;
use std::collections::BTreeMap;

fn binding(id: u32, path: &[&str]) -> CallableOrigin {
    CallableOrigin::Binding {
        binding: TypeBindingId(id),
        path: path.iter().map(|field| (*field).to_string()).collect(),
    }
}

#[test]
fn marker_shared_binding_dag_reuses_completed_proofs() {
    let checker = Checker::new(ModuleRegistry::new(vec![]));
    let mut graph = EffectGraph::default();
    graph
        .bindings
        .insert(TypeBindingId(0), CallableOrigin::Typed(Ty::I64));
    for id in 1..=30 {
        graph.bindings.insert(
            TypeBindingId(id),
            CallableOrigin::Aggregate(BTreeMap::from([
                ("left".into(), binding(id - 1, &[])),
                ("right".into(), binding(id - 1, &[])),
            ])),
        );
    }
    let mut solver = MarkerSolver::new(&checker, &graph);
    assert!(solver.proves(&binding(30, &[]), MarkerTrait::Send));
    for id in 0..=30 {
        assert!(solver.proves(&binding(id, &[]), MarkerTrait::Send));
    }
    assert!(solver.proves(&binding(30, &[]), MarkerTrait::Sync));
}

#[test]
fn marker_projections_preserve_sibling_identity() {
    let checker = Checker::new(ModuleRegistry::new(vec![]));
    let mut graph = EffectGraph::default();
    graph.bindings.insert(
        TypeBindingId(0),
        CallableOrigin::Aggregate(BTreeMap::from([
            ("alias".into(), binding(0, &["safe"])),
            ("safe".into(), CallableOrigin::Typed(Ty::I64)),
            ("unsafe".into(), CallableOrigin::Typed(Ty::rc(Ty::I64))),
        ])),
    );
    let mut solver = MarkerSolver::new(&checker, &graph);
    assert!(!solver.proves(&binding(0, &["unsafe"]), MarkerTrait::Send));
    assert!(solver.proves(&binding(0, &["alias"]), MarkerTrait::Send));
    assert!(solver.proves(&binding(0, &["safe"]), MarkerTrait::Sync));
    assert!(!solver.proves(&binding(0, &[]), MarkerTrait::Send));
    assert!(!solver.proves(&binding(0, &["missing"]), MarkerTrait::Send));
}

#[test]
fn marker_projected_alias_cycles_remain_unproven() {
    let checker = Checker::new(ModuleRegistry::new(vec![]));
    let mut graph = EffectGraph::default();
    graph
        .bindings
        .insert(TypeBindingId(0), binding(0, &["loop"]));
    graph.bindings.insert(
        TypeBindingId(1),
        CallableOrigin::Aggregate(BTreeMap::from([
            ("loop".into(), binding(1, &["loop", "loop"])),
            ("safe".into(), CallableOrigin::Typed(Ty::I64)),
        ])),
    );
    let mut solver = MarkerSolver::new(&checker, &graph);
    assert!(!solver.proves(&binding(0, &[]), MarkerTrait::Send));
    assert!(!solver.proves(&binding(1, &["loop"]), MarkerTrait::Send));
    assert!(solver.proves(&binding(1, &["safe"]), MarkerTrait::Send));
}

#[test]
fn marker_finite_projection_growth_reaches_its_leaf() {
    let checker = Checker::new(ModuleRegistry::new(vec![]));
    let mut graph = EffectGraph::default();
    graph.bindings.insert(
        TypeBindingId(0),
        CallableOrigin::Aggregate(BTreeMap::from([
            ("x".into(), binding(0, &["y", "x"])),
            (
                "y".into(),
                CallableOrigin::Aggregate(BTreeMap::from([
                    ("x".into(), binding(0, &["y", "y", "x"])),
                    (
                        "y".into(),
                        CallableOrigin::Aggregate(BTreeMap::from([(
                            "x".into(),
                            CallableOrigin::Typed(Ty::I64),
                        )])),
                    ),
                ])),
            ),
        ])),
    );
    let mut solver = MarkerSolver::new(&checker, &graph);
    assert!(solver.proves(&binding(0, &["x"]), MarkerTrait::Send));
}

fn cyclic_closure_capture(is_send: bool, is_sync: bool) -> (Checker, EffectGraph, CallableOrigin) {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let key = SpanKey::in_module(&(0..1), 0);
    let origin = CallableOrigin::Typed(Ty::Closure {
        capabilities: crate::CallableCapabilities::default(),
        params: Vec::new(),
        ret: Box::new(Ty::I64),
        captures: vec![Ty::rc(Ty::I64)],
        identity: EffectBody::Closure(key.clone()),
    });
    checker.closure_capture_facts.insert(
        key,
        vec![crate::check::ClosureCaptureFact {
            binding_id: TypeBindingId(0),
            value_candidates: crate::check::IndirectCallCandidates::unknown(),
            name: "captured".into(),
            ty: Ty::rc(Ty::I64),
            acquisition: crate::ClosureCaptureAcquisition::Move,
            access: crate::ClosureCaptureAccess::Read,
            consumption: crate::ClosureCaptureConsumption::Retained,
            is_send,
            is_sync,
            use_span: 0..1,
            def_span: None,
        }],
    );
    let mut graph = EffectGraph::default();
    graph.bindings.insert(TypeBindingId(0), origin.clone());
    (checker, graph, origin)
}

#[test]
fn marker_capture_cycles_cannot_prove_themselves() {
    let (checker, graph, origin) = cyclic_closure_capture(false, false);
    let mut solver = MarkerSolver::new(&checker, &graph);
    assert!(!solver.proves(&origin, MarkerTrait::Send));
    assert!(!solver.proves(&origin, MarkerTrait::Sync));
    assert!(!solver.proves(&binding(0, &[]), MarkerTrait::Send));
}

#[test]
fn marker_capture_facts_seed_only_their_own_marker() {
    let (checker, graph, origin) = cyclic_closure_capture(true, false);
    let mut solver = MarkerSolver::new(&checker, &graph);
    assert!(!solver.proves(&origin, MarkerTrait::Sync));
    assert!(solver.proves(&origin, MarkerTrait::Send));
    assert!(solver.proves(&binding(0, &[]), MarkerTrait::Send));
    assert!(!solver.proves(&binding(0, &[]), MarkerTrait::Sync));
}
