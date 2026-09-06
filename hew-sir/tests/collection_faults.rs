use hew_hir::{lower_program_host_target, ResolutionCtx};
use hew_sir::{
    lower_module, verify_module, CallResult, CallUnwind, SemModule, SemTerminator,
    SirDiagnosticKind, SirLoweringStatus,
};
use hew_types::{module_registry::ModuleRegistry, Checker};

fn collection_calls() -> SemModule {
    let parsed = hew_parser::parse(
        r#"
        fn main() -> i64 {
            var values: HashMap<i64, string> = HashMap.new();
            values.insert(1, "one");
            let read = values.get(1);
            let member = values.contains_key(1);
            let indexed = values[1];
            let removed = values.remove(1);
            var members: HashSet<string> = HashSet.new();
            let inserted = members.insert("one");
            let present = members.contains("one");
            let deleted = members.remove("one");
            0
        }
        "#,
    );
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let mut checker = Checker::new(ModuleRegistry::new(Vec::new()));
    let facts = checker.check_program(&parsed.program);
    assert!(facts.errors.is_empty(), "{:?}", facts.errors);
    let hir = lower_program_host_target(&parsed.program, &facts, &ResolutionCtx);
    assert!(hir.diagnostics.is_empty(), "{:?}", hir.diagnostics);
    let lowered = lower_module(&hir.module, &facts);
    assert!(
        lowered
            .statuses
            .iter()
            .any(|status| status.name == "main"
                && matches!(status.status, SirLoweringStatus::Lowered)),
        "{:?}",
        lowered.statuses
    );
    assert!(
        verify_module(&lowered.module).is_empty(),
        "{:?}",
        verify_module(&lowered.module)
    );
    lowered.module
}

#[test]
fn collection_callback_failures_end_in_existing_fault_propagation() {
    use hew_types::{
        runtime_call::{MapValueOp as Map, SetValueOp as Set},
        RuntimeCallFamily,
    };
    let module = collection_calls();
    let mut callbacks = Vec::new();
    for function in &module.functions {
        for block in &function.blocks {
            let SemTerminator::RtCall { family, unwind, .. } = &block.terminator else {
                continue;
            };
            if !family.semantic_contract().unwrap().propagates_fault() {
                continue;
            }
            let CallUnwind::Cleanup(edge) = unwind else {
                panic!("{family:?} requires cleanup")
            };
            assert!(
                matches!(
                    function.blocks[edge.target.0 as usize].terminator,
                    SemTerminator::ResumeUnwind
                ),
                "{family:?}"
            );
            callbacks.push(*family);
        }
    }
    for family in [
        Map::Insert,
        Map::Get,
        Map::ContainsKey,
        Map::Index,
        Map::Remove,
    ]
    .map(RuntimeCallFamily::Map)
    .into_iter()
    .chain([Set::Insert, Set::Contains, Set::Remove].map(RuntimeCallFamily::Set))
    {
        assert!(
            callbacks.contains(&family),
            "fixture must exercise {family:?}"
        );
    }
}

#[test]
fn collection_failure_cannot_replace_or_abandon_its_callback_fault() {
    let module = collection_calls();
    let function = module
        .functions
        .iter()
        .position(|function| function.declaration.full_path() == "main")
        .unwrap();
    let (cleanup, raw_result) = module.functions[function]
        .blocks
        .iter()
        .find_map(|block| {
            let SemTerminator::RtCall {
                family,
                unwind: CallUnwind::Cleanup(edge),
                result: CallResult::Value(result),
                ..
            } = &block.terminator
            else {
                return None;
            };
            family
                .semantic_contract()
                .unwrap()
                .propagates_fault()
                .then_some((edge.target, result.id))
        })
        .unwrap();
    for terminal in [
        SemTerminator::Return { value: None },
        SemTerminator::Trap {
            kind: hew_sir::TrapKind::IndexOutOfBounds,
        },
    ] {
        let mut invalid = module.clone();
        invalid.functions[function].blocks[cleanup.0 as usize].terminator = terminal;
        assert!(verify_module(&invalid)
            .iter()
            .any(|diagnostic| matches!(diagnostic.kind, SirDiagnosticKind::FaultLifetime { .. })));
    }
    let mut invalid = module;
    invalid.functions[function].blocks[cleanup.0 as usize].terminator = SemTerminator::Return {
        value: Some(hew_sir::BoundaryOperand {
            operand: hew_sir::Operand { value: raw_result },
            decision: hew_sir::BoundaryDecision::Move,
        }),
    };
    assert!(verify_module(&invalid).iter().any(|diagnostic| matches!(
        diagnostic.kind,
        SirDiagnosticKind::InvalidCallResultUse { .. }
    )));
}
