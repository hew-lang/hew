//! Local storage contracts, constructed independently of lexical source lowering.

use hew_hir::{lower_program_host_target, ResolutionCtx};
use hew_sir::{
    place_lifetimes, place_plan, verify_module, BlockId, CleanupMode, Edge, OpId, Operand, OwnKind,
    OwnerRoot, PlaceBase, PlaceDecl, PlaceId, PlaceOrigin, Provenance, SemBlock, SemFunction,
    SemModule, SemOp, SemOpKind, SemTerminator, SirDiagnosticKind, SirLoweringDemand, ValueDef,
    ValueId,
};
use hew_types::{module_registry::ModuleRegistry, Checker, ResolvedTy};

fn fixture(source: &str) -> SemModule {
    let parsed = hew_parser::parse(source);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let checked = Checker::new(ModuleRegistry::new(vec![])).check_program(&parsed.program);
    assert!(checked.errors.is_empty(), "{:?}", checked.errors);
    let hir = lower_program_host_target(&parsed.program, &checked, &ResolutionCtx);
    assert!(hir.diagnostics.is_empty(), "{:?}", hir.diagnostics);
    let lowered =
        hew_sir::lower_module_with_demand(&hir.module, &checked, SirLoweringDemand::EveryCallable);
    assert!(
        verify_module(&lowered.module).is_empty(),
        "{:?}",
        lowered.statuses
    );
    assert!(
        lowered
            .module
            .functions
            .iter()
            .any(|function| function.declaration.full_path() == "probe"),
        "{:?}",
        lowered.statuses
    );
    let mut module = lowered.module;
    let function = probe(&mut module);
    assert_eq!(function.params[0].value, ValueId(0));
    assert_eq!(function.params[1].value, ValueId(1));
    function.places = vec![PlaceDecl {
        id: PlaceId(0),
        ty: function.params[0].ty.clone(),
        origin: PlaceOrigin::Local,
    }];
    function.bindings.clear();
    function.blocks = vec![block(0, vec![alloc(0), init(0, 0), end(0)], done())];
    // Construct a unit-returning model body while retaining the checker's exact
    // declared type facts. This also lets linear fixtures model rejected exits.
    function.return_ty = ResolvedTy::Unit;
    let callable = function.callable;
    module
        .callables
        .iter_mut()
        .find(|entry| entry.id == callable)
        .unwrap()
        .signature
        .return_ty = ResolvedTy::Unit;
    module
}

fn strings() -> SemModule {
    fixture("fn probe(consume owner: string, flag: bool) {} fn main() {}")
}

fn linear() -> SemModule {
    let mut module = fixture(
        "type Token { payload: string } fn probe(consume owner: Token, flag: bool) {} fn main() {}",
    );
    let ty = probe(&mut module).params[0].ty.clone();
    // Construct the linear contract directly so malformed cleanup paths
    // reach the verifier independently of source admission.
    let row = module
        .type_facts
        .get_mut(&hew_types::TypeInstanceKey(ty.clone()))
        .unwrap();
    row.class = hew_types::ValueClass::Linear;
    row.clone = hew_types::CloneKind::None;
    module
        .aggregate_shapes
        .iter_mut()
        .find(|shape| shape.aggregate_ty == ty)
        .unwrap()
        .marker = hew_types::DeclarationMarker::Linear;
    module
}

fn probe(module: &mut SemModule) -> &mut SemFunction {
    module
        .functions
        .iter_mut()
        .find(|function| function.declaration.full_path() == "probe")
        .unwrap()
}

fn operand(value: u32) -> Operand {
    Operand {
        value: ValueId(value),
    }
}
fn op(kind: SemOpKind) -> SemOp {
    SemOp {
        id: OpId(0),
        kind,
        results: vec![],
        provenance: Provenance::Synthesized,
    }
}
fn alloc(place: u32) -> SemOp {
    op(SemOpKind::AllocPlace {
        place: PlaceId(place),
    })
}
fn init(place: u32, value: u32) -> SemOp {
    op(SemOpKind::StoreInit {
        place: PlaceId(place),
        value: operand(value),
    })
}
fn assign(place: u32, value: u32) -> SemOp {
    op(SemOpKind::StoreAssign {
        place: PlaceId(place),
        value: operand(value),
    })
}
fn end(place: u32) -> SemOp {
    op(SemOpKind::EndLifetime {
        place: PlaceId(place),
    })
}
fn destroy(value: u32) -> SemOp {
    op(SemOpKind::DestroyValue {
        value: operand(value),
    })
}
fn end_borrow(value: u32) -> SemOp {
    op(SemOpKind::EndBorrow {
        borrow: operand(value),
    })
}
fn load(kind: SemOpKind, value: u32, ty: ResolvedTy, own: OwnKind) -> SemOp {
    SemOp {
        results: vec![ValueDef {
            id: ValueId(value),
            ty,
            own,
        }],
        ..op(kind)
    }
}
fn take(place: u32, value: u32, ty: ResolvedTy, own: OwnKind) -> SemOp {
    load(
        SemOpKind::LoadTake {
            place: PlaceId(place),
        },
        value,
        ty,
        own,
    )
}
fn edge(target: u32) -> Edge {
    Edge {
        target: BlockId(target),
        args: vec![],
    }
}
fn block(id: u32, ops: Vec<SemOp>, terminator: SemTerminator) -> SemBlock {
    SemBlock {
        id: BlockId(id),
        args: vec![],
        ops,
        terminator,
    }
}
fn done() -> SemTerminator {
    SemTerminator::Return { value: None }
}
fn trap() -> SemTerminator {
    SemTerminator::Trap {
        kind: hew_sir::TrapKind::IndexOutOfBounds,
    }
}
fn branch(yes: u32, no: u32) -> SemTerminator {
    SemTerminator::Branch {
        condition: operand(1),
        then_target: edge(yes),
        else_target: edge(no),
    }
}

fn normalize(module: &mut SemModule) {
    let mut next = 0;
    for block in &mut probe(module).blocks {
        for operation in &mut block.ops {
            operation.id = OpId(next);
            next += 1;
        }
    }
}

fn valid(module: &mut SemModule) {
    normalize(module);
    let errors = verify_module(module);
    assert!(errors.is_empty(), "{errors:#?}");
    let function = module
        .functions
        .iter()
        .find(|function| function.declaration.full_path() == "probe")
        .unwrap();
    assert!(place_lifetimes(module, function).is_ok());
}

fn refuses(module: &mut SemModule, expected: &str) {
    normalize(module);
    let errors = verify_module(module);
    assert!(
        errors.iter().any(|error| match &error.kind {
            SirDiagnosticKind::UnconsumedLinear { .. } => expected == "UnconsumedLinear",
            SirDiagnosticKind::PlaceLifetime { reason, .. }
            | SirDiagnosticKind::OwnershipLifetime { reason, .. }
            | SirDiagnosticKind::FaultLifetime { reason, .. } => reason.contains(expected),
            SirDiagnosticKind::InvalidOperation { reason, .. }
            | SirDiagnosticKind::InvalidCallable { reason, .. } => reason.contains(expected),
            _ => false,
        }),
        "expected {expected}: {errors:#?}"
    );
    let function = module
        .functions
        .iter()
        .find(|function| function.declaration.full_path() == "probe")
        .unwrap();
    assert!(place_lifetimes(module, function).is_err());
}

#[test]
fn live_and_taken_predecessors_join_until_lexical_cleanup() {
    let mut module = strings();
    probe(&mut module).blocks = vec![
        block(0, vec![alloc(0), init(0, 0)], branch(1, 2)),
        block(
            1,
            vec![take(0, 2, ResolvedTy::String, OwnKind::Owned), destroy(2)],
            SemTerminator::Goto(edge(2)),
        ),
        block(2, vec![end(0)], done()),
    ];
    valid(&mut module);
    let function = probe(&mut module).clone();
    let plan = place_plan(&function, &module.aggregate_shapes, &module.type_facts).unwrap();
    assert_eq!(
        plan.leaves(OwnerRoot::Local(PlaceId(0))).unwrap(),
        [PlaceId(0)]
    );
    assert_eq!(plan.base(PlaceId(0)), None);
    assert!(plan.projection(PlaceId(0)).unwrap().path.is_empty());
    let mut read = module.clone();
    probe(&mut read).blocks[2]
        .ops
        .insert(0, take(0, 3, ResolvedTy::String, OwnKind::Owned));
    probe(&mut read).blocks[2].ops.insert(1, destroy(3));
    refuses(&mut read, "not initialized on every incoming path");
    let mut reinitialize = module.clone();
    probe(&mut reinitialize).blocks[2].ops.insert(0, init(0, 0));
    refuses(&mut reinitialize, "overwrite a live value");
}

#[test]
fn storage_activity_is_independent_of_empty_contents_and_reentrant_loops() {
    let mut module = strings();
    probe(&mut module).blocks = vec![
        block(0, vec![destroy(0)], SemTerminator::Goto(edge(1))),
        block(1, vec![alloc(0), end(0)], branch(1, 2)),
        block(2, vec![], done()),
    ];
    valid(&mut module);
    for mutation in 0..3 {
        let mut broken = module.clone();
        let operations = &mut probe(&mut broken).blocks[1].ops;
        match mutation {
            0 => {
                operations.remove(0);
            }
            1 => {
                operations.push(end(0));
            }
            2 => {
                operations.insert(1, alloc(0));
            }
            _ => unreachable!(),
        }
        refuses(
            &mut broken,
            if mutation == 2 {
                "already active"
            } else {
                "not active"
            },
        );
    }
    probe(&mut module).blocks[1].ops.pop();
    refuses(&mut module, "remains active at exit");
}

#[test]
fn allocation_on_only_one_predecessor_cannot_supply_an_active_local() {
    let mut module = strings();
    probe(&mut module).blocks = vec![
        block(0, vec![destroy(0)], branch(1, 2)),
        block(1, vec![alloc(0)], SemTerminator::Goto(edge(2))),
        block(2, vec![end(0)], done()),
    ];
    refuses(&mut module, "not active on every incoming path");
}

#[test]
fn assignment_replaces_live_or_empty_contents_after_a_join() {
    let mut module = strings();
    probe(&mut module).blocks = vec![
        block(
            0,
            vec![
                alloc(0),
                init(0, 0),
                load(
                    SemOpKind::LoadCopy { place: PlaceId(0) },
                    2,
                    ResolvedTy::String,
                    OwnKind::Owned,
                ),
            ],
            branch(1, 2),
        ),
        block(
            1,
            vec![take(0, 3, ResolvedTy::String, OwnKind::Owned), destroy(3)],
            SemTerminator::Goto(edge(2)),
        ),
        block(2, vec![assign(0, 2), end(0)], done()),
    ];
    valid(&mut module);
}

#[test]
fn local_loans_preserve_intermediate_parents_and_exclude_writes() {
    let mut module = strings();
    probe(&mut module).blocks[0].ops = vec![
        alloc(0),
        init(0, 0),
        load(
            SemOpKind::LoadBorrow { place: PlaceId(0) },
            2,
            ResolvedTy::String,
            OwnKind::Guaranteed,
        ),
        load(
            SemOpKind::BeginBorrow { owner: operand(2) },
            3,
            ResolvedTy::String,
            OwnKind::Guaranteed,
        ),
        end_borrow(3),
        end_borrow(2),
        end(0),
    ];
    valid(&mut module);
    for early in [
        end_borrow(2),
        end(0),
        take(0, 4, ResolvedTy::String, OwnKind::Owned),
    ] {
        let mut broken = module.clone();
        probe(&mut broken).blocks[0].ops.insert(4, early);
        refuses(&mut broken, "dependent borrow is live");
    }
}

#[test]
fn local_projection_partition_tracks_fields_without_an_intermediate_init_bit() {
    let mut module =
        fixture("fn probe(consume owner: (string, (string, bytes)), flag: bool) {} fn main() {}");
    let function = probe(&mut module);
    let inner = ResolvedTy::Tuple(vec![ResolvedTy::String, ResolvedTy::Bytes]);
    function.places.extend(
        [
            (1, 0, 0, ResolvedTy::String),
            (2, 0, 1, inner),
            (3, 2, 0, ResolvedTy::String),
            (4, 2, 1, ResolvedTy::Bytes),
        ]
        .into_iter()
        .map(|(id, parent, field, ty)| PlaceDecl {
            id: PlaceId(id),
            ty,
            origin: PlaceOrigin::Aggregate {
                base: PlaceBase::Place(PlaceId(parent)),
                shape: hew_sir::AggregateShapeRef::Tuple,
                field,
            },
        }),
    );
    function.blocks = vec![
        block(0, vec![alloc(0), init(0, 0)], branch(1, 2)),
        block(
            1,
            vec![take(3, 2, ResolvedTy::String, OwnKind::Owned), destroy(2)],
            SemTerminator::Goto(edge(2)),
        ),
        block(2, vec![end(0)], done()),
    ];
    valid(&mut module);
    let function = probe(&mut module).clone();
    let plan = place_plan(&function, &module.aggregate_shapes, &module.type_facts).unwrap();
    assert_eq!(
        plan.leaves(OwnerRoot::Local(PlaceId(0))).unwrap(),
        [PlaceId(1), PlaceId(3), PlaceId(4)]
    );
    assert_eq!(
        plan.projection(PlaceId(2)).unwrap().leaves,
        [PlaceId(3), PlaceId(4)]
    );
    let mut whole = module.clone();
    probe(&mut whole).blocks[2]
        .ops
        .insert(0, take(0, 3, function.params[0].ty.clone(), OwnKind::Owned));
    refuses(&mut whole, "not initialized on every incoming path");
    probe(&mut module).blocks[2].ops = vec![end(2), end(0)];
    refuses(&mut module, "require a local root");
}

#[test]
fn linear_trap_cleanup_is_derived_from_the_contiguous_suffix() {
    let mut module = linear();
    probe(&mut module).blocks[0].terminator = trap();
    valid(&mut module);
    let function = probe(&mut module).clone();
    let lifetimes = place_lifetimes(&module, &function).unwrap();
    assert_eq!(
        lifetimes.cleanup(function.blocks[0].ops[2].id),
        Some(CleanupMode::Trap)
    );
    let mut normal = module.clone();
    probe(&mut normal).blocks[0].terminator = done();
    refuses(&mut normal, "UnconsumedLinear");
    // A real observable write after EndLifetime separates it from the suffix.
    // Even a terminal Trap cannot retrospectively forgive that earlier end.
    let mut effect = module.clone();
    let function = probe(&mut effect);
    function.places.push(PlaceDecl {
        id: PlaceId(1),
        ty: ResolvedTy::Bool,
        origin: PlaceOrigin::Local,
    });
    function.blocks[0].ops.insert(0, alloc(1));
    function.blocks[0].ops.extend([init(1, 1), end(1)]);
    refuses(&mut effect, "UnconsumedLinear");
}

#[test]
fn terminal_completion_requires_receiver_identity_and_preserves_replacement_obligations() {
    let mut module = linear();
    let function = probe(&mut module);
    function.terminal_receiver = Some(ValueId(0));
    function.blocks[0]
        .ops
        .insert(2, op(SemOpKind::FinishLinearReceiver));
    valid(&mut module);
    let function = probe(&mut module).clone();
    assert_eq!(
        place_lifetimes(&module, &function)
            .unwrap()
            .cleanup(function.blocks[0].ops[3].id),
        Some(CleanupMode::TerminalReceiver)
    );

    probe(&mut module).terminal_receiver = None;
    refuses(&mut module, "checked owned terminal receiver");
    probe(&mut module).terminal_receiver = Some(ValueId(0));
    probe(&mut module).blocks[0].ops.insert(3, assign(0, 0));
    refuses(&mut module, "UnconsumedLinear");
}

#[test]
fn linear_assignment_is_never_forgiven_by_a_later_trap() {
    let mut module = linear();
    let ty = probe(&mut module).params[0].ty.clone();
    probe(&mut module).blocks[0].ops = vec![alloc(0), init(0, 0), assign(0, 0), end(0)];
    probe(&mut module).blocks[0].terminator = trap();
    refuses(&mut module, "UnconsumedLinear");
    // Taking then ending empty storage is valid; destroying the transferred
    // linear SSA value remains subject to the same trap-only boundary.
    probe(&mut module).blocks[0].ops = vec![
        alloc(0),
        init(0, 0),
        take(0, 2, ty, OwnKind::Owned),
        end(0),
        destroy(2),
    ];
    valid(&mut module);
    probe(&mut module).blocks[0].terminator = done();
    refuses(&mut module, "UnconsumedLinear");
}

#[test]
fn borrowed_parameters_need_an_explicit_copy_before_local_initialization() {
    let mut module = fixture("fn probe(owner: string, flag: bool) {} fn main() {}");
    refuses(&mut module, "guaranteed input cannot be consumed");
    probe(&mut module).blocks[0].ops = vec![
        alloc(0),
        load(
            SemOpKind::CopyValue { source: operand(0) },
            2,
            ResolvedTy::String,
            OwnKind::Owned,
        ),
        init(0, 2),
        end(0),
    ];
    valid(&mut module);
}

#[test]
fn local_copy_requires_the_exact_type_clone_contract() {
    let mut module =
        fixture("fn probe(consume owner: fn[once]() -> i64, flag: bool) {} fn main() {}");
    valid(&mut module);
    let ty = probe(&mut module).places[0].ty.clone();
    probe(&mut module).blocks[0].ops.insert(
        2,
        load(
            SemOpKind::LoadCopy { place: PlaceId(0) },
            2,
            ty,
            OwnKind::Owned,
        ),
    );
    probe(&mut module).blocks[0].ops.insert(3, destroy(2));
    refuses(&mut module, "has no copy operation");
}

#[test]
fn taking_a_zero_sized_field_still_empties_its_content_cell() {
    let mut module = fixture(
        "type Empty {} fn probe(consume owner: (string, Empty), flag: bool) {} fn main() {}",
    );
    let ResolvedTy::Tuple(fields) = probe(&mut module).params[0].ty.clone() else {
        unreachable!()
    };
    let empty = fields[1].clone();
    probe(&mut module)
        .places
        .extend(fields.into_iter().enumerate().map(|(index, ty)| PlaceDecl {
            id: PlaceId(u32::try_from(index).unwrap() + 1),
            ty,
            origin: PlaceOrigin::Aggregate {
                base: PlaceBase::Place(PlaceId(0)),
                shape: hew_sir::AggregateShapeRef::Tuple,
                field: u32::try_from(index).unwrap(),
            },
        }));
    probe(&mut module).blocks[0]
        .ops
        .insert(2, take(2, 2, empty.clone(), OwnKind::None));
    valid(&mut module);
    probe(&mut module).blocks[0]
        .ops
        .insert(3, take(2, 3, empty, OwnKind::None));
    refuses(&mut module, "not initialized on every incoming path");
}

#[test]
fn checked_module_retains_the_local_plan_and_cleanup_for_each_body() {
    let mut module = strings();
    normalize(&mut module);
    let checked = hew_sir::check_module(&module).unwrap();
    for function in &checked.module().functions {
        let analysis = checked.function(function.callable).unwrap();
        if function.declaration.full_path() == "probe" {
            assert_eq!(
                analysis
                    .place_plan()
                    .leaves(OwnerRoot::Local(PlaceId(0)))
                    .unwrap(),
                [PlaceId(0)]
            );
            let end = function.blocks[0].ops.last().unwrap();
            assert_eq!(
                analysis.place_lifetimes().cleanup(end.id),
                Some(CleanupMode::Ordinary)
            );
        } else if function.declaration.full_path() == "main" {
            assert!(analysis.place_plan().roots().next().is_none());
        }
    }
    probe(&mut module).blocks[0].ops.pop();
    assert!(hew_sir::check_module(&module)
        .unwrap_err()
        .iter()
        .any(|error| matches!(
            error.kind,
            SirDiagnosticKind::PlaceLifetime {
                reason: "local storage remains active at exit",
                ..
            }
        )));
}

fn module_context_fixture() -> SemModule {
    let mut module = fixture(
        r#"
        type Row { text: string }
        enum Choice { First(string), Second }
        fn probe(consume owner: string, flag: bool) {}
        fn main() {
            let rows = [Row { text: "row" }];
            let choice = Choice.First("choice");
            let raw = b"raw";
            println(rows == rows);
        }
    "#,
    );
    normalize(&mut module);
    assert!(hew_sir::check_module(&module).is_ok());
    module
}

fn rejects_module_context(module: &SemModule, expected: impl Fn(&SirDiagnosticKind) -> bool) {
    for diagnostics in [
        hew_sir::check_module(module).unwrap_err(),
        verify_module(module),
    ] {
        assert!(
            diagnostics
                .iter()
                .any(|diagnostic| expected(&diagnostic.kind)),
            "{diagnostics:#?}"
        );
    }
}

#[test]
fn checked_module_rejects_invalid_aggregate_and_variant_tables() {
    let module = module_context_fixture();
    let mut aggregate = module.clone();
    aggregate.aggregate_shapes[0].id = hew_sir::AggregateShapeId(u32::MAX);
    rejects_module_context(&aggregate, |kind| {
        matches!(kind, SirDiagnosticKind::InvalidAggregateShape { .. })
    });
    let mut variant = module;
    variant.variant_shapes[0].id = hew_sir::VariantShapeId(u32::MAX);
    rejects_module_context(&variant, |kind| {
        matches!(kind, SirDiagnosticKind::InvalidVariantShape { .. })
    });
}

#[test]
fn checked_module_rejects_invalid_capability_and_collection_rows() {
    let module = module_context_fixture();
    let mut capability = module.clone();
    capability
        .value_capabilities
        .values_mut()
        .next()
        .unwrap()
        .callable = Some(hew_sir::CallableId(u32::MAX));
    rejects_module_context(&capability, |kind| {
        matches!(kind, SirDiagnosticKind::InvalidValueCapability { .. })
    });
    let mut collection = module;
    let element = collection
        .type_facts
        .keys()
        .find_map(|key| {
            hew_types::runtime_call::collection_type_arguments(&key.0)
                .map(|(_, arguments)| arguments[0].clone())
        })
        .unwrap();
    collection
        .type_facts
        .remove(&hew_types::TypeInstanceKey(element));
    rejects_module_context(&collection, |kind| {
        matches!(kind, SirDiagnosticKind::InvalidCollectionType { .. })
    });
}

#[test]
fn checked_module_rejects_duplicate_identities_and_missing_literal_pool_entries() {
    let module = module_context_fixture();
    let mut duplicate = module.clone();
    duplicate.functions.push(duplicate.functions[0].clone());
    rejects_module_context(&duplicate, |kind| {
        matches!(kind, SirDiagnosticKind::DuplicateFunctionName(_))
    });
    rejects_module_context(&duplicate, |kind| {
        matches!(kind, SirDiagnosticKind::DuplicateFunctionDeclaration(_))
    });
    for bytes in [false, true] {
        let mut missing = module.clone();
        if bytes {
            missing.bytes_literals.clear();
        } else {
            missing.string_literals.clear();
        }
        // The isolated probe contains no literals: accepting it cannot certify
        // the pool entries referenced by a different body in this module.
        let probe = missing
            .functions
            .iter()
            .find(|function| function.declaration.full_path() == "probe")
            .unwrap();
        assert!(place_lifetimes(&missing, probe).is_ok());
        rejects_module_context(&missing, |kind| {
            matches!(kind, SirDiagnosticKind::InvalidOperation { reason, .. }
            if reason == "literal operation references a missing module pool entry")
        });
    }
}

#[test]
fn parked_fault_preserves_trap_only_linear_cleanup() {
    use hew_sir::{BoundaryDecision, BoundaryOperand, DeferId, DeferScopeId, FaultParkId};
    let mut module = linear();
    module
        .string_literals
        .insert(hew_sir::StringLiteralId(0), "body".into());
    let function = probe(&mut module);
    let mut registration = end(0);
    registration.kind = SemOpKind::RegisterDefer {
        defer: DeferId(0),
        scope: DeferScopeId(0),
        dependencies: vec![PlaceId(0)],
    };
    let mut message = end(0);
    message.kind = SemOpKind::ConstStr(hew_sir::StringLiteralId(0));
    message.results = vec![ValueDef {
        id: ValueId(2),
        ty: ResolvedTy::String,
        own: OwnKind::Owned,
    }];
    function.blocks = vec![
        block(
            0,
            vec![alloc(0), init(0, 0), registration, message],
            SemTerminator::Panic {
                message: BoundaryOperand {
                    operand: Operand { value: ValueId(2) },
                    decision: BoundaryDecision::Borrow,
                },
                cleanup: edge(1),
            },
        ),
        block(
            1,
            vec![destroy(2)],
            SemTerminator::EnterDefer {
                defer: DeferId(0),
                park: FaultParkId(0),
                body: edge(2),
            },
        ),
        block(
            2,
            vec![end(0)],
            SemTerminator::FinishDefer {
                defer: DeferId(0),
                park: FaultParkId(0),
                next: edge(3),
            },
        ),
        block(
            3,
            vec![],
            SemTerminator::CleanupDispatch {
                normal: edge(4),
                fault: edge(5),
            },
        ),
        block(4, vec![], done()),
        block(5, vec![], SemTerminator::ResumeUnwind),
    ];
    valid(&mut module);
    let function = probe(&mut module).clone();
    assert_eq!(
        place_lifetimes(&module, &function)
            .unwrap()
            .cleanup(function.blocks[2].ops[0].id),
        Some(CleanupMode::Trap)
    );
    probe(&mut module).blocks[0].terminator = SemTerminator::Goto(edge(1));
    refuses(&mut module, "UnconsumedLinear");
}
