use hew_hir::{lower_program_host_target, ResolutionCtx};
use hew_sir::{
    lower_module, verify_module, BindingTarget, CallUnwind, OwnKind, SemModule, SemOpKind,
    SemTerminator, SirLoweringStatus, ValueId,
};
use hew_types::{
    module_registry::ModuleRegistry,
    runtime_call::{MapValueOp, SetValueOp},
    Checker, RuntimeCallFamily, VecValueOp,
};

fn lower_source(source: &str) -> SemModule {
    let parsed = hew_parser::parse(source);
    assert!(parsed.errors.is_empty(), "parse: {:?}", parsed.errors);
    let mut checker = Checker::new(ModuleRegistry::new(Vec::new()));
    let facts = checker.check_program(&parsed.program);
    assert!(facts.errors.is_empty(), "checker: {:?}", facts.errors);
    let hir = lower_program_host_target(&parsed.program, &facts, &ResolutionCtx);
    assert!(hir.diagnostics.is_empty(), "HIR: {:?}", hir.diagnostics);
    let lowered = lower_module(&hir.module, &facts);
    assert!(
        lowered
            .statuses
            .iter()
            .any(|status| status.name == "main"
                && matches!(status.status, SirLoweringStatus::Lowered)),
        "SIR: {:?}",
        lowered.statuses
    );
    assert!(
        lowered
            .callable_statuses
            .iter()
            .all(|(_, status)| !matches!(status, SirLoweringStatus::Unsupported { .. })),
        "callables: {:?}",
        lowered.callable_statuses
    );
    let diagnostics = verify_module(&lowered.module);
    assert!(
        diagnostics.is_empty(),
        "verify: {diagnostics:?}\n{}",
        hew_sir::dump_sir(&lowered.module)
    );
    lowered.module
}

#[test]
fn generic_impl_can_mutate_its_local_record_field() {
    lower_source(
        r#"
        type Store<T> { slots: Vec<T>, label: string }
        impl<T> Store<T> {
            fn add(self, value: T) -> Store<T> {
                var updated = self;
                updated.slots.push(value);
                updated
            }
        }
        fn main() -> i64 {
            let original = Store { slots: ["first"], label: "kept" };
            let updated = original.add("second");
            original.slots.len() * 10 + updated.slots.len()
        }
    "#,
    );
}

#[test]
fn field_push_transfers_the_leaf_without_copying_its_container() {
    let module = lower_source(
        r#"
        type State { xs: Vec<string>, sibling: string }
        fn main() -> i64 {
            var state = State { xs: ["first"], sibling: "kept" };
            state.xs.push("second");
            state.xs.push(state.xs[0]);
            state.xs.len() + state.sibling.len()
        }
    "#,
    );
    let main = module
        .functions
        .iter()
        .find(|function| function.declaration.full_path() == "main")
        .unwrap();
    let initial = main
        .bindings
        .iter()
        .find(|binding| binding.name == "state")
        .unwrap();
    let BindingTarget::Place(initial) = initial.target else {
        panic!("state must have local storage")
    };
    // Construction may copy its field initializer. Mutating the established
    // place must transfer its container, including when another argument reads it.
    for operation in main
        .blocks
        .iter()
        .flat_map(|block| &block.ops)
        .skip_while(|operation| !matches!(operation.kind, SemOpKind::StoreInit { place, .. } if place == initial))
        .skip(1)
    {
        if matches!(
            operation.kind,
            SemOpKind::CopyValue { .. }
                | SemOpKind::AggregateProjectCopy { .. }
                | SemOpKind::LoadCopy { .. }
        ) {
            assert!(operation.results.iter().all(|result| {
                hew_types::runtime_call::collection_type_arguments(&result.ty).is_none()
                    && !matches!(&result.ty, hew_types::ResolvedTy::Named { name, .. } if name == "State")
            }), "receiver or its parent was copied: {operation:?}");
        }
    }
}

#[test]
fn nested_fields_and_tuple_segments_share_assignment_reconstruction() {
    lower_source(
        r#"
        type Inner { xs: Vec<string>, label: string }
        type Outer { inner: Inner, sibling: Vec<string> }
        fn main() -> i64 {
            var state = Outer { inner: Inner { xs: ["first"], label: "old" }, sibling: ["keep"] };
            state.inner.xs.push("second");
            state.inner.label = "new";
            var pair = (state, "tail");
            pair.0.inner.xs.push("third");
            pair.1 = "changed";
            pair.0.inner.xs.len() + pair.0.sibling.len() + pair.1.len()
        }
    "#,
    );
}

#[test]
fn later_argument_updates_are_in_the_receiver_version_taken_by_push() {
    let module = lower_source(
        r#"
        type State { xs: Vec<string>, sibling: string }
        fn main() -> i64 {
            var state = State { xs: ["first"], sibling: "old" };
            state.xs.push({ state.sibling = "changed"; state.xs.clear(); "tail" });
            state.xs.len() + state.sibling.len()
        }
    "#,
    );
    assert_receiver_update_order(&module);
}

fn assert_receiver_update_order(module: &SemModule) {
    let main = module
        .functions
        .iter()
        .find(|function| function.declaration.full_path() == "main")
        .unwrap();
    let BindingTarget::Place(root) = main
        .bindings
        .iter()
        .find(|binding| binding.name == "state")
        .unwrap()
        .target
    else {
        panic!("state must retain its local storage")
    };
    let plan = hew_sir::place_plan(main, &module.aggregate_shapes, &module.type_facts).unwrap();
    let clear = runtime_block(main, RuntimeCallFamily::Vector(VecValueOp::Clear));
    let SemTerminator::RtCall {
        normal,
        args: clear_args,
        ..
    } = &clear.terminator
    else {
        unreachable!()
    };
    let push = main
        .blocks
        .iter()
        .find(|block| block.id == normal.target)
        .unwrap();
    let SemTerminator::RtCall {
        family: RuntimeCallFamily::Vector(VecValueOp::Push),
        args,
        normal,
        ..
    } = &push.terminator
    else {
        panic!("push must follow the later argument's clear")
    };
    let (_, place) = taken_receiver(push, args[0].operand.value);
    assert_eq!(place, taken_receiver(clear, clear_args[0].operand.value).1);
    let field = plan.projection(place).unwrap();
    assert_eq!(field.root, hew_sir::OwnerRoot::Local(root));
    assert_eq!(
        field.path.iter().map(|step| step.field).collect::<Vec<_>>(),
        [0]
    );
    let sibling_write = clear
        .ops
        .iter()
        .position(|op| match op.kind {
            SemOpKind::StoreAssign { place, .. } => {
                let sibling = plan.projection(place).unwrap();
                sibling.root == hew_sir::OwnerRoot::Local(root)
                    && sibling
                        .path
                        .iter()
                        .map(|step| step.field)
                        .collect::<Vec<_>>()
                        == [1]
            }
            _ => false,
        })
        .unwrap();
    let clear_take = clear
        .ops
        .iter()
        .position(|op| matches!(op.kind, SemOpKind::LoadTake { place: p } if p == place))
        .unwrap();
    assert!(
        sibling_write < clear_take,
        "sibling replacement precedes clearing the receiver"
    );
    let clear_write = returned_receiver_store(push, place);
    let push_take = push
        .ops
        .iter()
        .position(|op| matches!(op.kind, SemOpKind::LoadTake { place: p } if p == place))
        .unwrap();
    assert!(
        clear_write < push_take,
        "push takes the field after installing clear's returned receiver"
    );
    let after_push = main
        .blocks
        .iter()
        .find(|block| block.id == normal.target)
        .unwrap();
    returned_receiver_store(after_push, place);
    for block in &main.blocks {
        if matches!(
            block.terminator,
            SemTerminator::Return { .. } | SemTerminator::Trap { .. }
        ) {
            assert_root_cleanup(block, root);
        }
    }
}

#[test]
fn later_argument_can_replace_the_root_and_source_copies_stay_independent() {
    lower_source(
        r#"
        type State { xs: Vec<string>, label: string }
        fn main() -> i64 {
            var state = State { xs: ["old"], label: "old label" };
            let copied = state;
            state.xs.push({ state = State { xs: ["new"], label: "new label" }; copied.xs[0] });
            copied.xs.len() * 10 + state.xs.len()
        }
    "#,
    );
}

#[test]
fn map_and_set_field_mutations_publish_receiver_and_returned_value() {
    let module = lower_source(
        r#"
        type State { map: HashMap<i64, string>, set: HashSet<string>, sibling: Vec<string> }
        fn main() -> i64 {
            var state = State { map: HashMap.new(), set: HashSet.new(), sibling: ["kept"] };
            state.map.insert(1, "value");
            let inserted = state.set.insert("member");
            let removed = state.map.remove(1);
            let deleted = state.set.remove("member");
            if inserted && deleted { state.map.len() + state.set.len() + state.sibling.len() } else { 99 }
        }
    "#,
    );
    let families: Vec<_> = module
        .functions
        .iter()
        .flat_map(|function| &function.blocks)
        .filter_map(|block| match block.terminator {
            SemTerminator::RtCall { family, .. } => Some(family),
            _ => None,
        })
        .collect();
    for family in [
        RuntimeCallFamily::Map(MapValueOp::Insert),
        RuntimeCallFamily::Map(MapValueOp::Remove),
        RuntimeCallFamily::Set(SetValueOp::Insert),
        RuntimeCallFamily::Set(SetValueOp::Remove),
    ] {
        assert!(families.contains(&family), "missing {family:?}");
    }
}

fn assert_retained_sibling_cleanup(module: &SemModule, family: RuntimeCallFamily) {
    let main = module
        .functions
        .iter()
        .find(|function| function.declaration.full_path() == "main")
        .unwrap();
    let (call, moved, cleanup) = main
        .blocks
        .iter()
        .rev()
        .find_map(|block| match &block.terminator {
            SemTerminator::RtCall {
                family: actual,
                args,
                unwind: CallUnwind::Cleanup(edge),
                ..
            } if *actual == family => Some((block, args[0].operand.value, edge.target)),
            _ => None,
        })
        .unwrap();
    let (leaf, place) = taken_receiver(call, moved);
    let cleanup = match &main.blocks[cleanup.0 as usize].terminator {
        SemTerminator::CheckedRaiseFault { kind, cleanup } => {
            assert_eq!(*kind, hew_sir::TrapKind::IndexOutOfBounds);
            cleanup.target
        }
        _ => cleanup,
    };
    let plan = hew_sir::place_plan(main, &module.aggregate_shapes, &module.type_facts).unwrap();
    let field = plan.projection(place).unwrap();
    assert_eq!(
        field.path.iter().map(|step| step.field).collect::<Vec<_>>(),
        [0]
    );
    assert!(
        plan.leaves(field.root).unwrap().iter().any(|&sibling| {
            sibling != place && plan.projection(sibling).unwrap().recipe.own == OwnKind::Owned
        }),
        "fixture must retain an owned sibling in the root partition"
    );
    let mut pending = vec![cleanup];
    let mut reachable = std::collections::BTreeSet::new();
    while let Some(id) = pending.pop() {
        if reachable.insert(id) {
            main.blocks[id.0 as usize]
                .terminator
                .visit_successors(|edge| pending.push(edge.target));
        }
    }
    let cleanup_ops = reachable
        .iter()
        .flat_map(|id| &main.blocks[id.0 as usize].ops)
        .collect::<Vec<_>>();
    assert!(
        cleanup_ops.iter().any(|op| matches!(op.kind,
        SemOpKind::EndLifetime { place } if hew_sir::OwnerRoot::Local(place) == field.root)),
        "failure must clean the partially initialized root's remaining fields"
    );
    assert!(!cleanup_ops.iter().any(|op| matches!(op.kind,
        SemOpKind::StoreAssign { place: p, .. } | SemOpKind::StoreInit { place: p, .. } if p == place)),
        "failure cannot reinstall the receiver");
    let mut missing_cleanup = module.clone();
    let function = missing_cleanup
        .functions
        .iter_mut()
        .find(|f| f.declaration.full_path() == "main")
        .unwrap();
    for block in &mut function.blocks {
        if reachable.contains(&block.id) {
            block.ops.retain(|op| {
                !matches!(op.kind,
                SemOpKind::EndLifetime { place } if hew_sir::OwnerRoot::Local(place) == field.root)
            });
        }
    }
    assert!(verify_module(&missing_cleanup).iter().any(|error| matches!(error.kind,
        hew_sir::SirDiagnosticKind::PlaceLifetime { place, reason, .. }
            if hew_sir::OwnerRoot::Local(place) == field.root && reason == "local storage remains active at exit")),
        "omitting remaining-root cleanup must be rejected");
    let destroyed = |value| {
        cleanup_ops.iter().any(|op| {
            matches!(&op.kind,
        SemOpKind::DestroyValue { value: operand } if operand.value == value)
        })
    };
    assert!(
        !destroyed(leaf),
        "the pre-transfer receiver cannot be destroyed again"
    );
    assert_eq!(
        destroyed(moved),
        family
            .semantic_contract()
            .unwrap()
            .preserves_inputs_on_failure()
    );
}

#[test]
fn bounds_failure_releases_retained_parent_siblings() {
    let module = lower_source(
        r#"
        type State { xs: Vec<string>, sibling: Vec<string> }
        fn main() -> i64 {
            var state = State { xs: ["first"], sibling: ["kept"] };
            state.xs.set(7, "outside");
            state.sibling.len()
        }
    "#,
    );
    assert_retained_sibling_cleanup(&module, RuntimeCallFamily::Vector(VecValueOp::Set));
}

#[test]
fn callback_failure_releases_retained_parent_siblings() {
    let module = lower_source(
        r#"
        type Key { divisor: i64 }
        impl Hash for Key { fn hash(self) -> i64 { 12 / self.divisor } }
        type State { values: HashMap<Key, string>, sibling: Vec<string> }
        fn main() -> i64 {
            var state = State { values: HashMap.new(), sibling: ["kept"] };
            state.values.insert(Key { divisor: 0 }, "value");
            state.sibling.len()
        }
    "#,
    );
    assert_retained_sibling_cleanup(&module, RuntimeCallFamily::Map(MapValueOp::Insert));
}

#[test]
fn immutable_field_receivers_and_wrong_element_types_remain_rejected() {
    for (declaration, mutation, expected) in [
        (
            "let",
            "state.xs.push(2)",
            hew_types::error::TypeErrorKind::MutabilityError,
        ),
        (
            "let",
            "state.map.insert(2, 3)",
            hew_types::error::TypeErrorKind::MutabilityError,
        ),
        (
            "let",
            "state.set.insert(2)",
            hew_types::error::TypeErrorKind::MutabilityError,
        ),
    ] {
        let source = format!("type State {{ xs: Vec<i64>, map: HashMap<i64, i64>, set: HashSet<i64> }} fn main() -> i64 {{ {declaration} state = State {{ xs: [1], map: HashMap.new(), set: HashSet.new() }}; {mutation}; 0 }}");
        let parsed = hew_parser::parse(&source);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let mut checker = Checker::new(ModuleRegistry::new(Vec::new()));
        let facts = checker.check_program(&parsed.program);
        assert!(
            facts.errors.iter().any(|error| error.kind == expected),
            "{mutation}: {:?}",
            facts.errors
        );
    }
    let parsed = hew_parser::parse(
        r#"type State { xs: Vec<i64> } fn main() -> i64 { var state = State { xs: [1] }; state.xs.push("wrong"); 0 }"#,
    );
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let mut checker = Checker::new(ModuleRegistry::new(Vec::new()));
    let facts = checker.check_program(&parsed.program);
    assert!(
        !facts.errors.is_empty(),
        "wrong element type must remain rejected"
    );
}

#[test]
fn malformed_mutable_places_cannot_bypass_root_or_projection_checks() {
    use hew_hir::{HirExprKind, HirItem, HirStmtKind};

    let parsed = hew_parser::parse(
        r#"type State { xs: Vec<i64>, sibling: string }
        fn main() -> i64 {
            var state = State { xs: [1], sibling: "kept" };
            state.xs.push(2);
            state.xs.len()
        }"#,
    );
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let mut checker = Checker::new(ModuleRegistry::new(Vec::new()));
    let facts = checker.check_program(&parsed.program);
    assert!(facts.errors.is_empty(), "{:?}", facts.errors);
    let hir = lower_program_host_target(&parsed.program, &facts, &ResolutionCtx);
    assert!(hir.diagnostics.is_empty(), "{:?}", hir.diagnostics);
    for expected in ["not mutable", "local binding root", "expected `string`"] {
        let mut invalid = hir.module.clone();
        let main = invalid
            .items
            .iter_mut()
            .find_map(|item| match item {
                HirItem::Function(function) if function.declaration.full_path() == "main" => {
                    Some(function)
                }
                _ => None,
            })
            .unwrap();
        let HirStmtKind::Let(binding, Some(initial)) = &mut main.body.statements[0].kind else {
            panic!("fixture must begin with a record binding")
        };
        let initial = initial.clone();
        if expected == "not mutable" {
            binding.mutable = false;
        } else {
            let HirStmtKind::Expr(call) = &mut main.body.statements[1].kind else {
                panic!("fixture must mutate its field")
            };
            let HirExprKind::Call { args, .. } = &mut call.kind else {
                panic!("fixture must lower to a runtime call")
            };
            let HirExprKind::FieldAccess { object, field } = &mut args[0].kind else {
                panic!("fixture receiver must be a field")
            };
            if expected == "local binding root" {
                **object = initial;
            } else {
                *field = "sibling".into();
            }
        }
        let lowered = lower_module(&invalid, &facts);
        let status = &lowered
            .statuses
            .iter()
            .find(|status| status.name == "main")
            .unwrap()
            .status;
        assert!(
            matches!(status, SirLoweringStatus::Unsupported { reason } if reason.contains(expected)),
            "{expected}: {status:?}"
        );
    }
}

fn taken_receiver(block: &hew_sir::SemBlock, receiver: ValueId) -> (ValueId, hew_sir::PlaceId) {
    let leaf = block
        .ops
        .iter()
        .find_map(|op| match &op.kind {
            SemOpKind::Move { source } if op.results[0].id == receiver => Some(source.value),
            _ => None,
        })
        .unwrap();
    let place = block
        .ops
        .iter()
        .find_map(|op| match op.kind {
            SemOpKind::LoadTake { place } if op.results[0].id == leaf => Some(place),
            _ => None,
        })
        .unwrap();
    (leaf, place)
}

fn runtime_block(function: &hew_sir::SemFunction, family: RuntimeCallFamily) -> &hew_sir::SemBlock {
    function
        .blocks
        .iter()
        .find(|block| {
            matches!(block.terminator,
        SemTerminator::RtCall { family: actual, .. } if actual == family)
        })
        .unwrap()
}

fn returned_receiver_store(block: &hew_sir::SemBlock, place: hew_sir::PlaceId) -> usize {
    block.ops.iter().position(|op| matches!(&op.kind,
        SemOpKind::StoreAssign { place: p, value } if *p == place && value.value == block.args[0].value))
        .expect("store the returned receiver into the same projected field")
}

fn assert_root_cleanup(block: &hew_sir::SemBlock, root: hew_sir::PlaceId) {
    assert_eq!(
        block
            .ops
            .iter()
            .filter(|op| matches!(&op.kind,
        SemOpKind::EndLifetime { place } if *place == root))
            .count(),
        1,
        "each exit cleans up the remaining root exactly once"
    );
}
