#[path = "support/borrowed_aggregate.rs"]
mod fixture;

use hew_sir::{
    verify_module, Operand, OwnKind, SemFunction, SemOpKind, SemTerminator, SirDiagnosticKind,
    ValueId,
};

fn leaf_loan(function: &SemFunction) -> ValueId {
    function
        .blocks
        .iter()
        .find_map(|block| match &block.terminator {
            SemTerminator::RtCall {
                family: hew_types::RuntimeCallFamily::Vector(hew_types::VecValueOp::Index),
                args,
                ..
            } => Some(args[0].operand.value),
            _ => None,
        })
        .unwrap()
}

fn parent(module: &hew_sir::SemModule, loan: ValueId) -> ValueId {
    let function = &module.functions[0];
    let base = function
        .blocks
        .iter()
        .flat_map(|block| &block.ops)
        .find(|op| op.results.iter().any(|result| result.id == loan))
        .unwrap()
        .kind
        .borrow_parent()
        .unwrap();
    match base {
        hew_sir::PlaceBase::Value(value) => value,
        hew_sir::PlaceBase::Place(place) => {
            let plan = hew_sir::place_plan(function, &module.aggregate_shapes, &module.type_facts)
                .unwrap();
            let hew_sir::OwnerRoot::Value(value) = plan.projection(place).unwrap().root else {
                panic!("fixture must borrow an SSA owner")
            };
            value
        }
    }
}

fn move_cleanup_before_read(function: &mut SemFunction, value: ValueId) {
    let mut cleanup = None;
    for block in &mut function.blocks {
        if let Some(index) = block.ops.iter().position(|op| {
            matches!(&op.kind,
                SemOpKind::EndBorrow { borrow: operand } | SemOpKind::DestroyValue { value: operand }
                if operand.value == value)
        }) {
            cleanup = Some(block.ops.remove(index));
            break;
        }
    }
    let read = function
        .blocks
        .iter_mut()
        .find(|block| {
            matches!(
                block.terminator,
                SemTerminator::RtCall {
                    family: hew_types::RuntimeCallFamily::Vector(hew_types::VecValueOp::Index),
                    ..
                }
            )
        })
        .unwrap();
    read.ops
        .push(cleanup.expect("the source must have cleanup on a successor"));
}

#[test]
fn nested_loans_end_before_owner_cleanup_on_normal_and_fault_paths() {
    fixture::nested_borrow_module();
}

#[test]
fn an_owner_or_parent_loan_cannot_end_while_its_child_is_live() {
    for end_owner in [false, true] {
        let mut module = fixture::nested_borrow_module();
        let leaf = leaf_loan(&module.functions[0]);
        let parent = parent(&module, leaf);
        let value = if end_owner {
            self::parent(&module, parent)
        } else {
            parent
        };
        move_cleanup_before_read(&mut module.functions[0], value);
        assert!(verify_module(&module).iter().any(|error| matches!(
            error.kind,
            SirDiagnosticKind::OwnershipLifetime { reason, .. }
                if reason == "value cannot be consumed or ended while a dependent borrow is live"
        )));
    }
}

#[test]
fn runtime_read_cannot_use_an_ended_field_loan() {
    for mut module in [fixture::nested_borrow_module(), projected_borrow_module()] {
        let function = &mut module.functions[0];
        let leaf = leaf_loan(function);
        move_cleanup_before_read(function, leaf);
        assert!(verify_module(&module).iter().any(|error| matches!(
            error.kind,
            SirDiagnosticKind::OwnershipLifetime { value, reason, .. }
                if value == leaf && reason == "borrow is not live on every incoming path"
        )));
    }
}

#[test]
fn every_fault_edge_must_end_its_local_loans() {
    for mut module in [fixture::nested_borrow_module(), projected_borrow_module()] {
        let function = &mut module.functions[0];
        let leaf = leaf_loan(function);
        let fault = function
            .blocks
            .iter_mut()
            .find(|block| matches!(block.terminator, SemTerminator::CheckedRaiseFault { .. }))
            .unwrap();
        let before = fault.ops.len();
        fault.ops.retain(
            |op| !matches!(&op.kind, SemOpKind::EndBorrow { borrow } if borrow.value == leaf),
        );
        assert_eq!(fault.ops.len(), before - 1);
        assert!(verify_module(&module).iter().any(|error| matches!(
            error.kind,
            SirDiagnosticKind::OwnershipLifetime { value, reason, .. }
                if value == leaf && reason == "local borrow remains live at exit"
        )));
    }
}

#[test]
fn borrowed_fields_cannot_be_destroyed_as_owned_values() {
    for mut module in [fixture::nested_borrow_module(), projected_borrow_module()] {
        let function = &mut module.functions[0];
        let loan = leaf_loan(function);
        let operation = function
            .blocks
            .iter_mut()
            .flat_map(|block| &mut block.ops)
            .find(|op| matches!(&op.kind, SemOpKind::EndBorrow { borrow } if borrow.value == loan))
            .unwrap();
        operation.kind = SemOpKind::DestroyValue {
            value: Operand { value: loan },
        };
        assert!(verify_module(&module).iter().any(|error| matches!(
            error.kind,
            SirDiagnosticKind::OwnershipLifetime { value, reason, .. }
                if value == loan && reason.starts_with("guaranteed input cannot be consumed")
        )));
    }
}

#[test]
fn projection_borrows_require_exact_shape_field_type_and_ownership() {
    for mutation in 0..3 {
        let mut module = fixture::nested_borrow_module();
        let operation = module.functions[0]
            .blocks
            .iter_mut()
            .flat_map(|block| &mut block.ops)
            .find(|op| matches!(op.kind, SemOpKind::AggregateProjectBorrow { .. }))
            .unwrap();
        match mutation {
            0 => {
                let SemOpKind::AggregateProjectBorrow { field, .. } = &mut operation.kind else {
                    unreachable!()
                };
                *field = u32::MAX;
            }
            1 => operation.results[0].ty = hew_types::ResolvedTy::String,
            2 => operation.results[0].own = OwnKind::Owned,
            _ => unreachable!(),
        }
        assert!(verify_module(&module).iter().any(|error| matches!(
            error.kind,
            SirDiagnosticKind::InvalidOperation { .. } | SirDiagnosticKind::OwnershipKind { .. }
        )));
    }
}

#[test]
fn projected_owner_cannot_end_while_its_leaf_is_live() {
    let mut module = projected_borrow_module();
    let leaf = leaf_loan(&module.functions[0]);
    let function = &module.functions[0];
    let place = function
        .blocks
        .iter()
        .flat_map(|block| &block.ops)
        .find_map(|op| match op.kind {
            SemOpKind::LoadBorrow { place } if op.results[0].id == leaf => Some(place),
            _ => None,
        })
        .unwrap();
    let plan = hew_sir::place_plan(function, &module.aggregate_shapes, &module.type_facts).unwrap();
    let hew_sir::OwnerRoot::Local(root) = plan.projection(place).unwrap().root else {
        panic!("source owner must have local storage")
    };
    let function = &mut module.functions[0];
    let mut cleanup = None;
    for block in &mut function.blocks {
        if let Some(index) = block
            .ops
            .iter()
            .position(|op| matches!(op.kind, SemOpKind::EndLifetime { place } if place == root))
        {
            cleanup = Some(block.ops.remove(index));
            break;
        }
    }
    let read = function
        .blocks
        .iter_mut()
        .find(|block| {
            matches!(
                block.terminator,
                SemTerminator::RtCall {
                    family: hew_types::RuntimeCallFamily::Vector(hew_types::VecValueOp::Index),
                    ..
                }
            )
        })
        .unwrap();
    read.ops
        .push(cleanup.expect("source owner must end on a successor"));
    assert!(verify_module(&module).iter().any(|error| matches!(error.kind,
        SirDiagnosticKind::PlaceLifetime { place, reason, .. }
            if place == root && reason == "value cannot be consumed or ended while a dependent borrow is live")));
}

#[test]
fn projected_loans_require_the_exact_root_type_and_ownership() {
    for mutation in 0..3 {
        let mut module = projected_borrow_module();
        let operation = module.functions[0]
            .blocks
            .iter_mut()
            .flat_map(|b| &mut b.ops)
            .find(|op| matches!(op.kind, SemOpKind::LoadBorrow { .. }))
            .unwrap();
        match mutation {
            0 => {
                let SemOpKind::LoadBorrow { place } = &mut operation.kind else {
                    unreachable!()
                };
                *place = hew_sir::PlaceId(u32::MAX);
            }
            1 => operation.results[0].ty = hew_types::ResolvedTy::String,
            2 => operation.results[0].own = OwnKind::Owned,
            _ => unreachable!(),
        }
        assert!(verify_module(&module).iter().any(|error| matches!(
            error.kind,
            SirDiagnosticKind::InvalidOperation { .. } | SirDiagnosticKind::OwnershipKind { .. }
        )));
    }
}

/// A local nested field loan depends directly on the root, with no intermediate loan.
fn projected_borrow_module() -> hew_sir::SemModule {
    let module = fixture::lower_source(
        r#"
        type Inner { items: Vec<string>, }
        type Outer { inner: Inner, sibling: string, }
        fn main() -> i64 {
            let outer = Outer { inner: Inner { items: ["first", "second"] }, sibling: "keep" };
            outer.inner.items[0].len()
        }
    "#,
    );
    let main = &module.functions[0];
    let plan = hew_sir::place_plan(main, &module.aggregate_shapes, &module.type_facts).unwrap();
    let loans: Vec<_> = main
        .blocks
        .iter()
        .flat_map(|b| &b.ops)
        .filter_map(|op| match &op.kind {
            SemOpKind::LoadBorrow { place } => {
                let projection = plan.projection(*place).unwrap();
                assert_eq!(
                    projection
                        .path
                        .iter()
                        .map(|step| step.field)
                        .collect::<Vec<_>>(),
                    [0, 0]
                );
                Some(op.results[0].id)
            }
            _ => None,
        })
        .collect();
    assert_eq!(loans.len(), 1, "the vector is the only borrowed field");
    module
}
