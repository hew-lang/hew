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

fn parent(function: &SemFunction, loan: ValueId) -> ValueId {
    function
        .blocks
        .iter()
        .flat_map(|block| &block.ops)
        .find(|op| op.results.iter().any(|result| result.id == loan))
        .unwrap()
        .kind
        .borrow_parent()
        .unwrap()
        .value
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
        let function = &mut module.functions[0];
        let leaf = leaf_loan(function);
        let parent = parent(function, leaf);
        let value = if end_owner {
            self::parent(function, parent)
        } else {
            parent
        };
        move_cleanup_before_read(function, value);
        assert!(verify_module(&module).iter().any(|error| matches!(
            error.kind,
            SirDiagnosticKind::OwnershipLifetime { reason, .. }
                if reason == "value cannot be consumed or ended while a dependent borrow is live"
        )));
    }
}

#[test]
fn runtime_read_cannot_use_an_ended_field_loan() {
    let mut module = fixture::nested_borrow_module();
    let function = &mut module.functions[0];
    let leaf = leaf_loan(function);
    move_cleanup_before_read(function, leaf);
    assert!(verify_module(&module).iter().any(|error| matches!(
        error.kind,
        SirDiagnosticKind::OwnershipLifetime { value, reason, .. }
            if value == leaf && reason == "borrow is not live on every incoming path"
    )));
}

#[test]
fn every_fault_edge_must_end_its_local_loans() {
    let mut module = fixture::nested_borrow_module();
    let function = &mut module.functions[0];
    let leaf = leaf_loan(function);
    let fault = function
        .blocks
        .iter_mut()
        .find(|block| matches!(block.terminator, SemTerminator::Trap { .. }))
        .unwrap();
    let before = fault.ops.len();
    fault
        .ops
        .retain(|op| !matches!(&op.kind, SemOpKind::EndBorrow { borrow } if borrow.value == leaf));
    assert_eq!(fault.ops.len(), before - 1);
    assert!(verify_module(&module).iter().any(|error| matches!(
        error.kind,
        SirDiagnosticKind::OwnershipLifetime { value, reason, .. }
            if value == leaf && reason == "local borrow remains live at exit"
    )));
}

#[test]
fn borrowed_fields_cannot_be_destroyed_as_owned_values() {
    let mut module = fixture::nested_borrow_module();
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
