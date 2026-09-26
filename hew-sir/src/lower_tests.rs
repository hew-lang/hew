//! Builder unit tests.

use super::{
    is_initial_value_type, require_initial_scalar_read, require_initial_value_transfer,
    PendingBlock,
};
use crate::ownership::{OwnKind, TypeFactTable};
use crate::{BlockId, OpId, Provenance, SemOp, SemOpKind, SemParamPassing, SemTerminator};
use hew_hir::IntentKind;
use hew_types::{ResolvedTy, TypeFactContext, TypeFactService};

#[test]
fn only_a_read_intent_reaches_an_initial_scalar_operand() {
    assert_eq!(Ok(()), require_initial_scalar_read(IntentKind::Read));

    for intent in [
        IntentKind::Modify,
        IntentKind::Consume,
        IntentKind::Discharge,
        IntentKind::Capture,
        IntentKind::Yield,
        IntentKind::Unknown,
    ] {
        let reason = require_initial_scalar_read(intent)
            .expect_err("a non-read HIR intent must not become a scalar SIR operand");
        assert!(
                reason.contains("ownership operation")
                    || reason.contains("requires")
                    || reason.contains("not a legal"),
                "the failure must explain why {intent:?} is outside the current SIR ownership domain: {reason}"
            );
    }
}

#[test]
fn scalar_and_tuple_binding_transfers_admit_only_bitcopy_values() {
    assert!(require_initial_value_transfer(IntentKind::Consume, &ResolvedTy::I64, "test").is_ok());
    assert!(require_initial_value_transfer(IntentKind::Read, &ResolvedTy::Bool, "test").is_ok());
    let tuple = ResolvedTy::Tuple(vec![
        ResolvedTy::I64,
        ResolvedTy::Tuple(vec![ResolvedTy::Bool]),
    ]);
    assert!(is_initial_value_type(&tuple));
    assert!(require_initial_value_transfer(IntentKind::Consume, &tuple, "test").is_ok());
    for intent in [IntentKind::Read, IntentKind::Consume] {
        let error = require_initial_value_transfer(intent, &ResolvedTy::String, "test").expect_err(
            "an ownership-bearing transfer must stay outside the SIR value-only subset",
        );
        assert!(
            error.contains("ownership-bearing")
                && error.contains("only aliases BitCopy scalar/tuple"),
            "the transfer diagnostic must explain that this would erase ownership: {error}",
        );
    }
}

/// The ownership kind of every value this lowering mints comes from a
/// published row. A missing row is never reclassified locally.
#[test]
fn missing_rows_are_never_reclassified_by_lowering() {
    let none = TypeFactTable::new();
    for ty in [
        ResolvedTy::I64,
        ResolvedTy::Tuple(vec![ResolvedTy::I64, ResolvedTy::Bool]),
        ResolvedTy::String,
        conn_ty(),
    ] {
        let refused =
            OwnKind::of_ty(&ty, &none).expect_err("every missing concrete row must be refused");
        assert!(
            refused.contains("concrete type facts are missing"),
            "{refused}"
        );
    }

    let mut service = TypeFactService::new(TypeFactContext::default(), none);
    service.require(&ResolvedTy::I64).unwrap();
    service.require(&ResolvedTy::Bool).unwrap();
    let tuple = ResolvedTy::Tuple(vec![ResolvedTy::I64, ResolvedTy::Bool]);
    service.require(&tuple).unwrap();
    service.require(&ResolvedTy::String).unwrap();
    assert_eq!(Ok(OwnKind::None), OwnKind::of_ty(&tuple, service.rows()));
    assert_eq!(
        Ok(OwnKind::Owned),
        OwnKind::of_ty(&ResolvedTy::String, service.rows())
    );
}

/// The checker's row is the authority the lowering reads: a user
/// declaration the class rule cannot reach on its own is decided by its
/// published row, so the same type is refused without one and owning with
/// it.
#[test]
fn a_published_row_decides_a_kind_the_empty_context_refuses() {
    let mut facts = TypeFactTable::new();
    facts.insert(
        hew_types::TypeInstanceKey(conn_ty()),
        hew_types::TypeFacts {
            class: hew_types::ValueClass::AffineResource,
            clone: hew_types::CloneKind::None,
            send: hew_types::SendFact::Known(true),
            hash: false,
            eq: false,
        },
    );
    assert!(OwnKind::of_ty(&conn_ty(), &TypeFactTable::new()).is_err());
    assert_eq!(Ok(OwnKind::Owned), OwnKind::of_ty(&conn_ty(), &facts));
}

/// §1.2 rule 3: a parameter whose header slot is `Borrow` is `Guaranteed`
/// for the whole body whatever its type's class says, and the same type in
/// a `ReadOnly` slot keeps the class table's kind. Without the slot read,
/// a borrowed parameter presents as an `Owned` value the callee owes a
/// consuming use it must never make.
#[test]
fn a_borrow_slot_parameter_is_guaranteed_whatever_its_class_says() {
    let none = TypeFactTable::new();
    assert_eq!(
        Ok(OwnKind::Guaranteed),
        OwnKind::of_param(&ResolvedTy::String, SemParamPassing::Borrow, &none)
    );
    assert!(OwnKind::of_param(&ResolvedTy::String, SemParamPassing::ReadOnly, &none).is_err());
    let mut service = TypeFactService::new(TypeFactContext::default(), none.clone());
    service.require(&ResolvedTy::String).unwrap();
    assert_eq!(
        Ok(OwnKind::Owned),
        OwnKind::of_param(
            &ResolvedTy::String,
            SemParamPassing::ReadOnly,
            service.rows()
        )
    );
    // The slot decides before the class rule is consulted, so a type the
    // rule cannot decide is still `Guaranteed` in a borrow slot.
    assert_eq!(
        Ok(OwnKind::Guaranteed),
        OwnKind::of_param(&conn_ty(), SemParamPassing::Borrow, &none)
    );
    assert!(OwnKind::of_param(&conn_ty(), SemParamPassing::ReadOnly, &none).is_err());
}

#[test]
fn consuming_parameters_require_concrete_owners() {
    let mut service = TypeFactService::new(TypeFactContext::default(), TypeFactTable::new());
    service.require(&ResolvedTy::String).unwrap();
    service.require(&ResolvedTy::I64).unwrap();
    assert_eq!(
        OwnKind::of_param(
            &ResolvedTy::String,
            SemParamPassing::Consume,
            service.rows()
        ),
        Ok(OwnKind::Owned)
    );
    assert!(OwnKind::of_param(&ResolvedTy::I64, SemParamPassing::Consume, service.rows()).is_err());
    assert!(OwnKind::of_param(&conn_ty(), SemParamPassing::Consume, service.rows()).is_err());
    assert_eq!(
        OwnKind::of_param(
            &ResolvedTy::String,
            SemParamPassing::BorrowMut,
            service.rows()
        ),
        Ok(OwnKind::Guaranteed)
    );
}

fn conn_ty() -> ResolvedTy {
    ResolvedTy::named_for_test("Conn", vec![])
}

#[test]
fn pending_blocks_do_not_conflate_open_with_semantic_unreachable() {
    let open = PendingBlock::new(BlockId(0), Vec::new());
    assert!(open.is_open());
    assert!(open
        .into_sem_block()
        .expect_err("an unfilled builder block must fail finalization")
        .contains("without a semantic terminator"));

    let mut completed = PendingBlock::new(BlockId(1), Vec::new());
    completed.terminator = Some(SemTerminator::Unreachable);
    assert!(!completed.is_open());
    let error = completed
        .append_op(SemOp {
            id: OpId(0),
            results: Vec::new(),
            kind: SemOpKind::ConstInteger(0),
            provenance: Provenance::Synthesized,
        })
        .expect_err("semantic unreachable must close the builder block");
    assert!(error.contains("after completed block bb1"));
    assert!(matches!(
        completed
            .into_sem_block()
            .expect("semantic unreachable is a completed block")
            .terminator,
        SemTerminator::Unreachable
    ));
}

#[test]
fn pending_blocks_reject_operations_after_a_semantic_terminator() {
    let mut completed = PendingBlock::new(BlockId(0), Vec::new());
    completed.terminator = Some(SemTerminator::Return { value: None });
    let error = completed
        .append_op(SemOp {
            id: OpId(0),
            results: Vec::new(),
            kind: SemOpKind::ConstInteger(0),
            provenance: Provenance::Synthesized,
        })
        .expect_err("completed blocks must reject late operations");
    assert!(error.contains("after completed block bb0"));
    assert!(completed.ops.is_empty());
}
