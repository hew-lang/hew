#[path = "support/defer.rs"]
mod fixture;

#[test]
fn inline_defer_bodies_preserve_mutable_places_and_optional_faults() {
    for failing in [false, true] {
        for module in [fixture::module(failing), fixture::nested(failing)] {
            let diagnostics = hew_sir::verify_module(&module);
            assert!(diagnostics.is_empty(), "{diagnostics:?}");
        }
    }
}

fn rejects(mut module: hew_sir::SemModule, reason: &str) {
    fixture::normalize(&mut module);
    let diagnostics = hew_sir::verify_module(&module);
    assert!(
        diagnostics.iter().any(|d| match &d.kind {
            hew_sir::SirDiagnosticKind::InvalidTerminator { reason: actual }
            | hew_sir::SirDiagnosticKind::InvalidOperation { reason: actual, .. } =>
                actual == reason,
            hew_sir::SirDiagnosticKind::FaultLifetime { reason: actual, .. }
            | hew_sir::SirDiagnosticKind::OwnershipLifetime { reason: actual, .. }
            | hew_sir::SirDiagnosticKind::PlaceLifetime { reason: actual, .. } => *actual == reason,
            _ => false,
        }),
        "expected {reason:?}: {diagnostics:?}"
    );
}

#[test]
fn defer_dependencies_reserve_availability_but_allow_replacement_and_own_consume() {
    use hew_sir::*;
    let mut replacement = fixture::module(false);
    fixture::probe(&mut replacement).blocks[0]
        .ops
        .push(fixture::op(SemOpKind::StoreAssign {
            place: PlaceId(0),
            value: fixture::operand(10),
        }));
    fixture::normalize(&mut replacement);
    assert!(verify_module(&replacement).is_empty());

    let mut own_consume = fixture::module(false);
    fixture::probe(&mut own_consume).blocks[9].ops[0].kind =
        SemOpKind::LoadTake { place: PlaceId(0) };
    assert!(
        verify_module(&own_consume).is_empty(),
        "called action has already popped its reservation"
    );

    let mut stolen = fixture::module(false);
    fixture::probe(&mut stolen).blocks[4].ops[0].kind = SemOpKind::LoadTake { place: PlaceId(0) };
    rejects(
        stolen,
        "consume or end would invalidate a pending defer dependency",
    );

    let mut ended = fixture::module(false);
    fixture::probe(&mut ended).blocks[0]
        .ops
        .push(fixture::op(SemOpKind::EndLifetime { place: PlaceId(0) }));
    rejects(
        ended,
        "consume or end would invalidate a pending defer dependency",
    );
}

#[test]
fn defer_rejects_reused_registration_wrong_finish_and_skipped_older_action() {
    use hew_sir::*;
    let mut repeated = fixture::module(true);
    let registration = fixture::probe(&mut repeated).blocks[0].ops[4].clone();
    fixture::probe(&mut repeated).blocks[0]
        .ops
        .push(registration);
    rejects(
        repeated,
        "defer identity has more than one registration operation",
    );

    let mut wrong = fixture::module(true);
    let SemTerminator::FinishDefer { park, .. } =
        &mut fixture::probe(&mut wrong).blocks[7].terminator
    else {
        unreachable!()
    };
    *park = FaultParkId(1);
    rejects(wrong, "defer finish uses the wrong fault park");

    let mut skipped = fixture::module(true);
    let SemTerminator::FinishDefer { next, .. } =
        &mut fixture::probe(&mut skipped).blocks[7].terminator
    else {
        unreachable!()
    };
    *next = fixture::edge(12);
    rejects(skipped, "exit leaves pending actions or live fault parks");
}

#[test]
fn defer_rejects_overwritten_nested_park_and_outstanding_body_storage() {
    use hew_sir::*;
    let mut overwritten = fixture::nested(true);
    for block in &mut fixture::probe(&mut overwritten).blocks {
        match &mut block.terminator {
            SemTerminator::EnterDefer {
                defer: DeferId(3),
                park,
                ..
            }
            | SemTerminator::FinishDefer {
                defer: DeferId(3),
                park,
                ..
            } => *park = FaultParkId(0),
            _ => {}
        }
    }
    rejects(overwritten, "defer entry would overwrite a live fault park");

    let mut local = fixture::nested(true);
    fixture::probe(&mut local).blocks[18].ops.pop();
    rejects(local, "defer finish leaves body-local storage active");

    let mut loan = fixture::nested(true);
    fixture::probe(&mut loan).blocks[18].ops.remove(0);
    rejects(loan, "defer finish leaves a body-local owner or loan live");
}

#[test]
fn checked_raise_requires_its_exact_producing_failure_edge() {
    use hew_sir::*;
    let mut wrong = fixture::module(true);
    let SemTerminator::CheckedRaiseFault { kind, .. } =
        &mut fixture::probe(&mut wrong).blocks[15].terminator
    else {
        unreachable!()
    };
    *kind = TrapKind::DivideByZero;
    rejects(
        wrong,
        "checked raise does not match its producing failure edge",
    );

    let mut invented = fixture::module(true);
    fixture::probe(&mut invented).blocks[2].terminator = SemTerminator::CheckedRaiseFault {
        kind: TrapKind::IntegerOverflow,
        cleanup: fixture::edge(3),
    };
    rejects(
        invented,
        "checked raise does not match its producing failure edge",
    );
}

#[test]
fn defer_body_loops_are_legal_but_failed_calls_must_advance() {
    use hew_sir::*;
    let mut looping = fixture::module(true);
    let function = fixture::probe(&mut looping);
    function.blocks[6].terminator = SemTerminator::Branch {
        condition: Operand {
            value: function.params[0].value,
        },
        then_target: fixture::edge(6),
        else_target: fixture::edge(7),
    };
    assert!(verify_module(&looping).is_empty());

    let mut stuck = looping;
    let function = fixture::probe(&mut stuck);
    function.blocks.push(fixture::block(
        16,
        vec![],
        SemTerminator::Goto(fixture::edge(16)),
    ));
    let SemTerminator::Call { unwind, .. } = &mut function.blocks[5].terminator else {
        unreachable!()
    };
    *unwind = CallUnwind::Cleanup(fixture::edge(16));
    rejects(
        stuck,
        "defer call failure must leave through bounded cleanup",
    );
}

#[test]
fn defer_calls_require_transitive_non_suspending_effects() {
    use hew_sir::*;
    let mut missing = fixture::module(true);
    missing
        .functions
        .retain(|f| f.declaration.full_path() != "fail");
    rejects(missing, "defer call has no proven non-suspending body");

    let mut suspending = fixture::module(true);
    let callee = suspending
        .functions
        .iter_mut()
        .find(|f| f.declaration.full_path() == "fail")
        .unwrap();
    callee.blocks[0].terminator = SemTerminator::RtCall {
        id: OpId(0),
        family: hew_types::RuntimeCallFamily::DuplexClose,
        args: vec![],
        result: CallResult::Unit,
        normal: fixture::edge(1),
        unwind: CallUnwind::NotApplicable,
    };
    rejects(suspending, "defer call has unproven or suspending effects");
}

#[test]
fn defer_rejects_active_fault_calls_and_failed_result_reads() {
    use hew_sir::*;
    let mut active = fixture::module(true);
    let function = fixture::probe(&mut active);
    function.blocks[1].terminator = SemTerminator::Panic {
        message: fixture::boundary(function.params[1].value, BoundaryDecision::Borrow),
        cleanup: fixture::edge(16),
    };
    let mut call = function.blocks[5].terminator.clone();
    let SemTerminator::Call { normal, unwind, .. } = &mut call else {
        unreachable!()
    };
    *normal = fixture::edge(3);
    *unwind = CallUnwind::Cleanup(fixture::edge(3));
    function.blocks.push(fixture::block(16, vec![], call));
    rejects(active, "active fault cannot be abandoned or overwritten");

    // The call defines a scalar only on success. Even a no-drop read on its
    // failure edge must be rejected by the ordinary SSA availability proof.
    let mut failed = fixture::module(true);
    let function = fixture::probe(&mut failed);
    let args: Vec<_> = function
        .params
        .iter()
        .enumerate()
        .map(|(index, param)| {
            fixture::boundary(
                param.value,
                if index == 0 {
                    BoundaryDecision::Copy
                } else {
                    BoundaryDecision::Borrow
                },
            )
        })
        .collect();
    let SemTerminator::Call {
        result,
        callee,
        args: call_args,
        normal,
        ..
    } = &mut function.blocks[5].terminator
    else {
        unreachable!()
    };
    *callee = function.callable;
    *call_args = args;
    *result = CallResult::Value(ValueDef {
        id: ValueId(90),
        ty: hew_types::ResolvedTy::I64,
        own: OwnKind::None,
    });
    normal.args.push(fixture::operand(90));
    function.blocks[6].args.push(BlockArg {
        value: ValueId(92),
        ty: hew_types::ResolvedTy::I64,
        own: OwnKind::None,
    });
    fixture::normalize(&mut failed);
    assert!(
        verify_module(&failed).is_empty(),
        "normal result forwarding remains valid"
    );
    let function = fixture::probe(&mut failed);
    let SemTerminator::Call {
        unwind: CallUnwind::Cleanup(unwind),
        ..
    } = &mut function.blocks[5].terminator
    else {
        unreachable!()
    };
    unwind.args.push(fixture::operand(90));
    function.blocks[7].args.push(BlockArg {
        value: ValueId(91),
        ty: hew_types::ResolvedTy::I64,
        own: OwnKind::None,
    });
    let diagnostics = verify_module(&failed);
    assert!(
        diagnostics.iter().any(|d| matches!(
            &d.kind,
            SirDiagnosticKind::InvalidCallResultUse {
                value: ValueId(90),
                ..
            }
        )),
        "{diagnostics:?}"
    );
}
