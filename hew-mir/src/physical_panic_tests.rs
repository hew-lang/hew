use super::*;

#[path = "../../hew-sir/tests/support/panic.rs"]
mod fixture;

fn physical(semantic: &SemModule) -> PhysicalModule {
    lower_physical_module(semantic, super::tests::target_for_inventory(semantic))
        .unwrap()
        .into_unverified()
}

#[test]
fn panic_requires_a_live_borrowed_string_and_finite_fault_cleanup() {
    for owned in [false, true] {
        let semantic = fixture::module(owned);
        physical(&semantic);
        for corruption in 0..if owned { 12 } else { 9 } {
            let mut module = semantic.clone();
            let f = module
                .functions
                .iter_mut()
                .find(|f| f.declaration.full_path() == "panic_probe")
                .unwrap();
            match corruption {
                0 => {
                    let SemTerminator::Panic { message, .. } = &mut f.blocks[0].terminator else {
                        unreachable!()
                    };
                    message.decision = BoundaryDecision::Move;
                }
                1 => {
                    let SemTerminator::Panic { message, .. } = &mut f.blocks[0].terminator else {
                        unreachable!()
                    };
                    message.operand.value = ValueId(999);
                }
                2 => f.blocks[1].terminator = SemTerminator::Return { value: None },
                3 => {
                    f.blocks[1].terminator = SemTerminator::Goto(Edge {
                        target: BlockId(1),
                        args: vec![],
                    });
                }
                4 => f.blocks[1].terminator = f.blocks[0].terminator.clone(),
                5 => {
                    f.blocks[1].terminator = SemTerminator::Trap {
                        kind: TrapKind::DivideByZero,
                    }
                }
                6 => {
                    f.blocks[0].terminator = SemTerminator::Goto(Edge {
                        target: BlockId(1),
                        args: vec![],
                    });
                }
                7 | 8 => {
                    let scalar = hew_sir::SemOp {
                        id: hew_sir::OpId(99),
                        kind: SemOpKind::ConstI64(7),
                        results: vec![hew_sir::ValueDef {
                            id: ValueId(99),
                            ty: ResolvedTy::I64,
                            own: OwnKind::None,
                        }],
                        provenance: hew_sir::Provenance::Synthesized,
                    };
                    if corruption == 7 {
                        f.blocks[0].ops.push(scalar);
                        let SemTerminator::Panic { message, .. } = &mut f.blocks[0].terminator
                        else {
                            unreachable!()
                        };
                        message.operand.value = ValueId(99);
                    } else {
                        f.blocks[1].ops.insert(0, scalar);
                    }
                }
                9 => {
                    f.blocks[1].ops.remove(0);
                }
                10 => {
                    f.blocks[1].ops.pop();
                }
                11 => f.blocks[1].ops.swap(0, 1),
                _ => unreachable!(),
            }
            let diagnostics = hew_sir::verify_module(&module);
            assert!(
                !diagnostics.is_empty(),
                "accepted corruption {corruption}, owned {owned}"
            );
            assert!(
                lower_physical_module(&module, super::tests::target_for_inventory(&module))
                    .is_err()
            );
        }
    }
}

#[test]
fn physical_panic_cannot_change_transfer_or_replace_fault_cleanup() {
    for owned in [false, true] {
        let baseline = physical(&fixture::module(owned));
        for corruption in 0..if owned { 9 } else { 6 } {
            let mut module = baseline.clone();
            let id = module
                .callables
                .iter()
                .find(|c| c.declaration.full_path() == "panic_probe")
                .unwrap()
                .id;
            let f = module
                .functions
                .iter_mut()
                .find(|f| f.callable == id)
                .unwrap();
            match corruption {
                0 => {
                    let PhysicalTerminator::Panic { message, .. } = &mut f.blocks[0].terminator
                    else {
                        unreachable!()
                    };
                    let ArgumentTransfer::Borrow(source) = *message else {
                        unreachable!()
                    };
                    *message = ArgumentTransfer::Move(source);
                }
                1 => {
                    f.blocks[1].terminator = PhysicalTerminator::Goto(PhysicalEdge {
                        target: BlockId(1),
                        transfers: vec![],
                        leaf_transfers: vec![],
                    });
                }
                2 => f.blocks[1].terminator = f.blocks[0].terminator.clone(),
                3 => f.blocks[1].terminator = PhysicalTerminator::Trap(TrapKind::DivideByZero),
                4 => {
                    f.blocks[0].terminator = PhysicalTerminator::Goto(PhysicalEdge {
                        target: BlockId(1),
                        transfers: vec![],
                        leaf_transfers: vec![],
                    });
                }
                5 => {
                    let PhysicalTerminator::Panic { cleanup, .. } = &mut f.blocks[0].terminator
                    else {
                        unreachable!()
                    };
                    cleanup.target = BlockId(999);
                }
                6 => {
                    f.blocks[1].ops.remove(0);
                }
                7 => {
                    f.blocks[1].ops.pop();
                }
                8 => f.blocks[1].ops.swap(0, 1),
                _ => unreachable!(),
            }
            assert!(
                verify_physical_module(&module).is_err(),
                "accepted corruption {corruption}, owned {owned}"
            );
        }
    }
}
