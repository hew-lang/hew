//! End-to-end Lane B Slice 4b verification against the real Lane A
//! machine fixtures.
//!
//! Loads `examples/machine/traffic_light.hew` and `tcp_handshake.hew`,
//! drives the parser → HIR → MIR pipeline, and asserts the synthesised
//! `<Name>__step` function has the Slice 4b dispatch tree (multi-block
//! CFG with `Terminator::Return` arms reachable plus
//! `MachineDispatchUnreachable` as the default fall-through). The
//! transition bodies must lower past MIR — no synthetic trap shim, no
//! `Unsupported` diagnostic for the machine surface.

use hew_hir::{lower_program, ResolutionCtx};
use hew_mir::{lower_hir_module, Instr, Place, Terminator, TrapKind};
use hew_types::TypeCheckOutput;

fn pipeline_for(path: &str) -> hew_mir::IrPipeline {
    let source = std::fs::read_to_string(path).unwrap_or_else(|e| panic!("read {path}: {e}"));
    let parsed = hew_parser::parse(&source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors for {path}: {:?}",
        parsed.errors
    );
    let output = lower_program(
        &parsed.program,
        &TypeCheckOutput::default(),
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    lower_hir_module(&output.module)
}

#[test]
fn traffic_light_step_fn_has_real_dispatch_tree() {
    let pipeline = pipeline_for("../examples/machine/traffic_light.hew");
    let step_fn = pipeline
        .raw_mir
        .iter()
        .find(|f| f.name == "TrafficLight__step")
        .expect("TrafficLight__step synthesised");

    // Multi-block CFG (entry + per-state checks + per-arm checks + bodies + trap).
    assert!(
        step_fn.blocks.len() > 1,
        "traffic_light dispatch tree spans multiple blocks; got {}",
        step_fn.blocks.len()
    );

    // Default arm: at least one block traps with MachineDispatchUnreachable.
    let trap_blocks: Vec<_> = step_fn
        .blocks
        .iter()
        .filter(|b| {
            matches!(
                b.terminator,
                Terminator::Trap {
                    kind: TrapKind::MachineDispatchUnreachable,
                }
            )
        })
        .collect();
    assert!(
        !trap_blocks.is_empty(),
        "fail-closed default arm: at least one block traps"
    );

    // Matched arms: at least one block returns the next-state value.
    let return_blocks: Vec<_> = step_fn
        .blocks
        .iter()
        .filter(|b| matches!(b.terminator, Terminator::Return))
        .collect();
    assert!(
        !return_blocks.is_empty(),
        "matched transition arms terminate in Terminator::Return"
    );

    // The dispatch tree must read the state tag (Place::MachineTag) and
    // event tag (Instr::EnumTagLoad). Without these, no real dispatch.
    let has_state_tag_read = step_fn.blocks.iter().any(|b| {
        b.instructions.iter().any(|i| {
            matches!(
                i,
                Instr::Move {
                    src: Place::MachineTag(_),
                    ..
                }
            )
        })
    });
    assert!(
        has_state_tag_read,
        "dispatch reads state via Place::MachineTag"
    );
    let has_event_tag_load = step_fn.blocks.iter().any(|b| {
        b.instructions
            .iter()
            .any(|i| matches!(i, Instr::EnumTagLoad { .. }))
    });
    assert!(
        has_event_tag_load,
        "dispatch reads event via Instr::EnumTagLoad"
    );

    // Transition body produces a machine value: at least one block writes
    // a Place::MachineTag (for the next-state constructor).
    let has_state_tag_write = step_fn.blocks.iter().any(|b| {
        b.instructions.iter().any(|i| {
            matches!(
                i,
                Instr::Move {
                    dest: Place::MachineTag(_),
                    ..
                }
            )
        })
    });
    assert!(
        has_state_tag_write,
        "transition body constructs next state by writing Place::MachineTag"
    );
}

#[test]
fn tcp_handshake_step_fn_has_real_dispatch_tree_with_payload_and_wildcards() {
    let pipeline = pipeline_for("../examples/machine/tcp_handshake.hew");
    let step_fn = pipeline
        .raw_mir
        .iter()
        .find(|f| f.name == "TcpHandshake__step")
        .expect("TcpHandshake__step synthesised");

    // Multi-block CFG.
    assert!(
        step_fn.blocks.len() > 1,
        "tcp_handshake dispatch tree spans multiple blocks; got {}",
        step_fn.blocks.len()
    );

    // At least one block traps (default arm).
    assert!(
        step_fn.blocks.iter().any(|b| matches!(
            b.terminator,
            Terminator::Trap {
                kind: TrapKind::MachineDispatchUnreachable,
            }
        )),
        "fail-closed default arm present"
    );

    // Multiple matched-arm Return blocks (TCP handshake has 13 transitions).
    let return_count = step_fn
        .blocks
        .iter()
        .filter(|b| matches!(b.terminator, Terminator::Return))
        .count();
    assert!(
        return_count >= 4,
        "tcp_handshake has multiple transitions; expected >= 4 Return blocks, got {return_count}"
    );

    // Payload reads via Place::MachineVariant (e.g. self.remote_port).
    let has_variant_read = step_fn.blocks.iter().any(|b| {
        b.instructions.iter().any(|i| {
            matches!(
                i,
                Instr::Move {
                    src: Place::MachineVariant { .. },
                    ..
                }
            )
        })
    });
    assert!(
        has_variant_read,
        "self.field reads use Place::MachineVariant addressing"
    );

    // Payload writes via Place::MachineVariant (e.g. SynReceived ctor).
    let has_variant_write = step_fn.blocks.iter().any(|b| {
        b.instructions.iter().any(|i| {
            matches!(
                i,
                Instr::Move {
                    dest: Place::MachineVariant { .. },
                    ..
                }
            )
        })
    });
    assert!(
        has_variant_write,
        "transition body constructs payload fields via Place::MachineVariant"
    );

    // Emit placeholders preserved (Slice 7 retrofits to runtime queue).
    let has_emit_placeholder = step_fn.blocks.iter().any(|b| {
        b.instructions
            .iter()
            .any(|i| matches!(i, Instr::MachineEmitPlaceholder { .. }))
    });
    assert!(
        has_emit_placeholder,
        "emit expressions in transition bodies lower to MachineEmitPlaceholder"
    );
}

#[test]
fn default_machine_step_fn_falls_through_to_stay_return_not_trap() {
    // A `default { state }` machine must NOT trap on an unhandled cell — the
    // dispatch fall-through returns `self` unchanged (the unhandled-⇒-stay
    // semantics). This is the MIR side of the Q379 fix; the runtime oracle is
    // `examples/machine/run_default_stay.expected`.
    let pipeline = pipeline_for("../examples/machine/run_default_stay.hew");
    let step_fn = pipeline
        .raw_mir
        .iter()
        .find(|f| f.name == "Tank__step")
        .expect("Tank__step synthesised");

    // No block may trap with MachineDispatchUnreachable: the default arm
    // replaces the fail-closed trap with a stay-return.
    assert!(
        !step_fn.blocks.iter().any(|b| matches!(
            b.terminator,
            Terminator::Trap {
                kind: TrapKind::MachineDispatchUnreachable,
            }
        )),
        "default machine fall-through must not trap; it stays in the current state"
    );

    // The fall-through returns: at least one Return block writes the return
    // slot from the `self` parameter (Local 0) — the stay path.
    let stays_via_self_return = step_fn.blocks.iter().any(|b| {
        matches!(b.terminator, Terminator::Return)
            && b.instructions.iter().any(|i| {
                matches!(
                    i,
                    Instr::Move {
                        dest: Place::ReturnSlot,
                        src: Place::Local(0),
                    }
                )
            })
    });
    assert!(
        stays_via_self_return,
        "default fall-through returns `self` (Local 0) unchanged into the return slot"
    );
}
