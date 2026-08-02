//! Direct structural tests for `derive_spawn_consumed_handle_bindings` and its
//! load-bearing effect on the W3.053 gate. A Sink/Stream half moved into an
//! actor initial-state record consumed by `SpawnActor` is owned by the actor's
//! synthesised `state_drop_fn`, so its source binding's standalone drop is
//! removed and the gate must admit it. The negative control disables the
//! derivation (empty `source_excluded`) and confirms the gate then REFUSES —
//! proving the exclusion is not a no-op (LESSONS
//! drop-allowset-from-value-flow: include a negative control).
use super::*;

fn is_refused(findings: &[MirCheck], binding: BindingId) -> bool {
    findings.iter().any(|c| {
        matches!(
            c,
            MirCheck::OwnedHandleAggregateDoubleFree { binding: b, .. } if *b == binding
        )
    })
}

fn sink_ty() -> ResolvedTy {
    ResolvedTy::named_builtin("Sink", BuiltinType::Sink, vec![ResolvedTy::String])
}

fn writer_state_ty() -> ResolvedTy {
    ResolvedTy::Named {
        name: "Writer".to_string(),
        args: vec![],
        builtin: None,
        is_opaque: false,
    }
}

/// sink(local 1) → state record(local 2) via `RecordInit`, consumed by
/// `SpawnActor` (handle local 3). The canonical `spawn Writer(sink: sink)`
/// shape.
fn spawn_blocks() -> Vec<BasicBlock> {
    vec![BasicBlock {
        id: 0,
        statements: vec![],
        instructions: vec![
            Instr::RecordInit {
                ty: writer_state_ty(),
                fields: vec![(FieldOffset(0), Place::Local(1))],
                dest: Place::Local(2),
            },
            Instr::SpawnActor {
                actor_name: "Writer".to_string(),
                state: Some(Place::Local(2)),
                init_args: vec![],
                dest: Place::ActorHandle(3),
                max_heap_bytes: None,
                cycle_capable: false,
                mailbox_capacity: None,
                overflow_policy: None,
            },
        ],
        terminator: Terminator::Return,
    }]
}

#[allow(
    clippy::type_complexity,
    reason = "test fixture returns the four detector inputs as a tuple"
)]
fn setup() -> (
    BindingId,
    HashMap<BindingId, Place>,
    Vec<(BindingId, String, ResolvedTy)>,
    Vec<ResolvedTy>,
) {
    let sink = BindingId(1);
    let mut binding_locals = HashMap::new();
    binding_locals.insert(sink, Place::Local(1));
    let owned = vec![(sink, "sink".to_string(), sink_ty())];
    let mut local_tys = vec![ResolvedTy::I64; 4];
    local_tys[1] = sink_ty();
    local_tys[2] = writer_state_ty();
    (sink, binding_locals, owned, local_tys)
}

#[test]
fn sink_into_spawn_state_is_derived_as_excluded() {
    let (sink, binding_locals, owned, local_tys) = setup();
    let excluded =
        derive_spawn_consumed_handle_bindings(&spawn_blocks(), &owned, &binding_locals, &local_tys);
    assert!(
        excluded.contains(&sink),
        "a Sink half moved into an actor initial-state record consumed by \
         SpawnActor must be derived as spawn-consumed; got {excluded:?}"
    );
}

#[test]
fn spawn_consumed_sink_admitted_with_exclusion_refused_without() {
    let (sink, binding_locals, owned, local_tys) = setup();
    let blocks = spawn_blocks();
    // Negative control: derivation disabled (empty source_excluded) → the
    // source's standalone drop is counted and the SpawnActor escape poisons
    // the origin, so the gate REFUSES.
    let refused_without = detect_unproven_aggregate_handle_double_free(
        &blocks,
        &HashMap::new(),
        &owned,
        &binding_locals,
        &local_tys,
        &HashMap::new(),
        &[],
        &HashSet::new(),
        &HashSet::new(),
    );
    assert!(
        is_refused(&refused_without, sink),
        "without the spawn-consumed exclusion the gate must refuse the moved \
         handle (negative control); got {refused_without:?}"
    );
    // With the derivation feeding `source_excluded` → exactly one free (the
    // actor state_drop_fn), so the gate ADMITS.
    let excluded =
        derive_spawn_consumed_handle_bindings(&blocks, &owned, &binding_locals, &local_tys);
    let findings = detect_unproven_aggregate_handle_double_free(
        &blocks,
        &HashMap::new(),
        &owned,
        &binding_locals,
        &local_tys,
        &HashMap::new(),
        &[],
        &excluded,
        &HashSet::new(),
    );
    assert!(
        !is_refused(&findings, sink),
        "with the spawn-consumed exclusion the gate must admit the moved \
         handle; got {findings:?}"
    );
}

#[test]
fn sink_also_returned_is_not_excluded() {
    // A handle flowing BOTH into a spawn-state record AND the ReturnSlot has
    // two candidate owners → left refused fail-closed (not derived).
    let (sink, binding_locals, owned, local_tys) = setup();
    let mut blocks = spawn_blocks();
    blocks[0].instructions.push(Instr::Move {
        dest: Place::ReturnSlot,
        src: Place::Local(1),
    });
    let excluded =
        derive_spawn_consumed_handle_bindings(&blocks, &owned, &binding_locals, &local_tys);
    assert!(
        !excluded.contains(&sink),
        "a handle also moved to the ReturnSlot must NOT be spawn-consumed \
         excluded (fail-closed); got {excluded:?}"
    );
}
