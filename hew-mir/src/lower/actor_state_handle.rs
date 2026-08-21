use super::{
    base_local, render_owned_handle_ty, AggregateOwner, BasicBlock, BindingId, HashMap, HashSet,
    Instr, MirCheck, ResolvedTy, Terminator,
};

/// True when overwriting this actor-state field kind leaks its previous handle (#2654).
///
/// The gated kinds are those whose scope-exit / actor-shutdown drop runs a real
/// close AND whose `ActorStateFieldStore` has NO release-before-store today:
///
///   - [`StateFieldCloneKind::Resource`] — a user `#[resource] #[opaque]` handle
///     whose `close(self)` runs once in `__hew_state_drop_<A>`; the store's
///     `lower_actor_state_field_store` no-release arm groups it with the other
///     opaque kinds and emits a bare `store` (the leak this gate fences).
///   - [`StateFieldCloneKind::IoHandle`] with a pointer-backed handle whose drop
///     actually frees/closes (`Stream`→`hew_stream_close`, `Sink`→`hew_sink_close`,
///     `Generator`→`hew_gen_coro_destroy`, `CancellationToken`→
///     `hew_cancel_token_release`). Same no-release store arm; the descriptor /
///     coroutine frame / refcount of the OLD handle leaks on overwrite.
///
/// NOT gated (no leak on overwrite, so no refusal):
///   - kinds with an existing release-before-store — `String`/`Bytes`/`Vec`/
///     `HashMap`/`HashSet` go through `emit_state_field_old_value_release`'s
///     pointer-inequality guard, which releases the old payload before the store;
///   - no-drop `IoHandle` kinds; clone refusal never transfers drop authority
///     to the runtime;
///   - no-close `OpaqueHandle` (e.g. `json.Value`) and `BitCopy` — no owned
///     resource to leak.
fn actor_state_kind_leaks_on_overwrite(kind: &crate::state_clone::StateFieldCloneKind) -> bool {
    use crate::state_clone::{IoHandleKind, StateFieldCloneKind};
    matches!(
        kind,
        StateFieldCloneKind::Resource { .. }
            | StateFieldCloneKind::IoHandle {
                kind: IoHandleKind::Stream
                    | IoHandleKind::Sink
                    | IoHandleKind::Generator
                    | IoHandleKind::CancellationToken,
            }
    )
}

/// #2654 fail-closed gate: refuse an in-place overwrite of an actor-state field
/// (`self.dq = src`, lowering to `Instr::ActorStateFieldStore`) whose classified
/// kind leaks the previous handle on overwrite.
///
/// This is the actor-state sibling of the record `RecordFieldStore` overwrite arm
/// in [`detect_opaque_resource_field_misuse`]: the SAME exactly-once-close
/// invariant, the SAME fail-closed posture (LESSONS boundary-fail-closed,
/// raii-null-after-move, lifecycle-symmetry). Codegen's actor-state store has no
/// release-before-store for these kinds (`lower_actor_state_field_store`
/// no-release arm; `emit_overwrite_neutralize_leaves` null-the-slot arms), so the
/// old handle's `close` never runs (leak) while the actor's shutdown drop
/// (`__hew_state_drop_<A>`) double-owns the freshly-stored handle. A safe
/// release-before-store is deferred to RAII-2 (retain-to-compare + source-slot
/// null-after-move); until then the operation is refused, exactly as the
/// structurally identical record store already is on HEAD.
///
/// Reuses the authoritative per-field classification
/// ([`ActorLayout::state_field_clone_kinds`]) — the same vector the drop-body
/// synthesis and clone/drop registration consume — so the gate cannot drift from
/// the exact kinds whose drop it fences.
///
/// Overwrite only: actor-state resource *extraction* (`let x = self.dq`, giving
/// `x` independent drop authority) is a distinct latent concern, out of scope for
/// #2654 — this gate never inspects `ActorStateFieldLoad`.
pub(super) fn detect_actor_state_resource_overwrite(
    blocks: &[BasicBlock],
    state_field_clone_kinds: &[crate::state_clone::StateFieldCloneKind],
    state_field_names: &[String],
    state_field_tys: &[ResolvedTy],
) -> Vec<MirCheck> {
    if state_field_clone_kinds.is_empty() {
        return Vec::new();
    }
    let mut findings = Vec::new();
    let mut seen: HashSet<u32> = HashSet::new();
    for block in blocks {
        for instr in &block.instructions {
            let Instr::ActorStateFieldStore { field_offset, .. } = instr else {
                continue;
            };
            let idx = field_offset.0;
            let Some(kind) = state_field_clone_kinds.get(idx as usize) else {
                continue;
            };
            if !actor_state_kind_leaks_on_overwrite(kind) || !seen.insert(idx) {
                continue;
            }
            // Name the violation by the mutated state field; fall back to the
            // rendered handle type when the layout carries no name (mirrors the
            // record gate's `name_for`).
            let name = state_field_names
                .get(idx as usize)
                .filter(|n| !n.is_empty())
                .cloned()
                .unwrap_or_else(|| {
                    state_field_tys
                        .get(idx as usize)
                        .map_or_else(|| format!("field{idx}"), render_owned_handle_ty)
                });
            let handle_ty = state_field_tys
                .get(idx as usize)
                .map_or_else(|| name.clone(), render_owned_handle_ty);
            findings.push(MirCheck::OwnedHandleAggregateDoubleFree {
                // No source-level `BindingId` for an actor-state field; the
                // field offset is a stable synthetic id (dedup is by offset
                // above, and `check_to_diagnostic` ignores the binding).
                binding: BindingId(idx),
                name,
                handle_ty,
                overwrite: true,
                owner: AggregateOwner::ActorState,
            });
        }
    }
    findings
}

/// CAP-08 fail-closed gate: refuse an explicit CONSUMING close on an owned
/// builtin-handle held in ACTOR STATE (`sink.close()` on the bare state field —
/// an `Instr::ActorStateFieldLoad` whose `dest` becomes the receiver of a
/// `consumes_receiver` runtime call, e.g. `hew_sink_close` / `hew_stream_close`).
///
/// The handle is owned by the actor's synthesised `state_drop_fn`, which closes
/// it EXACTLY ONCE at teardown (Stream->`hew_stream_close` / Sink->`hew_sink_close`;
/// the runtime close is an UNGUARDED `Box::from_raw`). A handler that also closes
/// it frees the one runtime context twice → a double-free (verified: exit 139
/// under `MallocScribble`; two `hew_sink_close` sites in the emitted IR). This is
/// the consume/extraction sibling of [`detect_actor_state_resource_overwrite`]
/// (the store/overwrite gate) — the SAME exactly-once-close invariant, the SAME
/// fail-closed posture, and it mirrors the `#[resource]` handle posture
/// (`detect_opaque_resource_field_misuse` refuses `h.dq.close()`): a resource
/// handle in actor state is closed only by teardown.
///
/// The consuming close is a `Terminator::Call` whose `builtin` family reports
/// `consumes_receiver()`; the receiver is `args[0]`, traced back (through
/// whole-value `Move`) to the `ActorStateFieldLoad` dest. Reuses the same
/// authoritative per-field classification the overwrite gate consumes
/// (`ActorLayout::state_field_clone_kinds` → `actor_state_kind_leaks_on_overwrite`)
/// so the two gates fence exactly the same close-bearing handle set.
///
/// Direction: refuse rather than emit the double-free (over-refusal is a compile
/// error, never a UAF). To signal EOF, close a sink owned as a LOCAL, or let the
/// actor's teardown close the state-held half.
///
/// LESSONS: boundary-fail-closed, raii-null-after-move, cleanup-all-exits,
/// lifecycle-symmetry.
pub(super) fn detect_actor_state_handle_consume(
    blocks: &[BasicBlock],
    state_field_clone_kinds: &[crate::state_clone::StateFieldCloneKind],
    state_field_names: &[String],
    state_field_tys: &[ResolvedTy],
) -> Vec<MirCheck> {
    if state_field_clone_kinds.is_empty() {
        return Vec::new();
    }
    // Locals carrying a close-bearing handle loaded out of an actor state field,
    // mapped to their field offset; grown forward through whole-value `Move`.
    let mut handle_field_local: HashMap<u32, u32> = HashMap::new();
    for block in blocks {
        for instr in &block.instructions {
            if let Instr::ActorStateFieldLoad {
                field_offset, dest, ..
            } = instr
            {
                let idx = field_offset.0;
                let Some(kind) = state_field_clone_kinds.get(idx as usize) else {
                    continue;
                };
                if !actor_state_kind_leaks_on_overwrite(kind) {
                    continue;
                }
                if let Some(dl) = base_local(*dest) {
                    handle_field_local.insert(dl, idx);
                }
            }
        }
    }
    if handle_field_local.is_empty() {
        return Vec::new();
    }
    loop {
        let mut changed = false;
        for block in blocks {
            for instr in &block.instructions {
                if let Instr::Move { dest, src } = instr {
                    if let (Some(sl), Some(dl)) = (base_local(*src), base_local(*dest)) {
                        if let Some(&idx) = handle_field_local.get(&sl) {
                            if handle_field_local.insert(dl, idx).is_none() {
                                changed = true;
                            }
                        }
                    }
                }
            }
        }
        if !changed {
            break;
        }
    }
    let mut findings = Vec::new();
    let mut seen: HashSet<u32> = HashSet::new();
    for block in blocks {
        let Terminator::Call {
            authority: crate::CallAuthority::Runtime(family),
            args,
            ..
        } = &block.terminator
        else {
            continue;
        };
        if !family.consumes_receiver() {
            continue;
        }
        let Some(receiver) = args.first() else {
            continue;
        };
        let Some(rl) = base_local(*receiver) else {
            continue;
        };
        let Some(&idx) = handle_field_local.get(&rl) else {
            continue;
        };
        if !seen.insert(idx) {
            continue;
        }
        let name = state_field_names
            .get(idx as usize)
            .filter(|n| !n.is_empty())
            .cloned()
            .unwrap_or_else(|| {
                state_field_tys
                    .get(idx as usize)
                    .map_or_else(|| format!("field{idx}"), render_owned_handle_ty)
            });
        let handle_ty = state_field_tys
            .get(idx as usize)
            .map_or_else(|| name.clone(), render_owned_handle_ty);
        findings.push(MirCheck::OwnedHandleAggregateDoubleFree {
            binding: BindingId(idx),
            name,
            handle_ty,
            overwrite: false,
            owner: AggregateOwner::ActorState,
        });
    }
    findings
}
