//! Retire an enum shell whose declared-close `#[resource]` payload has moved
//! into a match binder.
//!
//! Carved out of `mod.rs` as a coherent lowering concern (the line-ceiling
//! ratchet's intended remedy): one pass, one rule, no IR change.

use super::{
    drop_plan, shift_instr_spans_on_insert, BasicBlock, Builder, HashMap, Instr, Place, ResolvedTy,
};
use crate::lower::base_local;

/// Resolve the enum-shell owner whose sole selected payload is a declared-close
/// resource that `neutralize` just handed to a binder. The single-field layout
/// check is the proof that retiring the shell cannot strand an owned sibling
/// field. Returning `None` leaves every ambiguous, multi-field, non-resource,
/// and ownerless shape on its existing fail-closed cleanup path.
fn transferred_declared_release_parent(
    neutralize: &Instr,
    state: &HashMap<crate::model::OwnerId, Place>,
    builder: &Builder,
) -> Option<(crate::model::OwnerId, Place)> {
    let Instr::NeutralizePayloadSlot {
        place:
            Place::MachineVariant {
                local: parent_local,
                variant_idx,
                field_idx,
            }
            | Place::EnumVariant {
                local: parent_local,
                variant_idx,
                field_idx,
            },
        transferee: Some(child_place),
        authority: crate::model::NeutralizeAuthority::PayloadBindingTransfer,
    } = neutralize
    else {
        return None;
    };
    let child_ty = base_local(*child_place)
        .and_then(|local| builder.locals.get(local as usize))
        .cloned()?;
    if !matches!(
        drop_plan::resource_drop_fn(&child_ty, &builder.type_classes),
        Some(crate::model::DropFnSpec::UserClose(_))
    ) {
        return None;
    }
    let ResolvedTy::Named { name, args, .. } = builder.locals.get(*parent_local as usize)? else {
        return None;
    };
    let layout = crate::model::find_enum_layout(name, args, &builder.enum_layouts)?;
    if layout.is_indirect {
        return None;
    }
    let variant = layout.variants.get(*variant_idx as usize)?;
    let [field_ty] = variant.field_tys.as_slice() else {
        return None;
    };
    if *field_idx != 0 || builder.subst_ty(field_ty) != child_ty {
        return None;
    }
    let parent_place = Place::Local(*parent_local);
    let parents = state
        .iter()
        .filter_map(|(owner, place)| (*place == parent_place).then_some((*owner, *place)))
        .collect::<Vec<_>>();
    let [parent] = parents.as_slice() else {
        return None;
    };
    Some(*parent)
}

/// End the enum shell's generation at the point its declared-close payload
/// moves into a binder.
///
/// A `PayloadBindingTransfer` neutralize zeroes the variant slot, which is the
/// whole release proof for a POINTER-backed payload: the shell's tag-aware
/// `EnumInPlace` walk null-checks the handle and skips it. A `#[resource]`
/// RECORD payload has no null to skip — the shell's walk calls the user
/// `close(self)` on zeroed storage — so the shell must stop owning the payload
/// on this edge instead. `Result<Handle, string>` matched with `.Ok(h)` closed
/// `h` once through the binder and a second time through the shell, printing a
/// close over a zeroed field (#3070).
///
/// This is the one authority for "the shell no longer owns this payload". It
/// runs over the `PayloadBindingTransfer` neutralizes lowering has emitted by
/// this point rather than only the ones a consuming call discharges, so an
/// implicit binder drop and an explicit `h.close()` retire the shell
/// identically; previously only the explicit consume did.
///
/// SHORTCUT — WHY: the pass runs in `splice_body_ownership_releases`, before
/// `prepare_body_transfers`, so a neutralize that
/// `materialize_explicit_projection_adoptions` authors afterwards is outside
/// its window. That is the same window the discharge-time rule it replaces
/// had, so nothing regresses, and no reduction of that shape is known.
/// WHEN OBSOLETE: when a `#[resource]`-record payload is reported reaching a
/// binder only through that later canonicalization. WHAT: run this pass a
/// second time at the tail of `prepare_body_transfers` — it is idempotent, it
/// skips a Transfer the block already carries.
pub(super) fn release_transferred_declared_release_carriers(
    blocks: &mut [BasicBlock],
    builder: &mut Builder,
) {
    let _timing = crate::timing::stage("release_transferred_declared_release_carriers");
    let states = drop_plan::exact_owner_states(blocks);
    let entries = &states.0;
    for block in blocks {
        let Some(entry_state) = entries.get(&block.id) else {
            continue;
        };
        // The shell's Mint can share a block with the neutralize that hands its
        // payload away (a same-block `if let` / match, and the post-CFG
        // `Mint; Move` canonicalization that authors its own neutralize), so the
        // block-entry state is not enough: replay the owner ops instruction by
        // instruction and resolve each parent against the state at ITS point.
        let mut live = entry_state.clone();
        let mut commits: Vec<(usize, crate::model::OwnerId, Place)> = Vec::new();
        for (index, instruction) in block.instructions.iter().enumerate() {
            if let Some((owner, place)) =
                transferred_declared_release_parent(instruction, &live, builder)
            {
                commits.push((index, owner, place));
            }
            drop_plan::apply_exact_owner_ops(std::slice::from_ref(instruction), &mut live);
        }
        for (index, owner, place) in commits.into_iter().rev() {
            let event = Instr::OwnershipEvent(crate::model::OwnershipEvent::Transfer {
                owner,
                from: place,
                to: None,
                to_owner: None,
                to_ty: None,
            });
            if block.instructions.contains(&event) {
                continue;
            }
            let insert_at = index + 1;
            shift_instr_spans_on_insert(
                &mut builder.instr_spans,
                block.id,
                u32::try_from(insert_at).unwrap_or(u32::MAX),
            );
            block.instructions.insert(insert_at, event);
        }
    }
}
