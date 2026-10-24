//! The checker-published layout of every trait object the program names
//! (D540). One [`TraitObjectLayout`] per canonical `dyn` type is the only
//! slot list in the compiler: a dispatch names a slot by its 0-based
//! position here, and the slot carries the boundary signature and the
//! suspension effect its trait method declares (A421).

use super::generics::DynLayoutSlot;
#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
use super::*;
use crate::DefId;

/// How a slot's receiver crosses the trait-object boundary, as the trait
/// method declares it.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DynReceiver {
    /// `self`: the call borrows the boxed value.
    Borrow,
    /// `var self`: the call mutates the boxed value in place.
    BorrowMut,
    /// `consume self`: the call consumes the trait object.
    Consume,
}

/// Whether a dispatch through a slot may suspend its caller. A trait method
/// declaration is a written boundary: it suspends only when declared
/// `fn[suspends]` (A421).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SlotEffect {
    Plain,
    Suspends,
}

/// One method of a trait object's layout.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DynSlot {
    pub declaring_trait: DefId,
    pub trait_args: Vec<Ty>,
    /// The trait method declaration this slot dispatches.
    pub method: DefId,
    pub receiver: DynReceiver,
    /// Caller-side signature with the trait object's type arguments and
    /// associated-type bindings substituted; the receiver is not a parameter.
    pub signature: FnSig,
    pub effect: SlotEffect,
}

/// The dispatch layout of one canonical trait object: its bounds and their
/// supertraits, supertraits first, and one slot per distinct trait method.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TraitObjectLayout {
    /// Every trait in the closure, in layout order.
    pub closure: Vec<DefId>,
    /// Slot `s` is `slots[s]`.
    pub slots: Vec<DynSlot>,
}

impl TraitObjectLayout {
    /// The 0-based slot that dispatches `method`.
    #[must_use]
    pub fn slot_of(&self, method: DefId) -> Option<u32> {
        let index = self.slots.iter().position(|slot| slot.method == method)?;
        u32::try_from(index).ok()
    }
}

impl Checker {
    /// Record the layout the walk in [`Checker::dyn_layout`] computed for
    /// `traits`, so it is published once per trait object.
    pub(super) fn record_trait_object_layout(
        &mut self,
        traits: &[crate::ty::TraitObjectBound],
        closure: Vec<DefId>,
        walk: &[DynLayoutSlot],
    ) -> Result<(), String> {
        if self.trait_object_layouts.contains_key(traits) {
            return Ok(());
        }
        let mut slots = Vec::with_capacity(walk.len());
        for slot in walk {
            let Some(mut signature) = self.lookup_trait_method(slot.trait_id, &slot.method_name)
            else {
                return Err(format!(
                    "trait method `{}` has no signature",
                    slot.method_name
                ));
            };
            let applied: Vec<_> = traits
                .iter()
                .filter_map(|bound| {
                    Some(self.applied_trait_closure(
                        &TraitRef {
                            trait_id: bound.trait_id?,
                            args: bound.args.clone(),
                            assoc: Vec::new(),
                        },
                        None,
                    ))
                })
                .flatten()
                .collect();
            let mut arguments = applied
                .iter()
                .filter(|bound| bound.trait_id == slot.declaring_trait)
                .map(|bound| &bound.args);
            let trait_args = arguments.next().cloned().ok_or_else(|| {
                format!(
                    "trait method `{}` has no applied declaring bound",
                    slot.method_name
                )
            })?;
            if arguments.any(|args| *args != trait_args) {
                return Err(format!(
                    "trait method `{}` is inherited with conflicting type arguments",
                    slot.method_name
                ));
            }
            self.apply_trait_object_bound_substitutions(&mut signature, &traits[slot.bound]);
            slots.push(DynSlot {
                declaring_trait: slot.declaring_trait,
                trait_args,
                method: slot.method,
                receiver: slot.receiver,
                signature,
                effect: slot.effect,
            });
        }
        self.trait_object_layouts
            .insert(traits.to_vec(), TraitObjectLayout { closure, slots });
        Ok(())
    }

    /// The canonical trait-object type an erasure targets. Its type
    /// arguments must be known at the erasure.
    pub(super) fn dyn_coercion_target(
        &mut self,
        traits: &[crate::ty::TraitObjectBound],
        span: &Span,
    ) -> Option<ResolvedTy> {
        let target = self.finalize_type_for_handoff(&Ty::TraitObject {
            traits: traits.to_vec(),
        });
        if let Ok(resolved) = ResolvedTy::from_ty(&target) {
            return Some(resolved);
        }
        self.report_error(
            TypeErrorKind::InvalidOperation,
            span,
            format!(
                "cannot erase into `{}`: its type arguments are not known here; \
                 annotate the trait object's type",
                target.user_facing()
            ),
        );
        None
    }

    /// The effect of the slot that dispatches `method` on a trait object of
    /// `traits`. A layout is recorded before any dispatch through it is.
    pub(super) fn dyn_slot_effect(
        &self,
        traits: &[crate::ty::TraitObjectBound],
        method: DefId,
    ) -> Option<SlotEffect> {
        let layout = self.trait_object_layouts.get(traits)?;
        let slot = layout.slot_of(method)?;
        Some(layout.slots[slot as usize].effect)
    }

    /// Publish the recorded layouts keyed by their finalized trait-object
    /// type. A layout whose type does not resolve names no value that
    /// reaches lowering.
    pub(super) fn finalize_trait_object_layouts(
        &mut self,
    ) -> std::collections::BTreeMap<ResolvedTy, TraitObjectLayout> {
        let mut published = std::collections::BTreeMap::new();
        for (traits, mut layout) in std::mem::take(&mut self.trait_object_layouts) {
            let ty = self.finalize_type_for_handoff(&Ty::TraitObject { traits });
            let Ok(key) = ResolvedTy::from_ty(&ty) else {
                continue;
            };
            for slot in &mut layout.slots {
                for argument in &mut slot.trait_args {
                    *argument = self.finalize_type_for_handoff(argument);
                }
                for param in &mut slot.signature.params {
                    param.ty = self.finalize_type_for_handoff(&param.ty);
                }
                slot.signature.return_type =
                    self.finalize_type_for_handoff(&slot.signature.return_type);
            }
            published.insert(key, layout);
        }
        published
    }
}
