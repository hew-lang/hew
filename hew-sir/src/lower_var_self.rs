//! Mutable method calls use ordinary callable boundaries and canonical places.

use super::projection::OpenOwner;
use super::{BindingPlace, Builder, PreparedCallee, ScalarAggregateParent};
use crate::{
    BoundaryDecision, BoundaryOperand, Operand, OwnKind, PlaceId, Provenance, SemOpKind,
    SemParamPassing, ValueId,
};
use hew_hir::BindingId;
use hew_hir::{HirExpr, HirExprKind, IntentKind};
use hew_types::{CallTarget, ResolvedTy};

/// Where a `var self` call's receiver comes from and returns to, on the
/// normal edge and, handed back, on the fault edge.
#[derive(Clone)]
pub(super) enum ReceiverSeat {
    /// A place of this body the call took: re-initialized on both edges.
    Taken(PlaceId),
    /// A place a staged call copied: its value stays until the call returns.
    Copied(PlaceId),
    /// A plain SSA binding, rebuilt around the returned receiver.
    Scalar {
        binding: BindingId,
        parents: Vec<ScalarAggregateParent>,
    },
    /// A field beneath an owner the call opened: the owner is closed around
    /// the returned receiver.
    Opened(OpenOwner),
    /// A staged copy of a field beneath a whole owner, assigned back through
    /// the owner.
    StagedBeneath { root: PlaceId, base: BindingPlace },
}

impl Builder<'_, '_> {
    #[allow(
        clippy::too_many_lines,
        reason = "receiver acquisition and normal writeback share one exact source place"
    )]
    pub(super) fn lower_var_self_call(&mut self, expr: &HirExpr) -> Result<ValueId, String> {
        let HirExprKind::VarSelfMethodCall {
            receiver,
            receiver_update,
            call_target,
            args,
            evaluation_order,
            ret_ty,
            receiver_ty,
            ..
        } = &expr.kind
        else {
            return Err("mutable method lowering requires a var-self call".into());
        };
        let receiver_ty = self.ty(receiver_ty);
        let callee = match call_target {
            CallTarget::User(declaration) | CallTarget::ImplMethod(declaration) => self
                .service
                .resolve_direct_call(*declaration, expr.site, &self.substitution)?,
            // A generic body reaches `it.next()` through its bound; the
            // implementation is selected here, from the receiver type this
            // instance substituted.
            CallTarget::StaticTraitMethod {
                declaring_trait,
                method,
            } => self.service.resolve_static_trait_call(
                *declaring_trait,
                *method,
                &receiver_ty,
                expr.site,
                &self.substitution,
            )?,
            _ => {
                return Err(
                    "mutable method requires a resolved direct or static-trait declaration".into(),
                )
            }
        };
        let source = self
            .service
            .table
            .functions_by_item
            .get(&callee.function)
            .ok_or_else(|| "mutable method has no HIR source declaration".to_string())?;
        if source
            .params
            .first()
            .is_none_or(|param| source.var_self_receiver != Some(param.id))
        {
            return Err("mutable method target has no exact var-self receiver contract".into());
        }
        let return_ty = self.ty(ret_ty);
        let signature = callee.signature;
        let dual_return_ty = ResolvedTy::Tuple(vec![return_ty.clone(), receiver_ty.clone()]);
        if receiver.intent != IntentKind::Consume
            || self.ty(&receiver.ty) != receiver_ty
            || self.ty(&expr.ty) != return_ty
            || signature.return_ty != dual_return_ty
            || signature.params.len() != args.len() + 1
            || signature.params[0].ty != receiver_ty
        {
            return Err(format!(
                "mutable method `{}` differs from its checked receiver and dual-return signature: receiver {receiver_ty:?}, result {return_ty:?}, signature {signature:?}",
                self.service.module.defs.path(callee.declaration)
            ));
        }
        self.service.require_type_facts(&receiver_ty)?;
        if *receiver_update == hew_types::ReceiverUpdate::Staged
            && self
                .service
                .checked_facts
                .rows()
                .get(&hew_types::TypeInstanceKey(receiver_ty.clone()))
                .is_none_or(|facts| facts.clone == hew_types::CloneKind::None)
        {
            return Err("staged mutable receiver requires an independent value copy".into());
        }
        let owns_receiver =
            OwnKind::of_ty(&receiver_ty, self.service.checked_facts.rows())? == OwnKind::Owned;
        let passing = if owns_receiver {
            SemParamPassing::Consume
        } else {
            SemParamPassing::ReadOnly
        };
        if signature.params[0].passing != passing {
            return Err(
                "mutable method receiver passing differs from its exact ownership contract".into(),
            );
        }
        let place = self.resolve_mutable_place(receiver)?;
        let provenance = Provenance::Site(expr.site);
        let mut live_before_arguments: std::collections::HashSet<ValueId> =
            self.owned_live.keys().copied().collect();
        let mut loans = Vec::new();
        let mut arguments =
            self.lower_user_arguments(args, evaluation_order, &signature.params[1..], &mut loans)?;

        // Later arguments may replace this binding or one of its sibling fields.
        // Acquire its current value only after those effects have completed.
        let staged = *receiver_update == hew_types::ReceiverUpdate::Staged;
        let (value, seat) = self.acquire_receiver(
            &place,
            &receiver_ty,
            owns_receiver,
            staged,
            &mut arguments,
            &mut loans,
            &provenance,
        )?;
        // An opened owner's other fields outlive the call; they are not call
        // temporaries.
        if let ReceiverSeat::Opened(owner) = &seat {
            live_before_arguments.extend(owner.siblings());
        }
        let value = if owns_receiver {
            self.owned_live.remove(&value);
            self.emit_typed(
                provenance.clone(),
                &receiver_ty,
                SemOpKind::Move {
                    source: Operand { value },
                },
            )?
        } else {
            value
        };
        arguments.insert(
            0,
            BoundaryOperand {
                operand: Operand { value },
                decision: if owns_receiver {
                    BoundaryDecision::Move
                } else {
                    BoundaryDecision::Copy
                },
            },
        );
        // A failing callee hands the receiver back as last written; it goes
        // where the normal edge would publish it. A staged receiver's place
        // kept its value, so the handed-back copy is released instead.
        let handback = signature
            .hands_back_receiver()
            .then(|| super::FaultHandback {
                ty: receiver_ty.clone(),
                own: if owns_receiver {
                    OwnKind::Owned
                } else {
                    OwnKind::None
                },
                seat: (!staged).then(|| seat.clone()),
                provenance: provenance.clone(),
            });
        // Argument temporaries are released only once the receiver is back in
        // its place: a temporary's close can fail, and cleanup must then find
        // the receiver's owner whole.
        let temporaries: Vec<ValueId> = self
            .owned_live
            .keys()
            .filter(|value| {
                !live_before_arguments.contains(value)
                    && !arguments.iter().any(|argument| {
                        argument.decision == BoundaryDecision::Move
                            && argument.operand.value == **value
                    })
            })
            .copied()
            .collect();
        live_before_arguments.extend(temporaries.iter().copied());
        let result = self
            .finish_user_call(
                PreparedCallee::Direct(callee.id),
                signature,
                arguments,
                &loans,
                &live_before_arguments,
                true,
                handback,
            )?
            .ok_or_else(|| "mutable method did not return its result and receiver".to_string())?;
        let shape = self.service.require_aggregate_shape(&dual_return_ty)?;
        let fields =
            self.emit_destructure_value(result, &dual_return_ty, shape, provenance.clone())?;
        self.publish_receiver(seat, fields[1].id, provenance)?;
        for value in temporaries.into_iter().rev() {
            self.emit_destroy(value)?;
        }
        Ok(fields[0].id)
    }

    /// Take, copy or open the receiver's place for the call, and name where
    /// the receiver returns to.
    #[allow(
        clippy::too_many_arguments,
        reason = "the receiver place, its ownership and the call's argument loans are one acquisition"
    )]
    fn acquire_receiver(
        &mut self,
        place: &BindingPlace,
        receiver_ty: &ResolvedTy,
        owns_receiver: bool,
        staged: bool,
        arguments: &mut [BoundaryOperand],
        loans: &mut Vec<ValueId>,
        provenance: &Provenance,
    ) -> Result<(ValueId, ReceiverSeat), String> {
        if let Some(selected) = self.owned_projection(place)? {
            let root = self.place_borrow_root(selected)?;
            self.snapshot_arguments_rooted_at(root, arguments, loans, provenance)?;
            // The call takes its receiver's place, which is re-initialized on
            // both edges. A staged receiver works on a copy instead.
            let take = !staged;
            let value = self.emit_typed(
                provenance.clone(),
                receiver_ty,
                if take {
                    SemOpKind::LoadTake { place: selected }
                } else {
                    SemOpKind::LoadCopy { place: selected }
                },
            )?;
            let seat = if take {
                ReceiverSeat::Taken(selected)
            } else {
                ReceiverSeat::Copied(selected)
            };
            return Ok((value, seat));
        }
        if let Some(root) = self.whole_owner_root(place)? {
            let borrow_root = self.place_borrow_root(root)?;
            self.snapshot_arguments_rooted_at(borrow_root, arguments, loans, provenance)?;
            if staged {
                let value = self.copy_through_whole_owner(
                    root,
                    place,
                    provenance,
                    "a staged machine step",
                )?;
                let seat = ReceiverSeat::StagedBeneath {
                    root,
                    base: place.clone(),
                };
                return Ok((value, seat));
            }
            let (value, owner) = self.open_owner(root, place, provenance)?;
            return Ok((value, ReceiverSeat::Opened(owner)));
        }
        if owns_receiver {
            return Err("mutable method receiver has no owning place".into());
        }
        let (value, parents) = self.take_scalar_place(place, provenance)?;
        Ok((
            value,
            ReceiverSeat::Scalar {
                binding: place.binding,
                parents,
            },
        ))
    }

    /// Return a `var self` receiver to the place it came from.
    pub(super) fn publish_receiver(
        &mut self,
        seat: ReceiverSeat,
        value: ValueId,
        provenance: Provenance,
    ) -> Result<(), String> {
        match seat {
            ReceiverSeat::Taken(place) => self.restore_taken_place(place, value, provenance),
            ReceiverSeat::Copied(place) => self.store_projected(place, value, provenance),
            ReceiverSeat::Scalar { binding, parents } => {
                self.replace_scalar_aggregate_leaf(binding, value, parents, &provenance)
            }
            ReceiverSeat::Opened(owner) => self.close_owner(owner, value, provenance),
            ReceiverSeat::StagedBeneath { root, base } => {
                self.assign_through_whole_owner(root, &base, value, provenance)
            }
        }
    }
}
