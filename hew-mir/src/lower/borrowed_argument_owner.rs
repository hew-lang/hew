use super::{
    Builder, HashSet, HirExpr, MirDiagnostic, MirDiagnosticKind, Place, ProducedValueOwnership,
    ResolvedTy, ValueClass, SYNTHETIC_TEMP_ARG_NAME, SYNTHETIC_TEMP_RECEIVER_NAME,
};

#[derive(Clone, Copy)]
enum BorrowedValueRole {
    Argument,
    Receiver,
}

impl BorrowedValueRole {
    const fn label(self) -> &'static str {
        match self {
            Self::Argument => "argument",
            Self::Receiver => "receiver",
        }
    }
}

impl Builder {
    fn finalize_borrowed_value_owner(
        &mut self,
        value: &HirExpr,
        place: Place,
        role: BorrowedValueRole,
    ) {
        if self.borrowed_runtime_result_places.contains(&place) {
            // A receiver-interior result can be borrowed by the next call, but
            // it cannot become that call site's synthetic temporary owner.
            // The collection retains the sole destructor authority; minting
            // here would drop the aliased element after the call and corrupt
            // the still-live collection.
            return;
        }
        let ownership = self
            .param_ownership
            .produced_value_facts
            .get(&value.site)
            .map(|fact| fact.ownership);
        let owned_ty = self.subst_ty(&value.ty);
        if ValueClass::of_ty(&owned_ty, &self.type_classes) == ValueClass::Linear
            || (!matches!(owned_ty, ResolvedTy::TraitObject { .. })
                && !crate::model::ty_owns_heap_mir(
                    &owned_ty,
                    &self.record_field_orders,
                    &self.enum_layouts,
                ))
        {
            return;
        }
        if matches!(ownership, Some(ProducedValueOwnership::Unknown) | None) {
            let construct = match role {
                BorrowedValueRole::Argument => "borrowing argument ownership is unresolved",
                BorrowedValueRole::Receiver => "borrowing receiver ownership is unresolved",
            };
            self.typed_produced_value_demand_is_resolved(value, construct);
            return;
        }
        if !matches!(ownership, Some(ProducedValueOwnership::Owned { .. })) {
            return;
        }
        let Place::Local(local) = place else {
            return;
        };
        if self.parameter_locals.contains(&local) {
            return;
        }
        let owners = self.finalize_typed_produced_value_owners(
            match role {
                BorrowedValueRole::Argument => SYNTHETIC_TEMP_ARG_NAME,
                BorrowedValueRole::Receiver => SYNTHETIC_TEMP_RECEIVER_NAME,
            },
            value.site,
            Place::Local(local),
        );
        if owners.is_empty() {
            if self.typed_projection_has_live_parent_owner(value)
                || matches!(role, BorrowedValueRole::Receiver)
            {
                return;
            }
            let construct = format!("owned borrowing {} without provisional owner", role.label());
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct,
                    site: value.site,
                },
                note: format!(
                    "the typed owned {} must publish its exact MIR generation before the borrowing {} sink",
                    role.label(),
                    if matches!(role, BorrowedValueRole::Argument) {
                        "call"
                    } else {
                        "method"
                    }
                ),
            });
            return;
        }
        if owners
            .iter()
            .any(|(_, published_ty)| *published_ty != owned_ty)
        {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: format!("borrowing {} owner changed type at handoff", role.label()),
                    site: value.site,
                },
                note: format!(
                    "typed {} has type {owned_ty:?}, but its provisional owners are [{}]",
                    role.label(),
                    owners
                        .iter()
                        .map(|(owner, published_ty)| format!("{owner}: {published_ty:?}"))
                        .collect::<Vec<_>>()
                        .join(", ")
                ),
            });
            return;
        }
        if matches!(owned_ty, ResolvedTy::TraitObject { .. }) {
            for (binding, _) in owners {
                self.dyn_trait_storage
                    .insert(binding, crate::TraitObjectStorage::HeapBoxed);
            }
        }
    }

    /// Hand a borrowing method's anonymous owned receiver from its typed
    /// publication generation to the ordinary scope-exit planner.
    pub(super) fn finalize_borrowed_receiver_owner(
        &mut self,
        receiver: &HirExpr,
        receiver_place: Place,
    ) {
        if !self
            .param_ownership
            .produced_value_facts
            .get(&receiver.site)
            .is_some_and(|fact| matches!(fact.ownership, ProducedValueOwnership::Owned { .. }))
        {
            return;
        }
        self.finalize_borrowed_value_owner(receiver, receiver_place, BorrowedValueRole::Receiver);
    }

    /// Hand a borrowing call's anonymous owned arguments from their typed
    /// publication generation to the ordinary scope-exit planner.
    pub(super) fn finalize_borrowed_argument_owners(
        &mut self,
        callee_symbol: &str,
        hir_args: &[HirExpr],
        arg_places: &[Place],
        proven_borrow_args: &HashSet<usize>,
    ) {
        for (index, arg) in hir_args.iter().enumerate() {
            let owned_ty = self.subst_ty(&arg.ty);
            if ValueClass::of_ty(&owned_ty, &self.type_classes) == ValueClass::Linear
                || (!matches!(owned_ty, ResolvedTy::TraitObject { .. })
                    && !crate::model::ty_owns_heap_mir(
                        &owned_ty,
                        &self.record_field_orders,
                        &self.enum_layouts,
                    ))
            {
                continue;
            }
            let runtime_contract = crate::runtime_symbols::callee_ownership_contract(callee_symbol);
            if matches!(owned_ty, ResolvedTy::String) && runtime_contract.borrows_string_call_args()
            {
                // The runtime call consumes the field load's retained read-copy
                // through its existing inline drop. Complete an independently
                // owned temporary aggregate behind that projection as well, so
                // its original fields reach the exit LIFO.
                self.finalize_typed_projection_parent_owner(arg);
            }
            let callee_borrows = if matches!(owned_ty, ResolvedTy::String) {
                // Runtime string receivers already receive exactly one inline
                // release. Only an analyzed Hew function borrows the anonymous
                // string through the ordinary caller-owner path.
                !runtime_contract.borrows_string_call_args()
                    && self.callee_is_analyzed_hew_arg_sink(callee_symbol)
            } else {
                proven_borrow_args.contains(&index)
            };
            if !callee_borrows {
                continue;
            }
            let Some(place) = arg_places.get(index).copied() else {
                continue;
            };
            self.finalize_borrowed_value_owner(arg, place, BorrowedValueRole::Argument);
        }
    }
}
