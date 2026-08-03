use super::{
    Builder, HashSet, HirExpr, MirDiagnostic, MirDiagnosticKind, Place, ProducedValueOwnership,
    ResolvedTy, ValueClass, SYNTHETIC_TEMP_ARG_NAME,
};

impl Builder {
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
            let ownership = self
                .param_ownership
                .produced_value_facts
                .get(&arg.site)
                .map(|fact| fact.ownership);
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
            let callee_borrows = if matches!(owned_ty, ResolvedTy::String) {
                // Runtime string receivers already receive exactly one inline
                // release. Only an analyzed Hew function borrows the anonymous
                // string through the ordinary caller-owner path.
                !crate::runtime_symbols::callee_ownership_contract(callee_symbol)
                    .borrows_string_call_args()
                    && self.callee_is_analyzed_hew_arg_sink(callee_symbol)
            } else {
                proven_borrow_args.contains(&index)
            };
            if !callee_borrows {
                continue;
            }
            if matches!(ownership, Some(ProducedValueOwnership::Unknown) | None) {
                self.typed_produced_value_demand_is_resolved(
                    arg,
                    "borrowing argument ownership is unresolved",
                );
                continue;
            }
            if !matches!(ownership, Some(ProducedValueOwnership::Owned { .. })) {
                continue;
            }
            let Some(Place::Local(local)) = arg_places.get(index).copied() else {
                continue;
            };
            if self.parameter_locals.contains(&local) {
                continue;
            }
            let owners = self.finalize_typed_produced_value_owners(
                SYNTHETIC_TEMP_ARG_NAME,
                arg.site,
                Place::Local(local),
            );
            if owners.is_empty() {
                if self.typed_projection_has_live_parent_owner(arg) {
                    continue;
                }
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::NotYetImplemented {
                        construct: "owned borrowing argument without provisional owner".to_string(),
                        site: arg.site,
                    },
                    note: "the typed owned argument must publish its exact MIR generation before the borrowing call sink"
                        .to_string(),
                });
                continue;
            }
            if owners
                .iter()
                .any(|(_, published_ty)| *published_ty != owned_ty)
            {
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::NotYetImplemented {
                        construct: "borrowing argument owner changed type at handoff".to_string(),
                        site: arg.site,
                    },
                    note: format!(
                        "typed argument has type {owned_ty:?}, but its provisional owners are {owners:?}"
                    ),
                });
                continue;
            }
            if matches!(owned_ty, ResolvedTy::TraitObject { .. }) {
                for (binding, _) in owners {
                    self.dyn_trait_storage
                        .insert(binding, crate::TraitObjectStorage::HeapBoxed);
                }
            }
        }
    }
}
