use super::{
    outbound_record_layouts, ty_is_indirect_enum, BindingId, Builder, CaptureEnvOwnedLoad,
    HirBinding, HirExpr, Instr, MirDiagnostic, MirDiagnosticKind, OwnedCarrierNeutralizeTarget,
    OwnedCarrierParam, PendingOwnedCallArg, PendingOwnedCallSite, Place, SiteId, SnapshotFieldKind,
};

impl Builder {
    /// Propagate the original carrier authority through one aggregate field
    /// load. Non-string owned fields are byte-copy aliases, so a later move
    /// must neutralize the root-relative source path before terminal cleanup.
    pub(crate) fn note_carrier_projection(
        &mut self,
        aggregate: Place,
        field_index: u32,
        dest: Place,
        field_ty: &hew_types::ResolvedTy,
    ) {
        let Some(authority) = self.owned_carrier_neutralize.get(&aggregate).cloned() else {
            return;
        };
        let record_layouts = outbound_record_layouts(self);
        let Ok(plan) = crate::state_clone::classify_value_snapshot_plan_with_resource_handles(
            field_ty,
            &record_layouts,
            &self.enum_layouts,
            &self.opaque_handle_names,
            &self.resource_opaque_close,
        ) else {
            return;
        };
        if matches!(
            plan.root(),
            SnapshotFieldKind::BitCopy { .. } | SnapshotFieldKind::String
        ) {
            return;
        }
        let (root, mut fields) = match authority {
            OwnedCarrierNeutralizeTarget::Whole(root) => (root, Vec::new()),
            OwnedCarrierNeutralizeTarget::Projection { root, fields } => (root, fields),
        };
        fields.push(field_index);
        self.owned_carrier_neutralize.insert(
            dest,
            OwnedCarrierNeutralizeTarget::Projection { root, fields },
        );
    }

    /// Save the raw argument places for the post-CFG owned-carrier pass.
    pub(crate) fn note_owned_call_site(
        &mut self,
        callee_item: Option<hew_hir::ItemId>,
        hir_args: &[HirExpr],
        arg_places: &[Place],
    ) {
        let Some(callee_item) = callee_item else {
            return;
        };
        let args: Vec<PendingOwnedCallArg> = hir_args
            .iter()
            .zip(arg_places.iter().copied())
            .enumerate()
            .filter_map(|(index, (arg, source))| {
                let owned_ty = self.subst_ty(&arg.ty);
                // An indirect enum local is an owning pointer slot, not the
                // inline tagged-union storage consumed by the structural
                // snapshot protocol. Its existing match/move authority remains
                // responsible until an allocating node-clone protocol exists.
                (!ty_is_indirect_enum(&owned_ty, &self.enum_layouts)
                    && self
                        .param_ownership
                        .call_param_owned_carrier
                        .get(&(callee_item, index))
                        .copied()
                        == Some(true))
                .then_some(PendingOwnedCallArg {
                    index,
                    source,
                    ty: owned_ty,
                    site: arg.site,
                })
            })
            .collect();
        if args.is_empty() {
            return;
        }
        let replaced = self
            .pending_owned_call_args
            .insert(self.current_block_id, PendingOwnedCallSite { args });
        debug_assert!(replaced.is_none(), "one call terminator per basic block");
    }

    /// Register the callee half of an owned direct-call carrier contract.
    pub(crate) fn register_owned_call_carrier_param(
        &mut self,
        func_id: hew_hir::ItemId,
        index: usize,
        param: &HirBinding,
        slot: Place,
        param_is_consumed: bool,
    ) -> bool {
        let owned_ty = self.subst_ty(&param.ty);
        let is_carrier = !param_is_consumed
            && self
                .param_ownership
                .call_param_owned_carrier
                .get(&(func_id, index))
                .copied()
                == Some(true)
            // Indirect enums use a pointer-slot representation and have their
            // own move/match/drop authority. Structural enum snapshots accept
            // inline tagged-union storage only.
            && !ty_is_indirect_enum(&owned_ty, &self.enum_layouts);
        if !is_carrier {
            return false;
        }

        let record_layouts = outbound_record_layouts(self);
        match crate::state_clone::classify_value_snapshot_plan_with_resource_handles(
            &owned_ty,
            &record_layouts,
            &self.enum_layouts,
            &self.opaque_handle_names,
            &self.resource_opaque_close,
        ) {
            Ok(plan)
                if !matches!(plan.root(), SnapshotFieldKind::BitCopy { .. })
                    && plan
                        .is_clone_total(
                            &record_layouts,
                            &self.enum_layouts,
                            &self.opaque_handle_names,
                            &self.resource_opaque_close,
                        )
                        .unwrap_or(false) =>
            {
                self.owned_carrier_param_ids.insert(param.id);
                self.owned_carrier_params.push(OwnedCarrierParam {
                    value: slot,
                    ty: owned_ty,
                    plan,
                });
                self.owned_carrier_neutralize
                    .insert(slot, OwnedCarrierNeutralizeTarget::Whole(slot));
            }
            Ok(_) => {}
            Err(error) => self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: format!("owned call-carrier parameter `{}`", param.ty.user_facing()),
                    site: SiteId(0),
                },
                note: error.to_string(),
            }),
        }
        true
    }

    pub(crate) fn lower_capture_env_binding_ref(
        &mut self,
        binding: BindingId,
        name: &str,
        site: SiteId,
    ) -> Option<Place> {
        let source = self.capture_env_sources.get(&binding).cloned()?;
        let dest = self.alloc_local(source.ty.clone());
        if self.capture_env_whole_escape_requires_clone(&source.ty) {
            self.capture_env_owned_loads.insert(
                dest,
                CaptureEnvOwnedLoad {
                    name: name.to_string(),
                    ty: source.ty.clone(),
                    site,
                },
            );
        }
        self.push_instr(Instr::ClosureEnvFieldLoad {
            env: source.env,
            env_ty: source.env_ty,
            field_offset: source.field_offset,
            dest,
        });
        Some(dest)
    }

    /// Lower a complete value crossing an ownership boundary. Captured owned
    /// bindings alias still-owning environment fields, so moving one would
    /// create a second drop authority. Borrow and projection roots bypass this
    /// funnel and continue through `lower_value`.
    pub(crate) fn lower_value_for_move(&mut self, expr: &HirExpr) -> Option<Place> {
        if self.reject_capture_env_whole_escape_expr(expr) {
            return None;
        }
        let value = self.lower_value(expr)?;
        let Some(target) = self.owned_carrier_neutralize.remove(&value) else {
            return Some(value);
        };
        match target {
            OwnedCarrierNeutralizeTarget::Whole(source) => {
                let dest = self.alloc_local(self.subst_ty(&expr.ty));
                self.push_instr(Instr::Move { dest, src: value });
                self.push_instr(Instr::NeutralizePayloadSlot { place: source });
                Some(dest)
            }
            OwnedCarrierNeutralizeTarget::Projection { root, fields } => {
                self.push_instr(Instr::AggregateProjectionNeutralize { root, fields });
                Some(value)
            }
        }
    }

    pub(crate) fn lower_let_value(&mut self, binding: BindingId, value: &HirExpr) -> Option<Place> {
        let generator_clone_only = self.prepass_generator_capture_bindings.contains(&binding)
            && !self.prepass_binding_ref_uses.contains(&binding);
        if generator_clone_only {
            let place = self.lower_value(value)?;
            // The generator-capture prepass immediately deep-clones this staged
            // binding into the generator environment; the local is not otherwise
            // read. Preserve that proven clone-only ingress by clearing the
            // whole-capture tag before the let-binding's Move.
            self.capture_env_owned_loads.remove(&place);
            Some(place)
        } else {
            self.lower_value_for_move(value)
        }
    }

    pub(crate) fn lower_method_arg_value(&mut self, arg: &HirExpr, is_move: bool) -> Option<Place> {
        if is_move {
            self.lower_value_for_move(arg)
        } else {
            self.lower_value(arg)
        }
    }

    pub(crate) fn lower_direct_call_args(
        &mut self,
        callee_symbol: &str,
        callee_item: Option<hew_hir::ItemId>,
        args: &[HirExpr],
    ) -> Option<Vec<Place>> {
        // Supervisor bootstrap parameters own the spawned config snapshot even
        // though the synthesized function has no source `ItemId`.
        let move_all_args = self
            .supervisor_layout_map
            .values()
            .any(|layout| layout.bootstrap_symbol == callee_symbol);
        args.iter()
            .enumerate()
            .map(|(index, arg)| {
                let is_move = move_all_args
                    || callee_item.is_some_and(|item| {
                        self.param_ownership
                            .call_param_consume
                            .get(&(item, index))
                            .copied()
                            == Some(true)
                    });
                self.lower_method_arg_value(arg, is_move)
            })
            .collect()
    }
}
