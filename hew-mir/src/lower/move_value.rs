use super::{
    base_local, instr_source_places, outbound_record_layouts, terminator_source_places,
    ty_is_indirect_enum, BasicBlock, BindingId, Builder, CaptureEnvOwnedLoad, Disposition,
    FieldLoadClass, HashMap, HashSet, HirBinding, HirExpr, HirExprKind, Instr, MirDiagnostic,
    MirDiagnosticKind, MirStatement, OwnedCarrierNeutralizeTarget, OwnedCarrierParam,
    PendingOwnedCallArg, PendingOwnedCallSite, Place, ResolvedTy, SiteId, SnapshotFieldKind,
    SuspendKind,
};

impl Builder {
    /// Emit a move-out payload-slot neutralize whose ownership is consumed into
    /// an in-flight expression (no destination local to name as the transferee).
    /// The shared emit for the consuming-match move-out arms, so each arm records
    /// its discharge authority without repeating the field-carriage boilerplate.
    pub(crate) fn push_move_out_neutralize(
        &mut self,
        source: Place,
        authority: crate::model::NeutralizeAuthority,
    ) {
        self.push_instr(Instr::NeutralizePayloadSlot {
            place: source,
            transferee: None,
            authority,
        });
    }
    /// A `machine` value never enters the owned call-carrier protocol. Its
    /// layout registers in `machine_layouts` (codegen's enum-layout lookup
    /// for snapshot free synthesis fails closed on it), and the language
    /// contract passes machines BY VALUE — the caller keeps an independent
    /// copy — so transfer/neutralize semantics are wrong for it exactly as
    /// they are for indirect enums.
    pub(crate) fn ty_is_machine(&self, ty: &hew_types::ResolvedTy) -> bool {
        matches!(
            ty,
            hew_types::ResolvedTy::Named { name, .. }
                if super::machine_layout_name_matches(
                    &self.param_ownership.machine_decl_names,
                    name,
                )
        )
    }

    /// Propagate release authority through one aggregate field load.
    ///
    /// Registered call carriers already name their root in
    /// `owned_carrier_neutralize`. An ordinary scope-exit tuple owner also
    /// seeds a root-relative transfer for a `Vec` leaf: the load byte-copies
    /// the one owned pointer, so a later ownership boundary must clear the
    /// tuple slot before the loaded value can become a sole owner.
    ///
    /// This authority is deliberately Vec-only. Other `HandleTransfer` leaves
    /// (generator, stream/sink, indirect-enum, map/set) have distinct consume
    /// and close protocols; treating them as a plain projected Vec duplicates
    /// their downstream release. Inline aggregate aliases, retained strings,
    /// and `bytes` (whose MIR ownership pass inserts an explicit retain for
    /// field loads) likewise do not seed this route.
    pub(crate) fn note_carrier_projection(
        &mut self,
        aggregate: Place,
        field_index: u32,
        dest: Place,
        field_ty: &hew_types::ResolvedTy,
        site: SiteId,
    ) {
        let authority = self.owned_carrier_authority(aggregate).or_else(|| {
            let field_ty = self.subst_ty(field_ty);
            if !matches!(
                field_ty,
                ResolvedTy::Named {
                    builtin: Some(hew_types::BuiltinType::Vec),
                    ..
                }
            ) || self.classify_field_load(&field_ty) != Some(FieldLoadClass::HandleTransfer)
            {
                return None;
            }
            self.owned_locals
                .iter()
                .find(|entry| {
                    entry.disposition == Disposition::ScopeExit
                        && matches!(entry.ty, ResolvedTy::Tuple(_))
                        && self.binding_locals.get(&entry.binding).copied() == Some(aggregate)
                })
                .map(|entry| OwnedCarrierNeutralizeTarget::ScopeExitTuple {
                    root: aggregate,
                    owner: (entry.binding, entry.name.clone(), site),
                })
        });
        let Some(authority) = authority else {
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
        let (root, mut fields, scope_exit_owner) = match authority {
            OwnedCarrierNeutralizeTarget::Whole(root) => (root, Vec::new(), None),
            OwnedCarrierNeutralizeTarget::ScopeExitTuple { root, owner } => {
                (root, Vec::new(), Some(owner))
            }
            OwnedCarrierNeutralizeTarget::Projection {
                root,
                fields,
                scope_exit_owner,
            } => (root, fields, scope_exit_owner),
        };
        fields.push(field_index);
        self.owned_carrier_neutralize.insert(
            dest,
            OwnedCarrierNeutralizeTarget::Projection {
                root,
                fields,
                scope_exit_owner,
            },
        );
    }

    /// Propagate whole-carrier release authority into a match payload binder.
    ///
    /// The binder local holds a byte-copy ALIAS of the carrier scrutinee's
    /// variant payload slot. When the binder crosses an ownership boundary
    /// (arm-value move, `let`, `return`, a consuming call argument — every one
    /// funnels through `transfer_owned_carrier_place`), the variant slot must
    /// be neutralized on THAT path so the carrier's terminal snapshot drop
    /// observes null and the new owner keeps the single release authority. A
    /// binder that never escapes leaves the entry unfired and the terminal
    /// drop releases the payload — exactly once either way, per arm.
    ///
    /// Only a SELF-ROOTED whole carrier participates (the owned call-carrier
    /// parameter slot itself); projection or derived scrutinees keep their
    /// existing fail-closed handling. `BitCopy` payloads carry no release
    /// authority and are skipped.
    pub(crate) fn note_carrier_payload_binder(
        &mut self,
        scrutinee_local: u32,
        source: Place,
        dest: Place,
        binding_ty: &hew_types::ResolvedTy,
    ) {
        let scrutinee = Place::Local(scrutinee_local);
        if !matches!(
            self.owned_carrier_authority(scrutinee),
            Some(OwnedCarrierNeutralizeTarget::Whole(root)) if root == scrutinee
        ) {
            return;
        }
        let record_layouts = outbound_record_layouts(self);
        let Ok(plan) = crate::state_clone::classify_value_snapshot_plan_with_resource_handles(
            binding_ty,
            &record_layouts,
            &self.enum_layouts,
            &self.opaque_handle_names,
            &self.resource_opaque_close,
        ) else {
            // Unreachable for a registered carrier: the parameter registration
            // requires a clone-total plan over the whole enum, which
            // classifies every payload field.
            return;
        };
        if matches!(plan.root(), SnapshotFieldKind::BitCopy { .. }) {
            return;
        }
        self.owned_carrier_neutralize
            .insert(dest, OwnedCarrierNeutralizeTarget::Whole(source));
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
                    && !self.ty_is_machine(&owned_ty)
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
                    source_is_prepared_owner: self.prepared_owned_call_sources.contains(&source),
                })
            })
            .collect();
        if args.is_empty() {
            return;
        }
        for arg in &args {
            if arg.source_is_prepared_owner {
                self.prepared_owned_call_sources.remove(&arg.source);
            }
        }
        let replaced = self
            .pending_owned_call_args
            .insert(self.current_block_id, PendingOwnedCallSite { args });
        debug_assert!(replaced.is_none(), "one call terminator per basic block");
    }

    /// Register the callee half of an owned direct-call carrier contract.
    /// Returns `true` only when snapshot cleanup and neutralization authority
    /// were installed for the parameter; downstream fallback ownership must
    /// remain active for an eligible but non-clone-total plan.
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
            // inline tagged-union storage only. Machines are excluded for the
            // same class of reason — see `ty_is_machine`.
            && !ty_is_indirect_enum(&owned_ty, &self.enum_layouts)
            && !self.ty_is_machine(&owned_ty);
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
            // Callee half of the admission predicate (see
            // `snapshot_root_outside_carrier_protocol` for the full
            // rationale): registering a CoW-spine root as an owned carrier
            // adds a callee terminal drop WITHOUT the spine's retain — a
            // let-share of the param then double-frees. Aggregate roots
            // (records, tuples, enums, Vec) have no competing ownership
            // spine and stay on the carrier protocol.
            Ok(plan)
                if !super::snapshot_root_outside_carrier_protocol(plan.root())
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
                true
            }
            Ok(_) => false,
            Err(error) => {
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::NotYetImplemented {
                        construct: format!(
                            "owned call-carrier parameter `{}`",
                            param.ty.user_facing()
                        ),
                        site: SiteId(0),
                    },
                    note: error.to_string(),
                });
                false
            }
        }
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

    /// Whether a Vec index-assignment RHS moves into the slot. Mirrors the
    /// `effective_symbol` routing in `assign`'s Vec-set arm exactly: only a
    /// fresh materialised owner or a consumed non-parameter, non-capture bound
    /// local routes to the MOVE-in `hew_vec_set_owned_move`; every other RHS
    /// (a borrowed/read binding, a closure-captured value, a whole-parameter
    /// embed) stays COPY-IN via `hew_vec_set_owned` and its source keeps
    /// ownership.
    pub(crate) fn vec_set_owned_assign_moves_rhs(
        &self,
        target_symbol: &str,
        value: &HirExpr,
    ) -> bool {
        target_symbol == "hew_vec_set_owned"
            && (Self::expr_is_materialized_owner(
                value,
                &self.call_scrutinee_provenance.fresh_owner_verdicts,
                &self.funcupdate_param_ids,
                &self.proven_foreign_bindings,
            ) || self.is_consumed_bound_local(value))
    }

    /// Whether an assignment target's lowering deep-clones the RHS into place
    /// (COPY-IN) rather than moving it. Such an RHS never crosses an ownership
    /// boundary, so it must be lowered through the plain value funnel: the
    /// move funnel would wrongly reject a closure-captured RHS as a
    /// whole-value environment escape and would neutralize a carrier-tracked
    /// RHS whose source remains the owner.
    pub(crate) fn assign_target_stays_copy_in(&self, target: &HirExpr, value: &HirExpr) -> bool {
        match &target.kind {
            HirExprKind::ResolvedImplCall {
                target_symbol,
                target_family: hew_types::MethodTargetFamily::Vec(hew_types::VecMethod::Set),
                ..
            } => !self.vec_set_owned_assign_moves_rhs(target_symbol, value),
            _ => false,
        }
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
        Some(self.transfer_owned_carrier_value(expr, value))
    }

    /// Transfer one carrier-tracked place into an owning sink. Whole carriers
    /// move through a fresh local before their original slot is neutralized;
    /// projection carriers neutralize the root-relative field in place.
    ///
    /// Closure-env lowering also uses this funnel: an escaping `OwnsMoved`
    /// capture is an ownership boundary just like a consuming expression, and
    /// must discharge the parameter slot before the callee's terminal carrier
    /// drop runs.
    pub(crate) fn transfer_owned_carrier_place(
        &mut self,
        value: Place,
        ty: &hew_types::ResolvedTy,
    ) -> Place {
        let Some(target) = self.owned_carrier_authority(value) else {
            return value;
        };
        self.record_owned_carrier_transfer(value);
        match target {
            OwnedCarrierNeutralizeTarget::Whole(source) => {
                let dest = self.alloc_local(ty.clone());
                self.push_instr(Instr::Move { dest, src: value });
                self.push_instr(Instr::NeutralizePayloadSlot {
                    place: source,
                    transferee: Some(dest),
                    authority: crate::model::NeutralizeAuthority::WholeCarrierConsume,
                });
                dest
            }
            OwnedCarrierNeutralizeTarget::Projection {
                root,
                fields,
                scope_exit_owner,
            } => {
                self.push_instr(Instr::AggregateProjectionNeutralize {
                    root,
                    fields,
                    transferee: value,
                    scope_exit_owner,
                });
                self.prepared_owned_call_sources.insert(value);
                value
            }
            OwnedCarrierNeutralizeTarget::ScopeExitTuple { .. } => {
                unreachable!("scope-exit tuple authority must project before transfer")
            }
        }
    }

    /// Whether an earlier transfer of `value` can reach the block currently
    /// being lowered.
    ///
    /// Carrier authority is path-sensitive but MIR construction visits sibling
    /// arms one after another. Removing an authority from a global Builder map
    /// on the first arm suppresses the transfer in every later arm. Keep the
    /// authority stable instead and use the already-sealed CFG to reject only
    /// a second transfer on the same runtime path. A sibling arm has no path
    /// from the first arm's body; a join or later block does.
    fn owned_carrier_transfer_reaches_current(&self, value: Place) -> bool {
        let Some(starts) = self.owned_carrier_transferred_at.get(&value) else {
            return false;
        };
        let mut seen = HashSet::new();
        let mut stack = starts.clone();
        while let Some(block_id) = stack.pop() {
            if block_id == self.current_block_id {
                return true;
            }
            if !seen.insert(block_id) {
                continue;
            }
            if let Some(block) = self
                .pending_blocks
                .iter()
                .find(|block| block.id == block_id)
            {
                stack.extend(block.successors());
            }
        }
        false
    }

    /// Active transfer authority for `value` on the current CFG path.
    pub(crate) fn owned_carrier_authority(
        &self,
        value: Place,
    ) -> Option<OwnedCarrierNeutralizeTarget> {
        (!self.owned_carrier_transfer_reaches_current(value))
            .then(|| self.owned_carrier_neutralize.get(&value).cloned())
            .flatten()
    }

    /// Mark `value` consumed in the current basic block.
    pub(crate) fn record_owned_carrier_transfer(&mut self, value: Place) {
        self.owned_carrier_transferred_at
            .entry(value)
            .or_default()
            .push(self.current_block_id);
    }

    fn transfer_owned_carrier_value(&mut self, expr: &HirExpr, value: Place) -> Place {
        // A string/bytes let-share RETAINS the source (+1 on the shared
        // buffer) instead of moving it: the source slot stays live and keeps
        // its release authority, so the carrier must not be neutralized on
        // this edge — the retained binding and the original each release
        // their own count.
        if self.string_local_share_sites.contains_key(&expr.site)
            || self.bytes_local_share_sites.contains(&expr.site)
        {
            return value;
        }
        let ty = self.subst_ty(&expr.ty);
        self.transfer_owned_carrier_place(value, &ty)
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
                            .is_some_and(|v| v.is_consume())
                    });
                let target_is_owned_carrier = callee_item.is_some_and(|item| {
                    self.param_ownership
                        .call_param_owned_carrier
                        .get(&(item, index))
                        .copied()
                        == Some(true)
                });
                if is_move && target_is_owned_carrier {
                    if self.reject_capture_env_whole_escape_expr(arg) {
                        return None;
                    }
                    let value = self.lower_value(arg)?;
                    // A whole carrier parameter can be read by more than one
                    // freeing callee. Preserve its source until the post-CFG
                    // carrier pass can use liveness to choose snapshot or
                    // last-use transfer. Only a SELF-ROOTED whole (the carrier
                    // slot itself) defers: a payload-binder authority points at
                    // a variant slot inside a different root, and the post-CFG
                    // pass would neutralize the binder copy instead of that
                    // slot — it must transfer eagerly through the funnel.
                    // Projection carriers still transfer eagerly so their
                    // root-relative slot is neutralized once.
                    if matches!(
                        self.owned_carrier_authority(value),
                        Some(OwnedCarrierNeutralizeTarget::Whole(root)) if root == value
                    ) {
                        return Some(value);
                    }
                    return Some(self.transfer_owned_carrier_value(arg, value));
                }
                if !is_move || target_is_owned_carrier {
                    return self.lower_method_arg_value(arg, is_move);
                }

                // The general call-consume table is deliberately fail-closed
                // and can over-approximate borrow-only wrappers. A prepared
                // carrier must transfer only to a matching callee carrier (or
                // another explicit ownership boundary), otherwise a read such
                // as `string.is_empty(path)` would neutralize `path` before its
                // later use. Non-carrier arguments retain the historical move
                // lowering unchanged.
                if self.reject_capture_env_whole_escape_expr(arg) {
                    return None;
                }
                let value = self.lower_value(arg)?;
                if self.owned_carrier_authority(value).is_some() {
                    Some(value)
                } else {
                    Some(self.transfer_owned_carrier_value(arg, value))
                }
            })
            .collect()
    }
}

/// Reject a whole-tuple (or already-transferred field) read after an ordinary
/// tuple projection has physically cleared its source slot.
///
/// Hew's checker stream currently tracks ownership at binding granularity, so
/// consume-marking the tuple would also reject valid reads of unmoved siblings.
/// The backend stream retains the needed field precision: sibling
/// `TupleFieldLoad`s remain valid, while a whole-root source or another load of
/// the cleared field would expose a partially moved value. Fail closed on the
/// latter before codegen instead of emitting a null-bearing tuple that can
/// escape and fault in its caller.
pub(super) fn ordinary_projection_transfer_diagnostics(
    blocks: &[BasicBlock],
    suspend_kinds: &HashMap<u32, SuspendKind>,
) -> Vec<MirDiagnostic> {
    let mut diagnostics = Vec::new();
    let mut emitted = HashSet::new();

    for block in blocks {
        for (instr_index, instr) in block.instructions.iter().enumerate() {
            let Instr::AggregateProjectionNeutralize {
                root,
                fields,
                scope_exit_owner: Some((binding, name, transfer_site)),
                ..
            } = instr
            else {
                continue;
            };

            let violating_block = first_invalid_projection_root_use(
                blocks,
                suspend_kinds,
                block.id,
                instr_index.saturating_add(1),
                *root,
                fields,
            );
            let Some(violating_block) = violating_block else {
                continue;
            };

            let used_at = violating_block
                .statements
                .iter()
                .rev()
                .find_map(|statement| match statement {
                    MirStatement::Use {
                        binding: used_binding,
                        site,
                        ..
                    } if used_binding == binding => Some(*site),
                    _ => None,
                })
                .unwrap_or(*transfer_site);
            if emitted.insert((*binding, used_at)) {
                diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::UseAfterConsume {
                        binding: *binding,
                        name: name.clone(),
                        consumed_at: *transfer_site,
                        used_at,
                    },
                    note: format!(
                        "tuple field ownership transferred at site {transfer_site:?}; only unmoved sibling \
                         projections remain readable afterward, so using or returning the whole \
                         tuple `{name}` would expose the cleared field"
                    ),
                });
            }
        }
    }

    diagnostics
}

fn first_invalid_projection_root_use<'a>(
    blocks: &'a [BasicBlock],
    suspend_kinds: &HashMap<u32, SuspendKind>,
    start_block: u32,
    start_index: usize,
    root: Place,
    fields: &[u32],
) -> Option<&'a BasicBlock> {
    let mut frontier = vec![(start_block, start_index)];
    let mut visited = HashSet::new();
    while let Some((block_id, first_index)) = frontier.pop() {
        if !visited.insert((block_id, first_index)) {
            continue;
        }
        let Some(block) = blocks.iter().find(|block| block.id == block_id) else {
            continue;
        };
        let mut overwritten = false;
        for instr in &block.instructions[first_index..] {
            if projection_root_use_is_invalid(instr, root, fields) {
                return Some(block);
            }
            if crate::dataflow::instr_reads_writes(instr)
                .1
                .into_iter()
                .any(|place| place == root)
            {
                overwritten = true;
                break;
            }
        }
        if overwritten {
            continue;
        }
        if terminator_source_places(&block.terminator, suspend_kinds.get(&block.id))
            .into_iter()
            .any(|place| same_base_local(place, root))
        {
            return Some(block);
        }
        if crate::dataflow::terminator_write_places(&block.terminator)
            .into_iter()
            .any(|place| place == root)
        {
            continue;
        }
        frontier.extend(
            block
                .successors()
                .into_iter()
                .map(|successor| (successor, 0)),
        );
    }
    None
}

fn same_base_local(place: Place, root: Place) -> bool {
    base_local(place).is_some() && base_local(place) == base_local(root)
}

fn projection_root_use_is_invalid(instr: &Instr, root: Place, fields: &[u32]) -> bool {
    match instr {
        // A distinct sibling remains live after the projected slot is cleared.
        Instr::TupleFieldLoad {
            tuple, field_index, ..
        } if same_base_local(*tuple, root) => fields.first() == Some(field_index),
        // Structural cleanup is precisely why the source slot was cleared; all
        // supported drop paths are null-safe for the transferred handle leaf.
        Instr::Drop { place, .. } if same_base_local(*place, root) => false,
        Instr::RecordFieldDrop { record, .. } if same_base_local(*record, root) => false,
        Instr::FieldDropInPlace { base, .. } if same_base_local(*base, root) => false,
        Instr::ValueSnapshotDrop { value, .. } if same_base_local(*value, root) => false,
        Instr::WitnessDropGlue { place, .. } if same_base_local(*place, root) => false,
        // Further disjoint field transfers on the same tuple are valid.
        Instr::AggregateProjectionNeutralize { .. } => false,
        _ => instr_source_places(instr)
            .into_iter()
            .any(|place| same_base_local(place, root)),
    }
}
