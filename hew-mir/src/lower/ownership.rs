use hew_hir::HirProducedValueProducer;

use super::{
    actor_name_from_handle_ty, affine_release_needs_drop_flag, base_local, binding_ref_target,
    callee_returns_fresh_owner, callee_returns_retained_string_owner,
    hir_expr_contains_synthetic_vec_get_clone, local_is_rewritten_after_current_iteration,
    machine_layout_ty_matches, monomorphic_user_record_key, named_type_marker, ty_is_closure_pair,
    ty_is_heap_owning_enum_composite, ty_is_local_collection_handle, user_record_layout_key,
    vec_iter_record_layout_key, ActiveIterationOwner, AffineCallConsumeCandidate, BasicBlock,
    BindingId, Builder, BuiltinType, ClosurePairIngress, CmpPred, DecisionFact, DischargeSite,
    Disposition, FieldLoadClass, HashMap, HashSet, HirBinding, HirBlock, HirExpr, HirExprKind,
    HirProducedValueRelation, HirStmtKind, Instr, IntentKind, LayoutClass, MirDiagnostic,
    MirDiagnosticKind, MirStatement, OwnedCarrierNeutralizeTarget, OwnedLocalEntry,
    OwnerMintOrigin, OwnerMintWarrant, OwnershipCtx, OwnershipDecision, Place, PlaceProvenance,
    ProducedValueOwnership, Projection, ResolvedRef, ResolvedTy, ResourceMarker, SiteId, Strategy,
    Terminator, ValueClass, ValueOwnership, ValueProvenance, SYNTHETIC_CALL_SCRUTINEE_NAME,
    SYNTHETIC_COPY_IN_PARAM_TEMP_NAME, SYNTHETIC_DISCARDED_CALL_RESULT_NAME,
    SYNTHETIC_OWNED_TEMP_BINDING_BASE, SYNTHETIC_WHILE_LET_ITERATION_NAME,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum WholeParamEmbedClass {
    None,
    IndependentlyOwnedOnly,
    UnsupportedBorrowAlias,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum OwnedAliasInheritance {
    NotAlias,
    Exact(ValueProvenance),
    Ambiguous,
}

impl WholeParamEmbedClass {
    fn merge(self, other: Self) -> Self {
        match (self, other) {
            (Self::UnsupportedBorrowAlias, _) | (_, Self::UnsupportedBorrowAlias) => {
                Self::UnsupportedBorrowAlias
            }
            (Self::IndependentlyOwnedOnly, _) | (_, Self::IndependentlyOwnedOnly) => {
                Self::IndependentlyOwnedOnly
            }
            (Self::None, Self::None) => Self::None,
        }
    }
}

/// Return-carrier proof for one compiler-generated closure invoke shim.
///
/// A captured or closure-parameter string is admissible here: the shim lowers
/// the former through a retaining
/// `ClosureEnvFieldLoad` and inserts `StringRetain` for the latter at its return
/// edge. Calls still use the module carrier authority, so a direct opaque
/// extern wrapper poisons the proof instead of acquiring the closure ABI's
/// caller-owned postcondition.
struct ClosureStringReturnPolicy<'a> {
    builder: &'a Builder,
    retained_binding_returns: HashSet<BindingId>,
}

impl crate::return_provenance::LeafPolicy for ClosureStringReturnPolicy<'_> {
    fn classify_call(&self, callee: &HirExpr) -> crate::return_provenance::CallClass {
        let indirect_closure_invoke = !matches!(
            &callee.kind,
            HirExprKind::BindingRef {
                resolved: ResolvedRef::Item(_),
                ..
            }
        ) && matches!(
            self.builder.subst_ty(&callee.ty),
            ResolvedTy::Function { .. } | ResolvedTy::Closure { .. }
        );
        if indirect_closure_invoke
            || self.builder.call_produces_fresh_owned_string(callee)
            || self.builder.user_call_produces_owned_string_carrier(callee)
        {
            crate::return_provenance::CallClass::Fresh
        } else {
            crate::return_provenance::CallClass::Opaque
        }
    }

    fn leaf_bits(&self, expr: &HirExpr) -> crate::return_provenance::AliasBits {
        match &expr.kind {
            HirExprKind::BindingRef {
                resolved: ResolvedRef::Binding(id),
                ..
            } if matches!(self.builder.subst_ty(&expr.ty), ResolvedTy::String)
                && self.retained_binding_returns.contains(id) =>
            {
                crate::return_provenance::AliasBits::EMPTY
            }
            HirExprKind::Binary {
                op: super::BinaryOp::Add,
                ..
            } if matches!(self.builder.subst_ty(&expr.ty), ResolvedTy::String) => {
                crate::return_provenance::AliasBits::EMPTY
            }
            HirExprKind::ResolvedImplCall { target_symbol, .. }
                if crate::runtime_symbols::callee_ownership_contract(target_symbol)
                    .produces_fresh_owned_string() =>
            {
                crate::return_provenance::AliasBits::EMPTY
            }
            HirExprKind::ConnAwaitRead {
                to_string: true,
                deadline_ns: None,
                ..
            } if matches!(self.builder.subst_ty(&expr.ty), ResolvedTy::String) => {
                // `await conn.read_string()` materialises bytes on the suspend
                // resume edge, then converts them through
                // `hew_bytes_to_string`. That runtime contract returns a fresh
                // header-aware +1 string and borrows the bytes source, so the
                // closure return transfers one independent share. Deadline
                // reads return a Result carrier and are deliberately excluded.
                crate::return_provenance::AliasBits::EMPTY
            }
            _ => crate::return_provenance::AliasBits::OPAQUE,
        }
    }

    fn materialized_leaf_bits(&self, expr: &HirExpr) -> crate::return_provenance::AliasBits {
        if matches!(self.builder.subst_ty(&expr.ty), ResolvedTy::String) {
            crate::return_provenance::AliasBits::EMPTY
        } else {
            crate::return_provenance::AliasBits::OPAQUE
        }
    }
}

pub(super) fn returned_aggregate_consumes_source(
    block: &BasicBlock,
    constructor_index: usize,
    source: Place,
    destination: Place,
) -> bool {
    block
        .instructions
        .iter()
        .skip(constructor_index.saturating_add(1))
        .take_while(|instruction| matches!(instruction, Instr::NeutralizePayloadSlot { .. }))
        .any(|instruction| {
            matches!(
                instruction,
                Instr::NeutralizePayloadSlot {
                    place,
                    transferee: Some(transferee),
                    authority: crate::model::NeutralizeAuthority::ReturnedAggregateMemberConsume,
                } if *place == source && *transferee == destination
            )
        })
}

impl Builder {
    /// Resolve affine arguments whose declared extern contract adopts the value
    /// only on normal return. Direct Hew consuming parameters own their values
    /// from function entry and therefore use ordinary pre-invoke binding
    /// transfer instead of this deferred protocol.
    pub(super) fn affine_call_consume_candidates(
        &mut self,
        callee_symbol: &str,
        callee_item: Option<hew_hir::ItemId>,
        hir_args: &[hew_hir::HirExpr],
    ) -> Vec<AffineCallConsumeCandidate> {
        let mut candidates = Vec::new();
        for (index, arg) in hir_args.iter().enumerate() {
            let HirExprKind::BindingRef {
                resolved: ResolvedRef::Binding(binding),
                ..
            } = &arg.kind
            else {
                continue;
            };
            let use_intent = self.binding_ref_use_intent(arg);
            if use_intent == IntentKind::Discharge {
                continue;
            }
            let Some(guard) = self.affine_release_flags.get(binding).copied() else {
                continue;
            };
            if callee_item.is_some()
                || !self
                    .call_scrutinee_provenance
                    .extern_table
                    .extern_param_is_consume(callee_symbol, index)
            {
                continue;
            }
            if use_intent != IntentKind::Consume {
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::NotYetImplemented {
                        construct: "typed affine call consume without checker Consume intent"
                            .to_string(),
                        site: arg.site,
                    },
                    note: "the checker use and physical normal-edge handoff must share one consume authority"
                        .to_string(),
                });
                continue;
            }
            candidates.push(AffineCallConsumeCandidate {
                index,
                binding: *binding,
                guard,
                site: arg.site,
            });
        }
        candidates
    }

    pub(super) fn activate_affine_call_consume_sites(
        &mut self,
        candidates: &[AffineCallConsumeCandidate],
    ) {
        self.deferred_affine_call_consume_sites
            .extend(candidates.iter().map(|candidate| candidate.site));
    }

    pub(super) fn deactivate_affine_call_consume_sites(
        &mut self,
        candidates: &[AffineCallConsumeCandidate],
    ) {
        for candidate in candidates {
            self.deferred_affine_call_consume_sites
                .remove(&candidate.site);
        }
    }

    pub(crate) fn emit_scope_exit_marker<I>(&mut self, scopes: I)
    where
        I: IntoIterator<Item = hew_hir::ScopeId>,
    {
        self.emit_scope_exit_marker_with_carries(scopes, std::iter::empty());
    }

    pub(crate) fn emit_scope_exit_marker_with_carries<I, P>(&mut self, scopes: I, carries: P)
    where
        I: IntoIterator<Item = hew_hir::ScopeId>,
        P: IntoIterator<Item = Place>,
    {
        let mut scopes = scopes.into_iter().collect::<Vec<_>>();
        scopes.sort_by_key(|scope| scope.0);
        scopes.dedup();
        if scopes.is_empty() {
            return;
        }
        self.push_instr(Instr::OwnershipEvent(
            crate::model::OwnershipEvent::ScopeExit {
                scopes,
                owners: Vec::new(),
                carry_places: carries.into_iter().collect(),
                carried: Vec::new(),
            },
        ));
    }

    /// Resolve the unique currently-published owner identity for a lexical
    /// place while Raw MIR is still being constructed. The returned identity
    /// is copied into an explicit Checked-MIR fact; consumers never consult
    /// this lowering cursor after sealing.
    pub(crate) fn current_owner_id_at_place(&self, place: Place) -> Option<crate::model::OwnerId> {
        let mut owners = self.owned_locals.iter().filter_map(|entry| {
            (entry.disposition != Disposition::AliasOf
                && self.binding_locals.get(&entry.binding) == Some(&place))
            .then(|| {
                self.owner_generations
                    .get(&entry.binding)
                    .copied()
                    .map(|generation| crate::model::OwnerId {
                        binding: entry.binding,
                        generation,
                    })
            })
            .flatten()
        });
        let owner = owners.next()?;
        owners.next().is_none().then_some(owner)
    }

    /// Publish the physical runtime bit for the binding's current owner
    /// generation into semantic MIR. Physical flag registries are lowering
    /// cursors only; a cleanup cannot use a flag after Checked MIR sealing
    /// unless this event exists at the flag's defining program point.
    pub(crate) fn publish_current_owner_guard(
        &mut self,
        binding: BindingId,
        flag: Place,
        kind: crate::model::OwnershipGuardKind,
    ) -> bool {
        let Some(generation) = self.owner_generations.get(&binding).copied() else {
            return false;
        };
        self.push_instr(Instr::OwnershipEvent(crate::model::OwnershipEvent::Guard {
            owner: crate::model::OwnerId {
                binding,
                generation,
            },
            flag,
            kind,
        }));
        true
    }

    /// Republish an already-allocated physical flag for a freshly minted
    /// generation. A guard belongs to an `OwnerId`, not merely a source
    /// binding: after a reset, retaining only the generation-zero event would
    /// make cleanup for the replacement owner unverifiable (and could let a
    /// stale flag authorize the wrong value).
    fn publish_existing_physical_guard(
        &mut self,
        binding: BindingId,
        owner: crate::model::OwnerId,
    ) {
        let guards = [
            self.affine_release_flags
                .get(&binding)
                .copied()
                .map(|flag| (flag, crate::model::OwnershipGuardKind::AffineRelease)),
            self.overwrite_guard_flags
                .get(&binding)
                .copied()
                .map(|flag| (flag, crate::model::OwnershipGuardKind::Overwrite)),
            self.collection_drop_flags
                .get(&binding)
                .copied()
                .map(|flag| (flag, crate::model::OwnershipGuardKind::Collection)),
            self.actor_message_cow_drop_flags
                .get(&binding)
                .copied()
                .map(|flag| (flag, crate::model::OwnershipGuardKind::ActorMessageCow)),
            self.conditional_record_drop_flags
                .get(&binding)
                .copied()
                .map(|flag| (flag, crate::model::OwnershipGuardKind::ConditionalRecord)),
            self.projected_payload_overwrite_flags
                .get(&binding)
                .copied()
                .map(|flag| (flag, crate::model::OwnershipGuardKind::ProjectedPayload)),
            self.vec_iter_drop_flags
                .get(&binding)
                .copied()
                .map(|flag| (flag, crate::model::OwnershipGuardKind::VecIter)),
        ]
        .into_iter()
        .flatten()
        .collect::<Vec<_>>();
        assert!(
            guards.len() <= 1,
            "owner {owner:?} has conflicting physical cleanup guards: {guards:?}"
        );
        if let Some((flag, kind)) = guards.into_iter().next() {
            self.push_instr(Instr::OwnershipEvent(crate::model::OwnershipEvent::Guard {
                owner,
                flag,
                kind,
            }));
        }
    }

    /// Emit the handoff immediately. Correct wherever the destructure itself
    /// only runs on the path that selected the pattern (`if let` / `while let`
    /// / `let else`). A match arm destructures before its guard decides, so it
    /// uses [`Self::contextual_sink_payload_handoff`] and emits on the
    /// arm-selected edge instead.
    pub(crate) fn transfer_contextual_sink_payload(
        &mut self,
        scrutinee_owner: Option<&(BindingId, ResolvedTy)>,
        binding: BindingId,
        source: Place,
        dest: Place,
        binding_ty: &ResolvedTy,
    ) -> bool {
        match self.contextual_sink_payload_handoff(
            scrutinee_owner,
            binding,
            source,
            dest,
            binding_ty,
        ) {
            Some(instr) => {
                self.push_instr(instr);
                true
            }
            None => false,
        }
    }

    /// Transfer a runtime-close payload out of a fresh contextual scrutinee.
    ///
    /// A builtin `Sink` payload is a nullable handle with a typed runtime close
    /// descriptor. Once a fresh call carrier is the proven
    /// owner, moving its active payload into the contextual binder and clearing
    /// that exact variant slot makes the binder the sole close authority. The
    /// carrier can still clean up a different active variant on the mismatch
    /// path, and its drop is a no-op over the cleared handle on the success
    /// path. User-close resources are deliberately excluded: their close body
    /// can observe zeroed storage, so nulling a field does not make the shell's
    /// later user close inert.
    ///
    /// Admit the handoff and return the slot-clearing instruction the caller
    /// must emit on the path that selects this binder. Returning the `Instr`
    /// rather than pushing it keeps admission (which reads the ownership
    /// registries as they stand at destructure time) separate from placement.
    pub(crate) fn contextual_sink_payload_handoff(
        &mut self,
        scrutinee_owner: Option<&(BindingId, ResolvedTy)>,
        binding: BindingId,
        source: Place,
        dest: Place,
        binding_ty: &ResolvedTy,
    ) -> Option<Instr> {
        let (owner, owner_ty) = scrutinee_owner?;
        let source_root = base_local(source)?;
        let source_is_fresh_owned_result = matches!(
            (owner_ty, source),
            (
                ResolvedTy::Named {
                    args,
                    builtin: Some(hew_types::BuiltinType::Result),
                    ..
                },
                Place::MachineVariant {
                    variant_idx: 0,
                    field_idx: 0,
                    ..
                } | Place::EnumVariant {
                    variant_idx: 0,
                    field_idx: 0,
                    ..
                }
            ) if args.first() == Some(binding_ty)
        ) && self.locals.get(source_root as usize)
            == Some(owner_ty)
            && self.binding_locals.get(owner) == Some(&Place::Local(source_root))
            // The ledger disposition is path-insensitive: a sibling arm lowered
            // earlier already released the carrier at its body end, but on the
            // path that reaches THIS destructure the carrier is still the live
            // owner. The minted call-carrier fact is the path-independent proof.
            && self.call_scrutinee_carrier_mint_locals.contains(&source_root)
            && self.owned_locals.iter().any(|entry| {
                entry.binding == *owner
                    && entry.ty == *owner_ty
                    && entry.disposition != Disposition::AliasOf
            });
        let dest_is_proven_owner = self.binding_locals.get(&binding) == Some(&dest)
            && self.owned_locals.iter().any(|entry| {
                entry.binding == binding
                    && entry.ty == *binding_ty
                    && entry.disposition == Disposition::ScopeExit
            });
        if !source_is_fresh_owned_result
            || !dest_is_proven_owner
            || !matches!(
                super::drop_plan::resource_drop_fn(binding_ty, &self.type_classes),
                Some(crate::model::DropFnSpec::Runtime(
                    hew_types::runtime_call::RuntimeDropDescriptor::SinkClose
                ))
            )
        {
            return None;
        }
        if let Some(local) = base_local(dest) {
            self.fresh_variant_payload_binder_locals.insert(local);
        }
        Some(Instr::NeutralizePayloadSlot {
            place: source,
            transferee: Some(dest),
            authority: crate::model::NeutralizeAuthority::EphemeralTempConsume,
        })
    }

    pub(crate) fn publish_produced_value_place(&mut self, expr: &HirExpr, place: Place) {
        self.published_value_places.insert(expr.site, place);
        let borrowed_publication = self
            .param_ownership
            .produced_value_facts
            .get(&expr.site)
            .is_some_and(|fact| {
                matches!(expr.kind, HirExprKind::Match { .. })
                    && matches!(fact.ownership, ProducedValueOwnership::Borrowed)
                    && matches!(fact.relation, HirProducedValueRelation::Join(_))
            });
        if !borrowed_publication {
            return;
        }
        let Place::Local(local) = place else {
            return;
        };
        match self.subst_ty(&expr.ty) {
            ResolvedTy::String => {
                self.typed_borrowed_string_publication_locals.insert(local);
            }
            ResolvedTy::Bytes => {
                self.typed_borrowed_bytes_publication_locals.insert(local);
            }
            _ => {}
        }
    }

    /// The ownership classify context over this builder's live registries — the
    /// same three tables the drop derivations read, bundled so
    /// [`ValueOwnership::classify`] builds its answer from the one authority.
    pub(crate) fn ownership_ctx(&self) -> OwnershipCtx<'_> {
        OwnershipCtx::new(
            &self.record_field_orders,
            &self.enum_layouts,
            &self.type_classes,
        )
    }

    /// Whether every visible string return path of a closure body establishes
    /// the `ClosureInvoke` ABI's one-share postcondition.
    pub(crate) fn closure_body_returns_owned_string_carrier(
        &self,
        body: &HirExpr,
        params: &[HirBinding],
        captures: &[hew_hir::HirClosureCapture],
    ) -> bool {
        let retained_binding_returns = params
            .iter()
            .map(|binding| binding.id)
            .chain(captures.iter().map(|capture| capture.binding))
            .collect();
        let policy = ClosureStringReturnPolicy {
            builder: self,
            retained_binding_returns,
        };
        let HirExprKind::Block(block) = &body.kind else {
            return crate::return_provenance::return_alias_bits(body, &policy).is_fresh();
        };
        let mut return_values = Vec::new();
        super::collect_return_values_in_block(block, &mut return_values);
        if let Some(tail) = &block.tail {
            // A unit/never tail after an explicit `return <value>` is not a
            // value handed back by the closure and must not poison that path.
            if !matches!(
                self.subst_ty(&tail.ty),
                ResolvedTy::Unit | ResolvedTy::Never
            ) {
                return_values.push(tail);
            }
        }
        !return_values.is_empty()
            && return_values
                .into_iter()
                .all(|value| crate::return_provenance::return_alias_bits(value, &policy).is_fresh())
    }
    /// The single registration authority for the per-function owned-locals
    /// ledger: every seam that introduces a scope-exit drop obligation routes
    /// through here instead of pushing a bare tuple. Ownership is classified
    /// ONCE at this defining write and recorded on the entry; the drop passes
    /// read the written-down fact rather than re-deriving it per pass.
    ///
    /// The binding's backing place drives classify's handle-vs-type dispatch and
    /// is also written into the authoritative ownership event. Every caller must
    /// therefore allocate and publish the real destination before registration;
    /// fabricating a placeholder would make Checked MIR disagree with the value
    /// that codegen actually moves and drops. Provenance stays `None` at this
    /// stage — it is recorded only
    /// where a later stage can trivially prove it at the defining write. Every
    /// entry is minted `Disposition::ScopeExit`; a retraction seam later
    /// dispositions it off the scope-exit set via
    /// [`Builder::set_owned_local_disposition`] when its release is handled
    /// mid-lowering.
    ///
    /// # The warrant
    ///
    /// `warrant` is an [`OwnerMintWarrant`], which cannot be constructed outside
    /// [`crate::lower::owner_mint`] and cannot be constructed there without
    /// naming the value the owner is for and putting its provenance to the
    /// per-function ledger and/or the module authority. A seam that wants to
    /// mint an owner therefore cannot decide ownership from the binding's type
    /// alone: the signature will not let it. When the warrant answers foreign
    /// the mint is WITHHELD — the value keeps no caller-side release, which is
    /// the leak-not-double-free direction.
    #[allow(
        clippy::too_many_lines,
        reason = "owner minting publishes the complete definition, recipe, guard, and compatibility metadata atomically"
    )]
    pub(crate) fn register_owned_local(
        &mut self,
        binding: BindingId,
        name: String,
        ty: ResolvedTy,
        warrant: OwnerMintWarrant,
    ) {
        if warrant.withholds_mint() || super::drop_plan::ty_is_nonowning_handle_leaf(&ty) {
            return;
        }
        if warrant.origin() == OwnerMintOrigin::PayloadOfScrutinee {
            self.scrutinee_payload_owner_bindings.insert(binding);
        }
        if matches!(ty, ResolvedTy::TraitObject { .. }) {
            self.dyn_trait_storage
                .insert(binding, crate::TraitObjectStorage::HeapBoxed);
        }
        let place = *self.binding_locals.get(&binding).unwrap_or_else(|| {
            panic!("owned binding {binding:?} must publish its real MIR destination before mint")
        });
        // Closure pairs have a structural drop shape, but only a HeapBox env
        // owns storage that the pair destructor may free. Keep them out of the
        // generic structural admission so the provenance-sensitive gate below
        // remains authoritative: admitting a Stack env here would make its
        // frame alloca look like a heap allocation at scope exit.
        let carries_structural_cleanup = matches!(ty, ResolvedTy::TraitObject { .. })
            || (!ty_is_closure_pair(&ty)
                && crate::model::ty_carries_drop_obligation_mir(
                    &ty,
                    &self.record_field_orders,
                    &self.enum_layouts,
                    self.type_classes.lifecycle_registry(),
                ));
        // Some compiler-owned substrate handles carry their release protocol
        // in the MIR Place rather than in the surface type.  In particular,
        // an actor literal is surface-typed as `Duplex<Msg, Reply>` but its
        // `LambdaActorHandle` destination must select
        // `hew_lambda_actor_release`, while `DuplexHandle` and the split-half
        // places select their corresponding close rituals.  These values are
        // constructed by typed terminal producers, so the let registration in
        // the producer's normal continuation is their definition-site owner
        // publication.  Do not make the type-only structural query erase that
        // Mint: the exceptional call edge never reaches this continuation and
        // therefore never observes an owner for an uninitialised out-place.
        let carries_place_typed_substrate_cleanup = matches!(
            place,
            Place::DuplexHandle(_)
                | Place::LambdaActorHandle(_)
                | Place::SendHalf(_)
                | Place::RecvHalf(_)
        );
        let carries_vec_iter_cleanup = self.vec_iter_cursor_release_symbol(&ty).is_some();
        let carries_heap_closure_cleanup =
            matches!(ty, ResolvedTy::Function { .. } | ResolvedTy::Closure { .. })
                && (self.closure_pair_owned.contains(&binding)
                    || self.instructions.iter().any(|instruction| {
                        matches!(
                            instruction,
                            Instr::MakeClosure {
                                        dest,
                                        env_mode: crate::model::ClosureEnvMode::HeapBox,
                                        ..
                            } if *dest == place
                        )
                    }));
        // A scalar user `#[resource]` record can have no heap-bearing fields,
        // so the generic structural query above is false even though its
        // non-idempotent user `close` ritual is a real cleanup obligation.
        // Admit it through the same exact predicate that allocates the affine
        // flag later in let lowering. Registration therefore publishes Mint
        // before `maybe_alloc_affine_release_flag` publishes Guard; an
        // interior-alias runtime result never reaches this registrar and
        // remains ownerless.
        let carries_affine_cleanup = affine_release_needs_drop_flag(place, &ty, &self.type_classes);
        if !carries_structural_cleanup
            && !carries_place_typed_substrate_cleanup
            && !carries_vec_iter_cleanup
            && !carries_heap_closure_cleanup
            && !carries_affine_cleanup
        {
            // The move checker may still classify this value as affine or
            // non-BitCopy, but it has no destructor obligation. OwnerId is
            // the cleanup authority stream, so publishing a generation here
            // would require a destructor that cannot exist (for example
            // Option<f64>, a plain enum, or a null/stack closure pair).
            return;
        }
        let ownership = ValueOwnership::classify(&ty, place, &self.ownership_ctx());
        let previous_generation = self.owner_generations.get(&binding).copied();
        let generation = previous_generation.map_or(0, |value| value.saturating_add(1));
        self.owner_generations.insert(binding, generation);
        let owner = crate::model::OwnerId {
            binding,
            generation,
        };
        if let Some(index) = self.owned_locals.iter().position(|entry| {
            entry.binding != binding
                && self.binding_locals.get(&entry.binding) == Some(&place)
                && self
                    .synthetic_owner_publication_sites
                    .contains_key(&entry.binding)
        }) {
            // A named binding is adopting the provisional publication owner;
            // replace the ledger identity in place so there is still exactly
            // one cleanup authority for the local.
            let prior = self.owned_locals[index].binding;
            self.synthetic_owner_publication_sites.remove(&prior);
            self.typed_produced_value_owner_bindings.remove(&prior);
            let successor_ty = ty.clone();
            self.owned_locals[index] = OwnedLocalEntry {
                binding,
                name,
                ty,
                ownership,
                provenance: None,
                disposition: Disposition::ScopeExit,
            };
            let prior_generation = self.owner_generations.remove(&prior).unwrap_or(0);
            self.push_instr(Instr::OwnershipEvent(
                crate::model::OwnershipEvent::Transfer {
                    owner: crate::model::OwnerId {
                        binding: prior,
                        generation: prior_generation,
                    },
                    from: place,
                    to: Some(place),
                    to_owner: Some(owner),
                    to_ty: Some(successor_ty),
                },
            ));
            self.publish_existing_physical_guard(binding, owner);
            return;
        }
        let previous_is_live = self
            .owned_locals
            .iter()
            .any(|entry| entry.binding == binding && entry.disposition == Disposition::ScopeExit);
        let event = if let Some(previous) = previous_generation.filter(|_| previous_is_live) {
            crate::model::OwnershipEvent::Reset {
                previous: crate::model::OwnerId {
                    binding,
                    generation: previous,
                },
                replacement: owner,
                place,
                ty: ty.clone(),
            }
        } else {
            crate::model::OwnershipEvent::Mint {
                owner,
                place,
                ty: ty.clone(),
            }
        };
        self.owned_locals.push(OwnedLocalEntry {
            binding,
            name,
            ty,
            ownership,
            provenance: None,
            disposition: Disposition::ScopeExit,
        });
        self.push_instr(Instr::OwnershipEvent(event));
        self.publish_existing_physical_guard(binding, owner);
    }

    /// Publish a newly bound cleanup owner by transferring the exact source
    /// generation after the physical whole-value Move has executed.
    ///
    /// `VecIter` rebinds carry a separate runtime field-release flag, so their
    /// flag setup used to sit between the destination Mint and the physical
    /// Move. The generic adjacent-move canonicalizer could not see through
    /// that setup and both source and destination generations survived at one
    /// slot. This registrar makes the ownership operation explicit at the
    /// actual program point and never creates a parallel mint.
    pub(crate) fn register_owned_local_transfer_from(
        &mut self,
        binding: BindingId,
        name: String,
        ty: ResolvedTy,
        source_binding: BindingId,
        handoff: (Place, Place),
        warrant: OwnerMintWarrant,
    ) -> bool {
        if warrant.withholds_mint()
            || super::drop_plan::ty_is_nonowning_handle_leaf(&ty)
            || binding == source_binding
            || self.owner_generations.contains_key(&binding)
        {
            return false;
        }
        let Some(source_generation) = self.owner_generations.get(&source_binding).copied() else {
            return false;
        };
        let source_owner = crate::model::OwnerId {
            binding: source_binding,
            generation: source_generation,
        };
        let owner = crate::model::OwnerId {
            binding,
            generation: 0,
        };
        let (source_place, destination) = handoff;
        self.owner_generations.insert(binding, 0);
        let ownership = ValueOwnership::classify(&ty, destination, &self.ownership_ctx());
        self.owned_locals.push(OwnedLocalEntry {
            binding,
            name,
            ty: ty.clone(),
            ownership,
            provenance: None,
            disposition: Disposition::ScopeExit,
        });
        self.push_instr(Instr::OwnershipEvent(
            crate::model::OwnershipEvent::Transfer {
                owner: source_owner,
                from: source_place,
                to: Some(destination),
                to_owner: Some(owner),
                to_ty: Some(ty),
            },
        ));
        true
    }

    /// Publish the exact owner handoff for a whole-value `VecIter` assignment.
    ///
    /// The RHS binding's runtime flag is disarmed before the physical Move and
    /// the destination flag is restored from the saved value. Ownership MIR
    /// must perform the matching transition at that same program point: end the
    /// source generation and publish the next destination generation. Leaving
    /// the source as a mere `Relocate` makes its later flag-gated lexical drop
    /// name the moved-from slot even though Checked MIR records that generation
    /// at the destination.
    pub(crate) fn transfer_vec_iter_assignment_owner(
        &mut self,
        destination_binding: BindingId,
        source_binding: BindingId,
        source: Place,
        destination: Place,
        ty: &ResolvedTy,
    ) -> bool {
        if destination_binding == source_binding
            || !self.vec_iter_drop_flags.contains_key(&destination_binding)
            || !self.vec_iter_drop_flags.contains_key(&source_binding)
        {
            return false;
        }
        let Some(source_generation) = self.owner_generations.get(&source_binding).copied() else {
            return false;
        };
        let Some(previous_generation) = self.owner_generations.get(&destination_binding).copied()
        else {
            return false;
        };
        let replacement = crate::model::OwnerId {
            binding: destination_binding,
            generation: previous_generation.saturating_add(1),
        };
        self.push_instr(Instr::OwnershipEvent(
            crate::model::OwnershipEvent::Transfer {
                owner: crate::model::OwnerId {
                    binding: source_binding,
                    generation: source_generation,
                },
                from: source,
                to: Some(destination),
                to_owner: Some(replacement),
                to_ty: Some(self.subst_ty(ty)),
            },
        ));
        self.owner_generations
            .insert(destination_binding, replacement.generation);
        self.publish_existing_physical_guard(destination_binding, replacement);
        self.set_owned_local_disposition(destination_binding, Disposition::ScopeExit);
        true
    }

    /// Central generation-aware synthetic-owner adoption. A MIR local is never
    /// allocated twice within one function; if an old sink reaches a result
    /// already adopted at publication, it receives that same owner rather than
    /// adding a second cleanup entry.
    pub(crate) fn adopt_synthetic_owned_local(
        &mut self,
        name: &'static str,
        site: SiteId,
        local: u32,
        ty: ResolvedTy,
        warrant: OwnerMintWarrant,
    ) -> BindingId {
        if let Some(existing) = self.owned_locals.iter().find_map(|entry| {
            (self.binding_locals.get(&entry.binding) == Some(&Place::Local(local)))
                .then_some(entry.binding)
        }) {
            if self.synthetic_owner_publication_sites.get(&existing) == Some(&site) {
                // The typed publication got here first.  A specialised legacy
                // sink is still the authority for its drop-plan shape, so keep
                // its structural name while preserving the one owner entry.
                if let Some(entry) = self
                    .owned_locals
                    .iter_mut()
                    .find(|entry| entry.binding == existing)
                {
                    entry.name = name.to_string();
                }
                // The first adopter emitted the raw Bind statement.  A later
                // specialised sink owns the drop-plan shape, so reflect that
                // same generation's structural role in the checker stream as
                // well—without adding a second binding or inferring a match
                // from a SiteId coincidence.
                for statement in &mut self.statements {
                    if let MirStatement::Bind {
                        binding,
                        name: existing_name,
                        ..
                    } = statement
                    {
                        if *binding == existing {
                            *existing_name = name.to_string();
                        }
                    }
                }
                return existing;
            }
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "owned result rewrote a live provisional owner local".to_string(),
                    site,
                },
                note: format!(
                    "Local({local}) still belongs to synthetic owner {existing:?}; retire or \
                     transfer that generation before publishing a new owned result"
                ),
            });
            return existing;
        }
        let binding =
            BindingId(SYNTHETIC_OWNED_TEMP_BINDING_BASE - self.synthetic_owned_temp_bindings);
        self.synthetic_owned_temp_bindings += 1;
        self.push_bind_statement(binding, name.to_string(), site, ty.clone());
        self.binding_locals.insert(binding, Place::Local(local));
        self.synthetic_owner_publication_sites.insert(binding, site);
        self.record_binding_scope(binding);
        self.register_owned_local(binding, name.to_string(), ty, warrant);
        binding
    }

    /// A `let` binder is registered before its backend destination local is
    /// wired.  If its initializer was already adopted at the typed publication
    /// boundary, retire that provisional source owner once the ordinary binder
    /// owner has its destination.  The binder's existing ledger entry remains
    /// the sole `ElabDrop` authority; this only removes the moved-from temporary.
    #[allow(
        clippy::too_many_lines,
        reason = "let adoption retires the provisional ledger and publishes its exact transfer as one invariant"
    )]
    pub(crate) fn retire_provisional_owner_for_bound_value(
        &mut self,
        binding: BindingId,
        name: &str,
        source: Place,
    ) {
        let Some(index) = self.owned_locals.iter().position(|entry| {
            entry.binding != binding
                && self.binding_locals.get(&entry.binding) == Some(&source)
                && self
                    .synthetic_owner_publication_sites
                    .contains_key(&entry.binding)
        }) else {
            return;
        };
        let provisional = self.owned_locals[index].binding;
        let provisional_ty = self.owned_locals[index].ty.clone();
        let handoff_site = self
            .synthetic_owner_publication_sites
            .get(&provisional)
            .copied()
            .unwrap_or(SiteId(0));
        if let (Some(source_local), Some(destination_local)) = (
            base_local(source),
            self.binding_locals
                .get(&binding)
                .copied()
                .and_then(base_local),
        ) {
            if self
                .call_scrutinee_carrier_mint_locals
                .remove(&source_local)
            {
                self.call_scrutinee_carrier_mint_locals
                    .insert(destination_local);
            }
        }
        if let Some(destination) = self.binding_locals.get(&binding).copied() {
            self.typed_produced_value_handoffs
                .insert((source, destination));
        }
        self.synthetic_owner_publication_sites.remove(&provisional);
        let typed_owner = self
            .typed_produced_value_owner_bindings
            .remove(&provisional);
        if typed_owner {
            self.typed_produced_value_owner_bindings.insert(binding);
        }
        if self
            .owned_locals
            .iter()
            .any(|entry| entry.binding == binding)
        {
            let destination = self.binding_locals.get(&binding).copied();
            let named_owner = self
                .owner_generations
                .get(&binding)
                .copied()
                .map(|generation| crate::model::OwnerId {
                    binding,
                    generation,
                });
            let provisional_owner =
                self.owner_generations
                    .get(&provisional)
                    .copied()
                    .map(|generation| crate::model::OwnerId {
                        binding: provisional,
                        generation,
                    });
            // Registration happens after the destination slot is allocated but
            // before the physical initializer Move.  Replace that provisional
            // destination Mint with the one exact handoff operation beside the
            // Move: the source generation ends and the named generation begins.
            // Keeping both events would publish two owners for the same bytes.
            if let Some(named_owner) = named_owner {
                self.instructions.retain(|instruction| {
                    !matches!(
                        instruction,
                        Instr::OwnershipEvent(crate::model::OwnershipEvent::Mint {
                            owner,
                            ..
                        }) if *owner == named_owner
                    )
                });
            }
            if let (Some(owner), Some(destination), Some(to_owner)) =
                (provisional_owner, destination, named_owner)
            {
                self.push_instr(Instr::OwnershipEvent(
                    crate::model::OwnershipEvent::Transfer {
                        owner,
                        from: source,
                        to: Some(destination),
                        to_owner: Some(to_owner),
                        to_ty: Some(provisional_ty.clone()),
                    },
                ));
            }
            // The ordinary `let` registrar already owns the destination slot.
            // Its ledger entry is the normal-successor authority. Keep the
            // moved-from publication generation as a consumed cleanup
            // candidate: an earlier `invoke` unwind still reaches that live
            // source slot, while the physical handoff neutralises it before
            // any later unwind can observe the destination owner.
            self.set_owned_local_consumed_post_lowering(
                provisional,
                self.binding_locals.get(&binding).copied(),
                DischargeSite::BindingMoved,
            );
            self.statements.push(MirStatement::Use {
                binding: provisional,
                name: "__hew_produced_value".to_string(),
                site: handoff_site,
                ty: provisional_ty,
                intent: IntentKind::Consume,
            });
            return;
        }

        // Some typed producers (notably `Weak<T>`) have a concrete ownership
        // fact even though the legacy `let` classifier does not mint a second
        // ordinary entry for their surface type. The physical initializer Move
        // is still an exact same-frame rebind. Publish that source generation ->
        // named generation transition through the warranted registrar at this
        // program point; never clone a compatibility-ledger row into existence.
        let Some(destination) = self.binding_locals.get(&binding).copied() else {
            return;
        };
        let warrant = self.owner_warrant_for_rebind(binding, provisional, &provisional_ty);
        if !self.register_owned_local_transfer_from(
            binding,
            name.to_string(),
            provisional_ty.clone(),
            provisional,
            (source, destination),
            warrant,
        ) {
            return;
        }
        self.set_owned_local_consumed_post_lowering(
            provisional,
            Some(destination),
            DischargeSite::BindingMoved,
        );
        self.statements.push(MirStatement::Use {
            binding: provisional,
            name: "__hew_produced_value".to_string(),
            site: handoff_site,
            ty: provisional_ty,
            intent: IntentKind::Consume,
        });
    }

    /// Retire a typed-publication temporary after an assignment has emitted an
    /// exact whole-value `Move` into an existing binding slot.  Unlike the
    /// `let` adoption seam above, assignment may run before projection/drop
    /// derivation has admitted the destination binding to `owned_locals`; the
    /// move itself is the transfer proof, so destination-ledger membership is
    /// deliberately not a prerequisite.
    #[allow(
        clippy::too_many_lines,
        reason = "assignment adoption keeps ledger retirement and exact generation transfer synchronized"
    )]
    pub(crate) fn retire_provisional_owner_after_assignment_move(
        &mut self,
        binding: BindingId,
        dest: Place,
        target_ty: &ResolvedTy,
        source: Place,
        source_ty: &ResolvedTy,
    ) {
        if self.binding_locals.get(&binding) != Some(&dest) {
            return;
        }
        let target_ty = self.subst_ty(target_ty);
        let source_ty = self.subst_ty(source_ty);
        if target_ty != source_ty {
            return;
        }
        let Some(index) = self.owned_locals.iter().position(|entry| {
            entry.binding != binding
                && entry.ty == source_ty
                && self.binding_locals.get(&entry.binding) == Some(&source)
                && self
                    .synthetic_owner_publication_sites
                    .contains_key(&entry.binding)
        }) else {
            // Some indirect-call results do not carry an independently minted
            // provisional owner, but assignment is still a language-level
            // definition of the destination. Publish that new generation at
            // the physical store instead of leaving the prior generation dead
            // for every later loop iteration.
            let carries_cleanup = crate::model::ty_carries_drop_obligation_mir(
                &target_ty,
                &self.record_field_orders,
                &self.enum_layouts,
                self.type_classes.lifecycle_registry(),
            ) || self.vec_iter_cursor_release_symbol(&target_ty).is_some();
            if !carries_cleanup {
                return;
            }
            let previous_generation = self.owner_generations.get(&binding).copied();
            let previous_owner = previous_generation.map(|generation| crate::model::OwnerId {
                binding,
                generation,
            });
            let previous_ended = previous_owner.is_some_and(|owner| {
                self.instructions.iter().rev().any(|instruction| {
                    matches!(
                        instruction,
                        Instr::OwnershipEvent(
                            crate::model::OwnershipEvent::Release { owner: ended, .. }
                                | crate::model::OwnershipEvent::GuardedRelease {
                                    owner: ended,
                                    ..
                                }
                                | crate::model::OwnershipEvent::Transfer {
                                    owner: ended,
                                    ..
                                }
                                | crate::model::OwnershipEvent::DemoteToAlias {
                                    owner: ended,
                                    ..
                                }
                        ) if *ended == owner
                    )
                })
            });
            let replacement = crate::model::OwnerId {
                binding,
                generation: previous_generation
                    .map_or(0, |generation| generation.saturating_add(1)),
            };
            self.owner_generations
                .insert(binding, replacement.generation);
            let event = if let Some(previous) = previous_owner.filter(|_| !previous_ended) {
                if self.affine_release_flags.contains_key(&binding)
                    || self.overwrite_guard_flags.contains_key(&binding)
                {
                    crate::model::OwnershipEvent::Rearm {
                        previous,
                        replacement,
                        place: dest,
                        ty: target_ty,
                    }
                } else {
                    crate::model::OwnershipEvent::Reset {
                        previous,
                        replacement,
                        place: dest,
                        ty: target_ty,
                    }
                }
            } else {
                crate::model::OwnershipEvent::Mint {
                    owner: replacement,
                    place: dest,
                    ty: target_ty,
                }
            };
            self.push_instr(Instr::OwnershipEvent(event));
            self.publish_existing_physical_guard(binding, replacement);
            self.set_owned_local_disposition(binding, Disposition::ScopeExit);
            return;
        };
        let provisional = self.owned_locals[index].binding;
        let provisional_ty = self.owned_locals[index].ty.clone();
        let handoff_site = self
            .synthetic_owner_publication_sites
            .get(&provisional)
            .copied()
            .unwrap_or(SiteId(0));
        if let (Some(source_local), Some(destination_local)) =
            (base_local(source), base_local(dest))
        {
            if self
                .call_scrutinee_carrier_mint_locals
                .remove(&source_local)
            {
                self.call_scrutinee_carrier_mint_locals
                    .insert(destination_local);
            }
        }
        self.synthetic_owner_publication_sites.remove(&provisional);
        // Record the assignment move as a typed handoff, exactly as the `let`
        // adoption seam does. `finalize_string_ownership` consults this set to
        // suppress the share-retain at a handoff move; without it the
        // reassignment spliced a `StringRetain` whose source temp — retired
        // right here — had no balancing release, leaking one node per
        // `var s = ...; s = <fresh producer>;` on every path.
        self.typed_produced_value_handoffs.insert((source, dest));
        let typed_owner = self
            .typed_produced_value_owner_bindings
            .remove(&provisional);
        if typed_owner {
            self.typed_produced_value_owner_bindings.insert(binding);
        }
        let previous_generation = self.owner_generations.get(&binding).copied();
        let replacement_generation =
            previous_generation.map_or(0, |generation| generation.saturating_add(1));
        let replacement = crate::model::OwnerId {
            binding,
            generation: replacement_generation,
        };
        self.owner_generations
            .insert(binding, replacement_generation);
        if let Some(provisional_generation) = self.owner_generations.get(&provisional).copied() {
            self.push_instr(Instr::OwnershipEvent(
                crate::model::OwnershipEvent::Transfer {
                    owner: crate::model::OwnerId {
                        binding: provisional,
                        generation: provisional_generation,
                    },
                    from: source,
                    to: Some(dest),
                    to_owner: Some(replacement),
                    to_ty: Some(target_ty.clone()),
                },
            ));
            self.publish_existing_physical_guard(binding, replacement);
        }
        self.set_owned_local_consumed_post_lowering(
            provisional,
            Some(dest),
            DischargeSite::BindingMoved,
        );
        self.statements.push(MirStatement::Use {
            binding: provisional,
            name: "__hew_produced_value".to_string(),
            site: handoff_site,
            ty: provisional_ty,
            intent: IntentKind::Consume,
        });
    }

    pub(crate) fn transfer_typed_produced_value_owner(
        &mut self,
        site: SiteId,
        source: Place,
        destination: Place,
    ) {
        for instruction in self.take_typed_produced_value_owner_transfer(site, source, destination)
        {
            self.push_instr(instruction);
        }
    }

    /// Commit a typed produced-value handoff and return the exact MIR
    /// operations that carry its runtime/discharge authority. Post-sealing CFG
    /// rewrites must insert these operations beside the transfer they describe;
    /// appending through `push_instr` would attach the event to whichever block
    /// the lowering cursor happened to retain.
    pub(crate) fn take_typed_produced_value_owner_transfer(
        &mut self,
        site: SiteId,
        source: Place,
        destination: Place,
    ) -> Vec<Instr> {
        let owners: Vec<_> = self
            .owned_locals
            .iter()
            .filter(|entry| {
                self.binding_locals.get(&entry.binding) == Some(&source)
                    && self.synthetic_owner_publication_sites.get(&entry.binding) == Some(&site)
                    && self
                        .typed_produced_value_owner_bindings
                        .contains(&entry.binding)
            })
            .map(|entry| entry.binding)
            .collect();
        let mut operations = Vec::new();
        for binding in owners {
            if let Some(flag) = self.affine_release_flags.get(&binding).copied() {
                operations.push(Instr::ConstI64 {
                    dest: flag,
                    value: 1,
                });
            }
            if let Some(generation) = self.owner_generations.get(&binding).copied() {
                operations.push(Instr::OwnershipEvent(
                    crate::model::OwnershipEvent::Transfer {
                        owner: crate::model::OwnerId {
                            binding,
                            generation,
                        },
                        from: source,
                        to: Some(destination),
                        to_owner: None,
                        to_ty: None,
                    },
                ));
            }
            self.set_owned_local_consumed_post_lowering(
                binding,
                Some(destination),
                DischargeSite::BindingMoved,
            );
            self.typed_produced_value_handoffs
                .insert((source, destination));
        }
        operations
    }

    /// Move a freshly published owner across a terminal boundary whose new
    /// owner is outside this MIR frame's `Place` domain: actor-state storage
    /// or a direct Hew callee's consuming parameter.
    ///
    /// The typed publication site selects the exact provisional `OwnerId`. The
    /// caller chooses the semantic boundary: actor-state lowering emits the
    /// event after its non-unwinding store, while direct-call lowering emits it
    /// immediately before invoke so the callee owns cleanup on both return and
    /// unwind. Returning whether an owner was found lets the caller null only a
    /// source that actually transferred; borrowed and bit-copy publications
    /// remain untouched.
    pub(crate) fn consume_typed_produced_value_owner_at_terminal_boundary(
        &mut self,
        site: SiteId,
        source: Place,
    ) -> bool {
        let owners: Vec<_> = self
            .owned_locals
            .iter()
            .filter(|entry| {
                self.binding_locals.get(&entry.binding) == Some(&source)
                    && self.synthetic_owner_publication_sites.get(&entry.binding) == Some(&site)
                    && self
                        .typed_produced_value_owner_bindings
                        .contains(&entry.binding)
            })
            .map(|entry| entry.binding)
            .collect();
        for binding in &owners {
            if let Some(flag) = self.affine_release_flags.get(binding).copied() {
                self.push_instr(Instr::ConstI64 {
                    dest: flag,
                    value: 1,
                });
            }
            if let Some(generation) = self.owner_generations.get(binding).copied() {
                self.push_instr(Instr::OwnershipEvent(
                    crate::model::OwnershipEvent::Transfer {
                        owner: crate::model::OwnerId {
                            binding: *binding,
                            generation,
                        },
                        from: source,
                        to: None,
                        to_owner: None,
                        to_ty: None,
                    },
                ));
            }
            self.set_owned_local_consumed_post_lowering(
                *binding,
                None,
                DischargeSite::BindingMoved,
            );
        }
        !owners.is_empty()
    }

    /// Relinquish a caller-side `VecIter<T>` cursor TEMPORARY at a direct Hew
    /// call, the caller half of the by-value cursor ABI.
    ///
    /// `lower_params` makes every by-value `VecIter<T>` parameter callee-owned
    /// (`scope_vec_iter_bindings` + its guard flag) whatever the borrow/consume
    /// summary says: the cursor protocol has no borrowed callee representation.
    /// A named argument authors the matching caller-side transition from its
    /// HIR `Consume` intent, and a summary-owned carrier authors it in the
    /// post-CFG carrier pass. A temporary receiver has neither — the checker
    /// has no binding to stamp — so its synthetic owner stayed live and the
    /// caller's exit plan released a cursor the callee had already freed.
    ///
    /// Emitted before the invoke, matching
    /// [`Self::consume_typed_produced_value_owner_at_terminal_boundary`]: the
    /// callee owns cleanup on both the return and the unwind edge.
    pub(crate) fn relinquish_vec_iter_cursor_argument(
        &mut self,
        site: SiteId,
        value: Place,
    ) -> bool {
        // Only the synthetic temp registrar publishes here; a named binding
        // argument is absent from this map and keeps its own lineage.
        let Some(owner) = self.vec_iter_value_owners.get(&site).copied() else {
            return false;
        };
        if self.binding_locals.get(&owner.binding) != Some(&value)
            || self.owner_generations.get(&owner.binding).copied() != Some(owner.generation)
        {
            return false;
        }
        let Some((name, ty)) = self.owned_locals.iter().find_map(|entry| {
            (entry.binding == owner.binding).then(|| (entry.name.clone(), entry.ty.clone()))
        }) else {
            return false;
        };
        if let Some(flag) = self.vec_iter_value_drop_flags.get(&site).copied() {
            // Disarm the caller's guarded cursor release: the callee's own
            // parameter guard is the single release authority from entry.
            self.push_instr(Instr::ConstI64 {
                dest: flag,
                value: 1,
            });
        }
        self.push_instr(Instr::OwnershipEvent(
            crate::model::OwnershipEvent::Transfer {
                owner,
                from: value,
                to: None,
                to_owner: None,
                to_ty: None,
            },
        ));
        self.statements.push(MirStatement::Use {
            binding: owner.binding,
            name,
            site,
            ty,
            intent: IntentKind::Consume,
        });
        self.set_owned_local_consumed_post_lowering(
            owner.binding,
            None,
            DischargeSite::BindingMoved,
        );
        true
    }

    /// End the supplied live owner generations as whole-value transfers to
    /// `destination`. The caller derives these identities and their current
    /// places by replaying the explicit MIR owner stream to the insertion
    /// point; the Builder ledger is used only to retire lowering cursors, never
    /// to rediscover which generation occupies `source`.
    pub(crate) fn take_exact_owner_transfers(
        &mut self,
        owners: &[(crate::model::OwnerId, Place)],
        destination: Place,
    ) -> Vec<Instr> {
        let mut operations = Vec::with_capacity(owners.len());
        for (owner, source) in owners {
            operations.push(Instr::OwnershipEvent(
                crate::model::OwnershipEvent::Transfer {
                    owner: *owner,
                    from: *source,
                    to: Some(destination),
                    to_owner: None,
                    to_ty: None,
                },
            ));
            self.set_owned_local_consumed_post_lowering(
                owner.binding,
                Some(destination),
                DischargeSite::BindingMoved,
            );
            self.typed_produced_value_handoffs
                .insert((*source, destination));
        }
        operations
    }

    /// Emit the typed move of a fresh Vec snapshot into an owning `VecIter`'s
    /// field-0 authority. Borrowing iterators deliberately emit no transfer.
    fn transfer_vec_iter_snapshot_owner(&mut self, value: &HirExpr, cursor: Place) {
        if !self.vec_iter_value_is_owned(value) {
            return;
        }
        let Some(source) = super::vec_iter_init_vec_source_expr(value) else {
            return;
        };
        let Some(source_place) = self.published_value_places.get(&source.site).copied() else {
            return;
        };
        let owners: Vec<_> = self
            .owned_locals
            .iter()
            .filter(|entry| {
                self.binding_locals.get(&entry.binding) == Some(&source_place)
                    && self.synthetic_owner_publication_sites.get(&entry.binding)
                        == Some(&source.site)
                    && self
                        .typed_produced_value_owner_bindings
                        .contains(&entry.binding)
            })
            .map(|entry| (entry.binding, entry.name.clone(), entry.ty.clone()))
            .collect();
        for (binding, name, ty) in owners {
            self.set_owned_local_consumed(binding, Some(cursor), DischargeSite::BindingMoved);
            self.statements.push(MirStatement::Use {
                binding,
                name,
                site: source.site,
                ty,
                intent: IntentKind::Consume,
            });
        }
    }

    /// Adopt an owned result immediately after its successful local
    /// publication. HIR's typed `SiteId` fact is the authority; borrowed and
    /// unknown values intentionally remain ownerless here.
    #[expect(
        clippy::too_many_lines,
        reason = "one authority seam validates and adopts every produced-value relation"
    )]
    pub(crate) fn adopt_typed_produced_value_owner(&mut self, expr: &HirExpr, place: Place) {
        let expected_ty = self.subst_ty(&expr.ty);
        if self.borrowed_runtime_result_places.contains(&place) {
            // The typed runtime call in MIR is the immutable authority: this
            // result aliases receiver-owned storage and therefore cannot mint
            // an independent destructor obligation, regardless of the HIR
            // produced-value approximation for an index expression.
            return;
        }
        if self
            .vec_iter_cursor_release_protocol(&expected_ty)
            .is_some()
        {
            // VecIter owns only its field-0 snapshot, and its path-sensitive
            // sidecar is the sole mint/discharge authority for that field.
            // Registering the whole produced value here would add a competing
            // RecordInPlace owner to normal exits while abandonment exits use
            // VecIterCursor, yielding two incompatible teardown rituals for
            // the same local.
            self.transfer_vec_iter_snapshot_owner(expr, place);
            return;
        }
        if matches!(
            place,
            Place::DuplexHandle(_)
                | Place::SendHalf(_)
                | Place::RecvHalf(_)
                | Place::LambdaActorHandle(_)
                | Place::ActorHandle(_)
        ) {
            // Handle-shaped Places are themselves the established ownership
            // authority. Their binding/drop seams preserve the handle kind;
            // a second generic Local-backed publication cannot exist.
            return;
        }
        if matches!(place, Place::Local(local) if self.tuple_decomp.contains_key(&local)) {
            // Multi-output runtime calls publish a bookkeeping proxy whose
            // component Places are the real independently-owned values. The
            // proxy has neither aggregate storage nor the aggregate's type;
            // its component close authorities are the complete ownership row.
            return;
        }
        let published_ty = match place {
            Place::Local(local) => self.locals.get(local as usize),
            _ => None,
        };
        if published_ty
            .is_some_and(|published_ty| !published_ty.is_storage_congruent_with(&expected_ty))
        {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "typed produced value published into non-congruent storage"
                        .to_string(),
                    site: expr.site,
                },
                note: format!(
                    "site {} has type {expected_ty:?}, but {place:?} has type {published_ty:?}",
                    expr.site
                ),
            });
            return;
        }
        let Some(fact) = self
            .param_ownership
            .produced_value_facts
            .get(&expr.site)
            .cloned()
        else {
            return;
        };
        if matches!(fact.ownership, ProducedValueOwnership::Owned { .. }) {
            self.publish_unique_active_yield_forwarding_consume(expr, place);
        }
        let call_carrier_mint = matches!(
            fact.producer,
            HirProducedValueProducer::Call
                | HirProducedValueProducer::ActorAsk
                | HirProducedValueProducer::RemoteActorAsk
                | HirProducedValueProducer::Await
                | HirProducedValueProducer::AwaitTask
                | HirProducedValueProducer::AwaitRestart
                | HirProducedValueProducer::ConnAwaitRead
                | HirProducedValueProducer::ListenerAwaitAccept
                | HirProducedValueProducer::ChannelRecvAwait
                | HirProducedValueProducer::StreamRecvAwait
                | HirProducedValueProducer::CallDynMethod
                | HirProducedValueProducer::CallTraitMethodStatic
                | HirProducedValueProducer::VarSelfMethodCall
                | HirProducedValueProducer::ResolvedImplCall
        );
        if matches!(fact.ownership, ProducedValueOwnership::ReceiverIdentity) {
            self.transfer_identity_owner(expr.site, fact.receiver, place);
            return;
        }
        match &fact.relation {
            HirProducedValueRelation::Identity(source) => {
                // `Identity` records value flow, not ownership by itself. A
                // borrowed block/scope tail is commonly secured into a fresh
                // branch-local MIR slot before the join; moving the source
                // owner's drop authority to that slot would schedule a drop
                // on sibling paths where the slot was never initialised. Only
                // the checker-authoritative Owned verdict licenses the owner
                // transfer. Borrowed / NoOwner / Unknown identities preserve
                // their existing source authority (or lack of one).
                if matches!(expr.kind, HirExprKind::CoerceToDynTrait { .. }) {
                    // A dyn coercion is not storage identity even though the
                    // produced-value graph records value continuity with its
                    // concrete input. Codegen boxes the concrete bytes and
                    // constructs a new fat-pointer carrier at `place`; that
                    // carrier needs its own HeapBoxed OwnerId so a borrowing
                    // call keeps caller cleanup authority and a named binding
                    // can adopt it. Continue to the ordinary Owned
                    // publication below. The adjacent
                    // NeutralizePayloadSlot independently ends the concrete
                    // source generation at the physical boxing point.
                } else {
                    if matches!(fact.ownership, ProducedValueOwnership::Owned { .. }) {
                        self.transfer_identity_owner(expr.site, Some(*source), place);
                    }
                    return;
                }
            }
            HirProducedValueRelation::Join(_) => {
                if let HirProducedValueRelation::Join(sources) = &fact.relation {
                    self.transfer_join_owners(expr.site, sources, place, &expected_ty);
                }
                return;
            }
            HirProducedValueRelation::Projection(source_site) => {
                if self
                    .published_value_places
                    .get(source_site)
                    .is_some_and(|source_place| {
                        self.owned_locals.iter().any(|entry| {
                            entry.disposition == Disposition::ScopeExit
                                && self.binding_locals.get(&entry.binding) == Some(source_place)
                        })
                    })
                {
                    // A projection from a freshly published aggregate is a
                    // view into the existing owner. The parent stays
                    // responsible for recursive teardown unless a later
                    // structural transfer neutralizes the projected slot.
                    return;
                }
            }
            HirProducedValueRelation::Leaf
            | HirProducedValueRelation::Subsumes(_)
            | HirProducedValueRelation::MoveOut(_) => {}
        }
        // Aggregate constructors themselves commonly carry a
        // `StructInit / Unknown` produced-value verdict: their lexical binder
        // is the parent owner authority. Their payloads still carry their own
        // exact ownership facts, though, and the normal-success constructor
        // must retire any child owner physically moved into the aggregate.
        // Admit only constructor shapes handled by the payload handoff below;
        // captured/borrowed children remain excluded there, while a call that
        // fails before construction keeps its source owner for unwind cleanup.
        let is_aggregate_init = matches!(
            expr.kind,
            HirExprKind::TupleLiteral { .. }
                | HirExprKind::StructInit { .. }
                | HirExprKind::MachineVariantCtor { .. }
        );
        if !matches!(fact.ownership, ProducedValueOwnership::Owned { .. }) && !is_aggregate_init {
            return;
        }
        if ValueClass::of_ty(&expected_ty, &self.type_classes) == ValueClass::BitCopy {
            // Generic clone bodies retain their abstract owned publication
            // after monomorphisation. A concrete scalar clone deliberately
            // reuses the parameter slot because the value has no release
            // obligation and therefore cannot mint a local owner.
            return;
        }
        if self.owned_locals.iter().any(|entry| {
            self.binding_locals.get(&entry.binding) == Some(&place)
                && self.synthetic_owner_publication_sites.get(&entry.binding) == Some(&expr.site)
                && self
                    .typed_produced_value_owner_bindings
                    .contains(&entry.binding)
        }) {
            // A multi-call producer may need to publish ownership at an
            // intermediate normal successor before its final HIR expression
            // returns to generic lowering. That early exact-site Mint is the
            // authority; the generic publication pass must not mint it again.
            return;
        }
        let aggregate_payloads: Vec<&HirExpr> = match &expr.kind {
            HirExprKind::TupleLiteral { elements } => elements.iter().collect(),
            HirExprKind::StructInit { fields, base, .. } => fields
                .iter()
                .map(|(_, value)| value)
                .chain(base.iter().map(AsRef::as_ref))
                .collect(),
            HirExprKind::MachineVariantCtor { payload, .. } => {
                payload.iter().flatten().map(|(_, value)| value).collect()
            }
            _ => Vec::new(),
        };
        for payload in aggregate_payloads {
            // Capture-intent aggregate fields are borrowed topology, not an
            // ownership hand-off.  The canonical example is `for x in v`:
            // the synthetic VecIter stores a descriptor view of `v`, while
            // `v` remains the sole owner and is usable after the loop.  Only a
            // MoveOut/consume payload may publish a child-to-aggregate
            // Transfer; treating a capture as one ended the Vec owner at the
            // RecordInit and poisoned both source move dataflow and cleanup.
            if payload.intent == IntentKind::Capture {
                continue;
            }
            let Some(payload_place) = self.published_value_places.get(&payload.site).copied()
            else {
                continue;
            };
            let current_place = self
                .instructions
                .iter()
                .rev()
                .find_map(|instruction| match instruction {
                    Instr::Move { dest, src }
                        if *src == payload_place && base_local(*dest) == base_local(place) =>
                    {
                        Some(*dest)
                    }
                    _ => None,
                })
                .unwrap_or(payload_place);
            // Replay the current block's explicit owner operations for every
            // lexical owner of the payload slot. This admits named binders as
            // well as synthetic producers, but only when the physical
            // aggregate-field Move actually carried that exact generation to
            // `current_place`. A retained copy has a compensating Relocate
            // back to its source and therefore does not satisfy this proof.
            //
            // Do not filter this row through `OwnedLocal::disposition`.
            // Disposition is a lowering cursor, not path state: an early-return
            // constructor may already have marked the same lexical binding
            // consumed on its branch while a later constructor consumes it on
            // the disjoint fallthrough branch.  Each constructor must publish
            // its own terminal Transfer; Checked-MIR replay decides which one
            // is reachable on a particular path.
            let payload_owners: Vec<_> = self
                .owned_locals
                .iter()
                .filter(|entry| self.binding_locals.get(&entry.binding) == Some(&payload_place))
                .filter_map(|entry| {
                    let owner = crate::model::OwnerId {
                        binding: entry.binding,
                        generation: *self.owner_generations.get(&entry.binding)?,
                    };
                    let mut actual = Some(payload_place);
                    for instruction in &self.instructions {
                        match instruction {
                            Instr::Move { dest, src } | Instr::WitnessMove { dest, src, .. }
                                if actual == Some(*src) =>
                            {
                                actual = Some(*dest);
                            }
                            Instr::OwnershipEvent(crate::model::OwnershipEvent::Mint {
                                owner: event_owner,
                                place,
                                ..
                            }) if *event_owner == owner => actual = Some(*place),
                            Instr::OwnershipEvent(crate::model::OwnershipEvent::Relocate {
                                owner: event_owner,
                                to,
                                ..
                            }) if *event_owner == owner => actual = Some(*to),
                            Instr::OwnershipEvent(
                                crate::model::OwnershipEvent::Transfer {
                                    owner: event_owner, ..
                                }
                                | crate::model::OwnershipEvent::Release {
                                    owner: event_owner, ..
                                }
                                | crate::model::OwnershipEvent::GuardedRelease {
                                    owner: event_owner,
                                    ..
                                }
                                | crate::model::OwnershipEvent::DemoteToAlias {
                                    owner: event_owner,
                                    ..
                                },
                            ) if *event_owner == owner => actual = None,
                            Instr::OwnershipEvent(
                                crate::model::OwnershipEvent::Reset { previous, .. }
                                | crate::model::OwnershipEvent::Rearm { previous, .. },
                            ) if *previous == owner => actual = None,
                            Instr::OwnershipEvent(
                                crate::model::OwnershipEvent::Reset {
                                    replacement, place, ..
                                }
                                | crate::model::OwnershipEvent::Rearm {
                                    replacement, place, ..
                                },
                            ) if *replacement == owner => actual = Some(*place),
                            _ => {}
                        }
                    }
                    (actual == Some(current_place)).then_some(entry.binding)
                })
                .collect();
            for binding in payload_owners {
                self.set_owned_local_consumed_from(
                    binding,
                    current_place,
                    Some(place),
                    DischargeSite::BindingMoved,
                );
                self.typed_produced_value_handoffs
                    .insert((payload_place, place));
            }
        }
        if matches!(
            expr.kind,
            HirExprKind::BindingRef {
                resolved: ResolvedRef::Binding(_),
                ..
            }
        ) {
            // A consuming binding reference transfers the generation already
            // registered for its binding. Its typed MoveOut fact proves the
            // handoff to a parent/callee, but the reference does not produce a
            // second owner over the same MIR slot.
            return;
        }
        // Linear values are accounted for by the binding move checker, which
        // carries the MustConsume obligation across consuming-method calls.
        // A second synthetic scope owner has no corresponding consume edge and
        // therefore turns a valid `let tx = Tx { .. }; tx.commit()` into an
        // impossible extra obligation.
        if ValueClass::of_ty(&expected_ty, &self.type_classes) == ValueClass::Linear {
            return;
        }
        let Place::Local(local) = place else {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "typed owned producer without a local publication slot".to_string(),
                    site: expr.site,
                },
                note: format!(
                    "owned {:?} result was published at {place:?}",
                    fact.producer
                ),
            });
            return;
        };
        if self.parameter_locals.contains(&local) {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "typed owned producer reused a parameter slot".to_string(),
                    site: expr.site,
                },
                note: "a fresh owner must publish into a fresh MIR local".to_string(),
            });
            return;
        }
        let ty = expected_ty;
        let warrant = Self::owner_warrant_for_typed_produced_value(fact.ownership);
        // Do not reserve a synthetic binding when the typed publication cannot
        // carry a caller owner. A later specialised sink may have a narrower,
        // independently-proven admission path; an unminted placeholder here
        // must not block that path's real owner.
        if warrant.withholds_mint() {
            return;
        }
        // An owned publication may secure an already-owned child into this
        // result local with one adjacent Move (array/block/aggregate result
        // forwarding). Commit that exact, type-congruent source generation
        // here while the lowerer still has the typed ownership verdict and the
        // concrete operation; post-Checked global Move ancestry is neither
        // needed nor permitted. A read/copy publication has no Owned verdict
        // and never enters this transfer path.
        let adjacent_source = self
            .instructions
            .last()
            .and_then(|instruction| match instruction {
                Instr::Move { dest, src } if *dest == place => Some(*src),
                _ => None,
            });
        if let Some(source) = adjacent_source {
            let mut owners = self.owned_locals.iter().filter(|entry| {
                entry.ty == ty
                    && entry.disposition == Disposition::ScopeExit
                    && self.binding_locals.get(&entry.binding) == Some(&source)
            });
            if let Some(owner) = owners.next().cloned().filter(|_| owners.next().is_none()) {
                self.push_instr(Instr::NeutralizePayloadSlot {
                    place: source,
                    transferee: Some(place),
                    authority: crate::model::NeutralizeAuthority::WholeCarrierConsume,
                });
                self.set_owned_local_consumed(
                    owner.binding,
                    Some(place),
                    DischargeSite::BindingMoved,
                );
                self.statements.push(MirStatement::Use {
                    binding: owner.binding,
                    name: owner.name,
                    site: expr.site,
                    ty: owner.ty,
                    intent: IntentKind::Consume,
                });
                self.typed_produced_value_handoffs.insert((source, place));
            }
        }
        // A scoped block result puts its immutable `ScopeExit` marker between
        // the physical tail Move and this typed publication.  The marker is
        // bookkeeping, not another value-producing instruction: when its
        // carried destination is this exact place, the unique live typed owner
        // of the Move source remains the sole generation after the handoff.
        // Array literals are HIR-desugared to precisely this shape. Minting an
        // anonymous owner here would overlap the relocated `__hew_array_N`
        // generation at the destination.
        let mut crossed_scope_exit = false;
        let mut scoped_tail_source = None;
        for instruction in self.instructions.iter().rev() {
            match instruction {
                Instr::OwnershipEvent(crate::model::OwnershipEvent::ScopeExit {
                    carry_places,
                    ..
                }) if carry_places.contains(&place) => crossed_scope_exit = true,
                Instr::Move { dest, src } if crossed_scope_exit && *dest == place => {
                    scoped_tail_source = Some(*src);
                    break;
                }
                _ => break,
            }
        }
        if let Some(source) = scoped_tail_source {
            let mut owners = self.owned_locals.iter().filter(|entry| {
                entry.ty.is_storage_congruent_with(&ty)
                    && entry.disposition == Disposition::ScopeExit
                    && self.binding_locals.get(&entry.binding) == Some(&source)
                    && self
                        .typed_produced_value_owner_bindings
                        .contains(&entry.binding)
            });
            if owners.next().is_some() && owners.next().is_none() {
                self.typed_produced_value_handoffs.insert((source, place));
                return;
            }
        }
        // A composite result (`match`/`if` value) is published at the head of
        // its join block, after every arm has already moved its value into
        // `place`. An arm whose source is a live exact owner (a consumed
        // payload binding such as `__try_ok`) must end that generation at its
        // own Move: the fresh join generation minted below is then the sole
        // owner of the slot. Without this handoff the arm owner relocates into
        // `place` and two live generations share one Place.
        for owner in self.composite_join_relocated_owners(place, &ty, false) {
            self.set_owned_local_consumed_post_lowering(
                owner,
                Some(place),
                DischargeSite::BindingMoved,
            );
            let source = self.binding_locals[&owner];
            self.typed_produced_value_handoffs.insert((source, place));
        }
        let binding = self.adopt_synthetic_owned_local(
            "__hew_produced_value",
            expr.site,
            local,
            ty.clone(),
            warrant,
        );
        self.typed_produced_value_owner_bindings.insert(binding);
        if call_carrier_mint {
            self.call_scrutinee_carrier_mint_locals.insert(local);
        }
        if matches!(ty, ResolvedTy::TraitObject { .. }) {
            self.dyn_trait_storage
                .insert(binding, crate::TraitObjectStorage::HeapBoxed);
        }
    }

    /// End an active yield binder on the normal successor of a direct call
    /// whose result may alias exactly that binder and no other heap argument.
    ///
    /// A break/continue scope marker is lowered after the call. Without this
    /// path-local consume, that marker sees the binder's lexical owner still
    /// live and materialises a drop even though the unverified callee may have
    /// forwarded the same buffer into its result. The result's typed producer
    /// independently publishes the caller's destination owner below; this
    /// event closes only the argument generation on the successful call edge.
    ///
    /// The proof is deliberately narrower than `ParamsOnly`: the summary says
    /// "some parameter", not which one. Therefore exactly one non-scalar heap
    /// argument must exist, it must be a `string` active-yield binder, its place
    /// must have one unique lexical owner, and the just-sealed call must name
    /// that exact argument/result/continuation tuple. Zero, multiple, indirect,
    /// opaque, or mismatched cases publish nothing and remain fail-closed.
    fn publish_unique_active_yield_forwarding_consume(
        &mut self,
        expr: &HirExpr,
        result_place: Place,
    ) {
        if !matches!(self.subst_ty(&expr.ty), ResolvedTy::String) {
            return;
        }
        let HirExprKind::Call { callee, args, .. } = &expr.kind else {
            return;
        };
        let HirExprKind::BindingRef {
            name: callee_name,
            resolved: ResolvedRef::Item(callee_id),
        } = &callee.kind
        else {
            return;
        };
        if !self
            .call_scrutinee_provenance
            .provenance
            .get(callee_id)
            .is_some_and(|bits| bits.is_params_only())
        {
            return;
        }
        let heap_args: Vec<_> = args
            .iter()
            .enumerate()
            .filter(|(_, arg)| {
                !crate::return_provenance::ty_is_scalar_non_heap(&self.subst_ty(&arg.ty))
            })
            .collect();
        let [(arg_index, arg)] = heap_args.as_slice() else {
            return;
        };
        if !matches!(self.subst_ty(&arg.ty), ResolvedTy::String) {
            return;
        }
        let Some(binding) = binding_ref_target(arg) else {
            return;
        };
        let Some(source_place) = self.binding_locals.get(&binding).copied() else {
            return;
        };
        if !self.active_yield_binder_place(source_place) {
            return;
        }
        let Some(owner) = self
            .current_owner_id_at_place(source_place)
            .filter(|owner| owner.binding == binding)
        else {
            return;
        };
        let exact_call = self.pending_blocks.iter().filter(|block| {
            matches!(
                &block.terminator,
                Terminator::Call {
                    callee,
                    args,
                    dest: Some(dest),
                    next,
                    ..
                } if callee == callee_name
                    && *dest == result_place
                    && *next == self.current_block_id
                    && args.get(*arg_index) == Some(&source_place)
            )
        });
        if exact_call.count() != 1 {
            return;
        }
        let name = self
            .owned_locals
            .iter()
            .find(|entry| entry.binding == binding)
            .map_or_else(
                || "__hew_forwarded_yield".to_string(),
                |entry| entry.name.clone(),
            );
        self.statements.push(MirStatement::Use {
            binding,
            name,
            site: expr.site,
            ty: ResolvedTy::String,
            intent: IntentKind::Consume,
        });
        self.push_instr(Instr::OwnershipEvent(
            crate::model::OwnershipEvent::Transfer {
                owner,
                from: source_place,
                to: None,
                to_owner: None,
                to_ty: None,
            },
        ));
    }

    /// Consume typed-publication generations whose release was materialised
    /// by the established inline string/bytes temporary spine.  The release
    /// instruction and the synthetic binding name the same MIR local; this is
    /// a generation hand-off, not a type/name heuristic.  Persistent results
    /// receive no inline release and remain scope-exit owned.
    ///
    /// `released_before_splices` is the inline-release set as it stood BEFORE
    /// the ownership finalizers ran, and it is excluded here. Retirement is
    /// paired with the finalizers that placed the release — it is emphatically
    /// NOT "this local has an inline release somewhere, so drop its ledger
    /// entry". A body whose own lowering already emitted inline string/bytes
    /// drops (the lambda-actor handler's message and reply teardown) would
    /// otherwise have every publication owner retired against a release that
    /// discharges a different generation.
    pub(crate) fn consume_typed_publication_owners_at_inline_release(
        &mut self,
        blocks: &mut [BasicBlock],
        released_before_splices: &HashSet<u32>,
    ) {
        let (entries, _) = super::drop_plan::exact_owner_states(blocks);
        let mut release_sites = Vec::new();
        for block in blocks.iter() {
            let mut live = entries.get(&block.id).cloned().unwrap_or_default();
            for (index, instruction) in block.instructions.iter().enumerate() {
                if let Instr::Drop {
                    place: Place::Local(local),
                    drop_fn: Some(crate::model::DropFnSpec::Release(symbol)),
                    ..
                } = instruction
                {
                    if matches!(*symbol, "hew_string_drop" | "hew_bytes_drop")
                        && !released_before_splices.contains(local)
                        && !local_is_rewritten_after_current_iteration(
                            blocks, *local, block.id, index,
                        )
                    {
                        let owners = live
                            .iter()
                            .filter_map(|(owner, place)| {
                                (*place == Place::Local(*local)).then_some(*owner)
                            })
                            .collect::<Vec<_>>();
                        if let [owner] = owners.as_slice() {
                            release_sites.push((block.id, index + 1, *owner, Place::Local(*local)));
                        }
                    }
                }
                super::drop_plan::apply_exact_owner_ops(
                    std::slice::from_ref(instruction),
                    &mut live,
                );
            }
        }
        let released: HashMap<u32, Vec<u32>> =
            release_sites
                .iter()
                .fold(HashMap::new(), |mut by_local, (block, _, _, place)| {
                    let Place::Local(local) = place else {
                        return by_local;
                    };
                    by_local.entry(*local).or_default().push(*block);
                    by_local
                });
        let consumed: Vec<_> = self
            .owned_locals
            .iter()
            .filter_map(|entry| {
                self.typed_produced_value_owner_bindings
                    .contains(&entry.binding)
                    .then(|| self.binding_locals.get(&entry.binding).copied())
                    .flatten()
                    .and_then(|place| match place {
                        Place::Local(local) => released.get(&local).map(|release_blocks| {
                            (
                                entry.binding,
                                entry.name.clone(),
                                entry.ty.clone(),
                                release_blocks.clone(),
                            )
                        }),
                        _ => None,
                    })
            })
            .collect();
        for (block_id, index, owner, place) in release_sites.into_iter().rev() {
            let Some(block) = blocks.iter_mut().find(|block| block.id == block_id) else {
                continue;
            };
            block.instructions.insert(
                index,
                Instr::OwnershipEvent(crate::model::OwnershipEvent::Release { owner, place }),
            );
            super::shift_instr_spans_on_insert(
                &mut self.instr_spans,
                block_id,
                u32::try_from(index).unwrap_or(u32::MAX),
            );
        }
        for (binding, _name, _ty, _release_blocks) in consumed {
            self.set_owned_local_consumed_post_lowering(
                binding,
                None,
                DischargeSite::InlineRelease,
            );
        }
    }

    /// The unique scope-exit owners (of type `ty`) that a predecessor arm
    /// physically moved into the join slot `place`, when the join owner is
    /// about to be minted at the head of the join block. Each of these owners
    /// must end its generation at its arm's Move (recorded as a typed handoff)
    /// so the join owner is the sole generation at `place`; a named payload
    /// binder used as a bare arm value (`.Ok(value) => value`) carries a
    /// Borrowed produced-value fact and is otherwise invisible to the join.
    ///
    /// With `arm_local_only`, only owners bound in a scope that is no longer
    /// active (the arm's own scope) qualify; owners of an enclosing scope are
    /// still live on the sibling arms and belong to the divergent-selection
    /// passes.
    fn composite_join_relocated_owners(
        &self,
        place: Place,
        ty: &ResolvedTy,
        arm_local_only: bool,
    ) -> Vec<BindingId> {
        if !self.instructions.is_empty() {
            return Vec::new();
        }
        self.composite_join_predecessor_move_sources(place)
            .into_iter()
            .filter_map(|source| {
                let mut owners = self.owned_locals.iter().filter(|entry| {
                    entry.ty == *ty
                        && entry.disposition == Disposition::ScopeExit
                        && self.binding_locals.get(&entry.binding) == Some(&source)
                        && (!arm_local_only
                            || self
                                .binding_scope
                                .get(&entry.binding)
                                .is_some_and(|scope| !self.active_scopes.contains(scope)))
                });
                owners
                    .next()
                    .map(|entry| entry.binding)
                    .filter(|_| owners.next().is_none())
            })
            .collect()
    }

    /// The source of each predecessor arm's final physical Move into `place`,
    /// for every finished block that falls through into the current (join)
    /// block. A Move immediately preceded by a retain of its source is an
    /// independent `+1` share, not a relocation of the source generation, and
    /// is skipped: the source owner stays live and the join owns the share.
    ///
    /// SHORTCUT (structural scan, not an ownership fact).
    /// WHY: arm lowering does not record "this Move relocates a live owner
    /// generation" as an event, so the join publication has to recover it by
    /// re-reading the predecessor blocks; the retain exemption enumerates the
    /// `+1` share instructions (`StringRetain`/`BytesRetain`) because those are
    /// the only retains an arm emits directly before its result Move today.
    /// WHEN: delete once every arm Move that relocates an owner emits its own
    /// generation-ending event at the Move site (the ladder's virtual-value /
    /// `Place` seam, `Materialize { reason }`), at which point the join derives
    /// from replay and no predecessor scan is needed. Until then the retain
    /// exemption is a closed list, and its failure mode is NOT fail-closed: a
    /// new retain instruction that precedes an arm Move is not recognised as a
    /// `+1` share, so the still-live source owner is treated as relocated and
    /// its generation ended — the verifier then sees one owner per place (no
    /// finding) while the source's retained `+1` is never released. That is a
    /// silent under-release caught only by the leak oracles, so any new retain
    /// variant must be added to the match below in the same change.
    /// WHAT: the real solution is a per-arm "value relocated into the join
    /// slot" event produced by the arm lowering itself.
    fn composite_join_predecessor_move_sources(&self, place: Place) -> Vec<Place> {
        self.pending_blocks
            .iter()
            .filter(|block| {
                matches!(
                    block.terminator,
                    Terminator::Goto { target } if target == self.current_block_id
                )
            })
            .filter_map(|block| {
                let (index, source) = block.instructions.iter().enumerate().rev().find_map(
                    |(index, instruction)| match instruction {
                        Instr::Move { dest, src } if *dest == place => Some((index, *src)),
                        _ => None,
                    },
                )?;
                let retained_copy = index > 0
                    && match &block.instructions[index - 1] {
                        Instr::StringRetain { value, .. } | Instr::BytesRetain { value } => {
                            *value == source
                        }
                        _ => false,
                    };
                (!retained_copy && matches!(source, Place::Local(_))).then_some(source)
            })
            .collect()
    }

    /// Move (not clone) an existing receiver owner to a receiver-identity
    /// result. Both ends are SiteId/Place identities; no method spelling or
    /// display-name recovery participates in the transfer.
    #[allow(
        clippy::too_many_lines,
        reason = "identity-return transfer validates and publishes the full receiver authority tuple atomically"
    )]
    fn transfer_identity_owner(
        &mut self,
        result_site: SiteId,
        receiver_site: Option<SiteId>,
        result_place: Place,
    ) {
        let Some(receiver_site) = receiver_site else {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "receiver-identity result without receiver SiteId".to_string(),
                    site: result_site,
                },
                note: "the typed carrier must name the existing receiver owner".to_string(),
            });
            return;
        };
        let Some(receiver_place) = self.published_value_places.get(&receiver_site).copied() else {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "receiver-identity receiver publication missing".to_string(),
                    site: result_site,
                },
                note: format!("receiver site {receiver_site} has no published MIR place"),
            });
            return;
        };
        let owners: Vec<_> = self
            .owned_locals
            .iter()
            .filter(|entry| self.binding_locals.get(&entry.binding) == Some(&receiver_place))
            .map(|entry| (entry.binding, entry.ty.clone()))
            .collect();
        if owners.is_empty() {
            // Receiver identity is also used for by-value, non-owning
            // carriers (notably machine values).  Absence of a ledger owner
            // there is a structural no-op, not permission to mint one.
            return;
        }
        let result_ty = match result_place {
            Place::Local(local) => self.locals.get(local as usize),
            _ => None,
        };
        if let Some((_, owner_ty)) = owners
            .iter()
            .find(|(_, owner_ty)| result_ty.is_some_and(|result_ty| result_ty != owner_ty))
        {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "identity owner transfer into non-congruent storage".to_string(),
                    site: result_site,
                },
                note: format!(
                    "source owner has type {owner_ty:?}, but {result_place:?} has type {result_ty:?}"
                ),
            });
            return;
        }
        // The owners the call block already ended with a terminal Transfer:
        // a `consuming self` receiver hands its generation to the callee at
        // the argument (the callee owns it, on unwind too).
        let call_predecessor = self.pending_blocks.iter().find(|block| {
            matches!(
                &block.terminator,
                Terminator::Call {
                    args,
                    dest: Some(dest),
                    next,
                    ..
                } if *dest == result_place
                    && *next == self.current_block_id
                    && args.contains(&receiver_place)
            )
        });
        let ended_at_call: Option<HashSet<crate::model::OwnerId>> = call_predecessor.map(|block| {
            block
                .instructions
                .iter()
                .filter_map(|instruction| match instruction {
                    Instr::OwnershipEvent(crate::model::OwnershipEvent::Transfer {
                        owner,
                        to: None,
                        ..
                    }) => Some(*owner),
                    _ => None,
                })
                .collect()
        });
        for (binding, owner_ty) in owners {
            let generation = self.owner_generations.get(&binding).copied().unwrap_or(0);
            let owner = crate::model::OwnerId {
                binding,
                generation,
            };
            if let Some(ended_at_call) = &ended_at_call {
                let replacement = crate::model::OwnerId {
                    binding,
                    generation: generation.saturating_add(1),
                };
                // The returned identity of an already-ended receiver is a
                // fresh owner from the caller's side: publish it as the
                // binding's next generation minted at the result slot, never
                // as a transfer out of the generation the call block ended.
                let receiver_ended_at_call = ended_at_call.contains(&owner);
                self.owner_generations
                    .insert(binding, replacement.generation);
                self.push_bind_statement(
                    binding,
                    "__hew_produced_value".to_string(),
                    result_site,
                    owner_ty.clone(),
                );
                if let Some(entry) = self
                    .owned_locals
                    .iter_mut()
                    .find(|entry| entry.binding == binding)
                {
                    entry.disposition = Disposition::ScopeExit;
                }
                let event = if receiver_ended_at_call {
                    crate::model::OwnershipEvent::Mint {
                        owner: replacement,
                        place: result_place,
                        ty: owner_ty,
                    }
                } else {
                    // A borrowed receiver keeps caller cleanup authority
                    // through the call (and its unwind). Only the normal
                    // successor commits the receiver and publishes the
                    // returned identity as the next generation.
                    crate::model::OwnershipEvent::Transfer {
                        owner,
                        from: receiver_place,
                        to: Some(result_place),
                        to_owner: Some(replacement),
                        to_ty: Some(owner_ty),
                    }
                };
                self.push_instr(Instr::OwnershipEvent(event));
            } else if receiver_place != result_place {
                // `lower_value_for_move` conservatively records a consuming
                // binding reference before the typed produced-value fact is
                // available.  A ReceiverIdentity result is not a terminal
                // consume: the same generation continues in the result slot.
                // Canonicalise the exact adjacent `Transfer; Move` spine into
                // `Move; Relocate` at this program point.  Leaving both events
                // made the Relocate name a generation the Transfer had already
                // ended (the for-loop Option/VecIter carrier class).
                let terminal_move =
                    self.instructions
                        .windows(2)
                        .enumerate()
                        .rev()
                        .find_map(|(index, pair)| {
                            let matches_pair = matches!(
                                &pair[0],
                                Instr::OwnershipEvent(crate::model::OwnershipEvent::Transfer {
                                    owner: event_owner,
                                    from,
                                    to: None,
                                    to_owner: None,
                                    ..
                                }) if *event_owner == owner && *from == receiver_place
                            ) && matches!(
                                pair[1],
                                Instr::Move { dest, src }
                                    if dest == result_place && src == receiver_place
                            );
                            let metadata_only_tail = self.instructions[index + 2..]
                                .iter()
                                .all(|instruction| matches!(instruction, Instr::OwnershipEvent(_)));
                            (matches_pair && metadata_only_tail).then_some(index)
                        });
                let relocation = Instr::OwnershipEvent(crate::model::OwnershipEvent::Relocate {
                    owner,
                    from: receiver_place,
                    to: result_place,
                });
                if let Some(transfer_index) = terminal_move {
                    // ScopeExit/EdgeCarry are immutable ownership metadata,
                    // not executable program points. Typed identity may be
                    // finalized after they are published, but the physical
                    // operation remains the adjacent `Transfer; Move` pair.
                    // Rewrite that pair in place so source spans stay aligned;
                    // any executable instruction in the tail prevents this
                    // canonicalization and leaves the stale event visible to
                    // Checked-MIR validation.
                    self.instructions.swap(transfer_index, transfer_index + 1);
                    self.instructions[transfer_index + 1] = relocation;
                } else {
                    // Identity forwarding within one block keeps the same live
                    // generation but changes its physical carrier. Checked
                    // MIR, not the lowering ledger, records that relocation.
                    self.push_instr(relocation);
                }
            }
            self.binding_locals.insert(binding, result_place);
            if let Some(current_scope) = self.active_scopes.last().copied() {
                let scope = self
                    .binding_scope
                    .get(&binding)
                    .copied()
                    .filter(|existing_scope| {
                        self.active_scopes
                            .iter()
                            .position(|active| active == existing_scope)
                            .is_some_and(|existing_depth| existing_depth < self.active_scopes.len())
                    })
                    .unwrap_or(current_scope);
                self.binding_scope.insert(binding, scope);
            }
            if self
                .synthetic_owner_publication_sites
                .contains_key(&binding)
            {
                self.synthetic_owner_publication_sites
                    .insert(binding, result_site);
            }
        }
    }

    fn transfer_join_owners(
        &mut self,
        result_site: SiteId,
        source_sites: &[SiteId],
        result_place: Place,
        result_ty: &ResolvedTy,
    ) {
        // An arm-local owner moved bare into the join slot (a payload binder
        // returned as the arm value, `.Ok(value) => value`) has no Owned
        // produced-value fact of its own and cannot be live after its arm;
        // recover it from the predecessor's Move so its generation ends at the
        // arm and the join owner is the sole generation here. Owners of an
        // enclosing scope are left to the divergent-selection passes, which
        // also release the unselected sibling on the other arms.
        let mut transferred: HashSet<BindingId> = self
            .composite_join_relocated_owners(result_place, result_ty, true)
            .into_iter()
            .collect();
        for source_site in source_sites {
            if !self
                .param_ownership
                .produced_value_facts
                .get(source_site)
                .is_some_and(|fact| matches!(fact.ownership, ProducedValueOwnership::Owned { .. }))
            {
                continue;
            }
            let Some(source_place) = self.published_value_places.get(source_site).copied() else {
                continue;
            };
            let owners: Vec<_> = self
                .owned_locals
                .iter()
                .filter(|entry| {
                    self.binding_locals.get(&entry.binding) == Some(&source_place)
                        && entry.disposition == Disposition::ScopeExit
                })
                .map(|entry| entry.binding)
                .collect();
            for binding in owners {
                transferred.insert(binding);
            }
        }
        if transferred.is_empty() {
            return;
        }

        // A control-flow join is one runtime generation selected from several
        // mutually exclusive predecessors. Represent that phi as one owner
        // declared in the join block. Keeping the predecessor bindings mapped
        // to the shared result place makes the path-insensitive reverse map
        // pick one arbitrary branch and either release too early or miss the
        // other path entirely.
        let Some(template) = self
            .owned_locals
            .iter()
            .find(|entry| transferred.contains(&entry.binding))
            .cloned()
        else {
            return;
        };
        let binding =
            BindingId(SYNTHETIC_OWNED_TEMP_BINDING_BASE - self.synthetic_owned_temp_bindings);
        self.synthetic_owned_temp_bindings += 1;
        self.push_bind_statement(
            binding,
            "__hew_produced_value".to_string(),
            result_site,
            template.ty.clone(),
        );
        self.binding_locals.insert(binding, result_place);
        self.synthetic_owner_publication_sites
            .insert(binding, result_site);
        self.typed_produced_value_owner_bindings.insert(binding);
        self.record_binding_scope(binding);
        for predecessor in transferred {
            let Some(source) = self.binding_locals.get(&predecessor).copied() else {
                continue;
            };
            // The source generation is live only on its producing predecessor.
            // Record the staging disposition here, then let the finalized-CFG
            // handoff pass place the Transfer beside that predecessor's exact
            // physical Move. Emitting through the current builder cursor would
            // put a second transfer in the join block, where paths that never
            // minted this generation can reach it.
            self.set_owned_local_consumed_post_lowering(
                predecessor,
                Some(result_place),
                DischargeSite::BindingMoved,
            );
            self.typed_produced_value_handoffs
                .insert((source, result_place));
        }
        self.register_owned_local(
            binding,
            "__hew_produced_value".to_string(),
            template.ty.clone(),
            Self::owner_warrant_for_typed_produced_value(ProducedValueOwnership::owned(
                hew_types::ProducedValueAcquisition::MoveOut,
            )),
        );
        if let Some(entry) = self
            .owned_locals
            .iter_mut()
            .find(|entry| entry.binding == binding)
        {
            entry.ownership = template.ownership;
            entry.provenance = template.provenance;
        }
    }

    /// Finalize the typed owner for a fresh composite cloned by `Vec` indexing
    /// and immediately used as a record-projection base.
    pub(crate) fn finalize_vec_clone_projection_base_owner(
        &mut self,
        object: &HirExpr,
        record_place: Place,
    ) {
        if !matches!(object.kind, HirExprKind::Index { .. }) {
            return;
        }
        let owners = self.finalize_typed_produced_value_owners(
            "__hew_produced_value",
            object.site,
            record_place,
        );
        self.seed_exact_vec_clone_projection_root(record_place, &owners);
    }

    /// Publish destructive projection-transfer authority only for one exact
    /// synthetic clone owner.
    ///
    /// A fresh `Vec<T>` clone used directly as a record projection has no
    /// source binding through which the ordinary carrier path can discover its
    /// root. Once the typed produced-value publication resolves uniquely, the
    /// root is safe to neutralize field-by-field when a nested functional
    /// update consumes a projection. Borrowed projections resolve no owner;
    /// duplicate publications are ambiguous. Both remain fail-closed.
    fn seed_exact_vec_clone_projection_root(
        &mut self,
        record_place: Place,
        finalized_owners: &[(BindingId, ResolvedTy)],
    ) {
        if finalized_owners.len() == 1 {
            self.owned_carrier_neutralize
                .entry(record_place)
                .or_insert(OwnedCarrierNeutralizeTarget::Whole(record_place));
        }
    }
    /// Register a `let`-bound field projection whose result is a byte-copy
    /// interior ALIAS of the still-live owner named by `provenance` — the
    /// [`ByteCopyAlias`](FieldLoadClass::ByteCopyAlias) class of the field-load
    /// three-way split (record / tuple / inline-enum aggregate field). The entry
    /// carries its
    /// [`OwnershipDecision::InteriorAlias`]-shaped provenance and is minted
    /// [`Disposition::AliasOf`], so it drops out of the scope-exit-live view the
    /// ownership finalizers read and never mints an owner generation: the alias emits no
    /// composite drop of its own (the owner's composite frees the whole tree),
    /// and its base local never seeds the record/tuple provers'
    /// `release_owner_bases`, so it no longer trips their Defect-1 blanket
    /// exclusion of every root (#2375).
    pub(crate) fn register_owned_local_alias(
        &mut self,
        binding: BindingId,
        name: String,
        ty: ResolvedTy,
        provenance: ValueProvenance,
        warrant: OwnerMintWarrant,
    ) {
        if warrant.withholds_mint() {
            return;
        }
        let place = self
            .binding_locals
            .get(&binding)
            .copied()
            .unwrap_or(Place::Local(0));
        let ownership = ValueOwnership::classify(&ty, place, &self.ownership_ctx());
        self.owned_locals.push(OwnedLocalEntry {
            binding,
            name,
            ty,
            ownership,
            provenance: Some(provenance),
            disposition: Disposition::AliasOf,
        });
    }

    /// Return the unique byte-copy alias provenance currently recorded for a
    /// binding. A direct rebind of such an alias (`let tmp = alias`) must stay
    /// an alias: the whole-value `Move` copies the same parent-owned aggregate
    /// storage and does not recursively retain its heap leaves.
    ///
    /// Multiple ledger rows or a type mismatch report `Ambiguous` so the caller
    /// can reject the mint fail-closed. The source may already be `ConsumedAt`:
    /// lowering the consuming `BindingRef` ends that lexical generation before
    /// this successor is registered, but it does not turn the copied storage
    /// into an independent owner.
    pub(crate) fn exact_owned_local_alias_provenance(
        &self,
        binding: BindingId,
        ty: &ResolvedTy,
    ) -> OwnedAliasInheritance {
        let mut entries = self
            .owned_locals
            .iter()
            .filter(|entry| entry.binding == binding);
        let Some(entry) = entries.next() else {
            return OwnedAliasInheritance::NotAlias;
        };
        if entries.next().is_some() || entry.ty != *ty {
            return OwnedAliasInheritance::Ambiguous;
        }
        entry.provenance.clone().map_or(
            OwnedAliasInheritance::NotAlias,
            OwnedAliasInheritance::Exact,
        )
    }
    /// #2743 — the owned type of a fresh composite/string argument TEMPORARY that
    /// earns a caller-side scope-exit drop when passed to a proven-BORROW
    /// parameter, or `None` for every other arg shape.
    ///
    /// The gap #2735 left: a NAMED owned composite arg (`let x = Row{..}; g(x)`)
    /// keeps its scope-exit drop through the alias-escape scan because
    /// `proven_borrow_whole_arg_locals` exempts its owning binding. A fresh
    /// rvalue TEMPORARY (`g(Row{..})`) has no `let`, no `BindingId`, and no
    /// scope-exit drop to preserve — the exemption is a no-op and nobody frees
    /// it. Minting a synthetic owner over the temporary's MIR local (at the call
    /// site) routes it through the identical `owned_locals` machinery.
    ///
    /// ## Why the producer proofs are sound
    ///
    /// Finalize the typed owner of a Vec COPY-IN source after all call
    /// arguments have materialised. Push uses args[0]/places[1], while set uses
    /// args[1]/places[2]; the receiver and set index can never become owners.
    pub(crate) fn finalize_vec_copy_in_source_owner(
        &mut self,
        callee: &str,
        args: &[HirExpr],
        arg_places: &[Place],
    ) {
        let candidate = match callee {
            "hew_vec_push_owned" if args.len() == 1 => Some((&args[0], 1)),
            "hew_vec_set_owned" if args.len() == 2 => Some((&args[1], 2)),
            _ => None,
        }
        .and_then(|(arg, place_index)| Some((arg, arg_places.get(place_index).copied()?)));
        let Some((candidate_arg, Place::Local(local))) = candidate else {
            return;
        };
        if self.parameter_locals.contains(&local) {
            return;
        }
        self.finalize_typed_produced_value_owner(
            SYNTHETIC_COPY_IN_PARAM_TEMP_NAME,
            candidate_arg.site,
            Place::Local(local),
        );
    }
    /// Whether a direct `Call` has an audited fresh-owned-string result.
    ///
    /// Runtime primitives read the hand-written result contract used by MIR
    /// lowering. Declared externs read the generated ownership table instead:
    /// a measured `result = "fresh"` / transferred row is the positive proof
    /// that its caller receives exactly one releasable share. Both authorities
    /// fail closed for an absent symbol.
    ///
    /// The symbol is resolved from the callee identity exactly as value
    /// lowering does (`runtime_symbol_for_call_expr`): a
    /// `ResolvedRef::Builtin` family via its catalog `c_symbol()`, every other
    /// resolved callee via the checker-minted callee name.
    fn call_produces_fresh_owned_string(&self, callee: &HirExpr) -> bool {
        let HirExprKind::BindingRef { name, resolved } = &callee.kind else {
            return false;
        };
        let symbol = match resolved {
            ResolvedRef::Builtin(family) => family.c_symbol(),
            _ => name.as_str(),
        };
        crate::runtime_symbols::callee_ownership_contract(symbol).produces_fresh_owned_string()
            || self
                .call_scrutinee_provenance
                .extern_table
                .extern_return_is_audited_fresh_owner(symbol)
    }
    /// The symbol a direct-`Call` callee is keyed by: a `ResolvedRef::Builtin`
    /// family via its catalog `c_symbol()`, every other resolved callee via the
    /// checker-minted callee name. `""` for a callee that is not a `BindingRef`
    /// (a closure value, a fn-pointer parameter, any indirect dispatch) — no
    /// declared extern can carry the empty name, so an opacity lookup on it
    /// answers `false` and the caller's own resolved-item gate stays the
    /// authority for those shapes.
    fn callee_symbol_name(callee: &HirExpr) -> &str {
        let HirExprKind::BindingRef { name, resolved } = &callee.kind else {
            return "";
        };
        match resolved {
            ResolvedRef::Builtin(family) => family.c_symbol(),
            _ => name.as_str(),
        }
    }
    /// Whether a `string`-returning direct-`Call` callee is a non-runtime
    /// function the module return-carrier authority proves hands back exactly
    /// one independently releasable share.
    ///
    /// The runtime `produces_fresh_owned_string` contract only covers catalogued
    /// symbols; a user function (`fn mk(i: i64) -> string { f"tok{i}" }`, a
    /// `Display::fmt` impl, a generic monomorphisation) has no row and reads as
    /// `FAIL_CLOSED`. That left the caller-side mint blind to every user-produced
    /// string temp passed straight into a borrowing parameter — the shape
    /// `println(f"v={mk(i)}")` produces, where the interpolation operand feeds
    /// `string::fmt` with no `let` to anchor a scope-exit drop and the buffer
    /// leaked once per evaluation.
    ///
    /// Fail-closed on two halves:
    ///
    /// * a KNOWN runtime symbol is rejected here outright, so the runtime
    ///   contract keeps its veto — a catalogued callee returning a BORROWED or
    ///   receiver-interior-alias string can never be laundered into a mint;
    /// * every OTHER shape must satisfy the module return-carrier or fresh-owner
    ///   authority. Those authorities carry BOTH vetoes: a declared `extern "C"`
    ///   callee is rejected by NAME unless the
    ///   audited extern contract table proves its return is a fresh `+1` owner,
    ///   and a Hew callee is rejected unless the module-global least-fixpoint
    ///   proved its body fresh (generic origins included) AND free of unmeasured
    ///   opaque-extern laundering on every return path. The authority has no
    ///   permissive cross-ABI fallback: a body-less, unanalysed resolved item is
    ///   not a proof of freshness and reads `false`.
    ///
    /// Unlike the fresh-owner query, the return-carrier query admits a
    /// `ParamsOnly` body. String lowering turns each admitted parameter-derived
    /// path into exactly one caller-owned share: a whole by-value parameter is
    /// retained before the return-slot write, and a record/tuple string
    /// projection is retained by its field load. A mixed function is admitted
    /// only when every path has this same one-share postcondition. The authority
    /// is string-specific and does not widen global fresh/non-alias facts.
    ///
    /// Registration alone still never forces a release: the minted local flows
    /// through `derive_cow_sole_owner` / `derive_cow_fresh_borrowed_owner`,
    /// which drop it only when it is a proven fresh, untainted owner whose every
    /// use is a verified borrow.
    fn user_call_produces_owned_string_carrier(&self, callee: &HirExpr) -> bool {
        if crate::runtime_symbols::is_known_runtime_symbol(Self::callee_symbol_name(callee)) {
            return false;
        }
        self.call_scrutinee_provenance
            .callee_returns_owned_string_carrier(callee)
            || callee_returns_retained_string_owner(
                callee,
                &self.call_scrutinee_provenance.fresh_owner_verdicts,
            )
    }
    /// The ARGUMENT-side sibling of
    /// [`FreshOwnerVerdicts::symbol_is_ownership_opaque_extern`], which is the
    /// RETURN-side authority: `true` when `symbol` names a declared `extern "C"`
    /// fn with no audited contract proving it BORROWS the heap arguments it is
    /// handed.
    ///
    /// The two questions are different and must not be substituted for one
    /// another. A `-> ()` extern carries a vacuous audited fresh-RETURN row (it
    /// returns no handle at all), which says nothing whatsoever about what it
    /// does with the `string` it is PASSED — the host may retain it or release it
    /// with `hew_string_drop`. `extern_borrows_audited_heap_args` is the argument
    /// authority.
    ///
    /// This is the same veto, from the same table, that
    /// [`crate::lower::temp_drop::string_call_borrows`] applies ahead of its
    /// `module_fn_names` clause.
    ///
    /// [`FreshOwnerVerdicts::symbol_is_ownership_opaque_extern`]:
    ///     crate::return_provenance::FreshOwnerVerdicts::symbol_is_ownership_opaque_extern
    pub(crate) fn callee_is_arg_ownership_opaque_extern(&self, symbol: &str) -> bool {
        let table = &self.call_scrutinee_provenance.extern_table;
        table.is_extern_name(symbol) && !table.extern_borrows_audited_heap_args(symbol)
    }

    /// `true` when `symbol` is an analyzed Hew function whose `string` argument a
    /// caller may still hold a sole-owner release obligation over.
    ///
    /// This replaces a residual DISPATCH-SET check in `lower_direct_call`'s
    /// synthetic temp-arg mint. `module_fn_names` is the call-dispatch set: it
    /// deliberately carries every `HirItem::ExternFn` so extern calls lower as a
    /// `Terminator::Call`. Answering an OWNERSHIP question from it registers a
    /// caller-side owner for a temporary handed to an extern purely because the
    /// extern is dispatchable — a second, dispatch-shaped ownership answer over a
    /// handle whose real ownership behaviour is unknowable. So the audited
    /// [`ExternContractTable`] is consulted FIRST, exactly as at the payload gate
    /// ([`crate::lower::temp_drop::string_call_borrows`]), and only then does the
    /// Hew-body dispatch fallback apply.
    ///
    /// The veto reads the ARGUMENT authority
    /// ([`Builder::callee_is_arg_ownership_opaque_extern`]), not the return-side
    /// one: the question here is what the callee does with the handle it is
    /// GIVEN, and a `-> ()` extern's vacuous audited fresh-RETURN row proves
    /// nothing about that.
    ///
    /// Final string ownership re-checks the terminator through the same table and
    /// excludes an escaping argument, so this was never the only guard — but a
    /// preliminary ownership answer must come from the one authority too.
    ///
    /// [`ExternContractTable`]: crate::return_provenance::ExternContractTable
    pub(crate) fn callee_is_analyzed_hew_arg_sink(&self, symbol: &str) -> bool {
        !self.callee_is_arg_ownership_opaque_extern(symbol)
            && (self.module_fn_names.contains(symbol)
                || self.module_generic_fn_names.contains(symbol))
    }
    /// #2648 S2b — the caller-side argument scan for a `ParamsOnly` callee
    /// (plan Fix-design (2), pulled forward from S4b by the ratchet evidence:
    /// the interim PARAM-present reject falsely rejected genuine `ParamsOnly`
    /// stdlib callers — `template.try_parse("…")` and friends). True iff EVERY
    /// argument is provably fresh, in which case the callee's `PARAM`-aliasing
    /// return can only alias fresh inputs — a fresh sole owner.
    ///
    /// Consulted ONLY for a `{PARAM}`-only summary; an `OPAQUE`-carrying
    /// summary is never arg-rescuable.
    pub(crate) fn params_only_args_provably_fresh(&self, args: &[HirExpr]) -> bool {
        args.iter().all(|a| self.scrutinee_arg_provably_fresh(a))
    }
    /// The inline-fresh recursion. Fresh shapes ADMIT:
    /// - a scalar-typed argument (owns no heap — cannot be the forwarded
    ///   buffer);
    /// - a literal / record clone (fresh by construction);
    /// - an aggregate (`StructInit`/`TupleLiteral`/`MachineVariantCtor`) whose
    ///   EVERY operand is recursively fresh;
    /// - a nested call to a Fresh-summary module fn, or to a `ParamsOnly`
    ///   module fn whose own arguments are recursively fresh;
    /// - a builtin-collection method that lowers to a proved-owner EMITTED
    ///   symbol (clone/retain/take — the F1 contract);
    /// - a local binding proven solely-owned fresh by the per-function
    ///   freshness facts (S1 bits `∅`, plain `let`, unaliased, single read).
    ///
    /// EVERYTHING ELSE fails closed — notably a heap-owning PLACE
    /// (`h.b`, the primary #2648 forwarder repro), a bare parameter, an
    /// aliased or re-read local, an extern call, and any unmodelled form.
    pub(crate) fn scrutinee_arg_provably_fresh(&self, arg: &HirExpr) -> bool {
        use crate::return_provenance::{method_return_provenance, ty_is_scalar_non_heap};
        if ty_is_scalar_non_heap(&self.subst_ty(&arg.ty)) {
            return true;
        }
        match &arg.kind {
            HirExprKind::Literal(_) | HirExprKind::RecordCloneCall { .. } => true,
            HirExprKind::StructInit { fields, base, .. } => {
                fields
                    .iter()
                    .all(|(_, v)| self.scrutinee_arg_provably_fresh(v))
                    && base
                        .as_deref()
                        .is_none_or(|b| self.scrutinee_arg_provably_fresh(b))
            }
            HirExprKind::TupleLiteral { elements } => elements
                .iter()
                .all(|e| self.scrutinee_arg_provably_fresh(e)),
            HirExprKind::MachineVariantCtor { payload, .. } => payload
                .as_ref()
                .is_none_or(|fs| fs.iter().all(|(_, v)| self.scrutinee_arg_provably_fresh(v))),
            HirExprKind::Call { callee, args, .. } => {
                let HirExprKind::BindingRef {
                    name,
                    resolved: ResolvedRef::Item(id),
                } = &callee.kind
                else {
                    return false;
                };
                // An extern call dispatches by NAME (placeholder ItemId) and no
                // heap-returning extern is trusted fresh in the interim.
                if self.call_scrutinee_provenance.extern_names.contains(name) {
                    return false;
                }
                match self.call_scrutinee_provenance.provenance.get(id) {
                    Some(bits) if bits.is_fresh() => true,
                    Some(bits) if bits.is_params_only() => {
                        self.params_only_args_provably_fresh(args)
                    }
                    Some(_) => false,
                    // An audited builtin collection constructor (`Vec::new()`)
                    // is a fresh empty allocation — the same clause the
                    // Precise policy's `classify_call` applies.
                    None => crate::return_provenance::is_builtin_fresh_ctor(name),
                }
            }
            // A builtin-collection getter is fresh iff the EMITTED symbol is a
            // proved-owner clone/retain/take (the F1 contract) — never the HIR
            // placeholder.
            HirExprKind::ResolvedImplCall { .. } => self
                .method_scrutinee_emitted_symbol(arg)
                .is_some_and(|sym| method_return_provenance(&sym).is_fresh()),
            HirExprKind::BindingRef {
                resolved: ResolvedRef::Binding(id),
                ..
            } => self
                .call_scrutinee_local_freshness
                .local_is_provably_fresh(*id),
            _ => false,
        }
    }
    pub(crate) fn register_from_call_scrutinee_owner(
        &mut self,
        scrutinee: &HirExpr,
        scrutinee_local: u32,
    ) -> Option<(BindingId, ResolvedTy)> {
        let label = match &scrutinee.kind {
            HirExprKind::Call { callee, .. } => match &callee.kind {
                HirExprKind::BindingRef { name, .. } => format!("{name}(...)"),
                _ => "direct call expression".to_string(),
            },
            _ => "call scrutinee expression".to_string(),
        };
        let owner = self.finalize_typed_produced_value_owner(
            SYNTHETIC_CALL_SCRUTINEE_NAME,
            scrutinee.site,
            Place::Local(scrutinee_local),
        );
        if owner.is_some() {
            self.call_scrutinee_diagnostics
                .insert(scrutinee_local, (scrutinee.site, label));
        }
        owner
    }

    /// Reject an ownership-demanding sink whose total HIR row is unresolved.
    /// This check runs before the sink emits CFG or storage, keeping unknown
    /// ownership out of checked MIR and codegen.
    pub(crate) fn typed_produced_value_demand_is_resolved(
        &mut self,
        expr: &HirExpr,
        construct: &'static str,
    ) -> bool {
        let ty = self.subst_ty(&expr.ty);
        if matches!(
            expr.kind,
            HirExprKind::BindingRef {
                resolved: ResolvedRef::Binding(_),
                ..
            }
        ) {
            return true;
        }
        if !matches!(ty, ResolvedTy::TraitObject { .. })
            && !crate::model::ty_owns_heap_mir(&ty, &self.record_field_orders, &self.enum_layouts)
        {
            return true;
        }
        let ownership = self
            .param_ownership
            .produced_value_facts
            .get(&expr.site)
            .map_or(ProducedValueOwnership::Unknown, |fact| fact.ownership);
        if matches!(
            ownership,
            ProducedValueOwnership::Owned { .. }
                | ProducedValueOwnership::NoOwner
                | ProducedValueOwnership::Borrowed
                | ProducedValueOwnership::ReceiverIdentity
        ) {
            return true;
        }
        let construct = if matches!(expr.kind, HirExprKind::Call { .. }) {
            "call-scrutinee ownership is unresolved"
        } else {
            construct
        };
        self.diagnostics.push(MirDiagnostic {
            kind: MirDiagnosticKind::NotYetImplemented {
                construct: construct.to_string(),
                site: expr.site,
            },
            note: format!(
                "HIR produced-value ownership for site {} is {ownership:?}; a heap-owning value \
                 must be resolved before this sink can transfer or release it",
                expr.site,
            ),
        });
        false
    }

    /// Recover the unique typed owner carried into `place` by an exact handoff.
    /// The producer records the source/destination relation while both Places
    /// are concrete; a borrowing sink can then keep that same generation even
    /// though its lexical binding still names the pre-Move source slot.
    fn exact_handoff_publication_owner(&self, place: Place) -> Option<(BindingId, ResolvedTy)> {
        let mut owners = self
            .typed_produced_value_handoffs
            .iter()
            .filter_map(|(source, destination)| (*destination == place).then_some(*source))
            .flat_map(|source| {
                self.owned_locals.iter().filter(move |entry| {
                    entry.disposition == Disposition::ScopeExit
                        && self.binding_locals.get(&entry.binding) == Some(&source)
                        && self
                            .typed_produced_value_owner_bindings
                            .contains(&entry.binding)
                })
            });
        let owner = owners.next()?;
        owners
            .next()
            .is_none()
            .then(|| (owner.binding, owner.ty.clone()))
    }

    /// Complete a provisional publication generation when a structural sink
    /// takes responsibility for its cleanup shape. The site and exact MIR
    /// place must both match the typed publication; neither type nor display
    /// name can select an owner.
    pub(crate) fn finalize_typed_produced_value_owner(
        &mut self,
        name: &'static str,
        site: SiteId,
        place: Place,
    ) -> Option<(BindingId, ResolvedTy)> {
        self.finalize_typed_produced_value_owners(name, site, place)
            .into_iter()
            .next()
    }

    pub(crate) fn finalize_typed_produced_value_owners(
        &mut self,
        name: &'static str,
        site: SiteId,
        place: Place,
    ) -> Vec<(BindingId, ResolvedTy)> {
        let owners: Vec<_> = self
            .owned_locals
            .iter()
            .filter(|entry| {
                self.binding_locals.get(&entry.binding) == Some(&place)
                    && self.synthetic_owner_publication_sites.get(&entry.binding) == Some(&site)
                    && self
                        .typed_produced_value_owner_bindings
                        .contains(&entry.binding)
            })
            .map(|entry| (entry.binding, entry.ty.clone()))
            .collect();
        if owners.is_empty() {
            if let Some(owner) = self.exact_handoff_publication_owner(place) {
                return vec![owner];
            }
        }
        for (binding, _) in &owners {
            if let Some(entry) = self
                .owned_locals
                .iter_mut()
                .find(|entry| entry.binding == *binding)
            {
                entry.name = name.to_string();
            }
            for statement in &mut self.statements {
                if let MirStatement::Bind {
                    binding: statement_binding,
                    name: statement_name,
                    ..
                } = statement
                {
                    if *statement_binding == *binding {
                        *statement_name = name.to_string();
                    }
                }
            }
            self.synthetic_owner_publication_sites.remove(binding);
            self.typed_produced_value_owner_bindings.remove(binding);
        }
        owners
    }

    /// An owned projection can be represented by its parent aggregate's
    /// scope-exit owner instead of an independent provisional leaf owner. The
    /// relation and published source place must both agree, and the parent
    /// owner must still be live; otherwise the borrowing sink remains
    /// fail-closed.
    pub(crate) fn typed_projection_has_live_parent_owner(&self, expr: &HirExpr) -> bool {
        let Some(fact) = self.param_ownership.produced_value_facts.get(&expr.site) else {
            return false;
        };
        if !matches!(fact.ownership, ProducedValueOwnership::Owned { .. }) {
            return false;
        }
        let HirProducedValueRelation::Projection(source_site) = fact.relation else {
            return false;
        };
        let Some(source_place) = self.published_value_places.get(&source_site) else {
            return false;
        };
        self.owned_locals.iter().any(|entry| {
            entry.disposition == Disposition::ScopeExit
                && self.binding_locals.get(&entry.binding) == Some(source_place)
        })
    }

    /// Complete the exact parent owner behind a borrowed string projection.
    ///
    /// A retained field read owns its independent read-copy, while the parent
    /// aggregate still owns the original field. A direct call-result
    /// projection (`println(make_record().field)`) therefore needs both the
    /// leaf's inline release and the parent's recursive scope-exit drop. Merely
    /// observing the provisional parent owner is insufficient: unresolved
    /// synthetic publications intentionally mint no owner.
    /// Finalizing that exact publication makes the original aggregate owner
    /// eligible without minting a second owner.
    pub(crate) fn finalize_typed_projection_parent_owner(&mut self, expr: &HirExpr) -> bool {
        let Some(fact) = self.param_ownership.produced_value_facts.get(&expr.site) else {
            return false;
        };
        if !matches!(fact.ownership, ProducedValueOwnership::Borrowed) {
            return false;
        }
        let HirProducedValueRelation::Projection(source_site) = fact.relation else {
            return false;
        };
        if !self
            .param_ownership
            .produced_value_facts
            .get(&source_site)
            .is_some_and(|source| matches!(source.ownership, ProducedValueOwnership::Owned { .. }))
        {
            return false;
        }
        let Some(source_place) = self.published_value_places.get(&source_site).copied() else {
            return false;
        };
        let mut live_parents = self.owned_locals.iter().filter(|entry| {
            entry.disposition == Disposition::ScopeExit
                && self.binding_locals.get(&entry.binding) == Some(&source_place)
        });
        let Some(parent) = live_parents.next() else {
            return false;
        };
        if live_parents.next().is_some() {
            return false;
        }
        let parent_binding = parent.binding;
        match self
            .synthetic_owner_publication_sites
            .get(&parent_binding)
            .copied()
        {
            None => true,
            Some(site) if site == source_site => !self
                .finalize_typed_produced_value_owners(
                    super::SYNTHETIC_TEMP_PROJECTION_PARENT_NAME,
                    source_site,
                    source_place,
                )
                .is_empty(),
            Some(_) => false,
        }
    }
    pub(crate) fn register_while_let_iteration_owner(
        &mut self,
        scrutinee: &HirExpr,
        snapshot_local: u32,
        ty: ResolvedTy,
    ) -> BindingId {
        let warrant = self.owner_warrant_for_admitted_temp(scrutinee);
        let binding = self.adopt_synthetic_owned_local(
            SYNTHETIC_WHILE_LET_ITERATION_NAME,
            scrutinee.site,
            snapshot_local,
            ty,
            warrant,
        );
        self.back_edge_only_iteration_owners.insert(binding);
        binding
    }
    /// The discarded result of the synthetic `VecIter::next` state machine,
    /// when its `Option<T>` payload owns heap and therefore needs an immediate
    /// recursive in-place drop before the reusable result slot is forgotten.
    pub(crate) fn discarded_vec_iter_next_owned_ty(&self, expr: &HirExpr) -> Option<ResolvedTy> {
        if !hir_expr_contains_synthetic_vec_get_clone(expr) {
            return None;
        }
        let ty = self.subst_ty(&expr.ty);
        ty_is_heap_owning_enum_composite(
            &ty,
            &self.record_field_orders,
            &self.enum_layouts,
            self.type_classes.lifecycle_registry(),
        )
        .then_some(ty)
    }
    pub(crate) fn register_discarded_call_result_owner(&mut self, expr: &HirExpr, place: Place) {
        let typed_ty = self.subst_ty(&expr.ty);
        let enum_in_place = ty_is_heap_owning_enum_composite(
            &typed_ty,
            &self.record_field_orders,
            &self.enum_layouts,
            self.type_classes.lifecycle_registry(),
        );
        let owns_heap = crate::model::ty_owns_heap_mir(
            &typed_ty,
            &self.record_field_orders,
            &self.enum_layouts,
        );
        let is_resource = matches!(
            ValueClass::of_ty(&typed_ty, &self.type_classes),
            ValueClass::AffineResource | ValueClass::Linear
        );
        if !enum_in_place && !owns_heap && !is_resource {
            return;
        }
        let Some((binding, ty)) = self.finalize_typed_produced_value_owner(
            SYNTHETIC_DISCARDED_CALL_RESULT_NAME,
            expr.site,
            place,
        ) else {
            if self
                .param_ownership
                .produced_value_facts
                .get(&expr.site)
                .is_some_and(|fact| matches!(fact.ownership, ProducedValueOwnership::Owned { .. }))
            {
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::NotYetImplemented {
                        construct: "owned discarded result without provisional owner".to_string(),
                        site: expr.site,
                    },
                    note: "an owning HIR result must register its exact MIR generation before the discard sink"
                        .to_string(),
                });
            }
            return;
        };
        self.statements.push(MirStatement::Use {
            binding,
            name: SYNTHETIC_DISCARDED_CALL_RESULT_NAME.to_string(),
            site: expr.site,
            ty: ty.clone(),
            intent: IntentKind::Consume,
        });
        let Some(generation) = self.owner_generations.get(&binding).copied() else {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "owned discarded result without owner generation".to_string(),
                    site: expr.site,
                },
                note: "the exact typed discarded-result owner resolved, but no generation was published; refusing an untracked inline release"
                    .to_string(),
            });
            return;
        };
        let owner = crate::model::OwnerId {
            binding,
            generation,
        };
        let before = self.instructions.len();
        if enum_in_place {
            self.push_instr(Instr::Drop {
                place,
                ty: ty.clone(),
                drop_fn: Some(crate::model::DropFnSpec::InPlace(
                    crate::ownership::InPlaceReleaseKind::Enum,
                )),
            });
        } else {
            self.emit_local_overwrite_release(binding, place, &ty, None);
        }
        if self.instructions.len() == before {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "owned discarded result release".to_string(),
                    site: expr.site,
                },
                note: format!(
                    "discarded owned value of type {} has no complete inline release plan",
                    ty.user_facing()
                ),
            });
            return;
        }
        self.push_instr(Instr::OwnershipEvent(
            crate::model::OwnershipEvent::Release { owner, place },
        ));
        self.set_owned_local_disposition(binding, Disposition::ScopeReleased);
    }
    pub(crate) fn record_iteration_owner_drop(
        &mut self,
        binding: BindingId,
        name: &str,
        site: SiteId,
        ty: &ResolvedTy,
    ) {
        let Some(generation) = self.owner_generations.get(&binding).copied() else {
            return;
        };
        let Some(place) = self.binding_locals.get(&binding).copied() else {
            return;
        };
        let owner = crate::model::OwnerId {
            binding,
            generation,
        };
        if self.instructions.iter().any(|instr| {
            matches!(
                instr,
                Instr::OwnershipEvent(crate::model::OwnershipEvent::Release {
                    owner: released,
                    place: released_place,
                }) if *released == owner && *released_place == place
            )
        }) {
            return;
        }
        self.statements.push(MirStatement::Use {
            binding,
            name: name.to_string(),
            site,
            ty: ty.clone(),
            intent: IntentKind::Consume,
        });
        self.push_instr(Instr::Drop {
            place,
            ty: ty.clone(),
            drop_fn: Some(crate::model::DropFnSpec::InPlace(
                crate::ownership::InPlaceReleaseKind::Enum,
            )),
        });
        self.push_instr(Instr::OwnershipEvent(
            crate::model::OwnershipEvent::Release { owner, place },
        ));
    }
    pub(crate) fn record_active_iteration_owner_drops_for_exit_edge(
        &mut self,
        min_scope_depth: usize,
    ) {
        let owners: Vec<ActiveIterationOwner> = self
            .active_iteration_owners
            .iter()
            .rev()
            .filter(|owner| owner.scope_depth >= min_scope_depth)
            .cloned()
            .collect();
        for owner in owners {
            self.record_iteration_owner_drop(owner.binding, &owner.name, owner.site, &owner.ty);
        }
    }
    /// Provenance of a `let`-bound field projection that is a byte-copy interior
    /// ALIAS of a still-live owner — the
    /// [`ByteCopyAlias`](FieldLoadClass::ByteCopyAlias) class of the three-way
    /// field-load classification. Returns `Some` ONLY for a `root.field` /
    /// `root.N` projection (`let mid = o.mid`, `let inner = t.0`) of a live
    /// binding whose FIELD type is an inline aggregate
    /// (`OwnsHeap { layout: Product | TaggedUnion }` — record / tuple /
    /// inline-enum). Codegen byte-copies such a field with no retain, so the
    /// binder does not own the copied heap; the projected root's composite drop
    /// frees every original exactly once.
    ///
    /// Returns `None` — keeping today's `ScopeExit` ownership — for the other two
    /// load classes the split names, so their behaviour is unchanged:
    /// [`Retained`](FieldLoadClass::Retained) (a `string` field: codegen
    /// `hew_string_clone`s the load, so the binder owns a fresh `+1` released by
    /// its own drop) and [`HandleTransfer`](FieldLoadClass::HandleTransfer) (a
    /// single-pointer heap leaf — `Vec` / `bytes` / `HashMap` / `HashSet` /
    /// `Generator` / indirect-enum node: the load transfers the one handle, the
    /// binder becomes the owner, and the root's whole-root exclusion posture is
    /// correct).
    ///
    /// It also returns `None` for any non-projection RHS (a fresh call result /
    /// constructor owns itself) and whenever the owner root cannot be named at
    /// this defining write — so unrecorded provenance keeps the fail-closed
    /// blanket (leak-never-double-free).
    ///
    /// The mirror of the whole-local classifier
    /// [`Builder::local_storage_is_interior_alias`]; this one keys on the FIELD
    /// TYPE so the `string`-retain class is separated from the aggregate
    /// byte-copy class, which the whole-local walk does not distinguish.
    pub(crate) fn field_projection_alias_provenance(
        &self,
        value: &HirExpr,
        binding_ty: &ResolvedTy,
    ) -> Option<ValueProvenance> {
        // Only an inline aggregate field is a ByteCopyAlias. `string` (Retained)
        // and single-pointer handles (HandleTransfer) keep `ScopeExit`
        // ownership — the exact facts codegen implements (retain vs copy vs
        // transfer), so the classification cannot admit an owner the binder
        // also releases (the load-bearing double-free risk).
        if self.classify_field_load(binding_ty) != Some(FieldLoadClass::ByteCopyAlias) {
            return None;
        }
        // The RHS must be a field projection of a live owner: `root.field` /
        // `root.N`. A fresh producer (call result / constructor) owns itself.
        let (root_binding, projection) = match &value.kind {
            HirExprKind::FieldAccess { object, field } => {
                let root = binding_ref_target(object)?;
                let ordinal = self.record_field_ordinal(object, field)?;
                (root, Projection::Field(ordinal))
            }
            HirExprKind::TupleIndex { tuple, index } => {
                let root = binding_ref_target(tuple)?;
                let ordinal = u32::try_from(*index)
                    .expect("checked tuple projection index must fit the MIR u32 carrier");
                (root, Projection::Field(ordinal))
            }
            _ => return None,
        };
        let root_place = self.binding_locals.get(&root_binding).copied()?;
        Some(ValueProvenance::projection(
            PlaceProvenance::from(root_place),
            vec![projection],
        ))
    }
    /// The three-way ownership class of a `let`-bound field LOAD, keyed on the
    /// field type and frozen to mirror exactly what codegen emits for the load.
    /// Returns `None` for a heap-free field (no drop obligation to classify).
    /// This is the authority the field-projection alias seam and its verdict-
    /// table pin read; misassigning a class here is the load-bearing double-free
    /// risk, so it keys on the same facts codegen implements.
    pub(crate) fn classify_field_load(&self, ty: &ResolvedTy) -> Option<FieldLoadClass> {
        let ty = self.subst_ty(ty);
        let owned = ValueOwnership::classify(&ty, Place::Local(0), &self.ownership_ctx());
        match owned.decision() {
            // Inline aggregate (record / tuple / array / inline-enum): the load
            // byte-copies the member with no retain, so the binder is an
            // interior alias the owner's composite still frees.
            OwnershipDecision::OwnsHeap {
                layout: LayoutClass::Product | LayoutClass::TaggedUnion,
                ..
            } => Some(FieldLoadClass::ByteCopyAlias),
            // Every other heap-owning field is a single release handle. `string`
            // is the ONE retaining leaf (codegen `hew_string_clone`s the load →
            // the binder owns a fresh `+1`); every other leaf (`Vec` / `bytes` /
            // `HashMap` / `HashSet` / `Generator` / indirect-enum node) transfers
            // its one handle to the binder.
            OwnershipDecision::OwnsHeap { .. } => {
                if matches!(ty, ResolvedTy::String) {
                    Some(FieldLoadClass::Retained)
                } else {
                    Some(FieldLoadClass::HandleTransfer)
                }
            }
            // Heap-free / borrowed / already-an-alias / unsupported: no
            // scope-exit drop obligation for the field-projection seam to record.
            _ => None,
        }
    }

    /// Publish the exact owner handoff for a non-retaining field load.
    ///
    /// A `HandleTransfer` load moves one heap handle out of a live aggregate:
    /// the new binding owns that handle, the aggregate's remaining siblings
    /// are discharged separately, and the aggregate generation must not reach
    /// a terminal recursive drop. Ending it here keeps an earlier unwind edge
    /// covered while preventing final plan reconstruction from reviving it.
    pub(crate) fn publish_handle_transfer_projection(
        &mut self,
        value: &HirExpr,
        binding_ty: &ResolvedTy,
    ) {
        if self.owned_field_projection_move_sites.last() != Some(&value.site)
            || value.intent != hew_hir::IntentKind::Consume
            || self.classify_field_load(binding_ty) != Some(FieldLoadClass::HandleTransfer)
        {
            return;
        }
        self.publish_projection_source_transfer(value);
    }

    /// Publish the aggregate source handoff when an unguarded consuming match
    /// destructures an inline (`ByteCopyAlias`) field projection.
    ///
    /// `match slot.value { .Some(v) => ... }` loads the inline
    /// enum/record field by byte copy - no retain - and then moves the active
    /// payload into the arm binder, which
    /// [`Builder::register_owned_local`] mints a scope-exit owner for
    /// (`scrutinee_payload_owner_bindings`). Without this transfer the root
    /// aggregate's generation still reaches its terminal recursive drop and
    /// releases the same heap the binder now owns - the `Arena<Vec<T>>::remove`
    /// double free. Ending the root generation here leaves the binder the sole
    /// owner; the root's remaining owned fields are discharged by the escaped-
    /// sibling field splice, so the handoff does not leak them.
    pub(crate) fn publish_consuming_match_projection(&mut self, value: &HirExpr) {
        // A PROJECTION only. `projection_root_binding` also resolves a bare
        // `BindingRef`, so without this the transfer would fire for
        // `match r { .. }` over a whole local and end the generation of a
        // scrutinee that was never projected out of - the call-carrier and
        // let-bound scrutinee releases depend on that generation surviving.
        if !matches!(
            value.kind,
            HirExprKind::FieldAccess { .. } | HirExprKind::TupleIndex { .. }
        ) {
            return;
        }
        if self.classify_field_load(&value.ty) != Some(FieldLoadClass::ByteCopyAlias) {
            return;
        }
        if !self.projected_enum_payload_is_handle_transfer(&value.ty) {
            return;
        }
        self.publish_projection_source_transfer(value);
    }

    /// True when the inline enum type `ty` carries a payload the destructure
    /// hands over as a bare heap handle - a `Vec` / `HashMap` / `HashSet` /
    /// generator / indirect-enum node leaf.
    ///
    /// This is what separates the handoff from the retain. A `string` or
    /// `bytes` payload is copy-on-write: the binder takes a balanced `+1` of its own
    /// (`FieldLoadClass::Retained`), so the parent aggregate keeps its cleanup
    /// and suppressing it would leak the parent's other fields. A handle
    /// payload has no retain, so the binder and the parent would otherwise
    /// both release it.
    ///
    /// SHORTCUT - WHY: only a DIRECT handle payload is proven here. A payload
    /// that is itself an aggregate owning a handle (`Option<Inner>` where
    /// `Inner` holds a `Vec`) classifies `ByteCopyAlias` and answers `false`,
    /// so it keeps today's competing release (#3168). Widening it needs a
    /// transfer authority for the nested aggregate, not just its root, which
    /// is more than the reported class asks for. WHEN OBSOLETE: once a nested
    /// aggregate payload carries its own handoff authority. WHAT: recurse this
    /// classification through `ByteCopyAlias` payloads and publish the
    /// transfer for the exact nested leaf rather than the root generation.
    fn projected_enum_payload_is_handle_transfer(&self, ty: &ResolvedTy) -> bool {
        let subst = self.subst_ty(ty);
        let ResolvedTy::Named { name, args, .. } = &subst else {
            return false;
        };
        let Some(layout) = crate::model::find_enum_layout(name, args, &self.enum_layouts) else {
            return false;
        };
        layout
            .variants
            .iter()
            .flat_map(|variant| variant.field_tys.iter())
            .any(|field_ty| {
                self.classify_field_load(field_ty) == Some(FieldLoadClass::HandleTransfer)
            })
    }

    fn publish_projection_source_transfer(&mut self, value: &HirExpr) {
        let Some(root_binding) = Self::projection_root_binding(value) else {
            return;
        };
        let (Some(generation), Some(place)) = (
            self.owner_generations.get(&root_binding).copied(),
            self.binding_locals.get(&root_binding).copied(),
        ) else {
            return;
        };
        self.push_instr(Instr::OwnershipEvent(
            crate::model::OwnershipEvent::Transfer {
                owner: crate::model::OwnerId {
                    binding: root_binding,
                    generation,
                },
                from: place,
                to: None,
                to_owner: None,
                to_ty: None,
            },
        ));
    }
    /// Declaration-order ordinal of `field` on the record type of `object`, from
    /// the field-order table. `None` when the object type is not a registered
    /// record (a tuple projection uses its literal index instead).
    pub(crate) fn record_field_ordinal(&self, object: &HirExpr, field: &str) -> Option<u32> {
        let ResolvedTy::Named {
            name: type_name, ..
        } = self.subst_ty(&object.ty)
        else {
            return None;
        };
        let order = self.lookup_record_field_order(type_name.as_str())?;
        let idx = order.iter().position(|(f, _)| f == field)?;
        u32::try_from(idx).ok()
    }
    /// The `(alias_local, immediate_parent_local, field_ordinal)` triples for
    /// every recorded byte-copy interior alias ([`Disposition::AliasOf`]) whose
    /// provenance is a single record/tuple field projection of a nameable parent
    /// local — the IMMEDIATE hop of a `let mid = o.mid; let leaf = mid.leaf`
    /// chain (`leaf -> (mid, 0)`, `mid -> (o, 0)`). Unlike
    /// [`Builder::alias_owner_field_binders`], which resolves each alias straight
    /// to its ultimate owner, this preserves the intermediate structure so the
    /// escaped-record sibling-discharge emitter can walk the chain and compensate
    /// the non-escaped siblings at EVERY level (the outer `c` through the root,
    /// the intermediate `mid.x` through the `mid` alias). Without it the one-hop
    /// sibling emitter (blind past a one-hop alias) leaves every deeper sibling
    /// to leak unconditionally.
    ///
    /// An entry whose parent is not a named local, or whose provenance path is
    /// not a single field step, is dropped — the emitter keeps its fail-closed
    /// leak-as-before for it (leak, never a double-free).
    pub(crate) fn alias_projection_chain(&self) -> Vec<(u32, u32, u32)> {
        let mut chain = Vec::new();
        for entry in &self.owned_locals {
            let Some(provenance) = entry.provenance.as_ref() else {
                continue;
            };
            let PlaceProvenance::Local(parent_local) = provenance.root else {
                continue;
            };
            let [Projection::Field(field)] = provenance.path.as_slice() else {
                continue;
            };
            let Some(alias_local) = self
                .binding_locals
                .get(&entry.binding)
                .and_then(|p| base_local(*p))
            else {
                continue;
            };
            chain.push((alias_local, parent_local, *field));
        }
        chain
    }
    /// The scope-exit-live owned locals as `(binding, name, ty)` tuples — the
    /// compat shape the ownership finalizers and the double-free gate consume
    /// (exit plans themselves derive from event replay, not from this list).
    /// The `Disposition::ScopeExit` filter narrows the
    /// ledger to exactly the bindings still owned at scope exit:
    /// entries retracted by a [`Builder::set_owned_local_disposition`] write
    /// (consumed, body-end-released, inner-scope-released) are excluded, which is
    /// the same set the former `owned_locals.retain(...)` physical removals left
    /// behind — so the drop-elaboration view is byte-identical to the pre-
    /// disposition ledger. The retracted entries survive in the whole ledger
    /// ([`Builder::owned_locals_ledger`]) for an end-of-pass scan to observe.
    ///
    /// SHORTCUT — WHY: the string and bytes finalizers classify a slot as a
    /// mint or a retain while lowering is still emitting events, so they
    /// cannot replay a finished stream and read this Builder ledger view of
    /// scope-exit-live owners instead. WHEN obsolete: once every owner
    /// generation publishes its mint and its end as events at lowering time
    /// (the same condition as [`Builder::owned_locals_ledger`]'s marker), the
    /// finalizers classify from replay. WHAT: derive the scope-exit-live set
    /// from Checked-MIR event replay and delete this ledger reader; nothing
    /// downstream of `seal_checked` may read it.
    pub(crate) fn owned_locals_snapshot(&self) -> Vec<(BindingId, String, ResolvedTy)> {
        self.owned_locals
            .iter()
            .filter(|entry| entry.disposition == Disposition::ScopeExit)
            .map(|entry| (entry.binding, entry.name.clone(), entry.ty.clone()))
            .collect()
    }
    /// Every ledger entry that is or was an owner generation — all dispositions
    /// except `AliasOf`, deduplicated by binding in registration order. The
    /// pre-seal neutralize/release passes read this view to find the slots a
    /// physical transfer may null or a proven last read may release. It is not
    /// a plan source: exit plans derive from the minted owners' event replay,
    /// which is why a `BodyEndReleased` / `ScopeReleased` / `ConsumedAt`
    /// disposition keeps its entry here (the generation was live before that
    /// release, and a call before it can still unwind). Only `AliasOf` is
    /// never an owner.
    ///
    /// SHORTCUT — WHY: the pre-seal passes run while lowering is still
    /// emitting events, so they cannot replay the finished stream and read
    /// this Builder ledger view instead. WHEN obsolete: once reassignment and
    /// transfer publish their generation-end events at lowering time (the
    /// same condition as the `prepare_body_transfers` replay marker). WHAT:
    /// derive the slot view from Checked-MIR event replay and delete this
    /// ledger reader.
    pub(crate) fn owned_locals_owner_generations(&self) -> Vec<(BindingId, String, ResolvedTy)> {
        let mut seen = HashSet::new();
        self.owned_locals
            .iter()
            .filter(|entry| entry.disposition != Disposition::AliasOf)
            .filter(|entry| seen.insert(entry.binding))
            .map(|entry| (entry.binding, entry.name.clone(), entry.ty.clone()))
            .collect()
    }

    /// The WHOLE per-function owned-locals ledger — every entry regardless of
    /// disposition, in registration order — including bindings retracted off the
    /// scope-exit-live set by a [`Builder::set_owned_local_disposition`] write.
    ///
    /// An end-of-pass scan reads this (rather than [`owned_locals_snapshot`],
    /// the scope-exit-live view) when it must observe a binding whose release
    /// was handled mid-lowering. Under the former physical-removal model that
    /// binding was gone from the ledger by scan time — the retraction-invisible
    /// class behind the double-free and #2375. The disposition write keeps the
    /// entry observable while excluding it from the live drop set.
    ///
    /// [`owned_locals_snapshot`]: Builder::owned_locals_snapshot
    #[allow(
        dead_code,
        reason = "whole-ledger scan option consumed by the provenance-aware \
                  provers and end-of-pass scans in later drop-elaboration stages"
    )]
    pub(crate) fn owned_locals_ledger(&self) -> &[OwnedLocalEntry] {
        &self.owned_locals
    }
    /// Disposition a binding OFF the scope-exit-live set — the retraction-to-
    /// disposition replacement for `owned_locals.retain(|e| e.binding != b)`.
    /// The entry stays in the ledger (an end-of-pass whole-ledger scan can still
    /// observe it via [`Builder::owned_locals_ledger`]) but leaves the
    /// scope-exit view [`Builder::owned_locals_snapshot`] projects, so the
    /// ownership finalizers no longer treat it as a scope-exit owner. Sets
    /// every entry matching `binding`,
    /// mirroring `retain`'s remove-all semantics (at most one exists in
    /// practice).
    pub(crate) fn set_owned_local_disposition(
        &mut self,
        binding: BindingId,
        disposition: Disposition,
    ) {
        let demotes_live_owner = disposition == Disposition::AliasOf
            && self
                .owned_locals
                .iter()
                .any(|entry| entry.binding == binding && entry.disposition != Disposition::AliasOf);
        if demotes_live_owner {
            // Pattern lowering may initially mint a payload binder before the
            // selected carrier topology proves that it is only an interior
            // alias. End that provisional generation at this exact MIR point;
            // post-Checked elaboration must not consult the mutable ledger to
            // rediscover the demotion.
            if let (Some(generation), Some(place)) = (
                self.owner_generations.get(&binding).copied(),
                self.binding_locals.get(&binding).copied(),
            ) {
                self.push_instr(Instr::OwnershipEvent(
                    crate::model::OwnershipEvent::DemoteToAlias {
                        owner: crate::model::OwnerId {
                            binding,
                            generation,
                        },
                        place,
                    },
                ));
            }
        }
        for entry in &mut self.owned_locals {
            if entry.binding == binding {
                entry.disposition = disposition;
            }
        }
    }

    /// Build the explicit end-of-generation operation for an inline release.
    /// This is usable by post-sealing rewrites, which must insert the operation
    /// into the rewritten block rather than append through the builder cursor.
    pub(crate) fn owned_local_release_event(&self, binding: BindingId) -> Option<Instr> {
        Some(Instr::OwnershipEvent(
            crate::model::OwnershipEvent::Release {
                owner: crate::model::OwnerId {
                    binding,
                    generation: *self.owner_generations.get(&binding)?,
                },
                place: *self.binding_locals.get(&binding)?,
            },
        ))
    }

    /// End one owner generation at the exact current MIR program point.
    pub(crate) fn release_owned_local(&mut self, binding: BindingId, disposition: Disposition) {
        if let Some(event) = self.owned_local_release_event(binding) {
            self.push_instr(event);
        }
        self.set_owned_local_disposition(binding, disposition);
    }

    /// End one generation at the exact physical place selected by a
    /// path-sensitive body-end handoff. The lexical binding table may still
    /// name the original slot, but Checked MIR must pair the destructor with
    /// the terminal carrier that actually holds the value.
    pub(crate) fn release_owned_local_from(
        &mut self,
        binding: BindingId,
        place: Place,
        disposition: Disposition,
    ) {
        if let Some(generation) = self.owner_generations.get(&binding).copied() {
            self.push_instr(Instr::OwnershipEvent(
                crate::model::OwnershipEvent::Release {
                    owner: crate::model::OwnerId {
                        binding,
                        generation,
                    },
                    place,
                },
            ));
        }
        self.set_owned_local_disposition(binding, disposition);
    }

    /// Preserve the source generation across an explicit retain + bit-copy.
    /// Generic MIR `Move` relocates a sole owner, but after a retain the copied
    /// destination is a distinct share and the original owner remains in its
    /// source slot. Publish that exact post-copy location in MIR so cleanup
    /// derivation does not mistake the original generation for the returned
    /// share.
    pub(crate) fn restore_owner_after_retained_share(&mut self, source: Place, copied_to: Place) {
        let mut owners = self.owned_locals.iter().filter_map(|entry| {
            (entry.disposition != Disposition::AliasOf
                && self.binding_locals.get(&entry.binding) == Some(&source))
            .then(|| {
                self.owner_generations
                    .get(&entry.binding)
                    .copied()
                    .map(|generation| crate::model::OwnerId {
                        binding: entry.binding,
                        generation,
                    })
            })
            .flatten()
        });
        let Some(owner) = owners.next() else {
            return;
        };
        if owners.next().is_some() {
            return;
        }
        self.push_instr(Instr::OwnershipEvent(
            crate::model::OwnershipEvent::Relocate {
                owner,
                from: copied_to,
                to: source,
            },
        ));
    }
    /// Retract a binding to [`Disposition::ConsumedAt`], REQUIRING its discharge
    /// authority (`transferee` + `site`) by signature — the close-by-construction
    /// consume-write chokepoint (U221/U229). A consume retraction cannot be
    /// spelled without naming who took ownership and why, so the fact is never
    /// erased at the retraction seam. Every production consume routes through
    /// here (via [`Builder::mark_binding_moved`]).
    /// Record that `binding`'s ownership obligation has been discharged by a
    /// TRANSFER, at lowering time.
    ///
    /// This is the lowest seam the transfer passes through, so it is where the
    /// affine refcounted-handle (`Rc` / `Weak`) and user `#[resource]` release
    /// flag is written: the `ConsumedAt` disposition and the runtime transfer
    /// record are set together, by one statement, and a caller cannot record
    /// one without the other. `mark_binding_moved` funnels here, and so do the
    /// synthetic produced-value owner handoffs that bypass it.
    ///
    /// WHY a runtime flag and not the disposition alone. A flagged binding is
    /// deliberately KEPT in `owned_locals` across its consume so the guard can
    /// decide per control-flow path, so the disposition below does not retire
    /// its drop — only the flag can. The dataflow `Consumed` state suffices for
    /// a consume that DOMINATES the exit (`filter_drops_by_state` excludes a
    /// `Consumed` binding), but a conditional one meets `Live` at the join and
    /// yields `MaybeConsumed`, which the same filter admits as live. The drop
    /// then fires on the path that already transferred the handle, guard still
    /// reading 0 — `match flag { true => { let v: Vec<Rc<T>> = [shared]; .. }
    /// false => .. }` aborted with `Rc double-free`.
    ///
    /// A no-op for every unflagged binding.
    pub(crate) fn set_owned_local_consumed(
        &mut self,
        binding: BindingId,
        transferee: Option<Place>,
        site: DischargeSite,
    ) {
        if let Some(flag) = self.affine_release_flags.get(&binding).copied() {
            self.instructions.push(Instr::ConstI64 {
                dest: flag,
                value: 1,
            });
        }
        if let (Some(place), Some(generation)) = (
            self.binding_locals.get(&binding).copied(),
            self.live_owner_generation(binding),
        ) {
            self.push_instr(Instr::OwnershipEvent(
                crate::model::OwnershipEvent::Transfer {
                    owner: crate::model::OwnerId {
                        binding,
                        generation,
                    },
                    from: place,
                    to: transferee,
                    to_owner: None,
                    to_ty: None,
                },
            ));
        }
        self.set_owned_local_consumed_post_lowering(binding, transferee, site);
    }

    /// The binding's current owner generation, unless the ledger already
    /// demoted it to a byte-copy alias: `DemoteToAlias` ended that generation,
    /// so no later transfer may name it. `owner_generations` is a latest-mint
    /// cursor, not a liveness view.
    pub(crate) fn live_owner_generation(&self, binding: BindingId) -> Option<u32> {
        let is_alias = self
            .owned_locals
            .iter()
            .any(|entry| entry.binding == binding && entry.disposition == Disposition::AliasOf);
        (!is_alias)
            .then(|| self.owner_generations.get(&binding).copied())
            .flatten()
    }

    /// Consume an owner from the explicit instruction-position place rather
    /// than its original lexical slot. Aggregate construction moves a child
    /// into a projected field before the parent owner is minted; the event
    /// must therefore name that field, not the stale Builder binding place.
    fn set_owned_local_consumed_from(
        &mut self,
        binding: BindingId,
        from: Place,
        transferee: Option<Place>,
        site: DischargeSite,
    ) {
        if let Some(flag) = self.affine_release_flags.get(&binding).copied() {
            self.instructions.push(Instr::ConstI64 {
                dest: flag,
                value: 1,
            });
        }
        if let Some(generation) = self.live_owner_generation(binding) {
            self.push_instr(Instr::OwnershipEvent(
                crate::model::OwnershipEvent::Transfer {
                    owner: crate::model::OwnerId {
                        binding,
                        generation,
                    },
                    from,
                    to: transferee,
                    to_owner: None,
                    to_ty: None,
                },
            ));
        }
        self.set_owned_local_consumed_post_lowering(binding, transferee, site);
    }

    /// Disposition-only form of [`Self::set_owned_local_consumed`], for a
    /// handoff discovered AFTER block building has finished.
    ///
    /// The finalized-MIR `string` / `bytes` retain-site derivations run over
    /// `raw.blocks` once the builder's instruction buffer is closed, so a
    /// `push_instr` from there would append to a stream nothing emits. Those
    /// passes are also the wrong authority for the flag: they resolve
    /// CoW-carrier handoffs, whose release accounting is the retain-site
    /// derivation's, never an affine handle's. Recording the disposition alone
    /// is therefore both necessary and sufficient there.
    ///
    /// Every LOWERING-time consume must use [`Self::set_owned_local_consumed`]
    /// instead, so the transfer record cannot go missing.
    pub(crate) fn set_owned_local_consumed_post_lowering(
        &mut self,
        binding: BindingId,
        transferee: Option<Place>,
        site: DischargeSite,
    ) {
        self.set_owned_local_disposition(binding, Disposition::ConsumedAt { transferee, site });
    }
    pub(crate) fn owned_string_record_field_kinds_for_key(
        &self,
        key: &str,
    ) -> Option<Vec<crate::state_clone::StateFieldCloneKind>> {
        let fields = self.lookup_record_field_order(key)?;
        let field_tys: Vec<ResolvedTy> = fields.iter().map(|(_, ty)| ty.clone()).collect();
        let record_layouts = self.record_layouts_for_classification();
        crate::state_clone::classify_owned_string_record_fields(
            &field_tys,
            &record_layouts,
            &self.enum_layouts,
            &self.lifecycle_registry,
        )
        .ok()
        .flatten()
    }
    /// Unified owned-aggregate-record value-class authority (RC-4 / RC-6 / G12).
    ///
    /// Returns `Some(kinds)` iff the record named by `key`:
    ///   1. has a registered layout (`record_field_orders`),
    ///   2. classifies cleanly under the SAME resource-aware field classifier
    ///      actor state uses (`classify_actor_state_fields_with_lifecycle_registry`),
    ///      AND
    ///   3. every field's kind is admissible to the in-place record value-class
    ///      (`StateFieldCloneKind::supports_value_class_drop_spine`) — so codegen
    ///      can synthesize BOTH the clone and drop side of the
    ///      `__hew_record_{clone,drop}_inplace_<R>` thunk for every field. A
    ///      field carrying an `OpaqueHandle` (no dup helper) fails closed HERE at
    ///      the W3.029 value-class gate, not late at codegen clone-synthesis, AND
    ///   4. carries at least one non-`BitCopy` owned field (otherwise it is a
    ///      plain `BitCopy` aggregate that needs no owned-value drop and is
    ///      classified by `ValueClass::of_ty` upstream).
    ///
    /// This is the single admission gate for an owned record passed/returned by
    /// value: `string` fields (RC-6), `bytes` fields (RC-4), `Vec`/`HashMap`/
    /// `HashSet` fields (G12), and nested owned record/enum fields all classify
    /// here. A record carrying a field the classifier rejects (an unresolved
    /// nested type) — OR a field that classifies but whose CLONE direction has no
    /// helper (`OpaqueHandle` such as `json.Value`) — returns `None` and stays
    /// fail-closed at the W3.029 reject. The value-class-admissible gate (step 3)
    /// is the load-bearing addition: without it `Pair<json.Value, i64>` admitted
    /// as `CowValue` here, was seeded for `RecordInPlace`, and failed closed only
    /// LATE at codegen clone-synthesis. Codegen now never sees a record whose
    /// clone/drop thunk it cannot emit. (`IoHandle` fields are admitted: they
    /// drop field-wise via the resource-drop path, never via `RecordInPlace`, so
    /// no record clone thunk is synthesised for them.)
    ///
    /// Generalizes `owned_string_record_field_kinds_for_key` (String-only) to
    /// the full owned-field surface. Passing `self.enum_layouts` lets a record
    /// with an `Option`/`Result`/user-enum field classify as `Enum` rather than
    /// failing closed (the enum drop thunk already exists, W5.006/W5.020).
    pub(crate) fn owned_aggregate_record_field_kinds_for_key(
        &self,
        key: &str,
    ) -> Result<
        Option<Vec<crate::state_clone::StateFieldCloneKind>>,
        crate::state_clone::ClassificationError,
    > {
        let Some(fields) = self.lookup_record_field_order(key) else {
            return Ok(None);
        };
        if fields.is_empty() {
            return Ok(None);
        }
        // Normalize machine-typed fields before classification: a generic
        // machine instantiation in a record field (e.g. `m: Lifecycle<i64>`)
        // arrives as `Named { name: "Lifecycle", args: [I64] }`.  The machine
        // view is registered under the bare name "Lifecycle" (never mangled),
        // so `lookup_enum_layout` misses the mangled probe and falls through to
        // `classify_user_record`, which finds no RecordLayout for a machine →
        // MissingRecordLayout → UnsupportedUserRecordValueClass.  Strip the
        // args when the named type is a known machine (all-i64 args, same
        // condition as the actor-state normalize pass at lower.rs ~1513) so the
        // bare-name machine view is found.  Any other instantiation keeps its
        // args and fails closed — matching the Move-type refusal such programs
        // already hit at codegen.
        let field_tys: Vec<ResolvedTy> = fields
            .iter()
            .map(|(_, ty)| self.normalize_machine_field_ty(ty))
            .collect();
        let record_layouts = self.record_layouts_for_classification();
        let kinds = crate::state_clone::classify_actor_state_fields_with_lifecycle_registry(
            &field_tys,
            &record_layouts,
            &self.enum_layouts,
            &self.opaque_handle_names,
            &self.lifecycle_registry,
        )?;
        // Fail closed at the value-class gate, not late at codegen. An admitted
        // owned-aggregate record is seeded for `DropKind::RecordInPlace`, which
        // drives codegen to synthesise BOTH the clone and the drop body. A field
        // whose kind has no clone-side helper (`OpaqueHandle` — e.g.
        // `json.Value`) must NOT admit: codegen cannot synthesise the
        // `__hew_record_clone_inplace_<R>` thunk for it and would fail closed
        // LATE (llvm.rs ~6254), leaving a fragile MIR-admits/codegen-refuses
        // seam. Reject here so `Pair<json.Value, i64>` stays at the W3.029
        // reject (`UnsupportedUserRecordValueClass`) BEFORE codegen, while every
        // supported owned shape (String, Bytes, Vec/HashMap/HashSet, nested
        // UserRecord/Enum, AND IoHandle handles dropped field-wise) still
        // admits.
        if !kinds
            .iter()
            .all(crate::state_clone::StateFieldCloneKind::supports_value_class_drop_spine)
        {
            return Ok(None);
        }
        let has_owned_field = kinds
            .iter()
            .any(|k| !matches!(k, crate::state_clone::StateFieldCloneKind::BitCopy { .. }));
        Ok(has_owned_field.then_some(kinds))
    }
    /// Narrow correction for generic records whose inline generic-enum field
    /// owns heap even though the HIR value-class marker still says `BitCopy`.
    ///
    /// Copy classification and drop seeding both call this predicate. Keeping
    /// those decisions coupled prevents a shallow `BorrowRead` copy from
    /// aliasing an enum payload that the newly-admitted `RecordInPlace` drop
    /// would later release.
    pub(crate) fn record_with_ready_inline_enum_owned_field(&self, ty: &ResolvedTy) -> bool {
        let Some(record_key) = user_record_layout_key(ty) else {
            return false;
        };
        let Some(fields) = self.lookup_record_field_order(&record_key) else {
            return false;
        };
        let record_layouts = self.record_layouts_for_classification();
        let resource_record_names = self.resource_record_names_for_drop_readiness();
        if !crate::state_clone::record_with_inline_enum_drop_is_ready(
            ty,
            &record_layouts,
            &self.enum_layouts,
            &self.opaque_handle_names,
            &self.lifecycle_registry,
            &resource_record_names,
        )
        .unwrap_or(false)
        {
            return false;
        }
        fields.iter().any(|(_, field_ty)| {
            let field_ty = self.normalize_machine_field_ty(field_ty);
            crate::state_clone::inline_enum_overwrite_drop_is_ready(
                &field_ty,
                &record_layouts,
                &self.enum_layouts,
                &self.opaque_handle_names,
                &self.lifecycle_registry,
                &resource_record_names,
            )
            .unwrap_or(false)
                && crate::model::ty_owns_heap_mir(
                    &field_ty,
                    &self.record_field_orders,
                    &self.enum_layouts,
                )
        })
    }
    fn resource_record_names_for_drop_readiness(&self) -> Vec<String> {
        self.record_field_orders
            .keys()
            .filter(|name| {
                self.type_classes
                    .get(name.as_str())
                    .is_some_and(|(marker, _)| matches!(marker, ResourceMarker::Resource))
            })
            .cloned()
            .collect()
    }
    /// True when `ty` is a user record admitted by the unified
    /// owned-aggregate-record authority. The single predicate the `decide`
    /// value-class gate and the drop-elaboration allow-set derivation share so
    /// they can never disagree on which records are owned-by-value.
    ///
    /// Keyed via `user_record_layout_key`, which resolves BOTH a bare-name
    /// monomorphic record (`Wrapper`) AND a generic INSTANTIATION mangled by
    /// `hew_hir::mangle` (`Pair<i64, string>` → `Pair$$i64$string`). The
    /// per-instantiation layout the producer registers under the mangled key
    /// (`module.record_layouts`, lower.rs ~961) is resolved here, its
    /// SUBSTITUTED field types classified, and — when every field classifies —
    /// the instantiation is admitted as `CowValue` so codegen can synthesise
    /// the matching `__hew_record_{clone,drop}_inplace_<mangled>` thunk. The
    /// drop-plan validator (`expected_drop_kind_for_validation`,
    /// `RecordInPlace` arm) re-derives against the SAME key so the elaborated
    /// `RecordInPlace` drop on an args-bearing `ElabDrop::ty` is accepted, and
    /// the codegen `record_inplace_drop_name` mangles identically so the helper
    /// name agrees end-to-end.
    ///
    /// Fail-closed default is preserved: a record (bare or generic) whose
    /// mangled layout is absent, OR whose substituted fields do not all
    /// classify, returns `false` and stays at the W3.029 reject — codegen never
    /// observes a record whose drop thunk it cannot emit. The W3.029 gate is
    /// added-around, never relaxed: reverting this key change restores the
    /// reject for `Pair$$i64$string`.
    pub(crate) fn is_owned_aggregate_record_ty(&self, ty: &ResolvedTy) -> bool {
        user_record_layout_key(ty).is_some_and(|key| {
            self.owned_aggregate_record_field_kinds_for_key(&key)
                .is_ok_and(|kinds| kinds.is_some())
        })
    }
    pub(crate) fn owned_string_record_init_key_for_let(
        &self,
        binding_ty: &ResolvedTy,
        value: &HirExpr,
    ) -> Option<String> {
        let HirExprKind::StructInit { base, .. } = &value.kind else {
            return None;
        };
        if base.is_some() {
            return None;
        }
        let binding_key = monomorphic_user_record_key(binding_ty)?;
        let value_ty = self.subst_ty(&value.ty);
        if monomorphic_user_record_key(&value_ty).as_deref() != Some(binding_key.as_str()) {
            return None;
        }
        self.owned_string_record_field_kinds_for_key(&binding_key)
            .map(|_| binding_key)
    }
    /// Look up the field-order entry for a record type by key.
    ///
    /// Record-field tables are keyed by the exact checker-resolved nominal
    /// identity (or its exact generic layout mangle). A leaf-name retry would
    /// let a same-leaf declaration from another module supply this record's
    /// field order, so a miss remains a miss.
    pub(crate) fn lookup_record_field_order(
        &self,
        type_name: &str,
    ) -> Option<&Vec<(String, ResolvedTy)>> {
        self.record_field_orders.get(type_name)
    }
    pub(crate) fn mark_owned_string_record_field_site(&mut self, object: &HirExpr) {
        let HirExprKind::BindingRef {
            resolved: ResolvedRef::Binding(id),
            ..
        } = &object.kind
        else {
            return;
        };
        if !self.owned_string_record_bindings.contains(id) {
            return;
        }
        let object_ty = self.subst_ty(&object.ty);
        if monomorphic_user_record_key(&object_ty).is_some() {
            self.owned_string_record_value_sites.insert(object.site);
        }
    }
    pub(crate) fn mark_pattern_bindings_unproven(
        &mut self,
        bindings: &[hew_hir::HirMatchArmBinding],
    ) {
        for binding in bindings {
            if ty_is_closure_pair(&self.subst_ty(&binding.ty)) {
                self.closure_pair_env_may_be_nonnull.insert(binding.binding);
            }
        }
    }
    pub(crate) fn mark_nested_pattern_bindings_unproven(
        &mut self,
        predicates: &[hew_hir::HirPayloadVariantPredicate],
    ) {
        for predicate in predicates {
            self.mark_pattern_bindings_unproven(&predicate.bindings);
            self.mark_nested_pattern_bindings_unproven(&predicate.nested);
        }
    }
    pub(crate) fn mark_match_predicate_binding_unproven(
        &mut self,
        predicate: &hew_hir::HirMatchArmPredicate,
    ) {
        if let hew_hir::HirMatchArmPredicate::Binding { binding_id, ty, .. } = predicate {
            if ty_is_closure_pair(&self.subst_ty(ty)) {
                self.closure_pair_env_may_be_nonnull.insert(*binding_id);
            }
        }
    }
    pub(crate) fn collect_vec_owned_element_keys_from_stmt(&mut self, stmt: &hew_hir::HirStmt) {
        match &stmt.kind {
            HirStmtKind::Let(binding, value) => {
                let binding_ty = self.subst_ty(&binding.ty);
                self.harvest_vec_owned_element_key(&binding_ty);
                if let Some(v) = value {
                    // Closure-env provenance: a fn/closure-typed binding
                    // whose RHS may carry a heap env word must remain
                    // fail-closed at any later generator crossing.
                    if ty_is_closure_pair(&binding_ty) && self.closure_rhs_may_carry_env(v) {
                        self.closure_pair_env_may_be_nonnull.insert(binding.id);
                    }
                    // #2418 — a DIRECT consume-rebind initializer
                    // (`let y = xs;`) is the one consume shape the
                    // collection drop-flag covers; record the consume
                    // WITHOUT the non-rebind mark the general walk would
                    // apply, replicating the walk's other effects on a
                    // childless `BindingRef` (the type-key harvest).
                    if let HirExprKind::BindingRef {
                        resolved: ResolvedRef::Binding(id),
                        ..
                    } = &v.kind
                    {
                        if v.intent == IntentKind::Consume {
                            self.prepass_consumed_bindings.insert(*id);
                            let vty = self.subst_ty(&v.ty);
                            self.harvest_vec_owned_element_key(&vty);
                            return;
                        }
                    }
                    self.collect_vec_owned_element_keys_from_expr(v);
                }
            }
            HirStmtKind::LetElse {
                scrutinee,
                bindings,
                success_prelude,
                payload_variant_predicates,
                else_body,
                ..
            } => {
                self.collect_vec_owned_element_keys_from_expr(scrutinee);
                self.mark_pattern_bindings_unproven(bindings);
                self.mark_nested_pattern_bindings_unproven(payload_variant_predicates);
                for prelude_stmt in success_prelude {
                    self.collect_vec_owned_element_keys_from_stmt(prelude_stmt);
                }
                self.collect_vec_owned_element_keys_from_block(else_body);
            }
            HirStmtKind::Assign { target, value } => {
                // #2301 -- record a reassigned `var` target so a consumed
                // binding that is also overwritten gets an overwrite-release
                // drop-flag (the intersection keeps the common no-consume
                // overwrite on the zero-churn static gate).
                if let HirExprKind::BindingRef {
                    resolved: ResolvedRef::Binding(id),
                    ..
                } = &target.kind
                {
                    self.prepass_reassigned_bindings.insert(*id);
                    // Closure-env provenance for reassignments (`var g =
                    // triple; g = producer();`). The pre-pass runs before
                    // any call site lowers, so a back-edge reassignment
                    // taints the binding for the whole function.
                    if ty_is_closure_pair(&self.subst_ty(&target.ty))
                        && self.closure_rhs_may_carry_env(value)
                    {
                        self.closure_pair_env_may_be_nonnull.insert(*id);
                    }
                }
                self.collect_vec_owned_element_keys_from_expr(target);
                self.collect_vec_owned_element_keys_from_expr(value);
            }
            HirStmtKind::Expr(e) | HirStmtKind::Return(Some(e)) => {
                self.collect_vec_owned_element_keys_from_expr(e);
            }
            HirStmtKind::Defer { body, .. } => {
                self.collect_vec_owned_element_keys_from_expr(body);
            }
            HirStmtKind::Return(None) => {}
        }
    }
    pub(crate) fn collect_vec_owned_element_keys_from_block(&mut self, block: &HirBlock) {
        for stmt in &block.statements {
            self.collect_vec_owned_element_keys_from_stmt(stmt);
        }
        if let Some(tail) = &block.tail {
            self.collect_vec_owned_element_keys_from_expr(tail);
        }
    }
    /// #2301 -- record a genuine move-out consume (`intent=Consume` on a
    /// `BindingRef`) seen by the pre-pass walk. A binding that is BOTH
    /// consumed and reassigned (see the `Assign` arm in
    /// `collect_vec_owned_element_keys_from_block`) gets a path-sensitive
    /// overwrite-release drop-flag at its `let`.
    ///
    /// #2418 -- every consume reached through the general expression walk is
    /// a NON-REBIND shape (the direct `let y = xs;` initializer is
    /// intercepted in the block walker and never recurses here), so it also
    /// disqualifies the binding from the collection drop-flag (see
    /// `prepass_nonrebind_consumed`).
    pub(crate) fn prepass_note_nonrebind_consume(&mut self, expr: &HirExpr) {
        if expr.intent == IntentKind::Consume {
            if let HirExprKind::BindingRef {
                resolved: ResolvedRef::Binding(id),
                ..
            } = &expr.kind
            {
                self.prepass_consumed_bindings.insert(*id);
                self.prepass_nonrebind_consumed.insert(*id);
            }
        }
    }
    fn prepass_note_actor_message_args<'a>(&mut self, args: impl IntoIterator<Item = &'a HirExpr>) {
        for arg in args {
            if !matches!(self.subst_ty(&arg.ty), ResolvedTy::Bytes) {
                continue;
            }
            if let HirExprKind::BindingRef {
                resolved: ResolvedRef::Binding(binding),
                ..
            } = &arg.kind
            {
                self.prepass_actor_message_transfer_bindings
                    .insert(*binding);
            }
        }
    }
    /// Harvest owned-Vec element keys from an expression's type and recurse into
    /// the structural child expressions that may carry a `Vec<owned>` value.
    /// Every visited expr contributes its own `.ty` (so a `Vec<Header>`
    /// `BindingRef`/`Call`/`StructInit` receiver is caught); the recursion
    /// reaches nested blocks (if/match/scope/loop bodies) where an owned-Vec
    /// could be constructed or used.
    #[allow(
        clippy::too_many_lines,
        reason = "one structural HIR walk must keep every same-builder child expression \
                  visible to both ownership-key harvesting and closure-env provenance"
    )]
    pub(crate) fn collect_vec_owned_element_keys_from_expr(&mut self, expr: &HirExpr) {
        self.prepass_note_nonrebind_consume(expr);
        let ty = self.subst_ty(&expr.ty);
        self.harvest_vec_owned_element_key(&ty);
        if let HirExprKind::BindingRef {
            resolved: ResolvedRef::Binding(id),
            ..
        } = &expr.kind
        {
            self.prepass_binding_ref_uses.insert(*id);
        }
        match &expr.kind {
            HirExprKind::Binary { left, right, .. }
            | HirExprKind::IdentityCompare { left, right } => {
                self.collect_vec_owned_element_keys_from_expr(left);
                self.collect_vec_owned_element_keys_from_expr(right);
            }
            HirExprKind::RcIntrinsic {
                receiver, value, ..
            } => {
                if let Some(receiver) = receiver {
                    self.collect_vec_owned_element_keys_from_expr(receiver);
                }
                if let Some(value) = value {
                    self.collect_vec_owned_element_keys_from_expr(value);
                }
            }
            HirExprKind::Unary { operand, .. } => {
                self.collect_vec_owned_element_keys_from_expr(operand);
            }
            HirExprKind::NumericCast { value, .. }
            | HirExprKind::SaturatingWidthCast { value, .. }
            | HirExprKind::TryWidthCast { value, .. }
            | HirExprKind::CoerceToDynTrait { value, .. }
            | HirExprKind::WireCodec { operand: value, .. }
            | HirExprKind::RecordCloneCall { src: value, .. }
            | HirExprKind::CancellationTokenIsCancelled { receiver: value }
            | HirExprKind::GeneratorNext {
                receiver: value, ..
            }
            | HirExprKind::MachineStateName {
                receiver: value, ..
            }
            | HirExprKind::AwaitRestart { child: value }
            | HirExprKind::ConnAwaitRead { conn: value, .. }
            | HirExprKind::ListenerAwaitAccept {
                listener: value, ..
            }
            | HirExprKind::ChannelRecvAwait {
                receiver: value, ..
            }
            | HirExprKind::StreamRecvAwait { stream: value, .. } => {
                self.collect_vec_owned_element_keys_from_expr(value);
            }
            HirExprKind::TupleLiteral { elements } => {
                for e in elements {
                    self.collect_vec_owned_element_keys_from_expr(e);
                }
            }
            HirExprKind::Call { callee, args, .. } => {
                self.collect_vec_owned_element_keys_from_expr(callee);
                for a in args {
                    self.collect_vec_owned_element_keys_from_expr(a);
                }
            }
            HirExprKind::Spawn { args, .. } => {
                for (_, arg) in args {
                    self.collect_vec_owned_element_keys_from_expr(arg);
                }
            }
            HirExprKind::ActorSend { receiver, args, .. }
            | HirExprKind::ActorAsk { receiver, args, .. }
            | HirExprKind::ActorGenStream { receiver, args, .. } => {
                self.prepass_note_actor_message_args(args);
                self.collect_vec_owned_element_keys_from_expr(receiver);
                for a in args {
                    self.collect_vec_owned_element_keys_from_expr(a);
                }
            }
            HirExprKind::SpawnedCall {
                callee: receiver,
                args,
                ..
            }
            | HirExprKind::ResolvedImplCall { receiver, args, .. }
            | HirExprKind::VarSelfMethodCall { receiver, args, .. }
            | HirExprKind::CallDynMethod { receiver, args, .. }
            | HirExprKind::CallTraitMethodStatic { receiver, args, .. } => {
                self.collect_vec_owned_element_keys_from_expr(receiver);
                for a in args {
                    self.collect_vec_owned_element_keys_from_expr(a);
                }
            }
            HirExprKind::RemoteActorAsk {
                receiver,
                msg,
                timeout_ms,
                ..
            } => {
                self.prepass_note_actor_message_args(std::iter::once(msg.as_ref()));
                self.collect_vec_owned_element_keys_from_expr(receiver);
                self.collect_vec_owned_element_keys_from_expr(msg);
                self.collect_vec_owned_element_keys_from_expr(timeout_ms);
            }
            HirExprKind::StructInit { fields, base, .. } => {
                for (_, f) in fields {
                    self.collect_vec_owned_element_keys_from_expr(f);
                }
                if let Some(b) = base {
                    self.collect_vec_owned_element_keys_from_expr(b);
                }
            }
            HirExprKind::FieldAccess { object, .. }
            | HirExprKind::TupleIndex { tuple: object, .. } => {
                self.collect_vec_owned_element_keys_from_expr(object);
            }
            HirExprKind::Index { container, index } => {
                self.collect_vec_owned_element_keys_from_expr(container);
                self.collect_vec_owned_element_keys_from_expr(index);
            }
            HirExprKind::If {
                condition,
                then_expr,
                else_expr,
            } => {
                self.collect_vec_owned_element_keys_from_expr(condition);
                self.collect_vec_owned_element_keys_from_expr(then_expr);
                if let Some(eb) = else_expr {
                    self.collect_vec_owned_element_keys_from_expr(eb);
                }
            }
            HirExprKind::Block(body)
            | HirExprKind::Scope { body }
            | HirExprKind::Loop { body, .. } => {
                self.collect_vec_owned_element_keys_from_block(body);
            }
            HirExprKind::ScopeDeadline { duration, body } => {
                self.collect_vec_owned_element_keys_from_expr(duration);
                self.collect_vec_owned_element_keys_from_block(body);
            }
            HirExprKind::Match { scrutinee, arms } => {
                self.collect_vec_owned_element_keys_from_expr(scrutinee);
                for arm in arms {
                    self.mark_match_predicate_binding_unproven(&arm.predicate);
                    self.mark_pattern_bindings_unproven(&arm.bindings);
                    self.mark_nested_pattern_bindings_unproven(&arm.payload_variant_predicates);
                    if let Some(guard) = &arm.guard {
                        self.collect_vec_owned_element_keys_from_expr(guard);
                    }
                    self.collect_vec_owned_element_keys_from_expr(&arm.body);
                }
            }
            HirExprKind::WhileLet {
                scrutinee,
                bindings,
                payload_variant_predicates,
                body,
                ..
            } => {
                self.collect_vec_owned_element_keys_from_expr(scrutinee);
                self.mark_pattern_bindings_unproven(bindings);
                self.mark_nested_pattern_bindings_unproven(payload_variant_predicates);
                self.collect_vec_owned_element_keys_from_block(body);
            }
            HirExprKind::IfLet {
                scrutinee,
                bindings,
                payload_variant_predicates,
                body,
                else_body,
                ..
            } => {
                self.collect_vec_owned_element_keys_from_expr(scrutinee);
                self.mark_pattern_bindings_unproven(bindings);
                self.mark_nested_pattern_bindings_unproven(payload_variant_predicates);
                self.collect_vec_owned_element_keys_from_block(body);
                if let Some(else_body) = else_body {
                    self.collect_vec_owned_element_keys_from_block(else_body);
                }
            }
            HirExprKind::Select(select) => {
                for arm in &select.arms {
                    if let Some(binding) = arm.binding_id {
                        // The select result type is encoded by the arm source,
                        // not repeated on the binding. Mark it unproven; later
                        // consumers consult this set only for fn-typed values.
                        self.closure_pair_env_may_be_nonnull.insert(binding);
                    }
                    match &arm.kind {
                        hew_hir::HirSelectArmKind::StreamNext { stream } => {
                            self.collect_vec_owned_element_keys_from_expr(stream);
                        }
                        hew_hir::HirSelectArmKind::ActorAsk { actor, args, .. } => {
                            self.prepass_note_actor_message_args(args);
                            self.collect_vec_owned_element_keys_from_expr(actor);
                            for arg in args {
                                self.collect_vec_owned_element_keys_from_expr(arg);
                            }
                        }
                        hew_hir::HirSelectArmKind::TaskAwait { task } => {
                            self.collect_vec_owned_element_keys_from_expr(task);
                        }
                        hew_hir::HirSelectArmKind::ChannelRecv { receiver } => {
                            self.collect_vec_owned_element_keys_from_expr(receiver);
                        }
                        hew_hir::HirSelectArmKind::AfterTimer { duration } => {
                            self.collect_vec_owned_element_keys_from_expr(duration);
                        }
                    }
                    self.collect_vec_owned_element_keys_from_expr(&arm.body);
                }
            }
            HirExprKind::Join(join) => {
                for branch in &join.branches {
                    self.prepass_note_actor_message_args(&branch.args);
                    self.collect_vec_owned_element_keys_from_expr(&branch.actor);
                    for arg in &branch.args {
                        self.collect_vec_owned_element_keys_from_expr(arg);
                    }
                }
            }
            HirExprKind::While {
                condition, body, ..
            } => {
                self.collect_vec_owned_element_keys_from_expr(condition);
                self.collect_vec_owned_element_keys_from_block(body);
            }
            HirExprKind::ForRange {
                start,
                end,
                step,
                body,
                ..
            } => {
                self.collect_vec_owned_element_keys_from_expr(start);
                self.collect_vec_owned_element_keys_from_expr(end);
                self.collect_vec_owned_element_keys_from_expr(step);
                self.collect_vec_owned_element_keys_from_block(body);
            }
            HirExprKind::Yield { value, .. }
            | HirExprKind::Break { value, .. }
            | HirExprKind::Return { value } => {
                if let Some(value) = value {
                    self.collect_vec_owned_element_keys_from_expr(value);
                }
            }
            HirExprKind::Slice {
                container,
                start,
                end,
                ..
            } => {
                self.collect_vec_owned_element_keys_from_expr(container);
                if let Some(start) = start {
                    self.collect_vec_owned_element_keys_from_expr(start);
                }
                if let Some(end) = end {
                    self.collect_vec_owned_element_keys_from_expr(end);
                }
            }
            HirExprKind::NumericMethod { receiver, arg, .. }
            | HirExprKind::MachineStep {
                receiver,
                event: arg,
                ..
            }
            | HirExprKind::MachineTakeEmits {
                receiver,
                event: arg,
                ..
            } => {
                self.collect_vec_owned_element_keys_from_expr(receiver);
                self.collect_vec_owned_element_keys_from_expr(arg);
            }
            HirExprKind::MachineEmit { fields, .. }
            | HirExprKind::MachineVariantCtor {
                payload: Some(fields),
                ..
            } => {
                for (_, value) in fields {
                    self.collect_vec_owned_element_keys_from_expr(value);
                }
            }
            HirExprKind::GenBlock { captures, .. } => {
                self.prepass_generator_capture_bindings
                    .extend(captures.iter().map(|capture| capture.binding));
            }
            // Remaining variants either carry no owned-Vec sub-expression in
            // this slice's surface or are leaves; their own `.ty` was already
            // harvested above. Fail-open here is sound: a missed harvest only
            // leaves a value at the W3.029 fail-closed reject (never an
            // over-admit), so the worst case is a still-rejected program, not
            // an unsound lowering. Closure, lambda-actor, and generator bodies
            // lower through fresh child builders with dedicated fixed-point
            // pre-passes.
            _ => {}
        }
    }
    /// Run the shared function/body pre-pass until closure-env provenance
    /// reaches a fixed point. The walk's other outputs are set-valued and
    /// idempotent, so repeating it is semantically harmless.
    pub(crate) fn collect_prepass_facts(&mut self, block: &HirBlock) {
        loop {
            let provenance_count = self.closure_pair_env_may_be_nonnull.len();
            self.collect_vec_owned_element_keys_from_block(block);
            if self.closure_pair_env_may_be_nonnull.len() == provenance_count {
                break;
            }
        }
    }
    pub(crate) fn collect_expr_prepass_facts(&mut self, expr: &HirExpr) {
        loop {
            let provenance_count = self.closure_pair_env_may_be_nonnull.len();
            self.collect_vec_owned_element_keys_from_expr(expr);
            if self.closure_pair_env_may_be_nonnull.len() == provenance_count {
                break;
            }
        }
    }
    pub(crate) fn decide(&mut self, expr: &HirExpr) {
        if self
            .decisions
            .iter()
            .any(|decision| decision.site == expr.site)
        {
            return;
        }
        // Substitute the expression type through the monomorphisation
        // map BEFORE classifying — generic origins lowered under a
        // substitution carry `expr.ty` as the raw `T` / `Wrapper<U>`
        // and would otherwise resolve to `ValueClass::Unknown` →
        // `Strategy::UnknownBlocked`, failing the
        // `DecisionMapTotal` invariant for well-typed mono'd bodies.
        let resolved_ty = self.subst_ty(&expr.ty);
        let value_class = if self.record_with_ready_inline_enum_owned_field(&resolved_ty) {
            // Generic enum origins can leave the HIR marker at `BitCopy` even
            // after substitution reveals a heap-owning inline payload. Force
            // the enclosing record onto the same deep-COW path its recursive
            // clone/drop plan proves ready; the binder seed uses this exact
            // predicate as well.
            ValueClass::CowValue
        } else if expr.value_class == ValueClass::Unknown {
            let inferred = ValueClass::of_ty(&resolved_ty, &self.type_classes);
            if inferred != ValueClass::Unknown {
                inferred
            } else if self.is_known_actor_runtime_ty(&resolved_ty) {
                ValueClass::BitCopy
            } else if (self.owned_string_record_value_sites.contains(&expr.site)
                && monomorphic_user_record_key(&resolved_ty).is_some())
                || vec_iter_record_layout_key(&resolved_ty)
                    .is_some_and(|key| self.lookup_record_field_order(&key).is_some())
                || user_record_layout_key(&resolved_ty)
                    .is_some_and(|key| self.vec_owned_element_keys.contains(&key))
                || self.is_owned_aggregate_record_ty(&resolved_ty)
            {
                // Owned-aggregate gates that all classify as CowValue:
                //   1. the owned-string-record let-bound direct-string record
                //      site (legacy W3.029 narrow path),
                //   2. the VecIter record layout,
                //   3. (W5.016) any value whose type is used as an owned-Vec
                //      element in this function, and
                //   4. (value-class capstone) the UNIFIED authority — any
                //      monomorphic user record whose fields ALL classify under
                //      the actor-state field classifier, so codegen can
                //      synthesize the matching `__hew_record_{clone,drop}_
                //      inplace_<R>` thunk. This admits the standalone
                //      record-by-value shape (construct + return) the
                //      site-based gate (1) and the element-context gate (3)
                //      both missed: RC-6 (string field), RC-4 (bytes field),
                //      and G12 (Vec/HashMap/HashSet fields). It is NOT a
                //      blanket relaxation — a record carrying a field the
                //      classifier rejects (an IO handle with no clone helper,
                //      an unresolved nested type) is excluded by
                //      `is_owned_aggregate_record_ty` and stays fail-closed at
                //      the W3.029 reject below, so codegen never observes a
                //      record whose drop thunk it cannot emit.
                ValueClass::CowValue
            } else {
                ValueClass::Unknown
            }
        } else {
            expr.value_class
        };
        if value_class == ValueClass::Unknown {
            self.push_unsupported_user_record_value_class(&resolved_ty);
        }
        let strategy = match value_class {
            ValueClass::CowValue => Strategy::CowShare,
            // `@linear` and `@resource` (AffineResource) both move by default;
            // `MirCheck::MustConsume` rejects unconsumed `@linear` exits.
            ValueClass::AffineResource | ValueClass::Linear => Strategy::Move,
            ValueClass::Unknown => Strategy::UnknownBlocked,
            ValueClass::BitCopy | ValueClass::PersistentShare | ValueClass::View => {
                Strategy::BorrowRead
            }
        };
        let strategy = match (value_class, expr.intent) {
            (ValueClass::CowValue, IntentKind::Modify) => Strategy::EnsureUnique,
            (ValueClass::CowValue, IntentKind::Read | IntentKind::Capture) => Strategy::CowShare,
            (ValueClass::AffineResource, IntentKind::Read) => Strategy::BorrowRead,
            // `@linear` Read is *not* a borrow — the value must be consumed
            // exactly once; a read-without-consume leaves the binding
            // live for a later `MustConsume` rejection. Encode as Move
            // alongside the explicit Consume arm below.
            (ValueClass::Linear, IntentKind::Read | IntentKind::Capture)
            | (
                ValueClass::BitCopy
                | ValueClass::CowValue
                | ValueClass::AffineResource
                | ValueClass::Linear,
                IntentKind::Consume,
            ) => Strategy::Move,
            (_, IntentKind::Yield) => Strategy::Freeze,
            _ => strategy,
        };
        self.decisions.push(DecisionFact {
            site: expr.site,
            ty: resolved_ty,
            value_class,
            intent: expr.intent,
            strategy,
            why: "first vertical-slice classifier".to_string(),
        });
    }
    pub(crate) fn push_unsupported_user_record_value_class(&mut self, ty: &ResolvedTy) {
        let Some(key) = user_record_layout_key(ty) else {
            return;
        };
        let fields = match self.lookup_record_field_order(&key) {
            Some(f) if !f.is_empty() => f.clone(),
            _ => return,
        };
        if !self
            .unsupported_user_record_value_classes
            .insert(key.clone())
        {
            return;
        }

        let reason = match self.owned_aggregate_record_field_kinds_for_key(&key) {
            Err(err) => {
                format!("owned-aggregate field classifier failed for `{key}`: {err}")
            }
            Ok(_) => fields
                .iter()
                .find_map(|(field_name, field_ty)| {
                    let field_class = ValueClass::of_ty(field_ty, &self.type_classes);
                    (field_class != ValueClass::BitCopy).then(|| {
                        format!(
                            "field `{field_name}` has value class {field_class:?}; \
                             user record/type aggregates are BitCopy only when every \
                             substituted field is BitCopy"
                        )
                    })
                })
                .unwrap_or_else(|| {
                    "record layout is present but no BitCopy value-class registration was produced"
                        .to_string()
                }),
        };

        self.diagnostics.push(MirDiagnostic {
            kind: MirDiagnosticKind::UnsupportedUserRecordValueClass {
                name: key.clone(),
                reason: reason.clone(),
            },
            note: format!(
                "W3.029 user record/type value-class inference rejected `{key}`: {reason}"
            ),
        });
    }
    pub(crate) fn is_known_actor_runtime_ty(&self, ty: &ResolvedTy) -> bool {
        match ty {
            ResolvedTy::Named { .. }
                if named_type_marker(ty, &self.type_classes) == Some(ResourceMarker::BitCopy) =>
            {
                true
            }
            ResolvedTy::Named { name, .. } if name.as_str() == "LocalPid" => true,
            // `Generator<Y, R>` is the checker-supplied type for a gen-block
            // expression. The S3a shell allocates a local of this type as a
            // placeholder; S3b replaces it with the real state-record type.
            // Classify only the builtin-discriminated generator as BitCopy so
            // a user type named `Generator` still follows normal readiness.
            ResolvedTy::Named {
                builtin: Some(BuiltinType::Generator),
                ..
            } => true,
            // A checker-authored imported machine spelling must already carry
            // its declaration owner here. `machine_layout_names` contains only
            // canonical class-tagged projections, so a bare presentation leaf
            // cannot recover by suffix and accidentally select a same-leaf
            // machine from another module.
            ResolvedTy::Named { name, args, .. } if args.is_empty() => {
                self.actor_layouts.contains_key(name)
                    || machine_layout_ty_matches(&self.machine_layout_names, ty)
            }
            // Generic enum applications (`Named { name: "Option", args: [I64] }`):
            // its canonical class-tagged projection is in
            // `machine_layout_names` if the HIR mono pass discovered at least
            // one instantiation and registered it in `module.enum_layouts`.
            // Actor layouts never have type args, so this arm is purely for
            // generic enum types. Classifying as `BitCopy`
            // matches the tagged-union substrate — enums are stack-allocated
            // discriminated unions with no drop side-effect.
            ResolvedTy::Named { .. } => machine_layout_ty_matches(&self.machine_layout_names, ty),
            _ => actor_name_from_handle_ty(ty).is_some(),
        }
    }
    /// Exclude a bare returned binding (`return x` / tail `x`) from the
    /// function-exit drop set: handing `x`'s owner to the caller means the
    /// callee must not also drop it.
    ///
    /// This handles ONLY the syntactically-direct single-binding return. The
    /// harder member-exclusion problem — a composite return
    /// (`(a, b)` / `R { f: a, .. }`) reached directly, by name, or through any
    /// control-flow tail, whose constituent OWNED members are byte-copied into
    /// the returned aggregate with no retain (the M-COW spine emits no retain on
    /// share) — is solved by the value-flow authority
    /// [`derive_returned_aggregate_member_bindings`] in `elaborate`, NOT here.
    /// A prior revision tried to enumerate composite return shapes syntactically
    /// in this walk; that was fail-OPEN (it missed `let pair = (s, r); pair`,
    /// `if`/`match`/`scope`/`loop` tails, …) and the missed members
    /// double-freed. The value-flow pass tracks what actually flows into the
    /// `ReturnSlot` aggregate, so a return grammar this walk does not recognise
    /// can no longer leave a member drop-eligible.
    ///
    /// Removing `x` from `owned_locals` here for the aggregate-binding case
    /// (`let pair = (s, r); pair`) is still correct and complementary: it
    /// suppresses the aggregate's own in-place drop, while the value-flow pass
    /// independently suppresses the member handles' drops (they remain in
    /// `owned_locals`, which the pass reads from). LESSONS: raii-null-after-move.
    /// Allocate (once) the path-sensitive drop-flag for a non-idempotent
    /// user `#[resource]` binding (#1933 / #1941). Called at the binding's
    /// introducing `let` after its backend `Place` is wired into
    /// `binding_locals`. A no-op unless `affine_release_needs_drop_flag`
    /// holds, so unrelated values and idempotent handles are untouched.
    ///
    /// The flag is a fresh `i64` local zero-initialised at this point so the
    /// initialisation dominates every later `Consume` use site and every
    /// scope-exit drop; codegen gates the close on `flag == 0`. Re-entrant:
    /// a rebind of the same binding id keeps the existing flag (the
    /// dominating zero-init already fired).
    pub(crate) fn maybe_alloc_affine_release_flag(
        &mut self,
        binding_id: BindingId,
        ty: &ResolvedTy,
    ) {
        let Some(place) = self.binding_locals.get(&binding_id).copied() else {
            return;
        };
        if !affine_release_needs_drop_flag(place, ty, &self.type_classes) {
            return;
        }
        if self.affine_release_flags.contains_key(&binding_id) {
            return;
        }
        let flag = self.alloc_local(ResolvedTy::I64);
        self.instructions.push(Instr::ConstI64 {
            dest: flag,
            value: 0,
        });
        self.affine_release_flags.insert(binding_id, flag);
        assert!(
            self.publish_current_owner_guard(
                binding_id,
                flag,
                crate::model::OwnershipGuardKind::AffineRelease,
            ),
            "affine release flag for {binding_id:?} has no explicit owner generation"
        );
    }
    /// #2301 -- allocate a zero-init path-sensitive overwrite-release drop-flag
    /// for an owned `var`-local that the pre-pass saw both genuinely consumed
    /// (move-out) AND reassigned. Restricting to that intersection keeps every
    /// other owned-var overwrite on the zero-churn static gate. Gated on
    /// `owned_locals` membership so the flag is allocated only for a binding
    /// whose value `emit_local_overwrite_release` actually releases (the
    /// general owned-local push already classified the type as heap-owning).
    /// Zero-init here so the flag dominates every consume and overwrite,
    /// including loop back-edges (lazy alloc at the consume would be unsound for
    /// the non-consuming path and for an overwrite that precedes the consume in
    /// source order but follows it around a back-edge).
    pub(crate) fn maybe_alloc_overwrite_guard_flag(&mut self, binding: &HirBinding) {
        if !binding.mutable {
            return;
        }
        // Affine resources already use their release flag for both move-out
        // suppression and reassignment generation reset. A second overwrite
        // bit would publish two competing cleanup authorities for one OwnerId,
        // while `ElabDrop` deliberately carries exactly one guard.
        if self.affine_release_flags.contains_key(&binding.id) {
            return;
        }
        if !self.prepass_consumed_bindings.contains(&binding.id) {
            return;
        }
        if !self.prepass_reassigned_bindings.contains(&binding.id) {
            return;
        }
        if !self
            .owned_locals
            .iter()
            .any(|entry| entry.binding == binding.id && entry.disposition == Disposition::ScopeExit)
        {
            return;
        }
        if self.overwrite_guard_flags.contains_key(&binding.id) {
            return;
        }
        let flag = self.alloc_local(ResolvedTy::I64);
        self.instructions.push(Instr::ConstI64 {
            dest: flag,
            value: 0,
        });
        self.overwrite_guard_flags.insert(binding.id, flag);
        assert!(
            self.publish_current_owner_guard(
                binding.id,
                flag,
                crate::model::OwnershipGuardKind::Overwrite,
            ),
            "overwrite guard flag for {:?} has no explicit owner generation",
            binding.id
        );
    }
    /// #2418 -- allocate a zero-init path-sensitive scope-exit drop-flag for an
    /// owned collection local (owned-element `Vec`, plain `Vec`,
    /// `HashMap`/`HashSet` handle) whose pre-pass consumes are ALL direct
    /// `let`-rebind moves (`let ys = xs;`). The flag keeps the binding on the
    /// scope-exit set at its consume sites (see `collection_drop_flags`), so
    /// a conditional move (`if take { let ys = xs; }`) releases the value
    /// exactly once: the runtime gate skips the moved path, the not-moved
    /// path fires the release.
    ///
    /// Restricted to the collection classes whose releases are null-tolerant
    /// runtime frees and whose mint-site admission rides the local-collection
    /// escape scan; every other owned
    /// class keeps the legacy path-insensitive retraction (fail-closed: leak
    /// on the not-moved path, never a double-free). A mutable binding that is
    /// also reassigned takes the #2301 `overwrite_guard_flags` path instead —
    /// the two flag families never share a binding, so the overwrite reset
    /// discipline cannot re-arm a scope-exit drop this flag suppressed.
    /// Zero-init at the `let` so the flag dominates every consume site,
    /// including loop back-edges (a per-iteration `let` re-zeros it).
    pub(crate) fn maybe_alloc_collection_drop_flag(
        &mut self,
        binding: &HirBinding,
        ty: &ResolvedTy,
    ) {
        if !self.prepass_consumed_bindings.contains(&binding.id) {
            return;
        }
        // #2418 — any consume in a non-rebind position (call argument,
        // aggregate-literal field, return, assignment RHS, nested read)
        // disqualifies the flag: those shapes are owning-sink escapes to the
        // allow-set provers, and a flagged (still-registered) source would
        // taint its whole-value alias group where the legacy retraction lets
        // the destination stand alone. Fail closed to the retraction —
        // byte-identical to the pre-flag compiler for those shapes.
        if self.prepass_nonrebind_consumed.contains(&binding.id) {
            return;
        }
        if binding.mutable && self.prepass_reassigned_bindings.contains(&binding.id) {
            return;
        }
        if !(self.binding_ty_is_owned_element_vec(ty)
            || self.binding_ty_is_plain_vec(ty)
            || ty_is_local_collection_handle(ty))
        {
            return;
        }
        if !self
            .owned_locals
            .iter()
            .any(|entry| entry.binding == binding.id && entry.disposition == Disposition::ScopeExit)
        {
            return;
        }
        if self.collection_drop_flags.contains_key(&binding.id) {
            return;
        }
        let flag = self.alloc_local(ResolvedTy::I64);
        self.instructions.push(Instr::ConstI64 {
            dest: flag,
            value: 0,
        });
        self.collection_drop_flags.insert(binding.id, flag);
        assert!(
            self.publish_current_owner_guard(
                binding.id,
                flag,
                crate::model::OwnershipGuardKind::Collection,
            ),
            "collection drop flag for {:?} has no explicit owner generation",
            binding.id
        );
    }

    /// Allocate the path-sensitive scope-exit flag for a mailbox-owned `CoW`
    /// leaf parameter that is consumed on at least one handler path.
    ///
    /// Unlike ordinary by-value parameters, actor message parameters own the
    /// delivered value. A branch that moves the value into actor state (or
    /// forwards it) and then rejoins a borrow-only branch produces a
    /// `MaybeConsumed` exit. The static sole-owner scan must keep a drop for
    /// the live path, while the moved path must suppress it at runtime.
    pub(crate) fn maybe_alloc_actor_message_cow_drop_flag(
        &mut self,
        binding: BindingId,
        ty: &ResolvedTy,
    ) {
        // `string` and `bytes` intentionally have separate sole-owner
        // admission authorities, but both use the same actor-message transfer
        // protocol. `cow_value_leaf_drop_symbol` admits only `string`; accept
        // `bytes` explicitly here so its dedicated BytesTriple drop can carry
        // the same path-sensitive guard without creating a second drop class.
        let is_actor_message_cow_leaf =
            super::cow_value_leaf_drop_symbol(ty).is_some() || matches!(ty, ResolvedTy::Bytes);
        let is_registered_owner = self
            .owned_locals
            .iter()
            .any(|entry| entry.binding == binding && entry.disposition == Disposition::ScopeExit);
        if super::cow_value_leaf_drop_symbol(ty).is_some() && is_registered_owner {
            self.actor_message_string_owner_bindings.insert(binding);
        }
        let may_transfer = self.prepass_consumed_bindings.contains(&binding)
            || (matches!(ty, ResolvedTy::Bytes)
                && self
                    .prepass_actor_message_transfer_bindings
                    .contains(&binding));
        if !may_transfer
            || !is_actor_message_cow_leaf
            || !is_registered_owner
            || self.actor_message_cow_drop_flags.contains_key(&binding)
        {
            return;
        }
        let flag = self.alloc_local(ResolvedTy::I64);
        self.instructions.push(Instr::ConstI64 {
            dest: flag,
            value: 0,
        });
        self.actor_message_cow_drop_flags.insert(binding, flag);
        assert!(
            self.publish_current_owner_guard(
                binding,
                flag,
                crate::model::OwnershipGuardKind::ActorMessageCow,
            ),
            "actor-message CoW flag for {binding:?} has no explicit owner generation"
        );
    }

    /// Allocate the path-sensitive scope-exit flag for a freshly constructed
    /// String/BitCopy record that is consumed on at least one body path.
    ///
    /// The `owned_string_record_init_key_for_let` authority proves this is a
    /// direct monomorphic construction whose owned leaves use W60.108's
    /// retain-backed String ingress. Keeping the record registered is therefore
    /// sound: the flag selects between its local owner on a non-consuming path
    /// and the destination owner after a consume.
    ///
    /// This flag family is deliberately exclusive with every other runtime
    /// ownership flag. In particular, a mutable consume-and-reassign binding
    /// belongs solely to #2301's overwrite protocol, and a `#[resource]` /
    /// `#[linear]` marker belongs solely to its affine close protocol. Letting
    /// either also acquire `RecordInPlace` authority lets one flag re-arm a drop
    /// that the other flag suppressed.
    pub(crate) fn maybe_alloc_conditional_record_drop_flag(
        &mut self,
        binding: &HirBinding,
        ty: &ResolvedTy,
        is_owned_string_record_init: bool,
    ) {
        if !is_owned_string_record_init
            || !self.prepass_consumed_bindings.contains(&binding.id)
            || (binding.mutable && self.prepass_reassigned_bindings.contains(&binding.id))
            || matches!(
                named_type_marker(ty, &self.type_classes),
                Some(ResourceMarker::Resource | ResourceMarker::Linear)
            )
            || self.affine_release_flags.contains_key(&binding.id)
            || self.collection_drop_flags.contains_key(&binding.id)
            || self.actor_message_cow_drop_flags.contains_key(&binding.id)
            || self.overwrite_guard_flags.contains_key(&binding.id)
            || !self.owned_locals.iter().any(|entry| {
                entry.binding == binding.id && entry.disposition == Disposition::ScopeExit
            })
            || self.conditional_record_drop_flags.contains_key(&binding.id)
        {
            return;
        }
        let flag = self.alloc_local(ResolvedTy::I64);
        self.instructions.push(Instr::ConstI64 {
            dest: flag,
            value: 0,
        });
        self.conditional_record_drop_flags.insert(binding.id, flag);
        assert!(
            self.publish_current_owner_guard(
                binding.id,
                flag,
                crate::model::OwnershipGuardKind::ConditionalRecord,
            ),
            "conditional record flag for {:?} has no explicit owner generation",
            binding.id
        );
    }

    /// #2301 -- emit `if flag == 0 { <release old value of `dest`> }` as a CFG
    /// diamond, then leave the cursor at the continuation block so the caller's
    /// `Move` (store of the fresh value) and the `flag = 0` reset land there.
    /// `flag == 0` means the prior value is still owned on THIS runtime path (a
    /// consume on some other path set it to 1, handing the value to a new owner
    /// that drops it -- releasing here too would double-free). The nested
    /// `emit_local_overwrite_release` only pushes instructions (no terminator),
    /// so it is safe inside the release block.
    pub(crate) fn emit_flag_gated_overwrite_release(
        &mut self,
        binding: BindingId,
        dest: Place,
        target_ty: &ResolvedTy,
        flag: Place,
        value: &HirExpr,
    ) {
        let zero = self.alloc_local(ResolvedTy::I64);
        self.push_instr(Instr::ConstI64 {
            dest: zero,
            value: 0,
        });
        let still_owned = self.alloc_local(ResolvedTy::Bool);
        self.push_instr(Instr::IntCmp {
            dest: still_owned,
            pred: CmpPred::Eq,
            lhs: flag,
            rhs: zero,
        });
        let release_bb = self.alloc_block();
        let cont_bb = self.alloc_block();
        self.finish_current_block(Terminator::Branch {
            cond: still_owned,
            then_target: release_bb,
            else_target: cont_bb,
        });
        self.start_block(release_bb);
        self.emit_local_overwrite_release(binding, dest, target_ty, Some(flag));
        self.emit_enum_overwrite_release(binding, dest, target_ty, value, Some(flag));
        self.finish_current_block(Terminator::Goto { target: cont_bb });
        self.start_block(cont_bb);
    }
    fn mark_anonymous_typed_producer_moved(&mut self, expr: &HirExpr, transferee: Option<Place>) {
        if let HirExprKind::BindingRef {
            resolved: ResolvedRef::Binding(_),
            ..
        } = expr.kind
        {
            // `lower_value_for_move` has already lowered this binding under
            // its Consume intent and ended the live OwnerId generation at the
            // exact source-read program point.  A second logical consume here
            // used to append a duplicate Transfer later in the return block
            // (`result_dest -> binder -> ReturnSlot` for value-task awaits),
            // which correctly failed generation validation.  This hook only
            // has additional work for anonymous typed producer results whose
            // owner is not represented by a BindingRef use.
            return;
        }
        let Some(place) = self.published_value_places.get(&expr.site).copied() else {
            return;
        };
        let owners: Vec<_> = self
            .owned_locals
            .iter()
            .filter(|entry| {
                self.binding_locals.get(&entry.binding) == Some(&place)
                    && self.synthetic_owner_publication_sites.get(&entry.binding)
                        == Some(&expr.site)
                    && self
                        .typed_produced_value_owner_bindings
                        .contains(&entry.binding)
            })
            .map(|entry| (entry.binding, entry.name.clone(), entry.ty.clone()))
            .collect();
        for (binding, name, ty) in owners {
            // Returning or yielding is a consuming ownership event even when
            // lexical cleanup or a coroutine suspend inserts successor blocks
            // after the handoff. Carry it through the CFG authority instead of
            // asking a later block to rediscover an earlier physical move.
            self.statements.push(MirStatement::Use {
                binding,
                name,
                site: expr.site,
                ty,
                intent: IntentKind::Consume,
            });
            self.set_owned_local_consumed(binding, transferee, DischargeSite::BindingMoved);
        }
    }

    pub(crate) fn mark_returned_binding_moved(&mut self, expr: &HirExpr) {
        self.mark_anonymous_typed_producer_moved(expr, Some(Place::ReturnSlot));
    }

    /// End an anonymous produced-value generation at the generator output
    /// handoff. The companion output slot owns the yielded value from this
    /// point; retaining the producer's lexical drop would release it again on
    /// normal resume (and the output-drop thunk would release it a third time
    /// when an unconsumed generator is destroyed).
    pub(crate) fn mark_yielded_binding_moved(&mut self, expr: &HirExpr) {
        self.mark_anonymous_typed_producer_moved(expr, None);
    }
    pub(crate) fn mark_binding_moved(&mut self, id: BindingId) {
        // A parent overwrite can transfer an inline enum's old string payload
        // into its projected arm binder (`flag = 0`). If that binder is then
        // moved onward, it no longer owns the delayed release: re-arm the same
        // guard on this exact runtime path so an arm-close / early-exit plan
        // cannot release after the transferee. This must happen at the general
        // consume seam rather than at the parent overwrite — a conditional
        // consume leaves the sibling path at zero, where the binder still owns
        // and must release the payload.
        if let Some(flag) = self.projected_payload_overwrite_flags.get(&id).copied() {
            self.instructions.push(Instr::ConstI64 {
                dest: flag,
                value: 1,
            });
        }
        // #2301 -- record the move-out at runtime for a binding that carries a
        // path-sensitive overwrite-release flag. Setting the flag on EVERY
        // consume retraction (the `ConsumedAt` disposition write below, every
        // consume site, not just the primary `Use{Consume}` lowering) means a
        // later overwrite on a DIFFERENT
        // control-flow path correctly SKIPS the release (the moved-out value's
        // new owner drops it), while the non-consuming path keeps `flag == 0`
        // and releases. The flag is reset to 0 after that overwrite's store. A
        // no-op for every unflagged binding (the common case).
        if let Some(flag) = self.overwrite_guard_flags.get(&id).copied() {
            self.instructions.push(Instr::ConstI64 {
                dest: flag,
                value: 1,
            });
        }
        // General consume seam: the value is moved out (returned / sent / stored
        // into a longer-lived owner) with no destination local nameable here, so
        // the transferee is `None`; the authority is recorded regardless.
        self.set_owned_local_consumed(id, None, DischargeSite::BindingMoved);
    }
    // JUSTIFIED: this predicate deliberately stays adjacent to the aggregate
    // alias marker instead of collapsing to `ty_contains_heap_owning` alone.
    // `ValueClass::AffineResource | Linear` covers move-only handles,
    // `is_owned_aggregate_record_ty` is the record-admission authority, and the
    // recursive enum/tuple/array walk below avoids marking registered user
    // records that have not been admitted as owned aggregate values merely
    // because a generic argument is heap-owning.
    pub(crate) fn aggregate_ingress_moves_binding_ty(&self, ty: &ResolvedTy) -> bool {
        self.aggregate_ingress_moves_binding_ty_inner(ty, &mut HashSet::new())
    }
    pub(crate) fn aggregate_ingress_moves_binding_ty_inner(
        &self,
        ty: &ResolvedTy,
        visited_enum_layouts: &mut HashSet<String>,
    ) -> bool {
        // Actor PIDs are copyable scheduler identities, not affine runtime
        // owners. Their historical Resource-shaped marker must never turn a
        // record/actor-state copy into a destructive move. Keep this canonical
        // type rule ahead of the generic ValueClass axis so payload type args
        // cannot change LocalPid's ownership classification.
        if super::drop_plan::ty_is_nonowning_handle_leaf(ty) {
            return false;
        }
        if matches!(
            ValueClass::of_ty(ty, &self.type_classes),
            ValueClass::AffineResource | ValueClass::Linear
        ) {
            return true;
        }

        if self.is_owned_aggregate_record_ty(ty) {
            return true;
        }

        match ty {
            ResolvedTy::TraitObject { .. }
            | ResolvedTy::String
            | ResolvedTy::Bytes
            | ResolvedTy::CancellationToken
            | ResolvedTy::Named {
                builtin:
                    Some(
                        BuiltinType::Vec
                        | BuiltinType::HashMap
                        | BuiltinType::HashSet
                        | BuiltinType::Generator
                        | BuiltinType::AsyncGenerator,
                    ),
                ..
            } => true,
            ResolvedTy::Tuple(elems) => {
                elems.iter().any(|elem| {
                    self.aggregate_ingress_moves_binding_ty_inner(elem, visited_enum_layouts)
                }) || crate::model::ty_owns_heap_mir(
                    ty,
                    &self.record_field_orders,
                    &self.enum_layouts,
                )
            }
            ResolvedTy::Array(elem, _) => {
                self.aggregate_ingress_moves_binding_ty_inner(elem, visited_enum_layouts)
                    || crate::model::ty_owns_heap_mir(
                        ty,
                        &self.record_field_orders,
                        &self.enum_layouts,
                    )
            }
            ResolvedTy::Named { name, args, .. } => {
                let layout = crate::model::find_enum_layout(name, args, &self.enum_layouts);
                if let Some(layout) = layout {
                    let layout_name = layout.name.clone();
                    if !visited_enum_layouts.insert(layout_name.clone()) {
                        return true;
                    }
                    let field_tys: Vec<ResolvedTy> = layout
                        .variants
                        .iter()
                        .flat_map(|variant| variant.field_tys.iter().cloned())
                        .collect();
                    let owns = field_tys.iter().any(|field_ty| {
                        self.aggregate_ingress_moves_binding_ty_inner(
                            field_ty,
                            visited_enum_layouts,
                        )
                    });
                    visited_enum_layouts.remove(&layout_name);
                    return owns;
                }
                let is_registered_record = user_record_layout_key(ty)
                    .is_some_and(|key| self.lookup_record_field_order(&key).is_some());
                if is_registered_record {
                    return false;
                }
                crate::model::ty_owns_heap_mir(ty, &self.record_field_orders, &self.enum_layouts)
            }
            _ => crate::model::ty_owns_heap_mir(ty, &self.record_field_orders, &self.enum_layouts),
        }
    }
    /// B1 (use-after-move into an aggregate): when an owned or heap-owning
    /// operand is moved (aliased) into an aggregate constructor (tuple, record,
    /// enum variant payload, or array literal), emit a checker-stream
    /// `MirStatement::AggregateAlias` marker for the source binding so the
    /// move-checker dataflow flags any later use of it as `UseAfterConsume` at
    /// CHECK time.
    ///
    /// The marker is deliberately NOT a `Use { Consume }`: consuming the source
    /// would suppress its scope-exit drop and break the alias/escape-scan drop
    /// machinery (it would silently turn the W3.053 fail-closed aggregate-
    /// double-free refusals into leaks). `AggregateAlias` keeps the binding a
    /// live owner for every drop reader and only adds the use-after-move check.
    ///
    /// Copy operands carry no single-owner drop obligation and share freely, so
    /// they must NOT be flagged: `BitCopy` ints/durations, non-owning borrows,
    /// and persistent handles are excluded by `aggregate_ingress_moves_binding_ty`.
    ///
    /// `CowValue` operands with `IntentKind::Capture` share the refcounted handle
    /// via `CowShare` rather than moving it; the source binding stays Live.  The
    /// canonical case is `for x in vec { … }` desugaring, which places the Vec
    /// handle into `VecIter { vec: _, idx: 0 }` with Capture intent so the source
    /// collection is usable after the loop.  Emitting `AggregateAlias` for such
    /// a captured (shared) `CowValue` operand would incorrectly mark the source
    /// Consumed.
    ///
    /// All other intent values (Read, Consume, Modify, Yield, Unknown) trigger
    /// the alias marker as before — including `CowValue` with Read intent, where
    /// the HIR signals a structural move into an aggregate (e.g. strings into
    /// tuples or `HashSet.insert`).
    pub(crate) fn alias_moved_owned_operand(&mut self, operand: &HirExpr) {
        let HirExprKind::BindingRef {
            name,
            resolved: ResolvedRef::Binding(id),
        } = &operand.kind
        else {
            return;
        };
        let ty = self.subst_ty(&operand.ty);
        if !self.aggregate_ingress_moves_binding_ty(&ty) {
            return;
        }
        // Refcounted flat leaves use retain-on-share at aggregate ingress. The
        // finalized MIR prover emits the matching retain immediately before the
        // owning store and keeps the source binding's drop obligation.
        if matches!(ty, ResolvedTy::Bytes | ResolvedTy::String) {
            return;
        }
        // A CowValue binding with Capture intent is a refcount-share (CowShare),
        // not a structural move.  The source stays Live; skip the alias marker.
        // This intent is set exclusively by the for-in Vec borrow desugaring path.
        if operand.intent == IntentKind::Capture
            && ValueClass::of_ty(&ty, &self.type_classes) == ValueClass::CowValue
        {
            return;
        }
        self.statements.push(MirStatement::AggregateAlias {
            binding: *id,
            name: name.clone(),
            site: operand.site,
            ty,
            // Whole-value placement into a fresh aggregate: `(t, t)` double
            // placement IS a use-after-move, so keep the strict check.
            partial_projection: false,
        });
        self.record_affine_aggregate_ingress_transfer(*id);
    }

    /// Record the path-local ownership TRANSFER for an affine handle whose
    /// whole value was just placed into an owning aggregate.
    ///
    /// An `Rc` / `Weak` handle (and a non-idempotent user `#[resource]`) is
    /// byte-copied into a tuple / record / enum payload / machine payload /
    /// array element with NO retain: the aggregate's composite drop
    /// (`tuple_in_place`, `record_in_place`, `enum_in_place`, the container's
    /// element release) becomes an owner of the SAME count the source binder
    /// still holds a `DropKind::RcRelease` obligation for. Left unrecorded,
    /// both fire and the second one underflows the strong count — the runtime's
    /// `Rc double-free` abort, reachable from a plain
    /// `let pair = (shared, "tag")` with no `clone` anywhere.
    ///
    /// Every other owning class already answers this: `Vec`, `HashMap` /
    /// `HashSet`, `bytes`, `CoW string` and owned records each gate their
    /// scope-exit release on a per-class escape-scan allow-set that removes a
    /// handle proven to have escaped into an aggregate. `Rc` / `Weak` have no
    /// such allow-set — their `AffineResource` recipe is replayed onto every
    /// exit where the owner is still live — so the transfer has to be recorded
    /// where it happens.
    ///
    /// The record is the SAME path-sensitive drop-flag store a by-value
    /// `Use { Consume }` already emits (`#1933` / `#1941`):
    /// `affine_release_needs_drop_flag` mints the flag for EVERY `Rc` / `Weak`
    /// local at its introducing `let`, zero-initialised so the init dominates
    /// this store and every exit, and codegen gates the release on `flag == 0`.
    /// Storing `1` here therefore skips the binder's release on exactly the
    /// paths that transferred the handle and keeps it on the paths that did not
    /// — the conditional ingress (`match flag { true => (shared, "t"), false =>
    /// 0 }`) stays exactly-once on both arms.
    ///
    /// Deliberately NOT a dataflow `Consumed` transition: `AliasedIntoAggregate`
    /// is `Live` for every escape-scan / alias / drop reader, and demoting it
    /// would turn the W3.053 fail-closed aggregate refusals into leaks for the
    /// classes that DO have allow-sets. The binder keeps its `owned_locals`
    /// membership and its dataflow state; only the runtime transfer record is
    /// added. A binding with no flag (any non-affine class) is untouched.
    fn record_affine_aggregate_ingress_transfer(&mut self, binding: BindingId) {
        let Some(flag) = self.affine_release_flags.get(&binding).copied() else {
            return;
        };
        self.push_instr(Instr::ConstI64 {
            dest: flag,
            value: 1,
        });
    }
    /// True when an overriding functional-update field VALUE is, at its
    /// value-producing root, a bare interior alias of the consumed base's
    /// heap: a whole `base` reference or a `base.field` projection of an
    /// OWNED (heap-owning) field.
    ///
    /// Owned-record `..base` consumes the base (its carried fields escape via
    /// `RecordFieldLoad` into the new record and its OVERRIDDEN owned fields
    /// are destructively released at the construction site). An override value
    /// that bare-projects an owned field of that same base is a non-retaining
    /// interior alias; the override-drop frees it before the new record is
    /// built — a use-after-free (the repro-B self-override shape
    /// `{ items: s.items, ..s }`). Fail closed: the caller rejects.
    ///
    /// Values whose root is a method/function call, operator, index, or
    /// literal are NOT flagged even when they READ `base.field` internally:
    /// they produce a fresh or copied value (`base.items.clone()`,
    /// `base.n + 1`, `base.items.len()`), which the override-drop cannot
    /// dangle. A `BitCopy` / `View` / `PersistentShare` field projection
    /// (`base.count`) is a copied scalar that is never released, so it is not
    /// a hazard either. Transparent tail-only blocks are peeled.
    pub(crate) fn functional_update_value_aliases_base(
        &self,
        value: &HirExpr,
        base_id: BindingId,
    ) -> bool {
        match &value.kind {
            // Peel a transparent tail-only block wrapper (`{ base.items }`).
            HirExprKind::Block(block) if block.statements.is_empty() => block
                .tail
                .as_deref()
                .is_some_and(|tail| self.functional_update_value_aliases_base(tail, base_id)),
            // A whole `base` value handed into a field position.
            HirExprKind::BindingRef {
                resolved: ResolvedRef::Binding(id),
                ..
            } => *id == base_id,
            // A bare `base.field` projection of an owned (heap-owning) field.
            HirExprKind::FieldAccess { object, .. } => {
                matches!(
                    &object.kind,
                    HirExprKind::BindingRef {
                        resolved: ResolvedRef::Binding(id),
                        ..
                    } if *id == base_id
                ) && !matches!(
                    ValueClass::of_ty(&self.subst_ty(&value.ty), &self.type_classes),
                    ValueClass::BitCopy | ValueClass::View | ValueClass::PersistentShare
                )
            }
            _ => false,
        }
    }
    /// Fail-closed ALLOWLIST for the destructive functional-update base.
    ///
    /// An owned-record `..base` consumes the base in place: its non-overridden
    /// owned fields escape via shallow `RecordFieldLoad` into the new record,
    /// and its OVERRIDDEN owned fields are destructively released at the
    /// construction site (the override-drop). Both operations are sound ONLY
    /// when the base does not interior-alias storage that stays live after the
    /// update. Rather than enumerate the unsafe shapes (a denylist that has
    /// repeatedly missed cases — `FieldAccess`, then `Index`, then `TupleIndex`
    /// — each a fresh use-after-free), this admits a base ONLY when it is
    /// PROVABLY safe and rejects everything else, so no projection shape — now
    /// or as new expression forms are added — can silently reopen the UAF.
    ///
    /// A base is provably safe in exactly two ways:
    ///   (a) a SYNTACTICALLY bare live `BindingRef` whose PROVENANCE proves it
    ///       is the unique live owner of its heap fields. `alias_moved_owned_-
    ///       operand` consume-marks the binding so the move-checker rejects any
    ///       later read and excludes it from its scope-exit drop — but consuming
    ///       the NAME does not make the destructive release sound if the
    ///       binding's heap fields are ALIASED by a still-live owner. A binding
    ///       bound from a projection of a live value (`let b = o.inner; ..b`)
    ///       shares `o.inner`'s leaf pointers; consuming `b` then frees storage
    ///       `o` still references — a double-free (the 5th UAF this allowlist
    ///       leaked). So (a) holds ONLY when the per-function provenance prescan
    ///       (`compute_funcupdate_base_provenance`, consulted via
    ///       `funcupdate_base_proven`) proves EVERY definition of the binding is
    ///       a materialised owner — its `let` initialiser, every `=`
    ///       reassignment, or a by-value parameter origin — or a move-chain of
    ///       such (`let c = makeThing(); let d = c; ..d`). A binding wrapped in
    ///       ANY control/block form does NOT qualify as (a): the consume does
    ///       not peel wrappers, and a conditionally-selected binding cannot be
    ///       soundly consumed — such a wrapper is held to (b) below.
    ///   (b) a materialised owner with no live named alias — see
    ///       `expr_is_materialized_owner`, which looks THROUGH wrapper forms
    ///       (block tail, `if` branches, `match` arms) and requires EVERY
    ///       reachable value to be a fresh owned rvalue (`makeInner()`,
    ///       `o.inner.clone()`), a record/tuple/enum literal or funcupdate
    ///       result (`Record { .. }`), a `Vec<T>` element / slice (`v[i]`), or a
    ///       projection rooted at one (`makeOuter().inner`, `o.items[0].inner`).
    ///
    /// ANY other base — a bare binding whose provenance is unproven (a rebind of
    /// a live projection `let b = o.inner`/`let b = t.0`, a match-arm payload, a
    /// let-else binder, a loop variable), a projection of a LIVE binding
    /// (`o.inner`, `t.0`, `o.pair.0`, `t.0.inner`, nested), a machine-state field
    /// (`self.field`), a `Const`/`Item` ref, a deref, a wrapper whose
    /// tail/branch/arm is any of those (`{ base }`, `if c { o.inner } else {
    /// makeInner() }`), or any future expression form — is NOT provably safe and
    /// is rejected fail-closed.
    pub(crate) fn base_is_safe_for_destructive_funcupdate(&self, base: &HirExpr) -> bool {
        // (a) A syntactically bare binding is the consume case — but ONLY when
        //     its provenance proves it is the unique live owner of its heap
        //     fields. Fail closed (reject) for any binding the prescan did not
        //     prove (a live-projection rebind, a match/let-else/loop binder, or
        //     an unseen origin). NO wrapper peeling here: a block/if/match-
        //     wrapped binding is not reliably consume-marked, so it must instead
        //     prove materialised via (b).
        if let HirExprKind::BindingRef {
            resolved: ResolvedRef::Binding(binding_id),
            ..
        } = &base.kind
        {
            return self
                .funcupdate_base_proven
                .get(binding_id)
                .copied()
                .unwrap_or(false);
        }
        // (b) Every other base — INCLUDING every wrapper — is safe only if
        //     every reachable value is a materialised owner with no live alias.
        Self::expr_is_materialized_owner(
            base,
            &self.call_scrutinee_provenance.fresh_owner_verdicts,
            &self.funcupdate_param_ids,
            &self.proven_foreign_bindings,
        )
    }
    /// True when `expr` evaluates to a freshly MATERIALISED owner — a value in
    /// its own storage that does not alias any surviving named binding's inline
    /// fields. Used for the `(b)` arm of the funcupdate base allowlist and to
    /// recurse a projection's object chain.
    ///
    /// COMPLETE THROUGH WRAPPERS: a value-passthrough form (a block tail, both
    /// `if` branches, all `match` arm bodies) is materialised ONLY when EVERY
    /// reachable value-producing position is itself materialised. A bare-binding
    /// or live-projection leaf anywhere inside a wrapper fails the whole base
    /// (it is not consume-marked through the wrapper, and a conditionally-
    /// selected binding cannot be soundly consumed) — e.g.
    /// `if c { o.inner } else { makeInner() }` and `{ let z = 0; o.inner }` are
    /// rejected because a reachable value aliases the live `o`.
    ///
    /// Materialised leaves:
    ///   * a `.clone()` result (`RecordCloneCall`) — a fresh deep copy;
    ///   * a free-function `Call` whose callee is PROVEN to return a fresh owner
    ///     by the module interprocedural summary (`compute_fn_returns_fresh_-
    ///     owner`, threaded as `fresh`). A call is NOT blanket-fresh: a function
    ///     can launder a by-value heap parameter (a BORROW — LESSONS
    ///     `by-value-heap-params-are-borrows`) through its return without a
    ///     refcount bump (`fn id(p: Inner) -> Inner { p }`), so `..id(o.inner)`
    ///     would free the caller's live `o.inner` at the override-drop. Method
    ///     calls (`CallDynMethod`/`CallTraitMethodStatic`/`VarSelfMethodCall`/
    ///     `ResolvedImplCall`) can likewise return borrowed `self`/params and are
    ///     NOT summarised, so they fail closed;
    ///   * a `Vec<T>` element load (`v[i]`) or slice (`v[a..b]`). The element
    ///     is independent of any live binding: `hew_vec_push_owned` deep-clones
    ///     each element on insert (`clone_fn`) and the buffer carries its own
    ///     refcount, so the override-drop's in-place release of an element
    ///     field decrements a shared count rather than freeing storage a live
    ///     binding still references. (NOTE: `hew_vec_get_owned` itself returns a
    ///     BORROW into the buffer, not a clone — the safety comes from the
    ///     push-time deep clone + refcount, not from a materialising getter.
    ///     `Index` is checker-restricted to `Vec<T>`; a future aliasing
    ///     container would need re-evaluation here.)
    ///
    /// A projection (`expr.field`, `expr.0`) is materialised ONLY when its
    /// object chain bottoms out at a materialised owner — `makeOuter().inner`
    /// is safe, `o.inner` (rooted at a live binding) is not.
    ///
    /// EVERY other form — a `BindingRef` (live local, `Const`, or `Item`), a
    /// `MachineFieldAccess` (`self.field`), a deref, a loop-break value, or any
    /// expression form added later — returns false (fail closed).
    // The `RecordCloneCall`/`Call` arm and the `Index`/`Slice` arm both can
    // yield `true` but are kept separate: they admit a leaf for DIFFERENT safety
    // reasons (a clone / proven-fresh call is a fresh return-slot value; a `Vec`
    // element is heap-independent via push-clone + refcount). Merging them would
    // erase that distinction in a security-critical allowlist.
    #[allow(
        clippy::match_same_arms,
        reason = "distinct safety rationales per arm in a security-critical allowlist"
    )]
    pub(crate) fn expr_is_materialized_owner(
        expr: &HirExpr,
        fresh: &crate::return_provenance::FreshOwnerVerdicts,
        params: &HashSet<BindingId>,
        proven_foreign: &HashSet<BindingId>,
    ) -> bool {
        match &expr.kind {
            // ---- value-passthrough wrappers: ALL reachable values must be
            //      materialised (look THROUGH; reject any bare-binding leaf) ----
            // A block's value is its tail (statements are side-effecting only);
            // peel to the tail regardless of statement count.
            HirExprKind::Block(block) => block.tail.as_deref().is_some_and(|t| {
                Self::expr_is_materialized_owner(t, fresh, params, proven_foreign)
            }),
            // BOTH `if` branches must be materialised. A missing `else` cannot
            // produce an owned-record value, so it fails closed.
            HirExprKind::If {
                then_expr,
                else_expr,
                ..
            } => {
                Self::expr_is_materialized_owner(then_expr, fresh, params, proven_foreign)
                    && else_expr.as_deref().is_some_and(|e| {
                        Self::expr_is_materialized_owner(e, fresh, params, proven_foreign)
                    })
            }
            // EVERY `match` arm body must be materialised (an arm body that is a
            // bare payload binding aliases the scrutinee and fails closed).
            HirExprKind::Match { arms, .. } => {
                !arms.is_empty()
                    && arms.iter().all(|arm| {
                        Self::expr_is_materialized_owner(&arm.body, fresh, params, proven_foreign)
                    })
            }
            // ---- materialised leaves ----
            // A `.clone()` result is a fresh deep copy materialised into its own
            // slot — unconditionally an owner.
            HirExprKind::RecordCloneCall { .. } => true,
            // A free-function call is a materialised owner ONLY when the module
            // freshness AUTHORITY proves the callee returns a fresh owner on
            // every path. A blanket `Call => true` is UNSOUND twice over: a
            // function can launder a by-value heap parameter (a BORROW) through
            // its return without a refcount bump (`fn id(p: Inner) -> Inner { p }`),
            // so `..id(o.inner)` would free the caller's live `o.inner` at the
            // override-drop (the call-returns-borrowed-param use-after-free);
            // and a function can launder an ownership-OPAQUE extern's return
            // (`fn wrap() -> Record { unsafe { host() } }`), so
            // `v.push(wrap())` would route to `hew_vec_push_owned_move` and the
            // Vec's teardown would release a foreign host handle. The authority
            // vetoes both — the wrapper by its taint row (transitively, through
            // any number of Hew frames) and a DIRECT extern callee by name.
            // Method-call variants can likewise return borrowed `self`/params
            // and are not summarised — they fall to the fail-closed `_` arm.
            HirExprKind::Call { callee, .. } => callee_returns_fresh_owner(callee, fresh),
            // A `Vec<T>` element load / slice — an independent heap element
            // (see the push-clone + refcount note above), not an interior alias
            // of a surviving named binding.
            HirExprKind::Index { .. } | HirExprKind::Slice { .. } => true,
            // A record/tuple/enum literal AND a functional-update result
            // (`Record { f: .. }`, `Record { ..base, f: new }`) — construction
            // writes a FRESH record into its own storage. A field operand that is
            // a PROJECTION (`o.inner.label`), a bare LOCAL (moved-in), a CALL
            // result, or a `.clone()` is refcount-bumped / COW-copied / consumed
            // into the new slot — empirically owner-preserving (a destructive
            // funcupdate over such a record does not dangle the source). The ONE
            // exception is a WHOLE by-value PARAMETER operand. Non-string heap
            // parameters are borrowed aliases stored without a clone; string
            // parameters carry a retained share but remain intentionally outside
            // the destructive/MOVE-owner route. Reject both embed classes here.
            // A nested `..base` is checked too.
            //
            // The SECOND exception is an ownership-opaque FOREIGN operand
            // (`Holder { .. }` built around `unsafe { host_record() }`, or a
            // wrapper of one). Freshness of the container says nothing about it:
            // the move-in route byte-transfers the whole tree into the
            // collection slot, whose teardown then releases a handle the host
            // still owns. The authority's composite query answers that, and it
            // is asked FIRST because it is the type-agnostic veto.
            HirExprKind::StructInit { .. } => {
                fresh.value_is_free_of_opaque_foreign_provenance(expr)
                    && !crate::return_provenance::value_reads_a_proven_foreign_binding(
                        expr,
                        proven_foreign,
                        fresh.declared_release_types(),
                    )
                    && Self::classify_whole_param_embeds(
                        expr,
                        params,
                        &HashSet::new(),
                        &ResolvedTy::clone,
                        &|ty, _| matches!(ty, ResolvedTy::String),
                        false,
                        &|_| false,
                    ) == WholeParamEmbedClass::None
            }
            HirExprKind::TupleLiteral { .. } | HirExprKind::MachineVariantCtor { .. } => {
                fresh.value_is_free_of_opaque_foreign_provenance(expr)
                    && !crate::return_provenance::value_reads_a_proven_foreign_binding(
                        expr,
                        proven_foreign,
                        fresh.declared_release_types(),
                    )
                    && Self::classify_whole_param_embeds(
                        expr,
                        params,
                        &HashSet::new(),
                        &ResolvedTy::clone,
                        &|ty, _| matches!(ty, ResolvedTy::String),
                        false,
                        &|_| false,
                    ) == WholeParamEmbedClass::None
            }
            // A projection is materialised iff its object chain is.
            HirExprKind::FieldAccess { object, .. } => {
                Self::expr_is_materialized_owner(object, fresh, params, proven_foreign)
            }
            HirExprKind::TupleIndex { tuple, .. } => {
                Self::expr_is_materialized_owner(tuple, fresh, params, proven_foreign)
            }
            // Bare/`Const` binding ref, machine-state field projection, deref, a
            // method call (can return borrowed `self`/param), or any future
            // expression form — not provably a materialised owner. Fail closed.
            _ => false,
        }
    }

    /// Whether an owned-Vec element rvalue can transfer its one live carrier
    /// into the descriptor slot. Ordinary materialised owners use the shared
    /// freshness proof above. A constructor that embeds an affine owned call
    /// carrier or a registered carrier member is also a move source: field
    /// lowering has already transferred that value into the fresh aggregate
    /// and neutralized its carrier slot, so COPY-IN would manufacture a second
    /// owner and leave structural cleanup to release the first one.
    pub(crate) fn expr_is_owned_vec_move_ingress_owner(&self, expr: &HirExpr) -> bool {
        if Self::expr_is_materialized_owner(
            expr,
            &self.call_scrutinee_provenance.fresh_owner_verdicts,
            &self.funcupdate_param_ids,
            &self.proven_foreign_bindings,
        ) {
            return true;
        }

        matches!(
            &expr.kind,
            HirExprKind::StructInit { .. }
                | HirExprKind::TupleLiteral { .. }
                | HirExprKind::MachineVariantCtor { .. }
        ) && self
            .call_scrutinee_provenance
            .fresh_owner_verdicts
            .value_is_free_of_opaque_foreign_provenance(expr)
            && !crate::return_provenance::value_reads_a_proven_foreign_binding(
                expr,
                &self.proven_foreign_bindings,
                self.call_scrutinee_provenance
                    .fresh_owner_verdicts
                    .declared_release_types(),
            )
            && Self::classify_whole_param_embeds(
                expr,
                &self.funcupdate_param_ids,
                &self.owned_carrier_param_ids,
                &|ty| self.subst_ty(ty),
                &|ty, is_carrier| {
                    is_carrier
                        && matches!(
                            ValueClass::of_ty(ty, &self.type_classes),
                            ValueClass::AffineResource | ValueClass::Linear
                        )
                },
                true,
                &|ty| {
                    crate::model::ty_owns_heap_mir(
                        ty,
                        &self.record_field_orders,
                        &self.enum_layouts,
                    )
                },
            ) == WholeParamEmbedClass::IndependentlyOwnedOnly
    }

    /// Classify WHOLE by-value parameter embeds through constructors.
    ///
    /// Recurses only through constructions (struct / tuple / machine-variant
    /// literals), which embed operands by value. For the existing materialised-
    /// owner route, other leaves stop the recursion. The Vec COPY-IN mint uses
    /// the stricter mode: every non-constructor heap-owning leaf fails closed,
    /// because a projection or unproven call result can carry an unretained alias
    /// derived from another parameter. The MOVE ingress mode additionally
    /// rejects a whole retained string and permits a whole registered carrier
    /// only when its root is affine. Heap-owning record parameters keep the
    /// prepared COPY-IN owner; registered member projections remain eligible.
    fn classify_whole_param_embeds(
        expr: &HirExpr,
        params: &HashSet<BindingId>,
        owned_carrier_params: &HashSet<BindingId>,
        resolve_ty: &impl Fn(&ResolvedTy) -> ResolvedTy,
        param_leaf_is_independent: &impl Fn(&ResolvedTy, bool) -> bool,
        reject_unproven_owned_leaves: bool,
        owns_heap: &impl Fn(&ResolvedTy) -> bool,
    ) -> WholeParamEmbedClass {
        match &expr.kind {
            HirExprKind::BindingRef {
                resolved: ResolvedRef::Binding(id),
                ..
            } if params.contains(id) => {
                let ty = resolve_ty(&expr.ty);
                if param_leaf_is_independent(&ty, owned_carrier_params.contains(id)) {
                    WholeParamEmbedClass::IndependentlyOwnedOnly
                } else {
                    WholeParamEmbedClass::UnsupportedBorrowAlias
                }
            }
            HirExprKind::StructInit { fields, base, .. } => fields
                .iter()
                .map(|(_, value)| {
                    Self::classify_whole_param_embeds(
                        value,
                        params,
                        owned_carrier_params,
                        resolve_ty,
                        param_leaf_is_independent,
                        reject_unproven_owned_leaves,
                        owns_heap,
                    )
                })
                .chain(base.iter().map(|value| {
                    Self::classify_whole_param_embeds(
                        value,
                        params,
                        owned_carrier_params,
                        resolve_ty,
                        param_leaf_is_independent,
                        reject_unproven_owned_leaves,
                        owns_heap,
                    )
                }))
                .fold(WholeParamEmbedClass::None, WholeParamEmbedClass::merge),
            HirExprKind::TupleLiteral { elements } => elements
                .iter()
                .map(|value| {
                    Self::classify_whole_param_embeds(
                        value,
                        params,
                        owned_carrier_params,
                        resolve_ty,
                        param_leaf_is_independent,
                        reject_unproven_owned_leaves,
                        owns_heap,
                    )
                })
                .fold(WholeParamEmbedClass::None, WholeParamEmbedClass::merge),
            HirExprKind::MachineVariantCtor { payload, .. } => payload
                .iter()
                .flatten()
                .map(|(_, value)| {
                    Self::classify_whole_param_embeds(
                        value,
                        params,
                        owned_carrier_params,
                        resolve_ty,
                        param_leaf_is_independent,
                        reject_unproven_owned_leaves,
                        owns_heap,
                    )
                })
                .fold(WholeParamEmbedClass::None, WholeParamEmbedClass::merge),
            HirExprKind::FieldAccess { object, .. }
            | HirExprKind::Index {
                container: object, ..
            }
            | HirExprKind::Slice {
                container: object, ..
            } if Self::projection_root_binding(object)
                .is_some_and(|id| owned_carrier_params.contains(&id)) =>
            {
                WholeParamEmbedClass::IndependentlyOwnedOnly
            }
            HirExprKind::TupleIndex { tuple, .. }
                if Self::projection_root_binding(tuple)
                    .is_some_and(|id| owned_carrier_params.contains(&id)) =>
            {
                WholeParamEmbedClass::IndependentlyOwnedOnly
            }
            _ if reject_unproven_owned_leaves && owns_heap(&resolve_ty(&expr.ty)) => {
                WholeParamEmbedClass::UnsupportedBorrowAlias
            }
            _ => WholeParamEmbedClass::None,
        }
    }

    fn projection_root_binding(expr: &HirExpr) -> Option<BindingId> {
        match &expr.kind {
            HirExprKind::BindingRef {
                resolved: ResolvedRef::Binding(id),
                ..
            } => Some(*id),
            HirExprKind::FieldAccess { object, .. } => Self::projection_root_binding(object),
            HirExprKind::TupleIndex { tuple, .. } => Self::projection_root_binding(tuple),
            HirExprKind::Index { container, .. } | HirExprKind::Slice { container, .. } => {
                Self::projection_root_binding(container)
            }
            _ => None,
        }
    }
    /// Emit a `MirStatement::Use(Consume)` for a managed-type binding that is
    /// moved into a builtin aggregate method (HashMap/HashSet insert).
    ///
    /// WHY: A vacant `HashMap.insert` / `HashSet.insert` call moves the
    /// caller's key into the slot (`!existed` path in the runtime). Without
    /// this consume the scope-exit drop would run *after* the runtime has
    /// already taken ownership — a double-free. The static consume suppresses
    /// that scope-exit drop, making the vacant-insert path sound.
    ///
    /// On the OVERWRITE path (`existed`) the runtime keeps the stored key and
    /// the caller's duplicate is NOT consumed by the runtime. The static consume
    /// above still correctly suppresses the scope-exit drop; the overwrite-path
    /// release is materialised in codegen
    /// (`emit_insert_overwrite_key_release` in `hew-codegen-rs/src/llvm.rs`),
    /// which branches on the insert's `i1` return and frees the caller's
    /// duplicate exactly on the overwrite path. So this consume pairs with that
    /// conditional release: the key is freed exactly once on either path —
    /// vacant by the map, overwrite by the codegen release — never both, never
    /// leaked (issue #2033 — see the conditional-key-consume contract comment in
    /// `hew-runtime/src/hashmap.rs`).
    /// Refuse a `HashMap`/`HashSet` MOVE-ingress operand whose provenance is not
    /// proven free of an ownership-opaque foreign producer. Returns `true` when a
    /// diagnostic was pushed and the caller must abandon the lowering.
    ///
    /// The collection's ingress is MOVE by ABI (`hew_hashmap_insert_layout`
    /// documents copy-in as intentionally absent), so the compiler schedules a
    /// release of whatever is stored, through the value layout's `drop_fn`, at
    /// the collection's teardown. For a foreign value that release is a release
    /// of a handle the caller never owned — the same class as the caller-side
    /// mints, arriving through the collection instead of through a local.
    ///
    /// There is no COPY-IN sibling to fall back to, so "fail closed" cannot mean
    /// "mint nothing": the move is the only ingress the ABI has. It therefore
    /// means refusing the ingress — one clean `NotYetImplemented`, no partial
    /// MIR, exactly the posture the #2648 call-scrutinee reject takes.
    ///
    /// Scalars are exempt: with no heap to release, the teardown's `drop_fn`
    /// has nothing to free and the provenance question does not arise.
    pub(crate) fn reject_opaque_foreign_collection_ingress(&mut self, operand: &HirExpr) -> bool {
        let ty = self.subst_ty(&operand.ty);
        if !crate::model::ty_owns_heap_mir(&ty, &self.record_field_orders, &self.enum_layouts) {
            return false;
        }
        if self.value_is_free_of_opaque_foreign_provenance(operand) {
            return false;
        }
        self.diagnostics.push(MirDiagnostic {
            kind: MirDiagnosticKind::NotYetImplemented {
                construct: "collection ingress of a value with ownership-opaque provenance"
                    .to_string(),
                site: operand.site,
            },
            note: format!(
                "a `HashMap`/`HashSet` takes ownership of what it is given (its ingress is a \
                 move, with no copy-in), so the collection's teardown releases this `{}`. Its \
                 provenance is not proven free of an ownership-opaque producer — a declared \
                 `extern` return, a Hew function that forwards one, or an indirect/closure \
                 callee whose body is not statically in hand — so that release could free a \
                 handle this program never owned. Build the element from values this module \
                 owns, or clone it explicitly before inserting it.",
                ty.user_facing()
            ),
        });
        true
    }

    /// True when a `let` binder must NOT be registered as a scope-exit owner
    /// because its initializer provably hands back a handle a declared,
    /// non-audited extern produced.
    ///
    /// The seed gate this guards (`binding_seeds_drop_elaboration`) is purely
    /// TYPE-driven — "this type is not `BitCopy`, therefore drop it at scope
    /// exit" — so `let h = unsafe { host_record() }` earned a `RecordInPlace`
    /// release of the host's `Holder` with the initializer's provenance never
    /// consulted. That is the same defect class as the container mints, one
    /// construct over.
    ///
    /// # Why this reads the PROVEN query, not the strict one
    ///
    /// This is the only ownership decision in the lowering whose fail-closed
    /// direction is to REMOVE a release rather than to withhold one. Reading the
    /// strict `value_is_free_of_opaque_foreign_provenance` here would drop the
    /// scope-exit release of every binding whose initializer reaches an indirect
    /// or unanalysed callee — a leak in ordinary code that never touches an
    /// extern. So the veto requires proof, and the audited extern table is the
    /// only thing that can supply it.
    ///
    /// # The `string` carve-out is the ratified adoption ABI
    ///
    /// A root `extern "C" -> string` declared `ForeignAdopt` is ADOPTED at the
    /// call edge: codegen copies the foreign C string into a refcounted Hew
    /// buffer and `free`s the raw pointer, so the binding holds a value this
    /// program really does own and its release must survive. Adoption is defined
    /// at `return_ty == String` and nowhere else — it does not reach a `string`
    /// FIELD of a returned record, nor a `string` inside a returned
    /// `Option`/`Result` — so carving out exactly the `string`-typed binding
    /// matches the contract's domain precisely.
    pub(crate) fn let_binder_owns_proven_foreign_value(
        &self,
        value: &HirExpr,
        binding_ty: &ResolvedTy,
    ) -> bool {
        if matches!(binding_ty, ResolvedTy::String) {
            return false;
        }
        if !crate::model::ty_owns_heap_mir(
            binding_ty,
            &self.record_field_orders,
            &self.enum_layouts,
        ) {
            return false;
        }
        self.call_scrutinee_provenance
            .fresh_owner_verdicts
            .value_carries_proven_foreign_provenance(value)
            || crate::return_provenance::value_reads_a_proven_foreign_binding(
                value,
                &self.proven_foreign_bindings,
                self.call_scrutinee_provenance
                    .fresh_owner_verdicts
                    .declared_release_types(),
            )
    }

    /// [`Self::let_binder_owns_proven_foreign_value`], recording the answer in
    /// this function's proven-foreign ledger so the same fact travels with the
    /// binding into every container it is later embedded in.
    ///
    /// Recording at the point of decision, rather than re-deriving it at each
    /// container mint, is what keeps the two sites from drifting: there is one
    /// place that decides a binder is foreign, and one ledger that says so.
    pub(crate) fn note_let_binder_proven_foreign(
        &mut self,
        binding: BindingId,
        value: &HirExpr,
        binding_ty: &ResolvedTy,
    ) -> bool {
        if !self.let_binder_owns_proven_foreign_value(value, binding_ty) {
            return false;
        }
        self.proven_foreign_bindings.insert(binding);
        true
    }

    /// The payload-binder twin of [`Self::note_let_binder_proven_foreign`]:
    /// decide whether a binder projected OUT of `scrutinee` must be refused a
    /// scope-exit owner, and record the answer in the ledger.
    ///
    /// `match` / `if let` / `while let` / `let else` payload binders at every
    /// nesting depth, and the `Binding` arm predicate, all bind a field of the
    /// scrutinee, so the provenance question is the SCRUTINEE's. It is put to
    /// the same proven-foreign query and the same ledger the `let` binder reads,
    /// with the same polarity: withholding requires proof.
    ///
    /// # No `string` carve-out here, deliberately
    ///
    /// The `let` binder exempts `ResolvedTy::String` because a root
    /// `extern "C" -> string` is ADOPTED at the call edge into a refcounted Hew
    /// buffer, so that binding really owns its value. Adoption is defined at
    /// `return_ty == String` and NOWHERE else — not for a `string` field of a
    /// returned record, not for a `string` inside a returned `Option`/`Result`.
    /// A payload binder is exactly those un-adopted positions: the `string` it
    /// binds is a pointer the host still owns. Importing the carve-out here
    /// would mint precisely the release the adoption ABI does not back.
    pub(crate) fn note_payload_binder_proven_foreign(
        &mut self,
        binding: BindingId,
        scrutinee: &HirExpr,
        binding_ty: &ResolvedTy,
    ) -> bool {
        if !crate::model::ty_owns_heap_mir(
            binding_ty,
            &self.record_field_orders,
            &self.enum_layouts,
        ) {
            return false;
        }
        let foreign = self
            .call_scrutinee_provenance
            .fresh_owner_verdicts
            .value_carries_proven_foreign_provenance(scrutinee)
            || crate::return_provenance::value_reads_a_proven_foreign_binding(
                scrutinee,
                &self.proven_foreign_bindings,
                self.call_scrutinee_provenance
                    .fresh_owner_verdicts
                    .declared_release_types(),
            );
        if foreign {
            self.proven_foreign_bindings.insert(binding);
        }
        foreign
    }

    /// The rebind twin: a binding the ledger already refused an owner for must
    /// not acquire one by being rebound or restored, and the fact propagates
    /// onto the new binding so a chain of rebinds cannot launder it.
    pub(crate) fn note_rebind_proven_foreign(
        &mut self,
        binding: BindingId,
        source: BindingId,
        binding_ty: &ResolvedTy,
    ) -> bool {
        if !crate::model::ty_owns_heap_mir(
            binding_ty,
            &self.record_field_orders,
            &self.enum_layouts,
        ) {
            return false;
        }
        if !self.proven_foreign_bindings.contains(&source) {
            return false;
        }
        self.proven_foreign_bindings.insert(binding);
        true
    }

    /// Refuse a call that MOVES a proven-foreign value into a position whose
    /// release obligation this frame hands to someone else.
    ///
    /// # Why this exists at the caller
    ///
    /// A callee-owned parameter — a `consume` `#[resource]` parameter, an
    /// owned-carrier parameter, a CONSUME-classified heap-owning enum
    /// parameter, an actor-handler message parameter — is minted a scope-exit
    /// owner inside the CALLEE, from the parameter's type. That frame cannot ask
    /// the provenance question: there is no HIR expression for a parameter's
    /// value, and the per-function proven-foreign ledger is populated only by
    /// binder seams in the body, which lower strictly after `lower_params`, so a
    /// parameter binding is provably absent from it. Answering "unknown ⇒ no
    /// mint" there would withhold every callee-side parameter release in the
    /// language — every `consume` parameter and every actor message would leak.
    ///
    /// So the question is asked HERE, where the argument is an expression this
    /// module can walk, and the answer is enforced by refusing the transfer.
    /// With the transfer refused, the callee's type-driven mint is provably
    /// never a mint over a proven-foreign value — which is what makes
    /// [`Builder::owner_warrant_for_owned_parameter`] sound rather than merely
    /// unfalsified.
    ///
    /// # Why the PROVEN query, and what residual that leaves
    ///
    /// This consumer REJECTS a program. Reading the strict mint-side query would
    /// reject every call that forwards a value produced by an indirect or
    /// unanalysed callee into a `consume` parameter — ordinary code that never
    /// touches an extern. The threshold is therefore proof, exactly as at the
    /// `let` binder, and the residual is the same one: an indirect callee whose
    /// return really is foreign is not seen. That residual is finding U12 and is
    /// closed only by making indirect calls resolvable.
    ///
    /// Returns `true` when a diagnostic was pushed. Scalars are exempt: with no
    /// heap to release the callee's mint frees nothing.
    pub(crate) fn reject_opaque_foreign_ownership_transfer(
        &mut self,
        arg: &HirExpr,
        sink: &str,
    ) -> bool {
        let ty = self.subst_ty(&arg.ty);
        if !crate::model::ty_owns_heap_mir(&ty, &self.record_field_orders, &self.enum_layouts) {
            return false;
        }
        let foreign = self
            .call_scrutinee_provenance
            .fresh_owner_verdicts
            .value_carries_proven_foreign_provenance(arg)
            || crate::return_provenance::value_reads_a_proven_foreign_binding(
                arg,
                &self.proven_foreign_bindings,
                self.call_scrutinee_provenance
                    .fresh_owner_verdicts
                    .declared_release_types(),
            );
        if !foreign {
            return false;
        }
        self.diagnostics.push(MirDiagnostic {
            kind: MirDiagnosticKind::NotYetImplemented {
                construct: format!("ownership transfer of a proven-foreign value into {sink}"),
                site: arg.site,
            },
            note: format!(
                "{sink} takes ownership of this `{}`, so the release obligation leaves this \
                 frame and the receiving side mints its own scope-exit drop from the \
                 parameter's type. This value provably carries a handle a declared, \
                 non-audited `extern` produced, so that drop would free a handle this program \
                 never owned. Build the value from values this module owns, or clone it \
                 explicitly before handing it over.",
                ty.user_facing()
            ),
        });
        true
    }

    /// The whole argument list of a direct call, checked against this module's
    /// own parameter-ownership tables. Returns `true` when any argument was
    /// refused and the caller must abandon the lowering.
    ///
    /// # The predicate MIRRORS `lower_params`, it does not approximate it
    ///
    /// The only thing this function needs to detect is "the callee will mint a
    /// scope-exit owner over this parameter". `lower_params` has exactly six
    /// such mints and this is the free-call half of three of them, condition
    /// for condition:
    ///
    /// * `param_consume == Some(true)` — the affine `#[resource]` CONSUME
    ///   parameter, minted at any type;
    /// * `call_param_owned_carrier == Some(true)`, minus the two type classes
    ///   `register_owned_call_carrier_param` excludes up front (indirect enums
    ///   and machines, which carry their own move/drop authority);
    /// * `call_param_owned_carrier == Some(true)` **conjoined with**
    ///   `ty_is_heap_owning_enum_composite` — the enum-composite callee drop
    ///   for a summary-owned parameter the carrier protocol could not admit
    ///   (already covered by the bullet above).
    ///
    /// The remaining three mints are `ActorHandler`-convention only, so their
    /// caller is the mailbox hand-off in `lower_actor_send`
    /// (`actor_handler_mints_an_owner_for_message`), not a direct call: the
    /// #2747 owned-aggregate record message, the indirect-enum message, and
    /// the bare `bytes` message (the copy-mode mailbox transfers its one
    /// refcount into the delivered `BytesTriple`, so `lower_params` registers
    /// a scope-exit owner for it exactly like the other two).
    ///
    /// The body-escape bit is a summary, not a mint predicate:
    /// the `string::fmt` display shim carries CONSUME on its `string`
    /// parameter and mints nothing at all. Reading that summary here once
    /// refused `println(f"…{h.label}…")` for every proven-foreign `h` — a
    /// program with no double release in it. Refusing where the callee does
    /// not mint is not "fail closed", it is a false rejection, so the
    /// caller-side question is asked at precisely the callee-side mint
    /// conditions and nowhere else.
    pub(crate) fn reject_opaque_foreign_call_arg_transfers(
        &mut self,
        callee_item: Option<hew_hir::ItemId>,
        hir_args: &[HirExpr],
    ) -> bool {
        let Some(callee_item) = callee_item else {
            return false;
        };
        let mut refused = false;
        for (index, arg) in hir_args.iter().enumerate() {
            if !self.callee_mints_an_owner_for_param(callee_item, index, arg) {
                continue;
            }
            refused |=
                self.reject_opaque_foreign_ownership_transfer(arg, "a callee-owned parameter");
        }
        refused
    }

    /// True iff `lower_params` will register a scope-exit owner for parameter
    /// `index` of `callee_item` when that callee is lowered. See
    /// [`Builder::reject_opaque_foreign_call_arg_transfers`] for the mapping
    /// onto the four `lower_params` mint sites.
    fn callee_mints_an_owner_for_param(
        &self,
        callee_item: hew_hir::ItemId,
        index: usize,
        arg: &HirExpr,
    ) -> bool {
        let key = (callee_item, index);
        // Arm 1 — the affine `#[resource]` CONSUME parameter. Minted at any
        // type, and it short-circuits both arms below exactly as
        // `!param_is_consumed` does in `lower_params`.
        if self.param_ownership.param_consume.get(&key).copied() == Some(true) {
            return true;
        }
        let ty = self.subst_ty(&arg.ty);
        // Arm 2 — the owned-call-carrier parameter, up to the two type classes
        // `register_owned_call_carrier_param` excludes before it even consults
        // the snapshot plan.
        let carrier = self
            .param_ownership
            .call_param_owned_carrier
            .get(&key)
            .copied()
            == Some(true)
            && !crate::lower::drop_plan::ty_is_indirect_enum(&ty, &self.enum_layouts)
            && !self.ty_is_machine(&ty);
        // The enum-composite callee drop is gated on the same carrier summary
        // and adds no mint the carrier arm does not already report.
        carrier
    }

    /// The ONE composite-provenance query every mint site in this builder asks.
    ///
    /// It is the module authority's
    /// [`FreshOwnerVerdicts::value_is_free_of_opaque_foreign_provenance`]
    /// conjoined with this function's `proven_foreign_bindings` ledger. The
    /// authority walks an expression's own structure and treats a `BindingRef`
    /// as a leaf, so on its own it cannot see a foreign handle that reached the
    /// container through a `let`:
    ///
    /// ```hew
    /// let h = unsafe { host_record() };
    /// borrowOuter(Outer { inner: h, tag: 0 });
    /// ```
    ///
    /// The ledger closes exactly that: the `let` refused `h` a scope-exit owner
    /// because its initializer was proven foreign, and that same fact now
    /// travels with `h` into every container it is embedded in. Conjunction, not
    /// replacement — neither half can license a mint the other denies.
    pub(crate) fn value_is_free_of_opaque_foreign_provenance(&self, expr: &HirExpr) -> bool {
        self.call_scrutinee_provenance
            .fresh_owner_verdicts
            .value_is_free_of_opaque_foreign_provenance(expr)
            && !self.expr_reads_a_proven_foreign_binding(expr)
    }

    /// True when `expr` reads any binding this function refused a scope-exit
    /// owner for because its initializer was proven foreign.
    ///
    /// Structural and conservative: it runs the authority's own walk under a
    /// policy that carries only the ledger, so it visits exactly the value
    /// positions the authority does, and answering `true` only ever WITHHOLDS a
    /// mint.
    pub(crate) fn expr_reads_a_proven_foreign_binding(&self, expr: &HirExpr) -> bool {
        crate::return_provenance::value_reads_a_proven_foreign_binding(
            expr,
            &self.proven_foreign_bindings,
            self.call_scrutinee_provenance
                .fresh_owner_verdicts
                .declared_release_types(),
        )
    }
    /// Retain a `CoW`-carrier (`string`/`bytes`) operand entering a
    /// `HashMap`/`HashSet` MOVE ingress when this frame does not own it.
    /// Returns `true` when the retain was emitted and the caller must NOT
    /// record a consume.
    ///
    /// WHY: `string` and `bytes` ride the `CoW` borrow spine, so a by-value
    /// parameter of either type is BORROWED — `lower_params` excludes both
    /// from `param_summary_owned`, mints no callee-side owner, and the CALLER
    /// keeps the count and drops it at its own scope exit. The map's ingress
    /// is a MOVE, so the slot becomes a second owner of that same count: the
    /// map's teardown and the caller's scope exit both release it. The local
    /// static consume cannot suppress the caller's drop — it is in another
    /// frame — so the consume alone is not a sound pairing for a borrowed
    /// operand.
    ///
    /// A retain makes the ingress balanced: `+1` before the move, so the map
    /// owns the new count and the caller still owns its own. It mirrors what
    /// `let k2 = k; m.insert(k2, ..)` already emits for the same value
    /// (the `mir_share` retain on a borrowed-param share) and what
    /// `hew_vec_push_str` does at the `Vec<string>` seam, where the caller
    /// likewise keeps its drop obligation.
    ///
    /// The gate is the STRUCTURAL caller-borrowed verdict, not a per-carrier
    /// taught list: `borrowed_value_param_locals` is seeded from the final
    /// `callee_owns_param` answer in `lower_params`, so a consume-classified /
    /// owned-carrier / actor-handler parameter keeps its consume. The gate is
    /// also GENERATION-sensitive: reassigning the parameter binds a fresh
    /// frame-owned value into the slot, and `assign` deregisters the local
    /// from the borrowed registries at that boundary
    /// (`deregister_reassigned_borrowed_param`), so the new generation is
    /// consumed (transferred into the collection) rather than
    /// retained-and-leaked.
    ///
    /// The codegen overwrite-path release (`emit_insert_overwrite_key_release`)
    /// stays correct under this pairing: on the vacant path the map owns the
    /// retained count, on the overwrite path the release consumes it. Exactly
    /// one release either way, and the caller's own count is untouched.
    pub(crate) fn retain_caller_borrowed_cow_collection_ingress(
        &mut self,
        operand: &HirExpr,
        place: Place,
    ) -> bool {
        let ty = self.subst_ty(&operand.ty);
        if !matches!(ty, ResolvedTy::String | ResolvedTy::Bytes) {
            return false;
        }
        let Some(local) = base_local(place) else {
            return false;
        };
        if !self.borrowed_value_param_locals.contains(&local) {
            return false;
        }
        match ty {
            ResolvedTy::String => self.push_instr(Instr::StringRetain {
                value: place,
                condition: crate::model::StringRetainCondition::Always,
            }),
            ResolvedTy::Bytes => self.push_instr(Instr::BytesRetain { value: place }),
            _ => unreachable!("gated on String | Bytes above"),
        }
        true
    }

    /// The union of every binder-dest local class that receives a FRESH,
    /// independently-owned value out of a variant slot: the moved-out
    /// call-carrier payload binders (`fresh_variant_payload_binder_locals`)
    /// and the yield/recv payload binders (`yield_binder_locals`). Both
    /// classes answer the projection-alias question the same way — the dest
    /// is a genuine owner, not an interior alias of a still-live aggregate —
    /// so the `CoW` sole-owner derivation exempts both from its taint seed.
    pub(crate) fn fresh_owner_dest_locals(&self) -> HashSet<u32> {
        self.fresh_variant_payload_binder_locals
            .union(&self.yield_binder_locals)
            .copied()
            .collect()
    }

    /// True when `place` is the slot of a yield/recv payload binder whose
    /// consuming body is currently being lowered (`active_generator_yield_values`
    /// holds an entry between binder registration and body end).
    pub(crate) fn active_yield_binder_place(&self, place: Place) -> bool {
        self.active_generator_yield_values
            .iter()
            .any(|(_, entry_place, ..)| *entry_place == place)
    }

    /// Retain a `CoW`-carrier (`string`/`bytes`) ACTIVE yield-binder operand
    /// entering a `HashMap`/`HashSet` MOVE ingress. Returns `true` when the
    /// retain was emitted and the caller must NOT record a consume; the
    /// retained local is pushed so the ingress `Call` terminator can be
    /// registered in `yield_share_term_exempt`.
    ///
    /// The binder class answers "which frame owns this binding" differently
    /// from both an owned local and a borrowed parameter: the VALUE is
    /// frame-owned (a fresh per-iteration count from the clone-out / recv),
    /// but its release authority is the per-iteration BODY-END drop, not the
    /// function-scope owner. A static `Consume` here suppresses no local
    /// scope-exit drop; it ends the binder's generation on one path only, and
    /// the body-end drop is separately suppressed by the escape scan, so the
    /// replay-derived edge release is the only thing standing between the
    /// not-taken path and a leaked count.
    ///
    /// The retain pairing keeps every count balanced with no path
    /// ambiguity: clone `+1` (binder), retain `+1` (collection's own count),
    /// body-end drop `-1`, collection teardown `-1`. The binder stays live
    /// on every path, so no edge release is needed and the cancel/panic exit
    /// drops keep firing.
    pub(crate) fn retain_yield_binder_cow_collection_ingress(
        &mut self,
        operand: &HirExpr,
        place: Place,
        retained_locals: &mut Vec<u32>,
    ) -> bool {
        let ty = self.subst_ty(&operand.ty);
        if !matches!(ty, ResolvedTy::String | ResolvedTy::Bytes) {
            return false;
        }
        let Some(local) = base_local(place) else {
            return false;
        };
        if !self.active_yield_binder_place(place) {
            return false;
        }
        match ty {
            ResolvedTy::String => self.push_instr(Instr::StringRetain {
                value: place,
                condition: crate::model::StringRetainCondition::Always,
            }),
            ResolvedTy::Bytes => self.push_instr(Instr::BytesRetain { value: place }),
            _ => unreachable!("gated on String | Bytes above"),
        }
        retained_locals.push(local);
        true
    }

    /// Reassignment is a generation boundary for a caller-borrowed parameter
    /// slot: after `key = <rhs>` the local holds a fresh value this FRAME
    /// owns, so every "this slot holds the caller's value" registry must stop
    /// answering for it. Leaving the registration in place is a
    /// generation-insensitivity leak: the ingress retain would mint a `+1` on
    /// a frame-owned value whose last count nothing ever releases.
    ///
    /// Called only for an assignment lowered in the function's TOP-LEVEL body
    /// scope — a straight-line reassignment that dominates every later-lowered
    /// use. A conditional reassignment (inside an `if` arm / loop body) keeps
    /// the borrowed registration: on the not-reassigned path the slot still
    /// holds the caller's value, and consuming that would double-free. The
    /// fail-closed direction is retain-and-leak on the reassigned path, never
    /// a double release.
    pub(crate) fn deregister_reassigned_borrowed_param(&mut self, binding: BindingId) {
        let Some(local) = self
            .binding_locals
            .get(&binding)
            .copied()
            .and_then(base_local)
        else {
            return;
        };
        self.borrowed_value_param_locals.remove(&local);
        self.borrowed_string_param_locals.remove(&local);
        self.borrowed_bytes_param_locals.remove(&local);
    }

    pub(crate) fn consume_moved_builtin_method_arg(&mut self, operand: &HirExpr) {
        let HirExprKind::BindingRef {
            resolved: ResolvedRef::Binding(id),
            ..
        } = &operand.kind
        else {
            return;
        };
        if self.binding_ref_use_intent(operand) == IntentKind::Consume {
            return;
        }
        let ty = self.subst_ty(&operand.ty);
        if !self.aggregate_ingress_moves_binding_ty(&ty) {
            return;
        }
        // The physical transfer commits in the runtime call's normal successor
        // (see `splice_normal_call_ownership_commits`); retire only the mutable
        // lowering ledger here so no pre-call event can suppress unwind cleanup.
        self.set_owned_local_consumed_post_lowering(*id, None, DischargeSite::CallArgumentTransfer);
    }

    /// Commit a directly bound array-literal element to the Vec owned-move ABI.
    ///
    /// `hew_vec_push_owned_move` transfers the element bytes into the
    /// descriptor-owned slot without a clone. This is unlike an ordinary
    /// array-literal aggregate alias: its source becomes consumed only in the
    /// call's normal successor, where the Vec's element drop thunk becomes the
    /// sole close authority. The caller has already proven the synthetic array
    /// receiver, exact known move symbol, and concrete owned-element receiver.
    pub(crate) fn consume_owned_vec_move_array_element(&mut self, operand: &HirExpr) {
        let HirExprKind::BindingRef {
            resolved: ResolvedRef::Binding(id),
            ..
        } = &operand.kind
        else {
            return;
        };
        let ty = self.subst_ty(&operand.ty);
        if !self.aggregate_ingress_moves_binding_ty(&ty) {
            return;
        }
        // The runtime adopts the element only on the call's normal edge.
        // Retire the lowering ledger now, but leave the explicit OwnerId live
        // before the terminator so its unwind cleanup still owns the source;
        // `splice_normal_call_ownership_commits` emits the terminal Transfer
        // in the unique normal successor.
        self.set_owned_local_consumed_post_lowering(*id, None, DischargeSite::CallArgumentTransfer);
    }
    /// True when a `Let` RHS produces a named-function pair whose `env_ptr`
    /// is null by construction: a direct `Item`-resolved fn reference
    /// (`let f = double;`), a rebind of an already-exempt binding, or either
    /// shape behind transparent block tails. Null-env pairs are freely
    /// byte-copyable — no environment exists to double-free — so they are
    /// exempt from the closure-pair ingress discipline.
    pub(crate) fn closure_rhs_is_null_env_pair(&self, value: &HirExpr) -> bool {
        match &value.kind {
            HirExprKind::BindingRef {
                resolved: ResolvedRef::Item(_),
                ..
            } => true,
            HirExprKind::BindingRef {
                resolved: ResolvedRef::Binding(id),
                ..
            } => self.closure_pair_null_env.contains(id),
            HirExprKind::Block(block) => block
                .tail
                .as_deref()
                .is_some_and(|tail| self.closure_rhs_is_null_env_pair(tail)),
            _ => false,
        }
    }
    /// Emit the shared fail-closed diagnostic for fn-valued arguments crossing
    /// into either a standalone or actor `gen fn` constructor. Lowering
    /// continues after the fatal diagnostic so surrounding bindings remain
    /// structurally coherent; no binary is emitted while diagnostics exist.
    pub(crate) fn reject_unproven_generator_fn_args(&mut self, args: &[HirExpr]) {
        for arg in args {
            if let Some(what) = self.generator_arg_laundered_closure(arg) {
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::NotYetImplemented {
                        construct: "a capturing closure as a generator's `fn(..)` argument"
                            .to_string(),
                        site: arg.site,
                    },
                    note: format!(
                        "{what} would cross into the generator's flat-copied env: \
                         the generator can never release a capturing closure's \
                         environment box, so every generator constructed from it \
                         would leak that box. Pass a named function or a \
                         capture-free closure directly to the generator; forwarding \
                         through a parameter, call result, pattern payload, or aggregate \
                         remains unavailable until the clone-into-env protocol \
                         (`genfn-owned-captures`) exists."
                    ),
                });
            }
        }
    }
    /// `Some(description)` when a generator-constructor call argument may carry
    /// a capturing closure behind a `fn(..)` view — the CAP-11 fail-closed
    /// gate. The generator env is a
    /// flat `memcpy` (`Terminator::MakeGenerator`'s heap-copy) that never
    /// recurses into inner pointers and the body side never drops a fn-typed
    /// capture, so a non-null env word crossing this boundary is an
    /// unreleasable heap box: every constructed generator would leak it.
    /// `None` admits the argument.
    ///
    /// Admitted (provably null-env):
    ///   * named-fn references (`Item`-resolved — env word null by
    ///     construction);
    ///   * capture-free closure literals (no env box exists);
    ///   * bindings whose producer chain contains only those null-env shapes.
    ///
    /// Rejected (fail closed):
    ///   * a closure literal with captures;
    ///   * any expression whose resolved type is a capturing `Closure`;
    ///   * fn-typed parameters and fn-valued call results, whose env provenance
    ///     is not provably null;
    ///   * fn-valued aggregate/container reads and other unproven producers;
    ///   * bindings/merges/reassignments derived from any rejected shape.
    pub(crate) fn generator_arg_laundered_closure(&self, arg: &HirExpr) -> Option<String> {
        if let HirExprKind::Block(body) = &arg.kind {
            return body
                .tail
                .as_deref()
                .and_then(|tail| self.generator_arg_laundered_closure(tail));
        }
        if let HirExprKind::Closure { captures, .. } = &arg.kind {
            if captures.is_empty() {
                return None;
            }
            let names: Vec<String> = captures.iter().map(|c| format!("`{}`", c.name)).collect();
            return Some(format!("a closure capturing {}", names.join(", ")));
        }
        if let ResolvedTy::Closure { captures, .. } = self.subst_ty(&arg.ty) {
            if !captures.is_empty() {
                return Some("a value of a capturing-closure type".to_string());
            }
        }
        if let HirExprKind::BindingRef {
            resolved: ResolvedRef::Binding(id),
            name,
        } = &arg.kind
        {
            if ty_is_closure_pair(&self.subst_ty(&arg.ty))
                && self.closure_pair_env_may_be_nonnull.contains(id)
            {
                return Some(format!(
                    "`{name}` (whose fn value may carry a heap closure environment)"
                ));
            }
        }
        self.closure_rhs_may_carry_env(arg).then(|| {
            "a fn-valued producer whose closure environment is not provably null".to_string()
        })
    }
    /// Ownership classification for a closure-pair operand entering an
    /// owning container position (record field, Vec element store, machine
    /// payload, tuple element). Mirrors `classify_closure_pair_rhs` but
    /// answers the ingress question — "may this operand's pair be stored
    /// as an owner?" — rather than the `Let` drop-admission question.
    #[allow(
        clippy::match_same_arms,
        reason = "literal, named-fn reference, and call-result arms are \
                  semantically distinct fresh-pair producers (each comment \
                  documents WHY its pair is safely owned); merging them would \
                  obscure the per-shape ownership argument"
    )]
    pub(crate) fn classify_closure_pair_ingress(&self, operand: &HirExpr) -> ClosurePairIngress {
        match &operand.kind {
            // A closure literal in an aggregate-operand position is
            // Escapes-classified by the checker (non-direct-call use), so
            // its env is heap-or-null and the aggregate becomes the sole
            // owner of a fresh pair.
            HirExprKind::Closure { .. } => ClosurePairIngress::Fresh,
            // A named function used as a value synthesises a fresh pair
            // with a null env at every use site — nothing to double-free.
            HirExprKind::BindingRef {
                resolved: ResolvedRef::Item(_),
                ..
            } => ClosurePairIngress::Fresh,
            HirExprKind::BindingRef {
                name,
                resolved: ResolvedRef::Binding(id),
            } => {
                if self.closure_pair_null_env.contains(id) {
                    ClosurePairIngress::Fresh
                } else if self.closure_pair_owned.contains(id) {
                    ClosurePairIngress::OwnedBinding {
                        id: *id,
                        name: name.clone(),
                    }
                } else if self.closure_pair_moved.contains(id) {
                    // Ownership already left via a rebind; the move checker
                    // flags this read as `UseAfterConsume` on its own.
                    ClosurePairIngress::AlreadyMoved
                } else if self.closure_pair_param_owned.contains(id) {
                    // A forwarded fn-typed parameter: its closure env is
                    // provably heap (the checker `Escapes`-classifies a closure
                    // crossing a call boundary as an argument — see
                    // `closure_pair_param_owned`), so the container may own it
                    // outright. The store is the parameter's move: the
                    // `OwnedBinding` arm emits the `AggregateAlias` consume
                    // marker, and any later read of the parameter is rejected
                    // as `UseAfterConsume`. Ordered AFTER `closure_pair_moved`
                    // so a parameter already consumed by a prior store routes
                    // to `AlreadyMoved` (no second `ClosurePairBorrowedStore`).
                    ClosurePairIngress::OwnedBinding {
                        id: *id,
                        name: name.clone(),
                    }
                } else {
                    ClosurePairIngress::Borrowed {
                        name: Some(name.clone()),
                    }
                }
            }
            // Vec element reads are borrows: the vec slot keeps ownership
            // of the element's pair box and env.
            HirExprKind::ResolvedImplCall { target_symbol, .. }
                if target_symbol == "hew_vec_get_ptr" =>
            {
                ClosurePairIngress::Borrowed { name: None }
            }
            // A fn-typed call result is a fresh owned pair (heap-or-null
            // transitively: a pair crossing a return boundary is
            // Escapes-classified at its literal site).
            HirExprKind::Call { .. }
            | HirExprKind::ResolvedImplCall { .. }
            | HirExprKind::CallTraitMethodStatic { .. }
            | HirExprKind::CallDynMethod { .. } => ClosurePairIngress::Fresh,
            HirExprKind::Block(block) => block
                .tail
                .as_deref()
                .map_or(ClosurePairIngress::Borrowed { name: None }, |tail| {
                    self.classify_closure_pair_ingress(tail)
                }),
            // Everything else — record-field reads, parameters reached via
            // shapes above, `if`/`match` merges — is a borrow or a pair
            // whose ownership the analysis cannot prove. Fail closed.
            _ => ClosurePairIngress::Borrowed { name: None },
        }
    }
    /// Sole-owner ingress gate for closure-pair operands (the affine "first
    /// use moves" discipline). An owning container position byte-copies the
    /// 16-byte `{fn_ptr, env_ptr}` pair, so admitting anything but an owned
    /// pair creates a second owner of one closure environment and a double
    /// free at scope exit:
    ///
    /// - an OWNED binding operand is moved — a checker-stream
    ///   `AggregateAlias` marks it so the dataflow rejects every later use
    ///   (`UseAfterConsume` anchored at this store; invoking before the
    ///   store stays legal — invocation is the borrow the pair exists for);
    /// - a BORROWED operand (parameter, vec-element read, record-field
    ///   read, unproven merge shape) is refused outright with
    ///   `ClosurePairBorrowedStore` — its pair is owned elsewhere, and the
    ///   store itself is the corruption, with or without a later use;
    /// - fresh pairs (literals, fn-typed call results, named-fn null-env
    ///   pairs) pass through: the container becomes their sole owner.
    pub(crate) fn enforce_closure_pair_ingress(&mut self, operand: &HirExpr) {
        let ty = self.subst_ty(&operand.ty);
        if !ty_is_closure_pair(&ty) {
            return;
        }
        match self.classify_closure_pair_ingress(operand) {
            ClosurePairIngress::Fresh | ClosurePairIngress::AlreadyMoved => {}
            ClosurePairIngress::OwnedBinding { id, name } => {
                self.statements.push(MirStatement::AggregateAlias {
                    binding: id,
                    name,
                    site: operand.site,
                    ty,
                    // Whole-value closure-pair placement: strict `(t, t)` check.
                    partial_projection: false,
                });
            }
            ClosurePairIngress::Borrowed { name } => {
                let rendered = name
                    .as_ref()
                    .map_or_else(|| "this function value".to_string(), |n| format!("`{n}`"));
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::ClosurePairBorrowedStore {
                        name,
                        site: operand.site,
                    },
                    note: format!(
                        "storing {rendered} would give the container a second owner \
                         of one closure environment: closure pairs are sole-owner \
                         values with no clone path, so only an owned pair (a closure \
                         literal, a fresh fn-typed call result, or a binding that \
                         owns its closure) may be stored"
                    ),
                });
            }
        }
    }
}

#[cfg(test)]
mod typed_produced_owner_tests {
    use std::rc::Rc;

    use super::{
        BindingId, Builder, OwnedAliasInheritance, OwnedCarrierNeutralizeTarget, OwnerMintWarrant,
        Place, PlaceProvenance, Projection, ResolvedTy, SiteId, ValueProvenance,
    };
    use crate::lower::ParamOwnershipFacts;
    use crate::{BasicBlock, DropFnSpec, Instr, MirStatement, Terminator};
    use hew_hir::{
        HirExpr, HirExprKind, HirLiteral, HirNodeId, HirProducedValueFact,
        HirProducedValueProducer, IntentKind, ScopeId, ValueClass,
    };
    use hew_types::{BuiltinType, ProducedValueAcquisition, ProducedValueOwnership};

    fn owned_resource(site: SiteId) -> HirExpr {
        HirExpr {
            node: HirNodeId(site.0),
            site,
            // These tests exercise owner-generation transitions. Use a real
            // destructor-bearing type: an unregistered `Token` nominal has no
            // DropRecipe and therefore must not mint an OwnerId.
            ty: ResolvedTy::String,
            value_class: ValueClass::CowValue,
            intent: IntentKind::Read,
            kind: HirExprKind::Literal(HirLiteral::Unit),
            span: 0..0,
        }
    }

    #[test]
    fn alias_inheritance_is_exact_and_ambiguity_fails_closed() {
        let mut builder = Builder::default();
        let binding = BindingId(41);
        let ty = ResolvedTy::Tuple(vec![ResolvedTy::Bytes, ResolvedTy::I64]);
        let provenance = ValueProvenance::projection(
            PlaceProvenance::from(Place::Local(3)),
            vec![Projection::Field(0)],
        );
        builder.binding_locals.insert(binding, Place::Local(4));
        builder.register_owned_local_alias(
            binding,
            "alias".to_string(),
            ty.clone(),
            provenance.clone(),
            OwnerMintWarrant::granting_for_tests(),
        );
        builder.set_owned_local_disposition(
            binding,
            super::Disposition::ConsumedAt {
                transferee: None,
                site: super::DischargeSite::BindingMoved,
            },
        );

        assert_eq!(
            builder.exact_owned_local_alias_provenance(binding, &ty),
            OwnedAliasInheritance::Exact(provenance.clone()),
            "a consumed lexical alias still transfers alias provenance to its direct successor"
        );
        assert_eq!(
            builder.exact_owned_local_alias_provenance(BindingId(42), &ty),
            OwnedAliasInheritance::NotAlias
        );

        builder.register_owned_local_alias(
            binding,
            "ambiguous".to_string(),
            ty.clone(),
            provenance,
            OwnerMintWarrant::granting_for_tests(),
        );
        assert_eq!(
            builder.exact_owned_local_alias_provenance(binding, &ty),
            OwnedAliasInheritance::Ambiguous,
            "multiple ledger rows must never collapse into one alias lineage"
        );
    }

    #[test]
    fn scalar_user_resource_mints_before_its_affine_guard() {
        let binding = BindingId(699);
        let place = Place::Local(8);
        let ty = ResolvedTy::named_user("Handle", vec![]);
        let mut builder = Builder::default();
        builder.type_classes.insert(
            "Handle".to_owned(),
            (hew_hir::ResourceMarker::Resource, Some("close".to_owned())),
        );
        builder.binding_locals.insert(binding, place);
        builder.register_owned_local(
            binding,
            "handle".to_owned(),
            ty.clone(),
            crate::lower::OwnerMintWarrant::granting_for_tests(),
        );
        builder.maybe_alloc_affine_release_flag(binding, &ty);

        let owner = crate::model::OwnerId {
            binding,
            generation: 0,
        };
        let mint = builder.instructions.iter().position(|instruction| {
            matches!(
                instruction,
                Instr::OwnershipEvent(crate::model::OwnershipEvent::Mint {
                    owner: event_owner,
                    place: event_place,
                    ..
                }) if *event_owner == owner && *event_place == place
            )
        });
        let guard = builder.instructions.iter().position(|instruction| {
            matches!(
                instruction,
                Instr::OwnershipEvent(crate::model::OwnershipEvent::Guard {
                    owner: event_owner,
                    kind: crate::model::OwnershipGuardKind::AffineRelease,
                    ..
                }) if *event_owner == owner
            )
        });
        assert!(
            mint.zip(guard)
                .is_some_and(|(mint_index, guard_index)| mint_index < guard_index),
            "scalar resource must publish its owner before its affine guard: {:#?}",
            builder.instructions
        );
    }

    #[test]
    fn borrowed_runtime_resource_remains_ownerless_and_unguarded() {
        let site = SiteId(698);
        let place = Place::Local(7);
        let ty = ResolvedTy::named_user("Handle", vec![]);
        let mut facts = ParamOwnershipFacts::default();
        facts.produced_value_facts.insert(
            site,
            HirProducedValueFact {
                producer: HirProducedValueProducer::Call,
                ownership: ProducedValueOwnership::owned(ProducedValueAcquisition::Fresh),
                relation: hew_hir::HirProducedValueRelation::Leaf,
                receiver: None,
                receiver_boundary: None,
                arguments: Vec::new(),
            },
        );
        let mut builder = Builder {
            param_ownership: Rc::new(facts),
            ..Builder::default()
        };
        builder.type_classes.insert(
            "Handle".to_owned(),
            (hew_hir::ResourceMarker::Resource, Some("close".to_owned())),
        );
        builder.borrowed_runtime_result_places.insert(place);
        let expr = HirExpr {
            site,
            ty,
            ..owned_resource(site)
        };

        builder.adopt_typed_produced_value_owner(&expr, place);

        assert!(builder.owned_locals_ledger().is_empty());
        assert!(builder.owner_generations.is_empty());
        assert!(builder.affine_release_flags.is_empty());
        assert!(!builder.instructions.iter().any(|instruction| matches!(
            instruction,
            Instr::OwnershipEvent(
                crate::model::OwnershipEvent::Mint { .. }
                    | crate::model::OwnershipEvent::Guard { .. }
            )
        )));
    }

    #[test]
    fn rebind_replaces_scope_exit_authority_for_the_same_binding() {
        let binding = BindingId(700);
        let place = Place::Local(9);
        let mut value = owned_resource(SiteId(700));
        value.ty = ResolvedTy::String;
        let mut builder = Builder::default();
        builder.binding_locals.insert(binding, place);

        let first_warrant =
            builder.owner_warrant_for_initializer(binding, &value, &ResolvedTy::String);
        builder.register_owned_local(
            binding,
            "inner".to_string(),
            ResolvedTy::String,
            first_warrant,
        );
        builder.set_owned_local_consumed(binding, Some(place), super::DischargeSite::BindingMoved);
        let second_warrant =
            builder.owner_warrant_for_initializer(binding, &value, &ResolvedTy::String);
        builder.register_owned_local(
            binding,
            "inner".to_string(),
            ResolvedTy::String,
            second_warrant,
        );

        assert_eq!(
            builder.owned_locals_ledger().len(),
            2,
            "the ledger must preserve both value generations for later provenance scans"
        );
        assert_eq!(
            builder.owned_locals_owner_generations(),
            vec![(binding, "inner".to_string(), ResolvedTy::String)],
            "one mutable slot generation must have one scope-exit release authority"
        );
    }

    #[test]
    fn typed_owned_publication_mints_once_and_rejects_a_new_generation_over_live_local() {
        let site = SiteId(701);
        let expr = owned_resource(site);
        let mut builder = Builder::default();
        let mut facts = ParamOwnershipFacts::default();
        facts.produced_value_facts.insert(
            site,
            HirProducedValueFact {
                producer: HirProducedValueProducer::Literal,
                ownership: ProducedValueOwnership::owned(ProducedValueAcquisition::Fresh),
                relation: hew_hir::HirProducedValueRelation::Leaf,
                receiver: None,
                receiver_boundary: None,
                arguments: Vec::new(),
            },
        );
        builder.param_ownership = Rc::new(facts);

        let place = Place::Local(9);
        builder.adopt_typed_produced_value_owner(&expr, place);
        assert_eq!(builder.owned_locals_ledger().len(), 1);
        builder.adopt_typed_produced_value_owner(&expr, place);
        assert_eq!(builder.owned_locals_ledger().len(), 1);

        builder.adopt_synthetic_owned_local(
            "__test_rewrite",
            SiteId(702),
            match place {
                Place::Local(local) => local,
                other => panic!("expected local publication, got {other:?}"),
            },
            expr.ty.clone(),
            Builder::owner_warrant_for_typed_produced_value(ProducedValueOwnership::owned(
                ProducedValueAcquisition::Fresh,
            )),
        );
        assert_eq!(builder.owned_locals_ledger().len(), 1);
        assert!(builder.diagnostics.iter().any(|diagnostic| {
            matches!(
                diagnostic.kind,
                super::MirDiagnosticKind::NotYetImplemented { ref construct, .. }
                    if construct == "owned result rewrote a live provisional owner local"
            )
        }));
    }

    #[test]
    fn typed_scalar_clone_publication_reuses_a_parameter_without_minting_an_owner() {
        let site = SiteId(705);
        let expr = HirExpr {
            node: HirNodeId(site.0),
            site,
            ty: ResolvedTy::I64,
            value_class: ValueClass::BitCopy,
            intent: IntentKind::Read,
            kind: HirExprKind::Literal(HirLiteral::Integer(7)),
            span: 0..0,
        };
        let mut builder = Builder::default();
        let mut facts = ParamOwnershipFacts::default();
        facts.produced_value_facts.insert(
            site,
            HirProducedValueFact {
                producer: HirProducedValueProducer::RecordCloneCall,
                ownership: ProducedValueOwnership::owned(ProducedValueAcquisition::Fresh),
                relation: hew_hir::HirProducedValueRelation::Leaf,
                receiver: None,
                receiver_boundary: None,
                arguments: Vec::new(),
            },
        );
        builder.param_ownership = Rc::new(facts);
        builder.parameter_locals.insert(9);

        builder.adopt_typed_produced_value_owner(&expr, Place::Local(9));

        assert!(builder.owned_locals_ledger().is_empty());
        assert!(builder.diagnostics.is_empty());
    }

    #[test]
    fn projection_mints_after_parent_owner_was_consumed() {
        let parent_site = SiteId(706);
        let projection_site = SiteId(707);
        let parent = owned_resource(parent_site);
        let mut projection = owned_resource(projection_site);
        let mut facts = ParamOwnershipFacts::default();
        for (site, relation) in [
            (parent_site, hew_hir::HirProducedValueRelation::Leaf),
            (
                projection_site,
                hew_hir::HirProducedValueRelation::Projection(parent_site),
            ),
        ] {
            facts.produced_value_facts.insert(
                site,
                HirProducedValueFact {
                    producer: HirProducedValueProducer::Literal,
                    ownership: ProducedValueOwnership::owned(ProducedValueAcquisition::Fresh),
                    relation,
                    receiver: None,
                    receiver_boundary: None,
                    arguments: Vec::new(),
                },
            );
        }

        let mut builder = Builder {
            param_ownership: Rc::new(facts),
            ..Builder::default()
        };
        let parent_place = Place::Local(9);
        builder.adopt_typed_produced_value_owner(&parent, parent_place);
        let parent_binding = builder.owned_locals[0].binding;
        builder.set_owned_local_consumed(
            parent_binding,
            Some(Place::Local(10)),
            super::DischargeSite::BindingMoved,
        );

        projection.ty = parent.ty;
        builder.adopt_typed_produced_value_owner(&projection, Place::Local(11));

        assert_eq!(builder.owned_locals_ledger().len(), 2);
        assert_eq!(
            builder.owned_locals_snapshot(),
            vec![(
                builder.owned_locals[1].binding,
                "__hew_produced_value".to_string(),
                projection.ty,
            )]
        );
    }

    #[test]
    fn join_does_not_readmit_a_consumed_predecessor() {
        let source_site = SiteId(708);
        let result_site = SiteId(709);
        let source = owned_resource(source_site);
        let mut facts = ParamOwnershipFacts::default();
        facts.produced_value_facts.insert(
            source_site,
            HirProducedValueFact {
                producer: HirProducedValueProducer::Literal,
                ownership: ProducedValueOwnership::owned(ProducedValueAcquisition::Fresh),
                relation: hew_hir::HirProducedValueRelation::Leaf,
                receiver: None,
                receiver_boundary: None,
                arguments: Vec::new(),
            },
        );

        let mut builder = Builder {
            param_ownership: Rc::new(facts),
            ..Builder::default()
        };
        let source_place = Place::Local(12);
        let original_transferee = Place::Local(13);
        builder.adopt_typed_produced_value_owner(&source, source_place);
        builder
            .published_value_places
            .insert(source_site, source_place);
        let source_binding = builder.owned_locals[0].binding;
        builder.set_owned_local_consumed(
            source_binding,
            Some(original_transferee),
            super::DischargeSite::BindingMoved,
        );

        builder.transfer_join_owners(
            result_site,
            &[source_site],
            Place::Local(14),
            &ResolvedTy::String,
        );

        assert_eq!(builder.owned_locals_ledger().len(), 1);
        assert!(matches!(
            builder.owned_locals[0].disposition,
            super::Disposition::ConsumedAt {
                transferee: Some(place),
                site: super::DischargeSite::BindingMoved,
            } if place == original_transferee
        ));
    }

    #[test]
    fn cross_block_reinitialization_keeps_inline_released_owner_live() {
        let mut builder = Builder::default();
        let binding = builder.adopt_synthetic_owned_local(
            "__test_inline_release",
            SiteId(710),
            15,
            ResolvedTy::String,
            Builder::owner_warrant_for_typed_produced_value(ProducedValueOwnership::owned(
                ProducedValueAcquisition::Fresh,
            )),
        );
        builder.typed_produced_value_owner_bindings.insert(binding);
        let mut owner_definition = builder.instructions.clone();
        owner_definition.push(Instr::Drop {
            place: Place::Local(15),
            ty: ResolvedTy::String,
            drop_fn: Some(DropFnSpec::Release("hew_string_drop")),
        });
        let mut blocks = vec![
            BasicBlock {
                id: 0,
                statements: Vec::new(),
                instructions: owner_definition,
                terminator: Terminator::Goto { target: 1 },
            },
            BasicBlock {
                id: 1,
                statements: Vec::new(),
                instructions: vec![Instr::ConstI64 {
                    dest: Place::Local(15),
                    value: 0,
                }],
                terminator: Terminator::Return,
            },
        ];

        builder.consume_typed_publication_owners_at_inline_release(
            &mut blocks,
            &crate::lower::HashSet::new(),
        );

        assert_eq!(builder.owned_locals_snapshot().len(), 1);
    }

    #[test]
    fn preexisting_release_on_reused_local_cannot_retire_new_publication_generation() {
        let mut builder = Builder::default();
        let binding = builder.adopt_synthetic_owned_local(
            "__test_reused_release",
            SiteId(712),
            15,
            ResolvedTy::String,
            Builder::owner_warrant_for_typed_produced_value(ProducedValueOwnership::owned(
                ProducedValueAcquisition::Fresh,
            )),
        );
        builder.typed_produced_value_owner_bindings.insert(binding);
        let mut blocks = vec![BasicBlock {
            id: 0,
            statements: Vec::new(),
            instructions: vec![
                Instr::Move {
                    dest: Place::Local(15),
                    src: Place::Local(16),
                },
                Instr::Drop {
                    place: Place::Local(15),
                    ty: ResolvedTy::String,
                    drop_fn: Some(DropFnSpec::Release("hew_string_drop")),
                },
            ],
            terminator: Terminator::Return,
        }];

        builder.consume_typed_publication_owners_at_inline_release(
            &mut blocks,
            &crate::lower::HashSet::from([15]),
        );

        assert_eq!(builder.owned_locals_snapshot().len(), 1);
        assert!(!blocks[0].statements.iter().any(|statement| {
            matches!(
                statement,
                MirStatement::Use {
                    binding: consumed,
                    intent: IntentKind::Consume,
                    ..
                } if *consumed == binding
            )
        }));
    }

    #[test]
    fn loop_reentry_does_not_keep_inline_released_owner_live() {
        let mut builder = Builder::default();
        let binding = builder.adopt_synthetic_owned_local(
            "__test_inline_release",
            SiteId(710),
            15,
            ResolvedTy::String,
            Builder::owner_warrant_for_typed_produced_value(ProducedValueOwnership::owned(
                ProducedValueAcquisition::Fresh,
            )),
        );
        builder.typed_produced_value_owner_bindings.insert(binding);
        let mut owner_definition = builder.instructions.clone();
        owner_definition.push(Instr::Drop {
            place: Place::Local(15),
            ty: ResolvedTy::String,
            drop_fn: Some(DropFnSpec::Release("hew_string_drop")),
        });
        let mut blocks = vec![
            BasicBlock {
                id: 0,
                statements: Vec::new(),
                instructions: owner_definition,
                terminator: Terminator::Goto { target: 1 },
            },
            BasicBlock {
                id: 1,
                statements: Vec::new(),
                instructions: vec![Instr::ConstI64 {
                    dest: Place::Local(15),
                    value: 0,
                }],
                terminator: Terminator::Goto { target: 0 },
            },
        ];

        assert!(
            !crate::lower::cfg_util::local_is_rewritten_after_current_iteration(&blocks, 15, 0, 0,)
        );
        builder.consume_typed_publication_owners_at_inline_release(
            &mut blocks,
            &crate::lower::HashSet::new(),
        );

        assert!(builder.owned_locals_snapshot().is_empty());
    }

    #[test]
    fn identity_transfer_inside_loop_keeps_outer_binding_scope() {
        let receiver_site = SiteId(711);
        let receiver = owned_resource(receiver_site);
        let outer_scope = ScopeId(20);
        let loop_scope = ScopeId(21);
        let mut facts = ParamOwnershipFacts::default();
        facts.produced_value_facts.insert(
            receiver_site,
            HirProducedValueFact {
                producer: HirProducedValueProducer::Literal,
                ownership: ProducedValueOwnership::owned(ProducedValueAcquisition::Fresh),
                relation: hew_hir::HirProducedValueRelation::Leaf,
                receiver: None,
                receiver_boundary: None,
                arguments: Vec::new(),
            },
        );
        let mut builder = Builder {
            param_ownership: Rc::new(facts),
            active_scopes: vec![outer_scope],
            ..Builder::default()
        };
        let receiver_place = Place::Local(16);
        builder.adopt_typed_produced_value_owner(&receiver, receiver_place);
        builder
            .published_value_places
            .insert(receiver_site, receiver_place);
        let binding = builder.owned_locals[0].binding;
        assert_eq!(builder.binding_scope[&binding], outer_scope);

        builder.active_scopes.push(loop_scope);
        builder.transfer_identity_owner(receiver_site, Some(receiver_site), Place::Local(17));

        assert_eq!(builder.binding_scope[&binding], outer_scope);
    }

    #[test]
    fn borrowed_identity_keeps_the_source_owner_on_its_dominating_place() {
        let source_site = SiteId(712);
        let result_site = SiteId(713);
        let source = owned_resource(source_site);
        let mut result = owned_resource(result_site);
        result.intent = IntentKind::Read;
        let mut facts = ParamOwnershipFacts::default();
        facts.produced_value_facts.insert(
            source_site,
            HirProducedValueFact {
                producer: HirProducedValueProducer::Literal,
                ownership: ProducedValueOwnership::owned(ProducedValueAcquisition::Fresh),
                relation: hew_hir::HirProducedValueRelation::Leaf,
                receiver: None,
                receiver_boundary: None,
                arguments: Vec::new(),
            },
        );
        facts.produced_value_facts.insert(
            result_site,
            HirProducedValueFact {
                producer: HirProducedValueProducer::Block,
                ownership: ProducedValueOwnership::Borrowed,
                relation: hew_hir::HirProducedValueRelation::Identity(source_site),
                receiver: None,
                receiver_boundary: None,
                arguments: Vec::new(),
            },
        );
        let mut builder = Builder {
            param_ownership: Rc::new(facts),
            ..Builder::default()
        };
        let source_place = Place::Local(18);
        let branch_local_place = Place::Local(19);
        builder.adopt_typed_produced_value_owner(&source, source_place);
        builder
            .published_value_places
            .insert(source_site, source_place);

        builder.adopt_typed_produced_value_owner(&result, branch_local_place);

        assert_eq!(builder.owned_locals_ledger().len(), 1);
        let binding = builder.owned_locals[0].binding;
        assert_eq!(builder.binding_locals[&binding], source_place);
    }

    #[test]
    fn typed_vec_iter_publication_defers_to_the_cursor_sidecar() {
        let site = SiteId(714);
        let ty = ResolvedTy::named_builtin(
            BuiltinType::VecIter.canonical_name(),
            BuiltinType::VecIter,
            vec![ResolvedTy::I64],
        );
        let mut facts = ParamOwnershipFacts::default();
        facts.produced_value_facts.insert(
            site,
            HirProducedValueFact {
                producer: HirProducedValueProducer::Call,
                ownership: ProducedValueOwnership::owned(ProducedValueAcquisition::Fresh),
                relation: hew_hir::HirProducedValueRelation::Leaf,
                receiver: None,
                receiver_boundary: None,
                arguments: Vec::new(),
            },
        );
        let mut builder = Builder {
            param_ownership: Rc::new(facts),
            locals: vec![ty.clone()],
            ..Builder::default()
        };
        let expr = HirExpr {
            site,
            ty,
            ..owned_resource(site)
        };

        builder.adopt_typed_produced_value_owner(&expr, Place::Local(0));

        assert!(builder.owned_locals_ledger().is_empty());
    }

    #[test]
    fn synthetic_adoption_publishes_the_named_owners_existing_physical_guard() {
        let provisional = BindingId(790);
        let named = BindingId(791);
        let place = Place::Local(9);
        let flag = Place::Local(10);
        let mut builder = Builder::default();
        builder.binding_locals.insert(provisional, place);
        builder.register_owned_local(
            provisional,
            "__hew_produced_value".to_string(),
            ResolvedTy::String,
            crate::lower::OwnerMintWarrant::granting_for_tests(),
        );
        builder
            .synthetic_owner_publication_sites
            .insert(provisional, SiteId(790));
        builder.binding_locals.insert(named, place);
        builder.overwrite_guard_flags.insert(named, flag);

        builder.register_owned_local(
            named,
            "value".to_string(),
            ResolvedTy::String,
            crate::lower::OwnerMintWarrant::granting_for_tests(),
        );

        assert!(builder.instructions.iter().any(|instruction| matches!(
            instruction,
            Instr::OwnershipEvent(crate::model::OwnershipEvent::Guard {
                owner: crate::model::OwnerId {
                    binding,
                    generation: 0,
                },
                flag: published,
                kind: crate::model::OwnershipGuardKind::Overwrite,
            }) if *binding == named && *published == flag
        )));
    }

    #[test]
    fn synthetic_adoption_without_a_physical_guard_does_not_fabricate_one() {
        let provisional = BindingId(792);
        let named = BindingId(793);
        let place = Place::Local(9);
        let mut builder = Builder::default();
        builder.binding_locals.insert(provisional, place);
        builder.register_owned_local(
            provisional,
            "__hew_produced_value".to_string(),
            ResolvedTy::String,
            crate::lower::OwnerMintWarrant::granting_for_tests(),
        );
        builder
            .synthetic_owner_publication_sites
            .insert(provisional, SiteId(792));
        builder.binding_locals.insert(named, place);

        builder.register_owned_local(
            named,
            "value".to_string(),
            ResolvedTy::String,
            crate::lower::OwnerMintWarrant::granting_for_tests(),
        );

        assert!(builder.instructions.iter().any(|instruction| matches!(
            instruction,
            Instr::OwnershipEvent(crate::model::OwnershipEvent::Transfer {
                to_owner: Some(crate::model::OwnerId {
                    binding,
                    generation: 0,
                }),
                ..
            }) if *binding == named
        )));
        assert!(!builder.instructions.iter().any(|instruction| matches!(
            instruction,
            Instr::OwnershipEvent(crate::model::OwnershipEvent::Guard {
                owner: crate::model::OwnerId { binding, .. },
                ..
            }) if *binding == named
        )));
    }

    #[test]
    fn assignment_adoption_republishes_the_destination_guard() {
        let site = SiteId(794);
        let expr = owned_resource(site);
        let mut facts = ParamOwnershipFacts::default();
        facts.produced_value_facts.insert(
            site,
            HirProducedValueFact {
                producer: HirProducedValueProducer::Literal,
                ownership: ProducedValueOwnership::owned(ProducedValueAcquisition::Fresh),
                relation: hew_hir::HirProducedValueRelation::Leaf,
                receiver: None,
                receiver_boundary: None,
                arguments: Vec::new(),
            },
        );
        let source = Place::Local(9);
        let dest = Place::Local(10);
        let flag = Place::Local(11);
        let binding = BindingId(794);
        let mut builder = Builder {
            param_ownership: Rc::new(facts),
            ..Builder::default()
        };
        builder.adopt_typed_produced_value_owner(&expr, source);
        builder.binding_locals.insert(binding, dest);
        builder.overwrite_guard_flags.insert(binding, flag);

        builder.retire_provisional_owner_after_assignment_move(
            binding, dest, &expr.ty, source, &expr.ty,
        );

        assert!(builder.instructions.iter().any(|instruction| matches!(
            instruction,
            Instr::OwnershipEvent(crate::model::OwnershipEvent::Guard {
                owner: crate::model::OwnerId {
                    binding: owner_binding,
                    generation: 0,
                },
                flag: published,
                kind: crate::model::OwnershipGuardKind::Overwrite,
            }) if *owner_binding == binding && *published == flag
        )));
    }

    #[test]
    fn assignment_adoption_without_a_physical_guard_stays_unguarded() {
        let site = SiteId(795);
        let expr = owned_resource(site);
        let mut facts = ParamOwnershipFacts::default();
        facts.produced_value_facts.insert(
            site,
            HirProducedValueFact {
                producer: HirProducedValueProducer::Literal,
                ownership: ProducedValueOwnership::owned(ProducedValueAcquisition::Fresh),
                relation: hew_hir::HirProducedValueRelation::Leaf,
                receiver: None,
                receiver_boundary: None,
                arguments: Vec::new(),
            },
        );
        let source = Place::Local(9);
        let dest = Place::Local(10);
        let binding = BindingId(795);
        let mut builder = Builder {
            param_ownership: Rc::new(facts),
            ..Builder::default()
        };
        builder.adopt_typed_produced_value_owner(&expr, source);
        builder.binding_locals.insert(binding, dest);

        builder.retire_provisional_owner_after_assignment_move(
            binding, dest, &expr.ty, source, &expr.ty,
        );

        assert!(builder.instructions.iter().any(|instruction| matches!(
            instruction,
            Instr::OwnershipEvent(crate::model::OwnershipEvent::Transfer {
                to_owner: Some(crate::model::OwnerId {
                    binding: owner_binding,
                    generation: 0,
                }),
                ..
            }) if *owner_binding == binding
        )));
        assert!(!builder.instructions.iter().any(|instruction| matches!(
            instruction,
            Instr::OwnershipEvent(crate::model::OwnershipEvent::Guard {
                owner: crate::model::OwnerId {
                    binding: owner_binding,
                    ..
                },
                ..
            }) if *owner_binding == binding
        )));
    }

    #[test]
    fn assignment_move_retires_only_exact_type_congruent_synthetic_source() {
        let site = SiteId(711);
        let expr = owned_resource(site);
        let mut builder = Builder::default();
        let mut facts = ParamOwnershipFacts::default();
        facts.produced_value_facts.insert(
            site,
            HirProducedValueFact {
                producer: HirProducedValueProducer::Literal,
                ownership: ProducedValueOwnership::owned(ProducedValueAcquisition::Fresh),
                relation: hew_hir::HirProducedValueRelation::Leaf,
                receiver: None,
                receiver_boundary: None,
                arguments: Vec::new(),
            },
        );
        builder.param_ownership = Rc::new(facts);
        let source = Place::Local(9);
        let dest = Place::Local(10);
        let target = BindingId(88);
        builder.adopt_typed_produced_value_owner(&expr, source);
        let provisional = builder.owned_locals[0].binding;
        let publication_site = builder.synthetic_owner_publication_sites[&provisional];
        builder.binding_locals.insert(target, dest);

        builder.retire_provisional_owner_after_assignment_move(
            target,
            Place::Local(11),
            &expr.ty,
            source,
            &expr.ty,
        );
        assert_eq!(
            builder.owned_locals.len(),
            1,
            "wrong destination is not a move proof"
        );

        builder.retire_provisional_owner_after_assignment_move(
            target,
            dest,
            &expr.ty,
            source,
            &ResolvedTy::I64,
        );
        assert_eq!(
            builder.owned_locals.len(),
            1,
            "mismatched source type is not transferable"
        );

        builder
            .synthetic_owner_publication_sites
            .remove(&provisional);
        builder.retire_provisional_owner_after_assignment_move(
            target, dest, &expr.ty, source, &expr.ty,
        );
        assert_eq!(
            builder.owned_locals.len(),
            1,
            "non-synthetic owners are never retired here"
        );
        builder
            .synthetic_owner_publication_sites
            .insert(provisional, publication_site);

        builder.retire_provisional_owner_after_assignment_move(
            target, dest, &expr.ty, source, &expr.ty,
        );
        assert_eq!(builder.owned_locals.len(), 1);
        assert!(matches!(
            builder.owned_locals[0].disposition,
            super::Disposition::ConsumedAt {
                transferee: Some(place),
                site: super::DischargeSite::BindingMoved,
            } if place == dest
        ));
    }

    #[test]
    fn let_binding_adopts_typed_owner_when_legacy_registration_has_none() {
        let site = SiteId(721);
        let expr = owned_resource(site);
        let mut builder = Builder::default();
        let mut facts = ParamOwnershipFacts::default();
        facts.produced_value_facts.insert(
            site,
            HirProducedValueFact {
                producer: HirProducedValueProducer::Literal,
                ownership: ProducedValueOwnership::owned(ProducedValueAcquisition::Fresh),
                relation: hew_hir::HirProducedValueRelation::Leaf,
                receiver: None,
                receiver_boundary: None,
                arguments: Vec::new(),
            },
        );
        builder.param_ownership = Rc::new(facts);

        let source = Place::Local(9);
        let binding = BindingId(89);
        builder.adopt_typed_produced_value_owner(&expr, source);
        let provisional = builder.owned_locals[0].binding;
        builder.binding_locals.insert(binding, Place::Local(10));

        builder.retire_provisional_owner_for_bound_value(binding, "weak", source);

        assert_eq!(builder.owned_locals.len(), 2);
        let destination = builder
            .owned_locals
            .iter()
            .find(|entry| entry.binding == binding)
            .expect("named destination owner remains in the ledger");
        assert_eq!(destination.name, "weak");
        assert_eq!(destination.disposition, super::Disposition::ScopeExit);
        let source_entry = builder
            .owned_locals
            .iter()
            .find(|entry| entry.binding == provisional)
            .expect("moved-from source generation remains as unwind history");
        assert!(matches!(
            source_entry.disposition,
            super::Disposition::ConsumedAt {
                transferee: Some(Place::Local(10)),
                site: super::DischargeSite::BindingMoved,
            }
        ));
        assert_eq!(builder.binding_locals[&binding], Place::Local(10));
        assert!(builder
            .typed_produced_value_handoffs
            .contains(&(source, Place::Local(10))));
        assert_eq!(builder.binding_locals[&provisional], Place::Local(9));
        assert!(!builder
            .synthetic_owner_publication_sites
            .contains_key(&provisional));
        assert_eq!(
            builder.owned_locals_owner_generations(),
            vec![
                (
                    provisional,
                    "__hew_produced_value".to_string(),
                    expr.ty.clone(),
                ),
                (binding, "weak".to_string(), expr.ty),
            ]
        );
    }

    #[test]
    fn vec_clone_projection_root_requires_one_exact_typed_owner() {
        let root = Place::Local(9);
        let owner = (BindingId(1), ResolvedTy::named_user("Holder", vec![]));

        let mut exact = Builder::default();
        exact.seed_exact_vec_clone_projection_root(root, std::slice::from_ref(&owner));
        assert!(matches!(
            exact.owned_carrier_neutralize.get(&root),
            Some(OwnedCarrierNeutralizeTarget::Whole(place)) if *place == root
        ));

        let mut borrowed = Builder::default();
        borrowed.seed_exact_vec_clone_projection_root(root, &[]);
        assert!(borrowed.owned_carrier_neutralize.is_empty());

        let mut ambiguous = Builder::default();
        ambiguous.seed_exact_vec_clone_projection_root(root, &[owner.clone(), owner]);
        assert!(ambiguous.owned_carrier_neutralize.is_empty());
    }
}
