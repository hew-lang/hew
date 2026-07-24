use super::{
    actor_name_from_handle_ty, affine_release_needs_drop_flag, base_local, binding_ref_target,
    callee_returns_fresh_owner, machine_layout_name_matches, mangle_layout_key,
    monomorphic_user_record_key, named_type_marker, ty_is_closure_pair,
    ty_is_heap_owning_enum_composite, ty_is_local_collection_handle, user_record_layout_key,
    vec_iter_record_layout_key, ActiveIterationOwner, BindingId, Builder, BuiltinType,
    ClosurePairIngress, CmpPred, DecisionFact, DischargeSite, Disposition, FieldLoadClass,
    FreshVecGetCloneProjectionBase, HashMap, HashSet, HirBinding, HirBlock, HirExpr, HirExprKind,
    HirStmtKind, Instr, IntentKind, LayoutClass, MirDiagnostic, MirDiagnosticKind, MirStatement,
    OwnedLocalEntry, OwnershipCtx, OwnershipDecision, Place, PlaceProvenance, Projection,
    ResolvedRef, ResolvedTy, ResourceMarker, SiteId, Strategy, Terminator, ValueClass,
    ValueOwnership, ValueProvenance, SYNTHETIC_CALL_SCRUTINEE_NAME,
    SYNTHETIC_COPY_IN_PARAM_TEMP_NAME, SYNTHETIC_DISCARDED_CALL_RESULT_NAME,
    SYNTHETIC_OWNED_TEMP_BINDING_BASE, SYNTHETIC_VEC_GET_CLONE_PROJECTION_BASE_NAME,
    SYNTHETIC_WHILE_LET_ITERATION_NAME,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum WholeParamEmbedClass {
    None,
    IndependentlyOwnedOnly,
    UnsupportedBorrowAlias,
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

impl Builder {
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
    /// The single registration authority for the per-function owned-locals
    /// ledger: every seam that introduces a scope-exit drop obligation routes
    /// through here instead of pushing a bare tuple. Ownership is classified
    /// ONCE at this defining write and recorded on the entry; the drop passes
    /// read the written-down fact rather than re-deriving it per pass.
    ///
    /// The binding's backing place drives classify's handle-vs-type dispatch. It
    /// is resolved from `binding_locals` when the slot is already wired (the
    /// `let` / param / dyn-trait seams insert it first); a match binder registers
    /// before its `Place` insert, but a match binder always stores into a plain
    /// local, so the type-driven arm is the correct one and the exact local id is
    /// immaterial (the fallback local only feeds the never-firing handle
    /// dispatch). Provenance stays `None` at this stage — it is recorded only
    /// where a later stage can trivially prove it at the defining write. Every
    /// entry is minted `Disposition::ScopeExit`; a retraction seam later
    /// dispositions it off the scope-exit set via
    /// [`Builder::set_owned_local_disposition`] when its release is handled
    /// mid-lowering.
    pub(crate) fn register_owned_local(
        &mut self,
        binding: BindingId,
        name: String,
        ty: ResolvedTy,
    ) {
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
            provenance: None,
            disposition: Disposition::ScopeExit,
        });
    }
    pub(crate) fn register_synthetic_owned_local(
        &mut self,
        name: &'static str,
        site: SiteId,
        local: u32,
        ty: ResolvedTy,
    ) -> BindingId {
        let binding =
            BindingId(SYNTHETIC_OWNED_TEMP_BINDING_BASE - self.synthetic_owned_temp_bindings);
        self.synthetic_owned_temp_bindings += 1;
        self.statements.push(MirStatement::Bind {
            binding,
            name: name.to_string(),
            site,
            ty: ty.clone(),
        });
        self.binding_locals.insert(binding, Place::Local(local));
        self.record_binding_scope(binding);
        self.register_owned_local(binding, name.to_string(), ty);
        binding
    }

    pub(crate) fn note_fresh_vec_clone_projection_base(
        &mut self,
        place: Place,
        ty: ResolvedTy,
        site: SiteId,
    ) {
        let Place::Local(local) = place else {
            unreachable!("the Vec clone destination is always a local");
        };
        self.fresh_vec_get_clone_projection_bases
            .push(FreshVecGetCloneProjectionBase { local, ty, site });
    }

    /// Registers the ordinary scope-exit owner for a fresh composite cloned by
    /// `Vec` indexing and immediately used as a record-projection base.
    pub(crate) fn register_fresh_vec_get_clone_projection_base_owner(
        &mut self,
        object: &HirExpr,
        record_place: Place,
    ) {
        if !matches!(object.kind, HirExprKind::Index { .. }) {
            return;
        }
        let Place::Local(local) = record_place else {
            return;
        };
        let object_ty = self.subst_ty(&object.ty);
        let Some(fact_index) = self
            .fresh_vec_get_clone_projection_bases
            .iter()
            .position(|fact| fact.local == local && fact.ty == object_ty)
        else {
            return;
        };
        if self.parameter_locals.contains(&local)
            || self
                .binding_locals
                .values()
                .any(|place| *place == record_place)
            || !crate::model::ty_owns_heap_mir(
                &object_ty,
                &self.record_field_orders,
                &self.enum_layouts,
            )
        {
            return;
        }
        let fact = self.fresh_vec_get_clone_projection_bases.remove(fact_index);
        self.register_synthetic_owned_local(
            SYNTHETIC_VEC_GET_CLONE_PROJECTION_BASE_NAME,
            fact.site,
            fact.local,
            fact.ty,
        );
    }
    /// Register a `let`-bound field projection whose result is a byte-copy
    /// interior ALIAS of the still-live owner named by `provenance` — the
    /// [`ByteCopyAlias`](FieldLoadClass::ByteCopyAlias) class of the field-load
    /// three-way split (record / tuple / inline-enum aggregate field). The entry
    /// carries its
    /// [`OwnershipDecision::InteriorAlias`]-shaped provenance and is minted
    /// [`Disposition::AliasOf`], so it drops out of the scope-exit-live view the
    /// drop-elaboration provers and `build_lifo_drops` read: the alias emits no
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
    ) {
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
    /// #2429 — give a FROM-CALL enum-composite match scrutinee an owner.
    ///
    /// `match f() { Ok(b) => …, Err(e) => {} }` consumes the callee's
    /// by-value `Result`/`Option` return through an anonymous MIR temp. With
    /// no `BindingId`, the temp is invisible to `build_lifo_drops` /
    /// `enumerate_exits`, so the arm-destructured payload was released on NO
    /// edge — not the loop back-edge, not the return plan — and every
    /// iteration of a `while … { match f() { … } }` read loop leaked one
    /// payload allocation. Minting a synthetic owned binding over the temp
    /// routes it through the PROVEN let-bound discipline end to end: the
    /// fail-closed enum-composite sole-owner prover decides admission, the
    /// back-edge body-scope filter releases per iteration, the return/cancel
    /// plans cover the straight-line case, and the scope-close goto pass
    /// covers break/continue edges.
    ///
    /// Registration alone never emits a drop: an escaping payload keeps
    /// today's leak-not-double-free posture because
    /// `derive_enum_composite_drop_allowed` still excludes the composite.
    pub(crate) fn call_scrutinee_owned_ty(&self, scrutinee: &HirExpr) -> Option<ResolvedTy> {
        // Direct Hew calls and `Weak.upgrade` are the audited fresh enum
        // producers. A `BindingRef` scrutinee already owns its slot through its
        // own binding registration, so minting another owner would double-drop.
        // Other rvalues keep the fail-closed posture.
        let callee = match &scrutinee.kind {
            HirExprKind::Call { callee, .. } => Some(callee.as_ref()),
            HirExprKind::RcIntrinsic {
                op: hew_types::RcIntrinsicOp::WeakUpgrade,
                ..
            } => None,
            _ => return None,
        };
        // Recv-next / vec-string-iter-next scrutinees already carry their own
        // per-iteration release discipline (`Disposition::BodyEndReleased` on
        // the Some-arm payload binder); their codegen-materialised `Option`
        // shell owns no heap beyond that payload. Registering a second owner
        // here would double-release the payload. (A generator `.next()`
        // scrutinee is `HirExprKind::GeneratorNext`, not `Call`, so the shape
        // gate above already excludes it.)
        if callee.is_some()
            && (Self::is_recv_next_scrutinee(scrutinee)
                || self.is_vec_string_iter_next_scrutinee(scrutinee))
        {
            return None;
        }
        // Runtime-symbol / builtin producers have per-symbol ownership
        // contracts — an interior getter may hand back a BORROW of storage
        // the receiver still owns, and freeing that here would be the #2384
        // double-free class. Only a Hew callee's by-value return is
        // unconditionally caller-owned (the callee side retracts its drop of
        // every member handed out through the return —
        // `derive_returned_aggregate_member_bindings`), so this caller holds
        // the single release obligation.
        if let Some(callee) = callee {
            if let HirExprKind::BindingRef { name, resolved } = &callee.kind {
                if matches!(resolved, ResolvedRef::Builtin(_))
                    || crate::runtime_symbols::is_known_runtime_symbol(name)
                {
                    return None;
                }
            }
        }
        // Exactly the value class the `EnumInPlace` scope-exit machinery
        // owns; anything else keeps its pre-fix posture.
        let ty = self.subst_ty(&scrutinee.ty);
        if !ty_is_heap_owning_enum_composite(&ty, &self.record_field_orders, &self.enum_layouts) {
            return None;
        }
        Some(ty)
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
    /// ## Why the top-level producer allowlist is sound
    ///
    /// A fresh top-level constructor/producer (`StructInit` / `TupleLiteral` /
    /// `MachineVariantCtor` enum ctor / a string-producing `Binary`/`Unary`
    /// concat / an explicit `RecordCloneCall`) allocates an aggregate that SOLELY
    /// owns itself. The synthetic owner then flows through the SAME
    /// `derive_{record,tuple,enum}_composite_drop_allowed` / `derive_cow_sole_owner`
    /// prover as a `let`-bound composite, so field/element provenance (a moved-in
    /// vs cloned field) is decided by that authority exactly as for the named
    /// shape — registration alone never forces a drop (an escaping value is still
    /// excluded → leak, never a double-free).
    ///
    /// EVERYTHING ELSE fails closed to today's leak-not-double-free posture:
    /// - a `BindingRef` already has an owner (its `let`/param) — a second owner
    ///   over the same value double-frees;
    /// - a projection (`g(r.field)`, `g(t.0)`, `g(xs[i])`) is an interior alias
    ///   of an owner's storage;
    /// - a call return (`g(f())`) may hand back a borrowed parameter alias
    ///   (#2648) — trusted fresh ONLY when `callee_returns_fresh_owner` proves
    ///   (via the module freshness fixpoint) that EVERY return path of the callee
    ///   is a fresh sole owner, never a param/interior alias; an unproven or
    ///   alias-forwarding callee stays excluded (leak, never a double-free);
    /// - a bare string `Literal` (`h("abc")`) is a static/interned constant that
    ///   allocates nothing and must not be freed (verified: 0 leaks unminted).
    pub(crate) fn caller_borrowed_temp_arg_owned_ty(&self, arg: &HirExpr) -> Option<ResolvedTy> {
        let ty = self.subst_ty(&arg.ty);
        // Only a heap-owning value carries a drop obligation.
        if !crate::model::ty_owns_heap_mir(&ty, &self.record_field_orders, &self.enum_layouts) {
            return None;
        }
        let is_fresh_producer = match &arg.kind {
            HirExprKind::StructInit { .. }
            | HirExprKind::TupleLiteral { .. }
            | HirExprKind::MachineVariantCtor { .. }
            | HirExprKind::RecordCloneCall { .. } => true,
            // A string-producing concat/interpolation allocates a fresh buffer;
            // a bare string `Literal` is excluded above by being a non-producer
            // arm (it interns a static — freeing it is unsound).
            HirExprKind::Binary { .. } | HirExprKind::Unary { .. } => {
                matches!(ty, ResolvedTy::String)
            }
            // A string-returning direct `Call` whose callee carries the runtime
            // `produces_fresh_owned_string` contract: the `cstring` transforms
            // (`to_upper`/`to_lower`/`trim`/`repeat`, lowered through their
            // `RewriteToFunction` c-symbol) and an f-string interpolation (a
            // `Call`-chain of `string_concat`, `hew-hir/src/lower.rs`). Each
            // hands the caller a genuinely fresh rc==1 buffer it solely owns —
            // the last leaking #2428 shape, before this arm the `_ => false`
            // catch-all skipped the #2743/#2745 caller-side temp mint and the
            // buffer leaked (32 B/call). The runtime contract is the EXACT
            // fresh-vs-non-fresh discriminator: a call returning a BORROWED /
            // interior-alias string (`ResultOwnership::Borrowed`) or an
            // un-catalogued USER function (`FAIL_CLOSED`) is NOT
            // `produces_fresh_owned_string`, so it fails closed here (leak,
            // never a caller-side double-free). Registration alone never forces
            // the drop — the SAME string sole-owner prover then gates it, so a
            // fresh result that ESCAPES (consumed/returned) is still excluded.
            HirExprKind::Call { callee, .. } => {
                if matches!(ty, ResolvedTy::String) {
                    Self::call_produces_fresh_owned_string(callee)
                } else {
                    // A composite (record/tuple/enum) result: admit a PROVEN-fresh
                    // producer, gated by the SAME module sole-owner prover the push
                    // consult site uses (`callee_returns_fresh_owner`). A callee
                    // whose freshness fixpoint verdict is `false` — because a
                    // return path forwards, projects, or launders a by-value param
                    // (#2648) — stays excluded (leak, never a caller-side
                    // double-free). The mint caller then gates the actual drop on
                    // `proven_borrow_args` (borrow vs consume), so a CONSUMING
                    // composite callee's temp is never double-registered, and a
                    // fresh result that ESCAPES is still excluded by the sole-owner
                    // prover exactly as for the named `let x = mk(); g(x)` shape.
                    callee_returns_fresh_owner(callee, &self.funcupdate_fn_returns_fresh)
                }
            }
            _ => false,
        };
        is_fresh_producer.then_some(ty)
    }
    /// Return the owned type of a fresh Vec COPY-IN element temporary whose
    /// only whole-parameter embeds are independently retained strings.
    ///
    /// A by-value parameter remains caller-owned. Aggregate lowering explicitly
    /// retains a whole `string` parameter before storing it, so the anonymous
    /// source temp owns exactly that retained share and may receive one ordinary
    /// scope-exit drop. Other heap-owning parameter types are stored as borrowed
    /// aliases; minting an aggregate owner for them would free the caller's
    /// value.
    pub(crate) fn copy_in_param_embed_temp_owned_ty(
        &self,
        callee: &str,
        arg: &HirExpr,
    ) -> Option<ResolvedTy> {
        if !matches!(callee, "hew_vec_push_owned" | "hew_vec_set_owned") {
            return None;
        }
        if !matches!(
            arg.kind,
            HirExprKind::StructInit { .. }
                | HirExprKind::TupleLiteral { .. }
                | HirExprKind::MachineVariantCtor { .. }
        ) {
            return None;
        }
        let class = Self::classify_whole_param_embeds(
            arg,
            &self.funcupdate_param_ids,
            &self.owned_carrier_param_ids,
            &|ty| self.subst_ty(ty),
            true,
            &|ty| crate::model::ty_owns_heap_mir(ty, &self.record_field_orders, &self.enum_layouts),
        );
        if class != WholeParamEmbedClass::IndependentlyOwnedOnly {
            return None;
        }
        let ty = self.subst_ty(&arg.ty);
        crate::model::ty_owns_heap_mir(&ty, &self.record_field_orders, &self.enum_layouts)
            .then_some(ty)
    }
    /// Register the proven retain-backed Vec element source after all call
    /// arguments have materialised. Push uses args[0]/places[1], while set uses
    /// args[1]/places[2]; the receiver and set index can never become owners.
    pub(crate) fn register_copy_in_param_embed_temp_owner(
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
        .and_then(|(arg, place_index)| {
            let ty = self.copy_in_param_embed_temp_owned_ty(callee, arg)?;
            let place = arg_places.get(place_index).copied()?;
            Some((arg.site, place, ty))
        });
        let Some((site, Place::Local(local), ty)) = candidate else {
            return;
        };
        // Fresh constructors must never lower into a parameter slot. Keep the
        // mint fail-closed if that invariant ever changes.
        if self.parameter_locals.contains(&local) {
            return;
        }
        self.register_synthetic_owned_local(SYNTHETIC_COPY_IN_PARAM_TEMP_NAME, site, local, ty);
    }
    /// Whether a direct-`Call` callee resolves to a runtime symbol carrying the
    /// `produces_fresh_owned_string` ownership contract. The symbol is resolved
    /// from the callee identity exactly as value-lowering does
    /// (`runtime_symbol_for_call_expr`): a `ResolvedRef::Builtin` family via its
    /// catalog `c_symbol()`, every other resolved callee via the checker-minted
    /// callee name — the `RewriteToFunction` c-symbol for a method rewrite (e.g.
    /// `hew_string_to_uppercase`), or the catalog presentation name for f-string
    /// concat (`string_concat`). Both spellings are dual-listed in
    /// `callee_ownership_contract`.
    fn call_produces_fresh_owned_string(callee: &HirExpr) -> bool {
        let HirExprKind::BindingRef { name, resolved } = &callee.kind else {
            return false;
        };
        let symbol = match resolved {
            ResolvedRef::Builtin(family) => family.c_symbol(),
            _ => name.as_str(),
        };
        crate::runtime_symbols::callee_ownership_contract(symbol).produces_fresh_owned_string()
    }
    /// #2648 preflight admission classifier — pure HIR, run at the TOP of every
    /// call-scrutinee consumer BEFORE `lower_value`/CFG allocation. Returns the
    /// admission token the from-call owner mint and the #2523 origin consume, or
    /// an `Err(MirDiagnostic)` reject (ONE diagnostic, no partial MIR) for a
    /// scrutinee whose callee may hand back a borrowed by-value parameter alias
    /// (summary contains `PARAM`) or an un-audited heap-returning `extern`.
    ///
    /// # Interim behaviour (S2–S4, before the trusted-root precursor merges)
    ///
    /// - A resolved module-fn callee whose precise summary carries `PARAM`
    ///   REJECTS — the PRIMARY #2648 forwarder double-free fix (`match
    ///   passthru(h.b)`). Mixed `PARAM|OPAQUE` forwarders reject too.
    /// - A module fn whose summary is `∅` (Fresh) or `OPAQUE`-only takes
    ///   [`CallScrutineeAdmission::LegacyModuleCall`] — today's owner-mint
    ///   admission EXACTLY (jwt/encrypt/semver/template keep compiling). The
    ///   precise `OPAQUE`-only rejects + jwt/encrypt Fresh admits land at S4b.
    /// - A declared heap-returning `extern` REJECTS — including a user extern
    ///   whose NAME spoofs a runtime recv symbol (it resolves to
    ///   `ResolvedRef::Item`, so it is keyed by id, never the name).
    /// - Every recv/stream/`Builtin` carve-out, unknown/cross-module item, and
    ///   indirect/closure callee behaves as today (`NotApplicable` /
    ///   `LegacyModuleCall` fail-open).
    pub(crate) fn classify_call_scrutinee_admission(
        &self,
        scrutinee: &HirExpr,
    ) -> Result<crate::return_provenance::CallScrutineeAdmission, Box<MirDiagnostic>> {
        use crate::return_provenance::{is_typed_recv_callee, AliasBits, CallScrutineeAdmission};
        // `Weak.upgrade` always returns a fresh `Option<Rc<T>>` owner. Admit it
        // before the general Call gate so a matched `Some` payload is released
        // when the arm closes.
        if matches!(
            &scrutinee.kind,
            HirExprKind::RcIntrinsic {
                op: hew_types::RcIntrinsicOp::WeakUpgrade,
                ..
            }
        ) {
            let ty = self.subst_ty(&scrutinee.ty);
            return Ok(
                if ty_is_heap_owning_enum_composite(
                    &ty,
                    &self.record_field_orders,
                    &self.enum_layouts,
                ) {
                    CallScrutineeAdmission::Admit
                } else {
                    CallScrutineeAdmission::NotApplicable
                },
            );
        }

        // Structural Call-gate: only a direct `Call` rvalue can otherwise mint
        // the from-call owner. A non-`Call` scrutinee (a `Block`/`If` synthetic
        // `Vec<_>`-iteration desugar, a `GeneratorNext`, a bare place, an
        // aggregate) is `NotApplicable` ON KIND — exactly `call_scrutinee_owned_ty`'s
        // early `None`, before any runtime-identity resolution can be consulted.
        let HirExprKind::Call { callee, args } = &scrutinee.kind else {
            return Ok(CallScrutineeAdmission::NotApplicable);
        };
        if let HirExprKind::BindingRef { name, resolved } = &callee.kind {
            // Typed carve-outs, keyed on the compiler-minted identity (NOT the
            // display name): a recv/stream family or any `ResolvedRef::Builtin`
            // callee carries its own per-iteration `BodyEndReleased` release, so
            // no synthetic owner is minted. A user extern SPOOFING one of those
            // names resolves to `ResolvedRef::Item` → does NOT match here → falls
            // through to the reject arms below (closes the name-forgeable bypass).
            if is_typed_recv_callee(callee) || matches!(resolved, ResolvedRef::Builtin(_)) {
                return Ok(CallScrutineeAdmission::NotApplicable);
            }
            // Only a heap-owning-enum-composite return mints an owner; anything
            // else is `NotApplicable`, exactly as `call_scrutinee_owned_ty`
            // returns `None`.
            let ty = self.subst_ty(&scrutinee.ty);
            if !ty_is_heap_owning_enum_composite(&ty, &self.record_field_orders, &self.enum_layouts)
            {
                return Ok(CallScrutineeAdmission::NotApplicable);
            }
            let prov = &self.call_scrutinee_provenance;
            // A declared user extern — even one whose name spoofs a runtime
            // symbol — reaching the heap-enum ty-gate is an un-audited heap
            // extern. A call to an extern dispatches by NAME (its call-site
            // `ResolvedRef::Item` carries a placeholder id, not the declaration's),
            // so extern detection keys on the name, BEFORE the runtime-symbol
            // carve-out (closes the name-forgeable bypass). No heap extern is
            // trusted-Fresh in the interim (the marker-backed jwt/encrypt rows
            // land at S4b).
            if prov.extern_names.contains(name) {
                // OWN-0b carriage: the machine-checked per-symbol ownership
                // fact is consultable at exactly this extern-callee position.
                // Nothing is enforced from it yet (S1/V1 consume it); the
                // conservative reject below stays authoritative, and an
                // unclassified symbol is an explicit `Absent`, never a
                // fabricated contract.
                debug_assert!(
                    crate::ffi_contracts::extern_ownership_contract("hew_string_drop")
                        .contract()
                        .is_some_and(|contract| contract.params
                            == [crate::ffi_contracts::ExternParamOwnership::Consume]),
                    "FFI ownership carriage table unreadable at the extern-call \
                     lowering position"
                );
                return Err(Box::new(Self::call_scrutinee_reject(
                    scrutinee,
                    "an un-audited heap-returning `extern` may hand back an interior pointer \
                     the caller still owns",
                )));
            }
            if let ResolvedRef::Item(id) = resolved {
                // A resolved module fn with an analysable body: consult its
                // precise summary ONLY for the interim `PARAM`-present reject.
                if let Some(bits) = prov.provenance.get(id) {
                    if bits.contains(AliasBits::PARAM) {
                        // S2b — the ParamsOnly caller arg-scan. A `{PARAM}`-only
                        // summary means the return can alias ONLY the callee's
                        // by-value heap parameters, so when EVERY argument is
                        // provably fresh at this call site the returned value
                        // derives exclusively from fresh inputs — a fresh sole
                        // owner: ADMIT (the template/semver stdlib shape). A
                        // mixed `PARAM|OPAQUE` summary stays an unconditional
                        // reject — the `OPAQUE` component is never
                        // arg-rescuable.
                        if bits.is_params_only() && self.params_only_args_provably_fresh(args) {
                            return Ok(CallScrutineeAdmission::Admit);
                        }
                        return Err(Box::new(Self::call_scrutinee_reject(
                            scrutinee,
                            "the called function may return one of its by-value heap parameters \
                             (a borrow the caller still owns), so minting a second owner over \
                             the scrutinee would double-free",
                        )));
                    }
                    // Fresh or `OPAQUE`-only → interim legacy fail-open mint.
                    return Ok(CallScrutineeAdmission::LegacyModuleCall);
                }
            }
            // A genuine runtime-symbol callee that resolves to neither a user
            // extern nor an analysable module fn keeps today's name-based skip.
            if crate::runtime_symbols::is_known_runtime_symbol(name) {
                return Ok(CallScrutineeAdmission::NotApplicable);
            }
        }
        // An unknown/missing/cross-module item or a non-`BindingRef` (indirect /
        // closure) callee → interim legacy fail-open (today's mint); the precise
        // `OPAQUE` reject for these lands at S4b.
        Ok(CallScrutineeAdmission::LegacyModuleCall)
    }
    /// Build the single #2648 reject diagnostic (a clean NYI — no partial
    /// codegen). `why` names the specific unsound shape. Boxed to keep the
    /// preflight's `Result` `Err` variant small.
    pub(crate) fn call_scrutinee_reject(scrutinee: &HirExpr, why: &str) -> MirDiagnostic {
        MirDiagnostic {
            kind: MirDiagnosticKind::NotYetImplemented {
                construct: "call-scrutinee returning a borrowed parameter or un-audited heap \
                            extern"
                    .to_string(),
                site: scrutinee.site,
            },
            note: format!(
                "#2648: {why}. Bind the call result to a `let` and match on the binding, or \
                 return a freshly-constructed value from the callee."
            ),
        }
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
            HirExprKind::Call { callee, args } => {
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
        admission: crate::return_provenance::CallScrutineeAdmission,
        scrutinee: &HirExpr,
        scrutinee_local: u32,
    ) -> Option<(BindingId, ResolvedTy)> {
        use crate::return_provenance::CallScrutineeAdmission;
        // The non-optional admission token gates the mint [F4]: `NotApplicable`
        // mints nothing (the scrutinee's own release runs); `Admit` /
        // `LegacyModuleCall` proceed to the existing owner gate. A `Reject` never
        // reaches here — the preflight returned early at the consumer.
        match admission {
            CallScrutineeAdmission::NotApplicable => return None,
            CallScrutineeAdmission::Admit | CallScrutineeAdmission::LegacyModuleCall => {}
        }
        let ty = self.call_scrutinee_owned_ty(scrutinee)?;
        let binding = self.register_synthetic_owned_local(
            SYNTHETIC_CALL_SCRUTINEE_NAME,
            scrutinee.site,
            scrutinee_local,
            ty.clone(),
        );
        Some((binding, ty))
    }
    pub(crate) fn register_while_let_iteration_owner(
        &mut self,
        scrutinee: &HirExpr,
        snapshot_local: u32,
        ty: ResolvedTy,
    ) -> BindingId {
        let binding = self.register_synthetic_owned_local(
            SYNTHETIC_WHILE_LET_ITERATION_NAME,
            scrutinee.site,
            snapshot_local,
            ty,
        );
        self.back_edge_only_iteration_owners.insert(binding);
        binding
    }
    pub(crate) fn discarded_call_result_owned_ty(&self, expr: &HirExpr) -> Option<ResolvedTy> {
        if let Some(ty) = self.call_scrutinee_owned_ty(expr) {
            return Some(ty);
        }
        let HirExprKind::ResolvedImplCall {
            target_symbol,
            target_family,
            ..
        } = &expr.kind
        else {
            return None;
        };
        let caller_owned = matches!(
            (target_family, target_symbol.as_str()),
            (
                hew_types::MethodTargetFamily::HashMap(hew_types::HashMapMethod::Get),
                "hew_hashmap_get_layout"
            ) | (
                hew_types::MethodTargetFamily::HashMap(hew_types::HashMapMethod::Remove),
                "hew_hashmap_remove_take_layout"
            )
        );
        if !caller_owned {
            return None;
        }
        let ty = self.subst_ty(&expr.ty);
        ty_is_heap_owning_enum_composite(&ty, &self.record_field_orders, &self.enum_layouts)
            .then_some(ty)
    }
    pub(crate) fn register_discarded_call_result_owner(&mut self, expr: &HirExpr, place: Place) {
        let Some(ty) = self.discarded_call_result_owned_ty(expr) else {
            return;
        };
        let Place::Local(local) = place else {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "discarded caller-owned result place shape".to_string(),
                    site: expr.site,
                },
                note: format!(
                    "a proven caller-owned discarded result must lower to Place::Local; got \
                     {place:?}"
                ),
            });
            return;
        };
        self.register_synthetic_owned_local(
            SYNTHETIC_DISCARDED_CALL_RESULT_NAME,
            expr.site,
            local,
            ty,
        );
    }
    pub(crate) fn record_iteration_owner_drop(
        &mut self,
        binding: BindingId,
        name: &str,
        site: SiteId,
        ty: &ResolvedTy,
    ) {
        let drops = self
            .iteration_owner_drop_blocks
            .entry(self.current_block_id)
            .or_default();
        if drops.contains(&binding) {
            return;
        }
        self.statements.push(MirStatement::Use {
            binding,
            name: name.to_string(),
            site,
            ty: ty.clone(),
            intent: IntentKind::Consume,
        });
        drops.push(binding);
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
                let ordinal = u32::try_from(*index).ok()?;
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
    /// The `(alias_local, owner_root_local)` pairs for every `AliasOf` ledger
    /// entry: a `let mid = o.mid` / `let leaf = mid.leaf` byte-copy interior
    /// alias mapped to the base local of the still-live OWNER root its recorded
    /// provenance chains to, resolving intermediate aliases to the first
    /// non-alias owner (a `leaf -> mid -> o` chain resolves both `leaf` and
    /// `mid` to `o`).
    ///
    /// The record and tuple composite provers fold these into their field-
    /// binder set. The whole-value alias map and the field-load scan they build
    /// from the instruction stream only reach a ONE-hop alias (`mid` reads the
    /// root directly, so it is collected); a DEEPER alias (`leaf` reads `mid`,
    /// not the root) is invisible to them. Without the carried provenance a deep
    /// alias that ESCAPES into an owning sink (returned, stored into an owning
    /// record, sent) would leave the owner's composite admitted to free a
    /// subtree the escapee already handed to the caller — a double-free. Folding
    /// the recorded alias in, attributed to its owner, excludes exactly that
    /// owner when the deep alias escapes, and leaves the owner admitted when the
    /// alias is only read interiorly (the consumed-match path).
    ///
    /// An entry whose owner root is not a nameable local is dropped — the prover
    /// keeps its fail-closed blanket for it (leak, never double-free).
    pub(crate) fn alias_owner_field_binders(&self) -> Vec<(u32, u32)> {
        // One hop: each alias's base local -> its recorded provenance root local.
        // Keyed on the carried alias PROVENANCE, not the live disposition: a
        // recorded byte-copy alias that is later consumed (moved into the return
        // slot / a sink) is dispositioned off `AliasOf`, but consuming an alias
        // moves no ownership — the owner still holds the heap. Its escape must
        // still exclude the owner, so the provenance keeps it in scope here even
        // after the disposition flips.
        let mut one_hop: HashMap<u32, u32> = HashMap::new();
        for entry in &self.owned_locals {
            let Some(PlaceProvenance::Local(root_local)) =
                entry.provenance.as_ref().map(|p| p.root)
            else {
                continue;
            };
            let Some(alias_local) = self
                .binding_locals
                .get(&entry.binding)
                .and_then(|p| base_local(*p))
            else {
                continue;
            };
            one_hop.insert(alias_local, root_local);
        }
        // Resolve each alias to the ultimate owner by chasing intermediate
        // aliases. The hop count is bounded by the alias count, so the walk
        // terminates even if a (malformed) cycle ever appears.
        let mut resolved = Vec::with_capacity(one_hop.len());
        for (&alias_local, &first_root) in &one_hop {
            let mut owner = first_root;
            for _ in 0..one_hop.len() {
                match one_hop.get(&owner) {
                    Some(&next) => owner = next,
                    None => break,
                }
            }
            resolved.push((alias_local, owner));
        }
        resolved
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
    /// the intermediate `mid.x` through the `mid` alias) — matching the multi-hop
    /// reach `close_alias_binders_forward` gave the composite-drop prover's
    /// exclusion. Without it the widened exclusion removes the owner's composite
    /// drop while the one-hop sibling emitter (blind past a one-hop alias) leaves
    /// every deeper sibling to leak unconditionally.
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
    /// compat shape the twelve allow-set provers, `build_lifo_drops`, and the
    /// double-free gate consume. The `Disposition::ScopeExit` filter narrows the
    /// ledger to exactly the bindings the function-exit LIFO pass still owns:
    /// entries retracted by a [`Builder::set_owned_local_disposition`] write
    /// (consumed, body-end-released, inner-scope-released) are excluded, which is
    /// the same set the former `owned_locals.retain(...)` physical removals left
    /// behind — so the drop-elaboration view is byte-identical to the pre-
    /// disposition ledger. The retracted entries survive in the whole ledger
    /// ([`Builder::owned_locals_ledger`]) for an end-of-pass scan to observe.
    pub(crate) fn owned_locals_snapshot(&self) -> Vec<(BindingId, String, ResolvedTy)> {
        self.owned_locals
            .iter()
            .filter(|entry| entry.disposition == Disposition::ScopeExit)
            .map(|entry| (entry.binding, entry.name.clone(), entry.ty.clone()))
            .collect()
    }
    /// The owned-locals whose release obligation is still SOLE-OWNED per binding —
    /// either scope-exit-live (`ScopeExit`) or retracted only by a consume that
    /// transfers the value out (`ConsumedAt`). A binding in this view is a
    /// candidate for the path-sensitive returned-member re-admission in
    /// `elaborate`: a value returned/handed-off on SOME paths (so it is either an
    /// aggregate member the return handoff removes, or a whole-value return that
    /// retracts it to `ConsumedAt`) can still be the live sole owner on a guard
    /// early-return, where its scope-exit drop must be restored. `BodyEndReleased`
    /// / `ScopeReleased` (already released mid-body) and `AliasOf` (a non-owning
    /// interior alias — never its own drop) are excluded, so the re-admission
    /// never resurrects a drop those dispositions deliberately elide.
    pub(crate) fn owned_locals_returned_candidates(&self) -> Vec<(BindingId, String, ResolvedTy)> {
        self.owned_locals
            .iter()
            .filter(|entry| {
                matches!(
                    entry.disposition,
                    Disposition::ScopeExit | Disposition::ConsumedAt { .. }
                )
            })
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
    /// function-exit LIFO drop pass no longer fires on it — byte-identical to the
    /// physical removal it replaces. Sets every entry matching `binding`,
    /// mirroring `retain`'s remove-all semantics (at most one exists in
    /// practice).
    pub(crate) fn set_owned_local_disposition(
        &mut self,
        binding: BindingId,
        disposition: Disposition,
    ) {
        for entry in &mut self.owned_locals {
            if entry.binding == binding {
                entry.disposition = disposition;
            }
        }
    }
    /// Retract a binding to [`Disposition::ConsumedAt`], REQUIRING its discharge
    /// authority (`transferee` + `site`) by signature — the close-by-construction
    /// consume-write chokepoint (U221/U229). A consume retraction cannot be
    /// spelled without naming who took ownership and why, so the fact is never
    /// erased at the retraction seam. Every production consume routes through
    /// here (via [`Builder::mark_binding_moved`]).
    pub(crate) fn set_owned_local_consumed(
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
        crate::state_clone::classify_owned_string_record_fields(&field_tys, &record_layouts, &[])
            .ok()
            .flatten()
    }
    /// Unified owned-aggregate-record value-class authority (RC-4 / RC-6 / G12).
    ///
    /// Returns `Some(kinds)` iff the record named by `key`:
    ///   1. has a registered layout (`record_field_orders`),
    ///   2. classifies cleanly under the SAME resource-aware field classifier
    ///      actor state uses (`classify_actor_state_fields_with_resource_handles`),
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
        let kinds = crate::state_clone::classify_actor_state_fields_with_resource_handles(
            &field_tys,
            &record_layouts,
            &self.enum_layouts,
            &self.opaque_handle_names,
            &self.resource_opaque_close,
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
    /// The type checker qualifies imported record names with their module
    /// prefix (e.g. `"process.CommandOutput"`), but the MIR layout loop
    /// registers them under the bare type name (`"CommandOutput"`) taken
    /// from `HirTypeDecl.name`.  Try the full key first; if that misses,
    /// strip the last `.`-separated prefix and try the bare name.  This
    /// covers every single-level module-qualified record (`module.Type`)
    /// without touching the mangled generic case (`Type$$arg`) which
    /// never contains a dot at the type-name position.
    pub(crate) fn lookup_record_field_order(
        &self,
        type_name: &str,
    ) -> Option<&Vec<(String, ResolvedTy)>> {
        if let Some(order) = self.record_field_orders.get(type_name) {
            return Some(order);
        }
        // Fallback: strip the module prefix and try the bare type name.
        let bare = hew_types::short_name(type_name);
        if bare != type_name {
            if let Some(order) = self.record_field_orders.get(bare) {
                return Some(order);
            }
        }
        None
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
            HirExprKind::Call { callee, args } => {
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
            | HirExprKind::ActorGenStream { receiver, args, .. }
            | HirExprKind::SpawnedCall {
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
        let value_class = if expr.value_class == ValueClass::Unknown {
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
            ResolvedTy::Named { name, args, .. } if args.is_empty() => {
                self.actor_layouts.contains_key(name)
                    || machine_layout_name_matches(&self.machine_layout_names, name)
            }
            // Generic enum applications (`Named { name: "Option", args: [I64] }`):
            // the origin name is in `machine_layout_names` if the HIR mono pass
            // discovered at least one instantiation and registered it in
            // `module.enum_layouts`. Actor layouts never have type args, so this
            // arm is purely for generic enum types. Classifying as `BitCopy`
            // matches the tagged-union substrate — enums are stack-allocated
            // discriminated unions with no drop side-effect.
            ResolvedTy::Named { name, .. } => {
                machine_layout_name_matches(&self.machine_layout_names, name)
            }
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
    /// runtime frees and whose allow-set provers ride the
    /// `derive_local_collection_drop_allowed` escape scan; every other owned
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
        dest: Place,
        target_ty: &ResolvedTy,
        flag: Place,
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
        self.emit_local_overwrite_release(dest, target_ty);
        self.finish_current_block(Terminator::Goto { target: cont_bb });
        self.start_block(cont_bb);
    }
    pub(crate) fn mark_returned_binding_moved(&mut self, expr: &HirExpr) {
        let HirExprKind::BindingRef {
            resolved: ResolvedRef::Binding(id),
            ..
        } = expr.kind
        else {
            return;
        };
        self.mark_binding_moved(id);
    }
    pub(crate) fn mark_binding_moved(&mut self, id: BindingId) {
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
            ResolvedTy::String
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
                let short = hew_types::short_name(name);
                let layout = if args.is_empty() {
                    self.enum_layouts.iter().find(|layout| {
                        layout.name == *name || hew_types::short_name(&layout.name) == short
                    })
                } else {
                    let mangled = mangle_layout_key(short, args);
                    self.enum_layouts
                        .iter()
                        .find(|layout| layout.name == mangled || layout.name == *name)
                };
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
            &self.funcupdate_fn_returns_fresh,
            &self.funcupdate_param_ids,
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
        fresh: &HashMap<hew_hir::ItemId, bool>,
        params: &HashSet<BindingId>,
    ) -> bool {
        match &expr.kind {
            // ---- value-passthrough wrappers: ALL reachable values must be
            //      materialised (look THROUGH; reject any bare-binding leaf) ----
            // A block's value is its tail (statements are side-effecting only);
            // peel to the tail regardless of statement count.
            HirExprKind::Block(block) => block
                .tail
                .as_deref()
                .is_some_and(|t| Self::expr_is_materialized_owner(t, fresh, params)),
            // BOTH `if` branches must be materialised. A missing `else` cannot
            // produce an owned-record value, so it fails closed.
            HirExprKind::If {
                then_expr,
                else_expr,
                ..
            } => {
                Self::expr_is_materialized_owner(then_expr, fresh, params)
                    && else_expr
                        .as_deref()
                        .is_some_and(|e| Self::expr_is_materialized_owner(e, fresh, params))
            }
            // EVERY `match` arm body must be materialised (an arm body that is a
            // bare payload binding aliases the scrutinee and fails closed).
            HirExprKind::Match { arms, .. } => {
                !arms.is_empty()
                    && arms
                        .iter()
                        .all(|arm| Self::expr_is_materialized_owner(&arm.body, fresh, params))
            }
            // ---- materialised leaves ----
            // A `.clone()` result is a fresh deep copy materialised into its own
            // slot — unconditionally an owner.
            HirExprKind::RecordCloneCall { .. } => true,
            // A free-function call is a materialised owner ONLY when the module
            // interprocedural summary proves the callee returns a fresh owner on
            // every path. A blanket `Call => true` is UNSOUND: a function can
            // launder a by-value heap parameter (a BORROW) through its return
            // without a refcount bump (`fn id(p: Inner) -> Inner { p }`), so
            // `..id(o.inner)` would free the caller's live `o.inner` at the
            // override-drop (the call-returns-borrowed-param use-after-free).
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
            HirExprKind::StructInit { .. } => {
                Self::classify_whole_param_embeds(
                    expr,
                    params,
                    &HashSet::new(),
                    &ResolvedTy::clone,
                    false,
                    &|_| false,
                ) == WholeParamEmbedClass::None
            }
            HirExprKind::TupleLiteral { .. } | HirExprKind::MachineVariantCtor { .. } => {
                Self::classify_whole_param_embeds(
                    expr,
                    params,
                    &HashSet::new(),
                    &ResolvedTy::clone,
                    false,
                    &|_| false,
                ) == WholeParamEmbedClass::None
            }
            // A projection is materialised iff its object chain is.
            HirExprKind::FieldAccess { object, .. } => {
                Self::expr_is_materialized_owner(object, fresh, params)
            }
            HirExprKind::TupleIndex { tuple, .. } => {
                Self::expr_is_materialized_owner(tuple, fresh, params)
            }
            // Bare/`Const` binding ref, machine-state field projection, deref, a
            // method call (can return borrowed `self`/param), or any future
            // expression form — not provably a materialised owner. Fail closed.
            _ => false,
        }
    }
    /// Classify WHOLE by-value parameter embeds through constructors.
    ///
    /// Recurses only through constructions (struct / tuple / machine-variant
    /// literals), which embed operands by value. For the existing materialised-
    /// owner route, other leaves stop the recursion. The Vec COPY-IN mint uses
    /// the stricter mode: every non-constructor heap-owning leaf fails closed,
    /// because a projection or unproven call result can carry an unretained alias
    /// derived from another parameter. This admits only construction trees whose
    /// owned leaves are the whole retained string parameters being discharged.
    fn classify_whole_param_embeds(
        expr: &HirExpr,
        params: &HashSet<BindingId>,
        owned_carrier_params: &HashSet<BindingId>,
        resolve_ty: &impl Fn(&ResolvedTy) -> ResolvedTy,
        reject_unproven_owned_leaves: bool,
        owns_heap: &impl Fn(&ResolvedTy) -> bool,
    ) -> WholeParamEmbedClass {
        match &expr.kind {
            HirExprKind::BindingRef {
                resolved: ResolvedRef::Binding(id),
                ..
            } if params.contains(id) => {
                if matches!(resolve_ty(&expr.ty), ResolvedTy::String)
                    || owned_carrier_params.contains(id)
                {
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
    pub(crate) fn consume_moved_builtin_method_arg(&mut self, operand: &HirExpr) {
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
        self.statements.push(MirStatement::Use {
            binding: *id,
            name: name.clone(),
            site: operand.site,
            ty,
            intent: IntentKind::Consume,
        });
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
