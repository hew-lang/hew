//! The owner-mint warrant: the one thing a scope-exit owner mint cannot
//! fabricate.
//!
//! # The defect this closes
//!
//! Round 5 gave the `let` binder a ledger consultation and immediately found a
//! double release there. Every OTHER binder seam decided ownership from the
//! binding's TYPE, its layout, or a dataflow fact, and was reported as
//! "measures zero in the shapes I could construct" — a property of the shapes
//! reached, not a proof. That is the same defect the fresh-owner authority was
//! built to end, one layer down: the guard is consulted where someone
//! remembered to consult it.
//!
//! # The close
//!
//! [`OwnerMintWarrant`] has private fields and lives in this module. Its only
//! constructors are the `Builder` methods BELOW, each of which puts the
//! provenance question to the typed produced-value carrier, the per-function
//! proven-foreign ledger, the module's
//! [`FreshOwnerVerdicts`](crate::return_provenance::FreshOwnerVerdicts)
//! authority, or a combination. [`Builder::register_owned_local`],
//! [`Builder::register_owned_local_alias`] and
//! [`Builder::adopt_synthetic_owned_local`] each REQUIRE one, and withhold
//! the mint when it answers foreign.
//!
//! Rust's module privacy is the mechanism, so the property is a compile-time
//! one rather than a review convention:
//!
//! * a new mint site does not compile until it produces a warrant;
//! * a warrant cannot be produced outside this module, so it cannot be
//!   produced without asking;
//! * every constructor names the VALUE the owner is for — an expression, a
//!   source binding, or a parameter — so "I have nothing to ask about" is not
//!   expressible either.
//!
//! There is therefore no fourteenth seam to open later.
//!
//! # Where privacy stops, and what carries the claim there
//!
//! The three bullets above are claims about the REGISTRARS, and privacy really
//! does carry them: their signatures demand an [`OwnerMintWarrant`] whose only
//! constructors live here. They are NOT claims about the LEDGER those
//! registrars write into. `Builder::owned_locals` is `pub(crate)` and
//! `OwnedLocalEntry` is a private item of `lower/mod.rs` — which, in Rust,
//! every child module of `lower` can see. So a direct `push` of an
//! `OwnedLocalEntry` onto that ledger, written straight into `expr.rs`,
//! compiles, mints a scope-exit owner, and asks nothing. The two push sites in
//! the tree are both registrars, so nothing is wrong today; but "does not
//! compile" was never what stopped a third one.
//!
//! `every_owned_locals_ledger_mint_site_sits_behind_a_warrant` below carries
//! that half, as a property of the source over the whole crate rather than of
//! the shapes anyone happened to construct.
//!
//! # Polarity, and why it differs between constructors
//!
//! The two queries have deliberately opposite fail-closed directions and the
//! constructors below pick between them by what the mint is FOR. Neither can
//! ever license a mint the authority denies; they differ only in what an
//! unanswerable question costs.
//!
//! * A BINDER warrant (initializer, scrutinee payload, rebind) asks the DUAL
//!   `proven-foreign` query: unknown reads as domestic, so withholding requires
//!   proof. The strict reading at a binder would delete the scope-exit release
//!   of every binding whose initializer reaches an indirect or unanalysed
//!   callee — a leak in ordinary code that never touches an `extern`.
//! * A TEMP warrant asks the STRICT `free-of-opaque-foreign-provenance` query:
//!   unknown reads as opaque and the mint is withheld. A synthetic temp exists
//!   only for its own mint, so withholding it costs at most the temp's release
//!   and can never delete a release someone else was relying on.
//! * The OWNED-PARAMETER warrant asks neither, because in the callee's frame
//!   there is nothing to ask. See
//!   [`Builder::owner_warrant_for_owned_parameter`] for the argument and the
//!   tripwire that pins it.

use super::{BindingId, Builder, HirExpr, HirExprKind, ProducedValueOwnership, ResolvedTy};

/// Which value the owner is being minted over, as the question was actually
/// put. Recorded on the warrant so the withheld/granted decision names its own
/// evidence rather than a boolean with no provenance of its own.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum OwnerMintOrigin {
    /// A `let` / `var` initializer expression, or any other binder whose whole
    /// value is one HIR expression in this function.
    Initializer,
    /// A payload projected out of a scrutinee expression: `match`, `if let`,
    /// `while let`, `let else`, and the nested-pattern binders of each.
    PayloadOfScrutinee,
    /// A rebind of, or restore of, another binding in this same function.
    RebindOfBinding,
    /// A value that entered this frame across the call boundary as a parameter
    /// the callee owns. See [`Builder::owner_warrant_for_owned_parameter`] for
    /// why the question is answered at the CALLER and what enforces it.
    OwnedParameter,
    /// A synthetic temp whose own admission gate already asked the authority
    /// about the producing expression, forwarding that answer here.
    ForwardedFromAdmissionGate,
}

/// The provenance answer a scope-exit owner mint must present.
///
/// Constructed only by the `Builder` methods in this module. See the module
/// docs for why that is the whole point.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct OwnerMintWarrant {
    origin: OwnerMintOrigin,
    proven_foreign: bool,
}

impl OwnerMintWarrant {
    /// The one private constructor. Every public constructor below funnels
    /// through it, so there is exactly one place where a warrant comes into
    /// existence and it always carries an answer that was asked for.
    fn new(origin: OwnerMintOrigin, proven_foreign: bool) -> Self {
        Self {
            origin,
            proven_foreign,
        }
    }

    /// True when the authority proved the value foreign and the mint must be
    /// withheld.
    pub(crate) fn withholds_mint(self) -> bool {
        self.proven_foreign
    }

    /// The value the question was put about. Read by the inventory tests.
    pub(crate) fn origin(self) -> OwnerMintOrigin {
        self.origin
    }

    /// Test-only warrant that grants the mint, for unit tests that exercise a
    /// registrar's bookkeeping rather than its provenance gate.
    ///
    /// `#[cfg(test)]`, so the production build's "no warrant without asking"
    /// invariant is intact — the same discipline
    /// [`FreshOwnerVerdicts::from_parts_for_tests`](crate::return_provenance)
    /// uses for the authority itself. The gate is the whole guarantee: this
    /// constructor does not exist in a production build, so no production mint
    /// site can name it.
    #[cfg(test)]
    pub(crate) fn granting_for_tests() -> Self {
        Self::new(OwnerMintOrigin::Initializer, false)
    }
}

impl Builder {
    /// Ask about a binder whose whole value is one HIR expression in this
    /// function: a `let` / `var` initializer, or a `dyn Trait` binder's RHS.
    ///
    /// This is the round-5 `let` close, unchanged in substance and now
    /// unavoidable in form. The answer is RECORDED in this function's
    /// proven-foreign ledger, so the same fact travels with the binding into
    /// every container it is later embedded in.
    pub(crate) fn owner_warrant_for_initializer(
        &mut self,
        binding: BindingId,
        value: &HirExpr,
        binding_ty: &ResolvedTy,
    ) -> OwnerMintWarrant {
        let foreign = self.note_let_binder_proven_foreign(binding, value, binding_ty);
        OwnerMintWarrant::new(OwnerMintOrigin::Initializer, foreign)
    }

    /// Ask about a payload binder projected out of a scrutinee.
    ///
    /// `match` / `if let` / `while let` / `let else` payload binders, at every
    /// nesting depth, and the `Binding` arm predicate. The value is a field of
    /// the scrutinee, so the question is put about the SCRUTINEE expression —
    /// through the same proven-foreign query and the same ledger the `let`
    /// binder reads.
    ///
    /// # Why this does not carry the `let` binder's `string` carve-out
    ///
    /// The carve-out at the `let` binder exists because a root
    /// `extern "C" -> string` is ADOPTED at the call edge into a refcounted Hew
    /// buffer, so the binding really does own its value. Adoption is defined at
    /// `return_ty == String` and nowhere else — it does not reach a `string`
    /// FIELD of a returned record nor a `string` inside a returned
    /// `Option`/`Result`. A payload binder is exactly those un-adopted
    /// positions, so importing the carve-out here would mint a release for a
    /// pointer the host still owns. The payload question is therefore asked
    /// carve-out-free.
    pub(crate) fn owner_warrant_for_scrutinee_payload(
        &mut self,
        binding: BindingId,
        scrutinee: &HirExpr,
        binding_ty: &ResolvedTy,
    ) -> OwnerMintWarrant {
        let foreign = self.note_payload_binder_proven_foreign(binding, scrutinee, binding_ty);
        OwnerMintWarrant::new(OwnerMintOrigin::PayloadOfScrutinee, foreign)
    }

    /// Mint authority for one *active* enum payload of a mixed-return call.
    /// The normal payload warrant asks about the whole scrutinee, correctly
    /// refusing a `Result` that contains an opaque sibling. The variant summary
    /// has already proved this exact `(tag, field)` path fresh using the same
    /// precise and audited-transfer authorities, so this grants only that
    /// binder; it never makes the enclosing shell droppable. An imported call
    /// can carry a declaration identity distinct from its analyzed origin, so
    /// the emitted-symbol projection must preserve the measured fact. If
    /// neither identity has a row, a `Sink` or `Stream` payload is refused with
    /// a named diagnostic. Argument shapes cannot prove that a returned
    /// endpoint is newly owned.
    pub(crate) fn owner_warrant_for_fresh_variant_payload(
        &mut self,
        scrutinee: &HirExpr,
        variant_idx: u32,
        field_idx: u32,
        payload_ty: &ResolvedTy,
    ) -> Option<OwnerMintWarrant> {
        let HirExprKind::Call { callee, .. } = &scrutinee.kind else {
            return None;
        };
        let HirExprKind::BindingRef {
            name,
            resolved: hew_hir::ResolvedRef::Item(_),
        } = &callee.kind
        else {
            return None;
        };
        if self
            .call_scrutinee_provenance
            .extern_table
            .is_extern_name(name)
        {
            return None;
        }
        let measured_fresh = self
            .call_scrutinee_provenance
            .callee_returns_fresh_variant_payload(callee, variant_idx, field_idx);
        if measured_fresh {
            return Some(OwnerMintWarrant::new(
                OwnerMintOrigin::PayloadOfScrutinee,
                false,
            ));
        }

        let payload_ty = self.subst_ty(payload_ty);
        let resource_payload_without_summary = matches!(
            payload_ty,
            ResolvedTy::Named {
                builtin: Some(hew_types::BuiltinType::Sink | hew_types::BuiltinType::Stream),
                ..
            }
        );
        if resource_payload_without_summary
            && !self.diagnostics.iter().any(|diagnostic| {
                matches!(
                    &diagnostic.kind,
                    super::MirDiagnosticKind::ImportedResourcePayloadSummaryMissing {
                        symbol,
                        site,
                        ..
                    } if symbol == name && *site == scrutinee.site
                )
            })
        {
            self.diagnostics.push(super::MirDiagnostic {
                kind: super::MirDiagnosticKind::ImportedResourcePayloadSummaryMissing {
                    symbol: name.clone(),
                    payload_ty: payload_ty.user_facing().to_string(),
                    site: scrutinee.site,
                },
                note: "the callee has no measured active resource payload summary; publish a per-variant return summary before matching this payload"
                    .to_string(),
            });
        }
        None
    }

    /// Ask about a binder that rebinds or restores another binding in this same
    /// function — the `var`-self method receiver restore, and any future
    /// same-frame rebind.
    ///
    /// The ledger is the whole authority here: a source binding the `let`
    /// binder refused an owner for must not acquire one by being rebound. The
    /// fact propagates onto the new binding so a chain of rebinds cannot launder
    /// it.
    pub(crate) fn owner_warrant_for_rebind(
        &mut self,
        binding: BindingId,
        source: BindingId,
        binding_ty: &ResolvedTy,
    ) -> OwnerMintWarrant {
        let foreign = self.note_rebind_proven_foreign(binding, source, binding_ty);
        OwnerMintWarrant::new(OwnerMintOrigin::RebindOfBinding, foreign)
    }

    /// Ask about a parameter this frame owns: a `consume` parameter, an
    /// owned-carrier parameter, or an actor-handler message parameter.
    ///
    /// # Why the answer is `false` here, and what makes that a proof
    ///
    /// A parameter's value is not produced in this frame. There is no HIR
    /// expression to put to the authority, and the per-function ledger is
    /// populated only by binder seams in the BODY, which are lowered strictly
    /// after `lower_params` — so a parameter binding is provably absent from it.
    /// The question is unanswerable HERE, and "fail closed" cannot mean
    /// "withhold": withholding every callee-side parameter release would leak
    /// every `consume` parameter and every actor message in the language.
    ///
    /// So the question is answered at the CALLER, where it IS answerable, and
    /// answered fail-closed there:
    /// [`Builder::reject_opaque_foreign_ownership_transfer`] refuses the call
    /// outright when an argument is proven foreign and this module's
    /// `param_ownership` tables say the callee takes ownership of that
    /// parameter. A proven-foreign value therefore cannot reach an owned
    /// parameter position at all, which is what makes the type-driven mint here
    /// provably never a mint over a foreign value.
    ///
    /// The `debug_assert` is the tripwire for the one way that could stop being
    /// true: if a future change ever enters a parameter binding into the
    /// proven-foreign ledger, this fires rather than minting.
    pub(crate) fn owner_warrant_for_owned_parameter(
        &self,
        binding: BindingId,
        _binding_ty: &ResolvedTy,
    ) -> OwnerMintWarrant {
        debug_assert!(
            !self.proven_foreign_bindings.contains(&binding),
            "a parameter binding is in the proven-foreign ledger; the caller-side transfer \
             refusal is no longer the whole answer for owned parameters and this mint must \
             start consulting the ledger directly"
        );
        OwnerMintWarrant::new(
            OwnerMintOrigin::OwnedParameter,
            self.proven_foreign_bindings.contains(&binding),
        )
    }

    /// Forward the answer an admission gate already obtained about the
    /// expression that PRODUCES a synthetic temp.
    ///
    /// The call-scrutinee owner, the `while let` iteration owner, the discarded
    /// call-result owner, the `Vec` clone-projection base and the copy-in
    /// element temp each have a gate that already asked the authority about the
    /// producing expression before deciding the temp exists at all. This
    /// constructor re-asks the ledger over that same expression so the answer
    /// arrives at the mint rather than being remembered on the way, and so a
    /// gate that is ever loosened cannot silently take the mint with it.
    pub(crate) fn owner_warrant_for_admitted_temp(&self, producer: &HirExpr) -> OwnerMintWarrant {
        let foreign = !self.value_is_free_of_opaque_foreign_provenance(producer);
        OwnerMintWarrant::new(OwnerMintOrigin::ForwardedFromAdmissionGate, foreign)
    }

    /// The checker/HIR carrier is the sole release authority for a newly
    /// published result. MIR projects that verdict without reclassifying the
    /// producer or consulting a second freshness analysis.
    pub(crate) fn owner_warrant_for_typed_produced_value(
        ownership: ProducedValueOwnership,
    ) -> OwnerMintWarrant {
        OwnerMintWarrant::new(
            OwnerMintOrigin::ForwardedFromAdmissionGate,
            !matches!(ownership, ProducedValueOwnership::Owned { .. }),
        )
    }
}

#[cfg(test)]
mod tests {
    use super::{OwnerMintOrigin, OwnerMintWarrant};

    #[test]
    fn a_granting_warrant_does_not_withhold_and_records_its_origin() {
        let warrant = OwnerMintWarrant::granting_for_tests();
        assert!(!warrant.withholds_mint());
        assert_eq!(warrant.origin(), OwnerMintOrigin::Initializer);
    }
}
