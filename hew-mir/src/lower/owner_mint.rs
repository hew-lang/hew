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
    #[cfg(test)]
    pub(crate) fn origin(self) -> OwnerMintOrigin {
        self.origin
    }

    /// Test-only warrant that grants the mint, for unit tests that exercise a
    /// registrar's bookkeeping rather than its provenance gate.
    ///
    /// `#[cfg(test)]`, so the production build's "no warrant without asking"
    /// invariant is intact — the same discipline
    /// [`FreshOwnerVerdicts::from_parts_for_tests`](crate::return_provenance)
    /// uses for the authority itself. `granting_from_source_inventory_is_closed`
    /// pins that no production module can reach it.
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
    /// binder; it never makes the enclosing shell droppable.
    pub(crate) fn owner_warrant_for_fresh_variant_payload(
        &self,
        scrutinee: &HirExpr,
        variant_idx: u32,
        field_idx: u32,
    ) -> Option<OwnerMintWarrant> {
        let HirExprKind::Call { callee, .. } = &scrutinee.kind else {
            return None;
        };
        self.call_scrutinee_provenance
            .callee_returns_fresh_variant_payload(callee, variant_idx, field_idx)
            .then(|| OwnerMintWarrant::new(OwnerMintOrigin::PayloadOfScrutinee, false))
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

    /// The structural close, asserted as a property of the SOURCE rather than
    /// of the shapes anyone happened to construct.
    ///
    /// A warrant comes into existence in exactly one place — the private
    /// `OwnerMintWarrant::new` — and every caller of it is a constructor in this
    /// module that names the value the question is about. Nothing outside this
    /// file can write the struct literal, because the fields are private, so
    /// this scan is a belt on top of a compile-time brace: it fails loudly if a
    /// future edit adds a second in-module constructor that asks nothing.
    #[test]
    fn every_warrant_constructor_asks_the_authority_or_the_ledger() {
        // Every public constructor's body must contain a call that puts the
        // provenance question to the ledger or the module authority. This is a
        // property of the SOURCE, so it holds for constructors nobody has
        // written yet.
        const ASKS: [&str; 8] = [
            "note_let_binder_proven_foreign",
            "note_payload_binder_proven_foreign",
            "note_rebind_proven_foreign",
            "value_is_free_of_opaque_foreign_provenance",
            "proven_foreign_bindings",
            "callee_returns_fresh_variant_payload",
            "expr_reads_a_proven_foreign_binding",
            "ProducedValueOwnership::Owned",
        ];
        let source = include_str!("owner_mint.rs");
        let squeezed: String = source.chars().filter(|c| !c.is_whitespace()).collect();
        assert_eq!(
            squeezed.matches("Self{origin:").count(),
            1,
            "the struct literal must exist exactly once — inside the private \
             `new`. A second literal is a constructor that can skip the question."
        );

        // Assembled at run time so this test's own source does not contain the
        // needle and match itself.
        let needle = format!("pub(crate) fn {}", "owner_warrant_for_");
        let mut constructors = 0usize;
        for chunk in source.split(needle.as_str()).skip(1) {
            constructors += 1;
            let name: String = chunk.chars().take_while(|c| *c != '(').collect();
            let body = chunk.split("\n    /// ").next().unwrap_or(chunk);
            assert!(
                ASKS.iter().any(|ask| body.contains(ask)),
                "warrant constructor `{name}` is produced without asking \
                 the ledger or the authority; every constructor must contain \
                 one of {ASKS:?}"
            );
        }
        assert!(
            constructors >= 5,
            "expected at least the five documented warrant constructors, \
             found {constructors} — did a constructor lose its `owner_warrant_for_` \
             prefix and escape this scan?"
        );
    }

    /// The test-only granting constructor is reachable from tests only. Pins
    /// that the production tree never names it, so the `#[cfg(test)]` gate is
    /// not the only thing standing between a mint and an unasked question.
    #[test]
    fn granting_from_source_inventory_is_closed() {
        for (name, source) in [
            ("expr.rs", include_str!("expr.rs")),
            ("pattern.rs", include_str!("pattern.rs")),
            ("control_flow.rs", include_str!("control_flow.rs")),
            ("ownership.rs", include_str!("ownership.rs")),
            ("task.rs", include_str!("task.rs")),
            ("mod.rs", include_str!("mod.rs")),
            ("closure_gen.rs", include_str!("closure_gen.rs")),
            ("actor.rs", include_str!("actor.rs")),
        ] {
            assert!(
                !source.contains("granting_for_tests"),
                "{name} names the test-only warrant constructor; a production \
                 mint site would then be able to skip the provenance question"
            );
        }
    }

    /// The compile-time brace is the arity of the three registrars: a mint site
    /// cannot call one without producing a warrant. This pins the other half —
    /// that each registrar actually READS the warrant it demands, rather than
    /// accepting one and ignoring it.
    #[test]
    fn every_owner_registrar_consults_the_warrant_it_demands() {
        let source = include_str!("ownership.rs");
        for registrar in [
            "register_owned_local",
            "register_owned_local_alias",
            "adopt_synthetic_owned_local",
        ] {
            let needle = format!("pub(crate) fn {registrar}(");
            let start = source
                .find(needle.as_str())
                .unwrap_or_else(|| panic!("{registrar} must exist in ownership.rs"));
            // The body up to the next `pub(crate) fn` at item indentation.
            let rest = &source[start + needle.len()..];
            let body = rest
                .find("\n    pub(crate) fn ")
                .map_or(rest, |end| &rest[..end]);
            assert!(
                body.contains("warrant: OwnerMintWarrant"),
                "{registrar} must demand a warrant"
            );
            // Either the registrar reads the answer itself, or it hands the
            // same warrant on to one that does. It may not swallow it.
            assert!(
                body.contains("warrant.withholds_mint()") || body.contains(", warrant)"),
                "{registrar} demands a warrant but neither reads it nor forwards \
                 it; the mint is back to being decided without the provenance \
                 answer"
            );
        }
    }

    #[test]
    fn a_granting_warrant_does_not_withhold_and_records_its_origin() {
        let warrant = OwnerMintWarrant::granting_for_tests();
        assert!(!warrant.withholds_mint());
        assert_eq!(warrant.origin(), OwnerMintOrigin::Initializer);
    }

    /// Every `.rs` file in this crate, walked at run time.
    ///
    /// A hard-coded `include_str!` list is what the ledger scan below must NOT
    /// be: `Builder::owned_locals` is reachable from anywhere in `hew-mir`, so
    /// a file added tomorrow has to be in scope without anyone remembering to
    /// add it. `include_str!` cannot express that; a directory walk can.
    fn crate_sources() -> Vec<(String, String)> {
        fn walk(dir: &std::path::Path, out: &mut Vec<(String, String)>) {
            let entries =
                std::fs::read_dir(dir).unwrap_or_else(|e| panic!("read {}: {e}", dir.display()));
            for entry in entries.flatten() {
                let path = entry.path();
                if path.is_dir() {
                    walk(&path, out);
                } else if path.extension().is_some_and(|ext| ext == "rs") {
                    let rel = path
                        .strip_prefix(env!("CARGO_MANIFEST_DIR"))
                        .unwrap_or(&path)
                        .to_string_lossy()
                        .replace('\\', "/");
                    let text = std::fs::read_to_string(&path)
                        .unwrap_or_else(|e| panic!("read {}: {e}", path.display()));
                    out.push((rel, text));
                }
            }
        }
        let mut out = Vec::new();
        walk(
            &std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("src"),
            &mut out,
        );
        out.sort();
        assert!(
            out.len() > 20,
            "the crate source walk must find the tree, not silently nothing — found {}",
            out.len()
        );
        out
    }

    /// The ledger half of the module claim, which privacy alone does not carry.
    ///
    /// The three registrars are braced at compile time: their signatures demand
    /// an [`OwnerMintWarrant`], which cannot be constructed outside this module
    /// or without asking. The LEDGER they write into is not.
    /// `Builder::owned_locals` is `pub(crate)` and `OwnedLocalEntry` is a
    /// private item of `lower/mod.rs`, which in Rust means every child module of
    /// `lower` can see it — so a direct `push` of an entry onto that ledger,
    /// written in `expr.rs`, COMPILES, mints a scope-exit owner, and answers no
    /// provenance question at all. Nothing in the tree does that today; the
    /// point is that nothing was stopping it, so "a new mint site does not
    /// compile until it produces a warrant" was true of the registrars and not
    /// of the thing they register into.
    ///
    /// This closes it the same way `granting_from_source_inventory_is_closed`
    /// closes the test-only constructor: as a property of the SOURCE, so it
    /// holds for the mint site nobody has written yet. Every site that ADDS an
    /// entry to the ledger must sit inside a function that demands a warrant.
    /// Retraction sites (`set_owned_local_disposition`'s `&mut` walk) are not
    /// scanned: dispositioning an existing entry off the scope-exit set removes
    /// a release, which is the leak direction and is not an owner mint.
    ///
    /// The needles are assembled at run time so this test's own source does not
    /// contain them and match itself — the same device
    /// `every_warrant_constructor_asks_the_authority_or_the_ledger` uses.
    #[test]
    fn every_owned_locals_ledger_mint_site_sits_behind_a_warrant() {
        // Spellings that ADD an entry to the ledger. `retain` / `iter_mut` are
        // deliberately absent — see the doc comment.
        const REGISTRAR_FILE: &str = "src/lower/ownership.rs";
        let field = format!("owned_local{}", "s");
        let adds: Vec<String> = ["push", "extend", "append", "insert", "resize"]
            .iter()
            .map(|verb| format!("{field}.{verb}"))
            .collect();
        let literal = format!("OwnedLocal{}", "Entry {");
        let definition = format!("struct {literal}");
        let mut sites = 0usize;
        for (name, source) in crate_sources() {
            // The declaration of the entry type is not a construction of one.
            let literals = source.matches(&literal).count() - source.matches(&definition).count();
            let count = adds
                .iter()
                .map(|a| source.matches(a.as_str()).count())
                .sum::<usize>();
            if literals == 0 && count == 0 {
                continue;
            }
            assert_eq!(
                name, REGISTRAR_FILE,
                "{name} writes the owned-locals ledger directly. The ledger is \
                 `pub(crate)` and the entry type is visible to every child of \
                 `lower`, so this compiles — and mints a scope-exit owner having \
                 asked the provenance authority nothing. Route it through \
                 `register_owned_local` / `register_owned_local_alias` / \
                 `adopt_synthetic_owned_local`, which demand an \
                 `OwnerMintWarrant`."
            );
            sites += count;
            // Each adding site must sit inside a function that demands one.
            for add in &adds {
                let mut from = 0usize;
                while let Some(hit) = source[from..].find(add.as_str()) {
                    let at = from + hit;
                    let head = source[..at]
                        .rfind("\n    pub(crate) fn ")
                        .or_else(|| source[..at].rfind("\n    fn "))
                        .unwrap_or_else(|| {
                            panic!("{name}: `{add}` at byte {at} is not inside a method")
                        });
                    let signature = &source[head..at];
                    assert!(
                        signature.contains("warrant: OwnerMintWarrant"),
                        "{name}: `{add}` reaches the ledger from a function that \
                         does not demand an `OwnerMintWarrant`; the mint would be \
                         decided without the provenance answer"
                    );
                    from = at + add.len();
                }
            }
        }
        assert_eq!(
            sites, 2,
            "expected exactly the two registrar push sites in {REGISTRAR_FILE}; a \
             different count means a mint site was added or removed and this \
             inventory must be re-read rather than re-numbered"
        );
    }
}
