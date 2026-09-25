//! The ONE `(context, spelling) → declaration` resolution the checker owns.
//!
//! Before rc1-F1 stage D, three producers minted their own owner spelling for
//! the same declaration:
//!
//! * source resolution emitted the complete owner (`std.stream.Sink`);
//! * registry-backed stdlib signatures emitted the loaded module's SHORT owner
//!   (`stream.Sink`), canonicalized only at the few call sites that remembered
//!   to ask;
//! * peer assembly emitted a ROUTE-dependent owner — one declaration in
//!   `pkg/aaa.hew` became `pkg.Tok` when the file was reached through
//!   `import pkg` and `pkg.aaa.Tok` when reached through `import pkg::aaa`.
//!
//! Each disagreement then had to be repaired downstream by a spelling
//! heuristic. This module holds the single ladder — the one stage B/C built for
//! extern contracts — and every producer resolves through it, so a declaration
//! has exactly one identity no matter who asks or how its module was reached.
//!
//! The ladder is authority-ordered; every rung is DECLARATION-PROVEN (it names
//! a source file or a loaded module that actually declares the leaf) and the
//! whole ladder fails closed: an ambiguous spelling returns `None` and the
//! caller keeps the name as written rather than picking a winner.

#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
use super::*;

/// Which producer is asking, and the extra context only that producer holds.
///
/// The ladder itself is identical for every origin — the variant only supplies
/// the rung that needs producer-specific knowledge — which is the property that
/// makes one declaration mint one spelling everywhere.
#[derive(Clone, Copy)]
pub(super) enum NominalOrigin<'a> {
    /// A spelling written in Hew source and resolved in the declaring item's
    /// own lexical context: extern signatures and source type expressions.
    Lexical,
    /// A signature extracted by the module registry. Its owner segment is the
    /// loaded module's SHORT path (`stream.Sink`) rather than the complete
    /// source owner (`std.stream.Sink`), so the loaded module's canonical
    /// identity is supplied here and projected first.
    RegistrySignature { canonical_owner: &'a str },
}

impl Checker {
    /// The canonical owner-qualified declaration a nominal SPELLING denotes in
    /// this context, or `None` when no authority proves one (the caller keeps
    /// the spelling as written and the downstream exact compare fails closed).
    ///
    /// Authority order:
    /// 1. **Registry projection with a known owner** (registry origin only) —
    ///    the loaded module declares the leaf, so its short owner projects to
    ///    the complete source owner.
    /// 2. **Route normalization + the declaring FILE.** Signature resolution
    ///    has usually already qualified a module-local type with the CURRENT
    ///    module's owner, which is route-dependent under peer assembly. Strip
    ///    that qualifier back to the source leaf so the FILE rule decides:
    ///    every route then mints the declaring file's identity.
    /// 3. **The declaring file, then exactly one sibling file** of the
    ///    declaring module (`extern_nominal_file_owner`).
    /// 4. **The checker's canonical resolution** for imported/prelude
    ///    spellings (`canonical_nominal_name`), which refuses ambiguity.
    /// 5. **The module registry's declaration-proven projection**
    ///    (`canonical_method_receiver_identity`) — it refuses bare leaves and
    ///    ambiguous spellings, so it can never recover an owner from text.
    /// 6. **The import binding**, LAST: it recovers only what the proven
    ///    authorities above could not, and never pre-empts them.
    pub(super) fn resolve_nominal_declaration(
        &self,
        origin: NominalOrigin<'_>,
        name: &str,
    ) -> Option<String> {
        if let NominalOrigin::RegistrySignature { canonical_owner } = origin {
            if let Some(identity) = self
                .module_registry
                .canonical_registry_signature_type_identity(name, canonical_owner)
            {
                return Some(identity);
            }
        }
        // Route normalization: `pkg.Tok` (reached via `import pkg`) and
        // `pkg.aaa.Tok` (reached via `import pkg::aaa`) are one declaration in
        // `pkg/aaa.hew`. Strip the current module's own qualifier back to the
        // source leaf and let the FILE rule mint the owner, so the identity does
        // not depend on which route handed the file to the compiler.
        let module_local_leaf = self.current_module.as_deref().and_then(|module| {
            name.strip_prefix(module)
                .and_then(|rest| rest.strip_prefix('.'))
                .filter(|leaf| !leaf.contains('.') && !leaf.contains("::"))
        });
        if let Some(leaf) = module_local_leaf {
            if let Some(owner) = self.extern_nominal_file_owner(leaf) {
                return Some(owner);
            }
            return Some(name.to_string());
        }
        if !name.contains('.') {
            if let Some(owner) = self.extern_nominal_file_owner(name) {
                return Some(owner);
            }
            // A declaration in the root source file keeps its bare canonical
            // identity. `local_type_defs` is the lexical declaration set, not
            // a process-wide leaf registry, so this rung cannot select an
            // imported same-leaf type. Expression callers still apply value
            // binding precedence before consulting the nominal ladder.
            if self.current_module.is_none() && self.local_type_defs.contains(name) {
                return Some(name.to_string());
            }
        }
        if let Some(canonical) = self.canonical_nominal_name(name) {
            return Some(canonical);
        }
        // Registry-loaded stdlib signatures present nominal owners as the
        // loaded module's SHORT spelling (`stream.Sink`), while the same
        // declaration's source module registers the complete owner
        // (`std.stream.Sink`). The module registry is the declaration-proven
        // authority joining those two representations of one loaded
        // declaration.
        if let Some(identity) = self
            .module_registry
            .canonical_method_receiver_identity(name)
        {
            return Some(identity);
        }
        // Import-lexical fallback, LAST: it recovers only what the proven
        // canonical/registry authorities could not, never pre-empts them.
        if !name.contains('.') {
            if let Some(owner) = self.imported_binding_declaration(name) {
                return Some(owner);
            }
        }
        None
    }

    /// IMPORT-lexical declaration authority for a bare spelling: the identity
    /// an import statement actually BOUND under that spelling in this module.
    ///
    /// Two tables back one rung. `import_type_name_aliases` is the durable
    /// published record — keyed by the BOUND (possibly aliased) spelling and
    /// holding the owner-qualified SOURCE identity, so
    /// `import m::{ Tok as ForeignTok }` resolves `ForeignTok` to `m.Tok` and
    /// never to a reconstructed `m.ForeignTok`. It is consulted first because
    /// it outlives registration, and source type expressions resolve after
    /// registration has finished. `extern_nominal_imported_owner` is the
    /// registration-frame view of the same rung, used while the durable record
    /// is still being built.
    ///
    /// Declaration-proven, like every other rung: a published identity is
    /// authority only when it names a registered declaration, a known type, or
    /// a compiler-owned source lifecycle nominal. Anything else falls through
    /// and the ladder keeps failing closed.
    fn imported_binding_declaration(&self, name: &str) -> Option<String> {
        if let Some(identity) = self.import_type_name_aliases.get(&(
            self.current_module.clone(),
            self.current_module_idx,
            name.to_string(),
        )) {
            if self.type_def_at(identity).is_some()
                || self.known_types.contains(identity)
                || crate::lookup_source_owned_lifecycle_type(identity).is_some()
            {
                return Some(identity.clone());
            }
        }
        self.extern_nominal_imported_owner(name)
    }

    /// Resolve EVERY nominal in a registry-extracted signature through the
    /// shared ladder (rc1-F1 stage D, registry producer).
    ///
    /// Extracted ABI signatures spell their owners with the loaded module's
    /// final path segment (`regex.Pattern`) while the same declaration's source
    /// module registers the complete owner (`std.text.regex.Pattern`). Applying
    /// the projection only at the call sites that remembered to ask left the two
    /// spellings alive side by side; this is the one entry point, applied
    /// uniformly at registration. `binders` are the signature's own type
    /// parameters: a bare `A` there is the binder, never a same-spelled type
    /// another module declares.
    pub(super) fn canonicalize_registry_signature(
        &self,
        ty: &crate::ty::Ty,
        canonical_owner: &str,
        binders: &[String],
    ) -> crate::ty::Ty {
        let mapped = ty.map_children_pub(&|child| {
            self.canonicalize_registry_signature(child, canonical_owner, binders)
        });
        let crate::ty::Ty::Named {
            head: crate::TypeHead::Unresolved(spelling),
            args,
        } = mapped
        else {
            return mapped;
        };
        let spelling = spelling.as_str();
        if args.is_empty() && binders.iter().any(|binder| binder == spelling) {
            return crate::ty::Ty::param(spelling);
        }
        let name = self
            .resolve_nominal_declaration(
                NominalOrigin::RegistrySignature { canonical_owner },
                spelling,
            )
            .unwrap_or_else(|| spelling.to_string());
        if let Some(kind) = self
            .resolved_builtin_type(&name)
            .filter(|kind| kind.is_encoding_value())
        {
            return crate::ty::Ty::named_head(crate::TypeHead::Builtin(kind), args);
        }
        self.named_ty_for_key(&name, args)
    }

    /// The canonical identity of an extern signature's nominal type, resolved
    /// AT REGISTRATION in the declaring item's own lexical context (rc1-F1
    /// stage B/C). Thin wrapper over the shared ladder: extern contracts are
    /// the lexical producer.
    pub(super) fn extern_signature_nominal_owner(&self, name: &str) -> Option<String> {
        self.resolve_nominal_declaration(NominalOrigin::Lexical, name)
    }
}

impl Checker {
    /// The nominal head a string registry key names: the declaration the key
    /// was minted under, rendered as the key.
    ///
    /// TRANSITION(A1 commit 3): deleted when the registries are keyed by
    /// identity and every carrier holds the head `Scope::resolve` returned.
    pub(super) fn nominal_head_for_key(&self, key: &str) -> Option<crate::NominalHead> {
        let id = self.lookup_declaration(key)?;
        Some(crate::NominalHead::new(
            crate::NominalId::from_minted_declaration(id),
            self.defs.path(id),
        ))
    }

    /// The named type a string registry key names: its declared nominal, or
    /// the builtin the key spells when no declaration claims it.
    ///
    /// TRANSITION(A1 commit 3): see [`Self::nominal_head_for_key`].
    pub(super) fn named_ty_for_key(&self, key: &str, args: Vec<Ty>) -> Ty {
        if let Some(primitive) = Ty::from_name(key).filter(|_| args.is_empty()) {
            return primitive;
        }
        // An import alias spelling projects to its declaration's owner.
        let canonical = self.canonical_nominal_name(key);
        let key = canonical.as_deref().unwrap_or(key);
        if let Some(nominal) = self.nominal_head_for_key(key) {
            return Ty::named_head(self.head_of_declaration(nominal.id), args);
        }
        if let Some(builtin) = crate::builtin_type::lookup_builtin_type(key) {
            return Ty::named_head(crate::TypeHead::Builtin(builtin), args);
        }
        // A trait written in type position names the trait's declaration; a
        // handler-style trait becomes the actor handle it types.
        if let Some(id) = self
            .lookup_declaration(&self.trait_ref_lookup_key(key))
            .filter(|id| self.defs.kind(*id) == crate::DeclarationKind::Trait)
        {
            return Ty::named_head(
                crate::TypeHead::Nominal(crate::NominalHead::new(
                    crate::NominalId::from_minted_declaration(id),
                    self.defs.path(id),
                )),
                args,
            );
        }
        Ty::Named {
            head: crate::TypeHead::Unresolved(crate::Symbol::intern(key)),
            args,
        }
    }

    /// The head a declared nominal names in this run.
    pub(super) fn head_of_declaration(&self, nominal: crate::NominalId) -> crate::TypeHead {
        if let Some(known) = self.known_declaration(nominal) {
            return known.head();
        }
        crate::TypeHead::of_declaration(&self.defs, nominal)
    }
}

impl Checker {
    /// The known `std.builtins` declaration a nominal is. The embedded
    /// builtin run re-declares the cursors at its own root; those rows are
    /// the same declarations (TRANSITION(P2): deleted with that run, B1).
    fn known_declaration(&self, nominal: crate::NominalId) -> Option<crate::KnownDecl> {
        crate::KnownDecl::of(nominal).or_else(|| {
            let declaration = nominal.declaration();
            (self.checking_embedded_builtins
                && self.defs.module(declaration) == self.defs.root_module())
            .then(|| crate::KnownDecl::from_leaf(self.defs.name(declaration)))
            .flatten()
            .filter(|known| {
                matches!(
                    known,
                    crate::KnownDecl::VecIter | crate::KnownDecl::HashMapIter
                )
            })
        })
    }

    /// The file the checker is currently reading, for the spelling boundary.
    pub(super) fn scope_site(&self) -> Option<super::scope::ScopeSite> {
        Some(super::scope::ScopeSite {
            file: self.current_declaration_module()?,
            span_file: self.current_module_idx,
        })
    }

    /// The head a written type path names, resolved through `Scope`.
    pub(super) fn resolve_type_path_head(
        &mut self,
        path: &hew_parser::ast::Path,
    ) -> Option<crate::TypeHead> {
        let site = self.scope_site()?;
        match self.scopes.resolve(
            &self.env,
            site,
            super::scope::Namespace::Type,
            &path.segments,
        ) {
            Ok(super::scope::Resolution::Nominal(id)) => {
                Some(self.known_declaration(id).map_or_else(
                    || {
                        crate::TypeHead::Nominal(crate::NominalHead::new(
                            id,
                            self.defs.path(id.declaration()),
                        ))
                    },
                    crate::KnownDecl::head,
                ))
            }
            Ok(super::scope::Resolution::Builtin(builtin)) => {
                Some(crate::TypeHead::Builtin(builtin))
            }
            _ => None,
        }
    }
}
