//! Split from `registration.rs`: checker methods, part 4 of 6.
#![allow(
    unused_imports,
    redundant_imports,
    clippy::wildcard_imports,
    reason = "chunk files share the parent module's import header"
)]
use super::super::types::ImportBindingKey;
#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
use super::super::*;
use super::*;
use crate::BuiltinType;
use hew_parser::ast::WireMetadata;

impl Checker {
    /// The owner-qualified SOURCE identity (`owner.Name`) a bare TRAIT reference
    /// resolves to when exactly one imported module PUBLISHED the bare binding
    /// into `importer` and a qualified def for it is registered; `None` otherwise
    /// (local shadow, zero/ambiguous publishers, or no registered def). The exact
    /// mirror of `published_bare_type_qualified` (there are no builtin traits, so
    /// no builtin-exempt branch). The stored value IS the source identity, so an
    /// aliased opt-in (`import m::{ T as U }`) binds `U` to `m.T`, never a
    /// reconstructed `m.U`.
    pub(in crate::check) fn published_bare_trait_qualified(
        &self,
        name: &str,
        importer: Option<&str>,
    ) -> Option<String> {
        if self.local_trait_defs.contains(name) {
            return None;
        }
        let identities = self.published_bare_trait_owners.get(&(
            importer.map(str::to_string),
            self.current_module_idx,
            name.to_string(),
        ))?;
        if identities.len() != 1 {
            return None;
        }
        let qualified = identities.iter().next()?;
        self.trait_defs
            .contains_key(qualified)
            .then(|| qualified.clone())
    }

    /// THE canonical trait-reference resolver. Resolves a trait name spelled in a
    /// given scope to one owner-qualified `ResolvedTraitIdentity`, composing on
    /// the proven `published_bare_trait_owners` single-publisher-or-fail-closed
    /// primitive. Every trait reference — primary trait, bound, and supertrait
    /// edge — routes through here so identity is keyed by OWNER, never by the bare
    /// spelling (which is first-write-wins and polluted under same-name collisions).
    ///
    /// Scope is load-bearing:
    ///   * `Current` — the name is spelled in the importing program; a local
    ///     declaration shadows every imported same-name trait, and the importer's
    ///     own published-bare binding decides the owner.
    ///   * `Declaring { module }` — the name is a SUPERTRAIT edge spelled inside
    ///     `module`'s own declaration (`trait Sub: Base`). It is resolved through
    ///     `module`'s import bindings (the re-export chain to the original owner),
    ///     NEVER the importer's local trait of the same name — which is why the
    ///     local-shadow step is gated on `Current`.
    pub(in crate::check) fn resolve_trait_ref(
        &self,
        name: &str,
        scope: TraitRefScope<'_>,
    ) -> ResolvedTraitIdentity {
        // (1) LOCAL SHADOW — only in the CURRENT module's scope. A supertrait edge
        //     spelled in a DECLARING module is never the importer's local trait of
        //     the same name; gating this on `Current` is the H11 scope correctness.
        if matches!(scope, TraitRefScope::Current) && self.local_trait_defs.contains(name) {
            if let Some(module) = self.current_module.as_deref() {
                let qualified = format!("{module}.{name}");
                if self.trait_defs.contains_key(&qualified) {
                    return self.identity_from_trait_defs_key(&qualified);
                }
            }
            return ResolvedTraitIdentity {
                owner: None,
                source_trait_name: name.to_string(),
                is_local: true,
            };
        }

        // A trait reference written inside a non-root module resolves to that
        // module's own declaration before considering imported or suffix-based
        // candidates. The full current owner is essential here: two nested
        // modules may both be named `render`, and neither may select the other's
        // `Render` declaration through a leaf-only retry.
        if matches!(scope, TraitRefScope::Current) {
            if let Some(module) = self.current_module.as_deref() {
                let qualified = format!("{module}.{name}");
                if self.trait_defs.contains_key(&qualified) {
                    return self.identity_from_trait_defs_key(&qualified);
                }
            }
        }

        // (2) DECLARING-module import binding (closes H11): a re-exported super
        //     edge follows the chain to its original owner. `reexsub`'s
        //     `import reexbase::{ Base }` records `("reexsub","Base") -> "reexbase.Base"`,
        //     so `Sub: Base` resolves `Base` to `reexbase.Base` regardless of what
        //     same-named `Base` the final importer has in scope.
        if let TraitRefScope::Declaring { module } = scope {
            if let Some(source_key) = self
                .trait_import_bindings
                .get(&(module.to_string(), name.to_string()))
            {
                return self.identity_from_trait_defs_key(source_key);
            }
        }

        // (3) PUBLISHED-BARE single-publisher (the promoted primitive), only in
        //     the current scope: a `Declaring` edge's own-module super and
        //     re-exports are handled by (2) and (4) and never consult the final
        //     importer's published map. Handles aliased / imported-bare.
        if matches!(scope, TraitRefScope::Current) {
            if let Some(qualified) =
                self.published_bare_trait_qualified(name, self.current_module.as_deref())
            {
                return self.identity_from_trait_defs_key(&qualified);
            }
        }

        // (4) UNAMBIGUOUS SINGLE-OWNER suffix scan: a non-aliased name whose
        //     binding == its source name, recovered from the single
        //     `{module}.{Trait}` qualified key in `trait_defs`. Also serves a
        //     `Declaring` edge whose super is the declaring module's OWN def
        //     (`{module}.Base` registered) — so no separate own-def-first branch.
        //     With zero or an ambiguous set, leave the owner unresolved and fall
        //     back to the source-name comparison (best-effort for an external
        //     reference not in the loaded graph; the downstream method-set / sig
        //     check fires honestly against an absent/empty set — fail-closed).
        let suffix = format!(".{name}");
        let mut owners: Vec<String> = self
            .trait_defs
            .keys()
            .filter_map(|k| k.strip_suffix(&suffix))
            .filter(|module| {
                !module.is_empty()
                    && (self.modules.contains(*module)
                        || self.canonical_std_module_sources.contains(*module)
                        || self.canonical_std_root_sources.contains(*module)
                        || self
                            .current_module
                            .as_deref()
                            .is_some_and(|current| crate::short_name(current) == *module))
            })
            .map(|module| {
                self.module_import_bindings
                    .get(&(
                        self.current_module.clone(),
                        self.current_module_idx,
                        module.to_string(),
                    ))
                    .cloned()
                    .unwrap_or_else(|| module.to_string())
            })
            .collect();
        owners.sort_unstable();
        owners.dedup();
        let owner = match owners.as_slice() {
            [single] => Some(single.clone()),
            _ => None,
        };
        ResolvedTraitIdentity {
            owner,
            source_trait_name: name.to_string(),
            is_local: false,
        }
    }

    /// Return the registry key for a trait name written in the current module.
    ///
    /// Trait-object bounds retain their source spelling for HIR/codegen, while
    /// checker lookup must use the owning module's canonical key.
    pub(in crate::check) fn trait_ref_lookup_key(&self, name: &str) -> String {
        let identity = self.resolve_trait_ref(name, TraitRefScope::Current);
        self.trait_defs_key_for_identity(&identity)
    }

    pub(in crate::check) fn resolved_trait_defaults(
        &mut self,
    ) -> HashMap<crate::DefId, Vec<super::types::ResolvedTraitDefault>> {
        let saved_module = self.current_module.clone();
        let saved_file = self.current_module_idx;
        let mut declarations: Vec<_> = self
            .trait_defs
            .iter()
            .filter_map(|(key, info)| {
                self.lookup_declaration(key)
                    .cloned()
                    .map(|id| (key.clone(), id, info.clone()))
            })
            .collect();
        declarations.sort_by_key(|(key, id, _)| (key != id.full_path(), key.clone()));
        let mut defaults = HashMap::new();
        let mut visited = HashSet::new();
        for (key, trait_id, info) in declarations {
            if !visited.insert(trait_id.clone()) {
                continue;
            }
            self.current_module.clone_from(&info.source_module);
            self.current_module_idx = info.file_index;
            let identity = self.identity_from_trait_defs_key(&key);
            let mut names: HashSet<String> = info.methods.iter().map(|m| m.name.clone()).collect();
            self.collect_super_trait_method_names(&key, &mut names, &mut HashSet::new());
            for name in names {
                let Some(owner) = self.resolve_declaring_trait_identity(&identity, &key, &name)
                else {
                    continue;
                };
                let owner_key = self.trait_defs_key_for_identity(&owner);
                let Some(owner_id) = self.lookup_declaration(&owner_key) else {
                    continue;
                };
                let method_key = format!("{}::{name}", owner_id.full_path());
                let Some(ids) = self.trait_method_ids.get(&method_key).cloned() else {
                    continue;
                };
                self.trait_method_ids
                    .insert(format!("{}::{name}", trait_id.full_path()), ids);
            }
            let bodies = info
                .methods
                .into_iter()
                .filter(|method| method.body.is_some())
                .filter_map(|method| {
                    let (_, method_id) = self
                        .trait_method_ids
                        .get(&format!("{}::{}", trait_id.full_path(), method.name))?
                        .clone();
                    Some(super::types::ResolvedTraitDefault {
                        trait_id: trait_id.clone(),
                        method_id,
                        method,
                        source_module: info.source_module.clone(),
                        file_index: info.file_index,
                    })
                })
                .collect();
            defaults.insert(trait_id, bodies);
        }
        for (binding, trait_id) in &self.trait_bindings {
            let prefix = format!("{}::", trait_id.full_path());
            for (key, ids) in &self.trait_method_ids {
                if let Some(method) = key.strip_prefix(&prefix) {
                    self.trait_method_ids_by_binding.insert(
                        (
                            binding.0.clone(),
                            binding.1,
                            binding.2.clone(),
                            method.to_string(),
                        ),
                        ids.clone(),
                    );
                }
            }
        }
        self.current_module = saved_module;
        self.current_module_idx = saved_file;
        defaults
    }

    /// The set of method names a trait requires an impl to provide, resolved
    /// through the trait's OWNER-QUALIFIED identity so a same-name collision
    /// cannot leak a neighbouring trait's method set. A method with a default
    /// body is optional (impls may omit it), so only bodyless methods of the
    /// resolved trait ITSELF are "required". The "known" set additionally
    /// includes the trait's whole super-trait chain: an impl of a sub-trait may
    /// legitimately provide a super-trait method inline (`trait Sub: Super` →
    /// `impl Sub for T { fn <super-method> … }`), so such a method must not be
    /// flagged as extraneous. Super-trait REQUIRED methods are NOT folded into
    /// `required` here — they are satisfied by a separate `impl Super for T` (the
    /// idiomatic form) or enforced at the bound site, and folding them in would
    /// falsely reject that separate-impl pattern.
    ///
    /// Returns `(required, known)`, or `None` when the trait reference does not
    /// resolve to a known trait (a separate diagnostic covers an unknown bound).
    pub(super) fn trait_required_and_known_methods(
        &self,
        identity: &ResolvedTraitIdentity,
    ) -> Option<(HashSet<String>, HashSet<String>)> {
        // The one owner-qualified `trait_defs` key for this identity; collision-
        // free even when a same-name trait registered the bare key first. The
        // local-shadow and unresolved paths fall back to the identity's SOURCE
        // name (authoritative for a local trait, best-effort for an unresolved
        // reference) — derived by the single `trait_defs_key_for_identity` home.
        let lookup_key = self.trait_defs_key_for_identity(identity);
        let info = self.trait_defs.get(&lookup_key)?;

        let mut required = HashSet::new();
        let mut known = HashSet::new();
        for m in &info.methods {
            known.insert(m.name.clone());
            if m.body.is_none() {
                required.insert(m.name.clone());
            }
        }
        // Fold every (transitive) super-trait's declared methods into `known`
        // so an inline super-trait method is permitted, not flagged as extra.
        let mut visited: HashSet<String> = HashSet::new();
        self.collect_super_trait_method_names(&lookup_key, &mut known, &mut visited);
        Some((required, known))
    }

    /// Resolve a supertrait edge written inside `module_short` to its
    /// OWNER-QUALIFIED `trait_defs` key, through the one canonical resolver in the
    /// `Declaring` scope. A `trait Sub: Base` declaration spells `Base` bare, but
    /// it names the trait `module_short` itself resolves `Base` to: its OWN
    /// same-package `Base` (`{module_short}.Base`, found by the resolver's
    /// single-owner suffix scan), or a RE-IMPORTED `Base`
    /// (`import other::{ Base }`, followed through `module_short`'s import bindings
    /// to the original owner `other.Base`).
    ///
    /// The re-imported case is the H11 case the old `{module_short}.Base`-only
    /// check missed: there is no `{module_short}.Base` def for a re-imported super,
    /// so it fell back to the bare name and bound whatever `Base` the final
    /// importer had in scope (a collision-unsafe fail-open). When the super is
    /// genuinely external to the loaded graph, the resolver yields no owner and
    /// this returns the source name; the downstream method-set / signature check
    /// then fires honestly against an absent set — fail-closed, never accept-all.
    pub(super) fn resolve_super_trait_edge(&self, module_short: &str, super_name: &str) -> String {
        let identity = self.resolve_trait_ref(
            super_name,
            TraitRefScope::Declaring {
                module: module_short,
            },
        );
        self.trait_defs_key_for_identity(&identity)
    }

    /// Walk the (transitive) super-trait chain of `trait_key`, inserting every
    /// super-trait's declared method name into `known`. `visited` guards against
    /// cycles. Super-trait edges are owner-qualified `trait_defs` keys (an
    /// imported `Sub`'s edge points at `{owner}.Base`, never the importer's bare
    /// `Base`; a local trait's edge is its bare local key, which is authoritative
    /// for a local trait), so the recursion resolves each super against its
    /// defining trait, collision-free.
    pub(super) fn collect_super_trait_method_names(
        &self,
        trait_key: &str,
        known: &mut HashSet<String>,
        visited: &mut HashSet<String>,
    ) {
        if !visited.insert(trait_key.to_string()) {
            return;
        }
        let Some(supers) = self.trait_super.get(trait_key) else {
            return;
        };
        for super_name in supers.clone() {
            let resolved_super = if self.trait_defs.contains_key(&super_name) {
                super_name
            } else if let Some((declaring_module, _)) = trait_key.rsplit_once('.') {
                // Early module registration can retain a super edge's source
                // spelling before the import binding is published. Resolve it
                // in the declaring trait's exact module now; never consult the
                // final importer's bare namespace or a suffix/leaf retry.
                self.resolve_super_trait_edge(declaring_module, &super_name)
            } else {
                super_name
            };
            if let Some(super_info) = self.trait_defs.get(&resolved_super) {
                for m in &super_info.methods {
                    known.insert(m.name.clone());
                }
            }
            self.collect_super_trait_method_names(&resolved_super, known, visited);
        }
    }

    /// The `trait_defs` key for a resolved trait identity: the owner-qualified
    /// `{owner}.{source}` key when an owner resolved and that key is registered,
    /// otherwise the identity's SOURCE name (authoritative for a local trait, best
    /// effort for an unresolved reference). Mirrors the `lookup_key` derivation in
    /// `trait_required_and_known_methods`. The fallback is the source name — not
    /// the sub-trait the impl wrote — so a declaring SUPERTRAIT identity reached
    /// through the super chain keys on the supertrait's own name (`Base`), never
    /// the sub-trait's (`Sub`).
    pub(super) fn trait_defs_key_for_identity(&self, identity: &ResolvedTraitIdentity) -> String {
        identity
            .owner
            .as_ref()
            .map(|owner| {
                let canonical_owner = self
                    .module_import_bindings
                    .get(&(
                        self.current_module.clone(),
                        self.current_module_idx,
                        owner.clone(),
                    ))
                    .map_or(owner.as_str(), String::as_str);
                format!("{canonical_owner}.{}", identity.source_trait_name)
            })
            .filter(|q| self.trait_defs.contains_key(q))
            .unwrap_or_else(|| identity.source_trait_name.clone())
    }

    /// The owner-qualified `trait_defs` key for a bare trait-bound name spelled in
    /// an `impl <Trait> for <Type>` (the CURRENT module's scope). The impl-side
    /// associated-type / default-method machinery (required-assoc enforcement,
    /// default-assoc collection, default-method registration) keys its
    /// `trait_defs` lookups on this instead of the bare `tb.name`, so a same-name
    /// trait imported by a *different* module cannot poison the global bare
    /// `trait_defs[name]` entry and let an impl skip a required `type` or inherit
    /// the wrong defaults. Routes through the one canonical conformance resolver,
    /// exactly as `check_impl_method_set_against_trait` does for the method set.
    pub(in crate::check) fn trait_defs_key_for_bound(&self, name: &str) -> String {
        let identity = self.resolve_trait_conformance_identity(name);
        self.trait_defs_key_for_identity(&identity)
    }

    /// Resolve which trait DECLARES `method_name` for an `impl <Trait>`: the
    /// primary trait when it declares the method directly, otherwise the
    /// supertrait in the OWNER-QUALIFIED super chain that declares it (an impl of
    /// a sub-trait may provide an inherited supertrait method inline). Returns the
    /// declaring trait's owner-qualified identity so the caller's signature
    /// lookup and trait-owner canonicalization key off the SUPERTRAIT's owner —
    /// not the sub-trait's — when the method is inherited. Without this, an inline
    /// supermethod is never found on the primary trait and its signature goes
    /// unchecked (a fail-open: a wrong-signature inherited method is accepted).
    ///
    /// `primary_key` is the primary trait's `trait_defs` key (owner-qualified).
    /// The walk follows `trait_super` (owner-qualified edges) and matches each
    /// super against its `trait_defs` entry, so a same-name supertrait collision
    /// resolves through the defining owner, never the importer namespace.
    pub(super) fn resolve_declaring_trait_identity(
        &self,
        primary_identity: &ResolvedTraitIdentity,
        primary_key: &str,
        method_name: &str,
    ) -> Option<ResolvedTraitIdentity> {
        if self
            .trait_defs
            .get(primary_key)
            .is_some_and(|info| info.methods.iter().any(|m| m.name == method_name))
        {
            return Some(ResolvedTraitIdentity {
                owner: primary_identity.owner.clone(),
                source_trait_name: primary_identity.source_trait_name.clone(),
                is_local: primary_identity.is_local,
            });
        }
        let mut visited: HashSet<String> = HashSet::new();
        let mut stack: Vec<String> = self
            .trait_super
            .get(primary_key)
            .cloned()
            .unwrap_or_default();
        while let Some(super_key) = stack.pop() {
            if !visited.insert(super_key.clone()) {
                continue;
            }
            let declares = self
                .trait_defs
                .get(&super_key)
                .is_some_and(|info| info.methods.iter().any(|m| m.name == method_name));
            if declares {
                return Some(self.identity_from_trait_defs_key(&super_key));
            }
            if let Some(supers) = self.trait_super.get(&super_key) {
                stack.extend(supers.iter().cloned());
            }
        }
        None
    }

    /// Recover a trait identity from a `trait_defs` key. An owner-qualified
    /// `{module}.{Trait}` key registered by the checker yields
    /// `owner = Some(module)`, `source = Trait`; a bare key yields a local
    /// identity (`is_local = true`, no owner). Used to re-anchor the signature
    /// lookup on a declaring SUPERTRAIT reached through the super chain.
    pub(super) fn identity_from_trait_defs_key(&self, key: &str) -> ResolvedTraitIdentity {
        match key.rsplit_once('.') {
            Some((module, source)) if self.trait_defs.contains_key(key) => ResolvedTraitIdentity {
                owner: Some(module.to_string()),
                source_trait_name: source.to_string(),
                is_local: false,
            },
            _ => ResolvedTraitIdentity {
                owner: None,
                source_trait_name: key.to_string(),
                is_local: true,
            },
        }
    }

    /// Validate that an `impl <Trait> for <Type>` provides EXACTLY the trait's
    /// method set: every required (bodyless) trait method present, and no method
    /// that is not declared on the trait. Keyed off the trait's owner-qualified
    /// identity (`resolve_trait_conformance_identity`), so a same-name trait
    /// collision can never leak a neighbour's method set into the comparison.
    /// Per-method signature equivalence is enforced separately by
    /// `check_impl_method_against_trait`.
    pub(in crate::check) fn check_impl_method_set_against_trait(
        &mut self,
        type_name: &str,
        trait_bound: &TraitBound,
        impl_methods: &[FnDecl],
        impl_span: &Span,
    ) {
        let trait_name = &trait_bound.name;
        let identity = self.resolve_trait_conformance_identity(trait_name);
        let trait_key = self.trait_defs_key_for_identity(&identity);
        self.mark_imported_trait_used(self.current_module.as_deref(), trait_name);
        if let Some(declaration) = self.lookup_declaration(&trait_key).cloned() {
            self.trait_bindings.insert(
                (
                    self.current_module.clone(),
                    self.current_module_idx,
                    trait_name.clone(),
                ),
                declaration,
            );
        }
        let Some((required, known)) = self.trait_required_and_known_methods(&identity) else {
            // D429: no declaration in scope defines this trait, so there is no
            // method set to check the impl against. Accepting it would register
            // the impl's methods under a contract that does not exist.
            //
            // Marker traits (`Eq`, `Hash`, `Copy`, ...) carry no declared
            // method set, so a missing `trait_defs` entry is their normal
            // state and says nothing about whether the name resolves.
            let leaf = trait_name.rsplit('.').next().unwrap_or(trait_name);
            if crate::traits::MarkerTrait::from_name(leaf).is_some() {
                return;
            }
            self.report_error(
                TypeErrorKind::UnknownTraitInImpl {
                    trait_name: trait_name.clone(),
                    type_name: type_name.to_string(),
                },
                impl_span,
                format!("cannot find trait `{trait_name}` in this scope"),
            );
            return;
        };

        let type_identity = self.trait_impl_type_identity(type_name);
        let mut provided: HashSet<String> = impl_methods.iter().map(|m| m.name.clone()).collect();
        // Split trait surfaces are cumulative on one exact nominal identity.
        // This is how a subtrait that redeclares inherited methods is satisfied
        // by the already-registered supertrait impl for the same type. Never
        // aggregate by the type leaf: sibling modules may both define `Value`.
        for ((implemented_type, _), methods) in &self.trait_impl_method_names {
            if implemented_type == &type_identity {
                provided.extend(methods.iter().cloned());
            }
        }

        // INHERITED method names: every method declared by a (transitively
        // reachable) SUPERTRAIT of this trait. A sub-trait may REDECLARE its
        // supertrait's methods (`trait Sub: Base { fn base(self); ... }`), which
        // makes them bodyless-required on `Sub` even though they belong to
        // `Base`. Such an inherited requirement is satisfied by a SEPARATE
        // `impl Base for T` block (not the `impl Sub for T` block), exactly as a
        // non-redeclared inherited method is — supertrait conformance is resolved
        // lazily at the call site (`no method X on T` if no impl supplies it),
        // never eagerly forced onto the sub-trait's own impl block. Dropping an
        // inherited method from `missing` here keeps redeclared and non-redeclared
        // inherited methods behaving identically; the per-method signature check
        // (`resolve_declaring_trait_identity`) still validates any inline copy.
        let lookup_key = self.trait_defs_key_for_identity(&identity);
        let mut inherited: HashSet<String> = HashSet::new();
        let mut inherited_visited: HashSet<String> = HashSet::new();
        self.collect_super_trait_method_names(&lookup_key, &mut inherited, &mut inherited_visited);

        // Missing: a required (bodyless) trait method the impl never provided AND
        // that is not inherited from a supertrait (the supertrait's own impl
        // block carries that obligation).
        let mut missing: Vec<String> = required
            .iter()
            .filter(|name| !provided.contains(*name))
            .filter(|name| !inherited.contains(name.as_str()))
            .cloned()
            .collect();
        missing.sort_unstable();
        if !missing.is_empty() {
            self.report_error_with_note(
                TypeErrorKind::TraitImplMissingMethods {
                    trait_name: trait_name.clone(),
                    type_name: type_name.to_string(),
                    methods: missing.clone(),
                },
                impl_span,
                format!(
                    "impl `{trait_name}` for `{type_name}` is missing required method(s): {}",
                    missing.join(", ")
                ),
                impl_span,
                format!(
                    "trait `{trait_name}` requires {}",
                    if missing.len() == 1 {
                        format!("method `{}`", missing[0])
                    } else {
                        format!("methods {}", missing.join(", "))
                    }
                ),
            );
        }

        // Extra: an impl method that is not declared on the trait at all.
        let mut extra: Vec<String> = impl_methods
            .iter()
            .map(|m| m.name.clone())
            .filter(|name| !known.contains(name))
            .collect();
        extra.sort_unstable();
        extra.dedup();
        if !extra.is_empty() {
            self.report_error_with_note(
                TypeErrorKind::TraitImplExtraMethods {
                    trait_name: trait_name.clone(),
                    type_name: type_name.to_string(),
                    methods: extra.clone(),
                },
                impl_span,
                format!(
                    "impl `{trait_name}` for `{type_name}` declares method(s) not on the trait: {}",
                    extra.join(", ")
                ),
                impl_span,
                format!("trait `{trait_name}` declares no such method(s)"),
            );
        }
    }

    /// Enforce that an impl method's signature matches the declared trait
    /// method's signature, after substituting `Self`, trait type parameters,
    /// and the impl's associated-type aliases. Q004 / LESSONS
    /// `diagnostic-trust`: emits at the impl method's local span so the user
    /// sees the actual divergence instead of a confusing
    /// "type does not satisfy trait" cascaded from a later call site.
    ///
    /// Silent (no diagnostic) when:
    /// * the trait method is not declared (the impl-site method-SET check in
    ///   `check_impl_method_set_against_trait` reports an extra method; this
    ///   per-method check only enforces equivalence of methods the trait
    ///   declares);
    /// * the trait signature was not registered (already produced a
    ///   diagnostic, would double-fire);
    /// * any side of the comparison contains `Ty::Error` (cascading
    ///   suppression — see the `cascading-Ty::Error` invariant);
    /// * the receiver-skip stripped a different number of params on either
    ///   side because the impl elided the receiver (treat as receiver-kind
    ///   mismatch and report).
    #[allow(
        clippy::too_many_lines,
        reason = "single-source-of-truth for the impl-vs-trait sig comparison; \
                  factoring would obscure the substitution / projection / \
                  renaming order that the comparison depends on"
    )]
    pub(in crate::check) fn check_impl_method_against_trait(
        &mut self,
        type_name: &str,
        self_type_args: &[Ty],
        trait_bound: &TraitBound,
        method: &FnDecl,
        impl_sig: &FnSig,
    ) {
        // Two `Ty::Named` that share a name + args but disagree only on the
        // `builtin` discriminator denote the same nominal type: the tag is a
        // derived property of the name, stamped when a type resolves against a
        // canonical builtin source and left `None` when the same name resolves
        // against its in-scope user definition. The std dual-surface error
        // enums (`CloseError`, `SendError`, …) can hit this when a trait method
        // carries the local-enum form (`builtin: None`) while an implementation
        // resolves the same name through a builtin surface.
        // Re-derive the tag from the name on both sides so trait-conformance
        // compares nominal identity rather than the incidental resolution path.
        //
        // Under qualified-by-default the trait declaration records its sibling
        // types by their BARE name (as written inside the defining module) while
        // an importer's `impl` spells the same type through its module qualifier
        // (`module.CloseError`). These name the one type, so both spellings
        // must canonicalize to a single DEFINING-MODULE-qualified identity before
        // the comparison — never to a bare name. Stripping any known-module
        // prefix and comparing bare names is unsound: it collapses two distinct
        // nominal types that merely share a bare name across modules
        // (`closableerr.CloseError` vs `closableerr2.CloseError`), accepting an
        // impl that returns the wrong module's type. Instead:
        //   * an already module-qualified name keeps its qualifier (it is an
        //     explicit, unambiguous identity);
        //   * a bare name written in the TRAIT DECLARATION denotes the trait's
        //     own defining module's type, so it ALWAYS qualifies against that
        //     module when the module defines it. The trait side is canonicalized
        //     with `preserve_local_shadow = false` — the importer's local type
        //     names are irrelevant to what the trait declaration requires;
        //   * a bare name on the IMPL/ACTUAL side is canonicalized with
        //     `preserve_local_shadow = true`: if it shadows a local type in the
        //     impl's scope it stays bare so the local identity is preserved (and
        //     so a local `CloseError` correctly MISMATCHES the trait's
        //     `closableerr.CloseError` rather than being conflated with it).
        //
        // The carve-out MUST be side-specific. Applying the local-shadow filter
        // to BOTH sides with one shared predicate is fail-open: the trait's bare
        // `CloseError` would also be left bare when the importer has a local
        // `CloseError`, so it would compare EQUAL to the impl's local type
        // instead of to the trait owner's required `closableerr.CloseError`,
        // falsely accepting a wrong-module impl.
        //
        // The user-facing diagnostics still render the original, untouched types.
        //
        // `trait_owner` is the trait's defining module (`Some("closableerr")`)
        // or `None` for a root/local trait. `ctx` carries the in-scope module
        // set, the registered-type predicate, and the local-shadow predicate so
        // the recursion needs no `&self` borrow held across the later mutable
        // error-reporting calls. `preserve_local_shadow` selects the side.
        fn canonicalize_type_identity(
            ty: &Ty,
            ctx: &TraitSigCanonCtx,
            preserve_local_shadow: bool,
        ) -> Ty {
            let rec = |t: &Ty| canonicalize_type_identity(t, ctx, preserve_local_shadow);
            match ty {
                Ty::Tuple(elems) => Ty::Tuple(elems.iter().map(rec).collect()),
                Ty::Array(elem, n) => Ty::Array(Box::new(rec(elem)), *n),
                Ty::Slice(elem) => Ty::Slice(Box::new(rec(elem))),
                Ty::Named { name, args, .. } => {
                    let canonical = if ctx.modules.iter().any(|module| {
                        name.strip_prefix(module)
                            .is_some_and(|suffix| suffix.starts_with('.'))
                    }) {
                        // Already module-qualified by a known module: keep the
                        // complete qualifier as the type identity. Module
                        // owners may themselves be dotted package paths (for
                        // example `hew.closableerr`), so looking only at the
                        // first path segment would mistake
                        // `hew.closableerr.CloseError` for a bare type.
                        name.clone()
                    } else {
                        // Bare name: qualify against the trait's defining module
                        // when that module defines it. On the impl/actual side a
                        // local shadow is preserved (left bare); on the trait
                        // side the local-shadow carve-out does NOT apply, so a
                        // bare trait-declared name always qualifies to its owner.
                        // Otherwise (builtin, type param, or — on the impl side —
                        // a genuine local) leave it bare so its identity survives.
                        ctx.trait_owner
                            .filter(|_| !(preserve_local_shadow && (ctx.is_local)(name)))
                            .map(|owner| format!("{owner}.{name}"))
                            .filter(|qualified| (ctx.defines_qualified)(qualified))
                            .unwrap_or_else(|| name.clone())
                    };
                    // Primitive types (i64, bool, f64, …) are represented in two
                    // ways: as the flat `Ty::I64` / `Ty::Bool` / … variants (from
                    // `resolve_type_expr` hitting the `Ty::from_name` fast-path)
                    // and as `Ty::Named { name: "i64", builtin: Some(I64), … }`
                    // (from `Ty::normalize_named` when a `Self` annotation is
                    // eagerly substituted via `current_self_type` during
                    // `lookup_trait_method` resolution).  Both representations are
                    // semantically identical, but `Ty::Named { … } != Ty::I64` as
                    // Rust enum discriminants, so trait-impl signature comparison
                    // falsely rejects them.
                    //
                    // Collapsing the canonical name to the flat primitive variant
                    // here is the canonical reconcile point: both the expected
                    // (trait) and actual (impl) sides pass through this function
                    // before comparison, so a single normalization here handles
                    // every path (fn_sigs registered before impl, lookup_trait_method
                    // eager substitution, and substitute_trait_sig_for_impl output).
                    // Only fires for zero-arg names (primitives never carry type args).
                    let canonical_args = args.iter().map(rec).collect::<Vec<_>>();
                    if canonical_args.is_empty() {
                        if let Some(prim) = Ty::from_name(&canonical) {
                            return prim;
                        }
                    }
                    Ty::normalize_named(canonical, canonical_args)
                }
                Ty::Function {
                    capabilities,
                    params,
                    ret,
                } => Ty::Function {
                    capabilities: *capabilities,
                    params: params.iter().map(rec).collect(),
                    ret: Box::new(rec(ret)),
                },
                Ty::Closure {
                    capabilities,
                    params,
                    ret,
                    captures,
                    identity,
                } => Ty::Closure {
                    capabilities: *capabilities,
                    params: params.iter().map(rec).collect(),
                    ret: Box::new(rec(ret)),
                    captures: captures.iter().map(rec).collect(),
                    identity: identity.clone(),
                },
                Ty::Pointer {
                    is_mutable,
                    pointee,
                } => Ty::Pointer {
                    is_mutable: *is_mutable,
                    pointee: Box::new(rec(pointee)),
                },
                Ty::Borrow { pointee } => Ty::Borrow {
                    pointee: Box::new(rec(pointee)),
                },
                Ty::Task(inner) => Ty::Task(Box::new(rec(inner))),
                other => other.clone(),
            }
        }

        let trait_name = trait_bound.name.clone();

        // Resolve the trait as written in the impl (`impl C for X`) to its
        // OWNER-QUALIFIED identity, uniformly across all three reference kinds
        // (aliased / imported-bare, local-root shadow, unambiguous single-owner
        // import). The bare `Trait::method` key is first-write-wins and pollutes
        // under same-name collisions, so it is NEVER the authority when the
        // identity resolves to a real owner or a local trait. See
        // `resolve_trait_conformance_identity`.
        let primary_identity = self.resolve_trait_conformance_identity(&trait_name);
        let primary_key = self.trait_defs_key_for_identity(&primary_identity);

        // An `impl Sub for T` (where `trait Sub: Base`) may provide an inherited
        // SUPERTRAIT method (`base`) inline. Resolve which trait actually DECLARES
        // this method — the primary trait, or the declaring supertrait reached
        // through the OWNER-QUALIFIED super chain — and key the signature check
        // off THAT trait's identity. Skipping inherited methods here is a
        // fail-open: a wrong-signature inline supermethod would never be compared.
        let Some(identity) =
            self.resolve_declaring_trait_identity(&primary_identity, &primary_key, &method.name)
        else {
            return;
        };
        // The declaring trait's `trait_defs` entry supplies the trait method AST
        // (its type params, span, and receiver shape) for the comparison below.
        let declaring_key = self.trait_defs_key_for_identity(&identity);
        let Some(trait_info) = self.trait_defs.get(&declaring_key).cloned() else {
            return;
        };
        let Some(trait_method) = trait_info
            .methods
            .iter()
            .find(|m| m.name == method.name)
            .cloned()
        else {
            return;
        };

        // Materialise the trait method's required signature through the resolved
        // identity, collision-free:
        //   * a resolved owner reads the owner-qualified `fn_sigs` key
        //     `m.Trait::method` (always registered for module traits);
        //   * a LOCAL trait derives the signature from its own `TraitInfo`
        //     method AST — the polluted bare `fn_sigs` key may hold an imported
        //     same-name trait's signature (first-write-wins), so it must never be
        //     consulted for a local trait;
        //   * an unresolved reference falls back to the scoped/bare key (the
        //     genuinely-unambiguous case, where no collision is possible).
        let trait_sig = if let Some(owner) = identity.owner.as_ref() {
            let owner_key = format!("{owner}.{}::{}", identity.source_trait_name, method.name);
            match self.fn_sigs.get(&owner_key).cloned() {
                Some(sig) => sig,
                None => return,
            }
        } else if identity.is_local {
            // A local/root trait resolves its required signature from its own
            // `trait_defs` entry (last-write-wins → authoritative) rather than the
            // polluted bare `fn_sigs` key. `lookup_trait_method` strips the
            // receiver and projects `Self::Bar`, mirroring what
            // `register_trait_method_sig` would have written. The DECLARING trait
            // is keyed (`identity.source_trait_name`): for an inline supermethod
            // of a local sub-trait this is the supertrait that declares it, not
            // the sub-trait written in the impl.
            match self.lookup_trait_method(&identity.source_trait_name, &method.name) {
                Some(sig) => sig,
                None => return,
            }
        } else {
            let trait_method_key = format!("{}::{}", identity.source_trait_name, method.name);
            let scoped_trait_key =
                scoped_module_item_name(self.current_module.as_deref(), &trait_method_key)
                    .unwrap_or_else(|| trait_method_key.clone());
            match self
                .fn_sigs
                .get(&scoped_trait_key)
                .or_else(|| self.fn_sigs.get(&trait_method_key))
                .cloned()
            {
                Some(sig) => sig,
                None => return,
            }
        };

        // The trait's defining module anchors how a bare type name written in
        // the trait declaration is canonicalized. Imported traits carry that
        // owner explicitly. A trait local to a non-root module deliberately
        // resolves as `is_local`, but its sibling type names still belong to
        // the exact current module; only a root-local trait has no owner. For an
        // inline supermethod this is the SUPERTRAIT's owner (its declaration's
        // bare sibling types belong to its module), not the sub-trait's.
        let trait_owner_module = identity.owner.as_ref().map_or_else(
            || {
                identity
                    .is_local
                    .then(|| self.current_module.clone())
                    .flatten()
            },
            |owner| {
                Some(
                    self.module_import_bindings
                        .get(&(
                            self.current_module.clone(),
                            self.current_module_idx,
                            owner.clone(),
                        ))
                        .cloned()
                        .unwrap_or_else(|| owner.clone()),
                )
            },
        );

        // Build trait-type-param substitution map.
        let mut trait_param_map: HashMap<String, Ty> = HashMap::new();
        if let Some(args) = trait_bound.type_args.as_ref() {
            for (param_name, arg_expr) in trait_info.type_params.iter().zip(args.iter()) {
                let resolved = self.resolve_type_expr(arg_expr);
                trait_param_map.insert(param_name.clone(), resolved);
            }
        } else if self
            .lang_items
            .get("index")
            .is_some_and(|binding| binding.trait_id.full_path() == declaring_key)
            && trait_info.type_params.len() == 1
            && matches!(method.name.as_str(), "get" | "at")
        {
            // Legacy `impl Index for T` elides `Index<Idx>` and determines
            // `Idx` from the handler's concrete index parameter. Keep that
            // compatibility at the exact lang-item declaration only; applying
            // it to arbitrary generic traits would silently infer omitted
            // arguments from whichever method happened to be checked first.
            if let Some(actual_index_ty) = impl_sig.params.first() {
                trait_param_map.insert(
                    trait_info.type_params[0].clone(),
                    self.subst.resolve(actual_index_ty),
                );
            }
        }

        // Construct `impl_self` as the canonical `Ty` for the implementing
        // type. For primitive types (i64, bool, f64, …) `Ty::from_name`
        // returns the flat primitive variant (e.g. `Ty::I64`), which is what
        // the impl's annotation resolves to via
        // `resolve_registered_annotation_ty_no_holes`.  Using `Ty::Named` for
        // a primitive name produces a different enum variant than the impl's
        // resolved param type, causing a false "has type i64 but requires i64"
        // mismatch on non-receiver Self params.  `Ty::from_name` is the single
        // source of truth for primitive name → variant; non-primitive names
        // that have no flat variant (user-defined types, generics) take the
        // `Ty::Named` path as before.  Primitive types never carry type args,
        // so the from_name path only fires when self_type_args is empty.
        let impl_self_name = self
            .current_module
            .as_deref()
            .filter(|_| !type_name.contains('.'))
            .map(|module| format!("{module}.{type_name}"))
            .filter(|qualified| self.type_defs.contains_key(qualified))
            .unwrap_or_else(|| type_name.to_string());
        let impl_self = if self_type_args.is_empty() {
            Ty::from_name(&impl_self_name).unwrap_or_else(|| Ty::Named {
                builtin: None,
                name: impl_self_name.clone(),
                args: Vec::new(),
            })
        } else {
            Ty::Named {
                builtin: None,
                name: impl_self_name.clone(),
                args: self_type_args.to_vec(),
            }
        };

        // Materialise the expected impl-side signature.
        let expected_params: Vec<Ty> = trait_sig
            .params
            .iter()
            .map(|p| {
                let projected = self.substitute_trait_sig_for_impl(p, &impl_self, &trait_param_map);
                Self::rename_method_type_params(
                    &projected,
                    trait_method.type_params.as_ref(),
                    method.type_params.as_ref(),
                )
            })
            .collect();
        let expected_return = {
            let projected = self.substitute_trait_sig_for_impl(
                &trait_sig.return_type,
                &impl_self,
                &trait_param_map,
            );
            Self::rename_method_type_params(
                &projected,
                trait_method.type_params.as_ref(),
                method.type_params.as_ref(),
            )
        };

        // Cascading-Ty::Error suppression: if anything in expected or actual
        // is Error, skip — earlier diagnostics already explain the failure.
        let any_error = expected_params.iter().any(Ty::contains_error)
            || expected_return.contains_error()
            || impl_sig.params.iter().any(Ty::contains_error)
            || impl_sig.return_type.contains_error();
        if any_error {
            return;
        }

        let report_span = if method.decl_span.start != method.decl_span.end {
            method.decl_span.clone()
        } else if method.fn_span.start != method.fn_span.end {
            method.fn_span.clone()
        } else {
            // Defensive: if the parser left both blank, fall back to the trait
            // method span so the message still anchors to a real source range.
            trait_method.span.clone()
        };

        if trait_sig.consumes_receiver != impl_sig.consumes_receiver {
            self.report_error_with_note(
                TypeErrorKind::TraitImplSignatureMismatch {
                    trait_name: trait_name.clone(),
                    method_name: method.name.clone(),
                    detail: "receiver ownership",
                },
                &report_span,
                format!(
                    "impl method `{type_name}.{}` has a different receiver ownership \
                     contract than trait `{trait_name}`; `consume self` must match exactly",
                    method.name
                ),
                &trait_method.span,
                format!("trait method `{trait_name}.{}` declared here", method.name),
            );
            return;
        }

        if trait_sig.returns_receiver_identity != impl_sig.returns_receiver_identity {
            self.report_error_with_note(
                TypeErrorKind::TraitImplSignatureMismatch {
                    trait_name: trait_name.clone(),
                    method_name: method.name.clone(),
                    detail: "receiver identity",
                },
                &report_span,
                format!(
                    "impl method `{type_name}.{}` has a different `#[returns_receiver]` \
                     contract than trait `{trait_name}`; exact receiver/result ownership \
                     identity must match",
                    method.name
                ),
                &trait_method.span,
                format!("trait method `{trait_name}.{}` declared here", method.name),
            );
            return;
        }

        if expected_params.len() != impl_sig.params.len() {
            // Arity mismatch — also fires when the impl wrote a different
            // receiver shape (e.g. `(it: X)` vs `(self)`), because the impl's
            // non-Self first param is not detected as a receiver and so is
            // *not* skipped, producing a different post-skip arity.
            self.report_error_with_note(
                TypeErrorKind::TraitImplSignatureMismatch {
                    trait_name: trait_name.clone(),
                    method_name: method.name.clone(),
                    detail: "arity",
                },
                &report_span,
                format!(
                    "impl method `{type_name}.{}` has {} parameter(s) but trait `{trait_name}` declares {} \
                     (after substituting `Self` and projecting associated types)",
                    method.name,
                    impl_sig.params.len(),
                    expected_params.len(),
                ),
                &trait_method.span,
                format!(
                    "trait method `{trait_name}.{}` declared here",
                    method.name
                ),
            );
            return;
        }

        // A parameter name is part of the method's API: a named call through
        // the trait and one on the concrete type must bind alike. An impl may
        // mark a parameter unused as `_name`; callers still label it `name`.
        if let Some((trait_param, impl_param)) = trait_sig
            .param_names
            .iter()
            .zip(&impl_sig.param_names)
            .find(|(trait_param, impl_param)| {
                trait_param != impl_param
                    && impl_param.strip_prefix('_') != Some(trait_param.as_str())
            })
        {
            self.report_error_with_note(
                TypeErrorKind::ImplParamNameMismatch,
                &report_span,
                format!(
                    "impl method `{type_name}.{}` names parameter `{impl_param}` where trait `{trait_name}` names it `{trait_param}`",
                    method.name,
                ),
                &trait_method.span,
                format!("trait method `{trait_name}.{}` declared here", method.name),
            );
            return;
        }

        // Receiver-mutability axis (Q297 Stage 1): when both sides declare a
        // receiver, the `is_mutable` flag must match. A trait declaring
        // `fn next(var self)` and an impl declaring `fn next(self)` (or vice
        // versa) is a hard reject — the receiver-mutability axis is part of
        // the contract, not a free parameter the impl may choose.
        //
        // Determine each side's receiver-mutability flag by checking the
        // first parameter for receiver-shape. This mirrors how
        // `register_impl_method` and `register_fn_sig_with_name` already
        // detect-and-skip receivers when building the signature's params
        // list; the receiver's `is_mutable` flag is otherwise dropped on
        // the floor, which is precisely the contract gap this check closes.
        let trait_receiver_mut = trait_method
            .params
            .first()
            .is_some_and(|p| self.is_receiver_param(p) && p.is_mutable);
        let impl_receiver_mut = method
            .params
            .first()
            .is_some_and(|p| self.is_receiver_param(p) && p.is_mutable);
        if trait_receiver_mut != impl_receiver_mut {
            let (trait_shape, impl_shape) = if trait_receiver_mut {
                (
                    "`var self` (mutable receiver)",
                    "`self` (by-value receiver)",
                )
            } else {
                (
                    "`self` (by-value receiver)",
                    "`var self` (mutable receiver)",
                )
            };
            self.report_error_with_note(
                TypeErrorKind::TraitImplSignatureMismatch {
                    trait_name: trait_name.clone(),
                    method_name: method.name.clone(),
                    detail: "receiver mutability",
                },
                &report_span,
                format!(
                    "impl method `{type_name}.{}` declares {impl_shape} but trait `{trait_name}` requires {trait_shape}",
                    method.name,
                ),
                &trait_method.span,
                format!(
                    "trait method `{trait_name}.{}` declared here",
                    method.name
                ),
            );
            return;
        }

        // Canonicalize every comparison type to a defining-module-qualified
        // identity up front, holding the read-only `self` borrow only for this
        // block so the later mutable error reporting is unencumbered. The owned
        // canonical `Ty` values then drive the comparisons; the diagnostics
        // still render the original, un-canonicalized spellings.
        let (
            canon_expected_params,
            canon_actual_params,
            canon_expected_return,
            canon_actual_return,
        ) = {
            let ctx = TraitSigCanonCtx {
                modules: &self.modules,
                trait_owner: trait_owner_module.as_deref(),
                defines_qualified: &|qualified: &str| self.type_defs.contains_key(qualified),
                is_local: &|name: &str| {
                    self.local_type_defs.contains(name) || self.source_type_defs.contains(name)
                },
            };
            // EXPECTED is the trait declaration's required signature: a bare
            // name there denotes the trait owner's sibling type, so it ALWAYS
            // qualifies to the owner (`preserve_local_shadow = false`). The
            // importer's local type names do not change what the trait requires.
            let canon_expected_params: Vec<Ty> = expected_params
                .iter()
                .map(|t| canonicalize_type_identity(&self.normalize_for_use(t), &ctx, false))
                .collect();
            // ACTUAL is the impl's written signature: a bare name that shadows a
            // local type keeps its local identity (`preserve_local_shadow =
            // true`), so a local `CloseError` correctly mismatches the trait's
            // `closableerr.CloseError` instead of being conflated with it.
            let canon_actual_params: Vec<Ty> = impl_sig
                .params
                .iter()
                .map(|t| canonicalize_type_identity(&self.normalize_for_use(t), &ctx, true))
                .collect();
            let canon_expected_return =
                canonicalize_type_identity(&self.normalize_for_use(&expected_return), &ctx, false);
            let canon_actual_return = canonicalize_type_identity(
                &self.normalize_for_use(&impl_sig.return_type),
                &ctx,
                true,
            );
            (
                canon_expected_params,
                canon_actual_params,
                canon_expected_return,
                canon_actual_return,
            )
        };

        for (i, (expected, actual)) in expected_params
            .iter()
            .zip(impl_sig.params.iter())
            .enumerate()
        {
            if canon_expected_params[i] != canon_actual_params[i] {
                let param_label = impl_sig.param_names.get(i).map_or_else(
                    || format!("parameter {}", i + 1),
                    |n| format!("parameter `{n}`"),
                );
                self.report_error_with_note(
                    TypeErrorKind::TraitImplSignatureMismatch {
                        trait_name: trait_name.clone(),
                        method_name: method.name.clone(),
                        detail: "parameter",
                    },
                    &report_span,
                    format!(
                        "impl method `{type_name}.{}` {param_label} has type `{}` but trait `{trait_name}` \
                         requires `{}`",
                        method.name,
                        actual.user_facing(),
                        expected.user_facing(),
                    ),
                    &trait_method.span,
                    format!(
                        "trait method `{trait_name}.{}` declared here",
                        method.name
                    ),
                );
                return;
            }
        }

        if canon_expected_return != canon_actual_return {
            self.report_error_with_note(
                TypeErrorKind::TraitImplSignatureMismatch {
                    trait_name: trait_name.clone(),
                    method_name: method.name.clone(),
                    detail: "return type",
                },
                &report_span,
                format!(
                    "impl method `{type_name}.{}` returns `{}` but trait `{trait_name}` requires `{}`",
                    method.name,
                    impl_sig.return_type.user_facing(),
                    expected_return.user_facing(),
                ),
                &trait_method.span,
                format!(
                    "trait method `{trait_name}.{}` declared here",
                    method.name
                ),
            );
        }
    }

    /// The `trait_impls_set` / `trait_impl_method_names` identity for an impl
    /// target named `type_name`.
    ///
    /// A generated monomorphic builtin enum (`SendError`, `TimeoutError`,
    /// `LinkError`, `Delivery`, …) is declared in a stdlib `.hew` source and
    /// carries exactly one identity: the catalog's `canonical_name`. The
    /// resolver already stamps that spelling onto every annotation, field and
    /// variant payload naming the declaration, so registering its impls under
    /// the bare leaf mints an entry lookup can never find — which is why
    /// `ActorError.Rejected(reason)` could not interpolate `reason` while a
    /// bare `SendError.Full` could. Both sides now select the catalog identity.
    ///
    /// Every other receiver kind (primitives, `Vec`/`HashMap`/generics, the
    /// synthetic cursors) has its own arm in
    /// `canonical_primitive_or_builtin_key_for_impl_name` and never reaches
    /// the module-qualifying fallback.
    /// The canonical identity of an `impl` target spelled through a module
    /// binding, or `None` when the spelling is already an identity (a bare
    /// local name, a builtin, an exact owner-qualified path).
    ///
    /// Only a qualified spelling is resolved: a bare name is the declaring
    /// scope's own lexical spelling and the surrounding registration already
    /// owns its qualification.
    pub(in crate::check) fn canonical_impl_target_identity(&self, name: &str) -> Option<String> {
        name.contains('.')
            .then(|| self.canonical_nominal_name(name))
            .flatten()
    }

    pub(in crate::check) fn trait_impl_type_identity(&self, type_name: &str) -> String {
        self.canonical_primitive_or_builtin_key_for_impl_name(type_name)
            .or_else(|| {
                crate::builtin_enums::canonical_monomorphic_builtin_enum_identity(type_name)
                    .map(ToString::to_string)
            })
            .unwrap_or_else(|| {
                if crate::lookup_builtin_type(type_name).is_some() {
                    return type_name.to_string();
                }
                self.current_module
                    .as_ref()
                    .filter(|_| !type_name.contains('.'))
                    .map_or_else(
                        || type_name.to_string(),
                        |module| format!("{module}.{type_name}"),
                    )
            })
    }

    pub(in crate::check) fn trait_impl_method_declaration(
        &self,
        ty: &Ty,
        trait_name: &str,
        method_name: &str,
    ) -> Option<(crate::DefId, String)> {
        let Ty::Named { name, args, .. } = ty else {
            return None;
        };
        let type_identity = self.trait_impl_type_identity(name);
        let trait_identity = self.trait_defs_key_for_bound(trait_name);
        let args = args
            .iter()
            .map(|ty| ResolvedTy::from_ty(&self.subst.resolve(ty)).ok())
            .collect::<Option<Vec<_>>>()
            .unwrap_or_default();
        crate::type_facts::selected_impl_method(
            &self.trait_impl_method_declaration_ids,
            &type_identity,
            &args,
            &trait_identity,
            method_name,
        )
        .map(|(declaration, owner)| {
            (
                declaration,
                Self::method_declaration_key(&owner, method_name),
            )
        })
    }

    pub(in crate::check) fn record_trait_impl(&mut self, type_name: &str, trait_name: &str) {
        let type_identity = self.trait_impl_type_identity(type_name);
        let trait_identity = self.trait_defs_key_for_bound(trait_name);
        self.trait_impls_set.insert((type_identity, trait_identity));
    }

    pub(in crate::check) fn record_trait_impl_methods(
        &mut self,
        type_name: &str,
        trait_name: &str,
        method_names: impl IntoIterator<Item = String>,
    ) {
        let type_identity = self.trait_impl_type_identity(type_name);
        let trait_identity = self.trait_defs_key_for_bound(trait_name);
        let entry = self
            .trait_impl_method_names
            .entry((type_identity, trait_identity))
            .or_default();
        entry.extend(method_names);
    }

    /// Canonical receiver key used by the primitive-and-builtin trait impl
    /// table.  Returns `Some(canonical)` for any receiver kind whose user
    /// trait impls cannot be hung off `type_defs`:
    ///
    /// * Primitives — keyed by `Ty::canonical_lowering_name()`.  This collapses
    ///   the user-facing alias set (`isize` → `i64`) so registration and
    ///   dispatch agree on a single key.  `int` and `Int` are no longer
    ///   accepted; the resolver hard-errors at the type-position lookup.
    /// * Compiler-builtin generics `Vec`, `HashMap`, `HashSet`, `Generator`,
    ///   and the synthetic iterator cursors — keyed by their
    ///   catalog name; these lack a user-selectable nominal declaration entry
    ///   that executable impl dispatch can safely attach methods to.
    ///
    /// Returns `None` for receivers that already flow through `type_defs`
    /// (user structs, actors, opaque handle types).
    #[must_use]
    pub(crate) fn canonical_primitive_or_builtin_key(ty: &Ty) -> Option<String> {
        if let Some(canonical) = ty.canonical_lowering_name() {
            return Some(canonical.to_string());
        }
        if let Ty::Named {
            builtin: Some(builtin),
            ..
        } = ty
        {
            if let Some(identity) = match builtin {
                BuiltinType::VecIter => Some("std.builtins.VecIter"),
                BuiltinType::HashMapIter => Some("std.builtins.HashMapIter"),
                _ => None,
            } {
                return Some(identity.to_string());
            }
            // An actor is the type of its handle, so an actor handle's
            // canonical key is the actor's own nominal, never a builtin
            // presentation name.
            if builtin.is_collection()
                || matches!(
                    builtin,
                    BuiltinType::Generator | BuiltinType::ChildRef | BuiltinType::RemotePid
                )
            {
                return Some(builtin.canonical_name().to_string());
            }
        }
        None
    }

    /// Canonical receiver key at an impl declaration. Context-free raw-name
    /// lookup intentionally excludes synthetic cursors because a user may
    /// declare the same leaf. The provenance-aware resolver admits them only
    /// while registering their shipped stdlib declarations.
    pub(super) fn canonical_primitive_or_builtin_key_for_impl_name(
        &self,
        name: &str,
    ) -> Option<String> {
        Self::canonical_primitive_or_builtin_key_from_name(name).or_else(|| {
            self.resolved_builtin_type(name)
                .filter(|builtin| {
                    matches!(
                        builtin,
                        BuiltinType::VecIter
                            | BuiltinType::HashMapIter
                            | BuiltinType::ChildRef
                            | BuiltinType::RemotePid
                    )
                })
                .map(|builtin| match builtin {
                    BuiltinType::VecIter => "std.builtins.VecIter".to_string(),
                    BuiltinType::HashMapIter => "std.builtins.HashMapIter".to_string(),
                    BuiltinType::ChildRef | BuiltinType::RemotePid => {
                        builtin.canonical_name().to_string()
                    }
                    _ => unreachable!("filter admits only compiler carrier builtins"),
                })
        })
    }

    /// Same as [`Self::canonical_primitive_or_builtin_key`] but accepts the
    /// raw type-name string seen at impl-block registration (e.g. `"int"`,
    /// `"string"`, `"Vec"`).  Returns `None` for names that aren't primitive
    /// aliases or compiler-builtin generics.
    #[must_use]
    pub(in crate::check) fn canonical_primitive_or_builtin_key_from_name(
        name: &str,
    ) -> Option<String> {
        if let Some(prim) = Ty::from_name(name) {
            return Self::canonical_primitive_or_builtin_key(&prim);
        }
        if let Some(builtin) = crate::lookup_builtin_type(name) {
            if builtin.is_collection() || matches!(builtin, BuiltinType::Generator) {
                return Some(builtin.canonical_name().to_string());
            }
        }
        None
    }

    /// Record an `impl <Trait> for <PrimitiveOrBuiltinGeneric>` method in the
    /// side table.  `canonical_key` must come from
    /// [`Self::canonical_primitive_or_builtin_key_from_name`] so registration
    /// and dispatch agree.
    pub(in crate::check) fn record_primitive_trait_impl_method(
        &mut self,
        canonical_key: String,
        trait_name: &str,
        method_name: String,
        sig: FnSig,
    ) {
        // First-wins, matching `record_primitive_trait_impl_self_args` and the
        // assoc-type binding table. Under Hew's coherence rule there is at most
        // one impl of a trait per constructor, so a second registration is
        // either the same impl reprocessed across phases or a rejected
        // conflicting impl (diagnosed at `record_trait_impl`); in neither case
        // may it overwrite the surviving impl's signature. Keeping every side
        // table first-wins guarantees the dispatched method signature and the
        // applicability proof (self-args) always come from the *same* impl, so
        // they cannot drift.
        let trait_identity = self.trait_defs_key_for_bound(trait_name);
        self.primitive_trait_impls
            .entry((canonical_key, trait_identity))
            .or_default()
            .entry(method_name)
            .or_insert(sig);
    }

    /// Record the impl's `Self` type arguments for an `impl <Trait> for
    /// <PrimitiveOrBuiltinGeneric>` so a later dispatch on a concrete receiver
    /// can bind the impl's type parameters (see
    /// [`Checker::primitive_trait_impl_self_args`]). Idempotent within one impl:
    /// every method of that impl records the SAME `Self` args, so the first
    /// recorded entry per `(canonical, trait)` wins.
    ///
    /// Coherence enforcement (single-impl-per-constructor): if a *different*
    /// `Self` shape is already recorded for this `(canonical, trait)`, two
    /// distinct impls target the same builtin constructor with the same trait —
    /// e.g. a blanket `impl<T> Acc for Vec<T>` (`self_args = [T]`) and a concrete
    /// `impl Acc for Vec<i64>` (`self_args = [i64]`). Hew has no specialization
    /// or overlapping impls (single-crate coherence, mission Q66.b), so the
    /// second impl is rejected at its declaration site with a clean diagnostic
    /// and does NOT overwrite the first (first-wins keeps the surviving impl's
    /// method signatures and this `Self`-arg applicability proof from the SAME
    /// impl, so they cannot drift).
    ///
    /// Comparing the `Self` shape (rather than a source span) makes this robust
    /// to the registration architecture re-processing the same impl across
    /// module/import phases: a reprocessed impl re-presents an *identical*
    /// `Self` shape and is correctly treated as the same impl, while a genuine
    /// overlap presents a *different* shape. It also naturally scopes the check
    /// to builtin/primitive receivers — user-record impls never reach this side
    /// table — which is exactly where the drift fail-open lived.
    ///
    /// KNOWN GAP (tracked): two *genuinely-distinct* impls that share an
    /// *identical* `Self` shape — e.g. two literal `impl<T> Acc for Vec<T>`
    /// blocks — are NOT rejected here (they compare shape-equal and are treated
    /// as a re-presentation). Closing this needs the impl's DEFINING identity,
    /// but the only readily-available per-impl span (`impl.target_type` span)
    /// cannot be used as the coherence key because the documented user-redeclare
    /// path lets a user `pub trait Display` shadow the prelude `Display`: the
    /// prelude `impl Display for i64` and the user `impl Display for i64` are
    /// distinct traits that collapse to the same `(canonical, "Display")` key,
    /// so a defining-span key would falsely reject that legal shadow. A correct
    /// fix must additionally key on the trait's DEFINING identity (not its
    /// name), which is a larger cross-cutting change tracked as a separate
    /// follow-up ("trait-impl coherence: reject duplicate same-(type,trait)
    /// impls via defining-identity"). The projection fix this method supports is
    /// unaffected: first-wins keeps the dispatched method signature and the
    /// applicability proof from the SAME (first) impl, so even an accepted
    /// duplicate cannot cause the mis-projection fail-open this fix closes.
    pub(in crate::check) fn record_primitive_trait_impl_self_args(
        &mut self,
        canonical_key: String,
        trait_name: &str,
        self_args: Vec<Ty>,
        impl_span: &Span,
    ) {
        let trait_identity = self.trait_defs_key_for_bound(trait_name);
        match self
            .primitive_trait_impl_self_args
            .get(&(canonical_key.clone(), trait_identity.clone()))
        {
            None => {
                self.primitive_trait_impl_self_args
                    .insert((canonical_key, trait_identity), self_args);
            }
            Some(existing) if existing == &self_args => {
                // Same impl (same Self shape), re-presented across a later
                // registration phase — not a conflict. (See KNOWN GAP above:
                // this also admits a genuine same-shape duplicate, tracked.)
            }
            Some(_) => {
                // A second, structurally-different impl of the same trait on the
                // same builtin constructor: an overlapping impl, which Hew does
                // not permit. Reject it; keep the first-registered impl.
                let dedup = (
                    canonical_key.clone(),
                    trait_name.to_string(),
                    impl_span.start,
                    impl_span.end,
                );
                if self.conflicting_trait_impl_reported.insert(dedup) {
                    self.report_error(
                        TypeErrorKind::ConflictingTraitImpl {
                            trait_name: trait_name.to_string(),
                            type_name: canonical_key.clone(),
                        },
                        impl_span,
                        format!(
                            "conflicting implementation of trait `{trait_name}` for `{canonical_key}`: \
                             a trait may be implemented at most once per type constructor \
                             (Hew has no specialization or overlapping impls)"
                        ),
                    );
                }
            }
        }
    }

    /// Look up a method on the primitive/builtin-generic impl table.
    ///
    /// Walks every trait registered for the receiver's canonical kind and
    /// returns the first method whose name matches.  Returns the resolved
    /// `FnSig` (receiver already filtered) plus the trait name that
    /// provided it, so callers can record dispatch metadata keyed on the
    /// resolved trait rather than re-deriving it from a name string.
    #[must_use]
    pub(in crate::check) fn lookup_primitive_trait_method(
        &self,
        receiver_ty: &Ty,
        method: &str,
    ) -> Option<(String, FnSig)> {
        let canonical = Self::canonical_primitive_or_builtin_key(receiver_ty)?;
        let mut candidates: Vec<(&String, &FnSig)> = self
            .primitive_trait_impls
            .iter()
            .filter_map(|((rx_key, trait_name), methods)| {
                if rx_key != &canonical {
                    return None;
                }

                // A local trait declaration shadows an imported/prelude trait
                // with the same source leaf. Canonical keys make those impls
                // coexist in this global side table, so hide the shadowed owner
                // instead of letting HashMap iteration choose nondeterministically.
                let source_name = crate::short_name(trait_name);
                if self.local_trait_defs.contains(source_name) {
                    let local_key = self.current_module.as_ref().map_or_else(
                        || source_name.to_string(),
                        |module| format!("{module}.{source_name}"),
                    );
                    if trait_name != &local_key {
                        return None;
                    }
                }
                methods.get(method).map(|sig| (trait_name, sig))
            })
            .collect();
        // Multiple genuinely distinct visible traits can still declare the
        // same receiver method. Keep selection stable until the language grows
        // an explicit ambiguity diagnostic for this receiver-form surface.
        candidates.sort_unstable_by_key(|(trait_name, _)| trait_name.as_str());
        candidates
            .into_iter()
            .next()
            .map(|(trait_name, sig)| (trait_name.clone(), sig.clone()))
    }

    pub(in crate::check) fn register_receive_fn(&mut self, actor_name: &str, rf: &ReceiveFnDecl) {
        let mut generic_bindings = std::collections::HashMap::new();
        if let Some(type_params) = &rf.type_params {
            for tp in type_params {
                generic_bindings.insert(
                    tp.name.clone(),
                    Ty::Named {
                        builtin: None,
                        name: tp.name.clone(),
                        args: vec![],
                    },
                );
            }
        }
        if !generic_bindings.is_empty() {
            self.generic_ctx.push(generic_bindings);
        }

        let mut hole_vars = Vec::new();
        let rf_scope = self.collect_type_param_scope_with_assoc_bindings(
            rf.type_params.as_ref(),
            rf.where_clause.as_ref(),
            &mut hole_vars,
        );
        let pushed_rf_bounds = !rf_scope.bounds.is_empty();
        if pushed_rf_bounds {
            self.current_type_param_bounds.push(rf_scope.clone());
        }

        let param_names = rf.params.iter().map(|p| p.name.clone()).collect();
        let params = rf
            .params
            .iter()
            .map(|p| self.resolve_registered_annotation_ty(&p.ty, &mut hole_vars))
            .collect();
        let declared_return_type = rf.return_type.as_ref().map_or(Ty::Unit, |ret| {
            self.resolve_registered_annotation_ty(ret, &mut hole_vars)
        });
        let return_type = if rf.is_generator {
            Ty::stream(declared_return_type)
        } else {
            declared_return_type
        };

        if pushed_rf_bounds {
            self.current_type_param_bounds.pop();
        }
        if rf.type_params.as_ref().is_some_and(|tp| !tp.is_empty()) {
            self.generic_ctx.pop();
        }

        let type_param_bounds =
            self.collect_type_param_bounds(rf.type_params.as_ref(), rf.where_clause.as_ref());
        let sig = FnSig {
            type_params: rf.type_params.as_ref().map_or(vec![], |params| {
                params.iter().map(|p| p.name.clone()).collect()
            }),
            type_param_bounds,
            param_names,
            params,
            return_type,
            ..FnSig::default()
        };

        let method_name = format!("{}::{}", actor_name, rf.name);
        if rf.is_generator {
            self.receive_generator_methods.insert(method_name.clone());
        }
        if matches!(
            rf.return_type.as_ref().map(|ty| &ty.0),
            Some(hew_parser::ast::TypeExpr::Fallible { .. })
        ) {
            self.receive_fails_methods.insert(method_name.clone());
        }
        self.actor_receive_methods.insert(method_name.clone());
        self.record_fn_sig_inference_holes(&method_name, hole_vars);
        self.fn_type_param_assoc_bindings
            .insert(method_name.clone(), rf_scope.assoc_bindings);
        self.fn_sigs.insert(method_name, sig);
    }

    /// IMPORT-lexical nominal authority (rc1-F1 stage C): a bare name the
    /// declaring file does not itself declare resolves through the declaring
    /// module's DIRECT imports — through the names each import actually
    /// BINDS. A named item import binds exactly its
    /// bound name, so `import sm::{ Tok as ForeignTok }` binds `ForeignTok`
    /// and leaves bare `Tok` meaning NOTHING here. Exactly one bound source
    /// declaration across the import set mints its declaring file's
    /// identity; zero or several → `None`: the spelling stays as written and
    /// the contract compare fails closed — ambiguity never picks a winner on
    /// the C-ABI axis.
    pub(in crate::check) fn extern_nominal_imported_owner(&self, name: &str) -> Option<String> {
        // (declaring file, source-declared name) pairs the bound spelling
        // denotes. Deduped: two import edges to one declaration are one
        // meaning, not an ambiguity.
        let mut declarations = self
            .current_module_direct_import_bindings
            .iter()
            .filter_map(|(module, spec)| {
                // Which SOURCE name does the bound spelling `name` denote
                // under this import? None = this import does not bind it.
                let source_name = match spec {
                    None => return None,
                    Some(ImportSpec::Names(names)) => names
                        .iter()
                        .find(|n| n.alias.as_deref().unwrap_or(&n.name) == name)?
                        .name
                        .clone(),
                };
                Some((module, source_name))
            })
            .flat_map(|(module, source_name)| {
                self.module_source_paths
                    .get(module)
                    .into_iter()
                    .flatten()
                    .filter(|source| {
                        self.file_type_decls
                            .get(*source)
                            .is_some_and(|declared| declared.contains(&source_name))
                    })
                    .map(|source| (source, source_name.clone()))
                    .collect::<Vec<_>>()
            })
            .collect::<Vec<_>>();
        declarations.sort_unstable();
        declarations.dedup();
        match declarations.as_slice() {
            [(single, source_name)] => Some(format!(
                "{}.{source_name}",
                self.identity.module_path_for_source(single)?
            )),
            _ => None,
        }
    }

    /// Resolve an extern callable's nominal types to the registered source
    /// declaration used by field annotations and ordinary callable signatures.
    pub(super) fn resolve_extern_signature_nominals(&self, ty: &Ty) -> Ty {
        match ty {
            Ty::Named {
                name,
                args,
                builtin,
            } => {
                // The callable consumes source values, including fields from
                // peer files assembled into this module. Resolve their registered
                // declaration rather than substituting the ABI contract's file
                // provenance. Already-qualified source identities stay intact.
                let resolved = self.canonical_nominal_name(name).or_else(|| {
                    (!name.contains('.') && self.extern_nominal_file_owner(name).is_none())
                        .then(|| self.extern_nominal_imported_owner(name))
                        .flatten()
                });
                Ty::Named {
                    name: resolved.unwrap_or_else(|| name.clone()),
                    args: args
                        .iter()
                        .map(|arg| self.resolve_extern_signature_nominals(arg))
                        .collect(),
                    builtin: *builtin,
                }
            }
            _ => ty.map_children_pub(&|child| self.resolve_extern_signature_nominals(child)),
        }
    }

    /// FILE-lexical nominal authority: the item's own declaring file first,
    /// then exactly one sibling file of the declaring module.
    ///
    /// The item's own file is known only inside the per-item registration
    /// frame; outside it the sibling rule alone still decides, which is what
    /// lets source type expressions ask the same question extern signatures do.
    pub(in crate::check) fn extern_nominal_file_owner(&self, name: &str) -> Option<String> {
        let declares = |source: &std::path::PathBuf| {
            self.file_type_decls
                .get(source)
                .is_some_and(|declared| declared.contains(name))
        };
        if let Some(file) = self.current_item_source.as_ref() {
            if declares(file) {
                return Some(format!(
                    "{}.{name}",
                    self.identity.module_path_for_source(file)?
                ));
            }
        }
        let sources = self
            .module_source_paths
            .get(self.current_module.as_deref()?)?;
        let mut declaring = sources.iter().filter(|source| declares(source));
        let single = declaring.next()?;
        if declaring.next().is_some() {
            return None;
        }
        Some(format!(
            "{}.{name}",
            self.identity.module_path_for_source(single)?
        ))
    }

    /// Diagnostic routing token for an item declared in `file` under
    /// `module`: the file's own path when it is NOT the module's primary
    /// source (a peer file of a directory module), else `None` (the module
    /// identity routes as before). The CLI/LSP source maps carry an entry per
    /// source file keyed by this exact rendering.
    pub(in crate::check) fn item_file_routing_token(
        &self,
        module: Option<&str>,
        file: Option<&std::path::PathBuf>,
    ) -> Option<String> {
        let file = file?;
        let primary = self.module_source_paths.get(module?)?.first()?;
        (file != primary).then(|| file.display().to_string())
    }

    /// Capture the source identity that diagnostic emission must use after the
    /// current item frame has been cleared. Peer files need their path rather
    /// than the assembled directory module's identity.
    pub(in crate::check) fn current_diagnostic_source_module(&self) -> Option<String> {
        self.item_file_routing_token(
            self.current_module.as_deref(),
            self.current_item_source.as_ref(),
        )
        .or_else(|| self.current_module.clone())
    }

    /// Resolve one extern declaration against the single-owner table
    /// (rc1-F1 stage B): mint the symbol's contract, or run the canonicalized
    /// structural compare against the established one — adopt on agreement,
    /// register-then-report on conflict. The compare ALWAYS runs for a
    /// further declaration of an established symbol (no span-based
    /// same-site shortcut: a byte-offset span carries no file identity, and
    /// peer files of one directory module can align spans exactly).
    pub(super) fn resolve_extern_contract(
        &mut self,
        block_span: &Span,
        declaration_ordinal: usize,
        source_symbol: &str,
        sig: &FnSig,
        consuming_params: &[bool],
        f: &hew_parser::ast::ExternFnDecl,
    ) -> Option<crate::DefId> {
        let declaration = self.require_declaration_occurrence(
            block_span,
            crate::DeclarationKind::ExternFunction,
            declaration_ordinal,
        )?;
        if source_symbol.is_empty() {
            // Template declarations (`#[extern_symbol("…{T}…")]`) have no
            // call-independent symbol and therefore no symbol-keyed contract
            // slot; they still register as extern declarations (call-target
            // resolution, `unsafe` gating).
            self.extern_table.register_detached_declaration(
                declaration.clone(),
                String::new(),
                self.current_module.clone(),
            );
            return Some(declaration);
        }
        // Resolve the candidate signature's nominal identities NOW, in the
        // declaring item's own lexical context (rc1-F1 stage C). The
        // contract stores resolved signatures, so every later comparison is
        // identity equality — no compare-time spelling repair, whose context
        // would be the SECOND declaration's, not the declarer's.
        let resolved_params = sig
            .params
            .iter()
            .map(|ty| extern_contract_nominal_identity(self, ty))
            .collect::<Vec<_>>();
        let resolved_return = extern_contract_nominal_identity(self, &sig.return_type);
        let Some((established_id, established)) = self
            .extern_table
            .established(source_symbol)
            .map(|(id, contract)| (id, contract.clone()))
        else {
            self.extern_table.mint(crate::extern_table::ExternContract {
                owner: declaration.clone(),
                symbol: source_symbol.to_string(),
                params: resolved_params,
                return_type: resolved_return,
                consuming_params: consuming_params.to_vec(),
                is_variadic: f.is_variadic,
                span: f.span.clone(),
                declaring_module: self.current_module.clone(),
                declaring_source: self.current_item_source.clone(),
            });
            return Some(declaration);
        };
        let agrees = established.params == resolved_params
            && established.return_type == resolved_return
            && established.consuming_params == consuming_params
            && established.is_variadic == f.is_variadic;
        if agrees {
            // Single-owner property: a further agreeing declaration becomes
            // another name of the ONE established ABI contract, keeping its
            // OWN provenance (declaring module, endpoint).
            self.extern_table.adopt_declaration(
                declaration.clone(),
                source_symbol.to_string(),
                self.current_module.clone(),
                established_id,
            );
        } else {
            // Register FIRST, report second: the conflicting declaration
            // must keep its `unsafe` gate and call-target endpoint while the
            // hard error propagates (the unsafe registry is not conditional
            // on ABI agreement).
            self.extern_table.register_detached_declaration(
                declaration.clone(),
                source_symbol.to_string(),
                self.current_module.clone(),
            );
            self.report_extern_contract_conflict(
                source_symbol,
                &established,
                &resolved_params,
                &resolved_return,
                consuming_params,
                f,
            );
        }
        Some(declaration)
    }

    /// Report a declaration whose signature disagrees with the symbol's
    /// established contract, attributing both sides to their declaring FILES.
    pub(super) fn report_extern_contract_conflict(
        &mut self,
        source_symbol: &str,
        established: &crate::extern_table::ExternContract,
        resolved_params: &[Ty],
        resolved_return: &Ty,
        consuming_params: &[bool],
        f: &hew_parser::ast::ExternFnDecl,
    ) {
        let established_description = extern_signature_description(
            &established.params,
            &established.return_type,
            &established.consuming_params,
            established.is_variadic,
        );
        // Describe the RESOLVED identities — the things actually compared —
        // so two same-leaf nominals from different declaring files render
        // distinguishably (`pkg.Tok` vs `pkg.aaa.Tok`), never as one
        // spelling conflicting with itself.
        let conflicting = extern_signature_description(
            resolved_params,
            resolved_return,
            consuming_params,
            f.is_variadic,
        );
        // File-accurate attribution (rc1-F1 stage C): item spans are
        // file-relative byte offsets, so a declaration living in a peer
        // file of a directory module must route to THAT file, never the
        // module's primary source. The routing token is the declaring
        // file itself when it differs from the module's primary file;
        // otherwise the module identity routes as before.
        let error_token = self
            .item_file_routing_token(
                self.current_module.as_deref(),
                self.current_item_source.as_ref(),
            )
            .or_else(|| self.current_module.clone());
        let note_token = self
            .item_file_routing_token(
                established.declaring_module.as_deref(),
                established.declaring_source.as_ref(),
            )
            .or_else(|| established.declaring_module.clone());
        self.errors.push(TypeError {
            severity: crate::error::Severity::Error,
            kind: TypeErrorKind::ConflictingExternDeclaration {
                symbol_name: source_symbol.to_string(),
            },
            span: f.span.clone(),
            message: format!(
                "extern symbol `{source_symbol}` has conflicting declarations: \
                 `{conflicting}` does not match the established `{established_description}`"
            ),
            notes: vec![
                (
                    established.span.clone(),
                    format!(
                        "the first declaration of `{source_symbol}` established `{established_description}`"
                    ),
                    note_token,
                ),
                (
                    f.span.clone(),
                    "extern \"C\" symbol declarations are program-wide unique: the linker \
                     binds every call site to one implementation, so a redeclaration must \
                     match the established contract exactly, even if this declaration is \
                     never called"
                        .to_string(),
                    error_token.clone(),
                ),
            ],
            suggestions: vec![format!(
                "make every declaration of `{source_symbol}` use exactly `{established_description}`"
            )],
            source_module: error_token,
        });
    }

    /// Establish the extern declaration identity of a registry-loaded C
    /// function and register its `unsafe` gate.
    ///
    /// Registry metadata and codegen-intercepted witnesses publish these
    /// functions under `key` in `fn_sigs` and the `unsafe` gate resolves a
    /// call by that same key, so the declaration path is `key`. Shipped
    /// modules routinely re-export one runtime symbol, so a bare path that is
    /// already established is the same declaration reached through a second
    /// mirror, not a conflict; a source declaration already gating `key`
    /// keeps its own record.
    pub(super) fn declare_contractless_extern(
        &mut self,
        module: crate::ModuleId,
        module_path: &str,
        key: &str,
    ) {
        let declaration = if let Some(existing) = self.lookup_declaration(key) {
            existing.clone()
        } else {
            // One occurrence per source-less extern declaration in this
            // module. The registry mirror and the layout witnesses are two
            // inventories that both declare here; a per-inventory ordinal
            // made them collide, and `declare` resolves a collision to the
            // ESTABLISHED declaration, so the second inventory's rows
            // silently adopted the first's identities and endpoints.
            let ordinal = self
                .contractless_extern_occurrences
                .entry(module)
                .or_insert(0);
            let occurrence = crate::DeclarationOccurrence::new_with_synthetic_ordinal(
                Some(module),
                &(0..0),
                *ordinal,
                crate::DeclarationKind::ExternFunction,
                0,
            );
            *ordinal += 1;
            match self.identity.declare(occurrence, key) {
                Ok(declaration) => declaration,
                Err(error) => {
                    self.errors.push(TypeError::new(
                        TypeErrorKind::InvalidOperation,
                        0..0,
                        format!("extern declaration `{key}` in `{module_path}`: {error}"),
                    ));
                    return;
                }
            }
        };
        if !self.extern_table.requires_unsafe(key) {
            self.extern_table.register_contractless_declaration(
                declaration,
                key.to_string(),
                Some(module_path.to_string()),
            );
        }
    }

    #[expect(
        clippy::too_many_lines,
        reason = "extern registration validates ABI authority and records lifecycle provenance"
    )]
    pub(in crate::check) fn register_extern_block(&mut self, eb: &ExternBlock, block_span: &Span) {
        // `extern "rt"` is the Hew-side declaration surface for runtime
        // functions. Validate each declared symbol against the `stable`
        // section of scripts/runtime-export-classification.toml. Fail-closed: an
        // unclassified symbol is a hard error so the failure surfaces at check
        // time rather than at link time or (worse) silently routing to a wrong
        // runtime entry.
        //
        // `extern "C"` remains the raw user FFI surface (unsafe, no
        // validation). Other ABI strings are not yet defined by the language
        // and fall through to fn_sigs registration unchanged.
        if eb.abi == "rt" {
            let stable = crate::jit_symbols::stable_symbols();
            for f in &eb.functions {
                if !stable.contains(f.name.as_str()) {
                    self.errors.push(TypeError {
                        severity: crate::error::Severity::Error,
                        kind: TypeErrorKind::ExternRtSymbolUnclassified {
                            symbol_name: f.name.clone(),
                            hint: format!(
                                "add `\"{}\"` to the `stable` list in \
                                 scripts/runtime-export-classification.toml, \
                                 or use `extern \"C\"` for raw FFI symbols \
                                 that are not part of the Hew runtime export ABI",
                                f.name
                            ),
                        },
                        span: f.span.clone(),
                        message: format!(
                            "`extern \"rt\" fn {}` names a symbol outside the stable \
                             runtime export ABI — only symbols classified as `stable` \
                             in scripts/runtime-export-classification.toml may appear in \
                             `extern \"rt\"` blocks",
                            f.name
                        ),
                        notes: vec![(
                            f.span.clone(),
                            "The `non-declarable` classification covers compiler-emitted, \
                             lifecycle, and shutdown symbols. None of these may be named \
                             by user code in `extern \"rt\"` blocks."
                                .to_string(),
                            self.current_module.clone(),
                        )],
                        suggestions: vec![format!(
                            "add `\"{}\"` to the `stable` list in \
                             scripts/runtime-export-classification.toml",
                            f.name
                        )],
                        source_module: self.current_module.clone(),
                    });
                }
            }
        }

        for (declaration_ordinal, f) in eb.functions.iter().enumerate() {
            let mut hole_vars = Vec::new();
            let param_names = f.params.iter().map(|p| p.name.clone()).collect();
            let params: Vec<Ty> = f
                .params
                .iter()
                .map(|p| {
                    self.resolve_registered_annotation_ty_with_context(
                        &p.ty,
                        &mut hole_vars,
                        TypeResolutionContext::ExternSignature,
                    )
                })
                .collect();
            let return_type = f.return_type.as_ref().map_or(Ty::Unit, |ret| {
                self.resolve_registered_annotation_ty_with_context(
                    ret,
                    &mut hole_vars,
                    TypeResolutionContext::ExternSignature,
                )
            });
            for (index, ty) in params.iter().chain([&return_type]).enumerate() {
                let Some(reason) = unmarshallable_extern_ty(ty) else {
                    continue;
                };
                let span = f
                    .params
                    .get(index)
                    .map_or_else(|| f.span.clone(), |param| param.ty.1.clone());
                let position = if index < f.params.len() {
                    format!("parameter {index}")
                } else {
                    "return type".to_string()
                };
                self.errors.push(TypeError::new(
                    TypeErrorKind::InvalidOperation,
                    span,
                    format!(
                        "`extern fn {}` {position} is `{}`, which {reason} and has no C-ABI representation",
                        f.name,
                        ty.user_facing()
                    ),
                ));
            }
            let mut sig = FnSig {
                param_ownership: f
                    .params
                    .iter()
                    .map(|param| crate::env::ParameterOwnership::from_consume(param.is_consume))
                    .collect(),
                param_names,
                params,
                return_type,
                extern_symbol: self.ingest_extern_symbol_attrs(&f.attributes),
                ..FnSig::default()
            };
            let source_symbol = sig.extern_symbol.as_ref().map_or_else(
                || f.name.clone(),
                |spec| {
                    if spec.template.is_monomorphic() {
                        spec.template.raw.clone()
                    } else {
                        String::new()
                    }
                },
            );
            let source_symbol_template = sig
                .extern_symbol
                .as_ref()
                .filter(|spec| !spec.template.is_monomorphic())
                .map(|spec| spec.template.clone());
            // Call sites type against the registered source declaration, just
            // as field annotations do. The ABI contract separately checks the
            // declaring file's provenance when comparing repeated symbols.
            // Template declarations carry signature type holes, not concrete
            // nominals — leave them untouched.
            if !source_symbol.is_empty() {
                sig.params = sig
                    .params
                    .iter()
                    .map(|ty| self.resolve_extern_signature_nominals(ty))
                    .collect();
                sig.return_type = self.resolve_extern_signature_nominals(&sig.return_type);
            }
            // Extern declarations use the same canonical owner spelling as
            // ordinary free functions. Their exact DefId comes from the
            // enclosing source occurrence inventoried before registration.
            let key = scoped_module_item_name(self.canonical_fn_owner(), &f.name)
                .unwrap_or_else(|| f.name.clone());
            let consuming_params = f
                .params
                .iter()
                .map(|param| param.is_consume)
                .collect::<Vec<_>>();
            let Some(declaration) = self.resolve_extern_contract(
                block_span,
                declaration_ordinal,
                &source_symbol,
                &sig,
                &consuming_params,
                f,
            ) else {
                continue;
            };
            self.record_fn_sig_inference_holes(&key, hole_vars);
            self.fn_sigs.insert(key.clone(), sig);
            if !self
                .source_extern_declarations
                .iter()
                .any(|existing| existing.declaration == declaration)
            {
                self.source_extern_declarations
                    .push(SourceExternDeclaration {
                        declaration,
                        symbol: source_symbol,
                        symbol_template: source_symbol_template,
                        signature_key: key.clone(),
                        declaring_module: self.current_module.clone(),
                        declaring_file: self.current_module_idx,
                        direct_import_modules: self.current_module_direct_imports.clone(),
                        consuming_params,
                    });
            }

            self.record_root_value_binding(&f.name);
        }
    }

    /// Join generated owned-result contracts to exact source extern
    /// declarations and their consuming release declaration.
    ///
    /// The generated table is the only ownership authority. Source names
    /// participate only after the contract's qualified nominal proves the
    /// declaring module, so a root or foreign-module symbol collision cannot
    /// inherit a lifecycle.
    pub(in crate::check) fn derive_opaque_resource_candidate_graph(
        &self,
        fn_sigs: &HashMap<String, FnSig>,
    ) -> OpaqueResourceCandidateGraph {
        derive_opaque_resource_candidate_graph(
            &self.source_extern_declarations,
            fn_sigs,
            &self.module_import_bindings,
            &self.import_type_name_aliases,
            &self.impl_method_declaration_ids,
            crate::ffi_contracts::FFI_OWNERSHIP_CONTRACTS,
            &self.identity,
        )
    }

    #[cfg(test)]
    pub(in crate::check) fn derive_opaque_resource_candidate_graph_for_contracts(
        &self,
        fn_sigs: &HashMap<String, FnSig>,
        contracts: &[(&str, crate::ffi_contracts::ExternOwnershipContract)],
    ) -> OpaqueResourceCandidateGraph {
        derive_opaque_resource_candidate_graph(
            &self.source_extern_declarations,
            fn_sigs,
            &self.module_import_bindings,
            &self.import_type_name_aliases,
            &self.impl_method_declaration_ids,
            contracts,
            &self.identity,
        )
    }

    /// Snapshot the compiler-assumed part of the implicit prelude before source
    /// registration begins.
    ///
    /// The prelude manifest is deliberately broader than this protected set:
    /// ordinary builtins remain normal lexical bindings and may be shadowed by
    /// user declarations. Only declaration-level lang items plus the core
    /// enum/desugaring heads below are names the compiler cannot let source
    /// replace without changing language semantics.
    pub(in crate::check) fn capture_protected_prelude_bindings(&mut self) {
        self.protected_prelude_bindings.clear();
        self.protected_prelude_declaration_collisions.clear();

        let authority = crate::stdlib_authority::authority();
        let prelude_exports = authority.prelude_exports();
        let mut protected_names: HashSet<String> = authority
            .lang_items()
            .values()
            .filter(|binding| {
                matches!(
                    binding.kind,
                    crate::stdlib_authority::AuthorityDeclarationKind::Type
                        | crate::stdlib_authority::AuthorityDeclarationKind::Trait
                )
            })
            .map(|binding| binding.declaration.clone())
            .collect();
        // Option/Result construction and propagation, plus for-loop
        // conversion, are compiler desugarings whose declarations predate the
        // corresponding lang-item annotations.
        protected_names.extend(
            ["Option", "Result", "IntoIterator"]
                .into_iter()
                .map(str::to_string),
        );

        for export in prelude_exports {
            let source_name = &export.name;
            let binding = export.alias.as_ref().unwrap_or(source_name);
            if protected_names.contains(source_name) {
                self.protected_prelude_bindings
                    .insert(binding.clone(), export.module.clone());
            }
        }
    }

    pub(in crate::check) fn reject_protected_prelude_declaration(
        &mut self,
        name: &str,
        span: &Span,
    ) -> bool {
        let owner = self.current_module.clone();
        self.reject_protected_prelude_declaration_for_owner(owner.as_deref(), name, span)
    }

    /// Reject a user declaration that would replace an always-in-scope prelude
    /// binding. Authority follows the declaration's owner, not whichever
    /// importer happens to be active while that declaration is published.
    pub(super) fn reject_protected_prelude_declaration_for_owner(
        &mut self,
        declaration_owner: Option<&str>,
        name: &str,
        span: &Span,
    ) -> bool {
        let compiling_canonical_stdlib = self.checking_embedded_builtins
            || self.in_stdlib_registration
            || self
                .canonical_std_module_sources
                .contains(declaration_owner.unwrap_or_default())
            || (declaration_owner.is_none() && !self.canonical_std_root_sources.is_empty());
        if compiling_canonical_stdlib || !self.protected_prelude_bindings.contains_key(name) {
            return false;
        }
        let declaration_key = (declaration_owner.map(str::to_string), name.to_string());
        if self
            .protected_prelude_declaration_collisions
            .insert(declaration_key)
        {
            let mut error = TypeError::new(
                TypeErrorKind::PreludeDeclCollision,
                span.clone(),
                format!(
                    "declaration `{name}` collides with the protected prelude binding `{name}`"
                ),
            );
            error.source_module = declaration_owner.map(str::to_string);
            self.errors.push(error);
        }
        true
    }
}
