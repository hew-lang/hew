//! Checker methods grouped by responsibility: module resolution.
//! Split from `methods.rs`: checker methods, part 1 of 5.
#![allow(
    unused_imports,
    redundant_imports,
    clippy::wildcard_imports,
    reason = "chunk files share the parent module's import header"
)]
#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
use super::super::*;
use super::*;
use crate::builtin_names::BuiltinNamedType;
use crate::check::calls::SignatureArgApplication;
use crate::check::dispatch::resolve_method_call;
use crate::check::types::GenericCallee;
use crate::check::types::{BareActorResolution, DeferredBuiltinCloneAdmission, DeferredWireCodec};
use crate::method_resolution::{
    collect_method_sigs_for_receiver, instantiate_stdlib_method_sig, lookup_builtin_method_sig,
    lookup_named_method_sig as shared_lookup_named_method_sig,
};
use crate::runtime_call::{FloatMethodOp, IntArithKind, IntBitOp, IntMethodWidth};
use crate::stdlib::{STD_NET_CONNECTION, STD_NET_LISTENER};
use crate::BuiltinType;

impl Checker {
    /// Resolve a module spelling in the current source file to the exact
    /// imported module path.  The spelling is an input-namespace key only;
    /// declaration IDs must use this source owner, never an alias or final
    /// path segment.
    pub(in crate::check) fn canonical_module_import_owner(&self, module_name: &str) -> String {
        self.module_import_bindings
            .get(&(
                self.current_module.clone(),
                self.current_module_idx,
                module_name.to_string(),
            ))
            .cloned()
            .unwrap_or_else(|| module_name.to_string())
    }

    /// Whether `module_name` is a lexical module binding in the current file.
    /// The process-wide module registry is deliberately not consulted.
    pub(in crate::check) fn module_binding_in_current_file(&self, module_name: &str) -> bool {
        self.module_import_bindings.contains_key(&(
            self.current_module.clone(),
            self.current_module_idx,
            module_name.to_string(),
        ))
    }

    /// Whether this module spelling resolves to a user-source declaration.
    /// `user_modules` is intentionally not consulted: it is a legacy lexical
    /// spelling set and therefore cannot distinguish two paths with the same
    /// final component.
    pub(in crate::check) fn module_binding_has_user_declaration(
        &self,
        module_name: &str,
        method: &str,
    ) -> bool {
        let owner = self.canonical_module_import_owner(module_name);
        let declaration = format!("{owner}.{method}");
        self.fn_def_spans
            .get(&declaration)
            .is_some_and(|(_, declaring_module)| {
                declaring_module.as_deref() == Some(owner.as_str())
            })
    }

    /// Resolve callback endpoints while the argument still carries its concrete
    /// actor identity. Handler names are selected by the source declaration's
    /// runtime contract; this does not infer a protocol from runtime symbols.
    pub(super) fn resolved_runtime_actor_endpoints(
        &self,
        handler: &Spanned<Expr>,
        data_handler: &str,
        close_handler: &str,
    ) -> Result<crate::check::dispatch::ResolvedActorEndpoints, String> {
        use crate::check::dispatch::{ResolvedActorEndpoint, ResolvedActorEndpoints};
        let key = SpanKey::in_module(&handler.1, self.current_module_idx);
        let ty = self
            .expr_types
            .get(&key)
            .ok_or_else(|| "runtime handler argument has no checked type".to_string())?;
        let Ty::Named {
            head: crate::TypeHead::Actor(actor),
            ..
        } = self.subst.resolve(ty)
        else {
            return Err("runtime handler requires a concrete actor handle".to_string());
        };
        let name = actor.spelling.to_string();
        let canonical = self.canonical_nominal_name(&name).unwrap_or(name.clone());
        let protocol = self
            .actor_protocol_descriptors
            .get(&canonical)
            .or_else(|| self.actor_protocol_descriptors.get(&name))
            .ok_or_else(|| format!("actor `{canonical}` has no receive protocol"))?;
        let actor = self
            .lookup_declaration(&canonical)
            .ok_or_else(|| format!("actor `{canonical}` has no declaration identity"))?;
        let endpoint = |name: &str| -> Result<ResolvedActorEndpoint, String> {
            let receive = protocol
                .handlers
                .iter()
                .find(|handler| handler.name == name)
                .ok_or_else(|| format!("actor `{canonical}` has no `{name}` receive handler"))?;
            if receive.return_ty != ResolvedTy::Unit {
                return Err(format!(
                    "runtime delivery handler `{canonical}::{name}` must return unit"
                ));
            }
            let handler = self
                .lookup_declaration(&format!("{canonical}::{name}"))
                .ok_or_else(|| {
                    format!("handler `{canonical}::{name}` has no declaration identity")
                })?;
            Ok(ResolvedActorEndpoint {
                handler,
                msg_id: receive.msg_id,
            })
        };
        Ok(ResolvedActorEndpoints {
            actor,
            data: endpoint(data_handler)?,
            close: endpoint(close_handler)?,
        })
    }

    /// This compilation's type definitions, read by declaration.
    pub(in crate::check) fn type_def_view(&self) -> crate::check::TypeDefView<'_> {
        crate::check::TypeDefView::new(&self.defs, &self.type_defs)
    }

    /// The declaration a registry key spells: the current module's
    /// declaration of a bare key, its exact declared path, then the key with a
    /// module prefix stripped.
    ///
    /// TRANSITION(A1 commit 3): deleted when every caller holds the head or
    /// id `Scope::resolve` returned instead of a key.
    pub(in crate::check) fn type_def_key(&self, key: &str) -> Option<crate::NominalId> {
        let declared = |path: &str| {
            self.lookup_declaration(path)
                .map(crate::NominalId::from_minted_declaration)
                .filter(|id| self.type_defs.contains_key(id))
        };
        (!key.contains('.'))
            .then(|| self.current_module_identity())
            .flatten()
            .and_then(|owner| declared(&format!("{owner}.{key}")))
            .or_else(|| declared(key))
            .or_else(|| self.strip_module_prefix(key).and_then(declared))
    }

    /// The definition filed under an exact declaration path.
    ///
    /// TRANSITION(A1 commit 3): see [`Self::type_def_key`].
    pub(in crate::check) fn type_def_exact(&self, path: &str) -> Option<&TypeDef> {
        self.type_defs
            .get(&crate::NominalId::from_minted_declaration(
                self.lookup_declaration(path)?,
            ))
    }

    /// The definition a registry key spells (see [`Self::type_def_key`]).
    pub(in crate::check) fn type_def_at(&self, key: &str) -> Option<&TypeDef> {
        self.type_defs.get(&self.type_def_key(key)?)
    }

    /// The definition a registry key spells, mutably.
    pub(in crate::check) fn type_def_at_mut(&mut self, key: &str) -> Option<&mut TypeDef> {
        let id = self.type_def_key(key)?;
        self.type_defs.get_mut(&id)
    }

    /// This compilation's function signatures.
    pub(in crate::check) fn sigs(&self) -> crate::check::FnSigView<'_> {
        crate::check::FnSigView::new(&self.fn_sigs, &self.fn_sig_keys, &self.builtin_fn_sigs)
    }

    /// The signature a key spells.
    ///
    /// TRANSITION(A1 commit 3): see [`crate::check::TypeCheckOutput::fn_sig_keys`].
    pub(in crate::check) fn fn_sig(&self, key: &str) -> Option<&FnSig> {
        self.sigs().get(key)
    }

    /// Whether a key spells a signature.
    pub(in crate::check) fn has_fn_sig(&self, key: &str) -> bool {
        self.sigs().contains(key)
    }

    /// The signature a key spells, mutably.
    pub(in crate::check) fn fn_sig_mut(&mut self, key: &str) -> Option<&mut FnSig> {
        match self.fn_sig_keys.get(key) {
            Some(id) => self.fn_sigs.get_mut(id),
            None => self.builtin_fn_sigs.get_mut(&Symbol::intern(key)),
        }
    }

    /// File `sig` under `declaration`, reachable by `key`.
    pub(in crate::check) fn insert_fn_sig(
        &mut self,
        key: &str,
        declaration: crate::DefId,
        sig: FnSig,
    ) {
        self.fn_sig_keys.insert(key.to_string(), declaration);
        self.fn_sigs.insert(declaration, sig);
    }

    /// File the constructor signature of the member `name` of the declaration
    /// `owner` spells (a variant, a machine state), reachable by `key`.
    pub(in crate::check) fn insert_member_sig(
        &mut self,
        key: &str,
        owner: &str,
        name: Symbol,
        kind: crate::DeclarationKind,
        sig: FnSig,
    ) {
        match self
            .lookup_declaration(owner)
            .and_then(|owner| self.defs.member_of_kind(owner, name, kind))
        {
            Some(member) => self.insert_fn_sig(key, member, sig),
            None => self.insert_fn_sig_at(key, sig),
        }
    }

    /// Make `key` spell the signature `source` spells: an import binding or a
    /// module surface for one declaration.
    ///
    /// TRANSITION(A1 commit 3): a binding is a `Scope` import once callers
    /// resolve through it.
    pub(in crate::check) fn alias_fn_sig(&mut self, key: &str, source: &str) {
        if let Some(declaration) = self.fn_sig_keys.get(source).copied() {
            self.fn_sig_keys.insert(key.to_string(), declaration);
        } else if let Some(sig) = self.builtin_fn_sigs.get(&Symbol::intern(source)).cloned() {
            self.builtin_fn_sigs.insert(Symbol::intern(key), sig);
        }
    }

    /// File `sig` under the declaration `key` names: an established
    /// signature key, an impl or trait method key, or a declaration path.
    /// A key that names no declaration is an internal error, never dropped.
    ///
    /// TRANSITION(A1 commit 3): registration passes the declaration id.
    pub(in crate::check) fn insert_fn_sig_at(&mut self, key: &str, sig: FnSig) {
        let declaration = self
            .fn_sig_keys
            .get(key)
            .copied()
            .or_else(|| self.impl_method_declaration_ids.get(key).copied())
            .or_else(|| self.trait_method_ids.get(key).map(|(_, method)| *method))
            .or_else(|| self.lookup_declaration(key))
            .or_else(|| {
                self.current_module_identity()
                    .and_then(|owner| self.lookup_declaration(&format!("{owner}.{key}")))
            })
            .or_else(|| {
                let (owner, member) = key.rsplit_once("::")?;
                let owner = self
                    .lookup_declaration(owner)
                    .or_else(|| self.lookup_declaration(&self.canonical_nominal_name(owner)?))?;
                self.defs.member(owner, Symbol::intern(member))
            })
            .or_else(|| {
                // A module function a registry publishes before its source is
                // read: the source declaration adopts this row.
                (key.contains('.') && !key.contains("::"))
                    .then(|| self.defs.mint_sourceless_function(key))
            });
        match declaration {
            Some(declaration) => self.insert_fn_sig(key, declaration, sig),
            None => self.errors.push(crate::error::TypeError::new(
                crate::error::TypeErrorKind::InvalidOperation,
                0..0,
                format!("internal: signature `{key}` names no declaration"),
            )),
        }
    }

    /// Look up a type definition by registry key.
    pub(in crate::check) fn lookup_type_def(&self, name: &str) -> Option<TypeDef> {
        self.type_def_at(name).cloned()
    }

    /// Look up a type definition mutably by registry key.
    pub(in crate::check) fn lookup_type_def_mut(&mut self, name: &str) -> Option<&mut TypeDef> {
        self.type_def_at_mut(name)
    }

    /// File a definition under the declaration a registration key names in
    /// the current module. A key no declaration answers to is an import
    /// surface spelling of a declaration filed under its own identity.
    ///
    /// TRANSITION(A1 commit 3): registration passes the declaration id.
    pub(in crate::check) fn insert_type_def(&mut self, key: &str, def: TypeDef) {
        let declared = |path: &str| {
            self.lookup_declaration(path)
                .map(crate::NominalId::from_minted_declaration)
        };
        let id = if key.contains('.') {
            declared(key)
        } else {
            self.current_module_identity()
                .and_then(|owner| declared(&format!("{owner}.{key}")))
                .or_else(|| declared(key))
        };
        if let Some(id) = id {
            // TRANSITION(A1 commit 3): WHY a declaration is re-registered after
            // impls published methods onto it, and the rebuilt definition must
            // keep them. WHEN the dispatch table owns methods, `td.methods` is
            // deleted and this merge with it. WHAT: impl methods are dispatch
            // rows keyed by head, never members of the definition.
            let mut def = def;
            if let Some(existing) = self.type_defs.get(&id) {
                for (name, sig) in &existing.methods {
                    def.methods
                        .entry(name.clone())
                        .or_insert_with(|| sig.clone());
                }
            }
            self.type_defs.insert(id, def);
        }
    }

    /// Resolve a `(module, type)` pair to its `TypeDef`, gated on the type being
    /// in the imported module's exported set.  Returns `None` if the module is
    /// not a known alias, the type is not exported, or the qualified type alias
    /// was not registered (latter would be a registration bug — callers should
    /// treat as "type not exported" for diagnostic purposes).
    ///
    /// Mirrors the `module_fn_exports` guard pattern used by
    /// `check_method_call` for module-qualified function dispatch.
    pub(in crate::check) fn resolve_module_type(
        &self,
        module_short: &str,
        type_name: &str,
    ) -> Option<TypeDef> {
        if !self.module_binding_in_current_file(module_short) {
            return None;
        }
        let resolved_module = self
            .module_import_bindings
            .get(&(
                self.current_module.clone(),
                self.current_module_idx,
                module_short.to_string(),
            ))
            .map(String::as_str)?;
        let exports = self.module_type_exports.get(resolved_module)?;
        if !exports.contains(type_name) {
            return None;
        }
        let qualified = format!("{resolved_module}.{type_name}");
        self.type_def_at(&qualified).cloned()
    }

    /// Return the exact source owner's exported type set for a lexical module
    /// binding. Diagnostics use this helper as well as successful resolution so
    /// suggestions never accidentally consult a same-leaf surface key.
    pub(in crate::check) fn module_type_exports_for_binding(
        &self,
        module_short: &str,
    ) -> Option<&HashSet<String>> {
        if !self.module_binding_in_current_file(module_short) {
            return None;
        }
        let owner = self.module_import_bindings.get(&(
            self.current_module.clone(),
            self.current_module_idx,
            module_short.to_string(),
        ))?;
        self.module_type_exports.get(owner)
    }

    /// Canonicalize a supervisor child's user-spelled `actor_type` to the
    /// registered actor identity.
    ///
    /// A supervisor child records its actor type as the raw source string the
    /// user wrote (`child b: bank.Account` stores `bank.Account`). For a
    /// package-module child that spelling carries the user's import *alias*
    /// (`bank`), whereas the checker registers the actor under its exact source
    /// owner (`hew.bank.Account`, keyed off `current_module`). Left raw, the
    /// alias-prefixed string never matches the canonical `fn_sigs` /
    /// `actor_init_params` / `type_defs` keys, so `bank.Account`'s own
    /// actor-handle type finds no `receive fn` and every wall keyed on the
    /// actor identity silently skips.
    ///
    /// Resolve dotted module bindings and bare named/aliased import bindings
    /// through the same lexical facts ordinary type resolution consumes. A
    /// declaration authored in the current scope wins before an import, and a
    /// bare import resolves only when that exact binding published one source
    /// identity. There is deliberately no scan over globally loaded exports.
    pub(in crate::check) fn resolve_supervisor_child_type(&self, raw: &str) -> Option<String> {
        if let Some((module_short, type_name)) = raw.split_once('.') {
            return self
                .resolve_module_type(module_short, type_name)
                .map(|td| td.name);
        }

        if self.supervisor_children.contains_key(raw) {
            return Some(raw.to_string());
        }

        // A supervisor declared inside a non-root module shares that module's
        // nominal scope with its actors. Resolve only the exact owner-qualified
        // actor declaration; never search another loaded module by leaf name.
        // This rung precedes selected imports so a same-file actor retains
        // lexical authority over an imported binding with the same spelling.
        if let Some(owner) = self.current_module_identity() {
            let local_actor = format!("{owner}.{raw}");
            if self
                .type_def_exact(&local_actor)
                .is_some_and(|type_def| type_def.kind == TypeDefKind::Actor)
            {
                return Some(local_actor);
            }
        }

        if self.local_type_defs.contains(raw) || self.source_type_defs.contains(raw) {
            let local = self.declaration_identity(raw);
            if self.type_def_at(&local).is_some() {
                return Some(local);
            }
            if self.type_def_at(raw).is_some() {
                return Some(raw.to_string());
            }
            // A flattened file import is root-visible in the source sets but
            // its compatibility leaf key is retired after registration. Fall
            // through to the exact published bare binding below; a genuine
            // current-scope declaration returned from one of the two keys.
        }

        // Root actors and flattened file-import actors both publish an exact
        // root-surface key. This is not a leaf search: the key exists only
        // because that spelling was registered into the current root scope.
        if self.current_module_identity().is_none() && self.type_def_at(raw).is_some() {
            return Some(raw.to_string());
        }

        if let Some(identity) = self.published_bare_type_qualified(raw) {
            if let Some(owner) = self.unqualified_to_module.get(&(
                self.current_module.clone(),
                self.current_module_idx,
                raw.to_string(),
            )) {
                self.mark_module_owner_bindings_used(owner);
            }
            return Some(identity);
        }
        None
    }

    pub(in crate::check) fn canonical_supervisor_child_type(&self, raw: &str) -> String {
        self.resolve_supervisor_child_type(raw)
            .unwrap_or_else(|| raw.to_string())
    }

    /// Resolve a `(module, type, variant)` triple to its `VariantDef`, gated on
    /// the type being exported by the module.  Returns `None` if the module
    /// alias is unknown, the type is not exported, or the variant does not
    /// exist on the type.  The caller is responsible for emitting the
    /// fail-closed diagnostic in each failure case.
    pub(in crate::check) fn resolve_module_variant(
        &self,
        module_short: &str,
        type_name: &str,
        variant_name: &str,
    ) -> Option<(TypeDef, VariantDef)> {
        let td = self.resolve_module_type(module_short, type_name)?;
        let v = td.variants.get(variant_name).cloned()?;
        Some((td, v))
    }

    /// Full canonical owner path of the module whose declarations are being
    /// checked. Use this for declaration identity and layout-facing type
    /// lookup.
    pub(in crate::check) fn current_module_identity(&self) -> Option<&str> {
        self.current_module.as_deref()
    }

    /// The identity a declaration written in the scope currently being checked
    /// is published under: `{module}.{bare_name}` inside a module, the bare
    /// name at the root program.
    ///
    /// This is the one formula for a declaration's own name. Registration mints
    /// the `TypeDef` key and the declaration's `Ty::Named` with it, and every
    /// later pass that has to name that same declaration - a machine's
    /// transition bodies, for instance - must mint it the same way, because the
    /// bare spelling is only a transient row on the import path and is retired
    /// once the canonical owner is published (`retire_imported_type_keys`).
    pub(in crate::check) fn declaration_identity(&self, bare_name: &str) -> String {
        self.current_module_identity().map_or_else(
            || bare_name.to_string(),
            |module| format!("{module}.{bare_name}"),
        )
    }

    /// Resolve a bare actor reference to its registered checker identity.
    ///
    /// Resolution order (local-first, mirroring `per-module-type-identity`):
    /// 1. the current module's own actor (`{current_full_path}.{name}`)
    /// 2. a root/flat actor registered under the bare name
    /// 3. a named-import binding (`unqualified_to_module`)
    /// 4. the modules exporting an actor of that name: exactly one resolves
    ///    to it; two or more is `Ambiguous` (never silent first-wins).
    pub(in crate::check) fn resolve_bare_actor_identity(&self, name: &str) -> BareActorResolution {
        self.resolve_bare_declaration_identity(name, &[TypeDefKind::Actor])
    }

    /// Resolve a bare `spawn` target. A supervisor is spawned exactly as an
    /// actor is, so both declaration kinds answer to the same resolution.
    pub(in crate::check) fn resolve_bare_spawn_target_identity(
        &self,
        name: &str,
    ) -> BareActorResolution {
        self.resolve_bare_declaration_identity(name, &[TypeDefKind::Actor, TypeDefKind::Supervisor])
    }

    pub(super) fn resolve_bare_declaration_identity(
        &self,
        name: &str,
        kinds: &[TypeDefKind],
    ) -> BareActorResolution {
        let is_actor = |key: &str| {
            self.type_def_exact(key)
                .is_some_and(|td| kinds.contains(&td.kind))
        };
        if let Some(module) = self.current_module.as_deref() {
            let dotted = format!("{module}.{name}");
            if is_actor(&dotted) {
                return BareActorResolution::Resolved(dotted);
            }
        }
        if is_actor(name) {
            return BareActorResolution::Resolved(name.to_string());
        }
        if let Some(owners) = self.published_bare_type_owners.get(&(
            self.current_module.clone(),
            self.current_module_idx,
            name.to_string(),
        )) {
            let candidates: Vec<String> = owners
                .iter()
                .filter(|identity| is_actor(identity))
                .cloned()
                .collect();
            match candidates.as_slice() {
                [identity] => return BareActorResolution::Resolved(identity.clone()),
                [] => {}
                _ => {
                    let modules = candidates
                        .iter()
                        .filter_map(|identity| identity.rsplit_once('.'))
                        .map(|(module, _)| module.to_string())
                        .collect();
                    return BareActorResolution::Ambiguous(modules);
                }
            }
        }
        let mut candidates: Vec<&str> = self
            .module_type_exports
            .iter()
            .filter(|(module, exports)| {
                exports.contains(name) && is_actor(&format!("{module}.{name}"))
            })
            .map(|(module, _)| module.as_str())
            .collect();
        candidates.sort_unstable();
        match candidates.as_slice() {
            [] => BareActorResolution::Unknown,
            [module] => BareActorResolution::Resolved(format!("{module}.{name}")),
            _ => {
                BareActorResolution::Ambiguous(candidates.iter().map(ToString::to_string).collect())
            }
        }
    }
}
