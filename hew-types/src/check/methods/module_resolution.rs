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
    pub(in crate::check) fn canonical_module_import_owner(
        &self,
        module_name: hew_parser::ast::Ident,
    ) -> String {
        let ident = module_name;
        if let Some(scope::Resolution::Module(module)) = self
            .scope_site()
            .and_then(|site| self.scopes.value_head(&self.env, site, ident))
        {
            return self.defs.module_path(module).to_string();
        }
        let module_name = ident.name.as_str();
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
    pub(in crate::check) fn module_binding_in_current_file(
        &self,
        module_name: hew_parser::ast::Ident,
    ) -> bool {
        let ident = module_name;
        if let Some(scope::Resolution::Module(_)) = self
            .scope_site()
            .and_then(|site| self.scopes.value_head(&self.env, site, ident))
        {
            return true;
        }
        let module_name = ident.name.as_str();
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
        let owner = self.canonical_module_import_owner(Ident::new(module_name));
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

    /// The member `method` of the actor a head names, of declaration `kind`
    /// (a `receive fn` or a private `fn`).
    pub(in crate::check) fn actor_member(
        &self,
        head: crate::TypeHead,
        method: &str,
        kind: crate::DeclarationKind,
    ) -> Option<crate::DefId> {
        let actor = head.nominal()?.declaration();
        self.defs
            .member_of_kind(actor, Symbol::intern(method), kind)
    }

    /// This compilation's type definitions, read by declaration.
    pub(in crate::check) fn type_def_view(&self) -> crate::check::TypeDefView<'_> {
        crate::check::TypeDefView::new(&self.defs, &self.type_defs)
    }

    /// The definition a type head names: a nominal's or actor's own, a
    /// builtin's std declaration.
    pub(in crate::check) fn head_type_def(&self, head: crate::TypeHead) -> Option<&TypeDef> {
        self.type_defs.get(&head.declaration(&self.defs)?)
    }

    /// The definition the head of a named type names.
    pub(in crate::check) fn ty_type_def(&self, ty: &Ty) -> Option<&TypeDef> {
        match ty {
            Ty::Named { head, .. } => self.head_type_def(*head),
            _ => None,
        }
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

    /// Resolve a type item in its declaring namespace without consulting
    /// imports or the prelude.
    pub(in crate::check) fn nominal_in_module(
        &self,
        module: crate::ModuleId,
        ident: Ident,
    ) -> Option<crate::NominalId> {
        match self.scopes.item(self.scopes.namespace_of(module), ident)? {
            scope::Binding::Type(id) | scope::Binding::Actor(id) => Some(id),
            _ => None,
        }
    }

    /// The event type declared by a particular machine.
    pub(in crate::check) fn machine_event(
        &self,
        machine: crate::NominalId,
    ) -> Option<crate::NominalId> {
        self.defs
            .member_of_kind(
                machine.declaration(),
                hew_parser::ast::sym::EVENT,
                crate::DeclarationKind::MachineEventType,
            )
            .map(crate::NominalId::of_declaration)
    }

    /// The definition a registry key spells (see [`Self::type_def_key`]).
    pub(in crate::check) fn type_def_at(&self, key: &str) -> Option<&TypeDef> {
        self.type_defs.get(&self.type_def_key(key)?)
    }

    /// This compilation's function signatures.
    pub(in crate::check) fn sigs(&self) -> crate::check::FnSigView<'_> {
        crate::check::FnSigView::new(
            &self.defs,
            &self.fn_sigs,
            &self.impl_method_declaration_ids,
            &self.dispatch,
        )
    }

    /// The signature a key spells.
    ///
    /// TRANSITION(A1 commit 4): the remaining callers pass rendered names
    /// instead of the callable declaration.
    pub(in crate::check) fn fn_sig(&self, key: &str) -> Option<&FnSig> {
        self.fn_sigs.get(&self.signature_declaration(key)?)
    }

    pub(in crate::check) fn signature_declaration(&self, key: &str) -> Option<crate::DefId> {
        self.lookup_declaration(key)
            .filter(|id| self.fn_sigs.contains_key(id))
            .or_else(|| self.impl_method_declaration_ids.get(key).copied())
            .or_else(|| {
                let (module, name) = key.rsplit_once('.')?;
                let module = self.defs.module_for_path(module)?;
                match self
                    .scopes
                    .item(self.scopes.namespace_of(module), Ident::new(name))?
                {
                    scope::Binding::Fn(declaration) => Some(declaration),
                    _ => None,
                }
            })
            .or_else(|| match self.scopes.prelude_binding(Symbol::intern(key)) {
                Some(scope::Binding::Fn(declaration)) => Some(declaration),
                _ => self.defs.lookup_builtin_callable(Symbol::intern(key)),
            })
    }

    /// Whether a key spells a signature.
    pub(in crate::check) fn has_fn_sig(&self, key: &str) -> bool {
        self.fn_sig(key).is_some()
    }

    /// The signature a key spells, mutably.
    pub(in crate::check) fn fn_sig_mut(&mut self, key: &str) -> Option<&mut FnSig> {
        let declaration = self.signature_declaration(key)?;
        self.fn_sigs.get_mut(&declaration)
    }

    /// File the signature on its declaration.
    pub(in crate::check) fn insert_fn_sig(&mut self, declaration: crate::DefId, sig: FnSig) {
        self.fn_sigs.insert(declaration, sig);
    }

    /// Publish a constructor signature on the owner's declared member.
    pub(in crate::check) fn insert_member_sig(
        &mut self,
        owner: crate::DefId,
        name: Symbol,
        kind: crate::DeclarationKind,
        sig: FnSig,
    ) {
        if let Some(member) = self.defs.member_of_kind(owner, name, kind) {
            self.insert_fn_sig(member, sig);
        } else {
            self.errors.push(crate::error::TypeError::new(
                crate::error::TypeErrorKind::InvalidOperation,
                0..0,
                format!(
                    "internal: `{}::{name}` has no constructor declaration",
                    self.defs.path(owner)
                ),
            ));
        }
    }

    /// The trait declaration `id` names, when registered.
    pub(in crate::check) fn trait_info(&self, id: crate::DefId) -> Option<&TraitInfo> {
        self.trait_defs.get(&id)
    }

    /// File `info` under the trait declaration at `path`. A path that names
    /// no declaration is an internal error, never dropped.
    pub(in crate::check) fn insert_trait_def(
        &mut self,
        path: &str,
        info: TraitInfo,
    ) -> Option<crate::DefId> {
        let declaration = self.lookup_declaration(path);
        if let Some(declaration) = declaration {
            self.trait_defs.insert(declaration, info);
        } else {
            self.errors.push(crate::error::TypeError::new(
                crate::error::TypeErrorKind::InvalidOperation,
                0..0,
                format!("internal: trait `{path}` names no declaration"),
            ));
        }
        declaration
    }

    /// The direct super-traits of trait `id`.
    pub(in crate::check) fn trait_supers(
        &self,
        id: crate::DefId,
    ) -> impl Iterator<Item = crate::DefId> + '_ {
        self.trait_super
            .get(&id)
            .into_iter()
            .flatten()
            .map(|bound| bound.trait_id)
    }

    /// Look up a type definition by registry key.
    pub(in crate::check) fn lookup_type_def(&self, name: &str) -> Option<TypeDef> {
        self.type_def_at(name).cloned()
    }

    /// Publish a checked definition on its minted declaration.
    pub(in crate::check) fn file_type_def(&mut self, id: crate::NominalId, def: TypeDef) {
        self.type_defs.insert(id, def);
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
        let nominal = self.resolve_module_type_declaration(module_short, type_name)?;
        self.type_defs.get(&nominal).cloned()
    }

    pub(in crate::check) fn resolve_module_type_declaration(
        &self,
        module_short: &str,
        type_name: &str,
    ) -> Option<crate::NominalId> {
        if !self.module_binding_in_current_file(Ident::new(module_short)) {
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
        self.type_def_key(&qualified)
    }

    /// Return the exact source owner's exported type set for a lexical module
    /// binding. Diagnostics use this helper as well as successful resolution so
    /// suggestions never accidentally consult a same-leaf surface key.
    pub(in crate::check) fn module_type_exports_for_binding(
        &self,
        module_short: &str,
    ) -> Option<&HashSet<String>> {
        if !self.module_binding_in_current_file(Ident::new(module_short)) {
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
    /// Resolve a child through the same declaration scope as its actor type.
    pub(in crate::check) fn resolve_supervisor_child_type(
        &mut self,
        path: &hew_parser::ast::Path,
    ) -> Option<String> {
        let super::super::scope::Resolution::Nominal(owner) = self.resolve_type_path(path)? else {
            return None;
        };
        if path.segments.len() > 1 {
            let child_span = path.segments.last().map_or(&(0..0), |(_, span)| span);
            if !self.check_declaration_visible(owner.declaration(), child_span) {
                return None;
            }
        }
        Some(self.defs.path(owner.declaration()).to_string())
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
    pub(in crate::check) fn declaration_identity(
        &self,
        bare_name: hew_parser::ast::Ident,
    ) -> String {
        let ident = bare_name;
        if let Some(id) = self.named_item_declaration(ident) {
            return self.defs.path(id).to_string();
        }
        let bare_name = ident.name.as_str();
        self.current_module_identity().map_or_else(
            || bare_name.to_string(),
            |module| format!("{module}.{bare_name}"),
        )
    }

    /// Resolve a bare `spawn` target. A supervisor is spawned exactly as an
    /// actor is, so both declaration kinds answer to the same resolution.
    pub(in crate::check) fn resolve_bare_spawn_target_identity(
        &self,
        name: hew_parser::ast::Ident,
    ) -> BareActorResolution {
        let ident = name;
        if let Some(id) = self.named_item_declaration(ident) {
            if matches!(
                self.defs.kind(id),
                crate::DeclarationKind::Actor | crate::DeclarationKind::Supervisor
            ) {
                return BareActorResolution::Resolved(crate::NominalId::of_declaration(id));
            }
        }
        self.resolve_bare_declaration_identity(
            ident.name.as_str(),
            &[TypeDefKind::Actor, TypeDefKind::Supervisor],
        )
    }

    pub(super) fn resolve_bare_declaration_identity(
        &self,
        name: &str,
        kinds: &[TypeDefKind],
    ) -> BareActorResolution {
        let actor_at = |key: &str| {
            let nominal = crate::NominalId::of_declaration(self.lookup_declaration(key)?);
            self.type_defs
                .get(&nominal)
                .filter(|td| kinds.contains(&td.kind))
                .map(|_| nominal)
        };
        if let Some(module) = self.current_module.as_deref() {
            let dotted = format!("{module}.{name}");
            if let Some(nominal) = actor_at(&dotted) {
                return BareActorResolution::Resolved(nominal);
            }
        }
        if let Some(nominal) = actor_at(name) {
            return BareActorResolution::Resolved(nominal);
        }
        if let Some(owners) = self.published_bare_type_owners.get(&(
            self.current_module.clone(),
            self.current_module_idx,
            name.to_string(),
        )) {
            let candidates: Vec<_> = owners
                .iter()
                .filter_map(|identity| Some((identity, actor_at(identity)?)))
                .collect();
            match candidates.as_slice() {
                [(_, nominal)] => return BareActorResolution::Resolved(*nominal),
                [] => {}
                _ => {
                    let modules = candidates
                        .iter()
                        .filter_map(|(identity, _)| identity.rsplit_once('.'))
                        .map(|(module, _)| module.to_string())
                        .collect();
                    return BareActorResolution::Ambiguous(modules);
                }
            }
        }
        let mut candidates: Vec<_> = self
            .module_type_exports
            .iter()
            .filter(|(_, exports)| exports.contains(name))
            .filter_map(|(module, _)| Some((module, actor_at(&format!("{module}.{name}"))?)))
            .collect();
        candidates.sort_unstable();
        match candidates.as_slice() {
            [] => BareActorResolution::Unknown,
            [(_, nominal)] => BareActorResolution::Resolved(*nominal),
            _ => BareActorResolution::Ambiguous(
                candidates
                    .into_iter()
                    .map(|(module, _)| module.clone())
                    .collect(),
            ),
        }
    }
}
