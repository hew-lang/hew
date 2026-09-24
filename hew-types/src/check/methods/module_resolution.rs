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
            name,
            builtin: Some(BuiltinType::ActorHandle),
            ..
        } = self.subst.resolve(ty)
        else {
            return Err("runtime handler requires a concrete actor handle".to_string());
        };
        let canonical = self.canonical_nominal_name(&name).unwrap_or(name.clone());
        let protocol = self
            .actor_protocol_descriptors
            .get(&canonical)
            .or_else(|| self.actor_protocol_descriptors.get(&name))
            .ok_or_else(|| format!("actor `{canonical}` has no receive protocol"))?;
        let actor = self
            .lookup_declaration(&canonical)
            .cloned()
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
                .cloned()
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

    /// Look up a type definition, handling module-qualified names like `json.Value`.
    pub(in crate::check) fn lookup_type_def(&self, name: &str) -> Option<TypeDef> {
        let current_module_key = if name.contains('.') {
            None
        } else {
            self.current_module_identity()
                .map(|owner| format!("{owner}.{name}"))
        };
        self.type_defs
            .get(name)
            .or_else(|| {
                current_module_key
                    .as_ref()
                    .and_then(|key| self.type_defs.get(key))
            })
            .or_else(|| {
                self.strip_module_prefix(name)
                    .and_then(|u| self.type_defs.get(u))
            })
            .cloned()
    }

    /// Look up a type definition mutably, handling module-qualified names.
    pub(in crate::check) fn lookup_type_def_mut(&mut self, name: &str) -> Option<&mut TypeDef> {
        if self.type_defs.contains_key(name) {
            return self.type_defs.get_mut(name);
        }
        if !name.contains('.') {
            if let Some(owner) = self.current_module_identity() {
                let current_module_key = format!("{owner}.{name}");
                if self.type_defs.contains_key(&current_module_key) {
                    return self.type_defs.get_mut(&current_module_key);
                }
            }
        }
        let unqualified = self.strip_module_prefix(name)?;
        self.type_defs.get_mut(unqualified)
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
        self.type_defs.get(&qualified).cloned()
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
                .type_defs
                .get(&local_actor)
                .is_some_and(|type_def| type_def.kind == TypeDefKind::Actor)
            {
                return Some(local_actor);
            }
        }

        if self.local_type_defs.contains(raw) || self.source_type_defs.contains(raw) {
            let local = self.declaration_identity(raw);
            if self.type_defs.contains_key(&local) {
                return Some(local);
            }
            if self.type_defs.contains_key(raw) {
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
        if self.current_module_identity().is_none() && self.type_defs.contains_key(raw) {
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
            self.type_defs
                .get(key)
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
