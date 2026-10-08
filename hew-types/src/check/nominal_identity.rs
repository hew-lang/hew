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
        binders: &[crate::ParamHead],
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
        if args.is_empty() {
            if let Some(parameter) = binders
                .iter()
                .find(|binder| binder.spelling.as_str() == spelling)
            {
                return crate::ty::Ty::param(*parameter);
            }
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
            .lookup_declaration(key)
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
        let file = self.current_declaration_module()?;
        let publish = !self.registering_embedded_source;
        Some(super::scope::ScopeSite {
            file,
            span_file: self.current_module_idx,
            publish,
        })
    }

    /// Publish the exact lexical binding at a declaration or use site.
    pub(super) fn record_local_resolution(
        &mut self,
        name: hew_parser::ast::Ident,
        span: &hew_parser::ast::Span,
    ) {
        let Some(binding) = self.env.lookup_ref(name) else {
            return;
        };
        let Some(site) = self.scope_site() else {
            return;
        };
        self.scopes
            .record_resolution(site, span, super::scope::Resolution::Local(binding.id));
    }

    /// Record the item prefix of a written value path. `Scope` stops at the
    /// first value member, which the field or call checker publishes after it
    /// selects that member from the receiver's type.
    pub(super) fn record_value_path_resolution(
        &mut self,
        expr: &hew_parser::ast::Expr,
        span: &hew_parser::ast::Span,
    ) {
        fn segments(
            expr: &hew_parser::ast::Expr,
            span: &hew_parser::ast::Span,
            out: &mut Vec<hew_parser::ast::Spanned<hew_parser::ast::Ident>>,
        ) -> bool {
            match expr {
                hew_parser::ast::Expr::Ident(name) => {
                    out.push((*name, span.clone()));
                    true
                }
                hew_parser::ast::Expr::FieldAccess { object, field } => {
                    if !segments(&object.0, &object.1, out) {
                        return false;
                    }
                    out.push(field.clone());
                    true
                }
                _ => false,
            }
        }

        let Some(site) = self.scope_site() else {
            return;
        };
        let mut path = Vec::new();
        if segments(expr, span, &mut path) {
            let _ =
                self.scopes
                    .resolve_prefix(&self.env, site, super::scope::Namespace::Value, &path);
            if let Some((_, written)) = path.first() {
                let key = super::types::SpanKey::in_module(written, self.current_module_idx);
                if let Some(super::scope::Resolution::Local(binding)) =
                    self.scopes.resolutions().get(&key)
                {
                    if let Some((owner, index)) = self.actor_field_binding_ids.get(binding) {
                        self.scopes.record_resolution(
                            site,
                            written,
                            super::scope::Resolution::Field(*owner, *index),
                        );
                    }
                }
            }
            // An identifier expression's span can include the whitespace up
            // to the next token. Retain that expression key for compiler
            // consumers and publish the written token for editor consumers.
            if let Some((name, written)) = path.first() {
                let exact_end = written.start.saturating_add(name.name.as_str().len());
                if exact_end < written.end {
                    let key = super::types::SpanKey::in_module(written, self.current_module_idx);
                    if let Some(resolution) = self.scopes.resolutions().get(&key).copied() {
                        self.scopes.record_resolution(
                            site,
                            &(written.start..exact_end),
                            resolution,
                        );
                    }
                }
            }
        }
    }

    /// Publish a record field chosen from the receiver's resolved nominal.
    /// The field index is declaration order, not hash-map iteration order.
    pub(super) fn record_field_resolution(
        &mut self,
        object: &hew_parser::ast::Spanned<hew_parser::ast::Expr>,
        field: &hew_parser::ast::Spanned<hew_parser::ast::Ident>,
    ) {
        let key = super::types::SpanKey::in_module(&object.1, self.current_module_idx);
        let Some(receiver) = self.expr_types.get(&key) else {
            return;
        };
        let crate::Ty::Named { head, .. } = self.subst.resolve(receiver) else {
            return;
        };
        let Some(nominal) = head.nominal() else {
            return;
        };
        let Some(definition) = self.type_defs.get(&nominal) else {
            return;
        };
        let Some(index) = definition
            .field_order
            .iter()
            .position(|name| name == field.0.name.as_str())
        else {
            return;
        };
        let Some(site) = self.scope_site() else {
            return;
        };
        self.scopes.record_resolution(
            site,
            &field.1,
            super::scope::Resolution::Field(
                nominal,
                u32::try_from(index).expect("more than u32::MAX record fields"),
            ),
        );
    }

    /// The checker has already selected `self.field` as actor state. Publish
    /// that member at both the whole projection used by HIR and its written
    /// field token used by source navigation.
    pub(super) fn record_actor_state_projection_resolution(
        &mut self,
        span: &hew_parser::ast::Span,
        field: &hew_parser::ast::Spanned<hew_parser::ast::Ident>,
    ) {
        let key = super::types::SpanKey::in_module(span, self.current_module_idx);
        if !self.actor_self_state_fields.contains(&key) {
            return;
        }
        let Some(crate::Ty::Named { head, .. }) = self.current_actor_type.as_ref() else {
            return;
        };
        let Some(owner) = head.nominal() else {
            return;
        };
        let Some(index) = self
            .current_actor_fields
            .iter()
            .position(|member| member.name == field.0.name.as_str())
        else {
            return;
        };
        let Some(site) = self.scope_site() else {
            return;
        };
        let resolution = super::scope::Resolution::Field(
            owner,
            u32::try_from(index).expect("more than u32::MAX actor fields"),
        );
        self.scopes.record_resolution(site, span, resolution);
        self.scopes.record_resolution(site, &field.1, resolution);
        let token_end = field.1.start + field.0.name.as_str().len();
        if token_end < field.1.end {
            self.scopes
                .record_resolution(site, &(field.1.start..token_end), resolution);
        }
    }

    /// Publish labels after the record constructor has selected its nominal.
    pub(super) fn record_struct_init_field_resolutions(
        &mut self,
        fields: &[(
            hew_parser::ast::Ident,
            hew_parser::ast::Spanned<hew_parser::ast::Expr>,
        )],
        labels: &[hew_parser::ast::FieldLabel],
        ty: &crate::Ty,
    ) {
        let crate::Ty::Named { head, .. } = self.subst.resolve(ty) else {
            return;
        };
        let Some(nominal) = head.nominal() else {
            return;
        };
        let Some(definition) = self.type_defs.get(&nominal) else {
            return;
        };
        let Some(site) = self.scope_site() else {
            return;
        };
        for ((field, _), label) in fields.iter().zip(labels) {
            if let Some(index) = definition
                .field_order
                .iter()
                .position(|name| name == field.name.as_str())
            {
                let index = u32::try_from(index).expect("more than u32::MAX record fields");
                if label.shorthand {
                    self.scopes
                        .record_shorthand_label(site, &label.span, (nominal, index));
                } else {
                    self.scopes.record_resolution(
                        site,
                        &label.span,
                        super::scope::Resolution::Field(nominal, index),
                    );
                }
            }
        }
    }

    /// Publish the declaration the completed call checker selected for its
    /// written callee segment. Runtime and indirect calls have no source
    /// declaration to publish here.
    pub(super) fn record_call_resolution(
        &mut self,
        call_span: &hew_parser::ast::Span,
        callee_span: &hew_parser::ast::Span,
        method_like: bool,
    ) {
        let key = super::types::SpanKey::in_module(call_span, self.current_module_idx);
        let target = self
            .method_call_rewrites
            .get(&key)
            .and_then(|rewrite| match rewrite {
                MethodCallRewrite::RewriteToFunction { target, .. }
                | MethodCallRewrite::RewriteModuleQualifiedToFunction { target, .. }
                | MethodCallRewrite::StaticTraitDispatch { target, .. }
                | MethodCallRewrite::BinderStaticCall(super::types::BinderTraitCall {
                    target,
                    ..
                }) => Some(target),
                _ => None,
            })
            .or_else(|| self.direct_call_targets.get(&key));
        let Some(target) = target else {
            return;
        };
        let resolution = match target {
            CallTarget::User(id)
            | CallTarget::RecordConstructor(id)
            | CallTarget::Extern {
                declaration: id, ..
            }
            | CallTarget::DeclaredRuntime {
                declaration: id, ..
            } => {
                if method_like {
                    super::scope::Resolution::Member(*id)
                } else {
                    super::scope::Resolution::Def(*id)
                }
            }
            CallTarget::ImplMethod(id)
            | CallTarget::DynamicVtable { method: id, .. }
            | CallTarget::StaticTraitMethod { method: id, .. } => {
                super::scope::Resolution::Member(*id)
            }
            _ => return,
        };
        let Some(site) = self.scope_site() else {
            return;
        };
        self.scopes.record_resolution(site, callee_span, resolution);
    }

    /// The program's entry function: the `main` its root module declares.
    pub(super) fn entry_function(&self) -> Option<crate::DefId> {
        let root = self.defs.root_module()?;
        match self
            .scopes
            .item(self.scopes.namespace_of(root), hew_parser::ast::sym::MAIN)?
        {
            super::scope::Binding::Fn(id) => Some(id),
            _ => None,
        }
    }

    /// The trait or predicate a written bound path names, resolved through
    /// `Scope`.
    pub(super) fn resolve_trait_path(
        &mut self,
        path: &hew_parser::ast::Path,
    ) -> Option<crate::DefId> {
        let site = self.scope_site()?;
        match self.scopes.resolve(
            &self.env,
            site,
            super::scope::Namespace::Type,
            &path.segments,
        ) {
            Ok(super::scope::Resolution::Def(id))
                if self.defs.kind(id) == crate::DeclarationKind::Trait =>
            {
                Some(id)
            }
            _ => None,
        }
    }

    /// The declaration impl registration minted for one impl method, by its
    /// source occurrence.
    pub(super) fn impl_method_declaration(
        &self,
        method: &hew_parser::ast::FnDecl,
    ) -> Option<crate::DefId> {
        self.defs
            .declaration(crate::DeclarationOccurrence::new_with_synthetic_ordinal(
                self.current_declaration_module(),
                &method.fn_span,
                self.current_item_ordinal,
                crate::DeclarationKind::ImplMethod,
                0,
            ))
    }

    /// What a written type path names, resolved through `Scope`.
    pub(super) fn resolve_type_path(
        &mut self,
        path: &hew_parser::ast::Path,
    ) -> Option<super::scope::Resolution> {
        let site = self.scope_site()?;
        self.scopes
            .resolve(
                &self.env,
                site,
                super::scope::Namespace::Type,
                &path.segments,
            )
            .ok()
    }

    /// The type a `Scope` resolution of a written type path denotes, or
    /// `None` when the path names no type.
    pub(super) fn named_ty_from_resolution(
        &mut self,
        resolution: Option<super::scope::Resolution>,
        path: &hew_parser::ast::Path,
        args: &[crate::Ty],
        span: &hew_parser::ast::Span,
    ) -> Option<crate::Ty> {
        use super::scope::Resolution;
        let head = match resolution? {
            Resolution::Param(id) => {
                crate::TypeHead::param(crate::ParamHead::new(id, path.segments.last()?.0.name))
            }
            Resolution::Builtin(builtin) => crate::TypeHead::Builtin(builtin),
            Resolution::Nominal(id) => {
                // A bare name reaches a declaration of its own module or one
                // an import admitted; only a module-qualified path can name
                // another module's private declaration.
                if path.segments.len() > 1
                    && !self.check_declaration_visible(id.declaration(), span)
                {
                    return Some(crate::Ty::Error);
                }
                if let Some(alias) = self.type_aliases.get(&id.declaration()) {
                    if args.len() != alias.type_params.len() {
                        let expected = alias.type_params.len();
                        self.report_error(
                            super::TypeErrorKind::ArityMismatch,
                            span,
                            format!(
                                "type alias `{path}` expects {expected} type argument(s), found {}",
                                args.len()
                            ),
                        );
                        return Some(crate::Ty::Error);
                    }
                }
                self.head_of_declaration(id)
            }
            // A trait written in type position names the trait's declaration.
            Resolution::Def(id) if self.defs.kind(id) == crate::DeclarationKind::Trait => {
                if path.segments.len() > 1 && !self.check_declaration_visible(id, span) {
                    return Some(crate::Ty::Error);
                }
                crate::TypeHead::Nominal(crate::NominalHead::new(
                    crate::NominalId::from_minted_declaration(id),
                    self.defs.path(id),
                ))
            }
            _ => return None,
        };
        Some(match head {
            crate::TypeHead::Builtin(crate::BuiltinType::CancellationToken) if args.is_empty() => {
                crate::Ty::CancellationToken
            }
            head => crate::Ty::named_head(head, args.to_vec()),
        })
    }

    /// Whether the current file may name `declaration`; reports the first
    /// refusal per declaration.
    pub(super) fn check_declaration_visible(
        &mut self,
        declaration: crate::DefId,
        span: &hew_parser::ast::Span,
    ) -> bool {
        use hew_parser::ast::Visibility;
        let (Some(site), Some(owner)) = (self.scope_site(), self.defs.module(declaration)) else {
            return true;
        };
        let owner = self.scopes.namespace_of(owner);
        let here = self.scopes.namespace_of(site.file);
        let visibility = self.defs.visibility(declaration);
        let visible = match visibility {
            Visibility::Pub => true,
            Visibility::Private => owner == here,
            Visibility::Package => owner == here || self.defs.same_package(owner, here),
        };
        if !visible && self.reported_type_visibility_violations.insert(declaration) {
            let declaration_span = self.defs.site(declaration).map_or_else(
                || span.clone(),
                crate::def_table::DeclarationOccurrence::span,
            );
            self.errors.push(super::TypeError::visibility_violation(
                visibility,
                span.clone(),
                self.defs.name(declaration).as_str(),
                self.defs.module_path(owner),
                self.current_module.as_deref().unwrap_or("(root)"),
                declaration_span,
                self.current_module.clone(),
            ));
        }
        visible
    }

    /// Report a written type path `Scope` resolves to no type, and the
    /// placeholder type the checker continues with.
    pub(super) fn report_unresolved_named_type(
        &mut self,
        path: &hew_parser::ast::Path,
        args: Vec<crate::Ty>,
        span: &hew_parser::ast::Span,
    ) -> crate::Ty {
        let name = path.to_string();
        if let Some(replacement) = self.retired_machine_event_path(path) {
            let key = super::types::SpanKey::in_module(span, self.current_module_idx);
            if self
                .reported_undefined_named_types
                .insert((name.clone(), key))
            {
                self.report_error_with_suggestions(
                    super::TypeErrorKind::UndefinedType,
                    span,
                    format!("machine event type `{name}` is now `{replacement}`"),
                    vec![format!("replace `{name}` with `{replacement}`")],
                );
            }
            return crate::Ty::Error;
        }
        let site = self.scope_site();
        let head = path.segments.first().map(|(head, _)| head.name);
        if let (Some(site), Some(head), 1) = (site, head, path.segments.len()) {
            let candidates = self.scopes.ambiguous_import(site.file, head).to_vec();
            if candidates.len() > 1 {
                let mut owners: Vec<String> = candidates
                    .iter()
                    .filter_map(|binding| self.binding_path(*binding))
                    .collect();
                owners.sort();
                self.report_error_with_suggestions(
                    super::TypeErrorKind::AmbiguousType,
                    span,
                    format!(
                        "ambiguous type `{name}`: published bare by {} imported modules",
                        candidates.len()
                    ),
                    owners
                        .iter()
                        .map(|owner| format!("qualify the reference, e.g. `{owner}`"))
                        .collect(),
                );
                return crate::Ty::Error;
            }
        }
        // A catalog builtin answers to its qualified spelling with no import
        // (`channel.Sender`, `stream.Sink`) when no declaration in scope
        // claims the path.
        if path.segments.len() > 1 {
            if let Some(builtin) = crate::lookup_builtin_type(&name) {
                return crate::Ty::named_head(crate::TypeHead::Builtin(builtin), args);
            }
        }
        let unresolved = crate::Ty::Named {
            head: crate::TypeHead::Unresolved(Symbol::intern(&name)),
            args: args.clone(),
        };
        // TRANSITION(A1c4): WHY stdlib registration collects member
        // signatures before importer scopes exist, a registry-loaded std
        // module does not load its own imports, and `Self.X` projections are
        // matched by spelling. WHEN registration resolves after every import
        // is bound and projections resolve through `Scope`, these report like
        // any other path. WHAT: one resolution pass over bound scopes.
        let std_source = self
            .current_module
            .as_deref()
            .is_some_and(|module| self.checking_canonical_stdlib_source(module));
        let self_headed = head == Some(hew_parser::ast::sym::SELF_TYPE);
        if self.in_stdlib_registration || std_source || self_headed {
            return unresolved;
        }
        let exporters: Vec<String> = match (site, head) {
            (Some(site), Some(head)) => self
                .scopes
                .modules_exporting(site.file, head)
                .into_iter()
                .map(|module| self.defs.module_path(module).to_string())
                .collect(),
            _ => Vec::new(),
        };
        let leaf = path.segments.last().map(|(leaf, _)| leaf.name);
        let lifecycle =
            leaf.and_then(|leaf| crate::lookup_source_owned_lifecycle_type(leaf.as_str()));
        let hinted = lifecycle.is_some() || (path.segments.len() == 1 && !exporters.is_empty());
        // TRANSITION(A1c4): WHY registration resolves signatures before
        // every import is bound. WHEN registration resolves in declaration
        // order through `Scope`, every failure reports here and
        // `TypeHead::Unresolved` goes. WHAT: one resolution pass after
        // minting and import binding.
        if !hinted && (!self.type_decls_registered || self.suppress_undefined_type_report) {
            return unresolved;
        }
        let key = super::types::SpanKey::in_module(span, self.current_module_idx);
        if !self
            .reported_undefined_named_types
            .insert((name.clone(), key))
        {
            return crate::Ty::Error;
        }
        self.report_unknown_type(&name, span, lifecycle, path.segments.len() > 1, &exporters);
        crate::Ty::Error
    }

    /// Report an unknown type, with the import that would bring it in scope
    /// when one is known.
    fn report_unknown_type(
        &mut self,
        name: &str,
        span: &hew_parser::ast::Span,
        lifecycle: Option<crate::BuiltinType>,
        qualified: bool,
        exporters: &[String],
    ) {
        if lifecycle.is_some() && qualified {
            self.report_error_with_suggestions(
                super::TypeErrorKind::UndefinedType,
                span,
                format!("unknown type `{name}`"),
                vec![format!("import the owning module before using `{name}`")],
            );
            return;
        }
        if let Some(lifecycle) = lifecycle {
            let module = if matches!(
                lifecycle,
                crate::BuiltinType::CrashNotification | crate::BuiltinType::CrashKind
            ) {
                "failure"
            } else {
                "link_monitor"
            };
            self.report_error_with_suggestions(
                super::TypeErrorKind::UndefinedType,
                span,
                format!("unknown type `{name}`"),
                vec![format!(
                    "import the lifecycle type explicitly, e.g. `import std.{module}.{{ {name} }}`"
                )],
            );
            return;
        }
        if exporters.is_empty() {
            self.report_error(
                super::TypeErrorKind::UndefinedType,
                span,
                format!("unknown type `{name}`"),
            );
            return;
        }
        let mut suggestions = Vec::new();
        for owner in exporters {
            suggestions.push(format!("qualify the reference, e.g. `{owner}.{name}`"));
            suggestions.push(format!(
                "or opt in to the bare name: `import {owner}.{{ {name} }}`"
            ));
        }
        let detail = if exporters.len() == 1 {
            format!("module `{}`", exporters[0])
        } else {
            format!("modules {}", exporters.join(", "))
        };
        self.report_error_with_suggestions(
            super::TypeErrorKind::UndefinedType,
            span,
            format!(
                "type `{name}` is not in scope; it is exported by {detail} \
                 but a plain `import` does not publish it unqualified"
            ),
            suggestions,
        );
    }

    /// The declaration path a binding names, for diagnostics.
    pub(super) fn binding_path(&self, binding: super::scope::Binding) -> Option<String> {
        use super::scope::Binding;
        match binding {
            Binding::Type(id) | Binding::Actor(id) => {
                Some(self.defs.path(id.declaration()).to_string())
            }
            Binding::Trait(id) | Binding::Fn(id) | Binding::Const(id) | Binding::Predicate(id) => {
                Some(self.defs.path(id).to_string())
            }
            Binding::Module(module) => Some(self.defs.module_path(module).to_string()),
            Binding::Builtin(_) => None,
        }
    }

    /// The `Machine.Event` spelling a retired flat `MachineEvent` type path
    /// names, when the machine it stems from is in scope.
    fn retired_machine_event_path(&mut self, path: &hew_parser::ast::Path) -> Option<String> {
        let ((leaf, leaf_span), qualifier) = path.segments.split_last()?;
        let machine = leaf
            .name
            .as_str()
            .strip_suffix("Event")
            .filter(|machine| !machine.is_empty())?;
        let mut machine_path = qualifier.to_vec();
        machine_path.push((hew_parser::ast::Ident::new(machine), leaf_span.clone()));
        let site = self.scope_site()?;
        let Ok(super::scope::Resolution::Nominal(owner)) = self.scopes.resolve(
            &self.env,
            site,
            super::scope::Namespace::Type,
            &machine_path,
        ) else {
            return None;
        };
        let owner = owner.declaration();
        if self.defs.kind(owner) != crate::DeclarationKind::Machine {
            return None;
        }
        self.defs.member_of_kind(
            owner,
            hew_parser::ast::sym::EVENT,
            crate::DeclarationKind::MachineEventType,
        )?;
        let qualifier = qualifier
            .iter()
            .map(|(segment, _)| segment.name.as_str())
            .collect::<Vec<_>>();
        Some(if qualifier.is_empty() {
            format!("{machine}.Event")
        } else {
            format!("{}.{machine}.Event", qualifier.join("."))
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
            Ok(super::scope::Resolution::Nominal(id)) => Some(self.head_of_declaration(id)),
            // A trait written in type position names the trait's declaration.
            Ok(super::scope::Resolution::Def(id))
                if self.defs.kind(id) == crate::DeclarationKind::Trait =>
            {
                Some(crate::TypeHead::Nominal(crate::NominalHead::new(
                    crate::NominalId::from_minted_declaration(id),
                    self.defs.path(id),
                )))
            }
            Ok(super::scope::Resolution::Param(id)) => Some(crate::TypeHead::param(
                crate::ParamHead::new(id, path.segments[0].0.name),
            )),
            Ok(super::scope::Resolution::Builtin(builtin)) => {
                Some(crate::TypeHead::Builtin(builtin))
            }
            _ => None,
        }
    }
}
