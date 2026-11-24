//! Checker methods grouped by responsibility: type publication.
//! Split from `registration.rs`: checker methods, part 1 of 6.
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
use hew_parser::ast::Ident;
use hew_parser::ast::WireMetadata;

impl Checker {
    pub(super) fn type_contains_owned_handle(
        &self,
        id: crate::NominalId,
        explored: &mut HashSet<crate::NominalId>,
    ) -> bool {
        // This is one root reachability query. Retain declarations across
        // siblings so a shared member graph is expanded once, including cycles.
        if !explored.insert(id) {
            return false;
        }
        self.type_defs.get(&id).is_some_and(|definition| {
            definition.kind == TypeDefKind::Struct
                && definition
                    .fields
                    .values()
                    .any(|ty| self.ty_contains_owned_handle(ty, explored))
        })
    }

    pub(super) fn ty_contains_owned_handle(
        &self,
        ty: &Ty,
        explored: &mut HashSet<crate::NominalId>,
    ) -> bool {
        match ty {
            Ty::Generator { .. } => true,
            Ty::Tuple(items) => items
                .iter()
                .any(|item_ty| self.ty_contains_owned_handle(item_ty, explored)),
            Ty::Array(element_ty, _) | Ty::Slice(element_ty) => {
                self.ty_contains_owned_handle(element_ty, explored)
            }
            Ty::Named { head, args } => {
                self.registry.is_owned_handle(*head)
                    || args
                        .iter()
                        .any(|arg_ty| self.ty_contains_owned_handle(arg_ty, explored))
                    || head.nominal().is_some_and(|id| self.type_contains_owned_handle(id, explored))
            }
            Ty::I8
            | Ty::I16
            | Ty::I32
            | Ty::I64
            | Ty::U8
            | Ty::U16
            | Ty::U32
            | Ty::U64
            | Ty::Isize
            | Ty::Usize
            | Ty::F32
            | Ty::F64
            | Ty::IntLiteral
            | Ty::FloatLiteral
            | Ty::Bool
            | Ty::Char
            | Ty::String
            | Ty::Bytes
            | Ty::CancellationToken
            | Ty::Duration
            | Ty::Unit
            | Ty::Never
            | Ty::Var(_)
            | Ty::GenericCallable { .. }
            | Ty::Function { .. }
            | Ty::Closure { .. }
            | Ty::Pointer { .. }
            // `&T` borrow is non-owning: a borrow never holds an owned handle
            // (the owner is borrowed from, elsewhere). Mirrors the Pointer arm.
            | Ty::Borrow { .. }
            | Ty::TraitObject { .. }
            | Ty::Error
            // Task<T> is compiler-internal; it does not appear in user-declared
            // struct field types (there is no surface annotation for Task<T>),
            // so this arm is structurally unreachable today. Explicit rather
            // than wildcard so the sweep stays honest.
            | Ty::Task(_)
            // Ty::AssocType is a projection carrier present only in generic
            // signatures during checking; field-type validation walks
            // user-declared struct/record/enum fields, which cannot themselves
            // be associated-type projections (no `field: T::Item` surface).
            // If a future surface admits projections in field types, this arm
            // must descend into `base`.
            | Ty::AssocType { .. } => false,
        }
    }

    /// Returns `true` when an item's top-level declaration still needs minting
    /// into `namespace` (for example after an embedded stdlib partial seed).
    pub(super) fn item_needs_mint_in_scope(&self, namespace: crate::ModuleId, item: &Item) -> bool {
        let name = match item {
            Item::Const(d) => Some(d.name),
            Item::TypeDecl(d) => Some(d.name),
            Item::TypeAlias(d) => Some(d.name),
            Item::Trait(d) => Some(d.name),
            Item::Function(d) => Some(d.name),
            Item::Actor(d) => Some(d.name),
            Item::Supervisor(d) => Some(d.name),
            Item::Machine(d) => Some(d.name),
            Item::Record(d) => Some(d.name),
            Item::Impl(_) => return true,
            Item::Import(_) | Item::ExternBlock(_) | Item::Macro(_) | Item::MacroCall(_) => {
                return false
            }
        };
        name.is_some_and(|ident| self.scopes.item(namespace, ident).is_none())
    }

    #[expect(
        clippy::too_many_lines,
        reason = "registers compiled stdlib receiver impls and pre-seeded types"
    )]
    pub(super) fn register_compiled_stdlib_receiver_impls(
        &mut self,
        module_short: &str,
        source: &str,
        receiver_names: &[&str],
        extra_type_names: &[&str],
    ) {
        let parsed = hew_parser::parse(source);
        debug_assert!(
            parsed.errors.is_empty(),
            "std/{module_short}.hew failed to parse: {:?}",
            parsed.errors
        );
        if !parsed.errors.is_empty() {
            return;
        }
        let module_full_path = format!("std.{module_short}");
        let identity_module = self.defs.mint_module(&module_full_path, &[]);
        if self.defs.module_source(identity_module).is_none() {
            self.defs.set_module_package(
                identity_module,
                crate::def_table::PackageOrigin::StandardLibrary,
            );
        }
        if !extra_type_names.is_empty() {
            let fallback_file = self
                .defs
                .module_for_path(&module_full_path)
                .and_then(|module| self.defs.module_source(module))
                .and_then(|src| self.file_span_indices.path_index(src))
                .unwrap_or_else(|| self.allocate_embedded_file_index(&module_full_path));
            let saved_idx = std::mem::replace(&mut self.current_module_idx, fallback_file);
            for (item_ordinal, (item, span)) in parsed.program.items.iter().enumerate() {
                let include = matches!(
                    item,
                    Item::TypeDecl(td) if extra_type_names.contains(&td.name.name.as_str())
                );
                if include && self.item_needs_mint_in_scope(identity_module, item) {
                    self.mint_item_declaration_identities(
                        Some(identity_module),
                        Some(identity_module),
                        crate::check::NominalNamespace::Owned,
                        item_ordinal,
                        item,
                        span,
                    );
                }
            }
            self.declare_minted_items_in_scope(&[identity_module], identity_module);
            let saved_module = self.current_module.replace(module_full_path.clone());
            let saved_origin = self
                .registration_origin_module
                .replace(module_full_path.clone());
            let saved_local = std::mem::take(&mut self.local_type_defs);
            let saved_source = std::mem::take(&mut self.source_type_defs);
            for name in extra_type_names {
                self.local_type_defs.insert((*name).to_string());
                self.source_type_defs.insert((*name).to_string());
            }
            for (item, _) in &parsed.program.items {
                if let Item::TypeDecl(td) = item {
                    if extra_type_names.contains(&td.name.name.as_str()) {
                        self.pre_register_type_decl(td);
                        let canonical = format!("{module_full_path}.{}", td.name);
                        if let Some(source_def) = self.type_def_at(&canonical).cloned() {
                            self.register_canonical_type_def(
                                &module_full_path,
                                td.name.name.as_str(),
                                &source_def,
                            );
                        }
                    }
                }
            }
            self.local_type_defs = saved_local;
            self.source_type_defs = saved_source;
            self.registration_origin_module = saved_origin;
            self.current_module = saved_module;
            self.current_module_idx = saved_idx;
        }
        let impl_items: Vec<(usize, Spanned<Item>)> = parsed
            .program
            .items
            .into_iter()
            .enumerate()
            .filter(|(_, (item, _))| {
                let Item::Impl(id) = item else {
                    return false;
                };
                let TypeExpr::Named { path, .. } = &id.target_type.0 else {
                    return false;
                };
                path.as_single().is_some_and(|ident| {
                    receiver_names
                        .iter()
                        .chain(extra_type_names.iter())
                        .any(|receiver| ident.name.as_str() == *receiver)
                })
            })
            .collect();
        if !impl_items.is_empty() {
            self.register_stdlib_hew_item_bodies_with_ordinals(
                module_short,
                &module_full_path,
                &impl_items,
                StdlibBarePublication::Prelude,
            );
        }
    }

    /// Register a builtin collection constructor: a nullary generic function
    /// returning the canonical collection over its own type parameters, with
    /// the constructor's runtime family as its executable call target.
    pub(super) fn register_collection_constructor(
        &mut self,
        name: &str,
        builtin: BuiltinType,
        type_params: &[&str],
        family: crate::runtime_call::RuntimeCallFamily,
    ) {
        self.register_builtin_fn_with_bounds(
            name,
            type_params
                .iter()
                .map(|param| (*param).to_string())
                .collect(),
            None,
            |type_params| {
                (
                    vec![],
                    Ty::Named {
                        head: crate::TypeHead::Builtin(builtin),
                        args: type_params.iter().copied().map(Ty::param).collect(),
                    },
                )
            },
        );
        self.builtin_call_targets
            .insert(name.to_string(), CallTarget::Runtime(family));
    }

    pub(in crate::check) fn resolve_registered_annotation_ty(
        &mut self,
        type_expr: &Spanned<TypeExpr>,
        hole_vars: &mut Vec<TypeVar>,
    ) -> Ty {
        self.resolve_registered_annotation_ty_with_context(
            type_expr,
            hole_vars,
            TypeResolutionContext::Ordinary,
        )
    }

    pub(super) fn resolve_registered_annotation_ty_with_context(
        &mut self,
        type_expr: &Spanned<TypeExpr>,
        hole_vars: &mut Vec<TypeVar>,
        context: TypeResolutionContext,
    ) -> Ty {
        let ty = self.resolve_type_expr_tracking_holes_with_context(type_expr, hole_vars, context);
        self.validate_concrete_collection_types(&ty, &type_expr.1);
        ty
    }

    pub(super) fn resolve_registered_annotation_ty_no_holes(
        &mut self,
        type_expr: &Spanned<TypeExpr>,
    ) -> Ty {
        let mut hole_vars = Vec::new();
        self.resolve_registered_annotation_ty(type_expr, &mut hole_vars)
    }

    /// Establish compiler-floor authority from the module graph's resolved
    /// source, before that module publishes signatures or imports. A lexical
    /// `std.*` path is user-controlled; only the exact shipped path may enable
    /// compiler intrinsic metadata.
    pub(super) fn record_canonical_std_module_source(
        &mut self,
        module_name: &str,
        source_paths: &[std::path::PathBuf],
    ) {
        if source_paths.iter().any(|source| {
            crate::module_registry::is_canonical_stdlib_module_source(
                source,
                module_name,
                self.module_registry.std_roots(),
            )
        }) {
            self.canonical_std_module_sources
                .insert(module_name.to_string());
        }
    }

    /// The isolated-checker prelude is compiled from the shipped lifecycle
    /// sources. Preserve its intentionally import-free bare spellings, and
    /// make that trust explicit: the text is compiled into the checker, so no
    /// path or std root can supply it.
    pub(super) fn record_canonical_lifecycle_prelude_authority(&mut self, module_name: &str) {
        let owner = match module_name {
            "std.failure" => "std.failure",
            "std.link_monitor" => "std.link_monitor",
            _ => return,
        };
        let lifecycle_names: &[&str] = match owner {
            "std.failure" => &[
                "CrashInfo",
                "CrashAction",
                "CrashNotification",
                "CrashKind",
                "DownTarget",
            ],
            "std.link_monitor" => &["MonitorId", "DownReason", "DownNotification", "MonitorRef"],
            _ => unreachable!("matched canonical lifecycle owner"),
        };
        for source_name in lifecycle_names {
            self.bind_prelude_item(owner, source_name);
            self.canonical_lifecycle_import_authority.insert((
                None,
                (*source_name).to_string(),
                format!("{owner}.{source_name}"),
            ));
        }
    }

    /// Report any recursive type aliases once alias targets are registered.
    pub(in crate::check) fn report_recursive_alias_declarations(&mut self) {
        for (declaration, alias) in self.type_aliases.clone() {
            if self.alias_expansion_is_recursive(declaration) {
                let name = self.defs.path(declaration).to_string();
                let span = self
                    .defs
                    .site(declaration)
                    .map(crate::def_table::DeclarationOccurrence::span)
                    .unwrap_or_default();
                let mut error = TypeError::new(TypeErrorKind::InvalidOperation, span,
                    format!("type alias `{name}` is recursive: aliases cannot refer to themselves, directly or through a chain"));
                error.source_module = alias.source_module;
                self.errors.push(error);
            }
        }
    }

    pub(super) fn validate_type_decl_wire_optional_fields(&mut self, item: &Item) {
        let Item::TypeDecl(type_decl) = item else {
            return;
        };
        let Some(wire) = &type_decl.wire else {
            return;
        };

        let type_def_key = self.authoritative_type_def_key(type_decl.name.name.as_str());
        let Some(type_def) = self.type_def_at(&type_def_key).cloned() else {
            return;
        };
        let fields = type_def.fields.clone();
        self.validate_wire_type_members(type_decl, &type_def);

        for metadata in wire.field_meta.iter().filter(|field| field.is_optional) {
            let field_span = type_decl
                .body
                .iter()
                .find_map(|item| match item {
                    TypeBodyItem::Field { name, ty, .. }
                        if name.name.as_str() == metadata.field_name =>
                    {
                        Some(ty.1.clone())
                    }
                    _ => None,
                })
                .unwrap_or(0..0);
            let resolved_ty = fields
                .get(&metadata.field_name)
                .map(|field_ty| self.normalize_for_use(field_ty));

            if resolved_ty.as_ref().and_then(Ty::as_option).is_none() {
                self.report_error(
                    TypeErrorKind::WireOptionalFieldRequiresOption,
                    &field_span,
                    format!(
                        "E_WIRE_OPTIONAL_REQUIRES_OPTION: wire field `{}` is marked `optional` but must have type `Option<T>`",
                        metadata.field_name
                    ),
                );
            }
        }
    }

    pub(in crate::check) fn build_impl_alias_entries(
        &mut self,
        id: &ImplDecl,
    ) -> HashMap<String, ImplAliasEntry> {
        let mut entries = HashMap::new();
        let mut seen_spans: HashMap<String, Span> = HashMap::new();
        for alias in &id.type_aliases {
            if let Some(prev_span) = seen_spans.insert(alias.name.to_string(), alias.ty.1.clone()) {
                self.errors.push(TypeError::duplicate_definition(
                    alias.ty.1.clone(),
                    alias.name.name.as_str(),
                    prev_span,
                ));
                continue;
            }
            entries.insert(
                alias.name.to_string(),
                ImplAliasEntry {
                    expr: alias.ty.clone(),
                    resolved: None,
                    resolving: false,
                },
            );
        }
        if let Some(trait_info) = id
            .trait_bound
            .as_ref()
            .and_then(|tb| self.resolve_trait_path(&tb.path))
            .and_then(|trait_id| self.trait_info(trait_id))
        {
            {
                for assoc in &trait_info.associated_types {
                    if entries.contains_key(&assoc.name) {
                        continue;
                    }
                    if let Some(default) = &assoc.default {
                        entries.insert(
                            Ident::new(&assoc.name).to_string(),
                            ImplAliasEntry {
                                expr: default.clone(),
                                resolved: None,
                                resolving: false,
                            },
                        );
                    }
                }
            }
        }
        entries
    }

    pub(in crate::check) fn report_unsupported_impl_drop(&mut self, span: &Span) {
        self.errors.push(TypeError::new(
            TypeErrorKind::InvalidOperation,
            span.clone(),
            "`impl Drop` is not supported (its `drop` method would not run); use \
             `#[resource]` with a `close()` method for deterministic cleanup, or \
             rely on automatic field-wise drop",
        ));
    }

    /// DECISION: a method type parameter that shadows a type parameter of its
    /// enclosing `impl` block or `trait` is REFUSED, rather than the two being
    /// distinguished by scope.
    ///
    /// The two are already conflated everywhere downstream, silently and
    /// wrongly. `instantiate_named_method_sig` (`method_resolution.rs`)
    /// substitutes the enclosing type arguments by NAME and then drops every
    /// matching name from `sig.type_params`, so the method's own parameter is
    /// erased and bound to the enclosing argument: given
    /// `impl<T> Holder<T> { fn same<T>(self, marker: T) }`, a `Holder<i64>`
    /// receiver makes `same("text")` report `expected i64, found string`, and
    /// `trait Choice<T> { fn same<T>(self, marker: T) }` reports `expected T`.
    /// The method's `T` never existed in either case.
    ///
    /// Keying substitutions by `(scope, name)` would not fix that: the collapse
    /// happens upstream of any substitution map, and `type_params` is a flat
    /// `Vec<String>` read by every consumer of `FnSig`, all of which would need
    /// the two-level key. Shadowing buys no expressiveness — the method
    /// parameter can always be renamed — so the fail-closed refusal is both the
    /// smaller change and the honest one.
    ///
    /// One authority for all three shapes: inherent `impl` methods, trait
    /// declaration methods (including default bodies), and trait `impl`
    /// methods shadowing a parameter the trait declared.
    /// Module-qualified identity of a declaration owner, for the shadow-report
    /// dedup key.
    pub(in crate::check) fn declaration_owner_key(&self, name: &str) -> String {
        scoped_module_item_name(self.current_module.as_deref(), name)
            .unwrap_or_else(|| name.to_string())
    }

    /// The canonical identity of an `impl` target spelled through a module
    /// binding, or `None` when the spelling is already an identity (a bare
    /// local name, a builtin, an exact owner-qualified path).
    ///
    /// Only a qualified spelling is resolved: a bare name is the declaring
    /// scope's own lexical spelling and the surrounding registration already
    /// owns its qualification.
    pub(in crate::check) fn canonical_impl_target_identity(
        &mut self,
        path: &hew_parser::ast::Path,
    ) -> Option<String> {
        self.resolve_type_path_head(path)
            .and_then(|head| head.declaration(&self.defs))
            .map(|owner| self.defs.path(owner.declaration()).to_string())
    }

    pub(super) fn stdlib_hew_source_identity(decl: &ImportDecl, module_path: &str) -> String {
        // Always prefer the canonical module-path key when available so that
        // multiple ImportDecl objects for the same stdlib module (e.g. `import
        // std::fs` appearing in quic.hew, tls.hew, and the user file) all hash
        // to the same identity string even when only some of them have a
        // resolved_source_paths populated.  Using the file path as the primary
        // key produces two different strings for the same logical module and
        // defeats the registered_stdlib_hew_sources dedup guard.
        if module_path.is_empty() {
            decl.resolved_source_paths.first().map_or_else(
                || String::from("module:"),
                |p| format!("path:{}", p.display()),
            )
        } else {
            format!("module:{module_path}")
        }
    }

    /// Register one resolved Hew stdlib source while preserving two distinct
    /// scopes of authority:
    ///
    /// * declarations and qualified exports are global and therefore deduped;
    /// * bare import bindings belong to each importer and must be republished
    ///   every time that importer reaches the already-registered module.
    ///
    /// A transitive import can encounter `std::net` before the root's named
    /// import. Treating the global declaration-dedup bit as a reason to skip
    /// the second import silently loses the root's `Connection` binding and its
    /// HIR source identity.
    pub(in crate::check) fn register_resolved_stdlib_hew_source(
        &mut self,
        decl: &ImportDecl,
        module_path: &str,
        module_short: &str,
        module_full_path: &str,
        items: &[Spanned<Item>],
        publication: StdlibBarePublication<'_>,
    ) {
        if self.stdlib_hew_source_already_registered(decl, module_path) {
            self.publish_imported_hew_bindings(module_short, module_full_path, items, publication);
        } else {
            self.register_stdlib_hew_items(module_short, module_full_path, items, publication);
        }
    }

    /// Register type declarations, trait declarations, and impl blocks from
    /// stdlib modules that have Hew source files. This makes trait methods
    /// (e.g. bench.Bencher.iter) visible to the type checker.
    pub(in crate::check) fn register_stdlib_hew_items(
        &mut self,
        module_short: &str,
        module_full_path: &str,
        items: &[Spanned<Item>],
        import_spec: StdlibBarePublication<'_>,
    ) {
        let ordinal_items: Vec<(usize, Spanned<Item>)> =
            items.iter().cloned().enumerate().collect();
        self.register_stdlib_hew_item_bodies_with_ordinals(
            module_short,
            module_full_path,
            &ordinal_items,
            import_spec,
        );
    }

    #[expect(
        clippy::too_many_lines,
        reason = "multi-pass registration loop with local_type_defs scoping"
    )]
    fn register_stdlib_hew_item_bodies_with_ordinals(
        &mut self,
        module_short: &str,
        module_full_path: &str,
        items: &[(usize, Spanned<Item>)],
        import_spec: StdlibBarePublication<'_>,
    ) {
        // Compiler-embedded and registry-loaded Hew source enters outside the
        // program module graph. Establish its exact declarations in the same
        // table before any semantic registration; aliases and later graph
        // visits resolve the existing module/path rows and cannot mint again.
        let identity_module = self.defs.mint_module(module_full_path, &[]);
        let item_sources = self
            .module_item_sources
            .get(module_full_path)
            .cloned()
            .unwrap_or_default();
        let importer_source = self.current_item_source.clone();
        let importer_file = self.current_module_idx;
        let fallback_file = self
            .defs
            .module_for_path(module_full_path)
            .and_then(|module| self.defs.module_source(module))
            .and_then(|source| self.file_span_indices.path_index(source))
            .unwrap_or_else(|| self.allocate_embedded_file_index(module_full_path));
        self.current_module_idx = fallback_file;
        // Only the canonical std source below the run's std roots may
        // complete a partial registration with its types: a user-backed
        // `std.x` lookalike never gains declarations this way.
        let canonical_std = self
            .module_registry
            .get(module_full_path)
            .and_then(|info| info.source_path.as_deref())
            .is_some_and(|source| {
                crate::module_registry::is_canonical_stdlib_module_source(
                    source,
                    module_full_path,
                    self.module_registry.std_roots(),
                )
            });
        if self.defs.module_has_source_declarations(identity_module) {
            let mut files: Vec<crate::ModuleId> = item_sources
                .iter()
                .filter_map(|source| self.defs.module_for_source(source))
                .collect();
            files.push(identity_module);
            self.declare_minted_items_in_scope(&files, identity_module);
            for &(item_ordinal, (ref item, ref span)) in items {
                let file = item_sources
                    .get(item_ordinal)
                    .and_then(|source| self.defs.module_for_source(source))
                    .unwrap_or(identity_module);
                // A graph or pre-seeded builtin may inventory only part of a
                // module. Mint any unminted item and any impl blocks once.
                if matches!(item, Item::Impl(_))
                    || (canonical_std && self.item_needs_mint_in_scope(identity_module, item))
                {
                    self.mint_item_declaration_identities(
                        Some(file),
                        Some(identity_module),
                        crate::check::NominalNamespace::Owned,
                        item_ordinal,
                        item,
                        span,
                    );
                }
                if let Item::Import(decl) = item {
                    self.bind_import_in_scope(file, decl, span);
                }
                self.declare_item_type_parameter_scopes(
                    item_sources
                        .get(item_ordinal)
                        .and_then(|source| self.defs.module_for_source(source))
                        .or(Some(identity_module)),
                    item_ordinal,
                    item,
                    span,
                );
            }
        } else {
            for &(item_ordinal, (ref item, ref span)) in items {
                self.mint_item_declaration_identities(
                    Some(identity_module),
                    Some(identity_module),
                    crate::check::NominalNamespace::Owned,
                    item_ordinal,
                    item,
                    span,
                );
            }
        }
        let saved_registration_origin = self
            .registration_origin_module
            .replace(module_full_path.to_string());
        for &(index, (ref item, ref span)) in items {
            self.current_item_source = item_sources.get(index).cloned();
            self.current_module_idx = self
                .current_item_source
                .as_ref()
                .and_then(|source| self.file_span_indices.path_index(source))
                .unwrap_or(fallback_file);
            let Item::Import(decl) = item else {
                continue;
            };
            if decl.resolved_items.is_some()
                || (!decl.path.segments.is_empty() && self.module_registry.has_search_paths())
            {
                // Load the imported stdlib module so its re-exported traits become
                // visible to the eager trait-use path. Pass `None` for the import
                // span deliberately: this import statement lives in a stdlib source
                // file, so its span indexes that file — not the user document the
                // diagnostics are reported against. Reporting it as an import
                // would make it a user-facing unused-import lint candidate whose
                // span cannot be resolved to any user source, mis-attributing a
                // stdlib-internal offset to the user's document.
                let saved_current_module = self.current_module.clone();
                self.current_module = Some(module_full_path.to_string());
                self.register_import(decl, None);
                self.current_module = saved_current_module;
            } else {
                let file = item_sources
                    .get(index)
                    .and_then(|source| self.defs.module_for_source(source))
                    .unwrap_or(identity_module);
                self.bind_import_in_scope(file, decl, span);
            }
        }

        // Resolve imported declarations in the defining module's lexical scope.
        let saved_local_type_defs = std::mem::take(&mut self.local_type_defs);
        let saved_source_type_defs = std::mem::take(&mut self.source_type_defs);
        for (_, (item, _)) in items {
            // The program-wide type-parameter harvest in `collect_types` walks
            // `program.module_graph`, which is empty on the registry-only
            // checker path (LSP, browser compiler, inline checks). Without the
            // harvest a declaration's own parameters are unknown while its
            // members resolve, so `pub type ScopeError<E> { primary: E; }`
            // reports `unknown type E` against the module's own source.
            self.collect_item_type_param_names(item);
            match item {
                Item::TypeDecl(td) => {
                    self.local_type_defs.insert(td.name.to_string());
                    self.source_type_defs.insert(td.name.to_string());
                }
                Item::Actor(ad) => {
                    self.local_type_defs.insert(ad.name.to_string());
                    self.source_type_defs.insert(ad.name.to_string());
                }
                Item::TypeAlias(ta) => {
                    self.local_type_defs.insert(ta.name.to_string());
                    self.source_type_defs.insert(ta.name.to_string());
                }
                Item::Record(rd) => {
                    self.local_type_defs.insert(rd.name.to_string());
                    self.source_type_defs.insert(rd.name.to_string());
                }
                Item::Machine(md) => {
                    self.local_type_defs.insert(md.name.to_string());
                    self.source_type_defs.insert(md.name.to_string());
                }
                _ => {}
            }
        }

        // Pass 1a-prep: Insert trait shells and type aliases before any trait
        // bounds, type bodies, or signatures resolve.
        for &(index, (ref item, ref span)) in items {
            self.current_item_source = item_sources.get(index).cloned();
            self.current_module_idx = self
                .current_item_source
                .as_ref()
                .and_then(|source| self.file_span_indices.path_index(source))
                .unwrap_or(fallback_file);
            if let Item::Trait(tr) = item {
                let qualified_type = format!("{module_short}.{}", tr.name);
                self.type_visibility
                    .entry(qualified_type.clone())
                    .or_insert((tr.visibility, Some(module_full_path.to_string())));
                self.type_def_spans
                    .entry(qualified_type)
                    .or_insert_with(|| span.clone());
                if !tr.visibility.is_pub() {
                    continue;
                }
                if !self.register_type_namespace_name(Some(module_full_path), tr.name, span) {
                    continue;
                }
                let info = self.trait_shell_from_decl(
                    tr,
                    Some(module_full_path.to_string()),
                    self.current_module_idx,
                );
                let qualified = format!("{module_full_path}.{}", tr.name);
                self.insert_trait_def(&qualified, info);
            }
        }
        for &(index, (ref item, ref span)) in items {
            self.current_item_source = item_sources.get(index).cloned();
            self.current_module_idx = self
                .current_item_source
                .as_ref()
                .and_then(|source| self.file_span_indices.path_index(source))
                .unwrap_or(fallback_file);
            if let Item::TypeAlias(decl) = item {
                let saved_importer_module =
                    self.current_module.replace(module_full_path.to_string());
                self.register_type_alias_decl(decl, span);
                self.current_module = saved_importer_module;
                if decl.visibility.is_pub() {
                    self.record_module_type_export(module_full_path, decl.name.name.as_str());
                }
            }
        }

        // Pass 1a: Complete traits (supers + methods), types, machines, and actors.
        for &(index, (ref item, ref span)) in items {
            self.current_item_source = item_sources.get(index).cloned();
            self.current_module_idx = self
                .current_item_source
                .as_ref()
                .and_then(|source| self.file_span_indices.path_index(source))
                .unwrap_or(fallback_file);
            match item {
                Item::TypeDecl(td) => {
                    // Record visibility for all TypeDecls (both pub and non-pub)
                    // so the enforcement check can distinguish "private" from "unknown".
                    let qualified_type = format!("{module_full_path}.{}", td.name);
                    let surface_qualified_type = format!("{module_short}.{}", td.name);
                    self.type_visibility
                        .entry(qualified_type.clone())
                        .or_insert((td.visibility, Some(module_full_path.to_string())));
                    self.type_visibility
                        .entry(surface_qualified_type)
                        .or_insert((td.visibility, Some(module_full_path.to_string())));
                    // Record the declaration span so E_VISIBILITY can point "declared
                    // here" at the actual declaration for both pub and non-pub types.
                    self.type_def_spans
                        .entry(qualified_type.clone())
                        .or_insert_with(|| span.clone());
                    if !td.visibility.is_pub() {
                        // Private declarations are not importable, but source-local
                        // member annotations still need their exact declaration
                        // identity. The module-graph collection pass has already
                        // registered the bare definition; mirror it under the full
                        // owner before a public sibling resolves a bare reference to
                        // this private type (e.g. `std.net.tls.Holder` containing
                        // `Wrap`).
                        let canonical = format!("{module_full_path}.{}", td.name);
                        let source_def = self.type_def_at(&canonical).cloned().or_else(|| {
                            // Direct resolved-item imports have no module-graph
                            // pre-registration. Resolve this declaration in its
                            // own scope and capture its just-written bare def.
                            let saved_importer_module =
                                self.current_module.replace(module_full_path.to_string());
                            self.in_stdlib_registration = true;
                            self.register_type_decl(td);
                            self.in_stdlib_registration = false;
                            let source_def = self.type_def_at(td.name.name.as_str()).cloned();
                            self.current_module = saved_importer_module;
                            source_def
                        });
                        if let Some(source_def) = source_def.as_ref() {
                            self.register_canonical_type_def(
                                module_full_path,
                                td.name.name.as_str(),
                                source_def,
                            );
                        }
                        continue;
                    }
                    if !self.register_type_namespace_name(Some(module_full_path), td.name, span) {
                        continue;
                    }
                    let saved_importer_module =
                        self.current_module.replace(module_full_path.to_string());
                    self.in_stdlib_registration = true;
                    self.register_type_decl(td);
                    self.in_stdlib_registration = false;
                    let source_def = self.type_def_at(td.name.name.as_str()).cloned();
                    self.current_module = saved_importer_module;
                    self.known_types.insert(td.name.to_string());
                    // Qualified authority is always published, mirroring the
                    // user-module path: the qualified alias and the module-export
                    // record that drives the use-time gate's "exported by module
                    // X" diagnostic and ambiguity candidate naming.
                    self.register_qualified_type_alias(module_short, td.name.name.as_str());
                    if let Some(source_def) = source_def.as_ref() {
                        self.register_canonical_type_def(
                            module_full_path,
                            td.name.name.as_str(),
                            source_def,
                        );
                    }
                    self.record_module_type_export(module_short, td.name.name.as_str());
                    self.record_module_type_export(module_full_path, td.name.name.as_str());
                }
                Item::Machine(md) => {
                    // Record visibility for all Machines (both pub and non-pub).
                    let qualified_type = format!("{module_short}.{}", md.name);
                    self.type_visibility
                        .entry(qualified_type.clone())
                        .or_insert((md.visibility, Some(module_full_path.to_string())));
                    // Record the declaration span for non-pub machines.
                    self.type_def_spans
                        .entry(qualified_type.clone())
                        .or_insert_with(|| span.clone());
                    if !md.visibility.is_pub() {
                        continue;
                    }
                    if !self.register_machine_type_namespace_names(
                        Some(module_full_path),
                        md.name.name.as_str(),
                        span,
                    ) {
                        continue;
                    }
                    let event_name = format!("{}.Event", md.name);
                    // Resolved stdlib items are registered while the importer
                    // is the active checker frame. Re-enter the declaration's
                    // assembled module before building the machine and its
                    // generated event/method signatures, matching the module-
                    // graph pre-registration and ordinary source-body context.
                    let saved_importer_module =
                        self.current_module.replace(module_full_path.to_string());
                    self.register_machine_decl(md, span);
                    let machine_def = self.type_def_at(md.name.name.as_str()).cloned();
                    let event_def = self.type_def_at(&event_name).cloned();
                    self.current_module = saved_importer_module;
                    self.known_types.insert(md.name.to_string());
                    self.known_types.insert(event_name.clone());
                    self.register_qualified_type_alias(module_short, md.name.name.as_str());
                    self.register_qualified_type_alias(module_short, &event_name);
                    if let Some(machine_def) = machine_def.as_ref() {
                        self.register_canonical_type_def(
                            module_full_path,
                            md.name.name.as_str(),
                            machine_def,
                        );
                    }
                    if let Some(event_def) = event_def.as_ref() {
                        self.register_canonical_type_def(module_full_path, &event_name, event_def);
                    }
                    self.record_module_type_export(module_short, md.name.name.as_str());
                    self.record_module_type_export(module_short, &event_name);
                    self.record_module_type_export(module_full_path, md.name.name.as_str());
                    self.record_module_type_export(module_full_path, &event_name);
                }
                Item::Trait(tr) => {
                    if !tr.visibility.is_pub() {
                        continue;
                    }
                    let qualified = format!("{module_full_path}.{}", tr.name);
                    if let Some(trait_id) = self.lookup_declaration(&qualified) {
                        let saved_importer_module =
                            self.current_module.replace(module_full_path.to_string());
                        self.register_trait_supers(trait_id, tr);
                        let trait_scope =
                            self.enter_primary_sig_scope(&[(tr.type_params.as_ref(), None)]);
                        for trait_item in &tr.items {
                            if let TraitItem::Method(method) = trait_item {
                                self.register_trait_method_sig(trait_id, method, span);
                            }
                        }
                        self.exit_primary_sig_scope(trait_scope);
                        self.current_module = saved_importer_module;
                    }
                }
                Item::Actor(ad) => {
                    // Record visibility for all actors (both pub and non-pub) so a
                    // cross-module qualified reference to a non-pub actor produces a
                    // precise E_VISIBILITY at the reference site instead of leaking an
                    // `E_MIR: unknown type` at the MIR boundary. Mirrors the TypeDecl,
                    // Machine, and Trait registration above; actors share the
                    // type-namespace and the same qualified-reference enforcement path.
                    let qualified_type = format!("{module_short}.{}", ad.name);
                    self.type_visibility
                        .entry(qualified_type.clone())
                        .or_insert((ad.visibility, Some(module_full_path.to_string())));
                    self.type_def_spans
                        .entry(qualified_type)
                        .or_insert_with(|| span.clone());
                    if !self.register_type_namespace_name(Some(module_short), ad.name, span) {
                        continue;
                    }
                    // The actor's signatures resolve in its declaring module.
                    let saved_importer_module =
                        self.current_module.replace(module_full_path.to_string());
                    self.register_actor_base(ad, Some(module_full_path));
                    self.current_module = saved_importer_module;
                }
                _ => {}
            }
        }
        // Pass 1b: Register function and constant signatures (after types and traits exist).
        for &(index, (ref item, ref span)) in items {
            self.current_item_source = item_sources.get(index).cloned();
            self.current_module_idx = self
                .current_item_source
                .as_ref()
                .and_then(|source| self.file_span_indices.path_index(source))
                .unwrap_or(fallback_file);
            match item {
                Item::Function(fd) => {
                    let qualified =
                        self.canonical_fn_identity(Some(module_full_path), fd.name.name.as_str());
                    // The shipped builtins file also documents compiler-owned
                    // callables. Importing those declarations must retain the
                    // endpoint registered by the compiler, not turn their
                    // documentation bodies into executable implementations.
                    if canonical_std && module_full_path == "std.builtins" {
                        if let Some(target) = self
                            .builtin_call_targets
                            .get(fd.name.name.as_str())
                            .cloned()
                        {
                            self.builtin_call_targets.insert(qualified.clone(), target);
                        }
                    }
                    let surface_qualified = format!("{module_short}.{}", fd.name);
                    // Record visibility for all functions in the visibility table.
                    self.fn_visibility
                        .entry(qualified.clone())
                        .or_insert(fd.visibility);
                    self.fn_visibility
                        .entry(surface_qualified.clone())
                        .or_insert(fd.visibility);
                    self.fn_def_spans
                        .entry(qualified.clone())
                        .or_insert_with(|| (span.clone(), Some(module_full_path.to_string())));
                    // The parsed Hew declaration is the canonical signature
                    // authority.  A registry import may have installed an ABI
                    // wrapper under this same exact key first; that wrapper's
                    // embedded types use the legacy surface owner (`net.X`) and
                    // must not survive as the source declaration's type fact.
                    // Rebuild the complete signature in the source owner's
                    // scope on every first-source registration, independent of
                    // whether a registry slot already exists.
                    let saved_importer_module =
                        self.current_module.replace(module_full_path.to_string());
                    let sig = self.build_fn_sig_from_decl(fd);
                    self.current_module = saved_importer_module;
                    if fd.visibility == hew_parser::ast::Visibility::Pub {
                        self.module_fn_exports.insert(qualified.clone());
                    }
                    self.insert_fn_sig_at(&qualified, sig);
                    if let Some(intrinsic_key) = &fd.intrinsic {
                        let saved_importer_module =
                            self.current_module.replace(module_full_path.to_string());
                        self.register_intrinsic_declaration(
                            qualified,
                            intrinsic_key,
                            fd.name.name.as_str(),
                            fd,
                        );
                        self.current_module = saved_importer_module;
                    }
                }
                // The declaration owns a constant's type; imports publish only its binding.
                Item::Const(cd) => {
                    if !cd.visibility.is_pub() {
                        continue;
                    }
                    let ty = self.resolve_registered_annotation_ty_no_holes(&cd.ty);
                    self.register_source_constant(Some(module_full_path), cd, ty);
                }
                _ => {}
            }
        }
        // Pass 2: Register impl methods (after types exist)
        let impl_origin = match import_spec {
            StdlibBarePublication::Prelude => SourceImplOrigin::Prelude,
            StdlibBarePublication::Import(_) => SourceImplOrigin::Source,
        };
        for &(index, (ref item, ref span)) in items {
            self.current_item_source = item_sources.get(index).cloned();
            self.current_module_idx = self
                .current_item_source
                .as_ref()
                .and_then(|source| self.file_span_indices.path_index(source))
                .unwrap_or(fallback_file);
            if let Item::Impl(id) = item {
                if Self::impl_decl_is_drop_impl(id) {
                    self.report_unsupported_impl_drop(span);
                    continue;
                }
                if matches!(&id.target_type.0, TypeExpr::Named { .. }) {
                    let saved_importer_module =
                        self.current_module.replace(module_full_path.to_string());
                    // Set current_self_type for resolving `Self` in method parameters
                    let prev_self_type = self.current_self_type.take();
                    let self_ty = self.resolve_impl_target(id);
                    let type_name = self.impl_target_identity(&self_ty);
                    let primitive_key = id
                        .trait_bound
                        .as_ref()
                        .and_then(|_| Self::canonical_primitive_or_builtin_key(&self_ty));
                    let builtin_receiver = self_ty.named_builtin().filter(|b| {
                        matches!(
                            b,
                            BuiltinType::Result | BuiltinType::Option | BuiltinType::Vec
                        )
                    });
                    self.current_self_type = Some((type_name.clone(), self_ty));
                    if !self.admit_source_impl(id, span, impl_origin) {
                        self.current_self_type = prev_self_type;
                        self.current_module = saved_importer_module;
                        continue;
                    }
                    let scope_pushed =
                        self.enter_impl_scope(id, span, Some(type_name.as_str()), false);

                    let impl_trait = self.impl_trait_ref(id);
                    for method in &id.methods {
                        let sig = self.register_impl_method(
                            &type_name,
                            method,
                            id.type_params.as_ref(),
                            id.where_clause.as_ref(),
                            id.trait_bound.as_ref(),
                        );
                        if let Some(builtin) = builtin_receiver {
                            let impl_params = self.source_parameter_heads(
                                id.type_params.as_deref().unwrap_or_default(),
                                &method.fn_span,
                            );
                            if builtin == BuiltinType::Vec {
                                if id.trait_bound.is_none() {
                                    self.builtin_vec_method_sigs.insert(
                                        method.name.to_string(),
                                        (impl_params, sig.clone()),
                                    );
                                }
                            } else {
                                self.builtin_result_option_method_sigs.insert(
                                    (builtin, method.name.to_string()),
                                    (impl_params, sig.clone()),
                                );
                                if method.name.name.as_str() == "expect"
                                    && id.trait_bound.is_none()
                                    && (canonical_std
                                        || matches!(import_spec, StdlibBarePublication::Prelude))
                                {
                                    let item = match builtin {
                                        BuiltinType::Option => crate::LangItem::OptionExpect,
                                        BuiltinType::Result => crate::LangItem::ResultExpect,
                                        _ => unreachable!("canonical optional receiver"),
                                    };
                                    if let Some(method_id) =
                                        self.impl_method_declaration_id(&type_name, method, None)
                                    {
                                        self.lang_items.insert(
                                            item.key(),
                                            crate::LangItemBinding {
                                                trait_name: type_name.clone(),
                                                trait_id: None,
                                                method_name: Some(method.name.to_string()),
                                                method_id: Some(method_id),
                                            },
                                        );
                                    }
                                }
                            }
                        }
                        // Also register on qualified type name
                        let unqualified_type = type_name
                            .strip_prefix(&format!("{module_full_path}."))
                            .unwrap_or(type_name.as_str());
                        let qualified_type = format!("{module_short}.{unqualified_type}");
                        if let Some(td) = self.lookup_type_def_mut(&qualified_type) {
                            td.methods.insert(method.name.to_string(), sig.clone());
                        }
                        if let (Some(canonical), Some(trait_ref)) =
                            (primitive_key.clone(), impl_trait.as_ref())
                        {
                            self.record_primitive_trait_impl_method(
                                canonical,
                                trait_ref.trait_id,
                                method.name.to_string(),
                                sig,
                            );
                        }
                    }
                    if let Some(trait_ref) = &impl_trait {
                        self.record_trait_impl_from_decl(id, trait_ref, span);
                    }

                    // Restore previous self type
                    self.current_self_type = prev_self_type;
                    if scope_pushed {
                        self.exit_impl_scope();
                    }
                    self.current_module = saved_importer_module;
                }
            }
        }
        // Pass 3: publish canonical type definitions after impl registration.
        // Registration itself uses the source leaf as temporary assembly state;
        // only the full owner survives this pass.
        for &(index, (ref item, _)) in items {
            self.current_item_source = item_sources.get(index).cloned();
            self.current_module_idx = self
                .current_item_source
                .as_ref()
                .and_then(|source| self.file_span_indices.path_index(source))
                .unwrap_or(fallback_file);
            match item {
                Item::TypeDecl(td) => {
                    if let Some(source_def) = self
                        .type_def_exact(&format!("{module_full_path}.{}", td.name.name.as_str()))
                        .cloned()
                    {
                        self.register_canonical_type_def(
                            module_full_path,
                            td.name.name.as_str(),
                            &source_def,
                        );
                    }
                    self.retire_imported_type_keys(
                        module_short,
                        module_full_path,
                        td.name.name.as_str(),
                    );
                    if td.visibility.is_pub() {
                        self.record_module_type_export(module_short, td.name.name.as_str());
                        self.record_module_type_export(module_full_path, td.name.name.as_str());
                    }
                }
                Item::Machine(md) => {
                    if !md.visibility.is_pub() {
                        continue;
                    }
                    let event_name = format!("{}.Event", md.name);
                    if let Some(source_def) = self
                        .type_def_exact(&format!("{module_full_path}.{}", md.name.name.as_str()))
                        .cloned()
                    {
                        self.register_canonical_type_def(
                            module_full_path,
                            md.name.name.as_str(),
                            &source_def,
                        );
                    }
                    if let Some(source_def) = self
                        .type_def_exact(&format!("{module_full_path}.{event_name}"))
                        .cloned()
                    {
                        self.register_canonical_type_def(
                            module_full_path,
                            &event_name,
                            &source_def,
                        );
                    }
                    self.retire_imported_type_keys(
                        module_short,
                        module_full_path,
                        md.name.name.as_str(),
                    );
                    self.retire_imported_type_keys(module_short, module_full_path, &event_name);
                    // A public machine publishes its generated event enum as
                    // part of the same declaration surface.  Keep the export
                    // ledger paired with the qualified aliases so import
                    // validation, checker resolution, and HIR all agree that
                    // `module.MachineEvent::Payload` is callable.
                    self.record_module_type_export(module_short, md.name.name.as_str());
                    self.record_module_type_export(module_short, &event_name);
                    self.record_module_type_export(module_full_path, md.name.name.as_str());
                    self.record_module_type_export(module_full_path, &event_name);
                }
                Item::Actor(ad) => {
                    // The dotted `{module_short}.{name}` entry is authored
                    // directly by `register_actor_base`; only the export
                    // record is added here.
                    self.record_module_type_export(module_short, ad.name.name.as_str());
                    self.record_module_type_export(module_full_path, ad.name.name.as_str());
                }
                _ => {}
            }
        }
        self.local_type_defs = saved_local_type_defs;
        self.source_type_defs = saved_source_type_defs;
        self.registration_origin_module = saved_registration_origin;
        self.current_item_source = importer_source;
        self.current_module_idx = importer_file;
        // Declaration annotations belong to their source file. Bare bindings
        // belong to the importing file and are published after its frame is
        // restored, including when this visit first registered the source.
        let flat_items: Vec<Spanned<Item>> = items.iter().map(|(_, item)| item.clone()).collect();
        self.publish_imported_hew_bindings(
            module_short,
            module_full_path,
            &flat_items,
            import_spec,
        );
    }

    /// Publish a selected stdlib free function into one importer's bare scope.
    ///
    /// Declarations remain globally registered under their canonical full
    /// owner; explicit imports publish only an exact lexical binding. The
    /// implicit language floor may additionally expose an ambient signature.
    pub(super) fn publish_stdlib_hew_function_binding(
        &mut self,
        binding: String,
        source_identity: &str,
        publication: StdlibBarePublication<'_>,
    ) {
        if !self.has_fn_sig(source_identity) {
            // A declaration source that cannot supply its canonical signature
            // must not manufacture an ambient bare function binding.
            return;
        }
        if publication.records_import_identity() {
            self.import_fn_name_aliases.insert(
                (
                    self.current_module.clone(),
                    self.current_module_idx,
                    binding.clone(),
                ),
                source_identity.to_string(),
            );
        } else {
            // Implicit language-floor bindings are ambient; explicit imports
            // only publish a lexical binding to the canonical signature.
            self.alias_fn_sig(&binding, source_identity);
        }
        self.record_published_bare_function(&binding, source_identity);
        let source_owner = source_identity
            .rsplit_once('.')
            .map(|(owner, _)| owner.to_string())
            .expect("stdlib Hew function binding has an owner-qualified identity");
        self.unqualified_to_module.insert(
            (
                self.current_module.clone(),
                self.current_module_idx,
                binding,
            ),
            source_owner,
        );
    }

    pub(super) fn publish_stdlib_hew_type_binding(
        &mut self,
        _module_short: &str,
        binding: String,
        source_identity: String,
        publication: StdlibBarePublication<'_>,
    ) {
        self.known_types.insert(binding.clone());
        self.record_published_bare_type(&binding, &source_identity);
        // The binding has one declaration-owned source identity. Store its
        // complete owner here as well, so use-time lint credit can translate
        // it back through `module_import_bindings` to the user's lexical
        // module qualifier without shortening dotted stdlib paths.
        let source_owner = source_identity
            .rsplit_once('.')
            .map(|(owner, _)| owner.to_string())
            .expect("stdlib Hew type binding has an owner-qualified identity");
        if publication.records_import_identity() {
            self.import_type_name_aliases.insert(
                (
                    self.current_module.clone(),
                    self.current_module_idx,
                    binding.clone(),
                ),
                source_identity,
            );
        }
        self.unqualified_to_module.insert(
            (
                self.current_module.clone(),
                self.current_module_idx,
                binding,
            ),
            source_owner,
        );
    }

    pub(super) fn stdlib_hew_source_already_registered(
        &mut self,
        decl: &ImportDecl,
        module_path: &str,
    ) -> bool {
        !self
            .registered_stdlib_hew_sources
            .insert(Self::stdlib_hew_source_identity(decl, module_path))
    }

    /// Register items from a user module under the module's short-name namespace.
    ///
    /// `module_full_path` is the full dot-separated module path
    /// (e.g. `"subpkg.helper"` for `import subpkg::helper;`).  It is recorded
    /// in `fn_def_spans` as the declaring-module identity so the access-allowed
    /// check can compare full paths (not just the short-name qualifier) when
    /// enforcing package-visibility boundaries.
    #[expect(
        clippy::too_many_lines,
        clippy::ref_option,
        reason = "statement type checking requires many cases"
    )]
    pub(in crate::check) fn register_user_module(
        &mut self,
        module_short: &str,
        module_full_path: &str,
        items: &[Spanned<Item>],
        item_source_paths: &[std::path::PathBuf],
        spec: &Option<ImportSpec>,
    ) {
        // Match the defining module's lexical scope during registration.
        let saved_local_type_defs = std::mem::take(&mut self.local_type_defs);
        let saved_source_type_defs = std::mem::take(&mut self.source_type_defs);
        for (item, _) in items {
            match item {
                Item::TypeDecl(td) => {
                    self.local_type_defs.insert(td.name.to_string());
                    self.source_type_defs.insert(td.name.to_string());
                }
                Item::Actor(ad) => {
                    self.local_type_defs.insert(ad.name.to_string());
                    self.source_type_defs.insert(ad.name.to_string());
                }
                Item::TypeAlias(ta) => {
                    self.local_type_defs.insert(ta.name.to_string());
                    self.source_type_defs.insert(ta.name.to_string());
                }
                Item::Record(rd) => {
                    self.local_type_defs.insert(rd.name.to_string());
                    self.source_type_defs.insert(rd.name.to_string());
                }
                Item::Machine(md) => {
                    self.local_type_defs.insert(md.name.to_string());
                    self.source_type_defs.insert(md.name.to_string());
                }
                _ => {}
            }
        }

        let importer_file_idx = self.current_module_idx;
        let importer_item_source = self.current_item_source.clone();
        let importer_item_ordinal = self.current_item_ordinal;
        let fallback_file_idx = self
            .file_span_indices
            .module_base(&hew_parser::module::ModulePath::new(
                module_full_path.split('.'),
            ))
            .unwrap_or(importer_file_idx);

        // Phase 0: bind and publish module-internal imports before traits/aliases/types resolve.
        for (item_idx, (item, span)) in items.iter().enumerate() {
            let Item::Import(decl) = item else {
                continue;
            };
            let declaring_file_idx = item_source_paths
                .get(item_idx)
                .and_then(|source| self.file_span_indices.path_index(source))
                .unwrap_or(fallback_file_idx);
            self.current_item_source = item_source_paths.get(item_idx).cloned();
            self.current_item_ordinal = item_idx;
            let saved_importer_module = self.current_module.replace(module_full_path.to_string());
            self.current_module_idx = declaring_file_idx;
            let short_binding = decl
                .module_alias
                .or_else(|| decl.path.last())
                .map(|ident| ident.to_string());
            let already_registered = short_binding.as_deref().is_some_and(|short_name| {
                self.module_import_bindings.contains_key(&(
                    Some(module_full_path.to_string()),
                    declaring_file_idx,
                    short_name.to_string(),
                ))
            });
            if !already_registered && decl.resolved_items.is_some() {
                self.register_import(decl, None);
            } else if let Some(file) = self
                .current_item_source
                .as_deref()
                .and_then(|source| self.defs.module_for_source(source))
                .or_else(|| self.defs.module_for_path(module_full_path))
            {
                self.bind_import_in_scope(file, decl, span);
            }
            self.current_module = saved_importer_module;
            self.current_module_idx = importer_file_idx;
        }

        // Phase 1a-prep: Trait shells before type/signature resolution.
        for (item_idx, (item, span)) in items.iter().enumerate() {
            self.current_item_source = item_source_paths.get(item_idx).cloned();
            self.current_item_ordinal = item_idx;
            let declaring_file_idx = item_source_paths
                .get(item_idx)
                .and_then(|source| self.file_span_indices.path_index(source))
                .unwrap_or(fallback_file_idx);
            if let Item::Trait(tr) = item {
                let qualified_trait = format!("{module_full_path}.{}", tr.name);
                let surface_qualified_trait = format!("{module_short}.{}", tr.name);
                self.type_visibility
                    .entry(qualified_trait.clone())
                    .or_insert((tr.visibility, Some(module_full_path.to_string())));
                self.type_visibility
                    .entry(surface_qualified_trait)
                    .or_insert((tr.visibility, Some(module_full_path.to_string())));
                self.type_def_spans
                    .entry(qualified_trait.clone())
                    .or_insert_with(|| span.clone());
                if !tr.visibility.is_pub() {
                    continue;
                }
                let saved_importer_module =
                    self.current_module.replace(module_full_path.to_string());
                self.current_module_idx = declaring_file_idx;
                let info = self.trait_shell_from_decl(
                    tr,
                    Some(module_full_path.to_string()),
                    declaring_file_idx,
                );
                self.insert_trait_def(&qualified_trait, info);
                self.current_module = saved_importer_module;
                self.current_module_idx = importer_file_idx;
            }
        }

        // Phase 1a: Complete traits, types, machines, actors, records, supervisors.
        for (item_idx, (item, span)) in items.iter().enumerate() {
            self.current_item_source = item_source_paths.get(item_idx).cloned();
            self.current_item_ordinal = item_idx;
            let declaring_file_idx = item_source_paths
                .get(item_idx)
                .and_then(|source| self.file_span_indices.path_index(source))
                .unwrap_or(fallback_file_idx);
            match item {
                Item::Record(decl) => {
                    let canonical = format!("{module_full_path}.{}", decl.name);
                    let saved_module = self.current_module.replace(module_full_path.to_string());
                    self.current_module_idx = declaring_file_idx;
                    self.register_record_decl(decl);
                    if let Some(definition) = self.type_def_at(&canonical).cloned() {
                        self.register_canonical_type_def(
                            module_full_path,
                            decl.name.name.as_str(),
                            &definition,
                        );
                    }
                    self.current_module = saved_module;
                    self.current_module_idx = importer_file_idx;
                    self.type_visibility.insert(
                        canonical.clone(),
                        (decl.visibility, Some(module_full_path.to_string())),
                    );
                    self.type_def_spans
                        .entry(canonical.clone())
                        .or_insert_with(|| span.clone());
                    if matches!(decl.kind, RecordKind::Tuple(_)) {
                        self.fn_visibility
                            .insert(canonical.clone(), decl.visibility);
                        self.fn_def_spans
                            .entry(canonical.clone())
                            .or_insert_with(|| (span.clone(), Some(module_full_path.to_string())));
                        if decl.visibility == hew_parser::ast::Visibility::Pub {
                            self.module_fn_exports.insert(canonical.clone());
                        }
                    }
                    if decl.visibility.is_pub() {
                        self.record_module_type_export(module_full_path, decl.name.name.as_str());
                        if let Some(binding) = StdlibBarePublication::Import(spec)
                            .bare_binding(decl.name.name.as_str())
                        {
                            self.publish_stdlib_hew_type_binding(
                                module_short,
                                binding.clone(),
                                canonical.clone(),
                                StdlibBarePublication::Import(spec),
                            );
                            if matches!(decl.kind, RecordKind::Tuple(_)) {
                                self.publish_stdlib_hew_function_binding(
                                    binding,
                                    &canonical,
                                    StdlibBarePublication::Import(spec),
                                );
                            }
                        }
                    }
                }
                Item::TypeDecl(td) => {
                    // Record visibility for all TypeDecls so the enforcement check
                    // can distinguish "private, not accessible" from "unknown symbol".
                    // Use module_full_path (not just module_short) so cross-package
                    // checks compare full paths and don't conflate e.g. "helper" (short)
                    // with the root package when the module lives at subpkg::helper.
                    let qualified_type = format!("{module_full_path}.{}", td.name);
                    let surface_qualified_type = format!("{module_short}.{}", td.name);
                    self.type_visibility
                        .entry(qualified_type.clone())
                        .or_insert((td.visibility, Some(module_full_path.to_string())));
                    self.type_visibility
                        .entry(surface_qualified_type)
                        .or_insert((td.visibility, Some(module_full_path.to_string())));
                    // Record the declaration span so visibility-violation diagnostics
                    // can point "declared here" at the actual declaration even when the
                    // type is not pub (and therefore skips the full namespace registration).
                    self.type_def_spans
                        .entry(qualified_type.clone())
                        .or_insert_with(|| span.clone());
                    if !td.visibility.is_pub() {
                        // Private sibling types remain source-visible and can
                        // appear in a public actor's reply or extern signature.
                        // Publish their exact owner metadata without exposing
                        // them as module exports or importer bindings.
                        let source_def = self.type_def_at(&qualified_type).cloned().or_else(|| {
                            let saved_importer_module =
                                self.current_module.replace(module_full_path.to_string());
                            self.current_module_idx = declaring_file_idx;
                            self.register_type_decl(td);
                            let source_def = self.type_def_at(td.name.name.as_str()).cloned();
                            self.current_module = saved_importer_module;
                            self.current_module_idx = importer_file_idx;
                            source_def
                        });
                        if let Some(source_def) = source_def.as_ref() {
                            self.register_canonical_type_def(
                                module_full_path,
                                td.name.name.as_str(),
                                source_def,
                            );
                        }
                        continue;
                    }
                    if !self.register_type_namespace_name(Some(module_full_path), td.name, span) {
                        continue;
                    }
                    // Qualified authority is always published: the source
                    // module's own bare def (read by the alias copy), the
                    // qualified alias, and the module-export record that drives
                    // use-time ambiguity candidate naming.
                    let saved_importer_module =
                        self.current_module.replace(module_full_path.to_string());
                    self.current_module_idx = declaring_file_idx;
                    self.register_type_decl(td);
                    let source_def = self.type_def_at(td.name.name.as_str()).cloned();
                    self.current_module = saved_importer_module;
                    self.current_module_idx = importer_file_idx;
                    if spec.is_none() {
                        self.register_qualified_type_alias(module_short, td.name.name.as_str());
                    }
                    if let Some(source_def) = source_def.as_ref() {
                        self.register_canonical_type_def(
                            module_full_path,
                            td.name.name.as_str(),
                            source_def,
                        );
                    }
                    self.record_module_type_export(module_full_path, td.name.name.as_str());
                    if spec.is_none() {
                        self.record_module_type_export(module_short, td.name.name.as_str());
                    }
                    // The importer-scope bare binding is opt-in: a plain
                    // `import m;` publishes only the qualified name, mirroring
                    // the function/trait arms. Named (`::{ T }`) and glob
                    // imports publish the bare (or aliased) binding.
                    if Self::should_import_name(td.name.name.as_str(), spec) {
                        let explicit_import_name =
                            Self::resolve_import_name(spec, td.name.name.as_str());
                        let binding_name = explicit_import_name
                            .clone()
                            .unwrap_or_else(|| td.name.to_string());
                        self.known_types.insert(binding_name.clone());
                        let source_identity = format!("{module_full_path}.{}", td.name);
                        self.record_published_bare_type(&binding_name, &source_identity);
                        // Record every published bare type binding, including
                        // an unrenamed named/glob import. HIR needs the source
                        // identity for `import foo::{ Receiver }` just as much
                        // as for `Receiver as Rx`: otherwise the bare spelling
                        // can be stolen by the builtin catalog before HIR sees
                        // that it names `foo.Receiver`.
                        if explicit_import_name.is_some() {
                            self.import_type_name_aliases.insert(
                                (
                                    self.current_module.clone(),
                                    self.current_module_idx,
                                    binding_name.clone(),
                                ),
                                source_identity.clone(),
                            );
                        }
                        self.unqualified_to_module.insert(
                            (
                                self.current_module.clone(),
                                self.current_module_idx,
                                binding_name,
                            ),
                            module_full_path.to_string(),
                        );
                    }
                }
                Item::Machine(md) => {
                    // Record visibility for all Machines so the enforcement check
                    // can distinguish "private, not accessible" from "unknown symbol".
                    // Use module_full_path for the same reason as TypeDecl above.
                    let qualified_machine = format!("{module_short}.{}", md.name);
                    self.type_visibility
                        .entry(qualified_machine.clone())
                        .or_insert((md.visibility, Some(module_full_path.to_string())));
                    // Record the declaration span for non-pub machine types so the
                    // "declared here" note in E_VISIBILITY points at the actual decl.
                    self.type_def_spans
                        .entry(qualified_machine)
                        .or_insert_with(|| span.clone());
                    if !md.visibility.is_pub() {
                        continue;
                    }
                    if !self.register_machine_type_namespace_names(
                        Some(module_full_path),
                        md.name.name.as_str(),
                        span,
                    ) {
                        continue;
                    }
                    let event_name = format!("{}.Event", md.name);
                    // Qualified authority is always published for the machine
                    // and its companion event enum.
                    let saved_importer_module =
                        self.current_module.replace(module_full_path.to_string());
                    self.current_module_idx = declaring_file_idx;
                    self.register_machine_decl(md, span);
                    let machine_def = self.type_def_at(md.name.name.as_str()).cloned();
                    let event_def = self.type_def_at(&event_name).cloned();
                    self.current_module = saved_importer_module;
                    self.current_module_idx = importer_file_idx;
                    self.register_qualified_type_alias(module_short, md.name.name.as_str());
                    self.register_qualified_type_alias(module_short, &event_name);
                    if let Some(machine_def) = machine_def.as_ref() {
                        self.register_canonical_type_def(
                            module_full_path,
                            md.name.name.as_str(),
                            machine_def,
                        );
                    }
                    if let Some(event_def) = event_def.as_ref() {
                        self.register_canonical_type_def(module_full_path, &event_name, event_def);
                    }
                    self.record_module_type_export(module_short, md.name.name.as_str());
                    self.record_module_type_export(module_short, &event_name);
                    self.record_module_type_export(module_full_path, md.name.name.as_str());
                    self.record_module_type_export(module_full_path, &event_name);
                    // Bare publication of the machine and its event enum is
                    // opt-in, gated together so a named/glob import exposes both
                    // or neither.
                    if Self::should_import_name(md.name.name.as_str(), spec) {
                        let machine_binding =
                            Self::resolve_import_name(spec, md.name.name.as_str())
                                .unwrap_or_else(|| md.name.to_string());
                        let event_binding = Self::resolve_import_name(spec, &event_name)
                            .unwrap_or_else(|| event_name.clone());
                        self.known_types.insert(machine_binding.clone());
                        self.known_types.insert(event_binding.clone());
                        let machine_identity = format!("{module_full_path}.{}", md.name);
                        let event_identity = format!("{module_full_path}.{event_name}");
                        self.record_published_bare_type(&machine_binding, &machine_identity);
                        self.record_published_bare_type(&event_binding, &event_identity);
                        // HIR resolves annotations itself, and a machine's
                        // value-class/layout facts key by the qualified
                        // identity. Publish the binding→identity fact for
                        // every named/glob import (not only renames): without
                        // it, `fn f(m: Machine)` on a selectively-imported
                        // machine froze a bare binding type MIR could not
                        // classify (`owned call-carrier parameter` NYI +
                        // `E_MIR: unknown type`), while the checker's own
                        // expression facts already carried the dotted owner.
                        self.import_type_name_aliases.insert(
                            (
                                self.current_module.clone(),
                                self.current_module_idx,
                                machine_binding.clone(),
                            ),
                            machine_identity.clone(),
                        );
                        self.import_type_name_aliases.insert(
                            (
                                self.current_module.clone(),
                                self.current_module_idx,
                                event_binding.clone(),
                            ),
                            event_identity.clone(),
                        );
                        self.unqualified_to_module.insert(
                            (
                                self.current_module.clone(),
                                self.current_module_idx,
                                machine_binding,
                            ),
                            module_full_path.to_string(),
                        );
                        self.unqualified_to_module.insert(
                            (
                                self.current_module.clone(),
                                self.current_module_idx,
                                event_binding,
                            ),
                            module_full_path.to_string(),
                        );
                    }
                }
                Item::Trait(tr) => {
                    if !tr.visibility.is_pub() {
                        continue;
                    }
                    let qualified = format!("{module_full_path}.{}", tr.name);
                    if let Some(trait_id) = self.lookup_declaration(&qualified) {
                        let saved_importer_module =
                            self.current_module.replace(module_full_path.to_string());
                        self.current_module_idx = declaring_file_idx;
                        self.register_trait_supers(trait_id, tr);
                        let trait_scope =
                            self.enter_primary_sig_scope(&[(tr.type_params.as_ref(), None)]);
                        for trait_item in &tr.items {
                            if let TraitItem::Method(method) = trait_item {
                                self.register_trait_method_sig(trait_id, method, span);
                            }
                        }
                        self.exit_primary_sig_scope(trait_scope);
                        self.current_module = saved_importer_module;
                        self.current_module_idx = importer_file_idx;
                    }
                    let import_binding = if Self::should_import_name(tr.name.name.as_str(), spec) {
                        let binding_name = Self::resolve_import_name(spec, tr.name.name.as_str())
                            .unwrap_or_else(|| tr.name.to_string());
                        // The unqualified trait binding lands in the *importing*
                        // module's namespace, not the source module's.
                        let importer = self.current_module.clone();
                        if self.register_type_namespace_name(
                            importer.as_deref(),
                            Ident::new(&binding_name),
                            span,
                        ) {
                            Some(binding_name)
                        } else {
                            None
                        }
                    } else {
                        None
                    };

                    if let Some(binding_name) = import_binding {
                        self.unqualified_to_module.insert(
                            (
                                self.current_module.clone(),
                                self.current_module_idx,
                                binding_name,
                            ),
                            module_full_path.to_string(),
                        );
                    }
                }
                Item::Actor(ad) => {
                    let qualified_actor = format!("{module_full_path}.{}", ad.name);
                    self.type_visibility
                        .entry(qualified_actor.clone())
                        .or_insert((ad.visibility, Some(module_full_path.to_string())));
                    self.type_def_spans
                        .entry(qualified_actor.clone())
                        .or_insert_with(|| span.clone());
                    if !ad.visibility.is_pub() {
                        continue;
                    }
                    let actor_already_registered = self.type_def_at(&qualified_actor);
                    if actor_already_registered.is_none()
                        && !self.register_type_namespace_name(Some(module_full_path), ad.name, span)
                    {
                        continue;
                    }
                    let saved_importer_module =
                        self.current_module.replace(module_full_path.to_string());
                    self.current_module_idx = declaring_file_idx;
                    self.register_actor_base(ad, Some(module_full_path));
                    self.current_module = saved_importer_module;
                    self.current_module_idx = importer_file_idx;
                    self.record_module_type_export(module_full_path, ad.name.name.as_str());
                    if Self::should_import_name(ad.name.name.as_str(), spec) {
                        let binding_name = Self::resolve_import_name(spec, ad.name.name.as_str())
                            .unwrap_or_else(|| ad.name.to_string());
                        let source_identity = format!("{module_full_path}.{}", ad.name);
                        self.record_published_bare_type(&binding_name, &source_identity);
                        self.import_type_name_aliases.insert(
                            (
                                self.current_module.clone(),
                                self.current_module_idx,
                                binding_name.clone(),
                            ),
                            source_identity,
                        );
                        self.unqualified_to_module.insert(
                            (
                                self.current_module.clone(),
                                self.current_module_idx,
                                binding_name,
                            ),
                            module_full_path.to_string(),
                        );
                    }
                }
                Item::Supervisor(sd) => {
                    let qualified = format!("{module_full_path}.{}", sd.name);
                    self.type_visibility
                        .entry(qualified.clone())
                        .or_insert((sd.visibility, Some(module_full_path.to_string())));
                    self.type_def_spans
                        .entry(qualified.clone())
                        .or_insert_with(|| span.clone());
                    if !sd.visibility.is_pub() {
                        continue;
                    }
                    if self.type_def_at(&qualified).is_none()
                        && !self.register_type_namespace_name(Some(module_full_path), sd.name, span)
                    {
                        continue;
                    }
                    let saved_importer_module =
                        self.current_module.replace(module_full_path.to_string());
                    self.current_module_idx = declaring_file_idx;
                    self.register_supervisor_decl_as(sd, &qualified);
                    self.current_module = saved_importer_module;
                    self.current_module_idx = importer_file_idx;
                    self.record_module_type_export(module_full_path, sd.name.name.as_str());
                    if Self::should_import_name(sd.name.name.as_str(), spec) {
                        let binding_name = Self::resolve_import_name(spec, sd.name.name.as_str())
                            .unwrap_or_else(|| sd.name.to_string());
                        self.record_published_bare_type(&binding_name, &qualified);
                        self.import_type_name_aliases.insert(
                            (
                                self.current_module.clone(),
                                self.current_module_idx,
                                binding_name.clone(),
                            ),
                            qualified.clone(),
                        );
                        self.unqualified_to_module.insert(
                            (
                                self.current_module.clone(),
                                self.current_module_idx,
                                binding_name,
                            ),
                            module_full_path.to_string(),
                        );
                    }
                }
                Item::Macro(_)
                | Item::MacroCall(_)
                | Item::Import(_)
                | Item::Function(_)
                | Item::TypeAlias(_)
                | Item::Const(_)
                | Item::Impl(_)
                | Item::ExternBlock(_) => {}
            }
        }
        // Phase 1b: Functions, constants, and extern blocks.
        for (item_idx, (item, span)) in items.iter().enumerate() {
            self.current_item_source = item_source_paths.get(item_idx).cloned();
            self.current_item_ordinal = item_idx;
            let declaring_file_idx = item_source_paths
                .get(item_idx)
                .and_then(|source| self.file_span_indices.path_index(source))
                .unwrap_or(fallback_file_idx);
            match item {
                Item::Function(fd) => {
                    let qualified =
                        self.canonical_fn_identity(Some(module_full_path), fd.name.name.as_str());
                    let surface_qualified = format!("{module_short}.{}", fd.name);
                    self.fn_visibility
                        .entry(qualified.clone())
                        .or_insert(fd.visibility);
                    self.fn_visibility
                        .entry(surface_qualified.clone())
                        .or_insert(fd.visibility);
                    self.fn_def_spans
                        .entry(qualified.clone())
                        .or_insert_with(|| (span.clone(), Some(module_full_path.to_string())));

                    let saved_importer_module =
                        self.current_module.replace(module_full_path.to_string());
                    self.current_module_idx = declaring_file_idx;
                    let sig = self.build_fn_sig_from_decl(fd);
                    self.current_module = saved_importer_module;
                    self.current_module_idx = importer_file_idx;
                    if fd.visibility == hew_parser::ast::Visibility::Pub {
                        self.module_fn_exports.insert(qualified.clone());
                        if spec.is_none() {
                            self.module_fn_exports.insert(surface_qualified.clone());
                        }
                    }
                    self.insert_fn_sig_at(&qualified, sig);
                    if spec.is_none() && !self.has_fn_sig(&surface_qualified) {
                        self.alias_fn_sig(&surface_qualified, &qualified);
                    }

                    if let Some(intrinsic_key) = &fd.intrinsic {
                        let saved_importer_module =
                            self.current_module.replace(module_full_path.to_string());
                        self.current_module_idx = declaring_file_idx;
                        self.register_intrinsic_declaration(
                            qualified.clone(),
                            intrinsic_key,
                            fd.name.name.as_str(),
                            fd,
                        );
                        self.current_module = saved_importer_module;
                        self.current_module_idx = importer_file_idx;
                    }

                    if fd.visibility == hew_parser::ast::Visibility::Pub
                        && Self::should_import_name(fd.name.name.as_str(), spec)
                    {
                        let binding_name = Self::resolve_import_name(spec, fd.name.name.as_str())
                            .unwrap_or_else(|| fd.name.to_string());
                        self.import_fn_name_aliases.insert(
                            (
                                self.current_module.clone(),
                                self.current_module_idx,
                                binding_name.clone(),
                            ),
                            format!("{module_full_path}.{}", fd.name),
                        );
                        self.record_published_bare_function(
                            &binding_name,
                            &format!("{module_full_path}.{}", fd.name),
                        );
                        self.unqualified_to_module.insert(
                            (
                                self.current_module.clone(),
                                self.current_module_idx,
                                binding_name,
                            ),
                            module_full_path.to_string(),
                        );
                    }
                }
                Item::Const(cd) => {
                    if !cd.visibility.is_pub() {
                        continue;
                    }
                    let saved_importer_module =
                        self.current_module.replace(module_full_path.to_string());
                    self.current_module_idx = declaring_file_idx;
                    let ty = self.resolve_registered_annotation_ty_no_holes(&cd.ty);
                    self.current_module = saved_importer_module;
                    self.current_module_idx = importer_file_idx;
                    self.register_source_constant(Some(module_full_path), cd, ty);
                    if Self::should_import_name(cd.name.name.as_str(), spec) {
                        let binding_name = Self::resolve_import_name(spec, cd.name.name.as_str())
                            .unwrap_or_else(|| cd.name.to_string());
                        self.record_published_bare_const(
                            &binding_name,
                            &format!("{module_full_path}.{}", cd.name),
                        );
                    }
                }
                Item::ExternBlock(eb) => {
                    let has_unregistered_signature = eb.functions.iter().any(|function| {
                        !self.has_fn_sig(&format!("{module_full_path}.{}", function.name))
                    });
                    if has_unregistered_signature {
                        let saved_importer_module =
                            self.current_module.replace(module_full_path.to_string());
                        self.current_module_idx = declaring_file_idx;
                        self.register_extern_block(eb, span);
                        self.current_module = saved_importer_module;
                        self.current_module_idx = importer_file_idx;
                    }
                }
                _ => {}
            }
        }
        // Phase 2: Register impl blocks after types, traits, aliases, and functions.
        for (item_idx, (item, span)) in items.iter().enumerate() {
            let Item::Impl(id) = item else {
                continue;
            };
            self.current_item_source = item_source_paths.get(item_idx).cloned();
            self.current_item_ordinal = item_idx;
            let declaring_file_idx = item_source_paths
                .get(item_idx)
                .and_then(|source| self.file_span_indices.path_index(source))
                .unwrap_or(fallback_file_idx);
            let saved_importer_module = self.current_module.replace(module_full_path.to_string());
            self.current_module_idx = declaring_file_idx;
            if matches!(&id.target_type.0, TypeExpr::Named { .. }) {
                let prev_self_type = self.current_self_type.take();
                let self_ty = self.resolve_impl_target(id);
                let type_name = self.impl_target_identity(&self_ty);
                let primitive_key = id
                    .trait_bound
                    .as_ref()
                    .and_then(|_| Self::canonical_primitive_or_builtin_key(&self_ty));
                self.current_self_type = Some((type_name.clone(), self_ty));
                if !self.admit_source_impl(id, span, SourceImplOrigin::Source) {
                    self.current_self_type = prev_self_type;
                    self.current_module = saved_importer_module;
                    self.current_module_idx = importer_file_idx;
                    continue;
                }
                let scope_pushed = self.enter_impl_scope(id, span, Some(type_name.as_str()), false);

                let impl_trait = self.impl_trait_ref(id);
                for method in &id.methods {
                    let sig = self.register_impl_method(
                        &type_name,
                        method,
                        id.type_params.as_ref(),
                        id.where_clause.as_ref(),
                        id.trait_bound.as_ref(),
                    );
                    if let (Some(canonical), Some(trait_ref)) =
                        (primitive_key.clone(), impl_trait.as_ref())
                    {
                        self.record_primitive_trait_impl_method(
                            canonical,
                            trait_ref.trait_id,
                            method.name.to_string(),
                            sig,
                        );
                    }
                }
                if let Some(trait_ref) = &impl_trait {
                    self.record_trait_impl_from_decl(id, trait_ref, span);
                }

                self.current_self_type = prev_self_type;
                if scope_pushed {
                    self.exit_impl_scope();
                }
            }
            self.current_module = saved_importer_module;
            self.current_module_idx = importer_file_idx;
        }
        self.current_item_source = importer_item_source;
        self.current_item_ordinal = importer_item_ordinal;
        // Declaration and impl registration above use leaf keys as temporary
        // assembly state. Refresh the full-owner rows with the complete defs,
        // then remove both non-canonical spellings before returning to the
        // importer.
        for (item, _) in items {
            match item {
                Item::TypeDecl(td) => {
                    if let Some(source_def) = self
                        .type_def_exact(&format!("{module_full_path}.{}", td.name.name.as_str()))
                        .cloned()
                    {
                        self.register_canonical_type_def(
                            module_full_path,
                            td.name.name.as_str(),
                            &source_def,
                        );
                    }
                    self.retire_imported_type_keys(
                        module_short,
                        module_full_path,
                        td.name.name.as_str(),
                    );
                }
                Item::Machine(md) if md.visibility.is_pub() => {
                    let event_name = format!("{}.Event", md.name);
                    if let Some(source_def) = self
                        .type_def_exact(&format!("{module_full_path}.{}", md.name.name.as_str()))
                        .cloned()
                    {
                        self.register_canonical_type_def(
                            module_full_path,
                            md.name.name.as_str(),
                            &source_def,
                        );
                    }
                    if let Some(source_def) = self
                        .type_def_exact(&format!("{module_full_path}.{event_name}"))
                        .cloned()
                    {
                        self.register_canonical_type_def(
                            module_full_path,
                            &event_name,
                            &source_def,
                        );
                    }
                    self.retire_imported_type_keys(
                        module_short,
                        module_full_path,
                        md.name.name.as_str(),
                    );
                    self.retire_imported_type_keys(module_short, module_full_path, &event_name);
                }
                _ => {}
            }
        }

        self.local_type_defs = saved_local_type_defs;
        self.source_type_defs = saved_source_type_defs;
    }

    /// Compute the checker identity for an actor declared in `module_short`.
    ///
    /// Module actors are identified by the dotted `{module_short}.{name}` key
    /// (the same authoritative form `resolve_module_type` reads); root and
    /// flat-file actors keep the bare name. This is the single authority for
    /// the actor-identity key shape — every registration and lookup site
    /// derives the key through here so producer and consumer cannot drift.
    pub(in crate::check) fn actor_identity(module_short: Option<&str>, name: &str) -> String {
        match module_short {
            Some(m) => format!("{m}.{name}"),
            None => name.to_string(),
        }
    }

    /// Register an actor's core items: the type declaration, receive functions,
    /// and inline methods.  This block is identical across all three import
    /// registration paths; only the export-record and unqualified-binding
    /// steps differ and are therefore kept in each caller.
    ///
    /// Actor identity is the dotted `{module_short}.{name}` key for module
    /// actors (authored directly here — NOT copied from a bare entry, which
    /// is last-write-wins across modules) and the bare name for root and
    /// flat-file actors. The bare key is never written for module actors, so
    /// a second same-named import cannot clobber another module's actor.
    pub(in crate::check) fn register_actor_base(
        &mut self,
        ad: &ActorDecl,
        module_short: Option<&str>,
    ) {
        let identity = self.named_item_declaration(ad.name).map_or_else(
            || Self::actor_identity(module_short, ad.name.name.as_str()),
            |id| self.defs.path(id).to_string(),
        );
        self.register_actor_decl_as(ad, &identity);
        self.known_types.insert(identity.clone());
        // A generic actor's own params (`actor Worker<T>`) are in scope for every
        // receive-fn / method signature. Push them and register these primary
        // signatures scope-locally so an out-of-scope generic name is rejected at
        // the annotation, while the actor's legitimate `<T>` still resolves. This
        // mirrors the inline `Item::Actor` path for imported / flat-file actors.
        let actor_sig_scope = self.enter_primary_sig_scope(&[(Some(&ad.type_params), None)]);
        let actor_decl = self
            .named_item_declaration(ad.name)
            .or_else(|| self.lookup_declaration(&identity));
        for rf in &ad.receive_fns {
            self.register_receive_fn(actor_decl, &identity, rf);
        }
        for method in &ad.methods {
            let method_name = format!("{identity}::{}", method.name);
            let method_decl = actor_decl.and_then(|owner| {
                self.defs.member_of_kind(
                    owner,
                    method.name.name,
                    crate::DeclarationKind::ActorMethod,
                )
            });
            self.register_fn_sig_with_name(&method_name, method, method_decl);
        }
        self.exit_primary_sig_scope(actor_sig_scope);
    }

    /// Insert a qualified alias (`module_short.Name`) for a type that has
    /// already been registered under its bare name.
    ///
    /// Actors do NOT use this copy-based alias: their dotted key is authored
    /// directly by [`Self::register_actor_base`], so the qualified entry is
    /// always the module's own actor rather than whichever bare entry won.
    pub(in crate::check) fn register_qualified_type_alias(
        &mut self,
        module_short: &str,
        name: &str,
    ) {
        let qualified = format!("{module_short}.{name}");
        // Both source spellings must select the same declaration before its
        // source-span index can acquire an alias. Semantic tables use the ID.
        let declaration = self.type_def_key(name);
        if declaration.is_some() && declaration == self.type_def_key(&qualified) {
            if let Some(span) = self.type_def_spans.get(name).cloned() {
                self.type_def_spans.insert(qualified.clone(), span);
            }
        }
    }

    /// Publish a source module type under its full declaration owner.
    ///
    /// The source definition is assembled under its leaf during registration,
    /// but that spelling is never an imported identity. Calling this again
    /// after impl registration deliberately refreshes the canonical row with
    /// the declaration's complete method set.
    pub(super) fn register_canonical_type_def(
        &mut self,
        module_full_path: &str,
        name: &str,
        source_def: &TypeDef,
    ) {
        let qualified = format!("{module_full_path}.{name}");
        self.insert_type_def(&qualified, source_def.clone());
        if !self.type_def_spans.contains_key(&qualified) {
            if let Some(span) = self.type_def_spans.get(name).cloned() {
                self.type_def_spans.insert(qualified.clone(), span);
            }
        }
        // The full owner is marker authority too. Never copy marker rows from
        // `name`: that bare row is last-writer-wins across modules, so importing
        // `replysend.Reply` after `replynonsend.Reply` could otherwise stamp the
        // non-Send owner's canonical key with the Send sibling's fields. Build
        // the structural rows directly from this source definition instead.
        let members = if source_def.kind == TypeDefKind::Enum {
            Self::structural_member_types_for_type(source_def)
        } else if source_def.kind == TypeDefKind::Record && source_def.fields.is_empty() {
            self.fn_sig(&qualified).map_or_else(Vec::new, |signature| {
                signature
                    .params
                    .iter()
                    .map(|parameter| parameter.ty.clone())
                    .collect()
            })
        } else {
            source_def.fields.values().cloned().collect()
        };
        let members = self.expand_for_marker_registration(&members);
        self.registry.register_type(
            self.head_of_declaration(
                self.nominal_head_for_key(&qualified)
                    .expect("published type has a declaration")
                    .id,
            ),
            members,
        );
        self.registry.register_type_params(
            self.head_of_declaration(
                self.nominal_head_for_key(&qualified)
                    .expect("published type has a declaration")
                    .id,
            ),
            source_def.type_params.clone(),
        );
        self.handle_bearing_dirty = true;
    }

    /// Record that an imported module exports a type/actor name.
    ///
    /// Mirrors the `module_fn_exports` precedent (`register_builtin_sig` /
    /// `register_user_module` `Item::Function` arm) for type names.  Drives the
    /// module-qualified value-constructor pre-dispatch in `check_field_access`
    /// and `check_struct_init` so we can emit a precise "module `m` has no
    /// exported type `T`" diagnostic instead of leaking through to the
    /// "undefined variable" / "undefined type" fallbacks.
    pub(in crate::check) fn record_module_type_export(&mut self, module_short: &str, name: &str) {
        self.module_type_exports
            .entry(module_short.to_string())
            .or_default()
            .insert(name.to_string());
    }

    /// Record that the bare binding `bare_binding` was published into the current
    /// importer's scope, denoting `source_identity` (the owner-qualified SOURCE
    /// type name `owner.OriginalName`). Populated at every site that inserts a
    /// bare type binding into `unqualified_to_module`, this set is the authority
    /// the use-time ambiguity check reads: a bare reference is ambiguous only
    /// when more than one source identity is published under it, so a plain
    /// `import` that exported but did not publish it cannot poison an explicit
    /// named import of the same bare name.
    ///
    /// The value is the SOURCE identity, not merely the owner module, so an
    /// aliased import (`import m::{ T as U }`) records `U -> m.T` — the binding
    /// `U` resolves to the type `m` actually exports under `T`, never the wrong
    /// `m.U`. `published_bare_type_qualified` reads this identity back verbatim.
    pub(in crate::check) fn record_published_bare_type(
        &mut self,
        bare_binding: &str,
        source_identity: &str,
    ) {
        self.published_bare_type_owners
            .entry((
                self.current_module.clone(),
                self.current_module_idx,
                bare_binding.to_string(),
            ))
            .or_default()
            .insert(source_identity.to_string());
    }

    /// Record an exact source owner for a bare function import binding.
    pub(in crate::check) fn record_published_bare_function(
        &mut self,
        bare_binding: &str,
        source_identity: &str,
    ) {
        self.published_bare_function_owners
            .entry((
                self.current_module.clone(),
                self.current_module_idx,
                bare_binding.to_string(),
            ))
            .or_default()
            .insert(source_identity.to_string());
    }

    /// Record an exact source owner for a bare constant import binding.
    pub(in crate::check) fn record_published_bare_const(
        &mut self,
        bare_binding: &str,
        source_identity: &str,
    ) {
        self.published_bare_const_owners
            .entry((
                self.current_module.clone(),
                self.current_module_idx,
                bare_binding.to_string(),
            ))
            .or_default()
            .insert(source_identity.to_string());
    }
}
