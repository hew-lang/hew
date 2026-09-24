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
    pub(super) fn type_name_contains_owned_handle(
        &self,
        type_name: &str,
        visiting: &mut HashSet<String>,
    ) -> bool {
        let Some(lookup_name) = self.registered_type_def_name(type_name) else {
            return false;
        };
        if !visiting.insert(lookup_name.clone()) {
            return false;
        }
        let contains_owned_handle = self.type_defs.get(&lookup_name).is_some_and(|type_def| {
            type_def.kind == TypeDefKind::Struct
                && type_def
                    .fields
                    .values()
                    .any(|field_ty| self.ty_contains_owned_handle(field_ty, visiting))
        });
        visiting.remove(&lookup_name);
        contains_owned_handle
    }

    pub(super) fn ty_contains_owned_handle(&self, ty: &Ty, visiting: &mut HashSet<String>) -> bool {
        match ty {
            Ty::Tuple(items) => items
                .iter()
                .any(|item_ty| self.ty_contains_owned_handle(item_ty, visiting)),
            Ty::Array(element_ty, _) | Ty::Slice(element_ty) => {
                self.ty_contains_owned_handle(element_ty, visiting)
            }
            Ty::Named { name, args, .. } => {
                self.canonical_owned_handle_type_name(name).is_some()
                    || args
                        .iter()
                        .any(|arg_ty| self.ty_contains_owned_handle(arg_ty, visiting))
                    || self.type_name_contains_owned_handle(name, visiting)
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

    pub(super) fn register_compiled_stdlib_receiver_impls(
        &mut self,
        module_short: &str,
        source: &str,
        receiver_names: &[&str],
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
        let impl_items: Vec<Spanned<Item>> = parsed
            .program
            .items
            .into_iter()
            .filter(|(item, _)| {
                let Item::Impl(id) = item else {
                    return false;
                };
                let TypeExpr::Named { path, .. } = &id.target_type.0 else {
                    return false;
                };
                let name = &path.to_string(); // TRANSITION(P1): deleted by A1 commit 2
                receiver_names.iter().any(|receiver| name == receiver)
            })
            .collect();
        if !impl_items.is_empty() {
            let module_full_path = format!("std.{module_short}");
            self.register_stdlib_hew_items(
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
            HashMap::new(),
            vec![],
            Ty::Named {
                builtin: Some(builtin),
                name: builtin.canonical_name().to_string(),
                args: type_params
                    .iter()
                    .map(|param| Ty::named(*param, vec![]))
                    .collect(),
            },
        );
        self.builtin_call_targets
            .insert(name.to_string(), CallTarget::Runtime(family));
    }

    pub(super) fn resolve_registered_annotation_ty(
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
            crate::module_registry::is_canonical_stdlib_module_source(source, module_name)
        }) {
            self.canonical_std_module_sources
                .insert(module_name.to_string());
        }
    }

    /// The isolated-checker prelude is compiled from the shipped lifecycle
    /// sources. Preserve its intentionally import-free bare spellings, but
    /// make that trust explicit with the same canonical-source proof used for
    /// ordinary imports.
    pub(super) fn record_canonical_lifecycle_prelude_authority(&mut self, module_name: &str) {
        let owner = match module_name {
            "std.failure" => "std.failure",
            "std.link_monitor" => "std.link_monitor",
            _ => return,
        };
        let source = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .expect("hew-types crate has a workspace parent")
            .join("std")
            .join(format!("{}.hew", owner.rsplit('.').next().unwrap_or(owner)));
        if !crate::module_registry::is_canonical_stdlib_module_source(&source, module_name) {
            return;
        }
        let lifecycle_names: &[&str] = match owner {
            "std.failure" => &["CrashInfo", "CrashAction", "CrashNotification", "CrashKind"],
            "std.link_monitor" => &[
                "MonitorId",
                "DownTarget",
                "DownReason",
                "DownNotification",
                "MonitorError",
                "MonitorRef",
            ],
            _ => unreachable!("matched canonical lifecycle owner"),
        };
        for source_name in lifecycle_names {
            self.canonical_lifecycle_import_authority.insert((
                None,
                (*source_name).to_string(),
                format!("{owner}.{source_name}"),
            ));
        }
    }

    /// Complete alias targets once all lexical imports have been published.
    pub(in crate::check) fn resolve_alias_declarations(&mut self, program: &Program) {
        if let Some(graph) = &program.module_graph {
            let indices = graph.file_span_indices();
            for module_id in &graph.topo_order {
                if *module_id == graph.root {
                    continue;
                }
                let Some(module) = graph.modules.get(module_id) else {
                    continue;
                };
                self.current_module = Some(module_id.dotted());
                for (index, (item, span)) in module.items.iter().enumerate() {
                    self.current_module_idx =
                        indices.item_index(module_id, index).unwrap_or_default();
                    if let Item::TypeAlias(decl) = item {
                        self.register_type_alias_decl(decl, span);
                    }
                }
            }
        }
        self.current_module = None;
        self.current_module_idx = 0;
        for (item, span) in &program.items {
            if let Item::TypeAlias(decl) = item {
                self.register_type_alias_decl(decl, span);
            }
        }
        for (name, alias) in self.type_aliases.clone() {
            if self.alias_expansion_is_recursive(&name) {
                let span = self.type_def_spans.get(&name).cloned().unwrap_or_default();
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
        let Some(type_def) = self.type_defs.get(&type_def_key).cloned() else {
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
        if let Some(tb) = &id.trait_bound {
            let trait_key = self.trait_defs_key_for_bound(&tb.path.to_string()); // TRANSITION(P1): deleted by A1 commit 2
            if let Some(trait_info) = self.trait_defs.get(&trait_key) {
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

    pub(in crate::check) fn push_unique_bound(entry: &mut Vec<String>, bound: &str) {
        if !entry.iter().any(|b| b == bound) {
            entry.push(bound.to_string());
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
    /// (e.g. bench.Suite.add) visible to the type checker.
    #[expect(
        clippy::too_many_lines,
        reason = "three-pass registration loop with local_type_defs scoping"
    )]
    pub(in crate::check) fn register_stdlib_hew_items(
        &mut self,
        module_short: &str,
        module_full_path: &str,
        items: &[Spanned<Item>],
        import_spec: StdlibBarePublication<'_>,
    ) {
        // Compiler-embedded and registry-loaded Hew source enters outside the
        // program module graph. Establish its exact declarations in the same
        // table before any semantic registration; aliases and later graph
        // visits resolve the existing module/path rows and cannot mint again.
        let identity_module = self.identity.mint_module(module_full_path, &[]);
        if !self.identity.module_has_declarations(identity_module) {
            for (item_ordinal, (item, span)) in items.iter().enumerate() {
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
        for (item, _span) in items {
            let Item::Import(decl) = item else {
                continue;
            };
            if decl.resolved_items.is_some() {
                // Load the imported stdlib module so its re-exported traits become
                // visible to the eager trait-use path. Pass `None` for the import
                // span deliberately: this import statement lives in a stdlib source
                // file, so its span indexes that file — not the user document the
                // diagnostics are reported against. Recording it in `import_spans`
                // would make it a user-facing unused-import lint candidate whose
                // span cannot be resolved to any user source, mis-attributing a
                // stdlib-internal offset to the user's document.
                let saved_current_module = self.current_module.clone();
                self.current_module = Some(module_short.to_string());
                self.register_import(decl, None);
                self.current_module = saved_current_module;
            }
        }

        self.record_trait_import_bindings(module_short, items);

        // Resolve imported declarations in the defining module's lexical scope.
        let saved_local_type_defs = self.local_type_defs.clone();
        let saved_source_type_defs = self.source_type_defs.clone();
        for (item, _) in items {
            // The program-wide type-parameter harvest in `collect_types` walks
            // `program.module_graph`, which is empty on the registry-only
            // checker path (LSP, browser compiler, inline checks). Without the
            // harvest a declaration's own parameters are unknown while its
            // members resolve, so `pub type ScopeError<E> { primary: E; }`
            // reports `unknown type E` against the module's own source.
            self.collect_item_type_param_names(item);
            if let Item::TypeDecl(td) = item {
                self.local_type_defs.insert(td.name.to_string());
                self.source_type_defs.insert(td.name.to_string());
            }
        }

        // Pass 1: Register types, traits, and functions first
        for (item, span) in items {
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
                        let source_def = self.type_defs.get(&canonical).cloned().or_else(|| {
                            // Direct resolved-item imports have no module-graph
                            // pre-registration. Resolve this declaration in its
                            // own scope and capture its just-written bare def.
                            let saved_importer_module =
                                self.current_module.replace(module_full_path.to_string());
                            self.in_stdlib_registration = true;
                            self.register_type_decl(td);
                            self.in_stdlib_registration = false;
                            let source_def = self.type_defs.get(td.name.name.as_str()).cloned();
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
                    if !self.register_type_namespace_name(
                        Some(module_full_path),
                        td.name.name.as_str(),
                        span,
                    ) {
                        continue;
                    }
                    let saved_importer_module =
                        self.current_module.replace(module_full_path.to_string());
                    self.in_stdlib_registration = true;
                    self.register_type_decl(td);
                    self.in_stdlib_registration = false;
                    let source_def = self.type_defs.get(td.name.name.as_str()).cloned();
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
                    // The importer-scope bare binding obeys the qualified-by-
                    // default gate: `Prelude` (compiled-in bootstrap surfaces)
                    // always publishes bare; a real `import` publishes bare only
                    // on a named/glob/aliased opt-in, exactly like a user module.
                    if let Some(binding) = import_spec.bare_binding(td.name.name.as_str()) {
                        let source_identity = format!("{module_full_path}.{}", td.name);
                        self.publish_stdlib_hew_type_binding(
                            module_short,
                            binding,
                            source_identity,
                            import_spec,
                        );
                    }
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
                    let event_name = format!("{}Event", md.name);
                    // Resolved stdlib items are registered while the importer
                    // is the active checker frame. Re-enter the declaration's
                    // assembled module before building the machine and its
                    // generated event/method signatures, matching the module-
                    // graph pre-registration and ordinary source-body context.
                    let saved_importer_module =
                        self.current_module.replace(module_full_path.to_string());
                    self.register_machine_decl(md, span);
                    let machine_def = self.type_defs.get(md.name.name.as_str()).cloned();
                    let event_def = self.type_defs.get(&event_name).cloned();
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
                    // Bare publication of the machine and its companion event
                    // enum is gated together so a named/glob import exposes both
                    // or neither; `Prelude` publishes both unconditionally.
                    if let Some(binding) = import_spec.bare_binding(md.name.name.as_str()) {
                        let source_identity = format!("{module_full_path}.{}", md.name);
                        self.publish_stdlib_hew_type_binding(
                            module_short,
                            binding,
                            source_identity,
                            import_spec,
                        );
                    }
                    if let Some(binding) = import_spec.bare_binding(&event_name) {
                        let source_identity = format!("{module_full_path}.{event_name}");
                        self.publish_stdlib_hew_type_binding(
                            module_short,
                            binding,
                            source_identity,
                            import_spec,
                        );
                    }
                }
                Item::Trait(tr) => {
                    if let Some(supers) = &tr.super_traits {
                        for super_trait in supers {
                            self.mark_imported_trait_used_for_module_aliases(
                                module_short,
                                &super_trait.path.to_string(), // TRANSITION(P1): deleted by A1 commit 2
                            );
                        }
                    }
                    // Record visibility for all traits (both pub and non-pub) so a
                    // cross-module qualified reference to a non-pub trait produces a
                    // precise E_VISIBILITY at the reference site instead of leaking an
                    // `E_MIR: unknown type` at the MIR boundary. Mirrors the TypeDecl
                    // and Machine registration above; traits share the type-namespace
                    // and the same qualified-reference enforcement path in resolution.
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
                    if !self.register_type_namespace_name(
                        Some(module_full_path),
                        tr.name.name.as_str(),
                        span,
                    ) {
                        continue;
                    }
                    let info = Self::trait_info_from_decl(
                        tr,
                        Some(module_full_path.to_string()),
                        self.current_module_idx,
                    );
                    self.trait_defs.insert(tr.name.to_string(), info.clone());
                    let qualified = format!("{module_full_path}.{}", tr.name);
                    self.trait_defs.insert(qualified, info.clone());
                    // Retain the lexical import surface as a lookup index only;
                    // trait resolution and impl facts select the exact full owner.
                    self.trait_defs
                        .entry(format!("{module_short}.{}", tr.name))
                        .or_insert(info);
                }
                Item::Function(fd) => {
                    let qualified =
                        self.canonical_fn_identity(Some(module_full_path), fd.name.name.as_str());
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
                    let (mut sig, assoc_bindings) = self.build_fn_sig_from_decl_with_assoc(fd);
                    self.current_module = saved_importer_module;
                    sig.params = sig
                        .params
                        .iter()
                        .map(|ty| {
                            self.canonicalize_registry_signature(
                                ty,
                                module_full_path,
                                &sig.type_params,
                            )
                        })
                        .collect();
                    sig.return_type = self.canonicalize_registry_signature(
                        &sig.return_type,
                        module_full_path,
                        &sig.type_params,
                    );
                    if fd.visibility == hew_parser::ast::Visibility::Pub {
                        self.module_fn_exports.insert(qualified.clone());
                    }
                    self.fn_type_param_assoc_bindings
                        .insert(qualified.clone(), assoc_bindings);
                    self.fn_sigs.insert(qualified.clone(), sig);
                    // Mirror user-module named/glob import publication. The
                    // parser has already selected `fd.name`; an alias only
                    // changes the importing binding, never the declaration
                    // identity retained in `import_fn_name_aliases`.
                    if fd.visibility.is_pub() {
                        if let Some(binding) = import_spec.bare_binding(fd.name.name.as_str()) {
                            self.publish_stdlib_hew_function_binding(
                                binding,
                                &format!("{module_full_path}.{}", fd.name),
                                import_spec,
                            );
                        }
                    }
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
                    if !self.register_type_namespace_name(
                        Some(module_short),
                        ad.name.name.as_str(),
                        span,
                    ) {
                        continue;
                    }
                    self.register_actor_base(ad, Some(module_short));
                    if ad.visibility.is_pub() {
                        if let Some(binding) = import_spec.bare_binding(ad.name.name.as_str()) {
                            self.publish_stdlib_hew_type_binding(
                                module_short,
                                binding,
                                format!("{module_full_path}.{}", ad.name),
                                import_spec,
                            );
                        }
                    }
                }
                // Register pub consts from C-backed stdlib modules that also
                // ship Hew source (e.g. `std::misc::log` with `pub const JSON`).
                // `register_user_module` handles this for pure-Hew user modules;
                // this arm mirrors it for the stdlib Hew-source path so that
                // `module.CONST` field access resolves in the type checker via
                // the same `env.lookup_ref("{module}.{field}")` guard in
                // `check_field_access`.
                //
                // SHIM (visibility): non-pub consts are not registered, so a
                // cross-module `module.PRIVATE_CONST` reference fails closed with
                // "module has no exported constant" rather than a dedicated
                // E_VISIBILITY. WHY: const references resolve through the value
                // env / field-access path, which has no visibility-enforcement
                // consult point — unlike traits/actors/types which share the
                // type-reference path. WHEN obsolete: when a const-visibility
                // table + a field-access enforcement consult are added. WHAT the
                // real solution is: record (visibility, decl_module) for every
                // const here and check access_allowed in check_field_access,
                // emitting visibility_violation. Tracked as a follow-on; the
                // current message is already a clean fail-closed diagnostic.
                Item::Const(cd) => {
                    if !cd.visibility.is_pub() {
                        continue;
                    }
                    let ty = self.resolve_registered_annotation_ty_no_holes(&cd.ty);
                    let qualified = format!("{module_full_path}.{}", cd.name);
                    self.env.define(qualified, ty, false);
                }
                _ => {}
            }
        }
        // Pass 2: Register impl methods (after types exist)
        for (item, span) in items {
            if let Item::Impl(id) = item {
                if Self::impl_decl_is_drop_impl(id) {
                    self.report_unsupported_impl_drop(span);
                    continue;
                }
                if let TypeExpr::Named {
                    path: named_path,
                    type_args,
                } = &id.target_type.0
                {
                    let type_name = &named_path.to_string(); // TRANSITION(P1): deleted by A1 commit 2
                    let saved_importer_module =
                        self.current_module.replace(module_full_path.to_string());
                    // Set current_self_type for resolving `Self` in method parameters
                    let prev_self_type = self.current_self_type.take();
                    let self_type_args: Vec<Ty> =
                        self.resolve_impl_target_type_args(id, type_args.as_ref());
                    self.current_self_type = Some((type_name.clone(), self_type_args.clone()));
                    let scope_pushed =
                        self.enter_impl_scope(id, span, Some(type_name.as_str()), false);

                    let primitive_key = id.trait_bound.as_ref().and_then(|_| {
                        self.canonical_primitive_or_builtin_key_for_impl_name(type_name)
                    });
                    // Compiled-in stdlib impls are the origin of the builtin
                    // Result/Option/Vec receiver surfaces. Snapshot their
                    // canonical signatures before a user same-named type can
                    // clobber the colliding bare `Type::<method>` fn_sigs key.
                    let builtin_receiver = crate::lookup_builtin_type(type_name).filter(|b| {
                        matches!(
                            b,
                            BuiltinType::Result | BuiltinType::Option | BuiltinType::Vec
                        )
                    });
                    for method in &id.methods {
                        let sig = self.register_impl_method(
                            type_name,
                            method,
                            id.type_params.as_ref(),
                            id.where_clause.as_ref(),
                            id.trait_bound.as_ref(),
                        );
                        if let Some(builtin) = builtin_receiver {
                            let impl_params: Vec<String> = id
                                .type_params
                                .iter()
                                .flatten()
                                .map(|param| param.name.to_string())
                                .collect();
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
                            }
                        }
                        // Also register on qualified type name
                        let qualified_type = format!("{module_short}.{type_name}");
                        if let Some(td) = self.lookup_type_def_mut(&qualified_type) {
                            td.methods.insert(method.name.to_string(), sig.clone());
                        }
                        if let (Some(canonical), Some(tb)) =
                            (primitive_key.clone(), id.trait_bound.as_ref())
                        {
                            self.record_primitive_trait_impl_self_args(
                                canonical.clone(),
                                &tb.path.to_string(), // TRANSITION(P1): deleted by A1 commit 2
                                self_type_args.clone(),
                                &id.target_type.1,
                            );
                            self.record_primitive_trait_impl_method(
                                canonical,
                                &tb.path.to_string(), // TRANSITION(P1): deleted by A1 commit 2
                                method.name.to_string(),
                                sig,
                            );
                        }
                    }
                    if let Some(tb) = &id.trait_bound {
                        self.mark_imported_trait_used_for_module_aliases(
                            module_short,
                            &tb.path.to_string(),
                        ); // TRANSITION(P1): deleted by A1 commit 2
                        self.record_trait_impl_methods(
                            type_name,
                            &tb.path.to_string(), // TRANSITION(P1): deleted by A1 commit 2
                            id.methods.iter().map(|method| method.name.to_string()),
                        );
                        self.record_trait_impl(type_name, &tb.path.to_string());
                        // TRANSITION(P1): deleted by A1 commit 2
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
        for (item, _span) in items {
            match item {
                Item::TypeDecl(td) => {
                    if let Some(source_def) = self.type_defs.get(td.name.name.as_str()).cloned() {
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
                    let event_name = format!("{}Event", md.name);
                    if let Some(source_def) = self.type_defs.get(md.name.name.as_str()).cloned() {
                        self.register_canonical_type_def(
                            module_full_path,
                            md.name.name.as_str(),
                            &source_def,
                        );
                    }
                    if let Some(source_def) = self.type_defs.get(&event_name).cloned() {
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
        let Some(sig) = self.fn_sigs.get(source_identity).cloned() else {
            // A declaration source that cannot supply its canonical signature
            // must not manufacture an ambient bare function binding.
            return;
        };
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
            if let Some(assoc_bindings) = self
                .fn_type_param_assoc_bindings
                .get(source_identity)
                .cloned()
            {
                self.fn_type_param_assoc_bindings
                    .insert(binding.clone(), assoc_bindings);
            }
            self.fn_sigs.insert(binding.clone(), sig);
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
        // Record this module's own trait import bindings BEFORE any of its trait
        // declarations are registered, so a supertrait edge (`trait Sub: Base`)
        // that names a re-imported `Base` resolves through the chain to the
        // original owner (the H11 fix). Topo order guarantees the owner's def is
        // already registered by the time this module's sub-trait edge is built.
        self.record_trait_import_bindings(module_full_path, items);

        // Match the defining module's lexical scope during registration.
        let saved_local_type_defs = self.local_type_defs.clone();
        let saved_source_type_defs = self.source_type_defs.clone();
        for (item, _) in items {
            if let Item::TypeDecl(td) = item {
                self.local_type_defs.insert(td.name.to_string());
                self.source_type_defs.insert(td.name.to_string());
            }
        }

        let importer_file_idx = self.current_module_idx;
        let importer_item_source = self.current_item_source.clone();
        let importer_item_ordinal = self.current_item_ordinal;
        for (item_idx, (item, span)) in items.iter().enumerate() {
            self.current_item_source = item_source_paths.get(item_idx).cloned();
            self.current_item_ordinal = item_idx;
            let declaring_file_idx = item_source_paths
                .get(item_idx)
                .and_then(|source| self.source_file_span_indices.get(source))
                .copied()
                .unwrap_or(importer_file_idx);
            match item {
                Item::Function(fd) => {
                    let qualified =
                        self.canonical_fn_identity(Some(module_full_path), fd.name.name.as_str());
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

                    // The signature is a source declaration fact even though
                    // this pass is publishing it through an importer.  Build
                    // it in the declaring module's exact scope so a bound such
                    // as `T: Render` resolves to `left.render.Render` (and a
                    // source-local import alias follows that module's binding),
                    // never the importer's bare/surface spelling.
                    let saved_importer_module =
                        self.current_module.replace(module_full_path.to_string());
                    self.current_module_idx = declaring_file_idx;
                    let (sig, assoc_bindings) = self.build_fn_sig_from_decl_with_assoc(fd);
                    self.current_module = saved_importer_module;
                    self.current_module_idx = importer_file_idx;
                    // Only `Pub` functions are module exports: `package fn` must
                    // pass the access-allowed check at every call site, so it must
                    // NOT bypass the check by entering the exports set.  Non-pub
                    // functions are still registered into fn_sigs so the enforcement
                    // check can produce a precise E_VISIBILITY diagnostic.
                    if fd.visibility == hew_parser::ast::Visibility::Pub {
                        self.module_fn_exports.insert(qualified.clone());
                        if spec.is_none() {
                            self.module_fn_exports.insert(surface_qualified.clone());
                        }
                    }
                    self.fn_type_param_assoc_bindings
                        .insert(qualified.clone(), assoc_bindings.clone());
                    self.fn_sigs.insert(qualified.clone(), sig.clone());
                    if spec.is_none() {
                        self.fn_type_param_assoc_bindings
                            .entry(surface_qualified.clone())
                            .or_insert_with(|| assoc_bindings.clone());
                        self.fn_sigs
                            .entry(surface_qualified)
                            .or_insert_with(|| sig.clone());
                    }

                    // Direct resolved-item publication is a second module
                    // registration path used by import processing. Unlike the
                    // normal module-graph signature walk, it constructs the
                    // signature above by hand; publish an intrinsic
                    // declaration here under the declaring module's exact
                    // authority as well. This is intentionally not inferred
                    // from `std.*`: `record_canonical_std_module_source` ran
                    // before this function was entered.
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

                    // Publish selected names as lexical bindings to this declaration.
                    // Private functions cannot be imported bare.
                    if fd.visibility.is_pub()
                        && Self::should_import_name(fd.name.name.as_str(), spec)
                    {
                        let binding_name = Self::resolve_import_name(spec, fd.name.name.as_str())
                            .unwrap_or_else(|| fd.name.to_string());
                        // Preserve the resolver-selected source declaration
                        // identity; the binding itself may be an alias.
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
                Item::Record(decl) => {
                    let canonical = format!("{module_full_path}.{}", decl.name);
                    let saved_module = self.current_module.replace(module_full_path.to_string());
                    self.current_module_idx = declaring_file_idx;
                    self.register_record_decl(decl);
                    if let Some(definition) = self.type_defs.get(&canonical).cloned() {
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
                Item::TypeAlias(decl) => {
                    let saved_module = self.current_module.replace(module_full_path.to_string());
                    self.current_module_idx = declaring_file_idx;
                    self.register_type_alias_decl(decl, span);
                    self.current_module = saved_module;
                    self.current_module_idx = importer_file_idx;
                    if !decl.visibility.is_pub() {
                        continue;
                    }
                    self.record_module_type_export(module_full_path, decl.name.name.as_str());
                    if spec.is_none() {
                        self.record_module_type_export(module_short, decl.name.name.as_str());
                    }
                    if Self::should_import_name(decl.name.name.as_str(), spec) {
                        let binding = Self::resolve_import_name(spec, decl.name.name.as_str())
                            .unwrap_or_else(|| decl.name.to_string());
                        let source = format!("{module_full_path}.{}", decl.name);
                        self.import_type_name_aliases.insert(
                            (
                                self.current_module.clone(),
                                self.current_module_idx,
                                binding.clone(),
                            ),
                            source.clone(),
                        );
                        self.record_published_bare_type(&binding, &source);
                        self.unqualified_to_module.insert(
                            (
                                self.current_module.clone(),
                                self.current_module_idx,
                                binding,
                            ),
                            module_full_path.to_string(),
                        );
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
                        let source_def =
                            self.type_defs.get(&qualified_type).cloned().or_else(|| {
                                let saved_importer_module =
                                    self.current_module.replace(module_full_path.to_string());
                                self.current_module_idx = declaring_file_idx;
                                self.register_type_decl(td);
                                let source_def = self.type_defs.get(td.name.name.as_str()).cloned();
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
                    if !self.register_type_namespace_name(
                        Some(module_full_path),
                        td.name.name.as_str(),
                        span,
                    ) {
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
                    let source_def = self.type_defs.get(td.name.name.as_str()).cloned();
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
                    let event_name = format!("{}Event", md.name);
                    // Qualified authority is always published for the machine
                    // and its companion event enum.
                    let saved_importer_module =
                        self.current_module.replace(module_full_path.to_string());
                    self.current_module_idx = declaring_file_idx;
                    self.register_machine_decl(md, span);
                    let machine_def = self.type_defs.get(md.name.name.as_str()).cloned();
                    let event_def = self.type_defs.get(&event_name).cloned();
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
                    if let Some(supers) = &tr.super_traits {
                        let saved_importer_module =
                            self.current_module.replace(module_full_path.to_string());
                        self.current_module_idx = declaring_file_idx;
                        for super_trait in supers {
                            self.mark_imported_trait_used(
                                Some(module_full_path),
                                &super_trait.path.to_string(), // TRANSITION(P1): deleted by A1 commit 2
                            );
                        }
                        self.current_module = saved_importer_module;
                        self.current_module_idx = importer_file_idx;
                    }
                    // Record visibility for all traits so the enforcement check can
                    // distinguish "private, not accessible" from "unknown symbol".
                    // Use module_full_path (matching TypeDecl/Machine) so cross-package
                    // checks compare full paths. Without this a cross-module qualified
                    // reference to a non-pub trait leaks an `E_MIR: unknown type` at the
                    // MIR boundary instead of a precise E_VISIBILITY at the reference site.
                    let qualified_trait = format!("{module_full_path}.{}", tr.name);
                    let surface_qualified_trait = format!("{module_short}.{}", tr.name);
                    self.type_visibility
                        .entry(qualified_trait.clone())
                        .or_insert((tr.visibility, Some(module_full_path.to_string())));
                    self.type_visibility
                        .entry(surface_qualified_trait)
                        .or_insert((tr.visibility, Some(module_full_path.to_string())));
                    self.type_def_spans
                        .entry(qualified_trait)
                        .or_insert_with(|| span.clone());
                    if !tr.visibility.is_pub() {
                        continue;
                    }
                    let info = Self::trait_info_from_decl(
                        tr,
                        Some(module_full_path.to_string()),
                        declaring_file_idx,
                    );
                    let import_binding = if Self::should_import_name(tr.name.name.as_str(), spec) {
                        let binding_name = Self::resolve_import_name(spec, tr.name.name.as_str())
                            .unwrap_or_else(|| tr.name.to_string());
                        // The unqualified trait binding lands in the *importing*
                        // module's namespace, not the source module's.
                        let importer = self.current_module.clone();
                        if self.register_type_namespace_name(
                            importer.as_deref(),
                            &binding_name,
                            span,
                        ) {
                            Some(binding_name)
                        } else {
                            None
                        }
                    } else {
                        None
                    };

                    // Register under qualified name (e.g. "mymod.Drawable")
                    let qualified = format!("{module_full_path}.{}", tr.name);
                    self.trait_defs.insert(qualified.clone(), info.clone());
                    if spec.is_none() {
                        self.trait_defs
                            .entry(format!("{module_short}.{}", tr.name))
                            .or_insert_with(|| info.clone());
                    }
                    // A whole-module import exposes the trait through the exact
                    // qualified source binding (`alias.Trait`) rather than a
                    // published bare name. Record the checker-owned declaration
                    // IDs under that binding too, so call-target selection never
                    // has to recover an owner from the trait's leaf spelling.
                    let Some(trait_id) = self.require_declaration_path(&qualified, span) else {
                        continue;
                    };
                    let qualified_binding = format!("{module_short}.{}", tr.name);
                    for trait_item in &tr.items {
                        let TraitItem::Method(method) = trait_item else {
                            continue;
                        };
                        let Some(method_id) = self.require_declaration_path(
                            &format!("{}::{}", trait_id.full_path(), method.name),
                            &method.span,
                        ) else {
                            continue;
                        };
                        self.trait_method_ids_by_binding.insert(
                            (
                                self.current_module.clone(),
                                self.current_module_idx,
                                qualified_binding.clone(),
                                method.name.to_string(),
                            ),
                            (trait_id.clone(), method_id),
                        );
                    }

                    // Record super-trait relationships for both qualified and
                    // unqualified bindings. A supertrait reference written inside
                    // the source module (`trait Sub: Base`) is bare in the source
                    // spelling, but it names the trait `module_short` resolves
                    // `Base` to: its own same-package `Base` (`{module_short}.Base`),
                    // OR a re-imported `Base` followed through this module's import
                    // bindings to the original owner. Store the OWNER-QUALIFIED
                    // identity so `trait_super` values are collision-free
                    // `trait_defs` keys. Resolving the bare source spelling in the
                    // IMPORTER's namespace instead is both collision-unsafe (binds
                    // whatever `Base` the importer has) and over-strict (an
                    // import-only-`Sub` would fail to find its supertrait's method
                    // set, falsely rejecting an inline supermethod as extra).
                    if let Some(supers) = &tr.super_traits {
                        let saved_importer_module =
                            self.current_module.replace(module_full_path.to_string());
                        self.current_module_idx = declaring_file_idx;
                        let super_keys: Vec<String> = supers
                            .iter()
                            .map(|s| {
                                self.mark_imported_trait_used(
                                    Some(module_full_path),
                                    &s.path.to_string(),
                                ); // TRANSITION(P1): deleted by A1 commit 2
                                self.resolve_super_trait_edge(module_full_path, &s.path.to_string())
                                // TRANSITION(P1): deleted by A1 commit 2
                            })
                            .collect();
                        self.current_module = saved_importer_module;
                        self.current_module_idx = importer_file_idx;
                        self.trait_super
                            .insert(qualified.clone(), super_keys.clone());
                        if let Some(binding_name) = import_binding.as_ref() {
                            self.trait_super.insert(binding_name.clone(), super_keys);
                        }
                    }

                    // If glob or named import, also register unqualified (using alias if present)
                    if let Some(binding_name) = import_binding {
                        self.trait_defs.insert(binding_name.clone(), info.clone());
                        // Record the SOURCE identity (`module_short.tr.name`) under
                        // the binding so trait-conformance can recover the owner +
                        // original trait name for an aliased import. `qualified` is
                        // the source identity even when `binding_name` is an alias.
                        self.published_bare_trait_owners
                            .entry((
                                self.current_module.clone(),
                                self.current_module_idx,
                                binding_name.clone(),
                            ))
                            .or_default()
                            .insert(format!("{module_full_path}.{}", tr.name));
                        for trait_item in &tr.items {
                            let TraitItem::Method(method) = trait_item else {
                                continue;
                            };
                            let Some(method_id) = self.require_declaration_path(
                                &format!("{}::{}", trait_id.full_path(), method.name),
                                &method.span,
                            ) else {
                                continue;
                            };
                            self.trait_method_ids_by_binding.insert(
                                (
                                    self.current_module.clone(),
                                    self.current_module_idx,
                                    binding_name.clone(),
                                    method.name.to_string(),
                                ),
                                (trait_id.clone(), method_id),
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
                    let qualified = format!("{module_full_path}.{}", cd.name);
                    self.env.define(qualified, ty.clone(), false);
                    if Self::should_import_name(cd.name.name.as_str(), spec) {
                        let binding_name = Self::resolve_import_name(spec, cd.name.name.as_str())
                            .unwrap_or_else(|| cd.name.to_string());
                        self.record_published_bare_const(
                            &binding_name,
                            &format!("{module_full_path}.{}", cd.name),
                        );
                        self.env.define(binding_name, ty, false);
                    }
                }
                Item::Impl(id) => {
                    // This pass publishes an imported module's compatibility
                    // surface.  Its impl registrations must nevertheless run
                    // in the declaring module's scope: using the importer
                    // scope here would mint a second trait/impl identity for
                    // `alias.Type` in addition to the source-owned
                    // `source.module.Type` entry recorded by the module-graph
                    // traversal.  The temporary scope makes this a harmless
                    // canonical re-registration for graph-backed sources and
                    // the sole canonical registration for resolved sources
                    // which are not represented in the graph.
                    let saved_importer_module =
                        self.current_module.replace(module_full_path.to_string());
                    self.current_module_idx = declaring_file_idx;
                    if let TypeExpr::Named {
                        path: named_path,
                        type_args,
                    } = &id.target_type.0
                    {
                        let type_name = &named_path.to_string(); // TRANSITION(P1): deleted by A1 commit 2
                                                                 // Set current_self_type for resolving `Self` in method parameters
                        let prev_self_type = self.current_self_type.take();
                        let self_type_args: Vec<Ty> =
                            self.resolve_impl_target_type_args(id, type_args.as_ref());
                        self.current_self_type = Some((type_name.clone(), self_type_args.clone()));
                        let scope_pushed =
                            self.enter_impl_scope(id, span, Some(type_name.as_str()), false);

                        let primitive_key = id.trait_bound.as_ref().and_then(|_| {
                            self.canonical_primitive_or_builtin_key_for_impl_name(type_name)
                        });
                        for method in &id.methods {
                            // Register the declaring module's complete method
                            // set. Visibility is enforced when a caller selects
                            // a method; dropping non-pub methods here also breaks
                            // source-local calls and public values returned from
                            // the module with an incomplete canonical TypeDef.
                            let sig = self.register_impl_method(
                                type_name,
                                method,
                                id.type_params.as_ref(),
                                id.where_clause.as_ref(),
                                id.trait_bound.as_ref(),
                            );
                            if let (Some(canonical), Some(tb)) =
                                (primitive_key.clone(), id.trait_bound.as_ref())
                            {
                                self.record_primitive_trait_impl_self_args(
                                    canonical.clone(),
                                    &tb.path.to_string(), // TRANSITION(P1): deleted by A1 commit 2
                                    self_type_args.clone(),
                                    &id.target_type.1,
                                );
                                self.record_primitive_trait_impl_method(
                                    canonical,
                                    &tb.path.to_string(), // TRANSITION(P1): deleted by A1 commit 2
                                    method.name.to_string(),
                                    sig,
                                );
                            }
                        }
                        if let Some(tb) = &id.trait_bound {
                            self.record_trait_impl(type_name, &tb.path.to_string());
                            // TRANSITION(P1): deleted by A1 commit 2
                        }

                        // Restore previous self type
                        self.current_self_type = prev_self_type;
                        if scope_pushed {
                            self.exit_impl_scope();
                        }
                    }
                    self.current_module = saved_importer_module;
                    self.current_module_idx = importer_file_idx;
                }
                Item::Actor(ad) => {
                    // Record visibility for all actors so a cross-module qualified
                    // reference to a non-pub actor produces a precise E_VISIBILITY at
                    // the reference site instead of leaking an `E_MIR: unknown type` at
                    // the MIR boundary. Use module_full_path (matching TypeDecl/Machine/
                    // Trait) so cross-package checks compare full paths.
                    let qualified_actor = format!("{module_full_path}.{}", ad.name);
                    self.type_visibility
                        .entry(qualified_actor.clone())
                        .or_insert((ad.visibility, Some(module_full_path.to_string())));
                    self.type_def_spans
                        .entry(qualified_actor.clone())
                        .or_insert_with(|| span.clone());
                    // Skip non-pub actors (enforce visibility), matching every
                    // other item kind in this loop. A private actor must never
                    // become a module type export, qualified alias, or registered
                    // base in the importer's view: otherwise `spawn module.Account()`
                    // would accept a private target and -- after the qualifier is
                    // stripped to the bare name in HIR -- silently route to a
                    // same-named root/pub actor. This `Item::Actor` arm was the
                    // lone exporter that ignored `pub`, recording private actors in
                    // `module_type_exports` (the authoritative export registry).
                    if !ad.visibility.is_pub() {
                        continue;
                    }
                    let actor_already_registered = self.type_defs.contains_key(&qualified_actor);
                    if !actor_already_registered
                        && !self.register_type_namespace_name(
                            Some(module_full_path),
                            ad.name.name.as_str(),
                            span,
                        )
                    {
                        continue;
                    }
                    // `register_actor_base` authors the source-owned dotted
                    // `{module_full_path}.{name}` identity directly; no bare key
                    // and no copy-based qualified alias.
                    //
                    // Receive-fn reply types register BARE, then a collision-gated
                    // transform owner-qualifies ONLY the cross-module-colliding
                    // record names in each reply — recursing exactly as HIR's
                    // `qualify_colliding_module_record_ty` does. Scoping
                    // `current_module` for the whole registration instead would
                    // qualify EVERY nested local type: a reply
                    // `Result<Unique, Colliding>` becomes
                    // `Result<pkg.Unique, pkg.Colliding>` in checker state while
                    // HIR qualifies only `Colliding`, so MIR's actor-reply
                    // equality rejects the divergence (#2208). Qualifying only
                    // the colliding name keeps the checker, HIR, and the MIR
                    // record-layout keying (`type_layout_key` / `collided_type_names`)
                    // in lockstep: a colliding reply record takes its owner
                    // identity (`testffi.Result`), a unique one (`http.Response`,
                    // `xml.Node`, the `Unique` above) keeps its bare identity so
                    // its layout stays bare too and never SIGSEGVs at cabi.rs.
                    // Rebuild the actor's signatures even when a pre-pass
                    // already reserved/authored its TypeDef. Some resolved-item
                    // pre-registration paths run in the importer scope; the
                    // source-scoped write here is what makes sibling record
                    // references and handler keys canonical.
                    let saved_importer_module =
                        self.current_module.replace(module_full_path.to_string());
                    self.current_module_idx = declaring_file_idx;
                    self.register_actor_base(ad, Some(module_full_path));
                    self.current_module = saved_importer_module;
                    self.current_module_idx = importer_file_idx;
                    self.qualify_colliding_receive_reply_tys(ad, module_full_path);
                    // The source owner is the authoritative export key.  Keep
                    // the lexical module binding as a compatibility index for
                    // parsing `alias.Actor`, but never use it as the actor's
                    // declaration identity.
                    self.record_module_type_export(module_full_path, ad.name.name.as_str());
                    // If named import or glob, also register unqualified
                    if Self::should_import_name(ad.name.name.as_str(), spec) {
                        let binding_name = Self::resolve_import_name(spec, ad.name.name.as_str())
                            .unwrap_or_else(|| ad.name.to_string());
                        let source_identity = format!("{module_full_path}.{}", ad.name);
                        self.record_published_bare_type(&binding_name, &source_identity);
                        // Supervisor declarations retain their source spelling
                        // through parsing. Publish the same exact lexical
                        // binding fact every other named/glob type import gives
                        // HIR, so a child named by either `Worker` or an `as`
                        // alias reaches the actor's declaration-owned key.
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
                    // A module supervisor registers under the same dotted
                    // identity the resolver mints for it, exactly as a module
                    // actor does. Without this the declaration reached no
                    // checker table at all and `spawn Inner` fell through to a
                    // bare echo that SIR could not match to its declaration.
                    self.reject_wasm_feature(span, WasmUnsupportedFeature::SupervisionTrees);
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
                    if !self.type_defs.contains_key(&qualified)
                        && !self.register_type_namespace_name(
                            Some(module_full_path),
                            sd.name.name.as_str(),
                            span,
                        )
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
                Item::ExternBlock(eb) => {
                    // Resolved-item imports are not guaranteed to have a
                    // module-graph collection pass. Register their extern
                    // declarations in the source module's exact scope so both
                    // signature keys and bare sibling types in those
                    // signatures carry the declaring owner.
                    let has_unregistered_signature = eb.functions.iter().any(|function| {
                        !self
                            .fn_sigs
                            .contains_key(&format!("{module_full_path}.{}", function.name))
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
                Item::Import(_) => {}
            }
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
                    if let Some(source_def) = self.type_defs.get(td.name.name.as_str()).cloned() {
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
                    let event_name = format!("{}Event", md.name);
                    if let Some(source_def) = self.type_defs.get(md.name.name.as_str()).cloned() {
                        self.register_canonical_type_def(
                            module_full_path,
                            md.name.name.as_str(),
                            &source_def,
                        );
                    }
                    if let Some(source_def) = self.type_defs.get(&event_name).cloned() {
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

    /// Owner-qualify the cross-module-colliding record names in every receive
    /// fn's already-registered reply type, in place. Runs after
    /// `register_actor_base` for an imported actor so the checker's stored reply
    /// identity matches HIR's collision-gated handler transform and MIR's
    /// actor-reply equality (#2208).
    pub(super) fn qualify_colliding_receive_reply_tys(
        &mut self,
        ad: &ActorDecl,
        module_short: &str,
    ) {
        let identity = Self::actor_identity(Some(module_short), ad.name.name.as_str());
        for rf in &ad.receive_fns {
            let method_name = format!("{identity}::{}", rf.name);
            let Some(current) = self
                .fn_sigs
                .get(&method_name)
                .map(|s| s.return_type.clone())
            else {
                continue;
            };
            let qualified = self.qualify_colliding_reply_ty(&current, module_short);
            if qualified != current {
                if let Some(sig) = self.fn_sigs.get_mut(&method_name) {
                    sig.return_type = qualified;
                }
            }
        }
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
        let identity = Self::actor_identity(module_short, ad.name.name.as_str());
        self.register_actor_decl_as(ad, &identity);
        self.known_types.insert(identity.clone());
        // A generic actor's own params (`actor Worker<T>`) are in scope for every
        // receive-fn / method signature. Push them and register these primary
        // signatures scope-locally so an out-of-scope generic name is rejected at
        // the annotation, while the actor's legitimate `<T>` still resolves. This
        // mirrors the inline `Item::Actor` path for imported / flat-file actors.
        let actor_sig_scope = self.enter_primary_sig_scope(&[(Some(&ad.type_params), None)]);
        for rf in &ad.receive_fns {
            self.register_receive_fn(&identity, rf);
        }
        for method in &ad.methods {
            let method_name = format!("{identity}::{}", method.name);
            self.register_fn_sig_with_name(&method_name, method);
        }
        self.exit_primary_sig_scope(actor_sig_scope);
    }

    /// Seed the module-qualified marker-derivation alias for a type declared
    /// in a non-root module, immediately after its bare registration.
    ///
    /// The trait registry keys marker derivation by name; the bare key is
    /// last-write-wins across modules. Two imported packages that each export a
    /// type named `Reply` collide on the single bare `"Reply"` key, so a Send
    /// lookup at the ask-reply gate can read the wrong module's fields. The
    /// importer qualifies the dispatched actor's reply type as
    /// `{module_short}.{name}` (matching `actor_identity`), so the qualified
    /// registry alias gives the gate a collision-free identity to look up.
    ///
    /// Unlike `register_qualified_type_alias` (pub-only, import-surface), this
    /// runs for EVERY type a non-root module declares — including the non-pub
    /// records reachable only as an actor's `receive fn` reply type (the
    /// `testffi` fixture's `type Result` is one such non-pub reply). Bare
    /// lookups are unchanged; root / flat-file types (no `current_module`) are a
    /// no-op.
    pub(super) fn seed_qualified_type_markers_for_current_module(&mut self, name: &str) {
        if let Some(module_short) = self.current_module_identity() {
            let qualified = format!("{module_short}.{name}");
            self.registry.alias_type_markers(name, &qualified);
        }
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
        if let Some(def) = self.type_defs.get(name).cloned() {
            // The bare entry is last-write-wins across modules, while an existing
            // qualified entry is that module's authority. Cross-module import
            // registration must not overwrite it with another module's bare
            // winner. The owning module may overwrite its own qualified entry
            // during Pass 1.5 member re-resolution, when the bare definition has
            // just been upgraded after import aliases became available.
            let owns_qualified = self.current_module_identity() == Some(module_short);
            let write_alias = owns_qualified || !self.type_defs.contains_key(&qualified);
            if !write_alias {
                return;
            }
            self.type_defs.insert(qualified.clone(), def);
            if let Some(span) = self.type_def_spans.get(name).cloned() {
                self.type_def_spans.insert(qualified.clone(), span);
            }
            // Mirror the marker-derivation tables under the qualified key. The
            // bare `type_fields` (and sibling member maps) are last-write-wins
            // across modules — two packages each exporting `Reply` collide on
            // the single bare key, so a same-bare-name reply derives `Send`
            // from whichever module won the race. The qualified alias gives the
            // Send gate a collision-free identity to look up.
            self.registry.alias_type_markers(name, &qualified);
            self.handle_bearing_dirty = true;
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
        let mut published = source_def.clone();
        published.fields = published
            .fields
            .iter()
            .map(|(field, ty)| {
                (
                    field.clone(),
                    self.qualify_source_member_ty(module_full_path, ty, &source_def.type_params),
                )
            })
            .collect();
        published.variants = published
            .variants
            .iter()
            .map(|(variant, definition)| {
                (
                    variant.clone(),
                    self.qualify_source_variant_def(
                        module_full_path,
                        definition,
                        &source_def.type_params,
                    ),
                )
            })
            .collect();
        if let Some(existing) = self.type_defs.get(&qualified) {
            for (method_name, method_sig) in &existing.methods {
                published
                    .methods
                    .entry(method_name.clone())
                    .or_insert_with(|| method_sig.clone());
            }
        }
        self.type_defs.insert(qualified.clone(), published.clone());
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
        let members = if published.kind == TypeDefKind::Enum {
            Self::structural_member_types_for_type(&published)
        } else if published.kind == TypeDefKind::Record && published.fields.is_empty() {
            self.fn_sigs
                .get(&qualified)
                .map_or_else(Vec::new, |signature| signature.params.clone())
        } else {
            published.fields.values().cloned().collect()
        };
        let members = self.expand_for_marker_registration(&members);
        self.registry.register_type(qualified.clone(), members);
        self.registry
            .register_type_params(qualified.clone(), published.type_params.clone());
        self.handle_bearing_dirty = true;
    }

    /// A source declaration's members are registered while their module owns
    /// bare sibling names. Preserve that owner when publishing the declaration
    /// for an importer. Later checker phases run in the importer's scope, where
    /// a bare sibling can instead denote a builtin or an unimported export.
    pub(super) fn qualify_source_member_ty(
        &self,
        module_full_path: &str,
        ty: &Ty,
        parameters: &[String],
    ) -> Ty {
        let qualified_children = ty.map_children_pub(&|child| {
            self.qualify_source_member_ty(module_full_path, child, parameters)
        });
        let Ty::Named {
            name,
            args,
            builtin,
        } = qualified_children
        else {
            return qualified_children;
        };
        if name.contains('.') || parameters.contains(&name) {
            return Ty::Named {
                name,
                args,
                builtin,
            };
        }
        let canonical = format!("{module_full_path}.{name}");
        if self.type_defs.contains_key(&canonical) {
            Ty::named(canonical, args)
        } else {
            Ty::Named {
                name,
                args,
                builtin,
            }
        }
    }

    pub(super) fn qualify_source_variant_def(
        &self,
        module_full_path: &str,
        definition: &VariantDef,
        parameters: &[String],
    ) -> VariantDef {
        match definition {
            VariantDef::Unit => VariantDef::Unit,
            VariantDef::Tuple(fields) => VariantDef::Tuple(
                fields
                    .iter()
                    .map(|field| self.qualify_source_member_ty(module_full_path, field, parameters))
                    .collect(),
            ),
            VariantDef::Struct(fields) => VariantDef::Struct(
                fields
                    .iter()
                    .map(|(field, ty)| {
                        (
                            field.clone(),
                            self.qualify_source_member_ty(module_full_path, ty, parameters),
                        )
                    })
                    .collect(),
            ),
        }
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
