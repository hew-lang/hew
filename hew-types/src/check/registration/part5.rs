//! Split from `registration.rs`: checker methods, part 5 of 6.
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
    /// Diagnose protected-prelude declarations in every non-root module once,
    /// including private declarations that are never published by an import.
    pub(in crate::check) fn reject_non_root_protected_prelude_declarations(
        &mut self,
        program: &Program,
    ) {
        let Some(module_graph) = &program.module_graph else {
            return;
        };
        for module_id in &module_graph.topo_order {
            if *module_id == module_graph.root {
                continue;
            }
            let Some(module) = module_graph.modules.get(module_id) else {
                continue;
            };
            let owner = module_id.path.join(".");
            for (item, span) in &module.items {
                let name = match item {
                    Item::Const(item) => Some(item.name.as_str()),
                    Item::TypeDecl(item) => Some(item.name.as_str()),
                    Item::TypeAlias(item) => Some(item.name.as_str()),
                    Item::Trait(item) => Some(item.name.as_str()),
                    Item::Function(item) => Some(item.name.as_str()),
                    Item::Actor(item) => Some(item.name.as_str()),
                    Item::Supervisor(item) => Some(item.name.as_str()),
                    Item::Machine(item) => Some(item.name.as_str()),
                    Item::Record(item) => Some(item.name.as_str()),
                    Item::Import(_) | Item::Impl(_) | Item::ExternBlock(_) => None,
                };
                if let Some(name) = name {
                    self.reject_protected_prelude_declaration_for_owner(Some(&owner), name, span);
                }
            }
        }
    }

    pub(super) fn import_publication_candidates(decl: &ImportDecl) -> Vec<(String, String)> {
        let mut candidates = Vec::new();
        let module_binding = decl
            .module_alias
            .clone()
            .or_else(|| decl.path.last().cloned());
        if let Some(binding) = module_binding {
            candidates.push((binding.clone(), binding));
        }

        if let Some(ImportSpec::Names(names)) = &decl.spec {
            candidates.extend(names.iter().map(|name| {
                (
                    name.alias.clone().unwrap_or_else(|| name.name.clone()),
                    name.name.clone(),
                )
            }));
            return candidates;
        }

        if !decl.path.is_empty() {
            return candidates;
        }

        let Some(items) = decl.resolved_items.as_ref() else {
            return candidates;
        };
        for (item, _) in items.iter() {
            let mut push = |name: &str| candidates.push((name.to_string(), name.to_string()));
            match item {
                Item::Function(item) if item.visibility.is_pub() => push(&item.name),
                Item::Const(item) if item.visibility.is_pub() => push(&item.name),
                Item::TypeDecl(item) if item.visibility.is_pub() => push(&item.name),
                Item::TypeAlias(item) if item.visibility.is_pub() => push(&item.name),
                Item::Trait(item) if item.visibility.is_pub() => push(&item.name),
                Item::Actor(item) if item.visibility.is_pub() => push(&item.name),
                Item::Machine(item) if item.visibility.is_pub() => {
                    push(&item.name);
                    push(&format!("{}Event", item.name));
                }
                Item::Record(item) if item.visibility.is_pub() => push(&item.name),
                _ => {}
            }
        }
        candidates
    }

    pub(super) fn preflight_import_publication(&mut self, decl: &ImportDecl, span: &Span) -> bool {
        let candidates = Self::import_publication_candidates(decl);
        let source_owner = if decl.path.is_empty() {
            decl.file_path.as_deref().map_or_else(
                || "<file-import>".to_string(),
                |path| format!("file:{path}"),
            )
        } else {
            decl.path.join(".")
        };
        let mut seen = HashSet::new();
        let mut valid = true;

        for (binding, source_name) in &candidates {
            if !seen.insert(binding.clone()) {
                self.errors.push(TypeError::new(
                    TypeErrorKind::ImportBindingCollision,
                    span.clone(),
                    format!(
                        "import publishes `{binding}` more than once; no bindings from this import were added"
                    ),
                ));
                valid = false;
                continue;
            }

            if let Some(prelude_owner) = self.protected_prelude_bindings.get(binding) {
                let reimports_same_prelude_item =
                    prelude_owner == &source_owner && source_name == binding;
                if !reimports_same_prelude_item {
                    self.errors.push(TypeError::new(
                        TypeErrorKind::ImportPreludeCollision,
                        span.clone(),
                        format!(
                            "import binding `{binding}` collides with the protected prelude; no bindings from this import were added"
                        ),
                    ));
                    valid = false;
                    continue;
                }
            }

            let key = (
                self.current_module.clone(),
                self.current_module_idx,
                binding.clone(),
            );
            let source_identity = format!("{source_owner}.{source_name}");
            if let Some((previous_span, previous_module, previous_identity)) =
                self.import_binding_spans.get(&key).cloned()
            {
                if previous_identity == source_identity {
                    continue;
                }
                self.errors.push(
                    TypeError::new(
                        TypeErrorKind::ImportBindingCollision,
                        span.clone(),
                        format!(
                            "import binding `{binding}` is already defined in this file; no bindings from this import were added"
                        ),
                    )
                    .with_note_source(
                        previous_span,
                        "previous import binding here",
                        previous_module,
                    ),
                );
                valid = false;
            }
        }

        if valid {
            for (binding, source_name) in candidates {
                let source_identity = format!("{source_owner}.{source_name}");
                self.import_binding_spans.insert(
                    (
                        self.current_module.clone(),
                        self.current_module_idx,
                        binding,
                    ),
                    (span.clone(), self.current_module.clone(), source_identity),
                );
            }
        }
        valid
    }

    #[expect(
        clippy::too_many_lines,
        reason = "import registration consolidates stdlib, user-module, and error paths in one place"
    )]
    pub(in crate::check) fn register_import(
        &mut self,
        decl: &ImportDecl,
        import_span: Option<&Span>,
    ) {
        if import_span.is_some_and(|span| !self.preflight_import_publication(decl, span)) {
            return;
        }
        let mut resolved_module_owner: Option<String> = None;
        if let Some(items) = decl.resolved_items.as_ref() {
            let requested_owner = if decl.path.is_empty() {
                decl.file_path
                    .as_deref()
                    .and_then(|path| std::path::Path::new(path).file_stem())
                    .and_then(std::ffi::OsStr::to_str)
                    .unwrap_or("file")
                    .to_string()
            } else {
                decl.path.join(".")
            };
            let primary = self.identity.mint_module(
                &crate::module_registry::canonical_source_module_identity(
                    &requested_owner,
                    &decl.resolved_source_paths,
                ),
                &decl.resolved_source_paths,
            );
            // The identity table interns by canonical source, so a module the
            // compile already reached under another spelling answers with the
            // render it was minted under. That render is the one owner every
            // registration below keys by; the requested spelling is only how
            // this importer wrote it.
            let owner = self.identity.module_path(primary).to_string();
            resolved_module_owner = Some(owner.clone());
            for source in decl.resolved_source_paths.iter().skip(1) {
                self.identity.mint_source_file_module(&owner, source);
            }
            // A file import (`import "helper.hew";`, empty path) flattens its
            // items into the root's namespace, so its nominals answer bare as
            // well as under their own file. A module import
            // (`import pkg.alpha;`) does not: its nominals stay qualified by
            // the owning module, so two modules exporting the same leaf keep
            // distinct declarations.
            let namespace = crate::check::NominalNamespace::for_import(decl.path.is_empty());
            if !self.identity.module_has_declarations(primary) {
                for (index, (item, span)) in items.iter().enumerate() {
                    let module = decl
                        .resolved_item_source_paths
                        .get(index)
                        .and_then(|source| self.identity.module_for_source(source))
                        .unwrap_or(primary);
                    self.mint_item_declaration_identities(
                        Some(module),
                        Some(primary),
                        namespace,
                        index,
                        item,
                        span,
                    );
                }
            }
        }
        let module_path = decl.path.join(".");

        // Try to load from the registry first, keeping any error detail owned so the
        // `self.module_registry` borrow ends before we mutate `self.errors`.
        let load_error_detail: Option<String> = match self.module_registry.load(&module_path) {
            Ok(info) => {
                if info.unsupported_type_signatures.is_empty() {
                    // Clone all data from ModuleInfo before mutating self, because
                    // info borrows from self.module_registry.
                    let functions = info.functions.clone();
                    let wrapper_fns = info.wrapper_fns.clone();
                    let clean_names = info.clean_names.clone();
                    let handle_types = info.handle_types.clone();
                    let resource_wrapper_types = info.resource_wrapper_types.clone();
                    let drop_types = info.drop_types.clone();
                    let resolved_source_path = info.source_path.clone();
                    let registry_source_items = info.source_items.clone();

                    let requested_owner = module_path.clone();
                    // The resolved import is the authority for which module
                    // this is: the registry's own `source_path` can be one PEER
                    // file of a directory module, and that file has its own
                    // per-file identity, so minting from it would answer with
                    // the file rather than the module it assembles into.
                    let canonical_owner = resolved_module_owner.clone().unwrap_or_else(|| {
                        resolved_source_path.as_ref().map_or_else(
                            || requested_owner.clone(),
                            |source_path| {
                                crate::module_registry::canonical_source_module_identity(
                                    &requested_owner,
                                    std::slice::from_ref(source_path),
                                )
                            },
                        )
                    });
                    let registry_module = self
                        .identity
                        .mint_module(&canonical_owner, resolved_source_path.as_slice());
                    if let Some(source_path) = resolved_source_path {
                        self.record_canonical_std_module_source(
                            &canonical_owner,
                            std::slice::from_ref(&source_path),
                        );
                    }
                    // Preserve the importer's lexical module binding even
                    // when the selected source belongs to a directory
                    // module whose canonical owner has a different leaf
                    // (`http_client` -> `http`). The binding maps to the
                    // canonical owner below; it must not be renamed by
                    // source canonicalisation.
                    let requested_short = module_path
                        .rsplit('.')
                        .next()
                        .unwrap_or(&module_path)
                        .to_string();
                    let short = decl.module_alias.clone().unwrap_or(requested_short);

                    // Registry-backed stdlib imports can also carry a resolved
                    // Hew source surface.  Grant lifecycle authority only when
                    // that surface identifies the exact shipped source file;
                    // the registry's successful lookup by module spelling is
                    // not itself provenance proof.
                    let importer = self.current_module.clone();
                    self.record_canonical_lifecycle_import_authority(decl, importer.as_deref());

                    // Register extern C function signatures
                    for func in functions {
                        let sig = FnSig {
                            params: func
                                .params
                                .iter()
                                .map(|ty| {
                                    self.canonicalize_registry_signature(ty, &canonical_owner, &[])
                                })
                                .collect(),
                            return_type: self.canonicalize_registry_signature(
                                &func.return_type,
                                &canonical_owner,
                                &[],
                            ),
                            ..FnSig::default()
                        };
                        self.declare_contractless_extern(
                            registry_module,
                            &canonical_owner,
                            &func.name,
                        );
                        self.fn_sigs.insert(func.name, sig);
                    }

                    // Register wrapper pub fn signatures
                    for wfn in wrapper_fns {
                        let sig = FnSig {
                            params: wfn
                                .params
                                .iter()
                                .map(|ty| {
                                    self.canonicalize_registry_signature(
                                        ty,
                                        &canonical_owner,
                                        &wfn.type_params,
                                    )
                                })
                                .collect(),
                            return_type: self.canonicalize_registry_signature(
                                &wfn.return_type,
                                &canonical_owner,
                                &wfn.type_params,
                            ),
                            type_params: wfn.type_params,
                            type_param_bounds: wfn.type_param_bounds,
                            ..FnSig::default()
                        };
                        // Wrapper functions belong to the imported module;
                        // publishing them under a bare leaf here would make
                        // `import m::{ f as alias }` accidentally retain an
                        // ambient `f` binding. Selected bare bindings are
                        // published later from the resolved source surface.
                        self.fn_sigs
                            .insert(format!("{canonical_owner}.{}", wfn.name), sig);
                    }

                    // Register module and clean names
                    self.modules.insert(short.clone());
                    self.module_import_bindings.insert(
                        (
                            self.current_module.clone(),
                            self.current_module_idx,
                            short.clone(),
                        ),
                        canonical_owner.clone(),
                    );
                    if let Some(span) = import_span {
                        self.import_spans.insert(
                            ImportKey::in_file(
                                self.current_module.clone(),
                                self.current_module_idx,
                                short.clone(),
                            ),
                            (span.clone(), self.current_module.clone()),
                        );
                    }
                    for (method, c_symbol) in &clean_names {
                        // Prefer the wrapper function's own signature (registered under
                        // the method name) over the extern C function's signature.
                        // E.g. `log.setup()` should have 0 params (the wrapper's sig),
                        // not 1 param (the extern `hew_log_set_level(level)` sig).
                        let key = format!("{canonical_owner}.{method}");
                        let wrapper_sig = self.fn_sigs.get(&key).cloned();
                        let sig = wrapper_sig
                            .clone()
                            .or_else(|| self.fn_sigs.get(c_symbol.as_str()).cloned());
                        if let Some(sig) = sig {
                            // Module functions are source declarations, so their
                            // registry authority is the exact full module owner.
                            // The lexical `short`/alias is resolved through
                            // `module_import_bindings` at each call site.
                            self.module_fn_exports.insert(key.clone());
                            // A parsed Hew declaration already registered under
                            // the canonical owner is the type authority. Registry
                            // clean-name metadata is a lookup surface and must not
                            // replace that declaration with a legacy short-owner
                            // signature (`net.NetError` versus
                            // `std.net.NetError`). Only fill a genuinely absent
                            // canonical slot from the wrapper/extern registry.
                            self.fn_sigs.entry(key.clone()).or_insert(sig);
                            if wrapper_sig.is_none() {
                                // The clean name resolved straight to the C
                                // function: the call is an FFI call and keeps
                                // its `unsafe` gate under the canonical key.
                                self.declare_contractless_extern(
                                    registry_module,
                                    &canonical_owner,
                                    &key,
                                );
                            }
                        }
                    }

                    // Register opaque handles and fielded resource wrappers so
                    // registry-only imports can use either in type annotations.
                    //
                    // Under the loaded module's CANONICAL owner, never the
                    // extracted short spelling (rc1-F1 stage D, registry
                    // producer). Publishing `stream.Sink` beside the source
                    // module's `std.stream.Sink` puts two owners on one
                    // declaration, and every later unique-owner resolution then
                    // reads that as an ambiguity and leaves the name as written.
                    let canonical_known_types = handle_types
                        .iter()
                        .chain(&resource_wrapper_types)
                        .map(|type_name| {
                            self.resolve_nominal_declaration(
                                NominalOrigin::RegistrySignature {
                                    canonical_owner: &canonical_owner,
                                },
                                type_name,
                            )
                            .unwrap_or_else(|| type_name.clone())
                        })
                        .collect::<Vec<_>>();
                    self.known_types.extend(canonical_known_types);

                    // Populate TraitRegistry with handle/drop types
                    for ht in &handle_types {
                        self.registry.register_handle_type(ht.clone());
                    }
                    for dt in &drop_types {
                        self.registry.register_drop_type(dt.clone());
                    }
                    for resource in &resource_wrapper_types {
                        self.registry.register_resource_type(resource.clone());
                        if let Some((_, leaf)) = resource.rsplit_once('.') {
                            self.registry
                                .register_resource_type(format!("{canonical_owner}.{leaf}"));
                        }
                    }

                    // An import needs the Hew declaration, not just the
                    // registry ABI summary: the declaration is the authority
                    // for a bare binding's original source identity and it is
                    // the only place a module's `impl Trait for T` blocks
                    // exist.  Which declarations a module owns is a property
                    // of the module, not of how the importer spelled the
                    // import, so a bare `import std.net;` registers the same
                    // source items a selected `import std.net.{NetError};`
                    // does.  Bare-name publication stays gated on the spec in
                    // `publish_imported_hew_bindings`, so a plain module import
                    // still publishes only the `net.` namespace.
                    let resolved_items = decl.resolved_items.as_deref().or_else(|| {
                        (!registry_source_items.is_empty()).then_some(&registry_source_items)
                    });
                    if let Some(resolved_items) = resolved_items.filter(|items| !items.is_empty()) {
                        let module_full_path = canonical_owner.clone();
                        self.register_resolved_stdlib_hew_source(
                            decl,
                            &module_full_path,
                            &short,
                            &module_full_path,
                            resolved_items,
                            StdlibBarePublication::Import(&decl.spec),
                        );
                    }

                    self.handle_bearing_dirty = true;
                    return;
                }
                Some(format!(
                    "module file contains unsupported slice annotations in signature(s): {}. \
                     slice type composite lowering is not yet implemented",
                    info.unsupported_type_signatures.join(", ")
                ))
            }
            Err(ModuleError::NotFound { .. }) => {
                Some("module not found in any search path".to_string())
            }
            Err(ModuleError::ParseError {
                ref file_path,
                line,
                column,
                ref message,
                ..
            }) => Some(format!(
                "module file `{}` has parse error at {line}:{column}: {message}",
                file_path.display()
            )),
        };
        // `self.module_registry` borrow is released here.

        // --- User module path ---
        if let Some(ref resolved_items) = decl.resolved_items {
            if decl.path.is_empty() {
                if self.flat_file_import_already_registered(decl) {
                    return;
                }
                let owner = resolved_module_owner
                    .clone()
                    .unwrap_or_else(|| self.current_module.clone().unwrap_or_default());
                self.register_file_import_items(&owner, resolved_items);
            } else {
                // Lifecycle nominal identities require stronger provenance than
                // the ordinary resolved-item surface: only an exact canonical
                // stdlib source path can grant this import binding authority.
                let importer = self.current_module.clone();
                self.record_canonical_lifecycle_import_authority(decl, importer.as_deref());
                // The qualifier a bare reference reaches this module's names
                // through. A whole-module alias (`import path as m;`) overrides
                // the default last-segment qualifier, so the module's qualified
                // keys, exports, and importer bindings are all keyed on `m`.
                let short = decl
                    .module_alias
                    .clone()
                    .unwrap_or_else(|| decl.path.last().expect("import path is non-empty").clone());
                let full_dot_path = resolved_module_owner
                    .clone()
                    .unwrap_or_else(|| decl.path.join("."));
                // `resolved_items` can be supplied directly by a module
                // loader without a separately traversed graph node. Preserve
                // the same source-derived authority before publishing its
                // function metadata into the importer.
                self.record_canonical_std_module_source(
                    &full_dot_path,
                    &decl.resolved_source_paths,
                );
                self.modules.insert(short.clone());
                self.modules.insert(full_dot_path.clone());
                self.user_modules.insert(short.clone());
                self.user_modules.insert(full_dot_path.clone());
                // Selective imports retain the lexical module spelling for
                // qualified siblings (`m.Type`) just as whole-module imports
                // do. The table maps that surface binding to the exact source
                // owner; it never grants a short-name fallback.
                self.module_import_bindings.insert(
                    (
                        self.current_module.clone(),
                        self.current_module_idx,
                        short.clone(),
                    ),
                    full_dot_path.clone(),
                );
                if let Some(span) = import_span {
                    self.import_spans.insert(
                        ImportKey::in_file(
                            self.current_module.clone(),
                            self.current_module_idx,
                            short.clone(),
                        ),
                        (span.clone(), self.current_module.clone()),
                    );
                }
                // Dedup pure-Hew modules (e.g. `std::fs`) that may be transitively
                // imported by multiple stdlib sub-modules.  Without this guard,
                // each referring module's `ImportDecl` carries its own
                // `resolved_items` copy and `register_user_module` would register
                // types like `IoError` once per importer, triggering duplicate-
                // definition errors.  The `registered_stdlib_hew_sources` set tracks
                // by canonical `module_path` so all `import std::fs` ImportDecls
                // collapse to the same key.
                if self.stdlib_hew_source_already_registered(decl, &full_dot_path) {
                    // Global declaration registration is deliberately deduped,
                    // but each importing scope still needs its own selected
                    // bare bindings. This path is reached when a transitive
                    // import registered the source before a root named import.
                    self.publish_imported_hew_bindings(
                        &short,
                        &full_dot_path,
                        resolved_items,
                        StdlibBarePublication::Import(&decl.spec),
                    );
                } else {
                    // The full dot-path (e.g. "subpkg.helper") is the declaring-module
                    // identity used in access-check side tables.
                    self.register_user_module(
                        &short,
                        &full_dot_path,
                        resolved_items,
                        &decl.resolved_item_source_paths,
                        &decl.spec,
                    );
                }
            }
        } else if let Some(error) =
            Self::unresolved_import_error(decl, import_span, &module_path, load_error_detail)
        {
            self.errors.push(error);
        }
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

    pub(super) fn unresolved_import_error(
        decl: &ImportDecl,
        import_span: Option<&Span>,
        module_path: &str,
        load_error_detail: Option<String>,
    ) -> Option<TypeError> {
        let detail = if decl.path.is_empty() {
            Some("file import was not resolved before type checking".to_string())
        } else {
            load_error_detail
        }?;
        let span = import_span.cloned().unwrap_or(0..0);
        let import_target = if decl.path.is_empty() {
            decl.file_path.as_deref().unwrap_or("<file import>")
        } else {
            module_path
        };
        Some(TypeError::unresolved_import(span, import_target, &detail))
    }

    /// Determine whether a SOURCE export named `name` is opted in by the
    /// `ImportSpec`. Matching is by SOURCE NAME only: in `import m::{ T as U }`,
    /// the export `T` is opted in and `U` is the binding it publishes under — a
    /// DISTINCT source export literally named `U` is NOT opted in by that alias.
    /// Matching `alias == name` here would falsely opt a real `U` in and publish
    /// it under `U` too, conflating two distinct nominal types. The alias affects
    /// only the binding name (`resolve_import_name`), never which source item the
    /// spec selects.
    #[expect(clippy::ref_option, reason = "avoids cloning the option contents")]
    pub(in crate::check) fn should_import_name(name: &str, spec: &Option<ImportSpec>) -> bool {
        match spec {
            None => false, // bare import → qualified only
            Some(ImportSpec::Names(names)) => names.iter().any(|n| n.name == name),
        }
    }

    /// Resolve the binding name for an imported symbol, applying any alias.
    #[expect(clippy::ref_option, reason = "avoids cloning the option contents")]
    pub(in crate::check) fn resolve_import_name(
        spec: &Option<ImportSpec>,
        name: &str,
    ) -> Option<String> {
        match spec {
            Some(ImportSpec::Names(names)) => names
                .iter()
                .find(|n| n.name == name)
                .map(|n| n.alias.as_deref().unwrap_or(&n.name).to_string()),
            None => None,
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
                self.local_type_defs.insert(td.name.clone());
                self.source_type_defs.insert(td.name.clone());
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
                            let source_def = self.type_defs.get(&td.name).cloned();
                            self.current_module = saved_importer_module;
                            source_def
                        });
                        if let Some(source_def) = source_def.as_ref() {
                            self.register_canonical_type_def(
                                module_full_path,
                                &td.name,
                                source_def,
                            );
                        }
                        continue;
                    }
                    if !self.register_type_namespace_name(Some(module_full_path), &td.name, span) {
                        continue;
                    }
                    let saved_importer_module =
                        self.current_module.replace(module_full_path.to_string());
                    self.in_stdlib_registration = true;
                    self.register_type_decl(td);
                    self.in_stdlib_registration = false;
                    let source_def = self.type_defs.get(&td.name).cloned();
                    self.current_module = saved_importer_module;
                    self.known_types.insert(td.name.clone());
                    // Qualified authority is always published, mirroring the
                    // user-module path: the qualified alias and the module-export
                    // record that drives the use-time gate's "exported by module
                    // X" diagnostic and ambiguity candidate naming.
                    self.register_qualified_type_alias(module_short, &td.name);
                    if let Some(source_def) = source_def.as_ref() {
                        self.register_canonical_type_def(module_full_path, &td.name, source_def);
                    }
                    self.record_module_type_export(module_short, &td.name);
                    self.record_module_type_export(module_full_path, &td.name);
                    // The importer-scope bare binding obeys the qualified-by-
                    // default gate: `Prelude` (compiled-in bootstrap surfaces)
                    // always publishes bare; a real `import` publishes bare only
                    // on a named/glob/aliased opt-in, exactly like a user module.
                    if let Some(binding) = import_spec.bare_binding(&td.name) {
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
                        &md.name,
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
                    let machine_def = self.type_defs.get(&md.name).cloned();
                    let event_def = self.type_defs.get(&event_name).cloned();
                    self.current_module = saved_importer_module;
                    self.known_types.insert(md.name.clone());
                    self.known_types.insert(event_name.clone());
                    self.register_qualified_type_alias(module_short, &md.name);
                    self.register_qualified_type_alias(module_short, &event_name);
                    if let Some(machine_def) = machine_def.as_ref() {
                        self.register_canonical_type_def(module_full_path, &md.name, machine_def);
                    }
                    if let Some(event_def) = event_def.as_ref() {
                        self.register_canonical_type_def(module_full_path, &event_name, event_def);
                    }
                    self.record_module_type_export(module_short, &md.name);
                    self.record_module_type_export(module_short, &event_name);
                    self.record_module_type_export(module_full_path, &md.name);
                    self.record_module_type_export(module_full_path, &event_name);
                    // Bare publication of the machine and its companion event
                    // enum is gated together so a named/glob import exposes both
                    // or neither; `Prelude` publishes both unconditionally.
                    if let Some(binding) = import_spec.bare_binding(&md.name) {
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
                                &super_trait.name,
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
                    if !self.register_type_namespace_name(Some(module_full_path), &tr.name, span) {
                        continue;
                    }
                    let info = Self::trait_info_from_decl(
                        tr,
                        Some(module_full_path.to_string()),
                        self.current_module_idx,
                    );
                    self.trait_defs.insert(tr.name.clone(), info.clone());
                    let qualified = format!("{module_full_path}.{}", tr.name);
                    self.trait_defs.insert(qualified, info.clone());
                    // Retain the lexical import surface as a lookup index only;
                    // trait resolution and impl facts select the exact full owner.
                    self.trait_defs
                        .entry(format!("{module_short}.{}", tr.name))
                        .or_insert(info);
                }
                Item::Function(fd) => {
                    let qualified = self.canonical_fn_identity(Some(module_full_path), &fd.name);
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
                        if let Some(binding) = import_spec.bare_binding(&fd.name) {
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
                        self.register_intrinsic_declaration(qualified, intrinsic_key, &fd.name, fd);
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
                    if !self.register_type_namespace_name(Some(module_short), &ad.name, span) {
                        continue;
                    }
                    self.register_actor_base(ad, Some(module_short));
                    if ad.visibility.is_pub() {
                        if let Some(binding) = import_spec.bare_binding(&ad.name) {
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
                    name: type_name,
                    type_args,
                } = &id.target_type.0
                {
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
                                .map(|param| param.name.clone())
                                .collect();
                            if builtin == BuiltinType::Vec {
                                if id.trait_bound.is_none() {
                                    self.builtin_vec_method_sigs
                                        .insert(method.name.clone(), (impl_params, sig.clone()));
                                }
                            } else {
                                self.builtin_result_option_method_sigs.insert(
                                    (builtin, method.name.clone()),
                                    (impl_params, sig.clone()),
                                );
                            }
                        }
                        // Also register on qualified type name
                        let qualified_type = format!("{module_short}.{type_name}");
                        if let Some(td) = self.lookup_type_def_mut(&qualified_type) {
                            td.methods.insert(method.name.clone(), sig.clone());
                        }
                        if let (Some(canonical), Some(tb)) =
                            (primitive_key.clone(), id.trait_bound.as_ref())
                        {
                            self.record_primitive_trait_impl_self_args(
                                canonical.clone(),
                                &tb.name,
                                self_type_args.clone(),
                                &id.target_type.1,
                            );
                            self.record_primitive_trait_impl_method(
                                canonical,
                                &tb.name,
                                method.name.clone(),
                                sig,
                            );
                        }
                    }
                    if let Some(tb) = &id.trait_bound {
                        self.mark_imported_trait_used_for_module_aliases(module_short, &tb.name);
                        self.record_trait_impl_methods(
                            type_name,
                            &tb.name,
                            id.methods.iter().map(|method| method.name.clone()),
                        );
                        self.record_trait_impl(type_name, &tb.name);
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
                    if let Some(source_def) = self.type_defs.get(&td.name).cloned() {
                        self.register_canonical_type_def(module_full_path, &td.name, &source_def);
                    }
                    self.retire_imported_type_keys(module_short, module_full_path, &td.name);
                    if td.visibility.is_pub() {
                        self.record_module_type_export(module_short, &td.name);
                        self.record_module_type_export(module_full_path, &td.name);
                    }
                }
                Item::Machine(md) => {
                    if !md.visibility.is_pub() {
                        continue;
                    }
                    let event_name = format!("{}Event", md.name);
                    if let Some(source_def) = self.type_defs.get(&md.name).cloned() {
                        self.register_canonical_type_def(module_full_path, &md.name, &source_def);
                    }
                    if let Some(source_def) = self.type_defs.get(&event_name).cloned() {
                        self.register_canonical_type_def(
                            module_full_path,
                            &event_name,
                            &source_def,
                        );
                    }
                    self.retire_imported_type_keys(module_short, module_full_path, &md.name);
                    self.retire_imported_type_keys(module_short, module_full_path, &event_name);
                    // A public machine publishes its generated event enum as
                    // part of the same declaration surface.  Keep the export
                    // ledger paired with the qualified aliases so import
                    // validation, checker resolution, and HIR all agree that
                    // `module.MachineEvent::Payload` is callable.
                    self.record_module_type_export(module_short, &md.name);
                    self.record_module_type_export(module_short, &event_name);
                    self.record_module_type_export(module_full_path, &md.name);
                    self.record_module_type_export(module_full_path, &event_name);
                }
                Item::Actor(ad) => {
                    // The dotted `{module_short}.{name}` entry is authored
                    // directly by `register_actor_base`; only the export
                    // record is added here.
                    self.record_module_type_export(module_short, &ad.name);
                    self.record_module_type_export(module_full_path, &ad.name);
                }
                _ => {}
            }
        }
        self.local_type_defs = saved_local_type_defs;
        self.source_type_defs = saved_source_type_defs;
        self.registration_origin_module = saved_registration_origin;
    }

    /// Republish selected public names from an already-registered Hew source
    /// into one importing scope. This deliberately performs no declaration
    /// registration and therefore cannot create duplicate defs.
    #[expect(
        clippy::too_many_lines,
        reason = "one import publication dispatcher handles every namespace without re-registering declarations"
    )]
    pub(super) fn publish_imported_hew_bindings(
        &mut self,
        module_short: &str,
        module_full_path: &str,
        items: &[Spanned<Item>],
        publication: StdlibBarePublication<'_>,
    ) {
        for (item, _) in items {
            match item {
                Item::Function(fd) if fd.visibility.is_pub() => {
                    if let Some(binding) = publication.bare_binding(&fd.name) {
                        self.publish_stdlib_hew_function_binding(
                            binding,
                            &format!("{module_full_path}.{}", fd.name),
                            publication,
                        );
                    }
                }
                Item::Trait(decl) if decl.visibility.is_pub() => {
                    let canonical = format!("{module_full_path}.{}", decl.name);
                    let Some(trait_id) = self.lookup_declaration(&canonical).cloned() else {
                        continue;
                    };
                    let mut bindings = vec![format!("{module_short}.{}", decl.name)];
                    if let Some(binding) = publication.bare_binding(&decl.name) {
                        self.published_bare_trait_owners
                            .entry((
                                self.current_module.clone(),
                                self.current_module_idx,
                                binding.clone(),
                            ))
                            .or_default()
                            .insert(canonical.clone());
                        if let Some(info) = self.trait_defs.get(&canonical).cloned() {
                            self.trait_defs.insert(binding.clone(), info);
                        }
                        self.unqualified_to_module.insert(
                            (
                                self.current_module.clone(),
                                self.current_module_idx,
                                binding.clone(),
                            ),
                            module_full_path.to_string(),
                        );
                        bindings.push(binding);
                    }
                    for binding in bindings {
                        self.trait_bindings.insert(
                            (
                                self.current_module.clone(),
                                self.current_module_idx,
                                binding.clone(),
                            ),
                            trait_id.clone(),
                        );
                        for method in &decl.items {
                            let TraitItem::Method(method) = method else {
                                continue;
                            };
                            let Some(method_id) = self
                                .lookup_declaration(&format!(
                                    "{}::{}",
                                    trait_id.full_path(),
                                    method.name
                                ))
                                .cloned()
                            else {
                                continue;
                            };
                            self.trait_method_ids_by_binding.insert(
                                (
                                    self.current_module.clone(),
                                    self.current_module_idx,
                                    binding.clone(),
                                    method.name.clone(),
                                ),
                                (trait_id.clone(), method_id),
                            );
                        }
                    }
                }
                Item::TypeAlias(decl) if decl.visibility.is_pub() => {
                    if let Some(binding) = publication.bare_binding(&decl.name) {
                        self.publish_stdlib_hew_type_binding(
                            module_short,
                            binding,
                            format!("{module_full_path}.{}", decl.name),
                            publication,
                        );
                    }
                }
                Item::Record(decl) if decl.visibility.is_pub() => {
                    let canonical = format!("{module_full_path}.{}", decl.name);
                    if let Some(binding) = publication.bare_binding(&decl.name) {
                        self.publish_stdlib_hew_type_binding(
                            module_short,
                            binding.clone(),
                            canonical.clone(),
                            publication,
                        );
                        if matches!(decl.kind, RecordKind::Tuple(_)) {
                            self.publish_stdlib_hew_function_binding(
                                binding,
                                &canonical,
                                publication,
                            );
                        }
                    }
                }
                Item::TypeDecl(td) if td.visibility.is_pub() => {
                    if let Some(binding) = publication.bare_binding(&td.name) {
                        self.publish_stdlib_hew_type_binding(
                            module_short,
                            binding,
                            format!("{module_full_path}.{}", td.name),
                            publication,
                        );
                    }
                }
                Item::Machine(md) if md.visibility.is_pub() => {
                    let event_name = format!("{}Event", md.name);
                    for name in [&md.name, &event_name] {
                        if let Some(binding) = publication.bare_binding(name) {
                            self.publish_stdlib_hew_type_binding(
                                module_short,
                                binding,
                                format!("{module_full_path}.{name}"),
                                publication,
                            );
                        }
                    }
                }
                Item::Actor(ad) if ad.visibility.is_pub() => {
                    if let Some(binding) = publication.bare_binding(&ad.name) {
                        self.publish_stdlib_hew_type_binding(
                            module_short,
                            binding,
                            format!("{module_full_path}.{}", ad.name),
                            publication,
                        );
                    }
                }
                _ => {}
            }
        }
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

    /// Register items from a file-based import into the IMPORTING file's
    /// namespace.
    ///
    /// `owner` is the imported file's own module identity. A `pub fn` it
    /// declares is published under the importer's namespace — the same key
    /// shape the importer's own declarations use, so a file import is visible
    /// exactly where it was written and nowhere else — and aliased to the
    /// declaration `{owner}.{name}` that HIR, MIR and codegen derive the symbol
    /// from. A file two files both import registers once per importer.
    #[expect(
        clippy::too_many_lines,
        reason = "single-pass walk over every Item variant with parallel registration paths"
    )]
    pub(in crate::check) fn register_file_import_items(
        &mut self,
        owner: &str,
        items: &[Spanned<Item>],
    ) {
        let mut current_import_pub_spans = HashMap::new();
        let mut skipped_type_names = HashSet::new();

        for (item, span) in items {
            match item {
                Item::Function(fd) => {
                    if !fd.visibility.is_pub() {
                        continue;
                    }
                    if !self.register_flat_file_import_pub_name(
                        &mut current_import_pub_spans,
                        &fd.name,
                        span,
                    ) {
                        continue;
                    }
                    let binding = Self::declared_fn_identity(self.canonical_fn_owner(), &fd.name);
                    let declaration = Self::declared_fn_identity(Some(owner), &fd.name);
                    self.import_fn_name_aliases.insert(
                        (
                            self.current_module.clone(),
                            self.current_module_idx,
                            binding,
                        ),
                        declaration,
                    );
                }
                Item::Const(cd) => {
                    if !cd.visibility.is_pub() {
                        continue;
                    }
                    if !self.register_flat_file_import_pub_name(
                        &mut current_import_pub_spans,
                        &cd.name,
                        span,
                    ) {
                        continue;
                    }
                    let ty = self.resolve_registered_annotation_ty_no_holes(&cd.ty);
                    self.env
                        .define(format!("{owner}.{}", cd.name), ty.clone(), false);
                    self.env.define(cd.name.clone(), ty, false);
                    // The value environment is one flat scope, so the bare
                    // binding is visible program-wide once defined. The export
                    // record is what makes it in scope only where the import
                    // was written; the use-time gate reads both.
                    let source_identity = format!("{owner}.{}", cd.name);
                    self.record_published_bare_const(&cd.name, &source_identity);
                    self.file_import_const_exports
                        .entry(cd.name.clone())
                        .or_default()
                        .insert(source_identity);
                }
                Item::TypeAlias(decl) => {
                    if decl.visibility.is_pub()
                        && self.register_flat_file_import_type_name(
                            &mut current_import_pub_spans,
                            &decl.name,
                            span,
                        )
                    {
                        self.publish_file_import_type_name(owner, &decl.name);
                    }
                }
                Item::TypeDecl(td) => {
                    if !td.visibility.is_pub() {
                        continue;
                    }
                    if !self.register_flat_file_import_type_name(
                        &mut current_import_pub_spans,
                        &td.name,
                        span,
                    ) {
                        skipped_type_names.insert(td.name.clone());
                        continue;
                    }
                    self.register_type_decl(td);
                    self.known_types.insert(td.name.clone());
                    self.publish_file_import_type_name(owner, &td.name);
                }
                Item::Machine(md) => {
                    if !md.visibility.is_pub() {
                        continue;
                    }
                    if !self.register_flat_file_import_type_name(
                        &mut current_import_pub_spans,
                        &md.name,
                        span,
                    ) {
                        skipped_type_names.insert(md.name.clone());
                        continue;
                    }
                    let event_type_name = format!("{}Event", md.name);
                    if !self.register_flat_file_import_type_name(
                        &mut current_import_pub_spans,
                        &event_type_name,
                        span,
                    ) {
                        skipped_type_names.insert(event_type_name);
                        continue;
                    }
                    self.register_machine_decl(md, span);
                    self.known_types.insert(md.name.clone());
                    self.known_types.insert(format!("{}Event", md.name));
                    self.publish_file_import_type_name(owner, &md.name);
                    self.publish_file_import_type_name(owner, &format!("{}Event", md.name));
                }
                Item::Trait(tr) => {
                    if let Some(supers) = &tr.super_traits {
                        for super_trait in supers {
                            self.mark_imported_trait_used(None, &super_trait.name);
                        }
                    }
                    // A file import is flattened into the root program before
                    // HIR, so its items share the root's flat namespace: the
                    // importer can already write `impl <Trait> for <RootType>`
                    // against a non-pub imported trait and call its required
                    // methods. Only the `trait_defs` entry was gated on `pub`,
                    // which left the impl with no view of the trait's DEFAULT
                    // method bodies (and silently skipped method-set
                    // validation) — `x.default_method()` failed with
                    // "no method". Register the declaration for every imported
                    // trait; `pub` still governs the namespace claim below.
                    let info = Self::trait_info_from_decl(
                        tr,
                        self.current_module.clone(),
                        self.current_module_idx,
                    );
                    if tr.visibility.is_pub()
                        && !self.register_flat_file_import_type_name(
                            &mut current_import_pub_spans,
                            &tr.name,
                            span,
                        )
                    {
                        continue;
                    }
                    self.trait_defs.insert(tr.name.clone(), info);
                    if tr.visibility.is_pub() {
                        self.published_bare_trait_owners
                            .entry((
                                self.current_module.clone(),
                                self.current_module_idx,
                                tr.name.clone(),
                            ))
                            .or_default()
                            .insert(format!("{owner}.{}", tr.name));
                    }
                }
                Item::Actor(ad) => {
                    if !ad.visibility.is_pub() {
                        continue;
                    }
                    if !self.register_flat_file_import_type_name(
                        &mut current_import_pub_spans,
                        &ad.name,
                        span,
                    ) {
                        continue;
                    }
                    // The declaring file owns the identity; the bare spelling is
                    // only the binding published into this importer's scope. A
                    // bare declaration row here would be a second authority for
                    // one actor, and the ask/spawn boundaries downstream would
                    // then disagree with the qualified path HIR carries.
                    self.register_actor_base(ad, Some(owner).filter(|owner| !owner.is_empty()));
                    self.publish_file_import_type_name(owner, &ad.name);
                }
                Item::Supervisor(sd) => {
                    if !sd.visibility.is_pub() {
                        continue;
                    }
                    if !self.register_flat_file_import_type_name(
                        &mut current_import_pub_spans,
                        &sd.name,
                        span,
                    ) {
                        continue;
                    }
                    // Same rule as an actor above: the declaring file owns the
                    // identity and the bare spelling is only the binding this
                    // importer sees, so `spawn Inner` reaches the declaration
                    // SIR matches rather than a bare row of its own.
                    self.reject_wasm_feature(span, WasmUnsupportedFeature::SupervisionTrees);
                    let identity = if owner.is_empty() {
                        sd.name.clone()
                    } else {
                        format!("{owner}.{}", sd.name)
                    };
                    let saved_importer_module = self.current_module.take();
                    if !owner.is_empty() {
                        self.current_module = Some(owner.to_string());
                    }
                    self.register_supervisor_decl_as(sd, &identity);
                    self.current_module = saved_importer_module;
                    self.publish_file_import_type_name(owner, &sd.name);
                }
                Item::Impl(id) => {
                    if let TypeExpr::Named {
                        name: type_name,
                        type_args: target_type_args,
                        ..
                    } = &id.target_type.0
                    {
                        if skipped_type_names.contains(type_name) {
                            continue;
                        }
                        // Validate before collect_type_param_bounds erases positional type args.
                        // This path bypasses enter_impl_scope so validation must be explicit.
                        self.validate_type_param_bound_shapes(
                            id.type_params.as_ref(),
                            id.where_clause.as_ref(),
                            span,
                        );
                        // The impl's `Self` type arguments (e.g. `[E]` for
                        // `impl<E> Index for Vec<E>`), resolved so a later
                        // dispatch on a concrete receiver can bind the impl's
                        // type parameters: `E` resolves to a bare
                        // `Ty::Named { name: "E" }`, which is exactly the
                        // placeholder the dispatch-time binding zips against
                        // the receiver's concrete args.
                        let self_type_args: Vec<Ty> =
                            self.resolve_impl_target_type_args(id, target_type_args.as_ref());
                        let primitive_key = id.trait_bound.as_ref().and_then(|_| {
                            self.canonical_primitive_or_builtin_key_for_impl_name(type_name)
                        });
                        // The enclosing impl's self type, as every other
                        // registration path publishes it. `register_impl_method`
                        // reads it to tell `impl Render for Box<i64>` from
                        // `impl<T> Render for Box<T>`: without it a flat-file
                        // import's specialisation looks generic, so it claims the
                        // shared `Box::render` dispatch key the generic
                        // declaration owns instead of taking only its own
                        // mangled key.
                        let prev_self_type = self
                            .current_self_type
                            .replace((type_name.clone(), self_type_args.clone()));
                        for method in &id.methods {
                            if !method.visibility.is_pub() {
                                continue;
                            }
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
                                    &tb.name,
                                    self_type_args.clone(),
                                    &id.target_type.1,
                                );
                                self.record_primitive_trait_impl_method(
                                    canonical,
                                    &tb.name,
                                    method.name.clone(),
                                    sig,
                                );
                            }
                        }
                        self.current_self_type = prev_self_type;
                        // Track trait implementations
                        if let Some(tb) = &id.trait_bound {
                            self.mark_imported_trait_used(None, &tb.name);
                            self.record_trait_impl_methods(
                                type_name,
                                &tb.name,
                                id.methods.iter().map(|method| method.name.clone()),
                            );
                            self.record_trait_impl(type_name, &tb.name);
                        }
                    }
                }
                _ => {}
            }
        }

        self.flat_file_import_pub_spans
            .extend(current_import_pub_spans.into_iter().map(|(name, span)| {
                (
                    (self.current_module.clone(), self.current_module_idx, name),
                    span,
                )
            }));
    }

    pub(in crate::check) fn register_flat_file_import_pub_name(
        &mut self,
        current_import_pub_spans: &mut HashMap<String, Span>,
        name: &str,
        span: &Span,
    ) -> bool {
        let key = (
            self.current_module.clone(),
            self.current_module_idx,
            name.to_string(),
        );
        if let Some(prev_span) = self
            .flat_file_import_pub_spans
            .get(&key)
            .cloned()
            .or_else(|| current_import_pub_spans.get(name).cloned())
        {
            self.errors.push(TypeError::duplicate_definition(
                span.clone(),
                name,
                prev_span,
            ));
            return false;
        }

        match current_import_pub_spans.entry(name.to_string()) {
            Entry::Occupied(_) => {}
            Entry::Vacant(entry) => {
                entry.insert(span.clone());
            }
        }

        true
    }

    pub(in crate::check) fn register_flat_file_import_type_name(
        &mut self,
        current_import_pub_spans: &mut HashMap<String, Span>,
        name: &str,
        span: &Span,
    ) -> bool {
        // The claim belongs to the IMPORTING file's namespace, the same one the
        // importer's own declarations claim: a file import that collides with a
        // declaration the importer wrote is a duplicate, while two modules
        // importing one file each get their own copy of its names.
        let importer = self.current_module.clone();
        self.register_flat_file_import_pub_name(current_import_pub_spans, name, span)
            && self.register_type_namespace_name(importer.as_deref(), name, span)
    }

    /// Publish an imported file's type-shaped declaration into the importing
    /// file's scope, owned by the file that declared it.
    ///
    /// This is the type half of the same rule the callable arm applies: the
    /// declaration keeps the imported file's identity (`{owner}.{name}`) and
    /// the bare spelling is published only where the import was written. A file
    /// that did not import it sees the owner-qualified export, so the use-time
    /// scope gate names the declaring file instead of admitting the bare name
    /// program-wide.
    pub(super) fn publish_file_import_type_name(&mut self, owner: &str, name: &str) {
        self.record_module_type_export(owner, name);
        let source_identity = format!("{owner}.{name}");
        self.record_published_bare_type(name, &source_identity);
        self.unqualified_to_module.insert(
            (
                self.current_module.clone(),
                self.current_module_idx,
                name.to_string(),
            ),
            owner.to_string(),
        );
    }

    pub(super) fn flat_file_import_already_registered(&mut self, decl: &ImportDecl) -> bool {
        let import_source = decl
            .resolved_source_paths
            .first()
            .cloned()
            .or_else(|| decl.file_path.as_ref().map(std::path::PathBuf::from));
        let Some(import_source) = import_source else {
            return false;
        };
        // Per importing file: one file imported by two files publishes its
        // names into both scopes, and a global source set would leave the
        // second importer with nothing.
        !self.registered_flat_file_import_sources.insert((
            self.current_module.clone(),
            self.current_module_idx,
            import_source,
        ))
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

    /// Record `module_short`'s own trait import bindings into
    /// `trait_import_bindings`, so a supertrait edge declared in this module that
    /// names a re-imported trait resolves to the original owner (the re-export
    /// chain) rather than a same-named trait in the final importer's scope.
    ///
    /// For each `import other::path::{ Name }` (or `{ Name as B }`) the binding
    /// `Name`/`B` records the source identity `{imported_short}.{Name}`, where
    /// `imported_short` is the import's whole-module alias or its last path
    /// segment — identical to how the module's own qualified keys are formed. A
    /// whole-module `import other::m;` (no brace spec) publishes no bare binding,
    /// so it records nothing. The module's own pub traits self-register
    /// (`(module_short, T) -> {module_short}.T`) so a chain terminates at the
    /// origin. The recorded value is a string; `resolve_trait_ref` only treats it
    /// as a trait when it matches a registered `trait_defs` key, so recording a
    /// (possibly type) import binding here is harmless.
    pub(super) fn record_trait_import_bindings(
        &mut self,
        module_owner: &str,
        items: &[Spanned<Item>],
    ) {
        for (item, _) in items {
            match item {
                Item::Import(decl) => {
                    if decl.path.is_empty() {
                        continue;
                    }
                    let imported_owner = decl.path.join(".");
                    match &decl.spec {
                        Some(ImportSpec::Names(names)) => {
                            for import_name in names {
                                let binding = import_name
                                    .alias
                                    .clone()
                                    .unwrap_or_else(|| import_name.name.clone());
                                let source_identity =
                                    format!("{imported_owner}.{}", import_name.name);
                                self.trait_import_bindings
                                    .insert((module_owner.to_string(), binding), source_identity);
                            }
                        }
                        None => {
                            let prefix = format!("{imported_owner}.");
                            let loaded_traits: Vec<String> = self
                                .trait_defs
                                .keys()
                                .filter_map(|key| key.strip_prefix(&prefix))
                                .filter(|name| !name.contains('.'))
                                .map(str::to_string)
                                .collect();
                            for trait_name in loaded_traits {
                                self.trait_import_bindings.insert(
                                    (module_owner.to_string(), trait_name.clone()),
                                    format!("{imported_owner}.{trait_name}"),
                                );
                            }
                            if let Some(resolved_items) = &decl.resolved_items {
                                for (imported_item, _) in resolved_items.iter() {
                                    if let Item::Trait(tr) = imported_item {
                                        if tr.visibility.is_pub() {
                                            self.trait_import_bindings.insert(
                                                (module_owner.to_string(), tr.name.clone()),
                                                format!("{imported_owner}.{}", tr.name),
                                            );
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                // Self-register the module's own pub traits so a re-export chain
                // terminates here (a sub-trait whose super is this module's own
                // trait resolves directly, without consulting an import).
                Item::Trait(tr) if tr.visibility.is_pub() => {
                    self.trait_import_bindings.insert(
                        (module_owner.to_string(), tr.name.clone()),
                        format!("{module_owner}.{}", tr.name),
                    );
                }
                _ => {}
            }
        }
    }
}
