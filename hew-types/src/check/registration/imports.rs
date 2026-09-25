//! Checker methods grouped by responsibility: imports.
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
use hew_parser::module::ModulePath;

impl Checker {
    pub(super) fn mark_import_module_used_for_owner(
        &self,
        owner: Option<&str>,
        imported_module: &str,
    ) {
        for ((scope, file, binding), source) in &self.module_import_bindings {
            if scope.as_deref() == owner
                && *file == self.current_module_idx
                && (binding == imported_module || source == imported_module)
            {
                self.used_modules.borrow_mut().insert(ImportKey::in_file(
                    owner.map(str::to_string),
                    *file,
                    binding.clone(),
                ));
            }
        }
    }

    pub(super) fn mark_loaded_trait_owner_import_used(
        &self,
        module: Option<&str>,
        trait_name: &str,
    ) {
        let candidate_owners = [
            module.map(str::to_string),
            self.current_module.clone(),
            None::<String>,
        ];
        let mut used = self.used_modules.borrow_mut();
        for key in self.import_spans.keys() {
            if !candidate_owners
                .iter()
                .any(|owner| owner.as_ref() == key.owner_module.as_ref())
            {
                continue;
            }
            let qualified = format!("{}.{}", key.short_name, trait_name);
            if self.trait_defs.contains_key(&qualified) {
                used.insert(key.clone());
            }
        }
    }

    pub(super) fn mark_imported_trait_used(&self, module: Option<&str>, trait_name: &str) {
        if let Some((imported_module, _)) = trait_name.split_once('.') {
            if self.modules.contains(imported_module) {
                self.mark_import_module_used_for_owner(module, imported_module);
                if self.current_module.as_deref() != module {
                    self.mark_import_module_used_for_owner(
                        self.current_module.as_deref(),
                        imported_module,
                    );
                }
            }
            return;
        }

        if let Some(source_key) = self.trait_import_bindings.get(&(
            module.unwrap_or_default().to_string(),
            trait_name.to_string(),
        )) {
            if let Some((imported_module, _)) = source_key.rsplit_once('.') {
                if Some(imported_module) == module {
                    return;
                }
                self.mark_import_module_used_for_owner(module, imported_module);
                if self.current_module.as_deref() != module {
                    self.mark_import_module_used_for_owner(
                        self.current_module.as_deref(),
                        imported_module,
                    );
                }
            }
        } else if let Some(imported_module) = self.unqualified_to_module.get(&(
            module.map(str::to_string),
            self.current_module_idx,
            trait_name.to_string(),
        )) {
            self.mark_import_module_used_for_owner(module, imported_module.as_str());
            if self.current_module.as_deref() != module {
                self.mark_import_module_used_for_owner(
                    self.current_module.as_deref(),
                    imported_module.as_str(),
                );
            }
        } else {
            self.mark_loaded_trait_owner_import_used(module, trait_name);
        }
    }

    pub(super) fn mark_imported_trait_used_for_module_aliases(
        &self,
        module_short: &str,
        trait_name: &str,
    ) {
        self.mark_imported_trait_used(Some(module_short), trait_name);

        let owner_aliases: Vec<String> = self
            .import_spans
            .keys()
            .filter_map(|key| key.owner_module.as_deref())
            .filter(|owner| owner.rsplit("::").next() == Some(module_short))
            .map(str::to_string)
            .collect();
        for owner in owner_aliases {
            self.mark_imported_trait_used(Some(&owner), trait_name);
        }
    }

    /// Seed lifecycle import bindings from module-graph edges before type
    /// declaration members are pre-registered.
    ///
    /// `collect_types` intentionally runs before the ordinary import pass, but
    /// enum/record members can themselves name an imported lifecycle type
    /// (`std.link_monitor`'s `Crashed(CrashKind)`).  A source declaration alone
    /// is not authority: require a matching resolved graph edge, then publish
    /// exactly the same lexical/canonical bindings the later import pass would.
    #[expect(
        clippy::too_many_lines,
        reason = "lifecycle import seeding mirrors all declaration kinds atomically"
    )]
    pub(super) fn seed_resolved_lifecycle_import_bindings(
        &mut self,
        module: &hew_parser::module::Module,
        importer: Option<&str>,
        module_graph: &hew_parser::module::ModuleGraph,
    ) {
        let saved_importer_file_idx = self.current_module_idx;
        let span_indices = module_graph.file_span_indices();
        for (item_idx, (item, _)) in module.items.iter().enumerate() {
            // These bindings are written before the ordinary import pass, but
            // they still belong to the importing SOURCE FILE. A directory
            // module can assemble imports from several peers, so module-level
            // or ambient indexing would publish a cross-file binding.
            self.current_module_idx = if module.id == module_graph.root {
                0
            } else {
                span_indices
                    .item_index(&module.id, item_idx)
                    .unwrap_or_default()
            };
            let Item::Import(decl) = item else {
                continue;
            };
            let Some(resolved) = module.imports.iter().find(|edge| {
                edge.target
                    .segments
                    .iter()
                    .map(|segment| segment.as_str())
                    .eq(decl
                        .path
                        .segments
                        .iter()
                        .map(|(segment, _)| segment.name.as_str()))
            }) else {
                continue;
            };
            let canonical_module = resolved.target.dotted();
            let owner = match canonical_module.as_str() {
                "std.failure" => "std.failure",
                "std.link_monitor" => "std.link_monitor",
                _ => continue,
            };
            let target_is_canonical =
                module_graph
                    .modules
                    .get(&resolved.target)
                    .is_some_and(|target| {
                        target.source_paths.iter().any(|source| {
                            crate::module_registry::is_canonical_stdlib_module_source(
                                source,
                                &canonical_module,
                            )
                        })
                    });
            let module_binding = decl.module_alias.or_else(|| decl.path.last()).map_or_else(
                || owner.rsplit('.').next().unwrap_or(owner).to_string(),
                |ident| ident.to_string(),
            );
            // Every module-path import supplies an exact lexical owner for
            // qualified sibling references. A selective import such as
            // `import hew::closableerr::{ Closable as C }` still makes
            // `closableerr.CloseError` an explicit source spelling; keeping
            // that owner fact only for whole-module imports would leave the
            // type resolver to compare a short surface name against a full
            // declaration identity.
            self.module_import_bindings.insert(
                (
                    importer.map(str::to_owned),
                    self.current_module_idx,
                    module_binding.clone(),
                ),
                canonical_module.clone(),
            );

            let lifecycle_names: &[&str] = match owner {
                "std.failure" => &["CrashNotification", "CrashKind"],
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
                let source_identity = format!("{owner}.{source_name}");
                if target_is_canonical {
                    self.canonical_lifecycle_import_authority.insert((
                        importer.map(str::to_owned),
                        if decl.spec.is_none() {
                            module_binding.clone()
                        } else {
                            let Some(binding) =
                                StdlibBarePublication::Import(&decl.spec).bare_binding(source_name)
                            else {
                                continue;
                            };
                            binding
                        },
                        source_identity.clone(),
                    ));
                    if decl.spec.is_none() {
                        // HIR does not re-resolve module imports. Publish the
                        // checker-proven whole-module spelling so a hook
                        // annotation such as `f.CrashNotification` retains the
                        // canonical lifecycle identity across TypeCheckOutput.
                        let qualified_surface = format!("{module_binding}.{source_name}");
                        if qualified_surface != source_identity {
                            self.import_type_name_aliases.insert(
                                (
                                    importer.map(str::to_owned),
                                    self.current_module_idx,
                                    qualified_surface,
                                ),
                                source_identity.clone(),
                            );
                        }
                    }
                }
                let Some(binding) =
                    StdlibBarePublication::Import(&decl.spec).bare_binding(source_name)
                else {
                    continue;
                };
                self.known_types.insert(binding.clone());
                self.record_published_bare_type(&binding, &source_identity);
                self.import_type_name_aliases.insert(
                    (
                        importer.map(str::to_owned),
                        self.current_module_idx,
                        binding.clone(),
                    ),
                    source_identity,
                );
                self.unqualified_to_module.insert(
                    (
                        importer.map(str::to_owned),
                        self.current_module_idx,
                        binding,
                    ),
                    canonical_module.clone(),
                );
            }
        }
        self.current_module_idx = saved_importer_file_idx;
    }

    /// Record direct lexical authority for lifecycle types imported from an
    /// exact shipped stdlib source.  A user module can be named `std.failure`,
    /// so module spelling and ordinary visibility are intentionally not proof.
    pub(super) fn record_canonical_lifecycle_import_authority(
        &mut self,
        decl: &ImportDecl,
        importer: Option<&str>,
    ) {
        let module_name = decl.path.to_string(); // TRANSITION(P1): deleted by A1 commit 2
        let owner = match module_name.as_str() {
            "std.failure" => "std.failure",
            "std.link_monitor" => "std.link_monitor",
            _ => return,
        };
        if !decl.resolved_source_paths.iter().any(|source| {
            crate::module_registry::is_canonical_stdlib_module_source(source, &module_name)
        }) {
            return;
        }
        let module_binding = decl.module_alias.or_else(|| decl.path.last()).map_or_else(
            || owner.rsplit('.').next().unwrap_or(owner).to_string(),
            |ident| ident.to_string(),
        );
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
            let Some(binding) = StdlibBarePublication::Import(&decl.spec).bare_binding(source_name)
            else {
                if decl.spec.is_none() {
                    let source_identity = format!("{owner}.{source_name}");
                    self.canonical_lifecycle_import_authority.insert((
                        importer.map(str::to_owned),
                        module_binding.clone(),
                        source_identity.clone(),
                    ));
                    let qualified_surface = format!("{module_binding}.{source_name}");
                    if qualified_surface != source_identity {
                        self.import_type_name_aliases.insert(
                            (
                                importer.map(str::to_owned),
                                self.current_module_idx,
                                qualified_surface,
                            ),
                            source_identity,
                        );
                    }
                }
                continue;
            };
            self.canonical_lifecycle_import_authority.insert((
                importer.map(str::to_owned),
                binding,
                format!("{owner}.{source_name}"),
            ));
        }
    }

    /// Pass 1.5 — re-resolve type-declaration MEMBER types after import
    /// processing (#2202).
    ///
    /// `collect_types` (Pass 1) resolves record/struct field types, enum-variant
    /// payload types, and machine state/event field types BEFORE
    /// `collect_functions` (Pass 2) processes imports and populates the
    /// import-alias maps (`published_bare_type_owners` / `import_type_name_aliases`).
    /// A bare import alias used in member position therefore froze as an
    /// unresolved `Named("Tag")` while its construction (Pass 3) resolves to the
    /// canonical `aliassrc.Payload`, producing a spurious mismatch.
    ///
    /// This pass runs immediately after `collect_functions`, when every module's
    /// alias maps are live, and re-resolves each type declaration's member types
    /// under the OWNING module's context. A member that upgrades from a bare
    /// alias to its canonical qualified identity is committed back into
    /// `type_defs` (bare + module-qualified keys) and the member-derived facts
    /// are re-run over the canonical types: the structural marker set
    /// (`register_type` — Send/Copy/Frozen/Clone/Encode), the
    /// `Serializable` member set, the per-module qualified marker
    /// mirror (the ask-reply Send-gate anti-clobber), the variant-constructor
    /// `fn_sigs`, the wire codec layout, and the `Encode`-driven JSON/YAML/TOML
    /// methods. Members that did not change are left untouched, so the common
    /// (alias-free) path is a no-op and no derivation is re-run.
    ///
    /// The local-shadow rule is preserved: a local `type U` shadowing an import
    /// alias keeps `local_type_defs`/`source_type_defs` populated for the owning
    /// module, so `published_bare_type_qualified` returns `None` and the member
    /// stays bound to the local definition. Diagnostics emitted while
    /// re-resolving are dropped by this driver: Pass 1 already emitted for
    /// genuinely-unresolvable members and the value/use sites (Pass 3) re-emit,
    /// so this upgrade-only pass must never be the sole emitter.
    pub(in crate::check) fn reresolve_member_types_after_imports(&mut self, program: &Program) {
        let errors_before = self.errors.len();
        let warnings_before = self.warnings.len();
        let preferred_modules = collision_preferred_package_module_ids(program, &HashSet::new());
        // Member re-resolution is a secondary fix-up pass (it overwrites the
        // member types computed during `collect_types` once imports are
        // visible) and runs with `type_decls_registered` already true. Suppress
        // the undefined-named-type guard for its duration: a type declaration's
        // members are out of the F1 diagnostic's remit (they keep the existing
        // `E_MIR: unknown type` path), and emitting here would also let the
        // guard substitute `Ty::Error` for the member type, overwriting the
        // good type computed during `collect_types` and tripping the HIR
        // field-access checker-boundary conversion downstream.
        let prev_suppress = self.suppress_undefined_type_report;
        self.suppress_undefined_type_report = true;

        if let Some(ref mg) = program.module_graph {
            let span_indices = mg.file_span_indices();
            for mod_id in &mg.topo_order {
                if *mod_id == mg.root {
                    continue;
                }
                let Some(module) = mg.modules.get(mod_id) else {
                    continue;
                };
                self.current_module = Some(mod_id.dotted());
                let saved_local_type_defs = self.local_type_defs.clone();
                let saved_source_type_defs = self.source_type_defs.clone();
                self.seed_member_reresolution_scope(&module.items);
                for (item_idx, (item, item_span)) in module.items.iter().enumerate() {
                    self.current_module_idx = span_indices
                        .item_index(mod_id, item_idx)
                        .unwrap_or_default();
                    if member_item_is_absorbed_from_distinct_child(
                        program,
                        &preferred_modules,
                        mod_id,
                        item,
                        item_span,
                    ) {
                        continue;
                    }
                    self.reresolve_item_member_types(item);
                }
                self.local_type_defs = saved_local_type_defs;
                self.source_type_defs = saved_source_type_defs;
            }
        }

        self.current_module = None;
        self.current_module_idx = 0;
        let saved_local_type_defs = self.local_type_defs.clone();
        let saved_source_type_defs = self.source_type_defs.clone();
        self.seed_member_reresolution_scope(&program.items);
        for (item, _) in &program.items {
            self.reresolve_item_member_types(item);
        }
        self.local_type_defs = saved_local_type_defs;
        self.source_type_defs = saved_source_type_defs;

        self.errors.truncate(errors_before);
        self.warnings.truncate(warnings_before);
        self.suppress_undefined_type_report = prev_suppress;
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
                    Some(ImportSpec::Names(names)) => {
                        names
                            .iter()
                            .find(|n| {
                                n.alias
                                    .map_or(n.name.name.as_str(), |ident| ident.name.as_str())
                                    == name
                            })?
                            .name
                    }
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
                            .is_some_and(|declared| declared.contains(source_name.name.as_str()))
                    })
                    .map(|source| (source, source_name))
                    .collect::<Vec<_>>()
            })
            .collect::<Vec<_>>();
        declarations.sort_unstable();
        declarations.dedup();
        match declarations.as_slice() {
            [(single, source_name)] => Some(format!(
                "{}.{source_name}",
                self.defs.module_path_for_source(single)?
            )),
            _ => None,
        }
    }

    pub(super) fn import_publication_candidates(decl: &ImportDecl) -> Vec<(String, String)> {
        let mut candidates = Vec::new();
        let module_binding = decl.module_alias.or_else(|| decl.path.last());
        if let Some(binding) = module_binding {
            candidates.push((binding.to_string(), binding.to_string()));
        }

        if let Some(ImportSpec::Names(names)) = &decl.spec {
            candidates.extend(names.iter().map(|name| {
                (
                    name.alias.unwrap_or(name.name).to_string(),
                    name.name.to_string(),
                )
            }));
            return candidates;
        }

        if !decl.path.segments.is_empty() {
            return candidates;
        }

        let Some(items) = decl.resolved_items.as_ref() else {
            return candidates;
        };
        for (item, _) in items.iter() {
            let mut push = |name: &str| candidates.push((name.to_string(), name.to_string()));
            match item {
                Item::Function(item) if item.visibility.is_pub() => push(item.name.name.as_str()),
                Item::Const(item) if item.visibility.is_pub() => push(item.name.name.as_str()),
                Item::TypeDecl(item) if item.visibility.is_pub() => push(item.name.name.as_str()),
                Item::TypeAlias(item) if item.visibility.is_pub() => push(item.name.name.as_str()),
                Item::Trait(item) if item.visibility.is_pub() => push(item.name.name.as_str()),
                Item::Actor(item) if item.visibility.is_pub() => push(item.name.name.as_str()),
                Item::Machine(item) if item.visibility.is_pub() => {
                    push(item.name.name.as_str());
                    push(&format!("{}Event", item.name));
                }
                Item::Record(item) if item.visibility.is_pub() => push(item.name.name.as_str()),
                _ => {}
            }
        }
        candidates
    }

    pub(super) fn preflight_import_publication(&mut self, decl: &ImportDecl, span: &Span) -> bool {
        let candidates = Self::import_publication_candidates(decl);
        let source_owner = if decl.path.segments.is_empty() {
            decl.file_path.as_deref().map_or_else(
                || "<file-import>".to_string(),
                |path| format!("file:{path}"),
            )
        } else {
            decl.path.to_string() // TRANSITION(P1): deleted by A1 commit 2
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
            let requested_owner = if decl.path.segments.is_empty() {
                ModulePath::new([decl
                    .file_path
                    .as_deref()
                    .and_then(|path| std::path::Path::new(path).file_stem())
                    .and_then(std::ffi::OsStr::to_str)
                    .unwrap_or("file")])
            } else {
                import_module_path(decl)
            };
            let primary = self.defs.mint_module(
                &crate::module_registry::canonical_source_module_identity(
                    &requested_owner,
                    &decl.resolved_source_paths,
                )
                .dotted(),
                &decl.resolved_source_paths,
            );
            // The identity table interns by canonical source, so a module the
            // compile already reached under another spelling answers with the
            // render it was minted under. That render is the one owner every
            // registration below keys by; the requested spelling is only how
            // this importer wrote it.
            let owner = self.defs.module_path(primary).to_string();
            resolved_module_owner = Some(owner.clone());
            for source in decl.resolved_source_paths.iter().skip(1) {
                self.defs.mint_source_file_module(&owner, source);
            }
            // A file import (`import "helper.hew";`, empty path) flattens its
            // items into the root's namespace, so its nominals answer bare as
            // well as under their own file. A module import
            // (`import pkg.alpha;`) does not: its nominals stay qualified by
            // the owning module, so two modules exporting the same leaf keep
            // distinct declarations.
            let namespace =
                crate::check::NominalNamespace::for_import(decl.path.segments.is_empty());
            if !self.defs.module_has_source_declarations(primary) {
                for (index, (item, span)) in items.iter().enumerate() {
                    let module = decl
                        .resolved_item_source_paths
                        .get(index)
                        .and_then(|source| self.defs.module_for_source(source))
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
        let module_path = decl.path.to_string(); // TRANSITION(P1): deleted by A1 commit 2

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
                                    &import_module_path(decl),
                                    std::slice::from_ref(source_path),
                                )
                                .dotted()
                            },
                        )
                    });
                    let registry_module = self
                        .defs
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
                    let short = decl.module_alias.unwrap_or(Ident::new(&requested_short));

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
                            hew_parser::ast::Symbol::intern(&func.name),
                            &func.name,
                        );
                        self.insert_fn_sig_at(&func.name, sig);
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
                        self.insert_fn_sig_at(&format!("{canonical_owner}.{}", wfn.name), sig);
                    }

                    // Register module and clean names
                    self.modules.insert(short.to_string());
                    self.module_import_bindings.insert(
                        (
                            self.current_module.clone(),
                            self.current_module_idx,
                            short.to_string(),
                        ),
                        canonical_owner.clone(),
                    );
                    if let Some(span) = import_span {
                        self.import_spans.insert(
                            ImportKey::in_file(
                                self.current_module.clone(),
                                self.current_module_idx,
                                short.to_string(),
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
                        let wrapper_sig = self.fn_sig(&key).cloned();
                        let sig = wrapper_sig
                            .clone()
                            .or_else(|| self.fn_sig(c_symbol.as_str()).cloned());
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
                            if !self.has_fn_sig(&key) {
                                if wrapper_sig.is_some() {
                                    self.insert_fn_sig_at(&key, sig);
                                } else {
                                    self.alias_fn_sig(&key, c_symbol);
                                }
                            }
                            if wrapper_sig.is_none() {
                                // The clean name resolved straight to the C
                                // function: the call is an FFI call and keeps
                                // its `unsafe` gate under the canonical key.
                                self.declare_contractless_extern(
                                    registry_module,
                                    &canonical_owner,
                                    hew_parser::ast::Symbol::intern(method),
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
                            short.name.as_str(),
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
            if decl.path.segments.is_empty() {
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
                    .or_else(|| decl.path.last())
                    .expect("import path is non-empty")
                    .to_string();
                let full_dot_path = resolved_module_owner
                    .clone()
                    .unwrap_or_else(|| decl.path.to_string()); // TRANSITION(P1): deleted by A1 commit 2
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
                        short.as_str(),
                        &full_dot_path,
                        resolved_items,
                        StdlibBarePublication::Import(&decl.spec),
                    );
                } else {
                    // The full dot-path (e.g. "subpkg.helper") is the declaring-module
                    // identity used in access-check side tables.
                    self.register_user_module(
                        short.as_str(),
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

    pub(super) fn unresolved_import_error(
        decl: &ImportDecl,
        import_span: Option<&Span>,
        module_path: &str,
        load_error_detail: Option<String>,
    ) -> Option<TypeError> {
        let detail = if decl.path.segments.is_empty() {
            Some("file import was not resolved before type checking".to_string())
        } else {
            load_error_detail
        }?;
        let span = import_span.cloned().unwrap_or(0..0);
        let import_target = if decl.path.segments.is_empty() {
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
            Some(ImportSpec::Names(names)) => names.iter().any(|n| n.name == Ident::new(name)),
        }
    }

    /// Resolve the binding name for an imported symbol, applying any alias.
    #[expect(clippy::ref_option, reason = "avoids cloning the option contents")]
    pub(in crate::check) fn resolve_import_name(
        spec: &Option<ImportSpec>,
        name: &str,
    ) -> Option<String> {
        match spec {
            Some(ImportSpec::Names(names)) => {
                names.iter().find(|n| n.name == Ident::new(name)).map(|n| {
                    n.alias
                        .map_or(n.name.name.as_str(), |ident| ident.name.as_str())
                        .to_string()
                })
            }
            None => None,
        }
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
                    if let Some(binding) = publication.bare_binding(fd.name.name.as_str()) {
                        self.publish_stdlib_hew_function_binding(
                            binding,
                            &format!("{module_full_path}.{}", fd.name),
                            publication,
                        );
                    }
                }
                Item::Trait(decl) if decl.visibility.is_pub() => {
                    let canonical = format!("{module_full_path}.{}", decl.name);
                    let Some(trait_id) = self.lookup_declaration(&canonical) else {
                        continue;
                    };
                    let mut bindings = vec![format!("{module_short}.{}", decl.name)];
                    if let Some(binding) = publication.bare_binding(decl.name.name.as_str()) {
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
                            trait_id,
                        );
                        for method in &decl.items {
                            let TraitItem::Method(method) = method else {
                                continue;
                            };
                            let Some(method_id) = self.lookup_declaration(&format!(
                                "{}::{}",
                                self.defs.path(trait_id),
                                method.name
                            )) else {
                                continue;
                            };
                            self.trait_method_ids_by_binding.insert(
                                (
                                    self.current_module.clone(),
                                    self.current_module_idx,
                                    binding.clone(),
                                    method.name.to_string(),
                                ),
                                (trait_id, method_id),
                            );
                        }
                    }
                }
                Item::TypeAlias(decl) if decl.visibility.is_pub() => {
                    if let Some(binding) = publication.bare_binding(decl.name.name.as_str()) {
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
                    if let Some(binding) = publication.bare_binding(decl.name.name.as_str()) {
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
                    if let Some(binding) = publication.bare_binding(td.name.name.as_str()) {
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
                    for name in [&md.name.to_string(), &event_name] {
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
                    if let Some(binding) = publication.bare_binding(ad.name.name.as_str()) {
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
                        fd.name.name.as_str(),
                        span,
                    ) {
                        continue;
                    }
                    let binding = Self::declared_fn_identity(
                        self.canonical_fn_owner(),
                        fd.name.name.as_str(),
                    );
                    let declaration =
                        Self::declared_fn_identity(Some(owner), fd.name.name.as_str());
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
                        cd.name.name.as_str(),
                        span,
                    ) {
                        continue;
                    }
                    let ty = self.resolve_registered_annotation_ty_no_holes(&cd.ty);
                    self.env
                        .define(format!("{owner}.{}", cd.name), ty.clone(), false);
                    self.env.define(cd.name.to_string(), ty, false);
                    // The value environment is one flat scope, so the bare
                    // binding is visible program-wide once defined. The export
                    // record is what makes it in scope only where the import
                    // was written; the use-time gate reads both.
                    let source_identity = format!("{owner}.{}", cd.name);
                    self.record_published_bare_const(cd.name.name.as_str(), &source_identity);
                    self.file_import_const_exports
                        .entry(cd.name.to_string())
                        .or_default()
                        .insert(source_identity);
                }
                Item::TypeAlias(decl) => {
                    if decl.visibility.is_pub()
                        && self.register_flat_file_import_type_name(
                            &mut current_import_pub_spans,
                            decl.name.name.as_str(),
                            span,
                        )
                    {
                        self.publish_file_import_type_name(owner, decl.name.name.as_str());
                    }
                }
                Item::TypeDecl(td) => {
                    if !td.visibility.is_pub() {
                        continue;
                    }
                    if !self.register_flat_file_import_type_name(
                        &mut current_import_pub_spans,
                        td.name.name.as_str(),
                        span,
                    ) {
                        skipped_type_names.insert(td.name.to_string());
                        continue;
                    }
                    self.register_type_decl(td);
                    self.known_types.insert(td.name.to_string());
                    self.publish_file_import_type_name(owner, td.name.name.as_str());
                }
                Item::Machine(md) => {
                    if !md.visibility.is_pub() {
                        continue;
                    }
                    if !self.register_flat_file_import_type_name(
                        &mut current_import_pub_spans,
                        md.name.name.as_str(),
                        span,
                    ) {
                        skipped_type_names.insert(md.name.to_string());
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
                    self.known_types.insert(md.name.to_string());
                    self.known_types.insert(format!("{}Event", md.name));
                    self.publish_file_import_type_name(owner, md.name.name.as_str());
                    self.publish_file_import_type_name(owner, &format!("{}Event", md.name));
                }
                Item::Trait(tr) => {
                    if let Some(supers) = &tr.super_traits {
                        for super_trait in supers {
                            self.mark_imported_trait_used(None, &super_trait.path.to_string());
                            // TRANSITION(P1): deleted by A1 commit 2
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
                            tr.name.name.as_str(),
                            span,
                        )
                    {
                        continue;
                    }
                    self.trait_defs.insert(tr.name.to_string(), info);
                    if tr.visibility.is_pub() {
                        self.published_bare_trait_owners
                            .entry((
                                self.current_module.clone(),
                                self.current_module_idx,
                                tr.name.to_string(),
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
                        ad.name.name.as_str(),
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
                    self.publish_file_import_type_name(owner, ad.name.name.as_str());
                }
                Item::Supervisor(sd) => {
                    if !sd.visibility.is_pub() {
                        continue;
                    }
                    if !self.register_flat_file_import_type_name(
                        &mut current_import_pub_spans,
                        sd.name.name.as_str(),
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
                        sd.name.to_string()
                    } else {
                        format!("{owner}.{}", sd.name)
                    };
                    let saved_importer_module = self.current_module.take();
                    if !owner.is_empty() {
                        self.current_module = Some(owner.to_string());
                    }
                    self.register_supervisor_decl_as(sd, identity.as_str());
                    self.current_module = saved_importer_module;
                    self.publish_file_import_type_name(owner, sd.name.name.as_str());
                }
                Item::Impl(id) => {
                    if let TypeExpr::Named {
                        path: named_path,
                        type_args: target_type_args,
                        ..
                    } = &id.target_type.0
                    {
                        let type_name = &named_path.to_string(); // TRANSITION(P1): deleted by A1 commit 2
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
                        self.current_self_type = prev_self_type;
                        // Track trait implementations
                        if let Some(tb) = &id.trait_bound {
                            self.mark_imported_trait_used(None, &tb.path.to_string()); // TRANSITION(P1): deleted by A1 commit 2
                            self.record_trait_impl_methods(
                                type_name,
                                &tb.path.to_string(), // TRANSITION(P1): deleted by A1 commit 2
                                id.methods.iter().map(|method| method.name.to_string()),
                            );
                            self.record_trait_impl(type_name, &tb.path.to_string());
                            // TRANSITION(P1): deleted by A1 commit 2
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
                    if decl.path.segments.is_empty() {
                        continue;
                    }
                    let imported_owner = decl.path.to_string(); // TRANSITION(P1): deleted by A1 commit 2
                    match &decl.spec {
                        Some(ImportSpec::Names(names)) => {
                            for import_name in names {
                                let binding = import_name.alias.unwrap_or(import_name.name);
                                let source_identity =
                                    format!("{imported_owner}.{}", import_name.name);
                                self.trait_import_bindings.insert(
                                    (module_owner.to_string(), binding.to_string()),
                                    source_identity,
                                );
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
                                                (module_owner.to_string(), tr.name.to_string()),
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
                        (module_owner.to_string(), tr.name.to_string()),
                        format!("{module_owner}.{}", tr.name),
                    );
                }
                _ => {}
            }
        }
    }

    /// Build the precise cross-module record-name collision set, mirroring the
    /// HIR/MIR authoritative notion (`imported_type_name_collides` /
    /// `collided_type_names`): a bare record/type-decl name collides when 2+
    /// distinct non-root modules (package OR file-import) declare it, AFTER
    /// re-export subsumption so a stdlib module surfaced through two import
    /// paths (e.g. `std::net::http` and `std::net::http::http_client` both
    /// re-exporting `http.Response`) is not double-counted. Only a colliding
    /// record is owner-qualified to its declaring module; a name unique to one
    /// module keeps its bare identity, so no `http.Response`/`xml.Node`
    /// over-qualification and no cabi over-qualification SIGSEGV (#2208).
    pub(in crate::check) fn compute_cross_module_colliding_record_names(
        program: &Program,
    ) -> HashSet<String> {
        let Some(mg) = program.module_graph.as_ref() else {
            return HashSet::new();
        };
        // Empty file-import exclusion: a file-import module counts as a
        // declaring scope — the mixed file-import + package same-bare-name shape
        // #2208 depends on — matching the HIR lowering's collision set exactly.
        let no_file_exclusion: HashSet<hew_parser::module::ModulePath> = HashSet::new();
        let preferred = collision_preferred_package_module_ids(program, &no_file_exclusion);
        let mut colliding: HashSet<String> = HashSet::new();
        for module in mg.modules.values() {
            for (item, _) in &module.items {
                let name = match item {
                    Item::TypeDecl(decl) => &decl.name,
                    Item::Record(decl) => &decl.name,
                    _ => continue,
                };
                if colliding.contains(name.name.as_str()) {
                    continue;
                }
                if collision_imported_type_name_collides(
                    program,
                    &no_file_exclusion,
                    &preferred,
                    name.name.as_str(),
                ) {
                    colliding.insert(name.to_string());
                }
            }
        }
        colliding
    }

    /// Owner-qualify ONLY the cross-module-colliding record names inside a reply
    /// type, recursing through generic arguments exactly as HIR's
    /// `qualify_colliding_module_record_ty` does. Builtins, already-qualified
    /// names, and non-colliding records are returned unchanged, so a reply
    /// `Result<Unique, Colliding>` becomes `Result<Unique, {module}.Colliding>`
    /// — the same identity HIR produces — rather than qualifying `Unique` too
    /// (which MIR's actor-reply equality would reject). Only a name the module
    /// actually declares (`{module}.{name}` present in `type_defs`) is
    /// qualified; otherwise the bare name is preserved (#2208).
    pub(super) fn qualify_colliding_reply_ty(&self, ty: &Ty, module_short: &str) -> Ty {
        let Ty::Named { head, args } = ty else {
            return ty.clone();
        };
        let args: Vec<Ty> = args
            .iter()
            .map(|arg| self.qualify_colliding_reply_ty(arg, module_short))
            .collect();
        // A resolved head already names its declaration; only a spelling the
        // registry mirror left unresolved can still collide across modules.
        let crate::TypeHead::Unresolved(spelling) = head else {
            return Ty::Named { head: *head, args };
        };
        let name = spelling.as_str();
        let qualified = format!("{module_short}.{name}");
        if !name.contains('.')
            && self.cross_module_colliding_record_names.contains(name)
            && self.type_def_at(&qualified).is_some()
        {
            self.named_ty_for_key(&qualified, args)
        } else {
            self.named_ty_for_key(name, args)
        }
    }

    /// Retire the temporary source-leaf and lexical-module keys for an imported
    /// type after its full-owner definition has been published.
    pub(super) fn retire_imported_type_keys(
        &mut self,
        module_short: &str,
        module_full_path: &str,
        name: &str,
    ) {
        let canonical = format!("{module_full_path}.{name}");
        let surface = format!("{module_short}.{name}");
        for key in [name, surface.as_str()] {
            if key == canonical {
                continue;
            }
            // A root declaration owns its leaf spelling canonically. Imports
            // with the same leaf may use that row transiently while their
            // source definition is assembled, but must not retire the root's
            // namespace row when their own canonical publication completes.
            if key == name
                && self
                    .type_namespace_owners
                    .contains_key(&(None, name.to_string()))
            {
                continue;
            }
            self.type_def_spans.remove(key);
            self.registry.remove_type_marker_key(key);
        }
    }
}

/// The module an import names, segment by segment.
fn import_module_path(decl: &ImportDecl) -> ModulePath {
    ModulePath {
        segments: decl
            .path
            .segments
            .iter()
            .map(|(ident, _)| ident.name)
            .collect(),
    }
}
