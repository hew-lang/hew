//! Whole-program lowering entry point.

use super::*;

/// Variant of [`lower_program`] with an explicit monomorphisation-registry
/// cap. Intended for tests that exercise the
/// `MonomorphisationCapExceeded` diagnostic with a small fixture; the
/// production entry point [`lower_program`] always uses
/// `MONOMORPHISATION_REGISTRY_CAP`.
#[must_use]
#[allow(
    clippy::too_many_lines,
    reason = "three structured passes (fn pre-pass, record/type-decl pre-pass, \
              source-order emit) read more clearly here than across helpers"
)]
pub fn lower_program_with_mono_cap(
    program: &Program,
    type_check_output: &TypeCheckOutput,
    _ctx: &ResolutionCtx,
    mono_cap: usize,
    target_arch: TargetArch,
) -> LowerOutput {
    let program = type_check_output
        .normalized_machines
        .as_ref()
        .map_or(program, |normalized| &normalized.program);
    let mut ctx = LowerCtx::new(type_check_output, mono_cap, target_arch);
    let entry_exit_plan = type_check_output.entry_exit_plan.clone();
    let compiling_prelude_manifest = program.module_graph.as_ref().is_some_and(|graph| {
        graph
            .modules
            .get(&graph.root)
            .is_some_and(|root| root.items.is_empty() && root.source_paths.is_empty())
            && graph
                .modules
                .keys()
                .any(|module| module.path.join(".") == "std.prelude")
    });
    let file_import_module_idx = file_import_item_module_indices(program);
    // Source items flattened from a file import still belong to that file's
    // declaration namespace.  Compute the checker-aligned module-name carrier
    // once and use it in every HIR pass that creates declaration identity,
    // not only while lowering executable bodies in the third pass.
    // Every index a module owns — one per source file, so a directory module's
    // peers all carry their assembled module's name.
    let span_indices = program
        .module_graph
        .as_ref()
        .map(hew_parser::module::ModuleGraph::file_span_indices)
        .unwrap_or_default();
    if let Some(root) = ctx.identity.root_module() {
        ctx.declaration_module_by_file_index.insert(0, root);
    }
    if let Some(graph) = &program.module_graph {
        for module_id in &graph.topo_order {
            if *module_id == graph.root {
                continue;
            }
            let Some(module) = graph.modules.get(module_id) else {
                continue;
            };
            if module.source_paths.is_empty() {
                if let (Some(index), Some(identity_module)) = (
                    span_indices.module_base(module_id),
                    ctx.identity.module_for_path(&module_id.path.join(".")),
                ) {
                    ctx.declaration_module_by_file_index
                        .insert(index, identity_module);
                }
                continue;
            }
            for source in &module.source_paths {
                if let (Some(index), Some(identity_module)) = (
                    span_indices.path_index(source),
                    ctx.identity.module_for_source(source),
                ) {
                    ctx.declaration_module_by_file_index
                        .insert(index, identity_module);
                }
            }
            // A span index is allocated per (module, file), so a file that is
            // BOTH a directory module's peer and importable in its own right
            // owns two of them — `std/net/http/http_client.hew` is one index
            // under `std.net.http` and another under
            // `std.net.http.http_client`. `path_index` keeps only the first,
            // so map every index the item walk can actually produce, through
            // the two accessors that own the pairing.
            for item_idx in 0..module.items.len() {
                let Some(index) = span_indices.item_index(module_id, item_idx) else {
                    continue;
                };
                let identity_module = program
                    .module_graph
                    .as_ref()
                    .and_then(|graph| graph.item_source(module_id, item_idx))
                    .or_else(|| module.source_paths.first())
                    .and_then(|source| ctx.identity.module_for_source(source));
                if let Some(identity_module) = identity_module {
                    ctx.declaration_module_by_file_index
                        .entry(index)
                        .or_insert(identity_module);
                }
            }
        }
    }
    ctx.seed_stdlib_fn_registry();
    let builtin_declarations = builtin_callable_impl_program();
    let mut builtin_impl_diagnostics = Vec::new();
    let (builtin_callable_impl_program, builtin_callable_impl_output) =
        match builtin_declarations.clone() {
            Some(program) => match check_builtin_callable_impl_program(&program) {
                Ok(output) => (Some(program), Some(output)),
                Err(diagnostic) => {
                    builtin_impl_diagnostics.push(*diagnostic);
                    (None, None)
                }
            },
            None => (None, None),
        };

    // Root-authored declarations only: items spliced into `program.items` by
    // `flatten_file_import_items` keep their defining file's module identity
    // (`file_import_module_idx`), so they must not claim the root bare
    // namespace here. Their layouts key by `qualified_name()`; leaving the
    // spliced short name in this set made a root annotation resolve bare and
    // miss the qualified MIR field-order/value-class entries. The
    // bare-spelling route for these declarations is
    // `file_import_root_type_aliases` below.
    ctx.root_visible_source_type_short_names.extend(
        program
            .items
            .iter()
            .enumerate()
            .filter(|(item_idx, _)| !file_import_module_idx.contains_key(item_idx))
            .filter_map(|(_, (item, _))| match item {
                Item::TypeDecl(decl) => Some(decl.name.clone()),
                Item::Record(decl) => Some(decl.name.clone()),
                Item::Actor(decl) => Some(decl.name.clone()),
                Item::Supervisor(decl) => Some(decl.name.clone()),
                _ => None,
            }),
    );
    // Flat-file imports share the root bare namespace (the checker enforces
    // uniqueness there), but their declaration identity is the defining
    // module's qualified name. Publish the bare→qualified projection so an
    // annotation written at root (`fn f(w: WorkflowState)`) resolves to the
    // same identity the layout registries key by.
    for (item_idx, (item, _)) in program.items.iter().enumerate() {
        let Some(module_idx) = file_import_module_idx.get(&item_idx) else {
            continue;
        };
        let Some(module_full_path) = span_indices.module_name(*module_idx) else {
            continue;
        };
        match item {
            Item::TypeDecl(decl) => {
                ctx.file_import_root_type_aliases.insert(
                    decl.name.clone(),
                    format!("{module_full_path}.{}", decl.name),
                );
            }
            Item::Record(decl) => {
                ctx.file_import_root_type_aliases.insert(
                    decl.name.clone(),
                    format!("{module_full_path}.{}", decl.name),
                );
            }
            Item::Machine(decl) => {
                ctx.file_import_root_type_aliases.insert(
                    decl.name.clone(),
                    format!("{module_full_path}.{}", decl.name),
                );
                let event = machine_event_surface_type(&decl.name);
                ctx.file_import_root_type_aliases
                    .insert(event.clone(), format!("{module_full_path}.{event}"));
            }
            Item::Actor(decl) => {
                ctx.file_import_root_type_aliases.insert(
                    decl.name.clone(),
                    format!("{module_full_path}.{}", decl.name),
                );
            }
            _ => {}
        }
    }
    // The actor delivery declarations and `ScopeFailure` are authored in the
    // embedded `std/builtins.hew` program rather than the module graph, but
    // they are lowered under the same `std.builtins` owner every downstream
    // stage looks them up by. Publish their identities alongside the graph's.
    for name in hew_types::actor_delivery::DECLARATIONS
        .iter()
        .chain(&["ScopeFailure"])
    {
        let canonical = format!("std.builtins.{name}");
        ctx.source_type_identities.insert(canonical.clone());
        // A bare reference at root binds to the same owner. A root
        // declaration of the same name still wins: the local-declaration
        // check in `resolve_named_type_ref` runs before this alias.
        ctx.file_import_root_type_aliases
            .insert((*name).to_string(), canonical);
    }
    if let Some(module_graph) = &program.module_graph {
        for module_id in &module_graph.topo_order {
            if *module_id == module_graph.root {
                continue;
            }
            let module_full_path = module_id.path.join(".");
            let Some(module) = module_graph.modules.get(module_id) else {
                continue;
            };
            let identities = module
                .items
                .iter()
                .filter_map(|(item, _)| {
                    match item {
                        Item::TypeDecl(decl) => {
                            Some(vec![format!("{module_full_path}.{}", decl.name)])
                        }
                        Item::Record(decl) => {
                            Some(vec![format!("{module_full_path}.{}", decl.name)])
                        }
                        // A machine publishes two nominal declarations: its
                        // value state and the generated event companion. Both
                        // travel across imports with the exact source owner;
                        // omitting the companion loses `MachineEvent` at HIR
                        // lowering and poisons every `.step(event)` site at
                        // the MIR value-class boundary.
                        Item::Machine(decl) => Some(vec![
                            format!("{module_full_path}.{}", decl.name),
                            format!("{module_full_path}.{}Event", decl.name),
                        ]),
                        _ => None,
                    }
                })
                .flatten()
                .collect::<Vec<_>>();
            ctx.source_type_identities
                .extend(identities.iter().cloned());
            // A `std` path segment is not provenance: a user package may use
            // that spelling.  Only the resolved source selected by the
            // stdlib search-path authority can grant compiler builtin
            // identity to declarations in this module.
            let canonical_std_source = module.source_paths.iter().any(|source| {
                hew_types::module_registry::is_canonical_stdlib_module_source(
                    source,
                    &module_full_path,
                )
            });
            if canonical_std_source {
                ctx.canonical_std_source_type_identities.extend(identities);
            }
        }
    }

    let mut builtin_callable_impl_method_symbols: HashSet<String> = HashSet::new();

    // First pass: collect all function signatures so that forward and mutual
    // references in call expressions resolve to the correct return type.
    // Diagnostics from this pass are discarded — the same types are re-lowered
    // in the second pass, which is where canonical diagnostics are emitted.
    for (item_idx, (item, span)) in program.items.iter().enumerate() {
        ctx.current_item_ordinal = item_idx;
        ctx.current_module_idx = file_import_module_idx.get(&item_idx).copied().unwrap_or(0);
        ctx.current_module_name = span_indices
            .module_name(ctx.current_module_idx)
            .map(str::to_string);
        match item {
            Item::Function(func) => {
                let item = ctx.register_fn_entry(&func.name, func);
                if func.name == "main" {
                    let declaration =
                        ctx.source_declaration(span, hew_types::DeclarationKind::Function, 0);
                    if let (Some(plan), Some(declaration)) = (entry_exit_plan.as_ref(), declaration)
                    {
                        if plan.entry != declaration {
                            ctx.fn_symbol_overrides
                                .insert(item, authored_main_callable_symbol(&declaration));
                        }
                    }
                }
            }
            Item::ExternBlock(block) => {
                // Register extern fn signatures so call sites resolve them
                // to a `BindingRef::Item` like any other top-level function.
                // Codegen pre-declares the LLVM symbol with external linkage
                // (see `predeclare_extern_decls` in hew-codegen-rs).
                for extern_fn in &block.functions {
                    ctx.register_extern_fn_entry(extern_fn);
                }
            }
            Item::Impl(impl_decl) => {
                // Register methods of any V0b-acceptable impl block under the
                // qualified `<SelfType>::<method>` name so call sites can
                // resolve them in the second pass. The Index special-case is
                // a subset of this because its methods use the same
                // `fn_registry` key shape.
                if let TypeExpr::Named {
                    name,
                    type_args: target_type_args,
                } = &impl_decl.target_type.0
                {
                    if impl_decl.where_clause.is_none()
                        || classify_unsupported_where_clause(impl_decl).is_none()
                    {
                        let impl_type_params = impl_type_param_names(impl_decl);
                        // For concrete specialised impls (empty impl type-params,
                        // non-empty target type args), compute the mangled self-type
                        // name so the fn_registry key is distinct per instantiation.
                        // Mirrors the identical logic in `lower_impl_block` (#2270).
                        let symbol_name: std::borrow::Cow<str> = if impl_type_params.is_empty() {
                            let concrete_args: Vec<hew_types::ResolvedTy> = target_type_args
                                .as_deref()
                                .unwrap_or(&[])
                                .iter()
                                .map(|a| ctx.lower_type(a))
                                .collect();
                            if concrete_args.is_empty() {
                                std::borrow::Cow::Borrowed(name.as_str())
                            } else {
                                std::borrow::Cow::Owned(crate::monomorph::mangle(
                                    name,
                                    &concrete_args,
                                ))
                            }
                        } else {
                            std::borrow::Cow::Borrowed(name.as_str())
                        };
                        for method in &impl_decl.methods {
                            ctx.register_impl_method_fn_entry(
                                &symbol_name,
                                method,
                                &impl_type_params,
                            );
                        }
                        // Also register trait default methods that are NOT
                        // overridden in this impl block — they need fn_registry
                        // entries so call sites resolve `Type::method` even
                        // when the body lives on the trait declaration.
                        if let Some(tb) = &impl_decl.trait_bound {
                            let overridden: HashSet<&str> =
                                impl_decl.methods.iter().map(|m| m.name.as_str()).collect();
                            if let Some(default_owner) = ctx.trait_declaration(&tb.name) {
                                if let Some(defaults) =
                                    ctx.trait_defaults.get(&default_owner).cloned()
                                {
                                    for default_method in &defaults {
                                        if !overridden.contains(default_method.method.name.as_str())
                                        {
                                            ctx.register_trait_default_fn_entry(
                                                &symbol_name,
                                                default_method,
                                                &impl_type_params,
                                            );
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
            Item::Const(const_decl) => {
                ctx.register_const_entry(const_decl);
            }
            // No fn signatures to register for the variants below in this
            // pass. If a new Item variant is added, the compiler will force a
            // conscious decision here.
            Item::Import(_)
            | Item::TypeDecl(_)
            | Item::TypeAlias(_)
            | Item::Trait(_)
            | Item::Machine(_)
            | Item::Record(_)
            | Item::Actor(_)
            | Item::Supervisor(_) => {}
        }
    }
    ctx.current_module_idx = 0;
    ctx.current_module_name = None;
    // Pre-pass: register user-module pub fn signatures under their qualified,
    // native-symbol-safe key (e.g. `greeting$hello`) so that HIR's
    // `RewriteModuleQualifiedToFunction` arm can resolve the callee `ItemId`
    // at the call site.  Only `pub` functions are callable across module
    // boundaries; private functions are invisible to importers.
    //
    // This walk runs AFTER the root-item pre-pass above so that any name
    // clash between a qualified key and a root-level function is caught by
    // the duplicate-short-module diagnostic in the frontend before HIR ingest.
    let file_import_modules = file_import_module_ids(program);
    let preferred_modules = preferred_package_module_ids(program, &file_import_modules);
    let colliding_imported_record_names: HashSet<String> = program
        .module_graph
        .as_ref()
        .into_iter()
        .flat_map(|module_graph| module_graph.modules.values())
        .flat_map(|module| module.items.iter())
        .filter_map(|(item, _)| match item {
            Item::TypeDecl(decl) => Some(decl.name.clone()),
            Item::Record(decl) => Some(decl.name.clone()),
            _ => None,
        })
        .filter(|name| {
            imported_type_name_collides(program, &file_import_modules, &preferred_modules, name)
        })
        .collect();
    ctx.colliding_imported_record_names
        .clone_from(&colliding_imported_record_names);
    ctx.file_import_module_names = file_import_modules
        .iter()
        .map(|id| id.path.join("."))
        .collect();
    // Bare record/type-decl names that genuinely collide across modules,
    // INCLUDING a file-import vs package collision (the shape the actor-ask
    // identity coupling needs). Reuses `imported_type_name_collides` — which
    // already dedups re-exports via `preferred_package_module_ids` subsumption,
    // so a stdlib module re-imported through two paths (e.g. `std::net::http`
    // and `std::net::http::http_client` both surfacing `http.Response`) is NOT
    // counted twice — but with an EMPTY file-import exclusion so a file-import
    // module counts as a colliding declarant. A name unique to one owner is
    // absent and never owner-qualified (#2208).
    ctx.cross_module_colliding_record_names = {
        let no_file_exclusion: HashSet<hew_parser::module::ModuleId> = HashSet::new();
        let preferred_all = preferred_package_module_ids(program, &no_file_exclusion);
        program
            .module_graph
            .as_ref()
            .into_iter()
            .flat_map(|mg| mg.modules.values())
            .flat_map(|module| module.items.iter())
            .filter_map(|(item, _)| match item {
                Item::TypeDecl(decl) => Some(decl.name.clone()),
                Item::Record(decl) => Some(decl.name.clone()),
                _ => None,
            })
            .filter(|name| {
                imported_type_name_collides(program, &no_file_exclusion, &preferred_all, name)
            })
            .collect()
    };
    if let Some(ref mg) = program.module_graph {
        for mod_id in &mg.topo_order {
            if *mod_id == mg.root {
                continue;
            }
            let module_full_path = mod_id.path.join(".");
            if let Some(module) = mg.modules.get(mod_id) {
                // #2202: lower this imported module's type-decl/record members
                // under its OWN module context so a member typed by an import
                // alias canonicalises through `import_type_name_aliases` (keyed
                // by the full dotted path) instead of freezing as a bare name
                // that MIR cannot resolve.
                let saved_module_name = ctx.current_module_name.replace(module_full_path.clone());
                let saved_module_idx = ctx.current_module_idx;
                let private_fns = module
                    .items
                    .iter()
                    .filter_map(|(item, _)| match item {
                        Item::Function(function) if !function.visibility.is_pub() => {
                            Some(function.name.clone())
                        }
                        _ => None,
                    })
                    .collect();
                let private_closure = collect_imported_private_fn_closure(
                    module,
                    &private_fns,
                    ctx.trait_defaults
                        .values()
                        .flatten()
                        .filter(|default| {
                            default.source_module.as_deref() == Some(module_full_path.as_str())
                        })
                        .filter_map(|default| default.method.body.as_ref()),
                );
                for (item_idx, (item, item_span)) in module.items.iter().enumerate() {
                    ctx.current_item_ordinal = item_idx;
                    ctx.current_module_idx = span_indices
                        .item_index(mod_id, item_idx)
                        .unwrap_or_default();
                    match item {
                        Item::Function(func)
                            if func.visibility.is_pub() || private_closure.contains(&func.name) =>
                        {
                            if item_is_duplicated_in_distinct_leaf_module(
                                program,
                                &preferred_modules,
                                mod_id,
                                item,
                                item_span,
                            ) {
                                continue;
                            }
                            let qualified = crate::mangle_dotted_name(&format!(
                                "{module_full_path}.{}",
                                func.name
                            ));
                            ctx.register_fn_entry(&qualified, func);
                        }
                        // Register public type declarations plus private
                        // struct records from imported modules into
                        // `record_registry` so `Expr::StructInit` and
                        // `Expr::FieldAccess` lowering can resolve their field
                        // layouts. Without this, field access on imported
                        // non-generic private FFI-result records fails with
                        // `NotYetImplemented` because the layout is missing
                        // from `record_field_orders` at MIR time.
                        //
                        // All matching TypeDecls from non-root modules are
                        // registered, not just monomorphic ones; the generic
                        // case is filtered at MIR layout-emission time (same
                        // rule as root items). Private imported generic
                        // handles stay on their existing intrinsic paths, and
                        // private imported enums stay on the existing enum-only
                        // machinery.
                        Item::TypeDecl(decl)
                            if decl.visibility.is_pub()
                                || (decl.kind == TypeDeclKind::Struct
                                    && decl.type_params.is_none()) =>
                        {
                            let id = ctx.ids.item();
                            let kind = if decl.origin
                                == hew_parser::ast::DeclarationOrigin::MachineState
                            {
                                hew_types::DeclarationKind::Machine
                            } else {
                                hew_types::DeclarationKind::Type
                            };
                            let Some(declaration) = ctx.source_declaration(item_span, kind, 0)
                            else {
                                continue;
                            };
                            let Some(definition) =
                                ctx.checked_member_definition(&declaration, item_span)
                            else {
                                continue;
                            };
                            let fields = ctx.checked_record_fields(&definition, item_span);
                            let type_params = definition.type_params;
                            ctx.record_registry.insert(
                                format!("{module_full_path}.{}", decl.name),
                                RecordEntry {
                                    id,
                                    type_params: type_params.clone(),
                                    fields: fields.clone(),
                                },
                            );
                            // The bare spelling is a compatibility lookup
                            // only.  The full source owner above owns the
                            // record identity; never let a same-leaf sibling
                            // overwrite it here.
                            ctx.record_registry
                                .entry(decl.name.clone())
                                .or_insert_with(|| RecordEntry {
                                    id,
                                    type_params,
                                    fields,
                                });
                        }
                        Item::Record(decl) => {
                            let id = ctx.ids.item();
                            let Some(declaration) = ctx.source_declaration(
                                item_span,
                                hew_types::DeclarationKind::Record,
                                0,
                            ) else {
                                continue;
                            };
                            let Some(definition) =
                                ctx.checked_member_definition(&declaration, item_span)
                            else {
                                continue;
                            };
                            let fields = ctx.checked_record_fields(&definition, item_span);
                            let type_params = definition.type_params;
                            ctx.record_registry.insert(
                                format!("{module_full_path}.{}", decl.name),
                                RecordEntry {
                                    id,
                                    type_params: type_params.clone(),
                                    fields: fields.clone(),
                                },
                            );
                            ctx.record_registry
                                .entry(decl.name.clone())
                                .or_insert_with(|| RecordEntry {
                                    id,
                                    type_params,
                                    fields,
                                });
                            ctx.type_classes
                                .entry(decl.name.clone())
                                .or_insert((ResourceMarker::None, None));
                        }
                        // Register extern fn signatures declared by imported
                        // modules so call sites in user code resolve them
                        // through `BindingRef::Item` like any other top-level
                        // function. Without this, `import std::io; io.write(s)`
                        // fails with `UnresolvedSymbol("hew_io_write")` because
                        // the std/io.hew `extern "C" { fn hew_io_write(...); }`
                        // block never reaches `fn_registry`.
                        Item::ExternBlock(block) => {
                            for extern_fn in &block.functions {
                                ctx.register_extern_fn_entry(extern_fn);
                            }
                        }
                        // Register imported impl block methods in `fn_registry`
                        // under the same unqualified `<SelfType>::<method>` key
                        // used for root impl blocks. The key must be unqualified
                        // because the checker's `fn_sigs` table also uses
                        // unqualified keys for impl methods (see
                        // `scoped_module_item_name` / `register_impl_method` in
                        // checker).
                        //
                        // No per-method `pub` gate: impl methods in Hew have no
                        // independent visibility — access is governed by the
                        // trait/type, and impl bodies never carry `pub fn`. The
                        // checker registers EVERY imported impl method into
                        // `fn_sigs` without a `pub` filter and emits
                        // `RewriteToFunction { c_symbol: "<Type>::<method>" }`
                        // for any non-trivial builder method (one whose body is
                        // not a single C pass-through, so it is not captured by
                        // the runtime handle-method path). Gating this pre-pass
                        // on `pub` left those `<Type>::<method>` symbols absent
                        // from `fn_registry`, so the call lowered to a
                        // `ResolvedRef::Unresolved` callee and tripped
                        // `IndirectCallUnsupported` at the import boundary
                        // (e.g. `import std::encoding::json` then a `with_*`
                        // builder chain). Matching the root pre-pass (which has
                        // no `pub` filter) keeps HIR aligned with the
                        // checker-authoritative `fn_sigs`.
                        Item::Impl(impl_decl) => {
                            if let TypeExpr::Named { name, .. } = &impl_decl.target_type.0 {
                                if impl_decl.where_clause.is_none()
                                    || classify_unsupported_where_clause(impl_decl).is_none()
                                {
                                    let impl_type_params = impl_type_param_names(impl_decl);
                                    let symbol_self_name =
                                        imported_impl_symbol_self_name(&module_full_path, name);
                                    for method in &impl_decl.methods {
                                        ctx.register_impl_method_fn_entry(
                                            &symbol_self_name,
                                            method,
                                            &impl_type_params,
                                        );
                                    }
                                    if let Some(tb) = &impl_decl.trait_bound {
                                        let overridden: HashSet<&str> = impl_decl
                                            .methods
                                            .iter()
                                            .map(|m| m.name.as_str())
                                            .collect();
                                        if let Some(default_owner) = ctx.trait_declaration(&tb.name)
                                        {
                                            if let Some(defaults) =
                                                ctx.trait_defaults.get(&default_owner).cloned()
                                            {
                                                for default_method in &defaults {
                                                    if !overridden.contains(
                                                        default_method.method.name.as_str(),
                                                    ) {
                                                        let fn_decl = trait_method_to_fn_decl(
                                                            &default_method.method,
                                                        );
                                                        ctx.register_impl_method_fn_entry(
                                                            &symbol_self_name,
                                                            &fn_decl,
                                                            &impl_type_params,
                                                        );
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                        // Register all consts from imported modules under their
                        // qualified key `"module_short.CONST_NAME"`.  Pub
                        // consts are needed for cross-module `module.CONST`
                        // field-access resolution; private consts must also be
                        // registered here so that imported function bodies that
                        // reference the module's own private consts by bare
                        // name get a stable `ItemId` that matches the
                        // `HirItem::Const` emitted in the later pass.
                        Item::Const(const_decl) => {
                            let id = ctx.ids.item();
                            let ty = ctx.lower_type(&const_decl.ty);
                            let qualified = format!("{module_full_path}.{}", const_decl.name);
                            ctx.const_registry.insert(qualified, ConstEntry { id, ty });
                        }
                        // Non-pub Function/TypeDecl/Record fall here (not exported to importers).
                        Item::Import(_)
                        | Item::Function(_)
                        | Item::TypeDecl(_)
                        | Item::TypeAlias(_)
                        | Item::Trait(_)
                        | Item::Machine(_)
                        | Item::Actor(_)
                        | Item::Supervisor(_) => {}
                    }
                }
                ctx.current_module_idx = saved_module_idx;
                ctx.current_module_name = saved_module_name;
            }
        }
    }

    // Pre-pass: collect record/type-decl shapes so `Expr::StructInit`
    // lowering in the source-order pass can answer "is this a generic
    // user record?" regardless of declaration order relative to the
    // function that uses it. This mirrors the fn pre-pass above and is
    // the producer half of the record-layout registry.
    for (item_idx, (item, item_span)) in program.items.iter().enumerate() {
        ctx.current_item_ordinal = item_idx;
        ctx.current_module_idx = file_import_module_idx
            .get(&item_idx)
            .copied()
            .unwrap_or_default();
        ctx.current_module_name = span_indices
            .module_name(ctx.current_module_idx)
            .map(str::to_string);
        match item {
            Item::TypeDecl(decl) => {
                let id = ctx.ids.item();
                let kind = if decl.origin == hew_parser::ast::DeclarationOrigin::MachineState {
                    hew_types::DeclarationKind::Machine
                } else {
                    hew_types::DeclarationKind::Type
                };
                let Some(declaration) = ctx.source_declaration(item_span, kind, 0) else {
                    continue;
                };
                let Some(definition) = ctx.checked_member_definition(&declaration, item_span)
                else {
                    continue;
                };
                let fields = ctx.checked_record_fields(&definition, item_span);
                let type_params = definition.type_params;
                ctx.record_registry.insert(
                    decl.name.clone(),
                    RecordEntry {
                        id,
                        type_params,
                        fields,
                    },
                );
                ctx.type_classes
                    .insert(decl.name.clone(), (ResourceMarker::None, None));
            }
            Item::Record(decl) => {
                let id = ctx.ids.item();
                let Some(declaration) =
                    ctx.source_declaration(item_span, hew_types::DeclarationKind::Record, 0)
                else {
                    continue;
                };
                let Some(definition) = ctx.checked_member_definition(&declaration, item_span)
                else {
                    continue;
                };
                let fields = ctx.checked_record_fields(&definition, item_span);
                let type_params = definition.type_params;
                ctx.record_registry.insert(
                    decl.name.clone(),
                    RecordEntry {
                        id,
                        type_params,
                        fields,
                    },
                );
            }
            // No record shape to register for the variants below. If a new
            // Item variant with field layout is added, the compiler will force
            // a conscious decision here.
            Item::Import(_)
            | Item::Const(_)
            | Item::TypeAlias(_)
            | Item::Trait(_)
            | Item::Impl(_)
            | Item::Function(_)
            | Item::ExternBlock(_)
            | Item::Machine(_)
            | Item::Actor(_)
            | Item::Supervisor(_) => {}
        }
    }
    ctx.current_module_idx = 0;
    ctx.current_module_name = None;
    // Pre-pass: target architecture gates (P0.1-P0.4 fail-closed runtime-panic
    // prevention). Check if the program uses coroutine-dependent constructs
    // (actors/tasks) or wasm32-unsupported constructs (blocking channel recv)
    // on incompatible targets. Emit fatal diagnostics before lowering begins.
    //
    // This pass runs early so rejection happens before any HIR nodes are
    // materialized, keeping the fail-closed contract clear: if a program uses
    // unsupported runtime features, the compile stops here.
    check_target_gates(&mut ctx, program);

    // Pre-pass: register module-scope tagged-union constructors so
    // `lower_identifier` can lower variant references to `MachineVariantCtor`
    // regardless of declaration order relative to the function that uses them.
    //
    // Three surface forms share one tagged-union substrate:
    //   1. Machine states: `TrafficLight::Red`, bare `Red`.
    //   2. Machine event companions: `TrafficLightEvent::Tick`, bare `Tick`.
    //   3. User-defined enum unit variants: `Colour::Red`, bare `Red`.
    //
    // Bare names are registered only when unambiguous across ALL three forms
    // (machines + events + user enums). The qualified form (always prefixed
    // with the tagged-union typename) is always registered. See the doc on
    // `LowerCtx::machine_ctor_registry` for the consumer contract.
    {
        // First scan: count bare-name occurrences across machines' states,
        // machine events, and user enum unit variants. A bare name with
        // count > 1 is ambiguous and only the qualified form is registered.
        //
        // Built-in tagged unions (`Option<T>`, `Result<T, E>`) participate
        // in the same registry so `Ok(42)`, `None`, etc. lower through the
        // same `MachineVariantCtor` / match-arm path as user enums. Each
        // builtin variant contributes one count, so a user enum that
        // redeclares `Some` or `Ok` will correctly mark the bare name as
        // ambiguous (qualified forms still resolve via `Option::Some`,
        // `Result::Ok`).
        //
        // Local-shadows-global: builtins are NOT pre-seeded any longer.
        // Instead, after counting all user variants, builtin names are
        // inserted with count 1 ONLY WHERE user count is 0 (via `or_insert`).
        // A companion `user_declared_variant_names` set tracks which bare
        // names the root program declared, so the builtin spec registration
        // pass can skip the bare-form insertion for those names (preventing
        // the last-write-wins overwrite of the user's registration).
        let mut bare_counts: HashMap<String, usize> = HashMap::new();
        let mut surface_ctor_counts: HashMap<String, usize> = HashMap::new();
        // Accumulate user-declared variant names from root `program.items` so
        // the builtin registration pass can honour local-shadows-global.
        let mut user_declared_variant_names: std::collections::HashSet<String> =
            std::collections::HashSet::new();
        for (item, _) in &program.items {
            match item {
                Item::Machine(md) => {
                    for state in &md.states {
                        *bare_counts.entry(state.name.clone()).or_insert(0) += 1;
                        *surface_ctor_counts
                            .entry(tagged_union_surface_ctor_key(&md.name, &state.name))
                            .or_insert(0) += 1;
                        // Machine states shadow same-named builtins (local-shadows-global).
                        user_declared_variant_names.insert(state.name.clone());
                    }
                    for event in &md.events {
                        *bare_counts.entry(event.name.clone()).or_insert(0) += 1;
                        *surface_ctor_counts
                            .entry(tagged_union_surface_ctor_key(
                                &machine_event_surface_type(&md.name),
                                &event.name,
                            ))
                            .or_insert(0) += 1;
                        // Machine events shadow same-named builtins (local-shadows-global).
                        user_declared_variant_names.insert(event.name.clone());
                    }
                }
                Item::TypeDecl(td) if td.kind == TypeDeclKind::Enum => {
                    // Count every variant — unit, tuple, struct — so the
                    // ambiguity guard fires across all surface forms (e.g. a
                    // bare `Line` is ambiguous with a `Line(i64)` variant on
                    // another enum just as it is with a unit variant).
                    for body_item in &td.body {
                        if let TypeBodyItem::Variant(v) = body_item {
                            *bare_counts.entry(v.name.clone()).or_insert(0) += 1;
                            *surface_ctor_counts
                                .entry(tagged_union_surface_ctor_key(&td.name, &v.name))
                                .or_insert(0) += 1;
                            // Track root-program user variants for the
                            // local-shadows-global builtin registration guard.
                            user_declared_variant_names.insert(v.name.clone());
                        }
                    }
                }
                // No variant bare-names to count for these items. If a new
                // Item variant with enumerable named variants is added, the
                // compiler will force a conscious decision here.
                Item::Import(_)
                | Item::Const(_)
                | Item::TypeDecl(_)
                | Item::TypeAlias(_)
                | Item::Trait(_)
                | Item::Impl(_)
                | Item::Function(_)
                | Item::ExternBlock(_)
                | Item::Actor(_)
                | Item::Supervisor(_)
                | Item::Record(_) => {}
            }
        }
        // Mirror the bare-count scan over `program.module_graph` non-root
        // modules so cross-module machine states/events and enum variants
        // participate in the same bare-ambiguity decision as root items.
        // Without this, a bare `On` used by a root function that imports
        // `std::machines::toggle` would not be marked ambiguous against any
        // other `On` in scope, and — more importantly — the qualified form
        // `Toggle::On` would not be registered in `machine_ctor_registry`
        // (next module-graph loop below) since the registration loop is the
        // producer side of the same walk.
        //
        // Gated on `is_pub()` for cross-module emission: a private machine
        // or enum in an imported module is invisible to consumers.
        if let Some(ref mg) = program.module_graph {
            for mod_id in &mg.topo_order {
                if *mod_id == mg.root {
                    continue;
                }
                if let Some(module) = mg.modules.get(mod_id) {
                    for (item, _) in &module.items {
                        match item {
                            Item::Machine(md) => {
                                for state in &md.states {
                                    *bare_counts.entry(state.name.clone()).or_insert(0) += 1;
                                    *surface_ctor_counts
                                        .entry(tagged_union_surface_ctor_key(&md.name, &state.name))
                                        .or_insert(0) += 1;
                                    // Machine states (pub or private) shadow same-named
                                    // builtins across the flat global registry, matching
                                    // the checker's global register_machine_decl walk.
                                    user_declared_variant_names.insert(state.name.clone());
                                }
                                for event in &md.events {
                                    *bare_counts.entry(event.name.clone()).or_insert(0) += 1;
                                    *surface_ctor_counts
                                        .entry(tagged_union_surface_ctor_key(
                                            &machine_event_surface_type(&md.name),
                                            &event.name,
                                        ))
                                        .or_insert(0) += 1;
                                    // Machine events shadow same-named builtins.
                                    user_declared_variant_names.insert(event.name.clone());
                                }
                            }
                            Item::TypeDecl(td) if td.kind == TypeDeclKind::Enum => {
                                // All enum variants (pub or private) shadow same-named
                                // builtins, matching the checker's global
                                // pre_register_type_decl walk (registration.rs:1359)
                                // which inserts bare variant fn_sigs regardless of
                                // visibility.
                                for body_item in &td.body {
                                    if let TypeBodyItem::Variant(v) = body_item {
                                        *bare_counts.entry(v.name.clone()).or_insert(0) += 1;
                                        *surface_ctor_counts
                                            .entry(tagged_union_surface_ctor_key(&td.name, &v.name))
                                            .or_insert(0) += 1;
                                        user_declared_variant_names.insert(v.name.clone());
                                    }
                                }
                            }
                            // No variant bare-names to count for these items.
                            // Compiler enforces exhaustivity if a new Item variant
                            // is added. Item::Machine is handled by the arm above.
                            Item::Import(_)
                            | Item::Const(_)
                            | Item::TypeDecl(_)
                            | Item::TypeAlias(_)
                            | Item::Trait(_)
                            | Item::Impl(_)
                            | Item::Function(_)
                            | Item::ExternBlock(_)
                            | Item::Actor(_)
                            | Item::Supervisor(_)
                            | Item::Record(_) => {}
                        }
                    }
                }
            }
        }
        // Post-user-scan: seed builtin enum variant names with count 1 where
        // the user has not declared any variant with that name.  This preserves
        // bare-form accessibility for pure-builtin names (e.g. `Ok`, `None`,
        // `Full`) while preventing builtins from contributing a spurious "1"
        // to names the user already declared.  Combined with the guard in the
        // builtin spec registration pass below, this implements the
        // local-shadows-global rule at the HIR bare-name layer.
        for name in builtin_enum_variant_names() {
            bare_counts.entry(name.to_string()).or_insert(1);
        }
        for (item, _) in &program.items {
            match item {
                Item::Machine(md) => {
                    let event_type_name = format!("{}Event", md.name);
                    for (idx, state) in md.states.iter().enumerate() {
                        let qualified = format!("{}::{}", md.name, state.name);
                        ctx.machine_ctor_registry
                            .insert(qualified, (md.name.clone(), idx));
                        if bare_counts.get(&state.name).copied().unwrap_or(0) == 1 {
                            ctx.machine_ctor_registry
                                .insert(state.name.clone(), (md.name.clone(), idx));
                        }
                    }
                    for (idx, event) in md.events.iter().enumerate() {
                        let qualified = format!("{}::{}", event_type_name, event.name);
                        ctx.machine_ctor_registry
                            .insert(qualified, (event_type_name.clone(), idx));
                        if bare_counts.get(&event.name).copied().unwrap_or(0) == 1 {
                            ctx.machine_ctor_registry
                                .insert(event.name.clone(), (event_type_name.clone(), idx));
                        }
                    }
                }
                Item::TypeDecl(td) if td.kind == TypeDeclKind::Enum => {
                    // Register every variant — unit, tuple, struct — under
                    // the qualified `Type::Variant` key. The variant index is
                    // the ordinal in declaration order across all shapes; it
                    // matches the order `EnumLayout.variants` uses in
                    // MIR/codegen (see lane-plan D2 — variant-index ordering
                    // is HIR-pre-pass authoritative). Tuple variants are
                    // resolved by `Expr::Call` lowering; struct variants by
                    // `Expr::StructInit` lowering; unit variants by
                    // identifier-resolution. All three paths consult this
                    // registry and dispatch on the variant's `HirVariantKind`.
                    let mut variant_idx: usize = 0;
                    for body_item in &td.body {
                        if let TypeBodyItem::Variant(v) = body_item {
                            let qualified = format!("{}::{}", td.name, v.name);
                            ctx.machine_ctor_registry
                                .insert(qualified, (td.name.clone(), variant_idx));
                            if bare_counts.get(&v.name).copied().unwrap_or(0) == 1 {
                                ctx.machine_ctor_registry
                                    .insert(v.name.clone(), (td.name.clone(), variant_idx));
                            }
                            variant_idx += 1;
                        }
                    }
                }
                // No ctor entries to register for these items. If a new Item
                // variant with enumerable ctors is added, the compiler will
                // force a conscious decision here.
                Item::Import(_)
                | Item::Const(_)
                | Item::TypeDecl(_)
                | Item::TypeAlias(_)
                | Item::Trait(_)
                | Item::Impl(_)
                | Item::Function(_)
                | Item::ExternBlock(_)
                | Item::Actor(_)
                | Item::Supervisor(_)
                | Item::Record(_) => {}
            }
        }
        // Mirror the machine_ctor_registry fill over `program.module_graph`
        // non-root modules. The checker's `register_machine_decl` walk in
        // `hew-types/src/check/registration.rs` already populates `type_defs`
        // for imported machines and enums regardless of visibility; this loop
        // is the HIR-side symmetric producer.  Visibility is NOT used as a
        // filter here: private machines and enums in imported modules are
        // registered globally so that their own pub function bodies (which are
        // lowered in §4b / fourth-pass) resolve bare variant constructors
        // correctly.  This matches the checker's global pre_register_type_decl
        // behaviour (registration.rs:1359, no pub guard).
        if let Some(ref mg) = program.module_graph {
            for mod_id in &mg.topo_order {
                if *mod_id == mg.root {
                    continue;
                }
                if let Some(module) = mg.modules.get(mod_id) {
                    let source_module = mod_id.path.join(".");
                    for (item, _) in &module.items {
                        match item {
                            Item::Machine(md) => {
                                let event_type_name = format!("{}Event", md.name);
                                let source_state_type = format!("{source_module}.{}", md.name);
                                let source_event_type =
                                    format!("{source_module}.{event_type_name}");
                                for (idx, state) in md.states.iter().enumerate() {
                                    let module_qualified =
                                        format!("{source_module}.{}::{}", md.name, state.name);
                                    ctx.machine_ctor_registry
                                        .insert(module_qualified, (source_state_type.clone(), idx));
                                    let surface =
                                        tagged_union_surface_ctor_key(&md.name, &state.name);
                                    if surface_ctor_counts.get(&surface).copied() == Some(1) {
                                        ctx.machine_ctor_registry
                                            .entry(surface)
                                            .or_insert_with(|| (source_state_type.clone(), idx));
                                    }
                                    if bare_counts.get(&state.name).copied() == Some(1) {
                                        ctx.machine_ctor_registry
                                            .entry(state.name.clone())
                                            .or_insert_with(|| (source_state_type.clone(), idx));
                                    }
                                }
                                for (idx, event) in md.events.iter().enumerate() {
                                    let module_qualified = format!(
                                        "{source_module}.{event_type_name}::{}",
                                        event.name
                                    );
                                    ctx.machine_ctor_registry
                                        .insert(module_qualified, (source_event_type.clone(), idx));
                                    let surface = tagged_union_surface_ctor_key(
                                        &event_type_name,
                                        &event.name,
                                    );
                                    if surface_ctor_counts.get(&surface).copied() == Some(1) {
                                        ctx.machine_ctor_registry
                                            .entry(surface)
                                            .or_insert_with(|| (source_event_type.clone(), idx));
                                    }
                                    if bare_counts.get(&event.name).copied() == Some(1) {
                                        ctx.machine_ctor_registry
                                            .entry(event.name.clone())
                                            .or_insert_with(|| (source_event_type.clone(), idx));
                                    }
                                }
                            }
                            Item::TypeDecl(td) if td.kind == TypeDeclKind::Enum => {
                                let source_enum_name = format!("{source_module}.{}", td.name);
                                let mut variant_idx: usize = 0;
                                for body_item in &td.body {
                                    if let TypeBodyItem::Variant(v) = body_item {
                                        let module_qualified =
                                            format!("{source_module}.{}::{}", td.name, v.name);
                                        ctx.machine_ctor_registry.insert(
                                            module_qualified,
                                            (source_enum_name.clone(), variant_idx),
                                        );
                                        let surface =
                                            tagged_union_surface_ctor_key(&td.name, &v.name);
                                        if surface_ctor_counts.get(&surface).copied() == Some(1) {
                                            ctx.machine_ctor_registry
                                                .entry(surface)
                                                .or_insert_with(|| {
                                                    (source_enum_name.clone(), variant_idx)
                                                });
                                        }
                                        if bare_counts.get(&v.name).copied() == Some(1) {
                                            ctx.machine_ctor_registry
                                                .entry(v.name.clone())
                                                .or_insert_with(|| {
                                                    (source_enum_name.clone(), variant_idx)
                                                });
                                        }
                                        variant_idx += 1;
                                    }
                                }
                            }
                            // No ctor entries to register for these items.
                            // Compiler enforces exhaustivity if a new Item variant
                            // is added. Item::Machine and TypeDecl::Enum are
                            // handled by the arms above.
                            Item::Import(_)
                            | Item::Const(_)
                            | Item::TypeDecl(_)
                            | Item::TypeAlias(_)
                            | Item::Trait(_)
                            | Item::Impl(_)
                            | Item::Function(_)
                            | Item::ExternBlock(_)
                            | Item::Actor(_)
                            | Item::Supervisor(_)
                            | Item::Record(_) => {}
                        }
                    }
                }
            }
        }
        // Register built-in tagged unions (`Option<T>`, `Result<T, E>`).
        // These have no `Item::TypeDecl` in user source — the checker treats
        // them as primitive — but the HIR lowering pipeline routes their
        // constructors (`Some/None/Ok/Err`) and match arms through the same
        // `machine_ctor_registry` / `enum_variants_by_name` registries as
        // user enums. Without these entries, `Ok(42)` would lower as an
        // unresolved identifier and `match r { .Ok(n) => ... }` would emit
        // `match arm variant not registered in machine/enum ctor registry`.
        //
        // Local-shadows-global: skip the bare-form insertion for any builtin
        // variant name that the root program has already declared.  The
        // qualified form (`LookupError::NotFound`) is always registered so
        // existing code that uses the fully-qualified path keeps working.
        for spec in BUILTIN_ENUM_SPECS {
            for (variant_idx, variant_name) in spec.variant_names().enumerate() {
                let canonical = format!("{}::{variant_name}", spec.canonical_type_name);
                ctx.machine_ctor_registry.insert(
                    canonical,
                    (spec.canonical_type_name.to_string(), variant_idx),
                );
                // Register the bare form only when count == 1 (unique) AND
                // the user has not declared their own variant with this name.
                if bare_counts.get(variant_name).copied().unwrap_or(0) == 1
                    && !user_declared_variant_names.contains(variant_name)
                {
                    ctx.machine_ctor_registry.insert(
                        variant_name.to_string(),
                        (spec.canonical_type_name.to_string(), variant_idx),
                    );
                }
            }
        }
    }

    // Seed `enum_variants_by_name`, `enum_type_params`, and `enum_item_ids`
    // for the built-in tagged unions. These mirror what the type-decl second
    // pass below populates for user enums; the variant payload types reference
    // the generic type-parameter names (e.g. `Named { name: "T", args: [] }`)
    // so `substitute_type_params` in `try_register_enum_instantiation`
    // produces correctly substituted per-instantiation layouts that flow into
    // MIR via `module.enum_layouts`.
    for spec in BUILTIN_ENUM_SPECS {
        let variants = builtin_enum_hir_variants(spec);
        ctx.enum_variants_by_name
            .insert(spec.canonical_type_name.to_string(), variants);
        ctx.enum_type_params.insert(
            spec.canonical_type_name.to_string(),
            spec.type_params.iter().map(|s| (*s).to_string()).collect(),
        );
        ctx.enum_item_ids
            .insert(spec.canonical_type_name.to_string(), spec.item_id);
        // Tag-only / monomorphic builtin enums (e.g. `LookupError`) need a
        // `type_classes` registration so MIR `push_unknown_type_diagnostics`
        // does not flag them, and so `ValueClass::of_ty` resolves them as
        // `BitCopy` (no payload → no drop work). Generic builtin enums
        // (`Option`, `Result`) are skipped here: their per-instantiation
        // origin name is added to `machine_layout_names` via the
        // `enum_layouts.iter().map(origin_name)` chain in `hew-mir/src/lower.rs`,
        // and their `ValueClass` is computed on the substituted variants.
        if spec.type_params.is_empty() {
            ctx.type_classes.insert(
                spec.canonical_type_name.to_string(),
                (ResourceMarker::BitCopy, None),
            );
        }
    }

    // Discard pre-pass diagnostics from `lower_type`; the third pass re-emits
    // any real ones when it produces the canonical HirTypeDecl/HirRecordDecl.
    ctx.diagnostics.clear();
    ctx.diagnostics.extend(builtin_impl_diagnostics);

    // FC-P1-D: HIR pre-pass binary-operator gates. Dispatched HERE (after
    // diagnostics.clear above) so the gate's diagnostics survive into the
    // final LowerOutput. Unconditional across all targets — these gates
    // close MIR sites that are unsupported regardless of target until MIR
    // gains TargetSpec threading (see audit `:5336`, `:5564`, `:5696`).
    check_binary_operator_gates(&mut ctx, program);

    // FC-P1-A3: Supervisor spawn args gate. Same survival-ordering rationale
    // as the wasm gate above — dispatched AFTER `ctx.diagnostics.clear()` so
    // the SupervisorSpawnArgsUnsupported diagnostics survive into the final
    // LowerOutput. The checker already rejects supervisor declarations with
    // init params; this HIR gate is defense-in-depth that catches any future
    // surface which reaches MIR (`hew-mir/src/lower.rs:8852`) before the
    // checker guard does. Per slepp A222: compile-time fail-closed instead of
    // a `NotYetImplemented` runtime-style diagnostic at MIR-lowering time.
    check_supervisor_spawn_gate(&mut ctx, program);

    // FC-P1-E: Vec<T> index/slice element-type gates. Dispatched HERE (after
    // the diagnostics.clear above) so the gate's VecIndex/Slice element-type
    // diagnostics survive into the final LowerOutput. Target-independent: the
    // runtime ABI lacks `hew_vec_get_T` / `hew_vec_slice_range_T` for the
    // rejected element types on every target.
    check_vec_index_element_type_gates(&mut ctx, program);

    // Second pass: lower type declarations and populate the per-module
    // type-class registry. Stored here so the source-order pass can emit them
    // in program order without a second lowering call. Function bodies depend
    // on type markers (so `ValueClass::of_ty` resolves `Named` types
    // correctly), but type-decl bodies do not depend on function signatures,
    // so this pre-pass can safely run before the combined item pass below.
    let mut type_decl_cache: HashMap<*const hew_parser::ast::TypeDecl, HirTypeDecl> =
        HashMap::new();
    let mut diagnostic_source_modules: HashMap<ItemId, String> = HashMap::new();
    for (item_idx, (item, span)) in program.items.iter().enumerate() {
        ctx.current_item_ordinal = item_idx;
        if let Item::TypeDecl(decl) = item {
            ctx.current_module_idx = file_import_module_idx.get(&item_idx).copied().unwrap_or(0);
            ctx.current_module_name = span_indices
                .module_name(ctx.current_module_idx)
                .map(str::to_string);
            let diag_start = ctx.diagnostics.len();
            let lowered = if let Some(module) = ctx.current_module_name.clone() {
                ctx.lower_imported_type_decl(decl, span.clone(), &module)
            } else {
                ctx.lower_type_decl(decl, span.clone())
            };
            ctx.tag_spliced_diagnostics(diag_start);
            let Some(hir_decl) = lowered else {
                continue;
            };
            let marker = hir_decl.marker;
            let close_method = if marker == ResourceMarker::Resource {
                hir_decl
                    .consuming_methods
                    .iter()
                    .find(|m| m.as_str() == "close")
                    .cloned()
                    // W3.030 Q-α-B: a `#[resource]` whose `close` lives in a
                    // sibling inherent-impl block must still register the
                    // close-method symbol so the MIR elaborator's
                    // `resource_drop_fn` emits `Some("<T>::close")` rather
                    // than `None`. Without this fallback the type-class
                    // table would record `(Resource, None)` for the broadened
                    // surface and drop-elaboration would silently elide the
                    // close call.
                    .or_else(|| {
                        ctx.inherent_close_signature(&hir_decl.declaration)
                            .map(|_| "close".to_string())
                    })
            } else {
                None
            };
            let class_entry = (marker, close_method);
            ctx.type_classes
                .insert(hir_decl.name.clone(), class_entry.clone());
            if hir_decl.defining_module.is_some() {
                ctx.type_classes
                    .insert(hir_decl.qualified_name(), class_entry);
            }
            // Structural member set for the mailbox-transfer walk: record
            // fields plus every variant's payload types. Registered under the
            // same keys as `type_classes` so a module-qualified message type
            // resolves identically.
            let member_tys: Vec<ResolvedTy> = hir_decl
                .fields
                .iter()
                .map(|field| field.ty.clone())
                .chain(hir_decl.variants.iter().flat_map(hew_hir_variant_field_tys))
                .collect();
            ctx.type_member_tys
                .insert(hir_decl.name.clone(), member_tys.clone());
            if hir_decl.defining_module.is_some() {
                ctx.type_member_tys
                    .insert(hir_decl.qualified_name(), member_tys);
            }
            // Snapshot the enum's variant descriptors so call/struct-init
            // lowering can resolve payload ctors to `MachineVariantCtor`
            // without re-walking the parser AST.
            if hir_decl.kind == HirTypeDeclKind::Enum {
                // Keep this metadata under the declaration identity, not the
                // leaf spelling. The root program can define `Delivery`
                // while the generated prelude also contributes
                // `std.builtins.Delivery`; the latter must not replace the
                // source enum's variants or layout origin.
                let enum_name = hir_decl.qualified_name();
                ctx.enum_variants_by_name
                    .insert(enum_name.clone(), hir_decl.variants.clone());
                if hir_decl.is_indirect {
                    ctx.indirect_enum_names.insert(enum_name);
                }
            }
            // Snapshot type-params and ItemId for the enum-layout discovery
            // pass (slice 2). Needed to substitute variant payload types and
            // to build EnumMonoKey.origin. Stored even for non-generic enums
            // (empty type_params) so discovery can safely skip them. ENUMS
            // only: a struct sharing a prelude generic enum's name (e.g. a
            // user `type Result { handle: i64 }`) must not overwrite the
            // prelude's `enum_type_params` entry, or every later
            // `try_register_enum_instantiation` for that enum silently
            // no-ops and codegen-front fails with registration-mismatch.
            if decl.kind == TypeDeclKind::Enum {
                let enum_name = hir_decl.qualified_name();
                ctx.enum_type_params
                    .insert(enum_name.clone(), hir_decl.type_params.clone());
                ctx.enum_item_ids.insert(enum_name, hir_decl.id);
            }
            type_decl_cache.insert(decl as *const _, hir_decl);
        }
    }
    ctx.current_module_idx = 0;
    ctx.current_module_name = None;
    for (item_idx, (item, span)) in program.items.iter().enumerate() {
        ctx.current_item_ordinal = item_idx;
        ctx.current_module_idx = file_import_module_idx
            .get(&item_idx)
            .copied()
            .unwrap_or_default();
        ctx.current_module_name = span_indices
            .module_name(ctx.current_module_idx)
            .map(str::to_string);
        if let Item::Machine(machine) = item {
            ctx.register_machine_ctor_variant_metadata(None, machine, span);
        }
    }
    ctx.current_module_idx = 0;
    ctx.current_module_name = None;
    if let Some(ref mg) = program.module_graph {
        for mod_id in &mg.topo_order {
            if *mod_id == mg.root || file_import_modules.contains(mod_id) {
                continue;
            }
            if let Some(module) = mg.modules.get(mod_id) {
                let source_module = mod_id.path.join(".");
                let diag_start = ctx.diagnostics.len();
                // #2202: canonicalise this module's imported enum-payload and
                // machine state/event member aliases under its own context.
                let saved_module_name = ctx.current_module_name.replace(source_module.clone());
                let saved_module_idx = ctx.current_module_idx;
                for (item_idx, (item, span)) in module.items.iter().enumerate() {
                    ctx.current_item_ordinal = item_idx;
                    ctx.current_module_idx = span_indices
                        .item_index(mod_id, item_idx)
                        .unwrap_or_default();
                    match item {
                        Item::TypeDecl(decl)
                            if decl.visibility.is_pub() && decl.kind == TypeDeclKind::Enum =>
                        {
                            let Some(hir_decl) = ctx.lower_type_decl(decl, span.clone()) else {
                                continue;
                            };
                            let canonical_name = format!("{source_module}.{}", hir_decl.name);
                            if hir_decl.kind == HirTypeDeclKind::Enum {
                                ctx.enum_variants_by_name
                                    .insert(canonical_name.clone(), hir_decl.variants.clone());
                                if hir_decl.is_indirect {
                                    ctx.indirect_enum_names.insert(canonical_name.clone());
                                }
                            }
                            ctx.enum_type_params
                                .insert(canonical_name.clone(), hir_decl.type_params.clone());
                            ctx.enum_item_ids.insert(canonical_name, hir_decl.id);
                        }
                        Item::Machine(machine) if machine.visibility.is_pub() => {
                            ctx.register_machine_ctor_variant_metadata(
                                Some(&source_module),
                                machine,
                                span,
                            );
                        }
                        // No enum-variant metadata for these items in imported
                        // modules. Compiler enforces exhaustivity if a new
                        // Item variant is added.
                        Item::Import(_)
                        | Item::Const(_)
                        | Item::TypeDecl(_)
                        | Item::TypeAlias(_)
                        | Item::Trait(_)
                        | Item::Impl(_)
                        | Item::Function(_)
                        | Item::ExternBlock(_)
                        | Item::Machine(_)
                        | Item::Actor(_)
                        | Item::Supervisor(_)
                        | Item::Record(_) => {}
                    }
                }
                ctx.tag_diagnostics_since(diag_start, &source_module);
                ctx.current_module_idx = saved_module_idx;
                ctx.current_module_name = saved_module_name;
            }
        }
    }

    // Discovery-only layout universe (#2755): imported record/enum decls that
    // the visibility guard below excludes from HIR-item emission (a module-
    // private generic record behind a pub API). Collected under each owning
    // module's context in §4b so their field types resolve correctly, then
    // handed to `run_layout_mono_pass` so a `Vec<PrivateSlot<T>>` element
    // reached through a substituted body gets its `RecordLayout` registered.
    // These are NEVER pushed into `items` — no naming-surface leak.
    let mut layout_universe_decls: Vec<HirItem> = Vec::new();

    // §4b — Imported-module type-decl + machine pre-pass.
    //
    // Mirror the root second pass above for `program.module_graph` non-root
    // modules so cross-module enum and machine variant descriptors are
    // available before any function body in either the root or an imported
    // module is lowered. Without this, `lookup_variant_ctor` (which consults
    // `enum_variants_by_name`) misses for imported enum tuple/struct variants
    // and for imported machine struct-states, and struct-init lowering falls
    // through to the regular-record path which then fails at MIR.
    //
    // For imported enum TypeDecls: lower once via `lower_type_decl`, cache
    // the result so the fourth-pass HirItem-emit walk reuses it (avoiding
    // double lowering and the resulting `ItemId` mismatch between the id
    // seeded into `enum_item_ids` and the id stamped on the emitted
    // `HirItem::TypeDecl`).
    //
    // For imported machines: synthesise `HirVariant` descriptors directly
    // from the AST state + event lists (machines have no `HirTypeDecl`),
    // seeding `enum_variants_by_name` under both the machine name (states)
    // and the synthesised `{Name}Event` companion (events). The checker's
    // `register_machine_decl` produces the same two `TypeDef` entries; this
    // is the HIR-side symmetric producer.
    if let Some(ref mg) = program.module_graph {
        for mod_id in &mg.topo_order {
            if *mod_id == mg.root || file_import_modules.contains(mod_id) {
                continue;
            }
            if let Some(module) = mg.modules.get(mod_id) {
                let source_module = mod_id.path.join(".");
                let diag_start = ctx.diagnostics.len();
                // #2202: §4b lowers imported type-decl + machine members into
                // the HIR descriptors MIR consumes; resolve their alias-typed
                // members under this module's own context.
                let saved_module_name = ctx.current_module_name.replace(source_module.clone());
                let saved_module_idx = ctx.current_module_idx;
                for (item_idx, (item, span)) in module.items.iter().enumerate() {
                    ctx.current_item_ordinal = item_idx;
                    ctx.current_module_idx = span_indices
                        .item_index(mod_id, item_idx)
                        .unwrap_or_default();
                    match item {
                        Item::TypeDecl(decl)
                            if decl.visibility.is_pub()
                                || decl.kind == TypeDeclKind::Enum
                                || (decl.kind == TypeDeclKind::Struct
                                    && decl.type_params.is_none()) =>
                        {
                            let Some(hir_decl) =
                                ctx.lower_imported_type_decl(decl, span.clone(), &source_module)
                            else {
                                continue;
                            };
                            let close_method = if hir_decl.marker == ResourceMarker::Resource {
                                hir_decl
                                    .consuming_methods
                                    .iter()
                                    .find(|m| m.as_str() == "close")
                                    .cloned()
                                    .or_else(|| {
                                        ctx.inherent_close_signature(&hir_decl.declaration)
                                            .map(|_| "close".to_string())
                                    })
                            } else {
                                None
                            };
                            let bare_entry = ctx
                                .type_classes
                                .entry(hir_decl.name.clone())
                                .or_insert((hir_decl.marker, close_method.clone()));
                            if hir_decl.marker == ResourceMarker::Resource
                                && bare_entry.1.is_none()
                                && close_method.is_some()
                            {
                                bare_entry.1.clone_from(&close_method);
                            }
                            let qualified_entry = ctx
                                .type_classes
                                .entry(format!("{source_module}.{}", hir_decl.name))
                                .or_insert((hir_decl.marker, close_method.clone()));
                            if hir_decl.marker == ResourceMarker::Resource
                                && qualified_entry.1.is_none()
                                && close_method.is_some()
                            {
                                qualified_entry.1 = close_method;
                            }
                            if hir_decl.kind == HirTypeDeclKind::Enum {
                                ctx.enum_variants_by_name.insert(
                                    format!("{source_module}.{}", hir_decl.name),
                                    hir_decl.variants.clone(),
                                );
                            }
                            // Enum-layout discovery registries are seeded for
                            // ENUMS only, mirroring the root-module pre-pass
                            // above. An imported STRUCT must not insert under
                            // its bare name: a record sharing a prelude generic
                            // enum's name (e.g. sqlite's `type Result`) would
                            // overwrite `enum_type_params["Result"]` with an
                            // empty param list, silently turning every later
                            // `try_register_enum_instantiation` for that enum
                            // into a no-op (the ask-site `Result<R, ActorError>`
                            // layout then never registers and codegen-front
                            // fails closed with registration-mismatch).
                            if decl.kind == TypeDeclKind::Enum {
                                ctx.enum_type_params.insert(
                                    format!("{source_module}.{}", hir_decl.name),
                                    hir_decl.type_params.clone(),
                                );
                                ctx.enum_item_ids.insert(
                                    format!("{source_module}.{}", hir_decl.name),
                                    hir_decl.id,
                                );
                            }
                            type_decl_cache.insert(decl as *const _, hir_decl);
                        }
                        Item::Machine(md) => {
                            ctx.register_machine_ctor_variant_metadata(
                                Some(&source_module),
                                md,
                                span,
                            );
                        }
                        // #2755: a type-decl the emission guard above excluded
                        // (a module-private generic struct — pub/enum/non-generic
                        // structs are handled by the guarded arm) never becomes a
                        // `HirItem`, so `run_layout_mono_pass` has no decl for it
                        // and a `Vec<Slot<T>>` element behind a pub API fails
                        // closed with a missing `RecordLayout`. Lower its shape
                        // once under this module's context, then (a) register it
                        // in `record_registry` (bare + qualified) so an imported
                        // fn body lowered later keeps the concrete type args on a
                        // `Slot { .. }` StructInit's `expr.ty` — without this the
                        // args are dropped and MIR probes the bare `Slot` field
                        // order — and (b) hand it to the discovery-only layout
                        // universe. No `HirItem` is emitted: visibility still
                        // governs NAMING (the checker's authority and the emit
                        // guard are untouched), only ABI discovery is widened.
                        Item::TypeDecl(decl) => {
                            let Some(hir_decl) =
                                ctx.lower_imported_type_decl(decl, span.clone(), &source_module)
                            else {
                                continue;
                            };
                            let entry_fields: Vec<(String, ResolvedTy)> = hir_decl
                                .fields
                                .iter()
                                .map(|f| (f.name.clone(), f.ty.clone()))
                                .collect();
                            ctx.record_registry.insert(
                                format!("{source_module}.{}", hir_decl.name),
                                RecordEntry {
                                    id: hir_decl.id,
                                    type_params: hir_decl.type_params.clone(),
                                    fields: entry_fields.clone(),
                                },
                            );
                            ctx.record_registry
                                .entry(hir_decl.name.clone())
                                .or_insert_with(|| RecordEntry {
                                    id: hir_decl.id,
                                    type_params: hir_decl.type_params.clone(),
                                    fields: entry_fields,
                                });
                            layout_universe_decls.push(HirItem::TypeDecl(hir_decl));
                        }
                        // A module-private record excluded from emission (the
                        // emission arm admits only pub records) — same rationale.
                        // No enum-variant/machine descriptors to cache for
                        // these items in imported modules (§4b pre-pass). If
                        // a new Item variant is added, the compiler will force
                        // a conscious decision here. Item::Machine is handled
                        // by the arm above; pub records are emitted elsewhere.
                        Item::Import(_)
                        | Item::Const(_)
                        | Item::TypeAlias(_)
                        | Item::Trait(_)
                        | Item::Impl(_)
                        | Item::Function(_)
                        | Item::ExternBlock(_)
                        | Item::Actor(_)
                        | Item::Supervisor(_)
                        | Item::Record(_) => {}
                    }
                }
                ctx.tag_diagnostics_since(diag_start, &source_module);
                ctx.current_module_idx = saved_module_idx;
                ctx.current_module_name = saved_module_name;
            }
        }
    }

    // Project structural ownership from the checker's canonical classifier.
    // It resolves nested declarations directly, so HIR needs no fixed-point
    // inference over a second ownership table.
    let classes = hew_types::value_class::ClassContext::new(&ctx.type_declarations);
    for declaration in type_decl_cache.values() {
        if declaration.marker == ResourceMarker::None
            && declaration.kind == HirTypeDeclKind::Struct
            && declaration.type_params.is_empty()
            && !declaration.fields.is_empty()
            && ctx
                .type_classes
                .get(&declaration.name)
                .is_some_and(|(marker, _)| *marker == ResourceMarker::None)
            && declaration.fields.iter().all(|field| {
                hew_types::ValueClass::of_ty(&field.ty, &classes)
                    == Ok(hew_types::ValueClass::BitCopy)
            })
        {
            if let Some((marker, _)) = ctx.type_classes.get_mut(&declaration.name) {
                *marker = ResourceMarker::BitCopy;
            }
        }
    }

    // Register executable std builtins.hew impl methods only after all user
    // item IDs have been preallocated. This makes builtin bodies visible to
    // source-body lowering without perturbing stable user `ItemId`s. Ordinary
    // trait impls use the same module-qualified symbols as imported impls;
    // receiver-specific cursor and duration impls retain their compiler owner.
    if let Some(program) = &builtin_callable_impl_program {
        for (item, _) in &program.items {
            if let Item::ExternBlock(block) = item {
                for function in &block.functions {
                    ctx.register_extern_fn_entry(function);
                }
            }
            if let Item::Impl(impl_decl) = item {
                if !is_builtin_callable_impl(item) {
                    continue;
                }
                if let TypeExpr::Named { name, .. } = &impl_decl.target_type.0 {
                    let receiver_specific = is_builtin_receiver_impl(item);
                    let symbol_owner = if receiver_specific {
                        injected_builtin_impl_symbol_owner(name).to_string()
                    } else {
                        imported_impl_symbol_self_name("std.builtins", name)
                    };
                    let impl_type_params = impl_type_param_names(impl_decl);
                    for method in &impl_decl.methods {
                        let emitted_symbol =
                            crate::node::HirImplBlock::method_symbol(&symbol_owner, &method.name);
                        let source_symbol =
                            crate::node::HirImplBlock::method_symbol(name, &method.name);
                        let declaration = ctx
                            .impl_method_declaration_ids
                            .get(&emitted_symbol)
                            .or_else(|| ctx.impl_method_declaration_ids.get(&source_symbol))
                            .cloned()
                            .or_else(|| {
                                builtin_callable_impl_output.as_ref().and_then(|output| {
                                    output
                                        .impl_method_declaration_ids
                                        .get(&emitted_symbol)
                                        .or_else(|| {
                                            output.impl_method_declaration_ids.get(&source_symbol)
                                        })
                                        .cloned()
                                })
                            });
                        let selected_by_checker = declaration.as_ref().is_some_and(|declaration| {
                            ctx.direct_call_targets.values().any(|target| {
                                matches!(target, hew_types::CallTarget::ImplMethod(selected) if selected == declaration)
                            }) || ctx.method_call_rewrites.values().any(|rewrite| match rewrite {
                                hew_types::MethodCallRewrite::RewriteToFunction { target, .. }
                                | hew_types::MethodCallRewrite::RewriteModuleQualifiedToFunction {
                                    target,
                                    ..
                                } => matches!(target, hew_types::CallTarget::ImplMethod(selected) if selected == declaration),
                                _ => false,
                            })
                        });
                        if (!receiver_specific && !selected_by_checker)
                            || ctx.fn_registry.contains_key(&emitted_symbol)
                        {
                            continue;
                        }
                        ctx.register_impl_method_fn_entry(&symbol_owner, method, &impl_type_params);
                        // Establish the declaration-keyed body plan for the
                        // injected builtin impl NOW, before any user body is
                        // lowered in the third pass. The body itself is emitted
                        // later in the fourth pass (`lower_impl_block` under
                        // `lowering_injected_items`), but a direct method call
                        // such as `x.fmt()` inside a user body resolves its
                        // callee through `registered_impl_method_symbol` at
                        // third-pass time — before that emission. Without the
                        // plan entry the projection is absent and the call fails
                        // closed (`CallableUnsupportedInMir`), even though the
                        // identical value renders fine through the f-string
                        // Display path, which never consults this projection.
                        // `plan_imported_impl_bodies` covers user/imported
                        // bodies; the compiler-injected builtins live in a
                        // separate program, so they must be planned here.
                        if let Some(declaration) = declaration {
                            ctx.impl_body_plan
                                .compiler_selected
                                .insert(declaration.clone());
                            ctx.impl_body_plan
                                .symbols
                                .entry(declaration)
                                .or_insert_with(|| emitted_symbol.clone());
                        }
                        builtin_callable_impl_method_symbols.insert(emitted_symbol);
                    }
                }
            }
        }
    }

    // Establish every executable impl body before lowering any source body.
    // In particular, a root function can call a flattened file-import or a
    // package method whose HIR function is emitted later in the fourth pass.
    // This plan is declaration-keyed and uses the same imported-body skip
    // authority as that fourth pass; `fn_registry` never serves as proof.
    plan_imported_impl_bodies(
        &mut ctx,
        program,
        &file_import_module_idx,
        &file_import_modules,
        &preferred_modules,
        &span_indices,
        compiling_prelude_manifest,
    );

    // Third pass: emit all items in source order now that both fn signatures
    // and type markers are fully resolved.
    //
    // Spliced file-import items (flattened into the tail of `program.items` by
    // `flatten_file_import_items`) were validated by the checker under their
    // originating module's non-root `current_module_idx`, so their bodies must
    // be lowered under that same index for every `mk_key` lookup (actor-state
    // guards, closure facts, await reads, range bounds, channel/stream
    // rewrites, expr types) to resolve to the checker's facts. Genuine root
    // items stay at index 0. See `file_import_item_module_indices`.
    // Companion table: module_idx (1-based topo order) → FULL dotted module name
    // (e.g. "subpkg.helper"). Used alongside `file_import_module_idx` to set
    // `current_module_name` when lowering file-import items, mirroring the
    // checker's `Checker::current_module` (`mod_id.path.join(".")`). Keying by
    // the full path — not the short last segment — is what lets HIR's
    // `import_type_name_aliases` lookups hit the keys the checker wrote for
    // depth-≥2 importers.
    // Prelude declarations must precede lazy body checking, independently of
    // whether their executable methods are needed or have checked successfully.
    let scope_failure = builtin_declarations.as_ref().and_then(|builtins| {
        let (source, span) = builtins.items.iter().find_map(|(item, span)| match item {
            Item::TypeDecl(decl) if decl.name == "ScopeFailure" => Some((decl, span)),
            _ => None,
        })?;
        let canonical_name = "std.builtins.ScopeFailure";
        let Some(declaration) = ctx.identity.declaration_by_path(canonical_name).cloned() else {
            ctx.unsupported(
                span.clone(),
                "scope failure declaration identity",
                "checker-boundary",
            );
            return None;
        };
        let mut source = source.clone();
        source.name = canonical_name.to_string();
        let decl = ctx.lower_type_decl_with_identity(&source, span.clone(), declaration)?;
        ctx.type_classes
            .insert(canonical_name.to_string(), (decl.marker, None));
        ctx.type_member_tys.insert(
            canonical_name.to_string(),
            decl.variants
                .iter()
                .flat_map(hew_hir_variant_field_tys)
                .collect(),
        );
        ctx.enum_variants_by_name
            .insert(canonical_name.to_string(), decl.variants.clone());
        ctx.enum_type_params
            .insert(canonical_name.to_string(), decl.type_params.clone());
        ctx.enum_item_ids
            .insert(canonical_name.to_string(), decl.id);
        for (index, variant) in decl.variants.iter().enumerate() {
            ctx.machine_ctor_registry.insert(
                format!("{canonical_name}::{}", variant.name),
                (canonical_name.to_string(), index),
            );
        }
        Some(decl)
    });
    let mut delivery_declarations = Vec::new();
    if let Some(builtins) = builtin_declarations.as_ref() {
        for name in hew_types::actor_delivery::DECLARATIONS {
            let Some((source, span)) = builtins.items.iter().find_map(|(item, span)| match item {
                Item::TypeDecl(decl) if decl.name == *name => Some((decl, span)),
                _ => None,
            }) else {
                continue;
            };
            let canonical_name = format!("std.builtins.{name}");
            let Some(declaration) = ctx.identity.declaration_by_path(&canonical_name).cloned()
            else {
                ctx.unsupported(
                    span.clone(),
                    "actor delivery declaration identity",
                    "checker-boundary",
                );
                continue;
            };
            let mut source = source.clone();
            source.name.clone_from(&canonical_name);
            let Some(decl) = ctx.lower_type_decl_with_identity(&source, span.clone(), declaration)
            else {
                continue;
            };
            ctx.type_classes
                .insert(canonical_name.clone(), (decl.marker, None));
            ctx.type_member_tys.insert(
                canonical_name.clone(),
                decl.fields
                    .iter()
                    .map(|field| field.ty.clone())
                    .chain(decl.variants.iter().flat_map(hew_hir_variant_field_tys))
                    .collect(),
            );
            if decl.kind == HirTypeDeclKind::Struct {
                ctx.record_registry.insert(
                    canonical_name.clone(),
                    RecordEntry {
                        id: decl.id,
                        type_params: decl.type_params.clone(),
                        fields: decl
                            .fields
                            .iter()
                            .map(|field| (field.name.clone(), field.ty.clone()))
                            .collect(),
                    },
                );
            }
            if decl.kind == HirTypeDeclKind::Enum {
                ctx.enum_variants_by_name
                    .insert(canonical_name.clone(), decl.variants.clone());
                ctx.enum_type_params
                    .insert(canonical_name.clone(), decl.type_params.clone());
                ctx.enum_item_ids.insert(canonical_name.clone(), decl.id);
                for (index, variant) in decl.variants.iter().enumerate() {
                    ctx.machine_ctor_registry.insert(
                        format!("{canonical_name}::{}", variant.name),
                        (canonical_name.clone(), index),
                    );
                }
            }
            delivery_declarations.push(decl);
        }
    }
    let mut items: Vec<HirItem> = Vec::new();
    let mut const_fold_module_idx = 0;
    for (item_idx, (item, span)) in program.items.iter().enumerate() {
        ctx.current_item_ordinal = item_idx;
        ctx.current_module_idx = file_import_module_idx.get(&item_idx).copied().unwrap_or(0);
        ctx.current_module_name = span_indices
            .module_name(ctx.current_module_idx)
            .map(str::to_string);
        if ctx.current_module_idx != const_fold_module_idx {
            ctx.folded_integer_consts.clear();
            const_fold_module_idx = ctx.current_module_idx;
        }
        let diag_start = ctx.diagnostics.len();
        match item {
            Item::TypeDecl(decl) => {
                // Retrieve the already-lowered decl so diagnostics are not
                // emitted a second time.
                if let Some(hir_decl) = type_decl_cache.remove(&(decl as *const _)) {
                    items.push(HirItem::TypeDecl(hir_decl));
                }
            }
            Item::Function(func) => {
                // If this function carries `#[intrinsic("key")]`, the checker
                // recorded it in `intrinsic_declarations`. Validate the key
                // against the stdlib catalog and skip body lowering — the
                // catalog entry already supplies the semantics. Fail-closed:
                // an unknown key produces `UnknownIntrinsic` instead of a
                // silently-incorrect lowering.
                if let Some(intrinsic_key) = ctx.intrinsic_declarations.get(&func.name).cloned() {
                    let known = crate::stdlib_catalog::entries()
                        .iter()
                        .any(|e| e.name == intrinsic_key);
                    if !known {
                        ctx.diagnostics.push(HirDiagnostic::new(
                            HirDiagnosticKind::UnknownIntrinsic {
                                fn_name: func.name.clone(),
                                intrinsic_key,
                            },
                            span.clone(),
                            "intrinsic key not found in stdlib catalog; \
                             check the #[intrinsic(\"..\")] argument matches a catalog entry name",
                        ));
                    }
                    // Either way, do not lower a body — the declaration is a
                    // typed substrate stub that must match an existing catalog entry.
                } else {
                    let Some(mut hir_fn) = ctx.lower_fn(func, span.clone()) else {
                        // A refused body already pushed its diagnostics; this
                        // is the one path out of the loop that skips the tag
                        // below.
                        ctx.tag_spliced_diagnostics(diag_start);
                        continue;
                    };
                    // The process adapter owns the external `main` symbol. If
                    // a selected test displaces an authored source `main`, keep
                    // that declaration as an ordinary callable and give it a
                    // stable internal HIR symbol. The exact checker `DefId`
                    // remains the call authority: MIR's direct-call index maps
                    // that declaration to this symbol, so neither call sites
                    // nor codegen rediscover the target from its spelling.
                    if let Some(symbol) = ctx.fn_symbol_overrides.get(&hir_fn.id) {
                        hir_fn.name.clone_from(symbol);
                    }
                    // Positive root-origin record: a free function lowered from
                    // the root file (module index 0) has a body span that
                    // indexes the root compilation unit's source, so codegen may
                    // render a fail-closed caret against it. Injected items
                    // (`lowering_injected_items`) are excluded even at index 0 —
                    // their spans index a library file, not root.
                    if ctx.current_module_idx == 0 && !ctx.lowering_injected_items {
                        ctx.root_item_ids.insert(hir_fn.id);
                    }
                    items.push(HirItem::Function(hir_fn));
                }
            }
            Item::Impl(impl_decl) => {
                let imported_symbol_self_name = ctx.current_module_name.as_ref().and_then(|_| {
                    ctx.impl_body_plan
                        .symbol_self_names
                        .get(&(impl_decl as *const _))
                        .cloned()
                });
                let skip_methods = HashSet::new();
                let imported = imported_symbol_self_name
                    .as_deref()
                    .map(|symbol_self_name| ImportedImplLowering {
                        skip_methods: &skip_methods,
                        symbol_self_name: Some(symbol_self_name),
                    });
                ctx.lower_impl_block(
                    impl_decl,
                    span.clone(),
                    &mut items,
                    false,
                    imported.as_ref(),
                );
            }
            Item::Actor(actor) => {
                // P0.1: Fail-closed gate: actors require the actor runtime ABI.
                // wasm32 is admitted: `hew-runtime/src/scheduler_wasm.rs` provides
                // the cooperative actor scheduler, mailbox, and coroutine substrate
                // with C ABI parity to the native scheduler. `TargetArch::Other`
                // (unknown/unsupported triples) remains gated — no scheduler exists
                // for those targets (#1821).
                if matches!(ctx.target_arch, TargetArch::Other) {
                    ctx.diagnostics.push(HirDiagnostic::new(
                        HirDiagnosticKind::TargetCoroutineUnsupported {
                            target_arch: format!("{:?}", ctx.target_arch),
                            construct: "actor decl".to_string(),
                        },
                        span.clone(),
                        format!(
                            "actor '{}' requires the actor runtime ABI (unsupported target)",
                            actor.name
                        ),
                    ));
                }
                let defining_module = ctx.current_module_name.clone();
                if let Some(actor) =
                    ctx.lower_actor(actor, span.clone(), defining_module.as_deref())
                {
                    // Positive root-origin record, on the same terms as a free
                    // function: an actor declared in the root file gives its
                    // handlers bodies that index the root source, so a
                    // fail-closed caret and native debug metadata may name it.
                    if ctx.current_module_idx == 0 && !ctx.lowering_injected_items {
                        ctx.root_item_ids.insert(actor.id);
                    }
                    items.push(HirItem::Actor(actor));
                }
            }
            Item::Record(decl) => {
                if let Some(record) = ctx.lower_record_decl(decl, span.clone()) {
                    items.push(HirItem::Record(record));
                }
            }
            Item::Supervisor(decl) => {
                // P0.2: Fail-closed gate: supervisor restart machinery
                // (`SupervisorChildGet`, `Stop`, nested-restart) is not yet
                // available on wasm32 (#1475). Supervisors remain restricted
                // to x86_64/aarch64 until that work lands.
                if !matches!(ctx.target_arch, TargetArch::X86_64 | TargetArch::Aarch64) {
                    ctx.diagnostics.push(HirDiagnostic::new(
                        HirDiagnosticKind::TargetCoroutineUnsupported {
                            target_arch: format!("{:?}", ctx.target_arch),
                            construct: "supervisor decl".to_string(),
                        },
                        span.clone(),
                        format!(
                            "supervisor '{}' requires supervisor restart machinery \
                             (x86_64/aarch64 only; wasm32 support tracked in #1475)",
                            decl.name
                        ),
                    ));
                }
                if let Some(supervisor) = ctx.lower_supervisor(decl, span.clone()) {
                    items.push(HirItem::Supervisor(supervisor));
                }
            }
            Item::Import(_) | Item::TypeAlias(_) | Item::Machine(_) => {
                // The checker normalizes every machine into ordinary enum,
                // report and impl declarations before HIR, so no machine
                // declaration reaches lowering.
                // Imports are frontend-resolved: module-path imports
                // (`import greeting;`) are lowered from `program.module_graph`
                // below under their qualified mangled name (e.g. `greeting$hello`).
                // File-path imports (`import "util.hew";`) are still flattened
                // into `program.items` by `flatten_import_items` and lowered in
                // the loop above.  The residual `Item::Import` stub is kept in
                // `program.items` for diagnostic source-map attribution (removing
                // it would require auditing every span consumer — out of scope).
                // Type aliases likewise have no runtime artifact: checker and
                // annotation lowering expand them to their underlying storage.
            }
            Item::Trait(trait_decl) => {
                // User-defined `trait` declarations have no runtime artefact —
                // the type checker harvests their `trait_defs` entry (and any
                // `#[lang_item("...")]` registry mapping) during its registration
                // sweep, and impl-side method bodies become flattened
                // `HirItem::Function` entries via `lower_impl_block`.
                //
                // RAII-2 (#1295): a BODYLESS trait method signature is an
                // invisible-body boundary — a contract whose impls may disagree
                // on whether a `#[resource]`/`#[linear]` value parameter is
                // borrowed or consumed, so the disposition must be pinned with
                // `consume` at the signature. A default-bodied method carries a
                // visible body the MIR fixpoint can scan, so it is exempt.
                for trait_item in &trait_decl.items {
                    if let TraitItem::Method(method) = trait_item {
                        if method.body.is_none() {}
                    }
                }
            }
            Item::Const(const_decl) => {
                if let Some(constant) = ctx.lower_const(const_decl, span.clone()) {
                    items.push(HirItem::Const(constant));
                }
            }
            Item::ExternBlock(block) => {
                for (func_index, func) in block.functions.iter().enumerate() {
                    let param_tys = func
                        .params
                        .iter()
                        .map(|p| ctx.lower_type(&p.ty))
                        .collect::<Vec<_>>();
                    let param_consume =
                        func.params.iter().map(|p| p.is_consume).collect::<Vec<_>>();
                    let return_ty = func
                        .return_type
                        .as_ref()
                        .map_or(ResolvedTy::Unit, |ret| ctx.lower_type(ret));
                    let provenance = extern_provenance(ctx.current_module_name.as_deref());
                    let runtime_capability = extern_runtime_capability(&provenance, &func.name);
                    let Some(declaration) = ctx.source_declaration(
                        span,
                        hew_types::DeclarationKind::ExternFunction,
                        func_index,
                    ) else {
                        continue;
                    };
                    items.push(HirItem::ExternFn(crate::node::HirExternFn {
                        id: ctx.ids.item(),
                        node: ctx.ids.node(),
                        declaration,
                        name: func.name.clone(),
                        abi: block.abi.clone(),
                        param_tys,
                        param_consume,
                        return_ty,
                        // Defining-module provenance from the SAME current-module
                        // authority this pass sets per item (line ~3355): `None`
                        // is the genuine root compilation unit (module index 0),
                        // `Some(dotted)` a file-import module. Threaded to MIR so
                        // C-ABI string-return ownership is classified from a proven
                        // fact, not `diagnostic_source_modules` absence.
                        provenance,
                        runtime_capability,
                        span: func.span.clone(),
                    }));
                }
            }
        }
        ctx.tag_spliced_diagnostics(diag_start);
    }
    // Restore the root index after the file-import-aware third pass so any
    // subsequent root-context reads default to 0 before the module-graph walk
    // re-assigns it per non-root module.
    ctx.current_module_idx = 0;
    ctx.current_module_name = None;

    // Fourth pass: lower pub fn bodies from non-root modules under their
    // qualified, native-symbol-safe names (e.g. `greeting$hello`).
    //
    // Module-path imports (`import greeting;`) are NOT flattened into
    // `program.items` (see `flatten_import_items` restriction in
    // `hew-compile`), so the bodies would otherwise be absent from the
    // emitted `HirModule`.  Walking `module_graph` here ensures each
    // imported user module contributes exactly one `HirFn` per pub fn,
    // keyed by the same mangled name registered in the pre-pass above.
    //
    // Pub TypeDecls are emitted here for the same reason: imported stdlib
    // record types must appear as `HirItem::TypeDecl` so that `hew-mir`'s layout
    // pass (which walks `module.items`) can populate `record_field_orders` and
    // emit a `RecordLayout`. Without this, field accesses like `info.code` in
    // `#[on(crash)]` bodies fail at MIR time because the payload record layout
    // is absent from `record_field_orders`.
    if let Some(ref mg) = program.module_graph {
        // A file-imported module's declarations are lowered by the source-order
        // third pass: `flatten_file_import_items` splices them into
        // `program.items` under the declaring file's module index, and that pass
        // owns every kind they can carry. The same modules are also in
        // `mg.topo_order`, so this walk skips them by identity — lowering an
        // item twice emits two `HirItem`s for one declaration, which downstream
        // reads as two realizations of one identity (duplicate
        // `<SelfType>::<method>` symbols and LLVM verifier failures for impls,
        // a doubled HIR refusal for an extern or type declaration, two
        // lifecycle admissions for one `#[resource]` record).
        //
        // The discriminator is module ORIGIN, never a bare type/trait name:
        // Hew permits distinct modules to declare same-named types and impls,
        // so a name-keyed skip could silently drop a package-import declaration
        // that merely shares a name. `file_import_module_ids` holds exactly the
        // root's spliced file-import chain; package modules are never in it and
        // are lowered here exactly once.
        //
        // Prefer a source-specific package module's impl over a byte-identical
        // copy absorbed by a directory superset. Unique impls in the superset
        // still lower normally. See `preferred_package_module_ids`.
        // Read the checker's per-SOURCE-FILE span index. The checker stamps
        // `SpanKey`s in `expr_types` with it, so HIR must assign the same index
        // when looking those keys back up or `self.mk_key(span)` resolves to
        // another file's entry and byte-offset collisions across files are
        // misread as same-file types.
        for mod_id in &mg.topo_order {
            if *mod_id == mg.root || file_import_modules.contains(mod_id) {
                continue;
            }
            if let Some(module) = mg.modules.get(mod_id) {
                let module_idx = span_indices.module_base(mod_id).unwrap_or_default();
                ctx.current_module_idx = module_idx;
                let source_module = mod_id.path.join(".");
                // Match the checker's `current_module` key (full dotted path,
                // `mod_id.path.join(".")`) so `import_type_name_aliases` lookups
                // resolve for depth-≥2 modules (e.g. "subpkg.helper"); the short
                // last segment would miss the checker-written alias key.
                ctx.current_module_name = Some(source_module.clone());
                let diag_start = ctx.diagnostics.len();
                let item_start = items.len();
                // Per-module helper sets used by the imported-body scan in
                // both the free-fn (Item::Function) and impl-method
                // (Item::Impl) arms. Computed once per module so the two
                // arms agree on which same-module callees count as
                // private vs. pub and so iteration cost is linear in the
                // module's item count rather than quadratic across arms.
                let same_module_private_fns: HashSet<String> = module
                    .items
                    .iter()
                    .filter_map(|(it, _)| {
                        if let Item::Function(f) = it {
                            if !f.visibility.is_pub() {
                                return Some(f.name.clone());
                            }
                        }
                        None
                    })
                    .collect();
                let imported_private_closure = collect_imported_private_fn_closure(
                    module,
                    &same_module_private_fns,
                    ctx.trait_defaults
                        .values()
                        .flatten()
                        .filter(|default| {
                            default.source_module.as_deref() == Some(source_module.as_str())
                        })
                        .filter_map(|default| default.method.body.as_ref()),
                );
                let same_module_actor_rewrites: HashMap<String, String> = module
                    .items
                    .iter()
                    .filter_map(|(it, _)| {
                        if let Item::Actor(actor) = it {
                            Some((
                                actor.name.clone(),
                                format!("{source_module}.{}", actor.name),
                            ))
                        } else {
                            None
                        }
                    })
                    .collect();
                let prev_actor_rewrites = ctx
                    .imported_actor_rewrites
                    .replace(same_module_actor_rewrites);
                // Populate bare-name const scope for this module's own consts.
                // Functions inside the module reference module-level consts by
                // bare name (e.g. `STATUS_OK`), but the global `const_registry`
                // holds them under qualified keys `"module_short.STATUS_OK"`.
                // Both pub AND private consts are registered in the pre-pass
                // under their qualified keys; this map shadows them by bare
                // name so identifier resolution inside imported bodies finds
                // them without requiring the caller to qualify the access.
                let module_consts_scope: HashMap<String, ConstEntry> = module
                    .items
                    .iter()
                    .filter_map(|(it, _)| {
                        if let Item::Const(cd) = it {
                            let qualified = format!("{source_module}.{}", cd.name);
                            ctx.const_registry
                                .get(&qualified)
                                .map(|entry| (cd.name.clone(), entry.clone()))
                        } else {
                            None
                        }
                    })
                    .collect();
                let prev_module_consts = ctx.imported_module_consts.replace(module_consts_scope);

                let previous_folded_integer_consts = std::mem::take(&mut ctx.folded_integer_consts);
                for (item_idx, (item, span)) in module.items.iter().enumerate() {
                    ctx.current_item_ordinal = item_idx;
                    // Re-key per item: a directory module's peer files share
                    // this module but each owns its own span index.
                    ctx.current_module_idx = span_indices
                        .item_index(mod_id, item_idx)
                        .unwrap_or(module_idx);
                    match item {
                        Item::Function(func) if func.visibility.is_pub() => {
                            if item_is_duplicated_in_distinct_leaf_module(
                                program,
                                &preferred_modules,
                                mod_id,
                                item,
                                span,
                            ) {
                                continue;
                            }
                            let qualified = crate::mangle_dotted_name(&format!(
                                "{source_module}.{}",
                                func.name
                            ));
                            if let Some(lowered) = ctx.lower_imported_fn_floor_aware(
                                func,
                                &qualified,
                                &source_module,
                                span.clone(),
                            ) {
                                items.push(HirItem::Function(lowered));
                            }
                        }
                        Item::Function(func)
                            if imported_private_closure.contains(func.name.as_str()) =>
                        {
                            if item_is_duplicated_in_distinct_leaf_module(
                                program,
                                &preferred_modules,
                                mod_id,
                                item,
                                span,
                            ) {
                                continue;
                            }
                            let qualified = crate::mangle_dotted_name(&format!(
                                "{source_module}.{}",
                                func.name
                            ));
                            if let Some(lowered) = ctx.lower_imported_fn_floor_aware(
                                func,
                                &qualified,
                                &source_module,
                                span.clone(),
                            ) {
                                items.push(HirItem::Function(lowered));
                            }
                        }
                        Item::TypeDecl(decl)
                            if decl.visibility.is_pub()
                                || decl.kind == TypeDeclKind::Enum
                                || (decl.kind == TypeDeclKind::Struct
                                    && decl.type_params.is_none()) =>
                        {
                            if item_is_duplicated_in_preferred_module(
                                program,
                                &preferred_modules,
                                mod_id,
                                item,
                            ) {
                                continue;
                            }
                            // Consume the cached `HirTypeDecl` produced by the
                            // §4b imported-module pre-pass so the emitted item
                            // shares the same `ItemId` already seeded into
                            // `enum_item_ids`. Re-lowering here would mint a
                            // fresh id and silently drift the registry's view
                            // of the enum from the emitted decl. Non-enum
                            // TypeDecls (records) were not cached above — fall
                            // back to lowering them inline.
                            let hir_decl = if let Some(hir_decl) =
                                type_decl_cache.remove(&(decl as *const _))
                            {
                                hir_decl
                            } else {
                                let Some(hir_decl) = ctx.lower_imported_type_decl(
                                    decl,
                                    span.clone(),
                                    &source_module,
                                ) else {
                                    continue;
                                };
                                hir_decl
                            };
                            items.push(HirItem::TypeDecl(hir_decl));
                        }
                        // Emit HirItem::ExternFn entries for extern declarations
                        // in imported modules so MIR/codegen sees them in the
                        // lowered item list. Mirrors the root-item arm at the
                        // third pass.
                        Item::ExternBlock(block) => {
                            for (func_index, func) in block.functions.iter().enumerate() {
                                let param_tys = func
                                    .params
                                    .iter()
                                    .map(|p| ctx.lower_type(&p.ty))
                                    .collect::<Vec<_>>();
                                let param_consume =
                                    func.params.iter().map(|p| p.is_consume).collect::<Vec<_>>();
                                let return_ty = func
                                    .return_type
                                    .as_ref()
                                    .map_or(ResolvedTy::Unit, |ret| ctx.lower_type(ret));
                                let provenance =
                                    extern_provenance(ctx.current_module_name.as_deref());
                                let runtime_capability =
                                    extern_runtime_capability(&provenance, &func.name);
                                let Some(declaration) = ctx.source_declaration(
                                    span,
                                    hew_types::DeclarationKind::ExternFunction,
                                    func_index,
                                ) else {
                                    continue;
                                };
                                items.push(HirItem::ExternFn(crate::node::HirExternFn {
                                    id: ctx.ids.item(),
                                    node: ctx.ids.node(),
                                    declaration,
                                    name: func.name.clone(),
                                    abi: block.abi.clone(),
                                    param_tys,
                                    param_consume,
                                    return_ty,
                                    // Fourth-pass imported modules always carry a
                                    // named `current_module_name` (set to the
                                    // dotted `source_module` at the top of this
                                    // loop). Same authority as the third-pass site
                                    // above so a std vs. user/package extern is
                                    // classified identically regardless of which
                                    // pass emitted it.
                                    provenance,
                                    runtime_capability,
                                    span: func.span.clone(),
                                }));
                            }
                        }
                        // Emit `HirItem::Function` entries for the methods of an
                        // imported impl block so that MIR/codegen can process
                        // cross-module method calls on named types.
                        //
                        // No per-method `pub` gate: impl methods in Hew have no
                        // independent visibility (impl bodies never carry
                        // `pub fn`; access is governed by the trait/type), so a
                        // `pub` filter excluded ALL imported impl methods. That
                        // left non-trivial builder methods (e.g. JSON `with_*` /
                        // `push_*`, whose bodies are a void C call followed by
                        // `return self`, so they are NOT captured by the runtime
                        // handle-method pass-through path) without a MIR body,
                        // surfacing as `CallableUnsupportedInMir` /
                        // `IndirectCallUnsupported` at the import boundary. The
                        // root impl-block path emits every method
                        // (`pub_only = false`); the imported path now matches.
                        //
                        // A method whose body calls a private (non-pub) free
                        // function in the same module that is NOT in the imported
                        // private-fn closure cannot be lowered (the helper is not
                        // in the emitted item list and has no qualified rewrite).
                        // Such methods are skipped — matching the prior behaviour
                        // where every imported impl method was dropped — so the
                        // module still imports cleanly. If an importer actually
                        // CALLS a skipped method it fails closed downstream with
                        // `CallableUnsupportedInMir`, exactly as before this
                        // change. (Lowering those bodies requires registering the
                        // private-helper closure reachable from impl methods, a
                        // follow-up to the free-fn closure already wired here.)
                        Item::Impl(impl_decl) => {
                            // `std/prelude.hew` is the compiler's import-only
                            // authority manifest. Its explicit `std.builtins`
                            // edge selects the same implicit declarations whose
                            // executable receiver bodies are injected below;
                            // it must not materialise a second body set through
                            // the package-import path.
                            if compiling_prelude_manifest && source_module == "std.builtins" {
                                continue;
                            }
                            // Skip impl blocks of FILE-import modules: their
                            // items were spliced into `program.items` and
                            // already lowered by the source-order third pass.
                            // A directory module may absorb the same impl that a
                            // source-specific submodule also contributes. Keep
                            // the source-specific copy so its qualified type and
                            // impl symbol remain aligned; skip only the duplicate
                            // impl in the superset, not the superset's unique
                            // impls.
                            if item_is_duplicated_in_preferred_module(
                                program,
                                &preferred_modules,
                                mod_id,
                                item,
                            ) {
                                continue;
                            }
                            if let TypeExpr::Named {
                                name: self_type_name,
                                ..
                            } = &impl_decl.target_type.0
                            {
                                // Conservatively lower only the imported impl
                                // methods that are provably safe cross-module;
                                // skip the rest so the module still imports
                                // cleanly and an actual call to a skipped method
                                // fails closed downstream.
                                //
                                // A method is skipped when EITHER:
                                //  - its body calls a bare name that resolves in
                                //    neither the same-module rewrite map,
                                //    `fn_registry` (which by now holds every
                                //    seeded stdlib/runtime symbol and every
                                //    same-module extern fn), a lexically-bound
                                //    fn-typed parameter, nor the source builtin
                                //    overload set — catches codegen-intercepted
                                //    builtins that are not extern-declared, e.g.
                                //    `Stream.recv` → `hew_stream_next_layout`; OR
                                //  - its signature names a user type that would
                                //    not resolve at the MIR boundary — a
                                //    cross-module dotted type (`fs.IoError`) or a
                                //    user trait/type used as a generic argument
                                //    (an actor handle such as `WebSocketHandler`). Only
                                //    primitives/builtins and the impl's own self
                                //    type are admitted.
                                //
                                // This is intentionally tight: it captures the
                                // fluent-builder shape (e.g. JSON `with_*` /
                                // `push_*`, params `string`/`i64`/`f64`/`bool`,
                                // returning the opaque self handle) without
                                // eagerly lowering methods that would unmask
                                // pre-existing per-module cross-module-resolution
                                // gaps. Lifting the signature restriction needs
                                // imported user-type/trait registration at the
                                // MIR boundary — a separate lane.
                                // Generic type parameters in scope on the impl
                                // block. A signature naming one (`Option<B>` on
                                // `impl<I, A, B> Iterator for Map<I, A, B>`) is a
                                // carrier resolved at monomorphisation time, not a
                                // The pre-lowering body plan and this emitter
                                // share one exact eligibility authority.
                                let skip_methods =
                                    ctx.imported_impl_skip_methods(impl_decl, &source_module);
                                // Impl method symbols are declaration-owned,
                                // not collision-owned. Consume the canonical
                                // owner established by the declaration-keyed
                                // pre-lowering plan rather than reconstructing
                                // it from the source spelling.
                                let planned_symbol_self_name = ctx
                                    .impl_body_plan
                                    .symbol_self_names
                                    .get(&(impl_decl as *const _))
                                    .cloned();
                                if planned_symbol_self_name.is_none() {
                                    ctx.diagnostics.push(HirDiagnostic::new(
                                        HirDiagnosticKind::CheckerBoundaryViolation {
                                            name: format!(
                                                "impl body `{source_module}.{self_type_name}`"
                                            ),
                                            reason:
                                                "no pre-lowering canonical owner".to_string(),
                                        },
                                        span.clone(),
                                        "imported implementation body has no declaration-keyed owner plan",
                                    ));
                                    continue;
                                }
                                ctx.lower_impl_block(
                                    impl_decl,
                                    span.clone(),
                                    &mut items,
                                    false,
                                    Some(&ImportedImplLowering {
                                        skip_methods: &skip_methods,
                                        symbol_self_name: planned_symbol_self_name.as_deref(),
                                    }),
                                );
                            }
                        }
                        // Emit HirItem::Const for ALL consts from imported modules
                        // (both pub and private).  Pub consts are externally
                        // accessible via `module.CONST`; private consts are
                        // visible only inside the module's own function bodies.
                        // Both shapes produce `BindingRef::Const(id)` references
                        // and both require a `HirItem::Const` descriptor so that
                        // MIR's `build_const_descriptors` emits the global and
                        // `Instr::ConstGlobalLoad` resolves correctly at codegen.
                        //
                        // The pre-pass registered every const under its qualified key
                        // `"module_short.CONST_NAME"`.  `lower_const` looks up
                        // `const_registry[decl.name]` for the pre-allocated ItemId,
                        // so we temporarily alias the qualified entry under the bare
                        // name, lower, then remove the alias to avoid polluting the
                        // global registry.
                        Item::Const(const_decl) => {
                            let qualified = format!("{source_module}.{}", const_decl.name);
                            if let Some(entry) = ctx.const_registry.get(&qualified).cloned() {
                                ctx.const_registry.insert(const_decl.name.clone(), entry);
                                let lowered = ctx.lower_const(const_decl, span.clone());
                                ctx.const_registry.remove(&const_decl.name);
                                if let Some(lowered) = lowered {
                                    items.push(HirItem::Const(lowered));
                                }
                            }
                        }
                        // Emit `HirItem::Actor` entries for imported actors
                        // so MIR's actor-layout pass (which walks `module.items`)
                        // builds a layout keyed by the actor's bare name. Without
                        // it, `spawn module.Actor(...)` and the subsequent
                        // `receive fn` calls fail closed at MIR with
                        // `spawn of unknown actor` / `actor call on unknown actor`,
                        // even though HIR/types resolved the cross-module
                        // reference. Mirrors the `Item::Machine` arm above. The
                        // receive-fn bodies resolve names in their checker file scope
                        // active (see `lower_imported_actor`) so bare same-module
                        // calls resolve to their qualified symbols, exactly like
                        // the imported free-fn path.
                        // Visibility governs what a program may name, not what
                        // the module needs to run: a module's own non-pub actor
                        // is still spawned by its pub functions, so it needs its
                        // layout here.
                        Item::Actor(actor) => {
                            // Fail-closed target gate: actors require the actor
                            // runtime ABI (x86_64/aarch64), same as the root-item
                            // actor arm in the source-order emit pass.
                            if !matches!(ctx.target_arch, TargetArch::X86_64 | TargetArch::Aarch64)
                            {
                                ctx.diagnostics.push(HirDiagnostic::new(
                                    HirDiagnosticKind::TargetCoroutineUnsupported {
                                        target_arch: format!("{:?}", ctx.target_arch),
                                        construct: "actor decl".to_string(),
                                    },
                                    span.clone(),
                                    format!(
                                        "actor '{}' requires the actor runtime ABI \
                                         (x86_64/aarch64 only)",
                                        actor.name
                                    ),
                                ));
                            }
                            let lowered =
                                ctx.lower_imported_actor(actor, span.clone(), &source_module);
                            if let Some(lowered) = lowered {
                                items.push(HirItem::Actor(lowered));
                            }
                        }
                        // A supervisor declared in a module needs its
                        // `HirItem` for the same reason an actor does: SIR
                        // matches a spawned handle to the declaration this
                        // emits, and without it `spawn module.Rack` reaches
                        // lowering with a handle no declaration answers. The
                        // root arm gates the target the same way.
                        Item::Supervisor(supervisor) => {
                            if !matches!(ctx.target_arch, TargetArch::X86_64 | TargetArch::Aarch64)
                            {
                                ctx.diagnostics.push(HirDiagnostic::new(
                                    HirDiagnosticKind::TargetCoroutineUnsupported {
                                        target_arch: format!("{:?}", ctx.target_arch),
                                        construct: "supervisor decl".to_string(),
                                    },
                                    span.clone(),
                                    format!(
                                        "supervisor '{}' requires supervisor restart machinery \
                                         (x86_64/aarch64 only; wasm32 support tracked in #1475)",
                                        supervisor.name
                                    ),
                                ));
                            }
                            if let Some(lowered) = ctx.lower_supervisor(supervisor, span.clone()) {
                                items.push(HirItem::Supervisor(lowered));
                            }
                        }
                        // RAII-2 (#1295): a PACKAGE-imported trait is just as
                        // much an invisible-body boundary as a root or
                        // file-flattened one. Its bodyless method signatures are
                        // a contract whose impls may disagree on whether a
                        // `#[resource]`/`#[linear]` value parameter is borrowed
                        // or consumed, so the disposition must be pinned with
                        // `consume` at the signature. The root third pass checks
                        // `Item::Trait` (above); without this arm an imported
                        // trait fell through to the no-op catch-all below, so an
                        // imported `fn put(self, item: Handle)` could cross the
                        // boundary unannotated — a drop-safety bypass. Mirror the
                        // root check here. A trait has no runtime artefact, so
                        // (like the root arm) this emits no HirItem.
                        Item::Trait(trait_decl) => {
                            for trait_item in &trait_decl.items {
                                if let TraitItem::Method(method) = trait_item {
                                    if method.body.is_none() {}
                                }
                            }
                        }
                        Item::Record(decl) => {
                            if let Some(mut record) = ctx.lower_record_decl(decl, span.clone()) {
                                record.defining_module = Some(source_module.clone());
                                items.push(HirItem::Record(record));
                            }
                        }
                        // Machines are normalized into ordinary declarations
                        // by the checker before HIR.
                        Item::Import(_)
                        | Item::Function(_)
                        | Item::TypeDecl(_)
                        | Item::TypeAlias(_)
                        | Item::Machine(_) => {}
                    }
                }
                // Restore the const scope after lowering this module's bodies.
                ctx.imported_module_consts = prev_module_consts;
                ctx.imported_actor_rewrites = prev_actor_rewrites;
                ctx.folded_integer_consts = previous_folded_integer_consts;
                ctx.tag_diagnostics_since(diag_start, &source_module);
                record_source_modules_for_items(
                    &items[item_start..],
                    &source_module,
                    &mut diagnostic_source_modules,
                );
            }
        }
        // Restore to 0 so any subsequent root-level expression lowering (e.g.
        // `check_await_task_result`) uses module_idx=0 matching the checker.
        ctx.current_module_idx = 0;
        ctx.current_module_name = None;
    }

    // Every `actor |msg| { .. }` lowered above synthesized an ordinary actor
    // declaration. Publish them as items so the mono closure, the layout
    // passes and SIR see them exactly as they see a named actor.
    items.extend(
        std::mem::take(&mut ctx.pending_lambda_actors)
            .into_iter()
            .map(HirItem::Actor),
    );

    // Inject executable std builtins.hew impls through the same lowering path
    // as user and imported impls so direct method rewrites and the
    // static-dispatch index see them. Keep them after source items to avoid
    // changing user-item ordering guarantees.
    if let (Some(program), Some(output)) = (
        &builtin_callable_impl_program,
        &builtin_callable_impl_output,
    ) {
        let item_start = items.len();
        ctx.with_typecheck_facts(output, |ctx| {
            // The builtin callable impls lower at module index 0 but their
            // bodies index `std/builtins.hew`, not the user's root source. Flag
            // the phase so the `root_item_ids` inserts in `lower_impl_block`
            // skip them — a fail-closed in a builtin method must render bare,
            // never a false caret against the root. Restored below.
            let saved_injected = ctx.lowering_injected_items;
            ctx.lowering_injected_items = true;
            let saved_some = ctx
                .machine_ctor_registry
                .insert("Some".to_string(), ("Option".to_string(), 0));
            let saved_none = ctx
                .machine_ctor_registry
                .insert("None".to_string(), ("Option".to_string(), 1));
            for (item, span) in &program.items {
                if let Item::ExternBlock(block) = item {
                    for function in &block.functions {
                        let owner = format!("std.builtins.{}", function.name);
                        let Some(declaration) = ctx.identity.declaration_by_path(&owner).cloned()
                        else {
                            continue;
                        };
                        let provenance = extern_provenance(Some("std.builtins"));
                        items.push(HirItem::ExternFn(crate::node::HirExternFn {
                            id: ctx.ids.item(),
                            node: ctx.ids.node(),
                            declaration,
                            name: function.name.clone(),
                            abi: block.abi.clone(),
                            param_tys: function
                                .params
                                .iter()
                                .map(|parameter| ctx.lower_type(&parameter.ty))
                                .collect(),
                            param_consume: function
                                .params
                                .iter()
                                .map(|parameter| parameter.is_consume)
                                .collect(),
                            return_ty: function
                                .return_type
                                .as_ref()
                                .map_or(ResolvedTy::Unit, |ty| ctx.lower_type(ty)),
                            runtime_capability: extern_runtime_capability(
                                &provenance,
                                &function.name,
                            ),
                            provenance,
                            span: function.span.clone(),
                        }));
                    }
                }
                if let Item::Impl(impl_decl) = item {
                    if is_builtin_callable_impl(item) {
                        let TypeExpr::Named { name, .. } = &impl_decl.target_type.0 else {
                            continue;
                        };
                        let receiver_specific = is_builtin_receiver_impl(item);
                        let symbol_owner = if receiver_specific {
                            injected_builtin_impl_symbol_owner(name).to_string()
                        } else {
                            imported_impl_symbol_self_name("std.builtins", name)
                        };
                        let skipped_methods: HashSet<String> = impl_decl
                            .methods
                            .iter()
                            .filter(|method| {
                                !builtin_callable_impl_method_symbols.contains(
                                    &crate::node::HirImplBlock::method_symbol(
                                        &symbol_owner,
                                        &method.name,
                                    ),
                                )
                            })
                            .map(|method| method.name.clone())
                            .collect();
                        if skipped_methods.len() == impl_decl.methods.len() {
                            continue;
                        }
                        // The checker output for the isolated builtins source
                        // keys impl declarations by its source symbol
                        // (`HashMapIter::next`). The emitted synthetic cursor
                        // body uses the canonical owner-qualified linker symbol
                        // so it cannot collide with a root user same-leaf impl.
                        // Alias the exact checker declaration onto that linker
                        // projection only inside this injected-source scope.
                        for method in &impl_decl.methods {
                            let source_symbol =
                                crate::node::HirImplBlock::method_symbol(name, &method.name);
                            let emitted_symbol = crate::node::HirImplBlock::method_symbol(
                                &symbol_owner,
                                &method.name,
                            );
                            if !builtin_callable_impl_method_symbols.contains(&emitted_symbol) {
                                continue;
                            }
                            if let Some(declaration) = ctx
                                .impl_method_declaration_ids
                                .get(&emitted_symbol)
                                .or_else(|| ctx.impl_method_declaration_ids.get(&source_symbol))
                                .cloned()
                                .or_else(|| {
                                    output
                                        .impl_method_declaration_ids
                                        .get(&emitted_symbol)
                                        .or_else(|| {
                                            output.impl_method_declaration_ids.get(&source_symbol)
                                        })
                                        .cloned()
                                })
                            {
                                ctx.impl_method_declaration_ids
                                    .insert(emitted_symbol, declaration);
                            }
                        }
                        if receiver_specific {
                            ctx.lower_impl_block(impl_decl, span.clone(), &mut items, false, None);
                        } else {
                            let imported = ImportedImplLowering {
                                skip_methods: &skipped_methods,
                                symbol_self_name: Some(&symbol_owner),
                            };
                            ctx.lower_impl_block(
                                impl_decl,
                                span.clone(),
                                &mut items,
                                false,
                                Some(&imported),
                            );
                        }
                    }
                }
            }
            if let Some(previous) = saved_some {
                ctx.machine_ctor_registry
                    .insert("Some".to_string(), previous);
            } else {
                ctx.machine_ctor_registry.remove("Some");
            }
            if let Some(previous) = saved_none {
                ctx.machine_ctor_registry
                    .insert("None".to_string(), previous);
            } else {
                ctx.machine_ctor_registry.remove("None");
            }
            ctx.lowering_injected_items = saved_injected;
        });
        record_source_modules_for_items(
            &items[item_start..],
            "std.builtins",
            &mut diagnostic_source_modules,
        );
    }

    // An `#[extern_symbol]` method declares its C boundary with its own Hew
    // signature. Emit one extern declaration per dispatched method so later
    // stages read the declared parameters, their `consume` dispositions and
    // the return type from a single shape, exactly as for an `extern` block.
    for ((declaration, _), signature) in std::mem::take(&mut ctx.extern_method_signatures) {
        let (Ok(param_tys), Ok(return_ty)) = (
            signature
                .params
                .iter()
                .map(ResolvedTy::from_ty)
                .collect::<Result<Vec<_>, _>>(),
            ResolvedTy::from_ty(&signature.result),
        ) else {
            continue;
        };
        let provenance = extern_provenance(signature.declaring_module.as_deref());
        let runtime_capability = extern_runtime_capability(&provenance, &signature.endpoint);
        items.push(HirItem::ExternFn(crate::node::HirExternFn {
            id: ctx.ids.item(),
            node: ctx.ids.node(),
            declaration,
            name: signature.endpoint,
            abi: "C".to_string(),
            param_consume: signature.consumes,
            param_tys,
            return_ty,
            provenance,
            runtime_capability,
            span: hew_parser::ast::Span::default(),
        }));
    }
    items.extend(delivery_declarations.into_iter().map(HirItem::TypeDecl));
    if let Some(decl) = scope_failure {
        items.push(HirItem::TypeDecl(decl));
    }

    // Monomorphic builtin enums (e.g. `LookupError`) intentionally do NOT
    // appear in `items` here. Their declarations live in
    // `std/builtins.hew` and their tagged-union layout is registered
    // out-of-band into MIR via
    // `hew-mir::register_builtin_monomorphic_enum_layouts`, which reads
    // the catalog in `hew_types::builtin_enums::monomorphic_builtin_enums`.
    // Aside from the explicit Vec iterator impl harness entries above, this
    // keeps `HirProgram::items` a faithful mirror of user source — an earlier
    // prototype injected a synthetic `HirItem::TypeDecl` here for every
    // monomorphic builtin enum and leaked the type into the downstream
    // sandbox-VM bytecode descriptor table of every program, including ones
    // that never referenced `Node::lookup`. Generic builtin enums (`Option`,
    // `Result`) continue to flow through `EnumLayoutRegistry` per-instantiation
    // (see below).

    // A `Result<(), E>` process entry calls the checker-selected
    // `Display::fmt` target from its generated boundary adapter. That edge has
    // no source call expression, so it must enter the same monomorphisation
    // registry explicitly or a generic impl body is never materialized.
    if let Some((declaration, type_args)) =
        entry_exit_plan
            .as_ref()
            .and_then(|plan| match &plan.action {
                hew_types::EntryExitAction::Result { display, .. } => match display {
                    hew_types::EntryDisplayTarget::Declared {
                        declaration,
                        instance: hew_types::EntryCallableInstance::Generic { type_args },
                    } => Some((declaration, type_args)),
                    // A concrete target needs no specialization, and an erased
                    // one dispatches through a vtable slot rather than a body.
                    hew_types::EntryDisplayTarget::Declared { .. }
                    | hew_types::EntryDisplayTarget::DynSlot { .. } => None,
                },
                hew_types::EntryExitAction::Unit | hew_types::EntryExitAction::Integer(_) => None,
            })
    {
        if let Some(function) = items.iter().find_map(|item| match item {
            HirItem::Function(function) if &function.declaration == declaration => Some(function),
            _ => None,
        }) {
            let key = MonoKey {
                origin: function.id,
                declaration: declaration.clone(),
                linker_symbol: function.name.clone(),
                type_args: type_args.clone(),
            };
            if ctx.mono_registry.insert(key).is_err() && !ctx.mono_cap_diag_emitted {
                ctx.mono_cap_diag_emitted = true;
                ctx.diagnostics.push(HirDiagnostic::new(
                    HirDiagnosticKind::MonomorphisationCapExceeded { cap: mono_cap },
                    function.span.clone(),
                    "the selected process entry Display implementation exceeds the function monomorphisation cap",
                ));
            }
        }
    }

    let mut monomorphisations = ctx.mono_registry.into_vec();
    let call_site_type_args = ctx.call_site_type_args;
    let mut record_layouts = ctx.record_layout_registry.into_vec();
    let mut enum_layouts = ctx.enum_layout_registry.into_vec();
    let supervisor_child_slots = ctx.supervisor_child_slots;
    let pool_accessor_sites = ctx.pool_accessor_sites;

    // Closure under substitution: walk every monomorphisation's origin
    // body, find inner generic-fn call sites, substitute their recorded
    // type args via the monomorphisation's substitution map, and add
    // any newly discovered concrete instantiations to the registry.
    // Repeat to a fixed point (bounded by the configured cap).
    closure_under_substitution(
        &items,
        &call_site_type_args,
        &mut monomorphisations,
        mono_cap,
        &mut ctx.diagnostics,
    );

    // Post-function-mono record/enum layout discovery. MUST run AFTER
    // `closure_under_substitution` has closed the function-mono registry —
    // a generic record/enum constructed only inside a generic fn body (e.g.
    // `fn make<T>() -> Box<T> { Box { value: x } }`) is observable as a
    // concrete `Box<i64>` layout only after `make` is substituted as
    // `make$$i64`.
    // The pass is purely additive: the origin-site path already registered
    // every concrete-site instantiation, and the dedup set is seeded from
    // those so nothing is double-counted. New entries are merged BEFORE
    // `finalize_user_record_value_classes` so each new mangled layout gets a
    // value-class assigned.
    let (extra_record_layouts, extra_enum_layouts, layout_mono_diagnostics) =
        crate::layout_mono::run_layout_mono_pass(
            &items,
            &layout_universe_decls,
            &monomorphisations,
            &record_layouts,
            &enum_layouts,
            mono_cap,
        );
    record_layouts.extend(extra_record_layouts);
    enum_layouts.extend(extra_enum_layouts);
    ctx.diagnostics.extend(layout_mono_diagnostics);

    // Extern-backed record surface: the `#[extern_symbol]` method names were
    // collected at impl lowering (the attribute does not survive onto the
    // lowered `HirFn`); `extern` block fns are folded in here from their
    // lowered signatures, qualified by their captured defining-module
    // provenance so the spellings match the `record_registry` keys.
    let mut extern_backed_records = std::mem::take(&mut ctx.extern_backed_record_names);
    for item in &items {
        if let HirItem::ExternFn(extern_fn) = item {
            for ty in extern_fn
                .param_tys
                .iter()
                .chain(std::iter::once(&extern_fn.return_ty))
            {
                for name in crate::value_class::named_type_names(ty) {
                    if let crate::node::ExternProvenance::Module(module) = &extern_fn.provenance {
                        extern_backed_records.insert(format!("{module}.{name}"));
                    }
                    extern_backed_records.insert(name);
                }
            }
        }
    }

    finalize_user_record_value_classes(
        &ctx.record_registry,
        &record_layouts,
        &extern_backed_records,
        &mut ctx.type_classes,
        &ctx.type_declarations,
    );

    // FC-P1-B: HIR pre-pass for call-shape gates. Lifts MIR's call-shape
    // fail-closed diagnostics (`hew-mir/src/lower.rs:4194` / `:4236`) to the
    // HIR boundary so unresolved-Item callees and indirect-callable
    // unresolved callees surface during HIR lowering instead of after the
    // MIR producer has begun emitting instructions for the surrounding
    // function. Runs AFTER `ctx.diagnostics.clear()` at line ~921 and after
    // `closure_under_substitution` so the callable set includes the final
    // monomorphisation list. See `check_call_shape_gates` for the predicate.
    check_call_shape_gates(&items, &monomorphisations, &mut ctx.diagnostics);

    // W3.033c Stage 2 (R244=B): dedicated post-function-mono machine
    // instantiation discovery pass. MUST run AFTER
    // `closure_under_substitution` has finished closing the function-mono
    // registry — generic-function-mediated machine instantiations (e.g.
    // `fn make<T>() -> Lifecycle<T>` instantiated as `make::<File>()`) are
    // only observable once function-mono has substituted `T` into the
    // return-type slot — and BEFORE the `HirModule` is handed to MIR for
    // `machine_layouts` build (Stage 3, out of scope for this change).
    //
    admit_opaque_resource_lifecycles(
        &items,
        &ctx.opaque_resource_candidates,
        &mut ctx.type_classes,
        &mut ctx.diagnostics,
    );
    admit_declared_opaque_resource_lifecycles(
        &items,
        &ctx.opaque_resource_candidates,
        &ctx.identity,
        &mut ctx.type_classes,
        &mut ctx.diagnostics,
    );
    admit_resource_record_lifecycles(
        &items,
        &ctx.identity,
        &ctx.resource_close_discipline_failures,
        &mut ctx.type_classes,
        &mut ctx.diagnostics,
    );

    let module = HirModule {
        indexed_place_operations: ctx.indexed_place_operations,
        items,
        diagnostic_source_modules,
        root_item_ids: ctx.root_item_ids,
        entry_exit_plan,
        wire_layouts: Arc::new(type_check_output.wire_layouts.clone()),
        type_classes: ctx.type_classes,
        monomorphisations,
        call_site_type_args,
        vec_generic_element_abi: type_check_output.vec_generic_element_abi.clone(),
        record_layouts,
        enum_layouts,
        supervisor_child_slots,
        pool_accessor_sites,
        regex_literals: ctx.regex_literals,
    };

    LowerOutput {
        module,
        diagnostics: ctx.diagnostics,
    }
}

pub(super) fn finalize_user_record_value_classes(
    record_registry: &HashMap<String, RecordEntry>,
    record_layouts: &[RecordLayout],
    extern_backed_records: &HashSet<String>,
    type_classes: &mut crate::value_class::TypeClassTable,
    declarations: &std::collections::BTreeMap<String, hew_types::value_class::DeclaredType>,
) {
    for name in record_registry.keys() {
        type_classes
            .entry(name.clone())
            .or_insert((ResourceMarker::None, None));
    }

    // Zero-field records are structurally uninferable, and the bare
    // `type Empty {}` case is pinned fail-closed by the
    // `empty_field_user_type_remains_uninferred` oracle. The one honest
    // exception is the FFI handle stand-in: a zero-field, non-generic record
    // named in the signature of an `#[extern_symbol]` method or an `extern`
    // block fn. Its values only exist behind the C ABI (the checker rewrites
    // every call to the extern symbol; the declared stub body is dead), so
    // the record is a pointer-width stand-in with no implicit drop — the same
    // class the `#[opaque]` rule assigns (`ResourceMarker::BitCopy`), with
    // release explicit through the declared FFI surface. An explicit
    // `#[resource]`/`#[linear]` marker on such a record wins: only the
    // unmarked (`None`) state is promoted.
    for (name, entry) in record_registry {
        if entry.type_params.is_empty()
            && entry.fields.is_empty()
            && extern_backed_records.contains(name)
        {
            if let Some(slot) = type_classes.get_mut(name) {
                if slot.0 == ResourceMarker::None {
                    slot.0 = ResourceMarker::BitCopy;
                }
            }
        }
    }

    let classes = hew_types::value_class::ClassContext::new(declarations);

    for (name, entry) in record_registry {
        let Some((marker, _)) = type_classes.get(name) else {
            continue;
        };
        if *marker != ResourceMarker::None
            || !entry.type_params.is_empty()
            || entry.fields.is_empty()
        {
            continue;
        }
        if entry.fields.iter().all(|(_, ty)| {
            hew_types::ValueClass::of_ty(ty, &classes) == Ok(hew_types::ValueClass::BitCopy)
        }) {
            if let Some((marker, _)) = type_classes.get_mut(name) {
                *marker = ResourceMarker::BitCopy;
            }
        }
    }

    for layout in record_layouts {
        if layout.fields.is_empty() {
            continue;
        }
        if type_classes
            .get(&layout.mangled_name)
            .is_some_and(|(marker, _)| *marker != ResourceMarker::None)
        {
            continue;
        }
        if layout.fields.iter().all(|(_, ty)| {
            hew_types::ValueClass::of_ty(ty, &classes) == Ok(hew_types::ValueClass::BitCopy)
        }) {
            type_classes.insert(layout.mangled_name.clone(), (ResourceMarker::BitCopy, None));
        } else {
            type_classes
                .entry(layout.mangled_name.clone())
                .or_insert((ResourceMarker::None, None));
        }
    }
}
