//! Whole-program lowering entry point.

use super::*;
use hew_parser::ast::Ident;

#[must_use]
#[allow(
    clippy::too_many_lines,
    reason = "three structured passes (fn pre-pass, record/type-decl pre-pass, \
              source-order emit) read more clearly here than across helpers"
)]
pub fn lower_program(
    program: &Program,
    type_check_output: &TypeCheckOutput,
    _ctx: &ResolutionCtx,
    target_arch: TargetArch,
) -> LowerOutput {
    let program = type_check_output
        .normalized_program
        .as_ref()
        .map_or(program, |normalized| &normalized.program);
    let mut ctx = LowerCtx::new(type_check_output, target_arch);
    let entry_exit_plan = type_check_output.entry_exit_plan.clone();
    // Borrow the checker's declaration uses once for all private-helper walks.
    // Source order lets each item query only its own file and span.
    let mut declaration_uses: Vec<_> = type_check_output
        .resolutions
        .iter()
        .filter_map(|(site, resolution)| match resolution {
            Resolution::Def(declaration) | Resolution::Member(declaration) => {
                Some((site, declaration))
            }
            _ => None,
        })
        .collect();
    declaration_uses.sort_unstable_by_key(|(site, _)| (site.module_idx, site.start));
    // Every index a module owns — one per source file, so a directory module's
    // peers all carry their assembled module's name.
    let span_indices = program
        .module_graph
        .as_ref()
        .map(hew_parser::module::ModuleGraph::file_span_indices)
        .unwrap_or_default();
    // The checker selected this module's full canonical source as the builtin
    // declaration authority. HIR emits that source through its prelude and
    // callable projections below; the ordinary imported-module walks must not
    // realize the same declaration identities a second time.
    let selected_builtin_graph_module = program.module_graph.as_ref().and_then(|graph| {
        graph.topo_order.iter().find(|module_id| {
            module_id.dotted() == "std.builtins"
                && graph.modules.get(*module_id).is_some_and(|module| {
                    module.source_paths.iter().any(|source| {
                        hew_types::module_registry::is_canonical_stdlib_module_source(
                            source,
                            "std.builtins",
                        )
                    })
                })
        })
    });
    if let Some(root) = ctx.defs.root_module() {
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
                    ctx.defs.module_for_path(&module_id.dotted()),
                ) {
                    ctx.declaration_module_by_file_index
                        .insert(index, identity_module);
                }
                continue;
            }
            for source in &module.source_paths {
                if let (Some(index), Some(identity_module)) = (
                    span_indices.path_index(source),
                    ctx.defs.module_for_source(source),
                ) {
                    ctx.declaration_module_by_file_index
                        .insert(index, identity_module);
                }
            }
            // A span index is allocated per (module, file), so a file that is
            // BOTH a directory module's peer and importable in its own right
            // owns two of them — `std/net/http/codec/request.hew` is one index
            // under `std.net.http.codec` and another under
            // `std.net.http.codec.request`. `path_index` keeps only the first,
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
                    .and_then(|source| ctx.defs.module_for_source(source));
                if let Some(identity_module) = identity_module {
                    ctx.declaration_module_by_file_index
                        .entry(index)
                        .or_insert(identity_module);
                }
            }
        }
    }
    ctx.seed_stdlib_fn_registry();
    let builtin_program = type_check_output.selected_builtin_program.as_deref();
    let builtin_item_index = |ordinal| {
        selected_builtin_graph_module
            .and_then(|module| span_indices.item_index(module, ordinal))
            .unwrap_or(u32::MAX)
    };
    if let Some(module) = ctx.defs.module_for_path("std.builtins") {
        ctx.declaration_module_by_file_index
            .insert(u32::MAX, module);
    }

    ctx.root_visible_source_type_short_names
        .extend(program.items.iter().filter_map(|(item, _)| match item {
            Item::TypeDecl(decl) => Some(decl.name.to_string()),
            Item::Record(decl) => Some(decl.name.to_string()),
            Item::Actor(decl) => Some(decl.name.to_string()),
            Item::Supervisor(decl) => Some(decl.name.to_string()),
            _ => None,
        }));
    if let Some(builtins) = builtin_program {
        for (item, _) in &builtins.items {
            if let Item::TypeDecl(decl) = item {
                let canonical = format!("std.builtins.{}", decl.name);
                ctx.source_type_identities.insert(canonical.clone());
                ctx.canonical_std_source_type_identities.insert(canonical);
            }
        }
    }
    if let Some(module_graph) = &program.module_graph {
        for module_id in &module_graph.topo_order {
            if *module_id == module_graph.root || selected_builtin_graph_module == Some(module_id) {
                continue;
            }
            let module_full_path = module_id.dotted();
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
                            format!("{module_full_path}.{}.Event", decl.name),
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
        match item {
            Item::Function(func) => {
                let item = ctx.register_declared_fn_entry(func.name.name.as_str(), func, span);
                if func.name == Ident::new("main") {
                    let declaration =
                        ctx.source_declaration(span, hew_types::DeclarationKind::Function, 0);
                    if let (Some(plan), Some(declaration)) = (entry_exit_plan.as_ref(), declaration)
                    {
                        if plan.entry != declaration {
                            ctx.fn_symbol_overrides.insert(
                                item,
                                authored_main_callable_symbol(&ctx.defs, declaration),
                            );
                        }
                    }
                }
            }
            Item::ExternBlock(block) => {
                // Register extern fn signatures so call sites resolve them
                // to a `BindingRef::Item` like any other top-level function.
                // Codegen pre-declares the LLVM symbol with external linkage
                // (see `predeclare_extern_decls` in hew-codegen-rs).
                for (ordinal, extern_fn) in block.functions.iter().enumerate() {
                    ctx.register_declared_extern_fn_entry(extern_fn, span, ordinal);
                }
            }
            Item::Impl(impl_decl) => {
                // Register checked impl methods under the
                // qualified `<SelfType>::<method>` name so call sites can
                // resolve them in the second pass. The Index special-case is
                // a subset of this because its methods use the same
                // `fn_registry` key shape.
                if let TypeExpr::Named {
                    path: named_path,
                    type_args: target_type_args,
                } = &impl_decl.target_type.0
                {
                    let name = &named_path.to_string(); // TRANSITION(P1): deleted by A1 commit 2
                    let impl_type_params = impl_type_parameters(&mut ctx, impl_decl);
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
                            std::borrow::Cow::Owned(crate::monomorph::mangle(name, &concrete_args))
                        }
                    } else {
                        std::borrow::Cow::Borrowed(name.as_str())
                    };
                    for method in &impl_decl.methods {
                        ctx.register_impl_method_fn_entry(&symbol_name, method, &impl_type_params);
                    }
                    // Also register trait default methods that are NOT
                    // overridden in this impl block — they need fn_registry
                    // entries so call sites resolve `Type::method` even
                    // when the body lives on the trait declaration.
                    if let Some(tb) = &impl_decl.trait_bound {
                        let overridden: HashSet<&str> = impl_decl
                            .methods
                            .iter()
                            .map(|m| m.name.name.as_str())
                            .collect();
                        if let Some(default_owner) = ctx.trait_declaration(&tb.path) {
                            // TRANSITION(P1): deleted by A1 commit 2
                            if let Some(defaults) = ctx.trait_defaults.get(&default_owner).cloned()
                            {
                                for default_method in &defaults {
                                    if !overridden
                                        .contains(default_method.method.name.name.as_str())
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
            Item::Const(const_decl) => {
                ctx.register_const_entry(const_decl, span, true);
            }
            // No fn signatures to register for the variants below in this
            // pass. If a new Item variant is added, the compiler will force a
            // conscious decision here.
            Item::Macro(_)
            | Item::MacroCall(_)
            | Item::Import(_)
            | Item::TypeDecl(_)
            | Item::TypeAlias(_)
            | Item::Trait(_)
            | Item::Machine(_)
            | Item::Record(_)
            | Item::Actor(_)
            | Item::Supervisor(_) => {}
        }
    }
    // Pre-pass: register user-module pub fn signatures under their qualified,
    // native-symbol-safe key (e.g. `greeting$hello`) so that HIR's
    // `RewriteModuleQualifiedToFunction` arm can resolve the callee `ItemId`
    // at the call site.  Only `pub` functions are callable across module
    // boundaries; private functions are invisible to importers.
    //
    // This walk runs AFTER the root-item pre-pass above so that any name
    // clash between a qualified key and a root-level function is caught by
    // the duplicate-short-module diagnostic in the frontend before HIR ingest.
    let preferred_modules = preferred_package_module_ids(program);
    // Bare record/type-decl names declared by more than one module, the
    // root included. Re-exports are deduplicated through
    // `preferred_package_module_ids`, so a stdlib module reached through two
    // paths (`std.net.http.codec` and `std.net.http.codec.request` both surfacing
    // `codec.Request`) is not counted twice. A name unique to one owner keeps
    // its bare identity (#2208).
    ctx.cross_module_colliding_record_names = program
        .module_graph
        .as_ref()
        .into_iter()
        .flat_map(|mg| mg.modules.values())
        .flat_map(|module| module.items.iter())
        .filter_map(|(item, _)| match item {
            Item::TypeDecl(decl) => Some(decl.name.to_string()),
            Item::Record(decl) => Some(decl.name.to_string()),
            _ => None,
        })
        .filter(|name| imported_type_name_collides(program, &preferred_modules, name))
        .collect();
    if let Some(ref mg) = program.module_graph {
        for mod_id in &mg.topo_order {
            if *mod_id == mg.root || selected_builtin_graph_module == Some(mod_id) {
                continue;
            }
            let module_full_path = mod_id.dotted();
            if let Some(module) = mg.modules.get(mod_id) {
                // Preserve the declaring module while consuming its checked
                // member types and attaching source diagnostics.
                let saved_module_name = ctx.current_module_name.replace(module_full_path.clone());
                let saved_module_idx = ctx.current_module_idx;
                let private_closure = collect_imported_private_fn_closure(
                    &ctx,
                    module,
                    &span_indices,
                    &type_check_output.test_entry_plans,
                    &declaration_uses,
                );
                for (item_idx, (item, item_span)) in module.items.iter().enumerate() {
                    ctx.current_item_ordinal = item_idx;
                    ctx.current_module_idx = span_indices
                        .item_index(mod_id, item_idx)
                        .unwrap_or_default();
                    match item {
                        Item::Function(func)
                            if func.visibility.is_pub() || private_closure.contains(&item_idx) =>
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
                            ctx.register_declared_fn_entry(&qualified, func, item_span);
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
                            let kind = match decl.origin {
                                hew_parser::ast::DeclarationOrigin::MachineState => {
                                    hew_types::DeclarationKind::Machine
                                }
                                hew_parser::ast::DeclarationOrigin::MachineEventType { .. } => {
                                    hew_types::DeclarationKind::MachineEventType
                                }
                                _ => hew_types::DeclarationKind::Type,
                            };
                            let Some(declaration) = ctx.source_declaration(item_span, kind, 0)
                            else {
                                continue;
                            };
                            let Some(definition) =
                                ctx.checked_member_definition(declaration, item_span)
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
                                .entry(decl.name.to_string())
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
                                ctx.checked_member_definition(declaration, item_span)
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
                                .entry(decl.name.to_string())
                                .or_insert_with(|| RecordEntry {
                                    id,
                                    type_params,
                                    fields,
                                });
                            ctx.type_classes
                                .entry(decl.name.to_string())
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
                            for (ordinal, extern_fn) in block.functions.iter().enumerate() {
                                ctx.register_declared_extern_fn_entry(
                                    extern_fn, item_span, ordinal,
                                );
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
                            if let TypeExpr::Named {
                                path: named_path, ..
                            } = &impl_decl.target_type.0
                            {
                                let name = &named_path.to_string(); // TRANSITION(P1): deleted by A1 commit 2
                                let impl_type_params = impl_type_parameters(&mut ctx, impl_decl);
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
                                        .map(|m| m.name.name.as_str())
                                        .collect();
                                    if let Some(default_owner) = ctx.trait_declaration(&tb.path)
                                    // TRANSITION(P1): deleted by A1 commit 2
                                    {
                                        if let Some(defaults) =
                                            ctx.trait_defaults.get(&default_owner).cloned()
                                        {
                                            for default_method in &defaults {
                                                if !overridden.contains(
                                                    default_method.method.name.name.as_str(),
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
                        // Register all consts from imported modules under their
                        // qualified key `"module_short.CONST_NAME"`.  Pub
                        // consts are needed for cross-module `module.CONST`
                        // field-access resolution; private consts must also be
                        // registered here so that imported function bodies that
                        // reference the module's own private consts by bare
                        // name get a stable `ItemId` that matches the
                        // `HirItem::Const` emitted in the later pass.
                        Item::Const(const_decl) => {
                            ctx.register_const_entry(const_decl, item_span, false);
                        }
                        // Non-pub Function/TypeDecl/Record fall here (not exported to importers).
                        Item::Macro(_)
                        | Item::MacroCall(_)
                        | Item::Import(_)
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
        match item {
            Item::TypeDecl(decl) => {
                let id = ctx.ids.item();
                let kind = match decl.origin {
                    hew_parser::ast::DeclarationOrigin::MachineState => {
                        hew_types::DeclarationKind::Machine
                    }
                    hew_parser::ast::DeclarationOrigin::MachineEventType { .. } => {
                        hew_types::DeclarationKind::MachineEventType
                    }
                    _ => hew_types::DeclarationKind::Type,
                };
                let Some(declaration) = ctx.source_declaration(item_span, kind, 0) else {
                    continue;
                };
                let Some(definition) = ctx.checked_member_definition(declaration, item_span) else {
                    continue;
                };
                let fields = ctx.checked_record_fields(&definition, item_span);
                let type_params = definition.type_params;
                ctx.record_registry.insert(
                    decl.name.to_string(),
                    RecordEntry {
                        id,
                        type_params,
                        fields,
                    },
                );
                ctx.type_classes
                    .insert(decl.name.to_string(), (ResourceMarker::None, None));
            }
            Item::Record(decl) => {
                let id = ctx.ids.item();
                let Some(declaration) =
                    ctx.source_declaration(item_span, hew_types::DeclarationKind::Record, 0)
                else {
                    continue;
                };
                let Some(definition) = ctx.checked_member_definition(declaration, item_span) else {
                    continue;
                };
                let fields = ctx.checked_record_fields(&definition, item_span);
                let type_params = definition.type_params;
                ctx.record_registry.insert(
                    decl.name.to_string(),
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
            Item::Macro(_)
            | Item::MacroCall(_)
            | Item::Import(_)
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
                        *bare_counts.entry(state.name.to_string()).or_insert(0) += 1;
                        *surface_ctor_counts
                            .entry(tagged_union_surface_ctor_key(
                                md.name.name.as_str(),
                                state.name.name.as_str(),
                            ))
                            .or_insert(0) += 1;
                        // Machine states shadow same-named builtins (local-shadows-global).
                        user_declared_variant_names.insert(state.name.to_string());
                    }
                    for event in &md.events {
                        *bare_counts.entry(event.name.to_string()).or_insert(0) += 1;
                        *surface_ctor_counts
                            .entry(tagged_union_surface_ctor_key(
                                &machine_event_surface_type(md.name.name.as_str()),
                                event.name.name.as_str(),
                            ))
                            .or_insert(0) += 1;
                        // Machine events shadow same-named builtins (local-shadows-global).
                        user_declared_variant_names.insert(event.name.to_string());
                    }
                }
                Item::TypeDecl(td) if td.kind == TypeDeclKind::Enum => {
                    // Count every variant — unit, tuple, struct — so the
                    // ambiguity guard fires across all surface forms (e.g. a
                    // bare `Line` is ambiguous with a `Line(i64)` variant on
                    // another enum just as it is with a unit variant).
                    for body_item in &td.body {
                        if let TypeBodyItem::Variant(v) = body_item {
                            *bare_counts.entry(v.name.to_string()).or_insert(0) += 1;
                            *surface_ctor_counts
                                .entry(tagged_union_surface_ctor_key(
                                    td.name.name.as_str(),
                                    v.name.name.as_str(),
                                ))
                                .or_insert(0) += 1;
                            // Track root-program user variants for the
                            // local-shadows-global builtin registration guard.
                            user_declared_variant_names.insert(v.name.to_string());
                        }
                    }
                }
                // No variant bare-names to count for these items. If a new
                // Item variant with enumerable named variants is added, the
                // compiler will force a conscious decision here.
                Item::Macro(_)
                | Item::MacroCall(_)
                | Item::Import(_)
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
                if *mod_id == mg.root || selected_builtin_graph_module == Some(mod_id) {
                    continue;
                }
                if let Some(module) = mg.modules.get(mod_id) {
                    for (item, _) in &module.items {
                        match item {
                            Item::Machine(md) => {
                                for state in &md.states {
                                    *bare_counts.entry(state.name.to_string()).or_insert(0) += 1;
                                    *surface_ctor_counts
                                        .entry(tagged_union_surface_ctor_key(
                                            md.name.name.as_str(),
                                            state.name.name.as_str(),
                                        ))
                                        .or_insert(0) += 1;
                                    // Machine states (pub or private) shadow same-named
                                    // builtins across the flat global registry, matching
                                    // the checker's global register_machine_decl walk.
                                    user_declared_variant_names.insert(state.name.to_string());
                                }
                                for event in &md.events {
                                    *bare_counts.entry(event.name.to_string()).or_insert(0) += 1;
                                    *surface_ctor_counts
                                        .entry(tagged_union_surface_ctor_key(
                                            &machine_event_surface_type(md.name.name.as_str()),
                                            event.name.name.as_str(),
                                        ))
                                        .or_insert(0) += 1;
                                    // Machine events shadow same-named builtins.
                                    user_declared_variant_names.insert(event.name.to_string());
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
                                        *bare_counts.entry(v.name.to_string()).or_insert(0) += 1;
                                        *surface_ctor_counts
                                            .entry(tagged_union_surface_ctor_key(
                                                td.name.name.as_str(),
                                                v.name.name.as_str(),
                                            ))
                                            .or_insert(0) += 1;
                                        user_declared_variant_names.insert(v.name.to_string());
                                    }
                                }
                            }
                            // No variant bare-names to count for these items.
                            // Compiler enforces exhaustivity if a new Item variant
                            // is added. Item::Machine is handled by the arm above.
                            Item::Macro(_)
                            | Item::MacroCall(_)
                            | Item::Import(_)
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
                    let event_type_name = format!("{}.Event", md.name);
                    for (idx, state) in md.states.iter().enumerate() {
                        let qualified = format!("{}::{}", md.name, state.name);
                        ctx.machine_ctor_registry
                            .insert(qualified, (md.name.to_string(), idx));
                        if bare_counts
                            .get(state.name.name.as_str())
                            .copied()
                            .unwrap_or(0)
                            == 1
                        {
                            ctx.machine_ctor_registry
                                .insert(state.name.to_string(), (md.name.to_string(), idx));
                        }
                    }
                    for (idx, event) in md.events.iter().enumerate() {
                        let qualified = format!("{}::{}", event_type_name, event.name);
                        ctx.machine_ctor_registry
                            .insert(qualified, (event_type_name.clone(), idx));
                        if bare_counts
                            .get(event.name.name.as_str())
                            .copied()
                            .unwrap_or(0)
                            == 1
                        {
                            ctx.machine_ctor_registry
                                .insert(event.name.to_string(), (event_type_name.clone(), idx));
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
                                .insert(qualified, (td.name.to_string(), variant_idx));
                            if bare_counts.get(v.name.name.as_str()).copied().unwrap_or(0) == 1 {
                                ctx.machine_ctor_registry
                                    .insert(v.name.to_string(), (td.name.to_string(), variant_idx));
                            }
                            variant_idx += 1;
                        }
                    }
                }
                // No ctor entries to register for these items. If a new Item
                // variant with enumerable ctors is added, the compiler will
                // force a conscious decision here.
                Item::Macro(_)
                | Item::MacroCall(_)
                | Item::Import(_)
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
                if *mod_id == mg.root || selected_builtin_graph_module == Some(mod_id) {
                    continue;
                }
                if let Some(module) = mg.modules.get(mod_id) {
                    let source_module = mod_id.dotted();
                    for (item, _) in &module.items {
                        match item {
                            Item::Machine(md) => {
                                let event_type_name = format!("{}.Event", md.name);
                                let source_state_type = format!("{source_module}.{}", md.name);
                                let source_event_type =
                                    format!("{source_module}.{event_type_name}");
                                for (idx, state) in md.states.iter().enumerate() {
                                    let module_qualified =
                                        format!("{source_module}.{}::{}", md.name, state.name);
                                    ctx.machine_ctor_registry
                                        .insert(module_qualified, (source_state_type.clone(), idx));
                                    let surface = tagged_union_surface_ctor_key(
                                        md.name.name.as_str(),
                                        state.name.name.as_str(),
                                    );
                                    if surface_ctor_counts.get(&surface).copied() == Some(1) {
                                        ctx.machine_ctor_registry
                                            .entry(surface)
                                            .or_insert_with(|| (source_state_type.clone(), idx));
                                    }
                                    if bare_counts.get(state.name.name.as_str()).copied() == Some(1)
                                    {
                                        ctx.machine_ctor_registry
                                            .entry(state.name.to_string())
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
                                        event.name.name.as_str(),
                                    );
                                    if surface_ctor_counts.get(&surface).copied() == Some(1) {
                                        ctx.machine_ctor_registry
                                            .entry(surface)
                                            .or_insert_with(|| (source_event_type.clone(), idx));
                                    }
                                    if bare_counts.get(event.name.name.as_str()).copied() == Some(1)
                                    {
                                        ctx.machine_ctor_registry
                                            .entry(event.name.to_string())
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
                                        let surface = tagged_union_surface_ctor_key(
                                            td.name.name.as_str(),
                                            v.name.name.as_str(),
                                        );
                                        if surface_ctor_counts.get(&surface).copied() == Some(1) {
                                            ctx.machine_ctor_registry
                                                .entry(surface)
                                                .or_insert_with(|| {
                                                    (source_enum_name.clone(), variant_idx)
                                                });
                                        }
                                        if bare_counts.get(v.name.name.as_str()).copied() == Some(1)
                                        {
                                            ctx.machine_ctor_registry
                                                .entry(v.name.to_string())
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
                            Item::Macro(_)
                            | Item::MacroCall(_)
                            | Item::Import(_)
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

    for spec in BUILTIN_ENUM_SPECS {
        let variants = builtin_enum_hir_variants(spec);
        ctx.enum_variants_by_name
            .insert(spec.canonical_type_name.to_string(), variants);
        ctx.enum_type_params
            .insert(spec.canonical_type_name.to_string(), spec.parameter_heads());
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
            let Some(hir_decl) = ctx.lower_type_decl(decl, span.clone()) else {
                continue;
            };
            let marker = hir_decl.marker;
            let close_method = if marker == ResourceMarker::Resource {
                ctx.inherent_close_signature(hir_decl.declaration)
                    .map(|_| "close".to_string())
            } else {
                None
            };
            let class_entry = (marker, close_method);
            ctx.type_classes
                .insert(hir_decl.name.clone(), class_entry.clone());
            if hir_decl.defining_module.is_some() {
                ctx.type_classes
                    .insert(hir_decl.qualified_name(&ctx.defs), class_entry);
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
                    .insert(hir_decl.qualified_name(&ctx.defs), member_tys);
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
                let enum_name = hir_decl.qualified_name(&ctx.defs);
                ctx.enum_variants_by_name
                    .insert(enum_name.clone(), hir_decl.variants.clone());
            }
            if decl.kind == TypeDeclKind::Enum {
                let enum_name = hir_decl.qualified_name(&ctx.defs);
                ctx.enum_type_params
                    .insert(enum_name.clone(), hir_decl.type_params.clone());
            }
            type_decl_cache.insert(decl as *const _, hir_decl);
        }
    }
    ctx.current_module_idx = 0;
    ctx.current_module_name = None;
    for (item_idx, (item, span)) in program.items.iter().enumerate() {
        ctx.current_item_ordinal = item_idx;
        if let Item::Machine(machine) = item {
            ctx.register_machine_ctor_variant_metadata(None, machine, span);
        }
    }
    ctx.current_module_idx = 0;
    ctx.current_module_name = None;
    if let Some(ref mg) = program.module_graph {
        for mod_id in &mg.topo_order {
            if *mod_id == mg.root || selected_builtin_graph_module == Some(mod_id) {
                continue;
            }
            if let Some(module) = mg.modules.get(mod_id) {
                let source_module = mod_id.dotted();
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
                            }
                            ctx.enum_type_params
                                .insert(canonical_name.clone(), hir_decl.type_params.clone());
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
                        Item::Macro(_)
                        | Item::MacroCall(_)
                        | Item::Import(_)
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

    if let Some(ref mg) = program.module_graph {
        for mod_id in &mg.topo_order {
            if *mod_id == mg.root || selected_builtin_graph_module == Some(mod_id) {
                continue;
            }
            if let Some(module) = mg.modules.get(mod_id) {
                let source_module = mod_id.dotted();
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
                        Item::TypeDecl(decl) => {
                            let Some(hir_decl) =
                                ctx.lower_imported_type_decl(decl, span.clone(), &source_module)
                            else {
                                continue;
                            };
                            let close_method = if hir_decl.marker == ResourceMarker::Resource {
                                ctx.inherent_close_signature(hir_decl.declaration)
                                    .map(|_| "close".to_string())
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
                            // Pattern lowering needs enum parameters under the
                            // canonical owner, independently of record declarations.
                            if decl.kind == TypeDeclKind::Enum {
                                ctx.enum_type_params.insert(
                                    format!("{source_module}.{}", hir_decl.name),
                                    hir_decl.type_params.clone(),
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
                        Item::Macro(_)
                        | Item::MacroCall(_)
                        | Item::Import(_)
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

    let mut lowered_type_declarations: HashSet<_> = type_decl_cache
        .values()
        .map(|declaration| declaration.declaration)
        .collect();
    let builtin_method_demands = BuiltinMethodDemands::for_program(type_check_output);
    // Register executable std builtins.hew impl methods only after all user
    // item IDs have been preallocated. This makes builtin bodies visible to
    // source-body lowering without perturbing stable user `ItemId`s. Ordinary
    // trait impls use the same module-qualified symbols as imported impls;
    // receiver-specific cursor and duration impls retain their compiler owner.
    if let Some(builtins) = builtin_program {
        let saved_index = ctx.current_module_idx;
        let saved_ordinal = ctx.current_item_ordinal;
        let saved_module = ctx.current_module_name.replace("std.builtins".to_string());
        for (ordinal, (item, span)) in builtins.items.iter().enumerate() {
            ctx.current_item_ordinal = ordinal;
            ctx.current_module_idx = builtin_item_index(ordinal);
            if let Item::Function(function) = item {
                let symbol = format!("std.builtins.{}", function.name);
                ctx.register_declared_fn_entry(&symbol, function, span);
            }
            if let Item::ExternBlock(block) = item {
                for function in &block.functions {
                    ctx.register_extern_fn_entry(function);
                    let path = format!("std.builtins.{}", function.name);
                    if let Some(declaration) = ctx.defs.lookup_path(&path).filter(|declaration| {
                        ctx.defs.kind(*declaration) == hew_types::DeclarationKind::ExternFunction
                    }) {
                        if let Some(entry) =
                            ctx.fn_registry.get(function.name.name.as_str()).cloned()
                        {
                            ctx.source_fn_entries
                                .insert(declaration, (function.name.to_string(), entry));
                        }
                    }
                }
            }
            if let Item::Impl(impl_decl) = item {
                if !is_builtin_callable_impl(item) {
                    continue;
                }
                if let TypeExpr::Named {
                    path: named_path, ..
                } = &impl_decl.target_type.0
                {
                    let name = &named_path.to_string(); // TRANSITION(P1): deleted by A1 commit 2
                    let receiver_specific = is_builtin_receiver_impl(item);
                    let symbol_owner = if receiver_specific {
                        injected_builtin_impl_symbol_owner(name).to_string()
                    } else {
                        imported_impl_symbol_self_name("std.builtins", name)
                    };
                    let impl_type_params = impl_type_parameters(&mut ctx, impl_decl);
                    for method in &impl_decl.methods {
                        let declaration = ctx.source_declaration(
                            &method.fn_span,
                            hew_types::DeclarationKind::ImplMethod,
                            0,
                        );
                        let Some(declaration) = declaration else {
                            continue;
                        };
                        let emitted_symbol = ctx.emitted_impl_method_symbol(
                            declaration,
                            &symbol_owner,
                            method.name.name.as_str(),
                        );
                        let selected_by_checker = type_check_output
                            .fn_sigs
                            .get(&declaration)
                            .is_some_and(|signature| builtin_method_demands.selects(signature));
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
                        ctx.impl_body_plan.compiler_selected.insert(declaration);
                        ctx.impl_body_plan
                            .symbols
                            .entry(declaration)
                            .or_insert_with(|| emitted_symbol.clone());
                        builtin_callable_impl_method_symbols.insert(emitted_symbol);
                    }
                }
            }
        }
        ctx.current_module_idx = saved_index;
        ctx.current_item_ordinal = saved_ordinal;
        ctx.current_module_name = saved_module;
    }

    // Establish every executable impl body before lowering any source body.
    // In particular, a root function can call an imported module's
    // method whose HIR function is emitted later in the fourth pass.
    // This plan is declaration-keyed and uses the same imported-body skip
    // authority as that fourth pass; `fn_registry` never serves as proof.
    plan_imported_impl_bodies(
        &mut ctx,
        program,
        &preferred_modules,
        &span_indices,
        selected_builtin_graph_module,
    );

    // Third pass: emit all items in source order now that both fn signatures
    // and type markers are fully resolved.
    // Prelude declarations must precede lazy body checking, independently of
    // whether their executable methods are needed or have checked successfully.
    let actor_intercept_types = ctx.lower_actor_intercept_types();
    let mut prelude_declarations = Vec::new();
    if let Some(builtins) = builtin_program {
        for (source, span) in builtins.items.iter().filter_map(|(item, span)| {
            if let Item::TypeDecl(decl) = item {
                Some((decl, span))
            } else {
                None
            }
        }) {
            let canonical_name = format!("std.builtins.{}", source.name);
            let Some(declaration) = ctx.defs.lookup_path(&canonical_name) else {
                ctx.missing_checked_fact(
                    span.clone(),
                    "source prelude declaration identity",
                    "checker-boundary",
                );
                continue;
            };
            let mut source = source.clone();
            source.name = Ident::new(&canonical_name); // TRANSITION(P1): deleted by A1 commit 2
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

                for (index, variant) in decl.variants.iter().enumerate() {
                    ctx.machine_ctor_registry.insert(
                        format!("{canonical_name}::{}", variant.name),
                        (canonical_name.clone(), index),
                    );
                }
            }
            lowered_type_declarations.insert(declaration);
            prelude_declarations.push(decl);
        }
    }
    let mut items: Vec<HirItem> = Vec::new();
    for (item_idx, (item, span)) in program.items.iter().enumerate() {
        ctx.current_item_ordinal = item_idx;
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
                if let Some(intrinsic_key) = ctx
                    .intrinsic_declarations
                    .get(func.name.name.as_str())
                    .cloned()
                {
                    let known = crate::stdlib_catalog::entries()
                        .iter()
                        .any(|e| e.name == intrinsic_key);
                    if !known {
                        ctx.diagnostics.push(HirDiagnostic::new(
                            HirDiagnosticKind::UnknownIntrinsic {
                                fn_name: func.name.to_string(),
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
                ctx.lower_impl_block(impl_decl, span.clone(), &mut items, false, None);
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
                // Native and WASI share the checked supervisor runtime.
                // Other architectures have no runtime implementation.
                if matches!(ctx.target_arch, TargetArch::Other) {
                    ctx.diagnostics.push(HirDiagnostic::new(
                        HirDiagnosticKind::TargetCoroutineUnsupported {
                            target_arch: format!("{:?}", ctx.target_arch),
                            construct: "supervisor decl".to_string(),
                        },
                        span.clone(),
                        format!(
                            "supervisor '{}' requires supervisor restart machinery \
                             (unsupported target)",
                            decl.name
                        ),
                    ));
                }
                if let Some(supervisor) = ctx.lower_supervisor(decl, span.clone()) {
                    items.push(HirItem::Supervisor(supervisor));
                }
            }
            Item::Macro(_)
            | Item::MacroCall(_)
            | Item::Import(_)
            | Item::TypeAlias(_)
            | Item::Machine(_) => {
                // The checker normalizes every machine into ordinary enum,
                // report and impl declarations before HIR, so no machine
                // declaration reaches lowering.
                // Imports are frontend-resolved: every imported module, by
                // path or by quoted file, is lowered from `program.module_graph`
                // below under its qualified mangled name (e.g. `greeting$hello`).
                // The residual `Item::Import` stub is kept in
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
                    let params = func
                        .params
                        .iter()
                        .map(|p| hew_types::CallableParameter {
                            ty: ctx.lower_type(&p.ty),
                            ownership: hew_types::ParameterOwnership::from_consume(p.is_consume),
                        })
                        .collect::<Vec<_>>();
                    let return_ty = func
                        .return_type
                        .as_ref()
                        .map_or(ResolvedTy::Unit, |ret| ctx.lower_type(ret));
                    let provenance = extern_provenance(ctx.current_module_name.as_deref());
                    let runtime_capability =
                        extern_runtime_capability(&provenance, func.name.name.as_str());
                    let Some(declaration) = ctx.source_declaration(
                        span,
                        hew_types::DeclarationKind::ExternFunction,
                        func_index,
                    ) else {
                        continue;
                    };
                    items.push(HirItem::ExternFn(crate::node::HirExternFn {
                        failure_domain: hew_types::CallableFailureDomain::Abort,
                        id: ctx.ids.item(),
                        node: ctx.ids.node(),
                        declaration,
                        name: func.name.to_string(),
                        abi: block.abi.clone(),
                        params,
                        return_ty,
                        // Defining-module provenance: this pass lowers only the
                        // root compilation unit, so `None`. Threaded to MIR so
                        // C-ABI string-return ownership is classified from a proven
                        // fact, not `diagnostic_source_modules` absence.
                        provenance,
                        runtime_capability,
                        offload: type_check_output.extern_contracts.is_offload(declaration),
                        span: func.span.clone(),
                    }));
                }
            }
        }
    }

    // Fourth pass: lower pub fn bodies from non-root modules under their
    // qualified, native-symbol-safe names (e.g. `greeting$hello`).
    //
    // Imported modules' items are not part of `program.items`, so the bodies
    // would otherwise be absent from the emitted `HirModule`.  Walking `module_graph` here ensures each
    // imported user module contributes exactly one `HirFn` per pub fn,
    // keyed by the same mangled name registered in the pre-pass above.
    //
    // Pub TypeDecls are emitted here for the same reason: imported stdlib
    // record types must appear as `HirItem::TypeDecl` so SIR can derive exact
    // semantic shapes for demanded values, including callback payloads.
    if let Some(ref mg) = program.module_graph {
        // A file import (`import "x.hew";`) is a module like any other and is
        // lowered here, once, under its own module identity.
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
            if *mod_id == mg.root || selected_builtin_graph_module == Some(mod_id) {
                continue;
            }
            if let Some(module) = mg.modules.get(mod_id) {
                let module_idx = span_indices.module_base(mod_id).unwrap_or_default();
                ctx.current_module_idx = module_idx;
                let source_module = mod_id.dotted();
                // Keep source attribution under the checker's full module owner.
                ctx.current_module_name = Some(source_module.clone());
                let diag_start = ctx.diagnostics.len();
                let item_start = items.len();
                let imported_private_closure = collect_imported_private_fn_closure(
                    &ctx,
                    module,
                    &span_indices,
                    &type_check_output.test_entry_plans,
                    &declaration_uses,
                );
                let same_module_actor_rewrites: HashMap<String, String> = module
                    .items
                    .iter()
                    .filter_map(|(it, _)| {
                        if let Item::Actor(actor) = it {
                            Some((
                                actor.name.to_string(),
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
                                .map(|entry| (cd.name.to_string(), entry.clone()))
                        } else {
                            None
                        }
                    })
                    .collect();
                let prev_module_consts = ctx.imported_module_consts.replace(module_consts_scope);

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
                        Item::Function(func) if imported_private_closure.contains(&item_idx) => {
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
                        Item::TypeDecl(decl) => {
                            if item_is_duplicated_in_preferred_module(
                                program,
                                &preferred_modules,
                                mod_id,
                                item,
                            ) {
                                continue;
                            }
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
                                let params = func
                                    .params
                                    .iter()
                                    .map(|p| hew_types::CallableParameter {
                                        ty: ctx.lower_type(&p.ty),
                                        ownership: hew_types::ParameterOwnership::from_consume(
                                            p.is_consume,
                                        ),
                                    })
                                    .collect::<Vec<_>>();
                                let return_ty = func
                                    .return_type
                                    .as_ref()
                                    .map_or(ResolvedTy::Unit, |ret| ctx.lower_type(ret));
                                let provenance =
                                    extern_provenance(ctx.current_module_name.as_deref());
                                let runtime_capability =
                                    extern_runtime_capability(&provenance, func.name.name.as_str());
                                let Some(declaration) = ctx.source_declaration(
                                    span,
                                    hew_types::DeclarationKind::ExternFunction,
                                    func_index,
                                ) else {
                                    continue;
                                };
                                items.push(HirItem::ExternFn(crate::node::HirExternFn {
                                    failure_domain: hew_types::CallableFailureDomain::Abort,
                                    id: ctx.ids.item(),
                                    node: ctx.ids.node(),
                                    declaration,
                                    name: func.name.to_string(),
                                    abi: block.abi.clone(),
                                    params,
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
                                    offload: type_check_output
                                        .extern_contracts
                                        .is_offload(declaration),
                                    span: func.span.clone(),
                                }));
                            }
                        }
                        // Emit imported implementation bodies selected by the
                        // checker-owned declaration and typed-signature facts.
                        // A missing body fact skips that exact declaration;
                        // a call to it later fails at the callable-set boundary.
                        Item::Impl(impl_decl) => {
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
                                path: named_path, ..
                            } = &impl_decl.target_type.0
                            {
                                let self_type_name = &named_path.to_string();
                                // The pre-lowering plan and this emitter share
                                // the same exact declaration eligibility set.
                                let skip_methods = ctx.imported_impl_skip_methods(impl_decl);
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
                        // The declaration's canonical entry owns its ItemId.
                        Item::Const(const_decl) => {
                            if let Some(lowered) = ctx.lower_const(const_decl, span.clone()) {
                                items.push(HirItem::Const(lowered));
                            }
                        }
                        // Emit `HirItem::Actor` entries for imported actors
                        // so MIR's actor-layout pass (which walks `module.items`)
                        // builds a layout keyed by the actor's bare name. Without
                        // it, `spawn module.Actor { ... }` and the subsequent
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
                            // runtime ABI, same as the root-item
                            // actor arm in the source-order emit pass.
                            if matches!(ctx.target_arch, TargetArch::Other) {
                                ctx.diagnostics.push(HirDiagnostic::new(
                                    HirDiagnosticKind::TargetCoroutineUnsupported {
                                        target_arch: format!("{:?}", ctx.target_arch),
                                        construct: "actor decl".to_string(),
                                    },
                                    span.clone(),
                                    format!(
                                        "actor '{}' requires the actor runtime ABI \
                                         (unsupported target)",
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
                            if matches!(ctx.target_arch, TargetArch::Other) {
                                ctx.diagnostics.push(HirDiagnostic::new(
                                    HirDiagnosticKind::TargetCoroutineUnsupported {
                                        target_arch: format!("{:?}", ctx.target_arch),
                                        construct: "supervisor decl".to_string(),
                                    },
                                    span.clone(),
                                    format!(
                                        "supervisor '{}' requires supervisor restart machinery \
                                         (unsupported target)",
                                        supervisor.name
                                    ),
                                ));
                            }
                            if let Some(lowered) = ctx.lower_supervisor(supervisor, span.clone()) {
                                items.push(HirItem::Supervisor(lowered));
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
                        Item::Trait(_)
                        | Item::Macro(_)
                        | Item::MacroCall(_)
                        | Item::Import(_)
                        | Item::Function(_)
                        | Item::TypeAlias(_)
                        | Item::Machine(_) => {}
                    }
                }
                // Restore the const scope after lowering this module's bodies.
                ctx.imported_module_consts = prev_module_consts;
                ctx.imported_actor_rewrites = prev_actor_rewrites;
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
    if let Some(builtins) = builtin_program {
        let item_start = items.len();
        let saved_module_idx = ctx.current_module_idx;
        let saved_ordinal = ctx.current_item_ordinal;
        let saved_module = ctx.current_module_name.replace("std.builtins".to_string());
        let saved_injected = ctx.lowering_injected_items;
        ctx.lowering_injected_items = true;
        let private_helpers = super::private_refs::collect_builtin_private_fn_closure(
            &mut ctx,
            builtins,
            builtin_item_index,
            &declaration_uses,
        );
        let saved_some = ctx
            .machine_ctor_registry
            .insert("Some".to_string(), ("Option".to_string(), 0));
        let saved_none = ctx
            .machine_ctor_registry
            .insert("None".to_string(), ("Option".to_string(), 1));
        for (ordinal, (item, span)) in builtins.items.iter().enumerate() {
            ctx.current_item_ordinal = ordinal;
            ctx.current_module_idx = builtin_item_index(ordinal);
            if let Item::Function(function) = item {
                let declaration =
                    ctx.source_declaration(span, hew_types::DeclarationKind::Function, 0);
                let ordinary = declaration.is_some_and(|declaration| {
                    matches!(type_check_output.source_function_targets.get(&declaration),
                        Some(CallTarget::User(target)) if *target == declaration)
                });
                if ordinary && (function.visibility.is_pub() || private_helpers.contains(&ordinal))
                {
                    if let Some(function) = ctx.lower_fn(function, span.clone()) {
                        items.push(HirItem::Function(function));
                    }
                }
            }
            if let Item::ExternBlock(block) = item {
                for function in &block.functions {
                    let owner = format!("std.builtins.{}", function.name);
                    let Some(declaration) = ctx.defs.lookup_path(&owner) else {
                        continue;
                    };
                    let provenance = extern_provenance(Some("std.builtins"));
                    items.push(HirItem::ExternFn(crate::node::HirExternFn {
                        failure_domain: hew_types::CallableFailureDomain::Abort,
                        id: ctx.ids.item(),
                        node: ctx.ids.node(),
                        declaration,
                        name: function.name.to_string(),
                        abi: block.abi.clone(),
                        params: function
                            .params
                            .iter()
                            .map(|parameter| hew_types::CallableParameter {
                                ty: ctx.lower_type(&parameter.ty),
                                ownership: hew_types::ParameterOwnership::from_consume(
                                    parameter.is_consume,
                                ),
                            })
                            .collect(),
                        return_ty: function
                            .return_type
                            .as_ref()
                            .map_or(ResolvedTy::Unit, |ty| ctx.lower_type(ty)),
                        runtime_capability: extern_runtime_capability(
                            &provenance,
                            function.name.name.as_str(),
                        ),
                        provenance,
                        offload: false,
                        span: function.span.clone(),
                    }));
                }
            }
            if let Item::Impl(impl_decl) = item {
                if is_builtin_callable_impl(item) {
                    let TypeExpr::Named {
                        path: named_path, ..
                    } = &impl_decl.target_type.0
                    else {
                        continue;
                    };
                    let name = &named_path.to_string(); // TRANSITION(P1): deleted by A1 commit 2
                    let receiver_specific = is_builtin_receiver_impl(item);
                    let symbol_owner = if receiver_specific {
                        injected_builtin_impl_symbol_owner(name).to_string()
                    } else {
                        imported_impl_symbol_self_name("std.builtins", name)
                    };
                    let skipped_methods: HashSet<hew_types::DefId> = impl_decl
                        .methods
                        .iter()
                        .filter_map(|method| {
                            let declaration = ctx.source_declaration(
                                &method.fn_span,
                                hew_types::DeclarationKind::ImplMethod,
                                0,
                            )?;
                            let symbol = ctx.emitted_impl_method_symbol(
                                declaration,
                                &symbol_owner,
                                method.name.name.as_str(),
                            );
                            (!builtin_callable_impl_method_symbols.contains(&symbol))
                                .then_some(declaration)
                        })
                        .collect();
                    if skipped_methods.len() == impl_decl.methods.len() {
                        continue;
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
        ctx.current_module_idx = saved_module_idx;
        ctx.current_item_ordinal = saved_ordinal;
        ctx.current_module_name = saved_module;
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
    for ((declaration, _), (id, signature)) in std::mem::take(&mut ctx.extern_method_signatures) {
        let (Ok(params), Ok(return_ty)) = (
            signature
                .params
                .iter()
                .map(|parameter| {
                    ResolvedTy::from_ty(&parameter.ty).map(|ty| hew_types::CallableParameter {
                        ty,
                        ownership: parameter.ownership,
                    })
                })
                .collect::<Result<Vec<_>, _>>(),
            ResolvedTy::from_ty(&signature.result),
        ) else {
            continue;
        };
        let provenance = extern_provenance(signature.declaring_module.as_deref());
        let runtime_capability = extern_runtime_capability(&provenance, &signature.endpoint);
        items.push(HirItem::ExternFn(crate::node::HirExternFn {
            failure_domain: hew_types::CallableFailureDomain::Abort,
            id,
            node: ctx.ids.node(),
            declaration,
            name: signature.endpoint,
            abi: "C".to_string(),
            params,
            return_ty,
            provenance,
            runtime_capability,
            offload: false,
            span: hew_parser::ast::Span::default(),
        }));
    }
    items.extend(prelude_declarations.into_iter().map(HirItem::TypeDecl));
    items.extend(actor_intercept_types.into_iter().map(HirItem::TypeDecl));

    let call_site_type_args = ctx.call_site_type_args;
    let supervisor_child_slots = ctx.supervisor_child_slots;
    let pool_accessor_sites = ctx.pool_accessor_sites;

    // Extern-backed record surface: the `#[extern_symbol]` method names were
    // collected at impl lowering (the attribute does not survive onto the
    // lowered `HirFn`); `extern` block fns are folded in here from their
    // lowered signatures, qualified by their captured defining-module
    // provenance so the spellings match the `record_registry` keys.
    let mut extern_backed_records = std::mem::take(&mut ctx.extern_backed_record_names);
    for item in &items {
        if let HirItem::ExternFn(extern_fn) = item {
            for ty in extern_fn
                .params
                .iter()
                .map(|parameter| &parameter.ty)
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
        &extern_backed_records,
        &mut ctx.type_classes,
        &ctx.type_declarations,
    );

    check_call_shape_gates(&items, &mut ctx.diagnostics);

    admit_opaque_resource_lifecycles(
        &ctx.defs,
        &items,
        &ctx.opaque_resource_candidates,
        &mut ctx.type_classes,
        &mut ctx.diagnostics,
    );
    admit_declared_opaque_resource_lifecycles(
        &items,
        &ctx.opaque_resource_candidates,
        &ctx.defs,
        &mut ctx.type_classes,
        &mut ctx.diagnostics,
    );
    admit_resource_record_lifecycles(
        &items,
        &lowered_type_declarations,
        ctx.display_facts.context(),
        &ctx.defs,
        &ctx.resource_close_discipline_failures,
        &mut ctx.type_classes,
        &mut ctx.diagnostics,
    );

    let module = HirModule {
        session_entry: None,
        source_origins: type_check_output
            .normalized_program
            .as_ref()
            .map_or_else(Arc::default, |normalized| {
                Arc::clone(&normalized.source_origins)
            }),
        checked_sites: ctx.checked_sites,
        indexed_place_operations: ctx.indexed_place_operations,
        items,
        diagnostic_source_modules,
        root_item_ids: ctx.root_item_ids,
        entry_exit_plan,
        test_entry_plans: type_check_output.test_entry_plans.clone(),
        trait_object_layouts: Arc::clone(&ctx.trait_object_layouts),
        structural_witnesses: type_check_output.structural_witnesses.clone(),
        type_classes: ctx.type_classes,
        call_site_type_args,
        supervisor_child_slots,
        pool_accessor_sites,
        regex_literals: ctx.regex_literals,
        defs: Arc::clone(&ctx.defs),
    };

    LowerOutput {
        module,
        diagnostics: ctx.diagnostics,
    }
}

pub(super) fn finalize_user_record_value_classes(
    record_registry: &HashMap<String, RecordEntry>,
    extern_backed_records: &HashSet<String>,
    type_classes: &mut crate::value_class::TypeClassTable,
    declarations: &std::collections::BTreeMap<
        hew_types::NominalId,
        hew_types::value_class::DeclaredType,
    >,
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
}

/// Map the HIR lowering context's current-module authority to a total
/// [`ExternProvenance`] for an `extern` fn declared under it.
///
/// `None` is the genuine root compilation unit (module index 0), which lowers
/// to [`ExternProvenance::Root`]; `Some(dotted)` is a named module (std or
/// user/package) lowered to [`ExternProvenance::Module`]. This is the ONLY
/// mapping either construction site uses, so a std and a user extern are
/// classified from the same authority regardless of which lowering pass emitted
/// them.
fn extern_provenance(current_module_name: Option<&str>) -> ExternProvenance {
    match current_module_name {
        None => ExternProvenance::Root,
        Some(name) => ExternProvenance::Module(name.to_string()),
    }
}

fn extern_runtime_capability(
    provenance: &ExternProvenance,
    declaration: &str,
) -> Option<hew_types::ExternRuntimeCapability> {
    let ExternProvenance::Module(module_name) = provenance else {
        return None;
    };
    hew_types::stdlib_authority().extern_runtime_capability(module_name, declaration)
}

fn record_source_modules_for_items(
    items: &[HirItem],
    source_module: &str,
    diagnostic_source_modules: &mut HashMap<ItemId, String>,
) {
    for item in items {
        let id = match item {
            HirItem::Function(item) => item.id,
            HirItem::TypeDecl(item) => item.id,
            HirItem::Record(item) => item.id,
            HirItem::Actor(item) => item.id,
            HirItem::Supervisor(item) => item.id,
            HirItem::Impl(item) => item.id,
            HirItem::ExternFn(item) => item.id,
            HirItem::Const(item) => item.id,
        };
        diagnostic_source_modules.insert(id, source_module.to_string());
    }
}
