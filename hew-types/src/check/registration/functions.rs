//! Checker methods grouped by responsibility: functions.
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
use hew_parser::ast::WireMetadata;

impl Checker {
    /// Populate `declared_type_param_names` with every type-parameter name
    /// declared anywhere in the program and its modules — on type / record /
    /// trait / impl / machine / actor declarations and on every generic method
    /// (impl method, trait method, actor receive-fn) or free function — and
    /// `declared_nominal_type_names` with every declared NOMINAL type name
    /// (type / type-alias / record / trait / actor / supervisor / machine, plus
    /// the synthesised `<Machine>Event` companion).
    ///
    /// The undefined-named-type guard consults both sets. A name declared as a
    /// type parameter somewhere is intentionally left opaque (`Ty::named`) by
    /// the resolver and re-resolved at several secondary sites (signature
    /// rebuilds, receiver probes, trait-conformance checks) WITHOUT its scope
    /// re-pushed, so it must never be reported as undefined. A nominal type
    /// declared in an imported `module_graph` module is likewise resolvable even
    /// while that module's signatures are registered in a pass where the global
    /// `trait_defs` / `known_types` still hold only the root module's
    /// declarations. A genuinely undefined type (`Bogus`) is in neither set, so
    /// it is still caught.
    pub(in crate::check) fn collect_declared_type_param_names(&mut self, program: &Program) {
        for (item, _) in &program.items {
            self.collect_item_type_param_names(item);
            self.collect_item_nominal_type_name(item);
        }
        if let Some(mg) = &program.module_graph {
            for module in mg.modules.values() {
                for (item, _) in &module.items {
                    self.collect_item_type_param_names(item);
                    self.collect_item_nominal_type_name(item);
                }
            }
        }
        // Harvest trait-level type parameters from every registered trait def.
        // Built-in and stdlib traits (e.g. `Index<Idx>` from std/builtins.hew)
        // are registered into `trait_defs` by `register_builtins` rather than
        // appearing in the walked program AST; their parameter names surface in
        // user code when a `dyn Trait<...>` annotation pulls the trait's method
        // signatures through resolution without the trait scope re-pushed.
        let trait_param_names: Vec<String> = self
            .trait_defs
            .values()
            .flat_map(|trait_def| trait_def.type_params.iter().cloned())
            .collect();
        self.declared_type_param_names.extend(trait_param_names);
    }

    pub(super) fn collect_item_type_param_names(&mut self, item: &Item) {
        match item {
            Item::Supervisor(sd) => self.insert_type_param_names(&sd.type_params),
            Item::Function(fd) => self.insert_opt_type_param_names(fd.type_params.as_ref()),
            Item::TypeDecl(td) => self.insert_opt_type_param_names(td.type_params.as_ref()),
            Item::Record(rd) => self.insert_opt_type_param_names(rd.type_params.as_ref()),
            Item::Trait(tr) => {
                self.insert_opt_type_param_names(tr.type_params.as_ref());
                for trait_item in &tr.items {
                    if let TraitItem::Method(method) = trait_item {
                        self.insert_opt_type_param_names(method.type_params.as_ref());
                    }
                }
            }
            Item::Impl(id) => {
                self.insert_opt_type_param_names(id.type_params.as_ref());
                for method in &id.methods {
                    self.insert_opt_type_param_names(method.type_params.as_ref());
                }
            }
            Item::Actor(ad) => {
                self.insert_type_param_names(&ad.type_params);
                for receive_fn in &ad.receive_fns {
                    self.insert_opt_type_param_names(receive_fn.type_params.as_ref());
                }
                for method in &ad.methods {
                    self.insert_opt_type_param_names(method.type_params.as_ref());
                }
            }
            _ => {}
        }
    }

    pub(super) fn insert_opt_type_param_names(&mut self, tps: Option<&Vec<TypeParam>>) {
        if let Some(tps) = tps {
            self.insert_type_param_names(tps);
        }
    }

    pub(super) fn insert_type_param_names(&mut self, tps: &[TypeParam]) {
        for tp in tps {
            self.declared_type_param_names.insert(tp.name.clone());
        }
    }

    /// Validate that no trait bound in the given type parameters or
    /// where-clause carries positional type arguments (e.g. `T: Eq<U>`).
    /// Such forms are not valid in Hew — the checker cannot enforce
    /// phantom-parameterised marker bounds, and admitting them would silently
    /// erase the type arguments in `collect_type_param_bounds`, reducing
    /// `Eq<U>` to bare `Eq` without any diagnostic.
    ///
    /// Emits `UnknownTraitBoundShape` at `span` for every offending bound.
    /// Must be called before `collect_type_param_bounds` erases `type_args`.
    /// Covers fn/impl/impl-method/machine declaration positions.
    pub(in crate::check) fn validate_type_param_bound_shapes(
        &mut self,
        type_params: Option<&Vec<TypeParam>>,
        where_clause: Option<&WhereClause>,
        span: &Span,
    ) {
        // Check inline type-param bounds: e.g. `<T: Eq<U>>`.
        if let Some(params) = type_params {
            for param in params {
                for bound in &param.bounds {
                    if bound.type_args.as_ref().is_some_and(|a| !a.is_empty()) {
                        self.report_error(
                            TypeErrorKind::UnknownTraitBoundShape {
                                trait_name: bound.name.clone(),
                            },
                            span,
                            format!(
                                "trait bound `{}` on type parameter `{}` carries positional \
                                 type arguments, which are not supported; use associated-type \
                                 bindings (`Trait<Assoc = Ty>`) instead",
                                bound.name, param.name,
                            ),
                        );
                    }
                }
            }
        }
        // Check where-clause bounds: `where T: Eq<U>`.
        if let Some(wc) = where_clause {
            for predicate in &wc.predicates {
                for bound in &predicate.bounds {
                    if bound.type_args.as_ref().is_some_and(|a| !a.is_empty()) {
                        self.report_error(
                            TypeErrorKind::UnknownTraitBoundShape {
                                trait_name: bound.name.clone(),
                            },
                            span,
                            format!(
                                "trait bound `{}` in where-clause carries positional \
                                 type arguments, which are not supported; use associated-type \
                                 bindings (`Trait<Assoc = Ty>`) instead",
                                bound.name,
                            ),
                        );
                    }
                }
            }
        }
    }

    /// Thin wrapper for the fn-decl path; delegates to
    /// `validate_type_param_bound_shapes` using the function's own
    /// type-param list, where-clause, and declaration span.
    pub(in crate::check) fn validate_fn_type_param_bound_shapes(&mut self, fd: &FnDecl) {
        self.validate_type_param_bound_shapes(
            fd.type_params.as_ref(),
            fd.where_clause.as_ref(),
            &fd.decl_span,
        );
    }

    /// Pass 2: Collect function signatures
    #[expect(
        clippy::too_many_lines,
        reason = "signature collection maintains one ordered registration walk"
    )]
    pub(in crate::check) fn collect_functions(&mut self, program: &Program) {
        let flat_file_import_modules = flat_file_import_module_ids(program);
        self.flat_file_import_module_names = flat_file_import_modules
            .iter()
            .map(|module_id| module_id.path.join("."))
            .collect();
        // Process module graph items first (if multi-module).
        // Skip the root module — its items are already in program.items and
        // will be processed below with current_module = None (bare names).
        if let Some(ref mg) = program.module_graph {
            let span_indices = mg.file_span_indices();
            for mod_id in &mg.topo_order {
                if *mod_id == mg.root {
                    continue;
                }
                if let Some(module) = mg.modules.get(mod_id) {
                    let module_name = mod_id.path.join(".");
                    self.record_canonical_std_module_source(&module_name, &module.source_paths);
                    self.current_module = Some(module_name.clone());
                    self.registration_is_flat_file_import =
                        flat_file_import_modules.contains(mod_id);
                    self.current_module_direct_imports = module
                        .imports
                        .iter()
                        .map(|import| import.target.path.join("."))
                        .collect();
                    self.current_module_direct_import_bindings = module
                        .imports
                        .iter()
                        .map(|import| (import.target.path.join("."), import.spec.clone()))
                        .collect();
                    // Scope local declarations to the module being registered.
                    let saved_local_type_defs = self.local_type_defs.clone();
                    let saved_source_type_defs = self.source_type_defs.clone();
                    for (item, _) in &module.items {
                        match item {
                            Item::TypeDecl(td) => {
                                self.local_type_defs.insert(td.name.clone());
                                self.source_type_defs.insert(td.name.clone());
                            }
                            Item::Machine(md) => {
                                self.local_type_defs.insert(md.name.clone());
                                self.source_type_defs.insert(md.name.clone());
                                let event_type_name = format!("{}Event", md.name);
                                self.local_type_defs.insert(event_type_name.clone());
                                self.source_type_defs.insert(event_type_name);
                            }
                            _ => {}
                        }
                    }

                    // Snapshot error/warning counts before signature registration
                    // for this module.  Diagnostics emitted during collect_function_item
                    // (e.g. duplicate-definition errors, import errors) are tagged with
                    // the module name below so the CLI renders them against the correct
                    // source file rather than the root compilation unit.
                    let err_before = self.errors.len();
                    let warn_before = self.warnings.len();

                    let item_sources = self.module_item_sources.get(&module_name).cloned();
                    for (item_idx, (item, span)) in module.items.iter().enumerate() {
                        // Per-item defining-file identity (rc1-F1 stage C):
                        // registration-time facts (extern contracts, their
                        // conflict diagnostics) attribute to the item's own
                        // source file, not the assembled module's primary.
                        self.current_item_source = item_sources
                            .as_ref()
                            .and_then(|sources| sources.get(item_idx))
                            .cloned();
                        self.current_item_ordinal = item_idx;
                        self.current_module_idx = span_indices
                            .item_index(mod_id, item_idx)
                            .unwrap_or_default();
                        self.collect_function_item(item, span);
                    }
                    self.current_item_source = None;
                    self.current_item_ordinal = 0;

                    for e in &mut self.errors[err_before..] {
                        if e.source_module.is_none() {
                            e.source_module = Some(module_name.clone());
                        }
                    }
                    for w in &mut self.warnings[warn_before..] {
                        if w.source_module.is_none() {
                            w.source_module = Some(module_name.clone());
                        }
                    }

                    self.local_type_defs = saved_local_type_defs;
                    self.source_type_defs = saved_source_type_defs;
                }
            }
        }

        // Process main module items.
        self.current_module = None;
        self.current_module_idx = 0;
        self.registration_is_flat_file_import = false;
        self.current_module_direct_imports = program
            .module_graph
            .as_ref()
            .and_then(|graph| graph.modules.get(&graph.root))
            .map(|root| {
                root.imports
                    .iter()
                    .map(|import| import.target.path.join("."))
                    .collect()
            })
            .unwrap_or_default();
        self.current_module_direct_import_bindings = program
            .module_graph
            .as_ref()
            .and_then(|graph| graph.modules.get(&graph.root))
            .map(|root| {
                root.imports
                    .iter()
                    .map(|import| (import.target.path.join("."), import.spec.clone()))
                    .collect()
            })
            .unwrap_or_default();
        for (item_ordinal, (item, span)) in program.items.iter().enumerate() {
            self.current_item_ordinal = item_ordinal;
            self.collect_function_item(item, span);
        }
        self.canonicalize_root_super_trait_edges(program);
        self.current_module_direct_imports.clear();
        self.current_module_direct_import_bindings.clear();
    }

    #[expect(
        clippy::too_many_lines,
        reason = "expression type checking requires many cases"
    )]
    pub(in crate::check) fn collect_function_item(&mut self, item: &Item, span: &Span) {
        match item {
            Item::Function(fd) => {
                if self.reject_protected_prelude_declaration(&fd.name, span) {
                    return;
                }
                // rc1-F1 stage A: `fn_def_spans`/`fn_visibility` are
                // CANONICALIZED with `fn_sigs` — one key shape per
                // declaration, so declaration-authority probes never miss on
                // key misalignment. The stored declaring module stays
                // `current_module` (None = root): it is the display/provenance
                // axis, and the legacy root render at publication boundaries
                // derives from it.
                let scoped_name = Self::declared_fn_identity(self.canonical_fn_owner(), &fd.name);
                if let Some((prev_span, _)) = self.fn_def_spans.get(&scoped_name) {
                    // Root diagnostics render the bare leaf, exactly as the
                    // declaration is spelled in source.
                    let display_name = self
                        .root_owned_fn_leaf(&scoped_name)
                        .unwrap_or(&scoped_name)
                        .to_string();
                    self.errors.push(TypeError::duplicate_definition(
                        span.clone(),
                        &display_name,
                        prev_span.clone(),
                    ));
                } else {
                    self.fn_def_spans.insert(
                        scoped_name.clone(),
                        (span.clone(), self.current_module.clone()),
                    );
                    if fd.attributes.iter().any(|a| a.name.as_str() == "test") {
                        self.test_fn_names.insert(scoped_name.clone());
                    }
                    self.fn_visibility.insert(scoped_name, fd.visibility);
                }
                self.register_fn_sig(fd);
                self.record_root_value_binding(&fd.name);
            }
            Item::Actor(ad) => {
                // Module actors are identified by their full dotted source
                // owner (`{module_path}.{name}`); root actors stay bare.
                // Registering the full declaration here (not only the
                // signatures) covers PRIVATE module actors, which never pass
                // through the pub-only import paths but still need a type def
                // for in-module spawn checking. A root actor is normally
                // already registered; restore it only if import registration
                // replaced its bare compatibility slot with a non-actor type.
                let module_identity = self.current_module.clone();
                let identity = Self::actor_identity(module_identity.as_deref(), &ad.name);
                let local_actor_needs_restore = self
                    .type_defs
                    .get(&identity)
                    .is_none_or(|definition| definition.kind != TypeDefKind::Actor);
                if module_identity.is_some() || local_actor_needs_restore {
                    self.register_actor_decl_as(ad, &identity);
                }
                // The actor's own generics (`actor Worker<T>`) are in scope for
                // every receive fn and method signature; push them and resolve
                // those signatures scope-locally so an out-of-scope generic name
                // is rejected at the annotation while the actor's legitimate
                // `<T>` still resolves.
                let actor_sig_scope =
                    self.enter_primary_sig_scope(&[(Some(&ad.type_params), None)]);
                for rf in &ad.receive_fns {
                    self.register_receive_fn(&identity, rf);
                }
                for method in &ad.methods {
                    let method_name = format!("{identity}::{}", method.name);
                    self.register_fn_sig_with_name(&method_name, method);
                    // An actor-body plain `fn` is a callable declaration, so it
                    // needs the same declaration row every other source-declared
                    // callable has: `call_target_for_signature` reads
                    // `fn_def_spans` to publish `CallTarget::User`, and without a
                    // row the checker admits `Counter.helper()` and then hands
                    // HIR an `Unsupported` target (#3285). The key is the same
                    // `{owner}::{name}` string the identity table mints
                    // (`Kind::ActorMethod`), so `user_call_target_for_declared_fn`
                    // resolves it by path under both the bare and the module
                    // spelling. Widening this table is side-effect free for the
                    // visibility rung, which only fires for keys containing `.`
                    // and no `::`.
                    //
                    // A `#[on(...)]` lifecycle hook shares the `methods` list
                    // and the `ActorMethod` identity kind but is not a
                    // callable: the runtime enters it through its own
                    // trampoline and MIR emits it under a hook symbol. It gets
                    // no declaration row, so a call naming one still fails the
                    // way it does today.
                    if !method.attributes.iter().any(|a| a.name.as_str() == "on") {
                        self.fn_def_spans.insert(
                            method_name,
                            (method.fn_span.clone(), self.current_module.clone()),
                        );
                    }
                }
                self.exit_primary_sig_scope(actor_sig_scope);
            }
            Item::Impl(id) => {
                if Self::impl_decl_is_drop_impl(id) {
                    self.report_unsupported_impl_drop(span);
                    return;
                }
                // Register impl methods with Type::method naming
                if let TypeExpr::Named {
                    name: target_name,
                    type_args,
                } = &id.target_type.0
                {
                    // An impl target written through a module binding
                    // (`impl Tagged for json.Value`) names a declaration whose
                    // identity is `std.encoding.json.Value`. Resolve that
                    // surface spelling ONCE, here, so every method table this
                    // arm writes is keyed by the same identity method
                    // resolution looks the receiver up under. Leaving it as
                    // written registers the methods where nothing can find
                    // them, and the impl is silently ignored.
                    let canonical_target = self.canonical_impl_target_identity(target_name);
                    let type_name = canonical_target.as_ref().unwrap_or(target_name);
                    let prev_impl_surface_target = self
                        .current_impl_surface_target
                        .replace(target_name.clone());
                    // Do NOT push generic_ctx here — type params like T should remain
                    // as Ty::Named so that substitute_named_param can replace them
                    // at method call sites with concrete type arguments.

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

                    // Validate the impl provides EXACTLY the trait's method set
                    // (every required method present, no extraneous method),
                    // resolved through the trait's owner-qualified identity. Runs
                    // ONCE per impl, before the per-method loop, so an impl with
                    // ZERO methods (an empty `impl A for W { }` missing every
                    // required method) is still validated. Per-method signature
                    // equivalence is checked separately inside the loop.
                    if let Some(tb) = id.trait_bound.as_ref() {
                        self.check_impl_method_set_against_trait(type_name, tb, &id.methods, span);
                    }

                    for method in &id.methods {
                        let sig = self.register_impl_method(
                            type_name,
                            method,
                            id.type_params.as_ref(),
                            id.where_clause.as_ref(),
                            id.trait_bound.as_ref(),
                        );
                        // Q004: enforce impl-vs-trait signature equivalence at
                        // the impl site so mismatches surface where the user
                        // wrote them, not as a confusing "type does not satisfy
                        // trait" downstream. See LESSONS row `diagnostic-trust`.
                        if let Some(tb) = id.trait_bound.as_ref() {
                            self.check_impl_method_against_trait(
                                type_name,
                                &self_type_args,
                                tb,
                                method,
                                &sig,
                            );
                        }
                        // When the receiver is a primitive or compiler-builtin
                        // generic, `lookup_type_def_mut(type_name)` returns `None` so the
                        // sig has nowhere to live for later dispatch.  Mirror it onto the
                        // side table keyed by the canonical receiver kind + trait name so
                        // method-resolution can find it.
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

                    // Register default trait methods not overridden in this impl
                    if let Some(tb) = &id.trait_bound {
                        self.record_trait_impl_methods(
                            type_name,
                            &tb.name,
                            id.methods.iter().map(|method| method.name.clone()),
                        );
                        self.record_trait_impl(type_name, &tb.name);

                        let overridden: HashSet<&str> =
                            id.methods.iter().map(|m| m.name.as_str()).collect();
                        // Owner-qualified key so a same-name trait in another
                        // module cannot inject the wrong default-method bodies.
                        let trait_key = self.trait_defs_key_for_bound(&tb.name);
                        if let Some(trait_methods) = self.trait_defs.get(&trait_key) {
                            let defaults: Vec<_> = trait_methods
                                .methods
                                .iter()
                                .filter(|m| {
                                    m.body.is_some() && !overridden.contains(m.name.as_str())
                                })
                                .cloned()
                                .collect();
                            for m in defaults {
                                let method_key = format!("{type_name}::{}", m.name);
                                let skip = usize::from(
                                    m.params.first().is_some_and(|p| self.is_receiver_param(p)),
                                );
                                let param_names: Vec<String> =
                                    m.params.iter().skip(skip).map(|p| p.name.clone()).collect();
                                self.register_trait_method_sig(&tb.name, &m, span);
                                let trait_method_key = format!("{}::{}", tb.name, m.name);
                                let consumes_receiver = m.consumes_self;
                                let returns_receiver_identity =
                                    Self::trait_receiver_identity_is_structurally_valid(&m);
                                let concrete_self = Ty::Named {
                                    builtin: None,
                                    name: type_name.clone(),
                                    args: self_type_args.clone(),
                                };
                                let (params, return_type) = if let Some(sig) =
                                    self.fn_sigs.get(&trait_method_key).cloned()
                                {
                                    // Qualified trait signatures registered outside an impl
                                    // scope can still include a concrete receiver
                                    // (`fn bump(box: CounterBox)`) because receiver
                                    // detection there only knows about `Self`.
                                    // When copying defaults onto a concrete impl,
                                    // drop that leading receiver iff the trait sig
                                    // still has it.
                                    let sig_skip = usize::from(
                                        skip == 1 && sig.params.len() == m.params.len(),
                                    );
                                    (
                                        sig.params
                                            .iter()
                                            .skip(sig_skip)
                                            .map(|ty| {
                                                ty.substitute_named_param("Self", &concrete_self)
                                            })
                                            .collect::<Vec<_>>(),
                                        sig.return_type
                                            .substitute_named_param("Self", &concrete_self),
                                    )
                                } else {
                                    (
                                        m.params
                                            .iter()
                                            .skip(skip)
                                            .map(|p| {
                                                self.resolve_registered_annotation_ty_no_holes(
                                                    &p.ty,
                                                )
                                            })
                                            .collect(),
                                        m.return_type.as_ref().map_or(Ty::Unit, |ret| {
                                            self.resolve_registered_annotation_ty_no_holes(ret)
                                        }),
                                    )
                                };
                                let sig = FnSig {
                                    param_names: param_names.clone(),
                                    params: params.clone(),
                                    return_type: return_type.clone(),
                                    consumes_receiver,
                                    returns_receiver_identity,
                                    ..FnSig::default()
                                };
                                self.fn_sigs.insert(method_key, sig);
                                let receiver_name = if self.registration_is_flat_file_import
                                    || type_name.contains('.')
                                {
                                    type_name.clone()
                                } else {
                                    self.canonical_nominal_name(type_name).unwrap_or_else(|| {
                                        self.current_module.as_ref().map_or_else(
                                            || type_name.clone(),
                                            |module| format!("{module}.{type_name}"),
                                        )
                                    })
                                };
                                let receiver_nominal = self
                                    .require_declaration_path(&receiver_name, &m.span)
                                    .map(crate::NominalId::from_minted_declaration);
                                if let (
                                    Ok(receiver_args),
                                    Some((declaring_trait, _)),
                                    Some(receiver_nominal),
                                ) = (
                                    self_type_args
                                        .iter()
                                        .map(ResolvedTy::from_ty)
                                        .collect::<Result<Vec<_>, _>>(),
                                    self.trait_method_call_target_ids(&tb.name, &m.name),
                                    receiver_nominal,
                                ) {
                                    let declaration = crate::default_impl_method_declaration(
                                        &declaring_trait,
                                        &crate::NominalInstance {
                                            nominal: receiver_nominal,
                                            args: receiver_args,
                                        },
                                        &m.name,
                                    );
                                    // A materialized default is keyed exactly
                                    // like an explicit impl method: the
                                    // `Box<i64>` specialisation takes its own
                                    // mangled keys and leaves the generic
                                    // `impl<T> … Box<T>` default owning the
                                    // shared dispatch key.
                                    let keys = self.impl_method_declaration_keys(
                                        type_name,
                                        &m.name,
                                        id.type_params.as_deref(),
                                    );
                                    self.publish_impl_method_declaration_id(&keys, &declaration);
                                }
                                self.publish_impl_method_sig(
                                    type_name,
                                    &m.name,
                                    &FnSig {
                                        param_names,
                                        params,
                                        return_type,
                                        consumes_receiver,
                                        returns_receiver_identity,
                                        ..FnSig::default()
                                    },
                                );
                            }
                        }
                    }

                    // Restore previous self type
                    self.current_self_type = prev_self_type;
                    self.current_impl_surface_target = prev_impl_surface_target;
                    if scope_pushed {
                        self.exit_impl_scope();
                    }
                }
            }
            Item::TypeDecl(td) => {
                // Register methods defined inside struct/enum bodies
                for item in &td.body {
                    if let TypeBodyItem::Method(method) = item {
                        // An inline type-body method's signature can name the
                        // enclosing type's generics (`type Holder<T> { fn f(...) -> T }`)
                        // and its own (`fn idm<T>(...)`). Push BOTH so the whole
                        // block — `register_fn_sig_with_name` AND the secondary
                        // resolution below that rebuilds the `FnSig` for
                        // `type_def.methods` (which runs outside the method's own
                        // frame) — resolves them scope-locally: an out-of-scope
                        // name is rejected at the annotation, while these
                        // legitimate generics still resolve.
                        let method_sig_scope = self.enter_primary_sig_scope(&[
                            (td.type_params.as_ref(), td.where_clause.as_ref()),
                            (method.type_params.as_ref(), method.where_clause.as_ref()),
                        ]);
                        let method_key = format!("{}::{}", td.name, method.name);
                        // Inline type-body methods can declare their own
                        // generics alongside the type's; the same shadow
                        // refusal applies here as on `impl` blocks.
                        if let Some(type_tps) = td.type_params.as_ref() {
                            let enclosing: Vec<String> =
                                type_tps.iter().map(|tp| tp.name.clone()).collect();
                            if !enclosing.is_empty() {
                                let owner = Self::method_declaration_key(
                                    &self.declaration_owner_key(&td.name),
                                    &method.name,
                                );
                                self.reject_shadowing_method_type_params(
                                    method.type_params.as_ref(),
                                    &[(enclosing, format!("the type `{}`", td.name))],
                                    &owner,
                                    &method.decl_span,
                                );
                            }
                        }
                        self.register_fn_sig_with_name(&method_key, method);
                        let skip = usize::from(
                            method
                                .params
                                .first()
                                .is_some_and(|p| self.is_receiver_param(p)),
                        );
                        let param_names: Vec<String> = method
                            .params
                            .iter()
                            .skip(skip)
                            .map(|p| p.name.clone())
                            .collect();
                        let params: Vec<Ty> = method
                            .params
                            .iter()
                            .skip(skip)
                            .map(|p| self.resolve_registered_annotation_ty_no_holes(&p.ty))
                            .collect();
                        let return_type = method.return_type.as_ref().map_or(Ty::Unit, |ret| {
                            self.resolve_registered_annotation_ty_no_holes(ret)
                        });
                        self.exit_primary_sig_scope(method_sig_scope);
                        let method_name = method.name.clone();
                        let type_name = td.name.clone();
                        if let Some(type_def) = self.lookup_type_def_mut(&type_name) {
                            type_def.methods.insert(
                                method_name,
                                FnSig {
                                    param_names,
                                    params,
                                    return_type,
                                    ..FnSig::default()
                                },
                            );
                        }
                    }
                }
            }
            Item::Trait(td) => {
                if let Some(supers) = &td.super_traits {
                    let owner = self.current_module.as_deref();
                    for super_trait in supers {
                        self.mark_imported_trait_used(owner, &super_trait.name);
                    }
                }
                // A generic trait's own params (`trait Foo<T>`) are in scope for
                // every method signature; push them and resolve those signatures
                // scope-locally so an out-of-scope generic name is rejected at
                // the annotation while the trait's legitimate `<T>` resolves.
                // (`register_trait_method_sig` pushes each method's own params.)
                let trait_sig_scope =
                    self.enter_primary_sig_scope(&[(td.type_params.as_ref(), None)]);
                for trait_item in &td.items {
                    if let TraitItem::Method(method) = trait_item {
                        self.register_trait_method_sig(&td.name, method, span);
                    }
                }
                self.exit_primary_sig_scope(trait_sig_scope);
            }
            Item::ExternBlock(eb) => {
                self.register_extern_block(eb, span);
            }
            Item::Import(id) => {
                // Always track the import span. For non-root modules the span is a byte
                // offset into the sub-module's own source file; the stored `source_module`
                // in `import_spans` tells the diagnostic renderer which file owns the span.
                self.register_import(id, Some(span));
            }
            Item::Const(_)
            | Item::TypeAlias(_)
            | Item::Supervisor(_)
            | Item::Machine(_)
            | Item::Record(_) => {
                // Records have no method body items in v0.5; method registration
                // is a no-op here.  TODO(A-4): if records gain methods, register
                // them via a `register_record_methods` pass here.
            }
        }
    }

    pub(in crate::check) fn impl_decl_is_drop_impl(id: &ImplDecl) -> bool {
        id.trait_bound
            .as_ref()
            .is_some_and(|trait_bound| trait_bound.name == "Drop")
    }

    pub(in crate::check) fn register_fn_sig(&mut self, fd: &FnDecl) {
        // A top-level free function's signature is the primary resolution of its
        // own annotations: its type-param bounds frame is pushed before the
        // params/return resolve (see `register_fn_sig_with_name`), so the
        // in-scope checks alone prove its legitimate type params resolvable. A
        // free function has no enclosing generic scope, so no extra frame is
        // pushed — the scope-local guard only suppresses the program-wide
        // type-param fallback so an out-of-scope generic name (one declared only
        // on a *different* item) is reported as unknown rather than silently
        // exempted.
        let guard = self.enter_primary_sig_scope(&[]);
        self.register_fn_sig_with_name(&fd.name, fd);
        self.exit_primary_sig_scope(guard);
    }

    /// Open a primary item-signature registration block: resolve the item's
    /// signature annotations in scope-LOCAL mode, where a type-parameter name is
    /// proven resolvable ONLY through the in-scope frames (the enclosing item's
    /// generics pushed here, plus the item's own generics pushed by
    /// `register_fn_sig_with_name` / `register_receive_fn` while it resolves),
    /// NOT the program-wide `declared_type_param_names` fallback.
    ///
    /// This is the totality boundary for type-param names: an out-of-scope
    /// generic (declared only on a *different* item) fails closed AT the
    /// annotation with `unknown type` instead of leaking an opaque `Ty::named`
    /// that only aborts downstream as `E_MIR: unknown type` at the MIR boundary.
    /// The program-wide fallback is too broad to prove a SOURCE annotation valid
    /// — it would exempt `fn bad(x: T)` merely because some unrelated `fn id<T>`
    /// declared `T` somewhere — so suppressing it here makes the in-scope frames
    /// authoritative for the names a signature is allowed to spell.
    ///
    /// `scopes` lists the enclosing type-param sets to push as ONE names-only
    /// bounds frame (e.g. an actor/trait/type's `<T>`, and for inline type-body
    /// methods the method's own `<T>` too, so the secondary resolution that
    /// rebuilds the method's `FnSig` sees it). Pass `&[]` for items with no
    /// enclosing generics (free functions) or where the enclosing frame is
    /// already on the stack (impl methods, whose impl params `register_impl_method`
    /// pushes itself) — then only the scope-local flag is armed. The frame is
    /// names-only (`collect_type_param_scope_with_bounds` is pure) so opening the
    /// scope has no resolution side effects. Pair every call with
    /// [`Self::exit_primary_sig_scope`].
    pub(in crate::check) fn enter_primary_sig_scope(
        &mut self,
        scopes: &[(Option<&Vec<TypeParam>>, Option<&WhereClause>)],
    ) -> PrimarySigScope {
        let mut bounds: HashMap<String, Vec<String>> = HashMap::new();
        for (type_params, where_clause) in scopes {
            for (name, param_bounds) in
                self.collect_type_param_scope_with_bounds(*type_params, *where_clause)
            {
                let entry = bounds.entry(name).or_default();
                for bound in param_bounds {
                    if !entry.iter().any(|existing| existing == &bound) {
                        entry.push(bound);
                    }
                }
            }
        }
        let pushed_frame = !bounds.is_empty();
        if pushed_frame {
            self.current_type_param_bounds
                .push(TypeParamScope::new(bounds, HashMap::new()));
        }
        let prev_scope_local = self.scope_local_type_params_only;
        self.scope_local_type_params_only = true;
        PrimarySigScope {
            prev_scope_local,
            pushed_frame,
        }
    }

    /// Close a block opened by [`Self::enter_primary_sig_scope`], restoring the
    /// previous scope-local flag and popping the enclosing-generics frame.
    #[allow(
        clippy::needless_pass_by_value,
        reason = "the by-value parameter is the contract: consuming this one-shot \
                  guard makes a double exit_primary_sig_scope a use-after-move \
                  compile error, which is what stops the enclosing-generics frame \
                  from being popped twice. Taking it by reference would defeat that."
    )]
    pub(in crate::check) fn exit_primary_sig_scope(&mut self, scope: PrimarySigScope) {
        self.scope_local_type_params_only = scope.prev_scope_local;
        if scope.pushed_frame {
            self.current_type_param_bounds.pop();
        }
    }

    /// Declaration identity for the shadow-report dedup key: a module-qualified
    /// owner plus the method name. Built in one place so the three registration
    /// paths cannot drift into three spellings of the same identity.
    pub(super) fn method_declaration_key(owner: &str, method_name: &str) -> String {
        format!("{owner}::{method_name}")
    }

    /// `["a"]` -> `a`; `["a", "b"]` -> `a and b`; `["a", "b", "c"]` -> `a, b and c`.
    pub(super) fn and_list(items: &[&str]) -> String {
        match items {
            [] => String::new(),
            [only] => (*only).to_string(),
            [head @ .., last] => format!("{} and {last}", head.join(", ")),
        }
    }

    pub(in crate::check) fn reject_shadowing_method_type_params(
        &mut self,
        method_type_params: Option<&Vec<TypeParam>>,
        enclosing: &[(Vec<String>, String)],
        declaration_owner: &str,
        decl_span: &Span,
    ) {
        let Some(method_tps) = method_type_params else {
            return;
        };
        let mut shadowed: Vec<&str> = method_tps.iter().map(|tp| tp.name.as_str()).collect();
        shadowed.sort_unstable();
        shadowed.dedup();
        for name in shadowed {
            // A method can shadow more than one enclosing owner at once — an
            // `impl<T> Trait<T> for X<T>` block and the trait both declare `T`.
            // That is ONE mistake at ONE span, so it is one diagnostic naming
            // every owner, not one diagnostic per owner.
            let owners: Vec<&str> = enclosing
                .iter()
                .filter(|(params, _)| params.iter().any(|outer| outer == name))
                .map(|(_, description)| description.as_str())
                .collect();
            if owners.is_empty() {
                continue;
            }
            // Registration visits a declaration more than once — trait method
            // signatures repeatedly, and an inherited default once per
            // implementing module. The key is the DECLARATION's identity
            // (module-qualified owner, method, span, parameter), never the
            // registering module's: a span alone collides across files that
            // happen to place a method at the same byte offset.
            if !self.shadowed_method_type_param_reports.insert((
                declaration_owner.to_string(),
                decl_span.start,
                decl_span.end,
                name.to_string(),
            )) {
                continue;
            }
            self.report_error_with_suggestions(
                TypeErrorKind::DuplicateDefinition,
                decl_span,
                format!(
                    "method type parameter `{name}` shadows the type parameter `{name}` \
                     declared by {}; the two cannot be told apart once the enclosing type \
                     arguments are substituted",
                    Self::and_list(&owners)
                ),
                vec![format!(
                    "rename the method's parameter so it is distinct from the enclosing `{name}`"
                )],
            );
        }
    }

    /// Collect the full resolver scope for declared type params: trait-bound
    /// names plus any associated-type bindings attached to those bounds.
    pub(in crate::check) fn collect_type_param_scope_with_assoc_bindings(
        &mut self,
        type_params: Option<&Vec<TypeParam>>,
        where_clause: Option<&WhereClause>,
        hole_vars: &mut Vec<TypeVar>,
    ) -> TypeParamScope {
        let bounds = self.collect_type_param_scope_with_bounds(type_params, where_clause);
        let pushed_bounds_for_assoc = !bounds.is_empty();
        if pushed_bounds_for_assoc {
            self.current_type_param_bounds
                .push(TypeParamScope::new(bounds.clone(), HashMap::new()));
        }
        let assoc_bindings =
            self.collect_type_param_assoc_bindings(type_params, where_clause, hole_vars);
        if pushed_bounds_for_assoc {
            self.current_type_param_bounds.pop();
        }
        TypeParamScope::new(bounds, assoc_bindings)
    }

    /// Like `collect_type_param_bounds` but always includes a key for every
    /// declared type param, with an empty `Vec` when no bounds are
    /// declared. Used by the resolver to distinguish "type param in scope
    /// with no bounds" (emit missing-bound diagnostic) from "name is not a
    /// type param at all" (fall through to other resolution paths).
    pub(in crate::check) fn collect_type_param_scope_with_bounds(
        &self,
        type_params: Option<&Vec<TypeParam>>,
        where_clause: Option<&WhereClause>,
    ) -> HashMap<String, Vec<String>> {
        let mut map: HashMap<String, Vec<String>> = HashMap::new();
        if let Some(params) = type_params {
            for param in params {
                map.entry(param.name.clone()).or_default();
            }
        }
        let with_bounds = self.collect_type_param_bounds(type_params, where_clause);
        for (k, v) in with_bounds {
            let entry = map.entry(k).or_default();
            for b in v {
                if !entry.iter().any(|existing| existing == &b) {
                    entry.push(b);
                }
            }
        }
        map
    }

    pub(in crate::check) fn collect_type_param_bounds(
        &self,
        type_params: Option<&Vec<TypeParam>>,
        where_clause: Option<&WhereClause>,
    ) -> HashMap<String, Vec<String>> {
        let mut bounds = HashMap::new();
        let mut declared = HashSet::new();
        if let Some(params) = type_params {
            for param in params {
                declared.insert(param.name.clone());
                if param.bounds.is_empty() {
                    continue;
                }
                let entry = bounds.entry(param.name.clone()).or_default();
                for bound in &param.bounds {
                    Self::push_unique_bound(entry, &self.trait_defs_key_for_bound(&bound.name));
                }
            }
        }
        if let Some(wc) = where_clause {
            for predicate in &wc.predicates {
                if let TypeExpr::Named { name, type_args } = &predicate.ty.0 {
                    if !declared.contains(name) {
                        continue;
                    }
                    if type_args.as_ref().is_some_and(|args| !args.is_empty()) {
                        continue;
                    }
                    let entry = bounds.entry(name.clone()).or_default();
                    for bound in &predicate.bounds {
                        Self::push_unique_bound(entry, &self.trait_defs_key_for_bound(&bound.name));
                    }
                }
            }
        }
        bounds
    }

    pub(in crate::check) fn collect_type_param_assoc_bindings(
        &mut self,
        type_params: Option<&Vec<TypeParam>>,
        where_clause: Option<&WhereClause>,
        hole_vars: &mut Vec<TypeVar>,
    ) -> HashMap<(String, String, String), Ty> {
        let mut bindings = HashMap::new();
        let mut declared = HashSet::new();
        if let Some(params) = type_params {
            for param in params {
                declared.insert(param.name.clone());
                for bound in &param.bounds {
                    self.collect_type_param_bound_assoc_bindings(
                        &param.name,
                        bound,
                        &mut bindings,
                        hole_vars,
                    );
                }
            }
        }
        if let Some(wc) = where_clause {
            for predicate in &wc.predicates {
                if let TypeExpr::Named { name, type_args } = &predicate.ty.0 {
                    if !declared.contains(name) {
                        continue;
                    }
                    if type_args.as_ref().is_some_and(|args| !args.is_empty()) {
                        continue;
                    }
                    for bound in &predicate.bounds {
                        self.collect_type_param_bound_assoc_bindings(
                            name,
                            bound,
                            &mut bindings,
                            hole_vars,
                        );
                    }
                }
            }
        }
        bindings
    }

    pub(super) fn collect_type_param_bound_assoc_bindings(
        &mut self,
        param_name: &str,
        bound: &TraitBound,
        bindings: &mut HashMap<(String, String, String), Ty>,
        hole_vars: &mut Vec<TypeVar>,
    ) {
        let trait_key = self.trait_defs_key_for_bound(&bound.name);
        for binding in &bound.assoc_type_bindings {
            let key = (
                param_name.to_string(),
                trait_key.clone(),
                binding.name.clone(),
            );
            bindings
                .entry(key)
                .or_insert_with(|| self.resolve_registered_annotation_ty(&binding.ty, hole_vars));
        }
    }

    /// Check whether a parameter is the receiver (i.e. the implicit first
    /// parameter of an impl/trait method).  A parameter is a receiver if its
    /// declared type matches `Self` or the current impl target type.
    /// Note: name-based matching (`p.name == "self"`) has been intentionally
    /// removed — receivers are identified by type, not by name.
    pub(in crate::check) fn is_receiver_param(&mut self, p: &Param) -> bool {
        match &p.ty.0 {
            TypeExpr::Named { name, type_args } => {
                if name == "Self" {
                    return true;
                }
                // Clone to avoid borrowing self while we resolve type args.
                let impl_target = self.current_self_type.clone();
                if let Some((self_name, self_type_args)) = impl_target {
                    if name != &self_name {
                        return false;
                    }
                    // Name matches — also verify generic arguments match the
                    // impl target so that e.g. `impl Box<int>` rejects a
                    // parameter typed `Box<string>`.
                    let param_args: Vec<Ty> = type_args
                        .as_ref()
                        .map(|args| {
                            args.iter()
                                .map(|type_arg| self.resolve_type_expr(type_arg))
                                .collect()
                        })
                        .unwrap_or_default();
                    param_args == self_type_args
                } else {
                    false
                }
            }
            _ => false,
        }
    }

    pub(in crate::check) fn register_fn_sig_with_name(&mut self, name: &str, fd: &FnDecl) {
        // Only filter out the receiver for methods (Type::method), not free
        // functions that happen to have a parameter named `self`.
        let is_method = name.contains("::");
        let skip = if is_method {
            usize::from(fd.params.first().is_some_and(|p| self.is_receiver_param(p)))
        } else {
            0
        };
        // Validate that no bound carries unsupported positional type arguments
        // (e.g. `T: Eq<U>`) before `collect_type_param_bounds` erases them.
        self.validate_fn_type_param_bound_shapes(fd);
        // Push the type-param bounds map BEFORE resolving the signature so
        // the resolver can validate `T::Bar` projections that appear in
        // param/return types. Includes type params with no bounds so the
        // resolver can distinguish "in scope with no bounds" (emit
        // missing-bound diagnostic) from "not in scope" (fall through).
        let mut hole_vars = Vec::new();
        let fn_scope = self.collect_type_param_scope_with_assoc_bindings(
            fd.type_params.as_ref(),
            fd.where_clause.as_ref(),
            &mut hole_vars,
        );
        let pushed_bounds = !fn_scope.bounds.is_empty();
        if pushed_bounds {
            self.current_type_param_bounds.push(fn_scope.clone());
        }
        let param_names = fd
            .params
            .iter()
            .skip(skip)
            .map(|p| p.name.clone())
            .collect();
        let params = fd
            .params
            .iter()
            .skip(skip)
            .map(|p| self.resolve_registered_annotation_ty(&p.ty, &mut hole_vars))
            .collect();
        let declared_return = fd.return_type.as_ref().map_or(Ty::Unit, |ret| {
            self.resolve_registered_annotation_ty(ret, &mut hole_vars)
        });
        if pushed_bounds {
            self.current_type_param_bounds.pop();
        }
        // E_GEN_RETURN_SPELLING recovery + generator/async-generator wrap.
        let return_type =
            self.wrap_fn_return_type(fd, declared_return, fd.return_type.as_ref().map(|(_, s)| s));

        let fn_assoc_bindings = fn_scope.assoc_bindings;
        let sig = FnSig {
            type_params: fd.type_params.as_ref().map_or(vec![], |params| {
                params.iter().map(|p| p.name.clone()).collect()
            }),
            type_param_bounds: self
                .collect_type_param_bounds(fd.type_params.as_ref(), fd.where_clause.as_ref()),
            param_ownership: fd
                .params
                .iter()
                .skip(skip)
                .map(|param| crate::env::ParameterOwnership::from_consume(param.is_consume))
                .collect(),
            param_names,
            params,
            return_type,
            doc_comment: fd.doc_comment.clone(),
            extern_symbol: self.ingest_extern_symbol_attrs(&fd.attributes),
            // Receiver mutability flag — see `FnSig::requires_mutable_receiver`.
            // Only methods (Type::method) can carry a receiver; free functions
            // whose first parameter happens to be named `self` are not methods
            // in this sense (matches the `skip` logic above).
            requires_mutable_receiver: is_method
                && fd
                    .params
                    .first()
                    .is_some_and(|p| self.is_receiver_param(p) && p.is_mutable),
            receiver_update: if fd.origin == hew_parser::ast::DeclarationOrigin::MachineStep {
                super::ReceiverUpdate::Staged
            } else {
                super::ReceiverUpdate::Replace
            },
            consumes_receiver: fd.consumes_self,
            returns_receiver_identity: fd
                .attributes
                .iter()
                .any(|attribute| attribute.name == "returns_receiver"),
            ..FnSig::default()
        };

        // rc1-F1 stage A: mint the fn-sig key from the CANONICAL owning
        // module — a root free function keys `{root_module}.{name}`,
        // identical to the key the same declaration mints when its module is
        // imported. The side registries (`fn_type_param_assoc_bindings`, `fn_sig_inference_holes`,
        // `intrinsic_declarations`) are co-minted under this same key.
        let key = scoped_module_item_name(self.canonical_fn_owner(), name)
            .unwrap_or_else(|| name.to_string());
        self.fn_sigs.insert(key.clone(), sig);
        self.fn_type_param_assoc_bindings
            .insert(key.clone(), fn_assoc_bindings);
        self.record_fn_sig_inference_holes(&key, hole_vars);
        // If the declaration carries `#[intrinsic("name")]`, validate its
        // placement and (if accepted) record the mapping so HIR lowering can
        // skip the body and wire to the catalog.
        if let Some(intrinsic_key) = &fd.intrinsic {
            self.register_intrinsic_declaration(key, intrinsic_key, name, fd);
        }
    }

    /// Validate source declarations against the shared semantic contract before
    /// publishing runtime authority. Other floor operations retain their own
    /// catalogue validation in HIR.
    pub(super) fn validate_intrinsic_signature(
        &mut self,
        key: &str,
        intrinsic_key: &str,
        fd: &FnDecl,
    ) -> bool {
        let Some(family) =
            crate::runtime_call::RuntimeCallFamily::from_catalog_endpoint(intrinsic_key)
        else {
            return true;
        };
        let Some(contract) = family.semantic_contract() else {
            return true;
        };
        let signature_matches = self.fn_sigs.get(key).is_some_and(|signature| {
            let resolve = |ty: &Ty| {
                crate::ResolvedTy::from_ty_with_type_params(
                    ty,
                    &signature.type_params.iter().cloned().collect(),
                )
                .map(|ty| {
                    super::resolve_member_ty(
                        ty,
                        self.current_module.as_deref(),
                        &self.type_defs,
                        &|name| {
                            self.user_opaque_type_names.contains(name)
                                || self.module_registry.is_handle_type(name)
                        },
                    )
                })
            };
            let Ok(params) = signature
                .params
                .iter()
                .map(resolve)
                .collect::<Result<Vec<_>, _>>()
            else {
                return false;
            };
            let Ok(result) = resolve(&signature.return_type) else {
                return false;
            };
            signature
                .type_params
                .iter()
                .map(String::as_str)
                .eq(family.source_intrinsic_type_params().iter().copied())
                && !fd.is_generator
                // A parameter is consumed exactly when the contract moves it.
                && fd.params.len() == contract.arguments.len()
                && fd.params.iter().zip(contract.arguments).all(|(param, argument)| {
                    !param.is_mutable
                        && param.is_consume
                            == (argument.effect == crate::runtime_call::RuntimeArgumentEffect::Move)
                })
                && family
                    .source_intrinsic_declaration()
                    .is_none_or(|expected| expected == key)
                && contract.matches_signature(&params, &result)
        });
        if !signature_matches {
            self.errors.push(TypeError {
                severity: crate::error::Severity::Error,
                kind: TypeErrorKind::IntrinsicSignatureMismatch {
                    intrinsic_key: intrinsic_key.to_string(),
                },
                span: fd.decl_span.clone(),
                message: format!(
                    "canonical intrinsic `{intrinsic_key}` has a source declaration that \
                     does not match its semantic runtime contract"
                ),
                notes: vec![],
                suggestions: vec![
                    "restore the canonical standard-library declaration's name, parameters and \
                     return type"
                        .to_string(),
                ],
                source_module: self.current_module.clone(),
            });
        }
        signature_matches
    }

    /// Validate a `#[intrinsic("…")]` declaration's placement and, if it lives
    /// in a stdlib-floor module **and** is a top-level free function, record
    /// the name→key mapping consumed by HIR lowering.
    ///
    /// P0 surface-immutability gate (A605, plan §7 risk 4): the `#[intrinsic]`
    /// surface is compiler-internal-only. Any declaration outside the
    /// designated stdlib-floor modules — including the root/user module — is a
    /// hard `E_INTRINSIC_OUTSIDE_FLOOR` error so a user program (or any
    /// non-floor module) cannot wire itself to a compiler intrinsic. Fail-closed:
    /// every non-allowlisted module path is rejected, never silently allowed.
    ///
    /// **Two-axis gate** — this is the single complete enforcement point:
    ///
    /// * **(a) Module axis**: the current module must be on the floor allowlist.
    ///   `#[intrinsic]` in the root/user module or any non-floor module is
    ///   rejected with `E_INTRINSIC_OUTSIDE_FLOOR`.
    ///
    /// * **(b) Shape axis**: the `key` must be a top-level free-function name
    ///   (no `::` separator). A method key of the form `"Type::method"` — which
    ///   arises for impl methods, actor methods, and trait-impl methods that flow
    ///   through `register_fn_sig_with_name` — is rejected with
    ///   `E_INTRINSIC_ON_METHOD`, regardless of module. Compiler intrinsics are
    ///   wired to standalone catalog entries; they are never method dispatch
    ///   slots.
    ///
    /// A declaration that passes both axes is inserted into
    /// `intrinsic_declarations`. A rejected declaration is never inserted, so
    /// it cannot become a live intrinsic dispatch target.
    pub(super) fn register_intrinsic_declaration(
        &mut self,
        key: String,
        intrinsic_key: &str,
        name: &str,
        fd: &FnDecl,
    ) {
        // Shape axis (b): a key containing `::` is a method (e.g. `"Type::method"`).
        // Impl methods, actor methods, and trait-impl methods all reach here with
        // such a key via `register_fn_sig_with_name`.  Methods are never valid
        // intrinsic declarations, regardless of which module they live in.
        // Reject early and do NOT insert into `intrinsic_declarations`.
        if key.contains("::") {
            self.errors.push(TypeError {
                severity: crate::error::Severity::Error,
                kind: TypeErrorKind::IntrinsicOnMethod {
                    intrinsic_key: intrinsic_key.to_string(),
                    method_key: key.clone(),
                },
                span: fd.decl_span.clone(),
                message: format!(
                    "`#[intrinsic(\"{intrinsic_key}\")]` on \
                     `{name}` (key `{key}`) is declared on an impl method — \
                     the `#[intrinsic]` surface is valid only on top-level free \
                     functions inside a stdlib-floor module; method dispatch \
                     slots are never wired to compiler intrinsics (A605)"
                ),
                notes: vec![(
                    fd.decl_span.clone(),
                    "Compiler intrinsics are catalog entries keyed on bare function \
                     names; they cannot be dispatched through a receiver. Expose the \
                     intrinsic as a top-level free function in the floor module and \
                     call it from the impl method body if needed."
                        .to_string(),
                    self.current_module.clone(),
                )],
                suggestions: vec![
                    "remove the `#[intrinsic(\"…\")]` attribute from this method".to_string(),
                    "if a new intrinsic is genuinely needed, declare it as a top-level \
                     free function in the appropriate stdlib-floor module"
                        .to_string(),
                ],
                source_module: self.current_module.clone(),
            });
            // Deliberately do NOT record the intrinsic mapping: a rejected
            // declaration must not become a live intrinsic dispatch target.
            return;
        }
        // Module axis (a): the declaration must live in an allowlisted floor
        // module selected from the shipped stdlib source. A user package may
        // spell itself `std.math`, so the module name alone is never authority.
        if is_intrinsic_floor_module(self.current_module.as_deref())
            && self
                .current_module
                .as_ref()
                .is_some_and(|module| self.canonical_std_module_sources.contains(module))
        {
            if !self.validate_intrinsic_signature(&key, intrinsic_key, fd) {
                return;
            }
            self.intrinsic_declarations
                .insert(key, intrinsic_key.to_string());
            return;
        }
        let module_label = self
            .current_module
            .clone()
            .unwrap_or_else(|| "(root)".to_string());
        self.errors.push(TypeError {
            severity: crate::error::Severity::Error,
            kind: TypeErrorKind::IntrinsicOutsideFloor {
                intrinsic_key: intrinsic_key.to_string(),
                module: module_label.clone(),
            },
            span: fd.decl_span.clone(),
            message: format!(
                "`#[intrinsic(\"{intrinsic_key}\")]` on \
                 `{name}` is declared in `{module_label}`, which is not a \
                 stdlib-floor module — the `#[intrinsic]` surface is \
                 compiler-internal-only and cannot be declared by user code"
            ),
            notes: vec![(
                fd.decl_span.clone(),
                "Memory and math intrinsics are wired by the compiler; user \
                 programs call the stdlib functions that the floor exposes, \
                 they never declare `#[intrinsic]` themselves. There is no \
                 user-visible `unsafe`/`@unsafe` surface (A605)."
                    .to_string(),
                self.current_module.clone(),
            )],
            suggestions: vec!["remove the `#[intrinsic(\"…\")]` attribute and call the \
                 corresponding stdlib function instead"
                .to_string()],
            source_module: self.current_module.clone(),
        });
        // Deliberately do NOT record the intrinsic mapping: a rejected
        // declaration must not become a live intrinsic dispatch target.
    }

    pub(super) fn validate_receiver_identity_method(
        &mut self,
        type_name: &str,
        method: &FnDecl,
        return_type: &Ty,
    ) -> bool {
        let identity_attributes: Vec<_> = method
            .attributes
            .iter()
            .filter(|attribute| attribute.name == "returns_receiver")
            .collect();
        if identity_attributes.is_empty() {
            return false;
        }

        let direct_self_tail = method
            .body
            .trailing_expr
            .as_deref()
            .is_some_and(|(expr, _)| matches!(expr, Expr::Identifier(name) if name == "self"));
        let returns_self_type = match return_type {
            Ty::Named {
                name,
                args,
                builtin,
            } if name == "Self"
                || self.strict_names_same_owner(name, *builtin, type_name, None) =>
            {
                self.current_self_type
                    .as_ref()
                    .is_none_or(|(_, self_args)| {
                        args.len() == self_args.len()
                            && args.iter().zip(self_args).all(|(actual, expected)| {
                                self.subst.resolve(actual) == self.subst.resolve(expected)
                            })
                    })
            }
            _ => false,
        };
        let valid = identity_attributes.len() == 1
            && identity_attributes[0].args.is_empty()
            && method.consumes_self
            && direct_self_tail
            && returns_self_type
            && !block_has_explicit_return(&method.body);
        if !valid {
            self.report_error(
                TypeErrorKind::InvalidOperation,
                &method.decl_span,
                format!(
                    "`#[returns_receiver]` on `{type_name}.{}` requires a zero-argument \
                     attribute appearing exactly once, a `consume self` receiver, the same \
                     receiver return type, one direct trailing `self`, and no alternate \
                     `return` path",
                    method.name
                ),
            );
        }
        valid
    }

    /// Retain obligations that the immutable marker and Display services prove.
    /// Other trait/associated-type predicates require the live solver and are
    /// explicitly refused by concrete value-method selection.
    pub(super) fn value_method_obligations(
        &self,
        impl_params: Option<&Vec<TypeParam>>,
        impl_where: Option<&WhereClause>,
        method: &FnDecl,
    ) -> Option<Vec<(String, crate::type_facts::ImplMethodObligation)>> {
        let params: Vec<_> = impl_params
            .into_iter()
            .flatten()
            .chain(method.type_params.iter().flatten())
            .collect();
        let mut obligations = Vec::new();
        let mut add_bounds = |name: &str, bounds: &[TraitBound]| -> Option<()> {
            for bound in bounds {
                if bound
                    .type_args
                    .as_ref()
                    .is_some_and(|args| !args.is_empty())
                    || !bound.assoc_type_bindings.is_empty()
                {
                    return None;
                }
                let identity = self.trait_defs_key_for_bound(&bound.name);
                let obligation = if self
                    .lang_items
                    .get(crate::LANG_ITEM_DISPLAY)
                    .is_some_and(|binding| binding.trait_id.full_path() == identity)
                    || identity == "Display"
                {
                    crate::type_facts::ImplMethodObligation::Display
                } else {
                    match MarkerTrait::from_name(&identity)? {
                        MarkerTrait::Serializable => {
                            crate::type_facts::ImplMethodObligation::Serializable
                        }
                        marker => crate::type_facts::ImplMethodObligation::Marker(marker),
                    }
                };
                obligations.push((name.to_string(), obligation));
            }
            Some(())
        };
        for param in &params {
            add_bounds(&param.name, &param.bounds)?;
        }
        for clause in impl_where.into_iter().chain(method.where_clause.as_ref()) {
            for predicate in &clause.predicates {
                let TypeExpr::Named { name, type_args } = &predicate.ty.0 else {
                    return None;
                };
                if type_args.as_ref().is_some_and(|args| !args.is_empty())
                    || !params.iter().any(|param| param.name == *name)
                {
                    return None;
                }
                add_bounds(name, &predicate.bounds)?;
            }
        }
        Some(obligations)
    }

    /// Register an impl method on a type's method table and `fn_sigs`.
    ///
    /// `impl_type_params` carries the enclosing `impl<T, U, …>` type
    /// parameter names so they are included in the resulting `FnSig`.
    ///
    /// `impl_where_clause` carries the enclosing impl block's where-clause so
    /// that bounds of the form `impl<T> Holder<T> where T: Display` are
    /// propagated into the method signature — both the `td.methods` entry and
    /// the `fn_sigs` entry consulted by `type_param_carries_bound`.
    ///
    /// **Important**: the caller must have already pushed the impl-level type
    /// params into `self.generic_ctx` so that type resolution sees them.
    ///
    /// Returns the built `FnSig` for callers that need to insert it
    /// on additional type names (e.g., qualified aliases).
    #[allow(
        clippy::too_many_lines,
        reason = "single-source-of-truth for impl-method registration; \
                  factoring sub-passes would obscure the ordering invariants \
                  the surrounding code relies on (bounds push/pop, \
                  receiver-skip, double-write of fn_sigs + td.methods, \
                  W3.001 Stage-2 extern_symbol mirror)"
    )]
    pub(in crate::check) fn register_impl_method(
        &mut self,
        type_name: &str,
        method: &FnDecl,
        impl_type_params: Option<&Vec<TypeParam>>,
        impl_where_clause: Option<&WhereClause>,
        trait_bound: Option<&TraitBound>,
    ) -> FnSig {
        let method_key = format!("{type_name}::{}", method.name);
        let Some(declaration_id) = self.impl_method_declaration_id(type_name, method, trait_bound)
        else {
            return FnSig::default();
        };
        if trait_bound.is_none() && method.consumes_self {
            self.consuming_inherent_methods
                .insert(declaration_id.clone());
        }
        // Preserve the exact trait declaration selected while this impl's
        // source scope is active. A bare module import can make a trait
        // unambiguous without publishing it as an ordinary named import; HIR
        // still needs the checker-owned identity for static dispatch and must
        // not reconstruct it from the trait's leaf spelling.
        if let Some(bound) = trait_bound {
            let type_identity = self.trait_impl_type_identity(type_name);
            let trait_identity = self.trait_defs_key_for_bound(&bound.name);
            let receiver_args = self
                .current_self_type
                .as_ref()
                .map(|(_, args)| args.clone())
                .unwrap_or_default();
            let receiver = Ty::from_name(&type_identity).unwrap_or_else(|| Ty::Named {
                name: type_identity.clone(),
                args: receiver_args,
                builtin: None,
            });
            self.trait_impl_method_binders.insert(
                declaration_id.clone(),
                crate::type_facts::ImplMethodBinders {
                    receiver: self.normalize_for_use(&receiver),
                    obligations: self.value_method_obligations(
                        impl_type_params,
                        impl_where_clause,
                        method,
                    ),
                    impl_params: impl_type_params
                        .into_iter()
                        .flatten()
                        .map(|param| param.name.clone())
                        .collect(),
                    method_params: method
                        .type_params
                        .iter()
                        .flatten()
                        .map(|param| param.name.clone())
                        .collect(),
                },
            );
            let exact_type_identity = impl_type_params
                .is_none_or(Vec::is_empty)
                .then(|| {
                    self.current_self_type
                        .as_ref()
                        .filter(|(self_type_name, args)| {
                            self_type_name == type_name && !args.is_empty()
                        })
                        .and_then(|(_, args)| {
                            args.iter()
                                .map(|ty| ResolvedTy::from_ty(&self.subst.resolve(ty)).ok())
                                .collect::<Option<Vec<_>>>()
                        })
                        .and_then(|args| {
                            crate::resolved_ty::mangle_impl_self_name(&type_identity, &args)
                        })
                })
                .flatten();
            if let Some(exact_type_identity) = exact_type_identity {
                self.trait_impl_method_declaration_ids.insert(
                    (
                        exact_type_identity,
                        trait_identity.clone(),
                        method.name.clone(),
                    ),
                    declaration_id.clone(),
                );
            }
            let nominal_key = (type_identity, trait_identity, method.name.clone());
            if impl_type_params.is_some_and(|params| !params.is_empty()) {
                // A concrete specialization must not occupy the generic fallback
                // simply because it was registered before the generic impl.
                self.trait_impl_method_declaration_ids
                    .insert(nominal_key, declaration_id.clone());
            } else {
                self.trait_impl_method_declaration_ids
                    .entry(nominal_key)
                    .or_insert_with(|| declaration_id.clone());
            }
            if let Some(ids) = self.trait_method_call_target_ids(&bound.name, &method.name) {
                self.trait_method_ids_by_binding.insert(
                    (
                        self.current_module.clone(),
                        self.current_module_idx,
                        bound.name.clone(),
                        method.name.clone(),
                    ),
                    ids,
                );
            }
        }
        // Push impl-level bounds onto the resolver's stack so the method
        // signature can reference `T::Bar` where `T` is an impl type param
        // (e.g. `impl<I: Iterator> Foo for X { fn next() -> I::Item }`).
        let mut impl_scope_holes = Vec::new();
        let impl_bounds_map = self.collect_type_param_scope_with_assoc_bindings(
            impl_type_params,
            impl_where_clause,
            &mut impl_scope_holes,
        );
        let pushed_impl_bounds = !impl_bounds_map.bounds.is_empty();
        if pushed_impl_bounds {
            self.current_type_param_bounds.push(impl_bounds_map);
        }
        // The impl's own params are already pushed above; `register_fn_sig_with_name`
        // pushes the method's own params internally. Enable scope-local resolution
        // for this primary signature registration so an out-of-scope generic name is
        // rejected at the annotation. The secondary resolution below stays on the
        // program-wide fallback: out-of-scope names are already rejected here, and
        // the impl/method's legitimate params are in `declared_type_param_names`.
        let impl_method_sig_scope = self.enter_primary_sig_scope(&[]);
        self.register_fn_sig_with_name(&method_key, method);
        self.exit_primary_sig_scope(impl_method_sig_scope);
        if pushed_impl_bounds {
            self.current_type_param_bounds.pop();
        }

        // Patch the fn_sigs entry to include impl-level type params and their
        // bounds. `register_fn_sig_with_name` only records method-level params,
        // so `type_param_carries_bound` would otherwise miss impl-level bounds
        // such as `T: Display` in `impl<T: Display> Holder<T>`.
        if let Some(impl_tps) = impl_type_params {
            let mut impl_scope_holes = Vec::new();
            let impl_scope = self.collect_type_param_scope_with_assoc_bindings(
                impl_type_params,
                impl_where_clause,
                &mut impl_scope_holes,
            );
            let impl_bounds = impl_scope.bounds;
            let impl_assoc_bindings = impl_scope.assoc_bindings;
            let key = scoped_module_item_name(self.current_module.as_deref(), &method_key)
                .unwrap_or_else(|| method_key.clone());
            if let Some(sig) = self.fn_sigs.get_mut(&key) {
                for tp in impl_tps {
                    if !sig.type_params.contains(&tp.name) {
                        sig.type_params.push(tp.name.clone());
                    }
                }
                for (param, bounds) in impl_bounds {
                    let entry = sig.type_param_bounds.entry(param).or_default();
                    for bound in bounds {
                        Self::push_unique_bound(entry, &bound);
                    }
                }
            }
            let bindings = self.fn_type_param_assoc_bindings.entry(key).or_default();
            for (assoc_key, ty) in impl_assoc_bindings {
                bindings.entry(assoc_key).or_insert(ty);
            }
        }

        let skip = usize::from(
            method
                .params
                .first()
                .is_some_and(|p| self.is_receiver_param(p)),
        );
        let param_names: Vec<String> = method
            .params
            .iter()
            .skip(skip)
            .map(|p| p.name.clone())
            .collect();

        // A method type parameter that shadows an enclosing one is REFUSED.
        // See `reject_shadowing_method_type_params` for the decision and why
        // scoping the two apart is not the fix.
        // Every owner whose parameters this method could shadow: the `impl`
        // block's own, and — for a trait impl — the trait's, which the impl
        // block never mentions. Collected together so a method shadowing both
        // reports once, naming both.
        let mut shadow_owners: Vec<(Vec<String>, String)> = Vec::new();
        if let Some(impl_tps) = impl_type_params {
            let params: Vec<String> = impl_tps.iter().map(|tp| tp.name.clone()).collect();
            if !params.is_empty() {
                shadow_owners.push((params, format!("the `impl` block on `{type_name}`")));
            }
        }
        if let Some(bound) = trait_bound {
            let params = self.trait_type_param_names(&bound.name);
            if !params.is_empty() {
                shadow_owners.push((params, format!("trait `{}`", bound.name)));
            }
        }
        if !shadow_owners.is_empty() {
            // The declaration is the impl METHOD, so its identity is the
            // implementing module's type and method name — not the trait's key,
            // which every file implementing that trait would share.
            let owner =
                Self::method_declaration_key(&self.declaration_owner_key(type_name), &method.name);
            self.reject_shadowing_method_type_params(
                method.type_params.as_ref(),
                &shadow_owners,
                &owner,
                &method.decl_span,
            );
        }

        // Collect type param names: impl-level + method-level.
        let mut all_type_params: Vec<String> = impl_type_params
            .map(|tps| tps.iter().map(|tp| tp.name.clone()).collect())
            .unwrap_or_default();
        if let Some(method_tps) = &method.type_params {
            all_type_params.extend(method_tps.iter().map(|tp| tp.name.clone()));
        }

        // Collect bounds from both the impl's type params/where-clause and the
        // method's own where-clause. Impl-level bounds cover both inline
        // (`impl<T: Display>`) and where-clause (`impl<T> … where T: Display`)
        // shapes because `collect_type_param_bounds` reads both sources.
        let mut type_param_bounds =
            self.collect_type_param_bounds(impl_type_params, impl_where_clause);
        for (type_param, bounds) in self
            .collect_type_param_bounds(method.type_params.as_ref(), method.where_clause.as_ref())
        {
            let entry = type_param_bounds.entry(type_param).or_default();
            for bound in bounds {
                Self::push_unique_bound(entry, &bound);
            }
        }
        // Method where-clause may also constrain impl-level type params (e.g.
        // an additional bound on T added at the method level).
        for (type_param, bounds) in
            self.collect_type_param_bounds(impl_type_params, method.where_clause.as_ref())
        {
            let entry = type_param_bounds.entry(type_param).or_default();
            for bound in bounds {
                Self::push_unique_bound(entry, &bound);
            }
        }
        // Re-use the primary signature registered above. Besides keeping
        // `extern_symbol` validation single-shot, this preserves the exact
        // tracked inference variables shared with body checking. Every method
        // table must mirror this carrier rather than re-resolving annotations
        // into unrelated `Ty::Var`s.
        let registered_key = scoped_module_item_name(self.current_module.as_deref(), &method_key)
            .unwrap_or_else(|| method_key.clone());
        let registered = self
            .fn_sigs
            .get(&registered_key)
            .expect("register_fn_sig_with_name must publish the impl method");
        let params = registered.params.clone();
        let return_type = registered.return_type.clone();
        let extern_symbol = registered.extern_symbol.clone();

        let mut sig = FnSig {
            impl_method: Some(super::types::ImplMethodProvenance {
                declaration: declaration_id.clone(),
                receiver: self
                    .lookup_declaration(
                        &self
                            .canonical_nominal_name(type_name)
                            .unwrap_or_else(|| self.trait_impl_type_identity(type_name)),
                    )
                    .cloned(),
                name: method.name.clone(),
                is_inherent: trait_bound.is_none(),
                span: if method.decl_span.is_empty() {
                    method.fn_span.clone()
                } else {
                    method.decl_span.clone()
                },
            }),
            param_ownership: registered.param_ownership.clone(),
            type_params: all_type_params,
            type_param_bounds,
            param_names,
            params,
            return_type,
            extern_symbol,
            // Mirror `register_fn_sig_with_name`'s computation so that
            // `lookup_named_method_sig` (which prefers `td.methods` before
            // `fn_sigs`) returns a sig with the receiver-mutability flag
            // set. Without this, the call-site mutable-binding gate at
            // `methods.rs` (Q297 Stage 1) silently misses every trait impl
            // method on a user type.
            requires_mutable_receiver: method
                .params
                .first()
                .is_some_and(|p| self.is_receiver_param(p) && p.is_mutable),
            receiver_update: if method.origin == hew_parser::ast::DeclarationOrigin::MachineStep {
                super::ReceiverUpdate::Staged
            } else {
                super::ReceiverUpdate::Replace
            },
            consumes_receiver: method.consumes_self,
            ..FnSig::default()
        };
        sig.returns_receiver_identity =
            self.validate_receiver_identity_method(type_name, method, &sig.return_type);
        // Keep the primary signature's tracked inference variables. In
        // particular, an impl return annotation `-> _` must share the same
        // `Ty::Var` with body checking and every method-table mirror. Rebuilding
        // and reinserting the annotation here would create an unrelated hole
        // that can never be resolved by the method body.
        self.fn_sigs.insert(registered_key.clone(), sig.clone());
        if sig.extern_symbol.is_some() {
            let declaring_module = self
                .registration_origin_module
                .clone()
                .or_else(|| self.current_module.clone());
            let trusted_compiled_stdlib = self.registration_origin_module.is_some()
                || declaring_module
                    .as_deref()
                    .is_some_and(|module| self.checking_canonical_stdlib_source(module));
            let origin = (declaring_module, trusted_compiled_stdlib);
            self.extern_method_origins
                .insert(method_key.clone(), origin.clone());
            self.extern_method_origins.insert(registered_key, origin);
        }
        self.publish_impl_method_sig(type_name, &method.name, &sig);
        // Preserve every exact declaration even when a trait method or concrete
        // specialisation shares the ordinary receiver/method lookup spelling.
        self.fn_sigs
            .insert(declaration_id.full_path().to_string(), sig.clone());
        // D442: a `#[resource]` / `#[opaque]` type's inherent `close` must
        // consume its receiver. A borrowing `close(self)` runs the implicit
        // scope-exit release a second time when a caller invokes `close()`
        // explicitly — the receiver is still live afterward, so the
        // scope-exit drop dispatches `close` again. Only the inherent form is
        // checked here (mirrors the HIR W3.030 discipline, which only walks
        // trait-free `impl T { fn close }` blocks); trait implementation
        // signatures are validated separately.
        if method.name == "close"
            && !method.consumes_self
            && trait_bound.is_none()
            && (self.registry.is_resource(type_name)
                || self.user_opaque_type_names.contains(type_name))
        {
            let span = if method.decl_span.start == method.decl_span.end {
                method.fn_span.clone()
            } else {
                method.decl_span.clone()
            };
            self.errors.push(TypeError::new(
                TypeErrorKind::ResourceCloseMustConsume,
                span,
                "`close` on a resource type must consume its receiver; write \
                 `fn close(consume self)`",
            ));
        }
        // A `consume self` inherent method moves its receiver at every call
        // site. Register the qualified `Type::method` name into the
        // consume-receiver set so the dispatch site marks the receiver moved
        // (a later use surfaces `UseAfterMove`) and records the per-call-site
        // flag HIR lowers as `IntentKind::Consume`. This is the single
        // authority the move-checker reads (`checker-output-boundary`); keyed
        // on the resolved-method consume fact, never re-inferred from AST shape
        // downstream.
        if method.consumes_self {
            self.consume_receiver_methods.insert(method_key.clone());
        }

        // Concrete-specialised-impl dual registration (#2270).
        //
        // When two `impl Trait for Wrapper<i64>` and `impl Trait for Wrapper<string>`
        // blocks are present, both produce `method_key = "Wrapper::describe"`.  The
        // second call clobbers the first in `fn_sigs`, and codegen later emits two
        // LLVM functions under the same name → linkage crash.
        //
        // `impl_method_declaration_keys` owns the whole decision: a generic
        // declaration keeps the shared dispatch key, a concrete specialisation
        // takes only its mangled `"Wrapper$$i64::describe"` keys.  The bare key
        // entry in `fn_sigs` is kept as a fallback for lookup paths that have
        // not yet been updated (e.g. method-set validation, external lookup).
        let keys = self.impl_method_declaration_keys(
            type_name,
            &method.name,
            impl_type_params.map(Vec::as_slice),
        );
        self.publish_impl_method_declaration_id(&keys, &declaration_id);
        for key in keys.canonical.iter().chain(&keys.mangled) {
            // Publish the signature under the same module-owned identity as
            // the declaration, including imported non-generic methods. A previous
            // concrete-impl registration may already be present; overwriting is
            // correct because each impl block processes its own concrete args
            // in sequence.
            self.fn_sigs.insert(key.clone(), sig.clone());
            // Propagate consume-receiver membership to the same key
            // so HIR dispatch does not lose the move contract.
            if method.consumes_self {
                self.consume_receiver_methods.insert(key.clone());
            }
        }

        sig
    }

    /// Publish one resolved impl-method signature onto every type-definition
    /// entry a receiver can be spelled through.
    ///
    /// Two entries exist for a module-declared type: the BARE compatibility
    /// entry (`Dog`) and the declaring module's exact entry (`gm.Dog`). A
    /// receiver produced by the module's own constructor resolves through the
    /// qualified entry, so a method published only onto the bare entry is
    /// invisible and the call reports "no method" against the qualified type.
    /// Explicit impl methods always published to both; materialised trait
    /// defaults published only to the bare entry, which is why a trait default
    /// declared in an imported module never resolved on its implementing type.
    /// Both producers route through here so the two method classes cannot
    /// drift apart again.
    pub(super) fn publish_impl_method_sig(
        &mut self,
        type_name: &str,
        method_name: &str,
        sig: &FnSig,
    ) {
        if let Some(td) = self.lookup_type_def_mut(type_name) {
            td.methods.insert(method_name.to_string(), sig.clone());
        }
        // The bare type table is a compatibility surface and is
        // last-writer-wins across modules. Attach the method to the declaring
        // module's exact type definition as well; qualified receivers must
        // never recover methods through the polluted bare entry.
        if !type_name.contains('.') {
            if let Some(module) = self.current_module.clone() {
                let qualified_type = format!("{module}.{type_name}");
                if let Some(td) = self.type_defs.get_mut(&qualified_type) {
                    td.methods.insert(method_name.to_string(), sig.clone());
                }
            }
        }
    }

    /// Derive every key one impl method's declaration identity is published
    /// under, from the shape of the enclosing impl block.
    ///
    /// A generic declaration (`impl<T> Render for Box<T>`) owns the shared
    /// `Type::method` dispatch key and its module-canonical form. A concrete
    /// specialisation (`impl Render for Box<i64>`) owns ONLY the mangled
    /// `Type$$i64::method` keys, so it cannot overwrite the generic
    /// declaration's identity for a different receiver instance.
    ///
    /// Explicit impl methods and materialized trait defaults both publish
    /// through this one derivation. When the two producers keyed the same
    /// declaration differently, a specialisation's materialized default
    /// overwrote the generic impl's entry under the shared key, HIR's impl
    /// block then advertised a declaration its emitted body did not carry, and
    /// the generic default lost its monomorphisation entirely.
    pub(super) fn impl_method_declaration_keys(
        &self,
        type_name: &str,
        method_name: &str,
        impl_type_params: Option<&[TypeParam]>,
    ) -> ImplMethodDeclarationKeys {
        // The compatibility dispatch key follows the SPELLING the impl was
        // written with, because that is what HIR reconstructs the emitted
        // symbol from; the canonical key follows the identity the target
        // resolves to, which is what method resolution looks up. They are the
        // same string unless the impl targets a type through a module binding.
        let surface_name = self
            .current_impl_surface_target
            .as_deref()
            .unwrap_or(type_name);
        let shared = format!("{surface_name}::{method_name}");
        let identity_key = format!("{type_name}::{method_name}");
        // `scoped_module_item_name` deliberately rejects presentation names
        // containing `::`; an impl method key necessarily has that separator.
        // Build the module-owned form directly.
        let module_owned = |key: &str, owner: &str| -> String {
            if owner.contains('.') {
                key.to_string()
            } else {
                self.current_module
                    .as_ref()
                    .map_or_else(|| key.to_string(), |module| format!("{module}.{key}"))
            }
        };
        let canonical = module_owned(&identity_key, type_name);
        // No impl-level type params means this impl block is concrete; it is a
        // specialisation only when its self type also carries concrete args.
        let is_concrete_specialised_impl = impl_type_params.is_none_or(<[TypeParam]>::is_empty);
        let concrete_receiver_args = self
            .current_self_type
            .as_ref()
            .filter(|(self_type_name, args)| self_type_name == type_name && !args.is_empty())
            .map(|(_, args)| args.clone());
        let Some(self_type_args) = concrete_receiver_args.filter(|_| is_concrete_specialised_impl)
        else {
            return ImplMethodDeclarationKeys {
                shared: Some(shared),
                canonical: Some(canonical),
                mangled: Vec::new(),
            };
        };
        // Resolve each type arg to a concrete `ResolvedTy` before mangling.
        // An arg carrying an inference variable or error node cannot appear for
        // a concrete specialised impl; if one does, the specialisation
        // publishes nothing rather than falling back onto the generic
        // declaration's shared key.
        let mangled = self_type_args
            .iter()
            .map(|ty| ResolvedTy::from_ty(ty).ok())
            .collect::<Option<Vec<_>>>()
            .and_then(|resolved_args| {
                crate::resolved_ty::mangle_impl_self_name(type_name, &resolved_args)
            })
            .map(|mangled_self| {
                let key = format!("{mangled_self}::{method_name}");
                let canonical_key = module_owned(&key, &mangled_self);
                vec![key, canonical_key]
            })
            .unwrap_or_default();
        ImplMethodDeclarationKeys {
            shared: None,
            canonical: None,
            mangled,
        }
    }

    /// Publish one impl method's declaration identity under the keys
    /// [`Self::impl_method_declaration_keys`] derived for it.
    pub(super) fn publish_impl_method_declaration_id(
        &mut self,
        keys: &ImplMethodDeclarationKeys,
        declaration_id: &crate::DefId,
    ) {
        if let Some(shared) = &keys.shared {
            if self.registration_is_flat_file_import {
                // File-import items are lowered as root items after checking.
                // Their shared dispatch entry therefore outranks any package
                // module's compatibility alias, independent of graph/import
                // traversal order.
                self.impl_method_declaration_ids
                    .insert(shared.clone(), declaration_id.clone());
            } else {
                self.impl_method_declaration_ids
                    .entry(shared.clone())
                    .or_insert_with(|| declaration_id.clone());
            }
        }
        for key in keys.canonical.iter().chain(&keys.mangled) {
            self.impl_method_declaration_ids
                .insert(key.clone(), declaration_id.clone());
        }
    }

    /// Allocate an implementation-method declaration identity while its source
    /// impl is in scope. The declared self pattern distinguishes a generic
    /// `impl<T> Trait for Box<T>` from `impl Trait for Box<i64>`; concrete call
    /// arguments deliberately do not participate.
    pub(super) fn impl_method_declaration_id(
        &mut self,
        type_name: &str,
        method: &FnDecl,
        trait_bound: Option<&TraitBound>,
    ) -> Option<crate::DefId> {
        let receiver = if self.registration_is_flat_file_import || type_name.contains('.') {
            type_name.to_string()
        } else {
            self.canonical_nominal_name(type_name).unwrap_or_else(|| {
                self.current_module.as_ref().map_or_else(
                    || type_name.to_string(),
                    |module| format!("{module}.{type_name}"),
                )
            })
        };
        let declared_args = self
            .current_self_type
            .as_ref()
            .filter(|(self_type_name, _)| self_type_name == type_name)
            .map(|(_, args)| args)
            .filter(|args| !args.is_empty())
            .map(|args| {
                args.iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(", ")
            });
        let declared_receiver =
            declared_args.map_or_else(|| receiver.clone(), |args| format!("{receiver}<{args}>"));
        let trait_identity = trait_bound.map_or_else(
            || "inherent".to_string(),
            |bound| {
                if self.registration_is_flat_file_import {
                    bound.name.clone()
                } else {
                    self.trait_defs_key_for_bound(&bound.name)
                }
            },
        );
        let path = format!(
            "{receiver}::<impl {trait_identity} for {declared_receiver}>::{}",
            method.name
        );
        let occurrence = crate::DeclarationOccurrence::new_with_synthetic_ordinal(
            self.current_declaration_module(),
            &method.fn_span,
            self.current_item_ordinal,
            crate::DeclarationKind::ImplMethod,
            0,
        );
        if let Ok(declaration) = self.identity.declare(occurrence, path.clone()) {
            return Some(declaration);
        }
        // Impl methods are registered per route, and a shipped module reached
        // through the compiled-in stdlib pass and through the module graph
        // presents the same method under two occurrences. The canonical path
        // already names the receiver, the trait and the method, so an
        // established row for it is that same method: resolve it rather than
        // mint a second identity. A genuinely duplicated impl method collides
        // on this path too, and impl registration reports that.
        self.lookup_declaration(&path).cloned()
    }

    /// Rename method-level type parameter names in `ty` from the trait's
    /// declared names to the impl's declared names, paired positionally.
    ///
    /// Accepts the legitimate case where an impl renames a trait's method
    /// type param (`fn map<T>` in the trait, `fn map<U>` in the impl): after
    /// renaming, `Ty::Named { name: "T" }` becomes `Ty::Named { name: "U" }`
    /// in the expected sig so structural equality with the impl sig holds.
    ///
    /// Only renames when both sides declare the same number of method-level
    /// type params. Skips otherwise so a separate arity-of-type-params
    /// diagnostic (future) is not preempted.
    pub(super) fn rename_method_type_params(
        ty: &Ty,
        trait_method_tps: Option<&Vec<hew_parser::ast::TypeParam>>,
        impl_method_tps: Option<&Vec<hew_parser::ast::TypeParam>>,
    ) -> Ty {
        let trait_names: Vec<&str> = trait_method_tps
            .map(|v| v.iter().map(|tp| tp.name.as_str()).collect())
            .unwrap_or_default();
        let impl_names: Vec<&str> = impl_method_tps
            .map(|v| v.iter().map(|tp| tp.name.as_str()).collect())
            .unwrap_or_default();
        if trait_names.is_empty() || trait_names.len() != impl_names.len() {
            return ty.clone();
        }
        // Build the full rename map and substitute in parallel.  Sequential
        // substitution aliases entries when trait names and impl names
        // permute: renaming T→U then U→T would map both back to T.
        // Identity entries (t == u) are harmless to include.
        let subst_map: HashMap<String, Ty> = trait_names
            .iter()
            .zip(impl_names.iter())
            .map(|(t, u)| {
                (
                    (*t).to_string(),
                    Ty::Named {
                        builtin: None,
                        name: (*u).to_string(),
                        args: vec![],
                    },
                )
            })
            .collect();
        ty.substitute_named_params_parallel(&subst_map)
    }

    pub(in crate::check) fn register_receive_fn(&mut self, actor_name: &str, rf: &ReceiveFnDecl) {
        let mut generic_bindings = std::collections::HashMap::new();
        if let Some(type_params) = &rf.type_params {
            for tp in type_params {
                generic_bindings.insert(
                    tp.name.clone(),
                    Ty::Named {
                        builtin: None,
                        name: tp.name.clone(),
                        args: vec![],
                    },
                );
            }
        }
        if !generic_bindings.is_empty() {
            self.generic_ctx.push(generic_bindings);
        }

        let mut hole_vars = Vec::new();
        let rf_scope = self.collect_type_param_scope_with_assoc_bindings(
            rf.type_params.as_ref(),
            rf.where_clause.as_ref(),
            &mut hole_vars,
        );
        let pushed_rf_bounds = !rf_scope.bounds.is_empty();
        if pushed_rf_bounds {
            self.current_type_param_bounds.push(rf_scope.clone());
        }

        let param_names = rf.params.iter().map(|p| p.name.clone()).collect();
        let params = rf
            .params
            .iter()
            .map(|p| self.resolve_registered_annotation_ty(&p.ty, &mut hole_vars))
            .collect();
        let declared_return_type = rf.return_type.as_ref().map_or(Ty::Unit, |ret| {
            self.resolve_registered_annotation_ty(ret, &mut hole_vars)
        });
        let return_type = if rf.is_generator {
            Ty::stream(declared_return_type)
        } else {
            declared_return_type
        };

        if pushed_rf_bounds {
            self.current_type_param_bounds.pop();
        }
        if rf.type_params.as_ref().is_some_and(|tp| !tp.is_empty()) {
            self.generic_ctx.pop();
        }

        let type_param_bounds =
            self.collect_type_param_bounds(rf.type_params.as_ref(), rf.where_clause.as_ref());
        let sig = FnSig {
            type_params: rf.type_params.as_ref().map_or(vec![], |params| {
                params.iter().map(|p| p.name.clone()).collect()
            }),
            type_param_bounds,
            param_names,
            params,
            return_type,
            ..FnSig::default()
        };

        let method_name = format!("{}::{}", actor_name, rf.name);
        if rf.is_generator {
            self.receive_generator_methods.insert(method_name.clone());
        }
        if matches!(
            rf.return_type.as_ref().map(|ty| &ty.0),
            Some(hew_parser::ast::TypeExpr::Fallible { .. })
        ) {
            self.receive_fails_methods.insert(method_name.clone());
        }
        self.actor_receive_methods.insert(method_name.clone());
        self.record_fn_sig_inference_holes(&method_name, hole_vars);
        self.fn_type_param_assoc_bindings
            .insert(method_name.clone(), rf_scope.assoc_bindings);
        self.fn_sigs.insert(method_name, sig);
    }

    /// Build a `FnSig` from a function declaration (used for user module registration).
    pub(in crate::check) fn build_fn_sig_from_decl_with_assoc(
        &mut self,
        fd: &FnDecl,
    ) -> (FnSig, HashMap<(String, String, String), Ty>) {
        let mut hole_vars = Vec::new();
        let scope = self.collect_type_param_scope_with_assoc_bindings(
            fd.type_params.as_ref(),
            fd.where_clause.as_ref(),
            &mut hole_vars,
        );
        let pushed_bounds = !scope.bounds.is_empty();
        if pushed_bounds {
            self.current_type_param_bounds.push(scope.clone());
        }
        let param_names = fd.params.iter().map(|p| p.name.clone()).collect();
        let params = fd
            .params
            .iter()
            .map(|p| self.resolve_registered_annotation_ty(&p.ty, &mut hole_vars))
            .collect();
        let declared_return = fd.return_type.as_ref().map_or(Ty::Unit, |ret| {
            self.resolve_registered_annotation_ty(ret, &mut hole_vars)
        });
        if pushed_bounds {
            self.current_type_param_bounds.pop();
        }
        let type_params = fd.type_params.as_ref().map_or(vec![], |params| {
            params.iter().map(|p| p.name.clone()).collect()
        });
        // E_GEN_RETURN_SPELLING recovery + generator/async-generator wrap.
        let return_type =
            self.wrap_fn_return_type(fd, declared_return, fd.return_type.as_ref().map(|(_, s)| s));
        let assoc_bindings = scope.assoc_bindings;
        let sig = FnSig {
            type_params,
            type_param_bounds: self
                .collect_type_param_bounds(fd.type_params.as_ref(), fd.where_clause.as_ref()),
            param_ownership: fd
                .params
                .iter()
                .map(|param| crate::env::ParameterOwnership::from_consume(param.is_consume))
                .collect(),
            param_names,
            params,
            return_type,
            doc_comment: fd.doc_comment.clone(),
            ..FnSig::default()
        };
        (sig, assoc_bindings)
    }
}
