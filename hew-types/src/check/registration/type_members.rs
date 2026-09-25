//! Checker methods grouped by responsibility: type members.
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
    pub(super) fn refresh_handle_bearing_structs(&mut self) {
        // Tracked for testing: callers can assert this stays O(1) after the
        // deferred-refresh fix (see `ensure_handle_bearing_fresh`).
        self.refresh_call_count += 1;

        let struct_names: Vec<String> = self
            .type_defs
            .iter()
            .filter_map(|(name, type_def)| {
                (type_def.kind == TypeDefKind::Struct).then_some(name.clone())
            })
            .collect();

        self.handle_bearing_structs = struct_names
            .into_iter()
            .filter(|name| self.type_name_contains_owned_handle(name, &mut HashSet::new()))
            .collect();
    }

    /// Refresh the handle-bearing set once, iff it has been dirtied since the
    /// last refresh. Converts O(N²) repeated full scans during batch
    /// registration into a single fixpoint pass before the first lookup.
    pub(in crate::check) fn ensure_handle_bearing_fresh(&mut self) {
        if self.handle_bearing_dirty {
            self.handle_bearing_dirty = false;
            self.refresh_handle_bearing_structs();
        }
    }

    pub(in crate::check) fn canonical_owned_handle_type_name(
        &self,
        type_name: &str,
    ) -> Option<String> {
        self.module_registry
            .canonical_owned_type_identity(type_name)
    }

    pub(in crate::check) fn registered_type_def_name(&self, name: &str) -> Option<String> {
        if self.type_defs.contains_key(name) {
            return Some(name.to_string());
        }
        self.strip_module_prefix(name)
            .filter(|unqualified| self.type_defs.contains_key(*unqualified))
            .map(str::to_string)
    }

    pub(in crate::check) fn structural_member_types_for_type(type_def: &TypeDef) -> Vec<Ty> {
        let mut member_types: Vec<Ty> = type_def.fields.values().cloned().collect();
        for variant in type_def.variants.values() {
            match variant {
                VariantDef::Unit => {}
                VariantDef::Tuple(tys) => member_types.extend(tys.iter().cloned()),
                VariantDef::Struct(fields) => {
                    member_types.extend(fields.iter().map(|(_, ty)| ty.clone()));
                }
            }
        }
        member_types
    }

    /// Expand type aliases in a member-type list before it is handed to the
    /// `TraitRegistry` for marker derivation (Send/Frozen/Sync/Copy/Eq/Hash/
    /// Encode/Decode/…). `TraitRegistry` has no alias table of its own — it
    /// only knows nominal struct/record/enum member sets — so a field typed
    /// `Ty::Named { Label }` where `Label` is a top-level alias reads as an
    /// unknown nominal and derives conservatively false for every marker
    /// (`cannot send AppConfig to actor: type is not Send` even when `Label`
    /// is `string`). `type_def.fields` itself stays unexpanded: alias
    /// identity is still needed at annotation/impl-lookup sites (A316); only
    /// this admission-facing copy is normalized.
    pub(super) fn expand_for_marker_registration(&self, types: &[Ty]) -> Vec<Ty> {
        types.iter().map(|ty| self.normalize_for_use(ty)).collect()
    }

    /// Record the nominal type name a top-level item declares (if any) into
    /// `declared_nominal_type_names`. Mirrors the type-name registration in
    /// `collect_types`, but is a program-wide harvest the undefined-named-type
    /// guard consults so an imported module's own types/traits resolve even in
    /// the pass that registers that module's signatures.
    pub(super) fn collect_item_nominal_type_name(&mut self, item: &Item) {
        match item {
            Item::TypeDecl(td) => {
                self.declared_nominal_type_names.insert(td.name.to_string());
            }
            Item::TypeAlias(ta) => {
                self.declared_nominal_type_names.insert(ta.name.to_string());
            }
            Item::Trait(tr) => {
                self.declared_nominal_type_names.insert(tr.name.to_string());
            }
            Item::Actor(ad) => {
                self.declared_nominal_type_names.insert(ad.name.to_string());
            }
            Item::Supervisor(sd) => {
                self.declared_nominal_type_names.insert(sd.name.to_string());
            }
            Item::Record(rd) => {
                self.declared_nominal_type_names.insert(rd.name.to_string());
            }
            _ => {}
        }
    }

    #[expect(
        clippy::too_many_lines,
        reason = "type registration handles all root item variants in one place"
    )]
    /// Pass 1: Collect type definitions
    pub(in crate::check) fn collect_types(&mut self, program: &Program) {
        // Pre-register TypeDecls from non-root module_graph modules into
        // `type_defs` so non-root module body checking can access struct
        // fields and enum variants of types defined within those modules.
        //
        // Uses `pre_register_type_decl` which populates `type_defs` with
        // correct field/variant data but skips `type_def_spans` (so the
        // import path's `register_type_namespace_name` succeeds) and skips
        // trait-registry / wire-method side effects (those are handled by
        // the import path's full `register_type_decl` for pub types, and
        // are not needed for internal non-pub types).
        if let Some(ref mg) = program.module_graph {
            let span_indices = mg.file_span_indices();
            for mod_id in &mg.topo_order {
                if *mod_id == mg.root {
                    continue;
                }
                if let Some(module) = mg.modules.get(mod_id) {
                    let module_name = mod_id.dotted();
                    self.current_module = Some(module_name.clone());
                    self.seed_resolved_lifecycle_import_bindings(module, Some(&module_name), mg);
                    // Temporarily scope local_type_defs so that resolve_type_expr
                    // inside field type resolution does not inject fresh type vars
                    // on handle types from this module.
                    let saved_local_type_defs = self.local_type_defs.clone();
                    let saved_source_type_defs = self.source_type_defs.clone();
                    for (item, _) in &module.items {
                        match item {
                            Item::TypeDecl(td) => {
                                self.local_type_defs.insert(td.name.to_string());
                                self.source_type_defs.insert(td.name.to_string());
                            }
                            Item::Machine(md) => {
                                // Pre-seed the machine name so that resolve_type_expr
                                // inside state/event field resolution sees the machine
                                // as locally-non-generic instead of injecting a fresh var.
                                // Also seed the synthesised `<Name>Event` companion so
                                // imported machines surface their event union as a
                                // locally-defined type for the non-root module body.
                                self.local_type_defs.insert(md.name.to_string());
                                self.source_type_defs.insert(md.name.to_string());
                                let event_type_name = format!("{}Event", md.name);
                                self.local_type_defs.insert(event_type_name.clone());
                                self.source_type_defs.insert(event_type_name);
                            }
                            _ => {}
                        }
                    }
                    let err_before = self.errors.len();
                    let warn_before = self.warnings.len();
                    // Per-file lexical type authority (rc1-F1 stage C):
                    // record which FILE declares each type name, so
                    // extern-signature nominal identity can resolve a bare
                    // name to its declaring file's minted identity.
                    let item_sources = self.module_item_sources.get(&module_name).cloned();
                    for (item_idx, (item, _)) in module.items.iter().enumerate() {
                        let declared = match item {
                            Item::TypeDecl(td) => Some(td.name),
                            _ => None,
                        };
                        if let (Some(name), Some(source)) = (
                            declared,
                            item_sources
                                .as_ref()
                                .and_then(|sources| sources.get(item_idx)),
                        ) {
                            self.file_type_decls
                                .entry(source.clone())
                                .or_default()
                                .insert(name.to_string());
                        }
                    }
                    for (item_idx, (item, item_span)) in module.items.iter().enumerate() {
                        self.current_module_idx = span_indices
                            .item_index(mod_id, item_idx)
                            .unwrap_or_default();
                        match item {
                            Item::TypeDecl(td) => {
                                self.pre_register_type_decl(td);
                            }
                            Item::TypeAlias(decl) => {
                                self.register_type_alias_decl(decl, item_span);
                            }
                            // Function-signature registration runs over
                            // module-graph bodies before the root import
                            // declarations are processed. Seed each source
                            // trait under its exact module owner here so a
                            // same-leaf declaration (`alpha.Render` /
                            // `beta.Render`) has a canonical identity when
                            // `register_trait_method_sig` mints its DefIds.
                            // Import processing later owns visibility and
                            // binding publication; this is declaration
                            // identity only.
                            Item::Trait(td) => {
                                let qualified = format!("{module_name}.{}", td.name);
                                self.trait_defs.entry(qualified).or_insert_with(|| {
                                    Self::trait_info_from_decl(
                                        td,
                                        Some(module_name.clone()),
                                        self.current_module_idx,
                                    )
                                });
                            }
                            // Register machine state/event binding tables for the
                            // non-root module path, mirroring the root-loop arm at
                            // line ~1029. Deliberately skips
                            // `register_machine_type_namespace_names` (which claims
                            // `type_def_spans`) because the import-surface path
                            // handles namespace dedup for exported names; claiming
                            // spans here would cause false duplicate-definition
                            // errors when the import path later registers the same
                            // machine. Idempotency guard matches `pre_register_type_decl`.
                            _ => {}
                        }
                    }
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
        self.current_module = None;
        self.current_module_idx = 0;

        // The root module follows the same source-order-independent rule as
        // imported modules: direct, canonical lifecycle import edges and every
        // root-owned nominal name must be visible before the first record/enum
        // member annotation is resolved. This is deliberately a narrow seed:
        // only graph-proven shipped lifecycle sources acquire ABI authority.
        if let Some(ref mg) = program.module_graph {
            if let Some(root) = mg.modules.get(&mg.root) {
                self.seed_resolved_lifecycle_import_bindings(root, None, mg);
            }
        }
        self.seed_type_registration_scope(&program.items);

        // Process root module items (full registration with namespace dedup).
        for (item, span) in &program.items {
            match item {
                Item::TypeDecl(td) => {
                    if !self.register_type_namespace_name(None, td.name.name.as_str(), span) {
                        continue;
                    }
                    self.register_type_decl(td);
                    self.local_type_defs.insert(td.name.to_string());
                    self.source_type_defs.insert(td.name.to_string());
                }
                Item::Actor(ad) => {
                    if !self.register_type_namespace_name(None, ad.name.name.as_str(), span) {
                        continue;
                    }
                    self.register_actor_decl(ad);
                    self.local_type_defs.insert(ad.name.to_string());
                    self.source_type_defs.insert(ad.name.to_string());
                }
                Item::TypeAlias(ta) => {
                    if !self.register_type_namespace_name(None, ta.name.name.as_str(), span) {
                        continue;
                    }
                    self.register_type_alias_decl(ta, span);
                    self.local_type_defs.insert(ta.name.to_string());
                    self.source_type_defs.insert(ta.name.to_string());
                }
                Item::Trait(td) => {
                    if !self.register_type_namespace_name(None, td.name.name.as_str(), span) {
                        continue;
                    }
                    let mut trait_errors = Vec::new();
                    let info = Self::trait_info_from_decl_with_diagnostics(
                        td,
                        self.current_module.clone(),
                        self.current_module_idx,
                        &mut trait_errors,
                    );
                    self.errors.extend(trait_errors);
                    self.trait_defs.insert(td.name.to_string(), info);
                    self.local_trait_defs.insert(td.name.to_string());
                    // Record super-trait relationships
                    if let Some(supers) = &td.super_traits {
                        let super_names: Vec<String> = supers
                            .iter()
                            .map(|s| {
                                self.mark_imported_trait_used(None, &s.path.to_string()); // TRANSITION(P1): deleted by A1 commit 2
                                s.path.to_string() // TRANSITION(P1): deleted by A1 commit 2
                            })
                            .collect();
                        self.trait_super
                            .insert(td.name.to_string(), super_names.clone());
                        if let Some(module) = self.current_module.as_deref() {
                            self.trait_super
                                .insert(format!("{module}.{}", td.name), super_names);
                        }
                    }
                    // Harvest `#[lang_item("…")]` attributes into the
                    // lang-item registry so downstream passes (HIR f-string
                    // lowering) can discover the trait/method names by role
                    // rather than by hard-coded surface symbols. Trait-level
                    // tags register with `method_name: None`; method-level
                    // tags carry the enclosing trait's name so HIR can build
                    // the `<SelfType>::<method>` impl symbol.
                    self.register_trait_lang_items(td, span.clone());
                }
                Item::Supervisor(sd) => {
                    self.reject_wasm_feature(span, WasmUnsupportedFeature::SupervisionTrees);
                    if !self.register_type_namespace_name(None, sd.name.name.as_str(), span) {
                        continue;
                    }
                    // Root items: `current_module` is cleared above, so the
                    // declaration identity is the bare name.
                    let identity = self.declaration_identity(sd.name.name.as_str());
                    self.register_supervisor_decl_as(sd, &identity);
                    self.local_type_defs.insert(identity.clone());
                    self.source_type_defs.insert(identity);
                }
                Item::Record(rd) => {
                    if !self.register_type_namespace_name(None, rd.name.name.as_str(), span) {
                        continue;
                    }
                    self.register_record_decl(rd);
                    self.local_type_defs.insert(rd.name.to_string());
                    self.source_type_defs.insert(rd.name.to_string());
                }
                // Machines are normalized into ordinary declarations before
                // registration runs.
                Item::Import(_)
                | Item::Const(_)
                | Item::Impl(_)
                | Item::Machine(_)
                | Item::Function(_)
                | Item::ExternBlock(_) => {}
            }
        }
    }

    /// Register the target under the declaration's canonical identity. Imports
    /// publish bindings separately and never create another alias definition.
    pub(super) fn register_type_alias_decl(
        &mut self,
        decl: &hew_parser::ast::TypeAliasDecl,
        span: &Span,
    ) {
        let path = scoped_module_item_name(self.current_module.as_deref(), decl.name.name.as_str())
            .unwrap_or_else(|| decl.name.to_string());
        let Some(declaration) = self.require_declaration_path(&path, span) else {
            return;
        };
        let identity = self.defs.path(declaration).to_string();
        self.known_types.insert(identity.clone());
        self.type_visibility.insert(
            identity.clone(),
            (decl.visibility, self.current_module.clone()),
        );
        self.type_def_spans
            .entry(identity.clone())
            .or_insert_with(|| span.clone());
        let type_params: Vec<String> = decl
            .type_params
            .iter()
            .flatten()
            .map(|param| param.name.to_string())
            .collect();
        self.generic_ctx.push(
            type_params
                .iter()
                .map(|param| (param.clone(), Ty::param(param)))
                .collect(),
        );
        let mut holes = Vec::new();
        let target = self.resolve_type_expr_tracking_holes(&decl.ty, &mut holes);
        self.generic_ctx.pop();
        self.type_aliases.insert(
            identity.clone(),
            TypeAliasDef {
                declaration,
                type_params,
                target,
                source_module: self.current_module.clone(),
                file_index: self.current_module_idx,
            },
        );
        self.record_type_def_inference_holes(&identity, holes);
    }

    pub(in crate::check) fn resolved_type_aliases(
        &mut self,
    ) -> HashMap<crate::DefId, TypeAliasDef> {
        let previous_module = self.current_module.clone();
        let previous_index = self.current_module_idx;
        let aliases = self.type_aliases.values().cloned().collect::<Vec<_>>();
        let mut resolved = HashMap::new();
        for mut alias in aliases {
            self.current_module.clone_from(&alias.source_module);
            self.current_module_idx = alias.file_index;
            alias.target = self
                .normalize_for_type_params(&alias.target, &alias.type_params)
                .materialize_literal_defaults();
            resolved.insert(alias.declaration, alias);
        }
        self.current_module = previous_module;
        self.current_module_idx = previous_index;
        resolved
    }

    /// Seed root-owned names before resolving any root declaration members.
    ///
    /// Besides making declarations source-order independent, this is the
    /// local-shadow boundary for source-owned lifecycle spellings: a local
    /// `CrashNotification` remains an ordinary user type even when a canonical
    /// std import with the same leaf name is present.
    pub(super) fn seed_type_registration_scope(&mut self, items: &[Spanned<Item>]) {
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
                _ => {}
            }
        }
    }

    /// Admit `optional` wire fields only when their final semantic type is
    /// `Option<T>`.
    ///
    /// This deliberately runs after [`Self::reresolve_member_types_after_imports`].
    /// The parser cannot make this decision from syntax without rejecting a
    /// valid alias, and the first registration pass predates import aliases.
    /// Keeping this as the one checker-side gate means every later consumer
    /// sees either an admitted `Option<T>` field or a hard type error.
    pub(in crate::check) fn validate_wire_optional_field_admission(&mut self, program: &Program) {
        if let Some(module_graph) = &program.module_graph {
            for module_id in &module_graph.topo_order {
                if *module_id == module_graph.root {
                    continue;
                }
                let Some(module) = module_graph.modules.get(module_id) else {
                    continue;
                };
                self.current_module = Some(module_id.dotted());
                for (item, _) in &module.items {
                    self.validate_type_decl_wire_optional_fields(item);
                }
            }
        }

        self.current_module = None;
        for (item, _) in &program.items {
            self.validate_type_decl_wire_optional_fields(item);
        }
        self.current_module_idx = 0;
    }

    /// Every member of a `#[wire]` declaration must itself have a wire
    /// encoding; the resolved member types come from its checked definition
    /// and the spans from the source declaration.
    pub(super) fn validate_wire_type_members(&mut self, type_decl: &TypeDecl, type_def: &TypeDef) {
        let identity = self.current_module_identity().map_or_else(
            || type_decl.name.to_string(),
            |module| format!("{module}.{}", type_decl.name),
        );
        let mut members = Vec::new();
        for item in &type_decl.body {
            match item {
                TypeBodyItem::Field { name, ty, .. } => {
                    if let Some(field_ty) = type_def.fields.get(name.name.as_str()) {
                        members.push((format!("field `{name}`"), field_ty.clone(), ty.1.clone()));
                    }
                }
                TypeBodyItem::Variant(variant) => {
                    let payload: Vec<(String, Ty, Span)> = match (
                        &variant.kind,
                        type_def.variants.get(variant.name.name.as_str()),
                    ) {
                        (VariantKind::Tuple(spans), Some(VariantDef::Tuple(tys))) => spans
                            .iter()
                            .zip(tys)
                            .enumerate()
                            .map(|(index, (span, ty))| {
                                (
                                    format!("variant `{}` payload {index}", variant.name),
                                    ty.clone(),
                                    span.1.clone(),
                                )
                            })
                            .collect(),
                        (VariantKind::Struct(spans), Some(VariantDef::Struct(fields))) => spans
                            .iter()
                            .filter_map(|(name, span)| {
                                let (_, ty) = fields
                                    .iter()
                                    .find(|(field, _)| field == name.name.as_str())?;
                                Some((
                                    format!("variant `{}` field `{name}`", variant.name),
                                    ty.clone(),
                                    span.1.clone(),
                                ))
                            })
                            .collect(),
                        _ => Vec::new(),
                    };
                    members.extend(payload);
                }
                TypeBodyItem::Method(_) => {}
            }
        }
        self.validate_wire_type_encoding(identity.as_str(), members);
    }

    /// Seed `local_type_defs`/`source_type_defs` with the current scope's own
    /// type names so member re-resolution (a) treats them as locally-defined
    /// (no fresh-var injection) and (b) shadows any same-named import alias —
    /// the local-shadow rule. Mirrors the seeding `collect_types` performs.
    pub(super) fn seed_member_reresolution_scope(&mut self, items: &[Spanned<Item>]) {
        for (item, _) in items {
            match item {
                Item::TypeDecl(td) => {
                    self.local_type_defs.insert(td.name.to_string());
                    self.source_type_defs.insert(td.name.to_string());
                }
                Item::Record(rd) => {
                    self.local_type_defs.insert(rd.name.to_string());
                    self.source_type_defs.insert(rd.name.to_string());
                }
                Item::Actor(ad) => {
                    self.source_type_defs.insert(ad.name.to_string());
                }
                Item::TypeAlias(ta) => {
                    self.local_type_defs.insert(ta.name.to_string());
                    self.source_type_defs.insert(ta.name.to_string());
                }
                _ => {}
            }
        }
    }

    pub(super) fn reresolve_item_member_types(&mut self, item: &Item) {
        match item {
            Item::TypeDecl(td) => self.reresolve_type_decl_members(td),
            Item::Record(rd) => self.reresolve_record_members(rd),
            Item::Machine(md) => self.reresolve_machine_members(md),
            Item::Actor(ad) => self.reresolve_actor_members(ad),
            _ => {}
        }
    }

    /// Re-resolve an actor's state fields and init parameters.
    ///
    /// An actor is the type of its handle (D489), and the handle carrier is
    /// stamped by [`Self::canonicalize_actor_handles`] from the declarations
    /// registered so far. `actor Alpha { let beta: Beta }` with `Beta`
    /// declared below it, or in a module `collect_functions` registers later,
    /// therefore froze `beta` as a bare nominal while `Beta`'s own back
    /// reference carried the discriminator. Declaration order is not a
    /// semantic fact, so this pass re-reads the same authority once every
    /// actor, supervisor and module is registered.
    pub(super) fn reresolve_actor_members(&mut self, ad: &ActorDecl) {
        let has_type_params = !ad.type_params.is_empty();
        if has_type_params {
            let bounds = self.collect_type_param_bounds(Some(&ad.type_params), None);
            self.current_type_param_bounds
                .push(TypeParamScope::new(bounds, HashMap::new()));
        }
        let mut hole_vars = Vec::new();
        let mut fields: HashMap<String, Ty> = HashMap::new();
        for field in &ad.fields {
            let field_ty = self.resolve_registered_annotation_ty(&field.ty, &mut hole_vars);
            fields.insert(field.name.to_string(), field_ty);
        }
        let init_params: Vec<ActorInitParamInfo> = ad.init.as_ref().map_or_else(Vec::new, |init| {
            init.params
                .iter()
                .map(|p| ActorInitParamInfo {
                    name: p.name.to_string(),
                    ty: self.resolve_registered_annotation_ty(&p.ty, &mut hole_vars),
                })
                .collect()
        });
        if has_type_params {
            self.current_type_param_bounds.pop();
        }

        let identity = self.authoritative_type_def_key(ad.name.name.as_str());
        let mut changed = false;
        if let Some(stored) = self.type_defs.get_mut(&identity) {
            if stored.kind == TypeDefKind::Actor && stored.fields != fields {
                stored.fields = fields;
                changed = true;
            }
        }
        if let Some(stored) = self.actor_init_params.get_mut(&identity) {
            if *stored != init_params {
                *stored = init_params;
                changed = true;
            }
        }
        if changed {
            self.handle_bearing_dirty = true;
        }
    }

    /// The collision-free key under which this scope's `TypeDef` is stored: the
    /// module-qualified `{module_short}.{name}` for a non-root module (when it
    /// exists), else the bare `name` for the root program.
    pub(super) fn authoritative_type_def_key(&self, bare_name: &str) -> String {
        if let Some(module_owner) = self.current_module_identity() {
            let qualified = format!("{module_owner}.{bare_name}");
            if self.type_defs.contains_key(&qualified) {
                return qualified;
            }
        }
        bare_name.to_string()
    }

    /// Commit a re-resolved `TypeDef` under its declaration identity.
    /// Non-root declarations publish only their full owner; root declarations
    /// retain the bare key because that is their canonical identity.
    pub(super) fn commit_reresolved_type_def(&mut self, name: &str, type_def: TypeDef) {
        if let Some(module_owner) = self.current_module_identity().map(str::to_string) {
            self.register_canonical_type_def(&module_owner, name, &type_def);
        } else {
            self.type_defs.insert(name.to_string(), type_def);
        }
        self.handle_bearing_dirty = true;
    }

    /// Re-resolve a `type`/`enum` declaration's member types under the now-live
    /// import-alias maps; on a member upgrade, patch `type_defs` and re-run every
    /// member-derived fact. Mirrors `register_type_decl`'s member resolution and
    /// derivation tail. No-op when no member changed.
    pub(super) fn reresolve_type_decl_members(&mut self, td: &TypeDecl) {
        let scope =
            self.enter_primary_sig_scope(&[(td.type_params.as_ref(), td.where_clause.as_ref())]);
        self.reresolve_type_decl_members_in_scope(td);
        self.exit_primary_sig_scope(scope);
    }

    pub(super) fn reresolve_type_decl_members_in_scope(&mut self, td: &TypeDecl) {
        let kind = match td.kind {
            TypeDeclKind::Struct => TypeDefKind::Struct,
            TypeDeclKind::Enum => TypeDefKind::Enum,
        };
        let type_param_names: Vec<String> = td.type_params.as_ref().map_or(vec![], |params| {
            params.iter().map(|p| p.name.to_string()).collect()
        });

        let mut fields = HashMap::new();
        let mut field_order: Vec<String> = Vec::new();
        let mut variants = HashMap::new();
        let mut hole_vars = Vec::new();
        for item in &td.body {
            match item {
                TypeBodyItem::Field { name, ty, .. } => {
                    let field_ty = self.resolve_registered_annotation_ty(ty, &mut hole_vars);
                    field_order.push(name.to_string());
                    fields.insert(name.to_string(), field_ty);
                }
                TypeBodyItem::Variant(variant) => match &variant.kind {
                    VariantKind::Unit => {
                        variants.insert(variant.name.to_string(), VariantDef::Unit);
                    }
                    VariantKind::Tuple(tuple_fields) => {
                        let variant_tys: Vec<Ty> = tuple_fields
                            .iter()
                            .map(|f| self.resolve_registered_annotation_ty(f, &mut hole_vars))
                            .collect();
                        variants.insert(variant.name.to_string(), VariantDef::Tuple(variant_tys));
                    }
                    VariantKind::Struct(struct_fields) => {
                        let variant_fields: Vec<(String, Ty)> = struct_fields
                            .iter()
                            .map(|(n, f)| {
                                (
                                    n.to_string(),
                                    self.resolve_registered_annotation_ty(f, &mut hole_vars),
                                )
                            })
                            .collect();
                        variants
                            .insert(variant.name.to_string(), VariantDef::Struct(variant_fields));
                    }
                },
                TypeBodyItem::Method(_) => {}
            }
        }

        let stored_key = self.authoritative_type_def_key(td.name.name.as_str());
        let Some(stored) = self.type_defs.get(&stored_key) else {
            return;
        };
        if stored.fields == fields && stored.variants == variants {
            return;
        }

        let type_def = TypeDef {
            kind,
            name: td.name.to_string(),
            type_params: type_param_names.clone(),
            bounds: stored.bounds.clone(),
            fields,
            field_order,
            variants,
            methods: stored.methods.clone(),
            doc_comment: td.doc_comment.clone(),
            is_indirect: td.is_indirect,
        };

        // Re-key tuple variant constructors over the canonical payload types.
        // Unit/struct variants carry no member-dependent constructor signature.
        for (variant_name, variant_def) in &type_def.variants {
            if let VariantDef::Tuple(variant_tys) = variant_def {
                if let Some(sig) = self.fn_sigs.get_mut(variant_name) {
                    sig.params.clone_from(variant_tys);
                }
            }
        }

        // Re-derive member-dependent facts (all replace-semantics).
        let field_types: Vec<Ty> = if kind == TypeDefKind::Enum {
            Self::structural_member_types_for_type(&type_def)
        } else {
            type_def.fields.values().cloned().collect()
        };
        let field_types = self.expand_for_marker_registration(&field_types);
        self.registry
            .register_type(td.name.to_string(), field_types);
        self.seed_qualified_type_markers_for_current_module(td.name.name.as_str());
        self.commit_reresolved_type_def(td.name.name.as_str(), type_def);

        if let Some(ref wire) = td.wire {
            let variant_order: Vec<String> = td
                .body
                .iter()
                .filter_map(|i| match i {
                    TypeBodyItem::Variant(v) => Some(v.name.to_string()),
                    _ => None,
                })
                .collect();
            self.register_wire_methods(td.name.name.as_str(), wire, &variant_order);
        }
    }

    /// Re-resolve a `record` declaration's member types. Mirrors
    /// `register_record_decl`'s named/tuple split and derivation tail. No-op when
    /// no member changed. Positional constructors use the same canonical declaration key.
    pub(super) fn reresolve_record_members(&mut self, rd: &RecordDecl) {
        let scope =
            self.enter_primary_sig_scope(&[(rd.type_params.as_ref(), rd.where_clause.as_ref())]);
        self.reresolve_record_members_in_scope(rd);
        self.exit_primary_sig_scope(scope);
    }

    pub(super) fn reresolve_record_members_in_scope(&mut self, rd: &RecordDecl) {
        let type_param_names: Vec<String> = rd.type_params.as_ref().map_or(vec![], |params| {
            params.iter().map(|p| p.name.to_string()).collect()
        });
        let mut hole_vars = Vec::new();

        match &rd.kind {
            RecordKind::Named(record_fields) => {
                let mut fields: HashMap<String, Ty> = HashMap::new();
                let mut field_order: Vec<String> = Vec::new();
                for rf in record_fields {
                    let field_ty = self.resolve_registered_annotation_ty(&rf.ty, &mut hole_vars);
                    field_order.push(rf.name.to_string());
                    fields.insert(rf.name.to_string(), field_ty);
                }

                let stored_key = self.authoritative_type_def_key(rd.name.name.as_str());
                let Some(stored) = self.type_defs.get(&stored_key) else {
                    return;
                };
                if stored.fields == fields {
                    return;
                }

                let type_def = TypeDef {
                    kind: TypeDefKind::Record,
                    name: rd.name.to_string(),
                    type_params: type_param_names,
                    bounds: stored.bounds.clone(),
                    fields,
                    field_order,
                    variants: HashMap::new(),
                    methods: stored.methods.clone(),
                    doc_comment: rd.doc_comment.clone(),
                    is_indirect: false,
                };
                let field_types: Vec<Ty> = type_def.fields.values().cloned().collect();
                let field_types = self.expand_for_marker_registration(&field_types);
                self.registry.register_type(stored_key, field_types);
                self.commit_reresolved_type_def(rd.name.name.as_str(), type_def);
            }
            RecordKind::Tuple(positional_types) => {
                let param_tys: Vec<Ty> = positional_types
                    .iter()
                    .map(|te| self.resolve_registered_annotation_ty(te, &mut hole_vars))
                    .collect();
                // Tuple records store no fields (`.0`/`.1` access is forbidden);
                // the positional types live only in the constructor `fn_sig`.
                let canonical = self.authoritative_type_def_key(rd.name.name.as_str());
                let mut changed = false;
                if let Some(sig) = self.fn_sigs.get_mut(&canonical) {
                    if sig.params != param_tys {
                        sig.params.clone_from(&param_tys);
                        changed = true;
                    }
                }
                if !changed {
                    return;
                }
                let expanded_param_tys = self.expand_for_marker_registration(&param_tys);
                self.registry.register_type(canonical, expanded_param_tys);
                self.handle_bearing_dirty = true;
            }
        }
    }

    /// Re-resolve a `machine` declaration's state and event field types. Mirrors
    /// `register_machine_decl`'s state-variant / event-companion resolution and
    /// marker derivation. State and event companions are patched independently.
    pub(super) fn reresolve_machine_members(&mut self, md: &MachineDecl) {
        let scope =
            self.enter_primary_sig_scope(&[(Some(&md.type_params), md.where_clause.as_ref())]);
        self.reresolve_machine_members_in_scope(md);
        self.exit_primary_sig_scope(scope);
    }

    pub(super) fn reresolve_machine_members_in_scope(&mut self, md: &MachineDecl) {
        // --- State fields → machine `type_def` variants ---
        let mut variants = HashMap::new();
        let mut machine_hole_vars = Vec::new();
        for state in &md.states {
            if state.fields.is_empty() {
                variants.insert(state.name.to_string(), VariantDef::Unit);
            } else {
                let variant_fields: Vec<(String, Ty)> = state
                    .fields
                    .iter()
                    .map(|(name, spanned_te)| {
                        (
                            name.to_string(),
                            self.resolve_registered_annotation_ty(
                                spanned_te,
                                &mut machine_hole_vars,
                            ),
                        )
                    })
                    .collect();
                variants.insert(state.name.to_string(), VariantDef::Struct(variant_fields));
            }
        }

        let machine_key = self.authoritative_type_def_key(md.name.name.as_str());
        if let Some(stored) = self.type_defs.get(&machine_key) {
            if stored.variants != variants {
                let type_def = TypeDef {
                    kind: TypeDefKind::Machine,
                    name: md.name.to_string(),
                    type_params: stored.type_params.clone(),
                    bounds: stored.bounds.clone(),
                    fields: HashMap::new(),
                    field_order: vec![],
                    variants,
                    methods: stored.methods.clone(),
                    doc_comment: stored.doc_comment.clone(),
                    is_indirect: stored.is_indirect,
                };
                // Register field types for Send/Frozen derivation (mirrors the
                // `resolve_type_expr` flatten at the registration site).
                let mut all_field_types = Vec::new();
                for state in &md.states {
                    for (_, spanned_te) in &state.fields {
                        all_field_types.push(self.resolve_type_expr(spanned_te));
                    }
                }
                let all_field_types = self.expand_for_marker_registration(&all_field_types);
                self.registry
                    .register_type(md.name.to_string(), all_field_types);
                self.commit_reresolved_type_def(md.name.name.as_str(), type_def);
            }
        }

        // --- Event fields → `{Name}Event` companion enum ---
        let event_type_name = format!("{}Event", md.name);
        let mut event_variants = HashMap::new();
        let mut event_hole_vars = Vec::new();
        for event in &md.events {
            if event.fields.is_empty() {
                event_variants.insert(event.name.to_string(), VariantDef::Unit);
            } else {
                let variant_fields: Vec<(String, Ty)> = event
                    .fields
                    .iter()
                    .map(|(name, spanned_te)| {
                        (
                            name.to_string(),
                            self.resolve_registered_annotation_ty(spanned_te, &mut event_hole_vars),
                        )
                    })
                    .collect();
                event_variants.insert(event.name.to_string(), VariantDef::Struct(variant_fields));
            }
        }
        let event_key = self.authoritative_type_def_key(&event_type_name);
        if let Some(stored) = self.type_defs.get(&event_key) {
            if stored.variants != event_variants {
                let event_type_def = TypeDef {
                    kind: TypeDefKind::Enum,
                    name: event_type_name.clone(),
                    type_params: stored.type_params.clone(),
                    bounds: stored.bounds.clone(),
                    fields: HashMap::new(),
                    field_order: vec![],
                    variants: event_variants,
                    methods: stored.methods.clone(),
                    doc_comment: stored.doc_comment.clone(),
                    is_indirect: stored.is_indirect,
                };
                // Mirror `register_machine_decl`'s marker registration: the
                // re-resolved companion enum's field types must be
                // re-registered too, or a re-resolution that changes a
                // payload's Send-ability (e.g. an import alias resolving to
                // a resource type) is invisible to `TraitRegistry`.
                let event_field_types = Self::structural_member_types_for_type(&event_type_def);
                let event_field_types = self.expand_for_marker_registration(&event_field_types);
                self.registry
                    .register_type(event_type_name.clone(), event_field_types);
                self.registry.register_type_params(
                    event_type_name.clone(),
                    event_type_def.type_params.clone(),
                );
                self.commit_reresolved_type_def(&event_type_name, event_type_def);
            }
        }
    }

    /// Populate `type_defs` with a full `TypeDef` for a non-root module's
    /// `TypeDecl`, including resolved fields and variant constructors.
    ///
    /// Deliberately skips:
    ///   - `type_def_spans` — the import path handles namespace dedup
    ///   - `TraitRegistry` registration — the import path (or C module
    ///     registry) handles trait derivation for exported types
    ///   - Wire-method registration — only relevant for the import surface
    ///
    /// It still registers enum-constructor `fn_sigs` so non-root module body
    /// checking can construct local values. The import path's later
    /// `register_type_decl` call overwrites those signatures for `pub` types
    /// with the fully side-effected version.
    pub(super) fn pre_register_type_decl(&mut self, td: &TypeDecl) {
        let scope =
            self.enter_primary_sig_scope(&[(td.type_params.as_ref(), td.where_clause.as_ref())]);
        self.pre_register_type_decl_in_scope(td);
        self.exit_primary_sig_scope(scope);
    }

    #[expect(clippy::too_many_lines, reason = "type resolution requires many cases")]
    pub(super) fn pre_register_type_decl_in_scope(&mut self, td: &TypeDecl) {
        // Idempotency guard, keyed per-module. Two non-root modules that each
        // declare a type of the same bare name (`badpkg.Reply` and
        // `goodpkg.Reply`) must BOTH register: the bare `type_defs` entry is
        // last-write-wins across modules (the qualified alias is the authority),
        // but each module's qualified marker set must be seeded so the ask-reply
        // Send gate derives `Send` from the correct module's fields. Keying the
        // guard on the bare name skipped the second module's `Reply` entirely,
        // leaving the gate to read whichever module won the bare-key race.
        let guard_key = self
            .current_module_identity()
            .map_or_else(|| td.name.to_string(), |m| format!("{m}.{}", td.name));
        if self.type_defs.contains_key(guard_key.as_str()) {
            return;
        }
        // #1295: record `#[resource]` types from pre-registered (imported)
        // modules too, so an imported handle type's inherent `close(self)`
        // consumes its receiver at the call site (mirrors `register_type_decl`).
        if td.resource_marker == hew_parser::ast::ResourceMarker::Resource {
            self.registry.register_resource_type(guard_key.clone());
        }
        if td.resource_marker == hew_parser::ast::ResourceMarker::Linear {
            self.registry.register_linear_type(guard_key.clone());
        }
        if td.is_opaque {
            self.user_opaque_type_names.insert(guard_key.clone());
        }
        let kind = match td.kind {
            TypeDeclKind::Struct => TypeDefKind::Struct,
            TypeDeclKind::Enum => TypeDefKind::Enum,
        };
        let type_param_names: Vec<String> = td.type_params.as_ref().map_or(vec![], |params| {
            params.iter().map(|p| p.name.to_string()).collect()
        });
        let type_param_bounds =
            self.collect_type_param_bounds(td.type_params.as_ref(), td.where_clause.as_ref());

        // Reject duplicate type parameter names — same check as `register_type_decl`.
        {
            let mut seen: std::collections::HashSet<&str> = std::collections::HashSet::new();
            for name in &type_param_names {
                if !seen.insert(name.as_str()) {
                    self.errors.push(TypeError::new(
                        TypeErrorKind::DuplicateDefinition,
                        0..0,
                        format!(
                            "type parameter `{name}` is defined more than once in `{}`",
                            td.name
                        ),
                    ));
                }
            }
        }

        let mut fields = HashMap::new();
        let mut field_order: Vec<String> = Vec::new();
        let mut variants = HashMap::new();
        let mut hole_vars = Vec::new();
        let enum_return_args: Vec<Ty> = type_param_names
            .iter()
            .map(|name| Ty::param(name))
            .collect();

        for item in &td.body {
            match item {
                TypeBodyItem::Field { name, ty, .. } => {
                    let field_ty = self.resolve_registered_annotation_ty(ty, &mut hole_vars);
                    field_order.push(name.to_string());
                    fields.insert(name.to_string(), field_ty);
                }
                TypeBodyItem::Variant(variant) => {
                    let declaration_name = self.current_module.as_ref().map_or_else(
                        || td.name.to_string(),
                        |module| format!("{module}.{}", td.name),
                    );
                    let return_type =
                        self.variant_nominal_ty(&declaration_name, enum_return_args.clone());
                    match &variant.kind {
                        VariantKind::Unit => {
                            variants.insert(variant.name.to_string(), VariantDef::Unit);
                            // Register variant constructor so body-checking can construct values
                            self.fn_sigs.insert(
                                variant.name.to_string(),
                                FnSig {
                                    type_params: type_param_names.clone(),
                                    type_param_bounds: type_param_bounds.clone(),
                                    return_type,
                                    is_builtin_variant: self.in_stdlib_registration,
                                    ..FnSig::default()
                                },
                            );
                        }
                        VariantKind::Tuple(tfields) => {
                            let variant_tys: Vec<Ty> = tfields
                                .iter()
                                .map(|field| {
                                    self.resolve_registered_annotation_ty(field, &mut hole_vars)
                                })
                                .collect();
                            variants.insert(
                                variant.name.to_string(),
                                VariantDef::Tuple(variant_tys.clone()),
                            );
                            self.fn_sigs.insert(
                                variant.name.to_string(),
                                FnSig {
                                    type_params: type_param_names.clone(),
                                    type_param_bounds: type_param_bounds.clone(),
                                    params: variant_tys,
                                    return_type,
                                    is_builtin_variant: self.in_stdlib_registration,
                                    ..FnSig::default()
                                },
                            );
                        }
                        VariantKind::Struct(sfields) => {
                            let variant_fields: Vec<(String, Ty)> = sfields
                                .iter()
                                .map(|(name, field)| {
                                    (
                                        name.to_string(),
                                        self.resolve_registered_annotation_ty(
                                            field,
                                            &mut hole_vars,
                                        ),
                                    )
                                })
                                .collect();
                            variants.insert(
                                variant.name.to_string(),
                                VariantDef::Struct(variant_fields),
                            );
                        }
                    }
                }
                TypeBodyItem::Method(_) => {}
            }
        }

        let type_def = TypeDef {
            kind,
            name: td.name.to_string(),
            type_params: type_param_names,
            bounds: type_param_bounds,
            fields,
            field_order,
            variants,
            methods: HashMap::new(),
            doc_comment: td.doc_comment.clone(),
            is_indirect: td.is_indirect,
        };

        // Seed the trait-registry structural member set for imported module
        // types, mirroring `register_type_decl`. An imported actor whose `ask`
        // replies with one of these types (e.g. a `pub type Result { ... }`)
        // is gated on the reply being `Send` at the dispatch site
        // (`record_actor_method_dispatch`, `E_DUPLEX_NON_SEND`). Send and the
        // sibling structural markers derive from a named type's member set; if
        // the importer's registry has no `type_fields` entry the derivation
        // hits the "unknown type — conservatively fail" branch and rejects a
        // plainly-Send imported record. Seeding it here resolves the marker
        // through the imported record's actual fields. Enums register their
        // variant-payload member set (an empty `fields` map would derive a
        // spurious Copy/Frozen); the `Serializable` subset follows the same
        // wire/enum condition as the full registration path.
        let field_types: Vec<_> = if kind == TypeDefKind::Enum {
            Self::structural_member_types_for_type(&type_def)
        } else {
            type_def.fields.values().cloned().collect()
        };
        let field_types = self.expand_for_marker_registration(&field_types);
        self.registry
            .register_type(td.name.to_string(), field_types);
        self.registry
            .register_type_params(td.name.to_string(), type_def.type_params.clone());
        // Mirror the markers under the module-qualified key so a same-bare-name
        // reply from another package cannot clobber this type's Send derivation
        // at the ask-reply gate.
        self.seed_qualified_type_markers_for_current_module(td.name.name.as_str());

        // Keep the bare row as registration-local assembly state. A non-root
        // declaration is published through the canonical constructor so every
        // durable named-family insertion uses the same full-owner key path.
        if let Some(module_owner) = self.current_module_identity().map(str::to_string) {
            self.register_canonical_type_def(&module_owner, td.name.name.as_str(), &type_def);
        }
        self.type_defs.insert(td.name.to_string(), type_def);
        self.record_type_def_inference_holes(td.name.name.as_str(), hole_vars);
        self.handle_bearing_dirty = true;
    }

    /// Reserve a type-name in the given module's namespace and reject a second
    /// declaration of the same name *within the same module*.
    ///
    /// `module_owner` is the defining module (`None` for the root program and
    /// flat file imports, which share one namespace). Two distinct modules may
    /// each declare a type
    /// of the same bare name — the durable cross-module identity is the qualified
    /// `{module}.{name}` key inserted by `register_canonical_type_def`. The bare
    /// `type_def_spans` entry is still populated for the span-lookup consumers
    /// (cycle / actor-ref diagnostics); it is last-write-wins across modules and
    /// is no longer the uniqueness authority.
    pub(in crate::check) fn register_type_namespace_name(
        &mut self,
        module_owner: Option<&str>,
        name: &str,
        span: &Span,
    ) -> bool {
        if self.reject_protected_prelude_declaration_for_owner(module_owner, name, span) {
            return false;
        }
        if crate::ty::is_reserved_type_name(name) {
            self.errors
                .push(TypeError::reserved_type_name(span.clone(), name));
            return false;
        }
        let owner_key = (module_owner.map(str::to_string), name.to_string());
        if let Some(prev_span) = self.type_namespace_owners.get(&owner_key).cloned() {
            self.report_duplicate_type_namespace_name(name, span, prev_span);
            return false;
        }

        self.type_namespace_owners.insert(owner_key, span.clone());
        self.type_def_spans
            .entry(name.to_string())
            .or_insert_with(|| span.clone());
        true
    }

    pub(in crate::check) fn report_duplicate_type_namespace_name(
        &mut self,
        name: &str,
        span: &Span,
        prev_span: Span,
    ) {
        self.errors.push(TypeError::duplicate_definition(
            span.clone(),
            name,
            prev_span,
        ));
    }

    pub(in crate::check) fn register_machine_type_namespace_names(
        &mut self,
        module_owner: Option<&str>,
        machine_name: &str,
        span: &Span,
    ) -> bool {
        if self.reject_protected_prelude_declaration_for_owner(module_owner, machine_name, span)
            || self.reject_protected_prelude_declaration_for_owner(
                module_owner,
                &format!("{machine_name}Event"),
                span,
            )
        {
            return false;
        }
        if crate::ty::is_reserved_type_name(machine_name) {
            self.errors
                .push(TypeError::reserved_type_name(span.clone(), machine_name));
            return false;
        }
        let machine_key = (module_owner.map(str::to_string), machine_name.to_string());
        if let Some(prev_span) = self.type_namespace_owners.get(&machine_key).cloned() {
            self.report_duplicate_type_namespace_name(machine_name, span, prev_span);
            return false;
        }

        let event_type_name = format!("{machine_name}Event");
        let event_key = (module_owner.map(str::to_string), event_type_name.clone());
        if let Some(prev_span) = self.type_namespace_owners.get(&event_key).cloned() {
            self.report_duplicate_type_namespace_name(&event_type_name, span, prev_span);
            return false;
        }

        self.type_namespace_owners.insert(machine_key, span.clone());
        self.type_namespace_owners.insert(event_key, span.clone());
        self.type_def_spans
            .entry(machine_name.to_string())
            .or_insert_with(|| span.clone());
        self.type_def_spans
            .entry(event_type_name)
            .or_insert_with(|| span.clone());
        true
    }

    pub(in crate::check) fn register_type_decl(&mut self, td: &TypeDecl) {
        let scope =
            self.enter_primary_sig_scope(&[(td.type_params.as_ref(), td.where_clause.as_ref())]);
        self.register_type_decl_in_scope(td);
        self.exit_primary_sig_scope(scope);
    }

    #[expect(clippy::too_many_lines, reason = "type resolution requires many cases")]
    pub(super) fn register_type_decl_in_scope(&mut self, td: &TypeDecl) {
        if td.origin == hew_parser::ast::DeclarationOrigin::MachineReport {
            let qualified = self
                .current_declaration_module()
                .map(|module| format!("{}.{}", self.defs.module_path(module), td.name));
            if let Some(declaration) = qualified
                .as_deref()
                .and_then(|name| self.lookup_declaration(name))
                .or_else(|| self.lookup_declaration(td.name.name.as_str()))
            {
                self.must_use_types.insert(declaration);
            }
        }
        // #1295: record `#[resource]` types so their inherent `close(self)`
        // dispatch can mark the receiver moved + consume it (suppressing the
        // duplicate scope-exit implicit drop). HIR owns the close-discipline
        // diagnostics (W3.030); the checker only needs the marker fact here.
        if td.resource_marker == hew_parser::ast::ResourceMarker::Resource {
            let canonical_name = self.current_module_identity().map_or_else(
                || td.name.to_string(),
                |module| format!("{module}.{}", td.name),
            );
            self.registry.register_resource_type(canonical_name.clone());
        }
        if td.resource_marker == hew_parser::ast::ResourceMarker::Linear {
            let canonical_name = self.current_module_identity().map_or_else(
                || td.name.to_string(),
                |module| format!("{module}.{}", td.name),
            );
            self.registry.register_linear_type(canonical_name.clone());
        }
        // Track user-declared `#[opaque]` types so `record_clone_admissibility`
        // can detect opaque fields transitively. The module_registry only
        // carries opaque types imported via `use module::*`; user-declared
        // opaques in the same file are NOT registered there.
        if td.is_opaque {
            let canonical_name = self.current_module_identity().map_or_else(
                || td.name.to_string(),
                |module| format!("{module}.{}", td.name),
            );
            // Imported declarations keep their exact owner. Publishing their
            // bare spelling would mark an unrelated root type with the same
            // name opaque when declaration facts are collected.
            self.user_opaque_type_names.insert(canonical_name.clone());
        }

        let kind = match td.kind {
            TypeDeclKind::Struct => TypeDefKind::Struct,
            TypeDeclKind::Enum => TypeDefKind::Enum,
        };

        let mut fields = HashMap::new();
        let mut field_order: Vec<String> = Vec::new();
        let mut variants = HashMap::new();
        let mut variant_order = Vec::new();
        let mut hole_vars = Vec::new();
        let type_param_names: Vec<String> = td.type_params.as_ref().map_or(vec![], |params| {
            params.iter().map(|p| p.name.to_string()).collect()
        });

        // Reject duplicate type parameter names within the same declaration.
        // The parser cannot catch this because `parse_type_params` has no
        // seen-name accumulator; the checker is the authoritative gatekeeper.
        {
            let mut seen: std::collections::HashSet<&str> = std::collections::HashSet::new();
            for name in &type_param_names {
                if !seen.insert(name.as_str()) {
                    self.errors.push(TypeError::new(
                        TypeErrorKind::DuplicateDefinition,
                        0..0,
                        format!(
                            "type parameter `{name}` is defined more than once in `{}`",
                            td.name
                        ),
                    ));
                }
            }
        }

        let type_param_bounds =
            self.collect_type_param_bounds(td.type_params.as_ref(), td.where_clause.as_ref());
        let enum_return_args: Vec<Ty> = type_param_names
            .iter()
            .map(|name| Ty::param(name))
            .collect();

        for item in &td.body {
            match item {
                TypeBodyItem::Field { name, ty, .. } => {
                    let field_ty = self.resolve_registered_annotation_ty(ty, &mut hole_vars);
                    field_order.push(name.to_string());
                    fields.insert(name.to_string(), field_ty);
                }
                TypeBodyItem::Variant(variant) => {
                    variant_order.push(variant.name.to_string());
                    let declaration_name = self.current_module.as_ref().map_or_else(
                        || td.name.to_string(),
                        |module| format!("{module}.{}", td.name),
                    );
                    let return_type =
                        self.variant_nominal_ty(&declaration_name, enum_return_args.clone());
                    match &variant.kind {
                        VariantKind::Unit => {
                            variants.insert(variant.name.to_string(), VariantDef::Unit);
                            self.fn_sigs.insert(
                                variant.name.to_string(),
                                FnSig {
                                    type_params: type_param_names.clone(),
                                    type_param_bounds: type_param_bounds.clone(),
                                    return_type,
                                    is_builtin_variant: self.in_stdlib_registration,
                                    ..FnSig::default()
                                },
                            );
                        }
                        VariantKind::Tuple(fields) => {
                            let variant_tys: Vec<Ty> = fields
                                .iter()
                                .map(|field| {
                                    self.resolve_registered_annotation_ty(field, &mut hole_vars)
                                })
                                .collect();
                            variants.insert(
                                variant.name.to_string(),
                                VariantDef::Tuple(variant_tys.clone()),
                            );

                            // Register variant constructor as function
                            self.fn_sigs.insert(
                                variant.name.to_string(),
                                FnSig {
                                    type_params: type_param_names.clone(),
                                    type_param_bounds: type_param_bounds.clone(),
                                    params: variant_tys,
                                    return_type,
                                    is_builtin_variant: self.in_stdlib_registration,
                                    ..FnSig::default()
                                },
                            );
                        }
                        VariantKind::Struct(fields) => {
                            let variant_fields: Vec<(String, Ty)> = fields
                                .iter()
                                .map(|(name, field)| {
                                    (
                                        name.to_string(),
                                        self.resolve_registered_annotation_ty(
                                            field,
                                            &mut hole_vars,
                                        ),
                                    )
                                })
                                .collect();
                            variants.insert(
                                variant.name.to_string(),
                                VariantDef::Struct(variant_fields),
                            );
                        }
                    }
                }
                TypeBodyItem::Method(_) => {
                    // Methods are handled in pass 2
                }
            }
        }

        let type_def = TypeDef {
            kind,
            name: td.name.to_string(),
            type_params: type_param_names.clone(),
            bounds: type_param_bounds,
            fields,
            field_order,
            variants,
            methods: HashMap::new(),
            doc_comment: td.doc_comment.clone(),
            is_indirect: td.is_indirect,
        };

        // Register with trait registry for Send/Frozen/Copy/... derivation.
        //
        // Structural markers (`Copy`, `Send`, `Clone`, …) derive from a Named
        // type's reachable member types. For a struct/record those are its
        // fields; for an ENUM they are the variant PAYLOAD types — an enum with
        // a `string`-payload variant is NOT Copy even though it has no named
        // fields. The marker registry stores member types by name, and its
        // derivation walks them with `all(...)` (vacuously true on an empty
        // list), so an enum registered with only its (empty) `fields` would be
        // spuriously Copy/Frozen. Register the variant-inclusive member set for
        // enums so the marker derivation is correct (W5.016: the spurious-Copy
        // bug routed owned-payload enum Vecs down the BitCopy path → runtime
        // stride panic).
        let field_types: Vec<_> = if kind == TypeDefKind::Enum {
            Self::structural_member_types_for_type(&type_def)
        } else {
            type_def.fields.values().cloned().collect()
        };
        let field_types = self.expand_for_marker_registration(&field_types);

        self.registry
            .register_type(td.name.to_string(), field_types);
        self.registry
            .register_type_params(td.name.to_string(), type_param_names.clone());
        // Mirror the markers under the module-qualified key (when this type is
        // declared in a non-root module) so a same-bare-name reply from another
        // package cannot clobber this type's Send derivation at the ask-reply
        // gate. `register_qualified_type_alias` repeats this for the pub import
        // surface; this covers the registration call itself.
        self.seed_qualified_type_markers_for_current_module(td.name.name.as_str());

        self.type_defs.insert(td.name.to_string(), type_def);
        self.record_type_def_inference_holes(td.name.name.as_str(), hole_vars);
        self.handle_bearing_dirty = true;

        // If this is a wire type, register encode/decode/to_json/from_json/to_yaml/from_yaml methods
        if let Some(ref wire) = td.wire {
            self.register_wire_methods(td.name.name.as_str(), wire, &variant_order);
            self.validate_wire_version_constraints(td.name.name.as_str(), wire);
        }
    }

    /// Register a `record` declaration into the type table.
    ///
    /// Named-field form: populates `type_defs.fields` so that
    /// `check_struct_init` and `check_field_access` resolve field types by
    /// name.
    ///
    /// Tuple-positional form: registers a constructor `fn_sig` so that
    /// `R(1, 2)` resolves as a function call returning `Ty::Named { name: R
    /// }`.  The `fields` map is left empty — this deliberately prevents
    /// `.0`/`.1` index-style access (A-D2: positional destructuring only).
    ///
    /// In both cases `type_defs` receives a `TypeDef` with
    /// `kind = TypeDefKind::Record` so the field-write rejection in
    /// `statements.rs` can identify record types.
    pub(in crate::check) fn register_record_decl(&mut self, rd: &RecordDecl) {
        let scope =
            self.enter_primary_sig_scope(&[(rd.type_params.as_ref(), rd.where_clause.as_ref())]);
        self.register_record_decl_in_scope(rd);
        self.exit_primary_sig_scope(scope);
    }

    pub(super) fn register_record_decl_in_scope(&mut self, rd: &RecordDecl) {
        let type_param_names: Vec<String> = rd.type_params.as_ref().map_or(vec![], |params| {
            params.iter().map(|p| p.name.to_string()).collect()
        });
        let type_param_bounds =
            self.collect_type_param_bounds(rd.type_params.as_ref(), rd.where_clause.as_ref());

        // Build the return type for constructors: `R` or `R<T1, T2, …>`
        let enum_return_args: Vec<Ty> = type_param_names
            .iter()
            .map(|name| Ty::param(name))
            .collect();
        let declaration_name = self.current_module_identity().map_or_else(
            || rd.name.to_string(),
            |module| format!("{module}.{}", rd.name),
        );
        let return_type = self.named_ty_for_key(&declaration_name, enum_return_args);

        let mut fields: HashMap<String, Ty> = HashMap::new();
        let mut field_order: Vec<String> = Vec::new();
        let mut hole_vars = Vec::new();
        // Positional field types for tuple records, collected for marker
        // derivation (A-4). Named-record fields come from `type_def.fields`.
        let mut tuple_field_types: Vec<Ty> = Vec::new();

        match &rd.kind {
            RecordKind::Named(record_fields) => {
                for rf in record_fields {
                    let field_ty = self.resolve_registered_annotation_ty(&rf.ty, &mut hole_vars);
                    field_order.push(rf.name.to_string());
                    fields.insert(rf.name.to_string(), field_ty);
                }
            }
            RecordKind::Tuple(positional_types) => {
                // Resolve each positional field type for the constructor signature.
                let param_tys: Vec<Ty> = positional_types
                    .iter()
                    .map(|te| self.resolve_registered_annotation_ty(te, &mut hole_vars))
                    .collect();

                // Capture positional types for marker registration before moving
                // param_tys into fn_sigs. The `fields` map intentionally stays
                // empty — `.0`/`.1` access is not permitted on tuple records (A-D2).
                tuple_field_types.clone_from(&param_tys);

                // Register a constructor function so `R(1, 2)` resolves via
                // `check_call`.  The `fields` map intentionally stays empty —
                // `.0`/`.1` access is not permitted on tuple records (A-D2).
                let signature = FnSig {
                    type_params: type_param_names.clone(),
                    type_param_bounds: type_param_bounds.clone(),
                    params: param_tys,
                    return_type: return_type.clone(),
                    ..FnSig::default()
                };
                self.fn_sigs.insert(declaration_name.clone(), signature);
            }
        }

        let type_def = TypeDef {
            kind: TypeDefKind::Record,
            name: rd.name.to_string(),
            type_params: type_param_names.clone(),
            bounds: type_param_bounds,
            fields,
            field_order,
            variants: HashMap::new(),
            methods: HashMap::new(),
            doc_comment: rd.doc_comment.clone(),
            is_indirect: false,
        };

        // Register all field types for marker derivation (Eq/Hash/Send/Frozen/
        // Clone/Copy). Named-field records use type_def.fields; tuple records
        // use the positional types captured above (type_def.fields is empty for
        // tuple records by design — A-D2).
        let field_types: Vec<Ty> = if tuple_field_types.is_empty() {
            type_def.fields.values().cloned().collect()
        } else {
            tuple_field_types
        };
        let field_types = self.expand_for_marker_registration(&field_types);
        self.registry
            .register_type(declaration_name.clone(), field_types);
        self.registry
            .register_type_params(declaration_name.clone(), type_param_names.clone());
        // Mark this as a record type so implements_marker applies the correct
        // value-type semantics (Resource always false; all other markers field-driven).
        self.registry.register_record_type(declaration_name.clone());

        self.type_defs.insert(declaration_name.clone(), type_def);
        self.record_type_def_inference_holes(declaration_name.as_str(), hole_vars);
        self.handle_bearing_dirty = true;
    }

    /// Register codec methods for a wire type.
    ///
    /// - Wire structs expose binary + JSON/YAML helpers.
    /// - Wire enums expose JSON/YAML helpers.
    pub(in crate::check) fn register_wire_methods(
        &mut self,
        type_name: &str,
        wire: &WireMetadata,
        variant_order: &[String],
    ) {
        // ONE canonical wire identity (A316). A module declaration's wire
        // surface is keyed by `{module}.{Name}` — the identity every resolved
        // receiver and the codegen wire-layout lookup carry; a root
        // declaration's bare name IS its canonical identity. Surface
        // spellings resolve TO this key at lookup time
        // (`canonical_nominal_name`); no bare mirror entries exist, so two
        // same-leaf wire types from different modules never collide on a
        // shared last-write-wins key.
        let canonical_identity = self.current_module_identity().map_or_else(
            || type_name.to_string(),
            |module| format!("{module}.{type_name}"),
        );
        let self_ty = self.named_ty_for_key(&canonical_identity, vec![]);
        let bytes_ty = Ty::Bytes;

        let Some((is_wire_struct, is_serial_wire_enum, layout_entry)) =
            self.type_defs.get(type_name).map(|type_def| {
                let is_wire_struct = type_def.kind == TypeDefKind::Struct;
                let is_unit_wire_enum = type_def.kind == TypeDefKind::Enum
                    && type_def
                        .variants
                        .values()
                        .all(|variant| matches!(variant, VariantDef::Unit));
                let is_payload_wire_enum = type_def.kind == TypeDefKind::Enum
                    && type_def
                        .variants
                        .values()
                        .any(|variant| !matches!(variant, VariantDef::Unit));
                let is_serial_wire_enum = is_unit_wire_enum || is_payload_wire_enum;
                let layout_entry = Self::wire_layout_entry_from_metadata(
                    type_def,
                    wire,
                    is_wire_struct,
                    variant_order,
                );
                (is_wire_struct, is_serial_wire_enum, layout_entry)
            })
        else {
            return;
        };
        // Track wire structs and wire enums so the method-dispatch arms can
        // recognise the binary `encode`/`decode` codec calls (which lower to the
        // `__hew_cbor_serialize_*` / `__hew_cbor_deserialize_*` thunks) without
        // re-deriving wire-ness. Both ride the CBOR body codec: structs as a
        // tag-keyed map, enums as the "map-of-one" shape.
        if is_wire_struct {
            self.wire_struct_types.insert(canonical_identity.clone());
        }
        if is_serial_wire_enum {
            self.wire_enum_types.insert(canonical_identity.clone());
        }
        self.wire_layouts
            .insert(canonical_identity.clone(), layout_entry);

        // Wire structs and wire enums carry the same method surface: the binary
        // CBOR codec (`encode`/`decode`) plus the text-format helpers. The body
        // shapes differ at codegen (struct = tag-keyed map, enum =
        // "map-of-one"), but the registered signatures are identical.
        let instance_methods = if is_wire_struct || is_serial_wire_enum {
            vec![
                ("encode", vec![], bytes_ty.clone()),
                ("to_json", vec![], Ty::String),
                ("to_yaml", vec![], Ty::String),
            ]
        } else {
            vec![]
        };

        // Instance methods land on the DECLARATION record (the bare
        // `type_defs` entry this module's registration just wrote), then the
        // canonical definition is refreshed from it — the
        // `commit_reresolved_type_def` pattern. Pre-registration mints the
        // qualified skeleton before this runs; the later canonical refresh is
        // what carries the codec methods onto the durable definition.
        if let Some(type_def) = self.type_defs.get_mut(type_name) {
            for (method_name, params, return_type) in &instance_methods {
                type_def.methods.insert(
                    (*method_name).to_string(),
                    FnSig {
                        params: params.clone(),
                        return_type: return_type.clone(),
                        ..FnSig::default()
                    },
                );
            }
        }
        if let Some(module_owner) = self.current_module_identity().map(str::to_string) {
            self.register_qualified_type_alias(&module_owner, type_name);
        }

        // `decode` returns bare `Self` (binary CBOR is trap-on-failure); the
        // text-format `from_json`/`from_yaml` parsers can fail on arbitrary
        // user input (config files, HTTP bodies), so they return
        // `Result<Self, string>` — the only honest shape for a fallible parse.
        let from_result_ty = Ty::result(self_ty.clone(), Ty::String);
        let static_methods = if is_wire_struct || is_serial_wire_enum {
            vec![
                ("decode", vec![bytes_ty], self_ty),
                ("from_json", vec![Ty::String], from_result_ty.clone()),
                ("from_yaml", vec![Ty::String], from_result_ty),
            ]
        } else {
            vec![]
        };

        for (method_name, params, return_type) in static_methods {
            // Static codec entry points register under the canonical identity
            // ONLY. The call-site arm canonicalizes the receiver's surface
            // spelling (`Env.from_json` inside the defining module, an
            // importer's binding, an `as`-alias) to this key before lookup.
            self.fn_sigs.insert(
                format!("{canonical_identity}.{method_name}"),
                FnSig {
                    params,
                    return_type,
                    ..FnSig::default()
                },
            );
        }
    }

    pub(super) fn wire_layout_entry_from_metadata(
        type_def: &TypeDef,
        wire: &WireMetadata,
        is_wire_struct: bool,
        variant_order: &[String],
    ) -> WireLayoutEntry {
        let fields = if is_wire_struct {
            wire.field_meta
                .iter()
                .map(|field| WireFieldLayout {
                    name: field.field_name.clone(),
                    tag: field.field_number,
                    json_name: field.json_name.clone(),
                    yaml_name: field.yaml_name.clone(),
                    presence: if field.is_optional {
                        WireFieldPresence::Optional
                    } else {
                        WireFieldPresence::Required
                    },
                    repeated: field.is_repeated,
                })
                .collect()
        } else {
            Vec::new()
        };

        let variant_tags: HashMap<&str, u32> = wire
            .field_meta
            .iter()
            .map(|field| (field.field_name.as_str(), field.field_number))
            .collect();
        let variant_names: Vec<String> = if variant_order.is_empty() {
            let mut names: Vec<_> = type_def.variants.keys().cloned().collect();
            names.sort();
            names
        } else {
            variant_order
                .iter()
                .filter(|name| type_def.variants.contains_key(*name))
                .cloned()
                .collect()
        };
        let variants = if is_wire_struct {
            Vec::new()
        } else {
            variant_names
                .into_iter()
                .enumerate()
                .map(|(index, name)| {
                    #[expect(
                        clippy::cast_possible_truncation,
                        reason = "wire enum variant counts are bounded by source size"
                    )]
                    let default_tag = index as u32;
                    let tag = variant_tags
                        .get(name.as_str())
                        .copied()
                        .unwrap_or(default_tag);
                    (name, tag)
                })
                .collect()
        };

        WireLayoutEntry {
            is_struct: is_wire_struct,
            json_case: wire.json_case,
            yaml_case: wire.yaml_case,
            version: wire.version,
            min_version: wire.min_version,
            fields,
            variants,
        }
    }

    /// Validate version constraints on a wire type.
    pub(in crate::check) fn validate_wire_version_constraints(
        &mut self,
        type_name: &str,
        wire: &hew_parser::ast::WireMetadata,
    ) {
        use crate::error::Severity;

        let decl_span = self.type_def_spans.get(type_name).cloned().unwrap_or(0..0);
        let version = wire.version;
        let min_version = wire.min_version;

        // min_version cannot exceed version
        if let (Some(min_v), Some(v)) = (min_version, version) {
            if min_v > v {
                self.errors.push(TypeError {
                    severity: Severity::Error,
                    kind: TypeErrorKind::InvalidOperation,
                    span: decl_span.clone(),
                    message: format!(
                        "wire `{type_name}`: min_version ({min_v}) cannot exceed version ({v})"
                    ),
                    notes: vec![],
                    suggestions: vec![],
                    source_module: self.current_module.clone(),
                });
            }
        }

        // Per-field `since` constraints
        for fm in &wire.field_meta {
            if let Some(since) = fm.since {
                if version.is_none() {
                    // since has no effect without a schema version
                    self.warnings.push(TypeError {
                        severity: Severity::Warning,
                        kind: TypeErrorKind::StyleSuggestion,
                        span: decl_span.clone(),
                        message: format!(
                            "wire `{type_name}.{}`: field has `since {since}` but type \
                             has no #[wire(version = N)] attribute",
                            fm.field_name
                        ),
                        notes: vec![],
                        suggestions: vec![],
                        source_module: self.current_module.clone(),
                    });
                }

                // since cannot exceed version
                if let Some(v) = version {
                    if since > v {
                        self.errors.push(TypeError {
                            severity: Severity::Error,
                            kind: TypeErrorKind::InvalidOperation,
                            span: decl_span.clone(),
                            message: format!(
                                "wire `{type_name}.{}`: since ({since}) cannot exceed \
                                 schema version ({v})",
                                fm.field_name
                            ),
                            notes: vec![],
                            suggestions: vec![],
                            source_module: self.current_module.clone(),
                        });
                    }
                }
            }

            // Warn if version > 1 and a non-optional field lacks `since`
            if let Some(v) = version {
                if v > 1 && fm.since.is_none() && !fm.is_optional {
                    self.warnings.push(TypeError {
                        severity: Severity::Warning,
                        kind: TypeErrorKind::StyleSuggestion,
                        span: decl_span.clone(),
                        message: format!(
                            "wire `{type_name}.{}`: non-optional field has no `since` annotation \
                             (schema version is {v})",
                            fm.field_name
                        ),
                        notes: vec![],
                        suggestions: vec![],
                        source_module: self.current_module.clone(),
                    });
                }
            }
        }
    }

    /// Register a machine declaration as a type definition with variants and methods.
    #[expect(
        clippy::too_many_lines,
        reason = "machine registration covers states, events, and generated methods"
    )]
    pub(in crate::check) fn register_machine_decl(&mut self, md: &MachineDecl, span: &Span) {
        // Build the machine's self-type: `Machine` or `Machine<T, U, …>`.
        // MachineDecl.type_params is Vec<TypeParam> — we extract bare names
        // here for the self-type and collect declared trait bounds into a
        // side table consulted at use sites (struct-state brace init) and
        // mirrored onto unit-state constructor FnSigs for the call path.
        //
        // Validate before collect_type_param_bounds erases positional type args.
        self.validate_type_param_bound_shapes(
            Some(&md.type_params),
            md.where_clause.as_ref(),
            span,
        );
        let type_param_names: Vec<String> =
            md.type_params.iter().map(|p| p.name.to_string()).collect();
        // Collect inline `<T: Trait>` and `where T: Trait` bounds into a
        // single side table keyed by machine name then param name. At
        // the checker layer, a bound's source (inline vs where clause)
        // does not affect the enforcement question — the bound is
        // "satisfied at the instantiation site iff the substituted
        // type implements the trait" regardless of where the bound
        // was authored — so duplicates on the same (param, trait) pair
        // dedupe. Source provenance is preserved at the parser layer
        // (separate `type_params` / `where_clause` fields on
        // `MachineDecl`) so future lowering layers that want to point
        // diagnostics at the predicate's span can recover it.
        let type_param_bounds =
            self.collect_type_param_bounds(Some(&md.type_params), md.where_clause.as_ref());
        if !type_param_bounds.is_empty() {
            self.machine_type_param_bounds
                .insert(md.name.to_string(), type_param_bounds.clone());
        }
        // W3.039 Stage 2: register const-generic parameter declarations
        // into the side table so instantiation-site validation
        // (Stage 3 — gated on W3.033c) can recover arity, types, and
        // defaults without re-walking the parser AST. We also enforce
        // here that const-param names do not shadow type-param names.
        if !md.const_params.is_empty() {
            let type_param_names: std::collections::HashSet<&str> = md
                .type_params
                .iter()
                .map(|p| p.name.name.as_str())
                .collect();
            let mut const_param_decls: Vec<super::types::MachineConstParamDecl> =
                Vec::with_capacity(md.const_params.len());
            let mut seen_const_names: std::collections::HashSet<&str> =
                std::collections::HashSet::new();
            for cp in &md.const_params {
                if type_param_names.contains(cp.name.name.as_str()) {
                    self.errors.push(crate::error::TypeError::new(
                        crate::error::TypeErrorKind::DuplicateDefinition,
                        span.clone(),
                        format!(
                            "const parameter `{}` on machine `{}` shadows a type parameter \
                             of the same name",
                            cp.name, md.name
                        ),
                    ));
                    continue;
                }
                if !seen_const_names.insert(cp.name.name.as_str()) {
                    self.errors.push(crate::error::TypeError::new(
                        crate::error::TypeErrorKind::DuplicateDefinition,
                        span.clone(),
                        format!(
                            "duplicate const parameter `{}` on machine `{}`",
                            cp.name, md.name
                        ),
                    ));
                    continue;
                }
                let ty = match cp.ty {
                    hew_parser::ast::ConstParamTy::Usize => {
                        super::types::MachineConstParamTy::Usize
                    }
                };
                const_param_decls.push(super::types::MachineConstParamDecl {
                    name: cp.name.to_string(),
                    ty,
                    default: cp.default,
                });
            }
            if !const_param_decls.is_empty() {
                self.machine_const_params
                    .insert(md.name.to_string(), const_param_decls);
            }
        }
        let machine_generic_args: Vec<Ty> = type_param_names
            .iter()
            .map(|name| Ty::param(name))
            .collect();
        let machine_identity = self.declaration_identity(md.name.name.as_str());
        let machine_ty = self.named_ty_for_key(&machine_identity, machine_generic_args.clone());

        let event_type_name = format!("{}Event", md.name);
        let event_identity = self.declaration_identity(&event_type_name);
        let event_ty = self.named_ty_for_key(&event_identity, machine_generic_args.clone());

        // Build state variants
        let mut variants = HashMap::new();
        let mut machine_hole_vars = Vec::new();
        for state in &md.states {
            if state.fields.is_empty() {
                variants.insert(state.name.to_string(), VariantDef::Unit);
                // Register unit state constructor as a function. For generic
                // machines (e.g. `machine Worker<T>`), the constructor returns
                // `Worker<T>` so callers can instantiate with concrete args.
                self.fn_sigs.insert(
                    state.name.to_string(),
                    FnSig {
                        type_params: type_param_names.clone(),
                        type_param_bounds: type_param_bounds.clone(),
                        return_type: machine_ty.clone(),
                        ..FnSig::default()
                    },
                );
            } else {
                let variant_fields: Vec<(String, Ty)> = state
                    .fields
                    .iter()
                    .map(|(name, spanned_te)| {
                        (
                            name.to_string(),
                            self.resolve_registered_annotation_ty(
                                spanned_te,
                                &mut machine_hole_vars,
                            ),
                        )
                    })
                    .collect();
                variants.insert(state.name.to_string(), VariantDef::Struct(variant_fields));
            }
        }

        let type_def = TypeDef {
            kind: TypeDefKind::Machine,
            name: md.name.to_string(),
            type_params: type_param_names.clone(),
            bounds: type_param_bounds.clone(),
            fields: HashMap::new(),
            field_order: vec![],
            variants,
            methods: HashMap::new(),
            doc_comment: None,
            is_indirect: false,
        };

        // Register field types for Send/Frozen derivation
        let mut all_field_types = Vec::new();
        for state in &md.states {
            for (_, spanned_te) in &state.fields {
                all_field_types.push(self.resolve_type_expr(spanned_te));
            }
        }
        let all_field_types = self.expand_for_marker_registration(&all_field_types);
        self.registry
            .register_type(md.name.to_string(), all_field_types);
        self.registry
            .register_type_params(md.name.to_string(), type_param_names.clone());

        self.commit_reresolved_type_def(md.name.name.as_str(), type_def);
        self.record_type_def_inference_holes(&machine_identity, machine_hole_vars);
        self.known_types.insert(md.name.to_string());
        self.known_types.insert(machine_identity.clone());

        // Register the generated event companion enum
        let mut event_variants = HashMap::new();
        let mut event_hole_vars = Vec::new();
        for event in &md.events {
            if event.fields.is_empty() {
                event_variants.insert(event.name.to_string(), VariantDef::Unit);
            } else {
                let variant_fields: Vec<(String, Ty)> = event
                    .fields
                    .iter()
                    .map(|(name, spanned_te)| {
                        (
                            name.to_string(),
                            self.resolve_registered_annotation_ty(spanned_te, &mut event_hole_vars),
                        )
                    })
                    .collect();
                event_variants.insert(event.name.to_string(), VariantDef::Struct(variant_fields));
            }
        }
        let event_type_def = TypeDef {
            kind: TypeDefKind::Enum,
            name: event_type_name.clone(),
            type_params: type_param_names.clone(),
            bounds: type_param_bounds.clone(),
            fields: HashMap::new(),
            field_order: vec![],
            variants: event_variants,
            methods: HashMap::new(),
            doc_comment: None,
            is_indirect: false,
        };
        // Register the event companion's variant-payload member types for
        // Send/Frozen/… derivation — the same call an ordinary `enum`
        // declaration gets in `register_type_decl`. Without this the
        // companion enum has no `type_fields` entry, so `TraitRegistry`
        // treats it as an unknown type and conservatively derives every
        // marker false — including `Send` — even when every event payload
        // is itself Send (#3122: the spec's actor example, which sends the
        // companion enum to a `receive fn`, could not compile).
        let event_field_types = Self::structural_member_types_for_type(&event_type_def);
        let event_field_types = self.expand_for_marker_registration(&event_field_types);
        self.registry
            .register_type(event_type_name.clone(), event_field_types);
        self.registry
            .register_type_params(event_type_name.clone(), type_param_names.clone());
        self.commit_reresolved_type_def(&event_type_name, event_type_def);
        self.record_type_def_inference_holes(&event_identity, event_hole_vars);
        self.known_types.insert(event_type_name.clone());
        self.known_types.insert(event_identity);

        // Register the step() method on the machine type
        if let Some(td) = self.type_defs.get_mut(&machine_identity) {
            td.methods.insert(
                "step".to_string(),
                FnSig {
                    param_names: vec!["event".to_string()],
                    params: vec![event_ty.clone()],
                    ..FnSig::default()
                },
            );
            // Register state_name() method
            td.methods.insert(
                "state_name".to_string(),
                FnSig {
                    return_type: Ty::String,
                    ..FnSig::default()
                },
            );
            // Register take_emits(event) method: removes every queued emit
            // matching (this machine's type id, the argument's event tag)
            // from the thread-local emit queue and returns the count
            // removed. Sibling of `step`/`state_name` — same event
            // companion enum param.
            td.methods.insert(
                "take_emits".to_string(),
                FnSig {
                    param_names: vec!["event".to_string()],
                    params: vec![event_ty],
                    return_type: Ty::I64,
                    ..FnSig::default()
                },
            );
        }
        if machine_identity != md.name.name.as_str() {
            if let Some(type_def) = self.type_defs.get(&machine_identity).cloned() {
                self.type_defs.insert(md.name.to_string(), type_def);
            }
        }
    }
}
