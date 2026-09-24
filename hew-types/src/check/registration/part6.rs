//! Split from `registration.rs`: checker methods, part 6 of 6.
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
                self.local_type_defs.insert(td.name.clone());
                self.source_type_defs.insert(td.name.clone());
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
                            &fd.name,
                            fd,
                        );
                        self.current_module = saved_importer_module;
                        self.current_module_idx = importer_file_idx;
                    }

                    // Publish selected names as lexical bindings to this declaration.
                    // Private functions cannot be imported bare.
                    if fd.visibility.is_pub() && Self::should_import_name(&fd.name, spec) {
                        let binding_name = Self::resolve_import_name(spec, &fd.name)
                            .unwrap_or_else(|| fd.name.clone());
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
                        self.register_canonical_type_def(module_full_path, &decl.name, &definition);
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
                        self.record_module_type_export(module_full_path, &decl.name);
                        if let Some(binding) =
                            StdlibBarePublication::Import(spec).bare_binding(&decl.name)
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
                    self.record_module_type_export(module_full_path, &decl.name);
                    if spec.is_none() {
                        self.record_module_type_export(module_short, &decl.name);
                    }
                    if Self::should_import_name(&decl.name, spec) {
                        let binding = Self::resolve_import_name(spec, &decl.name)
                            .unwrap_or_else(|| decl.name.clone());
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
                                let source_def = self.type_defs.get(&td.name).cloned();
                                self.current_module = saved_importer_module;
                                self.current_module_idx = importer_file_idx;
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
                    // Qualified authority is always published: the source
                    // module's own bare def (read by the alias copy), the
                    // qualified alias, and the module-export record that drives
                    // use-time ambiguity candidate naming.
                    let saved_importer_module =
                        self.current_module.replace(module_full_path.to_string());
                    self.current_module_idx = declaring_file_idx;
                    self.register_type_decl(td);
                    let source_def = self.type_defs.get(&td.name).cloned();
                    self.current_module = saved_importer_module;
                    self.current_module_idx = importer_file_idx;
                    if spec.is_none() {
                        self.register_qualified_type_alias(module_short, &td.name);
                    }
                    if let Some(source_def) = source_def.as_ref() {
                        self.register_canonical_type_def(module_full_path, &td.name, source_def);
                    }
                    self.record_module_type_export(module_full_path, &td.name);
                    if spec.is_none() {
                        self.record_module_type_export(module_short, &td.name);
                    }
                    // The importer-scope bare binding is opt-in: a plain
                    // `import m;` publishes only the qualified name, mirroring
                    // the function/trait arms. Named (`::{ T }`) and glob
                    // imports publish the bare (or aliased) binding.
                    if Self::should_import_name(&td.name, spec) {
                        let explicit_import_name = Self::resolve_import_name(spec, &td.name);
                        let binding_name = explicit_import_name
                            .clone()
                            .unwrap_or_else(|| td.name.clone());
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
                        &md.name,
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
                    let machine_def = self.type_defs.get(&md.name).cloned();
                    let event_def = self.type_defs.get(&event_name).cloned();
                    self.current_module = saved_importer_module;
                    self.current_module_idx = importer_file_idx;
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
                    // Bare publication of the machine and its event enum is
                    // opt-in, gated together so a named/glob import exposes both
                    // or neither.
                    if Self::should_import_name(&md.name, spec) {
                        let machine_binding = Self::resolve_import_name(spec, &md.name)
                            .unwrap_or_else(|| md.name.clone());
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
                                &super_trait.name,
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
                    let import_binding = if Self::should_import_name(&tr.name, spec) {
                        let binding_name = Self::resolve_import_name(spec, &tr.name)
                            .unwrap_or_else(|| tr.name.clone());
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
                                method.name.clone(),
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
                                self.mark_imported_trait_used(Some(module_full_path), &s.name);
                                self.resolve_super_trait_edge(module_full_path, &s.name)
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
                                    method.name.clone(),
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
                    if Self::should_import_name(&cd.name, spec) {
                        let binding_name = Self::resolve_import_name(spec, &cd.name)
                            .unwrap_or_else(|| cd.name.clone());
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
                        name: type_name,
                        type_args,
                    } = &id.target_type.0
                    {
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
                            self.record_trait_impl(type_name, &tb.name);
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
                            &ad.name,
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
                    self.record_module_type_export(module_full_path, &ad.name);
                    // If named import or glob, also register unqualified
                    if Self::should_import_name(&ad.name, spec) {
                        let binding_name = Self::resolve_import_name(spec, &ad.name)
                            .unwrap_or_else(|| ad.name.clone());
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
                            &sd.name,
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
                    self.record_module_type_export(module_full_path, &sd.name);
                    if Self::should_import_name(&sd.name, spec) {
                        let binding_name = Self::resolve_import_name(spec, &sd.name)
                            .unwrap_or_else(|| sd.name.clone());
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
                    if let Some(source_def) = self.type_defs.get(&td.name).cloned() {
                        self.register_canonical_type_def(module_full_path, &td.name, &source_def);
                    }
                    self.retire_imported_type_keys(module_short, module_full_path, &td.name);
                }
                Item::Machine(md) if md.visibility.is_pub() => {
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
                }
                _ => {}
            }
        }

        self.local_type_defs = saved_local_type_defs;
        self.source_type_defs = saved_source_type_defs;
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
        let no_file_exclusion: HashSet<hew_parser::module::ModuleId> = HashSet::new();
        let preferred = collision_preferred_package_module_ids(program, &no_file_exclusion);
        let mut colliding: HashSet<String> = HashSet::new();
        for module in mg.modules.values() {
            for (item, _) in &module.items {
                let name = match item {
                    Item::TypeDecl(decl) => &decl.name,
                    Item::Record(decl) => &decl.name,
                    _ => continue,
                };
                if colliding.contains(name) {
                    continue;
                }
                if collision_imported_type_name_collides(
                    program,
                    &no_file_exclusion,
                    &preferred,
                    name,
                ) {
                    colliding.insert(name.clone());
                }
            }
        }
        colliding
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
        let identity = Self::actor_identity(Some(module_short), &ad.name);
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
        let Ty::Named {
            name,
            args,
            builtin,
        } = ty
        else {
            return ty.clone();
        };
        let args = args
            .iter()
            .map(|arg| self.qualify_colliding_reply_ty(arg, module_short))
            .collect();
        if builtin.is_some()
            || name.contains('.')
            || !self.cross_module_colliding_record_names.contains(name)
        {
            return Ty::Named {
                name: name.clone(),
                args,
                builtin: *builtin,
            };
        }
        let qualified = format!("{module_short}.{name}");
        if self.type_defs.contains_key(&qualified) {
            Ty::Named {
                name: qualified,
                args,
                builtin: None,
            }
        } else {
            Ty::Named {
                name: name.clone(),
                args,
                builtin: *builtin,
            }
        }
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
        let identity = Self::actor_identity(module_short, &ad.name);
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
            self.type_defs.remove(key);
            self.type_def_spans.remove(key);
            self.registry.remove_type_marker_key(key);
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
