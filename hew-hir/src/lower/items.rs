//! Function, impl-block and imported item lowering.

use super::*;

impl LowerCtx {
    pub(super) fn lower_fn(
        &mut self,
        func: &FnDecl,
        span: std::ops::Range<usize>,
    ) -> Option<HirFn> {
        self.lower_fn_with_name(func, &func.name, span)
    }

    /// V0b: lower a top-level `impl [<TypeParams>] [Trait for] TargetType { ... }`
    /// block. Methods are flattened into per-impl `HirItem::Function` entries
    /// (named `<SelfType>::<method>`) so the existing MIR / codegen function
    /// pipeline can pick them up unchanged; a metadata-only [`HirItem::Impl`]
    /// is also pushed so checker-side reasoning has a single anchor for the
    /// trait/self pair and the associated-type bindings.
    ///
    /// Fail-closed: any shape outside the V0b sufficient surface
    /// (where-clauses, default-method bodies, non-nominal targets, blanket
    /// impls) emits a named [`HirDiagnosticKind::ImplBlockShapeNotLowered`]
    /// rather than falling through to the generic catch-all.
    #[allow(
        clippy::too_many_lines,
        reason = "single linear lowering path with a sequence of fail-closed \
                  shape guards (where-clause / non-nominal / blanket / builtin \
                  inherent) followed by the per-method emission loop; splitting \
                  would scatter the guards from the emission they protect"
    )]
    pub(super) fn lower_impl_block(
        &mut self,
        decl: &hew_parser::ast::ImplDecl,
        span: std::ops::Range<usize>,
        items: &mut Vec<HirItem>,
        pub_only: bool,
        imported: Option<&ImportedImplLowering<'_>>,
    ) {
        // Pre-flight: classify the impl shape. Bail with a precise diagnostic
        // on any unsupported variant before lowering bodies.
        if let Some(shape) = classify_unsupported_where_clause(decl) {
            self.diagnostics.push(HirDiagnostic::new(
                HirDiagnosticKind::ImplBlockShapeNotLowered { shape },
                span,
                "impl-block shape not yet lowered: only where-clause predicates of the form \
                 `where T: Bound(s)` on the impl's own type parameters are admitted — \
                 predicates on parameterised types and non-type-param names require later slices",
            ));
            return;
        }
        let TypeExpr::Named {
            name: self_type_name,
            type_args: target_type_args,
        } = &decl.target_type.0
        else {
            self.diagnostics.push(HirDiagnostic::new(
                HirDiagnosticKind::ImplBlockShapeNotLowered {
                    shape: "impl on non-nominal target".to_string(),
                },
                span,
                "impl-block shape not yet lowered: impl target must be a named \
                 nominal type (e.g. `impl Trait for VecIter<T>`); tuple, array, \
                 function, and trait-object targets are not yet supported",
            ));
            return;
        };
        // Record the FFI-backed nominal surface of this impl BEFORE any
        // metadata-only skip below drops the block: a record named in the
        // signature of an `#[extern_symbol]` method is constructed/consumed
        // behind the C ABI, which is the signal
        // `finalize_user_record_value_classes` needs to admit a zero-field
        // record as a pointer-width handle stand-in.
        for method in &decl.methods {
            if !method
                .attributes
                .iter()
                .any(|attribute| attribute.name == "extern_symbol")
            {
                continue;
            }
            for ty in method_signature_type_exprs(method) {
                let mut names = Vec::new();
                collect_type_expr_named_leaves(ty, &mut names);
                for name in names {
                    if let Some(module) = &self.current_module_name {
                        self.extern_backed_record_names
                            .insert(format!("{module}.{name}"));
                    }
                    self.extern_backed_record_names.insert(name);
                }
            }
        }
        // Outer type-parameter names (e.g. `T` in `impl<T> Iterator for VecIter<T>`).
        let type_params: Vec<String> = decl
            .type_params
            .as_ref()
            .map(|ps| ps.iter().map(|p| p.name.clone()).collect())
            .unwrap_or_default();
        // For concrete specialised impls (`impl Describe for Wrapper<i64>`, i.e.
        // empty type_params with non-empty target type args), lower the target's
        // type args and compute a mangled self-type name for use in method symbols.
        // This prevents `impl Describe for Wrapper<i64>` and
        // `impl Describe for Wrapper<string>` from both emitting the symbol
        // `"Wrapper::describe"` and colliding in fn_registry / codegen (#2270).
        //
        // Generic impls (`impl<U> Describe for Wrapper<U>`, non-empty type_params)
        // are excluded: their method symbols stay bare (`"Wrapper::describe"`)
        // and monomorphisation suffixes them at instantiation time.
        let self_type_concrete_args: Vec<hew_types::ResolvedTy> = if type_params.is_empty() {
            target_type_args
                .as_deref()
                .unwrap_or(&[])
                .iter()
                .map(|a| self.lower_type(a))
                .collect()
        } else {
            Vec::new()
        };
        // Symbol name for this impl's methods: mangled when the impl is a concrete
        // specialisation of a generic type, bare otherwise.
        let base_symbol_self_name = imported
            .and_then(|context| context.symbol_self_name)
            .unwrap_or_else(|| {
                if self.lowering_injected_items {
                    injected_builtin_impl_symbol_owner(self_type_name)
                } else {
                    self_type_name.as_str()
                }
            });
        let symbol_self_name: std::borrow::Cow<str> = if imported
            .and_then(|context| context.symbol_self_name)
            .is_some()
            || self_type_concrete_args.is_empty()
        {
            std::borrow::Cow::Borrowed(base_symbol_self_name)
        } else {
            std::borrow::Cow::Owned(crate::monomorph::mangle(
                base_symbol_self_name,
                &self_type_concrete_args,
            ))
        };
        // Blanket-impl guard: reject `impl<T> Trait for T` (target name is
        // itself one of the outer type parameters) — V0b does not handle the
        // monomorphisation of blanket impls.
        if type_params.iter().any(|p| p == self_type_name) {
            self.diagnostics.push(HirDiagnostic::new(
                HirDiagnosticKind::ImplBlockShapeNotLowered {
                    shape: format!(
                        "blanket impl `impl<{self_type_name}> ... for {self_type_name}`"
                    ),
                },
                span,
                "impl-block shape not yet lowered: blanket impls (target is a \
                 bare type parameter) require specialisation infrastructure \
                 that is not in V0b — restrict the target to a concrete \
                 nominal type",
            ));
            return;
        }
        // The checker-resolved target type is the authority for every
        // compiler-reserved inherent-impl exception below. Source spellings
        // such as `Vec`, `Option`, and `Result` are ordinary user nominals
        // unless they carry the corresponding builtin discriminator.
        let target_is_alias = self.type_alias_for_name(self_type_name).is_some();
        let previous_type_params = std::mem::replace(
            &mut self.current_fn_type_params,
            type_params.iter().cloned().collect(),
        );
        let mut resolved_impl_self_ty = self.lower_type(&decl.target_type);
        self.current_fn_type_params = previous_type_params;
        // Injected `std/builtins.hew` impls are compiler-owned declarations.
        // A root user declaration with the same source leaf must not retag
        // their `Self` type or their static-dispatch metadata. Recover the
        // exact builtin discriminator only at this provenance-bearing injected
        // boundary; ordinary source impls continue to trust checker resolution.
        if self.lowering_injected_items {
            let injected_builtin = SYNTHETIC_CURSOR_LAYOUT_SPECS
                .iter()
                .find(|spec| spec.builtin.canonical_name() == self_type_name)
                .map(|spec| spec.builtin)
                .or_else(|| hew_types::lookup_builtin_type(self_type_name));
            if let (Some(builtin), ResolvedTy::Named { args, .. }) =
                (injected_builtin, &resolved_impl_self_ty)
            {
                resolved_impl_self_ty =
                    ResolvedTy::named_builtin(builtin.canonical_name(), builtin, args.clone());
            }
        }
        let builtin_impl_kind = match &resolved_impl_self_ty {
            ResolvedTy::Named { builtin, .. } => *builtin,
            ResolvedTy::Duration => Some(BuiltinType::Duration),
            _ => None,
        };
        // Inherent-impl on builtin nominal guard: reject `impl Vec<T> { ... }`
        // and similar bare inherent impls on builtin generic types (`Vec`,
        // `HashMap`, `Option`, `Result`, etc.). The stdlib ships its own
        // inherent impls on these types via compiled-in sources (registered
        // through the checker's `register_builtins_hew_impls` path, not via
        // HIR lowering), so a user-source inherent impl on the same nominal
        // collides downstream with a confusing duplicate-definition error.
        // Fail-closing at the V0b boundary makes the rejection site the
        // failure site. Trait impls (`impl MyTrait for Vec<T>`) are not
        // covered here — orphan-rule policing is a separate concern.
        //
        // Exception: declarative receiver FFI blocks whose methods are all
        // `#[extern_symbol]` are metadata-only; skip them as already consumed.
        //
        // The `duration` constructor block (`from_nanos`/`from_micros`/
        // `from_millis`/`from_secs`, each taking a non-receiver `i64`) is the
        // exception that must fall through to normal lowering: its arithmetic
        // bodies (`n * 1<unit>`) run, so it is registered as real `duration::from_*`
        // HIR fns via the user-impl spine (see `is_builtin_duration_ctor_impl`).
        // It carries no `#[extern_symbol]`, so without this bypass the guard
        // below would reject `duration` as a builtin nominal once its canonical
        // name became a recognised builtin lookup key.
        let duration_ctors = ["from_nanos", "from_micros", "from_millis", "from_secs"];
        let is_duration_ctor_block = builtin_impl_kind == Some(BuiltinType::Duration)
            && decl.methods.len() == duration_ctors.len()
            && decl.methods.iter().all(|m| {
                duration_ctors.contains(&m.name.as_str())
                    && m.params.first().is_none_or(|param| {
                        !matches!(
                            &param.ty.0,
                            TypeExpr::Named { name, .. } if name == "Self" || name == "duration"
                        )
                    })
            });
        // The standard-library module that owns a builtin type's method
        // surface (`std.option` for `Option`, `std.result` for `Result`,
        // `std.iter` for the `VecIter` cursor) lowers its inherent impl
        // through the normal impl-body pipeline. User-source inherent impls on
        // builtin nominals still take the guard below.
        let is_std_method_surface = matches!(
            (builtin_impl_kind, self.current_module_name.as_deref()),
            (Some(BuiltinType::Option), Some("std.option"))
                | (Some(BuiltinType::Result), Some("std.result"))
                | (Some(BuiltinType::VecIter), Some("std.iter"))
        );
        // Encoding values retain a compiler representation, but their methods
        // are ordinary source bodies in the checked declaration's own module.
        let is_declaring_encoding_impl = builtin_impl_kind
            .is_some_and(BuiltinType::is_encoding_value)
            && matches!(&resolved_impl_self_ty, ResolvedTy::Named { name, .. } if {
                self.identity.declarations().any(|(occurrence, declaration)| {
                    self.identity.declaration_by_path(name) == Some(declaration)
                        && occurrence.module().is_some_and(|module| {
                            Some(self.identity.module_path(module))
                                == self.current_module_name.as_deref()
                                    .or_else(|| self.identity.root_module_path())
                        })
                })
            });
        if !target_is_alias
            && !is_duration_ctor_block
            && !is_std_method_surface
            && !is_declaring_encoding_impl
            && decl.trait_bound.is_none()
            && builtin_impl_kind.is_some()
        {
            // Read the class under the checker-resolved declaration path: the
            // source spelling is a bare leaf that a root nominal of the same
            // name also claims in `type_classes`.
            let declared_resource_close_impl = matches!(
                &resolved_impl_self_ty,
                ResolvedTy::Named { name, .. }
                    if self.type_classes.get(name).is_some_and(|(marker, _)| {
                        *marker == ResourceMarker::Resource
                    })
            ) && decl.methods.iter().any(|m| m.name == "close");
            if declared_resource_close_impl {
                // `std/link_monitor.hew` is both the source declaration for the
                // builtin `MonitorRef` nominal and the `#[resource]` close
                // ritual that drop elaboration must call. It is an authored
                // standard-library source file, not a user extension of a
                // pre-existing builtin receiver surface, so lower the close
                // method normally.
            } else {
                let all_methods_are_extern_symbol_ffi = !decl.methods.is_empty()
                    && decl
                        .methods
                        .iter()
                        .all(|m| m.attributes.iter().any(|a| a.name == "extern_symbol"));
                if all_methods_are_extern_symbol_ffi {
                    return;
                }
                self.diagnostics.push(HirDiagnostic::new(
                    HirDiagnosticKind::ImplBlockShapeNotLowered {
                        shape: format!("inherent impl on builtin nominal `{self_type_name}`"),
                    },
                    span,
                    "impl-block shape not yet lowered: inherent impls on builtin \
                 nominal types (`Vec`, `HashMap`, `Option`, `Result`, etc.) are \
                 reserved for the standard library — user code may add trait \
                 impls (`impl MyTrait for Vec<T>`) but not bare \
                 `impl Vec<T> { ... }`",
                ));
                return;
            }
        }
        // V0b uses `FnDecl` for impl-block methods (per parser ast), which
        // always carries a `Block` body — there is no body-less / default-method
        // shape representable on the impl AST node itself.  Trait-decl default
        // methods are lowered below, after the explicitly-overridden methods,
        // by synthesising a `FnDecl` from the trait's `TraitMethod` body.  This
        // was previously skipped (out of scope for V0b) but is now required to
        // support `self.other_method()` dispatch inside a default body.

        // Lower methods through the standard `lower_fn_with_name` path. Each
        // method becomes a top-level `HirItem::Function` keyed by
        // `<SelfType>::<method>` — identical naming to the pre-V0b Index
        // special-case, so downstream consumers (MIR / codegen / monomorph)
        // see ordinary functions and need no new wiring per-method.
        // When `pub_only` is true (imported-module path) only pub-visibility
        // methods are lowered; private methods are not accessible to importers
        // and must not leak into the emitted HirItem list.
        let mut method_symbols: Vec<String> = Vec::with_capacity(decl.methods.len());
        let mut method_names: Vec<String> = Vec::with_capacity(decl.methods.len());
        let mut method_declaring_traits: Vec<String> = Vec::with_capacity(decl.methods.len());
        let mut method_declaring_trait_ids: Vec<Option<hew_types::DefId>> =
            Vec::with_capacity(decl.methods.len());
        let mut method_trait_method_ids: Vec<Option<hew_types::DefId>> =
            Vec::with_capacity(decl.methods.len());
        let mut method_ids: Vec<Option<hew_types::DefId>> = Vec::with_capacity(decl.methods.len());
        let mut method_item_ids: Vec<ItemId> = Vec::with_capacity(decl.methods.len());
        // W3.042 S2-S1: stash the resolved impl-target type so that `Self`
        // appearing in any method's parameter/return annotation (notably the
        // parser-injected `self: Self` for bare `self` / `var self` receivers)
        // lowers to the concrete type. The lowering of `target_type` happens
        // outside the per-method loop so it pays the cost once. Restore the
        // previous value (almost always `None`) on exit so nested
        // impl-lowering reentry — should it ever arise — does not leak state.
        // `HirImplBlock::self_type_name` is receiver identity metadata, not a
        // callable-symbol prefix. Imported impl symbols carry the canonical
        // declaration owner from `impl_body_plan`, while their resolved receiver
        // type independently carries the source identity. Preserve both facts:
        // MIR compares this metadata with parameter zero to distinguish a true
        // receiver from an associated function's ordinary first argument.
        let hir_impl_self_type_name = match &resolved_impl_self_ty {
            ResolvedTy::Named {
                builtin: Some(BuiltinType::VecIter),
                ..
            } => "std.builtins.VecIter".to_string(),
            ResolvedTy::Named {
                builtin: Some(BuiltinType::HashMapIter),
                ..
            } => "std.builtins.HashMapIter".to_string(),
            ResolvedTy::Named { name, .. } => self.current_module_name.as_deref().map_or_else(
                || name.clone(),
                |module| {
                    // Runtime carrier presentation may deliberately collapse a
                    // source-owned builtin to its catalog name (`MonitorRef`),
                    // but impl metadata participates in declaration/lifecycle
                    // joins and must retain the exact source owner.  Consult the
                    // declaration registry rather than rebuilding identity from
                    // a builtin name, so a same-leaf user carrier cannot inherit
                    // the standard-library lifecycle.
                    let declared = format!("{module}.{self_type_name}");
                    if self.source_type_identities.contains(&declared) {
                        return declared;
                    }
                    let module_short = hew_types::short_name(module);
                    name.strip_prefix(&format!("{module_short}."))
                        .map_or_else(|| name.clone(), |local| format!("{module}.{local}"))
                },
            ),
            _ => resolved_impl_self_ty.impl_receiver_instance().map_or_else(
                || base_symbol_self_name.to_string(),
                |instance| instance.nominal.declaration().full_path().to_string(),
            ),
        };
        let impl_self_nominal = resolved_impl_self_ty
            .impl_receiver_instance()
            .map(|instance| instance.nominal);
        let prior_self_ty = self.current_impl_self_ty.take();
        self.current_impl_self_ty = Some(resolved_impl_self_ty);
        for method in &decl.methods {
            if pub_only && !method.visibility.is_pub() {
                continue;
            }
            if let Some(imp) = imported {
                if imp.skip_methods.contains(method.name.as_str()) {
                    continue;
                }
            }
            let symbol = crate::node::HirImplBlock::method_symbol(&symbol_self_name, &method.name);
            let declaration = self.impl_method_declaration_ids.get(&symbol).cloned();
            if let Some((declaration, selected)) = declaration.as_ref().and_then(|declaration| {
                self.impl_body_plan
                    .symbols
                    .get(declaration)
                    .map(|selected| (declaration, selected))
            }) {
                if impl_body_symbols_alias_one_declaration(declaration, selected, &symbol) {
                    // This AST body is a second import-path view of the exact
                    // declaration already selected by the plan. Only the
                    // selected spelling may materialise a HIR function.
                    continue;
                }
            }
            // Pass impl-level type params so that methods of e.g. `impl<U> Trait for Wrapper<U>`
            // carry `U` as a `HirFn::type_params` entry — required for monomorphization
            // of generic-over-generic impl methods (W3.022 Stage 3).
            let Some(hir_method) = self.lower_fn_with_name_and_impl_params(
                method,
                &symbol,
                span.clone(),
                &type_params,
                Some(&symbol_self_name),
                None,
            ) else {
                continue;
            };
            // Explicit impl-method bodies are written in the file that declares
            // the impl, so record as root-origin when this impl is lowered from
            // the root file (module index 0). Injected builtin receiver impls
            // (`lowering_injected_items`) are excluded even at index 0 — their
            // bodies index `std/builtins.hew`, not the user's root source, so a
            // fail-closed there must render bare, not a false root caret. Default
            // methods (below) are deliberately NOT recorded: their bodies are
            // copied from the trait declaration and index the trait's source,
            // which may be an imported module — attributing them to the root
            // would render a false caret.
            if self.current_module_idx == 0 && !self.lowering_injected_items {
                self.root_item_ids.insert(hir_method.id);
            }
            // Publish the body only after it has actually emitted, and prove
            // it is the symbol promised by the pre-lowering declaration plan.
            // The plan deliberately excludes imported methods skipped for an
            // unresolved body/signature, so a checker compatibility alias can
            // never be promoted into a callable implementation body.
            if let Some(declaration) = &declaration {
                if self.lowering_injected_items
                    || self.validate_impl_body_plan(declaration, &symbol, &span)
                {
                    self.impl_method_body_symbols
                        .entry(declaration.clone())
                        .or_insert_with(|| symbol.clone());
                }
            }
            method_item_ids.push(hir_method.id);
            items.push(HirItem::Function(hir_method));
            method_symbols.push(symbol.clone());
            method_names.push(method.name.clone());
            let declaring_trait = decl
                .trait_bound
                .as_ref()
                .map_or(String::new(), |tb| tb.name.clone());
            let ids = self.trait_method_identity(&declaring_trait, &method.name);
            let declaring_trait = ids.as_ref().map_or(declaring_trait, |(trait_id, _)| {
                trait_id.full_path().to_string()
            });
            method_declaring_traits.push(declaring_trait);
            method_declaring_trait_ids.push(ids.as_ref().map(|(trait_id, _)| trait_id.clone()));
            method_trait_method_ids.push(ids.as_ref().map(|(_, method_id)| method_id.clone()));
            method_ids.push(declaration);
        }

        // Lower trait default methods that are NOT overridden in this impl.
        // Each default body becomes its own `HirItem::Function` keyed by
        // `<SelfType>::<method>`, exactly like an explicit impl method.
        // `current_impl_self_ty` is already set to the concrete self type above
        // so `Self` inside the default body lowers to the correct concrete type.
        if let Some(tb) = &decl.trait_bound {
            let overridden: HashSet<&str> = decl.methods.iter().map(|m| m.name.as_str()).collect();
            if let Some(default_owner_key) = self.trait_declaration(&tb.name) {
                if let Some(defaults) = self.trait_defaults.get(&default_owner_key).cloned() {
                    let saved_module_idx = self.current_module_idx;
                    let saved_module = self.current_module_name.clone();
                    for default_method in &defaults {
                        if overridden.contains(default_method.method.name.as_str()) {
                            continue;
                        }
                        self.current_module_idx = default_method.file_index;
                        self.current_module_name
                            .clone_from(&default_method.source_module);
                        let fn_decl = trait_method_to_fn_decl(&default_method.method);
                        let symbol = crate::node::HirImplBlock::method_symbol(
                            &symbol_self_name,
                            &fn_decl.name,
                        );
                        let declaring_trait = default_method.trait_id.full_path().to_string();
                        let ids = Some((
                            default_method.trait_id.clone(),
                            default_method.method_id.clone(),
                        ));
                        // Trait declaration IDs own static lookup; this
                        // materialised default body needs a distinct concrete
                        // implementation identity for body lookup and
                        // monomorphisation. The checker has no explicit
                        // method declaration for a body it did not see in the
                        // impl AST, so mint one exactly at this synthesis
                        // boundary from the carried trait and self identities.
                        let synthetic_default_declaration =
                            ids.as_ref().and_then(|(declaring_trait, _)| {
                                Self::synthetic_default_impl_body_declaration(
                                    declaring_trait,
                                    self.current_impl_self_ty.as_ref(),
                                    &fn_decl.name,
                                )
                            });
                        if let Some(declaration) = &synthetic_default_declaration {
                            if let Some(existing) = self
                                .impl_body_plan
                                .symbols
                                .insert(declaration.clone(), symbol.clone())
                            {
                                if existing != symbol {
                                    self.impl_body_plan.symbols.remove(declaration);
                                    self.diagnostics.push(HirDiagnostic::new(
                                        HirDiagnosticKind::CheckerBoundaryViolation {
                                            name: format!(
                                                "impl body `{}`",
                                                declaration.full_path()
                                            ),
                                            reason: format!(
                                                "conflicting pre-lowering symbols `{existing}` and `{symbol}`"
                                            ),
                                        },
                                        span.clone(),
                                        "one materialized trait default selected two distinct emitted-body symbols",
                                    ));
                                }
                            }
                        }
                        // Deliberately NOT recorded into `root_item_ids`: the body
                        // was copied from the trait declaration (see
                        // `trait_method_to_fn_decl`), so its span indexes the
                        // trait's source — the root file only when the trait is
                        // itself root-local. Excluding all generated default-method
                        // bodies degrades to a bare fail-closed line (never a false
                        // caret against the root source). This is the exact producer
                        // an absence-from-a-foreign-set proxy misclassified.
                        let Some(hir_method) = self.lower_fn_with_name_and_impl_params(
                            &fn_decl,
                            &symbol,
                            span.clone(),
                            &type_params,
                            Some(&symbol_self_name),
                            synthetic_default_declaration.clone(),
                        ) else {
                            continue;
                        };
                        if let Some(declaration) = &synthetic_default_declaration {
                            if self.validate_impl_body_plan(declaration, &symbol, &span) {
                                self.impl_method_body_symbols
                                    .entry(declaration.clone())
                                    .or_insert_with(|| symbol.clone());
                            }
                        }
                        method_item_ids.push(hir_method.id);
                        items.push(HirItem::Function(hir_method));
                        method_symbols.push(symbol.clone());
                        method_names.push(fn_decl.name.clone());
                        let declaring_trait =
                            ids.as_ref().map_or(declaring_trait, |(trait_id, _)| {
                                trait_id.full_path().to_string()
                            });
                        method_declaring_trait_ids
                            .push(ids.as_ref().map(|(trait_id, _)| trait_id.clone()));
                        method_trait_method_ids
                            .push(ids.as_ref().map(|(_, method_id)| method_id.clone()));
                        method_ids.push(
                            self.impl_method_declaration_ids
                                .get(&symbol)
                                .cloned()
                                .or(synthetic_default_declaration),
                        );
                        method_declaring_traits.push(declaring_trait);
                    }
                    self.current_module_idx = saved_module_idx;
                    self.current_module_name = saved_module;
                }
            }
        }

        self.current_impl_self_ty = prior_self_ty;

        // Lower associated-type bindings to `ResolvedTy`s. Recorded as
        // metadata only — no runtime artefact (the type-erasure model in
        // use treats `type Item = T;` as a checker-only projection).
        let type_aliases: Vec<(String, ResolvedTy)> = decl
            .type_aliases
            .iter()
            .map(|alias| (alias.name.clone(), self.lower_type(&alias.ty)))
            .collect();

        items.push(HirItem::Impl(crate::node::HirImplBlock {
            id: self.ids.item(),
            node: self.ids.node(),
            trait_name: decl.trait_bound.as_ref().map(|b| b.name.clone()),
            self_type_name: hir_impl_self_type_name,
            self_type: impl_self_nominal,
            type_params,
            self_type_concrete_args,
            type_aliases,
            method_symbols,
            method_names,
            method_declaring_traits,
            method_declaring_trait_ids,
            method_trait_method_ids,
            method_ids,
            method_item_ids,
            span,
        }));
    }

    pub(super) fn lower_fn_with_name(
        &mut self,
        func: &FnDecl,
        name: &str,
        span: std::ops::Range<usize>,
    ) -> Option<HirFn> {
        self.lower_fn_with_name_and_impl_params(func, name, span, &[], None, None)
    }

    /// Lower an imported actor under its checker's current module and file scope.
    pub(super) fn lower_imported_actor(
        &mut self,
        decl: &ActorDecl,
        span: Span,
        module_full_path: &str,
    ) -> Option<HirActorDecl> {
        let lowered = self.lower_actor(decl, span, Some(module_full_path));
        let mut lowered = lowered?;
        // Owner-qualify each receive handler's return type to the declaring
        // module (`testffi.Result`) ONLY when the returned record's bare name
        // genuinely collides across modules — the same collision the MIR
        // record-layout keying (`type_layout_key` / `collided_type_names`) uses
        // to key the layout under the qualified identity. Both the ask-reply
        // type and this handler-layout return type must then agree on that
        // qualified identity, or `lower_actor_ask` fails closed and codegen
        // sees a bare value against a qualified layout struct (#2208). A
        // non-colliding record (stdlib `xml.Node`, `http.Response`) keeps its
        // bare identity — its layout stays bare too, so qualifying here would
        // instead CREATE a mismatch.
        for handler in &mut lowered.receive_handlers {
            handler.return_ty = self.qualify_current_module_record_ty(handler.return_ty.clone());
            handler.return_ty =
                self.qualify_colliding_module_record_ty(&handler.return_ty, module_full_path);
        }
        Some(lowered)
    }

    /// Extract the declaring-module short segment from an imported actor's
    /// method id (`{module}.{Actor}::{method}` → `module`). Returns `None` for
    /// a bare/root actor (`Actor::method`, no leading module segment), which
    /// carries no module identity to qualify against.
    pub(super) fn actor_module_short_of_method_id(method_id: &str) -> Option<&str> {
        let actor_identity = method_id.split("::").next()?;
        actor_identity
            .rsplit_once('.')
            .map(|(module, _actor)| module)
    }

    pub(super) fn qualify_imported_actor_method_id(&self, method_id: String) -> String {
        let Some((actor, method)) = method_id.rsplit_once("::") else {
            return method_id;
        };
        if actor.contains('.') {
            return method_id;
        }
        self.imported_actor_rewrites
            .as_ref()
            .and_then(|rewrites| rewrites.get(actor))
            .map_or(method_id.clone(), |qualified| {
                format!("{qualified}::{method}")
            })
    }

    /// Qualify a bare user-record type reference to `{module_short}.{name}` when
    /// that record's bare name genuinely collides across modules AND the named
    /// module declares it, recursing through generic arguments. Builtins,
    /// `#[opaque]` handles, already-qualified names, and non-colliding records
    /// are returned unchanged. The collision gate keeps this aligned with the
    /// MIR record-layout keying: only a colliding record is keyed by its
    /// qualified identity, so only its value-flow (actor handler return / ask
    /// reply) must carry the same identity (#2208).
    pub(super) fn qualify_colliding_module_record_ty(
        &self,
        ty: &ResolvedTy,
        module_full_path: &str,
    ) -> ResolvedTy {
        let ResolvedTy::Named {
            name,
            args,
            builtin,
            is_opaque,
        } = ty
        else {
            return ty.clone();
        };
        let args = args
            .iter()
            .map(|arg| self.qualify_colliding_module_record_ty(arg, module_full_path))
            .collect();
        if builtin.is_some()
            || *is_opaque
            || name.contains('.')
            || !self.cross_module_colliding_record_names.contains(name)
        {
            return ResolvedTy::Named {
                name: name.clone(),
                args,
                builtin: *builtin,
                is_opaque: *is_opaque,
            };
        }
        let qualified = format!("{module_full_path}.{name}");
        if self.record_registry.contains_key(&qualified) {
            return ResolvedTy::named_user(qualified, args);
        }
        ResolvedTy::Named {
            name: name.clone(),
            args,
            builtin: *builtin,
            is_opaque: *is_opaque,
        }
    }

    /// Lower ordinary imported bodies and retain callable memory-floor stubs.
    /// Checker-admitted semantic runtime operations have no source body: their
    /// calls already carry a typed runtime family. Verify the same signature
    /// contract before suppressing them, so inconsistent checker/HIR facts
    /// cannot silently discard an ordinary function.
    pub(super) fn lower_imported_fn_floor_aware(
        &mut self,
        func: &FnDecl,
        qualified: &str,
        source_module: &str,
        span: std::ops::Range<usize>,
    ) -> Option<HirFn> {
        let source_key = format!("{source_module}.{}", func.name);
        let Some(intrinsic_key) = self.intrinsic_declarations.get(&source_key).cloned() else {
            return self.lower_fn_with_name(func, qualified, span);
        };
        let Some(entry) = crate::stdlib_catalog::entries()
            .iter()
            .find(|e| e.name == intrinsic_key)
        else {
            self.diagnostics.push(HirDiagnostic::new(
                HirDiagnosticKind::UnknownIntrinsic {
                    fn_name: func.name.clone(),
                    intrinsic_key,
                },
                span,
                "intrinsic key not found in stdlib catalogue; \
                 check the #[intrinsic(\"..\")] argument matches a catalogue entry name",
            ));
            return None;
        };
        if let Some(family) =
            hew_types::runtime_call::RuntimeCallFamily::from_catalog_endpoint(&intrinsic_key)
        {
            if let Some(contract) = family.semantic_contract() {
                let matches = self.fn_registry.get(qualified).is_some_and(|signature| {
                    signature
                        .type_params
                        .iter()
                        .map(String::as_str)
                        .eq(family.source_intrinsic_type_params().iter().copied())
                        && !func.is_generator
                        // A parameter is consumed exactly when the contract moves it.
                        && func.params.len() == contract.arguments.len()
                        && func.params.iter().zip(contract.arguments).all(|(param, argument)| {
                            !param.is_mutable
                                && param.is_consume
                                    == (argument.effect
                                        == hew_types::runtime_call::RuntimeArgumentEffect::Move)
                        })
                        && family
                            .source_intrinsic_declaration()
                            .is_none_or(|expected| expected == source_key)
                        && contract.matches_signature(&signature.param_tys, &signature.return_ty)
                });
                if !matches {
                    self.diagnostics.push(HirDiagnostic::new(
                        HirDiagnosticKind::CheckerBoundaryViolation {
                            name: source_key,
                            reason: format!("intrinsic `{intrinsic_key}` signature differs from its semantic runtime contract"),
                        },
                        span,
                        "cannot suppress an imported floor body with inconsistent signature facts",
                    ));
                }
                return None;
            }
        }
        match entry.linkage {
            crate::stdlib_catalog::BuiltinLinkage::CalleeNameDispatchOnly => {
                let mut lowered = self.lower_fn_with_name(func, qualified, span)?;
                lowered.intrinsic_id = Some(intrinsic_key);
                Some(lowered)
            }
            _ => None,
        }
    }

    /// Variant of `lower_fn_with_name` that prepends impl-block-level type
    /// parameters into the lowered `HirFn::type_params`. Used by
    /// `lower_impl_block` so that methods of `impl<U> Trait for Wrapper<U>`
    /// carry `U` and can be monomorphized per concrete instantiation.
    ///
    /// The combined `type_params` is `impl_type_params ++ method.type_params`,
    /// matching how `Wrapper<U>::show` is logically `fn show<U>(w: Wrapper<U>) -> ...`
    /// once the impl-level binder is flattened into the method.
    pub(super) fn lower_fn_with_name_and_impl_params(
        &mut self,
        func: &FnDecl,
        name: &str,
        span: std::ops::Range<usize>,
        impl_type_params: &[String],
        impl_self_type_name: Option<&str>,
        known_declaration: Option<hew_types::DefId>,
    ) -> Option<HirFn> {
        // Use the stable ItemId pre-allocated during the first pass.
        let id = self
            .fn_registry
            .get(name)
            .map_or_else(|| self.ids.item(), |entry| entry.id);
        // Keep the checker/source declaration identity distinct from `name`:
        // imported free functions emit a linker-safe mangled symbol, while
        // direct-call targets retain the dotted source path. Impl methods are
        // already checker-published under their emitted symbol; ordinary
        // functions use the exact defining-module context active while their
        // source body is lowered, never a reverse parse of the linker name.
        // A materialised trait default has no source `fn` in the impl AST, so
        // the checker never inventoried one: its identity is minted at the
        // synthesis boundary and handed in here. Everything else resolves
        // through the checker's own tables.
        let declaration = if let Some(declaration) = known_declaration {
            declaration
        } else if let Some(declaration) = self.impl_method_declaration_ids.get(name) {
            declaration.clone()
        } else {
            self.source_declaration(&span, hew_types::DeclarationKind::Function, 0)?
        };

        self.push_scope();
        // Track this function's declared type parameters for the duration of
        // its body so lowering can recognise abstract-`T` operands (the
        // checker represents `T` as `ResolvedTy::Named { args: [] }`). Restored
        // on every return path below.
        let prior_fn_type_params = std::mem::replace(
            &mut self.current_fn_type_params,
            Self::concat_type_params(impl_type_params, func)
                .into_iter()
                .collect(),
        );
        let mut params = Vec::new();
        for (index, param) in func.params.iter().enumerate() {
            let mut binding = self.bind_param(param);
            binding.is_consume |= index == 0 && func.consumes_self;
            params.push(binding);
        }

        let source_return_ty = func
            .return_type
            .as_ref()
            .map_or(ResolvedTy::Unit, |ty| self.lower_type(ty));
        // A `gen fn`'s declared `-> T` is the yield element type, NOT the body's
        // return type. The body falls off the end (Unit) and the function value
        // is `Generator<Yield = T, Return = Unit>` — matching the checker's
        // registered signature (`Ty::generator(declared, Unit)`,
        // registration.rs). The body lowers through the same gen-body path the
        // `gen { ... }` block expression uses (`lower_gen_block`), so MIR/codegen
        // construction (`Terminator::Yield`, state-record synthesis) is shared.
        if func.is_generator {
            let (body, generator_ty) = self.lower_generator_fn_body(func, source_return_ty, &span);
            self.pop_scope();
            self.current_fn_type_params = prior_fn_type_params;
            return Some(HirFn {
                id,
                node: self.ids.node(),
                declaration,
                name: name.to_string(),
                type_params: Self::concat_type_params(impl_type_params, func),
                params,
                var_self_receiver: None,
                terminal_receiver: None,
                return_ty: generator_ty,
                body,
                span,
                is_generator: true,
                intrinsic_id: None,
            });
        }
        // Same predicate AND same name reduction as
        // `register_impl_method_fn_entry`: the fn-registry entry for `name`
        // already carries the dual-return `(ret, Self)` tuple when this fires,
        // so the wrapped body below is what makes the emitted return type
        // match the ABI every call site was told about.
        let bare_self_type_name = impl_self_type_name.map(Self::bare_impl_self_type_name);
        let var_self_receiver = if Self::is_var_self_method_for_type(func, bare_self_type_name) {
            params.first().cloned()
        } else {
            None
        };
        let mut body = self.with_current_return_type(source_return_ty.clone(), |ctx| {
            let mut body = ctx.lower_block(&func.body, &source_return_ty);
            if let Some(annotation) = func.return_type.as_ref().filter(|annotation| {
                ctx.result_return_coercions
                    .contains_key(&ctx.mk_key(&annotation.1))
            }) {
                let value = ctx.make_unit_expr(annotation.1.clone());
                body.tail = Some(Box::new(ctx.wrap_tail_ok(value, &annotation.1)));
            }
            body
        });
        let return_ty = if let Some(receiver) = &var_self_receiver {
            let abi_return_ty =
                Self::var_self_dual_return_ty(source_return_ty.clone(), receiver.ty.clone());
            self.wrap_var_self_function_returns(&mut body, receiver, &abi_return_ty);
            abi_return_ty
        } else {
            source_return_ty
        };
        self.pop_scope();
        self.current_fn_type_params = prior_fn_type_params;

        let terminal_receiver = params
            .first()
            .filter(|_| self.consuming_inherent_methods.contains(&declaration))
            .map(|parameter| parameter.id);
        Some(HirFn {
            id,
            node: self.ids.node(),
            declaration,
            name: name.to_string(),
            type_params: Self::concat_type_params(impl_type_params, func),
            params,
            var_self_receiver: var_self_receiver.map(|receiver| receiver.id),
            terminal_receiver,
            return_ty,
            body,
            span,
            is_generator: false,
            intrinsic_id: None,
        })
    }

    /// Concatenate impl-level and method-level type parameters for a lowered
    /// function. A method MAY shadow an impl-level type param name; that is a
    /// checker-level concern, so here we just concatenate.
    pub(super) fn concat_type_params(impl_type_params: &[String], func: &FnDecl) -> Vec<String> {
        let method_type_params: Vec<String> = func
            .type_params
            .as_ref()
            .map(|params| params.iter().map(|param| param.name.clone()).collect())
            .unwrap_or_default();
        let mut type_params: Vec<String> =
            Vec::with_capacity(impl_type_params.len() + method_type_params.len());
        type_params.extend(impl_type_params.iter().cloned());
        type_params.extend(method_type_params);
        type_params
    }

    /// Lower a `gen fn` body into a `(HirBlock, Generator<Yield, Return>)` pair.
    ///
    /// The declared `-> T` is the Yield element type, NOT the body return type:
    /// the body falls off the end (Unit) and the function value is
    /// `Generator<Yield = T, Return = Unit>`, matching the checker's registered
    /// signature (`Ty::generator(declared, Unit)`, registration.rs). The body
    /// lowers through the same `GenBlock` path the `gen { ... }` block expression
    /// uses, so MIR/codegen construction (`Terminator::Yield`, state-record
    /// synthesis) is shared between both surfaces.
    ///
    /// Caller owns the surrounding `push_scope`/`pop_scope`; this only lowers the
    /// body and wraps it. The returned block is a thin tail-only block whose tail
    /// is the `GenBlock` expression.
    pub(super) fn lower_generator_fn_body(
        &mut self,
        func: &FnDecl,
        source_return_ty: ResolvedTy,
        span: &std::ops::Range<usize>,
    ) -> (HirBlock, ResolvedTy) {
        let yield_ty = source_return_ty;
        let gen_return_ty = ResolvedTy::Unit;
        // Snapshot the enclosing scope (param scope on top) BEFORE lowering the
        // body so the body's own `let`/`var` bindings are excluded and only the
        // gen-fn's formal parameters (and any enclosing locals) are capture
        // candidates. See `collect_gen_captures`.
        let outer_bindings = self.visible_outer_bindings();
        self.generator_yield_tys.push(yield_ty.clone());
        let gen_body = self.with_current_return_type(gen_return_ty.clone(), |ctx| {
            ctx.lower_block(&func.body, &gen_return_ty)
        });
        self.generator_yield_tys.pop();
        let captures = Self::collect_gen_captures(&gen_body, &outer_bindings);

        let generator_ty = ResolvedTy::Named {
            name: "Generator".to_string(),
            args: vec![yield_ty.clone(), gen_return_ty.clone()],
            builtin: Some(hew_types::BuiltinType::Generator),
            is_opaque: false,
        };
        let gen_block_expr = HirExpr {
            node: self.ids.node(),
            site: self.ids.site(),
            ty: generator_ty.clone(),
            intent: IntentKind::Read,
            kind: HirExprKind::GenBlock {
                body: gen_body,
                yield_ty,
                return_ty: gen_return_ty,
                captures,
            },
            span: span.clone(),
        };
        let body = HirBlock {
            node: self.ids.node(),
            scope: self.ids.scope(),
            statements: Vec::new(),
            tail: Some(Box::new(gen_block_expr)),
            ty: generator_ty.clone(),
            span: span.clone(),
        };
        (body, generator_ty)
    }

    /// Project a checker-owned implementation declaration onto the one HIR
    /// function symbol that was registered to emit its body.  The declaration
    /// id is authoritative; a surface spelling such as `Foo::apply` is only a
    /// compatibility presentation and must never be used to rediscover an
    /// imported implementation.
    ///
    /// The projection is populated only when HIR materialises the exact body.
    /// Registry presence is deliberately insufficient: the checker retains
    /// compatibility aliases for methods HIR can skip, and a same-spelling
    /// method may be owned by a different flattened-file or package module.
    /// A caller that is lowered before its selected body is emitted must remain
    /// unresolved until lowering order establishes that body evidence.
    pub(super) fn registered_impl_method_symbol(
        &self,
        declaration: &hew_types::DefId,
    ) -> Option<String> {
        self.impl_method_body_symbols
            .get(declaration)
            .cloned()
            .or_else(|| self.impl_body_plan.symbols.get(declaration).cloned())
    }

    /// Verify that actual emission fulfils the declaration-keyed body plan.
    /// This remains a release-mode boundary diagnostic: an optimised compiler
    /// must fail closed rather than publish a wrong `DefId -> symbol` map.
    pub(super) fn validate_impl_body_plan(
        &mut self,
        declaration: &hew_types::DefId,
        symbol: &str,
        span: &Span,
    ) -> bool {
        let expected = self.impl_body_plan.symbols.get(declaration).cloned();
        if expected.as_deref() == Some(symbol) {
            return true;
        }
        self.diagnostics.push(HirDiagnostic::new(
            HirDiagnosticKind::CheckerBoundaryViolation {
                name: format!("impl body `{}`", declaration.full_path()),
                reason: expected.map_or_else(
                    || "no pre-lowering emitted-body plan".to_string(),
                    |expected| format!("planned symbol `{expected}`, emitted `{symbol}`"),
                ),
            },
            span.clone(),
            "implementation body emission diverged from the declaration-keyed body plan",
        ));
        false
    }

    /// The single eligibility authority for imported impl bodies.  The
    /// pre-lowering body plan and the actual imported-module emitter both use
    /// this result, so a first-pass signature can never masquerade as an
    /// executable body for a method this gate skips.
    pub(super) fn imported_impl_skip_methods(
        &self,
        impl_decl: &hew_parser::ast::ImplDecl,
        source_module: &str,
    ) -> HashSet<String> {
        let TypeExpr::Named {
            name: self_type_name,
            ..
        } = &impl_decl.target_type.0
        else {
            return impl_decl
                .methods
                .iter()
                .map(|method| method.name.clone())
                .collect();
        };
        let impl_generic_params: HashSet<String> = impl_decl
            .type_params
            .as_ref()
            .map(|tps| tps.iter().map(|tp| tp.name.clone()).collect())
            .unwrap_or_default();
        let mut skip_methods: HashSet<String> = HashSet::new();
        for method in &impl_decl.methods {
            let callable_params: HashSet<&str> = method
                .params
                .iter()
                .filter(|param| matches!(&param.ty.0, TypeExpr::Function { .. }))
                .map(|param| param.name.as_str())
                .collect();
            let body_unresolvable =
                collect_all_bare_call_names(&method.body)
                    .into_iter()
                    .any(|callee| {
                        !is_builtin_enum_variant_bare_name(&callee)
                            && self.resolved_bare_function_symbol(&callee).is_none()
                            && !self.fn_registry.contains_key(&callee)
                            && !callable_params.contains(callee.as_str())
                            && !stdlib_catalog::is_overloaded_builtin(&callee)
                    });
            let mut method_generic_params = impl_generic_params.clone();
            if let Some(tps) = &method.type_params {
                method_generic_params.extend(tps.iter().map(|tp| tp.name.clone()));
            }
            let is_known_registered_type = |name: &str| {
                self.enum_variants_by_name.contains_key(name)
                    || self.type_classes.contains_key(name)
                    || self.record_registry.contains_key(name)
                    || self
                        .source_type_identities
                        .contains(&format!("{source_module}.{name}"))
                    || name.rsplit_once('.').is_some_and(|(binding, item)| {
                        self.module_import_bindings
                            .get(&(
                                Some(source_module.to_string()),
                                self.current_module_idx,
                                binding.to_string(),
                            ))
                            .is_some_and(|owner| {
                                self.source_type_identities
                                    .contains(&format!("{owner}.{item}"))
                            })
                    })
            };
            let sig_unresolvable = method_signature_type_exprs(method).any(|ty| {
                !imported_impl_signature_type_is_safe(
                    ty,
                    self_type_name,
                    &method_generic_params,
                    &is_known_registered_type,
                )
            });
            if body_unresolvable || sig_unresolvable {
                skip_methods.insert(method.name.clone());
            }
        }
        loop {
            let mut grew = false;
            for method in &impl_decl.methods {
                if skip_methods.contains(&method.name) {
                    continue;
                }
                if collect_all_method_call_names(&method.body)
                    .iter()
                    .any(|callee| skip_methods.contains(callee))
                {
                    skip_methods.insert(method.name.clone());
                    grew = true;
                }
            }
            if !grew {
                break;
            }
        }
        skip_methods
    }

    /// Allocate the direct-body identity for a trait default materialised in a
    /// concrete impl. The trait method `DefId` remains the static-dispatch key;
    /// this synthetic ID names the distinct body HIR emits for one selected
    /// `(trait, self-type, method)` tuple.
    ///
    /// This is intentionally constructed only at the default-body synthesis
    /// boundary. It uses the carried declaration/type structures directly and
    /// never parses a linker symbol or leaf method spelling back into an owner.
    pub(super) fn synthetic_default_impl_body_declaration(
        declaring_trait: &hew_types::DefId,
        self_ty: Option<&ResolvedTy>,
        method: &str,
    ) -> Option<hew_types::DefId> {
        let self_ty = self_ty?;
        let instance = self_ty.impl_receiver_instance()?;
        Some(hew_types::default_impl_method_declaration(
            declaring_trait,
            &instance,
            method,
        ))
    }
}
