//! Type declaration, type reference and enum instantiation lowering.

use super::*;

impl LowerCtx {
    pub(super) fn lower_type_decl(&mut self, decl: &TypeDecl, span: Span) -> Option<HirTypeDecl> {
        let declaration = self.source_declaration(
            &span,
            if decl.origin == hew_parser::ast::DeclarationOrigin::MachineState {
                hew_types::DeclarationKind::Machine
            } else {
                hew_types::DeclarationKind::Type
            },
            0,
        )?;
        self.lower_type_decl_with_identity(decl, span, declaration)
    }

    #[expect(
        clippy::too_many_lines,
        reason = "declaration lowering keeps checker classification, scoped fields and variants together"
    )]
    pub(super) fn lower_type_decl_with_identity(
        &mut self,
        decl: &TypeDecl,
        span: Span,
        declaration: hew_types::DefId,
    ) -> Option<HirTypeDecl> {
        let definition = self.checked_member_definition(declaration, &span)?;
        let facts = self
            .type_declarations
            .get(self.defs.path(declaration))
            .cloned()
            .unwrap_or_else(|| {
                self.diagnostics.push(HirDiagnostic::new(
                    HirDiagnosticKind::CheckerBoundaryViolation {
                        name: self.defs.path(declaration).to_string(),
                        reason: "missing declared type facts".to_string(),
                    },
                    span.clone(),
                    "type declaration reached HIR without checker classification",
                ));
                hew_types::value_class::DeclaredType::default()
            });
        let marker = match facts.marker {
            hew_types::value_class::DeclarationMarker::Resource => ResourceMarker::Resource,
            hew_types::value_class::DeclarationMarker::Linear => ResourceMarker::Linear,
            hew_types::value_class::DeclarationMarker::None if facts.is_opaque => {
                ResourceMarker::BitCopy
            }
            hew_types::value_class::DeclarationMarker::None => ResourceMarker::None,
        };
        // The ownership map currently keys resource/linear declarations by nominal identity.
        if matches!(marker, ResourceMarker::Resource | ResourceMarker::Linear)
            && !facts.type_params.is_empty()
        {
            self.diagnostics.push(HirDiagnostic::new(
                HirDiagnosticKind::ResourceGenericUnsupported {
                    name: decl.name.to_string(),
                },
                span.clone(),
                "`#[resource]` / `#[linear]` types cannot have type parameters in v0.5",
            ));
        }

        match marker {
            ResourceMarker::Resource => {
                self.check_resource_close_discipline(decl, &span, declaration);
            }
            ResourceMarker::Linear => {
                self.check_linear_consume_discipline(decl, &span, declaration);
            }
            ResourceMarker::None | ResourceMarker::BitCopy => {}
        }

        let fields = decl
            .body
            .iter()
            .filter_map(|item| {
                let TypeBodyItem::Field {
                    name,
                    span: field_span,
                    ..
                } = item
                else {
                    return None;
                };
                Some(HirField {
                    name: name.to_string(),
                    ty: self.checked_field_ty(&definition, name.name.as_str(), field_span),
                    default: None,
                    is_mutable: false,
                    deferred: false,
                    span: field_span.clone(),
                })
            })
            .collect::<Vec<_>>();
        let variants = decl
            .body
            .iter()
            .filter_map(|item| {
                let TypeBodyItem::Variant(variant) = item else {
                    return None;
                };
                Some(HirVariant {
                    name: variant.name.to_string(),
                    kind: self.checked_variant_kind(
                        &definition,
                        variant.name.name.as_str(),
                        &span,
                    )?,
                })
            })
            .collect();

        // Register concrete generic-enum layouts from stored field types even
        // when no expression constructs the enclosing value. Codegen emits a
        // declared type's layout unconditionally, so expression-only discovery
        // otherwise leaves declaration-only `Option<T>` fields without a layout.
        // This can go away when monomorphization owns layout discovery globally.
        for field in &fields {
            self.try_register_enum_instantiation_ty(&field.ty, &field.span);
        }

        // Reuse the stable ItemId pre-allocated during the record/
        // type-decl pre-pass. Falling back to a fresh id keeps
        // the path safe if the pre-pass ever skips a decl.
        let id = self
            .record_registry
            .get(self.defs.path(declaration))
            .map_or_else(|| self.ids.item(), |entry| entry.id);
        let type_params = definition.type_params;
        Some(HirTypeDecl {
            kind: match decl.kind {
                TypeDeclKind::Struct => HirTypeDeclKind::Struct,
                TypeDeclKind::Enum => HirTypeDeclKind::Enum,
            },
            id,
            node: self.ids.node(),
            declaration,
            name: decl.name.to_string(),
            // Root/local identity by default; the imported-module carrier
            // (`lower_imported_type_decl`) stamps `Some(module_short)` for
            // package-exported types.
            defining_module: None,
            marker,
            is_opaque: facts.is_opaque,
            is_indirect: decl.is_indirect,
            consuming_methods: decl
                .consuming_methods
                .iter()
                .map(ToString::to_string)
                .collect(),
            type_params,
            fields,
            variants,
            span,
        })
    }

    /// Lower a `record` declaration into `HirRecordDecl`.
    ///
    /// Only named-form records produce a named field list. Tuple-form records
    /// keep `HirRecordDecl.fields` empty because their constructor is a `Call`
    /// (`R(a, b)`) rather than a `StructInit` (`R { x: a, y: b }`), while
    /// `HirRecordDecl.positional_field_tys` preserves the stored payload shape
    /// for downstream layout and clone/drop classification.
    pub(super) fn lower_record_decl(
        &mut self,
        decl: &RecordDecl,
        span: std::ops::Range<usize>,
    ) -> Option<HirRecordDecl> {
        let declaration = self.source_declaration(&span, hew_types::DeclarationKind::Record, 0)?;
        let definition = self.checked_member_definition(declaration, &span)?;
        let type_params = definition.type_params.clone();
        let (fields, positional_field_tys) = match &decl.kind {
            RecordKind::Named(record_fields) => (
                record_fields
                    .iter()
                    .map(|field| HirField {
                        name: field.name.to_string(),
                        ty: self.checked_field_ty(
                            &definition,
                            field.name.name.as_str(),
                            &field.span,
                        ),
                        default: None,
                        is_mutable: false,
                        deferred: false,
                        span: field.span.clone(),
                    })
                    .collect(),
                Vec::new(),
            ),
            RecordKind::Tuple(_) => {
                let Some(signature) = self
                    .fn_sigs_by_path
                    .get(self.defs.path(declaration))
                    .cloned()
                else {
                    self.diagnostics.push(HirDiagnostic::new(
                        HirDiagnosticKind::CheckerBoundaryViolation {
                            name: self.defs.path(declaration).to_string(),
                            reason: "missing checked positional constructor signature".to_string(),
                        },
                        span.clone(),
                        "tuple record reached HIR without positional member facts",
                    ));
                    return None;
                };
                (
                    Vec::new(),
                    signature
                        .params
                        .iter()
                        .map(|ty| self.checked_member_ty(ty, &definition.type_params, &span))
                        .collect(),
                )
            }
        };

        // Reuse the stable ItemId pre-allocated during the record/
        // type-decl pre-pass. Falling back to a fresh id keeps
        // the path safe if the pre-pass ever skips a decl.
        let id = self
            .record_registry
            .get(self.defs.path(declaration))
            .map_or_else(|| self.ids.item(), |entry| entry.id);
        Some(HirRecordDecl {
            id,
            node: self.ids.node(),
            declaration,
            name: decl.name.to_string(),
            // Imported emission stamps the declaration's source module.
            defining_module: None,
            type_params,
            positional_field_tys,
            fields,
            span,
        })
    }

    /// Lower a type declared in an imported package module.
    ///
    /// Identical to [`lower_type_decl`](Self::lower_type_decl) except that the
    /// resulting `HirTypeDecl` carries `defining_module = Some(module_short)` —
    /// the `(defining-module, name)` identity (mirroring
    /// [`lower_imported_actor`](Self::lower_imported_actor)) that lets MIR
    /// layout keys and codegen symbols distinguish two same-named types from
    /// different modules. The decl `name` stays bare; switching keys/symbols to
    /// `qualified_name()` is the downstream re-key this carrier enables.
    pub(super) fn lower_imported_type_decl(
        &mut self,
        decl: &TypeDecl,
        span: std::ops::Range<usize>,
        module_name: &str,
    ) -> Option<HirTypeDecl> {
        let mut lowered = self.lower_type_decl(decl, span)?;
        lowered.defining_module = Some(module_name.to_string());
        Some(lowered)
    }

    /// Omitted `ActorError` parameters default to `Never`, as they do in the
    /// checker. Both stages preserve the same concrete envelope identity.
    pub(super) fn resolve_named_type_ref(&self, name: &str, args: Vec<ResolvedTy>) -> ResolvedTy {
        let mut resolved = self.resolve_named_type_ref_inner(name, args);
        if let ResolvedTy::Named { head, args, .. } = &mut resolved {
            if *head == hew_types::KnownDecl::ActorError.head() {
                args.resize_with(args.len().max(2), || {
                    ResolvedTy::named_user(
                        hew_types::NominalHead::new(
                            hew_types::KnownDecl::Never.nominal(),
                            hew_types::KnownDecl::Never.path(),
                        ),
                        Vec::new(),
                    )
                });
            }
        }
        resolved
    }

    #[allow(
        clippy::too_many_lines,
        reason = "single match over every TypeExpr variant; splitting would scatter the type-lowering authority"
    )]
    /// Resolve a non-keyword `TypeExpr::Named` reference to a `ResolvedTy`,
    /// classifying it as builtin / opaque-handle / user type. `name` is the
    /// annotation as written (possibly module-qualified); `args` are the
    /// already-lowered generic arguments. Split out of `lower_type` to keep
    /// that dispatcher under the line budget.
    pub(super) fn resolve_named_type_ref_inner(
        &self,
        name: &str,
        args: Vec<ResolvedTy>,
    ) -> ResolvedTy {
        if let Some(checked) = self.checked_encoding_type(name, &args) {
            return checked;
        }
        let type_name = hew_types::short_name(name);
        let current_module_is_file_import = self
            .current_module_name
            .as_deref()
            .is_some_and(|module| self.file_import_module_names.contains(module));
        // Root-visible source declarations outrank the builtin catalog. A user
        // `#[opaque] type Receiver {}` is a
        // distinct nominal resource, not the std channel endpoint merely
        // because the short spelling collides.
        if self.current_module_name.is_none()
            && !name.contains('.')
            && self.declared_type_is_opaque(name)
        {
            return ResolvedTy::named_opaque_path(&self.defs, name, args);
        }
        if !name.contains('.') {
            if let Some(module_owner) = self.current_module_name.as_deref() {
                let qualified = format!("{module_owner}.{name}");
                if let Some(checked) = self.checked_encoding_type(&qualified, &args) {
                    return checked;
                }
                // Several compiler carriers (notably Stream/Sink and lifecycle
                // payloads) are non-opaque source declarations. Recover their
                // builtin identity only while lowering a canonical `std.*`
                // module. An arbitrary `acme.stream.Sink<T>` has the same leaf
                // qualifier but is outside this authority and stays nominal.
                if self
                    .current_module_name
                    .as_deref()
                    .is_some_and(|module| module.starts_with("std."))
                {
                    if let Some(builtin) = self.qualified_source_builtin(&qualified) {
                        return Self::resolved_source_builtin_ty(&qualified, builtin, args);
                    }
                }
                if self.declared_type_is_opaque(&qualified) {
                    if let Some(builtin) = self.qualified_source_builtin(&qualified) {
                        return Self::resolved_source_builtin_ty(&qualified, builtin, args);
                    }
                    return ResolvedTy::named_opaque_path(&self.defs, &qualified, args);
                }
            }
        }

        // A whole-module lifecycle alias is canonical only when the checker
        // proved that its import resolves to the shipped std source. Consume
        // the exact TypeCheckOutput fact before treating this qualified
        // spelling as an ordinary user nominal; module spelling alone is not
        // lifecycle authority.
        if name.contains('.') {
            if let Some(canonical) = self
                .import_type_name_aliases
                .get(&(
                    self.current_module_name.clone(),
                    self.current_module_idx,
                    name.to_string(),
                ))
                .cloned()
            {
                if canonical != name {
                    return self.resolve_named_type_ref(&canonical, args);
                }
            }
        }

        // Whole-module imports carry a lexical qualifier, not a declaration
        // identity.  Rewrite it through the checker-owned owner map before
        // generic record/enum layout registration observes the type.  This
        // preserves every path segment (`lmonobox.Box` →
        // `hew.lmonobox.Box`) and deliberately does not use a leaf fallback.
        if let Some((binding, tail)) = name.split_once('.') {
            if let Some(owner) = self.module_import_bindings.get(&(
                self.current_module_name.clone(),
                self.current_module_idx,
                binding.to_string(),
            )) {
                let canonical = format!("{owner}.{tail}");
                if canonical != name {
                    return self.resolve_named_type_ref(&canonical, args);
                }
            }
        }

        // A bare name inside its defining module canonicalises to that module's
        // exact identity. Known std carrier identities stay builtin; a declared
        // opaque user identity keeps its full qualified name.
        let canonical = self.canonical_current_module_record_name(name);
        if canonical != name {
            if let Some(checked) = self.checked_encoding_type(&canonical, &args) {
                return checked;
            }
            if let Some(builtin) = self.qualified_source_builtin(&canonical) {
                return Self::resolved_source_builtin_ty(&canonical, builtin, args);
            }
            if self.declared_type_is_opaque(&canonical) {
                return ResolvedTy::named_opaque_path(&self.defs, &canonical, args);
            }
            return ResolvedTy::named_path(&self.defs, &canonical, args);
        }

        // A declaration authored in the current source scope outranks both an
        // imported binding and the builtin catalog, including when generic.
        // Do not use the global `record_registry` here: it contains every
        // imported record too and would turn `import std::failure::{
        // CrashInfo }` into a user type, losing the crash-hook ABI identity.
        if !name.contains('.')
            && self.current_scope_declares_source_type(name, current_module_is_file_import)
            && !self.declared_type_is_opaque(name)
        {
            return ResolvedTy::named_path(&self.defs, name, args);
        }

        // Named/glob imports are source bindings, not aliases only. Resolve
        // their bare spelling to the published qualified identity before any
        // builtin lookup so `import foo::{ Receiver }` cannot become the
        // runtime channel endpoint. This remains a fallback after the local
        // declaration check above: authored local types shadow imports.
        if !name.contains('.') {
            if let Some(canonical) = self
                .import_type_name_aliases
                .get(&(
                    self.current_module_name.clone(),
                    self.current_module_idx,
                    name.to_string(),
                ))
                .cloned()
            {
                return self.resolve_named_type_ref(&canonical, args);
            }
        }

        // A flat file import's declaration is spelled bare at root but its
        // identity is the defining file's `{module}.{name}` — the key every
        // layout registry uses. Project the bare annotation to that identity
        // before the global record-registry fallback can freeze the bare
        // spelling into a binding type MIR cannot look up.
        if !name.contains('.') && self.current_module_name.is_none() {
            if let Some(canonical) = self.file_import_root_type_aliases.get(name).cloned() {
                return self.resolve_named_type_ref(&canonical, args);
            }
        }

        // A global record registry is layout metadata, not lexical authority.
        // In particular, source-owned lifecycle payloads must reach this point
        // through the checker-published import binding above; their bare
        // catalog spelling must never become valid merely because another
        // module loaded the record declaration.
        if !name.contains('.')
            && self.record_registry.contains_key(name)
            && hew_types::lookup_source_owned_lifecycle_type(name).is_none()
            && crate::builtin_type_classes::builtin_type_registration(type_name).is_none()
            && !self.declared_type_is_opaque(name)
        {
            return ResolvedTy::named_path(&self.defs, name, args);
        }

        // Generated monomorphic enum annotations use the same exact source
        // identity as checker-authored expression facts and synthetic HIR
        // layouts. This runs only after local declarations and import bindings
        // have had their chance to win, so a user same-leaf enum remains user
        // owned and an unrelated qualified owner is never retried by leaf.
        let builtin_hint = crate::builtin_type_classes::builtin_type_registration(name)
            .map(|registration| registration.builtin)
            .or_else(|| hew_types::lookup_builtin_type(type_name));
        if let Some(canonical) = self.canonical_monomorphic_builtin_enum_name(
            name,
            builtin_hint,
            current_module_is_file_import,
        ) {
            return if let Some(builtin) = builtin_hint {
                ResolvedTy::named_builtin(builtin, args)
            } else {
                ResolvedTy::named_path(&self.defs, canonical, args)
            };
        }

        // Qualified inputs are resolved by exact identity only. The known std
        // spellings live in `lookup_builtin_type`; an arbitrary
        // `foo.Stream` must never inherit the bare `Stream` registration.
        if let Some(registration) = crate::builtin_type_classes::builtin_type_registration(name) {
            // A builtin classified by its own std declaration only has facts
            // under its canonical source identity. The prelude publishes the
            // bare spelling without an import alias, so mint that identity here
            // rather than handing the catalog's bare presentation name on.
            ResolvedTy::named_builtin(registration.builtin, args)
        } else if let Some(builtin) = self.qualified_source_builtin(name) {
            Self::resolved_source_builtin_ty(name, builtin, args)
        } else if name.contains('.') && self.declared_type_is_opaque(name) {
            // `#[opaque]` runtime handle (e.g. `json.Value`). Stamp the
            // type-identity discriminator so the actor-state clone/drop
            // classifier fails closed on the handle even when its short name
            // collides with a user record/enum of the same name. See
            // `LowerCtx::declared_type_is_opaque`.
            ResolvedTy::named_opaque_path(&self.defs, name, args)
        } else if name.contains('.') {
            // Module-qualified user type (`widgeti64.Widget`). Preserve the
            // full `{module}.{name}` identity so MIR layout keys and field
            // resolution distinguish two same-bare-name types from different
            // packages (the `i8` vs `i64` `Widget` collision). The MIR
            // `lookup_record_field_order` already strips the prefix on a miss,
            // so a layout registered under either the qualified or the bare
            // key still resolves; carrying the qualifier is what lets the
            // per-module layout (keyed by `HirTypeDecl::qualified_name()`) win
            // over the bare last-write-wins entry once MIR keys by it. A bare
            // reference (single-module program) keeps its short name unchanged.
            ResolvedTy::named_path(&self.defs, name, args)
        } else if let Some(registration) =
            crate::builtin_type_classes::builtin_type_registration(type_name)
        {
            ResolvedTy::named_builtin(registration.builtin, args)
        } else if let Some(builtin) = hew_types::lookup_builtin_type(type_name)
            .filter(|builtin| !builtin.requires_source_import())
        {
            ResolvedTy::named_builtin(builtin, args)
        } else if self.declared_type_is_opaque(name) {
            ResolvedTy::named_opaque_path(&self.defs, name, args)
        } else {
            ResolvedTy::named_path(&self.defs, type_name, args)
        }
    }

    /// Resolve a source declaration that uses one of the three spellings with
    /// dedicated HIR fallbacks (`Task`, `Unit`, or `CancellationToken`).
    ///
    /// This deliberately does not participate in general named-type
    /// resolution: reserved primitives and contextual `Self` keep their
    /// existing authority. It exists only so source identity wins before the
    /// audited early arms below, including for generic `Task<T>` / `Unit<T>`.
    pub(super) fn resolve_early_source_type_ref(
        &self,
        name: &str,
        args: Vec<ResolvedTy>,
    ) -> Option<ResolvedTy> {
        if name.contains('.') {
            return None;
        }

        if let Some(canonical) = self.import_type_name_aliases.get(&(
            self.current_module_name.clone(),
            self.current_module_idx,
            name.to_string(),
        )) {
            return Some(self.resolve_named_type_ref(canonical, args));
        }

        // A flat-file-imported declaration sharing a compiler-special leaf
        // (`Unit`, `Task`, `CancellationToken`) keeps source authority under
        // its qualified identity, same as the root-authored branch below.
        if self.current_module_name.is_none() {
            if let Some(canonical) = self.file_import_root_type_aliases.get(name).cloned() {
                return Some(self.resolve_named_type_ref(&canonical, args));
            }
        }

        if self.current_module_name.is_none()
            && self.root_visible_source_type_short_names.contains(name)
        {
            return Some(if self.declared_type_is_opaque(name) {
                ResolvedTy::named_opaque_path(&self.defs, name, args)
            } else {
                ResolvedTy::named_path(&self.defs, name, args)
            });
        }

        if let Some(module_owner) = self.current_module_name.as_deref() {
            let qualified = format!("{module_owner}.{name}");
            if self.source_type_identities.contains(&qualified) {
                if let Some(builtin) = self.qualified_source_builtin(&qualified) {
                    return Some(Self::resolved_source_builtin_ty(&qualified, builtin, args));
                }
                return Some(if self.declared_type_is_opaque(&qualified) {
                    ResolvedTy::named_opaque_path(&self.defs, &qualified, args)
                } else {
                    ResolvedTy::named_path(&self.defs, &qualified, args)
                });
            }
        }

        None
    }

    #[allow(
        clippy::too_many_lines,
        reason = "one recursive checker-to-HIR identity authority covers every ResolvedTy wrapper"
    )]
    pub(super) fn qualify_current_module_record_ty(&self, ty: ResolvedTy) -> ResolvedTy {
        let ty = match ty {
            ResolvedTy::Tuple(elements) => {
                return ResolvedTy::Tuple(
                    elements
                        .into_iter()
                        .map(|element| self.qualify_current_module_record_ty(element))
                        .collect(),
                );
            }
            ResolvedTy::Array(element, size) => {
                return ResolvedTy::Array(
                    Box::new(self.qualify_current_module_record_ty(*element)),
                    size,
                );
            }
            ResolvedTy::Slice(element) => {
                return ResolvedTy::Slice(Box::new(
                    self.qualify_current_module_record_ty(*element),
                ));
            }
            ResolvedTy::Function {
                capabilities,
                params,
                ret,
            } => {
                return ResolvedTy::Function {
                    capabilities,
                    params: params
                        .into_iter()
                        .map(|param| self.qualify_current_module_record_ty(param))
                        .collect(),
                    ret: Box::new(self.qualify_current_module_record_ty(*ret)),
                };
            }
            ResolvedTy::Closure {
                params,
                ret,
                captures,
                capabilities,
            } => {
                return ResolvedTy::Closure {
                    capabilities,
                    params: params
                        .into_iter()
                        .map(|param| self.qualify_current_module_record_ty(param))
                        .collect(),
                    ret: Box::new(self.qualify_current_module_record_ty(*ret)),
                    captures: captures
                        .into_iter()
                        .map(|capture| self.qualify_current_module_record_ty(capture))
                        .collect(),
                };
            }
            ResolvedTy::Pointer {
                is_mutable,
                pointee,
            } => {
                return ResolvedTy::Pointer {
                    is_mutable,
                    pointee: Box::new(self.qualify_current_module_record_ty(*pointee)),
                };
            }
            ResolvedTy::Borrow { pointee } => {
                return ResolvedTy::Borrow {
                    pointee: Box::new(self.qualify_current_module_record_ty(*pointee)),
                };
            }
            ResolvedTy::TraitObject { traits } => {
                return ResolvedTy::TraitObject {
                    traits: traits
                        .into_iter()
                        .map(|bound| ResolvedTraitBound {
                            trait_name: bound.trait_name,
                            args: bound
                                .args
                                .into_iter()
                                .map(|arg| self.qualify_current_module_record_ty(arg))
                                .collect(),
                            assoc_bindings: bound
                                .assoc_bindings
                                .into_iter()
                                .map(|(name, ty)| (name, self.qualify_current_module_record_ty(ty)))
                                .collect(),
                        })
                        .collect(),
                };
            }
            ResolvedTy::Task(result) => {
                return ResolvedTy::Task(Box::new(self.qualify_current_module_record_ty(*result)));
            }
            other => other,
        };
        let ResolvedTy::Named {
            head,
            args,
            is_opaque,
        } = ty
        else {
            return ty;
        };
        let args: Vec<ResolvedTy> = args
            .into_iter()
            .map(|arg| self.qualify_current_module_record_ty(arg))
            .collect();
        let name = head.registry_key();
        if let Some(expected) = head.builtin().filter(|kind| kind.is_encoding_value()) {
            if let Some(checked) = self
                .checked_encoding_type(name, &args)
                .filter(|ty| ty.is_builtin(expected))
            {
                return checked;
            }
        }
        // A resolved head already names its declaration; `Ty::Named` carries
        // no opacity bit, so restore it from the declaration registry here.
        let hew_types::TypeHead::Unresolved(_) = head else {
            let is_opaque = is_opaque || (head.is_user() && self.declared_type_is_opaque(name));
            return ResolvedTy::Named {
                head,
                args,
                is_opaque,
            };
        };
        // TRANSITION(A1): a spelling HIR's own type resolution left unresolved
        // is qualified to its declaration; deleted by B1 with that resolver.
        let name = name.to_string();
        let current_module_is_file_import = self
            .current_module_name
            .as_deref()
            .is_some_and(|module| self.file_import_module_names.contains(module));
        if !name.contains('.')
            && self.current_scope_declares_source_type(&name, current_module_is_file_import)
        {
            let canonical = self.canonical_current_module_record_name(&name);
            if canonical.contains('.') {
                if let Some(builtin) = self.qualified_source_builtin(&canonical) {
                    return Self::resolved_source_builtin_ty(&canonical, builtin, args);
                }
            }
            if self.declared_type_is_opaque(&canonical) {
                return ResolvedTy::named_opaque_path(&self.defs, &canonical, args);
            }
            return ResolvedTy::named_path(&self.defs, &canonical, args);
        }
        if let Some(canonical) =
            self.canonical_monomorphic_builtin_enum_name(&name, None, current_module_is_file_import)
        {
            return ResolvedTy::named_path(&self.defs, canonical, args);
        }
        if !name.contains('.')
            && self.current_module_name.is_none()
            && self.declared_type_is_opaque(&name)
        {
            return ResolvedTy::named_opaque_path(&self.defs, &name, args);
        }
        if !name.contains('.') {
            if let Some(module_owner) = self.current_module_name.as_deref() {
                let qualified = format!("{module_owner}.{name}");
                if let Some(builtin) = self.qualified_source_builtin(&qualified) {
                    return Self::resolved_source_builtin_ty(&qualified, builtin, args);
                }
                if self.declared_type_is_opaque(&qualified) {
                    return ResolvedTy::named_opaque_path(&self.defs, &qualified, args);
                }
            }
        }
        if !name.contains('.') && self.current_module_name.is_none() {
            if let Some(canonical) = self.file_import_root_type_aliases.get(&name) {
                return self.qualify_current_module_record_ty(ResolvedTy::named_path(
                    &self.defs, canonical, args,
                ));
            }
        }
        if self.declared_type_is_opaque(&name) {
            return ResolvedTy::named_opaque_path(&self.defs, &name, args);
        }
        if !name.contains('.')
            && !self.current_scope_declares_source_type(&name, current_module_is_file_import)
        {
            if let Some(imported) = self.import_type_name_aliases.get(&(
                self.current_module_name.clone(),
                self.current_module_idx,
                name.clone(),
            )) {
                return self.qualify_current_module_record_ty(ResolvedTy::named_path(
                    &self.defs,
                    &imported.clone(),
                    args,
                ));
            }
        }
        let canonical = self.canonical_current_module_record_name(&name);
        if let Some(builtin) = self.qualified_source_builtin(&canonical) {
            return Self::resolved_source_builtin_ty(&canonical, builtin, args);
        }
        if self.declared_type_is_opaque(&canonical) {
            ResolvedTy::named_opaque_path(&self.defs, &canonical, args)
        } else {
            ResolvedTy::named_path(&self.defs, &canonical, args)
        }
    }

    /// Map a checker/presentation spelling of a generated monomorphic builtin
    /// enum to its exact source owner. A bare leaf is admitted only when the
    /// current source scope does not declare that leaf itself; an explicitly
    /// qualified foreign owner is never retried by leaf.
    pub(super) fn canonical_monomorphic_builtin_enum_name(
        &self,
        name: &str,
        builtin: Option<BuiltinType>,
        current_module_is_file_import: bool,
    ) -> Option<&'static str> {
        for fact in MONOMORPHIC_BUILTIN_ENUMS {
            if name == fact.canonical_name {
                return Some(fact.canonical_name);
            }
            if name.contains('.') || name != fact.name {
                continue;
            }
            if self.current_scope_declares_source_type(name, current_module_is_file_import)
                && self.current_module_name.as_deref() != Some(fact.owner)
            {
                continue;
            }
            if builtin.is_none_or(|kind| kind.canonical_name() == fact.name) {
                return Some(fact.canonical_name);
            }
        }
        None
    }

    pub(super) fn canonical_current_module_record_name(&self, name: &str) -> String {
        // An explicitly-qualified annotation normally preserves its owner.
        // The lexical self spelling is the exception: while lowering
        // `hew.alpha.render`, `render.Box` is `hew.alpha.render.Box`, not an
        // independent owner named only `render`.
        //
        // A true import binding wins first, preserving a same-leaf foreign
        // module such as `import hew.right.render`. Only then use the shared
        // current-module candidate helper, and only when the checker recorded
        // that *full* identity. There is intentionally no short-name retry.
        if name.contains('.') {
            if let Some((binding, tail)) = name.split_once('.') {
                if let Some(owner) = self.module_import_bindings.get(&(
                    self.current_module_name.clone(),
                    self.current_module_idx,
                    binding.to_string(),
                )) {
                    return format!("{owner}.{tail}");
                }
            }
            if let Some(canonical) = hew_types::current_module_qualified_type_candidate(
                self.current_module_name.as_deref(),
                name,
            ) {
                if self.checked_type_defs.contains_key(&canonical) {
                    return canonical;
                }
            }
            return name.to_string();
        }
        // Bare reference inside an imported module to a record that module
        // itself declares: qualify to the declaring module's owner identity
        // (`testffi.Result`). A bare `Result` inside `hew::testffi` names
        // testffi's own record (local-shadows-imported), and its owner
        // identity — not the bare last-write-wins key — is what the checker's
        // qualified actor-ask reply type, the imported extern's return
        // signature, and the record's per-module layout must all agree on.
        //
        // The cross-module short-name collision this once gated on
        // (`colliding_imported_record_names`) is only ONE instance: two
        // imported packages sharing a short `Result` forced qualification, but
        // a NON-colliding `-> Result` was left bare — so a mixed file-import
        // (root, bare `%Result`) + package-import (`testffi.Result`) program
        // resolved testffi's extern return to the file's struct and failed
        // closed at codegen (#2208). Owner-qualifying every bare self-record
        // reference closes that gap without a collision precondition. The
        // `record_registry` membership check keeps this scoped to records the
        // current module actually declares: a name imported bare FROM another
        // module has no `{module_short}.{name}` entry and stays unqualified,
        // and a root item (`current_module_name` unset) is never rewritten.
        //
        // Gated on a genuine cross-module bare-name collision: a record unique
        // to its module (e.g. stdlib `xml.Node`) is never rewritten, so the
        // qualification is inert for every non-colliding type and only fires
        // for the same-bare-name shape the actor-ask identity coupling needs.
        if let Some(module_full_path) = self.current_module_name.as_deref() {
            let qualified = format!("{module_full_path}.{name}");
            // The checker can carry a bare `Ty::Named` for either a record or
            // an opaque source declaration. Both recover their exact full
            // module owner here. File-import flattening controls emission
            // order only; it is not declaration-identity authority.
            if self.record_registry.contains_key(&qualified)
                || self.source_type_identities.contains(&qualified)
            {
                return qualified;
            }
        }
        name.to_string()
    }

    pub(super) fn imported_module_member_key(&self, module_binding: &str, member: &str) -> String {
        self.module_import_bindings
            .get(&(
                self.current_module_name.clone(),
                self.current_module_idx,
                module_binding.to_string(),
            ))
            .map_or_else(
                || format!("{module_binding}.{member}"),
                |owner| format!("{owner}.{member}"),
            )
    }

    /// Whether bare `name` is authored by the scope currently being lowered.
    ///
    /// Root declarations use the root namespace. Every imported module,
    /// including a flattened file import, uses the exact declaration
    /// identities harvested from the module graph. This is deliberately narrower than
    /// `record_registry`, which is a global layout index containing imports.
    pub(super) fn current_scope_declares_source_type(
        &self,
        name: &str,
        _current_module_is_file_import: bool,
    ) -> bool {
        if self.current_module_name.is_none() {
            return self.root_visible_source_type_short_names.contains(name);
        }
        self.current_module_name
            .as_deref()
            .is_some_and(|module_full_path| {
                self.source_type_identities
                    .contains(&format!("{module_full_path}.{name}"))
            })
    }

    /// Read encoding identity and opacity from the checker-owned declaration.
    /// Neither a catalogue match nor an opaque annotation supplies authority.
    pub(super) fn checked_encoding_type(
        &self,
        name: &str,
        args: &[ResolvedTy],
    ) -> Option<ResolvedTy> {
        let declaration = self.type_declarations.get(name)?;
        let builtin = declaration
            .builtin
            .filter(|kind| kind.is_encoding_value())?;
        if !args.is_empty() || !declaration.type_params.is_empty() {
            return None;
        }
        Some(ResolvedTy::Named {
            args: Vec::new(),
            head: hew_types::TypeHead::Builtin(builtin),
            is_opaque: declaration.is_opaque,
        })
    }

    /// Resolve an owner-qualified compiler carrier using source provenance.
    ///
    /// The ordinary catalog covers opaque/substrate carriers such as
    /// `stream.Sink`. Lifecycle records are source-defined and therefore need
    /// an exact owner mapping as well. In both cases a colliding user package
    /// declaration wins unless the module graph proves the declaration came
    /// from canonical `std.*`.
    pub(super) fn qualified_source_builtin(&self, name: &str) -> Option<BuiltinType> {
        if !name.contains('.') {
            return hew_types::lookup_builtin_type(name)
                .filter(|builtin| !builtin.requires_source_import());
        }

        // Parser/checker compatibility spellings for the core channel/stream
        // carriers omit the leading `std.`.  While lowering a canonical stdlib
        // module, project only those fixed catalog identities to their exact
        // source declarations, and require that declaration provenance to be
        // present. This is not a module-leaf retry: arbitrary `stream.Sink`
        // source outside `std.*` remains a user nominal.
        let canonical_compat = self
            .current_module_name
            .as_deref()
            .filter(|module| module.starts_with("std."))
            .and(match name {
                "stream.Stream" => Some(("std.stream.Stream", BuiltinType::Stream)),
                "stream.Sink" => Some(("std.stream.Sink", BuiltinType::Sink)),
                _ => None,
            });
        if let Some((canonical, builtin)) = canonical_compat {
            if self
                .canonical_std_source_type_identities
                .contains(canonical)
            {
                return Some(builtin);
            }
        }

        let owner = name.rsplit_once('.').map(|(owner, _)| owner);
        let current_std_owner = self.current_module_name.as_deref().is_some_and(|module| {
            owner == Some(module)
                && self
                    .canonical_std_source_type_identities
                    .iter()
                    .any(|identity| identity.starts_with(&format!("{module}.")))
        });
        let canonical_std_owner =
            current_std_owner || self.canonical_std_source_type_identities.contains(name);

        // The catalog still contains leaf aliases such as `stream.Sink`.
        // They describe ABI, not source ownership: a user module named
        // `stream` is not thereby the shipped std module.  A
        // qualified source spelling becomes a builtin only after an exact
        // canonical-stdlib provenance check; do not retry by its final path
        // segment or by a catalog alias.
        if !canonical_std_owner {
            return None;
        }
        if let Some(builtin) = hew_types::lookup_builtin_type(name).or_else(|| {
            // Lifecycle declarations are source-owned.  Their old short-owner
            // compatibility spellings (`failure.CrashNotification`) remain
            // readable as ordinary nominal identities, but do not acquire
            // compiler representation authority.  Only the exact canonical
            // `std.failure` / `std.link_monitor` declaration identity can
            // carry that authority across this boundary.
            (name.starts_with("std.failure.") || name.starts_with("std.link_monitor."))
                .then(|| hew_types::lookup_source_owned_lifecycle_type(name))
                .flatten()
        }) {
            return Some(builtin);
        }
        None
    }

    /// Retain the exact declaration identity for source-owned lifecycle
    /// carriers while also attaching their runtime representation class.
    /// Ordinary compiler carriers continue to use the catalog's canonical
    /// presentation name. A lifecycle record needs both facts: its qualified
    /// source name selects the nominal layout, while `builtin` selects the ABI.
    pub(super) fn resolved_source_builtin_ty(
        _source_name: &str,
        builtin: BuiltinType,
        args: Vec<ResolvedTy>,
    ) -> ResolvedTy {
        ResolvedTy::named_builtin(builtin, args)
    }

    /// Opacity belongs to the checker declaration at this exact identity.
    pub(super) fn declared_type_is_opaque(&self, identity: &str) -> bool {
        self.type_declarations
            .get(identity)
            .is_some_and(|decl| decl.is_opaque)
    }

    pub(super) fn type_alias_for_name(&self, name: &str) -> Option<&hew_types::TypeAliasDef> {
        let local = self.current_module_name.as_ref().map_or_else(
            || name.to_string(),
            |module| {
                if name.contains('.') {
                    name.to_string()
                } else {
                    format!("{module}.{name}")
                }
            },
        );
        if let Some(declaration) = self.defs.lookup_path(&local) {
            return self.type_aliases.get(&declaration);
        }
        let canonical = self
            .import_type_name_aliases
            .get(&(
                self.current_module_name.clone(),
                self.current_module_idx,
                name.to_string(),
            ))
            .cloned()
            .or_else(|| {
                let (binding, tail) = name.split_once('.')?;
                self.module_import_bindings
                    .get(&(
                        self.current_module_name.clone(),
                        self.current_module_idx,
                        binding.to_string(),
                    ))
                    .map(|owner| format!("{owner}.{tail}"))
            })?;
        let declaration = self.defs.lookup_path(&canonical)?;
        self.type_aliases.get(&declaration)
    }

    pub(super) fn instantiate_type_alias(
        &mut self,
        alias: &hew_types::TypeAliasDef,
        args: &[ResolvedTy],
        span: &Span,
    ) -> ResolvedTy {
        let parameters = alias.type_params.iter().cloned().collect();
        let target = ResolvedTy::from_ty_with_type_params(&alias.target, &parameters);
        match target {
            Ok(target) if alias.type_params.len() == args.len() => {
                let instantiated = substitute_type_params(&target, &alias.type_params, args);
                self.qualify_current_module_record_ty(instantiated)
            }
            target => {
                self.diagnostics.push(HirDiagnostic::new(
                    HirDiagnosticKind::CheckerBoundaryViolation {
                        name: self.defs.path(alias.declaration).to_string(),
                        reason: format!("invalid resolved alias target or arity: {target:?}"),
                    },
                    span.clone(),
                    "type alias reached HIR without a resolved checker contract",
                ));
                ResolvedTy::Unit
            }
        }
    }

    #[expect(
        clippy::too_many_lines,
        reason = "type lowering keeps every TypeExpr boundary in one exhaustive dispatcher"
    )]
    pub(super) fn lower_type(&mut self, ty: &Spanned<TypeExpr>) -> ResolvedTy {
        match &ty.0 {
            TypeExpr::Named {
                path: named_path,
                type_args,
            } => {
                let name = &named_path.to_string(); // TRANSITION(P1): deleted by A1 commit 2
                let args: Vec<ResolvedTy> = type_args
                    .as_ref()
                    .map(|args| args.iter().map(|arg| self.lower_type(arg)).collect())
                    .unwrap_or_default();
                if args.is_empty() && self.current_fn_type_params.contains(name) {
                    // Checker expression facts retain abstract binders as named
                    // types until monomorphisation. Annotations must use the same
                    // representation while keeping lexical binders ahead of aliases.
                    return ResolvedTy::param(name);
                }
                if let Some(alias) = self.type_alias_for_name(name).cloned() {
                    return self.instantiate_type_alias(&alias, &args, &ty.1);
                }
                // W3.042 S2-S1: `Self` in an impl-method body annotation
                // resolves to the concrete impl-target type. Without this
                // interception, `self`'s parser-assigned `TypeExpr::Named
                // { name: "Self" }` leaks into MIR as `ResolvedTy::named_user
                // ("Self", _)` and field access fails with "unregistered
                // record type `Self`". When called outside an impl-method
                // context, `current_impl_self_ty` is `None` and Self falls
                // through to the generic unknown-type path below (preserving
                // the existing behaviour for trait declarations and any
                // not-yet-covered Self surface).
                if name == "Self" && args.is_empty() {
                    if let Some(self_ty) = self.current_impl_self_ty.clone() {
                        return self_ty;
                    }
                }
                // The embedded builtins projection owns this source record
                // under its qualified module identity. Keep that identity in
                // the injected constructor's body and signature; callers have
                // the same checker-proven canonical type.
                if name == "NodeConfig" && args.is_empty() {
                    return ResolvedTy::named_path(&self.defs, "std.builtins.NodeConfig", vec![]);
                }
                match name.as_str() {
                    "i8" => ResolvedTy::I8,
                    "i16" => ResolvedTy::I16,
                    "i32" => ResolvedTy::I32,
                    // `instant` joins `i64` here: it is a monotonic
                    // i64-nanosecond timestamp with no representation of
                    // its own. The checker keeps `Ty::Named { instant }`
                    // to route `impl instant` method dispatch; below the
                    // checker it is an `i64`, exactly as
                    // `ResolvedTy::from_ty` resolves it. Annotations
                    // resolve the same way, so an annotated binding and
                    // an inferred one are one type at every stage after
                    // HIR.
                    "i64" | "instant" => ResolvedTy::I64,
                    "u8" => ResolvedTy::U8,
                    "u16" => ResolvedTy::U16,
                    "u32" => ResolvedTy::U32,
                    "u64" => ResolvedTy::U64,
                    // Platform-sized integers: distinct from fixed-width
                    // i64/u64. Codegen branches on target: 32-bit for
                    // wasm32, 64-bit for native (B-D1 / Q42 ratification).
                    "isize" => ResolvedTy::Isize,
                    "usize" => ResolvedTy::Usize,
                    "f32" => ResolvedTy::F32,
                    "f64" => ResolvedTy::F64,
                    "bool" => ResolvedTy::Bool,
                    "char" => ResolvedTy::Char,
                    "string" => ResolvedTy::String,
                    "duration" => ResolvedTy::Duration,
                    "bytes" => ResolvedTy::Bytes,
                    "CancellationToken" => self
                        .resolve_early_source_type_ref(name, args)
                        .unwrap_or(ResolvedTy::CancellationToken),
                    "Unit" => self
                        .resolve_early_source_type_ref(name, args)
                        .unwrap_or(ResolvedTy::Unit),
                    "()" => ResolvedTy::Unit,
                    // `Task` is a compiler-internal value class with no user-source
                    // syntax. Writing `Task<T>` in any annotation position is a
                    // compile error. Use `fork name = call(...)` to obtain a task
                    // handle. (TI-5 structural enforcement.)
                    "Task" => {
                        if let Some(source_ty) = self.resolve_early_source_type_ref(name, args) {
                            return source_ty;
                        }
                        self.diagnostics.push(HirDiagnostic::new(
                            HirDiagnosticKind::TaskNotNameable,
                            ty.1.clone(),
                            "`Task<T>` is a compiler-internal type and cannot be written \
                             in source. Use `fork name = call(...)` to create a task handle.",
                        ));
                        ResolvedTy::Unit
                    }
                    _ => {
                        let resolved = self.resolve_named_type_ref(name, args);
                        // An actor is the type of its handle: a written actor
                        // name in any position holds the actor, and a bare name
                        // inside a module names that module's actor.
                        let owner = self.current_module_name.clone();
                        self.canonicalize_actor_ref_field_ty(resolved, owner.as_deref())
                    }
                }
            }
            TypeExpr::Infer => {
                self.diagnostics.push(HirDiagnostic::new(
                    HirDiagnosticKind::UnresolvedInferenceVar,
                    ty.1.clone(),
                    "inferred type reached resolved HIR boundary",
                ));
                ResolvedTy::Unit
            }
            TypeExpr::Fallible { success, error } => {
                let success = self.lower_type(success);
                let error = self.lower_type(error);
                ResolvedTy::named_builtin(BuiltinType::Result, vec![success, error])
            }
            TypeExpr::Tuple(elems) if elems.is_empty() => ResolvedTy::Unit,
            TypeExpr::Tuple(elems) => {
                ResolvedTy::Tuple(elems.iter().map(|elem| self.lower_type(elem)).collect())
            }
            TypeExpr::Array { element, size } => {
                ResolvedTy::Array(Box::new(self.lower_type(element)), *size)
            }
            TypeExpr::Slice(elem) => {
                ResolvedTy::named_builtin(BuiltinType::Vec, vec![self.lower_type(elem)])
            }
            TypeExpr::Function {
                capabilities,
                params,
                return_type,
            } => ResolvedTy::Function {
                capabilities: *capabilities,
                params: params.iter().map(|param| self.lower_type(param)).collect(),
                ret: Box::new(self.lower_type(return_type)),
            },
            TypeExpr::ActorFn {
                params,
                return_type,
            } => {
                let resolved: Vec<ResolvedTy> =
                    params.iter().map(|param| self.lower_type(param)).collect();
                let msg = match resolved.len() {
                    0 => ResolvedTy::Unit,
                    1 => resolved.into_iter().next().unwrap_or(ResolvedTy::Unit),
                    _ => ResolvedTy::Tuple(resolved),
                };
                ResolvedTy::named_builtin(
                    BuiltinType::ActorFn,
                    vec![msg, Box::new(self.lower_type(return_type)).as_ref().clone()],
                )
            }
            TypeExpr::Pointer {
                is_mutable,
                pointee,
            } => ResolvedTy::Pointer {
                is_mutable: *is_mutable,
                pointee: Box::new(self.lower_type(pointee)),
            },
            // Preserve an extern signature's non-owning foreign boundary view.
            TypeExpr::Borrow(inner) => ResolvedTy::Borrow {
                pointee: Box::new(self.lower_type(inner)),
            },
            // `dyn Trait` / `dyn Trait<Arg>` / `dyn Trait<Assoc = T>` in any
            // type-annotation position. Each `TraitBound` maps to a
            // `ResolvedTraitBound` via the same three-field lowering
            // `lower_machine_trait_bound` uses; the result reaches the existing
            // downstream `ResolvedTy::TraitObject` paths in MIR and codegen
            // (fat-pointer / vtable machinery — W3.031 / W3.042).
            TypeExpr::TraitObject(bounds) => ResolvedTy::TraitObject {
                traits: bounds
                    .iter()
                    .map(|tb| self.lower_machine_trait_bound(tb))
                    .collect(),
            },
            _ => {
                self.unsupported(ty.1.clone(), "type-expression", "slice-2");
                ResolvedTy::Unit
            }
        }
    }

    pub(super) fn register_option_layout(
        &mut self,
        operand_ty: &ResolvedTy,
        span: &Span,
        context: &str,
    ) {
        // #1929 Stage 2: a synthetic `Option<elem>` registered under a generic
        // body carries an abstract element (`Option$$<T>`), which would leak a
        // `T` payload to the MIR value-class boundary (`UnknownType`). Skip it —
        // the concrete `Option$$<elem>` for each instantiation is recovered by
        // the post-function-mono layout walk (`Option` is seeded into
        // `crate::layout_mono`'s enum-decl table). A concrete operand never
        // takes this branch, so every concrete synthetic-Option path is
        // byte-identical.
        if self.contains_abstract_type_param(operand_ty) {
            return;
        }
        let key = EnumMonoKey {
            origin: SYNTHETIC_OPTION_ITEM,
            origin_name: "Option".to_string(),
            type_args: vec![operand_ty.clone()],
        };
        if self
            .enum_layout_registry
            .insert(
                key,
                vec![
                    EnumVariantLayout {
                        name: "Some".to_string(),
                        field_tys: vec![operand_ty.clone()],
                    },
                    EnumVariantLayout {
                        name: "None".to_string(),
                        field_tys: Vec::new(),
                    },
                ],
                // `Option` is a builtin tagged union, never `indirect`.
                false,
            )
            .is_err()
        {
            self.diagnostics.push(HirDiagnostic::new(
                HirDiagnosticKind::EnumLayoutCapExceeded {
                    cap: self.enum_layout_registry.cap(),
                },
                span.clone(),
                format!(
                    "enum monomorphisation cap exceeded while registering {context} Option layout"
                ),
            ));
        }
    }

    /// Register a concrete compiler-owned cursor layout from the typed catalog.
    /// Abstract instantiations are deferred to `layout_mono`, which consumes the
    /// same catalog after substituting the enclosing function.
    pub(super) fn register_synthetic_cursor_layout(
        &mut self,
        builtin: BuiltinType,
        type_args: &[ResolvedTy],
        span: &Span,
    ) {
        if type_args
            .iter()
            .any(|ty| self.contains_abstract_type_param(ty))
        {
            return;
        }
        let Some((spec, fields)) = synthetic_cursor_layout(builtin, type_args) else {
            debug_assert!(
                false,
                "invalid synthetic cursor layout request: {builtin:?}<{type_args:?}>"
            );
            return;
        };
        let key = RecordMonoKey {
            origin: spec.origin,
            origin_name: builtin.canonical_name().to_string(),
            type_args: type_args.to_vec(),
            symbol_class: crate::mono::SymbolClass::SyntheticRecord,
        };
        if self
            .record_layout_registry
            .insert(key, fields, span.clone())
            .is_err()
            && !self.record_layout_cap_diag_emitted
        {
            self.record_layout_cap_diag_emitted = true;
            let cap = self.record_layout_registry.cap();
            self.diagnostics.push(HirDiagnostic::new(
                HirDiagnosticKind::RecordLayoutCapExceeded { cap },
                span.clone(),
                format!(
                    "too many distinct {} instantiations; the compiler refuses to \
                     monomorphise beyond the configured record-layout cap",
                    builtin.canonical_name()
                ),
            ));
        }
    }

    pub(super) fn resolved_vec_ty(elem_ty: ResolvedTy) -> ResolvedTy {
        ResolvedTy::Named {
            args: vec![elem_ty],
            head: hew_types::TypeHead::Builtin(BuiltinType::Vec),
            is_opaque: false,
        }
    }

    pub(super) fn resolved_vec_iter_ty(elem_ty: ResolvedTy) -> ResolvedTy {
        ResolvedTy::named_builtin(BuiltinType::VecIter, vec![elem_ty])
    }

    pub(super) fn resolved_hashmap_iter_ty(key_ty: ResolvedTy, val_ty: ResolvedTy) -> ResolvedTy {
        ResolvedTy::named_builtin(BuiltinType::HashMapIter, vec![key_ty, val_ty])
    }

    pub(super) fn resolved_option_ty(elem_ty: ResolvedTy) -> ResolvedTy {
        ResolvedTy::named_builtin(BuiltinType::Option, vec![elem_ty])
    }

    /// Register one concrete instantiation of a generic enum in the
    /// `enum_layout_registry`.
    ///
    /// Called from every lowering site that establishes a concrete enum type:
    /// tuple-variant ctor calls (`Option::Some(42)`) and match scrutinee
    /// lowering. The span is the source span of the expression whose checker-
    /// produced type carries the full `Named { name, args }` type.
    ///
    /// The method is a no-op when:
    /// - The span has no `expr_types` entry (checker never assigned a type).
    /// - The type is not `Named` — not an enum instantiation.
    /// - The enum has no type params (monomorphic enum — skip).
    /// - The registry already contains this `(origin, type_args)` key.
    /// - The cap is exceeded (emits `EnumLayoutCapExceeded` diagnostic
    ///   once per module lowering).
    ///
    /// Transitive expansion: after registering a key, this method recurses
    /// into each type arg that is itself a generic enum, running to a fixed
    /// point bounded by the registry cap.
    pub(super) fn try_register_enum_instantiation(&mut self, span: &std::ops::Range<usize>) {
        // Collect concrete Named types reachable from the expression type at
        // this span. Uses a worklist to avoid recursion depth issues.
        let Some(checker_ty) = self.expr_types.get(&self.mk_key(span)).cloned() else {
            return;
        };
        let Ok(resolved) = ResolvedTy::from_ty(&checker_ty) else {
            return;
        };
        let resolved = self.qualify_current_module_record_ty(resolved);
        self.try_register_enum_instantiation_ty(&resolved, span);
    }

    pub(super) fn try_register_enum_instantiation_ty(
        &mut self,
        resolved: &ResolvedTy,
        span: &std::ops::Range<usize>,
    ) {
        // Flatten `resolved` into all Named types that could be generic-enum
        // instantiations, including nested ones inside the type args.
        let mut worklist: Vec<ResolvedTy> = vec![resolved.clone()];
        while let Some(ty) = worklist.pop() {
            let ResolvedTy::Named { head, ref args, .. } = ty else {
                continue;
            };
            let name = head.registry_key();
            // Only act if this enum has type params and this call provides args.
            let Some(type_params) = self.enum_type_params.get(name).cloned() else {
                continue;
            };
            if type_params.is_empty() || args.is_empty() {
                // Enqueue nested Named types even if this one is monomorphic,
                // in case a type arg contains a generic enum.
                for arg in args {
                    worklist.push(arg.clone());
                }
                continue;
            }
            // Enqueue type args for transitive expansion.
            for arg in args {
                worklist.push(arg.clone());
            }
            if args
                .iter()
                .any(|arg| self.contains_abstract_type_param(arg))
            {
                continue;
            }
            // Retrieve the origin ItemId for this enum.
            let Some(&origin) = self.enum_item_ids.get(name) else {
                continue;
            };
            let key = EnumMonoKey {
                origin,
                origin_name: name.to_string(),
                type_args: args.clone(),
            };
            // Build the substituted variant list from the unsubstituted HIR
            // variant descriptors stored in `enum_variants_by_name`.
            let Some(variants_proto) = self.enum_variants_by_name.get(name).cloned() else {
                continue;
            };
            let variant_layouts: Vec<EnumVariantLayout> = variants_proto
                .iter()
                .map(|v| {
                    let raw_field_tys = v.field_tys();
                    let subst_field_tys = raw_field_tys
                        .iter()
                        .map(|ft| substitute_type_params(ft, &type_params, args))
                        .collect();
                    EnumVariantLayout {
                        name: v.name.clone(),
                        field_tys: subst_field_tys,
                    }
                })
                .collect();
            let is_indirect = self.indirect_enum_names.contains(name);
            let insert_result = self
                .enum_layout_registry
                .insert(key, variant_layouts, is_indirect);
            if insert_result.is_err() {
                // Cap exceeded — emit diagnostic and abort further expansion.
                self.diagnostics.push(HirDiagnostic::new(
                    HirDiagnosticKind::EnumLayoutCapExceeded {
                        cap: self.enum_layout_registry.cap(),
                    },
                    span.clone(),
                    "enum monomorphisation cap exceeded; increase `mono_cap` or reduce generic enum instantiation count",
                ));
                return;
            }
        }
    }
}
