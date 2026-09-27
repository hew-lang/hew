//! Type declaration, type reference and enum instantiation lowering.

use super::*;

impl LowerCtx {
    pub(super) fn lower_type_decl(&mut self, decl: &TypeDecl, span: Span) -> Option<HirTypeDecl> {
        let declaration = self.source_declaration(
            &span,
            match decl.origin {
                hew_parser::ast::DeclarationOrigin::MachineState => {
                    hew_types::DeclarationKind::Machine
                }
                hew_parser::ast::DeclarationOrigin::MachineEventType { .. } => {
                    hew_types::DeclarationKind::MachineEventType
                }
                _ => hew_types::DeclarationKind::Type,
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
            .get(&hew_types::NominalId::of_declaration(declaration))
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

    #[allow(
        clippy::too_many_lines,
        reason = "one recursive checker-to-HIR identity authority covers every ResolvedTy wrapper"
    )]
    pub(super) fn restore_type_declaration_facts(&self, ty: ResolvedTy) -> ResolvedTy {
        let ty = match ty {
            ResolvedTy::Tuple(elements) => {
                return ResolvedTy::Tuple(
                    elements
                        .into_iter()
                        .map(|element| self.restore_type_declaration_facts(element))
                        .collect(),
                );
            }
            ResolvedTy::Array(element, size) => {
                return ResolvedTy::Array(
                    Box::new(self.restore_type_declaration_facts(*element)),
                    size,
                );
            }
            ResolvedTy::Slice(element) => {
                return ResolvedTy::Slice(Box::new(self.restore_type_declaration_facts(*element)));
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
                        .map(|param| self.restore_type_declaration_facts(param))
                        .collect(),
                    ret: Box::new(self.restore_type_declaration_facts(*ret)),
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
                        .map(|param| self.restore_type_declaration_facts(param))
                        .collect(),
                    ret: Box::new(self.restore_type_declaration_facts(*ret)),
                    captures: captures
                        .into_iter()
                        .map(|capture| self.restore_type_declaration_facts(capture))
                        .collect(),
                };
            }
            ResolvedTy::Pointer {
                is_mutable,
                pointee,
            } => {
                return ResolvedTy::Pointer {
                    is_mutable,
                    pointee: Box::new(self.restore_type_declaration_facts(*pointee)),
                };
            }
            ResolvedTy::Borrow { pointee } => {
                return ResolvedTy::Borrow {
                    pointee: Box::new(self.restore_type_declaration_facts(*pointee)),
                };
            }
            ResolvedTy::TraitObject { traits } => {
                return ResolvedTy::TraitObject {
                    traits: traits
                        .into_iter()
                        .map(|bound| ResolvedTraitBound {
                            trait_name: bound.trait_name,
                            trait_id: bound.trait_id,
                            args: bound
                                .args
                                .into_iter()
                                .map(|arg| self.restore_type_declaration_facts(arg))
                                .collect(),
                            assoc_bindings: bound
                                .assoc_bindings
                                .into_iter()
                                .map(|(name, ty)| (name, self.restore_type_declaration_facts(ty)))
                                .collect(),
                        })
                        .collect(),
                };
            }
            ResolvedTy::Task(result) => {
                return ResolvedTy::Task(Box::new(self.restore_type_declaration_facts(*result)));
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
            .map(|arg| self.restore_type_declaration_facts(arg))
            .collect();
        let is_opaque = is_opaque
            || head
                .declaration(&self.defs)
                .and_then(|id| self.type_declarations.get(&id))
                .is_some_and(|declaration| declaration.is_opaque);
        ResolvedTy::Named {
            head,
            args,
            is_opaque,
        }
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
            // A generated machine method spells its companion `Tank.Event`
            // inside the defining module. The event is a declaration-owned
            // member of `Tank`, so retain the complete source owner before
            // its parameter crosses the SIR call boundary.
            if let Some(module) = self.current_module_name.as_deref() {
                let canonical = format!("{module}.{name}");
                if self.defs.lookup_path(&canonical).is_some_and(|id| {
                    self.defs.kind(id) == hew_types::DeclarationKind::MachineEventType
                }) {
                    return canonical;
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

    /// Consume the checker-resolved annotation at its exact source occurrence.
    pub(super) fn lower_type(&mut self, ty: &Spanned<TypeExpr>) -> ResolvedTy {
        let key = SpanKey::in_module(&ty.1, self.current_module_idx);
        if let Some(resolved) = self.resolved_annotation_types.get(&key) {
            return resolved.clone();
        }
        self.diagnostics.push(HirDiagnostic::new(
            HirDiagnosticKind::CheckerBoundaryViolation {
                name: format!("{key:?}"),
                reason: "source annotation has no resolved checker type".to_string(),
            },
            ty.1.clone(),
            format!(
                "annotation type did not survive the checker boundary at {key:?}: {:?}",
                ty.0
            ),
        ));
        ResolvedTy::Unit
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
        if super::substitution::contains_abstract_symbol(operand_ty) {
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
            .any(super::substitution::contains_abstract_symbol)
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
        let resolved = self.restore_type_declaration_facts(resolved);
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
                .any(super::substitution::contains_abstract_symbol)
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
