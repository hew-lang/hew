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
        let interceptor_token = [hew_types::KnownDecl::Call, hew_types::KnownDecl::Outcome]
            .into_iter()
            .any(|known| declaration == hew_types::DefTable::known(known));
        // Interceptor tokens have compiler-owned terminal operations and concrete
        // actor-specific fields; user resources still need their declared methods.
        if !interceptor_token
            && matches!(marker, ResourceMarker::Resource | ResourceMarker::Linear)
            && facts.is_opaque
            && ![hew_types::KnownDecl::ServiceExport]
                .into_iter()
                .any(|known| declaration == hew_types::DefTable::known(known))
            && !facts.type_params.is_empty()
        {
            self.diagnostics.push(HirDiagnostic::new(
                HirDiagnosticKind::ResourceGenericUnsupported {
                    name: decl.name.to_string(),
                },
                span.clone(),
                "generic opaque resource types have no per-instance release ABI",
            ));
        }

        match marker {
            ResourceMarker::Resource => {
                self.check_resource_close_discipline(decl, &span, declaration);
            }
            ResourceMarker::Linear if !interceptor_token => {
                self.check_linear_consume_discipline(decl, &span, declaration);
            }
            ResourceMarker::Linear | ResourceMarker::None | ResourceMarker::BitCopy => {}
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
                let Some(signature) = self.fn_sigs.get(&declaration).cloned() else {
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

    pub(super) fn resolved_option_ty(elem_ty: ResolvedTy) -> ResolvedTy {
        ResolvedTy::named_builtin(BuiltinType::Option, vec![elem_ty])
    }
}
