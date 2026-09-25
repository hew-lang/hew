//! Checker methods grouped by responsibility: extern contracts.
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
    /// Ingest a `#[extern_symbol("…")]` attribute (Stage 2 of W3.001).
    ///
    /// Stage 1 already validated the attribute's **attachment position**
    /// (parser rejects it on free fns, actors, trait fns,
    /// type-decl methods). This helper runs at FnSig-ingest time on
    /// the surviving attachment sites (extern `"C"` block fns,
    /// inherent impl methods, trait-impl methods) and:
    ///
    /// 1. Finds the (at most one) `extern_symbol` attribute.
    /// 2. Parses its template via
    ///    [`crate::extern_symbol::ExternSymbolTemplate::parse`].
    /// 3. On success returns a populated
    ///    [`crate::extern_symbol::ExternSymbolSpec`].
    /// 4. On failure emits a span-anchored
    ///    [`TypeErrorKind::InvalidExternSymbolTemplate`] diagnostic
    ///    and returns `None` (fail-closed: the `FnSig` records no
    ///    template, so Stage-3 monomorphic dispatch will surface the
    ///    same call site as an unresolved-symbol diagnostic rather
    ///    than silently routing through a malformed template).
    ///
    /// Returns `None` when no `extern_symbol` attribute is present —
    /// the normal case for ordinary functions and methods.
    pub(in crate::check) fn ingest_extern_symbol_attrs(
        &mut self,
        attrs: &[Attribute],
    ) -> Option<crate::extern_symbol::ExternSymbolSpec> {
        let attr = attrs.iter().find(|a| a.name == "extern_symbol")?;
        // Stage 1 parser accepts only a single positional string argument
        // for `#[extern_symbol("...")]` (see hew-parser tests at
        // `extern_symbol_attribute_on_*_is_captured`). If a future
        // parser regression lets a malformed shape through, fail closed
        // with a precise diagnostic rather than panic.
        let raw_payload = match attr.args.as_slice() {
            [AttributeArg::Positional(s)] => s.as_str(),
            [] => {
                self.errors.push(TypeError::new(
                    TypeErrorKind::InvalidExternSymbolTemplate {
                        reason: "missing template string — expected `#[extern_symbol(\"...\")]`"
                            .to_string(),
                    },
                    attr.span.clone(),
                    "`#[extern_symbol]` requires a single string argument naming the C-ABI \
                     runtime symbol (with optional `{T}` placeholders for per-monomorphization \
                     dispatch)"
                        .to_string(),
                ));
                return None;
            }
            _ => {
                self.errors.push(TypeError::new(
                    TypeErrorKind::InvalidExternSymbolTemplate {
                        reason: "expected exactly one positional string argument".to_string(),
                    },
                    attr.span.clone(),
                    "`#[extern_symbol(\"hew_symbol\")]` accepts exactly one positional string \
                     argument; multi-argument and key-value forms are not part of the W3.001 \
                     grammar"
                        .to_string(),
                ));
                return None;
            }
        };
        match crate::extern_symbol::ExternSymbolTemplate::parse(raw_payload) {
            Ok(template) => Some(crate::extern_symbol::ExternSymbolSpec {
                template,
                span: attr.span.clone(),
            }),
            Err(err) => {
                let reason = err.reason();
                self.errors.push(TypeError::new(
                    TypeErrorKind::InvalidExternSymbolTemplate {
                        reason: reason.clone(),
                    },
                    attr.span.clone(),
                    format!("invalid `#[extern_symbol]` template: {reason}"),
                ));
                None
            }
        }
    }

    /// Resolve an extern callable's nominal types to the registered source
    /// declaration used by field annotations and ordinary callable signatures.
    pub(super) fn resolve_extern_signature_nominals(&self, ty: &Ty) -> Ty {
        match ty {
            Ty::Named { head, args, .. } => {
                let name = head.registry_key();
                let builtin = head.builtin();
                // The callable consumes source values, including fields from
                // peer files assembled into this module. Resolve their registered
                // declaration rather than substituting the ABI contract's file
                // provenance. Already-qualified source identities stay intact.
                let resolved = self.canonical_nominal_name(name).or_else(|| {
                    (!name.contains('.') && self.extern_nominal_file_owner(name).is_none())
                        .then(|| self.extern_nominal_imported_owner(name))
                        .flatten()
                });
                let args = args
                    .iter()
                    .map(|arg| self.resolve_extern_signature_nominals(arg))
                    .collect();
                if builtin.is_some() || head.is_param() {
                    Ty::Named { head: *head, args }
                } else {
                    self.named_ty_for_key(&resolved.unwrap_or_else(|| name.to_string()), args)
                }
            }
            _ => ty.map_children_pub(&|child| self.resolve_extern_signature_nominals(child)),
        }
    }

    /// FILE-lexical nominal authority: the item's own declaring file first,
    /// then exactly one sibling file of the declaring module.
    ///
    /// The item's own file is known only inside the per-item registration
    /// frame; outside it the sibling rule alone still decides, which is what
    /// lets source type expressions ask the same question extern signatures do.
    pub(in crate::check) fn extern_nominal_file_owner(&self, name: &str) -> Option<String> {
        let declares = |source: &std::path::PathBuf| {
            self.file_type_decls
                .get(source)
                .is_some_and(|declared| declared.contains(name))
        };
        if let Some(file) = self.current_item_source.as_ref() {
            if declares(file) {
                return Some(format!(
                    "{}.{name}",
                    self.defs.module_path_for_source(file)?
                ));
            }
        }
        let sources = self
            .module_source_paths
            .get(self.current_module.as_deref()?)?;
        let mut declaring = sources.iter().filter(|source| declares(source));
        let single = declaring.next()?;
        if declaring.next().is_some() {
            return None;
        }
        Some(format!(
            "{}.{name}",
            self.defs.module_path_for_source(single)?
        ))
    }

    /// Diagnostic routing token for an item declared in `file` under
    /// `module`: the file's own path when it is NOT the module's primary
    /// source (a peer file of a directory module), else `None` (the module
    /// identity routes as before). The CLI/LSP source maps carry an entry per
    /// source file keyed by this exact rendering.
    pub(in crate::check) fn item_file_routing_token(
        &self,
        module: Option<&str>,
        file: Option<&std::path::PathBuf>,
    ) -> Option<String> {
        let file = file?;
        let primary = self.module_source_paths.get(module?)?.first()?;
        (file != primary).then(|| file.display().to_string())
    }

    /// Capture the source identity that diagnostic emission must use after the
    /// current item frame has been cleared. Peer files need their path rather
    /// than the assembled directory module's identity.
    pub(in crate::check) fn current_diagnostic_source_module(&self) -> Option<String> {
        self.item_file_routing_token(
            self.current_module.as_deref(),
            self.current_item_source.as_ref(),
        )
        .or_else(|| self.current_module.clone())
    }

    /// Resolve one extern declaration against the single-owner table
    /// (rc1-F1 stage B): mint the symbol's contract, or run the canonicalized
    /// structural compare against the established one — adopt on agreement,
    /// register-then-report on conflict. The compare ALWAYS runs for a
    /// further declaration of an established symbol (no span-based
    /// same-site shortcut: a byte-offset span carries no file identity, and
    /// peer files of one directory module can align spans exactly).
    pub(super) fn resolve_extern_contract(
        &mut self,
        block_span: &Span,
        declaration_ordinal: usize,
        source_symbol: &str,
        sig: &FnSig,
        consuming_params: &[bool],
        f: &hew_parser::ast::ExternFnDecl,
    ) -> Option<crate::DefId> {
        let declaration = self.require_declaration_occurrence(
            block_span,
            crate::DeclarationKind::ExternFunction,
            declaration_ordinal,
        )?;
        if source_symbol.is_empty() {
            // Template declarations (`#[extern_symbol("…{T}…")]`) have no
            // call-independent symbol and therefore no symbol-keyed contract
            // slot; they still register as extern declarations (call-target
            // resolution, `unsafe` gating).
            self.extern_table.register_detached_declaration(
                declaration,
                String::new(),
                self.current_module.clone(),
            );
            return Some(declaration);
        }
        // Resolve the candidate signature's nominal identities NOW, in the
        // declaring item's own lexical context (rc1-F1 stage C). The
        // contract stores resolved signatures, so every later comparison is
        // identity equality — no compare-time spelling repair, whose context
        // would be the SECOND declaration's, not the declarer's.
        let resolved_params = sig
            .params
            .iter()
            .map(|ty| extern_contract_nominal_identity(self, ty))
            .collect::<Vec<_>>();
        let resolved_return = extern_contract_nominal_identity(self, &sig.return_type);
        let Some((established_id, established)) = self
            .extern_table
            .established(source_symbol)
            .map(|(id, contract)| (id, contract.clone()))
        else {
            self.extern_table.mint(crate::extern_table::ExternContract {
                owner: declaration,
                symbol: source_symbol.to_string(),
                params: resolved_params,
                return_type: resolved_return,
                consuming_params: consuming_params.to_vec(),
                is_variadic: f.is_variadic,
                span: f.span.clone(),
                declaring_module: self.current_module.clone(),
                declaring_source: self.current_item_source.clone(),
            });
            return Some(declaration);
        };
        let agrees = established.params == resolved_params
            && established.return_type == resolved_return
            && established.consuming_params == consuming_params
            && established.is_variadic == f.is_variadic;
        if agrees {
            // Single-owner property: a further agreeing declaration becomes
            // another name of the ONE established ABI contract, keeping its
            // OWN provenance (declaring module, endpoint).
            self.extern_table.adopt_declaration(
                declaration,
                source_symbol.to_string(),
                self.current_module.clone(),
                established_id,
            );
        } else {
            // Register FIRST, report second: the conflicting declaration
            // must keep its `unsafe` gate and call-target endpoint while the
            // hard error propagates (the unsafe registry is not conditional
            // on ABI agreement).
            self.extern_table.register_detached_declaration(
                declaration,
                source_symbol.to_string(),
                self.current_module.clone(),
            );
            self.report_extern_contract_conflict(
                source_symbol,
                &established,
                &resolved_params,
                &resolved_return,
                consuming_params,
                f,
            );
        }
        Some(declaration)
    }

    /// Report a declaration whose signature disagrees with the symbol's
    /// established contract, attributing both sides to their declaring FILES.
    pub(super) fn report_extern_contract_conflict(
        &mut self,
        source_symbol: &str,
        established: &crate::extern_table::ExternContract,
        resolved_params: &[Ty],
        resolved_return: &Ty,
        consuming_params: &[bool],
        f: &hew_parser::ast::ExternFnDecl,
    ) {
        let established_description = extern_signature_description(
            &established.params,
            &established.return_type,
            &established.consuming_params,
            established.is_variadic,
        );
        // Describe the RESOLVED identities — the things actually compared —
        // so two same-leaf nominals from different declaring files render
        // distinguishably (`pkg.Tok` vs `pkg.aaa.Tok`), never as one
        // spelling conflicting with itself.
        let conflicting = extern_signature_description(
            resolved_params,
            resolved_return,
            consuming_params,
            f.is_variadic,
        );
        // File-accurate attribution (rc1-F1 stage C): item spans are
        // file-relative byte offsets, so a declaration living in a peer
        // file of a directory module must route to THAT file, never the
        // module's primary source. The routing token is the declaring
        // file itself when it differs from the module's primary file;
        // otherwise the module identity routes as before.
        let error_token = self
            .item_file_routing_token(
                self.current_module.as_deref(),
                self.current_item_source.as_ref(),
            )
            .or_else(|| self.current_module.clone());
        let note_token = self
            .item_file_routing_token(
                established.declaring_module.as_deref(),
                established.declaring_source.as_ref(),
            )
            .or_else(|| established.declaring_module.clone());
        self.errors.push(TypeError {
            severity: crate::error::Severity::Error,
            kind: TypeErrorKind::ConflictingExternDeclaration {
                symbol_name: source_symbol.to_string(),
            },
            span: f.span.clone(),
            message: format!(
                "extern symbol `{source_symbol}` has conflicting declarations: \
                 `{conflicting}` does not match the established `{established_description}`"
            ),
            notes: vec![
                (
                    established.span.clone(),
                    format!(
                        "the first declaration of `{source_symbol}` established `{established_description}`"
                    ),
                    note_token,
                ),
                (
                    f.span.clone(),
                    "extern \"C\" symbol declarations are program-wide unique: the linker \
                     binds every call site to one implementation, so a redeclaration must \
                     match the established contract exactly, even if this declaration is \
                     never called"
                        .to_string(),
                    error_token.clone(),
                ),
            ],
            suggestions: vec![format!(
                "make every declaration of `{source_symbol}` use exactly `{established_description}`"
            )],
            source_module: error_token,
        });
    }

    /// Establish the extern declaration identity of a registry-loaded C
    /// function and register its `unsafe` gate.
    ///
    /// Registry metadata and codegen-intercepted witnesses publish these
    /// functions under `key` in `fn_sigs` and the `unsafe` gate resolves a
    /// call by that same key, so the declaration path is `key`. Shipped
    /// modules routinely re-export one runtime symbol, so a bare path that is
    /// already established is the same declaration reached through a second
    /// mirror, not a conflict; a source declaration already gating `key`
    /// keeps its own record.
    pub(super) fn declare_contractless_extern(
        &mut self,
        module: crate::ModuleId,
        module_path: &str,
        name: hew_parser::ast::Symbol,
        key: &str,
    ) {
        let declaration = if let Some(existing) = self.lookup_declaration(key) {
            existing
        } else {
            // One occurrence per source-less extern declaration in this
            // module. The registry mirror and the layout witnesses are two
            // inventories that both declare here; a per-inventory ordinal
            // made them collide, and `declare` resolves a collision to the
            // ESTABLISHED declaration, so the second inventory's rows
            // silently adopted the first's identities and endpoints.
            let ordinal = self
                .contractless_extern_occurrences
                .entry(module)
                .or_insert(0);
            let occurrence = crate::DeclarationOccurrence::new_with_synthetic_ordinal(
                Some(module),
                &(0..0),
                *ordinal,
                crate::DeclarationKind::ExternFunction,
                0,
            );
            *ordinal += 1;
            match self.defs.declare(occurrence, name, None, key) {
                Ok(declaration) => declaration,
                Err(error) => {
                    self.errors.push(TypeError::new(
                        TypeErrorKind::InvalidOperation,
                        0..0,
                        format!("extern declaration `{key}` in `{module_path}`: {error}"),
                    ));
                    return;
                }
            }
        };
        if !self.extern_table.requires_unsafe(declaration) {
            self.extern_table.register_contractless_declaration(
                declaration,
                key.to_string(),
                Some(module_path.to_string()),
            );
        }
    }

    #[expect(
        clippy::too_many_lines,
        reason = "extern registration validates ABI authority and records lifecycle provenance"
    )]
    pub(in crate::check) fn register_extern_block(&mut self, eb: &ExternBlock, block_span: &Span) {
        // `extern "rt"` is the Hew-side declaration surface for runtime
        // functions. Validate each declared symbol against the `stable`
        // section of scripts/runtime-export-classification.toml. Fail-closed: an
        // unclassified symbol is a hard error so the failure surfaces at check
        // time rather than at link time or (worse) silently routing to a wrong
        // runtime entry.
        //
        // `extern "C"` remains the raw user FFI surface (unsafe, no
        // validation). Other ABI strings are not yet defined by the language
        // and fall through to fn_sigs registration unchanged.
        if eb.abi == "rt" {
            let stable = crate::jit_symbols::stable_symbols();
            for f in &eb.functions {
                if !stable.contains(f.name.name.as_str()) {
                    self.errors.push(TypeError {
                        severity: crate::error::Severity::Error,
                        kind: TypeErrorKind::ExternRtSymbolUnclassified {
                            symbol_name: f.name.to_string(),
                            hint: format!(
                                "add `\"{}\"` to the `stable` list in \
                                 scripts/runtime-export-classification.toml, \
                                 or use `extern \"C\"` for raw FFI symbols \
                                 that are not part of the Hew runtime export ABI",
                                f.name
                            ),
                        },
                        span: f.span.clone(),
                        message: format!(
                            "`extern \"rt\" fn {}` names a symbol outside the stable \
                             runtime export ABI — only symbols classified as `stable` \
                             in scripts/runtime-export-classification.toml may appear in \
                             `extern \"rt\"` blocks",
                            f.name
                        ),
                        notes: vec![(
                            f.span.clone(),
                            "The `non-declarable` classification covers compiler-emitted, \
                             lifecycle, and shutdown symbols. None of these may be named \
                             by user code in `extern \"rt\"` blocks."
                                .to_string(),
                            self.current_module.clone(),
                        )],
                        suggestions: vec![format!(
                            "add `\"{}\"` to the `stable` list in \
                             scripts/runtime-export-classification.toml",
                            f.name
                        )],
                        source_module: self.current_module.clone(),
                    });
                }
            }
        }

        for (declaration_ordinal, f) in eb.functions.iter().enumerate() {
            let mut hole_vars = Vec::new();
            let param_names = f.params.iter().map(|p| p.name.to_string()).collect();
            let params: Vec<Ty> = f
                .params
                .iter()
                .map(|p| {
                    self.resolve_registered_annotation_ty_with_context(
                        &p.ty,
                        &mut hole_vars,
                        TypeResolutionContext::ExternSignature,
                    )
                })
                .collect();
            let return_type = f.return_type.as_ref().map_or(Ty::Unit, |ret| {
                self.resolve_registered_annotation_ty_with_context(
                    ret,
                    &mut hole_vars,
                    TypeResolutionContext::ExternSignature,
                )
            });
            for (index, ty) in params.iter().chain([&return_type]).enumerate() {
                let Some(reason) = unmarshallable_extern_ty(ty) else {
                    continue;
                };
                let span = f
                    .params
                    .get(index)
                    .map_or_else(|| f.span.clone(), |param| param.ty.1.clone());
                let position = if index < f.params.len() {
                    format!("parameter {index}")
                } else {
                    "return type".to_string()
                };
                self.errors.push(TypeError::new(
                    TypeErrorKind::InvalidOperation,
                    span,
                    format!(
                        "`extern fn {}` {position} is `{}`, which {reason} and has no C-ABI representation",
                        f.name,
                        ty.user_facing()
                    ),
                ));
            }
            let mut sig = FnSig {
                param_ownership: f
                    .params
                    .iter()
                    .map(|param| crate::env::ParameterOwnership::from_consume(param.is_consume))
                    .collect(),
                param_names,
                params,
                return_type,
                extern_symbol: self.ingest_extern_symbol_attrs(&f.attributes),
                ..FnSig::default()
            };
            let source_symbol = sig.extern_symbol.as_ref().map_or_else(
                || f.name.to_string(),
                |spec| {
                    if spec.template.is_monomorphic() {
                        spec.template.raw.clone()
                    } else {
                        String::new()
                    }
                },
            );
            let source_symbol_template = sig
                .extern_symbol
                .as_ref()
                .filter(|spec| !spec.template.is_monomorphic())
                .map(|spec| spec.template.clone());
            // Call sites type against the registered source declaration, just
            // as field annotations do. The ABI contract separately checks the
            // declaring file's provenance when comparing repeated symbols.
            // Template declarations carry signature type holes, not concrete
            // nominals — leave them untouched.
            if !source_symbol.is_empty() {
                sig.params = sig
                    .params
                    .iter()
                    .map(|ty| self.resolve_extern_signature_nominals(ty))
                    .collect();
                sig.return_type = self.resolve_extern_signature_nominals(&sig.return_type);
            }
            // Extern declarations use the same canonical owner spelling as
            // ordinary free functions. Their exact DefId comes from the
            // enclosing source occurrence inventoried before registration.
            let key = scoped_module_item_name(self.canonical_fn_owner(), f.name.name.as_str())
                .unwrap_or_else(|| f.name.to_string());
            let consuming_params = f
                .params
                .iter()
                .map(|param| param.is_consume)
                .collect::<Vec<_>>();
            let Some(declaration) = self.resolve_extern_contract(
                block_span,
                declaration_ordinal,
                &source_symbol,
                &sig,
                &consuming_params,
                f,
            ) else {
                continue;
            };
            self.record_fn_sig_inference_holes(&key, hole_vars);
            self.fn_sigs.insert(key.clone(), sig);
            if !self
                .source_extern_declarations
                .iter()
                .any(|existing| existing.declaration == declaration)
            {
                self.source_extern_declarations
                    .push(SourceExternDeclaration {
                        declaration,
                        symbol: source_symbol,
                        symbol_template: source_symbol_template,
                        signature_key: key.clone(),
                        declaring_module: self.current_module.clone(),
                        declaring_file: self.current_module_idx,
                        direct_import_modules: self.current_module_direct_imports.clone(),
                        consuming_params,
                    });
            }

            self.record_root_value_binding(f.name.name.as_str());
        }
    }

    /// Join generated owned-result contracts to exact source extern
    /// declarations and their consuming release declaration.
    ///
    /// The generated table is the only ownership authority. Source names
    /// participate only after the contract's qualified nominal proves the
    /// declaring module, so a root or foreign-module symbol collision cannot
    /// inherit a lifecycle.
    pub(in crate::check) fn derive_opaque_resource_candidate_graph(
        &self,
        fn_sigs: &HashMap<String, FnSig>,
    ) -> OpaqueResourceCandidateGraph {
        derive_opaque_resource_candidate_graph(
            &self.source_extern_declarations,
            fn_sigs,
            &self.module_import_bindings,
            &self.import_type_name_aliases,
            &self.impl_method_declaration_ids,
            crate::ffi_contracts::FFI_OWNERSHIP_CONTRACTS,
            &self.defs,
        )
    }

    #[cfg(test)]
    pub(in crate::check) fn derive_opaque_resource_candidate_graph_for_contracts(
        &self,
        fn_sigs: &HashMap<String, FnSig>,
        contracts: &[(&str, crate::ffi_contracts::ExternOwnershipContract)],
    ) -> OpaqueResourceCandidateGraph {
        derive_opaque_resource_candidate_graph(
            &self.source_extern_declarations,
            fn_sigs,
            &self.module_import_bindings,
            &self.import_type_name_aliases,
            &self.impl_method_declaration_ids,
            contracts,
            &self.defs,
        )
    }
}
