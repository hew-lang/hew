#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
use super::*;

pub(super) enum SignatureArgApplication<'a> {
    PositionalOnly {
        arity_context: String,
    },
    FunctionLike {
        param_names: &'a [String],
        accepts_kwargs: bool,
        module_qualified: bool,
    },
}

pub(super) struct AppliedCallSignature {
    pub(super) params: Vec<Ty>,
    pub(super) return_type: Ty,
}

impl Checker {
    pub(super) fn lookup_variant_constructor(
        &self,
        func_name: &str,
    ) -> Option<(String, Vec<Ty>, Vec<String>)> {
        if let Some(pos) = func_name.rfind("::") {
            let type_prefix = &func_name[..pos];
            let variant_name = &func_name[pos + 2..];
            // A constructor written inside its declaring package module uses
            // the bare lexical prefix (`Parcel::Filled`), but the declaration
            // is keyed under its full owner (`left.render.Parcel`). Resolve
            // that owner before consulting the compatibility bare entry.
            let canonical_owner_key = self.canonical_nominal_name(type_prefix);
            let local_owner_key = (!type_prefix.contains('.'))
                .then(|| {
                    self.current_module_identity()
                        .map(|owner| format!("{owner}.{type_prefix}"))
                })
                .flatten();
            let direct_key = canonical_owner_key
                .as_deref()
                .filter(|key| self.type_defs.contains_key(*key))
                .or_else(|| {
                    local_owner_key
                        .as_deref()
                        .filter(|key| self.type_defs.contains_key(*key))
                })
                .unwrap_or(type_prefix);
            let direct = self.type_defs.get(direct_key).and_then(|td| {
                if td.kind != TypeDefKind::Enum && td.kind != TypeDefKind::Struct {
                    return None;
                }
                td.variants.get(variant_name).and_then(|variant| {
                    let params = match variant {
                        VariantDef::Unit => Vec::new(),
                        VariantDef::Tuple(p) => p.clone(),
                        VariantDef::Struct(_) => return None,
                    };
                    Some((direct_key.to_string(), params, td.type_params.clone()))
                })
            });
            if direct.is_some() {
                return direct;
            }
            // Import-alias fallback: `Geo::Box` where "Geo" was bound as an
            // alias for "shapes.Shape".  Resolve through `import_type_name_aliases`
            // and retry the variant lookup under the canonical qualified name.
            let canonical = self.import_type_name_aliases.get(&(
                self.current_module.clone(),
                self.current_module_idx,
                type_prefix.to_string(),
            ))?;
            self.type_defs.get(canonical.as_str()).and_then(|td| {
                if td.kind != TypeDefKind::Enum && td.kind != TypeDefKind::Struct {
                    return None;
                }
                td.variants.get(variant_name).and_then(|variant| {
                    let params = match variant {
                        VariantDef::Unit => Vec::new(),
                        VariantDef::Tuple(p) => p.clone(),
                        VariantDef::Struct(_) => return None,
                    };
                    // Return the canonical type name so HIR lowering resolves
                    // the constructor against the right registered type.
                    Some((canonical.clone(), params, td.type_params.clone()))
                })
            })
        } else {
            // Two-pass scan: local/source types win over builtin/imported types
            // (local-shadows-global rule).  Pass 1 scans only locally-declared
            // types; pass 2 scans the remainder.  This prevents a builtin unit
            // variant (e.g. `LookupError::NotFound`) from shadowing a user-
            // declared tuple variant with the same bare name
            // (e.g. `AppError::NotFound(string)`).
            let find_in = |name: &str, check_local: bool| {
                self.type_defs
                    .iter()
                    .filter(|(type_name, td)| {
                        let is_local = self.local_type_defs.contains(type_name.as_str())
                            || self.source_type_defs.contains(type_name.as_str());
                        let kind_ok =
                            td.kind == TypeDefKind::Enum || td.kind == TypeDefKind::Struct;
                        kind_ok && (is_local == check_local)
                    })
                    .find_map(|(type_name, td)| {
                        td.variants.get(name).and_then(|variant| {
                            let params = match variant {
                                VariantDef::Unit => Vec::new(),
                                VariantDef::Tuple(p) => p.clone(),
                                VariantDef::Struct(_) => return None,
                            };
                            Some((type_name.clone(), params, td.type_params.clone()))
                        })
                    })
            };
            // Pass 1: user-declared types.
            find_in(func_name, true).or_else(|| {
                // Pass 2: builtin/imported types.
                find_in(func_name, false)
            })
        }
    }

    pub(super) fn expected_constructor_type_args(
        &self,
        expected: &Ty,
        type_name: &str,
        arity: usize,
    ) -> Option<Vec<Ty>> {
        match expected {
            Ty::Named { name, args, .. }
                if self.strict_nominal_identity(name)
                    == self.strict_nominal_identity(type_name)
                    && args.len() == arity =>
            {
                Some(args.clone())
            }
            _ => None,
        }
    }

    /// Rebuild a constructor result from the already-resolved expected
    /// nominal, changing only its inferred arguments. The expected type owns
    /// both declaration identity and builtin discrimination; consulting its
    /// display spelling again would lose renamed builtin presentations or
    /// retag a same-spelling source declaration.
    pub(super) fn variant_nominal_from_expected(expected: &Ty, args: Vec<Ty>) -> Option<Ty> {
        let Ty::Named { name, builtin, .. } = expected else {
            return None;
        };
        Some(Ty::Named {
            name: name.clone(),
            args,
            builtin: *builtin,
        })
    }

    fn lower_turbofish_elem(
        &mut self,
        constructor_name: &str,
        expected_arity: usize,
        supplied_args: &[Spanned<TypeExpr>],
        span: &Span,
    ) -> Option<Vec<Ty>> {
        if supplied_args.len() != expected_arity {
            self.report_error(
                TypeErrorKind::ArityMismatch,
                span,
                format!(
                    "`{constructor_name}` takes {expected_arity} type argument{} but {} {} supplied",
                    if expected_arity == 1 { "" } else { "s" },
                    supplied_args.len(),
                    if supplied_args.len() == 1 { "was" } else { "were" }
                ),
            );
            return None;
        }

        Some(
            supplied_args
                .iter()
                .map(|type_arg| self.resolve_type_expr(type_arg))
                .collect(),
        )
    }

    fn lower_turbofish_collection_constructor(
        &mut self,
        constructor_name: &str,
        builtin: crate::BuiltinType,
        expected_arity: usize,
        supplied_args: &[Spanned<TypeExpr>],
        span: &Span,
    ) -> Option<Ty> {
        let lowered =
            self.lower_turbofish_elem(constructor_name, expected_arity, supplied_args, span)?;
        let resolved_args: Vec<Ty> = lowered.iter().map(|ty| self.subst.resolve(ty)).collect();
        let result_ty = Ty::Named {
            builtin: Some(builtin),
            name: constructor_name.to_string(),
            args: resolved_args,
        };
        match builtin {
            crate::BuiltinType::HashMap => {
                self.validate_concrete_hashmap_type(&result_ty, span);
                self.record_direct_call_target(
                    span,
                    CallTarget::Runtime(crate::runtime_call::RuntimeCallFamily::HashMapNew),
                );
            }
            crate::BuiltinType::HashSet => {
                self.validate_concrete_hashset_type(&result_ty, span);
                self.record_direct_call_target(
                    span,
                    CallTarget::Runtime(crate::runtime_call::RuntimeCallFamily::HashSetNew),
                );
            }
            crate::BuiltinType::Vec => {
                self.validate_concrete_vec_type(&result_ty, span);
                self.record_direct_call_target(
                    span,
                    CallTarget::Runtime(crate::runtime_call::RuntimeCallFamily::VecNew),
                );
            }
            _ => {}
        }
        self.record_type(span, &result_ty);
        Some(result_ty)
    }

    /// Record the resolved type arguments for a generic function call site so
    /// HIR/MIR can mangle and dispatch the monomorphised symbol.
    ///
    /// Snapshot through `subst.resolve` and store **unconditionally**, even when
    /// an argument still carries a `Ty::Var`. A return-type-polymorphic call
    /// (`let s: Stack<i64> = new_stack()` / `Stack::new()`) determines its type
    /// parameters from the *expected return type*, which `check_against` only
    /// unifies in *after* `synthesize` already ran this recording inside
    /// `apply_instantiated_call_signature_with_assoc`. Dropping the entry here
    /// while `T` is still an inference var (the old eager guard) starved the
    /// monomorphisation pipeline and tripped the function-call NYI arm — the
    /// argument-driven case (`singleton(99)`) worked only because its `T` was
    /// already pinned by an argument at recording time.
    ///
    /// Discipline mirrors [`Self::record_concrete_record_init_type_args`]: the
    /// snapshot stores the substitution representative, so a later binding of
    /// that var propagates at the `check_program` output boundary (where every
    /// entry is re-resolved through `subst.resolve` + `materialize_literal_defaults`).
    /// The fail-closed invariant (no `Ty::Var` crosses into HIR) is preserved —
    /// [`Self::validate_call_type_args_output_contract`] prunes any entry that is
    /// still partial after inference settles, exactly as it did before; it is
    /// just enforced at the output boundary rather than at emission, parallel to
    /// how `expr_types` works.
    pub(super) fn record_concrete_call_type_args(&mut self, span: &Span, type_args: &[Ty]) {
        if type_args.is_empty() {
            return;
        }
        let snapshot: Vec<Ty> = type_args.iter().map(|ty| self.subst.resolve(ty)).collect();
        self.call_type_args
            .insert(SpanKey::in_module(span, self.current_module_idx), snapshot);
    }

    /// Record the resolved type arguments for a record (or enum-struct-variant)
    /// initialiser site on a user-defined generic type.
    ///
    /// Mirrors [`record_concrete_call_type_args`] for the record-init
    /// monomorphisation surface, with one structural difference: a
    /// record-init's type args may only become fully concrete *after*
    /// `check_struct_init` returns — via an outer annotation
    /// (`let x: Box<int> = Box { value: 1 }`) or an enclosing return-type
    /// unification.  Eagerly rejecting at emission time when an arg still
    /// carries a `Ty::Var` would drop entries the post-inference boundary
    /// resolve in `check_program` would have made fully concrete.
    ///
    /// Discipline: snapshot through `subst.resolve` here so later updates to
    /// the substitution propagate at the boundary; rely on
    /// [`Self::validate_record_init_type_args_output_contract`] to prune
    /// entries that are still partial after `materialize_literal_defaults`
    /// settles.  The fail-closed invariant (no `Ty::Var` crosses into HIR)
    /// is preserved — it is just enforced at the output boundary rather
    /// than at emission, parallel to how `expr_types` works.
    pub(super) fn record_concrete_record_init_type_args(&mut self, span: &Span, type_args: &[Ty]) {
        if type_args.is_empty() {
            return;
        }
        let snapshot: Vec<Ty> = type_args.iter().map(|ty| self.subst.resolve(ty)).collect();
        self.record_init_type_args
            .insert(SpanKey::in_module(span, self.current_module_idx), snapshot);
    }

    fn record_builtin_result_output_type_args(&mut self, span: &Span, ok_ty: &Ty, err_ty: &Ty) {
        self.builtin_result_output_type_args.insert(
            SpanKey::in_module(span, self.current_module_idx),
            (ok_ty.clone(), err_ty.clone()),
        );
    }

    pub(super) fn apply_instantiated_call_signature(
        &mut self,
        sig: &FnSig,
        type_args: Option<&[Spanned<TypeExpr>]>,
        args: &[CallArg],
        span: &Span,
        arg_application: SignatureArgApplication<'_>,
        record_call_type_args: bool,
    ) -> AppliedCallSignature {
        let empty_assoc_bindings = HashMap::new();
        self.apply_instantiated_call_signature_with_assoc(
            sig,
            &empty_assoc_bindings,
            type_args,
            args,
            span,
            arg_application,
            record_call_type_args,
        )
    }

    #[expect(
        clippy::too_many_arguments,
        reason = "call application needs the signature, its associated-type side table, source args, span, and arity mode"
    )]
    pub(super) fn apply_instantiated_call_signature_with_assoc(
        &mut self,
        sig: &FnSig,
        type_param_assoc_bindings: &HashMap<(String, String, String), Ty>,
        type_args: Option<&[Spanned<TypeExpr>]>,
        args: &[CallArg],
        span: &Span,
        arg_application: SignatureArgApplication<'_>,
        record_call_type_args: bool,
    ) -> AppliedCallSignature {
        let (freshened_params, freshened_ret, resolved_type_args) =
            self.instantiate_fn_sig_for_call(sig, type_args, span);

        match arg_application {
            SignatureArgApplication::PositionalOnly { arity_context } => {
                self.check_arity(args, freshened_params.len(), &arity_context, span);
                for (i, arg) in args.iter().enumerate() {
                    if let Some(param_ty) = freshened_params.get(i) {
                        let (expr, sp) = arg.expr();
                        self.check_against(expr, sp, param_ty);
                    }
                }
            }
            SignatureArgApplication::FunctionLike {
                param_names,
                accepts_kwargs,
                module_qualified,
            } => {
                let positional_count = args.iter().take_while(|arg| arg.name().is_none()).count();
                let positional_args = &args[..positional_count];
                let named_args = &args[positional_count..];

                if !accepts_kwargs && args.len() != freshened_params.len() {
                    let message = if module_qualified {
                        format!(
                            "expected {} arguments, found {}",
                            freshened_params.len(),
                            args.len()
                        )
                    } else {
                        format!(
                            "this function takes {} argument(s) but {} were supplied",
                            freshened_params.len(),
                            args.len()
                        )
                    };
                    self.report_error(TypeErrorKind::ArityMismatch, span, message);
                } else if accepts_kwargs && positional_count < freshened_params.len() {
                    let message = if module_qualified {
                        format!(
                            "expected at least {} positional arguments, found {}",
                            freshened_params.len(),
                            positional_count
                        )
                    } else {
                        format!(
                            "this function takes at least {} positional argument(s) but {} were supplied",
                            freshened_params.len(),
                            positional_count
                        )
                    };
                    self.report_error(TypeErrorKind::ArityMismatch, span, message);
                }

                for (i, arg) in positional_args.iter().enumerate() {
                    if let Some(param_ty) = freshened_params.get(i) {
                        let (expr, sp) = arg.expr();
                        self.check_against(expr, sp, param_ty);
                    }
                }

                for arg in named_args {
                    if let Some(name) = arg.name() {
                        if let Some(idx) = param_names.iter().position(|param| param == name) {
                            if let Some(param_ty) = freshened_params.get(idx) {
                                let (expr, sp) = arg.expr();
                                self.check_against(expr, sp, param_ty);
                            }
                        } else if !accepts_kwargs {
                            let (_, sp) = arg.expr();
                            self.report_error(
                                TypeErrorKind::InvalidOperation,
                                sp,
                                format!("unknown named argument `{name}`"),
                            );
                        } else {
                            let (expr, sp) = arg.expr();
                            self.synthesize(expr, sp);
                        }
                    }
                }
            }
        }

        self.enforce_type_param_bounds_with_assoc(
            sig,
            type_param_assoc_bindings,
            &resolved_type_args,
            span,
        );

        if record_call_type_args && !sig.type_params.is_empty() {
            self.record_concrete_call_type_args(span, &resolved_type_args);
        }

        AppliedCallSignature {
            params: freshened_params,
            return_type: freshened_ret,
        }
    }

    #[expect(
        clippy::too_many_lines,
        reason = "expected-type constructor checking shares variant, Option/Result, and Vec::new context"
    )]
    pub(super) fn check_call_against_expected_constructor(
        &mut self,
        func: &Spanned<Expr>,
        type_args: Option<&[Spanned<TypeExpr>]>,
        args: &[CallArg],
        expected: &Ty,
        span: &Span,
    ) -> Option<Ty> {
        // Resolve the function name first so we can route turbofish for
        // `Vec::new` before the blanket early-return for other constructors.
        let mut contextual_name = None;
        let func_name = match &func.0 {
            Expr::Identifier(name) => name.clone(),
            Expr::ContextVariant(context) => {
                let Some(owner) = self.context_variant_expected_owner(expected, span) else {
                    for arg in args {
                        let (expr, arg_span) = arg.expr();
                        self.synthesize(expr, arg_span);
                    }
                    return Some(Ty::Error);
                };
                contextual_name = Some(context.name.clone());
                format!("{owner}::{}", context.name)
            }
            Expr::FieldAccess { object, field } => {
                let Expr::Identifier(obj_name) = &object.0 else {
                    return None;
                };
                format!("{obj_name}::{field}")
            }
            _ => return None,
        };

        // Constructors with explicit type args that are not covered here fall
        // through to the generic call resolver.
        if type_args.is_some()
            && !matches!(
                func_name.as_str(),
                "Vec::new" | "HashMap::new" | "HashSet::new"
            )
        {
            return None;
        }

        let resolved_expected = self.subst.resolve(expected);

        if let Some(context_name) = contextual_name.as_deref() {
            let owner = func_name
                .rsplit_once("::")
                .map_or(func_name.as_str(), |(owner, _)| owner);
            match self.context_variant_definition(owner, context_name) {
                Some(VariantDef::Tuple(_)) => {}
                Some(_) => {
                    for arg in args {
                        let (expr, arg_span) = arg.expr();
                        self.synthesize(expr, arg_span);
                    }
                    self.report_error(
                        TypeErrorKind::PathKindMismatch,
                        span,
                        format!(
                            "E_PATH_KIND_MISMATCH: variant `{func_name}` is not a tuple constructor"
                        ),
                    );
                    return Some(Ty::Error);
                }
                None => {
                    for arg in args {
                        let (expr, arg_span) = arg.expr();
                        self.synthesize(expr, arg_span);
                    }
                    self.report_error(
                        TypeErrorKind::PathMemberNotFound,
                        span,
                        format!(
                            "E_PATH_MEMBER_NOT_FOUND: expected type `{}` has no variant `{context_name}`",
                            resolved_expected.user_facing()
                        ),
                    );
                    return Some(Ty::Error);
                }
            }
        }

        if let Some(targs) = type_args {
            match func_name.as_str() {
                "HashMap::new" => {
                    self.check_arity(args, 0, "`HashMap::new`", span);
                    let Some(result_ty) = self.lower_turbofish_collection_constructor(
                        "HashMap",
                        crate::BuiltinType::HashMap,
                        2,
                        targs,
                        span,
                    ) else {
                        self.record_type(span, &Ty::Error);
                        return Some(Ty::Error);
                    };
                    return Some(result_ty);
                }
                "HashSet::new" => {
                    self.check_arity(args, 0, "`HashSet::new`", span);
                    let Some(result_ty) = self.lower_turbofish_collection_constructor(
                        "HashSet",
                        crate::BuiltinType::HashSet,
                        1,
                        targs,
                        span,
                    ) else {
                        self.record_type(span, &Ty::Error);
                        return Some(Ty::Error);
                    };
                    return Some(result_ty);
                }
                _ => {}
            }
        }

        if func_name == "Vec::new" {
            self.check_arity(args, 0, "`Vec::new`", span);

            // Determine element type. Turbofish (`Vec::<T>::new()` or
            // `Vec::new::<T>()`) takes priority over the expected-type
            // annotation path (`let v: Vec<T> = Vec::new()`).
            let elem_ty: Ty = if let Some(targs) = type_args {
                let Some(mut lowered) = self.lower_turbofish_elem("Vec", 1, targs, span) else {
                    self.record_type(span, &Ty::Error);
                    return Some(Ty::Error);
                };
                lowered.remove(0)
            } else {
                // Expected-type path: infer element type from the surrounding
                // `Vec<T>` annotation.
                let Ty::Named {
                    name,
                    args: vec_args,
                    ..
                } = &resolved_expected
                else {
                    return None;
                };
                if name != "Vec" || vec_args.len() != 1 {
                    return None;
                }
                self.subst.resolve(&vec_args[0])
            };

            if matches!(elem_ty, Ty::Var(_)) {
                // Element type still unresolved — return a Vec<Var> or the
                // expected type as-is (both are valid deferred placeholders).
                let result_ty = if type_args.is_some() {
                    Ty::Named {
                        builtin: Some(crate::BuiltinType::Vec),
                        name: "Vec".to_string(),
                        args: vec![elem_ty],
                    }
                } else {
                    resolved_expected.clone()
                };
                self.record_type(span, &result_ty);
                // The ELEMENT type is deferred here; the CALLEE identity is not.
                // `Vec::new` resolves to one runtime family whatever `T` turns
                // out to be, and HIR treats an ordinary call that reaches
                // lowering without a `direct_call_targets` entry as a hard
                // boundary violation. Deferring the fact alongside the element
                // type is what made a generic record-literal field initializer
                // (`Bag { items: Vec::new() }`, `T` seeded from the field) fail
                // closed at the HIR boundary while its annotated sibling
                // (`let b: Bag<i64> = …`) compiled.
                self.record_direct_call_target(
                    span,
                    CallTarget::Runtime(crate::runtime_call::RuntimeCallFamily::VecNew),
                );
                return Some(result_ty);
            }
            if matches!(elem_ty, Ty::Error) {
                self.record_type(span, &Ty::Error);
                return Some(Ty::Error);
            }
            // #2647 — converge the MIR indirect-enum reject here. An indirect
            // enum takes the `Ptr` token, so it bypasses the `Layout`-gated
            // admissibility check below and is admitted today, then fails closed
            // deep in MIR. Reject it at the checker boundary with the same
            // release-protocol reason.
            if let Some(reason) = self.indirect_enum_vec_element_reject_reason(&elem_ty) {
                self.report_error(
                    TypeErrorKind::InvalidOperation,
                    span,
                    format!(
                        "`{}` cannot be a `Vec` element: {reason}",
                        elem_ty.user_facing()
                    ),
                );
                self.record_type(span, &Ty::Error);
                return Some(Ty::Error);
            }
            if crate::vec_authority::classify_element(&elem_ty, &self.type_defs)
                == Some(crate::vec_authority::VecElementToken::Layout)
                && matches!(elem_ty, Ty::Named { .. })
                // An element that still carries an unresolved type parameter
                // (`Option<T>`, `W<T>`) cannot have its Copy-vs-owned verdict
                // decided on the generic spine — the abstract `T` reads
                // non-Copy AND non-owned-admissible for a builtin nominal, which
                // would falsely reject `Vec<Option<T>>::new()`. Admit here and
                // let the per-monomorphisation resolver classify the substituted
                // element (fail-closed there if genuinely unsupported) — the
                // same deferral the element-typed method resolution takes (#2737).
                && !self.vec_element_contains_abstract_type_param(&elem_ty)
            {
                let is_copy = self.vec_element_has_copy_layout(&elem_ty);
                // W5.016: admit a non-Copy record/enum element with a
                // synthesizable owned thunk path (constructed through the owned
                // ABI). Stays fail-closed for elements with no thunk path.
                if !is_copy && !self.vec_owned_element_admissible(&elem_ty) {
                    let reason = self.vec_element_rejection_reason(&elem_ty);
                    self.report_error(
                        TypeErrorKind::InvalidOperation,
                        span,
                        format!(
                            "`{}` cannot be a `Vec` element: {reason}",
                            elem_ty.user_facing()
                        ),
                    );
                    self.record_type(span, &Ty::Error);
                    return Some(Ty::Error);
                }
            }
            // Construct and record the result type. Turbofish builds Vec<elem_ty>
            // directly; the expected-type path returns resolved_expected (which
            // already has the correct Vec<T> shape).
            let result_ty = if type_args.is_some() {
                Ty::Named {
                    builtin: Some(crate::BuiltinType::Vec),
                    name: "Vec".to_string(),
                    args: vec![self.subst.resolve(&elem_ty)],
                }
            } else {
                resolved_expected.clone()
            };
            self.record_type(span, &result_ty);
            self.record_direct_call_target(
                span,
                CallTarget::Runtime(crate::runtime_call::RuntimeCallFamily::VecNew),
            );
            return Some(result_ty);
        }

        let Ok(canonical_lifecycle) =
            self.canonicalize_source_lifecycle_value_path(&func_name, span)
        else {
            return Some(Ty::Error);
        };
        let constructor_name = canonical_lifecycle.as_deref().unwrap_or(&func_name);
        if let Some((type_name, expected_params, type_params)) =
            self.lookup_variant_constructor(constructor_name)
        {
            if contextual_name.is_none() && !func_name.contains("::") {
                self.warn_bare_variant_expr(&func_name, span);
            }
            let mut inferred_args = self.expected_constructor_type_args(
                &resolved_expected,
                &type_name,
                type_params.len(),
            )?;
            self.check_arity(args, expected_params.len(), "this function", span);
            {
                let subst_map: HashMap<String, Ty> = type_params
                    .iter()
                    .zip(inferred_args.iter())
                    .map(|(p, a)| (p.clone(), a.clone()))
                    .collect();
                for (i, arg) in args.iter().enumerate() {
                    if let Some(param_ty) = expected_params.get(i) {
                        let (expr, arg_span) = arg.expr();
                        let expected_ty = param_ty.substitute_named_params_parallel(&subst_map);
                        self.check_against(expr, arg_span, &expected_ty);
                    }
                }
            }
            let resolved_args: Vec<Ty> = inferred_args
                .drain(..)
                .map(|ty| self.subst.resolve(&ty))
                .collect();
            self.enforce_type_def_instantiation_bounds(&type_name, &resolved_args, span);
            let result_ty = Self::variant_nominal_from_expected(&resolved_expected, resolved_args)
                .expect("constructor expected-type match requires a named nominal");
            self.record_type(span, &result_ty);
            return Some(result_ty);
        }

        if let Some(context_name) = contextual_name {
            for arg in args {
                let (expr, arg_span) = arg.expr();
                self.synthesize(expr, arg_span);
            }
            self.report_error(
                TypeErrorKind::PathMemberNotFound,
                span,
                format!(
                    "E_PATH_MEMBER_NOT_FOUND: expected type `{}` has no tuple variant `{context_name}`",
                    resolved_expected.user_facing()
                ),
            );
            return Some(Ty::Error);
        }

        match func_name.as_str() {
            "Some" => {
                let inner_ty = resolved_expected.as_option()?.clone();
                self.check_arity(args, 1, "`Some`", span);
                if let Some(arg) = args.first() {
                    let (expr, arg_span) = arg.expr();
                    self.check_against(expr, arg_span, &inner_ty);
                }
                let result_ty = Ty::option(self.subst.resolve(&inner_ty));
                self.record_type(span, &result_ty);
                Some(result_ty)
            }
            "Ok" => {
                let (ok_ty, err_ty) = resolved_expected.as_result()?;
                self.check_arity(args, 1, "`Ok`", span);
                if let Some(arg) = args.first() {
                    let (expr, arg_span) = arg.expr();
                    self.check_against(expr, arg_span, ok_ty);
                }
                self.record_builtin_result_output_type_args(span, ok_ty, err_ty);
                let result_ty = Ty::result(self.subst.resolve(ok_ty), self.subst.resolve(err_ty));
                self.record_type(span, &result_ty);
                Some(result_ty)
            }
            "Err" => {
                let (ok_ty, err_ty) = resolved_expected.as_result()?;
                self.check_arity(args, 1, "`Err`", span);
                if let Some(arg) = args.first() {
                    let (expr, arg_span) = arg.expr();
                    self.check_against(expr, arg_span, err_ty);
                }
                self.record_builtin_result_output_type_args(span, ok_ty, err_ty);
                let result_ty = Ty::result(self.subst.resolve(ok_ty), self.subst.resolve(err_ty));
                self.record_type(span, &result_ty);
                Some(result_ty)
            }
            _ => None,
        }
    }

    pub(super) fn reject_if_wasm_incompatible_call(&mut self, func_name: &str, span: &Span) {
        if !self.wasm_target {
            return;
        }
        // These diagnostics exist to fail closed on the *stdlib primitives*
        // (`sleep`, `link`, `random_bytes`, `Node::*`, ...) that have no
        // wasm32 implementation. They must not fire for an ordinary user
        // function that merely happens to share one of these bare names —
        // `fn_sigs` cannot distinguish the two (a user declaration silently
        // overwrites the builtin's `register_builtin_fn` entry at the same
        // key), but `fn_def_spans` is populated only from real source-level
        // `Item::Function` declarations (see `collect_function_item`) and
        // never from builtin registration, so its presence for this exact
        // resolved name is a reliable "the user defined this symbol" signal.
        // Mirror the module-qualified-first resolution `check_call` itself
        // uses below so a module-scoped shadow (`mod.sleep`) is caught too.
        // rc1-F1 stage A: probe under the CANONICAL owner (root included) —
        // `fn_def_spans` keys root declarations canonically.
        let resolved_name = scoped_module_item_name(self.canonical_fn_owner(), func_name)
            .filter(|qualified| self.fn_def_spans.contains_key(qualified))
            .unwrap_or_else(|| func_name.to_string());
        if self.fn_def_spans.contains_key(&resolved_name) {
            return;
        }
        match func_name {
            "link" | "unlink" | "monitor" | "demonitor" | "link_remote" => {
                self.reject_wasm_feature(span, WasmUnsupportedFeature::LinkMonitor);
            }
            "supervisor_child" | "supervisor_stop" => {
                self.reject_wasm_feature(span, WasmUnsupportedFeature::SupervisionTrees);
            }
            // sleep / sleep_until: the wasm32 scheduler parks the calling actor
            // at the message boundary and re-enqueues it once the deadline passes
            // (see hew-runtime/src/scheduler_wasm.rs :: park_actor_sleep).
            // Semantics are cooperative: code after sleep in the same handler
            // runs before the park takes effect.  Warn rather than reject so
            // WASM programs can use timers with the degraded-semantics caveat.
            "sleep" | "sleep_until" => {
                self.warn_wasm_limitation(span, WasmUnsupportedFeature::Timers);
            }
            // crypto.random_bytes depends on a secure native-only entropy source
            // that is absent from the wasm32 link set. Reject at check time so
            // secure randomness fails closed instead of compiling to a non-secure
            // host import or fallback.
            "random_bytes" => {
                self.reject_wasm_feature(span, WasmUnsupportedFeature::CryptoRandom);
            }
            // The `Node::*` distributed cluster API (`Node::start`,
            // `Node::connect`, `Node::load_keys`, `Node::register`,
            // `Node::lookup`, …) lowers to the native mesh transport
            // (`hew_node_api_*`), which is not compiled for wasm32. Reject at
            // check time so the call fails closed with a structured diagnostic
            // instead of compiling to a wasm module that imports an undefined
            // `env::hew_node_api_*` symbol and traps at instantiation.
            name if crate::runtime_call::RuntimeCallFamily::from_c_symbol(name).is_some_and(
                |family| {
                    family.runtime_capability()
                        == Some(crate::runtime_call::RuntimeCapability::Node)
                },
            ) =>
            {
                self.reject_wasm_feature(span, WasmUnsupportedFeature::Distributed);
            }
            _ => {}
        }
    }

    /// Emit a `BlockingCallInReceiveFn` warning when a known blocking operation
    /// is called from inside an actor receive function.
    /// Await-suspending forms must be filtered before calling this helper.
    ///
    /// Actor receive functions run synchronously on scheduler worker threads.
    /// A blocking call (e.g. `recv`, `read`, `accept`) will stall that thread
    /// for the duration of the wait, preventing other actors from being
    /// scheduled and potentially causing deadlocks when all worker threads
    /// are occupied by blocked receive handlers.
    ///
    /// `op_desc` should be a short human-readable label such as
    /// `"Receiver::recv"` or `"net.Connection::read"`.
    pub(super) fn warn_if_blocking_in_receive_fn(&mut self, op_desc: &str, span: &Span) {
        self.warn_if_blocking_in_receive_fn_with_fix(op_desc, span, None);
    }

    /// Same warning as [`Self::warn_if_blocking_in_receive_fn`], with an
    /// optional caller-supplied `(remedy clause, suggestion)` pair in place
    /// of the generic "send it as a message" text. `warn_if_blocking_handle_method`
    /// uses this to point `accept`/`read` at their already-shipping `await`
    /// forms instead of the generic redesign-your-actor advice — those two
    /// ops have a direct, drop-in suspending replacement; most other
    /// blocking calls (e.g. `Receiver::recv`) do not.
    fn warn_if_blocking_in_receive_fn_with_fix(
        &mut self,
        op_desc: &str,
        span: &Span,
        fix: Option<(&str, String)>,
    ) {
        if !self.in_receive_fn {
            return;
        }
        let (remedy_clause, suggestion) = fix.unwrap_or((
            "consider passing the value in via a message instead",
            "send the blocking work to a dedicated actor or async task and \
             deliver the result as a message"
                .to_string(),
        ));
        self.warnings.push(TypeError {
            severity: crate::error::Severity::Warning,
            kind: TypeErrorKind::BlockingCallInReceiveFn,
            span: span.clone(),
            message: format!(
                "blocking call `{op_desc}` inside an actor receive function \
                 can stall the scheduler thread and cause deadlocks; {remedy_clause}"
            ),
            notes: vec![(
                span.clone(),
                "actor receive functions run synchronously on scheduler worker threads".to_string(),
                self.current_module.clone(),
            )],
            suggestions: vec![suggestion],
            source_module: self.current_module.clone(),
        });
    }

    pub(super) fn warn_if_blocking_handle_method(
        &mut self,
        type_name: &str,
        method: &str,
        span: &Span,
    ) {
        if matches!(
            (type_name, method),
            ("http.Server" | crate::stdlib::STD_NET_LISTENER, "accept")
                | (crate::stdlib::STD_NET_CONNECTION, "read")
        ) {
            let suggestion = "use the suspending form instead: `await` the call (e.g. \
                 `await listener.accept()` or `await conn.read()`) — it parks the \
                 actor on the reactor instead of blocking the worker thread, so \
                 the scheduler worker stays free and the process can shut down \
                 promptly; see examples/net/http_await_service.hew"
                .to_string();
            self.warn_if_blocking_in_receive_fn_with_fix(
                &format!("{type_name}::{method}"),
                span,
                Some((
                    "use the suspending `await` form instead of the blocking call",
                    suggestion,
                )),
            );
        }
    }

    /// Resolve a namespaced module-qualified call `module::fn(args)` against an
    /// imported module's free functions, mirroring the dot-form dispatch in
    /// `check_method_call`.
    ///
    /// Returns `Some(ret_ty)` when the call names a known module and the
    /// `module.fn` registry key resolves (including a reported `Ty::Error` for
    /// a visibility violation), and `None` when `func_name` is not a
    /// module-qualified call, the module is unknown, or no `module.fn` key
    /// exists — leaving the existing `undefined function` diagnostic to fire.
    #[expect(
        clippy::too_many_lines,
        reason = "module-qualified calls validate visibility, target support, and ownership"
    )]
    fn try_check_namespaced_module_call(
        &mut self,
        func_name: &str,
        args: &[CallArg],
        span: &Span,
    ) -> Option<Ty> {
        let (module_name, method) = func_name.split_once("::")?;
        // A trait-qualified call (`Trait::method`) is handled by the dedicated
        // paths above and must not be re-interpreted as a module call.
        if self.trait_defs.contains_key(module_name) {
            return None;
        }
        if method.contains("::") {
            return None;
        }
        if !self.module_binding_in_current_file(module_name) {
            return None;
        }
        // The parsed qualifier is only a lexical binding. Resolve it through
        // the import table before consulting declaration/export authority so
        // nested modules and aliases retain their exact source identity.
        let canonical_owner = self.canonical_module_import_owner(module_name);
        let key = format!("{canonical_owner}.{method}");
        if !self.fn_sigs.contains_key(&key) {
            return None;
        }
        if self.module_binding_in_current_file(module_name) {
            self.used_modules.borrow_mut().insert(ImportKey::in_file(
                self.current_module.clone(),
                self.current_module_idx,
                module_name.to_string(),
            ));
        }
        // Export gate: only `pub` functions are reachable across the module
        // boundary, mirroring the dot-form path. A `package fn` accessible
        // within the same package falls through to the success branch.
        if !self.module_fn_exports.contains(&key) {
            if let Some(&vis) = self.fn_visibility.get(&key) {
                let decl_module_owned = self.fn_def_spans.get(&key).and_then(|(_, m)| m.clone());
                let decl_span_owned = self
                    .fn_def_spans
                    .get(&key)
                    .map_or_else(|| span.clone(), |(s, _)| s.clone());
                let acc_module_owned = self.current_module.clone();
                if !visibility::access_allowed(
                    decl_module_owned.as_deref(),
                    acc_module_owned.as_deref(),
                    vis,
                ) {
                    for arg in args {
                        let (expr, sp) = arg.expr();
                        self.synthesize(expr, sp);
                    }
                    let acc_module_str =
                        acc_module_owned.as_deref().unwrap_or("(root)").to_string();
                    let err = TypeError::visibility_violation(
                        vis,
                        span.clone(),
                        method,
                        decl_module_owned.as_deref().unwrap_or("(root)"),
                        &acc_module_str,
                        decl_span_owned,
                        acc_module_owned,
                    );
                    self.errors.push(err);
                    return Some(Ty::Error);
                }
                // access_allowed: `package fn` reachable from this package.
            } else {
                return None;
            }
        }
        self.require_unsafe(&key, span);
        if let Some(feature) = self.wasm_native_only_module_feature(module_name) {
            self.reject_wasm_feature(span, feature);
        }
        self.reject_wasm_native_only_module_function(module_name, method, span);
        if self.is_shipped_crypto_module(module_name) && method == "random_bytes" {
            self.reject_wasm_feature(span, WasmUnsupportedFeature::CryptoRandom);
        }
        let sig = self.fn_sigs.get(&key).cloned()?;
        if let Some(caller) = &self.current_function {
            self.call_graph
                .entry(caller.clone())
                .or_default()
                .insert(key.clone());
        }
        self.record_module_qualified_stdlib_call_rewrite_if_any(module_name, method, span);
        self.record_module_qualified_user_call_rewrite_if_any(module_name, method, span);
        let assoc_bindings = self
            .fn_type_param_assoc_bindings
            .get(&key)
            .cloned()
            .unwrap_or_default();
        let applied_sig = self.apply_instantiated_call_signature_with_assoc(
            &sig,
            &assoc_bindings,
            None,
            args,
            span,
            SignatureArgApplication::FunctionLike {
                param_names: &sig.param_names,
                accepts_kwargs: sig.accepts_kwargs,
                module_qualified: true,
            },
            true,
        );
        self.record_resolved_direct_call_ownership(
            &key,
            &sig,
            args,
            &applied_sig.return_type,
            span,
        );
        // A module-qualified call has one canonical target shared by its
        // rewrite and its ordinary-call fact.  The signature key above may be
        // a lexical compatibility alias, so never mint a second identity from
        // it after the module resolver already selected the source owner.
        let rewrite_target = self
            .method_call_rewrites
            .get(&SpanKey::in_module(span, self.current_module_idx))
            .and_then(|rewrite| match rewrite {
                crate::MethodCallRewrite::RewriteModuleQualifiedToFunction { target, .. } => {
                    Some(target.clone())
                }
                _ => None,
            });
        self.record_direct_call_target(
            span,
            rewrite_target.unwrap_or_else(|| self.call_target_for_signature(&key)),
        );
        Some(applied_sig.return_type)
    }

    /// Publish the canonical target of an admitted ordinary call.  This is the
    /// authority boundary for `HirExprKind::Call`; lowerings never recover it
    /// from a callee name or a linker symbol.
    pub(super) fn record_direct_call_target(&mut self, span: &Span, target: CallTarget) {
        self.direct_call_targets
            .insert(SpanKey::in_module(span, self.current_module_idx), target);
    }

    /// Recover a compiler intrinsic only through the exact declaration mapping
    /// produced during canonical-floor registration.  The source spelling may
    /// be an imported module alias or a named-import alias, but both paths
    /// retain the declaring key; never infer an intrinsic from a callee leaf.
    pub(super) fn intrinsic_runtime_target_for_signature(
        &self,
        signature_key: &str,
    ) -> Option<crate::runtime_call::RuntimeCallFamily> {
        let intrinsic_key = self
            .intrinsic_declarations
            .get(signature_key)
            .or_else(|| {
                let (surface_module, source_leaf) = signature_key.rsplit_once('.')?;
                let source_module = self.module_import_bindings.get(&(
                    self.current_module.clone(),
                    self.current_module_idx,
                    surface_module.to_string(),
                ))?;
                self.intrinsic_declarations
                    .get(&format!("{source_module}.{source_leaf}"))
            })
            .or_else(|| {
                let source_key = self.import_fn_name_aliases.get(&(
                    self.current_module.clone(),
                    self.current_module_idx,
                    signature_key.to_string(),
                ))?;
                self.intrinsic_declarations.get(source_key)
            })?;

        // `abs`/`min`/`max` are a single source/catalog identity with a
        // closed overload chosen from the resolved operand type. Their
        // `GenericMathIntrinsic` rewrite carries that type-directed choice;
        // never freeze one of the overloads here.
        if matches!(intrinsic_key.as_str(), "math.abs" | "math.min" | "math.max") {
            return None;
        }
        let math_symbol = intrinsic_key.strip_prefix("math.")?;
        match crate::runtime_call::RuntimeCallFamily::from_c_symbol(math_symbol) {
            Some(family @ crate::runtime_call::RuntimeCallFamily::MathIntrinsic(_)) => Some(family),
            _ => None,
        }
    }

    /// Return the type-directed generic math operation carried by an exact
    /// intrinsic declaration. This preserves source/alias authority while HIR
    /// selects its closed i64/f64 runtime discriminator from checked types.
    pub(super) fn intrinsic_math_generic_op_for_signature(
        &self,
        signature_key: &str,
    ) -> Option<crate::MathGenericOp> {
        let intrinsic_key = self
            .intrinsic_declarations
            .get(signature_key)
            .or_else(|| {
                let (surface_module, source_leaf) = signature_key.rsplit_once('.')?;
                let source_module = self.module_import_bindings.get(&(
                    self.current_module.clone(),
                    self.current_module_idx,
                    surface_module.to_string(),
                ))?;
                self.intrinsic_declarations
                    .get(&format!("{source_module}.{source_leaf}"))
            })
            .or_else(|| {
                let source_key = self.import_fn_name_aliases.get(&(
                    self.current_module.clone(),
                    self.current_module_idx,
                    signature_key.to_string(),
                ))?;
                self.intrinsic_declarations.get(source_key)
            })?;
        match intrinsic_key.as_str() {
            "math.abs" => Some(crate::MathGenericOp::Abs),
            "math.min" => Some(crate::MathGenericOp::Min),
            "math.max" => Some(crate::MathGenericOp::Max),
            _ => None,
        }
    }

    /// Publish the `CallTarget::User` for a source-declared function found in
    /// `fn_def_spans` under `signature_key`, or `None` when no declaration
    /// exists under that key.
    ///
    /// LEGACY ROOT RENDER (rc1-F1 stage A): the checker keys root free
    /// functions canonically (`{root}.{name}`), but the published `DefId`
    /// must keep the legacy bare spelling — HIR/MIR/codegen still derive
    /// symbols and lookups from it, and stage A must be byte-identical
    /// downstream. The stored declaring module (`None` = root) selects the
    /// render: root declarations publish their bare leaf, module
    /// declarations publish `{module}.{leaf}`.
    /// WHEN OBSOLETE: the rc2 identity continuation's render-canonicalization
    /// stage re-keys downstream consumers by canonical `DefId`; this render
    /// then publishes the canonical key unchanged. rc1 stages D and E
    /// deliberately do NOT delete it — doing so renames every root symbol.
    fn user_call_target_for_declared_fn(&self, signature_key: &str) -> Option<CallTarget> {
        let (_, declaring_module) = self.fn_def_spans.get(signature_key)?;
        let declaration = declaring_module.as_ref().map_or_else(
            || {
                self.root_owned_fn_leaf(signature_key)
                    .unwrap_or(signature_key)
                    .to_string()
            },
            |module| {
                let name = signature_key.rsplit('.').next().unwrap_or(signature_key);
                format!("{module}.{name}")
            },
        );
        Some(CallTarget::User(crate::DefId::new(declaration)))
    }

    #[expect(
        clippy::too_many_lines,
        reason = "call-target precedence stays explicit in one resolution ladder"
    )]
    fn call_target_for_signature(&self, signature_key: &str) -> CallTarget {
        // Extern declarations are source declarations too and may therefore
        // also have an fn_def_spans entry. Classify them first: their exact
        // declaration identity is semantic authority, while the validated ABI
        // symbol is the executable endpoint. Treating them as ordinary User
        // calls loses that endpoint and leaves MIR without a symbol mapping.
        if let Some(extern_decl) = self.extern_table.declaration(signature_key) {
            if extern_decl.symbol.is_empty() {
                return CallTarget::Unsupported {
                    reason: format!(
                        "generic extern declaration `{signature_key}` has no monomorphic endpoint"
                    ),
                };
            }
            // Provenance is per DECLARATION: the published identity is the
            // declaration used at the call site, and `trusted_compiled_stdlib`
            // derives from ITS declaring module — never from whichever
            // declaration minted the symbol's ABI contract (a user extern
            // stays user-provenance even when its spelling collides with an
            // audited runtime endpoint, in either registration order).
            //
            // LEGACY ROOT RENDER (rc1-F1 stage B, mirrors stage A's fn_sigs
            // publication): a root extern declaration is keyed canonically
            // inside the checker but publishes its bare leaf, because HIR
            // still resolves root declarations by source spelling.
            // WHEN OBSOLETE: the rc2 identity continuation's
            // render-canonicalization stage re-keys downstream consumers by
            // DefId.
            let declaration = self
                .root_owned_fn_leaf(signature_key)
                .unwrap_or(signature_key)
                .to_string();
            return CallTarget::Extern {
                declaration: crate::DefId::new(declaration),
                endpoint: extern_decl.symbol.clone(),
                trusted_compiled_stdlib: extern_decl
                    .declaring_module
                    .as_deref()
                    .is_some_and(|module| self.canonical_std_module_sources.contains(module)),
            };
        }
        if let Some(family) = self.intrinsic_runtime_target_for_signature(signature_key) {
            return CallTarget::Runtime(family);
        }
        if !self.fn_def_spans.contains_key(signature_key) {
            if let Some(declaration) = self.impl_method_declaration_ids.get(signature_key) {
                if !declaration.full_path().starts_with("std.builtins.") {
                    return CallTarget::ImplMethod(declaration.clone());
                }
            }
        }
        if let Some(target) = self.user_call_target_for_declared_fn(signature_key) {
            return target;
        }
        // A source declaration always wins over a catalog spelling.  Once that
        // authority check above has ruled it out, every executable monomorphic
        // catalog builtin publishes its exact catalog identity.  HIR/MIR use
        // that identity to select the catalog row and its linkage; they never
        // reclassify a callee by its leaf spelling.  In particular, this keeps
        // `assert(true)` on the same path as every other catalog FFI shim.
        if let Some(endpoint) =
            crate::stdlib_catalog_identity::monomorphic_callable_identity(signature_key)
        {
            return CallTarget::Builtin {
                endpoint: endpoint.to_string(),
            };
        }
        // Compiler-registered source builtins outside the executable stdlib
        // catalog have no source declaration span. Publish their typed runtime
        // families only after the source and catalog identity checks above.
        match signature_key {
            "link" => {
                return CallTarget::Runtime(crate::runtime_call::RuntimeCallFamily::ActorLink);
            }
            "link_remote" => {
                return CallTarget::Runtime(crate::runtime_call::RuntimeCallFamily::LinkRemote);
            }
            "monitor" => {
                return CallTarget::Runtime(crate::runtime_call::RuntimeCallFamily::ActorMonitor);
            }
            "unlink" => {
                return CallTarget::Runtime(crate::runtime_call::RuntimeCallFamily::ActorUnlink);
            }
            "supervisor_stop" => {
                return CallTarget::Runtime(crate::runtime_call::RuntimeCallFamily::SupervisorStop);
            }
            _ => {}
        }
        // A whole-module import may expose a source declaration through an
        // arbitrary local binding (`import left::render as left_render`).  The
        // signature registry deliberately keeps that surface key
        // (`left_render.identity`) for checking, but it is not a declaration
        // identity.  Re-anchor it through the exact importer binding before
        // publishing a call target.  In particular, never recover ownership
        // by stripping the module leaf: two nested modules can share both the
        // leaf and every exported function name.
        if let Some((surface_module, source_leaf)) = signature_key.rsplit_once('.') {
            if let Some(source_module) = self.module_import_bindings.get(&(
                self.current_module.clone(),
                self.current_module_idx,
                surface_module.to_string(),
            )) {
                let declaration = format!("{source_module}.{source_leaf}");
                // Reaching this branch means the checker has already admitted
                // the signature under `signature_key`; the binding supplies
                // the missing declaration owner even when the signature was
                // populated by an earlier graph-registration pass that did
                // not retain a second `fn_def_spans` compatibility entry.
                return CallTarget::User(crate::DefId::new(declaration));
            }
        }
        // rc1-F1 stage A: a bare spelling that reaches this rung re-anchors
        // through the CANONICAL owner (current module, or the root unit's
        // minted identity) — `fn_def_spans` is canonically keyed, so the
        // bare-for-root probe would silently miss. The publication render is
        // the declared-fn helper's (root declarations publish bare leaves).
        if let Some(target) = scoped_module_item_name(self.canonical_fn_owner(), signature_key)
            .and_then(|source_key| self.user_call_target_for_declared_fn(&source_key))
        {
            return target;
        }
        if let Some(source_key) = self.import_fn_name_aliases.get(&(
            self.current_module.clone(),
            self.current_module_idx,
            signature_key.to_string(),
        )) {
            return CallTarget::User(crate::DefId::new(source_key));
        }
        // Compiler-registered builtins have no source declaration span. Their
        // executable identity comes from the typed registry populated during
        // builtin registration, never by reconstructing an ABI symbol from a
        // signature string at this call-site boundary.
        if let Some(family) = self.runtime_builtin_targets.get(signature_key) {
            return CallTarget::Runtime(*family);
        }
        // These three layout-witness calls are compiler-intercepted ABI
        // helpers declared synthetically while checking the shipped channel
        // source. Their source key deliberately keeps its full owner, whereas
        // the runtime catalogue is keyed by the ABI symbol alone. Bridge that
        // representation boundary only after the module graph has proved the
        // exact stdlib source owner; a user package named `channel` (or a
        // spoofed `std.channel` module) must not acquire a runtime
        // call target merely by sharing these leaves.
        if self.canonical_std_module_sources.contains("std.channel") {
            if let Some(c_symbol) = signature_key.strip_prefix("std.channel.") {
                if let Some(family) =
                    crate::runtime_call::RuntimeCallFamily::from_c_symbol(c_symbol)
                {
                    return CallTarget::Runtime(family);
                }
            }
        }
        if let Some(family) = crate::runtime_call::RuntimeCallFamily::from_c_symbol(signature_key) {
            return CallTarget::Runtime(family);
        }
        CallTarget::Unsupported {
            reason: format!(
                "checker admitted `{signature_key}` without a source declaration or runtime family"
            ),
        }
    }

    pub(super) fn record_resolved_direct_call_ownership(
        &mut self,
        signature_key: &str,
        sig: &FnSig,
        args: &[CallArg],
        result_ty: &Ty,
        span: &Span,
    ) {
        use crate::runtime_call::{
            ProducedArgumentBoundary as Boundary, ProducedValueOwnership as Ownership,
        };

        let formal_modes = self
            .fn_param_ownership
            .get(signature_key)
            .cloned()
            .unwrap_or_else(|| vec![Boundary::Unknown; sig.params.len()]);
        let arguments = args
            .iter()
            .enumerate()
            .map(|(source_index, arg)| {
                let formal_index = arg
                    .name()
                    .and_then(|name| sig.param_names.iter().position(|formal| formal == name))
                    .unwrap_or(source_index);
                formal_modes
                    .get(formal_index)
                    .copied()
                    .unwrap_or(Boundary::Unknown)
            })
            .collect();
        let resolved_result_ty = self.subst.resolve(result_ty).materialize_literal_defaults();
        let non_owning = resolved_result_ty.is_copy()
            || self
                .registry
                .implements_marker(&resolved_result_ty, MarkerTrait::Copy);
        let builtin_result_ownership =
            crate::stdlib_catalog_identity::monomorphic_callable_identity(signature_key)
                .and_then(crate::runtime_call::RuntimeCallFamily::from_c_symbol)
                .map(crate::runtime_call::RuntimeCallFamily::result_ownership)
                .filter(|ownership| {
                    !matches!(
                        ownership,
                        crate::runtime_call::RuntimeResultOwnership::Untracked
                    )
                });
        // Per-declaration record: ownership provenance attributes to the
        // declaration used at this call site, not the symbol's contract
        // minter.
        let source_extern = self
            .extern_table
            .declaration(signature_key)
            .map(|extern_decl| {
                (
                    extern_decl.symbol.clone(),
                    extern_decl.declaring_module.clone(),
                )
            });
        let call_key = SpanKey::in_module(span, self.current_module_idx);
        let exact_extern_symbol = source_extern.as_ref().and_then(|(symbol, _)| {
            sig.extern_symbol.as_ref().map_or_else(
                || (!symbol.is_empty()).then(|| symbol.clone()),
                |spec| {
                    if spec.template.is_monomorphic() {
                        Some(spec.template.raw.clone())
                    } else {
                        self.call_type_args
                            .get(&call_key)
                            .and_then(|type_args| type_args.first())
                            .map(|ty| self.subst.resolve(ty).materialize_literal_defaults())
                            .and_then(|type_arg| {
                                spec.template.expand(&type_arg, &self.type_defs).ok()
                            })
                    }
                },
            )
        });
        self.produced_call_arities
            .insert(call_key.clone(), (false, args.len()));
        self.resolved_direct_call_ownership.insert(
            call_key,
            PendingDirectCallOwnership {
                fact: ProducedValueFact {
                    ownership: if non_owning {
                        Ownership::NoOwner
                    } else if builtin_result_ownership.is_some() {
                        Ownership::owned(crate::runtime_call::ProducedValueAcquisition::Fresh)
                    } else {
                        Ownership::Unknown
                    },
                    receiver_span: None,
                    receiver_boundary: None,
                    arguments,
                },
                extern_symbol: exact_extern_symbol,
                extern_declaring_module: source_extern.and_then(|(_, module)| module),
                extern_param_count: sig.params.len(),
                resolved_result_ty,
            },
        );
    }

    #[expect(
        clippy::too_many_lines,
        reason = "call checking covers many builtin and method signatures"
    )]
    pub(super) fn check_call(
        &mut self,
        func: &Spanned<Expr>,
        type_args: Option<&[Spanned<TypeExpr>]>,
        args: &[CallArg],
        span: &Span,
    ) -> Ty {
        if let Expr::ContextVariant(context) = &func.0 {
            for arg in args {
                let (expr, arg_span) = arg.expr();
                self.synthesize(expr, arg_span);
            }
            self.report_error(
                TypeErrorKind::ContextVariantNoType,
                span,
                format!(
                    "E_CONTEXT_VARIANT_NO_TYPE: contextual variant `.{}` requires an expected enum or machine type",
                    context.name
                ),
            );
            return Ty::Error;
        }
        // Get function name from expression
        let func_name = match &func.0 {
            Expr::Identifier(name) => name.clone(),
            Expr::FieldAccess { object, field } => {
                if let Expr::Identifier(obj_name) = &object.0 {
                    // When the object identifier names a live value binding,
                    // the callee is a field access on a value — not a
                    // module-qualified or type-namespaced name. Synthesise the
                    // field type and delegate to the indirect-call path so that
                    // `(rec.f)(args)` where `f: fn(A)->B` reaches
                    // `check_call_with_type` instead of failing with
                    // "undefined function `rec::f`".
                    //
                    // The `env.lookup_ref` guard keeps type and module
                    // identifiers on the existing name-building path
                    // (`(Vec.new)(args)` → `"Vec::new"`) because type/module
                    // names are not registered as value bindings.
                    if self.env.lookup_ref(obj_name).is_some() {
                        let field_ty = self.synthesize(&func.0, &func.1);
                        let resolved = self.subst.resolve(&field_ty);
                        return self.check_call_with_type(&resolved, args, span);
                    }
                    format!("{obj_name}::{field}")
                } else {
                    let func_ty = self.synthesize(&func.0, &func.1);
                    return self.check_call_with_type(&func_ty, args, span);
                }
            }
            _ => {
                let func_ty = self.synthesize(&func.0, &func.1);
                return self.check_call_with_type(&func_ty, args, span);
            }
        };

        if self.report_bare_function_import_ambiguity(&func_name, span) {
            return Ty::Error;
        }

        self.require_unsafe(&func_name, span);
        self.reject_if_wasm_incompatible_call(&func_name, span);
        if let Some(source_identity) = self
            .import_fn_name_aliases
            .get(&(
                self.current_module.clone(),
                self.current_module_idx,
                func_name.clone(),
            ))
            .cloned()
        {
            self.reject_wasm_native_only_function_identity(&source_identity, span);
        }

        // Check if name is a user-defined enum variant constructor first.
        // Separate lookup (immutable borrow) from processing (mutable borrow)
        // to avoid cloning the entire type_defs map.
        //
        // Handle both unqualified (`Circle(5)`) and qualified (`Shape::Circle(5)`) forms.
        let Ok(canonical_lifecycle) =
            self.canonicalize_source_lifecycle_value_path(&func_name, span)
        else {
            return Ty::Error;
        };
        let constructor_name = canonical_lifecycle.as_deref().unwrap_or(&func_name);
        let constructor_match = self.lookup_variant_constructor(constructor_name);
        if let Some((type_name, expected_params, type_params)) = constructor_match {
            if !func_name.contains("::") {
                self.warn_bare_variant_expr(&func_name, span);
            }
            let type_param_count = type_params.len();
            if type_param_count == 0 {
                if let Some(type_args_provided) = type_args {
                    if !type_args_provided.is_empty() {
                        self.report_error(
                            TypeErrorKind::ArityMismatch,
                            span,
                            format!(
                                "this constructor takes 0 type parameter(s) but {} type argument(s) were supplied",
                                type_args_provided.len()
                            ),
                        );
                    }
                }
            }
            let mut inferred_args = Vec::new();
            if type_param_count > 0 {
                if let Some(type_args_provided) = type_args {
                    if type_args_provided.len() != type_param_count {
                        self.report_error(
                            TypeErrorKind::ArityMismatch,
                            span,
                            format!(
                                "this constructor takes {} type parameter(s) but {} type argument(s) were supplied",
                                type_param_count,
                                type_args_provided.len()
                            ),
                        );
                    }
                    inferred_args = type_args_provided
                        .iter()
                        .take(type_param_count)
                        .map(|type_arg| self.resolve_type_expr(type_arg))
                        .collect();
                }
                while inferred_args.len() < type_param_count {
                    inferred_args.push(Ty::Var(TypeVar::fresh()));
                }
            }
            self.check_arity(args, expected_params.len(), "this function", span);
            {
                let subst_map: HashMap<String, Ty> = type_params
                    .iter()
                    .zip(inferred_args.iter())
                    .map(|(p, a)| (p.clone(), a.clone()))
                    .collect();
                for (i, arg) in args.iter().enumerate() {
                    if let Some(param_ty) = expected_params.get(i) {
                        let (expr, span) = arg.expr();
                        let expected_ty = if subst_map.is_empty() {
                            param_ty.clone()
                        } else {
                            param_ty.substitute_named_params_parallel(&subst_map)
                        };
                        self.check_against(expr, span, &expected_ty);
                    }
                }
            }
            let resolved_args: Vec<Ty> = inferred_args
                .iter()
                .map(|ty| self.subst.resolve(ty))
                .collect();
            self.enforce_type_def_instantiation_bounds(&type_name, &resolved_args, span);
            let result_ty = self.variant_nominal_ty(type_name, resolved_args);
            let non_owning = result_ty.is_copy()
                || self
                    .registry
                    .implements_marker(&result_ty, MarkerTrait::Copy);
            let arguments = args
                .iter()
                .map(|arg| {
                    let ty = self
                        .expr_types
                        .get(&SpanKey::in_module(&arg.expr().1, self.current_module_idx))
                        .map(|ty| self.subst.resolve(ty));
                    if ty.as_ref().is_some_and(|ty| {
                        ty.is_copy() || self.registry.implements_marker(ty, MarkerTrait::Copy)
                    }) {
                        crate::runtime_call::ProducedArgumentBoundary::Borrow
                    } else {
                        crate::runtime_call::ProducedArgumentBoundary::Transfer
                    }
                })
                .collect();
            let call_key = SpanKey::in_module(span, self.current_module_idx);
            self.produced_call_arities
                .insert(call_key.clone(), (false, args.len()));
            self.resolved_direct_call_ownership.insert(
                call_key,
                PendingDirectCallOwnership {
                    fact: ProducedValueFact {
                        ownership: if non_owning {
                            crate::runtime_call::ProducedValueOwnership::NoOwner
                        } else {
                            crate::runtime_call::ProducedValueOwnership::owned(
                                crate::runtime_call::ProducedValueAcquisition::Fresh,
                            )
                        },
                        receiver_span: None,
                        receiver_boundary: None,
                        arguments,
                    },
                    extern_symbol: None,
                    extern_declaring_module: None,
                    extern_param_count: 0,
                    resolved_result_ty: result_ty.clone(),
                },
            );
            return result_ty;
        }

        // Handle polymorphic constructors with fresh linked type vars
        match func_name.as_str() {
            // Turbofish constructor `Vec::<T>::new()` or `Vec::new::<T>()`.
            // Guard: only intercept when type_args are supplied; the no-turbofish
            // path falls through to the `fn_sigs` lookup which returns Vec<TypeVar>
            // and lets the call site unify the element type normally.
            "Vec::new" if type_args.is_some() => {
                self.check_arity(args, 0, "`Vec::new`", span);
                let targs = type_args.expect("guarded by `is_some()` above");
                let Some(mut lowered) = self.lower_turbofish_elem("Vec", 1, targs, span) else {
                    return Ty::Error;
                };
                let elem_ty = lowered.remove(0);
                let resolved_elem = self.subst.resolve(&elem_ty);
                // #2647 — converge the MIR indirect-enum reject at the checker
                // boundary (the `Ptr`-token element bypasses the `Layout` gate
                // below). Same reason MIR fails closed with.
                if let Some(reason) = self.indirect_enum_vec_element_reject_reason(&resolved_elem) {
                    self.report_error(
                        TypeErrorKind::InvalidOperation,
                        span,
                        format!(
                            "`{}` cannot be a `Vec` element: {reason}",
                            resolved_elem.user_facing()
                        ),
                    );
                    return Ty::Error;
                }
                // Inherit the layout+Copy guard from check_call_against_expected_constructor.
                if crate::vec_authority::classify_element(&resolved_elem, &self.type_defs)
                    == Some(crate::vec_authority::VecElementToken::Layout)
                    && matches!(resolved_elem, Ty::Named { .. })
                {
                    let is_copy = self.vec_element_has_copy_layout(&resolved_elem);
                    // W5.016: a non-Copy record/enum element with a synthesizable
                    // owned clone/drop thunk path constructs through
                    // `hew_vec_new_with_elem_layout` (the owned ABI), so do not
                    // reject it here. Stays fail-closed for elements with no
                    // thunk path (e.g. a record carrying a `Vec` field).
                    if !is_copy && !self.vec_owned_element_admissible(&resolved_elem) {
                        let reason = self.vec_element_rejection_reason(&resolved_elem);
                        self.report_error(
                            TypeErrorKind::InvalidOperation,
                            span,
                            format!(
                                "`{}` cannot be a `Vec` element: {reason}",
                                resolved_elem.user_facing()
                            ),
                        );
                        return Ty::Error;
                    }
                }
                let result_ty = Ty::Named {
                    builtin: Some(crate::BuiltinType::Vec),
                    name: "Vec".to_string(),
                    args: vec![resolved_elem],
                };
                self.record_type(span, &result_ty);
                self.record_direct_call_target(
                    span,
                    CallTarget::Runtime(crate::runtime_call::RuntimeCallFamily::VecNew),
                );
                return result_ty;
            }
            "HashMap::new" if type_args.is_some() => {
                self.check_arity(args, 0, "`HashMap::new`", span);
                let targs = type_args.expect("guarded by `is_some()` above");
                let Some(result_ty) = self.lower_turbofish_collection_constructor(
                    "HashMap",
                    crate::BuiltinType::HashMap,
                    2,
                    targs,
                    span,
                ) else {
                    return Ty::Error;
                };
                return result_ty;
            }
            "HashSet::new" if type_args.is_some() => {
                self.check_arity(args, 0, "`HashSet::new`", span);
                let targs = type_args.expect("guarded by `is_some()` above");
                let Some(result_ty) = self.lower_turbofish_collection_constructor(
                    "HashSet",
                    crate::BuiltinType::HashSet,
                    1,
                    targs,
                    span,
                ) else {
                    return Ty::Error;
                };
                return result_ty;
            }
            "Some" => {
                self.check_arity(args, 1, "`Some`", span);
                let t = Ty::Var(TypeVar::fresh());
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    self.check_against(expr, sp, &t);
                }
                return Ty::option(t);
            }
            "None" => {
                self.check_arity(args, 0, "`None`", span);
                return Ty::option(Ty::Var(TypeVar::fresh()));
            }
            "Ok" => {
                self.check_arity(args, 1, "`Ok`", span);
                let ok_ty = Ty::Var(TypeVar::fresh());
                let err_ty = Ty::Var(TypeVar::fresh());
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    self.check_against(expr, sp, &ok_ty);
                }
                self.record_builtin_result_output_type_args(span, &ok_ty, &err_ty);
                return Ty::result(ok_ty, err_ty);
            }
            "Err" => {
                self.check_arity(args, 1, "`Err`", span);
                let ok_ty = Ty::Var(TypeVar::fresh());
                let err_ty = Ty::Var(TypeVar::fresh());
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    self.check_against(expr, sp, &err_ty);
                }
                self.record_builtin_result_output_type_args(span, &ok_ty, &err_ty);
                return Ty::result(ok_ty, err_ty);
            }
            "close" => {
                if !self.check_arity(args, 1, "`close`", span) {
                    return Ty::Error;
                }
                let (expr, sp) = args[0].expr();
                let actor_ty = self.synthesize(expr, sp);
                let resolved = self.subst.resolve(&actor_ty);
                if resolved.as_actor_handle().is_some() {
                    return resolved;
                }
                self.report_error(
                    TypeErrorKind::InvalidOperation,
                    span,
                    "`close` expects an actor handle".to_string(),
                );
                return Ty::Error;
            }
            "bytes::from" => {
                self.check_arity(args, 1, "`bytes::from`", span);
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    self.synthesize(expr, sp);
                }
                return Ty::Bytes;
            }
            "Vec::from" => {
                self.check_arity(args, 1, "`Vec::from`", span);
                let elem = Ty::Var(TypeVar::fresh());
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    let arr_ty = self.synthesize(expr, sp);
                    match self.subst.resolve(&arr_ty) {
                        // The parser's `[a, b]` surface is represented as a
                        // temporary Array during some checker paths and as
                        // the already-desugared `Vec<T>` in others. Both
                        // forms feed the same HIR identity lowering below.
                        Ty::Array(inner, _) => self.expect_type(&elem, &inner, span),
                        Ty::Named {
                            builtin: Some(crate::BuiltinType::Vec),
                            args,
                            ..
                        } if args.len() == 1 => self.expect_type(&elem, &args[0], span),
                        other => {
                            self.report_error(
                                TypeErrorKind::InvalidOperation,
                                span,
                                format!(
                                    "`Vec::from` accepts an array or Vec source; `{}` is not supported",
                                    other.user_facing()
                                ),
                            );
                            return Ty::Error;
                        }
                    }
                }
                self.record_method_call_rewrite(span, MethodCallRewrite::VecFrom);
                return self.make_vec_type(elem, span);
            }
            // `link`/`unlink` of a `RemotePid<T>` → a targeted "use link_remote"
            // diagnostic. The local `link(LocalPid<T>)` form stays on the generic
            // `fn_sigs` path below; a `link`/`unlink` of a remote pid would
            // otherwise surface as an opaque LocalPid-vs-RemotePid type mismatch.
            // The cross-node link surface is `link_remote(pid, policy)` — it
            // carries a `PartitionPolicy` the bare `link` cannot express — so name
            // it explicitly here rather than letting the mismatch stand.
            "link" | "unlink" if args.len() == 1 => {
                let (expr, sp) = args[0].expr();
                let arg_ty = self.synthesize(expr, sp);
                let resolved = self.subst.resolve(&arg_ty);
                if resolved.as_remote_pid().is_some() {
                    self.report_error(
                        TypeErrorKind::InvalidOperation,
                        span,
                        format!(
                            "`{func_name}` links a LOCAL actor; for a cross-node link use \
                             `link_remote(pid, policy)`, which carries a `PartitionPolicy` \
                             governing what happens to the local actor when its remote peer dies"
                        ),
                    );
                    return Ty::Error;
                }
                // Not a RemotePid — fall through to the generic `fn_sigs` path,
                // which applies the `link`/`unlink(LocalPid<T>)` builtin signature.
            }
            // Cross-node monitor: `monitor(RemotePid<T>)`. The local
            // `monitor(LocalPid<T>)` form stays on the generic `fn_sigs` path
            // below (registered with a `LocalPid` receiver). When the argument
            // resolves to a `RemotePid<T>`, accept it here and return
            // `Result<MonitorRef, MonitorError>` — remote setup can fail before
            // a registration exists, so it must not manufacture a zero-valued
            // handle. The MIR lowering branches on the argument's resolved type
            // to route a remote receiver to the node monitor ABI
            // (`hew_node_monitor_location`) instead of the in-process
            // `hew_actor_monitor`.
            // The cross-node LINK form is `link_remote(RemotePid<T>,
            // PartitionPolicy)` — its own builtin, routed via the generic
            // `fn_sigs` path; a bare `link(RemotePid)` is rejected above with a
            // "use link_remote" diagnostic.
            "monitor" if args.len() == 1 => {
                let (expr, sp) = args[0].expr();
                let arg_ty = self.synthesize(expr, sp);
                let resolved = self.subst.resolve(&arg_ty);
                if resolved.as_remote_pid().is_some() {
                    let result_ty = Ty::result(Ty::monitor_ref(), Ty::monitor_error());
                    self.record_type(span, &result_ty);
                    self.record_direct_call_target(
                        span,
                        CallTarget::Runtime(crate::runtime_call::RuntimeCallFamily::ActorMonitor),
                    );
                    return result_ty;
                }
                // Not a RemotePid — fall through to the generic `fn_sigs` path,
                // which applies the local typed-result builtin signature (and
                // reports the precise mismatch for a bad argument).
            }
            "supervisor_child" if args.len() == 2 => {
                // supervisor_child(sup, index) → typed LocalPid based on supervisor decl
                let (sup_expr, sup_sp) = args[0].expr();
                let sup_ty = self.synthesize(sup_expr, sup_sp);
                let sup_ty_resolved = self.subst.resolve(&sup_ty);
                let (idx_expr, idx_sp) = args[1].expr();
                self.check_against(idx_expr, idx_sp, &Ty::I64);

                // Accept local actor handles as supervisor handles.
                if let Some(Ty::Named { name: sup_name, .. }) = sup_ty_resolved.as_actor_handle() {
                    if let Some(sup_children) = self.supervisor_children.get(sup_name) {
                        // `supervisor_child` builtin indexes into the static slot space.
                        let statics = &sup_children.statics;
                        if let Expr::Literal(hew_parser::ast::Literal::Integer {
                            value: idx, ..
                        }) = idx_expr
                        {
                            #[expect(
                                clippy::cast_sign_loss,
                                clippy::cast_possible_truncation,
                                reason = "supervisor child index is always non-negative and small"
                            )]
                            let i = *idx as usize;
                            if i < statics.len() {
                                let child_type = &statics[i].1;
                                return Ty::local_pid(Ty::Named {
                                    builtin: None,
                                    // Canonicalize the raw user-spelled child
                                    // type (`bank.Account`) to the registered
                                    // actor identity so the PID matches the
                                    // spawn-derived identity and dispatches.
                                    name: self.canonical_supervisor_child_type(child_type),
                                    args: vec![],
                                });
                            }
                        }
                        // Non-constant index: fresh type var
                        return Ty::local_pid(Ty::Var(TypeVar::fresh()));
                    }
                }
                return Ty::local_pid(Ty::Var(TypeVar::fresh()));
            }
            _ => {}
        }

        // UFCS-style trait-qualified call against a primitive or builtin
        // generic receiver: `Display::fmt(x)` where `x: int`.  The trait
        // method sig in `fn_sigs` has the receiver param stripped (because
        // it was registered through `register_fn_sig_with_name` with a
        // `Trait::method` key), so the existing path at the `fn_sigs`
        // lookup below would mis-arity the call (0 expected vs 1 supplied).
        // Intercept here and route through the side table populated for
        // primitive / builtin-generic receivers; this preserves the
        // receiver param on the resolved sig so arity matches and the
        // first arg is type-checked against the canonical receiver.
        if let Some((trait_name, method_name)) = func_name.split_once("::") {
            if self.trait_defs.contains_key(trait_name) {
                if let Some(ret_ty) = self.try_dispatch_ufcs_primitive_trait_method(
                    trait_name,
                    method_name,
                    args,
                    span,
                ) {
                    return ret_ty;
                }
            }
        }

        // Look up function signature first, preferring the current module's
        // private helper/extern over another module's same-named item.
        // rc1-F1 stage A: canonical-first — a root caller resolves its own
        // free fns under the canonical `{root_module}.{name}` key the mint
        // produces; the contains_key filter keeps bare registrations
        // (builtins, externs) resolving unchanged (the bare rung is the
        // builtin/extern floor, not a root fallback).
        let resolved_fn_name = scoped_module_item_name(self.canonical_fn_owner(), &func_name)
            .filter(|qualified| self.fn_sigs.contains_key(qualified))
            .unwrap_or_else(|| func_name.clone());
        if let Some(sig) = self.fn_sigs.get(&resolved_fn_name).cloned() {
            // Visibility enforcement: check that the caller's module is allowed
            // to reference this function.  We only check when the resolved key
            // is module-qualified (contains '.') because bare calls have no
            // cross-module boundary.  Root programs (current_module == None)
            // are subject to the same check: a root caller referencing
            // `module.private_fn()` is a cross-module access and must be
            // rejected.  access_allowed handles the None caller correctly.
            if resolved_fn_name.contains('.') && !resolved_fn_name.contains("::") {
                if let Some(&vis) = self.fn_visibility.get(&resolved_fn_name) {
                    let decl_module = self
                        .fn_def_spans
                        .get(&resolved_fn_name)
                        .and_then(|(_, m)| m.as_deref());
                    let acc_module = self.current_module.as_deref();
                    if !visibility::access_allowed(decl_module, acc_module, vis) {
                        // Extract just the function name (last segment after '.').
                        let symbol = crate::short_name(&resolved_fn_name);
                        let decl_span = self
                            .fn_def_spans
                            .get(&resolved_fn_name)
                            .map_or_else(|| span.clone(), |(s, _)| s.clone());
                        let err = TypeError::visibility_violation(
                            vis,
                            span.clone(),
                            symbol,
                            decl_module.unwrap_or("(root)"),
                            acc_module.unwrap_or("(root)"),
                            decl_span,
                            self.current_module.clone(),
                        );
                        self.errors.push(err);
                        return Ty::Error;
                    }
                }
            }

            if let Some(caller) = &self.current_function {
                self.call_graph
                    .entry(caller.clone())
                    .or_default()
                    .insert(resolved_fn_name.clone());
            }
            // Mark the originating module as used for unqualified imports
            if let Some(module) = self
                .unqualified_to_module
                .get(&(
                    self.current_module.clone(),
                    self.current_module_idx,
                    func_name.clone(),
                ))
                .cloned()
            {
                self.mark_module_owner_bindings_used(&module);
            }
            let assoc_bindings = self
                .fn_type_param_assoc_bindings
                .get(&resolved_fn_name)
                .cloned()
                .unwrap_or_default();
            let applied_sig = self.apply_instantiated_call_signature_with_assoc(
                &sig,
                &assoc_bindings,
                type_args,
                args,
                span,
                SignatureArgApplication::FunctionLike {
                    param_names: &sig.param_names,
                    accepts_kwargs: sig.accepts_kwargs,
                    module_qualified: false,
                },
                // Record the resolved type arguments at every generic call
                // site — whether the args were inferred (from an argument or
                // the expected return type), or supplied via explicit turbofish
                // (`id<i64>(5)`). `record_concrete_call_type_args` snapshots the
                // args (deferring any still-`Ty::Var` to the output-boundary
                // re-resolve + fail-closed prune), so recording is always safe.
                // HIR and MIR consume this side-table to emit and dispatch the
                // monomorphised symbol; skipping these sites starved that
                // pipeline and tripped the NYI function-call lowering arm.
                true,
            );

            if resolved_fn_name == "Rc::new" {
                if let (Some(payload_ty), Some(arg)) = (applied_sig.params.first(), args.first()) {
                    let (arg_expr, arg_span) = arg.expr();
                    let resolved_payload = self
                        .subst
                        .resolve(payload_ty)
                        .materialize_literal_defaults();
                    self.validate_rc_payload_type(&resolved_payload, arg_span);
                    self.reject_borrowed_parameter_consumption(arg_expr, arg_span, "Rc::new");
                    if let Ok(payload_ty) = ResolvedTy::from_ty(&resolved_payload) {
                        self.method_call_rewrites.insert(
                            SpanKey::in_module(span, self.current_module_idx),
                            MethodCallRewrite::RcIntrinsic {
                                op: RcIntrinsicOp::New,
                                payload_ty,
                            },
                        );
                    }
                }
            }

            if resolved_fn_name == "len" {
                if let Some(Ty::Named {
                    args,
                    builtin: Some(crate::BuiltinType::HashSet),
                    ..
                }) = applied_sig.params.first().map(|ty| self.subst.resolve(ty))
                {
                    let elem_ty = args.first().cloned().unwrap_or(Ty::Var(TypeVar::fresh()));
                    if !self.validate_hashset_element_type(&elem_ty, span) {
                        return Ty::Error;
                    }
                    self.record_hashset_lowering_fact(span, &elem_ty);
                }
            }

            self.record_resolved_direct_call_ownership(
                &resolved_fn_name,
                &sig,
                args,
                &applied_sig.return_type,
                span,
            );
            self.record_direct_call_target(span, self.call_target_for_signature(&resolved_fn_name));
            return applied_sig.return_type;
        }

        // Then check if it's a variable with a function type (e.g., lambda parameters)
        if let Some((binding_depth, binding)) = self.env.lookup_with_depth(&func_name) {
            if let Some(sig) = binding
                .def_span
                .as_ref()
                .and_then(|def_span| {
                    self.lambda_poly_sig_map
                        .get(&SpanKey::in_module(def_span, self.current_module_idx))
                })
                .cloned()
            {
                return self
                    .apply_instantiated_call_signature(
                        &sig.call_sig,
                        type_args,
                        args,
                        span,
                        SignatureArgApplication::FunctionLike {
                            param_names: &sig.call_sig.param_names,
                            accepts_kwargs: sig.call_sig.accepts_kwargs,
                            module_qualified: false,
                        },
                        // Record at every generic call site, turbofish or
                        // inferred — see the resolved-fn path above.
                        // `record_concrete_call_type_args` defers any
                        // still-`Ty::Var` arg to the output-boundary prune, which
                        // keeps the checker output fail-closed.
                        true,
                    )
                    .return_type;
            }

            let func_ty = binding.ty.clone();
            // Captured-closure-as-callee identity, snapshotted while `binding`
            // is still borrowable (the mutable `self` operations below end its
            // borrow). Used by the capture-fact push after the LambdaPid gate.
            let callee_binding_id = binding.id;
            let callee_def_span = binding.def_span.clone();

            // Explicit fail-closed gate: a regular fn-closure must not capture a
            // lambda-actor handle (`LambdaPid<M,R>`) and call it with call syntax.
            //
            // Authority: checker (this site). MIR has a defence-in-depth guard at
            // `materialize_closure_env` that names this site as authoritative. The
            // rejection is deliberate: there is no env-materialization protocol for
            // lambda-actor handles yet — the MIR routing discriminator
            // (`Place::LambdaActorHandle`) is bound to the spawning-scope slot, not
            // to an env-loaded copy, so emitting the capture would silently
            // misroute to `hew_duplex_send` instead of `hew_lambda_actor_send`.
            //
            // When this restriction is lifted (full env-materialization protocol
            // for lambda-actor captures), remove this guard AND the MIR assert in
            // `materialize_closure_env`.
            let resolved_func_ty = self.subst.resolve(&func_ty);
            if matches!(
                &resolved_func_ty,
                Ty::Named {
                    builtin: Some(crate::BuiltinType::LambdaPid),
                    ..
                }
            ) {
                if let Some(capture_depth) = self.lambda_capture_depth {
                    // Allow the lambda-actor body to call its own handle
                    // recursively (self-send pattern). Regular fn-closures must not
                    // capture lambda-actor handles — no env-materialization protocol exists yet.
                    if binding_depth < capture_depth && !self.in_lambda_actor_body {
                        self.report_error(
                            TypeErrorKind::ClosureCapturesDuplexHandle {
                                name: func_name.clone(),
                            },
                            span,
                            format!(
                                "fn-closure captures lambda-actor handle `{func_name}` \
                                 from enclosing scope — no env-materialization protocol \
                                 exists for lambda-actor handle captures yet; call the handle directly \
                                 from the enclosing scope or spawn a dedicated forwarding actor \
                                 (E_CLOSURE_CAPTURES_LAMBDA_HANDLE)"
                            ),
                        );
                        return Ty::Error;
                    }
                }
            }

            // A captured CLOSURE used as a call callee (`|y| base(y)` where
            // `base` is a closure binding from an enclosing scope) is a capture
            // exactly as reading its identifier would be — but the call
            // dispatch resolves the bare callee HERE rather than through
            // `check_identifier`, so without this push the capture fact is never
            // recorded and HIR's `materialize_closure_captures` later finds the
            // binding with no metadata (E_HIR CheckerBoundaryViolation). Record
            // it now, mirroring the identifier path; `check_lambda` refines the
            // placeholder mode from a body scan. LambdaPid handles are excluded:
            // their capture is rejected (above) or routed through the dedicated
            // lambda-actor protocol, never the closure-env protocol.
            if let Some(capture_depth) = self.lambda_capture_depth {
                if binding_depth < capture_depth
                    && !matches!(
                        &resolved_func_ty,
                        Ty::Named {
                            builtin: Some(crate::BuiltinType::LambdaPid),
                            ..
                        }
                    )
                {
                    self.lambda_captures.push(func_ty.clone());
                    self.lambda_capture_facts.push(ClosureCaptureFact {
                        binding_id: callee_binding_id,
                        name: func_name.clone(),
                        ty: func_ty.clone(),
                        mode: ClosureCaptureMode::Borrow,
                        mode_origin: CaptureModeOrigin::InferredBorrow,
                        is_send: false,
                        is_sync: false,
                        use_span: span.clone(),
                        def_span: callee_def_span.clone(),
                    });
                }
            }

            let ret = self.check_call_with_type(&func_ty, args, span);
            return ret;
        }

        // Qualified trait method call: e.g. `Measurable::measure(item)`.
        // If the function name has the form `TraitName::method` and TraitName
        // is a known trait, resolve the method from the trait definition.
        if let Some((trait_name, method_name)) = func_name.split_once("::") {
            if self.trait_defs.contains_key(trait_name) {
                // Use the full signature (receiver included) for qualified calls.
                if let Some(sig) = self.lookup_trait_method_inner(trait_name, method_name, false) {
                    // The trait sig includes all non-receiver params.
                    // For qualified calls the first positional arg is the receiver.
                    self.check_arity(args, sig.params.len(), &format!("`{func_name}`"), span);
                    for (i, arg) in args.iter().enumerate() {
                        if let Some(param_ty) = sig.params.get(i) {
                            let (expr, sp) = arg.expr();
                            self.check_against(expr, sp, param_ty);
                        }
                    }
                    return sig.return_type;
                }
                self.report_error(
                    TypeErrorKind::UndefinedMethod,
                    span,
                    format!("no method `{method_name}` in trait `{trait_name}`"),
                );
                return Ty::Error;
            }
        }

        // Namespaced module-qualified call: `module::fn(args)` parses as a
        // `FieldAccess` callee and arrives here as `func_name = "module::fn"`.
        // The dot form `module.fn(args)` already resolves through the
        // method-call path (`check_method_call`) under the `module.fn`
        // registry key. Route the namespaced form through the same dispatch so
        // an imported generic free function (e.g. `iter::map`) resolves to the
        // same signature, records the same module-qualified rewrite for HIR,
        // and applies the associated-type binding pins. The dot key is the
        // canonical registry identity for both surfaces.
        if let Some(ret) = self.try_check_namespaced_module_call(&func_name, args, span) {
            return ret;
        }

        // Detect recursive closure self-reference in call position: if we are
        // inside a lambda body and the callee name matches the pending let-binding,
        // emit ClosureRecursive rather than UndefinedFunction.
        if self.lambda_capture_depth.is_some()
            && self
                .pending_let_closure_name
                .as_deref()
                .is_some_and(|pending| pending == func_name.as_str())
        {
            self.report_error(
                TypeErrorKind::ClosureRecursive {
                    name: func_name.clone(),
                },
                span,
                format!(
                    "closure cannot call itself via binding \
                     `{func_name}` — recursive closures require a fixed-point surface \
                     that is not available in this version; use a named function instead"
                ),
            );
            return Ty::Error;
        }
        let similar = crate::error::find_similar(
            &func_name,
            self.fn_sigs
                .keys()
                .map(String::as_str)
                .chain(self.env.all_names()),
        );
        self.report_error_with_suggestions(
            TypeErrorKind::UndefinedFunction,
            span,
            format!("undefined function `{func_name}`"),
            similar,
        );
        Ty::Error
    }

    /// Reject a bare function binding published by more than one imported
    /// owner before the legacy `fn_sigs` slot can select its last writer.
    fn report_bare_function_import_ambiguity(&mut self, name: &str, span: &Span) -> bool {
        if name.contains('.') || name.contains("::") {
            return false;
        }
        // rc1-F1 stage A: the local-declaration probe resolves through the
        // CANONICAL owner — root declarations live under `{root}.{name}` in
        // `fn_def_spans` now.
        let current_local = scoped_module_item_name(self.canonical_fn_owner(), name)
            .is_some_and(|qualified| self.fn_def_spans.contains_key(&qualified));
        if current_local
            || (self.current_module.is_none() && self.root_value_bindings.contains(name))
        {
            return false;
        }
        let Some(owners) = self.published_bare_function_owners.get(&(
            self.current_module.clone(),
            self.current_module_idx,
            name.to_string(),
        )) else {
            return false;
        };
        if owners.len() < 2 {
            return false;
        }
        let candidates: Vec<String> = owners.iter().cloned().collect();
        self.mark_ambiguous_import_owners_used(&candidates);
        self.report_error_with_suggestions(
            TypeErrorKind::AmbiguousType,
            span,
            format!(
                "ambiguous function `{name}`: published by {} imported modules",
                candidates.len()
            ),
            candidates
                .iter()
                .map(|candidate| format!("qualify the call, e.g. `{candidate}(...)`"))
                .collect(),
        );
        true
    }

    #[expect(
        clippy::too_many_lines,
        reason = "typed calls publish both ordinary call results and ownership boundaries"
    )]
    pub(super) fn check_call_with_type(
        &mut self,
        func_ty: &Ty,
        args: &[CallArg],
        span: &Span,
    ) -> Ty {
        self.record_direct_call_target(span, CallTarget::IndirectFunctionValue);
        let resolved = self.subst.resolve(func_ty);
        match resolved {
            Ty::Function { params, ret } | Ty::Closure { params, ret, .. } => {
                self.check_arity(args, params.len(), "this function", span);
                for (i, arg) in args.iter().enumerate() {
                    if let Some(param) = params.get(i) {
                        let (expr, sp) = arg.expr();
                        self.check_against(expr, sp, param);
                    }
                }
                let result_ty = *ret;
                let resolved_result_ty = self
                    .subst
                    .resolve(&result_ty)
                    .materialize_literal_defaults();
                let non_owning = resolved_result_ty.is_copy()
                    || self
                        .registry
                        .implements_marker(&resolved_result_ty, MarkerTrait::Copy);
                let call_key = SpanKey::in_module(span, self.current_module_idx);
                self.produced_call_arities
                    .insert(call_key.clone(), (false, args.len()));
                self.resolved_direct_call_ownership.insert(
                    call_key,
                    PendingDirectCallOwnership {
                        fact: ProducedValueFact {
                            ownership: if non_owning {
                                crate::runtime_call::ProducedValueOwnership::NoOwner
                            } else if matches!(&resolved_result_ty, Ty::String) {
                                crate::runtime_call::ProducedValueOwnership::owned(
                                    crate::runtime_call::ProducedValueAcquisition::Delivery,
                                )
                            } else {
                                crate::runtime_call::ProducedValueOwnership::Unknown
                            },
                            receiver_span: None,
                            receiver_boundary: None,
                            // Closure/function-value parameters borrow by
                            // default. A future typed consuming-callable
                            // signature extends this vector; expression intent
                            // is not consulted here.
                            arguments: vec![
                                crate::runtime_call::ProducedArgumentBoundary::Borrow;
                                args.len()
                            ],
                        },
                        extern_symbol: None,
                        extern_declaring_module: None,
                        extern_param_count: 0,
                        resolved_result_ty,
                    },
                );
                result_ty
            }
            Ty::Unit => {
                self.check_arity(args, 0, "this function", span);
                Ty::Unit
            }
            // LambdaPid<Msg, Reply>: lambda-actor handle — call-syntax dispatch.
            //
            // tell-shaped: `LambdaPid<Msg, ()>` — `handle(msg)` returns `Result<(), SendError>`
            // ask-shaped:  `LambdaPid<Msg, R>`  — `handle(msg)` returns `Result<R, AskError>`
            //
            // Exactly one argument required (the message). The message type must match
            // the handle's message type (M). The message must be Send (crosses actor boundary).
            Ty::Named {
                args: ref type_args,
                builtin: Some(crate::BuiltinType::LambdaPid),
                ..
            } if type_args.len() == 2 => {
                let msg_ty = type_args[0].clone();
                let reply_ty = type_args[1].clone();
                // A multi-param lambda actor carries a Tuple message type
                // (`actor |a: i64, b: string| { .. }` → `LambdaPid<(i64, string), R>`).
                // Its call surface is the N-arg form `handle(a, b)`: each call
                // argument checks against its tuple component and each crosses
                // the actor boundary independently (per-arg Send enforcement).
                // Every other shape — including a single literal-tuple argument
                // for a single-tuple-param lambda — stays on the one-message
                // path below.
                let multi_component_tys: Option<Vec<Ty>> = match &msg_ty {
                    Ty::Tuple(parts) if parts.len() > 1 && parts.len() == args.len() => {
                        Some(parts.clone())
                    }
                    _ => None,
                };
                if let Some(parts) = multi_component_tys {
                    for (arg, part) in args.iter().zip(parts.iter()) {
                        let (expr, sp) = arg.expr();
                        let actual = self.check_against(expr, sp, part);
                        // Enforce Send per argument (E_DUPLEX_NON_SEND).
                        if !matches!(actual, Ty::Error | Ty::Var(_))
                            && !self.registry.implements_marker(&actual, MarkerTrait::Send)
                        {
                            self.report_error(
                                TypeErrorKind::InvalidSend,
                                sp,
                                format!(
                                    "message type `{}` is not Send; lambda actor calls cross an actor boundary (E_DUPLEX_NON_SEND)",
                                    actual.user_facing()
                                ),
                            );
                        }
                    }
                } else {
                    // Arity: exactly one call argument (the message).
                    self.check_arity(args, 1, "lambda actor handle", span);
                    if let Some(arg) = args.first() {
                        let (expr, sp) = arg.expr();
                        let actual = self.check_against(expr, sp, &msg_ty);
                        // Enforce Send on the call-site argument (E_DUPLEX_NON_SEND).
                        if !matches!(actual, Ty::Error | Ty::Var(_))
                            && !self.registry.implements_marker(&actual, MarkerTrait::Send)
                        {
                            self.report_error(
                                TypeErrorKind::InvalidSend,
                                sp,
                                format!(
                                    "message type `{}` is not Send; lambda actor calls cross an actor boundary (E_DUPLEX_NON_SEND)",
                                    actual.user_facing()
                                ),
                            );
                        }
                    }
                }
                // Return type depends on reply direction:
                //   tell-shaped (Reply = ()) → Result<(), SendError>
                //   ask-shaped  (Reply = R)  → Result<R, AskError>
                let result_ty = if matches!(reply_ty, Ty::Unit) {
                    Ty::result(Ty::Unit, Ty::send_error())
                } else {
                    Ty::result(reply_ty, Ty::ask_error())
                };
                let resolved_result_ty = self
                    .subst
                    .resolve(&result_ty)
                    .materialize_literal_defaults();
                let call_key = SpanKey::in_module(span, self.current_module_idx);
                self.produced_call_arities
                    .insert(call_key.clone(), (false, args.len()));
                self.resolved_direct_call_ownership.insert(
                    call_key,
                    PendingDirectCallOwnership {
                        fact: ProducedValueFact {
                            ownership: crate::runtime_call::ProducedValueOwnership::owned(
                                crate::runtime_call::ProducedValueAcquisition::Delivery,
                            ),
                            receiver_span: None,
                            receiver_boundary: None,
                            arguments: vec![
                                crate::runtime_call::ProducedArgumentBoundary::Transfer;
                                args.len()
                            ],
                        },
                        extern_symbol: None,
                        extern_declaring_module: None,
                        extern_param_count: 0,
                        resolved_result_ty,
                    },
                );
                result_ty
            }
            _ => {
                // Synthesize args even when the callee type is already an error/var so that
                // independent argument diagnostics are not cascade-suppressed.  This mirrors
                // what check_method_call's (Ty::Error, _) arm does.
                for arg in args {
                    let (expr, sp) = arg.expr();
                    self.synthesize(expr, sp);
                }
                // Don't cascade errors from already-failed expressions.
                if !matches!(resolved, Ty::Error | Ty::Var(_)) {
                    self.report_error(
                        TypeErrorKind::Mismatch {
                            expected: "function".to_string(),
                            actual: resolved.user_facing().to_string(),
                        },
                        span,
                        format!("cannot call value of type `{}`", resolved.user_facing()),
                    );
                }
                Ty::Error
            }
        }
    }

    pub(super) fn synthesize_actor_concurrency_source(
        &mut self,
        expr: &Expr,
        span: &Span,
        construct: &str,
    ) -> Ty {
        // NEW-4: a `pat from rx.recv()` select/join arm over a std/channel
        // `Receiver<T>`. Recognised before the actor-ask shape: the receiver is
        // a channel handle (not an actor), and `recv` resolves to `Option<T>`
        // with a recorded runtime rewrite (hew_channel_recv_layout), exactly as an
        // awaited `rx.recv()`. The select substrate polls the channel core for
        // readiness and binds `Option<T>` on the winning edge.
        if let Expr::MethodCall {
            receiver, method, ..
        } = expr
        {
            if method == "recv" {
                let recv_ty = {
                    let ty = self.synthesize(&receiver.0, &receiver.1);
                    self.subst.resolve(&ty)
                };
                if matches!(
                    &recv_ty,
                    Ty::Named {
                        builtin: Some(crate::BuiltinType::Receiver),
                        ..
                    }
                ) {
                    let prev = self.inside_await_expr;
                    self.inside_await_expr = true;
                    let synthesized = self.synthesize(expr, span);
                    self.inside_await_expr = prev;
                    return self.subst.resolve(&synthesized);
                }
            }
        }

        let (method_expr, method_span, receiver_expr, receiver_span) = match expr {
            Expr::MethodCall { receiver, .. } => (expr, span, &receiver.0, &receiver.1),
            Expr::Await(inner) => {
                if let Expr::MethodCall { receiver, .. } = &inner.0 {
                    (&inner.0, &inner.1, &receiver.0, &receiver.1)
                } else {
                    self.report_error(
                        TypeErrorKind::InvalidOperation,
                        span,
                        format!("{construct} must be actor.method(args)"),
                    );
                    return Ty::Error;
                }
            }
            _ => {
                self.report_error(
                    TypeErrorKind::InvalidOperation,
                    span,
                    format!("{construct} must be actor.method(args)"),
                );
                return Ty::Error;
            }
        };

        let receiver_ty = {
            let ty = self.synthesize(receiver_expr, receiver_span);
            self.subst.resolve(&ty)
        };
        if receiver_ty.as_actor_handle().is_none() {
            self.report_error(
                TypeErrorKind::InvalidOperation,
                span,
                format!("{construct} must be actor.method(args)"),
            );
            return Ty::Error;
        }

        let ty = {
            // Treat the method call inside a select arm or join as if it is
            // under `await` so the ask-without-await guard does not fire here.
            // Select / join sources are the select-flavoured equivalent of
            // awaited asks — the caller is the concurrency construct itself.
            let prev = self.inside_await_expr;
            self.inside_await_expr = true;
            let synthesized = self.synthesize(method_expr, method_span);
            self.inside_await_expr = prev;
            self.subst.resolve(&synthesized)
        };
        if ty == Ty::Unit {
            self.report_error(
                TypeErrorKind::InvalidOperation,
                span,
                format!("{construct} requires a receive handler with a return type"),
            );
            return Ty::Error;
        }

        ty
    }

    /// Validates that a `Receiver<T>` element type is resolved and supported for
    /// `for await`.
    pub(super) fn check_receiver_element_type_for_await(&mut self, inner: &Ty, span: &Span) {
        let resolved = self.subst.resolve(inner);
        if resolved.has_inference_var() {
            self.report_error(
                TypeErrorKind::InvalidOperation,
                span,
                "`for await` over a channel receiver requires a resolved element type".to_string(),
            );
            return;
        }
        if !self.queue_elem_admissible(&resolved) {
            let reason = self.queue_elem_rejection_reason(&resolved);
            self.report_error(
                TypeErrorKind::InvalidOperation,
                span,
                format!("`Channel<{resolved}>` is not supported in `for await`: {reason}"),
            );
            return;
        }

        self.reject_wasm_feature(span, WasmUnsupportedFeature::BlockingChannelRecv);
    }
}

#[cfg(test)]
mod channel_layout_target_tests {
    use super::*;

    #[test]
    fn channel_layout_runtime_target_requires_canonical_source_provenance() {
        let signature = "std.channel.hew_channel_recv_layout";

        let mut canonical = Checker::default();
        canonical
            .canonical_std_module_sources
            .insert("std.channel".to_string());
        assert_eq!(
            canonical.call_target_for_signature(signature),
            CallTarget::Runtime(crate::runtime_call::RuntimeCallFamily::ChannelRecvLayout)
        );

        let user_spelling = Checker::default();
        assert!(matches!(
            user_spelling.call_target_for_signature(signature),
            CallTarget::Unsupported { .. }
        ));
    }

    #[test]
    fn executable_catalog_identities_publish_builtin_targets() {
        let checker = Checker::new(crate::module_registry::ModuleRegistry::new(vec![]));

        for endpoint in crate::stdlib_catalog_identity::MONOMORPHIC_CALLABLE_IDENTITIES {
            assert_eq!(
                checker.call_target_for_signature(endpoint),
                CallTarget::Builtin {
                    endpoint: (*endpoint).to_string(),
                },
                "catalog builtin `{endpoint}` must cross the checker boundary with its exact catalog identity"
            );
        }
    }

    #[test]
    fn registered_runtime_builtin_families_publish_runtime_targets() {
        let mut checker = Checker::new(crate::module_registry::ModuleRegistry::new(vec![]));
        checker.register_builtins();
        let mut registered_families = 0;

        for (signature_key, family) in &checker.runtime_builtin_targets {
            let target = checker.call_target_for_signature(signature_key);
            if crate::stdlib_catalog_identity::monomorphic_callable_identity(signature_key)
                .is_some()
            {
                assert!(
                    matches!(target, CallTarget::Builtin { .. }),
                    "catalog builtin `{signature_key}` must retain its catalog target, got {target:?}"
                );
            } else {
                registered_families += 1;
                assert_eq!(
                    target,
                    CallTarget::Runtime(*family),
                    "registered runtime builtin `{signature_key}` must publish its exact executable family"
                );
            }
        }

        assert!(
            registered_families > 0,
            "the inventory must exercise at least one registered runtime builtin family"
        );
        assert_eq!(
            checker.call_target_for_signature("duplex_pair"),
            CallTarget::Runtime(crate::runtime_call::RuntimeCallFamily::DuplexPair)
        );
    }

    #[test]
    fn imported_duplex_pair_spelling_does_not_inherit_builtin_runtime_authority() {
        let mut checker = Checker::default();
        checker.register_builtins();
        checker.fn_sigs.insert(
            "wire.duplex_pair".to_string(),
            FnSig {
                params: vec![],
                return_type: Ty::Unit,
                ..FnSig::default()
            },
        );
        checker
            .module_import_bindings
            .insert((None, 0, "wire".to_string()), "app.transport".to_string());

        assert_eq!(
            checker.call_target_for_signature("wire.duplex_pair"),
            CallTarget::User(crate::DefId::new("app.transport.duplex_pair".to_string()))
        );
    }

    #[test]
    fn user_function_named_assert_remains_a_user_target() {
        let parsed = hew_parser::parse(
            r"
            fn assert(value: bool) -> () {}

            fn main() -> i64 {
                assert(true);
                0
            }
            ",
        );
        assert!(
            parsed.errors.is_empty(),
            "parse errors: {:?}",
            parsed.errors
        );

        let mut checker = Checker::new(crate::module_registry::ModuleRegistry::new(vec![]));
        checker.checking_embedded_builtins = true;
        let output = checker.check_program(&parsed.program);
        assert!(
            output.errors.is_empty(),
            "type errors: {:#?}",
            output.errors
        );
        assert!(
            output.direct_call_targets.values().any(|target| {
                matches!(target, CallTarget::User(declaration) if declaration.full_path() == "assert")
            }),
            "a user declaration must shadow the catalog `assert` endpoint"
        );
        assert!(
            !output.direct_call_targets.values().any(
                |target| matches!(target, CallTarget::Builtin { endpoint } if endpoint == "assert")
            ),
            "a user declaration must not inherit the catalog `assert` endpoint"
        );
    }
}
