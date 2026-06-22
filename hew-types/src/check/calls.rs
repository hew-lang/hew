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
            self.type_defs.get(type_prefix).and_then(|td| {
                if td.kind != TypeDefKind::Enum && td.kind != TypeDefKind::Struct {
                    return None;
                }
                td.variants.get(variant_name).and_then(|variant| {
                    let params = match variant {
                        VariantDef::Unit => Vec::new(),
                        VariantDef::Tuple(p) => p.clone(),
                        VariantDef::Struct(_) => return None,
                    };
                    Some((type_prefix.to_string(), params, td.type_params.clone()))
                })
            })
        } else {
            self.type_defs
                .iter()
                .filter(|(_, td)| td.kind == TypeDefKind::Enum || td.kind == TypeDefKind::Struct)
                .find_map(|(type_name, td)| {
                    td.variants.get(func_name).and_then(|variant| {
                        let params = match variant {
                            VariantDef::Unit => Vec::new(),
                            VariantDef::Tuple(p) => p.clone(),
                            VariantDef::Struct(_) => return None,
                        };
                        Some((type_name.clone(), params, td.type_params.clone()))
                    })
                })
        }
    }

    fn expected_constructor_type_args(
        expected: &Ty,
        type_name: &str,
        arity: usize,
    ) -> Option<Vec<Ty>> {
        match expected {
            Ty::Named { name, args, .. }
                if Ty::names_match_qualified(name, type_name) && args.len() == arity =>
            {
                Some(args.clone())
            }
            _ => None,
        }
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
            }
            crate::BuiltinType::HashSet => {
                self.validate_concrete_hashset_type(&result_ty, span);
            }
            crate::BuiltinType::Vec => {
                self.validate_concrete_vec_type(&result_ty, span);
            }
            _ => {}
        }
        self.record_type(span, &result_ty);
        Some(result_ty)
    }

    pub(super) fn record_concrete_call_type_args(&mut self, span: &Span, type_args: &[Ty]) {
        let concrete: Vec<Ty> = type_args.iter().map(|ty| self.subst.resolve(ty)).collect();
        if concrete.iter().all(|ty| !ty.has_inference_var()) {
            self.call_type_args
                .insert(SpanKey::in_module(span, self.current_module_idx), concrete);
        }
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
        let func_name = match &func.0 {
            Expr::Identifier(name) => name.clone(),
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
                return Some(result_ty);
            }
            if matches!(elem_ty, Ty::Error) {
                self.record_type(span, &Ty::Error);
                return Some(Ty::Error);
            }
            if crate::stdlib::vec_element_runtime_suffix(&elem_ty, &self.type_defs)
                == Some("layout")
                && matches!(elem_ty, Ty::Named { .. })
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
            return Some(result_ty);
        }

        if let Some((type_name, expected_params, type_params)) =
            self.lookup_variant_constructor(&func_name)
        {
            let mut inferred_args = Self::expected_constructor_type_args(
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
            let result_ty = Ty::normalize_named(
                type_name,
                inferred_args
                    .drain(..)
                    .map(|ty| self.subst.resolve(&ty))
                    .collect(),
            );
            self.record_type(span, &result_ty);
            return Some(result_ty);
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
        match func_name {
            "link" | "unlink" | "monitor" | "demonitor" => {
                self.reject_wasm_feature(span, WasmUnsupportedFeature::LinkMonitor);
            }
            "supervisor_child" | "supervisor_stop" => {
                self.reject_wasm_feature(span, WasmUnsupportedFeature::SupervisionTrees);
            }
            // sleep_ms / sleep: the wasm32 scheduler now parks the calling actor
            // at the message boundary and re-enqueues it once the deadline passes
            // (see hew-runtime/src/scheduler_wasm.rs :: park_actor_sleep).
            // Semantics are cooperative: code after sleep_ms in the same handler
            // runs before the park takes effect.  Warn rather than reject so
            // WASM programs can use timers with the degraded-semantics caveat.
            "sleep_ms" | "sleep" => {
                self.warn_wasm_limitation(span, WasmUnsupportedFeature::Timers);
            }
            // crypto.random_bytes depends on a secure native-only entropy source
            // that is absent from the wasm32 link set. Reject at check time so
            // secure randomness fails closed instead of compiling to a non-secure
            // host import or fallback.
            "random_bytes" => {
                self.reject_wasm_feature(span, WasmUnsupportedFeature::CryptoRandom);
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
        if !self.in_receive_fn {
            return;
        }
        self.warnings.push(TypeError {
            severity: crate::error::Severity::Warning,
            kind: TypeErrorKind::BlockingCallInReceiveFn,
            span: span.clone(),
            message: format!(
                "blocking call `{op_desc}` inside an actor receive function \
                 can stall the scheduler thread and cause deadlocks; \
                 consider passing the value in via a message instead"
            ),
            notes: vec![(
                span.clone(),
                "actor receive functions run synchronously on scheduler worker threads".to_string(),
            )],
            suggestions: vec![
                "send the blocking work to a dedicated actor or async task and \
                 deliver the result as a message"
                    .to_string(),
            ],
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
            ("http.Server" | "net.Listener", "accept") | ("net.Connection", "read")
        ) {
            self.warn_if_blocking_in_receive_fn(&format!("{type_name}::{method}"), span);
        }
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
        // Get function name from expression
        let func_name = match &func.0 {
            Expr::Identifier(name) => name.clone(),
            // Handle path-style calls like Vec::new(), HashMap::new()
            Expr::FieldAccess { object, field } => {
                if let Expr::Identifier(obj_name) = &object.0 {
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

        self.require_unsafe(&func_name, span);
        self.reject_if_wasm_incompatible_call(&func_name, span);

        // Check if name is a user-defined enum variant constructor first.
        // Separate lookup (immutable borrow) from processing (mutable borrow)
        // to avoid cloning the entire type_defs map.
        //
        // Handle both unqualified (`Circle(5)`) and qualified (`Shape::Circle(5)`) forms.
        let constructor_match = self.lookup_variant_constructor(&func_name);
        if let Some((type_name, expected_params, type_params)) = constructor_match {
            let type_param_count = type_params.len();
            if type_param_count == 0 {
                if let Some(type_args_provided) = type_args {
                    if !type_args_provided.is_empty() {
                        self.report_error(
                            TypeErrorKind::ArityMismatch,
                            span,
                            format!(
                                "this constructor takes 0 type argument(s) but {} were supplied",
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
                                "this constructor takes {} type argument(s) but {} were supplied",
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
            return Ty::normalize_named(type_name, resolved_args);
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
                // Inherit the layout+Copy guard from check_call_against_expected_constructor.
                if crate::stdlib::vec_element_runtime_suffix(&resolved_elem, &self.type_defs)
                    == Some("layout")
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
                    if let Ty::Array(inner, _) = arr_ty {
                        self.expect_type(&elem, &inner, span);
                    }
                }
                return self.make_vec_type(elem, span);
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
                                    name: child_type.clone(),
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
        let resolved_fn_name = scoped_module_item_name(self.current_module.as_deref(), &func_name)
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
                        let symbol = resolved_fn_name
                            .rsplit_once('.')
                            .map_or(resolved_fn_name.as_str(), |(_, n)| n);
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
                .get(&(self.current_module.clone(), func_name.clone()))
                .cloned()
            {
                self.used_modules
                    .borrow_mut()
                    .insert(ImportKey::new(self.current_module.clone(), module));
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
                // site — whether the args were inferred or supplied via
                // explicit turbofish (`id<i64>(5)`). Turbofish args resolve
                // to fully-concrete `Ty` through `instantiate_fn_sig_for_call`,
                // and `record_concrete_call_type_args` stays fail-closed on any
                // residual inference var, so recording is always safe. HIR and
                // MIR consume this side-table to emit and dispatch the
                // monomorphised symbol; skipping turbofish sites starved that
                // pipeline and tripped the NYI function-call lowering arm.
                true,
            );

            if resolved_fn_name == "Rc::new" {
                if let (Some(payload_ty), Some(arg)) = (applied_sig.params.first(), args.first()) {
                    let (_, arg_span) = arg.expr();
                    let resolved_payload = self.subst.resolve(payload_ty);
                    self.validate_rc_payload_type(&resolved_payload, arg_span);
                }
            }

            if resolved_fn_name == "len" {
                if let Some(Ty::Named { name, args, .. }) =
                    applied_sig.params.first().map(|ty| self.subst.resolve(ty))
                {
                    if Ty::names_match_qualified(&name, "HashSet") {
                        let elem_ty = args.first().cloned().unwrap_or(Ty::Var(TypeVar::fresh()));
                        if !self.validate_hashset_element_type(&elem_ty, span) {
                            return Ty::Error;
                        }
                        self.record_hashset_lowering_fact(span, &elem_ty);
                    }
                }
            }

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
                        // inferred — see the resolved-fn path above. The
                        // `record_concrete_call_type_args` inference-var guard
                        // keeps the boundary fail-closed.
                        true,
                    )
                    .return_type;
            }

            let func_ty = binding.ty.clone();

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
                    "E_CLOSURE_RECURSIVE: closure cannot call itself via binding \
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

    pub(super) fn check_call_with_type(
        &mut self,
        func_ty: &Ty,
        args: &[CallArg],
        span: &Span,
    ) -> Ty {
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
                *ret
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
                if matches!(reply_ty, Ty::Unit) {
                    Ty::result(Ty::Unit, Ty::send_error())
                } else {
                    Ty::result(reply_ty, Ty::ask_error())
                }
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
                    Ty::Named { name, .. } if name == "Receiver" || name == "channel.Receiver"
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
                format!("Channel<{resolved}> is not supported in `for await`: {reason}"),
            );
            return;
        }

        self.reject_wasm_feature(span, WasmUnsupportedFeature::BlockingChannelRecv);
    }
}
