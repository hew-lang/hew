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
    fn lookup_variant_constructor(
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
            Ty::Named { name, args }
                if Ty::names_match_qualified(name, type_name) && args.len() == arity =>
            {
                Some(args.clone())
            }
            _ => None,
        }
    }

    pub(super) fn record_concrete_call_type_args(&mut self, span: &Span, type_args: &[Ty]) {
        let concrete: Vec<Ty> = type_args.iter().map(|ty| self.subst.resolve(ty)).collect();
        if concrete.iter().all(|ty| !ty.has_inference_var()) {
            self.call_type_args.insert(SpanKey::from(span), concrete);
        }
    }

    fn record_builtin_result_output_type_args(&mut self, span: &Span, ok_ty: &Ty, err_ty: &Ty) {
        self.builtin_result_output_type_args
            .insert(SpanKey::from(span), (ok_ty.clone(), err_ty.clone()));
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

        self.enforce_type_param_bounds(sig, &resolved_type_args, span);

        if record_call_type_args && !sig.type_params.is_empty() {
            self.record_concrete_call_type_args(span, &resolved_type_args);
        }

        AppliedCallSignature {
            params: freshened_params,
            return_type: freshened_ret,
        }
    }

    pub(super) fn check_call_against_expected_constructor(
        &mut self,
        func: &Spanned<Expr>,
        type_args: Option<&[Spanned<TypeExpr>]>,
        args: &[CallArg],
        expected: &Ty,
        span: &Span,
    ) -> Option<Ty> {
        if type_args.is_some() {
            return None;
        }

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

        let resolved_expected = self.subst.resolve(expected);

        if let Some((type_name, expected_params, type_params)) =
            self.lookup_variant_constructor(&func_name)
        {
            let mut inferred_args = Self::expected_constructor_type_args(
                &resolved_expected,
                &type_name,
                type_params.len(),
            )?;
            self.check_arity(args, expected_params.len(), "this function", span);
            for (i, arg) in args.iter().enumerate() {
                if let Some(param_ty) = expected_params.get(i) {
                    let (expr, arg_span) = arg.expr();
                    let mut expected_ty = param_ty.clone();
                    for (param, replacement) in type_params.iter().zip(inferred_args.iter()) {
                        expected_ty = expected_ty.substitute_named_param(param, replacement);
                    }
                    self.check_against(expr, arg_span, &expected_ty);
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
            // crypto.random_bytes falls back to a seeded non-cryptographic PRNG
            // on wasm32; warn rather than reject so programs can still use it
            // for test data with the degraded-semantics caveat.
            "random_bytes" => {
                self.warn_wasm_limitation(span, WasmUnsupportedFeature::CryptoRandom);
            }
            _ => {}
        }
    }

    /// Emit a `BlockingCallInReceiveFn` warning when a known blocking operation
    /// is called from inside an actor receive function.
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
            for (i, arg) in args.iter().enumerate() {
                if let Some(param_ty) = expected_params.get(i) {
                    let (expr, span) = arg.expr();
                    let mut expected_ty = param_ty.clone();
                    if !type_params.is_empty() {
                        for (param, replacement) in type_params.iter().zip(inferred_args.iter()) {
                            expected_ty = expected_ty.substitute_named_param(param, replacement);
                        }
                    }
                    self.check_against(expr, span, &expected_ty);
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
                // supervisor_child(sup, index) → typed ActorRef based on supervisor decl
                let (sup_expr, sup_sp) = args[0].expr();
                let sup_ty = self.synthesize(sup_expr, sup_sp);
                let sup_ty_resolved = self.subst.resolve(&sup_ty);
                let (idx_expr, idx_sp) = args[1].expr();
                self.check_against(idx_expr, idx_sp, &Ty::I64);

                if let Some(Ty::Named { name: sup_name, .. }) = sup_ty_resolved.as_actor_ref() {
                    if let Some(children) = self.supervisor_children.get(sup_name) {
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
                            if i < children.len() {
                                let child_type = &children[i].1;
                                return Ty::actor_ref(Ty::Named {
                                    name: child_type.clone(),
                                    args: vec![],
                                });
                            }
                        }
                        // Non-constant index: fresh type var
                        return Ty::actor_ref(Ty::Var(TypeVar::fresh()));
                    }
                }
                return Ty::actor_ref(Ty::Var(TypeVar::fresh()));
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
            // Purity check: pure functions can only call other pure functions
            if self.in_pure_function && !sig.is_pure {
                self.report_error(
                    TypeErrorKind::PurityViolation,
                    span,
                    format!("cannot call impure function `{func_name}` from a pure function"),
                );
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
            let applied_sig = self.apply_instantiated_call_signature(
                &sig,
                type_args,
                args,
                span,
                SignatureArgApplication::FunctionLike {
                    param_names: &sig.param_names,
                    accepts_kwargs: sig.accepts_kwargs,
                    module_qualified: false,
                },
                type_args.is_none(),
            );

            if resolved_fn_name == "Rc::new" {
                if let (Some(payload_ty), Some(arg)) = (applied_sig.params.first(), args.first()) {
                    let (_, arg_span) = arg.expr();
                    let resolved_payload = self.subst.resolve(payload_ty);
                    self.validate_rc_payload_type(&resolved_payload, arg_span);
                }
            }

            if resolved_fn_name == "len" {
                if let Some(Ty::Named { name, args }) =
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
        if let Some(binding) = self.env.lookup(&func_name) {
            if let Some(sig) = binding
                .def_span
                .as_ref()
                .and_then(|def_span| self.lambda_poly_sig_map.get(&SpanKey::from(def_span)))
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
                        type_args.is_none(),
                    )
                    .return_type;
            }

            let func_ty = binding.ty.clone();
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
            let synthesized = self.synthesize(method_expr, method_span);
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
        if !matches!(resolved, Ty::String) && !resolved.is_integer() {
            self.report_error(
                TypeErrorKind::InvalidOperation,
                span,
                format!(
                    "Channel<{resolved}> is not supported in `for await`; \
                     only Channel<String> and Channel<int> are currently supported"
                ),
            );
            return;
        }

        self.reject_wasm_feature(span, WasmUnsupportedFeature::BlockingChannelRecv);
    }
}
