#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
use super::*;

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
        if concrete
            .iter()
            .all(|ty| !ty_has_unresolved_inference_var(ty))
        {
            self.call_type_args.insert(SpanKey::from(span), concrete);
        }
    }

    fn record_builtin_result_output_type_args(&mut self, span: &Span, ok_ty: &Ty, err_ty: &Ty) {
        self.builtin_result_output_type_args
            .insert(SpanKey::from(span), (ok_ty.clone(), err_ty.clone()));
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
                        expected_ty = self.substitute_named_param(&expected_ty, param, replacement);
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

    pub(super) fn warn_if_wasm_incompatible_call(&mut self, func_name: &str, span: &Span) {
        if !self.wasm_target {
            return;
        }
        match func_name {
            "link" | "unlink" | "monitor" | "demonitor" => {
                self.warn_wasm_limitation(span, WasmUnsupportedFeature::LinkMonitor);
            }
            "supervisor_child" | "supervisor_stop" => {
                self.warn_wasm_limitation(span, WasmUnsupportedFeature::SupervisionTrees);
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
        self.warn_if_wasm_incompatible_call(&func_name, span);

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
                            expected_ty =
                                self.substitute_named_param(&expected_ty, param, replacement);
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
            let (freshened_params, freshened_ret, resolved_type_args) =
                self.instantiate_fn_sig_for_call(&sig, type_args, span);

            // Separate positional and named args
            let positional_count = args.iter().take_while(|a| a.name().is_none()).count();
            let positional_args = &args[..positional_count];
            let named_args = &args[positional_count..];

            // Arity check: for accepts_kwargs functions, only check positional args
            // against required params; for normal functions, all args must match
            if !sig.accepts_kwargs && args.len() != freshened_params.len() {
                self.report_error(
                    TypeErrorKind::ArityMismatch,
                    span,
                    format!(
                        "this function takes {} argument(s) but {} were supplied",
                        freshened_params.len(),
                        args.len()
                    ),
                );
            } else if sig.accepts_kwargs && positional_count < freshened_params.len() {
                self.report_error(
                    TypeErrorKind::ArityMismatch,
                    span,
                    format!(
                        "this function takes at least {} positional argument(s) but {} were supplied",
                        freshened_params.len(),
                        positional_count
                    ),
                );
            }

            // Check positional args by index
            for (i, arg) in positional_args.iter().enumerate() {
                if let Some(param_ty) = freshened_params.get(i) {
                    let (expr, sp) = arg.expr();
                    self.check_against(expr, sp, param_ty);
                }
            }

            // Check named args by name lookup
            for arg in named_args {
                if let Some(name) = arg.name() {
                    if let Some(idx) = sig.param_names.iter().position(|n| n == name) {
                        if let Some(param_ty) = freshened_params.get(idx) {
                            let (expr, sp) = arg.expr();
                            self.check_against(expr, sp, param_ty);
                        }
                    } else if !sig.accepts_kwargs {
                        let (_, sp) = arg.expr();
                        self.report_error(
                            TypeErrorKind::InvalidOperation,
                            sp,
                            format!("unknown named argument `{name}`"),
                        );
                    } else {
                        // For kwargs functions, synthesize the expression type
                        let (expr, sp) = arg.expr();
                        self.synthesize(expr, sp);
                    }
                }
            }
            self.enforce_type_param_bounds(&sig, &resolved_type_args, span);

            // When the caller omitted explicit type arguments, resolve the
            // inferred type variables to concrete types and record them so the
            // enrichment layer can fill them in before serialization.
            if type_args.is_none() && !sig.type_params.is_empty() {
                self.record_concrete_call_type_args(span, &resolved_type_args);
            }

            if resolved_fn_name == "Rc::new" {
                if let (Some(payload_ty), Some(arg)) = (freshened_params.first(), args.first()) {
                    let (_, arg_span) = arg.expr();
                    let resolved_payload = self.subst.resolve(payload_ty);
                    self.validate_rc_payload_type(&resolved_payload, arg_span);
                }
            }

            if resolved_fn_name == "len" {
                if let Some(Ty::Named { name, args }) =
                    freshened_params.first().map(|ty| self.subst.resolve(ty))
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

            return freshened_ret;
        }

        // Then check if it's a variable with a function type (e.g., lambda parameters)
        if let Some(binding) = self.env.lookup(&func_name) {
            let func_ty = binding.ty.clone();
            let ret = self.check_call_with_type(&func_ty, args, span);
            // If this variable was bound to a generic lambda, extract the now-
            // resolved concrete types for each type parameter and record them
            // in call_type_args so the enricher can fill in the type_args field
            // before the AST is serialised for the codegen.
            if let Some(poly_vars) = self.lambda_poly_type_var_map.get(&func_name).cloned() {
                if type_args.is_none() {
                    let inferred_type_args: Vec<Ty> =
                        poly_vars.iter().map(|(_, tv)| Ty::Var(*tv)).collect();
                    self.record_concrete_call_type_args(span, &inferred_type_args);
                }
            }
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
        if ty_has_unresolved_inference_var(&resolved) {
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
        }
    }
}
