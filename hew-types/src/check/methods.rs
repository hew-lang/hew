#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
use super::*;
use crate::builtin_names::BuiltinNamedType;
use crate::check::calls::SignatureArgApplication;
use crate::method_resolution::{
    collect_method_sigs_for_receiver, lookup_builtin_method_sig,
    lookup_named_method_sig as shared_lookup_named_method_sig,
};
use crate::BuiltinType;

impl Checker {
    fn numeric_method_signedness(ty: &Ty) -> Option<NumericSignedness> {
        match ty {
            Ty::I8 | Ty::I16 | Ty::I32 | Ty::I64 | Ty::Isize => Some(NumericSignedness::Signed),
            Ty::U8 | Ty::U16 | Ty::U32 | Ty::U64 | Ty::Usize => Some(NumericSignedness::Unsigned),
            _ => None,
        }
    }

    fn numeric_method_width(ty: &Ty) -> Option<NumericWidth> {
        match ty {
            Ty::I8 | Ty::U8 => Some(NumericWidth::Bits(8)),
            Ty::I16 | Ty::U16 => Some(NumericWidth::Bits(16)),
            Ty::I32 | Ty::U32 => Some(NumericWidth::Bits(32)),
            Ty::I64 | Ty::U64 => Some(NumericWidth::Bits(64)),
            Ty::Isize | Ty::Usize => Some(NumericWidth::Pointer),
            _ => None,
        }
    }

    pub(super) fn record_hashset_lowering_fact(&mut self, span: &Span, elem_ty: &Ty) {
        let key = SpanKey::from(span);
        // If deferred admission was already recorded for this span, the
        // lowering-fact finalizer becomes the sole authority for any
        // InferenceFailed diagnostic at this site.  Remove the deferred entry
        // to prevent a duplicate error from finalize_hashset_admission.
        self.deferred_hashset_admission.remove(&key);
        self.pending_lowering_facts.insert(
            key,
            PendingLoweringFact::hashset(elem_ty.clone(), self.current_module.clone()),
        );
    }

    /// Drain `pending_lowering_facts`, resolve element types through the
    /// substitution, and materialize concrete [`LoweringFact`] entries.
    ///
    /// Any fact whose element type is still unresolved after inference emits a
    /// checker error and is **not** inserted into the returned map.  Downstream
    /// codegen (`requireLoweringFactOf`) will detect the missing entry and fail
    /// closed rather than guessing.
    pub(super) fn finalize_lowering_facts(&mut self) -> HashMap<SpanKey, LoweringFact> {
        let pending = std::mem::take(&mut self.pending_lowering_facts);
        let mut result = HashMap::with_capacity(pending.len());
        let mut new_errors: Vec<crate::error::TypeError> = Vec::new();
        // Track which unresolved TypeVars have already produced a diagnostic so
        // that repeated method calls on the same unresolved HashSet (e.g.
        // `s.len(); s.is_empty()`) emit exactly one InferenceFailed rather than
        // one per call site.  Each unique unresolved root var gets one error.
        let mut reported_unresolved_vars: std::collections::HashSet<TypeVar> =
            std::collections::HashSet::new();

        for (span_key, pending_fact) in pending {
            let resolved_ty = self
                .subst
                .resolve(&pending_fact.hashset_element_ty)
                .materialize_literal_defaults();
            // Guard 1: element type is already erroneous — a prior diagnostic was
            // reported on the upstream expression.  Drop the pending fact silently;
            // downstream codegen fails closed via the absent lowering-fact entry.
            // Emitting a new diagnostic here would produce a spurious secondary
            // "element type is unresolved" error even though inference completed
            // correctly — it just completed to Ty::Error.
            if resolved_ty.contains_error() {
                continue;
            }
            match LoweringFact::from_hashset_element_type(&resolved_ty) {
                Ok(fact) => {
                    result.insert(span_key, fact);
                }
                Err(LoweringFactError::UnresolvedHashSetElementType) => {
                    // Inference did not resolve the element type by the checker
                    // boundary.  Emit a clear diagnostic (at most once per
                    // unique unresolved TypeVar) and prune the fact so
                    // downstream codegen fails closed via requireLoweringFactOf.
                    if let Ty::Var(var) = resolved_ty {
                        if !reported_unresolved_vars.insert(var) {
                            // Already emitted for this root var (another call
                            // site on the same unresolved set).  Skip to avoid
                            // spraying one error per method-call site.
                            continue;
                        }
                    }
                    let span = span_key.start..span_key.end;
                    let mut err = crate::error::TypeError::new(
                        TypeErrorKind::InferenceFailed,
                        span,
                        "cannot lower HashSet: element type is unresolved at the checker \
                         boundary — add an explicit type annotation, e.g. \
                         `HashSet<i64>` or `HashSet<String>`"
                            .to_string(),
                    );
                    if let Some(module) = &pending_fact.source_module {
                        err = err.with_source_module(module.clone());
                    }
                    new_errors.push(err);
                    // Fact NOT inserted — downstream will fail closed.
                }
                Err(LoweringFactError::UnsupportedHashSetElementType { .. }) => {
                    // The checker already rejected unsupported element types via
                    // validate_hashset_element_type / validate_hashset_owned_element_type.
                    // Skip silently to avoid a duplicate diagnostic.
                }
            }
        }

        self.errors.extend(new_errors);
        result
    }

    /// Drain `deferred_hashmap_admission`, resolve key/value types through the
    /// current substitution, and fail closed on any that are still unresolved
    /// or error-typed at the checker boundary.
    ///
    /// * `Ty::Var` → `InferenceFailed`: inference did not resolve the type.
    /// * `Ty::Error` → silent drop: upstream already emitted a diagnostic.
    /// * Fully-resolved unsupported pairs → already caught inline; silently
    ///   skipped here to avoid duplicate diagnostics.
    pub(super) fn finalize_hashmap_admission(&mut self) {
        let checks = std::mem::take(&mut self.deferred_hashmap_admission);
        let mut new_errors: Vec<crate::error::TypeError> = Vec::new();
        // Track which (key_var, val_var) pairs have already produced a
        // diagnostic so that repeated method calls on the same unresolved
        // HashMap (e.g. `m.len(); m.is_empty()`) emit exactly one
        // InferenceFailed rather than one per call site.
        let mut reported_var_pairs: std::collections::HashSet<(Option<TypeVar>, Option<TypeVar>)> =
            std::collections::HashSet::new();

        for (_span_key, check) in checks {
            let resolved_key = self
                .subst
                .resolve(&check.key_ty)
                .materialize_literal_defaults();
            let resolved_val = self
                .subst
                .resolve(&check.val_ty)
                .materialize_literal_defaults();

            // Already-errored types: fail closed without cascading.
            if matches!(resolved_key, Ty::Error) || matches!(resolved_val, Ty::Error) {
                continue;
            }

            // Still unresolved at the checker boundary → fail closed, but
            // deduplicate across multiple call sites that share the same
            // unresolved root vars.
            if matches!(resolved_key, Ty::Var(_)) || matches!(resolved_val, Ty::Var(_)) {
                let key_var = if let Ty::Var(v) = resolved_key {
                    Some(v)
                } else {
                    None
                };
                let val_var = if let Ty::Var(v) = resolved_val {
                    Some(v)
                } else {
                    None
                };
                if !reported_var_pairs.insert((key_var, val_var)) {
                    // Already emitted for this root (key_var, val_var) pair.
                    continue;
                }
                let key_resolved_display = self
                    .subst
                    .resolve(&check.key_ty)
                    .materialize_literal_defaults();
                let val_resolved_display = self
                    .subst
                    .resolve(&check.val_ty)
                    .materialize_literal_defaults();
                let key_display = key_resolved_display.user_facing();
                let val_display = val_resolved_display.user_facing();
                let mut err = crate::error::TypeError::new(
                    TypeErrorKind::InferenceFailed,
                    check.span.clone(),
                    format!(
                        "cannot infer HashMap key or value type at the checker boundary \
                         (HashMap<{key_display}, {val_display}>); add an explicit type \
                         annotation, e.g. `HashMap<String, i64>`",
                    ),
                );
                if let Some(module) = check.source_module {
                    err = err.with_source_module(module);
                }
                new_errors.push(err);
            }

            // Fully resolved but unsupported pair: the inline check should have
            // already emitted a diagnostic. Skip to avoid duplicates.
        }

        self.errors.extend(new_errors);
    }

    /// Drain `deferred_hashset_admission`, resolve element types through the
    /// current substitution, and fail closed on any that are still unresolved
    /// or error-typed at the checker boundary.
    ///
    /// * `Ty::Var` → `InferenceFailed`: inference did not resolve the element type.
    /// * `Ty::Error` → silent drop: upstream already emitted a diagnostic.
    /// * Fully-resolved unsupported elements → already caught inline; silently
    ///   skipped here to avoid duplicate diagnostics.
    pub(super) fn finalize_hashset_admission(&mut self) {
        let checks = std::mem::take(&mut self.deferred_hashset_admission);
        let mut new_errors: Vec<crate::error::TypeError> = Vec::new();

        for (_span_key, check) in checks {
            let resolved = self
                .subst
                .resolve(&check.elem_ty)
                .materialize_literal_defaults();

            // Already-errored type: fail closed without cascading.
            if matches!(resolved, Ty::Error) {
                continue;
            }

            // Still unresolved at the checker boundary → fail closed.
            if matches!(resolved, Ty::Var(_)) {
                let mut err = crate::error::TypeError::new(
                    TypeErrorKind::InferenceFailed,
                    check.span.clone(),
                    format!(
                        "cannot infer HashSet element type at the checker boundary \
                         (HashSet<{}>); add an explicit type annotation, \
                         e.g. `HashSet<String>` or `HashSet<i64>`",
                        resolved.user_facing(),
                    ),
                );
                if let Some(module) = check.source_module {
                    err = err.with_source_module(module);
                }
                new_errors.push(err);
            }

            // Fully resolved but unsupported element: the inline check should
            // have already emitted a diagnostic. Skip to avoid duplicates.
        }

        self.errors.extend(new_errors);
    }

    /// Drain `deferred_vec_admission`, resolve element types through the
    /// current substitution, and fail closed on any that are still unresolved
    /// or error-typed at the checker boundary.
    ///
    /// * Any surviving inference variable inside the element type →
    ///   [`TypeErrorKind::InferenceFailed`].
    /// * `Ty::Error` (anywhere inside the element type) → silent drop:
    ///   upstream already emitted a diagnostic.
    /// * Fully-resolved types are revalidated so late-resolved unsupported
    ///   element types are rejected just like inline admission sites.
    pub(super) fn finalize_vec_admission(&mut self) {
        let checks = std::mem::take(&mut self.deferred_vec_admission);
        let mut new_errors: Vec<crate::error::TypeError> = Vec::new();
        let mut reported_unresolved_roots: std::collections::HashSet<Vec<u32>> =
            std::collections::HashSet::new();

        for (_span_key, check) in checks {
            let resolved = self
                .subst
                .resolve(&check.elem_ty)
                .materialize_literal_defaults();

            if resolved.contains_error() {
                continue;
            }

            if resolved.has_inference_var() {
                let mut unresolved_vars = HashSet::new();
                collect_unresolved_inference_vars(&resolved, &mut unresolved_vars);
                let mut unresolved_roots: Vec<u32> =
                    unresolved_vars.into_iter().map(|var| var.0).collect();
                unresolved_roots.sort_unstable();
                unresolved_roots.dedup();
                if !reported_unresolved_roots.insert(unresolved_roots) {
                    continue;
                }

                let mut err = crate::error::TypeError::new(
                    TypeErrorKind::InferenceFailed,
                    check.span.clone(),
                    format!(
                        "cannot infer Vec element type at the checker boundary \
                         (Vec<{}>); add an explicit type annotation",
                        resolved.user_facing(),
                    ),
                );
                if let Some(module) = check.source_module {
                    err = err.with_source_module(module);
                }
                new_errors.push(err);
                continue;
            }

            let _ = self.validate_resolved_vec_element_type(&resolved, &check.span);
        }

        self.errors.extend(new_errors);
    }

    /// Drain `deferred_channel_rewrites`, resolve each inner type through the
    /// current substitution, and record the correct type-specific C symbol.
    ///
    /// This must be called from `check_program` **after** all inference has
    /// settled (i.e. after `check_item` for every item in the program, and
    /// after all other inference-driving passes like `apply_deferred_range_bound_types`).
    ///
    /// * Fully resolved concrete type (`String` or integer) → select symbol and
    ///   record [`MethodCallRewrite::RewriteToFunction`].
    /// * `Ty::Error` → skip silently; a diagnostic was already emitted upstream.
    /// * `Ty::Var` (still unresolved) → emit [`TypeErrorKind::InferenceFailed`];
    ///   leave the span absent from `method_call_rewrites` so codegen fails
    ///   closed rather than silently using the wrong ABI.
    /// * Unsupported concrete type → emit [`TypeErrorKind::InvalidOperation`];
    ///   the inline validation pass may have already emitted a diagnostic, but
    ///   deferred entries bypass that guard, so we re-check here.
    pub(super) fn finalize_channel_rewrites(&mut self) {
        let deferred = std::mem::take(&mut self.deferred_channel_rewrites);
        let mut new_errors: Vec<crate::error::TypeError> = Vec::new();

        for (span_key, entry) in deferred {
            let resolved = self
                .subst
                .resolve(&entry.inner_ty)
                .materialize_literal_defaults();

            // Already-errored upstream: fail closed, no duplicate diagnostic.
            if resolved.contains_error() {
                continue;
            }

            // Still unresolved: inference did not converge on a concrete type.
            if let Ty::Var(_) = &resolved {
                let mut err = crate::error::TypeError::new(
                    TypeErrorKind::InferenceFailed,
                    span_key.start..span_key.end,
                    format!(
                        "cannot resolve channel method `{}`: inner type of \
                         {}<T> is still unknown after inference — \
                         add an explicit type annotation, e.g. \
                         `Sender<int>` or `Receiver<string>`",
                        entry.method, entry.handle_kind,
                    ),
                );
                if let Some(module) = &entry.source_module {
                    err = err.with_source_module(module.clone());
                }
                new_errors.push(err);
                // Span intentionally absent from method_call_rewrites → codegen fails closed.
                continue;
            }

            // Reject unsupported concrete types (guard against deferred entries
            // that escaped the inline validation because T was Var at visit time
            // but resolved to something unsupported, e.g. a user struct).
            if !matches!(resolved, Ty::String) && !resolved.is_integer() {
                let mut err = crate::error::TypeError::new(
                    TypeErrorKind::InvalidOperation,
                    span_key.start..span_key.end,
                    format!(
                        "Channel<{resolved}> is not supported; \
                         only Channel<string> and Channel<i64> are currently supported"
                    ),
                );
                if let Some(module) = &entry.source_module {
                    err = err.with_source_module(module.clone());
                }
                new_errors.push(err);
                continue;
            }

            // Concrete, supported type: select the correct C symbol.
            if let Some(c_symbol) = crate::stdlib::resolve_channel_method(
                &entry.handle_kind,
                &entry.method,
                Some(&resolved),
            ) {
                self.method_call_rewrites.insert(
                    span_key,
                    MethodCallRewrite::RewriteToFunction {
                        c_symbol: c_symbol.to_string(),
                        elem_ty: None,
                    },
                );
            } else {
                let mut err = crate::error::TypeError::new(
                    TypeErrorKind::InvalidOperation,
                    span_key.start..span_key.end,
                    format!(
                        "internal compiler error: builtin {}::{} is missing runtime rewrite metadata",
                        entry.handle_kind, entry.method
                    ),
                );
                if let Some(module) = &entry.source_module {
                    err = err.with_source_module(module.clone());
                }
                new_errors.push(err);
            }
        }

        self.errors.extend(new_errors);
    }

    pub(super) fn record_method_call_receiver_kind(
        &mut self,
        span: &Span,
        kind: MethodCallReceiverKind,
    ) {
        self.method_call_receiver_kinds
            .insert(SpanKey::from(span), kind);
    }

    /// Returns whether the qualified method name `Trait::method` is in the
    /// recognised consume-receiver set. PR 1 (#1295) ships an empty set; PR 2
    /// populates it for `Closable::close` when the trait is registered.
    fn is_consume_receiver_method(&self, qualified_name: &str) -> bool {
        self.consume_receiver_methods.contains(qualified_name)
    }

    /// Returns true if any trait impl on `type_name` registered a method
    /// named `method` that is in the recognised consume-receiver set.
    ///
    /// Stdlib `impl Closable for T { fn close }` flattens trait methods into
    /// the inherent-method table on `T`, so the dispatch at the named-type
    /// site doesn't carry the originating trait. To honour
    /// `consumes_receiver` declared on the trait, we walk the
    /// `trait_impls_set` for matching `(type, trait)` pairs and check the
    /// qualified `Trait::method` form against the consume set.
    fn named_type_method_consumes_receiver(&self, type_name: &str, method: &str) -> bool {
        if self.consume_receiver_methods.is_empty() {
            return false;
        }
        self.trait_impls_set
            .iter()
            .filter(|(ty, _)| ty == type_name)
            .any(|(_, trait_name)| {
                self.is_consume_receiver_method(&format!("{trait_name}::{method}"))
            })
    }

    /// Apply the consume-receiver marker for a trait-dispatched method call:
    /// record the per-call-site flag for codegen and mark the receiver
    /// expression moved so subsequent uses surface `UseAfterMove`. No-op
    /// when the resolved method does not declare `consumes_receiver`.
    fn apply_consume_receiver_if_flagged(
        &mut self,
        qualified_method: &str,
        receiver: &Spanned<Expr>,
        receiver_ty: &Ty,
        span: &Span,
    ) {
        if !self.is_consume_receiver_method(qualified_method) {
            return;
        }
        self.method_call_consumes_receiver
            .insert(SpanKey::from(span));
        let resolved_ty = self.subst.resolve(receiver_ty);
        self.mark_expr_moved_if_non_copy(&receiver.0, &receiver.1, &resolved_ty);
    }

    fn record_method_call_rewrite(&mut self, span: &Span, rewrite: MethodCallRewrite) {
        self.method_call_rewrites
            .insert(SpanKey::from(span), rewrite);
    }

    fn record_actor_method_dispatch(&mut self, span: &Span, method_id: String, reply_ty: Ty) {
        let dispatch = if matches!(self.subst.resolve(&reply_ty), Ty::Unit) {
            ActorMethodKind::Fire(method_id)
        } else {
            ActorMethodKind::Ask(method_id, reply_ty)
        };
        self.actor_method_dispatch
            .insert(SpanKey::from(span), dispatch);
    }

    fn canonical_handle_receiver_type_name(&self, receiver_ty: &Ty) -> Option<String> {
        let Ty::Named { name, .. } = receiver_ty else {
            return None;
        };
        if self.module_registry.is_handle_type(name) {
            return Some(name.clone());
        }
        if name.contains('.') {
            return Some(name.clone());
        }
        self.module_registry.qualify_handle_type(name)
    }

    fn record_handle_method_call_receiver_kind_if_any(&mut self, receiver_ty: &Ty, span: &Span) {
        let Some(type_name) = self.canonical_handle_receiver_type_name(receiver_ty) else {
            return;
        };
        self.record_method_call_receiver_kind(
            span,
            MethodCallReceiverKind::HandleInstance { type_name },
        );
    }

    fn record_runtime_method_call_rewrite(&mut self, span: &Span, c_symbol: impl Into<String>) {
        self.record_method_call_rewrite(
            span,
            MethodCallRewrite::RewriteToFunction {
                c_symbol: c_symbol.into(),
                elem_ty: None,
            },
        );
    }

    fn record_monomorphic_extern_symbol_rewrite_if_any(
        &mut self,
        sig: &FnSig,
        span: &Span,
    ) -> bool {
        let Some(spec) = &sig.extern_symbol else {
            return false;
        };
        if !spec.template.is_monomorphic() {
            self.report_error(
                TypeErrorKind::InvalidOperation,
                span,
                format!(
                    "extern-symbol template `{}` is not monomorphic; Stage 1 only supports \
                     monomorphic receiver FFI canaries",
                    spec.template.raw
                ),
            );
            return false;
        }
        self.record_runtime_method_call_rewrite(span, spec.template.raw.clone());
        true
    }

    fn dispatch_monomorphic_extern_symbol_method(
        &mut self,
        receiver_type_name: &str,
        type_args: &[Ty],
        method: &str,
        args: &[CallArg],
        span: &Span,
    ) -> Option<Ty> {
        let sig = self.lookup_named_method_sig(receiver_type_name, type_args, method)?;
        sig.extern_symbol.as_ref()?;
        let applied_sig = self.apply_instantiated_call_signature(
            &sig,
            None,
            args,
            span,
            SignatureArgApplication::PositionalOnly {
                arity_context: format!("method '{method}'"),
            },
            true,
        );
        self.record_monomorphic_extern_symbol_rewrite_if_any(&sig, span);
        Some(applied_sig.return_type)
    }

    fn missing_builtin_contract_error(
        &mut self,
        span: &Span,
        builtin: &str,
        method: &str,
        item: &str,
    ) {
        self.report_error(
            TypeErrorKind::InvalidOperation,
            span,
            format!(
                "internal compiler error: builtin {builtin}::{method} is missing {item} metadata"
            ),
        );
    }

    fn require_builtin_runtime_symbol(
        &mut self,
        span: &Span,
        builtin: &str,
        method: &str,
        symbol: Option<&'static str>,
    ) -> Option<&'static str> {
        symbol.or_else(|| {
            self.missing_builtin_contract_error(span, builtin, method, "runtime rewrite");
            None
        })
    }

    fn require_builtin_method_sig(
        &mut self,
        span: &Span,
        receiver_ty: &Ty,
        builtin: &str,
        method: &str,
    ) -> Option<FnSig> {
        lookup_builtin_method_sig(receiver_ty, method).or_else(|| {
            self.missing_builtin_contract_error(span, builtin, method, "type signature");
            None
        })
    }

    fn record_module_qualified_method_call_rewrite(
        &mut self,
        span: &Span,
        c_symbol: impl Into<String>,
    ) {
        self.record_method_call_rewrite(
            span,
            MethodCallRewrite::RewriteModuleQualifiedToFunction {
                c_symbol: c_symbol.into(),
                elem_ty: None,
            },
        );
    }

    fn record_deferred_method_call_rewrite(&mut self, span: &Span) {
        self.record_method_call_rewrite(span, MethodCallRewrite::DeferToLowering);
    }

    /// Record a channel method rewrite to be resolved after inference settles.
    ///
    /// Called instead of `record_runtime_method_call_rewrite` when the inner
    /// type `T` of `Sender<T>` / `Receiver<T>` is still a `Ty::Var` at the
    /// call site.  The deferred entry is drained by `finalize_channel_rewrites`
    /// in `check_program`.
    fn record_deferred_channel_method_rewrite(
        &mut self,
        span: &Span,
        handle_kind: &str,
        method: &str,
        inner_ty: Ty,
    ) {
        self.deferred_channel_rewrites.insert(
            SpanKey::from(span),
            DeferredChannelMethodRewrite {
                handle_kind: handle_kind.to_string(),
                method: method.to_string(),
                inner_ty,
                source_module: self.current_module.clone(),
            },
        );
    }

    fn record_handle_method_call_rewrite_if_any(
        &mut self,
        receiver_ty: &Ty,
        method: &str,
        span: &Span,
    ) {
        self.record_handle_method_call_receiver_kind_if_any(receiver_ty, span);
        let Ty::Named { name, .. } = receiver_ty else {
            return;
        };
        if let Some(c_symbol) = self.module_registry.resolve_handle_method(name, method) {
            self.record_runtime_method_call_rewrite(span, c_symbol);
        }
    }

    fn record_module_qualified_stdlib_call_rewrite_if_any(
        &mut self,
        module_name: &str,
        method: &str,
        span: &Span,
    ) {
        if self.user_modules.contains(module_name) {
            return;
        }
        if let Some(c_symbol) = self
            .module_registry
            .resolve_module_call(module_name, method)
        {
            if c_symbol != method {
                self.record_module_qualified_method_call_rewrite(span, c_symbol);
            }
        }
    }

    /// Record a direct-call rewrite for a `module.fn(args)` invocation
    /// against a user-defined module.
    ///
    /// Mirrors `record_module_qualified_stdlib_call_rewrite_if_any` but for
    /// user modules: the qualified `module.fn` key is the rewrite target, no
    /// receiver is injected (per LESSONS `module-qualified-rewrite-authority`
    /// — argument list preserved). HIR's `RewriteModuleQualifiedToFunction`
    /// arm consumes the rewrite to emit a direct function call against the
    /// qualified symbol.
    fn record_module_qualified_user_call_rewrite_if_any(
        &mut self,
        module_name: &str,
        method: &str,
        span: &Span,
    ) {
        if !self.user_modules.contains(module_name) {
            return;
        }
        let key = format!("{module_name}.{method}");
        if self.fn_sigs.contains_key(&key) {
            self.record_module_qualified_method_call_rewrite(span, key);
        }
    }

    fn reject_if_wasm_native_only_network_module_call(&mut self, module_name: &str, span: &Span) {
        if self.user_modules.contains(module_name) {
            return;
        }
        match module_name {
            "http_client" => self.reject_wasm_feature(span, WasmUnsupportedFeature::HttpClient),
            "smtp" => self.reject_wasm_feature(span, WasmUnsupportedFeature::Smtp),
            _ => {}
        }
    }

    fn reject_if_wasm_native_only_handle(&mut self, receiver_ty: &Ty, span: &Span) {
        let Ty::Named { name, builtin, .. } = receiver_ty else {
            return;
        };
        if builtin.is_some_and(|builtin| {
            builtin.has_role(crate::builtin_type::BuiltinTypeRole::WasmNativeOnlyHandle)
        }) {
            self.reject_wasm_feature(span, WasmUnsupportedFeature::TcpNetworking);
            return;
        }
        let Some(module_name) = name.split('.').next() else {
            return;
        };
        if self.user_modules.contains(module_name) {
            return;
        }
        match name.as_str() {
            "http_client.Response" => {
                self.reject_wasm_feature(span, WasmUnsupportedFeature::HttpClient);
            }
            "smtp.Conn" => self.reject_wasm_feature(span, WasmUnsupportedFeature::Smtp),
            "process.Child" => {
                self.reject_wasm_feature(span, WasmUnsupportedFeature::ProcessExecution);
            }
            "http.Server" | "http.Request" => {
                self.reject_wasm_feature(span, WasmUnsupportedFeature::HttpServer);
            }
            "net.Listener" | "net.Connection" => {
                self.reject_wasm_feature(span, WasmUnsupportedFeature::TcpNetworking);
            }
            "tls.TlsStream" => {
                self.reject_wasm_feature(span, WasmUnsupportedFeature::Tls);
            }
            "quic.QUICEndpoint" | "quic.QUICConnection" | "quic.QUICStream" | "quic.QUICEvent" => {
                self.reject_wasm_feature(span, WasmUnsupportedFeature::Quic);
            }
            _ => {}
        }
    }

    fn reject_if_wasm_blocking_semaphore_method(
        &mut self,
        receiver_ty: &Ty,
        method: &str,
        span: &Span,
    ) {
        let Ty::Named { name, .. } = receiver_ty else {
            return;
        };
        if name != "semaphore.Semaphore" || self.user_modules.contains("semaphore") {
            return;
        }
        if matches!(method, "acquire" | "acquire_timeout") {
            self.reject_wasm_feature(span, WasmUnsupportedFeature::BlockingSemaphoreAcquire);
        }
    }

    pub(super) fn runtime_stream_element_name(ty: &Ty) -> Option<&'static str> {
        match ty {
            Ty::String => Some("string"),
            Ty::Bytes => Some("bytes"),
            _ => None,
        }
    }

    fn stream_receiver_element_kind(ty: &Ty) -> &'static str {
        match ty {
            Ty::String => "string",
            Ty::Bytes => "bytes",
            _ => "",
        }
    }

    pub(super) fn strip_module_prefix<'a>(&self, name: &'a str) -> Option<&'a str> {
        let dot = name.find('.')?;
        if self.modules.contains(&name[..dot]) {
            Some(&name[dot + 1..])
        } else {
            None
        }
    }

    /// Look up a type definition, handling module-qualified names like `json.Value`.
    pub(super) fn lookup_type_def(&self, name: &str) -> Option<TypeDef> {
        self.type_defs
            .get(name)
            .or_else(|| {
                self.strip_module_prefix(name)
                    .and_then(|u| self.type_defs.get(u))
            })
            .cloned()
    }

    /// Look up a type definition mutably, handling module-qualified names.
    pub(super) fn lookup_type_def_mut(&mut self, name: &str) -> Option<&mut TypeDef> {
        if self.type_defs.contains_key(name) {
            return self.type_defs.get_mut(name);
        }
        let unqualified = self.strip_module_prefix(name)?;
        self.type_defs.get_mut(unqualified)
    }

    /// Resolve a `(module, type)` pair to its `TypeDef`, gated on the type being
    /// in the imported module's exported set.  Returns `None` if the module is
    /// not a known alias, the type is not exported, or the qualified type alias
    /// was not registered (latter would be a registration bug — callers should
    /// treat as "type not exported" for diagnostic purposes).
    ///
    /// Mirrors the `module_fn_exports` guard pattern used by
    /// `check_method_call` for module-qualified function dispatch.
    pub(super) fn resolve_module_type(
        &self,
        module_short: &str,
        type_name: &str,
    ) -> Option<TypeDef> {
        if !self.modules.contains(module_short) {
            return None;
        }
        let exports = self.module_type_exports.get(module_short)?;
        if !exports.contains(type_name) {
            return None;
        }
        let qualified = format!("{module_short}.{type_name}");
        self.type_defs.get(&qualified).cloned()
    }

    /// Resolve a `(module, type, variant)` triple to its `VariantDef`, gated on
    /// the type being exported by the module.  Returns `None` if the module
    /// alias is unknown, the type is not exported, or the variant does not
    /// exist on the type.  The caller is responsible for emitting the
    /// fail-closed diagnostic in each failure case.
    pub(super) fn resolve_module_variant(
        &self,
        module_short: &str,
        type_name: &str,
        variant_name: &str,
    ) -> Option<(TypeDef, VariantDef)> {
        let td = self.resolve_module_type(module_short, type_name)?;
        let v = td.variants.get(variant_name).cloned()?;
        Some((td, v))
    }

    /// Look up a non-builtin named method via `type_defs` first, then `fn_sigs`.
    pub(super) fn lookup_named_method_sig(
        &self,
        type_name: &str,
        type_args: &[Ty],
        method: &str,
    ) -> Option<FnSig> {
        shared_lookup_named_method_sig(&self.type_defs, &self.fn_sigs, type_name, type_args, method)
            .or_else(|| {
                self.module_registry
                    .resolve_handle_method_sig(type_name, method)
                    .map(|(_c_symbol, params, return_type)| FnSig {
                        params,
                        return_type,
                        ..FnSig::default()
                    })
            })
    }

    /// Try to resolve a method call on a named type via `type_defs` and `fn_sigs`.
    ///
    /// Used as a fallback from hardcoded handle-type dispatch tables so that
    /// methods added via `.hew` impl blocks work without updating the tables.
    pub(super) fn try_resolve_named_method(
        &mut self,
        receiver_ty: &Ty,
        method: &str,
        args: &[CallArg],
        span: &Span,
    ) -> Option<Ty> {
        let Ty::Named {
            name,
            args: type_args,
            ..
        } = receiver_ty
        else {
            return None;
        };
        let sig = self.lookup_named_method_sig(name, type_args, method)?;
        Some(
            self.apply_instantiated_call_signature(
                &sig,
                None,
                args,
                span,
                SignatureArgApplication::PositionalOnly {
                    arity_context: format!("method '{method}'"),
                },
                true,
            )
            .return_type,
        )
    }

    /// Enforce the actor mailbox boundary on every arg of an actor receive
    /// method dispatch. Called after [`Self::try_resolve_named_method`] has
    /// already type-checked the args (so `self.expr_types` is populated), to
    /// avoid double synthesis.
    ///
    /// Each arg's type is looked up from `expr_types`; on a miss (e.g. the
    /// program already has an error at that arg) we skip the boundary record
    /// for that arg rather than re-synthesize. Codegen's fail-closed lookup
    /// is gated to non-error programs.
    fn enforce_actor_method_send_args(&mut self, args: &[CallArg]) {
        // Snapshot per-arg types from `expr_types` first; calling
        // `enforce_actor_boundary_send` mutates `self`, so we cannot hold a
        // borrow into `self.expr_types` across the call.
        let arg_types: Vec<Option<Ty>> = args
            .iter()
            .map(|arg| {
                let (_expr, sp) = arg.expr();
                self.expr_types.get(&SpanKey::from(sp)).cloned()
            })
            .collect();
        for (arg, ty_opt) in args.iter().zip(arg_types) {
            let (expr, sp) = arg.expr();
            if let Some(ty) = ty_opt {
                self.enforce_actor_boundary_send(expr, sp, sp, &ty);
            }
        }
    }

    pub(super) fn check_named_method_fallback(
        &mut self,
        receiver_ty: &Ty,
        method_name: &str,
        args: &[CallArg],
        span: &Span,
        type_display_name: &str,
    ) -> Ty {
        if let Some(ty) = self.try_resolve_named_method(receiver_ty, method_name, args, span) {
            if let Ty::Named { name, .. } = receiver_ty {
                // If the receiver type is a registered actor declaration AND
                // the resolved method is a receive handler (tracked in
                // `actor_receive_methods`), this dispatch crosses the
                // actor mailbox boundary. Record the per-arg alias-vs-copy
                // decision so codegen does not have to guess. Non-receive
                // `methods` declared on the same actor (also keyed
                // `{Actor}::{name}` in `fn_sigs`) stay on the regular
                // method-call path.
                let method_key = format!("{name}::{method_name}");
                let is_actor_receive_dispatch = self
                    .type_defs
                    .get(name)
                    .is_some_and(|td| td.kind == TypeDefKind::Actor)
                    && self.actor_receive_methods.contains(&method_key);
                if is_actor_receive_dispatch {
                    self.record_method_call_receiver_kind(
                        span,
                        MethodCallReceiverKind::ActorInstance {
                            actor_name: name.clone(),
                        },
                    );
                    self.enforce_actor_method_send_args(args);
                    self.record_actor_method_dispatch(span, method_key, ty.clone());
                } else {
                    self.record_method_call_receiver_kind(
                        span,
                        MethodCallReceiverKind::NamedTypeInstance {
                            type_name: name.clone(),
                        },
                    );
                }
            }
            self.record_handle_method_call_rewrite_if_any(receiver_ty, method_name, span);
            return ty;
        }

        // Synthesize args for error recovery so independent arg diagnostics are not suppressed.
        for arg in args {
            let (expr, sp) = arg.expr();
            self.synthesize(expr, sp);
        }
        self.report_error_with_suggestions(
            TypeErrorKind::UndefinedMethod,
            span,
            format!("no method `{method_name}` on {type_display_name}"),
            self.similar_methods(receiver_ty, method_name),
        );
        Ty::Error
    }

    fn similar_methods(&self, receiver_ty: &Ty, method_name: &str) -> Vec<String> {
        crate::error::find_similar(
            method_name,
            collect_method_sigs_for_receiver(&self.type_defs, &self.fn_sigs, receiver_ty)
                .iter()
                .map(|(name, _)| name.as_str()),
        )
    }

    #[expect(
        clippy::too_many_lines,
        reason = "builtin stream typing and checker-owned rewrite metadata stay together"
    )]
    pub(super) fn check_stream_method(
        &mut self,
        type_args: &[Ty],
        method: &str,
        args: &[CallArg],
        span: &Span,
    ) -> Ty {
        let Some(inner) = self.validate_stream_sink_element_type(
            type_args,
            BuiltinNamedType::Stream.canonical_name(),
            method,
            span,
        ) else {
            return Ty::Error;
        };
        if method == "decode" {
            return self.report_unlowerable_stream_codec_boundary(
                BuiltinNamedType::Stream.canonical_name(),
                &inner,
                method,
                span,
            );
        }
        // Gate 2: lowering-capability check.  Only string and bytes have a
        // runtime symbol; other Wire-capable types pass gate 1 but cannot be
        // lowered yet.  Emit a user-facing diagnostic rather than the ICE-
        // flavoured "missing runtime rewrite metadata" from require_builtin_runtime_symbol.
        let resolved_inner = self.subst.resolve(&inner);
        if Self::runtime_stream_element_name(&resolved_inner).is_none() {
            self.report_error(
                TypeErrorKind::InvalidOperation,
                span,
                format!(
                    "`Stream<{}>` is not supported; \
                     runtime lowering is currently implemented only for string and bytes",
                    inner.user_facing()
                ),
            );
            return Ty::Error;
        }
        let receiver_ty = Ty::stream(inner.clone());
        let Some(sig) = lookup_builtin_method_sig(&receiver_ty, method) else {
            for arg in args {
                let (expr, sp) = arg.expr();
                self.synthesize(expr, sp);
            }
            self.report_error_with_suggestions(
                TypeErrorKind::UndefinedMethod,
                span,
                format!("no method `{method}` on `Stream<{}>`", inner.user_facing()),
                self.similar_methods(&receiver_ty, method),
            );
            return Ty::Error;
        };
        let resolved_inner = self.subst.resolve(&inner);
        match method {
            // Channel-family naming: .recv() replaced .next() as the fundamental
            // recv surface (routes to the same hew_stream_next runtime symbol).
            // .try_recv() routes to hew_stream_try_next (non-blocking variant).
            // .lines() and .collect() are iterator-style ops removed from the
            // fundamental surface; they will land via trait impls in stdlib work.
            "recv" | "try_recv" | "close" => {
                let Some(c_symbol) = self.require_builtin_runtime_symbol(
                    span,
                    BuiltinNamedType::Stream.canonical_name(),
                    method,
                    crate::stdlib::resolve_stream_method(
                        BuiltinNamedType::Stream.canonical_name(),
                        method,
                        Self::runtime_stream_element_name(&resolved_inner),
                    ),
                ) else {
                    return Ty::Error;
                };
                self.record_runtime_method_call_rewrite(span, c_symbol);
                sig.return_type
            }
            "chunks" => {
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    if let Some(param_ty) = sig.params.first() {
                        self.check_against(expr, sp, param_ty);
                    }
                }
                let Some(c_symbol) = self.require_builtin_runtime_symbol(
                    span,
                    BuiltinNamedType::Stream.canonical_name(),
                    method,
                    crate::stdlib::resolve_stream_method(
                        BuiltinNamedType::Stream.canonical_name(),
                        method,
                        None,
                    ),
                ) else {
                    return Ty::Error;
                };
                self.record_runtime_method_call_rewrite(span, c_symbol);
                sig.return_type
            }
            "take" | "map" | "filter" => {
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    if let Some(param_ty) = sig.params.first() {
                        self.check_against(expr, sp, param_ty);
                    }
                }
                self.record_deferred_method_call_rewrite(span);
                self.record_method_call_receiver_kind(
                    span,
                    MethodCallReceiverKind::StreamInstance {
                        element_kind: Self::stream_receiver_element_kind(&resolved_inner)
                            .to_string(),
                    },
                );
                sig.return_type
            }
            _ => {
                for arg in args {
                    let (expr, sp) = arg.expr();
                    self.synthesize(expr, sp);
                }
                self.report_error_with_suggestions(
                    TypeErrorKind::UndefinedMethod,
                    span,
                    format!("no method `{method}` on `Stream<{}>`", inner.user_facing()),
                    self.similar_methods(&receiver_ty, method),
                );
                Ty::Error
            }
        }
    }

    /// Type-check a method call on `Duplex<S, R>`.
    ///
    /// Wired methods:
    ///   - `.send(msg: S)` → `Result<(), SendError>`  — verifies `S: @send`.
    ///   - `.try_send(msg: S)` → `Result<(), SendError>` — non-blocking; same
    ///     Send bound as `.send()`; returns `SendError::Full` if at capacity.
    ///   - `.recv()` → `Result<R, RecvError>`.
    ///   - `.try_recv()` → `Result<R, RecvError>` — non-blocking; returns
    ///     `RecvError::Empty` if no message is waiting.
    ///   - `.send_half()` → `SendHalf<S>`  — consuming; moves the receiver.
    ///   - `.recv_half()` → `RecvHalf<R>`  — consuming; moves the receiver.
    ///   - `.close()` → `Result<(), CloseError>`  — consuming; moves the receiver.
    ///
    /// Lambda-actor handles type as `Duplex<Msg, Reply>` underneath, so this
    /// function handles both raw-duplex and lambda-actor method calls.
    ///
    /// Unknown methods fall through to a targeted `UndefinedMethod` error.
    #[allow(
        clippy::too_many_arguments,
        clippy::too_many_lines,
        reason = "mirrors check_stream_method arity; all params are load-bearing; \
                  the match arms each encode a distinct method contract"
    )]
    pub(super) fn check_duplex_method(
        &mut self,
        type_args: &[Ty],
        receiver_ty: &Ty,
        receiver: &Spanned<Expr>,
        method: &str,
        args: &[CallArg],
        span: &Span,
    ) -> Ty {
        // Extract S and R from Duplex<S, R>; fabricate fresh vars if malformed.
        let (s_ty, r_ty) = if let [s, r] = type_args {
            (s.clone(), r.clone())
        } else {
            for arg in args {
                let (expr, sp) = arg.expr();
                self.synthesize(expr, sp);
            }
            self.report_error(
                TypeErrorKind::InvalidOperation,
                span,
                "internal error: Duplex type has wrong arity".to_string(),
            );
            return Ty::Error;
        };

        match method {
            "send" => {
                // Check the argument against S.
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    let ty = self.check_against(expr, sp, &s_ty);
                    // Enforce Send bound: the payload must cross thread boundaries.
                    let resolved = self.subst.resolve(&ty);
                    self.enforce_actor_boundary_send(expr, sp, span, &resolved);
                } else {
                    self.report_error(
                        TypeErrorKind::ArityMismatch,
                        span,
                        "Duplex::send expects one argument (the message)".to_string(),
                    );
                }
                // Synthesize extra args for recovery diagnostics.
                for arg in args.iter().skip(1) {
                    let (expr, sp) = arg.expr();
                    self.synthesize(expr, sp);
                }
                self.record_runtime_method_call_rewrite(span, "hew_duplex_send");
                // Return type depends on reply direction, mirroring call-syntax dispatch:
                //   tell-shaped (R = ())  → Result<(), SendError>
                //   ask-shaped  (R = R)   → Result<R, AskError>
                let resolved_r = self.subst.resolve(&r_ty);
                if matches!(resolved_r, Ty::Unit) {
                    Ty::result(Ty::Unit, Ty::send_error())
                } else {
                    Ty::result(resolved_r, Ty::ask_error())
                }
            }
            "try_send" => {
                // Non-blocking send: same argument and Send-bound check as
                // `.send()`, but routes to hew_duplex_try_send which returns
                // SendError::Full instead of blocking when at capacity.
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    let ty = self.check_against(expr, sp, &s_ty);
                    let resolved = self.subst.resolve(&ty);
                    self.enforce_actor_boundary_send(expr, sp, span, &resolved);
                } else {
                    self.report_error(
                        TypeErrorKind::ArityMismatch,
                        span,
                        "Duplex::try_send expects one argument (the message)".to_string(),
                    );
                }
                for arg in args.iter().skip(1) {
                    let (expr, sp) = arg.expr();
                    self.synthesize(expr, sp);
                }
                self.record_runtime_method_call_rewrite(span, "hew_duplex_try_send");
                Ty::result(Ty::Unit, Ty::send_error())
            }
            "recv" => {
                // No arguments expected.
                for arg in args {
                    let (expr, sp) = arg.expr();
                    self.synthesize(expr, sp);
                }
                self.record_runtime_method_call_rewrite(span, "hew_duplex_recv");
                let resolved_r = self.subst.resolve(&r_ty);
                Ty::result(resolved_r, Ty::recv_error())
            }
            "try_recv" => {
                // Non-blocking recv: returns RecvError::Empty instead of
                // blocking when no message is waiting.
                for arg in args {
                    let (expr, sp) = arg.expr();
                    self.synthesize(expr, sp);
                }
                self.record_runtime_method_call_rewrite(span, "hew_duplex_try_recv");
                let resolved_r = self.subst.resolve(&r_ty);
                Ty::result(resolved_r, Ty::recv_error())
            }
            "send_half" => {
                // No arguments expected.
                for arg in args {
                    let (expr, sp) = arg.expr();
                    self.synthesize(expr, sp);
                }
                self.record_runtime_method_call_rewrite(span, "hew_duplex_send_half");
                // Consuming: the Duplex<S, R> binding is moved.
                self.method_call_consumes_receiver
                    .insert(SpanKey::from(span));
                let resolved_recv = self.subst.resolve(receiver_ty);
                self.mark_expr_moved_if_non_copy(&receiver.0, &receiver.1, &resolved_recv);
                let resolved_s = self.subst.resolve(&s_ty);
                Ty::send_half(resolved_s)
            }
            "recv_half" => {
                // No arguments expected.
                for arg in args {
                    let (expr, sp) = arg.expr();
                    self.synthesize(expr, sp);
                }
                self.record_runtime_method_call_rewrite(span, "hew_duplex_recv_half");
                // Consuming: the Duplex<S, R> binding is moved.
                self.method_call_consumes_receiver
                    .insert(SpanKey::from(span));
                let resolved_recv = self.subst.resolve(receiver_ty);
                self.mark_expr_moved_if_non_copy(&receiver.0, &receiver.1, &resolved_recv);
                let resolved_r = self.subst.resolve(&r_ty);
                Ty::recv_half(resolved_r)
            }
            "close" => {
                // No arguments expected.
                for arg in args {
                    let (expr, sp) = arg.expr();
                    self.synthesize(expr, sp);
                }
                self.record_runtime_method_call_rewrite(span, "hew_duplex_close");
                // Consuming: the Duplex<S, R> binding is moved.
                self.method_call_consumes_receiver
                    .insert(SpanKey::from(span));
                let resolved_recv = self.subst.resolve(receiver_ty);
                self.mark_expr_moved_if_non_copy(&receiver.0, &receiver.1, &resolved_recv);
                Ty::result(Ty::Unit, Ty::duplex_close_error())
            }
            _ => {
                // Synthesize args for error recovery.
                for arg in args {
                    let (expr, sp) = arg.expr();
                    self.synthesize(expr, sp);
                }
                self.report_error(
                    TypeErrorKind::UndefinedMethod,
                    span,
                    format!(
                        "no method `{method}` on `{}`; \
                         supported methods: \
                         send / try_send / recv / try_recv / \
                         send_half / recv_half / close",
                        receiver_ty.user_facing()
                    ),
                );
                Ty::Error
            }
        }
    }

    /// Type-check a method call on `SendHalf<S>`.
    ///
    /// Wired methods:
    ///   - `.send(msg: S)` → `Result<(), SendError>`  — verifies `S: @send`.
    ///   - `.try_send(msg: S)` → `Result<(), SendError>` — non-blocking variant;
    ///     returns `SendError::Full` if at capacity.
    ///   - `.close()` → `Result<(), CloseError>`  — consuming; moves the receiver.
    ///
    /// `.recv()` / `.try_recv()` are rejected with targeted `UndefinedMethod` diagnostics.
    pub(super) fn check_send_half_method(
        &mut self,
        type_args: &[Ty],
        receiver: &Spanned<Expr>,
        method: &str,
        args: &[CallArg],
        span: &Span,
    ) -> Ty {
        let s_ty = type_args
            .first()
            .cloned()
            .unwrap_or_else(|| Ty::Var(TypeVar::fresh()));

        let receiver_ty = Ty::send_half(self.subst.resolve(&s_ty));

        match method {
            "send" => {
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    let ty = self.check_against(expr, sp, &s_ty);
                    let resolved = self.subst.resolve(&ty);
                    self.enforce_actor_boundary_send(expr, sp, span, &resolved);
                } else {
                    self.report_error(
                        TypeErrorKind::ArityMismatch,
                        span,
                        "SendHalf::send expects one argument (the message)".to_string(),
                    );
                }
                for arg in args.iter().skip(1) {
                    let (expr, sp) = arg.expr();
                    self.synthesize(expr, sp);
                }
                self.record_runtime_method_call_rewrite(span, "hew_send_half_send");
                Ty::result(Ty::Unit, Ty::send_error())
            }
            "try_send" => {
                // Non-blocking: same Send bound as .send(); routes to
                // hew_send_half_try_send which returns SendError::Full at capacity.
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    let ty = self.check_against(expr, sp, &s_ty);
                    let resolved = self.subst.resolve(&ty);
                    self.enforce_actor_boundary_send(expr, sp, span, &resolved);
                } else {
                    self.report_error(
                        TypeErrorKind::ArityMismatch,
                        span,
                        "SendHalf::try_send expects one argument (the message)".to_string(),
                    );
                }
                for arg in args.iter().skip(1) {
                    let (expr, sp) = arg.expr();
                    self.synthesize(expr, sp);
                }
                self.record_runtime_method_call_rewrite(span, "hew_send_half_try_send");
                Ty::result(Ty::Unit, Ty::send_error())
            }
            "close" => {
                for arg in args {
                    let (expr, sp) = arg.expr();
                    self.synthesize(expr, sp);
                }
                self.record_runtime_method_call_rewrite(span, "hew_duplex_close_half");
                // Consuming: the SendHalf<S> binding is moved.
                self.method_call_consumes_receiver
                    .insert(SpanKey::from(span));
                let resolved_recv = self.subst.resolve(&receiver_ty);
                self.mark_expr_moved_if_non_copy(&receiver.0, &receiver.1, &resolved_recv);
                Ty::result(Ty::Unit, Ty::duplex_close_error())
            }
            _ => {
                for arg in args {
                    let (expr, sp) = arg.expr();
                    self.synthesize(expr, sp);
                }
                self.report_error(
                    TypeErrorKind::UndefinedMethod,
                    span,
                    format!(
                        "no method `{method}` on `{}`; \
                         `SendHalf` only supports `.send()`, `.try_send()`, and `.close()`",
                        receiver_ty.user_facing()
                    ),
                );
                Ty::Error
            }
        }
    }

    /// Type-check a method call on `RecvHalf<R>`.
    ///
    /// Wired methods:
    ///   - `.recv()` → `Result<R, RecvError>`.
    ///   - `.try_recv()` → `Result<R, RecvError>` — non-blocking; returns
    ///     `RecvError::Empty` if no message is waiting.
    ///   - `.close()` → `Result<(), CloseError>`  — consuming; moves the receiver.
    ///
    /// `.send()` / `.try_send()` are rejected with targeted `UndefinedMethod` diagnostics.
    pub(super) fn check_recv_half_method(
        &mut self,
        type_args: &[Ty],
        receiver: &Spanned<Expr>,
        method: &str,
        args: &[CallArg],
        span: &Span,
    ) -> Ty {
        let r_ty = type_args
            .first()
            .cloned()
            .unwrap_or_else(|| Ty::Var(TypeVar::fresh()));

        let receiver_ty = Ty::recv_half(self.subst.resolve(&r_ty));

        match method {
            "recv" => {
                for arg in args {
                    let (expr, sp) = arg.expr();
                    self.synthesize(expr, sp);
                }
                self.record_runtime_method_call_rewrite(span, "hew_recv_half_recv");
                let resolved_r = self.subst.resolve(&r_ty);
                Ty::result(resolved_r, Ty::recv_error())
            }
            "try_recv" => {
                // Non-blocking: returns RecvError::Empty instead of blocking
                // when no message is waiting.
                for arg in args {
                    let (expr, sp) = arg.expr();
                    self.synthesize(expr, sp);
                }
                self.record_runtime_method_call_rewrite(span, "hew_recv_half_try_recv");
                let resolved_r = self.subst.resolve(&r_ty);
                Ty::result(resolved_r, Ty::recv_error())
            }
            "close" => {
                for arg in args {
                    let (expr, sp) = arg.expr();
                    self.synthesize(expr, sp);
                }
                self.record_runtime_method_call_rewrite(span, "hew_duplex_close_half");
                // Consuming: the RecvHalf<R> binding is moved.
                self.method_call_consumes_receiver
                    .insert(SpanKey::from(span));
                let resolved_recv = self.subst.resolve(&receiver_ty);
                self.mark_expr_moved_if_non_copy(&receiver.0, &receiver.1, &resolved_recv);
                Ty::result(Ty::Unit, Ty::duplex_close_error())
            }
            _ => {
                for arg in args {
                    let (expr, sp) = arg.expr();
                    self.synthesize(expr, sp);
                }
                self.report_error(
                    TypeErrorKind::UndefinedMethod,
                    span,
                    format!(
                        "no method `{method}` on `{}`; \
                         `RecvHalf` only supports `.recv()`, `.try_recv()`, and `.close()`",
                        receiver_ty.user_facing()
                    ),
                );
                Ty::Error
            }
        }
    }

    /// Map a `string` method name to the catalog FFI symbol that backs it
    /// at runtime, or `None` when no direct FFI rewrite is wired (the call
    /// will then resolve through other catalog/HIR mechanisms or surface a
    /// downstream error if unsupported).
    fn string_method_runtime_symbol(method: &str) -> Option<&'static str> {
        Some(match method {
            "len" => "len_str",
            "contains" => "contains_str",
            "starts_with" => "starts_with_str",
            "ends_with" => "ends_with_str",
            "is_empty" => "is_empty_str",
            "is_digit" => "is_digit_str",
            "is_alpha" => "is_alpha_str",
            "is_alphanumeric" => "is_alphanumeric_str",
            "trim" => "trim_str",
            "to_uppercase" | "to_upper" => "to_uppercase_str",
            "to_lowercase" | "to_lower" => "to_lowercase_str",
            "clone" => "clone_str",
            "replace" => "replace_str",
            "split" => "split_str",
            "lines" => "lines_str",
            "find" => "find_str",
            "index_of" => "index_of_str",
            "slice" => "slice_str",
            "repeat" => "repeat_str",
            "char_at" => "char_at_str",
            "chars" => "chars_str",
            _ => return None,
        })
    }

    /// Resolve a method call on `Ty::String` through the `impl string` block
    /// declared in `std/builtins.hew` when the method is a built-in inherent
    /// (one with a known runtime FFI symbol).  Anything else — including
    /// user `impl MyTrait for string` dispatch — falls back to the legacy
    /// path so primitive-trait-impl metadata continues to be recorded for
    /// codegen.
    pub(super) fn dispatch_string_method(
        &mut self,
        method: &str,
        args: &[CallArg],
        span: &Span,
    ) -> Ty {
        if let Some(c_symbol) = Self::string_method_runtime_symbol(method) {
            if let Some(sig) = self.lookup_named_method_sig("string", &[], method) {
                let applied = self.apply_instantiated_call_signature(
                    &sig,
                    None,
                    args,
                    span,
                    SignatureArgApplication::PositionalOnly {
                        arity_context: format!("method '{method}'"),
                    },
                    true,
                );
                self.record_runtime_method_call_rewrite(span, c_symbol);
                return applied.return_type;
            }
        }
        self.check_string_method(method, args, span)
    }

    #[allow(
        clippy::too_many_lines,
        reason = "String has many methods to type-check"
    )]
    pub(super) fn check_string_method(
        &mut self,
        method: &str,
        args: &[CallArg],
        span: &Span,
    ) -> Ty {
        match method {
            "len" => {
                self.record_runtime_method_call_rewrite(span, "len_str");
                Ty::I64
            }
            "contains" => {
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    self.check_against(expr, sp, &Ty::String);
                }
                self.record_runtime_method_call_rewrite(span, "contains_str");
                Ty::Bool
            }
            "starts_with" => {
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    self.check_against(expr, sp, &Ty::String);
                }
                self.record_runtime_method_call_rewrite(span, "starts_with_str");
                Ty::Bool
            }
            "ends_with" => {
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    self.check_against(expr, sp, &Ty::String);
                }
                self.record_runtime_method_call_rewrite(span, "ends_with_str");
                Ty::Bool
            }
            "is_empty" => {
                self.check_arity(args, 0, "`String::is_empty`", span);
                self.record_runtime_method_call_rewrite(span, "is_empty_str");
                Ty::Bool
            }
            "is_digit" | "is_alpha" | "is_alphanumeric" => {
                self.check_arity(args, 0, &format!("`String::{method}`"), span);
                self.record_runtime_method_call_rewrite(
                    span,
                    match method {
                        "is_digit" => "is_digit_str",
                        "is_alpha" => "is_alpha_str",
                        _ => "is_alphanumeric_str",
                    },
                );
                Ty::Bool
            }
            "to_uppercase" | "to_upper" => {
                self.record_runtime_method_call_rewrite(span, "to_uppercase_str");
                Ty::String
            }
            "to_lowercase" | "to_lower" => {
                self.record_runtime_method_call_rewrite(span, "to_lowercase_str");
                Ty::String
            }
            "trim" => {
                self.record_runtime_method_call_rewrite(span, "trim_str");
                Ty::String
            }
            "clone" => {
                // v0.5 uses the stable clone shim (`hew_string_clone`); when the string ABI
                // grows a header/refcount, revisit per `.tmp/orchestration/sota-string-cow-research-2026-05-23.md`.
                self.record_runtime_method_call_rewrite(span, "clone_str");
                Ty::String
            }
            "replace" => {
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    self.check_against(expr, sp, &Ty::String);
                }
                if let Some(arg) = args.get(1) {
                    let (expr, sp) = arg.expr();
                    self.check_against(expr, sp, &Ty::String);
                }
                self.record_runtime_method_call_rewrite(span, "replace_str");
                Ty::String
            }
            "split" => {
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    self.check_against(expr, sp, &Ty::String);
                }
                self.record_runtime_method_call_rewrite(span, "split_str");
                self.make_vec_type(Ty::String, span)
            }
            "lines" => {
                self.check_arity(args, 0, "`String::lines`", span);
                self.record_runtime_method_call_rewrite(span, "lines_str");
                self.make_vec_type(Ty::String, span)
            }
            "find" | "index_of" => {
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    self.check_against(expr, sp, &Ty::String);
                }
                self.record_runtime_method_call_rewrite(
                    span,
                    if method == "find" {
                        "find_str"
                    } else {
                        "index_of_str"
                    },
                );
                Ty::I64
            }
            "slice" => {
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    self.check_against(expr, sp, &Ty::I64);
                }
                if let Some(arg) = args.get(1) {
                    let (expr, sp) = arg.expr();
                    self.check_against(expr, sp, &Ty::I64);
                }
                self.record_runtime_method_call_rewrite(span, "slice_str");
                Ty::String
            }
            "repeat" => {
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    self.check_against(expr, sp, &Ty::I64);
                }
                self.record_runtime_method_call_rewrite(span, "repeat_str");
                Ty::String
            }
            "char_at" => {
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    self.check_against(expr, sp, &Ty::I64);
                }
                self.record_runtime_method_call_rewrite(span, "char_at_str");
                Ty::I64
            }
            "chars" => {
                self.check_arity(args, 0, "`String::chars`", span);
                self.record_runtime_method_call_rewrite(span, "chars_str");
                self.make_vec_type(Ty::Char, span)
            }
            _ => {
                if let Some(ret_ty) =
                    self.try_dispatch_primitive_trait_method(&Ty::String, method, args, span)
                {
                    return ret_ty;
                }
                for arg in args {
                    let (expr, sp) = arg.expr();
                    self.synthesize(expr, sp);
                }
                self.report_error(
                    TypeErrorKind::UndefinedMethod,
                    span,
                    format!("no method `{method}` on string"),
                );
                Ty::Error
            }
        }
    }

    fn check_hashset_element_arg(&mut self, elem_ty: &Ty, arg: &CallArg) -> bool {
        let (expr, sp) = arg.expr();
        let err_before = self.errors.len();
        let actual = self.check_against(expr, sp, elem_ty);
        if self.errors.len() > err_before || matches!(actual, Ty::Error) {
            return false;
        }

        let err_before = self.errors.len();
        self.expect_type(elem_ty, &actual, sp);
        self.errors.len() == err_before
    }

    #[allow(
        clippy::too_many_lines,
        reason = "HashMap has multiple methods plus ABI-boundary validation"
    )]
    pub(super) fn check_hashmap_method(
        &mut self,
        type_args: &[Ty],
        method: &str,
        args: &[CallArg],
        span: &Span,
    ) -> Ty {
        let key_ty = type_args
            .first()
            .cloned()
            .unwrap_or(Ty::Var(TypeVar::fresh()));
        let val_ty = type_args
            .get(1)
            .cloned()
            .unwrap_or(Ty::Var(TypeVar::fresh()));
        match method {
            "insert" => {
                self.check_arity(args, 2, "`HashMap::insert`", span);
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    self.check_against(expr, sp, &key_ty);
                }
                if let Some(arg) = args.get(1) {
                    let (expr, sp) = arg.expr();
                    self.check_against(expr, sp, &val_ty);
                }
                if !self.validate_hashmap_owned_element_types(&key_ty, &val_ty, span) {
                    return Ty::Error;
                }
                Ty::Unit
            }
            "get" => {
                self.check_arity(args, 1, "`HashMap::get`", span);
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    self.check_against(expr, sp, &key_ty);
                }
                if !self.validate_hashmap_owned_element_types(&key_ty, &val_ty, span) {
                    return Ty::Error;
                }
                Ty::option(val_ty)
            }
            "remove" => {
                self.check_arity(args, 1, "`HashMap::remove`", span);
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    self.check_against(expr, sp, &key_ty);
                }
                if !self.validate_hashmap_owned_element_types(&key_ty, &val_ty, span) {
                    return Ty::Error;
                }
                Ty::Bool
            }
            "contains_key" => {
                self.check_arity(args, 1, "`HashMap::contains_key`", span);
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    self.check_against(expr, sp, &key_ty);
                }
                if !self.validate_hashmap_key_value_types(&key_ty, &val_ty, span) {
                    return Ty::Error;
                }
                Ty::Bool
            }
            "keys" => {
                self.check_arity(args, 0, "`HashMap::keys`", span);
                if !self.validate_hashmap_owned_element_types(&key_ty, &val_ty, span) {
                    return Ty::Error;
                }
                self.make_vec_type(key_ty, span)
            }
            "values" => {
                self.check_arity(args, 0, "`HashMap::values`", span);
                if !self.validate_hashmap_owned_element_types(&key_ty, &val_ty, span) {
                    return Ty::Error;
                }
                self.make_vec_type(val_ty, span)
            }
            "clone" => {
                self.check_arity(args, 0, "`HashMap::clone`", span);
                if !self.validate_hashmap_key_value_types(&key_ty, &val_ty, span) {
                    return Ty::Error;
                }
                Ty::Named {
                    builtin: Some(BuiltinType::HashMap),
                    name: "HashMap".to_string(),
                    args: vec![key_ty.clone(), val_ty.clone()],
                }
            }
            "len" => {
                if !self.validate_hashmap_key_value_types(&key_ty, &val_ty, span) {
                    return Ty::Error;
                }
                Ty::I64
            }
            "is_empty" => {
                if !self.validate_hashmap_key_value_types(&key_ty, &val_ty, span) {
                    return Ty::Error;
                }
                Ty::Bool
            }
            _ => {
                // Receiver kind for impl table lookup: bare `HashMap` (the
                // canonical_primitive_or_builtin_key strips type args).
                let receiver = Ty::Named {
                    builtin: Some(BuiltinType::HashMap),
                    name: "HashMap".to_string(),
                    args: vec![],
                };
                if let Some(ret_ty) =
                    self.try_dispatch_primitive_trait_method(&receiver, method, args, span)
                {
                    return ret_ty;
                }
                for arg in args {
                    let (expr, sp) = arg.expr();
                    self.synthesize(expr, sp);
                }
                self.report_error(
                    TypeErrorKind::UndefinedMethod,
                    span,
                    format!("no method `{method}` on HashMap"),
                );
                Ty::Error
            }
        }
    }

    pub(super) fn check_hashset_method(
        &mut self,
        type_args: &[Ty],
        method: &str,
        args: &[CallArg],
        span: &Span,
    ) -> Ty {
        let elem_ty = type_args
            .first()
            .cloned()
            .unwrap_or(Ty::Var(TypeVar::fresh()));
        match method {
            "insert" => {
                self.check_arity(args, 1, "`HashSet::insert`", span);
                if let Some(arg) = args.first() {
                    if !self.check_hashset_element_arg(&elem_ty, arg) {
                        return Ty::Error;
                    }
                }
                if !self.validate_hashset_owned_element_type(&elem_ty, span) {
                    return Ty::Error;
                }
                self.record_hashset_lowering_fact(span, &elem_ty);
                Ty::Bool
            }
            "contains" | "remove" => {
                self.check_arity(args, 1, &format!("`HashSet::{method}`"), span);
                if let Some(arg) = args.first() {
                    if !self.check_hashset_element_arg(&elem_ty, arg) {
                        return Ty::Error;
                    }
                }
                if !self.validate_hashset_element_type(&elem_ty, span) {
                    return Ty::Error;
                }
                self.record_hashset_lowering_fact(span, &elem_ty);
                Ty::Bool
            }
            "clone" => {
                self.check_arity(args, 0, "`HashSet::clone`", span);
                if !self.validate_hashset_element_type(&elem_ty, span) {
                    return Ty::Error;
                }
                self.record_hashset_lowering_fact(span, &elem_ty);
                Ty::Named {
                    builtin: Some(BuiltinType::HashSet),
                    name: "HashSet".to_string(),
                    args: vec![elem_ty.clone()],
                }
            }
            "len" => {
                if !self.validate_hashset_element_type(&elem_ty, span) {
                    return Ty::Error;
                }
                self.record_hashset_lowering_fact(span, &elem_ty);
                Ty::I64
            }
            "is_empty" => {
                if !self.validate_hashset_element_type(&elem_ty, span) {
                    return Ty::Error;
                }
                self.record_hashset_lowering_fact(span, &elem_ty);
                Ty::Bool
            }
            "clear" => {
                if !self.validate_hashset_element_type(&elem_ty, span) {
                    return Ty::Error;
                }
                self.record_hashset_lowering_fact(span, &elem_ty);
                Ty::Unit
            }
            _ => {
                let receiver = Ty::Named {
                    builtin: Some(BuiltinType::HashSet),
                    name: "HashSet".to_string(),
                    args: vec![],
                };
                if let Some(ret_ty) =
                    self.try_dispatch_primitive_trait_method(&receiver, method, args, span)
                {
                    return ret_ty;
                }
                for arg in args {
                    let (expr, sp) = arg.expr();
                    self.synthesize(expr, sp);
                }
                self.report_error(
                    TypeErrorKind::UndefinedMethod,
                    span,
                    format!("no method `{method}` on HashSet"),
                );
                Ty::Error
            }
        }
    }

    pub(super) fn check_rc_method(
        &mut self,
        type_args: &[Ty],
        method: &str,
        args: &[CallArg],
        span: &Span,
    ) -> Ty {
        let inner_ty = type_args
            .first()
            .cloned()
            .unwrap_or(Ty::Var(TypeVar::fresh()));
        match method {
            // rc.clone() increments the reference count and returns a new Rc<T>
            "clone" => {
                self.check_arity(args, 0, "`Rc::clone`", span);
                Ty::rc(inner_ty)
            }
            // rc.get() copies the inner value out of the Rc.
            // `LoadOp` performs a bitwise copy, which is only sound for `Copy`
            // types (no ownership to duplicate).  For non-Copy `T`, callers
            // share access via `rc.clone()` instead.
            "get" => {
                self.check_arity(args, 0, "`Rc::get`", span);
                if !self
                    .registry
                    .implements_marker(&inner_ty, MarkerTrait::Copy)
                {
                    self.report_error(
                        TypeErrorKind::BoundsNotSatisfied,
                        span,
                        format!(
                            "`Rc::get` requires `T: Copy`; `{}` is not `Copy` — \
                             use `rc.clone()` to share the reference instead",
                            inner_ty.user_facing()
                        ),
                    );
                    return Ty::Error;
                }
                inner_ty
            }
            // rc.strong_count() returns the current reference count as i64
            "strong_count" => {
                self.check_arity(args, 0, "`Rc::strong_count`", span);
                Ty::I64
            }
            _ => {
                for arg in args {
                    let (expr, sp) = arg.expr();
                    self.synthesize(expr, sp);
                }
                self.report_error(
                    TypeErrorKind::UndefinedMethod,
                    span,
                    format!("no method `{method}` on `Rc<{}>`", inner_ty.user_facing()),
                );
                Ty::Error
            }
        }
    }

    #[allow(clippy::too_many_lines, reason = "Vec has many methods to type-check")]
    pub(super) fn check_vec_method(
        &mut self,
        type_args: &[Ty],
        receiver_ty: &Ty,
        resolved: &Ty,
        method: &str,
        args: &[CallArg],
        span: &Span,
    ) -> Ty {
        let elem_ty = type_args
            .first()
            .cloned()
            .unwrap_or(Ty::Var(TypeVar::fresh()));
        let elem_ty_before = self.subst.resolve(&elem_ty);
        let mut elem_ty_before_visiting = HashSet::new();
        let elem_ty_before_has_structural_array = self
            .vec_element_contains_structural_array(&elem_ty_before, &mut elem_ty_before_visiting);
        let _ = self.validate_vec_element_type(&elem_ty, span);
        let result = match method {
            "push" => {
                self.check_arity(args, 1, "`Vec::push`", span);
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    self.check_against(expr, sp, &elem_ty);
                }
                self.reject_rc_collection_element("Vec", &elem_ty, span);
                let resolved_elem = self.subst.resolve(&elem_ty);
                if let Some(c_symbol) = crate::stdlib::resolve_vec_method("push", &resolved_elem) {
                    self.record_runtime_method_call_rewrite(span, c_symbol);
                }
                Ty::Unit
            }
            "pop" => {
                self.check_arity(args, 0, "`Vec::pop`", span);
                self.reject_rc_collection_element("Vec", &elem_ty, span);
                let resolved_elem = self.subst.resolve(&elem_ty);
                if let Some(c_symbol) = crate::stdlib::resolve_vec_method("pop", &resolved_elem) {
                    self.record_runtime_method_call_rewrite(span, c_symbol);
                }
                elem_ty.clone()
            }
            "len" => {
                self.record_runtime_method_call_rewrite(span, "len_vec");
                Ty::I64
            }
            "get" | "remove" => {
                self.check_arity(args, 1, &format!("`Vec::{method}`"), span);
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    self.check_against(expr, sp, &Ty::I64);
                }
                self.reject_rc_collection_element("Vec", &elem_ty, span);
                let resolved_elem = self.subst.resolve(&elem_ty);
                if let Some(c_symbol) = crate::stdlib::resolve_vec_method(method, &resolved_elem) {
                    self.record_runtime_method_call_rewrite(span, c_symbol);
                }
                elem_ty.clone()
            }
            "contains" => {
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    self.check_against(expr, sp, &elem_ty);
                }
                let resolved_elem = self.subst.resolve(&elem_ty);
                if let Some(c_symbol) =
                    crate::stdlib::resolve_vec_method("contains", &resolved_elem)
                {
                    self.record_runtime_method_call_rewrite(span, c_symbol);
                }
                Ty::Bool
            }
            "is_empty" => {
                if let Some(c_symbol) = crate::stdlib::resolve_vec_method("is_empty", &elem_ty) {
                    self.record_runtime_method_call_rewrite(span, c_symbol);
                }
                Ty::Bool
            }
            "clear" => {
                self.check_arity(args, 0, "`Vec::clear`", span);
                if let Some(c_symbol) = crate::stdlib::resolve_vec_method("clear", &elem_ty) {
                    self.record_runtime_method_call_rewrite(span, c_symbol);
                }
                Ty::Unit
            }
            "clone" => {
                self.check_arity(args, 0, "`Vec::clone`", span);
                if let Some(c_symbol) = crate::stdlib::resolve_vec_method("clone", &elem_ty) {
                    self.record_runtime_method_call_rewrite(span, c_symbol);
                }
                resolved.clone()
            }
            "set" => {
                if let Some(idx) = args.first() {
                    let (expr, sp) = idx.expr();
                    self.check_against(expr, sp, &Ty::I64);
                }
                if let Some(val) = args.get(1) {
                    let (expr, sp) = val.expr();
                    self.check_against(expr, sp, &elem_ty);
                }
                self.reject_rc_collection_element("Vec", &elem_ty, span);
                let resolved_elem = self.subst.resolve(&elem_ty);
                if let Some(c_symbol) = crate::stdlib::resolve_vec_method("set", &resolved_elem) {
                    self.record_runtime_method_call_rewrite(span, c_symbol);
                }
                Ty::Unit
            }
            "append" | "extend" => {
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    self.check_against(expr, sp, receiver_ty);
                }
                self.reject_rc_collection_element("Vec", &elem_ty, span);
                if let Some(c_symbol) = crate::stdlib::resolve_vec_method(method, &elem_ty) {
                    self.record_runtime_method_call_rewrite(span, c_symbol);
                }
                Ty::Unit
            }
            "join" => {
                self.check_arity(args, 1, "`Vec::join`", span);
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    self.check_against(expr, sp, &Ty::String);
                }
                if elem_ty == Ty::String {
                    // Register the runtime rewrite for `Vec<string>::join`;
                    // non-string element rejection remains the type gate.
                    self.record_runtime_method_call_rewrite(span, "hew_vec_join_str");
                } else {
                    self.report_error(
                        TypeErrorKind::UndefinedMethod,
                        span,
                        format!(
                            "`Vec::join` is only available on Vec<String>, not Vec<{}>",
                            elem_ty.user_facing()
                        ),
                    );
                }
                Ty::String
            }
            "map" => {
                self.check_arity(args, 1, "`Vec::map`", span);
                self.reject_rc_collection_element("Vec", &elem_ty, span);
                let ret_ty = Ty::Var(TypeVar::fresh());
                let expected_fn = Ty::Function {
                    params: vec![elem_ty.clone()],
                    ret: Box::new(ret_ty.clone()),
                };
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    self.check_against(expr, sp, &expected_fn);
                }
                let resolved_ret = self.subst.resolve(&ret_ty);
                self.reject_rc_collection_element("Vec", &resolved_ret, span);
                self.make_vec_type(resolved_ret, span)
            }
            "filter" => {
                self.check_arity(args, 1, "`Vec::filter`", span);
                self.reject_rc_collection_element("Vec", &elem_ty, span);
                let expected_fn = Ty::Function {
                    params: vec![elem_ty.clone()],
                    ret: Box::new(Ty::Bool),
                };
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    self.check_against(expr, sp, &expected_fn);
                }
                resolved.clone()
            }
            "fold" => {
                self.check_arity(args, 2, "`Vec::fold`", span);
                self.reject_rc_collection_element("Vec", &elem_ty, span);
                let acc_ty = if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    self.synthesize(expr, sp)
                } else {
                    Ty::Var(TypeVar::fresh())
                };
                let expected_fn = Ty::Function {
                    params: vec![acc_ty.clone(), elem_ty.clone()],
                    ret: Box::new(acc_ty.clone()),
                };
                if let Some(arg) = args.get(1) {
                    let (expr, sp) = arg.expr();
                    self.check_against(expr, sp, &expected_fn);
                }
                self.subst.resolve(&acc_ty)
            }
            _ => {
                let receiver = Ty::Named {
                    builtin: Some(BuiltinType::Vec),
                    name: "Vec".to_string(),
                    args: vec![],
                };
                if let Some(ret_ty) =
                    self.try_dispatch_primitive_trait_method(&receiver, method, args, span)
                {
                    return ret_ty;
                }
                for arg in args {
                    let (expr, sp) = arg.expr();
                    self.synthesize(expr, sp);
                }
                self.report_error(
                    TypeErrorKind::UndefinedMethod,
                    span,
                    format!("no method `{method}` on Vec"),
                );
                Ty::Error
            }
        };
        let elem_ty_after = self.subst.resolve(&elem_ty);
        let mut elem_ty_after_visiting = HashSet::new();
        let elem_ty_after_has_structural_array =
            self.vec_element_contains_structural_array(&elem_ty_after, &mut elem_ty_after_visiting);
        if elem_ty_after_has_structural_array && !elem_ty_before_has_structural_array {
            let _ = self.validate_vec_element_type(&elem_ty_after, span);
            return Ty::Error;
        }
        result
    }

    /// Stage A2: dispatch a method call on a primitive or compiler-builtin
    /// generic receiver to a user `impl Trait for <kind>` body via the
    /// `primitive_trait_impls` side table populated in Stage A1.
    ///
    /// Receiver-keyed (NOT trait-name-keyed): the lookup goes through
    /// `lookup_primitive_trait_method`, which keys on the canonical receiver
    /// kind first, so the surviving five magic `dyn Display` callers
    /// (`assert_eq` / `assert_ne` / `to_string` / `len` / `stop`) cannot be
    /// hijacked by trait-name string matching.
    ///
    /// Returns `Some(return_ty)` after applying argument checks and recording
    /// the dispatch metadata for codegen, or `None` when no impl matches and
    /// the caller should emit its own "no method on X" diagnostic so existing
    /// "no method on Vec" / "no method on string" wording survives.
    ///
    /// Limitation: if two distinct traits each define a method of the same
    /// name on the same receiver kind, this returns the first match the
    /// table iteration encounters.  Acceptable today (only `Display` is in
    /// scope per Phase 1 of #1565), but Phase 2 (`Debug`) must add a
    /// disambiguation rule before introducing same-name conflicts.
    fn try_dispatch_primitive_trait_method(
        &mut self,
        resolved_receiver: &Ty,
        method: &str,
        args: &[CallArg],
        span: &Span,
    ) -> Option<Ty> {
        // Default `IntLiteral` / `FloatLiteral` receivers to their canonical
        // numeric kind before the side-table lookup.  Without this,
        // `(42).fmt()` (literal-form receiver, never bound to a typed `let`)
        // would short-circuit on `canonical_primitive_or_builtin_key` returning
        // `None` for the still-polymorphic literal shape and the caller would
        // emit `no method `fmt` on int`, even though `(42_i64).fmt()` and
        // `let x: i64 = 42; x.fmt()` both succeed.  Mirrors the existing
        // defaulting sites at methods.rs:49 / 125 / 129 / 157 / 161 / 202 /
        // 254 / 317 — collapse the literal exactly at the boundary that would
        // otherwise diagnose, never eagerly upstream.
        let defaulted_receiver = resolved_receiver.materialize_literal_defaults();
        let canonical = Checker::canonical_primitive_or_builtin_key(&defaulted_receiver)?;
        let (trait_name, sig) = self.lookup_primitive_trait_method(&defaulted_receiver, method)?;
        let applied_sig = self.apply_instantiated_call_signature(
            &sig,
            None,
            args,
            span,
            SignatureArgApplication::PositionalOnly {
                arity_context: format!("method '{method}'"),
            },
            true,
        );
        self.record_method_call_receiver_kind(
            span,
            MethodCallReceiverKind::PrimitiveTraitImpl {
                trait_name,
                canonical_receiver: canonical,
            },
        );
        Some(applied_sig.return_type)
    }

    /// Stage A3: UFCS form of [`Self::try_dispatch_primitive_trait_method`].
    ///
    /// `Display::fmt(x)` registers `Display::fmt` in `fn_sigs` with the
    /// receiver param stripped (the `Trait::method` key triggers the
    /// is-method branch in `register_fn_sig_with_name`), so the call site
    /// would mis-arity (sig.params=[] vs args=[x]).  This helper detects
    /// the trait-qualified form, synthesizes the first arg as the
    /// receiver, and consults the same side table that powers
    /// receiver-form dispatch.  The first arg is type-checked against the
    /// canonical receiver kind itself; remaining args are type-checked
    /// against the registered sig's params (already receiver-stripped at
    /// registration time).
    ///
    /// Returns `None` when the receiver is not a primitive or builtin
    /// generic, or when no impl exists for the (kind, trait, method)
    /// triple.  The caller falls through to the existing trait-qualified
    /// dispatch in `calls.rs`, which keeps emitting today's diagnostics.
    pub(super) fn try_dispatch_ufcs_primitive_trait_method(
        &mut self,
        trait_name: &str,
        method_name: &str,
        args: &[CallArg],
        span: &Span,
    ) -> Option<Ty> {
        let first_arg = args.first()?;
        // Short-circuit before synthesising first_arg: if trait_name has no
        // primitive impls registered at all, return immediately.  This
        // prevents the fallback trait-qualified path from synthesising
        // first_arg a second time when the helper was never going to handle
        // the dispatch.  Synthesis of first_arg is deferred to after this
        // guard so it only happens when there is a real chance we will own
        // the call.
        let has_primitive_impl = self
            .primitive_trait_impls
            .keys()
            .any(|(_, tn)| tn == trait_name);
        if !has_primitive_impl {
            return None;
        }
        let (first_expr, first_sp) = first_arg.expr();
        // Synthesize the receiver arg's type so we can route to the
        // canonical primitive/builtin-generic key.
        let receiver_ty = self.synthesize(first_expr, first_sp);
        // Default `IntLiteral` / `FloatLiteral` receivers in UFCS form
        // (e.g. `Display::fmt(42)`) for the same reason as the method-form
        // path at try_dispatch_primitive_trait_method: without defaulting,
        // the synthesized receiver_ty is still in literal shape and
        // `canonical_primitive_or_builtin_key` returns `None`, causing the
        // caller to fall through to the trait-qualified path which then
        // mis-arities (the receiver-stripped sig has 0 params vs the 1 arg
        // we just synthesized).
        let resolved_receiver = self
            .subst
            .resolve(&receiver_ty)
            .materialize_literal_defaults();
        let canonical = Checker::canonical_primitive_or_builtin_key(&resolved_receiver)?;
        // Lookup keyed on the canonical receiver kind + trait name +
        // method.  We call into the table directly (not the
        // walk-every-trait helper) because the trait name is known at
        // this call site and there is no ambiguity to resolve.
        let sig = self
            .primitive_trait_impls
            .get(&(canonical.clone(), trait_name.to_string()))
            .and_then(|methods| methods.get(method_name))
            .cloned()?;
        // Do not add an outer check_arity here.  apply_instantiated_call_signature
        // already calls check_arity on trailing_args via PositionalOnly, matching
        // the receiver-form path at try_dispatch_primitive_trait_method.  An
        // outer check on all args (receiver + trailing) would fire a second
        // arity diagnostic for the same call — e.g. Display::fmt(x, extra)
        // would emit both "expected 1 arg" and "expected 0 trailing args".
        // Type-check remaining args against the (receiver-stripped)
        // params using the same machinery as method-form dispatch.
        let trailing_args = &args[1.min(args.len())..];
        let applied = self.apply_instantiated_call_signature(
            &sig,
            None,
            trailing_args,
            span,
            SignatureArgApplication::PositionalOnly {
                arity_context: format!("method '{trait_name}::{method_name}'"),
            },
            true,
        );
        self.record_method_call_receiver_kind(
            span,
            MethodCallReceiverKind::PrimitiveTraitImpl {
                trait_name: trait_name.to_string(),
                canonical_receiver: canonical,
            },
        );
        Some(applied.return_type)
    }

    #[expect(
        clippy::too_many_lines,
        reason = "pattern matching type checker with many variants"
    )]
    pub(super) fn check_method_call(
        &mut self,
        receiver: &Spanned<Expr>,
        method: &str,
        args: &[CallArg],
        span: &Span,
    ) -> Ty {
        // Module-qualified calls: e.g. http.listen(addr) → lookup "http.listen" in fn_sigs
        if let Expr::Identifier(name) = &receiver.0 {
            let receiver_is_binding = self.env.lookup_ref(name).is_some();
            let receiver_is_known_type = self.type_defs.contains_key(name);
            let key = format!("{name}.{method}");
            let looks_like_module_call = !receiver_is_binding
                && !receiver_is_known_type
                && (self.modules.contains(name)
                    || self.module_fn_exports.contains(&key)
                    || self.fn_sigs.contains_key(&key));
            if looks_like_module_call {
                if self.modules.contains(name) {
                    self.used_modules
                        .borrow_mut()
                        .insert(ImportKey::new(self.current_module.clone(), name.clone()));
                }
                // Cross-module enum variant construction: e.g. `fs.IoError::TimedOut(0)`.
                // method contains "::" → treat as a qualified variant constructor rather than a
                // module function. Mirrors the lookup in check_call (calls.rs:407-465).
                if method.contains("::") {
                    let constructor_match = self.lookup_variant_constructor(method);
                    if let Some((type_name, expected_params, type_params)) = constructor_match {
                        let type_param_count = type_params.len();
                        let mut inferred_args = Vec::new();
                        while inferred_args.len() < type_param_count {
                            inferred_args.push(Ty::Var(TypeVar::fresh()));
                        }
                        self.check_arity(args, expected_params.len(), "this function", span);
                        for (i, arg) in args.iter().enumerate() {
                            if let Some(param_ty) = expected_params.get(i) {
                                let (expr, sp) = arg.expr();
                                let mut expected_ty = param_ty.clone();
                                if !type_params.is_empty() {
                                    for (param, replacement) in
                                        type_params.iter().zip(inferred_args.iter())
                                    {
                                        expected_ty =
                                            expected_ty.substitute_named_param(param, replacement);
                                    }
                                }
                                self.check_against(expr, sp, &expected_ty);
                            }
                        }
                        let resolved_args: Vec<Ty> = inferred_args
                            .iter()
                            .map(|ty| self.subst.resolve(ty))
                            .collect();
                        return Ty::normalize_named(type_name, resolved_args);
                    }
                }
                if !self.module_fn_exports.contains(&key) {
                    for arg in args {
                        let (expr, sp) = arg.expr();
                        self.synthesize(expr, sp);
                    }
                    self.report_error(
                        TypeErrorKind::UndefinedMethod,
                        span,
                        format!("no function `{method}` in module `{name}`"),
                    );
                    return Ty::Error;
                }
                self.require_unsafe(&key, span);
                self.reject_if_wasm_native_only_network_module_call(name, span);
                // Native-only stdlib modules are rejected on wasm32 because
                // their runtime implementations are not compiled there.
                if !self.user_modules.contains(name) {
                    match name.as_str() {
                        "stream" => self.reject_wasm_feature(span, WasmUnsupportedFeature::Streams),
                        "http" => {
                            self.reject_wasm_feature(span, WasmUnsupportedFeature::HttpServer);
                        }
                        "net" => {
                            self.reject_wasm_feature(span, WasmUnsupportedFeature::TcpNetworking);
                        }
                        "process" => {
                            self.reject_wasm_feature(
                                span,
                                WasmUnsupportedFeature::ProcessExecution,
                            );
                        }
                        "tls" => self.reject_wasm_feature(span, WasmUnsupportedFeature::Tls),
                        "quic" => self.reject_wasm_feature(span, WasmUnsupportedFeature::Quic),
                        "dns" => self.reject_wasm_feature(span, WasmUnsupportedFeature::Dns),
                        "os" => self.reject_wasm_feature(span, WasmUnsupportedFeature::OsEnv),
                        _ => {}
                    }
                    // Warn-level module-qualified calls with degraded wasm32
                    // semantics (non-cryptographic PRNG fallback).
                    if name == "crypto" && method == "random_bytes" {
                        self.warn_wasm_limitation(span, WasmUnsupportedFeature::CryptoRandom);
                    }
                }
                if let Some(sig) = self.fn_sigs.get(&key).cloned() {
                    if let Some(caller) = &self.current_function {
                        self.call_graph
                            .entry(caller.clone())
                            .or_default()
                            .insert(key.clone());
                    }
                    self.record_module_qualified_stdlib_call_rewrite_if_any(name, method, span);
                    self.record_module_qualified_user_call_rewrite_if_any(name, method, span);
                    let applied_sig = self.apply_instantiated_call_signature(
                        &sig,
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
                    // Channel constructor: inject a shared type variable so
                    // Sender<T> and Receiver<T> from the same `new` call are
                    // linked through unification.
                    if key == "channel.new" {
                        let t = Ty::Var(TypeVar::fresh());
                        return Ty::Tuple(vec![Ty::sender(t.clone()), Ty::receiver(t)]);
                    }
                    return applied_sig.return_type;
                }
                for arg in args {
                    let (expr, sp) = arg.expr();
                    self.synthesize(expr, sp);
                }
                self.report_error(
                    TypeErrorKind::UndefinedMethod,
                    span,
                    format!("no function `{method}` in module `{name}`"),
                );
                return Ty::Error;
            }

            // Static method calls on type names: e.g. Point.from_json(json)
            // Look up "TypeName.method" in fn_sigs (registered by wire types etc.)
            let static_key = format!("{name}.{method}");
            if let Some(sig) = self.fn_sigs.get(&static_key).cloned() {
                self.check_arity(args, sig.params.len(), &format!("`{static_key}`"), span);
                for (i, arg) in args.iter().enumerate() {
                    if let Some(param_ty) = sig.params.get(i) {
                        let (expr, sp) = arg.expr();
                        self.check_against(expr, sp, param_ty);
                    }
                }
                return sig.return_type;
            }
        }

        let receiver_ty = self.synthesize(&receiver.0, &receiver.1);
        let resolved = self.subst.resolve(&receiver_ty);
        self.reject_if_wasm_native_only_handle(&resolved, span);
        self.reject_if_wasm_blocking_semaphore_method(&resolved, method, span);
        if let Ty::Named { name, .. } = &resolved {
            self.warn_if_blocking_handle_method(name, method, span);
        }

        match (&resolved, method) {
            // Vec methods
            (
                Ty::Named {
                    builtin: Some(BuiltinType::Vec),
                    args: type_args,
                    ..
                },
                _,
            ) => self.check_vec_method(type_args, &receiver_ty, &resolved, method, args, span),
            // HashMap methods
            (
                Ty::Named {
                    builtin: Some(BuiltinType::HashMap),
                    args: type_args,
                    ..
                },
                _,
            ) => self.check_hashmap_method(type_args, method, args, span),
            // HashSet methods
            (
                Ty::Named {
                    builtin: Some(BuiltinType::HashSet),
                    args: type_args,
                    ..
                },
                _,
            ) => {
                // Preserve the receiver's original inference vars so a later non-literal insert
                // can refine an earlier `IntLiteral` element before we validate lowerability.
                let original_type_args = match &receiver_ty {
                    Ty::Named {
                        builtin: Some(BuiltinType::HashSet),
                        args,
                        ..
                    } => args.as_slice(),
                    _ => type_args,
                };
                self.check_hashset_method(original_type_args, method, args, span)
            }
            // Rc<T> methods
            (
                Ty::Named {
                    builtin: Some(BuiltinType::Rc),
                    args: type_args,
                    ..
                },
                _,
            ) => self.check_rc_method(type_args, method, args, span),
            // bytes methods (ref-counted byte buffer)
            (Ty::Bytes, _) => match method {
                "push" => {
                    if let Some(arg) = args.first() {
                        let (expr, sp) = arg.expr();
                        self.check_against(expr, sp, &Ty::I32);
                    }
                    Ty::Unit
                }
                "pop" => Ty::I32,
                "len" => Ty::I64,
                "get" | "remove" => {
                    if let Some(idx) = args.first() {
                        let (expr, sp) = idx.expr();
                        self.check_against(expr, sp, &Ty::I64);
                    }
                    Ty::I32
                }
                "set" => {
                    if let Some(idx) = args.first() {
                        let (expr, sp) = idx.expr();
                        self.check_against(expr, sp, &Ty::I64);
                    }
                    if let Some(val) = args.get(1) {
                        let (expr, sp) = val.expr();
                        self.check_against(expr, sp, &Ty::I32);
                    }
                    Ty::Unit
                }
                "is_empty" => Ty::Bool,
                "clear" => Ty::Unit,
                "contains" => {
                    if let Some(arg) = args.first() {
                        let (expr, sp) = arg.expr();
                        self.check_against(expr, sp, &Ty::I32);
                    }
                    Ty::Bool
                }
                "to_string" => Ty::String,
                "append" | "extend" => {
                    if let Some(arg) = args.first() {
                        let (expr, sp) = arg.expr();
                        self.check_against(expr, sp, &Ty::Bytes);
                    }
                    Ty::Unit
                }
                _ => {
                    if let Some(ret_ty) =
                        self.try_dispatch_primitive_trait_method(&Ty::Bytes, method, args, span)
                    {
                        return ret_ty;
                    }
                    for arg in args {
                        let (expr, sp) = arg.expr();
                        self.synthesize(expr, sp);
                    }
                    self.report_error(
                        TypeErrorKind::UndefinedMethod,
                        span,
                        format!("no method `{method}` on `bytes`"),
                    );
                    Ty::Error
                }
            },
            // Duration methods are declared in `std/builtins.hew` with
            // monomorphic `#[extern_symbol]` annotations.
            (Ty::Duration, _) => {
                if let Some(ret_ty) = self.dispatch_monomorphic_extern_symbol_method(
                    "duration",
                    &[],
                    method,
                    args,
                    span,
                ) {
                    return ret_ty;
                }
                for arg in args {
                    let (expr, sp) = arg.expr();
                    self.synthesize(expr, sp);
                }
                self.report_error(
                    TypeErrorKind::UndefinedMethod,
                    span,
                    format!("no method `{method}` on `duration`"),
                );
                Ty::Error
            }
            // Infallible width-widening: `.to_<W>()` — same signedness, strictly wider.
            //
            // Rule (B-1c): For integer sources, only same-sign strictly-wider fixed-width
            // targets are admitted. Same-width, narrowing, cross-sign, and platform-sized
            // (isize/usize) sources must use `.try_to_<W>()` instead.
            //
            // Float sources retain the legacy permissive behaviour: any numeric source
            // may call `.to_f32()`/`.to_f64()`.  Float-to-integer is also unchanged.
            //
            // Admitted pairs:
            //   signed:   i8→{i16,i32,i64}, i16→{i32,i64}, i32→{i64}
            //   unsigned: u8→{u16,u32,u64}, u16→{u32,u64}, u32→{u64}
            //   float:    any numeric → f32/f64  (legacy, unchanged)
            //   to_f*:    any numeric source     (legacy, unchanged)
            (resolved, method) if resolved.is_numeric() && method.starts_with("to_") => {
                // Resolve target type from method name.
                let target_opt: Option<Ty> = match method {
                    "to_i8" => Some(Ty::I8),
                    "to_i16" => Some(Ty::I16),
                    "to_i32" => Some(Ty::I32),
                    "to_i64" => Some(Ty::I64),
                    "to_isize" => Some(Ty::Isize),
                    "to_u8" => Some(Ty::U8),
                    "to_u16" => Some(Ty::U16),
                    "to_u32" => Some(Ty::U32),
                    "to_u64" => Some(Ty::U64),
                    "to_usize" => Some(Ty::Usize),
                    "to_f32" => Some(Ty::F32),
                    "to_f64" => Some(Ty::F64),
                    _ => None,
                };
                let Some(target) = target_opt else {
                    for arg in args {
                        let (expr, sp) = arg.expr();
                        self.synthesize(expr, sp);
                    }
                    self.report_error(
                        TypeErrorKind::UndefinedMethod,
                        span,
                        format!(
                            "no conversion method `{method}` on `{}`",
                            resolved.user_facing()
                        ),
                    );
                    return Ty::Error;
                };
                // Float targets: always admitted regardless of source width.
                if matches!(target, Ty::F32 | Ty::F64) {
                    return target;
                }
                // Float source converting to integer: always admitted (legacy).
                if resolved.is_float() {
                    return target;
                }
                // Integer-to-integer: enforce strict widening rules.
                // Reject platform-sized source (isize/usize have no fixed width).
                if matches!(resolved, Ty::Isize | Ty::Usize) {
                    self.report_error(
                        TypeErrorKind::UndefinedMethod,
                        span,
                        format!(
                            "`{}` is platform-sized; use `.try_to_{}()` for width conversion",
                            resolved.user_facing(),
                            target.user_facing()
                        ),
                    );
                    return Ty::Error;
                }
                // Reject platform-sized target (to_isize/to_usize on integer source).
                if matches!(target, Ty::Isize | Ty::Usize) {
                    self.report_error(
                        TypeErrorKind::UndefinedMethod,
                        span,
                        format!(
                            "`.{}()` targets a platform-sized type; use `.try_to_{}()` \
                             for width conversion",
                            method,
                            target.user_facing()
                        ),
                    );
                    return Ty::Error;
                }
                // Both are fixed-width integers. Check sign and width.
                let src_unsigned = resolved.is_unsigned();
                let tgt_unsigned = target.is_unsigned();
                if src_unsigned != tgt_unsigned {
                    // Cross-sign: must use try_to_*.
                    self.report_error(
                        TypeErrorKind::UndefinedMethod,
                        span,
                        format!(
                            "`.{}()` crosses integer signedness; use `.try_to_{}()` for \
                             cross-sign conversion",
                            method,
                            target.user_facing()
                        ),
                    );
                    return Ty::Error;
                }
                let src_bits = resolved.integer_bit_width().unwrap(); // non-None: fixed-width checked above
                let tgt_bits = target.integer_bit_width().unwrap();
                if src_bits >= tgt_bits {
                    // Same width or narrowing: must use try_to_*.
                    let hint = if src_bits == tgt_bits {
                        format!(
                            "source and target are the same width ({src_bits}); no conversion needed"
                        )
                    } else {
                        let tgt_name = target.user_facing();
                        format!("use `.try_to_{tgt_name}()` for narrowing conversions")
                    };
                    self.report_error(
                        TypeErrorKind::UndefinedMethod,
                        span,
                        format!("`.{method}()` is not an infallible widening; {hint}"),
                    );
                    return Ty::Error;
                }
                // Admitted: same sign, strictly wider fixed-width target.
                target
            }
            // Fallible narrowing / cross-sign: `.try_to_<W>() -> Result<W, NarrowError>`.
            //
            // Admitted for all integer-to-integer pairs that are NOT covered by the
            // infallible `.to_<W>()` family: narrowing, cross-sign, isize/usize source
            // or target.  Also admitted for same-width (caller may not know widths).
            (resolved, method) if resolved.is_integer() && method.starts_with("try_to_") => {
                let suffix = &method["try_to_".len()..];
                let target_opt: Option<Ty> = match suffix {
                    "i8" => Some(Ty::I8),
                    "i16" => Some(Ty::I16),
                    "i32" => Some(Ty::I32),
                    "i64" => Some(Ty::I64),
                    "isize" => Some(Ty::Isize),
                    "u8" => Some(Ty::U8),
                    "u16" => Some(Ty::U16),
                    "u32" => Some(Ty::U32),
                    "u64" => Some(Ty::U64),
                    "usize" => Some(Ty::Usize),
                    _ => None,
                };
                if let Some(target) = target_opt {
                    Ty::result(target, Ty::narrow_error())
                } else {
                    for arg in args {
                        let (expr, sp) = arg.expr();
                        self.synthesize(expr, sp);
                    }
                    let receiver_name = resolved.user_facing();
                    self.report_error(
                        TypeErrorKind::UndefinedMethod,
                        span,
                        format!(
                            "no method `{method}` on `{receiver_name}`; supported targets: \
                             i8, i16, i32, i64, isize, u8, u16, u32, u64, usize",
                        ),
                    );
                    Ty::Error
                }
            }
            // Explicit-wrap width reinterpretation: `.wrapping_as_<W>() -> W`.
            //
            // Admitted for all integer-to-integer pairs (any width, any sign).
            // Truncates / sign-extends / zero-extends bits per LLVM trunc/sext/zext.
            // MIR lowering is a follow-up slice; a NotYetImplemented stub is emitted.
            //
            // Guard: `wrapping_as_` must be checked BEFORE the arithmetic `wrapping_*`
            // arm so the suffix "as_<W>" does not fall through to the op-name matcher.
            (resolved, method) if resolved.is_integer() && method.starts_with("wrapping_as_") => {
                let suffix = &method["wrapping_as_".len()..];
                let target_opt: Option<Ty> = match suffix {
                    "i8" => Some(Ty::I8),
                    "i16" => Some(Ty::I16),
                    "i32" => Some(Ty::I32),
                    "i64" => Some(Ty::I64),
                    "isize" => Some(Ty::Isize),
                    "u8" => Some(Ty::U8),
                    "u16" => Some(Ty::U16),
                    "u32" => Some(Ty::U32),
                    "u64" => Some(Ty::U64),
                    "usize" => Some(Ty::Usize),
                    _ => None,
                };
                if let Some(target) = target_opt {
                    target
                } else {
                    for arg in args {
                        let (expr, sp) = arg.expr();
                        self.synthesize(expr, sp);
                    }
                    let receiver_name = resolved.user_facing();
                    self.report_error(
                        TypeErrorKind::UndefinedMethod,
                        span,
                        format!(
                            "no method `{method}` on `{receiver_name}`; supported targets: \
                             i8, i16, i32, i64, isize, u8, u16, u32, u64, usize",
                        ),
                    );
                    Ty::Error
                }
            }
            // Saturating-clamp width conversion: `.saturating_as_<W>() -> W`.
            //
            // Admitted for all integer-to-integer pairs (any width, any sign).
            // Returns W::MAX / W::MIN on overflow.
            // MIR lowering is a follow-up slice; a NotYetImplemented stub is emitted.
            //
            // Guard: `saturating_as_` must be checked BEFORE the arithmetic `saturating_*`
            // arm so the suffix "as_<W>" does not fall through to the op-name matcher.
            (resolved, method) if resolved.is_integer() && method.starts_with("saturating_as_") => {
                let suffix = &method["saturating_as_".len()..];
                let target_opt: Option<Ty> = match suffix {
                    "i8" => Some(Ty::I8),
                    "i16" => Some(Ty::I16),
                    "i32" => Some(Ty::I32),
                    "i64" => Some(Ty::I64),
                    "isize" => Some(Ty::Isize),
                    "u8" => Some(Ty::U8),
                    "u16" => Some(Ty::U16),
                    "u32" => Some(Ty::U32),
                    "u64" => Some(Ty::U64),
                    "usize" => Some(Ty::Usize),
                    _ => None,
                };
                if let Some(target) = target_opt {
                    target
                } else {
                    for arg in args {
                        let (expr, sp) = arg.expr();
                        self.synthesize(expr, sp);
                    }
                    let receiver_name = resolved.user_facing();
                    self.report_error(
                        TypeErrorKind::UndefinedMethod,
                        span,
                        format!(
                            "no method `{method}` on `{receiver_name}`; supported targets: \
                             i8, i16, i32, i64, isize, u8, u16, u32, u64, usize",
                        ),
                    );
                    Ty::Error
                }
            }
            // Numeric opt-out arithmetic methods: .wrapping_*, .checked_*, .saturating_*
            // for every integer width. Floats are excluded (is_integer() ≠ is_numeric()).
            // Only add/sub/mul are in scope here; div/mod/shift are separate slices.
            // Wrapping variants map to non-trapping MIR ops; checked variants return
            // Option<W>; saturating variants clamp to MAX/MIN (codegen slice pending).
            //
            // Note: `.wrapping_as_<W>` and `.saturating_as_<W>` (width-conversion family)
            // are handled by the arms above; those arms must appear first so that the
            // `_as_` suffix does not reach this arm's op-name matcher.
            (resolved, method)
                if resolved.is_integer()
                    && (method.starts_with("wrapping_")
                        || method.starts_with("checked_")
                        || method.starts_with("saturating_")) =>
            {
                let is_wrapping = method.starts_with("wrapping_");
                let is_checked = method.starts_with("checked_");
                let family = if is_wrapping {
                    NumericMethodFamily::Wrapping
                } else if is_checked {
                    NumericMethodFamily::Checked
                } else {
                    NumericMethodFamily::Saturating
                };
                let op_name = if is_wrapping {
                    &method["wrapping_".len()..]
                } else if is_checked {
                    &method["checked_".len()..]
                } else {
                    &method["saturating_".len()..]
                };
                match op_name {
                    "add" | "sub" | "mul" => {
                        self.check_arity(args, 1, &format!("`{method}`"), span);
                        if let Some(arg) = args.first() {
                            let (expr, sp) = arg.expr();
                            self.check_against(expr, sp, resolved);
                        }
                        let op = match op_name {
                            "add" => NumericMethodOp::Add,
                            "sub" => NumericMethodOp::Sub,
                            "mul" => NumericMethodOp::Mul,
                            _ => unreachable!("op_name matched add/sub/mul above"),
                        };
                        if let (Some(signedness), Some(width)) = (
                            Self::numeric_method_signedness(resolved),
                            Self::numeric_method_width(resolved),
                        ) {
                            let result_ty = if is_checked {
                                Ty::option(resolved.clone())
                            } else {
                                resolved.clone()
                            };
                            let prior = self.numeric_method_lowerings.insert(
                                SpanKey::from(span),
                                NumericMethodLowering {
                                    family,
                                    op,
                                    result_ty: result_ty.clone(),
                                    operand_ty: resolved.clone(),
                                    signedness,
                                    width,
                                },
                            );
                            debug_assert!(
                                prior.is_none(),
                                "duplicate numeric method lowering for span {:?}",
                                SpanKey::from(span)
                            );
                            result_ty
                        } else if is_checked {
                            Ty::option(resolved.clone())
                        } else {
                            resolved.clone()
                        }
                    }
                    _ => {
                        for arg in args {
                            let (expr, sp) = arg.expr();
                            self.synthesize(expr, sp);
                        }
                        self.report_error(
                            TypeErrorKind::UndefinedMethod,
                            span,
                            format!(
                                "no method `{method}` on `{}`; only add, sub, mul are supported \
                                 in this family",
                                resolved.user_facing()
                            ),
                        );
                        Ty::Error
                    }
                }
            }
            // LocalPid<T> methods — first check LocalPid's own impl methods,
            // then fall through to actor receive-fn dispatch on the inner type T.
            //
            // Own methods (e.g. `tell`, `to_remote_via`) are declared in
            // `impl LocalPid<T>` in std/builtins.hew and registered in type_defs /
            // fn_sigs as `"LocalPid::{method}"`.  Actor receive-fn dispatch
            // (e.g. `pid.greet(arg)`) remains the local actor-dispatch path.
            (resolved, _) if resolved.as_local_pid().is_some() => {
                // `.send(payload)` is the legacy fire-and-forget surface; enforce
                // the Send bound on the payload.
                if method == "send" {
                    for arg in args {
                        let (expr, sp) = arg.expr();
                        let ty = self.synthesize(expr, sp);
                        self.enforce_actor_boundary_send(expr, sp, sp, &ty);
                    }
                    return Ty::Unit;
                }
                // Try LocalPid's own methods first.
                if let Ty::Named {
                    args: receiver_args,
                    ..
                } = resolved
                {
                    if let Some(sig) = self.lookup_named_method_sig(
                        crate::BuiltinType::LocalPid.canonical_name(),
                        receiver_args,
                        method,
                    ) {
                        let applied_sig = self.apply_instantiated_call_signature(
                            &sig,
                            None,
                            args,
                            span,
                            SignatureArgApplication::PositionalOnly {
                                arity_context: format!("method '{method}'"),
                            },
                            true,
                        );
                        if method == "tell" {
                            self.enforce_actor_method_send_args(args);
                        }
                        return applied_sig.return_type;
                    }
                }
                // Fall through to actor receive-fn dispatch on the inner type.
                let inner = resolved.as_local_pid().unwrap();
                if let Ty::Named {
                    name: actor_name, ..
                } = inner
                {
                    let method_key = format!("{actor_name}::{method}");
                    if let Some(sig) = self.fn_sigs.get(&method_key).cloned() {
                        for (i, arg) in args.iter().enumerate() {
                            let (expr, sp) = arg.expr();
                            let ty = if let Some(param_ty) = sig.params.get(i) {
                                self.check_against(expr, sp, param_ty)
                            } else {
                                self.synthesize(expr, sp)
                            };
                            self.enforce_actor_boundary_send(expr, sp, sp, &ty);
                        }
                        self.record_method_call_receiver_kind(
                            span,
                            MethodCallReceiverKind::ActorInstance {
                                actor_name: actor_name.clone(),
                            },
                        );
                        self.record_actor_method_dispatch(
                            span,
                            method_key,
                            sig.return_type.clone(),
                        );
                        return sig.return_type;
                    }
                }
                for arg in args {
                    let (expr, sp) = arg.expr();
                    self.synthesize(expr, sp);
                }
                self.report_error_with_suggestions(
                    TypeErrorKind::UndefinedMethod,
                    span,
                    format!("no method `{method}` on `{}`", resolved.user_facing()),
                    self.similar_methods(resolved, method),
                );
                Ty::Error
            }
            // RemotePid<T> methods — dispatch to RemotePid's own impl methods.
            //
            // RemotePid does NOT fall through to actor receive-fn dispatch; it is
            // a distinct remote type that cannot dispatch local actor methods.
            (resolved, _) if resolved.as_remote_pid().is_some() => {
                if let Ty::Named {
                    args: receiver_args,
                    ..
                } = resolved
                {
                    if let Some(sig) = self.lookup_named_method_sig(
                        crate::BuiltinType::RemotePid.canonical_name(),
                        receiver_args,
                        method,
                    ) {
                        let applied_sig = self.apply_instantiated_call_signature(
                            &sig,
                            None,
                            args,
                            span,
                            SignatureArgApplication::PositionalOnly {
                                arity_context: format!("method '{method}'"),
                            },
                            true,
                        );
                        if method == "tell" {
                            self.enforce_actor_method_send_args(args);
                            // S5: real RemotePid<T>::tell lowering. Record a
                            // direct-call rewrite so HIR/MIR lower the call
                            // to `hew_remote_pid_tell`, which codegen
                            // intercepts and lowers to the
                            // `hew_actor_send_by_id` runtime ABI plus a
                            // `Result<(), SendError>` construction. The
                            // catalog entry registers the FFI shape; the
                            // codegen Terminator::Call branch consumes the
                            // resolved receiver + msg arg types from the
                            // checker output (no re-inference in codegen
                            // per the `checker-authority` invariant).
                            self.method_call_rewrites.insert(
                                SpanKey::from(span),
                                MethodCallRewrite::RewriteToFunction {
                                    c_symbol: "hew_remote_pid_tell".to_string(),
                                    elem_ty: None,
                                },
                            );
                        }
                        return applied_sig.return_type;
                    }
                }
                for arg in args {
                    let (expr, sp) = arg.expr();
                    self.synthesize(expr, sp);
                }
                self.report_error_with_suggestions(
                    TypeErrorKind::UndefinedMethod,
                    span,
                    format!("no method `{method}` on `{}`", resolved.user_facing()),
                    self.similar_methods(resolved, method),
                );
                Ty::Error
            }
            // ActorRef methods
            (resolved, _) if resolved.as_actor_ref().is_some() => {
                let inner = resolved.as_actor_ref().unwrap();
                if method == "send" {
                    for arg in args {
                        let (expr, sp) = arg.expr();
                        let ty = self.synthesize(expr, sp);
                        self.enforce_actor_boundary_send(expr, sp, sp, &ty);
                    }
                    Ty::Unit
                } else {
                    // Try to dispatch to the actor's receive methods
                    if let Ty::Named {
                        name: actor_name, ..
                    } = inner
                    {
                        let method_key = format!("{actor_name}::{method}");
                        if let Some(sig) = self.fn_sigs.get(&method_key).cloned() {
                            for (i, arg) in args.iter().enumerate() {
                                let (expr, sp) = arg.expr();
                                let ty = if let Some(param_ty) = sig.params.get(i) {
                                    self.check_against(expr, sp, param_ty)
                                } else {
                                    self.synthesize(expr, sp)
                                };
                                self.enforce_actor_boundary_send(expr, sp, sp, &ty);
                            }
                            self.record_method_call_receiver_kind(
                                span,
                                MethodCallReceiverKind::ActorInstance {
                                    actor_name: actor_name.clone(),
                                },
                            );
                            self.record_actor_method_dispatch(
                                span,
                                method_key,
                                sig.return_type.clone(),
                            );
                            return sig.return_type;
                        }
                    }
                    for arg in args {
                        let (expr, sp) = arg.expr();
                        self.synthesize(expr, sp);
                    }
                    self.report_error_with_suggestions(
                        TypeErrorKind::UndefinedMethod,
                        span,
                        format!("no method `{method}` on `{}`", resolved.user_facing()),
                        self.similar_methods(resolved, method),
                    );
                    Ty::Error
                }
            }
            // Duplex<S, R>: bidirectional channel handle.
            //
            // Methods: .send(msg) / .recv() / .send_half() / .recv_half() / .close()
            //
            // Lambda-actor handles are also typed as `Duplex<S, R>` underneath, so
            // this arm handles both raw-duplex and lambda-actor method calls.
            // Call-syntax `handle(msg)` is canonical for lambda-actor handles;
            // `.send(msg)` is an allowed-secondary surface — both route to the same
            // runtime symbol (`hew_duplex_send`).
            (
                Ty::Named {
                    builtin: Some(BuiltinType::Duplex),
                    args: type_args,
                    ..
                },
                _,
            ) => self.check_duplex_method(type_args, &receiver_ty, receiver, method, args, span),
            // SendHalf<S>: send-direction half of a split Duplex<S, R>.
            //
            // Methods: .send(msg) / .close()
            // Produced by `Duplex<S, R>::send_half()`.
            (
                Ty::Named {
                    builtin: Some(BuiltinType::SendHalf),
                    args: type_args,
                    ..
                },
                _,
            ) => self.check_send_half_method(type_args, receiver, method, args, span),
            // RecvHalf<R>: receive-direction half of a split Duplex<S, R>.
            //
            // Methods: .recv() / .close()
            // Produced by `Duplex<S, R>::recv_half()`.
            (
                Ty::Named {
                    builtin: Some(BuiltinType::RecvHalf),
                    args: type_args,
                    ..
                },
                _,
            ) => self.check_recv_half_method(type_args, receiver, method, args, span),
            // Named types that have built-in methods (Actor<T> from lambda actors)
            (
                Ty::Named {
                    builtin: Some(BuiltinType::Actor),
                    args: type_args,
                    ..
                },
                "send",
            ) => {
                for arg in args {
                    let (expr, sp) = arg.expr();
                    let ty = if let Some(param_ty) = type_args.first() {
                        self.check_against(expr, sp, param_ty)
                    } else {
                        self.synthesize(expr, sp)
                    };
                    self.enforce_actor_boundary_send(expr, sp, sp, &ty);
                }
                Ty::Unit
            }
            // String methods — prefer the `impl string` block declared in
            // std/builtins.hew (registered via register_builtins_hew_impls).
            // If the method exists on that impl, type-check the call against
            // the impl signature and record the runtime FFI rewrite mapping
            // (catalog overload symbol).  Falls back to the legacy hardcoded
            // dispatch in `check_string_method` only when the impl block has
            // no matching method, preserving today's behaviour for methods
            // whose rewrite isn't yet wired through the catalog.
            (Ty::String, _) => self.dispatch_string_method(method, args, span),
            // Generator methods route through the Iterator contract:
            // .next() returns Option<yielded type>.
            (
                Ty::Named {
                    builtin: Some(BuiltinType::Generator | BuiltinType::AsyncGenerator),
                    args: type_args,
                    ..
                },
                "next",
            ) => Ty::option(
                type_args
                    .first()
                    .cloned()
                    .unwrap_or(Ty::Var(TypeVar::fresh())),
            ),
            // Stream<T> methods
            //
            // LIMITATION: Stream element-type validation only triggers here (on
            // method resolution).  A function parameter typed `Stream<MyStruct>`
            // passes typecheck if no stream methods are called on it.  Ideally
            // we would reject unsupported element types in resolve_type_expr when
            // the Stream<T> type is first formed, but that requires propagating
            // the span and restructuring the named-type resolution path.  For
            // now codegen will fail if the type is actually used.
            (
                Ty::Named {
                    builtin: Some(BuiltinType::Stream),
                    args: type_args,
                    ..
                },
                _,
            ) => {
                // Stream<T> methods are not supported on wasm32: the stream
                // runtime module is not compiled for wasm32.
                self.reject_wasm_feature(span, WasmUnsupportedFeature::Streams);
                self.check_stream_method(type_args, method, args, span)
            }
            // Sink<T> methods
            (
                Ty::Named {
                    builtin: Some(BuiltinType::Sink),
                    args: type_args,
                    ..
                },
                _,
            ) => {
                let Some(inner) = self.validate_stream_sink_element_type(
                    type_args,
                    BuiltinNamedType::Sink.canonical_name(),
                    method,
                    span,
                ) else {
                    return Ty::Error;
                };
                if method == "encode" {
                    return self.report_unlowerable_stream_codec_boundary(
                        BuiltinNamedType::Sink.canonical_name(),
                        &inner,
                        method,
                        span,
                    );
                }
                // Gate 2: lowering-capability check.  Only string and bytes have
                // runtime symbols; other Wire-capable types pass gate 1 but cannot
                // be lowered yet.  Emit a user-facing diagnostic rather than the
                // ICE-flavoured "missing runtime rewrite metadata" from
                // require_builtin_runtime_symbol.
                let resolved_inner = self.subst.resolve(&inner);
                if Self::runtime_stream_element_name(&resolved_inner).is_none() {
                    self.report_error(
                        TypeErrorKind::InvalidOperation,
                        span,
                        format!(
                            "`Sink<{}>` is not supported; \
                             runtime lowering is currently implemented only for string and bytes",
                            inner.user_facing()
                        ),
                    );
                    return Ty::Error;
                }
                let receiver_ty = Ty::sink(inner.clone());
                match method {
                    // Channel-family naming: .send() replaced .write() as the
                    // fundamental send surface (routes to the same
                    // hew_sink_write_* runtime symbols). .try_send() routes to
                    // hew_sink_try_write_* (non-blocking variant). .flush() is
                    // removed from the fundamental surface; it may re-surface
                    // via an I/O-sink trait in stdlib work.
                    "send" | "try_send" => {
                        let Some(sig) = self.require_builtin_method_sig(
                            span,
                            &receiver_ty,
                            BuiltinNamedType::Sink.canonical_name(),
                            method,
                        ) else {
                            return Ty::Error;
                        };
                        if let Some(arg) = args.first() {
                            let (expr, sp) = arg.expr();
                            if let Some(param_ty) = sig.params.first() {
                                self.check_against(expr, sp, param_ty);
                            }
                        }
                        let Some(c_symbol) = self.require_builtin_runtime_symbol(
                            span,
                            BuiltinNamedType::Sink.canonical_name(),
                            method,
                            crate::stdlib::resolve_stream_method(
                                BuiltinNamedType::Sink.canonical_name(),
                                method,
                                Self::runtime_stream_element_name(&self.subst.resolve(&inner)),
                            ),
                        ) else {
                            return Ty::Error;
                        };
                        self.record_runtime_method_call_rewrite(span, c_symbol);
                        sig.return_type
                    }
                    "close" => {
                        let Some(c_symbol) = self.require_builtin_runtime_symbol(
                            span,
                            BuiltinNamedType::Sink.canonical_name(),
                            method,
                            crate::stdlib::resolve_stream_method(
                                BuiltinNamedType::Sink.canonical_name(),
                                method,
                                None,
                            ),
                        ) else {
                            return Ty::Error;
                        };
                        self.record_runtime_method_call_rewrite(span, c_symbol);
                        let Some(sig) = self.require_builtin_method_sig(
                            span,
                            &receiver_ty,
                            BuiltinNamedType::Sink.canonical_name(),
                            method,
                        ) else {
                            return Ty::Error;
                        };
                        sig.return_type
                    }
                    _ => {
                        for arg in args {
                            let (expr, sp) = arg.expr();
                            self.synthesize(expr, sp);
                        }
                        self.report_error_with_suggestions(
                            TypeErrorKind::UndefinedMethod,
                            span,
                            format!("no method `{method}` on `{}`", resolved.user_facing()),
                            self.similar_methods(&receiver_ty, method),
                        );
                        Ty::Error
                    }
                }
            }
            // Sender<T> methods
            (
                Ty::Named {
                    builtin: Some(BuiltinType::Sender),
                    args: type_args,
                    ..
                },
                _,
            ) => {
                let inner = type_args
                    .first()
                    .cloned()
                    .unwrap_or(Ty::Var(TypeVar::fresh()));
                let receiver_ty = Ty::sender(inner.clone());
                let resolved_inner = self.subst.resolve(&inner);
                match method {
                    "send" => {
                        let Some(sig) = self.require_builtin_method_sig(
                            span,
                            &receiver_ty,
                            BuiltinNamedType::Sender.canonical_name(),
                            method,
                        ) else {
                            return Ty::Error;
                        };
                        if let Some(arg) = args.first() {
                            let (expr, sp) = arg.expr();
                            if let Some(param_ty) = sig.params.first() {
                                self.check_against(expr, sp, param_ty);
                            }
                        }
                        // Validate after unification so the concrete type is known.
                        let resolved_inner = self.subst.resolve(&inner);
                        if !matches!(resolved_inner, Ty::Var(_) | Ty::String)
                            && !resolved_inner.is_integer()
                        {
                            self.report_error(
                                TypeErrorKind::InvalidOperation,
                                span,
                                format!(
                                    "Channel<{resolved_inner}> is not supported; \
                                     only Channel<string> and Channel<i64> are currently supported"
                                ),
                            );
                            return Ty::Error;
                        }
                        if matches!(resolved_inner, Ty::Var(_)) {
                            // Inner type is still unresolved after argument
                            // unification — the constraint may arrive from the
                            // call-site's surrounding context (e.g.
                            // `let _: () = tx.send(v)` where `v: int` is
                            // declared elsewhere).  Defer the symbol selection
                            // until post-inference drain.
                            self.record_deferred_channel_method_rewrite(
                                span,
                                BuiltinNamedType::Sender.canonical_name(),
                                method,
                                inner.clone(),
                            );
                        } else {
                            let Some(c_symbol) = self.require_builtin_runtime_symbol(
                                span,
                                BuiltinNamedType::Sender.canonical_name(),
                                method,
                                crate::stdlib::resolve_channel_method(
                                    BuiltinNamedType::Sender.canonical_name(),
                                    method,
                                    Some(&resolved_inner),
                                ),
                            ) else {
                                return Ty::Error;
                            };
                            self.record_runtime_method_call_rewrite(span, c_symbol);
                        }
                        sig.return_type
                    }
                    "clone" | "close" => {
                        let Some(c_symbol) = self.require_builtin_runtime_symbol(
                            span,
                            BuiltinNamedType::Sender.canonical_name(),
                            method,
                            crate::stdlib::resolve_channel_method(
                                BuiltinNamedType::Sender.canonical_name(),
                                method,
                                Some(&resolved_inner),
                            ),
                        ) else {
                            return Ty::Error;
                        };
                        self.record_runtime_method_call_rewrite(span, c_symbol);
                        let Some(sig) = self.require_builtin_method_sig(
                            span,
                            &receiver_ty,
                            BuiltinNamedType::Sender.canonical_name(),
                            method,
                        ) else {
                            return Ty::Error;
                        };
                        sig.return_type
                    }
                    _ => {
                        self.check_named_method_fallback(&resolved, method, args, span, "Sender<T>")
                    }
                }
            }
            // Receiver<T> methods
            (
                Ty::Named {
                    builtin: Some(BuiltinType::Receiver),
                    args: type_args,
                    ..
                },
                _,
            ) => {
                let inner = type_args
                    .first()
                    .cloned()
                    .unwrap_or(Ty::Var(TypeVar::fresh()));
                let receiver_ty = Ty::receiver(inner.clone());
                let resolved_inner = self.subst.resolve(&inner);
                if !matches!(resolved_inner, Ty::Var(_) | Ty::String)
                    && !resolved_inner.is_integer()
                {
                    self.report_error(
                        TypeErrorKind::InvalidOperation,
                        span,
                        format!(
                            "Channel<{resolved_inner}> is not supported; \
                             only Channel<string> and Channel<i64> are currently supported"
                        ),
                    );
                    return Ty::Error;
                }
                match method {
                    "recv" => {
                        self.reject_wasm_feature(span, WasmUnsupportedFeature::BlockingChannelRecv);
                        let Some(sig) = self.require_builtin_method_sig(
                            span,
                            &receiver_ty,
                            BuiltinNamedType::Receiver.canonical_name(),
                            method,
                        ) else {
                            return Ty::Error;
                        };
                        self.warn_if_blocking_in_receive_fn("Receiver::recv", span);
                        if matches!(resolved_inner, Ty::Var(_)) {
                            // No argument to unify against — the return-type
                            // constraint (e.g. `let v: int = rx.recv()`) is
                            // applied by the caller *after* this arm returns.
                            // Defer the C-symbol selection until
                            // post-inference drain.
                            self.record_deferred_channel_method_rewrite(
                                span,
                                BuiltinNamedType::Receiver.canonical_name(),
                                method,
                                inner.clone(),
                            );
                        } else {
                            let Some(c_symbol) = self.require_builtin_runtime_symbol(
                                span,
                                BuiltinNamedType::Receiver.canonical_name(),
                                method,
                                crate::stdlib::resolve_channel_method(
                                    BuiltinNamedType::Receiver.canonical_name(),
                                    method,
                                    Some(&resolved_inner),
                                ),
                            ) else {
                                return Ty::Error;
                            };
                            self.record_runtime_method_call_rewrite(span, c_symbol);
                        }
                        sig.return_type
                    }
                    "try_recv" => {
                        if matches!(resolved_inner, Ty::Var(_)) {
                            self.record_deferred_channel_method_rewrite(
                                span,
                                BuiltinNamedType::Receiver.canonical_name(),
                                method,
                                inner.clone(),
                            );
                        } else {
                            let Some(c_symbol) = self.require_builtin_runtime_symbol(
                                span,
                                BuiltinNamedType::Receiver.canonical_name(),
                                method,
                                crate::stdlib::resolve_channel_method(
                                    BuiltinNamedType::Receiver.canonical_name(),
                                    method,
                                    Some(&resolved_inner),
                                ),
                            ) else {
                                return Ty::Error;
                            };
                            self.record_runtime_method_call_rewrite(span, c_symbol);
                        }
                        let Some(sig) = self.require_builtin_method_sig(
                            span,
                            &receiver_ty,
                            BuiltinNamedType::Receiver.canonical_name(),
                            method,
                        ) else {
                            return Ty::Error;
                        };
                        sig.return_type
                    }
                    "close" => {
                        // `close` maps to a single type-independent symbol.
                        let Some(c_symbol) = self.require_builtin_runtime_symbol(
                            span,
                            BuiltinNamedType::Receiver.canonical_name(),
                            method,
                            crate::stdlib::resolve_channel_method(
                                BuiltinNamedType::Receiver.canonical_name(),
                                method,
                                Some(&resolved_inner),
                            ),
                        ) else {
                            return Ty::Error;
                        };
                        self.record_runtime_method_call_rewrite(span, c_symbol);
                        let Some(sig) = self.require_builtin_method_sig(
                            span,
                            &receiver_ty,
                            BuiltinNamedType::Receiver.canonical_name(),
                            method,
                        ) else {
                            return Ty::Error;
                        };
                        sig.return_type
                    }
                    _ => self.check_named_method_fallback(
                        &resolved,
                        method,
                        args,
                        span,
                        "Receiver<T>",
                    ),
                }
            }
            // User-defined struct/actor methods from type_defs
            (
                Ty::Named {
                    name,
                    args: type_args,
                    ..
                },
                _,
            ) => {
                if let Some(sig) = self.lookup_named_method_sig(name, type_args, method) {
                    let applied_sig = self.apply_instantiated_call_signature(
                        &sig,
                        None,
                        args,
                        span,
                        SignatureArgApplication::PositionalOnly {
                            arity_context: format!("method '{method}'"),
                        },
                        true,
                    );
                    self.record_method_call_receiver_kind(
                        span,
                        MethodCallReceiverKind::NamedTypeInstance {
                            type_name: name.clone(),
                        },
                    );
                    // Actor receive-method dispatch on a bare actor-typed
                    // receiver (e.g. `let target: Printer; target.foo(arg)`)
                    // routes here: `lookup_named_method_sig` finds the
                    // signature in `fn_sigs` keyed `{Actor}::{method}`.
                    // Every arg crosses the mailbox boundary and must be
                    // routed through `enforce_actor_boundary_send` so the
                    // codegen consumer (fail-closed on missing entries)
                    // sees an alias-vs-copy decision per arg.
                    if self
                        .type_defs
                        .get(name)
                        .is_some_and(|td| td.kind == TypeDefKind::Actor)
                        && self
                            .actor_receive_methods
                            .contains(&format!("{name}::{method}"))
                    {
                        self.enforce_actor_method_send_args(args);
                    }
                    // Machine method dispatch: `.step()` and `.state_name()` on a
                    // machine-typed receiver are recorded in the checker-owned
                    // `machine_method_dispatch` side-table so HIR lowering can
                    // produce dedicated HIR nodes without falling through to the
                    // generic `method_call_rewrites` path (which would emit
                    // `MethodCallNoRewrite`).
                    //
                    // `.step()` additionally requires a mutable binding receiver:
                    // the internal `<Name>__step` helper returns a new machine
                    // value that must be stored back into the binding (slice 6).
                    // R-value and immutable-binding receivers are rejected here
                    // with a typed diagnostic.
                    if self
                        .type_defs
                        .get(name)
                        .is_some_and(|td| td.kind == TypeDefKind::Machine)
                    {
                        match method {
                            "step" => {
                                // Enforce mutable-binding receiver requirement.
                                // A bare identifier receiver is the common case;
                                // r-value and non-identifier receivers are also
                                // rejected because store-back (slice 6) cannot
                                // target them.
                                let receiver_binding_name = match &receiver.0 {
                                    Expr::Identifier(n) => Some(n.clone()),
                                    _ => None,
                                };
                                let receiver_is_mutable = receiver_binding_name
                                    .as_deref()
                                    .and_then(|n| self.env.lookup_ref(n))
                                    .is_some_and(|b| b.is_mutable);
                                if !receiver_is_mutable {
                                    let receiver_name = if let Some(n) = &receiver_binding_name {
                                        format!("`{n}`")
                                    } else {
                                        "this expression".to_string()
                                    };
                                    self.report_error(
                                        TypeErrorKind::MutabilityError,
                                        span,
                                        format!(
                                            "`.step()` requires a mutable binding receiver; \
                                             {receiver_name} is not declared with `var`"
                                        ),
                                    );
                                } else if let Some(n) = &receiver_binding_name {
                                    // `.step()` semantically reassigns the binding via the
                                    // synthesised store-back primitive (slice 6). Mark the
                                    // binding as written so the unused-mut analysis does
                                    // not flag `var lc = ...; lc.step(...)` as a
                                    // never-reassigned mutable binding.
                                    self.env.mark_written(n);
                                }
                                self.machine_method_dispatch.insert(
                                    SpanKey::from(span),
                                    MachineMethodKind::Step {
                                        machine_name: name.clone(),
                                    },
                                );
                            }
                            "state_name" => {
                                self.machine_method_dispatch.insert(
                                    SpanKey::from(span),
                                    MachineMethodKind::StateName {
                                        machine_name: name.clone(),
                                    },
                                );
                            }
                            _ => {}
                        }
                    }
                    // #1295: stdlib `impl Closable for T { fn close }` flattens
                    // into the inherent-method table on T; honour any
                    // `consumes_receiver` declared on a trait whose impl
                    // contributed this method.
                    if self.named_type_method_consumes_receiver(name, method) {
                        self.method_call_consumes_receiver
                            .insert(SpanKey::from(span));
                        let resolved_recv = self.subst.resolve(&receiver_ty);
                        self.mark_expr_moved_if_non_copy(&receiver.0, &receiver.1, &resolved_recv);
                    }
                    self.record_handle_method_call_rewrite_if_any(&resolved, method, span);
                    self.record_monomorphic_extern_symbol_rewrite_if_any(&sig, span);
                    return applied_sig.return_type;
                }
                // Type-parameter method dispatch: resolve from trait bounds.
                // When the receiver is a generic type parameter (e.g. `T` in
                // `fn report<T: Measurable>(item: T)`), look up the method
                // from the traits that bound that parameter.
                let bounds_for_type_param = self.current_function.as_ref().and_then(|fn_name| {
                    self.fn_sigs.get(fn_name).and_then(|sig| {
                        if sig.type_params.contains(name) {
                            sig.type_param_bounds.get(name).cloned()
                        } else {
                            None
                        }
                    })
                });
                if let Some(bounds) = bounds_for_type_param {
                    for bound_trait in &bounds {
                        if let Some(mut trait_sig) = self.lookup_trait_method(bound_trait, method) {
                            // Replace `Self` references with the type parameter type.
                            let self_ty = resolved.clone();
                            for param_ty in &mut trait_sig.params {
                                *param_ty = param_ty.substitute_named_param("Self", &self_ty);
                            }
                            trait_sig.return_type = trait_sig
                                .return_type
                                .substitute_named_param("Self", &self_ty);
                            let applied_sig = self.apply_instantiated_call_signature(
                                &trait_sig,
                                None,
                                args,
                                span,
                                SignatureArgApplication::PositionalOnly {
                                    arity_context: format!("method '{method}'"),
                                },
                                true,
                            );
                            self.record_method_call_receiver_kind(
                                span,
                                MethodCallReceiverKind::NamedTypeInstance {
                                    type_name: name.clone(),
                                },
                            );
                            let qualified = format!("{bound_trait}::{method}");
                            self.apply_consume_receiver_if_flagged(
                                &qualified,
                                receiver,
                                &receiver_ty,
                                span,
                            );
                            return applied_sig.return_type;
                        }
                    }
                }
                // Synthesize args even if method unknown (for error recovery)
                for arg in args {
                    let (expr, sp) = arg.expr();
                    self.synthesize(expr, sp);
                }
                self.report_error_with_suggestions(
                    TypeErrorKind::UndefinedMethod,
                    span,
                    format!("no method `{method}` on `{}`", resolved.user_facing()),
                    self.similar_methods(&resolved, method),
                );
                Ty::Error
            }
            // Trait object method dispatch: look up methods from all trait bounds
            (Ty::TraitObject { traits }, _) => {
                // Try to find the method in any of the traits
                let mut found_sig = None;
                let mut found_bound = None;
                for bound in traits {
                    if let Some(sig) = self.lookup_trait_method(&bound.trait_name, method) {
                        found_sig = Some(sig);
                        found_bound = Some(bound);
                        break;
                    }
                }

                if let Some(mut sig) = found_sig {
                    if let Some(bound) = found_bound {
                        self.record_method_call_receiver_kind(
                            span,
                            MethodCallReceiverKind::TraitObject {
                                trait_name: bound.trait_name.clone(),
                            },
                        );
                        // Record the per-call-site vtable-slot resolution that
                        // HIR/MIR lowering will consume to emit
                        // `Instr::CallTraitMethod`. Slot convention follows
                        // `hew-runtime/src/trait_object.rs::HewVtable`: slots
                        // 0..3 are the fixed prefix triple
                        // (`drop_in_place`/`size_of`/`align_of`), trait methods
                        // start at slot 3 in trait-declaration order.
                        if let Some(trait_info) = self.trait_defs.get(&bound.trait_name) {
                            if let Some(method_idx) =
                                trait_info.methods.iter().position(|m| m.name == method)
                            {
                                // Slot index is bounded by the trait's
                                // method count, which Hew limits to
                                // u32-sized vtables long before any
                                // truncation risk. `try_from` keeps the
                                // boundary explicit (LESSONS:
                                // `boundary-fail-closed`).
                                let slot = 3 + u32::try_from(method_idx).unwrap_or(u32::MAX);
                                self.dyn_trait_method_calls.insert(
                                    SpanKey::from(span),
                                    crate::check::types::DynMethodCall {
                                        trait_name: bound.trait_name.clone(),
                                        method_name: method.to_string(),
                                        slot,
                                    },
                                );
                            }
                        }
                        let qualified = format!("{}::{method}", bound.trait_name);
                        self.apply_consume_receiver_if_flagged(
                            &qualified,
                            receiver,
                            &receiver_ty,
                            span,
                        );
                    }
                    if let Some(bound) = found_bound {
                        self.apply_trait_object_bound_substitutions(&mut sig, bound);
                    }
                    self.apply_instantiated_call_signature(
                        &sig,
                        None,
                        args,
                        span,
                        SignatureArgApplication::PositionalOnly {
                            arity_context: format!("method '{method}'"),
                        },
                        true,
                    )
                    .return_type
                } else {
                    for arg in args {
                        let (expr, sp) = arg.expr();
                        self.synthesize(expr, sp);
                    }
                    self.report_error(
                        TypeErrorKind::UndefinedMethod,
                        span,
                        format!("no method `{method}` on `{}`", resolved.user_facing()),
                    );
                    Ty::Error
                }
            }
            // For error types, don't report additional errors
            (Ty::Error, _) => {
                for arg in args {
                    let (expr, sp) = arg.expr();
                    self.synthesize(expr, sp);
                }
                Ty::Error
            }
            _ => {
                // Stage A2: before reporting "no method on X", consult the
                // user-impl side table for primitive / compiler-builtin
                // generic receivers.  This catches `Ty::I64`, `Ty::Bool`,
                // `Ty::Char`, the integer/float width aliases, and bare
                // `Vec`/`HashMap`/`HashSet` references that fall through
                // every earlier arm.  Per-receiver-kind sites that route
                // through `check_*_method` (Vec, HashMap, HashSet, String,
                // Bytes) consult the same table at their own not-found
                // branches so dispatch is exhaustive.
                if let Some(ret_ty) =
                    self.try_dispatch_primitive_trait_method(&resolved, method, args, span)
                {
                    return ret_ty;
                }
                for arg in args {
                    let (expr, sp) = arg.expr();
                    self.synthesize(expr, sp);
                }
                self.report_error(
                    TypeErrorKind::UndefinedMethod,
                    span,
                    format!("no method `{method}` on `{}`", resolved.user_facing()),
                );
                Ty::Error
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::module_registry::ModuleRegistry;

    /// A pending lowering fact whose element type resolves to `Ty::Error` must be
    /// dropped silently by `finalize_lowering_facts` without emitting a new error.
    ///
    /// Background: `validate_hashset_element_type` allows `Ty::Error` through
    /// (correct — avoids cascading diagnostics), which means
    /// `record_hashset_lowering_fact` can be called with `Ty::Error` as the
    /// element type.  Before this fix, `from_hashset_element_type(Ty::Error)`
    /// returned `Err(UnresolvedHashSetElementType)` and the handler emitted a
    /// spurious "element type is unresolved" diagnostic even though the real error
    /// had already been reported upstream.
    #[test]
    fn runtime_stream_element_name_stays_canonical() {
        assert_eq!(
            Checker::runtime_stream_element_name(&Ty::String),
            Some("string")
        );
        assert_eq!(
            Checker::runtime_stream_element_name(&Ty::Bytes),
            Some("bytes")
        );
        assert_eq!(
            Checker::runtime_stream_element_name(&Ty::Named {
                builtin: None,
                name: "string".into(),
                args: vec![],
            }),
            None
        );
        assert_eq!(
            Checker::runtime_stream_element_name(&Ty::Named {
                builtin: None,
                name: "str".into(),
                args: vec![],
            }),
            None
        );
    }

    #[test]
    fn stream_receiver_element_kind_stays_canonical() {
        assert_eq!(Checker::stream_receiver_element_kind(&Ty::String), "string");
        assert_eq!(Checker::stream_receiver_element_kind(&Ty::Bytes), "bytes");
        assert_eq!(
            Checker::stream_receiver_element_kind(&Ty::Named {
                builtin: None,
                name: "String".into(),
                args: vec![],
            }),
            ""
        );
        assert_eq!(
            Checker::stream_receiver_element_kind(&Ty::Var(TypeVar::fresh())),
            ""
        );
    }

    #[test]
    fn finalize_lowering_facts_silently_drops_error_element_type() {
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let span = 10..20;
        checker.record_hashset_lowering_fact(&span, &Ty::Error);

        let facts = checker.finalize_lowering_facts();

        assert!(
            facts.is_empty(),
            "a pending fact with Ty::Error element must not appear in the finalized output"
        );
        assert!(
            checker.errors.is_empty(),
            "finalize_lowering_facts must not emit a spurious error for Ty::Error elements; \
             the real error was reported upstream"
        );
    }

    /// A pending lowering fact whose element type is genuinely unresolved
    /// (`Ty::Var`) after inference must be pruned AND must emit an
    /// `InferenceFailed` diagnostic pointing at the lowering site.
    #[test]
    fn finalize_lowering_facts_emits_error_for_unresolved_inference_var() {
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let span = 30..40;
        let unresolved_var = Ty::Var(crate::ty::TypeVar::fresh());
        checker.record_hashset_lowering_fact(&span, &unresolved_var);

        let facts = checker.finalize_lowering_facts();

        assert!(
            facts.is_empty(),
            "a pending fact with an unresolved Ty::Var element must not appear in the output"
        );
        assert!(
            checker
                .errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::InferenceFailed),
            "finalize_lowering_facts must emit InferenceFailed for a genuinely unresolved \
             element type; got: {:?}",
            checker.errors
        );
    }

    // ── HashMap admission finalization ───────────────────────────────────────

    /// A deferred `HashMap` admission whose key type is `Ty::Error` must be
    /// dropped silently — no new diagnostic, no cascade.
    #[test]
    fn finalize_hashmap_admission_silently_drops_error_key() {
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let span = 10..20;
        checker.deferred_hashmap_admission.insert(
            SpanKey::from(&span),
            DeferredHashMapAdmission {
                span: span.clone(),
                key_ty: Ty::Error,
                val_ty: Ty::I64,
                source_module: None,
            },
        );

        checker.finalize_hashmap_admission();

        assert!(
            checker.errors.is_empty(),
            "finalize_hashmap_admission must not emit an error when key_ty is Ty::Error; \
             the upstream diagnostic already covers it. Got: {:?}",
            checker.errors
        );
    }

    /// A deferred `HashMap` admission whose value type is still an unresolved
    /// inference variable after inference must emit `InferenceFailed`.
    #[test]
    fn finalize_hashmap_admission_emits_inference_failed_for_var_value() {
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let span = 30..40;
        checker.deferred_hashmap_admission.insert(
            SpanKey::from(&span),
            DeferredHashMapAdmission {
                span: span.clone(),
                key_ty: Ty::String,
                val_ty: Ty::Var(TypeVar::fresh()),
                source_module: None,
            },
        );

        checker.finalize_hashmap_admission();

        assert!(
            checker
                .errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::InferenceFailed),
            "finalize_hashmap_admission must emit InferenceFailed when val_ty is an unresolved \
             Ty::Var; got: {:?}",
            checker.errors
        );
    }

    /// Two deferred `HashMap` admissions sharing the same unresolved
    /// `(key_var, val_var)` pair must emit exactly one `InferenceFailed`.
    #[test]
    fn finalize_hashmap_admission_dedup_pair_emits_single_error() {
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let key_var = TypeVar::fresh();
        let val_var = TypeVar::fresh();
        let span_a = 100..110;
        let span_b = 200..210;

        checker.deferred_hashmap_admission.insert(
            SpanKey::from(&span_a),
            DeferredHashMapAdmission {
                span: span_a.clone(),
                key_ty: Ty::Var(key_var),
                val_ty: Ty::Var(val_var),
                source_module: None,
            },
        );
        checker.deferred_hashmap_admission.insert(
            SpanKey::from(&span_b),
            DeferredHashMapAdmission {
                span: span_b.clone(),
                key_ty: Ty::Var(key_var),
                val_ty: Ty::Var(val_var),
                source_module: None,
            },
        );

        checker.finalize_hashmap_admission();

        let inference_failed: Vec<_> = checker
            .errors
            .iter()
            .filter(|e| e.kind == TypeErrorKind::InferenceFailed)
            .collect();
        assert_eq!(
            inference_failed.len(),
            1,
            "two admissions sharing the same (key_var, val_var) pair must emit exactly one \
             InferenceFailed; got {}: {:?}",
            inference_failed.len(),
            checker.errors,
        );
    }

    // ── HashSet admission finalization ───────────────────────────────────────

    /// A deferred `HashSet` admission whose element type is `Ty::Error` must be
    /// dropped silently — mirrors the lowering-facts sentinel for the admission path.
    #[test]
    fn finalize_hashset_admission_silently_drops_error_element() {
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let span = 50..60;
        checker.deferred_hashset_admission.insert(
            SpanKey::from(&span),
            DeferredHashSetAdmission {
                span: span.clone(),
                elem_ty: Ty::Error,
                source_module: None,
            },
        );

        checker.finalize_hashset_admission();

        assert!(
            checker.errors.is_empty(),
            "finalize_hashset_admission must not emit an error when elem_ty is Ty::Error; \
             the upstream diagnostic already covers it. Got: {:?}",
            checker.errors
        );
    }

    /// A deferred `HashSet` admission whose element type is still an unresolved
    /// inference variable after inference must emit `InferenceFailed` with an
    /// "add annotation" hint.
    #[test]
    fn finalize_hashset_admission_emits_inference_failed_for_var_element() {
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let span = 70..80;
        checker.deferred_hashset_admission.insert(
            SpanKey::from(&span),
            DeferredHashSetAdmission {
                span: span.clone(),
                elem_ty: Ty::Var(TypeVar::fresh()),
                source_module: None,
            },
        );

        checker.finalize_hashset_admission();

        assert!(
            checker
                .errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::InferenceFailed),
            "finalize_hashset_admission must emit InferenceFailed for an unresolved \
             Ty::Var element; got: {:?}",
            checker.errors
        );
    }

    // ── Vec admission finalization ───────────────────────────────────────────

    /// A deferred `Vec` admission whose element type contains `Ty::Error` (via
    /// `contains_error()`) must be dropped silently — no cascade.
    #[test]
    fn finalize_vec_admission_silently_drops_error_element() {
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let span = 90..100;
        // Nest Ty::Error inside a Vec element to exercise the contains_error() path,
        // not just a bare Ty::Error match.
        let elem_ty = Ty::Named {
            builtin: None,
            name: "Result".into(),
            args: vec![Ty::Error, Ty::I64],
        };
        checker.deferred_vec_admission.insert(
            SpanKey::from(&span),
            DeferredVecAdmission {
                span: span.clone(),
                elem_ty,
                source_module: None,
            },
        );

        checker.finalize_vec_admission();

        assert!(
            checker.errors.is_empty(),
            "finalize_vec_admission must not emit an error when the element type \
             contains Ty::Error; the upstream diagnostic already covers it. Got: {:?}",
            checker.errors
        );
    }

    /// A deferred `Vec` admission whose element type contains an unresolved
    /// inference variable after inference must emit `InferenceFailed`.
    #[test]
    fn finalize_vec_admission_emits_inference_failed_for_unresolved_var() {
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let span = 110..120;
        checker.deferred_vec_admission.insert(
            SpanKey::from(&span),
            DeferredVecAdmission {
                span: span.clone(),
                elem_ty: Ty::Var(TypeVar::fresh()),
                source_module: None,
            },
        );

        checker.finalize_vec_admission();

        assert!(
            checker
                .errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::InferenceFailed),
            "finalize_vec_admission must emit InferenceFailed for an unresolved \
             Ty::Var element; got: {:?}",
            checker.errors
        );
    }
}
