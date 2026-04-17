#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
use super::*;
use crate::builtin_names::BuiltinNamedType;
use crate::method_resolution::{
    collect_method_sigs_for_receiver, lookup_builtin_method_sig,
    lookup_named_method_sig as shared_lookup_named_method_sig,
};

impl Checker {
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
                         `Sender<int>` or `Receiver<String>`",
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
                         only Channel<String> and Channel<int> are currently supported"
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
                    },
                );
            }
            // resolve_channel_method returning None for a concrete, supported
            // type would be a compiler bug — the match table in stdlib.rs is
            // exhaustive for the (Sender|Receiver, method, is_int) combinations
            // we produce.  No else-branch needed; the span stays absent and
            // codegen fails closed, which is the desired invariant.
        }

        self.errors.extend(new_errors);
    }

    fn record_method_call_receiver_kind(&mut self, span: &Span, kind: MethodCallReceiverKind) {
        self.method_call_receiver_kinds
            .insert(SpanKey::from(span), kind);
    }

    fn record_method_call_rewrite(&mut self, span: &Span, rewrite: MethodCallRewrite) {
        self.method_call_rewrites
            .insert(SpanKey::from(span), rewrite);
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
            },
        );
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
        let Ty::Named { name, .. } = receiver_ty else {
            return;
        };
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

    fn runtime_stream_element_name(ty: &Ty) -> Option<&'static str> {
        match ty {
            Ty::String => Some("String"),
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
        _span: &Span,
    ) -> Option<Ty> {
        let Ty::Named {
            name,
            args: type_args,
        } = receiver_ty
        else {
            return None;
        };
        let sig = self.lookup_named_method_sig(name, type_args, method)?;
        for (i, arg) in args.iter().enumerate() {
            if let Some(param_ty) = sig.params.get(i) {
                let (expr, sp) = arg.expr();
                self.check_against(expr, sp, param_ty);
            }
        }
        Some(sig.return_type)
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
                self.record_method_call_receiver_kind(
                    span,
                    MethodCallReceiverKind::NamedTypeInstance {
                        type_name: name.clone(),
                    },
                );
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
            "next" | "close" => {
                let c_symbol = crate::stdlib::resolve_stream_method(
                    BuiltinNamedType::Stream.canonical_name(),
                    method,
                    Self::runtime_stream_element_name(&resolved_inner),
                )
                .unwrap_or_else(|| unreachable!("builtin Stream::{method} rewrite missing"));
                self.record_runtime_method_call_rewrite(span, c_symbol);
                sig.return_type
            }
            "lines" => {
                if inner != Ty::String {
                    self.report_error(
                        TypeErrorKind::InvalidOperation,
                        span,
                        format!(
                            "`lines()` is only supported on `Stream<String>`, \
                             not `Stream<{}>`",
                            inner.user_facing()
                        ),
                    );
                }
                let c_symbol = crate::stdlib::resolve_stream_method(
                    BuiltinNamedType::Stream.canonical_name(),
                    method,
                    None,
                )
                .unwrap_or_else(|| unreachable!("builtin Stream::{method} rewrite missing"));
                self.record_runtime_method_call_rewrite(span, c_symbol);
                sig.return_type
            }
            "collect" => {
                if inner != Ty::String {
                    self.report_error(
                        TypeErrorKind::InvalidOperation,
                        span,
                        format!(
                            "`collect()` is only supported on `Stream<String>`, \
                             not `Stream<{}>`",
                            inner.user_facing()
                        ),
                    );
                }
                let c_symbol = crate::stdlib::resolve_stream_method(
                    BuiltinNamedType::Stream.canonical_name(),
                    method,
                    None,
                )
                .unwrap_or_else(|| unreachable!("builtin Stream::{method} rewrite missing"));
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
                let c_symbol = crate::stdlib::resolve_stream_method(
                    BuiltinNamedType::Stream.canonical_name(),
                    method,
                    None,
                )
                .unwrap_or_else(|| unreachable!("builtin Stream::{method} rewrite missing"));
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

    pub(super) fn check_string_method(
        &mut self,
        method: &str,
        args: &[CallArg],
        span: &Span,
    ) -> Ty {
        match method {
            "len" => Ty::I64,
            "contains" | "starts_with" | "ends_with" => {
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    self.check_against(expr, sp, &Ty::String);
                }
                Ty::Bool
            }
            "is_digit" | "is_alpha" | "is_alphanumeric" | "is_empty" => {
                self.check_arity(args, 0, &format!("`String::{method}`"), span);
                Ty::Bool
            }
            "to_uppercase" | "to_lowercase" | "to_upper" | "to_lower" | "trim" | "clone" => {
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
                Ty::String
            }
            "split" => {
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    self.check_against(expr, sp, &Ty::String);
                }
                self.make_vec_type(Ty::String, span)
            }
            "lines" => {
                self.check_arity(args, 0, "`String::lines`", span);
                self.make_vec_type(Ty::String, span)
            }
            "find" | "index_of" => {
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    self.check_against(expr, sp, &Ty::String);
                }
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
                Ty::String
            }
            "repeat" => {
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    self.check_against(expr, sp, &Ty::I64);
                }
                Ty::String
            }
            "char_at" => {
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    self.check_against(expr, sp, &Ty::I64);
                }
                Ty::I64
            }
            "chars" => {
                self.check_arity(args, 0, "`String::chars`", span);
                self.make_vec_type(Ty::Char, span)
            }
            _ => {
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
        let result = match method {
            "push" => {
                self.check_arity(args, 1, "`Vec::push`", span);
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    self.check_against(expr, sp, &elem_ty);
                }
                self.reject_rc_collection_element("Vec", &elem_ty, span);
                Ty::Unit
            }
            "pop" => {
                self.check_arity(args, 0, "`Vec::pop`", span);
                self.reject_rc_collection_element("Vec", &elem_ty, span);
                elem_ty.clone()
            }
            "len" => Ty::I64,
            "get" | "remove" => {
                self.check_arity(args, 1, &format!("`Vec::{method}`"), span);
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    self.check_against(expr, sp, &Ty::I64);
                }
                self.reject_rc_collection_element("Vec", &elem_ty, span);
                elem_ty.clone()
            }
            "contains" => {
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    self.check_against(expr, sp, &elem_ty);
                }
                Ty::Bool
            }
            "is_empty" => Ty::Bool,
            "clear" => {
                self.check_arity(args, 0, "`Vec::clear`", span);
                Ty::Unit
            }
            "clone" => {
                self.check_arity(args, 0, "`Vec::clone`", span);
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
                Ty::Unit
            }
            "append" | "extend" => {
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    self.check_against(expr, sp, receiver_ty);
                }
                self.reject_rc_collection_element("Vec", &elem_ty, span);
                Ty::Unit
            }
            "join" => {
                self.check_arity(args, 1, "`Vec::join`", span);
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    self.check_against(expr, sp, &Ty::String);
                }
                if elem_ty != Ty::String {
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
                    let (freshened_params, freshened_ret, resolved_type_args) =
                        self.instantiate_fn_sig_for_call(&sig, None, span);
                    self.record_module_qualified_stdlib_call_rewrite_if_any(name, method, span);
                    // Separate positional and named args
                    let positional_count = args.iter().take_while(|a| a.name().is_none()).count();
                    let positional_args = &args[..positional_count];
                    let named_args = &args[positional_count..];

                    // Arity check
                    if !sig.accepts_kwargs && args.len() != freshened_params.len() {
                        self.report_error(
                            TypeErrorKind::ArityMismatch,
                            span,
                            format!(
                                "expected {} arguments, found {}",
                                freshened_params.len(),
                                args.len()
                            ),
                        );
                    } else if sig.accepts_kwargs && positional_count < freshened_params.len() {
                        self.report_error(
                            TypeErrorKind::ArityMismatch,
                            span,
                            format!(
                                "expected at least {} positional arguments, found {}",
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
                        if let Some(arg_name) = arg.name() {
                            if let Some(idx) = sig.param_names.iter().position(|n| n == arg_name) {
                                if let Some(param_ty) = freshened_params.get(idx) {
                                    let (expr, sp) = arg.expr();
                                    self.check_against(expr, sp, param_ty);
                                }
                            } else if !sig.accepts_kwargs {
                                let (_, sp) = arg.expr();
                                self.report_error(
                                    TypeErrorKind::InvalidOperation,
                                    sp,
                                    format!("unknown named argument `{arg_name}`"),
                                );
                            } else {
                                // For kwargs functions, synthesize the expression type
                                let (expr, sp) = arg.expr();
                                self.synthesize(expr, sp);
                            }
                        }
                    }
                    self.enforce_type_param_bounds(&sig, &resolved_type_args, span);

                    if !sig.type_params.is_empty() {
                        self.record_concrete_call_type_args(span, &resolved_type_args);
                    }
                    // Channel constructor: inject a shared type variable so
                    // Sender<T> and Receiver<T> from the same `new` call are
                    // linked through unification.
                    if key == "channel.new" {
                        let t = Ty::Var(TypeVar::fresh());
                        return Ty::Tuple(vec![Ty::sender(t.clone()), Ty::receiver(t)]);
                    }
                    return freshened_ret;
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
                    name,
                    args: type_args,
                },
                _,
            ) if name == "Vec" => {
                self.check_vec_method(type_args, &receiver_ty, &resolved, method, args, span)
            }
            // HashMap methods
            (
                Ty::Named {
                    name,
                    args: type_args,
                },
                _,
            ) if name == "HashMap" => self.check_hashmap_method(type_args, method, args, span),
            // HashSet methods
            (
                Ty::Named {
                    name,
                    args: type_args,
                },
                _,
            ) if name == "HashSet" => {
                // Preserve the receiver's original inference vars so a later non-literal insert
                // can refine an earlier `IntLiteral` element before we validate lowerability.
                let original_type_args = match &receiver_ty {
                    Ty::Named { name, args } if name == "HashSet" => args.as_slice(),
                    _ => type_args,
                };
                self.check_hashset_method(original_type_args, method, args, span)
            }
            // Rc<T> methods
            (
                Ty::Named {
                    name,
                    args: type_args,
                },
                _,
            ) if name == "Rc" => self.check_rc_method(type_args, method, args, span),
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
            // Duration methods
            (Ty::Duration, _) => match method {
                "nanos" | "micros" | "millis" | "secs" | "mins" | "hours" => {
                    self.check_arity(args, 0, &format!("`duration::{method}`"), span);
                    Ty::I64
                }
                "abs" => {
                    self.check_arity(args, 0, "`duration::abs`", span);
                    Ty::Duration
                }
                "is_zero" => {
                    self.check_arity(args, 0, "`duration::is_zero`", span);
                    Ty::Bool
                }
                _ => {
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
            },
            // Numeric type conversion methods (§10.1 intrinsics)
            // .to_i8(), .to_i16(), .to_i32(), .to_i64(), .to_u8(), .to_u16(),
            // .to_u32(), .to_u64(), .to_f32(), .to_f64(), .to_isize(), .to_usize()
            (resolved, method) if resolved.is_numeric() && method.starts_with("to_") => {
                match method {
                    "to_i8" => Ty::I8,
                    "to_i16" => Ty::I16,
                    "to_i32" => Ty::I32,
                    // to_isize maps to I64 (platform-dependent, default 64-bit)
                    "to_i64" | "to_isize" => Ty::I64,
                    "to_u8" => Ty::U8,
                    "to_u16" => Ty::U16,
                    "to_u32" => Ty::U32,
                    // to_usize maps to U64 (platform-dependent, default 64-bit)
                    "to_u64" | "to_usize" => Ty::U64,
                    "to_f32" => Ty::F32,
                    "to_f64" => Ty::F64,
                    _ => {
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
                        Ty::Error
                    }
                }
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
            // Named types that have built-in methods (Actor<T> from lambda actors)
            (
                Ty::Named {
                    name,
                    args: type_args,
                },
                "send",
            ) if name == "Actor" => {
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
            // String methods
            (Ty::String, _) => self.check_string_method(method, args, span),
            // Generator methods: .next() returns the yielded type
            (
                Ty::Named {
                    name,
                    args: type_args,
                },
                "next",
            ) if name == "Generator" || name == "AsyncGenerator" => type_args
                .first()
                .cloned()
                .unwrap_or(Ty::Var(TypeVar::fresh())),
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
                    name,
                    args: type_args,
                },
                _,
            ) if builtin_named_type(name) == Some(BuiltinNamedType::Stream) => {
                // Stream<T> methods are not supported on wasm32: the stream
                // runtime module is not compiled for wasm32.
                self.reject_wasm_feature(span, WasmUnsupportedFeature::Streams);
                self.check_stream_method(type_args, method, args, span)
            }
            // Sink<T> methods
            (
                Ty::Named {
                    name,
                    args: type_args,
                },
                _,
            ) if builtin_named_type(name) == Some(BuiltinNamedType::Sink) => {
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
                let receiver_ty = Ty::sink(inner.clone());
                match method {
                    "write" => {
                        let sig =
                            lookup_builtin_method_sig(&receiver_ty, method).unwrap_or_else(|| {
                                unreachable!("builtin Sink::write signature missing")
                            });
                        if let Some(arg) = args.first() {
                            let (expr, sp) = arg.expr();
                            if let Some(param_ty) = sig.params.first() {
                                self.check_against(expr, sp, param_ty);
                            }
                        }
                        let c_symbol = crate::stdlib::resolve_stream_method(
                            BuiltinNamedType::Sink.canonical_name(),
                            method,
                            Self::runtime_stream_element_name(&self.subst.resolve(&inner)),
                        )
                        .unwrap_or_else(|| unreachable!("builtin Sink::write rewrite missing"));
                        self.record_runtime_method_call_rewrite(span, c_symbol);
                        sig.return_type
                    }
                    "close" | "flush" => {
                        let c_symbol = crate::stdlib::resolve_stream_method(
                            BuiltinNamedType::Sink.canonical_name(),
                            method,
                            None,
                        )
                        .unwrap_or_else(|| unreachable!("builtin Sink::{method} rewrite missing"));
                        self.record_runtime_method_call_rewrite(span, c_symbol);
                        lookup_builtin_method_sig(&receiver_ty, method)
                            .unwrap_or_else(|| {
                                unreachable!("builtin Sink::{method} signature missing")
                            })
                            .return_type
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
                    name,
                    args: type_args,
                },
                _,
            ) if builtin_named_type(name) == Some(BuiltinNamedType::Sender) => {
                let inner = type_args
                    .first()
                    .cloned()
                    .unwrap_or(Ty::Var(TypeVar::fresh()));
                let receiver_ty = Ty::sender(inner.clone());
                let resolved_inner = self.subst.resolve(&inner);
                match method {
                    "send" => {
                        let sig =
                            lookup_builtin_method_sig(&receiver_ty, method).unwrap_or_else(|| {
                                unreachable!("builtin Sender::send signature missing")
                            });
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
                                     only Channel<String> and Channel<int> are currently supported"
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
                            let c_symbol = crate::stdlib::resolve_channel_method(
                                BuiltinNamedType::Sender.canonical_name(),
                                method,
                                Some(&resolved_inner),
                            )
                            .unwrap_or_else(|| {
                                unreachable!("builtin Sender::send rewrite missing")
                            });
                            self.record_runtime_method_call_rewrite(span, c_symbol);
                        }
                        sig.return_type
                    }
                    "clone" | "close" => {
                        let c_symbol = crate::stdlib::resolve_channel_method(
                            BuiltinNamedType::Sender.canonical_name(),
                            method,
                            Some(&resolved_inner),
                        )
                        .unwrap_or_else(|| {
                            unreachable!("builtin Sender::{method} rewrite missing")
                        });
                        self.record_runtime_method_call_rewrite(span, c_symbol);
                        lookup_builtin_method_sig(&receiver_ty, method)
                            .unwrap_or_else(|| {
                                unreachable!("builtin Sender::{method} signature missing")
                            })
                            .return_type
                    }
                    _ => {
                        self.check_named_method_fallback(&resolved, method, args, span, "Sender<T>")
                    }
                }
            }
            // Receiver<T> methods
            (
                Ty::Named {
                    name,
                    args: type_args,
                },
                _,
            ) if builtin_named_type(name) == Some(BuiltinNamedType::Receiver) => {
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
                             only Channel<String> and Channel<int> are currently supported"
                        ),
                    );
                    return Ty::Error;
                }
                match method {
                    "recv" => {
                        self.reject_wasm_feature(span, WasmUnsupportedFeature::BlockingChannelRecv);
                        let sig =
                            lookup_builtin_method_sig(&receiver_ty, method).unwrap_or_else(|| {
                                unreachable!("builtin Receiver::recv signature missing")
                            });
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
                            let c_symbol = crate::stdlib::resolve_channel_method(
                                BuiltinNamedType::Receiver.canonical_name(),
                                method,
                                Some(&resolved_inner),
                            )
                            .unwrap_or_else(|| {
                                unreachable!("builtin Receiver::recv rewrite missing")
                            });
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
                            let c_symbol = crate::stdlib::resolve_channel_method(
                                BuiltinNamedType::Receiver.canonical_name(),
                                method,
                                Some(&resolved_inner),
                            )
                            .unwrap_or_else(|| {
                                unreachable!("builtin Receiver::try_recv rewrite missing")
                            });
                            self.record_runtime_method_call_rewrite(span, c_symbol);
                        }
                        lookup_builtin_method_sig(&receiver_ty, method)
                            .unwrap_or_else(|| {
                                unreachable!("builtin Receiver::try_recv signature missing")
                            })
                            .return_type
                    }
                    "close" => {
                        // `close` maps to a single type-independent symbol.
                        let c_symbol = crate::stdlib::resolve_channel_method(
                            BuiltinNamedType::Receiver.canonical_name(),
                            method,
                            Some(&resolved_inner),
                        )
                        .unwrap_or_else(|| unreachable!("builtin Receiver::close rewrite missing"));
                        self.record_runtime_method_call_rewrite(span, c_symbol);
                        lookup_builtin_method_sig(&receiver_ty, method)
                            .unwrap_or_else(|| {
                                unreachable!("builtin Receiver::close signature missing")
                            })
                            .return_type
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
                },
                _,
            ) => {
                if let Some(sig) = self.lookup_named_method_sig(name, type_args, method) {
                    let (freshened_params, freshened_ret, resolved_type_args) =
                        self.instantiate_fn_sig_for_call(&sig, None, span);
                    self.check_arity(
                        args,
                        freshened_params.len(),
                        &format!("method '{method}'"),
                        span,
                    );
                    for (i, arg) in args.iter().enumerate() {
                        if let Some(param_ty) = freshened_params.get(i) {
                            let (expr, sp) = arg.expr();
                            self.check_against(expr, sp, param_ty);
                        }
                    }
                    self.enforce_type_param_bounds(&sig, &resolved_type_args, span);
                    if !sig.type_params.is_empty() {
                        self.record_concrete_call_type_args(span, &resolved_type_args);
                    }
                    self.record_method_call_receiver_kind(
                        span,
                        MethodCallReceiverKind::NamedTypeInstance {
                            type_name: name.clone(),
                        },
                    );
                    self.record_handle_method_call_rewrite_if_any(&resolved, method, span);
                    return freshened_ret;
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
                            let (freshened_params, freshened_ret, resolved_type_args) =
                                self.instantiate_fn_sig_for_call(&trait_sig, None, span);

                            self.check_arity(
                                args,
                                freshened_params.len(),
                                &format!("method '{method}'"),
                                span,
                            );
                            for (i, arg) in args.iter().enumerate() {
                                if let Some(param_ty) = freshened_params.get(i) {
                                    let (expr, sp) = arg.expr();
                                    self.check_against(expr, sp, param_ty);
                                }
                            }
                            self.enforce_type_param_bounds(&trait_sig, &resolved_type_args, span);
                            if !trait_sig.type_params.is_empty() {
                                self.record_concrete_call_type_args(span, &resolved_type_args);
                            }
                            self.record_method_call_receiver_kind(
                                span,
                                MethodCallReceiverKind::NamedTypeInstance {
                                    type_name: name.clone(),
                                },
                            );
                            return freshened_ret;
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
                    }
                    // Apply type substitutions from bound's type arguments
                    if let Some(bound) = found_bound {
                        if let Some(trait_info) = self.trait_defs.get(&bound.trait_name) {
                            let type_params = &trait_info.type_params;
                            if type_params.len() == bound.args.len() {
                                // Apply substitutions
                                for (param_name, replacement) in
                                    type_params.iter().zip(bound.args.iter())
                                {
                                    // Substitute in parameter types
                                    for param_ty in &mut sig.params {
                                        *param_ty = param_ty
                                            .substitute_named_param(param_name, replacement);
                                    }
                                    // Substitute in return type
                                    sig.return_type = sig
                                        .return_type
                                        .substitute_named_param(param_name, replacement);
                                }
                            }
                        }
                    }
                    let (freshened_params, freshened_ret, resolved_type_args) =
                        self.instantiate_fn_sig_for_call(&sig, None, span);

                    self.check_arity(
                        args,
                        freshened_params.len(),
                        &format!("method '{method}'"),
                        span,
                    );
                    for (i, arg) in args.iter().enumerate() {
                        if let Some(param_ty) = freshened_params.get(i) {
                            let (expr, sp) = arg.expr();
                            self.check_against(expr, sp, param_ty);
                        }
                    }
                    self.enforce_type_param_bounds(&sig, &resolved_type_args, span);
                    if !sig.type_params.is_empty() {
                        // CODEGEN-TODO: TraitDispatchOp does not yet thread per-method type args;
                        // vtable ABI extension needed for generic trait-object method dispatch.
                        self.record_concrete_call_type_args(span, &resolved_type_args);
                    }
                    freshened_ret
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
            Some("String")
        );
        assert_eq!(
            Checker::runtime_stream_element_name(&Ty::Bytes),
            Some("bytes")
        );
        assert_eq!(
            Checker::runtime_stream_element_name(&Ty::Named {
                name: "string".into(),
                args: vec![],
            }),
            None
        );
        assert_eq!(
            Checker::runtime_stream_element_name(&Ty::Named {
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
}
