//! Checker methods grouped by responsibility: collections.
//! Split from `methods.rs`: checker methods, part 1 of 5.
#![allow(
    unused_imports,
    redundant_imports,
    clippy::wildcard_imports,
    reason = "chunk files share the parent module's import header"
)]
#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
use super::super::*;
use super::*;
use crate::builtin_names::BuiltinNamedType;
use crate::check::calls::SignatureArgApplication;
use crate::check::dispatch::resolve_method_call;
use crate::check::types::GenericCallee;
use crate::check::types::{BareActorResolution, DeferredBuiltinCloneAdmission, DeferredWireCodec};
use crate::method_resolution::{
    collect_method_sigs_for_receiver, instantiate_stdlib_method_sig, lookup_builtin_method_sig,
    lookup_named_method_sig as shared_lookup_named_method_sig,
};
use crate::runtime_call::{FloatMethodOp, IntArithKind, IntBitOp, IntMethodWidth};
use crate::stdlib::{STD_NET_CONNECTION, STD_NET_LISTENER};
use crate::BuiltinType;

impl Checker {
    pub(in crate::check) fn record_hashset_lowering_fact(&mut self, span: &Span, elem_ty: &Ty) {
        let key = SpanKey::in_module(span, self.current_module_idx);
        // If deferred admission was already recorded for this span, the
        // lowering-fact finalizer becomes the sole authority for any
        // InferenceFailed diagnostic at this site.  Remove the deferred entry
        // to prevent a duplicate error from finalize_hashset_admission.
        self.deferred_hashset_admission.remove(&key);
        // The resolver proves a template key from its declared bounds. No
        // concrete scalar metadata exists until the type is instantiated.
        if self.is_hashmap_abstract_key_param(elem_ty) {
            return;
        }
        self.pending_lowering_facts.insert(
            key,
            PendingLoweringFact::hashset(elem_ty.clone(), self.current_module.clone()),
        );
    }

    /// Finish map admission after inference and declaration registration.
    /// The replayed scope carries each type parameter's declared bounds from
    /// the record site, so a bare key type parameter (e.g. `K` in a generic
    /// actor's `HashMap<K, V>` field) is checked against its own `Hash + Eq`
    /// bounds rather than skipped or checked against an empty bound set.
    pub(in crate::check) fn finalize_hashmap_admission(&mut self) {
        let checks = std::mem::take(&mut self.deferred_hashmap_admission);
        let mut reported_var_pairs = HashSet::new();
        for (_, check) in checks {
            let key = self
                .subst
                .resolve(&check.key_ty)
                .materialize_literal_defaults();
            let value = self
                .subst
                .resolve(&check.val_ty)
                .materialize_literal_defaults();
            if key.contains_error() || value.contains_error() {
                continue;
            }
            let before = self.errors.len();
            if key.has_inference_var() || value.has_inference_var() {
                if reported_var_pairs.insert((key.clone(), value.clone())) {
                    self.report_error(
                        TypeErrorKind::InferenceFailed,
                        &check.span,
                        format!(
                            "cannot infer HashMap key or value type at the checker boundary \
                            (HashMap<{}, {}>); add an explicit type annotation",
                            key.user_facing(),
                            value.user_facing()
                        ),
                    );
                }
            } else {
                self.current_type_param_bounds
                    .push(super::types::TypeParamScope::new(
                        check.type_param_bounds,
                        HashMap::new(),
                    ));
                self.validate_collection_key_capabilities(&key, "Map", &check.span);
                self.current_type_param_bounds.pop();
            }
            for error in &mut self.errors[before..] {
                error.source_module.clone_from(&check.source_module);
            }
        }
        // The value-copy obligations a copying operation left behind: the value
        // type has settled by now.
        for (_span_key, check) in std::mem::take(&mut self.deferred_collection_value_copy) {
            let value = self
                .subst
                .resolve(&check.val_ty)
                .materialize_literal_defaults();
            if matches!(value, Ty::Error) || value.has_inference_var() {
                continue;
            }
            let before = self.errors.len();
            self.validate_collection_value_clone_type(
                &value,
                check.collection,
                &check.operation,
                &check.span,
            );
            for error in &mut self.errors[before..] {
                error.source_module.clone_from(&check.source_module);
            }
        }
    }

    /// Drain `deferred_hashset_admission`, resolve element types through the
    /// current substitution, and fail closed on any that are still unresolved
    /// or error-typed at the checker boundary.
    ///
    /// * `Ty::Var` → `InferenceFailed`: inference did not resolve the element type.
    /// * `Ty::Error` → silent drop: upstream already emitted a diagnostic.
    /// * Fully-resolved unsupported elements → already caught inline; silently
    ///   skipped here to avoid duplicate diagnostics.
    pub(in crate::check) fn finalize_hashset_admission(&mut self) {
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

            if !resolved.has_inference_var() {
                self.validate_collection_key_capabilities(&resolved, "Set", &check.span);
            }
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
    pub(in crate::check) fn finalize_vec_admission(&mut self) {
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

    /// Resolve a runtime-backed Vec method from the compiled-in stdlib source,
    /// never from user-shadowable `Vec::<method>` keys.
    pub(super) fn lookup_builtin_vec_method_sig(
        &self,
        type_args: &[Ty],
        method: &str,
    ) -> Option<FnSig> {
        let (impl_params, sig) = self.builtin_vec_method_sigs.get(method)?;
        Some(instantiate_stdlib_method_sig(sig, impl_params, type_args))
    }

    /// Validate the exceptional element types that cannot use `Vec`'s
    /// borrow-only index path.
    ///
    /// Ordinary `#[resource]` / `#[linear]` elements are deliberately admitted:
    /// MIR lowers `values[i]` through the owned-layout getter as an interior
    /// borrow and its escape/consume/rebind checks keep that borrow inside the
    /// collection's release authority. A pipe half, however, is an opaque
    /// endpoint with no readable borrowed-value surface, so it remains
    /// rejected regardless of its payload type.
    pub(in crate::check) fn validate_vec_index_borrow_surface(
        &mut self,
        ty: &Ty,
        span: &Span,
    ) -> bool {
        let resolved = self.subst.resolve(ty).materialize_literal_defaults();
        if let Ty::Named {
            head: crate::TypeHead::Builtin(builtin),
            ..
        } = resolved
        {
            if builtin.is_pipe_half() {
                self.report_error(
                    TypeErrorKind::InvalidOperation,
                    span,
                    format!(
                        "cannot index `Vec<{}>` by value: a pipe half is non-cloneable and \
                         has no copy operation; use `pop`, `remove`, or consuming iteration \
                         to move the endpoint out",
                        resolved.user_facing()
                    ),
                );
                return false;
            }
        }
        true
    }

    #[expect(
        clippy::too_many_lines,
        reason = "builtin stream typing and checker-owned rewrite metadata stay together"
    )]
    pub(in crate::check) fn check_stream_method(
        &mut self,
        type_args: &[Ty],
        method: &str,
        args: &[CallArg],
        span: &Span,
    ) -> Ty {
        let inner = Self::stream_element_type(type_args);
        // Gate 2: lowering-capability check. The element-layout witness
        // carries every describable element type through the layout recv
        // entries; only elements the witness provably cannot describe
        // (containers, handles, closures) fail closed here. Emit a
        // user-facing diagnostic rather than the ICE-flavoured "missing
        // runtime rewrite metadata" from require_builtin_runtime_symbol.
        let resolved_inner = self.subst.resolve(&inner);
        if !matches!(resolved_inner, Ty::Var(_)) && !self.queue_elem_admissible(&resolved_inner) {
            let reason = self.queue_elem_rejection_reason(&resolved_inner);
            self.report_error(
                TypeErrorKind::InvalidOperation,
                span,
                format!(
                    "`Stream<{}>` is not supported: {reason}",
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
            // `recv` parks until an item, EOF or a fault is ready; `try_recv`
            // never parks. Both ride the layout-witness entries for every
            // describable element type. `collect` drains a `Stream<string>`.
            "recv" | "try_recv" | "close" | "collect" => {
                if method == "collect" && resolved_inner != Ty::String {
                    self.report_error(
                        TypeErrorKind::InvalidOperation,
                        span,
                        format!(
                            "`Stream<{}>.collect` is not supported: `collect` drains a \
                             `Stream<string>`",
                            inner.user_facing()
                        ),
                    );
                    return Ty::Error;
                }
                let Some(c_symbol) = self.require_builtin_runtime_symbol(
                    span,
                    BuiltinNamedType::Stream.canonical_name(),
                    method,
                    crate::stdlib::resolve_stream_method(
                        BuiltinNamedType::Stream.canonical_name(),
                        method,
                    ),
                ) else {
                    return Ty::Error;
                };
                self.record_runtime_method_call_rewrite(span, c_symbol);
                sig.return_type
            }
            // The lazy adaptors: each consumes its source stream and returns a
            // fresh one. `lines` frames a `Stream<bytes>` into text, one line
            // per item; `chunks` re-frames bytes by size; `take` bounds any
            // stream.
            "lines" | "chunks" | "take" => {
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    if let Some(param_ty) = sig.params.first() {
                        self.check_against(expr, sp, param_ty);
                    }
                }
                if method != "take" && resolved_inner != Ty::Bytes {
                    self.report_error(
                        TypeErrorKind::InvalidOperation,
                        span,
                        format!(
                            "`Stream<{}>.{method}` is not supported: `{method}` frames a \
                             `Stream<bytes>`",
                            inner.user_facing()
                        ),
                    );
                    return Ty::Error;
                }
                let Some(c_symbol) = self.require_builtin_runtime_symbol(
                    span,
                    BuiltinNamedType::Stream.canonical_name(),
                    method,
                    crate::stdlib::resolve_stream_method(
                        BuiltinNamedType::Stream.canonical_name(),
                        method,
                    ),
                ) else {
                    return Ty::Error;
                };
                self.record_runtime_method_call_rewrite(span, c_symbol);
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

    /// Resolve a method call on `Ty::String` through the declarative
    /// `impl string` block declared in `std/string.hew`. Anything else —
    /// including user `impl MyTrait for string` dispatch — falls through to
    /// primitive-trait lookup so primitive-trait-impl metadata continues to be
    /// recorded for codegen.
    pub(in crate::check) fn dispatch_string_method(
        &mut self,
        method: &str,
        args: &[CallArg],
        span: &Span,
    ) -> Ty {
        if let Some(ret_ty) = self.dispatch_monomorphic_extern_symbol_method(
            "string",
            &[],
            method,
            args,
            span,
            &Ty::String,
        ) {
            return ret_ty;
        }
        self.check_primitive_receiver_method_fallback(&Ty::String, "string", method, args, span)
    }

    pub(super) fn check_hashset_element_arg(&mut self, elem_ty: &Ty, arg: &CallArg) -> bool {
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

    pub(super) fn dispatch_primitive_pattern_name(ty: &Ty) -> Option<&'static str> {
        Some(match ty {
            Ty::I8 => "i8",
            Ty::I16 => "i16",
            Ty::I32 | Ty::IntLiteral => "i32",
            Ty::I64 => "i64",
            Ty::U8 => "u8",
            Ty::U16 => "u16",
            Ty::U32 => "u32",
            Ty::U64 => "u64",
            Ty::Isize => "isize",
            Ty::Usize => "usize",
            Ty::F32 => "f32",
            Ty::F64 | Ty::FloatLiteral => "f64",
            Ty::Bool => "bool",
            Ty::Char => "char",
            Ty::String => "String",
            Ty::Bytes => "bytes",
            Ty::Duration => "duration",
            Ty::Unit => "()",
            Ty::Never => "!",
            Ty::CancellationToken => "CancellationToken",
            Ty::Var(_)
            | Ty::Tuple(_)
            | Ty::Array(_, _)
            | Ty::Slice(_)
            | Ty::Named { .. }
            | Ty::Function { .. }
            | Ty::Closure { .. }
            | Ty::Pointer { .. }
            | Ty::Borrow { .. }
            | Ty::TraitObject { .. }
            | Ty::Task(_)
            | Ty::AssocType { .. }
            | Ty::Error => return None,
        })
    }

    pub(super) fn dispatch_pattern_to_ty(&self, pattern: &TyPattern) -> Ty {
        match pattern {
            TyPattern::Primitive(name) => match name.as_str() {
                "i8" => Ty::I8,
                "i16" => Ty::I16,
                "i32" => Ty::I32,
                "i64" => Ty::I64,
                "u8" => Ty::U8,
                "u16" => Ty::U16,
                "u32" => Ty::U32,
                "u64" => Ty::U64,
                "isize" => Ty::Isize,
                "usize" => Ty::Usize,
                "f32" => Ty::F32,
                "f64" => Ty::F64,
                "bool" => Ty::Bool,
                "char" => Ty::Char,
                "String" => Ty::String,
                "bytes" => Ty::Bytes,
                "duration" => Ty::Duration,
                "()" => Ty::Unit,
                "!" => Ty::Never,
                // TRANSITION(A1 commit 3): the dispatch pattern carries the
                // receiver's spelling until the catalog move keys it by head.
                other if self.is_type_param_in_scope(other) => Ty::param(other),
                other => self.named_ty_for_key(other, vec![]),
            },
            TyPattern::App { ctor, args } => self.named_ty_for_key(
                ctor,
                args.iter()
                    .map(|arg| self.dispatch_pattern_to_ty(arg))
                    .collect(),
            ),
            TyPattern::Tuple(items) => Ty::Tuple(
                items
                    .iter()
                    .map(|item| self.dispatch_pattern_to_ty(item))
                    .collect(),
            ),
            TyPattern::Var(name) => Ty::param(name),
        }
    }

    pub(super) fn record_resolved_collection_call(
        &mut self,
        trait_name: &str,
        method: &str,
        receiver: &TyPattern,
        span: &Span,
    ) {
        // W4.001 Stage C3 (DI-017): the Stage-B `collection_dispatch_registry`
        // wrapper has retired; call the impl directly. Authority for HashMap /
        // HashSet method dispatch is now the resolver, with the result emitted
        // via `resolved_calls` (no parallel `method_call_rewrites` entry).
        let registry = collection_dispatch_registry_impl();
        let resolved =
            resolve_method_call(&registry, trait_name, method, receiver, &|marker, ty| {
                let ty = self.dispatch_pattern_to_ty(ty);
                self.collection_key_marker_available(&ty, marker)
            });
        match resolved {
            Ok(call) => {
                self.resolved_calls
                    .insert(SpanKey::in_module(span, self.current_module_idx), call);
            }
            Err(LookupError::BoundsNotSatisfied {
                unsatisfied,
                witness,
                ..
            }) => {
                // W4.001 Stage C3 hard cutover: the resolver is now the
                // sole admission authority for HashMap/HashSet dispatch.
                // An unsatisfied where-bound (e.g. `K: Hash` failing on
                // `f64`) becomes a user-facing `BoundsNotSatisfied`
                // diagnostic with attribution to the witness type.
                // `MethodCallNoRewrite` is permanently demoted to a
                // boundary-violation-only diagnostic.
                let witness_ty = self.dispatch_pattern_to_ty(&witness);
                let bound_summary = unsatisfied
                    .iter()
                    .map(|b| format!("{}: {}", b.var, b.trait_name))
                    .collect::<Vec<_>>()
                    .join(", ");
                self.report_error(
                    TypeErrorKind::BoundsNotSatisfied,
                    span,
                    format!(
                        "`{}` does not satisfy the required bounds for \
                         `{trait_name}.{method}` ({bound_summary})",
                        witness_ty.user_facing()
                    ),
                );
            }
            Err(LookupError::NoImpl { .. } | LookupError::UnknownMethod { .. }) => {
                // Unrecognised receiver shape or method — should not occur
                // because callers gate by ctor/method names matching the
                // registry. Emit a fail-closed `InvalidOperation` so any
                // future drift surfaces loudly rather than silently
                // skipping `resolved_calls` population.
                self.report_error(
                    TypeErrorKind::InvalidOperation,
                    span,
                    format!(
                        "internal compiler error: collection resolver could \
                         not locate `{trait_name}.{method}` for receiver \
                         `{receiver:?}`"
                    ),
                );
            }
        }
    }

    pub(in crate::check) fn record_resolved_hashmap_call(
        &mut self,
        method: &str,
        key_ty: &Ty,
        val_ty: &Ty,
        span: &Span,
    ) {
        let receiver = TyPattern::App {
            ctor: "HashMap".to_string(),
            args: vec![
                self.ty_to_dispatch_pattern(key_ty),
                self.ty_to_dispatch_pattern(val_ty),
            ],
        };
        if !self.is_hashmap_abstract_key_param(key_ty) {
            self.record_resolved_collection_call("Map", method, &receiver, span);
            return;
        }
        let key_param_name = self
            .hashmap_abstract_key_param_name(key_ty)
            .expect("abstract HashMap key param was checked above");

        let key_pattern = self.ty_to_dispatch_pattern(key_ty);
        let registry = collection_dispatch_registry_impl();
        let resolved = resolve_method_call(&registry, "Map", method, &receiver, &|marker, ty| {
            if *ty == key_pattern {
                return self.type_param_has_marker_bound(&key_param_name, marker);
            }
            let ty = self.dispatch_pattern_to_ty(ty);
            self.registry.implements_marker(&ty, marker)
        });
        match resolved {
            Ok(call) => {
                self.resolved_calls
                    .insert(SpanKey::in_module(span, self.current_module_idx), call);
            }
            Err(LookupError::BoundsNotSatisfied {
                unsatisfied,
                witness,
                ..
            }) => {
                let witness_ty = self.dispatch_pattern_to_ty(&witness);
                let bound_summary = unsatisfied
                    .iter()
                    .map(|b| format!("{}: {}", b.var, b.trait_name))
                    .collect::<Vec<_>>()
                    .join(", ");
                self.report_error(
                    TypeErrorKind::BoundsNotSatisfied,
                    span,
                    format!(
                        "`{}` does not satisfy the required bounds for \
                         `Map.{method}` ({bound_summary})",
                        witness_ty.user_facing()
                    ),
                );
            }
            Err(LookupError::NoImpl { .. } | LookupError::UnknownMethod { .. }) => {
                self.report_error(
                    TypeErrorKind::InvalidOperation,
                    span,
                    format!(
                        "internal compiler error: collection resolver could \
                         not locate `Map.{method}` for receiver `{receiver:?}`"
                    ),
                );
            }
        }
    }

    pub(super) fn hashmap_abstract_key_param_name(&self, key_ty: &Ty) -> Option<String> {
        match self.subst.resolve(key_ty).materialize_literal_defaults() {
            Ty::Named {
                head: crate::TypeHead::Param(param),
                args,
            } if args.is_empty() && self.is_type_param_in_scope(param.spelling.as_str()) => {
                Some(param.spelling.to_string())
            }
            _ => None,
        }
    }

    pub(super) fn is_hashmap_abstract_key_param(&self, key_ty: &Ty) -> bool {
        self.hashmap_abstract_key_param_name(key_ty).is_some()
    }

    pub(in crate::check) fn record_resolved_hashset_call(
        &mut self,
        method: &str,
        elem_ty: &Ty,
        span: &Span,
    ) {
        let receiver = TyPattern::App {
            ctor: "HashSet".to_string(),
            args: vec![self.ty_to_dispatch_pattern(elem_ty)],
        };
        self.record_resolved_collection_call("Set", method, &receiver, span);
    }

    /// Record one Vec resolved call, selecting its symbol through the shared
    /// source-derived Vec authority. Abstract element methods retain the
    /// registry's `_FAMILY` placeholder for MIR monomorphisation.
    pub(in crate::check) fn record_resolved_vec_call(
        &mut self,
        method: &str,
        elem_ty: &Ty,
        span: &Span,
    ) {
        let Some(vec_method) = VecMethod::from_name(method) else {
            return;
        };
        let elem_ty = self.subst.resolve(elem_ty).materialize_literal_defaults();
        let receiver = TyPattern::App {
            ctor: "Vec".to_string(),
            args: vec![self.ty_to_dispatch_pattern(&elem_ty)],
        };
        let key = SpanKey::in_module(span, self.current_module_idx);
        self.record_resolved_collection_call("Seq", method, &receiver, span);
        if !self.resolved_calls.contains_key(&key) {
            return;
        }
        // `append` gives the receiver its own copy of every source element, so
        // an element that owns a closure environment has nothing to copy: a
        // shallow buffer copy would leave two owners of one environment.
        if vec_method == VecMethod::Append
            && matches!(elem_ty, Ty::Function { .. } | Ty::Closure { .. })
        {
            self.report_vec_symbol_unsupported(
                vec_method,
                &elem_ty,
                crate::vec_authority::VecUnsupported::FunctionSharedCopy,
                span,
            );
            self.resolved_calls.remove(&key);
            return;
        }
        if let Some(op) = crate::VecValueOp::from_method(vec_method) {
            // Final value operations retain semantic identity. Element ABI
            // selection belongs to physical MIR after concrete type demand.
            self.resolved_calls
                .get_mut(&key)
                .expect("resolved Vec call")
                .method_target
                .symbol_name = crate::RuntimeCallFamily::Vector(op).c_symbol().to_string();
            return;
        }

        let is_abstract = self.vec_element_contains_abstract_type_param(&elem_ty);
        let is_copy_layout = self.vec_element_has_copy_layout(&elem_ty);
        let profile = crate::vec_authority::VecElementProfile {
            abi: crate::vec_authority::classify_element(&elem_ty, &self.type_defs),
            is_owned: self.element_owns_heap(&elem_ty),
            is_copy_layout,
            is_function_like: matches!(elem_ty, Ty::Function { .. } | Ty::Closure { .. }),
            is_abstract,
        };
        match crate::vec_authority::resolve_runtime_symbol(
            vec_method,
            profile,
            crate::vec_authority::VecResolutionContext::CheckerConcrete,
        ) {
            crate::vec_authority::VecSymbolResolution::Resolved(symbol_name) => {
                self.resolved_calls
                    .get_mut(&key)
                    .expect("collection resolver inserted Vec call before symbol override")
                    .method_target
                    .symbol_name = symbol_name;
            }
            crate::vec_authority::VecSymbolResolution::Deferred => {}
            crate::vec_authority::VecSymbolResolution::Unavailable => {
                self.resolved_calls.remove(&key);
            }
            crate::vec_authority::VecSymbolResolution::Unsupported(reason) => {
                self.report_vec_symbol_unsupported(vec_method, &elem_ty, reason, span);
                self.resolved_calls.remove(&key);
            }
        }
    }

    pub(super) fn report_vec_symbol_unsupported(
        &mut self,
        method: VecMethod,
        elem_ty: &Ty,
        reason: crate::vec_authority::VecUnsupported,
        span: &Span,
    ) {
        // A class-rule refusal keeps its own kind and message: the element has
        // no class at all, so naming the runtime symbol it would have selected
        // would blame the method for the declaration's limit.
        if let Some((kind, refusal)) = self.element_admission_refusal(elem_ty) {
            self.report_error(kind, span, refusal);
            return;
        }
        let message = match reason {
            crate::vec_authority::VecUnsupported::FunctionGet => {
                "`Vec.get` on a function/closure element is not supported under \
                 the `Option<T>` accessor model: the element owns a heap-boxed \
                 closure environment that the fresh-owner get choke point does \
                 not yet clone (tracked gap)"
                    .to_string()
            }
            crate::vec_authority::VecUnsupported::FunctionSharedCopy => format!(
                "`Vec.{}` is not supported for function-valued elements: each element \
                 owns its closure environment, and a shallow buffer copy would create \
                 two owners of one environment",
                method.name()
            ),
            crate::vec_authority::VecUnsupported::Layout {
                expected_symbol,
                bitcopy_supported,
            } => {
                if bitcopy_supported {
                    format!(
                        "`Vec.{}` on `{}` is not runtime-backed: its value class needs a copy \
                         this operation has no symbol for (runtime symbol \
                         `{expected_symbol}`)",
                        method.name(),
                        elem_ty.user_facing()
                    )
                } else {
                    format!(
                        "`Vec.{}` on layout-backed element type `{}` is not \
                         runtime-backed yet (runtime symbol `{expected_symbol}`); supported \
                         layout Vec methods are push/get/set/pop/remove/clone for Copy \
                         record/tuple elements",
                        method.name(),
                        elem_ty.user_facing()
                    )
                }
            }
        };
        self.report_error(TypeErrorKind::InvalidOperation, span, message);
    }

    /// True when `elem_ty` transitively references an in-scope type parameter —
    /// a bare `T`, or a composite that *contains* one (`W<T>`, `Option<T>`,
    /// `(T, i64)`, `Vec<T>`). Such an element cannot be classified to a concrete
    /// runtime symbol at check time: its owned-vs-plain / Copy-vs-heap verdict
    /// depends on the monomorphised argument, so eager resolution on the generic
    /// spine can pick a different ABI than the constructor codegen stamps for the
    /// concrete instantiation (the #2737 clone-thunk divergence — `W<T>`
    /// classified owned generically vs the plain all-scalar `W<i64>`). Marking it
    /// abstract routes the method through the per-monomorphisation re-resolver
    /// (MIR `resolve_polymorphic_vec_element_symbol`), which classifies the
    /// substituted element and stays congruent with the constructor by
    /// construction (`dedup-semantic-boundary`).
    pub(in crate::check) fn vec_element_contains_abstract_type_param(&self, elem_ty: &Ty) -> bool {
        match elem_ty {
            Ty::Named { head, args, .. } => {
                let name = head.registry_key();
                let builtin = head.builtin();
                (builtin.is_none() && args.is_empty() && self.is_type_param_in_scope(name))
                    || args
                        .iter()
                        .any(|a| self.vec_element_contains_abstract_type_param(a))
            }
            Ty::Tuple(elems) => elems
                .iter()
                .any(|e| self.vec_element_contains_abstract_type_param(e)),
            Ty::Array(inner, _) | Ty::Slice(inner) => {
                self.vec_element_contains_abstract_type_param(inner)
            }
            _ => false,
        }
    }

    /// Build the `Ty → VecElementToken` verdict table that MIR consults when
    /// re-resolving a `Vec<T>` element-typed method under a type parameter
    /// (#1929 Stage 1). Every concrete type observed as a generic call /
    /// record-init type-argument is classified through
    /// [`Self::classify_vec_generic_element`]; types absent from the result
    /// (non-`Copy`/owned layout, owned heap-handles, closures, nested
    /// collections, unresolved nominals) fail closed downstream.
    pub(in crate::check) fn build_vec_generic_element_abi(
        &self,
        call_type_args: &HashMap<SpanKey, Vec<Ty>>,
        record_init_type_args: &HashMap<SpanKey, Vec<Ty>>,
        type_defs: &HashMap<String, TypeDef>,
    ) -> HashMap<Ty, crate::vec_authority::VecElementToken> {
        let mut out = HashMap::new();
        for args in call_type_args
            .values()
            .chain(record_init_type_args.values())
        {
            for ty in args {
                if out.contains_key(ty) {
                    continue;
                }
                if let Some(token) = self.classify_vec_generic_element(ty, type_defs) {
                    out.insert(ty.clone(), token);
                }
            }
        }
        out
    }

    /// Classify a concrete element type's `Vec<T>` runtime ABI for the
    /// monomorphisation re-resolution path, using
    /// [`crate::vec_authority::classify_element`].
    ///
    /// Stage 1 admits exactly what already round-trips on the concrete path
    /// without new runtime/codegen machinery: scalar (`bool`/`i32`/`i64`/`f64`)
    /// and `string` elements unconditionally, and pointer / layout-descriptor
    /// elements **only when the element is `Copy`**. The `Copy` gate is what
    /// makes the pointer and layout arms safe: it admits identity handles
    /// (actor handles) and bit-copy value records, while deferring
    /// every shape with an ownership contract — owned heap-handles, non-`Copy`
    /// records, closures (each owns a captured environment), and nested
    /// collections (each owns a backing store). Those would alias an owner
    /// across a shallow `_ptr`/`_layout` op and double-free; they stay
    /// fail-closed until the owned generic path lands.
    pub(super) fn classify_vec_generic_element(
        &self,
        ty: &Ty,
        type_defs: &HashMap<String, TypeDef>,
    ) -> Option<crate::vec_authority::VecElementToken> {
        use crate::vec_authority::VecElementToken;
        let token = crate::vec_authority::classify_element(ty, type_defs)?;
        let admissible = match token {
            // Bit-copy scalars and the CoW `string` representation carry no
            // owner-aliasing hazard across the shared-buffer element ops. Every
            // integer width and both float widths route to a dedicated
            // runtime kernel, so the whole scalar set is unconditionally
            // admissible.
            VecElementToken::Bool
            | VecElementToken::I8
            | VecElementToken::U8
            | VecElementToken::I16
            | VecElementToken::U16
            | VecElementToken::I32
            | VecElementToken::I64
            | VecElementToken::F32
            | VecElementToken::F64
            | VecElementToken::Str => true,
            // Pointer-identity and layout-descriptor elements are admitted
            // only when `Copy` — see the doc comment for why.
            VecElementToken::Ptr | VecElementToken::Layout => {
                self.registry.implements_marker(ty, MarkerTrait::Copy)
            }
        };
        admissible.then_some(token)
    }

    /// Fail-closed gate for Vec pipeline methods whose elements cannot be
    /// copied into a second owner. Function slots own their closure-pair box;
    /// trait-object slots own their concrete `HeapBoxed` value. The pipeline
    /// desugar reads elements without consuming the source Vec, so neither can
    /// safely manufacture a result owner.
    /// Returns `true` when the call was rejected.
    pub(super) fn reject_vec_pipeline_fn_element(
        &mut self,
        method: &str,
        elem_ty: &Ty,
        span: &Span,
    ) -> bool {
        if matches!(elem_ty, Ty::TraitObject { .. }) {
            self.report_error(
                TypeErrorKind::InvalidOperation,
                span,
                format!(
                    "`Vec.{method}` is not supported for trait-object elements: \
                     the source Vec retains each HeapBoxed owner and `dyn Trait` has no \
                     semantic clone operation; consume the Vec with `into_iter()` instead"
                ),
            );
            return true;
        }
        if matches!(elem_ty, Ty::Function { .. } | Ty::Closure { .. }) {
            self.report_error(
                TypeErrorKind::InvalidOperation,
                span,
                format!(
                    "`Vec.{method}` is not supported for function-valued elements: \
                     each element owns its closure environment, and reading elements \
                     into a pipeline result would create a second owner of one \
                     environment"
                ),
            );
            return true;
        }
        false
    }

    /// Record the [`MethodCallRewrite::BuiltinVecHigherOrder`] entry that
    /// drives the HIR pipeline-loop expansion. Skipped (fail-closed: the call
    /// then dies at HIR with `MethodCallNoRewrite`) when either type fails
    /// boundary conversion — an unresolved inference hole here means the call
    /// site itself already carries a type diagnostic.
    pub(super) fn record_vec_higher_order_rewrite(
        &mut self,
        op: VecHigherOrderOp,
        elem_ty: &Ty,
        out_ty: &Ty,
        span: &Span,
    ) {
        // A place receiver must remain usable after the eager pipeline.
        // Pre-author the zero-width clone call that HIR inserts to give its
        // eval-once source binding an independent Vec owner. This is the same
        // element-aware clone authority used by `Vec.iter()`; non-place
        // receivers do not consume the unused side-table row.
        let clone_span = span.start..span.start;
        let vec_ty = self.make_vec_type(elem_ty.clone(), &clone_span);
        self.record_type(&clone_span, &vec_ty);
        self.record_resolved_vec_call("clone", elem_ty, &clone_span);
        let elem = ResolvedTy::from_ty(&elem_ty.clone().materialize_literal_defaults());
        let out = ResolvedTy::from_ty(&out_ty.clone().materialize_literal_defaults());
        if let (Ok(elem_ty), Ok(out_ty)) = (elem, out) {
            self.record_method_call_rewrite(
                span,
                MethodCallRewrite::BuiltinVecHigherOrder {
                    op,
                    elem_ty,
                    out_ty,
                },
            );
        }
    }

    /// Instantiate an [`ArgTemplate`] against the receiver's concrete types.
    pub(super) fn collection_arg_ty(template: ArgTemplate, cx: &CollectionTyCx) -> Ty {
        match template {
            ArgTemplate::Key => cx.key.clone(),
            ArgTemplate::Value => cx.val.clone(),
            ArgTemplate::Elem => cx.elem.clone(),
        }
    }

    /// Shared argument *walk*: check each supplied argument against its template
    /// type.  Returns `false` only when a per-collection arg hook signals an
    /// early `Ty::Error` (today: `HashSet`'s `check_hashset_element_arg`
    /// coercion).  Missing trailing arguments are skipped — the arity check (if
    /// any) is the sole authority for argument-count diagnostics, preserving the
    /// historical `if let Some(arg) = args.first()` behaviour.
    pub(super) fn check_collection_args(
        &mut self,
        kind: CollectionKind,
        templates: &[ArgTemplate],
        cx: &CollectionTyCx,
        args: &[CallArg],
        span: &Span,
    ) -> bool {
        let _ = span;
        for (i, template) in templates.iter().enumerate() {
            let Some(arg) = args.get(i) else {
                continue;
            };
            // HashSet element arguments go through the coercion hook (returns a
            // bool, early-returns `Ty::Error`) rather than a bare `check_against`.
            if kind == CollectionKind::HashSet && matches!(template, ArgTemplate::Elem) {
                if !self.check_hashset_element_arg(&cx.elem, arg) {
                    return false;
                }
                continue;
            }
            let expected = Self::collection_arg_ty(*template, cx);
            let (expr, sp) = arg.expr();
            self.check_against(expr, sp, &expected);
        }
        true
    }

    /// Returns `true` when `ty` is a concrete signed integer type strictly
    /// narrower than `i64` (i.e. `i8`, `i16`, or `i32`).  Used as the guard
    /// for implicit index-site widening — we do NOT widen unsigned types,
    /// float literals, or `IntLiteral` (integer literals are already accepted
    /// by the `check_against` literal-coercion arm).
    pub(in crate::check) fn is_narrower_signed_int(ty: &Ty) -> bool {
        matches!(ty, Ty::I8 | Ty::I16 | Ty::I32)
    }

    /// Construct a collection method's return type from its [`RetTemplate`].
    ///
    /// `VecOfKey`/`VecOfVal` route through `make_vec_type`, which itself
    /// validates the synthesized element type — this MUST run after the
    /// per-collection element validation hook (it does, because the driver calls
    /// this last), preserving the historical ordering of the `HashMap`
    /// `keys`/`values` arms.
    pub(super) fn collection_ret(
        &mut self,
        kind: CollectionKind,
        ret: RetTemplate,
        cx: &CollectionTyCx,
        span: &Span,
    ) -> Ty {
        match ret {
            RetTemplate::Unit => Ty::Unit,
            RetTemplate::Bool => Ty::Bool,
            RetTemplate::I64 => Ty::I64,
            RetTemplate::VecOfKey => self.make_vec_type(cx.key.clone(), span),
            RetTemplate::VecOfVal => self.make_vec_type(cx.val.clone(), span),
            RetTemplate::VecOfElem => self.make_vec_type(cx.elem.clone(), span),
            RetTemplate::VecOfPair => {
                self.make_vec_type(Ty::Tuple(vec![cx.key.clone(), cx.val.clone()]), span)
            }
            RetTemplate::SelfTy => match kind {
                CollectionKind::HashMap => Ty::Named {
                    head: crate::TypeHead::Builtin(BuiltinType::HashMap),
                    args: vec![cx.key.clone(), cx.val.clone()],
                },
                CollectionKind::HashSet => Ty::Named {
                    head: crate::TypeHead::Builtin(BuiltinType::HashSet),
                    args: vec![cx.elem.clone()],
                },
            },
        }
    }

    /// Code-side hook dispatch for the genuinely divergent per-collection
    /// admission policy: element validation, the `HashSet` lowering fact, and
    /// `ResolvedCall` recording.  This is the "do not centralise the decision"
    /// half of the refactor — the validators/recorders stay as separate
    /// functions; this only selects which named hook each `(kind, method)` runs,
    /// in one place instead of three mirrored resolvers.
    ///
    /// Returns `false` (→ caller emits `Ty::Error`) when an element validator
    /// rejects the call.  The Vec `reject_rc` hook deliberately does NOT
    /// short-circuit (matching the historical fire-and-continue behaviour).
    pub(super) fn run_collection_admission(
        &mut self,
        kind: CollectionKind,
        method: &str,
        cx: &CollectionTyCx,
        span: &Span,
    ) -> bool {
        match kind {
            CollectionKind::HashMap => {
                // Owned-vs-key_value validator split (deliberate per-arm asymmetry).
                let validated = match method {
                    "insert" | "get" | "remove" | "keys" | "values" | "entries" => {
                        self.validate_hashmap_owned_element_types(&cx.key, &cx.val, span)
                    }
                    _ => self.validate_hashmap_key_value_types(&cx.key, &cx.val, span),
                };
                if !validated {
                    return false;
                }
                // Only the operations that copy a value out of the map need a
                // value clone; `get` borrows and `remove` moves.
                if matches!(method, "values" | "entries" | "clone")
                    && !self.validate_collection_value_clone_type(
                        &cx.val,
                        BuiltinType::HashMap,
                        &format!("HashMap.{method}()"),
                        span,
                    )
                {
                    return false;
                }
                if matches!(
                    method,
                    "insert"
                        | "get"
                        | "remove"
                        | "contains_key"
                        | "len"
                        | "is_empty"
                        | "keys"
                        | "values"
                        | "entries"
                        | "clone"
                        | "clear"
                ) {
                    self.record_resolved_hashmap_call(method, &cx.key, &cx.val, span);
                }
            }
            CollectionKind::HashSet => {
                // Owned (insert) vs plain (rest) validator split.
                let validated = match method {
                    "insert" => self.validate_hashset_owned_element_type(&cx.elem, span),
                    _ => self.validate_hashset_element_type(&cx.elem, span),
                };
                if !validated {
                    return false;
                }
                if method == "to_vec"
                    && !self.validate_collection_value_clone_type(
                        &cx.elem,
                        BuiltinType::HashSet,
                        &format!("HashSet.{method}()"),
                        span,
                    )
                {
                    return false;
                }
                // Every known HashSet arm records a lowering fact (HashMap/Vec
                // do not) — a genuine per-collection hook.
                self.record_hashset_lowering_fact(span, &cx.elem);
                if matches!(
                    method,
                    "insert"
                        | "contains"
                        | "remove"
                        | "len"
                        | "is_empty"
                        | "clone"
                        | "clear"
                        | "to_vec"
                ) {
                    self.record_resolved_hashset_call(method, &cx.elem, span);
                }
            }
        }
        true
    }

    /// Fail-closed fallback for an unknown collection method: try a user
    /// `impl Trait for <collection>` body, then synthesize the arguments and
    /// emit the per-collection `no method `{m}` on {Collection}` diagnostic.
    pub(super) fn collection_method_fallback(
        &mut self,
        kind: CollectionKind,
        cx: &CollectionTyCx,
        method: &str,
        args: &[CallArg],
        span: &Span,
    ) -> Ty {
        // Reconstruct the receiver carrying its concrete type arguments so a
        // user `impl<T> Trait for HashMap<K, V>` / `Vec<E>` dispatched here can
        // bind the impl's type parameters from the element/key/value types and
        // project its `Output`/return — the builtin-vs-user asymmetry fix.
        let receiver = cx.receiver_with_args(kind);
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
            format!("no method `{method}` on {}", kind.name()),
        );
        Ty::Error
    }

    /// The single descriptor-driven front-half admission authority for builtin
    /// collection method calls, replacing the three mirrored resolvers.
    ///
    /// Flow (preserving the historical per-arm ordering exactly):
    /// arity → arguments → element validation / lowering / recording → return.
    /// Unknown / genuinely divergent methods (those absent from
    /// [`collection_method_desc`]) fall through to the fail-closed fallback; the
    /// Vec-specific `contains`/`map`/`filter`/`fold`/`join` arms and the
    /// structural-array guard are handled by `check_vec_method` before it
    /// delegates here.
    pub(in crate::check) fn check_collection_method(
        &mut self,
        kind: CollectionKind,
        cx: &CollectionTyCx,
        method: &str,
        args: &[CallArg],
        span: &Span,
    ) -> Ty {
        let Some(desc) = collection_method_desc(kind, method) else {
            return self.collection_method_fallback(kind, cx, method, args, span);
        };
        if let Some(arity) = desc.arity {
            self.check_arity(args, arity, &format!("`{}.{method}`", kind.name()), span);
        }
        if !self.check_collection_args(kind, desc.arg_templates, cx, args, span) {
            return Ty::Error;
        }
        if !self.run_collection_admission(kind, method, cx, span) {
            return Ty::Error;
        }
        self.collection_ret(kind, desc.ret, cx, span)
    }

    /// Resolve the per-call-site `ResolvedCall` for HashMap/HashSet via the
    /// registry, populate `resolved_calls`, and surface user-facing
    /// diagnostics on resolver failure.
    ///
    /// After W4.001 Stage C3 this is the sole admission authority for
    /// HashMap/HashSet method dispatch — the per-V symbol-selection
    /// helpers (`resolve_hashmap_runtime_symbol` / `_hashset_`) and the
    /// dual-emit `MethodCallRewrite::RewriteToFunction` arms have retired.
    /// Unsatisfied `where`-bounds (e.g. `HashMap<f64, _>` failing
    /// `K: Hash`) emit `TypeErrorKind::BoundsNotSatisfied` with attribution
    /// to the witness type; missing impls emit `InvalidOperation`.
    pub(in crate::check) fn check_hashmap_method(
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
        // Trait-routed `Index<K>` accessor: `<HashMap<K, V> as Index>::get
        // -> Option<V>` — the map twin of `Vec::get`. Dispatch is marked as
        // the `Index` primitive-trait impl and records the
        // `hew_hashmap_get_layout` resolved call, which codegen rewrites to
        // the SINGLE fresh-owner clone choke (`hew_hashmap_get_clone_layout`)
        // that writes a retained/cloned owner into the `Option` payload
        // (drop-safe; `by-value-heap-params-are-borrows` P0). `get` is
        // intentionally NOT in `collection_method_desc`: this explicit arm
        // keeps the accessor model uniform with `Vec`, while the trapping
        // `m[k]` read is the sibling `Index::at` (`-> V`).
        if method == "get" {
            self.check_arity(args, 1, "`HashMap.get`", span);
            if let Some(arg) = args.first() {
                let (expr, sp) = arg.expr();
                self.check_against(expr, sp, &key_ty);
            }
            // Enforce `K: Hash + Eq` and reject unsafe key/value element
            // types — the same admission the table-driven path ran for
            // `get` (fire-and-return on rejection).
            if !self.validate_hashmap_owned_element_types(&key_ty, &val_ty, span) {
                return Ty::Error;
            }
            self.record_method_call_receiver_kind(
                span,
                MethodCallReceiverKind::PrimitiveTraitImpl {
                    trait_name: "Index".to_string(),
                    canonical_receiver: "HashMap".to_string(),
                },
            );
            // D432: a value with no clone is read as a loan of the slot the
            // map still owns, so `Some` carries the loan and the owning
            // removal stays the way to move a value out.
            let resolved_val = self.subst.resolve(&val_ty);
            let Some(mode) = self.vec_iteration_element_mode(&resolved_val, span) else {
                return Ty::Error;
            };
            // Records the `Map::get` resolved call. `<HashMap<K, V> as
            // Index>::Output` is `V`, so the projected return is `Option<V>`.
            self.record_resolved_hashmap_call("get", &key_ty, &val_ty, span);
            if mode == super::types::VecIterationMode::Borrow {
                let key = SpanKey::in_module(span, self.current_module_idx);
                self.borrowed_element_option_reads.insert(key.clone());
                if let Some(call) = self.resolved_calls.get_mut(&key) {
                    call.method_target.symbol_name =
                        crate::RuntimeCallFamily::Map(crate::runtime_call::MapValueOp::GetBorrow)
                            .c_symbol()
                            .to_string();
                }
            }
            return Ty::option(val_ty);
        }
        // `HashMap::remove(k) -> Option<V>` (A233): the removing twin of `get`.
        // Handled here (not in the descriptor table) for the same reason as
        // `get` — the `Option<V>` projection plus resolved-call recording is a
        // code hook. Records the `Map::remove` resolved call, which resolves to
        // the `hew_hashmap_remove_take_layout` move-out kernel (drop the key,
        // MOVE the value out into the `Some` payload; drop-safe — the map keeps
        // no copy, so exactly one owner of V). `remove(absent)` yields `None`.
        if method == "remove" {
            self.check_arity(args, 1, "`HashMap.remove`", span);
            if let Some(arg) = args.first() {
                let (expr, sp) = arg.expr();
                self.check_against(expr, sp, &key_ty);
            }
            // Enforce `K: Hash + Eq` and reject unsafe key/value element types —
            // the same admission `get` runs (fire-and-return on rejection).
            if !self.validate_hashmap_owned_element_types(&key_ty, &val_ty, span) {
                return Ty::Error;
            }
            self.record_resolved_hashmap_call("remove", &key_ty, &val_ty, span);
            return Ty::option(val_ty);
        }
        // `into_iter` resolves to a `HashMapIter<K, V>` cursor so the pipeline
        // form (`iter::map(m.into_iter(), ..)`) matches `Vec::into_iter` — the
        // map twin of the `check_vec_method` `into_iter` arm. The cursor is
        // built (in HIR) from `keys()` / `values()` snapshots, the same proven
        // clone-on-read path the `for (k, v) in m` desugar uses, so record both
        // projection facts here: zero-width synthetic spans at the call's
        // start/end offsets, matching the for-in span derivation and reproduced
        // byte-for-byte by the HIR rewrite. A standalone `impl IntoIterator for
        // HashMap` is intentionally absent — its body would project on an
        // abstract receiver the checker cannot admit (see std/builtins.hew).
        if method == "into_iter" {
            self.check_arity(args, 0, "`HashMap.into_iter`", span);
            if !self.validate_collection_value_clone_type(
                &val_ty,
                BuiltinType::HashMap,
                "HashMap.into_iter()",
                span,
            ) || !self.validate_hashmap_owned_element_types(&key_ty, &val_ty, span)
            {
                return Ty::Error;
            }
            let keys_span = span.start..span.start;
            let values_span = span.end..span.end;
            let key_vec = self.make_vec_type(key_ty.clone(), &keys_span);
            let val_vec = self.make_vec_type(val_ty.clone(), &values_span);
            self.record_type(&keys_span, &key_vec);
            self.record_type(&values_span, &val_vec);
            self.record_resolved_hashmap_call("keys", &key_ty, &val_ty, &keys_span);
            self.record_resolved_hashmap_call("values", &key_ty, &val_ty, &values_span);
            let resolved_key = self.subst.resolve(&key_ty);
            let resolved_val = self.subst.resolve(&val_ty);
            if let (Ok(key_resolved), Ok(val_resolved)) = (
                ResolvedTy::from_ty(&resolved_key),
                ResolvedTy::from_ty(&resolved_val),
            ) {
                self.record_method_call_receiver_kind(
                    span,
                    MethodCallReceiverKind::PrimitiveTraitImpl {
                        trait_name: "IntoIterator".to_string(),
                        canonical_receiver: "HashMap".to_string(),
                    },
                );
                self.record_method_call_rewrite(
                    span,
                    MethodCallRewrite::BuiltinHashMapIntoIter {
                        key_ty: key_resolved,
                        val_ty: val_resolved,
                    },
                );
            }
            return Ty::builtin_named(BuiltinType::HashMapIter, vec![resolved_key, resolved_val]);
        }
        let cx = CollectionTyCx::hashmap(key_ty, val_ty);
        self.check_collection_method(CollectionKind::HashMap, &cx, method, args, span)
    }

    pub(in crate::check) fn check_hashset_method(
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
        let cx = CollectionTyCx::hashset(elem_ty);
        self.check_collection_method(CollectionKind::HashSet, &cx, method, args, span)
    }

    pub(super) fn report_vec_contains_layout_equality_gate(
        &mut self,
        elem_ty: &Ty,
        eligibility: crate::eq_eligibility::EqEligibility,
        span: &Span,
    ) {
        use crate::eq_eligibility::EqEligibility;

        let reason = match eligibility {
            EqEligibility::Eligible => format!(
                "`Vec.contains` on layout-backed element type `{}` is equality-eligible, \
                 but layout contains is not yet supported for this element type",
                elem_ty.user_facing()
            ),
            EqEligibility::IneligibleManaged(managed_ty) => format!(
                "`Vec.contains` on layout-backed element type `{}` requires aggregate \
                 equality, but `{}` is layout-managed/non-Copy data",
                elem_ty.user_facing(),
                managed_ty.user_facing()
            ),
            EqEligibility::IneligibleOwned(owned_ty) => format!(
                "`Vec.contains` on layout-backed element type `{}` requires aggregate \
                 equality, but `{}` is owned or heap-backed data",
                elem_ty.user_facing(),
                owned_ty.user_facing()
            ),
            EqEligibility::IneligibleUnknown => format!(
                "`Vec.contains` on layout-backed element type `{}` requires aggregate \
                 equality, but equality eligibility is unknown",
                elem_ty.user_facing()
            ),
        };

        self.report_error(
            TypeErrorKind::InvalidOperation,
            span,
            format!(
                "{reason}; no runtime method rewrite was recorded, and layout Vec contains \
                 remains fail-closed"
            ),
        );
    }

    /// Channel/stream element admission for the layout-witness queue path
    /// (`Sender<T>`/`Receiver<T>`/`Stream<T>` recv/send). An element is
    /// admissible when the codegen element witness can describe it:
    ///
    /// - `string` / `bytes` — content-encoded queue envelopes;
    /// - Copy-eligible primitives and `BitCopy` records ([`primitive_copy_layout`]
    ///   resolves a fixed width) — Plain raw-representation envelopes;
    /// - heap-owning value types the §1.1 class rule gives an ownership
    ///   obligation ([`Checker::element_owns_heap`] — the same class the
    ///   backend reads for the element's clone and destroy actions, so the
    ///   checker and the witness cannot disagree about one element type);
    /// - monomorphic machine values — machines are tagged-union value types
    ///   whose state-variant layout is registered in `type_defs.variants`,
    ///   so the owned-element queue witness can describe them (with the same
    ///   no-unowned-container requirement as enum channel elements).
    ///   Generic machine instantiations are excluded (the substrate
    ///   canonicalizes to one bare-named layout; per-instantiation witnesses
    ///   do not exist).
    ///
    /// Everything else fails closed: builtin container/handle nominals
    /// (`Vec`/`HashMap`/streams/channels/pids), closures, and any type
    /// without a clone/drop thunk path. `BitCopy` enums ride the
    /// owned-element authority's record/enum admission and are lowered
    /// Plain by the witness (no heap leaf → no thunks), which is the
    /// correct Copy semantics.
    pub(in crate::check) fn queue_elem_admissible(&self, elem_ty: &Ty) -> bool {
        match elem_ty {
            // String/bytes are content-encoded envelopes; unconstrained
            // numeric literals default to i64/f64 (Plain 8-byte envelopes)
            // at literal-defaulting time, so a queue element constrained
            // only by a literal (`tx.send(42)`) must not be rejected
            // before defaulting runs.
            Ty::String | Ty::Bytes | Ty::IntLiteral | Ty::FloatLiteral => true,
            // Monomorphic machine values travel the owned-element queue
            // witness: machines are tagged-union value types registered in
            // `type_defs.variants`, satisfying the same thunk-path
            // requirements as enums. Generic machine instantiations are
            // refused (canonicalised to one bare-named decl layout; no
            // per-instantiation witness exists).
            Ty::Named { head, args } if head.builtin().is_none() => {
                let name = head.registry_key();
                if let Some(type_def) = self.type_defs.get(name) {
                    if matches!(type_def.kind, TypeDefKind::Machine) {
                        // Generic instantiation: no per-instantiation layout.
                        if !args.is_empty() {
                            return false;
                        }
                        // Monomorphic: apply the same no-unowned-container
                        // requirement as for enum channel elements.
                        return !self.queue_element_holds_collection(
                            elem_ty,
                            &HashSet::new(),
                            &mut HashSet::new(),
                        );
                    }
                }
                self.queue_element_describable(elem_ty)
            }
            // Builtin container/handle nominals (`Vec`/`HashMap`/`HashSet`/
            // `Rc`/handles/...) can never ride the element-layout queue
            // witness: their ownership lives in a runtime context the queue
            // cannot clone or drop. This stays in lockstep with
            // `queue_elem_rejection_reason`, which rejects every `builtin:
            // Some(_)`. A nested-container Vec ELEMENT is admitted for
            // copy-in push, but that is a Vec-storage property, not a queue
            // property, and must not leak here. Primitives (`i64`/`bool`/`char`/...) are dedicated `Ty`
            // variants (not `Ty::Named`), so they remain queue-admissible via
            // the `_` arm's `primitive_copy_layout` check.
            //
            // A callable owns a heap environment the envelope has no ingress
            // for, which `queue_elem_rejection_reason` states in the same
            // words; every other shape is admitted on its value class.
            Ty::Named {
                head: crate::TypeHead::Builtin(_) | crate::TypeHead::Actor(_),
                ..
            }
            | Ty::Function { .. }
            | Ty::Closure { .. } => false,
            _ => self.queue_element_describable(elem_ty),
        }
    }

    /// Can the element-layout queue witness describe this element?
    ///
    /// It can when the element has a value class at all — the class carries the
    /// clone and destroy actions the envelope needs — and holds no builtin
    /// collection. A type with no class (an abstract parameter among them) has
    /// no witness either and stays fail-closed here; the collection rule is the
    /// mailbox's own, stated on [`Self::queue_element_holds_collection`].
    pub(super) fn queue_element_describable(&self, elem_ty: &Ty) -> bool {
        self.element_value_facts(elem_ty).is_ok()
            && !self.queue_element_holds_collection(elem_ty, &HashSet::new(), &mut HashSet::new())
    }

    /// Explain why a channel/stream element type was rejected by
    /// [`Self::queue_elem_admissible`], for the fail-closed diagnostic.
    /// Completes "`{Container}<X>` is not supported: {clause}".
    pub(in crate::check) fn queue_elem_rejection_reason(&self, elem_ty: &Ty) -> String {
        if let Ty::Named {
            head: crate::TypeHead::Builtin(_) | crate::TypeHead::Actor(_),
            ..
        } = elem_ty
        {
            return "builtin container and handle types cannot ride the \
                    element-layout queue witness; their ownership lives in a \
                    runtime context the queue cannot clone or drop"
                .to_string();
        }
        if matches!(elem_ty, Ty::Function { .. } | Ty::Closure { .. }) {
            return "function values cannot be queue elements".to_string();
        }
        if self.queue_element_holds_collection(elem_ty, &HashSet::new(), &mut HashSet::new()) {
            return "it holds a `Vec`/`HashMap`/`HashSet` field, and the mailbox envelope \
                    has no per-message release for one"
                .to_string();
        }
        self.element_admission_refusal(elem_ty).map_or_else(
            || "it has no value class the queue witness can describe".to_string(),
            |(_, refusal)| refusal,
        )
    }

    /// True when `ty` (or a transitive record/enum member) is — or holds a
    /// field of — a builtin collection (`Vec`/`HashMap`/`HashSet`).
    ///
    /// This is a MAILBOX rule, not a value-class one: the envelope deep-copies
    /// an element in but has no per-message release that recurses through a
    /// collection field, so a collection-bearing message would leak the field
    /// on every send. Vec storage admits the same shape (the collection's own
    /// destroy action releases it); the queue does not, until the mailbox
    /// release path recurses. The recursive enum's own self-edge through a
    /// `Vec` (`Array(Vec<RedisReply>)`) is the one admitted exception.
    pub(super) fn queue_element_holds_collection(
        &self,
        ty: &Ty,
        roots: &HashSet<String>,
        visiting: &mut HashSet<String>,
    ) -> bool {
        match ty {
            Ty::Named { head, args, .. } => {
                let name = head.registry_key();
                let builtin = head.builtin();
                if matches!(
                    builtin,
                    Some(BuiltinType::Vec | BuiltinType::HashMap | BuiltinType::HashSet)
                ) {
                    // A collection whose every type argument is a `root` (the
                    // recursing element type) is the admitted self-recursion
                    // (`enum R { A(Vec<R>) }`): the enum's own owned thunk
                    // recurses through this field. Any other container element
                    // is unowned (no thunk path) — reject.
                    return !args.iter().all(|a| match a {
                        Ty::Named { head, .. } => {
                            roots.iter().any(|root| root == head.registry_key())
                        }
                        _ => false,
                    });
                }
                if builtin.is_some() {
                    // Other builtins (Option/Result/Rc/handles) carry their own
                    // ABI; recurse only through their type arguments.
                    return args
                        .iter()
                        .any(|a| self.queue_element_holds_collection(a, roots, visiting));
                }
                if !visiting.insert(name.to_string()) {
                    // Self-recursive edge on a user type: the recursion through a
                    // user record/enum is finite by construction here (it only
                    // recurses once per name). It carries no bare container.
                    return false;
                }
                let result = self.type_defs.get(name).is_some_and(|td| {
                    td.fields
                        .values()
                        .any(|fty| self.queue_element_holds_collection(fty, roots, visiting))
                        || td.variants.values().any(|variant| match variant {
                            VariantDef::Unit => false,
                            VariantDef::Tuple(tys) => tys
                                .iter()
                                .any(|t| self.queue_element_holds_collection(t, roots, visiting)),
                            VariantDef::Struct(fields) => fields.iter().any(|(_, t)| {
                                self.queue_element_holds_collection(t, roots, visiting)
                            }),
                        })
                });
                visiting.remove(name);
                result
            }
            Ty::Tuple(elems) => elems
                .iter()
                .any(|e| self.queue_element_holds_collection(e, roots, visiting)),
            Ty::Array(inner, _) | Ty::Slice(inner) => {
                self.queue_element_holds_collection(inner, roots, visiting)
            }
            _ => false,
        }
    }

    pub(super) fn check_runtime_vec_method_from_source(
        &mut self,
        type_args: &[Ty],
        method: &str,
        args: &[CallArg],
        span: &Span,
    ) -> Option<Ty> {
        let sig = self.lookup_builtin_vec_method_sig(type_args, method)?;
        sig.extern_symbol.as_ref()?;

        if matches!(method, "push" | "pop" | "remove" | "clear" | "clone") {
            self.check_arity(args, sig.params.len(), &format!("`Vec.{method}`"), span);
        }

        for (index, expected) in sig.params.iter().enumerate() {
            let Some(arg) = args.get(index) else {
                continue;
            };
            let (expr, arg_span) = arg.expr();
            if matches!(method, "set" | "remove") && index == 0 {
                let actual = self.synthesize(expr, arg_span);
                let resolved = self.subst.resolve(&actual);
                if Self::is_narrower_signed_int(&resolved) {
                    self.numeric_operand_coercions.insert(
                        SpanKey::in_module(arg_span, self.current_module_idx),
                        Ty::I64,
                    );
                    continue;
                }
            }
            self.check_against(expr, arg_span, expected);
            // A value with no copy operation enters the collection by
            // transfer: the slot becomes its only owner, so a later use of the
            // source binding is a use after the move.
            self.record_value_transfer(expr, arg_span);
        }

        let elem_ty = type_args
            .first()
            .map_or_else(|| Ty::Var(TypeVar::fresh()), |ty| self.subst.resolve(ty));
        self.record_resolved_vec_call(method, &elem_ty, span);
        Some(sig.return_type)
    }

    #[expect(
        clippy::too_many_lines,
        reason = "Vec keeps divergent contains/join/map/filter/fold arms inline; runtime-backed signatures delegate to the stdlib-source authority."
    )]
    pub(in crate::check) fn check_vec_method(
        &mut self,
        type_args: &[Ty],
        _receiver_ty: &Ty,
        resolved: &Ty,
        method: &str,
        args: &[CallArg],
        span: &Span,
    ) -> Ty {
        let elem_ty = type_args
            .first()
            .cloned()
            .unwrap_or(Ty::Var(TypeVar::fresh()));
        let _ = self.validate_vec_element_type(&elem_ty, span);
        let runtime_method_declared = self
            .lookup_builtin_vec_method_sig(type_args, method)
            .is_some();
        if method == "clone" && matches!(self.subst.resolve(&elem_ty), Ty::TraitObject { .. }) {
            self.check_arity(args, 0, "`Vec.clone`", span);
            self.report_error(
                TypeErrorKind::InvalidOperation,
                span,
                "`Vec<dyn Trait>.clone()` is not supported because trait objects have no \
                 semantic clone operation; use `into_iter()` to transfer the existing owners"
                    .to_string(),
            );
            return Ty::Error;
        }
        // A pipe half is cloned one handle at a time: `sink.clone()` retains
        // the pipe's handle count, and no Vec clone recipe duplicates one.
        if method == "clone"
            && matches!(
                self.subst.resolve(&elem_ty),
                Ty::Named { head: crate::TypeHead::Builtin(builtin), .. } if builtin.is_pipe_half()
            )
        {
            self.check_arity(args, 0, "`Vec.clone`", span);
            self.report_error(
                TypeErrorKind::InvalidOperation,
                span,
                format!(
                    "`Vec<{}>.clone()` is not supported: a pipe half is cloned one handle at a \
                     time with `.clone()` on a `Sink`, and a `Stream` has one consumer",
                    elem_ty.user_facing()
                ),
            );
            return Ty::Error;
        }
        let result = match method {
            "into_iter" => {
                self.check_arity(args, 0, "`Vec.into_iter`", span);
                let resolved_elem = self.subst.resolve(&elem_ty);
                if !self.record_vec_iter_element_mode(&resolved_elem, span) {
                    return Ty::Error;
                }
                self.record_method_call_receiver_kind(
                    span,
                    MethodCallReceiverKind::PrimitiveTraitImpl {
                        trait_name: "IntoIterator".to_string(),
                        canonical_receiver: "Vec".to_string(),
                    },
                );
                self.record_method_call_rewrite(span, MethodCallRewrite::BuiltinVecIntoIter);
                Ty::builtin_named(BuiltinType::VecIter, vec![resolved_elem])
            }
            "iter" => {
                // An ordinary value transfer creates the independent snapshot.
                // Record the operation now and consume the finalized receiver
                // type at the HIR boundary, after inference has completed.
                self.check_arity(args, 0, "`Vec.iter`", span);
                let resolved_elem = self.subst.resolve(&elem_ty);
                if matches!(resolved_elem, Ty::TraitObject { .. }) {
                    self.report_error(
                        TypeErrorKind::InvalidOperation,
                        span,
                        "`Vec<dyn Trait>.iter()` is not supported because a borrowed iterator \
                         needs an independent clone of each trait object; use `into_iter()` to \
                         transfer the existing owners"
                            .to_string(),
                    );
                    return Ty::Error;
                }
                if !self.record_vec_iter_element_mode(&resolved_elem, span) {
                    return Ty::Error;
                }
                self.record_method_call_rewrite(span, MethodCallRewrite::BuiltinVecIter);
                Ty::builtin_named(BuiltinType::VecIter, vec![resolved_elem])
            }
            "get" if runtime_method_declared => {
                // Trait-routed `Index<i64>` accessor: `<Vec<T> as Index>::get
                // -> Option<T>`. Dispatch is marked as the `Index` primitive
                // trait impl and lowered to the element-agnostic
                // `hew_vec_get_clone` intrinsic — the SINGLE fresh-owner choke
                // point that writes a retained/cloned owner into the `Option`
                // payload (drop-safe; `by-value-heap-params-are-borrows` P0).
                // `get` is intentionally NOT in `collection_method_desc`: this
                // explicit arm preserves the index-site widening ergonomic
                // (i8/i16/i32 → i64) that the strict trait signature would
                // otherwise reject.
                self.check_arity(args, 1, "`Vec.get`", span);
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    let actual = self.synthesize(expr, sp);
                    let resolved_idx = self.subst.resolve(&actual);
                    // Accept i64 or any narrower signed int (widened at the
                    // index site, identical to `[]`/`set`/`remove`); otherwise
                    // run the normal i64 coercion (literals, error wording).
                    if Self::is_narrower_signed_int(&resolved_idx) {
                        self.numeric_operand_coercions
                            .insert(SpanKey::in_module(sp, self.current_module_idx), Ty::I64);
                    } else {
                        self.check_against(expr, sp, &Ty::I64);
                    }
                }
                let resolved_elem = self.subst.resolve(&elem_ty);
                // A trait object stays refused: dispatching on a loaned `dyn`
                // payload has no working lowering yet, so the consuming
                // iterator remains the trait-object surface.
                if matches!(resolved_elem, Ty::TraitObject { .. }) {
                    self.report_error(
                        TypeErrorKind::InvalidOperation,
                        span,
                        "`Vec<dyn Trait>.get()` is not supported because returning an owned \
                         element would require a semantic trait-object clone; use pop/remove or \
                         consuming iteration to move an owner out"
                            .to_string(),
                    );
                    return Ty::Error;
                }
                self.record_method_call_receiver_kind(
                    span,
                    MethodCallReceiverKind::PrimitiveTraitImpl {
                        trait_name: "Index".to_string(),
                        canonical_receiver: "Vec".to_string(),
                    },
                );
                // `get` copies the element out, so a concrete element with no
                // copy operation refuses here exactly as it refuses at `v[i]`
                // and at a range slice (#3395). An unbounded type parameter is
                // not concrete: a generic body keeps the borrowed read every
                // instantiation shares.
                if !self.validate_vec_get_element_clone_type(&resolved_elem, span) {
                    return Ty::Error;
                }
                // D432: an abstract element is read as a loan of the slot the
                // vector still owns, so `Some` carries the loan and the owning
                // removal stays the way to move an element out.
                let Some(mode) = self.vec_iteration_element_mode(&resolved_elem, span) else {
                    return Ty::Error;
                };
                // Records the resolved call through the shared Vec authority.
                self.record_resolved_vec_call("get", &resolved_elem, span);
                if mode == super::types::VecIterationMode::Borrow {
                    let key = SpanKey::in_module(span, self.current_module_idx);
                    self.borrowed_element_option_reads.insert(key);
                    if let Some(call) = self
                        .resolved_calls
                        .get_mut(&SpanKey::in_module(span, self.current_module_idx))
                    {
                        call.method_target.symbol_name =
                            crate::RuntimeCallFamily::Vector(crate::VecValueOp::GetBorrow)
                                .c_symbol()
                                .to_string();
                    }
                }
                // `<Vec<T> as Index>::Output` is `T`, so the projected return
                // is `Option<T>`.
                Ty::option(resolved_elem)
            }
            "contains" if runtime_method_declared => {
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    self.check_against(expr, sp, &elem_ty);
                }
                let resolved_elem = self.subst.resolve(&elem_ty);
                if crate::vec_authority::classify_element(&resolved_elem, &self.type_defs)
                    == Some(crate::vec_authority::VecElementToken::Layout)
                {
                    // W3.032 Slice 3e: lift the layout gate for equality-
                    // eligible Copy records/tuples.  Authority chain: the
                    // checker is the sole arbiter; HIR/MIR/codegen treat the
                    // recorded `"hew_vec_contains_thunk"` symbol string as an
                    // opaque eligibility certificate and do NOT re-derive
                    // eligibility (see W3.032 plan §"Checker authority
                    // carry").
                    let eligibility =
                        crate::eq_eligibility::ty_is_eq_eligible(&resolved_elem, &self.type_defs);
                    let is_copy = self.vec_element_has_copy_layout(&resolved_elem);
                    let is_owned_admissible = self.element_owns_heap(&resolved_elem);
                    if matches!(eligibility, crate::eq_eligibility::EqEligibility::Eligible)
                        && (is_copy || is_owned_admissible)
                    {
                        self.record_resolved_vec_call("contains", &resolved_elem, span);
                    } else if matches!(eligibility, crate::eq_eligibility::EqEligibility::Eligible)
                    {
                        // Eligible but not Copy: layout-managed semantics
                        // (clone/drop) are not yet supported here.  The
                        // historical `_layout` fail-closed diagnostic is the
                        // closest substitute and names the would-be symbol.
                        self.report_error(
                            TypeErrorKind::InvalidOperation,
                            span,
                            format!(
                                "`Vec.contains` on layout-backed element type `{}` requires \
                                 the element to be `Copy`; layout-managed records require \
                                 clone/drop semantics that are not implemented for \
                                 equality-based contains",
                                resolved_elem.user_facing()
                            ),
                        );
                    } else {
                        self.report_vec_contains_layout_equality_gate(
                            &resolved_elem,
                            eligibility,
                            span,
                        );
                    }
                } else {
                    self.record_resolved_vec_call("contains", &resolved_elem, span);
                }
                Ty::Bool
            }
            "join" if runtime_method_declared => {
                self.check_arity(args, 1, "`Vec.join`", span);
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    self.check_against(expr, sp, &Ty::String);
                }
                if elem_ty == Ty::String {
                    // `Vec<string>::join` is the sole element-type cell;
                    // non-string element rejection remains the type gate
                    // below.
                    let resolved_elem = self.subst.resolve(&elem_ty);
                    self.record_resolved_vec_call("join", &resolved_elem, span);
                } else {
                    self.report_error(
                        TypeErrorKind::UndefinedMethod,
                        span,
                        format!(
                            "`Vec.join` is only available on Vec<string>, not Vec<{}>",
                            elem_ty.user_facing()
                        ),
                    );
                }
                Ty::String
            }
            "map" => {
                self.check_arity(args, 1, "`Vec.map`", span);
                let ret_ty = Ty::Var(TypeVar::fresh());
                let expected_fn = Ty::Function {
                    capabilities: crate::CallableCapabilities {
                        suspends: true,
                        ..crate::CallableCapabilities::default()
                    },
                    params: vec![elem_ty.clone()],
                    ret: Box::new(ret_ty.clone()),
                };
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    self.check_against(expr, sp, &expected_fn);
                }
                let resolved_ret = self.subst.resolve(&ret_ty);
                let resolved_elem = self.subst.resolve(&elem_ty);
                if !self.reject_vec_pipeline_fn_element("map", &resolved_elem, span) {
                    self.record_vec_higher_order_rewrite(
                        VecHigherOrderOp::Map,
                        &resolved_elem,
                        &resolved_ret,
                        span,
                    );
                }
                self.make_vec_type(resolved_ret, span)
            }
            "filter" => {
                self.check_arity(args, 1, "`Vec.filter`", span);
                let expected_fn = Ty::Function {
                    capabilities: crate::CallableCapabilities {
                        suspends: true,
                        ..crate::CallableCapabilities::default()
                    },
                    params: vec![elem_ty.clone()],
                    ret: Box::new(Ty::Bool),
                };
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    self.check_against(expr, sp, &expected_fn);
                }
                let resolved_elem = self.subst.resolve(&elem_ty);
                if !self.reject_vec_pipeline_fn_element("filter", &resolved_elem, span) {
                    self.record_vec_higher_order_rewrite(
                        VecHigherOrderOp::Filter,
                        &resolved_elem,
                        &resolved_elem,
                        span,
                    );
                }
                resolved.clone()
            }
            "reduce" => {
                // Argument order: closure first, seed second
                // (`numbers.reduce(|a, b| a + b, 0)`) — `fold` with the
                // arguments flipped for chain readability (spec §3.8.6
                // documents this seeded form). A seedless 1-arg `reduce`
                // is deliberately not provided: it would need an
                // empty-vector answer, and we refuse to invent one.
                self.check_arity(args, 2, "`Vec.reduce`", span);
                let acc_ty = if let Some(arg) = args.get(1) {
                    let (expr, sp) = arg.expr();
                    self.synthesize(expr, sp)
                } else {
                    Ty::Var(TypeVar::fresh())
                };
                let expected_fn = Ty::Function {
                    capabilities: crate::CallableCapabilities {
                        suspends: true,
                        ..crate::CallableCapabilities::default()
                    },
                    params: vec![acc_ty.clone(), elem_ty.clone()],
                    ret: Box::new(acc_ty.clone()),
                };
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    self.check_against(expr, sp, &expected_fn);
                }
                let resolved_acc = self.subst.resolve(&acc_ty);
                let resolved_elem = self.subst.resolve(&elem_ty);
                if !self.reject_vec_pipeline_fn_element("reduce", &resolved_elem, span) {
                    self.record_vec_higher_order_rewrite(
                        VecHigherOrderOp::Reduce,
                        &resolved_elem,
                        &resolved_acc,
                        span,
                    );
                }
                resolved_acc
            }
            "fold" => {
                self.check_arity(args, 2, "`Vec.fold`", span);
                let acc_ty = if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    self.synthesize(expr, sp)
                } else {
                    Ty::Var(TypeVar::fresh())
                };
                let expected_fn = Ty::Function {
                    capabilities: crate::CallableCapabilities {
                        suspends: true,
                        ..crate::CallableCapabilities::default()
                    },
                    params: vec![acc_ty.clone(), elem_ty.clone()],
                    ret: Box::new(acc_ty.clone()),
                };
                if let Some(arg) = args.get(1) {
                    let (expr, sp) = arg.expr();
                    self.check_against(expr, sp, &expected_fn);
                }
                let resolved_elem = self.subst.resolve(&elem_ty);
                if self.reject_vec_pipeline_fn_element("fold", &resolved_elem, span) {
                    Ty::Error
                } else {
                    self.subst.resolve(&acc_ty)
                }
            }
            _ if runtime_method_declared => self
                .check_runtime_vec_method_from_source(type_args, method, args, span)
                .expect("Vec method signature was present immediately before dispatch"),
            _ => {
                // Unknown method fail-closed fallback.  Kept inline (NOT routed
                // through the shared `collection_method_fallback`) to preserve
                // the historical asymmetry: a successful primitive-trait
                // dispatch `return`s early and bypasses the structural-array
                // post-guard, whereas the "no method on Vec" path falls through
                // to it.
                let receiver = Ty::Named {
                    head: crate::TypeHead::Builtin(BuiltinType::Vec),
                    args: vec![self.subst.resolve(&elem_ty)],
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
        result
    }
}
