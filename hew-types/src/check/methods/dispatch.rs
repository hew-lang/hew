//! Checker methods grouped by responsibility: dispatch.
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
    /// Mutable methods write back to the receiver's place. Record projections
    /// share their root's mutability, just as they do for field assignment.
    pub(super) fn check_mutable_method_receiver(
        &mut self,
        receiver: &Spanned<Expr>,
        description: &str,
        span: &Span,
    ) {
        self.reject_indexed_writable_borrow(receiver);
        let place = self.expr_place(&receiver.0).or_else(|| {
            self.assignment_root_binding_name(&receiver.0)
                .map(|root| (root.to_string(), Vec::new()))
        });
        let root = place.as_ref().map(|(root, _)| root.as_str());
        if !root
            .and_then(|root| self.env.lookup_ref(root))
            .is_some_and(|binding| binding.is_mutable)
        {
            if let Some(error) =
                root.and_then(|root| self.private_capture_mutation_error(root, span))
            {
                self.errors.push(error);
                return;
            }
            let label =
                root.map_or_else(|| "this expression".to_string(), |root| format!("`{root}`"));
            let declaration = root
                .and_then(|root| self.env.lookup_ref(root))
                .and_then(|binding| binding.def_span.clone());
            let error_index = self.errors.len();
            self.report_error(
                TypeErrorKind::MutabilityError,
                span,
                format!("{description} requires a mutable binding receiver; {label} is not declared with `var`"),
            );
            if let (Some(declaration), Some(error)) =
                (declaration, self.errors.get_mut(error_index))
            {
                error.notes.push((
                    declaration,
                    "immutable binding declared here; use `var` to allow mutation".to_string(),
                    error.source_module.clone(),
                ));
            }
        } else if let Some((root, path)) = place {
            self.env.discount_mutation_receiver_read(&root);
            self.env.mark_written(&root);
            self.reject_borrowed_parameter_mutation(&root, &path, span);
        }
    }

    pub(in crate::check) fn check_method_call(
        &mut self,
        receiver: &Spanned<Expr>,
        method: &str,
        args: &[CallArg],
        span: &Span,
    ) -> Ty {
        let result = self.check_method_call_inner(receiver, method, args, span);
        let result = self.finish_actor_receive_call(receiver, span, result);
        let key = SpanKey::in_module(span, self.current_module_idx);
        self.check_method_callable_place(receiver, method, span);
        let runtime_rewrite_consumes_receiver = matches!(
            self.method_call_rewrites.get(&key),
            Some(MethodCallRewrite::RewriteToFunction {
                consumes_receiver: true,
                ..
            })
        );
        let runtime_rewrite_updates_receiver = matches!(
            self.method_call_rewrites.get(&key),
            Some(MethodCallRewrite::RewriteToFunction {
                descriptor: Some(descriptor),
                ..
            }) if matches!(
                descriptor.family().semantic_contract().map(|contract| contract.result),
                Some(
                    crate::runtime_call::RuntimeResultEffect::UpdatedReceiver(_)
                        | crate::runtime_call::RuntimeResultEffect::UpdatedReceiverAndValue(_)
                )
            )
        );
        let collection_updates_receiver = self
            .resolved_calls
            .get(&key)
            .and_then(|call| match call.target {
                CallTarget::RuntimeCollection(crate::MethodTargetFamily::Vec(method)) => {
                    crate::VecValueOp::from_method(method).map(crate::RuntimeCallFamily::Vector)
                }
                CallTarget::RuntimeCollection(crate::MethodTargetFamily::HashMap(method)) => {
                    crate::runtime_call::MapValueOp::from_method(method)
                        .map(crate::RuntimeCallFamily::Map)
                }
                CallTarget::RuntimeCollection(crate::MethodTargetFamily::HashSet(method)) => {
                    crate::runtime_call::SetValueOp::from_method(method)
                        .map(crate::RuntimeCallFamily::Set)
                }
                _ => None,
            })
            .is_some_and(|family| {
                matches!(
                    family.semantic_contract().map(|contract| contract.result),
                    Some(
                        crate::RuntimeResultEffect::UpdatedReceiver(_)
                            | crate::RuntimeResultEffect::UpdatedReceiverAndValue(_)
                    )
                )
            });
        if runtime_rewrite_updates_receiver || collection_updates_receiver {
            self.check_mutable_method_receiver(
                receiver,
                &format!("collection method `{method}`"),
                span,
            );
        }

        if runtime_rewrite_consumes_receiver {
            self.method_call_consumes_receiver.insert(key);
            if let Expr::Ident(name) = &receiver.0 {
                // The typed consumption decision overrides a surface Copy
                // derivation. In particular, a lambda-actor handle is an
                // opaque wrapper, but release still consumes its sole runtime
                // handle and any later receiver use is invalid.
                self.env.mark_moved(name.name.as_str(), receiver.1.clone());
            }
        }

        result
    }

    pub(in crate::check) fn check_dotted_type_member_call_against_expected(
        &mut self,
        receiver: &Spanned<Expr>,
        method: &str,
        args: &[CallArg],
        expected: &Ty,
        span: &Span,
    ) -> Option<Ty> {
        let head = self.resolve_dotted_type_head(receiver, method)?;
        let result = self.dispatch_dotted_type_member(
            &head,
            method,
            &DottedTypeMemberUse::Call {
                args,
                expected: Some(expected),
                span,
            },
        )?;
        self.mark_resolved_nominal_owner_used(&head.canonical_type);

        Some(result)
    }

    #[expect(
        clippy::too_many_lines,
        reason = "pattern matching type checker with many variants"
    )]
    pub(super) fn check_method_call_inner(
        &mut self,
        receiver: &Spanned<Expr>,
        method: &str,
        args: &[CallArg],
        span: &Span,
    ) -> Ty {
        let dotted_type_head = self.resolve_dotted_type_head(receiver, method);
        if let Some(head) = dotted_type_head.as_ref() {
            if let Some(result) = self.dispatch_dotted_type_member(
                head,
                method,
                &DottedTypeMemberUse::Call {
                    args,
                    expected: None,
                    span,
                },
            ) {
                self.mark_resolved_nominal_owner_used(&head.canonical_type);
                return result;
            }
            let source_member = format!("{}.{method}", head.canonical_type);
            if !self.has_fn_sig(&source_member) {
                for arg in args {
                    let (expr, arg_span) = arg.expr();
                    self.synthesize(expr, arg_span);
                }
                self.report_error(
                    TypeErrorKind::UndefinedFunction,
                    span,
                    format!(
                        "undefined static function `{}.{method}`",
                        head.canonical_type
                    ),
                );
                return Ty::Error;
            }
        }
        // Module-qualified calls: e.g. http.listen(addr) → lookup "http.listen" in fn_sigs
        if let Expr::Ident(name) = &receiver.0 {
            let receiver_is_binding = self.env.lookup_ref(name.name.as_str()).is_some();
            // The shared type-head resolver already selected every
            // declaration-proven nominal above. Preserve that classification
            // while deciding whether an unresolved head is a module call.
            let receiver_is_known_type = dotted_type_head.is_some();
            let receiver_shadows_module = receiver_is_binding
                && self.module_import_bindings.contains_key(&(
                    self.current_module.clone(),
                    self.current_module_idx,
                    name.to_string(),
                ));
            if receiver_shadows_module {
                self.record_method_call_receiver_kind(
                    span,
                    MethodCallReceiverKind::LexicalBinding {
                        binding_name: name.to_string(),
                    },
                );
            }
            // `name` is a lexical import binding (`string`, or an explicit
            // module alias). Declaration and export registries are keyed by the
            // exact source owner, so resolve the binding before every authority
            // lookup. Never recover the owner from the final path segment: two
            // nested stdlib modules may share both a leaf and a function name.
            let canonical_owner = self.canonical_module_import_owner(name.name.as_str());
            let key = self.canonical_fn_identity(Some(&canonical_owner), method);
            let looks_like_module_call = !receiver_is_binding
                && !receiver_is_known_type
                && (self.module_binding_in_current_file(name.name.as_str())
                    || self.module_fn_exports.contains(&key)
                    || self.has_fn_sig(&key));
            if looks_like_module_call {
                self.record_method_call_receiver_kind(
                    span,
                    MethodCallReceiverKind::ModuleBinding {
                        module_name: canonical_owner.clone(),
                    },
                );
                if self.module_binding_in_current_file(name.name.as_str()) {
                    self.used_modules.borrow_mut().insert(ImportKey::in_file(
                        self.current_module.clone(),
                        self.current_module_idx,
                        name.to_string(),
                    ));
                }
                // Cross-module enum variant construction: e.g. `fs.IoError::TimedOut(0)`.
                // method contains "::" → treat as a qualified variant constructor rather than a
                // module function. Mirrors the lookup in check_call (calls.rs:407-465).
                if method.contains("::") {
                    let lifecycle_surface = format!("{name}.{method}");
                    let Ok(canonical_lifecycle) =
                        self.canonicalize_source_lifecycle_value_path(&lifecycle_surface, span)
                    else {
                        return Ty::Error;
                    };
                    let constructor_surface = canonical_lifecycle.as_deref().unwrap_or(method);
                    let constructor_match = self.lookup_variant_constructor(constructor_surface);
                    if let Some((type_name, expected_params, type_params)) = constructor_match {
                        let type_param_count = type_params.len();
                        let mut inferred_args = Vec::new();
                        while inferred_args.len() < type_param_count {
                            inferred_args.push(Ty::Var(TypeVar::fresh()));
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
                                    let (expr, sp) = arg.expr();
                                    let expected_ty = if subst_map.is_empty() {
                                        param_ty.clone()
                                    } else {
                                        param_ty.substitute_named_params_parallel(&subst_map)
                                    };
                                    self.check_against(expr, sp, &expected_ty);
                                }
                            }
                        }
                        let resolved_args: Vec<Ty> = inferred_args
                            .iter()
                            .map(|ty| self.subst.resolve(ty))
                            .collect();
                        return self.variant_nominal_ty(&type_name, resolved_args);
                    }
                }
                if !self.module_fn_exports.contains(&key) {
                    // The function is not a `pub` export.  Determine why:
                    //   • no fn_visibility entry → truly unknown symbol
                    //   • fn_visibility entry, access denied → E_VISIBILITY
                    //   • fn_visibility entry, access allowed → `package fn` accessed
                    //     from within the same package; fall through to the success path.
                    if let Some(&vis) = self.fn_visibility.get(&key) {
                        // Materialise owned copies so the &self borrow from fn_def_spans
                        // is released before calling synthesize (&mut self).
                        let decl_module_owned =
                            self.fn_def_spans.get(&key).and_then(|(_, m)| m.clone());
                        let decl_span_owned = self
                            .fn_def_spans
                            .get(&key)
                            .map_or_else(|| span.clone(), |(s, _)| s.clone());
                        let acc_module_owned = self.current_module.clone();
                        let decl_module = decl_module_owned.as_deref();
                        if !visibility::access_allowed(
                            decl_module,
                            acc_module_owned.as_deref(),
                            vis,
                        ) {
                            // Access denied — synthesize args for error recovery and reject.
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
                                decl_module.unwrap_or("(root)"),
                                &acc_module_str,
                                decl_span_owned,
                                acc_module_owned,
                            );
                            self.errors.push(err);
                            return Ty::Error;
                        }
                        // access_allowed returned true: `package fn` accessible from this
                        // package — fall through to the success path below.
                    } else {
                        // No visibility record: the function is genuinely unknown.
                        for arg in args {
                            let (expr, sp) = arg.expr();
                            self.synthesize(expr, sp);
                        }
                        let kind = if self
                            .resolve_module_type(name.name.as_str(), method)
                            .is_some()
                        {
                            TypeErrorKind::PathKindMismatch
                        } else {
                            TypeErrorKind::PathMemberNotFound
                        };
                        self.report_error(
                            kind,
                            span,
                            format!("no function `{method}` in module `{name}`"),
                        );
                        return Ty::Error;
                    }
                }
                self.require_unsafe(&key, span);
                // Call and value-position references share the manifest's
                // canonical member/module capability selection.
                self.reject_wasm_native_only_module_function(name.name.as_str(), method, span);
                // crypto.random_bytes and its fallible twin depend on a
                // native-only secure entropy source absent from the wasm32 link
                // set; reject so secure randomness fails closed on wasm32.
                if self.is_shipped_crypto_module(name.name.as_str())
                    && matches!(method, "random_bytes" | "try_random_bytes")
                {
                    self.reject_wasm_feature(span, WasmUnsupportedFeature::CryptoRandom);
                }
                if let Some(sig) = self.fn_sig(&key).cloned() {
                    self.record_call_edge(&key);
                    self.record_module_qualified_stdlib_call_rewrite_if_any(
                        name.name.as_str(),
                        method,
                        span,
                    );
                    self.record_module_qualified_user_call_rewrite_if_any(
                        name.name.as_str(),
                        method,
                        span,
                    );
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
                            arity_context: "this function".to_string(),
                        },
                        true,
                        Some(GenericCallee::Function { key: &key }),
                    );
                    if self.record_generic_wire_codec_rewrite(
                        &key,
                        &applied_sig.params,
                        &applied_sig.return_type,
                        span,
                    ) {
                        return applied_sig.return_type;
                    }
                    if let Some(op) = self.intrinsic_math_generic_op_for_signature(&key) {
                        self.record_method_call_rewrite(
                            span,
                            MethodCallRewrite::GenericMathIntrinsic { op },
                        );
                    }
                    return applied_sig.return_type;
                }
                for arg in args {
                    let (expr, sp) = arg.expr();
                    self.synthesize(expr, sp);
                }
                self.report_error(
                    TypeErrorKind::PathMemberNotFound,
                    span,
                    format!("no function `{method}` in module `{name}`"),
                );
                return Ty::Error;
            }

            // Static method calls on type names: e.g. Point.from_json(json)
            // Look up "TypeName.method" in fn_sigs (registered by wire types
            // etc.). The surface spelling resolves to its canonical
            // declaration identity first (A316): the wire codec surface
            // registers under `{module}.{Name}.{method}` only, so the bare
            // binding an import published (or an `as`-alias) must consult the
            // canonical key of ITS owner. The surface key remains the lookup
            // for root-canonical (bare) declarations and any non-wire dotted
            // signature registered under its own spelling.
            let canonical_static_owner = self
                .canonical_nominal_name(name.name.as_str())
                .unwrap_or_else(|| name.to_string());
            let static_key = format!("{canonical_static_owner}.{method}");
            let static_sig = self.fn_sig(&static_key).cloned().or_else(|| {
                (canonical_static_owner != name.name.as_str())
                    .then(|| self.fn_sig(&format!("{name}.{method}")).cloned())
                    .flatten()
            });
            if let Some(sig) = static_sig {
                self.check_arity(args, sig.params.len(), &format!("`{static_key}`"), span);
                for (i, arg) in args.iter().enumerate() {
                    if let Some(param_ty) = sig.params.get(i) {
                        let (expr, sp) = arg.expr();
                        self.check_against(expr, sp, param_ty);
                    }
                }
                let impl_key = format!("{name}::{method}");
                if let Some(declaration) = self.impl_method_declaration_ids.get(&impl_key).copied()
                {
                    let target = CallTarget::ImplMethod(declaration);
                    self.record_method_call_rewrite(
                        span,
                        MethodCallRewrite::RewriteModuleQualifiedToFunction {
                            target: target.clone(),
                            c_symbol: impl_key.clone(),
                        },
                    );
                    self.record_direct_call_target(span, target);
                }
                // Wire codec static deserialize methods on a `#[wire]` struct or
                // enum. `decode` is the binary CBOR path
                // (`Type.decode(bytes) -> Type`); `from_json`/`from_yaml` are the
                // text path (`Type.from_json(string) -> Result<Type, string>`),
                // lowered through the CBOR↔text bridge. Each lives in `fn_sigs`
                // under the dotted `Type.<method>` key but records no rewrite
                // here without this arm, so the call would lower to
                // `MethodCallNoRewrite`. Record a dedicated codec rewrite so
                // HIR/codegen drive the matching thunk with the correct ABI.
                if self.wire_struct_types.contains(&canonical_static_owner)
                    || self.wire_enum_types.contains(&canonical_static_owner)
                {
                    let text_dir = match method {
                        "decode" => Some(WireCodecDirection::Decode),
                        "from_json" => Some(WireCodecDirection::FromJson),
                        "from_yaml" => Some(WireCodecDirection::FromYaml),
                        _ => None,
                    };
                    if let Some(direction) = text_dir {
                        // The codec `value_ty` is the produced wire type. For
                        // `decode` the registered return type IS that type; for
                        // `from_json`/`from_yaml` it is `Result<Self, string>`, so
                        // peel the `Ok` payload to recover the wire type codegen
                        // keys the thunk by.
                        let resolved_ret = self.subst.resolve(&sig.return_type);
                        let value_source = if direction == WireCodecDirection::Decode {
                            resolved_ret.clone()
                        } else {
                            result_ok_payload(&resolved_ret).unwrap_or_else(|| resolved_ret.clone())
                        };
                        if let Ok(value_ty) = ResolvedTy::from_ty(&value_source) {
                            self.record_method_call_rewrite(
                                span,
                                MethodCallRewrite::WireCodec {
                                    direction,
                                    value_ty,
                                },
                            );
                        }
                    }
                }
                return sig.return_type;
            }
        }

        self.place_base_depth += 1;
        let receiver_ty = self.synthesize(&receiver.0, &receiver.1);
        self.place_base_depth -= 1;
        let resolved = self.subst.resolve(&receiver_ty);
        if let Some(result) =
            self.check_actor_delivery_method(receiver, &resolved, method, args, span)
        {
            return result;
        }

        // If the receiver is still an unresolved inference variable that was
        // created from a coercible integer-literal / const-integer range (both
        // bounds were literals or let-/const-bound integer literals), eagerly
        // bind it to i64 before method dispatch.  Coercible-bounds ranges
        // produce a fresh TypeVar so that function-call use-sites can still
        // narrow the element type via unification (e.g. `fib(i: i32)` narrows
        // to i32 without going through a method call), but method dispatch
        // cannot drive unification from the receiver type alone.  The i64
        // default matches what `default_unconstrained_range_types` would apply
        // at the end of the inference pass, moved forward so receiver-only
        // numeric methods resolve correctly inside the loop body.
        let resolved = if let Ty::Var(v) = resolved {
            let is_int_range_var = self
                .deferred_range_bounds
                .iter()
                .any(|(_, dv, ..)| *dv == v);
            if is_int_range_var {
                self.subst
                    .insert(v, &Ty::I64)
                    .expect("binding integer range element var to i64 must stay acyclic");
                Ty::I64
            } else {
                Ty::Var(v)
            }
        } else {
            resolved
        };
        let resolved = match &resolved {
            Ty::Named {
                head,
                args: type_args,
            } if crate::method_resolution::lookup_named_method_sig(
                &self.defs,
                &self.type_defs,
                self.sigs(),
                head.registry_key(),
                type_args,
                method,
            )
            .is_none() =>
            {
                self.alias_target_for_instance(head.registry_key(), type_args)
                    .unwrap_or(resolved)
            }
            _ => resolved,
        };
        if let Some((_, child_ty)) = resolved.as_supervisor_pool() {
            let kind = match method {
                "len" => crate::check::types::PoolAccessorKind::Len,
                "get" => crate::check::types::PoolAccessorKind::Get,
                _ => {
                    self.report_error(
                        TypeErrorKind::UndefinedMethod,
                        span,
                        format!(
                            "supervisor pool has no method `{method}`; supported methods are \
                             `get(i)` and `len()`"
                        ),
                    );
                    for arg in args {
                        let (expr, sp) = arg.expr();
                        self.synthesize(expr, sp);
                    }
                    return Ty::Error;
                }
            };
            self.pool_accessor_sites.insert(
                SpanKey::in_module(span, self.current_module_idx),
                crate::check::types::PoolAccessor { kind },
            );
            return match kind {
                crate::check::types::PoolAccessorKind::Len => {
                    self.check_arity(args, 0, "pool `len`", span);
                    Ty::I64
                }
                crate::check::types::PoolAccessorKind::Get => {
                    self.check_arity(args, 1, "pool `get`", span);
                    if let Some(arg) = args.first() {
                        let (expr, sp) = arg.expr();
                        self.check_against(expr, sp, &Ty::I64);
                    }
                    Ty::option(Ty::child_ref(child_ty.clone()))
                }
                crate::check::types::PoolAccessorKind::Index => {
                    unreachable!("index access does not enter method checking")
                }
            };
        }
        self.reject_if_wasm_native_only_handle(&resolved, span);
        self.reject_if_wasm_blocking_semaphore_method(&resolved, method, span);
        if let Ty::Named { head, .. } = &resolved {
            let name = head.registry_key();
            self.warn_if_blocking_handle_method(name, method, span);
        }
        // Structural clone admission is member-wise for tuples and built-in
        // value enums. Collection clones keep their existing runtime rewrites,
        // but pass through the same affine/member gate first.
        if method == "clone"
            && args.is_empty()
            && matches!(
                &resolved,
                Ty::Tuple(_)
                    | Ty::Array(_, _)
                    | Ty::Function { .. }
                    | Ty::Closure { .. }
                    | Ty::Named {
                        head: crate::TypeHead::Builtin(_) | crate::TypeHead::Actor(_),
                        ..
                    }
            )
        {
            let is_structural_value = matches!(
                &resolved,
                Ty::Tuple(_)
                    | Ty::Array(_, _)
                    | Ty::Function { .. }
                    | Ty::Closure { .. }
                    | Ty::Named {
                        head: crate::TypeHead::Builtin(BuiltinType::Option | BuiltinType::Result),
                        ..
                    }
            );
            if resolved.has_inference_var()
                && matches!(
                    &resolved,
                    Ty::Named {
                        head: crate::TypeHead::Builtin(BuiltinType::Vec | BuiltinType::HashMap),
                        ..
                    }
                )
            {
                self.deferred_builtin_clone_admission
                    .entry(SpanKey::in_module(span, self.current_module_idx))
                    .or_insert_with(|| DeferredBuiltinCloneAdmission {
                        span: span.clone(),
                        receiver_ty: resolved.clone(),
                        source_module: self.current_module.clone(),
                    });
            }
            if let Some(blocker) = self.structural_clone_blocker(&resolved) {
                let receiver_name = resolved.user_facing().to_string();
                let rejected = match blocker {
                    CloneCapabilityBlocker::Affine {
                        type_name,
                        marker,
                        member,
                    } => {
                        self.report_affine_record_clone_error(
                            &receiver_name,
                            &type_name,
                            marker,
                            &member,
                            span,
                        );
                        true
                    }
                    CloneCapabilityBlocker::Opaque { type_name, member } if is_structural_value => {
                        self.report_error(
                            TypeErrorKind::UndefinedMethod,
                            span,
                            format!(
                                "type `{receiver_name}` cannot be cloned because member `{member}` \
                                 contains opaque value `{type_name}`"
                            ),
                        );
                        true
                    }
                    CloneCapabilityBlocker::Missing { member, member_ty }
                        if is_structural_value =>
                    {
                        self.report_error(
                            TypeErrorKind::UndefinedMethod,
                            span,
                            format!(
                                "type `{receiver_name}` cannot be cloned because member `{member}` \
                                 of type `{}` has no Clone capability",
                                member_ty.user_facing()
                            ),
                        );
                        true
                    }
                    CloneCapabilityBlocker::Opaque { .. }
                    | CloneCapabilityBlocker::Missing { .. } => false,
                };
                if rejected {
                    return Ty::Error;
                }
            }
            if is_structural_value {
                self.record_method_call_rewrite(
                    span,
                    MethodCallRewrite::RecordCloneInplace {
                        record_name: resolved.user_facing().to_string(),
                    },
                );
                return resolved;
            }
        }

        match (&resolved, method) {
            (Ty::Array(_, _), "len") => {
                self.check_arity(args, 0, "fixed array len", span);
                self.record_runtime_method_family_rewrite(
                    span,
                    crate::RuntimeCallFamily::Array(crate::runtime_call::ArrayValueOp::Len),
                );
                Ty::I64
            }
            (Ty::CancellationToken, "is_cancelled") => {
                self.check_arity(args, 0, "`CancellationToken.is_cancelled`", span);
                self.record_method_call_rewrite(
                    span,
                    MethodCallRewrite::CancellationTokenIsCancelled,
                );
                Ty::Bool
            }
            (Ty::CancellationToken, _) => {
                for arg in args {
                    let (expr, sp) = arg.expr();
                    self.synthesize(expr, sp);
                }
                self.report_error_with_suggestions(
                    TypeErrorKind::UndefinedMethod,
                    span,
                    format!("no method `{method}` on `CancellationToken`"),
                    self.similar_methods(&resolved, method),
                );
                Ty::Error
            }
            // Vec methods
            (
                Ty::Named {
                    head: crate::TypeHead::Builtin(BuiltinType::Vec),
                    args: type_args,
                    ..
                },
                _,
            ) => self.check_vec_method(type_args, &receiver_ty, &resolved, method, args, span),
            // HashMap methods
            (
                Ty::Named {
                    head: crate::TypeHead::Builtin(BuiltinType::HashMap),
                    args: type_args,
                    ..
                },
                _,
            ) => self.check_hashmap_method(type_args, method, args, span),
            // HashSet methods
            (
                Ty::Named {
                    head: crate::TypeHead::Builtin(BuiltinType::HashSet),
                    args: type_args,
                    ..
                },
                _,
            ) => {
                // Preserve the receiver's original inference vars so a later non-literal insert
                // can refine an earlier `IntLiteral` element before we validate lowerability.
                let original_type_args = match &receiver_ty {
                    Ty::Named {
                        head: crate::TypeHead::Builtin(BuiltinType::HashSet),
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
                    head: crate::TypeHead::Builtin(BuiltinType::Rc),
                    args: type_args,
                    ..
                },
                _,
            ) => self.check_rc_method(type_args, method, args, span),
            // Weak<T> methods
            (
                Ty::Named {
                    head: crate::TypeHead::Builtin(BuiltinType::Weak),
                    args: type_args,
                    ..
                },
                _,
            ) => self.check_weak_method(type_args, method, args, span),
            // instant receiver methods (`.elapsed()`, `.duration_since()`) are
            // declared in the `impl instant` block in `std/builtins.hew` with
            // monomorphic `#[extern_symbol(hew_instant_*)]` annotations, mirroring
            // the `Ty::Duration` arm below. `instant` is i64-backed at the MIR
            // boundary, so the receiver lowers as a bare `i64` nanos timestamp.
            (
                Ty::Named {
                    head: crate::TypeHead::Builtin(BuiltinType::Instant),
                    ..
                },
                _,
            ) => {
                if let Some(ret_ty) = self.dispatch_monomorphic_extern_symbol_method(
                    "instant",
                    &[],
                    method,
                    args,
                    span,
                    &receiver_ty,
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
                    format!("no method `{method}` on `instant`"),
                );
                Ty::Error
            }
            // bytes methods are declared in `std/io.hew` with monomorphic
            // `#[extern_symbol]` annotations over the current Vec<i32>-backed
            // bytes ABI.
            (Ty::Bytes, _) => {
                if let Some(ret_ty) = self.dispatch_monomorphic_extern_symbol_method(
                    "bytes",
                    &[],
                    method,
                    args,
                    span,
                    &Ty::Bytes,
                ) {
                    return ret_ty;
                }
                self.check_primitive_receiver_method_fallback(
                    &Ty::Bytes,
                    "`bytes`",
                    method,
                    args,
                    span,
                )
            }
            // Duration methods are declared in `std/builtins.hew` with
            // monomorphic `#[extern_symbol]` annotations.
            (Ty::Duration, _) => {
                if let Some(ret_ty) = self.dispatch_monomorphic_extern_symbol_method(
                    "duration",
                    &[],
                    method,
                    args,
                    span,
                    &Ty::Duration,
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
            // Exact fallible numeric conversion: `.try_to_<W>() -> Option<W>`.
            (resolved, method) if resolved.is_numeric() && method.starts_with("try_to_") => {
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
                    "f32" => Some(Ty::F32),
                    "f64" => Some(Ty::F64),
                    _ => None,
                };
                if let Some(target) = target_opt {
                    if !self.check_arity(args, 0, method, span) {
                        for arg in args {
                            let (expr, sp) = arg.expr();
                            self.synthesize(expr, sp);
                        }
                        return Ty::Error;
                    }
                    let resolved = resolved.materialize_literal_defaults();
                    let from_range =
                        super::util::integer_type_range(&resolved, self.pointer_width());
                    let to_range = super::util::integer_type_range(&target, self.pointer_width());
                    let kind = match (resolved.is_integer(), target.is_integer()) {
                        (true, true) => TryConversionKind::IntToInt,
                        (false, true) => TryConversionKind::FloatToInt,
                        (true, false) => TryConversionKind::IntToFloat,
                        (false, false) => TryConversionKind::FloatToFloat,
                    };
                    self.try_width_cast_lowerings.insert(
                        SpanKey::in_module(span, self.current_module_idx),
                        TryWidthCastLowering {
                            from_ty: resolved.clone(),
                            from_range,
                            to_range,
                            to_ty: target.clone(),
                            kind,
                        },
                    );
                    Ty::option(target)
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
                             i8, i16, i32, i64, isize, u8, u16, u32, u64, usize, f32, f64",
                        ),
                    );
                    Ty::Error
                }
            }
            // Explicit-wrap width reinterpretation: `.wrapping_as_<W>() -> W`.
            //
            // Admitted for all integer-to-integer pairs (any width, any sign).
            // Truncates / sign-extends / zero-extends bits per LLVM trunc/sext/zext.
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
                    if !self.check_arity(args, 0, method, span) {
                        for arg in args {
                            let (expr, sp) = arg.expr();
                            self.synthesize(expr, sp);
                        }
                        return Ty::Error;
                    }
                    let resolved = resolved.materialize_literal_defaults();
                    let from_range =
                        super::util::integer_type_range(&resolved, self.pointer_width());
                    let to_range = super::util::integer_type_range(&target, self.pointer_width());
                    self.width_cast_lowerings.insert(
                        SpanKey::in_module(span, self.current_module_idx),
                        WidthCastLowering {
                            from_ty: resolved.clone(),
                            from_range,
                            to_range,
                            to_ty: target.clone(),
                            kind: WidthCastKind::Wrapping,
                        },
                    );
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
            // Returns W::MAX on positive overflow, W::MIN on negative overflow.
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
                    if !self.check_arity(args, 0, method, span) {
                        for arg in args {
                            let (expr, sp) = arg.expr();
                            self.synthesize(expr, sp);
                        }
                        return Ty::Error;
                    }
                    let resolved = resolved.materialize_literal_defaults();
                    let from_range =
                        super::util::integer_type_range(&resolved, self.pointer_width());
                    let to_range = super::util::integer_type_range(&target, self.pointer_width());
                    self.width_cast_lowerings.insert(
                        SpanKey::in_module(span, self.current_module_idx),
                        WidthCastLowering {
                            from_ty: resolved.clone(),
                            from_range,
                            to_range,
                            to_ty: target.clone(),
                            kind: WidthCastKind::Saturating,
                        },
                    );
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
            // `f64` bit/classification methods. `abs` reuses the existing
            // `MathIntrinsic::AbsF64` family (the same `llvm.fabs` `math.abs`
            // already calls); the rest are new `RuntimeCallFamily::FloatMethod`
            // rows. Scoped to `f64`: `RuntimeValueKind` has no `F32` kind yet.
            (resolved, method)
                if resolved.materialize_literal_defaults() == Ty::F64
                    && matches!(
                        method,
                        "to_bits"
                            | "is_nan"
                            | "is_finite"
                            | "is_infinite"
                            | "is_sign_negative"
                            | "abs"
                    ) =>
            {
                for arg in args {
                    let (expr, sp) = arg.expr();
                    self.synthesize(expr, sp);
                }
                self.check_arity(args, 0, &format!("`{method}`"), span);
                let (family, ret_ty) = match method {
                    "to_bits" => (
                        crate::runtime_call::RuntimeCallFamily::FloatMethod(FloatMethodOp::ToBits),
                        Ty::U64,
                    ),
                    "is_nan" => (
                        crate::runtime_call::RuntimeCallFamily::FloatMethod(FloatMethodOp::IsNan),
                        Ty::Bool,
                    ),
                    "is_finite" => (
                        crate::runtime_call::RuntimeCallFamily::FloatMethod(
                            FloatMethodOp::IsFinite,
                        ),
                        Ty::Bool,
                    ),
                    "is_infinite" => (
                        crate::runtime_call::RuntimeCallFamily::FloatMethod(
                            FloatMethodOp::IsInfinite,
                        ),
                        Ty::Bool,
                    ),
                    "is_sign_negative" => (
                        crate::runtime_call::RuntimeCallFamily::FloatMethod(
                            FloatMethodOp::IsSignNegative,
                        ),
                        Ty::Bool,
                    ),
                    "abs" => (
                        crate::runtime_call::RuntimeCallFamily::MathIntrinsic(
                            crate::runtime_call::MathIntrinsic::AbsF64,
                        ),
                        Ty::F64,
                    ),
                    _ => unreachable!("method matched the guard above"),
                };
                self.record_runtime_method_family_rewrite(span, family);
                ret_ty
            }
            // Integer bit-manipulation methods: each lowers to one LLVM
            // intrinsic (ctpop/ctlz/cttz/bswap/bitreverse/fshl/fshr) carried
            // as `RuntimeCallFamily::IntMethod`, at every integer width Hew
            // has. `int_method_width` only misses an untyped `IntLiteral`
            // receiver (no concrete width yet to pick a row for); that case
            // reports `UndefinedMethod` rather than silently guessing one.
            (resolved, method)
                if resolved.is_integer()
                    && matches!(
                        method,
                        "count_ones"
                            | "count_zeros"
                            | "leading_zeros"
                            | "trailing_zeros"
                            | "swap_bytes"
                            | "reverse_bits"
                            | "rotate_left"
                            | "rotate_right"
                    ) =>
            {
                let Some(width) = int_method_width(resolved) else {
                    for arg in args {
                        let (expr, sp) = arg.expr();
                        self.synthesize(expr, sp);
                    }
                    self.report_error(
                        TypeErrorKind::UndefinedMethod,
                        span,
                        format!(
                            "no method `{method}` on `{}`; bit-manipulation methods need a \
                             concrete integer width",
                            resolved.user_facing()
                        ),
                    );
                    return Ty::Error;
                };
                let op = match method {
                    "count_ones" => IntBitOp::CountOnes,
                    "count_zeros" => IntBitOp::CountZeros,
                    "leading_zeros" => IntBitOp::LeadingZeros,
                    "trailing_zeros" => IntBitOp::TrailingZeros,
                    "swap_bytes" => IntBitOp::SwapBytes,
                    "reverse_bits" => IntBitOp::ReverseBits,
                    "rotate_left" => IntBitOp::RotateLeft,
                    "rotate_right" => IntBitOp::RotateRight,
                    _ => unreachable!("method matched the guard above"),
                };
                let is_rotate = matches!(op, IntBitOp::RotateLeft | IntBitOp::RotateRight);
                self.check_arity(args, usize::from(is_rotate), &format!("`{method}`"), span);
                if is_rotate {
                    if let Some(arg) = args.first() {
                        let (expr, sp) = arg.expr();
                        self.check_against(expr, sp, &Ty::U32);
                    }
                } else {
                    for arg in args {
                        let (expr, sp) = arg.expr();
                        self.synthesize(expr, sp);
                    }
                }
                let ret_ty = match op {
                    IntBitOp::CountOnes
                    | IntBitOp::CountZeros
                    | IntBitOp::LeadingZeros
                    | IntBitOp::TrailingZeros => Ty::U32,
                    IntBitOp::SwapBytes
                    | IntBitOp::ReverseBits
                    | IntBitOp::RotateLeft
                    | IntBitOp::RotateRight => resolved.clone(),
                };
                self.record_runtime_method_family_rewrite(
                    span,
                    crate::runtime_call::RuntimeCallFamily::IntMethod(op, width),
                );
                ret_ty
            }
            // Numeric opt-out arithmetic methods: .wrapping_*, .checked_*, .saturating_*
            // for every integer width. Floats are excluded (is_integer() ≠ is_numeric()).
            // Only add/sub/mul are in scope here; div/mod/shift are separate slices.
            // Every one of these rewrites to `RuntimeCallFamily::IntArith` (D465):
            // wrapping is a plain, non-trapping LLVM add/sub/mul; saturating add/sub
            // is `llvm.{s,u}{add,sub}.sat`; saturating multiply is built from
            // `llvm.{s,u}mul.with.overflow` plus a saturating select (no direct
            // LLVM intrinsic exists for it); checked add/sub/mul is the matching
            // `.with.overflow` intrinsic delivered as `Option<T>`.
            //
            // Note: `.wrapping_as_<W>` and `.saturating_as_<W>` (width-conversion
            // family) are handled by the arms above; those arms must appear first so
            // the `_as_` suffix does not reach this arm's op-name matcher.
            (resolved, method)
                if resolved.is_integer()
                    && (method.starts_with("wrapping_")
                        || method.starts_with("checked_")
                        || method.starts_with("saturating_")) =>
            {
                let is_wrapping = method.starts_with("wrapping_");
                let is_checked = method.starts_with("checked_");
                let op_name = if is_wrapping {
                    &method["wrapping_".len()..]
                } else if is_checked {
                    &method["checked_".len()..]
                } else {
                    &method["saturating_".len()..]
                };
                let kind = match (is_wrapping, is_checked, op_name) {
                    (true, _, "add") => Some(IntArithKind::WrappingAdd),
                    (true, _, "sub") => Some(IntArithKind::WrappingSub),
                    (true, _, "mul") => Some(IntArithKind::WrappingMul),
                    (false, false, "add") => Some(IntArithKind::SaturatingAdd),
                    (false, false, "sub") => Some(IntArithKind::SaturatingSub),
                    (false, false, "mul") => Some(IntArithKind::SaturatingMul),
                    (false, true, "add") => Some(IntArithKind::CheckedAdd),
                    (false, true, "sub") => Some(IntArithKind::CheckedSub),
                    (false, true, "mul") => Some(IntArithKind::CheckedMul),
                    _ => None,
                };
                let (Some(kind), Some(width)) = (kind, int_method_width(resolved)) else {
                    for arg in args {
                        let (expr, sp) = arg.expr();
                        self.synthesize(expr, sp);
                    }
                    let reason = if kind.is_none() {
                        "only add, sub, mul are supported in this family".to_string()
                    } else {
                        "needs a concrete integer width".to_string()
                    };
                    self.report_error(
                        TypeErrorKind::UndefinedMethod,
                        span,
                        format!(
                            "no method `{method}` on `{}`; {reason}",
                            resolved.user_facing()
                        ),
                    );
                    return Ty::Error;
                };
                self.check_arity(args, 1, &format!("`{method}`"), span);
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    self.check_against(expr, sp, resolved);
                }
                self.record_runtime_method_family_rewrite(
                    span,
                    crate::runtime_call::RuntimeCallFamily::IntArith(kind, width),
                );
                if is_checked {
                    Ty::option(resolved.clone())
                } else {
                    resolved.clone()
                }
            }
            // Local actor-reference methods first check the concrete reference
            // type's own impl, then fall through to actor receive-fn dispatch.
            //
            // `ChildRef<T>` and an actor handle are distinct value representations;
            // their own methods are registered under their respective nominal
            // owners. Named receive handlers share the local dispatch path.
            (resolved, _) if resolved.as_local_actor_ref().is_some() => {
                let actor_ref_builtin = if resolved.as_child_ref().is_some() {
                    crate::BuiltinType::ChildRef
                } else {
                    crate::BuiltinType::ActorHandle
                };
                // `stop` is the actor handle's own lifecycle method
                // (HEW-SPEC-2026 §2.1): it requests a graceful stop and
                // returns at once, so `self.stop()` lets the current handler
                // finish before `#[on(stop)]` runs. A supervisor's lifecycle
                // is `close`, which tears its tree down.
                let supervisor = matches!(
                    resolved.as_actor_handle(),
                    Some(Ty::Named { head, .. }) if self.supervisor_children.contains_key(head.registry_key())
                );
                if method == "stop" && resolved.as_actor_handle().is_some() && !supervisor {
                    if !self.check_arity(args, 0, "`stop`", span) {
                        return Ty::Error;
                    }
                    self.actor_delivery_calls.insert(
                        SpanKey::in_module(span, self.current_module_idx),
                        crate::actor_delivery::ActorDeliveryCall::Stop,
                    );
                    return Ty::Unit;
                }
                // A user handler named `send` is actor dispatch; otherwise
                // `send` resolves through the reference type's own method.
                let has_user_send_handler = if method == "send" {
                    resolved.as_local_actor_ref().and_then(|inner| {
                        if let Ty::Named { head, .. } = inner { let name = head.registry_key();
                            Some(name.to_string())
                        } else {
                            None
                        }
                    }).is_some_and(|actor_name| {
                        self.actor_receive_methods.contains(&format!("{actor_name}::send"))
                            || matches!(
                                self.resolve_bare_actor_identity(&actor_name),
                                BareActorResolution::Resolved(ref id) if self.actor_receive_methods.contains(&format!("{id}::send"))
                            )
                    })
                } else {
                    false
                };
                // A concrete actor-handle `.send(msg)` call with no user
                // `receive fn send` handler has no lowerable local-
                // mailbox delivery path (#2367). Declaring `impl
                // ActorMsg for T` records a message-envelope binding but
                // does not, by itself, wire delivery — no receive fn is
                // ever resolved to receive the message. Admitting this
                // case let it reach HIR lowering with no
                // `method_call_rewrites` / `actor_method_dispatch` entry
                // and fail closed there with an internal
                // `MethodCallNoRewrite` diagnostic instead of an
                // actionable one. Reject uniformly here, whether or not
                // the actor declares `impl ActorMsg` — same diagnostic
                // as the no-envelope case.
                if method == "send"
                    && !has_user_send_handler
                    && !self.checking_canonical_stdlib_source("std.builtins")
                {
                    for arg in args {
                        let (expr, sp) = arg.expr();
                        self.synthesize(expr, sp);
                    }
                    let actor_hint = resolved
                        .as_local_actor_ref()
                        .and_then(|inner| {
                            if let Ty::Named { head, .. } = inner {
                                Some(head.registry_key().to_string())
                            } else {
                                None
                            }
                        })
                        .unwrap_or_else(|| "this actor".to_string());
                    self.report_error(
                        TypeErrorKind::UndefinedMethod,
                        span,
                        format!(
                            "no `send` handler on `{actor_hint}` — declare \
                             `receive fn send(...)` to accept it, or call a \
                             named handler: `ref.method_name(payload)`"
                        ),
                    );
                    return Ty::Error;
                }
                // Try the actor handle's own methods first.
                if !has_user_send_handler {
                    if let Ty::Named {
                        args: receiver_args,
                        ..
                    } = resolved
                    {
                        if let Some(sig) = self.lookup_named_method_sig(
                            actor_ref_builtin.canonical_name(),
                            receiver_args,
                            method,
                        ) {
                            let applied_sig = self.apply_instantiated_call_signature(
                                &sig,
                                None,
                                args,
                                span,
                                SignatureArgApplication::PositionalOnly {
                                    arity_context: format!("method `{method}`"),
                                },
                                true,
                                Some(GenericCallee::Method {
                                    type_name: actor_ref_builtin.canonical_name(),
                                    method,
                                    owner_type_args: receiver_args,
                                }),
                            );
                            if method == "send"
                                && !self.checking_canonical_stdlib_source("std.builtins")
                            {
                                self.enforce_actor_method_send_args(args);
                            }
                            return applied_sig.return_type;
                        }
                    }
                }
                // Fall through to actor receive-fn dispatch on the inner type.
                let inner = resolved.as_local_actor_ref().unwrap();
                if let Ty::Named {
                    head,
                    args: actor_type_args,
                } = inner
                {
                    let actor_name = head.registry_key();
                    // An annotation-derived `Account` actor-handle type carries
                    // the actor's bare name directly; resolve it to the
                    // registered actor identity (current module's actor, root actor, or a
                    // unique module export) before keying `fn_sigs`. Spawn-
                    // derived handles already carry the dotted identity.
                    let actor_identity = if self.has_fn_sig(&format!("{actor_name}::{method}")) {
                        actor_name.to_string()
                    } else if let BareActorResolution::Resolved(identity) =
                        self.resolve_bare_actor_identity(actor_name)
                    {
                        identity
                    } else {
                        actor_name.to_string()
                    };
                    let method_key = format!("{actor_identity}::{method}");
                    // A plain (non-receive) `fn` on the actor lands in `fn_sigs` under the
                    // same `{identity}::{method}` key as a `receive fn` handler (see
                    // `register_actor_base`), but only `register_receive_fn` adds to
                    // `actor_receive_methods`. A key present in the former but absent from
                    // the latter names an internal method with no mailbox-handler shape —
                    // MIR has no `ActorHandlerLayout` row for it (#2366). Reject here,
                    // fail-closed, instead of deferring to a MIR NotYetImplemented.
                    if self.has_fn_sig(&method_key)
                        && !self.actor_receive_methods.contains(&method_key)
                    {
                        for arg in args {
                            let (expr, sp) = arg.expr();
                            self.synthesize(expr, sp);
                        }
                        self.report_error(
                            TypeErrorKind::UndefinedMethod,
                            span,
                            format!(
                                "`{method}` is an internal actor method, not a message \
                                 handler — declare it `receive fn` to expose it"
                            ),
                        );
                        return Ty::Error;
                    }
                    if let Some(sig) =
                        self.lookup_named_method_sig(&actor_identity, actor_type_args, method)
                    {
                        // Route through the one application authority rather
                        // than checking args against `sig.params` directly: a
                        // generic `receive fn keep<T>(..)` needs its type
                        // parameters freshened and inferred from the arguments,
                        // and its instantiation recorded so structural-equality
                        // obligations raised in the handler body are discharged.
                        // The hand-rolled loop that used to live here skipped
                        // both, so a generic handler reported `expected T` at
                        // every call site.
                        let applied_sig = self.apply_instantiated_call_signature(
                            &sig,
                            None,
                            args,
                            span,
                            SignatureArgApplication::FunctionLike {
                                param_names: &sig.param_names,
                                arity_context: format!("method `{method}`"),
                            },
                            true,
                            Some(GenericCallee::Method {
                                type_name: &actor_identity,
                                method,
                                owner_type_args: actor_type_args,
                            }),
                        );
                        // Every argument crosses the mailbox boundary. This is
                        // the funnel-compatible pairing (it reads the per-arg
                        // types the application just published) used by the bare
                        // actor-instance dispatch arm.
                        self.enforce_actor_method_send_args(args);
                        self.record_method_call_receiver_kind(
                            span,
                            MethodCallReceiverKind::ActorInstance {
                                actor_name: actor_identity.clone(),
                            },
                        );
                        let call_ty = self.record_actor_method_dispatch(
                            span,
                            method_key,
                            applied_sig.return_type.clone(),
                        );
                        return call_ty;
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
                                arity_context: format!("method `{method}`"),
                            },
                            true,
                            Some(GenericCallee::Method {
                                type_name: crate::BuiltinType::RemotePid.canonical_name(),
                                method,
                                owner_type_args: receiver_args,
                            }),
                        );
                        let return_type = if method == "ask" {
                            self.project_assoc_types(&applied_sig.return_type)
                        } else {
                            applied_sig.return_type.clone()
                        };
                        if matches!(method, "send" | "ask")
                            && !self.checking_canonical_stdlib_source("std.builtins")
                        {
                            // `RemotePid<T>::send` / `::ask` route to the native
                            // mesh transport (`hew_node_api_send_location` /
                            // `hew_remote_call_*`), which is not compiled for
                            // wasm32. Reject at check time so remote messaging
                            // fails closed with a structured diagnostic instead
                            // of compiling to a module that imports undefined
                            // native send symbols and traps at instantiation.
                            self.reject_wasm_feature(span, WasmUnsupportedFeature::Distributed);
                            self.enforce_actor_method_send_args(args);
                            if let Some(actor) = receiver_args.first() {
                                self.check_remote_actor_payloads(actor, method == "ask", span);
                            }
                            self.method_call_rewrites.insert(
                                SpanKey::in_module(span, self.current_module_idx),
                                if method == "ask" {
                                    MethodCallRewrite::RemoteActorAsk
                                } else {
                                    MethodCallRewrite::RemoteActorSend
                                },
                            );
                        }
                        if let Some(c_symbol) = match method {
                            "location" => Some("hew_remote_pid_location"),
                            "node_id" => Some("hew_remote_pid_node_id"),
                            "slot" => Some("hew_remote_pid_slot"),
                            "incarnation" => Some("hew_remote_pid_incarnation"),
                            "display" => Some("hew_remote_pid_display"),
                            _ => None,
                        } {
                            self.record_runtime_method_call_rewrite(span, c_symbol);
                        }
                        return return_type;
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
            // actor(M) -> R: lambda-actor handle.
            //
            // Methods: .send(msg) / .close()
            //
            // Call-syntax `handle(msg)` is the canonical lambda-actor surface;
            // `.send(msg)` is an allowed-secondary tell surface. A lambda actor
            // is NOT a channel: it has no `.recv()` / `.try_recv()` /
            // `.send_half()` / `.recv_half()` surface (the caller never reads the
            // mailbox, and an actor cannot be split in two). The reply (for an
            // ask-shaped actor) is delivered through the call-site Result, never a
            // separate `.recv()`.
            (
                Ty::Named {
                    head: crate::TypeHead::Builtin(BuiltinType::ActorFn),
                    args: type_args,
                    ..
                },
                _,
            ) => {
                self.check_lambda_pid_method(type_args, &receiver_ty, receiver, method, args, span)
            }
            // String methods are declared in `std/string.hew` with
            // monomorphic `#[extern_symbol]` annotations.
            (Ty::String, _) => self.dispatch_string_method(method, args, span),
            // Generator methods route through the Iterator contract:
            // .next() returns Option<yielded type>.
            (
                Ty::Named {
                    head: crate::TypeHead::Builtin(BuiltinType::Generator),
                    args: type_args,
                    ..
                },
                "next",
            ) => {
                let yield_ty = type_args
                    .first()
                    .cloned()
                    .unwrap_or(Ty::Var(TypeVar::fresh()));
                // Record the consumption rewrite so HIR lowering emits the
                // dedicated `GeneratorNext` node (codegen drives `hew_gen_next`
                // and unboxes the result into `Option<yield_ty>`). Without this
                // entry, HIR rejects the call with `MethodCallNoRewrite`.
                //
                // `materialize_literal_defaults` collapses any residual
                // `IntLiteral`/`FloatLiteral` yield type to its concrete default
                // (i64/f64): a `gen { yield 7; 0 }` yields an unconstrained
                // integer literal, and `ResolvedTy::from_ty` rejects an
                // unmaterialized literal — so without the default the rewrite
                // would be silently skipped and HIR would reject the call.
                let resolved_yield = self.subst.resolve(&yield_ty).materialize_literal_defaults();
                if let Ok(yield_resolved) = ResolvedTy::from_ty(&resolved_yield) {
                    self.record_method_call_rewrite(
                        span,
                        MethodCallRewrite::GeneratorNext {
                            yield_ty: yield_resolved,
                        },
                    );
                }
                Ty::option(yield_ty)
            }
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
                    head: crate::TypeHead::Builtin(BuiltinType::Stream),
                    args: type_args,
                    ..
                },
                _,
            ) => self.check_stream_method(type_args, method, args, span),
            // Sink<T> methods
            (
                Ty::Named {
                    head: crate::TypeHead::Builtin(BuiltinType::Sink),
                    args: type_args,
                    ..
                },
                _,
            ) => {
                let inner = Self::stream_element_type(type_args);
                // Gate 2: lowering-capability check.  Only string and bytes have
                // runtime symbols; other Wire-capable types pass gate 1 but cannot
                // be lowered yet.  Emit a user-facing diagnostic rather than the
                // ICE-flavoured "missing runtime rewrite metadata" from
                // require_builtin_runtime_symbol.
                let resolved_inner = self.subst.resolve(&inner);
                if !matches!(resolved_inner, Ty::Var(_))
                    && !self.queue_elem_admissible(&resolved_inner)
                {
                    let reason = self.queue_elem_rejection_reason(&resolved_inner);
                    self.report_error(
                        TypeErrorKind::InvalidOperation,
                        span,
                        format!("`Sink<{}>` is not supported: {reason}", inner.user_facing()),
                    );
                    return Ty::Error;
                }
                let receiver_ty = Ty::sink(inner.clone());
                match method {
                    // `send` parks on a full pipe and reports `SendError.Closed`
                    // once the reader is gone; `try_send` never parks and adds
                    // `SendError.Full`. Both carry every describable element
                    // through the layout witness; the element identity rides
                    // the checked value type, never the symbol.
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
                            ),
                        ) else {
                            return Ty::Error;
                        };
                        self.record_runtime_method_call_rewrite(span, c_symbol);
                        sig.return_type
                    }
                    // `clone` adds a producer handle on the same pipe; `finish`
                    // publishes EOF and keeps the handle; `close` finishes and
                    // releases it.
                    "clone" | "finish" | "close" => {
                        let Some(c_symbol) = self.require_builtin_runtime_symbol(
                            span,
                            BuiltinNamedType::Sink.canonical_name(),
                            method,
                            crate::stdlib::resolve_stream_method(
                                BuiltinNamedType::Sink.canonical_name(),
                                method,
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
            // Range<T> iterator adapters: `.rev()` (descending iteration) and
            // `.step_by(k)` (strided iteration).  Both return `Range<T>` so they
            // compose (`(0..=10).rev().step_by(3)`) and feed the for-loop's
            // `Range<T>` element-type extraction unchanged.  Crucially the
            // returned type reuses the receiver's element `T` (not a fresh var),
            // so the #1857 `deferred_range_bounds` i64-defaulting still resolves
            // an unconstrained `(0..n).rev()` exactly as a bare range would.
            (
                Ty::Named {
                    head: crate::TypeHead::Builtin(BuiltinType::Range),
                    args: range_args,
                    ..
                },
                "rev" | "step_by",
            ) => {
                let elem_ty = range_args.first().cloned().unwrap_or(Ty::I64);
                let range_ty = Ty::range(elem_ty.clone());
                if method == "rev" {
                    self.check_arity(args, 0, "`Range.rev`", span);
                } else {
                    self.check_arity(args, 1, "`Range.step_by`", span);
                    if let Some(arg) = args.first() {
                        let (expr, sp) = arg.expr();
                        // The stride is counted in the range's element type so a
                        // `for i in (0u32..10).step_by(2)` strides in `u32`.
                        let _step_ty = self.check_against(expr, sp, &elem_ty);
                        // Fail-closed: reject a statically-known non-positive
                        // step at compile time.  A zero step would spin forever
                        // and a negative step is meaningless for an unsigned
                        // stride magnitude; `.rev()` is the descending form.
                        // A non-literal step is validated at runtime (MIR traps
                        // on a zero step before entering the loop).
                        if let Some(value) = Self::literal_integer_value(expr) {
                            if value <= 0 {
                                self.report_error(
                                    TypeErrorKind::InvalidOperation,
                                    span,
                                    format!(
                                        "`step_by` requires a positive step; `{value}` is not \
                                         allowed (use `.rev()` for descending iteration)"
                                    ),
                                );
                            }
                        }
                    }
                }
                range_ty
            }
            // User-defined struct/actor methods from type_defs
            (
                Ty::Named {
                    head,
                    args: type_args,
                },
                _,
            ) => {
                let name = head.registry_key();
                let builtin = &head.builtin();
                let canonical_receiver_name = self
                    .canonical_nominal_name(name)
                    .unwrap_or_else(|| name.to_string());
                // Builtin `Result<T, E>` / `Option<T>` receivers (e.g. the
                // `Result<T, AskError>` wrapper an actor ask produces) resolve
                // their methods against the origin-based stdlib snapshot ONLY,
                // never the user `type_defs`/`fn_sigs`. A user package that
                // declares its own `type Result`/`type Option` registers its
                // methods under the same bare `Result::<method>` keys in
                // `fn_sigs`; resolving a builtin receiver through
                // `lookup_named_method_sig` would return whichever collided last
                // by registration order — e.g. a user `fn is_ok(self) -> i64`
                // shadowing the builtin `bool`-returning `is_ok`, producing an
                // ill-typed call codegen-front rejects. Confining the lookup to
                // `builtin_result_option_method_sigs` selects the canonical
                // builtin method (and its `extern_symbol` rewrite) for ALL
                // methods; any method absent from the builtin surface yields
                // `None` and falls through to the `no method on
                // Result<...>`/`Option<...>` diagnostic below.
                let sig = match builtin {
                    Some(b @ (BuiltinType::Result | BuiltinType::Option)) => {
                        self.lookup_builtin_result_option_method_sig(*b, type_args, method)
                    }
                    _ => self.lookup_named_method_sig(&canonical_receiver_name, type_args, method),
                };
                if let Some(sig) = sig {
                    if sig.requires_mutable_receiver {
                        self.check_mutable_method_receiver(
                            receiver,
                            &format!("method `{method}` on `{name}`"),
                            span,
                        );
                    }
                    let is_actor_receive_dispatch = self
                        .type_def_at(name)
                        .is_some_and(|td| td.kind == TypeDefKind::Actor)
                        && self
                            .actor_receive_methods
                            .contains(&format!("{name}::{method}"));
                    let applied_sig = self.apply_instantiated_call_signature(
                        &sig,
                        None,
                        args,
                        span,
                        SignatureArgApplication::FunctionLike {
                            param_names: &sig.param_names,
                            arity_context: format!("method `{method}`"),
                        },
                        true,
                        Some(GenericCallee::Method {
                            type_name: &canonical_receiver_name,
                            method,
                            owner_type_args: type_args,
                        }),
                    );
                    // Actor receive-method dispatch on a bare actor-typed
                    // receiver — e.g. an actor field holding a reference
                    // (`let out: W; out.put(arg)`) or a `let target: Printer`
                    // binding. `lookup_named_method_sig` finds the signature in
                    // `fn_sigs` keyed `{Actor}::{method}`, but a value of bare
                    // actor type `W` is still an actor handle, not a struct: the
                    // call must cross the mailbox boundary exactly like the
                    // actor-handle arm above. Route it through
                    // the same send/ask dispatch machinery instead of falling
                    // through to the synchronous `W::method(self, ...)`
                    // `RewriteToFunction` path (which HIR cannot lower — there is
                    // no standalone callable body for a receive handler, so it
                    // surfaces as `IndirectCallUnsupported`). Non-receive
                    // `methods {}` declared on the same actor (also keyed
                    // `{Actor}::{method}` in `fn_sigs`) are NOT in
                    // `actor_receive_methods`, so they stay on the direct path.
                    if is_actor_receive_dispatch {
                        self.record_method_call_receiver_kind(
                            span,
                            MethodCallReceiverKind::ActorInstance {
                                actor_name: name.to_string(),
                            },
                        );
                        // Every arg crosses the mailbox boundary; record the
                        // per-arg alias-vs-copy decision so the fail-closed
                        // codegen consumer does not have to guess.
                        self.enforce_actor_method_send_args(args);
                        let method_key = format!("{name}::{method}");
                        // Record the dispatch discriminator (Fire vs Ask). This
                        // also marks the span as already-rewritten below, so the
                        // synchronous `RewriteToFunction` path is skipped and the
                        // call lowers to `ActorSend` / `ActorAsk` in HIR.
                        let call_ty = self.record_actor_method_dispatch(
                            span,
                            method_key,
                            applied_sig.return_type.clone(),
                        );
                        return call_ty;
                    }
                    self.record_method_call_receiver_kind(
                        span,
                        MethodCallReceiverKind::NamedTypeInstance {
                            type_name: name.to_string(),
                        },
                    );
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
                        .type_def_at(name)
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
                                    Expr::Ident(n) => Some(*n),
                                    _ => None,
                                };
                                let receiver_is_mutable = receiver_binding_name
                                    .map(|ident| ident.name.as_str())
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
                                    self.env.discount_mutation_receiver_read(n.name.as_str());
                                    self.env.mark_written(n.name.as_str());
                                    self.reject_borrowed_parameter_mutation(
                                        n.name.as_str(),
                                        &[],
                                        span,
                                    );
                                }
                                self.machine_method_dispatch.insert(
                                    SpanKey::in_module(span, self.current_module_idx),
                                    MachineMethodKind::Step {
                                        machine_name: canonical_receiver_name.clone(),
                                    },
                                );
                            }
                            "state_name" => {
                                self.machine_method_dispatch.insert(
                                    SpanKey::in_module(span, self.current_module_idx),
                                    MachineMethodKind::StateName {
                                        machine_name: canonical_receiver_name.clone(),
                                    },
                                );
                            }
                            "take_emits" => {
                                self.machine_method_dispatch.insert(
                                    SpanKey::in_module(span, self.current_module_idx),
                                    MachineMethodKind::TakeEmits {
                                        machine_name: canonical_receiver_name.clone(),
                                    },
                                );
                            }
                            _ => {}
                        }
                    }
                    // A terminal consuming method moves its receiver, so record
                    // the per-call-site flag for HIR/codegen AND mark the receiver
                    // expression moved (a later use surfaces `UseAfterMove`).
                    // A resource's canonical close additionally records a
                    // one-time discharge: lowering uses it to suppress the
                    // scope-exit implicit drop on the consumed path, and the
                    // checker uses it to report a second close with the
                    // specific double-close diagnostic. Discharging the
                    // obligation is a consequence of the move, never a
                    // substitute for it — close-then-use is use-after-move.
                    // Two surfaces qualify:
                    //   1. a `#[resource]` type's inherent `fn close(consume self)` — the
                    //      implicit-drop dispatch target, which when called
                    //      explicitly also moves the receiver so the scope-exit
                    //      implicit drop is suppressed on the consumed path (no
                    //      double-close).
                    //   2. any `fn m(consume self)` inherent method — the
                    //      terminal single-consume surface (a builder's
                    //      `build(consume self)`, a `#[linear]` type's consuming
                    //      method). The resolved sig carries the consume fact.
                    let consumes_receiver = sig.consumes_receiver
                        || self.named_type_method_consumes_receiver(name, method)
                        || self.named_type_inherent_close_consumes_receiver(
                            name, *builtin, method, &sig,
                        );
                    if consumes_receiver {
                        self.method_call_consumes_receiver
                            .insert(SpanKey::in_module(span, self.current_module_idx));
                        let resolved_recv = self.subst.resolve(&receiver_ty);
                        let discharges_resource = self.named_type_inherent_close_consumes_receiver(
                            name, *builtin, method, &sig,
                        );
                        let borrowed_refused =
                            self.reject_borrowed_consumption(&receiver.0, &receiver.1);
                        if discharges_resource && !borrowed_refused {
                            self.method_call_discharges_receiver
                                .insert(SpanKey::in_module(span, self.current_module_idx));
                            if let Expr::Ident(receiver_name) = &receiver.0 {
                                match self
                                    .env
                                    .mark_released(receiver_name.name.as_str(), receiver.1.clone())
                                {
                                    Some(Some(prior)) => {
                                        let mut error = TypeError::new(
                                            TypeErrorKind::UseAfterConsume,
                                            receiver.1.clone(),
                                            format!(
                                                "resource `{receiver_name}` cannot be closed more than once"
                                            ),
                                        )
                                        .with_note(prior, "resource was first closed here");
                                        if let Some(source_module) = &self.current_module {
                                            error = error.with_source_module(source_module.clone());
                                        }
                                        self.errors.push(error);
                                    }
                                    // First discharge: the close consumes its
                                    // receiver, so the move lands with it and any
                                    // later use is use-after-move. Marked directly
                                    // (not via `mark_expr_moved_if_non_copy`)
                                    // because the released flag was just set by
                                    // this very call and must not read as a prior
                                    // consumption of the receiver.
                                    Some(None)
                                        if !self.registry.implements_marker(
                                            &resolved_recv,
                                            MarkerTrait::Copy,
                                        ) =>
                                    {
                                        self.env.mark_moved(
                                            receiver_name.name.as_str(),
                                            receiver.1.clone(),
                                        );
                                    }
                                    Some(None) | None => {}
                                }
                            } else {
                                self.mark_expr_moved_if_non_copy(
                                    &receiver.0,
                                    &receiver.1,
                                    &resolved_recv,
                                );
                            }
                        } else if !borrowed_refused {
                            self.mark_expr_moved_if_non_copy(
                                &receiver.0,
                                &receiver.1,
                                &resolved_recv,
                            );
                        }
                    }
                    self.record_handle_method_call_rewrite_if_any(&resolved, method, args, span);
                    self.record_named_extern_symbol_rewrite_if_any(
                        &canonical_receiver_name,
                        type_args,
                        method,
                        &sig,
                        span,
                        &self.subst.resolve(&receiver_ty),
                    );
                    // W3.042 S2-S2: user-defined methods on named types (both
                    // inherent `impl Type { fn m(...) }` and trait `impl T for
                    // Type { fn m(...) }`) must record a `RewriteToFunction`
                    // entry naming the qualified `Type::method` symbol so HIR
                    // lowering can emit a direct `Call` (with the receiver
                    // injected as the first argument) instead of falling
                    // through to `MethodCallNoRewrite`. The qualified symbol
                    // is the same key that `hew-hir`'s pre-pass seeds into
                    // `fn_registry` (`HirImplBlock::method_symbol`), so
                    // resolution succeeds without further wiring.
                    //
                    // Skipped when an earlier helper above already recorded a
                    // rewrite (handle methods, monomorphic-extern symbols), or
                    // when a dedicated dispatch side-table will be consulted
                    // by HIR before `method_call_rewrites` (machine
                    // `step`/`state_name`, actor send/ask, dyn-trait,
                    // resolved-impl call kernel).
                    let span_key = SpanKey::in_module(span, self.current_module_idx);
                    let already_rewritten = self.method_call_rewrites.contains_key(&span_key)
                        || self.machine_method_dispatch.contains_key(&span_key)
                        || self.actor_method_dispatch.contains_key(&span_key)
                        || self.dyn_trait_method_calls.contains_key(&span_key)
                        || self.resolved_calls.contains_key(&span_key);
                    if !already_rewritten {
                        // The resolved receiver owner is executable dispatch
                        // authority. Registration publishes this exact key;
                        // never retry through the receiver's final segment.
                        // The receiver's resolved nominal owner is executable
                        // dispatch authority. The declaration map is keyed by
                        // that exact source identity even when compatibility
                        // `fn_sigs` aliases retain a shorter presentation.
                        // Never retry through either the type or method leaf.
                        let method_owner = canonical_receiver_name.as_str();
                        let method_key = format!("{method_owner}::{method}");
                        // Wire codec instance serialize methods on a `#[wire]`
                        // struct or enum. `encode` is the binary CBOR path
                        // (`value.encode() -> bytes`); `to_json`/`to_yaml` are the
                        // text path (`value.to_json() -> string`), lowered through
                        // the CBOR↔text bridge. The instance method is registered
                        // in `type_def.methods` (not `fn_sigs`), so it never
                        // matches the `fn_sigs` branch below and would otherwise
                        // fall through to `MethodCallNoRewrite`. Record a dedicated
                        // codec rewrite so HIR/codegen drive the matching thunk
                        // with the correct ABI.
                        let wire_serialize_dir =
                            if self.wire_struct_types.contains(&canonical_receiver_name)
                                || self.wire_enum_types.contains(&canonical_receiver_name)
                            {
                                match method {
                                    "encode" => Some(WireCodecDirection::Encode),
                                    "to_json" => Some(WireCodecDirection::ToJson),
                                    "to_yaml" => Some(WireCodecDirection::ToYaml),
                                    _ => None,
                                }
                            } else {
                                None
                            };
                        if let Some(direction) = wire_serialize_dir {
                            // `value_ty` is the receiver wire type (the value being
                            // serialized) regardless of the textual return type,
                            // so codegen keys the thunk by the same type the binary
                            // path uses. Carry the CANONICAL nominal: the wire
                            // layout table is keyed by the declaration identity
                            // only, and codegen's tag probe falls back to
                            // positional keys on a miss — a bare spelling here
                            // would silently change the encoded schema.
                            let mut value_source = self.subst.resolve(&resolved);
                            if let Ty::Named { head, .. } = &mut value_source {
                                if head.registry_key() != canonical_receiver_name {
                                    if let Some(canonical) = self
                                        .named_ty_for_key(&canonical_receiver_name, Vec::new())
                                        .head()
                                    {
                                        *head = canonical;
                                    }
                                }
                            }
                            if let Ok(value_ty) = ResolvedTy::from_ty(&value_source) {
                                self.record_method_call_rewrite(
                                    span,
                                    MethodCallRewrite::WireCodec {
                                        direction,
                                        value_ty,
                                    },
                                );
                            }
                        } else if matches!(*builtin, Some(BuiltinType::VecIter)) && method == "next"
                        {
                            if let Some(elem_ty) = type_args.first() {
                                if !self.record_vec_iter_element_mode(elem_ty, span) {
                                    return Ty::Error;
                                }
                                self.record_method_call_rewrite(
                                    span,
                                    MethodCallRewrite::BuiltinVecIterNext,
                                );
                            }
                        } else if self.has_fn_sig(&method_key)
                            || self.impl_method_declaration_ids.contains_key(&method_key)
                            || (!type_args.is_empty() && {
                                // Concrete-specialised-impl check (#2270): the
                                // type_args may resolve to a mangled key even
                                // when the bare key is absent (e.g. after the
                                // first concrete impl was registered and the bare
                                // key was clobbered by the second).
                                let resolved_args: Option<Vec<ResolvedTy>> = type_args
                                    .iter()
                                    .map(|ty| ResolvedTy::from_ty(&self.subst.resolve(ty)).ok())
                                    .collect();
                                resolved_args
                                    .as_ref()
                                    .and_then(|args| {
                                        crate::resolved_ty::mangle_impl_self_name(
                                            method_owner,
                                            args,
                                        )
                                    })
                                    .is_some_and(|m| self.has_fn_sig(&format!("{m}::{method}")))
                            })
                        {
                            // For concrete-specialised impls, use the mangled
                            // c_symbol so HIR looks up the right `fn_registry`
                            // entry.  Falls back to the bare key for all other
                            // cases (generic impls, inherent methods, etc.).
                            let dispatch_key = if type_args.is_empty() {
                                method_key.clone()
                            } else {
                                let resolved_args: Option<Vec<ResolvedTy>> = type_args
                                    .iter()
                                    .map(|ty| ResolvedTy::from_ty(&self.subst.resolve(ty)).ok())
                                    .collect();
                                resolved_args
                                    .as_ref()
                                    .and_then(|args| {
                                        crate::resolved_ty::mangle_impl_self_name(
                                            method_owner,
                                            args,
                                        )
                                    })
                                    .filter(|m| self.has_fn_sig(&format!("{m}::{method}")))
                                    .map_or_else(
                                        || method_key.clone(),
                                        |m| format!("{m}::{method}"),
                                    )
                            };
                            // A builtin Option/Result receiver resolved its
                            // signature from the std snapshot, which carries its
                            // declaration; the bare `Result::<method>` key can
                            // name a same-spelled user method instead.
                            let declaration = match *builtin {
                                Some(BuiltinType::Option | BuiltinType::Result) => sig
                                    .impl_method
                                    .as_ref()
                                    .map(|provenance| provenance.declaration),
                                _ => self
                                    .impl_method_declaration_ids
                                    .get(&dispatch_key)
                                    .or_else(|| self.impl_method_declaration_ids.get(&method_key))
                                    .copied(),
                            };
                            self.record_method_call_rewrite(
                                span,
                                MethodCallRewrite::RewriteToFunction {
                                    target: declaration
                                        .map_or_else(
                                            || CallTarget::Unsupported {
                                                reason: format!(
                                                    "impl method `{dispatch_key}` has no registered declaration identity"
                                                ),
                                            },
                                            CallTarget::impl_method,
                                        ),
                                    c_symbol: dispatch_key,
                                    // User-defined `Type::method` dispatch is
                                    // open-set; the typed runtime-call catalog
                                    // does not enumerate user method keys.
                                    descriptor: None,
                                    extern_identity: None,
                                    // #1295: a `#[resource]` type's inherent
                                    // `close(self)` is a terminal handle-release
                                    // consume — HIR lowers the receiver with
                                    // `IntentKind::Consume` so MIR marks it
                                    // `Consumed` and suppresses the duplicate
                                    // scope-exit implicit drop. The `consumes`
                                    // flag was computed above (resource close or
                                    // a consuming trait method flattened onto
                                    // this type); other inherent/trait methods
                                    // are not consuming releases.
                                    consumes_receiver,
                                    requires_mutable_receiver: sig.requires_mutable_receiver,
                                    receiver_update: sig.receiver_update,
                                    returns_receiver_identity: sig.returns_receiver_identity,
                                },
                            );
                        }
                    }
                    return self.qualify_method_return_to_receiver_owner(
                        &canonical_receiver_name,
                        &applied_sig.return_type,
                    );
                }
                // Type-parameter method dispatch: resolve from trait bounds.
                // When the receiver is a generic type parameter (e.g. `T` in
                // `fn report<T: Measurable>(item: T)`), look up the method
                // from the traits that bound that parameter.
                //
                // Algorithm (origin-aware supertrait expansion):
                // 1. For each bound, call lookup_trait_method_with_origin → (declaring_trait, sig)
                // 2. Collect all hits, deduplicate by declaring_trait
                // 3. 0 hits → UndefinedMethod, >1 distinct declaring traits → AmbiguousTraitMethod,
                //    1 → record StaticTraitDispatch rewrite
                let bounds_for_type_param = self.current_function.as_ref().and_then(|fn_name| {
                    self.fn_sig(fn_name).and_then(|sig| {
                        if sig.type_params.iter().any(|param| param == name) {
                            sig.type_param_bounds.get(name).cloned()
                        } else {
                            None
                        }
                    })
                });
                if let Some(bounds) = bounds_for_type_param {
                    // Expand all bounds into (bound_trait, declaring_trait, sig) tuples.
                    // For each bound, also walk its supertrait DAG to collect every
                    // trait that DIRECTLY declares the method — this catches the
                    // supertrait-redeclaration case (plan §4 V14) where a bound
                    // `T: B` with `trait B: A` and both A and B declaring the same
                    // method reaches two distinct declaring traits.
                    let mut hits: Vec<(String, String, FnSig)> = Vec::new();
                    for bound_trait in &bounds {
                        // Keep the source spelling for diagnostics, but resolve
                        // the dispatch lookup through the declaration owner.
                        // An imported alias such as `AlphaRender` is not a
                        // declaration identity and must never reach HIR as one.
                        let bound_trait_key = self.trait_ref_lookup_key(bound_trait);
                        let declaring =
                            self.collect_all_declaring_traits_for_method(&bound_trait_key, method);
                        for declaring_trait in declaring {
                            // Resolve the sig from the declaring trait directly.
                            if let Some((_, sig)) =
                                self.lookup_trait_method_with_origin(&declaring_trait, method)
                            {
                                hits.push((bound_trait.clone(), declaring_trait, sig));
                            }
                        }
                    }
                    // Deduplicate by declaring_trait — same origin via multiple bounds is NOT ambiguous.
                    hits.sort_by(|a, b| a.1.cmp(&b.1));
                    hits.dedup_by_key(|h| h.1.clone());

                    if hits.len() == 1 {
                        let (bound_trait, declaring_trait, mut trait_sig) =
                            hits.into_iter().next().unwrap();
                        // Replace `Self` references with the type parameter type.
                        let self_ty = resolved.clone();
                        for param_ty in &mut trait_sig.params {
                            *param_ty = param_ty.substitute_named_param("Self", &self_ty);
                        }
                        trait_sig.return_type = trait_sig
                            .return_type
                            .substitute_named_param("Self", &self_ty);
                        if trait_sig.requires_mutable_receiver {
                            self.check_mutable_method_receiver(
                                receiver,
                                &format!("trait method `{declaring_trait}.{method}` (statically dispatched on type parameter `{name}`)"),
                                span,
                            );
                        }
                        let applied_sig = self.apply_instantiated_call_signature(
                            &trait_sig,
                            None,
                            args,
                            span,
                            SignatureArgApplication::FunctionLike {
                                param_names: &trait_sig.param_names,
                                arity_context: format!("method `{method}`"),
                            },
                            true,
                            Some(GenericCallee::Method {
                                type_name: &declaring_trait,
                                method,
                                owner_type_args: &[],
                            }),
                        );
                        if declaring_trait == "std.builtins.Pid" && method == "send" {
                            // TODO(A640): replace this fail-closed branch with
                            // a first-class `P::Msg: Serializable` projection
                            // bound once the checker can express that shape on
                            // pid-polymorphic call sites. If the projection is
                            // already concretely bound (for example
                            // `P: Pid<Msg = Ping>`), the regular Serializable
                            // gate below proves it and the call may proceed.
                            if !self.enforce_pid_polymorphic_send_serializable_args(args, name) {
                                return Ty::Error;
                            }
                            self.enforce_actor_method_send_args(args);
                        }
                        self.record_method_call_receiver_kind(
                            span,
                            MethodCallReceiverKind::NamedTypeInstance {
                                type_name: name.to_string(),
                            },
                        );
                        if trait_sig.consumes_receiver {
                            self.method_call_consumes_receiver
                                .insert(SpanKey::in_module(span, self.current_module_idx));
                            let resolved_ty = self.subst.resolve(&receiver_ty);
                            self.mark_expr_moved_if_non_copy(
                                &receiver.0,
                                &receiver.1,
                                &resolved_ty,
                            );
                        }
                        // Record the StaticTraitDispatch rewrite for HIR consumption.
                        let target = self
                            .trait_method_call_target_ids(&declaring_trait, method)
                            .or_else(|| self.trait_method_call_target_ids(&bound_trait, method))
                            .map_or_else(
                                || CallTarget::Unsupported {
                                    reason: format!(
                                        "trait method `{declaring_trait}.{method}` has no registered declaration identity"
                                    ),
                                },
                                |(declaring_trait, method)| CallTarget::StaticTraitMethod {
                                    declaring_trait,
                                    method,
                                },
                            );
                        self.record_method_call_rewrite(
                            span,
                            MethodCallRewrite::StaticTraitDispatch {
                                target,
                                receiver_type_param: name.to_string(),
                                requires_mutable_receiver: trait_sig.requires_mutable_receiver,
                                consumes_receiver: trait_sig.consumes_receiver,
                                returns_receiver_identity: trait_sig.returns_receiver_identity,
                            },
                        );
                        return self.project_assoc_types(&applied_sig.return_type);
                    } else if hits.len() > 1 {
                        // Multiple distinct declaring traits → ambiguous.
                        for arg in args {
                            let (expr, sp) = arg.expr();
                            self.synthesize(expr, sp);
                        }
                        let declaring_traits: Vec<&str> =
                            hits.iter().map(|h| h.1.as_str()).collect();
                        self.report_error(
                            TypeErrorKind::AmbiguousTraitMethod,
                            span,
                            format!(
                                "ambiguous trait method `{method}` on `{}`: method is declared by \
                                 multiple traits ({}); qualify the call to disambiguate",
                                resolved.user_facing(),
                                declaring_traits.join(", ")
                            ),
                        );
                        return Ty::Error;
                    }
                    // hits.is_empty() → fall through to UndefinedMethod below.
                }
                // Fn-typed field call: `w.cb(args)` where `cb` is a record
                // field of function type dispatches as a field-load +
                // closure call, not a method lookup. Pre-validated (arity +
                // per-arg types against the field signature) and recorded as
                // a structured rewrite so HIR never guesses
                // (`checker-codegen-pattern-contract`). A field that exists
                // but is NOT fn-typed falls through to `UndefinedMethod`.
                if let Some(ret_ty) = self.try_record_fn_field_call(&resolved, method, args, span) {
                    return ret_ty;
                }
                // `clone` on a user-defined named type: intercept before
                // `UndefinedMethod` for admissible records.
                // This arm handles the (Ty::Named { head: crate::TypeHead::Nominal(_) | crate::TypeHead::Param(_) | crate::TypeHead::Unresolved(_), .. }, "clone")
                // case where `try_resolve_named_method` found no `clone` in fn_sigs.
                if method == "clone" && args.is_empty() {
                    if let Ty::Named {
                        head:
                            head @ (crate::TypeHead::Nominal(_)
                            | crate::TypeHead::Param(_)
                            | crate::TypeHead::Unresolved(_)),
                        args: type_args,
                        ..
                    } = &resolved
                    {
                        let name = head.registry_key();
                        match self.record_clone_admissibility(name, type_args, span) {
                            RecordCloneAdmissibility::Admissible => {
                                self.record_method_call_rewrite(
                                    span,
                                    MethodCallRewrite::RecordCloneInplace {
                                        record_name: name.to_string(),
                                    },
                                );
                                // Bare-seed monomorphic records only; a generic
                                // instantiation is MIR-keyed by its mono layout
                                // and seeded from the `RecordCloneInplace` walk
                                // in codegen (see the sibling clone intercept).
                                if type_args.is_empty()
                                    && !self.user_clone_record_seeds.iter().any(|seed| seed == name)
                                {
                                    self.user_clone_record_seeds.push(name.to_string());
                                }
                                return resolved;
                            }
                            RecordCloneAdmissibility::OpaqueField {
                                opaque_name,
                                member,
                            } => {
                                self.report_error(
                                    TypeErrorKind::UndefinedMethod,
                                    span,
                                    format!(
                                        "type `{name}` cannot be cloned because member `{member}` \
                                         contains opaque value `{opaque_name}`"
                                    ),
                                );
                                return Ty::Error;
                            }
                            RecordCloneAdmissibility::AffineValue {
                                type_name,
                                marker,
                                member,
                            } => {
                                let receiver_name = resolved.user_facing().to_string();
                                self.report_affine_record_clone_error(
                                    &receiver_name,
                                    &type_name,
                                    marker,
                                    &member,
                                    span,
                                );
                                return Ty::Error;
                            }
                            RecordCloneAdmissibility::MissingClone { member, member_ty } => {
                                self.report_error(
                                    TypeErrorKind::UndefinedMethod,
                                    span,
                                    format!(
                                        "type `{}` cannot be cloned because member `{member}` of \
                                         type `{}` has no Clone capability",
                                        resolved.user_facing(),
                                        member_ty.user_facing()
                                    ),
                                );
                                return Ty::Error;
                            }
                            RecordCloneAdmissibility::GenericRecord => {
                                self.report_error(
                                    TypeErrorKind::UndefinedMethod,
                                    span,
                                    format!(
                                        "cloning generic record `{name}` is not yet \
                                         supported; only monomorphic records can be cloned"
                                    ),
                                );
                                return Ty::Error;
                            }
                            RecordCloneAdmissibility::AbstractParamClone => {
                                // Bare type param `x: T` with `T: Clone`; defer
                                // the concrete copy path to monomorphization. No
                                // seed: `T` names no monomorphic record layout.
                                self.record_method_call_rewrite(
                                    span,
                                    MethodCallRewrite::RecordCloneInplace {
                                        record_name: name.to_string(),
                                    },
                                );
                                return resolved;
                            }
                            RecordCloneAdmissibility::EnumClone { enum_name } => {
                                // User enum: same rewrite + HIR node as a record
                                // clone; MIR demuxes by the resolved layout and
                                // emits `EnumCloneInplace`. No bare-name seed —
                                // the MIR thunk registry's `collect_enum_clone_inplace_seeds`
                                // keys the per-mono helper.
                                self.record_method_call_rewrite(
                                    span,
                                    MethodCallRewrite::RecordCloneInplace {
                                        record_name: enum_name,
                                    },
                                );
                                return resolved;
                            }
                            RecordCloneAdmissibility::NotARecord => {
                                // Fall through to UndefinedMethod below.
                            }
                        }
                    }
                }
                // Synthesize args even if method unknown (for error recovery)
                for arg in args {
                    let (expr, sp) = arg.expr();
                    self.synthesize(expr, sp);
                }
                // `unwrap` said nothing about why the value had to be there;
                // point at the surfaces that do.
                let suggestions = if method == "unwrap"
                    && matches!(
                        resolved,
                        Ty::Named {
                            head: crate::TypeHead::Builtin(
                                BuiltinType::Option | BuiltinType::Result
                            ),
                            ..
                        }
                    ) {
                    vec![
                        "expect(\"reason\") to state why the value must be there".to_string(),
                        "`?` to propagate the failure to the caller".to_string(),
                        "`??` to supply a default".to_string(),
                        "`handle` to recover".to_string(),
                    ]
                } else {
                    self.similar_methods(&resolved, method)
                };
                self.report_error_with_suggestions(
                    TypeErrorKind::UndefinedMethod,
                    span,
                    format!("no method `{method}` on `{}`", resolved.user_facing()),
                    suggestions,
                );
                Ty::Error
            }
            // Trait object method dispatch: the method's slot in the whole
            // trait object's layout, the one numbering the coercion shares.
            (Ty::TraitObject { traits }, _) => {
                let Some(layout) = self.dyn_layout(traits, span) else {
                    for arg in args {
                        let (expr, sp) = arg.expr();
                        self.synthesize(expr, sp);
                    }
                    return Ty::Error;
                };
                // Every slot of that name, as the static-bound path collects
                // them for `T: A + B` (plan §4 V14).
                let mut matching: Vec<_> = layout
                    .into_iter()
                    .filter(|slot| slot.method_name == method)
                    .collect();
                if matching.len() > 1 {
                    for arg in args {
                        let (expr, sp) = arg.expr();
                        self.synthesize(expr, sp);
                    }
                    self.report_error(
                        TypeErrorKind::AmbiguousTraitMethod,
                        span,
                        format!(
                            "ambiguous trait method `{method}` on `{}`: traits {} each declare \
                             it, and a call on a trait object cannot name one trait; rename \
                             the method in all but one of them",
                            resolved.user_facing(),
                            matching
                                .iter()
                                .map(|slot| format!("`{}`", slot.trait_spelling))
                                .collect::<Vec<_>>()
                                .join(" and ")
                        ),
                    );
                    return Ty::Error;
                }
                if let Some(layout_slot) = matching.pop() {
                    let bound = &traits[layout_slot.bound];
                    let Some(mut sig) = self.lookup_trait_method(&layout_slot.trait_key, method)
                    else {
                        // JUSTIFIED: the layout lists only methods registered
                        // in `trait_defs`, which always resolve.
                        unreachable!(
                            "trait method `{}.{method}` is in a dyn layout but is not resolvable",
                            layout_slot.trait_key
                        );
                    };
                    let pid_send_dispatch =
                        layout_slot.trait_key == "std.builtins.Pid" && method == "send";
                    self.record_method_call_receiver_kind(
                        span,
                        MethodCallReceiverKind::TraitObject {
                            trait_name: bound.trait_name.clone(),
                        },
                    );
                    // Apply trait-type-param and associated-type substitution
                    // up front so the substituted `FnSig` is recorded on
                    // `DynMethodCall` alongside the slot; codegen never
                    // re-derives it from the impl fn or the vtable entries.
                    self.apply_trait_object_bound_substitutions(&mut sig, bound);
                    if sig.requires_mutable_receiver {
                        self.check_mutable_method_receiver(
                            receiver,
                            &format!("method `{method}` on `dyn {}`", bound.trait_name),
                            span,
                        );
                    }
                    self.dyn_trait_method_calls.insert(
                        SpanKey::in_module(span, self.current_module_idx),
                        crate::check::types::DynMethodCall {
                            target: CallTarget::DynamicVtable {
                                declaring_trait: layout_slot.declaring_trait,
                                method: layout_slot.method,
                                slot: layout_slot.slot,
                            },
                            trait_name: bound.trait_name.clone(),
                            method_name: method.to_string(),
                            slot: layout_slot.slot,
                            signature: sig.clone(),
                        },
                    );
                    if sig.consumes_receiver {
                        self.method_call_consumes_receiver
                            .insert(SpanKey::in_module(span, self.current_module_idx));
                        let resolved_ty = self.subst.resolve(&receiver_ty);
                        self.mark_expr_moved_if_non_copy(&receiver.0, &receiver.1, &resolved_ty);
                    }
                    let applied_sig = self.apply_instantiated_call_signature(
                        &sig,
                        None,
                        args,
                        span,
                        SignatureArgApplication::FunctionLike {
                            param_names: &sig.param_names,
                            arity_context: format!("method `{method}`"),
                        },
                        true,
                        // Dynamic vtable dispatch pins no static instantiation; the concrete
                        // impl is selected at run time, so there is nothing to discharge here.
                        None,
                    );
                    if pid_send_dispatch {
                        self.enforce_actor_method_send_args(args);
                        if !self.enforce_remote_actor_method_serializable_args(args) {
                            return Ty::Error;
                        }
                    }
                    applied_sig.return_type
                } else {
                    for arg in args {
                        let (expr, sp) = arg.expr();
                        self.synthesize(expr, sp);
                    }
                    let message = if resolved.is_numeric()
                        && method.starts_with("to_")
                        && matches!(
                            &method["to_".len()..],
                            "i8" | "i16"
                                | "i32"
                                | "i64"
                                | "isize"
                                | "u8"
                                | "u16"
                                | "u32"
                                | "u64"
                                | "usize"
                                | "f32"
                                | "f64"
                        ) {
                        format!(
                            "no method `.{method}()` on numeric type `{}`; use `as` for numeric casts \
                             or `.try_to_<W>()` for exact fallible conversion",
                            resolved.user_facing()
                        )
                    } else if method == "clone" && args.is_empty() {
                        // A trait object is a two-word fat pointer whose
                        // concrete type is erased. Its vtable carries only
                        // `drop_in_place`/`size_of`/`align_of` plus the trait's
                        // own methods (see `hew-runtime/src/trait_object.rs`),
                        // so no slot can reproduce the concrete value. Naming
                        // the limit and the supported ordering keeps this a
                        // user-facing rejection instead of a reinterpretation
                        // of the fat pointer as the concrete layout.
                        //
                        // WHY (shim): duplicating a `dyn Trait` needs a clone
                        // slot in the vtable prefix, which renumbers every
                        // method slot across the runtime, MIR, and codegen.
                        // WHEN-OBSOLETE: when the trait-object ABI grows that
                        // slot and every coercion site emits a clone thunk.
                        // WHAT (real solution): a `clone_in_place` vtable entry
                        // emitted alongside `drop_in_place`.
                        format!(
                            "`clone` is not supported on `{ty}`: a trait object erases its \
                             concrete type and its vtable carries no clone slot; clone the \
                             concrete value before erasing it \
                             (`let copy: {ty} = clone original;`)",
                            ty = resolved.user_facing(),
                        )
                    } else {
                        format!("no method `{method}` on `{}`", resolved.user_facing())
                    };
                    self.report_missing_method_with_shadow_note(receiver, method, span, message);
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
                // `clone` on a Copy/BitCopy type: warn (non-fatal) and return
                // the operand type. The value is already a copy — no extra work
                // needed. HIR lowers this as a plain read via `CopyCloneNoop`.
                // LESSONS: `fail-closed-never-fail-open` (exit 0, not Ty::Error).
                if method == "clone" && args.is_empty() {
                    let is_copy_ty = matches!(
                        &resolved,
                        Ty::I8
                            | Ty::I16
                            | Ty::I32
                            | Ty::I64
                            | Ty::U8
                            | Ty::U16
                            | Ty::U32
                            | Ty::U64
                            | Ty::Isize
                            | Ty::Usize
                            | Ty::F32
                            | Ty::F64
                            | Ty::Bool
                            | Ty::Char
                    );
                    if is_copy_ty {
                        let module = self.current_module.clone();
                        self.emit_main_pass_lint(
                            LintId::CloneOnCopy,
                            span,
                            module.as_deref(),
                            format!(
                                "cloning a Copy type `{}` is redundant; \
                                 this is equivalent to a plain copy",
                                resolved.user_facing()
                            ),
                            "remove the `clone` — Copy types are duplicated automatically"
                                .to_string(),
                        );
                        self.record_method_call_rewrite(span, MethodCallRewrite::CopyCloneNoop);
                        return resolved;
                    }
                }
                for arg in args {
                    let (expr, sp) = arg.expr();
                    self.synthesize(expr, sp);
                }
                let message = if resolved.is_numeric()
                    && method.starts_with("to_")
                    && matches!(
                        &method["to_".len()..],
                        "i8" | "i16"
                            | "i32"
                            | "i64"
                            | "isize"
                            | "u8"
                            | "u16"
                            | "u32"
                            | "u64"
                            | "usize"
                            | "f32"
                            | "f64"
                    ) {
                    format!(
                        "no method `.{method}()` on numeric type `{}`; use `as` for numeric casts \
                         or `.try_to_<W>()` for exact fallible conversion",
                        resolved.user_facing()
                    )
                } else {
                    format!("no method `{method}` on `{}`", resolved.user_facing())
                };
                self.report_missing_method_with_shadow_note(receiver, method, span, message);
                Ty::Error
            }
        }
    }

    pub(super) fn report_missing_method_with_shadow_note(
        &mut self,
        receiver: &Spanned<Expr>,
        method: &str,
        span: &Span,
        message: String,
    ) {
        let mut error = TypeError::new(TypeErrorKind::UndefinedMethod, span.clone(), message);
        if let Expr::Ident(binding) = &receiver.0 {
            if self.env.lookup_ref(binding.name.as_str()).is_some()
                && self.module_import_bindings.contains_key(&(
                    self.current_module.clone(),
                    self.current_module_idx,
                    binding.to_string(),
                ))
            {
                error = error.with_note(
                    receiver.1.clone(),
                    format!(
                        "lexical binding `{binding}` shadows the imported module; `{binding}.{method}` was resolved as a value method lookup"
                    ),
                );
            }
        }
        self.errors.push(error);
    }
}
