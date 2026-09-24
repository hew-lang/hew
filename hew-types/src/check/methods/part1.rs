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

    /// Retain scalar collection call metadata. Key admission is decided by the
    /// semantic capability service, independently of target representation.
    pub(in crate::check) fn finalize_lowering_facts(&mut self) -> HashMap<SpanKey, LoweringFact> {
        let pending = std::mem::take(&mut self.pending_lowering_facts);
        let mut result = HashMap::new();
        let mut reported_vars = HashSet::new();
        for (site, fact) in pending {
            let ty = self
                .subst
                .resolve(&fact.hashset_element_ty)
                .materialize_literal_defaults();
            if ty.contains_error() {
                continue;
            }
            let before = self.errors.len();
            if let Ty::Var(var) = &ty {
                if reported_vars.insert(*var) {
                    self.report_error(
                        TypeErrorKind::InferenceFailed,
                        &(site.start..site.end),
                        "cannot infer HashSet element type; add an explicit type annotation".into(),
                    );
                }
            } else {
                self.validate_collection_key_capabilities(&ty, "Set", &(site.start..site.end));
                if let Ok(fact) = LoweringFact::from_hashset_element_type(&ty) {
                    result.insert(site, fact);
                }
            }
            for error in &mut self.errors[before..] {
                error.source_module.clone_from(&fact.source_module);
            }
        }
        result
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

    /// Recheck built-in value-container clones after inference has settled.
    ///
    /// A call can be visited while its payload is still `Ty::Var`, then become
    /// affine through a later source branch even though that branch executes
    /// before the clone at runtime. Inline admission alone is therefore
    /// source-order dependent. This finalizer closes that gap using the same
    /// transitive affine authority as the immediate clone gate.
    pub(in crate::check) fn finalize_builtin_clone_admission(&mut self) {
        let checks = std::mem::take(&mut self.deferred_builtin_clone_admission);
        let mut new_errors = Vec::new();

        for (_span_key, check) in checks {
            let resolved = self
                .subst
                .resolve(&check.receiver_ty)
                .materialize_literal_defaults();
            if resolved.contains_error() || resolved.has_inference_var() {
                continue;
            }
            let Some(blocker) = self.structural_clone_blocker(&resolved) else {
                continue;
            };
            let receiver_name = resolved.user_facing().to_string();
            let message = match blocker {
                CloneCapabilityBlocker::Affine {
                    type_name,
                    marker,
                    member,
                } => Self::affine_record_clone_error_message(
                    &receiver_name,
                    &type_name,
                    marker,
                    &member,
                ),
                CloneCapabilityBlocker::Opaque { type_name, member } => format!(
                    "type `{receiver_name}` cannot be cloned because member `{member}` contains \
                     opaque value `{type_name}`"
                ),
                CloneCapabilityBlocker::Missing { member, member_ty } => format!(
                    "type `{receiver_name}` cannot be cloned because member `{member}` of type \
                     `{}` has no Clone capability",
                    member_ty.user_facing()
                ),
            };
            let mut err =
                crate::error::TypeError::new(TypeErrorKind::InvalidOperation, check.span, message);
            if let Some(module) = check.source_module {
                err = err.with_source_module(module);
            }
            new_errors.push(err);
        }

        self.errors.extend(new_errors);
    }

    pub(in crate::check) fn record_method_call_receiver_kind(
        &mut self,
        span: &Span,
        kind: MethodCallReceiverKind,
    ) {
        let key = SpanKey::in_module(span, self.current_module_idx);
        if matches!(
            self.method_call_receiver_kinds.get(&key),
            Some(MethodCallReceiverKind::LexicalBinding { .. })
        ) {
            return;
        }
        self.method_call_receiver_kinds.insert(key, kind);
    }

    /// Returns whether the qualified method name `Trait::method` is in the
    /// recognised consume-receiver set.
    pub(super) fn is_consume_receiver_method(&self, qualified_name: &str) -> bool {
        self.consume_receiver_methods.contains(qualified_name)
    }

    /// Returns true if any trait impl on `type_name` registered a method
    /// named `method` that is in the recognised consume-receiver set.
    ///
    /// Trait methods flatten into the inherent-method table on `T`, so the
    /// dispatch at the named-type
    /// site doesn't carry the originating trait. To honour
    /// `consumes_receiver` declared on the trait, we walk the
    /// `trait_impls_set` for matching `(type, trait)` pairs and check the
    /// qualified `Trait::method` form against the consume set.
    pub(super) fn named_type_method_consumes_receiver(
        &self,
        type_name: &str,
        method: &str,
    ) -> bool {
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

    /// Returns true when the dispatched call is a `#[resource]` type's inherent
    /// terminal `close(self)` — the implicit-drop dispatch target (W3.030) that
    /// also moves its receiver when called explicitly (#1295).
    ///
    /// The match is precise: the receiver type must carry the `#[resource]`
    /// marker, the method must be the discipline-mandated unit-returning
    /// `close`, and the receiver must be by-value `self` (a `var self` /
    /// mutable-receiver method takes the in-place-mutation path and is NOT an
    /// ownership-transfer move — R4). A `#[resource]` type's `close` is required
    /// to be `fn close(consume self)` by `check_resource_close_discipline`; this guard
    /// keeps the consume marking aligned with that contract.
    pub(super) fn named_type_inherent_close_consumes_receiver(
        &self,
        type_name: &str,
        builtin: Option<BuiltinType>,
        method: &str,
        sig: &FnSig,
    ) -> bool {
        if method != "close" || sig.requires_mutable_receiver {
            return false;
        }
        // Compiler carriers such as `MonitorRef` already carry their exact,
        // shadow-proof identity on the resolved `Ty`.  Use that discriminator
        // instead of asking the name-indexed source registry to rediscover a
        // prelude spelling: imported source declarations are registry-owned,
        // while compiler carriers are catalog-owned.
        if builtin.is_some_and(|kind| kind.close_method() == Some(method)) {
            return true;
        }
        // The trait registry is the single authority for source-declared
        // `#[resource]` facts, keyed by exact declaration identity.
        self.registry.is_resource(type_name)
    }

    pub(in crate::check) fn record_method_call_rewrite(
        &mut self,
        span: &Span,
        rewrite: MethodCallRewrite,
    ) {
        self.method_call_rewrites
            .insert(SpanKey::in_module(span, self.current_module_idx), rewrite);
    }

    /// Resolve the reply type used for the ask-reply `Send` gate to the
    /// module-qualified identity of the dispatched actor's defining module.
    ///
    /// The reply `Ty::Named` carries the bare type name (`Reply`) as written in
    /// the imported actor's `receive fn` return annotation. The trait registry
    /// keys marker derivation by name and the bare key is last-write-wins across
    /// modules: two imported packages each exporting `Reply` collide, so a Send
    /// lookup on the bare name can read the wrong module's fields and either
    /// over-accept a non-Send reply (it reaches codegen and trips the D10 gate)
    /// or over-reject a Send one. `method_id` is `{module}.{Actor}::{method}` for
    /// a module actor, so the reply type is defined in `{module}`; if a
    /// collision-free `{module}.{Name}` registry alias exists (seeded by
    /// `register_qualified_type_alias` → `alias_type_markers`), derive `Send`
    /// through that qualified identity. Root / flat-file actors retain their
    /// bare identity. A module actor whose lexical import binding or canonical
    /// marker row is absent returns `None`: the Send gate must reject rather
    /// than consulting a same-name bare marker row.
    pub(super) fn send_gate_reply_ty(&self, method_id: &str, resolved_reply: &Ty) -> Option<Ty> {
        let Ty::Named {
            name,
            args,
            builtin,
        } = resolved_reply
        else {
            return Some(resolved_reply.clone());
        };
        // Builtins carry their own marker authority. A qualified user name,
        // by contrast, must have an exact structural marker row.
        if builtin.is_some() {
            return Some(resolved_reply.clone());
        }
        if name.contains('.') {
            return self
                .registry
                .has_type_markers(name)
                .then(|| resolved_reply.clone());
        }
        let Some((actor_identity, _method)) = method_id.rsplit_once("::") else {
            return Some(resolved_reply.clone());
        };
        let Some((module_short, _actor)) = actor_identity.rsplit_once('.') else {
            return Some(resolved_reply.clone());
        };
        // `method_id` carries the imported actor's lexical module binding;
        // marker derivation keys on the source declaration's full owner. A
        // missing binding is not evidence for a bare reply type — fail closed
        // so a same-name sibling cannot lend it a Send marker.
        let module_owner = self.module_import_bindings.get(&(
            self.current_module.clone(),
            self.current_module_idx,
            module_short.to_string(),
        ))?;
        let qualified = format!("{module_owner}.{name}");
        if self.registry.has_type_markers(&qualified) {
            Some(Ty::Named {
                name: qualified,
                args: args.clone(),
                builtin: *builtin,
            })
        } else {
            None
        }
    }

    pub(super) fn record_actor_method_dispatch(
        &mut self,
        span: &Span,
        method_id: String,
        reply_ty: Ty,
    ) -> Ty {
        let resolved_reply = self.subst.resolve(&reply_ty);
        let dispatch = if self.receive_generator_methods.contains(&method_id) {
            // `receive_generator_methods` is checker authority for gen-ness —
            // HIR/MIR consume this discriminator directly rather than
            // re-deriving stream-producer-ness from `is_generator` or from
            // `reply_ty`'s shape (`type-info-survival`). `register_receive_fn`
            // (registration.rs) always wraps a generator method's `fn_sigs`
            // return type in `Ty::stream(declared_return_type)`, so every
            // `record_actor_method_dispatch` call site for a gen method passes
            // a `Stream<T>` here; unwrap to the element type `T`.
            let elem_ty = match reply_ty.as_stream() {
                Some(elem) => elem.clone(),
                None => unreachable!(
                    "receive_generator_methods `{method_id}` recorded with a non-Stream \
                     reply type `{reply_ty:?}` — registration.rs always wraps a generator \
                     method's fn_sigs return_type in Ty::stream(..)"
                ),
            };
            ActorMethodKind::StreamProducer(method_id, elem_ty)
        } else if matches!(resolved_reply, Ty::Unit) {
            ActorMethodKind::Message {
                method_id,
                policy: crate::actor_delivery::SendPolicy::Reject,
            }
        } else {
            // Ask-shaped: the reply value crosses the actor boundary back to the
            // caller, so `R` must be `Send` — the same obligation the lambda
            // actor reply gate enforces (`E_DUPLEX_NON_SEND`, see
            // `check_lambda_actor` in expressions.rs). Declared-actor asks
            // previously gated only the message arguments
            // (`enforce_actor_method_send_args`), so a non-Send reply type slipped
            // past the checker and surfaced only later at codegen, where the
            // #1739 reply-drop classifier fails closed on it with a far less
            // actionable diagnostic. Gating it here — at the single
            // dispatch-recording chokepoint shared by every declared-actor ask
            // site — turns it into a clean type error at the call.
            //
            // The guard requires a fully resolved, error-free type: an
            // inference-in-progress reply (`Var`) or a reply that already carries
            // an error must not mis-fire a spurious Send rejection (the
            // admissibility output-contract pruner applies the same
            // `!has_inference_var() && !contains_error()` discipline). Every
            // value that is constructible and returnable in safe Hew is Send
            // (handles are `Send + Copy`; `Stream`/`Sink`/`Duplex` are `Send`
            // iff their element is), so in practice this fires only on genuinely
            // non-transferable replies (`Rc`, and any record/tuple/enum that
            // transitively carries one).
            // Derive `Send` through the reply type's module-qualified identity
            // so two imported packages that both export a same-bare-named reply
            // (`badpkg.Reply` vs `goodpkg.Reply`) do not collide on the bare
            // registry key. The qualified form is used only for the marker
            // lookup and diagnostic text; the dispatch table keeps the original
            // bare `reply_ty` the rest of the pipeline expects.
            match self.send_gate_reply_ty(&method_id, &resolved_reply) {
                Some(send_check_ty)
                    if !send_check_ty.has_inference_var()
                        && !send_check_ty.contains_error()
                        && !self
                            .registry
                            .implements_marker(&send_check_ty, MarkerTrait::Send) =>
                {
                    self.report_error(
                        TypeErrorKind::InvalidSend,
                        span,
                        format!(
                            "ask-shaped actor reply type `{}` is not Send (E_DUPLEX_NON_SEND)",
                            resolved_reply.user_facing()
                        ),
                    );
                }
                Some(_) => {}
                None => self.report_error(
                    TypeErrorKind::InvalidSend,
                    span,
                    format!(
                        "ask-shaped actor reply type `{}` has no exact module-owned Send proof \
                         (E_DUPLEX_NON_SEND)",
                        resolved_reply.user_facing()
                    ),
                ),
            }
            ActorMethodKind::Ask {
                method_id,
                reply_ty: reply_ty.clone(),
                policy: crate::actor_delivery::SendPolicy::Wait,
            }
        };
        let call_ty = match &dispatch {
            ActorMethodKind::Ask {
                reply_ty: reply, ..
            } => Ty::result(reply.clone(), Ty::actor_error(Ty::never_type())),
            _ => reply_ty,
        };
        self.actor_method_dispatch
            .insert(SpanKey::in_module(span, self.current_module_idx), dispatch);
        call_ty
    }

    pub(in crate::check) fn canonical_handle_receiver_type_name(
        &self,
        receiver_ty: &Ty,
    ) -> Option<String> {
        let Ty::Named { name, .. } = receiver_ty else {
            return None;
        };
        self.module_registry.canonical_handle_type_identity(name)
    }

    pub(in crate::check) fn record_handle_method_call_receiver_kind_if_any(
        &mut self,
        receiver_ty: &Ty,
        span: &Span,
    ) {
        let Some(type_name) = self.canonical_handle_receiver_type_name(receiver_ty) else {
            return;
        };
        self.record_method_call_receiver_kind(
            span,
            MethodCallReceiverKind::HandleInstance { type_name },
        );
    }

    /// Return the runtime family for a canonical stdlib extern method whose
    /// ABI is compiler-lowered.
    ///
    /// The endpoint spelling is deliberately insufficient here: user impls
    /// may publish the same `#[extern_symbol]`.  This join requires the exact
    /// registered method identity, its canonical stdlib provenance, and the
    /// checked parameter/result shape before a runtime descriptor can cross
    /// the checker boundary.
    pub(super) fn canonical_std_io_runtime_method_family(
        &self,
        signature_key: &str,
        c_symbol: &str,
        sig: &FnSig,
    ) -> Option<crate::runtime_call::RuntimeCallFamily> {
        let declaration = crate::runtime_call::canonical_std_io_extern_signature(
            signature_key,
            c_symbol,
            &sig.params,
            &sig.return_type,
        )?;
        let canonical_stdlib = self.extern_method_origins
                .get(signature_key)
                .is_some_and(|(module, trusted)| {
                    *trusted && module.as_deref() == Some(declaration.module)
                })
            // Directly checking a shipped stdlib file has a root module id
            // unrelated to its dotted import owner. `canonical_std_root_sources`
            // is populated only for that exact root path (not for an ordinary
            // user program that merely imports the module), so it is a safe
            // provenance substitute for the registration origin here.
            || self.canonical_std_root_sources.contains(declaration.module);
        if !canonical_stdlib {
            return None;
        }
        // The exact trusted declaration selects the semantic operation.
        declaration.family
    }

    /// Record a rewrite for a **closed-set builtin** runtime-ABI method call.
    ///
    /// Every `c_symbol` reaching this helper is one the checker resolved from
    /// its own builtin tables — stdlib method resolution
    /// (`require_builtin_runtime_symbol` / `resolve_*_method`), the literal
    /// close-family handle releases, and handle-method auto-derivation. Open-set
    /// `#[extern_symbol]` FFI strings do NOT come here: they route through
    /// [`Self::record_extern_symbol_method_call_rewrite`], which records
    /// `descriptor: None`. Keeping the two producers split is the
    /// `checker-output-boundary` guarantee — a user FFI symbol that happens to
    /// collide with a catalog name must never be reclassified into a typed
    /// runtime descriptor.
    pub(super) fn record_runtime_method_family_rewrite(
        &mut self,
        span: &Span,
        family: crate::runtime_call::RuntimeCallFamily,
    ) {
        let c_symbol = family.c_symbol().to_string();
        // Shared argument effects also govern captured receivers. An updated
        // receiver retains its source binding; a consuming result does not.
        let consumes_receiver =
            family.semantic_contract().map_or_else(
                || crate::builtin_names::runtime_symbol_consumes_receiver(&c_symbol),
                |contract| {
                    contract.arguments.first().is_some_and(|argument| {
                        argument.effect == crate::RuntimeArgumentEffect::Move
                    }) && !matches!(
                        contract.result,
                        crate::RuntimeResultEffect::UpdatedReceiver(_)
                            | crate::RuntimeResultEffect::UpdatedReceiverAndValue(_)
                    )
                },
            );
        // Recover the typed family for this closed builtin symbol. Because the
        // helper only ever sees checker-emitted catalog symbols (the extern
        // split routes every open-set `#[extern_symbol]` string elsewhere), this
        // is a bijection-guarded catalog round-trip of the checker's OWN output
        // — not a reverse-parse of arbitrary input. `from_c_symbol` returns
        // `None` only for the few builtin symbols the substrate does not yet
        // enumerate (pre-staged families); those keep `descriptor: None` and
        // consumers fall back to `c_symbol`.
        let descriptor = crate::runtime_call::RuntimeCallDescriptor::new(family, None)
            .expect("substrate variant rejects elem; runtime symbols never carry elem here");
        self.record_method_call_rewrite(
            span,
            MethodCallRewrite::RewriteToFunction {
                target: CallTarget::Runtime(descriptor.family()),
                c_symbol,
                descriptor: Some(descriptor),
                extern_identity: None,
                consumes_receiver,
                requires_mutable_receiver: false,
                receiver_update: crate::ReceiverUpdate::Replace,
                returns_receiver_identity: false,
            },
        );
    }

    pub(super) fn record_runtime_method_call_rewrite(
        &mut self,
        span: &Span,
        c_symbol: impl Into<String>,
    ) {
        let c_symbol = c_symbol.into();
        let Some(family) = crate::runtime_call::RuntimeCallFamily::from_c_symbol(&c_symbol) else {
            // Some compiler-synthetic identity accessors are closed catalog
            // endpoints but do not need a `RuntimeCallFamily`: their lowering
            // is owned by the identity producer in MIR/codegen.  Preserve the
            // checker-selected catalog endpoint instead of degrading a valid
            // source method to an unsupported call just because that producer
            // is not represented in the runtime-family enum.
            if crate::stdlib_catalog_identity::compiler_synthetic_identity_endpoint(&c_symbol)
                .is_some()
            {
                self.record_method_call_rewrite(
                    span,
                    MethodCallRewrite::RewriteToFunction {
                        target: CallTarget::Builtin {
                            endpoint: c_symbol.clone(),
                        },
                        c_symbol,
                        descriptor: None,
                        extern_identity: None,
                        consumes_receiver: false,
                        requires_mutable_receiver: false,
                        receiver_update: crate::ReceiverUpdate::Replace,
                        returns_receiver_identity: false,
                    },
                );
                return;
            }
            self.record_method_call_rewrite(
                span,
                MethodCallRewrite::RewriteToFunction {
                    target: CallTarget::Unsupported {
                        reason: format!("unregistered runtime method `{c_symbol}`"),
                    },
                    c_symbol,
                    descriptor: None,
                    extern_identity: None,
                    consumes_receiver: false,
                    requires_mutable_receiver: false,
                    receiver_update: crate::ReceiverUpdate::Replace,
                    returns_receiver_identity: false,
                },
            );
            return;
        };
        self.record_runtime_method_family_rewrite(span, family);
    }

    /// Record a direct opaque-handle call through the exact source extern
    /// declaration that owns its ABI endpoint.
    ///
    /// Extracted registry metadata is intentionally only a signature surface:
    /// it may use the legacy `net.Listener` presentation spelling while the
    /// source declaration is owned by `std.net`.  A non-catalog endpoint must
    /// therefore not be fabricated as a runtime call merely because the
    /// registry found it.  This bridge admits it only when the canonical
    /// receiver owner and the source extern declaration agree exactly.
    pub(super) fn record_source_extern_handle_method_rewrite(
        &mut self,
        span: &Span,
        receiver_name: &str,
        c_symbol: String,
    ) -> bool {
        let Some((owner_module, _)) = receiver_name.rsplit_once('.') else {
            return false;
        };
        // rc1-F1 stage B: resolve through the extern table's declaration
        // index — the receiver's canonical owner must ITSELF declare the
        // symbol, and the published identity is THAT declaration (its own
        // key, its own provenance), never whichever declaration happened to
        // mint the symbol's ABI contract.
        let Some((declaration_key, declaration)) = self
            .extern_table
            .declaration_by_symbol_and_module(&c_symbol, owner_module)
        else {
            return false;
        };

        let extern_identity = ExternMethodCallIdentity {
            endpoint: declaration.symbol.clone(),
            signature_key: declaration_key.full_path().to_string(),
            declaring_module: declaration.declaring_module.clone(),
            trusted_compiled_stdlib: self.canonical_std_module_sources.contains(owner_module),
        };
        let consumes_receiver =
            crate::builtin_names::runtime_symbol_consumes_receiver(&extern_identity.endpoint);
        self.record_method_call_rewrite(
            span,
            MethodCallRewrite::RewriteToFunction {
                target: CallTarget::Extern {
                    declaration: declaration_key.clone(),
                    endpoint: extern_identity.endpoint.clone(),
                    trusted_compiled_stdlib: extern_identity.trusted_compiled_stdlib,
                },
                c_symbol,
                descriptor: None,
                extern_identity: Some(extern_identity),
                consumes_receiver,
                requires_mutable_receiver: false,
                receiver_update: crate::ReceiverUpdate::Replace,
                returns_receiver_identity: false,
            },
        );
        true
    }

    /// Record a rewrite for an **open-set** `#[extern_symbol]` FFI method call.
    ///
    /// Unlike [`Self::record_runtime_method_call_rewrite`], the typed
    /// `descriptor` is unconditionally `None`. An `#[extern_symbol]` method —
    /// stdlib `duration` / `instant` bindings as well as
    /// user-authored FFI on inherent impls — is open-set *by mechanism*: the
    /// checker has no first-class runtime-call-family knowledge for it. The
    /// family would only be recoverable by reverse-parsing the symbol string,
    /// which is exactly the `checker-output-boundary` violation this split
    /// closes. So even when the raw/expanded symbol collides with a catalog
    /// name (e.g. `hew_duration_hours` == `RuntimeCallFamily::DurationHours`,
    /// or a user binding that string-matches `hew_vec_push_layout`), no typed
    /// descriptor is produced.
    ///
    /// `consumes_receiver` IS still derived from the resolved symbol via the
    /// single consume authority. This is NOT string reclassification but a
    /// load-bearing ownership fact with no other source: stdlib declares
    /// `#[extern_symbol(hew_lambda_actor_release)]`, a genuine consuming handle
    /// release, and dropping its consume mark would let the handle's scope-exit
    /// drop fire on already-freed memory (double-free). The verdict stays
    /// fail-closed (LESSONS: drop-allowset-from-value-flow): any symbol the
    /// allow-set does not name is borrowing, so an FFI binding that merely
    /// collides with a non-release name at worst leaks — it never double-frees.
    pub(super) fn record_extern_symbol_method_call_rewrite(
        &mut self,
        span: &Span,
        c_symbol: String,
        signature_key: String,
        sig: &FnSig,
        receiver_ty: &Ty,
    ) {
        let consumes_receiver = crate::builtin_names::runtime_symbol_consumes_receiver(&c_symbol);
        let (declaring_module, trusted_compiled_stdlib) = self
            .extern_method_origins
            .get(&signature_key)
            .cloned()
            .unwrap_or((None, false));
        let extern_identity = ExternMethodCallIdentity {
            endpoint: c_symbol.clone(),
            signature_key,
            declaring_module,
            trusted_compiled_stdlib,
        };
        // The endpoint is an ABI spelling, not a declaration identity.  In
        // particular an imported receiver may be written through an alias at
        // the call site, while the source impl was registered under its full
        // owner path.  Carry the ID allocated at that registration boundary;
        // do not manufacture one from the call-site signature key.
        let target = crate::stdlib_catalog_identity::compiler_synthetic_identity_endpoint(
            &extern_identity.endpoint,
        )
        .map_or_else(
            || {
                self.impl_method_declaration_ids
                    .get(&extern_identity.signature_key)
                    .cloned()
                    .map_or_else(
                        || CallTarget::Unsupported {
                            reason: format!(
                                "extern-symbol method `{}` has no registered declaration identity",
                                extern_identity.signature_key
                            ),
                        },
                        |declaration| {
                            self.publish_extern_method_signature(
                                &declaration,
                                &extern_identity,
                                sig,
                                receiver_ty,
                                consumes_receiver,
                            );
                            CallTarget::Extern {
                                declaration,
                                endpoint: extern_identity.endpoint.clone(),
                                trusted_compiled_stdlib: extern_identity.trusted_compiled_stdlib,
                            }
                        },
                    )
            },
            |endpoint| CallTarget::Builtin {
                endpoint: endpoint.to_string(),
            },
        );
        self.record_method_call_rewrite(
            span,
            MethodCallRewrite::RewriteToFunction {
                target,
                c_symbol,
                descriptor: None,
                extern_identity: Some(extern_identity),
                consumes_receiver,
                requires_mutable_receiver: false,
                receiver_update: crate::ReceiverUpdate::Replace,
                returns_receiver_identity: false,
            },
        );
    }

    /// Publish the declared C-boundary signature of one `#[extern_symbol]`
    /// method so later stages call through the declaration rather than
    /// re-deriving a signature from the endpoint spelling.
    pub(super) fn publish_extern_method_signature(
        &mut self,
        declaration: &crate::DefId,
        identity: &ExternMethodCallIdentity,
        sig: &FnSig,
        receiver_ty: &Ty,
        consumes_receiver: bool,
    ) {
        // A receiver method's signature carries only its explicit parameters;
        // the C boundary takes the receiver first, exactly as the source
        // declaration spells it.
        let params = std::iter::once(receiver_ty.clone())
            .chain(sig.params.iter().cloned())
            .map(|ty| self.subst.resolve(&ty).materialize_literal_defaults())
            .collect::<Vec<_>>();
        let consumes = std::iter::once(consumes_receiver)
            .chain(
                sig.param_ownership
                    .iter()
                    .map(|ownership| *ownership == crate::env::ParameterOwnership::Consume),
            )
            .collect();
        let signature = crate::check::types::ExternMethodSignature {
            endpoint: identity.endpoint.clone(),
            params,
            consumes,
            result: self
                .subst
                .resolve(&sig.return_type)
                .materialize_literal_defaults(),
            declaring_module: identity.declaring_module.clone(),
        };
        self.extern_method_signatures
            .insert((declaration.clone(), identity.endpoint.clone()), signature);
    }

    pub(super) fn record_monomorphic_extern_symbol_rewrite_if_any(
        &mut self,
        sig: &FnSig,
        signature_key: &str,
        span: &Span,
        receiver_ty: &Ty,
    ) -> bool {
        let Some(spec) = &sig.extern_symbol else {
            return false;
        };
        if let Some(family) =
            self.canonical_std_io_runtime_method_family(signature_key, &spec.template.raw, sig)
        {
            self.record_runtime_method_family_rewrite(span, family);
            return true;
        }
        if !spec.template.is_monomorphic() {
            self.report_error(
                TypeErrorKind::InvalidOperation,
                span,
                format!(
                    "extern-symbol template `{}` is not monomorphic; this receiver dispatch \
                     path only supports monomorphic FFI symbols",
                    spec.template.raw
                ),
            );
            return false;
        }
        self.record_extern_symbol_method_call_rewrite(
            span,
            spec.template.raw.clone(),
            signature_key.to_string(),
            sig,
            receiver_ty,
        );
        true
    }

    pub(super) fn record_named_extern_symbol_rewrite_if_any(
        &mut self,
        receiver_type_name: &str,
        type_args: &[Ty],
        method: &str,
        sig: &FnSig,
        span: &Span,
        receiver_ty: &Ty,
    ) -> bool {
        let Some(spec) = &sig.extern_symbol else {
            return false;
        };
        let signature_key = format!("{receiver_type_name}::{method}");
        if let Some(family) =
            self.canonical_std_io_runtime_method_family(&signature_key, &spec.template.raw, sig)
        {
            self.record_runtime_method_family_rewrite(span, family);
            return true;
        }
        if spec.template.is_monomorphic() {
            self.record_extern_symbol_method_call_rewrite(
                span,
                spec.template.raw.clone(),
                signature_key,
                sig,
                receiver_ty,
            );
            return true;
        }
        if !matches!(receiver_type_name, "Option" | "Result") {
            self.report_error(
                TypeErrorKind::InvalidOperation,
                span,
                format!(
                    "extern-symbol template `{}` is not monomorphic; this receiver dispatch \
                     path only supports monomorphic FFI symbols",
                    spec.template.raw
                ),
            );
            return false;
        }
        let Some(type_arg) = type_args.first() else {
            self.report_error(
                TypeErrorKind::InvalidOperation,
                span,
                format!(
                    "extern-symbol template `{}` requires receiver type argument `T`, \
                     but `{receiver_type_name}` has no type argument",
                    spec.template.raw
                ),
            );
            return false;
        };
        let resolved_type_arg = self.subst.resolve(type_arg).materialize_literal_defaults();
        let expanded = match spec.template.expand(&resolved_type_arg, &self.type_defs) {
            Ok(symbol) => symbol,
            Err(crate::extern_symbol::TemplateExpansionError::UnsupportedCallingConvention {
                expected_symbol,
                convention,
            }) => {
                self.report_error(
                    TypeErrorKind::InvalidOperation,
                    span,
                    format!(
                        "cannot lower {receiver_type_name}::{method}: extern-symbol template \
                         `{}` expands to unsupported runtime calling convention {:?} \
                         (would require `{expected_symbol}`)",
                        spec.template.raw, convention
                    ),
                );
                return false;
            }
        };
        self.record_extern_symbol_method_call_rewrite(
            span,
            expanded,
            format!("{receiver_type_name}::{method}"),
            sig,
            receiver_ty,
        );
        true
    }

    pub(super) fn dispatch_monomorphic_extern_symbol_method(
        &mut self,
        receiver_type_name: &str,
        type_args: &[Ty],
        method: &str,
        args: &[CallArg],
        span: &Span,
        receiver_ty: &Ty,
    ) -> Option<Ty> {
        let sig = self.lookup_named_method_sig(receiver_type_name, type_args, method)?;
        sig.extern_symbol.as_ref()?;
        let method_key = format!("{receiver_type_name}::{method}");
        let assoc_bindings = self
            .fn_type_param_assoc_bindings
            .get(&method_key)
            .cloned()
            .unwrap_or_default();
        let applied_sig = self.apply_instantiated_call_signature_with_assoc(
            &sig,
            &assoc_bindings,
            None,
            args,
            span,
            SignatureArgApplication::PositionalOnly {
                arity_context: format!("method `{method}`"),
            },
            true,
            Some(GenericCallee::Method {
                type_name: receiver_type_name,
                method,
                owner_type_args: type_args,
            }),
        );
        self.record_monomorphic_extern_symbol_rewrite_if_any(&sig, &method_key, span, receiver_ty);
        Some(applied_sig.return_type)
    }

    pub(super) fn check_primitive_receiver_method_fallback(
        &mut self,
        receiver_ty: &Ty,
        receiver_label: &str,
        method: &str,
        args: &[CallArg],
        span: &Span,
    ) -> Ty {
        if let Some(ret_ty) =
            self.try_dispatch_primitive_trait_method(receiver_ty, method, args, span)
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
            format!("no method `{method}` on {receiver_label}"),
        );
        Ty::Error
    }

    pub(super) fn missing_builtin_contract_error(
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
                "internal compiler error: builtin {builtin}.{method} is missing {item} metadata"
            ),
        );
    }

    pub(super) fn require_builtin_runtime_symbol(
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

    pub(super) fn require_builtin_method_sig(
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

    pub(super) fn record_module_qualified_method_call_rewrite(
        &mut self,
        span: &Span,
        c_symbol: impl Into<String>,
        source_declaration: impl Into<String>,
    ) {
        let c_symbol = c_symbol.into();
        let source_declaration = source_declaration.into();
        let target = if let Some(family) =
            crate::runtime_call::RuntimeCallFamily::from_c_symbol(&c_symbol)
        {
            CallTarget::Runtime(family)
        } else {
            self.lookup_declaration(&source_declaration)
                .cloned()
                .map_or_else(
                    || CallTarget::Builtin {
                        endpoint: c_symbol.clone(),
                    },
                    |declaration| self.source_call_target(declaration),
                )
        };
        self.record_method_call_rewrite(
            span,
            MethodCallRewrite::RewriteModuleQualifiedToFunction { target, c_symbol },
        );
    }

    /// Resolve a module spelling in the current source file to the exact
    /// imported module path.  The spelling is an input-namespace key only;
    /// declaration IDs must use this source owner, never an alias or final
    /// path segment.
    pub(in crate::check) fn canonical_module_import_owner(&self, module_name: &str) -> String {
        self.module_import_bindings
            .get(&(
                self.current_module.clone(),
                self.current_module_idx,
                module_name.to_string(),
            ))
            .cloned()
            .unwrap_or_else(|| module_name.to_string())
    }

    /// Whether `module_name` is a lexical module binding in the current file.
    /// The process-wide module registry is deliberately not consulted.
    pub(in crate::check) fn module_binding_in_current_file(&self, module_name: &str) -> bool {
        self.module_import_bindings.contains_key(&(
            self.current_module.clone(),
            self.current_module_idx,
            module_name.to_string(),
        ))
    }

    /// Whether this module spelling resolves to a user-source declaration.
    /// `user_modules` is intentionally not consulted: it is a legacy lexical
    /// spelling set and therefore cannot distinguish two paths with the same
    /// final component.
    pub(in crate::check) fn module_binding_has_user_declaration(
        &self,
        module_name: &str,
        method: &str,
    ) -> bool {
        let owner = self.canonical_module_import_owner(module_name);
        let declaration = format!("{owner}.{method}");
        self.fn_def_spans
            .get(&declaration)
            .is_some_and(|(_, declaring_module)| {
                declaring_module.as_deref() == Some(owner.as_str())
            })
    }

    /// Reject an exact native-only function at the semantic call/reference
    /// boundary. Module spellings are lexical only: aliases resolve to their
    /// canonical imported owner before consulting the fully-qualified
    /// manifest policy, while user declarations with the same spelling remain
    /// valid. The shared lookup selects one member or module capability so the
    /// two tables cannot emit duplicate diagnostics for the same call.
    pub(in crate::check) fn reject_wasm_native_only_module_function(
        &mut self,
        module_name: &str,
        method: &str,
        span: &Span,
    ) {
        if !self.wasm_target {
            return;
        }
        if let Some(feature) = self.wasm_native_only_function_feature(module_name, method) {
            self.reject_wasm_feature(span, feature);
        }
    }

    /// Apply exact function policy after a named import has resolved its
    /// declaration owner.  Unlike a bare surface spelling this carries the
    /// source identity (`std.fs.read`) and cannot be captured by a user
    /// function or a same-leaf module.
    pub(in crate::check) fn reject_wasm_native_only_function_identity(
        &mut self,
        source_identity: &str,
        span: &Span,
    ) {
        if !self.wasm_target {
            return;
        }
        let Some((module, function)) = source_identity.rsplit_once('.') else {
            return;
        };
        if let Some(feature) = self.wasm_native_only_function_feature(module, function) {
            self.reject_wasm_feature(span, feature);
        }
    }

    pub(super) fn record_handle_method_call_rewrite_if_any(
        &mut self,
        receiver_ty: &Ty,
        method: &str,
        args: &[CallArg],
        span: &Span,
    ) {
        self.record_handle_method_call_receiver_kind_if_any(receiver_ty, span);
        let Ty::Named { name, .. } = receiver_ty else {
            return;
        };
        if self.record_declared_runtime_method(name, method, args, span) {
            return;
        }
        if let Some(c_symbol) = self.module_registry.resolve_handle_method(name, method) {
            // Only a genuine fieldless `#[opaque]` runtime handle — where the
            // receiver value IS the runtime pointer — may be rewritten to a
            // direct extern call that passes the receiver as the handle
            // argument. A fielded `#[resource]` wrapper (e.g.
            // `regex.Pattern { handle }`) registers its thin-forward methods for
            // imported-signature resolution only; rewriting it would pass the
            // whole struct by value to a pointer-typed extern
            // (`hew_regex_is_match(%Pattern, …)`), and for a handle-returning
            // method (`clone -> Pattern`) a bare rewrite cannot reconstruct the
            // wrapper from the extern's inner-handle return. Those dispatch
            // through their real impl body, which forwards `self.handle` and
            // rebuilds the wrapper. This is the `is_handle_type` gate the
            // wrapper-registration path documents but must actually enforce here.
            if self.receiver_is_opaque_handle(name)
                && !self
                    .module_registry
                    .handle_method_dispatches_through_impl(name, method)
            {
                if crate::runtime_call::RuntimeCallFamily::from_c_symbol(&c_symbol).is_some() {
                    self.record_runtime_method_call_rewrite(span, c_symbol);
                } else {
                    let _ = self.record_source_extern_handle_method_rewrite(span, name, c_symbol);
                }
            }
        }
    }

    /// Resolve callback endpoints while the argument still carries its concrete
    /// actor identity. Handler names are selected by the source declaration's
    /// runtime contract; this does not infer a protocol from runtime symbols.
    pub(super) fn resolved_runtime_actor_endpoints(
        &self,
        handler: &Spanned<Expr>,
        data_handler: &str,
        close_handler: &str,
    ) -> Result<crate::check::dispatch::ResolvedActorEndpoints, String> {
        use crate::check::dispatch::{ResolvedActorEndpoint, ResolvedActorEndpoints};
        let key = SpanKey::in_module(&handler.1, self.current_module_idx);
        let ty = self
            .expr_types
            .get(&key)
            .ok_or_else(|| "runtime handler argument has no checked type".to_string())?;
        let Ty::Named {
            name,
            builtin: Some(BuiltinType::ActorHandle),
            ..
        } = self.subst.resolve(ty)
        else {
            return Err("runtime handler requires a concrete actor handle".to_string());
        };
        let canonical = self.canonical_nominal_name(&name).unwrap_or(name.clone());
        let protocol = self
            .actor_protocol_descriptors
            .get(&canonical)
            .or_else(|| self.actor_protocol_descriptors.get(&name))
            .ok_or_else(|| format!("actor `{canonical}` has no receive protocol"))?;
        let actor = self
            .lookup_declaration(&canonical)
            .cloned()
            .ok_or_else(|| format!("actor `{canonical}` has no declaration identity"))?;
        let endpoint = |name: &str| -> Result<ResolvedActorEndpoint, String> {
            let receive = protocol
                .handlers
                .iter()
                .find(|handler| handler.name == name)
                .ok_or_else(|| format!("actor `{canonical}` has no `{name}` receive handler"))?;
            if receive.return_ty != ResolvedTy::Unit {
                return Err(format!(
                    "runtime delivery handler `{canonical}::{name}` must return unit"
                ));
            }
            let handler = self
                .lookup_declaration(&format!("{canonical}::{name}"))
                .cloned()
                .ok_or_else(|| {
                    format!("handler `{canonical}::{name}` has no declaration identity")
                })?;
            Ok(ResolvedActorEndpoint {
                handler,
                msg_id: receive.msg_id,
            })
        };
        Ok(ResolvedActorEndpoints {
            actor,
            data: endpoint(data_handler)?,
            close: endpoint(close_handler)?,
        })
    }

    pub(super) fn record_declared_runtime_method(
        &mut self,
        receiver_name: &str,
        method: &str,
        args: &[CallArg],
        span: &Span,
    ) -> bool {
        use crate::check::dispatch::ResolvedRuntimeResult;
        use crate::runtime_call::DeclaredRuntimeResult;
        let canonical = self
            .canonical_nominal_name(receiver_name)
            .unwrap_or_else(|| receiver_name.to_string());
        let key = format!("{canonical}::{method}");
        let Some(contract) = crate::runtime_call::declared_runtime_method(&key) else {
            return false;
        };
        if !self.canonical_std_module_sources.contains(contract.module)
            || self.user_modules.contains(contract.module)
            || self
                .module_registry
                .canonical_handle_type_identity(receiver_name)
                .as_deref()
                != Some(canonical.as_str())
        {
            return false;
        }
        let selected = (|| {
            let declaration = self
                .impl_method_declaration_ids
                .get(&key)
                .cloned()
                .ok_or_else(|| format!("runtime method `{key}` has no declaration identity"))?;
            let [handler] = args else {
                return Err(format!("runtime method `{key}` requires one actor handler"));
            };
            let endpoints = self.resolved_runtime_actor_endpoints(
                handler.expr(),
                contract.data_handler,
                contract.close_handler,
            )?;
            let result = match contract.result {
                DeclaredRuntimeResult::DiscardStatus => ResolvedRuntimeResult::DiscardStatus,
                DeclaredRuntimeResult::StatusResult {
                    error_type,
                    error_variant,
                } => {
                    let error_ty = Ty::Named {
                        name: error_type.to_string(),
                        args: Vec::new(),
                        builtin: None,
                    };
                    let error = self.resolve_variant_match(
                        &format!("{error_type}::{error_variant}"), &error_ty,
                    ).ok_or_else(|| format!("runtime error variant `{error_type}::{error_variant}` is not declared"))?;
                    if !self.lookup_type_def(&error.type_name).is_some_and(|ty| {
                        matches!(ty.variants.get(&error.variant_name), Some(VariantDef::Unit))
                    }) {
                        return Err("runtime status error must be a unit enum variant".to_string());
                    }
                    ResolvedRuntimeResult::StatusResult { error }
                }
            };
            Ok::<_, String>(CallTarget::DeclaredRuntime {
                declaration,
                family: contract.family,
                actor_endpoints: Some(endpoints),
                result,
            })
        })();
        let target = match selected {
            Ok(target) => target,
            Err(reason) => {
                self.errors.push(TypeError::new(
                    TypeErrorKind::InvalidOperation,
                    span.clone(),
                    reason.clone(),
                ));
                CallTarget::Unsupported { reason }
            }
        };
        // The source signature has been checked; this declaration contract
        // is the invocation authority, including for suspension effects.
        self.resolved_calls
            .remove(&SpanKey::in_module(span, self.current_module_idx));
        self.record_method_call_rewrite(
            span,
            MethodCallRewrite::RewriteToFunction {
                target,
                c_symbol: contract.family.c_symbol().to_string(),
                descriptor: Some(
                    crate::RuntimeCallDescriptor::new(contract.family, None)
                        .expect("declared runtime family has no element parameter"),
                ),
                extern_identity: None,
                consumes_receiver: contract.consumes_receiver,
                requires_mutable_receiver: false,
                receiver_update: crate::ReceiverUpdate::Replace,
                returns_receiver_identity: false,
            },
        );
        true
    }

    /// True when `name` (qualified `regex.PatternHandle` or bare `Listener`)
    /// resolves to a fieldless `#[opaque]` runtime handle: the receiver value is
    /// itself the runtime pointer, so a handle-method call is safe to rewrite to
    /// a direct extern that takes the receiver as the handle argument. False for
    /// a fielded `#[resource]` wrapper: its exact source declaration is not
    /// an opaque handle, so its methods dispatch through their real impl body.
    pub(in crate::check) fn receiver_is_opaque_handle(&self, name: &str) -> bool {
        self.module_registry.is_handle_type(name)
    }

    pub(in crate::check) fn record_module_qualified_stdlib_call_rewrite_if_any(
        &mut self,
        module_name: &str,
        method: &str,
        span: &Span,
    ) {
        let canonical_owner = self.canonical_module_import_owner(module_name);
        let source_declaration = format!("{canonical_owner}.{method}");
        if let Some(target) = self.intrinsic_runtime_target_for_signature(&source_declaration) {
            // The checker has already proved the exact canonical declaration
            // and catalog identity. Keep the user-facing callee spelling only
            // for HIR presentation; the typed target is the executable
            // authority and no lowering re-parses this string.
            self.record_method_call_rewrite(
                span,
                MethodCallRewrite::RewriteModuleQualifiedToFunction {
                    target: CallTarget::Runtime(target),
                    c_symbol: method.to_string(),
                },
            );
            return;
        }
        if self.module_binding_has_user_declaration(module_name, method) {
            return;
        }
        if let Some(c_symbol) = self
            .module_registry
            .resolve_module_call(&canonical_owner, method)
        {
            let symbol = if c_symbol == method {
                let source_qualified = format!("{canonical_owner}.{method}");
                let surface_qualified = format!("{module_name}.{method}");
                if !self.fn_sigs.contains_key(&source_qualified)
                    && !self.fn_sigs.contains_key(&surface_qualified)
                {
                    return;
                }
                // Linker presentation remains the checker-selected registry
                // spelling for now; the target below carries the canonical
                // source declaration identity.
                surface_qualified
            } else {
                c_symbol
            };
            self.record_module_qualified_method_call_rewrite(span, symbol, source_declaration);
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
    pub(in crate::check) fn record_module_qualified_user_call_rewrite_if_any(
        &mut self,
        module_name: &str,
        method: &str,
        span: &Span,
    ) {
        if !self.module_binding_has_user_declaration(module_name, method) {
            return;
        }
        let canonical_owner = self.canonical_module_import_owner(module_name);
        let source_declaration = format!("{canonical_owner}.{method}");
        // A canonical compiler-intrinsic declaration is source-backed but not
        // an ordinary user function. Its typed runtime or type-directed math
        // rewrite was selected by the stdlib path; never overwrite it with a
        // linker-name `User` fallback merely because it also has a source fn.
        if self
            .intrinsic_runtime_target_for_signature(&source_declaration)
            .is_some()
            || self
                .intrinsic_math_generic_op_for_signature(&source_declaration)
                .is_some()
        {
            return;
        }
        if self.fn_sigs.contains_key(&source_declaration)
            || self
                .fn_sigs
                .contains_key(&format!("{module_name}.{method}"))
        {
            self.record_module_qualified_method_call_rewrite(
                span,
                source_declaration.clone(),
                source_declaration,
            );
        }
    }

    pub(super) fn reject_if_wasm_native_only_handle(&mut self, receiver_ty: &Ty, span: &Span) {
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
            "std.net.http.Response"
                if self.canonical_std_module_sources.contains("std.net.http") =>
            {
                self.reject_wasm_feature(span, WasmUnsupportedFeature::HttpClient);
            }
            "smtp.Conn" | "std.net.smtp.Conn" => {
                self.reject_wasm_feature(span, WasmUnsupportedFeature::Smtp);
            }
            "websocket.Conn"
            | "websocket.Server"
            | "websocket.Message"
            | "std.net.websocket.Conn"
            | "std.net.websocket.Server"
            | "std.net.websocket.Message" => {
                self.reject_wasm_feature(span, WasmUnsupportedFeature::WebSocket);
            }
            "process.Child" | "std.process.Child" => {
                self.reject_wasm_feature(span, WasmUnsupportedFeature::ProcessExecution);
            }
            "http.Server" | "http.Request" | "std.net.http.Server" | "std.net.http.Request" => {
                self.reject_wasm_feature(span, WasmUnsupportedFeature::HttpServer);
            }
            STD_NET_LISTENER | STD_NET_CONNECTION => {
                self.reject_wasm_feature(span, WasmUnsupportedFeature::TcpNetworking);
            }
            "tls.TlsStream" | "std.net.tls.TlsStream" => {
                self.reject_wasm_feature(span, WasmUnsupportedFeature::Tls);
            }
            "quic.QUICEndpoint"
            | "quic.QUICConnection"
            | "quic.QUICStream"
            | "quic.QUICEvent"
            | "std.net.quic.QUICEndpoint"
            | "std.net.quic.QUICConnection"
            | "std.net.quic.QUICStream"
            | "std.net.quic.QUICEvent" => {
                self.reject_wasm_feature(span, WasmUnsupportedFeature::Quic);
            }
            _ => {}
        }
    }

    pub(super) fn reject_if_wasm_blocking_semaphore_method(
        &mut self,
        receiver_ty: &Ty,
        method: &str,
        span: &Span,
    ) {
        let Ty::Named { name, .. } = receiver_ty else {
            return;
        };
        if name != "std.semaphore.Semaphore" {
            return;
        }
        if matches!(method, "acquire" | "acquire_timeout") {
            self.reject_wasm_feature(span, WasmUnsupportedFeature::BlockingSemaphoreAcquire);
        }
    }

    pub(in crate::check) fn strip_module_prefix<'a>(&self, name: &'a str) -> Option<&'a str> {
        let dot = name.find('.')?;
        if self.module_binding_in_current_file(&name[..dot]) {
            Some(&name[dot + 1..])
        } else {
            None
        }
    }

    /// Look up a type definition, handling module-qualified names like `json.Value`.
    pub(in crate::check) fn lookup_type_def(&self, name: &str) -> Option<TypeDef> {
        let current_module_key = if name.contains('.') {
            None
        } else {
            self.current_module_identity()
                .map(|owner| format!("{owner}.{name}"))
        };
        self.type_defs
            .get(name)
            .or_else(|| {
                current_module_key
                    .as_ref()
                    .and_then(|key| self.type_defs.get(key))
            })
            .or_else(|| {
                self.strip_module_prefix(name)
                    .and_then(|u| self.type_defs.get(u))
            })
            .cloned()
    }

    /// Look up a type definition mutably, handling module-qualified names.
    pub(in crate::check) fn lookup_type_def_mut(&mut self, name: &str) -> Option<&mut TypeDef> {
        if self.type_defs.contains_key(name) {
            return self.type_defs.get_mut(name);
        }
        if !name.contains('.') {
            if let Some(owner) = self.current_module_identity() {
                let current_module_key = format!("{owner}.{name}");
                if self.type_defs.contains_key(&current_module_key) {
                    return self.type_defs.get_mut(&current_module_key);
                }
            }
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
    pub(in crate::check) fn resolve_module_type(
        &self,
        module_short: &str,
        type_name: &str,
    ) -> Option<TypeDef> {
        if !self.module_binding_in_current_file(module_short) {
            return None;
        }
        let resolved_module = self
            .module_import_bindings
            .get(&(
                self.current_module.clone(),
                self.current_module_idx,
                module_short.to_string(),
            ))
            .map(String::as_str)?;
        let exports = self.module_type_exports.get(resolved_module)?;
        if !exports.contains(type_name) {
            return None;
        }
        let qualified = format!("{resolved_module}.{type_name}");
        self.type_defs.get(&qualified).cloned()
    }

    /// Return the exact source owner's exported type set for a lexical module
    /// binding. Diagnostics use this helper as well as successful resolution so
    /// suggestions never accidentally consult a same-leaf surface key.
    pub(in crate::check) fn module_type_exports_for_binding(
        &self,
        module_short: &str,
    ) -> Option<&HashSet<String>> {
        if !self.module_binding_in_current_file(module_short) {
            return None;
        }
        let owner = self.module_import_bindings.get(&(
            self.current_module.clone(),
            self.current_module_idx,
            module_short.to_string(),
        ))?;
        self.module_type_exports.get(owner)
    }

    /// Canonicalize a supervisor child's user-spelled `actor_type` to the
    /// registered actor identity.
    ///
    /// A supervisor child records its actor type as the raw source string the
    /// user wrote (`child b: bank.Account` stores `bank.Account`). For a
    /// package-module child that spelling carries the user's import *alias*
    /// (`bank`), whereas the checker registers the actor under its exact source
    /// owner (`hew.bank.Account`, keyed off `current_module`). Left raw, the
    /// alias-prefixed string never matches the canonical `fn_sigs` /
    /// `actor_init_params` / `type_defs` keys, so `bank.Account`'s own
    /// actor-handle type finds no `receive fn` and every wall keyed on the
    /// actor identity silently skips.
    ///
    /// Resolve dotted module bindings and bare named/aliased import bindings
    /// through the same lexical facts ordinary type resolution consumes. A
    /// declaration authored in the current scope wins before an import, and a
    /// bare import resolves only when that exact binding published one source
    /// identity. There is deliberately no scan over globally loaded exports.
    pub(in crate::check) fn resolve_supervisor_child_type(&self, raw: &str) -> Option<String> {
        if let Some((module_short, type_name)) = raw.split_once('.') {
            return self
                .resolve_module_type(module_short, type_name)
                .map(|td| td.name);
        }

        if self.supervisor_children.contains_key(raw) {
            return Some(raw.to_string());
        }

        // A supervisor declared inside a non-root module shares that module's
        // nominal scope with its actors. Resolve only the exact owner-qualified
        // actor declaration; never search another loaded module by leaf name.
        // This rung precedes selected imports so a same-file actor retains
        // lexical authority over an imported binding with the same spelling.
        if let Some(owner) = self.current_module_identity() {
            let local_actor = format!("{owner}.{raw}");
            if self
                .type_defs
                .get(&local_actor)
                .is_some_and(|type_def| type_def.kind == TypeDefKind::Actor)
            {
                return Some(local_actor);
            }
        }

        if self.local_type_defs.contains(raw) || self.source_type_defs.contains(raw) {
            let local = self.declaration_identity(raw);
            if self.type_defs.contains_key(&local) {
                return Some(local);
            }
            if self.type_defs.contains_key(raw) {
                return Some(raw.to_string());
            }
            // A flattened file import is root-visible in the source sets but
            // its compatibility leaf key is retired after registration. Fall
            // through to the exact published bare binding below; a genuine
            // current-scope declaration returned from one of the two keys.
        }

        // Root actors and flattened file-import actors both publish an exact
        // root-surface key. This is not a leaf search: the key exists only
        // because that spelling was registered into the current root scope.
        if self.current_module_identity().is_none() && self.type_defs.contains_key(raw) {
            return Some(raw.to_string());
        }

        if let Some(identity) = self.published_bare_type_qualified(raw) {
            if let Some(owner) = self.unqualified_to_module.get(&(
                self.current_module.clone(),
                self.current_module_idx,
                raw.to_string(),
            )) {
                self.mark_module_owner_bindings_used(owner);
            }
            return Some(identity);
        }
        None
    }

    pub(in crate::check) fn canonical_supervisor_child_type(&self, raw: &str) -> String {
        self.resolve_supervisor_child_type(raw)
            .unwrap_or_else(|| raw.to_string())
    }

    /// Resolve a `(module, type, variant)` triple to its `VariantDef`, gated on
    /// the type being exported by the module.  Returns `None` if the module
    /// alias is unknown, the type is not exported, or the variant does not
    /// exist on the type.  The caller is responsible for emitting the
    /// fail-closed diagnostic in each failure case.
    pub(in crate::check) fn resolve_module_variant(
        &self,
        module_short: &str,
        type_name: &str,
        variant_name: &str,
    ) -> Option<(TypeDef, VariantDef)> {
        let td = self.resolve_module_type(module_short, type_name)?;
        let v = td.variants.get(variant_name).cloned()?;
        Some((td, v))
    }

    /// Full canonical owner path of the module whose declarations are being
    /// checked. Use this for declaration identity and layout-facing type
    /// lookup.
    pub(in crate::check) fn current_module_identity(&self) -> Option<&str> {
        self.current_module.as_deref()
    }

    /// The identity a declaration written in the scope currently being checked
    /// is published under: `{module}.{bare_name}` inside a module, the bare
    /// name at the root program.
    ///
    /// This is the one formula for a declaration's own name. Registration mints
    /// the `TypeDef` key and the declaration's `Ty::Named` with it, and every
    /// later pass that has to name that same declaration - a machine's
    /// transition bodies, for instance - must mint it the same way, because the
    /// bare spelling is only a transient row on the import path and is retired
    /// once the canonical owner is published (`retire_imported_type_keys`).
    pub(in crate::check) fn declaration_identity(&self, bare_name: &str) -> String {
        self.current_module_identity().map_or_else(
            || bare_name.to_string(),
            |module| format!("{module}.{bare_name}"),
        )
    }

    /// Resolve a bare actor reference to its registered checker identity.
    ///
    /// Resolution order (local-first, mirroring `per-module-type-identity`):
    /// 1. the current module's own actor (`{current_full_path}.{name}`)
    /// 2. a root/flat actor registered under the bare name
    /// 3. a named-import binding (`unqualified_to_module`)
    /// 4. the modules exporting an actor of that name: exactly one resolves
    ///    to it; two or more is `Ambiguous` (never silent first-wins).
    pub(in crate::check) fn resolve_bare_actor_identity(&self, name: &str) -> BareActorResolution {
        self.resolve_bare_declaration_identity(name, &[TypeDefKind::Actor])
    }

    /// Resolve a bare `spawn` target. A supervisor is spawned exactly as an
    /// actor is, so both declaration kinds answer to the same resolution.
    pub(in crate::check) fn resolve_bare_spawn_target_identity(
        &self,
        name: &str,
    ) -> BareActorResolution {
        self.resolve_bare_declaration_identity(name, &[TypeDefKind::Actor, TypeDefKind::Supervisor])
    }

    pub(super) fn resolve_bare_declaration_identity(
        &self,
        name: &str,
        kinds: &[TypeDefKind],
    ) -> BareActorResolution {
        let is_actor = |key: &str| {
            self.type_defs
                .get(key)
                .is_some_and(|td| kinds.contains(&td.kind))
        };
        if let Some(module) = self.current_module.as_deref() {
            let dotted = format!("{module}.{name}");
            if is_actor(&dotted) {
                return BareActorResolution::Resolved(dotted);
            }
        }
        if is_actor(name) {
            return BareActorResolution::Resolved(name.to_string());
        }
        if let Some(owners) = self.published_bare_type_owners.get(&(
            self.current_module.clone(),
            self.current_module_idx,
            name.to_string(),
        )) {
            let candidates: Vec<String> = owners
                .iter()
                .filter(|identity| is_actor(identity))
                .cloned()
                .collect();
            match candidates.as_slice() {
                [identity] => return BareActorResolution::Resolved(identity.clone()),
                [] => {}
                _ => {
                    let modules = candidates
                        .iter()
                        .filter_map(|identity| identity.rsplit_once('.'))
                        .map(|(module, _)| module.to_string())
                        .collect();
                    return BareActorResolution::Ambiguous(modules);
                }
            }
        }
        let mut candidates: Vec<&str> = self
            .module_type_exports
            .iter()
            .filter(|(module, exports)| {
                exports.contains(name) && is_actor(&format!("{module}.{name}"))
            })
            .map(|(module, _)| module.as_str())
            .collect();
        candidates.sort_unstable();
        match candidates.as_slice() {
            [] => BareActorResolution::Unknown,
            [module] => BareActorResolution::Resolved(format!("{module}.{name}")),
            _ => {
                BareActorResolution::Ambiguous(candidates.iter().map(ToString::to_string).collect())
            }
        }
    }

    /// Resolve a method signature against the *module-local* type definition.
    ///
    /// When the checker is inside module `m` and resolving `Type::method`, the
    /// authoritative definition is `m`'s own `Type` (registered under the
    /// qualified `{short}.{Type}` key), not the bare `Type` key which is
    /// last-write-wins across every module that declares a same-named type.
    /// Used by the impl-body return-type check so a method body in module `m`
    /// is validated against `m`'s type, not whichever module registered the
    /// bare key last. Returns `None` outside a module or when the qualified
    /// type def / method is absent (caller falls back to the bare lookup).
    pub(in crate::check) fn module_local_method_sig(
        &self,
        type_name: &str,
        method: &str,
    ) -> Option<FnSig> {
        let owner = self.current_module_identity()?;
        let qualified = format!("{owner}.{type_name}");
        let td = self.type_defs.get(&qualified)?;
        td.methods.get(method).cloned()
    }

    pub(in crate::check) fn lookup_named_method_sig(
        &self,
        type_name: &str,
        type_args: &[Ty],
        method: &str,
    ) -> Option<FnSig> {
        shared_lookup_named_method_sig(&self.type_defs, &self.fn_sigs, type_name, type_args, method)
            .or_else(|| {
                let target = self.alias_target_for_instance(type_name, type_args)?;
                crate::method_resolution::lookup_method_sig(
                    &self.type_defs,
                    &self.fn_sigs,
                    &target,
                    method,
                )
            })
            .or_else(|| {
                self.module_registry
                    .resolve_handle_method_sig(type_name, method)
                    .map(|(_c_symbol, params, return_type, canonical_owner)| {
                        // The registry's own projection sees only the loaded
                        // module and its imports, so a nominal that module
                        // neither declares nor imports (`stream.Sink` reached
                        // from `std.net.http`) comes back at the legacy short
                        // owner while source resolution mints the complete one.
                        // Re-resolve through the shared ladder so a method
                        // signature and the source around it name one owner
                        // (rc1-F1 stage D, registry producer).
                        FnSig {
                            params: params
                                .iter()
                                .map(|ty| {
                                    self.canonicalize_registry_signature(ty, &canonical_owner, &[])
                                })
                                .collect(),
                            return_type: self.canonicalize_registry_signature(
                                &return_type,
                                &canonical_owner,
                                &[],
                            ),
                            ..FnSig::default()
                        }
                    })
            })
    }

    /// Resolve a method on a builtin `Result`/`Option` receiver against the
    /// canonical stdlib method surface ONLY.
    ///
    /// Dispatch on a builtin `Result<T, E>` / `Option<T>` receiver (e.g. the
    /// `Result<T, AskError>` wrapper an actor ask produces) must never consult
    /// the user `type_defs`/`fn_sigs`: a user package may declare its own
    /// `type Result`/`type Option` whose methods land under the same bare
    /// `Result::<method>` keys and shadow the stdlib entries by registration
    /// order. Resolving here against the origin-based
    /// [`Checker::builtin_result_option_method_sigs`] snapshot guarantees the
    /// builtin surface (and its `extern_symbol` rewrite) is selected for every
    /// method, not just a fixed allowlist of names. A method absent from the
    /// snapshot returns `None`, so the caller falls through to the
    /// `no method on Result<...>`/`Option<...>` diagnostic.
    pub(in crate::check) fn lookup_builtin_result_option_method_sig(
        &self,
        builtin: BuiltinType,
        type_args: &[Ty],
        method: &str,
    ) -> Option<FnSig> {
        let (impl_params, sig) = self
            .builtin_result_option_method_sigs
            .get(&(builtin, method.to_string()))?;
        Some(instantiate_stdlib_method_sig(sig, impl_params, type_args))
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

    /// Try to resolve a method call on a named type via `type_defs` and `fn_sigs`.
    ///
    /// Used as a fallback from hardcoded handle-type dispatch tables so that
    /// methods added via `.hew` impl blocks work without updating the tables.
    pub(in crate::check) fn try_resolve_named_method(
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
        let canonical_name = self
            .canonical_nominal_name(name)
            .unwrap_or_else(|| name.clone());
        let sig = self.lookup_named_method_sig(&canonical_name, type_args, method)?;
        let return_type = self
            .apply_instantiated_call_signature(
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
                    type_name: &canonical_name,
                    method,
                    owner_type_args: type_args,
                }),
            )
            .return_type;
        // The successful signature lookup and the emitted impl body must use
        // the same declaration identity.  In particular, a fielded resource
        // wrapper such as `std.text.regex.Pattern` deliberately cannot be
        // rewritten to its raw handle extern: its source impl forwards
        // `self.handle` and reconstructs wrappers where needed.  Resolve the
        // canonical, source-qualified impl key here, at the lookup boundary,
        // rather than making HIR rediscover it from a presentation name.
        self.record_named_source_method_rewrite(receiver_ty, method, &sig, span);
        Some(self.qualify_method_return_to_receiver_owner(&canonical_name, &return_type))
    }

    /// The key a named type's source method is published under in
    /// `impl_method_declaration_ids`.  A concrete specialisation takes its own
    /// mangled receiver key; every other receiver keeps the plain one.
    pub(in crate::check) fn named_source_method_dispatch_key(
        &self,
        receiver_ty: &Ty,
        method: &str,
    ) -> Option<String> {
        let Ty::Named {
            name,
            args: type_args,
            ..
        } = receiver_ty
        else {
            return None;
        };
        let canonical_name = self
            .canonical_nominal_name(name)
            .unwrap_or_else(|| name.clone());
        let method_key = format!("{canonical_name}::{method}");
        if type_args.is_empty() {
            return Some(method_key);
        }
        Some(
            type_args
                .iter()
                .map(|ty| ResolvedTy::from_ty(&self.subst.resolve(ty)).ok())
                .collect::<Option<Vec<_>>>()
                .as_ref()
                .and_then(|args| crate::resolved_ty::mangle_impl_self_name(&canonical_name, args))
                .map(|owner| format!("{owner}::{method}"))
                .filter(|key| self.impl_method_declaration_ids.contains_key(key))
                .unwrap_or(method_key),
        )
    }

    /// The inherent `impl T { fn method(…) }` declaration identity, which is
    /// what a direct `T.method(…)` call targets.  A trait slot filled by
    /// structural satisfaction names this same declaration.
    pub(in crate::check) fn inherent_impl_method_declaration(
        &self,
        receiver_ty: &Ty,
        method: &str,
    ) -> Option<crate::DefId> {
        let key = self.named_source_method_dispatch_key(receiver_ty, method)?;
        self.impl_method_declaration_ids.get(&key).cloned()
    }

    /// Record a direct call to the exact source implementation that supplied
    /// a named-method signature.  This is intentionally keyed only by the
    /// checker-owned declaration map: same-leaf user types and registry
    /// aliases cannot mint an imported stdlib impl dispatch.
    pub(super) fn record_named_source_method_rewrite(
        &mut self,
        receiver_ty: &Ty,
        method: &str,
        sig: &FnSig,
        span: &Span,
    ) {
        let Ty::Named { name, builtin, .. } = receiver_ty else {
            return;
        };
        let canonical_name = self
            .canonical_nominal_name(name)
            .unwrap_or_else(|| name.clone());
        let Some(dispatch_key) = self.named_source_method_dispatch_key(receiver_ty, method) else {
            return;
        };
        let Some(declaration) = self.impl_method_declaration_ids.get(&dispatch_key).cloned() else {
            return;
        };
        let consumes_receiver = sig.consumes_receiver
            || self.named_type_method_consumes_receiver(&canonical_name, method)
            || self.named_type_inherent_close_consumes_receiver(
                &canonical_name,
                *builtin,
                method,
                sig,
            );
        if consumes_receiver {
            self.method_call_consumes_receiver
                .insert(SpanKey::in_module(span, self.current_module_idx));
        }
        self.record_method_call_rewrite(
            span,
            MethodCallRewrite::RewriteToFunction {
                target: CallTarget::impl_method(declaration),
                c_symbol: dispatch_key,
                descriptor: None,
                extern_identity: None,
                consumes_receiver,
                requires_mutable_receiver: sig.requires_mutable_receiver,
                receiver_update: sig.receiver_update,
                returns_receiver_identity: sig.returns_receiver_identity,
            },
        );
    }

    /// Restore the source owner on bare nominal types in a qualified receiver's
    /// method result.
    ///
    /// Impl signatures are registered while their declaring module is active,
    /// where a self-module type is legitimately spelled bare (`Listener::accept
    /// -> Connection`). At an imported call site the receiver has already
    /// acquired its exact identity (`net.Listener`), so letting that bare return
    /// escape would lose the owner again and make downstream layout/codegen
    /// confuse it with a root or foreign `Connection`.
    ///
    /// Only names proven in the same owner's `type_defs` are qualified. An
    /// already-qualified result (including `foo.Connection`) is authoritative
    /// and unchanged, as are builtins and a method on a bare/root receiver.
    pub(in crate::check) fn qualify_method_return_to_receiver_owner(
        &self,
        receiver_name: &str,
        ty: &Ty,
    ) -> Ty {
        let canonical_registry_receiver = self
            .module_registry
            .canonical_method_receiver_identity(receiver_name);
        let exact_receiver = if let Some(canonical) = canonical_registry_receiver.as_deref() {
            canonical
        } else if self.type_defs.contains_key(receiver_name) {
            receiver_name
        } else {
            return ty.clone();
        };
        let Some((owner, _)) = exact_receiver.rsplit_once('.') else {
            return ty.clone();
        };
        self.qualify_method_return_to_owner(owner, ty)
    }

    pub(super) fn qualify_method_return_to_owner(&self, owner: &str, ty: &Ty) -> Ty {
        let mapped =
            ty.map_children_pub(&|child| self.qualify_method_return_to_owner(owner, child));
        let Ty::Named {
            name,
            args,
            builtin: None,
        } = mapped
        else {
            return mapped;
        };
        if name.contains('.') {
            let name = self
                .module_registry
                .canonical_registry_signature_type_identity(&name, owner)
                .unwrap_or(name);
            return Ty::Named {
                name,
                args,
                builtin: None,
            };
        }
        let qualified = format!("{owner}.{name}");
        Ty::Named {
            name: if self.type_defs.contains_key(&qualified)
                || self.module_registry.is_method_receiver_type(&qualified)
            {
                qualified
            } else {
                name
            },
            args,
            builtin: None,
        }
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
    pub(super) fn enforce_actor_method_send_args(&mut self, args: &[CallArg]) {
        // Snapshot per-arg types from `expr_types` first; calling
        // `enforce_actor_boundary_send` mutates `self`, so we cannot hold a
        // borrow into `self.expr_types` across the call.
        let arg_types: Vec<Option<Ty>> = args
            .iter()
            .map(|arg| {
                let (_expr, sp) = arg.expr();
                self.expr_types
                    .get(&SpanKey::in_module(sp, self.current_module_idx))
                    .cloned()
            })
            .collect();
        for (arg, ty_opt) in args.iter().zip(arg_types) {
            let (expr, sp) = arg.expr();
            if let Some(ty) = ty_opt {
                self.enforce_actor_boundary_send(expr, sp, sp, &ty);
            }
        }
    }

    pub(super) fn call_arg_types(&self, args: &[CallArg]) -> Vec<Option<Ty>> {
        args.iter()
            .map(|arg| {
                let (_expr, sp) = arg.expr();
                self.expr_types
                    .get(&SpanKey::in_module(sp, self.current_module_idx))
                    .cloned()
            })
            .collect()
    }

    /// The codec direction of a `std.encoding.wire` facade declaration,
    /// selected by its intrinsic key.
    pub(in crate::check) fn wire_codec_intrinsic(
        &self,
        signature_key: &str,
    ) -> Option<WireCodecDirection> {
        Some(match self.intrinsic_key_for_signature(signature_key)? {
            "wire.encode" => WireCodecDirection::Encode,
            "wire.decode" => WireCodecDirection::Decode,
            "wire.to_json" => WireCodecDirection::ToJson,
            "wire.from_json" => WireCodecDirection::FromJson,
            "wire.to_yaml" => WireCodecDirection::ToYaml,
            "wire.from_yaml" => WireCodecDirection::FromYaml,
            _ => return None,
        })
    }

    pub(in crate::check) fn record_generic_wire_codec_rewrite(
        &mut self,
        signature_key: &str,
        params: &[Ty],
        return_type: &Ty,
        span: &Span,
    ) -> bool {
        let Some(direction) = self.wire_codec_intrinsic(signature_key) else {
            return false;
        };
        let value_source = if direction.is_serialize() {
            params.first().cloned()
        } else if direction == WireCodecDirection::Decode {
            Some(return_type.clone())
        } else {
            result_ok_payload(return_type)
        };
        let Some(value_source) = value_source.map(|ty| self.subst.resolve(&ty)) else {
            return true;
        };
        match ResolvedTy::from_ty(&value_source) {
            Ok(value_ty) => self.record_method_call_rewrite(
                span,
                MethodCallRewrite::GenericWireCodec {
                    direction,
                    value_ty,
                },
            ),
            // `wire.to_json([1, 2, 3])`: the element type settles only when
            // literal defaulting runs, after the body is checked.
            Err(_) => self.deferred_wire_codecs.push(DeferredWireCodec {
                key: SpanKey::in_module(span, self.current_module_idx),
                span: span.clone(),
                source_module: self.current_module.clone(),
                direction,
                value_ty: value_source,
            }),
        }
        true
    }

    /// Record each deferred facade call now that its value type has settled.
    /// A call whose type never settles is refused here: without a recorded
    /// rewrite the call has no codec to lower to.
    pub(in crate::check) fn drain_deferred_wire_codecs(&mut self) {
        for entry in std::mem::take(&mut self.deferred_wire_codecs) {
            let value_ty = self
                .subst
                .resolve(&entry.value_ty)
                .materialize_literal_defaults();
            if let Ok(value_ty) = ResolvedTy::from_ty(&value_ty) {
                self.method_call_rewrites.insert(
                    entry.key,
                    MethodCallRewrite::GenericWireCodec {
                        direction: entry.direction,
                        value_ty,
                    },
                );
                continue;
            }
            // An errored operand already carries its own diagnostic.
            if value_ty.contains_error() {
                continue;
            }
            self.errors.push(TypeError {
                severity: crate::error::Severity::Error,
                kind: TypeErrorKind::InferenceFailed,
                span: entry.span,
                message: "cannot infer the value type of this wire codec call".to_string(),
                notes: vec![],
                suggestions: vec![
                    "name the type, for example `wire.from_json<Config>(text)`".to_string()
                ],
                source_module: entry.source_module,
            });
        }
    }

    pub(super) fn report_nonserializable_remote_actor_msg(&mut self, ty: &Ty, span: &Span) {
        self.report_error(
            TypeErrorKind::BoundsNotSatisfied,
            span,
            format!(
                "remote actor message type `{}` must implement Serializable before it can \
                 cross a RemotePid boundary; only scalars, collections of serializable \
                 values and `#[wire]` types have a wire encoding",
                ty.user_facing()
            ),
        );
    }

    pub(super) fn enforce_remote_actor_msg_serializable(&mut self, ty: &Ty, span: &Span) -> bool {
        let resolved = self.subst.resolve(ty);
        if matches!(resolved, Ty::Var(_) | Ty::Error) {
            return true;
        }
        if self.satisfies_serializable(&resolved) {
            true
        } else {
            self.report_nonserializable_remote_actor_msg(&resolved, span);
            false
        }
    }
}
