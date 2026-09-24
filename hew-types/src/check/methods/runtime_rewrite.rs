//! Checker methods grouped by responsibility: runtime rewrite.
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
    /// Returns whether the qualified method name `Trait::method` is in the
    /// recognised consume-receiver set.
    pub(super) fn is_consume_receiver_method(&self, qualified_name: &str) -> bool {
        self.consume_receiver_methods.contains(qualified_name)
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
}
