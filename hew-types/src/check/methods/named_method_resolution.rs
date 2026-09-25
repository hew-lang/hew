//! Checker methods grouped by responsibility: named method resolution.
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
        let td = self.type_def_at(&qualified)?;
        td.methods.get(method).cloned()
    }

    pub(in crate::check) fn lookup_named_method_sig(
        &self,
        type_name: &str,
        type_args: &[Ty],
        method: &str,
    ) -> Option<FnSig> {
        shared_lookup_named_method_sig(
            &self.defs,
            &self.type_defs,
            &self.fn_sigs,
            type_name,
            type_args,
            method,
        )
        .or_else(|| {
            let target = self.alias_target_for_instance(type_name, type_args)?;
            crate::method_resolution::lookup_method_sig(
                &self.defs,
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
            head,
            args: type_args,
        } = receiver_ty
        else {
            return None;
        };
        let name = head.registry_key();
        let canonical_name = self
            .canonical_nominal_name(name)
            .unwrap_or_else(|| name.to_string());
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
            head,
            args: type_args,
        } = receiver_ty
        else {
            return None;
        };
        let name = head.registry_key();
        let canonical_name = self
            .canonical_nominal_name(name)
            .unwrap_or_else(|| name.to_string());
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
        let Ty::Named { head, .. } = receiver_ty else {
            return;
        };
        let name = head.registry_key();
        let builtin = head.builtin();
        let canonical_name = self
            .canonical_nominal_name(name)
            .unwrap_or_else(|| name.to_string());
        let Some(dispatch_key) = self.named_source_method_dispatch_key(receiver_ty, method) else {
            return;
        };
        let Some(declaration) = self.impl_method_declaration_ids.get(&dispatch_key).copied() else {
            return;
        };
        let consumes_receiver = sig.consumes_receiver
            || self.named_type_method_consumes_receiver(&canonical_name, method)
            || self.named_type_inherent_close_consumes_receiver(
                &canonical_name,
                builtin,
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
        } else if self.type_def_at(receiver_name).is_some() {
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
        // Only a spelling the registry mirror left unresolved is qualified;
        // a resolved head already names its declaration.
        let Ty::Named {
            head: crate::TypeHead::Unresolved(spelling),
            args,
        } = mapped
        else {
            return mapped;
        };
        let name = spelling.as_str();
        if name.contains('.') {
            let name = self
                .module_registry
                .canonical_registry_signature_type_identity(name, owner)
                .unwrap_or_else(|| name.to_string());
            return self.named_ty_for_key(&name, args);
        }
        let qualified = format!("{owner}.{name}");
        self.named_ty_for_key(
            &if self.type_def_at(&qualified).is_some()
                || self.module_registry.is_method_receiver_type(&qualified)
            {
                qualified
            } else {
                name.to_string()
            },
            args,
        )
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

    #[allow(
        clippy::too_many_lines,
        reason = "handles all named-type method dispatch; splitting would scatter related intercepts"
    )]
    pub(in crate::check) fn check_named_method_fallback(
        &mut self,
        receiver_ty: &Ty,
        method_name: &str,
        args: &[CallArg],
        span: &Span,
        type_display_name: &str,
    ) -> Ty {
        if let Some(ty) = self.try_resolve_named_method(receiver_ty, method_name, args, span) {
            if let Ty::Named { head, .. } = receiver_ty {
                let name = head.registry_key();
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
                    .type_def_at(name)
                    .is_some_and(|td| td.kind == TypeDefKind::Actor)
                    && self.actor_receive_methods.contains(&method_key);
                if is_actor_receive_dispatch {
                    self.record_method_call_receiver_kind(
                        span,
                        MethodCallReceiverKind::ActorInstance {
                            actor_name: name.to_string(),
                        },
                    );
                    self.enforce_actor_method_send_args(args);
                    return self.record_actor_method_dispatch(span, method_key, ty.clone());
                }
                self.record_method_call_receiver_kind(
                    span,
                    MethodCallReceiverKind::NamedTypeInstance {
                        type_name: name.to_string(),
                    },
                );
            }
            self.record_handle_method_call_rewrite_if_any(receiver_ty, method_name, args, span);
            return ty;
        }

        // Fn-typed field call: `w.cb(args)` where `cb` is a record field of
        // function type dispatches as a field-load + closure call, not a
        // method lookup. Pre-validated here (arity + per-arg types against
        // the field's signature) and recorded as a structured rewrite so HIR
        // never guesses (`checker-codegen-pattern-contract`). A field that
        // exists but is NOT fn-typed falls through to `UndefinedMethod` —
        // the gate keeps rejecting what it claims to.
        if let Some(ret_ty) = self.try_record_fn_field_call(receiver_ty, method_name, args, span) {
            return ret_ty;
        }

        // `clone` on a user-defined record type: intercept before `UndefinedMethod`
        // and record a `RecordCloneInplace` rewrite when the record is admissible
        // (no opaque fields, not a generic record, not an enum/actor/machine).
        // Fail closed with a named diagnostic for unclonable shapes (opaque fields,
        // generic params). LESSONS: `checker-authority`, `admit-only-what-you-lower`,
        // `unclonable-leaf-fails-closed-transitively`.
        if method_name == "clone" && args.is_empty() {
            if let Ty::Named {
                head:
                    head @ (crate::TypeHead::Nominal(_)
                    | crate::TypeHead::Param(_)
                    | crate::TypeHead::Unresolved(_)),
                args: type_args,
                ..
            } = receiver_ty
            {
                let name = head.registry_key();
                match self.record_clone_admissibility(name, type_args, span) {
                    RecordCloneAdmissibility::Admissible => {
                        let record_ty = receiver_ty.clone();
                        self.record_method_call_rewrite(
                            span,
                            MethodCallRewrite::RecordCloneInplace {
                                record_name: name.to_string(),
                            },
                        );
                        // Seed for codegen's `emit_state_clone_drop_synthesis`.
                        // Bare-seed MONOMORPHIC records only: a generic
                        // instantiation (`type_args` present) is keyed by its
                        // monomorphised layout (`Pair$$i64$i64`) in MIR and
                        // seeded from the `RecordCloneInplace` walk in codegen
                        // (`collect_record_clone_inplace_seeds`). The bare name
                        // names no monomorphic layout, so seeding it here would
                        // register a dead key. This mirrors the MIR keying
                        // (`monomorphic_user_record_key`, `args.is_empty()`).
                        if type_args.is_empty()
                            && !self.user_clone_record_seeds.iter().any(|seed| seed == name)
                        {
                            self.user_clone_record_seeds.push(name.to_string());
                        }
                        return record_ty;
                    }
                    RecordCloneAdmissibility::OpaqueField {
                        opaque_name,
                        member,
                    } => {
                        // Synthesize args (none here) for error-recovery symmetry.
                        self.report_error(
                            TypeErrorKind::UndefinedMethod,
                            span,
                            format!(
                                "type `{name}` cannot be cloned because member `{member}` contains \
                                 opaque value `{opaque_name}`"
                            ),
                        );
                        return Ty::Error;
                    }
                    RecordCloneAdmissibility::AffineValue {
                        type_name,
                        marker,
                        member,
                    } => {
                        let receiver_name = receiver_ty.user_facing().to_string();
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
                                "type `{}` cannot be cloned because member `{member}` of type `{}` \
                                 has no Clone capability",
                                receiver_ty.user_facing(),
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
                                "cloning generic record `{name}` is not yet supported; \
                                 only monomorphic (non-generic) records can be cloned"
                            ),
                        );
                        return Ty::Error;
                    }
                    RecordCloneAdmissibility::AbstractParamClone => {
                        // Bare type param `x: T` with `T: Clone`. Record the
                        // clone rewrite but DO NOT seed `user_clone_record_seeds`
                        // — `T` names no monomorphic record layout; codegen
                        // would synthesise a dead `__hew_record_clone_inplace_T`.
                        // The concrete copy path is selected per-mono in MIR
                        // (`subst_ty(T)` → value-class dispatch).
                        let param_ty = receiver_ty.clone();
                        self.record_method_call_rewrite(
                            span,
                            MethodCallRewrite::RecordCloneInplace {
                                record_name: name.to_string(),
                            },
                        );
                        return param_ty;
                    }
                    RecordCloneAdmissibility::EnumClone { enum_name } => {
                        // A user enum routes through the SAME rewrite + HIR node
                        // as a record clone; MIR demuxes record-vs-enum by the
                        // resolved monomorphised layout (`enum_clone_layout_key`)
                        // and emits `EnumCloneInplace`, lowered to
                        // `__hew_enum_clone_inplace_<E>`. No bare-name seed: the
                        // enum clone-site is seeded from the `EnumCloneInplace`
                        // walk in the MIR thunk registry (`collect_enum_clone_inplace_seeds`),
                        // keyed by the monomorphised layout, so a generic
                        // instantiation never registers a dead bare key.
                        let enum_ty = receiver_ty.clone();
                        self.record_method_call_rewrite(
                            span,
                            MethodCallRewrite::RecordCloneInplace {
                                record_name: enum_name,
                            },
                        );
                        return enum_ty;
                    }
                    RecordCloneAdmissibility::NotARecord => {
                        // Fall through to `UndefinedMethod` below for non-record
                        // Named types (actors, machines, etc.); enums are
                        // handled by the `EnumClone` arm above.
                    }
                }
            }
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

    /// Recognise `receiver.field(args)` where `field` resolves to a record
    /// field of function/closure type. Returns the call's type (the field
    /// signature's return type) after checking arity and arguments, or
    /// `None` when the receiver is not a record, the field does not exist,
    /// or the field is not function-typed (the caller's `UndefinedMethod`
    /// fall-through then applies).
    pub(super) fn try_record_fn_field_call(
        &mut self,
        receiver_ty: &Ty,
        method_name: &str,
        args: &[CallArg],
        span: &Span,
    ) -> Option<Ty> {
        let Ty::Named {
            head:
                head @ (crate::TypeHead::Nominal(_)
                | crate::TypeHead::Param(_)
                | crate::TypeHead::Unresolved(_)),
            args: type_args,
            ..
        } = receiver_ty
        else {
            return None;
        };
        let name = head.registry_key();
        let type_def = self.lookup_type_def(name)?;
        let field_ty = type_def.fields.get(method_name)?;
        let field_ty =
            Self::instantiate_type_def_member(field_ty, &type_def.type_params, type_args);
        let resolved_field = self.subst.resolve(&field_ty);
        // A record field holding a lambda-actor handle answers a call the same
        // way the handle does: `job.run(3)` is the completion call on the
        // stored handle, not an indirect function call.
        if let Ty::Named {
            args: ref type_args,
            head: crate::TypeHead::Builtin(crate::BuiltinType::ActorFn),
            ..
        } = resolved_field
        {
            if type_args.len() == 2 {
                let type_args = type_args.clone();
                let call =
                    self.check_lambda_actor_call(&resolved_field, &type_args, args, span, None);
                // The delivery receiver is the field read, so record the
                // field's exact type for the lowering that builds it.
                if let Ok(field_resolved) = crate::resolved_ty::ResolvedTy::from_ty(&resolved_field)
                {
                    self.record_method_call_rewrite(
                        span,
                        MethodCallRewrite::RecordFnFieldCall {
                            field_ty: field_resolved,
                        },
                    );
                }
                return Some(call);
            }
        }
        let (params, ret) = match &resolved_field {
            Ty::Function { params, ret, .. } | Ty::Closure { params, ret, .. } => {
                (params.clone(), (**ret).clone())
            }
            _ => return None,
        };
        self.record_direct_call_target(span, CallTarget::IndirectFunctionValue);
        if args.len() != params.len() {
            self.report_error(
                TypeErrorKind::ArityMismatch,
                span,
                format!(
                    "field `{method_name}` on `{name}` is `{}` and takes {} argument(s), \
                     but {} were supplied",
                    resolved_field.user_facing(),
                    params.len(),
                    args.len()
                ),
            );
            return Some(Ty::Error);
        }
        for (arg, param_ty) in args.iter().zip(params.iter()) {
            let (expr, sp) = arg.expr();
            self.check_against(expr, sp, param_ty);
        }
        if let Ok(field_resolved) = crate::resolved_ty::ResolvedTy::from_ty(&resolved_field) {
            self.record_method_call_rewrite(
                span,
                MethodCallRewrite::RecordFnFieldCall {
                    field_ty: field_resolved,
                },
            );
        }
        Some(ret)
    }

    /// Extract the signed value of a syntactic integer-literal expression,
    /// folding a single leading unary negation (`-2` parses as
    /// `Unary { Negate, Literal::Integer(2) }`).  Returns `None` for any
    /// non-literal expression — those are validated at runtime, never const.
    pub(super) fn literal_integer_value(expr: &Expr) -> Option<i128> {
        match expr {
            Expr::Literal(Literal::Integer { value, .. }) => Some(*value),
            Expr::Unary {
                op: UnaryOp::Negate,
                operand,
            } => match &operand.0 {
                Expr::Literal(Literal::Integer { value, .. }) => value.checked_neg(),
                _ => None,
            },
            _ => None,
        }
    }

    pub(super) fn similar_methods(&self, receiver_ty: &Ty, method_name: &str) -> Vec<String> {
        crate::error::find_similar(
            method_name,
            collect_method_sigs_for_receiver(
                &self.defs,
                &self.type_defs,
                &self.fn_sigs,
                receiver_ty,
            )
            .iter()
            .map(|(name, _)| name.as_str()),
        )
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
    /// Type-check a method call on `actor(M) -> R` — the lambda-actor handle.
    ///
    /// Wired methods (the actor surface, NOT the channel surface):
    ///   - `.send(msg: M)` → `Result<(), SendError>` (tell-shaped, R = ()) or
    ///     `Result<R, AskError>` (ask-shaped). Verifies `M: @send`. Secondary
    ///     surface to the canonical call-syntax `handle(msg)`.
    ///   - `.close()` → `()` — consuming; moves the handle. Deliberately returns
    ///     plain `()` rather than `Result<(), CloseError>` (unlike `Duplex::close`):
    ///     the lambda-actor release is unconditionally successful, and the
    ///     `CloseError` layout is not yet codegen-able.
    ///
    /// `.recv()` / `.try_recv()` / `.try_send()` / `.send_half()` / `.recv_half()`
    /// are NOT a lambda-actor surface: a lambda actor is not a channel. The caller
    /// never reads the actor's mailbox, and an actor handle cannot be split in two.
    /// These names are rejected with a targeted `UndefinedMethod` diagnostic.
    pub(in crate::check) fn check_lambda_pid_method(
        &mut self,
        type_args: &[Ty],
        receiver_ty: &Ty,
        receiver: &Spanned<Expr>,
        method: &str,
        args: &[CallArg],
        span: &Span,
    ) -> Ty {
        // Extract M and R from the `actor(M) -> R` handle; fabricate fresh vars if malformed.
        let (m_ty, _r_ty) = if let [m, r] = type_args {
            (m.clone(), r.clone())
        } else {
            for arg in args {
                let (expr, sp) = arg.expr();
                self.synthesize(expr, sp);
            }
            self.report_error(
                TypeErrorKind::InvalidOperation,
                span,
                "internal error: actor(M) -> R handle type has wrong arity".to_string(),
            );
            return Ty::Error;
        };

        match method {
            "send" => {
                if args.len() != 1 {
                    self.report_error(
                        TypeErrorKind::ArityMismatch,
                        span,
                        format!(
                            "`send` on an actor handle expects one argument (the message), but {} were supplied",
                            args.len()
                        ),
                    );
                }
                // Check the argument against M (the message type) when present so
                // the caller still gets the most specific message-type diagnostic
                // alongside any arity error.
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    let ty = self.check_against(expr, sp, &m_ty);
                    // Enforce Send bound: the message crosses the actor boundary.
                    let resolved = self.subst.resolve(&ty);
                    self.enforce_actor_boundary_send(expr, sp, span, &resolved);
                }
                // Synthesize extra args for recovery diagnostics, but do not accept
                // them: MIR only lowers the receiver plus the first message arg.
                for arg in args.iter().skip(1) {
                    let (expr, sp) = arg.expr();
                    self.synthesize(expr, sp);
                }
                // `.send(msg)` is the same completion call as `handle(msg)`,
                // so it publishes the same dispatch and yields the same
                // envelope rather than a second spelling with its own
                // delivery and error type.
                self.check_lambda_actor_call(receiver_ty, type_args, args, span, None)
            }
            "close" => {
                // No arguments expected. Synthesize any supplied args for
                // recovery diagnostics, but do not accept them: MIR lowers only
                // the receiver for the handle's close.
                if !args.is_empty() {
                    self.report_error(
                        TypeErrorKind::ArityMismatch,
                        span,
                        format!(
                            "`close` on an actor handle expects no arguments, but {} were supplied",
                            args.len()
                        ),
                    );
                }
                for arg in args {
                    let (expr, sp) = arg.expr();
                    self.synthesize(expr, sp);
                }
                // `.close()` is the same terminal release `close(handle)`
                // performs on any local actor handle.
                self.actor_delivery_calls.insert(
                    SpanKey::in_module(span, self.current_module_idx),
                    crate::actor_delivery::ActorDeliveryCall::Close,
                );
                self.record_submission_suspension(span, true);
                // Consuming: the actor(M) -> R handle binding is moved.
                self.method_call_consumes_receiver
                    .insert(SpanKey::in_module(span, self.current_module_idx));
                let resolved_recv = self.subst.resolve(receiver_ty);
                self.mark_expr_moved_if_non_copy(&receiver.0, &receiver.1, &resolved_recv);
                // Returns `()` — the lambda-actor release is unconditionally
                // successful (the runtime refcount decrement / stop signal never
                // fails for a well-formed handle). Using `Unit` rather than
                // `Result<(), CloseError>` avoids registering the `CloseError`
                // enum instantiation in HIR, which would require a codegen layout
                // for the `CloseError` payload that the pipeline does not yet have.
                // A raw `Duplex::close()` keeps `Result<(), CloseError>` because
                // its close CAN fail (I/O flush errors on streams and connections).
                Ty::Unit
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
                         a lambda actor is not a channel — supported methods: \
                         send / close (the canonical call surface is `handle(msg)`)",
                        receiver_ty.user_facing()
                    ),
                );
                Ty::Error
            }
        }
    }

    pub(super) fn ty_to_dispatch_pattern(&self, ty: &Ty) -> TyPattern {
        let resolved = self.subst.resolve(ty).materialize_literal_defaults();
        if let Some(name) = Self::dispatch_primitive_pattern_name(&resolved) {
            return TyPattern::Primitive(name.to_string());
        }
        match resolved {
            Ty::Tuple(items) => TyPattern::Tuple(
                items
                    .iter()
                    .map(|item| self.ty_to_dispatch_pattern(item))
                    .collect(),
            ),
            Ty::Named { head, args } => {
                let name = head.registry_key();
                if args.is_empty() {
                    TyPattern::Primitive(name.to_string())
                } else {
                    TyPattern::App {
                        ctor: name.to_string(),
                        args: args
                            .iter()
                            .map(|arg| self.ty_to_dispatch_pattern(arg))
                            .collect(),
                    }
                }
            }
            other => TyPattern::Primitive(other.user_facing().to_string()),
        }
    }

    pub(in crate::check) fn type_param_has_marker_bound(
        &self,
        param_name: &str,
        marker: MarkerTrait,
    ) -> bool {
        let marker_name = marker.to_string();
        for frame in self.current_type_param_bounds.iter().rev() {
            if let Some(bounds) = frame.bounds.get(param_name) {
                return bounds.iter().any(|bound| bound == &marker_name);
            }
        }
        if let Some(fn_name) = self.current_function.as_ref() {
            if let Some(sig) = self.fn_sigs.get(fn_name) {
                if sig.type_params.iter().any(|param| param == param_name) {
                    return sig
                        .type_param_bounds
                        .get(param_name)
                        .is_some_and(|bounds| bounds.iter().any(|bound| bound == &marker_name));
                }
            }
        }
        false
    }
}
