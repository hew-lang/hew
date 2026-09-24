//! Split from `methods.rs`: checker methods, part 2 of 5.
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
    /// Enforce the A640 remote serializability floor after method signature
    /// application has populated `expr_types` for every argument.
    pub(super) fn enforce_remote_actor_method_serializable_args(
        &mut self,
        args: &[CallArg],
    ) -> bool {
        let arg_types = self.call_arg_types(args);
        let mut all_serializable = true;
        for (arg, ty_opt) in args.iter().zip(arg_types) {
            let (_expr, sp) = arg.expr();
            if let Some(ty) = ty_opt {
                all_serializable &= self.enforce_remote_actor_msg_serializable(&ty, sp);
            }
        }
        all_serializable
    }

    pub(super) fn is_unresolved_pid_msg_projection(ty: &Ty) -> bool {
        matches!(
            ty,
            Ty::AssocType {
                trait_name,
                assoc_name,
                ..
            } if trait_name.as_ref() == "std.builtins.Pid" && assoc_name.as_ref() == "Msg"
        )
    }

    pub(super) fn report_pid_polymorphic_send_fail_closed(
        &mut self,
        type_param_name: &str,
        ty: &Ty,
        span: &Span,
    ) {
        self.report_error(
            TypeErrorKind::BoundsNotSatisfied,
            span,
            format!(
                "generic `Pid.send` on `{type_param_name}` is fail-closed: `{}` must be proven \
                 Serializable, but the current checker cannot express the required \
                 `P.Msg: Serializable` associated-type projection bound yet (TODO A640)",
                ty.user_facing()
            ),
        );
    }

    pub(super) fn enforce_pid_polymorphic_send_serializable_args(
        &mut self,
        args: &[CallArg],
        type_param_name: &str,
    ) -> bool {
        let arg_types = self.call_arg_types(args);
        let mut all_serializable = true;
        for (arg, ty_opt) in args.iter().zip(arg_types) {
            let (_expr, sp) = arg.expr();
            if let Some(ty) = ty_opt {
                let resolved = self.subst.resolve(&ty);
                if Self::is_unresolved_pid_msg_projection(&resolved) {
                    self.report_pid_polymorphic_send_fail_closed(type_param_name, &resolved, sp);
                    all_serializable = false;
                } else {
                    all_serializable &= self.enforce_remote_actor_msg_serializable(&resolved, sp);
                }
            }
        }
        all_serializable
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
                    return self.record_actor_method_dispatch(span, method_key, ty.clone());
                }
                self.record_method_call_receiver_kind(
                    span,
                    MethodCallReceiverKind::NamedTypeInstance {
                        type_name: name.clone(),
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
                name,
                args: type_args,
                builtin: None,
            } = receiver_ty
            {
                match self.record_clone_admissibility(name, type_args, span) {
                    RecordCloneAdmissibility::Admissible => {
                        let record_ty = receiver_ty.clone();
                        self.record_method_call_rewrite(
                            span,
                            MethodCallRewrite::RecordCloneInplace {
                                record_name: name.clone(),
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
                        if type_args.is_empty() && !self.user_clone_record_seeds.contains(name) {
                            self.user_clone_record_seeds.push(name.clone());
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
                                record_name: name.clone(),
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
            name,
            args: type_args,
            builtin: None,
        } = receiver_ty
        else {
            return None;
        };
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
            builtin: Some(crate::BuiltinType::ActorFn),
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
            collect_method_sigs_for_receiver(&self.type_defs, &self.fn_sigs, receiver_ty)
                .iter()
                .map(|(name, _)| name.as_str()),
        )
    }

    /// Decide whether a user-defined `Ty::Named` record type is admissible for
    /// `clone`. Returns one of the following outcomes:
    ///
    /// - `Admissible`: the record can be cloned end-to-end via the synthesised
    ///   `__hew_record_clone_inplace_<R>` thunk.
    /// - `OpaqueField { opaque_name }`: the record (or a transitively reachable
    ///   field) contains an opaque handle — fail closed with a named diagnostic.
    /// - `GenericRecord`: the record has un-substituted generic type parameters
    ///   — not yet supported; fail closed with an NYI diagnostic.
    /// - `EnumClone { enum_name }`: the receiver is a user enum — clone via the
    ///   enum twin `__hew_enum_clone_inplace_<E>` (tag-dispatched payload clone).
    /// - `NotARecord`: not a clone-eligible named type (actor, machine, etc.) —
    ///   fall through to `UndefinedMethod`.
    ///
    /// LESSONS: `checker-authority` (sole authority for clone admissibility),
    /// `unclonable-leaf-fails-closed-transitively`, `admit-only-what-you-lower`.
    pub(in crate::check) fn record_clone_admissibility(
        &self,
        name: &str,
        type_args: &[Ty],
        _span: &Span,
    ) -> RecordCloneAdmissibility {
        use TypeDefKind::{Enum, Record, Struct};
        let receiver_ty = Ty::Named {
            name: name.to_string(),
            args: type_args.to_vec(),
            builtin: None,
        };
        if self.registry.is_resource(name) {
            return RecordCloneAdmissibility::AffineValue {
                type_name: name.to_string(),
                marker: hew_parser::ast::ResourceMarker::Resource,
                member: "value".to_string(),
            };
        }
        if self.registry.is_linear(name) {
            return RecordCloneAdmissibility::AffineValue {
                type_name: name.to_string(),
                marker: hew_parser::ast::ResourceMarker::Linear,
                member: "value".to_string(),
            };
        }
        let Some(type_def) = self.type_defs.get(name) else {
            // A bare type parameter (`x: T`) has no `type_defs` entry. When it
            // carries a `Clone` bound in scope (`fn f<T: Clone>(x: T)`), admit
            // the clone and defer the concrete copy path to monomorphization
            // (`AbstractParamClone`). This is the abstract-`T: Clone` spine
            // (mirrors `type_param_has_marker_bound` as used by abstract-key
            // HashMap dispatch). Without the bound, fall through to `NotARecord`
            // → `UndefinedMethod` (fail closed — `admit-only-what-you-lower`).
            if self.is_type_param_in_scope(name)
                && self.type_param_has_marker_bound(name, MarkerTrait::Clone)
            {
                return RecordCloneAdmissibility::AbstractParamClone;
            }
            return RecordCloneAdmissibility::NotARecord;
        };
        if !type_def.type_params.is_empty() && type_args.iter().any(|arg| matches!(arg, Ty::Var(_)))
        {
            return RecordCloneAdmissibility::GenericRecord;
        }
        if let Some(blocker) = self.structural_clone_blocker(&receiver_ty) {
            return match blocker {
                CloneCapabilityBlocker::Affine {
                    type_name,
                    marker,
                    member,
                } => RecordCloneAdmissibility::AffineValue {
                    type_name,
                    marker,
                    member,
                },
                CloneCapabilityBlocker::Opaque { type_name, member } => {
                    RecordCloneAdmissibility::OpaqueField {
                        opaque_name: type_name,
                        member,
                    }
                }
                CloneCapabilityBlocker::Missing { member, member_ty } => {
                    RecordCloneAdmissibility::MissingClone { member, member_ty }
                }
            };
        }
        // An enum is clone-eligible via the enum twin of the record thunk. It is
        // checked BEFORE the Record/Struct gate because the two paths diverge:
        // an enum's owned leaves live in variant payloads, not declared fields.
        if matches!(type_def.kind, Enum) {
            return RecordCloneAdmissibility::EnumClone {
                enum_name: name.to_string(),
            };
        }
        // Only Record and Struct (value-type) kinds are clone-eligible.
        if !matches!(type_def.kind, Record | Struct) {
            return RecordCloneAdmissibility::NotARecord;
        }
        RecordCloneAdmissibility::Admissible
    }

    pub(super) fn report_affine_record_clone_error(
        &mut self,
        receiver_name: &str,
        affine_name: &str,
        marker: hew_parser::ast::ResourceMarker,
        member: &str,
        span: &Span,
    ) {
        let message =
            Self::affine_record_clone_error_message(receiver_name, affine_name, marker, member);
        self.report_error(TypeErrorKind::InvalidOperation, span, message);
    }

    pub(super) fn affine_record_clone_error_message(
        receiver_name: &str,
        affine_name: &str,
        marker: hew_parser::ast::ResourceMarker,
        member: &str,
    ) -> String {
        let (attribute, contract) = match marker {
            hew_parser::ast::ResourceMarker::Resource => (
                "#[resource]",
                "has an affine close contract and no semantic clone",
            ),
            hew_parser::ast::ResourceMarker::Linear => (
                "#[linear]",
                "must be consumed exactly once and has no semantic clone",
            ),
            hew_parser::ast::ResourceMarker::None => {
                unreachable!("affine clone blocker cannot carry ResourceMarker::None")
            }
        };
        let subject = if receiver_name == affine_name {
            format!("type `{receiver_name}` is `{attribute}`")
        } else {
            format!("type `{receiver_name}` contains `{attribute}` value `{affine_name}`")
        };
        format!(
            "{subject} and cannot be cloned: member `{member}` contains `{affine_name}`, which \
             {contract}"
        )
    }

    pub(super) fn clone_member_path(parent: &str, member: &str) -> String {
        if parent.is_empty() {
            member.to_string()
        } else {
            format!("{parent}.{member}")
        }
    }

    pub(super) fn structural_clone_blocker(&self, ty: &Ty) -> Option<CloneCapabilityBlocker> {
        self.structural_clone_blocker_inner(ty, "", &mut std::collections::HashSet::new())
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
            builtin: Some(builtin),
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
        reason = "the closed member walk keeps clone refusal paths aligned with every stored shape"
    )]
    pub(super) fn structural_clone_blocker_inner(
        &self,
        ty: &Ty,
        path: &str,
        visiting: &mut std::collections::HashSet<String>,
    ) -> Option<CloneCapabilityBlocker> {
        use hew_parser::ast::ResourceMarker;

        let resolved = self.subst.resolve(ty).materialize_literal_defaults();
        match &resolved {
            Ty::Tuple(items) => {
                for (index, item) in items.iter().enumerate() {
                    let member = Self::clone_member_path(path, &index.to_string());
                    if let Some(blocker) =
                        self.structural_clone_blocker_inner(item, &member, visiting)
                    {
                        return Some(blocker);
                    }
                }
            }
            Ty::Named {
                name,
                args,
                builtin,
            } => {
                if builtin.is_some_and(BuiltinType::is_affine_clone_terminal) {
                    return None;
                }
                let member = if path.is_empty() { "value" } else { path };
                // Both pipe halves are affine: no composite recipe duplicates
                // one as a member, and the payload type is irrelevant to that.
                if builtin.is_some_and(BuiltinType::is_pipe_half) {
                    return Some(CloneCapabilityBlocker::Missing {
                        member: member.to_string(),
                        member_ty: resolved.clone(),
                    });
                }
                // Type-parameter capability inside a generic template comes
                // from the parameter's declared BOUND, never from a concrete
                // type (there is none yet). `T: Clone` makes every `T`-shaped
                // member clonable in the template; an unbounded `T` is not.
                // The concrete capability is decided at instantiation, where
                // `enforce_type_param_bounds` rejects an argument that does not
                // satisfy the bound — so an affine resource can never reach a
                // `T: Clone` position. This is the single template-capability
                // authority for Clone. Equality demands use
                // `finalize_eq_requirements` to select the exact concrete Eq
                // implementation at each instantiation.
                if let Some(capability) = self.type_param_template_clone_capability(&resolved) {
                    return if capability {
                        None
                    } else {
                        Some(CloneCapabilityBlocker::Missing {
                            member: member.to_string(),
                            member_ty: resolved.clone(),
                        })
                    };
                }
                if self.registry.is_resource(name) {
                    return Some(CloneCapabilityBlocker::Affine {
                        type_name: name.clone(),
                        marker: ResourceMarker::Resource,
                        member: member.to_string(),
                    });
                }
                if self.registry.is_linear(name) {
                    return Some(CloneCapabilityBlocker::Affine {
                        type_name: name.clone(),
                        marker: ResourceMarker::Linear,
                        member: member.to_string(),
                    });
                }
                if self.canonical_owned_handle_type_name(name).is_some()
                    || self.user_opaque_type_names.contains(name.as_str())
                {
                    // The carrier's own capability is canonical. In particular,
                    // Receiver<T> is one non-cloneable endpoint regardless of T;
                    // walking T first made diagnostics and semantics payload-dependent.
                    return Some(CloneCapabilityBlocker::Opaque {
                        type_name: name.clone(),
                        member: member.to_string(),
                    });
                }
                if matches!(builtin, Some(BuiltinType::Option | BuiltinType::Result)) {
                    for (index, arg) in args.iter().enumerate() {
                        let label = if args.len() == 1 {
                            "Some"
                        } else if index == 0 {
                            "Ok"
                        } else {
                            "Err"
                        };
                        let member = Self::clone_member_path(path, label);
                        if let Some(blocker) =
                            self.structural_clone_blocker_inner(arg, &member, visiting)
                        {
                            return Some(blocker);
                        }
                    }
                    return None;
                }
                if builtin.is_some() {
                    for (index, arg) in args.iter().enumerate() {
                        let label = if args.len() == 1 {
                            "element".to_string()
                        } else {
                            index.to_string()
                        };
                        let member = Self::clone_member_path(path, &label);
                        if let Some(blocker) =
                            self.structural_clone_blocker_inner(arg, &member, visiting)
                        {
                            return Some(blocker);
                        }
                    }
                }
                if let Some(type_def) = self.lookup_type_def(name) {
                    let visit_key = type_def.name.clone();
                    if !visiting.insert(visit_key.clone()) {
                        return None;
                    }
                    let mut field_names: Vec<&String> = type_def.fields.keys().collect();
                    field_names.sort();
                    for field_name in field_names {
                        let field_ty = type_def
                            .fields
                            .get(field_name)
                            .expect("field name came from this type definition");
                        let field_ty = Self::instantiate_type_def_member(
                            field_ty,
                            &type_def.type_params,
                            args,
                        );
                        let member = Self::clone_member_path(path, field_name);
                        if let Some(blocker) =
                            self.structural_clone_blocker_inner(&field_ty, &member, visiting)
                        {
                            visiting.remove(&visit_key);
                            return Some(blocker);
                        }
                    }
                    for (index, field_ty) in self
                        .tuple_record_constructor_fields(name, &type_def)
                        .iter()
                        .enumerate()
                    {
                        let field_ty = Self::instantiate_type_def_member(
                            field_ty,
                            &type_def.type_params,
                            args,
                        );
                        let member = Self::clone_member_path(path, &index.to_string());
                        if let Some(blocker) =
                            self.structural_clone_blocker_inner(&field_ty, &member, visiting)
                        {
                            visiting.remove(&visit_key);
                            return Some(blocker);
                        }
                    }
                    let mut variant_names: Vec<&String> = type_def.variants.keys().collect();
                    variant_names.sort();
                    for variant_name in variant_names {
                        let variant = type_def
                            .variants
                            .get(variant_name)
                            .expect("variant name came from this type definition");
                        let blocker = match variant {
                            VariantDef::Unit => None,
                            VariantDef::Tuple(fields) => {
                                fields.iter().enumerate().find_map(|(index, field_ty)| {
                                    let field_ty = Self::instantiate_type_def_member(
                                        field_ty,
                                        &type_def.type_params,
                                        args,
                                    );
                                    let member = Self::clone_member_path(
                                        path,
                                        &format!("{variant_name}.{index}"),
                                    );
                                    self.structural_clone_blocker_inner(
                                        &field_ty, &member, visiting,
                                    )
                                })
                            }
                            VariantDef::Struct(fields) => {
                                fields.iter().find_map(|(field_name, field_ty)| {
                                    let field_ty = Self::instantiate_type_def_member(
                                        field_ty,
                                        &type_def.type_params,
                                        args,
                                    );
                                    let member = Self::clone_member_path(
                                        path,
                                        &format!("{variant_name}.{field_name}"),
                                    );
                                    self.structural_clone_blocker_inner(
                                        &field_ty, &member, visiting,
                                    )
                                })
                            }
                        };
                        if blocker.is_some() {
                            visiting.remove(&visit_key);
                            return blocker;
                        }
                    }
                    visiting.remove(&visit_key);
                    return None;
                }
            }
            Ty::Array(elem, _) => {
                let member = Self::clone_member_path(path, "element");
                if let Some(blocker) = self.structural_clone_blocker_inner(elem, &member, visiting)
                {
                    return Some(blocker);
                }
            }
            _ => {}
        }

        if self
            .registry
            .implements_marker(&resolved, MarkerTrait::Clone)
        {
            return None;
        }
        // The template-capability authority applies at EVERY position a type
        // parameter appears, not just at a bare `T`. Reaching here means the
        // structural walk above found no blocker, so every type-parameter
        // position inside `resolved` was already decided by its declared bound.
        // The marker registry cannot answer for a partially abstract type — it
        // has no impl for `Vec<T>` — so letting it veto here rejected
        // `fn dup<T: Clone>(v: Option<Vec<T>>)` even though every leaf was
        // clonable. An unbounded parameter still refuses, with a member path,
        // from the recursive call that examined it.
        let in_scope: Vec<String> = self.current_type_param_names().into_iter().collect();
        if Self::ty_mentions_type_params(&resolved, &in_scope) {
            return None;
        }
        Some(CloneCapabilityBlocker::Missing {
            member: if path.is_empty() {
                "value".to_string()
            } else {
                path.to_string()
            },
            member_ty: resolved,
        })
    }

    /// Transitive, substitution-aware walk of a (possibly generic) record's
    /// fields looking for an opaque handle leaf. `type_args` are the concrete
    /// arguments at the clone site; each field is instantiated with them before
    /// recursing, so a concrete instantiation like `Box<Handle>` resolves its
    /// `item: T` field to `item: Handle` and the opaque leaf is detected. A
    /// monomorphic record passes `type_args = []`, so the substitution is a
    /// no-op and behaviour is unchanged. Returns the first opaque field-type
    /// name found, or `None` if clean. Uses `canonical_owned_handle_type_name`
    /// as the single opaque-detection authority (mirrors `ty_contains_owned_handle`
    /// in `registration.rs`); the substitution mirrors
    /// the ordinary recursive member walk.
    pub(super) fn record_field_contains_opaque(
        &self,
        name: &str,
        type_args: &[Ty],
        visiting: &mut std::collections::HashSet<String>,
        skip_channel_handles: bool,
    ) -> Option<String> {
        if !visiting.insert(name.to_string()) {
            return None; // cycle protection
        }
        let mut found = None;
        if let Some(type_def) = self.type_defs.get(name) {
            for field_ty in type_def.fields.values() {
                let field_ty =
                    Self::instantiate_type_def_member(field_ty, &type_def.type_params, type_args);
                if let Some(opaque) =
                    self.ty_field_contains_opaque(&field_ty, visiting, skip_channel_handles)
                {
                    found = Some(opaque);
                    break;
                }
            }
        }
        visiting.remove(name);
        found
    }

    /// Transitive, substitution-aware walk of a (possibly generic) enum's
    /// variant payloads looking for an opaque-handle leaf — the enum twin of
    /// [`Self::record_field_contains_opaque`]. Each variant payload type
    /// (`Tuple` positional, `Struct` named) is instantiated with the concrete
    /// clone-site `type_args` before recursing, so `Maybe<Handle>` resolves its
    /// `Some(T)` payload to `Some(Handle)` and the opaque leaf is detected. A
    /// monomorphic enum passes `type_args = []` (a no-op substitution). Returns
    /// the first opaque payload-type name found, or `None` if clean. Shares the
    /// `ty_field_contains_opaque` leaf classifier with the record walk, so the
    /// two stay in lockstep.
    pub(super) fn enum_variant_contains_opaque(
        &self,
        name: &str,
        type_args: &[Ty],
        visiting: &mut std::collections::HashSet<String>,
        skip_channel_handles: bool,
    ) -> Option<String> {
        if !visiting.insert(name.to_string()) {
            return None; // cycle protection
        }
        let mut found = None;
        if let Some(type_def) = self.type_defs.get(name) {
            'variants: for variant in type_def.variants.values() {
                let payload_tys: Vec<Ty> = match variant {
                    VariantDef::Unit => Vec::new(),
                    VariantDef::Tuple(tys) => tys.clone(),
                    VariantDef::Struct(fields) => fields.iter().map(|(_, ty)| ty.clone()).collect(),
                };
                for payload_ty in &payload_tys {
                    let payload_ty = Self::instantiate_type_def_member(
                        payload_ty,
                        &type_def.type_params,
                        type_args,
                    );
                    if let Some(opaque) =
                        self.ty_field_contains_opaque(&payload_ty, visiting, skip_channel_handles)
                    {
                        found = Some(opaque);
                        break 'variants;
                    }
                }
            }
        }
        visiting.remove(name);
        found
    }

    /// Return the first opaque message-payload type, exempting compiler-built-in
    /// channel endpoints and actor references that local actor delivery
    /// transfers as handle values.
    pub(in crate::check) fn ty_message_payload_contains_opaque(
        &self,
        ty: &Ty,
        visiting: &mut std::collections::HashSet<String>,
    ) -> Option<String> {
        self.ty_field_contains_opaque(ty, visiting, true)
    }

    pub(super) fn ty_field_contains_opaque(
        &self,
        ty: &Ty,
        visiting: &mut std::collections::HashSet<String>,
        skip_channel_handles: bool,
    ) -> Option<String> {
        // Resolve inference vars so a field whose type is still a `Ty::Var`
        // bound in the substitution environment is walked at its concrete type
        // through each concrete member.
        let resolved = self.subst.resolve(ty);
        match &resolved {
            Ty::Named {
                name,
                args,
                builtin,
                ..
            } => {
                if skip_channel_handles
                    && matches!(
                        builtin,
                        Some(
                            crate::BuiltinType::Sink
                                | crate::BuiltinType::Stream
                                | crate::BuiltinType::ActorHandle
                                | crate::BuiltinType::RemotePid
                        )
                    )
                {
                    return None;
                }
                // Direct opaque handle (imported via module registry OR user-declared #[opaque])?
                if self.canonical_owned_handle_type_name(name).is_some()
                    || self.user_opaque_type_names.contains(name.as_str())
                {
                    return Some(name.clone());
                }
                // Recurse into type args (e.g. `Vec<Handle>`, `Option<Handle>`).
                for arg in args {
                    if let Some(n) =
                        self.ty_field_contains_opaque(arg, visiting, skip_channel_handles)
                    {
                        return Some(n);
                    }
                }
                // Recurse into the type def's fields, substituting the def's
                // params with the concrete `args` at this use site.
                if let Some(n) =
                    self.record_field_contains_opaque(name, args, visiting, skip_channel_handles)
                {
                    return Some(n);
                }
                // Recurse into enum variant payloads too — a nested enum with a
                // hard-coded opaque payload (`enum Inner { B(Handle) }`) is
                // reachable from neither the type-arg walk nor the record-field
                // walk, so without this an outer clone would admit an
                // unclonable leaf. (`record_field_contains_opaque` is a no-op
                // for an enum, and vice-versa, so both calls are kind-safe.)
                if let Some(n) =
                    self.enum_variant_contains_opaque(name, args, visiting, skip_channel_handles)
                {
                    return Some(n);
                }
                None
            }
            Ty::Tuple(items) => {
                for item in items {
                    if let Some(n) =
                        self.ty_field_contains_opaque(item, visiting, skip_channel_handles)
                    {
                        return Some(n);
                    }
                }
                None
            }
            _ => None,
        }
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
            Ty::Named { name, args, .. } => {
                if args.is_empty() {
                    TyPattern::Primitive(name)
                } else {
                    TyPattern::App {
                        ctor: name,
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

    pub(super) fn dispatch_pattern_to_ty(pattern: &TyPattern) -> Ty {
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
                other => Ty::Named {
                    builtin: None,
                    name: other.to_string(),
                    args: vec![],
                },
            },
            TyPattern::App { ctor, args } => Ty::Named {
                builtin: crate::lookup_builtin_type(ctor),
                name: ctor.clone(),
                args: args.iter().map(Self::dispatch_pattern_to_ty).collect(),
            },
            TyPattern::Tuple(items) => {
                Ty::Tuple(items.iter().map(Self::dispatch_pattern_to_ty).collect())
            }
            TyPattern::Var(name) => Ty::Named {
                builtin: None,
                name: name.clone(),
                args: vec![],
            },
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
                let ty = Self::dispatch_pattern_to_ty(ty);
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
                let witness_ty = Self::dispatch_pattern_to_ty(&witness);
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
            let ty = Self::dispatch_pattern_to_ty(ty);
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
                let witness_ty = Self::dispatch_pattern_to_ty(&witness);
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
                name,
                args,
                builtin: None,
            } if args.is_empty() && self.is_type_param_in_scope(&name) => Some(name),
            _ => None,
        }
    }

    /// The single template-capability authority for `clone`.
    ///
    /// Returns `None` when `ty` is not a bare in-scope type parameter (the
    /// caller then continues its structural walk). Returns `Some(true)` when
    /// the parameter's declared bounds grant `Clone`, `Some(false)` when they
    /// do not.
    ///
    /// Inside a generic template there is no concrete type to interrogate, so
    /// the bound *is* the capability. Instantiation then decides the concrete
    /// capability: `enforce_type_param_bounds` refuses a type argument that
    /// does not satisfy `T: Clone`, which is what keeps affine resources
    /// non-clonable through a generic seam.
    pub(in crate::check) fn type_param_template_clone_capability(&self, ty: &Ty) -> Option<bool> {
        let Ty::Named {
            name,
            args,
            builtin: None,
        } = ty
        else {
            return None;
        };
        if !args.is_empty() || !self.is_type_param_in_scope(name) {
            return None;
        }
        Some(self.type_param_has_marker_bound(name, MarkerTrait::Clone))
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
            Ty::Named {
                name,
                args,
                builtin,
            } => {
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
                    builtin: Some(BuiltinType::HashMap),
                    name: "HashMap".to_string(),
                    args: vec![cx.key.clone(), cx.val.clone()],
                },
                CollectionKind::HashSet => Ty::Named {
                    builtin: Some(BuiltinType::HashSet),
                    name: "HashSet".to_string(),
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
}
