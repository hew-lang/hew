//! Checker methods grouped by responsibility: actor wire.
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

    /// The codec operation of a format module's `encode`/`decode`
    /// declaration, selected by its intrinsic.
    pub(in crate::check) fn codec_intrinsic(&self, signature_key: &str) -> Option<Codec> {
        crate::stdlib_authority::Intrinsic::from_key(
            self.intrinsic_key_for_signature(signature_key)?,
        )?
        .codec()
    }

    pub(in crate::check) fn record_generic_wire_codec_rewrite(
        &mut self,
        signature_key: &str,
        params: &[Ty],
        return_type: &Ty,
        span: &Span,
    ) -> bool {
        let Some(codec) = self.codec_intrinsic(signature_key) else {
            return false;
        };
        let value_source = if codec.is_serialize() {
            params.first().cloned()
        } else {
            result_ok_payload(return_type)
        };
        let Some(value_source) = value_source.map(|ty| self.subst.resolve(&ty)) else {
            return true;
        };
        match ResolvedTy::from_ty(&value_source) {
            Ok(value_ty) => self.record_codec_rewrite(span, codec, value_ty),
            // `json.encode([1, 2, 3])`: the element type settles only when
            // literal defaulting runs, after the body is checked.
            Err(_) => self.deferred_wire_codecs.push(DeferredWireCodec {
                key: SpanKey::in_module(span, self.current_module_idx),
                span: span.clone(),
                source_module: self.current_module.clone(),
                codec,
                value_ty: value_source,
            }),
        }
        true
    }

    /// Record a settled codec call, or refuse a value its format cannot
    /// represent.
    fn record_codec_rewrite(&mut self, span: &Span, codec: Codec, value_ty: ResolvedTy) {
        if codec.format == CodecFormat::Toml {
            if let Some(reason) = self.toml_unrepresentable(&value_ty) {
                self.report_error(
                    TypeErrorKind::BoundsNotSatisfied,
                    span,
                    format!(
                        "E_FORMAT_CANNOT_REPRESENT: TOML cannot represent `{}`: {reason}",
                        value_ty.user_facing()
                    ),
                );
                return;
            }
        }
        self.method_call_rewrites.insert(
            SpanKey::in_module(span, self.current_module_idx),
            MethodCallRewrite::Codec { codec, value_ty },
        );
    }

    /// Why TOML cannot carry `ty`: a document is a table, so the root must be
    /// a named record or a string-keyed map, and TOML has no null, so a
    /// `#[wire]` record cannot hold a required `Option` field.
    fn toml_unrepresentable(&self, ty: &ResolvedTy) -> Option<String> {
        let ResolvedTy::Named { head, .. } = ty else {
            return Some("a TOML document is a table".to_string());
        };
        if ty.is_builtin(BuiltinType::HashMap) {
            return None;
        }
        let def = self.type_def_view().of(head.clone())?;
        let layout = ty
            .nominal_instance(&self.defs)
            .and_then(|instance| self.serial_layouts.get(&instance.nominal));
        if def.kind != TypeDefKind::Struct || layout.is_none_or(|layout| layout.positional) {
            return Some("a TOML document is a table".to_string());
        }
        let layout = layout?;
        if !layout.tagged {
            return None;
        }
        def.field_order
            .iter()
            .zip(&layout.members)
            .find(|(name, member)| {
                def.fields
                    .get(*name)
                    .is_some_and(|ty| ty.as_option().is_some())
                    && member.flags & hew_codec::Member::OMIT_NULL == 0
            })
            .map(|(name, _)| {
                format!(
                    "field `{name}` is a required `Option` and TOML has no null; mark it \
                     `optional`"
                )
            })
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
                let saved_idx =
                    std::mem::replace(&mut self.current_module_idx, entry.key.module_idx);
                let saved = std::mem::replace(&mut self.current_module, entry.source_module);
                self.record_codec_rewrite(&entry.span, entry.codec, value_ty);
                self.current_module = saved;
                self.current_module_idx = saved_idx;
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
                message: "E_TYPE_ANNOTATION_NEEDED: cannot infer the value type of this codec call"
                    .to_string(),
                notes: vec![],
                suggestions: vec![
                    "name the type, for example `json.decode<Config>(text)`".to_string()
                ],
                source_module: entry.source_module,
            });
        }
    }

    /// A remote message must be data whose records and enums are `#[wire]`,
    /// the same rule `RemotePid` sends check (D524).
    pub(super) fn enforce_remote_actor_msg_serializable(&mut self, ty: &Ty, span: &Span) -> bool {
        let resolved = self.subst.resolve(ty);
        if matches!(resolved, Ty::Var(_) | Ty::Error) {
            return true;
        }
        let resolved = self.normalize_for_use(&resolved.materialize_literal_defaults());
        let Ok(concrete) = ResolvedTy::from_ty(&resolved) else {
            return true;
        };
        let Some(error) = self.remote_payload_error(&concrete) else {
            return true;
        };
        let owner = format!("remote actor message `{}`", resolved.user_facing());
        self.report_error(
            TypeErrorKind::BoundsNotSatisfied,
            span,
            Self::not_data_message(&owner, &error),
        );
        false
    }

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
}
