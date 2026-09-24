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
