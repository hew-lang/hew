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
    /// method dispatch. Called after [`Self::try_resolve_source_method`] has
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
    /// a named record or a string-keyed map, and TOML has no null, so neither
    /// `()` nor a non-optional `Option` value can appear anywhere in the
    /// reachable schema.
    fn toml_unrepresentable(&self, ty: &ResolvedTy) -> Option<String> {
        let ResolvedTy::Named { head, args, .. } = ty else {
            return Some("a TOML document is a table".to_string());
        };
        let facts = crate::TypeFactService::new(
            self.type_fact_context(),
            std::collections::BTreeMap::new(),
        );
        let mut visiting = std::collections::HashSet::new();
        if ty.is_builtin(BuiltinType::HashMap) {
            let [key, value] = args.as_slice() else {
                return Some("a TOML document is a table".to_string());
            };
            if !matches!(key, ResolvedTy::String) {
                return Some(
                    "a TOML document is a table, so a root `HashMap` must use `string` keys"
                        .to_string(),
                );
            }
            return self.toml_value_unrepresentable(&facts, value, &mut visiting);
        }
        let def = self.type_def_view().of(*head)?;
        let layout = ty
            .nominal_instance(&self.defs)
            .and_then(|instance| self.serial_layouts.get(&instance.nominal));
        if def.kind != TypeDefKind::Struct || layout.is_none_or(|layout| layout.positional) {
            return Some("a TOML document is a table".to_string());
        }
        self.toml_value_unrepresentable(&facts, ty, &mut visiting)
    }

    fn toml_value_unrepresentable(
        &self,
        facts: &crate::TypeFactService,
        ty: &ResolvedTy,
        visiting: &mut std::collections::HashSet<ResolvedTy>,
    ) -> Option<String> {
        match ty {
            ResolvedTy::Unit => Some("TOML has no null to represent `()`".to_string()),
            ResolvedTy::Tuple(elements) => elements
                .iter()
                .find_map(|elem| self.toml_value_unrepresentable(facts, elem, visiting)),
            ResolvedTy::Array(element, _) => {
                self.toml_value_unrepresentable(facts, element, visiting)
            }
            ResolvedTy::Named {
                head: crate::TypeHead::Builtin(builtin),
                args,
                ..
            } => match (builtin, args.as_slice()) {
                (BuiltinType::Option, _) => Some(
                    "TOML has no null; `Option` is only supported as a record field".to_string(),
                ),
                (BuiltinType::Vec | BuiltinType::HashSet, [elem]) => {
                    self.toml_value_unrepresentable(facts, elem, visiting)
                }
                (BuiltinType::HashMap, [key, value]) => self
                    .toml_value_unrepresentable(facts, key, visiting)
                    .or_else(|| self.toml_value_unrepresentable(facts, value, visiting)),
                (BuiltinType::Result, [ok, err]) => self
                    .toml_value_unrepresentable(facts, ok, visiting)
                    .or_else(|| self.toml_value_unrepresentable(facts, err, visiting)),
                _ => None,
            },
            ResolvedTy::Named { head, .. } => {
                if !visiting.insert(ty.clone()) {
                    return None;
                }
                let def = self.type_def_view().of(*head)?;
                let layout = ty
                    .nominal_instance(&self.defs)
                    .and_then(|instance| self.serial_layouts.get(&instance.nominal));
                if def.kind == TypeDefKind::Struct {
                    let Ok((_, fields)) = facts.record_fields(ty) else {
                        return None;
                    };
                    let layout = layout?;
                    if layout.positional {
                        return fields.iter().find_map(|(_, field_ty)| {
                            self.toml_value_unrepresentable(facts, field_ty, visiting)
                        });
                    }
                    for ((name, field_ty), member) in fields.iter().zip(&layout.members) {
                        if member.flags & hew_codec::Member::SKIP != 0 {
                            continue;
                        }
                        if let ResolvedTy::Named {
                            head: crate::TypeHead::Builtin(BuiltinType::Option),
                            args,
                            ..
                        } = field_ty
                        {
                            if layout.tagged && member.flags & hew_codec::Member::OMIT_NULL == 0 {
                                return Some(format!(
                                    "field `{name}` is a required `Option` and TOML has no null; \
                                     mark it `optional`"
                                ));
                            }
                            if let Some(inner) = args.first() {
                                if let Some(reason) =
                                    self.toml_value_unrepresentable(facts, inner, visiting)
                                {
                                    return Some(reason);
                                }
                            }
                        } else if let Some(reason) =
                            self.toml_value_unrepresentable(facts, field_ty, visiting)
                        {
                            return Some(reason);
                        }
                    }
                    None
                } else {
                    let Ok(members) = facts.declared_capability_members(ty) else {
                        return None;
                    };
                    members.iter().find_map(|payload| {
                        self.toml_value_unrepresentable(facts, payload, visiting)
                    })
                }
            }
            _ => None,
        }
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
                fixes: Vec::new(),
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
}
