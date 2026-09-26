//! Checker methods grouped by responsibility: clone rc.
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
        let receiver_ty = self.named_ty_for_key(name, type_args.to_vec());
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
        let Some(type_def) = self.type_def_at(name) else {
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
            Ty::Named { head, args, .. } => {
                let name = head.registry_key();
                let builtin = head.builtin();
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
                        type_name: name.to_string(),
                        marker: ResourceMarker::Resource,
                        member: member.to_string(),
                    });
                }
                if self.registry.is_linear(name) {
                    return Some(CloneCapabilityBlocker::Affine {
                        type_name: name.to_string(),
                        marker: ResourceMarker::Linear,
                        member: member.to_string(),
                    });
                }
                if self.canonical_owned_handle_type_name(name).is_some()
                    || self.user_opaque_type_names.contains(name)
                {
                    // The carrier's own capability is canonical. In particular,
                    // Receiver<T> is one non-cloneable endpoint regardless of T;
                    // walking T first made diagnostics and semantics payload-dependent.
                    return Some(CloneCapabilityBlocker::Opaque {
                        type_name: name.to_string(),
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
        if let Some(type_def) = self.type_def_at(name) {
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
        if let Some(type_def) = self.type_def_at(name) {
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
            Ty::Named { head, args, .. } => {
                let name = head.registry_key();
                let builtin = head.builtin();
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
                    || self.user_opaque_type_names.contains(name)
                {
                    return Some(name.to_string());
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
            head:
                head @ (crate::TypeHead::Nominal(_)
                | crate::TypeHead::Param(_)
                | crate::TypeHead::Unresolved(_)),
            args,
            ..
        } = ty
        else {
            return None;
        };
        let name = head.registry_key();
        if !args.is_empty() || !self.is_type_param_in_scope(name) {
            return None;
        }
        Some(self.type_param_has_marker_bound(name, MarkerTrait::Clone))
    }

    pub(in crate::check) fn check_rc_method(
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
        let record = |checker: &mut Self, op| {
            checker.record_rc_intrinsic(span, op, &inner_ty);
        };
        match method {
            // rc.clone() increments the reference count and returns a new Rc<T>
            "clone" => {
                self.check_arity(args, 0, "`Rc.clone`", span);
                record(self, RcIntrinsicOp::Clone);
                Ty::rc(inner_ty)
            }
            // rc.get() copies the inner value out of the Rc.
            // `LoadOp` performs a bitwise copy, which is only sound for `Copy`
            // types (no ownership to duplicate).  For non-Copy `T`, callers
            // share access via `rc.clone()` instead.
            "get" => {
                self.check_arity(args, 0, "`Rc.get`", span);
                if !self
                    .registry
                    .implements_marker(&inner_ty, MarkerTrait::Copy)
                {
                    self.report_error(
                        TypeErrorKind::BoundsNotSatisfied,
                        span,
                        format!(
                            "`Rc.get` requires `T: Copy`; `{}` is not `Copy` — \
                             use `rc.clone()` to share the reference instead",
                            inner_ty.user_facing()
                        ),
                    );
                    return Ty::Error;
                }
                record(self, RcIntrinsicOp::GetCopy);
                inner_ty
            }
            "set" => {
                self.check_arity(args, 1, "`Rc.set`", span);
                if let Some(arg) = args.first() {
                    let (expr, arg_span) = arg.expr();
                    let arg_ty = self.synthesize(expr, arg_span);
                    self.expect_type(&inner_ty, &arg_ty, arg_span);
                    self.reject_borrowed_parameter_consumption(expr, arg_span, "Rc.set");
                }
                record(self, RcIntrinsicOp::Set);
                Ty::Unit
            }
            "downgrade" => {
                self.check_arity(args, 0, "`Rc.downgrade`", span);
                record(self, RcIntrinsicOp::Downgrade);
                Ty::weak(inner_ty)
            }
            // rc.strong_count() returns the current reference count as i64
            "strong_count" => {
                self.check_arity(args, 0, "`Rc.strong_count`", span);
                record(self, RcIntrinsicOp::StrongCount);
                Ty::I64
            }
            "weak_count" => {
                self.check_arity(args, 0, "`Rc.weak_count`", span);
                record(self, RcIntrinsicOp::WeakCount);
                Ty::I64
            }
            "is_unique" => {
                self.check_arity(args, 0, "`Rc.is_unique`", span);
                record(self, RcIntrinsicOp::IsUnique);
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
                    format!("no method `{method}` on `Rc<{}>`", inner_ty.user_facing()),
                );
                Ty::Error
            }
        }
    }

    pub(in crate::check) fn check_weak_method(
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
            "clone" => {
                self.check_arity(args, 0, "`Weak.clone`", span);
                self.record_rc_intrinsic(span, RcIntrinsicOp::WeakClone, &inner_ty);
                Ty::weak(inner_ty)
            }
            "upgrade" => {
                self.check_arity(args, 0, "`Weak.upgrade`", span);
                self.record_rc_intrinsic(span, RcIntrinsicOp::WeakUpgrade, &inner_ty);
                Ty::option(Ty::rc(inner_ty))
            }
            _ => {
                for arg in args {
                    let (expr, arg_span) = arg.expr();
                    self.synthesize(expr, arg_span);
                }
                self.report_error(
                    TypeErrorKind::UndefinedMethod,
                    span,
                    format!("no method `{method}` on `Weak<{}>`", inner_ty.user_facing()),
                );
                Ty::Error
            }
        }
    }

    pub(super) fn record_rc_intrinsic(&mut self, span: &Span, op: RcIntrinsicOp, payload_ty: &Ty) {
        let resolved = self
            .subst
            .resolve(payload_ty)
            .materialize_literal_defaults();
        match ResolvedTy::from_ty(&resolved) {
            Ok(payload_ty) => self.record_method_call_rewrite(
                span,
                MethodCallRewrite::RcIntrinsic { op, payload_ty },
            ),
            Err(_) => self.report_error(
                TypeErrorKind::InvalidOperation,
                span,
                "Rc/Weak operations require a concrete payload type".to_string(),
            ),
        }
    }
}
