//! Shared builtin and named method resolution helpers.
#![expect(
    clippy::implicit_hasher,
    reason = "helpers mirror the concrete HashMap types stored in TypeCheckOutput and Checker"
)]

use std::collections::{HashMap, HashSet};

use crate::builtin_names::{builtin_named_kind, builtin_type_def as builtin_named_type_def};
use crate::check::dispatch_table::MethodSelection;
#[cfg(test)]
use crate::check::TypeDefKind;
use crate::check::{FnSig, FnSigView, ReceiverObligation, TypeDef};
use crate::resolved_ty::ResolvedTy;
use crate::BuiltinType;
use crate::DeclarationKind;
use crate::Ty;
use crate::{DefTable, NominalId, TypeHead};
use hew_parser::ast::Symbol;

pub(crate) fn instantiate_named_method_sig(
    mut sig: FnSig,
    type_params: &[crate::ParamHead],
    type_args: &[Ty],
) -> FnSig {
    let subst_map: HashMap<crate::ParamHead, Ty> = if let Some(method) = &sig.impl_method {
        let Some(substitution) = impl_receiver_substitution(
            &method.receiver_parameters,
            &method.receiver_args,
            type_args,
        ) else {
            sig.return_type = Ty::Error;
            return sig;
        };
        substitution
    } else {
        type_params
            .iter()
            .copied()
            .zip(type_args.iter().cloned())
            .collect()
    };
    apply_method_receiver_substitution(sig, &subst_map)
}

pub(crate) fn apply_method_receiver_substitution(
    mut sig: FnSig,
    subst_map: &HashMap<crate::ParamHead, Ty>,
) -> FnSig {
    for provider in sig.param_defaults.iter_mut().flatten() {
        for argument in &mut provider.type_args {
            *argument = argument.substitute_type_params_parallel(subst_map);
        }
    }
    for param_ty in &mut sig.params {
        *param_ty = param_ty.map(|ty| ty.substitute_type_params_parallel(subst_map));
    }
    sig.return_type = sig.return_type.substitute_type_params_parallel(subst_map);

    let substituted_params: HashSet<_> = subst_map.keys().copied().collect();
    sig.type_params
        .retain(|type_param| !substituted_params.contains(type_param));
    let remaining: HashSet<crate::TypeParamId> = sig
        .type_params
        .iter()
        .map(|parameter| parameter.id)
        .collect();
    sig.bounds = sig
        .bounds
        .map_types(|ty| ty.substitute_type_params_parallel(subst_map));
    for (parameter, bound) in sig.bounds.iter() {
        if let Some((param, arg)) = subst_map.iter().find(|(param, _)| param.id == *parameter) {
            sig.receiver_obligations.push(ReceiverObligation {
                param: *param,
                arg: arg.clone(),
                bound: bound.clone(),
            });
        }
    }
    sig.bounds.retain(|param, _| remaining.contains(&param));
    sig
}

pub(crate) fn impl_receiver_substitution(
    parameters: &[crate::ParamHead],
    receiver_args: &[Ty],
    type_args: &[Ty],
) -> Option<HashMap<crate::ParamHead, Ty>> {
    if receiver_args.len() != type_args.len() {
        return None;
    }
    let variables: HashMap<_, _> = parameters
        .iter()
        .copied()
        .map(|parameter| (parameter, Ty::Var(crate::ty::TypeVar::fresh())))
        .collect();
    let mut inference = crate::ty::Substitution::new();
    for (pattern, argument) in receiver_args.iter().zip(type_args) {
        let pattern = pattern.substitute_type_params_parallel(&variables);
        if crate::unify::unify_exact(&mut inference, &pattern, argument).is_err() {
            return None;
        }
    }
    Some(
        variables
            .into_iter()
            .map(|(parameter, variable)| (parameter, inference.resolve(&variable)))
            .collect(),
    )
}

fn merge_builtin_type_def(mut type_def: TypeDef, builtin: TypeDef) -> TypeDef {
    if type_def.type_params.is_empty() {
        type_def.type_params = builtin.type_params;
    }
    for (method_name, sig) in builtin.methods {
        type_def.methods.insert(method_name, sig);
    }
    type_def
}

fn receiver_head_and_args(ty: &Ty) -> Option<(TypeHead, &[Ty])> {
    if let Ty::Named { head, args } = ty {
        return Some((*head, args));
    }
    let instance = ResolvedTy::from_ty(ty).ok()?.impl_receiver_instance()?;
    Some((instance.head, &[]))
}

const MEMBER_METHOD_KINDS: [DeclarationKind; 3] = [
    DeclarationKind::ActorReceive,
    DeclarationKind::ActorMethod,
    DeclarationKind::TraitMethod,
];

fn lookup_collection_clone_method_sig(receiver_ty: &Ty, method: &str) -> Option<FnSig> {
    if method != "clone" {
        return None;
    }
    match receiver_ty {
        Ty::Named {
            head: crate::TypeHead::Builtin(BuiltinType::Vec),
            args,
            ..
        } if args.len() == 1 => Some(FnSig {
            return_type: receiver_ty.clone(),
            ..FnSig::default()
        }),
        Ty::Named {
            head: crate::TypeHead::Builtin(BuiltinType::HashMap),
            args,
            ..
        } if args.len() == 2 => Some(FnSig {
            return_type: receiver_ty.clone(),
            ..FnSig::default()
        }),
        Ty::Named {
            head: crate::TypeHead::Builtin(BuiltinType::HashSet),
            args,
            ..
        } if args.len() == 1 => Some(FnSig {
            return_type: receiver_ty.clone(),
            ..FnSig::default()
        }),
        _ => None,
    }
}

/// Look up a source method on the checked receiver.
#[must_use]
pub fn lookup_named_method_sig(
    defs: &DefTable,
    type_defs: &HashMap<NominalId, TypeDef>,
    fn_sigs: FnSigView<'_>,
    head: TypeHead,
    type_args: &[Ty],
    method: &str,
) -> Option<FnSig> {
    let user = crate::check::TypeDefView::new(defs, type_defs).of(head);
    if let Some(td) = user {
        if let Some(sig) = td.methods.get(method) {
            return Some(instantiate_named_method_sig(
                sig.clone(),
                &td.type_params,
                type_args,
            ));
        }
    }

    let name = Symbol::intern(method);
    let receiver = Ty::named_head(head, type_args.to_vec()).materialize_literal_defaults();
    let instance = ResolvedTy::from_ty(&receiver)
        .ok()
        .and_then(|ty| ty.impl_receiver_instance());
    let declaration = match fn_sigs.dispatch.select(head, instance.as_ref(), name) {
        MethodSelection::Unique(_, method) => method,
        MethodSelection::Ambiguous(_) => return None,
        MethodSelection::Missing => {
            let owner = head.declaration(defs)?.declaration();
            MEMBER_METHOD_KINDS
                .into_iter()
                .find_map(|kind| defs.member_of_kind(owner, name, kind))?
        }
    };
    let sig = fn_sigs.of(declaration)?;
    let type_params = user.map_or(&sig.type_params, |td| &td.type_params);
    Some(instantiate_named_method_sig(
        sig.clone(),
        type_params,
        type_args,
    ))
}

/// Instantiate a snapshotted stdlib method signature against the receiver's
/// type arguments.
///
/// `sig` is the canonical signature captured from the compiled-in
/// `std/result.hew` / `std/option.hew` impl block (see
/// `Checker::builtin_result_option_method_sigs`). `type_params` are the impl's
/// type parameters (`["T", "E"]` for `Result`, `["T"]` for `Option`) and
/// `type_args` are the receiver's concrete arguments. Substitution mirrors
/// [`lookup_named_method_sig`] so a builtin receiver dispatch produces the same
/// instantiated signature it would have before a user `type Result`/`type
/// Option` collision could shadow the stdlib entry in `fn_sigs`.
#[must_use]
pub fn instantiate_stdlib_method_sig(
    sig: &FnSig,
    type_params: &[crate::ParamHead],
    type_args: &[Ty],
) -> FnSig {
    instantiate_named_method_sig(sig.clone(), type_params, type_args)
}

/// Look up a builtin method signature for `Sender`, `Receiver`, `Stream`, or `Sink`.
#[must_use]
pub fn lookup_builtin_method_sig(receiver_ty: &Ty, method: &str) -> Option<FnSig> {
    let Ty::Named {
        head,
        args: type_args,
    } = receiver_ty
    else {
        return None;
    };
    let builtin = builtin_named_type_def(builtin_named_kind(head.builtin()?)?);
    let sig = builtin.methods.get(method).cloned()?;
    Some(instantiate_named_method_sig(
        sig,
        &builtin.type_params,
        type_args,
    ))
}

/// Look up a method signature for any named receiver, including builtin methods.
#[must_use]
pub fn lookup_method_sig(
    defs: &DefTable,
    type_defs: &HashMap<NominalId, TypeDef>,
    fn_sigs: FnSigView<'_>,
    receiver_ty: &Ty,
    method: &str,
) -> Option<FnSig> {
    let (head, type_args) = receiver_head_and_args(receiver_ty)?;
    lookup_builtin_method_sig(receiver_ty, method)
        .or_else(|| lookup_collection_clone_method_sig(receiver_ty, method))
        .or_else(|| lookup_named_method_sig(defs, type_defs, fn_sigs, head, type_args, method))
}

/// Look up a type definition, augmenting builtin placeholders with builtin methods.
#[must_use]
pub fn lookup_type_def(
    defs: &DefTable,
    type_defs: &HashMap<NominalId, TypeDef>,
    head: TypeHead,
) -> Option<TypeDef> {
    type_def_with_builtin(
        crate::check::TypeDefView::new(defs, type_defs).of(head),
        head.builtin(),
    )
}

fn type_def_with_builtin(user: Option<&TypeDef>, builtin: Option<BuiltinType>) -> Option<TypeDef> {
    let builtin_type = builtin
        .and_then(builtin_named_kind)
        .map(builtin_named_type_def);
    match (user, builtin_type) {
        (Some(type_def), Some(builtin)) => {
            Some(merge_builtin_type_def(type_def.clone(), builtin.clone()))
        }
        (Some(type_def), None) | (None, Some(type_def)) => Some(type_def.clone()),
        (None, None) => None,
    }
}

/// Look up the type definition for a named receiver.
///
/// Returns `None` for an actor handle: actor-handle types do not expose the
/// actor's internal fields to callers. Method completions on handles come
/// from `collect_method_sigs_for_receiver` instead.
#[must_use]
pub fn lookup_type_def_for_receiver(
    defs: &DefTable,
    type_defs: &HashMap<NominalId, TypeDef>,
    receiver_ty: &Ty,
) -> Option<TypeDef> {
    // Actor handles have no public fields accessible via the handle.
    if let Ty::Named {
        head: crate::TypeHead::Actor(_),
        ..
    } = receiver_ty
    {
        return None;
    }
    lookup_type_def(defs, type_defs, receiver_ty.head()?)
}

/// Collect method signatures visible on the receiver, including intrinsic
/// methods and the actor's receive handlers.
#[must_use]
pub fn collect_method_sigs_for_receiver(
    defs: &DefTable,
    type_defs: &HashMap<NominalId, TypeDef>,
    fn_sigs: FnSigView<'_>,
    receiver_ty: &Ty,
) -> Vec<(String, FnSig)> {
    let Some((head, _)) = receiver_head_and_args(receiver_ty) else {
        return Vec::new();
    };
    let mut names = HashSet::new();
    if let Some(definition) = lookup_type_def(defs, type_defs, head) {
        names.extend(definition.methods.keys().map(|name| Symbol::intern(name)));
    }
    if lookup_collection_clone_method_sig(receiver_ty, "clone").is_some() {
        names.insert(Symbol::intern("clone"));
    }
    names.extend(fn_sigs.dispatch.methods_of(head).map(|(name, _, _)| name));
    if let Some(owner) = head.declaration(defs) {
        for kind in MEMBER_METHOD_KINDS {
            names.extend(
                defs.members_of_kind(owner.declaration(), kind)
                    .map(|id| defs.name(id)),
            );
        }
    }

    names
        .into_iter()
        .filter_map(|name| {
            let sig = lookup_method_sig(defs, type_defs, fn_sigs, receiver_ty, name.as_str())?;
            Some((name.to_string(), sig))
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn named_method_lookup_builtin_receiver_instantiates_type_args() {
        let sig = lookup_builtin_method_sig(&Ty::stream(Ty::String), "recv")
            .expect("builtin stream method should resolve");
        assert_eq!(sig.return_type, Ty::option(Ty::String));
    }

    #[test]
    fn named_method_lookup_builtin_stream_merges_methods_into_type_defs() {
        let mut type_defs = HashMap::new();
        type_defs.insert(
            crate::NominalId::for_test("Stream"),
            TypeDef {
                kind: TypeDefKind::Struct,
                name: "Stream".to_string(),
                type_params: vec![],
                bounds: crate::check::ParamBounds::default(),
                fields: HashMap::new(),
                variants: HashMap::new(),
                implicit_variant_fields: HashMap::default(),
                methods: HashMap::new(),
                doc_comment: None,
                field_order: vec![],
                is_indirect: false,
            },
        );

        let type_def = lookup_type_def(
            &crate::DefTable::new(),
            &type_defs,
            TypeHead::Builtin(BuiltinType::Stream),
        )
        .expect("builtin stream type def should resolve");
        // Channel-family naming: .recv() replaced .next() in the fundamental surface.
        assert!(type_def.methods.contains_key("recv"));
        // Iterator-style aliases are removed from the fundamental method table.
        assert!(!type_def.methods.contains_key("next"));
        assert!(!type_def.methods.contains_key("decode"));
    }

    #[test]
    fn nominal_stream_does_not_acquire_builtin_pipe_methods() {
        let receiver = Ty::user_for_test("Stream", vec![Ty::String]);
        assert!(lookup_builtin_method_sig(&receiver, "recv").is_none());
    }

    #[test]
    fn unlowerable_stream_codec_boundaries_are_not_builtin_methods() {
        assert!(
            lookup_builtin_method_sig(&Ty::stream(Ty::Bytes), "decode").is_none(),
            "Stream::decode must fail closed until lowering exists"
        );
        assert!(
            lookup_builtin_method_sig(&Ty::sink(Ty::Bytes), "encode").is_none(),
            "Sink::encode must fail closed until lowering exists"
        );
    }

    #[test]
    fn collect_method_sigs_fallback_instantiates_receiver_type_args() {
        let mut defs = crate::DefTable::new();
        let mut type_defs = HashMap::new();
        let wrapper = defs.mint_nominal_for_test("Wrapper");
        type_defs.insert(
            wrapper,
            TypeDef {
                kind: TypeDefKind::Struct,
                name: "Wrapper".to_string(),
                type_params: vec![crate::ParamHead::for_test("T")],
                bounds: crate::check::ParamBounds::default(),
                fields: HashMap::new(),
                variants: HashMap::new(),
                implicit_variant_fields: HashMap::default(),
                methods: HashMap::new(),
                doc_comment: None,
                field_order: vec![],
                is_indirect: false,
            },
        );

        let method = defs.mint_for_test("Wrapper::value");
        let mut dispatch = crate::check::dispatch_table::DispatchTable::default();
        let head = TypeHead::of_declaration(&defs, wrapper);
        dispatch.insert_generic(
            head,
            crate::check::dispatch_table::MethodOwner::Inherent,
            Symbol::intern("value"),
            method,
        );
        let mut fn_sigs = HashMap::new();
        fn_sigs.insert(
            method,
            FnSig {
                param_names: vec!["next".to_string()],
                params: vec![crate::CallableParameter {
                    ty: Ty::param(crate::ParamHead::for_test("T")),
                    ownership: crate::ParameterOwnership::Consume,
                }],
                return_type: Ty::param(crate::ParamHead::for_test("T")),
                ..FnSig::default()
            },
        );

        let methods = collect_method_sigs_for_receiver(
            &defs,
            &type_defs,
            FnSigView::new(&defs, &fn_sigs, &HashMap::new(), &dispatch),
            &Ty::named_head(head, vec![Ty::String]),
        );
        let (_, value_sig) = methods
            .into_iter()
            .find(|(method_name, _)| method_name == "value")
            .expect("fallback method should be collected");
        assert_eq!(
            value_sig.params,
            vec![crate::CallableParameter {
                ty: Ty::String,
                ownership: crate::ParameterOwnership::Consume
            }]
        );
        assert_eq!(value_sig.return_type, Ty::String);
    }

    #[test]
    fn lookup_method_sig_prefers_builtin_pipe_method_over_imported_stdlib_signature() {
        let mut type_defs = HashMap::new();
        type_defs.insert(
            crate::NominalId::for_test("Sink"),
            TypeDef {
                kind: TypeDefKind::Struct,
                name: "Sink".to_string(),
                type_params: vec![],
                bounds: crate::check::ParamBounds::default(),
                fields: HashMap::new(),
                variants: HashMap::new(),
                implicit_variant_fields: HashMap::default(),
                methods: {
                    let mut methods = HashMap::new();
                    methods.insert(
                        "send".to_string(),
                        FnSig {
                            param_names: vec!["data".to_string()],
                            params: vec![crate::CallableParameter::borrowed(Ty::String)],
                            return_type: Ty::Unit,
                            ..FnSig::default()
                        },
                    );
                    methods
                },
                doc_comment: None,
                field_order: vec![],
                is_indirect: false,
            },
        );

        let mut fn_sigs = HashMap::new();
        fn_sigs.insert(
            "Sink::send".to_string(),
            FnSig {
                param_names: vec!["data".to_string()],
                params: vec![crate::CallableParameter::borrowed(Ty::String)],
                return_type: Ty::Unit,
                ..FnSig::default()
            },
        );

        let sig = lookup_method_sig(
            &crate::DefTable::new(),
            &type_defs,
            crate::check::FnSigFixture::new(fn_sigs.clone()).view(),
            &Ty::sink(Ty::I64),
            "send",
        )
        .expect("builtin pipe method should resolve");
        assert_eq!(
            sig.params,
            vec![crate::CallableParameter::borrowed(Ty::I64)]
        );
    }

    #[test]
    fn lookup_type_def_overrides_imported_stdlib_pipe_methods_with_builtin_generics() {
        let mut type_defs = HashMap::new();
        type_defs.insert(
            crate::NominalId::for_test("Stream"),
            TypeDef {
                kind: TypeDefKind::Struct,
                name: "Stream".to_string(),
                type_params: vec![],
                bounds: crate::check::ParamBounds::default(),
                fields: HashMap::new(),
                variants: HashMap::new(),
                implicit_variant_fields: HashMap::default(),
                methods: {
                    let mut methods = HashMap::new();
                    methods.insert(
                        "recv".to_string(),
                        FnSig {
                            return_type: Ty::option(Ty::String),
                            ..FnSig::default()
                        },
                    );
                    methods
                },
                doc_comment: None,
                field_order: vec![],
                is_indirect: false,
            },
        );

        let type_def = lookup_type_def(
            &crate::DefTable::new(),
            &type_defs,
            TypeHead::Builtin(BuiltinType::Stream),
        )
        .expect("builtin stream type def should resolve");
        assert_eq!(
            type_def.type_params,
            vec![crate::DefTable::builtin_parameter(
                BuiltinType::Stream,
                0,
                "T"
            )]
        );
        assert_eq!(
            type_def.methods["recv"].return_type,
            Ty::option(Ty::param(type_def.type_params[0]))
        );
    }

    #[test]
    fn lookup_method_sig_collection_clone_returns_receiver_type() {
        let type_defs = HashMap::new();
        let fn_sigs: HashMap<String, FnSig> = HashMap::new();
        for receiver_ty in [
            Ty::Named {
                head: crate::TypeHead::Builtin(BuiltinType::Vec),
                args: vec![Ty::String],
            },
            Ty::Named {
                head: crate::TypeHead::Builtin(BuiltinType::HashMap),
                args: vec![Ty::String, Ty::I64],
            },
            Ty::Named {
                head: crate::TypeHead::Builtin(BuiltinType::HashSet),
                args: vec![Ty::String],
            },
        ] {
            let sig = lookup_method_sig(
                &crate::DefTable::new(),
                &type_defs,
                crate::check::FnSigFixture::new(fn_sigs.clone()).view(),
                &receiver_ty,
                "clone",
            )
            .expect("collection clone should resolve");
            assert!(sig.params.is_empty());
            assert_eq!(sig.return_type, receiver_ty);
        }
    }

    #[test]
    fn collect_method_sigs_includes_hashset_clone() {
        let methods = collect_method_sigs_for_receiver(
            &crate::DefTable::new(),
            &HashMap::new(),
            crate::check::FnSigFixture::default().view(),
            &Ty::builtin_named(BuiltinType::HashSet, vec![Ty::String]),
        );
        let (_, sig) = methods
            .into_iter()
            .find(|(method_name, _)| method_name == "clone")
            .expect("HashSet clone should be collected");
        assert_eq!(
            sig.return_type,
            Ty::Named {
                head: crate::TypeHead::Builtin(BuiltinType::HashSet),
                args: vec![Ty::String]
            }
        );
    }
}
