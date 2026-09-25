//! Shared builtin and named method resolution helpers.
#![expect(
    clippy::implicit_hasher,
    reason = "helpers mirror the concrete HashMap types stored in TypeCheckOutput and Checker"
)]

use std::collections::{HashMap, HashSet};

use crate::builtin_names::{builtin_named_type, builtin_type_def as builtin_named_type_def};
#[cfg(test)]
use crate::check::TypeDefKind;
use crate::check::{FnSig, TypeDef};
use crate::resolved_ty::{mangle_impl_self_name, ResolvedTy};
use crate::BuiltinType;
use crate::Ty;
use crate::{DefTable, NominalId};

fn instantiate_named_method_sig(mut sig: FnSig, type_params: &[String], type_args: &[Ty]) -> FnSig {
    // A method's own type parameter (`map<U>`) is a binder distinct from any
    // caller parameter the receiver carries in (`Option<U>` inside `fn f<U>`).
    // Rename a colliding binder first so substituting the impl parameters
    // cannot make the two indistinguishable.
    let captures = |name: &str| type_args.iter().any(|arg| arg.mentions_named_param(name));
    let mut renames: HashMap<String, Ty> = HashMap::new();
    for method_param in sig
        .type_params
        .iter()
        .filter(|param| !type_params.contains(param) && captures(param))
    {
        let mut fresh = format!("{method_param}'");
        while captures(&fresh) || sig.type_params.contains(&fresh) {
            fresh.push('\'');
        }
        renames.insert(method_param.clone(), Ty::param(&fresh));
    }
    if !renames.is_empty() {
        let binder_name = |name: &String| match renames.get(name) {
            Some(Ty::Named { head, .. }) => head.spelling().to_string(),
            _ => name.clone(),
        };
        for param_ty in &mut sig.params {
            *param_ty = param_ty.substitute_named_params_parallel(&renames);
        }
        sig.return_type = sig.return_type.substitute_named_params_parallel(&renames);
        sig.type_params = sig.type_params.iter().map(binder_name).collect();
        sig.type_param_bounds = sig
            .type_param_bounds
            .into_iter()
            .map(|(name, bounds)| (binder_name(&name), bounds))
            .collect();
    }

    let subst_map: HashMap<String, Ty> = type_params
        .iter()
        .zip(type_args.iter())
        .map(|(p, a)| (p.clone(), a.clone()))
        .collect();
    for param_ty in &mut sig.params {
        *param_ty = param_ty.substitute_named_params_parallel(&subst_map);
    }
    sig.return_type = sig.return_type.substitute_named_params_parallel(&subst_map);

    let substituted_params: HashSet<_> = type_params.iter().cloned().collect();
    sig.type_params
        .retain(|type_param| !substituted_params.contains(type_param));
    sig.type_param_bounds
        .retain(|type_param, _| !substituted_params.contains(type_param));
    sig
}

fn lookup_user_type_def<'a>(
    defs: &'a DefTable,
    type_defs: &'a HashMap<NominalId, TypeDef>,
    type_name: &str,
) -> Option<&'a TypeDef> {
    // TRANSITION(A1 commit 3): the catalog move hands method lookup the
    // receiver head; a registry key that spells a builtin names its std
    // declaration until then.
    let types = crate::check::TypeDefView::new(defs, type_defs);
    types.at_path(type_name).or_else(|| {
        types.of(crate::TypeHead::Builtin(crate::lookup_builtin_type(
            type_name,
        )?))
    })
}

fn lookup_user_fn_sig<'a>(fn_sigs: &'a HashMap<String, FnSig>, key: &str) -> Option<&'a FnSig> {
    fn_sigs.get(key)
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

fn named_receiver_parts(ty: &Ty) -> Option<(&str, &[Ty])> {
    match ty {
        Ty::I8 => Some(("i8", &[])),
        Ty::I16 => Some(("i16", &[])),
        Ty::I32 => Some(("i32", &[])),
        Ty::I64 => Some(("i64", &[])),
        Ty::U8 => Some(("u8", &[])),
        Ty::U16 => Some(("u16", &[])),
        Ty::U32 => Some(("u32", &[])),
        Ty::U64 => Some(("u64", &[])),
        Ty::Isize => Some(("isize", &[])),
        Ty::Usize => Some(("usize", &[])),
        Ty::F32 => Some(("f32", &[])),
        Ty::F64 => Some(("f64", &[])),
        Ty::Bool => Some(("bool", &[])),
        Ty::Char => Some(("char", &[])),
        Ty::String => Some(("string", &[])),
        Ty::Bytes => Some(("bytes", &[])),
        Ty::CancellationToken => Some(("CancellationToken", &[])),
        Ty::Duration => Some(("duration", &[])),
        // RemotePid<T> is intentionally NOT unwrapped here: its methods are
        // resolved against RemotePid itself, not T.
        //
        // An actor handle names the actor itself, so the single-method
        // `lookup_method_sig` path the checker falls back to for receive-fn
        // dispatch resolves `orders.order(arg)` against `Orders::order` in
        // `fn_sigs` through the ordinary named arm below.
        Ty::Named { head, args } => Some((head.registry_key(), args.as_slice())),
        _ => None,
    }
}

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

/// Look up a non-builtin named method via `type_defs` first, then `fn_sigs`.
#[must_use]
pub fn lookup_named_method_sig(
    defs: &DefTable,
    type_defs: &HashMap<NominalId, TypeDef>,
    fn_sigs: &HashMap<String, FnSig>,
    type_name: &str,
    type_args: &[Ty],
    method: &str,
) -> Option<FnSig> {
    named_method_sig(
        lookup_user_type_def(defs, type_defs, type_name),
        fn_sigs,
        type_name,
        type_args,
        method,
    )
}

fn named_method_sig(
    user: Option<&TypeDef>,
    fn_sigs: &HashMap<String, FnSig>,
    type_name: &str,
    type_args: &[Ty],
    method: &str,
) -> Option<FnSig> {
    if let Some(td) = user {
        if let Some(sig) = td.methods.get(method).cloned() {
            return Some(instantiate_named_method_sig(
                sig,
                &td.type_params,
                type_args,
            ));
        }
    }

    let type_params = user.map(|td| td.type_params.clone());

    // Concrete-specialised-impl lookup (#2270): when the receiver has non-empty
    // concrete type args (e.g. `Wrapper<i64>`), try the mangled key first
    // (`"Wrapper$$i64::describe"`).  This resolves to the correct impl when two
    // concrete specialisations (`Wrapper<i64>` and `Wrapper<string>`) both exist.
    // Falls back to the bare key for generic impls (`impl<T> Trait for Wrapper<T>`),
    // inherent methods, and any arg that cannot be mangled.
    //
    // A specialised key is exact and unambiguous. It must never fall through
    // to a leaf-name retry when the specialised declaration is absent.
    if !type_args.is_empty() {
        let resolved_args: Option<Vec<ResolvedTy>> = type_args
            .iter()
            .map(|ty| ResolvedTy::from_ty(ty).ok())
            .collect();
        if let Some(resolved_args) = resolved_args {
            if let Some(mangled_self) = mangle_impl_self_name(type_name, &resolved_args) {
                let mangled_key = format!("{mangled_self}::{method}");
                if let Some(sig) = fn_sigs.get(&mangled_key).cloned() {
                    // The mangled-key entry has no generic type params (it was
                    // registered for a concrete specialised impl), so we
                    // instantiate with the type-def params (empty for concrete).
                    let tps = type_params.clone().unwrap_or_default();
                    return Some(instantiate_named_method_sig(sig, &tps, type_args));
                }
            }
        }
    }

    lookup_user_fn_sig(fn_sigs, &format!("{type_name}::{method}"))
        .cloned()
        .map(|sig| {
            let type_params = type_params.unwrap_or_else(|| sig.type_params.clone());
            instantiate_named_method_sig(sig, &type_params, type_args)
        })
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
    type_params: &[String],
    type_args: &[Ty],
) -> FnSig {
    instantiate_named_method_sig(sig.clone(), type_params, type_args)
}

/// Look up a builtin method signature for `Sender`, `Receiver`, `Stream`, or `Sink`.
#[must_use]
pub fn lookup_builtin_method_sig(receiver_ty: &Ty, method: &str) -> Option<FnSig> {
    let (type_name, type_args) = named_receiver_parts(receiver_ty)?;
    let builtin = builtin_named_type_def(builtin_named_type(type_name)?);
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
    fn_sigs: &HashMap<String, FnSig>,
    receiver_ty: &Ty,
    method: &str,
) -> Option<FnSig> {
    let (type_name, type_args) = named_receiver_parts(receiver_ty)?;
    let user = crate::check::TypeDefView::new(defs, type_defs).of_ty(receiver_ty);
    lookup_builtin_method_sig(receiver_ty, method)
        .or_else(|| lookup_collection_clone_method_sig(receiver_ty, method))
        .or_else(|| named_method_sig(user, fn_sigs, type_name, type_args, method))
}

/// Synthesize a type definition for a builtin type name.
#[must_use]
pub fn builtin_type_def(type_name: &str) -> Option<TypeDef> {
    builtin_named_type(type_name).map(|kind| builtin_named_type_def(kind).clone())
}

/// Look up a type definition, augmenting builtin placeholders with builtin methods.
#[must_use]
pub fn lookup_type_def(
    defs: &DefTable,
    type_defs: &HashMap<NominalId, TypeDef>,
    type_name: &str,
) -> Option<TypeDef> {
    type_def_with_builtin(lookup_user_type_def(defs, type_defs, type_name), type_name)
}

fn type_def_with_builtin(user: Option<&TypeDef>, type_name: &str) -> Option<TypeDef> {
    let user_type = user.cloned();
    let builtin_type = builtin_type_def(type_name);
    match (user_type, builtin_type) {
        (Some(type_def), Some(builtin)) => Some(merge_builtin_type_def(type_def, builtin)),
        (Some(type_def), None) => Some(type_def),
        (None, Some(builtin)) => Some(builtin),
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
    let (type_name, _) = named_receiver_parts(receiver_ty)?;
    type_def_with_builtin(
        crate::check::TypeDefView::new(defs, type_defs).of_ty(receiver_ty),
        type_name,
    )
}

/// Collect all method signatures visible on a named type.
#[must_use]
pub fn collect_method_sigs_for_named_type(
    defs: &DefTable,
    type_defs: &HashMap<NominalId, TypeDef>,
    fn_sigs: &HashMap<String, FnSig>,
    type_name: &str,
    type_args: &[Ty],
) -> Vec<(String, FnSig)> {
    collect_named_method_sigs(
        lookup_user_type_def(defs, type_defs, type_name),
        fn_sigs,
        type_name,
        type_args,
    )
}

fn collect_named_method_sigs(
    user: Option<&TypeDef>,
    fn_sigs: &HashMap<String, FnSig>,
    type_name: &str,
    type_args: &[Ty],
) -> Vec<(String, FnSig)> {
    let mut methods = Vec::new();
    let mut seen = HashSet::new();
    let receiver_ty = Ty::registry_named(type_name, type_args.to_vec());
    let receiver_type_params = type_def_with_builtin(user, type_name)
        .map(|type_def| type_def.type_params)
        .unwrap_or_default();

    if let Some(sig) = lookup_collection_clone_method_sig(&receiver_ty, "clone") {
        seen.insert("clone".to_string());
        methods.push(("clone".to_string(), sig));
    }

    if let Some(type_def) = type_def_with_builtin(user, type_name) {
        for (method_name, sig) in type_def.methods {
            if seen.insert(method_name.clone()) {
                methods.push((
                    method_name,
                    instantiate_named_method_sig(sig, &type_def.type_params, type_args),
                ));
            }
        }
    }

    let exact_prefix = format!("{type_name}::");
    for (sig_name, sig) in fn_sigs {
        let method_name = sig_name.strip_prefix(&exact_prefix);
        if let Some(method_name) = method_name {
            let method_name = method_name.to_string();
            if seen.insert(method_name.clone()) {
                methods.push((
                    method_name,
                    instantiate_named_method_sig(sig.clone(), &receiver_type_params, type_args),
                ));
            }
        }
    }

    methods
}

/// Collect all method signatures visible on a receiver type.
///
/// For an actor handle, this produces two groups:
/// 1. The handle's own impl methods (`send`, etc.).
/// 2. The actor's receive handlers (the methods callers can dispatch to via the
///    handle), resolved against the handle's own name.
///
/// An actor handle is the actor's own type, so this path offers the actor's
/// receive handlers; the checker's own dispatch arm in `methods.rs` decides the
/// call, and this path drives LSP completions.
#[must_use]
pub fn collect_method_sigs_for_receiver(
    defs: &DefTable,
    type_defs: &HashMap<NominalId, TypeDef>,
    fn_sigs: &HashMap<String, FnSig>,
    receiver_ty: &Ty,
) -> Vec<(String, FnSig)> {
    let Some((type_name, type_args)) = named_receiver_parts(receiver_ty) else {
        return Vec::new();
    };
    collect_named_method_sigs(
        crate::check::TypeDefView::new(defs, type_defs).of_ty(receiver_ty),
        fn_sigs,
        type_name,
        type_args,
    )
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
                bounds: HashMap::new(),
                fields: HashMap::new(),
                variants: HashMap::new(),
                methods: HashMap::new(),
                doc_comment: None,
                field_order: vec![],
                is_indirect: false,
            },
        );

        let type_def = lookup_type_def(&crate::DefTable::new(), &type_defs, "stream.Stream")
            .expect("builtin stream type def should resolve");
        // Channel-family naming: .recv() replaced .next() in the fundamental surface.
        assert!(type_def.methods.contains_key("recv"));
        // Iterator-style aliases are removed from the fundamental method table.
        assert!(!type_def.methods.contains_key("next"));
        // `.collect` is now wired into the fundamental surface (Stream<string>).
        assert!(type_def.methods.contains_key("collect"));
        assert!(!type_def.methods.contains_key("decode"));
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
        type_defs.insert(
            defs.mint_nominal_for_test("Wrapper"),
            TypeDef {
                kind: TypeDefKind::Struct,
                name: "Wrapper".to_string(),
                type_params: vec!["T".to_string()],
                bounds: HashMap::new(),
                fields: HashMap::new(),
                variants: HashMap::new(),
                methods: HashMap::new(),
                doc_comment: None,
                field_order: vec![],
                is_indirect: false,
            },
        );

        let mut fn_sigs = HashMap::new();
        fn_sigs.insert(
            "Wrapper::value".to_string(),
            FnSig {
                param_names: vec!["next".to_string()],
                params: vec![Ty::param("T")],
                return_type: Ty::param("T"),
                ..FnSig::default()
            },
        );

        let methods = collect_method_sigs_for_named_type(
            &defs,
            &type_defs,
            &fn_sigs,
            "Wrapper",
            &[Ty::String],
        );
        let (_, value_sig) = methods
            .into_iter()
            .find(|(method_name, _)| method_name == "value")
            .expect("fallback method should be collected");
        assert_eq!(value_sig.params, vec![Ty::String]);
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
                bounds: HashMap::new(),
                fields: HashMap::new(),
                variants: HashMap::new(),
                methods: {
                    let mut methods = HashMap::new();
                    methods.insert(
                        "send".to_string(),
                        FnSig {
                            param_names: vec!["data".to_string()],
                            params: vec![Ty::String],
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
                params: vec![Ty::String],
                return_type: Ty::Unit,
                ..FnSig::default()
            },
        );

        let sig = lookup_method_sig(
            &crate::DefTable::new(),
            &type_defs,
            &fn_sigs,
            &Ty::sink(Ty::I64),
            "send",
        )
        .expect("builtin pipe method should resolve");
        assert_eq!(sig.params, vec![Ty::I64]);
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
                bounds: HashMap::new(),
                fields: HashMap::new(),
                variants: HashMap::new(),
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

        let type_def = lookup_type_def(&crate::DefTable::new(), &type_defs, "Stream")
            .expect("builtin stream type def should resolve");
        assert_eq!(type_def.type_params, vec!["T".to_string()]);
        assert_eq!(
            type_def.methods["recv"].return_type,
            Ty::option(Ty::param("T"))
        );
    }

    #[test]
    fn lookup_method_sig_collection_clone_returns_receiver_type() {
        let type_defs = HashMap::new();
        let fn_sigs = HashMap::new();
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
                &fn_sigs,
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
        let methods = collect_method_sigs_for_named_type(
            &crate::DefTable::new(),
            &HashMap::new(),
            &HashMap::new(),
            "HashSet",
            &[Ty::String],
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
