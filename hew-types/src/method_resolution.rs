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

fn instantiate_named_method_sig(mut sig: FnSig, type_params: &[String], type_args: &[Ty]) -> FnSig {
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
    type_defs: &'a HashMap<String, TypeDef>,
    type_name: &str,
) -> Option<&'a TypeDef> {
    type_defs.get(type_name).or_else(|| {
        type_name
            .rsplit_once('.')
            .and_then(|(_, short)| type_defs.get(short))
    })
}

fn lookup_user_fn_sig<'a>(fn_sigs: &'a HashMap<String, FnSig>, key: &str) -> Option<&'a FnSig> {
    fn_sigs
        .get(key)
        .or_else(|| {
            let (type_name, method_name) = key.split_once("::")?;
            let (_, short) = type_name.rsplit_once('.')?;
            fn_sigs.get(&format!("{short}::{method_name}"))
        })
        .or_else(|| {
            let (type_name, method_name) = key.split_once("::")?;
            fn_sigs.iter().find_map(|(sig_name, sig)| {
                (scoped_method_name(sig_name, type_name) == Some(method_name)).then_some(sig)
            })
        })
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
        // LocalPid<T> wraps an actor type T.
        // RemotePid<T> is intentionally NOT unwrapped here: its methods are
        // resolved against RemotePid itself, not T.
        //
        // NOTE: `collect_method_sigs_for_receiver` handles LocalPid specially
        // (collecting own handle methods + actor receive handlers) and does NOT
        // call this helper for that type. This arm covers the single-method
        // `lookup_method_sig` path used by the checker's fallback for actor
        // receive-fn dispatch — it unwraps to T so e.g. `pid.increment(arg)`
        // resolves against `Counter::increment` in fn_sigs.
        Ty::Named {
            builtin: Some(BuiltinType::LocalPid),
            args,
            ..
        } if args.len() == 1 => named_receiver_parts(&args[0]),
        Ty::Named { name, args, .. } => Some((name.as_str(), args.as_slice())),
        _ => None,
    }
}

fn scoped_method_name<'a>(sig_name: &'a str, type_name: &str) -> Option<&'a str> {
    let scoped_prefix = format!("{type_name}::");
    sig_name
        .rsplit_once('.')
        .and_then(|(_, unqualified)| unqualified.strip_prefix(&scoped_prefix))
}

fn lookup_collection_clone_method_sig(receiver_ty: &Ty, method: &str) -> Option<FnSig> {
    if method != "clone" {
        return None;
    }
    match receiver_ty {
        Ty::Named {
            builtin: Some(BuiltinType::Vec),
            args,
            ..
        } if args.len() == 1 => Some(FnSig {
            return_type: receiver_ty.clone(),
            ..FnSig::default()
        }),
        Ty::Named {
            builtin: Some(BuiltinType::HashMap),
            args,
            ..
        } if args.len() == 2 => Some(FnSig {
            return_type: receiver_ty.clone(),
            ..FnSig::default()
        }),
        Ty::Named {
            builtin: Some(BuiltinType::HashSet),
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
    type_defs: &HashMap<String, TypeDef>,
    fn_sigs: &HashMap<String, FnSig>,
    type_name: &str,
    type_args: &[Ty],
    method: &str,
) -> Option<FnSig> {
    if let Some(td) = lookup_user_type_def(type_defs, type_name) {
        if let Some(sig) = td.methods.get(method).cloned() {
            return Some(instantiate_named_method_sig(
                sig,
                &td.type_params,
                type_args,
            ));
        }
    }

    let type_params = lookup_user_type_def(type_defs, type_name).map(|td| td.type_params.clone());

    // Concrete-specialised-impl lookup (#2270): when the receiver has non-empty
    // concrete type args (e.g. `Wrapper<i64>`), try the mangled key first
    // (`"Wrapper$$i64::describe"`).  This resolves to the correct impl when two
    // concrete specialisations (`Wrapper<i64>` and `Wrapper<string>`) both exist.
    // Falls back to the bare key for generic impls (`impl<T> Trait for Wrapper<T>`),
    // inherent methods, and any arg that cannot be mangled.
    //
    // IMPORTANT: use a direct `fn_sigs.get` (not `lookup_user_fn_sig`) for the
    // mangled key.  `lookup_user_fn_sig` has a module-stripping fallback that
    // strips after the last `.` in the type-name part of the key.  A mangled key
    // like `"LocalPid$$bank.Account::who"` contains a `.` in the mangled segment,
    // causing the fallback to strip `"LocalPid$$bank"` and resolve `"Account::who"` —
    // i.e. the root-module actor's method — instead of failing.  The mangled
    // key is an exact, unambiguous identifier; it must only match via direct lookup.
    if !type_args.is_empty() {
        let resolved_args: Option<Vec<ResolvedTy>> = type_args
            .iter()
            .map(|ty| ResolvedTy::from_ty(ty).ok())
            .collect();
        if let Some(resolved_args) = resolved_args {
            if let Some(mangled_self) = mangle_impl_self_name(type_name, &resolved_args) {
                let mangled_key = format!("{mangled_self}::{method}");
                // Direct lookup only — no module-stripping fallback (see comment above).
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

/// Instantiate a snapshotted builtin `Result`/`Option` method signature against
/// the receiver's type arguments.
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
pub fn instantiate_builtin_result_option_method_sig(
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
    type_defs: &HashMap<String, TypeDef>,
    fn_sigs: &HashMap<String, FnSig>,
    receiver_ty: &Ty,
    method: &str,
) -> Option<FnSig> {
    let (type_name, type_args) = named_receiver_parts(receiver_ty)?;
    lookup_builtin_method_sig(receiver_ty, method)
        .or_else(|| lookup_collection_clone_method_sig(receiver_ty, method))
        .or_else(|| lookup_named_method_sig(type_defs, fn_sigs, type_name, type_args, method))
}

/// Synthesize a type definition for a builtin type name.
#[must_use]
pub fn builtin_type_def(type_name: &str) -> Option<TypeDef> {
    builtin_named_type(type_name).map(|kind| builtin_named_type_def(kind).clone())
}

/// Look up a type definition, augmenting builtin placeholders with builtin methods.
#[must_use]
pub fn lookup_type_def(type_defs: &HashMap<String, TypeDef>, type_name: &str) -> Option<TypeDef> {
    let user_type = lookup_user_type_def(type_defs, type_name).cloned();
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
/// Returns `None` for `LocalPid<T>`: actor-handle types do not expose the
/// actor's internal fields to callers. Method completions on handles come
/// from `collect_method_sigs_for_receiver` instead.
#[must_use]
pub fn lookup_type_def_for_receiver(
    type_defs: &HashMap<String, TypeDef>,
    receiver_ty: &Ty,
) -> Option<TypeDef> {
    // Actor handles have no public fields accessible via the handle.
    if let Ty::Named {
        builtin: Some(BuiltinType::LocalPid),
        ..
    } = receiver_ty
    {
        return None;
    }
    let (type_name, _) = named_receiver_parts(receiver_ty)?;
    lookup_type_def(type_defs, type_name)
}

/// Collect all method signatures visible on a named type.
#[must_use]
pub fn collect_method_sigs_for_named_type(
    type_defs: &HashMap<String, TypeDef>,
    fn_sigs: &HashMap<String, FnSig>,
    type_name: &str,
    type_args: &[Ty],
) -> Vec<(String, FnSig)> {
    let mut methods = Vec::new();
    let mut seen = HashSet::new();
    let receiver_ty = Ty::Named {
        builtin: crate::lookup_builtin_type(type_name),
        name: type_name.to_string(),
        args: type_args.to_vec(),
    };
    let receiver_type_params = lookup_type_def(type_defs, type_name)
        .map(|type_def| type_def.type_params)
        .unwrap_or_default();

    if let Some(sig) = lookup_collection_clone_method_sig(&receiver_ty, "clone") {
        seen.insert("clone".to_string());
        methods.push(("clone".to_string(), sig));
    }

    if let Some(type_def) = lookup_type_def(type_defs, type_name) {
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
    let short_prefix = type_name
        .rsplit_once('.')
        .map(|(_, short)| format!("{short}::"));
    for (sig_name, sig) in fn_sigs {
        let method_name = sig_name
            .strip_prefix(&exact_prefix)
            .or_else(|| {
                short_prefix
                    .as_ref()
                    .and_then(|prefix| sig_name.strip_prefix(prefix))
            })
            .or_else(|| scoped_method_name(sig_name, type_name));
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
/// For `LocalPid<T>`, this produces two groups:
/// 1. The handle's own impl methods (`tell`, `to_remote_via`, etc.) registered
///    in `fn_sigs` as `"LocalPid::{method}"`.
/// 2. The actor's receive handlers (the methods callers can dispatch to via the
///    handle), resolved against the inner actor type T.
///
/// This two-group collection is why the handle type is NOT unwrapped through
/// `named_receiver_parts` for this call path — the checker's own `LocalPid` arm
/// in `methods.rs` handles the dispatch logic; this path drives LSP completions.
#[must_use]
pub fn collect_method_sigs_for_receiver(
    type_defs: &HashMap<String, TypeDef>,
    fn_sigs: &HashMap<String, FnSig>,
    receiver_ty: &Ty,
) -> Vec<(String, FnSig)> {
    // Actor-handle types: collect from the handle type itself AND from the
    // inner actor type T (receive handlers).
    let handle_name = match receiver_ty {
        Ty::Named {
            builtin: Some(BuiltinType::LocalPid),
            args,
            ..
        } if args.len() == 1 => Some(("LocalPid", &args[0])),
        _ => None,
    };

    if let Some((handle_type_name, inner_ty)) = handle_name {
        // Own handle methods (e.g. `tell`, `to_remote_via`).
        let handle_methods =
            collect_method_sigs_for_named_type(type_defs, fn_sigs, handle_type_name, &[]);
        // Actor receive handlers resolved against the inner actor type.
        let actor_methods = if let Some((actor_type_name, actor_type_args)) =
            named_receiver_parts(inner_ty)
        {
            collect_method_sigs_for_named_type(type_defs, fn_sigs, actor_type_name, actor_type_args)
        } else {
            Vec::new()
        };

        // Merge: handle's own methods first, then actor methods, deduplicating
        // by name so handle methods win over same-named actor methods.
        let mut seen = HashSet::new();
        let mut methods = Vec::with_capacity(handle_methods.len() + actor_methods.len());
        for (name, sig) in handle_methods.into_iter().chain(actor_methods) {
            if seen.insert(name.clone()) {
                methods.push((name, sig));
            }
        }
        return methods;
    }

    let Some((type_name, type_args)) = named_receiver_parts(receiver_ty) else {
        return Vec::new();
    };
    collect_method_sigs_for_named_type(type_defs, fn_sigs, type_name, type_args)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn named_method_lookup_builtin_receiver_instantiates_type_args() {
        let sig = lookup_builtin_method_sig(&Ty::receiver(Ty::String), "recv")
            .expect("builtin receiver method should resolve");
        assert_eq!(sig.return_type, Ty::option(Ty::String));
    }

    #[test]
    fn named_method_lookup_builtin_stream_merges_methods_into_type_defs() {
        let mut type_defs = HashMap::new();
        type_defs.insert(
            "Stream".to_string(),
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

        let type_def = lookup_type_def(&type_defs, "stream.Stream")
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
        let mut type_defs = HashMap::new();
        type_defs.insert(
            "Wrapper".to_string(),
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
                params: vec![Ty::Named {
                    builtin: None,
                    name: "T".to_string(),
                    args: vec![],
                }],
                return_type: Ty::Named {
                    builtin: None,
                    name: "T".to_string(),
                    args: vec![],
                },
                ..FnSig::default()
            },
        );

        let methods =
            collect_method_sigs_for_named_type(&type_defs, &fn_sigs, "Wrapper", &[Ty::String]);
        let (_, value_sig) = methods
            .into_iter()
            .find(|(method_name, _)| method_name == "value")
            .expect("fallback method should be collected");
        assert_eq!(value_sig.params, vec![Ty::String]);
        assert_eq!(value_sig.return_type, Ty::String);
    }

    #[test]
    fn lookup_method_sig_prefers_builtin_channel_method_over_imported_stdlib_signature() {
        let mut type_defs = HashMap::new();
        type_defs.insert(
            "Sender".to_string(),
            TypeDef {
                kind: TypeDefKind::Struct,
                name: "Sender".to_string(),
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
            "Sender::send".to_string(),
            FnSig {
                param_names: vec!["data".to_string()],
                params: vec![Ty::String],
                return_type: Ty::Unit,
                ..FnSig::default()
            },
        );

        let sig = lookup_method_sig(&type_defs, &fn_sigs, &Ty::sender(Ty::I64), "send")
            .expect("builtin channel method should resolve");
        assert_eq!(sig.params, vec![Ty::I64]);
    }

    #[test]
    fn lookup_type_def_overrides_imported_stdlib_channel_methods_with_builtin_generics() {
        let mut type_defs = HashMap::new();
        type_defs.insert(
            "Receiver".to_string(),
            TypeDef {
                kind: TypeDefKind::Struct,
                name: "Receiver".to_string(),
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

        let type_def = lookup_type_def(&type_defs, "Receiver")
            .expect("builtin receiver type def should resolve");
        assert_eq!(type_def.type_params, vec!["T".to_string()]);
        assert_eq!(
            type_def.methods["recv"].return_type,
            Ty::option(Ty::Named {
                builtin: None,
                name: "T".to_string(),
                args: vec![],
            })
        );
    }

    #[test]
    fn lookup_method_sig_collection_clone_returns_receiver_type() {
        let type_defs = HashMap::new();
        let fn_sigs = HashMap::new();
        for receiver_ty in [
            Ty::Named {
                builtin: Some(BuiltinType::Vec),
                name: "Vec".to_string(),
                args: vec![Ty::String],
            },
            Ty::Named {
                builtin: Some(BuiltinType::HashMap),
                name: "HashMap".to_string(),
                args: vec![Ty::String, Ty::I64],
            },
            Ty::Named {
                builtin: Some(BuiltinType::HashSet),
                name: "HashSet".to_string(),
                args: vec![Ty::String],
            },
        ] {
            let sig = lookup_method_sig(&type_defs, &fn_sigs, &receiver_ty, "clone")
                .expect("collection clone should resolve");
            assert!(sig.params.is_empty());
            assert_eq!(sig.return_type, receiver_ty);
        }
    }

    #[test]
    fn collect_method_sigs_includes_hashset_clone() {
        let methods = collect_method_sigs_for_named_type(
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
                builtin: Some(BuiltinType::HashSet),
                name: "HashSet".to_string(),
                args: vec![Ty::String],
            }
        );
    }
}
