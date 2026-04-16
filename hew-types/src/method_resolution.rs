//! Shared builtin and named method resolution helpers.
#![expect(
    clippy::implicit_hasher,
    reason = "helpers mirror the concrete HashMap types stored in TypeCheckOutput and Checker"
)]

use std::collections::{HashMap, HashSet};

use crate::builtin_names::{builtin_named_type, BuiltinNamedType};
use crate::check::{FnSig, TypeDef, TypeDefKind};
use crate::Ty;

fn generic_ty(name: &str) -> Ty {
    Ty::Named {
        name: name.to_string(),
        args: vec![],
    }
}

fn instantiate_named_method_sig(mut sig: FnSig, type_params: &[String], type_args: &[Ty]) -> FnSig {
    for (type_param, type_arg) in type_params.iter().zip(type_args.iter()) {
        for param_ty in &mut sig.params {
            *param_ty = param_ty.substitute_named_param(type_param, type_arg);
        }
        sig.return_type = sig.return_type.substitute_named_param(type_param, type_arg);
    }

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
    fn_sigs.get(key).or_else(|| {
        let (type_name, method_name) = key.split_once("::")?;
        let (_, short) = type_name.rsplit_once('.')?;
        fn_sigs.get(&format!("{short}::{method_name}"))
    })
}

#[expect(
    clippy::too_many_lines,
    reason = "builtin method table stays clearer when each intrinsic signature is listed inline"
)]
fn builtin_methods(kind: BuiltinNamedType) -> HashMap<String, FnSig> {
    let item_ty = generic_ty("T");
    let option_item_ty = Ty::option(item_ty.clone());
    let item_fn = Ty::Function {
        params: vec![item_ty.clone()],
        ret: Box::new(item_ty.clone()),
    };
    let item_predicate = Ty::Function {
        params: vec![item_ty.clone()],
        ret: Box::new(Ty::Bool),
    };

    let entries: &[(&str, FnSig)] = match kind {
        BuiltinNamedType::Sender => &[
            (
                "send",
                FnSig {
                    param_names: vec!["value".to_string()],
                    params: vec![item_ty.clone()],
                    return_type: Ty::Unit,
                    ..FnSig::default()
                },
            ),
            (
                "clone",
                FnSig {
                    return_type: Ty::sender(item_ty.clone()),
                    ..FnSig::default()
                },
            ),
            (
                "close",
                FnSig {
                    return_type: Ty::Unit,
                    ..FnSig::default()
                },
            ),
        ],
        BuiltinNamedType::Receiver => &[
            (
                "recv",
                FnSig {
                    return_type: option_item_ty.clone(),
                    ..FnSig::default()
                },
            ),
            (
                "try_recv",
                FnSig {
                    return_type: option_item_ty,
                    ..FnSig::default()
                },
            ),
            (
                "close",
                FnSig {
                    return_type: Ty::Unit,
                    ..FnSig::default()
                },
            ),
        ],
        BuiltinNamedType::Stream => &[
            (
                "next",
                FnSig {
                    return_type: Ty::option(item_ty.clone()),
                    ..FnSig::default()
                },
            ),
            (
                "collect",
                FnSig {
                    return_type: Ty::String,
                    ..FnSig::default()
                },
            ),
            (
                "close",
                FnSig {
                    return_type: Ty::Unit,
                    ..FnSig::default()
                },
            ),
            (
                "lines",
                FnSig {
                    return_type: Ty::stream(Ty::String),
                    ..FnSig::default()
                },
            ),
            (
                "chunks",
                FnSig {
                    param_names: vec!["size".to_string()],
                    params: vec![Ty::I64],
                    return_type: Ty::stream(item_ty.clone()),
                    ..FnSig::default()
                },
            ),
            (
                "take",
                FnSig {
                    param_names: vec!["count".to_string()],
                    params: vec![Ty::I64],
                    return_type: Ty::stream(item_ty.clone()),
                    ..FnSig::default()
                },
            ),
            (
                "map",
                FnSig {
                    param_names: vec!["mapper".to_string()],
                    params: vec![item_fn],
                    return_type: Ty::stream(item_ty.clone()),
                    ..FnSig::default()
                },
            ),
            (
                "filter",
                FnSig {
                    param_names: vec!["predicate".to_string()],
                    params: vec![item_predicate],
                    return_type: Ty::stream(item_ty),
                    ..FnSig::default()
                },
            ),
        ],
        BuiltinNamedType::Sink => &[
            (
                "write",
                FnSig {
                    param_names: vec!["value".to_string()],
                    params: vec![item_ty],
                    return_type: Ty::Unit,
                    ..FnSig::default()
                },
            ),
            (
                "flush",
                FnSig {
                    return_type: Ty::Unit,
                    ..FnSig::default()
                },
            ),
            (
                "close",
                FnSig {
                    return_type: Ty::Unit,
                    ..FnSig::default()
                },
            ),
        ],
    };

    entries
        .iter()
        .map(|(name, sig)| ((*name).to_string(), sig.clone()))
        .collect()
}

fn builtin_type_def_for(kind: BuiltinNamedType) -> TypeDef {
    TypeDef {
        kind: TypeDefKind::Struct,
        name: kind.canonical_name().to_string(),
        type_params: vec!["T".to_string()],
        fields: HashMap::new(),
        variants: HashMap::new(),
        methods: builtin_methods(kind),
        doc_comment: None,
        is_indirect: false,
    }
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
        Ty::Named { name, args } if name == "ActorRef" && args.len() == 1 => {
            named_receiver_parts(&args[0])
        }
        Ty::Named { name, args } => Some((name.as_str(), args.as_slice())),
        _ => None,
    }
}

fn lookup_collection_clone_method_sig(receiver_ty: &Ty, method: &str) -> Option<FnSig> {
    if method != "clone" {
        return None;
    }
    match receiver_ty {
        Ty::Named { name, args } if name == "Vec" && args.len() == 1 => Some(FnSig {
            return_type: receiver_ty.clone(),
            ..FnSig::default()
        }),
        Ty::Named { name, args } if name == "HashMap" && args.len() == 2 => Some(FnSig {
            return_type: receiver_ty.clone(),
            ..FnSig::default()
        }),
        Ty::Named { name, args } if name == "HashSet" && args.len() == 1 => Some(FnSig {
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

    let type_params = lookup_user_type_def(type_defs, type_name)
        .map(|td| td.type_params.clone())
        .unwrap_or_default();

    lookup_user_fn_sig(fn_sigs, &format!("{type_name}::{method}"))
        .cloned()
        .map(|sig| instantiate_named_method_sig(sig, &type_params, type_args))
}

/// Look up a builtin method signature for `Sender`, `Receiver`, `Stream`, or `Sink`.
#[must_use]
pub fn lookup_builtin_method_sig(receiver_ty: &Ty, method: &str) -> Option<FnSig> {
    let (type_name, type_args) = named_receiver_parts(receiver_ty)?;
    let builtin = builtin_type_def(type_name)?;
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
    builtin_named_type(type_name).map(builtin_type_def_for)
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

/// Look up the type definition for a named receiver, unwrapping `ActorRef<T>`.
#[must_use]
pub fn lookup_type_def_for_receiver(
    type_defs: &HashMap<String, TypeDef>,
    receiver_ty: &Ty,
) -> Option<TypeDef> {
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
        name: type_name.to_string(),
        args: type_args.to_vec(),
    };
    let receiver_type_params = lookup_user_type_def(type_defs, type_name)
        .map(|type_def| type_def.type_params.clone())
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
        let method_name = sig_name.strip_prefix(&exact_prefix).or_else(|| {
            short_prefix
                .as_ref()
                .and_then(|prefix| sig_name.strip_prefix(prefix))
        });
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
#[must_use]
pub fn collect_method_sigs_for_receiver(
    type_defs: &HashMap<String, TypeDef>,
    fn_sigs: &HashMap<String, FnSig>,
    receiver_ty: &Ty,
) -> Vec<(String, FnSig)> {
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
                fields: HashMap::new(),
                variants: HashMap::new(),
                methods: HashMap::new(),
                doc_comment: None,
                is_indirect: false,
            },
        );

        let type_def = lookup_type_def(&type_defs, "stream.Stream")
            .expect("builtin stream type def should resolve");
        assert!(type_def.methods.contains_key("next"));
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
                fields: HashMap::new(),
                variants: HashMap::new(),
                methods: HashMap::new(),
                doc_comment: None,
                is_indirect: false,
            },
        );

        let mut fn_sigs = HashMap::new();
        fn_sigs.insert(
            "Wrapper::value".to_string(),
            FnSig {
                param_names: vec!["next".to_string()],
                params: vec![Ty::Named {
                    name: "T".to_string(),
                    args: vec![],
                }],
                return_type: Ty::Named {
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
                is_indirect: false,
            },
        );

        let type_def = lookup_type_def(&type_defs, "Receiver")
            .expect("builtin receiver type def should resolve");
        assert_eq!(type_def.type_params, vec!["T".to_string()]);
        assert_eq!(
            type_def.methods["recv"].return_type,
            Ty::option(Ty::Named {
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
                name: "Vec".to_string(),
                args: vec![Ty::String],
            },
            Ty::Named {
                name: "HashMap".to_string(),
                args: vec![Ty::String, Ty::I64],
            },
            Ty::Named {
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
                name: "HashSet".to_string(),
                args: vec![Ty::String],
            }
        );
    }
}
