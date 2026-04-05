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

fn substitute_named_param(ty: &Ty, param_name: &str, replacement: &Ty) -> Ty {
    match ty {
        Ty::Named { name, args } if args.is_empty() && name == param_name => replacement.clone(),
        Ty::Named { name, args } => Ty::Named {
            name: name.clone(),
            args: args
                .iter()
                .map(|arg| substitute_named_param(arg, param_name, replacement))
                .collect(),
        },
        Ty::Tuple(elems) => Ty::Tuple(
            elems
                .iter()
                .map(|elem| substitute_named_param(elem, param_name, replacement))
                .collect(),
        ),
        Ty::Array(inner, len) => Ty::Array(
            Box::new(substitute_named_param(inner, param_name, replacement)),
            *len,
        ),
        Ty::Slice(inner) => Ty::Slice(Box::new(substitute_named_param(
            inner,
            param_name,
            replacement,
        ))),
        Ty::Function { params, ret } => Ty::Function {
            params: params
                .iter()
                .map(|param| substitute_named_param(param, param_name, replacement))
                .collect(),
            ret: Box::new(substitute_named_param(ret, param_name, replacement)),
        },
        Ty::Closure {
            params,
            ret,
            captures,
        } => Ty::Closure {
            params: params
                .iter()
                .map(|param| substitute_named_param(param, param_name, replacement))
                .collect(),
            ret: Box::new(substitute_named_param(ret, param_name, replacement)),
            captures: captures
                .iter()
                .map(|capture| substitute_named_param(capture, param_name, replacement))
                .collect(),
        },
        Ty::Pointer {
            is_mutable,
            pointee,
        } => Ty::Pointer {
            is_mutable: *is_mutable,
            pointee: Box::new(substitute_named_param(pointee, param_name, replacement)),
        },
        Ty::TraitObject { traits } => Ty::TraitObject {
            traits: traits
                .iter()
                .map(|bound| crate::TraitObjectBound {
                    trait_name: bound.trait_name.clone(),
                    args: bound
                        .args
                        .iter()
                        .map(|arg| substitute_named_param(arg, param_name, replacement))
                        .collect(),
                })
                .collect(),
        },
        _ => ty.clone(),
    }
}

fn instantiate_named_method_sig(mut sig: FnSig, type_params: &[String], type_args: &[Ty]) -> FnSig {
    for (type_param, type_arg) in type_params.iter().zip(type_args.iter()) {
        for param_ty in &mut sig.params {
            *param_ty = substitute_named_param(param_ty, type_param, type_arg);
        }
        sig.return_type = substitute_named_param(&sig.return_type, type_param, type_arg);
    }
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
    let row_ty = generic_ty("Row");
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
            (
                "decode",
                FnSig {
                    type_params: vec!["Row".to_string()],
                    return_type: Ty::stream(row_ty),
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
            (
                "encode",
                FnSig {
                    type_params: vec!["Row".to_string()],
                    return_type: Ty::sink(row_ty),
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
        type_def.methods.entry(method_name).or_insert(sig);
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
    lookup_named_method_sig(type_defs, fn_sigs, type_name, type_args, method)
        .or_else(|| lookup_builtin_method_sig(receiver_ty, method))
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
    let receiver_type_params = lookup_user_type_def(type_defs, type_name)
        .map(|type_def| type_def.type_params.clone())
        .unwrap_or_default();

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
}
