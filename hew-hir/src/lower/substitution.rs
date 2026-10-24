//! Type substitution for checker-owned binders.

use super::*;

/// Substitute declaration-owned parameter heads throughout a type spine.
/// Checked trait defaults and dispatch preserve nominal identities while
/// binding their receiver parameters.
#[must_use]
#[allow(
    clippy::match_same_arms,
    reason = "DI-019: the abstract-parameter `TypeParam` case is listed \
              explicitly (totality over the new variant) even though an \
              un-substituted parameter clones identically to the leaf \
              fall-through"
)]
pub fn substitute_ty<S: std::hash::BuildHasher>(
    ty: &ResolvedTy,
    subst: &HashMap<hew_types::ParamHead, ResolvedTy, S>,
) -> ResolvedTy {
    match ty {
        // Only a binder is substituted: a nominal of the same spelling is a
        // different type.
        ResolvedTy::Named {
            head: hew_types::TypeHead::Param(parameter),
            args,
            ..
        } if args.is_empty() && subst.contains_key(parameter) => subst[parameter].clone(),
        ResolvedTy::Named {
            head,
            args,
            is_opaque,
        } => ResolvedTy::Named {
            head: *head,
            args: args.iter().map(|a| substitute_ty(a, subst)).collect(),
            is_opaque: *is_opaque,
        },
        ResolvedTy::Tuple(items) => {
            ResolvedTy::Tuple(items.iter().map(|t| substitute_ty(t, subst)).collect())
        }
        ResolvedTy::Array(elem, n) => ResolvedTy::Array(Box::new(substitute_ty(elem, subst)), *n),
        ResolvedTy::Slice(elem) => ResolvedTy::Slice(Box::new(substitute_ty(elem, subst))),
        ResolvedTy::Function {
            capabilities,
            params,
            ret,
        } => ResolvedTy::Function {
            capabilities: *capabilities,
            params: params
                .iter()
                .map(|parameter| parameter.map(|ty| substitute_ty(ty, subst)))
                .collect(),
            ret: Box::new(substitute_ty(ret, subst)),
        },
        ResolvedTy::Closure {
            capabilities,
            params,
            ret,
            captures,
        } => ResolvedTy::Closure {
            capabilities: *capabilities,
            params: params
                .iter()
                .map(|parameter| parameter.map(|ty| substitute_ty(ty, subst)))
                .collect(),
            ret: Box::new(substitute_ty(ret, subst)),
            captures: captures.iter().map(|c| substitute_ty(c, subst)).collect(),
        },
        ResolvedTy::Pointer {
            is_mutable,
            pointee,
        } => ResolvedTy::Pointer {
            is_mutable: *is_mutable,
            pointee: Box::new(substitute_ty(pointee, subst)),
        },
        ResolvedTy::Task(inner) => ResolvedTy::Task(Box::new(substitute_ty(inner, subst))),
        // Borrow and trait-object are composite type constructors: their
        // payloads must be descended so a nested `TypeParam` is substituted
        // (DI-019 — no wildcard absorbs a composite). Mirrors the scope-aware
        // converter in `hew_types::ResolvedTy::from_ty_scoped`.
        ResolvedTy::Borrow { pointee } => ResolvedTy::Borrow {
            pointee: Box::new(substitute_ty(pointee, subst)),
        },
        ResolvedTy::TraitObject { traits } => ResolvedTy::TraitObject {
            traits: traits
                .iter()
                .map(|bound| hew_types::ResolvedTraitBound {
                    trait_name: bound.trait_name.clone(),
                    trait_id: bound.trait_id,
                    args: bound.args.iter().map(|a| substitute_ty(a, subst)).collect(),
                    assoc_bindings: bound
                        .assoc_bindings
                        .iter()
                        .map(|(name, t)| (name.clone(), substitute_ty(t, subst)))
                        .collect(),
                })
                .collect(),
        },
        // A structural abstract parameter (A622). Substitute it when the map
        // carries its name; otherwise keep it abstract.
        ResolvedTy::TypeParam { name } if subst.contains_key(name) => subst[name].clone(),
        ResolvedTy::TypeParam { .. } => ty.clone(),
        _ => ty.clone(),
    }
}

/// Whether a type still contains a declaration-owned generic parameter.
pub(super) fn contains_abstract_symbol(ty: &ResolvedTy) -> bool {
    match ty {
        ResolvedTy::Named { head, args, .. } => {
            if head.is_param() {
                return true;
            }
            args.iter().any(contains_abstract_symbol)
        }
        ResolvedTy::Tuple(items) => items.iter().any(contains_abstract_symbol),
        ResolvedTy::Array(elem, _) | ResolvedTy::Slice(elem) => contains_abstract_symbol(elem),
        ResolvedTy::Function { params, ret, .. } => {
            params
                .iter()
                .any(|parameter| contains_abstract_symbol(&parameter.ty))
                || contains_abstract_symbol(ret)
        }
        ResolvedTy::Closure {
            params,
            ret,
            captures,
            ..
        } => {
            params
                .iter()
                .any(|parameter| contains_abstract_symbol(&parameter.ty))
                || contains_abstract_symbol(ret)
                || captures.iter().any(contains_abstract_symbol)
        }
        ResolvedTy::Pointer { pointee, .. } | ResolvedTy::Borrow { pointee } => {
            contains_abstract_symbol(pointee)
        }
        ResolvedTy::TraitObject { traits } => traits.iter().any(|bound| {
            bound.args.iter().any(contains_abstract_symbol)
                || bound
                    .assoc_bindings
                    .iter()
                    .any(|(_, t)| contains_abstract_symbol(t))
        }),
        ResolvedTy::Task(inner) => contains_abstract_symbol(inner),
        // A structural type parameter is abstract by construction.
        ResolvedTy::TypeParam { .. } => true,
        _ => false,
    }
}
