//! Checker-side equality eligibility for layout-backed aggregate elements.
//!
//! This predicate is intentionally conservative: anything that would require
//! ownership-aware or heap-handle equality is rejected before a later slice can
//! route layout-backed `Vec::contains` through generated thunks.
//!
//! Floating-point fields (`f32`/`f64`) ARE eligible. Their structural equality
//! uses bitwise/total semantics — the codegen eq thunk bit-casts each float to
//! an integer of the same width and compares the bit patterns, so two NaN
//! values with identical bits compare equal and `+0.0` is distinct from `-0.0`.
//! This is reflexive (`x == x` for every `x`, including NaN), which is the
//! property structural equality needs to drive dedup, `contains`, and
//! `HashMap` keys correctly. It is deliberately different from the IEEE numeric `==` a
//! bare `f64 == f64` expression lowers to (where `NaN != NaN`).
//!
//! This gate belongs to `Vec.contains`. Ordinary comparisons and generic Eq
//! demands use `TypeFactService` selected capabilities instead.

use crate::check::{TypeDef, TypeDefKind, TypeDefView, VariantDef};
use crate::ty::Ty;
use crate::BuiltinType;
use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum EqEligibility {
    Eligible,
    IneligibleManaged(Ty),
    IneligibleOwned(Ty),
    IneligibleUnknown,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct EqEligibilityFailure {
    pub reason: EqEligibility,
    pub member: String,
}

impl EqEligibilityFailure {
    fn at_member(mut self, member: impl Into<String>) -> Self {
        let member = member.into();
        self.member = if self.member.is_empty() {
            member
        } else {
            format!("{member}.{}", self.member)
        };
        self
    }
}

#[derive(Default)]
struct EqWalk {
    completed: HashMap<Ty, Option<EqEligibilityFailure>>,
    active: HashSet<Ty>,
    path: Vec<Ty>,
}

fn contains_type(ty: &Ty, needle: &Ty) -> bool {
    let mut pending = vec![ty];
    while let Some(ty) = pending.pop() {
        if ty == needle {
            return true;
        }
        ty.for_each_child(&mut |child| pending.push(child));
    }
    false
}

/// Returns `Some(rejection)` if `ty` is not equality-eligible, `None` if eligible.
///
/// Only concrete elements are eligible at this layout gate.
fn eq_ineligibility(
    ty: &Ty,
    types: TypeDefView<'_>,
    walk: &mut EqWalk,
) -> Option<EqEligibilityFailure> {
    if let Some(completed) = walk.completed.get(ty) {
        return completed.clone();
    }
    let growing = matches!(ty, Ty::Named { head, args } if head.nominal().is_some() && !args.is_empty())
        && walk.path.iter().any(|previous| match (previous, ty) {
            (
                Ty::Named {
                    head: previous_head,
                    args: previous,
                },
                Ty::Named { head, args },
            ) => {
                previous_head == head
                    && previous.len() == args.len()
                    && previous != args
                    && previous
                        .iter()
                        .zip(args)
                        .all(|(previous, actual)| contains_type(actual, previous))
            }
            _ => false,
        });
    if growing || !walk.active.insert(ty.clone()) {
        return Some(EqEligibilityFailure {
            reason: EqEligibility::IneligibleUnknown,
            member: String::new(),
        });
    }
    walk.path.push(ty.clone());
    let result = eq_ineligibility_inner(ty, types, walk);
    walk.path.pop();
    walk.active.remove(ty);
    walk.completed.insert(ty.clone(), result.clone());
    result
}

fn eq_ineligibility_inner(
    ty: &Ty,
    types: TypeDefView<'_>,
    walk: &mut EqWalk,
) -> Option<EqEligibilityFailure> {
    match ty {
        Ty::I8
        | Ty::I16
        | Ty::I32
        | Ty::IntLiteral
        | Ty::U8
        | Ty::U16
        | Ty::U32
        | Ty::I64
        | Ty::U64
        | Ty::Isize
        | Ty::Usize
        | Ty::Bool
        | Ty::Char
        | Ty::Duration
        | Ty::Unit
        // Floats are equality-eligible: codegen compares their bit patterns
        // (bitwise/total semantics — reflexive, NaN==NaN iff identical bits).
        | Ty::F32
        | Ty::F64
        | Ty::FloatLiteral
        | Ty::String
        | Ty::Named { head: crate::TypeHead::Builtin(BuiltinType::NodeId | BuiltinType::Location | BuiltinType::RemotePid), .. } => None,
        Ty::Tuple(elems) => elems.iter().enumerate().find_map(|(index, elem)| {
            eq_ineligibility(elem, types, walk)
                .map(|failure| failure.at_member(index.to_string()))
        }),
        Ty::Named { head: crate::TypeHead::Builtin(BuiltinType::Option | BuiltinType::Result), args, .. } => args.iter().enumerate().find_map(|(index, arg)| {
            let member = if args.len() == 1 {
                "Some"
            } else if index == 0 {
                "Ok"
            } else {
                "Err"
            };
            eq_ineligibility(arg, types, walk)
                .map(|failure| failure.at_member(member))
        }),
        Ty::Named { head: crate::TypeHead::Builtin(BuiltinType::Vec), args, .. } if args.len() == 1 => eq_ineligibility(&args[0], types, walk)
            .map(|failure| failure.at_member("element")),
        Ty::Named { head: crate::TypeHead::Builtin(BuiltinType::HashMap
                    | BuiltinType::HashSet
                    | BuiltinType::Rc
                    | BuiltinType::Weak
                    | BuiltinType::JsonValue
                    | BuiltinType::YamlValue,), .. }
        | Ty::CancellationToken
        | Ty::Array(_, _)
        | Ty::Slice(_)
        | Ty::GenericCallable { .. }
            | Ty::Function { .. }
        | Ty::Closure { .. }
        | Ty::Pointer { .. }
        | Ty::Borrow { .. }
        | Ty::TraitObject { .. }
        | Ty::Task(_)
        | Ty::AssocType { .. } => Some(EqEligibilityFailure {
            reason: EqEligibility::IneligibleOwned(ty.clone()),
            member: String::new(),
        }),
        Ty::Named { head, args } => match types.of(*head) {
            Some(type_def) if type_def.is_indirect => {
                Some(EqEligibilityFailure {
                    reason: EqEligibility::IneligibleOwned(ty.clone()),
                    member: String::new(),
                })
            }
            Some(type_def) if type_def.kind == TypeDefKind::Enum => {
                walk_variants_for_eq_eligibility(type_def, args, types, walk)
            }
            Some(type_def) => {
                walk_instantiated_fields_for_eq_eligibility(type_def, args, types, walk)
            }
            None => Some(EqEligibilityFailure {
                reason: EqEligibility::IneligibleUnknown,
                member: String::new(),
            }),
        },
        Ty::Var(_) | Ty::Error | Ty::Never => {
            Some(EqEligibilityFailure {
                reason: EqEligibility::IneligibleUnknown,
                member: String::new(),
            })
        }
        Ty::Bytes => Some(EqEligibilityFailure {
            reason: EqEligibility::IneligibleManaged(ty.clone()),
            member: String::new(),
        }),
    }
}

fn instantiate_type_def_member(ty: &Ty, type_params: &[crate::ParamHead], type_args: &[Ty]) -> Ty {
    if type_params.is_empty() || type_params.len() != type_args.len() {
        return ty.clone();
    }
    let substitutions: HashMap<crate::ParamHead, Ty> = type_params
        .iter()
        .copied()
        .zip(type_args.iter().cloned())
        .collect();
    ty.substitute_type_params_parallel(&substitutions)
}

fn walk_instantiated_fields_for_eq_eligibility(
    type_def: &TypeDef,
    type_args: &[Ty],
    types: TypeDefView<'_>,
    walk: &mut EqWalk,
) -> Option<EqEligibilityFailure> {
    if type_args.is_empty() {
        let mut field_names: Vec<&String> = type_def.fields.keys().collect();
        field_names.sort();
        return field_names.into_iter().find_map(|name| {
            let field_ty = type_def.fields.get(name)?;
            eq_ineligibility(field_ty, types, walk).map(|failure| failure.at_member(name.clone()))
        });
    }
    let mut field_names: Vec<&String> = type_def.fields.keys().collect();
    field_names.sort();
    field_names.into_iter().find_map(|name| {
        let field_ty = type_def.fields.get(name)?;
        let field_ty = instantiate_type_def_member(field_ty, &type_def.type_params, type_args);
        eq_ineligibility(&field_ty, types, walk).map(|failure| failure.at_member(name.clone()))
    })
}

fn walk_variants_for_eq_eligibility(
    type_def: &TypeDef,
    type_args: &[Ty],
    types: TypeDefView<'_>,
    walk: &mut EqWalk,
) -> Option<EqEligibilityFailure> {
    let mut variant_names: Vec<&String> = type_def.variants.keys().collect();
    variant_names.sort();
    variant_names.into_iter().find_map(|name| {
        let variant = type_def.variants.get(name)?;
        match variant {
            VariantDef::Unit => None,
            VariantDef::Tuple(fields) => fields.iter().enumerate().find_map(|(index, field_ty)| {
                let field_ty =
                    instantiate_type_def_member(field_ty, &type_def.type_params, type_args);
                eq_ineligibility(&field_ty, types, walk)
                    .map(|failure| failure.at_member(format!("{name}.{index}")))
            }),
            VariantDef::Struct(fields) => fields.iter().find_map(|(field_name, field_ty)| {
                let field_ty =
                    instantiate_type_def_member(field_ty, &type_def.type_params, type_args);
                eq_ineligibility(&field_ty, types, walk)
                    .map(|failure| failure.at_member(format!("{name}.{field_name}")))
            }),
        }
    })
}

#[must_use]
pub(crate) fn ty_is_eq_eligible(ty: &Ty, types: TypeDefView<'_>) -> EqEligibility {
    ty_eq_ineligibility(ty, types).map_or(EqEligibility::Eligible, |failure| failure.reason)
}

#[must_use]
pub(crate) fn ty_eq_ineligibility(ty: &Ty, types: TypeDefView<'_>) -> Option<EqEligibilityFailure> {
    eq_ineligibility(ty, types, &mut EqWalk::default())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn checked(source: &str) -> crate::check::TypeCheckOutput {
        let parsed = hew_parser::parse(source);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let mut checker =
            crate::check::Checker::new(crate::module_registry::ModuleRegistry::new(vec![]));
        let output = checker.check_program(&parsed.program);
        assert!(output.errors.is_empty(), "{:?}", output.errors);
        output
    }

    fn named(output: &crate::check::TypeCheckOutput, name: &str, args: Vec<Ty>) -> Ty {
        Ty::named_head(
            crate::TypeHead::Nominal(crate::NominalHead::new(
                output
                    .defs
                    .lookup_nominal(name)
                    .expect("checked declaration"),
                name,
            )),
            args,
        )
    }

    #[test]
    fn equality_shared_vector_member_dag_preserves_leaf_failures() {
        for leaf in ["i64", "bytes"] {
            let mut source = format!("type EqDag0 {{ value: {leaf}; }}\n");
            for level in 1..=26 {
                let child = level - 1;
                source.push_str(&format!(
                    "type EqDag{level} {{ left: Vec<EqDag{child}>; right: Vec<EqDag{child}>; }}\n"
                ));
            }
            source.push_str("fn main() {}\n");
            let output = checked(&source);
            let ty = named(&output, "EqDag26", vec![]);
            let result = ty_eq_ineligibility(&ty, output.types());
            if leaf == "i64" {
                assert!(result.is_none(), "{result:?}");
            } else {
                let failure = result.expect("managed leaf must remain ineligible");
                assert_eq!(failure.reason, EqEligibility::IneligibleManaged(Ty::Bytes));
                assert!(failure.member.ends_with("element.value"), "{failure:?}");
            }
        }
    }

    #[test]
    fn equality_nested_generic_instances_do_not_share_results() {
        let output = checked("type EqHolder<T> { value: T; } fn main() {}");
        let safe = named(
            &output,
            "EqHolder",
            vec![named(&output, "EqHolder", vec![Ty::I64])],
        );
        let unsafe_value = named(
            &output,
            "EqHolder",
            vec![named(&output, "EqHolder", vec![Ty::Bytes])],
        );
        let failure = ty_eq_ineligibility(&Ty::Tuple(vec![safe, unsafe_value]), output.types())
            .expect("second instance is managed");
        assert_eq!(failure.reason, EqEligibility::IneligibleManaged(Ty::Bytes));
        assert_eq!(failure.member, "1.value.value");
    }

    #[test]
    fn equality_recursive_layout_gate_terminates_conservatively() {
        let output = checked("enum EqNode { Leaf(i64); Branch(Vec<EqNode>); } fn main() {}");
        let failure = ty_eq_ineligibility(&named(&output, "EqNode", vec![]), output.types())
            .expect("recursive layout gate remains unproven");
        assert_eq!(failure.reason, EqEligibility::IneligibleUnknown);
        assert!(!failure.member.is_empty());
    }
}
