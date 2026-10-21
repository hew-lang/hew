//! Internal variant slots retain physical value semantics while the checker
//! explicitly supplies their omitted initialization.
use std::collections::HashMap;

use super::{
    Checker, TypeDef, VariantConstructorField, VariantConstructorPlan, VariantConstructorValue,
    VariantDef, VariantMatch,
};
use crate::{SpanKey, Ty};
use hew_parser::ast::{
    Expr, Ident, ImplicitVariantField, ImplicitVariantFieldInitialization, Span, Spanned,
    TypeBodyItem, TypeDecl,
};

impl Checker {
    pub(super) fn implicit_variant_fields(
        td: &TypeDecl,
    ) -> HashMap<String, Vec<ImplicitVariantField>> {
        td.body
            .iter()
            .filter_map(|item| match item {
                TypeBodyItem::Variant(variant) if !variant.implicit_fields.is_empty() => {
                    Some((variant.name.to_string(), variant.implicit_fields.clone()))
                }
                _ => None,
            })
            .collect()
    }

    pub(super) fn variant_is_source_unit(td: &TypeDef, name: &str) -> bool {
        match td.variants.get(name) {
            Some(VariantDef::Unit) => true,
            Some(VariantDef::Struct(fields)) => {
                td.implicit_variant_fields
                    .get(name)
                    .is_some_and(|implicit| {
                        fields.iter().enumerate().all(|(index, (_, ty))| {
                            implicit.iter().any(|field| field.field_index == index)
                                && ty.as_option().is_some()
                        })
                    })
            }
            _ => false,
        }
    }

    pub(super) fn is_implicit_variant_field(
        &self,
        owner: &str,
        variant: &str,
        index: usize,
    ) -> bool {
        self.type_def_at(owner).is_some_and(|td| {
            matches!(td.variants.get(variant), Some(VariantDef::Struct(fields))
                if fields.get(index).is_some_and(|(_, ty)| ty.as_option().is_some()))
                && td
                    .implicit_variant_fields
                    .get(variant)
                    .is_some_and(|fields| fields.iter().any(|field| field.field_index == index))
        })
    }

    pub(super) fn record_variant_constructor_plan(
        &mut self,
        owner: &str,
        variant: &str,
        ty: &Ty,
        source: &[(Ident, Spanned<Expr>)],
        span: &Span,
    ) {
        let Some(td) = self.type_def_at(owner) else {
            return;
        };
        let Some(implicit) = td.implicit_variant_fields.get(variant) else {
            return;
        };
        let Some(VariantDef::Struct(fields)) = td.variants.get(variant) else {
            return;
        };
        let args = match ty {
            Ty::Named { args, .. } => args.as_slice(),
            _ => &[],
        };
        let substitutions: HashMap<_, _> = td
            .type_params
            .iter()
            .copied()
            .zip(args.iter().cloned())
            .collect();
        let mut planned = Vec::with_capacity(fields.len());
        for (index, (name, ty)) in fields.iter().enumerate() {
            let ty = ty.substitute_type_params_parallel(&substitutions);
            let value = if let Some(source_index) = source
                .iter()
                .position(|(field, _)| field.name.as_str() == name)
            {
                VariantConstructorValue::Source(source_index)
            } else if implicit.iter().any(|field| {
                field.field_index == index
                    && field.initialization == ImplicitVariantFieldInitialization::EmptyOption
            }) && ty.as_option().is_some()
            {
                VariantConstructorValue::EmptyOption
            } else {
                return;
            };
            planned.push(VariantConstructorField {
                name: name.clone(),
                ty,
                value,
            });
        }
        self.variant_constructor_plans.insert(
            SpanKey::in_module(span, self.current_module_idx),
            VariantConstructorPlan {
                variant: VariantMatch {
                    type_name: owner.to_string(),
                    variant_name: variant.to_string(),
                },
                fields: planned,
            },
        );
    }
}
