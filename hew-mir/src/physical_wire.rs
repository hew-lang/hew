//! Verify the native storage selections made by a checked wire schema.

use std::collections::BTreeSet;

use hew_types::ResolvedTy;

use super::{PhysicalError, PhysicalModule, SemWireKind, SemWirePlan};

#[expect(
    clippy::too_many_lines,
    reason = "the exhaustive schema check validates each physical field selection before codegen"
)]
pub(super) fn verify_wire_plan(
    module: &PhysicalModule,
    plan: &SemWirePlan,
) -> Result<(), PhysicalError> {
    let mismatch = || PhysicalError::new("wire schema selects a different physical value shape");
    let child = |expected: &ResolvedTy, child: &SemWirePlan| -> Result<(), PhysicalError> {
        if expected != &child.ty {
            return Err(mismatch());
        }
        verify_wire_plan(module, child)
    };
    match &plan.kind {
        SemWireKind::Scalar => {
            if !plan.ty.is_integer()
                && !plan.ty.is_float()
                && !matches!(
                    plan.ty,
                    ResolvedTy::Bool
                        | ResolvedTy::Char
                        | ResolvedTy::Duration
                        | ResolvedTy::String
                        | ResolvedTy::Bytes
                )
            {
                return Err(mismatch());
            }
        }
        SemWireKind::Vector(value) => {
            let glue = module
                .vector_glue
                .iter()
                .find(|glue| glue.ty == plan.ty)
                .ok_or_else(mismatch)?;
            child(&glue.element.ty, value)?;
        }
        SemWireKind::Set(value) => {
            let glue = module
                .set_glue
                .iter()
                .find(|glue| glue.ty == plan.ty)
                .ok_or_else(mismatch)?;
            child(&glue.element.ty, value)?;
        }
        SemWireKind::Map { key, value } => {
            let glue = module
                .map_glue
                .iter()
                .find(|glue| glue.ty == plan.ty)
                .ok_or_else(mismatch)?;
            child(&glue.key.ty, key)?;
            child(&glue.value.ty, value)?;
        }
        SemWireKind::Option {
            none, some, value, ..
        } => {
            let glue = module
                .variant_glue
                .iter()
                .find(|glue| glue.ty == plan.ty)
                .ok_or_else(mismatch)?;
            if none == some
                || glue.variants.len() != 2
                || !glue
                    .variants
                    .get(*none as usize)
                    .is_some_and(|case| case.fields.is_empty())
            {
                return Err(mismatch());
            }
            let fields = &glue
                .variants
                .get(*some as usize)
                .ok_or_else(mismatch)?
                .fields;
            let [field] = fields.as_slice() else {
                return Err(mismatch());
            };
            child(&field.ty, value)?;
        }
        SemWireKind::Record { fields, .. } => {
            let glue = module
                .aggregate_glue
                .iter()
                .find(|glue| glue.ty == plan.ty)
                .ok_or_else(mismatch)?;
            if fields.len() != glue.fields.len() {
                return Err(mismatch());
            }
            let mut selected = BTreeSet::new();
            for field in fields {
                if !selected.insert(field.index) {
                    return Err(mismatch());
                }
                let recipe = glue.fields.get(field.index as usize).ok_or_else(mismatch)?;
                child(&recipe.ty, &field.value)?;
            }
        }
        SemWireKind::Enum { variants, .. } => {
            let glue = module
                .variant_glue
                .iter()
                .find(|glue| glue.ty == plan.ty)
                .ok_or_else(mismatch)?;
            if variants.len() != glue.variants.len() {
                return Err(mismatch());
            }
            let mut selected = BTreeSet::new();
            for variant in variants {
                if !selected.insert(variant.index) {
                    return Err(mismatch());
                }
                let fields = &glue
                    .variants
                    .get(variant.index as usize)
                    .ok_or_else(mismatch)?
                    .fields;
                if fields.len() != variant.fields.len() {
                    return Err(mismatch());
                }
                for (field, plan) in fields.iter().zip(&variant.fields) {
                    child(&field.ty, plan)?;
                }
            }
        }
    }
    Ok(())
}
