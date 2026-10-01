//! Verify the native storage selections made by a checked wire schema.

use hew_types::ResolvedTy;

use super::{PhysicalError, PhysicalModule, SemWireKind, SemWirePlans};

/// Every plan in the set selects the physical value shape its type has.
#[expect(
    clippy::too_many_lines,
    reason = "the exhaustive plan check validates each physical selection before codegen"
)]
pub(super) fn verify_wire_plan(
    module: &PhysicalModule,
    plans: &SemWirePlans,
) -> Result<(), PhysicalError> {
    let mismatch = || PhysicalError::new("codec plan selects a different physical value shape");
    let same = |expected: &[ResolvedTy], planned: &[ResolvedTy]| {
        if expected == planned {
            Ok(())
        } else {
            Err(mismatch())
        }
    };
    for plan in plans.plans.values() {
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
            SemWireKind::Unit => {
                if plan.ty != ResolvedTy::Unit {
                    return Err(mismatch());
                }
            }
            SemWireKind::Tuple(elements) => {
                let ResolvedTy::Tuple(expected) = &plan.ty else {
                    return Err(mismatch());
                };
                same(expected, elements)?;
            }
            SemWireKind::Array { element, len } => {
                let ResolvedTy::Array(expected, expected_len) = &plan.ty else {
                    return Err(mismatch());
                };
                if **expected != *element || expected_len != len {
                    return Err(mismatch());
                }
            }
            SemWireKind::Vector(value) => {
                let glue = module
                    .vector_glue
                    .iter()
                    .find(|glue| glue.ty == plan.ty)
                    .ok_or_else(mismatch)?;
                same(
                    std::slice::from_ref(&glue.element.ty),
                    std::slice::from_ref(value),
                )?;
            }
            SemWireKind::Set(value) => {
                let glue = module
                    .set_glue
                    .iter()
                    .find(|glue| glue.ty == plan.ty)
                    .ok_or_else(mismatch)?;
                same(
                    std::slice::from_ref(&glue.element.ty),
                    std::slice::from_ref(value),
                )?;
            }
            SemWireKind::Map { key, value } => {
                let glue = module
                    .map_glue
                    .iter()
                    .find(|glue| glue.ty == plan.ty)
                    .ok_or_else(mismatch)?;
                same(
                    &[glue.key.ty.clone(), glue.value.ty.clone()],
                    &[key.clone(), value.clone()],
                )?;
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
                same(std::slice::from_ref(&field.ty), std::slice::from_ref(value))?;
            }
            SemWireKind::Record { fields, .. } => {
                let glue = module
                    .aggregate_glue
                    .iter()
                    .find(|glue| glue.ty == plan.ty)
                    .ok_or_else(mismatch)?;
                let expected = glue
                    .fields
                    .iter()
                    .map(|field| field.ty.clone())
                    .collect::<Vec<_>>();
                same(&expected, fields)?;
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
                for (case, payload) in glue.variants.iter().zip(variants) {
                    let expected = case
                        .fields
                        .iter()
                        .map(|field| field.ty.clone())
                        .collect::<Vec<_>>();
                    same(&expected, payload.types())?;
                }
            }
        }
    }
    Ok(())
}

/// Verify each portable protocol against its selected handler signature.
pub(super) fn verify_actor_codecs(module: &PhysicalModule) -> Result<(), PhysicalError> {
    for actor in &module.actors {
        for handler in &actor.handlers {
            if let Some(codec) = &handler.codec {
                if codec.params.len() != handler.params.len()
                    || codec
                        .params
                        .iter()
                        .zip(&handler.params)
                        .any(|(plan, ty)| &plan.root != ty)
                    || codec.reply.as_ref().map(|plan| &plan.root)
                        != (handler.return_ty != ResolvedTy::Unit).then_some(&handler.return_ty)
                {
                    return Err(PhysicalError::new(
                        "actor codec differs from its handler signature",
                    ));
                }
                for plan in codec.params.iter().chain(codec.reply.iter()) {
                    verify_wire_plan(module, plan)?;
                }
            }
        }
    }
    Ok(())
}
