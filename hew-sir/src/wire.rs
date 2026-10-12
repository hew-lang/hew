//! Codec plans: checked data shapes bound to semantic storage layouts.

use crate::{AggregateShapeId, VariantShapeId};
pub use hew_types::data_plan::{
    DataMember as SemWireMember, DataPayload as SemWirePayload, DataTable as SemWireTable,
};
use hew_types::ResolvedTy;
use std::collections::BTreeMap;
pub type SemWirePlan = hew_types::data_plan::DataPlan<AggregateShapeId, VariantShapeId>;
pub type SemWireKind = hew_types::data_plan::DataKind<AggregateShapeId, VariantShapeId>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SemWirePlans {
    pub property_input: bool,
    pub root: ResolvedTy,
    pub plans: BTreeMap<ResolvedTy, SemWirePlan>,
}

impl SemWirePlans {
    /// Project the checked codec walk into the shared generator and VM vocabulary.
    ///
    /// # Errors
    /// Refuses missing child plans, unsupported scalars or oversized arrays.
    pub fn property_shape(&self) -> Result<hew_codec::property::Shape, String> {
        hew_types::data_plan::property_shape(
            &self.root,
            &self.plans,
            |shape| Some(shape.0),
            |shape| Some(shape.0),
        )
    }

    #[must_use]
    pub fn get(&self, ty: &ResolvedTy) -> Option<&SemWirePlan> {
        self.plans.get(ty)
    }

    /// Verify each storage binding against its checked data plan.
    pub(crate) fn verify(
        &self,
        records: &[crate::SemAggregateShape],
        enums: &[crate::SemVariantShape],
    ) -> Result<(), String> {
        let bad = |plan: &SemWirePlan| {
            format!(
                "codec plan for `{}` disagrees with its checked shape",
                plan.ty.user_facing()
            )
        };
        if !self.plans.contains_key(&self.root) {
            return Err("codec plan set lacks its root".into());
        }
        for (ty, plan) in &self.plans {
            if *ty != plan.ty
                || plan
                    .children()
                    .iter()
                    .any(|child| !self.plans.contains_key(child))
            {
                return Err(bad(plan));
            }
            match &plan.kind {
                SemWireKind::Record {
                    shape,
                    table,
                    fields,
                } => {
                    let shape = records
                        .get(shape.0 as usize)
                        .filter(|shape| shape.aggregate_ty == plan.ty)
                        .ok_or_else(|| bad(plan))?;
                    if shape.fields.len() != fields.len()
                        || shape
                            .fields
                            .iter()
                            .zip(fields)
                            .any(|(field, ty)| field.ty != *ty)
                        || table
                            .as_ref()
                            .is_some_and(|table| table.members.len() != fields.len())
                    {
                        return Err(bad(plan));
                    }
                }
                SemWireKind::Enum {
                    shape,
                    table,
                    variants,
                } => {
                    let shape = enums
                        .get(shape.0 as usize)
                        .filter(|shape| shape.enum_ty == plan.ty)
                        .ok_or_else(|| bad(plan))?;
                    if shape.variants.len() != variants.len()
                        || table.members.len() != variants.len()
                        || shape
                            .variants
                            .iter()
                            .zip(variants)
                            .any(|(variant, payload)| {
                                variant.fields.len() != payload.types().len()
                                    || variant
                                        .fields
                                        .iter()
                                        .zip(payload.types())
                                        .any(|(field, ty)| field.ty != *ty)
                            })
                    {
                        return Err(bad(plan));
                    }
                }
                SemWireKind::Option {
                    shape,
                    none,
                    some,
                    value,
                } => {
                    let shape = enums
                        .get(shape.0 as usize)
                        .filter(|shape| shape.enum_ty == plan.ty)
                        .ok_or_else(|| bad(plan))?;
                    if shape.variants.len() != 2
                        || none == some
                        || !shape
                            .variants
                            .get(*none as usize)
                            .is_some_and(|case| case.fields.is_empty())
                        || !shape.variants.get(*some as usize).is_some_and(|case| {
                            case.fields.len() == 1 && case.fields[0].ty == *value
                        })
                    {
                        return Err(bad(plan));
                    }
                }
                _ => {}
            }
        }
        Ok(())
    }

    /// Decoding a map or set invokes its selected key capabilities to build
    /// the collection and reject duplicate semantic keys.
    pub fn visit_decode_capabilities(
        &self,
        visit: &mut impl FnMut(&ResolvedTy, hew_types::ValueCapability),
    ) {
        for plan in self.plans.values() {
            if let SemWireKind::Set(key) | SemWireKind::Map { key, .. } = &plan.kind {
                for capability in [
                    hew_types::ValueCapability::Hash,
                    hew_types::ValueCapability::Eq,
                ] {
                    visit(key, capability);
                }
            }
        }
    }

    /// Visit every exact type the codec walks.
    pub fn visit_types(&self, visit: &mut impl FnMut(&ResolvedTy)) {
        self.plans.keys().for_each(visit);
    }
}
/// Declaration-order Result cases selected for a decode's source result, and
/// the `wire.DecodeError` type a failure decodes from the reader's error.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SemWireDecodeResult {
    pub shape: VariantShapeId,
    pub ok: u32,
    pub error: u32,
    pub error_ty: ResolvedTy,
}
