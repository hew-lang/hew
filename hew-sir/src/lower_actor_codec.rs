//! Consume checked actor codec facts without deriving field or tag policy.

use super::InstanceService;
use crate::{AggregateShapeRef, SemWireField, SemWireKind, SemWirePlan};
use hew_types::actor_protocol::{ActorCodecPlan, ActorCodecValue};
use std::sync::Arc;

fn ready(value: &ActorCodecValue) -> bool {
    value.fields.as_ref().is_none_or(|fields| {
        fields
            .iter()
            .all(|field| field.tag.is_some() && ready(&field.value))
    })
}

impl InstanceService<'_> {
    pub(super) fn request_actor_codec_roots(&mut self) -> Result<(), String> {
        let roots = self
            .module
            .items
            .iter()
            .filter_map(|item| {
                let hew_hir::HirItem::Actor(actor) = item else {
                    return None;
                };
                if !actor.type_params.is_empty() || actor.lambda_handle_ty.is_some() {
                    return None;
                }
                actor
                    .protocol_descriptor
                    .as_ref()
                    .filter(|protocol| {
                        protocol.handlers.iter().any(|handler| {
                            handler.codec.as_ref().is_some_and(|codec| {
                                codec.params.iter().all(ready)
                                    && codec.reply.as_ref().is_none_or(ready)
                            })
                        })
                    })
                    .map(|_| {
                        hew_types::ResolvedTy::named_builtin(
                            actor.declaration.full_path(),
                            hew_types::BuiltinType::ActorHandle,
                            Vec::new(),
                        )
                    })
            })
            .collect::<Vec<_>>();
        for ty in roots {
            self.require_actor(&ty)?;
        }
        Ok(())
    }

    pub(super) fn resolve_actor_codec(
        &mut self,
        plan: &ActorCodecPlan,
    ) -> Result<Option<crate::actor::SemActorCodec>, String> {
        if !plan.params.iter().all(ready) || !plan.reply.as_ref().is_none_or(ready) {
            return Ok(None);
        }
        Ok(Some(crate::actor::SemActorCodec {
            params: plan
                .params
                .iter()
                .map(|value| self.actor_codec_value(value))
                .collect::<Result<_, _>>()?,
            reply: plan
                .reply
                .as_ref()
                .map(|value| self.actor_codec_value(value))
                .transpose()?,
        }))
    }

    fn actor_codec_value(&mut self, value: &ActorCodecValue) -> Result<Arc<SemWirePlan>, String> {
        self.require_type_facts(&value.ty)?;
        let kind = if let Some(fields) = &value.fields {
            let AggregateShapeRef::Record(shape) = self.require_aggregate_shape(&value.ty)? else {
                return Err("actor codec record has no exact SIR shape".into());
            };
            let selected = self.aggregate_shapes[shape.0 as usize].fields.clone();
            if fields.len() != selected.len() {
                return Err("actor codec field inventory differs".into());
            }
            let fields = fields
                .iter()
                .zip(selected)
                .enumerate()
                .map(|(index, (field, selected))| {
                    if selected.name != field.name || selected.ty != field.value.ty {
                        return Err("actor codec field differs from its checked identity".into());
                    }
                    Ok(SemWireField {
                        index: u32::try_from(index).map_err(|_| "actor field index exceeds u32")?,
                        tag: field.tag.ok_or("actor codec field lacks a portable tag")?,
                        json_name: field.name.clone(),
                        yaml_name: field.name.clone(),
                        presence: hew_types::WireFieldPresence::Required,
                        value: self.actor_codec_value(&field.value)?,
                    })
                })
                .collect::<Result<_, String>>()?;
            SemWireKind::Record { shape, fields }
        } else {
            SemWireKind::Scalar
        };
        let plan = Arc::new(SemWirePlan {
            ty: value.ty.clone(),
            kind,
        });
        plan.verify(&self.aggregate_shapes, &self.variant_shapes)?;
        Ok(plan)
    }
}
