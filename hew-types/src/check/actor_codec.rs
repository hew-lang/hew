//! Portable actor payloads are selected from checked declaration identities.

use super::{Checker, TypeDefKind};
use crate::actor_protocol::{ActorCodecField, ActorCodecPlan, ActorCodecValue};
use crate::ResolvedTy;

impl Checker {
    pub(super) fn select_actor_codecs(&mut self) {
        let mut protocols = std::mem::take(&mut self.actor_protocol_descriptors);
        for protocol in protocols.values_mut() {
            for handler in &mut protocol.handlers {
                let params = handler
                    .param_tys
                    .iter()
                    .map(|ty| self.actor_codec_value(ty, &mut Vec::new()))
                    .collect::<Option<Vec<_>>>();
                let reply = if handler.return_ty == ResolvedTy::Unit {
                    Some(None)
                } else {
                    self.actor_codec_value(&handler.return_ty, &mut Vec::new())
                        .map(Some)
                };
                handler.codec = params
                    .zip(reply)
                    .map(|(params, reply)| ActorCodecPlan { params, reply });
            }
        }
        self.actor_protocol_descriptors = protocols;
    }

    fn actor_codec_value(
        &self,
        ty: &ResolvedTy,
        visiting: &mut Vec<ResolvedTy>,
    ) -> Option<ActorCodecValue> {
        if !self.registry.is_serializable(&ty.to_ty()) || visiting.contains(ty) {
            return None;
        }
        let fields = match ty {
            ResolvedTy::I8
            | ResolvedTy::I16
            | ResolvedTy::I32
            | ResolvedTy::I64
            | ResolvedTy::U8
            | ResolvedTy::U16
            | ResolvedTy::U32
            | ResolvedTy::U64
            | ResolvedTy::Isize
            | ResolvedTy::Usize
            | ResolvedTy::F32
            | ResolvedTy::F64
            | ResolvedTy::Bool
            | ResolvedTy::Char
            | ResolvedTy::Duration
            | ResolvedTy::String
            | ResolvedTy::Bytes => None,
            ResolvedTy::Named {
                name,
                builtin: None,
                args,
                is_opaque: false,
            } if args.is_empty() => {
                let declaration = self.type_defs.get(name)?;
                if declaration.kind != TypeDefKind::Struct {
                    return None;
                }
                visiting.push(ty.clone());
                let fields = declaration
                    .field_order
                    .iter()
                    .map(|name| {
                        let field = ResolvedTy::from_ty(declaration.fields.get(name)?).ok()?;
                        // Wire metadata owns portable tags. Keep plain-record field
                        // identities without inventing a wire encoding.
                        let tag = if let Some(wire) = self.wire_layouts.get(&declaration.name) {
                            Some(wire.fields.iter().find(|field| field.name == *name)?.tag)
                        } else {
                            None
                        };
                        Some(ActorCodecField {
                            name: name.clone(),
                            tag,
                            value: self.actor_codec_value(&field, visiting)?,
                        })
                    })
                    .collect::<Option<Vec<_>>>();
                visiting.pop();
                Some(fields?)
            }
            _ => return None,
        };
        Some(ActorCodecValue {
            ty: ty.clone(),
            fields,
        })
    }
}
