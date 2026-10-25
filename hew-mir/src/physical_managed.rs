//! Checked storage plans from explicit physical construction and SIR layouts.

use super::*;

impl FunctionLowerer<'_> {
    pub(crate) fn managed_acquisition(
        &self,
        operations: &[PhysicalOp],
    ) -> Result<PhysicalMaterialization, PhysicalError> {
        let [operation] = operations else {
            return Err(PhysicalError::new(
                "managed construction needs one operation",
            ));
        };
        let width = |ty: &ResolvedTy| {
            hew_sir::managed_slot(self.module, ty)
                .map(|slot| slot.bytes)
                .map_err(|error| PhysicalError::new(format!("managed slot: {error:?}")))
        };
        let ty = |id: StorageId| &self.storage[id.0 as usize].ty;
        let input = |id: StorageId, copies, consume| PhysicalManagedInput {
            source: PhysicalManagedSource::Value(id),
            ty: ty(id).clone(),
            copies,
            consume,
        };
        let moved = |ids: &[StorageId]| {
            ids.iter()
                .filter(|id| self.storage[id.0 as usize].own == OwnKind::Owned)
                .map(|id| input(*id, 0, true))
                .collect::<Vec<_>>()
        };
        let (result, fresh_bytes, bytes_literal_length, inputs, start_task) = match operation {
            PhysicalOp::Clone { dest, source, .. } => {
                (*dest, Some(0), None, vec![input(*source, 1, false)], false)
            }
            PhysicalOp::AggregateProjectCopy {
                dest,
                aggregate,
                field,
                ..
            }
            | PhysicalOp::TupleGet {
                dest,
                tuple: aggregate,
                index: field,
            } => (
                *dest,
                Some(0),
                None,
                vec![PhysicalManagedInput {
                    source: PhysicalManagedSource::AggregateField {
                        source: *aggregate,
                        field: *field,
                    },
                    ty: ty(*dest).clone(),
                    copies: 1,
                    consume: false,
                }],
                false,
            ),
            PhysicalOp::VariantProjectCopy {
                dest,
                source,
                variant,
                field,
                glue,
                ..
            } => (
                *dest,
                Some(0),
                None,
                vec![PhysicalManagedInput {
                    source: PhysicalManagedSource::VariantField {
                        source: *source,
                        variant: *variant,
                        field: *field,
                        glue: *glue,
                    },
                    ty: ty(*dest).clone(),
                    copies: 1,
                    consume: false,
                }],
                false,
            ),
            PhysicalOp::Const {
                dest,
                value: PhysicalConst::String(literal),
            } => {
                let literal = self
                    .module
                    .string_literals
                    .get(literal)
                    .ok_or_else(|| PhysicalError::new("managed string literal is absent"))?;
                (
                    *dest,
                    u64::try_from(literal.len()).ok(),
                    None,
                    Vec::new(),
                    false,
                )
            }
            PhysicalOp::Const {
                dest,
                value: PhysicalConst::Bytes(literal),
            } => {
                let literal = self
                    .module
                    .bytes_literals
                    .get(literal)
                    .ok_or_else(|| PhysicalError::new("managed bytes literal is absent"))?;
                let length = u32::try_from(literal.len())
                    .map_err(|_| PhysicalError::new("bytes literal exceeds its native length"))?;
                (*dest, Some(0), Some(length), Vec::new(), false)
            }
            PhysicalOp::ClosureMake { dest, fields, .. } => {
                let bytes = fields.iter().try_fold(Some(0_u64), |total, field| {
                    let field_bytes = width(ty(*field))?;
                    Ok::<_, PhysicalError>(total.and_then(|sum| sum.checked_add(field_bytes)))
                })?;
                (*dest, bytes, None, moved(fields), false)
            }
            PhysicalOp::DynMake { dest, source, .. } => (
                *dest,
                Some(width(ty(*source))?),
                None,
                moved(&[*source]),
                false,
            ),
            PhysicalOp::VariantMake { dest, fields, .. } => {
                let shape = hew_sir::managed_storage_shape(self.module, ty(*dest))
                    .map_err(|error| PhysicalError::new(format!("managed variant: {error:?}")))?;
                let hew_sir::ManagedStorageShape::Variant {
                    indirect,
                    payload_bytes,
                    ..
                } = shape
                else {
                    return Err(PhysicalError::new(
                        "variant construction has no managed shape",
                    ));
                };
                (
                    *dest,
                    Some(if indirect { payload_bytes } else { 0 }),
                    None,
                    moved(fields),
                    false,
                )
            }
            PhysicalOp::ArrayMake { dest, fields, .. } => {
                let ResolvedTy::Array(element, length) = ty(*dest) else {
                    return Err(PhysicalError::new("array construction has no exact length"));
                };
                (
                    *dest,
                    width(element)?.checked_mul(*length),
                    None,
                    moved(fields),
                    false,
                )
            }
            PhysicalOp::ArrayRepeat { dest, seed, .. } => {
                let ResolvedTy::Array(element, length) = ty(*dest) else {
                    return Err(PhysicalError::new("array repeat has no exact length"));
                };
                let inputs = if *length == 0 {
                    Vec::new()
                } else {
                    vec![input(
                        *seed,
                        length - 1,
                        self.storage[seed.0 as usize].own == OwnKind::Owned,
                    )]
                };
                (
                    *dest,
                    width(element)?.checked_mul(*length),
                    None,
                    inputs,
                    false,
                )
            }
            PhysicalOp::GeneratorMake {
                dest,
                callable,
                yielded,
                returned,
                callable_slot_bytes,
                ..
            } => (
                *dest,
                callable_slot_bytes
                    .checked_add(yielded.managed_slot_bytes.max(returned.managed_slot_bytes)),
                None,
                moved(&[*callable]),
                false,
            ),
            PhysicalOp::TaskSpawn {
                dest,
                callable,
                output,
                callable_slot_bytes,
                ..
            } => (
                *dest,
                callable_slot_bytes
                    .checked_add(output.as_ref().map_or(0, |row| row.managed_slot_bytes)),
                None,
                moved(&[*callable]),
                true,
            ),
            PhysicalOp::TaskInvoke {
                dest,
                callee,
                output,
                callable_slot_bytes,
                ..
            } => {
                // Prepared arguments alias the invoking frame; the child moves
                // them only after publication. They are not new provider slots.
                let inputs = callee
                    .receiver()
                    .into_iter()
                    .filter_map(|receiver| match receiver {
                        ArgumentTransfer::Move(source) => Some(input(source, 0, true)),
                        _ => None,
                    })
                    .collect();
                (
                    *dest,
                    callable_slot_bytes
                        .checked_add(output.as_ref().map_or(0, |row| row.managed_slot_bytes)),
                    None,
                    inputs,
                    true,
                )
            }
            PhysicalOp::TaskRace {
                dest,
                members,
                output,
                member_slot_bytes,
                ..
            } => (
                *dest,
                u64::try_from(members.len())
                    .ok()
                    .and_then(|length| member_slot_bytes.checked_mul(length))
                    .and_then(|bytes| {
                        bytes.checked_add(output.as_ref().map_or(0, |row| row.managed_slot_bytes))
                    }),
                None,
                moved(members),
                true,
            ),
            _ => {
                return Err(PhysicalError::new(
                    "operation has no managed acquisition plan",
                ))
            }
        };
        Ok(PhysicalMaterialization {
            result,
            fresh_bytes,
            bytes_literal_length,
            inputs,
            start_task,
        })
    }
}
