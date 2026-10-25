//! Canonical payload widths for actor-owned managed storage.
//!
//! These are language accounting units, not ABI sizes. A stored handle is
//! eight bytes on every target; aggregates have no padding, and a variant
//! reserves a four-byte discriminator plus its largest payload. Allocator,
//! reference-count and lookup-table metadata never enters this calculation.
//! Primitive SSA locals do not allocate storage merely by having a width.

use std::collections::BTreeSet;

use hew_types::{BuiltinType, ResolvedTy, TypeHead};

use crate::SemModule;

/// The canonical width of one value slot within managed payload storage.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ManagedSlot {
    pub bytes: u64,
}

impl ManagedSlot {
    /// Charge actual payload capacity, never an estimated host-object size.
    pub fn capacity_bytes(self, capacity: u64) -> Result<u64, ManagedLayoutError> {
        self.bytes
            .checked_mul(capacity)
            .ok_or(ManagedLayoutError::Overflow)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ManagedLayoutError {
    Overflow,
    MissingShape(ResolvedTy),
    RecursiveInline(ResolvedTy),
}

/// Resolve widths from the same exact semantic shapes that own field access
/// and copy/drop traversal. Backends must consume the result, not recalculate
/// it using their target's pointer sizes or host object representations.
pub fn managed_slot(
    module: &SemModule,
    ty: &ResolvedTy,
) -> Result<ManagedSlot, ManagedLayoutError> {
    slot(module, ty, &mut BTreeSet::new()).map(|bytes| ManagedSlot { bytes })
}

fn sum_fields<'a>(
    module: &SemModule,
    fields: impl IntoIterator<Item = &'a ResolvedTy>,
    active: &mut BTreeSet<ResolvedTy>,
) -> Result<u64, ManagedLayoutError> {
    fields.into_iter().try_fold(0_u64, |total, ty| {
        total
            .checked_add(slot(module, ty, active)?)
            .ok_or(ManagedLayoutError::Overflow)
    })
}

fn slot(
    module: &SemModule,
    ty: &ResolvedTy,
    active: &mut BTreeSet<ResolvedTy>,
) -> Result<u64, ManagedLayoutError> {
    use ResolvedTy as T;
    match ty {
        T::Unit | T::Never => return Ok(0),
        T::Bool | T::I8 | T::U8 => return Ok(1),
        T::I16 | T::U16 => return Ok(2),
        T::Char | T::I32 | T::U32 | T::F32 => return Ok(4),
        T::I64 | T::U64 | T::Isize | T::Usize | T::F64 | T::Duration => return Ok(8),
        T::String
        | T::Bytes
        | T::Array(_, _)
        | T::Slice(_)
        | T::CancellationToken
        | T::Generator { .. }
        | T::Function { .. }
        | T::Closure { .. }
        | T::TraitObject { .. }
        | T::Task(_)
        | T::Pointer { .. }
        | T::Borrow { .. } => return Ok(8),
        T::GenericCallable { callable, .. } => return slot(module, callable, active),
        T::Named {
            head: TypeHead::Actor(_),
            ..
        } => return Ok(8),
        T::Named {
            head: TypeHead::Builtin(builtin),
            ..
        } if matches!(
            builtin,
            BuiltinType::Vec
                | BuiltinType::HashMap
                | BuiltinType::HashSet
                | BuiltinType::Rc
                | BuiltinType::Weak
                | BuiltinType::VecIter
                | BuiltinType::HashMapIter
                | BuiltinType::Task
                | BuiltinType::ActorCall
                | BuiltinType::Generator
                | BuiltinType::Stream
                | BuiltinType::Sink
                | BuiltinType::ActorHandle
                | BuiltinType::ActorFn
                | BuiltinType::CancellationToken
        ) =>
        {
            return Ok(8)
        }
        // An opaque value's slot is its handle. Its provider must describe
        // managed payload separately; this does not exempt that payload.
        T::Named {
            is_opaque: true, ..
        } => return Ok(8),
        _ => {}
    }
    if let Some(shape) = module.variant_shape_for_type(ty) {
        if shape.is_indirect {
            return Ok(8);
        }
    }
    if !active.insert(ty.clone()) {
        return Err(ManagedLayoutError::RecursiveInline(ty.clone()));
    }
    let result = match ty {
        T::Tuple(fields) => sum_fields(module, fields, active),
        _ => {
            if let Some(shape) = module.aggregate_shape_for_type(ty) {
                sum_fields(module, shape.fields.iter().map(|field| &field.ty), active)
            } else if let Some(shape) = module.variant_shape_for_type(ty) {
                variant_payload(module, shape, active)?
                    .checked_add(4)
                    .ok_or(ManagedLayoutError::Overflow)
            } else {
                Err(ManagedLayoutError::MissingShape(ty.clone()))
            }
        }
    };
    active.remove(ty);
    result
}

/// Fixed arrays own a sequence backing even as locals. Nested arrays occupy
/// handle slots and charge each distinct backing separately, like vectors.
pub fn managed_array_payload(
    module: &SemModule,
    ty: &ResolvedTy,
) -> Result<ManagedSlot, ManagedLayoutError> {
    let ResolvedTy::Array(element, count) = ty else {
        return Err(ManagedLayoutError::MissingShape(ty.clone()));
    };
    managed_slot(module, element)?
        .capacity_bytes(*count)
        .map(|bytes| ManagedSlot { bytes })
}

/// The fixed payload capacity of an indirect variant allocation. The
/// discriminator is included; the owning handle's slot is charged only when
/// it occupies another managed allocation.
pub fn managed_variant_payload(
    module: &SemModule,
    shape: &crate::SemVariantShape,
) -> Result<ManagedSlot, ManagedLayoutError> {
    let bytes = variant_payload(module, shape, &mut BTreeSet::new())?
        .checked_add(4)
        .ok_or(ManagedLayoutError::Overflow)?;
    Ok(ManagedSlot { bytes })
}

fn variant_payload(
    module: &SemModule,
    shape: &crate::SemVariantShape,
    active: &mut BTreeSet<ResolvedTy>,
) -> Result<u64, ManagedLayoutError> {
    shape.variants.iter().try_fold(0, |largest, variant| {
        sum_fields(module, variant.fields.iter().map(|field| &field.ty), active)
            .map(|bytes| largest.max(bytes))
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn payload_widths_ignore_target_padding_and_keep_integer_widths() {
        let module = SemModule::default();
        let row = ResolvedTy::Tuple(vec![ResolvedTy::Bool, ResolvedTy::U64, ResolvedTy::String]);
        let layout = managed_slot(&module, &row).unwrap();
        assert_eq!(layout.bytes, 17);
        assert_eq!(layout.capacity_bytes(4), Ok(68));
        assert_eq!(managed_slot(&module, &ResolvedTy::Usize).unwrap().bytes, 8);
    }

    #[test]
    fn nested_arrays_and_capacity_overflow_fail_closed() {
        let module = SemModule::default();
        let row = ResolvedTy::Array(Box::new(ResolvedTy::U64), u64::MAX);
        assert_eq!(
            managed_array_payload(&module, &row),
            Err(ManagedLayoutError::Overflow)
        );
        assert_eq!(
            ManagedSlot { bytes: 8 }.capacity_bytes(u64::MAX),
            Err(ManagedLayoutError::Overflow)
        );
        assert_eq!(ManagedSlot { bytes: 0 }.capacity_bytes(u64::MAX), Ok(0));
    }

    #[test]
    fn nested_arrays_charge_backings_without_inlining_them_twice() {
        let module = SemModule::default();
        let inner = ResolvedTy::Array(Box::new(ResolvedTy::I64), 3);
        let outer = ResolvedTy::Array(Box::new(inner.clone()), 2);
        assert_eq!(managed_slot(&module, &outer).unwrap().bytes, 8);
        assert_eq!(managed_array_payload(&module, &inner).unwrap().bytes, 24);
        assert_eq!(managed_array_payload(&module, &outer).unwrap().bytes, 16);
    }
}
