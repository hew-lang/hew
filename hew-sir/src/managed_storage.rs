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
    MissingFacts(ResolvedTy),
    RecursiveInline(ResolvedTy),
}

/// Concrete storage families. Backends project these typed facts instead of
/// classifying type names or reconstructing ownership from native callbacks.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ManagedStorageShape {
    Scalar,
    Handle(ManagedHandleKind),
    String,
    Bytes,
    Vector {
        element: ResolvedTy,
        share_on_copy: bool,
    },
    Array {
        element: ResolvedTy,
        length: u64,
        share_on_copy: bool,
    },
    Slice {
        element: ResolvedTy,
        share_on_copy: bool,
    },
    Map {
        key: ResolvedTy,
        value: ResolvedTy,
    },
    Set {
        element: ResolvedTy,
    },
    Aggregate {
        fields: Vec<ResolvedTy>,
    },
    Variant {
        indirect: bool,
        payload_bytes: u64,
        cases: Vec<Vec<ResolvedTy>>,
    },
    /// `None` uses the callable's concrete environment descriptor. A function
    /// signature can carry a closure; it does not prove an empty environment.
    Callable {
        captures: Option<Vec<ResolvedTy>>,
    },
    Shared {
        payload: ResolvedTy,
        weak: bool,
    },
    Dynamic,
    Generator {
        yielded: ResolvedTy,
        returned: ResolvedTy,
        payload_bytes: u64,
    },
    Provider {
        kind: ManagedProviderKind,
        children: Vec<ResolvedTy>,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ManagedHandleKind {
    Actor,
    Pointer,
    Borrow,
    Cancellation,
}

/// Provider state is not charged by its Rust/JS representation. Its concrete
/// descriptor must expose retained typed Hew values; an opaque provider without
/// that description fails closed at a bounded ownership boundary.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ManagedProviderKind {
    Task,
    ActorCall,
    Stream,
    Sink,
    Encoding(BuiltinType),
    Opaque,
}

pub fn managed_storage_shape(
    module: &SemModule,
    ty: &ResolvedTy,
) -> Result<ManagedStorageShape, ManagedLayoutError> {
    use ManagedStorageShape as S;
    use ResolvedTy as T;
    let ty = ty.callable_body();
    let shape = match ty {
        T::Unit
        | T::Never
        | T::Bool
        | T::I8
        | T::U8
        | T::I16
        | T::U16
        | T::Char
        | T::I32
        | T::U32
        | T::F32
        | T::I64
        | T::U64
        | T::Isize
        | T::Usize
        | T::F64
        | T::Duration => S::Scalar,
        T::String => S::String,
        T::Bytes => S::Bytes,
        T::Pointer { .. } => S::Handle(ManagedHandleKind::Pointer),
        T::Borrow { .. } => S::Handle(ManagedHandleKind::Borrow),
        T::CancellationToken => S::Handle(ManagedHandleKind::Cancellation),
        T::Array(element, length) => S::Array {
            element: element.as_ref().clone(),
            length: *length,
            share_on_copy: managed_slot_is_plain(module, element)?,
        },
        T::Slice(element) => S::Slice {
            element: element.as_ref().clone(),
            share_on_copy: managed_slot_is_plain(module, element)?,
        },
        T::Closure { captures, .. } => S::Callable {
            captures: Some(captures.clone()),
        },
        T::Function { .. } => S::Callable { captures: None },
        T::TraitObject { .. } => S::Dynamic,
        T::Task(result) => S::Provider {
            kind: ManagedProviderKind::Task,
            children: vec![result.as_ref().clone()],
        },
        _ => {
            if let Some((yielded, returned)) = ty.generator_parts() {
                S::Generator {
                    yielded: yielded.clone(),
                    returned: returned.clone(),
                    payload_bytes: managed_generator_payload(module, ty)?.bytes,
                }
            } else if let Some((builtin, args)) =
                hew_types::runtime_call::collection_type_arguments(ty)
            {
                match builtin {
                    BuiltinType::Vec => S::Vector {
                        element: args[0].clone(),
                        share_on_copy: managed_slot_is_plain(module, &args[0])?,
                    },
                    BuiltinType::HashMap => S::Map {
                        key: args[0].clone(),
                        value: args[1].clone(),
                    },
                    BuiltinType::HashSet => S::Set {
                        element: args[0].clone(),
                    },
                    _ => unreachable!("the collection authority admits only these families"),
                }
            } else if let Some(payload) = hew_types::runtime_call::shared_handle_payload(ty) {
                S::Shared {
                    payload: payload.clone(),
                    weak: matches!(
                        ty,
                        T::Named {
                            head: TypeHead::Builtin(BuiltinType::Weak),
                            ..
                        }
                    ),
                }
            } else if let T::Named {
                head: TypeHead::Builtin(builtin),
                args,
                ..
            } = ty
            {
                match builtin {
                    BuiltinType::NodeId | BuiltinType::Location => S::Scalar,
                    BuiltinType::ActorHandle | BuiltinType::ActorFn => {
                        S::Handle(ManagedHandleKind::Actor)
                    }
                    BuiltinType::CancellationToken => S::Handle(ManagedHandleKind::Cancellation),
                    BuiltinType::Task
                    | BuiltinType::ActorCall
                    | BuiltinType::Stream
                    | BuiltinType::Sink => {
                        if args.len() != 1 {
                            return Err(ManagedLayoutError::MissingShape(ty.clone()));
                        }
                        S::Provider {
                            kind: match builtin {
                                BuiltinType::Task => ManagedProviderKind::Task,
                                BuiltinType::ActorCall => ManagedProviderKind::ActorCall,
                                BuiltinType::Stream => ManagedProviderKind::Stream,
                                BuiltinType::Sink => ManagedProviderKind::Sink,
                                _ => unreachable!(),
                            },
                            children: args.clone(),
                        }
                    }
                    BuiltinType::JsonValue | BuiltinType::YamlValue | BuiltinType::TomlValue => {
                        S::Provider {
                            kind: ManagedProviderKind::Encoding(*builtin),
                            children: Vec::new(),
                        }
                    }
                    _ => managed_structural_shape(module, ty)?,
                }
            } else if matches!(
                ty,
                T::Named {
                    head: TypeHead::Actor(_),
                    ..
                }
            ) {
                S::Handle(ManagedHandleKind::Actor)
            } else {
                managed_structural_shape(module, ty)?
            }
        }
    };
    Ok(shape)
}

fn managed_structural_shape(
    module: &SemModule,
    ty: &ResolvedTy,
) -> Result<ManagedStorageShape, ManagedLayoutError> {
    if let Some(shape) = module.variant_shape_for_type(ty) {
        Ok(ManagedStorageShape::Variant {
            indirect: shape.is_indirect,
            payload_bytes: managed_variant_payload(module, shape)?.bytes,
            cases: shape
                .variants
                .iter()
                .map(|case| case.fields.iter().map(|field| field.ty.clone()).collect())
                .collect(),
        })
    } else if let Some(shape) = module.aggregate_shape_for_type(ty) {
        Ok(ManagedStorageShape::Aggregate {
            fields: shape.fields.iter().map(|field| field.ty.clone()).collect(),
        })
    } else if let ResolvedTy::Tuple(fields) = ty {
        Ok(ManagedStorageShape::Aggregate {
            fields: fields.clone(),
        })
    } else if matches!(
        ty,
        ResolvedTy::Named {
            is_opaque: true,
            ..
        }
    ) {
        Ok(ManagedStorageShape::Provider {
            kind: ManagedProviderKind::Opaque,
            children: Vec::new(),
        })
    } else {
        Err(ManagedLayoutError::MissingShape(ty.clone()))
    }
}

/// Whether copying a sequence can share its backing without element callbacks.
/// This uses the checker's exact copy and ownership facts, the same facts that
/// produce a Plain native layout with no clone, drop or release callback.
pub fn managed_slot_is_plain(
    module: &SemModule,
    ty: &ResolvedTy,
) -> Result<bool, ManagedLayoutError> {
    let facts = module
        .type_facts
        .get(&hew_types::TypeInstanceKey(ty.clone()))
        .ok_or_else(|| ManagedLayoutError::MissingFacts(ty.clone()))?;
    Ok(
        crate::OwnKind::of_class(facts.class) == crate::OwnKind::None
            && facts.clone == hew_types::CloneKind::Bits,
    )
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
            head: TypeHead::Builtin(BuiltinType::NodeId),
            ..
        } => return Ok(16),
        T::Named {
            head: TypeHead::Builtin(BuiltinType::Location),
            ..
        } => return Ok(32),
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

/// A lazy generator owns one callable slot and an output slot large enough
/// for either yielded or returned values. Captures and other managed backing
/// storage retain their own identities through the ordinary value recipes.
/// Coroutine scheduling fields and primitive SSA spill slots are bookkeeping,
/// not a target-dependent charge for the runtime's native frame allocation.
pub fn managed_generator_payload(
    module: &SemModule,
    ty: &ResolvedTy,
) -> Result<ManagedSlot, ManagedLayoutError> {
    let (yielded, returned) =
        crate::generator_parts(ty).ok_or_else(|| ManagedLayoutError::MissingShape(ty.clone()))?;
    let output = managed_slot(module, yielded)?
        .bytes
        .max(managed_slot(module, returned)?.bytes);
    output
        .checked_add(8)
        .map(|bytes| ManagedSlot { bytes })
        .ok_or(ManagedLayoutError::Overflow)
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

    #[test]
    fn generator_output_reserves_the_larger_typed_slot() {
        let module = SemModule::default();
        let ty = ResolvedTy::Generator {
            yields: Box::new(ResolvedTy::Tuple(vec![ResolvedTy::U64, ResolvedTy::U8])),
            returns: Box::new(ResolvedTy::Bool),
            producers: Vec::new(),
        };
        assert_eq!(managed_generator_payload(&module, &ty).unwrap().bytes, 17);
        assert_eq!(managed_slot(&module, &ty).unwrap().bytes, 8);
        assert!(matches!(
            managed_storage_shape(&module, &ty).unwrap(),
            ManagedStorageShape::Generator {
                payload_bytes: 17,
                ..
            }
        ));
    }

    #[test]
    fn providers_keep_typed_results_and_handles_keep_nonownership() {
        let module = SemModule::default();
        let task = ResolvedTy::Task(Box::new(ResolvedTy::Bytes));
        assert_eq!(
            managed_storage_shape(&module, &task).unwrap(),
            ManagedStorageShape::Provider {
                kind: ManagedProviderKind::Task,
                children: vec![ResolvedTy::Bytes]
            }
        );
        let borrow = ResolvedTy::Borrow {
            pointee: Box::new(ResolvedTy::String),
        };
        assert_eq!(
            managed_storage_shape(&module, &borrow).unwrap(),
            ManagedStorageShape::Handle(ManagedHandleKind::Borrow)
        );
        let fields = ResolvedTy::Tuple(vec![ResolvedTy::U64, ResolvedTy::String]);
        assert_eq!(
            managed_storage_shape(&module, &fields).unwrap(),
            ManagedStorageShape::Aggregate {
                fields: vec![ResolvedTy::U64, ResolvedTy::String]
            }
        );
    }

    #[test]
    fn plain_sequence_eligibility_requires_published_copy_facts() {
        let mut module = SemModule::default();
        let ty = ResolvedTy::U64;
        assert_eq!(
            managed_slot_is_plain(&module, &ty),
            Err(ManagedLayoutError::MissingFacts(ty.clone()))
        );
        let mut facts = hew_types::TypeFacts {
            class: hew_types::ValueClass::BitCopy,
            clone: hew_types::CloneKind::Bits,
            send: hew_types::SendFact::Known(true),
            hash: true,
            eq: true,
            partial_ord: true,
        };
        module.type_facts.insert(ty.clone().into(), facts);
        assert_eq!(managed_slot_is_plain(&module, &ty), Ok(true));
        facts.clone = hew_types::CloneKind::FieldWise;
        module.type_facts.insert(ty.clone().into(), facts);
        assert_eq!(managed_slot_is_plain(&module, &ty), Ok(false));
    }
}
