//! Structural rendering recipes for `f"{v:?}"`.
//!
//! Physical MIR owns how a concrete type spells itself: the runtime builder
//! entry each leaf appends through, the source names a record or enum prints,
//! and the recipe of every part reached from it. Codegen realizes one
//! borrow-only formatter thunk per recipe and decides nothing about spelling,
//! traversal order or ownership - a rendered value is borrowed throughout.
//!
//! Names come from the SIR shape tables, the same source the debug metadata
//! projects, so rendering and `-g` cannot disagree and rendering does not
//! depend on a debug build.

use std::collections::BTreeMap;

use hew_sir::{CallableId, SemModule, SemVariantKind, StructuralType};
use hew_types::{BuiltinType, ResolvedTy};

use super::PhysicalError;

/// Module-local identity of one structural rendering recipe.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PhysicalStructuralId(pub u32);

/// One record field as the source spells it, with its own recipe.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PhysicalStructuralField {
    pub name: String,
    pub recipe: PhysicalStructuralId,
}

/// One enum case as the source spells it. Its position is its tag value.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PhysicalStructuralCase {
    pub name: String,
    pub kind: SemVariantKind,
    /// Payload recipes in declaration order; empty for a unit case.
    pub fields: Vec<PhysicalStructuralField>,
}

/// How one concrete type renders.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PhysicalStructuralShape {
    /// Borrow the receiver through its checker-selected `Display::fmt` body.
    Display {
        callable: CallableId,
    },
    /// Sign-extend the value's own integer width and append it as `i64`.
    SignedInt,
    /// Zero-extend the value's own integer width and append it as `u64`.
    UnsignedInt,
    /// Extend the value's own float width and append it as `f64`.
    Float,
    Bool,
    Char,
    /// The unit value prints as `()`.
    Unit,
    /// A managed string prints its text unquoted.
    String,
    /// An `#[opaque]` handle prints `<Name@address>`: its identity without its
    /// representation bits.
    Identity {
        name: String,
    },
    /// `(a, b)` over the tuple's members in declaration order.
    Tuple {
        fields: Vec<PhysicalStructuralId>,
    },
    /// `Name { field: value, .. }` over the record's fields.
    Record {
        name: String,
        fields: Vec<PhysicalStructuralField>,
    },
    /// `Case`, `Case(a, b)` or `Case { field: value }`, selected by the tag.
    Enum {
        cases: Vec<PhysicalStructuralCase>,
    },
    /// `[a, b]` through the runtime's borrowed element walk.
    Vector {
        element: PhysicalStructuralId,
    },
    /// `{k: v}` through the runtime's borrowed slot walk.
    Map {
        key: PhysicalStructuralId,
        value: PhysicalStructuralId,
    },
}

/// One concrete type's rendering recipe.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PhysicalStructuralGlue {
    pub id: PhysicalStructuralId,
    pub ty: ResolvedTy,
    pub shape: PhysicalStructuralShape,
}

/// Interning builder over the types reached by structural rendering.
///
/// A recipe's identity is reserved before its parts are resolved, so a type
/// that reaches itself through an indirect enum terminates here.
#[derive(Debug, Default)]
pub(super) struct StructuralGlue {
    ids: BTreeMap<StructuralType, PhysicalStructuralId>,
    glue: Vec<Option<PhysicalStructuralGlue>>,
}

impl StructuralGlue {
    /// Resolve the recipe for `ty`, creating it and its parts on first reach.
    ///
    /// # Errors
    ///
    /// Refuses a type with no rendering recipe. The checker admits only the
    /// renderable types, so this is physical MIR verifying its own contract.
    pub(super) fn intern(
        &mut self,
        module: &SemModule,
        ty: &ResolvedTy,
    ) -> Result<PhysicalStructuralId, PhysicalError> {
        self.intern_source(module, &StructuralType::canonical(ty))
    }

    fn intern_source(
        &mut self,
        module: &SemModule,
        key: &StructuralType,
    ) -> Result<PhysicalStructuralId, PhysicalError> {
        if let Some(id) = self.ids.get(key) {
            return Ok(*id);
        }
        let index = u32::try_from(self.glue.len())
            .map_err(|_| PhysicalError::new("structural recipe count exceeds u32"))?;
        let id = PhysicalStructuralId(index);
        self.ids.insert(key.clone(), id);
        self.glue.push(None);
        let selected = module.structural_display.get(key).ok_or_else(|| {
            PhysicalError::new("structural rendering lacks its checked source selection")
        })?;
        let shape = if let Some(callable) = selected.display {
            PhysicalStructuralShape::Display { callable }
        } else {
            self.shape(module, &key.value, &selected.members)?
        };
        self.glue[index as usize] = Some(PhysicalStructuralGlue {
            id,
            ty: key.value.clone(),
            shape,
        });
        Ok(id)
    }

    /// Every interned recipe in identity order.
    ///
    /// # Errors
    ///
    /// Refuses a reserved identity whose recipe was never completed.
    pub(super) fn finish(self) -> Result<Vec<PhysicalStructuralGlue>, PhysicalError> {
        self.glue
            .into_iter()
            .enumerate()
            .map(|(index, glue)| {
                glue.ok_or_else(|| {
                    PhysicalError::new(format!("structural recipe {index} was never completed"))
                })
            })
            .collect()
    }

    fn shape(
        &mut self,
        module: &SemModule,
        ty: &ResolvedTy,
        members: &[StructuralType],
    ) -> Result<PhysicalStructuralShape, PhysicalError> {
        match ty {
            ResolvedTy::I8
            | ResolvedTy::I16
            | ResolvedTy::I32
            | ResolvedTy::I64
            | ResolvedTy::Isize => Ok(PhysicalStructuralShape::SignedInt),
            ResolvedTy::U8
            | ResolvedTy::U16
            | ResolvedTy::U32
            | ResolvedTy::U64
            | ResolvedTy::Usize => Ok(PhysicalStructuralShape::UnsignedInt),
            ResolvedTy::F32 | ResolvedTy::F64 => Ok(PhysicalStructuralShape::Float),
            ResolvedTy::Bool => Ok(PhysicalStructuralShape::Bool),
            ResolvedTy::Char => Ok(PhysicalStructuralShape::Char),
            ResolvedTy::Unit => Ok(PhysicalStructuralShape::Unit),
            ResolvedTy::String => Ok(PhysicalStructuralShape::String),
            ResolvedTy::Tuple(_) => {
                let fields = members
                    .iter()
                    .map(|member| self.intern_source(module, member))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(PhysicalStructuralShape::Tuple { fields })
            }
            ResolvedTy::Named {
                name,
                builtin,
                is_opaque,
                ..
            } => self.named_shape(module, ty, name, *builtin, *is_opaque, members),
            _ => Err(refusal(ty)),
        }
    }

    fn named_shape(
        &mut self,
        module: &SemModule,
        ty: &ResolvedTy,
        name: &str,
        builtin: Option<BuiltinType>,
        is_opaque: bool,
        members: &[StructuralType],
    ) -> Result<PhysicalStructuralShape, PhysicalError> {
        if builtin == Some(BuiltinType::Vec) {
            let [element] = members else {
                return Err(refusal(ty));
            };
            return Ok(PhysicalStructuralShape::Vector {
                element: self.intern_source(module, element)?,
            });
        }
        if builtin == Some(BuiltinType::HashMap) {
            let [key, value] = members else {
                return Err(refusal(ty));
            };
            return Ok(PhysicalStructuralShape::Map {
                key: self.intern_source(module, key)?,
                value: self.intern_source(module, value)?,
            });
        }
        // An opaque handle is not its members, so it renders as its identity
        // rather than through a field walk it must not see through.
        if is_opaque {
            return Ok(PhysicalStructuralShape::Identity {
                name: name.to_string(),
            });
        }
        if let Some(shape) = module.variant_shape_for_type(ty) {
            let mut members = members.iter();
            let cases = shape
                .variants
                .iter()
                .map(|variant| {
                    let fields = variant
                        .fields
                        .iter()
                        .map(|field| {
                            Ok(PhysicalStructuralField {
                                name: field.name.clone(),
                                recipe: self.intern_source(
                                    module,
                                    members.next().ok_or_else(|| refusal(ty))?,
                                )?,
                            })
                        })
                        .collect::<Result<Vec<_>, _>>()?;
                    Ok(PhysicalStructuralCase {
                        name: variant.name.clone(),
                        kind: variant.kind,
                        fields,
                    })
                })
                .collect::<Result<Vec<_>, PhysicalError>>()?;
            return Ok(PhysicalStructuralShape::Enum { cases });
        }
        if let Some(shape) = module.aggregate_shape_for_type(ty) {
            let fields = shape
                .fields
                .iter()
                .zip(members)
                .map(|(field, member)| {
                    Ok(PhysicalStructuralField {
                        name: field.name.clone(),
                        recipe: self.intern_source(module, member)?,
                    })
                })
                .collect::<Result<Vec<_>, PhysicalError>>()?;
            return Ok(PhysicalStructuralShape::Record {
                name: name.to_string(),
                fields,
            });
        }
        Err(refusal(ty))
    }
}

fn refusal(ty: &ResolvedTy) -> PhysicalError {
    PhysicalError::new(format!(
        "`{}` has no structural rendering recipe",
        ty.user_facing()
    ))
}
