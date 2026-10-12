//! Codec plans: the shape of every concrete type a codec walk visits, joined
//! to the checker's serial layouts. Physical stages supply storage layout.

use std::collections::BTreeMap;

use hew_types::ResolvedTy;

use crate::{AggregateShapeId, VariantShapeId};

/// Every plan one codec call reaches, closed under children. A child is named
/// by its type and planned once, so recursive types terminate.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SemWirePlans {
    /// Generated arguments preserve fields and explicit optional presence.
    pub property_input: bool,
    pub root: ResolvedTy,
    pub plans: BTreeMap<ResolvedTy, SemWirePlan>,
}

/// One concrete type's encode and decode shape.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SemWirePlan {
    pub ty: ResolvedTy,
    pub kind: SemWireKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SemWireKind {
    Scalar,
    Unit,
    Tuple(Vec<ResolvedTy>),
    Array {
        element: ResolvedTy,
        len: u64,
    },
    Vector(ResolvedTy),
    Set(ResolvedTy),
    Map {
        key: ResolvedTy,
        value: ResolvedTy,
    },
    Option {
        shape: VariantShapeId,
        none: u32,
        some: u32,
        value: ResolvedTy,
    },
    /// Fields in declaration order: field `i` is aggregate field `i` and
    /// table member `i`. A positional record has no table and encodes as a
    /// sequence.
    Record {
        shape: AggregateShapeId,
        table: Option<SemWireTable>,
        fields: Vec<ResolvedTy>,
    },
    /// Variants in declaration order: variant `i` is shape variant `i` and
    /// table member `i`.
    Enum {
        shape: VariantShapeId,
        table: SemWireTable,
        variants: Vec<SemWirePayload>,
    },
}

/// The runtime table of a record's fields or an enum's variants.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SemWireTable {
    pub tagged: bool,
    pub members: Vec<SemWireMember>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SemWireMember {
    pub key: String,
    pub tag: u64,
    /// `hew_codec::Member` flag bits.
    pub flags: u32,
}

/// One variant's payload: one value, a sequence, or named fields.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SemWirePayload {
    Unit,
    Single(ResolvedTy),
    Tuple(Vec<ResolvedTy>),
    Record {
        table: SemWireTable,
        fields: Vec<ResolvedTy>,
    },
}

impl SemWirePayload {
    #[must_use]
    pub fn types(&self) -> &[ResolvedTy] {
        match self {
            Self::Unit => &[],
            Self::Single(ty) => std::slice::from_ref(ty),
            Self::Tuple(fields) | Self::Record { fields, .. } => fields,
        }
    }
}

impl SemWirePlan {
    /// The types this plan's walk visits directly.
    #[must_use]
    pub fn children(&self) -> Vec<&ResolvedTy> {
        match &self.kind {
            SemWireKind::Scalar | SemWireKind::Unit => Vec::new(),
            SemWireKind::Tuple(elements)
            | SemWireKind::Record {
                fields: elements, ..
            } => elements.iter().collect(),
            SemWireKind::Array { element, .. }
            | SemWireKind::Vector(element)
            | SemWireKind::Set(element)
            | SemWireKind::Option { value: element, .. } => vec![element],
            SemWireKind::Map { key, value } => vec![key, value],
            SemWireKind::Enum { variants, .. } => {
                variants.iter().flat_map(SemWirePayload::types).collect()
            }
        }
    }
}

impl SemWirePlans {
    /// Project the checked codec walk into the shared generator and VM value vocabulary.
    ///
    /// # Errors
    /// Returns an error for missing child plans, unsupported scalars or oversized arrays.
    #[expect(
        clippy::too_many_lines,
        reason = "one exhaustive projection keeps codec shape and value metadata together"
    )]
    pub fn property_shape(&self) -> Result<hew_codec::property::Shape, String> {
        use hew_codec::property::{Key, Node, Shape};
        let ids: BTreeMap<_, _> = self
            .plans
            .keys()
            .enumerate()
            .map(|(index, ty)| (ty, index))
            .collect();
        let child = |ty: &ResolvedTy| {
            ids.get(ty)
                .copied()
                .ok_or_else(|| format!("property shape lacks `{}`", ty.user_facing()))
        };
        let keys = |table: &SemWireTable| {
            table
                .members
                .iter()
                .map(|member| Key {
                    name: member.key.clone(),
                    tag: table.tagged.then_some(member.tag),
                    flags: member.flags,
                })
                .collect::<Vec<_>>()
        };
        let mut nodes = vec![Node::Unit; self.plans.len()];
        let mut names = self
            .plans
            .keys()
            .map(|ty| ty.user_facing().to_string())
            .collect::<Vec<_>>();
        let mut aggregates = vec![None; self.plans.len()];
        let mut variant_shapes = vec![None; self.plans.len()];
        let mut grouped_payloads = vec![false; self.plans.len()];
        for (ty, plan) in &self.plans {
            match plan.kind {
                SemWireKind::Record { shape, .. } => aggregates[child(ty)?] = Some(shape.0),
                SemWireKind::Enum { shape, .. } | SemWireKind::Option { shape, .. } => {
                    variant_shapes[child(ty)?] = Some(shape.0);
                }
                _ => {}
            }
            let node = match &plan.kind {
                SemWireKind::Scalar => match ty {
                    ResolvedTy::Bool => Node::Bool,
                    ResolvedTy::Char => Node::Char,
                    ResolvedTy::String => Node::String,
                    ResolvedTy::Bytes => Node::Bytes,
                    ResolvedTy::F32 => Node::Float { bits: 32 },
                    ResolvedTy::F64 => Node::Float { bits: 64 },
                    ResolvedTy::I8 => Node::Signed {
                        min: i64::from(i8::MIN),
                        max: i64::from(i8::MAX),
                    },
                    ResolvedTy::I16 => Node::Signed {
                        min: i64::from(i16::MIN),
                        max: i64::from(i16::MAX),
                    },
                    ResolvedTy::I32 => Node::Signed {
                        min: i64::from(i32::MIN),
                        max: i64::from(i32::MAX),
                    },
                    ResolvedTy::I64 | ResolvedTy::Isize | ResolvedTy::Duration => Node::Signed {
                        min: i64::MIN,
                        max: i64::MAX,
                    },
                    ResolvedTy::U8 => Node::Unsigned {
                        max: u64::from(u8::MAX),
                    },
                    ResolvedTy::U16 => Node::Unsigned {
                        max: u64::from(u16::MAX),
                    },
                    ResolvedTy::U32 => Node::Unsigned {
                        max: u64::from(u32::MAX),
                    },
                    ResolvedTy::U64 | ResolvedTy::Usize => Node::Unsigned { max: u64::MAX },
                    _ => {
                        return Err(format!(
                            "property scalar `{}` is unsupported",
                            ty.user_facing()
                        ))
                    }
                },
                SemWireKind::Unit => Node::Unit,
                SemWireKind::Tuple(fields) => {
                    Node::Tuple(fields.iter().map(&child).collect::<Result<_, _>>()?)
                }
                SemWireKind::Array { element, len } => Node::Array {
                    element: child(element)?,
                    len: usize::try_from(*len).map_err(|_| "property array is too large")?,
                },
                SemWireKind::Vector(element) => Node::Sequence(child(element)?),
                SemWireKind::Set(element) => Node::Set(child(element)?),
                SemWireKind::Map { key, value } => Node::Map {
                    key: child(key)?,
                    value: child(value)?,
                },
                SemWireKind::Option { value, .. } => Node::Option(child(value)?),
                SemWireKind::Record {
                    table: None,
                    fields,
                    ..
                } => Node::Tuple(fields.iter().map(&child).collect::<Result<_, _>>()?),
                SemWireKind::Record {
                    table: Some(table),
                    fields,
                    ..
                } => Node::Record(
                    keys(table)
                        .into_iter()
                        .zip(fields.iter().map(&child).collect::<Result<Vec<_>, _>>()?)
                        .collect(),
                ),
                SemWireKind::Enum {
                    table, variants, ..
                } => {
                    let mut cases = Vec::new();
                    for (key, variant) in keys(table).into_iter().zip(variants) {
                        let payload = match variant {
                            SemWirePayload::Unit => None,
                            SemWirePayload::Single(ty) => Some(child(ty)?),
                            SemWirePayload::Tuple(fields) => {
                                let index = nodes.len();
                                names.push(String::new());
                                grouped_payloads.push(true);
                                aggregates.push(None);
                                variant_shapes.push(None);
                                nodes.push(Node::Tuple(
                                    fields.iter().map(&child).collect::<Result<_, _>>()?,
                                ));
                                Some(index)
                            }
                            SemWirePayload::Record { table, fields } => {
                                let index = nodes.len();
                                names.push(String::new());
                                grouped_payloads.push(true);
                                aggregates.push(None);
                                variant_shapes.push(None);
                                nodes.push(Node::Record(
                                    keys(table)
                                        .into_iter()
                                        .zip(
                                            fields
                                                .iter()
                                                .map(&child)
                                                .collect::<Result<Vec<_>, _>>()?,
                                        )
                                        .collect(),
                                ));
                                Some(index)
                            }
                        };
                        cases.push((key, payload));
                    }
                    Node::Variant(cases)
                }
            };
            nodes[child(ty)?] = node;
        }
        Ok(Shape {
            root: child(&self.root)?,
            nodes,
            names,
            aggregates,
            variants: variant_shapes,
            grouped_payloads,
        })
    }

    #[must_use]
    pub fn get(&self, ty: &ResolvedTy) -> Option<&SemWirePlan> {
        self.plans.get(ty)
    }

    /// Check every plan against the exact shapes it names before physical
    /// lowering uses them.
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
