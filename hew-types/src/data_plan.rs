//! Codec plans: the shape of every concrete type a codec walk visits, joined
//! to the checker's serial layouts. Physical stages supply storage layout.

use std::collections::BTreeMap;

use crate::def_table::DeclarationOccurrence;
use crate::ResolvedTy;

/// Largest fractional nanosecond in a normalized timestamp data value.
pub const TIMESTAMP_NANOSECONDS_MAX: u64 = 999_999_999;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DataVariant {
    pub name: String,
    pub named: bool,
    pub fields: Vec<(String, ResolvedTy)>,
}

impl crate::TypeFactService {
    /// Read exact declaration-order enum payloads, including substituted binders.
    ///
    /// # Errors
    /// Refuses missing declarations, malformed binders and non-enum types.
    pub fn data_variants(&self, ty: &ResolvedTy) -> Result<Vec<DataVariant>, String> {
        if let ResolvedTy::Named {
            head: crate::TypeHead::Builtin(builtin),
            args,
            ..
        } = ty
        {
            let declaration = builtin
                .generic_enum()
                .ok_or_else(|| format!("`{ty}` is not a data enum"))?;
            if declaration.type_params.len() != args.len() {
                return Err("data enum has incorrect type argument arity".into());
            }
            return Ok(declaration
                .variants
                .iter()
                .map(|variant| DataVariant {
                    name: variant.name.into(),
                    named: false,
                    fields: variant
                        .payload_type_args
                        .iter()
                        .enumerate()
                        .map(|(index, argument)| (index.to_string(), args[*argument].clone()))
                        .collect(),
                })
                .collect());
        }
        let defs = self.defs();
        let instance = ty
            .nominal_instance(defs)
            .ok_or_else(|| format!("`{ty}` is not a data enum"))?;
        let context = self.context();
        let definition = context
            .types()
            .type_defs
            .get(&instance.nominal)
            .ok_or("data enum has no exact declaration")?;
        if definition.kind != crate::check::TypeDefKind::Enum
            || definition.type_params.len() != instance.args.len()
        {
            return Err("data enum has no exact instantiated enum declaration".into());
        }
        let member_kind =
            if defs.kind(instance.nominal.declaration()) == crate::DeclarationKind::Machine {
                crate::DeclarationKind::MachineState
            } else {
                crate::DeclarationKind::Variant
            };
        let mut variants = defs
            .members_of_kind(instance.nominal.declaration(), member_kind)
            .collect::<Vec<_>>();
        variants.sort_by_key(|variant| defs.site(*variant).map(DeclarationOccurrence::ordinal));
        if variants.len() != definition.variants.len() {
            return Err("data enum members and declaration disagree".into());
        }
        variants
            .into_iter()
            .map(|variant| {
                let name = defs.name(variant).to_string();
                let (named, fields) = match definition
                    .variants
                    .get(&name)
                    .ok_or("data enum member has no payload declaration")?
                {
                    crate::check::VariantDef::Unit => (false, Vec::new()),
                    crate::check::VariantDef::Tuple(fields) => (
                        false,
                        fields
                            .iter()
                            .enumerate()
                            .map(|(index, ty)| (index.to_string(), ty))
                            .collect(),
                    ),
                    crate::check::VariantDef::Struct(fields) => (
                        true,
                        fields.iter().map(|(name, ty)| (name.clone(), ty)).collect(),
                    ),
                };
                let fields = fields
                    .into_iter()
                    .map(|(name, ty)| {
                        let ty = ResolvedTy::from_ty(ty)
                            .map_err(|error| format!("invalid data enum payload: {error:?}"))?;
                        Ok((
                            name,
                            crate::resolved_ty::substitute_type_params(
                                &ty,
                                &definition.type_params,
                                &instance.args,
                            ),
                        ))
                    })
                    .collect::<Result<_, String>>()?;
                Ok(DataVariant {
                    name,
                    named,
                    fields,
                })
            })
            .collect()
    }

    /// Build the format-neutral codec walk used by compile-time and runtime decoding.
    ///
    /// # Errors
    /// Refuses non-data types and incomplete declaration metadata.
    pub fn data_plans(&self, root: &ResolvedTy, property: bool) -> Result<DataPlans, String> {
        let error = if property {
            self.property_error(root)
        } else {
            self.data_error(root, false, &|_, _| false)
        };
        if let Some(error) = error {
            return Err(format!(
                "{}{} is {}",
                root.user_facing(),
                error.path,
                error.describe()
            ));
        }
        let mut plans = BTreeMap::new();
        let mut work = vec![root.clone()];
        while let Some(ty) = work.pop() {
            if plans.contains_key(&ty) {
                continue;
            }
            let plan = DataPlan {
                kind: self.data_plan_kind(&ty)?,
                ty: ty.clone(),
            };
            work.extend(plan.children().into_iter().cloned());
            plans.insert(ty, plan);
        }
        Ok(DataPlans {
            root: root.clone(),
            plans,
        })
    }

    #[expect(
        clippy::too_many_lines,
        reason = "one exhaustive match keeps every checked data shape together"
    )]
    fn data_plan_kind(&self, ty: &ResolvedTy) -> Result<DataKind<(), ()>, String> {
        use crate::{BuiltinType, TypeHead};
        Ok(match ty {
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
            | ResolvedTy::Bytes => DataKind::Scalar,
            ResolvedTy::Unit => DataKind::Unit,
            ResolvedTy::Tuple(fields) => DataKind::Tuple(fields.clone()),
            ResolvedTy::Array(element, len) => DataKind::Array {
                element: (**element).clone(),
                len: *len,
            },
            ResolvedTy::Named {
                head: TypeHead::Builtin(builtin),
                args,
                ..
            } => match (builtin, args.as_slice()) {
                (BuiltinType::Timestamp, []) => {
                    let (_, fields) = self.record_fields(ty)?;
                    let fields = fields.into_iter().map(|(_, ty)| ty).collect::<Vec<_>>();
                    if fields != [ResolvedTy::I64, ResolvedTy::U32] {
                        return Err("timestamp data shape lacks its normalized pair".into());
                    }
                    DataKind::Timestamp {
                        shape: (),
                        fields,
                        nanoseconds_max: TIMESTAMP_NANOSECONDS_MAX,
                    }
                }
                (BuiltinType::Vec, [element]) => DataKind::Vector(element.clone()),
                (BuiltinType::HashSet, [element]) => DataKind::Set(element.clone()),
                (BuiltinType::HashMap, [key, value]) => DataKind::Map {
                    key: key.clone(),
                    value: value.clone(),
                },
                (BuiltinType::Option, [value]) => {
                    let variants = self.data_variants(ty)?;
                    let none = variants
                        .iter()
                        .position(|variant| variant.fields.is_empty())
                        .ok_or("data Option has no empty case")?;
                    let some = variants
                        .iter()
                        .position(|variant| {
                            variant.fields.len() == 1 && variant.fields[0].1 == *value
                        })
                        .ok_or("data Option has no exact payload case")?;
                    DataKind::Option {
                        shape: (),
                        none: u32::try_from(none).map_err(|_| "Option index exceeds u32")?,
                        some: u32::try_from(some).map_err(|_| "Option index exceeds u32")?,
                        value: value.clone(),
                    }
                }
                (BuiltinType::Result, [_, _]) => {
                    let variants = self.data_variants(ty)?;
                    DataKind::Enum {
                        shape: (),
                        table: DataTable {
                            tagged: false,
                            members: variants
                                .iter()
                                .map(|variant| DataMember {
                                    key: variant.name.clone(),
                                    tag: 0,
                                    flags: hew_codec::Member::PAYLOAD,
                                })
                                .collect(),
                        },
                        variants: variants
                            .iter()
                            .map(|variant| data_payload(variant, None))
                            .collect(),
                    }
                }
                _ => return Err(format!("`{ty}` is outside the checked data shape")),
            },
            ResolvedTy::Named { .. } => {
                let instance = ty
                    .nominal_instance(self.defs())
                    .ok_or("data type has no nominal identity")?;
                let layout = self
                    .context()
                    .serial_layout(instance.nominal)
                    .ok_or("data type has no serial layout")?;
                if let Ok((_, fields)) = self.record_fields(ty) {
                    let fields = fields.into_iter().map(|(_, ty)| ty).collect::<Vec<_>>();
                    if !layout.positional && fields.len() != layout.members.len() {
                        return Err("data fields and serial layout disagree".into());
                    }
                    DataKind::Record {
                        shape: (),
                        table: (!layout.positional)
                            .then(|| data_table(layout.tagged, &layout.members)),
                        fields,
                    }
                } else {
                    let variants = self.data_variants(ty)?;
                    if variants.len() != layout.members.len() {
                        return Err("data variants and serial layout disagree".into());
                    }
                    DataKind::Enum {
                        shape: (),
                        table: data_table(layout.tagged, &layout.members),
                        variants: variants
                            .iter()
                            .zip(&layout.members)
                            .map(|(variant, member)| data_payload(variant, Some(member)))
                            .collect(),
                    }
                }
            }
            _ => return Err(format!("`{ty}` is outside the checked data shape")),
        })
    }
}

fn data_table(tagged: bool, members: &[crate::data_shape::SerialMember]) -> DataTable {
    DataTable {
        tagged,
        members: members
            .iter()
            .map(|member| DataMember {
                key: member.key.clone(),
                tag: member.tag,
                flags: member.flags,
            })
            .collect(),
    }
}

fn data_payload(
    variant: &DataVariant,
    member: Option<&crate::data_shape::SerialMember>,
) -> DataPayload {
    let fields = variant
        .fields
        .iter()
        .map(|(_, ty)| ty.clone())
        .collect::<Vec<_>>();
    match (variant.named, fields.as_slice()) {
        (_, []) => DataPayload::Unit,
        (true, _) => DataPayload::Record {
            table: member.map_or_else(
                || DataTable {
                    tagged: false,
                    members: Vec::new(),
                },
                |member| data_table(false, &member.fields),
            ),
            fields,
        },
        (_, [single]) => DataPayload::Single(single.clone()),
        _ => DataPayload::Tuple(fields),
    }
}

/// Every plan one codec call reaches, closed under children. A child is named
/// by its type and planned once, so recursive types terminate.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DataPlans {
    pub root: ResolvedTy,
    pub plans: BTreeMap<ResolvedTy, DataPlan<(), ()>>,
}

/// One concrete type's encode and decode shape.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DataPlan<A, V> {
    pub ty: ResolvedTy,
    pub kind: DataKind<A, V>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DataKind<A, V> {
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
        shape: V,
        none: u32,
        some: u32,
        value: ResolvedTy,
    },
    /// A normalized Unix-seconds/nanoseconds pair. The wire sequence has
    /// exactly two fields and rejects fractions outside `0..1_000_000_000`.
    Timestamp {
        shape: A,
        fields: Vec<ResolvedTy>,
        nanoseconds_max: u64,
    },
    /// Fields in declaration order: field `i` is aggregate field `i` and
    /// table member `i`. A positional record has no table and encodes as a
    /// sequence.
    Record {
        shape: A,
        table: Option<DataTable>,
        fields: Vec<ResolvedTy>,
    },
    /// Variants in declaration order: variant `i` is shape variant `i` and
    /// table member `i`.
    Enum {
        shape: V,
        table: DataTable,
        variants: Vec<DataPayload>,
    },
}

/// The runtime table of a record's fields or an enum's variants.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DataTable {
    pub tagged: bool,
    pub members: Vec<DataMember>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DataMember {
    pub key: String,
    pub tag: u64,
    /// `hew_codec::Member` flag bits.
    pub flags: u32,
}

/// One variant's payload: one value, a sequence, or named fields.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DataPayload {
    Unit,
    Single(ResolvedTy),
    Tuple(Vec<ResolvedTy>),
    Record {
        table: DataTable,
        fields: Vec<ResolvedTy>,
    },
}

impl DataPayload {
    #[must_use]
    pub fn types(&self) -> &[ResolvedTy] {
        match self {
            Self::Unit => &[],
            Self::Single(ty) => std::slice::from_ref(ty),
            Self::Tuple(fields) | Self::Record { fields, .. } => fields,
        }
    }
}

impl<A, V> DataPlan<A, V> {
    /// The types this plan's walk visits directly.
    #[must_use]
    pub fn children(&self) -> Vec<&ResolvedTy> {
        match &self.kind {
            DataKind::Scalar | DataKind::Unit => Vec::new(),
            DataKind::Tuple(elements)
            | DataKind::Timestamp {
                fields: elements, ..
            }
            | DataKind::Record {
                fields: elements, ..
            } => elements.iter().collect(),
            DataKind::Array { element, .. }
            | DataKind::Vector(element)
            | DataKind::Set(element)
            | DataKind::Option { value: element, .. } => vec![element],
            DataKind::Map { key, value } => vec![key, value],
            DataKind::Enum { variants, .. } => {
                variants.iter().flat_map(DataPayload::types).collect()
            }
        }
    }
}

/// Project the checked codec walk into the shared generator and VM value vocabulary.
/// Platform-sized integers use the consumer's target pointer width.
///
/// # Errors
/// Returns an error for missing child plans, unsupported scalars or oversized arrays.
#[expect(
    clippy::too_many_lines,
    reason = "one exhaustive projection keeps codec shape and value metadata together"
)]
pub fn property_shape<A, V>(
    root: &ResolvedTy,
    plans: &BTreeMap<ResolvedTy, DataPlan<A, V>>,
    pointer_width: u8,
    aggregate: impl Fn(&A) -> Option<u32>,
    variant: impl Fn(&V) -> Option<u32>,
) -> Result<hew_codec::property::Shape, String> {
    use hew_codec::property::{Key, Node, Shape};
    let ids: BTreeMap<_, _> = plans
        .keys()
        .enumerate()
        .map(|(index, ty)| (ty, index))
        .collect();
    let child = |ty: &ResolvedTy| {
        ids.get(ty)
            .copied()
            .ok_or_else(|| format!("property shape lacks `{}`", ty.user_facing()))
    };
    let keys = |table: &DataTable| {
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
    let mut nodes = vec![Node::Unit; plans.len()];
    let mut names = plans
        .keys()
        .map(|ty| ty.user_facing().to_string())
        .collect::<Vec<_>>();
    let mut aggregates = vec![None; plans.len()];
    let mut variant_shapes = vec![None; plans.len()];
    let mut grouped_payloads = vec![false; plans.len()];
    for (ty, plan) in plans {
        match &plan.kind {
            DataKind::Record { shape, .. } | DataKind::Timestamp { shape, .. } => {
                aggregates[child(ty)?] = aggregate(shape);
            }
            DataKind::Enum { shape, .. } | DataKind::Option { shape, .. } => {
                variant_shapes[child(ty)?] = variant(shape);
            }
            _ => {}
        }
        let node = match &plan.kind {
            DataKind::Scalar => match ty {
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
                ResolvedTy::Isize if pointer_width == 32 => Node::Signed {
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
                ResolvedTy::Usize if pointer_width == 32 => Node::Unsigned {
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
            DataKind::Timestamp {
                fields,
                nanoseconds_max,
                ..
            } => {
                if fields != &[ResolvedTy::I64, ResolvedTy::U32] {
                    return Err("timestamp property shape lacks its normalized pair".into());
                }
                let fraction = nodes.len();
                nodes.push(Node::Unsigned {
                    max: *nanoseconds_max,
                });
                names.push("u32".into());
                aggregates.push(None);
                variant_shapes.push(None);
                grouped_payloads.push(false);
                Node::Tuple(vec![child(&fields[0])?, fraction])
            }
            DataKind::Unit => Node::Unit,
            DataKind::Tuple(fields) => {
                Node::Tuple(fields.iter().map(&child).collect::<Result<_, _>>()?)
            }
            DataKind::Array { element, len } => Node::Array {
                element: child(element)?,
                len: usize::try_from(*len).map_err(|_| "property array is too large")?,
            },
            DataKind::Vector(element) => Node::Sequence(child(element)?),
            DataKind::Set(element) => Node::Set(child(element)?),
            DataKind::Map { key, value } => Node::Map {
                key: child(key)?,
                value: child(value)?,
            },
            DataKind::Option { value, .. } => Node::Option(child(value)?),
            DataKind::Record {
                table: None,
                fields,
                ..
            } => Node::Tuple(fields.iter().map(&child).collect::<Result<_, _>>()?),
            DataKind::Record {
                table: Some(table),
                fields,
                ..
            } => Node::Record(
                keys(table)
                    .into_iter()
                    .zip(fields.iter().map(&child).collect::<Result<Vec<_>, _>>()?)
                    .collect(),
            ),
            DataKind::Enum {
                table, variants, ..
            } => {
                let mut cases = Vec::new();
                for (key, variant) in keys(table).into_iter().zip(variants) {
                    let payload = match variant {
                        DataPayload::Unit => None,
                        DataPayload::Single(ty) => Some(child(ty)?),
                        DataPayload::Tuple(fields) => {
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
                        DataPayload::Record { table, fields } => {
                            let index = nodes.len();
                            names.push(String::new());
                            grouped_payloads.push(true);
                            aggregates.push(None);
                            variant_shapes.push(None);
                            nodes.push(Node::Record(
                                keys(table)
                                    .into_iter()
                                    .zip(fields.iter().map(&child).collect::<Result<Vec<_>, _>>()?)
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
        root: child(root)?,
        nodes,
        names,
        aggregates,
        variants: variant_shapes,
        grouped_payloads,
    })
}
