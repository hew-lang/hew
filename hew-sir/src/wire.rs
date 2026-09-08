//! Exact wire schemas selected from checked type and field identities.

use std::sync::Arc;

use hew_types::{ResolvedTy, WireFieldPresence};

use crate::{AggregateShapeId, VariantShapeId};

/// One encode/decode schema. Physical stages supply storage layout separately.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SemWirePlan {
    pub ty: ResolvedTy,
    pub kind: SemWireKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SemWireKind {
    Scalar,
    Vector(Arc<SemWirePlan>),
    Set(Arc<SemWirePlan>),
    Map {
        key: Arc<SemWirePlan>,
        value: Arc<SemWirePlan>,
    },
    Option {
        shape: VariantShapeId,
        none: u32,
        some: u32,
        value: Arc<SemWirePlan>,
    },
    Record {
        shape: AggregateShapeId,
        fields: Vec<SemWireField>,
    },
    Enum {
        shape: VariantShapeId,
        variants: Vec<SemWireVariant>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SemWireField {
    pub index: u32,
    pub tag: u32,
    pub json_name: String,
    pub yaml_name: String,
    pub presence: WireFieldPresence,
    pub value: Arc<SemWirePlan>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SemWireVariant {
    pub index: u32,
    pub tag: u32,
    pub json_name: String,
    pub yaml_name: String,
    pub fields: Vec<Arc<SemWirePlan>>,
}

impl SemWirePlan {
    /// Check semantic field selections before physical lowering can use them.
    #[expect(
        clippy::too_many_lines,
        reason = "one exhaustive check keeps schema selections tied to their checked value shapes"
    )]
    pub(crate) fn verify(
        &self,
        records: &[crate::SemAggregateShape],
        enums: &[crate::SemVariantShape],
    ) -> Result<(), String> {
        fn child(
            expected: &ResolvedTy,
            plan: &SemWirePlan,
            records: &[crate::SemAggregateShape],
            enums: &[crate::SemVariantShape],
        ) -> Result<(), String> {
            if expected != &plan.ty {
                return Err("wire child type disagrees with checked shape".into());
            }
            plan.verify(records, enums)
        }
        let bad = || "wire plan disagrees with its concrete checked type".to_string();
        match (&self.ty, &self.kind) {
            (
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
                | ResolvedTy::Bytes,
                SemWireKind::Scalar,
            ) => Ok(()),
            (
                ResolvedTy::Named {
                    builtin: Some(hew_types::BuiltinType::Vec),
                    args,
                    ..
                },
                SemWireKind::Vector(value),
            )
            | (
                ResolvedTy::Named {
                    builtin: Some(hew_types::BuiltinType::HashSet),
                    args,
                    ..
                },
                SemWireKind::Set(value),
            ) if args.len() == 1 => child(&args[0], value, records, enums),
            (
                ResolvedTy::Named {
                    builtin: Some(hew_types::BuiltinType::HashMap),
                    args,
                    ..
                },
                SemWireKind::Map { key, value },
            ) if args.len() == 2 => {
                child(&args[0], key, records, enums)?;
                child(&args[1], value, records, enums)
            }
            (
                ResolvedTy::Named {
                    builtin: Some(hew_types::BuiltinType::Option),
                    args,
                    ..
                },
                SemWireKind::Option {
                    shape,
                    none,
                    some,
                    value,
                },
            ) if args.len() == 1 => {
                let shape = enums
                    .get(shape.0 as usize)
                    .filter(|shape| shape.enum_ty == self.ty)
                    .ok_or_else(bad)?;
                if matches!(value.kind, SemWireKind::Option { .. })
                    || shape.variants.len() != 2
                    || none == some
                    || !shape
                        .variants
                        .get(*none as usize)
                        .is_some_and(|case| case.fields.is_empty())
                    || !shape
                        .variants
                        .get(*some as usize)
                        .is_some_and(|case| case.fields.len() == 1 && case.fields[0].ty == args[0])
                {
                    return Err(bad());
                }
                child(&args[0], value, records, enums)
            }
            (_, SemWireKind::Record { shape, fields }) => {
                let shape = records
                    .get(shape.0 as usize)
                    .filter(|shape| shape.aggregate_ty == self.ty)
                    .ok_or_else(bad)?;
                if fields.len() != shape.fields.len() {
                    return Err(bad());
                }
                let mut indices = std::collections::HashSet::new();
                let mut tags = std::collections::HashSet::new();
                for field in fields {
                    if !indices.insert(field.index) || !tags.insert(field.tag) {
                        return Err(bad());
                    }
                    let expected = shape.fields.get(field.index as usize).ok_or_else(bad)?;
                    if field.presence == WireFieldPresence::Optional
                        && !matches!(field.value.kind, SemWireKind::Option { .. })
                    {
                        return Err(bad());
                    }
                    child(&expected.ty, &field.value, records, enums)?;
                }
                Ok(())
            }
            (_, SemWireKind::Enum { shape, variants }) => {
                let shape = enums
                    .get(shape.0 as usize)
                    .filter(|shape| shape.enum_ty == self.ty)
                    .ok_or_else(bad)?;
                if variants.len() != shape.variants.len() {
                    return Err(bad());
                }
                let mut indices = std::collections::HashSet::new();
                let mut tags = std::collections::HashSet::new();
                for variant in variants {
                    if !indices.insert(variant.index) || !tags.insert(variant.tag) {
                        return Err(bad());
                    }
                    let expected = shape.variants.get(variant.index as usize).ok_or_else(bad)?;
                    if expected.fields.len() != variant.fields.len() {
                        return Err(bad());
                    }
                    for (expected, field) in expected.fields.iter().zip(&variant.fields) {
                        child(&expected.ty, field, records, enums)?;
                    }
                }
                Ok(())
            }
            _ => Err(bad()),
        }
    }
}

impl SemWirePlan {
    /// Visit every exact type needed by the codec, including nested payloads.
    pub fn visit_types(&self, visit: &mut impl FnMut(&ResolvedTy)) {
        visit(&self.ty);
        match &self.kind {
            SemWireKind::Scalar => {}
            SemWireKind::Vector(value)
            | SemWireKind::Set(value)
            | SemWireKind::Option { value, .. } => value.visit_types(visit),
            SemWireKind::Map { key, value } => {
                key.visit_types(visit);
                value.visit_types(visit);
            }
            SemWireKind::Record { fields, .. } => {
                for field in fields {
                    field.value.visit_types(visit);
                }
            }
            SemWireKind::Enum { variants, .. } => {
                for variant in variants {
                    for field in &variant.fields {
                        field.visit_types(visit);
                    }
                }
            }
        }
    }
}

/// Declaration-order Result cases selected for a text decoder's source result.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SemWireTextResult {
    pub shape: VariantShapeId,
    pub ok: u32,
    pub error: u32,
}

impl SemWirePlan {
    /// A text mapping must retain every field and variant after applying names.
    pub(crate) fn verify_text_names(
        &self,
        format: hew_types::WireTextFormat,
    ) -> Result<(), String> {
        let yaml = format == hew_types::WireTextFormat::Yaml;
        let format_name = if yaml { "YAML" } else { "JSON" };
        match &self.kind {
            SemWireKind::Scalar => {}
            SemWireKind::Vector(value)
            | SemWireKind::Set(value)
            | SemWireKind::Option { value, .. } => value.verify_text_names(format)?,
            SemWireKind::Map { key, value } => {
                key.verify_text_names(format)?;
                value.verify_text_names(format)?;
            }
            SemWireKind::Record { fields, .. } => {
                let mut names = std::collections::HashSet::new();
                for field in fields {
                    let name = if yaml {
                        &field.yaml_name
                    } else {
                        &field.json_name
                    };
                    if !names.insert(name) {
                        return Err(format!("wire {format_name} field name `{name}` is ambiguous after naming metadata"));
                    }
                    field.value.verify_text_names(format)?;
                }
            }
            SemWireKind::Enum { variants, .. } => {
                let mut names = std::collections::HashSet::new();
                for variant in variants {
                    let name = if yaml {
                        &variant.yaml_name
                    } else {
                        &variant.json_name
                    };
                    if !names.insert(name) {
                        return Err(format!("wire {format_name} variant name `{name}` is ambiguous after naming metadata"));
                    }
                    for field in &variant.fields {
                        field.verify_text_names(format)?;
                    }
                }
            }
        }
        Ok(())
    }
}
