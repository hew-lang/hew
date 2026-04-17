//! `MsgpackCodecDesc` — descriptor-driven msgpack emitter.
//!
//! Consumes a [`WireCodecPlan`] and produces a serde-serializable descriptor
//! (`MsgpackCodecDesc`) that encodes to msgpack bytes via `rmp-serde`. This is
//! the single entry point for wire-type msgpack emission from the Rust side;
//! the descriptor is also the byte payload consumed by the MLIR-side reader.
//!
//! The descriptor carries all information the C++ consumer needs to mechanically
//! emit `Foo_encode` / `Foo_decode` helpers without re-inferring type shape.
//! Lane 7b stage 4 extends the C++ reader to consume these records; for now
//! this module is the new plan-driven path whose bytes are compared against
//! the legacy `rmp_serde::to_vec_named(&WireDecl)` output in the shadow test.

use serde::{Deserialize, Serialize};

use crate::kind::PrimitiveWireKind;
use crate::plan::{FieldPlan, IntegerBounds, WireCodecPlan, WireShape};

/// The msgpack wire operation for a single field.
///
/// Mirrors the `WireTypeInfo` struct in `hew-codegen/src/mlir/MLIRGenWire.cpp`
/// line 49 but expresses each dispatch arm as a typed variant so the emitter
/// cannot silently fall through. Adding a new `PrimitiveWireKind` variant
/// forces a compile error in `field_op_for_kind` until wired up.
///
/// WHY the struct-with-tag form: rmp-serde's default enum encoding rejects
/// internally tagged newtype variants carrying string payloads ("cannot
/// serialize tagged newtype variant"). Using struct variants everywhere keeps
/// the on-wire shape consistent and rmp-serde-compatible.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "op", rename_all = "snake_case")]
pub enum MsgpackOp {
    /// Variable-length integer; `zigzag=true` for signed integers.
    Varint {
        /// Apply zigzag encoding (signed integer).
        zigzag: bool,
        /// Zero-extend to i64 before encoding (unsigned integer).
        unsigned: bool,
    },
    /// Fixed 32-bit payload (`f32`).
    Fixed32,
    /// Fixed 64-bit payload (`f64`).
    Fixed64,
    /// UTF-8 length-delimited string.
    String,
    /// Length-delimited byte string.
    Bytes,
    /// Nested wire-type reference; `type_name` is the canonical nested type
    /// name (as it appears in `WireDecl::name`).
    Nested {
        /// Name of the nested wire-type referenced by this field.
        type_name: String,
    },
}

/// Serialized form of a single field's msgpack operation.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct MsgpackFieldOp {
    /// Wire protocol field number.
    pub tag: u32,
    /// The structural operation for this field.
    pub op: MsgpackOp,
    /// Integer narrowing bounds, applied on decode.
    pub bounds: Option<IntegerBounds>,
    /// Field modifiers (optional, repeated, deprecated).
    pub is_optional: bool,
    /// Whether the field is `repeated`.
    pub is_repeated: bool,
}

/// Top-level msgpack codec descriptor for one wire type.
///
/// Serializes as a named map via `rmp-serde::to_vec_named`, matching the
/// convention used for the parser's `WireDecl` (`rmp_serde::to_vec_named`
/// everywhere else).
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct MsgpackCodecDesc {
    /// Wire type name.
    pub name: String,
    /// Per-field operations in declared order.
    pub fields: Vec<MsgpackFieldOp>,
    /// Enum variant names in declared order (empty for structs).
    pub variants: Vec<String>,
}

impl MsgpackCodecDesc {
    /// Lower a [`WireCodecPlan`] to an [`MsgpackCodecDesc`].
    #[must_use]
    pub fn from_plan(plan: &WireCodecPlan) -> Self {
        let (fields, variants) = match &plan.shape {
            WireShape::Struct { fields } => (
                fields
                    .iter()
                    .filter(|f| !f.modifiers.is_reserved)
                    .map(field_op_from_plan)
                    .collect(),
                Vec::new(),
            ),
            WireShape::Enum { variants } => (
                Vec::new(),
                variants.iter().map(|v| v.name.clone()).collect(),
            ),
        };
        Self {
            name: plan.name.clone(),
            fields,
            variants,
        }
    }

    /// Encode this descriptor as msgpack bytes using named-field encoding.
    ///
    /// # Panics
    ///
    /// Panics if serialization fails; `rmp-serde` only fails on IO errors,
    /// which cannot occur when writing to an in-memory buffer.
    #[must_use]
    pub fn to_msgpack_bytes(&self) -> Vec<u8> {
        rmp_serde::to_vec_named(self).expect("msgpack descriptor serialization never fails")
    }
}

fn field_op_from_plan(f: &FieldPlan) -> MsgpackFieldOp {
    MsgpackFieldOp {
        tag: f.number,
        op: field_op_for_kind(&f.kind),
        bounds: f.narrowing,
        is_optional: f.modifiers.is_optional,
        is_repeated: f.modifiers.is_repeated,
    }
}

/// Map a [`PrimitiveWireKind`] to its msgpack dispatch operation.
///
/// Exhaustive over all variants — adding a new kind forces a compile error.
fn field_op_for_kind(kind: &PrimitiveWireKind) -> MsgpackOp {
    match kind {
        // Bool and Duration share the msgpack shape with zigzag-less, non-
        // unsigned varints — Bool is a 0/1 byte and Duration is an i64
        // nanosecond count that happens to always be non-negative in the
        // supported API surface.
        PrimitiveWireKind::Bool | PrimitiveWireKind::Duration => MsgpackOp::Varint {
            zigzag: false,
            unsigned: false,
        },
        PrimitiveWireKind::I8
        | PrimitiveWireKind::I16
        | PrimitiveWireKind::I32
        | PrimitiveWireKind::I64 => MsgpackOp::Varint {
            zigzag: true,
            unsigned: false,
        },
        PrimitiveWireKind::U8
        | PrimitiveWireKind::U16
        | PrimitiveWireKind::U32
        | PrimitiveWireKind::U64
        | PrimitiveWireKind::Char => MsgpackOp::Varint {
            zigzag: false,
            unsigned: true,
        },
        PrimitiveWireKind::F32 => MsgpackOp::Fixed32,
        PrimitiveWireKind::F64 => MsgpackOp::Fixed64,
        PrimitiveWireKind::String => MsgpackOp::String,
        PrimitiveWireKind::Bytes => MsgpackOp::Bytes,
        PrimitiveWireKind::Nested(name) => MsgpackOp::Nested {
            type_name: name.clone(),
        },
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::plan::{FieldModifiers, VariantPlan};

    fn plan_with_fields(name: &str, fields: Vec<FieldPlan>) -> WireCodecPlan {
        WireCodecPlan {
            name: name.to_string(),
            shape: WireShape::Struct { fields },
            json_case: None,
            yaml_case: None,
        }
    }

    fn simple_field(name: &str, number: u32, kind: PrimitiveWireKind) -> FieldPlan {
        let narrowing = IntegerBounds::for_kind(&kind);
        FieldPlan {
            name: name.to_string(),
            number,
            json_name: name.to_string(),
            yaml_name: name.to_string(),
            kind,
            modifiers: FieldModifiers::default(),
            narrowing,
        }
    }

    #[test]
    fn signed_integer_field_uses_zigzag_varint() {
        let plan = plan_with_fields("A", vec![simple_field("x", 1, PrimitiveWireKind::I32)]);
        let desc = MsgpackCodecDesc::from_plan(&plan);
        assert_eq!(desc.fields.len(), 1);
        assert_eq!(
            desc.fields[0].op,
            MsgpackOp::Varint {
                zigzag: true,
                unsigned: false
            }
        );
        assert!(desc.fields[0].bounds.is_some());
    }

    #[test]
    fn unsigned_integer_field_uses_unsigned_varint() {
        let plan = plan_with_fields("A", vec![simple_field("x", 1, PrimitiveWireKind::U32)]);
        let desc = MsgpackCodecDesc::from_plan(&plan);
        assert_eq!(
            desc.fields[0].op,
            MsgpackOp::Varint {
                zigzag: false,
                unsigned: true
            }
        );
    }

    #[test]
    fn string_field_uses_string_op_with_no_bounds() {
        let plan = plan_with_fields("A", vec![simple_field("s", 1, PrimitiveWireKind::String)]);
        let desc = MsgpackCodecDesc::from_plan(&plan);
        assert_eq!(desc.fields[0].op, MsgpackOp::String);
        assert!(desc.fields[0].bounds.is_none());
    }

    #[test]
    fn nested_field_preserves_reference_name() {
        let plan = plan_with_fields(
            "A",
            vec![simple_field(
                "sub",
                1,
                PrimitiveWireKind::Nested("B".to_string()),
            )],
        );
        let desc = MsgpackCodecDesc::from_plan(&plan);
        assert_eq!(
            desc.fields[0].op,
            MsgpackOp::Nested {
                type_name: "B".into()
            }
        );
    }

    #[test]
    fn enum_shape_produces_variants_and_no_fields() {
        let plan = WireCodecPlan {
            name: "E".into(),
            shape: WireShape::Enum {
                variants: vec![
                    VariantPlan { name: "A".into() },
                    VariantPlan { name: "B".into() },
                ],
            },
            json_case: None,
            yaml_case: None,
        };
        let desc = MsgpackCodecDesc::from_plan(&plan);
        assert!(desc.fields.is_empty());
        assert_eq!(desc.variants, vec!["A".to_string(), "B".to_string()]);
    }

    #[test]
    fn msgpack_bytes_round_trip_through_rmp_serde() {
        let plan = plan_with_fields(
            "Point",
            vec![
                simple_field("x", 1, PrimitiveWireKind::I64),
                simple_field("y", 2, PrimitiveWireKind::I64),
            ],
        );
        let desc = MsgpackCodecDesc::from_plan(&plan);
        let bytes = desc.to_msgpack_bytes();
        let round: MsgpackCodecDesc = rmp_serde::from_slice(&bytes).expect("round-trip");
        assert_eq!(round, desc);
    }
}
