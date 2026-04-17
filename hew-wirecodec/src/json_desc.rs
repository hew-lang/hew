//! `JsonCodecDesc` — descriptor-driven JSON emitter.
//!
//! Consumes a [`WireCodecPlan`] and produces a serde-serializable descriptor
//! (`JsonCodecDesc`) that records the JSON shape for each wire-type field.
//! The descriptor is the canonical record of "what `to_json` / `from_json`
//! do per field" — it replaces the hand-written `jsonKindOf` dispatch in
//! `hew-codegen/src/mlir/MLIRGenWire.cpp` line 127.
//!
//! The emission model mirrors the msgpack descriptor: a single exhaustive
//! match over [`PrimitiveWireKind`] turns each wire field into a typed
//! operation. Adding a new kind forces a compile error until wired up, which
//! is the structural defence against the silent-default class of bugs
//! (PRs #914, #944) that motivated this consolidation.

use serde::{Deserialize, Serialize};

use crate::kind::PrimitiveWireKind;
use crate::plan::{FieldPlan, IntegerBounds, WireCodecPlan, WireShape};

/// The JSON operation for a single field.
///
/// Every variant corresponds to one call into the runtime's JSON object API
/// (`hew_json_object_set_*` / `hew_json_object_get_*`). The runtime prefix is
/// chosen by the consumer (JSON uses `hew_json_`, YAML uses `hew_yaml_`); the
/// descriptor itself is format-agnostic at the op level.
///
/// WHY the struct-with-tag form: consistent with `MsgpackOp` and rmp-serde's
/// constraints. Keeps the on-wire shape uniform across descriptors so the C++
/// reader does not need a separate parsing strategy per descriptor.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "op", rename_all = "snake_case")]
pub enum JsonOp {
    /// Boolean scalar.
    SetBool,
    /// Signed or unsigned integer scalar.
    SetInt {
        /// Zero-extend to i64 before emitting (unsigned integer).
        unsigned: bool,
    },
    /// Floating-point scalar.
    SetFloat {
        /// Widen f32 → f64 before emitting (JSON has no f32 type).
        widen: bool,
    },
    /// UTF-8 string scalar.
    SetString,
    /// Byte string — emitted as base64-encoded JSON string.
    SetBytes,
    /// Duration — emitted as i64 nanoseconds.
    SetDuration,
    /// Emits the char as an unsigned integer codepoint in BMP range (0..=0xFFFF).
    /// Full Unicode scalar range (0..=0x10FFFF) is deferred — see `plan.rs` SHIM
    /// comment on `IntegerBounds::for_kind` Char arm.
    SetChar,
    /// Nested wire-type reference.
    Nested {
        /// Name of the nested wire-type referenced by this field.
        type_name: String,
    },
}

/// Serialized form of a single field's JSON operation.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct JsonFieldOp {
    /// Effective JSON object key (either the field's explicit `json_name`
    /// override or the struct-level naming case applied to `name`).
    pub key: String,
    /// Declared source-level field name (retained for diagnostics).
    pub name: String,
    /// Wire protocol field number (carried through for parity with msgpack).
    pub tag: u32,
    /// The structural operation for this field.
    pub op: JsonOp,
    /// Integer narrowing bounds, applied on decode.
    pub bounds: Option<IntegerBounds>,
    /// Field is `optional` — key may be absent on decode; encode may omit.
    pub is_optional: bool,
    /// Field is `repeated` — value is a JSON array of the scalar form.
    pub is_repeated: bool,
}

/// Top-level JSON codec descriptor for one wire type.
///
/// Serializes via `rmp-serde::to_vec_named` for parity with
/// [`crate::MsgpackCodecDesc`]. The descriptor is the byte payload consumed
/// by the C++ MLIR emitter in a follow-on commit; this PR lands the Rust
/// producer and the shadow-comparison oracle.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct JsonCodecDesc {
    /// Wire type name.
    pub name: String,
    /// Per-field operations in declared order (empty for enums).
    pub fields: Vec<JsonFieldOp>,
    /// Enum variant names in declared order (empty for structs).
    pub variants: Vec<String>,
}

impl JsonCodecDesc {
    /// Lower a [`WireCodecPlan`] to a [`JsonCodecDesc`].
    #[must_use]
    pub fn from_plan(plan: &WireCodecPlan) -> Self {
        let (fields, variants) = match &plan.shape {
            WireShape::Struct { fields } => (
                fields
                    .iter()
                    .filter(|f| !f.modifiers.is_reserved)
                    .map(json_field_op_from_plan)
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
        rmp_serde::to_vec_named(self).expect("JSON descriptor serialization never fails")
    }
}

fn json_field_op_from_plan(f: &FieldPlan) -> JsonFieldOp {
    JsonFieldOp {
        key: f.json_name.clone(),
        name: f.name.clone(),
        tag: f.number,
        op: json_op_for_kind(&f.kind),
        bounds: f.narrowing,
        is_optional: f.modifiers.is_optional,
        is_repeated: f.modifiers.is_repeated,
    }
}

/// Map a [`PrimitiveWireKind`] to its JSON dispatch operation.
///
/// Exhaustive over all variants — adding a new kind forces a compile error.
/// This is the single choke point that replaces `jsonKindOf` in
/// `hew-codegen/src/mlir/MLIRGenWire.cpp`; follow-on work wires the C++
/// consumer onto this descriptor.
///
/// `pub(crate)` so `yaml_desc` can reuse it without a forwarding wrapper.
/// Consumers outside this crate should use [`JsonCodecDesc::from_plan`] or
/// [`crate::YamlCodecDesc::from_plan`] instead.
#[must_use]
pub(crate) fn json_op_for_kind(kind: &PrimitiveWireKind) -> JsonOp {
    match kind {
        PrimitiveWireKind::Bool => JsonOp::SetBool,
        PrimitiveWireKind::I8
        | PrimitiveWireKind::I16
        | PrimitiveWireKind::I32
        | PrimitiveWireKind::I64 => JsonOp::SetInt { unsigned: false },
        PrimitiveWireKind::U8
        | PrimitiveWireKind::U16
        | PrimitiveWireKind::U32
        | PrimitiveWireKind::U64 => JsonOp::SetInt { unsigned: true },
        PrimitiveWireKind::F32 => JsonOp::SetFloat { widen: true },
        PrimitiveWireKind::F64 => JsonOp::SetFloat { widen: false },
        PrimitiveWireKind::String => JsonOp::SetString,
        PrimitiveWireKind::Bytes => JsonOp::SetBytes,
        PrimitiveWireKind::Duration => JsonOp::SetDuration,
        PrimitiveWireKind::Char => JsonOp::SetChar,
        PrimitiveWireKind::Nested(name) => JsonOp::Nested {
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
    fn bool_field_maps_to_set_bool() {
        let plan = plan_with_fields("A", vec![simple_field("b", 1, PrimitiveWireKind::Bool)]);
        let desc = JsonCodecDesc::from_plan(&plan);
        assert_eq!(desc.fields.len(), 1);
        assert_eq!(desc.fields[0].op, JsonOp::SetBool);
    }

    #[test]
    fn signed_integer_field_uses_signed_set_int() {
        let plan = plan_with_fields("A", vec![simple_field("x", 1, PrimitiveWireKind::I32)]);
        let desc = JsonCodecDesc::from_plan(&plan);
        assert_eq!(desc.fields[0].op, JsonOp::SetInt { unsigned: false });
        assert!(desc.fields[0].bounds.is_some());
    }

    #[test]
    fn unsigned_integer_field_uses_unsigned_set_int() {
        let plan = plan_with_fields("A", vec![simple_field("x", 1, PrimitiveWireKind::U32)]);
        let desc = JsonCodecDesc::from_plan(&plan);
        assert_eq!(desc.fields[0].op, JsonOp::SetInt { unsigned: true });
    }

    #[test]
    fn f32_field_widens_for_json() {
        let plan = plan_with_fields("A", vec![simple_field("x", 1, PrimitiveWireKind::F32)]);
        let desc = JsonCodecDesc::from_plan(&plan);
        assert_eq!(desc.fields[0].op, JsonOp::SetFloat { widen: true });
        assert!(desc.fields[0].bounds.is_none());
    }

    #[test]
    fn f64_field_does_not_widen() {
        let plan = plan_with_fields("A", vec![simple_field("x", 1, PrimitiveWireKind::F64)]);
        let desc = JsonCodecDesc::from_plan(&plan);
        assert_eq!(desc.fields[0].op, JsonOp::SetFloat { widen: false });
    }

    #[test]
    fn string_field_uses_set_string_with_no_bounds() {
        let plan = plan_with_fields("A", vec![simple_field("s", 1, PrimitiveWireKind::String)]);
        let desc = JsonCodecDesc::from_plan(&plan);
        assert_eq!(desc.fields[0].op, JsonOp::SetString);
        assert!(desc.fields[0].bounds.is_none());
    }

    #[test]
    fn bytes_field_uses_set_bytes() {
        let plan = plan_with_fields("A", vec![simple_field("b", 1, PrimitiveWireKind::Bytes)]);
        let desc = JsonCodecDesc::from_plan(&plan);
        assert_eq!(desc.fields[0].op, JsonOp::SetBytes);
    }

    #[test]
    fn duration_field_uses_set_duration() {
        let plan = plan_with_fields("A", vec![simple_field("d", 1, PrimitiveWireKind::Duration)]);
        let desc = JsonCodecDesc::from_plan(&plan);
        assert_eq!(desc.fields[0].op, JsonOp::SetDuration);
    }

    #[test]
    fn char_field_uses_set_char() {
        let plan = plan_with_fields("A", vec![simple_field("c", 1, PrimitiveWireKind::Char)]);
        let desc = JsonCodecDesc::from_plan(&plan);
        assert_eq!(desc.fields[0].op, JsonOp::SetChar);
    }

    /// `SetChar` must carry integer-range bounds. `MLIRGenWire.cpp:147-149`
    /// routes `Char → WireJsonKind::Integer` on the C++ side; without bounds
    /// the descriptor consumer has no signal that this is an integer op.
    #[test]
    fn char_field_carries_integer_bounds() {
        let plan = plan_with_fields("A", vec![simple_field("c", 1, PrimitiveWireKind::Char)]);
        let desc = JsonCodecDesc::from_plan(&plan);
        assert_eq!(desc.fields[0].op, JsonOp::SetChar);
        assert!(
            desc.fields[0].bounds.is_some(),
            "SetChar must carry integer bounds to match the C++ integer-codepoint path"
        );
        let b = desc.fields[0].bounds.unwrap();
        assert_eq!(b.min, 0, "char min codepoint is 0");
        // Plan uses U16-width bounds for msgpack parity. Assert the actual
        // contract pinned by IntegerBounds::for_kind Char arm.
        assert_eq!(
            b.max,
            u64::from(u16::MAX),
            "char uses u16 bounds (BMP only) for msgpack parity"
        );
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
        let desc = JsonCodecDesc::from_plan(&plan);
        assert_eq!(
            desc.fields[0].op,
            JsonOp::Nested {
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
        let desc = JsonCodecDesc::from_plan(&plan);
        assert!(desc.fields.is_empty());
        assert_eq!(desc.variants, vec!["A".to_string(), "B".to_string()]);
    }

    #[test]
    fn reserved_fields_are_excluded_from_descriptor() {
        let mut reserved = simple_field("_res", 2, PrimitiveWireKind::I32);
        reserved.modifiers.is_reserved = true;
        let plan = plan_with_fields(
            "A",
            vec![simple_field("x", 1, PrimitiveWireKind::I32), reserved],
        );
        let desc = JsonCodecDesc::from_plan(&plan);
        assert_eq!(desc.fields.len(), 1);
        assert_eq!(desc.fields[0].tag, 1);
    }

    #[test]
    fn field_key_uses_the_plans_json_name() {
        let mut f = simple_field("my_field", 1, PrimitiveWireKind::I32);
        f.json_name = "myField".into();
        let plan = plan_with_fields("A", vec![f]);
        let desc = JsonCodecDesc::from_plan(&plan);
        assert_eq!(desc.fields[0].key, "myField");
        assert_eq!(desc.fields[0].name, "my_field");
    }

    #[test]
    fn optional_and_repeated_modifiers_propagate() {
        let mut f = simple_field("x", 1, PrimitiveWireKind::I32);
        f.modifiers.is_optional = true;
        f.modifiers.is_repeated = true;
        let plan = plan_with_fields("A", vec![f]);
        let desc = JsonCodecDesc::from_plan(&plan);
        assert!(desc.fields[0].is_optional);
        assert!(desc.fields[0].is_repeated);
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
        let desc = JsonCodecDesc::from_plan(&plan);
        let bytes = desc.to_msgpack_bytes();
        let round: JsonCodecDesc = rmp_serde::from_slice(&bytes).expect("round-trip");
        assert_eq!(round, desc);
    }
}
