//! `YamlCodecDesc` — descriptor-driven YAML emitter.
//!
//! YAML's structural shape for the subset Hew supports (primitives, nested
//! wire types, enum-as-string) is identical to JSON: both bind to a shared
//! runtime API of `object_set_*` / `object_get_*` calls keyed by string. The
//! op set is therefore reused from `json_desc`; the YAML descriptor differs
//! only in:
//!
//! - the effective object key (per-field `yaml_name`, not `json_name`)
//! - the top-level type name (distinct `YamlCodecDesc`) so downstream
//!   consumers can dispatch YAML-specific concerns without confusing it for
//!   the JSON descriptor at the byte boundary.
//!
//! This module is a thin re-projection of the plan onto [`YamlCodecDesc`].
//! The [`YamlOp`] alias keeps the op set in one place; any YAML-only behaviour
//! would fork the alias if and when it materialises.

use serde::{Deserialize, Serialize};

use crate::json_desc::{json_op_for_kind, JsonOp};
use crate::plan::{FieldPlan, IntegerBounds, WireCodecPlan, WireShape};

/// YAML field op — structurally identical to [`JsonOp`].
///
/// WHY a re-export alias: the two op sets are identical for every variant
/// Hew supports; forking them would be premature. If YAML grows a format-
/// specific op (e.g. block vs flow style), this alias is replaced by a full
/// `enum YamlOp` at that point — the `YamlCodecDesc` consumers are already
/// separated from `JsonCodecDesc` consumers, so the change is local.
pub type YamlOp = JsonOp;

/// Serialized form of a single field's YAML operation.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct YamlFieldOp {
    /// Effective YAML object key (either the field's explicit `yaml_name`
    /// override or the struct-level naming case applied to `name`).
    pub key: String,
    /// Declared source-level field name (retained for diagnostics).
    pub name: String,
    /// Wire protocol field number (carried through for parity with msgpack).
    pub tag: u32,
    /// The structural operation for this field.
    pub op: YamlOp,
    /// Integer narrowing bounds, applied on decode.
    pub bounds: Option<IntegerBounds>,
    /// Field is `optional` — key may be absent on decode; encode may omit.
    pub is_optional: bool,
    /// Field is `repeated` — value is a YAML sequence of the scalar form.
    pub is_repeated: bool,
}

/// Top-level YAML codec descriptor for one wire type.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct YamlCodecDesc {
    /// Wire type name.
    pub name: String,
    /// Per-field operations in declared order (empty for enums).
    pub fields: Vec<YamlFieldOp>,
    /// Enum variant names in declared order (empty for structs).
    pub variants: Vec<String>,
}

impl YamlCodecDesc {
    /// Lower a [`WireCodecPlan`] to a [`YamlCodecDesc`].
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
        rmp_serde::to_vec_named(self).expect("YAML descriptor serialization never fails")
    }
}

fn field_op_from_plan(f: &FieldPlan) -> YamlFieldOp {
    YamlFieldOp {
        key: f.yaml_name.clone(),
        name: f.name.clone(),
        tag: f.number,
        op: json_op_for_kind(&f.kind),
        bounds: f.narrowing,
        is_optional: f.modifiers.is_optional,
        is_repeated: f.modifiers.is_repeated,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::kind::PrimitiveWireKind;
    use crate::plan::VariantPlan;
    use crate::test_helpers::{plan_with_fields, simple_field};

    #[test]
    fn struct_shape_produces_one_field_op_per_field() {
        let plan = plan_with_fields(
            "Point",
            vec![
                simple_field("x", 1, PrimitiveWireKind::I64),
                simple_field("y", 2, PrimitiveWireKind::I64),
            ],
        );
        let desc = YamlCodecDesc::from_plan(&plan);
        assert_eq!(desc.fields.len(), 2);
        assert_eq!(desc.fields[0].tag, 1);
        assert_eq!(desc.fields[1].tag, 2);
    }

    #[test]
    fn yaml_descriptor_uses_yaml_name_as_key() {
        let mut f = simple_field("my_field", 1, PrimitiveWireKind::I32);
        f.yaml_name = "my-field".into();
        f.json_name = "myField".into();
        let plan = plan_with_fields("A", vec![f]);
        let desc = YamlCodecDesc::from_plan(&plan);
        assert_eq!(desc.fields[0].key, "my-field");
        assert_eq!(desc.fields[0].name, "my_field");
    }

    #[test]
    fn yaml_and_json_ops_agree_for_every_primitive() {
        // The alias contract: YAML op for every primitive must equal the JSON
        // op. If YAML ever forks, this test is the canary that tells us the
        // alias is no longer safe.
        use crate::json_desc::JsonCodecDesc;
        for (i, kind) in [
            PrimitiveWireKind::Bool,
            PrimitiveWireKind::I8,
            PrimitiveWireKind::I16,
            PrimitiveWireKind::I32,
            PrimitiveWireKind::I64,
            PrimitiveWireKind::U8,
            PrimitiveWireKind::U16,
            PrimitiveWireKind::U32,
            PrimitiveWireKind::U64,
            PrimitiveWireKind::F32,
            PrimitiveWireKind::F64,
            PrimitiveWireKind::String,
            PrimitiveWireKind::Bytes,
            PrimitiveWireKind::Duration,
            PrimitiveWireKind::Char,
            PrimitiveWireKind::Nested("Foo".to_string()),
        ]
        .into_iter()
        .enumerate()
        {
            #[expect(
                clippy::cast_possible_truncation,
                reason = "test vector indices fit in u32 trivially"
            )]
            let n = i as u32 + 1;
            let plan = plan_with_fields("A", vec![simple_field("f", n, kind.clone())]);
            let json = JsonCodecDesc::from_plan(&plan);
            let yaml = YamlCodecDesc::from_plan(&plan);
            assert_eq!(json.fields[0].op, yaml.fields[0].op, "op drift on {kind:?}");
        }
    }

    #[test]
    fn reserved_fields_are_excluded() {
        let mut reserved = simple_field("_res", 2, PrimitiveWireKind::I32);
        reserved.modifiers.is_reserved = true;
        let plan = plan_with_fields(
            "A",
            vec![simple_field("x", 1, PrimitiveWireKind::I32), reserved],
        );
        let desc = YamlCodecDesc::from_plan(&plan);
        assert_eq!(desc.fields.len(), 1);
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
        let desc = YamlCodecDesc::from_plan(&plan);
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
        let desc = YamlCodecDesc::from_plan(&plan);
        let bytes = desc.to_msgpack_bytes();
        let round: YamlCodecDesc = rmp_serde::from_slice(&bytes).expect("round-trip");
        assert_eq!(round, desc);
    }

    #[test]
    fn yaml_nested_reference_preserved() {
        let plan = plan_with_fields(
            "Outer",
            vec![simple_field(
                "inner",
                1,
                PrimitiveWireKind::Nested("Inner".to_string()),
            )],
        );
        let desc = YamlCodecDesc::from_plan(&plan);
        assert_eq!(
            desc.fields[0].op,
            YamlOp::Nested {
                type_name: "Inner".into()
            }
        );
    }

    #[test]
    fn yaml_modifier_propagation() {
        let mut f = simple_field("items", 1, PrimitiveWireKind::I32);
        f.modifiers.is_optional = true;
        f.modifiers.is_repeated = true;
        let plan = plan_with_fields("A", vec![f]);
        let desc = YamlCodecDesc::from_plan(&plan);
        assert!(desc.fields[0].is_optional);
        assert!(desc.fields[0].is_repeated);
    }
}
