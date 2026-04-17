//! Shared test helpers for descriptor unit tests.
//!
//! Gated by `#[cfg(test)]` — compiled only in test builds. All symbols are
//! `pub(crate)` so the three descriptor test modules (`json_desc`, `yaml_desc`,
//! `msgpack_desc`) can `use crate::test_helpers::{plan_with_fields, simple_field}`
//! rather than each carrying a verbatim copy.

use crate::kind::PrimitiveWireKind;
use crate::plan::{FieldModifiers, FieldPlan, IntegerBounds, WireCodecPlan, WireShape};

pub(crate) fn plan_with_fields(name: &str, fields: Vec<FieldPlan>) -> WireCodecPlan {
    WireCodecPlan {
        name: name.to_string(),
        shape: WireShape::Struct { fields },
        json_case: None,
        yaml_case: None,
    }
}

pub(crate) fn simple_field(name: &str, number: u32, kind: PrimitiveWireKind) -> FieldPlan {
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
