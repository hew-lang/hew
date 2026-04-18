//! Neutral codec-op helper shared by JSON and YAML descriptors.
//!
//! Both `json_desc` and `yaml_desc` need to map a [`PrimitiveWireKind`] to
//! its JSON/YAML op. Keeping the function here — rather than in `json_desc` —
//! lets `yaml_desc` consume the canonical mapping without a layer inversion
//! (YAML depending on JSON) that would make the two formats harder to evolve
//! independently.
//!
//! If YAML ever adds format-specific ops (e.g. block vs flow style), this
//! module can be forked into a `json_op_for_kind` and `yaml_op_for_kind` pair
//! without touching the shared descriptor infrastructure.

use crate::json_desc::JsonOp;
use crate::kind::PrimitiveWireKind;

/// Map a [`PrimitiveWireKind`] to its JSON/YAML dispatch operation.
///
/// Exhaustive over all variants — adding a new kind forces a compile error.
/// This is the single choke point that will replace `jsonKindOf` in
/// `hew-codegen/src/mlir/MLIRGenWire.cpp` when the descriptor-driven consumer
/// lands (Phase M of the canonical Value plan, tracked in issue #1272).
///
/// `pub(crate)` — consumers outside this crate use descriptor `from_plan`
/// constructors; this helper is an implementation detail.
#[must_use]
pub(crate) fn op_for_kind(kind: &PrimitiveWireKind) -> JsonOp {
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
