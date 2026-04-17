//! Canonical wire value — a runtime-typed value that pairs with a
//! [`WireCodecPlan`].
//!
//! [`WireValue`] is the single type that carries a decoded or in-flight wire
//! value across the codec boundary. Having one canonical carrier avoids
//! ad-hoc `Vec<u8>` / `serde_json::Value` conversions in test helpers and
//! in the upcoming triple-roundtrip harness (Lane 7 Stage 7).
//!
//! Design notes:
//! - `Debug` and `Clone` are derived; `PartialEq` / `Eq` are NOT derived at
//!   the enum level because `F32` and `F64` do not implement `Eq`.  Callers
//!   that need equality must use [`f32::to_bits`] / [`f64::to_bits`] for the
//!   float arms.
//! - `Duration` stores nanoseconds as `i64` (matching the msgpack wire
//!   encoding; negative means "time in the past").
//!
//! LESSONS upheld: `type-info-survival`, `exhaustive-traversal-and-lowering`.

use crate::kind::PrimitiveWireKind;
use crate::plan::{WireCodecPlan, WireShape};

/// A runtime-typed value at the wire boundary.
///
/// Each variant corresponds 1-to-1 to a [`PrimitiveWireKind`]. Struct and
/// Enum variants carry the type name so that consumers can resolve the value
/// against a [`WireCodecPlan`] without external context.
#[derive(Debug, Clone)]
pub enum WireValue {
    /// Boolean scalar.
    Bool(bool),
    /// 8-bit signed integer.
    I8(i8),
    /// 16-bit signed integer.
    I16(i16),
    /// 32-bit signed integer.
    I32(i32),
    /// 64-bit signed integer.
    I64(i64),
    /// 8-bit unsigned integer.
    U8(u8),
    /// 16-bit unsigned integer.
    U16(u16),
    /// 32-bit unsigned integer.
    U32(u32),
    /// 64-bit unsigned integer.
    U64(u64),
    /// Unicode scalar value.
    Char(char),
    /// 32-bit float. **Not `Eq`** — use [`f32::to_bits`] for equality.
    F32(f32),
    /// 64-bit float. **Not `Eq`** — use [`f64::to_bits`] for equality.
    F64(f64),
    /// UTF-8 string.
    String(String),
    /// Raw byte sequence.
    Bytes(Vec<u8>),
    /// Duration in nanoseconds (negative = past; matches msgpack encoding).
    Duration(i64),
    /// A struct-typed value. `fields` is ordered by declaration, not by name.
    Struct {
        /// The canonical wire-type name (matches `WireCodecPlan::name`).
        type_name: String,
        /// Field values as `(field_name, value)` pairs.
        fields: Vec<(String, WireValue)>,
    },
    /// A unit-enum variant.
    Enum {
        /// The canonical wire-type name (matches `WireCodecPlan::name`).
        type_name: String,
        /// The selected variant name.
        variant: String,
    },
}

impl WireValue {
    /// The [`PrimitiveWireKind`] that classifies this value.
    ///
    /// For `Struct` and `Enum`, returns `Nested(type_name.clone())`.
    #[must_use]
    pub fn kind(&self) -> PrimitiveWireKind {
        match self {
            Self::Bool(_) => PrimitiveWireKind::Bool,
            Self::I8(_) => PrimitiveWireKind::I8,
            Self::I16(_) => PrimitiveWireKind::I16,
            Self::I32(_) => PrimitiveWireKind::I32,
            Self::I64(_) => PrimitiveWireKind::I64,
            Self::U8(_) => PrimitiveWireKind::U8,
            Self::U16(_) => PrimitiveWireKind::U16,
            Self::U32(_) => PrimitiveWireKind::U32,
            Self::U64(_) => PrimitiveWireKind::U64,
            Self::Char(_) => PrimitiveWireKind::Char,
            Self::F32(_) => PrimitiveWireKind::F32,
            Self::F64(_) => PrimitiveWireKind::F64,
            Self::String(_) => PrimitiveWireKind::String,
            Self::Bytes(_) => PrimitiveWireKind::Bytes,
            Self::Duration(_) => PrimitiveWireKind::Duration,
            Self::Struct { type_name, .. } | Self::Enum { type_name, .. } => {
                PrimitiveWireKind::Nested(type_name.clone())
            }
        }
    }

    /// Shallow structural conformance check against a [`WireCodecPlan`].
    ///
    /// Returns `true` when:
    /// - For `Struct`: `plan.name == type_name` **and** `plan.shape` is
    ///   `WireShape::Struct`.
    /// - For `Enum`: `plan.name == type_name` **and** `plan.shape` is
    ///   `WireShape::Enum` **and** the variant name exists in `plan.variants`.
    /// - For all primitive variants: `false` — plans model top-level named
    ///   wire types; a bare primitive is not a plan subject.
    ///
    /// "Shallow" means field values are not recursed into; only the top-level
    /// shape and variant membership are verified.
    #[must_use]
    pub fn conforms_to_plan(&self, plan: &WireCodecPlan) -> bool {
        match self {
            Self::Struct { type_name, .. } => {
                plan.name == *type_name && matches!(plan.shape, WireShape::Struct { .. })
            }
            Self::Enum { type_name, variant } => {
                if plan.name != *type_name {
                    return false;
                }
                match &plan.shape {
                    WireShape::Enum { variants } => variants.iter().any(|v| v.name == *variant),
                    WireShape::Struct { .. } => false,
                }
            }
            // Primitives are not plan subjects.
            _ => false,
        }
    }
}
