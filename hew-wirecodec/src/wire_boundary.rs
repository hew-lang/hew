//! Wire boundary errors and the `TypeDescriptorWireExt` extension trait.
//!
//! This module bridges [`hew_types::ResolvedTy`] (the canonical type
//! descriptor) with [`PrimitiveWireKind`] (the wire codec dispatch key). The
//! bridge lives here rather than in `hew-types` because `PrimitiveWireKind`
//! is owned by this crate and `hew-types` must not depend on `hew-wirecodec`.
//!
//! ## Stage 9 API surface
//!
//! The extension trait `TypeDescriptorWireExt` is the intended API for Stage 9
//! (Rust-side Hew MLIR textual emitter). Stage 14 should consume
//! `descriptor.wire_kind()` to map field types to their wire encoding; until
//! that migration is complete, this module is the authority and downstream
//! string matching remains a Stage 14 TODO. The return value drives codec
//! selection without any ad-hoc type-name string matching.
//!
//! LESSONS upheld: `boundary-fail-closed`, `exhaustive-traversal-and-lowering`,
//! `serializer-fail-closed`, `wire-contract-test-presence`.

use hew_types::ResolvedTy;

use crate::kind::PrimitiveWireKind;

/// Errors raised at the wire encode/decode boundary.
///
/// Every variant is fail-closed — callers MUST propagate errors rather than
/// silently skip or default.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum WireBoundaryError {
    /// An integer value exceeded the declared width's valid range.
    OutOfRange {
        /// The wire kind whose range was violated.
        kind: PrimitiveWireKind,
        /// The out-of-range value, as a signed 64-bit integer for display.
        value: i64,
    },
    /// A `Char` value fell in the surrogate range `[0xD800, 0xDFFF]`.
    InvalidScalar {
        /// The invalid Unicode scalar value.
        value: u32,
    },
    /// A `String` field contained bytes that are not valid UTF-8.
    InvalidUtf8,
    /// An `F32` or `F64` field contained a signalling NaN.
    SignallingNaN,
    /// A type that cannot cross the wire boundary was presented for encoding.
    NonWireType {
        /// Canonical string of the offending type.
        canonical: String,
    },
    /// A type name is not recognised as a primitive or valid nested reference.
    WireKindUnknown {
        /// The unrecognised type name.
        name: String,
    },
}

impl core::fmt::Display for WireBoundaryError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::OutOfRange { kind, value } => {
                write!(f, "value {value} out of range for {kind:?}")
            }
            Self::InvalidScalar { value } => {
                write!(f, "Unicode scalar 0x{value:X} is in the surrogate range")
            }
            Self::InvalidUtf8 => f.write_str("string bytes are not valid UTF-8"),
            Self::SignallingNaN => {
                f.write_str("signalling NaN is not permitted at the wire boundary")
            }
            Self::NonWireType { canonical } => {
                write!(f, "type `{canonical}` cannot cross the wire boundary")
            }
            Self::WireKindUnknown { name } => {
                write!(f, "type name `{name}` is not a recognised wire kind")
            }
        }
    }
}

impl std::error::Error for WireBoundaryError {}

/// Extension trait that maps a [`ResolvedTy`] descriptor to its wire encoding.
///
/// This is the **single authority** for `TypeDescriptor → PrimitiveWireKind`
/// dispatch. Stage 9 (MLIR textual emitter) uses this API; Stage 14 (wire
/// codec) is expected to migrate to it so no ad-hoc type-name string matching
/// remains downstream.
///
/// # Design note
///
/// `wire_kind()` returns `Err(WireBoundaryError::NonWireType)` for types that
/// must not cross the wire boundary (`Never`, `Function`, `Closure`,
/// `Pointer`). `Unit` maps to [`PrimitiveWireKind::Unit`] and is encoded as
/// msgpack nil. Composite types (`Tuple`, `Array`, `Slice`) and
/// named/trait-object types produce `Nested` or are handled recursively by the
/// caller.
///
/// `Unit` is intentionally part of the dispatch contract rather than a caller
/// special case: native-wire callers can use `wire_kind()` as the single
/// authority for primitive codec selection.
pub trait TypeDescriptorWireExt {
    /// Map this descriptor to its [`PrimitiveWireKind`].
    ///
    /// Returns `Ok(kind)` for every type that has a direct wire primitive
    /// representation. Returns `Err(WireBoundaryError::NonWireType)` for types
    /// that are wire-rejected (`Never`, `Function`, `Closure`, `Pointer`).
    /// Returns `Ok(Nested(canonical_string()))` for `Named` and `TraitObject`.
    ///
    /// Composite types (`Tuple`, `Array`, `Slice`) encode as arrays
    /// recursively but do not have a single `PrimitiveWireKind`; this function
    /// returns `Err(WireBoundaryError::NonWireType)` for them. The caller is
    /// responsible for recursing into element types.
    ///
    /// # Errors
    ///
    /// Returns [`WireBoundaryError::NonWireType`] for wire-rejected types.
    fn wire_kind(&self) -> Result<PrimitiveWireKind, WireBoundaryError>;
}

impl TypeDescriptorWireExt for ResolvedTy {
    fn wire_kind(&self) -> Result<PrimitiveWireKind, WireBoundaryError> {
        match self {
            ResolvedTy::Bool => Ok(PrimitiveWireKind::Bool),
            ResolvedTy::I8 => Ok(PrimitiveWireKind::I8),
            ResolvedTy::I16 => Ok(PrimitiveWireKind::I16),
            ResolvedTy::I32 => Ok(PrimitiveWireKind::I32),
            ResolvedTy::I64 => Ok(PrimitiveWireKind::I64),
            ResolvedTy::U8 => Ok(PrimitiveWireKind::U8),
            ResolvedTy::U16 => Ok(PrimitiveWireKind::U16),
            ResolvedTy::U32 => Ok(PrimitiveWireKind::U32),
            ResolvedTy::U64 => Ok(PrimitiveWireKind::U64),
            ResolvedTy::Isize => Ok(PrimitiveWireKind::Isize),
            ResolvedTy::Usize => Ok(PrimitiveWireKind::Usize),
            ResolvedTy::F32 => Ok(PrimitiveWireKind::F32),
            ResolvedTy::F64 => Ok(PrimitiveWireKind::F64),
            ResolvedTy::Char => Ok(PrimitiveWireKind::Char),
            ResolvedTy::String => Ok(PrimitiveWireKind::String),
            ResolvedTy::Bytes => Ok(PrimitiveWireKind::Bytes),
            ResolvedTy::Duration => Ok(PrimitiveWireKind::Duration),
            ResolvedTy::Unit => Ok(PrimitiveWireKind::Unit),
            ResolvedTy::Named { .. } | ResolvedTy::TraitObject { .. } => {
                Ok(PrimitiveWireKind::Nested(self.canonical_string()))
            }
            // Task<T> is compiler-internal and not wire-serialisable; task
            // handles cannot cross actor/wire boundaries in v0.5.
            ResolvedTy::Never
            | ResolvedTy::Function { .. }
            | ResolvedTy::Closure { .. }
            | ResolvedTy::Pointer { .. }
            | ResolvedTy::Tuple(_)
            | ResolvedTy::Array(_, _)
            | ResolvedTy::Slice(_)
            | ResolvedTy::Task(_) => Err(WireBoundaryError::NonWireType {
                canonical: self.canonical_string(),
            }),
        }
    }
}
