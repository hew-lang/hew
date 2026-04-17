//! Primitive wire-kind enum — the Rust mirror of the C++ `PrimitiveTypeKind` in
//! `hew-codegen/src/mlir/MLIRGenHelpers.h`.
//!
//! Unlike the C++ enum, `PrimitiveWireKind` forbids an `Unknown` variant by
//! construction — any conversion from a type name either yields a concrete
//! variant, a `Nested(String)` reference to a user-defined wire type, or an
//! explicit `KindError`. This turns the `llvm_unreachable("unhandled
//! PrimitiveTypeKind")` sites in `MLIRGenWire.cpp` into a compile-time check.
//!
//! LESSONS upheld: `exhaustive-traversal-and-lowering`, `serializer-fail-closed`.

use serde::{Deserialize, Serialize};

/// Exhaustive, non-`Unknown` mirror of C++ `PrimitiveTypeKind` augmented with a
/// `Nested` variant for user-defined wire-type references.
///
/// This is the single dispatch key for every codec descriptor. Adding a new
/// primitive to the checker forces a compile error here until it is wired up.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(tag = "kind", content = "name", rename_all = "snake_case")]
pub enum PrimitiveWireKind {
    /// Boolean scalar (`bool`).
    Bool,
    /// 8-bit signed integer (`i8`).
    I8,
    /// 16-bit signed integer (`i16`).
    I16,
    /// 32-bit signed integer (`i32`).
    I32,
    /// 64-bit signed integer (`i64` / `int` / `Int` / `isize`).
    I64,
    /// 8-bit unsigned integer (`u8` / `byte`).
    U8,
    /// 16-bit unsigned integer (`u16`).
    U16,
    /// 32-bit unsigned integer (`u32`).
    U32,
    /// 64-bit unsigned integer (`u64` / `uint` / `usize`).
    U64,
    /// Unicode scalar (`char` / `Char`).
    Char,
    /// 32-bit float (`f32`).
    F32,
    /// 64-bit float (`f64` / `float` / `Float`).
    F64,
    /// UTF-8 string (`string` / `String` / `str`).
    String,
    /// Ref-counted bytes (`bytes` / `Bytes`).
    Bytes,
    /// Duration in nanoseconds (`duration` / `Duration`).
    Duration,
    /// User-defined wire-type reference; the string is the canonical wire-type
    /// name (as it appears in `WireDecl::name`).
    Nested(String),
}

/// Error raised when a type name cannot be classified as a primitive or nested
/// reference. Replaces the silent `Unknown` fallthrough in the C++ dispatch.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum KindError {
    /// The type name was empty.
    Empty,
}

impl core::fmt::Display for KindError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::Empty => f.write_str("empty wire field type name"),
        }
    }
}

impl std::error::Error for KindError {}

impl PrimitiveWireKind {
    /// Parse a canonical wire-field type name into a [`PrimitiveWireKind`].
    ///
    /// Mirrors `primitiveTypeKind` in `MLIRGenHelpers.h`. Any name that is not
    /// a recognized primitive falls into the `Nested` branch — callers that
    /// need the nested reference to resolve to an actual `WireDecl` must
    /// validate separately (this enum is a purely structural classifier).
    ///
    /// # Errors
    ///
    /// Returns [`KindError::Empty`] if `name` is empty.
    pub fn from_type_name(name: &str) -> Result<Self, KindError> {
        if name.is_empty() {
            return Err(KindError::Empty);
        }
        let k = match name {
            "bool" | "Bool" => Self::Bool,
            "i8" => Self::I8,
            "i16" => Self::I16,
            "i32" => Self::I32,
            "i64" | "int" | "Int" | "isize" => Self::I64,
            "u8" | "byte" => Self::U8,
            "u16" => Self::U16,
            "u32" => Self::U32,
            "u64" | "uint" | "usize" => Self::U64,
            "char" | "Char" => Self::Char,
            "f32" => Self::F32,
            "f64" | "float" | "Float" => Self::F64,
            "string" | "String" | "str" => Self::String,
            "bytes" | "Bytes" => Self::Bytes,
            "duration" | "Duration" => Self::Duration,
            // WHY: any non-primitive name is treated as a reference to a
            // user-defined wire-type. Whether that reference resolves to an
            // actual WireDecl is the caller's responsibility (plan-build
            // enforces it via the resolved-types map).
            // WHEN: can be tightened once all nested wire references are
            // statically known at the plan-build site.
            // WHAT: see WireCodecPlan::build_struct for the resolution step.
            other => Self::Nested(other.to_string()),
        };
        Ok(k)
    }

    /// `true` if this kind is a fixed-width scalar that the msgpack wire
    /// protocol encodes as a varint.
    #[must_use]
    pub fn is_varint(&self) -> bool {
        matches!(
            self,
            Self::Bool
                | Self::I8
                | Self::I16
                | Self::I32
                | Self::I64
                | Self::U8
                | Self::U16
                | Self::U32
                | Self::U64
                | Self::Char
                | Self::Duration
        )
    }

    /// `true` if this kind is a signed integer (requires zigzag encoding for
    /// varint wire output).
    #[must_use]
    pub fn is_signed_integer(&self) -> bool {
        matches!(self, Self::I8 | Self::I16 | Self::I32 | Self::I64)
    }

    /// `true` if this kind is an unsigned integer or `Char` (zero-extend for
    /// JSON output, no zigzag).
    #[must_use]
    pub fn is_unsigned_integer(&self) -> bool {
        matches!(
            self,
            Self::U8 | Self::U16 | Self::U32 | Self::U64 | Self::Char
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn from_type_name_maps_int_alias_to_i64() {
        assert_eq!(
            PrimitiveWireKind::from_type_name("int").unwrap(),
            PrimitiveWireKind::I64
        );
        assert_eq!(
            PrimitiveWireKind::from_type_name("Int").unwrap(),
            PrimitiveWireKind::I64
        );
        assert_eq!(
            PrimitiveWireKind::from_type_name("isize").unwrap(),
            PrimitiveWireKind::I64
        );
    }

    #[test]
    fn from_type_name_maps_byte_alias_to_u8() {
        assert_eq!(
            PrimitiveWireKind::from_type_name("byte").unwrap(),
            PrimitiveWireKind::U8
        );
    }

    #[test]
    fn from_type_name_rejects_empty_string() {
        assert_eq!(PrimitiveWireKind::from_type_name(""), Err(KindError::Empty));
    }

    #[test]
    fn unknown_name_becomes_nested_reference() {
        let k = PrimitiveWireKind::from_type_name("MyWireStruct").unwrap();
        assert_eq!(k, PrimitiveWireKind::Nested("MyWireStruct".to_string()));
    }

    #[test]
    fn signed_integer_classifier_rejects_unsigned_kinds() {
        for k in [
            PrimitiveWireKind::U8,
            PrimitiveWireKind::U16,
            PrimitiveWireKind::U32,
            PrimitiveWireKind::U64,
        ] {
            assert!(!k.is_signed_integer(), "{k:?} should not be signed");
        }
    }

    #[test]
    fn varint_classifier_excludes_floats_and_strings() {
        for k in [
            PrimitiveWireKind::F32,
            PrimitiveWireKind::F64,
            PrimitiveWireKind::String,
            PrimitiveWireKind::Bytes,
        ] {
            assert!(!k.is_varint(), "{k:?} should not be varint");
        }
    }

    #[test]
    fn duration_is_varint_but_not_integer() {
        let k = PrimitiveWireKind::Duration;
        assert!(k.is_varint());
        assert!(!k.is_signed_integer());
        assert!(!k.is_unsigned_integer());
    }
}
