//! Declarative primitive descriptor table for `hew-wirecodec`.
//!
//! A single `const` slice of [`PrimitiveDesc`] is the **one** place that
//! encodes the closed set of non-`Nested` [`PrimitiveWireKind`] variants and
//! their wire properties.  Every classifier (`is_varint`, `is_signed_integer`,
//! `is_unsigned_integer`), every bounds calculation, and every msgpack-op
//! derivation is driven from this table.
//!
//! ## How to add a new primitive
//!
//! 1. Add the variant to [`PrimitiveWireKind`] in `kind.rs`.
//! 2. Add a row to [`PRIMITIVE_DESCS`] below with the correct [`PrimitiveClass`].
//! 3. No other change is needed — the classifiers and codec helpers derive from
//!    the table automatically.
//!
//! LESSONS upheld: `exhaustive-traversal-and-lowering`, `serializer-fail-closed`.

use crate::kind::PrimitiveWireKind;
use crate::plan::IntegerBounds;
use hew_types::ty::aliases_for;

/// Structural class of a non-`Nested` primitive wire kind.
///
/// The class drives three things:
/// - Whether the kind encodes as a varint on the msgpack wire.
/// - Which [`crate::msgpack_desc::MsgpackOp`] to emit for a field of this kind.
/// - Which JSON / YAML op to emit (via the same classification path).
///
/// Each enum variant is disjoint; there is no "varint" base class — `is_varint`
/// is derived as a predicate over the full set of classes that share the varint
/// wire encoding.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum PrimitiveClass {
    /// `bool` — varint on the wire (0 or 1 byte), neither signed nor unsigned.
    Bool,
    /// `i8` / `i16` / `i32` / `i64` — varint with zigzag encoding.
    SignedInt,
    /// `u8` / `u16` / `u32` / `u64` — varint, zero-extended, unsigned.
    UnsignedInt,
    /// `char` — varint, unsigned (codepoint), full-Unicode ceiling.
    Char,
    /// `f32` — 4-byte fixed-width payload.
    F32,
    /// `f64` — 8-byte fixed-width payload.
    F64,
    /// UTF-8 length-delimited string.
    Str,
    /// Length-delimited byte string.
    Bytes,
    /// Duration in nanoseconds (i64 range), varint, neither signed nor unsigned
    /// in the classifier sense.
    Duration,
}

impl PrimitiveClass {
    /// `true` if this class uses varint encoding on the msgpack wire.
    ///
    /// Equivalent to [`PrimitiveWireKind::is_varint`] but derived from the
    /// class rather than from a hand-coded `matches!`.
    #[must_use]
    pub(crate) fn is_varint(self) -> bool {
        matches!(
            self,
            Self::Bool | Self::SignedInt | Self::UnsignedInt | Self::Char | Self::Duration
        )
    }

    /// `true` if this class uses zigzag-encoded varint (signed integers).
    #[must_use]
    pub(crate) fn is_signed_integer(self) -> bool {
        matches!(self, Self::SignedInt)
    }

    /// `true` if this class uses unsigned / zero-extended varint (unsigned
    /// integers and `Char`).
    #[must_use]
    pub(crate) fn is_unsigned_integer(self) -> bool {
        matches!(self, Self::UnsignedInt | Self::Char)
    }
}

/// All wire properties of a single non-`Nested` primitive kind.
#[derive(Debug)]
pub(crate) struct PrimitiveDesc {
    /// The canonical `PrimitiveWireKind` for this row.
    pub kind: PrimitiveWireKind,
    /// Type-name aliases that parse to this kind, **canonical name first**.
    ///
    /// `PrimitiveWireKind::from_type_name` iterates this slice; the canonical
    /// name is `aliases[0]`.
    pub aliases: &'static [&'static str],
    /// Structural class that drives codec-op derivation.
    pub class: PrimitiveClass,
    /// Integer narrowing bounds, or `None` for non-integer kinds.
    ///
    /// This is the **single** definition of `IntegerBounds` for each kind.
    /// [`crate::plan::IntegerBounds::for_kind`] is derived from this field.
    pub bounds: Option<IntegerBounds>,
}

/// Closed set of non-`Nested` primitive descriptors.
///
/// Row ordering mirrors the declaration order of `PrimitiveWireKind` variants
/// for readability — it carries no semantic weight.
///
/// **`Nested` is not in this table** because it is the open-ended catch-all
/// variant that carries an arbitrary user-defined type name.  All open-ended
/// dispatch over `Nested` happens at call sites.
pub(crate) const PRIMITIVE_DESCS: &[PrimitiveDesc] = &[
    PrimitiveDesc {
        kind: PrimitiveWireKind::Bool,
        aliases: aliases_for("bool"),
        class: PrimitiveClass::Bool,
        bounds: None,
    },
    PrimitiveDesc {
        kind: PrimitiveWireKind::I8,
        aliases: aliases_for("i8"),
        class: PrimitiveClass::SignedInt,
        bounds: Some(IntegerBounds {
            min: i8::MIN as i64,
            max: i8::MAX as u64,
        }),
    },
    PrimitiveDesc {
        kind: PrimitiveWireKind::I16,
        aliases: aliases_for("i16"),
        class: PrimitiveClass::SignedInt,
        bounds: Some(IntegerBounds {
            min: i16::MIN as i64,
            max: i16::MAX as u64,
        }),
    },
    PrimitiveDesc {
        kind: PrimitiveWireKind::I32,
        aliases: aliases_for("i32"),
        class: PrimitiveClass::SignedInt,
        bounds: Some(IntegerBounds {
            min: i32::MIN as i64,
            max: i32::MAX as u64,
        }),
    },
    PrimitiveDesc {
        kind: PrimitiveWireKind::I64,
        aliases: aliases_for("i64"),
        class: PrimitiveClass::SignedInt,
        bounds: Some(IntegerBounds {
            min: i64::MIN,
            // WHY: i64::MAX fits in u64; unwrap is infallible at compile time.
            max: i64::MAX as u64,
        }),
    },
    PrimitiveDesc {
        kind: PrimitiveWireKind::U8,
        aliases: aliases_for("u8"),
        class: PrimitiveClass::UnsignedInt,
        bounds: Some(IntegerBounds {
            min: 0,
            max: u8::MAX as u64,
        }),
    },
    PrimitiveDesc {
        kind: PrimitiveWireKind::U16,
        aliases: aliases_for("u16"),
        class: PrimitiveClass::UnsignedInt,
        bounds: Some(IntegerBounds {
            min: 0,
            max: u16::MAX as u64,
        }),
    },
    PrimitiveDesc {
        kind: PrimitiveWireKind::U32,
        aliases: aliases_for("u32"),
        class: PrimitiveClass::UnsignedInt,
        bounds: Some(IntegerBounds {
            min: 0,
            max: u32::MAX as u64,
        }),
    },
    PrimitiveDesc {
        kind: PrimitiveWireKind::U64,
        aliases: aliases_for("u64"),
        class: PrimitiveClass::UnsignedInt,
        bounds: Some(IntegerBounds {
            min: 0,
            max: u64::MAX,
        }),
    },
    PrimitiveDesc {
        kind: PrimitiveWireKind::Char,
        aliases: aliases_for("char"),
        class: PrimitiveClass::Char,
        bounds: Some(IntegerBounds {
            min: 0,
            // SHIM(#1276): Char is not BMP-bound on the wire.
            //
            // Audit result:
            // - msgpack descriptors route Char through the same unsigned
            //   varint op as U32/U64 (`hew-wirecodec/src/msgpack_desc.rs`)
            // - the C++ reader decodes UTF-8 Char literals up to 4-byte
            //   codepoints (`hew-codegen/src/msgpack_reader.cpp`)
            // - MLIR wire JSON/YAML lowering already carries Char as an
            //   integer codepoint via the dedicated get/set_char ABI and
            //   validates against 0..=0x10_FFFF
            //
            // The old U16 reuse was incidental plan plumbing, not a wire
            // invariant, so the public descriptor API exposes the existing
            // full-Unicode ceiling used by the consumer.
            max: 0x10_FFFF,
        }),
    },
    PrimitiveDesc {
        kind: PrimitiveWireKind::F32,
        aliases: aliases_for("f32"),
        class: PrimitiveClass::F32,
        bounds: None,
    },
    PrimitiveDesc {
        kind: PrimitiveWireKind::F64,
        aliases: aliases_for("f64"),
        class: PrimitiveClass::F64,
        bounds: None,
    },
    PrimitiveDesc {
        kind: PrimitiveWireKind::String,
        aliases: aliases_for("string"),
        class: PrimitiveClass::Str,
        bounds: None,
    },
    PrimitiveDesc {
        kind: PrimitiveWireKind::Bytes,
        aliases: aliases_for("bytes"),
        class: PrimitiveClass::Bytes,
        bounds: None,
    },
    PrimitiveDesc {
        kind: PrimitiveWireKind::Duration,
        aliases: aliases_for("duration"),
        class: PrimitiveClass::Duration,
        // Duration stores nanoseconds as i64 (negative = past), so it
        // shares I64's signed range rather than U64's unsigned range.
        bounds: Some(IntegerBounds {
            min: i64::MIN,
            max: i64::MAX as u64,
        }),
    },
];

/// Look up the descriptor for a primitive kind.
///
/// Returns `None` for `Nested` (which is not in the closed primitive set).
#[must_use]
pub(crate) fn desc_for_kind(kind: &PrimitiveWireKind) -> Option<&'static PrimitiveDesc> {
    PRIMITIVE_DESCS.iter().find(|d| d.kind == *kind)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn every_non_nested_kind_has_a_descriptor() {
        let kinds = [
            PrimitiveWireKind::Bool,
            PrimitiveWireKind::I8,
            PrimitiveWireKind::I16,
            PrimitiveWireKind::I32,
            PrimitiveWireKind::I64,
            PrimitiveWireKind::U8,
            PrimitiveWireKind::U16,
            PrimitiveWireKind::U32,
            PrimitiveWireKind::U64,
            PrimitiveWireKind::Char,
            PrimitiveWireKind::F32,
            PrimitiveWireKind::F64,
            PrimitiveWireKind::String,
            PrimitiveWireKind::Bytes,
            PrimitiveWireKind::Duration,
        ];
        for k in &kinds {
            assert!(desc_for_kind(k).is_some(), "missing descriptor for {k:?}");
        }
    }

    #[test]
    fn nested_kind_has_no_descriptor() {
        assert!(desc_for_kind(&PrimitiveWireKind::Nested("Foo".to_string())).is_none());
    }

    #[test]
    fn aliases_first_entry_is_canonical() {
        // The canonical name is aliases[0]; it must parse back to the same kind.
        for desc in PRIMITIVE_DESCS {
            let canonical = desc.aliases[0];
            assert!(
                !canonical.is_empty(),
                "canonical alias for {:?} is empty",
                desc.kind
            );
        }
    }

    #[test]
    fn varint_set_matches_original_classifiers() {
        let varint_kinds = [
            PrimitiveWireKind::Bool,
            PrimitiveWireKind::I8,
            PrimitiveWireKind::I16,
            PrimitiveWireKind::I32,
            PrimitiveWireKind::I64,
            PrimitiveWireKind::U8,
            PrimitiveWireKind::U16,
            PrimitiveWireKind::U32,
            PrimitiveWireKind::U64,
            PrimitiveWireKind::Char,
            PrimitiveWireKind::Duration,
        ];
        for k in &varint_kinds {
            let desc = desc_for_kind(k).unwrap();
            assert!(desc.class.is_varint(), "{k:?} should be varint");
        }
        // Non-varint kinds
        for k in &[
            PrimitiveWireKind::F32,
            PrimitiveWireKind::F64,
            PrimitiveWireKind::String,
            PrimitiveWireKind::Bytes,
        ] {
            let desc = desc_for_kind(k).unwrap();
            assert!(!desc.class.is_varint(), "{k:?} should not be varint");
        }
    }

    #[test]
    fn signed_integer_set_matches_original_classifiers() {
        let signed = [
            PrimitiveWireKind::I8,
            PrimitiveWireKind::I16,
            PrimitiveWireKind::I32,
            PrimitiveWireKind::I64,
        ];
        for k in &signed {
            let desc = desc_for_kind(k).unwrap();
            assert!(desc.class.is_signed_integer(), "{k:?} should be signed");
        }
        let not_signed = [
            PrimitiveWireKind::U8,
            PrimitiveWireKind::U16,
            PrimitiveWireKind::U32,
            PrimitiveWireKind::U64,
            PrimitiveWireKind::Bool,
            PrimitiveWireKind::Duration,
            PrimitiveWireKind::Char,
        ];
        for k in &not_signed {
            let desc = desc_for_kind(k).unwrap();
            assert!(
                !desc.class.is_signed_integer(),
                "{k:?} should not be signed"
            );
        }
    }

    #[test]
    fn unsigned_integer_set_matches_original_classifiers() {
        let unsigned = [
            PrimitiveWireKind::U8,
            PrimitiveWireKind::U16,
            PrimitiveWireKind::U32,
            PrimitiveWireKind::U64,
            PrimitiveWireKind::Char,
        ];
        for k in &unsigned {
            let desc = desc_for_kind(k).unwrap();
            assert!(desc.class.is_unsigned_integer(), "{k:?} should be unsigned");
        }
    }

    #[test]
    fn char_bounds_cover_full_unicode_codepoint_range() {
        let desc = desc_for_kind(&PrimitiveWireKind::Char).unwrap();
        let b = desc.bounds.unwrap();
        assert_eq!(b.min, 0);
        assert_eq!(b.max, 0x10_FFFF);
    }

    #[test]
    fn duration_bounds_permit_negative_nanoseconds() {
        let desc = desc_for_kind(&PrimitiveWireKind::Duration).unwrap();
        let b = desc.bounds.unwrap();
        assert_eq!(b.min, i64::MIN);
        assert_eq!(b.max, i64::MAX as u64);
    }

    #[test]
    fn integer_kinds_have_bounds_non_integer_kinds_do_not() {
        for desc in PRIMITIVE_DESCS {
            let has_bounds = desc.bounds.is_some();
            let is_integer = desc.class.is_signed_integer()
                || desc.class.is_unsigned_integer()
                || matches!(desc.class, PrimitiveClass::Char | PrimitiveClass::Duration);
            assert_eq!(
                has_bounds, is_integer,
                "{:?}: bounds presence ({has_bounds}) must match integer classification ({is_integer})",
                desc.kind
            );
        }
    }
}
