//! `WireCodecPlan` — the single choke point for wire-type codec emission.
//!
//! Every wire declaration is lowered once into a plan that carries:
//! - per-field ordering, field numbers, and modifiers
//! - per-field `PrimitiveWireKind` (fail-closed; unresolved → error)
//! - per-field integer narrowing bounds
//! - struct-level JSON / YAML naming-case overrides
//!
//! The plan is data: it is consumed by codec descriptors (msgpack, JSON, YAML)
//! to produce target-specific byte or text output. No codec dispatches on raw
//! type-name strings; every dispatch goes through `PrimitiveWireKind`.

use hew_parser::ast::{NamingCase, WireDecl, WireDeclKind, WireFieldDecl};
use serde::{Deserialize, Serialize};

use crate::kind::{KindError, PrimitiveWireKind};
use crate::primitives::desc_for_kind;

/// Inclusive integer bounds for narrowing guards (mirrors the helpers emitted
/// by `hew-astgen/src/special_cases.rs:1173` for the Rust→C++ boundary).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct IntegerBounds {
    /// Minimum value allowed at the decode boundary.
    pub min: i64,
    /// Maximum value allowed at the decode boundary.
    pub max: u64,
}

impl IntegerBounds {
    /// Bounds for the given primitive integer kind, or `None` if the kind is
    /// not an integer type.
    ///
    /// Derived from the canonical bounds in [`crate::primitives::PRIMITIVE_DESCS`].
    #[must_use]
    pub fn for_kind(kind: &PrimitiveWireKind) -> Option<Self> {
        desc_for_kind(kind)?.bounds
    }
}

/// Per-field modifier flags mirroring `WireFieldMeta` at the parser boundary.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize)]
#[expect(
    clippy::struct_excessive_bools,
    reason = "mirrors wire format field modifier set (WireFieldMeta)"
)]
pub struct FieldModifiers {
    /// `optional` modifier — field may be omitted from the encoded form.
    pub is_optional: bool,
    /// `repeated` modifier — zero-or-more instances under one field number.
    pub is_repeated: bool,
    /// `deprecated` modifier — field is still decoded but encoders may skip.
    pub is_deprecated: bool,
    /// `reserved` modifier — field number is reserved, not emitted.
    pub is_reserved: bool,
    /// Schema version that introduced this field (from `since N`).
    pub since: Option<u32>,
}

/// A single wire-type field, lowered from `WireFieldDecl` into a plan-level
/// record that codec descriptors consume without re-inferring.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct FieldPlan {
    /// Declared source-level field name (used as the default object key).
    pub name: String,
    /// Wire-protocol field number (auto-assigned or explicit via `@N`).
    pub number: u32,
    /// Effective JSON object key (per-field override or name).
    pub json_name: String,
    /// Effective YAML object key (per-field override or name).
    pub yaml_name: String,
    /// Structural wire kind — exhaustive, non-`Unknown`.
    pub kind: PrimitiveWireKind,
    /// Modifier flags carried verbatim from the parser.
    pub modifiers: FieldModifiers,
    /// Integer narrowing bounds when `kind` is an integer; `None` otherwise.
    pub narrowing: Option<IntegerBounds>,
}

/// A single enum variant plan entry.
///
/// Only the name is retained today; payload variants are represented by their
/// name plus a follow-up descriptor in later stages. Lane 7b extends this.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct VariantPlan {
    /// Variant name.
    pub name: String,
}

/// Top-level shape of the wire type — struct vs enum.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "shape", rename_all = "snake_case")]
pub enum WireShape {
    /// Struct shape: ordered fields, each with a number.
    Struct {
        /// Fields in declared order.
        fields: Vec<FieldPlan>,
    },
    /// Enum shape: ordered variants.
    Enum {
        /// Variants in declared order.
        variants: Vec<VariantPlan>,
    },
}

/// The unified wire-codec plan. Exactly one plan per `#[wire]`-annotated type.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct WireCodecPlan {
    /// User-declared wire-type name.
    pub name: String,
    /// Structural shape (struct or enum).
    pub shape: WireShape,
    /// Struct-level JSON naming case override, if any.
    pub json_case: Option<NamingCase>,
    /// Struct-level YAML naming case override, if any.
    pub yaml_case: Option<NamingCase>,
}

impl WireCodecPlan {
    /// Return the struct fields if this plan is a struct; `None` otherwise.
    #[must_use]
    pub fn fields(&self) -> Option<&[FieldPlan]> {
        match &self.shape {
            WireShape::Struct { fields } => Some(fields),
            WireShape::Enum { .. } => None,
        }
    }

    /// Return the enum variants if this plan is an enum; `None` otherwise.
    #[must_use]
    pub fn variants(&self) -> Option<&[VariantPlan]> {
        match &self.shape {
            WireShape::Enum { variants } => Some(variants),
            WireShape::Struct { .. } => None,
        }
    }

    /// Fold the plan's shape into a `(fields, variants)` pair by applying
    /// `on_field` to every non-reserved struct field, or collecting variant
    /// names for an enum.
    ///
    /// This is the single traversal helper shared by all codec descriptors
    /// (`JsonCodecDesc`, `YamlCodecDesc`, `MsgpackCodecDesc`) so that the
    /// reserved-field filter and shape dispatch never diverge.
    #[must_use]
    pub fn fold_shape<T, F>(&self, on_field: F) -> (Vec<T>, Vec<String>)
    where
        F: Fn(&FieldPlan) -> T,
    {
        match &self.shape {
            WireShape::Struct { fields } => (
                fields
                    .iter()
                    .filter(|f| !f.modifiers.is_reserved)
                    .map(on_field)
                    .collect(),
                Vec::new(),
            ),
            WireShape::Enum { variants } => (
                Vec::new(),
                variants.iter().map(|v| v.name.clone()).collect(),
            ),
        }
    }
}

/// Errors produced while lowering a `WireDecl` into a [`WireCodecPlan`].
///
/// All variants are fail-closed — callers MUST stop emission on error rather
/// than encode a partial buffer (per `serializer-fail-closed`).
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum WireCodecError {
    /// A field's declared type name could not be classified.
    UnresolvedFieldType {
        /// The field that failed resolution.
        field: String,
        /// The type name as written in source (empty etc.).
        ty: String,
    },
    /// Duplicate field number detected on build (checker may have missed it).
    DuplicateFieldNumber {
        /// The field number that appears twice.
        number: u32,
    },
}

impl core::fmt::Display for WireCodecError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::UnresolvedFieldType { field, ty } => {
                write!(f, "unresolved wire field type: {field}: {ty:?}")
            }
            Self::DuplicateFieldNumber { number } => {
                write!(f, "duplicate wire field number: @{number}")
            }
        }
    }
}

impl std::error::Error for WireCodecError {}

impl From<KindError> for WireCodecError {
    fn from(e: KindError) -> Self {
        match e {
            KindError::Empty => Self::UnresolvedFieldType {
                field: String::new(),
                ty: String::new(),
            },
        }
    }
}

impl WireCodecPlan {
    /// Build a `WireCodecPlan` from a parsed `WireDecl`.
    ///
    /// Currently reads field types directly from `WireFieldDecl::ty` (the
    /// canonical type name string recorded by the parser). When Lane 1 lands,
    /// an enriched overload that takes the checker's resolved field-type map
    /// can replace this by-name resolution; the plan shape is unchanged.
    ///
    /// # Errors
    ///
    /// Returns `WireCodecError::UnresolvedFieldType` when a field's type name
    /// is empty. Returns `WireCodecError::DuplicateFieldNumber` when two
    /// fields share a wire number.
    pub fn build(decl: &WireDecl) -> Result<Self, WireCodecError> {
        match decl.kind {
            WireDeclKind::Struct => build_struct(decl),
            WireDeclKind::Enum => Ok(build_enum(decl)),
        }
    }
}

fn build_struct(decl: &WireDecl) -> Result<WireCodecPlan, WireCodecError> {
    let mut fields = Vec::with_capacity(decl.fields.len());
    let mut seen_numbers: Vec<u32> = Vec::new();
    for f in &decl.fields {
        // Skip `reserved N` pseudo-fields — they hold a number but no type.
        if f.is_reserved {
            if seen_numbers.contains(&f.field_number) {
                return Err(WireCodecError::DuplicateFieldNumber {
                    number: f.field_number,
                });
            }
            seen_numbers.push(f.field_number);
            continue;
        }
        let kind = PrimitiveWireKind::from_type_name(&f.ty).map_err(|_| {
            WireCodecError::UnresolvedFieldType {
                field: f.name.clone(),
                ty: f.ty.clone(),
            }
        })?;
        if seen_numbers.contains(&f.field_number) {
            return Err(WireCodecError::DuplicateFieldNumber {
                number: f.field_number,
            });
        }
        seen_numbers.push(f.field_number);
        let narrowing = IntegerBounds::for_kind(&kind);
        fields.push(FieldPlan {
            name: f.name.clone(),
            number: f.field_number,
            json_name: json_name_for(f, decl.json_case),
            yaml_name: yaml_name_for(f, decl.yaml_case),
            kind,
            modifiers: FieldModifiers {
                is_optional: f.is_optional,
                is_repeated: f.is_repeated,
                is_deprecated: f.is_deprecated,
                is_reserved: f.is_reserved,
                since: f.since,
            },
            narrowing,
        });
    }
    Ok(WireCodecPlan {
        name: decl.name.clone(),
        shape: WireShape::Struct { fields },
        json_case: decl.json_case,
        yaml_case: decl.yaml_case,
    })
}

fn build_enum(decl: &WireDecl) -> WireCodecPlan {
    let variants = decl
        .variants
        .iter()
        .map(|v| VariantPlan {
            name: v.name.clone(),
        })
        .collect();
    WireCodecPlan {
        name: decl.name.clone(),
        shape: WireShape::Enum { variants },
        json_case: decl.json_case,
        yaml_case: decl.yaml_case,
    }
}

fn json_name_for(f: &WireFieldDecl, case: Option<NamingCase>) -> String {
    // Per-field override wins; otherwise apply the struct-level case.
    if let Some(explicit) = &f.json_name {
        return explicit.clone();
    }
    apply_case(&f.name, case)
}

fn yaml_name_for(f: &WireFieldDecl, case: Option<NamingCase>) -> String {
    if let Some(explicit) = &f.yaml_name {
        return explicit.clone();
    }
    apply_case(&f.name, case)
}

/// Apply a struct-level naming case to a field name.
///
/// WHY: the parser records the case option but does not pre-bake the final
/// key string. Keeping the transform here lets every codec descriptor consume
/// the same `json_name` / `yaml_name` without re-implementing the rule.
/// WHEN: can move to the parser once all call sites agree on a single rule.
/// WHAT: the real solution lowers case selection into parser post-processing.
fn apply_case(name: &str, case: Option<NamingCase>) -> String {
    let Some(case) = case else {
        return name.to_string();
    };
    match case {
        NamingCase::CamelCase => to_camel_case(name),
        NamingCase::PascalCase => to_pascal_case(name),
        NamingCase::SnakeCase => to_snake_case(name),
        NamingCase::ScreamingSnake => to_screaming_snake(name),
        NamingCase::KebabCase => to_kebab_case(name),
    }
}

fn tokenize(name: &str) -> Vec<String> {
    // Split on `_` / `-` plus lowerUpper boundaries.
    let mut out: Vec<String> = Vec::new();
    let mut cur = String::new();
    let mut prev_lower = false;
    for ch in name.chars() {
        if ch == '_' || ch == '-' || ch == ' ' {
            if !cur.is_empty() {
                out.push(std::mem::take(&mut cur));
            }
            prev_lower = false;
            continue;
        }
        if ch.is_uppercase() && prev_lower && !cur.is_empty() {
            out.push(std::mem::take(&mut cur));
        }
        cur.push(ch);
        prev_lower = ch.is_lowercase();
    }
    if !cur.is_empty() {
        out.push(cur);
    }
    out
}

fn to_snake_case(name: &str) -> String {
    tokenize(name)
        .into_iter()
        .map(|t| t.to_lowercase())
        .collect::<Vec<_>>()
        .join("_")
}

fn to_screaming_snake(name: &str) -> String {
    tokenize(name)
        .into_iter()
        .map(|t| t.to_uppercase())
        .collect::<Vec<_>>()
        .join("_")
}

fn to_kebab_case(name: &str) -> String {
    tokenize(name)
        .into_iter()
        .map(|t| t.to_lowercase())
        .collect::<Vec<_>>()
        .join("-")
}

fn to_camel_case(name: &str) -> String {
    let parts = tokenize(name);
    let mut out = String::new();
    for (i, p) in parts.iter().enumerate() {
        if i == 0 {
            out.push_str(&p.to_lowercase());
        } else {
            let mut cs = p.chars();
            if let Some(first) = cs.next() {
                for u in first.to_uppercase() {
                    out.push(u);
                }
                for rest in cs {
                    for l in rest.to_lowercase() {
                        out.push(l);
                    }
                }
            }
        }
    }
    out
}

fn to_pascal_case(name: &str) -> String {
    tokenize(name)
        .into_iter()
        .map(|p| {
            let mut cs = p.chars();
            let mut s = String::new();
            if let Some(first) = cs.next() {
                for u in first.to_uppercase() {
                    s.push(u);
                }
                for rest in cs {
                    for l in rest.to_lowercase() {
                        s.push(l);
                    }
                }
            }
            s
        })
        .collect::<String>()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn snake_case_splits_mixed_input() {
        assert_eq!(to_snake_case("FooBar"), "foo_bar");
        assert_eq!(to_snake_case("foo_bar"), "foo_bar");
        assert_eq!(to_snake_case("foo-bar"), "foo_bar");
        assert_eq!(to_snake_case("fooBar"), "foo_bar");
    }

    #[test]
    fn pascal_case_capitalises_each_token() {
        assert_eq!(to_pascal_case("foo_bar"), "FooBar");
        assert_eq!(to_pascal_case("foo-bar"), "FooBar");
        assert_eq!(to_pascal_case("fooBar"), "FooBar");
    }

    #[test]
    fn camel_case_keeps_first_token_lowercase() {
        assert_eq!(to_camel_case("foo_bar"), "fooBar");
        assert_eq!(to_camel_case("FooBar"), "fooBar");
    }

    #[test]
    fn screaming_snake_upcases_all_tokens() {
        assert_eq!(to_screaming_snake("fooBar"), "FOO_BAR");
    }

    #[test]
    fn kebab_case_joins_with_dashes() {
        assert_eq!(to_kebab_case("fooBar"), "foo-bar");
    }

    #[test]
    fn bounds_for_kind_covers_integer_primitives() {
        assert_eq!(
            IntegerBounds::for_kind(&PrimitiveWireKind::U8),
            Some(IntegerBounds {
                min: 0,
                max: u64::from(u8::MAX)
            })
        );
        assert_eq!(
            IntegerBounds::for_kind(&PrimitiveWireKind::I32),
            Some(IntegerBounds {
                min: i64::from(i32::MIN),
                max: u64::try_from(i64::from(i32::MAX)).unwrap()
            })
        );
        assert_eq!(IntegerBounds::for_kind(&PrimitiveWireKind::F32), None);
        assert_eq!(IntegerBounds::for_kind(&PrimitiveWireKind::String), None);
    }

    #[test]
    fn duration_bounds_permit_negative_nanoseconds() {
        let bounds = IntegerBounds::for_kind(&PrimitiveWireKind::Duration)
            .expect("Duration must have bounds");
        // Duration stores nanoseconds as i64 — negative values mean "time in
        // the past". The plan bounds must cover i64::MIN so that negative
        // Duration values are not rejected by the encode guard.
        assert_eq!(bounds.min, i64::MIN);
        assert_eq!(bounds.max, u64::try_from(i64::MAX).unwrap());
    }

    #[test]
    fn char_bounds_cover_full_unicode_codepoint_range() {
        let bounds =
            IntegerBounds::for_kind(&PrimitiveWireKind::Char).expect("Char must have bounds");
        assert_eq!(bounds.min, 0);
        assert_eq!(bounds.max, 0x10_FFFF);
    }

    #[test]
    fn fold_shape_filters_reserved_and_maps_fields() {
        use crate::kind::PrimitiveWireKind;
        let reserved = FieldPlan {
            name: "_pad".into(),
            number: 2,
            json_name: "_pad".into(),
            yaml_name: "_pad".into(),
            kind: PrimitiveWireKind::I32,
            modifiers: FieldModifiers {
                is_reserved: true,
                ..FieldModifiers::default()
            },
            narrowing: None,
        };
        let active = FieldPlan {
            name: "x".into(),
            number: 1,
            json_name: "x".into(),
            yaml_name: "x".into(),
            kind: PrimitiveWireKind::I64,
            modifiers: FieldModifiers::default(),
            narrowing: IntegerBounds::for_kind(&PrimitiveWireKind::I64),
        };
        let plan = WireCodecPlan {
            name: "S".into(),
            shape: WireShape::Struct {
                fields: vec![active.clone(), reserved],
            },
            json_case: None,
            yaml_case: None,
        };
        let (fields, variants) = plan.fold_shape(|f| f.name.clone());
        assert_eq!(
            fields,
            vec!["x".to_string()],
            "reserved field must be excluded"
        );
        assert!(variants.is_empty());
    }

    #[test]
    fn fold_shape_enum_returns_variant_names() {
        let plan = WireCodecPlan {
            name: "E".into(),
            shape: WireShape::Enum {
                variants: vec![
                    VariantPlan {
                        name: "Alpha".into(),
                    },
                    VariantPlan {
                        name: "Beta".into(),
                    },
                ],
            },
            json_case: None,
            yaml_case: None,
        };
        let (fields, variants) = plan.fold_shape(|f| f.name.clone());
        assert!(fields.is_empty());
        assert_eq!(variants, vec!["Alpha".to_string(), "Beta".to_string()]);
    }
}
