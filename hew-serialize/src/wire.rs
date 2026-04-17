//! Wire-type msgpack emission routed through `hew-wirecodec`.
//!
//! This module is the single choke point for wire-type descriptor bytes:
//! every caller that previously reached into `WireDecl` directly to drive a
//! hand-written msgpack walker now goes through [`serialize_wire_decl_via_plan`].
//! The descriptor bytes are stable across runs of the same plan and round-trip
//! through `rmp-serde`.
//!
//! Stages 1-3 of Lane 7 land this behind a default-on code path. The legacy
//! `serialize_wire_decl_legacy` function is retained behind the
//! `legacy-wire-msgpack` feature for the 10,000-iteration random-corpus
//! shadow comparison performed in Lane 7b stage 7; it is not on the hot path
//! for any default build.

use hew_parser::ast::WireDecl;
use hew_wirecodec::{MsgpackCodecDesc, WireCodecError, WireCodecPlan};

/// Serialize a [`WireDecl`] to msgpack bytes via the plan-driven descriptor.
///
/// This is the primary wire-type serialization path. It runs the
/// `WireCodecPlan` build (which fail-closes on unresolved field types or
/// duplicate field numbers), lowers to a [`MsgpackCodecDesc`], and encodes
/// with `rmp_serde::to_vec_named`.
///
/// # Errors
///
/// Returns a [`WireCodecError`] if the plan build rejects the decl.
pub fn serialize_wire_decl_via_plan(decl: &WireDecl) -> Result<Vec<u8>, WireCodecError> {
    let plan = WireCodecPlan::build(decl)?;
    let desc = MsgpackCodecDesc::from_plan(&plan);
    Ok(desc.to_msgpack_bytes())
}

/// Legacy hand-derived path — encode the `WireDecl` AST directly via
/// `rmp_serde::to_vec_named`.
///
/// The bytes emitted here are a different shape from the descriptor path
/// (this encodes the source-level AST; the descriptor encodes the lowered
/// op set) — callers must NOT assume byte equality across the two paths.
/// Stage 3 moves this behind a feature flag so the default build exposes
/// only the descriptor-driven entry point.
///
/// # Panics
///
/// Panics if `rmp-serde` serialization fails; `to_vec_named` only fails on
/// IO errors against in-memory buffers, which cannot occur.
#[must_use]
pub fn serialize_wire_decl_legacy(decl: &WireDecl) -> Vec<u8> {
    rmp_serde::to_vec_named(decl).expect("WireDecl msgpack serialization never fails")
}

#[cfg(test)]
mod tests {
    use super::*;
    use hew_parser::ast::{Visibility, WireDeclKind, WireFieldDecl};

    fn sample_decl() -> WireDecl {
        WireDecl {
            visibility: Visibility::Pub,
            kind: WireDeclKind::Struct,
            name: "Point".into(),
            fields: vec![
                WireFieldDecl {
                    name: "x".into(),
                    ty: "int".into(),
                    field_number: 1,
                    is_optional: false,
                    is_repeated: false,
                    is_reserved: false,
                    is_deprecated: false,
                    json_name: None,
                    yaml_name: None,
                    since: None,
                },
                WireFieldDecl {
                    name: "y".into(),
                    ty: "int".into(),
                    field_number: 2,
                    is_optional: false,
                    is_repeated: false,
                    is_reserved: false,
                    is_deprecated: false,
                    json_name: None,
                    yaml_name: None,
                    since: None,
                },
            ],
            variants: vec![],
            json_case: None,
            yaml_case: None,
        }
    }

    #[test]
    fn via_plan_emits_bytes_that_round_trip() {
        let bytes = serialize_wire_decl_via_plan(&sample_decl()).expect("plan ok");
        let decoded: MsgpackCodecDesc = rmp_serde::from_slice(&bytes).expect("decode");
        assert_eq!(decoded.name, "Point");
        assert_eq!(decoded.fields.len(), 2);
        assert_eq!(decoded.fields[0].tag, 1);
        assert_eq!(decoded.fields[1].tag, 2);
    }

    #[test]
    fn via_plan_is_deterministic_across_calls() {
        let decl = sample_decl();
        let a = serialize_wire_decl_via_plan(&decl).unwrap();
        let b = serialize_wire_decl_via_plan(&decl).unwrap();
        assert_eq!(a, b, "descriptor bytes must be deterministic");
    }

    #[test]
    fn via_plan_rejects_unresolved_field_type() {
        let mut decl = sample_decl();
        decl.fields[0].ty = String::new();
        assert!(serialize_wire_decl_via_plan(&decl).is_err());
    }
}
