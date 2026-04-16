//! `hew-wirecodec` — unified wire-type codec plan for the Hew compiler.
//!
//! This crate owns [`WireCodecPlan`], the single choke point for lowering
//! `#[wire]`-annotated types into target-specific codec descriptors. Every
//! wire codec path (msgpack, JSON, YAML) reads the same plan rather than
//! dispatching directly on `PrimitiveTypeKind` or type-name strings.
//!
//! Stages 1–3 (this crate's initial landing):
//! 1. Introduce `WireCodecPlan`, [`PrimitiveWireKind`], and [`FieldPlan`]
//!    as the structural-canonical record of a wire type.
//! 2. Add [`MsgpackCodecDesc`] as the descriptor-driven msgpack emitter.
//!    A shadow test in `hew-serialize` compares byte-for-byte against the
//!    legacy path on every `e2e_wire` fixture.
//! 3. Make the descriptor-driven path the canonical wire-type codec entry;
//!    the legacy `rmp_serde::to_vec_named(&WireDecl)` path stays behind a
//!    feature flag for the 10,000-iteration random-corpus check (Lane 7b).
//!
//! LESSONS upheld: `serializer-fail-closed`, `generated-narrowing-guards`,
//! `exhaustive-traversal-and-lowering`.

pub mod kind;
pub mod msgpack_desc;
pub mod plan;

pub use crate::kind::{KindError, PrimitiveWireKind};
pub use crate::msgpack_desc::{MsgpackCodecDesc, MsgpackFieldOp, MsgpackOp};
pub use crate::plan::{
    FieldModifiers, FieldPlan, IntegerBounds, VariantPlan, WireCodecError, WireCodecPlan, WireShape,
};
