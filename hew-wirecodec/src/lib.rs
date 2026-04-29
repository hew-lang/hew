//! `hew-wirecodec` — unified wire-type codec plan for the Hew compiler.
//!
//! This crate owns [`WireCodecPlan`], the single choke point for lowering
//! `#[wire]`-annotated types into target-specific codec descriptors. Every
//! wire codec path (msgpack, JSON, YAML) reads the same plan rather than
//! dispatching directly on `PrimitiveTypeKind` or type-name strings.
//!
//! The crate currently exposes three descriptor types — [`MsgpackCodecDesc`],
//! [`JsonCodecDesc`], and [`YamlCodecDesc`] — plus the plan and kind
//! primitives. Every descriptor is produced from the same
//! [`WireCodecPlan`] via a `from_plan` constructor whose per-field dispatch
//! is an exhaustive match on [`PrimitiveWireKind`]; adding a new kind forces
//! a compile error in every descriptor until wired up, which is the
//! structural defence against the silent-default class of bugs (PRs #914,
//! #944) that motivated this consolidation.
//!
//! LESSONS upheld: `serializer-fail-closed`, `generated-narrowing-guards`,
//! `exhaustive-traversal-and-lowering`.

pub mod json_desc;
pub mod kind;
pub mod msgpack_desc;
pub(crate) mod op_codec;
pub mod plan;
pub(crate) mod primitives;
#[cfg(test)]
pub(crate) mod test_helpers;
pub mod value;
pub mod yaml_desc;

pub use crate::json_desc::{JsonCodecDesc, JsonFieldOp, JsonOp};
pub use crate::kind::{KindError, PrimitiveWireKind};
pub use crate::msgpack_desc::{MsgpackCodecDesc, MsgpackFieldOp, MsgpackOp};
pub use crate::plan::{
    FieldModifiers, FieldPlan, IntegerBounds, VariantPlan, WireCodecError, WireCodecPlan, WireShape,
};
pub use crate::value::WireValue;
pub use crate::yaml_desc::{YamlCodecDesc, YamlFieldOp, YamlOp};
