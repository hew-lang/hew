//! MIR-facing re-export of the compiler-wide FFI ownership contract table.
//!
//! The table is generated in `hew-types` from
//! `scripts/jit-symbol-classification.toml`, which lets HIR validate borrowed
//! resource parameters and MIR consume the same contract without a second
//! carrier that could drift.

pub use hew_types::ffi_contracts::*;
