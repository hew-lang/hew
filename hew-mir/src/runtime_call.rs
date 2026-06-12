//! Typed cross-layer descriptor for compiler-known runtime / builtin
//! calls. Substrate canonically lives in [`hew_types::runtime_call`] so
//! the type checker can construct typed descriptors (the checker
//! depends on `hew-types`, never `hew-mir`).
//!
//! This module is a thin re-export to keep historical
//! `hew_mir::runtime_call::*` import paths working. The bijection and
//! allowlist parity tests live in `hew-mir/tests/runtime_call_allowlist.rs`
//! because they require `crate::runtime_symbols` (the MIR-side
//! emitter allowlist), which by design has no place in `hew-types`.

pub use hew_types::runtime_call::*;
