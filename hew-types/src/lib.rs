//! Hew type checker with bidirectional inference.
//!
//! Implements constraint-based type inference with unification,
//! automatic marker trait derivation (Send, Frozen, Copy),
//! and exhaustive pattern match checking.

pub mod check;
pub mod cycle;
pub mod env;
pub mod error;
pub mod stdlib;
pub mod stdlib_loader;
pub mod traits;
pub mod ty;
pub mod unify;

pub use check::{Checker, TypeCheckOutput, VariantDef};
pub use error::TypeError;
pub use ty::{TraitObjectBound, Ty};
