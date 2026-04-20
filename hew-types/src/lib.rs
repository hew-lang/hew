//! Hew type checker with bidirectional inference.
//!
//! Implements constraint-based type inference with unification,
//! automatic marker trait derivation (Send, Frozen, Copy),
//! and exhaustive pattern match checking.

pub mod builtin_names;
pub mod check;
pub mod cycle;
pub mod env;
pub mod error;
pub mod lowering_facts;
pub mod method_resolution;
pub mod module_registry;
pub mod resolved_ty;
pub mod stdlib;
pub mod stdlib_loader;
pub mod traits;
pub mod ty;
pub mod unify;

pub use check::{
    builtin_function_names, Checker, MethodCallReceiverKind, MethodCallRewrite, TypeCheckOutput,
    VariantDef,
};
pub use error::TypeError;
pub use lowering_facts::{
    DropKind, HashSetAbi, HashSetElementType, LoweringFact, LoweringFactError, LoweringKind,
};
pub use resolved_ty::{BoundaryError, ResolvedTraitBound, ResolvedTy};
pub use ty::{TraitObjectBound, Ty};
