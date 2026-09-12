//! LLVM object emission from verified physical MIR.
//!
//! Ownership and cleanup are explicit inputs, not backend inferences.

pub mod llvm;
pub mod physical;
pub(crate) mod sanitizer;

pub use llvm::{
    entry_body_symbol_for_triple, native_emission_triple, CodegenError, EmitArtefacts, OptLevel,
};
pub use physical::{
    emit_physical_object, physical_target_for_triple, validate_physical_codegen,
    PhysicalEmitOptions,
};
