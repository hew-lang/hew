//! Native and WebAssembly LLVM IR emitter for the Hew backend.
//!
//! Produces native object files and standalone WebAssembly modules from
//! `hew-mir`'s raw MIR, with the LLVM module verified by `Module::verify()`
//! before emission. IR construction and object emission both run in-process;
//! textual `.ll` files are still written as diagnostics artefacts.
//!
//! ## Spine subset (initial)
//!
//! The emitter accepts integer literals, integer arithmetic
//! (`+`, `-`, `*`), `let` bindings, value moves, and `Return`. Composite
//! types, strings, closures, generators, and coroutines fail closed with
//! a `CodegenError::Unsupported` carrying the construct name; the CLI
//! surfaces this as a non-zero exit. Later work widens the accepted
//! subset to cover strings, drop elaboration with cleanup CFG edges,
//! and closures/generators.
//!
//! ## Public surface
//!
//! - [`emit_module`] — emit native + wasm artefacts for an `IrPipeline`.
//! - [`validate_codegen_front`] — in-process build + LLVM-verify without any
//!   artefact emission.
//! - [`verify_pipeline`] — compatibility alias for [`validate_codegen_front`].
//! - [`EmitOptions`] — output configuration.
//! - [`EmitArtefacts`] — paths of emitted files.
//! - [`CodegenError`] — failure variants for diagnostic mapping.

pub mod llvm;

pub use llvm::{
    emit_module, validate_codegen_front, verify_pipeline, CodegenError, EmitArtefacts, EmitOptions,
};
