//! Native and WebAssembly LLVM IR emitter for the Hew backend.
//!
//! Produces native object files and standalone WebAssembly modules from
//! `hew-mir`'s raw MIR, with the LLVM module verified by `Module::verify()`
//! before emission. The pipeline is split across two processes: IR
//! construction runs in-process, object emission runs in the
//! `hew-emit` helper binary (see `src/llvm.rs` for the architectural
//! reasoning).
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
//! - [`EmitOptions`] — output configuration.
//! - [`EmitArtefacts`] — paths of emitted files.
//! - [`CodegenError`] — failure variants for diagnostic mapping.

pub mod llvm;

pub use llvm::{emit_module, CodegenError, EmitArtefacts, EmitOptions};
