//! Native and WebAssembly LLVM IR emitter for the v0.5 Hew backend.
//!
//! The Cluster 1 cutover replaces the earlier Stage-9 text-trace emitter
//! (`emit.rs`, deleted in commit 5 of this branch) with a direct LLVM IR
//! emitter built on `inkwell`, adopted from the backend probe (Track C) at
//! `.claude/worktrees/c1-backend-probe` HEAD `e6c83faa`. The emitter
//! produces native object files and standalone WebAssembly modules from
//! the `hew-mir` raw MIR, both verified by `Module::verify()`.
//!
//! ## Spine subset (Cluster 1)
//!
//! The emitter accepts integer literals, integer arithmetic
//! (`+`, `-`, `*`), `let` bindings, value moves, and `Return`. Composite
//! types, strings, closures, generators, and coroutines fail closed with
//! a `CodegenError::Unsupported` carrying the construct name; the CLI
//! surfaces this as a non-zero exit. Later clusters widen the accepted
//! subset:
//!
//! - Cluster 2: String / `to_string` / Display / composite types.
//! - Cluster 3: Drop elaboration with cleanup CFG edges.
//! - Cluster 4: Closures, generators, `Lazy<T>`.
//!
//! ## Public surface
//!
//! - [`emit_module`] — emit native + wasm artefacts for an `IrPipeline`.
//! - [`EmitOptions`] — output configuration.
//! - [`EmitArtefacts`] — paths of emitted files.
//! - [`CodegenError`] — failure variants for diagnostic mapping.

mod emit;
pub mod llvm;

pub use emit::emit_mlir;
pub use llvm::{emit_module, CodegenError, EmitArtefacts, EmitOptions};
