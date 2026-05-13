//! D2 textual and D7-style pre-MLIR proof emitter for the Hew compiler.
//!
//! # Design decisions honoured
//!
//! - **D2** (`textual_first_tablegen_after_c1`): emits a deterministic Hew-IR textual proof using
//!   named-op-like syntax.  No TableGen, no C++ dialect registration required,
//!   and this Stage 9 text is not claimed to parse with MLIR tooling yet.
//!
//! - **D7** (`type_attribute_storage_class`): records the current canonical
//!   Hew dialect-style textual spellings. Full ownership/storage attributes remain
//!   pending Stage 4 TypeDescriptor/THIR authority:
//!   - `!hew.string`  — Hew string
//!   - `!hew.bytes`   — Hew byte buffer
//!   - `!hew.char`    — Unicode scalar value
//!   - `!hew.duration` — nanosecond-precision duration
//!   - MLIR built-ins (`i64`, `f32`, `i1`, …) for scalars
//!
//! The sole public entry point is [`emit_mlir`].

mod emit;

pub use emit::emit_mlir;
