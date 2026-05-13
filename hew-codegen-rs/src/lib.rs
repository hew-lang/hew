//! D2/D7-compliant textual MLIR emitter for the Hew compiler.
//!
//! # Design decisions honoured
//!
//! - **D2** (`textual_first_tablegen_after_c1`): emits plain MLIR text using
//!   named-op syntax.  No TableGen, no C++ dialect registration required.
//!
//! - **D7** (`type_attribute_storage_class`): maps Hew types to MLIR dialect
//!   types that encode ownership at the type level:
//!   - `!hew.string`  — owned UTF-8 string (Hew's default string)
//!   - `!hew.bytes`   — owned byte buffer
//!   - `!hew.char`    — Unicode scalar value
//!   - `!hew.duration` — nanosecond-precision duration
//!   - MLIR built-ins (`i64`, `f32`, `i1`, …) for scalars
//!
//! The sole public entry point is [`emit_mlir`].

mod emit;

pub use emit::emit_mlir;
