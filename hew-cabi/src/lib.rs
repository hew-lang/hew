//! Shared C ABI helpers for Hew native package bindings.
//!
//! This crate provides the common types and functions that native Hew packages
//! need to implement `#[no_mangle] extern "C"` functions:
//!
//! - String conversion helpers (`malloc_cstring`, `str_to_malloc`, `cstr_to_str`)
//! - `HewVec` type definition and byte-conversion helpers
//! - `HewSink` / `SinkBacking` trait for custom sink implementations
//!
//! Native package authors depend on this crate; it gets compiled into each
//! package's staticlib. At link time, `hew_vec_*` symbols resolve against
//! `libhew_runtime.a`.

pub mod cabi;
pub mod sink;
pub mod vec;
