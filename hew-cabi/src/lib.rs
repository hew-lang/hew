//! Shared C ABI helpers for Hew native package bindings.
//!
//! This crate provides the common types and functions that native Hew packages
//! need to implement `#[no_mangle] extern "C"` functions:
//!
//! - String conversion helpers (`malloc_cstring`, `str_to_malloc`, `cstr_to_str`)
//! - `HewValueLayout` and semantic value copy/drop callbacks
//! - `HewVec` type definition and byte-conversion helpers
//! - `HewSink` construction helpers for custom sink implementations
//!
//! Native package authors depend on this crate; it gets compiled into each
//! package's staticlib. At link time, `hew_vec_*` symbols resolve against
//! `libhew_runtime.a`.

pub mod cabi;
pub mod callable;
pub mod host_error;
pub mod map;
pub mod mem;

/// Words of a trait-object table before its first method slot: the
/// `drop_in_place`, size and alignment words and the concrete value's
/// release descriptor (`hew-runtime/src/trait_object.rs::HewVtable`). Slot
/// `s` sits at word `HEW_VTABLE_PREFIX_WORDS + s`; physical MIR is the one
/// compiler stage that reads this.
pub const HEW_VTABLE_PREFIX_WORDS: u32 = 4;
pub mod sink;
pub mod string;
pub mod value;
pub mod vec;
