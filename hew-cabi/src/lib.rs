//! Shared C ABI helpers for Hew native package bindings.
//!
//! This crate provides the common types and functions that native Hew packages
//! need to implement `#[no_mangle] extern "C"` functions:
//!
//! - String conversion helpers (`malloc_cstring`, `str_to_malloc`, `cstr_to_str`)
//! - `HewValueLayout` and semantic value copy/drop callbacks
//! - `HewVec` type definitions and runtime ABI declarations
//! - `HewSink` construction helpers for custom sink implementations
//!
//! Native package authors depend on this crate; it gets compiled into each
//! package's staticlib. At link time, `hew_vec_*` symbols resolve against
//! `libhew_runtime.a`.

pub mod cabi;
pub mod callable;
pub mod code;
pub mod coro;
pub mod host_error;
pub mod live;
pub mod managed;
pub mod map;
pub mod mem;
pub mod node;
pub mod sink;
pub mod string;
pub mod time;
pub mod value;
pub mod vec;

/// wasm-ld arguments for an executable or component that calls Hew code.
/// The shadow stack must leave room above the runtime's recovery reserve.
/// Placing it first keeps a downward overflow away from static data.
pub const WASM_SHADOW_STACK_LINK_ARGS: [&str; 3] = ["-z", "stack-size=1048576", "--stack-first"];
