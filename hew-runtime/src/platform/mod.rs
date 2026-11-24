//! Narrow internal platform seams isolating native and `wasm32` runtime
//! implementations.
//!
//! Native remains the primary first-class deployment target; these contracts
//! keep hot native fast paths inline while confining target selection to this
//! module tree instead of duplicating `#[cfg]` ladders across subsystems.
//! Public `#[no_mangle]` C-ABI exports retain their target gates at their own
//! definitions so the exported ABI surface stays invariant.

#![deny(unsafe_op_in_unsafe_fn)]

#[cfg(not(target_arch = "wasm32"))]
mod native;
#[cfg(not(target_arch = "wasm32"))]
use native as imp;

#[cfg(target_arch = "wasm32")]
mod wasm;
#[cfg(target_arch = "wasm32")]
use wasm as imp;

pub(crate) mod clock;
pub(crate) mod entropy;
pub(crate) mod executor;
pub(crate) mod host_io;
