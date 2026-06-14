//! Hew `std::mem` — low-level memory primitives (compiler-internal floor).
//!
//! The public `std::mem` surface lives in `mem.hew` as bodyless
//! `#[intrinsic("mem.*")]` declarations; the compiler supplies their lowering
//! (the allocator ops call the hew-runtime allocator, the typed-pointer ops
//! lower to LLVM load/store/GEP). This crate exists only as a placeholder for
//! the workspace build; no FFI functions are required here.
