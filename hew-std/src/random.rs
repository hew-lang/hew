//! Hew `std::random` — seedable pseudo-random number generation.
//!
//! The public `std::random` surface lives in `random.hew`. All FFI
//! functions are implemented in `hew-runtime/src/random.rs` (MT19937
//! Mersenne Twister). This crate exists as a workspace build placeholder;
//! no additional FFI symbols are needed here.
