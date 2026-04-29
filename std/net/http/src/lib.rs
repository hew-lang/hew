//! Hew `std::net::http` — HTTP client and server.

// Force-link hew-runtime so the linker can resolve hew_vec_* symbols
// referenced by hew-cabi's object code, and to access the shared
// error slot.
extern crate hew_runtime;

pub mod client;
pub mod server;
