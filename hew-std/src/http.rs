//! Hew `std::net::http` — HTTP client and server.
//!
//! A pure consumer of the runtime C ABI: it references runtime symbols
//! (`hew_vec_*`, `hew_stream_*`, …) only through `hew-cabi`'s `extern "C"`
//! declarations, which resolve against `libhew.a` at the final link.

pub mod client;
pub mod headers_vec;
pub mod server;

// Producer-specific R1/R2/R3 ownership proofs for the seven HTTP string
// accessors whose classification carries `result-retention = "transferred"`.
#[cfg(test)]
mod string_result_retention;
