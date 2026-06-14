//! Hew `std::net::http` — HTTP client and server.
//!
//! A pure consumer of the runtime C ABI: it references runtime symbols
//! (`hew_vec_*`, `hew_stream_*`, …) only through `hew-cabi`'s `extern "C"`
//! declarations, which resolve against `libhew.a` at the final link.

pub mod client;
pub mod headers_vec;
pub mod server;
