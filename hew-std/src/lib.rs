//! Hew standard library — the Rust-backed FFI surface for every `std` module.
//!
//! This crate folds the formerly-separate per-module std crates into a single
//! `hew-std` crate so the workspace builds one staticlib (`libhew_std.a`) and
//! `hew-lib` carries one stdlib dependency. Each module keeps its own
//! `#[no_mangle]` C ABI symbols (`hew_json_*`, `hew_http_*`, `hew_cron_*`, …) in
//! the shared global namespace; the linker's per-function dead-strip prunes
//! whichever modules a program does not reference, so consolidation is
//! symbol-transparent and per-program pruning is preserved.
//!
//! The compiler resolves stdlib modules by `.hew` path, not by crate, so the
//! `.hew` source tree and package manifests under `std/` are unchanged.

// Force-link hew-runtime so the linker can resolve the runtime `#[no_mangle]`
// symbols (`hew_vec_*`, `hew_bytes_*`, …) that hew-cabi declares as `extern "C"`,
// and so modules that use runtime Rust paths (e.g. the datetime/parse-error slot
// and the DNS blocking pool) resolve at compile time.
extern crate hew_runtime;

// crypto
pub mod crypto;
pub mod encrypt;
pub mod jwt;
pub mod password;
pub mod sign;

// encoding
pub mod base64;
pub mod compress;
pub mod csv;
pub mod hex;
pub mod json;
pub mod markdown;
pub mod msgpack;
pub mod protobuf;
pub mod toml;
pub mod xml;
pub mod yaml;

// math + memory
pub mod math;
pub mod mem;

// misc
pub mod log;
pub mod uuid;

// net
pub mod dns;
pub mod http;
pub mod ipnet;
pub mod mime;
pub mod quic;
pub mod smtp;
pub mod tls;
pub mod url;
pub mod websocket;

// random + sort
pub mod random;
pub mod sort;

// text
pub mod regex;
pub mod semver;
pub mod unicode;

// time
pub mod time;
