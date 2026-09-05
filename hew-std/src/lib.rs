//! Hew standard library — the Rust-backed FFI surface for every `std` module.
//!
//! This crate contains the Rust-backed implementation for all std modules, so
//! the workspace builds one staticlib (`libhew_std.a`) and `hew-lib` carries one
//! stdlib dependency. Each module keeps its own
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

// ── WASM build surface ────────────────────────────────────────────────────
// The wasm32 target (browser playground wire codecs) compiles only the
// wire-codec and dependency-free modules. Every native-only module is gated
// behind `cfg(not(wasm))` because its crates (ring, quinn/tokio's socket2,
// flate2, getrandom, argon2, aws-lc, …) hard-`compile_error!` on wasm32. This
// preserves the pre-consolidation surface: wasm exports the pure-Rust codecs
// (json, yaml, toml, csv, hex, base64, log, math, mem, uuid) and dependency-free
// modules, while native-only deps remain behind cfg gates. The .hew-path module
// resolution and the native dead-strip are unaffected; this only narrows what
// compiles for wasm.

// encoding — wire codecs (wasm + native)
pub mod json;
pub mod toml;
pub mod yaml;

// pure-Rust / dependency-free modules (wasm + native)
pub mod base64;
pub mod csv;
pub mod hex;
pub mod log;
pub mod math;
pub mod mem;
pub mod mime;
pub mod random;
pub mod semver;
pub mod sort;
pub mod unicode;

// ── Native-only modules ───────────────────────────────────────────────────

// crypto
#[cfg(not(target_family = "wasm"))]
pub mod crypto;
#[cfg(not(target_family = "wasm"))]
pub mod encrypt;
#[cfg(not(target_family = "wasm"))]
pub mod jwt;
#[cfg(not(target_family = "wasm"))]
pub mod password;
#[cfg(not(target_family = "wasm"))]
pub mod sign;

// encoding — native-only codecs
#[cfg(not(target_family = "wasm"))]
pub mod compress;
#[cfg(not(target_family = "wasm"))]
pub mod markdown;
#[cfg(not(target_family = "wasm"))]
pub mod msgpack;
#[cfg(not(target_family = "wasm"))]
pub mod protobuf;
#[cfg(not(target_family = "wasm"))]
pub mod xml;

// misc
#[cfg(not(target_family = "wasm"))]
pub mod uuid;

// net
#[cfg(not(target_family = "wasm"))]
pub mod dns;
#[cfg(not(target_family = "wasm"))]
pub mod http;
#[cfg(not(target_family = "wasm"))]
pub mod ipnet;

#[cfg(not(target_family = "wasm"))]
mod bind_addr;
// Shared #[cfg(test)] scaffolding for the tls/smtp/quic actor-scoped
// last-error regression tests (#2659) — a single runtime-guard/actor-context
// helper reused by all three so their tests serialize on one process-global
// scheduler slot instead of racing on independent per-module locks.
#[cfg(all(test, not(target_family = "wasm")))]
mod net_error_slot_test_support;
// The hew-std half of the `*_last_error` result-retention oracle (#2828) —
// establishes, per symbol, that the returned buffer is transferred to the
// caller rather than borrowed from storage this crate keeps. The hew-runtime
// half and the counterfactual live in
// `hew-runtime/tests/last_error_result_retention.rs`.
#[cfg(all(test, not(target_family = "wasm")))]
mod last_error_retention;
// Measured retention of native OS/I/O `-> string` exports.  This remains a
// family-specific oracle so the `*_last_error` suite retains its narrow slot
// semantics and the direct input-state witnesses stay legible.
#[cfg(all(test, not(target_family = "wasm")))]
mod os_io_string_retention;
#[cfg(not(target_family = "wasm"))]
pub mod quic;
#[cfg(not(target_family = "wasm"))]
pub mod smtp;
// R1/R2/R3 proofs for deterministic, no-I/O string producers. These are the
// executable authority for the corresponding `result-retention` rows.
#[cfg(all(test, not(target_family = "wasm")))]
mod string_result_retention;
#[cfg(not(target_family = "wasm"))]
pub mod tls;
#[cfg(not(target_family = "wasm"))]
pub mod url;
#[cfg(not(target_family = "wasm"))]
pub mod websocket;
// Producer-specific R1/R2/R3 proofs for WebSocket message/error string
// results. Kept separate from the batch string/last-error family modules.
#[cfg(all(test, not(target_family = "wasm")))]
mod websocket_string_result_retention;

// text
#[cfg(not(target_family = "wasm"))]
pub mod regex;

// time
#[cfg(not(target_family = "wasm"))]
pub mod time;

#[cfg(test)]
mod test_string;
