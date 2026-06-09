//! Hew `std::net::http` — HTTP client and server.
//!
//! The shipped `libhew_std_net_http.a` is a pure consumer of the runtime C ABI.
//! It references runtime symbols (`hew_vec_*`, `hew_stream_*`, …) only through
//! `hew-cabi`'s `extern "C"` declarations, which resolve against `libhew.a` at
//! the final link. It deliberately does NOT depend on or force-link
//! `hew-runtime` in normal builds: doing so would bundle a second copy of every
//! runtime `#[no_mangle]` export into the archive, which then collides with
//! `libhew.a` when this module is linked into a Hew binary as a native package
//! (`--link-lib`). The compiler driver lists `libhew.a` after the package
//! archives so these undefined references resolve cleanly at the final link.
//!
//! The crate's own unit tests are an exception: they compile into a standalone
//! executable that has no `libhew.a` to resolve against, so they force-link
//! `hew-runtime` (a dev-dependency) below under `cfg(test)` only. This is never
//! present in the shipped staticlib.
#[cfg(test)]
extern crate hew_runtime;

pub mod client;
pub mod headers_vec;
pub mod server;
