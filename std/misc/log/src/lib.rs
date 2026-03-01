//! Hew stdlib: `std::misc::log` — structured logging.
//!
//! All logging primitives (`hew_log_emit`, `hew_log_set_level`, etc.) live in
//! `hew-runtime/src/log_core.rs` and are unconditionally linked into every Hew
//! binary. This crate exists only to provide `export-meta` metadata for the
//! stdlib generator.

#[cfg(feature = "export-meta")]
pub mod export_meta;
