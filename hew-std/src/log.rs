//! Hew stdlib: `std::misc::log` — structured logging.
//!
//! All logging primitives (`hew_log_emit`, `hew_log_set_level`, etc.) live in
//! `hew-runtime/src/log_core.rs` and are unconditionally linked into every Hew
//! binary. This crate remains as the package boundary that `hew-lib` links into
//! compiled programs.
