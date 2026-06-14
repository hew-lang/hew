//! Hew `std::time` — combined cron scheduling and datetime operations.
//!
//! This crate folds the formerly-separate `std::time::cron` and
//! `std::time::datetime` runtime modules into one domain crate. The
//! `#[no_mangle]` C ABI symbols (`hew_cron_*`, `hew_datetime_*`) are unchanged
//! and live in a single archive; the linker's dead-strip prunes whichever
//! module a program does not reference.

// Force-link hew-runtime so the linker can resolve hew_vec_* symbols
// referenced by hew-cabi's object code, and to access the shared
// parse-error slot used by the datetime module.
extern crate hew_runtime;

pub mod cron;
pub mod datetime;
