//! Hew `std::time` — cron scheduling and datetime operations.
//!
//! The `#[no_mangle]` C ABI symbols (`hew_cron_*`, `hew_datetime_*`) live in
//! the single `libhew_std.a` archive; the linker's dead-strip prunes whichever
//! module a program does not reference.

pub mod cron;
pub mod datetime;
