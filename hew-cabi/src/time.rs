//! Normalized wall-clock timestamp at the native C boundary.

/// Unix seconds and the nanosecond fraction in `0..1_000_000_000`.
#[repr(C)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct HewTimestamp {
    pub seconds: i64,
    pub nanoseconds: u32,
}
