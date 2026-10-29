//! Admission arithmetic for every managed allocation and growth reservation.
pub(super) fn admits(live: u64, reserved: u64, bytes: u64, limit: Option<u64>) -> bool {
    live.checked_add(reserved)
        .and_then(|n| n.checked_add(bytes))
        .is_some_and(|n| limit.is_none_or(|limit| n <= limit))
}
