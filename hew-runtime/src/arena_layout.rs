//! Checked arithmetic for arena allocation extents and caps.

pub(crate) fn valid_request(size: usize, align: usize) -> bool {
    size > 0 && isize::try_from(size).is_ok() && align.is_power_of_two() && align <= 4096
}

pub(crate) fn budget(used: usize, cap: usize, size: usize) -> Option<usize> {
    if cap == 0 {
        Some(used)
    } else {
        used.checked_add(size).filter(|total| *total <= cap)
    }
}

pub(crate) fn fit(
    cursor: usize,
    size: usize,
    align: usize,
    capacity: usize,
) -> Option<std::ops::Range<usize>> {
    if !valid_request(size, align) || capacity > isize::MAX as usize {
        return None;
    }
    let start = cursor.checked_add(align - 1)? & !(align - 1);
    let end = start.checked_add(size)?;
    (end <= capacity).then_some(start..end)
}
