//! Check the signed ABI value before converting it to a target-width index.

#[inline]
pub(super) fn checked_index<W: TryFrom<i64> + Copy + Ord>(index: i64, len: W) -> Option<W> {
    let index = W::try_from(index).ok()?;
    (index < len).then_some(index)
}

#[inline]
pub(super) fn checked_range<W: TryFrom<i64> + Copy + Ord>(
    start: i64,
    end: i64,
    len: W,
) -> Option<(W, W)> {
    let start = W::try_from(start).ok()?;
    let end = W::try_from(end).ok()?;
    (start <= end && end <= len).then_some((start, end))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn narrow_word_rejects_wrapping_indices_and_ranges() {
        for index in [i64::MIN, -1, 3, 4_294_967_296, 4_294_967_297, i64::MAX] {
            assert_eq!(checked_index(index, 3u32), None);
        }
        assert_eq!(checked_index(0, 3u32), Some(0));
        assert_eq!(checked_index(2, 3u32), Some(2));
        assert_eq!(checked_range(0, 3, 3u32), Some((0, 3)));
        assert_eq!(checked_range(0, 0, 0u32), Some((0, 0)));
        for (start, end) in [
            (-1, 0),
            (2, 1),
            (0, 4),
            (0, 4_294_967_296),
            (4_294_967_296, 4_294_967_296),
        ] {
            assert_eq!(checked_range(start, end, 3u32), None);
        }
        assert_eq!(
            checked_index(4_294_967_296, 4_294_967_297u64),
            Some(4_294_967_296)
        );
        assert_eq!(
            checked_range(4_294_967_296, 4_294_967_297, 4_294_967_297u64),
            Some((4_294_967_296, 4_294_967_297))
        );
    }
}
