//! Instantiate the production conversion logic at both target word widths.
use super::*;

fn unexpected_panic(_: std::fmt::Arguments<'_>) -> ! {
    assert!(false, "vector: bounds helper panicked");
    kani::assume(false);
    loop {}
}

macro_rules! index_proof {
    ($name:ident, $word:ty) => {
        #[kani::proof]
        #[kani::stub(core::panicking::panic_fmt, unexpected_panic)]
        fn $name() {
            let index: i64 = kani::any();
            let len: $word = kani::any();
            let actual = checked_index(index, len);
            let expected = index >= 0 && (index as i128) < len as i128;
            assert_eq!(
                actual.is_some(),
                expected,
                "vector: index narrowed before checking"
            );
            if let Some(value) = actual {
                assert_eq!(value as i128, index as i128, "vector: index value changed");
                assert!(value < len, "vector: accepted index outside live range");
            }
            kani::cover!(index < 0, "vector cover: negative ABI index");
            kani::cover!(
                index == 4_294_967_296 && len == 3,
                "vector cover: 32-bit wrap witness"
            );
            kani::cover!(index == 0 && len > 0, "vector cover: representable index");
        }
    };
}

macro_rules! range_proof {
    ($name:ident, $word:ty) => {
        #[kani::proof]
        #[kani::stub(core::panicking::panic_fmt, unexpected_panic)]
        fn $name() {
            let start: i64 = kani::any();
            let end: i64 = kani::any();
            let len: $word = kani::any();
            let actual = checked_range(start, end, len);
            let expected = start >= 0 && start <= end && (end as i128) <= len as i128;
            assert_eq!(
                actual.is_some(),
                expected,
                "vector: range narrowed before checking"
            );
            if let Some((a, b)) = actual {
                assert_eq!(a as i128, start as i128, "vector: slice start changed");
                assert_eq!(b as i128, end as i128, "vector: slice end changed");
                assert!(a <= b && b <= len, "vector: accepted invalid range");
            }
            kani::cover!(
                start == 0 && end == 0 && len == 0,
                "vector cover: empty slice"
            );
            kani::cover!(
                start == 4_294_967_296 && end == start && len == 3,
                "vector cover: wrapped slice endpoints"
            );
        }
    };
}

index_proof!(vector_index_u32_checks_entire_abi_domain, u32);
index_proof!(vector_index_u64_checks_entire_abi_domain, u64);
range_proof!(vector_range_u32_checks_entire_abi_domain, u32);
range_proof!(vector_range_u64_checks_entire_abi_domain, u64);
