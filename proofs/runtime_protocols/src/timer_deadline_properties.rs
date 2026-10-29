use crate::timer_deadline::after;
use proptest::prelude::*;

proptest! {
    #![proptest_config(ProptestConfig { cases: 2048, ..ProptestConfig::default() })]
    #[test]
    fn generated_deadlines_never_expire_early_or_wrap(
        now in any::<u64>(),
        duration in prop_oneof![
            (any::<u64>(), 0u32..1_000_000_000).prop_map(|(seconds, nanos)| std::time::Duration::new(seconds, nanos)),
            any::<u64>().prop_map(std::time::Duration::from_nanos),
            (0u64..=2_000_000).prop_map(std::time::Duration::from_nanos),
        ],
    ) {
        let maximum = u128::from(u64::MAX) * 1_000_000 + 999_999;
        let deadline = after(now, duration);
        prop_assert_eq!(deadline.is_some(), duration.as_nanos() <= maximum - u128::from(now));
        if let Some(tick) = deadline {
            let expiry = u128::from(now) + duration.as_nanos();
            let scheduled = u128::from(tick.millis) * 1_000_000 + u128::from(tick.fraction_ns);
            prop_assert_eq!(scheduled, expiry);
            prop_assert_eq!(tick.remaining(crate::timer_deadline::Deadline::from_nanos(now)), duration);
        }
    }
}
