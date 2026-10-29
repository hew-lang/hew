use super::*;
use proptest::prelude::*;

proptest! {
    #![proptest_config(ProptestConfig { cases: 512, ..ProptestConfig::default() })]
    #[test]
    fn generated_circuit_breaker_windows_match_full_crash_history(
        start in prop_oneof![any::<u64>(), (u64::MAX - 1_000_000_000)..=u64::MAX],
        threshold in 0u32..=96,
        window_secs in 0u32..=60,
        delays in prop::collection::vec(0u64..=4_000_000_000, 1..256),
    ) {
        let _rt = crate::runtime_test_guard();
        let mut breaker = CircuitBreakerState::default();
        breaker.max_crashes = threshold;
        breaker.window_secs = window_secs;
        // The oracle retains every event on an unbounded logical clock;
        // production retains only the configured threshold of u64 timestamps.
        let mut clock = u128::from(start);
        let mut events = Vec::new();
        let mut opened = None;
        for delay in delays {
            clock += u128::from(delay);
            events.push(clock);
            let count = events.iter().filter(|&&time| {
                clock - time <= u128::from(window_secs) * 1_000_000_000
            }).count();
            let now = u64::try_from(clock % (1u128 << 64)).unwrap();
            if threshold != 0 && count >= usize::try_from(threshold).unwrap() {
                opened.get_or_insert(now);
            }
            circuit_breaker_record_crash_at(&mut breaker, 1, 0, now);
            prop_assert_eq!(breaker.state, i32::from(opened.is_some()));
            if let Some(stamp) = opened {
                prop_assert_eq!(breaker.opened_at_ns, stamp);
            }
            let retained = if threshold == 0 { 0 } else {
                count.min(usize::try_from(threshold).unwrap())
            };
            prop_assert_eq!(breaker.crash_times.len(), retained);
        }
    }
}

#[test]
fn circuit_breaker_window_expires_old_crashes_and_includes_boundary() {
    let _rt = crate::runtime_test_guard();
    let mut breaker = CircuitBreakerState::default();
    breaker.max_crashes = 3;
    breaker.window_secs = 1;
    for now in [0, 1_000_000_000, 1_000_000_001, 2_000_000_001] {
        circuit_breaker_record_crash_at(&mut breaker, 1, 0, now);
        assert_eq!(
            breaker.state, 0,
            "expired crashes must not trip the breaker"
        );
    }
    circuit_breaker_record_crash_at(&mut breaker, 1, 0, 2_000_000_001);
    assert_eq!(
        breaker.state, 1,
        "the exact window boundary remains included"
    );
}
