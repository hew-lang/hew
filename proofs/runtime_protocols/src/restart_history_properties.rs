use super::*;
use proptest::prelude::*;
proptest! {
    #![proptest_config(ProptestConfig{cases:1024,..ProptestConfig::default()})]
    #[test]
    fn generated_crash_histories_obey_configured_restart_budget(
        start in any::<u64>(), limit in 0usize..100, window in 0u16..100,
        delays in prop::collection::vec(0u16..5000,1..150)
    ){
        let mut history=RestartHistory::default();
        let mut accepted=Vec::new();let mut now=start;
        for delay in delays {
            now=now.wrapping_add(u64::from(delay));
            let expected=accepted.iter().filter(|&&stamp|now.wrapping_sub(stamp)<=u64::from(window)*1000).count();
            prop_assert_eq!(history.recent(now,i32::from(window)),i32::try_from(expected).unwrap());
            if expected<limit{history.record(now,limit);accepted.push(now);}
            prop_assert!(history.0.len()<=limit);
        }
    }
}
