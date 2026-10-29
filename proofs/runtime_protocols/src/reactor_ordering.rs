use crate::reactor_wait::{inserted, sleep_limit};
use crate::runtime_sync::AtomicU64;
use crate::timer_deadline::Deadline;

#[kani::proof]
fn reactor_wake_preserves_every_deadline_order() {
    let parked = Deadline {
        millis: kani::any(),
        fraction_ns: kani::any(),
    };
    let incoming = Deadline {
        millis: kani::any(),
        fraction_ns: kani::any(),
    };
    kani::assume(parked.millis < u64::MAX && incoming.millis < u64::MAX);
    kani::assume(parked.fraction_ns < 1_000_000 && incoming.fraction_ns < 1_000_000);
    let empty: bool = kani::any();
    let limit = sleep_limit((!empty).then_some(parked));
    let mut woke = false;
    inserted(&AtomicU64::new(limit), sleep_limit(Some(incoming)), || {
        woke = true
    });

    // Independent wider arithmetic orders the full wheel range, including
    // deadlines whose nanosecond encoding saturates. No sampled clock bounds.
    let old_ns = u128::from(parked.millis) * 1_000_000 + u128::from(parked.fraction_ns);
    let new_ns = u128::from(incoming.millis) * 1_000_000 + u128::from(incoming.fraction_ns);
    if empty || new_ns < old_ns {
        assert!(
            woke,
            "reactor: earlier insertion must notify a parked reactor"
        );
    }
    if !empty && old_ns < u128::from(u64::MAX - 1) && new_ns >= old_ns {
        assert!(
            !woke,
            "reactor: exact later deadlines must not generate a wake"
        );
    }
    kani::cover!(empty && woke, "empty wheel notification");
    kani::cover!(
        !empty && new_ns < old_ns && old_ns < u128::from(u64::MAX - 1) && woke,
        "earlier exact deadline notification"
    );
    kani::cover!(
        !empty && new_ns < old_ns && new_ns >= u128::from(u64::MAX - 1) && woke,
        "distinct saturated deadlines still notify"
    );
    kani::cover!(
        !empty && new_ns >= old_ns && !woke,
        "later exact deadline remains quiet"
    );
}
