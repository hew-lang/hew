use std::time::Duration;

/// Exact monotonic deadline. Millisecond buckets index the wheel; the fraction
/// controls eligibility. Keeping the fraction in the entry's former padding
/// preserves its allocation size on 64-bit targets.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct Deadline {
    pub(crate) millis: u64,
    pub(crate) fraction_ns: u32,
}

impl Deadline {
    pub(crate) const fn from_millis(millis: u64) -> Self {
        Self {
            millis,
            fraction_ns: 0,
        }
    }

    pub(crate) fn from_nanos(nanos: u64) -> Self {
        Self {
            millis: nanos / 1_000_000,
            fraction_ns: (nanos % 1_000_000) as u32,
        }
    }

    pub(crate) fn checked_add(self, duration: Duration) -> Option<Self> {
        let fraction = u64::from(self.fraction_ns) + u64::from(duration.subsec_nanos());
        Some(Self {
            millis: self
                .millis
                .checked_add(duration.as_secs().checked_mul(1_000)?)?
                .checked_add(fraction / 1_000_000)?,
            fraction_ns: (fraction % 1_000_000) as u32,
        })
    }

    pub(crate) fn ceil_millis(self) -> Option<u64> {
        self.millis.checked_add(u64::from(self.fraction_ns != 0))
    }

    pub(crate) fn remaining(self, now: Self) -> Duration {
        if self <= now {
            return Duration::ZERO;
        }
        let millis = self.millis - now.millis;
        if self.fraction_ns >= now.fraction_ns {
            Duration::from_millis(millis)
                + Duration::from_nanos(u64::from(self.fraction_ns - now.fraction_ns))
        } else {
            Duration::from_millis(millis - 1)
                + Duration::from_nanos(
                    1_000_000 + u64::from(self.fraction_ns) - u64::from(now.fraction_ns),
                )
        }
    }

    pub(crate) fn saturated_nanos(self) -> u64 {
        self.millis
            .saturating_mul(1_000_000)
            .saturating_add(u64::from(self.fraction_ns))
    }
}

/// Add a duration without quantizing its absolute deadline.
pub(crate) fn after(now_ns: u64, duration: Duration) -> Option<Deadline> {
    Deadline::from_nanos(now_ns).checked_add(duration)
}

#[cfg(all(test, not(hew_protocol_host), not(target_arch = "wasm32")))]
#[path = "../../proofs/runtime_protocols/src/timer_deadline_properties.rs"]
mod failure_properties;
