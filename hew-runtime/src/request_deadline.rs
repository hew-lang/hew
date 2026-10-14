//! Non-cancelling request metadata, measured on this process's monotonic clock.

/// An absolute local expiry. The compiler-private ABI uses -1 for absence.
#[repr(transparent)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct RequestDeadline(i64);

impl Default for RequestDeadline {
    fn default() -> Self {
        Self(-1)
    }
}

impl RequestDeadline {
    pub(crate) const fn from_raw(expiry_ns: i64) -> Self {
        Self(expiry_ns)
    }

    pub(crate) const fn raw(self) -> i64 {
        self.0
    }

    pub(crate) fn after(duration_ns: i64) -> Self {
        // SAFETY: the monotonic clock has no preconditions.
        Self(unsafe { crate::clock::hew_instant_now() }.saturating_add(duration_ns.max(0)))
    }

    pub(crate) fn earlier(self, other: Self) -> Self {
        match (self.0 >= 0, other.0 >= 0) {
            (true, true) => Self(self.0.min(other.0)),
            (false, _) => other,
            (_, false) => self,
        }
    }

    pub(crate) fn remaining(self) -> Option<i64> {
        // SAFETY: the monotonic clock has no preconditions.
        (self.0 >= 0).then(|| {
            self.0
                .saturating_sub(unsafe { crate::clock::hew_instant_now() })
        })
    }

    pub(crate) fn expired(self) -> bool {
        self.remaining().is_some_and(|remaining| remaining <= 0)
    }

    pub(crate) fn current() -> Self {
        // SAFETY: the installed context remains live throughout this invocation.
        unsafe { crate::execution_context::current_context().as_ref() }
            .map_or_else(Self::default, |context| context.request_deadline)
    }
}
