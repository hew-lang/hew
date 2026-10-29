/// Per-continuation lifecycle tag the slice-4 executor CAS-transitions to
/// serialize resume and destroy against a single parked `HewCont` handle.
///
/// The continuation REPRESENTATION (`cont.rs`) is fail-closed on a NULL handle
/// but has no scheduler state, so it cannot detect a *double-resume*, a
/// *destroy-after-destroy*, or a *concurrent resume+destroy* on a non-null
/// dangling handle — those are exactly the executor's job (FG1, FG2, FG4).
/// This tag is the executor's serialization point: every transition is a CAS
/// from a single expected current tag, so an unexpected current tag fails
/// closed (the caller refuses the operation) rather than corrupting memory.
///
/// The per-actor lock (`hew_actor_state_lock_*`) is RELEASED while an actor is
/// `Suspended` (the suspend edge must release it so senders do not deadlock),
/// so this tag — not the actor lock — is what serializes resume vs destroy on
/// the handle.
///
/// Lifecycle (the only legal transitions):
/// ```text
///   (slot empty) --park--> Parked
///   Parked --resume--> Resuming --(Pending)--> Parked   (suspended again)
///   Parked --resume--> Resuming --(Ready)----> Done --destroy--> Destroyed
///   Parked --abandon--> Destroyed                    (cancel — skips Done)
///   Done   --destroy--> Destroyed
/// ```
/// A second `resume` (tag already `Resuming`/`Done`/`Destroyed`) refuses; a
/// second `destroy` (tag already `Destroyed`) refuses. `Destroyed` is terminal
/// and the slot is nulled in the same critical section (FG4: no use-after-
/// destroy).
#[repr(i32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ContTag {
    /// No continuation is parked on the actor (the slot is null). This is the
    /// initial / quiescent tag and the value a fresh `HewActor` zero-inits to.
    Empty = 0,
    /// A continuation is parked against a readiness source, suspended at a
    /// non-final `coro.suspend`, awaiting resume. The handle is live and
    /// `resume` may transition it to `Resuming`.
    Parked = 1,
    /// The executor is currently driving `hew_cont_resume` on the handle. No
    /// concurrent `resume` or `destroy` may proceed; both refuse against this
    /// tag (FG2). Returns to `Parked` on `Pending` or advances to `Done` on
    /// `Ready`.
    Resuming = 2,
    /// The continuation reached its final suspend (`ResumePoll::Ready`). It
    /// awaits exactly one `destroy`. `resume` refuses against this tag.
    /// Abandoned continuations skip this state and go directly to `Destroyed`
    /// via a `Parked → Destroyed` CAS in `destroy_parked`.
    Done = 3,
    /// The continuation has been destroyed exactly once (FG1). Terminal:
    /// every further `resume` and `destroy` refuses (FG2/FG4), and the actor's
    /// `suspended_cont` slot was nulled in the same critical section as this
    /// transition so no later activation reads the freed handle.
    Destroyed = 4,
}

impl ContTag {
    /// Convert from the raw `i32` representation. Returns `None` for any value
    /// outside the legal tag range — a fail-closed decode for the executor's
    /// CAS, which only ever compares against a known-good expected tag.
    #[must_use]
    pub fn from_i32(v: i32) -> Option<Self> {
        match v {
            0 => Some(Self::Empty),
            1 => Some(Self::Parked),
            2 => Some(Self::Resuming),
            3 => Some(Self::Done),
            4 => Some(Self::Destroyed),
            _ => None,
        }
    }
}
