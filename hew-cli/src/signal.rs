//! Scoped termination requests for interactive child runs, plus the CLI's
//! default SIGPIPE disposition.

use std::sync::atomic::{AtomicI32, Ordering};

const RUN_SIGNALS: [libc::c_int; 3] = [libc::SIGTERM, libc::SIGINT, libc::SIGHUP];
static RUN_SIGNAL: AtomicI32 = AtomicI32::new(0);

/// Record the first termination request. Child ownership and forwarding belong
/// to the wait loop, which can safely report errors, enforce a grace period and
/// reap the child. The handler never retains a PID that could be reused.
extern "C" fn request_termination(signal: libc::c_int) {
    let _ = RUN_SIGNAL.compare_exchange(0, signal, Ordering::SeqCst, Ordering::SeqCst);
}

/// Install before spawning so a termination request cannot orphan a child in
/// the gap between spawn and handler installation. Restore all previous
/// dispositions when the owned run has finished, including failed spawns.
pub(crate) struct RunSignals {
    previous: Vec<(libc::c_int, libc::sigaction)>,
}

impl RunSignals {
    pub(crate) fn install() -> Result<Self, String> {
        RUN_SIGNAL.store(0, Ordering::SeqCst);
        let mut guard = Self {
            previous: Vec::new(),
        };
        for signal in RUN_SIGNALS {
            // SAFETY: both actions are valid C structs; the handler performs
            // only lock-free atomic operations and retains no process handles.
            unsafe {
                let mut action: libc::sigaction = std::mem::zeroed();
                let mut previous: libc::sigaction = std::mem::zeroed();
                action.sa_sigaction = request_termination as *const () as libc::sighandler_t;
                libc::sigemptyset(&raw mut action.sa_mask);
                action.sa_flags = libc::SA_RESTART;
                if libc::sigaction(signal, &raw const action, &raw mut previous) != 0 {
                    return Err(format!(
                        "cannot install run signal handler: {}",
                        std::io::Error::last_os_error()
                    ));
                }
                guard.previous.push((signal, previous));
            }
        }
        Ok(guard)
    }

    #[expect(
        clippy::unused_self,
        reason = "request reads require an installed signal guard"
    )]
    pub(crate) fn requested(&self) -> Option<libc::c_int> {
        match RUN_SIGNAL.load(Ordering::SeqCst) {
            0 => None,
            signal => Some(signal),
        }
    }
}

impl Drop for RunSignals {
    fn drop(&mut self) {
        for (signal, previous) in self.previous.iter().rev() {
            // SAFETY: restore the exact disposition saved during installation.
            unsafe { libc::sigaction(*signal, previous, std::ptr::null_mut()) };
        }
    }
}

/// Restore SIGPIPE's DEFAULT disposition for the CLI process so a closed output
/// pipe (`hew check foo.hew 2>&1 | head -3`) terminates this process cleanly
/// instead of surfacing as a Rust I/O panic. Returns `false` if the reset
/// failed.
///
/// # Why this belongs at process start, not at child spawn
///
/// Rust installs `SIG_IGN` for SIGPIPE before `main`, so a write to a closed
/// pipe returns `EPIPE` and `println!`/`eprintln!` PANIC on it
/// (`failed printing to stderr`). The workspace builds with `panic = "abort"`,
/// so that panic is a SIGABRT — the compiler dies with exit 134 and a core
/// dump, having emitted nothing, on a run whose only unusual property is that
/// its consumer stopped reading.
///
/// That is exactly what `scripts/corpus-ratchet.sh stdlib` does when it REPORTS an
/// unexpected failure (`"$HEW_BIN" check "$path" 2>&1 | head -3`), and it is
/// why a plain diagnosable rejection reached CI as `exit 134` while the same
/// file run directly exited 1 with a full diagnostic. The reset used to be
/// reachable only from the child signal-forwarding setup — i.e. only on the
/// `hew run` child-spawn path — so every non-spawning subcommand (`check`,
/// `compile`, `fmt`, `doc`, …) kept the inherited `SIG_IGN` and could abort
/// this way.
///
/// This is deliberately the OPPOSITE of the compiled-program runtime, which
/// IGNORES SIGPIPE (`hew-runtime`'s `ignore_sigpipe`, installed in
/// `hew_sched_init`) so a broken-pipe network send fails closed to a typed
/// error instead of killing the program. The CLI and the emitted program are
/// separate binaries/processes and the CLI never calls `hew_sched_init`, so the
/// two dispositions never collide.
pub fn reset_sigpipe_to_default() -> bool {
    // SAFETY: `signal(2)` with SIG_DFL for SIGPIPE is async-signal-safe and the
    // canonical "restore default disposition" call.
    let prev = unsafe { libc::signal(libc::SIGPIPE, libc::SIG_DFL) };
    prev != libc::SIG_ERR
}

#[cfg(test)]
mod tests {
    use super::*;
    /// The CLI must RESET SIGPIPE to its default disposition (unlike the
    /// compiled-program runtime, which IGNORES it) so `hew run foo | head`
    /// terminates cleanly. Simulate an inherited `SIG_IGN` and verify the reset
    /// flips it back to `SIG_DFL`. This touches ONLY SIGPIPE — not the
    /// process-global SIGTERM/SIGINT handlers the other test avoids.
    #[test]
    fn reset_sigpipe_restores_default_disposition() {
        // SAFETY: installing SIG_IGN for SIGPIPE is async-signal-safe and valid.
        unsafe {
            assert_ne!(
                libc::signal(libc::SIGPIPE, libc::SIG_IGN),
                libc::SIG_ERR,
                "test setup: could not install SIG_IGN"
            );
        }

        assert!(
            reset_sigpipe_to_default(),
            "reset_sigpipe_to_default must succeed"
        );

        // SAFETY: an all-zero `libc::sigaction` is a valid initialised value (a
        // plain C struct); it is immediately overwritten by the query below.
        let mut cur: libc::sigaction = unsafe { std::mem::zeroed() };
        // SAFETY: a null `act` makes `sigaction` a pure query into the valid
        // `cur` out-param; the signal number is valid.
        let rc = unsafe { libc::sigaction(libc::SIGPIPE, std::ptr::null(), &raw mut cur) };
        assert_eq!(rc, 0, "sigaction(SIGPIPE) query failed");
        assert_eq!(
            cur.sa_sigaction,
            libc::SIG_DFL,
            "the CLI must leave SIGPIPE at SIG_DFL so `hew run | head` terminates cleanly"
        );
    }
}
