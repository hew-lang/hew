//! Signal forwarding: when `hew run` receives SIGTERM or SIGINT, forward it to
//! the compiled child binary so it is not orphaned.

use std::sync::atomic::{AtomicI32, Ordering};

/// PID of the child process to receive forwarded signals.
/// Written once before the child is waited on; read only inside the handler.
static CHILD_PID: AtomicI32 = AtomicI32::new(0);

#[allow(
    clippy::cast_possible_wrap,
    reason = "PIDs fit in i32 on all supported platforms"
)]
fn set_child_pid(child_pid: u32) {
    CHILD_PID.store(child_pid as i32, Ordering::SeqCst);
}

fn clear_child_pid() {
    CHILD_PID.store(0, Ordering::SeqCst);
}

/// Signal handler: forward the received signal to the child process.
/// SAFETY: only async-signal-safe operations are performed (kill, atomic load).
unsafe extern "C" fn forward_to_child(sig: libc::c_int) {
    let pid = CHILD_PID.load(Ordering::SeqCst);
    if pid > 0 {
        // SAFETY: kill(2) is async-signal-safe.
        unsafe { libc::kill(pid, sig) };
    }
}

/// Installs SIGTERM and SIGINT handlers that forward the signal to `child_pid`.
/// Call this once after the child is spawned and before
/// `Child::wait`.  The parent process is *not* terminated by the handler —
/// it continues and `Child::wait` will return once the child has exited.
pub fn forward_signals_to_child(child_pid: u32) {
    set_child_pid(child_pid);

    // SAFETY: zeroed sigaction is a valid starting value; `forward_to_child`
    // performs only async-signal-safe operations.  SA_RESTART ensures that
    // syscalls interrupted by the signal (e.g. waitpid) are restarted.
    unsafe {
        let mut sa: libc::sigaction = std::mem::zeroed();
        sa.sa_sigaction = forward_to_child as *const () as libc::sighandler_t;
        libc::sigemptyset(&raw mut sa.sa_mask);
        sa.sa_flags = libc::SA_RESTART;

        let term_result = libc::sigaction(libc::SIGTERM, &raw const sa, std::ptr::null_mut());
        let int_result = libc::sigaction(libc::SIGINT, &raw const sa, std::ptr::null_mut());

        if term_result != 0 || int_result != 0 {
            clear_child_pid();
            eprintln!("hew: warning: failed to install signal forwarding handlers");
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::process::{Child, Command, ExitStatus};
    use std::thread;
    use std::time::Duration;

    fn wait_for_child_exit(child: &mut Child) -> ExitStatus {
        for _ in 0..20 {
            if let Some(status) = child.try_wait().expect("failed to poll child") {
                return status;
            }
            thread::sleep(Duration::from_millis(50));
        }

        child.kill().ok();
        let _ = child.wait();
        panic!("child did not exit after forwarded signal");
    }

    #[test]
    fn handler_forwards_sigterm_and_sigint_to_child() {
        for sig in [libc::SIGTERM, libc::SIGINT] {
            let mut child = Command::new("sleep")
                .arg("60")
                .spawn()
                .expect("failed to spawn sleep");
            set_child_pid(child.id());

            // Call the handler directly so the test exercises the forwarding
            // logic without mutating process-global signal handlers.
            unsafe { forward_to_child(sig) };

            let status = wait_for_child_exit(&mut child);
            clear_child_pid();

            // A process killed by a signal has no numeric exit code on Unix.
            assert!(
                status.code().is_none(),
                "child should have been killed by signal, not exited normally"
            );
        }
    }
}
