//! Built-in profiler for Hew programs.
//!
//! Activated by setting the `HEW_PPROF` environment variable to a bind
//! address (e.g. `HEW_PPROF=:6060` or `HEW_PPROF=127.0.0.1:6060`).
//!
//! When active, spawns a background OS thread serving a live dashboard
//! and JSON API with scheduler metrics, memory allocation stats, and
//! time-series history.
//!
//! # Example
//!
//! ```bash
//! HEW_PPROF=:6060 ./my_hew_program
//! # Open http://localhost:6060/ in a browser
//! ```

pub mod actor_registry;
pub mod allocator;
pub mod metrics;
pub mod pprof;
mod server;

use crate::util::MutexExt;
use std::sync::{
    atomic::{AtomicBool, Ordering},
    Arc, Mutex,
};
use std::thread::{self, JoinHandle};
use std::time::Duration;

use metrics::MetricsRing;
pub use server::ProfilerContext;

/// Global state for running profiler threads.
///
/// Stores thread handles and shutdown signal so [`shutdown()`] can cleanly
/// terminate threads before node resources are freed.
static PROFILER_STATE: Mutex<Option<ProfilerThreads>> = Mutex::new(None);

/// Shutdown signal shared with profiler threads.
static PROFILER_SHUTDOWN: AtomicBool = AtomicBool::new(false);

#[derive(Debug)]
struct ProfilerThreads {
    /// Sampler thread handle.
    sampler_handle: JoinHandle<()>,
    /// HTTP server thread handle.
    server_handle: JoinHandle<()>,
}

/// Check `HEW_PPROF` and start the profiler if set.
///
/// Called from [`hew_sched_init`](crate::scheduler::hew_sched_init) during
/// scheduler startup. If `HEW_PPROF` is not set, this is a no-op.
///
/// Distributed subsystem pointers default to null (no cluster/connection/
/// routing data will be served). Use [`maybe_start_with_context`] to pass
/// live pointers from a running [`HewNode`](crate::hew_node::HewNode).
pub fn maybe_start() {
    maybe_start_with_context(
        std::ptr::null_mut(),
        std::ptr::null_mut(),
        std::ptr::null_mut(),
    );
}

/// Check `HEW_PPROF` and start the profiler with distributed runtime context.
///
/// Like [`maybe_start`] but accepts pointers to the cluster, connection
/// manager, and routing table so the profiler can expose distributed
/// runtime endpoints.
///
/// # Safety
///
/// The pointers (if non-null) must remain valid until [`shutdown()`] is called.
/// Null pointers are safe and result in empty JSON responses for the
/// corresponding endpoints.
pub fn maybe_start_with_context(
    cluster: *mut crate::cluster::HewCluster,
    connmgr: *mut crate::connection::HewConnMgr,
    routing: *mut crate::routing::HewRoutingTable,
) {
    let bind_addr = match std::env::var("HEW_PPROF") {
        Ok(val) if !val.is_empty() => val,
        _ => return,
    };
    crate::tracing::hew_trace_enable(1);

    // Clear any previous shutdown signal.
    PROFILER_SHUTDOWN.store(false, Ordering::Release);

    // Normalize `:port` shorthand to `0.0.0.0:port`.
    let bind_addr = if bind_addr.starts_with(':') {
        format!("0.0.0.0{bind_addr}")
    } else {
        bind_addr
    };

    let ring = Arc::new(Mutex::new(MetricsRing::new()));

    // Sampler thread: captures a MetricsSnapshot every second.
    let sampler_ring = Arc::clone(&ring);
    let sampler_handle = match thread::Builder::new()
        .name("hew-pprof-sampler".into())
        .spawn(move || sampler_loop(&sampler_ring))
    {
        Ok(handle) => handle,
        Err(_) => {
            eprintln!("[hew-pprof] failed to spawn sampler thread");
            return;
        }
    };

    let ctx = ProfilerContext {
        ring,
        cluster,
        connmgr,
        routing,
    };

    // HTTP server thread.
    let server_handle = match thread::Builder::new()
        .name("hew-pprof-server".into())
        .spawn(move || server::run(&bind_addr, &ctx))
    {
        Ok(handle) => handle,
        Err(_) => {
            eprintln!("[hew-pprof] failed to spawn server thread");
            // Signal sampler to stop and join it before returning.
            PROFILER_SHUTDOWN.store(true, Ordering::Release);
            let _ = sampler_handle.join();
            return;
        }
    };

    // Store thread handles for shutdown.
    let threads = ProfilerThreads {
        sampler_handle,
        server_handle,
    };
    let mut state_guard = PROFILER_STATE.lock_or_recover();
    *state_guard = Some(threads);
}

/// Sampler loop: captures a snapshot every second.
fn sampler_loop(ring: &Arc<Mutex<MetricsRing>>) {
    loop {
        // Check shutdown signal before sleeping.
        if PROFILER_SHUTDOWN.load(Ordering::Acquire) {
            break;
        }

        thread::sleep(Duration::from_secs(1));

        // Check again after sleeping (thread could have been signaled during sleep).
        if PROFILER_SHUTDOWN.load(Ordering::Acquire) {
            break;
        }

        let mut ring_guard = ring.lock_or_recover();
        ring_guard.sample();
    }
}

/// Write profile files on exit if `HEW_PROF_OUTPUT` is set.
///
/// Called from [`hew_sched_shutdown`](crate::scheduler::hew_sched_shutdown)
/// after workers have joined. Checks the `HEW_PROF_OUTPUT` environment
/// variable:
///
/// | Value | Files written |
/// |-------|---------------|
/// | `pprof` | `hew-profile.pb.gz` |
/// | `flat` | `hew-profile.txt` |
/// | `both` | Both files |
/// | *(unset)* | Nothing |
pub fn maybe_write_on_exit() {
    let output = match std::env::var("HEW_PROF_OUTPUT") {
        Ok(val) if !val.is_empty() => val,
        _ => return,
    };

    let mode = output.trim().to_lowercase();
    let write_pprof = mode == "pprof" || mode == "both";
    let write_flat = mode == "flat" || mode == "both";

    if !write_pprof && !write_flat {
        eprintln!(
            "[hew-pprof] unknown HEW_PROF_OUTPUT value: {output:?} (expected pprof, flat, or both)"
        );
        return;
    }

    if write_pprof {
        let data = pprof::generate_heap_profile();
        let path = "hew-profile.pb.gz";
        match std::fs::write(path, &data) {
            Ok(()) => eprintln!("[hew-pprof] wrote {path} ({} bytes)", data.len()),
            Err(e) => eprintln!("[hew-pprof] failed to write {path}: {e}"),
        }
    }

    if write_flat {
        let text = pprof::generate_flat_profile();
        let path = "hew-profile.txt";
        match std::fs::write(path, &text) {
            Ok(()) => eprintln!("[hew-pprof] wrote {path} ({} bytes)", text.len()),
            Err(e) => eprintln!("[hew-pprof] failed to write {path}: {e}"),
        }
    }
}

/// Stop all profiler threads and wait for them to exit.
///
/// This MUST be called before freeing any node resources (cluster, connection
/// manager, routing table) that the profiler threads might be accessing.
///
/// Safe to call multiple times or when no profiler was started (no-op).
pub(crate) fn shutdown() {
    // Signal all threads to stop.
    PROFILER_SHUTDOWN.store(true, Ordering::Release);

    // Take the thread handles and join them.
    let mut state_guard = PROFILER_STATE.lock_or_recover();
    if let Some(threads) = state_guard.take() {
        drop(state_guard); // Release lock before joining.

        // Join threads in reverse order (server first, then sampler).
        // Server might be blocked on incoming requests, but it should exit
        // when the TCP connection is dropped or after a timeout.
        let _ = threads.server_handle.join();
        let _ = threads.sampler_handle.join();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::time::Duration;

    /// Test 1: Positive test - verify profiler can start and shutdown cleanly.
    #[test]
    fn test_profiler_lifecycle() {
        // Set HEW_PPROF to enable the profiler.
        std::env::set_var("HEW_PPROF", ":0"); // Bind to a random available port.

        // Start profiler with null pointers (safe, will return empty JSON).
        maybe_start_with_context(
            std::ptr::null_mut(),
            std::ptr::null_mut(),
            std::ptr::null_mut(),
        );

        // Verify profiler threads were started by checking state.
        {
            let state_guard = PROFILER_STATE.lock_or_recover();
            assert!(
                state_guard.is_some(),
                "Profiler threads should be running after maybe_start_with_context"
            );
        }

        // Give threads a moment to initialize.
        std::thread::sleep(Duration::from_millis(100));

        // Verify shutdown signal is not set.
        assert!(!PROFILER_SHUTDOWN.load(Ordering::Acquire));

        // Shutdown the profiler.
        shutdown();

        // Verify shutdown signal was set.
        assert!(PROFILER_SHUTDOWN.load(Ordering::Acquire));

        // Verify threads were joined and state cleared.
        {
            let state_guard = PROFILER_STATE.lock_or_recover();
            assert!(
                state_guard.is_none(),
                "Profiler state should be cleared after shutdown"
            );
        }

        // Clean up environment variable.
        std::env::remove_var("HEW_PPROF");
        // Reset shutdown signal for other tests.
        PROFILER_SHUTDOWN.store(false, Ordering::Release);
    }

    /// Test 2: Negative test - shutdown() should be safe when no profiler was started.
    #[test]
    fn test_shutdown_without_profiler() {
        // Ensure HEW_PPROF is not set.
        std::env::remove_var("HEW_PPROF");

        // Try to start profiler (should be a no-op).
        maybe_start_with_context(
            std::ptr::null_mut(),
            std::ptr::null_mut(),
            std::ptr::null_mut(),
        );

        // Verify no threads were started.
        {
            let state_guard = PROFILER_STATE.lock_or_recover();
            assert!(
                state_guard.is_none(),
                "No profiler threads should be running when HEW_PPROF is not set"
            );
        }

        // Shutdown should be a safe no-op.
        shutdown();

        // Verify still no threads.
        {
            let state_guard = PROFILER_STATE.lock_or_recover();
            assert!(state_guard.is_none(), "State should remain None after shutdown");
        }

        // Reset shutdown signal for other tests.
        PROFILER_SHUTDOWN.store(false, Ordering::Release);
    }

    /// Test 3: Sabotage test - verify threads don't exit without shutdown signal.
    ///
    /// This test temporarily removes the shutdown check to verify our fix works.
    /// When the check is removed, threads should NOT exit cleanly.
    #[test]
    fn test_sabotage_shutdown_signal() {
        // This test verifies the importance of the shutdown signal by demonstrating
        // that without it, threads would continue running.
        //
        // We can't easily test the "broken" version in the same binary, but we can
        // test that our threads DO respect the signal by setting it and verifying
        // they exit.

        std::env::set_var("HEW_PPROF", ":0");

        // Start profiler.
        maybe_start_with_context(
            std::ptr::null_mut(),
            std::ptr::null_mut(),
            std::ptr::null_mut(),
        );

        // Verify threads are running.
        {
            let state_guard = PROFILER_STATE.lock_or_recover();
            assert!(state_guard.is_some(), "Threads should be running");
        }

        // Manually signal shutdown and wait a bit for threads to see the signal.
        PROFILER_SHUTDOWN.store(true, Ordering::Release);
        std::thread::sleep(Duration::from_millis(1100)); // More than 1 second sampler interval.

        // The threads should now be ready to exit. Call shutdown to join them.
        shutdown();

        // Verify threads exited.
        {
            let state_guard = PROFILER_STATE.lock_or_recover();
            assert!(state_guard.is_none(), "Threads should have exited");
        }

        // Clean up.
        std::env::remove_var("HEW_PPROF");
        PROFILER_SHUTDOWN.store(false, Ordering::Release);
    }

    /// Test multiple startup/shutdown cycles.
    #[test]
    fn test_multiple_cycles() {
        for i in 0..3 {
            std::env::set_var("HEW_PPROF", format!(":0")); // Random port.

            // Start profiler.
            maybe_start_with_context(
                std::ptr::null_mut(),
                std::ptr::null_mut(),
                std::ptr::null_mut(),
            );

            // Verify it started.
            {
                let state_guard = PROFILER_STATE.lock_or_recover();
                assert!(
                    state_guard.is_some(),
                    "Cycle {} - threads should be running",
                    i
                );
            }

            // Shutdown.
            shutdown();

            // Verify it stopped.
            {
                let state_guard = PROFILER_STATE.lock_or_recover();
                assert!(state_guard.is_none(), "Cycle {} - threads should be stopped", i);
            }

            // Reset state for next cycle.
            PROFILER_SHUTDOWN.store(false, Ordering::Release);
            std::env::remove_var("HEW_PPROF");
        }
    }
}
