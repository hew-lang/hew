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
pub mod discovery;
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
pub(super) static PROFILER_SHUTDOWN: AtomicBool = AtomicBool::new(false);

/// Discovery directory path, stored so shutdown can clean up the socket
/// and discovery file.
static PROFILER_DISCOVERY_DIR: Mutex<Option<std::path::PathBuf>> = Mutex::new(None);

#[derive(Debug)]
struct ProfilerThreads {
    /// Sampler thread handle.
    sampler_handle: JoinHandle<()>,
    /// HTTP server thread handle.
    server_handle: JoinHandle<()>,
}

/// How the profiler should listen for connections.
enum ListenMode {
    /// Bind to a TCP address (e.g. `0.0.0.0:6060`).
    Tcp(String),
    /// Bind to a per-user unix domain socket with auto-discovery.
    Unix,
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
    let env_val = match std::env::var("HEW_PPROF") {
        Ok(val) if !val.is_empty() => val,
        _ => return,
    };
    crate::tracing::hew_trace_enable(1);

    let mode = parse_listen_mode(&env_val);

    // Clear any previous shutdown signal.
    PROFILER_SHUTDOWN.store(false, Ordering::Release);

    let ring = Arc::new(Mutex::new(MetricsRing::new()));

    // Sampler thread: captures a MetricsSnapshot every second.
    let sampler_ring = Arc::clone(&ring);
    let Ok(sampler_handle) = thread::Builder::new()
        .name("hew-pprof-sampler".into())
        .spawn(move || sampler_loop(&sampler_ring))
    else {
        eprintln!("[hew-pprof] failed to spawn sampler thread");
        return;
    };

    let ctx = Arc::new(ProfilerContext {
        ring,
        cluster,
        connmgr,
        routing,
    });

    // HTTP server thread — TCP or Unix depending on mode.
    let server_handle = match mode {
        ListenMode::Tcp(bind_addr) => thread::Builder::new()
            .name("hew-pprof-server".into())
            .spawn(move || server::run_tcp(&bind_addr, ctx)),
        ListenMode::Unix => {
            let Some(disc_dir) = discovery::discovery_dir() else {
                eprintln!("[hew-pprof] could not resolve a safe discovery directory");
                PROFILER_SHUTDOWN.store(true, Ordering::Release);
                let _ = sampler_handle.join();
                return;
            };

            let sock_path = discovery::socket_path(&disc_dir);

            // Remove stale socket from a previous crashed run.
            let _ = std::fs::remove_file(&sock_path);

            // Store discovery dir so shutdown() can clean up.
            *PROFILER_DISCOVERY_DIR.lock_or_recover() = Some(disc_dir.clone());

            thread::Builder::new()
                .name("hew-pprof-server".into())
                .spawn(move || {
                    // Write discovery file after we're on the server thread
                    // (the socket is created inside run_unix).
                    server::run_unix(&sock_path, ctx);
                })
        }
    };

    let Ok(server_handle) = server_handle else {
        eprintln!("[hew-pprof] failed to spawn server thread");
        PROFILER_SHUTDOWN.store(true, Ordering::Release);
        let _ = sampler_handle.join();
        return;
    };

    // For unix mode, write the discovery file now that the thread is running.
    if let Some(disc_dir) = PROFILER_DISCOVERY_DIR.lock_or_recover().as_ref() {
        let sock_path = discovery::socket_path(disc_dir);
        if let Err(e) = discovery::write_discovery_file(disc_dir, &sock_path) {
            eprintln!("[hew-pprof] failed to write discovery file: {e}");
        }
    }

    // Store thread handles for shutdown.
    let threads = ProfilerThreads {
        sampler_handle,
        server_handle,
    };
    let mut state_guard = PROFILER_STATE.lock_or_recover();
    *state_guard = Some(threads);
}

/// Parse the `HEW_PPROF` env var into a listen mode.
fn parse_listen_mode(val: &str) -> ListenMode {
    match val {
        "auto" | "1" | "true" | "yes" => ListenMode::Unix,
        addr if addr.starts_with(':') => ListenMode::Tcp(format!("0.0.0.0{addr}")),
        addr => ListenMode::Tcp(addr.to_owned()),
    }
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
    // Signal all threads to stop. The server's tokio accept loop checks
    // this flag every 200ms via `tokio::select!` with a sleep timeout.
    PROFILER_SHUTDOWN.store(true, Ordering::Release);

    // Take the thread handles and join them.
    let mut state_guard = PROFILER_STATE.lock_or_recover();
    if let Some(threads) = state_guard.take() {
        drop(state_guard); // Release lock before joining.

        let _ = threads.server_handle.join();
        let _ = threads.sampler_handle.join();
    }

    // Clean up unix socket and discovery file if we were in unix mode.
    if let Some(disc_dir) = PROFILER_DISCOVERY_DIR.lock_or_recover().take() {
        discovery::cleanup(&disc_dir);
    }
}
