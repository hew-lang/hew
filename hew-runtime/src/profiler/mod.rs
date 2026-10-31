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
#[cfg(unix)]
pub mod discovery;
pub mod metrics;
pub mod pprof;
mod server;

use crate::lifetime::PoisonSafe;
use crate::util::MutexExt;
use std::sync::{
    atomic::{AtomicBool, Ordering},
    mpsc, Arc, Mutex,
};
use std::thread::{self, JoinHandle};
use std::time::{Duration, Instant};
#[cfg(unix)]
use std::{os::unix::fs::PermissionsExt as _, path::PathBuf};

use metrics::MetricsRing;
pub use server::{run_tcp_with_listener, ProfilerContext, OBSERVE_SCHEMA_VERSION};
pub use tokio::sync::oneshot;

/// Global state for running profiler threads.
///
/// Stores thread handles and shutdown signal so [`shutdown()`] can cleanly
/// terminate threads before node resources are freed.
static PROFILER_STATE: PoisonSafe<Option<ProfilerThreads>> = PoisonSafe::new(None);

/// Shutdown signal shared with profiler threads.
pub(super) static PROFILER_SHUTDOWN: AtomicBool = AtomicBool::new(false);

/// Discovery directory path, stored so shutdown can clean up the socket
/// and discovery file.
#[cfg(unix)]
static PROFILER_DISCOVERY_DIR: PoisonSafe<Option<std::path::PathBuf>> = PoisonSafe::new(None);

#[derive(Debug)]
struct ProfilerThreads {
    /// Sampler thread handle.
    sampler_handle: JoinHandle<()>,
    /// HTTP server thread handle.
    server_handle: JoinHandle<()>,
    /// Wake the server's accept loop during shutdown.
    server_shutdown: tokio::sync::oneshot::Sender<()>,
}

/// How the profiler should listen for connections.
enum ListenMode {
    /// Bind to a TCP address (e.g. `0.0.0.0:6060`).
    Tcp(String),
    /// Bind to a per-user unix domain socket with auto-discovery.
    #[cfg(unix)]
    Unix,
}

fn stop_sampler_after_startup_failure(sampler_handle: JoinHandle<()>) {
    PROFILER_SHUTDOWN.store(true, Ordering::Release);
    sampler_handle.thread().unpark();
    crate::util::report_join_panic("hew profiler sampler thread", sampler_handle.join());
}

fn stop_threads_after_startup_failure(
    sampler_handle: JoinHandle<()>,
    server_handle: JoinHandle<()>,
    server_shutdown: tokio::sync::oneshot::Sender<()>,
) {
    PROFILER_SHUTDOWN.store(true, Ordering::Release);
    let _ = server_shutdown.send(());
    sampler_handle.thread().unpark();
    crate::util::report_join_panic("hew profiler server thread", server_handle.join());
    crate::util::report_join_panic("hew profiler sampler thread", sampler_handle.join());
}

fn await_server_startup(ready_rx: &mpsc::Receiver<Result<(), String>>) -> bool {
    match ready_rx.recv() {
        Ok(Ok(())) => true,
        Ok(Err(error)) => {
            eprintln!("[hew-pprof] server startup failed: {error}");
            false
        }
        Err(error) => {
            eprintln!("[hew-pprof] server exited during startup: {error}");
            false
        }
    }
}

#[cfg(unix)]
fn prepare_unix_listener() -> Result<(std::os::unix::net::UnixListener, PathBuf, PathBuf), String> {
    let disc_dir = discovery::discovery_dir()
        .ok_or_else(|| "could not resolve a safe discovery directory".to_owned())?;
    let sock_path = discovery::socket_path(&disc_dir);

    // Preserve the existing cleanup behavior for a stale socket from a
    // previous crashed run. An entry that `remove_file` cannot remove (for
    // example, a directory) makes the fail-closed bind below return an error.
    let _ = std::fs::remove_file(&sock_path);
    let listener = std::os::unix::net::UnixListener::bind(&sock_path).map_err(|error| {
        format!(
            "failed to bind unix socket {}: {error}",
            sock_path.display()
        )
    })?;

    if let Err(error) = std::fs::set_permissions(&sock_path, std::fs::Permissions::from_mode(0o600))
    {
        drop(listener);
        let _ = std::fs::remove_file(&sock_path);
        return Err(format!("failed to set socket permissions: {error}"));
    }
    Ok((listener, disc_dir, sock_path))
}

/// Register profiler-owned session reset hooks.
///
/// Called unconditionally from `hew_sched_init` (before `maybe_start`) so that
/// the dispatch-type registry is cleared on every JIT reload cycle regardless
/// of whether `HEW_PPROF` is set.  `maybe_start` is gated on the env variable
/// and must not be used for hook registration.
///
/// The registration is Once-guarded; safe to call multiple times per process.
pub(crate) fn register_reset_hooks() {
    use std::sync::Once;
    static ONCE: Once = Once::new();
    ONCE.call_once(|| {
        fn clear_dispatch_registry_hook() {
            actor_registry::clear_dispatch_registry();
        }
        crate::session::register_reset_hook(clear_dispatch_registry_hook);
    });
}

/// Whether a profile consumer (the `HEW_PPROF` server or the
/// `HEW_PROF_OUTPUT` exit report) is configured. Fixed at scheduler startup,
/// before any program code runs.
static CONSUMER_CONFIGURED: AtomicBool = AtomicBool::new(false);

/// Per-actor turn timing and the live Rc census cost work on hot paths; only
/// pay it when something will read the result.
#[must_use]
pub fn consumer_configured() -> bool {
    CONSUMER_CONFIGURED.load(Ordering::Relaxed)
}

fn configure_consumer() {
    let requested = |name| std::env::var_os(name).is_some_and(|value| !value.is_empty());
    CONSUMER_CONFIGURED.store(
        requested("HEW_PPROF") || requested("HEW_PROF_OUTPUT"),
        Ordering::Relaxed,
    );
}

/// Check `HEW_PPROF` and start the profiler if set.
///
/// Called from [`hew_sched_init`](crate::scheduler::hew_sched_init) during
/// scheduler startup. If `HEW_PPROF` is not set, this is a no-op.
///
/// Distributed endpoints read the current node through its lifetime guard.
pub fn maybe_start() {
    configure_consumer();
    let env_val = match std::env::var("HEW_PPROF") {
        Ok(val) if !val.is_empty() => val,
        _ => return,
    };
    let Some(mode) = parse_listen_mode(&env_val) else {
        return;
    };

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

    let ctx = Arc::new(ProfilerContext { ring });

    // HTTP server thread — TCP or Unix depending on mode. Startup waits for
    // the server to finish every fallible runtime/listener conversion before
    // tracing is enabled or discovery is published.
    let (ready_tx, ready_rx) = mpsc::sync_channel(0);
    let (server_shutdown, shutdown_rx) = tokio::sync::oneshot::channel();
    #[cfg(unix)]
    let mut unix_discovery = None;
    let server_handle = match mode {
        ListenMode::Tcp(bind_addr) => {
            // Bind before spawning so an invalid or unavailable address cannot
            // create a phantom enabled tracing epoch with no listening server.
            let listener = match std::net::TcpListener::bind(&bind_addr) {
                Ok(listener) => listener,
                Err(error) => {
                    eprintln!("[hew-pprof] failed to bind {bind_addr}: {error}");
                    stop_sampler_after_startup_failure(sampler_handle);
                    return;
                }
            };
            thread::Builder::new()
                .name("hew-pprof-server".into())
                .spawn(move || {
                    server::run_tcp_with_listener_ready(listener, ctx, ready_tx, shutdown_rx);
                })
        }
        #[cfg(unix)]
        ListenMode::Unix => {
            let (listener, disc_dir, sock_path) = match prepare_unix_listener() {
                Ok(prepared) => prepared,
                Err(error) => {
                    eprintln!("[hew-pprof] {error}");
                    stop_sampler_after_startup_failure(sampler_handle);
                    return;
                }
            };
            unix_discovery = Some((disc_dir, sock_path.clone()));

            thread::Builder::new()
                .name("hew-pprof-server".into())
                .spawn(move || {
                    server::run_unix_with_listener_ready(listener, ctx, ready_tx, shutdown_rx);
                })
        }
    };

    let Ok(server_handle) = server_handle else {
        eprintln!("[hew-pprof] failed to spawn server thread");
        stop_sampler_after_startup_failure(sampler_handle);
        #[cfg(unix)]
        if let Some((disc_dir, _)) = unix_discovery.as_ref() {
            discovery::cleanup(disc_dir);
        }
        return;
    };

    if !await_server_startup(&ready_rx) {
        stop_threads_after_startup_failure(sampler_handle, server_handle, server_shutdown);
        #[cfg(unix)]
        if let Some((disc_dir, _)) = unix_discovery.as_ref() {
            discovery::cleanup(disc_dir);
        }
        return;
    }

    // For unix mode, publish discovery only after the listener is known ready.
    #[cfg(unix)]
    if let Some((disc_dir, sock_path)) = unix_discovery.as_ref() {
        if let Err(error) = discovery::write_discovery_file(disc_dir, sock_path) {
            eprintln!("[hew-pprof] failed to write discovery file: {error}");
            stop_threads_after_startup_failure(sampler_handle, server_handle, server_shutdown);
            discovery::cleanup(disc_dir);
            return;
        }
        PROFILER_DISCOVERY_DIR.access(|slot| *slot = Some(disc_dir.clone()));
        eprintln!("[hew-pprof] listening on unix:{}", sock_path.display());
    }

    // Start the trace epoch only after mode validation, resource setup, bind,
    // server readiness, discovery publication, and both thread spawns succeed.
    crate::tracing::hew_trace_enable(1);

    // Store thread handles for shutdown.
    let threads = ProfilerThreads {
        sampler_handle,
        server_handle,
        server_shutdown,
    };
    PROFILER_STATE.access(|state| *state = Some(threads));
}

/// Parse the `HEW_PPROF` env var into a listen mode.
///
/// Returns `None` on non-unix platforms when `auto`/`1` is requested.
#[cfg_attr(
    unix,
    expect(
        clippy::unnecessary_wraps,
        reason = "returns None on non-unix platforms for auto/1; all-Some on unix is correct"
    )
)]
fn parse_listen_mode(val: &str) -> Option<ListenMode> {
    match val {
        #[cfg(unix)]
        "auto" | "1" | "true" | "yes" => Some(ListenMode::Unix),
        #[cfg(not(unix))]
        "auto" | "1" | "true" | "yes" => {
            eprintln!(
                "[hew-pprof] unix socket mode is not supported on this platform, use host:port"
            );
            None
        }
        addr => {
            let (bind_addr, warns_on_wildcard) = normalise_tcp_bind_address(addr);
            if warns_on_wildcard {
                eprintln!(
                    "[hew-pprof] WARNING: binding profiler to wildcard address {bind_addr}; it is reachable from the network"
                );
            }
            Some(ListenMode::Tcp(bind_addr))
        }
    }
}

/// Return the address to bind and whether the user explicitly requested a
/// wildcard listener.
fn normalise_tcp_bind_address(addr: &str) -> (String, bool) {
    if addr.starts_with(':') {
        return (format!("127.0.0.1{addr}"), false);
    }

    let warns_on_wildcard = addr.starts_with("0.0.0.0:") || addr.starts_with("[::]:");
    (addr.to_owned(), warns_on_wildcard)
}

/// Sampler loop: captures a snapshot every second.
fn sampler_loop(ring: &Arc<Mutex<MetricsRing>>) {
    loop {
        if PROFILER_SHUTDOWN.load(Ordering::Acquire) {
            break;
        }

        let deadline = Instant::now() + Duration::from_secs(1);
        loop {
            if PROFILER_SHUTDOWN.load(Ordering::Acquire) {
                break;
            }
            let remaining = deadline.saturating_duration_since(Instant::now());
            if remaining.is_zero() {
                break;
            }
            thread::park_timeout(remaining);
        }

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
    shutdown();
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
/// Call before freeing the runtime whose metrics the profiler samples.
///
/// Safe to call multiple times or when no profiler was started (no-op).
pub(crate) fn shutdown() {
    // Set shared shutdown state before waking either profiler thread.
    PROFILER_SHUTDOWN.store(true, Ordering::Release);

    // Take the thread handles out of the global; release the lock before
    // joining so no other path is blocked while we wait for the threads.
    let threads = PROFILER_STATE.access(Option::take);
    if let Some(threads) = threads {
        let _ = threads.server_shutdown.send(());
        threads.sampler_handle.thread().unpark();
        crate::util::report_join_panic("hew profiler server thread", threads.server_handle.join());
        crate::util::report_join_panic(
            "hew profiler sampler thread",
            threads.sampler_handle.join(),
        );
    }

    // Clean up unix socket and discovery file if we were in unix mode.
    #[cfg(unix)]
    {
        let disc_dir = PROFILER_DISCOVERY_DIR.access(Option::take);
        if let Some(disc_dir) = disc_dir {
            discovery::cleanup(&disc_dir);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::ffi::CStr;

    static PROFILER_TEST_MUTEX: Mutex<()> = Mutex::new(());

    fn last_error_string() -> String {
        let error_ptr = crate::hew_last_error();
        assert!(!error_ptr.is_null(), "expected a last-error diagnostic");
        // SAFETY: the caller established that the current thread's last-error is set.
        unsafe {
            CStr::from_ptr(error_ptr)
                .to_str()
                .expect("last error should be utf-8")
                .to_owned()
        }
    }

    #[test]
    fn port_only_profiler_address_binds_to_loopback() {
        let (bind_addr, warns_on_wildcard) = normalise_tcp_bind_address(":6060");

        assert_eq!(bind_addr, "127.0.0.1:6060");
        assert!(!warns_on_wildcard);
    }

    #[test]
    fn explicit_wildcard_profiler_address_requests_warning() {
        let (bind_addr, warns_on_wildcard) = normalise_tcp_bind_address("0.0.0.0:6060");

        assert_eq!(bind_addr, "0.0.0.0:6060");
        assert!(warns_on_wildcard);
    }

    #[test]
    fn sampler_shutdown_interrupts_its_wait() {
        let _guard = PROFILER_TEST_MUTEX.lock_or_recover();
        shutdown();
        PROFILER_SHUTDOWN.store(false, Ordering::Release);

        let ring = Arc::new(Mutex::new(MetricsRing::new()));
        let sampler_ring = Arc::clone(&ring);
        let (done_tx, done_rx) = mpsc::channel();
        let sampler = thread::spawn(move || {
            sampler_loop(&sampler_ring);
            let _ = done_tx.send(());
        });

        thread::sleep(Duration::from_millis(20));
        PROFILER_SHUTDOWN.store(true, Ordering::Release);
        sampler.thread().unpark();
        done_rx
            .recv_timeout(Duration::from_millis(500))
            .expect("shutdown should wake the sampler before its next sample");
        sampler.join().expect("sampler should exit cleanly");
    }

    #[test]
    fn invalid_tcp_address_does_not_enable_tracing() {
        let _guard = PROFILER_TEST_MUTEX.lock_or_recover();
        let _trace_guard = crate::tracing::tracing_test_guard();
        shutdown();
        crate::tracing::hew_trace_reset();

        crate::env::ENV_LOCK.access(|()| {
            let previous = std::env::var_os("HEW_PPROF");
            // SAFETY: ENV_LOCK provides exclusive process-environment access.
            unsafe { std::env::set_var("HEW_PPROF", "definitely-not-a-socket-address") };
            maybe_start();
            match previous {
                Some(value) => {
                    // SAFETY: ENV_LOCK provides exclusive process-environment access.
                    unsafe { std::env::set_var("HEW_PPROF", value) };
                }
                None => {
                    // SAFETY: ENV_LOCK provides exclusive process-environment access.
                    unsafe { std::env::remove_var("HEW_PPROF") };
                }
            }
        });

        assert_eq!(crate::tracing::hew_trace_is_enabled(), 0);
        assert!(PROFILER_STATE.access(|state| state.is_none()));
        crate::tracing::hew_trace_reset();
    }

    #[cfg(unix)]
    #[test]
    fn failed_unix_bind_does_not_publish_or_enable_profiler() {
        let _guard = PROFILER_TEST_MUTEX.lock_or_recover();
        let _trace_guard = crate::tracing::tracing_test_guard();
        shutdown();
        crate::tracing::hew_trace_reset();

        let temp = tempfile::tempdir().expect("temporary runtime directory");
        crate::env::ENV_LOCK.access(|()| {
            let previous_pprof = std::env::var_os("HEW_PPROF");
            let previous_xdg = std::env::var_os("XDG_RUNTIME_DIR");
            // SAFETY: ENV_LOCK provides exclusive process-environment access.
            unsafe {
                std::env::set_var("HEW_PPROF", "auto");
                std::env::set_var("XDG_RUNTIME_DIR", temp.path());
            }

            let disc_dir = discovery::discovery_dir().expect("test discovery directory");
            let sock_path = discovery::socket_path(&disc_dir);
            std::fs::create_dir(&sock_path).expect("directory obstructs unix socket bind");

            maybe_start();

            assert_eq!(crate::tracing::hew_trace_is_enabled(), 0);
            assert!(PROFILER_STATE.access(|state| state.is_none()));
            assert!(PROFILER_DISCOVERY_DIR.access(|state| state.is_none()));
            assert!(
                !discovery::discovery_file_path(&disc_dir).exists(),
                "failed startup must not publish discovery"
            );

            match previous_pprof {
                Some(value) => {
                    // SAFETY: ENV_LOCK provides exclusive process-environment access.
                    unsafe { std::env::set_var("HEW_PPROF", value) };
                }
                None => {
                    // SAFETY: ENV_LOCK provides exclusive process-environment access.
                    unsafe { std::env::remove_var("HEW_PPROF") };
                }
            }
            match previous_xdg {
                Some(value) => {
                    // SAFETY: ENV_LOCK provides exclusive process-environment access.
                    unsafe { std::env::set_var("XDG_RUNTIME_DIR", value) };
                }
                None => {
                    // SAFETY: ENV_LOCK provides exclusive process-environment access.
                    unsafe { std::env::remove_var("XDG_RUNTIME_DIR") };
                }
            }
        });

        crate::tracing::hew_trace_reset();
    }

    #[test]
    fn shutdown_reports_server_panic_and_consumes_profiler_state() {
        let _guard = PROFILER_TEST_MUTEX.lock_or_recover();
        shutdown();
        crate::hew_clear_error();
        PROFILER_SHUTDOWN.store(false, Ordering::Release);
        PROFILER_STATE.access(|state| {
            *state = Some(ProfilerThreads {
                server_shutdown: tokio::sync::oneshot::channel().0,
                server_handle: std::thread::spawn(|| {
                    panic!("profiler server intentional panic");
                }),
                sampler_handle: std::thread::spawn(|| {}),
            });
        });

        shutdown();

        let error = last_error_string();
        assert!(
            error.contains("hew profiler server thread panicked during teardown"),
            "unexpected last error: {error}"
        );
        assert!(
            error.contains("profiler server intentional panic"),
            "panic payload must be included in the diagnostic: {error}"
        );
        assert!(PROFILER_STATE.access(|state| state.is_none()));
        assert!(PROFILER_SHUTDOWN.load(Ordering::Acquire));
    }

    #[test]
    fn shutdown_reports_sampler_panic_and_consumes_profiler_state() {
        let _guard = PROFILER_TEST_MUTEX.lock_or_recover();
        shutdown();
        crate::hew_clear_error();
        PROFILER_SHUTDOWN.store(false, Ordering::Release);
        PROFILER_STATE.access(|state| {
            *state = Some(ProfilerThreads {
                server_shutdown: tokio::sync::oneshot::channel().0,
                server_handle: std::thread::spawn(|| {}),
                sampler_handle: std::thread::spawn(|| {
                    panic!("profiler sampler intentional panic");
                }),
            });
        });

        shutdown();

        let error = last_error_string();
        assert!(
            error.contains("hew profiler sampler thread panicked during teardown"),
            "unexpected last error: {error}"
        );
        assert!(
            error.contains("profiler sampler intentional panic"),
            "panic payload must be included in the diagnostic: {error}"
        );
        assert!(PROFILER_STATE.access(|state| state.is_none()));
        assert!(PROFILER_SHUTDOWN.load(Ordering::Acquire));
    }
}
