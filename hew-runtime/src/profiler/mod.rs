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

use std::sync::{Arc, Mutex};
use std::thread;
use std::time::Duration;

use metrics::MetricsRing;
pub use server::ProfilerContext;

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
/// The pointers (if non-null) must remain valid for the lifetime of the
/// program. Null pointers are safe and result in empty JSON responses
/// for the corresponding endpoints.
pub fn maybe_start_with_context(
    cluster: *mut crate::cluster::HewCluster,
    connmgr: *mut crate::connection::HewConnMgr,
    routing: *mut crate::routing::HewRoutingTable,
) {
    let bind_addr = match std::env::var("HEW_PPROF") {
        Ok(val) if !val.is_empty() => val,
        _ => return,
    };

    // Normalize `:port` shorthand to `0.0.0.0:port`.
    let bind_addr = if bind_addr.starts_with(':') {
        format!("0.0.0.0{bind_addr}")
    } else {
        bind_addr
    };

    let ring = Arc::new(Mutex::new(MetricsRing::new()));

    // Sampler thread: captures a MetricsSnapshot every second.
    let sampler_ring = Arc::clone(&ring);
    if thread::Builder::new()
        .name("hew-pprof-sampler".into())
        .spawn(move || sampler_loop(&sampler_ring))
        .is_err()
    {
        eprintln!("[hew-pprof] failed to spawn sampler thread");
        return;
    }

    let ctx = ProfilerContext {
        ring,
        cluster,
        connmgr,
        routing,
    };

    // HTTP server thread.
    if thread::Builder::new()
        .name("hew-pprof-server".into())
        .spawn(move || server::run(&bind_addr, &ctx))
        .is_err()
    {
        eprintln!("[hew-pprof] failed to spawn server thread");
    }
}

/// Sampler loop: captures a snapshot every second.
fn sampler_loop(ring: &Arc<Mutex<MetricsRing>>) {
    loop {
        thread::sleep(Duration::from_secs(1));
        let mut ring_guard = match ring.lock() {
            Ok(g) => g,
            Err(e) => e.into_inner(),
        };
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
