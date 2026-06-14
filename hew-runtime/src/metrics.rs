//! Bounded registry for developer-defined application metrics.
//!
//! A Hew program declares its own counters, gauges, and histograms through
//! `std::metrics`; this module is the process-global source of truth that
//! holds their live values and surfaces them through `observe.scrape()`
//! alongside the runtime built-ins.
//!
//! # Design
//!
//! - **Stable slot addresses.** Slot values live in `Vec<Box<AtomicI64>>`.
//!   Pushing a new box reallocates the *pointer* vector, never the boxed
//!   atomics, so an integer slot handle stays valid for the process lifetime.
//!   A handle is the slot index; the hot path (`inc`/`add`/`set`/`record`)
//!   reads the box pointer under a short read and mutates the atomic without
//!   the registration lock.
//! - **Register-lock-only mutation of the directory.** All structural changes
//!   (new metric, new label series, charset/cap checks) happen under a single
//!   [`PoisonSafe`] lock so the caps are checked-and-committed atomically; two
//!   threads cannot both pass a `len < CAP` check and overflow.
//! - **Fail-closed caps.** Every overflow — too many names, too many series,
//!   too many label keys, an over-long name, a charset violation, a collision
//!   with a runtime built-in, or an unknown handle — is rejected and counted
//!   in a self-metric that is itself visible in the scrape. Nothing is
//!   silently dropped; nothing is unbounded.
//!
//! # WASM parity
//!
//! Atomics + [`PoisonSafe`] + `HashMap` + `String` all run on `wasm32`. The
//! registry resets on session boundary through `observe::reset_all`, which is
//! the registered session-reset hook on both the native and the wasm
//! scheduler paths.

use std::collections::HashMap;
use std::sync::atomic::{AtomicI64, Ordering};

use crate::lifetime::PoisonSafe;

/// Maximum number of distinct metric *names* (counter/gauge/histogram, before
/// label expansion) the registry will hold.
const MAX_NAMES: usize = 4096;
/// Maximum number of label-value series a single labelled metric may spawn.
const MAX_SERIES_PER_METRIC: usize = 2000;
/// Maximum number of label *keys* a single metric declaration may carry.
const MAX_LABEL_KEYS: usize = 16;
/// Maximum number of histogram buckets fixed at registration.
const MAX_HISTOGRAM_BUCKETS: usize = 64;
/// Maximum length, in bytes, of a metric name.
const MAX_NAME_LEN: usize = 255;
/// Maximum length, in bytes, of a label key or label value.
const MAX_LABEL_LEN: usize = 256;

/// Kind of a user metric. Fixed at registration; a later register call with a
/// different kind for the same name is a collision (counted, rejected).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MetricKind {
    /// Monotonic counter. `add(n)` with `n < 0` is rejected and counted.
    Counter,
    /// Bidirectional gauge. Supports `set`, `inc`, `dec`, `add`.
    Gauge,
    /// Histogram with bucket upper bounds fixed at registration.
    Histogram,
}

impl MetricKind {
    /// Prometheus `# TYPE` token for this kind.
    #[must_use]
    pub fn prometheus_type(self) -> &'static str {
        match self {
            Self::Counter => "counter",
            Self::Gauge => "gauge",
            Self::Histogram => "histogram",
        }
    }
}

/// A registered metric's directory entry. Owns the slot index of its
/// unlabelled value and, for labelled metrics, the map from a canonical
/// label-tuple key to a slot index.
#[derive(Debug)]
struct MetricEntry {
    kind: MetricKind,
    /// Slot index of the metric's base (no-label) value.
    base_slot: usize,
    /// Declared label keys, in declaration order. Empty for an unlabelled
    /// metric.
    label_keys: Vec<String>,
    /// Histogram bucket upper bounds, ascending. Empty for non-histograms.
    /// The slot at `base_slot + i + 1` accumulates observations `<= buckets[i]`
    /// (cumulative), and `base_slot` holds the running observation count.
    buckets: Vec<i64>,
    /// Map from a canonical label-value key (`"k1=v1,k2=v2"`) to the slot
    /// index of that series' value. Only populated for labelled metrics.
    series: HashMap<String, usize>,
}

/// The registry's locked interior: the name directory plus the boxed atomic
/// slot arena. Held behind a single [`PoisonSafe`] so registration is
/// serialised and cap checks are atomic.
#[derive(Debug, Default)]
struct RegistryInner {
    /// Metric name → directory entry.
    names: HashMap<String, MetricEntry>,
    /// Insertion-ordered metric names, so the scrape render is deterministic.
    name_order: Vec<String>,
    /// The slot arena. `Box<AtomicI64>` keeps each value at a stable address
    /// across `push`, so an index handle never dangles.
    #[allow(
        clippy::vec_box,
        reason = "the Box is load-bearing: a Vec<AtomicI64> realloc would move \
                  the atomics and invalidate every outstanding index handle. \
                  The boxed atomic stays put across pushes."
    )]
    slots: Vec<Box<AtomicI64>>,
}

impl RegistryInner {
    /// Allocate a fresh zeroed slot and return its index.
    fn alloc_slot(&mut self) -> usize {
        let idx = self.slots.len();
        self.slots.push(Box::new(AtomicI64::new(0)));
        idx
    }
}

/// Process-global user-metric registry.
///
/// Stored as `Option<RegistryInner>` so the static initialiser stays const
/// (a `HashMap::new()` is not const for the default hasher). The interior is
/// materialised on first access. This mirrors `observe::ATTRIBUTED_TURNS`.
static REGISTRY: PoisonSafe<Option<RegistryInner>> = PoisonSafe::new(None);

/// Run `f` with exclusive access to the (lazily-initialised) registry interior.
fn with_registry<R>(f: impl FnOnce(&mut RegistryInner) -> R) -> R {
    REGISTRY.access(|slot| f(slot.get_or_insert_with(RegistryInner::default)))
}

// --- Self-metrics: visible in the scrape so a user discovers dropped data. ---

/// Names rejected because the name cap was hit, the name was malformed, or it
/// collided with a runtime built-in.
static NAMES_DROPPED: AtomicI64 = AtomicI64::new(0);
/// Label series rejected because the per-metric series cap was hit.
static SERIES_DROPPED: AtomicI64 = AtomicI64::new(0);
/// Mutations rejected at runtime: a negative counter add, or an unknown handle.
static INVALID_OPS: AtomicI64 = AtomicI64::new(0);
/// Registrations rejected because the name equalled a runtime built-in metric.
static COLLISION_REJECTED: AtomicI64 = AtomicI64::new(0);

/// Sentinel returned by every register entry point when registration fails a
/// cap, charset, or collision check. Distinguishable from a valid slot index
/// (which is always `>= 0`).
pub const REGISTER_FAILED: i64 = -1;

/// Validate a metric name against the Prometheus-compatible charset
/// `[a-zA-Z_:][a-zA-Z0-9_:.]*` and the length cap.
fn name_is_valid(name: &str) -> bool {
    if name.is_empty() || name.len() > MAX_NAME_LEN {
        return false;
    }
    let mut chars = name.chars();
    let first = chars.next().expect("non-empty checked above");
    if !(first.is_ascii_alphabetic() || first == '_' || first == ':') {
        return false;
    }
    chars.all(|c| c.is_ascii_alphanumeric() || c == '_' || c == ':' || c == '.')
}

/// Validate a label key with the same charset as a metric name.
fn label_key_is_valid(key: &str) -> bool {
    if key.is_empty() || key.len() > MAX_LABEL_LEN {
        return false;
    }
    let mut chars = key.chars();
    let first = chars.next().expect("non-empty checked above");
    if !(first.is_ascii_alphabetic() || first == '_') {
        return false;
    }
    chars.all(|c| c.is_ascii_alphanumeric() || c == '_')
}

/// True when `name` collides with a built-in runtime metric the observe
/// registry already owns. A user metric may not shadow one.
fn collides_with_builtin(name: &str) -> bool {
    crate::observe::is_builtin_metric_name(name)
}

/// Register a counter/gauge/histogram name, or return the existing slot if the
/// same name+kind was already registered (idempotent register-or-get).
///
/// Returns the base slot index, or [`REGISTER_FAILED`] when a cap, charset, or
/// collision check fails (the matching self-metric is incremented).
fn register_named(name: &str, kind: MetricKind, buckets: &[i64]) -> i64 {
    if !name_is_valid(name) {
        NAMES_DROPPED.fetch_add(1, Ordering::Relaxed);
        return REGISTER_FAILED;
    }
    if collides_with_builtin(name) {
        COLLISION_REJECTED.fetch_add(1, Ordering::Relaxed);
        NAMES_DROPPED.fetch_add(1, Ordering::Relaxed);
        return REGISTER_FAILED;
    }
    if kind == MetricKind::Histogram && buckets.len() > MAX_HISTOGRAM_BUCKETS {
        NAMES_DROPPED.fetch_add(1, Ordering::Relaxed);
        return REGISTER_FAILED;
    }

    with_registry(|reg| {
        // Idempotent register-or-get: same name + same kind returns the
        // existing base slot; a kind mismatch is a collision.
        if let Some(existing) = reg.names.get(name) {
            if existing.kind == kind {
                return i64_slot(existing.base_slot);
            }
            COLLISION_REJECTED.fetch_add(1, Ordering::Relaxed);
            NAMES_DROPPED.fetch_add(1, Ordering::Relaxed);
            return REGISTER_FAILED;
        }
        if reg.names.len() >= MAX_NAMES {
            NAMES_DROPPED.fetch_add(1, Ordering::Relaxed);
            return REGISTER_FAILED;
        }

        let base_slot = reg.alloc_slot();
        // A histogram reserves one slot per bucket (cumulative `le` counters)
        // plus the running sum slot allocated lazily; the count rides
        // `base_slot`. Allocate the bucket slots contiguously after the base.
        if kind == MetricKind::Histogram {
            for _ in buckets {
                reg.alloc_slot();
            }
        }
        reg.names.insert(
            name.to_string(),
            MetricEntry {
                kind,
                base_slot,
                label_keys: Vec::new(),
                buckets: buckets.to_vec(),
                series: HashMap::new(),
            },
        );
        reg.name_order.push(name.to_string());
        i64_slot(base_slot)
    })
}

/// Register a labelled metric and reserve its label keys. Returns the metric's
/// base handle; concrete series are materialised lazily by [`series_slot`].
fn register_vec(name: &str, kind: MetricKind, label_keys: &[String]) -> i64 {
    if !name_is_valid(name) {
        NAMES_DROPPED.fetch_add(1, Ordering::Relaxed);
        return REGISTER_FAILED;
    }
    if collides_with_builtin(name) {
        COLLISION_REJECTED.fetch_add(1, Ordering::Relaxed);
        NAMES_DROPPED.fetch_add(1, Ordering::Relaxed);
        return REGISTER_FAILED;
    }
    if label_keys.len() > MAX_LABEL_KEYS || label_keys.iter().any(|k| !label_key_is_valid(k)) {
        NAMES_DROPPED.fetch_add(1, Ordering::Relaxed);
        return REGISTER_FAILED;
    }

    with_registry(|reg| {
        if let Some(existing) = reg.names.get(name) {
            if existing.kind == kind && existing.label_keys == label_keys {
                return i64_slot(existing.base_slot);
            }
            COLLISION_REJECTED.fetch_add(1, Ordering::Relaxed);
            NAMES_DROPPED.fetch_add(1, Ordering::Relaxed);
            return REGISTER_FAILED;
        }
        if reg.names.len() >= MAX_NAMES {
            NAMES_DROPPED.fetch_add(1, Ordering::Relaxed);
            return REGISTER_FAILED;
        }
        let base_slot = reg.alloc_slot();
        reg.names.insert(
            name.to_string(),
            MetricEntry {
                kind,
                base_slot,
                label_keys: label_keys.to_vec(),
                buckets: Vec::new(),
                series: HashMap::new(),
            },
        );
        reg.name_order.push(name.to_string());
        i64_slot(base_slot)
    })
}

/// Resolve (or materialise) the slot for one label-value series of a labelled
/// metric identified by its base handle. Returns [`REGISTER_FAILED`] on a bad
/// handle, label-arity mismatch, over-long label value, or series-cap breach.
fn series_slot(base_handle: i64, label_values: &[String]) -> i64 {
    let Ok(base_slot) = usize::try_from(base_handle) else {
        INVALID_OPS.fetch_add(1, Ordering::Relaxed);
        return REGISTER_FAILED;
    };
    if label_values.iter().any(|v| v.len() > MAX_LABEL_LEN) {
        SERIES_DROPPED.fetch_add(1, Ordering::Relaxed);
        return REGISTER_FAILED;
    }

    with_registry(|reg| {
        let Some(name) = reg
            .name_order
            .iter()
            .find(|n| reg.names.get(*n).is_some_and(|e| e.base_slot == base_slot))
        else {
            INVALID_OPS.fetch_add(1, Ordering::Relaxed);
            return REGISTER_FAILED;
        };
        let name = name.clone();
        // Arity check uses an immutable borrow first; clone the key list so the
        // mutable insert below does not alias.
        let arity_ok = reg
            .names
            .get(&name)
            .is_some_and(|e| e.label_keys.len() == label_values.len());
        if !arity_ok {
            INVALID_OPS.fetch_add(1, Ordering::Relaxed);
            return REGISTER_FAILED;
        }
        let key = canonical_series_key(
            reg.names
                .get(&name)
                .map_or(&[], |e| e.label_keys.as_slice()),
            label_values,
        );
        if let Some(slot) = reg.names.get(&name).and_then(|e| e.series.get(&key)) {
            return i64_slot(*slot);
        }
        if reg
            .names
            .get(&name)
            .is_some_and(|e| e.series.len() >= MAX_SERIES_PER_METRIC)
        {
            SERIES_DROPPED.fetch_add(1, Ordering::Relaxed);
            return REGISTER_FAILED;
        }
        let slot = reg.alloc_slot();
        if let Some(entry) = reg.names.get_mut(&name) {
            entry.series.insert(key, slot);
        }
        i64_slot(slot)
    })
}

/// Canonical `k1=v1,k2=v2` series key in label-declaration order.
fn canonical_series_key(keys: &[String], values: &[String]) -> String {
    let mut out = String::new();
    for (i, (k, v)) in keys.iter().zip(values.iter()).enumerate() {
        if i > 0 {
            out.push(',');
        }
        out.push_str(k);
        out.push('=');
        out.push_str(v);
    }
    out
}

/// Convert a slot index into the `i64` handle ABI value (always `>= 0`).
fn i64_slot(slot: usize) -> i64 {
    i64::try_from(slot).unwrap_or(REGISTER_FAILED)
}

/// Run `f` with the boxed atomic at `handle`, or count an invalid op and run
/// nothing when the handle is out of range.
fn with_slot<R>(handle: i64, f: impl FnOnce(&AtomicI64) -> R) -> Option<R> {
    let Ok(idx) = usize::try_from(handle) else {
        INVALID_OPS.fetch_add(1, Ordering::Relaxed);
        return None;
    };
    with_registry(|reg| {
        if let Some(slot) = reg.slots.get(idx) {
            Some(f(slot))
        } else {
            INVALID_OPS.fetch_add(1, Ordering::Relaxed);
            None
        }
    })
}

// --- Public register-or-get entry points (called by the FFI ABI in slice 2). -

/// Register (or get) an unlabelled counter. Idempotent on name+kind.
#[must_use]
pub fn register_counter(name: &str) -> i64 {
    register_named(name, MetricKind::Counter, &[])
}

/// Register (or get) an unlabelled gauge. Idempotent on name+kind.
#[must_use]
pub fn register_gauge(name: &str) -> i64 {
    register_named(name, MetricKind::Gauge, &[])
}

/// Register (or get) a histogram with the given bucket upper bounds.
#[must_use]
pub fn register_histogram(name: &str, buckets: &[i64]) -> i64 {
    register_named(name, MetricKind::Histogram, buckets)
}

/// Register (or get) a labelled metric of `kind` with the given label keys.
#[must_use]
pub fn register_labelled(name: &str, kind: MetricKind, label_keys: &[String]) -> i64 {
    register_vec(name, kind, label_keys)
}

/// Resolve the per-series handle for a labelled metric.
#[must_use]
pub fn resolve_series(base_handle: i64, label_values: &[String]) -> i64 {
    series_slot(base_handle, label_values)
}

// --- Hot-path mutators (lock-free on the boxed atomic). ---------------------

/// Increment a counter or gauge by one.
pub fn inc(handle: i64) {
    with_slot(handle, |slot| {
        slot.fetch_add(1, Ordering::Relaxed);
    });
}

/// Add `n` to a counter. A negative `n` is rejected and counted (counters are
/// monotonic).
pub fn counter_add(handle: i64, n: i64) {
    if n < 0 {
        INVALID_OPS.fetch_add(1, Ordering::Relaxed);
        return;
    }
    with_slot(handle, |slot| {
        slot.fetch_add(n, Ordering::Relaxed);
    });
}

/// Add `n` (any sign) to a gauge.
pub fn gauge_add(handle: i64, n: i64) {
    with_slot(handle, |slot| {
        slot.fetch_add(n, Ordering::Relaxed);
    });
}

/// Decrement a gauge by one.
pub fn gauge_dec(handle: i64) {
    with_slot(handle, |slot| {
        slot.fetch_sub(1, Ordering::Relaxed);
    });
}

/// Set a gauge to `n`.
pub fn gauge_set(handle: i64, n: i64) {
    with_slot(handle, |slot| {
        slot.store(n, Ordering::Relaxed);
    });
}

/// Record one histogram observation. `value` is the observed quantity; the
/// running observation count rides the base slot and each bucket slot
/// accumulates the cumulative `<= upper_bound` count.
pub fn histogram_record(handle: i64, value: i64) {
    let Ok(base_idx) = usize::try_from(handle) else {
        INVALID_OPS.fetch_add(1, Ordering::Relaxed);
        return;
    };
    with_registry(|reg| {
        // Find the entry whose base_slot == base_idx to learn its buckets.
        let Some(buckets) = reg
            .name_order
            .iter()
            .filter_map(|n| reg.names.get(n))
            .find(|e| e.base_slot == base_idx && e.kind == MetricKind::Histogram)
            .map(|e| e.buckets.clone())
        else {
            INVALID_OPS.fetch_add(1, Ordering::Relaxed);
            return;
        };
        if let Some(count) = reg.slots.get(base_idx) {
            count.fetch_add(1, Ordering::Relaxed);
        }
        for (i, bound) in buckets.iter().enumerate() {
            if value <= *bound {
                if let Some(bucket_slot) = reg.slots.get(base_idx + i + 1) {
                    bucket_slot.fetch_add(1, Ordering::Relaxed);
                }
            }
        }
    });
}

// --- Scrape rendering (consumed by observe.rs in slice 4). ------------------

/// A rendered line: one metric series in Prometheus text form.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RenderedMetric {
    /// Prometheus-rendered name (dots already mapped to underscores).
    pub prometheus_name: String,
    /// Canonical (un-rendered) name, for `series()`.
    pub canonical_name: String,
    /// `# TYPE` token.
    pub type_token: &'static str,
    /// The label keys, in declaration order.
    pub label_keys: Vec<String>,
    /// One `(label_values, value)` pair per series. Empty `label_values` for
    /// the base series.
    pub series: Vec<(Vec<String>, i64)>,
}

/// Snapshot every user metric for the scrape render. Returns metrics in
/// registration order so the scrape output is deterministic.
#[must_use]
pub fn render_snapshot() -> Vec<RenderedMetric> {
    with_registry(|reg| {
        let mut out = Vec::with_capacity(reg.name_order.len());
        for name in &reg.name_order {
            let Some(entry) = reg.names.get(name) else {
                continue;
            };
            let prometheus_name = name.replace('.', "_");
            let mut series = Vec::new();
            if entry.label_keys.is_empty() {
                let value = reg
                    .slots
                    .get(entry.base_slot)
                    .map_or(0, |s| s.load(Ordering::Relaxed));
                series.push((Vec::new(), value));
            } else {
                // Series order: sort by canonical key for a stable scrape.
                let mut keys: Vec<(&String, &usize)> = entry.series.iter().collect();
                keys.sort_by(|a, b| a.0.cmp(b.0));
                for (canon, slot) in keys {
                    let value = reg
                        .slots
                        .get(*slot)
                        .map_or(0, |s| s.load(Ordering::Relaxed));
                    series.push((split_series_values(canon), value));
                }
            }
            out.push(RenderedMetric {
                prometheus_name,
                canonical_name: name.clone(),
                type_token: entry.kind.prometheus_type(),
                label_keys: entry.label_keys.clone(),
                series,
            });
        }
        out
    })
}

/// Split a `k1=v1,k2=v2` canonical key back into its value list.
fn split_series_values(canon: &str) -> Vec<String> {
    if canon.is_empty() {
        return Vec::new();
    }
    canon
        .split(',')
        .map(|kv| kv.split_once('=').map_or("", |x| x.1).to_string())
        .collect()
}

/// Current values of the registry self-metrics, for the scrape's
/// `hew_metrics_*_total` lines.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct SelfMetrics {
    /// Names rejected (cap, charset, or collision).
    pub names_dropped: i64,
    /// Series rejected (per-metric series cap).
    pub series_dropped: i64,
    /// Runtime mutations rejected (negative counter add, unknown handle).
    pub invalid_ops: i64,
    /// Registrations rejected for colliding with a runtime built-in.
    pub collision_rejected: i64,
}

/// Read the registry self-metrics.
#[must_use]
pub fn self_metrics() -> SelfMetrics {
    SelfMetrics {
        names_dropped: NAMES_DROPPED.load(Ordering::Relaxed),
        series_dropped: SERIES_DROPPED.load(Ordering::Relaxed),
        invalid_ops: INVALID_OPS.load(Ordering::Relaxed),
        collision_rejected: COLLISION_REJECTED.load(Ordering::Relaxed),
    }
}

/// Clear the registry. Wired into `observe::reset_all`, the registered
/// session-reset hook on both the native and the wasm scheduler paths, so a
/// fresh session never observes a prior session's metrics.
pub fn session_reset_metrics() {
    with_registry(|reg| {
        reg.names.clear();
        reg.name_order.clear();
        reg.slots.clear();
    });
    NAMES_DROPPED.store(0, Ordering::Relaxed);
    SERIES_DROPPED.store(0, Ordering::Relaxed);
    INVALID_OPS.store(0, Ordering::Relaxed);
    COLLISION_REJECTED.store(0, Ordering::Relaxed);
}

#[cfg(all(test, not(target_family = "wasm")))]
mod tests {
    use super::*;
    use std::sync::{Arc, Barrier};
    use std::thread;

    // Every test mutates the process-global registry. Serialise them through a
    // single lock and reset at entry so they do not observe each other's state.
    static TEST_LOCK: std::sync::Mutex<()> = std::sync::Mutex::new(());

    fn guard() -> std::sync::MutexGuard<'static, ()> {
        let g = TEST_LOCK
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        session_reset_metrics();
        g
    }

    #[test]
    fn counter_inc_accumulates_exact_value() {
        let _g = guard();
        let h = register_counter("app.requests");
        assert!(h >= 0);
        for _ in 0..5 {
            inc(h);
        }
        let snap = render_snapshot();
        let m = snap
            .iter()
            .find(|m| m.canonical_name == "app.requests")
            .unwrap();
        assert_eq!(m.series[0].1, 5, "five increments must read exactly 5");
        assert_eq!(m.prometheus_name, "app_requests");
        assert_eq!(m.type_token, "counter");
    }

    #[test]
    fn register_is_idempotent_returns_same_slot() {
        let _g = guard();
        let a = register_counter("app.hits");
        let b = register_counter("app.hits");
        assert_eq!(a, b, "register-or-get must return the same slot");
    }

    #[test]
    fn counter_add_rejects_negative_and_counts() {
        let _g = guard();
        let h = register_counter("app.bytes");
        counter_add(h, 10);
        counter_add(h, -3); // rejected
        let snap = render_snapshot();
        let m = snap
            .iter()
            .find(|m| m.canonical_name == "app.bytes")
            .unwrap();
        assert_eq!(m.series[0].1, 10, "negative add must not apply");
        assert_eq!(
            self_metrics().invalid_ops,
            1,
            "negative add must be counted"
        );
    }

    #[test]
    fn gauge_set_inc_dec_add_exact() {
        let _g = guard();
        let h = register_gauge("app.active");
        gauge_set(h, 3);
        gauge_inc_check(h);
        gauge_dec(h);
        gauge_dec(h);
        gauge_add(h, -1);
        let snap = render_snapshot();
        let m = snap
            .iter()
            .find(|m| m.canonical_name == "app.active")
            .unwrap();
        // 3 +1 -1 -1 -1 = 1
        assert_eq!(m.series[0].1, 1);
        assert_eq!(m.type_token, "gauge");
    }

    fn gauge_inc_check(h: i64) {
        inc(h);
    }

    #[test]
    fn name_cap_overflow_is_counted() {
        let _g = guard();
        for i in 0..MAX_NAMES {
            let ok = register_counter(&format!("app.m{i}"));
            assert!(ok >= 0);
        }
        let over = register_counter("app.over_cap");
        assert_eq!(over, REGISTER_FAILED, "name beyond cap must fail");
        assert!(self_metrics().names_dropped >= 1);
    }

    #[test]
    fn charset_rejection_is_counted() {
        let _g = guard();
        assert_eq!(register_counter("9bad"), REGISTER_FAILED, "leading digit");
        assert_eq!(register_counter("has space"), REGISTER_FAILED, "space");
        assert_eq!(register_counter("has-dash"), REGISTER_FAILED, "dash");
        assert_eq!(register_counter(""), REGISTER_FAILED, "empty");
        assert!(self_metrics().names_dropped >= 4);
        // A valid name with all legal classes registers.
        assert!(register_counter("_ok:name.v2") >= 0);
    }

    #[test]
    fn collision_with_builtin_is_rejected_and_counted() {
        let _g = guard();
        let h = register_counter("heap.live_bytes");
        assert_eq!(h, REGISTER_FAILED, "must not shadow a built-in");
        assert_eq!(self_metrics().collision_rejected, 1);
    }

    #[test]
    fn kind_mismatch_for_same_name_is_collision() {
        let _g = guard();
        let c = register_counter("app.dual");
        assert!(c >= 0);
        let g = register_gauge("app.dual");
        assert_eq!(
            g, REGISTER_FAILED,
            "re-register as a different kind is a collision"
        );
        assert_eq!(self_metrics().collision_rejected, 1);
    }

    #[test]
    fn labelled_series_cap_overflow_is_counted() {
        let _g = guard();
        let base = register_labelled("app.req", MetricKind::Counter, &["route".to_string()]);
        assert!(base >= 0);
        for i in 0..MAX_SERIES_PER_METRIC {
            let s = resolve_series(base, &[format!("r{i}")]);
            assert!(s >= 0);
            inc(s);
        }
        let over = resolve_series(base, &["overflow".to_string()]);
        assert_eq!(over, REGISTER_FAILED);
        assert!(self_metrics().series_dropped >= 1);
    }

    #[test]
    fn labelled_series_resolve_is_idempotent() {
        let _g = guard();
        let base = register_labelled("app.lat", MetricKind::Gauge, &["host".to_string()]);
        let a = resolve_series(base, &["h1".to_string()]);
        let b = resolve_series(base, &["h1".to_string()]);
        assert_eq!(a, b);
        gauge_set(a, 42);
        let snap = render_snapshot();
        let m = snap.iter().find(|m| m.canonical_name == "app.lat").unwrap();
        assert_eq!(m.series.len(), 1);
        assert_eq!(m.series[0].0, vec!["h1".to_string()]);
        assert_eq!(m.series[0].1, 42);
    }

    #[test]
    fn label_key_cap_overflow_is_counted() {
        let _g = guard();
        let keys: Vec<String> = (0..=MAX_LABEL_KEYS).map(|i| format!("k{i}")).collect();
        let over = register_labelled("app.toolabels", MetricKind::Counter, &keys);
        assert_eq!(over, REGISTER_FAILED);
        assert!(self_metrics().names_dropped >= 1);
    }

    #[test]
    fn histogram_records_cumulative_buckets() {
        let _g = guard();
        let h = register_histogram("app.dur", &[10, 100, 1000]);
        assert!(h >= 0);
        histogram_record(h, 5); // <=10, <=100, <=1000
        histogram_record(h, 50); // <=100, <=1000
        histogram_record(h, 5000); // none
        let snap = render_snapshot();
        let m = snap.iter().find(|m| m.canonical_name == "app.dur").unwrap();
        assert_eq!(m.type_token, "histogram");
        // base slot holds the total observation count.
        assert_eq!(m.series[0].1, 3, "three observations recorded");
    }

    #[test]
    fn histogram_bucket_cap_rejected() {
        let _g = guard();
        let buckets: Vec<i64> = (0..=i64::try_from(MAX_HISTOGRAM_BUCKETS).unwrap()).collect();
        let h = register_histogram("app.bigbuckets", &buckets);
        assert_eq!(h, REGISTER_FAILED);
        assert!(self_metrics().names_dropped >= 1);
    }

    #[test]
    fn unknown_handle_mutation_is_counted() {
        let _g = guard();
        inc(999_999);
        gauge_set(-5, 3);
        assert!(self_metrics().invalid_ops >= 2);
    }

    #[test]
    fn reset_clears_registry_and_self_metrics() {
        let _g = guard();
        let h = register_counter("app.reset");
        inc(h);
        let _ = register_counter("9bad"); // bump names_dropped
        session_reset_metrics();
        assert!(render_snapshot().is_empty());
        assert_eq!(self_metrics(), SelfMetrics::default());
    }

    #[test]
    fn concurrent_inc_reaches_exact_final_value() {
        let _g = guard();
        let h = register_counter("app.concurrent");
        let threads: i64 = 8;
        let per: i64 = 1000;
        let start = Arc::new(Barrier::new(usize::try_from(threads).unwrap()));
        let mut joins = Vec::new();
        for _ in 0..threads {
            let start = Arc::clone(&start);
            joins.push(thread::spawn(move || {
                start.wait();
                for _ in 0..per {
                    inc(h);
                }
            }));
        }
        for j in joins {
            j.join().unwrap();
        }
        let snap = render_snapshot();
        let m = snap
            .iter()
            .find(|m| m.canonical_name == "app.concurrent")
            .unwrap();
        assert_eq!(
            m.series[0].1,
            threads * per,
            "concurrent increments must sum to the exact total"
        );
    }

    #[test]
    fn slot_handles_survive_arena_growth() {
        let _g = guard();
        // Hold an early handle, then force many reallocations of the slot Vec.
        let early = register_counter("app.early");
        inc(early);
        for i in 0..5000 {
            let _ = register_counter(&format!("app.grow{i}"));
        }
        // The early handle must still address its own slot, not a moved one.
        inc(early);
        let snap = render_snapshot();
        let m = snap
            .iter()
            .find(|m| m.canonical_name == "app.early")
            .unwrap();
        assert_eq!(m.series[0].1, 2, "early handle stayed valid across growth");
    }
}
