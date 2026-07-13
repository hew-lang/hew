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
//!   A handle is the slot index.
//! - **Single registry lock today.** All access — both structural changes (new
//!   metric, new label series, charset/cap checks) and the hot-path mutators
//!   (`inc`/`add`/`set`/`record`) — goes through one [`PoisonSafe`] lock. A
//!   mutator takes the lock to resolve the slot index to its boxed atomic and
//!   mutates the atomic under that lock. The boxed-atomic design already keeps a
//!   handle stable across registration, so a future revision can resolve a slot
//!   under a short read lock (or a lock-free index→pointer map) and mutate the
//!   atomic without holding the registration lock; that lock-free hot path is a
//!   Phase B optimisation, not the current behaviour.
//! - **Atomic cap checks.** Structural changes happen under the same lock so the
//!   caps are checked-and-committed atomically; two threads cannot both pass a
//!   `len < CAP` check and overflow.
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
    /// A histogram's slot layout is: `base_slot` holds the running observation
    /// count, `base_slot + 1` holds the running observation sum, and the slot at
    /// `base_slot + 2 + i` accumulates observations `<= buckets[i]` (cumulative).
    /// Tracking the sum lets the scrape emit a valid `name_count` / `name_sum`
    /// histogram exposition; a bare count is not valid Prometheus.
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

/// Per-runtime user-metric state: the bounded registry plus its fail-closed
/// self-metrics, visible in the scrape so a user discovers dropped data.
///
/// Was the process-global `REGISTRY` plus the four free self-metric statics
/// (`NAMES_DROPPED`, `SERIES_DROPPED`, `INVALID_OPS`, `COLLISION_REJECTED`).
/// Each runtime now owns its own copy as `RuntimeInner.metrics`, resolved
/// through [`metrics_state`]: a second runtime counts its own registrations and
/// rejects into its own scrape without aliasing another's.
///
/// The registry interior is stored as `Option<RegistryInner>` so the
/// constructor stays `const` (a `HashMap::new()` is not const for the default
/// hasher); the interior is materialised on first access. The self-metrics stay
/// lock-free `AtomicI64`s beside the registry lock so a rejection on an
/// early-return validation path counts without taking the registry mutex.
#[derive(Debug)]
pub(crate) struct MetricsState {
    /// The bounded name directory and boxed-atomic slot arena, behind one lock.
    registry: PoisonSafe<Option<RegistryInner>>,
    /// Names rejected because the name cap was hit, the name was malformed, or
    /// it collided with a runtime built-in.
    names_dropped: AtomicI64,
    /// Label series rejected because the per-metric series cap was hit.
    series_dropped: AtomicI64,
    /// Mutations rejected at runtime: a negative counter add, or an unknown
    /// handle.
    invalid_ops: AtomicI64,
    /// Registrations rejected because the name equalled a runtime built-in.
    collision_rejected: AtomicI64,
}

impl MetricsState {
    /// The empty metric state for a fresh runtime. `const` so the WASM module
    /// global and the `RuntimeInner` field initialiser share one constructor.
    pub(crate) const fn new() -> Self {
        Self {
            registry: PoisonSafe::new(None),
            names_dropped: AtomicI64::new(0),
            series_dropped: AtomicI64::new(0),
            invalid_ops: AtomicI64::new(0),
            collision_rejected: AtomicI64::new(0),
        }
    }
}

/// The single metric state for the WASM cooperative runtime, which has no
/// `RuntimeInner`. Mirrors the other ungated subsystems (`registry`) that keep
/// a module static on wasm while the native build resolves a per-runtime field.
#[cfg(target_arch = "wasm32")]
static WASM_METRICS: MetricsState = MetricsState::new();

/// Resolve this thread's metric state: the current runtime's `metrics` field on
/// native, the single cooperative-runtime state on wasm.
#[cfg(not(target_arch = "wasm32"))]
#[inline]
fn metrics_state() -> &'static MetricsState {
    &crate::runtime::rt_current().metrics
}

#[cfg(target_arch = "wasm32")]
#[inline]
fn metrics_state() -> &'static MetricsState {
    &WASM_METRICS
}

/// Resolve this thread's metric state without panicking when no runtime is
/// installed. Backs the observability read/reset surface (`render_snapshot`,
/// `self_metrics`, `session_reset_metrics`), which a host may call before a
/// runtime exists or after teardown: with no runtime there are no per-runtime
/// metrics to read or clear, so those callers treat `None` as "empty".
#[cfg(not(target_arch = "wasm32"))]
#[inline]
fn metrics_state_opt() -> Option<&'static MetricsState> {
    crate::runtime::rt_current_opt().map(|rt| &rt.metrics)
}

#[cfg(target_arch = "wasm32")]
#[inline]
#[allow(
    clippy::unnecessary_wraps,
    reason = "signature mirrors the native arm, which is genuinely fallible; the \
              single cooperative-runtime state is always present on wasm"
)]
fn metrics_state_opt() -> Option<&'static MetricsState> {
    Some(&WASM_METRICS)
}

/// Run `f` with exclusive access to the (lazily-initialised) registry interior.
fn with_registry<R>(f: impl FnOnce(&mut RegistryInner) -> R) -> R {
    metrics_state()
        .registry
        .access(|slot| f(slot.get_or_insert_with(RegistryInner::default)))
}

/// Run `f` over the registry interior for a *read*, tolerating a missing
/// runtime. Returns `None` when no runtime is installed (the read surface — see
/// [`metrics_state_opt`]) or when no metric has yet been registered (the
/// registry interior is still unmaterialised), in both of which cases there is
/// nothing to read. Mirrors the no-runtime tolerance the old process-global
/// registry had for `read_u64`: a read before any registration simply found no
/// such name. Unlike [`with_registry`] this never materialises the registry, so
/// a bare read does not allocate one on a host scrape outside the runtime
/// lifecycle.
fn with_registry_opt<R>(f: impl FnOnce(&RegistryInner) -> R) -> Option<R> {
    let state = metrics_state_opt()?;
    state.registry.access(|slot| slot.as_ref().map(f))
}

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

/// True when `name`'s rendered Prometheus series name collides with a built-in
/// runtime metric the observe registry already owns. A user metric may not
/// shadow one.
///
/// The check is on the rendered name (`.` → `_`), not the canonical name,
/// because the scrape render is non-injective: `metrics.counter("heap_live_bytes")`
/// is a distinct canonical name from the built-in `heap.live_bytes` but renders
/// to the same `heap_live_bytes` series, shadowing the built-in's scrape output.
fn collides_with_builtin(name: &str) -> bool {
    crate::observe::rendered_name_collides_with_builtin(name)
}

/// True when `name`'s rendered Prometheus series name collides with an
/// already-registered user metric's rendered name (under a *different*
/// canonical name). `foo.bar` and `foo_bar` both render to `foo_bar`; the
/// second registration would emit a duplicate `# TYPE` block and an aliased
/// series, so it is rejected. An exact canonical-name match is not a collision
/// here — that is the idempotent register-or-get path handled by the caller.
fn rendered_name_collides_with_existing(reg: &RegistryInner, name: &str) -> bool {
    let rendered = crate::observe::render_prometheus_name(name);
    reg.name_order.iter().any(|existing| {
        existing != name && crate::observe::render_prometheus_name(existing) == rendered
    })
}

/// Register a counter/gauge/histogram name, or return the existing slot if the
/// same name+kind was already registered (idempotent register-or-get).
///
/// Returns the base slot index, or [`REGISTER_FAILED`] when a cap, charset, or
/// collision check fails (the matching self-metric is incremented).
fn register_named(name: &str, kind: MetricKind, buckets: &[i64]) -> i64 {
    if !name_is_valid(name) {
        metrics_state()
            .names_dropped
            .fetch_add(1, Ordering::Relaxed);
        return REGISTER_FAILED;
    }
    if collides_with_builtin(name) {
        metrics_state()
            .collision_rejected
            .fetch_add(1, Ordering::Relaxed);
        metrics_state()
            .names_dropped
            .fetch_add(1, Ordering::Relaxed);
        return REGISTER_FAILED;
    }
    if kind == MetricKind::Histogram && buckets.len() > MAX_HISTOGRAM_BUCKETS {
        metrics_state()
            .names_dropped
            .fetch_add(1, Ordering::Relaxed);
        return REGISTER_FAILED;
    }

    with_registry(|reg| {
        // Idempotent register-or-get: same name + same kind returns the
        // existing base slot; a kind mismatch is a collision.
        if let Some(existing) = reg.names.get(name) {
            if existing.kind == kind {
                return i64_slot(existing.base_slot);
            }
            metrics_state()
                .collision_rejected
                .fetch_add(1, Ordering::Relaxed);
            metrics_state()
                .names_dropped
                .fetch_add(1, Ordering::Relaxed);
            return REGISTER_FAILED;
        }
        // A distinct canonical name that renders to the same Prometheus series
        // as an existing user metric (`foo.bar` vs `foo_bar`) would emit a
        // duplicate `# TYPE` block and an aliased series. Reject it.
        if rendered_name_collides_with_existing(reg, name) {
            metrics_state()
                .collision_rejected
                .fetch_add(1, Ordering::Relaxed);
            metrics_state()
                .names_dropped
                .fetch_add(1, Ordering::Relaxed);
            return REGISTER_FAILED;
        }
        if reg.names.len() >= MAX_NAMES {
            metrics_state()
                .names_dropped
                .fetch_add(1, Ordering::Relaxed);
            return REGISTER_FAILED;
        }

        let base_slot = reg.alloc_slot();
        // A histogram's count rides `base_slot`; the slot immediately after
        // holds the running observation sum, and one cumulative `le` counter
        // slot follows per bucket. Allocate them contiguously after the base so
        // `histogram_record` and the scrape render can address them by offset.
        if kind == MetricKind::Histogram {
            reg.alloc_slot(); // sum slot at base_slot + 1
            for _ in buckets {
                reg.alloc_slot(); // bucket slots at base_slot + 2 + i
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
        metrics_state()
            .names_dropped
            .fetch_add(1, Ordering::Relaxed);
        return REGISTER_FAILED;
    }
    // Labelled histograms are deferred (the bucketed/labelled surface arrives in
    // v0.5.3). `register_vec` reserves only the base slot, but a histogram needs
    // the `base_slot + 1` sum slot that `histogram_record` and the scrape render
    // assume; accepting one here would alias the sum into the next registered
    // metric's slot. Reject it until the labelled surface reserves the slots.
    if kind == MetricKind::Histogram {
        metrics_state()
            .names_dropped
            .fetch_add(1, Ordering::Relaxed);
        return REGISTER_FAILED;
    }
    if collides_with_builtin(name) {
        metrics_state()
            .collision_rejected
            .fetch_add(1, Ordering::Relaxed);
        metrics_state()
            .names_dropped
            .fetch_add(1, Ordering::Relaxed);
        return REGISTER_FAILED;
    }
    if label_keys.len() > MAX_LABEL_KEYS || label_keys.iter().any(|k| !label_key_is_valid(k)) {
        metrics_state()
            .names_dropped
            .fetch_add(1, Ordering::Relaxed);
        return REGISTER_FAILED;
    }

    with_registry(|reg| {
        if let Some(existing) = reg.names.get(name) {
            if existing.kind == kind && existing.label_keys == label_keys {
                return i64_slot(existing.base_slot);
            }
            metrics_state()
                .collision_rejected
                .fetch_add(1, Ordering::Relaxed);
            metrics_state()
                .names_dropped
                .fetch_add(1, Ordering::Relaxed);
            return REGISTER_FAILED;
        }
        // A distinct canonical name that renders to the same Prometheus series
        // as an existing user metric (`foo.bar` vs `foo_bar`) would emit a
        // duplicate `# TYPE` block and an aliased series. Reject it.
        if rendered_name_collides_with_existing(reg, name) {
            metrics_state()
                .collision_rejected
                .fetch_add(1, Ordering::Relaxed);
            metrics_state()
                .names_dropped
                .fetch_add(1, Ordering::Relaxed);
            return REGISTER_FAILED;
        }
        if reg.names.len() >= MAX_NAMES {
            metrics_state()
                .names_dropped
                .fetch_add(1, Ordering::Relaxed);
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
        metrics_state().invalid_ops.fetch_add(1, Ordering::Relaxed);
        return REGISTER_FAILED;
    };
    if label_values.iter().any(|v| v.len() > MAX_LABEL_LEN) {
        metrics_state()
            .series_dropped
            .fetch_add(1, Ordering::Relaxed);
        return REGISTER_FAILED;
    }

    with_registry(|reg| {
        let Some(name) = reg
            .name_order
            .iter()
            .find(|n| reg.names.get(*n).is_some_and(|e| e.base_slot == base_slot))
        else {
            metrics_state().invalid_ops.fetch_add(1, Ordering::Relaxed);
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
            metrics_state().invalid_ops.fetch_add(1, Ordering::Relaxed);
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
            metrics_state()
                .series_dropped
                .fetch_add(1, Ordering::Relaxed);
            return REGISTER_FAILED;
        }
        let slot = reg.alloc_slot();
        if let Some(entry) = reg.names.get_mut(&name) {
            entry.series.insert(key, slot);
        }
        i64_slot(slot)
    })
}

/// Canonical, injection-safe series key in label-declaration order.
///
/// Each label key and value is length-prefixed (`<byte_len>:<bytes>`), so a
/// label value that itself contains the join characters `,` / `=` cannot alias
/// onto a different series or corrupt the decode. Prometheus length-caps label
/// values but does not charset-restrict them, so `{a="x,b=y"}` is a legal,
/// distinct series from `{a="x",b="y"}` — joining raw values with unescaped
/// delimiters would collapse them onto one slot and break the render
/// round-trip. The length prefix makes the key a faithful, reversible identity.
fn canonical_series_key(keys: &[String], values: &[String]) -> String {
    let mut out = String::new();
    for (k, v) in keys.iter().zip(values.iter()) {
        push_len_prefixed(&mut out, k);
        push_len_prefixed(&mut out, v);
    }
    out
}

/// Append `<byte_len>:<bytes>` to `out`.
fn push_len_prefixed(out: &mut String, s: &str) {
    out.push_str(&s.len().to_string());
    out.push(':');
    out.push_str(s);
}

/// Convert a slot index into the `i64` handle ABI value (always `>= 0`).
fn i64_slot(slot: usize) -> i64 {
    i64::try_from(slot).unwrap_or(REGISTER_FAILED)
}

/// Run `f` with the boxed atomic at `handle`, or count an invalid op and run
/// nothing when the handle is out of range.
fn with_slot<R>(handle: i64, f: impl FnOnce(&AtomicI64) -> R) -> Option<R> {
    let Ok(idx) = usize::try_from(handle) else {
        metrics_state().invalid_ops.fetch_add(1, Ordering::Relaxed);
        return None;
    };
    with_registry(|reg| {
        if let Some(slot) = reg.slots.get(idx) {
            Some(f(slot))
        } else {
            metrics_state().invalid_ops.fetch_add(1, Ordering::Relaxed);
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
        if slot
            .fetch_update(Ordering::Relaxed, Ordering::Relaxed, |value| {
                value.checked_add(1)
            })
            .is_err()
        {
            metrics_state().invalid_ops.fetch_add(1, Ordering::Relaxed);
        }
    });
}

/// Add `n` to a counter. A negative `n` is rejected and counted (counters are
/// monotonic).
pub fn counter_add(handle: i64, n: i64) {
    if n < 0 {
        metrics_state().invalid_ops.fetch_add(1, Ordering::Relaxed);
        return;
    }
    with_slot(handle, |slot| {
        if slot
            .fetch_update(Ordering::Relaxed, Ordering::Relaxed, |value| {
                value.checked_add(n)
            })
            .is_err()
        {
            metrics_state().invalid_ops.fetch_add(1, Ordering::Relaxed);
        }
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
/// running observation count rides the base slot, the slot after it accumulates
/// the running sum as an `f64` bit pattern, and each bucket slot accumulates the
/// cumulative `<= upper_bound` count.
pub fn histogram_record(handle: i64, value: f64) {
    let Ok(base_idx) = usize::try_from(handle) else {
        metrics_state().invalid_ops.fetch_add(1, Ordering::Relaxed);
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
            metrics_state().invalid_ops.fetch_add(1, Ordering::Relaxed);
            return;
        };
        if let Some(count) = reg.slots.get(base_idx) {
            count.fetch_add(1, Ordering::Relaxed);
        }
        if let Some(sum) = reg.slots.get(base_idx + 1) {
            atomic_f64_fetch_add(sum, value);
        }
        for (i, bound) in buckets.iter().enumerate() {
            if observation_within_bucket(value, *bound) {
                if let Some(bucket_slot) = reg.slots.get(base_idx + 2 + i) {
                    bucket_slot.fetch_add(1, Ordering::Relaxed);
                }
            }
        }
    });
}

#[allow(
    clippy::cast_precision_loss,
    reason = "histogram buckets are registered as i64 upper bounds; comparing the \
              f64 observation against the same numeric bound preserves the public \
              bucket contract while allowing fractional sums"
)]
fn observation_within_bucket(value: f64, bound: i64) -> bool {
    value <= (bound as f64)
}

/// Atomically add to an `f64` stored as raw bits in an `AtomicI64` slot.
///
/// Histogram sums are the only slot class that uses this representation; count,
/// bucket, counter, and gauge slots remain ordinary integer atomics.
fn atomic_f64_fetch_add(slot: &AtomicI64, delta: f64) {
    let mut current = slot.load(Ordering::Relaxed);
    loop {
        let next = (f64::from_bits(current.cast_unsigned()) + delta)
            .to_bits()
            .cast_signed();
        match slot.compare_exchange_weak(current, next, Ordering::Relaxed, Ordering::Relaxed) {
            Ok(_) => return,
            Err(observed) => current = observed,
        }
    }
}

fn atomic_f64_load(slot: &AtomicI64) -> f64 {
    f64::from_bits(slot.load(Ordering::Relaxed).cast_unsigned())
}

// --- Scrape rendering (consumed by observe.rs in slice 4). ------------------

/// A rendered line: one metric series in Prometheus text form.
#[derive(Debug, Clone, PartialEq)]
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
    /// the base series. For a histogram, the base series value is the running
    /// observation count.
    pub series: Vec<(Vec<String>, i64)>,
    /// For a histogram, the running observation sum (the companion to the count
    /// in `series`). `None` for counters and gauges. Lets the scrape emit a
    /// valid `name_count` / `name_sum` exposition rather than a bare sample
    /// under `# TYPE name histogram`, which Prometheus rejects.
    pub histogram_sum: Option<f64>,
}

/// Snapshot every user metric for the scrape render. Returns metrics in
/// registration order so the scrape output is deterministic. Empty when no
/// runtime is installed (a host scrape outside the runtime lifecycle).
#[must_use]
pub fn render_snapshot() -> Vec<RenderedMetric> {
    let Some(state) = metrics_state_opt() else {
        return Vec::new();
    };
    state.registry.access(|slot| {
        let Some(reg) = slot.as_ref() else {
            return Vec::new();
        };
        let mut out = Vec::with_capacity(reg.name_order.len());
        for name in &reg.name_order {
            let Some(entry) = reg.names.get(name) else {
                continue;
            };
            let prometheus_name = crate::observe::render_prometheus_name(name);
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
            // A histogram's sum rides the slot after its count (base_slot + 1)
            // as an f64 bit pattern.
            let histogram_sum = (entry.kind == MetricKind::Histogram).then(|| {
                reg.slots
                    .get(entry.base_slot + 1)
                    .map_or(0.0, |s| atomic_f64_load(s))
            });
            out.push(RenderedMetric {
                prometheus_name,
                canonical_name: name.clone(),
                type_token: entry.kind.prometheus_type(),
                label_keys: entry.label_keys.clone(),
                series,
                histogram_sum,
            });
        }
        out
    })
}

/// Read the base value for an unlabelled user metric by canonical name.
///
/// A pure read: tolerates a missing runtime (and an unmaterialised registry) by
/// returning `None`, exactly as the old process-global registry did when a name
/// was read before any registration. `observe::read_u64` reaches this for any
/// unknown name, including from a host scrape outside the runtime lifecycle.
#[must_use]
pub fn read_u64(name: &str) -> Option<u64> {
    with_registry_opt(|reg| {
        let entry = reg.names.get(name)?;
        if !entry.label_keys.is_empty() {
            return None;
        }
        let value = reg.slots.get(entry.base_slot)?.load(Ordering::Relaxed);
        u64::try_from(value).ok()
    })
    .flatten()
}

/// Decode a [`canonical_series_key`] back into its label-value list.
///
/// The key is a flat sequence of length-prefixed `<byte_len>:<bytes>` fields,
/// alternating key, value, key, value, …; this returns the value fields in
/// order. A malformed key (only possible from a corrupted registry) yields an
/// empty list rather than panicking.
fn split_series_values(canon: &str) -> Vec<String> {
    let mut values = Vec::new();
    let mut rest = canon;
    let mut field_index: usize = 0;
    while !rest.is_empty() {
        let Some((field, tail)) = take_len_prefixed(rest) else {
            return Vec::new();
        };
        // Even fields are keys, odd fields are values.
        if field_index % 2 == 1 {
            values.push(field.to_string());
        }
        field_index += 1;
        rest = tail;
    }
    values
}

/// Read one `<byte_len>:<bytes>` field from the front of `s`, returning the
/// field bytes and the remaining suffix, or `None` on a malformed prefix.
fn take_len_prefixed(s: &str) -> Option<(&str, &str)> {
    let (len_str, after_colon) = s.split_once(':')?;
    let len: usize = len_str.parse().ok()?;
    if after_colon.len() < len {
        return None;
    }
    // `len` counts bytes; the field boundary must land on a char boundary for
    // the slice to be valid UTF-8 (it always does, since we encoded whole
    // strings, but guard rather than panic on a corrupted key).
    if !after_colon.is_char_boundary(len) {
        return None;
    }
    Some(after_colon.split_at(len))
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

/// Read the registry self-metrics. All zero when no runtime is installed.
#[must_use]
pub fn self_metrics() -> SelfMetrics {
    let Some(state) = metrics_state_opt() else {
        return SelfMetrics::default();
    };
    SelfMetrics {
        names_dropped: state.names_dropped.load(Ordering::Relaxed),
        series_dropped: state.series_dropped.load(Ordering::Relaxed),
        invalid_ops: state.invalid_ops.load(Ordering::Relaxed),
        collision_rejected: state.collision_rejected.load(Ordering::Relaxed),
    }
}

/// Clear the registry. Wired into `observe::reset_all`, the registered
/// session-reset hook on both the native and the wasm scheduler paths, so a
/// fresh session never observes a prior session's metrics. A no-op when no
/// runtime is installed (nothing to clear).
pub fn session_reset_metrics() {
    let Some(state) = metrics_state_opt() else {
        return;
    };
    state.registry.access(|slot| {
        if let Some(reg) = slot.as_mut() {
            reg.names.clear();
            reg.name_order.clear();
            reg.slots.clear();
        }
    });
    state.names_dropped.store(0, Ordering::Relaxed);
    state.series_dropped.store(0, Ordering::Relaxed);
    state.invalid_ops.store(0, Ordering::Relaxed);
    state.collision_rejected.store(0, Ordering::Relaxed);
}

// =============================================================================
// FFI ABI — hew_metric_* — the emit path std::metrics lowers to.
// =============================================================================
//
// `std::metrics` declares these in an `extern "C"` block and calls them inside
// `unsafe`. The ABI is intentionally narrow: a `*const c_char` name in, an
// `i64` handle out (>= 0 valid, -1 = REGISTER_FAILED), and `i64`/`f64` values.
// Handles are non-owning index IDs — Copy on the Hew side, no free, no
// double-free class.

use std::ffi::{c_char, CStr};

/// Borrow a C string as `&str`, or `None` on null / non-UTF-8.
///
/// # Safety
///
/// `ptr` must be null or a valid NUL-terminated C string for the call.
unsafe fn cstr_opt<'a>(ptr: *const c_char) -> Option<&'a str> {
    if ptr.is_null() {
        return None;
    }
    // SAFETY: caller guarantees a valid NUL-terminated C string per contract.
    unsafe { CStr::from_ptr(ptr) }.to_str().ok()
}

/// Collect `n` C strings from a `*const *const c_char` array into owned
/// `String`s. Returns `None` if the array pointer is null or any element is
/// null / non-UTF-8.
///
/// # Safety
///
/// `arr` must be null or point to `n` valid `*const c_char` entries, each null
/// or a valid NUL-terminated C string.
unsafe fn cstr_array(arr: *const *const c_char, n: i64) -> Option<Vec<String>> {
    let Ok(n) = usize::try_from(n) else {
        return None;
    };
    if n == 0 {
        return Some(Vec::new());
    }
    if arr.is_null() {
        return None;
    }
    let mut out = Vec::with_capacity(n);
    for i in 0..n {
        // SAFETY: caller guarantees `n` valid entries; `i < n`.
        let elem = unsafe { *arr.add(i) };
        // SAFETY: each entry is null or a valid C string per contract.
        let s = unsafe { cstr_opt(elem) }?;
        out.push(s.to_string());
    }
    Some(out)
}

/// Register (or get) a counter. Returns the handle, or -1 on failure.
///
/// # Safety
///
/// `name` must be null or a valid NUL-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_metric_counter_register(name: *const c_char) -> i64 {
    // SAFETY: forwarded contract on `name`.
    match unsafe { cstr_opt(name) } {
        Some(name) => register_counter(name),
        None => REGISTER_FAILED,
    }
}

/// Increment a counter by one.
#[no_mangle]
pub extern "C" fn hew_metric_counter_inc(handle: i64) {
    inc(handle);
}

/// Add `n` to a counter; a negative `n` is rejected and counted.
#[no_mangle]
pub extern "C" fn hew_metric_counter_add(handle: i64, n: i64) {
    counter_add(handle, n);
}

/// Register (or get) a gauge. Returns the handle, or -1 on failure.
///
/// # Safety
///
/// `name` must be null or a valid NUL-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_metric_gauge_register(name: *const c_char) -> i64 {
    // SAFETY: forwarded contract on `name`.
    match unsafe { cstr_opt(name) } {
        Some(name) => register_gauge(name),
        None => REGISTER_FAILED,
    }
}

/// Set a gauge to `n`.
#[no_mangle]
pub extern "C" fn hew_metric_gauge_set(handle: i64, n: i64) {
    gauge_set(handle, n);
}

/// Increment a gauge by one.
#[no_mangle]
pub extern "C" fn hew_metric_gauge_inc(handle: i64) {
    inc(handle);
}

/// Decrement a gauge by one.
#[no_mangle]
pub extern "C" fn hew_metric_gauge_dec(handle: i64) {
    gauge_dec(handle);
}

/// Add `n` (any sign) to a gauge.
#[no_mangle]
pub extern "C" fn hew_metric_gauge_add(handle: i64, n: i64) {
    gauge_add(handle, n);
}

/// Register (or get) a histogram with `n_buckets` ascending bucket upper
/// bounds. Returns the handle, or -1 on failure.
///
/// # Safety
///
/// `name` must be null or a valid NUL-terminated C string; `buckets` must be
/// null or point to `n_buckets` readable `i64` values.
#[no_mangle]
pub unsafe extern "C" fn hew_metric_histogram_register(
    name: *const c_char,
    buckets: *const i64,
    n_buckets: i64,
) -> i64 {
    // SAFETY: forwarded contract on `name`.
    let Some(name) = (unsafe { cstr_opt(name) }) else {
        return REGISTER_FAILED;
    };
    let Ok(n) = usize::try_from(n_buckets) else {
        return REGISTER_FAILED;
    };
    let bucket_slice: &[i64] = if n == 0 || buckets.is_null() {
        &[]
    } else {
        // SAFETY: caller guarantees `n_buckets` readable i64s at `buckets`.
        unsafe { std::slice::from_raw_parts(buckets, n) }
    };
    register_histogram(name, bucket_slice)
}

/// Register (or get) a bucketless histogram by name. Returns the handle, or -1
/// on failure.
///
/// The bucketed [`hew_metric_histogram_register`] takes a raw `(*const i64,
/// len)` bucket array that a Hew `extern "C"` declaration cannot express, so
/// `std::metrics` reaches this name-only entry point instead. The registered
/// histogram carries the running observation count with no `le` buckets.
///
/// # Safety
///
/// `name` must be null or a valid NUL-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_metric_histogram_register_simple(name: *const c_char) -> i64 {
    // SAFETY: forwarded contract on `name`.
    match unsafe { cstr_opt(name) } {
        Some(name) => register_histogram(name, &[]),
        None => REGISTER_FAILED,
    }
}

/// Record one histogram observation. Fractional observations contribute their
/// full value to the running `_sum`; integer bucket bounds are compared against
/// the original floating-point observation.
#[no_mangle]
pub extern "C" fn hew_metric_histogram_record(handle: i64, value: f64) {
    // A non-finite observation is rejected and counted.
    if !value.is_finite() {
        metrics_state().invalid_ops.fetch_add(1, Ordering::Relaxed);
        return;
    }
    histogram_record(handle, value);
}

/// Register (or get) a labelled metric of `kind` (0 = counter, 1 = gauge,
/// 2 = histogram) with `n_keys` label keys. Returns the base handle, or -1.
///
/// # Safety
///
/// `name` must be null or a valid C string; `keys` must be null or point to
/// `n_keys` valid `*const c_char` entries.
#[no_mangle]
pub unsafe extern "C" fn hew_metric_vec_register(
    name: *const c_char,
    kind: i64,
    keys: *const *const c_char,
    n_keys: i64,
) -> i64 {
    // SAFETY: forwarded contract on `name`.
    let Some(name) = (unsafe { cstr_opt(name) }) else {
        return REGISTER_FAILED;
    };
    let kind = match kind {
        0 => MetricKind::Counter,
        1 => MetricKind::Gauge,
        2 => MetricKind::Histogram,
        _ => {
            metrics_state()
                .names_dropped
                .fetch_add(1, Ordering::Relaxed);
            return REGISTER_FAILED;
        }
    };
    // SAFETY: forwarded contract on `keys`.
    let Some(label_keys) = (unsafe { cstr_array(keys, n_keys) }) else {
        metrics_state()
            .names_dropped
            .fetch_add(1, Ordering::Relaxed);
        return REGISTER_FAILED;
    };
    register_labelled(name, kind, &label_keys)
}

/// Resolve (or materialise) the per-series handle for a labelled metric. The
/// `vals` array must carry exactly the metric's declared label arity. Returns
/// the series handle, or -1.
///
/// # Safety
///
/// `vals` must be null or point to `n_vals` valid `*const c_char` entries.
#[no_mangle]
pub unsafe extern "C" fn hew_metric_vec_with(
    base_handle: i64,
    vals: *const *const c_char,
    n_vals: i64,
) -> i64 {
    // SAFETY: forwarded contract on `vals`.
    let Some(label_values) = (unsafe { cstr_array(vals, n_vals) }) else {
        metrics_state().invalid_ops.fetch_add(1, Ordering::Relaxed);
        return REGISTER_FAILED;
    };
    resolve_series(base_handle, &label_values)
}

#[cfg(all(test, not(target_family = "wasm")))]
mod tests {
    use super::*;
    use std::sync::{Arc, Barrier};
    use std::thread;

    // The metric registry now lives in `RuntimeInner.metrics`, so each test owns
    // a *private* runtime and `enter`s it on its own thread; `rt_current()`
    // resolves that runtime through thread-local state. Two metrics tests on two
    // threads therefore touch independent registries — there is no
    // process-global registry left to serialise, so the dedicated `TEST_LOCK`
    // this commit deletes is gone. The observe scrape tests keep using
    // `runtime_test_guard()` (the default slot under `SchedTestLock`); a
    // private-runtime metrics test cannot alias their registry, so the two
    // families no longer need a shared mutual-exclusion point over one global.

    /// A test's private runtime, entered on the test thread for its duration.
    /// `_enter` is declared before `rt` so it drops first: the thread-local
    /// selection is restored before the runtime it named is freed.
    struct MetricsTestGuard {
        _enter: crate::runtime::EnterGuard,
        rt: Box<crate::runtime::RuntimeInner>,
    }

    impl MetricsTestGuard {
        /// Pointer to the entered runtime, so a spawned worker can `enter` the
        /// *same* per-runtime registry (thread-local state is per-thread, so a
        /// worker thread must re-enter the runtime explicitly).
        fn runtime_ptr(&self) -> *const crate::runtime::RuntimeInner {
            std::ptr::from_ref(&*self.rt)
        }
    }

    fn guard() -> MetricsTestGuard {
        let rt = Box::new(crate::runtime::RuntimeInner::new(
            crate::scheduler::worker_less_scheduler(),
        ));
        // SAFETY: the guard owns `rt` and drops `_enter` before it, so the
        // entered runtime outlives the thread-local selection that names it.
        let enter = unsafe { crate::runtime::enter(&rt) };
        MetricsTestGuard { _enter: enter, rt }
    }

    /// `*const RuntimeInner` that may cross into a worker thread. The pointer is
    /// dereferenced only while the owning [`MetricsTestGuard`] keeps the runtime
    /// alive — every test that shares it joins its workers before dropping the
    /// guard.
    #[derive(Clone, Copy)]
    struct SharedRuntime(*const crate::runtime::RuntimeInner);
    // SAFETY: deref is bounded by the owning guard's lifetime (workers are
    // joined before the guard drops), and `RuntimeInner`'s metric state is
    // internally synchronised (a `PoisonSafe` lock plus atomics).
    unsafe impl Send for SharedRuntime {}

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
    fn rendered_collision_with_builtin_is_rejected_and_counted() {
        let _g = guard();
        // `heap_live_bytes` is a distinct canonical name from the built-in
        // `heap.live_bytes`, but both render to the same Prometheus series
        // `heap_live_bytes`. Registering it would shadow the built-in's scrape
        // output, so the rendered-name check must reject it.
        let h = register_counter("heap_live_bytes");
        assert_eq!(
            h, REGISTER_FAILED,
            "a name that renders to a built-in's series must be rejected"
        );
        assert_eq!(self_metrics().collision_rejected, 1);
    }

    #[test]
    fn rendered_collision_between_user_metrics_is_rejected_and_counted() {
        let _g = guard();
        // `foo.bar` and `foo_bar` are distinct canonical names that render to
        // the same `foo_bar` Prometheus series. The first registers; the second
        // would emit a duplicate `# TYPE` block and alias the first's series, so
        // it is rejected and counted.
        let first = register_counter("foo.bar");
        assert!(first >= 0, "first canonical name registers");
        let second = register_counter("foo_bar");
        assert_eq!(
            second, REGISTER_FAILED,
            "a second name rendering to an existing series must be rejected"
        );
        assert_eq!(self_metrics().collision_rejected, 1);
        // The exact-name re-register is still the idempotent get, not a
        // collision: it returns the same slot and does not bump the counter.
        let again = register_counter("foo.bar");
        assert_eq!(again, first, "exact re-register is idempotent get");
        assert_eq!(self_metrics().collision_rejected, 1);
    }

    #[test]
    fn collision_with_attributed_turn_series_is_rejected_and_counted() {
        let _g = guard();
        // The scrape emits the per-handler attributed-turn series under these
        // rendered names. A user metric must not shadow them. The canonical
        // (dotted) form renders onto the reserved series, so it is rejected.
        let dotted = register_counter("actors.attributed_turns_by_handler_total");
        assert_eq!(
            dotted, REGISTER_FAILED,
            "must not shadow the attributed-turn series"
        );
        // The already-rendered (underscored) form must be rejected too.
        let rendered = register_counter("actors_attributed_turn_duration_ns_by_handler_total");
        assert_eq!(
            rendered, REGISTER_FAILED,
            "rendered attributed-turn name must also be rejected"
        );
        assert_eq!(self_metrics().collision_rejected, 2);
    }

    #[test]
    fn collision_with_self_metric_is_rejected_and_counted() {
        let _g = guard();
        // The registry's own fail-closed self-metrics (`hew_metrics_*_total`)
        // are scrape-owned reserved names; a user metric that shadowed one would
        // mask the very drop/reject counters that signal lost data.
        let h = register_counter("hew_metrics_names_dropped_total");
        assert_eq!(h, REGISTER_FAILED, "must not shadow a registry self-metric");
        assert_eq!(self_metrics().collision_rejected, 1);
    }

    #[test]
    fn labelled_histogram_register_is_rejected_and_counted() {
        let _g = guard();
        // Labelled histograms are deferred: `register_vec` reserves only the
        // base slot, not the sum slot the histogram path assumes, so accepting
        // one would alias the sum into the next metric's slot. It is rejected.
        let h = register_labelled("app.lhist", MetricKind::Histogram, &["route".to_string()]);
        assert_eq!(h, REGISTER_FAILED, "labelled histogram must be rejected");
        assert!(self_metrics().names_dropped >= 1);
        // The FFI vec-register path (kind 2 = histogram) must reject too.
        let name = CString::new("app.lhist.ffi").unwrap();
        // SAFETY: valid name, null key array with zero keys.
        let ffi = unsafe { hew_metric_vec_register(name.as_ptr(), 2, std::ptr::null(), 0) };
        assert_eq!(
            ffi, REGISTER_FAILED,
            "FFI labelled histogram must be rejected"
        );
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
    fn series_key_is_injection_safe() {
        let _g = guard();
        // A two-label metric whose first value embeds the join characters
        // `,` and `=` must NOT alias onto the single-label series whose value
        // happens to render to the same raw bytes, and the value must survive
        // the render round-trip intact.
        let two = register_labelled(
            "app.inj",
            MetricKind::Counter,
            &["a".to_string(), "b".to_string()],
        );
        assert!(two >= 0);
        // `{a="x,b=y", b="z"}` — the first value contains the delimiters.
        let injected = resolve_series(two, &["x,b=y".to_string(), "z".to_string()]);
        // `{a="x", b="y"}` on the SAME metric — a naive `k=v,` join would
        // collapse both onto the key `a=x,b=y` and the second would alias the
        // first. The length-prefixed key keeps them distinct.
        let plain = resolve_series(two, &["x".to_string(), "y".to_string()]);
        assert_ne!(
            injected, plain,
            "delimiter-bearing value must not alias a distinct series"
        );
        inc(injected);
        gauge_set(plain, 7); // gauge_set works on any slot; sets plain to 7

        let snap = render_snapshot();
        let m = snap.iter().find(|m| m.canonical_name == "app.inj").unwrap();
        assert_eq!(m.series.len(), 2, "two distinct series");
        // The injected value round-trips byte-for-byte.
        let injected_series = m
            .series
            .iter()
            .find(|(vals, _)| vals == &vec!["x,b=y".to_string(), "z".to_string()])
            .expect("injected series must round-trip its raw value");
        assert_eq!(injected_series.1, 1);
        let plain_series = m
            .series
            .iter()
            .find(|(vals, _)| vals == &vec!["x".to_string(), "y".to_string()])
            .expect("plain series present and distinct");
        assert_eq!(plain_series.1, 7);
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
        histogram_record(h, 5.0); // <=10, <=100, <=1000
        histogram_record(h, 50.0); // <=100, <=1000
        histogram_record(h, 5000.0); // none
        let snap = render_snapshot();
        let m = snap.iter().find(|m| m.canonical_name == "app.dur").unwrap();
        assert_eq!(m.type_token, "histogram");
        // base slot holds the total observation count.
        assert_eq!(m.series[0].1, 3, "three observations recorded");
        // The sum rides the slot after the count: 5 + 50 + 5000 = 5055.
        assert_eq!(
            m.histogram_sum,
            Some(5055.0),
            "histogram must track the observation sum"
        );
    }

    #[test]
    fn scalar_histogram_tracks_count_and_sum() {
        let _g = guard();
        // The Phase A scalar histogram (no `le` buckets) must still track both
        // the observation count and the running sum so the scrape can emit a
        // valid `_count` / `_sum` exposition.
        let h = register_histogram("app.scalar", &[]);
        assert!(h >= 0);
        histogram_record(h, 3.0);
        histogram_record(h, 7.0);
        let snap = render_snapshot();
        let m = snap
            .iter()
            .find(|m| m.canonical_name == "app.scalar")
            .unwrap();
        assert_eq!(m.type_token, "histogram");
        assert_eq!(m.series[0].1, 2, "two observations recorded");
        assert_eq!(m.histogram_sum, Some(10.0), "sum is 3 + 7");
    }

    #[test]
    fn scalar_histogram_preserves_fractional_sum() {
        let _g = guard();
        let h = register_histogram("app.scalar_fractional", &[]);
        assert!(h >= 0);
        hew_metric_histogram_record(h, 0.5);
        hew_metric_histogram_record(h, 0.25);
        let snap = render_snapshot();
        let m = snap
            .iter()
            .find(|m| m.canonical_name == "app.scalar_fractional")
            .unwrap();
        assert_eq!(m.series[0].1, 2, "two observations recorded");
        assert_eq!(m.histogram_sum, Some(0.75), "sum keeps fractions");
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
    fn counter_overflow_is_rejected_without_wrapping() {
        let _g = guard();
        let h = register_counter("app.counter_overflow");
        with_slot(h, |slot| slot.store(i64::MAX, Ordering::Relaxed));
        let invalid_before = self_metrics().invalid_ops;

        inc(h);
        counter_add(h, 1);

        assert_eq!(
            with_slot(h, |slot| slot.load(Ordering::Relaxed)),
            Some(i64::MAX),
            "overflow must preserve the monotonic counter value"
        );
        assert_eq!(self_metrics().invalid_ops, invalid_before + 2);
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
        let g = guard();
        let h = register_counter("app.concurrent");
        let threads: i64 = 8;
        let per: i64 = 1000;
        // Workers share the test's private runtime: each re-enters it so `inc`
        // resolves the same per-runtime registry (thread-local is per-thread).
        let shared = SharedRuntime(g.runtime_ptr());
        let start = Arc::new(Barrier::new(usize::try_from(threads).unwrap()));
        let mut joins = Vec::new();
        for _ in 0..threads {
            let start = Arc::clone(&start);
            joins.push(thread::spawn(move || {
                let shared = shared; // capture the Send wrapper whole, not its raw field
                                     // SAFETY: `g` is held until after every join below, so the
                                     // shared runtime stays alive for this enter.
                let _enter = unsafe { crate::runtime::enter(&*shared.0) };
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
        drop(g);
    }

    #[test]
    fn independent_runtimes_keep_separate_metric_registries() {
        // The de-globalization dividend: each runtime owns its metric registry,
        // so two threads — each with its *own* runtime — register the SAME
        // metric name and mutate it concurrently to independent values. The old
        // process-global registry needed the now-deleted `TEST_LOCK` serializer
        // for this; independent runtimes stay isolated with no lock at all.
        let start = Arc::new(Barrier::new(2));
        let workers: Vec<_> = [3_i64, 7_i64]
            .into_iter()
            .map(|increments| {
                let start = Arc::clone(&start);
                thread::spawn(move || {
                    let _g = guard(); // a private runtime for this thread
                    start.wait(); // register + mutate concurrently
                    let h = register_counter("app.same_name");
                    assert!(h >= 0);
                    for _ in 0..increments {
                        inc(h);
                    }
                    render_snapshot()
                        .iter()
                        .find(|m| m.canonical_name == "app.same_name")
                        .expect("each runtime sees its own metric")
                        .series[0]
                        .1
                })
            })
            .collect();
        let mut seen: Vec<i64> = workers.into_iter().map(|w| w.join().unwrap()).collect();
        seen.sort_unstable();
        // A shared registry would read 10 (3 + 7) on both threads (or race);
        // independent registries each see only their own increments.
        assert_eq!(
            seen,
            vec![3, 7],
            "each runtime's counter reflects only its own increments"
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

    // --- FFI ABI boundary tests: call each hew_metric_* symbol directly. -----

    use std::ffi::CString;

    fn value_of(name: &str) -> i64 {
        render_snapshot()
            .into_iter()
            .find(|m| m.canonical_name == name)
            .map_or(i64::MIN, |m| m.series[0].1)
    }

    #[test]
    fn ffi_counter_register_inc_add_round_trips() {
        let _g = guard();
        let name = CString::new("ffi.counter").unwrap();
        // SAFETY: name is a valid C string.
        let h = unsafe { hew_metric_counter_register(name.as_ptr()) };
        assert!(h >= 0);
        hew_metric_counter_inc(h);
        hew_metric_counter_add(h, 4);
        hew_metric_counter_add(h, -1); // rejected
        assert_eq!(value_of("ffi.counter"), 5, "1 + 4, negative add ignored");
        assert_eq!(self_metrics().invalid_ops, 1);
    }

    #[test]
    fn ffi_counter_register_null_name_is_failed_sentinel() {
        let _g = guard();
        // SAFETY: null is an explicitly handled input.
        let h = unsafe { hew_metric_counter_register(std::ptr::null()) };
        assert_eq!(h, REGISTER_FAILED);
    }

    #[test]
    fn ffi_gauge_full_surface_round_trips() {
        let _g = guard();
        let name = CString::new("ffi.gauge").unwrap();
        // SAFETY: valid C string.
        let h = unsafe { hew_metric_gauge_register(name.as_ptr()) };
        assert!(h >= 0);
        hew_metric_gauge_set(h, 10);
        hew_metric_gauge_inc(h);
        hew_metric_gauge_dec(h);
        hew_metric_gauge_dec(h);
        hew_metric_gauge_add(h, -3);
        // 10 +1 -1 -1 -3 = 6
        assert_eq!(value_of("ffi.gauge"), 6);
    }

    #[test]
    fn ffi_histogram_register_and_record() {
        let _g = guard();
        let name = CString::new("ffi.hist").unwrap();
        let buckets: [i64; 3] = [10, 100, 1000];
        let n = i64::try_from(buckets.len()).unwrap();
        // SAFETY: valid name and a 3-element i64 array.
        let h = unsafe { hew_metric_histogram_register(name.as_ptr(), buckets.as_ptr(), n) };
        assert!(h >= 0);
        hew_metric_histogram_record(h, 5.0);
        hew_metric_histogram_record(h, 50.0);
        hew_metric_histogram_record(h, 5000.0);
        hew_metric_histogram_record(h, f64::NAN); // rejected
                                                  // base slot = observation count = 3 (NaN rejected).
        assert_eq!(value_of("ffi.hist"), 3);
        assert_eq!(self_metrics().invalid_ops, 1, "NaN observation counted");
    }

    #[test]
    fn ffi_vec_register_and_with_round_trips() {
        let _g = guard();
        let name = CString::new("ffi.req").unwrap();
        let key = CString::new("route").unwrap();
        let keys: [*const c_char; 1] = [key.as_ptr()];
        // SAFETY: valid name and a 1-element key array.
        let base = unsafe { hew_metric_vec_register(name.as_ptr(), 0, keys.as_ptr(), 1) };
        assert!(base >= 0);

        let val = CString::new("home").unwrap();
        let vals: [*const c_char; 1] = [val.as_ptr()];
        // SAFETY: valid 1-element value array matching the declared arity.
        let series = unsafe { hew_metric_vec_with(base, vals.as_ptr(), 1) };
        assert!(series >= 0);
        hew_metric_counter_inc(series);
        hew_metric_counter_inc(series);

        let snap = render_snapshot();
        let m = snap.iter().find(|m| m.canonical_name == "ffi.req").unwrap();
        assert_eq!(m.label_keys, vec!["route".to_string()]);
        assert_eq!(m.series.len(), 1);
        assert_eq!(m.series[0].0, vec!["home".to_string()]);
        assert_eq!(m.series[0].1, 2);
    }

    #[test]
    fn ffi_vec_register_rejects_unknown_kind() {
        let _g = guard();
        let name = CString::new("ffi.badkind").unwrap();
        // SAFETY: valid name, null key array with zero keys.
        let h = unsafe { hew_metric_vec_register(name.as_ptr(), 99, std::ptr::null(), 0) };
        assert_eq!(h, REGISTER_FAILED);
    }

    /// `read_u64` is a pure read and must tolerate a missing runtime, returning
    /// `None` rather than aborting "no runtime installed". `observe::read_u64`
    /// reaches it for any unknown name with no runtime guard installed (the
    /// observe read tests), which is the path that aborted before the fix. With
    /// no runtime there is no registry, so any name reads as absent.
    #[test]
    fn read_u64_tolerates_no_runtime() {
        // No `guard()`: this thread has no runtime installed.
        assert_eq!(read_u64("does.not.exist"), None);
        // A name that a runtime-installed test would register also reads None
        // here, since there is no registry to hold it.
        assert_eq!(read_u64("app.unregistered"), None);
    }

    /// With a runtime installed `read_u64` resolves the per-runtime registry and
    /// returns the registered value — confirming the deglobalization is intact
    /// and the no-runtime tolerance did not flatten the read to an always-None.
    #[test]
    fn read_u64_with_runtime_returns_registered_value() {
        let _g = guard();
        let h = register_counter("app.read_back");
        assert!(h >= 0);
        inc(h);
        inc(h);
        assert_eq!(read_u64("app.read_back"), Some(2));
        assert_eq!(read_u64("app.never_registered"), None);
    }
}
