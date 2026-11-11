//! Standalone verifier regression; not part of the Hew proof selection.
//!
//! `rustc atomic_vec.rs && ./atomic_vec` succeeds, including with the verifier's
//! pinned nightly-2026-08-21 toolchain. `kani atomic_vec.rs` with Kani 0.68.0
//! reports that an empty Vec has nonzero capacity after a counter changes.
//! There are no Hew operations, unsafe operations or concurrent threads here.
//! Keep this reproducer separate from product proofs: its failure demonstrates
//! the tool limitation that motivated invocation-local ownership observers.

use std::sync::atomic::{AtomicUsize, Ordering};
static CLONES: AtomicUsize = AtomicUsize::new(0);
static DROPS: AtomicUsize = AtomicUsize::new(0);
static FREED: AtomicUsize = AtomicUsize::new(0);

#[cfg_attr(kani, kani::proof)]
fn counters_do_not_change_empty_vector_capacity() {
    CLONES.store(0, Ordering::Relaxed);
    DROPS.store(0, Ordering::Relaxed);
    FREED.store(0, Ordering::Relaxed);
    CLONES.fetch_add(1, Ordering::Relaxed);
    assert_eq!(Vec::<*mut u8>::new().capacity(), 0);
    DROPS.fetch_add(1, Ordering::Relaxed);
    assert_eq!(Vec::<*mut u8>::new().capacity(), 0);
    FREED.fetch_add(1, Ordering::Relaxed);
    assert_eq!(Vec::<*mut u8>::new().capacity(), 0);
}

#[cfg(not(kani))]
fn main() {
    counters_do_not_change_empty_vector_capacity();
}
