//! Criterion benchmark for the HashMap layout-kernel (Stage C0a substrate).
//!
//! Establishes a measurable baseline immediately after Stage C0a (commit
//! `83189329`) so subsequent Stage C/D changes have regression signal. The
//! bench exercises the public layout-aware C ABI exactly as a real codegen
//! consumer would (no internal helpers): `hew_hashmap_new_with_layout`,
//! `hew_hashmap_insert_layout`, `hew_hashmap_remove_layout`,
//! `hew_hashmap_free_layout`, with real `extern "C"` drop/hash/eq thunks.
//!
//! Five groups, covering all three drop-hook sites the C0a substrate added
//! (insert-overwrite, remove, free):
//!   1. `insert_string_kv`           — K=string V=string, N ∈ {1, 10, 100, 1k, 10k}.
//!                                     Insert-only path; free at end exercises the
//!                                     bulk-drop hook for K and V.
//!   2. `insert_remove_i64`          — K=i64 V=i64 Plain (drop_fn = None),
//!                                     N ∈ {1k, 10k}. Insert N, remove N, free —
//!                                     exercises the drop-skip fast path.
//!   3. `free_string_kv`             — K=string V=string, N ∈ {1k, 10k}. Isolates
//!                                     the free-time iteration + drop-hook cost
//!                                     (populate outside the timed region).
//!   4. `insert_overwrite_string_v`  — K=string V=string, N = 1_000. Pre-populate,
//!                                     then time overwriting EVERY key with a
//!                                     fresh value — each insert fires the
//!                                     drop-old-V hook (occupied-slot path).
//!   5. `remove_string_kv`           — K=string V=string, N = 1_000. Pre-populate,
//!                                     then time `hew_hashmap_remove_layout` for
//!                                     every key — each remove fires both the K
//!                                     and V drop hooks at the per-slot remove site.
//!
//! Baseline numbers recorded in `HASHMAP_LAYOUT_BASELINE.md` adjacent.
//! Future Stage C/D commits compare their `cargo bench` output to that file.

#![allow(
    clippy::cast_possible_truncation,
    clippy::cast_possible_wrap,
    clippy::cast_sign_loss,
    clippy::doc_markdown,
    clippy::doc_overindented_list_items,
    clippy::undocumented_unsafe_blocks,
    reason = "bench harness — scalar casts are intentional, table-style doc \
              comments use intentional indentation for column alignment, FFI \
              thunk safety is documented in the sibling tests under \
              hew-runtime/tests/hashmap_layout_drop_*.rs"
)]

use std::ffi::{c_char, c_void, CString};

use criterion::{criterion_group, criterion_main, BatchSize, BenchmarkId, Criterion, Throughput};

use hew_cabi::map::{
    HewMapKeyEqThunk, HewMapKeyHashThunk, HewMapKeyLayout, HewMapValueDropThunk, HewMapValueLayout,
};
use hew_cabi::vec::HewTypeOwnershipKind;
use hew_runtime::hashmap::{
    hew_hashmap_free_layout, hew_hashmap_insert_layout, hew_hashmap_new_with_layout,
    hew_hashmap_remove_layout,
};

// ---------------------------------------------------------------------------
// String-K / String-V descriptors
// ---------------------------------------------------------------------------
//
// Slot shape: each K slot holds a `*mut c_char` (pointer-sized); the chars
// themselves live on the heap and were produced by `CString::into_raw`. Same
// shape for V slots. `drop_fn` reclaims via `CString::from_raw` + drop.

extern "C" fn cstring_slot_drop(blob: *mut c_void) {
    unsafe {
        let cstr_ptr = *blob.cast::<*mut c_char>();
        if !cstr_ptr.is_null() {
            drop(CString::from_raw(cstr_ptr));
        }
    }
}

unsafe extern "C" fn hash_cstr_slot(blob: *const c_void) -> u64 {
    let cstr_ptr = unsafe { *blob.cast::<*const c_char>() };
    // FNV-1a over the C string bytes.
    let mut h: u64 = 0xcbf2_9ce4_8422_2325;
    let mut p = cstr_ptr;
    unsafe {
        while *p != 0 {
            h ^= u64::from(*p as u8);
            h = h.wrapping_mul(0x0000_0100_0000_01B3);
            p = p.add(1);
        }
    }
    h
}

unsafe extern "C" fn eq_cstr_slot(lhs: *const c_void, rhs: *const c_void) -> i32 {
    let l = unsafe { *lhs.cast::<*const c_char>() };
    let r = unsafe { *rhs.cast::<*const c_char>() };
    if l == r {
        return 1;
    }
    unsafe {
        let mut lp = l;
        let mut rp = r;
        loop {
            let lc = *lp;
            let rc = *rp;
            if lc != rc {
                return 0;
            }
            if lc == 0 {
                return 1;
            }
            lp = lp.add(1);
            rp = rp.add(1);
        }
    }
}

fn string_descriptors() -> (HewMapKeyLayout, HewMapValueLayout) {
    let kl = HewMapKeyLayout {
        size: size_of::<*mut c_char>(),
        align: align_of::<*mut c_char>(),
        ownership_kind: HewTypeOwnershipKind::String,
        hash_fn: Some(hash_cstr_slot as HewMapKeyHashThunk),
        eq_fn: Some(eq_cstr_slot as HewMapKeyEqThunk),
        drop_fn: Some(cstring_slot_drop as HewMapValueDropThunk),
    };
    let vl = HewMapValueLayout {
        size: size_of::<*mut c_char>(),
        align: align_of::<*mut c_char>(),
        ownership_kind: HewTypeOwnershipKind::String,
        drop_fn: Some(cstring_slot_drop as HewMapValueDropThunk),
        clone_fn: None,
    };
    (kl, vl)
}

/// Produce N unique owned key/value C string raw pointers. The caller will
/// hand each pair to `hew_hashmap_insert_layout`, which takes ownership.
fn make_string_pairs(n: usize) -> Vec<(*mut c_char, *mut c_char)> {
    (0..n)
        .map(|i| {
            let k = CString::new(format!("key-{i:010}")).unwrap().into_raw();
            let v = CString::new(format!("val-{i:010}")).unwrap().into_raw();
            (k, v)
        })
        .collect()
}

// ---------------------------------------------------------------------------
// i64 / i64 Plain descriptors
// ---------------------------------------------------------------------------

unsafe extern "C" fn hash_i64(key: *const c_void) -> u64 {
    let v = unsafe { *key.cast::<i64>() };
    (v as u64).wrapping_mul(0x9E37_79B9_7F4A_7C15)
}

unsafe extern "C" fn eq_i64(lhs: *const c_void, rhs: *const c_void) -> i32 {
    let l = unsafe { *lhs.cast::<i64>() };
    let r = unsafe { *rhs.cast::<i64>() };
    i32::from(l == r)
}

fn i64_plain_descriptors() -> (HewMapKeyLayout, HewMapValueLayout) {
    let kl = HewMapKeyLayout {
        size: size_of::<i64>(),
        align: align_of::<i64>(),
        ownership_kind: HewTypeOwnershipKind::Plain,
        hash_fn: Some(hash_i64 as HewMapKeyHashThunk),
        eq_fn: Some(eq_i64 as HewMapKeyEqThunk),
        drop_fn: None,
    };
    let vl = HewMapValueLayout {
        size: size_of::<i64>(),
        align: align_of::<i64>(),
        ownership_kind: HewTypeOwnershipKind::Plain,
        drop_fn: None,
        clone_fn: None,
    };
    (kl, vl)
}

// ---------------------------------------------------------------------------
// Benches
// ---------------------------------------------------------------------------

fn bench_insert_string_kv(c: &mut Criterion) {
    let mut group = c.benchmark_group("insert_string_kv");
    let (kl, vl) = string_descriptors();
    for &n in &[1usize, 10, 100, 1_000, 10_000] {
        group.throughput(Throughput::Elements(n as u64));
        group.bench_with_input(BenchmarkId::from_parameter(n), &n, |b, &n| {
            b.iter_batched(
                || make_string_pairs(n),
                |pairs| unsafe {
                    let m = hew_hashmap_new_with_layout(&raw const kl, &raw const vl);
                    for (k_ptr, v_ptr) in &pairs {
                        let k_slot: *mut c_char = *k_ptr;
                        let v_slot: *mut c_char = *v_ptr;
                        hew_hashmap_insert_layout(
                            m,
                            (&raw const k_slot).cast::<c_void>(),
                            (&raw const v_slot).cast::<c_void>(),
                        );
                    }
                    // Free drops every stored K + V via the registered drop_fn.
                    hew_hashmap_free_layout(m);
                },
                BatchSize::SmallInput,
            );
        });
    }
    group.finish();
}

fn bench_insert_remove_i64(c: &mut Criterion) {
    let mut group = c.benchmark_group("insert_remove_i64");
    let (kl, vl) = i64_plain_descriptors();
    for &n in &[1_000i64, 10_000] {
        group.throughput(Throughput::Elements(n as u64));
        group.bench_with_input(BenchmarkId::from_parameter(n), &n, |b, &n| {
            b.iter(|| unsafe {
                let m = hew_hashmap_new_with_layout(&raw const kl, &raw const vl);
                for i in 0..n {
                    let v = i.wrapping_mul(2);
                    hew_hashmap_insert_layout(
                        m,
                        (&raw const i).cast::<c_void>(),
                        (&raw const v).cast::<c_void>(),
                    );
                }
                for i in 0..n {
                    let _ = hew_hashmap_remove_layout(m, (&raw const i).cast::<c_void>());
                }
                hew_hashmap_free_layout(m);
            });
        });
    }
    group.finish();
}

fn bench_free_string_kv(c: &mut Criterion) {
    let mut group = c.benchmark_group("free_string_kv");
    let (kl, vl) = string_descriptors();
    for &n in &[1_000usize, 10_000] {
        group.throughput(Throughput::Elements(n as u64));
        group.bench_with_input(BenchmarkId::from_parameter(n), &n, |b, &n| {
            b.iter_batched(
                || unsafe {
                    // Setup outside the timed region: build a fully populated map.
                    let m = hew_hashmap_new_with_layout(&raw const kl, &raw const vl);
                    for (k_ptr, v_ptr) in make_string_pairs(n) {
                        let k_slot: *mut c_char = k_ptr;
                        let v_slot: *mut c_char = v_ptr;
                        hew_hashmap_insert_layout(
                            m,
                            (&raw const k_slot).cast::<c_void>(),
                            (&raw const v_slot).cast::<c_void>(),
                        );
                    }
                    m
                },
                |m| unsafe {
                    // Timed: iteration + drop_fn for every occupied K and V.
                    hew_hashmap_free_layout(m);
                },
                BatchSize::SmallInput,
            );
        });
    }
    group.finish();
}

fn bench_insert_overwrite_string_v(c: &mut Criterion) {
    let mut group = c.benchmark_group("insert_overwrite_string_v");
    let (kl, vl) = string_descriptors();
    let n: usize = 1_000;
    group.throughput(Throughput::Elements(n as u64));
    group.bench_with_input(BenchmarkId::from_parameter(n), &n, |b, &n| {
        b.iter_batched(
            || unsafe {
                // Pre-populate: caller-owned K transferred to kernel; V_old also
                // transferred so the overwrite path has a real owned V to drop.
                // The fresh V_new pointers for the timed region are produced
                // here too so the hot loop is purely insert + drop_old_v.
                let m = hew_hashmap_new_with_layout(&raw const kl, &raw const vl);
                let mut keys_for_replay: Vec<*mut c_char> = Vec::with_capacity(n);
                for (k_ptr, v_ptr) in make_string_pairs(n) {
                    let k_slot: *mut c_char = k_ptr;
                    let v_slot: *mut c_char = v_ptr;
                    hew_hashmap_insert_layout(
                        m,
                        (&raw const k_slot).cast::<c_void>(),
                        (&raw const v_slot).cast::<c_void>(),
                    );
                    // The same K bytes will be re-presented at overwrite time —
                    // build a fresh owned copy (per the C0a contract: kernel
                    // does NOT consume K_in on the occupied-slot path; the
                    // caller is responsible for freeing the duplicate K_in
                    // AFTER the overwrite call returns).
                    let k_replay = CString::new(format!("key-{:010}", keys_for_replay.len()))
                        .unwrap()
                        .into_raw();
                    keys_for_replay.push(k_replay);
                }
                // Fresh V_new strings — one per overwrite — transferred to
                // the kernel by the insert call inside the timed region.
                let v_new: Vec<*mut c_char> = (0..n)
                    .map(|i| CString::new(format!("vnew-{i:010}")).unwrap().into_raw())
                    .collect();
                (m, keys_for_replay, v_new)
            },
            |(m, keys_for_replay, v_new)| unsafe {
                // Timed: N overwrite inserts. Every iteration of the inner
                // loop fires the V drop_fn on the previously-stored V.
                for (k_replay, v_new_ptr) in keys_for_replay.iter().zip(v_new.iter()) {
                    let k_slot: *mut c_char = *k_replay;
                    let v_slot: *mut c_char = *v_new_ptr;
                    let was_new = hew_hashmap_insert_layout(
                        m,
                        (&raw const k_slot).cast::<c_void>(),
                        (&raw const v_slot).cast::<c_void>(),
                    );
                    debug_assert!(!was_new, "expected overwrite path");
                }
                // Post-timed teardown (allocated in setup, freed here to keep
                // the timed region pure): caller-owned K_in duplicates and
                // the kernel-owned map itself.
                for k_replay in keys_for_replay {
                    drop(CString::from_raw(k_replay));
                }
                hew_hashmap_free_layout(m);
            },
            BatchSize::SmallInput,
        );
    });
    group.finish();
}

fn bench_remove_string_kv(c: &mut Criterion) {
    let mut group = c.benchmark_group("remove_string_kv");
    let (kl, vl) = string_descriptors();
    let n: usize = 1_000;
    group.throughput(Throughput::Elements(n as u64));
    group.bench_with_input(BenchmarkId::from_parameter(n), &n, |b, &n| {
        b.iter_batched(
            || unsafe {
                // Pre-populate: kernel takes ownership of all K + V strings.
                // Build a parallel vector of equal-content lookup keys (kernel
                // does NOT consume lookup K on remove — caller still owns it).
                let m = hew_hashmap_new_with_layout(&raw const kl, &raw const vl);
                let mut lookup_keys: Vec<*mut c_char> = Vec::with_capacity(n);
                for (k_ptr, v_ptr) in make_string_pairs(n) {
                    let k_slot: *mut c_char = k_ptr;
                    let v_slot: *mut c_char = v_ptr;
                    hew_hashmap_insert_layout(
                        m,
                        (&raw const k_slot).cast::<c_void>(),
                        (&raw const v_slot).cast::<c_void>(),
                    );
                    let k_lookup = CString::new(format!("key-{:010}", lookup_keys.len()))
                        .unwrap()
                        .into_raw();
                    lookup_keys.push(k_lookup);
                }
                (m, lookup_keys)
            },
            |(m, lookup_keys)| unsafe {
                // Timed: N removes. Each fires both the K and V drop_fn on
                // the stored slot (per-slot remove drop site).
                for k_lookup in &lookup_keys {
                    let k_slot: *const c_char = *k_lookup;
                    let removed =
                        hew_hashmap_remove_layout(m, (&raw const k_slot).cast::<c_void>());
                    debug_assert!(removed, "expected hit on pre-populated key");
                }
                // Post-timed teardown: caller-owned lookup K_in (the kernel
                // never consumed it) + the now-empty map.
                for k_lookup in lookup_keys {
                    drop(CString::from_raw(k_lookup));
                }
                hew_hashmap_free_layout(m);
            },
            BatchSize::SmallInput,
        );
    });
    group.finish();
}

criterion_group!(
    benches,
    bench_insert_string_kv,
    bench_insert_remove_i64,
    bench_free_string_kv,
    bench_insert_overwrite_string_v,
    bench_remove_string_kv,
);
criterion_main!(benches);
