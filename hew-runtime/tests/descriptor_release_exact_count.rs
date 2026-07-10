#![cfg(unix)]

use core::ffi::c_void;
use core::sync::atomic::{AtomicUsize, Ordering};
use std::os::unix::process::ExitStatusExt;
use std::process::Command;

use hew_cabi::map::{HewMapKeyLayout, HewMapValueLayout};
use hew_cabi::vec::{HewTypeOwnershipKind, HewVec, HewVecElemLayout};
use hew_runtime::hashmap::{
    hew_hashmap_free_layout, hew_hashmap_insert_layout, hew_hashmap_new_with_layout,
};
use hew_runtime::vec::{hew_vec_free_owned, hew_vec_new_with_elem_layout, hew_vec_push_owned_move};

static RECORD_DROPS: AtomicUsize = AtomicUsize::new(0);
static RECORD_EXPECTED: AtomicUsize = AtomicUsize::new(usize::MAX);
static ENUM_DROPS: AtomicUsize = AtomicUsize::new(0);
static ENUM_EXPECTED: AtomicUsize = AtomicUsize::new(usize::MAX);
static TUPLE_DROPS: AtomicUsize = AtomicUsize::new(0);
static TUPLE_EXPECTED: AtomicUsize = AtomicUsize::new(usize::MAX);
static MAP_VALUE_DROPS: AtomicUsize = AtomicUsize::new(0);
static MAP_VALUE_EXPECTED: AtomicUsize = AtomicUsize::new(usize::MAX);
static MAP_INNER_DROPS: AtomicUsize = AtomicUsize::new(0);
static MAP_INNER_EXPECTED: AtomicUsize = AtomicUsize::new(usize::MAX);

#[repr(C)]
#[derive(Clone, Copy)]
struct RecordOwningHeap {
    heap: *mut u64,
}

#[repr(C)]
#[derive(Clone, Copy)]
struct EnumOwningHeap {
    tag: u64,
    heap: *mut u64,
}

#[repr(C)]
#[derive(Clone, Copy)]
struct TupleCarryingRecord {
    prefix: i64,
    record: RecordOwningHeap,
}

fn count_or_abort(counter: &AtomicUsize, expected: &AtomicUsize) {
    let observed = counter.fetch_add(1, Ordering::SeqCst) + 1;
    if observed > expected.load(Ordering::SeqCst) {
        std::process::abort();
    }
}

unsafe extern "C" fn drop_record(slot: *mut c_void) {
    count_or_abort(&RECORD_DROPS, &RECORD_EXPECTED);
    // SAFETY: the descriptor is used only with `RecordOwningHeap` slots.
    let value = unsafe { &mut *slot.cast::<RecordOwningHeap>() };
    // SAFETY: every test value contains one uniquely owned Box allocation.
    unsafe { drop(Box::from_raw(value.heap)) };
    value.heap = core::ptr::null_mut();
}

unsafe extern "C" fn drop_enum(slot: *mut c_void) {
    count_or_abort(&ENUM_DROPS, &ENUM_EXPECTED);
    // SAFETY: the descriptor is used only with `EnumOwningHeap` slots.
    let value = unsafe { &mut *slot.cast::<EnumOwningHeap>() };
    if value.tag == 1 {
        // SAFETY: tag 1 carries one uniquely owned Box allocation.
        unsafe { drop(Box::from_raw(value.heap)) };
        value.heap = core::ptr::null_mut();
    }
}

unsafe extern "C" fn drop_tuple(slot: *mut c_void) {
    count_or_abort(&TUPLE_DROPS, &TUPLE_EXPECTED);
    // SAFETY: the descriptor is used only with `TupleCarryingRecord` slots.
    let value = unsafe { &mut *slot.cast::<TupleCarryingRecord>() };
    // SAFETY: every test value contains one uniquely owned Box allocation.
    unsafe { drop(Box::from_raw(value.record.heap)) };
    value.record.heap = core::ptr::null_mut();
}

extern "C" fn drop_map_vec_value(slot: *mut c_void) {
    count_or_abort(&MAP_VALUE_DROPS, &MAP_VALUE_EXPECTED);
    // SAFETY: the map value blob stores one owned HewVec pointer.
    unsafe {
        let vec = *slot.cast::<*mut HewVec>();
        hew_vec_free_owned(vec);
    }
}

unsafe extern "C" fn drop_map_inner_record(slot: *mut c_void) {
    count_or_abort(&MAP_INNER_DROPS, &MAP_INNER_EXPECTED);
    // SAFETY: the descriptor is used only with `RecordOwningHeap` slots.
    let value = unsafe { &mut *slot.cast::<RecordOwningHeap>() };
    // SAFETY: every test value contains one uniquely owned Box allocation.
    unsafe { drop(Box::from_raw(value.heap)) };
    value.heap = core::ptr::null_mut();
}

unsafe extern "C" fn hash_i64(key: *const c_void) -> u64 {
    // SAFETY: map tests pass an i64 key blob.
    unsafe { (*key.cast::<i64>()).cast_unsigned() }
}

unsafe extern "C" fn eq_i64(lhs: *const c_void, rhs: *const c_void) -> i32 {
    // SAFETY: map tests pass i64 key blobs.
    unsafe { i32::from(*lhs.cast::<i64>() == *rhs.cast::<i64>()) }
}

fn layout<T>(drop_fn: unsafe extern "C" fn(*mut c_void)) -> HewVecElemLayout {
    HewVecElemLayout {
        size: size_of::<T>(),
        align: align_of::<T>(),
        ownership_kind: HewTypeOwnershipKind::LayoutManaged,
        clone_fn: None,
        drop_fn: Some(drop_fn),
    }
}

unsafe fn make_vec<T>(layout: &HewVecElemLayout, values: &[T]) -> *mut HewVec {
    // SAFETY: the descriptor matches T and every source lives through the copy.
    let vec = unsafe { hew_vec_new_with_elem_layout(layout) };
    for value in values {
        // SAFETY: push-by-move copies exactly one T into the matching Vec layout.
        unsafe { hew_vec_push_owned_move(vec, (value as *const T).cast()) };
    }
    vec
}

fn reset(counter: &AtomicUsize, expected: &AtomicUsize, count: usize) {
    counter.store(0, Ordering::SeqCst);
    expected.store(count, Ordering::SeqCst);
}

fn assert_detector_rejects(counter: &AtomicUsize, expected: usize, shape: &str) {
    let observed = counter.load(Ordering::SeqCst);
    assert!(
        std::panic::catch_unwind(|| assert_eq!(observed, expected, "{shape}")).is_err(),
        "{shape}: exact-count oracle accepted suppressed recursive release"
    );
}

#[test]
fn descriptor_release_exact_counts_cover_issue_2553_shapes() {
    reset(&RECORD_DROPS, &RECORD_EXPECTED, 3);
    let record_layout = layout::<RecordOwningHeap>(drop_record);
    let records = [
        RecordOwningHeap {
            heap: Box::into_raw(Box::new(1)),
        },
        RecordOwningHeap {
            heap: Box::into_raw(Box::new(2)),
        },
        RecordOwningHeap {
            heap: Box::into_raw(Box::new(3)),
        },
    ];
    // SAFETY: values and descriptor agree and ownership moves into the Vec.
    unsafe { hew_vec_free_owned(make_vec(&record_layout, &records)) };
    assert_eq!(RECORD_DROPS.load(Ordering::SeqCst), 3);

    reset(&ENUM_DROPS, &ENUM_EXPECTED, 3);
    let enum_layout = layout::<EnumOwningHeap>(drop_enum);
    let enums = [
        EnumOwningHeap {
            tag: 1,
            heap: Box::into_raw(Box::new(4)),
        },
        EnumOwningHeap {
            tag: 0,
            heap: core::ptr::null_mut(),
        },
        EnumOwningHeap {
            tag: 1,
            heap: Box::into_raw(Box::new(5)),
        },
    ];
    // SAFETY: values and descriptor agree and ownership moves into the Vec.
    unsafe { hew_vec_free_owned(make_vec(&enum_layout, &enums)) };
    assert_eq!(ENUM_DROPS.load(Ordering::SeqCst), 3);

    reset(&TUPLE_DROPS, &TUPLE_EXPECTED, 3);
    let tuple_layout = layout::<TupleCarryingRecord>(drop_tuple);
    let tuples = [
        TupleCarryingRecord {
            prefix: 6,
            record: RecordOwningHeap {
                heap: Box::into_raw(Box::new(7)),
            },
        },
        TupleCarryingRecord {
            prefix: 8,
            record: RecordOwningHeap {
                heap: Box::into_raw(Box::new(9)),
            },
        },
        TupleCarryingRecord {
            prefix: 10,
            record: RecordOwningHeap {
                heap: Box::into_raw(Box::new(11)),
            },
        },
    ];
    // SAFETY: values and descriptor agree and ownership moves into the Vec.
    unsafe { hew_vec_free_owned(make_vec(&tuple_layout, &tuples)) };
    assert_eq!(TUPLE_DROPS.load(Ordering::SeqCst), 3);

    reset(&MAP_VALUE_DROPS, &MAP_VALUE_EXPECTED, 2);
    reset(&MAP_INNER_DROPS, &MAP_INNER_EXPECTED, 4);
    let inner_layout = layout::<RecordOwningHeap>(drop_map_inner_record);
    let key_layout = HewMapKeyLayout {
        size: size_of::<i64>(),
        align: align_of::<i64>(),
        ownership_kind: HewTypeOwnershipKind::Plain,
        hash_fn: Some(hash_i64),
        eq_fn: Some(eq_i64),
        drop_fn: None,
    };
    let value_layout = HewMapValueLayout {
        size: size_of::<*mut HewVec>(),
        align: align_of::<*mut HewVec>(),
        ownership_kind: HewTypeOwnershipKind::LayoutManaged,
        drop_fn: Some(drop_map_vec_value),
        clone_fn: None,
    };
    // SAFETY: every descriptor matches the inserted key/value blobs.
    unsafe {
        let map = hew_hashmap_new_with_layout(&raw const key_layout, &raw const value_layout);
        for key in 0..2i64 {
            let inner = [
                RecordOwningHeap {
                    heap: Box::into_raw(Box::new(key.cast_unsigned())),
                },
                RecordOwningHeap {
                    heap: Box::into_raw(Box::new(key.cast_unsigned() + 10)),
                },
            ];
            let vec = make_vec(&inner_layout, &inner);
            hew_hashmap_insert_layout(map, (&raw const key).cast(), (&raw const vec).cast());
        }
        hew_hashmap_free_layout(map);
    }
    assert_eq!(MAP_VALUE_DROPS.load(Ordering::SeqCst), 2);
    assert_eq!(MAP_INNER_DROPS.load(Ordering::SeqCst), 4);
}

#[test]
fn suppressed_release_fails_exact_count_for_each_issue_2553_shape() {
    reset(&RECORD_DROPS, &RECORD_EXPECTED, 1);
    let record_heap = Box::into_raw(Box::new(1));
    let record = [RecordOwningHeap { heap: record_heap }];
    let record_layout = layout::<RecordOwningHeap>(drop_record);
    // SAFETY: this deliberately seeds a shallow release by hiding the live slot.
    unsafe {
        let vec = make_vec(&record_layout, &record);
        (*vec).len = 0;
        hew_vec_free_owned(vec);
    }
    assert_detector_rejects(&RECORD_DROPS, 1, "Vec<record-owning-heap>");
    // SAFETY: the deliberately suppressed release left this Box live.
    unsafe { drop(Box::from_raw(record_heap)) };

    reset(&ENUM_DROPS, &ENUM_EXPECTED, 1);
    let enum_heap = Box::into_raw(Box::new(2));
    let value = [EnumOwningHeap {
        tag: 1,
        heap: enum_heap,
    }];
    let enum_layout = layout::<EnumOwningHeap>(drop_enum);
    // SAFETY: this deliberately seeds a shallow release by hiding the live slot.
    unsafe {
        let vec = make_vec(&enum_layout, &value);
        (*vec).len = 0;
        hew_vec_free_owned(vec);
    }
    assert_detector_rejects(&ENUM_DROPS, 1, "Vec<enum-owning-heap>");
    // SAFETY: the deliberately suppressed release left this Box live.
    unsafe { drop(Box::from_raw(enum_heap)) };

    reset(&TUPLE_DROPS, &TUPLE_EXPECTED, 1);
    let tuple_heap = Box::into_raw(Box::new(3));
    let value = [TupleCarryingRecord {
        prefix: 4,
        record: RecordOwningHeap { heap: tuple_heap },
    }];
    let tuple_layout = layout::<TupleCarryingRecord>(drop_tuple);
    // SAFETY: this deliberately seeds a shallow release by hiding the live slot.
    unsafe {
        let vec = make_vec(&tuple_layout, &value);
        (*vec).len = 0;
        hew_vec_free_owned(vec);
    }
    assert_detector_rejects(&TUPLE_DROPS, 1, "Vec<tuple-carrying-record>");
    // SAFETY: the deliberately suppressed release left this Box live.
    unsafe { drop(Box::from_raw(tuple_heap)) };

    reset(&MAP_VALUE_DROPS, &MAP_VALUE_EXPECTED, 1);
    reset(&MAP_INNER_DROPS, &MAP_INNER_EXPECTED, 1);
    let inner_layout = layout::<RecordOwningHeap>(drop_map_inner_record);
    let inner = [RecordOwningHeap {
        heap: Box::into_raw(Box::new(5)),
    }];
    // SAFETY: descriptor and element agree.
    let vec = unsafe { make_vec(&inner_layout, &inner) };
    let key_layout = HewMapKeyLayout {
        size: size_of::<i64>(),
        align: align_of::<i64>(),
        ownership_kind: HewTypeOwnershipKind::Plain,
        hash_fn: Some(hash_i64),
        eq_fn: Some(eq_i64),
        drop_fn: None,
    };
    let shallow_value_layout = HewMapValueLayout {
        size: size_of::<*mut HewVec>(),
        align: align_of::<*mut HewVec>(),
        ownership_kind: HewTypeOwnershipKind::Plain,
        drop_fn: None,
        clone_fn: None,
    };
    let key = 1i64;
    // SAFETY: this deliberately seeds a shallow map-value release.
    unsafe {
        let map =
            hew_hashmap_new_with_layout(&raw const key_layout, &raw const shallow_value_layout);
        hew_hashmap_insert_layout(map, (&raw const key).cast(), (&raw const vec).cast());
        hew_hashmap_free_layout(map);
    }
    assert_detector_rejects(&MAP_VALUE_DROPS, 1, "HashMap<K, Vec<record-owning-heap>>");
    // SAFETY: the shallow map release left the inner Vec live.
    unsafe { hew_vec_free_owned(vec) };
}

#[test]
fn injected_extra_release_aborts_for_each_issue_2553_shape() {
    let current_exe = std::env::current_exe().expect("current test executable");
    for shape in ["record", "enum", "tuple", "hashmap_vec"] {
        let output = Command::new(&current_exe)
            .args(["--exact", "injected_extra_release_helper", "--nocapture"])
            .env("HEW_OWN_R_EXTRA_RELEASE_SHAPE", shape)
            .output()
            .unwrap_or_else(|error| panic!("run extra-release helper for {shape}: {error}"));
        assert_eq!(
            output.status.signal(),
            Some(libc::SIGABRT),
            "{shape}: injected extra release must abort loudly; status={:?}\nstdout:\n{}\nstderr:\n{}",
            output.status,
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
    }
}

#[test]
fn injected_extra_release_helper() {
    let Ok(shape) = std::env::var("HEW_OWN_R_EXTRA_RELEASE_SHAPE") else {
        return;
    };
    match shape.as_str() {
        "record" => {
            reset(&RECORD_DROPS, &RECORD_EXPECTED, 1);
            let value = [RecordOwningHeap {
                heap: Box::into_raw(Box::new(1)),
            }];
            let layout = layout::<RecordOwningHeap>(drop_record);
            // SAFETY: the first explicit drop is the injected extra release.
            unsafe {
                let vec = make_vec(&layout, &value);
                drop_record((*vec).data.cast());
                hew_vec_free_owned(vec);
            }
        }
        "enum" => {
            reset(&ENUM_DROPS, &ENUM_EXPECTED, 1);
            let value = [EnumOwningHeap {
                tag: 1,
                heap: Box::into_raw(Box::new(2)),
            }];
            let layout = layout::<EnumOwningHeap>(drop_enum);
            // SAFETY: the first explicit drop is the injected extra release.
            unsafe {
                let vec = make_vec(&layout, &value);
                drop_enum((*vec).data.cast());
                hew_vec_free_owned(vec);
            }
        }
        "tuple" => {
            reset(&TUPLE_DROPS, &TUPLE_EXPECTED, 1);
            let value = [TupleCarryingRecord {
                prefix: 3,
                record: RecordOwningHeap {
                    heap: Box::into_raw(Box::new(4)),
                },
            }];
            let layout = layout::<TupleCarryingRecord>(drop_tuple);
            // SAFETY: the first explicit drop is the injected extra release.
            unsafe {
                let vec = make_vec(&layout, &value);
                drop_tuple((*vec).data.cast());
                hew_vec_free_owned(vec);
            }
        }
        "hashmap_vec" => {
            reset(&MAP_VALUE_DROPS, &MAP_VALUE_EXPECTED, 1);
            reset(&MAP_INNER_DROPS, &MAP_INNER_EXPECTED, 1);
            let inner_layout = layout::<RecordOwningHeap>(drop_map_inner_record);
            let inner = [RecordOwningHeap {
                heap: Box::into_raw(Box::new(5)),
            }];
            // SAFETY: descriptor and value agree.
            let vec = unsafe { make_vec(&inner_layout, &inner) };
            let key_layout = HewMapKeyLayout {
                size: size_of::<i64>(),
                align: align_of::<i64>(),
                ownership_kind: HewTypeOwnershipKind::Plain,
                hash_fn: Some(hash_i64),
                eq_fn: Some(eq_i64),
                drop_fn: None,
            };
            let value_layout = HewMapValueLayout {
                size: size_of::<*mut HewVec>(),
                align: align_of::<*mut HewVec>(),
                ownership_kind: HewTypeOwnershipKind::LayoutManaged,
                drop_fn: Some(drop_map_vec_value),
                clone_fn: None,
            };
            let key = 1i64;
            // SAFETY: the explicit value drop is the injected extra release.
            unsafe {
                let map =
                    hew_hashmap_new_with_layout(&raw const key_layout, &raw const value_layout);
                hew_hashmap_insert_layout(map, (&raw const key).cast(), (&raw const vec).cast());
                drop_map_vec_value((&raw const vec).cast_mut().cast());
                hew_hashmap_free_layout(map);
            }
        }
        other => panic!("unknown injected extra-release shape: {other}"),
    }
    panic!("{shape}: injected extra release did not abort");
}
