//! Borrowed collection probing. Hashing and equality are requests to the
//! caller, so a callback can suspend without retaining a Rust stack or holding
//! a scheduler worker. Only commit changes the map's ownership.

use super::{
    abort_layout_clone, alloc_layout_entries, clone_layout_blob, dealloc_layout_entries, key_eq,
    key_hash, require_clone, slot_key, slot_state, slot_val, validate_op_inputs, HewLayoutHashMap,
    EMPTY, LOAD_PCTG, OCCUPIED, TOMBSTONE,
};
use crate::release_walker::{hew_release_sync, HewReleaseCursor};
use core::{ffi::c_void, ptr};
use hew_cabi::map::HewMapProbeStatus;

#[derive(Debug, Clone, Copy)]
enum Phase {
    Rehash,
    Hash { old_slot: Option<usize> },
    Scan,
    Equal,
    Ready,
}

/// An unfinished operation borrows the map and query. A resize buffer contains
/// borrowed copies of old slots, and freeing it never invokes value cleanup.
/// No request owns a producer thread or a callback: the caller must finish or
/// drain its active callback before freeing these borrowed operands.
#[derive(Debug)]
pub struct HewLayoutMapProbe {
    map: *mut HewLayoutHashMap,
    key: *const c_void,
    entries: *mut u8,
    cap: usize,
    staged: bool,
    rehash_next: usize,
    phase: Phase,
    start: usize,
    index: usize,
    tombstone: Option<usize>,
    found: bool,
}

impl Drop for HewLayoutMapProbe {
    fn drop(&mut self) {
        if self.staged {
            // SAFETY: the live borrowed map supplies the allocation geometry;
            // staged bytes still borrow their owners from its unchanged table.
            unsafe {
                let map = &*self.map;
                dealloc_layout_entries(
                    self.entries,
                    self.cap,
                    map.stride,
                    map.key_layout.value.align.max(map.val_layout.align),
                );
            }
        }
    }
}

impl HewLayoutMapProbe {
    unsafe fn advance(&mut self) -> HewMapProbeStatus {
        // SAFETY: every probe entry point retains the same exclusive map loan;
        // no mutable Rust reference crosses a callback or allocation.
        unsafe {
            loop {
                match self.phase {
                    Phase::Rehash => {
                        while self.rehash_next < (*self.map).cap {
                            let index = self.rehash_next;
                            self.rehash_next += 1;
                            if *slot_state((*self.map).entries, index, (*self.map).stride)
                                == OCCUPIED
                            {
                                self.phase = Phase::Hash {
                                    old_slot: Some(index),
                                };
                                return HewMapProbeStatus::NeedHash;
                            }
                        }
                        self.phase = Phase::Hash { old_slot: None };
                    }
                    Phase::Hash { .. } => return HewMapProbeStatus::NeedHash,
                    Phase::Equal => return HewMapProbeStatus::NeedEq,
                    Phase::Ready => return HewMapProbeStatus::Ready,
                    Phase::Scan => {
                        match *slot_state(self.entries, self.index, (*self.map).stride) {
                            EMPTY => {
                                self.index = self.tombstone.unwrap_or(self.index);
                                self.phase = Phase::Ready;
                            }
                            OCCUPIED => self.phase = Phase::Equal,
                            TOMBSTONE => {
                                self.tombstone.get_or_insert(self.index);
                                self.next_slot();
                            }
                            _ => unreachable!("map probe encountered an invalid slot state"),
                        }
                    }
                }
            }
        }
    }

    fn next_slot(&mut self) {
        self.index = (self.index + 1) & (self.cap - 1);
        if self.index == self.start {
            self.index = self
                .tombstone
                .expect("a full probe must contain a tombstone");
            self.phase = Phase::Ready;
        } else {
            self.phase = Phase::Scan;
        }
    }

    unsafe fn accept_hash(&mut self, hash: u64) {
        let Phase::Hash { old_slot } = self.phase else {
            panic!("map probe did not request a hash");
        };
        #[allow(
            clippy::cast_possible_truncation,
            reason = "hash bits are masked to table capacity"
        )]
        let index = (hash as usize) & (self.cap - 1);
        if let Some(old_slot) = old_slot {
            // SAFETY: rehashing only copies borrowed slot bits into fresh,
            // larger storage. The old map still owns every copied value.
            unsafe {
                let map = &*self.map;
                let mut destination = index;
                while *slot_state(self.entries, destination, map.stride) != EMPTY {
                    destination = (destination + 1) & (self.cap - 1);
                }
                ptr::copy_nonoverlapping(
                    slot_key(map.entries, old_slot, map.stride, map.key_offset),
                    slot_key(self.entries, destination, map.stride, map.key_offset),
                    map.key_layout.value.size,
                );
                if map.val_layout.size > 0 {
                    ptr::copy_nonoverlapping(
                        slot_val(map.entries, old_slot, map.stride, map.val_offset),
                        slot_val(self.entries, destination, map.stride, map.val_offset),
                        map.val_layout.size,
                    );
                }
                *slot_state(self.entries, destination, map.stride) = OCCUPIED;
            }
            self.phase = Phase::Rehash;
        } else {
            self.index = index;
            self.start = index;
            self.phase = Phase::Scan;
        }
    }
}

/// Begin a lookup or insertion without invoking a user callback. An insertion
/// stages any required resize; all original owners remain in the old map.
///
/// # Panics
/// Panics if doubling the table capacity overflows.
///
/// # Safety
/// Map and key are live, match the map's descriptors, and remain exclusively
/// borrowed until commit/free. Drain any pending callback before ending that
/// loan. `inserting` is zero for lookup/removal and nonzero for insertion.
#[no_mangle]
pub unsafe extern "C" fn hew_hashmap_probe_begin(
    map: *mut HewLayoutHashMap,
    key: *const c_void,
    inserting: i32,
) -> *mut HewLayoutMapProbe {
    // SAFETY: forwarded input contract; the gate validates pointers/descriptors.
    unsafe { validate_op_inputs(map, key, None) };
    // SAFETY: the gate established a live map. Copy geometry before allocating.
    let (old_entries, old_cap, len, stride, alignment) = unsafe {
        (
            (*map).entries,
            (*map).cap,
            (*map).len,
            (*map).stride,
            (*map).key_layout.value.align.max((*map).val_layout.align),
        )
    };
    let staged = inserting != 0
        && len.saturating_add(1).saturating_mul(100) >= old_cap.saturating_mul(LOAD_PCTG);
    let cap = if staged {
        old_cap.checked_mul(2).expect("map capacity overflow")
    } else {
        old_cap
    };
    let entries = if staged {
        // SAFETY: the map constructor validated this allocation geometry.
        unsafe { alloc_layout_entries(cap, stride, alignment) }
    } else {
        old_entries
    };
    let phase = if staged {
        Phase::Rehash
    } else if inserting == 0 && len == 0 {
        Phase::Ready
    } else {
        Phase::Hash { old_slot: None }
    };
    Box::into_raw(Box::new(HewLayoutMapProbe {
        map,
        key,
        entries,
        cap,
        staged,
        rehash_next: 0,
        phase,
        start: 0,
        index: 0,
        tombstone: None,
        found: false,
    }))
}

/// # Safety
/// `probe` is live and uniquely driven; its borrowed operands remain live.
#[no_mangle]
pub unsafe extern "C" fn hew_hashmap_probe_step(probe: *mut HewLayoutMapProbe) -> i32 {
    // SAFETY: forwarded unique probe contract.
    unsafe { (*probe).advance() as i32 }
}

/// # Panics
/// Panics if the probe has no pending callback request.
///
/// # Safety
/// The last step requested Hash or Eq. The returned key is borrowed until the
/// matching result is submitted or the probe is freed after callback drain.
#[no_mangle]
pub unsafe extern "C" fn hew_hashmap_probe_left(probe: *const HewLayoutMapProbe) -> *const c_void {
    // SAFETY: the probe and the selected map slot are borrowed and live.
    unsafe {
        let probe = &*probe;
        let map = &*probe.map;
        match probe.phase {
            Phase::Hash {
                old_slot: Some(index),
            } => slot_key(map.entries, index, map.stride, map.key_offset).cast(),
            Phase::Hash { old_slot: None } => probe.key,
            Phase::Equal => slot_key(probe.entries, probe.index, map.stride, map.key_offset).cast(),
            _ => panic!("map probe has no callback operand"),
        }
    }
}

/// # Safety
/// The last step requested Eq and the probe's key loan is still live.
#[no_mangle]
pub unsafe extern "C" fn hew_hashmap_probe_right(probe: *const HewLayoutMapProbe) -> *const c_void {
    // SAFETY: caller retains the query through this borrowed access.
    unsafe { (*probe).key }
}

/// # Safety
/// Submit exactly one result for the last `NeedHash` request; the callback has
/// completed and no longer accesses its borrowed operands.
#[no_mangle]
pub unsafe extern "C" fn hew_hashmap_probe_submit_hash(probe: *mut HewLayoutMapProbe, hash: u64) {
    // SAFETY: forwarded exclusive probe and completed callback contract.
    unsafe { (*probe).accept_hash(hash) };
}

/// # Panics
/// Panics if the probe did not request equality.
///
/// # Safety
/// Submit exactly one result for the last `NeedEq` request after its callback
/// has completed and released its operand loans.
#[no_mangle]
pub unsafe extern "C" fn hew_hashmap_probe_submit_eq(probe: *mut HewLayoutMapProbe, equal: bool) {
    // SAFETY: caller uniquely drives this live probe.
    unsafe {
        let probe = &mut *probe;
        assert!(
            matches!(probe.phase, Phase::Equal),
            "map probe did not request equality"
        );
        if equal {
            probe.found = true;
            probe.phase = Phase::Ready;
        } else {
            probe.next_slot();
        }
    }
}

/// Abandon an uncommitted probe without changing its map or releasing values.
///
/// # Safety
/// `probe` is null or uniquely owned. Its active callback, if any, has finished
/// or been cancelled and drained before the receiver/key loan ends.
#[no_mangle]
pub unsafe extern "C" fn hew_hashmap_probe_free(probe: *mut HewLayoutMapProbe) {
    if !probe.is_null() {
        // SAFETY: the caller transfers the unique probe allocation.
        drop(unsafe { Box::from_raw(probe) });
    }
}

unsafe fn ready(probe: *mut HewLayoutMapProbe) -> HewLayoutMapProbe {
    // SAFETY: a commit transfers the unique live probe allocation.
    let probe = unsafe { *Box::from_raw(probe) };
    assert!(
        matches!(probe.phase, Phase::Ready),
        "map probe is not ready to commit"
    );
    probe
}

impl HewLayoutMapProbe {
    unsafe fn value(&self) -> *mut u8 {
        // SAFETY: a found ready probe names an occupied slot in this live map.
        unsafe {
            slot_val(
                self.entries,
                self.index,
                (*self.map).stride,
                (*self.map).val_offset,
            )
        }
    }

    unsafe fn publish_resize(&mut self) {
        if self.staged {
            // SAFETY: staged bits become the sole owners when their old slot
            // storage is freed; neither allocation runs any value destructor.
            unsafe {
                let map = &mut *self.map;
                dealloc_layout_entries(
                    map.entries,
                    map.cap,
                    map.stride,
                    map.key_layout.value.align.max(map.val_layout.align),
                );
                map.entries = self.entries;
                map.cap = self.cap;
            }
            self.staged = false;
        }
    }
}

/// # Safety
/// Consume one ready lookup probe whose receiver remains borrowed. No output
/// owner is acquired, and no user callback is invoked.
#[no_mangle]
pub unsafe extern "C" fn hew_hashmap_probe_contains(probe: *mut HewLayoutMapProbe) -> bool {
    // SAFETY: forwarded ready probe ownership.
    unsafe { ready(probe).found }
}

/// # Safety
/// Consume a ready lookup probe. If present, `out` is aligned writable storage
/// for the map's value, disjoint from the map and key, and acquires one clone.
#[no_mangle]
pub unsafe extern "C" fn hew_hashmap_probe_get_clone(
    probe: *mut HewLayoutMapProbe,
    out: *mut c_void,
) -> bool {
    // SAFETY: the caller transfers the ready probe and its live borrowed map.
    unsafe {
        let probe = ready(probe);
        if probe.found {
            clone_layout_blob(
                (*probe.map).val_layout,
                probe.value(),
                out.cast(),
                "map lookup value",
            );
        }
        probe.found
    }
}

/// # Safety
/// Consume a ready lookup probe. If present, copy the value's borrowed bits
/// into aligned `out`; its loan must end before any map mutation or release.
#[no_mangle]
pub unsafe extern "C" fn hew_hashmap_probe_get_borrow(
    probe: *mut HewLayoutMapProbe,
    out: *mut c_void,
) -> bool {
    // SAFETY: output borrows the same live value and acquires no owner.
    unsafe {
        let probe = ready(probe);
        if probe.found && (*probe.map).val_layout.size > 0 {
            ptr::copy_nonoverlapping(probe.value(), out.cast(), (*probe.map).val_layout.size);
        }
        probe.found
    }
}

/// # Safety
/// Consume a ready removal probe. A found value moves into aligned writable
/// `out`; the stored key is released. Caller captures any release fault.
#[no_mangle]
pub unsafe extern "C-unwind" fn hew_hashmap_probe_remove_take(
    probe: *mut HewLayoutMapProbe,
    out: *mut c_void,
    release_out: *mut *mut HewReleaseCursor,
) -> bool {
    // SAFETY: forwarded ready probe, live receiver and disjoint output contract.
    unsafe { remove(&ready(probe), Some(out), release_out) }
}

unsafe fn remove(
    probe: &HewLayoutMapProbe,
    output: Option<*mut c_void>,
    release_out: *mut *mut HewReleaseCursor,
) -> bool {
    // SAFETY: the caller provides a disjoint writable cursor output.
    unsafe { *release_out = ptr::null_mut() };
    if !probe.found {
        return false;
    }
    // SAFETY: the ready probe identifies one occupied entry in the exclusively
    // borrowed map. It changes ownership without an intervening callback request.
    unsafe {
        let map = &mut *probe.map;
        let key = slot_key(map.entries, probe.index, map.stride, map.key_offset);
        let value = slot_val(map.entries, probe.index, map.stride, map.val_offset);
        let mut displaced = vec![HewReleaseCursor::detached(key.cast(), map.key_layout.value)];
        if let Some(output) = output {
            if map.val_layout.size > 0 {
                ptr::copy_nonoverlapping(value, output.cast(), map.val_layout.size);
            }
        } else {
            displaced.push(HewReleaseCursor::detached(value.cast(), map.val_layout));
        }
        *slot_state(map.entries, probe.index, map.stride) = TOMBSTONE;
        map.len -= 1;
        *release_out = HewReleaseCursor::join(displaced);
    }
    true
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum InsertMode {
    Clone,
    TakeValue,
    TakeElement,
    RawTransfer,
}

unsafe fn insert(
    mut probe: HewLayoutMapProbe,
    value: *const c_void,
    mode: InsertMode,
    release_out: *mut *mut HewReleaseCursor,
) -> bool {
    // SAFETY: snapshot geometry before allocating or changing the map.
    let (key_layout, value_layout, stride, key_offset, val_offset) = unsafe {
        (
            (*probe.map).key_layout.value,
            (*probe.map).val_layout,
            (*probe.map).stride,
            (*probe.map).key_offset,
            (*probe.map).val_offset,
        )
    };
    let alignment = key_layout.align.max(value_layout.align);
    if mode == InsertMode::Clone && value.is_null() && value_layout.clone_fn.is_some() {
        abort_layout_clone("map insert value: clone callback requires a non-null input slot");
    }
    // Stage inputs before publishing a resize: copying ingress may borrow the
    // old map's own slots, which cease to exist when storage is committed.
    // SAFETY: map construction verified the allocation and field geometry.
    let scratch = unsafe { alloc_layout_entries(1, stride, alignment) };
    // SAFETY: scratch contains one aligned map slot; the input contracts name
    // matching initialized key/value blobs. Raw transfers still borrow the
    // caller's owner until all slot updates commit below.
    unsafe {
        let key = scratch.add(key_offset);
        let val = scratch.add(val_offset);
        if matches!(mode, InsertMode::Clone | InsertMode::TakeValue) {
            require_clone(&key_layout, "map insert key");
            clone_layout_blob(key_layout, probe.key.cast(), key, "map insert key");
        } else if key_layout.size > 0 {
            ptr::copy_nonoverlapping(probe.key.cast::<u8>(), key, key_layout.size);
        }
        if mode == InsertMode::Clone {
            require_clone(&value_layout, "map insert value");
            clone_layout_blob(value_layout, value.cast(), val, "map insert value");
        } else if value_layout.size > 0 {
            ptr::copy_nonoverlapping(value.cast::<u8>(), val, value_layout.size);
        }
        probe.publish_resize();
        let map = &mut *probe.map;
        let destination_key = slot_key(map.entries, probe.index, stride, key_offset);
        let destination_value = slot_val(map.entries, probe.index, stride, val_offset);
        let mut displaced = Vec::new();
        if probe.found {
            displaced.push(HewReleaseCursor::detached(
                destination_value.cast(),
                value_layout,
            ));
        } else {
            ptr::copy_nonoverlapping(key, destination_key, key_layout.size);
            *slot_state(map.entries, probe.index, stride) = OCCUPIED;
            map.len += 1;
        }
        if value_layout.size > 0 {
            ptr::copy_nonoverlapping(val, destination_value, value_layout.size);
        }
        if probe.found && mode != InsertMode::RawTransfer {
            displaced.push(HewReleaseCursor::detached(key.cast(), key_layout));
        }
        dealloc_layout_entries(scratch, 1, stride, alignment);
        *release_out = HewReleaseCursor::join(displaced);
    }
    !probe.found
}

/// # Safety
/// Consume a ready insertion probe and copy its borrowed key/value into the
/// map. `value` matches the map descriptor. Caller captures release faults.
#[no_mangle]
pub unsafe extern "C-unwind" fn hew_hashmap_probe_insert_clone(
    probe: *mut HewLayoutMapProbe,
    value: *const c_void,
    release_out: *mut *mut HewReleaseCursor,
) -> bool {
    // SAFETY: forwarded live insertion operands and ready probe ownership.
    unsafe { insert(ready(probe), value, InsertMode::Clone, release_out) }
}

/// # Safety
/// Consume a ready insertion probe, copy its borrowed key and transfer one
/// independent value owner into the map. Caller captures release faults.
#[no_mangle]
pub unsafe extern "C-unwind" fn hew_hashmap_probe_insert_take(
    probe: *mut HewLayoutMapProbe,
    value: *const c_void,
    release_out: *mut *mut HewReleaseCursor,
) -> bool {
    // SAFETY: forwarded key loan, value transfer and ready probe ownership.
    unsafe { insert(ready(probe), value, InsertMode::TakeValue, release_out) }
}

/// # Safety
/// Consume a ready set insertion probe. Copy its borrowed element into the set
/// when new. A duplicate copy is released; caller captures release faults.
#[no_mangle]
pub unsafe extern "C-unwind" fn hew_hashset_probe_insert_clone(
    probe: *mut HewLayoutMapProbe,
    release_out: *mut *mut HewReleaseCursor,
) -> bool {
    // SAFETY: a set probe's value descriptor is the plain zero-sized marker.
    unsafe { insert(ready(probe), ptr::null(), InsertMode::Clone, release_out) }
}

/// # Safety
/// Consume a ready set insertion probe and its independent element owner. The
/// set takes it when new and releases it when duplicate. Capture release faults.
#[no_mangle]
pub unsafe extern "C-unwind" fn hew_hashset_probe_insert_take(
    probe: *mut HewLayoutMapProbe,
    release_out: *mut *mut HewReleaseCursor,
) -> bool {
    // SAFETY: the caller transfers the query owner; a set's value is plain ZST.
    unsafe {
        insert(
            ready(probe),
            ptr::null(),
            InsertMode::TakeElement,
            release_out,
        )
    }
}

/// # Safety
/// Consume a ready set removal probe, releasing a found element. The query is
/// still borrowed and caller-owned; capture any element release fault.
#[no_mangle]
pub unsafe extern "C-unwind" fn hew_hashset_probe_remove(
    probe: *mut HewLayoutMapProbe,
    release_out: *mut *mut HewReleaseCursor,
) -> bool {
    // SAFETY: the set probe borrows its wrapped map, whose value is plain ZST.
    unsafe { remove(&ready(probe), None, release_out) }
}

/// The synchronous C ABI uses the same state machine. Generated Hew code
/// drives requests through its own checked continuation protocol instead.
pub(super) unsafe fn drive(
    map: *mut HewLayoutHashMap,
    key: *const c_void,
    inserting: bool,
    fault: *mut *mut c_void,
) -> Result<*mut HewLayoutMapProbe, i32> {
    // SAFETY: the C entry point validated a live receiver and matching key.
    let probe = unsafe { hew_hashmap_probe_begin(map, key, i32::from(inserting)) };
    loop {
        // SAFETY: this driver uniquely owns the probe and never suspends.
        let result = unsafe {
            match (*probe).advance() {
                HewMapProbeStatus::Ready => return Ok(probe),
                HewMapProbeStatus::NeedHash => {
                    let hash = (*map)
                        .key_layout
                        .hash_fn
                        .expect("synchronous map operation requires a hash callback");
                    key_hash(hash, hew_hashmap_probe_left(probe), fault)
                        .map(|value| hew_hashmap_probe_submit_hash(probe, value))
                }
                HewMapProbeStatus::NeedEq => {
                    let equal = (*map)
                        .key_layout
                        .eq_fn
                        .expect("synchronous map operation requires an equality callback");
                    key_eq(
                        equal,
                        hew_hashmap_probe_left(probe),
                        hew_hashmap_probe_right(probe),
                        fault,
                    )
                    .map(|value| hew_hashmap_probe_submit_eq(probe, value))
                }
            }
        };
        if let Err(status) = result {
            // SAFETY: the callback finished without transferring any input;
            // freeing the probe drops raw staging storage only.
            unsafe { hew_hashmap_probe_free(probe) };
            return Err(status);
        }
    }
}

pub(super) unsafe fn get_pointer(probe: *mut HewLayoutMapProbe) -> *const c_void {
    // SAFETY: the caller transfers the ready probe; returned storage remains
    // borrowed from its caller-owned map.
    unsafe {
        let probe = ready(probe);
        if probe.found {
            probe.value().cast()
        } else {
            ptr::null()
        }
    }
}

pub(super) unsafe fn remove_drop(probe: *mut HewLayoutMapProbe) -> bool {
    // SAFETY: the caller transfers a ready removal probe and captures releases.
    unsafe {
        let mut cursor = ptr::null_mut();
        let found = remove(&ready(probe), None, &raw mut cursor);
        hew_release_sync(cursor);
        found
    }
}

pub(super) unsafe fn insert_transfer(probe: *mut HewLayoutMapProbe, value: *const c_void) -> bool {
    // SAFETY: the C transfer-in contract lends independent owners, taking the
    // value on both paths and the key only for a new entry.
    unsafe {
        let mut cursor = ptr::null_mut();
        let inserted = insert(
            ready(probe),
            value,
            InsertMode::RawTransfer,
            &raw mut cursor,
        );
        hew_release_sync(cursor);
        inserted
    }
}
