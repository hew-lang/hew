//! Full-width layout validity and actual C ABI realloc ownership behavior.
use super::*;
use std::cell::UnsafeCell;

#[repr(C, align(8))]
struct Heap {
    blocks: [[u8; 32]; 2],
    live: bool,
    index: usize,
    size: usize,
    align: usize,
}
struct Slot(UnsafeCell<Heap>);
unsafe impl Sync for Slot {}
static HEAP: Slot = Slot(UnsafeCell::new(Heap {
    blocks: [[0; 32]; 2],
    live: false,
    index: 0,
    size: 0,
    align: 0,
}));

// Only the allocator primitive is modeled. It enforces exact layout pairing,
// relocates on realloc, preserves the prefix, and records physical release.
unsafe fn allocate(layout: Layout) -> *mut u8 {
    let h = &mut *HEAP.0.get();
    assert!(!h.live, "heap: allocation lost an existing owner");
    assert!(layout.size() > 0 && layout.size() <= 32 && layout.align() <= 8);
    h.live = true;
    h.size = layout.size();
    h.align = layout.align();
    h.index = 0;
    h.blocks[0].as_mut_ptr()
}
unsafe fn release(p: *mut u8, layout: Layout) {
    let h = &mut *HEAP.0.get();
    assert!(h.live, "heap: double release");
    assert_eq!(
        p,
        h.blocks[h.index].as_mut_ptr(),
        "heap: freed the wrong allocation"
    );
    assert_eq!(layout.size(), h.size, "heap: free used the wrong size");
    assert_eq!(
        layout.align(),
        h.align,
        "heap: free used the wrong alignment"
    );
    h.live = false;
}
unsafe fn resize(p: *mut u8, old: Layout, size: usize) -> *mut u8 {
    let h = &mut *HEAP.0.get();
    assert!(h.live, "heap: resizing released memory");
    assert_eq!(
        p,
        h.blocks[h.index].as_mut_ptr(),
        "heap: resizing the wrong allocation"
    );
    assert_eq!(old.size(), h.size, "heap: realloc used the wrong old size");
    assert_eq!(
        old.align(),
        h.align,
        "heap: realloc used the wrong alignment"
    );
    assert!(size > 0 && size <= 32);
    let old_index = h.index;
    let new_index = 1 - old_index;
    for i in 0..h.size.min(size) {
        h.blocks[new_index][i] = h.blocks[old_index][i];
    }
    h.index = new_index;
    h.size = size;
    h.blocks[new_index].as_mut_ptr()
}
fn unexpected_panic(_: std::fmt::Arguments<'_>) -> ! {
    assert!(false, "heap: unexpected panic");
    kani::assume(false);
    loop {}
}

#[kani::proof]
fn general_heap_layout_rejects_unrepresentable_extents() {
    let size: usize = kani::any();
    let align: usize = kani::any();
    let result = floor_layout(size, align);
    let expected = if align > 0 && align.count_ones() == 1 && size > 0 {
        let rounded = (size as u128 + align as u128 - 1) & !(align as u128 - 1);
        rounded <= isize::MAX as u128
    } else {
        false
    };
    assert_eq!(
        result.is_some(),
        expected,
        "heap: invalid or unrepresentable layout accepted"
    );
    if let Some(layout) = result {
        assert_eq!(
            layout.size(),
            size,
            "heap: allocation layout truncated size"
        );
        assert_eq!(
            layout.align(),
            align,
            "heap: allocation layout truncated alignment"
        );
    }
}

#[kani::proof]
#[kani::unwind(33)]
#[kani::stub(std::alloc::alloc, allocate)]
#[kani::stub(std::alloc::dealloc, release)]
#[kani::stub(std::alloc::realloc, resize)]
#[kani::stub(core::panicking::panic_fmt, unexpected_panic)]
fn general_heap_realloc_preserves_or_releases_the_exact_owner() {
    let power: u8 = kani::any();
    kani::assume(power < 4);
    let align = 1u64 << power;
    let requested: u8 = kani::any();
    kani::assume(requested <= 16);
    let invalid: bool = kani::any();
    let size = if invalid {
        u64::MAX
    } else {
        u64::from(requested)
    };
    unsafe {
        let first = hew_alloc(8, align);
        assert!(!first.is_null());
        for i in 0..8 {
            first.add(i).write(i as u8 + 10);
        }
        let second = hew_realloc(first, 8, size, align);
        if invalid {
            assert!(second.is_null(), "heap: invalid realloc succeeded");
            let h = &*HEAP.0.get();
            assert!(
                h.live && h.size == 8 && h.index == 0,
                "heap: rejected realloc changed old ownership"
            );
            for i in 0..8 {
                assert_eq!(
                    first.add(i).read(),
                    i as u8 + 10,
                    "heap: rejected realloc damaged old bytes"
                );
            }
            hew_dealloc(first, 8, align);
        } else if requested == 0 {
            assert!(
                second.is_null(),
                "heap: shrink-to-empty returned live memory"
            );
            assert!(
                !(*HEAP.0.get()).live,
                "heap: shrink-to-empty leaked the allocation"
            );
        } else {
            assert!(!second.is_null());
            assert_eq!(
                second,
                (*HEAP.0.get()).blocks[1].as_mut_ptr(),
                "heap: realloc returned the old freed allocation"
            );
            for i in 0..8.min(usize::from(requested)) {
                assert_eq!(
                    second.add(i).read(),
                    i as u8 + 10,
                    "heap: realloc lost the live prefix"
                );
            }
            second.add(usize::from(requested) - 1).write(77);
            hew_dealloc(second, size, align);
        }
        assert!(
            !(*HEAP.0.get()).live,
            "heap: allocator lifecycle leaked its final owner"
        );
        let from_null = hew_realloc(ptr::null_mut(), 0, 8, align);
        assert!(
            !from_null.is_null(),
            "heap: grow-from-null refused a valid allocation"
        );
        hew_dealloc(from_null, 8, align);
        assert!(!(*HEAP.0.get()).live);
    }
}
