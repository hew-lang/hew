//! Bounded heap checks of the unchanged production allocation entry points.
use super::*;
use std::sync::atomic::Ordering;

fn unexpected_panic(_: std::fmt::Arguments<'_>) -> ! {
    assert!(false, "arena: unexpected runtime panic");
    kani::assume(false);
    loop {}
}

#[kani::stub(core::panicking::panic_fmt, unexpected_panic)]
#[kani::proof]
#[kani::unwind(3)]
fn arena_allocation_is_disjoint_or_refused() {
    let small: u8 = kani::any();
    kani::assume(small <= 80);
    let choice: u8 = kani::any();
    let requested = match choice {
        0 => usize::MAX,
        1 => usize::MAX - 7,
        2 => isize::MAX as usize + 1,
        _ => usize::from(small),
    };
    let mut arena = ActorArena::new_with_sizes(32, 128).unwrap();
    let mut context = crate::execution_context::Context {
        arena: &mut arena,
        cancel_token: ptr::null_mut(),
        request_deadline: crate::request_deadline::RequestDeadline::default(),
    };
    crate::execution_context::CURRENT.store(&mut context, Ordering::Relaxed);
    unsafe {
        let first = hew_arena_malloc(8).cast::<u64>();
        assert!(!first.is_null());
        first.write(0xa5a5_a5a5_a5a5_a5a5);
        let second = hew_arena_malloc(requested).cast::<u8>();
        if !second.is_null() {
            assert!(requested > 0, "arena: zero-sized allocation");
            assert_eq!(second as usize % 8, 0, "arena: misaligned block");
            second.write(0x3c);
            second.add(requested - 1).write(0x3c);
        }
        assert_eq!(
            first.read(),
            0xa5a5_a5a5_a5a5_a5a5,
            "arena: live blocks overlap"
        );
    }
    crate::execution_context::CURRENT.store(ptr::null_mut(), Ordering::Relaxed);
    // This harness proves allocation, not teardown. Avoid expanding Vec's
    // unrelated destructor; the allocation operations above are not stubbed.
    std::mem::forget(arena);
}

#[kani::stub(core::panicking::panic_fmt, unexpected_panic)]
#[kani::proof]
#[kani::unwind(3)]
fn arena_mapping_failure_preserves_quota_and_reset_reuses_chunks() {
    let mut arena = ActorArena::new_with_sizes(32, 128).unwrap();
    // An arbitrary valid, partly used first chunk; enough quota for the
    // request, which must fail solely because the OS mapping fails.
    let cursor: u8 = kani::any();
    kani::assume(cursor <= 32);
    let used: u8 = kani::any();
    kani::assume(used <= cursor);
    arena.cursor = usize::from(cursor);
    arena.used = usize::from(used);
    arena.cap = 128;
    crate::arena_host::FAIL_MAPPING.store(true, Ordering::Relaxed);
    assert!(
        arena.alloc(80, 8).is_null(),
        "arena: failed mapping accepted"
    );
    assert_eq!(
        arena.cursor,
        usize::from(cursor),
        "arena: failed allocation consumed cursor"
    );
    assert_eq!(
        arena.used,
        usize::from(used),
        "arena: failed allocation consumed quota"
    );
    assert_eq!(
        arena.current_chunk, 0,
        "arena: failed allocation abandoned retained chunk"
    );
    assert_eq!(
        arena.chunks.len(),
        1,
        "arena: failed mapping retained a chunk"
    );
    let base = arena.chunks[0].base;
    arena.reset();
    assert_eq!(arena.used, 0, "arena: reset retained quota usage");
    crate::arena_host::FAIL_MAPPING.store(true, Ordering::Relaxed);
    assert_eq!(
        arena.alloc(32, 8),
        base,
        "arena: reset failed to reuse retained chunk"
    );
    crate::arena_host::FAIL_MAPPING.store(false, Ordering::Relaxed);
    std::mem::forget(arena);
}
