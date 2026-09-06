//! Callable environment allocation and the shared erased clone/drop protocol.
//!
//! Compiler-generated layouts interpret capture initialization masks. These
//! helpers only manage outer storage and invoke those layout callbacks. LLVM
//! adapters own invocation and dispose once-call environments on both outcomes.

use core::{ffi::c_void, ptr};
use std::alloc::Layout;

pub use hew_cabi::callable::{HewCallableDescriptor, HewCallableInvoke, HewCallableValue};
use hew_cabi::value::{HewTypeOwnershipKind, HewValueLayout};

/// Read and validate storage metadata without interpreting capture semantics.
///
/// # Safety
/// Non-null descriptor and layout pointers must refer to valid immutable data.
unsafe fn environment_layout(descriptor: *const HewCallableDescriptor) -> Option<HewValueLayout> {
    // SAFETY: Pointer validity is the caller's contract; null is rejected.
    let descriptor = unsafe { descriptor.as_ref()? };
    // SAFETY: The descriptor's non-null layout must remain valid.
    let layout = *unsafe { descriptor.environment.as_ref()? };
    Layout::from_size_align(layout.size, layout.align).ok()?;
    if layout.size == 0 {
        // A capture, including a ZST, requires a nonzero initialization mask.
        // Null storage is reserved for empty functions with no callbacks.
        if layout.align != 1
            || layout.ownership_kind != HewTypeOwnershipKind::Plain
            || layout.clone_fn.is_some()
            || layout.drop_fn.is_some()
        {
            return None;
        }
    } else if layout.ownership_kind != HewTypeOwnershipKind::Plain && layout.drop_fn.is_none() {
        return None;
    }
    Some(layout)
}

/// # Safety
/// `layout` has been checked by `environment_layout`.
unsafe fn allocate_environment(layout: &HewValueLayout) -> *mut c_void {
    if layout.size == 0 {
        return ptr::null_mut();
    }
    // SAFETY: Valid nonzero layout; OOM aborts in the allocator authority.
    let environment = unsafe { crate::mem::hew_alloc(layout.size as u64, layout.align as u64) };
    if environment.is_null() {
        std::process::abort();
    }
    // SAFETY: The new allocation is writable for exactly layout.size bytes.
    unsafe { environment.write_bytes(0, layout.size) };
    environment.cast()
}

/// Allocate zeroed storage for the descriptor's exact environment layout.
///
/// Zero bytes initialize capture masks; they do not initialize capture values.
/// Empty functions return null. Invalid descriptors and allocation failure abort.
///
/// # Safety
/// The descriptor and its layout must be valid immutable data and outlive the
/// allocation. The caller must eventually transfer or drop that allocation.
#[no_mangle]
pub unsafe extern "C" fn hew_callable_env_alloc(
    descriptor: *const HewCallableDescriptor,
) -> *mut c_void {
    // SAFETY: The caller provides descriptor validity.
    let Some(layout) = (unsafe { environment_layout(descriptor) }) else {
        std::process::abort();
    };
    // SAFETY: The layout was checked above.
    unsafe { allocate_environment(&layout) }
}

/// Deep-clone a callable, publishing the output carrier only after success.
///
/// A callback failure has already rolled back partial capture copies. Release
/// only outer storage on that path and propagate its status without touching
/// `out`. Missing non-Plain clone glue or invalid carriers return -1.
///
/// # Safety
/// Non-null `src` and `out` must be valid non-overlapping carrier slots; `out`
/// is writable and must not own a live environment when this call succeeds.
/// The source owns its environment, its descriptor remains valid, and callbacks
/// obey the shared value clone/drop protocol without unwinding across C.
#[no_mangle]
pub unsafe extern "C" fn hew_callable_clone(
    src: *const HewCallableValue,
    out: *mut HewCallableValue,
) -> i32 {
    if src.is_null() || out.is_null() || ptr::eq(src, out) {
        return -1;
    }
    // SAFETY: The caller provides a valid source carrier.
    let source = unsafe { &*src };
    // SAFETY: The source descriptor and layout remain valid.
    let Some(layout) = (unsafe { environment_layout(source.descriptor) }) else {
        return -1;
    };
    if (layout.size == 0) != source.environment.is_null()
        || (layout.ownership_kind != HewTypeOwnershipKind::Plain && layout.clone_fn.is_none())
    {
        return -1;
    }
    // SAFETY: Checked layout, fresh independent allocation.
    let environment = unsafe { allocate_environment(&layout) };
    if layout.size != 0 {
        // SAFETY: Source and fresh destination match this exact nonzero layout.
        unsafe {
            ptr::copy_nonoverlapping(
                source.environment.cast::<u8>(),
                environment.cast::<u8>(),
                layout.size,
            );
        }
        if let Some(clone) = layout.clone_fn {
            // SAFETY: Destination bytes were copied before semantic cloning.
            let status = unsafe { clone(source.environment, environment) };
            if status != 0 {
                // SAFETY: The thunk rolled back its partial copies. No drop is
                // owed, but this exact outer allocation still belongs to us.
                unsafe {
                    crate::mem::hew_dealloc(
                        environment.cast(),
                        layout.size as u64,
                        layout.align as u64,
                    );
                }
                return status;
            }
        }
    }
    // SAFETY: The output is writable and does not alias the source. Publish
    // the complete independent owner only after every fallible action succeeds.
    unsafe {
        out.write(HewCallableValue {
            environment,
            descriptor: source.descriptor,
        });
    }
    0
}

/// Clear a carrier, drop initialized captures, and free its outer storage.
///
/// Null pointers and cleared carriers are inert. Invalid live carriers abort.
/// The layout callback releases captures; it must not free the environment.
///
/// # Safety
/// A non-null pointer must name a writable carrier with sole ownership of its
/// environment, or a cleared carrier. Descriptor and callback storage remain
/// valid until cleanup completes, and callbacks cannot unwind across C.
#[no_mangle]
pub unsafe extern "C" fn hew_callable_drop(value: *mut HewCallableValue) {
    if value.is_null() {
        return;
    }
    // SAFETY: The carrier slot is writable. Clearing before callbacks detaches
    // the owner and makes subsequent drops of that slot inert.
    let carrier = unsafe {
        value.replace(HewCallableValue {
            environment: ptr::null_mut(),
            descriptor: ptr::null(),
        })
    };
    if carrier.descriptor.is_null() && carrier.environment.is_null() {
        return;
    }
    // SAFETY: A live carrier's descriptor and layout must remain valid.
    let Some(layout) = (unsafe { environment_layout(carrier.descriptor) }) else {
        std::process::abort();
    };
    if (layout.size == 0) != carrier.environment.is_null() {
        std::process::abort();
    }
    if let Some(drop) = layout.drop_fn {
        // SAFETY: The compiler callback reads its mask and releases only live
        // captures in this exact environment. It does not free outer storage.
        unsafe { drop(carrier.environment) };
    }
    // SAFETY: The detached allocation is released once with its original layout.
    unsafe {
        crate::mem::hew_dealloc(
            carrier.environment.cast(),
            layout.size as u64,
            layout.align as u64,
        );
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use core::mem::MaybeUninit;
    use std::sync::atomic::{AtomicUsize, Ordering};

    #[derive(Default)]
    struct Counts {
        clones: AtomicUsize,
        drops: AtomicUsize,
        drop_calls: AtomicUsize,
    }

    struct Capture {
        value: i64,
        counts: *const Counts,
    }

    impl Drop for Capture {
        fn drop(&mut self) {
            // SAFETY: Each test keeps its counters alive until all captures drop.
            unsafe { &*self.counts }
                .drops
                .fetch_add(1, Ordering::SeqCst);
        }
    }

    #[repr(C, align(64))]
    struct Environment {
        mask: u8,
        captures: [*mut Capture; 2],
        counts: *const Counts,
        fail_clone: bool,
    }

    const ENVIRONMENT_LAYOUT: HewValueLayout = HewValueLayout {
        size: size_of::<Environment>(),
        align: align_of::<Environment>(),
        ownership_kind: HewTypeOwnershipKind::LayoutManaged,
        clone_fn: Some(clone_environment),
        drop_fn: Some(drop_environment),
    };

    unsafe extern "C" fn drop_environment(raw: *mut c_void) {
        // SAFETY: This descriptor is used only with Environment allocations.
        // A zeroed mask has no capture obligations and permits null counters.
        unsafe {
            let environment = &mut *raw.cast::<Environment>();
            if let Some(counts) = environment.counts.as_ref() {
                counts.drop_calls.fetch_add(1, Ordering::SeqCst);
            }
            for index in 0..2 {
                if environment.mask & (1 << index) != 0 {
                    drop(Box::from_raw(environment.captures[index]));
                    environment.mask &= !(1 << index);
                    environment.captures[index] = ptr::null_mut();
                }
            }
        }
    }

    unsafe extern "C" fn clone_environment(from: *const c_void, to: *mut c_void) -> i32 {
        // SAFETY: The helper copied these exact Environment bytes first. Only
        // mask-marked source captures are valid; rollback owns only new copies.
        unsafe {
            let source = &*from.cast::<Environment>();
            let target = &mut *to.cast::<Environment>();
            target.mask = 0;
            target.captures = [ptr::null_mut(); 2];
            for index in 0..2 {
                if source.mask & (1 << index) != 0 {
                    let capture = &*source.captures[index];
                    target.captures[index] = Box::into_raw(Box::new(Capture {
                        value: capture.value,
                        counts: capture.counts,
                    }));
                    target.mask |= 1 << index;
                    (*capture.counts).clones.fetch_add(1, Ordering::SeqCst);
                    if source.fail_clone {
                        drop_environment(to);
                        return 17;
                    }
                }
            }
        }
        0
    }

    unsafe extern "C" fn borrow_invoke(
        raw: *mut c_void,
        arguments: *const *mut c_void,
        result: *mut c_void,
        fault: *mut *mut c_void,
    ) -> i32 {
        // SAFETY: The direct adapter tests supply the exact environment and
        // one i64 argument/output slot. The first capture is initialized.
        unsafe {
            let environment = &mut *raw.cast::<Environment>();
            let capture = &mut *environment.captures[0];
            capture.value += *(*arguments).cast::<i64>();
            result.cast::<i64>().write(capture.value);
            fault.write(ptr::null_mut());
        }
        0
    }

    unsafe extern "C" fn once_invoke(
        raw: *mut c_void,
        arguments: *const *mut c_void,
        result: *mut c_void,
        fault: *mut *mut c_void,
    ) -> i32 {
        let descriptor = descriptor();
        let mut owner = HewCallableValue {
            environment: raw,
            descriptor: &raw const descriptor,
        };
        // SAFETY: The caller transferred this environment and supplied an i64
        // argument/output. The adapter releases its environment on both exits.
        unsafe {
            let status = if *(*arguments).cast::<i64>() < 0 {
                fault.write(Box::into_raw(Box::new(29_i32)).cast());
                29
            } else {
                borrow_invoke(raw, arguments, result, fault)
            };
            hew_callable_drop(&raw mut owner);
            status
        }
    }

    fn descriptor() -> HewCallableDescriptor {
        HewCallableDescriptor {
            environment: &ENVIRONMENT_LAYOUT,
            invoke_borrow: Some(borrow_invoke),
            invoke_once: once_invoke,
        }
    }

    fn carrier(descriptor: &HewCallableDescriptor, counts: &Counts, mask: u8) -> HewCallableValue {
        // SAFETY: Descriptor matches Environment and outlives the carrier.
        // Tests retain counters until every resulting carrier is released.
        unsafe {
            let raw = hew_callable_env_alloc(descriptor);
            assert_eq!(raw.addr() % align_of::<Environment>(), 0);
            let bytes = std::slice::from_raw_parts(raw.cast::<u8>(), size_of::<Environment>());
            assert!(bytes.iter().all(|byte| *byte == 0));
            let environment = &mut *raw.cast::<Environment>();
            environment.counts = counts;
            for index in 0..2 {
                if mask & (1 << index) != 0 {
                    environment.captures[index] =
                        Box::into_raw(Box::new(Capture { value: 10, counts }));
                    environment.mask |= 1 << index;
                }
            }
            HewCallableValue {
                environment: raw,
                descriptor,
            }
        }
    }

    #[test]
    fn aligned_zeroed_environments_clone_independently_and_drop_once() {
        let counts = Counts::default();
        let descriptor = descriptor();
        let mut source = carrier(&descriptor, &counts, 3);
        let mut output = MaybeUninit::uninit();
        // SAFETY: Valid source, fresh output and the exact adapter slot types.
        unsafe {
            assert_eq!(
                hew_callable_clone(&raw const source, output.as_mut_ptr()),
                0
            );
            let mut copy = output.assume_init();
            assert_ne!(source.environment, copy.environment);
            let copied = &*copy.environment.cast::<Environment>();
            let original = &*source.environment.cast::<Environment>();
            assert_ne!(copied.captures[0], original.captures[0]);
            let mut increment = 7_i64;
            let arguments = [(&raw mut increment).cast()];
            let mut result = 0_i64;
            let mut fault = ptr::null_mut();
            assert_eq!(
                descriptor.invoke_borrow.unwrap()(
                    copy.environment,
                    arguments.as_ptr(),
                    (&raw mut result).cast(),
                    &raw mut fault
                ),
                0
            );
            assert_eq!(result, 17);
            assert!(fault.is_null());
            assert_eq!((*original.captures[0]).value, 10);
            hew_callable_drop(&raw mut copy);
            assert!(copy.environment.is_null() && copy.descriptor.is_null());
            hew_callable_drop(&raw mut copy);
            hew_callable_drop(&raw mut source);
        }
        assert_eq!(counts.clones.load(Ordering::SeqCst), 2);
        assert_eq!(counts.drops.load(Ordering::SeqCst), 4);
        assert_eq!(counts.drop_calls.load(Ordering::SeqCst), 2);
    }

    #[test]
    fn clone_failure_rolls_back_captures_preserves_output_and_does_not_drop_twice() {
        let counts = Counts::default();
        let descriptor = descriptor();
        let mut source = carrier(&descriptor, &counts, 3);
        let mut output = carrier(&descriptor, &counts, 1);
        let previous = output.environment;
        // SAFETY: The deliberately failing thunk leaves the existing output
        // owner unchanged. Source and output are distinct live environments.
        unsafe {
            (*source.environment.cast::<Environment>()).fail_clone = true;
            assert_eq!(hew_callable_clone(&raw const source, &raw mut output), 17);
            assert_eq!(output.environment, previous);
            assert!(ptr::eq(output.descriptor, &raw const descriptor));
            assert_eq!(counts.clones.load(Ordering::SeqCst), 1);
            assert_eq!(counts.drops.load(Ordering::SeqCst), 1);
            assert_eq!(counts.drop_calls.load(Ordering::SeqCst), 1);
            hew_callable_drop(&raw mut source);
            hew_callable_drop(&raw mut output);
        }
        assert_eq!(counts.drops.load(Ordering::SeqCst), 4);
        assert_eq!(counts.drop_calls.load(Ordering::SeqCst), 3);
    }

    #[test]
    fn release_only_environments_refuse_clone_without_touching_output() {
        let counts = Counts::default();
        let layout = HewValueLayout {
            clone_fn: None,
            ..ENVIRONMENT_LAYOUT
        };
        let mut descriptor = descriptor();
        descriptor.environment = &raw const layout;
        let mut source = carrier(&descriptor, &counts, 1);
        let mut output = HewCallableValue {
            environment: ptr::null_mut(),
            descriptor: ptr::null(),
        };
        // SAFETY: Valid release-only source; cloning must return before publication.
        unsafe {
            assert_eq!(hew_callable_clone(&raw const source, &raw mut output), -1);
            assert!(output.environment.is_null() && output.descriptor.is_null());
            hew_callable_drop(&raw mut source);
        }
        assert_eq!(counts.clones.load(Ordering::SeqCst), 0);
        assert_eq!(counts.drops.load(Ordering::SeqCst), 1);
    }

    #[test]
    fn mask_controls_partial_and_zero_sized_capture_cleanup() {
        let counts = Counts::default();
        let descriptor = descriptor();
        // Mask zero represents a newly allocated environment. Mask two owns
        // only the second field; its first capture bytes remain uninitialized.
        for mask in [0, 2] {
            let mut source = carrier(&descriptor, &counts, mask);
            let mut output = MaybeUninit::uninit();
            // SAFETY: Clone/drop callbacks inspect only initialized captures.
            unsafe {
                assert_eq!(
                    hew_callable_clone(&raw const source, output.as_mut_ptr()),
                    0
                );
                let mut copy = output.assume_init();
                assert_eq!((*copy.environment.cast::<Environment>()).mask, mask);
                hew_callable_drop(&raw mut source);
                hew_callable_drop(&raw mut copy);
            }
        }
        assert_eq!(counts.clones.load(Ordering::SeqCst), 1);
        assert_eq!(counts.drops.load(Ordering::SeqCst), 2);
        // A captured ZST still has a mask byte, independently allocated/copied.
        let layout = HewValueLayout {
            size: 1,
            align: 1,
            ownership_kind: HewTypeOwnershipKind::Plain,
            clone_fn: None,
            drop_fn: None,
        };
        let descriptor = HewCallableDescriptor {
            environment: &raw const layout,
            ..descriptor
        };
        // SAFETY: This layout's only byte is compiler-owned initialization state.
        unsafe {
            let mut source = HewCallableValue {
                environment: hew_callable_env_alloc(&raw const descriptor),
                descriptor: &raw const descriptor,
            };
            assert!(!source.environment.is_null());
            source.environment.cast::<u8>().write(1);
            let mut output = MaybeUninit::uninit();
            assert_eq!(
                hew_callable_clone(&raw const source, output.as_mut_ptr()),
                0
            );
            let mut copy = output.assume_init();
            assert_ne!(source.environment, copy.environment);
            assert_eq!(copy.environment.cast::<u8>().read(), 1);
            source.environment.cast::<u8>().write(0);
            assert_eq!(copy.environment.cast::<u8>().read(), 1);
            hew_callable_drop(&raw mut source);
            hew_callable_drop(&raw mut copy);
        }
    }

    #[test]
    fn empty_function_carrier_is_distinct_from_a_cleared_carrier() {
        unsafe extern "C" fn invoke(
            environment: *mut c_void,
            _arguments: *const *mut c_void,
            result: *mut c_void,
            fault: *mut *mut c_void,
        ) -> i32 {
            assert!(environment.is_null());
            // SAFETY: The test provides writable result and fault slots.
            unsafe {
                result.cast::<i64>().write(42);
                fault.write(ptr::null_mut());
            }
            0
        }
        let layout = HewValueLayout {
            size: 0,
            align: 1,
            ownership_kind: HewTypeOwnershipKind::Plain,
            clone_fn: None,
            drop_fn: None,
        };
        let descriptor = HewCallableDescriptor {
            environment: &raw const layout,
            invoke_borrow: Some(invoke),
            invoke_once: invoke,
        };
        // SAFETY: Empty-function descriptors own no environment storage.
        unsafe {
            let mut source = HewCallableValue {
                environment: hew_cabi::callable::hew_callable_env_alloc(&raw const descriptor),
                descriptor: &raw const descriptor,
            };
            assert!(source.environment.is_null());
            let mut output = MaybeUninit::uninit();
            assert_eq!(
                hew_cabi::callable::hew_callable_clone(&raw const source, output.as_mut_ptr()),
                0
            );
            let mut copy = output.assume_init();
            assert!(copy.environment.is_null());
            assert!(ptr::eq(copy.descriptor, &raw const descriptor));
            let mut result = 0_i64;
            let mut fault = ptr::null_mut();
            assert_eq!(
                (descriptor.invoke_borrow.unwrap())(
                    copy.environment,
                    ptr::null(),
                    (&raw mut result).cast(),
                    &raw mut fault,
                ),
                0
            );
            assert_eq!(result, 42);
            assert!(fault.is_null());
            hew_cabi::callable::hew_callable_drop(&raw mut source);
            assert_eq!(hew_callable_clone(&raw const source, &raw mut copy), -1);
            assert!(ptr::eq(copy.descriptor, &raw const descriptor));
            hew_callable_drop(&raw mut copy);
        }
    }

    #[test]
    fn consuming_adapter_disposes_environment_on_success_and_fault() {
        for increment in [5_i64, -1] {
            let counts = Counts::default();
            let descriptor = descriptor();
            let mut owner = carrier(&descriptor, &counts, 1);
            let mut argument = increment;
            let arguments = [(&raw mut argument).cast()];
            let mut result = 91_i64;
            let mut fault = ptr::null_mut();
            let raw = owner.environment;
            owner.environment = ptr::null_mut();
            owner.descriptor = ptr::null();
            // SAFETY: The caller transfers/invalidate its carrier before the
            // once adapter, which owns cleanup regardless of returned status.
            unsafe {
                let status = (descriptor.invoke_once)(
                    raw,
                    arguments.as_ptr(),
                    (&raw mut result).cast(),
                    &raw mut fault,
                );
                if increment < 0 {
                    assert_eq!(status, 29);
                    assert_eq!(result, 91);
                    assert_eq!(*Box::from_raw(fault.cast::<i32>()), 29);
                } else {
                    assert_eq!(status, 0);
                    assert_eq!(result, 15);
                    assert!(fault.is_null());
                }
                hew_callable_drop(&raw mut owner);
            }
            assert_eq!(counts.drops.load(Ordering::SeqCst), 1);
            assert_eq!(counts.drop_calls.load(Ordering::SeqCst), 1);
        }
    }
}
