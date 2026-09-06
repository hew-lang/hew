use super::*;
use crate::callable::{hew_callable_env_alloc, HewCallableDescriptor};
use hew_cabi::value::HewTypeOwnershipKind;
use std::sync::atomic::{AtomicUsize, Ordering};

#[derive(Default)]
struct Counts {
    starts: AtomicUsize,
    captures: AtomicUsize,
    returns: AtomicUsize,
}

unsafe extern "C" fn drop_capture(slot: *mut c_void) {
    // SAFETY: the test retains the counters beyond this owning pointer slot.
    let counts = unsafe { &**slot.cast::<*const Counts>() };
    counts.captures.fetch_add(1, Ordering::SeqCst);
}

unsafe extern "C" fn drop_return(slot: *mut c_void) {
    // SAFETY: the invocation writes one pointer with the exact return layout.
    let counts = unsafe { &**slot.cast::<*const Counts>() };
    counts.returns.fetch_add(1, Ordering::SeqCst);
}

const CAPTURE: HewValueLayout = HewValueLayout {
    size: size_of::<*const Counts>(),
    align: align_of::<*const Counts>(),
    ownership_kind: HewTypeOwnershipKind::LayoutManaged,
    clone_fn: None,
    drop_fn: Some(drop_capture),
};

const RETURN: HewValueLayout = HewValueLayout {
    drop_fn: Some(drop_return),
    ..CAPTURE
};

unsafe extern "C" fn complete(
    environment: *mut c_void,
    _arguments: *const *mut c_void,
    output: *mut c_void,
    fault: *mut *mut c_void,
    state: *mut c_void,
) -> *mut c_void {
    let descriptor = descriptor();
    let mut callable = HewCallableValue {
        environment,
        descriptor: &raw const descriptor,
    };
    // SAFETY: the test uses this exact environment and output layout, keeps
    // counters live, and transfers the once environment to this invocation.
    unsafe {
        let counts = *environment.cast::<*const Counts>();
        (*counts).starts.fetch_add(1, Ordering::SeqCst);
        output.cast::<*const Counts>().write(counts);
        fault.write(ptr::null_mut());
        hew_callable_drop(&raw mut callable);
        crate::coro_state::hew_coro_state_finish(state.cast(), 0);
    }
    ptr::null_mut()
}

fn descriptor() -> HewCallableDescriptor {
    HewCallableDescriptor {
        environment: &CAPTURE,
        invoke_borrow: None,
        invoke_once: complete,
    }
}

fn producer(descriptor: &HewCallableDescriptor, counts: &Counts) -> *mut HewCheckedGenerator {
    // SAFETY: descriptor and counters outlive the generator in each test.
    unsafe {
        let environment = hew_callable_env_alloc(descriptor);
        environment.cast::<*const Counts>().write(counts);
        let mut callable = HewCallableValue {
            environment,
            descriptor,
        };
        hew_checked_generator_new(&raw mut callable, &CAPTURE, &RETURN)
    }
}

#[test]
fn lazy_close_releases_captures_without_starting_the_body() {
    let counts = Counts::default();
    let descriptor = descriptor();
    let generator = producer(&descriptor, &counts);
    // SAFETY: this unique unstarted generator needs no parent invocation state.
    unsafe {
        assert_eq!(
            hew_checked_generator_poll(generator, ptr::null_mut(), true),
            CoroStatus::Complete as i32
        );
        assert_eq!(
            hew_checked_generator_poll(generator, ptr::null_mut(), true),
            CoroStatus::Complete as i32
        );
        hew_checked_generator_free(generator);
    }
    assert_eq!(counts.starts.load(Ordering::SeqCst), 0);
    assert_eq!(counts.captures.load(Ordering::SeqCst), 1);
    assert_eq!(counts.returns.load(Ordering::SeqCst), 0);
}

#[test]
fn repeated_completion_keeps_one_return_owner_until_close() {
    let counts = Counts::default();
    let descriptor = descriptor();
    let generator = producer(&descriptor, &counts);
    let (_readiness, waker) = crate::wake::blocking::Readiness::new();
    // SAFETY: the retained waker outlives this parent and its generator child.
    unsafe {
        let parent = hew_coro_state_new(waker.descriptor(), ptr::null_mut());
        for _ in 0..3 {
            assert_eq!(
                hew_checked_generator_poll(generator, parent, false),
                CoroStatus::Complete as i32
            );
        }
        assert_eq!(counts.starts.load(Ordering::SeqCst), 1);
        assert_eq!(counts.captures.load(Ordering::SeqCst), 1);
        assert_eq!(counts.returns.load(Ordering::SeqCst), 0);
        assert_eq!(
            hew_checked_generator_poll(generator, parent, true),
            CoroStatus::Complete as i32
        );
        assert_eq!(counts.returns.load(Ordering::SeqCst), 1);
        hew_checked_generator_free(generator);
        hew_coro_state_free(parent);
    }
    assert_eq!(counts.returns.load(Ordering::SeqCst), 1);
}
