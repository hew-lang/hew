use super::*;
use crate::callable::{hew_callable_drop, hew_callable_env_alloc, HewCallableDescriptor};
use hew_cabi::value::HewTypeOwnershipKind;
use std::sync::atomic::{AtomicUsize, Ordering};

#[derive(Default)]
struct Counts {
    starts: AtomicUsize,
    captures: AtomicUsize,
    returns: AtomicUsize,
    closes: AtomicUsize,
}

unsafe extern "C-unwind" fn drop_capture(slot: *mut c_void) {
    // SAFETY: the test retains the counters beyond this owning pointer slot.
    let counts = unsafe { &**slot.cast::<*const Counts>() };
    counts.captures.fetch_add(1, Ordering::SeqCst);
}

unsafe extern "C-unwind" fn drop_return(slot: *mut c_void) {
    // SAFETY: the invocation writes one pointer with the exact return layout.
    let counts = unsafe { &**slot.cast::<*const Counts>() };
    counts.returns.fetch_add(1, Ordering::SeqCst);
}

const CAPTURE: HewValueLayout = HewValueLayout {
    code_owner: core::ptr::null(),
    managed: core::ptr::null(),
    release_start: None,
    size: size_of::<*const Counts>(),
    align: align_of::<*const Counts>(),
    ownership_kind: HewTypeOwnershipKind::LayoutManaged,
    clone_fn: None,
    drop_fn: Some(drop_capture),
};

const RETURN: HewValueLayout = HewValueLayout {
    release_start: None,
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
        invoke_once: Some(complete),
        invoke_borrow_sync: None,
        invoke_once_sync: None,
        specializations: core::ptr::null(),
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
        hew_checked_generator_new(&raw mut callable, &CAPTURE, &RETURN, 8, ptr::null())
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

#[repr(C)]
struct CloseFrame {
    resume: Option<unsafe extern "C-unwind" fn(*mut c_void)>,
    destroy: Option<unsafe extern "C-unwind" fn(*mut c_void)>,
    slot: *mut c_void,
    state: *mut HewCoroState,
    drop: hew_cabi::value::HewValueDropThunk,
}

unsafe extern "C-unwind" fn resume_close(frame: *mut c_void) {
    // SAFETY: the fixture supplied this continuation prefix and retained value slot.
    unsafe {
        let frame = &mut *frame.cast::<CloseFrame>();
        let counts = &**frame.slot.cast::<*const Counts>();
        counts.closes.fetch_add(1, Ordering::SeqCst);
        (frame.drop)(frame.slot);
        crate::coro_state::hew_coro_state_finish(frame.state, 0);
        frame.resume = None;
    }
}

unsafe extern "C-unwind" fn destroy_close(frame: *mut c_void) {
    // SAFETY: semantic cleanup completed; the driver transfers the unique frame.
    unsafe { crate::cont::hew_cont_frame_free(frame) };
}

unsafe fn release_count(
    slot: *mut c_void,
    state: *mut c_void,
    drop: hew_cabi::value::HewValueDropThunk,
) -> *mut c_void {
    // SAFETY: the test retains this exact slot and state through completion.
    unsafe {
        let counts = &**slot.cast::<*const Counts>();
        counts.closes.fetch_add(1, Ordering::SeqCst);
        let frame =
            crate::cont::hew_cont_frame_alloc(size_of::<CloseFrame>() as u64).cast::<CloseFrame>();
        frame.write(CloseFrame {
            resume: Some(resume_close),
            destroy: Some(destroy_close),
            slot,
            state: state.cast(),
            drop,
        });
        frame.cast()
    }
}

unsafe extern "C" fn release_capture(
    slot: *mut c_void,
    _: *mut *mut c_void,
    state: *mut c_void,
) -> *mut c_void {
    // SAFETY: the descriptor selects the capture slot and its consuming callback.
    unsafe { release_count(slot, state, drop_capture) }
}

unsafe extern "C" fn release_return(
    slot: *mut c_void,
    _: *mut *mut c_void,
    state: *mut c_void,
) -> *mut c_void {
    // SAFETY: the descriptor selects the return slot and its consuming callback.
    unsafe { release_count(slot, state, drop_return) }
}

#[test]
fn pending_nested_close_retains_lazy_captures_and_terminal_outputs() {
    for lazy in [true, false] {
        let counts = Counts::default();
        let capture = HewValueLayout {
            release_start: lazy.then_some(release_capture),
            ..CAPTURE
        };
        let returned = HewValueLayout {
            release_start: (!lazy).then_some(release_return),
            ..RETURN
        };
        let descriptor = HewCallableDescriptor {
            environment: &raw const capture,
            ..descriptor()
        };
        let (_readiness, waker) = crate::wake::blocking::Readiness::new();
        // SAFETY: descriptors, counters and parent remain live until close/free.
        unsafe {
            let parent = hew_coro_state_new(waker.descriptor(), ptr::null_mut());
            let environment = hew_callable_env_alloc(&raw const descriptor);
            environment.cast::<*const Counts>().write(&raw const counts);
            let mut callable = HewCallableValue {
                environment,
                descriptor: &raw const descriptor,
            };
            let generator = hew_checked_generator_new(
                &raw mut callable,
                &CAPTURE,
                &raw const returned,
                8,
                ptr::null(),
            );
            if !lazy {
                assert_eq!(
                    hew_checked_generator_poll(generator, parent, false),
                    CoroStatus::Complete as i32
                );
            }
            assert_eq!(
                hew_checked_generator_poll(generator, parent, true),
                CoroStatus::Pending as i32
            );
            assert_eq!(counts.captures.load(Ordering::SeqCst), usize::from(!lazy));
            assert_eq!(counts.returns.load(Ordering::SeqCst), 0);
            assert_eq!(
                hew_checked_generator_poll(generator, parent, true),
                CoroStatus::Complete as i32
            );
            hew_checked_generator_free(generator);
            hew_coro_state_free(parent);
        }
        assert_eq!(counts.starts.load(Ordering::SeqCst), usize::from(!lazy));
        assert_eq!(counts.captures.load(Ordering::SeqCst), 1);
        assert_eq!(counts.returns.load(Ordering::SeqCst), usize::from(!lazy));
        assert_eq!(counts.closes.load(Ordering::SeqCst), 2);
    }
}

#[test]
#[expect(
    clippy::too_many_lines,
    reason = "keep descriptor and actor lifetimes local to both transfer and refund paths"
)]
fn generator_execution_keeps_retained_account_after_aborted_or_committed_transfer() {
    use crate::owned_value_budget::{current_owner, transfer::ValueTransfer, ValueOwner};
    use hew_cabi::managed::{HewManagedValueDescriptor, HEAP_EXCEEDED};

    struct Probe {
        expected: Option<ValueOwner>,
        descriptor: *const HewCallableDescriptor,
        observed: AtomicUsize,
    }
    unsafe extern "C" fn finish(
        environment: *mut c_void,
        _arguments: *const *mut c_void,
        _output: *mut c_void,
        fault: *mut *mut c_void,
        state: *mut c_void,
    ) -> *mut c_void {
        // SAFETY: the test retains this probe through the complete invocation.
        let probe = unsafe { &**environment.cast::<*const Probe>() };
        let actual = current_owner();
        assert!(match (&actual, &probe.expected) {
            (None, None) => true,
            (Some(actual), Some(expected)) => actual.same_account(expected),
            _ => false,
        });
        probe.observed.fetch_add(1, Ordering::SeqCst);
        let mut callable = HewCallableValue {
            environment,
            descriptor: probe.descriptor,
        };
        // SAFETY: completion consumes the original environment once under its
        // retained account; the unit output requires no initialized bytes.
        unsafe {
            hew_callable_drop(&raw mut callable);
            fault.write(ptr::null_mut());
            crate::coro_state::hew_coro_state_finish(state.cast(), 0);
        }
        ptr::null_mut()
    }
    let _guard = crate::runtime_test_guard();
    let environment_metadata = HewManagedValueDescriptor {
        slot_bytes: 8,
        visit: None,
    };
    let environment = HewValueLayout {
        managed: &raw const environment_metadata,
        ownership_kind: HewTypeOwnershipKind::Plain,
        drop_fn: None,
        ..CAPTURE
    };
    let unit_metadata = HewManagedValueDescriptor {
        slot_bytes: 0,
        visit: None,
    };
    let unit = HewValueLayout {
        size: 0,
        align: 1,
        managed: &raw const unit_metadata,
        ..environment
    };
    let descriptor = HewCallableDescriptor {
        environment: &raw const environment,
        invoke_once: Some(finish),
        ..descriptor()
    };
    let generator_metadata = HewManagedValueDescriptor {
        slot_bytes: 0,
        visit: Some(hew_managed_visit_generator),
    };
    let generator_layout = HewValueLayout {
        managed: &raw const generator_metadata,
        ..environment
    };
    let ambient = ValueOwner::new(Some(0));
    let mut actor = crate::test_actor::stub_actor();
    let mut completion = crate::actor_native::NativeActorCompletion::default();
    completion.cleanup.value_owner = Some(ambient.clone());
    actor.native_completion = Some(Arc::new(completion));
    let (_readiness, waker) = crate::wake::blocking::Readiness::new();
    for committed in [false, true] {
        let destination = ValueOwner::new(Some(if committed { 64 } else { 0 }));
        let probe = Probe {
            expected: committed.then(|| destination.clone()),
            descriptor: &raw const descriptor,
            observed: AtomicUsize::new(0),
        };
        // SAFETY: descriptors, probe and actor outlive the transferred generator,
        // its synchronous invocation and all semantic cleanup.
        unsafe {
            let slot = hew_callable_env_alloc(&raw const descriptor);
            slot.cast::<*const Probe>().write(&raw const probe);
            let mut callable = HewCallableValue {
                environment: slot,
                descriptor: &raw const descriptor,
            };
            let generator = hew_checked_generator_new(
                &raw mut callable,
                &raw const unit,
                &raw const unit,
                8,
                ptr::null(),
            );
            let mut transfer = ValueTransfer::begin(
                None,
                Some(destination.clone()),
                &raw const generator_layout,
                (&raw const generator).cast(),
            );
            if committed {
                assert!(transfer.poll_prepare().unwrap());
                transfer.commit().unwrap();
            } else {
                assert_eq!(transfer.poll_prepare(), Err(HEAP_EXCEEDED));
                drop(transfer);
            }
            let _context =
                crate::execution_context::TestExecutionContext::install(HewExecutionContext {
                    actor: &raw mut actor,
                    ..HewExecutionContext::default()
                });
            assert!(current_owner().unwrap().same_account(&ambient));
            let parent = hew_coro_state_new(waker.descriptor(), ptr::null_mut());
            assert_eq!(
                hew_checked_generator_poll(generator, parent, false),
                CoroStatus::Complete as i32
            );
            assert!(current_owner().unwrap().same_account(&ambient));
            assert_eq!(probe.observed.load(Ordering::SeqCst), 1);
            assert_eq!(
                hew_checked_generator_poll(generator, parent, true),
                CoroStatus::Complete as i32
            );
            hew_checked_generator_free(generator);
            hew_coro_state_free(parent);
        }
        assert_eq!(destination.live_bytes(), 0);
        assert_eq!(ambient.live_bytes(), 0);
    }
}
