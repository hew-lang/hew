//! Lazy generator invocation through the shared callable and coroutine ABI.
//!
//! Polling never waits or runs a scheduler. The consumer's checked suspension
//! edges own cancellation and drain; synchronous release is legal only after
//! that drain (or before the lazy body has ever been invoked).

use crate::callable::{hew_callable_drop, HewCallableValue};
use crate::cont::{hew_cont_destroy, hew_cont_done, hew_cont_resume};
use crate::coro_state::{
    hew_coro_state_cancel, hew_coro_state_free, hew_coro_state_new, hew_coro_state_resume_yield,
    hew_coro_state_status, hew_coro_state_token, hew_coro_state_waker, CoroStatus, HewCoroState,
};
use crate::execution_context::{current_context, set_current_context, HewExecutionContext};
use crate::fault::{hew_fault_drop, HewFault};
use crate::util::MutexExt;
use crate::wake::{HewWaker, OwnedWaker};
use hew_cabi::value::HewValueLayout;
use std::alloc::{alloc, dealloc, handle_alloc_error, Layout};
use std::ffi::c_void;
use std::ptr;
use std::sync::{Arc, Mutex};

/// Notifications retain this relay, never a generator or LLVM frame address.
/// Registering the next consumer and taking readiness use the same lock, so a
/// completion cannot be lost while a generator moves between invocation owners.
#[derive(Debug, Default)]
struct WakeRelay {
    inner: Mutex<(bool, Option<OwnedWaker>)>,
}

impl WakeRelay {
    fn descriptor(this: &Arc<Self>) -> HewWaker {
        unsafe extern "C" fn wake(context: *mut c_void) {
            // SAFETY: each descriptor owns an Arc reference to this relay.
            let relay = unsafe { &*context.cast::<WakeRelay>() };
            let target = {
                let mut inner = relay.inner.lock_or_recover();
                inner.0 = true;
                inner.1.clone()
            };
            if let Some(target) = target {
                target.wake();
            }
        }
        unsafe extern "C" fn retain(context: *mut c_void) {
            // SAFETY: the descriptor is borrowed from a live Arc allocation.
            unsafe { Arc::increment_strong_count(context.cast::<WakeRelay>()) };
        }
        unsafe extern "C" fn release(context: *mut c_void) {
            // SAFETY: consume exactly one reference acquired by retain.
            unsafe { Arc::decrement_strong_count(context.cast::<WakeRelay>()) };
        }
        HewWaker {
            context: Arc::as_ptr(this).cast_mut().cast(),
            wake,
            retain,
            release,
        }
    }
}

/// One affine generator owner. Yields and final returns reuse one aligned
/// output allocation; the published coroutine status selects its exact layout.
#[derive(Debug)]
pub struct HewCheckedGenerator {
    callable: Option<HewCallableValue>,
    yielded: *const HewValueLayout,
    returned: *const HewValueLayout,
    allocation: Layout,
    output: *mut c_void,
    initialized: Option<*const HewValueLayout>,
    relay: Arc<WakeRelay>,
    state: *mut HewCoroState,
    frame: *mut c_void,
    fault: *mut HewFault,
    closed: bool,
}

impl HewCheckedGenerator {
    unsafe fn discard_output(&mut self) {
        if let Some(layout) = self.initialized.take() {
            // SAFETY: the publication status selected this initialized layout;
            // taking the flag first prevents a second release.
            if let Some(drop) = unsafe { (*layout).drop_fn } {
                // SAFETY: the generator exclusively owns this exact value.
                unsafe { drop(self.output) };
            }
        }
    }
}

/// Consume a nullary once callable without executing its body.
///
/// # Safety
/// The callable must have the compiler's generator body contract: yields write
/// `yielded`, completion writes `returned`, to the same output address before
/// publishing the corresponding status. Both immutable layouts outlive it.
#[no_mangle]
pub unsafe extern "C" fn hew_checked_generator_new(
    callable: *mut HewCallableValue,
    yielded: *const HewValueLayout,
    returned: *const HewValueLayout,
) -> *mut HewCheckedGenerator {
    if callable.is_null() || yielded.is_null() || returned.is_null() {
        std::process::abort();
    }
    // SAFETY: the compiler supplies both exact immutable value layouts.
    let (yield_layout, return_layout) = unsafe { (&*yielded, &*returned) };
    let allocation = Layout::from_size_align(
        yield_layout.size.max(return_layout.size).max(1),
        yield_layout.align.max(return_layout.align),
    )
    .unwrap_or_else(|_| std::process::abort());
    // SAFETY: allocation has nonzero size and valid alignment.
    let output = unsafe { alloc(allocation) };
    if output.is_null() {
        handle_alloc_error(allocation);
    }
    // SAFETY: transfer the caller's unique carrier and invalidate its old slot.
    let callable = unsafe {
        ptr::replace(
            callable,
            HewCallableValue {
                environment: ptr::null_mut(),
                descriptor: ptr::null(),
            },
        )
    };
    Box::into_raw(Box::new(HewCheckedGenerator {
        callable: Some(callable),
        yielded,
        returned,
        allocation,
        output: output.cast(),
        initialized: None,
        relay: Arc::new(WakeRelay::default()),
        state: ptr::null_mut(),
        frame: ptr::null_mut(),
        fault: ptr::null_mut(),
        closed: false,
    }))
}

/// Advance one ready producer or cancel and drain it during source cleanup.
/// Returns the shared `CoroStatus`; Pending requires consumer suspension.
///
/// # Safety
/// The caller exclusively borrows the live generator and retains its parent
/// invocation state through this call. A previous yield must be taken before
/// advancing, except that closing disposes it. Only this call drives the frame.
#[no_mangle]
pub unsafe extern "C" fn hew_checked_generator_poll(
    generator: *mut HewCheckedGenerator,
    parent: *mut HewCoroState,
    closing: bool,
) -> i32 {
    // SAFETY: the checked consumer grants exclusive access for each poll.
    let generator = unsafe { &mut *generator };
    if generator.closed {
        return CoroStatus::Complete as i32;
    }
    if closing && generator.state.is_null() {
        if let Some(mut callable) = generator.callable.take() {
            // SAFETY: the lazy callable has not transferred its environment.
            unsafe { hew_callable_drop(&raw mut callable) };
        }
        generator.closed = true;
        return CoroStatus::Complete as i32;
    }
    // SAFETY: parent is retained for this call; the relay retains its waker.
    let waker = unsafe { hew_coro_state_waker(parent) };
    if waker.is_null() {
        std::process::abort();
    }
    // SAFETY: parent retains the descriptor during this registration.
    generator.relay.inner.lock_or_recover().1 = Some(unsafe { OwnedWaker::retain(&*waker) });
    if generator.state.is_null() {
        let descriptor = WakeRelay::descriptor(&generator.relay);
        // SAFETY: relay and parent token stay live while state retains them.
        generator.state =
            unsafe { hew_coro_state_new(&raw const descriptor, hew_coro_state_token(parent)) };
    }
    if closing {
        // SAFETY: this owner retains the child invocation state.
        unsafe { hew_coro_state_cancel(generator.state) };
    }
    // SAFETY: status is read only while this owner is not executing the frame.
    let before = unsafe { hew_coro_state_status(generator.state) };
    let starting = generator.callable.is_some();
    let resuming_yield = before == CoroStatus::Yielded as i32;
    if resuming_yield {
        if closing {
            // SAFETY: closing consumes any previously published yield.
            unsafe { generator.discard_output() };
        } else if generator.initialized.is_some() {
            std::process::abort();
        }
        // SAFETY: output is uninitialized and this owner resumes exclusively.
        unsafe { hew_coro_state_resume_yield(generator.state) };
    }
    let ready = std::mem::take(&mut generator.relay.inner.lock_or_recover().0);
    if starting || resuming_yield || (before == CoroStatus::Pending as i32 && ready) {
        let previous = current_context();
        let mut context = HewExecutionContext {
            // SAFETY: child state retains its cancellation token.
            cancel_token: unsafe { hew_coro_state_token(generator.state) },
            prev_context: previous,
            ..HewExecutionContext::default()
        };
        let _previous = set_current_context(&raw mut context);
        if let Some(callable) = generator.callable.take() {
            // SAFETY: the compiler's once adapter owns environment cleanup on
            // every terminal path; output/state remain live until frame release.
            generator.frame = unsafe {
                ((*callable.descriptor).invoke_once)(
                    callable.environment,
                    ptr::null(),
                    generator.output,
                    (&raw mut generator.fault).cast(),
                    generator.state.cast(),
                )
            };
        } else {
            // SAFETY: Pending/Yielded frames must remain suspended and live.
            if generator.frame.is_null() || unsafe { hew_cont_done(generator.frame) } {
                std::process::abort();
            }
            // SAFETY: readiness and unique ownership authorize this resume.
            unsafe { hew_cont_resume(generator.frame) };
        }
        let _installed = set_current_context(previous);
        // SAFETY: execution returned; publication precedes this status read.
        let status = unsafe { hew_coro_state_status(generator.state) };
        if status == CoroStatus::Yielded as i32 {
            generator.initialized = Some(generator.yielded);
        } else if status == CoroStatus::Complete as i32 {
            generator.initialized = Some(generator.returned);
        }
    }
    // SAFETY: the frame is no longer executing.
    let status = unsafe { hew_coro_state_status(generator.state) };
    if status != CoroStatus::Pending as i32 && status != CoroStatus::Yielded as i32 {
        if !generator.frame.is_null() {
            // SAFETY: terminal publication must precede LLVM final suspension.
            if !unsafe { hew_cont_done(generator.frame) } {
                std::process::abort();
            }
            // SAFETY: all typed cleanup completed before final suspension.
            unsafe { hew_cont_destroy(generator.frame) };
            generator.frame = ptr::null_mut();
        }
        if closing {
            // SAFETY: close consumes the final return value, if any.
            unsafe { generator.discard_output() };
            generator.closed = true;
            if status == CoroStatus::Cancelled as i32 {
                // SAFETY: this close requested cancellation solely to drain.
                // Retain real producer cleanup failures, not that control marker.
                generator.fault =
                    unsafe { crate::fault::hew_fault_finish_cleanup(generator.fault) };
                return if generator.fault.is_null() {
                    CoroStatus::Complete as i32
                } else {
                    CoroStatus::Fault as i32
                };
            }
        }
    }
    status
}

/// Move the published yield into the consumer's exact destination slot.
///
/// # Safety
/// The generator must have just returned Yielded. `output` is writable storage
/// with its yield layout, currently uninitialized and disjoint from the handle.
#[no_mangle]
pub unsafe extern "C" fn hew_checked_generator_take(
    generator: *mut HewCheckedGenerator,
    output: *mut c_void,
) {
    // SAFETY: the consumer exclusively owns the current published yield.
    let generator = unsafe { &mut *generator };
    if generator.initialized.take() != Some(generator.yielded) {
        std::process::abort();
    }
    // SAFETY: exact size and validity are established by the checked consumer.
    unsafe {
        ptr::copy_nonoverlapping(
            generator.output.cast::<u8>(),
            output.cast::<u8>(),
            (*generator.yielded).size,
        );
    }
}

/// Transfer a terminal producer fault to its checked consumer cleanup edge.
///
/// # Safety
/// The caller exclusively owns a terminal generator and a writable fault slot.
#[no_mangle]
pub unsafe extern "C" fn hew_checked_generator_take_fault(
    generator: *mut HewCheckedGenerator,
    output: *mut *mut HewFault,
) -> i32 {
    // SAFETY: move one fault obligation, leaving the producer slot empty.
    unsafe {
        *output = ptr::replace(&raw mut (*generator).fault, ptr::null_mut());
        (*output).as_ref().map_or(0, HewFault::code)
    }
}

/// Release storage after explicit checked cancellation and drain.
///
/// # Safety
/// This consumes the unique handle. A started producer must already be terminal;
/// invoking synchronous destruction on a pending or yielded frame is invalid.
#[no_mangle]
pub unsafe extern "C" fn hew_checked_generator_free(generator: *mut HewCheckedGenerator) {
    if generator.is_null() {
        return;
    }
    // SAFETY: the caller transfers the unique owning handle.
    let mut generator = unsafe { Box::from_raw(generator) };
    if !generator.frame.is_null() {
        std::process::abort();
    }
    // SAFETY: no frame can access these uniquely owned values or invocation state.
    unsafe {
        if let Some(mut callable) = generator.callable.take() {
            hew_callable_drop(&raw mut callable);
        }
        generator.discard_output();
        hew_fault_drop(generator.fault);
        hew_coro_state_free(generator.state);
        dealloc(generator.output.cast(), generator.allocation);
    }
}

#[cfg(all(test, not(target_arch = "wasm32")))]
#[path = "generator_checked_tests.rs"]
mod tests;
