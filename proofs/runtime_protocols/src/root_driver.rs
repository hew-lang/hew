//! Caller-side ownership proof for the real shared root poll function.
//!
//! Only generated-frame operations and opaque state reads are modeled. The
//! frame really is allocated/deallocated in Kani's memory model. Completion
//! and the public outcome vary independently, including malformed producer
//! combinations: abort is allowed only when the independent table says so.
use crate::coro_state::CoroStatus;
use std::cell::UnsafeCell;
use std::ffi::c_void;

pub struct HewCoroState {
    status: u8,
    private: i32,
}

struct Frame {
    done: bool,
}

struct Boundary {
    state: *mut HewCoroState,
    live: bool,
    ready: bool,
    next_status: u8,
    next_done: bool,
    next_private: i32,
    resumes: u8,
    destroys: u8,
    expected_abort: bool,
}
struct Slot(UnsafeCell<Boundary>);
// Every harness is single-threaded; this is the explicit FFI callback context.
unsafe impl Sync for Slot {}
static BOUNDARY: Slot = Slot(UnsafeCell::new(Boundary {
    state: std::ptr::null_mut(),
    live: false,
    ready: false,
    next_status: 0,
    next_done: false,
    next_private: 0,
    resumes: 0,
    destroys: 0,
    expected_abort: false,
}));

pub unsafe fn hew_coro_state_status(state: *mut HewCoroState) -> i32 {
    i32::from((*state).status)
}

pub unsafe fn hew_coro_state_private_status(state: *mut HewCoroState) -> i32 {
    let b = &*BOUNDARY.0.get();
    assert!(!b.live, "root: read result before retiring the frame");
    (*state).private
}

pub mod cont {
    use super::*;

    pub unsafe fn hew_cont_done(frame: *mut c_void) -> bool {
        assert!((*BOUNDARY.0.get()).live, "root: accessed released frame");
        (*frame.cast::<Frame>()).done
    }

    pub unsafe fn hew_cont_resume(frame: *mut c_void) {
        let b = &mut *BOUNDARY.0.get();
        assert!(b.live, "root: resumed released frame");
        assert!(b.ready, "root: resumed without consumed readiness");
        assert!(
            !(*frame.cast::<Frame>()).done,
            "root: resumed completed frame"
        );
        assert_eq!(
            (*b.state).status,
            CoroStatus::Pending as u8,
            "root: resumed terminal invocation"
        );
        b.resumes += 1;
        // The compiler/runtime contract keeps storage live across resume. Do
        // not assume that outcome publication and the prefix agree: check the
        // driver's behavior for every combination of these two observations.
        (*b.state).status = b.next_status;
        (*b.state).private = b.next_private;
        (*frame.cast::<Frame>()).done = b.next_done;
    }

    pub unsafe fn hew_cont_destroy(frame: *mut c_void) {
        let b = &mut *BOUNDARY.0.get();
        assert!(b.live, "root: destroyed released frame");
        assert!(
            (*frame.cast::<Frame>()).done,
            "root: destroyed unfinished frame"
        );
        assert!(
            matches!((*b.state).status, 1..=3),
            "root: destroyed invocation without terminal publication"
        );
        b.destroys += 1;
        // This is a real deallocation, not just a logical reference counter.
        drop(Box::from_raw(frame.cast::<Frame>()));
        b.live = false;
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Expected {
    Wait,
    Resume,
    Finish,
    ResumeFinish,
    Reject,
}

// Declarative boundary contract. Rows describe allowed observable behavior,
// rather than executing or extracting the implementation's branches.
fn expectation(status: u8, present: bool, done: bool, ready: bool) -> Expected {
    use Expected::*;
    match (status, present, done, ready) {
        (0, true, false, false) => Wait,
        (0, true, false, true) => Resume,
        (1..=3, false, _, _) | (1..=3, true, true, _) => Finish,
        _ => Reject,
    }
}

fn after_resume(status: u8, done: bool) -> Expected {
    use Expected::*;
    match (status, done) {
        (0, _) => Resume,
        (1..=3, true) => ResumeFinish,
        _ => Reject,
    }
}

fn expected_abort() -> ! {
    unsafe {
        assert!(
            (*BOUNDARY.0.get()).expected_abort,
            "root: valid boundary unexpectedly aborted"
        );
        kani::cover!(true, "root cover: rejected invalid protocol");
    }
    // Abort terminates the production process. Prune only after checking that
    // this execution is required to reject; a returned invalid case fails in
    // the harness below. Neither unexpected abort nor missing abort can pass.
    kani::assume(false);
    loop {}
}

fn unexpected_panic(_: std::fmt::Arguments<'_>) -> ! {
    assert!(false, "root: unexpected panic");
    kani::assume(false);
    loop {}
}

#[kani::proof]
#[kani::unwind(3)]
#[kani::stub(std::process::abort, expected_abort)]
#[kani::stub(core::panicking::panic_fmt, unexpected_panic)]
fn root_driver_preserves_frame_ownership_and_readiness() {
    let mut state = HewCoroState {
        status: kani::any(),
        private: kani::any(),
    };
    kani::assume(state.status <= 4);
    let present: bool = kani::any();
    let done: bool = kani::any();
    let mut frame = if present {
        Box::into_raw(Box::new(Frame { done })).cast::<c_void>()
    } else {
        std::ptr::null_mut()
    };
    unsafe {
        (*BOUNDARY.0.get()).state = &raw mut state;
        (*BOUNDARY.0.get()).live = present;
    }

    // Two arbitrary consecutive polls: the second also checks the concrete
    // freed/null owner produced by the first, not a reconstructed fake owner.
    let mut retired = false;
    for _ in 0..2 {
        let ready: bool = kani::any();
        let next_status: u8 = kani::any();
        kani::assume(next_status <= 4);
        let next_done: bool = kani::any();
        let next_private: i32 = kani::any();
        let old_frame = frame;
        let old_status = state.status;
        let old_done = if frame.is_null() {
            false
        } else {
            unsafe { (*frame.cast::<Frame>()).done }
        };
        let entry = expectation(old_status, !frame.is_null(), old_done, ready);
        let expected = if entry == Expected::Resume {
            after_resume(next_status, next_done)
        } else {
            entry
        };
        let private = state.private;
        unsafe {
            let b = &mut *BOUNDARY.0.get();
            b.ready = ready;
            b.next_status = next_status;
            b.next_done = next_done;
            b.next_private = next_private;
            b.resumes = 0;
            b.destroys = 0;
            b.expected_abort = expected == Expected::Reject;
        }

        let actual = unsafe { crate::root_poll::poll_root(&mut frame, &raw mut state, ready) };
        assert_ne!(
            expected,
            Expected::Reject,
            "root: invalid boundary returned"
        );
        kani::cover!(
            expected == Expected::Wait,
            "root cover: waiting for readiness"
        );
        kani::cover!(
            expected == Expected::Resume,
            "root cover: resumed and pending"
        );
        kani::cover!(
            expected == Expected::Finish,
            "root cover: already completed"
        );
        kani::cover!(
            expected == Expected::ResumeFinish,
            "root cover: completed during resume"
        );
        kani::cover!(
            expected == Expected::Finish && old_frame.is_null(),
            "root cover: terminal null owner"
        );
        kani::cover!(
            retired && expected == Expected::Finish && old_frame.is_null(),
            "root cover: poll after concrete destruction"
        );
        let finished = matches!(expected, Expected::Finish | Expected::ResumeFinish);
        let resumed = matches!(expected, Expected::Resume | Expected::ResumeFinish);
        assert_eq!(
            actual,
            if finished {
                Some(if resumed { next_private } else { private })
            } else {
                None
            },
            "root: wrong result or readiness decision"
        );
        unsafe {
            let b = &*BOUNDARY.0.get();
            assert_eq!(b.resumes, u8::from(resumed), "root: wrong resume count");
            assert_eq!(
                b.destroys,
                u8::from(finished && !old_frame.is_null()),
                "root: lost or duplicated destruction"
            );
            assert_eq!(b.live, !finished, "root: wrong surviving frame owner");
            retired |= b.destroys != 0;
        }
        assert_eq!(
            frame,
            if finished {
                std::ptr::null_mut()
            } else {
                old_frame
            },
            "root: dangling or replaced frame slot"
        );
        assert_eq!(
            state.status,
            if resumed { next_status } else { old_status },
            "root: poll altered outcome outside resume"
        );
    }
    if !frame.is_null() {
        // Harness cleanup of a still-suspended owner; no claim about generated
        // language-value finalizers is made by this storage model.
        unsafe { drop(Box::from_raw(frame.cast::<Frame>())) };
    }
}
