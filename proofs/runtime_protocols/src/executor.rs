//! The real executor, with only its actor fields and generated-frame ABI hosted.
use crate::cont_tag::ContTag;
use crate::coro_exec;
use crate::runtime_sync::{AtomicBool, AtomicI32, AtomicPtr, AtomicUsize};
use std::ffi::c_void;
use std::ptr;
use std::sync::atomic::Ordering;

pub mod actor {
    use super::*;
    pub struct HewActor {
        pub id: usize,
        pub cont_tag: AtomicI32,
        pub suspended_cont: AtomicPtr<c_void>,
        pub pending_wake: AtomicBool,
        pub checked_invocation: AtomicPtr<c_void>,
    }
    impl HewActor {
        pub fn new() -> Self {
            Self {
                id: 1,
                cont_tag: AtomicI32::new(ContTag::Empty as i32),
                suspended_cont: AtomicPtr::new(ptr::null_mut()),
                pending_wake: AtomicBool::new(false),
                checked_invocation: AtomicPtr::new(ptr::null_mut()),
            }
        }
    }
}
pub mod internal {
    pub mod types {
        pub use crate::cont_tag::ContTag;
    }
}

#[cfg(feature = "executor_loom")]
use loom::cell::UnsafeCell;
#[cfg(not(feature = "executor_loom"))]
use std::cell::UnsafeCell;

struct Frame {
    // A real allocation is released by destruction; tracked accesses also
    // expose concurrent resume/destroy in Loom, without adding a lock or CAS.
    payload: UnsafeCell<Option<Box<u64>>>,
    done: AtomicBool,
    ready_on_resume: bool,
    resumes: AtomicUsize,
    destroys: AtomicUsize,
    raw_frees: AtomicUsize,
}
// Access is checked by Loom; Kani fixtures are sequential. The executor must
// supply exclusive access, rather than the frame monitor supplying a lock.
unsafe impl Sync for Frame {}
impl Frame {
    fn new(ready_on_resume: bool) -> Self {
        Self {
            payload: UnsafeCell::new(Some(Box::new(37))),
            done: AtomicBool::new(false),
            ready_on_resume,
            resumes: AtomicUsize::new(0),
            destroys: AtomicUsize::new(0),
            raw_frees: AtomicUsize::new(0),
        }
    }
    fn payload<T>(&self, f: impl FnOnce(*mut Option<Box<u64>>) -> T) -> T {
        #[cfg(feature = "executor_loom")]
        {
            self.payload.with_mut(f)
        }
        #[cfg(not(feature = "executor_loom"))]
        {
            f(self.payload.get())
        }
    }
    fn address(&self) -> *mut c_void {
        ptr::from_ref(self).cast_mut().cast()
    }
    fn release(&self) {
        self.payload(|payload| unsafe {
            let owned = (*payload).take().expect("executor: frame released twice");
            drop(owned);
        });
    }
    fn live(&self) -> bool {
        self.payload(|payload| unsafe { (*payload).is_some() })
    }
}

pub mod cont {
    use super::*;
    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    pub enum ResumePoll {
        Pending,
        Ready,
    }

    pub unsafe fn hew_cont_resume(handle: *mut c_void) {
        let frame = &*handle.cast::<Frame>();
        assert!(
            !frame.done.load(Ordering::Relaxed),
            "executor: resumed completed frame"
        );
        frame.payload(|payload| {
            let value = (*payload)
                .as_mut()
                .expect("executor: resumed released frame");
            #[cfg(feature = "executor_loom")]
            loom::thread::yield_now();
            **value += 1;
        });
        frame.resumes.fetch_add(1, Ordering::Relaxed);
        frame.done.store(frame.ready_on_resume, Ordering::Relaxed);
    }
    pub unsafe fn hew_cont_poll(handle: *mut c_void, out: *mut c_void) -> ResumePoll {
        assert!(out.is_null(), "executor: unsupported frame output ABI");
        let frame = &*handle.cast::<Frame>();
        assert!(frame.live(), "executor: polled released frame");
        if frame.done.load(Ordering::Relaxed) {
            ResumePoll::Ready
        } else {
            ResumePoll::Pending
        }
    }
    pub unsafe fn hew_cont_destroy(handle: *mut c_void) {
        if !handle.is_null() {
            let frame = &*handle.cast::<Frame>();
            frame.release();
            frame.destroys.fetch_add(1, Ordering::Relaxed);
        }
    }
    pub unsafe fn hew_cont_frame_free(handle: *mut c_void) {
        if !handle.is_null() {
            let frame = &*handle.cast::<Frame>();
            frame.release();
            frame.raw_frees.fetch_add(1, Ordering::Relaxed);
        }
    }
}

fn park(actor: &actor::HewActor, frame: &Frame) {
    assert!(
        coro_exec::begin_park(actor).is_ok(),
        "executor: valid park refused"
    );
    unsafe { coro_exec::finish_park(actor, frame.address()) };
}

#[cfg(kani)]
mod symbolic {
    use super::*;
    fn unexpected_panic(_: std::fmt::Arguments<'_>) -> ! {
        assert!(false, "executor: unexpected panic");
        kani::assume(false);
        loop {}
    }

    #[kani::proof]
    #[kani::stub(core::panicking::panic_fmt, unexpected_panic)]
    fn executor_raw_tag_admission() {
        let raw: i32 = kani::any();
        let occupied: bool = kani::any();
        let frame = Frame::new(false);
        let actor = actor::HewActor::new();
        let slot = if occupied {
            frame.address()
        } else {
            ptr::null_mut()
        };
        actor.cont_tag.store(raw, Ordering::Relaxed);
        actor.suspended_cont.store(slot, Ordering::Relaxed);
        assert_eq!(
            coro_exec::begin_resume(&actor).is_ok(),
            raw == 1,
            "executor: resume admission accepted a non-parked tag"
        );
        assert_eq!(
            actor.cont_tag.load(Ordering::Relaxed),
            if raw == 1 { 2 } else { raw },
            "executor: refused resume changed tag"
        );
        actor.cont_tag.store(raw, Ordering::Relaxed);
        assert_eq!(
            coro_exec::re_arm(&actor).is_ok(),
            raw == 4 && !occupied,
            "executor: re-arm orphaned a live frame or revived the wrong state"
        );
        assert_eq!(
            actor.cont_tag.load(Ordering::Relaxed),
            if raw == 4 && !occupied { 0 } else { raw },
            "executor: refused re-arm changed tag"
        );
        assert_eq!(
            actor.suspended_cont.load(Ordering::Relaxed),
            slot,
            "executor: admission changed frame identity"
        );
        assert!(frame.live(), "executor: admission released a frame");
        kani::cover!(raw < 0 || raw > 4, "executor cover: invalid tag rejected");
        kani::cover!(raw == 1, "executor cover: parked resume admitted");
        kani::cover!(
            raw == 4 && occupied,
            "executor cover: occupied re-arm refused"
        );
        kani::cover!(
            raw == 4 && !occupied,
            "executor cover: empty re-arm admitted"
        );
    }

    #[kani::proof]
    #[kani::stub(core::panicking::panic_fmt, unexpected_panic)]
    fn executor_teardown_respects_checked_owner() {
        let raw: i32 = kani::any();
        let checked: bool = kani::any();
        let occupied: bool = kani::any();
        let frame = Frame::new(false);
        let actor = actor::HewActor::new();
        let slot = if occupied {
            frame.address()
        } else {
            ptr::null_mut()
        };
        actor.cont_tag.store(raw, Ordering::Relaxed);
        actor.suspended_cont.store(slot, Ordering::Relaxed);
        actor.checked_invocation.store(
            if checked {
                frame.address()
            } else {
                ptr::null_mut()
            },
            Ordering::Relaxed,
        );
        let allowed = !checked && (raw == 1 || raw == 3);
        assert_eq!(
            unsafe { coro_exec::destroy_parked(&actor) }.is_ok(),
            allowed,
            "executor: teardown bypassed active resume or checked cleanup"
        );
        assert_eq!(
            actor.cont_tag.load(Ordering::Relaxed),
            if allowed { 4 } else { raw },
            "executor: refused teardown changed lifecycle"
        );
        assert_eq!(
            actor.suspended_cont.load(Ordering::Relaxed),
            if allowed { ptr::null_mut() } else { slot },
            "executor: teardown left a dangling frame slot"
        );
        assert_eq!(
            frame.live(),
            !(allowed && occupied),
            "executor: teardown released wrong owner"
        );
        assert_eq!(
            frame.destroys.load(Ordering::Relaxed),
            usize::from(allowed && occupied),
            "executor: teardown did not destroy exactly one frame"
        );
        assert_eq!(
            frame.raw_frees.load(Ordering::Relaxed),
            0,
            "executor: teardown skipped typed cleanup"
        );
        kani::cover!(
            checked && raw == 1,
            "executor cover: checked owner prevents teardown"
        );
        kani::cover!(
            !checked && raw == 1 && occupied,
            "executor cover: pending abandonment"
        );
        kani::cover!(
            !checked && raw == 3 && occupied,
            "executor cover: completed teardown"
        );
        kani::cover!(
            raw == 2 && occupied,
            "executor cover: running frame protected"
        );
    }

    #[kani::proof]
    #[kani::stub(core::panicking::panic_fmt, unexpected_panic)]
    fn executor_crash_frees_allocation_without_repeating_cleanup() {
        let frame = Frame::new(false);
        let actor = actor::HewActor::new();
        park(&actor, &frame);
        assert!(coro_exec::begin_resume(&actor).is_ok());
        assert!(unsafe { coro_exec::abandon_resuming_after_crash(&actor) }.is_ok());
        assert!(!frame.live(), "executor: crashed frame allocation leaked");
        assert_eq!(
            frame.raw_frees.load(Ordering::Relaxed),
            1,
            "executor: crash allocation not reclaimed once"
        );
        assert_eq!(
            frame.destroys.load(Ordering::Relaxed),
            0,
            "executor: crash repeated suspended cleanup"
        );
        assert!(
            actor.suspended_cont.load(Ordering::Relaxed).is_null(),
            "executor: crash left dangling slot"
        );
        assert!(!unsafe { coro_exec::abandon_resuming_after_crash(&actor) }.is_ok());
        assert!(!unsafe { coro_exec::destroy_parked(&actor) }.is_ok());
        assert!(
            coro_exec::re_arm(&actor).is_ok(),
            "executor: crash prevented next actor turn"
        );
    }

    #[kani::proof]
    #[kani::unwind(6)]
    #[kani::stub(core::panicking::panic_fmt, unexpected_panic)]
    fn executor_repeated_turns_preserve_frame_ownership() {
        let actor = actor::HewActor::new();
        for _ in 0..2 {
            let frame = Frame::new(kani::any());
            park(&actor, &frame);
            let mut live = true;
            let mut complete = false;
            let mut resumes = 0;
            for _ in 0..4 {
                let op: u8 = kani::any();
                kani::assume(op < 3);
                match op {
                    0 => {
                        let result = unsafe { coro_exec::resume_park(&actor) };
                        assert_eq!(
                            result.is_some(),
                            live && !complete,
                            "executor: resumed without a live pending owner"
                        );
                        if live && !complete {
                            resumes += 1;
                            complete = frame.ready_on_resume;
                            assert_eq!(
                                result,
                                Some(if complete {
                                    cont::ResumePoll::Ready
                                } else {
                                    cont::ResumePoll::Pending
                                }),
                                "executor: resume outcome lost"
                            );
                        }
                    }
                    1 => {
                        assert_eq!(
                            unsafe { coro_exec::destroy_parked(&actor) }.is_ok(),
                            live,
                            "executor: repeated destruction accepted or valid owner leaked"
                        );
                        live = false;
                    }
                    _ => {
                        let _ = coro_exec::re_arm(&actor);
                    }
                }
                assert_eq!(frame.live(), live, "executor: frame liveness diverged");
                assert_eq!(
                    frame.resumes.load(Ordering::Relaxed),
                    resumes,
                    "executor: phantom resume"
                );
                assert_eq!(
                    frame.destroys.load(Ordering::Relaxed),
                    usize::from(!live),
                    "executor: release count diverged"
                );
                assert_eq!(
                    actor.suspended_cont.load(Ordering::Relaxed),
                    if live {
                        frame.address()
                    } else {
                        ptr::null_mut()
                    },
                    "executor: frame identity lost across operations"
                );
            }
            if live {
                assert!(unsafe { coro_exec::destroy_parked(&actor) }.is_ok());
            }
            let _ = coro_exec::re_arm(&actor);
            assert_eq!(
                actor.cont_tag.load(Ordering::Relaxed),
                0,
                "executor: next actor turn cannot park"
            );
            assert_eq!(
                frame.destroys.load(Ordering::Relaxed),
                1,
                "executor: actor turn leaked frame"
            );
            assert_eq!(
                frame.raw_frees.load(Ordering::Relaxed),
                0,
                "executor: normal turn skipped cleanup"
            );
        }
    }
}

#[cfg(feature = "executor_loom")]
pub fn verify(selection: Option<&str>) {
    use loom::sync::Arc;
    use loom::thread;
    for scenario in ["resume_destroy", "two_resumes", "two_destroys"] {
        if selection.is_some_and(|selected| selected != scenario) {
            continue;
        }
        for ready in [false, true] {
            println!("Checking executor {scenario}: ready_on_resume={ready}");
            let mut model = loom::model::Builder::new();
            model.preemption_bound = None;
            model.max_permutations = None;
            model.max_duration = None;
            model.checkpoint_file = None;
            model.expect_explicit_explore = false;
            model.check(move || {
                let frame = Arc::new(Frame::new(ready));
                let actor = Arc::new(actor::HewActor::new());
                park(&actor, &frame);
                let mut workers = Vec::new();
                for n in 0..2 {
                    let a = actor.clone();
                    let f = frame.clone();
                    workers.push(thread::spawn(move || {
                        let _keep_frame_live = f;
                        if scenario == "two_destroys" || (scenario == "resume_destroy" && n == 1) {
                            unsafe { coro_exec::destroy_parked(&a) }.is_ok()
                        } else {
                            unsafe { coro_exec::resume_park(&a) }.is_some()
                        }
                    }));
                }
                let successes: usize = workers
                    .into_iter()
                    .map(|worker| usize::from(worker.join().unwrap()))
                    .sum();
                let resumes = frame.resumes.load(Ordering::Relaxed);
                let destroys = frame.destroys.load(Ordering::Relaxed);
                match scenario {
                    "two_destroys" => {
                        assert_eq!(successes, 1, "executor: teardown lost or duplicated");
                        assert_eq!(destroys, 1, "executor: teardown owner missing");
                        assert_eq!(resumes, 0, "executor: teardown resumed a frame");
                    }
                    "two_resumes" => {
                        assert_eq!(resumes, successes, "executor: refused resume touched frame");
                        assert!(
                            resumes >= 1 && resumes <= if ready { 1 } else { 2 },
                            "executor: resume ownership violated"
                        );
                        assert_eq!(destroys, 0, "executor: resume destroyed a frame");
                    }
                    _ => {
                        assert!(
                            resumes <= 1 && destroys <= 1,
                            "executor: frame operation duplicated"
                        );
                        assert_eq!(
                            successes,
                            resumes + destroys,
                            "executor: refused operation touched frame"
                        );
                    }
                }
                assert_eq!(
                    frame.live(),
                    destroys == 0,
                    "executor: frame owner diverged"
                );
                if destroys == 0 {
                    assert!(unsafe { coro_exec::destroy_parked(&actor) }.is_ok());
                }
                assert_eq!(
                    frame.destroys.load(Ordering::Relaxed),
                    1,
                    "executor: frame leaked after race"
                );
                assert!(
                    actor.suspended_cont.load(Ordering::Relaxed).is_null(),
                    "executor: teardown left dangling slot"
                );
                assert!(
                    unsafe { coro_exec::resume_park(&actor) }.is_none(),
                    "executor: resumed after destruction"
                );
            });
        }
        println!("Verified executor {scenario} (all interleavings, pending and ready frames)");
    }
    if selection.is_some_and(|selected| {
        !["resume_destroy", "two_resumes", "two_destroys"].contains(&selected)
    }) {
        panic!("executor: unknown scenario");
    }
}
