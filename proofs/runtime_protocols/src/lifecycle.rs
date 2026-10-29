//! Resource conservation across production lifecycle operations.
use crate::wake::HewWaker;
use loom::sync::atomic::{AtomicUsize, Ordering};
use loom::thread;
use std::ffi::c_void;
use std::sync::Arc;

fn model() -> loom::model::Builder {
    let mut model = loom::model::Builder::new();
    model.preemption_bound = Some(2);
    model.max_permutations = None;
    model.max_duration = None;
    model.checkpoint_file = None;
    model.expect_explicit_explore = false;
    model
}
unsafe extern "C" fn retain(p: *mut c_void) {
    Arc::increment_strong_count(p.cast::<AtomicUsize>());
}
unsafe extern "C" fn release(p: *mut c_void) {
    Arc::decrement_strong_count(p.cast::<AtomicUsize>());
}
unsafe extern "C" fn wake(p: *mut c_void) {
    (&*p.cast::<AtomicUsize>()).fetch_add(1, Ordering::SeqCst);
}
fn descriptor(trace: &Arc<AtomicUsize>) -> HewWaker {
    HewWaker {
        context: Arc::as_ptr(trace).cast_mut().cast(),
        retain,
        release,
        wake,
    }
}

fn semaphore(abandon: bool, timed: bool) {
    model().check(move || unsafe {
        use crate::semaphore::*;
        use crate::timer_wheel::*;
        crate::clock::NOW.store(0, std::sync::atomic::Ordering::Relaxed);
        let trace = Arc::new(AtomicUsize::new(0));
        let wheel = hew_timer_wheel_new();
        crate::timer_periodic::WHEEL.store(wheel, std::sync::atomic::Ordering::Relaxed);
        let sem = hew_semaphore_new(0);
        // One queued operation, followed by a live successor. The operations
        // must retain backing after the creator's handle is dropped.
        let first = hew_semaphore_acquire_new(sem, if timed { 1 } else { -1 }, &descriptor(&trace));
        let second = hew_semaphore_acquire_new(sem, -1, &descriptor(&trace));
        let releaser = hew_semaphore_clone(sem) as usize;
        hew_semaphore_free(sem);
        let a = first as usize;
        let worker = thread::spawn(move || {
            if abandon {
                hew_semaphore_acquire_free(a as *mut _);
                0
            } else {
                let taken = if hew_semaphore_acquire_status(a as *mut _) != 0 {
                    hew_semaphore_acquire_take(a as *mut _)
                } else {
                    0
                };
                hew_semaphore_acquire_free(a as *mut _);
                taken
            }
        });
        let release_thread = thread::spawn(move || hew_semaphore_release(releaser as *mut _));
        let w = wheel as usize;
        let ticker = thread::spawn(move || {
            if timed {
                timer_wheel_tick_to(w as *mut _, 1);
            }
        });
        let taken = worker.join().unwrap();
        release_thread.join().unwrap();
        ticker.join().unwrap();
        assert_eq!(
            hew_semaphore_acquire_status(second),
            if taken == 0 { 1 } else { 0 },
            "lifecycle: live semaphore waiter was stranded despite an available permit"
        );
        assert_eq!(
            hew_semaphore_count(releaser as *mut _),
            0,
            "lifecycle: semaphore parked a waiter while retaining a permit"
        );
        let second_taken = if hew_semaphore_acquire_status(second) != 0 {
            hew_semaphore_acquire_take(second)
        } else {
            0
        };
        hew_semaphore_acquire_free(second);
        assert_eq!(
            taken + second_taken + hew_semaphore_count(releaser as *mut _),
            1,
            "lifecycle: semaphore lost or duplicated a released permit"
        );
        assert!(
            trace.load(Ordering::SeqCst) <= 2,
            "lifecycle: semaphore issued duplicate completion notifications"
        );
        assert_eq!(
            Arc::strong_count(&trace),
            1,
            "lifecycle: semaphore leaked a readiness owner"
        );
        assert_eq!(
            hew_timer_wheel_next_deadline_ms(wheel),
            -1,
            "lifecycle: semaphore teardown left a live deadline"
        );
        hew_semaphore_free(releaser as *mut _);
        crate::timer_periodic::WHEEL
            .store(std::ptr::null_mut(), std::sync::atomic::Ordering::Relaxed);
        hew_timer_wheel_free(wheel);
    });
}

fn supervisor() {
    model().check(|| {
        let pins = Arc::new(crate::pin_state::PinState::new());
        let accesses = Arc::new(AtomicUsize::new(0));
        let destroyed = Arc::new(AtomicUsize::new(0));
        let mut workers = Vec::new();
        for _ in 0..2 {
            let (pins, accesses, destroyed) = (pins.clone(), accesses.clone(), destroyed.clone());
            workers.push(thread::spawn(move || {
                if pins.try_pin() {
                    accesses.fetch_add(1, Ordering::SeqCst);
                    assert!(
                        !pins.is_closed_and_drained(),
                        "lifecycle: supervisor reported drained with a live pin"
                    );
                    assert_eq!(
                        destroyed.load(Ordering::SeqCst),
                        0,
                        "lifecycle: supervisor reclaimed during access"
                    );
                    accesses.fetch_sub(1, Ordering::SeqCst);
                    pins.release_pin();
                }
            }));
        }
        let close_winners = Arc::new(AtomicUsize::new(0));
        let mut closers = Vec::new();
        for _ in 0..2 {
            let (closing, live, freed, winners) = (
                pins.clone(),
                accesses.clone(),
                destroyed.clone(),
                close_winners.clone(),
            );
            closers.push(thread::spawn(move || {
                if closing.begin_close() {
                    winners.fetch_add(1, Ordering::SeqCst);
                }
                assert!(
                    !closing.try_pin(),
                    "lifecycle: closed supervisor admitted new access"
                );
                if closing.is_closed_and_drained() {
                    assert_eq!(
                        live.load(Ordering::SeqCst),
                        0,
                        "lifecycle: drain preceded pin release"
                    );
                    if closing.claim_teardown() {
                        assert_eq!(
                            freed.fetch_add(1, Ordering::SeqCst),
                            0,
                            "lifecycle: duplicate supervisor destructor authority"
                        );
                    }
                }
            }));
        }
        for worker in workers {
            worker.join().unwrap();
        }
        for closer in closers {
            closer.join().unwrap();
        }
        assert_eq!(
            close_winners.load(Ordering::SeqCst),
            1,
            "lifecycle: supervisor close had zero or multiple winners"
        );
        assert!(
            pins.is_closed_and_drained(),
            "lifecycle: supervisor never drained after pin release"
        );
        if destroyed.load(Ordering::SeqCst) == 0 {
            assert!(pins.claim_teardown());
            destroyed.fetch_add(1, Ordering::SeqCst);
        }
        assert_eq!(destroyed.load(Ordering::SeqCst), 1);
        // A distinct, unreclaimed allocation exercises failed ownership handoff.
        let retry = crate::pin_state::PinState::new();
        assert!(retry.begin_close());
        assert!(retry.claim_teardown());
        retry.release_teardown();
        assert!(
            !retry.try_pin(),
            "lifecycle: teardown rollback reopened supervisor admission"
        );
        assert!(retry.claim_teardown());
    });
}

fn budget_aliases(bytes: u64, limit: u64) {
    model().check(move || {
        use crate::managed_budget::{ManagedStorage, ValueOwner};
        let owner = ValueOwner::new(Some(limit));
        let backing = ManagedStorage {
            identity: 11,
            bytes,
        };
        // These are the admission/guard APIs used by Rc, buffers, streams,
        // and replies. No utility-only transfer wrapper participates.
        let first_owner = owner.clone();
        let first = thread::spawn(move || match first_owner.acquire(&[backing]) {
            Ok(guard) => {
                guard.commit();
                true
            }
            Err(_) => false,
        });
        let second_owner = owner.clone();
        let second = thread::spawn(move || match second_owner.acquire(&[backing]) {
            Ok(guard) => {
                guard.commit();
                true
            }
            Err(_) => false,
        });
        let one = first.join().unwrap();
        let two = second.join().unwrap();
        assert_eq!(
            one && two,
            bytes <= limit,
            "lifecycle: managed admission misapplied payload quota"
        );
        if bytes <= limit {
            assert_eq!(
                owner.live_bytes(),
                bytes,
                "lifecycle: aliases duplicated a backing's charge"
            );
            owner.release(&[backing]);
            assert_eq!(
                owner.live_bytes(),
                bytes,
                "lifecycle: alias release refunded shared storage"
            );
            owner.release(&[backing]);
        } else {
            assert!(!one && !two);
            assert_eq!(
                owner.live_bytes(),
                0,
                "lifecycle: refused admission changed account ownership"
            );
        }
        assert_eq!(
            owner.live_bytes(),
            0,
            "lifecycle: managed final release leaked quota"
        );
        assert!(
            owner.ledger().storage.is_empty(),
            "lifecycle: managed final release retained an alias or refused candidate"
        );
    });
}

fn budget_rollback() {
    model().check(|| {
        use crate::managed_budget::{ManagedStorage, ValueOwner};
        let owner = ValueOwner::new(Some(16));
        let backing = ManagedStorage {
            identity: 1,
            bytes: 8,
        };
        owner.acquire(&[backing]).unwrap().commit();
        drop(owner.acquire(&[
            backing,
            ManagedStorage {
                identity: 3,
                bytes: 4,
            },
        ]));
        assert_eq!(
            owner.live_bytes(),
            8,
            "lifecycle: abandoned acquisition leaked a backing"
        );
        let other = owner.clone();
        let reserve = thread::spawn(move || {
            let guard = other.reserve_new(8);
            drop(guard); // cancelled construction must refund reservation
        });
        let other = owner.clone();
        let growth = thread::spawn(move || {
            let guard = other.prepare_backing_change(Some(backing), 16, true);
            drop(guard); // cancelled growth must preserve the old backing
        });
        reserve.join().unwrap();
        growth.join().unwrap();
        assert_eq!(
            owner.live_bytes(),
            8,
            "lifecycle: abandoned managed construction changed live ownership"
        );
        owner
            .reserve_new(8)
            .expect("lifecycle: abandoned managed reservation leaked quota")
            .commit(&[ManagedStorage {
                identity: 2,
                bytes: 8,
            }]);
        owner.release(&[
            backing,
            ManagedStorage {
                identity: 2,
                bytes: 8,
            },
        ]);
        assert_eq!(owner.live_bytes(), 0);
    });
}

fn read_slot(attached: bool) {
    model().check(move || unsafe {
        use crate::read_slot::*;
        let slot = hew_read_slot_new();
        crate::resume::WAKES.store(0, std::sync::atomic::Ordering::Relaxed);
        let mut actor = crate::actor::HewActor { id: 1 };
        let registration = if attached {
            let reg = crate::await_cancel::hew_await_cancel_new(
                &mut actor,
                Some(hew_read_slot_cancel_cleanup),
                slot.cast(),
            );
            hew_read_slot_set_await_cancel(slot, reg);
            reg
        } else {
            std::ptr::null_mut()
        };
        read_slot_retain(slot);
        read_slot_retain(slot);
        let a = slot as usize;
        let first = thread::spawn(move || {
            let won = read_slot_deposit_status(a as *mut _, ReadStatus::Data);
            hew_read_slot_free(a as *mut _);
            won
        });
        let a = slot as usize;
        let second = thread::spawn(move || {
            let won = read_slot_deposit_status(a as *mut _, ReadStatus::Eof);
            hew_read_slot_free(a as *mut _);
            won
        });
        if attached {
            crate::await_cancel::hew_await_cancel_cancel(registration, 2, 1);
        } else {
            hew_read_slot_cancel(slot);
        }
        let one = first.join().unwrap();
        let two = second.join().unwrap();
        let status = hew_read_slot_status(slot);
        assert!(
            !(one && two),
            "lifecycle: two readiness depositors gained wake authority"
        );
        if attached {
            let outcome = crate::await_cancel::hew_await_cancel_status(registration);
            assert_eq!(
                one || two,
                outcome == 1,
                "lifecycle: read-slot notification disagrees with common await authority"
            );
            assert_eq!(
                usize::from(one)
                    + usize::from(two)
                    + crate::resume::WAKES.load(std::sync::atomic::Ordering::Relaxed),
                1,
                "lifecycle: read-slot cancellation/completion lost or duplicated its wake"
            );
        } else {
            assert_eq!(
                one || two,
                status == 1 || status == 2,
                "lifecycle: read-slot terminal outcome disagrees with wake authority"
            );
        }
        assert!(
            status == 4 || status == 1 || status == 2,
            "lifecycle: read-slot cancellation lost all terminal outcomes"
        );
        assert!(
            !read_slot_deposit_status(slot, ReadStatus::Error),
            "lifecycle: cancelled/ready slot accepted late publication"
        );
        hew_read_slot_free(slot);
        if attached {
            crate::await_cancel::hew_await_cancel_free(registration);
        }
    });
}

pub fn verify(name: &str) {
    match name {
        "semaphore" => {
            for abandon in [false, true] {
                for timed in [false, true] {
                    semaphore(abandon, timed);
                }
            }
        }
        "supervisor" => supervisor(),
        "read_slot" => {
            read_slot(false);
            read_slot(true);
        }
        "managed_budget" => {
            for (bytes, limit) in [
                (0, 0),
                (8, 8),
                (9, 8),
                (u64::MAX, u64::MAX),
                (u64::MAX, u64::MAX - 1),
            ] {
                budget_aliases(bytes, limit);
            }
            budget_rollback();
        }
        _ => panic!("unknown lifecycle contract: {name}"),
    }
    println!("Verified lifecycle {name} (two preemptions)");
}
