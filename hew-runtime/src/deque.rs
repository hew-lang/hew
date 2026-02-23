//! Chase-Lev work-stealing deque wrappers for actor scheduling.
//!
//! Wraps [`crossbeam_deque`] to provide the three work-stealing primitives
//! used by the scheduler:
//!
//! - [`WorkDeque`] — per-worker LIFO push/pop (owner thread only).
//! - [`WorkStealer`] — handle for other threads to steal FIFO from a worker.
//! - [`GlobalQueue`] — shared injector queue for overflow / external submissions.
//!
//! All queues store `*mut ()` because we traffic in raw actor pointers.
//! The scheduler casts them to the appropriate type.

use crossbeam_deque::{Injector, Steal, Stealer, Worker};

/// Per-worker work-stealing deque.
///
/// The owning thread pushes and pops from the bottom (LIFO).
/// Other threads steal from the top (FIFO) via [`WorkStealer`].
#[derive(Debug)]
pub struct WorkDeque {
    worker: Worker<*mut ()>,
}

/// Handle for stealing from another thread's [`WorkDeque`].
///
/// Cloneable — distribute to all stealer threads.
#[derive(Debug, Clone)]
pub struct WorkStealer {
    stealer: Stealer<*mut ()>,
}

/// Global injector queue shared across all worker threads.
///
/// Used for overflow or external work submission. Supports bulk steal
/// into a local [`WorkDeque`].
#[derive(Debug)]
pub struct GlobalQueue {
    injector: Injector<*mut ()>,
}

// SAFETY: The raw pointers stored in the queues are opaque handles managed
// by the scheduler. crossbeam-deque itself is `Send + Sync`, and we only
// add a thin wrapper. The scheduler is responsible for pointer validity.
unsafe impl Send for WorkDeque {}
// SAFETY: `WorkStealer` wraps `crossbeam_deque::Stealer` which is already
// `Send + Sync`. The `*mut ()` values are opaque scheduler-managed handles.
unsafe impl Send for WorkStealer {}
// SAFETY: `Stealer::steal` is safe to call from multiple threads concurrently.
unsafe impl Sync for WorkStealer {}
// SAFETY: `Injector` is already `Send + Sync`; the `*mut ()` payloads are
// scheduler-managed.
unsafe impl Send for GlobalQueue {}
// SAFETY: `Injector::push` and `steal_batch_and_pop` are safe for concurrent use.
unsafe impl Sync for GlobalQueue {}

impl WorkDeque {
    /// Creates a new work-stealing deque and its corresponding stealer handle.
    ///
    /// # Safety
    ///
    /// The caller must ensure that all `*mut ()` values pushed into the deque
    /// remain valid until they are popped or stolen.
    #[must_use]
    pub unsafe fn new() -> (Self, WorkStealer) {
        let worker = Worker::new_lifo();
        let stealer = worker.stealer();
        (Self { worker }, WorkStealer { stealer })
    }

    /// Pushes a raw pointer onto the bottom of the deque (owner thread only).
    pub fn push(&self, ptr: *mut ()) {
        self.worker.push(ptr);
    }

    /// Pops a raw pointer from the bottom of the deque (owner thread, LIFO).
    ///
    /// Returns `None` if the deque is empty.
    #[must_use]
    pub fn pop(&self) -> Option<*mut ()> {
        self.worker.pop()
    }

    /// Returns `true` if the deque is empty.
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.worker.is_empty()
    }
}

impl WorkStealer {
    /// Steals a raw pointer from the top of the associated deque (FIFO).
    ///
    /// Returns `None` if the deque is empty or the steal was contended.
    #[must_use]
    pub fn steal(&self) -> Option<*mut ()> {
        loop {
            match self.stealer.steal() {
                Steal::Success(ptr) => return Some(ptr),
                Steal::Empty => return None,
                Steal::Retry => {}
            }
        }
    }

    /// Returns `true` if the associated deque is empty.
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.stealer.is_empty()
    }
}

impl GlobalQueue {
    /// Creates a new global injector queue.
    ///
    /// # Safety
    ///
    /// The caller must ensure that all `*mut ()` values pushed into the queue
    /// remain valid until they are consumed.
    #[must_use]
    pub unsafe fn new() -> Self {
        Self {
            injector: Injector::new(),
        }
    }

    /// Pushes a raw pointer into the global queue.
    pub fn push(&self, ptr: *mut ()) {
        self.injector.push(ptr);
    }

    /// Steals a batch of work from the global queue into `dest`, returning
    /// one item immediately.
    ///
    /// Transfers roughly half the global queue into the local deque and
    /// returns one element to the caller.
    #[must_use]
    pub fn steal_batch_and_pop(&self, dest: &WorkDeque) -> Option<*mut ()> {
        loop {
            match self.injector.steal_batch_and_pop(&dest.worker) {
                Steal::Success(ptr) => return Some(ptr),
                Steal::Empty => return None,
                Steal::Retry => {}
            }
        }
    }

    /// Returns `true` if the global queue is empty.
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.injector.is_empty()
    }

    /// Returns the approximate number of items in the global queue.
    #[must_use]
    pub fn len(&self) -> usize {
        self.injector.len()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Arc;
    use std::thread;

    #[test]
    fn work_deque_push_pop_lifo() {
        // SAFETY: test pointers are just integers cast to *mut ().
        let (deque, _stealer) = unsafe { WorkDeque::new() };

        deque.push(1_usize as *mut ());
        deque.push(2_usize as *mut ());
        deque.push(3_usize as *mut ());

        assert_eq!(deque.pop(), Some(3_usize as *mut ()));
        assert_eq!(deque.pop(), Some(2_usize as *mut ()));
        assert_eq!(deque.pop(), Some(1_usize as *mut ()));
        assert_eq!(deque.pop(), None);
    }

    #[test]
    fn work_stealer_fifo() {
        // SAFETY: test pointers.
        let (deque, stealer) = unsafe { WorkDeque::new() };

        deque.push(1_usize as *mut ());
        deque.push(2_usize as *mut ());
        deque.push(3_usize as *mut ());

        assert_eq!(stealer.steal(), Some(1_usize as *mut ()));
        assert_eq!(stealer.steal(), Some(2_usize as *mut ()));
        assert_eq!(stealer.steal(), Some(3_usize as *mut ()));
        assert_eq!(stealer.steal(), None);
    }

    #[test]
    fn empty_deque_returns_none() {
        let (deque, stealer) = unsafe { WorkDeque::new() };
        assert!(deque.is_empty());
        assert_eq!(deque.pop(), None);
        assert_eq!(stealer.steal(), None);
    }

    #[test]
    fn global_queue_inject_and_steal() {
        let global = unsafe { GlobalQueue::new() };
        let (deque, _stealer) = unsafe { WorkDeque::new() };

        global.push(10_usize as *mut ());
        global.push(20_usize as *mut ());
        global.push(30_usize as *mut ());

        // Steal batch into local deque.
        let first = global.steal_batch_and_pop(&deque);
        assert!(first.is_some());

        // Drain anything moved into the local deque.
        let mut all = vec![first.unwrap()];
        while let Some(ptr) = deque.pop() {
            all.push(ptr);
        }
        // May also have remaining in global.
        while let Some(ptr) = global.steal_batch_and_pop(&deque) {
            all.push(ptr);
            while let Some(ptr) = deque.pop() {
                all.push(ptr);
            }
        }

        // All three items should be accounted for.
        all.sort();
        let mut expected = vec![
            10_usize as *mut (),
            20_usize as *mut (),
            30_usize as *mut (),
        ];
        expected.sort();
        assert_eq!(all, expected);
    }

    #[test]
    fn concurrent_steal_no_duplicates() {
        const NUM_ITEMS: usize = 10_000;
        const NUM_STEALERS: usize = 4;

        // SAFETY: test pointers.
        let (deque, stealer) = unsafe { WorkDeque::new() };

        for i in 0..NUM_ITEMS {
            deque.push(i as *mut ());
        }

        let stealer = Arc::new(stealer);
        let stolen_counts: Vec<_> = (0..NUM_STEALERS)
            .map(|_| {
                let stealer = Arc::clone(&stealer);
                thread::spawn(move || {
                    let mut count = 0usize;
                    loop {
                        match stealer.steal() {
                            Some(_) => count += 1,
                            None => {
                                // Retry a few times.
                                let mut got_more = false;
                                for _ in 0..100 {
                                    if stealer.steal().is_some() {
                                        count += 1;
                                        got_more = true;
                                        break;
                                    }
                                }
                                if !got_more {
                                    break;
                                }
                            }
                        }
                    }
                    count
                })
            })
            .collect();

        // Owner also pops.
        let mut owner_count = 0usize;
        while deque.pop().is_some() {
            owner_count += 1;
        }

        let total_stolen: usize = stolen_counts
            .into_iter()
            .map(|h| h.join().expect("stealer panicked"))
            .sum();

        assert_eq!(
            owner_count + total_stolen,
            NUM_ITEMS,
            "items lost or duplicated"
        );
    }
}
