//! Lock-free MPSC (Multiple Producer, Single Consumer) queue.
//!
//! This is an intrusive Vyukov-style MPSC queue. Multiple threads can push
//! concurrently (lock-free via atomic exchange), while a single consumer pops
//! (wait-free in the common case).
//!
//! The queue uses a stub sentinel node to avoid null-pointer edge cases.
//! When the consumer drains the last real node, the stub is re-injected so
//! the tail always has a successor to advance to.
//!
//! # Memory ordering
//!
//! - **push**: `AcqRel` on exchange (makes node data visible to consumer),
//!   `Release` on linking `prev.next`.
//! - **pop**: `Acquire` on loading `next` (sees producer's writes).

use std::ptr;
use std::sync::atomic::{AtomicPtr, Ordering};

/// Intrusive MPSC node.
///
/// Embed this as the first field of your message struct so that a
/// `*mut MpscNode` can be cast to `*mut YourMessage`.
#[repr(C)]
#[derive(Debug)]
pub struct MpscNode {
    /// Pointer to the next node in the queue.
    pub next: AtomicPtr<MpscNode>,
}

impl MpscNode {
    /// Creates a new node with `next` set to null.
    #[must_use]
    pub const fn new() -> Self {
        Self {
            next: AtomicPtr::new(ptr::null_mut()),
        }
    }
}

impl Default for MpscNode {
    fn default() -> Self {
        Self::new()
    }
}

/// Result of a [`MpscQueue::pop`] operation.
#[derive(Debug, PartialEq, Eq)]
pub enum PopResult {
    /// Successfully dequeued a node.
    Success(*mut MpscNode),
    /// Queue is empty.
    Empty,
    /// A push is in-flight (producer has exchanged head but not yet linked
    /// `prev.next`). The caller should retry shortly.
    Inconsistent,
}

/// Vyukov-style lock-free MPSC queue.
///
/// Only the consumer thread may call [`pop`](MpscQueue::pop) or
/// [`flush`](MpscQueue::flush). Any thread may call
/// [`push`](MpscQueue::push).
///
/// # Safety
///
/// This queue stores raw pointers. The caller is responsible for ensuring
/// that pushed nodes remain valid until they are popped.
pub struct MpscQueue {
    /// Producers push here via atomic exchange.
    head: AtomicPtr<MpscNode>,
    /// Consumer pops from here. Only accessed by the consumer thread.
    tail: *mut MpscNode,
    /// Heap-allocated sentinel node. Must be on the heap so that pointers
    /// to it remain stable when the `MpscQueue` itself is moved.
    stub: *mut MpscNode,
}

impl std::fmt::Debug for MpscQueue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("MpscQueue")
            .field("head", &self.head.load(Ordering::Relaxed))
            .field("tail", &self.tail)
            .field("stub", &self.stub)
            .finish()
    }
}

impl Drop for MpscQueue {
    fn drop(&mut self) {
        // SAFETY: `self.stub` was allocated via `Box::into_raw` in `new()`.
        unsafe {
            drop(Box::from_raw(self.stub));
        }
    }
}

impl MpscQueue {
    /// Creates a new empty MPSC queue.
    ///
    /// The stub sentinel is heap-allocated so that pointers remain stable.
    #[must_use]
    pub fn new() -> Self {
        let stub = Box::into_raw(Box::new(MpscNode::new()));
        Self {
            head: AtomicPtr::new(stub),
            tail: stub,
            stub,
        }
    }

    /// Returns a raw pointer to the stub sentinel.
    fn stub_ptr(&self) -> *mut MpscNode {
        self.stub
    }

    /// Pushes a node onto the queue.
    ///
    /// This is lock-free and may be called from any thread.
    ///
    /// # Safety
    ///
    /// `node` must point to a valid, properly aligned `MpscNode` that will
    /// remain live until it is popped.
    pub unsafe fn push(&self, node: *mut MpscNode) {
        // SAFETY: Caller guarantees `node` is valid.
        unsafe {
            (*node).next.store(ptr::null_mut(), Ordering::Relaxed);
        }

        // Atomically swap head to our node. AcqRel ensures:
        //  - Release: node data visible to consumer
        //  - Acquire: we see previous head for linking
        let prev = self.head.swap(node, Ordering::AcqRel);

        // Link previous head to this node. Release pairs with consumer's
        // Acquire load of `next`.
        // SAFETY: `prev` was either the stub or a previously pushed valid node.
        unsafe {
            (*prev).next.store(node, Ordering::Release);
        }
    }

    /// Pops a node from the queue (consumer only).
    ///
    /// # Safety
    ///
    /// Must only be called from the single consumer thread.
    pub unsafe fn pop(&mut self) -> PopResult {
        let tail = self.tail;
        // SAFETY: `tail` is always a valid pointer (stub or a pushed node).
        let next = unsafe { (*tail).next.load(Ordering::Acquire) };

        let stub = self.stub_ptr();

        // Skip past the stub sentinel.
        if tail == stub {
            if next.is_null() {
                return PopResult::Empty;
            }
            self.tail = next;
            // SAFETY: `next` was loaded from stub.next and is a valid node.
            return unsafe { self.pop_inner(next) };
        }

        if !next.is_null() {
            // Common case: tail has a successor, return tail.
            self.tail = next;
            return PopResult::Success(tail);
        }

        // tail.next is null. Check if a push is in-flight.
        let head = self.head.load(Ordering::Acquire);
        if tail != head {
            // A producer has exchanged head but hasn't linked prev.next yet.
            return PopResult::Inconsistent;
        }

        // tail == head: last real node. Re-inject stub as sentinel.
        // SAFETY: stub is heap-allocated and always valid.
        unsafe {
            self.push(stub);
        }

        // SAFETY: tail is valid (same pointer we checked above).
        let next = unsafe { (*tail).next.load(Ordering::Acquire) };
        if !next.is_null() {
            self.tail = next;
            return PopResult::Success(tail);
        }

        // The push of stub hasn't completed linking yet.
        PopResult::Inconsistent
    }

    /// Inner helper: after advancing past stub, retry the pop logic on the
    /// new tail node.
    unsafe fn pop_inner(&mut self, tail: *mut MpscNode) -> PopResult {
        // SAFETY: `tail` is a valid node that was the stub's successor.
        let next = unsafe { (*tail).next.load(Ordering::Acquire) };

        if !next.is_null() {
            self.tail = next;
            return PopResult::Success(tail);
        }

        let head = self.head.load(Ordering::Acquire);
        if tail != head {
            return PopResult::Inconsistent;
        }

        let stub = self.stub_ptr();
        // SAFETY: stub is heap-allocated and always valid.
        unsafe {
            self.push(stub);
        }

        // SAFETY: tail is still valid.
        let next = unsafe { (*tail).next.load(Ordering::Acquire) };
        if !next.is_null() {
            self.tail = next;
            return PopResult::Success(tail);
        }

        PopResult::Inconsistent
    }

    /// Drains up to `max_count` nodes into a `Vec`.
    ///
    /// Stops on `Empty` or `Inconsistent`. Returns the popped nodes.
    ///
    /// # Safety
    ///
    /// Must only be called from the single consumer thread.
    pub unsafe fn flush(&mut self, max_count: usize) -> Vec<*mut MpscNode> {
        let mut result = Vec::new();
        for _ in 0..max_count {
            // SAFETY: caller guarantees single-consumer access.
            match unsafe { self.pop() } {
                PopResult::Success(node) => result.push(node),
                PopResult::Empty | PopResult::Inconsistent => break,
            }
        }
        result
    }

    /// Returns `true` if the queue appears empty.
    ///
    /// This is approximate — a concurrent push may cause a false negative.
    #[must_use]
    pub fn is_empty(&self) -> bool {
        let stub = self.stub_ptr();
        let tail = self.tail;
        if tail != stub {
            return false;
        }
        // SAFETY: tail (== stub) is always valid.
        let next = unsafe { (*tail).next.load(Ordering::Acquire) };
        next.is_null()
    }
}

impl Default for MpscQueue {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Arc;
    use std::thread;

    /// Test node wrapping an `MpscNode` with a value.
    #[repr(C)]
    struct TestNode {
        link: MpscNode,
        value: i32,
    }

    impl TestNode {
        fn new_boxed(value: i32) -> *mut Self {
            Box::into_raw(Box::new(Self {
                link: MpscNode::new(),
                value,
            }))
        }

        unsafe fn from_mpsc(ptr: *mut MpscNode) -> *mut Self {
            ptr.cast::<Self>()
        }
    }

    #[test]
    fn new_queue_is_empty() {
        let q = MpscQueue::new();
        assert!(q.is_empty());
    }

    #[test]
    fn pop_empty_returns_empty() {
        let mut q = MpscQueue::new();
        // SAFETY: single-threaded test, we are the consumer.
        let result = unsafe { q.pop() };
        assert_eq!(result, PopResult::Empty);
    }

    #[test]
    fn push_one_pop_one() {
        let mut q = MpscQueue::new();
        let node = TestNode::new_boxed(42);

        // SAFETY: node is valid and we are single-threaded.
        unsafe {
            q.push(node.cast::<MpscNode>());
        }
        assert!(!q.is_empty());

        // SAFETY: single consumer.
        let result = unsafe { q.pop() };
        match result {
            PopResult::Success(ptr) => {
                // SAFETY: we know this was our TestNode.
                let tn = unsafe { &*TestNode::from_mpsc(ptr) };
                assert_eq!(tn.value, 42);
            }
            other => panic!("expected Success, got {other:?}"),
        }

        // Now empty.
        // SAFETY: single-threaded test, we are the consumer.
        let result = unsafe { q.pop() };
        assert_eq!(result, PopResult::Empty);

        // SAFETY: we own the allocation.
        unsafe {
            drop(Box::from_raw(node));
        }
    }

    #[test]
    fn fifo_ordering() {
        let mut q = MpscQueue::new();
        let nodes: Vec<*mut TestNode> = (0..5).map(|i| TestNode::new_boxed(i * 10)).collect();

        for &node in &nodes {
            // SAFETY: nodes are valid.
            unsafe {
                q.push(node.cast::<MpscNode>());
            }
        }

        for (i, _) in nodes.iter().enumerate() {
            // SAFETY: single consumer.
            let result = unsafe { q.pop() };
            match result {
                PopResult::Success(ptr) => {
                    // SAFETY: ptr was pushed as a TestNode; cast back is valid.
                    let tn = unsafe { &*TestNode::from_mpsc(ptr) };
                    assert_eq!(tn.value, (i as i32) * 10);
                }
                other => panic!("expected Success at {i}, got {other:?}"),
            }
        }

        for node in nodes {
            // SAFETY: we own the allocations.
            unsafe {
                drop(Box::from_raw(node));
            }
        }
    }

    #[test]
    fn interleaved_push_pop() {
        let mut q = MpscQueue::new();
        let n1 = TestNode::new_boxed(1);
        let n2 = TestNode::new_boxed(2);
        let n3 = TestNode::new_boxed(3);

        // SAFETY: single-threaded.
        unsafe {
            q.push(n1.cast());
            q.push(n2.cast());
        }

        // SAFETY: single-threaded test, we are the consumer.
        let r = unsafe { q.pop() };
        // SAFETY: ptr was pushed as a TestNode; cast back is valid.
        assert!(
            matches!(r, PopResult::Success(ptr) if unsafe { (*TestNode::from_mpsc(ptr)).value } == 1)
        );

        // SAFETY: node is valid and we are single-threaded.
        unsafe {
            q.push(n3.cast());
        }

        // SAFETY: single-threaded test, we are the consumer.
        let r = unsafe { q.pop() };
        // SAFETY: ptr was pushed as a TestNode; cast back is valid.
        assert!(
            matches!(r, PopResult::Success(ptr) if unsafe { (*TestNode::from_mpsc(ptr)).value } == 2)
        );

        // SAFETY: single-threaded test, we are the consumer.
        let r = unsafe { q.pop() };
        // SAFETY: ptr was pushed as a TestNode; cast back is valid.
        assert!(
            matches!(r, PopResult::Success(ptr) if unsafe { (*TestNode::from_mpsc(ptr)).value } == 3)
        );

        // SAFETY: single-threaded test, we are the consumer.
        let r = unsafe { q.pop() };
        assert_eq!(r, PopResult::Empty);

        // SAFETY: we own these.
        unsafe {
            drop(Box::from_raw(n1));
            drop(Box::from_raw(n2));
            drop(Box::from_raw(n3));
        }
    }

    #[test]
    fn flush_drains_nodes() {
        let mut q = MpscQueue::new();
        let nodes: Vec<*mut TestNode> = (0..10).map(TestNode::new_boxed).collect();

        for &n in &nodes {
            // SAFETY: valid nodes.
            unsafe {
                q.push(n.cast());
            }
        }

        // SAFETY: single consumer.
        let flushed = unsafe { q.flush(100) };
        assert_eq!(flushed.len(), 10);
        assert!(q.is_empty());

        for node in nodes {
            // SAFETY: each node was allocated by TestNode::new_boxed (Box::into_raw).
            unsafe {
                drop(Box::from_raw(node));
            }
        }
    }

    /// Shared queue wrapper to allow safe multi-threaded push.
    ///
    /// The `MpscQueue` itself is not `Sync` because `tail` is a raw pointer.
    /// For testing, we wrap it so producers can call push (which only touches
    /// the atomic `head`) while the consumer thread owns the `&mut` for pop.
    #[derive(Debug)]
    struct SharedQueue {
        inner: MpscQueue,
    }

    // SAFETY: Only `push` is called from non-consumer threads, and `push`
    // only touches `self.head` (an atomic) plus the node's `next` field.
    // The consumer (pop/flush) is called from a single thread after joining.
    unsafe impl Sync for SharedQueue {}
    // SAFETY: The queue can be sent between threads.
    unsafe impl Send for SharedQueue {}

    #[test]
    fn concurrent_producers() {
        const NUM_PRODUCERS: usize = 8;
        const PER_PRODUCER: usize = 5_000;
        let total = NUM_PRODUCERS * PER_PRODUCER;

        let q = Arc::new(SharedQueue {
            inner: MpscQueue::new(),
        });

        // Allocate all nodes up front.
        let all_nodes: Vec<*mut TestNode> =
            (0..total).map(|i| TestNode::new_boxed(i as i32)).collect();

        // Wrapper to send raw pointers across threads.
        struct SendPtr(*mut MpscNode);
        // SAFETY: Each pointer is exclusively owned by one producer thread.
        unsafe impl Send for SendPtr {}

        // Distribute nodes to producer threads.
        let mut handles = Vec::new();
        for chunk in all_nodes.chunks(PER_PRODUCER) {
            let q = Arc::clone(&q);
            let ptrs: Vec<SendPtr> = chunk
                .iter()
                .map(|&p| SendPtr(p.cast::<MpscNode>()))
                .collect();

            handles.push(thread::spawn(move || {
                for SendPtr(node) in ptrs {
                    // SAFETY: push is safe for concurrent producers.
                    unsafe {
                        q.inner.push(node);
                    }
                }
            }));
        }

        for h in handles {
            h.join().expect("producer thread panicked");
        }

        // Now consume from the single consumer thread (this one).
        let q = Arc::try_unwrap(q).expect("arc still shared");
        let mut q = q.inner;
        let mut consumed = 0;
        let mut spins = 0;
        loop {
            // SAFETY: single consumer after all producers joined.
            match unsafe { q.pop() } {
                PopResult::Success(_) => {
                    consumed += 1;
                    spins = 0;
                }
                PopResult::Inconsistent => {
                    spins += 1;
                    if spins > 1000 {
                        break;
                    }
                }
                PopResult::Empty => break,
            }
        }

        assert_eq!(consumed, total, "lost messages");

        // Free all nodes.
        for node in all_nodes {
            // SAFETY: each node was allocated by TestNode::new_boxed (Box::into_raw).
            unsafe {
                drop(Box::from_raw(node));
            }
        }
    }

    #[test]
    fn stress_sequential() {
        let mut q = MpscQueue::new();
        let nodes: Vec<*mut TestNode> = (0..1000).map(TestNode::new_boxed).collect();

        for cycle in 0..10 {
            let start = cycle * 100;
            for &n in &nodes[start..start + 100] {
                // SAFETY: valid nodes.
                unsafe {
                    q.push(n.cast());
                }
            }

            let mut count = 0;
            loop {
                // SAFETY: single consumer.
                match unsafe { q.pop() } {
                    PopResult::Success(_) => count += 1,
                    _ => break,
                }
            }
            assert_eq!(count, 100);
        }

        for node in nodes {
            // SAFETY: each node was allocated by TestNode::new_boxed (Box::into_raw).
            unsafe {
                drop(Box::from_raw(node));
            }
        }
    }
}
