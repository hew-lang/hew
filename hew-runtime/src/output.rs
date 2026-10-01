//! Standard output and standard error through one ordered writer.
//!
//! Every write from Hew code (`print`, `println`, `io.write`, `io.write_err`)
//! and every user-visible runtime report copies its bytes into one
//! process-wide queue and returns. A single drainer job on the blocking pool
//! writes the queue to the two streams in order, so stdout and stderr writes
//! never reorder against each other and a worker never waits on a slow
//! terminal or pipe. Without a pool (before the runtime starts, after it
//! stops, or on wasm32) the writer drains on the calling thread.
//!
//! A writer that gets more than [`BACKLOG`] bytes ahead of the drainer waits
//! for it. Terminal paths call [`flush`] before the process ends.

use std::collections::VecDeque;
use std::io::Write;
use std::sync::{Condvar, Mutex};

use crate::util::{CondvarExt, MutexExt};

/// How far writers may run ahead of the drainer before they wait.
const BACKLOG: usize = 1 << 20;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum Stream {
    Out,
    Err,
}

struct Queue {
    chunks: VecDeque<(Stream, Vec<u8>)>,
    /// Bytes queued or being written.
    bytes: usize,
    /// A drainer owns the queue until it finds it empty.
    draining: bool,
}

static QUEUE: Mutex<Queue> = Mutex::new(Queue {
    chunks: VecDeque::new(),
    bytes: 0,
    draining: false,
});
static PROGRESS: Condvar = Condvar::new();

/// Queue a copy of `bytes` for `stream`, in order with every other write.
pub(crate) fn write(stream: Stream, bytes: &[u8]) {
    if bytes.is_empty() {
        return;
    }
    let start = {
        let mut queue = QUEUE.lock_or_recover();
        while queue.bytes >= BACKLOG {
            queue = PROGRESS.wait_or_recover(queue);
        }
        if !queue.draining && pool().is_none() {
            // No pool and nothing queued: write in place, in order under the lock.
            emit(stream, bytes);
            return;
        }
        match queue.chunks.back_mut() {
            Some((last, chunk)) if *last == stream => chunk.extend_from_slice(bytes),
            _ => queue.chunks.push_back((stream, bytes.to_vec())),
        }
        queue.bytes += bytes.len();
        !std::mem::replace(&mut queue.draining, true)
    };
    if start {
        start_drainer();
    }
}

/// Wait until everything queued so far has been written.
pub(crate) fn flush() {
    let mut queue = QUEUE.lock_or_recover();
    while queue.draining {
        queue = PROGRESS.wait_or_recover(queue);
    }
}

#[cfg(not(target_arch = "wasm32"))]
fn pool() -> Option<*mut crate::blocking_pool::HewBlockingPool> {
    crate::blocking_pool::shared_blocking_pool_opt()
}

#[cfg(target_arch = "wasm32")]
fn pool() -> Option<()> {
    None
}

fn start_drainer() {
    #[cfg(not(target_arch = "wasm32"))]
    if let Some(pool) = pool() {
        // SAFETY: the pool belongs to the installed runtime; the job takes no
        // argument.
        let status = unsafe {
            crate::blocking_pool::hew_blocking_pool_submit(pool, drain_job, std::ptr::null_mut())
        };
        if status == 0 {
            return;
        }
    }
    drain();
}

#[cfg(not(target_arch = "wasm32"))]
unsafe extern "C" fn drain_job(_: *mut std::ffi::c_void) {
    drain();
}

fn drain() {
    loop {
        let batch = {
            let mut queue = QUEUE.lock_or_recover();
            if queue.chunks.is_empty() {
                queue.draining = false;
                PROGRESS.notify_all();
                return;
            }
            std::mem::take(&mut queue.chunks)
        };
        let mut written = 0;
        for (stream, bytes) in &batch {
            emit(*stream, bytes);
            written += bytes.len();
        }
        QUEUE.lock_or_recover().bytes -= written;
        PROGRESS.notify_all();
    }
}

fn emit(stream: Stream, bytes: &[u8]) {
    // A closed or failing stream drops its output; the program goes on.
    let _ = match stream {
        Stream::Out => {
            let mut out = std::io::stdout().lock();
            out.write_all(bytes).and_then(|()| out.flush())
        }
        Stream::Err => std::io::stderr().lock().write_all(bytes),
    };
}

/// An `io::Write` that queues what it collects when dropped, for formatters
/// that write one report in several pieces.
pub(crate) struct Report {
    stream: Stream,
    bytes: Vec<u8>,
}

impl Report {
    pub(crate) fn new(stream: Stream) -> Self {
        Self {
            stream,
            bytes: Vec::new(),
        }
    }
}

impl Write for Report {
    fn write(&mut self, bytes: &[u8]) -> std::io::Result<usize> {
        self.bytes.extend_from_slice(bytes);
        Ok(bytes.len())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        write(self.stream, &std::mem::take(&mut self.bytes));
        Ok(())
    }
}

impl Drop for Report {
    fn drop(&mut self) {
        write(self.stream, &self.bytes);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn flush_waits_for_queued_writes_and_leaves_the_queue_idle() {
        write(Stream::Out, b"");
        write(Stream::Err, b"output test line\n");
        flush();
        let queue = QUEUE.lock_or_recover();
        assert!(!queue.draining);
        assert!(queue.chunks.is_empty());
        assert_eq!(queue.bytes, 0);
    }
}
