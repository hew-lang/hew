//! Content-backed stream and sink chunks submitted to the blocking pool.
//!
//! Submission never waits for the syscall. The queued job owns its inputs and
//! an operation reference even if its coroutine is cancelled immediately.

use std::ffi::c_void;
use std::sync::Arc;

use super::{HewAsyncIo, IoFailure, IoProducer, IoValue};
use crate::blocking_pool::{hew_blocking_pool_submit, shared_blocking_pool_opt, HewBlockingPool};
use crate::wake::HewWaker;

enum FileRequest {
    StreamRead(*mut crate::stream::HewStream),
    SinkWrite(*mut crate::stream::HewSink, Vec<u8>),
}

// SAFETY: a stream or sink submission lends its heap handle exclusively
// until the caller observes IoProducer quiescence; no job points into a
// generated coroutine frame.
unsafe impl Send for FileRequest {}

impl FileRequest {
    fn run(self) -> Result<IoValue, IoFailure> {
        match self {
            Self::StreamRead(stream) => {
                // SAFETY: the producer lease keeps the caller's exclusive loan live.
                unsafe { crate::stream::native::read_content(stream) }.map(IoValue::StreamItem)
            }
            Self::SinkWrite(sink, data) => {
                // SAFETY: the producer lease keeps the caller's exclusive loan live.
                unsafe { crate::stream::native::write_content(sink, &data) }?;
                Ok(IoValue::Count(
                    i64::try_from(data.len()).expect("stream item size"),
                ))
            }
        }
    }
}

struct FileJob {
    request: FileRequest,
    operation: IoProducer,
}

unsafe extern "C" fn run_file_job(context: *mut c_void) {
    // SAFETY: a successful submit transfers this unique Box to one pool callback.
    let FileJob { operation, request } = *unsafe { Box::from_raw(context.cast::<FileJob>()) };
    if !operation.is_pending() {
        drop(request);
        return;
    }
    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| request.run()))
        .unwrap_or_else(|_| {
            Err(IoFailure::from_io(
                "file operation worker panicked",
                &std::io::Error::other("worker panicked"),
            ))
        });
    operation.complete(result);
}

unsafe fn submit(
    pool: Option<*mut HewBlockingPool>,
    waker: *const HewWaker,
    request: Result<FileRequest, IoFailure>,
) -> *const HewAsyncIo {
    // SAFETY: the start entrypoint lends the waker for this call.
    let operation = unsafe { HewAsyncIo::new(waker) };
    match (pool, request) {
        (_, Err(error)) => operation.complete(Err(error)),
        (None, Ok(_)) => operation.complete(Err(IoFailure::invalid(
            "asynchronous file I/O requires an installed runtime",
        ))),
        (Some(pool), Ok(request)) => {
            let job = Box::into_raw(Box::new(FileJob {
                operation: IoProducer::new(Arc::clone(&operation)),
                request,
            }));
            // SAFETY: pool is installed for this call; the job is owned until
            // its sole callback runs. Runtime shutdown joins submitted work.
            let status = unsafe { hew_blocking_pool_submit(pool, run_file_job, job.cast()) };
            if status != 0 {
                // SAFETY: failed admission did not transfer the job to a worker.
                drop(unsafe { Box::from_raw(job) });
                operation.complete(Err(IoFailure::invalid("file I/O pool is stopped")));
            }
        }
    }
    Arc::into_raw(operation)
}

/// Submit a content-backed read while retaining the caller's exclusive heap
/// handle loan through producer quiescence.
///
/// # Safety
/// `stream` remains live and untouched until the operation's cleanup status is
/// ready. `waker` is a borrowed live descriptor.
pub(crate) unsafe fn start_stream_read(
    stream: *mut crate::stream::HewStream,
    waker: *const HewWaker,
) -> *const HewAsyncIo {
    // SAFETY: the caller keeps the borrowed heap handle live through quiescence.
    unsafe {
        submit(
            shared_blocking_pool_opt(),
            waker,
            Ok(FileRequest::StreamRead(stream)),
        )
    }
}

/// Submit owned content while retaining the sink loan through quiescence.
///
/// # Safety
/// `sink` remains live and untouched until cleanup is ready. `waker` is borrowed.
pub(crate) unsafe fn start_sink_write(
    sink: *mut crate::stream::HewSink,
    data: Vec<u8>,
    waker: *const HewWaker,
) -> *const HewAsyncIo {
    // SAFETY: the job owns data and the caller retains the exclusive heap loan.
    unsafe {
        submit(
            shared_blocking_pool_opt(),
            waker,
            Ok(FileRequest::SinkWrite(sink, data)),
        )
    }
}
