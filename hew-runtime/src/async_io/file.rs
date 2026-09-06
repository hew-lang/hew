//! File operations submitted to the runtime's existing blocking pool.
//!
//! Submission never waits for the syscall. The queued job owns copied inputs
//! and an operation reference even if its coroutine is cancelled immediately.

use std::ffi::c_void;
use std::sync::Arc;

use hew_cabi::string::{string_as_str, HewString};

use super::{HewAsyncIo, IoFailure, IoProducer, IoValue};
use crate::blocking_pool::{hew_blocking_pool_submit, shared_blocking_pool_opt, HewBlockingPool};
use crate::bytes::BytesTriple;
use crate::wake::HewWaker;

enum FileRequest {
    Read(String),
    Write(String, Vec<u8>),
}

impl FileRequest {
    fn run(self) -> Result<IoValue, IoFailure> {
        match self {
            Self::Read(path) => std::fs::read(path)
                .map(IoValue::Bytes)
                .map_err(|error| IoFailure::from_io("read file", &error)),
            Self::Write(path, data) => {
                std::fs::write(path, &data)
                    .map_err(|error| IoFailure::from_io("write file", &error))?;
                // All inputs originate from the u32-sized Hew bytes carrier.
                Ok(IoValue::Count(
                    i64::try_from(data.len()).expect("Hew bytes length"),
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
    let result = std::panic::catch_unwind(|| request.run()).unwrap_or_else(|_| {
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

unsafe fn owned_path(path: *const HewString) -> Result<String, IoFailure> {
    // SAFETY: a start operation borrows a live managed path (null means empty).
    let path = unsafe { string_as_str(path) };
    if path.is_empty() || path.as_bytes().contains(&0) {
        return Err(IoFailure::invalid("file path is empty or contains NUL"));
    }
    Ok(path.to_owned())
}

/// Start a whole-file byte read without blocking the calling worker.
///
/// The path is copied before return. Use `hew_async_io_take_bytes` on readiness
/// and `hew_async_io_free` on every terminal or abandon edge. A null waker is
/// allowed for a caller that polls; it does not make this call blocking.
///
/// # Safety
/// `path` is a live borrowed managed string; `waker` is null or a borrowed valid
/// `HewWaker`. The current runtime must outlive submission into its pool.
#[no_mangle]
pub unsafe extern "C" fn hew_async_file_read(
    path: *const HewString,
    waker: *const HewWaker,
) -> *const HewAsyncIo {
    // SAFETY: both borrowed arguments are valid for this call.
    unsafe {
        submit(
            shared_blocking_pool_opt(),
            waker,
            owned_path(path).map(FileRequest::Read),
        )
    }
}

/// Start an overwrite of a whole file, owning copies of path and contents.
/// The successful result is the byte count. Cancellation before the job starts
/// prevents the write; cancellation during an OS write does not roll it back.
///
/// # Safety
/// `path` and `data` are live borrowed Hew values for this call. The bytes
/// carrier's nonempty region is readable. `waker` is null or a valid borrowed
/// descriptor, and the current runtime outlives submission.
#[no_mangle]
pub unsafe extern "C" fn hew_async_file_write(
    path: *const HewString,
    data: BytesTriple,
    waker: *const HewWaker,
) -> *const HewAsyncIo {
    // SAFETY: path is borrowed for this call and copied before queue admission.
    let request = unsafe { owned_path(path) }.and_then(|path| {
        if data.len != 0 && data.ptr.is_null() {
            return Err(IoFailure::invalid(
                "nonempty file contents have a null buffer",
            ));
        }
        let bytes = if data.len == 0 {
            Vec::new()
        } else {
            // SAFETY: the caller guarantees the carrier region is readable.
            unsafe {
                std::slice::from_raw_parts(data.ptr.add(data.offset as usize), data.len as usize)
                    .to_vec()
            }
        };
        Ok(FileRequest::Write(path, bytes))
    });
    // SAFETY: the owned request has no borrowed inputs; waker is lent by caller.
    unsafe { submit(shared_blocking_pool_opt(), waker, request) }
}
