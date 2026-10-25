//! Content-backed stream and sink chunks submitted to the blocking pool.
//!
//! Submission never waits for the syscall. The queued job owns its inputs and
//! an operation reference even if its coroutine is cancelled immediately.

use std::ffi::c_void;
use std::sync::Arc;

use super::{HewAsyncIo, IoFailure, IoProducer, IoValue};
use crate::blocking_pool::{shared_blocking_pool_opt, submit_owned, HewBlockingPool};
use crate::wake::HewWaker;

enum FileRequest {
    StreamRead(*mut crate::stream::HewStream),
    SinkWrite(*mut crate::stream::HewSink, Arc<super::content::Content>),
    SinkFinish(hew_cabi::sink::PendingFinish),
    ReadFile(*const crate::file_resources::HewReadFile),
    WriteFile(*const crate::file_resources::HewWriteFile, Vec<u8>),
}

// SAFETY: submissions retain their heap-resource loans until the caller
// observes producer quiescence. File resources serialize borrowed operations;
// stream/sink loans are exclusive. No job points into a coroutine frame.
unsafe impl Send for FileRequest {}

impl FileRequest {
    fn discard(self) {
        if let Self::SinkFinish(pending) = self {
            pending.finish();
        }
    }
    #[cfg(windows)]
    fn cancellation_handle(&self) -> Option<usize> {
        match self {
            Self::SinkWrite(sink, _) => {
                // SAFETY: this request retains the exclusive sink loan.
                unsafe { (**sink).native_pipe_handle() }
            }
            Self::SinkFinish(pending) => pending.native_pipe_handle(),
            Self::StreamRead(_) | Self::ReadFile(_) | Self::WriteFile(_, _) => None,
        }
    }

    fn run(self) -> Result<IoValue, IoFailure> {
        match self {
            Self::ReadFile(file) => {
                // SAFETY: the producer retains the caller's resource loan.
                let file = unsafe { file.as_ref() }
                    .ok_or_else(|| IoFailure::invalid("closed read resource"))?;
                file.read_remaining()
                    .map(IoValue::Bytes)
                    .map_err(|error| IoFailure::from_io("read file resource", &error))
            }
            Self::WriteFile(file, bytes) => {
                // SAFETY: the producer retains the caller's resource loan.
                let file = unsafe { file.as_ref() }
                    .ok_or_else(|| IoFailure::invalid("closed write resource"))?;
                file.write(&bytes)
                    .map_err(|error| IoFailure::from_io("write file resource", &error))?;
                // The source wrapper declares a status, not a byte count.
                Ok(IoValue::Count(0))
            }
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
            Self::SinkFinish(pending) => {
                let _ = crate::stream_error::take_last_error();
                // SAFETY: the producer retains the sink loan until quiescence.
                pending.finish();
                match IoFailure::take_recorded() {
                    Some(failure) => Err(failure),
                    None => Ok(IoValue::Count(0)),
                }
            }
        }
    }
}

pub(super) unsafe fn start_sink_finish(
    pending: hew_cabi::sink::PendingFinish,
    waker: *const HewWaker,
) -> *const HewAsyncIo {
    // SAFETY: the caller retains the exclusive sink loan and supplies the waker.
    unsafe {
        submit(
            shared_blocking_pool_opt(),
            waker,
            Ok(FileRequest::SinkFinish(pending)),
            None,
        )
    }
}

struct FileJob {
    request: FileRequest,
    operation: IoProducer,
}

unsafe extern "C" fn run_file_job(context: *mut c_void) {
    // SAFETY: a successful submit transfers this unique Box to one pool callback.
    let FileJob { operation, request } = *unsafe { Box::from_raw(context.cast::<FileJob>()) };
    #[cfg(windows)]
    let _cancellation = match operation
        .file_cancel
        .register(request.cancellation_handle())
    {
        Ok(guard) => guard,
        Err(error) => {
            request.discard();
            operation.complete(Err(IoFailure::from_io(
                "register pipe cancellation",
                &error,
            )));
            return;
        }
    };
    if !operation.is_pending() {
        request.discard();
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
    stream_owner: Option<Option<crate::owned_value_budget::ValueOwner>>,
) -> *const HewAsyncIo {
    // SAFETY: the start entrypoint lends the waker for this call.
    let operation = unsafe { HewAsyncIo::with_net(waker, None, stream_owner) };
    match (pool, request) {
        (_, Err(error)) => operation.complete(Err(error)),
        (None, Ok(request)) => {
            request.discard();
            operation.complete(Err(IoFailure::invalid(
                "asynchronous file I/O requires an installed runtime",
            )));
        }
        (Some(pool), Ok(request)) => {
            let job = Box::new(FileJob {
                operation: IoProducer::new(Arc::clone(&operation)),
                request,
            });
            // SAFETY: pool is installed for this call; the job is owned until
            // its sole callback runs. Runtime shutdown joins submitted work.
            if let Err(job) = unsafe { submit_owned(pool, run_file_job, job) } {
                let FileJob {
                    request,
                    operation: _,
                } = *job;
                request.discard();
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
    owner: Option<crate::owned_value_budget::ValueOwner>,
) -> *const HewAsyncIo {
    // SAFETY: the caller keeps the borrowed heap handle live through quiescence.
    unsafe {
        submit(
            shared_blocking_pool_opt(),
            waker,
            Ok(FileRequest::StreamRead(stream)),
            Some(owner),
        )
    }
}

/// Submit owned content while retaining the sink loan through quiescence.
///
/// # Safety
/// `sink` remains live and untouched until cleanup is ready. `waker` is borrowed.
pub(crate) unsafe fn start_sink_write(
    sink: *mut crate::stream::HewSink,
    data: Arc<super::content::Content>,
    waker: *const HewWaker,
) -> *const HewAsyncIo {
    // SAFETY: the job owns data and the caller retains the exclusive heap loan.
    unsafe {
        submit(
            shared_blocking_pool_opt(),
            waker,
            Ok(FileRequest::SinkWrite(sink, data)),
            None,
        )
    }
}

/// Submit a read while retaining its source resource until quiescence.
///
/// # Safety
/// `file` is a live resource loan until cleanup is ready; `waker` is borrowed.
#[no_mangle]
pub unsafe extern "C" fn hew_async_file_read(
    file: *const crate::file_resources::HewReadFile,
    waker: *const HewWaker,
) -> *const HewAsyncIo {
    // SAFETY: the compiler retains the caller's resource loan through cleanup.
    unsafe {
        submit(
            shared_blocking_pool_opt(),
            waker,
            Ok(FileRequest::ReadFile(file)),
            None,
        )
    }
}

/// Copy the submitted bytes and retain the write-resource loan to quiescence.
///
/// # Safety
/// `file` remains live until cleanup is ready. `data` lends a valid bytes triple
/// and storage through submission; `waker` is a borrowed descriptor.
#[no_mangle]
pub unsafe extern "C" fn hew_async_file_write(
    file: *const crate::file_resources::HewWriteFile,
    data: *const crate::bytes::BytesTriple,
    waker: *const HewWaker,
) -> *const HewAsyncIo {
    // SAFETY: the caller lends the triple and its storage through submission.
    let request = unsafe { data.as_ref() }.map_or_else(
        || Err(IoFailure::invalid("null bytes triple")),
        |data| {
            let bytes = if data.len == 0 {
                Vec::new()
            } else {
                // SAFETY: the triple lends data.len initialized bytes at offset.
                unsafe {
                    std::slice::from_raw_parts(
                        data.ptr.add(data.offset as usize),
                        data.len as usize,
                    )
                }
                .to_vec()
            };
            Ok(FileRequest::WriteFile(file, bytes))
        },
    );
    // SAFETY: the job owns its bytes and the compiler retains the file loan.
    unsafe { submit(shared_blocking_pool_opt(), waker, request, None) }
}

#[cfg(windows)]
#[derive(Default)]
pub(super) struct FileCancellation {
    handle: std::sync::Mutex<Option<std::os::windows::io::OwnedHandle>>,
}

#[cfg(windows)]
impl FileCancellation {
    fn register(&self, handle: Option<usize>) -> std::io::Result<FileCancellationGuard<'_>> {
        use crate::util::MutexExt;
        use std::os::windows::io::BorrowedHandle;
        let handle = handle
            .map(|handle| {
                // SAFETY: the producer retains the exclusive backing loan until
                // this guard is dropped, including any pending kernel operation.
                unsafe { BorrowedHandle::borrow_raw(handle as *mut c_void) }.try_clone_to_owned()
            })
            .transpose()?;
        *self.handle.lock_or_recover() = handle;
        Ok(FileCancellationGuard(self))
    }

    pub(super) fn cancel(&self) -> bool {
        use crate::util::MutexExt;
        use std::os::windows::io::AsRawHandle;
        use windows_sys::Win32::Foundation::ERROR_NOT_FOUND;
        use windows_sys::Win32::System::IO::CancelIoEx;
        let handle = self.handle.lock_or_recover();
        let Some(handle) = handle.as_ref() else {
            return false;
        };
        // SAFETY: the registered duplicate belongs only to this producer;
        // clearing registration uses this same lock before releasing its loan.
        (unsafe { CancelIoEx(handle.as_raw_handle(), std::ptr::null()) }) == 0
            && std::io::Error::last_os_error().raw_os_error() == Some(ERROR_NOT_FOUND as i32)
    }
}

#[cfg(windows)]
struct FileCancellationGuard<'a>(&'a FileCancellation);

#[cfg(windows)]
impl Drop for FileCancellationGuard<'_> {
    fn drop(&mut self) {
        use crate::util::MutexExt;
        drop(self.0.handle.lock_or_recover().take());
    }
}
