//! TLS producers retain owned inputs and restore diagnostics on resumption.

use hew_runtime::async_io::library::{submit_socket_job, take_library_result};
use hew_runtime::async_io::{AsyncIoStatus, HewAsyncIo};
use hew_runtime::wake::HewWaker;

use super::*;

enum Value {
    Connect(Option<HewTlsStream>),
    Read(HewTlsReadResult),
    Write(HewTlsWriteResult),
}

struct Completion {
    value: Value,
    error: String,
    connect_failure: Option<(c_int, i64)>,
}

// SAFETY: the read result uniquely owns its atomic-refcounted bytes allocation;
// the other variants contain only owned Send values. Publication transfers
// access from the producer to the consumer under the operation state lock.
unsafe impl Send for Completion {}

impl Drop for Completion {
    fn drop(&mut self) {
        if let Value::Read(result) = &mut self.value {
            // SAFETY: this completion owns the bytes until take transfers them.
            unsafe { hew_runtime::bytes::hew_bytes_drop(result.data.ptr) };
        }
    }
}

impl Completion {
    fn io(value: Value) -> Self {
        Self {
            value,
            error: get_tls_last_error(),
            connect_failure: None,
        }
    }

    fn restore(&self) {
        if self.error.is_empty() {
            clear_tls_last_error();
        } else {
            set_tls_last_error(&self.error);
        }
        if let Some(failure) = self.connect_failure {
            LAST_CONNECT_FAILURE.with(|slot| slot.set(failure));
        }
    }
}

/// Start a verified TLS connection without occupying the caller's worker.
///
/// # Safety
/// String, bytes and waker arguments are null or live borrowed values.
#[no_mangle]
pub unsafe extern "C" fn hew_async_tls_connect(
    host: *const HewString,
    port: c_int,
    trust: c_int,
    pem: *const BytesTriple,
    handshake_ms: i64,
    io_ms: i64,
    waker: *const HewWaker,
) -> *const HewAsyncIo {
    // SAFETY: the caller lends both inputs through submission; the job copies them.
    let (host, pem) = unsafe { (string_as_str(host).to_owned(), bytes_view(pem).to_vec()) };
    // SAFETY: the caller lends the descriptor, and the closure owns every input.
    unsafe {
        submit_socket_job(waker, move |cancellation| {
            match connect_validated(
                &host,
                port,
                trust,
                &pem,
                handshake_ms,
                io_ms,
                Some(cancellation),
            ) {
                Ok(stream) => Completion {
                    value: Value::Connect(Some(stream)),
                    error: String::new(),
                    connect_failure: Some((0, 0)),
                },
                Err(failure) => Completion {
                    value: Value::Connect(None),
                    error: format!("tls.connect: {}", failure.message),
                    connect_failure: Some((failure.class, failure.errno)),
                },
            }
        })
    }
}

/// Read TLS bytes while unrelated actors continue to run.
///
/// # Safety
/// `stream` is null or a live borrowed TLS handle; `waker` is null or live.
#[no_mangle]
pub unsafe extern "C" fn hew_async_tls_read(
    stream: *const HewTlsStream,
    size: c_int,
    waker: *const HewWaker,
) -> *const HewAsyncIo {
    // SAFETY: the caller lends its handle until we clone the shared stream owner.
    let stream = unsafe { stream.as_ref() }.map(|stream| Arc::clone(&stream.inner));
    // SAFETY: the descriptor is borrowed for submit and the job owns its stream.
    unsafe {
        submit_socket_job(waker, move |cancellation| {
            let result = if let Some(stream) = &stream {
                read_tls_shared(stream, size, Some(Arc::clone(cancellation)))
            } else {
                set_tls_last_error("hew_tls_read: invalid stream");
                HewTlsReadResult {
                    data: empty_bytes_triple(),
                    status: TLS_STATUS_IO_ERROR,
                }
            };
            Completion::io(Value::Read(result))
        })
    }
}

/// Write TLS bytes while unrelated actors continue to run.
///
/// # Safety
/// `stream`, `data` and `waker` are null or live borrowed values.
#[no_mangle]
pub unsafe extern "C" fn hew_async_tls_write(
    stream: *const HewTlsStream,
    data: *const BytesTriple,
    waker: *const HewWaker,
) -> *const HewAsyncIo {
    // SAFETY: both inputs are borrowed through submission and copied into the job.
    let (stream, data) = unsafe {
        (
            stream.as_ref().map(|stream| Arc::clone(&stream.inner)),
            bytes_view(data).to_vec(),
        )
    };
    // SAFETY: the descriptor is borrowed for submit and the job owns its inputs.
    unsafe {
        submit_socket_job(waker, move |cancellation| {
            let result = if let Some(stream) = &stream {
                write_tls_shared(stream, &data, Some(Arc::clone(cancellation)))
            } else {
                set_tls_last_error("hew_tls_write: invalid stream");
                HewTlsWriteResult {
                    written: -1,
                    status: TLS_STATUS_IO_ERROR,
                }
            };
            Completion::io(Value::Write(result))
        })
    }
}

unsafe fn take(operation: *const HewAsyncIo) -> Result<Completion, i32> {
    // SAFETY: the matching native-I/O resume adapter owns the operation.
    if let Some(completion) = unsafe { take_library_result::<Completion>(operation) } {
        completion.restore();
        return Ok(completion);
    }
    // SAFETY: the caller still owns the operation after an unsuccessful take.
    let status = unsafe { hew_runtime::async_io::hew_async_io_restore_error(operation) };
    if status == AsyncIoStatus::Error as i32 {
        set_tls_last_error(
            hew_runtime::stream_error::take_last_error()
                .unwrap_or_else(|| "TLS operation failed".into()),
        );
    }
    Err(status)
}

/// Transfer a completed connection and restore its failure metadata.
///
/// # Safety
/// `operation` is a live TLS-connect request; `out` is writable pointer storage.
#[no_mangle]
pub unsafe extern "C" fn hew_async_tls_take_connect(
    operation: *const HewAsyncIo,
    out: *mut *mut HewTlsStream,
) -> i32 {
    // SAFETY: the caller owns this request and its writable result storage.
    unsafe {
        match take(operation) {
            Ok(mut completion) => {
                let Value::Connect(stream) = &mut completion.value else {
                    return 0;
                };
                *out = stream.take().map_or(std::ptr::null_mut(), |stream| {
                    Box::into_raw(Box::new(stream))
                });
            }
            Err(status) if status == AsyncIoStatus::Error as i32 => {
                LAST_CONNECT_FAILURE.with(|slot| slot.set((TLS_CONNECT_OS, i64::from(libc::EIO))));
                *out = std::ptr::null_mut();
            }
            Err(_) => return 0,
        }
    }
    1
}

/// Transfer completed bytes and restore the TLS error detail.
///
/// # Safety
/// `operation` is a live TLS-read request; `out` is writable result storage.
#[no_mangle]
pub unsafe extern "C" fn hew_async_tls_take_read(
    operation: *const HewAsyncIo,
    out: *mut HewTlsReadResult,
) -> i32 {
    // SAFETY: the caller owns this request and its writable result storage.
    unsafe {
        match take(operation) {
            Ok(mut completion) => {
                let Value::Read(result) = &mut completion.value else {
                    return 0;
                };
                *out = HewTlsReadResult {
                    data: std::mem::replace(&mut result.data, empty_bytes_triple()),
                    status: result.status,
                };
            }
            Err(status) if status == AsyncIoStatus::Error as i32 => {
                *out = HewTlsReadResult {
                    data: empty_bytes_triple(),
                    status: TLS_STATUS_IO_ERROR,
                };
            }
            Err(_) => return 0,
        }
    }
    1
}

/// Transfer a completed write count and restore the TLS error detail.
///
/// # Safety
/// `operation` is a live TLS-write request; `out` is writable result storage.
#[no_mangle]
pub unsafe extern "C" fn hew_async_tls_take_write(
    operation: *const HewAsyncIo,
    out: *mut HewTlsWriteResult,
) -> i32 {
    // SAFETY: the caller owns this request and its writable result storage.
    unsafe {
        match take(operation) {
            Ok(mut completion) => {
                let Value::Write(result) = &mut completion.value else {
                    return 0;
                };
                *out = HewTlsWriteResult {
                    written: result.written,
                    status: result.status,
                };
            }
            Err(status) if status == AsyncIoStatus::Error as i32 => {
                *out = HewTlsWriteResult {
                    written: -1,
                    status: TLS_STATUS_IO_ERROR,
                };
            }
            Err(_) => return 0,
        }
    }
    1
}
