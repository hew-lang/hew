//! WASI preview1 regular-file operations complete at submission.
//!
//! The host supplies preopened directories. Existing file owners perform the
//! syscall and the shared operation owns its result until take or cancellation.
//! Preview1 cannot interrupt a synchronous host syscall after entry.

use std::sync::Arc;

use super::{HewAsyncIo, IoFailure, IoValue};
use crate::bytes::BytesTriple;
use crate::wake::HewWaker;

/// Read the remaining bytes from a supplied regular-file owner.
///
/// # Safety
/// `file` is live through submission. `waker` lends a live descriptor or is null.
#[no_mangle]
pub unsafe extern "C" fn hew_async_file_read(
    file: *const crate::file_resources::HewReadFile,
    waker: *const HewWaker,
) -> *const HewAsyncIo {
    // SAFETY: the caller lends the descriptor through construction.
    let operation = unsafe { HewAsyncIo::new(waker) };
    // SAFETY: the caller retains its file loan through this submission.
    let result = unsafe { file.as_ref() }
        .ok_or_else(|| IoFailure::invalid("closed read resource"))
        .and_then(|file| {
            file.read_remaining()
                .map(IoValue::Bytes)
                .map_err(|error| IoFailure::from_io("read file resource", &error))
        });
    operation.complete(result);
    Arc::into_raw(operation)
}

/// Write bytes through a supplied regular-file owner.
///
/// # Safety
/// `file`, `data` and its byte storage remain live through submission. `waker`
/// lends a live descriptor or is null.
#[no_mangle]
pub unsafe extern "C" fn hew_async_file_write(
    file: *const crate::file_resources::HewWriteFile,
    data: *const BytesTriple,
    waker: *const HewWaker,
) -> *const HewAsyncIo {
    // SAFETY: the caller lends the descriptor through construction.
    let operation = unsafe { HewAsyncIo::new(waker) };
    // SAFETY: the caller retains both loans through this submission.
    let result = match (unsafe { file.as_ref() }, unsafe { data.as_ref() }) {
        (Some(file), Some(data)) => {
            if data.len != 0 && data.ptr.is_null() {
                Err(IoFailure::invalid(
                    "bytes triple has a null data pointer with non-zero length",
                ))
            } else {
                let bytes = if data.len == 0 {
                    &[]
                } else {
                    // SAFETY: the nonempty triple has a non-null data pointer and
                    // the caller lends data.len initialized bytes at offset.
                    unsafe {
                        std::slice::from_raw_parts(
                            data.ptr.add(data.offset as usize),
                            data.len as usize,
                        )
                    }
                };
                file.write(bytes)
                    .map(|()| IoValue::Count(0))
                    .map_err(|error| IoFailure::from_io("write file resource", &error))
            }
        }
        _ => Err(IoFailure::invalid(
            "closed write resource or null bytes triple",
        )),
    };
    operation.complete(result);
    Arc::into_raw(operation)
}
