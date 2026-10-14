//! Directional authority over files opened by the application launcher.
//!
//! Resource operations use the opened descriptor, never a retained path. The
//! OS descriptor has only the requested access mode. Each returned box has one
//! consuming release; the runtime retains no second handle or registry entry.

use std::fs::{File, OpenOptions};
use std::io::{self, Read, Write};
use std::sync::Mutex;

use hew_cabi::string::HewString;

use crate::bytes::BytesTriple;
use crate::file_io::{clear_file_io_error, file_path, set_file_io_errno, set_file_io_error};
use crate::generator::Access;

/// An opened read-only descriptor. The mutex serializes borrowed operations.
#[derive(Debug)]
pub struct HewReadFile(Mutex<File>);

/// An opened write-only descriptor. Writes are unbuffered and report errors.
#[derive(Debug)]
pub struct HewWriteFile(Mutex<File>);

impl HewReadFile {
    pub(crate) fn read_remaining(&self) -> io::Result<Vec<u8>> {
        let mut file = self
            .0
            .lock()
            .map_err(|_| io::Error::other("file resource lock poisoned"))?;
        let mut data = Vec::new();
        file.read_to_end(&mut data)?;
        Ok(data)
    }
}

impl HewWriteFile {
    pub(crate) fn write(&self, bytes: &[u8]) -> io::Result<()> {
        self.0
            .lock()
            .map_err(|_| io::Error::other("file resource lock poisoned"))?
            .write_all(bytes)
    }
}

/// Open a read-only resource, or return null with the existing I/O error set.
///
/// # Safety
/// `path` must be a live borrowed managed string.
#[no_mangle]
pub unsafe extern "C" fn hew_file_open_read(path: *const HewString) -> *mut HewReadFile {
    // SAFETY: the caller lends the managed path for this invocation.
    let Some(path) = (unsafe { file_path(path, "open read file", Access::Read) }) else {
        return std::ptr::null_mut();
    };
    match File::open(path) {
        Ok(file) => {
            clear_file_io_error();
            Box::into_raw(Box::new(HewReadFile(Mutex::new(file))))
        }
        Err(error) => {
            set_file_io_error("open read file", &error);
            std::ptr::null_mut()
        }
    }
}

/// Create or truncate a write-only resource, or return null with an I/O error.
///
/// # Safety
/// `path` must be a live borrowed managed string.
#[no_mangle]
pub unsafe extern "C" fn hew_file_open_write(path: *const HewString) -> *mut HewWriteFile {
    // SAFETY: the caller lends the managed path for this invocation.
    let Some(path) = (unsafe { file_path(path, "open write file", Access::Write) }) else {
        return std::ptr::null_mut();
    };
    match OpenOptions::new()
        .write(true)
        .create(true)
        .truncate(true)
        .open(path)
    {
        Ok(file) => {
            clear_file_io_error();
            Box::into_raw(Box::new(HewWriteFile(Mutex::new(file))))
        }
        Err(error) => {
            set_file_io_error("open write file", &error);
            std::ptr::null_mut()
        }
    }
}

/// Whether the read constructor returned a resource.
#[no_mangle]
pub extern "C" fn hew_file_read_is_valid(file: *const HewReadFile) -> bool {
    !file.is_null()
}

/// Whether the write constructor returned a resource.
#[no_mangle]
pub extern "C" fn hew_file_write_is_valid(file: *const HewWriteFile) -> bool {
    !file.is_null()
}

fn invalid_resource(operation: &str) {
    set_file_io_error(
        operation,
        &io::Error::new(io::ErrorKind::InvalidInput, "closed file resource"),
    );
}

/// Read the remaining bytes. Empty success is distinguished by the error slot.
///
/// # Safety
/// `file` must be null or a live read resource borrowed until this call returns.
#[no_mangle]
pub unsafe extern "C" fn hew_file_read_remaining(file: *const HewReadFile) -> BytesTriple {
    let empty = BytesTriple {
        ptr: std::ptr::null_mut(),
        offset: 0,
        len: 0,
    };
    // SAFETY: the caller retains the resource for this borrowed operation.
    let Some(file) = (unsafe { file.as_ref() }) else {
        invalid_resource("read file resource");
        return empty;
    };
    let data = match file.read_remaining() {
        Ok(data) => data,
        Err(error) => {
            set_file_io_error("read file resource", &error);
            return empty;
        }
    };
    let Ok(len) = u32::try_from(data.len()) else {
        set_file_io_errno(
            "read file resource",
            "file exceeds Bytes capacity",
            libc::EFBIG,
        );
        return empty;
    };
    clear_file_io_error();
    // SAFETY: data lends len initialized bytes; the constructor copies them.
    unsafe { crate::bytes::hew_bytes_from_static(data.as_ptr(), len) }
}

/// Write all bytes at the descriptor's current position, reporting OS errors.
///
/// # Safety
/// `file` must be null or a live write resource. `data` must lend a valid bytes
/// triple and its storage until this call returns.
#[no_mangle]
pub unsafe extern "C" fn hew_file_write_handle(
    file: *const HewWriteFile,
    data: *const BytesTriple,
) -> i32 {
    // SAFETY: the caller retains both borrowed arguments for this operation.
    let (Some(file), Some(data)) = (unsafe { file.as_ref() }, unsafe { data.as_ref() }) else {
        invalid_resource("write file resource");
        return -1;
    };
    let bytes = if data.len == 0 {
        &[]
    } else {
        // SAFETY: the caller lends data.len initialized bytes at this offset.
        unsafe { std::slice::from_raw_parts(data.ptr.add(data.offset as usize), data.len as usize) }
    };
    match file.write(bytes) {
        Ok(()) => {
            clear_file_io_error();
            0
        }
        Err(error) => {
            set_file_io_error("write file resource", &error);
            -1
        }
    }
}

/// Release the sole read-resource owner. Null is an empty failed acquisition.
///
/// # Safety
/// `file` must be null or an unreleased resource from `hew_file_open_read`, with
/// no outstanding borrowed operation.
#[no_mangle]
pub unsafe extern "C" fn hew_file_read_close(file: *mut HewReadFile) {
    if !file.is_null() {
        // SAFETY: this invocation consumes the unique Box::into_raw owner.
        drop(unsafe { Box::from_raw(file) });
    }
}

/// Release the sole write-resource owner. Writes have no user-space buffer.
///
/// # Safety
/// `file` must be null or an unreleased resource from `hew_file_open_write`, with
/// no outstanding borrowed operation.
#[no_mangle]
pub unsafe extern "C" fn hew_file_write_close(file: *mut HewWriteFile) {
    if !file.is_null() {
        // SAFETY: this invocation consumes the unique Box::into_raw owner.
        drop(unsafe { Box::from_raw(file) });
    }
}
