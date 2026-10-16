//! Directional authority rooted in an opened directory object.
//!
//! cap-std owns beneath-root path resolution, including symlinks and concurrent
//! filesystem changes. Child handles transfer independent owners; no pathname
//! or handle registry is retained by the runtime.

use std::io;
use std::path::PathBuf;

use cap_std::fs::Dir;
use hew_cabi::string::HewString;

use crate::file_io::{clear_file_io_error, file_path, managed_path, set_file_io_error};
use crate::file_resources::{HewReadFile, HewWriteFile};
use crate::generator::Access;

/// An owned directory root exposing only read operations.
#[derive(Debug)]
pub struct HewReadDir(Dir);

/// An owned directory root exposing only write operations.
#[derive(Debug)]
pub struct HewWriteDir(Dir);

fn resource_result<T>(operation: &str, result: io::Result<T>) -> *mut T {
    match result {
        Ok(value) => {
            clear_file_io_error();
            Box::into_raw(Box::new(value))
        }
        Err(error) => {
            set_file_io_error(operation, &error);
            std::ptr::null_mut()
        }
    }
}

unsafe fn child_path(path: *const HewString) -> io::Result<PathBuf> {
    // SAFETY: the caller lends the managed path for this invocation.
    unsafe { managed_path(path) }
        .map_err(|message| io::Error::new(io::ErrorKind::InvalidInput, message))
}

fn closed_directory() -> io::Error {
    io::Error::new(io::ErrorKind::InvalidInput, "closed directory resource")
}

/// Acquire a read root using the trusted launcher's ambient authority.
///
/// # Safety
/// `path` must lend a live managed string for this invocation.
#[no_mangle]
pub unsafe extern "C" fn hew_dir_open_read(path: *const HewString) -> *mut HewReadDir {
    // SAFETY: the caller lends the managed root path.
    let Some(path) = (unsafe { file_path(path, "open read directory", Access::Read) }) else {
        return std::ptr::null_mut();
    };
    resource_result(
        "open read directory",
        Dir::open_ambient_dir(path, cap_std::ambient_authority()).map(HewReadDir),
    )
}

/// Acquire a write root using the trusted launcher's ambient authority.
///
/// # Safety
/// `path` must lend a live managed string for this invocation.
#[no_mangle]
pub unsafe extern "C" fn hew_dir_open_write(path: *const HewString) -> *mut HewWriteDir {
    // SAFETY: the caller lends the managed root path.
    let Some(path) = (unsafe { file_path(path, "open write directory", Access::Write) }) else {
        return std::ptr::null_mut();
    };
    resource_result(
        "open write directory",
        Dir::open_ambient_dir(path, cap_std::ambient_authority()).map(HewWriteDir),
    )
}

#[no_mangle]
pub extern "C" fn hew_dir_read_is_valid(directory: *const HewReadDir) -> bool {
    !directory.is_null()
}

#[no_mangle]
pub extern "C" fn hew_dir_write_is_valid(directory: *const HewWriteDir) -> bool {
    !directory.is_null()
}

/// Open a read-only child file; cap-std rejects resolution outside the root.
///
/// # Safety
/// Both arguments must be live borrowed managed handles for this invocation.
#[no_mangle]
pub unsafe extern "C" fn hew_dir_read_open_file(
    directory: *const HewReadDir,
    path: *const HewString,
) -> *mut HewReadFile {
    let result = (|| {
        // SAFETY: the caller retains the directory and managed path.
        let directory = unsafe { directory.as_ref() }.ok_or_else(closed_directory)?;
        // SAFETY: the caller lends the managed path for this invocation.
        let path = unsafe { child_path(path) }?;
        directory
            .0
            .open(path)
            .map(|file| HewReadFile::new(file.into_std()))
    })();
    resource_result("open file in read directory", result)
}

/// Create or truncate a write-only child file beneath the supplied root.
///
/// # Safety
/// Both arguments must be live borrowed managed handles for this invocation.
#[no_mangle]
pub unsafe extern "C" fn hew_dir_write_open_file(
    directory: *const HewWriteDir,
    path: *const HewString,
) -> *mut HewWriteFile {
    let result = (|| {
        // SAFETY: the caller retains the directory and managed path.
        let directory = unsafe { directory.as_ref() }.ok_or_else(closed_directory)?;
        // SAFETY: the caller lends the managed path for this invocation.
        let path = unsafe { child_path(path) }?;
        directory
            .0
            .create(path)
            .map(|file| HewWriteFile::new(file.into_std()))
    })();
    resource_result("open file in write directory", result)
}

/// Attenuate a read root to an independently owned child directory.
///
/// # Safety
/// Both arguments must be live borrowed managed handles for this invocation.
#[no_mangle]
pub unsafe extern "C" fn hew_dir_read_subdir(
    directory: *const HewReadDir,
    path: *const HewString,
) -> *mut HewReadDir {
    let result = (|| {
        // SAFETY: the caller retains the directory and managed path.
        let directory = unsafe { directory.as_ref() }.ok_or_else(closed_directory)?;
        // SAFETY: the caller lends the managed path for this invocation.
        let path = unsafe { child_path(path) }?;
        directory.0.open_dir(path).map(HewReadDir)
    })();
    resource_result("open subdirectory in read directory", result)
}

/// Attenuate a write root without granting read authority.
///
/// # Safety
/// Both arguments must be live borrowed managed handles for this invocation.
#[no_mangle]
pub unsafe extern "C" fn hew_dir_write_subdir(
    directory: *const HewWriteDir,
    path: *const HewString,
) -> *mut HewWriteDir {
    let result = (|| {
        // SAFETY: the caller retains the directory and managed path.
        let directory = unsafe { directory.as_ref() }.ok_or_else(closed_directory)?;
        // SAFETY: the caller lends the managed path for this invocation.
        let path = unsafe { child_path(path) }?;
        directory.0.open_dir(path).map(HewWriteDir)
    })();
    resource_result("open subdirectory in write directory", result)
}

/// Release the sole read-root owner.
///
/// # Safety
/// `directory` must be null or an unreleased owner with no outstanding loans.
#[no_mangle]
pub unsafe extern "C" fn hew_dir_read_close(directory: *mut HewReadDir) {
    if !directory.is_null() {
        // SAFETY: this call consumes the unique Box::into_raw owner.
        drop(unsafe { Box::from_raw(directory) });
    }
}

/// Release the sole write-root owner.
///
/// # Safety
/// `directory` must be null or an unreleased owner with no outstanding loans.
#[no_mangle]
pub unsafe extern "C" fn hew_dir_write_close(directory: *mut HewWriteDir) {
    if !directory.is_null() {
        // SAFETY: this call consumes the unique Box::into_raw owner.
        drop(unsafe { Box::from_raw(directory) });
    }
}
