//! Hew runtime: `stdio` module.
//!
//! Standard I/O operations (stdout, stderr, stdin) with C ABI.
//!
//! All string values use the managed, length-bounded Hew string carrier.

use hew_cabi::string::{string_as_bytes, HewString};

use crate::output::{write_checked, Stream};

/// Write a string to stdout without a trailing newline.
///
/// # Safety
///
/// `s` must be null or a live managed string handle.
#[no_mangle]
pub unsafe extern "C" fn hew_io_write(s: *const HewString) -> i32 {
    // SAFETY: the caller supplies a live managed string handle; null is empty.
    write_result(write_checked(Stream::Out, unsafe { string_as_bytes(s) }))
}

/// Write a string to stderr without a trailing newline.
///
/// # Safety
///
/// `s` must be null or a live managed string handle.
#[no_mangle]
pub unsafe extern "C" fn hew_io_write_err(s: *const HewString) -> i32 {
    // SAFETY: the caller supplies a live managed string handle; null is empty.
    write_result(write_checked(Stream::Err, unsafe { string_as_bytes(s) }))
}

/// Queue a compiler-generated diagnostic without making reporting fallible.
///
/// # Safety
///
/// `s` must be null or a live managed string handle.
#[no_mangle]
pub unsafe extern "C" fn hew_report_stderr(s: *const HewString) {
    // SAFETY: the caller supplies a live managed string handle; null is empty.
    crate::output::write(Stream::Err, unsafe { string_as_bytes(s) });
}

fn write_result(result: std::io::Result<()>) -> i32 {
    match result {
        Ok(()) => {
            crate::file_io::clear_file_io_error();
            0
        }
        Err(error) => {
            crate::file_io::set_file_io_error("write", &error);
            -1
        }
    }
}

#[cfg(unix)]
fn watchable_descriptor(
    fd: std::os::fd::RawFd,
    direction: crate::reactor::Direction,
) -> std::io::Result<()> {
    use std::io::{Error, ErrorKind};
    let mut stat = std::mem::MaybeUninit::<libc::stat>::uninit();
    // SAFETY: fstat initializes the local stat on success.
    if unsafe { libc::fstat(fd, stat.as_mut_ptr()) } < 0 {
        return Err(Error::last_os_error());
    }
    // SAFETY: fstat succeeded.
    let mode = unsafe { stat.assume_init() }.st_mode & libc::S_IFMT;
    if matches!(mode, libc::S_IFREG | libc::S_IFDIR) {
        return Err(Error::new(
            ErrorKind::Unsupported,
            "this file type has no readiness watch",
        ));
    }
    // SAFETY: F_GETFL reads the status flags of this descriptor.
    let flags = unsafe { libc::fcntl(fd, libc::F_GETFL) };
    if flags < 0 {
        return Err(Error::last_os_error());
    }
    let access = flags & libc::O_ACCMODE;
    if matches!(
        (direction, access),
        (crate::reactor::Direction::Read, libc::O_WRONLY)
            | (crate::reactor::Direction::Write, libc::O_RDONLY)
    ) {
        return Err(Error::new(
            ErrorKind::Unsupported,
            "descriptor access mode does not support this direction",
        ));
    }
    Ok(())
}

/// Probe whether a descriptor can be watched for reading. The returned status
/// describes this instantaneous probe; opening a watch independently pins its descriptor.
#[cfg(not(target_arch = "wasm32"))]
#[no_mangle]
pub extern "C" fn hew_io_watch_check(fd: i32) -> i32 {
    #[cfg(unix)]
    return match watchable_descriptor(fd, crate::reactor::Direction::Read) {
        Ok(()) => 0,
        Err(error) if error.raw_os_error() == Some(libc::EBADF) => 1,
        Err(_) => 2,
    };
    #[cfg(not(unix))]
    {
        let _ = fd;
        2
    }
}

#[cfg(unix)]
fn open_watch(
    fd: i32,
    direction: crate::reactor::Direction,
) -> std::io::Result<*mut crate::stream::HewStreamPair> {
    use std::os::fd::{AsRawFd, FromRawFd, OwnedFd};
    // SAFETY: F_DUPFD_CLOEXEC obtains a new owned descriptor or returns -1.
    let duplicate = unsafe { libc::fcntl(fd, libc::F_DUPFD_CLOEXEC, 0) };
    if duplicate < 0 {
        return Err(std::io::Error::last_os_error());
    }
    // SAFETY: the duplicate is exclusively owned by this constructor.
    let duplicate = unsafe { OwnedFd::from_raw_fd(duplicate) };
    watchable_descriptor(duplicate.as_raw_fd(), direction)?;
    Ok(crate::stream::readiness_stream(duplicate, direction))
}

#[cfg(not(target_arch = "wasm32"))]
fn descriptor_watch(
    fd: i32,
    direction: crate::reactor::Direction,
) -> *mut crate::stream::HewStreamPair {
    #[cfg(unix)]
    let error = match open_watch(fd, direction) {
        Ok(pair) => return pair,
        Err(error) => error,
    };
    #[cfg(not(unix))]
    let error = {
        let _ = direction;
        std::io::Error::new(
            std::io::ErrorKind::Unsupported,
            "descriptor readiness requires Unix",
        )
    };
    // A missing descriptor and an unsupported watch are stable categories for
    // the source wrapper. Other OS failures retain their original detail.
    let errno = error.raw_os_error().unwrap_or(0);
    let kind = if errno == libc::EBADF {
        crate::stream_error::IO_ERROR_KIND_NOT_FOUND
    } else {
        crate::stream_error::io_error_kind_tag(error.kind())
    };
    crate::stream_error::set_last_error_with_errno_and_kind(
        format!("watch descriptor {fd}: {error}"),
        errno,
        kind,
    );
    std::ptr::null_mut()
}

/// Watch a descriptor for readable data, EOF or error without consuming bytes.
#[cfg(not(target_arch = "wasm32"))]
#[no_mangle]
pub extern "C" fn hew_io_watch_readable(fd: i32) -> *mut crate::stream::HewStreamPair {
    descriptor_watch(fd, crate::reactor::Direction::Read)
}

/// Watch for write capacity or a terminal error without writing a byte.
#[cfg(not(target_arch = "wasm32"))]
#[no_mangle]
pub extern "C" fn hew_io_watch_writable(fd: i32) -> *mut crate::stream::HewStreamPair {
    descriptor_watch(fd, crate::reactor::Direction::Write)
}

#[cfg(test)]
mod tests {
    use super::*;
    use hew_cabi::string::string_from_str;

    fn managed(text: &str) -> *mut HewString {
        string_from_str(text)
    }

    #[test]
    fn write_null_is_noop() {
        // Passing null should not panic.
        // SAFETY: Null pointer passed deliberately to verify no-op behaviour.
        unsafe { hew_io_write(std::ptr::null()) };
    }

    #[test]
    fn write_err_null_is_noop() {
        // Passing null should not panic.
        // SAFETY: Null pointer passed deliberately to verify no-op behaviour.
        unsafe { hew_io_write_err(std::ptr::null()) };
    }

    #[test]
    fn write_valid_string() {
        let s = managed("hello from test");
        // SAFETY: `s` is a live managed owner and is released exactly once.
        unsafe {
            hew_io_write(s);
            hew_cabi::string::string_release(s);
        }
    }

    #[test]
    fn write_err_valid_string() {
        let s = managed("error from test");
        // SAFETY: `s` is a live managed owner and is released exactly once.
        unsafe {
            hew_io_write_err(s);
            hew_cabi::string::string_release(s);
        }
    }
}
