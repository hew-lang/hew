//! WASI preview1 descriptor-relative directory operations.
//!
//! The host's path_open performs beneath-descriptor resolution, including safe
//! relative symlinks and parent components. The nocwd libc entry forwards this
//! exact descriptor instead of searching ambient/preopened path names. Native
//! POSIX openat does not provide this contract; this adapter is WASI-only.

use std::ffi::CString;
use std::fs::File;
use std::io;
use std::os::fd::{AsRawFd, FromRawFd};
use std::path::Path;

#[derive(Debug)]
pub(super) struct Dir(File);

fn path_string(path: &Path) -> io::Result<CString> {
    CString::new(path.as_os_str().as_encoded_bytes())
        .map_err(|_| io::Error::new(io::ErrorKind::InvalidInput, "path contains interior NUL"))
}

fn owned_file(descriptor: libc::c_int) -> io::Result<File> {
    if descriptor < 0 {
        return Err(io::Error::last_os_error());
    }
    // SAFETY: a successful WASI open transfers one uniquely owned descriptor.
    Ok(unsafe { File::from_raw_fd(descriptor) })
}

impl Dir {
    pub(super) fn open_root(path: impl AsRef<Path>) -> io::Result<Self> {
        let path = path_string(path.as_ref())?;
        // SAFETY: path is NUL-terminated and lives through the call. WASI libc
        // resolves this acquisition only through host-supplied preopens.
        owned_file(unsafe { libc::open(path.as_ptr(), libc::O_RDONLY | libc::O_DIRECTORY) })
            .map(Self)
    }

    fn open_with(&self, path: &Path, flags: libc::c_int) -> io::Result<File> {
        let path = path_string(path)?;
        // SAFETY: the directory retains its descriptor and path lives through
        // the call. This WASI-specific entry forwards to path_open on that
        // descriptor, with host-enforced confinement and symlink resolution.
        owned_file(unsafe {
            libc::__wasilibc_nocwd_openat_nomode(self.0.as_raw_fd(), path.as_ptr(), flags)
        })
    }

    pub(super) fn open(&self, path: impl AsRef<Path>) -> io::Result<File> {
        self.open_with(path.as_ref(), libc::O_RDONLY)
    }

    pub(super) fn create(&self, path: impl AsRef<Path>) -> io::Result<File> {
        self.open_with(
            path.as_ref(),
            libc::O_WRONLY | libc::O_CREAT | libc::O_TRUNC,
        )
    }

    pub(super) fn open_dir(&self, path: impl AsRef<Path>) -> io::Result<Self> {
        self.open_with(path.as_ref(), libc::O_RDONLY | libc::O_DIRECTORY)
            .map(Self)
    }
}
