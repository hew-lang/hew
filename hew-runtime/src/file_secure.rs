//! Hew runtime: owner, permission, lock and capacity primitives behind
//! `std.fs`.
//!
//! Each entry reports failure through the stream error slot, as `file_io`
//! does. What a platform cannot do is reported as unsupported, never
//! approximated: Windows has no Unix permission mode or numeric owner, so the
//! entries that create with a mode or check one refuse there.
#![allow(
    unsafe_op_in_unsafe_fn,
    reason = "FFI entry-point module; SAFETY documented at fn signature."
)]

use crate::bytes::BytesTriple;
use crate::file_io::{clear_file_io_error, file_path, set_file_io_errno, set_file_io_error_or};
use hew_cabi::string::HewString;
use std::fs::{File, OpenOptions};
use std::io::{self, Read};
use std::path::Path;

const OK: i32 = 0;
const FAILED: i32 = -1;

/// `HewFsMetadata::kind` values, mirrored by `FileKind` in `std/fs.hew`.
const KIND_FILE: i64 = 0;
const KIND_DIRECTORY: i64 = 1;
const KIND_SYMLINK: i64 = 2;
const KIND_OTHER: i64 = 3;

/// One file's metadata. `status` is 0, or -1 with the error in the stream
/// slot. `mode` and `owner` are -1 where the platform has none (Windows).
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct HewFsMetadata {
    pub status: i64,
    pub kind: i64,
    pub size: i64,
    pub mode: i64,
    pub owner: i64,
}

/// The capacity of the file system holding a path, in bytes. `status` is 0,
/// or -1 with the error in the stream slot.
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct HewFsSpace {
    pub status: i64,
    pub total: i64,
    pub free: i64,
    pub available: i64,
}

#[cfg(not(unix))]
fn unsupported(what: &str) -> io::Error {
    io::Error::new(
        io::ErrorKind::Unsupported,
        format!("{what} is not supported on this platform"),
    )
}

/// Record `error` for `operation`. An error this module builds itself carries
/// no OS code, so it records code 0 with its portable kind.
fn record_error(operation: &str, error: &io::Error) {
    set_file_io_error_or(operation, error, 0);
}

/// An argument no platform call could accept: `EINVAL` on Unix.
fn invalid_argument(what: &str) -> io::Error {
    #[cfg(unix)]
    {
        let _ = what;
        io::Error::from_raw_os_error(libc::EINVAL)
    }
    #[cfg(not(unix))]
    {
        io::Error::new(io::ErrorKind::InvalidInput, what.to_string())
    }
}

/// The refusal for a path that opened but is not a regular file: `EISDIR`
/// for a directory and `EINVAL` for anything else on Unix.
fn not_regular(metadata: &std::fs::Metadata) -> io::Error {
    #[cfg(unix)]
    {
        let code = if metadata.is_dir() {
            libc::EISDIR
        } else {
            libc::EINVAL
        };
        io::Error::from_raw_os_error(code)
    }
    #[cfg(not(unix))]
    {
        let _ = metadata;
        io::Error::new(
            io::ErrorKind::InvalidInput,
            "path does not name a regular file",
        )
    }
}

fn saturating_i64(value: u64) -> i64 {
    i64::try_from(value).unwrap_or(i64::MAX)
}

/// The permission bits a mode argument may carry.
#[cfg(unix)]
fn permission_bits(mode: i64) -> io::Result<u32> {
    u32::try_from(mode)
        .ok()
        .filter(|bits| *bits <= 0o7777)
        .ok_or_else(|| io::Error::from_raw_os_error(libc::EINVAL))
}

fn describe(metadata: &std::fs::Metadata) -> HewFsMetadata {
    let file_type = metadata.file_type();
    let kind = if file_type.is_symlink() {
        KIND_SYMLINK
    } else if file_type.is_dir() {
        KIND_DIRECTORY
    } else if file_type.is_file() {
        KIND_FILE
    } else {
        KIND_OTHER
    };
    #[cfg(unix)]
    let (mode, owner) = {
        use std::os::unix::fs::MetadataExt;
        (
            i64::from(metadata.mode() & 0o7777),
            i64::from(metadata.uid()),
        )
    };
    #[cfg(not(unix))]
    let (mode, owner) = (-1, -1);
    HewFsMetadata {
        status: 0,
        kind,
        size: saturating_i64(metadata.len()),
        mode,
        owner,
    }
}

const FAILED_METADATA: HewFsMetadata = HewFsMetadata {
    status: -1,
    kind: KIND_OTHER,
    size: 0,
    mode: -1,
    owner: -1,
};

unsafe fn metadata_entry(
    path: *const HewString,
    operation: &str,
    read: fn(&Path) -> io::Result<std::fs::Metadata>,
) -> HewFsMetadata {
    // SAFETY: path is a borrowed managed handle per the caller's contract.
    let Some(target) = (unsafe { file_path(path, operation) }) else {
        return FAILED_METADATA;
    };
    match read(Path::new(&target)) {
        Ok(metadata) => {
            clear_file_io_error();
            describe(&metadata)
        }
        Err(error) => {
            record_error(operation, &error);
            FAILED_METADATA
        }
    }
}

/// Metadata of the file `path` names, following symbolic links.
///
/// # Safety
///
/// `path` must be a live managed string handle (null spells empty).
#[no_mangle]
pub unsafe extern "C" fn hew_fs_metadata(path: *const HewString) -> HewFsMetadata {
    // SAFETY: forwarded caller contract.
    unsafe { metadata_entry(path, "hew_fs_metadata", |path| std::fs::metadata(path)) }
}

/// Metadata of `path` itself: a symbolic link describes the link.
///
/// # Safety
///
/// `path` must be a live managed string handle (null spells empty).
#[no_mangle]
pub unsafe extern "C" fn hew_fs_symlink_metadata(path: *const HewString) -> HewFsMetadata {
    // SAFETY: forwarded caller contract.
    unsafe {
        metadata_entry(path, "hew_fs_symlink_metadata", |path| {
            std::fs::symlink_metadata(path)
        })
    }
}

/// The user that owns files this process creates (the effective user ID), or
/// -1 where the platform has no numeric owner (Windows).
#[no_mangle]
pub extern "C" fn hew_fs_current_owner() -> i64 {
    #[cfg(unix)]
    {
        // SAFETY: geteuid has no preconditions and cannot fail.
        i64::from(unsafe { libc::geteuid() })
    }
    #[cfg(not(unix))]
    {
        -1
    }
}

/// The file `create_new` made, removed again unless the creation completes.
#[cfg(unix)]
struct CreatedFile<'a> {
    path: &'a Path,
    file: File,
}

#[cfg(unix)]
impl CreatedFile<'_> {
    /// Remove the file this call created. The path is unlinked only while it
    /// still names the open descriptor's file, so a file put in its place
    /// since is left alone.
    fn discard(self) {
        use std::os::unix::fs::MetadataExt;
        let ours = self.file.metadata();
        let named = std::fs::symlink_metadata(self.path);
        if let (Ok(ours), Ok(named)) = (ours, named) {
            if (ours.dev(), ours.ino()) == (named.dev(), named.ino()) {
                let _ = std::fs::remove_file(self.path);
            }
        }
    }
}

#[cfg(unix)]
fn create_new(target: &Path, data: &[u8], mode: i64) -> io::Result<()> {
    use std::io::Write;
    use std::os::unix::fs::{MetadataExt, OpenOptionsExt, PermissionsExt};
    let mode = permission_bits(mode)?;
    let file = OpenOptions::new()
        .write(true)
        .create_new(true)
        .mode(mode & 0o777)
        .custom_flags(libc::O_NOFOLLOW | libc::O_CLOEXEC)
        .open(target)?;
    let mut created = CreatedFile { path: target, file };
    // Write before setting the mode: a write by a non-root process clears
    // the set-user-ID and set-group-ID bits. The mode is then set through
    // the descriptor, past the umask, and read back, since the system may
    // decline a bit (set-group-ID for a group the user is not in).
    let completed = created
        .file
        .write_all(data)
        .and_then(|()| {
            created
                .file
                .set_permissions(std::fs::Permissions::from_mode(mode))
        })
        .and_then(|()| created.file.metadata())
        .and_then(|metadata| {
            if metadata.mode() & 0o7777 == mode {
                Ok(())
            } else {
                Err(io::Error::from_raw_os_error(libc::EPERM))
            }
        })
        .and_then(|()| created.file.sync_all());
    if let Err(error) = completed {
        created.discard();
        return Err(error);
    }
    Ok(())
}

#[cfg(not(unix))]
fn create_new(_target: &Path, _data: &[u8], _mode: i64) -> io::Result<()> {
    Err(unsupported("creating a file with a Unix permission mode"))
}

/// Create the file `path`, which must not exist, write `data`, set exactly the
/// permission bits `mode` and flush it to stable storage. A final symbolic
/// link is refused. A mode bit the system declines (set-group-ID for a group
/// the user is not in) fails with `EPERM`. On failure after creation the file
/// is removed.
///
/// Returns 0, or -1 with the error in the stream slot.
///
/// # Safety
///
/// `path` must be a live managed string handle (null spells empty). `data`
/// must point to a valid `BytesTriple`.
#[no_mangle]
pub unsafe extern "C" fn hew_fs_create_new(
    path: *const HewString,
    data: *const BytesTriple,
    mode: i64,
) -> i32 {
    // SAFETY: path is a borrowed managed handle.
    let Some(target) = (unsafe { file_path(path, "hew_fs_create_new") }) else {
        return FAILED;
    };
    // SAFETY: data is a valid borrowed triple per the contract.
    let data = unsafe { crate::bytes::active(&*data) };
    match create_new(Path::new(&target), data, mode) {
        Ok(()) => {
            clear_file_io_error();
            OK
        }
        Err(error) => {
            record_error("hew_fs_create_new", &error);
            FAILED
        }
    }
}

#[cfg(unix)]
fn mkdir_mode(target: &Path, mode: i64) -> io::Result<()> {
    use std::os::unix::fs::{DirBuilderExt, OpenOptionsExt, PermissionsExt};
    let mode = permission_bits(mode)?;
    std::fs::DirBuilder::new().mode(mode).create(target)?;
    // Set the exact mode on the directory this call created, without
    // following a link put in its place since.
    OpenOptions::new()
        .read(true)
        .custom_flags(libc::O_NOFOLLOW | libc::O_DIRECTORY | libc::O_CLOEXEC)
        .open(target)?
        .set_permissions(std::fs::Permissions::from_mode(mode))
}

#[cfg(not(unix))]
fn mkdir_mode(_target: &Path, _mode: i64) -> io::Result<()> {
    Err(unsupported(
        "creating a directory with a Unix permission mode",
    ))
}

/// Create the directory `path`, whose parent must exist, with exactly the
/// permission bits `mode`.
///
/// Returns 0, or -1 with the error in the stream slot.
///
/// # Safety
///
/// `path` must be a live managed string handle (null spells empty).
#[no_mangle]
pub unsafe extern "C" fn hew_fs_mkdir_mode(path: *const HewString, mode: i64) -> i32 {
    // SAFETY: path is a borrowed managed handle.
    let Some(target) = (unsafe { file_path(path, "hew_fs_mkdir_mode") }) else {
        return FAILED;
    };
    match mkdir_mode(Path::new(&target), mode) {
        Ok(()) => {
            clear_file_io_error();
            OK
        }
        Err(error) => {
            record_error("hew_fs_mkdir_mode", &error);
            FAILED
        }
    }
}

/// Open `target` for reading without following a final symbolic link.
#[cfg(unix)]
fn open_nofollow(target: &Path) -> io::Result<File> {
    use std::os::unix::fs::OpenOptionsExt;
    // O_NONBLOCK keeps a FIFO from blocking the open; the type check after it
    // refuses anything but a regular file.
    OpenOptions::new()
        .read(true)
        .custom_flags(libc::O_NOFOLLOW | libc::O_CLOEXEC | libc::O_NONBLOCK)
        .open(target)
}

/// Open `target` for reading without following a final symbolic link.
#[cfg(windows)]
fn open_nofollow(target: &Path) -> io::Result<File> {
    use std::os::windows::fs::OpenOptionsExt;
    use windows_sys::Win32::Storage::FileSystem::FILE_FLAG_OPEN_REPARSE_POINT;
    let file = OpenOptions::new()
        .read(true)
        .custom_flags(FILE_FLAG_OPEN_REPARSE_POINT)
        .open(target)?;
    if file.metadata()?.file_type().is_symlink() {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            "path names a symbolic link",
        ));
    }
    Ok(file)
}

/// Whether the opened file is private to this process's user: owned by it,
/// with no group or other permission bits.
#[cfg(unix)]
fn check_private(metadata: &std::fs::Metadata) -> io::Result<()> {
    use std::os::unix::fs::MetadataExt;
    // SAFETY: geteuid has no preconditions and cannot fail.
    let me = unsafe { libc::geteuid() };
    if metadata.uid() != me || metadata.mode() & 0o077 != 0 {
        return Err(io::Error::from_raw_os_error(libc::EACCES));
    }
    Ok(())
}

#[cfg(not(unix))]
fn check_private(_metadata: &std::fs::Metadata) -> io::Result<()> {
    Err(unsupported("checking a file's owner and permission mode"))
}

/// Read a regular file of at most `limit` bytes, opened without following a
/// final symbolic link. With `private`, the opened file must also be owned by
/// this process's user and grant nothing to group or other. Every check reads
/// the file that was opened, so a path swapped in between is never read.
fn read_nofollow(target: &Path, limit: i64, private: bool) -> io::Result<Vec<u8>> {
    let limit = u64::try_from(limit).map_err(|_| invalid_argument("negative read limit"))?;
    let mut file = open_nofollow(target)?;
    let metadata = file.metadata()?;
    if !metadata.is_file() {
        return Err(not_regular(&metadata));
    }
    if private {
        check_private(&metadata)?;
    }
    let too_large = || {
        #[cfg(unix)]
        let error = io::Error::from_raw_os_error(libc::EFBIG);
        #[cfg(not(unix))]
        let error = io::Error::new(io::ErrorKind::InvalidData, "file exceeds the limit");
        error
    };
    if metadata.len() > limit {
        return Err(too_large());
    }
    let mut data = Vec::new();
    // Read one byte past the limit so a file that grew after the check is
    // refused rather than truncated.
    Read::by_ref(&mut file)
        .take(limit.saturating_add(1))
        .read_to_end(&mut data)?;
    if u64::try_from(data.len()).unwrap_or(u64::MAX) > limit {
        return Err(too_large());
    }
    Ok(data)
}

const EMPTY: BytesTriple = BytesTriple {
    ptr: std::ptr::null_mut(),
    offset: 0,
    len: 0,
};

unsafe fn read_entry(
    path: *const HewString,
    limit: i64,
    private: bool,
    operation: &str,
) -> BytesTriple {
    // SAFETY: path is a borrowed managed handle per the caller's contract.
    let Some(target) = (unsafe { file_path(path, operation) }) else {
        return EMPTY;
    };
    match read_nofollow(Path::new(&target), limit, private) {
        Ok(data) => {
            let Ok(len) = u32::try_from(data.len()) else {
                set_file_io_errno(operation, "file exceeds bytes capacity", libc::EFBIG);
                return EMPTY;
            };
            clear_file_io_error();
            // SAFETY: data is readable for len bytes.
            unsafe { crate::bytes::hew_bytes_from_static(data.as_ptr(), len) }
        }
        Err(error) => {
            record_error(operation, &error);
            EMPTY
        }
    }
}

/// Read a regular file of at most `limit` bytes without following a final
/// symbolic link. Returns the empty value on failure with the error in the
/// stream slot.
///
/// # Safety
///
/// `path` must be a live managed string handle (null spells empty).
#[no_mangle]
pub unsafe extern "C" fn hew_fs_read_nofollow(path: *const HewString, limit: i64) -> BytesTriple {
    // SAFETY: forwarded caller contract.
    unsafe { read_entry(path, limit, false, "hew_fs_read_nofollow") }
}

/// As [`hew_fs_read_nofollow`], and the opened file must be owned by this
/// process's user with no group or other permission bits.
///
/// # Safety
///
/// `path` must be a live managed string handle (null spells empty).
#[no_mangle]
pub unsafe extern "C" fn hew_fs_read_private(path: *const HewString, limit: i64) -> BytesTriple {
    // SAFETY: forwarded caller contract.
    unsafe { read_entry(path, limit, true, "hew_fs_read_private") }
}

/// Open or create the lock file `target` (mode 0600 on Unix, never through a
/// final symbolic link) and take its exclusive lock without waiting.
fn try_lock(target: &Path) -> io::Result<Option<File>> {
    let mut options = OpenOptions::new();
    options.read(true).write(true).create(true);
    #[cfg(unix)]
    {
        use std::os::unix::fs::OpenOptionsExt;
        options
            .mode(0o600)
            .custom_flags(libc::O_NOFOLLOW | libc::O_CLOEXEC);
    }
    #[cfg(windows)]
    {
        use std::os::windows::fs::OpenOptionsExt;
        use windows_sys::Win32::Storage::FileSystem::FILE_FLAG_OPEN_REPARSE_POINT;
        options.custom_flags(FILE_FLAG_OPEN_REPARSE_POINT);
    }
    let file = options.open(target)?;
    let metadata = file.metadata()?;
    if !metadata.is_file() {
        return Err(not_regular(&metadata));
    }
    match file.try_lock() {
        Ok(()) => Ok(Some(file)),
        Err(std::fs::TryLockError::WouldBlock) => Ok(None),
        Err(std::fs::TryLockError::Error(error)) => Err(error),
    }
}

/// A held file lock: the open lock file whose descriptor carries the lock.
/// Dropping it closes the descriptor, which releases the lock.
#[derive(Debug)]
pub struct HewFileLock {
    _file: File,
}

/// Take the exclusive lock on the file `path`, creating it if
/// absent, without waiting.
///
/// Returns the lock for [`hew_fs_unlock`], or null: with the stream error
/// slot clear when another holder has the lock, or set on failure.
///
/// # Safety
///
/// `path` must be a live managed string handle (null spells empty).
#[no_mangle]
pub unsafe extern "C" fn hew_fs_try_lock(path: *const HewString) -> *mut HewFileLock {
    // SAFETY: path is a borrowed managed handle.
    let Some(target) = (unsafe { file_path(path, "hew_fs_try_lock") }) else {
        return std::ptr::null_mut();
    };
    match try_lock(Path::new(&target)) {
        Ok(Some(file)) => {
            clear_file_io_error();
            Box::into_raw(Box::new(HewFileLock { _file: file }))
        }
        Ok(None) => {
            clear_file_io_error();
            std::ptr::null_mut()
        }
        Err(error) => {
            record_error("hew_fs_try_lock", &error);
            std::ptr::null_mut()
        }
    }
}

/// Whether [`hew_fs_try_lock`] returned a held lock.
#[no_mangle]
pub extern "C" fn hew_fs_lock_is_valid(lock: *const HewFileLock) -> bool {
    !lock.is_null()
}

/// Release a lock [`hew_fs_try_lock`] returned. Null is a no-op.
///
/// # Safety
///
/// `lock` must be null or a lock `hew_fs_try_lock` returned that has not been
/// released.
#[no_mangle]
pub unsafe extern "C" fn hew_fs_unlock(lock: *mut HewFileLock) {
    if lock.is_null() {
        return;
    }
    // SAFETY: lock came from Box::into_raw in hew_fs_try_lock and is released once.
    drop(unsafe { Box::from_raw(lock) });
}

#[cfg(unix)]
fn space(target: &str) -> io::Result<HewFsSpace> {
    let path = std::ffi::CString::new(target).map_err(|_| invalid_argument("path contains NUL"))?;
    // SAFETY: statvfs is plain data; an all-zero value is valid storage.
    let mut info: libc::statvfs = unsafe { std::mem::zeroed() };
    // SAFETY: path is NUL-terminated and info is writable storage.
    if unsafe { libc::statvfs(path.as_ptr(), &raw mut info) } != 0 {
        return Err(io::Error::last_os_error());
    }
    #[allow(
        clippy::useless_conversion,
        reason = "statvfs field widths differ between Unix targets"
    )]
    let bytes = |blocks| saturating_i64(u64::from(blocks).saturating_mul(u64::from(info.f_frsize)));
    Ok(HewFsSpace {
        status: 0,
        total: bytes(info.f_blocks),
        free: bytes(info.f_bfree),
        available: bytes(info.f_bavail),
    })
}

#[cfg(windows)]
fn space(target: &str) -> io::Result<HewFsSpace> {
    use std::os::windows::ffi::OsStrExt;
    use windows_sys::Win32::Storage::FileSystem::GetDiskFreeSpaceExW;
    // GetDiskFreeSpaceExW takes a directory; a file reports the volume of
    // the directory that holds it, as statvfs does on Unix.
    let target = Path::new(target);
    let directory = if std::fs::metadata(target)?.is_dir() {
        target
    } else {
        match target.parent() {
            Some(parent) if !parent.as_os_str().is_empty() => parent,
            _ => Path::new("."),
        }
    };
    let wide: Vec<u16> = directory
        .as_os_str()
        .encode_wide()
        .chain(std::iter::once(0))
        .collect();
    let (mut available, mut total, mut free) = (0u64, 0u64, 0u64);
    // SAFETY: wide is NUL-terminated and the outputs are writable u64 storage.
    let ok = unsafe {
        GetDiskFreeSpaceExW(
            wide.as_ptr(),
            &raw mut available,
            &raw mut total,
            &raw mut free,
        )
    };
    if ok == 0 {
        return Err(io::Error::last_os_error());
    }
    Ok(HewFsSpace {
        status: 0,
        total: saturating_i64(total),
        free: saturating_i64(free),
        available: saturating_i64(available),
    })
}

/// The total, free and caller-available capacity of the file system holding
/// `path`, in bytes.
///
/// # Safety
///
/// `path` must be a live managed string handle (null spells empty).
#[no_mangle]
pub unsafe extern "C" fn hew_fs_space(path: *const HewString) -> HewFsSpace {
    const FAILED_SPACE: HewFsSpace = HewFsSpace {
        status: -1,
        total: 0,
        free: 0,
        available: 0,
    };
    // SAFETY: path is a borrowed managed handle.
    let Some(target) = (unsafe { file_path(path, "hew_fs_space") }) else {
        return FAILED_SPACE;
    };
    match space(&target) {
        Ok(space) => {
            clear_file_io_error();
            space
        }
        Err(error) => {
            record_error("hew_fs_space", &error);
            FAILED_SPACE
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use hew_cabi::string::string_from_str;

    struct Scratch(std::path::PathBuf);

    impl Scratch {
        fn new(name: &str) -> Self {
            let dir =
                std::env::temp_dir().join(format!("hew-file-secure-{name}-{}", std::process::id()));
            let _ = std::fs::remove_dir_all(&dir);
            std::fs::create_dir_all(&dir).unwrap();
            Self(dir)
        }
    }

    impl Drop for Scratch {
        fn drop(&mut self) {
            let _ = std::fs::remove_dir_all(&self.0);
        }
    }

    fn managed(path: &Path) -> *mut HewString {
        string_from_str(path.to_str().unwrap())
    }

    fn triple(data: &[u8]) -> BytesTriple {
        BytesTriple {
            ptr: data.as_ptr().cast_mut(),
            offset: 0,
            len: u32::try_from(data.len()).unwrap(),
        }
    }

    fn release(path: *mut HewString) {
        // SAFETY: each test releases the managed path it created once.
        unsafe { crate::string::hew_string_drop(path) };
    }

    #[cfg(unix)]
    #[test]
    fn create_new_sets_the_exact_mode_and_refuses_an_existing_path() {
        use std::os::unix::fs::PermissionsExt;
        let scratch = Scratch::new("create");
        let target = scratch.0.join("key");
        let path = managed(&target);
        let data = triple(b"secret");
        // SAFETY: the path and triple outlive both calls.
        unsafe {
            assert_eq!(hew_fs_create_new(path, &raw const data, 0o600), OK);
            assert_eq!(hew_fs_create_new(path, &raw const data, 0o600), FAILED);
        }
        assert_eq!(crate::stream_error::take_last_errno(), libc::EEXIST);
        let mode = std::fs::metadata(&target).unwrap().permissions().mode();
        assert_eq!(mode & 0o7777, 0o600);
        assert_eq!(std::fs::read(&target).unwrap(), b"secret");
        release(path);
    }

    #[cfg(unix)]
    #[test]
    fn create_new_refuses_a_dangling_symlink() {
        let scratch = Scratch::new("create-link");
        let link = scratch.0.join("link");
        std::os::unix::fs::symlink(scratch.0.join("elsewhere"), &link).unwrap();
        let path = managed(&link);
        let data = triple(b"x");
        // SAFETY: the path and triple outlive the call.
        let status = unsafe { hew_fs_create_new(path, &raw const data, 0o600) };
        assert_eq!(status, FAILED);
        assert!(!scratch.0.join("elsewhere").exists());
        release(path);
    }

    #[cfg(unix)]
    #[test]
    fn mkdir_mode_overrides_the_umask() {
        use std::os::unix::fs::PermissionsExt;
        let scratch = Scratch::new("mkdir");
        let target = scratch.0.join("private");
        let path = managed(&target);
        // SAFETY: the path outlives the call.
        assert_eq!(unsafe { hew_fs_mkdir_mode(path, 0o700) }, OK);
        let mode = std::fs::metadata(&target).unwrap().permissions().mode();
        assert_eq!(mode & 0o7777, 0o700);
        // SAFETY: the path outlives the call.
        assert_eq!(unsafe { hew_fs_mkdir_mode(path, 0o70000) }, FAILED);
        release(path);
    }

    #[cfg(unix)]
    #[test]
    fn metadata_distinguishes_a_link_from_its_target() {
        let scratch = Scratch::new("meta");
        let file = scratch.0.join("file");
        std::fs::write(&file, b"abc").unwrap();
        let link = scratch.0.join("link");
        std::os::unix::fs::symlink(&file, &link).unwrap();
        let path = managed(&link);
        // SAFETY: the path outlives both calls.
        let (followed, own) = unsafe { (hew_fs_metadata(path), hew_fs_symlink_metadata(path)) };
        assert_eq!(
            (followed.status, followed.kind, followed.size),
            (0, KIND_FILE, 3)
        );
        assert_eq!((own.status, own.kind), (0, KIND_SYMLINK));
        assert_eq!(followed.owner, hew_fs_current_owner());
        release(path);
    }

    #[cfg(unix)]
    #[test]
    fn read_private_checks_the_opened_file() {
        use std::os::unix::fs::PermissionsExt;
        let scratch = Scratch::new("read");
        let file = scratch.0.join("token");
        std::fs::write(&file, b"tok").unwrap();
        std::fs::set_permissions(&file, std::fs::Permissions::from_mode(0o600)).unwrap();
        let link = scratch.0.join("link");
        std::os::unix::fs::symlink(&file, &link).unwrap();
        let (file_path, link_path) = (managed(&file), managed(&link));
        // SAFETY: the paths outlive every call; each result is released once.
        unsafe {
            let data = hew_fs_read_private(file_path, 16);
            assert_eq!(crate::bytes::active(&data), b"tok");
            crate::bytes::hew_bytes_drop(data.ptr);
            let refused = hew_fs_read_private(link_path, 16);
            assert!(refused.ptr.is_null());
            assert!(crate::stream_error::take_last_errno() != 0);
            let small = hew_fs_read_nofollow(file_path, 2);
            assert!(small.ptr.is_null());
            assert_eq!(crate::stream_error::take_last_errno(), libc::EFBIG);
            std::fs::set_permissions(&file, std::fs::Permissions::from_mode(0o640)).unwrap();
            let shared = hew_fs_read_private(file_path, 16);
            assert!(shared.ptr.is_null());
            assert_eq!(crate::stream_error::take_last_errno(), libc::EACCES);
            let open = hew_fs_read_nofollow(file_path, 16);
            assert_eq!(crate::bytes::active(&open), b"tok");
            crate::bytes::hew_bytes_drop(open.ptr);
        }
        release(file_path);
        release(link_path);
    }

    /// The code and kind a refused read records, with the empty result.
    #[cfg(unix)]
    fn refused_read(path: &Path, limit: i64) -> (i32, i32) {
        let managed_path = managed(path);
        // SAFETY: the path outlives the call; a refusal returns no buffer.
        let result = unsafe { hew_fs_read_nofollow(managed_path, limit) };
        release(managed_path);
        assert!(result.ptr.is_null());
        let kind = crate::stream_error::take_last_error_kind();
        (crate::stream_error::take_last_errno(), kind)
    }

    #[cfg(unix)]
    #[test]
    fn read_nofollow_refusals_carry_their_os_code() {
        let scratch = Scratch::new("refuse");
        let fifo = scratch.0.join("fifo");
        let fifo_path = std::ffi::CString::new(fifo.to_str().unwrap()).unwrap();
        // SAFETY: fifo_path is NUL-terminated.
        assert_eq!(unsafe { libc::mkfifo(fifo_path.as_ptr(), 0o600) }, 0);
        let file = scratch.0.join("data");
        std::fs::write(&file, b"abc").unwrap();
        let unclassified = crate::stream_error::IO_ERROR_KIND_UNCLASSIFIED;
        assert_eq!(refused_read(&scratch.0, 16), (libc::EISDIR, unclassified));
        assert_eq!(refused_read(&fifo, 16), (libc::EINVAL, unclassified));
        assert_eq!(
            refused_read(Path::new("/dev/null"), 16),
            (libc::EINVAL, unclassified)
        );
        assert_eq!(refused_read(&file, -1), (libc::EINVAL, unclassified));
    }

    #[cfg(unix)]
    #[test]
    fn create_new_keeps_the_set_user_id_bit_after_writing() {
        use std::os::unix::fs::PermissionsExt;
        let scratch = Scratch::new("suid");
        let target = scratch.0.join("tool");
        let path = managed(&target);
        let data = triple(b"#!/bin/sh\n");
        // SAFETY: the path and triple outlive the call.
        let status = unsafe { hew_fs_create_new(path, &raw const data, 0o4750) };
        assert_eq!(status, OK);
        let mode = std::fs::metadata(&target).unwrap().permissions().mode();
        assert_eq!(mode & 0o7777, 0o4750);
        assert_eq!(std::fs::read(&target).unwrap(), b"#!/bin/sh\n");
        release(path);
    }

    #[test]
    fn try_lock_is_exclusive_until_unlock() {
        let scratch = Scratch::new("lock");
        let path = managed(&scratch.0.join("state.lock"));
        // SAFETY: the path outlives every call; each lock is released once.
        unsafe {
            let held = hew_fs_try_lock(path);
            assert!(hew_fs_lock_is_valid(held));
            let contended = hew_fs_try_lock(path);
            assert!(contended.is_null());
            assert!(!crate::stream_error::hew_stream_has_error());
            hew_fs_unlock(held);
            let again = hew_fs_try_lock(path);
            assert!(hew_fs_lock_is_valid(again));
            hew_fs_unlock(again);
        }
        release(path);
    }

    #[test]
    fn space_reports_a_plausible_capacity() {
        let scratch = Scratch::new("space");
        let path = managed(&scratch.0);
        // SAFETY: the path outlives the call.
        let space = unsafe { hew_fs_space(path) };
        assert_eq!(space.status, 0);
        assert!(space.total > 0);
        assert!(space.free <= space.total && space.available <= space.free);
        release(path);
        let file = scratch.0.join("data");
        std::fs::write(&file, b"x").unwrap();
        let file_path = managed(&file);
        // SAFETY: the path outlives the call.
        let of_file = unsafe { hew_fs_space(file_path) };
        assert_eq!((of_file.status, of_file.total), (0, space.total));
        release(file_path);
        let missing = managed(&scratch.0.join("absent"));
        // SAFETY: the path outlives the call.
        assert_eq!(unsafe { hew_fs_space(missing) }.status, -1);
        release(missing);
    }

    #[cfg(windows)]
    #[test]
    fn unix_modes_are_unsupported_on_windows() {
        let scratch = Scratch::new("windows");
        let path = managed(&scratch.0.join("key"));
        let data = triple(b"x");
        // SAFETY: the path and triple outlive the calls.
        unsafe {
            assert_eq!(hew_fs_create_new(path, &raw const data, 0o600), FAILED);
            assert_eq!(
                crate::stream_error::hew_stream_last_error_kind(),
                crate::stream_error::IO_ERROR_KIND_UNSUPPORTED
            );
            assert_eq!(crate::stream_error::take_last_errno(), 0);
        }
        release(path);
    }
}
