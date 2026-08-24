use std::io;
use std::path::Path;

use sha2::{Digest, Sha256};

const SKIPPED_PACKAGE_DIRS: &[&str] = &[".git", "target", ".hew"];
pub const CACHE_METADATA_FILE: &str = ".hew-registry-cache.toml";
pub const CACHE_ARCHIVE_FILE: &str = ".hew-registry-cache.tar.zst";
const TREE_DIGEST_DOMAIN: &[u8; 23] = b"hew.package-tree-digest";
const TREE_DIGEST_VERSION: [u8; 4] = 1_u32.to_be_bytes();
const TREE_ENTRY_REGULAR_FILE: u8 = 1;

/// One regular file captured from a package tree.
#[derive(Debug, Eq, PartialEq)]
pub(crate) struct PackageFile {
    pub(crate) path: String,
    pub(crate) contents: Vec<u8>,
}

/// Incremental canonical digest of a package's regular-file tree.
pub(crate) struct TreeDigest {
    hasher: Sha256,
}

impl TreeDigest {
    pub(crate) fn new() -> Self {
        let mut hasher = Sha256::new();
        hasher.update(TREE_DIGEST_DOMAIN);
        hasher.update(TREE_DIGEST_VERSION);
        Self { hasher }
    }

    pub(crate) fn add_file(&mut self, canonical_path: &str, contents: &[u8]) {
        self.hasher.update([TREE_ENTRY_REGULAR_FILE]);
        self.hasher
            .update((canonical_path.len() as u64).to_be_bytes());
        self.hasher.update(canonical_path.as_bytes());
        self.hasher.update((contents.len() as u64).to_be_bytes());
        self.hasher.update(contents);
    }

    pub(crate) fn finish(self) -> String {
        format_sha256(&self.hasher.finalize())
    }
}

/// Return true when a directory should be skipped for package traversal.
#[must_use]
pub fn is_skipped_package_dir(name: &str) -> bool {
    SKIPPED_PACKAGE_DIRS.contains(&name)
}

fn is_skipped_package_entry(name: &str) -> bool {
    is_skipped_package_dir(name) || name == CACHE_METADATA_FILE || name == CACHE_ARCHIVE_FILE
}

/// Capture a package tree as sorted canonical `/`-separated paths and bytes.
pub(crate) fn collect_package_snapshot(root: &Path) -> Result<Vec<PackageFile>, io::Error> {
    let mut files = platform::collect(root)?;
    files.sort_by(|left, right| left.path.cmp(&right.path));
    Ok(files)
}

fn invalid_name(path: &str) -> io::Error {
    io::Error::new(
        io::ErrorKind::InvalidData,
        format!("package path is not valid UTF-8: {path}"),
    )
}

fn invalid_component(path: &str) -> io::Error {
    io::Error::new(
        io::ErrorKind::InvalidData,
        format!("package path contains a traversal component: {path}"),
    )
}

fn unsupported_file_type(path: &str, kind: &str) -> io::Error {
    io::Error::new(
        io::ErrorKind::InvalidData,
        format!("unsupported {kind} in package tree: {path}"),
    )
}

fn changed_during_snapshot(path: &str) -> io::Error {
    io::Error::new(
        io::ErrorKind::InvalidData,
        format!("package entry changed while being read: {path}"),
    )
}

fn canonical_child(prefix: &str, name: &str) -> Result<String, io::Error> {
    if name.is_empty() || name == "." || name == ".." || name.contains('/') {
        return Err(invalid_component(name));
    }
    Ok(if prefix.is_empty() {
        name.to_owned()
    } else {
        format!("{prefix}/{name}")
    })
}

#[cfg(unix)]
mod platform {
    use std::ffi::{CStr, CString};
    use std::fs::File;
    use std::io::{self, Read as _};
    use std::mem::MaybeUninit;
    use std::os::fd::{AsRawFd as _, BorrowedFd, FromRawFd as _, IntoRawFd as _, OwnedFd, RawFd};
    use std::os::unix::ffi::OsStrExt as _;
    use std::os::unix::fs::MetadataExt as _;
    use std::path::Path;
    use std::ptr::NonNull;

    use super::{
        canonical_child, changed_during_snapshot, invalid_component, invalid_name,
        is_skipped_package_entry, unsupported_file_type, PackageFile,
    };

    struct DirStream(NonNull<libc::DIR>);

    impl DirStream {
        fn open(fd: OwnedFd) -> io::Result<Self> {
            let raw_fd = fd.into_raw_fd();
            // SAFETY: `raw_fd` is a valid owned directory descriptor. On
            // success, fdopendir takes ownership of it.
            let stream = unsafe { libc::fdopendir(raw_fd) };
            if let Some(stream) = NonNull::new(stream) {
                Ok(Self(stream))
            } else {
                let error = io::Error::last_os_error();
                // SAFETY: fdopendir failed, so ownership remained here.
                unsafe {
                    libc::close(raw_fd);
                }
                Err(error)
            }
        }

        fn fd(&self) -> RawFd {
            // SAFETY: the stream remains live for the duration of the call.
            unsafe { libc::dirfd(self.0.as_ptr()) }
        }
    }

    impl Drop for DirStream {
        fn drop(&mut self) {
            // SAFETY: fdopendir owns the descriptor and closedir releases both
            // the stream and that descriptor exactly once.
            unsafe {
                libc::closedir(self.0.as_ptr());
            }
        }
    }

    pub(super) fn collect(root: &Path) -> io::Result<Vec<PackageFile>> {
        collect_with_hook(root, &mut |_| {})
    }

    fn collect_with_hook<F>(root: &Path, hook: &mut F) -> io::Result<Vec<PackageFile>>
    where
        F: FnMut(&str),
    {
        let root = CString::new(root.as_os_str().as_bytes()).map_err(|_| {
            io::Error::new(
                io::ErrorKind::InvalidInput,
                "package root contains an interior NUL",
            )
        })?;
        let flags = libc::O_RDONLY | libc::O_CLOEXEC | libc::O_DIRECTORY | libc::O_NOFOLLOW;
        // SAFETY: `root` is a valid NUL-terminated pathname.
        let raw_fd = unsafe { libc::open(root.as_ptr(), flags) };
        if raw_fd < 0 {
            return Err(io::Error::last_os_error());
        }
        // SAFETY: open returned a new owned descriptor.
        let root_fd = unsafe { OwnedFd::from_raw_fd(raw_fd) };
        let root_stat = stat_fd(root_fd.as_raw_fd())?;
        if file_kind(root_stat.st_mode) != libc::S_IFDIR {
            return Err(unsupported_file_type(
                &root.to_string_lossy(),
                "non-directory package root",
            ));
        }

        let mut files = Vec::new();
        collect_dir(root_fd, "", &mut files, hook)?;
        Ok(files)
    }

    fn collect_dir<F>(
        dir_fd: OwnedFd,
        prefix: &str,
        files: &mut Vec<PackageFile>,
        hook: &mut F,
    ) -> io::Result<()>
    where
        F: FnMut(&str),
    {
        let before = metadata_fd(dir_fd.as_raw_fd())?;
        let stream = DirStream::open(dir_fd)?;

        loop {
            set_errno(0);
            // SAFETY: `stream` is a valid directory stream and readdir's
            // returned pointer is consumed before the next call.
            let entry = unsafe { libc::readdir(stream.0.as_ptr()) };
            if entry.is_null() {
                let errno = get_errno();
                if errno == 0 {
                    break;
                }
                return Err(io::Error::from_raw_os_error(errno));
            }

            // SAFETY: d_name is NUL-terminated for a successful readdir call.
            let name_bytes = unsafe { CStr::from_ptr((*entry).d_name.as_ptr()) }.to_bytes();
            if name_bytes == b"." || name_bytes == b".." {
                continue;
            }
            let name = std::str::from_utf8(name_bytes)
                .map_err(|_| invalid_name(&format!("{prefix}/<non-UTF-8>")))?;
            let path = canonical_child(prefix, name)?;
            let name = CString::new(name_bytes).map_err(|_| invalid_component(&path))?;
            let expected = stat_at(stream.fd(), &name)?;
            let expected_kind = file_kind(expected.st_mode);
            if expected_kind == libc::S_IFLNK {
                return Err(unsupported_file_type(&path, "symbolic link"));
            }
            if is_skipped_package_entry(name.to_str().expect("validated UTF-8 name")) {
                continue;
            }

            hook(&path);
            match expected_kind {
                libc::S_IFDIR => {
                    let child = open_at(
                        stream.fd(),
                        &name,
                        libc::O_RDONLY | libc::O_CLOEXEC | libc::O_DIRECTORY | libc::O_NOFOLLOW,
                    )?;
                    let opened = stat_fd(child.as_raw_fd())?;
                    if !same_identity_and_type(&expected, &opened) {
                        return Err(changed_during_snapshot(&path));
                    }
                    collect_dir(child, &path, files, hook)?;
                }
                libc::S_IFREG => {
                    let file_fd = open_at(
                        stream.fd(),
                        &name,
                        libc::O_RDONLY | libc::O_CLOEXEC | libc::O_NOFOLLOW | libc::O_NONBLOCK,
                    )?;
                    let opened = stat_fd(file_fd.as_raw_fd())?;
                    if !same_identity_and_type(&expected, &opened) {
                        return Err(changed_during_snapshot(&path));
                    }

                    // File::from owns the descriptor from this point onward.
                    let mut file = File::from(file_fd);
                    let before_read = file.metadata()?;
                    if !before_read.is_file() {
                        return Err(unsupported_file_type(&path, "non-regular file"));
                    }
                    let mut contents = Vec::new();
                    file.read_to_end(&mut contents)?;
                    let after_read = file.metadata()?;
                    if !same_file_state(&before_read, &after_read) {
                        return Err(changed_during_snapshot(&path));
                    }
                    files.push(PackageFile { path, contents });
                }
                _ => return Err(unsupported_file_type(&path, "non-regular file")),
            }
        }

        let after = metadata_fd(stream.fd())?;
        if !same_file_state(&before, &after) {
            return Err(changed_during_snapshot(if prefix.is_empty() {
                "."
            } else {
                prefix
            }));
        }
        Ok(())
    }

    fn open_at(dir_fd: RawFd, name: &CStr, flags: libc::c_int) -> io::Result<OwnedFd> {
        // SAFETY: `dir_fd` remains open, and `name` is a valid single-component
        // NUL-terminated name.
        let fd = unsafe { libc::openat(dir_fd, name.as_ptr(), flags) };
        if fd < 0 {
            Err(io::Error::last_os_error())
        } else {
            // SAFETY: openat returned a new owned descriptor.
            Ok(unsafe { OwnedFd::from_raw_fd(fd) })
        }
    }

    fn stat_at(dir_fd: RawFd, name: &CStr) -> io::Result<libc::stat> {
        let mut stat = MaybeUninit::uninit();
        // SAFETY: all pointers are valid and stat points to writable storage.
        let result = unsafe {
            libc::fstatat(
                dir_fd,
                name.as_ptr(),
                stat.as_mut_ptr(),
                libc::AT_SYMLINK_NOFOLLOW,
            )
        };
        if result == 0 {
            // SAFETY: successful fstatat initialized stat.
            Ok(unsafe { stat.assume_init() })
        } else {
            Err(io::Error::last_os_error())
        }
    }

    fn stat_fd(fd: RawFd) -> io::Result<libc::stat> {
        let mut stat = MaybeUninit::uninit();
        // SAFETY: `fd` is open and stat points to writable storage.
        let result = unsafe { libc::fstat(fd, stat.as_mut_ptr()) };
        if result == 0 {
            // SAFETY: successful fstat initialized stat.
            Ok(unsafe { stat.assume_init() })
        } else {
            Err(io::Error::last_os_error())
        }
    }

    fn file_kind(mode: libc::mode_t) -> libc::mode_t {
        mode & libc::S_IFMT
    }

    fn same_identity_and_type(left: &libc::stat, right: &libc::stat) -> bool {
        left.st_dev == right.st_dev
            && left.st_ino == right.st_ino
            && file_kind(left.st_mode) == file_kind(right.st_mode)
    }

    fn metadata_fd(fd: RawFd) -> io::Result<std::fs::Metadata> {
        // SAFETY: the borrowed descriptor remains owned by the caller and open
        // for the duration of the clone.
        let borrowed = unsafe { BorrowedFd::borrow_raw(fd) };
        let duplicate = borrowed.try_clone_to_owned()?;
        File::from(duplicate).metadata()
    }

    fn same_file_state(left: &std::fs::Metadata, right: &std::fs::Metadata) -> bool {
        left.dev() == right.dev()
            && left.ino() == right.ino()
            && left.mode() == right.mode()
            && left.size() == right.size()
            && left.mtime() == right.mtime()
            && left.mtime_nsec() == right.mtime_nsec()
            && left.ctime() == right.ctime()
            && left.ctime_nsec() == right.ctime_nsec()
    }

    #[cfg(any(target_os = "linux", target_os = "android"))]
    fn errno_location() -> *mut libc::c_int {
        // SAFETY: libc returns the calling thread's errno storage.
        unsafe { libc::__errno_location() }
    }

    #[cfg(any(
        target_os = "macos",
        target_os = "ios",
        target_os = "freebsd",
        target_os = "netbsd",
        target_os = "openbsd",
        target_os = "dragonfly"
    ))]
    fn errno_location() -> *mut libc::c_int {
        // SAFETY: libc returns the calling thread's errno storage.
        unsafe { libc::__error() }
    }

    fn set_errno(value: libc::c_int) {
        // SAFETY: errno_location returns writable thread-local errno storage.
        unsafe {
            *errno_location() = value;
        }
    }

    fn get_errno() -> libc::c_int {
        // SAFETY: errno_location returns readable thread-local errno storage.
        unsafe { *errno_location() }
    }

    #[cfg(test)]
    pub(super) fn collect_identity_counterfactual<F>(
        root: &Path,
        mut hook: F,
    ) -> io::Result<Vec<PackageFile>>
    where
        F: FnMut(&str),
    {
        collect_with_hook(root, &mut hook)
    }
}

#[cfg(windows)]
mod platform {
    use std::fs::{File, Metadata, OpenOptions};
    use std::io::{self, Read as _};
    use std::mem::MaybeUninit;
    use std::os::windows::fs::{MetadataExt as _, OpenOptionsExt as _};
    use std::os::windows::io::AsRawHandle as _;
    use std::path::Path;

    use windows_sys::Win32::Storage::FileSystem::{
        GetFileInformationByHandle, BY_HANDLE_FILE_INFORMATION, FILE_ATTRIBUTE_DIRECTORY,
        FILE_ATTRIBUTE_REPARSE_POINT, FILE_FLAG_BACKUP_SEMANTICS, FILE_FLAG_OPEN_REPARSE_POINT,
        FILE_SHARE_DELETE, FILE_SHARE_READ, FILE_SHARE_WRITE,
    };

    use super::{
        canonical_child, changed_during_snapshot, invalid_name, is_skipped_package_entry,
        unsupported_file_type, PackageFile,
    };

    struct OpenedEntry {
        file: File,
        metadata: Metadata,
        identity: WindowsIdentity,
    }

    #[derive(Clone, Copy, Eq, PartialEq)]
    struct WindowsIdentity {
        volume_serial: u32,
        file_index: u64,
        attributes: u32,
    }

    pub(super) fn collect(root: &Path) -> io::Result<Vec<PackageFile>> {
        let root_handle = open_entry(root, true)?;
        validate_metadata(&root_handle.metadata, ".", true)?;
        let mut files = Vec::new();
        collect_dir(root, "", root_handle, &mut files)?;
        Ok(files)
    }

    fn collect_dir(
        dir: &Path,
        prefix: &str,
        dir_handle: OpenedEntry,
        files: &mut Vec<PackageFile>,
    ) -> io::Result<()> {
        for entry in std::fs::read_dir(dir)? {
            let entry = entry?;
            let name = entry
                .file_name()
                .into_string()
                .map_err(|_| invalid_name(&entry.path().display().to_string()))?;
            let path = canonical_child(prefix, &name)?;
            let entry_path = entry.path();
            let expected = std::fs::symlink_metadata(&entry_path)?;
            let is_dir = expected.file_attributes() & FILE_ATTRIBUTE_DIRECTORY != 0;
            validate_metadata(&expected, &path, is_dir)?;
            if is_skipped_package_entry(&name) {
                continue;
            }

            let mut opened = open_entry(&entry_path, is_dir)?;
            validate_metadata(&opened.metadata, &path, is_dir)?;
            if !same_metadata_state(&expected, &opened.metadata) {
                return Err(changed_during_snapshot(&path));
            }
            if is_dir {
                collect_dir(&entry_path, &path, opened, files)?;
            } else {
                let mut contents = Vec::new();
                opened.file.read_to_end(&mut contents)?;
                let after = opened.file.metadata()?;
                if !same_metadata_state(&opened.metadata, &after) {
                    return Err(changed_during_snapshot(&path));
                }
                files.push(PackageFile { path, contents });
            }
        }

        let after = dir_handle.file.metadata()?;
        let reopened = open_entry(dir, true)?;
        if !same_metadata_state(&dir_handle.metadata, &after)
            || dir_handle.identity != reopened.identity
        {
            return Err(changed_during_snapshot(if prefix.is_empty() {
                "."
            } else {
                prefix
            }));
        }
        Ok(())
    }

    fn open_entry(path: &Path, is_dir: bool) -> io::Result<OpenedEntry> {
        let mut options = OpenOptions::new();
        options
            .read(true)
            .share_mode(FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE)
            .custom_flags(
                FILE_FLAG_OPEN_REPARSE_POINT
                    | if is_dir {
                        FILE_FLAG_BACKUP_SEMANTICS
                    } else {
                        0
                    },
            );
        let file = options.open(path)?;
        let metadata = file.metadata()?;
        let identity = file_identity(&file)?;
        Ok(OpenedEntry {
            file,
            metadata,
            identity,
        })
    }

    fn validate_metadata(metadata: &Metadata, path: &str, is_dir: bool) -> io::Result<()> {
        let attributes = metadata.file_attributes();
        if attributes & FILE_ATTRIBUTE_REPARSE_POINT != 0 {
            return Err(unsupported_file_type(path, "reparse point"));
        }
        let opened_is_dir = attributes & FILE_ATTRIBUTE_DIRECTORY != 0;
        if opened_is_dir != is_dir {
            return Err(changed_during_snapshot(path));
        }
        if !is_dir && !metadata.is_file() {
            return Err(unsupported_file_type(path, "non-regular file"));
        }
        Ok(())
    }

    fn same_metadata_state(left: &Metadata, right: &Metadata) -> bool {
        (left.file_attributes() & (FILE_ATTRIBUTE_DIRECTORY | FILE_ATTRIBUTE_REPARSE_POINT))
            == (right.file_attributes() & (FILE_ATTRIBUTE_DIRECTORY | FILE_ATTRIBUTE_REPARSE_POINT))
            && left.file_size() == right.file_size()
            && left.last_write_time() == right.last_write_time()
            && left.creation_time() == right.creation_time()
    }

    fn file_identity(file: &File) -> io::Result<WindowsIdentity> {
        let mut information = MaybeUninit::<BY_HANDLE_FILE_INFORMATION>::uninit();
        // SAFETY: the handle remains valid for the call and `information`
        // points to writable storage of the required type.
        let result =
            unsafe { GetFileInformationByHandle(file.as_raw_handle(), information.as_mut_ptr()) };
        if result == 0 {
            return Err(io::Error::last_os_error());
        }
        // SAFETY: a successful call initialized the structure.
        let information = unsafe { information.assume_init() };
        Ok(WindowsIdentity {
            volume_serial: information.dwVolumeSerialNumber,
            file_index: (u64::from(information.nFileIndexHigh) << 32)
                | u64::from(information.nFileIndexLow),
            attributes: information.dwFileAttributes
                & (FILE_ATTRIBUTE_DIRECTORY | FILE_ATTRIBUTE_REPARSE_POINT),
        })
    }
}

#[cfg(not(any(unix, windows)))]
mod platform {
    use std::fs::File;
    use std::io::{self, Read as _};
    use std::path::Path;

    use super::{
        canonical_child, changed_during_snapshot, invalid_name, is_skipped_package_entry,
        unsupported_file_type, PackageFile,
    };

    pub(super) fn collect(root: &Path) -> io::Result<Vec<PackageFile>> {
        let metadata = std::fs::symlink_metadata(root)?;
        if metadata.file_type().is_symlink() || !metadata.is_dir() {
            return Err(unsupported_file_type(".", "non-directory package root"));
        }
        let mut files = Vec::new();
        collect_dir(root, "", &mut files)?;
        Ok(files)
    }

    fn collect_dir(dir: &Path, prefix: &str, files: &mut Vec<PackageFile>) -> io::Result<()> {
        for entry in std::fs::read_dir(dir)? {
            let entry = entry?;
            let name = entry
                .file_name()
                .into_string()
                .map_err(|_| invalid_name(&entry.path().display().to_string()))?;
            let path = canonical_child(prefix, &name)?;
            let metadata = std::fs::symlink_metadata(entry.path())?;
            if metadata.file_type().is_symlink() {
                return Err(unsupported_file_type(&path, "symbolic link"));
            }
            if is_skipped_package_entry(&name) {
                continue;
            }
            if metadata.is_dir() {
                collect_dir(&entry.path(), &path, files)?;
            } else if metadata.is_file() {
                let mut file = File::open(entry.path())?;
                let before = file.metadata()?;
                let mut contents = Vec::new();
                file.read_to_end(&mut contents)?;
                let after = file.metadata()?;
                if before.len() != after.len() || before.modified()? != after.modified()? {
                    return Err(changed_during_snapshot(&path));
                }
                files.push(PackageFile { path, contents });
            } else {
                return Err(unsupported_file_type(&path, "non-regular file"));
            }
        }
        Ok(())
    }
}

/// Compute a `sha256:{hex}` digest string for raw bytes.
#[must_use]
pub fn sha256_prefixed(data: &[u8]) -> String {
    format_sha256(&Sha256::digest(data))
}

fn format_sha256(hash: &[u8]) -> String {
    use std::fmt::Write as _;

    let mut hex = String::with_capacity(hash.len() * 2);
    for byte in hash {
        write!(&mut hex, "{byte:02x}").expect("writing to a String cannot fail");
    }
    format!("sha256:{hex}")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn snapshot_paths_are_sorted_and_canonical() {
        let dir = tempfile::tempdir().unwrap();
        std::fs::create_dir(dir.path().join("nested")).unwrap();
        std::fs::write(dir.path().join("z"), b"z").unwrap();
        std::fs::write(dir.path().join("nested").join("a"), b"a").unwrap();

        let snapshot = collect_package_snapshot(dir.path()).unwrap();
        assert_eq!(
            snapshot
                .iter()
                .map(|entry| entry.path.as_str())
                .collect::<Vec<_>>(),
            ["nested/a", "z"]
        );
    }

    #[cfg(unix)]
    #[test]
    fn snapshot_rejects_entry_identity_substitution() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("file");
        std::fs::write(&path, b"original").unwrap();
        let replacement = dir.path().join("replacement");
        std::fs::write(&replacement, b"replacement").unwrap();

        let result = platform::collect_identity_counterfactual(dir.path(), |entry| {
            if entry == "file" {
                std::fs::rename(&replacement, &path).unwrap();
            }
        });

        assert!(result.is_err());
    }
}
