use std::fs::{self, File, OpenOptions};
use std::io::{self, Write as _};
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::SystemTime;

use rand::RngExt as _;

static TEMP_COUNTER: AtomicU64 = AtomicU64::new(0);
const RETAINED_SUPERSEDED_GENERATIONS: usize = 2;

pub fn write_atomic(path: &Path, content: &[u8], mode: u32) -> io::Result<()> {
    write_atomic_with_hook(path, content, mode, |_| Ok(()))
}

#[derive(Debug)]
pub struct StagedDir {
    path: PathBuf,
    active: bool,
}

impl StagedDir {
    pub fn new(target: &Path) -> io::Result<Self> {
        let parent = target.parent().ok_or_else(|| {
            io::Error::new(
                io::ErrorKind::InvalidInput,
                "target has no parent directory",
            )
        })?;
        fs::create_dir_all(parent)?;

        for _ in 0..100 {
            let path = temp_path_for(target)?;
            match fs::create_dir(&path) {
                Ok(()) => return Ok(Self { path, active: true }),
                Err(error) if error.kind() == io::ErrorKind::AlreadyExists => {}
                Err(error) => return Err(error),
            }
        }

        Err(io::Error::new(
            io::ErrorKind::AlreadyExists,
            "could not allocate a unique staging directory",
        ))
    }

    pub fn path(&self) -> &Path {
        &self.path
    }

    #[cfg(test)]
    pub fn publish(mut self, target: &Path) -> io::Result<PathBuf> {
        let pinned = self.publish_pinned_with_hooks(target, || Ok(()), || Ok(()))?;
        Ok(pinned.path().to_path_buf())
    }

    pub(crate) fn publish_pinned(mut self, target: &Path) -> io::Result<PinnedDir> {
        self.publish_pinned_with_hooks(target, || Ok(()), || Ok(()))
    }

    #[cfg(test)]
    fn publish_with_hook<F>(
        &mut self,
        target: &Path,
        before_pointer_replace: F,
    ) -> io::Result<PathBuf>
    where
        F: FnOnce() -> io::Result<()>,
    {
        let pinned = self.publish_pinned_with_hooks(target, before_pointer_replace, || Ok(()))?;
        Ok(pinned.path().to_path_buf())
    }

    #[cfg(test)]
    fn publish_with_hooks<B, A>(
        &mut self,
        target: &Path,
        before_pointer_replace: B,
        after_pointer_replace: A,
    ) -> io::Result<PathBuf>
    where
        B: FnOnce() -> io::Result<()>,
        A: FnOnce() -> io::Result<()>,
    {
        let pinned =
            self.publish_pinned_with_hooks(target, before_pointer_replace, after_pointer_replace)?;
        Ok(pinned.path().to_path_buf())
    }

    fn publish_pinned_with_hooks<B, A>(
        &mut self,
        target: &Path,
        before_pointer_replace: B,
        after_pointer_replace: A,
    ) -> io::Result<PinnedDir>
    where
        B: FnOnce() -> io::Result<()>,
        A: FnOnce() -> io::Result<()>,
    {
        let _slot_lock = lock_slot(target, LockMode::Exclusive, false)?
            .expect("a blocking lock acquisition always returns a guard");
        let generation = generation_path_for(target)?;
        fs::rename(&self.path, &generation)?;
        self.active = false;
        let mut ownership = RenamedGeneration::new(generation.clone());
        if let Err(error) = sync_parent_dir(&generation) {
            return Err(ownership.reclaim_after(error));
        }

        if let Err(error) = before_pointer_replace() {
            return Err(ownership.reclaim_after(error));
        }
        let lease_path = match generation_lease_path_for(&generation) {
            Ok(path) => path,
            Err(error) => return Err(ownership.reclaim_after(error)),
        };
        let generation_lock = match lock_file(&lease_path, LockMode::Shared, false) {
            Ok(Some(lock)) => lock,
            Ok(None) => unreachable!("a blocking lock acquisition always returns a guard"),
            Err(error) => return Err(ownership.reclaim_after(error)),
        };

        let Some(generation_name) = generation.file_name().and_then(|name| name.to_str()) else {
            return Err(ownership.reclaim_after(io::Error::new(
                io::ErrorKind::InvalidInput,
                "invalid generation name",
            )));
        };
        let pointer = pointer_path_for(target)?;
        let publish_result = write_atomic_with_commit_hook(
            &pointer,
            format!("{generation_name}\n").as_bytes(),
            0o644,
            || {
                ownership.release();
                after_pointer_replace()
            },
        );
        if let Err(error) = publish_result {
            if ownership.owned {
                match resolve_published_dir_with(target, read_pointer) {
                    Ok(active) if active == generation => ownership.release(),
                    Ok(_) => {}
                    Err(_) => ownership.release(),
                }
            }
            if ownership.owned {
                return Err(ownership.reclaim_after(error));
            }
            return Err(error);
        }
        ownership.release();
        cleanup_generations_locked(target);
        Ok(PinnedDir {
            path: generation,
            lease: Some(GenerationLease {
                lock: Some(generation_lock),
            }),
        })
    }
}

impl Drop for StagedDir {
    fn drop(&mut self) {
        if self.active {
            let _ = remove_path(&self.path);
        }
    }
}

fn write_atomic_with_hook<F>(
    path: &Path,
    content: &[u8],
    mode: u32,
    before_rename: F,
) -> io::Result<()>
where
    F: FnOnce(&Path) -> io::Result<()>,
{
    write_atomic_with_hooks(path, content, mode, before_rename, || Ok(()))
}

fn write_atomic_with_commit_hook<F>(
    path: &Path,
    content: &[u8],
    mode: u32,
    after_replace: F,
) -> io::Result<()>
where
    F: FnOnce() -> io::Result<()>,
{
    write_atomic_with_hooks(path, content, mode, |_| Ok(()), after_replace)
}

fn write_atomic_with_hooks<B, A>(
    path: &Path,
    content: &[u8],
    mode: u32,
    before_rename: B,
    after_replace: A,
) -> io::Result<()>
where
    B: FnOnce(&Path) -> io::Result<()>,
    A: FnOnce() -> io::Result<()>,
{
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)?;
    }

    let temp_path = temp_path_for(path)?;
    let mut file = create_temp_file(&temp_path, mode)?;
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt as _;
        file.set_permissions(fs::Permissions::from_mode(mode))?;
    }
    file.write_all(content)?;
    file.sync_all()?;
    drop(file);

    before_rename(&temp_path)?;

    replace_file(&temp_path, path)?;
    after_replace()?;
    sync_parent_dir(path)?;
    Ok(())
}

#[derive(Debug)]
struct RenamedGeneration {
    path: PathBuf,
    owned: bool,
}

impl RenamedGeneration {
    fn new(path: PathBuf) -> Self {
        Self { path, owned: true }
    }

    fn release(&mut self) {
        self.owned = false;
    }

    fn reclaim_after(&mut self, publication_error: io::Error) -> io::Error {
        match remove_path(&self.path) {
            Ok(()) => {
                self.owned = false;
                if let Err(sync_error) = sync_parent_dir(&self.path) {
                    return io::Error::other(format!(
                        "{publication_error}; never-active generation was removed but its parent could not be synchronized: {sync_error}"
                    ));
                }
                publication_error
            }
            Err(reclaim_error) => io::Error::other(format!(
                "{publication_error}; could not reclaim never-active generation {}: {reclaim_error}",
                self.path.display()
            )),
        }
    }
}

impl Drop for RenamedGeneration {
    fn drop(&mut self) {
        if self.owned {
            let _ = remove_path(&self.path);
        }
    }
}

#[cfg(unix)]
#[allow(
    dead_code,
    reason = "binary install path uses atomic symlink replacement"
)]
pub fn replace_symlink_atomic(link: &Path, target: &Path) -> io::Result<()> {
    replace_symlink_atomic_with_hook(link, target, || Ok(()))
}

#[cfg(unix)]
#[allow(dead_code, reason = "test hook exercises rename-over semantics")]
fn replace_symlink_atomic_with_hook<F>(
    link: &Path,
    target: &Path,
    before_rename: F,
) -> io::Result<()>
where
    F: FnOnce() -> io::Result<()>,
{
    if let Some(parent) = link.parent() {
        fs::create_dir_all(parent)?;
    }

    let temp_link = temp_path_for(link)?;
    std::os::unix::fs::symlink(target, &temp_link)?;
    before_rename()?;
    fs::rename(&temp_link, link)?;
    sync_parent_dir(link)?;
    Ok(())
}

fn create_temp_file(path: &Path, mode: u32) -> io::Result<File> {
    let mut options = OpenOptions::new();
    options.write(true).create_new(true);
    #[cfg(unix)]
    {
        use std::os::unix::fs::OpenOptionsExt as _;
        options.mode(mode);
    }
    #[cfg(not(unix))]
    let _ = mode;
    options.open(path)
}

fn temp_path_for(path: &Path) -> io::Result<PathBuf> {
    let file_name = path
        .file_name()
        .and_then(|name| name.to_str())
        .ok_or_else(|| io::Error::new(io::ErrorKind::InvalidInput, "path has no file name"))?;
    let counter = TEMP_COUNTER.fetch_add(1, Ordering::Relaxed);
    Ok(path.with_file_name(format!(".{file_name}.tmp-{}-{counter}", std::process::id())))
}

fn pointer_path_for(target: &Path) -> io::Result<PathBuf> {
    let file_name = target
        .file_name()
        .and_then(|name| name.to_str())
        .ok_or_else(|| io::Error::new(io::ErrorKind::InvalidInput, "path has no file name"))?;
    Ok(target.with_file_name(format!(".{file_name}.current")))
}

fn slot_lock_path_for(target: &Path) -> io::Result<PathBuf> {
    let file_name = target
        .file_name()
        .and_then(|name| name.to_str())
        .ok_or_else(|| io::Error::new(io::ErrorKind::InvalidInput, "path has no file name"))?;
    Ok(target.with_file_name(format!(".{file_name}.slot.lock")))
}

fn generation_lease_path_for(generation: &Path) -> io::Result<PathBuf> {
    let file_name = generation
        .file_name()
        .and_then(|name| name.to_str())
        .ok_or_else(|| io::Error::new(io::ErrorKind::InvalidInput, "path has no file name"))?;
    Ok(generation.with_file_name(format!("{file_name}.lease")))
}

fn generation_path_for(target: &Path) -> io::Result<PathBuf> {
    let mut rng = rand::rng();
    generation_path_for_with(target, || rng.random::<u128>())
}

fn generation_path_for_with<F>(target: &Path, mut nonce: F) -> io::Result<PathBuf>
where
    F: FnMut() -> u128,
{
    let file_name = target
        .file_name()
        .and_then(|name| name.to_str())
        .ok_or_else(|| io::Error::new(io::ErrorKind::InvalidInput, "path has no file name"))?;
    for _ in 0..100 {
        let candidate = target.with_file_name(format!(".{file_name}.generation-{:032x}", nonce()));
        match fs::symlink_metadata(&candidate) {
            Err(error) if error.kind() == io::ErrorKind::NotFound => return Ok(candidate),
            Ok(_) => {}
            Err(error) => return Err(error),
        }
    }
    Err(io::Error::new(
        io::ErrorKind::AlreadyExists,
        "could not allocate a unique generation name",
    ))
}

/// Resolve a logical publication slot to its immutable active generation.
/// A missing pointer denotes the legacy direct-directory representation.
pub(crate) fn resolve_published_dir(target: &Path) -> io::Result<PathBuf> {
    resolve_published_dir_with(target, read_pointer)
}

/// An immutable published directory held under a cross-process generation
/// lease. Legacy direct directories are never generation-collected and carry
/// no generation lease.
#[derive(Debug)]
pub(crate) struct PinnedDir {
    path: PathBuf,
    lease: Option<GenerationLease>,
}

impl PinnedDir {
    pub(crate) fn legacy(path: PathBuf) -> Self {
        Self { path, lease: None }
    }

    pub(crate) fn path(&self) -> &Path {
        &self.path
    }

    pub(crate) fn canonicalize(mut self) -> io::Result<Self> {
        self.path = self.path.canonicalize()?;
        Ok(self)
    }

    pub(crate) fn is_generation(&self) -> bool {
        self.lease.is_some()
    }
}

#[derive(Debug)]
struct GenerationLease {
    lock: Option<FileLock>,
}

impl Drop for GenerationLease {
    fn drop(&mut self) {
        if let Some(mut lock) = self.lock.take() {
            lock.unlock();
        }
    }
}

/// Pin the active immutable generation while holding the slot lock across the
/// pointer read and lease acquisition.
pub(crate) fn pin_published_dir(target: &Path) -> io::Result<PinnedDir> {
    let pointer = pointer_path_for(target)?;
    match fs::symlink_metadata(&pointer) {
        Err(error) if error.kind() == io::ErrorKind::NotFound => {
            validate_generation_directory(target)?;
            return Ok(PinnedDir::legacy(target.to_path_buf()));
        }
        Err(error) => return Err(error),
        Ok(metadata) if metadata.file_type().is_symlink() || !metadata.is_file() => {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!(
                    "published package pointer is not a regular file: {}",
                    pointer.display()
                ),
            ));
        }
        Ok(_) => {}
    }

    let _slot_lock = lock_slot_existing(target, LockMode::Shared, false)?
        .expect("a blocking lock acquisition always returns a guard");
    let path = resolve_published_dir(target)?;
    validate_generation_directory(&path)?;
    let lease_path = generation_lease_path_for(&path)?;
    let lock = lock_file_existing(&lease_path, LockMode::Shared, false)?
        .expect("a blocking lock acquisition always returns a guard");
    validate_generation_directory(&path)?;
    Ok(PinnedDir {
        path,
        lease: Some(GenerationLease { lock: Some(lock) }),
    })
}

pub(crate) fn pin_published_dir_if_present(target: &Path) -> io::Result<Option<PinnedDir>> {
    let pointer = pointer_path_for(target)?;
    let target_state = fs::symlink_metadata(target);
    let pointer_state = fs::symlink_metadata(&pointer);
    match (target_state, pointer_state) {
        (Err(target_error), Err(pointer_error))
            if target_error.kind() == io::ErrorKind::NotFound
                && pointer_error.kind() == io::ErrorKind::NotFound =>
        {
            Ok(None)
        }
        (Err(error), _) | (_, Err(error)) if error.kind() != io::ErrorKind::NotFound => Err(error),
        _ => pin_published_dir(target).map(Some),
    }
}

fn validate_generation_directory(path: &Path) -> io::Result<()> {
    let metadata = fs::symlink_metadata(path)?;
    if metadata.file_type().is_symlink() || !metadata.is_dir() {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            format!(
                "published package generation is not a directory: {}",
                path.display()
            ),
        ));
    }
    Ok(())
}

#[cfg(not(windows))]
fn read_pointer(path: &Path) -> io::Result<String> {
    fs::read_to_string(path)
}

#[cfg(windows)]
fn read_pointer(path: &Path) -> io::Result<String> {
    use std::io::Read as _;

    let mut file = open_pointer_file(path)?;
    let mut pointer = String::new();
    file.read_to_string(&mut pointer)?;
    Ok(pointer)
}

#[cfg(windows)]
fn open_pointer_file(path: &Path) -> io::Result<File> {
    use std::os::windows::fs::OpenOptionsExt as _;

    let mut options = OpenOptions::new();
    options.read(true).share_mode(windows_pointer_share_mode());
    options.open(path)
}

#[cfg(windows)]
fn windows_pointer_share_mode() -> u32 {
    use windows_sys::Win32::Storage::FileSystem::{
        FILE_SHARE_DELETE, FILE_SHARE_READ, FILE_SHARE_WRITE,
    };

    FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE
}

fn resolve_published_dir_with<F>(target: &Path, read_pointer: F) -> io::Result<PathBuf>
where
    F: FnOnce(&Path) -> io::Result<String>,
{
    let pointer_path = pointer_path_for(target)?;
    let pointer = match read_pointer(&pointer_path) {
        Ok(pointer) => pointer,
        Err(error) if error.kind() == io::ErrorKind::NotFound => return Ok(target.to_path_buf()),
        Err(error) => return Err(error),
    };
    let Some(target_name) = target.file_name().and_then(|name| name.to_str()) else {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            "path has no file name",
        ));
    };
    let generation = pointer.trim();
    let expected_prefix = format!(".{target_name}.generation-");
    if is_generation_name(generation, &expected_prefix) {
        Ok(target.with_file_name(generation))
    } else {
        Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "invalid package generation pointer",
        ))
    }
}

fn is_generation_name(name: &str, prefix: &str) -> bool {
    name.strip_prefix(prefix).is_some_and(|nonce| {
        nonce.len() == 32 && nonce.as_bytes().iter().all(u8::is_ascii_hexdigit)
    })
}

#[derive(Debug, Clone, Copy)]
enum LockMode {
    Shared,
    Exclusive,
}

#[derive(Debug)]
struct FileLock {
    file: Option<File>,
}

impl FileLock {
    fn unlock(&mut self) {
        if let Some(file) = self.file.take() {
            os_unlock(&file);
        }
    }
}

impl Drop for FileLock {
    fn drop(&mut self) {
        self.unlock();
    }
}

fn lock_slot(target: &Path, mode: LockMode, nonblocking: bool) -> io::Result<Option<FileLock>> {
    lock_file(&slot_lock_path_for(target)?, mode, nonblocking)
}

fn lock_slot_existing(
    target: &Path,
    mode: LockMode,
    nonblocking: bool,
) -> io::Result<Option<FileLock>> {
    lock_file_existing(&slot_lock_path_for(target)?, mode, nonblocking)
}

fn lock_file(path: &Path, mode: LockMode, nonblocking: bool) -> io::Result<Option<FileLock>> {
    lock_file_with_create(path, mode, nonblocking, true)
}

fn lock_file_existing(
    path: &Path,
    mode: LockMode,
    nonblocking: bool,
) -> io::Result<Option<FileLock>> {
    lock_file_with_create(path, mode, nonblocking, false)
}

fn lock_file_with_create(
    path: &Path,
    mode: LockMode,
    nonblocking: bool,
    create: bool,
) -> io::Result<Option<FileLock>> {
    if let Some(parent) = path.parent() {
        if create {
            fs::create_dir_all(parent)?;
        }
    }
    if let Ok(metadata) = fs::symlink_metadata(path) {
        if metadata.file_type().is_symlink() || !metadata.is_file() {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!("lock path is not a regular file: {}", path.display()),
            ));
        }
    }
    let file = OpenOptions::new()
        .read(true)
        .write(true)
        .create(create)
        .truncate(false)
        .open(path)?;
    if !file.metadata()?.is_file() {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            format!("lock path is not a regular file: {}", path.display()),
        ));
    }
    if os_lock(&file, mode, nonblocking)? {
        Ok(Some(FileLock { file: Some(file) }))
    } else {
        Ok(None)
    }
}

#[cfg(unix)]
fn os_lock(file: &File, mode: LockMode, nonblocking: bool) -> io::Result<bool> {
    use std::os::fd::AsRawFd as _;

    let operation = match mode {
        LockMode::Shared => libc::LOCK_SH,
        LockMode::Exclusive => libc::LOCK_EX,
    } | if nonblocking { libc::LOCK_NB } else { 0 };
    loop {
        // SAFETY: the descriptor belongs to `file` and remains open throughout
        // the call.
        if unsafe { libc::flock(file.as_raw_fd(), operation) } == 0 {
            return Ok(true);
        }
        let error = io::Error::last_os_error();
        if error.kind() == io::ErrorKind::Interrupted {
            continue;
        }
        if nonblocking && error.kind() == io::ErrorKind::WouldBlock {
            return Ok(false);
        }
        return Err(error);
    }
}

#[cfg(unix)]
fn os_unlock(file: &File) {
    use std::os::fd::AsRawFd as _;

    // SAFETY: the descriptor belongs to `file` and remains open for this call.
    let _ = unsafe { libc::flock(file.as_raw_fd(), libc::LOCK_UN) };
}

#[cfg(windows)]
fn os_lock(file: &File, mode: LockMode, nonblocking: bool) -> io::Result<bool> {
    use std::mem::zeroed;
    use std::os::windows::io::AsRawHandle as _;
    use windows_sys::Win32::Storage::FileSystem::{
        LockFileEx, LOCKFILE_EXCLUSIVE_LOCK, LOCKFILE_FAIL_IMMEDIATELY,
    };
    use windows_sys::Win32::System::IO::OVERLAPPED;

    let mut flags = match mode {
        LockMode::Shared => 0,
        LockMode::Exclusive => LOCKFILE_EXCLUSIVE_LOCK,
    };
    if nonblocking {
        flags |= LOCKFILE_FAIL_IMMEDIATELY;
    }
    // SAFETY: zero is a valid OVERLAPPED value for a synchronous whole-file
    // lock, and the file handle remains open while the lock is held.
    let mut overlapped: OVERLAPPED = unsafe { zeroed() };
    let result = unsafe {
        LockFileEx(
            file.as_raw_handle(),
            flags,
            0,
            u32::MAX,
            u32::MAX,
            &raw mut overlapped,
        )
    };
    if result != 0 {
        return Ok(true);
    }
    let error = io::Error::last_os_error();
    if nonblocking && error.raw_os_error() == Some(ERROR_LOCK_VIOLATION.cast_signed()) {
        Ok(false)
    } else {
        Err(error)
    }
}

#[cfg(windows)]
fn os_unlock(file: &File) {
    use std::mem::zeroed;
    use std::os::windows::io::AsRawHandle as _;
    use windows_sys::Win32::Storage::FileSystem::UnlockFileEx;
    use windows_sys::Win32::System::IO::OVERLAPPED;

    // SAFETY: this unlocks the same whole-file byte range used by `os_lock`;
    // the file handle remains open throughout the call.
    let mut overlapped: OVERLAPPED = unsafe { zeroed() };
    let _ = unsafe {
        UnlockFileEx(
            file.as_raw_handle(),
            0,
            u32::MAX,
            u32::MAX,
            &raw mut overlapped,
        )
    };
}

#[derive(Debug)]
struct GenerationCandidate {
    path: PathBuf,
    modified: SystemTime,
    name: String,
}

/// Collect only generations beyond the bounded recent-history allowance.
/// Every uncertainty is handled by retaining the affected generation.
fn cleanup_generations_locked(target: &Path) {
    let Ok(active) = resolve_published_dir(target) else {
        return;
    };
    if active == target {
        return;
    }
    let Some(parent) = target.parent() else {
        return;
    };
    let Some(target_name) = target.file_name().and_then(|name| name.to_str()) else {
        return;
    };
    let prefix = format!(".{target_name}.generation-");
    let Ok(entries) = fs::read_dir(parent) else {
        return;
    };
    let mut candidates = Vec::new();
    for entry in entries {
        let Ok(entry) = entry else {
            return;
        };
        let Some(name) = entry.file_name().to_str().map(str::to_owned) else {
            continue;
        };
        if !is_generation_name(&name, &prefix) {
            continue;
        }
        let Ok(file_type) = entry.file_type() else {
            continue;
        };
        if !file_type.is_dir() {
            continue;
        }
        let path = entry.path();
        if path == active {
            continue;
        }
        let Ok(modified) = entry.metadata().and_then(|metadata| metadata.modified()) else {
            continue;
        };
        candidates.push(GenerationCandidate {
            path,
            modified,
            name,
        });
    }
    candidates.sort_by(|left, right| {
        right
            .modified
            .cmp(&left.modified)
            .then_with(|| right.name.cmp(&left.name))
    });

    for candidate in candidates.into_iter().skip(RETAINED_SUPERSEDED_GENERATIONS) {
        let Ok(lease_path) = generation_lease_path_for(&candidate.path) else {
            continue;
        };
        let Ok(Some(collector_lease)) = lock_file_existing(&lease_path, LockMode::Exclusive, true)
        else {
            continue;
        };
        let Ok(current) = resolve_published_dir(target) else {
            continue;
        };
        if current == candidate.path {
            continue;
        }
        let Ok(metadata) = fs::symlink_metadata(&candidate.path) else {
            continue;
        };
        if !metadata.is_dir() || metadata.file_type().is_symlink() {
            continue;
        }
        if fs::remove_dir_all(&candidate.path).is_ok() {
            drop(collector_lease);
            let _ = fs::remove_file(lease_path);
        }
    }
}

#[cfg(not(windows))]
fn replace_file(source: &Path, target: &Path) -> io::Result<()> {
    // POSIX rename atomically replaces an existing directory entry.
    fs::rename(source, target)
}

#[cfg(any(test, windows))]
const WINDOWS_REPLACE_MAX_ATTEMPTS: usize = 8;

#[cfg(all(test, windows))]
use windows_sys::Win32::Foundation::ERROR_ACCESS_DENIED;
#[cfg(windows)]
use windows_sys::Win32::Foundation::{
    ERROR_FILE_NOT_FOUND, ERROR_LOCK_VIOLATION, ERROR_SHARING_VIOLATION,
};

#[cfg(all(test, not(windows)))]
const ERROR_ACCESS_DENIED: u32 = 5;
#[cfg(all(test, not(windows)))]
const ERROR_LOCK_VIOLATION: u32 = 33;
#[cfg(all(test, not(windows)))]
const ERROR_SHARING_VIOLATION: u32 = 32;

#[cfg(any(test, windows))]
fn is_windows_replace_contention(error: &io::Error) -> bool {
    matches!(
        error.raw_os_error(),
        Some(code)
            if code == ERROR_SHARING_VIOLATION.cast_signed()
                || code == ERROR_LOCK_VIOLATION.cast_signed()
    )
}

#[cfg(any(test, windows))]
fn retry_windows_replace_with<F, W>(mut replace: F, mut wait: W) -> io::Result<()>
where
    F: FnMut() -> io::Result<()>,
    W: FnMut(usize),
{
    let mut original_error = None;
    for attempt in 0..WINDOWS_REPLACE_MAX_ATTEMPTS {
        match replace() {
            Ok(()) => return Ok(()),
            Err(error) if is_windows_replace_contention(&error) => {
                if original_error.is_none() {
                    original_error = Some(error);
                }
                if attempt + 1 == WINDOWS_REPLACE_MAX_ATTEMPTS {
                    return Err(original_error.expect("a contention error was retained"));
                }
                wait(attempt);
            }
            Err(error) => return Err(error),
        }
    }
    unreachable!("the finite replacement loop always returns")
}

#[cfg(windows)]
fn replace_file(source: &Path, target: &Path) -> io::Result<()> {
    use std::iter;
    use std::os::windows::ffi::OsStrExt as _;

    let source_wide = source
        .as_os_str()
        .encode_wide()
        .chain(iter::once(0))
        .collect::<Vec<_>>();
    let target_wide = target
        .as_os_str()
        .encode_wide()
        .chain(iter::once(0))
        .collect::<Vec<_>>();
    retry_windows_replace_with(
        || replace_file_once(&source_wide, &target_wide),
        |attempt| std::thread::sleep(std::time::Duration::from_millis(1_u64 << attempt)),
    )
}

#[cfg(windows)]
fn replace_file_once(source: &[u16], target: &[u16]) -> io::Result<()> {
    use windows_sys::Win32::Storage::FileSystem::{
        MoveFileExW, ReplaceFileW, MOVEFILE_REPLACE_EXISTING, MOVEFILE_WRITE_THROUGH,
        REPLACEFILE_WRITE_THROUGH,
    };

    // SAFETY: both strings are NUL-terminated and remain alive for the call;
    // no backup or metadata exclusion buffers are supplied.
    let result = unsafe {
        ReplaceFileW(
            target.as_ptr(),
            source.as_ptr(),
            std::ptr::null(),
            REPLACEFILE_WRITE_THROUGH,
            std::ptr::null(),
            std::ptr::null(),
        )
    };
    if result != 0 {
        return Ok(());
    }

    let error = io::Error::last_os_error();
    if error.raw_os_error() != Some(ERROR_FILE_NOT_FOUND.cast_signed()) {
        return Err(error);
    }

    // ReplaceFileW requires an existing target. MoveFileExW provides the same
    // write-through atomic rename when the target disappeared or never existed.
    // SAFETY: both strings are NUL-terminated and remain alive for the call.
    let result = unsafe {
        MoveFileExW(
            source.as_ptr(),
            target.as_ptr(),
            MOVEFILE_REPLACE_EXISTING | MOVEFILE_WRITE_THROUGH,
        )
    };
    if result != 0 {
        Ok(())
    } else {
        Err(io::Error::last_os_error())
    }
}

fn remove_path(path: &Path) -> io::Result<()> {
    let metadata = fs::symlink_metadata(path)?;
    if metadata.is_dir() && !metadata.file_type().is_symlink() {
        fs::remove_dir_all(path)
    } else {
        fs::remove_file(path)
    }
}

fn sync_parent_dir(path: &Path) -> io::Result<()> {
    #[cfg(unix)]
    {
        if let Some(parent) = path.parent() {
            File::open(parent)?.sync_all()?;
        }
    }
    #[cfg(not(unix))]
    {
        // On Windows, directories cannot be opened with `File::open` for
        // `sync_all` — that requires CreateFile with FILE_FLAG_BACKUP_SEMANTICS,
        // which is not exposed by std. `fs::rename` is already atomic on
        // Windows; parent-dir durability requires a Windows-specific path
        // that's out of scope here.
        let _ = path;
    }
    Ok(())
}

#[cfg(test)]
pub fn simulate_interrupted_atomic_write(path: &Path, content: &[u8], mode: u32) -> PathBuf {
    let mut temp_path = None;
    let result = write_atomic_with_hook(path, content, mode, |temp| {
        temp_path = Some(temp.to_path_buf());
        Err(io::Error::other("simulated crash before rename"))
    });
    assert!(result.is_err());
    temp_path.expect("temp path should be captured before interruption")
}

#[cfg(all(test, unix))]
pub fn replace_symlink_atomic_for_test<F>(
    link: &Path,
    target: &Path,
    before_rename: F,
) -> io::Result<()>
where
    F: FnOnce() -> io::Result<()>,
{
    replace_symlink_atomic_with_hook(link, target, before_rename)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn generation_dirs(target: &Path) -> Vec<PathBuf> {
        let parent = target.parent().unwrap();
        let prefix = format!(
            ".{}.generation-",
            target.file_name().unwrap().to_string_lossy()
        );
        fs::read_dir(parent)
            .unwrap()
            .map(Result::unwrap)
            .filter(|entry| {
                entry.file_name().to_string_lossy().starts_with(&prefix)
                    && entry.file_type().unwrap().is_dir()
            })
            .map(|entry| entry.path())
            .collect()
    }

    fn publish_state(target: &Path, state: &str) -> PathBuf {
        let staged = StagedDir::new(target).unwrap();
        fs::write(staged.path().join("state"), state).unwrap();
        staged.publish(target).unwrap()
    }

    #[test]
    fn interrupted_atomic_write_keeps_original_target() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("credentials.toml");
        fs::write(&path, "old-token").unwrap();

        let temp_path = simulate_interrupted_atomic_write(&path, b"new-token", 0o600);

        assert_eq!(fs::read_to_string(&path).unwrap(), "old-token");
        assert_eq!(fs::read_to_string(&temp_path).unwrap(), "new-token");
    }

    #[test]
    fn staged_directory_publication_never_exposes_partial_tree() {
        use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
        use std::sync::{mpsc, Arc};
        use std::time::Duration;

        let dir = tempfile::tempdir().unwrap();
        let target = dir.path().join("package");
        fs::create_dir(&target).unwrap();
        fs::write(target.join("state"), "old").unwrap();

        let stop = Arc::new(AtomicBool::new(false));
        let saw_partial = Arc::new(AtomicBool::new(false));
        let reads = Arc::new(AtomicUsize::new(0));
        let (started_tx, started_rx) = mpsc::sync_channel(0);
        let reader_stop = Arc::clone(&stop);
        let reader_partial = Arc::clone(&saw_partial);
        let reader_reads = Arc::clone(&reads);
        let reader_target = target.clone();
        let reader = std::thread::spawn(move || {
            let mut first_read = true;
            while !reader_stop.load(Ordering::Relaxed) {
                // Read through the same pinning authority used by package
                // consumers.  Resolving the pointer alone neither serializes
                // with pointer replacement nor leases the returned generation:
                // Windows may report the former race as a sharing violation,
                // and a collector may retire the generation before `state` is
                // opened.  The pin couples the slot lock and generation lease
                // for the entire tree read.
                let active = pin_published_dir(&reader_target).unwrap();
                if !matches!(
                    fs::read_to_string(active.path().join("state")).as_deref(),
                    Ok("old" | "new")
                ) {
                    reader_partial.store(true, Ordering::Relaxed);
                    break;
                }
                reader_reads.fetch_add(1, Ordering::Relaxed);
                if first_read {
                    started_tx.send(()).unwrap();
                    first_read = false;
                }
                std::thread::yield_now();
            }
        });

        started_rx
            .recv_timeout(Duration::from_secs(5))
            .expect("reader must observe the initial complete publication");
        for index in 0..100 {
            let staged = StagedDir::new(&target).unwrap();
            let state = if index % 2 == 0 { "new" } else { "old" };
            fs::write(staged.path().join("state"), state).unwrap();
            staged.publish(&target).unwrap();
        }
        stop.store(true, Ordering::Relaxed);
        reader.join().unwrap();

        assert!(!saw_partial.load(Ordering::Relaxed));
        assert!(reads.load(Ordering::Relaxed) > 0);
        let active = pin_published_dir(&target).unwrap();
        assert!(matches!(
            fs::read_to_string(active.path().join("state")).as_deref(),
            Ok("old" | "new")
        ));
    }

    #[test]
    fn interrupted_generation_publish_keeps_previous_generation_active() {
        let dir = tempfile::tempdir().unwrap();
        let target = dir.path().join("package");
        fs::create_dir(&target).unwrap();
        fs::write(target.join("state"), "old").unwrap();

        let mut staged = StagedDir::new(&target).unwrap();
        fs::write(staged.path().join("state"), "new").unwrap();
        let error = staged
            .publish_with_hook(&target, || Err(io::Error::other("simulated power loss")))
            .unwrap_err();

        assert_eq!(error.kind(), io::ErrorKind::Other);
        assert_eq!(
            fs::read_to_string(resolve_published_dir(&target).unwrap().join("state")).unwrap(),
            "old"
        );
        assert!(
            generation_dirs(&target).is_empty(),
            "the renamed never-active generation must be reclaimed immediately"
        );
    }

    #[test]
    fn post_commit_failure_retains_potentially_active_generation() {
        let dir = tempfile::tempdir().unwrap();
        let target = dir.path().join("package");
        let mut staged = StagedDir::new(&target).unwrap();
        fs::write(staged.path().join("state"), "committed").unwrap();

        let error = staged
            .publish_with_hooks(
                &target,
                || Ok(()),
                || Err(io::Error::other("simulated parent durability failure")),
            )
            .unwrap_err();

        assert_eq!(error.kind(), io::ErrorKind::Other);
        let active = resolve_published_dir(&target).unwrap();
        assert_eq!(
            fs::read_to_string(active.join("state")).unwrap(),
            "committed"
        );
        assert_eq!(generation_dirs(&target), [active]);
    }

    #[test]
    fn successful_swaps_have_bounded_generation_retention() {
        let dir = tempfile::tempdir().unwrap();
        let target = dir.path().join("package");

        for index in 0..12 {
            publish_state(&target, &index.to_string());
        }

        assert!(generation_dirs(&target).len() <= 3);
        assert_eq!(
            fs::read_to_string(resolve_published_dir(&target).unwrap().join("state")).unwrap(),
            "11"
        );
    }

    #[test]
    fn pinned_old_generation_is_collected_by_later_publication() {
        let dir = tempfile::tempdir().unwrap();
        let target = dir.path().join("package");
        let first = publish_state(&target, "first");
        let pinned = pin_published_dir(&target).unwrap();
        assert_eq!(pinned.path(), first);

        for index in 0..6 {
            std::thread::sleep(std::time::Duration::from_millis(2));
            publish_state(&target, &format!("new-{index}"));
        }

        assert_eq!(
            fs::read_to_string(pinned.path().join("state")).unwrap(),
            "first"
        );
        assert!(first.is_dir());
        assert!(generation_dirs(&target).len() > 3);

        drop(pinned);

        assert!(
            first.exists(),
            "releasing a read lease must not trigger collection"
        );
        std::thread::sleep(std::time::Duration::from_millis(2));
        publish_state(&target, "cleanup-trigger");

        assert!(!first.exists());
        assert!(generation_dirs(&target).len() <= 3);
    }

    #[test]
    fn a_recent_non_current_generation_is_not_deleted_merely_for_being_old() {
        let dir = tempfile::tempdir().unwrap();
        let target = dir.path().join("package");
        let first = publish_state(&target, "first");
        publish_state(&target, "second");

        assert!(first.is_dir());
        assert_eq!(generation_dirs(&target).len(), 2);
    }

    #[test]
    fn os_reader_lock_blocks_nonblocking_collector_lock() {
        let dir = tempfile::tempdir().unwrap();
        let lock_path = dir.path().join("generation.lease");
        let reader = lock_file(&lock_path, LockMode::Shared, false)
            .unwrap()
            .unwrap();

        assert!(
            lock_file(&lock_path, LockMode::Exclusive, true)
                .unwrap()
                .is_none(),
            "an active OS reader lease must exclude collection"
        );
        drop(reader);
        assert!(
            lock_file(&lock_path, LockMode::Exclusive, true)
                .unwrap()
                .is_some(),
            "releasing the reader lease must permit collection"
        );
    }

    #[test]
    fn uncertain_lease_metadata_retains_collection_candidate() {
        let dir = tempfile::tempdir().unwrap();
        let target = dir.path().join("package");
        let retained = publish_state(&target, "retained");
        let lease_path = generation_lease_path_for(&retained).unwrap();
        fs::remove_file(&lease_path).unwrap();
        fs::create_dir(&lease_path).unwrap();

        for index in 0..5 {
            std::thread::sleep(std::time::Duration::from_millis(2));
            publish_state(&target, &format!("new-{index}"));
        }

        assert!(
            retained.is_dir(),
            "invalid lock metadata must fail closed by retaining the generation"
        );
        assert!(generation_dirs(&target).len() > 3);
    }

    #[test]
    fn invalid_utf8_pointer_never_revives_stale_legacy_directory() {
        let dir = tempfile::tempdir().unwrap();
        let target = dir.path().join("package");
        fs::create_dir(&target).unwrap();
        fs::write(target.join("state"), "stale legacy").unwrap();
        fs::write(pointer_path_for(&target).unwrap(), [0xff, 0xfe]).unwrap();

        let error = resolve_published_dir(&target).unwrap_err();
        assert_eq!(error.kind(), io::ErrorKind::InvalidData);
    }

    #[test]
    fn unreadable_pointer_never_revives_stale_legacy_directory() {
        let dir = tempfile::tempdir().unwrap();
        let target = dir.path().join("package");
        fs::create_dir(&target).unwrap();
        fs::write(target.join("state"), "stale legacy").unwrap();

        let error = resolve_published_dir_with(&target, |_| {
            Err(io::Error::new(
                io::ErrorKind::PermissionDenied,
                "simulated unreadable pointer",
            ))
        })
        .unwrap_err();
        assert_eq!(error.kind(), io::ErrorKind::PermissionDenied);
    }

    #[test]
    fn windows_replace_retry_accepts_only_sharing_contention() {
        assert!(is_windows_replace_contention(
            &io::Error::from_raw_os_error(ERROR_SHARING_VIOLATION.cast_signed())
        ));
        assert!(is_windows_replace_contention(
            &io::Error::from_raw_os_error(ERROR_LOCK_VIOLATION.cast_signed())
        ));
        assert!(!is_windows_replace_contention(
            &io::Error::from_raw_os_error(ERROR_ACCESS_DENIED.cast_signed())
        ));
        assert!(!is_windows_replace_contention(&io::Error::new(
            io::ErrorKind::InvalidData,
            "invalid pointer"
        )));
    }

    #[test]
    fn windows_replace_retry_stops_after_success() {
        let mut attempts = 0;
        let mut waits = Vec::new();

        retry_windows_replace_with(
            || {
                attempts += 1;
                if attempts < 3 {
                    Err(io::Error::from_raw_os_error(
                        ERROR_SHARING_VIOLATION.cast_signed(),
                    ))
                } else {
                    Ok(())
                }
            },
            |attempt| waits.push(attempt),
        )
        .unwrap();

        assert_eq!(attempts, 3);
        assert_eq!(waits, [0, 1]);
    }

    #[test]
    fn windows_replace_retry_is_bounded_and_returns_original_error() {
        let mut attempts = 0;
        let error = retry_windows_replace_with(
            || {
                attempts += 1;
                let code = if attempts == 1 {
                    ERROR_SHARING_VIOLATION
                } else {
                    ERROR_LOCK_VIOLATION
                };
                Err(io::Error::from_raw_os_error(code.cast_signed()))
            },
            |_| {},
        )
        .unwrap_err();

        assert_eq!(attempts, WINDOWS_REPLACE_MAX_ATTEMPTS);
        assert_eq!(
            error.raw_os_error(),
            Some(ERROR_SHARING_VIOLATION.cast_signed())
        );
    }

    #[test]
    fn windows_replace_retry_does_not_retry_access_denied() {
        let mut attempts = 0;
        let error = retry_windows_replace_with(
            || {
                attempts += 1;
                Err(io::Error::from_raw_os_error(
                    ERROR_ACCESS_DENIED.cast_signed(),
                ))
            },
            |_| panic!("non-contention errors must not wait"),
        )
        .unwrap_err();

        assert_eq!(attempts, 1);
        assert_eq!(
            error.raw_os_error(),
            Some(ERROR_ACCESS_DENIED.cast_signed())
        );
    }

    #[cfg(windows)]
    #[test]
    fn windows_pointer_reader_allows_delete_compatible_replacement() {
        use windows_sys::Win32::Storage::FileSystem::{
            FILE_SHARE_DELETE, FILE_SHARE_READ, FILE_SHARE_WRITE,
        };

        assert_eq!(
            windows_pointer_share_mode(),
            FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE
        );

        let dir = tempfile::tempdir().unwrap();
        let pointer = dir.path().join(".package.current");
        fs::write(&pointer, "old").unwrap();
        let held_reader = open_pointer_file(&pointer).unwrap();

        write_atomic(&pointer, b"new", 0o644).unwrap();

        assert_eq!(read_pointer(&pointer).unwrap(), "new");
        drop(held_reader);
    }

    #[test]
    fn generation_name_retries_a_retained_collision() {
        let dir = tempfile::tempdir().unwrap();
        let target = dir.path().join("package");
        let collision = target.with_file_name(format!(".package.generation-{:032x}", 7_u128));
        fs::create_dir(&collision).unwrap();
        let mut nonces = [7_u128, 8_u128].into_iter();

        let generation = generation_path_for_with(&target, || nonces.next().unwrap()).unwrap();

        assert_ne!(generation, collision);
        assert_eq!(
            generation.file_name().unwrap().to_string_lossy(),
            format!(".package.generation-{:032x}", 8_u128)
        );
    }

    #[cfg(unix)]
    #[test]
    fn symlink_replace_renames_over_existing_link() {
        use std::sync::atomic::{AtomicBool, Ordering};
        use std::sync::Arc;
        use std::time::Duration;

        let dir = tempfile::tempdir().unwrap();
        let old_target = dir.path().join("old");
        let new_target = dir.path().join("new");
        fs::create_dir_all(&old_target).unwrap();
        fs::create_dir_all(&new_target).unwrap();

        let link = dir.path().join("pkg");
        std::os::unix::fs::symlink(&old_target, &link).unwrap();

        let stop = Arc::new(AtomicBool::new(false));
        let saw_missing = Arc::new(AtomicBool::new(false));
        let reader_stop = Arc::clone(&stop);
        let reader_missing = Arc::clone(&saw_missing);
        let reader_link = link.clone();
        let reader = std::thread::spawn(move || {
            while !reader_stop.load(Ordering::Relaxed) {
                if fs::symlink_metadata(&reader_link).is_err() {
                    reader_missing.store(true, Ordering::Relaxed);
                    break;
                }
                std::thread::yield_now();
            }
        });

        replace_symlink_atomic_for_test(&link, &new_target, || {
            std::thread::sleep(Duration::from_millis(20));
            Ok(())
        })
        .unwrap();
        stop.store(true, Ordering::Relaxed);
        reader.join().unwrap();

        assert!(!saw_missing.load(Ordering::Relaxed));
        assert_eq!(fs::read_link(&link).unwrap(), new_target);
    }
}
