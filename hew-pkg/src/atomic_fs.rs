use std::fs::{self, File, OpenOptions};
use std::io::{self, Write as _};
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicU64, Ordering};

static TEMP_COUNTER: AtomicU64 = AtomicU64::new(0);

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

    pub fn publish(mut self, target: &Path) -> io::Result<()> {
        match fs::symlink_metadata(target) {
            Ok(_) => {
                exchange_paths(&self.path, target)?;
                remove_path(&self.path)?;
                self.active = false;
            }
            Err(error) if error.kind() == io::ErrorKind::NotFound => {
                fs::rename(&self.path, target)?;
                self.active = false;
            }
            Err(error) => return Err(error),
        }

        sync_parent_dir(target)
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

    fs::rename(&temp_path, path)?;
    sync_parent_dir(path)?;
    Ok(())
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

#[cfg(any(target_os = "linux", target_os = "android"))]
fn exchange_paths(left: &Path, right: &Path) -> io::Result<()> {
    use std::ffi::CString;
    use std::os::unix::ffi::OsStrExt as _;

    let left = CString::new(left.as_os_str().as_bytes())
        .map_err(|_| io::Error::new(io::ErrorKind::InvalidInput, "path contains a NUL byte"))?;
    let right = CString::new(right.as_os_str().as_bytes())
        .map_err(|_| io::Error::new(io::ErrorKind::InvalidInput, "path contains a NUL byte"))?;

    // SAFETY: both pointers remain valid for the duration of the syscall and
    // identify existing paths on the same filesystem.
    let result = unsafe {
        libc::syscall(
            libc::SYS_renameat2,
            libc::AT_FDCWD,
            left.as_ptr(),
            libc::AT_FDCWD,
            right.as_ptr(),
            libc::RENAME_EXCHANGE,
        )
    };
    if result == 0 {
        Ok(())
    } else {
        Err(io::Error::last_os_error())
    }
}

#[cfg(any(target_os = "macos", target_os = "ios"))]
fn exchange_paths(left: &Path, right: &Path) -> io::Result<()> {
    use std::ffi::CString;
    use std::os::unix::ffi::OsStrExt as _;

    let left = CString::new(left.as_os_str().as_bytes())
        .map_err(|_| io::Error::new(io::ErrorKind::InvalidInput, "path contains a NUL byte"))?;
    let right = CString::new(right.as_os_str().as_bytes())
        .map_err(|_| io::Error::new(io::ErrorKind::InvalidInput, "path contains a NUL byte"))?;

    // SAFETY: both pointers remain valid for the duration of the call and
    // identify existing paths on the same filesystem.
    let result = unsafe { libc::renamex_np(left.as_ptr(), right.as_ptr(), libc::RENAME_SWAP) };
    if result == 0 {
        Ok(())
    } else {
        Err(io::Error::last_os_error())
    }
}

#[cfg(not(any(
    target_os = "linux",
    target_os = "android",
    target_os = "macos",
    target_os = "ios"
)))]
fn exchange_paths(left: &Path, right: &Path) -> io::Result<()> {
    let backup = temp_path_for(right)?;
    fs::rename(right, &backup)?;
    if let Err(error) = fs::rename(left, right) {
        let restore = fs::rename(&backup, right);
        return match restore {
            Ok(()) => Err(error),
            Err(restore_error) => Err(io::Error::other(format!(
                "publication failed: {error}; restoring previous content failed: {restore_error}"
            ))),
        };
    }
    fs::rename(backup, left)
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
        use std::sync::atomic::{AtomicBool, Ordering};
        use std::sync::Arc;

        let dir = tempfile::tempdir().unwrap();
        let target = dir.path().join("package");
        fs::create_dir(&target).unwrap();
        fs::write(target.join("state"), "old").unwrap();

        let stop = Arc::new(AtomicBool::new(false));
        let saw_partial = Arc::new(AtomicBool::new(false));
        let reader_stop = Arc::clone(&stop);
        let reader_partial = Arc::clone(&saw_partial);
        let reader_target = target.clone();
        let reader = std::thread::spawn(move || {
            while !reader_stop.load(Ordering::Relaxed) {
                if !matches!(
                    fs::read_to_string(reader_target.join("state")).as_deref(),
                    Ok("old" | "new")
                ) {
                    reader_partial.store(true, Ordering::Relaxed);
                    break;
                }
                std::thread::yield_now();
            }
        });

        for index in 0..100 {
            let staged = StagedDir::new(&target).unwrap();
            let state = if index % 2 == 0 { "new" } else { "old" };
            fs::write(staged.path().join("state"), state).unwrap();
            staged.publish(&target).unwrap();
        }
        stop.store(true, Ordering::Relaxed);
        reader.join().unwrap();

        assert!(!saw_partial.load(Ordering::Relaxed));
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
