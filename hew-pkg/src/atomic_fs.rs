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
        self.publish_with_hook(target, || Ok(()))
    }

    fn publish_with_hook<F>(&mut self, target: &Path, before_pointer_replace: F) -> io::Result<()>
    where
        F: FnOnce() -> io::Result<()>,
    {
        let generation = generation_path_for(target)?;
        fs::rename(&self.path, &generation)?;
        self.active = false;
        sync_parent_dir(&generation)?;

        before_pointer_replace()?;

        let generation_name = generation
            .file_name()
            .and_then(|name| name.to_str())
            .ok_or_else(|| {
                io::Error::new(io::ErrorKind::InvalidInput, "invalid generation name")
            })?;
        write_atomic(
            &pointer_path_for(target)?,
            format!("{generation_name}\n").as_bytes(),
            0o644,
        )
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

    replace_file(&temp_path, path)?;
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

fn pointer_path_for(target: &Path) -> io::Result<PathBuf> {
    let file_name = target
        .file_name()
        .and_then(|name| name.to_str())
        .ok_or_else(|| io::Error::new(io::ErrorKind::InvalidInput, "path has no file name"))?;
    Ok(target.with_file_name(format!(".{file_name}.current")))
}

fn generation_path_for(target: &Path) -> io::Result<PathBuf> {
    let file_name = target
        .file_name()
        .and_then(|name| name.to_str())
        .ok_or_else(|| io::Error::new(io::ErrorKind::InvalidInput, "path has no file name"))?;
    let counter = TEMP_COUNTER.fetch_add(1, Ordering::Relaxed);
    Ok(target.with_file_name(format!(
        ".{file_name}.generation-{}-{counter}",
        std::process::id()
    )))
}

/// Resolve a logical publication slot to its immutable active generation.
/// A missing pointer denotes the legacy direct-directory representation.
pub(crate) fn resolve_published_dir(target: &Path) -> PathBuf {
    let Ok(pointer_path) = pointer_path_for(target) else {
        return target.to_path_buf();
    };
    let Ok(pointer) = fs::read_to_string(pointer_path) else {
        return target.to_path_buf();
    };
    let Some(target_name) = target.file_name().and_then(|name| name.to_str()) else {
        return target.to_path_buf();
    };
    let generation = pointer.trim();
    let expected_prefix = format!(".{target_name}.generation-");
    if generation.starts_with(&expected_prefix)
        && !generation.contains('/')
        && !generation.contains('\\')
    {
        target.with_file_name(generation)
    } else {
        // Fail closed on a corrupt or attacker-controlled pointer rather than
        // falling back to a potentially stale legacy directory.
        target.with_file_name(format!(".{target_name}.invalid-generation-pointer"))
    }
}

#[cfg(not(windows))]
fn replace_file(source: &Path, target: &Path) -> io::Result<()> {
    // POSIX rename atomically replaces an existing directory entry.
    fs::rename(source, target)
}

#[cfg(windows)]
fn replace_file(source: &Path, target: &Path) -> io::Result<()> {
    use std::iter;
    use std::os::windows::ffi::OsStrExt as _;
    use windows_sys::Win32::Storage::FileSystem::{ReplaceFileW, REPLACEFILE_WRITE_THROUGH};

    if !target.exists() {
        return fs::rename(source, target);
    }

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
    // SAFETY: both strings are NUL-terminated and remain alive for the call;
    // no backup or metadata exclusion buffers are supplied.
    let result = unsafe {
        ReplaceFileW(
            target_wide.as_ptr(),
            source_wide.as_ptr(),
            std::ptr::null(),
            REPLACEFILE_WRITE_THROUGH,
            std::ptr::null(),
            std::ptr::null(),
        )
    };
    if result == 0 {
        Err(io::Error::last_os_error())
    } else {
        Ok(())
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
                let active = resolve_published_dir(&reader_target);
                if !matches!(
                    fs::read_to_string(active.join("state")).as_deref(),
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
        assert!(matches!(
            fs::read_to_string(resolve_published_dir(&target).join("state")).as_deref(),
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
            fs::read_to_string(resolve_published_dir(&target).join("state")).unwrap(),
            "old"
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
