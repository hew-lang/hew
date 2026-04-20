use std::fs::{self, File, OpenOptions};
use std::io::{self, Write as _};
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicU64, Ordering};

static TEMP_COUNTER: AtomicU64 = AtomicU64::new(0);

pub fn write_atomic(path: &Path, content: &[u8], mode: u32) -> io::Result<()> {
    write_atomic_with_hook(path, content, mode, |_| Ok(()))
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

fn sync_parent_dir(path: &Path) -> io::Result<()> {
    if let Some(parent) = path.parent() {
        File::open(parent)?.sync_all()?;
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
