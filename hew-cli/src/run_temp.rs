//! Temporary artifacts owned by a run/debug process through an OS file lease.
//!
//! PID visibility and directory age cannot prove a run has stopped: sandboxes
//! may share a filesystem while using different PID namespaces, and legitimate
//! runs may last arbitrarily long. The lease survives for the artifact's whole
//! lifetime and the OS releases it even after a killed process.

use std::fs::{File, OpenOptions};
use std::path::{Path, PathBuf};

// Keep lease-owned artifacts outside the directory swept by older compilers
// using PID and age guesses. Those compilers may be running concurrently.
const HEW_RUN_DIR_NAME: &str = "hew-run-v2";
const ROOT_LOCK: &str = ".lock";
const LEASE: &str = ".lease";

pub struct RunTempDir {
    // Field order closes the lease before TempDir removes it, including on Windows.
    _lease: File,
    directory: tempfile::TempDir,
}

impl RunTempDir {
    pub fn path(&self) -> &Path {
        self.directory.path()
    }
}

/// Uses the same platform temporary root exposed to compiled Hew programs.
pub fn hew_run_root() -> PathBuf {
    std::env::temp_dir().join(HEW_RUN_DIR_NAME)
}

fn open_lock(path: &Path) -> std::io::Result<File> {
    OpenOptions::new()
        .read(true)
        .write(true)
        .create(true)
        .truncate(false)
        .open(path)
}

pub fn create_hew_run_temp_dir() -> std::io::Result<RunTempDir> {
    create_in(&hew_run_root())
}

fn create_in(root: &Path) -> std::io::Result<RunTempDir> {
    std::fs::create_dir_all(root)?;
    let root_lock = open_lock(&root.join(ROOT_LOCK))?;
    root_lock.lock()?;
    // Creation and lease acquisition are atomic with respect to the sweeper.
    let directory = tempfile::Builder::new().prefix("run-").tempdir_in(root)?;
    let lease = open_lock(&directory.path().join(LEASE))?;
    lease.lock()?;
    Ok(RunTempDir {
        _lease: lease,
        directory,
    })
}

/// Cleanup is best effort and never blocks behind another startup sweep.
pub fn sweep_on_startup() {
    sweep_stale_run_dirs(&hew_run_root());
}

fn sweep_stale_run_dirs(root: &Path) {
    let Ok(root_lock) = open_lock(&root.join(ROOT_LOCK)) else {
        return;
    };
    if root_lock.try_lock().is_err() {
        return;
    }
    let Ok(entries) = std::fs::read_dir(root) else {
        return;
    };
    for entry in entries.flatten() {
        if !entry.file_type().is_ok_and(|kind| kind.is_dir()) {
            continue;
        }
        // Unknown directories have no ownership proof and must be preserved.
        let Ok(lease) = OpenOptions::new()
            .read(true)
            .write(true)
            .open(entry.path().join(LEASE))
        else {
            continue;
        };
        if lease.try_lock().is_err() {
            continue;
        }
        // No creator can race this removal while root_lock is held. Close the
        // lease first so Windows also permits removing its containing directory.
        drop(lease);
        let _ = std::fs::remove_dir_all(entry.path());
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::time::{Duration, SystemTime};

    #[test]
    fn live_lease_survives_age_and_invisible_pid() {
        let root = tempfile::tempdir().unwrap();
        let directory = root.path().join("999999999-old");
        std::fs::create_dir(&directory).unwrap();
        let lease = open_lock(&directory.join(LEASE)).unwrap();
        lease.lock().unwrap();
        let old = SystemTime::now() - Duration::from_hours(24);
        filetime::set_file_mtime(&directory, filetime::FileTime::from_system_time(old)).unwrap();

        sweep_stale_run_dirs(root.path());
        assert!(directory.exists());
        drop(lease);
        sweep_stale_run_dirs(root.path());
        assert!(!directory.exists());
    }

    #[test]
    fn normal_drop_removes_artifact_and_unknown_entries_are_preserved() {
        let root = tempfile::tempdir().unwrap();
        let unknown = root.path().join("unknown");
        std::fs::create_dir(&unknown).unwrap();
        let artifact = create_in(root.path()).unwrap();
        let path = artifact.path().to_owned();
        std::fs::write(path.join("program"), b"artifact").unwrap();
        sweep_stale_run_dirs(root.path());
        assert!(path.join("program").exists());
        drop(artifact);
        assert!(!path.exists());
        assert!(unknown.exists());
    }

    #[test]
    fn sweep_on_missing_root_is_a_silent_noop() {
        let parent = tempfile::tempdir().unwrap();
        let missing = parent.path().join("missing");
        sweep_stale_run_dirs(&missing);
        assert!(!missing.exists());
    }

    #[test]
    fn process_exit_releases_lease_without_running_directory_cleanup() {
        let root = tempfile::tempdir().unwrap();
        let status = std::process::Command::new(std::env::current_exe().unwrap())
            .args(["--exact", "run_temp::tests::abandoned_artifact_process"])
            .env("HEW_TEST_ABANDONED_RUN_ROOT", root.path())
            .status()
            .unwrap();
        assert!(status.success());
        let path =
            PathBuf::from(std::fs::read_to_string(root.path().join("artifact-path")).unwrap());
        assert!(path.join("program").exists());
        sweep_stale_run_dirs(root.path());
        assert!(!path.exists());
    }

    #[test]
    fn abandoned_artifact_process() {
        let Some(root) = std::env::var_os("HEW_TEST_ABANDONED_RUN_ROOT") else {
            return;
        };
        let root = PathBuf::from(root);
        let artifact = create_in(&root).unwrap();
        std::fs::write(artifact.path().join("program"), b"artifact").unwrap();
        std::fs::write(
            root.join("artifact-path"),
            artifact.path().to_str().unwrap(),
        )
        .unwrap();
        // Models termination without Rust destructors; the OS still closes locks.
        std::process::exit(0);
    }
}
