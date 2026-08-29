//! Owned, sweepable temp storage for `hew run` / `hew debug` compiled
//! artifacts (#3132).
//!
//! Before this module, `hew run` and `hew debug` compiled into a bare
//! `tempfile::tempdir()` and relied on its RAII `Drop` as the only cleanup
//! authority. Every normal exit path drops the artifact first, but nothing
//! survives `SIGKILL` or an uncaught fatal signal — and test harnesses kill
//! `hew run` routinely through timeouts and watchdogs. The leaked
//! directories landed at unnamed `/tmp/.tmpXXXXXX` paths that nothing could
//! recognise or sweep.
//!
//! Artifacts now live under a recognisable, owned home:
//! `$TMPDIR/hew-run/<pid>-<rand>/`. `hew run` and `hew debug` startup
//! opportunistically sweeps that home (bounded work, no daemon): an entry
//! whose pid no longer exists is removed unconditionally, and any entry
//! older than [`STALE_AGE`] is removed regardless of pid state, so pid reuse
//! or a platform without a liveness check still bounds disk growth. A live
//! pid is always skipped. RAII drop stays the fast path for a normal exit;
//! the sweep is the second cleanup authority that survives a kill.

use std::path::{Path, PathBuf};
use std::time::{Duration, SystemTime};

/// Directory name under the system temp root that owns every `hew run` /
/// `hew debug` compiled artifact.
const HEW_RUN_DIR_NAME: &str = "hew-run";

/// An entry older than this is swept on startup even if its pid still looks
/// alive — it defends against pid reuse, and on a platform where liveness
/// can't be checked (see `pid_is_alive` below) it is the *only* signal that
/// ever removes anything.
const STALE_AGE: Duration = Duration::from_hours(6);

/// Root directory owning every `hew run` / `hew debug` artifact:
/// `$TMPDIR/hew-run`. Uses `std::env::temp_dir()`, which honours
/// `TMPDIR`/`TMP`/`TEMP` — the same convention `hew_temp_dir()`
/// (`hew-runtime/src/env.rs`) exposes to compiled Hew programs.
pub fn hew_run_root() -> PathBuf {
    std::env::temp_dir().join(HEW_RUN_DIR_NAME)
}

/// Create a fresh, recognisably-named temp dir under [`hew_run_root`] for
/// this process's compiled artifact: `<pid>-<rand>`.
pub fn create_hew_run_temp_dir() -> std::io::Result<tempfile::TempDir> {
    let root = hew_run_root();
    std::fs::create_dir_all(&root)?;
    tempfile::Builder::new()
        .prefix(&format!("{}-", std::process::id()))
        .tempdir_in(&root)
}

/// Best-effort startup sweep for `hew run` / `hew debug`. Never panics and
/// never surfaces an error to the caller: a sweep failure (unreadable
/// entry, permission denied, root missing) must not block the compile-and-run
/// the user is waiting on.
pub fn sweep_on_startup() {
    sweep_stale_run_dirs(&hew_run_root());
}

/// Remove stale entries directly under `root`. Every per-entry I/O error is
/// swallowed; a single unreadable or unremovable entry does not stop the
/// sweep from considering the rest.
fn sweep_stale_run_dirs(root: &Path) {
    let Ok(entries) = std::fs::read_dir(root) else {
        return; // no hew-run dir yet, or unreadable - nothing to sweep
    };
    let now = SystemTime::now();
    for entry in entries.flatten() {
        if !entry.file_type().is_ok_and(|ft| ft.is_dir()) {
            continue;
        }
        let name = entry.file_name();
        let dead_pid = name
            .to_str()
            .and_then(leading_pid)
            .is_some_and(|pid| !pid_is_alive(pid));
        let stale = entry_age(&entry, now).is_some_and(|age| age > STALE_AGE);
        if dead_pid || stale {
            let _ = std::fs::remove_dir_all(entry.path());
        }
    }
}

/// Parse the leading `<pid>` off a `<pid>-<rand>` directory name. Any other
/// shape (no hyphen, non-numeric prefix) yields `None`, and such an entry is
/// then only ever swept by [`STALE_AGE`] — an unrecognised name is not
/// evidence of a dead process.
fn leading_pid(dir_name: &str) -> Option<u32> {
    dir_name.split('-').next()?.parse().ok()
}

fn entry_age(entry: &std::fs::DirEntry, now: SystemTime) -> Option<Duration> {
    let modified = entry.metadata().ok()?.modified().ok()?;
    now.duration_since(modified).ok()
}

/// Cross-platform pid-liveness check.
#[cfg(unix)]
fn pid_is_alive(pid: u32) -> bool {
    let Ok(pid) = libc::pid_t::try_from(pid) else {
        return false; // does not fit a real pid_t - cannot be a live process
    };
    // SAFETY: signal 0 sends nothing; it only asks the kernel whether `pid`
    // exists and is visible to us.
    let result = unsafe { libc::kill(pid, 0) };
    if result == 0 {
        return true;
    }
    // EPERM: the process exists but is owned by another user - still alive.
    // ESRCH (no such process), or anything else: treat as dead; STALE_AGE is
    // the backstop for any state `kill(2)` cannot resolve.
    std::io::Error::last_os_error().raw_os_error() == Some(libc::EPERM)
}

#[cfg(windows)]
fn pid_is_alive(pid: u32) -> bool {
    use windows_sys::Win32::Foundation::{
        CloseHandle, GetLastError, ERROR_INVALID_PARAMETER, HANDLE, STILL_ACTIVE,
    };
    use windows_sys::Win32::System::Threading::{
        GetExitCodeProcess, OpenProcess, PROCESS_QUERY_LIMITED_INFORMATION,
    };

    // SAFETY: `OpenProcess` is called with a plain integer pid and no
    // pointers; the returned handle (if any) is closed below before this
    // function returns on every path.
    let handle: HANDLE = unsafe { OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION, 0, pid) };
    if handle.is_null() {
        // ERROR_INVALID_PARAMETER: no process with this pid exists - dead.
        // Anything else (e.g. access denied): the process exists but we
        // can't query it - treat as alive, mirroring the Unix EPERM case.
        return unsafe { GetLastError() } != ERROR_INVALID_PARAMETER;
    }
    let mut exit_code: u32 = 0;
    // SAFETY: `handle` was just returned non-null by `OpenProcess` above and
    // is closed unconditionally after this call.
    let got_exit_code = unsafe { GetExitCodeProcess(handle, &mut exit_code) };
    unsafe { CloseHandle(handle) };
    // If we couldn't read the exit code, don't guess dead - treat as alive.
    got_exit_code == 0 || exit_code == STILL_ACTIVE as u32
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A pid value no live process on Linux (default `pid_max` 32768, or the
    /// raised distro default of a few million) or macOS (`pid_max` 99999)
    /// will ever hold, so `pid_is_alive` reliably reports it dead without
    /// racing a real process.
    const SURELY_DEAD_PID: u32 = 999_999_999;

    #[test]
    fn sweep_removes_dead_pid_entry_and_keeps_live_pid_entry() {
        let root = tempfile::tempdir().expect("create sweep-test root");
        let dead_dir = root.path().join(format!("{SURELY_DEAD_PID}-abcd1234"));
        let live_dir = root.path().join(format!("{}-abcd1234", std::process::id()));
        std::fs::create_dir(&dead_dir).expect("create dead-pid entry");
        std::fs::create_dir(&live_dir).expect("create live-pid entry");

        sweep_stale_run_dirs(root.path());

        assert!(
            !dead_dir.exists(),
            "dead-pid entry must be swept: {}",
            dead_dir.display()
        );
        assert!(
            live_dir.exists(),
            "live-pid entry must be kept: {}",
            live_dir.display()
        );
    }

    #[test]
    fn sweep_removes_entry_older_than_stale_age_even_with_live_pid() {
        let root = tempfile::tempdir().expect("create sweep-test root");
        let old_dir = root.path().join(format!("{}-old", std::process::id()));
        std::fs::create_dir(&old_dir).expect("create aged entry");
        // `std::fs::File::open` + `set_modified` fails on Windows for a
        // directory target (`PermissionDenied`, os error 5): opening a
        // directory as a plain file handle doesn't grant the attribute-write
        // access needed to change its mtime there. `filetime::set_file_mtime`
        // uses the platform-correct call for a directory (on Windows, an
        // explicit `FILE_FLAG_BACKUP_SEMANTICS` open), so it works on every
        // supported OS.
        let old_mtime = SystemTime::now() - (STALE_AGE + Duration::from_mins(1));
        filetime::set_file_mtime(&old_dir, filetime::FileTime::from_system_time(old_mtime))
            .expect("backdate aged entry's mtime");

        sweep_stale_run_dirs(root.path());

        assert!(
            !old_dir.exists(),
            "an entry older than STALE_AGE must be swept even under a live pid: {}",
            old_dir.display()
        );
    }

    #[test]
    fn sweep_on_missing_root_is_a_silent_noop() {
        let parent = tempfile::tempdir().expect("create sweep-test parent");
        let missing_root = parent.path().join("hew-run-does-not-exist");

        // Must not panic and must leave nothing behind - this is the shape of
        // the very first `hew run` on a machine, before any artifact has
        // ever been created.
        sweep_stale_run_dirs(&missing_root);

        assert!(!missing_root.exists());
    }
}
