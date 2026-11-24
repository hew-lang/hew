//! Scan per-user discovery directory for running Hew profilers.
//!
//! ## Discovery directory
//!
//! Uses the same directory resolution as the runtime, tried in order:
//!
//! 1. `$XDG_RUNTIME_DIR/hew-profilers/` (Linux standard runtime dir, if set)
//! 2. `$TMPDIR/hew-profilers-{uid}/` (macOS / BSD, if `$TMPDIR` is set)
//! 3. `/tmp/hew-profilers-{uid}/` (fallback)
//!
//! The first directory that exists and is owned by the calling user is used.
//!
//! ## Discovery lifecycle
//!
//! When a Hew program starts with `HEW_PPROF=auto`, the runtime creates a
//! JSON descriptor file (`{pid}.json`) in the discovery directory containing
//! the process PID, the unix socket path, the start timestamp (unix seconds),
//! and the program name.  The unix socket is also created at that point.
//!
//! On clean shutdown the runtime removes both the descriptor file and the
//! socket.  If the process is killed or crashes, [`scan_profilers`] and
//! [`find_by_pid`] prune stale entries automatically: when a PID is no longer
//! alive (`kill(pid, 0)` returns `ESRCH`) the descriptor file and socket are
//! removed before the entry is surfaced to the caller.
//!
//! ## Security
//!
//! [`discovery_dir`] validates that the chosen directory is owned by the
//! calling UID and has no group or other write bits (lower 6 mode bits must be
//! zero), preventing symlink attacks or injection of fake descriptors by other
//! users on the same machine.

use std::fs;
use std::os::unix::fs::PermissionsExt;
use std::path::{Path, PathBuf};

/// A discovered Hew profiler process.
#[derive(Debug, Clone)]
pub struct DiscoveredProfiler {
    pub pid: u32,
    pub socket_path: PathBuf,
    pub started: u64,
    pub program: String,
}

/// Resolve the per-user discovery directory (read-only — does not create).
///
/// Returns `None` if no valid discovery directory exists.
pub fn discovery_dir() -> Option<PathBuf> {
    // SAFETY: getuid() has no preconditions and no side effects.
    let uid = unsafe { libc::getuid() };

    candidate_dirs(uid)
        .into_iter()
        .find(|candidate| candidate.is_dir() && validate_dir_ownership(candidate, uid))
}

/// Scan for all live profilers in the discovery directory.
///
/// Prunes stale entries (dead PIDs) automatically.
pub fn scan_profilers() -> Vec<DiscoveredProfiler> {
    let Some(dir) = discovery_dir() else {
        return Vec::new();
    };
    scan_profilers_in(&dir)
}

fn scan_profilers_in(dir: &Path) -> Vec<DiscoveredProfiler> {
    let Ok(entries) = fs::read_dir(dir) else {
        return Vec::new();
    };

    let mut profilers = Vec::new();
    for entry in entries.flatten() {
        let path = entry.path();
        if path.extension().and_then(|e| e.to_str()) != Some("json") {
            continue;
        }

        let Some(profiler) = parse_discovery_file(&path) else {
            continue;
        };

        if is_pid_alive(profiler.pid) {
            profilers.push(profiler);
        } else {
            prune_stale_entry(dir, &path, &profiler);
        }
    }

    profilers.sort_by_key(|p| p.pid);
    profilers
}

/// Look up a specific PID in the discovery directory.
pub fn find_by_pid(pid: u32) -> Option<DiscoveredProfiler> {
    let dir = discovery_dir()?;
    find_by_pid_in(&dir, pid)
}

fn find_by_pid_in(dir: &Path, pid: u32) -> Option<DiscoveredProfiler> {
    let path = dir.join(format!("{pid}.json"));
    let profiler = parse_discovery_file(&path)?;
    if profiler.pid != pid {
        return None;
    }

    if is_pid_alive(profiler.pid) {
        Some(profiler)
    } else {
        prune_stale_entry(dir, &path, &profiler);
        None
    }
}

fn prune_stale_entry(dir: &Path, descriptor_path: &Path, profiler: &DiscoveredProfiler) {
    if fs::symlink_metadata(descriptor_path).is_ok_and(|meta| meta.file_type().is_file()) {
        let _ = fs::remove_file(descriptor_path);
    }
    remove_stale_socket(dir, profiler.pid, &profiler.socket_path);
}

/// Remove a stale profiler socket only when the descriptor names `<pid>.sock`
/// directly inside `dir` and `symlink_metadata` confirms it is a genuine Unix
/// domain socket (never following symlinks, never unlinking out-of-directory
/// paths or non-socket files).
fn remove_stale_socket(dir: &Path, pid: u32, declared_socket: &Path) {
    use std::os::unix::fs::FileTypeExt;

    let expected_name = format!("{pid}.sock");
    let expected_path = dir.join(&expected_name);

    let has_parent_dir_component = declared_socket
        .components()
        .any(|component| matches!(component, std::path::Component::ParentDir));
    if has_parent_dir_component {
        return;
    }

    let same_direct_child = declared_socket == expected_path
        || (declared_socket.file_name().and_then(|name| name.to_str())
            == Some(expected_name.as_str())
            && declared_socket
                .parent()
                .is_some_and(|parent| fs::canonicalize(parent).ok() == fs::canonicalize(dir).ok()));
    if !same_direct_child {
        return;
    }

    let Ok(meta) = fs::symlink_metadata(&expected_path) else {
        return;
    };
    if meta.file_type().is_socket() {
        let _ = fs::remove_file(&expected_path);
    }
}

fn candidate_dirs(uid: u32) -> Vec<PathBuf> {
    let mut dirs = Vec::new();

    if let Ok(xdg) = std::env::var("XDG_RUNTIME_DIR") {
        if !xdg.is_empty() {
            dirs.push(PathBuf::from(xdg).join("hew-profilers"));
        }
    }

    if let Ok(tmpdir) = std::env::var("TMPDIR") {
        if !tmpdir.is_empty() {
            dirs.push(PathBuf::from(tmpdir).join(format!("hew-profilers-{uid}")));
        }
    }

    dirs.push(PathBuf::from(format!("/tmp/hew-profilers-{uid}")));
    dirs
}

fn validate_dir_ownership(dir: &Path, expected_uid: u32) -> bool {
    use std::os::unix::fs::MetadataExt;

    let Ok(meta) = fs::metadata(dir) else {
        return false;
    };

    if meta.uid() != expected_uid {
        return false;
    }

    // No group/other permissions allowed (lower 6 bits must be zero).
    let mode = meta.permissions().mode() & 0o777;
    mode.trailing_zeros() >= 6
}

fn parse_discovery_file(path: &Path) -> Option<DiscoveredProfiler> {
    let contents = fs::read_to_string(path).ok()?;
    let val: serde_json::Value = serde_json::from_str(&contents).ok()?;

    let pid = val.get("pid")?.as_u64()?;
    // Reject PIDs `kill(2)` cannot address as one process: 0 names the
    // caller's process group, and values past `i32::MAX` wrap to negative
    // group or broadcast targets. Truncating them would probe the wrong PID.
    if pid == 0 || i32::try_from(pid).is_err() {
        return None;
    }

    Some(DiscoveredProfiler {
        pid: u32::try_from(pid).ok()?,
        socket_path: PathBuf::from(val.get("socket")?.as_str()?),
        started: val.get("started")?.as_u64().unwrap_or(0),
        program: val
            .get("program")
            .and_then(|v| v.as_str())
            .unwrap_or("unknown")
            .to_owned(),
    })
}

fn kill_probe_indicates_alive(result: libc::c_int, errno: Option<libc::c_int>) -> bool {
    if result == 0 {
        return true;
    }
    // `EPERM` means the process exists even though this user cannot signal it.
    // Prune a discovery entry only on confirmed `ESRCH`.
    errno != Some(libc::ESRCH)
}

fn is_pid_alive(pid: u32) -> bool {
    // `kill(0, 0)` and negative PIDs address process groups, which would
    // report a stale descriptor as alive.
    let Ok(pid) = libc::pid_t::try_from(pid) else {
        return false;
    };
    if pid <= 0 {
        return false;
    }
    // SAFETY: kill(pid, 0) checks if the process exists without sending a
    // signal. No preconditions beyond a valid signal number (0 is valid).
    let result = unsafe { libc::kill(pid, 0) };
    let errno = (result != 0)
        .then(|| std::io::Error::last_os_error().raw_os_error())
        .flatten();
    kill_probe_indicates_alive(result, errno)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_valid_discovery_file() {
        let tmp = tempfile::tempdir().unwrap();
        let path = tmp.path().join("12345.json");
        fs::write(
            &path,
            r#"{"pid":12345,"socket":"/tmp/hew-profilers/12345.sock","started":1711612800,"program":"my_app"}"#,
        )
        .unwrap();

        let p = parse_discovery_file(&path).unwrap();
        assert_eq!(p.pid, 12345);
        assert_eq!(
            p.socket_path,
            PathBuf::from("/tmp/hew-profilers/12345.sock")
        );
        assert_eq!(p.started, 1_711_612_800);
        assert_eq!(p.program, "my_app");
    }

    #[test]
    fn parse_invalid_json_returns_none() {
        let tmp = tempfile::tempdir().unwrap();
        let path = tmp.path().join("bad.json");
        fs::write(&path, "not json").unwrap();

        assert!(parse_discovery_file(&path).is_none());
    }

    #[test]
    fn parse_missing_fields_returns_none() {
        let tmp = tempfile::tempdir().unwrap();
        let path = tmp.path().join("partial.json");
        fs::write(&path, r#"{"pid":123}"#).unwrap();

        // Missing "socket" field should fail.
        assert!(parse_discovery_file(&path).is_none());
    }

    #[test]
    fn is_pid_alive_for_self() {
        assert!(is_pid_alive(std::process::id()));
    }

    #[test]
    fn is_pid_alive_rejects_bogus_pid() {
        // PID 0 is the kernel scheduler — kill(0, 0) sends signal to the
        // calling process's group, not what we want to test. Use a very
        // large PID that almost certainly doesn't exist.
        assert!(!is_pid_alive(4_000_000));
    }

    #[test]
    fn is_pid_alive_rejects_process_group_targets() {
        // kill(0, 0) probes the caller's own process group and kill(-1, 0)
        // probes every signalable process; both succeed, so neither may be
        // read as "this profiler is alive".
        assert!(!is_pid_alive(0));
        assert!(!is_pid_alive(u32::MAX));
        assert!(!is_pid_alive(1u32 << 31));
    }

    #[test]
    fn parse_rejects_pids_kill_cannot_address() {
        let tmp = tempfile::tempdir().unwrap();
        let self_pid = u64::from(std::process::id());
        for pid in [0, u64::from(u32::MAX), (1u64 << 32) + self_pid] {
            let path = tmp.path().join(format!("{pid}.json"));
            fs::write(
                &path,
                format!(r#"{{"pid":{pid},"socket":"/tmp/x.sock","started":1}}"#),
            )
            .unwrap();
            assert!(
                parse_discovery_file(&path).is_none(),
                "pid {pid} must not parse (it would alias another process or group)"
            );
        }
    }

    #[test]
    fn kill_probe_treats_eperm_as_alive_and_only_esrch_as_dead() {
        assert!(kill_probe_indicates_alive(0, None));
        assert!(kill_probe_indicates_alive(-1, Some(libc::EPERM)));
        assert!(!kill_probe_indicates_alive(-1, Some(libc::ESRCH)));
        // PID 1 always exists on Unix (EPERM when unprivileged, 0 when root).
        assert!(is_pid_alive(1));
    }

    #[test]
    fn stale_cleanup_removes_only_matching_unix_socket_in_discovery_dir() {
        use std::os::unix::net::UnixListener;

        let tmp = tempfile::tempdir().unwrap();
        let discovery = tmp.path().join("discovery");
        let outside = tmp.path().join("outside");
        fs::create_dir_all(&discovery).unwrap();
        fs::create_dir_all(&outside).unwrap();

        // Find a confirmed-dead PID (ESRCH) so pruning triggers reliably.
        let dead_pid = (4_000_000..4_001_000)
            .find(|&pid| !is_pid_alive(pid))
            .expect("find dead pid");
        let write_descriptor = |file_name: &str, pid: u32, socket: &Path| {
            let descriptor = discovery.join(file_name);
            let json = serde_json::json!({
                "pid": pid,
                "socket": socket.to_string_lossy(),
                "started": 1,
                "program": "stale"
            })
            .to_string();
            fs::write(&descriptor, json).unwrap();
            descriptor
        };

        // 1. Genuine Unix socket matching `<pid>.sock` inside discovery dir is removed.
        let valid_sock = discovery.join(format!("{dead_pid}.sock"));
        let listener = UnixListener::bind(&valid_sock).unwrap();
        drop(listener);
        let valid_json = write_descriptor(&format!("{dead_pid}.json"), dead_pid, &valid_sock);
        assert!(scan_profilers_in(&discovery).is_empty());
        assert!(!valid_json.exists(), "stale descriptor should be pruned");
        assert!(
            !valid_sock.exists(),
            "matching stale unix socket should be pruned"
        );

        // 2. Out-of-directory regular file and out-of-directory Unix socket survive.
        let outside_file = outside.join("victim.txt");
        fs::write(&outside_file, b"keep").unwrap();
        write_descriptor("out_file.json", dead_pid, &outside_file);
        let outside_sock = outside.join(format!("{dead_pid}.sock"));
        let outside_listener = UnixListener::bind(&outside_sock).unwrap();
        drop(outside_listener);
        write_descriptor("out_sock.json", dead_pid, &outside_sock);
        assert!(scan_profilers_in(&discovery).is_empty());
        assert!(
            outside_file.exists(),
            "out-of-dir regular file must never be unlinked"
        );
        assert!(
            outside_sock.exists(),
            "out-of-dir unix socket must never be unlinked"
        );

        // 3. Regular file named `<pid>.sock` inside discovery dir survives.
        let regular_in_dir = discovery.join(format!("{dead_pid}.sock"));
        fs::write(&regular_in_dir, b"not a socket").unwrap();
        write_descriptor("reg.json", dead_pid, &regular_in_dir);
        assert!(scan_profilers_in(&discovery).is_empty());
        assert!(
            regular_in_dir.exists(),
            "regular file must never be unlinked as a socket"
        );
        fs::remove_file(&regular_in_dir).unwrap();

        // 4. Symlink named `<pid>.sock` inside discovery dir (even to a real socket) survives.
        let symlink_in_dir = discovery.join(format!("{dead_pid}.sock"));
        std::os::unix::fs::symlink(&outside_sock, &symlink_in_dir).unwrap();
        write_descriptor("sym.json", dead_pid, &symlink_in_dir);
        assert!(scan_profilers_in(&discovery).is_empty());
        assert!(
            fs::symlink_metadata(&symlink_in_dir).is_ok(),
            "symlink at socket path must never be unlinked"
        );
        assert!(
            outside_sock.exists(),
            "symlink target socket must remain untouched"
        );
        fs::remove_file(&symlink_in_dir).unwrap();

        // 5. Genuine Unix socket for a different PID inside discovery dir survives.
        let other_sock = discovery.join(format!("{}.sock", dead_pid + 1));
        let other_listener = UnixListener::bind(&other_sock).unwrap();
        drop(other_listener);
        write_descriptor("wrong_pid.json", dead_pid, &other_sock);
        assert!(scan_profilers_in(&discovery).is_empty());
        assert!(
            other_sock.exists(),
            "socket belonging to a different PID must never be unlinked"
        );
    }
}
