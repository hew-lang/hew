//! Scan per-user discovery directory for running Hew profilers.
//!
//! Uses the same directory resolution as the runtime
//! (`$XDG_RUNTIME_DIR/hew-profilers/` → `$TMPDIR/hew-profilers-{uid}/`
//! → `/tmp/hew-profilers-{uid}/`).

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

    let Ok(entries) = fs::read_dir(&dir) else {
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
            // Prune stale entry.
            let _ = fs::remove_file(&path);
            let _ = fs::remove_file(&profiler.socket_path);
        }
    }

    profilers.sort_by_key(|p| p.pid);
    profilers
}

/// Look up a specific PID in the discovery directory.
pub fn find_by_pid(pid: u32) -> Option<DiscoveredProfiler> {
    let dir = discovery_dir()?;
    let path = dir.join(format!("{pid}.json"));
    let profiler = parse_discovery_file(&path)?;

    if is_pid_alive(profiler.pid) {
        Some(profiler)
    } else {
        // Prune stale.
        let _ = fs::remove_file(&path);
        let _ = fs::remove_file(&profiler.socket_path);
        None
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

    #[expect(
        clippy::cast_possible_truncation,
        reason = "PID values fit in u32 on all supported platforms"
    )]
    Some(DiscoveredProfiler {
        pid: val.get("pid")?.as_u64()? as u32,
        socket_path: PathBuf::from(val.get("socket")?.as_str()?),
        started: val.get("started")?.as_u64().unwrap_or(0),
        program: val
            .get("program")
            .and_then(|v| v.as_str())
            .unwrap_or("unknown")
            .to_owned(),
    })
}

fn is_pid_alive(pid: u32) -> bool {
    #[expect(
        clippy::cast_possible_wrap,
        reason = "PID values fit in i32 on all supported platforms"
    )]
    // SAFETY: kill(pid, 0) checks if the process exists without sending a
    // signal. No preconditions beyond a valid signal number (0 is valid).
    let result = unsafe { libc::kill(pid as i32, 0) };
    result == 0
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
}
