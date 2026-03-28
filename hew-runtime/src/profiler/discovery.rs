//! Per-user profiler discovery directory and registration.
//!
//! When `HEW_PPROF=auto` or `HEW_PPROF=1`, the profiler binds to a unix
//! domain socket and writes a JSON discovery file so `hew-observe` can
//! find running programs automatically.
//!
//! # Directory resolution
//!
//! 1. `$XDG_RUNTIME_DIR/hew-profilers/`
//! 2. `$TMPDIR/hew-profilers-{uid}/`
//! 3. `/tmp/hew-profilers-{uid}/`
//!
//! All directories are created mode 0700 and validated for ownership.

use std::fs;
use std::io;
use std::os::unix::fs::PermissionsExt;
use std::path::{Path, PathBuf};

/// Resolve the per-user discovery directory, creating it if needed.
///
/// Returns `None` if no suitable directory can be created or validated.
pub fn discovery_dir() -> Option<PathBuf> {
    let uid = unsafe { libc::getuid() };

    // Try $XDG_RUNTIME_DIR/hew-profilers/ first (Linux, per-user, mode 0700).
    if let Ok(xdg) = std::env::var("XDG_RUNTIME_DIR") {
        if !xdg.is_empty() {
            let dir = PathBuf::from(xdg).join("hew-profilers");
            if ensure_dir(&dir, uid) {
                return Some(dir);
            }
        }
    }

    // Try $TMPDIR/hew-profilers-{uid}/ (macOS sets TMPDIR to a per-user path).
    if let Ok(tmpdir) = std::env::var("TMPDIR") {
        if !tmpdir.is_empty() {
            let dir = PathBuf::from(tmpdir).join(format!("hew-profilers-{uid}"));
            if ensure_dir(&dir, uid) {
                return Some(dir);
            }
        }
    }

    // Fallback: /tmp/hew-profilers-{uid}/
    let dir = PathBuf::from(format!("/tmp/hew-profilers-{uid}"));
    if ensure_dir(&dir, uid) {
        return Some(dir);
    }

    None
}

/// Socket path for the current process.
pub fn socket_path(dir: &Path) -> PathBuf {
    let pid = std::process::id();
    dir.join(format!("{pid}.sock"))
}

/// Discovery file path for the current process.
pub fn discovery_file_path(dir: &Path) -> PathBuf {
    let pid = std::process::id();
    dir.join(format!("{pid}.json"))
}

/// Write the discovery JSON file for the current process.
pub fn write_discovery_file(dir: &Path, socket_path: &Path) -> io::Result<PathBuf> {
    let pid = std::process::id();
    let path = discovery_file_path(dir);

    let program = std::env::args()
        .next()
        .and_then(|arg0| {
            Path::new(&arg0)
                .file_name()
                .map(|n| n.to_string_lossy().into_owned())
        })
        .unwrap_or_else(|| "unknown".to_owned());

    let started = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map_or(0, |d| d.as_secs());

    let socket_str = socket_path.to_string_lossy();
    // Escape the socket path for JSON (it may contain special chars on some systems).
    let socket_escaped = socket_str.replace('\\', "\\\\").replace('"', "\\\"");
    let program_escaped = program.replace('\\', "\\\\").replace('"', "\\\"");

    let json = format!(
        r#"{{"pid":{pid},"socket":"{socket_escaped}","started":{started},"program":"{program_escaped}"}}"#
    );

    fs::write(&path, json.as_bytes())?;
    // Mode 0600 — only the owning user can read.
    fs::set_permissions(&path, fs::Permissions::from_mode(0o600))?;

    Ok(path)
}

/// Remove the socket and discovery file for the current process.
pub fn cleanup(dir: &Path) {
    let _ = fs::remove_file(socket_path(dir));
    let _ = fs::remove_file(discovery_file_path(dir));
}

/// Ensure a directory exists with mode 0700 and is owned by `expected_uid`.
///
/// Creates the directory if missing. Refuses to use a directory owned by
/// a different user (defence against symlink attacks in /tmp).
fn ensure_dir(dir: &Path, expected_uid: u32) -> bool {
    if dir.exists() {
        return validate_dir(dir, expected_uid);
    }

    // Create with restrictive permissions.
    if fs::create_dir_all(dir).is_err() {
        return false;
    }
    if fs::set_permissions(dir, fs::Permissions::from_mode(0o700)).is_err() {
        return false;
    }

    validate_dir(dir, expected_uid)
}

/// Check that a directory is owned by `expected_uid` and has no
/// group/other permissions.
fn validate_dir(dir: &Path, expected_uid: u32) -> bool {
    use std::os::unix::fs::MetadataExt;

    let meta = match fs::metadata(dir) {
        Ok(m) => m,
        Err(_) => return false,
    };

    if !meta.is_dir() {
        return false;
    }

    // Owner must match.
    if meta.uid() != expected_uid {
        eprintln!(
            "[hew-pprof] discovery dir {} owned by uid {}, expected {}",
            dir.display(),
            meta.uid(),
            expected_uid,
        );
        return false;
    }

    // Must not have group/other permissions.
    let mode = meta.permissions().mode() & 0o777;
    if mode & 0o077 != 0 {
        eprintln!(
            "[hew-pprof] discovery dir {} has mode {:04o}, expected 0700",
            dir.display(),
            mode,
        );
        return false;
    }

    true
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::os::unix::fs::MetadataExt;

    #[test]
    fn socket_path_contains_pid() {
        let dir = Path::new("/tmp/test-hew-profilers");
        let path = socket_path(dir);
        let pid = std::process::id();
        assert_eq!(path, dir.join(format!("{pid}.sock")));
    }

    #[test]
    fn discovery_file_path_contains_pid() {
        let dir = Path::new("/tmp/test-hew-profilers");
        let path = discovery_file_path(dir);
        let pid = std::process::id();
        assert_eq!(path, dir.join(format!("{pid}.json")));
    }

    #[test]
    fn ensure_dir_creates_with_correct_mode() {
        let tmp = tempfile::tempdir().unwrap();
        let dir = tmp.path().join("hew-profilers");
        let uid = unsafe { libc::getuid() };

        assert!(ensure_dir(&dir, uid));
        assert!(dir.is_dir());

        let mode = fs::metadata(&dir).unwrap().permissions().mode() & 0o777;
        assert_eq!(mode, 0o700);
    }

    #[test]
    fn ensure_dir_reuses_existing_valid_dir() {
        let tmp = tempfile::tempdir().unwrap();
        let dir = tmp.path().join("hew-profilers");
        let uid = unsafe { libc::getuid() };

        fs::create_dir_all(&dir).unwrap();
        fs::set_permissions(&dir, fs::Permissions::from_mode(0o700)).unwrap();

        assert!(ensure_dir(&dir, uid));
    }

    #[test]
    fn validate_dir_rejects_wrong_uid() {
        let tmp = tempfile::tempdir().unwrap();
        let dir = tmp.path().join("hew-profilers");
        fs::create_dir_all(&dir).unwrap();
        fs::set_permissions(&dir, fs::Permissions::from_mode(0o700)).unwrap();

        // Use a UID that isn't ours.
        let fake_uid = unsafe { libc::getuid() } + 1;
        assert!(!validate_dir(&dir, fake_uid));
    }

    #[test]
    fn validate_dir_rejects_permissive_mode() {
        let tmp = tempfile::tempdir().unwrap();
        let dir = tmp.path().join("hew-profilers");
        let uid = unsafe { libc::getuid() };

        fs::create_dir_all(&dir).unwrap();
        fs::set_permissions(&dir, fs::Permissions::from_mode(0o755)).unwrap();

        assert!(!validate_dir(&dir, uid));
    }

    #[test]
    fn write_and_cleanup_discovery_file() {
        let tmp = tempfile::tempdir().unwrap();
        let dir = tmp.path();
        let sock = dir.join(format!("{}.sock", std::process::id()));

        let path = write_discovery_file(dir, &sock).unwrap();
        assert!(path.exists());

        // Verify the file is valid JSON with expected fields.
        let contents = fs::read_to_string(&path).unwrap();
        assert!(contents.contains(&format!("\"pid\":{}", std::process::id())));
        assert!(contents.contains("\"socket\":"));
        assert!(contents.contains("\"started\":"));
        assert!(contents.contains("\"program\":"));

        // Verify mode 0600.
        let mode = fs::metadata(&path).unwrap().permissions().mode() & 0o777;
        assert_eq!(mode, 0o600);

        // Create a fake socket file so cleanup can remove it.
        fs::write(&sock, b"").unwrap();

        cleanup(dir);
        assert!(!path.exists());
        assert!(!sock.exists());
    }

    #[test]
    fn validate_dir_rejects_file_not_dir() {
        let tmp = tempfile::tempdir().unwrap();
        let path = tmp.path().join("not-a-dir");
        fs::write(&path, b"").unwrap();
        let uid = unsafe { libc::getuid() };

        assert!(!validate_dir(&path, uid));
    }
}
