//! The temporary directory one `hew run`, `hew debug` or `hew eval`
//! invocation owns for the program it builds.
//!
//! Each invocation creates its own `$TMPDIR/hew-<purpose>-XXXXXX`: a fresh,
//! owner-only (mode 0700) directory with a random name, the way `go run` uses
//! `$TMPDIR/go-build…`. The invocation that created it is its only owner. It
//! removes the directory once the program it ran has finished, on every path
//! it controls: a normal exit, a failing program, a compile error, a timeout,
//! and an interrupt or termination request it waits out. No other `hew`
//! process reads, sweeps or deletes it.
//!
//! A process killed outright (`SIGKILL`, `TerminateProcess`) runs no code at
//! all, so it cannot remove its directory, exactly as `go run` leaves its
//! build directory behind. What the runner guarantees instead is that the
//! program it started dies with it where the platform can say so (see
//! `process::run_command_interactive`), so nothing keeps running from, or
//! writing into, the leftover directory.

use std::path::Path;

/// A directory this invocation owns; dropping it removes it and everything
/// the build put in it.
pub struct RunTempDir {
    directory: tempfile::TempDir,
}

impl RunTempDir {
    pub fn path(&self) -> &Path {
        self.directory.path()
    }
}

/// Create this invocation's own directory, named `hew-<purpose>-XXXXXX`, in
/// the platform temporary directory (`TMPDIR` on Unix, `TEMP` on Windows).
pub fn create(purpose: &str) -> std::io::Result<RunTempDir> {
    create_in(&std::env::temp_dir(), purpose)
}

fn create_in(parent: &Path, purpose: &str) -> std::io::Result<RunTempDir> {
    let prefix = format!("hew-{purpose}-");
    let mut builder = tempfile::Builder::new();
    builder.prefix(&prefix);
    // Owner-only, so no other user on a shared temporary directory can read
    // or replace the program between its link and its start.
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        builder.permissions(std::fs::Permissions::from_mode(0o700));
    }
    let directory = builder.tempdir_in(parent)?;
    Ok(RunTempDir { directory })
}

/// The name a temporary build gives its program: the source's stem, so the
/// program's `argv[0]`, `ps` listings and debugger sessions name the program
/// rather than a placeholder. A stem that is not a plain file name (empty, or
/// `.`/`..`) falls back to `main`.
pub fn program_stem(source: &Path) -> String {
    source
        .file_stem()
        .and_then(|stem| stem.to_str())
        .filter(|stem| !stem.is_empty() && *stem != "." && *stem != "..")
        .unwrap_or("main")
        .to_string()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn each_invocation_owns_a_fresh_named_directory_and_removes_only_it() {
        let parent = tempfile::tempdir().unwrap();
        // A directory some earlier, killed invocation left behind.
        let leftover = parent.path().join("hew-run-abandoned");
        std::fs::create_dir(&leftover).unwrap();
        std::fs::write(leftover.join("program"), b"stale").unwrap();

        let first = create_in(parent.path(), "run").unwrap();
        let second = create_in(parent.path(), "run").unwrap();
        assert_ne!(first.path(), second.path());
        let name = first.path().file_name().unwrap().to_str().unwrap();
        assert!(name.starts_with("hew-run-"), "{name}");

        let path = first.path().to_owned();
        std::fs::write(path.join("program"), b"artifact").unwrap();
        drop(first);
        assert!(!path.exists(), "the owner removes its own directory");
        assert!(
            second.path().exists(),
            "another invocation's directory is untouched"
        );
        assert!(
            leftover.join("program").exists(),
            "no invocation removes a directory it did not create"
        );
    }

    #[cfg(unix)]
    #[test]
    fn the_directory_is_private_to_its_owner() {
        use std::os::unix::fs::PermissionsExt;
        let parent = tempfile::tempdir().unwrap();
        let directory = create_in(parent.path(), "run").unwrap();
        let mode = std::fs::metadata(directory.path())
            .unwrap()
            .permissions()
            .mode();
        assert_eq!(mode & 0o077, 0, "mode {mode:o}");
    }

    #[test]
    fn programs_are_named_after_their_source() {
        assert_eq!(program_stem(Path::new("src/server.hew")), "server");
        assert_eq!(program_stem(Path::new("main.hew")), "main");
        assert_eq!(program_stem(Path::new("..")), "main");
        assert_eq!(program_stem(Path::new("")), "main");
    }
}
