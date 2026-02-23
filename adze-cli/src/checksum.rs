//! Deterministic SHA-256 checksums for package directories.

use std::io;
use std::path::Path;

use sha2::{Digest, Sha256};

/// Compute a deterministic SHA-256 checksum of a package directory.
///
/// Walks all files sorted by relative path, hashing each file's relative path
/// and contents. Directories named `.git`, `target`, and `.adze` are skipped.
///
/// Returns a string in `"sha256:{hex}"` format.
///
/// # Errors
///
/// Returns an [`io::Error`] if any file cannot be read or the directory cannot
/// be traversed.
pub fn compute_dir_checksum(dir: &Path) -> Result<String, io::Error> {
    let mut hasher = Sha256::new();

    let mut files = Vec::new();
    collect_files(dir, dir, &mut files)?;
    files.sort();

    for rel_path in &files {
        let abs_path = dir.join(rel_path);
        hasher.update(rel_path.as_bytes());
        let contents = std::fs::read(&abs_path)?;
        hasher.update(&contents);
    }

    let hash = hasher.finalize();
    Ok(format!("sha256:{hash:x}"))
}

/// Verify that a directory's checksum matches an expected value.
///
/// # Errors
///
/// Returns an [`io::Error`] if the directory cannot be checksummed.
#[cfg(test)]
pub fn verify_checksum(dir: &Path, expected: &str) -> Result<bool, io::Error> {
    let actual = compute_dir_checksum(dir)?;
    Ok(actual == expected)
}

/// Recursively collect relative file paths under `dir`, skipping `.git`,
/// `target`, and `.adze` directories.
fn collect_files(root: &Path, dir: &Path, files: &mut Vec<String>) -> Result<(), io::Error> {
    for entry in std::fs::read_dir(dir)? {
        let entry = entry?;
        let name = entry.file_name();
        let name_str = name.to_string_lossy();
        if matches!(name_str.as_ref(), ".git" | "target" | ".adze") {
            continue;
        }
        let path = entry.path();
        if path.is_dir() {
            collect_files(root, &path, files)?;
        } else {
            let rel = path.strip_prefix(root).map_err(io::Error::other)?;
            // Use forward slashes for cross-platform determinism.
            let rel_str = rel
                .components()
                .map(|c| c.as_os_str().to_string_lossy().into_owned())
                .collect::<Vec<_>>()
                .join("/");
            files.push(rel_str);
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn checksum_format() {
        let dir = tempfile::tempdir().unwrap();
        std::fs::write(dir.path().join("hello.hew"), "actor Main {}").unwrap();

        let checksum = compute_dir_checksum(dir.path()).unwrap();
        assert!(checksum.starts_with("sha256:"), "got: {checksum}");
        // sha256 hex is 64 chars + "sha256:" prefix = 71 total.
        assert_eq!(checksum.len(), 71, "got: {checksum}");
    }

    #[test]
    fn checksum_is_deterministic() {
        let dir = tempfile::tempdir().unwrap();
        std::fs::write(dir.path().join("a.hew"), "actor A {}").unwrap();
        std::fs::write(dir.path().join("b.hew"), "actor B {}").unwrap();

        let c1 = compute_dir_checksum(dir.path()).unwrap();
        let c2 = compute_dir_checksum(dir.path()).unwrap();
        assert_eq!(c1, c2);
    }

    #[test]
    fn checksum_changes_with_content() {
        let dir = tempfile::tempdir().unwrap();
        std::fs::write(dir.path().join("main.hew"), "v1").unwrap();
        let c1 = compute_dir_checksum(dir.path()).unwrap();

        std::fs::write(dir.path().join("main.hew"), "v2").unwrap();
        let c2 = compute_dir_checksum(dir.path()).unwrap();

        assert_ne!(c1, c2);
    }

    #[test]
    fn checksum_changes_with_filename() {
        let dir1 = tempfile::tempdir().unwrap();
        std::fs::write(dir1.path().join("a.hew"), "content").unwrap();
        let c1 = compute_dir_checksum(dir1.path()).unwrap();

        let dir2 = tempfile::tempdir().unwrap();
        std::fs::write(dir2.path().join("b.hew"), "content").unwrap();
        let c2 = compute_dir_checksum(dir2.path()).unwrap();

        assert_ne!(c1, c2);
    }

    #[test]
    fn checksum_skips_git_and_target() {
        let dir = tempfile::tempdir().unwrap();
        std::fs::write(dir.path().join("main.hew"), "content").unwrap();
        let c1 = compute_dir_checksum(dir.path()).unwrap();

        // Adding .git and target dirs should not change checksum.
        std::fs::create_dir(dir.path().join(".git")).unwrap();
        std::fs::write(dir.path().join(".git").join("HEAD"), "ref").unwrap();
        std::fs::create_dir(dir.path().join("target")).unwrap();
        std::fs::write(dir.path().join("target").join("out"), "bin").unwrap();
        let c2 = compute_dir_checksum(dir.path()).unwrap();

        assert_eq!(c1, c2);
    }

    #[test]
    fn verify_checksum_matches() {
        let dir = tempfile::tempdir().unwrap();
        std::fs::write(dir.path().join("main.hew"), "content").unwrap();

        let checksum = compute_dir_checksum(dir.path()).unwrap();
        assert!(verify_checksum(dir.path(), &checksum).unwrap());
    }

    #[test]
    fn verify_checksum_mismatch() {
        let dir = tempfile::tempdir().unwrap();
        std::fs::write(dir.path().join("main.hew"), "content").unwrap();

        assert!(!verify_checksum(dir.path(), "sha256:0000").unwrap());
    }

    #[test]
    fn empty_dir_checksum() {
        let dir = tempfile::tempdir().unwrap();
        let checksum = compute_dir_checksum(dir.path()).unwrap();
        assert!(checksum.starts_with("sha256:"));
    }

    #[test]
    fn subdirectory_files_included() {
        let dir = tempfile::tempdir().unwrap();
        std::fs::create_dir(dir.path().join("src")).unwrap();
        std::fs::write(dir.path().join("src").join("lib.hew"), "actor Lib {}").unwrap();

        let checksum = compute_dir_checksum(dir.path()).unwrap();
        assert!(checksum.starts_with("sha256:"));

        // Changing subdirectory content changes checksum.
        let c1 = checksum;
        std::fs::write(dir.path().join("src").join("lib.hew"), "actor Lib2 {}").unwrap();
        let c2 = compute_dir_checksum(dir.path()).unwrap();
        assert_ne!(c1, c2);
    }
}
