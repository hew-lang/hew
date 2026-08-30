//! Deterministic SHA-256 checksums for package directories.

use std::io;
use std::path::Path;

/// Compute a deterministic SHA-256 checksum of a package directory.
///
/// Walks all files sorted by relative path, hashing each file's relative path
/// and contents. Directories named `.git`, `target`, and `.hew` are skipped.
///
/// Returns a string in `"sha256:{hex}"` format.
///
/// # Errors
///
/// Returns an [`io::Error`] if any file cannot be read or the directory cannot
/// be traversed.
pub fn compute_dir_checksum(dir: &Path) -> Result<String, io::Error> {
    let mut digest = crate::package_fs::TreeDigest::new();
    let files = crate::package_fs::collect_package_snapshot(dir)?;
    for file in &files {
        digest.add_file(&file.path, &file.contents);
    }

    Ok(digest.finish())
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
    fn checksum_frames_path_and_content() {
        let tree_a = tempfile::tempdir().unwrap();
        std::fs::write(tree_a.path().join("a"), "bc").unwrap();

        let tree_b = tempfile::tempdir().unwrap();
        std::fs::write(tree_b.path().join("ab"), "c").unwrap();

        assert_ne!(
            compute_dir_checksum(tree_a.path()).unwrap(),
            compute_dir_checksum(tree_b.path()).unwrap()
        );
    }

    #[test]
    fn checksum_has_stable_domain_version_golden() {
        let dir = tempfile::tempdir().unwrap();
        std::fs::write(dir.path().join("a"), "bc").unwrap();
        assert_eq!(
            compute_dir_checksum(dir.path()).unwrap(),
            "sha256:e76ce035ee455d973bbe1ab1636cb48437c085f1dab958efa6a25370db8073a8"
        );
    }

    #[test]
    fn checksum_skips_package_artifacts() {
        let dir = tempfile::tempdir().unwrap();
        std::fs::write(dir.path().join("main.hew"), "content").unwrap();
        let c1 = compute_dir_checksum(dir.path()).unwrap();

        for (directory, file) in [(".git", "HEAD"), ("target", "out"), (".hew", "state")] {
            std::fs::create_dir(dir.path().join(directory)).unwrap();
            std::fs::write(dir.path().join(directory).join(file), "ignored").unwrap();
        }
        std::fs::write(
            dir.path().join(crate::package_fs::CACHE_METADATA_FILE),
            "mutable metadata",
        )
        .unwrap();
        std::fs::write(
            dir.path().join(crate::package_fs::CACHE_ARCHIVE_FILE),
            "mutable archive",
        )
        .unwrap();
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

    #[cfg(unix)]
    #[test]
    fn checksum_rejects_symlinks() {
        use std::os::unix::fs::symlink;

        let dir = tempfile::tempdir().unwrap();
        std::fs::write(dir.path().join("outside"), "content").unwrap();
        symlink(dir.path().join("outside"), dir.path().join("link")).unwrap();

        assert!(compute_dir_checksum(dir.path()).is_err());
    }
}
