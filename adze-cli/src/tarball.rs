//! Tarball packing and unpacking for package publishing.
//!
//! Produces reproducible `.tar.zst` archives with sorted entries and zeroed
//! timestamps. Respects `include`/`exclude` globs from the manifest.

use std::fmt;
use std::io::{self, Read};
use std::path::Path;

use sha2::{Digest, Sha256};

/// Maximum compressed tarball size (10 MB).
pub const MAX_TARBALL_SIZE: usize = 10 * 1024 * 1024;

/// Errors from tarball operations.
#[derive(Debug)]
pub enum TarballError {
    /// An I/O error occurred.
    Io(io::Error),
    /// The tarball exceeds the maximum allowed size.
    TooLarge { size: usize, max: usize },
    /// The tarball is missing `hew.toml`.
    MissingManifest,
}

impl fmt::Display for TarballError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Io(e) => write!(f, "tarball I/O error: {e}"),
            Self::TooLarge { size, max } => {
                write!(
                    f,
                    "tarball too large: {size} bytes exceeds {max} byte limit"
                )
            }
            Self::MissingManifest => write!(f, "tarball must contain hew.toml"),
        }
    }
}

impl std::error::Error for TarballError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::Io(e) => Some(e),
            Self::TooLarge { .. } | Self::MissingManifest => None,
        }
    }
}

impl From<io::Error> for TarballError {
    fn from(e: io::Error) -> Self {
        Self::Io(e)
    }
}

/// Result of packing a tarball.
#[derive(Debug)]
pub struct PackResult {
    /// The compressed tarball bytes.
    pub data: Vec<u8>,
    /// SHA-256 checksum in `"sha256:{hex}"` format.
    pub checksum: String,
}

/// Pack a directory into a compressed tarball.
///
/// - `dir`: the package directory to archive
/// - `exclude`: glob patterns to exclude (from manifest `exclude` field)
/// - `include`: if set, only files matching these patterns are included
///
/// Returns the compressed bytes and checksum.
///
/// # Errors
///
/// Returns [`TarballError::MissingManifest`] if no `hew.toml` is found,
/// [`TarballError::TooLarge`] if the result exceeds [`MAX_TARBALL_SIZE`],
/// or [`TarballError::Io`] on I/O failures.
pub fn pack(
    dir: &Path,
    exclude: &[String],
    include: &[String],
) -> Result<PackResult, TarballError> {
    // Collect and sort files for reproducibility.
    let mut files = Vec::new();
    collect_files(dir, dir, &mut files, exclude, include)?;
    files.sort();

    // Verify hew.toml is present.
    if !files.iter().any(|f| f == "hew.toml") {
        return Err(TarballError::MissingManifest);
    }

    // Build tar archive in memory.
    let mut tar_data = Vec::new();
    {
        let mut builder = tar::Builder::new(&mut tar_data);
        for rel_path in &files {
            let abs_path = dir.join(rel_path);
            let metadata = std::fs::metadata(&abs_path)?;
            let mut header = tar::Header::new_gnu();
            header.set_size(metadata.len());
            header.set_mode(0o644);
            header.set_uid(0);
            header.set_gid(0);
            header.set_mtime(0);
            header.set_cksum();

            let file_data = std::fs::read(&abs_path)?;
            builder.append_data(&mut header, rel_path, file_data.as_slice())?;
        }
        builder.finish()?;
    }

    // Compress with zstd at maximum level — publish is a one-time cost.
    let compressed = zstd::encode_all(tar_data.as_slice(), 22)
        .map_err(|e| TarballError::Io(io::Error::new(io::ErrorKind::Other, e)))?;

    if compressed.len() > MAX_TARBALL_SIZE {
        return Err(TarballError::TooLarge {
            size: compressed.len(),
            max: MAX_TARBALL_SIZE,
        });
    }

    // Compute checksum of compressed data.
    let hash = Sha256::digest(&compressed);
    let checksum = format!("sha256:{hash:x}");

    Ok(PackResult {
        data: compressed,
        checksum,
    })
}

/// Unpack a compressed tarball into a target directory.
///
/// # Errors
///
/// Returns [`TarballError::Io`] on I/O failures.
pub fn unpack(data: &[u8], target: &Path) -> Result<(), TarballError> {
    std::fs::create_dir_all(target)?;

    let decompressed = zstd::decode_all(data)
        .map_err(|e| TarballError::Io(io::Error::new(io::ErrorKind::InvalidData, e)))?;

    let mut archive = tar::Archive::new(decompressed.as_slice());
    for entry in archive.entries()? {
        let mut entry = entry?;
        let path = entry.path()?.into_owned();

        // Security: reject absolute paths and path traversal.
        if path.is_absolute()
            || path
                .components()
                .any(|c| c == std::path::Component::ParentDir)
        {
            continue;
        }

        let target_path = target.join(&path);
        if let Some(parent) = target_path.parent() {
            std::fs::create_dir_all(parent)?;
        }

        let mut contents = Vec::new();
        entry.read_to_end(&mut contents)?;
        std::fs::write(&target_path, contents)?;
    }
    Ok(())
}

/// Compute the SHA-256 checksum of raw bytes.
///
/// Returns `"sha256:{hex}"` format.
#[must_use]
pub fn checksum_bytes(data: &[u8]) -> String {
    let hash = Sha256::digest(data);
    format!("sha256:{hash:x}")
}

// ── helpers ─────────────────────────────────────────────────────────────────

/// Recursively collect relative file paths, applying include/exclude filters.
fn collect_files(
    root: &Path,
    dir: &Path,
    files: &mut Vec<String>,
    exclude: &[String],
    include: &[String],
) -> Result<(), io::Error> {
    for entry in std::fs::read_dir(dir)? {
        let entry = entry?;
        let name = entry.file_name();
        let name_str = name.to_string_lossy();

        // Always skip these directories.
        if matches!(name_str.as_ref(), ".git" | "target" | ".adze") {
            continue;
        }

        let path = entry.path();
        if path.is_dir() {
            collect_files(root, &path, files, exclude, include)?;
        } else {
            let rel = path.strip_prefix(root).map_err(io::Error::other)?;
            let rel_str = rel
                .components()
                .map(|c| c.as_os_str().to_string_lossy().into_owned())
                .collect::<Vec<_>>()
                .join("/");

            // Apply include filter (if set, only matching files are included).
            if !include.is_empty() && !matches_any_glob(&rel_str, include) {
                continue;
            }

            // Apply exclude filter.
            if matches_any_glob(&rel_str, exclude) {
                continue;
            }

            files.push(rel_str);
        }
    }
    Ok(())
}

/// Simple glob matching (supports `*` and `**`).
fn matches_any_glob(path: &str, patterns: &[String]) -> bool {
    patterns.iter().any(|pat| glob_match(pat, path))
}

/// Match a path against a simple glob pattern.
///
/// Supports:
/// - `*` — matches any characters within a single path segment
/// - `**` — matches any number of path segments
/// - Literal characters
fn glob_match(pattern: &str, path: &str) -> bool {
    let pat_parts: Vec<&str> = pattern.split('/').collect();
    let path_parts: Vec<&str> = path.split('/').collect();
    glob_match_parts(&pat_parts, &path_parts)
}

fn glob_match_parts(pattern: &[&str], path: &[&str]) -> bool {
    if pattern.is_empty() {
        return path.is_empty();
    }
    if pattern[0] == "**" {
        // ** matches zero or more path segments.
        for i in 0..=path.len() {
            if glob_match_parts(&pattern[1..], &path[i..]) {
                return true;
            }
        }
        return false;
    }
    if path.is_empty() {
        return false;
    }
    if segment_match(pattern[0], path[0]) {
        glob_match_parts(&pattern[1..], &path[1..])
    } else {
        false
    }
}

/// Match a single path segment against a pattern segment (supports `*`).
fn segment_match(pattern: &str, segment: &str) -> bool {
    if pattern == "*" {
        return true;
    }
    if !pattern.contains('*') {
        return pattern == segment;
    }
    // Simple wildcard: split on * and check prefix/suffix.
    let parts: Vec<&str> = pattern.split('*').collect();
    if parts.len() == 2 {
        segment.starts_with(parts[0]) && segment.ends_with(parts[1])
    } else {
        pattern == segment
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn setup_package_dir() -> tempfile::TempDir {
        let dir = tempfile::tempdir().unwrap();
        std::fs::write(
            dir.path().join("hew.toml"),
            "[package]\nname = \"test\"\nversion = \"0.1.0\"\n",
        )
        .unwrap();
        std::fs::write(dir.path().join("main.hew"), "fn main() {}\n").unwrap();
        std::fs::create_dir(dir.path().join("src")).unwrap();
        std::fs::write(
            dir.path().join("src/lib.hew"),
            "fn add(a: i32, b: i32) -> i32 { a + b }\n",
        )
        .unwrap();
        dir
    }

    #[test]
    fn pack_and_unpack_roundtrip() {
        let src = setup_package_dir();
        let result = pack(src.path(), &[], &[]).unwrap();
        assert!(result.checksum.starts_with("sha256:"));
        assert!(!result.data.is_empty());

        let dst = tempfile::tempdir().unwrap();
        unpack(&result.data, dst.path()).unwrap();

        assert!(dst.path().join("hew.toml").exists());
        assert!(dst.path().join("main.hew").exists());
        assert!(dst.path().join("src/lib.hew").exists());

        let original = std::fs::read_to_string(src.path().join("main.hew")).unwrap();
        let unpacked = std::fs::read_to_string(dst.path().join("main.hew")).unwrap();
        assert_eq!(original, unpacked);
    }

    #[test]
    fn pack_is_deterministic() {
        let src = setup_package_dir();
        let r1 = pack(src.path(), &[], &[]).unwrap();
        let r2 = pack(src.path(), &[], &[]).unwrap();
        assert_eq!(r1.checksum, r2.checksum);
        assert_eq!(r1.data, r2.data);
    }

    #[test]
    fn pack_excludes_patterns() {
        let src = setup_package_dir();
        std::fs::create_dir(src.path().join("tests")).unwrap();
        std::fs::write(src.path().join("tests/test.hew"), "test").unwrap();

        let result = pack(src.path(), &["tests/*".to_string()], &[]).unwrap();
        let dst = tempfile::tempdir().unwrap();
        unpack(&result.data, dst.path()).unwrap();

        assert!(dst.path().join("hew.toml").exists());
        assert!(!dst.path().join("tests/test.hew").exists());
    }

    #[test]
    fn pack_include_filter() {
        let src = setup_package_dir();
        std::fs::write(src.path().join("extra.txt"), "extra").unwrap();

        let result = pack(
            src.path(),
            &[],
            &["*.hew".to_string(), "hew.toml".to_string()],
        )
        .unwrap();
        let dst = tempfile::tempdir().unwrap();
        unpack(&result.data, dst.path()).unwrap();

        assert!(dst.path().join("hew.toml").exists());
        assert!(dst.path().join("main.hew").exists());
        assert!(!dst.path().join("extra.txt").exists());
    }

    #[test]
    fn pack_rejects_missing_manifest() {
        let dir = tempfile::tempdir().unwrap();
        std::fs::write(dir.path().join("main.hew"), "fn main() {}").unwrap();
        let result = pack(dir.path(), &[], &[]);
        assert!(matches!(result, Err(TarballError::MissingManifest)));
    }

    #[test]
    fn pack_skips_git_and_target() {
        let src = setup_package_dir();
        std::fs::create_dir(src.path().join(".git")).unwrap();
        std::fs::write(src.path().join(".git/HEAD"), "ref").unwrap();
        std::fs::create_dir(src.path().join("target")).unwrap();
        std::fs::write(src.path().join("target/bin"), "binary").unwrap();

        let result = pack(src.path(), &[], &[]).unwrap();
        let dst = tempfile::tempdir().unwrap();
        unpack(&result.data, dst.path()).unwrap();

        assert!(!dst.path().join(".git").exists());
        assert!(!dst.path().join("target").exists());
    }

    #[test]
    fn checksum_bytes_format() {
        let data = b"hello world";
        let cksum = checksum_bytes(data);
        assert!(cksum.starts_with("sha256:"));
        assert_eq!(cksum.len(), 71); // sha256: + 64 hex chars
    }

    #[test]
    fn checksum_bytes_is_deterministic() {
        let data = b"package tarball contents";
        assert_eq!(checksum_bytes(data), checksum_bytes(data));
    }

    #[test]
    fn checksum_bytes_detects_modification() {
        let original = b"original tarball";
        let modified = b"modified tarball";
        assert_ne!(checksum_bytes(original), checksum_bytes(modified));
    }

    #[test]
    fn pack_checksum_matches_tarball_bytes() {
        // Verify that the checksum returned by pack() matches
        // what checksum_bytes() computes on the tarball data.
        let src = setup_package_dir();
        let result = pack(src.path(), &[], &[]).unwrap();
        let recomputed = checksum_bytes(&result.data);
        assert_eq!(result.checksum, recomputed);
    }

    #[test]
    fn glob_match_star() {
        assert!(glob_match("*.hew", "main.hew"));
        assert!(!glob_match("*.hew", "main.rs"));
        assert!(glob_match("tests/*", "tests/test.hew"));
        assert!(!glob_match("tests/*", "src/main.hew"));
    }

    #[test]
    fn glob_match_doublestar() {
        assert!(glob_match("**/*.hew", "src/lib.hew"));
        assert!(glob_match("**/*.hew", "deep/nested/file.hew"));
        assert!(glob_match("src/**", "src/lib.hew"));
        assert!(glob_match("src/**", "src/deep/nested.hew"));
    }

    #[test]
    fn glob_match_literal() {
        assert!(glob_match("hew.toml", "hew.toml"));
        assert!(!glob_match("hew.toml", "other.toml"));
    }
}
