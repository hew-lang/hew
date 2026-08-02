//! Tarball packing and unpacking for package publishing.
//!
//! Produces reproducible `.tar.zst` archives with sorted entries and zeroed
//! timestamps. Respects `include`/`exclude` globs from the manifest.

use std::fmt;
use std::fs::File;
use std::io::{self, Read, Write};
use std::path::{Component, Path, PathBuf};

/// Maximum compressed tarball size (10 MB).
pub const MAX_TARBALL_SIZE: usize = 10 * 1024 * 1024;

/// Maximum cumulative number of bytes extracted from a tarball.
pub const MAX_UNPACKED_TARBALL_BYTES: u64 = 64 * 1024 * 1024;

/// Errors from tarball operations.
#[derive(Debug)]
pub enum TarballError {
    /// An I/O error occurred.
    Io(io::Error),
    /// The tarball exceeds the maximum allowed size.
    TooLarge { size: u64, max: u64 },
    /// The tarball is missing `hew.toml`.
    MissingManifest,
    /// The tarball contains an invalid path.
    InvalidPath(PathBuf),
    /// The tarball contains an unsupported entry type.
    UnsupportedEntryType { path: PathBuf, kind: String },
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
            Self::InvalidPath(path) => {
                write!(
                    f,
                    "tarball contains invalid entry path `{}`",
                    path.display()
                )
            }
            Self::UnsupportedEntryType { path, kind } => {
                write!(
                    f,
                    "tarball entry `{}` has unsupported type {kind}",
                    path.display()
                )
            }
        }
    }
}

impl std::error::Error for TarballError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::Io(e) => Some(e),
            Self::TooLarge { .. }
            | Self::MissingManifest
            | Self::InvalidPath(_)
            | Self::UnsupportedEntryType { .. } => None,
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
    let mut files = crate::package_fs::collect_package_files(dir)?;
    files.retain(|path| include.is_empty() || matches_any_glob(path, include));
    files.retain(|path| !matches_any_glob(path, exclude));
    files.sort();

    if !files.iter().any(|f| f == "hew.toml") {
        return Err(TarballError::MissingManifest);
    }

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

    let compressed = zstd::encode_all(tar_data.as_slice(), 22)
        .map_err(|e| TarballError::Io(io::Error::other(e)))?;

    if compressed.len() > MAX_TARBALL_SIZE {
        return Err(TarballError::TooLarge {
            size: compressed.len() as u64,
            max: MAX_TARBALL_SIZE as u64,
        });
    }

    Ok(PackResult {
        checksum: crate::package_fs::sha256_prefixed(&compressed),
        data: compressed,
    })
}

/// Unpack a compressed tarball into a target directory.
///
/// # Errors
///
/// Returns [`TarballError`] when the archive is malformed, unsafe, or exceeds
/// the cumulative extraction limit.
pub fn unpack(data: &[u8], target: &Path) -> Result<(), TarballError> {
    unpack_with_limit(data, target, MAX_UNPACKED_TARBALL_BYTES)
}

fn unpack_with_limit(data: &[u8], target: &Path, max_bytes: u64) -> Result<(), TarballError> {
    struct CleanupOnError {
        path: PathBuf,
        active: bool,
    }

    impl Drop for CleanupOnError {
        fn drop(&mut self) {
            if self.active {
                let _ = std::fs::remove_dir_all(&self.path);
            }
        }
    }

    let target_existed = target.exists();
    std::fs::create_dir_all(target)?;
    let mut cleanup = CleanupOnError {
        path: target.to_path_buf(),
        active: !target_existed,
    };

    let decoder = zstd::Decoder::new(std::io::Cursor::new(data))
        .map_err(|e| TarballError::Io(io::Error::new(io::ErrorKind::InvalidData, e)))?;
    let mut archive = tar::Archive::new(decoder);
    let mut total_bytes = 0_u64;
    let mut found_manifest = false;

    for entry in archive.entries()? {
        let mut entry = entry?;
        let original_path = entry.path()?.into_owned();
        let normalized_path = normalize_unpack_path(&original_path)?;
        let target_path = target.join(&normalized_path);
        let entry_type = entry.header().entry_type();

        if normalized_path == Path::new("hew.toml") {
            found_manifest = true;
        }

        if entry_type.is_dir() {
            std::fs::create_dir_all(&target_path)?;
            continue;
        }

        if !entry_type.is_file() {
            return Err(TarballError::UnsupportedEntryType {
                path: normalized_path,
                kind: format!("{entry_type:?}"),
            });
        }

        if let Some(parent) = target_path.parent() {
            std::fs::create_dir_all(parent)?;
        }

        let mut output = File::create(&target_path)?;
        let mut buffer = [0_u8; 8192];
        loop {
            let bytes_read = entry.read(&mut buffer)?;
            if bytes_read == 0 {
                break;
            }
            total_bytes += bytes_read as u64;
            if total_bytes > max_bytes {
                return Err(TarballError::TooLarge {
                    size: total_bytes,
                    max: max_bytes,
                });
            }
            output.write_all(&buffer[..bytes_read])?;
        }
        output.sync_all()?;
    }

    if !found_manifest {
        return Err(TarballError::MissingManifest);
    }

    cleanup.active = false;
    Ok(())
}

fn normalize_unpack_path(path: &Path) -> Result<PathBuf, TarballError> {
    let mut normalized = PathBuf::new();
    for component in path.components() {
        match component {
            Component::Normal(part) => normalized.push(part),
            Component::CurDir => {}
            Component::ParentDir | Component::RootDir | Component::Prefix(_) => {
                return Err(TarballError::InvalidPath(path.to_path_buf()));
            }
        }
    }

    if normalized.as_os_str().is_empty() {
        return Err(TarballError::InvalidPath(path.to_path_buf()));
    }

    Ok(normalized)
}

/// Compute the SHA-256 checksum of raw bytes.
///
/// Returns `"sha256:{hex}"` format.
#[must_use]
pub fn checksum_bytes(data: &[u8]) -> String {
    crate::package_fs::sha256_prefixed(data)
}

fn matches_any_glob(path: &str, patterns: &[String]) -> bool {
    patterns.iter().any(|pat| glob_match(pat, path))
}

/// Match a path against a simple glob pattern.
///
/// Supports:
/// - `*` — matches any characters within a single path segment
/// - `**` — matches any number of path segments
/// - Literal characters
#[expect(
    clippy::similar_names,
    reason = "pat_parts and path_parts are distinct concepts"
)]
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

fn segment_match(pattern: &str, segment: &str) -> bool {
    if pattern == "*" {
        return true;
    }
    if !pattern.contains('*') {
        return pattern == segment;
    }
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

    fn compressed_tar(entries: &[(&str, &[u8])]) -> Vec<u8> {
        let mut tar_data = Vec::new();
        {
            let mut builder = tar::Builder::new(&mut tar_data);
            for (path, contents) in entries {
                let mut header = tar::Header::new_gnu();
                header.set_size(contents.len() as u64);
                header.set_mode(0o644);
                header.set_uid(0);
                header.set_gid(0);
                header.set_mtime(0);
                header.set_cksum();
                builder.append_data(&mut header, *path, *contents).unwrap();
            }
            builder.finish().unwrap();
        }
        zstd::encode_all(tar_data.as_slice(), 22).unwrap()
    }

    fn compressed_raw_tar(entries: &[(&[u8], &[u8])]) -> Vec<u8> {
        fn write_octal(field: &mut [u8], value: u64) {
            let width = field.len();
            let formatted = format!("{value:0width$o}\0", width = width - 1);
            field[..formatted.len()].copy_from_slice(formatted.as_bytes());
        }

        let mut tar_data = Vec::new();
        for (path, contents) in entries {
            let mut header = [0_u8; 512];
            header[..path.len()].copy_from_slice(path);
            write_octal(&mut header[100..108], 0o644);
            write_octal(&mut header[108..116], 0);
            write_octal(&mut header[116..124], 0);
            write_octal(&mut header[124..136], contents.len() as u64);
            write_octal(&mut header[136..148], 0);
            header[148..156].fill(b' ');
            header[156] = b'0';
            header[257..263].copy_from_slice(b"ustar\0");
            header[263..265].copy_from_slice(b"00");
            let checksum: u32 = header.iter().map(|byte| u32::from(*byte)).sum();
            let checksum_str = format!("{checksum:06o}\0 ");
            header[148..156].copy_from_slice(checksum_str.as_bytes());
            tar_data.extend_from_slice(&header);
            tar_data.extend_from_slice(contents);
            let padding = (512 - (contents.len() % 512)) % 512;
            tar_data.extend(std::iter::repeat_n(0, padding));
        }
        tar_data.extend([0_u8; 1024]);
        zstd::encode_all(tar_data.as_slice(), 22).unwrap()
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
    fn unpack_rejects_missing_manifest() {
        let tarball = compressed_tar(&[("main.hew", b"fn main() {}")]);
        let dst = tempfile::tempdir().unwrap();
        let result = unpack(&tarball, dst.path());
        assert!(matches!(result, Err(TarballError::MissingManifest)));
    }

    #[test]
    fn unpack_rejects_absolute_path() {
        let tarball =
            compressed_raw_tar(&[(b"/hew.toml", b"[package]\nname=\"x\"\nversion=\"1.0.0\"\n")]);
        let dst = tempfile::tempdir().unwrap();
        let result = unpack(&tarball, dst.path());
        assert!(
            matches!(result, Err(TarballError::InvalidPath(path)) if path == Path::new("/hew.toml"))
        );
    }

    #[test]
    fn unpack_rejects_parent_dir_path() {
        let tarball = compressed_raw_tar(&[
            (b"hew.toml", b"[package]\nname=\"x\"\nversion=\"1.0.0\"\n"),
            (b"../escape.txt", b"nope"),
        ]);
        let dst = tempfile::tempdir().unwrap();
        let result = unpack(&tarball, dst.path());
        assert!(
            matches!(result, Err(TarballError::InvalidPath(path)) if path == Path::new("../escape.txt"))
        );
    }

    #[test]
    fn unpack_rejects_cumulative_size_over_limit() {
        let large = vec![b'a'; 4096];
        let tarball = compressed_tar(&[
            ("hew.toml", b"[package]\nname=\"x\"\nversion=\"1.0.0\"\n"),
            ("blob.bin", large.as_slice()),
        ]);
        let dst = tempfile::tempdir().unwrap();
        let result = unpack_with_limit(&tarball, dst.path(), 1024);
        assert!(matches!(
            result,
            Err(TarballError::TooLarge { max: 1024, .. })
        ));
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
        assert_eq!(cksum.len(), 71);
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
