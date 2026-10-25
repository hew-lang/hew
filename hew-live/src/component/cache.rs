//! Immutable component bytes retained across host restarts.

use std::fmt::Write as _;
use std::io::{self, Write};
use std::path::{Path, PathBuf};

use sha2::{Digest, Sha256};

/// A content-addressed store of portable component bytes. Loading always checks
/// the digest; execution still requires validation by the host's current engine.
/// Compiled native code is never deserialized from this store.
#[derive(Debug)]
pub struct ArtifactCache {
    directory: PathBuf,
}

impl ArtifactCache {
    /// Open or create a persistent component directory.
    ///
    /// # Errors
    /// Returns filesystem errors creating the directory.
    pub fn open(directory: impl AsRef<Path>) -> io::Result<Self> {
        std::fs::create_dir_all(directory.as_ref())?;
        Ok(Self {
            directory: directory.as_ref().to_owned(),
        })
    }

    /// Read a complete artefact and verify its content identity.
    ///
    /// # Errors
    /// Returns filesystem errors or `InvalidData` for corrupted bytes.
    pub fn load(&self, expected: [u8; 32]) -> io::Result<Vec<u8>> {
        let bytes = std::fs::read(self.path(expected))?;
        if digest(&bytes) != expected {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "cached component digest does not match its content",
            ));
        }
        Ok(bytes)
    }

    pub(super) fn insert(&self, bytes: &[u8], expected: [u8; 32]) -> io::Result<()> {
        let mut staged = tempfile::NamedTempFile::new_in(&self.directory)?;
        staged.write_all(bytes)?;
        staged.as_file().sync_all()?;
        match staged.persist_noclobber(self.path(expected)) {
            Ok(_) => {
                #[cfg(unix)]
                std::fs::File::open(&self.directory)?.sync_all()?;
                Ok(())
            }
            Err(error) if error.error.kind() == io::ErrorKind::AlreadyExists => {
                self.load(expected)?;
                Ok(())
            }
            Err(error) => Err(error.error),
        }
    }

    fn path(&self, digest: [u8; 32]) -> PathBuf {
        let mut name = format_digest(digest);
        name.push_str(".wasm");
        self.directory.join(name)
    }
}

/// Encode a portable component's content identity.
#[must_use]
pub fn format_digest(digest: [u8; 32]) -> String {
    let mut text = String::with_capacity(64);
    for byte in digest {
        let _ = write!(text, "{byte:02x}");
    }
    text
}

pub(super) fn digest(bytes: &[u8]) -> [u8; 32] {
    Sha256::digest(bytes).into()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn reopening_preserves_bytes_and_repeated_publication() {
        let directory = tempfile::tempdir().unwrap();
        let bytes = b"immutable component contents";
        let identity = digest(bytes);
        {
            let cache = ArtifactCache::open(directory.path()).unwrap();
            cache.insert(bytes, identity).unwrap();
            cache.insert(bytes, identity).unwrap();
        }
        let reopened = ArtifactCache::open(directory.path()).unwrap();
        assert_eq!(reopened.load(identity).unwrap(), bytes);
        assert_eq!(std::fs::read_dir(directory.path()).unwrap().count(), 1);
    }

    #[test]
    fn corruption_is_rejected_without_overwriting_the_entry() {
        let directory = tempfile::tempdir().unwrap();
        let cache = ArtifactCache::open(directory.path()).unwrap();
        let bytes = b"original bytes";
        let identity = digest(bytes);
        cache.insert(bytes, identity).unwrap();
        std::fs::write(cache.path(identity), b"truncated").unwrap();
        assert_eq!(
            cache.load(identity).unwrap_err().kind(),
            io::ErrorKind::InvalidData
        );
        assert_eq!(
            cache.insert(bytes, identity).unwrap_err().kind(),
            io::ErrorKind::InvalidData
        );
        assert_eq!(std::fs::read(cache.path(identity)).unwrap(), b"truncated");
    }
}
