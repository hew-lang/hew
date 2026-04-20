use std::io;
use std::path::Path;

use sha2::{Digest, Sha256};

const SKIPPED_PACKAGE_DIRS: &[&str] = &[".git", "target", ".adze"];

/// Return true when a directory should be skipped for package traversal.
#[must_use]
pub fn is_skipped_package_dir(name: &str) -> bool {
    SKIPPED_PACKAGE_DIRS.contains(&name)
}

/// Recursively collect relative file paths under `root`, using `/` separators.
pub fn collect_package_files(root: &Path) -> Result<Vec<String>, io::Error> {
    let mut files = Vec::new();
    collect_package_files_inner(root, root, &mut files)?;
    Ok(files)
}

fn collect_package_files_inner(
    root: &Path,
    dir: &Path,
    files: &mut Vec<String>,
) -> Result<(), io::Error> {
    for entry in std::fs::read_dir(dir)? {
        let entry = entry?;
        let name = entry.file_name();
        let name_str = name.to_string_lossy();
        if is_skipped_package_dir(&name_str) {
            continue;
        }
        let path = entry.path();
        if path.is_dir() {
            collect_package_files_inner(root, &path, files)?;
        } else {
            let rel = path.strip_prefix(root).map_err(io::Error::other)?;
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

/// Compute a `sha256:{hex}` digest string for raw bytes.
#[must_use]
pub fn sha256_prefixed(data: &[u8]) -> String {
    let hash = Sha256::digest(data);
    format!("sha256:{hash:x}")
}
