//! Local package registry (`~/.hew/packages/`).

use std::path::{Path, PathBuf};

use serde::{Deserialize, Serialize};

use crate::package_fs::{CACHE_ARCHIVE_FILE, CACHE_METADATA_FILE};

#[derive(Debug, Deserialize, Serialize)]
struct CacheMetadata {
    name: String,
    version: String,
    registry_checksum: String,
    tree_checksum: String,
}

/// An installed package discovered in the global registry.
#[derive(Debug)]
pub struct InstalledPackage {
    /// Package name in dotted notation, e.g. `std.net.http`.
    pub name: String,
    /// Installed version string, e.g. `1.0.0`.
    pub version: String,
    /// Absolute path to the package version directory.
    pub path: PathBuf,
}

#[derive(Debug)]
pub(crate) struct VerifiedCacheEntry {
    pub(crate) path: PathBuf,
    pub(crate) tree_checksum: String,
}

/// The global Hew package registry (`~/.hew/packages/`).
///
/// Package layout on disk:
/// ```text
/// ~/.hew/packages/{package}/{version}/hew.toml
/// ```
/// e.g. `~/.hew/packages/std.net.http/1.0.0/hew.toml`
/// corresponds to the package `std.net.http` at version `1.0.0`.
#[derive(Debug)]
pub struct Registry {
    root: PathBuf,
}

impl Registry {
    /// Open the registry rooted at the default location (`$HOME/.hew/packages/`).
    ///
    /// Falls back to `%USERPROFILE%` (Windows) then the system temp directory.
    #[must_use]
    pub fn new() -> Self {
        Self {
            root: crate::paths::hew_home().join("packages"),
        }
    }

    /// Open a registry at an arbitrary `root` path.
    #[must_use]
    pub fn with_root(root: PathBuf) -> Self {
        Self { root }
    }

    /// Return the directory for a specific package name and version.
    ///
    /// Dotted package names are valid directory names. For example
    /// `ecosystem.db.postgres` at `1.0.0` returns
    /// `<root>/ecosystem.db.postgres/1.0.0`.
    #[must_use]
    pub fn package_dir(&self, name: &str, version: &str) -> PathBuf {
        self.package_dir_checked(name, version).unwrap_or_else(|_| {
            self.root
                .join(name)
                .join(format!(".{version}.invalid-generation-pointer"))
        })
    }

    pub(crate) fn package_dir_checked(
        &self,
        name: &str,
        version: &str,
    ) -> std::io::Result<PathBuf> {
        crate::atomic_fs::resolve_published_dir(&self.package_slot(name, version))
    }

    /// Return the stable logical slot used to publish a package generation.
    #[must_use]
    pub(crate) fn package_slot(&self, name: &str, version: &str) -> PathBuf {
        self.root.join(name).join(version)
    }

    /// Return `true` if `name@version` is present in the registry.
    #[must_use]
    pub fn is_installed(&self, name: &str, version: &str) -> bool {
        self.package_dir_checked(name, version)
            .is_ok_and(|path| is_package_dir(&path))
    }

    pub(crate) fn verified_online_cache_entry(
        &self,
        name: &str,
        version: &str,
        registry_checksum: &str,
    ) -> Result<Option<VerifiedCacheEntry>, String> {
        let package_dir = self
            .package_dir_checked(name, version)
            .map_err(|error| format!("cannot resolve cache for {name}@{version}: {error}"))?;
        if !is_package_dir(&package_dir) {
            return Ok(None);
        }
        let package_dir = package_dir
            .canonicalize()
            .map_err(|error| format!("cannot pin cache for {name}@{version}: {error}"))?;

        let metadata_text = match std::fs::read_to_string(package_dir.join(CACHE_METADATA_FILE)) {
            Ok(text) => text,
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => return Ok(None),
            Err(error) => {
                return Err(format!(
                    "cannot read cache metadata for {name}@{version}: {error}"
                ));
            }
        };
        let metadata: CacheMetadata = match toml::from_str(&metadata_text) {
            Ok(metadata) => metadata,
            Err(_) => return Ok(None),
        };
        if metadata.name != name
            || metadata.version != version
            || metadata.registry_checksum != registry_checksum
        {
            return Ok(None);
        }

        let archive = match std::fs::read(package_dir.join(CACHE_ARCHIVE_FILE)) {
            Ok(archive) => archive,
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => return Ok(None),
            Err(error) => {
                return Err(format!(
                    "cannot read cached archive for {name}@{version}: {error}"
                ));
            }
        };
        if crate::tarball::checksum_bytes(&archive) != registry_checksum {
            return Ok(None);
        }
        let Ok(archive_tree_checksum) = crate::tarball::unpacked_tree_checksum(&archive) else {
            return Ok(None);
        };
        if archive_tree_checksum != metadata.tree_checksum {
            return Ok(None);
        }

        let Ok(manifest) = crate::manifest::parse_manifest(&package_dir.join("hew.toml")) else {
            return Ok(None);
        };
        if manifest.package.name != name || manifest.package.version != version {
            return Ok(None);
        }

        let actual = crate::checksum::compute_dir_checksum(&package_dir)
            .map_err(|error| format!("cannot verify cache for {name}@{version}: {error}"))?;
        if actual == metadata.tree_checksum {
            Ok(Some(VerifiedCacheEntry {
                path: package_dir,
                tree_checksum: actual,
            }))
        } else {
            Ok(None)
        }
    }

    pub(crate) fn write_cache_metadata(
        package_dir: &Path,
        name: &str,
        version: &str,
        registry_checksum: &str,
        archive: &[u8],
    ) -> Result<(), String> {
        let manifest = crate::manifest::parse_manifest(&package_dir.join("hew.toml"))
            .map_err(|error| format!("cannot validate unpacked {name}@{version}: {error}"))?;
        if manifest.package.name != name || manifest.package.version != version {
            return Err(format!(
                "unpacked package identity mismatch for {name}@{version}: manifest declares {}@{}",
                manifest.package.name, manifest.package.version
            ));
        }

        if crate::tarball::checksum_bytes(archive) != registry_checksum {
            return Err(format!(
                "cached archive checksum changed before publication for {name}@{version}"
            ));
        }
        let tree_checksum = crate::tarball::unpacked_tree_checksum(archive)
            .map_err(|error| format!("cannot verify unpacked {name}@{version}: {error}"))?;
        let actual_tree_checksum = crate::checksum::compute_dir_checksum(package_dir)
            .map_err(|error| format!("cannot checksum unpacked {name}@{version}: {error}"))?;
        if actual_tree_checksum != tree_checksum {
            return Err(format!(
                "unpacked tree checksum mismatch for {name}@{version}"
            ));
        }
        let metadata = CacheMetadata {
            name: name.to_string(),
            version: version.to_string(),
            registry_checksum: registry_checksum.to_string(),
            tree_checksum,
        };
        let content = toml::to_string(&metadata)
            .map_err(|error| format!("cannot serialize cache metadata: {error}"))?;
        crate::atomic_fs::write_atomic(&package_dir.join(CACHE_ARCHIVE_FILE), archive, 0o644)
            .map_err(|error| {
                format!("cannot retain verified archive for {name}@{version}: {error}")
            })?;
        crate::atomic_fs::write_atomic(
            &package_dir.join(CACHE_METADATA_FILE),
            content.as_bytes(),
            0o644,
        )
        .map_err(|error| format!("cannot write cache metadata for {name}@{version}: {error}"))
    }

    /// Return the root path of this registry.
    #[must_use]
    pub fn root(&self) -> &Path {
        &self.root
    }

    /// List all packages installed in the registry.
    ///
    /// A directory is considered an installed package version when it contains
    /// a `hew.toml` file.  The path relative to the registry root determines
    /// the name (all but the last segment joined with dots) and version (the
    /// last segment).
    #[must_use]
    pub fn list_packages(&self) -> Vec<InstalledPackage> {
        let mut packages = Vec::new();
        collect_packages(&self.root, &self.root, &mut packages);
        packages
    }
}

impl Default for Registry {
    fn default() -> Self {
        Self::new()
    }
}

/// Recursively walk `dir`, collecting entries whose directories contain a
/// `hew.toml` into `packages`.  `root` is the registry root used to compute
/// relative paths.
fn collect_packages(
    root: &std::path::Path,
    dir: &std::path::Path,
    packages: &mut Vec<InstalledPackage>,
) {
    let Ok(entries) = std::fs::read_dir(dir) else {
        return;
    };

    let entries = entries.flatten().collect::<Vec<_>>();
    let mut published_versions = std::collections::BTreeSet::new();
    let mut subdirs = Vec::new();

    for entry in &entries {
        let name = entry.file_name();
        let Some(name) = name.to_str() else {
            continue;
        };
        let Some(version) = name
            .strip_prefix('.')
            .and_then(|name| name.strip_suffix(".current"))
        else {
            continue;
        };
        // Once a generation pointer exists it is authoritative. A corrupt or
        // missing generation must not revive a stale legacy directory.
        published_versions.insert(version.to_string());
        let logical_slot = dir.join(version);
        let Ok(active) = crate::atomic_fs::resolve_published_dir(&logical_slot) else {
            continue;
        };
        if !is_package_dir(&active) {
            continue;
        }
        let Ok(rel) = dir.strip_prefix(root) else {
            continue;
        };
        let name_parts = rel
            .components()
            .filter_map(|component| component.as_os_str().to_str())
            .collect::<Vec<_>>();
        if name_parts.is_empty() {
            continue;
        }
        packages.push(InstalledPackage {
            name: name_parts.join("."),
            version: version.to_string(),
            path: active,
        });
    }

    for entry in entries {
        let path = entry.path();
        let name = entry.file_name();
        let name = name.to_string_lossy();
        if path.is_dir() && !name.starts_with('.') && !published_versions.contains(name.as_ref()) {
            subdirs.push(path);
        }
    }

    if is_package_dir(dir) {
        if let Ok(rel) = dir.strip_prefix(root) {
            let parts: Vec<&str> = rel
                .components()
                .filter_map(|c| c.as_os_str().to_str())
                .collect();
            if let Some((version, name_parts)) = parts.split_last() {
                if !name_parts.is_empty() {
                    packages.push(InstalledPackage {
                        name: name_parts.join("."),
                        version: (*version).to_string(),
                        path: dir.to_path_buf(),
                    });
                }
            }
        }
    }

    for subdir in subdirs {
        collect_packages(root, &subdir, packages);
    }
}

fn is_package_dir(path: &Path) -> bool {
    path.join("hew.toml").is_file()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn package_dir_simple_name() {
        let dir = tempfile::tempdir().unwrap();
        let reg = Registry::with_root(dir.path().to_path_buf());
        let p = reg.package_dir("myapp", "0.1.0");
        assert_eq!(p, dir.path().join("myapp").join("0.1.0"));
    }

    #[test]
    fn package_dir_namespaced() {
        let dir = tempfile::tempdir().unwrap();
        let reg = Registry::with_root(dir.path().to_path_buf());
        let p = reg.package_dir("ecosystem.db.postgres", "1.0.0");
        assert_eq!(p, dir.path().join("ecosystem.db.postgres").join("1.0.0"));
    }

    #[test]
    fn list_packages_empty_registry() {
        let dir = tempfile::tempdir().unwrap();
        let reg = Registry::with_root(dir.path().to_path_buf());
        assert!(reg.list_packages().is_empty());
    }

    #[test]
    fn list_packages_finds_installed() {
        let dir = tempfile::tempdir().unwrap();
        let reg = Registry::with_root(dir.path().to_path_buf());

        let pkg_dir = reg.package_dir("std.net.http", "1.0.0");
        std::fs::create_dir_all(&pkg_dir).unwrap();
        std::fs::write(
            pkg_dir.join("hew.toml"),
            "[package]\nname = \"std.net.http\"\nversion = \"1.0.0\"\n",
        )
        .unwrap();

        let pkgs = reg.list_packages();
        assert_eq!(pkgs.len(), 1);
        assert_eq!(pkgs[0].name, "std.net.http");
        assert_eq!(pkgs[0].version, "1.0.0");
    }

    #[test]
    fn list_packages_multiple() {
        let dir = tempfile::tempdir().unwrap();
        let reg = Registry::with_root(dir.path().to_path_buf());

        for (name, ver) in [
            ("std.net.http", "1.0.0"),
            ("ecosystem.db.postgres", "2.1.0"),
        ] {
            let pkg_dir = reg.package_dir(name, ver);
            std::fs::create_dir_all(&pkg_dir).unwrap();
            std::fs::write(
                pkg_dir.join("hew.toml"),
                format!("[package]\nname = \"{name}\"\nversion = \"{ver}\"\n"),
            )
            .unwrap();
        }

        let mut pkgs = reg.list_packages();
        pkgs.sort_by(|a, b| a.name.cmp(&b.name));
        assert_eq!(pkgs.len(), 2);
        assert_eq!(pkgs[0].name, "ecosystem.db.postgres");
        assert_eq!(pkgs[1].name, "std.net.http");
    }

    #[test]
    fn list_packages_reports_only_active_published_generation() {
        let dir = tempfile::tempdir().unwrap();
        let reg = Registry::with_root(dir.path().to_path_buf());
        let slot = reg.package_slot("foo", "1.0.0");
        std::fs::create_dir_all(&slot).unwrap();
        std::fs::write(
            slot.join("hew.toml"),
            "[package]\nname = \"foo\"\nversion = \"1.0.0\"\n",
        )
        .unwrap();

        let staged = crate::atomic_fs::StagedDir::new(&slot).unwrap();
        std::fs::write(
            staged.path().join("hew.toml"),
            "[package]\nname = \"foo\"\nversion = \"1.0.0\"\n",
        )
        .unwrap();
        std::fs::write(staged.path().join("generation"), "new").unwrap();
        staged.publish(&slot).unwrap();

        assert_eq!(
            std::fs::read_to_string(reg.package_dir("foo", "1.0.0").join("generation")).unwrap(),
            "new"
        );
        let packages = reg.list_packages();
        assert_eq!(packages.len(), 1);
        assert_eq!(packages[0].name, "foo");
        assert_eq!(packages[0].version, "1.0.0");
        assert_eq!(packages[0].path, reg.package_dir("foo", "1.0.0"));
    }

    #[test]
    fn corrupt_pointer_does_not_revive_valid_legacy_package() {
        let dir = tempfile::tempdir().unwrap();
        let reg = Registry::with_root(dir.path().to_path_buf());
        let legacy = reg.package_slot("foo", "1.0.0");
        std::fs::create_dir_all(&legacy).unwrap();
        std::fs::write(
            legacy.join("hew.toml"),
            "[package]\nname = \"foo\"\nversion = \"1.0.0\"\n",
        )
        .unwrap();
        std::fs::write(legacy.with_file_name(".1.0.0.current"), [0xff, 0xfe]).unwrap();

        assert!(!reg.is_installed("foo", "1.0.0"));
        assert_ne!(reg.package_dir("foo", "1.0.0"), legacy);
        assert!(reg.list_packages().is_empty());
    }

    #[test]
    fn is_installed_returns_false_when_absent() {
        let dir = tempfile::tempdir().unwrap();
        let reg = Registry::with_root(dir.path().to_path_buf());
        assert!(!reg.is_installed("std.net.http", "1.0.0"));
    }

    #[test]
    fn is_installed_returns_true_when_present() {
        let dir = tempfile::tempdir().unwrap();
        let reg = Registry::with_root(dir.path().to_path_buf());
        let pkg_dir = reg.package_dir("std.net.http", "1.0.0");
        std::fs::create_dir_all(&pkg_dir).unwrap();
        std::fs::write(
            pkg_dir.join("hew.toml"),
            "[package]\nname = \"std.net.http\"\nversion = \"1.0.0\"\n",
        )
        .unwrap();
        assert!(reg.is_installed("std.net.http", "1.0.0"));
    }

    #[test]
    fn is_installed_returns_false_for_incomplete_directory() {
        let dir = tempfile::tempdir().unwrap();
        let reg = Registry::with_root(dir.path().to_path_buf());
        let pkg_dir = reg.package_dir("std.net.http", "1.0.0");
        std::fs::create_dir_all(&pkg_dir).unwrap();
        std::fs::write(pkg_dir.join("partial.marker"), "incomplete").unwrap();
        assert!(!reg.is_installed("std.net.http", "1.0.0"));
        assert!(reg.list_packages().is_empty());
    }

    #[test]
    fn online_cache_verification_detects_tampering() {
        let dir = tempfile::tempdir().unwrap();
        let reg = Registry::with_root(dir.path().to_path_buf());
        let pkg_dir = reg.package_dir("foo", "1.0.0");
        std::fs::create_dir_all(&pkg_dir).unwrap();
        std::fs::write(
            pkg_dir.join("hew.toml"),
            "[package]\nname = \"foo\"\nversion = \"1.0.0\"\n",
        )
        .unwrap();
        std::fs::write(pkg_dir.join("foo.hew"), "pub fn value() -> i64 { 1 }\n").unwrap();
        let archive = crate::tarball::pack(&pkg_dir, &[], &[]).unwrap();
        Registry::write_cache_metadata(&pkg_dir, "foo", "1.0.0", &archive.checksum, &archive.data)
            .unwrap();
        assert!(reg
            .verified_online_cache_entry("foo", "1.0.0", &archive.checksum)
            .unwrap()
            .is_some());

        std::fs::write(pkg_dir.join("foo.hew"), "pub fn value() -> i64 { 2 }\n").unwrap();
        assert!(reg
            .verified_online_cache_entry("foo", "1.0.0", &archive.checksum)
            .unwrap()
            .is_none());
    }
}
