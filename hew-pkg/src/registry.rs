//! Local package registry (`~/.hew/packages/`).

use std::path::{Path, PathBuf};

use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};

use crate::package_fs::{CACHE_ARCHIVE_FILE, CACHE_METADATA_FILE};

#[derive(Debug, Deserialize, Serialize)]
struct CacheMetadata {
    name: String,
    version: String,
    registry: String,
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
pub(crate) struct PinnedInstalledPackage {
    pub(crate) name: String,
    pub(crate) version: String,
    pub(crate) path: PathBuf,
    pub(crate) pin: crate::atomic_fs::PinnedDir,
}

impl PinnedInstalledPackage {
    fn into_installed(self) -> InstalledPackage {
        InstalledPackage {
            name: self.name,
            version: self.version,
            path: self.path,
        }
    }
}

#[derive(Debug)]
pub(crate) struct VerifiedCacheEntry {
    pub(crate) pin: crate::atomic_fs::PinnedDir,
    pub(crate) tree_checksum: String,
}

/// The global Hew package registry (`~/.hew/packages/`).
///
/// Package layout on disk:
/// ```text
/// ~/.hew/packages/.registries/{sha256(registry-id)}/{package}/{version}/hew.toml
/// ```
/// The legacy unnamespaced layout is consulted only by explicit offline
/// default-registry resolution and is migrated before a new lockfile is used.
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

    /// Return the active package generation for a canonical registry identity.
    #[must_use]
    pub fn package_dir_for(&self, registry: &str, name: &str, version: &str) -> PathBuf {
        self.package_dir_for_checked(registry, name, version)
            .unwrap_or_else(|_| {
                self.source_root(registry)
                    .join(name)
                    .join(format!(".{version}.invalid-generation-pointer"))
            })
    }

    pub(crate) fn package_dir_for_checked(
        &self,
        registry: &str,
        name: &str,
        version: &str,
    ) -> std::io::Result<PathBuf> {
        crate::atomic_fs::resolve_published_dir(&self.package_slot_for(registry, name, version))
    }

    pub(crate) fn pin_package_dir_for_if_present(
        &self,
        registry: &str,
        name: &str,
        version: &str,
    ) -> std::io::Result<Option<crate::atomic_fs::PinnedDir>> {
        crate::atomic_fs::pin_published_dir_if_present(
            &self.package_slot_for(registry, name, version),
        )
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

    /// Return the source-namespaced logical package slot.
    #[must_use]
    pub(crate) fn package_slot_for(&self, registry: &str, name: &str, version: &str) -> PathBuf {
        self.source_root(registry).join(name).join(version)
    }

    /// Return the cache root dedicated to one canonical registry identity.
    #[must_use]
    pub(crate) fn source_root(&self, registry: &str) -> PathBuf {
        use std::fmt::Write as _;

        let digest = Sha256::digest(registry.as_bytes());
        let mut namespace = String::with_capacity(digest.len() * 2);
        for byte in digest {
            write!(&mut namespace, "{byte:02x}").expect("writing to a String cannot fail");
        }
        self.root.join(".registries").join(namespace)
    }

    /// Return `true` if `name@version` is present in the registry.
    #[must_use]
    pub fn is_installed(&self, name: &str, version: &str) -> bool {
        self.package_dir_checked(name, version)
            .is_ok_and(|path| is_package_dir(&path))
    }

    pub(crate) fn verified_online_cache_entry(
        &self,
        registry: &str,
        name: &str,
        version: &str,
        registry_checksum: &str,
    ) -> Result<Option<VerifiedCacheEntry>, String> {
        let pin = match self.pin_package_dir_for_if_present(registry, name, version) {
            Ok(pin) => pin,
            Err(error)
                if matches!(
                    error.kind(),
                    std::io::ErrorKind::NotFound | std::io::ErrorKind::InvalidData
                ) =>
            {
                return Ok(None);
            }
            Err(error) => {
                return Err(format!(
                    "cannot resolve cache for {name}@{version}: {error}"
                ));
            }
        };
        let Some(pin) = pin else {
            return Ok(None);
        };
        let package_dir = pin.path();
        if !is_package_dir(package_dir) {
            return Ok(None);
        }
        let pin = pin
            .canonicalize()
            .map_err(|error| format!("cannot pin cache for {name}@{version}: {error}"))?;
        let package_dir = pin.path();

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
            || metadata.registry != registry
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
        if crate::resolver::validate_registry_manifest(name, &manifest).is_err() {
            return Ok(None);
        }

        let actual = crate::checksum::compute_dir_checksum(package_dir)
            .map_err(|error| format!("cannot verify cache for {name}@{version}: {error}"))?;
        if actual == metadata.tree_checksum {
            Ok(Some(VerifiedCacheEntry {
                pin,
                tree_checksum: actual,
            }))
        } else {
            Ok(None)
        }
    }

    pub(crate) fn write_cache_metadata(
        package_dir: &Path,
        registry: &str,
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
            registry: registry.to_string(),
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
        let _ = collect_packages(&self.root, &self.root, &mut packages);
        packages
            .into_iter()
            .map(PinnedInstalledPackage::into_installed)
            .collect()
    }

    /// List packages cached for one canonical registry identity.
    ///
    /// `include_legacy` is reserved for explicit offline compatibility with the
    /// default registry. Online, named, and locked callers must pass `false`.
    #[must_use]
    pub fn list_packages_for(&self, registry: &str, include_legacy: bool) -> Vec<InstalledPackage> {
        let source_root = self.source_root(registry);
        let mut packages = Vec::new();
        let _ = collect_packages(&source_root, &source_root, &mut packages);
        if include_legacy {
            let _ = collect_packages(&self.root, &self.root, &mut packages);
            packages.retain(|package| {
                package
                    .path
                    .strip_prefix(self.root.join(".registries"))
                    .is_err()
            });
        }
        packages
            .into_iter()
            .map(PinnedInstalledPackage::into_installed)
            .collect()
    }

    pub(crate) fn try_list_packages_for(
        &self,
        registry: &str,
        include_legacy: bool,
    ) -> std::io::Result<Vec<PinnedInstalledPackage>> {
        let source_root = self.source_root(registry);
        let mut packages = Vec::new();
        collect_packages(&source_root, &source_root, &mut packages)?;
        if include_legacy {
            collect_packages(&self.root, &self.root, &mut packages)?;
            packages.retain(|package| {
                package
                    .path
                    .strip_prefix(self.root.join(".registries"))
                    .is_err()
            });
        }
        Ok(packages)
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
    packages: &mut Vec<PinnedInstalledPackage>,
) -> std::io::Result<()> {
    let entries = match std::fs::read_dir(dir) {
        Ok(entries) => entries,
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => return Ok(()),
        Err(error) => return Err(error),
    };

    let entries = entries.collect::<Result<Vec<_>, _>>()?;
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
        let pin = crate::atomic_fs::pin_published_dir(&logical_slot)?;
        let active = pin.path().to_path_buf();
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
        packages.push(PinnedInstalledPackage {
            name: name_parts.join("."),
            version: version.to_string(),
            path: active,
            pin,
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
                    packages.push(PinnedInstalledPackage {
                        name: name_parts.join("."),
                        version: (*version).to_string(),
                        path: dir.to_path_buf(),
                        pin: crate::atomic_fs::PinnedDir::legacy(dir.to_path_buf()),
                    });
                }
            }
        }
    }

    for subdir in subdirs {
        collect_packages(root, &subdir, packages)?;
    }
    Ok(())
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
    fn registry_identities_have_disjoint_cache_slots() {
        let dir = tempfile::tempdir().unwrap();
        let reg = Registry::with_root(dir.path().to_path_buf());
        let first = "https://registry-a.example/api/v1";
        let second = "https://registry-b.example/api/v1";
        assert_ne!(
            reg.package_slot_for(first, "foo", "1.0.0"),
            reg.package_slot_for(second, "foo", "1.0.0")
        );

        let package = reg.package_dir_for(first, "foo", "1.0.0");
        std::fs::create_dir_all(&package).unwrap();
        std::fs::write(
            package.join("hew.toml"),
            "[package]\nname = \"foo\"\nversion = \"1.0.0\"\n",
        )
        .unwrap();
        assert_eq!(reg.list_packages_for(first, false).len(), 1);
        assert!(reg.list_packages_for(second, false).is_empty());
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
        let registry_id = crate::config::default_registry_identity();
        let pkg_dir = reg.package_dir_for(&registry_id, "foo", "1.0.0");
        std::fs::create_dir_all(&pkg_dir).unwrap();
        std::fs::write(
            pkg_dir.join("hew.toml"),
            "[package]\nname = \"foo\"\nversion = \"1.0.0\"\n",
        )
        .unwrap();
        std::fs::write(pkg_dir.join("foo.hew"), "pub fn value() -> i64 { 1 }\n").unwrap();
        let archive = crate::tarball::pack(&pkg_dir, &[], &[]).unwrap();
        Registry::write_cache_metadata(
            &pkg_dir,
            &registry_id,
            "foo",
            "1.0.0",
            &archive.checksum,
            &archive.data,
        )
        .unwrap();
        assert!(reg
            .verified_online_cache_entry(&registry_id, "foo", "1.0.0", &archive.checksum)
            .unwrap()
            .is_some());

        std::fs::write(pkg_dir.join("foo.hew"), "pub fn value() -> i64 { 2 }\n").unwrap();
        assert!(reg
            .verified_online_cache_entry(&registry_id, "foo", "1.0.0", &archive.checksum)
            .unwrap()
            .is_none());
    }

    #[test]
    fn online_cache_treats_missing_or_invalid_generation_lease_as_untrusted() {
        let dir = tempfile::tempdir().unwrap();
        let reg = Registry::with_root(dir.path().to_path_buf());
        let registry_id = crate::config::default_registry_identity();
        let slot = reg.package_slot_for(&registry_id, "foo", "1.0.0");
        let staged = crate::atomic_fs::StagedDir::new(&slot).unwrap();
        std::fs::write(
            staged.path().join("hew.toml"),
            "[package]\nname = \"foo\"\nversion = \"1.0.0\"\n",
        )
        .unwrap();
        let generation = staged.publish(&slot).unwrap();
        let generation_name = generation.file_name().unwrap().to_string_lossy();
        let lease = generation.with_file_name(format!("{generation_name}.lease"));
        std::fs::remove_file(&lease).unwrap();

        assert!(reg
            .verified_online_cache_entry(&registry_id, "foo", "1.0.0", "sha256:unused")
            .unwrap()
            .is_none());
        assert!(
            generation.is_dir(),
            "uncertain published generation must be retained for repair"
        );
        assert!(!lease.exists());

        std::fs::create_dir(&lease).unwrap();
        assert!(reg
            .verified_online_cache_entry(&registry_id, "foo", "1.0.0", "sha256:unused")
            .unwrap()
            .is_none());
        assert!(
            generation.is_dir(),
            "invalid lease metadata must not consume the published generation"
        );
    }

    #[test]
    fn cache_metadata_cannot_confirm_a_different_registry() {
        let dir = tempfile::tempdir().unwrap();
        let reg = Registry::with_root(dir.path().to_path_buf());
        let first = "https://registry-a.example/api/v1";
        let second = "https://registry-b.example/api/v1";
        let first_dir = reg.package_dir_for(first, "foo", "1.0.0");
        std::fs::create_dir_all(&first_dir).unwrap();
        std::fs::write(
            first_dir.join("hew.toml"),
            "[package]\nname = \"foo\"\nversion = \"1.0.0\"\n",
        )
        .unwrap();
        std::fs::write(first_dir.join("foo.hew"), "pub fn value() -> i64 { 1 }\n").unwrap();
        let archive = crate::tarball::pack(&first_dir, &[], &[]).unwrap();
        Registry::write_cache_metadata(
            &first_dir,
            first,
            "foo",
            "1.0.0",
            &archive.checksum,
            &archive.data,
        )
        .unwrap();

        let second_dir = reg.package_dir_for(second, "foo", "1.0.0");
        std::fs::create_dir_all(&second_dir).unwrap();
        for file in [
            "hew.toml",
            "foo.hew",
            CACHE_ARCHIVE_FILE,
            CACHE_METADATA_FILE,
        ] {
            std::fs::copy(first_dir.join(file), second_dir.join(file)).unwrap();
        }
        assert!(reg
            .verified_online_cache_entry(second, "foo", "1.0.0", &archive.checksum)
            .unwrap()
            .is_none());
    }
}
