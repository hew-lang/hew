//! Local package registry (`~/.adze/packages/`).

use std::path::{Path, PathBuf};

/// An installed package discovered in the global registry.
#[derive(Debug)]
pub struct InstalledPackage {
    /// Package name in `::` notation, e.g. `std::net::http`.
    pub name: String,
    /// Installed version string, e.g. `1.0.0`.
    pub version: String,
    /// Absolute path to the package version directory.
    pub path: PathBuf,
}

/// The global Adze package registry (`~/.adze/packages/`).
///
/// Package layout on disk:
/// ```text
/// ~/.adze/packages/{seg1}/{seg2}/{…}/{version}/hew.toml
/// ```
/// e.g. `~/.adze/packages/std/net/http/1.0.0/hew.toml`
/// corresponds to the package `std::net::http` at version `1.0.0`.
#[derive(Debug)]
pub struct Registry {
    root: PathBuf,
}

impl Registry {
    /// Open the registry rooted at the default location (`$HOME/.adze/packages/`).
    ///
    /// Falls back to `/tmp/.adze/packages` when `$HOME` is not set.
    #[must_use]
    pub fn new() -> Self {
        let home = std::env::var("HOME").unwrap_or_else(|_| "/tmp".to_string());
        Self {
            root: PathBuf::from(home).join(".adze").join("packages"),
        }
    }

    /// Open a registry at an arbitrary `root` path.
    #[must_use]
    pub fn with_root(root: PathBuf) -> Self {
        Self { root }
    }

    /// Return the directory for a specific package name and version.
    ///
    /// Package names use `::` as namespace separator; each segment becomes a
    /// directory component.  For example `ecosystem::db::postgres` at `1.0.0`
    /// returns `<root>/ecosystem/db/postgres/1.0.0`.
    #[must_use]
    pub fn package_dir(&self, name: &str, version: &str) -> PathBuf {
        let mut path = self.root.clone();
        for part in name.split("::") {
            path = path.join(part);
        }
        path.join(version)
    }

    /// Return `true` if `name@version` is present in the registry.
    #[must_use]
    pub fn is_installed(&self, name: &str, version: &str) -> bool {
        self.package_dir(name, version).is_dir()
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
    /// the name (all but the last segment joined with `::`) and version (the
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

    let mut has_toml = false;
    let mut subdirs = Vec::new();

    for entry in entries.flatten() {
        let path = entry.path();
        if path.is_dir() {
            subdirs.push(path);
        } else if entry.file_name() == "hew.toml" {
            has_toml = true;
        }
    }

    if has_toml {
        if let Ok(rel) = dir.strip_prefix(root) {
            let parts: Vec<&str> = rel
                .components()
                .filter_map(|c| c.as_os_str().to_str())
                .collect();
            if let Some((version, name_parts)) = parts.split_last() {
                if !name_parts.is_empty() {
                    packages.push(InstalledPackage {
                        name: name_parts.join("::"),
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
        let p = reg.package_dir("ecosystem::db::postgres", "1.0.0");
        assert_eq!(
            p,
            dir.path()
                .join("ecosystem")
                .join("db")
                .join("postgres")
                .join("1.0.0")
        );
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

        let pkg_dir = reg.package_dir("std::net::http", "1.0.0");
        std::fs::create_dir_all(&pkg_dir).unwrap();
        std::fs::write(
            pkg_dir.join("hew.toml"),
            "[package]\nname = \"std::net::http\"\nversion = \"1.0.0\"\n",
        )
        .unwrap();

        let pkgs = reg.list_packages();
        assert_eq!(pkgs.len(), 1);
        assert_eq!(pkgs[0].name, "std::net::http");
        assert_eq!(pkgs[0].version, "1.0.0");
    }

    #[test]
    fn list_packages_multiple() {
        let dir = tempfile::tempdir().unwrap();
        let reg = Registry::with_root(dir.path().to_path_buf());

        for (name, ver) in [
            ("std::net::http", "1.0.0"),
            ("ecosystem::db::postgres", "2.1.0"),
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
        assert_eq!(pkgs[0].name, "ecosystem::db::postgres");
        assert_eq!(pkgs[1].name, "std::net::http");
    }

    #[test]
    fn is_installed_returns_false_when_absent() {
        let dir = tempfile::tempdir().unwrap();
        let reg = Registry::with_root(dir.path().to_path_buf());
        assert!(!reg.is_installed("std::net::http", "1.0.0"));
    }

    #[test]
    fn is_installed_returns_true_when_present() {
        let dir = tempfile::tempdir().unwrap();
        let reg = Registry::with_root(dir.path().to_path_buf());
        let pkg_dir = reg.package_dir("std::net::http", "1.0.0");
        std::fs::create_dir_all(&pkg_dir).unwrap();
        assert!(reg.is_installed("std::net::http", "1.0.0"));
    }
}
