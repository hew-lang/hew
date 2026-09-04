//! Package resolution for the file-less CLI forms.
//!
//! `hew build`, `hew run`, and `hew check` accept a package instead of a source
//! file: with no argument, or with a directory argument. All three resolve
//! through [`resolve_package`] so the manifest is the single authority on where
//! a package starts, what it compiles, and what the produced binary is called.

use std::fmt;
use std::path::{Component, Path, PathBuf};

use crate::manifest::{self, HewManifest, ManifestError};

/// The manifest file name that marks a package root.
pub const MANIFEST_FILE: &str = "hew.toml";

/// A package located on disk, with its manifest parsed.
#[derive(Debug)]
pub struct ResolvedPackage {
    /// Directory holding `hew.toml`; all package-relative paths start here.
    pub root: PathBuf,
    /// Path to the package's `hew.toml`.
    pub manifest_path: PathBuf,
    /// The parsed manifest.
    pub manifest: HewManifest,
}

impl ResolvedPackage {
    /// Absolute-or-relative path to the entry-point source file named by
    /// `[package] main` (default `main.hew`).
    #[must_use]
    pub fn entry_path(&self) -> PathBuf {
        self.root.join(self.manifest.package.main_source())
    }

    /// Path of the build artefact `hew build` writes by default, cargo-style:
    /// `<root>/target/<profile>/<name><suffix>`. `profile` is `"debug"` or
    /// `"release"`; `suffix` is the target's executable suffix (`""`, `.exe`,
    /// `.wasm`) for a linked binary, or its object suffix (`.o`, `.obj`) for
    /// `--emit-obj`.
    #[must_use]
    pub fn default_binary_path(&self, profile: &str, suffix: &str) -> PathBuf {
        self.root
            .join("target")
            .join(profile)
            .join(format!("{}{suffix}", self.manifest.package.binary_name()))
    }

    /// True when this package declares a `[native]` FFI library that must be
    /// built before its Hew sources compile.
    #[must_use]
    pub fn has_native(&self) -> bool {
        self.manifest.native.is_some()
    }
}

/// Why a package could not be resolved. Every variant renders a diagnostic
/// naming the path involved — a directory argument never reaches the compiler
/// as a source path, so no raw `Is a directory` OS error can escape.
#[derive(Debug)]
pub enum ResolveError {
    /// The process current directory could not be read to resolve a relative path.
    CurrentDirectory(std::io::Error),
    /// The starting directory does not exist or is not a directory.
    NotADirectory(PathBuf),
    /// No `hew.toml` in the starting directory or any ancestor.
    NoManifest(PathBuf),
    /// `hew.toml` was found but could not be read or parsed.
    Manifest {
        /// The manifest that failed to load.
        path: PathBuf,
        /// The underlying parse/read failure.
        source: ManifestError,
    },
    /// The manifest resolved, but its entry-point source file is missing.
    MissingEntry {
        /// Entry path that does not exist.
        entry: PathBuf,
        /// The `[package] main` value used (declared or defaulted).
        declared: String,
        /// Whether `[package] main` was written explicitly.
        explicit: bool,
    },
}

impl fmt::Display for ResolveError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::CurrentDirectory(source) => {
                write!(f, "cannot read the current directory: {source}")
            }
            Self::NotADirectory(path) => {
                write!(f, "{} is not a directory", path.display())
            }
            Self::NoManifest(start) => write!(
                f,
                "no {MANIFEST_FILE} in {} or any parent directory\n  \
                 hint: pass a .hew file to compile, or run `hew init` to create a package",
                start.display()
            ),
            Self::Manifest { path, source } => {
                write!(f, "cannot load {}: {source}", path.display())
            }
            Self::MissingEntry {
                entry,
                declared,
                explicit: true,
            } => write!(
                f,
                "entry point {} does not exist\n  \
                 hint: `[package] main = \"{declared}\"` names a file that is not there",
                entry.display()
            ),
            Self::MissingEntry {
                entry,
                declared,
                explicit: false,
            } => write!(
                f,
                "entry point {} does not exist\n  \
                 hint: packages compile `{declared}` by default; set `[package] main` to name a \
                 different entry point",
                entry.display()
            ),
        }
    }
}

impl std::error::Error for ResolveError {}

/// Return the nearest directory at or above `start` that holds a `hew.toml`.
#[must_use]
pub fn find_package_root(start: &Path) -> Option<PathBuf> {
    start
        .ancestors()
        .find(|dir| dir.join(MANIFEST_FILE).is_file())
        .map(Path::to_path_buf)
}

/// Resolve the package rooted at or above `start`, requiring its entry-point
/// source file to exist.
///
/// `start` is the current directory for the no-argument forms, or the directory
/// the user named. Relative paths are made absolute against the current
/// directory before the search walks up, so running inside a package
/// subdirectory builds the enclosing package.
///
/// # Errors
///
/// Returns [`ResolveError`] when `start` is not a directory, no manifest is
/// found, the manifest fails to parse, or the entry-point file is absent.
pub fn resolve_package(start: &Path) -> Result<ResolvedPackage, ResolveError> {
    let start = absolute_normalized_path(start)?;
    if !start.is_dir() {
        return Err(ResolveError::NotADirectory(start));
    }
    let root = find_package_root(&start).ok_or_else(|| ResolveError::NoManifest(start.clone()))?;
    let manifest_path = root.join(MANIFEST_FILE);
    let manifest =
        manifest::parse_manifest(&manifest_path).map_err(|source| ResolveError::Manifest {
            path: manifest_path.clone(),
            source,
        })?;
    let resolved = ResolvedPackage {
        root,
        manifest_path,
        manifest,
    };
    let entry = resolved.entry_path();
    if !entry.is_file() {
        return Err(ResolveError::MissingEntry {
            entry,
            declared: resolved.manifest.package.main_source().to_string(),
            explicit: resolved.manifest.package.main.is_some(),
        });
    }
    Ok(resolved)
}

fn absolute_normalized_path(path: &Path) -> Result<PathBuf, ResolveError> {
    let absolute = if path.is_absolute() {
        path.to_path_buf()
    } else {
        std::env::current_dir()
            .map_err(ResolveError::CurrentDirectory)?
            .join(path)
    };

    let mut normalized = PathBuf::new();
    for component in absolute.components() {
        match component {
            Component::CurDir => {}
            Component::ParentDir => {
                normalized.pop();
            }
            Component::Prefix(_) | Component::RootDir | Component::Normal(_) => {
                normalized.push(component.as_os_str());
            }
        }
    }
    Ok(normalized)
}

/// Build the package's own `[native]` FFI crate and return the archive the
/// linker needs. `None` when the package declares no `[native]` section.
///
/// This is a prerequisite step of every package-mode build: sources that call
/// `extern` functions cannot link until the archive exists.
///
/// # Errors
///
/// Returns the underlying cargo build failure message.
pub fn build_native_lib(root: &Path) -> Result<Option<PathBuf>, String> {
    let expected = crate::native::embedded_rustc_identity();
    let Some(artifact) = crate::native::build_native(root, &expected)? else {
        return Ok(None);
    };
    Ok(Some(artifact.path))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn write_package(dir: &Path, manifest_body: &str, entry: Option<&str>) {
        std::fs::write(dir.join(MANIFEST_FILE), manifest_body).expect("write manifest");
        if let Some(entry) = entry {
            let path = dir.join(entry);
            if let Some(parent) = path.parent() {
                std::fs::create_dir_all(parent).expect("create entry dir");
            }
            std::fs::write(path, "fn main() {}\n").expect("write entry");
        }
    }

    const BASIC: &str = "[package]\nname = \"myproj\"\nversion = \"0.1.0\"\nedition = \"2026\"\n";

    #[test]
    fn resolves_default_entry_from_package_root() {
        let dir = tempfile::tempdir().expect("tempdir");
        write_package(dir.path(), BASIC, Some("main.hew"));

        let pkg = resolve_package(dir.path()).expect("resolve");

        assert_eq!(pkg.entry_path(), dir.path().join("main.hew"));
        assert_eq!(
            pkg.default_binary_path("debug", ""),
            dir.path().join("target/debug/myproj")
        );
    }

    #[test]
    fn default_binary_path_separates_debug_and_release_profiles() {
        let dir = tempfile::tempdir().expect("tempdir");
        write_package(dir.path(), BASIC, Some("main.hew"));

        let pkg = resolve_package(dir.path()).expect("resolve");

        assert_eq!(
            pkg.default_binary_path("release", ""),
            dir.path().join("target/release/myproj")
        );
        assert_ne!(
            pkg.default_binary_path("debug", ""),
            pkg.default_binary_path("release", "")
        );
    }

    #[test]
    fn resolves_from_subdirectory_by_walking_up() {
        let dir = tempfile::tempdir().expect("tempdir");
        write_package(dir.path(), BASIC, Some("main.hew"));
        let nested = dir.path().join("util").join("deep");
        std::fs::create_dir_all(&nested).expect("create nested");

        let pkg = resolve_package(&nested).expect("resolve");

        assert_eq!(pkg.root, dir.path());
    }

    #[test]
    fn explicit_main_field_overrides_the_default_entry() {
        let dir = tempfile::tempdir().expect("tempdir");
        write_package(
            dir.path(),
            &format!("{BASIC}main = \"src/app.hew\"\n"),
            Some("src/app.hew"),
        );

        let pkg = resolve_package(dir.path()).expect("resolve");

        assert_eq!(pkg.entry_path(), dir.path().join("src/app.hew"));
    }

    #[test]
    fn dotted_package_name_binary_is_the_last_segment() {
        let dir = tempfile::tempdir().expect("tempdir");
        write_package(
            dir.path(),
            "[package]\nname = \"hew.db.sqlite\"\nversion = \"0.1.0\"\nedition = \"2026\"\n",
            Some("main.hew"),
        );

        let pkg = resolve_package(dir.path()).expect("resolve");

        assert_eq!(
            pkg.default_binary_path("debug", ".exe"),
            dir.path().join("target/debug/sqlite.exe")
        );
    }

    #[test]
    fn missing_manifest_names_the_search_origin() {
        let dir = tempfile::tempdir().expect("tempdir");

        let error = resolve_package(dir.path()).expect_err("no manifest");

        assert!(
            matches!(error, ResolveError::NoManifest(_)),
            "expected NoManifest, got {error:?}"
        );
        assert!(error.to_string().contains("hew init"), "{error}");
    }

    #[test]
    fn missing_default_entry_reports_the_convention() {
        let dir = tempfile::tempdir().expect("tempdir");
        write_package(dir.path(), BASIC, None);

        let error = resolve_package(dir.path()).expect_err("no entry");

        let rendered = error.to_string();
        assert!(rendered.contains("main.hew"), "{rendered}");
        assert!(rendered.contains("by default"), "{rendered}");
    }

    #[test]
    fn missing_declared_entry_points_at_the_manifest_field() {
        let dir = tempfile::tempdir().expect("tempdir");
        write_package(dir.path(), &format!("{BASIC}main = \"app.hew\"\n"), None);

        let error = resolve_package(dir.path()).expect_err("no entry");

        let rendered = error.to_string();
        assert!(rendered.contains("[package] main"), "{rendered}");
        assert!(rendered.contains("app.hew"), "{rendered}");
    }

    #[test]
    fn malformed_manifest_surfaces_as_a_manifest_error() {
        let dir = tempfile::tempdir().expect("tempdir");
        write_package(
            dir.path(),
            "[package]\nname = \"p\"\nversion = \"0.1.0\"\nedition = \"1999\"\n",
            Some("main.hew"),
        );

        let error = resolve_package(dir.path()).expect_err("bad edition");

        assert!(
            matches!(error, ResolveError::Manifest { .. }),
            "expected Manifest, got {error:?}"
        );
    }

    #[test]
    fn a_file_path_is_not_a_package_root() {
        let dir = tempfile::tempdir().expect("tempdir");
        write_package(dir.path(), BASIC, Some("main.hew"));

        let error = resolve_package(&dir.path().join("main.hew")).expect_err("file, not dir");

        assert!(
            matches!(error, ResolveError::NotADirectory(_)),
            "expected NotADirectory, got {error:?}"
        );
    }
}
