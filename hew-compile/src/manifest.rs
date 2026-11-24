//! Project manifest, lockfile, edition, and compiler-version loading.

use std::collections::BTreeMap;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use serde::{de::DeserializeOwned, Deserialize};

use crate::diagnostics::FrontendFailure;
use crate::frontend_options::FrontendOptions;
use crate::source_set::{read_source, SourceSet, EMPTY_DOCUMENTS};

#[derive(Debug)]
pub(crate) struct LockedPackageCheck {
    pub(crate) package_dir: PathBuf,
    pub(crate) name: String,
    pub(crate) version: String,
}

pub(crate) struct ProjectContext {
    pub(crate) source: Arc<str>,
    pub(crate) project_dir: PathBuf,
    pub(crate) manifest_deps: Option<Vec<String>>,
    pub(crate) package_name: Option<String>,
    pub(crate) locked_versions: Option<Vec<(String, String)>>,
}

pub(crate) fn load_project_context(
    input: &str,
    options: Option<&FrontendOptions>,
    source_override: Option<&str>,
) -> Result<ProjectContext, FrontendFailure> {
    // A directory is a package root, not a source file. The CLI resolves
    // package forms through the manifest before calling in here; anything else
    // reaching this point gets a real diagnostic rather than the raw
    // `Is a directory` OS error a bare read would surface.
    let documents = options.map_or(&*EMPTY_DOCUMENTS, |options| &options.documents);
    let source = if let Some(source) = source_override {
        documents
            .get(Path::new(input))
            .map_or_else(|| Arc::from(source), Arc::from)
    } else {
        if documents.filesystem && Path::new(input).is_dir() {
            return Err(FrontendFailure::message_only(format!(
                "Error: {input} is a directory, not a .hew source file\n  \
                 hint: a package directory is built with `hew build {input}`"
            )));
        }
        read_source(documents, Path::new(input)).map_err(|e| {
            FrontendFailure::message_only(format!("Error: cannot read {input}: {e}"))
        })?
    };
    let input_dir = Path::new(input).parent().unwrap_or(Path::new("."));
    let project_dir = options
        .and_then(|options| options.project_dir.clone())
        .or_else(|| {
            input_dir
                .ancestors()
                .find(|dir| documents.is_file(&dir.join("hew.toml")))
                .map(Path::to_path_buf)
        })
        .unwrap_or_else(|| input_dir.to_path_buf());
    let (manifest_deps, package_name) = load_manifest_metadata_from(&project_dir, documents)?;
    Ok(ProjectContext {
        source,
        project_dir: project_dir.clone(),
        manifest_deps,
        package_name,
        locked_versions: if options.is_some_and(|options| options.ignore_lockfile) {
            None
        } else {
            load_lockfile_from(&project_dir, documents)?
        },
    })
}

pub(crate) fn project_context_for_program(
    source: &str,
    options: &FrontendOptions,
) -> Result<ProjectContext, FrontendFailure> {
    match &options.project_dir {
        Some(dir) => {
            let (manifest_deps, package_name) =
                load_manifest_metadata_from(dir, &options.documents)?;
            Ok(ProjectContext {
                source: source.into(),
                project_dir: dir.clone(),
                manifest_deps,
                package_name,
                locked_versions: if options.ignore_lockfile {
                    None
                } else {
                    load_lockfile_from(dir, &options.documents)?
                },
            })
        }
        None => Ok(ProjectContext {
            source: source.into(),
            project_dir: std::env::current_dir().unwrap_or_else(|_| PathBuf::from(".")),
            manifest_deps: None,
            package_name: None,
            locked_versions: None,
        }),
    }
}

/// The workspace release version. Development display identities also include
/// their Git count and commit. Every crate in the workspace shares this base.
pub const COMPILER_VERSION: &str = env!("CARGO_PKG_VERSION");

/// Whether a `[package] hew` compiler range admits this compiler. The one
/// reader of that range: the frontend, `hew publish`, `hew check` and the
/// package resolver all ask it.
/// Ordinary ranges describe the compiler's feature version; explicit
/// prerelease bounds compare its full release version.
///
/// # Errors
///
/// Returns the parse error when `range` is not a semver range.
///
/// # Panics
///
/// Never: [`COMPILER_VERSION`] is the workspace's semver version.
pub fn compiler_supports(range: &str) -> Result<bool, semver::Error> {
    let range = semver::VersionReq::parse(range)?;
    let compiler =
        semver::Version::parse(COMPILER_VERSION).expect("the workspace version is semver");
    Ok(compiler_requirement_matches(&range, compiler))
}

fn compiler_requirement_matches(range: &semver::VersionReq, mut compiler: semver::Version) -> bool {
    if range.comparators.iter().all(|bound| bound.pre.is_empty()) {
        compiler.pre = semver::Prerelease::EMPTY;
    }
    range.matches(&compiler)
}

#[cfg(test)]
mod compiler_range_tests {
    #[test]
    fn feature_ranges_and_explicit_prerelease_bounds() {
        for (requirement, compiler, admitted) in [
            (">=0.9.3", "0.10.0-rc1", true),
            ("<0.10.0", "0.10.0-rc1", false),
            ("=0.9.3", "0.10.0-rc1", false),
            (">=0.10.0-rc2", "0.10.0-rc1", false),
            (">=0.10.0-rc1", "0.10.0-rc1", true),
            (">=0.10.0-rc2", "0.10.0", true),
            (">=0.9.3", "0.9.2", false),
        ] {
            let requirement = semver::VersionReq::parse(requirement).unwrap();
            let compiler = semver::Version::parse(compiler).unwrap();
            assert_eq!(
                super::compiler_requirement_matches(&requirement, compiler),
                admitted
            );
        }
    }
}

/// Hew language editions the compiler accepts. Sources in a package whose
/// `hew.toml` names an edition outside this set are rejected before parsing.
const SUPPORTED_EDITIONS: &[&str] = &["2026"];

/// Edition assumed when `hew.toml` is absent or omits the `edition` field.
const DEFAULT_EDITION: &str = "2026";

fn default_edition() -> String {
    DEFAULT_EDITION.to_string()
}

#[derive(Debug, Deserialize)]
pub(crate) struct PackageSection {
    pub(crate) name: String,
    #[serde(default)]
    version: Option<String>,
    #[serde(default = "default_edition")]
    edition: String,
    #[serde(default)]
    hew: Option<String>,
}

/// Table form of a `hew.toml` dependency: `{ version = "^1.0", path = "...",
/// features = [...], optional = true }`. The field set mirrors hew-pkg's `DepTable`
/// so the compiler parses exactly the manifests the package manager accepts.
/// Only dependency *names* (the map keys) are used by the compiler, so these
/// values are parsed for cross-tool compatibility and are otherwise unused.
#[derive(Debug, Deserialize)]
#[allow(
    dead_code,
    reason = "manifest compatibility fields are parsed but not all consumed by the compiler"
)]
struct DepTable {
    /// Absent for a path dependency, as in the package manager's table.
    #[serde(default = "any_version")]
    version: String,
    #[serde(default)]
    path: Option<String>,
    #[serde(default)]
    features: Option<Vec<String>>,
    #[serde(default)]
    optional: Option<bool>,
    #[serde(default)]
    default_features: Option<bool>,
    #[serde(default)]
    registry: Option<String>,
}

fn any_version() -> String {
    "*".to_string()
}

/// A `hew.toml` dependency value: a bare version string (`"^1.0"`) or a detailed
/// table. Untagged to match the package manager's `DepSpec` so the compiler no
/// longer rejects table/path/feature dependencies that `hew install` accepts.
#[derive(Debug, Deserialize)]
#[serde(untagged)]
#[allow(
    dead_code,
    reason = "manifest compatibility variants preserve package-manager dependency syntax"
)]
enum DepSpec {
    Version(String),
    Table(DepTable),
}

#[derive(Debug, Deserialize)]
pub(crate) struct TomlManifest {
    pub(crate) package: Option<PackageSection>,
    #[serde(default)]
    dependencies: BTreeMap<String, DepSpec>,
}

#[derive(Debug, Deserialize)]
struct HewTomlLock {
    #[serde(default)]
    package: Vec<LockedEntry>,
}

#[derive(Debug, Deserialize)]
struct LockedEntry {
    name: String,
    version: String,
}

fn load_optional_toml_from<T: DeserializeOwned>(
    path: &Path,
    sources: &SourceSet,
) -> Result<Option<T>, FrontendFailure> {
    let text = match read_source(sources, path) {
        Ok(text) => text,
        // An optional project file is absent both when the directory does not
        // hold one and when the host has no filesystem to hold it: the browser
        // compiles a buffer with no project behind it, and wasm32 reports that
        // as `Unsupported` rather than `NotFound`.
        Err(err)
            if matches!(
                err.kind(),
                std::io::ErrorKind::NotFound | std::io::ErrorKind::Unsupported
            ) =>
        {
            return Ok(None);
        }
        Err(err) => {
            return Err(FrontendFailure::message_only(format!(
                "Error: cannot read {}: {err}",
                path.display()
            )));
        }
    };
    toml::from_str(&text).map(Some).map_err(|err| {
        FrontendFailure::message_only(format!("Error: cannot parse {}: {err}", path.display()))
    })
}

#[cfg(test)]
fn load_manifest(dir: &Path) -> Result<Option<TomlManifest>, FrontendFailure> {
    load_manifest_from(dir, &EMPTY_DOCUMENTS)
}

pub(crate) fn load_manifest_from(
    dir: &Path,
    sources: &SourceSet,
) -> Result<Option<TomlManifest>, FrontendFailure> {
    let path = dir.join("hew.toml");
    let manifest: Option<TomlManifest> = load_optional_toml_from(&path, sources)?;
    if let Some(m) = &manifest {
        if let Some(package) = &m.package {
            if let Some(requirement) = &package.hew {
                let supported = compiler_supports(requirement).map_err(|error| {
                    FrontendFailure::coded_message(
                        "E_COMPILER_VERSION",
                        format!(
                            "{} has an invalid package.hew requirement: {error}",
                            path.display()
                        ),
                    )
                })?;
                if !supported {
                    return Err(FrontendFailure::coded_message(
                        "E_COMPILER_VERSION",
                        format!(
                            "{} requires Hew {requirement}; this compiler implements Hew \
                             {COMPILER_VERSION}",
                            path.display()
                        ),
                    ));
                }
            }
            if !SUPPORTED_EDITIONS.contains(&package.edition.as_str()) {
                return Err(FrontendFailure::message_only(format!(
                    "Error: E_UNSUPPORTED_EDITION: {} declares edition = \"{}\", which this compiler does not support (supported: {:?})",
                    path.display(),
                    package.edition,
                    SUPPORTED_EDITIONS
                )));
            }
        }
    }
    Ok(manifest)
}

pub(crate) fn verify_locked_project_package(
    check: &LockedPackageCheck,
    sources: &SourceSet,
) -> Result<(), FrontendFailure> {
    let Some(manifest) = load_manifest_from(&check.package_dir, sources)? else {
        return Err(FrontendFailure::message_only(format!(
            "Error: locked package `{}` resolved through `{}` is missing hew.toml\n  hint: run `hew install` to refresh .hew/packages",
            check.name,
            check.package_dir.display()
        )));
    };
    let Some(package) = manifest.package else {
        return Err(FrontendFailure::message_only(format!(
            "Error: locked package `{}` resolved through `{}` has no [package] section\n  hint: run `hew install` to refresh .hew/packages",
            check.name,
            check.package_dir.display()
        )));
    };
    if package.name != check.name || package.version.as_deref() != Some(check.version.as_str()) {
        let found = package.version.as_deref().map_or_else(
            || format!("{}@<missing-version>", package.name),
            |version| format!("{}@{version}", package.name),
        );
        return Err(FrontendFailure::message_only(format!(
            "Error: locked package `{}` resolved through `{}` does not match hew.lock (expected {}@{}, found {found})\n  hint: run `hew install` to refresh .hew/packages",
            check.name,
            check.package_dir.display(),
            check.name,
            check.version
        )));
    }
    Ok(())
}

#[cfg(test)]
pub(crate) fn load_manifest_metadata(
    dir: &Path,
) -> Result<(Option<Vec<String>>, Option<String>), FrontendFailure> {
    load_manifest_metadata_from(dir, &EMPTY_DOCUMENTS)
}

pub(crate) fn load_manifest_metadata_from(
    dir: &Path,
    sources: &SourceSet,
) -> Result<(Option<Vec<String>>, Option<String>), FrontendFailure> {
    match load_manifest_from(dir, sources)? {
        Some(TomlManifest {
            package,
            dependencies,
        }) => Ok((
            Some(dependencies.into_keys().collect()),
            package.map(|package| package.name),
        )),
        None => Ok((None, None)),
    }
}

#[cfg(test)]
pub(crate) fn load_lockfile(dir: &Path) -> Result<Option<Vec<(String, String)>>, FrontendFailure> {
    load_lockfile_from(dir, &EMPTY_DOCUMENTS)
}

pub(crate) fn load_lockfile_from(
    dir: &Path,
    sources: &SourceSet,
) -> Result<Option<Vec<(String, String)>>, FrontendFailure> {
    let path = dir.join("hew.lock");
    let Some(lock) = load_optional_toml_from::<HewTomlLock>(&path, sources)? else {
        return Ok(None);
    };
    Ok(Some(
        lock.package
            .into_iter()
            .map(|entry| (entry.name, entry.version))
            .collect(),
    ))
}

#[cfg(test)]
pub(crate) fn load_package_name(dir: &Path) -> Result<Option<String>, FrontendFailure> {
    Ok(load_manifest(dir)?.and_then(|manifest| manifest.package.map(|package| package.name)))
}

#[cfg(test)]
pub(crate) fn load_dependencies(dir: &Path) -> Result<Option<Vec<String>>, FrontendFailure> {
    Ok(load_manifest(dir)?.map(|manifest| manifest.dependencies.into_keys().collect()))
}
