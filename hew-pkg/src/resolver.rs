//! Semver version resolution for Hew package dependencies.
//!
//! Provides version requirement parsing with package-manager-specific rules and resolution
//! of manifest dependencies against the installed package registry.

// The `ManifestRead` variant embeds `manifest::ManifestError` (~136 bytes).
// Boxing it would add indirection on every error path for minimal gain in a CLI
// tool, so we suppress the lint at module level.
#![allow(
    clippy::result_large_err,
    reason = "ManifestError variant is large but boxing adds unnecessary indirection"
)]

use std::collections::{BTreeMap, BTreeSet};
use std::fmt;
use std::path::{Path, PathBuf};

use crate::index::IndexEntry;
use crate::manifest::{self, DepSpec, HewManifest};
use crate::package_name;
use crate::registry::Registry;

/// A dependency that could not be resolved locally.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnresolvedDep {
    /// Fully qualified package name.
    pub package: String,
    /// Every version requirement currently imposed on the package.
    pub requirements: Vec<String>,
    /// Canonical identity of the registry required for this package.
    pub registry: String,
    /// Config selector used to construct the client, if this is a named source.
    pub registry_selector: Option<String>,
}

/// A resolved package in the dependency graph.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ResolvedPackage {
    /// The exact selected version.
    pub version: String,
    /// All version requirements unified for this package.
    pub requirements: Vec<String>,
    /// The root manifest's direct requirement, if this is a direct dependency.
    pub direct_requirement: Option<String>,
    /// Location from which this package is materialized.
    pub source: PackageSource,
    /// Manifest spelling of a direct path dependency, for lockfile freshness.
    pub direct_path: Option<String>,
}

/// Source selected for a resolved package.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum PackageSource {
    /// A package version in the global registry cache.
    Registry {
        /// Canonical source identity.
        registry: String,
    },
    /// A package rooted at the given local directory.
    Path(PathBuf),
}

/// Errors that can occur during version resolution.
#[derive(Debug)]
pub enum ResolveError {
    /// The version requirement string could not be parsed.
    InvalidVersionReq {
        /// The original input string.
        input: String,
        /// The underlying parse error.
        source: semver::Error,
    },
    /// The package name is not safe to resolve as a registry package.
    InvalidPackageName {
        /// The invalid package name.
        package: String,
    },
    /// No installed version matches the requirement.
    NoMatchingVersion {
        /// The package that was requested.
        package: String,
        /// The requirement string that could not be satisfied.
        requirement: String,
    },
    /// An installed package manifest could not be read.
    ManifestRead {
        /// The package whose manifest could not be read.
        package: String,
        /// The selected version.
        version: String,
        /// The underlying manifest error.
        source: manifest::ManifestError,
    },
    /// A local dependency path does not exist or is not a directory.
    PathDependencyNotFound {
        /// Dependency name from the parent manifest.
        package: String,
        /// Fully resolved local path.
        path: PathBuf,
    },
    /// A local dependency manifest could not be read.
    PathManifestRead {
        /// Dependency name from the parent manifest.
        package: String,
        /// Fully resolved local path.
        path: PathBuf,
        /// Underlying manifest error.
        source: manifest::ManifestError,
    },
    /// A local package's declared name differs from its dependency key.
    PathPackageNameMismatch {
        /// Dependency name from the parent manifest.
        package: String,
        /// Name declared by the local package.
        declared: String,
        /// Fully resolved local path.
        path: PathBuf,
    },
    /// A local package version does not satisfy all active requirements.
    PathVersionMismatch {
        /// Dependency name from the parent manifest.
        package: String,
        /// Version declared by the local package.
        version: String,
        /// Active version requirements.
        requirements: Vec<String>,
        /// Fully resolved local path.
        path: PathBuf,
    },
    /// The graph refers to one package name through incompatible sources.
    ConflictingSources {
        /// Conflicting package name.
        package: String,
        /// First source identity.
        first: PackageSource,
        /// Conflicting source identity.
        second: PackageSource,
    },
    /// A registry-origin manifest contains a forbidden path dependency.
    RegistryPathDependency {
        /// Registry package declaring the dependency.
        package: String,
        /// Dependency key.
        dependency: String,
        /// Untrusted path spelling from the manifest.
        path: String,
    },
    /// A dependency table combines mutually exclusive path and registry fields.
    PathAndRegistry {
        /// Dependency key.
        package: String,
    },
    /// A manifest refers to an unconfigured named registry.
    UnknownRegistry {
        /// Unknown selector.
        registry: String,
    },
    /// The selected dependency graph contains a cycle.
    CircularDependency {
        /// The cycle path, with the start node repeated at the end.
        cycle: Vec<String>,
    },
    /// One or more dependencies could not be resolved locally.
    UnresolvableDeps {
        /// Each unresolved package along with all active requirements.
        failures: Vec<UnresolvedDep>,
    },
    /// A resolver pass repeated without changing its accumulated state.
    NoProgress,
    /// Resolution exceeded its finite pass limit.
    ProgressLimitExceeded {
        /// Maximum number of passes attempted.
        limit: usize,
    },
}

#[expect(
    clippy::too_many_lines,
    reason = "the exhaustive diagnostic mapping keeps every resolver error precise"
)]
impl fmt::Display for ResolveError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::InvalidVersionReq { input, source } => {
                write!(f, "invalid version requirement `{input}`: {source}")
            }
            Self::InvalidPackageName { package } => {
                f.write_str(&package_name::invalid_message(package))
            }
            Self::NoMatchingVersion {
                package,
                requirement,
            } => {
                write!(
                    f,
                    "no installed version of `{package}` matches `{requirement}`"
                )
            }
            Self::ManifestRead {
                package,
                version,
                source,
            } => {
                write!(
                    f,
                    "cannot read installed manifest for `{package}@{version}`: {source}"
                )
            }
            Self::PathDependencyNotFound { package, path } => write!(
                f,
                "path dependency `{package}` was not found at `{}`",
                path.display()
            ),
            Self::PathManifestRead {
                package,
                path,
                source,
            } => write!(
                f,
                "cannot read path dependency `{package}` at `{}`: {source}",
                path.display()
            ),
            Self::PathPackageNameMismatch {
                package,
                declared,
                path,
            } => write!(
                f,
                "path dependency `{package}` at `{}` declares package name `{declared}`",
                path.display()
            ),
            Self::PathVersionMismatch {
                package,
                version,
                requirements,
                path,
            } => write!(
                f,
                "path dependency `{package}` at `{}` has version `{version}`, which does not match [{}]",
                path.display(),
                requirements.join(", ")
            ),
            Self::ConflictingSources {
                package,
                first,
                second,
            } => {
                write!(
                    f,
                    "dependency `{package}` is requested from conflicting sources ({first:?} and {second:?})"
                )
            }
            Self::RegistryPathDependency {
                package,
                dependency,
                path,
            } => write!(
                f,
                "registry package `{package}` contains forbidden path dependency `{dependency}` with path `{path}`"
            ),
            Self::PathAndRegistry { package } => write!(
                f,
                "dependency `{package}` cannot specify both `path` and `registry`"
            ),
            Self::UnknownRegistry { registry } => {
                write!(f, "dependency refers to unknown registry `{registry}`")
            }
            Self::CircularDependency { cycle } => {
                write!(f, "circular dependency detected: {}", cycle.join(" -> "))
            }
            Self::UnresolvableDeps { failures } => {
                write!(f, "unresolvable dependencies:")?;
                for failure in failures {
                    write!(
                        f,
                        "\n  {} [{}]",
                        failure.package,
                        failure.requirements.join(", ")
                    )?;
                }
                Ok(())
            }
            Self::NoProgress => {
                f.write_str("dependency resolution repeated without making progress")
            }
            Self::ProgressLimitExceeded { limit } => write!(
                f,
                "dependency resolution exceeded the finite limit of {limit} passes"
            ),
        }
    }
}

impl std::error::Error for ResolveError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::InvalidVersionReq { source, .. } => Some(source),
            Self::ManifestRead { source, .. } | Self::PathManifestRead { source, .. } => {
                Some(source)
            }
            Self::NoMatchingVersion { .. }
            | Self::InvalidPackageName { .. }
            | Self::PathDependencyNotFound { .. }
            | Self::PathPackageNameMismatch { .. }
            | Self::PathVersionMismatch { .. }
            | Self::ConflictingSources { .. }
            | Self::RegistryPathDependency { .. }
            | Self::PathAndRegistry { .. }
            | Self::UnknownRegistry { .. }
            | Self::CircularDependency { .. }
            | Self::UnresolvableDeps { .. }
            | Self::NoProgress
            | Self::ProgressLimitExceeded { .. } => None,
        }
    }
}

/// A parsed semver version requirement.
///
/// Wraps [`semver::VersionReq`] with package-manager-specific parsing rules:
/// - `"*"` matches any version
/// - Bare versions like `"1.0"` or `"1.0.0"` are treated as **exact** matches
/// - Prefixed versions (`"^1.0"`, `"~1.0"`, `">=1.0"`) use standard semver semantics
/// - Two-part versions are normalized to three parts (e.g. `"1.0"` → `"1.0.0"`)
#[derive(Debug)]
pub struct VersionReq {
    inner: semver::VersionReq,
}

impl VersionReq {
    /// Parse a version requirement string.
    ///
    /// # Errors
    ///
    /// Returns [`ResolveError::InvalidVersionReq`] if the string cannot be
    /// parsed as a valid semver requirement.
    pub fn parse(input: &str) -> Result<Self, ResolveError> {
        let trimmed = input.trim();

        if trimmed == "*" {
            return Ok(Self {
                inner: semver::VersionReq::STAR,
            });
        }

        let has_operator = trimmed.starts_with('^')
            || trimmed.starts_with('~')
            || trimmed.starts_with('>')
            || trimmed.starts_with('<')
            || trimmed.starts_with('=');

        let req_str = if has_operator {
            let (prefix, version_part) = split_operator(trimmed);
            let normalized = normalize_version(version_part.trim());
            format!("{prefix}{normalized}")
        } else {
            // Bare version → exact match.
            let normalized = normalize_version(trimmed);
            format!("={normalized}")
        };

        let inner =
            semver::VersionReq::parse(&req_str).map_err(|e| ResolveError::InvalidVersionReq {
                input: input.to_string(),
                source: e,
            })?;
        Ok(Self { inner })
    }

    /// Returns `true` if `version` satisfies this requirement.
    #[must_use]
    pub fn matches(&self, version: &semver::Version) -> bool {
        self.inner.matches(version)
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
struct PackageState {
    requirements: BTreeSet<String>,
    direct_requirement: Option<String>,
    requested_features: BTreeSet<String>,
    use_default_features: bool,
    registry_selector: Option<String>,
    source: Option<PackageSource>,
    direct_path: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct ExpandedState {
    version: String,
    requested_features: BTreeSet<String>,
    use_default_features: bool,
}

#[derive(Debug, Clone)]
struct DepRequest {
    name: String,
    requirement: String,
    direct_requirement: Option<String>,
    features: BTreeSet<String>,
    use_default_features: bool,
    registry_selector: Option<String>,
    source: PackageSource,
    direct_path: Option<String>,
}

#[derive(Debug, Clone, Default)]
struct FailureState {
    requirements: BTreeSet<String>,
    registry: String,
    registry_selector: Option<String>,
}

#[derive(Debug, Clone, Default)]
struct ActiveFeatures {
    enabled_optional_dependencies: BTreeSet<String>,
}

struct CachedManifest {
    manifest: HewManifest,
    root: PathBuf,
}

/// Registry identities available while resolving one dependency graph.
#[derive(Debug, Clone)]
pub struct RegistrySources {
    default_registry: String,
    default_selector: Option<String>,
    named: BTreeMap<String, String>,
}

impl RegistrySources {
    /// Construct source identities for the CLI-selected default and all named
    /// registries configured for explicit dependency selectors.
    #[must_use]
    pub fn new(
        default_registry: String,
        default_selector: Option<String>,
        named: BTreeMap<String, String>,
    ) -> Self {
        Self {
            default_registry,
            default_selector,
            named,
        }
    }

    /// Construct the compiled-in default source without named registries.
    #[must_use]
    pub fn default_source() -> Self {
        Self::new(
            crate::config::default_registry_identity(),
            None,
            BTreeMap::new(),
        )
    }

    /// Canonical identity used for root and local dependencies without a selector.
    #[must_use]
    pub fn default_registry(&self) -> &str {
        &self.default_registry
    }

    /// Configured selector used for the default source, if named.
    #[must_use]
    pub fn default_selector(&self) -> Option<&str> {
        self.default_selector.as_deref()
    }

    /// Configured named source identities.
    #[must_use]
    pub fn named(&self) -> &BTreeMap<String, String> {
        &self.named
    }

    fn resolve(
        &self,
        selector: Option<&str>,
        inherited: Option<(&str, Option<&str>)>,
    ) -> Result<(String, Option<String>), ResolveError> {
        if let Some(selector) = selector {
            let identity =
                self.named
                    .get(selector)
                    .ok_or_else(|| ResolveError::UnknownRegistry {
                        registry: selector.to_string(),
                    })?;
            return Ok((identity.clone(), Some(selector.to_string())));
        }
        if let Some((identity, selector)) = inherited {
            return Ok((
                identity.to_string(),
                selector.map(std::string::ToString::to_string),
            ));
        }
        Ok((self.default_registry.clone(), self.default_selector.clone()))
    }
}

enum VisitControl {
    Continue,
    Restart,
}

enum PassOutcome {
    Resolved(BTreeMap<String, ResolvedPackage>),
    Restart(BTreeMap<String, PackageState>),
    Unresolved(Vec<UnresolvedDep>),
}

struct ResolverPass<'a> {
    registry: &'a Registry,
    registry_sources: &'a RegistrySources,
    root: &'a Path,
    allow_legacy_default: bool,
    pinned_registry_paths: Option<&'a BTreeMap<(String, String, String), PathBuf>>,
    available_versions: BTreeMap<(String, String), Vec<semver::Version>>,
    package_states: BTreeMap<String, PackageState>,
    selected_versions: BTreeMap<String, String>,
    expanded_states: BTreeMap<String, ExpandedState>,
    failures: BTreeMap<String, FailureState>,
    manifest_cache: BTreeMap<(PackageSource, String, String), CachedManifest>,
}

impl<'a> ResolverPass<'a> {
    fn with_seed(
        registry: &'a Registry,
        registry_sources: &'a RegistrySources,
        root: &'a Path,
        allow_legacy_default: bool,
        confirmed_versions: Option<&'a BTreeMap<(String, String), BTreeSet<String>>>,
        pinned_registry_paths: Option<&'a BTreeMap<(String, String, String), PathBuf>>,
        package_states: BTreeMap<String, PackageState>,
    ) -> Self {
        let available_versions = pinned_registry_paths.map_or_else(
            || {
                available_versions(
                    registry,
                    registry_sources,
                    allow_legacy_default,
                    confirmed_versions,
                )
            },
            |paths| {
                let mut versions = BTreeMap::<(String, String), Vec<semver::Version>>::new();
                for (registry, name, version) in paths.keys() {
                    if let Ok(version) = semver::Version::parse(version) {
                        versions
                            .entry((registry.clone(), name.clone()))
                            .or_default()
                            .push(version);
                    }
                }
                versions
            },
        );
        Self {
            registry,
            registry_sources,
            root,
            allow_legacy_default,
            pinned_registry_paths,
            available_versions,
            package_states,
            selected_versions: BTreeMap::new(),
            expanded_states: BTreeMap::new(),
            failures: BTreeMap::new(),
            manifest_cache: BTreeMap::new(),
        }
    }

    fn resolve_manifest(mut self, manifest: &HewManifest) -> Result<PassOutcome, ResolveError> {
        for request in root_requests(manifest, self.root, self.registry_sources)? {
            match self.visit_request(request, &[])? {
                VisitControl::Continue => {}
                VisitControl::Restart => return Ok(PassOutcome::Restart(self.package_states)),
            }
        }

        if self.failures.is_empty() {
            let resolved = self
                .selected_versions
                .iter()
                .map(|(name, version)| {
                    let state = self
                        .package_states
                        .get(name)
                        .expect("selected package must have accumulated state");
                    (
                        name.clone(),
                        ResolvedPackage {
                            version: version.clone(),
                            requirements: state.requirements.iter().cloned().collect(),
                            direct_requirement: state.direct_requirement.clone(),
                            source: state
                                .source
                                .clone()
                                .expect("selected package must have a source"),
                            direct_path: state.direct_path.clone(),
                        },
                    )
                })
                .collect();
            Ok(PassOutcome::Resolved(resolved))
        } else {
            let failures = self
                .failures
                .into_iter()
                .map(|(package, failure)| UnresolvedDep {
                    package,
                    requirements: failure.requirements.into_iter().collect(),
                    registry: failure.registry,
                    registry_selector: failure.registry_selector,
                })
                .collect();
            Ok(PassOutcome::Unresolved(failures))
        }
    }

    fn visit_request(
        &mut self,
        request: DepRequest,
        path: &[String],
    ) -> Result<VisitControl, ResolveError> {
        validate_package_name(&request.name)?;

        if let Some(index) = path.iter().position(|node| node == &request.name) {
            let mut cycle = path[index..].to_vec();
            cycle.push(request.name.clone());
            return Err(ResolveError::CircularDependency { cycle });
        }

        let request_source = request.source.clone();
        let (requirements, requested_features, use_default_features) = {
            let state = self.package_states.entry(request.name.clone()).or_default();
            if state
                .source
                .as_ref()
                .is_some_and(|source| source != &request_source)
            {
                return Err(ResolveError::ConflictingSources {
                    package: request.name,
                    first: state.source.clone().expect("checked existing source"),
                    second: request_source,
                });
            }
            state.source.get_or_insert_with(|| request_source.clone());
            state.requirements.insert(request.requirement.clone());
            if state.direct_requirement.is_none() {
                state
                    .direct_requirement
                    .clone_from(&request.direct_requirement);
            }
            if state.registry_selector.is_none() {
                state
                    .registry_selector
                    .clone_from(&request.registry_selector);
            }
            if state.direct_path.is_none() {
                state.direct_path.clone_from(&request.direct_path);
            }
            state
                .requested_features
                .extend(request.features.iter().cloned());
            state.use_default_features |= request.use_default_features;
            (
                state.requirements.iter().cloned().collect::<Vec<_>>(),
                state.requested_features.clone(),
                state.use_default_features,
            )
        };

        let Some(version) = self.select_version(&request, &request_source, &requirements)? else {
            return Ok(VisitControl::Continue);
        };

        self.failures.remove(&request.name);
        self.selected_versions
            .insert(request.name.clone(), version.clone());

        let expanded = ExpandedState {
            version: version.clone(),
            requested_features,
            use_default_features,
        };

        if let Some(previous) = self.expanded_states.get(&request.name) {
            if previous.version != expanded.version {
                return Ok(VisitControl::Restart);
            }
            if previous == &expanded {
                return Ok(VisitControl::Continue);
            }
        }

        let state_snapshot = self
            .package_states
            .get(&request.name)
            .expect("package state must exist after merging request")
            .clone();
        let dependency_requests = {
            let registry_sources = self.registry_sources.clone();
            let cached = self.load_manifest(&request.name, &version, &request_source)?;
            dependency_requests_from_manifest(
                &cached.manifest,
                &state_snapshot,
                &cached.root,
                &request_source,
                &registry_sources,
            )?
        };

        self.expanded_states.insert(request.name.clone(), expanded);

        let mut next_path = path.to_vec();
        next_path.push(request.name);

        for dependency_request in dependency_requests {
            match self.visit_request(dependency_request, &next_path)? {
                VisitControl::Continue => {}
                VisitControl::Restart => return Ok(VisitControl::Restart),
            }
        }

        Ok(VisitControl::Continue)
    }

    fn select_version(
        &mut self,
        request: &DepRequest,
        source: &PackageSource,
        requirements: &[String],
    ) -> Result<Option<String>, ResolveError> {
        match source {
            PackageSource::Registry { registry } => {
                let version = select_highest_matching_version(
                    registry,
                    &request.name,
                    requirements,
                    &self.available_versions,
                )?;
                if version.is_none() {
                    self.failures
                        .entry(request.name.clone())
                        .and_modify(|failure| {
                            failure.requirements.extend(requirements.iter().cloned());
                            if failure.registry_selector.is_none() {
                                failure
                                    .registry_selector
                                    .clone_from(&request.registry_selector);
                            }
                        })
                        .or_insert_with(|| FailureState {
                            requirements: requirements.iter().cloned().collect(),
                            registry: registry.clone(),
                            registry_selector: request.registry_selector.clone(),
                        });
                }
                Ok(version)
            }
            PackageSource::Path(path) => self
                .load_path_manifest(&request.name, path, requirements)
                .map(Some),
        }
    }

    fn load_manifest(
        &mut self,
        package: &str,
        version: &str,
        source: &PackageSource,
    ) -> Result<&CachedManifest, ResolveError> {
        let key = (source.clone(), package.to_string(), version.to_string());
        if !self.manifest_cache.contains_key(&key) {
            match source {
                PackageSource::Registry { registry } => {
                    let root = self.pinned_registry_paths.map_or_else(
                        || {
                            let namespaced =
                                self.registry.package_dir_for(registry, package, version);
                            if namespaced.is_dir() {
                                namespaced
                            } else if self.allow_legacy_default
                                && registry == self.registry_sources.default_registry()
                            {
                                self.registry.package_dir(package, version)
                            } else {
                                namespaced
                            }
                        },
                        |paths| {
                            paths
                                .get(&(registry.clone(), package.to_string(), version.to_string()))
                                .expect("locked available versions must have a pinned path")
                                .clone()
                        },
                    );
                    let manifest =
                        manifest::parse_manifest(&root.join("hew.toml")).map_err(|source| {
                            ResolveError::ManifestRead {
                                package: package.to_string(),
                                version: version.to_string(),
                                source,
                            }
                        })?;
                    validate_registry_manifest(package, &manifest)?;
                    self.manifest_cache
                        .insert(key.clone(), CachedManifest { manifest, root });
                }
                PackageSource::Path(path) => {
                    self.load_path_manifest(package, path, &[version.to_string()])?;
                }
            }
        }
        Ok(self
            .manifest_cache
            .get(&key)
            .expect("manifest cache entry must exist after insertion"))
    }

    fn load_path_manifest(
        &mut self,
        package: &str,
        path: &Path,
        requirements: &[String],
    ) -> Result<String, ResolveError> {
        if !path.is_dir() {
            return Err(ResolveError::PathDependencyNotFound {
                package: package.to_string(),
                path: path.to_path_buf(),
            });
        }
        let manifest = manifest::parse_manifest(&path.join("hew.toml")).map_err(|source| {
            ResolveError::PathManifestRead {
                package: package.to_string(),
                path: path.to_path_buf(),
                source,
            }
        })?;
        if manifest.package.name != package {
            return Err(ResolveError::PathPackageNameMismatch {
                package: package.to_string(),
                declared: manifest.package.name,
                path: path.to_path_buf(),
            });
        }
        let version = semver::Version::parse(&manifest.package.version).map_err(|source| {
            ResolveError::InvalidVersionReq {
                input: manifest.package.version.clone(),
                source,
            }
        })?;
        let parsed_requirements = parse_requirements(requirements)?;
        if !parsed_requirements
            .iter()
            .all(|requirement| requirement.matches(&version))
        {
            return Err(ResolveError::PathVersionMismatch {
                package: package.to_string(),
                version: version.to_string(),
                requirements: requirements.to_vec(),
                path: path.to_path_buf(),
            });
        }
        let version = version.to_string();
        self.manifest_cache.insert(
            (
                PackageSource::Path(path.to_path_buf()),
                package.to_string(),
                version.clone(),
            ),
            CachedManifest {
                manifest,
                root: path.to_path_buf(),
            },
        );
        Ok(version)
    }
}

/// Split a version string with an operator prefix into `(operator, version)`.
fn split_operator(s: &str) -> (&str, &str) {
    if s.starts_with(">=") || s.starts_with("<=") || s.starts_with("!=") {
        s.split_at(2)
    } else {
        // Single-char operators: ^, ~, >, <, =
        s.split_at(1)
    }
}

/// Pad a version string to three dot-separated parts.
///
/// `"1"` → `"1.0.0"`, `"1.0"` → `"1.0.0"`, `"1.0.0"` unchanged.
fn normalize_version(v: &str) -> String {
    let dot_count = v.chars().filter(|&c| c == '.').count();
    match dot_count {
        0 => format!("{v}.0.0"),
        1 => format!("{v}.0"),
        _ => v.to_string(),
    }
}

fn available_versions(
    registry: &Registry,
    sources: &RegistrySources,
    allow_legacy_default: bool,
    confirmed_versions: Option<&BTreeMap<(String, String), BTreeSet<String>>>,
) -> BTreeMap<(String, String), Vec<semver::Version>> {
    let mut versions = BTreeMap::<(String, String), Vec<semver::Version>>::new();
    let mut identities = sources.named.values().cloned().collect::<BTreeSet<_>>();
    identities.insert(sources.default_registry.clone());
    for identity in identities {
        let include_legacy = allow_legacy_default && identity == sources.default_registry;
        for package in registry.list_packages_for(&identity, include_legacy) {
            let package_key = (identity.clone(), package.name.clone());
            if confirmed_versions.is_some_and(|confirmed| {
                !confirmed
                    .get(&package_key)
                    .is_some_and(|versions| versions.contains(&package.version))
            }) {
                continue;
            }
            if let Ok(version) = semver::Version::parse(&package.version) {
                versions.entry(package_key).or_default().push(version);
            }
        }
    }
    for package_versions in versions.values_mut() {
        package_versions.sort();
        package_versions.dedup();
    }
    versions
}

fn parse_requirements(requirements: &[String]) -> Result<Vec<VersionReq>, ResolveError> {
    requirements
        .iter()
        .map(|requirement| VersionReq::parse(requirement))
        .collect()
}

fn select_highest_matching_version(
    registry: &str,
    package_name: &str,
    requirements: &[String],
    versions_by_package: &BTreeMap<(String, String), Vec<semver::Version>>,
) -> Result<Option<String>, ResolveError> {
    let reqs = parse_requirements(requirements)?;
    Ok(versions_by_package
        .get(&(registry.to_string(), package_name.to_string()))
        .and_then(|versions| {
            versions
                .iter()
                .rev()
                .find(|version| reqs.iter().all(|req| req.matches(version)))
        })
        .map(ToString::to_string))
}

fn root_requests(
    manifest: &HewManifest,
    root: &Path,
    sources: &RegistrySources,
) -> Result<Vec<DepRequest>, ResolveError> {
    manifest
        .dependencies
        .iter()
        .map(|(name, dep_spec)| {
            let (source, registry_selector) =
                dependency_source(name, dep_spec, root, None, sources, None)?;
            Ok(DepRequest {
                name: name.clone(),
                requirement: dep_spec.version_req().to_string(),
                direct_requirement: Some(dep_spec.version_req().to_string()),
                features: requested_features(dep_spec),
                use_default_features: uses_default_features(dep_spec),
                registry_selector,
                source,
                direct_path: dependency_path(dep_spec).map(ToString::to_string),
            })
        })
        .collect()
}

fn dependency_path(dep_spec: &DepSpec) -> Option<&str> {
    match dep_spec {
        DepSpec::Version(_) => None,
        DepSpec::Table(table) => table.path.as_deref(),
    }
}

fn dependency_source(
    package: &str,
    dep_spec: &DepSpec,
    root: &Path,
    inherited: Option<(&str, Option<&str>)>,
    sources: &RegistrySources,
    registry_origin: Option<&str>,
) -> Result<(PackageSource, Option<String>), ResolveError> {
    let path = dependency_path(dep_spec);
    let selector = dependency_registry(dep_spec);
    if let Some(parent) = registry_origin {
        if let Some(path) = path {
            return Err(ResolveError::RegistryPathDependency {
                package: parent.to_string(),
                dependency: package.to_string(),
                path: path.to_string(),
            });
        }
    }
    if path.is_some() && selector.is_some() {
        return Err(ResolveError::PathAndRegistry {
            package: package.to_string(),
        });
    }
    let Some(path) = path else {
        let (registry, registry_selector) = sources.resolve(selector.as_deref(), inherited)?;
        return Ok((PackageSource::Registry { registry }, registry_selector));
    };
    let resolved = root.join(path);
    let canonical = resolved
        .canonicalize()
        .map_err(|_| ResolveError::PathDependencyNotFound {
            package: package.to_string(),
            path: resolved.clone(),
        })?;
    if !canonical.is_dir() {
        return Err(ResolveError::PathDependencyNotFound {
            package: package.to_string(),
            path: canonical,
        });
    }
    Ok((PackageSource::Path(canonical), None))
}

fn requested_features(dep_spec: &DepSpec) -> BTreeSet<String> {
    match dep_spec {
        DepSpec::Version(_) => BTreeSet::new(),
        DepSpec::Table(table) => table
            .features
            .clone()
            .unwrap_or_default()
            .into_iter()
            .collect(),
    }
}

fn uses_default_features(dep_spec: &DepSpec) -> bool {
    match dep_spec {
        DepSpec::Version(_) => true,
        DepSpec::Table(table) => table.default_features.unwrap_or(true),
    }
}

fn dependency_registry(dep_spec: &DepSpec) -> Option<String> {
    match dep_spec {
        DepSpec::Version(_) => None,
        DepSpec::Table(table) => table.registry.clone(),
    }
}

fn dependency_requests_from_manifest(
    manifest: &HewManifest,
    state: &PackageState,
    root: &Path,
    parent_source: &PackageSource,
    sources: &RegistrySources,
) -> Result<Vec<DepRequest>, ResolveError> {
    if let PackageSource::Registry { .. } = parent_source {
        validate_registry_manifest(&manifest.package.name, manifest)?;
    }
    let active_features = resolve_active_features(manifest, state);
    let mut requests = Vec::new();
    for (name, dep_spec) in &manifest.dependencies {
        if dependency_is_optional(dep_spec)
            && !active_features.enabled_optional_dependencies.contains(name)
        {
            continue;
        }

        let inherited = match parent_source {
            PackageSource::Registry { registry } => {
                Some((registry.as_str(), state.registry_selector.as_deref()))
            }
            PackageSource::Path(_) => None,
        };
        let (source, registry_selector) = dependency_source(
            name,
            dep_spec,
            root,
            inherited,
            sources,
            matches!(parent_source, PackageSource::Registry { .. })
                .then_some(manifest.package.name.as_str()),
        )?;
        requests.push(DepRequest {
            name: name.clone(),
            requirement: dep_spec.version_req().to_string(),
            direct_requirement: None,
            features: requested_features(dep_spec),
            use_default_features: uses_default_features(dep_spec),
            registry_selector,
            source,
            direct_path: None,
        });
    }

    Ok(requests)
}

/// Reject path dependencies in a manifest obtained from a registry.
///
/// This checks normal and development dependencies so cached or downloaded
/// registry content cannot smuggle any local filesystem dependency.
pub(crate) fn validate_registry_manifest(
    package: &str,
    manifest: &HewManifest,
) -> Result<(), ResolveError> {
    for (dependency, spec) in manifest
        .dependencies
        .iter()
        .chain(manifest.dev_dependencies.iter())
    {
        if let Some(path) = dependency_path(spec) {
            return Err(ResolveError::RegistryPathDependency {
                package: package.to_string(),
                dependency: dependency.clone(),
                path: path.to_string(),
            });
        }
    }
    Ok(())
}

fn dependency_is_optional(dep_spec: &DepSpec) -> bool {
    matches!(dep_spec, DepSpec::Table(table) if table.optional.unwrap_or(false))
}

fn resolve_active_features(manifest: &HewManifest, state: &PackageState) -> ActiveFeatures {
    let mut pending: Vec<String> = state.requested_features.iter().cloned().collect();
    if state.use_default_features {
        pending.extend(
            manifest
                .features
                .get("default")
                .cloned()
                .unwrap_or_default(),
        );
    }

    let mut seen_features = BTreeSet::new();
    let mut active = ActiveFeatures::default();

    while let Some(feature) = pending.pop() {
        if manifest.dependencies.contains_key(&feature) {
            active.enabled_optional_dependencies.insert(feature.clone());
        }

        if !seen_features.insert(feature.clone()) {
            continue;
        }

        if let Some(implied) = manifest.features.get(&feature) {
            for item in implied {
                if manifest.dependencies.contains_key(item) {
                    active.enabled_optional_dependencies.insert(item.clone());
                } else {
                    pending.push(item.clone());
                }
            }
        }
    }

    active
}

/// Find the highest installed version of `package_name` matching `requirement`.
///
/// Scans the registry for all installed versions of the named package, filters
/// them against the parsed requirement, and returns the highest match.
///
/// # Errors
///
/// Returns [`ResolveError::InvalidVersionReq`] if `requirement` cannot be
/// parsed, or [`ResolveError::NoMatchingVersion`] if no installed version
/// satisfies the requirement.
pub fn resolve_version(
    package_name: &str,
    requirement: &str,
    registry: &Registry,
) -> Result<String, ResolveError> {
    validate_package_name(package_name)?;

    let requirements = vec![requirement.to_string()];
    let sources = RegistrySources::default_source();
    select_highest_matching_version(
        sources.default_registry(),
        package_name,
        &requirements,
        &available_versions(registry, &sources, true, None),
    )?
    .ok_or_else(|| ResolveError::NoMatchingVersion {
        package: package_name.to_string(),
        requirement: requirement.to_string(),
    })
}

/// Resolve one direct dependency from the manifest that declares it.
///
/// Registry dependencies are selected from the installed package registry.
/// Path dependencies are resolved relative to `root` and validated against
/// their local manifest.
///
/// # Errors
///
/// Returns [`ResolveError`] when the dependency requirement is invalid, no
/// registry version matches, or a path dependency is missing or malformed.
pub fn resolve_dependency_from_root(
    package_name: &str,
    spec: &DepSpec,
    root: &Path,
    registry: &Registry,
) -> Result<String, ResolveError> {
    validate_package_name(package_name)?;
    let sources = RegistrySources::default_source();
    let (source, _) = dependency_source(package_name, spec, root, None, &sources, None)?;
    match source {
        PackageSource::Registry { .. } => {
            resolve_version(package_name, spec.version_req(), registry)
        }
        PackageSource::Path(path) => {
            let mut resolver = ResolverPass::with_seed(
                registry,
                &sources,
                root,
                true,
                None,
                None,
                BTreeMap::new(),
            );
            resolver.load_path_manifest(package_name, &path, &[spec.version_req().to_string()])
        }
    }
}

/// Find the highest cached version satisfying all active requirements.
///
/// # Errors
///
/// Returns [`ResolveError::InvalidVersionReq`] if any requirement is invalid.
pub fn resolve_cached_version(
    package_name: &str,
    requirements: &[String],
    registry: &Registry,
) -> Result<Option<String>, ResolveError> {
    validate_package_name(package_name)?;
    let sources = RegistrySources::default_source();
    select_highest_matching_version(
        sources.default_registry(),
        package_name,
        requirements,
        &available_versions(registry, &sources, true, None),
    )
}

fn validate_package_name(package: &str) -> Result<(), ResolveError> {
    if package_name::is_valid(package) {
        Ok(())
    } else {
        Err(ResolveError::InvalidPackageName {
            package: package.to_string(),
        })
    }
}

/// Resolved version from a remote index query.
#[derive(Debug)]
pub struct ResolvedEntry {
    pub version: String,
    pub checksum: String,
    pub dl: Option<String>,
    /// Ed25519 signature of the checksum.
    pub sig: String,
    /// Fingerprint of the signing key.
    pub key_fp: String,
    /// Registry counter-signature.
    pub registry_sig: Option<String>,
    /// Timestamp when the registry accepted the publish.
    pub published_at: Option<String>,
}

fn best_matching_entry<'a>(
    entries: &'a [IndexEntry],
    requirements: &[String],
) -> Result<Option<(semver::Version, &'a IndexEntry)>, ResolveError> {
    let reqs = parse_requirements(requirements)?;

    let mut matching: Vec<_> = entries
        .iter()
        .filter(|entry| !entry.yanked.is_yanked())
        .filter_map(|entry| {
            semver::Version::parse(&entry.vers)
                .ok()
                .filter(|version| reqs.iter().all(|req| req.matches(version)))
                .map(|version| (version, entry))
        })
        .collect();

    matching.sort_by(|(left, _), (right, _)| left.cmp(right));
    Ok(matching.pop())
}

/// Find the highest non-yanked version from remote index entries that matches
/// a version requirement.
///
/// # Errors
///
/// Returns [`ResolveError::InvalidVersionReq`] if `requirement` cannot be parsed.
#[allow(
    dead_code,
    reason = "public API reserved for single-requirement callers"
)]
pub fn resolve_version_from_entries(
    entries: &[IndexEntry],
    requirement: &str,
) -> Result<Option<ResolvedEntry>, ResolveError> {
    resolve_version_from_entries_with_requirements(entries, &[requirement.to_string()])
}

/// Find the highest non-yanked version from remote index entries that satisfies
/// every version requirement in `requirements`.
///
/// # Errors
///
/// Returns [`ResolveError::InvalidVersionReq`] if any requirement cannot be parsed.
pub fn resolve_version_from_entries_with_requirements(
    entries: &[IndexEntry],
    requirements: &[String],
) -> Result<Option<ResolvedEntry>, ResolveError> {
    Ok(
        best_matching_entry(entries, requirements)?.map(|(version, entry)| ResolvedEntry {
            version: version.to_string(),
            checksum: entry.cksum.clone(),
            dl: entry.dl.clone(),
            sig: entry.sig.clone(),
            key_fp: entry.key_fp.clone(),
            registry_sig: entry.registry_sig.clone(),
            published_at: entry.published_at.clone(),
        }),
    )
}

/// Resolve every dependency in `manifest` to exact installed versions,
/// traversing the full transitive dependency graph.
///
/// Resolution is greedy: for each package, the resolver picks the highest locally
/// installed version compatible with every requirement currently imposed on that
/// package. Feature requests are unified across the graph.
///
/// # Errors
///
/// Returns [`ResolveError::InvalidVersionReq`] if any requirement string is
/// unparseable, [`ResolveError::CircularDependency`] when the resolved graph
/// contains a cycle, [`ResolveError::ManifestRead`] when an installed package
/// manifest cannot be read, or [`ResolveError::UnresolvableDeps`] listing every
/// package that still needs a locally available compatible version.
pub fn resolve_all(
    manifest: &HewManifest,
    registry: &Registry,
) -> Result<BTreeMap<String, ResolvedPackage>, ResolveError> {
    resolve_all_from_root(manifest, Path::new("."), registry)
}

/// Resolve dependencies from the cache, interpreting path dependencies
/// relative to `root`.
///
/// # Errors
///
/// Returns [`ResolveError`] when the graph cannot be resolved.
pub fn resolve_all_from_root(
    manifest: &HewManifest,
    root: &Path,
    registry: &Registry,
) -> Result<BTreeMap<String, ResolvedPackage>, ResolveError> {
    let sources = RegistrySources::default_source();
    resolve_all_inner(manifest, root, registry, &sources, true, None, None)
}

/// Resolve from source-namespaced cache slots, optionally admitting the legacy
/// default-registry layout for explicit offline compatibility.
///
/// # Errors
///
/// Returns [`ResolveError`] when the source-bound graph cannot be resolved.
pub fn resolve_all_from_root_with_sources(
    manifest: &HewManifest,
    root: &Path,
    registry: &Registry,
    sources: &RegistrySources,
    allow_legacy_default: bool,
) -> Result<BTreeMap<String, ResolvedPackage>, ResolveError> {
    resolve_all_inner(
        manifest,
        root,
        registry,
        sources,
        allow_legacy_default,
        None,
        None,
    )
}

/// Resolve dependencies using only registry versions confirmed by the current
/// online install operation. Path dependencies remain available from disk.
///
/// # Errors
///
/// Returns [`ResolveError`] when the graph cannot be resolved.
pub fn resolve_all_confirmed(
    manifest: &HewManifest,
    root: &Path,
    registry: &Registry,
    confirmed_versions: &BTreeMap<(String, String), BTreeSet<String>>,
) -> Result<BTreeMap<String, ResolvedPackage>, ResolveError> {
    let sources = RegistrySources::default_source();
    resolve_all_inner(
        manifest,
        root,
        registry,
        &sources,
        false,
        Some(confirmed_versions),
        None,
    )
}

/// Resolve a dependency graph using the exact registry versions recorded in a
/// lockfile. Path dependencies continue to be resolved from their declared
/// paths, but no registry version outside `pinned_registry_paths` is eligible.
///
/// # Errors
///
/// Returns [`ResolveError`] when an exact locked version is missing,
/// incompatible with the graph, or otherwise cannot be resolved.
pub fn resolve_all_locked(
    manifest: &HewManifest,
    root: &Path,
    registry: &Registry,
    pinned_registry_paths: &BTreeMap<(String, String, String), PathBuf>,
) -> Result<BTreeMap<String, ResolvedPackage>, ResolveError> {
    resolve_all_pinned(manifest, root, registry, pinned_registry_paths)
}

/// Resolve dependencies using only the exact immutable registry generations
/// verified by the current install operation. Path dependencies remain
/// available from disk.
///
/// # Errors
///
/// Returns [`ResolveError`] when the pinned graph cannot satisfy all dependency
/// requirements or a pinned manifest cannot be loaded.
pub fn resolve_all_pinned(
    manifest: &HewManifest,
    root: &Path,
    registry: &Registry,
    pinned_registry_paths: &BTreeMap<(String, String, String), PathBuf>,
) -> Result<BTreeMap<String, ResolvedPackage>, ResolveError> {
    let sources = RegistrySources::default_source();
    resolve_all_inner(
        manifest,
        root,
        registry,
        &sources,
        false,
        None,
        Some(pinned_registry_paths),
    )
}

/// Resolve using exact source-bound immutable registry generations.
///
/// # Errors
///
/// Returns [`ResolveError`] when the pinned source-bound graph cannot be
/// resolved.
pub fn resolve_all_pinned_with_sources(
    manifest: &HewManifest,
    root: &Path,
    registry: &Registry,
    sources: &RegistrySources,
    pinned_registry_paths: &BTreeMap<(String, String, String), PathBuf>,
) -> Result<BTreeMap<String, ResolvedPackage>, ResolveError> {
    resolve_all_inner(
        manifest,
        root,
        registry,
        sources,
        false,
        None,
        Some(pinned_registry_paths),
    )
}

fn resolve_all_inner(
    manifest: &HewManifest,
    root: &Path,
    registry: &Registry,
    registry_sources: &RegistrySources,
    allow_legacy_default: bool,
    confirmed_versions: Option<&BTreeMap<(String, String), BTreeSet<String>>>,
    pinned_registry_paths: Option<&BTreeMap<(String, String, String), PathBuf>>,
) -> Result<BTreeMap<String, ResolvedPackage>, ResolveError> {
    const MAX_RESOLUTION_PASSES: usize = 1024;

    let mut seed_states = BTreeMap::new();
    for _ in 0..MAX_RESOLUTION_PASSES {
        let previous_seed_states = seed_states.clone();
        let pass = ResolverPass::with_seed(
            registry,
            registry_sources,
            root,
            allow_legacy_default,
            confirmed_versions,
            pinned_registry_paths,
            seed_states,
        );
        match pass.resolve_manifest(manifest)? {
            PassOutcome::Resolved(resolved) => return Ok(resolved),
            PassOutcome::Restart(next_seed_states) => {
                if next_seed_states == previous_seed_states {
                    return Err(ResolveError::NoProgress);
                }
                seed_states = next_seed_states;
            }
            PassOutcome::Unresolved(failures) => {
                return Err(ResolveError::UnresolvableDeps { failures });
            }
        }
    }
    Err(ResolveError::ProgressLimitExceeded {
        limit: MAX_RESOLUTION_PASSES,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::manifest::{DepTable, Package};

    #[derive(Clone, Copy)]
    struct FakeDep<'a> {
        name: &'a str,
        version: &'a str,
        optional: bool,
        features: &'a [&'a str],
        default_features: bool,
    }

    /// Create a temporary registry directory and `Registry` handle.
    ///
    /// The returned `TempDir` must be kept alive for the registry to remain
    /// valid.
    fn test_registry() -> (tempfile::TempDir, Registry) {
        let dir = tempfile::tempdir().unwrap();
        let reg = Registry::with_root(dir.path().to_path_buf());
        (dir, reg)
    }

    /// Install a fake package version in the registry.
    fn install_fake(registry: &Registry, name: &str, version: &str) {
        install_fake_package(registry, name, version, &[], &[]);
    }

    fn install_fake_package(
        registry: &Registry,
        name: &str,
        version: &str,
        dependencies: &[FakeDep<'_>],
        features: &[(&str, &[&str])],
    ) {
        use std::fmt::Write as _;

        let dir = registry.package_dir(name, version);
        std::fs::create_dir_all(&dir).unwrap();

        let mut content = format!("[package]\nname = \"{name}\"\nversion = \"{version}\"\n");
        if !dependencies.is_empty() {
            content.push_str("\n[dependencies]\n");
            for dependency in dependencies {
                if !dependency.optional
                    && dependency.features.is_empty()
                    && dependency.default_features
                {
                    let _ = writeln!(
                        content,
                        "\"{}\" = \"{}\"",
                        dependency.name, dependency.version
                    );
                } else {
                    let _ = write!(
                        content,
                        "\"{}\" = {{ version = \"{}\"",
                        dependency.name, dependency.version
                    );
                    if dependency.optional {
                        content.push_str(", optional = true");
                    }
                    if !dependency.features.is_empty() {
                        let features = dependency
                            .features
                            .iter()
                            .map(|feature| format!("\"{feature}\""))
                            .collect::<Vec<_>>()
                            .join(", ");
                        let _ = write!(content, ", features = [{features}]");
                    }
                    if !dependency.default_features {
                        content.push_str(", default_features = false");
                    }
                    content.push_str(" }\n");
                }
            }
        }

        if !features.is_empty() {
            content.push_str("\n[features]\n");
            for (feature_name, enabled) in features {
                let enabled = enabled
                    .iter()
                    .map(|item| format!("\"{item}\""))
                    .collect::<Vec<_>>()
                    .join(", ");
                let _ = writeln!(content, "{feature_name} = [{enabled}]");
            }
        }

        std::fs::write(dir.join("hew.toml"), content).unwrap();
    }

    // ── VersionReq parsing ──────────────────────────────────────────────

    #[test]
    fn parse_star_matches_anything() {
        let req = VersionReq::parse("*").unwrap();
        assert!(req.matches(&semver::Version::new(0, 0, 1)));
        assert!(req.matches(&semver::Version::new(1, 0, 0)));
        assert!(req.matches(&semver::Version::new(99, 99, 99)));
    }

    #[test]
    fn parse_exact_three_part() {
        let req = VersionReq::parse("1.2.3").unwrap();
        assert!(req.matches(&semver::Version::new(1, 2, 3)));
        assert!(!req.matches(&semver::Version::new(1, 2, 4)));
        assert!(!req.matches(&semver::Version::new(1, 3, 0)));
        assert!(!req.matches(&semver::Version::new(2, 0, 0)));
    }

    #[test]
    fn parse_exact_two_part_normalizes() {
        let req = VersionReq::parse("1.0").unwrap();
        assert!(req.matches(&semver::Version::new(1, 0, 0)));
        assert!(!req.matches(&semver::Version::new(1, 0, 1)));
        assert!(!req.matches(&semver::Version::new(1, 1, 0)));
    }

    #[test]
    fn parse_exact_one_part_normalizes() {
        let req = VersionReq::parse("2").unwrap();
        assert!(req.matches(&semver::Version::new(2, 0, 0)));
        assert!(!req.matches(&semver::Version::new(2, 0, 1)));
        assert!(!req.matches(&semver::Version::new(2, 1, 0)));
    }

    #[test]
    fn parse_caret_two_part() {
        let req = VersionReq::parse("^1.2").unwrap();
        assert!(req.matches(&semver::Version::new(1, 2, 0)));
        assert!(req.matches(&semver::Version::new(1, 9, 9)));
        assert!(!req.matches(&semver::Version::new(2, 0, 0)));
        assert!(!req.matches(&semver::Version::new(0, 9, 0)));
    }

    #[test]
    fn parse_caret_three_part() {
        let req = VersionReq::parse("^1.2.3").unwrap();
        assert!(req.matches(&semver::Version::new(1, 2, 3)));
        assert!(req.matches(&semver::Version::new(1, 9, 0)));
        assert!(!req.matches(&semver::Version::new(1, 2, 2)));
        assert!(!req.matches(&semver::Version::new(2, 0, 0)));
    }

    #[test]
    fn parse_tilde_two_part() {
        let req = VersionReq::parse("~1.2").unwrap();
        assert!(req.matches(&semver::Version::new(1, 2, 0)));
        assert!(req.matches(&semver::Version::new(1, 2, 9)));
        assert!(!req.matches(&semver::Version::new(1, 3, 0)));
        assert!(!req.matches(&semver::Version::new(2, 0, 0)));
    }

    #[test]
    fn parse_tilde_three_part() {
        let req = VersionReq::parse("~1.2.3").unwrap();
        assert!(req.matches(&semver::Version::new(1, 2, 3)));
        assert!(req.matches(&semver::Version::new(1, 2, 9)));
        assert!(!req.matches(&semver::Version::new(1, 3, 0)));
    }

    #[test]
    fn parse_gte() {
        let req = VersionReq::parse(">=1.0").unwrap();
        assert!(req.matches(&semver::Version::new(1, 0, 0)));
        assert!(req.matches(&semver::Version::new(2, 0, 0)));
        assert!(req.matches(&semver::Version::new(99, 0, 0)));
        assert!(!req.matches(&semver::Version::new(0, 9, 9)));
    }

    #[test]
    fn parse_gt() {
        let req = VersionReq::parse(">1.0.0").unwrap();
        assert!(!req.matches(&semver::Version::new(1, 0, 0)));
        assert!(req.matches(&semver::Version::new(1, 0, 1)));
    }

    #[test]
    fn parse_lte() {
        let req = VersionReq::parse("<=2.0.0").unwrap();
        assert!(req.matches(&semver::Version::new(2, 0, 0)));
        assert!(req.matches(&semver::Version::new(1, 0, 0)));
        assert!(!req.matches(&semver::Version::new(2, 0, 1)));
    }

    #[test]
    fn parse_eq_prefix() {
        let req = VersionReq::parse("=1.5.0").unwrap();
        assert!(req.matches(&semver::Version::new(1, 5, 0)));
        assert!(!req.matches(&semver::Version::new(1, 5, 1)));
    }

    #[test]
    fn parse_invalid_version_is_error() {
        assert!(VersionReq::parse("not-a-version").is_err());
    }

    #[test]
    fn parse_empty_string_is_error() {
        assert!(VersionReq::parse("").is_err());
    }

    // ── resolve_version ─────────────────────────────────────────────────

    #[test]
    fn resolve_exact_version() {
        let (_dir, reg) = test_registry();
        install_fake(&reg, "std.net.http", "1.0.0");
        install_fake(&reg, "std.net.http", "2.0.0");

        let version = resolve_version("std.net.http", "1.0.0", &reg).unwrap();
        assert_eq!(version, "1.0.0");
    }

    #[test]
    fn resolve_star_picks_highest() {
        let (_dir, reg) = test_registry();
        install_fake(&reg, "mypkg", "1.0.0");
        install_fake(&reg, "mypkg", "2.3.0");
        install_fake(&reg, "mypkg", "2.1.0");

        let version = resolve_version("mypkg", "*", &reg).unwrap();
        assert_eq!(version, "2.3.0");
    }

    #[test]
    fn resolve_caret_picks_highest_compatible() {
        let (_dir, reg) = test_registry();
        install_fake(&reg, "mypkg", "1.0.0");
        install_fake(&reg, "mypkg", "1.5.0");
        install_fake(&reg, "mypkg", "1.9.3");
        install_fake(&reg, "mypkg", "2.0.0");

        let version = resolve_version("mypkg", "^1.0", &reg).unwrap();
        assert_eq!(version, "1.9.3");
    }

    #[test]
    fn resolve_tilde_picks_highest_patch() {
        let (_dir, reg) = test_registry();
        install_fake(&reg, "mypkg", "1.2.0");
        install_fake(&reg, "mypkg", "1.2.5");
        install_fake(&reg, "mypkg", "1.3.0");

        let version = resolve_version("mypkg", "~1.2", &reg).unwrap();
        assert_eq!(version, "1.2.5");
    }

    #[test]
    fn resolve_gte_picks_highest() {
        let (_dir, reg) = test_registry();
        install_fake(&reg, "mypkg", "0.9.0");
        install_fake(&reg, "mypkg", "1.0.0");
        install_fake(&reg, "mypkg", "3.0.0");

        let version = resolve_version("mypkg", ">=1.0", &reg).unwrap();
        assert_eq!(version, "3.0.0");
    }

    #[test]
    fn resolve_no_matching_version() {
        let (_dir, reg) = test_registry();
        install_fake(&reg, "mypkg", "1.0.0");

        let err = resolve_version("mypkg", ">=2.0", &reg).unwrap_err();
        assert!(matches!(err, ResolveError::NoMatchingVersion { .. }));
    }

    #[test]
    fn resolve_missing_package() {
        let (_dir, reg) = test_registry();

        let err = resolve_version("nonexistent", "*", &reg).unwrap_err();
        assert!(matches!(err, ResolveError::NoMatchingVersion { .. }));
    }

    #[test]
    fn resolve_version_rejects_traversal_package_name() {
        let (_dir, reg) = test_registry();

        let err = resolve_version("evil.../../../tmp/pwned", "1.0.0", &reg).unwrap_err();
        assert!(matches!(err, ResolveError::InvalidPackageName { .. }));
        assert!(err
            .to_string()
            .contains("invalid package name `evil.../../../tmp/pwned`"));
    }

    #[test]
    fn resolve_two_part_exact() {
        let (_dir, reg) = test_registry();
        install_fake(&reg, "mypkg", "1.0.0");
        install_fake(&reg, "mypkg", "1.0.1");

        // "1.0" treated as exact "=1.0.0" — should NOT match 1.0.1.
        let version = resolve_version("mypkg", "1.0", &reg).unwrap();
        assert_eq!(version, "1.0.0");
    }

    // ── resolve_all ─────────────────────────────────────────────────────

    fn test_manifest(deps: BTreeMap<String, manifest::DepSpec>) -> HewManifest {
        HewManifest {
            package: Package {
                name: "myapp".to_string(),
                version: "0.1.0".to_string(),
                description: None,
                authors: None,
                license: None,
                keywords: None,
                categories: None,
                homepage: None,
                repository: None,
                documentation: None,
                readme: None,
                exclude: None,
                include: None,
                edition: crate::manifest::default_edition(),
                hew: None,
                main: None,
            },
            dependencies: deps,
            dev_dependencies: BTreeMap::new(),
            features: BTreeMap::new(),
            native: None,
        }
    }

    #[test]
    fn resolve_all_success() {
        let (_dir, reg) = test_registry();
        install_fake(&reg, "std.net.http", "1.0.0");
        install_fake(&reg, "std.net.http", "1.2.0");
        install_fake(&reg, "ecosystem.db.postgres", "2.0.0");

        let manifest = test_manifest(BTreeMap::from([
            (
                "std.net.http".to_string(),
                manifest::DepSpec::Version("^1.0".to_string()),
            ),
            (
                "ecosystem.db.postgres".to_string(),
                manifest::DepSpec::Version("2.0.0".to_string()),
            ),
        ]));

        let resolved = resolve_all(&manifest, &reg).unwrap();
        assert_eq!(resolved.len(), 2);
        assert_eq!(resolved["std.net.http"].version, "1.2.0");
        assert_eq!(resolved["ecosystem.db.postgres"].version, "2.0.0");
        assert_eq!(
            resolved["std.net.http"].direct_requirement.as_deref(),
            Some("^1.0")
        );
    }

    #[test]
    fn resolve_all_empty_deps() {
        let (_dir, reg) = test_registry();

        let manifest = test_manifest(BTreeMap::new());

        let resolved = resolve_all(&manifest, &reg).unwrap();
        assert!(resolved.is_empty());
    }

    #[test]
    fn resolve_all_collects_failures() {
        let (_dir, reg) = test_registry();
        install_fake(&reg, "std.net.http", "1.0.0");

        let manifest = test_manifest(BTreeMap::from([
            (
                "std.net.http".to_string(),
                manifest::DepSpec::Version("^1.0".to_string()),
            ),
            (
                "missing.one".to_string(),
                manifest::DepSpec::Version("1.0".to_string()),
            ),
            (
                "missing.two".to_string(),
                manifest::DepSpec::Version(">=2.0".to_string()),
            ),
        ]));

        let err = resolve_all(&manifest, &reg).unwrap_err();
        match err {
            ResolveError::UnresolvableDeps { failures } => {
                assert_eq!(failures.len(), 2);
                let names: Vec<&str> = failures
                    .iter()
                    .map(|failure| failure.package.as_str())
                    .collect();
                assert!(names.contains(&"missing.one"));
                assert!(names.contains(&"missing.two"));
            }
            other => panic!("expected UnresolvableDeps, got: {other}"),
        }
    }

    #[test]
    fn resolve_all_includes_transitive_dependencies() {
        let (_dir, reg) = test_registry();
        install_fake_package(
            &reg,
            "app.alpha",
            "1.0.0",
            &[FakeDep {
                name: "shared.leaf",
                version: "^1.0",
                optional: false,
                features: &[],
                default_features: true,
            }],
            &[],
        );
        install_fake_package(
            &reg,
            "app.beta",
            "1.0.0",
            &[FakeDep {
                name: "shared.leaf",
                version: "^1.0",
                optional: false,
                features: &[],
                default_features: true,
            }],
            &[],
        );
        install_fake(&reg, "shared.leaf", "1.0.0");
        install_fake(&reg, "shared.leaf", "1.4.0");

        let manifest = test_manifest(BTreeMap::from([
            (
                "app.alpha".to_string(),
                manifest::DepSpec::Version("^1.0".to_string()),
            ),
            (
                "app.beta".to_string(),
                manifest::DepSpec::Version("^1.0".to_string()),
            ),
        ]));

        let resolved = resolve_all(&manifest, &reg).unwrap();
        assert_eq!(resolved["app.alpha"].version, "1.0.0");
        assert_eq!(resolved["app.beta"].version, "1.0.0");
        assert_eq!(resolved["shared.leaf"].version, "1.4.0");
        assert!(resolved["shared.leaf"].direct_requirement.is_none());
    }

    #[test]
    fn resolve_all_rejects_traversal_transitive_dependency_name() {
        let (_dir, reg) = test_registry();
        install_fake_package(
            &reg,
            "acme.innocent",
            "1.0.0",
            &[FakeDep {
                name: "evil.../../../tmp/pwned",
                version: "1.0.0",
                optional: false,
                features: &[],
                default_features: true,
            }],
            &[],
        );

        let manifest = test_manifest(BTreeMap::from([(
            "acme.innocent".to_string(),
            manifest::DepSpec::Version("1.0.0".to_string()),
        )]));

        let err = resolve_all(&manifest, &reg).unwrap_err();
        match err {
            ResolveError::InvalidPackageName { package } => {
                assert_eq!(package, "evil.../../../tmp/pwned");
            }
            other => panic!("expected InvalidPackageName, got: {other}"),
        }
    }

    #[test]
    fn resolve_all_picks_highest_compatible_version_across_diamond() {
        let (_dir, reg) = test_registry();
        install_fake_package(
            &reg,
            "graph.left",
            "1.0.0",
            &[FakeDep {
                name: "graph.shared",
                version: "^1.0",
                optional: false,
                features: &[],
                default_features: true,
            }],
            &[],
        );
        install_fake_package(
            &reg,
            "graph.right",
            "1.0.0",
            &[FakeDep {
                name: "graph.shared",
                version: "^1.2",
                optional: false,
                features: &[],
                default_features: true,
            }],
            &[],
        );
        install_fake(&reg, "graph.shared", "1.0.0");
        install_fake(&reg, "graph.shared", "1.2.0");
        install_fake(&reg, "graph.shared", "1.8.0");
        install_fake(&reg, "graph.shared", "2.0.0");

        let manifest = test_manifest(BTreeMap::from([
            (
                "graph.left".to_string(),
                manifest::DepSpec::Version("^1.0".to_string()),
            ),
            (
                "graph.right".to_string(),
                manifest::DepSpec::Version("^1.0".to_string()),
            ),
        ]));

        let resolved = resolve_all(&manifest, &reg).unwrap();
        assert_eq!(resolved["graph.shared"].version, "1.8.0");
        assert_eq!(
            resolved["graph.shared"].requirements,
            vec!["^1.0".to_string(), "^1.2".to_string()]
        );
    }

    #[test]
    fn resolve_all_converges_when_later_constraint_lowers_version() {
        let (_dir, reg) = test_registry();
        install_fake_package(
            &reg,
            "lowering.alpha",
            "1.0.0",
            &[FakeDep {
                name: "lowering.shared",
                version: "^1.0",
                optional: false,
                features: &[],
                default_features: true,
            }],
            &[],
        );
        install_fake_package(
            &reg,
            "lowering.beta",
            "1.0.0",
            &[FakeDep {
                name: "lowering.shared",
                version: "<=1.2.0",
                optional: false,
                features: &[],
                default_features: true,
            }],
            &[],
        );
        install_fake(&reg, "lowering.shared", "1.0.0");
        install_fake(&reg, "lowering.shared", "1.2.0");
        install_fake(&reg, "lowering.shared", "1.8.0");

        let manifest = test_manifest(BTreeMap::from([
            (
                "lowering.alpha".to_string(),
                manifest::DepSpec::Version("^1.0".to_string()),
            ),
            (
                "lowering.beta".to_string(),
                manifest::DepSpec::Version("^1.0".to_string()),
            ),
        ]));

        let resolved = resolve_all(&manifest, &reg).unwrap();
        assert_eq!(resolved["lowering.shared"].version, "1.2.0");
        assert_eq!(
            resolved["lowering.shared"].requirements,
            vec!["<=1.2.0".to_string(), "^1.0".to_string()]
        );
    }

    #[test]
    fn resolve_all_reports_incompatible_version_conflicts() {
        let (_dir, reg) = test_registry();
        install_fake_package(
            &reg,
            "conflict.left",
            "1.0.0",
            &[FakeDep {
                name: "conflict.shared",
                version: "1.0.0",
                optional: false,
                features: &[],
                default_features: true,
            }],
            &[],
        );
        install_fake_package(
            &reg,
            "conflict.right",
            "1.0.0",
            &[FakeDep {
                name: "conflict.shared",
                version: "2.0.0",
                optional: false,
                features: &[],
                default_features: true,
            }],
            &[],
        );
        install_fake(&reg, "conflict.shared", "1.0.0");
        install_fake(&reg, "conflict.shared", "2.0.0");

        let manifest = test_manifest(BTreeMap::from([
            (
                "conflict.left".to_string(),
                manifest::DepSpec::Version("^1.0".to_string()),
            ),
            (
                "conflict.right".to_string(),
                manifest::DepSpec::Version("^1.0".to_string()),
            ),
        ]));

        let err = resolve_all(&manifest, &reg).unwrap_err();
        match err {
            ResolveError::UnresolvableDeps { failures } => {
                assert_eq!(failures.len(), 1);
                assert_eq!(failures[0].package, "conflict.shared");
                assert_eq!(
                    failures[0].requirements,
                    vec!["1.0.0".to_string(), "2.0.0".to_string()]
                );
            }
            other => panic!("expected UnresolvableDeps, got: {other}"),
        }
    }

    #[test]
    fn resolve_all_detects_circular_dependencies() {
        let (_dir, reg) = test_registry();
        install_fake_package(
            &reg,
            "cycle.a",
            "1.0.0",
            &[FakeDep {
                name: "cycle.b",
                version: "^1.0",
                optional: false,
                features: &[],
                default_features: true,
            }],
            &[],
        );
        install_fake_package(
            &reg,
            "cycle.b",
            "1.0.0",
            &[FakeDep {
                name: "cycle.c",
                version: "^1.0",
                optional: false,
                features: &[],
                default_features: true,
            }],
            &[],
        );
        install_fake_package(
            &reg,
            "cycle.c",
            "1.0.0",
            &[FakeDep {
                name: "cycle.a",
                version: "^1.0",
                optional: false,
                features: &[],
                default_features: true,
            }],
            &[],
        );

        let manifest = test_manifest(BTreeMap::from([(
            "cycle.a".to_string(),
            manifest::DepSpec::Version("^1.0".to_string()),
        )]));

        let err = resolve_all(&manifest, &reg).unwrap_err();
        match err {
            ResolveError::CircularDependency { cycle } => {
                assert_eq!(
                    cycle,
                    vec![
                        "cycle.a".to_string(),
                        "cycle.b".to_string(),
                        "cycle.c".to_string(),
                        "cycle.a".to_string(),
                    ]
                );
            }
            other => panic!("expected CircularDependency, got: {other}"),
        }
    }

    #[test]
    fn resolve_all_unifies_feature_requests_across_graph() {
        let (_dir, reg) = test_registry();
        install_fake(&reg, "feature.tls", "1.1.0");
        install_fake_package(
            &reg,
            "feature.core",
            "1.0.0",
            &[FakeDep {
                name: "feature.tls",
                version: "^1.0",
                optional: true,
                features: &[],
                default_features: true,
            }],
            &[("default", &["tls"]), ("tls", &["feature.tls"])],
        );

        let manifest = test_manifest(BTreeMap::from([(
            "feature.core".to_string(),
            manifest::DepSpec::Table(DepTable {
                version: "^1.0".to_string(),
                optional: None,
                features: Some(vec!["tls".to_string()]),
                default_features: Some(false),
                registry: None,
                path: None,
            }),
        )]));

        let resolved = resolve_all(&manifest, &reg).unwrap();
        assert_eq!(resolved["feature.core"].version, "1.0.0");
        assert_eq!(resolved["feature.tls"].version, "1.1.0");
    }

    #[test]
    fn confirmed_resolution_refuses_unconfirmed_cached_version() {
        let (_dir, registry) = test_registry();
        install_fake(&registry, "cached.pkg", "1.0.0");
        let registry_id = crate::config::default_registry_identity();
        let namespaced = registry.package_dir_for(&registry_id, "cached.pkg", "1.0.0");
        std::fs::create_dir_all(&namespaced).unwrap();
        std::fs::copy(
            registry.package_dir("cached.pkg", "1.0.0").join("hew.toml"),
            namespaced.join("hew.toml"),
        )
        .unwrap();
        let manifest = test_manifest(BTreeMap::from([(
            "cached.pkg".to_string(),
            DepSpec::Version("1.0.0".to_string()),
        )]));
        let confirmed = BTreeMap::new();

        let error = resolve_all_confirmed(&manifest, Path::new("."), &registry, &confirmed)
            .expect_err("unconfirmed cache entry must be unavailable");
        assert!(matches!(error, ResolveError::UnresolvableDeps { .. }));

        let confirmed = BTreeMap::from([(
            (registry_id, "cached.pkg".to_string()),
            BTreeSet::from(["1.0.0".to_string()]),
        )]);
        let resolved = resolve_all_confirmed(&manifest, Path::new("."), &registry, &confirmed)
            .expect("confirmed cache entry should resolve");
        assert_eq!(resolved["cached.pkg"].version, "1.0.0");
    }

    #[test]
    fn path_dependency_resolves_with_transitive_registry_dependency() {
        let (registry_dir, registry) = test_registry();
        install_fake(&registry, "registry.leaf", "2.0.0");
        let project = tempfile::tempdir().unwrap();
        let local = project.path().join("local");
        std::fs::create_dir(&local).unwrap();
        std::fs::write(
            local.join("hew.toml"),
            concat!(
                "[package]\n",
                "name = \"local\"\n",
                "version = \"1.2.3\"\n",
                "\n[dependencies]\n",
                "\"registry.leaf\" = \"2.0.0\"\n",
            ),
        )
        .unwrap();
        let manifest = test_manifest(BTreeMap::from([(
            "local".to_string(),
            DepSpec::Table(DepTable {
                version: "*".to_string(),
                optional: None,
                features: None,
                default_features: None,
                registry: None,
                path: Some("local".to_string()),
            }),
        )]));

        let resolved = resolve_all_from_root(&manifest, project.path(), &registry).unwrap();
        assert_eq!(resolved["local"].version, "1.2.3");
        assert_eq!(resolved["registry.leaf"].version, "2.0.0");
        assert_eq!(
            resolved["local"].source,
            PackageSource::Path(local.canonicalize().unwrap())
        );
        drop(registry_dir);
    }

    #[test]
    fn registry_manifest_rejects_every_path_dependency_spelling() {
        for (index, path) in ["relative", "../../escape", "/absolute/escape"]
            .into_iter()
            .enumerate()
        {
            let (_dir, registry) = test_registry();
            let package = registry.package_dir("registry.parent", "1.0.0");
            std::fs::create_dir_all(&package).unwrap();
            std::fs::write(
                package.join("hew.toml"),
                format!(
                    "[package]\nname = \"registry.parent\"\nversion = \"1.0.0\"\n\n[dependencies]\nevil = {{ path = {path:?} }}\n"
                ),
            )
            .unwrap();
            let manifest = test_manifest(BTreeMap::from([(
                "registry.parent".to_string(),
                DepSpec::Version("1.0.0".to_string()),
            )]));

            let error = resolve_all(&manifest, &registry)
                .expect_err(&format!("path spelling {index} must be rejected"));
            assert!(matches!(
                error,
                ResolveError::RegistryPathDependency { ref path, .. } if path
                    == ["relative", "../../escape", "/absolute/escape"][index]
            ));
        }
    }

    #[test]
    fn same_package_from_different_registry_identities_conflicts() {
        let (_dir, registry) = test_registry();
        let registry_a = "https://a.example/api/v1".to_string();
        let registry_b = "https://b.example/api/v1".to_string();
        for (identity, parent) in [(&registry_a, "left"), (&registry_b, "right")] {
            let package = registry.package_dir_for(identity, parent, "1.0.0");
            std::fs::create_dir_all(&package).unwrap();
            std::fs::write(
                package.join("hew.toml"),
                format!(
                    "[package]\nname = \"{parent}\"\nversion = \"1.0.0\"\n\n[dependencies]\nshared = \"1.0.0\"\n"
                ),
            )
            .unwrap();
        }
        let table = |registry: &str| {
            DepSpec::Table(DepTable {
                version: "1.0.0".to_string(),
                optional: None,
                features: None,
                default_features: None,
                registry: Some(registry.to_string()),
                path: None,
            })
        };
        let manifest = test_manifest(BTreeMap::from([
            ("left".to_string(), table("a")),
            ("right".to_string(), table("b")),
        ]));
        let sources = RegistrySources::new(
            crate::config::default_registry_identity(),
            None,
            BTreeMap::from([
                ("a".to_string(), registry_a.clone()),
                ("b".to_string(), registry_b.clone()),
            ]),
        );

        let error = resolve_all_from_root_with_sources(
            &manifest,
            Path::new("."),
            &registry,
            &sources,
            false,
        )
        .unwrap_err();
        assert!(matches!(
            error,
            ResolveError::ConflictingSources {
                package,
                first: PackageSource::Registry { registry: first },
                second: PackageSource::Registry { registry: second },
            } if package == "shared"
                && BTreeSet::from([first.clone(), second.clone()])
                    == BTreeSet::from([registry_a, registry_b])
        ));
    }

    #[test]
    fn missing_path_dependency_is_named() {
        let (_registry_dir, registry) = test_registry();
        let project = tempfile::tempdir().unwrap();
        let manifest = test_manifest(BTreeMap::from([(
            "missing".to_string(),
            DepSpec::Table(DepTable {
                version: "*".to_string(),
                optional: None,
                features: None,
                default_features: None,
                registry: None,
                path: Some("does-not-exist".to_string()),
            }),
        )]));

        let error = resolve_all_from_root(&manifest, project.path(), &registry).unwrap_err();
        let message = error.to_string();
        assert!(message.contains("missing"));
        assert!(message.contains("does-not-exist"));
    }

    #[test]
    fn dependency_cannot_combine_path_and_registry() {
        let (_dir, registry) = test_registry();
        let project = tempfile::tempdir().unwrap();
        let local = project.path().join("local");
        std::fs::create_dir(&local).unwrap();
        let manifest = test_manifest(BTreeMap::from([(
            "local".to_string(),
            DepSpec::Table(DepTable {
                version: "*".to_string(),
                optional: None,
                features: None,
                default_features: None,
                registry: Some("internal".to_string()),
                path: Some("local".to_string()),
            }),
        )]));

        let error = resolve_all_from_root(&manifest, project.path(), &registry).unwrap_err();
        assert!(matches!(
            error,
            ResolveError::PathAndRegistry { package } if package == "local"
        ));
    }

    // ── Display / Error impls ───────────────────────────────────────────

    #[test]
    fn error_display_invalid_req() {
        let err = VersionReq::parse("xyz").unwrap_err();
        let msg = err.to_string();
        assert!(msg.contains("invalid version requirement"));
        assert!(msg.contains("xyz"));
    }

    #[test]
    fn error_display_no_match() {
        let err = ResolveError::NoMatchingVersion {
            package: "foo".to_string(),
            requirement: "^1.0".to_string(),
        };
        let msg = err.to_string();
        assert!(msg.contains("foo"));
        assert!(msg.contains("^1.0"));
    }

    #[test]
    fn error_display_unresolvable() {
        let err = ResolveError::UnresolvableDeps {
            failures: vec![UnresolvedDep {
                package: "a".to_string(),
                requirements: vec!["1.0".to_string(), "^1.2".to_string()],
                registry: crate::config::default_registry_identity(),
                registry_selector: None,
            }],
        };
        let msg = err.to_string();
        assert!(msg.contains("unresolvable"));
        assert!(msg.contains('a'));
        assert!(msg.contains("1.0"));
        assert!(msg.contains("^1.2"));
    }

    #[test]
    fn unresolved_dependency_preserves_named_registry() {
        let (_dir, reg) = test_registry();
        let mut dependencies = BTreeMap::new();
        dependencies.insert(
            "corp.auth".to_string(),
            DepSpec::Table(DepTable {
                version: "^1.0".to_string(),
                optional: None,
                features: None,
                default_features: None,
                registry: Some("internal".to_string()),
                path: None,
            }),
        );
        let manifest = HewManifest {
            package: Package {
                name: "app".to_string(),
                version: "0.1.0".to_string(),
                description: None,
                authors: None,
                license: None,
                keywords: None,
                categories: None,
                homepage: None,
                repository: None,
                documentation: None,
                readme: None,
                exclude: None,
                include: None,
                edition: crate::manifest::default_edition(),
                hew: None,
                main: None,
            },
            dependencies,
            dev_dependencies: BTreeMap::new(),
            features: BTreeMap::new(),
            native: None,
        };

        let internal = "https://registry.internal.example/api/v1".to_string();
        let sources = RegistrySources::new(
            crate::config::default_registry_identity(),
            None,
            BTreeMap::from([("internal".to_string(), internal.clone())]),
        );
        let err =
            resolve_all_from_root_with_sources(&manifest, Path::new("."), &reg, &sources, true)
                .unwrap_err();
        let ResolveError::UnresolvableDeps { failures } = err else {
            panic!("expected unresolved dependency");
        };
        assert_eq!(failures.len(), 1);
        assert_eq!(failures[0].package, "corp.auth");
        assert_eq!(failures[0].registry, internal);
        assert_eq!(failures[0].registry_selector.as_deref(), Some("internal"));
    }

    // ── resolve_version_from_entries ─────────────────────────────────

    fn sample_entry(name: &str, vers: &str) -> IndexEntry {
        IndexEntry {
            name: name.to_string(),
            vers: vers.to_string(),
            deps: vec![],
            features: std::collections::BTreeMap::new(),
            cksum: format!("sha256:fake_{vers}"),
            sig: "ed25519:sig".to_string(),
            key_fp: "SHA256:key".to_string(),
            yanked: crate::index::YankStatus::Bool(false),
            yanked_reason: None,
            tombstoned_at: None,
            edition: None,
            hew: None,
            dl: None,
            registry_sig: None,
            registry_key_fp: None,
            published_at: None,
        }
    }

    #[test]
    fn from_entries_picks_highest_matching() {
        let entries = vec![
            sample_entry("pkg", "1.0.0"),
            sample_entry("pkg", "1.5.0"),
            sample_entry("pkg", "2.0.0"),
        ];
        let resolved = resolve_version_from_entries(&entries, "^1.0")
            .unwrap()
            .unwrap();
        assert_eq!(resolved.version, "1.5.0");
        assert_eq!(resolved.checksum, "sha256:fake_1.5.0");
    }

    #[test]
    fn from_entries_skips_yanked() {
        let mut yanked = sample_entry("pkg", "2.0.0");
        yanked.yanked = crate::index::YankStatus::Bool(true);
        let entries = vec![sample_entry("pkg", "1.0.0"), yanked];
        let resolved = resolve_version_from_entries(&entries, "*")
            .unwrap()
            .unwrap();
        assert_eq!(resolved.version, "1.0.0");
    }

    #[test]
    fn from_entries_returns_none_when_no_match() {
        let entries = vec![sample_entry("pkg", "1.0.0")];
        let result = resolve_version_from_entries(&entries, ">=2.0").unwrap();
        assert!(result.is_none());
    }

    #[test]
    fn from_entries_empty_input() {
        let result = resolve_version_from_entries(&[], "*").unwrap();
        assert!(result.is_none());
    }

    #[test]
    fn from_entries_respects_multiple_requirements() {
        let entries = vec![
            sample_entry("pkg", "1.0.0"),
            sample_entry("pkg", "1.3.0"),
            sample_entry("pkg", "1.8.0"),
            sample_entry("pkg", "2.0.0"),
        ];
        let resolved = resolve_version_from_entries_with_requirements(
            &entries,
            &["^1.0".to_string(), "^1.2".to_string()],
        )
        .unwrap()
        .unwrap();
        assert_eq!(resolved.version, "1.8.0");
    }

    #[test]
    fn from_entries_carries_sig_and_key_fp() {
        let mut entry = sample_entry("pkg", "1.0.0");
        entry.sig = "ed25519:abc123".to_string();
        entry.key_fp = "SHA256:mykey".to_string();
        entry.dl = Some("https://cdn.example.com/pkg/1.0.0.tar.zst".to_string());

        let resolved = resolve_version_from_entries(&[entry], "*")
            .unwrap()
            .unwrap();
        assert_eq!(resolved.sig, "ed25519:abc123");
        assert_eq!(resolved.key_fp, "SHA256:mykey");
        assert_eq!(
            resolved.dl.as_deref(),
            Some("https://cdn.example.com/pkg/1.0.0.tar.zst")
        );
    }
}
