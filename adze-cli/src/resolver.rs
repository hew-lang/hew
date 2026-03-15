//! Semver version resolution for Adze dependencies.
//!
//! Provides version requirement parsing with Adze-specific rules and resolution
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

use crate::index::IndexEntry;
use crate::manifest::{self, DepSpec, HewManifest};
use crate::registry::Registry;

/// A dependency that could not be resolved locally.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnresolvedDep {
    /// Fully qualified package name.
    pub package: String,
    /// Every version requirement currently imposed on the package.
    pub requirements: Vec<String>,
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
}

impl fmt::Display for ResolveError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::InvalidVersionReq { input, source } => {
                write!(f, "invalid version requirement `{input}`: {source}")
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
        }
    }
}

impl std::error::Error for ResolveError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::InvalidVersionReq { source, .. } => Some(source),
            Self::ManifestRead { source, .. } => Some(source),
            Self::NoMatchingVersion { .. }
            | Self::CircularDependency { .. }
            | Self::UnresolvableDeps { .. } => None,
        }
    }
}

/// A parsed semver version requirement.
///
/// Wraps [`semver::VersionReq`] with Adze-specific parsing rules:
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

#[derive(Debug, Clone, Default)]
struct PackageState {
    requirements: BTreeSet<String>,
    direct_requirement: Option<String>,
    requested_features: BTreeSet<String>,
    use_default_features: bool,
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
}

#[derive(Debug, Clone, Default)]
struct ActiveFeatures {
    enabled_optional_dependencies: BTreeSet<String>,
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
    available_versions: BTreeMap<String, Vec<semver::Version>>,
    package_states: BTreeMap<String, PackageState>,
    selected_versions: BTreeMap<String, String>,
    expanded_states: BTreeMap<String, ExpandedState>,
    failures: BTreeMap<String, BTreeSet<String>>,
    manifest_cache: BTreeMap<(String, String), HewManifest>,
}

impl<'a> ResolverPass<'a> {
    fn with_seed(registry: &'a Registry, package_states: BTreeMap<String, PackageState>) -> Self {
        Self {
            registry,
            available_versions: available_versions(registry),
            package_states,
            selected_versions: BTreeMap::new(),
            expanded_states: BTreeMap::new(),
            failures: BTreeMap::new(),
            manifest_cache: BTreeMap::new(),
        }
    }

    fn resolve_manifest(mut self, manifest: &HewManifest) -> Result<PassOutcome, ResolveError> {
        for request in root_requests(manifest) {
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
                        },
                    )
                })
                .collect();
            Ok(PassOutcome::Resolved(resolved))
        } else {
            let failures = self
                .failures
                .into_iter()
                .map(|(package, requirements)| UnresolvedDep {
                    package,
                    requirements: requirements.into_iter().collect(),
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
        if let Some(index) = path.iter().position(|node| node == &request.name) {
            let mut cycle = path[index..].to_vec();
            cycle.push(request.name.clone());
            return Err(ResolveError::CircularDependency { cycle });
        }

        let (requirements, requested_features, use_default_features) = {
            let state = self.package_states.entry(request.name.clone()).or_default();
            state.requirements.insert(request.requirement.clone());
            if state.direct_requirement.is_none() {
                state
                    .direct_requirement
                    .clone_from(&request.direct_requirement);
            }
            state.requested_features.extend(request.features);
            state.use_default_features |= request.use_default_features;
            (
                state.requirements.iter().cloned().collect::<Vec<_>>(),
                state.requested_features.clone(),
                state.use_default_features,
            )
        };

        let Some(version) = select_highest_matching_version(
            &request.name,
            &requirements,
            &self.available_versions,
        )?
        else {
            self.failures
                .entry(request.name)
                .or_default()
                .extend(requirements);
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
            let manifest = self.load_manifest(&request.name, &version)?;
            dependency_requests_from_manifest(manifest, &state_snapshot)
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

    fn load_manifest(
        &mut self,
        package: &str,
        version: &str,
    ) -> Result<&HewManifest, ResolveError> {
        let key = (package.to_string(), version.to_string());
        if !self.manifest_cache.contains_key(&key) {
            let manifest_path = self.registry.package_dir(package, version).join("hew.toml");
            let manifest = manifest::parse_manifest(&manifest_path).map_err(|source| {
                ResolveError::ManifestRead {
                    package: package.to_string(),
                    version: version.to_string(),
                    source,
                }
            })?;
            self.manifest_cache.insert(key.clone(), manifest);
        }
        Ok(self
            .manifest_cache
            .get(&key)
            .expect("manifest cache entry must exist after insertion"))
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

fn available_versions(registry: &Registry) -> BTreeMap<String, Vec<semver::Version>> {
    let mut versions = BTreeMap::<String, Vec<semver::Version>>::new();
    for package in registry.list_packages() {
        if let Ok(version) = semver::Version::parse(&package.version) {
            versions.entry(package.name).or_default().push(version);
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
    package_name: &str,
    requirements: &[String],
    versions_by_package: &BTreeMap<String, Vec<semver::Version>>,
) -> Result<Option<String>, ResolveError> {
    let reqs = parse_requirements(requirements)?;
    Ok(versions_by_package
        .get(package_name)
        .and_then(|versions| {
            versions
                .iter()
                .rev()
                .find(|version| reqs.iter().all(|req| req.matches(version)))
        })
        .map(ToString::to_string))
}

fn root_requests(manifest: &HewManifest) -> Vec<DepRequest> {
    manifest
        .dependencies
        .iter()
        .map(|(name, dep_spec)| DepRequest {
            name: name.clone(),
            requirement: dep_spec.version_req().to_string(),
            direct_requirement: Some(dep_spec.version_req().to_string()),
            features: requested_features(dep_spec),
            use_default_features: uses_default_features(dep_spec),
        })
        .collect()
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

fn dependency_requests_from_manifest(
    manifest: &HewManifest,
    state: &PackageState,
) -> Vec<DepRequest> {
    let active_features = resolve_active_features(manifest, state);

    manifest
        .dependencies
        .iter()
        .filter_map(|(name, dep_spec)| {
            if dependency_is_optional(dep_spec)
                && !active_features.enabled_optional_dependencies.contains(name)
            {
                return None;
            }

            Some(DepRequest {
                name: name.clone(),
                requirement: dep_spec.version_req().to_string(),
                direct_requirement: None,
                features: requested_features(dep_spec),
                use_default_features: uses_default_features(dep_spec),
            })
        })
        .collect()
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
    let requirements = vec![requirement.to_string()];
    select_highest_matching_version(package_name, &requirements, &available_versions(registry))?
        .ok_or_else(|| ResolveError::NoMatchingVersion {
            package: package_name.to_string(),
            requirement: requirement.to_string(),
        })
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
/// Resolution is greedy: for each package, Adze picks the highest locally
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
    let mut seed_states = BTreeMap::new();
    loop {
        let pass = ResolverPass::with_seed(registry, seed_states);
        match pass.resolve_manifest(manifest)? {
            PassOutcome::Resolved(resolved) => return Ok(resolved),
            PassOutcome::Restart(next_seed_states) => {
                seed_states = next_seed_states;
            }
            PassOutcome::Unresolved(failures) => {
                return Err(ResolveError::UnresolvableDeps { failures });
            }
        }
    }
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
        install_fake(&reg, "std::net::http", "1.0.0");
        install_fake(&reg, "std::net::http", "2.0.0");

        let version = resolve_version("std::net::http", "1.0.0", &reg).unwrap();
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
                edition: None,
                hew: None,
            },
            dependencies: deps,
            dev_dependencies: BTreeMap::new(),
            features: BTreeMap::new(),
        }
    }

    #[test]
    fn resolve_all_success() {
        let (_dir, reg) = test_registry();
        install_fake(&reg, "std::net::http", "1.0.0");
        install_fake(&reg, "std::net::http", "1.2.0");
        install_fake(&reg, "ecosystem::db::postgres", "2.0.0");

        let manifest = test_manifest(BTreeMap::from([
            (
                "std::net::http".to_string(),
                manifest::DepSpec::Version("^1.0".to_string()),
            ),
            (
                "ecosystem::db::postgres".to_string(),
                manifest::DepSpec::Version("2.0.0".to_string()),
            ),
        ]));

        let resolved = resolve_all(&manifest, &reg).unwrap();
        assert_eq!(resolved.len(), 2);
        assert_eq!(resolved["std::net::http"].version, "1.2.0");
        assert_eq!(resolved["ecosystem::db::postgres"].version, "2.0.0");
        assert_eq!(
            resolved["std::net::http"].direct_requirement.as_deref(),
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
        install_fake(&reg, "std::net::http", "1.0.0");

        let manifest = test_manifest(BTreeMap::from([
            (
                "std::net::http".to_string(),
                manifest::DepSpec::Version("^1.0".to_string()),
            ),
            (
                "missing::one".to_string(),
                manifest::DepSpec::Version("1.0".to_string()),
            ),
            (
                "missing::two".to_string(),
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
                assert!(names.contains(&"missing::one"));
                assert!(names.contains(&"missing::two"));
            }
            other => panic!("expected UnresolvableDeps, got: {other}"),
        }
    }

    #[test]
    fn resolve_all_includes_transitive_dependencies() {
        let (_dir, reg) = test_registry();
        install_fake_package(
            &reg,
            "app::alpha",
            "1.0.0",
            &[FakeDep {
                name: "shared::leaf",
                version: "^1.0",
                optional: false,
                features: &[],
                default_features: true,
            }],
            &[],
        );
        install_fake_package(
            &reg,
            "app::beta",
            "1.0.0",
            &[FakeDep {
                name: "shared::leaf",
                version: "^1.0",
                optional: false,
                features: &[],
                default_features: true,
            }],
            &[],
        );
        install_fake(&reg, "shared::leaf", "1.0.0");
        install_fake(&reg, "shared::leaf", "1.4.0");

        let manifest = test_manifest(BTreeMap::from([
            (
                "app::alpha".to_string(),
                manifest::DepSpec::Version("^1.0".to_string()),
            ),
            (
                "app::beta".to_string(),
                manifest::DepSpec::Version("^1.0".to_string()),
            ),
        ]));

        let resolved = resolve_all(&manifest, &reg).unwrap();
        assert_eq!(resolved["app::alpha"].version, "1.0.0");
        assert_eq!(resolved["app::beta"].version, "1.0.0");
        assert_eq!(resolved["shared::leaf"].version, "1.4.0");
        assert!(resolved["shared::leaf"].direct_requirement.is_none());
    }

    #[test]
    fn resolve_all_picks_highest_compatible_version_across_diamond() {
        let (_dir, reg) = test_registry();
        install_fake_package(
            &reg,
            "graph::left",
            "1.0.0",
            &[FakeDep {
                name: "graph::shared",
                version: "^1.0",
                optional: false,
                features: &[],
                default_features: true,
            }],
            &[],
        );
        install_fake_package(
            &reg,
            "graph::right",
            "1.0.0",
            &[FakeDep {
                name: "graph::shared",
                version: "^1.2",
                optional: false,
                features: &[],
                default_features: true,
            }],
            &[],
        );
        install_fake(&reg, "graph::shared", "1.0.0");
        install_fake(&reg, "graph::shared", "1.2.0");
        install_fake(&reg, "graph::shared", "1.8.0");
        install_fake(&reg, "graph::shared", "2.0.0");

        let manifest = test_manifest(BTreeMap::from([
            (
                "graph::left".to_string(),
                manifest::DepSpec::Version("^1.0".to_string()),
            ),
            (
                "graph::right".to_string(),
                manifest::DepSpec::Version("^1.0".to_string()),
            ),
        ]));

        let resolved = resolve_all(&manifest, &reg).unwrap();
        assert_eq!(resolved["graph::shared"].version, "1.8.0");
        assert_eq!(
            resolved["graph::shared"].requirements,
            vec!["^1.0".to_string(), "^1.2".to_string()]
        );
    }

    #[test]
    fn resolve_all_converges_when_later_constraint_lowers_version() {
        let (_dir, reg) = test_registry();
        install_fake_package(
            &reg,
            "lowering::alpha",
            "1.0.0",
            &[FakeDep {
                name: "lowering::shared",
                version: "^1.0",
                optional: false,
                features: &[],
                default_features: true,
            }],
            &[],
        );
        install_fake_package(
            &reg,
            "lowering::beta",
            "1.0.0",
            &[FakeDep {
                name: "lowering::shared",
                version: "<=1.2.0",
                optional: false,
                features: &[],
                default_features: true,
            }],
            &[],
        );
        install_fake(&reg, "lowering::shared", "1.0.0");
        install_fake(&reg, "lowering::shared", "1.2.0");
        install_fake(&reg, "lowering::shared", "1.8.0");

        let manifest = test_manifest(BTreeMap::from([
            (
                "lowering::alpha".to_string(),
                manifest::DepSpec::Version("^1.0".to_string()),
            ),
            (
                "lowering::beta".to_string(),
                manifest::DepSpec::Version("^1.0".to_string()),
            ),
        ]));

        let resolved = resolve_all(&manifest, &reg).unwrap();
        assert_eq!(resolved["lowering::shared"].version, "1.2.0");
        assert_eq!(
            resolved["lowering::shared"].requirements,
            vec!["<=1.2.0".to_string(), "^1.0".to_string()]
        );
    }

    #[test]
    fn resolve_all_reports_incompatible_version_conflicts() {
        let (_dir, reg) = test_registry();
        install_fake_package(
            &reg,
            "conflict::left",
            "1.0.0",
            &[FakeDep {
                name: "conflict::shared",
                version: "1.0.0",
                optional: false,
                features: &[],
                default_features: true,
            }],
            &[],
        );
        install_fake_package(
            &reg,
            "conflict::right",
            "1.0.0",
            &[FakeDep {
                name: "conflict::shared",
                version: "2.0.0",
                optional: false,
                features: &[],
                default_features: true,
            }],
            &[],
        );
        install_fake(&reg, "conflict::shared", "1.0.0");
        install_fake(&reg, "conflict::shared", "2.0.0");

        let manifest = test_manifest(BTreeMap::from([
            (
                "conflict::left".to_string(),
                manifest::DepSpec::Version("^1.0".to_string()),
            ),
            (
                "conflict::right".to_string(),
                manifest::DepSpec::Version("^1.0".to_string()),
            ),
        ]));

        let err = resolve_all(&manifest, &reg).unwrap_err();
        match err {
            ResolveError::UnresolvableDeps { failures } => {
                assert_eq!(failures.len(), 1);
                assert_eq!(failures[0].package, "conflict::shared");
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
            "cycle::a",
            "1.0.0",
            &[FakeDep {
                name: "cycle::b",
                version: "^1.0",
                optional: false,
                features: &[],
                default_features: true,
            }],
            &[],
        );
        install_fake_package(
            &reg,
            "cycle::b",
            "1.0.0",
            &[FakeDep {
                name: "cycle::c",
                version: "^1.0",
                optional: false,
                features: &[],
                default_features: true,
            }],
            &[],
        );
        install_fake_package(
            &reg,
            "cycle::c",
            "1.0.0",
            &[FakeDep {
                name: "cycle::a",
                version: "^1.0",
                optional: false,
                features: &[],
                default_features: true,
            }],
            &[],
        );

        let manifest = test_manifest(BTreeMap::from([(
            "cycle::a".to_string(),
            manifest::DepSpec::Version("^1.0".to_string()),
        )]));

        let err = resolve_all(&manifest, &reg).unwrap_err();
        match err {
            ResolveError::CircularDependency { cycle } => {
                assert_eq!(
                    cycle,
                    vec![
                        "cycle::a".to_string(),
                        "cycle::b".to_string(),
                        "cycle::c".to_string(),
                        "cycle::a".to_string(),
                    ]
                );
            }
            other => panic!("expected CircularDependency, got: {other}"),
        }
    }

    #[test]
    fn resolve_all_unifies_feature_requests_across_graph() {
        let (_dir, reg) = test_registry();
        install_fake(&reg, "feature::tls", "1.1.0");
        install_fake_package(
            &reg,
            "feature::core",
            "1.0.0",
            &[FakeDep {
                name: "feature::tls",
                version: "^1.0",
                optional: true,
                features: &[],
                default_features: true,
            }],
            &[("default", &["tls"]), ("tls", &["feature::tls"])],
        );

        let manifest = test_manifest(BTreeMap::from([(
            "feature::core".to_string(),
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
        assert_eq!(resolved["feature::core"].version, "1.0.0");
        assert_eq!(resolved["feature::tls"].version, "1.1.0");
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
            }],
        };
        let msg = err.to_string();
        assert!(msg.contains("unresolvable"));
        assert!(msg.contains('a'));
        assert!(msg.contains("1.0"));
        assert!(msg.contains("^1.2"));
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
