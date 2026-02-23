//! Semver version resolution for Adze dependencies.
//!
//! Provides version requirement parsing with Adze-specific rules and resolution
//! of manifest dependencies against the installed package registry.

use std::collections::BTreeMap;
use std::fmt;

use crate::index::IndexEntry;
use crate::manifest::HewManifest;
use crate::registry::Registry;

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
    /// One or more dependencies could not be resolved.
    UnresolvableDeps {
        /// Each entry is `(package_name, requirement_string)`.
        failures: Vec<(String, String)>,
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
            Self::UnresolvableDeps { failures } => {
                write!(f, "unresolvable dependencies:")?;
                for (name, req) in failures {
                    write!(f, "\n  {name} {req}")?;
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
            Self::NoMatchingVersion { .. } | Self::UnresolvableDeps { .. } => None,
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
    let req = VersionReq::parse(requirement)?;

    let packages = registry.list_packages();
    let mut matching: Vec<semver::Version> = packages
        .iter()
        .filter(|p| p.name == package_name)
        .filter_map(|p| semver::Version::parse(&p.version).ok())
        .filter(|v| req.matches(v))
        .collect();

    matching.sort();

    matching
        .last()
        .map(semver::Version::to_string)
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

/// Find the highest non-yanked version from remote index entries that matches
/// a version requirement.
///
/// # Errors
///
/// Returns [`ResolveError::InvalidVersionReq`] if `requirement` cannot be parsed.
pub fn resolve_version_from_entries(
    entries: &[IndexEntry],
    requirement: &str,
) -> Result<Option<ResolvedEntry>, ResolveError> {
    let req = VersionReq::parse(requirement)?;

    let mut matching: Vec<_> = entries
        .iter()
        .filter(|e| !e.yanked.is_yanked())
        .filter_map(|e| {
            semver::Version::parse(&e.vers)
                .ok()
                .filter(|v| req.matches(v))
                .map(|v| (v, e))
        })
        .collect();

    matching.sort_by(|(va, _), (vb, _)| va.cmp(vb));

    Ok(matching.last().map(|(v, e)| ResolvedEntry {
        version: v.to_string(),
        checksum: e.cksum.clone(),
        dl: e.dl.clone(),
        sig: e.sig.clone(),
        key_fp: e.key_fp.clone(),
        registry_sig: e.registry_sig.clone(),
        published_at: e.published_at.clone(),
    }))
}

/// Resolve every dependency in `manifest` to an exact installed version.
///
/// Returns a map from package name to the resolved version string.  If any
/// dependency cannot be resolved, all failures are collected and returned
/// together in [`ResolveError::UnresolvableDeps`].
///
/// # Errors
///
/// Returns [`ResolveError::InvalidVersionReq`] if any requirement string is
/// unparseable, or [`ResolveError::UnresolvableDeps`] listing every dependency
/// that has no matching installed version.
pub fn resolve_all(
    manifest: &HewManifest,
    registry: &Registry,
) -> Result<BTreeMap<String, String>, ResolveError> {
    let mut resolved = BTreeMap::new();
    let mut failures = Vec::new();

    for (name, dep_spec) in &manifest.dependencies {
        match resolve_version(name, dep_spec.version_req(), registry) {
            Ok(version) => {
                resolved.insert(name.clone(), version);
            }
            Err(ResolveError::NoMatchingVersion { requirement, .. }) => {
                failures.push((name.clone(), requirement));
            }
            Err(e) => return Err(e),
        }
    }

    if failures.is_empty() {
        Ok(resolved)
    } else {
        Err(ResolveError::UnresolvableDeps { failures })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::manifest::{self, Package};

    /// Create a temporary registry directory and `Registry` handle.
    ///
    /// The returned `TempDir` must be kept alive for the registry to remain
    /// valid.
    fn test_registry() -> (tempfile::TempDir, Registry) {
        let dir = tempfile::tempdir().unwrap();
        let reg = Registry::with_root(dir.path().to_path_buf());
        (dir, reg)
    }

    /// Install a fake package version in the registry (creates the directory
    /// tree and a minimal `hew.toml`).
    fn install_fake(registry: &Registry, name: &str, version: &str) {
        let dir = registry.package_dir(name, version);
        std::fs::create_dir_all(&dir).unwrap();
        std::fs::write(
            dir.join("hew.toml"),
            format!("[package]\nname = \"{name}\"\nversion = \"{version}\"\n"),
        )
        .unwrap();
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

        let v = resolve_version("std::net::http", "1.0.0", &reg).unwrap();
        assert_eq!(v, "1.0.0");
    }

    #[test]
    fn resolve_star_picks_highest() {
        let (_dir, reg) = test_registry();
        install_fake(&reg, "mypkg", "1.0.0");
        install_fake(&reg, "mypkg", "2.3.0");
        install_fake(&reg, "mypkg", "2.1.0");

        let v = resolve_version("mypkg", "*", &reg).unwrap();
        assert_eq!(v, "2.3.0");
    }

    #[test]
    fn resolve_caret_picks_highest_compatible() {
        let (_dir, reg) = test_registry();
        install_fake(&reg, "mypkg", "1.0.0");
        install_fake(&reg, "mypkg", "1.5.0");
        install_fake(&reg, "mypkg", "1.9.3");
        install_fake(&reg, "mypkg", "2.0.0");

        let v = resolve_version("mypkg", "^1.0", &reg).unwrap();
        assert_eq!(v, "1.9.3");
    }

    #[test]
    fn resolve_tilde_picks_highest_patch() {
        let (_dir, reg) = test_registry();
        install_fake(&reg, "mypkg", "1.2.0");
        install_fake(&reg, "mypkg", "1.2.5");
        install_fake(&reg, "mypkg", "1.3.0");

        let v = resolve_version("mypkg", "~1.2", &reg).unwrap();
        assert_eq!(v, "1.2.5");
    }

    #[test]
    fn resolve_gte_picks_highest() {
        let (_dir, reg) = test_registry();
        install_fake(&reg, "mypkg", "0.9.0");
        install_fake(&reg, "mypkg", "1.0.0");
        install_fake(&reg, "mypkg", "3.0.0");

        let v = resolve_version("mypkg", ">=1.0", &reg).unwrap();
        assert_eq!(v, "3.0.0");
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

        // "1.0" treated as exact "=1.0.0" — should NOT match 1.0.1
        let v = resolve_version("mypkg", "1.0", &reg).unwrap();
        assert_eq!(v, "1.0.0");
    }

    // ── resolve_all ─────────────────────────────────────────────────────

    fn test_manifest(deps: BTreeMap<String, String>) -> HewManifest {
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
            dependencies: deps
                .into_iter()
                .map(|(k, v)| (k, manifest::DepSpec::Version(v)))
                .collect(),
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
            ("std::net::http".to_string(), "^1.0".to_string()),
            ("ecosystem::db::postgres".to_string(), "2.0.0".to_string()),
        ]));

        let resolved = resolve_all(&manifest, &reg).unwrap();
        assert_eq!(resolved.len(), 2);
        assert_eq!(resolved["std::net::http"], "1.2.0");
        assert_eq!(resolved["ecosystem::db::postgres"], "2.0.0");
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
            ("std::net::http".to_string(), "^1.0".to_string()),
            ("missing::one".to_string(), "1.0".to_string()),
            ("missing::two".to_string(), ">=2.0".to_string()),
        ]));

        let err = resolve_all(&manifest, &reg).unwrap_err();
        match err {
            ResolveError::UnresolvableDeps { failures } => {
                assert_eq!(failures.len(), 2);
                let names: Vec<&str> = failures.iter().map(|(n, _)| n.as_str()).collect();
                assert!(names.contains(&"missing::one"));
                assert!(names.contains(&"missing::two"));
            }
            other => panic!("expected UnresolvableDeps, got: {other}"),
        }
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
            failures: vec![("a".to_string(), "1.0".to_string())],
        };
        let msg = err.to_string();
        assert!(msg.contains("unresolvable"));
        assert!(msg.contains("a"));
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
