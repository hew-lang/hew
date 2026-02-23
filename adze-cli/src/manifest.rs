//! `hew.toml` manifest parsing for the Hew package manager.

use std::collections::BTreeMap;
use std::fmt;
use std::path::Path;

use serde::{Deserialize, Serialize};

// ── Dependency specification ────────────────────────────────────────────────

/// A dependency can be specified as a bare version string or as a table with
/// additional options.
#[derive(Debug, Clone, Deserialize, Serialize, PartialEq, Eq)]
#[serde(untagged)]
pub enum DepSpec {
    /// Shorthand: `"^1.0"`.
    Version(String),
    /// Table: `{ version = "^1.0", optional = true, features = ["tls"] }`.
    Table(DepTable),
}

impl PartialEq<&str> for DepSpec {
    fn eq(&self, other: &&str) -> bool {
        match self {
            Self::Version(v) => v == *other,
            Self::Table(t) => t.version == *other,
        }
    }
}

impl DepSpec {
    /// Return the version requirement string regardless of form.
    #[must_use]
    pub fn version_req(&self) -> &str {
        match self {
            Self::Version(v) => v,
            Self::Table(t) => &t.version,
        }
    }
}

impl fmt::Display for DepSpec {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.version_req())
    }
}

/// Table form of a dependency specification.
#[derive(Debug, Clone, Deserialize, Serialize, PartialEq, Eq)]
pub struct DepTable {
    /// Semver version requirement (required).
    pub version: String,
    /// If `true`, only included when a feature enables it.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub optional: Option<bool>,
    /// Features to activate on this dependency.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub features: Option<Vec<String>>,
    /// Whether to include the dependency's default features (default: `true`).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub default_features: Option<bool>,
    /// Non-default registry name for this dependency.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub registry: Option<String>,
    /// Local path dependency (not publishable).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub path: Option<String>,
}

/// Errors that can occur when reading, parsing, or writing a `hew.toml` manifest.
#[derive(Debug)]
pub enum ManifestError {
    Io(std::io::Error),
    Parse(toml::de::Error),
    Serialize(toml::ser::Error),
}

impl fmt::Display for ManifestError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Io(e) => write!(f, "cannot read manifest: {e}"),
            Self::Parse(e) => write!(f, "invalid manifest: {e}"),
            Self::Serialize(e) => write!(f, "cannot serialize manifest: {e}"),
        }
    }
}

impl std::error::Error for ManifestError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::Io(e) => Some(e),
            Self::Parse(e) => Some(e),
            Self::Serialize(e) => Some(e),
        }
    }
}

impl From<std::io::Error> for ManifestError {
    fn from(e: std::io::Error) -> Self {
        Self::Io(e)
    }
}

impl From<toml::de::Error> for ManifestError {
    fn from(e: toml::de::Error) -> Self {
        Self::Parse(e)
    }
}

impl From<toml::ser::Error> for ManifestError {
    fn from(e: toml::ser::Error) -> Self {
        Self::Serialize(e)
    }
}

/// The `[package]` section of a `hew.toml` manifest.
#[derive(Debug, Deserialize, Serialize)]
pub struct Package {
    /// Package name (required).
    pub name: String,
    /// Semver version string (required).
    pub version: String,
    /// Optional human-readable description.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
    /// Package authors (e.g. `["Alice <alice@example.com>"]`).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub authors: Option<Vec<String>>,
    /// SPDX license identifier (e.g. `"MIT"` or `"Apache-2.0"`).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub license: Option<String>,
    /// Search keywords for registry discovery (max 5).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub keywords: Option<Vec<String>>,
    /// Categories from the fixed taxonomy.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub categories: Option<Vec<String>>,
    /// Project homepage URL.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub homepage: Option<String>,
    /// Source repository URL.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub repository: Option<String>,
    /// Documentation URL.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub documentation: Option<String>,
    /// Path to the README file.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub readme: Option<String>,
    /// Glob patterns to exclude from publishing.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub exclude: Option<Vec<String>>,
    /// Glob patterns to include when publishing (overrides exclude).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub include: Option<Vec<String>>,
    /// Hew language edition (e.g. `"2026"`).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub edition: Option<String>,
    /// Minimum Hew compiler version required (e.g. `">=0.8.0"`).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub hew: Option<String>,
}

/// Template kind used when generating a default `hew.toml`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ManifestTemplate {
    /// Binary with a `main.hew` entry point.
    Bin,
    /// Library package.
    Lib,
    /// Actor-based project.
    Actor,
}

/// A parsed `hew.toml` manifest.
#[derive(Debug, Deserialize, Serialize)]
pub struct HewManifest {
    /// `[package]` metadata.
    pub package: Package,
    /// `[dependencies]` map from module path to dependency specification.
    #[serde(default)]
    pub dependencies: BTreeMap<String, DepSpec>,
    /// `[dev-dependencies]` — not pulled transitively.
    #[serde(
        default,
        rename = "dev-dependencies",
        skip_serializing_if = "BTreeMap::is_empty"
    )]
    pub dev_dependencies: BTreeMap<String, DepSpec>,
    /// `[features]` — named feature flags and their implications.
    #[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
    pub features: BTreeMap<String, Vec<String>>,
}

impl HewManifest {
    /// Returns a one-line human-readable summary of this manifest.
    #[must_use]
    pub fn summary(&self) -> String {
        use std::fmt::Write as _;
        let mut s = format!("{} v{}", self.package.name, self.package.version);
        if let Some(desc) = &self.package.description {
            let _ = write!(s, ": {desc}");
        }
        if let Some(authors) = &self.package.authors {
            if !authors.is_empty() {
                let _ = write!(s, " by {}", authors.join(", "));
            }
        }
        if let Some(license) = &self.package.license {
            let _ = write!(s, " [{license}]");
        }
        if !self.dependencies.is_empty() {
            let n = self.dependencies.len();
            let _ = write!(s, " ({n} dep{})", if n == 1 { "" } else { "s" });
        }
        s
    }
}

/// Parse a `hew.toml` manifest at `path`.
///
/// # Errors
///
/// Returns [`ManifestError`] when the file cannot be read or its TOML is
/// malformed / missing required fields.
pub fn parse_manifest(path: &Path) -> Result<HewManifest, ManifestError> {
    let text = std::fs::read_to_string(path)?;
    let manifest: HewManifest = toml::from_str(&text)?;
    Ok(manifest)
}

/// Write a default `hew.toml` for a new project named `name` to `path`.
///
/// # Errors
///
/// Returns [`ManifestError`] when the file cannot be written.
#[cfg(test)]
pub fn write_default_manifest(path: &Path, name: &str) -> Result<(), ManifestError> {
    write_manifest_with_template(path, name, ManifestTemplate::Bin)
}

/// Write a `hew.toml` for a new project named `name` using the given template.
///
/// # Errors
///
/// Returns [`ManifestError`] when the file cannot be written.
pub fn write_manifest_with_template(
    path: &Path,
    name: &str,
    template: ManifestTemplate,
) -> Result<(), ManifestError> {
    let description = match template {
        ManifestTemplate::Bin => "A Hew binary project",
        ManifestTemplate::Lib => "A Hew library",
        ManifestTemplate::Actor => "A Hew actor project",
    };
    let content = format!(
        "[package]\nname = \"{name}\"\nversion = \"0.1.0\"\ndescription = \"{description}\"\n\n[dependencies]\n"
    );
    std::fs::write(path, content)?;
    Ok(())
}

/// Serialize `manifest` and write it to `path`, overwriting any existing file.
///
/// # Errors
///
/// Returns [`ManifestError`] when the manifest cannot be serialized or the file
/// cannot be written.
pub fn save_manifest(path: &Path, manifest: &HewManifest) -> Result<(), ManifestError> {
    let content = toml::to_string(manifest)?;
    std::fs::write(path, content)?;
    Ok(())
}

/// Add or update a dependency in the `hew.toml` at `path`.
///
/// Reads the existing manifest, inserts `name = version` into `[dependencies]`,
/// then writes the manifest back.
///
/// # Errors
///
/// Returns [`ManifestError`] when the manifest cannot be read, parsed,
/// serialized, or written.
pub fn add_dependency(path: &Path, name: &str, version: &str) -> Result<(), ManifestError> {
    let mut manifest = parse_manifest(path)?;
    manifest
        .dependencies
        .insert(name.to_string(), DepSpec::Version(version.to_string()));
    save_manifest(path, &manifest)
}

/// Validate that a manifest has all required fields for publishing.
///
/// Returns a list of missing field names. An empty list means the manifest is
/// ready to publish.
#[must_use]
pub fn validate_for_publish(manifest: &HewManifest) -> Vec<&'static str> {
    let mut missing = Vec::new();
    if manifest.package.name.is_empty() {
        missing.push("name");
    }
    if manifest.package.version.is_empty() {
        missing.push("version");
    }
    if manifest
        .package
        .description
        .as_ref()
        .is_none_or(String::is_empty)
    {
        missing.push("description");
    }
    if manifest
        .package
        .license
        .as_ref()
        .is_none_or(String::is_empty)
    {
        missing.push("license");
    }
    if manifest.package.authors.as_ref().is_none_or(Vec::is_empty) {
        missing.push("authors");
    }
    // Path dependencies are not publishable.
    for (name, spec) in &manifest.dependencies {
        if matches!(spec, DepSpec::Table(t) if t.path.is_some()) {
            missing.push("dependencies: path deps not publishable");
            let _ = name; // used for error context in future
            break;
        }
    }
    missing
}

/// Remove a dependency from the `hew.toml` at `path`.
///
/// Returns `true` if the dependency was present and removed, `false` if it
/// was not found.
///
/// # Errors
///
/// Returns [`ManifestError`] when the manifest cannot be read, parsed,
/// serialized, or written.
pub fn remove_dependency(path: &Path, name: &str) -> Result<bool, ManifestError> {
    let mut manifest = parse_manifest(path)?;
    let removed = manifest.dependencies.remove(name).is_some();
    if removed {
        save_manifest(path, &manifest)?;
    }
    Ok(removed)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write as _;

    fn write_temp(content: &str) -> tempfile::NamedTempFile {
        let mut f = tempfile::NamedTempFile::new().unwrap();
        f.write_all(content.as_bytes()).unwrap();
        f
    }

    #[test]
    fn parse_minimal_manifest() {
        let f = write_temp("[package]\nname = \"myproject\"\nversion = \"0.1.0\"\n");
        let m = parse_manifest(f.path()).unwrap();
        assert_eq!(m.package.name, "myproject");
        assert_eq!(m.package.version, "0.1.0");
        assert!(m.package.description.is_none());
        assert!(m.dependencies.is_empty());
    }

    #[test]
    fn parse_full_manifest() {
        let f = write_temp(
            "[package]\nname = \"myproject\"\nversion = \"0.1.0\"\ndescription = \"An example Hew project\"\n\n[dependencies]\n\"std::net::http\" = \"1.0\"\n\"ecosystem::db::postgres\" = \"1.0\"\n\"myorg::router\" = \"0.5\"\n",
        );
        let m = parse_manifest(f.path()).unwrap();
        assert_eq!(m.package.name, "myproject");
        assert_eq!(
            m.package.description.as_deref(),
            Some("An example Hew project")
        );
        assert_eq!(m.dependencies["std::net::http"].version_req(), "1.0");
        assert_eq!(
            m.dependencies["ecosystem::db::postgres"].version_req(),
            "1.0"
        );
        assert_eq!(m.dependencies["myorg::router"].version_req(), "0.5");
    }

    #[test]
    fn missing_required_field_is_error() {
        let f = write_temp("[package]\nname = \"missingversion\"\n");
        assert!(parse_manifest(f.path()).is_err());
    }

    #[test]
    fn missing_file_is_error() {
        let path = std::path::Path::new("/tmp/nonexistent_hew_manifest_xyz_adze.toml");
        assert!(parse_manifest(path).is_err());
    }

    #[test]
    fn write_and_read_default_manifest() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("hew.toml");
        write_default_manifest(&path, "testproject").unwrap();
        let m = parse_manifest(&path).unwrap();
        assert_eq!(m.package.name, "testproject");
        assert_eq!(m.package.version, "0.1.0");
        assert!(m.dependencies.is_empty());
    }

    #[test]
    fn add_dependency_to_manifest() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("hew.toml");
        write_default_manifest(&path, "myproject").unwrap();
        add_dependency(&path, "ecosystem::db::postgres", "1.0").unwrap();
        let m = parse_manifest(&path).unwrap();
        assert_eq!(
            m.dependencies["ecosystem::db::postgres"].version_req(),
            "1.0"
        );
    }

    #[test]
    fn add_dependency_updates_existing() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("hew.toml");
        write_default_manifest(&path, "myproject").unwrap();
        add_dependency(&path, "std::net::http", "1.0").unwrap();
        add_dependency(&path, "std::net::http", "2.0").unwrap();
        let m = parse_manifest(&path).unwrap();
        assert_eq!(m.dependencies["std::net::http"].version_req(), "2.0");
        assert_eq!(m.dependencies.len(), 1);
    }

    #[test]
    fn parse_manifest_with_all_new_fields() {
        let f = write_temp(concat!(
            "[package]\n",
            "name = \"fullpkg\"\n",
            "version = \"1.2.3\"\n",
            "description = \"Full metadata\"\n",
            "authors = [\"Alice <alice@example.com>\", \"Bob\"]\n",
            "license = \"MIT\"\n",
            "keywords = [\"actor\", \"concurrency\"]\n",
            "homepage = \"https://example.com\"\n",
            "repository = \"https://github.com/example/fullpkg\"\n",
            "readme = \"README.md\"\n",
            "exclude = [\"tests/*\", \".github/*\"]\n",
            "\n[dependencies]\n",
        ));
        let m = parse_manifest(f.path()).unwrap();
        assert_eq!(m.package.name, "fullpkg");
        assert_eq!(m.package.version, "1.2.3");
        assert_eq!(
            m.package.authors.as_deref().unwrap(),
            &["Alice <alice@example.com>", "Bob"]
        );
        assert_eq!(m.package.license.as_deref(), Some("MIT"));
        assert_eq!(
            m.package.keywords.as_deref().unwrap(),
            &["actor", "concurrency"]
        );
        assert_eq!(m.package.homepage.as_deref(), Some("https://example.com"));
        assert_eq!(
            m.package.repository.as_deref(),
            Some("https://github.com/example/fullpkg")
        );
        assert_eq!(m.package.readme.as_deref(), Some("README.md"));
        assert_eq!(
            m.package.exclude.as_deref().unwrap(),
            &["tests/*", ".github/*"]
        );
    }

    #[test]
    fn old_format_backward_compat() {
        let f = write_temp("[package]\nname = \"oldpkg\"\nversion = \"0.1.0\"\n");
        let m = parse_manifest(f.path()).unwrap();
        assert_eq!(m.package.name, "oldpkg");
        assert!(m.package.description.is_none());
        assert!(m.package.authors.is_none());
        assert!(m.package.license.is_none());
        assert!(m.package.keywords.is_none());
        assert!(m.package.homepage.is_none());
        assert!(m.package.repository.is_none());
        assert!(m.package.readme.is_none());
        assert!(m.package.exclude.is_none());
    }

    #[test]
    fn roundtrip_with_new_fields() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("hew.toml");
        let manifest = HewManifest {
            package: Package {
                name: "roundtrip".to_string(),
                version: "0.2.0".to_string(),
                description: Some("Roundtrip test".to_string()),
                authors: Some(vec!["Carol".to_string()]),
                license: Some("Apache-2.0".to_string()),
                keywords: Some(vec!["test".to_string()]),
                categories: None,
                homepage: None,
                repository: Some("https://github.com/example/roundtrip".to_string()),
                documentation: None,
                readme: None,
                exclude: Some(vec!["target/*".to_string()]),
                include: None,
                edition: None,
                hew: None,
            },
            dependencies: BTreeMap::new(),
            dev_dependencies: BTreeMap::new(),
            features: BTreeMap::new(),
        };
        save_manifest(&path, &manifest).unwrap();
        let m = parse_manifest(&path).unwrap();
        assert_eq!(m.package.name, "roundtrip");
        assert_eq!(m.package.license.as_deref(), Some("Apache-2.0"));
        assert_eq!(m.package.authors.as_deref().unwrap(), &["Carol"]);
        assert!(m.package.homepage.is_none());
        assert!(m.package.readme.is_none());
        assert_eq!(
            m.package.repository.as_deref(),
            Some("https://github.com/example/roundtrip")
        );
        assert_eq!(m.package.exclude.as_deref().unwrap(), &["target/*"]);
    }

    #[test]
    fn manifest_template_descriptions() {
        let dir = tempfile::tempdir().unwrap();
        for (template, expected) in [
            (ManifestTemplate::Bin, "A Hew binary project"),
            (ManifestTemplate::Lib, "A Hew library"),
            (ManifestTemplate::Actor, "A Hew actor project"),
        ] {
            let path = dir.path().join(format!("{template:?}.toml"));
            write_manifest_with_template(&path, "tpl", template).unwrap();
            let m = parse_manifest(&path).unwrap();
            assert_eq!(m.package.description.as_deref(), Some(expected));
            assert_eq!(m.package.name, "tpl");
            assert_eq!(m.package.version, "0.1.0");
        }
    }

    #[test]
    fn summary_includes_authors_and_license() {
        let m = HewManifest {
            package: Package {
                name: "sumtest".to_string(),
                version: "1.0.0".to_string(),
                description: Some("A test".to_string()),
                authors: Some(vec!["Alice".to_string(), "Bob".to_string()]),
                license: Some("MIT".to_string()),
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
            dependencies: BTreeMap::new(),
            dev_dependencies: BTreeMap::new(),
            features: BTreeMap::new(),
        };
        let s = m.summary();
        assert!(s.contains("by Alice, Bob"), "summary = {s}");
        assert!(s.contains("[MIT]"), "summary = {s}");
    }

    #[test]
    fn parse_table_dependency() {
        let f = write_temp(concat!(
            "[package]\n",
            "name = \"tabdep\"\n",
            "version = \"0.1.0\"\n",
            "\n[dependencies]\n",
            "\"std::net::http\" = \"^1.0\"\n",
            "\"alice::db::postgres\" = { version = \"^2.0\", optional = true }\n",
            "\"alice::telemetry\" = { version = \"^1.0\", features = [\"otlp\"] }\n",
        ));
        let m = parse_manifest(f.path()).unwrap();
        assert_eq!(m.dependencies["std::net::http"].version_req(), "^1.0");
        assert!(matches!(
            m.dependencies["std::net::http"],
            DepSpec::Version(_)
        ));

        assert_eq!(m.dependencies["alice::db::postgres"].version_req(), "^2.0");
        match &m.dependencies["alice::db::postgres"] {
            DepSpec::Table(t) => assert_eq!(t.optional, Some(true)),
            _ => panic!("expected table dep"),
        }

        assert_eq!(m.dependencies["alice::telemetry"].version_req(), "^1.0");
        match &m.dependencies["alice::telemetry"] {
            DepSpec::Table(t) => {
                assert_eq!(t.features.as_deref().unwrap(), &["otlp"]);
            }
            DepSpec::Version(_) => panic!("expected table dep"),
        }
    }

    #[test]
    fn parse_dev_dependencies() {
        let f = write_temp(concat!(
            "[package]\n",
            "name = \"devdep\"\n",
            "version = \"0.1.0\"\n",
            "\n[dependencies]\n",
            "\"std::net::http\" = \"^1.0\"\n",
            "\n[dev-dependencies]\n",
            "\"testing::assert\" = \"^1.0\"\n",
        ));
        let m = parse_manifest(f.path()).unwrap();
        assert_eq!(m.dependencies.len(), 1);
        assert_eq!(m.dev_dependencies.len(), 1);
        assert_eq!(m.dev_dependencies["testing::assert"].version_req(), "^1.0");
    }

    #[test]
    fn parse_features() {
        let f = write_temp(concat!(
            "[package]\n",
            "name = \"feat\"\n",
            "version = \"0.1.0\"\n",
            "\n[features]\n",
            "default = [\"json\"]\n",
            "json = []\n",
            "tls = [\"std::crypto\"]\n",
            "full = [\"json\", \"tls\"]\n",
        ));
        let m = parse_manifest(f.path()).unwrap();
        assert_eq!(m.features["default"], vec!["json"]);
        assert!(m.features["json"].is_empty());
        assert_eq!(m.features["tls"], vec!["std::crypto"]);
        assert_eq!(m.features["full"], vec!["json", "tls"]);
    }

    #[test]
    fn parse_edition_and_hew_version() {
        let f = write_temp(concat!(
            "[package]\n",
            "name = \"edpkg\"\n",
            "version = \"0.1.0\"\n",
            "edition = \"2026\"\n",
            "hew = \">=0.8.0\"\n",
        ));
        let m = parse_manifest(f.path()).unwrap();
        assert_eq!(m.package.edition.as_deref(), Some("2026"));
        assert_eq!(m.package.hew.as_deref(), Some(">=0.8.0"));
    }

    #[test]
    fn parse_path_dependency() {
        let f = write_temp(concat!(
            "[package]\n",
            "name = \"pathdep\"\n",
            "version = \"0.1.0\"\n",
            "\n[dependencies]\n",
            "\"alice::router\" = { version = \"^1.0\", path = \"../router\" }\n",
        ));
        let m = parse_manifest(f.path()).unwrap();
        match &m.dependencies["alice::router"] {
            DepSpec::Table(t) => {
                assert_eq!(t.path.as_deref(), Some("../router"));
            }
            DepSpec::Version(_) => panic!("expected table dep"),
        }
    }

    #[test]
    fn parse_registry_override_dependency() {
        let f = write_temp(concat!(
            "[package]\n",
            "name = \"regdep\"\n",
            "version = \"0.1.0\"\n",
            "\n[dependencies]\n",
            "\"corp::auth\" = { version = \"^1.0\", registry = \"internal\" }\n",
        ));
        let m = parse_manifest(f.path()).unwrap();
        match &m.dependencies["corp::auth"] {
            DepSpec::Table(t) => {
                assert_eq!(t.registry.as_deref(), Some("internal"));
            }
            DepSpec::Version(_) => panic!("expected table dep"),
        }
    }

    #[test]
    fn validate_publish_complete() {
        let m = HewManifest {
            package: Package {
                name: "valid".to_string(),
                version: "1.0.0".to_string(),
                description: Some("A package".to_string()),
                authors: Some(vec!["Alice".to_string()]),
                license: Some("MIT".to_string()),
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
            dependencies: BTreeMap::new(),
            dev_dependencies: BTreeMap::new(),
            features: BTreeMap::new(),
        };
        assert!(validate_for_publish(&m).is_empty());
    }

    #[test]
    fn validate_publish_missing_fields() {
        let m = HewManifest {
            package: Package {
                name: "incomplete".to_string(),
                version: "1.0.0".to_string(),
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
            dependencies: BTreeMap::new(),
            dev_dependencies: BTreeMap::new(),
            features: BTreeMap::new(),
        };
        let missing = validate_for_publish(&m);
        assert!(missing.contains(&"description"));
        assert!(missing.contains(&"license"));
        assert!(missing.contains(&"authors"));
    }

    #[test]
    fn validate_publish_rejects_path_deps() {
        let mut deps = BTreeMap::new();
        deps.insert(
            "local::dep".to_string(),
            DepSpec::Table(DepTable {
                version: "^1.0".to_string(),
                optional: None,
                features: None,
                default_features: None,
                registry: None,
                path: Some("../local".to_string()),
            }),
        );
        let m = HewManifest {
            package: Package {
                name: "pathdep".to_string(),
                version: "1.0.0".to_string(),
                description: Some("Has path dep".to_string()),
                authors: Some(vec!["Alice".to_string()]),
                license: Some("MIT".to_string()),
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
        };
        let missing = validate_for_publish(&m);
        assert!(!missing.is_empty());
    }
}
