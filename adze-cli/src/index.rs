//! Git-based package index format.
//!
//! The index is a GitHub repository with one file per package. Each file
//! contains one JSON line per published version (append-only).

use std::fmt;
use std::path::{Path, PathBuf};

use serde::{Deserialize, Serialize};

/// A single version entry in the package index.
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct IndexEntry {
    /// Fully qualified package name (e.g. `"alice::router"`).
    pub name: String,
    /// Exact version string.
    pub vers: String,
    /// Dependencies for this version.
    #[serde(default)]
    pub deps: Vec<IndexDep>,
    /// Feature flags and their implications.
    #[serde(default)]
    pub features: std::collections::BTreeMap<String, Vec<String>>,
    /// SHA-256 checksum of the tarball (`"sha256:..."`).
    pub cksum: String,
    /// Ed25519 signature of the checksum (`"ed25519:..."`).
    pub sig: String,
    /// Fingerprint of the signing key (`"SHA256:..."`).
    pub key_fp: String,
    /// Yank status: `false`, `true` (soft-yanked), or `"tombstone"`.
    #[serde(default)]
    pub yanked: YankStatus,
    /// Reason for yanking (if yanked or tombstoned).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub yanked_reason: Option<String>,
    /// Timestamp when tombstoned (ISO 8601).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub tombstoned_at: Option<String>,
    /// Hew language edition.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub edition: Option<String>,
    /// Minimum Hew compiler version required.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub hew: Option<String>,
    /// Download URL for the tarball (provided by the registry, not stored).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub dl: Option<String>,
    /// Registry counter-signature (`"ed25519:{hex}"`).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub registry_sig: Option<String>,
    /// Fingerprint of the registry signing key (`"SHA256:..."`).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub registry_key_fp: Option<String>,
    /// Timestamp when the registry accepted the publish (ISO 8601).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub published_at: Option<String>,
}

/// A dependency entry in the index.
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct IndexDep {
    /// Package name.
    pub name: String,
    /// Version requirement string.
    pub req: String,
    /// Features to activate.
    #[serde(default)]
    pub features: Vec<String>,
    /// Whether this is an optional dependency.
    #[serde(default)]
    pub optional: bool,
    /// Whether to include default features.
    #[serde(default = "default_true")]
    pub default_features: bool,
    /// Non-default registry name.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub registry: Option<String>,
}

fn default_true() -> bool {
    true
}

/// Yank status for an index entry.
#[derive(Debug, Clone, PartialEq, Eq, Deserialize, Serialize)]
#[serde(untagged)]
pub enum YankStatus {
    /// Not yanked or soft-yanked (`true`/`false`).
    Bool(bool),
    /// Tombstoned — hard yank, all installs fail.
    Tombstone(String),
}

impl Default for YankStatus {
    fn default() -> Self {
        Self::Bool(false)
    }
}

impl YankStatus {
    /// Returns `true` if the entry is soft-yanked (excluded from new resolutions
    /// but still downloadable from lockfiles).
    #[must_use]
    pub fn is_yanked(&self) -> bool {
        match self {
            Self::Bool(b) => *b,
            Self::Tombstone(_) => true,
        }
    }

    /// Returns `true` if the entry is tombstoned (all installs fail).
    #[must_use]
    pub fn is_tombstoned(&self) -> bool {
        matches!(self, Self::Tombstone(_))
    }
}

/// Package-level deprecation metadata (stored separately from version entries).
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct DeprecationInfo {
    /// Whether the package is deprecated.
    pub deprecated: bool,
    /// Human-readable deprecation message.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub message: Option<String>,
    /// Suggested replacement package.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub successor: Option<String>,
}

/// Errors from index operations.
#[derive(Debug)]
pub enum IndexError {
    /// An I/O error occurred.
    Io(std::io::Error),
    /// A JSON line could not be parsed.
    Parse(String),
}

impl fmt::Display for IndexError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Io(e) => write!(f, "index I/O error: {e}"),
            Self::Parse(msg) => write!(f, "index parse error: {msg}"),
        }
    }
}

impl std::error::Error for IndexError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::Io(e) => Some(e),
            Self::Parse(_) => None,
        }
    }
}

impl From<std::io::Error> for IndexError {
    fn from(e: std::io::Error) -> Self {
        Self::Io(e)
    }
}

// ── Path mapping ────────────────────────────────────────────────────────────

/// Convert a package name to its index file path relative to the index root.
///
/// Namespace `::` segments become directory components.
/// `"alice::router"` → `"alice/router"`.
#[must_use]
pub fn index_path(package_name: &str) -> PathBuf {
    let mut path = PathBuf::new();
    for segment in package_name.split("::") {
        path = path.join(segment);
    }
    path
}

// ── Reading / writing ───────────────────────────────────────────────────────

/// Read all version entries for a package from its index file.
///
/// # Errors
///
/// Returns [`IndexError::Io`] if the file cannot be read, or
/// [`IndexError::Parse`] if a JSON line is malformed.
pub fn read_index_entries(
    index_root: &Path,
    package_name: &str,
) -> Result<Vec<IndexEntry>, IndexError> {
    let path = index_root.join(index_path(package_name));
    if !path.exists() {
        return Ok(Vec::new());
    }
    let content = std::fs::read_to_string(&path)?;
    parse_index_lines(&content)
}

/// Parse JSON lines into index entries.
///
/// # Errors
///
/// Returns [`IndexError::Parse`] if a non-empty line is not valid JSON.
pub fn parse_index_lines(content: &str) -> Result<Vec<IndexEntry>, IndexError> {
    let mut entries = Vec::new();
    for (i, line) in content.lines().enumerate() {
        let trimmed = line.trim();
        if trimmed.is_empty() {
            continue;
        }
        let entry: IndexEntry = serde_json::from_str(trimmed)
            .map_err(|e| IndexError::Parse(format!("line {}: {e}", i + 1)))?;
        entries.push(entry);
    }
    Ok(entries)
}

/// Append a new version entry to a package's index file.
///
/// Creates parent directories if needed.
///
/// # Errors
///
/// Returns [`IndexError::Io`] if the file cannot be written.
#[cfg(test)]
pub fn append_index_entry(index_root: &Path, entry: &IndexEntry) -> Result<(), IndexError> {
    let path = index_root.join(index_path(&entry.name));
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent)?;
    }
    let mut line =
        serde_json::to_string(entry).map_err(|e| IndexError::Parse(format!("serialize: {e}")))?;
    line.push('\n');

    use std::io::Write as _;
    let mut file = std::fs::OpenOptions::new()
        .create(true)
        .append(true)
        .open(&path)?;
    file.write_all(line.as_bytes())?;
    Ok(())
}

/// Find the latest non-yanked version of a package in the index that matches
/// a version requirement.
///
/// # Errors
///
/// Returns [`IndexError`] if the index file cannot be read or parsed.
pub fn resolve_from_index(
    index_root: &Path,
    package_name: &str,
    requirement: &str,
) -> Result<Option<IndexEntry>, IndexError> {
    let entries = read_index_entries(index_root, package_name)?;
    let Ok(req) = crate::resolver::VersionReq::parse(requirement) else {
        return Ok(None);
    };

    let mut candidates: Vec<_> = entries
        .into_iter()
        .filter(|e| !e.yanked.is_yanked())
        .filter(|e| {
            semver::Version::parse(&e.vers)
                .ok()
                .is_some_and(|v| req.matches(&v))
        })
        .collect();

    candidates.sort_by(|a, b| {
        let va = semver::Version::parse(&a.vers).unwrap_or(semver::Version::new(0, 0, 0));
        let vb = semver::Version::parse(&b.vers).unwrap_or(semver::Version::new(0, 0, 0));
        va.cmp(&vb)
    });

    Ok(candidates.pop())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn sample_entry(name: &str, vers: &str) -> IndexEntry {
        IndexEntry {
            name: name.to_string(),
            vers: vers.to_string(),
            deps: vec![],
            features: std::collections::BTreeMap::new(),
            cksum: "sha256:abc123".to_string(),
            sig: "ed25519:def456".to_string(),
            key_fp: "SHA256:xyz".to_string(),
            yanked: YankStatus::Bool(false),
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
    fn index_path_simple() {
        assert_eq!(index_path("alice::router"), PathBuf::from("alice/router"));
    }

    #[test]
    fn index_path_deep() {
        assert_eq!(index_path("std::net::http"), PathBuf::from("std/net/http"));
    }

    #[test]
    fn index_path_single_segment() {
        assert_eq!(index_path("mypackage"), PathBuf::from("mypackage"));
    }

    #[test]
    fn roundtrip_json_line() {
        let entry = sample_entry("alice::router", "1.0.0");
        let json = serde_json::to_string(&entry).unwrap();
        let parsed: IndexEntry = serde_json::from_str(&json).unwrap();
        assert_eq!(parsed.name, "alice::router");
        assert_eq!(parsed.vers, "1.0.0");
        assert!(!parsed.yanked.is_yanked());
    }

    #[test]
    fn parse_yanked_entry() {
        let json = r#"{"name":"evil::pkg","vers":"1.0.0","deps":[],"features":{},"cksum":"sha256:x","sig":"ed25519:y","key_fp":"SHA256:z","yanked":true,"yanked_reason":"CVE-2026-1234"}"#;
        let entry: IndexEntry = serde_json::from_str(json).unwrap();
        assert!(entry.yanked.is_yanked());
        assert!(!entry.yanked.is_tombstoned());
        assert_eq!(entry.yanked_reason.as_deref(), Some("CVE-2026-1234"));
    }

    #[test]
    fn parse_tombstoned_entry() {
        let json = r#"{"name":"evil::pkg","vers":"1.0.0","deps":[],"features":{},"cksum":"sha256:x","sig":"ed25519:y","key_fp":"SHA256:z","yanked":"tombstone","yanked_reason":"malware","tombstoned_at":"2026-02-22T00:00:00Z"}"#;
        let entry: IndexEntry = serde_json::from_str(json).unwrap();
        assert!(entry.yanked.is_yanked());
        assert!(entry.yanked.is_tombstoned());
    }

    #[test]
    fn write_and_read_index() {
        let dir = tempfile::tempdir().unwrap();
        let entry1 = sample_entry("alice::router", "1.0.0");
        let entry2 = sample_entry("alice::router", "1.1.0");

        append_index_entry(dir.path(), &entry1).unwrap();
        append_index_entry(dir.path(), &entry2).unwrap();

        let entries = read_index_entries(dir.path(), "alice::router").unwrap();
        assert_eq!(entries.len(), 2);
        assert_eq!(entries[0].vers, "1.0.0");
        assert_eq!(entries[1].vers, "1.1.0");
    }

    #[test]
    fn read_nonexistent_package_returns_empty() {
        let dir = tempfile::tempdir().unwrap();
        let entries = read_index_entries(dir.path(), "no::such::pkg").unwrap();
        assert!(entries.is_empty());
    }

    #[test]
    fn resolve_picks_highest_non_yanked() {
        let dir = tempfile::tempdir().unwrap();
        append_index_entry(dir.path(), &sample_entry("pkg", "1.0.0")).unwrap();
        append_index_entry(dir.path(), &sample_entry("pkg", "1.5.0")).unwrap();

        let mut yanked = sample_entry("pkg", "2.0.0");
        yanked.yanked = YankStatus::Bool(true);
        append_index_entry(dir.path(), &yanked).unwrap();

        let resolved = resolve_from_index(dir.path(), "pkg", "^1.0").unwrap();
        assert_eq!(resolved.unwrap().vers, "1.5.0");
    }

    #[test]
    fn resolve_skips_tombstoned() {
        let dir = tempfile::tempdir().unwrap();
        append_index_entry(dir.path(), &sample_entry("pkg", "1.0.0")).unwrap();

        let mut tombstoned = sample_entry("pkg", "2.0.0");
        tombstoned.yanked = YankStatus::Tombstone("tombstone".to_string());
        append_index_entry(dir.path(), &tombstoned).unwrap();

        let resolved = resolve_from_index(dir.path(), "pkg", "*").unwrap();
        assert_eq!(resolved.unwrap().vers, "1.0.0");
    }

    #[test]
    fn entry_with_deps() {
        let mut entry = sample_entry("alice::router", "1.0.0");
        entry.deps.push(IndexDep {
            name: "std::net::http".to_string(),
            req: "^2.0".to_string(),
            features: vec![],
            optional: false,
            default_features: true,
            registry: None,
        });

        let json = serde_json::to_string(&entry).unwrap();
        let parsed: IndexEntry = serde_json::from_str(&json).unwrap();
        assert_eq!(parsed.deps.len(), 1);
        assert_eq!(parsed.deps[0].name, "std::net::http");
        assert_eq!(parsed.deps[0].req, "^2.0");
        assert!(parsed.deps[0].default_features);
    }

    #[test]
    fn entry_with_features() {
        let mut entry = sample_entry("pkg", "1.0.0");
        entry
            .features
            .insert("default".to_string(), vec!["json".to_string()]);
        entry.features.insert("json".to_string(), vec![]);
        entry
            .features
            .insert("tls".to_string(), vec!["std::crypto".to_string()]);

        let json = serde_json::to_string(&entry).unwrap();
        let parsed: IndexEntry = serde_json::from_str(&json).unwrap();
        assert_eq!(parsed.features["default"], vec!["json"]);
        assert!(parsed.features["json"].is_empty());
        assert_eq!(parsed.features["tls"], vec!["std::crypto"]);
    }

    #[test]
    fn deprecation_info_serialization() {
        let info = DeprecationInfo {
            deprecated: true,
            message: Some("Use alice::router instead".to_string()),
            successor: Some("alice::router".to_string()),
        };
        let json = serde_json::to_string(&info).unwrap();
        let parsed: DeprecationInfo = serde_json::from_str(&json).unwrap();
        assert!(parsed.deprecated);
        assert_eq!(parsed.successor.as_deref(), Some("alice::router"));
    }
}
