//! Lightweight `hew.toml` loader used by the compiler driver.

use std::collections::BTreeMap;
use std::path::Path;

use serde::Deserialize;

#[derive(Debug, Deserialize)]
struct TomlManifest {
    #[serde(default)]
    dependencies: BTreeMap<String, String>,
}

/// Minimal representation of `adze.lock` for the compiler driver.
#[derive(Debug, Deserialize)]
struct AdzeTomlLock {
    #[serde(default)]
    package: Vec<LockedEntry>,
}

/// A single locked package entry.
#[derive(Debug, Deserialize)]
struct LockedEntry {
    name: String,
    version: String,
}

/// Reads `adze.lock` from `dir` and returns `(name, version)` pairs.
///
/// Returns `None` when no lockfile is present or it cannot be parsed.
pub fn load_lockfile(dir: &Path) -> Option<Vec<(String, String)>> {
    let path = dir.join("adze.lock");
    let text = std::fs::read_to_string(path).ok()?;
    let lock: AdzeTomlLock = toml::from_str(&text).ok()?;
    Some(
        lock.package
            .into_iter()
            .map(|e| (e.name, e.version))
            .collect(),
    )
}

/// Returns the dependency module paths declared in `hew.toml` located in
/// `dir`, or `None` when no manifest is present (script mode).
/// Returns `Some(vec![])` when a manifest exists but has no dependencies.
pub fn load_dependencies(dir: &Path) -> Option<Vec<String>> {
    let path = dir.join("hew.toml");
    if !path.exists() {
        return None;
    }
    let text = match std::fs::read_to_string(&path) {
        Ok(t) => t,
        Err(_) => return None,
    };
    let manifest: TomlManifest = match toml::from_str(&text) {
        Ok(m) => m,
        Err(_) => return None,
    };
    Some(manifest.dependencies.into_keys().collect())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;

    fn write_toml(dir: &std::path::Path, content: &str) {
        let mut f = std::fs::File::create(dir.join("hew.toml")).unwrap();
        f.write_all(content.as_bytes()).unwrap();
    }

    #[test]
    fn no_manifest_returns_none() {
        let dir = tempfile::tempdir().unwrap();
        assert!(load_dependencies(dir.path()).is_none());
    }

    #[test]
    fn manifest_no_deps_returns_some_empty() {
        let dir = tempfile::tempdir().unwrap();
        write_toml(dir.path(), "[package]\nname = \"foo\"\n");
        let deps = load_dependencies(dir.path()).expect("should be Some");
        assert!(deps.is_empty());
    }

    #[test]
    fn manifest_with_deps_returns_keys() {
        let dir = tempfile::tempdir().unwrap();
        write_toml(
            dir.path(),
            "[dependencies]\nstd_utils = \"1.0\"\nmath = \"0.2\"\n",
        );
        let mut deps = load_dependencies(dir.path()).expect("should be Some");
        deps.sort();
        assert_eq!(deps, vec!["math", "std_utils"]);
    }

    // -- lockfile tests --

    fn write_lockfile(dir: &std::path::Path, content: &str) {
        let mut f = std::fs::File::create(dir.join("adze.lock")).unwrap();
        f.write_all(content.as_bytes()).unwrap();
    }

    #[test]
    fn no_lockfile_returns_none() {
        let dir = tempfile::tempdir().unwrap();
        assert!(load_lockfile(dir.path()).is_none());
    }

    #[test]
    fn empty_lockfile_returns_some_empty() {
        let dir = tempfile::tempdir().unwrap();
        write_lockfile(dir.path(), "# empty\n");
        let entries = load_lockfile(dir.path()).expect("should be Some");
        assert!(entries.is_empty());
    }

    #[test]
    fn lockfile_with_packages() {
        let dir = tempfile::tempdir().unwrap();
        write_lockfile(
            dir.path(),
            "[[package]]\nname = \"ecosystem::db::postgres\"\nversion = \"1.0.0\"\n\n\
             [[package]]\nname = \"std::net::http\"\nversion = \"2.1.0\"\n",
        );
        let mut entries = load_lockfile(dir.path()).expect("should be Some");
        entries.sort();
        assert_eq!(
            entries,
            vec![
                ("ecosystem::db::postgres".to_string(), "1.0.0".to_string()),
                ("std::net::http".to_string(), "2.1.0".to_string()),
            ]
        );
    }

    #[test]
    fn lockfile_ignores_extra_fields() {
        let dir = tempfile::tempdir().unwrap();
        write_lockfile(
            dir.path(),
            "[[package]]\nname = \"mypkg\"\nversion = \"0.1.0\"\nchecksum = \"sha256:abc\"\n",
        );
        let entries = load_lockfile(dir.path()).expect("should be Some");
        assert_eq!(entries.len(), 1);
        assert_eq!(entries[0], ("mypkg".to_string(), "0.1.0".to_string()));
    }

    #[test]
    fn lockfile_invalid_toml_returns_none() {
        let dir = tempfile::tempdir().unwrap();
        write_lockfile(dir.path(), "this is not valid toml {{{\n");
        assert!(load_lockfile(dir.path()).is_none());
    }
}
