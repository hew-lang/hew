//! Global configuration for the Adze package manager (`~/.adze/config.toml`).

use std::path::PathBuf;

use serde::{Deserialize, Serialize};

/// Top-level configuration loaded from `~/.adze/config.toml`.
#[derive(Debug, Default, Deserialize, Serialize)]
pub struct AdzeConfig {
    /// Default values used when creating new projects.
    #[serde(default)]
    pub defaults: Option<Defaults>,
    /// Registry location override.
    #[serde(default)]
    pub registry: Option<RegistryConfig>,
    /// Named alternative registries.
    #[serde(default)]
    pub registries: Option<std::collections::BTreeMap<String, RemoteRegistry>>,
}

/// Default values applied during `adze init`.
#[derive(Debug, Default, Deserialize, Serialize)]
pub struct Defaults {
    /// Default author name inserted into new `hew.toml` manifests.
    pub author: Option<String>,
    /// Default SPDX license identifier (e.g. `"MIT"`).
    pub license: Option<String>,
}

/// Registry configuration.
#[derive(Debug, Default, Deserialize, Serialize)]
pub struct RegistryConfig {
    /// Override the global registry path (default `~/.adze/packages`).
    /// Supports `~` for the home directory.
    pub path: Option<String>,
    /// Fallback registry API URL used when the primary is unavailable.
    #[serde(default, rename = "fallback-api")]
    pub fallback_api: Option<String>,
}

/// A named remote registry.
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct RemoteRegistry {
    /// URL of the git index repository.
    pub index: String,
    /// URL of the registry API.
    pub api: String,
}

/// Default registry API URL.
pub const DEFAULT_REGISTRY_API: &str = "https://registry.adze.sh/api/v1";

/// Default package CDN URL.
pub const DEFAULT_REGISTRY_CDN: &str = "https://pkg.adze.sh";

/// Default fallback registry API URL.
pub const DEFAULT_FALLBACK_API: Option<&str> = Some("https://mirror.hewpkg.com/api/v1");

/// The default public registry index URL.
pub const DEFAULT_REGISTRY_INDEX: &str = "https://github.com/hew-lang/registry-index";

/// Compiled-in registry endpoints.
#[derive(Debug, Clone)]
pub struct RegistryEndpoints {
    /// Base URL for the registry API (e.g. `https://registry.hew.sh/api/v1`).
    pub api: String,
    /// Base URL for the package CDN (e.g. `https://pkg.adze.sh`).
    pub cdn: String,
    /// Optional fallback API URL for read operations when the primary is down.
    pub fallback_api: Option<String>,
}

/// Return the compiled-in registry endpoints.
#[must_use]
pub fn discover_registry() -> RegistryEndpoints {
    RegistryEndpoints {
        api: DEFAULT_REGISTRY_API.to_string(),
        cdn: DEFAULT_REGISTRY_CDN.to_string(),
        fallback_api: DEFAULT_FALLBACK_API.map(std::string::ToString::to_string),
    }
}

/// Look up a named registry from the configuration.
///
/// Returns `None` if the registry name is not configured.
#[must_use]
pub fn get_named_registry(config: &AdzeConfig, name: &str) -> Option<RemoteRegistry> {
    config
        .registries
        .as_ref()
        .and_then(|regs| regs.get(name).cloned())
}

/// Load the global configuration from `~/.adze/config.toml`.
///
/// Returns a default (empty) config if the file does not exist or cannot be
/// parsed.
#[must_use]
pub fn load_config() -> AdzeConfig {
    let Some(path) = config_path() else {
        return AdzeConfig::default();
    };
    match std::fs::read_to_string(&path) {
        Ok(text) => toml::from_str(&text).unwrap_or_default(),
        Err(_) => AdzeConfig::default(),
    }
}

/// Resolve the registry path from configuration, expanding `~` to `$HOME`.
///
/// Falls back to `$HOME/.adze/packages` when no override is configured.
#[must_use]
pub fn registry_path(config: &AdzeConfig) -> PathBuf {
    if let Some(reg) = &config.registry {
        if let Some(p) = &reg.path {
            return expand_tilde(p);
        }
    }
    default_registry_path()
}

/// Return the path to `~/.adze/config.toml`, or `None` if `$HOME` is unset.
fn config_path() -> Option<PathBuf> {
    std::env::var("HOME")
        .ok()
        .map(|home| PathBuf::from(home).join(".adze").join("config.toml"))
}

/// Return the default registry path (`$HOME/.adze/packages`).
fn default_registry_path() -> PathBuf {
    let home = std::env::var("HOME").unwrap_or_else(|_| "/tmp".to_string());
    PathBuf::from(home).join(".adze").join("packages")
}

/// Return the path to the local package index cache (`$HOME/.adze/index/`).
#[must_use]
pub fn local_index_path() -> PathBuf {
    let home = std::env::var("HOME").unwrap_or_else(|_| "/tmp".to_string());
    PathBuf::from(home).join(".adze").join("index")
}

/// Expand a leading `~` or `~/` to the value of `$HOME`.
fn expand_tilde(path: &str) -> PathBuf {
    if let Some(rest) = path.strip_prefix("~/") {
        let home = std::env::var("HOME").unwrap_or_else(|_| "/tmp".to_string());
        PathBuf::from(home).join(rest)
    } else if path == "~" {
        let home = std::env::var("HOME").unwrap_or_else(|_| "/tmp".to_string());
        PathBuf::from(home)
    } else {
        PathBuf::from(path)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn load_config_returns_default_when_missing() {
        let config = load_config();
        assert!(config.defaults.is_none());
        assert!(config.registry.is_none());
    }

    #[test]
    fn parse_full_config() {
        let toml_str = r#"
[defaults]
author = "Alice"
license = "MIT"

[registry]
path = "/custom/packages"
"#;
        let config: AdzeConfig = toml::from_str(toml_str).unwrap();
        let defaults = config.defaults.unwrap();
        assert_eq!(defaults.author.as_deref(), Some("Alice"));
        assert_eq!(defaults.license.as_deref(), Some("MIT"));
        let reg = config.registry.unwrap();
        assert_eq!(reg.path.as_deref(), Some("/custom/packages"));
    }

    #[test]
    fn parse_partial_config() {
        let toml_str = "[defaults]\nauthor = \"Bob\"\n";
        let config: AdzeConfig = toml::from_str(toml_str).unwrap();
        let defaults = config.defaults.unwrap();
        assert_eq!(defaults.author.as_deref(), Some("Bob"));
        assert!(defaults.license.is_none());
        assert!(config.registry.is_none());
    }

    #[test]
    fn parse_empty_config() {
        let config: AdzeConfig = toml::from_str("").unwrap();
        assert!(config.defaults.is_none());
        assert!(config.registry.is_none());
    }

    #[test]
    fn registry_path_uses_override() {
        let config = AdzeConfig {
            defaults: None,
            registry: Some(RegistryConfig {
                path: Some("/my/registry".to_string()),
                ..Default::default()
            }),
            registries: None,
        };
        assert_eq!(registry_path(&config), PathBuf::from("/my/registry"));
    }

    #[test]
    fn registry_path_falls_back_to_default() {
        let config = AdzeConfig::default();
        let path = registry_path(&config);
        assert!(path.ends_with(PathBuf::from(".adze").join("packages")));
    }

    #[test]
    fn expand_tilde_expands_home() {
        let expanded = expand_tilde("~/foo/bar");
        // Should not start with ~ anymore.
        assert!(!expanded.to_string_lossy().starts_with('~'));
        assert!(expanded.to_string_lossy().ends_with("foo/bar"));
    }

    #[test]
    fn expand_tilde_absolute_unchanged() {
        let expanded = expand_tilde("/absolute/path");
        assert_eq!(expanded, PathBuf::from("/absolute/path"));
    }

    #[test]
    fn parse_config_with_registries() {
        let toml_str = r#"
[defaults]
author = "Alice"

[registries.internal]
index = "https://github.com/acme/hew-index"
api = "https://hew.acme.internal/api/v1"

[registries.staging]
index = "https://github.com/acme/hew-index-staging"
api = "https://hew-staging.acme.internal/api/v1"
"#;
        let config: AdzeConfig = toml::from_str(toml_str).unwrap();
        let regs = config.registries.unwrap();
        assert_eq!(regs.len(), 2);
        assert_eq!(regs["internal"].api, "https://hew.acme.internal/api/v1");
        assert_eq!(
            regs["staging"].index,
            "https://github.com/acme/hew-index-staging"
        );
    }

    #[test]
    fn get_named_registry_returns_none_when_missing() {
        let config = AdzeConfig::default();
        assert!(get_named_registry(&config, "internal").is_none());
    }

    #[test]
    fn parse_config_with_fallback_api() {
        let toml_str = r#"
[registry]
fallback-api = "https://mirror.example.com/api/v1"
"#;
        let config: AdzeConfig = toml::from_str(toml_str).unwrap();
        let reg = config.registry.unwrap();
        assert_eq!(
            reg.fallback_api.as_deref(),
            Some("https://mirror.example.com/api/v1")
        );
    }
}
