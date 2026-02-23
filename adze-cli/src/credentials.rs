//! Credentials management for the Adze registry.
//!
//! Stores API tokens in `~/.adze/credentials.toml` (mode 0600).

use std::fmt;
use std::path::{Path, PathBuf};

use serde::{Deserialize, Serialize};

/// Stored credentials for a registry.
#[derive(Debug, Deserialize, Serialize)]
pub struct Credentials {
    /// Registry API token.
    pub token: String,
    /// GitHub username associated with this token.
    pub github_user: Option<String>,
}

/// Top-level credentials file.
#[derive(Debug, Default, Deserialize, Serialize)]
pub struct CredentialsFile {
    /// Default registry credentials.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub registry: Option<Credentials>,
    /// Named registry credentials.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub registries: Option<std::collections::BTreeMap<String, Credentials>>,
}

/// Errors from credential operations.
#[derive(Debug)]
pub enum CredentialError {
    Io(std::io::Error),
    Parse(String),
    NotLoggedIn,
}

impl fmt::Display for CredentialError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Io(e) => write!(f, "credentials I/O error: {e}"),
            Self::Parse(msg) => write!(f, "invalid credentials file: {msg}"),
            Self::NotLoggedIn => write!(f, "not logged in; run `adze login`"),
        }
    }
}

impl std::error::Error for CredentialError {}

impl From<std::io::Error> for CredentialError {
    fn from(e: std::io::Error) -> Self {
        Self::Io(e)
    }
}

/// Return the path to `~/.adze/credentials.toml`.
#[must_use]
pub fn credentials_path() -> PathBuf {
    let home = std::env::var("HOME").unwrap_or_else(|_| "/tmp".to_string());
    PathBuf::from(home).join(".adze").join("credentials.toml")
}

/// Load credentials from the file.
///
/// # Errors
///
/// Returns [`CredentialError::NotLoggedIn`] if no credentials file exists.
pub fn load_credentials(path: &Path) -> Result<CredentialsFile, CredentialError> {
    if !path.exists() {
        return Err(CredentialError::NotLoggedIn);
    }
    let text = std::fs::read_to_string(path)?;
    toml::from_str(&text).map_err(|e| CredentialError::Parse(e.to_string()))
}

/// Save credentials to the file with restricted permissions.
///
/// # Errors
///
/// Returns [`CredentialError::Io`] on I/O failures.
pub fn save_credentials(path: &Path, creds: &CredentialsFile) -> Result<(), CredentialError> {
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent)?;
    }
    let content = toml::to_string(creds).map_err(|e| CredentialError::Parse(e.to_string()))?;
    std::fs::write(path, &content)?;

    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt as _;
        std::fs::set_permissions(path, std::fs::Permissions::from_mode(0o600))?;
    }

    Ok(())
}

/// Get the API token for the default registry.
///
/// # Errors
///
/// Returns [`CredentialError::NotLoggedIn`] if no token is stored.
pub fn get_token(path: &Path) -> Result<String, CredentialError> {
    let creds = load_credentials(path)?;
    creds
        .registry
        .map(|r| r.token)
        .ok_or(CredentialError::NotLoggedIn)
}

/// Get the API token for a named registry.
///
/// # Errors
///
/// Returns [`CredentialError::NotLoggedIn`] if no token is stored for this
/// registry.
pub fn get_named_token(path: &Path, name: &str) -> Result<String, CredentialError> {
    let creds = load_credentials(path)?;
    creds
        .registries
        .and_then(|regs| regs.get(name).map(|c| c.token.clone()))
        .ok_or(CredentialError::NotLoggedIn)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn save_and_load_roundtrip() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("credentials.toml");

        let creds = CredentialsFile {
            registry: Some(Credentials {
                token: "tok_abc123".to_string(),
                github_user: Some("alice".to_string()),
            }),
            registries: None,
        };

        save_credentials(&path, &creds).unwrap();
        let loaded = load_credentials(&path).unwrap();
        assert_eq!(loaded.registry.unwrap().token, "tok_abc123");
    }

    #[test]
    fn get_token_returns_not_logged_in() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("credentials.toml");
        let result = get_token(&path);
        assert!(matches!(result, Err(CredentialError::NotLoggedIn)));
    }

    #[test]
    fn named_registry_tokens() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("credentials.toml");

        let mut regs = std::collections::BTreeMap::new();
        regs.insert(
            "internal".to_string(),
            Credentials {
                token: "tok_internal".to_string(),
                github_user: Some("bob".to_string()),
            },
        );

        let creds = CredentialsFile {
            registry: None,
            registries: Some(regs),
        };

        save_credentials(&path, &creds).unwrap();
        assert_eq!(get_named_token(&path, "internal").unwrap(), "tok_internal");
        assert!(matches!(
            get_named_token(&path, "other"),
            Err(CredentialError::NotLoggedIn)
        ));
    }
}
