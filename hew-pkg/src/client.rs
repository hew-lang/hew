//! HTTP client for the Hew package registry API.
//!
//! Communicates with the Cloudflare Workers API for publishing, yanking,
//! searching, namespace management, and key registration.

use std::fmt;
use std::fmt::Write as _;

use serde::{Deserialize, Serialize};

use crate::config;
use crate::config::WireNames;
use crate::index::IndexEntry;
use crate::package_name;

/// Errors from registry API operations.
#[derive(Debug)]
pub enum ApiError {
    /// HTTP request failed.
    Http(String),
    /// Server returned an error response.
    Server { status: u16, message: String },
    /// Response could not be parsed.
    Parse(String),
    /// Not authenticated.
    NotAuthenticated,
}

impl fmt::Display for ApiError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Http(msg) => write!(f, "HTTP error: {msg}"),
            Self::Server { status, message } => write!(f, "registry error ({status}): {message}"),
            Self::Parse(msg) => write!(f, "parse error: {msg}"),
            Self::NotAuthenticated => write!(f, "not authenticated; run `hew login`"),
        }
    }
}

impl std::error::Error for ApiError {}

/// Convert a [`ureq::Error`] into an [`ApiError`].
///
/// Maps `Error::StatusCode` (non-2xx HTTP responses) to [`ApiError::Server`]
/// and all transport/IO errors to [`ApiError::Http`].
#[allow(
    clippy::needless_pass_by_value,
    reason = "used as map_err(map_ureq_error)"
)]
fn map_ureq_error(e: ureq::Error) -> ApiError {
    if let ureq::Error::StatusCode(code) = e {
        ApiError::Server {
            status: code,
            message: format!("HTTP {code}"),
        }
    } else {
        ApiError::Http(e.to_string())
    }
}

// ── API response types ──────────────────────────────────────────────────────

/// GitHub OAuth device flow initiation response.
#[derive(Debug, Deserialize)]
pub struct DeviceFlowResponse {
    pub device_code: String,
    pub user_code: String,
    pub verification_uri: String,
    pub expires_in: u64,
    pub interval: u64,
}

/// Token exchange response.
#[derive(Debug, Deserialize)]
pub struct TokenResponse {
    pub token: Option<String>,
    pub error: Option<String>,
    pub github_user: Option<String>,
}

/// Package search result.
#[derive(Debug, Deserialize)]
pub struct SearchResult {
    pub results: Vec<SearchHit>,
    pub total: usize,
}

/// A single search result entry.
#[derive(Debug, Deserialize)]
pub struct SearchHit {
    pub name: String,
    pub description: Option<String>,
    pub latest_version: Option<String>,
    pub downloads: Option<u64>,
}

/// Namespace ownership info.
#[derive(Debug, Deserialize)]
pub struct NamespaceInfo {
    pub prefix: String,
    pub owner: String,
    pub source: String,
}

/// Public key record from the registry.
#[derive(Debug, Deserialize)]
pub struct PublicKeyResponse {
    pub fingerprint: String,
    pub public_key: String,
    pub key_type: String,
    pub github_user: String,
    pub github_id: u64,
}

/// Registry signing key info.
#[derive(Debug, Deserialize)]
pub struct RegistryKeyResponse {
    pub key_id: String,
    pub public_key: String,
    pub algorithm: String,
}

/// Publish request body.
#[derive(Debug, Serialize)]
pub struct PublishRequest {
    pub metadata: PublishMetadata,
    pub checksum: String,
    pub signature: String,
    pub key_fingerprint: String,
}

/// Metadata portion of a publish request.
#[derive(Debug, Serialize)]
pub struct PublishMetadata {
    pub name: String,
    pub vers: String,
    pub description: String,
    pub license: String,
    pub authors: Vec<String>,
    pub deps: Vec<crate::index::IndexDep>,
    pub features: std::collections::BTreeMap<String, Vec<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub edition: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub hew: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub keywords: Option<Vec<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub categories: Option<Vec<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub homepage: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub repository: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub documentation: Option<String>,
}

// ── Client ──────────────────────────────────────────────────────────────────

/// A Hew registry API client.
#[derive(Debug)]
pub struct RegistryClient {
    api_url: String,
    wire_names: WireNames,
    fallback_urls: Vec<RegistryEndpoint>,
    /// CDN base URL for package downloads.
    cdn_url: Option<String>,
    token: Option<String>,
    /// Shared HTTP agent with Happy Eyeballs TCP connector.
    agent: ureq::Agent,
}

#[derive(Debug)]
struct RegistryEndpoint {
    api_url: String,
    wire_names: WireNames,
}

impl RegistryClient {
    /// Create a client for the official Hew registry using compiled-in defaults.
    ///
    /// Loads `~/.hew/config.toml` to check for a `fallback-api` override.
    #[must_use]
    pub fn new() -> Self {
        let cfg = config::load_config().unwrap_or_else(|error| {
            eprintln!("hew: {error}");
            std::process::exit(1);
        });
        Self::new_with_config(&cfg)
    }

    /// Create a client using the provided config.
    #[must_use]
    pub fn new_with_config(cfg: &config::PkgConfig) -> Self {
        let endpoints = config::discover_registry();

        // Config fallback-api overrides the compiled-in default.
        let fallback_api = cfg
            .registry
            .as_ref()
            .and_then(|r| r.fallback_api.clone())
            .or(endpoints.fallback_api);

        let mut client = Self::with_url(endpoints.api);
        if let Some(mode) = cfg
            .registry
            .as_ref()
            .and_then(|registry| registry.wire_names)
        {
            client = client.with_wire_names(mode);
        }
        client.cdn_url = endpoints.cdn;
        if let Some(url) = fallback_api {
            let mode = cfg
                .registry
                .as_ref()
                .and_then(|registry| registry.fallback_wire_names)
                .unwrap_or_else(|| WireNames::for_api(&url));
            client = client.with_fallback_wire_names(url, mode);
        }
        client
    }

    /// Create a client for a named registry.
    #[must_use]
    pub fn with_url(api_url: impl AsRef<str>) -> Self {
        Self {
            api_url: config::registry_identity(api_url.as_ref()),
            wire_names: WireNames::for_api(api_url.as_ref()),
            fallback_urls: Vec::new(),
            cdn_url: None,
            token: None,
            agent: build_agent(),
        }
    }

    /// Add a fallback URL to try when the primary is unavailable.
    #[must_use]
    pub fn with_fallback(self, url: String) -> Self {
        let wire_names = WireNames::for_api(&url);
        self.with_fallback_wire_names(url, wire_names)
    }

    /// Select package-name encoding for this API without changing source identity.
    #[must_use]
    pub fn with_wire_names(mut self, mode: WireNames) -> Self {
        self.wire_names = mode;
        self
    }

    /// Add an explicitly selected mirror and its independent wire-name mode.
    #[must_use]
    pub fn with_fallback_wire_names(mut self, mut url: String, mode: WireNames) -> Self {
        let identity = config::registry_identity(&url);
        url.truncate(0);
        url.push_str(&identity);
        self.fallback_urls.push(RegistryEndpoint {
            api_url: url,
            wire_names: mode,
        });
        self
    }

    /// Set the authentication token.
    #[must_use]
    pub fn with_token(mut self, token: String) -> Self {
        self.token = Some(token);
        self
    }

    /// Return the registry API URL used to query a package.
    #[must_use]
    pub fn package_url(&self, name: &str) -> String {
        let wire_name =
            package_name::to_wire(name, self.wire_names).unwrap_or_else(|_| percent_encode(name));
        format!("{}/packages/{wire_name}", self.api_url)
    }

    /// Canonical source identity bound to this client's primary API URL.
    #[must_use]
    pub fn registry_identity(&self) -> &str {
        &self.api_url
    }

    /// Start the GitHub OAuth device flow.
    ///
    /// # Errors
    ///
    /// Returns [`ApiError`] on HTTP or parse failures.
    pub fn login_device(&self) -> Result<DeviceFlowResponse, ApiError> {
        let url = format!("{}/login/device", self.api_url);
        let resp = self.agent.post(&url).send_empty().map_err(map_ureq_error)?;

        if resp.status().as_u16() != 200 {
            return Err(self.parse_error_response(resp));
        }

        resp.into_body()
            .read_json()
            .map_err(|e| ApiError::Parse(e.to_string()))
    }

    /// Poll for token exchange.
    ///
    /// # Errors
    ///
    /// Returns [`ApiError`] on HTTP or parse failures.
    pub fn login_token(&self, device_code: &str) -> Result<TokenResponse, ApiError> {
        let url = format!("{}/login/token", self.api_url);
        let body = serde_json::json!({ "device_code": device_code });

        let resp = self
            .agent
            .post(&url)
            .send_json(&body)
            .map_err(map_ureq_error)?;

        resp.into_body()
            .read_json()
            .map_err(|e| ApiError::Parse(e.to_string()))
    }

    /// Publish a package version.
    ///
    /// # Errors
    ///
    /// Returns [`ApiError`] on HTTP, auth, or server failures.
    pub fn publish(
        &self,
        name: &str,
        version: &str,
        tarball: &[u8],
        request: &PublishRequest,
    ) -> Result<(), ApiError> {
        use base64::Engine as _;

        let token = self.token.as_ref().ok_or(ApiError::NotAuthenticated)?;
        let wire_name = package_name::to_wire(name, self.wire_names).map_err(ApiError::Parse)?;
        if request.metadata.name != name || request.metadata.vers != version {
            return Err(ApiError::Parse(
                "publish metadata identity does not match requested package/version".to_string(),
            ));
        }
        validate_version(version)?;
        let url = format!("{}/packages/{}/{}", self.api_url, wire_name, version);

        let metadata_json =
            serde_json::to_string(request).map_err(|e| ApiError::Parse(e.to_string()))?;
        let tarball_b64 = base64::engine::general_purpose::STANDARD.encode(tarball);

        let body = serde_json::json!({
            "metadata": metadata_json,
            "tarball": tarball_b64,
        });

        let resp = self
            .agent
            .put(&url)
            .header("Authorization", &format!("Bearer {token}"))
            .send_json(&body)
            .map_err(map_ureq_error)?;

        if resp.status().as_u16() != 200 && resp.status().as_u16() != 201 {
            return Err(self.parse_error_response(resp));
        }

        Ok(())
    }

    /// Yank or unyank a package version.
    ///
    /// # Errors
    ///
    /// Returns [`ApiError`] on HTTP, auth, or server failures.
    pub fn yank(
        &self,
        name: &str,
        version: &str,
        yanked: bool,
        reason: Option<&str>,
    ) -> Result<(), ApiError> {
        let token = self.token.as_ref().ok_or(ApiError::NotAuthenticated)?;
        let wire_name = package_name::to_wire(name, self.wire_names).map_err(ApiError::Parse)?;
        validate_version(version)?;
        let url = format!("{}/packages/{}/{}/yank", self.api_url, wire_name, version);

        let mut body = serde_json::json!({ "yanked": yanked });
        if let Some(r) = reason {
            body["reason"] = serde_json::Value::String(r.to_string());
        }

        let resp = self
            .agent
            .patch(&url)
            .header("Authorization", &format!("Bearer {token}"))
            .send_json(&body)
            .map_err(map_ureq_error)?;

        if resp.status().as_u16() != 200 {
            return Err(self.parse_error_response(resp));
        }

        Ok(())
    }

    /// Search for packages.
    ///
    /// # Errors
    ///
    /// Returns [`ApiError`] on HTTP or parse failures.
    pub fn search(
        &self,
        query: &str,
        category: Option<&str>,
        page: u32,
        per_page: u32,
    ) -> Result<SearchResult, ApiError> {
        self.try_with_fallback(|base_url, mode| {
            let query = if package_name::is_valid(query) && query.contains('.') {
                package_name::to_wire(query, mode).map_err(ApiError::Parse)?
            } else {
                query.to_string()
            };
            let mut url = format!(
                "{base_url}/search?q={}&page={page}&per_page={per_page}",
                percent_encode(&query)
            );
            if let Some(cat) = category {
                let _ = write!(url, "&category={}", percent_encode(cat));
            }

            let resp = self.agent.get(&url).call().map_err(map_ureq_error)?;

            if resp.status().as_u16() != 200 {
                return Err(self.parse_error_response(resp));
            }

            let mut result: SearchResult = resp
                .into_body()
                .read_json()
                .map_err(|e| ApiError::Parse(e.to_string()))?;
            let mut names = std::collections::HashSet::new();
            for hit in &mut result.results {
                hit.name = package_name::from_wire(&hit.name, mode).map_err(ApiError::Parse)?;
                if !names.insert(hit.name.clone()) {
                    return Err(ApiError::Parse(
                        "duplicate package identity in search response".to_string(),
                    ));
                }
            }
            Ok(result)
        })
    }

    /// Get all versions of a package.
    ///
    /// # Errors
    ///
    /// Returns [`ApiError`] on HTTP or parse failures.
    pub fn get_package(&self, name: &str) -> Result<Vec<IndexEntry>, ApiError> {
        self.try_with_fallback(|base_url, mode| {
            #[derive(Deserialize)]
            struct PackageRecord {
                #[serde(default)]
                metadata: Option<PackageMetadataIdentity>,
                versions: Vec<IndexEntry>,
            }
            #[derive(Deserialize)]
            struct PackageMetadataIdentity {
                name: String,
            }

            let wire_name = package_name::to_wire(name, mode).map_err(ApiError::Parse)?;
            let url = format!("{base_url}/packages/{wire_name}");

            let resp = self.agent.get(&url).call().map_err(map_ureq_error)?;

            if resp.status().as_u16() != 200 {
                return Err(self.parse_error_response(resp));
            }

            let mut record: PackageRecord = resp
                .into_body()
                .read_json()
                .map_err(|e| ApiError::Parse(e.to_string()))?;
            match record.metadata {
                Some(metadata) if metadata_identity_matches(&metadata.name, name, mode) => {}
                None if mode == WireNames::Dotted => {}
                _ => {
                    return Err(ApiError::Parse(
                        "registry metadata identity does not match requested package".to_string(),
                    ))
                }
            }
            let mut versions = std::collections::HashSet::new();
            for entry in &mut record.versions {
                if entry.name != wire_name
                    || package_name::from_wire(&entry.name, mode).map_err(ApiError::Parse)? != name
                {
                    return Err(ApiError::Parse(
                        "registry version identity does not match requested package".to_string(),
                    ));
                }
                validate_version(&entry.vers)?;
                if !versions.insert(entry.vers.clone()) {
                    return Err(ApiError::Parse(
                        "duplicate version identity in registry response".to_string(),
                    ));
                }
                entry.registry_name = Some(entry.name.clone());
                entry.name = name.to_string();
                normalize_dependencies(entry)?;
            }
            Ok(record.versions)
        })
    }

    /// Register a custom namespace prefix.
    ///
    /// # Errors
    ///
    /// Returns [`ApiError`] on HTTP, auth, or server failures.
    pub fn register_namespace(&self, prefix: &str) -> Result<(), ApiError> {
        let token = self.token.as_ref().ok_or(ApiError::NotAuthenticated)?;
        let url = format!("{}/namespaces/{}", self.api_url, percent_encode(prefix));

        let resp = self
            .agent
            .put(&url)
            .header("Authorization", &format!("Bearer {token}"))
            .send_empty()
            .map_err(map_ureq_error)?;

        if resp.status().as_u16() != 200 && resp.status().as_u16() != 201 {
            return Err(self.parse_error_response(resp));
        }

        Ok(())
    }

    /// Get namespace info.
    ///
    /// # Errors
    ///
    /// Returns [`ApiError`] on HTTP or parse failures.
    pub fn get_namespace(&self, prefix: &str) -> Result<NamespaceInfo, ApiError> {
        self.try_with_fallback(|base_url, _| {
            let url = format!("{base_url}/namespaces/{}", percent_encode(prefix));

            let resp = self.agent.get(&url).call().map_err(map_ureq_error)?;

            if resp.status().as_u16() != 200 {
                return Err(self.parse_error_response(resp));
            }

            resp.into_body()
                .read_json()
                .map_err(|e| ApiError::Parse(e.to_string()))
        })
    }

    /// Register a signing public key.
    ///
    /// # Errors
    ///
    /// Returns [`ApiError`] on HTTP, auth, or server failures.
    pub fn register_key(&self, public_key_b64: &str) -> Result<String, ApiError> {
        #[derive(Deserialize)]
        struct KeyResponse {
            fingerprint: String,
        }

        let token = self.token.as_ref().ok_or(ApiError::NotAuthenticated)?;
        let url = format!("{}/keys", self.api_url);

        let body = serde_json::json!({
            "public_key": public_key_b64,
            "key_type": "ed25519",
        });

        let resp = self
            .agent
            .put(&url)
            .header("Authorization", &format!("Bearer {token}"))
            .send_json(&body)
            .map_err(map_ureq_error)?;

        if resp.status().as_u16() != 200 && resp.status().as_u16() != 201 {
            return Err(self.parse_error_response(resp));
        }
        let kr: KeyResponse = resp
            .into_body()
            .read_json()
            .map_err(|e| ApiError::Parse(e.to_string()))?;
        Ok(kr.fingerprint)
    }

    /// Deprecate a package.
    ///
    /// # Errors
    ///
    /// Returns [`ApiError`] on HTTP, auth, or server failures.
    pub fn deprecate(
        &self,
        name: &str,
        message: Option<&str>,
        successor: Option<&str>,
    ) -> Result<(), ApiError> {
        self.set_deprecation(name, true, message, successor)
    }

    /// Set or clear package deprecation without changing its wire identity.
    ///
    /// # Errors
    ///
    /// Returns [`ApiError`] on invalid names, HTTP, auth, or server failures.
    pub fn set_deprecation(
        &self,
        name: &str,
        deprecated: bool,
        message: Option<&str>,
        successor: Option<&str>,
    ) -> Result<(), ApiError> {
        let token = self.token.as_ref().ok_or(ApiError::NotAuthenticated)?;
        let wire_name = package_name::to_wire(name, self.wire_names).map_err(ApiError::Parse)?;
        let successor = successor
            .map(|name| package_name::to_wire(name, self.wire_names))
            .transpose()
            .map_err(ApiError::Parse)?;
        let url = format!("{}/packages/{wire_name}/deprecate", self.api_url);

        let mut body = serde_json::json!({ "deprecated": deprecated });
        if let Some(msg) = message {
            body["message"] = serde_json::Value::String(msg.to_string());
        }
        if let Some(succ) = successor {
            body["successor"] = serde_json::Value::String(succ);
        }

        let resp = self
            .agent
            .patch(&url)
            .header("Authorization", &format!("Bearer {token}"))
            .send_json(&body)
            .map_err(map_ureq_error)?;

        if resp.status().as_u16() != 200 {
            return Err(self.parse_error_response(resp));
        }

        Ok(())
    }

    /// Download a tarball from the registry.
    ///
    /// The `url` is an absolute download URL (e.g. from the package CDN).
    /// On retriable failure, the path component is extracted and retried
    /// against each fallback base URL.
    ///
    /// # Errors
    ///
    /// Returns [`ApiError`] on HTTP failures.
    pub fn download_tarball(&self, url: &str) -> Result<Vec<u8>, ApiError> {
        self.download_tarball_with_package(url, None)
    }

    /// Download an identified package, using explicit CDN and mirror archive
    /// routes on retriable failure. The primary URL remains the checked API's
    /// supplied download URL; logical package names are never inferred from it.
    ///
    /// # Errors
    ///
    /// Returns [`ApiError`] on invalid identity or HTTP failures.
    pub fn download_package_tarball(
        &self,
        name: &str,
        version: &str,
        url: &str,
    ) -> Result<Vec<u8>, ApiError> {
        package_name::to_wire(name, self.wire_names).map_err(ApiError::Parse)?;
        validate_version(version)?;
        self.download_tarball_with_package(url, Some((name, version)))
    }

    fn download_tarball_with_package(
        &self,
        url: &str,
        package: Option<(&str, &str)>,
    ) -> Result<Vec<u8>, ApiError> {
        let do_download = |download_url: &str| -> Result<Vec<u8>, ApiError> {
            use std::io::Read as _;

            let resp = self
                .agent
                .get(download_url)
                .call()
                .map_err(map_ureq_error)?;

            if resp.status().as_u16() != 200 {
                return Err(self.parse_error_response(resp));
            }
            let mut data = Vec::new();
            resp.into_body()
                .into_reader()
                .read_to_end(&mut data)
                .map_err(|e| ApiError::Http(e.to_string()))?;
            Ok(data)
        };

        let has_fallbacks = self.cdn_url.is_some() || !self.fallback_urls.is_empty();

        match do_download(url) {
            Ok(data) => Ok(data),
            Err(err) if Self::is_retriable(&err) && has_fallbacks => {
                eprintln!("warning: primary registry unavailable, trying fallback...");
                let path = extract_url_path(url);
                let mut last_err = err;

                // Try CDN first (typically faster/more available).
                if let Some(ref cdn) = self.cdn_url {
                    let cdn_download = if let Some((name, version)) = package {
                        let wire = package_name::to_wire(name, WireNames::Slash)
                            .map_err(ApiError::Parse)?;
                        format!(
                            "{}/tarballs/{wire}/{}.tar.zst",
                            cdn.trim_end_matches('/'),
                            version
                        )
                    } else {
                        format!("{}{}", cdn.trim_end_matches('/'), path)
                    };
                    match do_download(&cdn_download) {
                        Ok(data) => return Ok(data),
                        Err(e) if Self::is_retriable(&e) => {
                            last_err = e;
                        }
                        Err(e) => return Err(e),
                    }
                }

                for endpoint in &self.fallback_urls {
                    let fallback_download = if let Some((name, version)) = package {
                        mirror_download_url(&endpoint.api_url, name, version, endpoint.wire_names)?
                    } else {
                        format!("{}{}", endpoint.api_url.trim_end_matches('/'), path)
                    };
                    match do_download(&fallback_download) {
                        Ok(data) => return Ok(data),
                        Err(e) if Self::is_retriable(&e) => {
                            last_err = e;
                        }
                        Err(e) => return Err(e),
                    }
                }
                Err(last_err)
            }
            Err(err) => Err(err),
        }
    }

    /// Fetch a public signing key by its fingerprint.
    ///
    /// Returns the base64-encoded public key bytes on success.
    ///
    /// # Errors
    ///
    /// Returns [`ApiError`] if the key is not found or on HTTP failures.
    pub fn get_public_key(&self, fingerprint: &str) -> Result<PublicKeyResponse, ApiError> {
        // Percent-encode the fingerprint for the URL path.
        // Fingerprints look like `SHA256:{base64}` — the `:` and
        // base64 chars like `/` and `+` must be encoded.
        let encoded_fp = percent_encode(fingerprint);
        self.try_with_fallback(|base_url, _| {
            let url = format!("{base_url}/keys/{encoded_fp}");
            let resp = self.agent.get(&url).call().map_err(map_ureq_error)?;

            if resp.status().as_u16() != 200 {
                return Err(self.parse_error_response(resp));
            }

            resp.into_body()
                .read_json()
                .map_err(|e| ApiError::Parse(e.to_string()))
        })
    }

    /// Fetch the registry's public signing key.
    ///
    /// # Errors
    ///
    /// Returns [`ApiError`] if the registry has no key configured or on HTTP failures.
    pub fn get_registry_key(&self) -> Result<RegistryKeyResponse, ApiError> {
        self.try_with_fallback(|base_url, _| {
            let url = format!("{base_url}/registry-key");
            let resp = self.agent.get(&url).call().map_err(map_ureq_error)?;

            if resp.status().as_u16() != 200 {
                return Err(self.parse_error_response(resp));
            }

            resp.into_body()
                .read_json()
                .map_err(|e| ApiError::Parse(e.to_string()))
        })
    }

    /// Check whether an error is retriable (network error or server 5xx).
    fn is_retriable(err: &ApiError) -> bool {
        match err {
            ApiError::Http(_) => true,
            ApiError::Server { status, .. } => *status >= 500,
            _ => false,
        }
    }

    /// Execute `f` against the primary URL, falling back to mirrors on
    /// retriable errors. Prints a warning on the first fallback attempt.
    fn try_with_fallback<T>(
        &self,
        f: impl Fn(&str, WireNames) -> Result<T, ApiError>,
    ) -> Result<T, ApiError> {
        match f(&self.api_url, self.wire_names) {
            Ok(val) => Ok(val),
            Err(err) if Self::is_retriable(&err) => {
                if self.fallback_urls.is_empty() {
                    return Err(err);
                }
                eprintln!("warning: primary registry unavailable, trying fallback...");
                let mut last_err = err;
                for endpoint in &self.fallback_urls {
                    match f(&endpoint.api_url, endpoint.wire_names) {
                        Ok(val) => return Ok(val),
                        Err(e) if Self::is_retriable(&e) => {
                            last_err = e;
                        }
                        Err(e) => return Err(e),
                    }
                }
                Err(last_err)
            }
            Err(err) => Err(err),
        }
    }

    /// Parse an error response body.
    #[expect(clippy::unused_self, reason = "method is part of the client API")]
    fn parse_error_response(&self, resp: ureq::http::Response<ureq::Body>) -> ApiError {
        #[derive(Deserialize)]
        struct ErrorBody {
            message: Option<String>,
            error: Option<String>,
        }

        let status: u16 = resp.status().into();

        let message = resp
            .into_body()
            .read_json::<ErrorBody>()
            .ok()
            .and_then(|b| b.message.or(b.error))
            .unwrap_or_else(|| format!("HTTP {status}"));

        ApiError::Server { status, message }
    }
}

impl Default for RegistryClient {
    fn default() -> Self {
        Self::new()
    }
}

/// Historical metadata may retain an authored descriptive spelling. Bind it
/// to the requested logical identity without admitting aliases in version
/// entries, request paths, dependencies or signed registry identities.
fn metadata_identity_matches(metadata: &str, name: &str, mode: WireNames) -> bool {
    if mode == WireNames::Dotted {
        return metadata == name;
    }
    if metadata.contains("::") {
        if metadata.contains('.') || metadata.contains('/') {
            return false;
        }
        let logical = metadata.replace("::", ".");
        return package_name::is_valid(&logical) && logical == name;
    }
    package_name::dependency_from_wire(metadata).is_ok_and(|logical| logical == name)
}

fn validate_version(version: &str) -> Result<(), ApiError> {
    // Valid SemVer is already safe in a path segment. Preserve its literal `+`:
    // the registry wildcard parser uses the raw path as the version identity.
    semver::Version::parse(version)
        .map(|_| ())
        .map_err(|error| ApiError::Parse(format!("invalid registry version `{version}`: {error}")))
}

fn normalize_dependencies(entry: &mut IndexEntry) -> Result<(), ApiError> {
    let mut aliases = std::collections::HashMap::new();
    let mut identities = std::collections::HashSet::new();
    for dep in &mut entry.deps {
        let logical = package_name::dependency_from_wire(&dep.name).map_err(ApiError::Parse)?;
        if !identities.insert(logical.clone()) {
            return Err(ApiError::Parse(
                "duplicate dependency identity in registry response".to_string(),
            ));
        }
        aliases.insert(dep.name.clone(), logical.clone());
        dep.name = logical;
    }
    // Feature labels are not package names. Rewrite only implication values
    // that refer to an actual dependency alias in this response.
    for implications in entry.features.values_mut() {
        for implication in implications {
            if let Some(logical) = aliases.get(implication) {
                implication.clone_from(logical);
            }
        }
    }
    Ok(())
}

fn mirror_download_url(
    api: &str,
    name: &str,
    version: &str,
    mode: WireNames,
) -> Result<String, ApiError> {
    let uri = api
        .parse::<ureq::http::Uri>()
        .map_err(|error| ApiError::Parse(format!("invalid mirror API URL: {error}")))?;
    let scheme = uri
        .scheme_str()
        .filter(|scheme| matches!(*scheme, "http" | "https"))
        .ok_or_else(|| ApiError::Parse("mirror API URL must use HTTP or HTTPS".to_string()))?;
    let authority = uri
        .authority()
        .ok_or_else(|| ApiError::Parse("mirror API URL has no authority".to_string()))?;
    let wire = package_name::to_wire(name, mode).map_err(ApiError::Parse)?;
    Ok(format!(
        "{scheme}://{authority}/packages/{wire}/{version}.tar.zst"
    ))
}

/// Build a [`ureq::Agent`] whose TCP connector implements Happy Eyeballs
/// (RFC 8305), racing IPv6/IPv4 connections in parallel.
fn build_agent() -> ureq::Agent {
    use ureq::unversioned::resolver::DefaultResolver;
    use ureq::unversioned::transport::{ConnectProxyConnector, Connector, RustlsConnector};

    use crate::happy_eyeballs::HappyEyeballsConnector;

    let connector =
        ().chain(ConnectProxyConnector::default())
            .chain(HappyEyeballsConnector)
            .chain(RustlsConnector::default());

    ureq::Agent::with_parts(
        ureq::config::Config::default(),
        connector,
        DefaultResolver::default(),
    )
}

/// Extract the path component from an absolute URL.
///
/// Given `https://host/path/to/file`, returns `/path/to/file`.
/// Returns the original string if no path separator is found after the host.
fn extract_url_path(url: &str) -> &str {
    let rest = url
        .strip_prefix("https://")
        .or_else(|| url.strip_prefix("http://"))
        .unwrap_or(url);
    rest.find('/').map_or(url, |pos| &rest[pos..])
}

/// Minimal percent-encoding for URL path segments.
///
/// Encodes characters that are not unreserved per RFC 3986.
fn percent_encode(s: &str) -> String {
    let mut out = String::with_capacity(s.len() * 3);
    for b in s.bytes() {
        match b {
            b'A'..=b'Z' | b'a'..=b'z' | b'0'..=b'9' | b'-' | b'_' | b'.' | b'~' => {
                out.push(b as char);
            }
            _ => {
                out.push('%');
                out.push(char::from(b"0123456789ABCDEF"[(b >> 4) as usize]));
                out.push(char::from(b"0123456789ABCDEF"[(b & 0x0f) as usize]));
            }
        }
    }
    out
}

#[cfg(test)]
mod tests {
    use std::sync::Mutex;

    use super::*;

    // Serialize env-var–mutating tests: `std::env::set_var` / `remove_var`
    // are not thread-safe when multiple tests share the same process, and
    // `client_default_url` below pins the env-free default.
    static ENV_LOCK: Mutex<()> = Mutex::new(());

    #[test]
    fn descriptive_metadata_aliases_require_complete_matching_identity() {
        let name = "hew.math.stats";
        for alias in ["hew/math/stats", "hew.math.stats", "hew::math::stats"] {
            assert!(
                metadata_identity_matches(alias, name, WireNames::Slash),
                "{alias}"
            );
        }
        for alias in [
            "hew::math.stats",
            "hew/math::stats",
            "hew.math/stats",
            "hew::::math::stats",
            "::hew::math::stats",
            "hew::math::stats::",
            "hew::..::stats",
            "hew::%2e%2e::stats",
            "hew::math::statistics",
            "hew::math:stats",
            "hew/../stats",
            "hew//math/stats",
            "hew.math..stats",
            "hew.math.statistics",
            "hew/math/statistics",
        ] {
            assert!(
                !metadata_identity_matches(alias, name, WireNames::Slash),
                "{alias}"
            );
        }
        assert!(metadata_identity_matches(name, name, WireNames::Dotted));
        assert!(!metadata_identity_matches(
            "hew/math/stats",
            name,
            WireNames::Dotted
        ));
        assert!(!metadata_identity_matches(
            "hew::math::stats",
            name,
            WireNames::Dotted
        ));
    }

    #[test]
    fn descriptive_metadata_does_not_relax_version_wire_identity() {
        for (metadata, version_name, accepted) in [
            ("hew/math/stats", "hew/math/stats", true),
            ("hew.math.stats", "hew/math/stats", true),
            ("hew::math::stats", "hew/math/stats", true),
            ("hew::math::stats", "hew::math::stats", false),
            ("hew.math.stats", "hew.math.stats", false),
        ] {
            let server = tiny_http::Server::http("127.0.0.1:0").unwrap();
            let api = format!("http://{}/api/v1", server.server_addr());
            let handle = std::thread::spawn(move || {
                let request = server
                    .recv_timeout(std::time::Duration::from_secs(5))
                    .unwrap()
                    .expect("package request");
                let path = request.url().to_string();
                let response = serde_json::json!({
                    "metadata": { "name": metadata }, "versions": [{
                        "name": version_name, "vers": "0.2.0", "cksum": "sha256:fixture",
                        "sig": "", "key_fp": ""
                    }]
                });
                request
                    .respond(tiny_http::Response::from_string(response.to_string()))
                    .unwrap();
                path
            });
            let result = RegistryClient::with_url(api)
                .with_wire_names(WireNames::Slash)
                .get_package("hew.math.stats");
            assert_eq!(handle.join().unwrap(), "/api/v1/packages/hew/math/stats");
            if accepted {
                let entries = result.unwrap();
                assert_eq!(entries[0].name, "hew.math.stats");
                assert_eq!(entries[0].registry_name.as_deref(), Some("hew/math/stats"));
            } else {
                assert!(matches!(result, Err(ApiError::Parse(_))));
            }
        }
    }

    #[test]
    fn response_dependency_aliases_preserve_feature_labels() {
        let mut entry: IndexEntry = serde_json::from_value(serde_json::json!({
            "name": "alice.router", "vers": "1.2.3", "cksum": "sha256:test",
            "sig": "", "key_fp": "", "deps": [
                { "name": "alice/helper", "req": "^1", "optional": true },
                { "name": "bob.tools", "req": "^2" }
            ], "features": {
                "alice/helper": ["alice/helper", "bob.tools", "other/label"]
            }
        }))
        .unwrap();
        normalize_dependencies(&mut entry).unwrap();
        assert_eq!(entry.deps[0].name, "alice.helper");
        assert_eq!(
            entry.features["alice/helper"],
            ["alice.helper", "bob.tools", "other/label"]
        );
        entry.deps.push(entry.deps[0].clone());
        entry.deps.last_mut().unwrap().name = "alice/helper".to_string();
        assert!(normalize_dependencies(&mut entry).is_err());
    }

    #[test]
    fn mirror_archive_routes_preserve_wire_mode_and_semver_identity() {
        assert_eq!(
            mirror_download_url(
                "https://mirror.example/api/v1",
                "alice.router",
                "1.2.3+build.4",
                WireNames::Slash
            )
            .unwrap(),
            "https://mirror.example/packages/alice/router/1.2.3+build.4.tar.zst"
        );
        assert_eq!(
            mirror_download_url(
                "http://localhost:9000/api/v1",
                "alice.router",
                "1.2.3",
                WireNames::Dotted
            )
            .unwrap(),
            "http://localhost:9000/packages/alice.router/1.2.3.tar.zst"
        );
    }

    #[test]
    fn custom_package_urls_preserve_dotted_names() {
        let client = RegistryClient::with_url("https://registry.example.com");
        assert_eq!(
            client.package_url("alice.router"),
            "https://registry.example.com/packages/alice.router"
        );
    }

    #[test]
    fn client_default_url() {
        let _guard = ENV_LOCK.lock().unwrap();
        std::env::remove_var("HEW_REGISTRY");
        let client = RegistryClient::new_with_config(&config::PkgConfig::default());
        assert_eq!(client.api_url, config::DEFAULT_REGISTRY_API);
        assert_eq!(client.wire_names, WireNames::Slash);
        assert_eq!(
            client.cdn_url.as_deref(),
            Some(config::DEFAULT_REGISTRY_CDN)
        );
        assert!(client.token.is_none());
    }

    #[test]
    fn client_honours_hew_registry_env() {
        let _guard = ENV_LOCK.lock().unwrap();
        std::env::set_var("HEW_REGISTRY", "https://registry.internal.example/api/v1");
        let client = RegistryClient::new_with_config(&config::PkgConfig::default());
        std::env::remove_var("HEW_REGISTRY");
        assert_eq!(client.api_url, "https://registry.internal.example/api/v1");
        assert_eq!(client.wire_names, WireNames::Dotted);
        assert!(client.cdn_url.is_none());
        assert!(client.fallback_urls.is_empty());
    }

    #[test]
    fn client_with_token() {
        let client = RegistryClient::new_with_config(&config::PkgConfig::default())
            .with_token("tok123".to_string());
        assert_eq!(client.token.as_deref(), Some("tok123"));
    }

    #[test]
    fn client_custom_url() {
        let client = RegistryClient::with_url("https://internal.example.com/api/v1");
        assert_eq!(client.api_url, "https://internal.example.com/api/v1");
    }

    #[test]
    fn publish_requires_token() {
        let client = RegistryClient::new_with_config(&config::PkgConfig::default());
        let req = PublishRequest {
            metadata: PublishMetadata {
                name: "test".to_string(),
                vers: "0.1.0".to_string(),
                description: "test".to_string(),
                license: "MIT".to_string(),
                authors: vec!["Alice".to_string()],
                deps: vec![],
                features: std::collections::BTreeMap::new(),
                edition: None,
                hew: None,
                keywords: None,
                categories: None,
                homepage: None,
                repository: None,
                documentation: None,
            },
            checksum: "sha256:abc".to_string(),
            signature: "ed25519:def".to_string(),
            key_fingerprint: "SHA256:xyz".to_string(),
        };
        let result = client.publish("test", "0.1.0", b"tarball", &req);
        assert!(matches!(result, Err(ApiError::NotAuthenticated)));
    }

    #[test]
    fn yank_requires_token() {
        let client = RegistryClient::new_with_config(&config::PkgConfig::default());
        let result = client.yank("test", "0.1.0", true, Some("reason"));
        assert!(matches!(result, Err(ApiError::NotAuthenticated)));
    }

    #[test]
    fn register_namespace_requires_token() {
        let client = RegistryClient::new_with_config(&config::PkgConfig::default());
        let result = client.register_namespace("myprefix");
        assert!(matches!(result, Err(ApiError::NotAuthenticated)));
    }

    #[test]
    fn register_key_requires_token() {
        let client = RegistryClient::new_with_config(&config::PkgConfig::default());
        let result = client.register_key("base64key");
        assert!(matches!(result, Err(ApiError::NotAuthenticated)));
    }

    #[test]
    fn percent_encode_fingerprint() {
        // SHA256:{base64} contains `:` which must be encoded.
        let fp = "SHA256:xYzAbCdEfGhIjK";
        let encoded = percent_encode(fp);
        assert_eq!(encoded, "SHA256%3AxYzAbCdEfGhIjK");
    }

    #[test]
    fn percent_encode_preserves_unreserved() {
        assert_eq!(percent_encode("hello-world_1.0"), "hello-world_1.0");
    }

    #[test]
    fn percent_encode_encodes_special() {
        assert_eq!(percent_encode("a/b+c"), "a%2Fb%2Bc");
    }

    #[test]
    fn client_with_fallback() {
        let client = RegistryClient::with_url("https://primary.example.com/api/v1")
            .with_fallback("https://mirror.example.com/api/v1".to_string());
        assert_eq!(
            client
                .fallback_urls
                .iter()
                .map(|endpoint| endpoint.api_url.as_str())
                .collect::<Vec<_>>(),
            vec!["https://mirror.example.com/api/v1"]
        );
        assert_eq!(client.fallback_urls[0].wire_names, WireNames::Dotted);
    }

    #[test]
    fn is_retriable_http_error() {
        assert!(RegistryClient::is_retriable(&ApiError::Http(
            "connection refused".to_string()
        )));
    }

    #[test]
    fn is_retriable_server_500() {
        assert!(RegistryClient::is_retriable(&ApiError::Server {
            status: 500,
            message: "Internal Server Error".to_string(),
        }));
    }

    #[test]
    fn is_retriable_server_502() {
        assert!(RegistryClient::is_retriable(&ApiError::Server {
            status: 502,
            message: "Bad Gateway".to_string(),
        }));
    }

    #[test]
    fn is_retriable_not_for_client_error() {
        assert!(!RegistryClient::is_retriable(&ApiError::Server {
            status: 404,
            message: "Not Found".to_string(),
        }));
    }

    #[test]
    fn is_retriable_not_for_auth() {
        assert!(!RegistryClient::is_retriable(&ApiError::NotAuthenticated));
    }

    #[test]
    fn is_retriable_not_for_parse() {
        assert!(!RegistryClient::is_retriable(&ApiError::Parse(
            "bad json".to_string()
        )));
    }

    #[test]
    fn extract_url_path_https() {
        assert_eq!(
            extract_url_path("https://cdn.hewpkg.com/packages/alice.router/0.1.0.tar.gz"),
            "/packages/alice.router/0.1.0.tar.gz"
        );
    }

    #[test]
    fn extract_url_path_http() {
        assert_eq!(
            extract_url_path("http://localhost:8080/api/v1/packages"),
            "/api/v1/packages"
        );
    }

    #[test]
    fn extract_url_path_no_path() {
        assert_eq!(
            extract_url_path("https://example.com"),
            "https://example.com"
        );
    }
}
