//! HTTP client for the Adze package registry API.
//!
//! Communicates with the Cloudflare Workers API for publishing, yanking,
//! searching, namespace management, and key registration.

use std::fmt;

use serde::{Deserialize, Serialize};

use crate::config;
use crate::index::IndexEntry;

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
            Self::NotAuthenticated => write!(f, "not authenticated; run `adze login`"),
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

/// An Adze registry API client.
#[derive(Debug)]
pub struct RegistryClient {
    api_url: String,
    fallback_urls: Vec<String>,
    /// CDN base URL for package downloads.
    cdn_url: Option<String>,
    token: Option<String>,
}

impl RegistryClient {
    /// Create a client for the official Hew registry using compiled-in defaults.
    ///
    /// Loads `~/.adze/config.toml` to check for a `fallback-api` override.
    #[must_use]
    pub fn new() -> Self {
        let endpoints = config::discover_registry();
        let cfg = config::load_config();

        // Config fallback-api overrides the compiled-in default.
        let fallback_api = cfg
            .registry
            .as_ref()
            .and_then(|r| r.fallback_api.clone())
            .or(endpoints.fallback_api);

        let mut client = Self::with_url(endpoints.api);
        client.cdn_url = Some(endpoints.cdn);
        if let Some(url) = fallback_api {
            client = client.with_fallback(url);
        }
        client
    }

    /// Create a client for a named registry.
    #[must_use]
    pub fn with_url(api_url: String) -> Self {
        Self {
            api_url,
            fallback_urls: Vec::new(),
            cdn_url: None,
            token: None,
        }
    }

    /// Add a fallback URL to try when the primary is unavailable.
    #[must_use]
    pub fn with_fallback(mut self, url: String) -> Self {
        self.fallback_urls.push(url);
        self
    }

    /// Set the authentication token.
    #[must_use]
    pub fn with_token(mut self, token: String) -> Self {
        self.token = Some(token);
        self
    }

    /// Start the GitHub OAuth device flow.
    ///
    /// # Errors
    ///
    /// Returns [`ApiError`] on HTTP or parse failures.
    pub fn login_device(&self) -> Result<DeviceFlowResponse, ApiError> {
        let url = format!("{}/login/device", self.api_url);
        let resp = ureq::post(&url).send_empty().map_err(map_ureq_error)?;

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

        let resp = ureq::post(&url).send_json(&body).map_err(map_ureq_error)?;

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
        let token = self.token.as_ref().ok_or(ApiError::NotAuthenticated)?;
        let url = format!(
            "{}/packages/{}/{}",
            self.api_url,
            encode_name(name),
            version
        );

        let metadata_json =
            serde_json::to_string(request).map_err(|e| ApiError::Parse(e.to_string()))?;

        use base64::Engine as _;
        let tarball_b64 = base64::engine::general_purpose::STANDARD.encode(tarball);

        let body = serde_json::json!({
            "metadata": metadata_json,
            "tarball": tarball_b64,
        });

        let resp = ureq::put(&url)
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
        let url = format!(
            "{}/packages/{}/{}/yank",
            self.api_url,
            encode_name(name),
            version
        );

        let mut body = serde_json::json!({ "yanked": yanked });
        if let Some(r) = reason {
            body["reason"] = serde_json::Value::String(r.to_string());
        }

        let resp = ureq::patch(&url)
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
        self.try_with_fallback(|base_url| {
            let mut url = format!(
                "{}/search?q={}&page={}&per_page={}",
                base_url, query, page, per_page
            );
            if let Some(cat) = category {
                url.push_str(&format!("&category={cat}"));
            }

            let resp = ureq::get(&url).call().map_err(map_ureq_error)?;

            if resp.status().as_u16() != 200 {
                return Err(self.parse_error_response(resp));
            }

            resp.into_body()
                .read_json()
                .map_err(|e| ApiError::Parse(e.to_string()))
        })
    }

    /// Get all versions of a package.
    ///
    /// # Errors
    ///
    /// Returns [`ApiError`] on HTTP or parse failures.
    pub fn get_package(&self, name: &str) -> Result<Vec<IndexEntry>, ApiError> {
        self.try_with_fallback(|base_url| {
            let url = format!("{}/packages/{}", base_url, encode_name(name));

            let resp = ureq::get(&url).call().map_err(map_ureq_error)?;

            if resp.status().as_u16() != 200 {
                return Err(self.parse_error_response(resp));
            }

            #[derive(Deserialize)]
            struct PackageRecord {
                versions: Vec<IndexEntry>,
            }

            let record: PackageRecord = resp
                .into_body()
                .read_json()
                .map_err(|e| ApiError::Parse(e.to_string()))?;
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
        let url = format!("{}/namespaces/{}", self.api_url, prefix);

        let resp = ureq::put(&url)
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
        self.try_with_fallback(|base_url| {
            let url = format!("{}/namespaces/{}", base_url, prefix);

            let resp = ureq::get(&url).call().map_err(map_ureq_error)?;

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
        let token = self.token.as_ref().ok_or(ApiError::NotAuthenticated)?;
        let url = format!("{}/keys", self.api_url);

        let body = serde_json::json!({
            "public_key": public_key_b64,
            "key_type": "ed25519",
        });

        let resp = ureq::put(&url)
            .header("Authorization", &format!("Bearer {token}"))
            .send_json(&body)
            .map_err(map_ureq_error)?;

        if resp.status().as_u16() != 200 && resp.status().as_u16() != 201 {
            return Err(self.parse_error_response(resp));
        }

        #[derive(Deserialize)]
        struct KeyResponse {
            fingerprint: String,
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
        let token = self.token.as_ref().ok_or(ApiError::NotAuthenticated)?;
        let url = format!("{}/packages/{}/deprecate", self.api_url, encode_name(name));

        let mut body = serde_json::json!({ "deprecated": true });
        if let Some(msg) = message {
            body["message"] = serde_json::Value::String(msg.to_string());
        }
        if let Some(succ) = successor {
            body["successor"] = serde_json::Value::String(succ.to_string());
        }

        let resp = ureq::patch(&url)
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
        let do_download = |download_url: &str| -> Result<Vec<u8>, ApiError> {
            let resp = ureq::get(download_url).call().map_err(map_ureq_error)?;

            if resp.status().as_u16() != 200 {
                return Err(self.parse_error_response(resp));
            }

            use std::io::Read as _;
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
                    let cdn_download = format!("{}{}", cdn.trim_end_matches('/'), path);
                    match do_download(&cdn_download) {
                        Ok(data) => return Ok(data),
                        Err(e) if Self::is_retriable(&e) => {
                            last_err = e;
                        }
                        Err(e) => return Err(e),
                    }
                }

                for fallback_url in &self.fallback_urls {
                    let fallback_download =
                        format!("{}{}", fallback_url.trim_end_matches('/'), path);
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
        self.try_with_fallback(|base_url| {
            let url = format!("{}/keys/{}", base_url, encoded_fp);
            let resp = ureq::get(&url).call().map_err(map_ureq_error)?;

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
        self.try_with_fallback(|base_url| {
            let url = format!("{}/registry-key", base_url);
            let resp = ureq::get(&url).call().map_err(map_ureq_error)?;

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
    fn try_with_fallback<T>(&self, f: impl Fn(&str) -> Result<T, ApiError>) -> Result<T, ApiError> {
        match f(&self.api_url) {
            Ok(val) => Ok(val),
            Err(err) if Self::is_retriable(&err) => {
                if self.fallback_urls.is_empty() {
                    return Err(err);
                }
                eprintln!("warning: primary registry unavailable, trying fallback...");
                let mut last_err = err;
                for fallback_url in &self.fallback_urls {
                    match f(fallback_url) {
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
    fn parse_error_response(&self, resp: ureq::http::Response<ureq::Body>) -> ApiError {
        let status: u16 = resp.status().into();

        #[derive(Deserialize)]
        struct ErrorBody {
            message: Option<String>,
            error: Option<String>,
        }

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

/// Encode a package name for use in URL paths.
///
/// Replaces `::` with `/` for the API path format.
fn encode_name(name: &str) -> String {
    name.replace("::", "/")
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
    use super::*;

    #[test]
    fn encode_name_replaces_colons() {
        assert_eq!(encode_name("alice::router"), "alice/router");
        assert_eq!(encode_name("std::net::http"), "std/net/http");
        assert_eq!(encode_name("simple"), "simple");
    }

    #[test]
    fn client_default_url() {
        let client = RegistryClient::new();
        assert_eq!(client.api_url, config::DEFAULT_REGISTRY_API);
        assert!(client.token.is_none());
    }

    #[test]
    fn client_with_token() {
        let client = RegistryClient::new().with_token("tok123".to_string());
        assert_eq!(client.token.as_deref(), Some("tok123"));
    }

    #[test]
    fn client_custom_url() {
        let client = RegistryClient::with_url("https://internal.example.com/api/v1".to_string());
        assert_eq!(client.api_url, "https://internal.example.com/api/v1");
    }

    #[test]
    fn publish_requires_token() {
        let client = RegistryClient::new();
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
        let client = RegistryClient::new();
        let result = client.yank("test", "0.1.0", true, Some("reason"));
        assert!(matches!(result, Err(ApiError::NotAuthenticated)));
    }

    #[test]
    fn register_namespace_requires_token() {
        let client = RegistryClient::new();
        let result = client.register_namespace("myprefix");
        assert!(matches!(result, Err(ApiError::NotAuthenticated)));
    }

    #[test]
    fn register_key_requires_token() {
        let client = RegistryClient::new();
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
        let client = RegistryClient::with_url("https://primary.example.com/api/v1".to_string())
            .with_fallback("https://mirror.example.com/api/v1".to_string());
        assert_eq!(
            client.fallback_urls,
            vec!["https://mirror.example.com/api/v1"]
        );
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
            extract_url_path("https://pkg.adze.sh/packages/alice/router/0.1.0.tar.gz"),
            "/packages/alice/router/0.1.0.tar.gz"
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
