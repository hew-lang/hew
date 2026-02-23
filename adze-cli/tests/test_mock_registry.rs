//! Integration tests for `RegistryClient` against a controllable mock HTTP server.
//!
//! Each test spins up its own `MockRegistry` on a random port, seeds it with
//! whatever data the test needs, then creates a `RegistryClient::with_url()`
//! pointing at that local server.

use std::collections::HashMap;
use std::sync::{Arc, Mutex};
use std::thread;

use adze_cli::client::{ApiError, RegistryClient};
use adze_cli::index::IndexEntry;

// ── Mock registry server ────────────────────────────────────────────────────

/// Behavior for a specific endpoint: return a canned response or an error.
#[derive(Clone, Debug)]
#[allow(
    dead_code,
    reason = "Drop variant reserved for future network-failure tests"
)]
enum MockResponse {
    /// Respond with the given status code and JSON body.
    Json { status: u16, body: String },
    /// Respond with the given status code and raw bytes (for tarball downloads).
    Raw {
        status: u16,
        body: Vec<u8>,
        content_type: String,
    },
    /// Immediately drop the connection (simulates network error).
    Drop,
}

/// A mock HTTP registry server running in-process on a random port.
///
/// # Usage
///
/// ```ignore
/// let mock = MockRegistry::start();
/// mock.seed_package("alice::router", &[entry]);
/// let client = RegistryClient::with_url(mock.api_url());
/// let versions = client.get_package("alice::router").unwrap();
/// ```
struct MockRegistry {
    server: Arc<tiny_http::Server>,
    /// Thread handle for the request-processing loop.
    _handle: thread::JoinHandle<()>,
    /// Seeded packages: name -> list of `IndexEntry`.
    packages: Arc<Mutex<HashMap<String, Vec<IndexEntry>>>>,
    /// Custom route overrides: (method, `path_prefix`) -> canned response.
    /// Checked before the default handlers.
    overrides: Arc<Mutex<Vec<(String, String, MockResponse)>>>,
    /// Base URL including `http://127.0.0.1:{port}/api/v1`.
    base_url: String,
    /// Raw base URL without the `/api/v1` suffix (for tarball serving).
    raw_base_url: String,
}

/// What the router decided to do with a request.
enum RouteResult {
    /// Send a JSON response.
    Json { status: u16, body: String },
    /// Send raw bytes.
    Raw {
        status: u16,
        body: Vec<u8>,
        content_type: String,
    },
    /// Drop the request without responding (simulates network failure).
    Drop,
}

impl MockRegistry {
    /// Start a new mock registry server on a random port.
    fn start() -> Self {
        let server =
            Arc::new(tiny_http::Server::http("127.0.0.1:0").expect("failed to bind mock server"));
        let port = server.server_addr().to_ip().unwrap().port();

        let base_url = format!("http://127.0.0.1:{port}/api/v1");
        let raw_base_url = format!("http://127.0.0.1:{port}");

        let packages: Arc<Mutex<HashMap<String, Vec<IndexEntry>>>> =
            Arc::new(Mutex::new(HashMap::new()));
        let overrides: Arc<Mutex<Vec<(String, String, MockResponse)>>> =
            Arc::new(Mutex::new(Vec::new()));

        let srv = Arc::clone(&server);
        let pkgs = Arc::clone(&packages);
        let ovrs = Arc::clone(&overrides);

        let handle = thread::spawn(move || {
            Self::serve_loop(srv, pkgs, ovrs);
        });

        Self {
            server,
            _handle: handle,
            packages,
            overrides,
            base_url,
            raw_base_url,
        }
    }

    /// The API base URL for use with `RegistryClient::with_url()`.
    fn api_url(&self) -> String {
        self.base_url.clone()
    }

    /// The raw server URL (no `/api/v1` suffix), useful for tarball download
    /// URLs.
    fn raw_url(&self) -> String {
        self.raw_base_url.clone()
    }

    /// Seed the mock with package version entries.
    fn seed_package(&self, name: &str, entries: &[IndexEntry]) {
        self.packages
            .lock()
            .unwrap()
            .insert(name.to_string(), entries.to_vec());
    }

    /// Add a custom response override. The first matching override wins.
    ///
    /// `method` is e.g. `"GET"`, `path_prefix` is matched against the full
    /// request URL path (e.g. `"/api/v1/packages/alice/router"`).
    fn add_override(&self, method: &str, path_prefix: &str, response: MockResponse) {
        self.overrides.lock().unwrap().push((
            method.to_uppercase(),
            path_prefix.to_string(),
            response,
        ));
    }

    /// Convenience: add a 500 response for a specific GET path.
    fn fail_with_500(&self, path_prefix: &str) {
        self.add_override(
            "GET",
            path_prefix,
            MockResponse::Json {
                status: 500,
                body: r#"{"error":"Internal Server Error"}"#.to_string(),
            },
        );
    }

    /// Convenience: add a 404 response for a specific GET path.
    fn fail_with_404(&self, path_prefix: &str) {
        self.add_override(
            "GET",
            path_prefix,
            MockResponse::Json {
                status: 404,
                body: r#"{"error":"Not Found"}"#.to_string(),
            },
        );
    }

    /// Serve a raw tarball at a specific path.
    fn serve_tarball(&self, path: &str, data: Vec<u8>) {
        self.add_override(
            "GET",
            path,
            MockResponse::Raw {
                status: 200,
                body: data,
                content_type: "application/octet-stream".to_string(),
            },
        );
    }

    /// The main request-handling loop. Runs until the server is shut down.
    #[allow(
        clippy::needless_pass_by_value,
        reason = "Arc parameters are moved into thread::spawn"
    )]
    fn serve_loop(
        server: Arc<tiny_http::Server>,
        packages: Arc<Mutex<HashMap<String, Vec<IndexEntry>>>>,
        overrides: Arc<Mutex<Vec<(String, String, MockResponse)>>>,
    ) {
        loop {
            let Ok(request) = server.recv() else {
                // Server was shut down.
                break;
            };

            let method = request.method().as_str().to_uppercase();
            let url = request.url().to_string();

            // Determine the response.
            let result = Self::route(&method, &url, &packages, &overrides);

            // Send the response.
            match result {
                RouteResult::Json { status, body } => {
                    let response = tiny_http::Response::from_string(body)
                        .with_status_code(status)
                        .with_header(
                            "Content-Type: application/json"
                                .parse::<tiny_http::Header>()
                                .unwrap(),
                        );
                    let _ = request.respond(response);
                }
                RouteResult::Raw {
                    status,
                    body,
                    content_type,
                } => {
                    let header_str = format!("Content-Type: {content_type}");
                    let len = body.len();
                    let cursor = std::io::Cursor::new(body);
                    let response = tiny_http::Response::new(
                        tiny_http::StatusCode(status),
                        vec![header_str.parse::<tiny_http::Header>().unwrap()],
                        cursor,
                        Some(len),
                        None,
                    );
                    let _ = request.respond(response);
                }
                RouteResult::Drop => {
                    // Just drop the request without responding.
                    drop(request);
                }
            }
        }
    }

    /// Determine the response for a given request. This is a pure function
    /// over the shared state — it never touches the request itself.
    fn route(
        method: &str,
        url: &str,
        packages: &Mutex<HashMap<String, Vec<IndexEntry>>>,
        overrides: &Mutex<Vec<(String, String, MockResponse)>>,
    ) -> RouteResult {
        // Check overrides first.
        {
            let ovrs = overrides.lock().unwrap();
            for (m, prefix, resp) in ovrs.iter() {
                if *m == method && url.starts_with(prefix) {
                    return match resp {
                        MockResponse::Json { status, body } => RouteResult::Json {
                            status: *status,
                            body: body.clone(),
                        },
                        MockResponse::Raw {
                            status,
                            body,
                            content_type,
                        } => RouteResult::Raw {
                            status: *status,
                            body: body.clone(),
                            content_type: content_type.clone(),
                        },
                        MockResponse::Drop => RouteResult::Drop,
                    };
                }
            }
        }

        // Default routing.
        if method == "GET" {
            Self::route_get(url, packages)
        } else {
            RouteResult::Json {
                status: 405,
                body: r#"{"error":"Method Not Allowed"}"#.to_string(),
            }
        }
    }

    /// Route a GET request via default handlers.
    fn route_get(url: &str, packages: &Mutex<HashMap<String, Vec<IndexEntry>>>) -> RouteResult {
        // Strip the `/api/v1` prefix for matching.
        let path = url.strip_prefix("/api/v1").unwrap_or(url);

        if path.starts_with("/packages/") {
            Self::route_get_package(path, packages)
        } else if path.starts_with("/search") {
            Self::route_search(path, packages)
        } else if path == "/registry-key" {
            Self::route_registry_key()
        } else if path.starts_with("/keys/") {
            Self::route_get_public_key(path)
        } else if path.starts_with("/namespaces/") {
            Self::route_get_namespace(path)
        } else {
            RouteResult::Json {
                status: 404,
                body: r#"{"error":"Not Found"}"#.to_string(),
            }
        }
    }

    /// GET /api/v1/packages/{namespace}/{name} or /api/v1/packages/{name}
    fn route_get_package(
        path: &str,
        packages: &Mutex<HashMap<String, Vec<IndexEntry>>>,
    ) -> RouteResult {
        // path = "/packages/alice/router" or "/packages/simple"
        let pkg_path = path.strip_prefix("/packages/").unwrap_or("");

        // Reconstruct the package name: "alice/router" -> "alice::router"
        let pkg_name = pkg_path.replace('/', "::");

        let pkgs = packages.lock().unwrap();
        if let Some(entries) = pkgs.get(&pkg_name) {
            let body = serde_json::json!({ "versions": entries });
            RouteResult::Json {
                status: 200,
                body: body.to_string(),
            }
        } else {
            RouteResult::Json {
                status: 404,
                body: r#"{"error":"package not found"}"#.to_string(),
            }
        }
    }

    /// `GET /api/v1/search?q=...&page=...&per_page=...`
    fn route_search(path: &str, packages: &Mutex<HashMap<String, Vec<IndexEntry>>>) -> RouteResult {
        // Parse query string.
        let query_str = path.split_once('?').map_or("", |(_, q)| q);
        let mut q = String::new();
        let mut page: u32 = 1;
        let mut per_page: u32 = 20;

        for param in query_str.split('&') {
            if let Some((key, val)) = param.split_once('=') {
                match key {
                    "q" => q = val.to_string(),
                    "page" => page = val.parse().unwrap_or(1),
                    "per_page" => per_page = val.parse().unwrap_or(20),
                    _ => {}
                }
            }
        }

        let pkgs = packages.lock().unwrap();
        let q_lower = q.to_lowercase();

        // Simple substring search over package names.
        let mut all_hits: Vec<serde_json::Value> = pkgs
            .iter()
            .filter(|(name, _)| name.to_lowercase().contains(&q_lower))
            .map(|(name, entries)| {
                let latest = entries.last();
                serde_json::json!({
                    "name": name,
                    "description": null,
                    "latest_version": latest.map_or("0.0.0", |e| &e.vers),
                    "downloads": null,
                })
            })
            .collect();

        // Sort for deterministic pagination.
        all_hits.sort_by(|a, b| {
            a["name"]
                .as_str()
                .unwrap_or("")
                .cmp(b["name"].as_str().unwrap_or(""))
        });

        let total = all_hits.len();
        let start = ((page - 1) * per_page) as usize;
        let page_hits: Vec<_> = all_hits
            .into_iter()
            .skip(start)
            .take(per_page as usize)
            .collect();

        let body = serde_json::json!({
            "results": page_hits,
            "total": total,
        });

        RouteResult::Json {
            status: 200,
            body: body.to_string(),
        }
    }

    /// GET /api/v1/registry-key -- return a fixed mock registry key.
    fn route_registry_key() -> RouteResult {
        let body = serde_json::json!({
            "key_id": "registry-key-001",
            "public_key": "bW9jay1yZWdpc3RyeS1rZXk=",
            "algorithm": "ed25519",
        });
        RouteResult::Json {
            status: 200,
            body: body.to_string(),
        }
    }

    /// GET /api/v1/keys/{fingerprint} -- return a mock public key.
    fn route_get_public_key(path: &str) -> RouteResult {
        let fp_encoded = path.strip_prefix("/keys/").unwrap_or("");
        // URL-decode the fingerprint for the response.
        let fp = percent_decode(fp_encoded);

        let body = serde_json::json!({
            "fingerprint": fp,
            "public_key": "bW9jay1wdWJsaWMta2V5",
            "key_type": "ed25519",
            "github_user": "mock-user",
            "github_id": 12345,
        });
        RouteResult::Json {
            status: 200,
            body: body.to_string(),
        }
    }

    /// GET /api/v1/namespaces/{prefix} -- return mock namespace info.
    fn route_get_namespace(path: &str) -> RouteResult {
        let prefix = path.strip_prefix("/namespaces/").unwrap_or("");
        let body = serde_json::json!({
            "prefix": prefix,
            "owner": "mock-owner",
            "source": "github",
        });
        RouteResult::Json {
            status: 200,
            body: body.to_string(),
        }
    }
}

impl Drop for MockRegistry {
    fn drop(&mut self) {
        self.server.unblock();
    }
}

// ── Helpers ─────────────────────────────────────────────────────────────────

/// Create a sample `IndexEntry` for testing.
fn sample_entry(name: &str, vers: &str) -> IndexEntry {
    IndexEntry {
        name: name.to_string(),
        vers: vers.to_string(),
        deps: vec![],
        features: std::collections::BTreeMap::new(),
        cksum: "sha256:abc123".to_string(),
        sig: "ed25519:def456".to_string(),
        key_fp: "SHA256:xyz".to_string(),
        yanked: adze_cli::index::YankStatus::Bool(false),
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

/// Minimal percent-decoding for URL path segments.
fn percent_decode(s: &str) -> String {
    let mut out = Vec::new();
    let bytes = s.as_bytes();
    let mut i = 0;
    while i < bytes.len() {
        if bytes[i] == b'%' && i + 2 < bytes.len() {
            let hi = hex_val(bytes[i + 1]);
            let lo = hex_val(bytes[i + 2]);
            if let (Some(h), Some(l)) = (hi, lo) {
                out.push((h << 4) | l);
                i += 3;
                continue;
            }
        }
        out.push(bytes[i]);
        i += 1;
    }
    String::from_utf8(out).unwrap_or_else(|_| s.to_string())
}

fn hex_val(b: u8) -> Option<u8> {
    match b {
        b'0'..=b'9' => Some(b - b'0'),
        b'A'..=b'F' => Some(b - b'A' + 10),
        b'a'..=b'f' => Some(b - b'a' + 10),
        _ => None,
    }
}

// ── Tests ───────────────────────────────────────────────────────────────────

// ── Basic CRUD endpoint tests ───────────────────────────────────────────────

#[test]
fn get_package_returns_seeded_versions() {
    let mock = MockRegistry::start();
    mock.seed_package(
        "alice::router",
        &[
            sample_entry("alice::router", "0.1.0"),
            sample_entry("alice::router", "0.2.0"),
            sample_entry("alice::router", "1.0.0"),
        ],
    );

    let client = RegistryClient::with_url(mock.api_url());
    let versions = client.get_package("alice::router").unwrap();
    assert_eq!(versions.len(), 3);
    assert_eq!(versions[0].vers, "0.1.0");
    assert_eq!(versions[1].vers, "0.2.0");
    assert_eq!(versions[2].vers, "1.0.0");
    assert_eq!(versions[0].name, "alice::router");
}

#[test]
fn get_package_not_found_returns_404_error() {
    let mock = MockRegistry::start();
    // No packages seeded.

    let client = RegistryClient::with_url(mock.api_url());
    let result = client.get_package("nonexistent::pkg");
    match result {
        Err(ApiError::Server { status, .. }) => assert_eq!(status, 404),
        other => panic!("expected Server 404 error, got {other:?}"),
    }
}

#[test]
fn get_package_single_segment_name() {
    let mock = MockRegistry::start();
    mock.seed_package("simple", &[sample_entry("simple", "1.0.0")]);

    let client = RegistryClient::with_url(mock.api_url());
    let versions = client.get_package("simple").unwrap();
    assert_eq!(versions.len(), 1);
    assert_eq!(versions[0].name, "simple");
}

#[test]
fn search_returns_matching_packages() {
    let mock = MockRegistry::start();
    mock.seed_package("alice::router", &[sample_entry("alice::router", "1.0.0")]);
    mock.seed_package("alice::logger", &[sample_entry("alice::logger", "0.5.0")]);
    mock.seed_package("bob::router", &[sample_entry("bob::router", "2.0.0")]);

    let client = RegistryClient::with_url(mock.api_url());
    let result = client.search("router", None, 1, 20).unwrap();
    assert_eq!(result.total, 2);
    assert_eq!(result.results.len(), 2);

    let names: Vec<&str> = result.results.iter().map(|h| h.name.as_str()).collect();
    assert!(names.contains(&"alice::router"));
    assert!(names.contains(&"bob::router"));
}

#[test]
fn search_returns_empty_for_no_match() {
    let mock = MockRegistry::start();
    mock.seed_package("alice::router", &[sample_entry("alice::router", "1.0.0")]);

    let client = RegistryClient::with_url(mock.api_url());
    let result = client.search("nonexistent", None, 1, 20).unwrap();
    assert_eq!(result.total, 0);
    assert!(result.results.is_empty());
}

#[test]
fn search_pagination() {
    let mock = MockRegistry::start();
    // Seed several packages.
    for i in 0..5 {
        let name = format!("pkg{i}");
        mock.seed_package(&name, &[sample_entry(&name, "1.0.0")]);
    }

    let client = RegistryClient::with_url(mock.api_url());

    // Request page 1 with 2 per page.
    let result = client.search("pkg", None, 1, 2).unwrap();
    assert_eq!(result.total, 5);
    assert_eq!(result.results.len(), 2);

    // Request page 3 with 2 per page should return 1 result.
    let result = client.search("pkg", None, 3, 2).unwrap();
    assert_eq!(result.total, 5);
    assert_eq!(result.results.len(), 1);
}

#[test]
fn download_tarball_returns_bytes() {
    let mock = MockRegistry::start();
    let tarball_data = b"fake-tarball-contents-1234567890".to_vec();
    mock.serve_tarball("/packages/alice/router/1.0.0.tar.zst", tarball_data.clone());

    let client = RegistryClient::with_url(mock.api_url());
    let download_url = format!("{}/packages/alice/router/1.0.0.tar.zst", mock.raw_url());
    let result = client.download_tarball(&download_url).unwrap();
    assert_eq!(result, tarball_data);
}

#[test]
fn download_tarball_404_returns_error() {
    let mock = MockRegistry::start();
    // No tarball served.

    let client = RegistryClient::with_url(mock.api_url());
    let download_url = format!("{}/packages/alice/router/1.0.0.tar.zst", mock.raw_url());
    let result = client.download_tarball(&download_url);
    match result {
        Err(ApiError::Server { status, .. }) => assert_eq!(status, 404),
        other => panic!("expected Server 404 error, got {other:?}"),
    }
}

#[test]
fn get_registry_key_returns_key_info() {
    let mock = MockRegistry::start();

    let client = RegistryClient::with_url(mock.api_url());
    let key = client.get_registry_key().unwrap();
    assert_eq!(key.key_id, "registry-key-001");
    assert_eq!(key.algorithm, "ed25519");
    assert!(!key.public_key.is_empty());
}

#[test]
fn get_public_key_returns_key_record() {
    let mock = MockRegistry::start();

    let client = RegistryClient::with_url(mock.api_url());
    let key = client.get_public_key("SHA256:xYzAbCdEfGhIjK").unwrap();
    assert_eq!(key.fingerprint, "SHA256:xYzAbCdEfGhIjK");
    assert_eq!(key.key_type, "ed25519");
    assert_eq!(key.github_user, "mock-user");
    assert_eq!(key.github_id, 12345);
}

#[test]
fn get_namespace_returns_info() {
    let mock = MockRegistry::start();

    let client = RegistryClient::with_url(mock.api_url());
    let info = client.get_namespace("alice").unwrap();
    assert_eq!(info.prefix, "alice");
    assert_eq!(info.owner, "mock-owner");
    assert_eq!(info.source, "github");
}

// ── Fallback behavior tests ────────────────────────────────────────────────

#[test]
fn fallback_on_primary_500() {
    let primary = MockRegistry::start();
    let fallback = MockRegistry::start();

    // Primary always returns 500 for packages.
    primary.fail_with_500("/api/v1/packages");
    // Fallback has the data.
    fallback.seed_package("alice::router", &[sample_entry("alice::router", "1.0.0")]);

    let client = RegistryClient::with_url(primary.api_url()).with_fallback(fallback.api_url());

    let versions = client.get_package("alice::router").unwrap();
    assert_eq!(versions.len(), 1);
    assert_eq!(versions[0].vers, "1.0.0");
}

#[test]
fn fallback_on_primary_500_for_search() {
    let primary = MockRegistry::start();
    let fallback = MockRegistry::start();

    primary.fail_with_500("/api/v1/search");
    fallback.seed_package("alice::router", &[sample_entry("alice::router", "1.0.0")]);

    let client = RegistryClient::with_url(primary.api_url()).with_fallback(fallback.api_url());

    let result = client.search("router", None, 1, 20).unwrap();
    assert_eq!(result.total, 1);
}

#[test]
fn fallback_on_primary_500_for_registry_key() {
    let primary = MockRegistry::start();
    let fallback = MockRegistry::start();

    primary.fail_with_500("/api/v1/registry-key");

    let client = RegistryClient::with_url(primary.api_url()).with_fallback(fallback.api_url());

    let key = client.get_registry_key().unwrap();
    assert_eq!(key.key_id, "registry-key-001");
}

#[test]
fn fallback_on_primary_500_for_public_key() {
    let primary = MockRegistry::start();
    let fallback = MockRegistry::start();

    primary.fail_with_500("/api/v1/keys");

    let client = RegistryClient::with_url(primary.api_url()).with_fallback(fallback.api_url());

    let key = client.get_public_key("SHA256:abc").unwrap();
    assert_eq!(key.fingerprint, "SHA256:abc");
}

#[test]
fn no_fallback_on_4xx_error() {
    let primary = MockRegistry::start();
    let fallback = MockRegistry::start();

    // Primary returns 404 (a client error, not retriable).
    // The client should NOT try the fallback.
    // We verify this by checking that a 404 error is returned even though
    // the fallback has the package.
    fallback.seed_package("alice::router", &[sample_entry("alice::router", "1.0.0")]);

    let client = RegistryClient::with_url(primary.api_url()).with_fallback(fallback.api_url());

    let result = client.get_package("alice::router");
    match result {
        Err(ApiError::Server { status, .. }) => assert_eq!(status, 404),
        other => panic!("expected Server 404 error (no fallback), got {other:?}"),
    }
}

#[test]
fn no_fallback_on_404_for_search() {
    // Override search on primary to return 404 instead of a normal response.
    let primary = MockRegistry::start();
    let fallback = MockRegistry::start();

    primary.fail_with_404("/api/v1/search");
    fallback.seed_package("alice::router", &[sample_entry("alice::router", "1.0.0")]);

    let client = RegistryClient::with_url(primary.api_url()).with_fallback(fallback.api_url());

    let result = client.search("router", None, 1, 20);
    match result {
        Err(ApiError::Server { status, .. }) => assert_eq!(status, 404),
        other => panic!("expected Server 404 error (no fallback), got {other:?}"),
    }
}

#[test]
fn tarball_fallback_on_primary_500() {
    let primary = MockRegistry::start();
    let fallback = MockRegistry::start();

    let tarball_data = b"real-tarball-bytes".to_vec();
    // Primary returns 500 for tarballs.
    primary.add_override(
        "GET",
        "/packages/",
        MockResponse::Json {
            status: 500,
            body: r#"{"error":"Internal Server Error"}"#.to_string(),
        },
    );
    // Fallback serves the tarball.
    fallback.serve_tarball("/packages/alice/router/1.0.0.tar.zst", tarball_data.clone());

    let client = RegistryClient::with_url(primary.api_url()).with_fallback(fallback.raw_url());

    // Primary URL for the tarball.
    let download_url = format!("{}/packages/alice/router/1.0.0.tar.zst", primary.raw_url());
    let result = client.download_tarball(&download_url).unwrap();
    assert_eq!(result, tarball_data);
}

#[test]
fn tarball_no_fallback_on_404() {
    let primary = MockRegistry::start();
    let fallback = MockRegistry::start();

    let tarball_data = b"tarball-on-fallback".to_vec();
    // Primary returns 404 for the tarball.
    primary.fail_with_404("/packages/alice/router/1.0.0.tar.zst");
    // Fallback has the tarball.
    fallback.serve_tarball("/packages/alice/router/1.0.0.tar.zst", tarball_data);

    let client = RegistryClient::with_url(primary.api_url()).with_fallback(fallback.raw_url());

    let download_url = format!("{}/packages/alice/router/1.0.0.tar.zst", primary.raw_url());
    let result = client.download_tarball(&download_url);
    // 404 is NOT retriable, so the fallback should not be tried.
    match result {
        Err(ApiError::Server { status, .. }) => assert_eq!(status, 404),
        other => panic!("expected Server 404 error (no fallback), got {other:?}"),
    }
}

#[test]
fn multiple_fallbacks_tried_in_order() {
    let primary = MockRegistry::start();
    let fallback1 = MockRegistry::start();
    let fallback2 = MockRegistry::start();

    primary.fail_with_500("/api/v1/packages");
    fallback1.fail_with_500("/api/v1/packages");
    fallback2.seed_package("alice::router", &[sample_entry("alice::router", "3.0.0")]);

    let client = RegistryClient::with_url(primary.api_url())
        .with_fallback(fallback1.api_url())
        .with_fallback(fallback2.api_url());

    let versions = client.get_package("alice::router").unwrap();
    assert_eq!(versions.len(), 1);
    assert_eq!(versions[0].vers, "3.0.0");
}

#[test]
fn all_fallbacks_fail_returns_last_error() {
    let primary = MockRegistry::start();
    let fallback1 = MockRegistry::start();
    let fallback2 = MockRegistry::start();

    primary.fail_with_500("/api/v1/packages");
    fallback1.fail_with_500("/api/v1/packages");
    fallback2.fail_with_500("/api/v1/packages");

    let client = RegistryClient::with_url(primary.api_url())
        .with_fallback(fallback1.api_url())
        .with_fallback(fallback2.api_url());

    let result = client.get_package("alice::router");
    match result {
        Err(ApiError::Server { status, .. }) => assert_eq!(status, 500),
        other => panic!("expected Server 500 error, got {other:?}"),
    }
}

// ── Package data integrity tests ────────────────────────────────────────────

#[test]
fn package_entries_preserve_all_fields() {
    let mock = MockRegistry::start();

    let mut entry = sample_entry("alice::router", "1.0.0");
    entry.deps.push(adze_cli::index::IndexDep {
        name: "std::net::http".to_string(),
        req: "^2.0".to_string(),
        features: vec!["tls".to_string()],
        optional: false,
        default_features: true,
        registry: None,
    });
    entry
        .features
        .insert("default".to_string(), vec!["json".to_string()]);
    entry.edition = Some("2026".to_string());
    entry.hew = Some("0.1.0".to_string());
    entry.dl = Some("https://cdn.example.com/alice/router/1.0.0.tar.zst".to_string());

    mock.seed_package("alice::router", &[entry]);

    let client = RegistryClient::with_url(mock.api_url());
    let versions = client.get_package("alice::router").unwrap();
    assert_eq!(versions.len(), 1);
    let v = &versions[0];
    assert_eq!(v.name, "alice::router");
    assert_eq!(v.vers, "1.0.0");
    assert_eq!(v.cksum, "sha256:abc123");
    assert_eq!(v.sig, "ed25519:def456");
    assert_eq!(v.key_fp, "SHA256:xyz");
    assert_eq!(v.deps.len(), 1);
    assert_eq!(v.deps[0].name, "std::net::http");
    assert_eq!(v.deps[0].req, "^2.0");
    assert_eq!(v.deps[0].features, vec!["tls"]);
    assert_eq!(v.features["default"], vec!["json"]);
    assert_eq!(v.edition.as_deref(), Some("2026"));
    assert_eq!(v.hew.as_deref(), Some("0.1.0"));
    assert!(v.dl.is_some());
}

#[test]
fn get_package_with_yanked_versions() {
    let mock = MockRegistry::start();

    let normal = sample_entry("pkg", "1.0.0");
    let mut yanked = sample_entry("pkg", "2.0.0");
    yanked.yanked = adze_cli::index::YankStatus::Bool(true);
    yanked.yanked_reason = Some("CVE-2026-0001".to_string());

    mock.seed_package("pkg", &[normal, yanked]);

    let client = RegistryClient::with_url(mock.api_url());
    let versions = client.get_package("pkg").unwrap();
    assert_eq!(versions.len(), 2);
    assert!(!versions[0].yanked.is_yanked());
    assert!(versions[1].yanked.is_yanked());
    assert_eq!(versions[1].yanked_reason.as_deref(), Some("CVE-2026-0001"));
}

// ── Custom override tests ───────────────────────────────────────────────────

#[test]
fn custom_override_json_response() {
    let mock = MockRegistry::start();
    mock.add_override(
        "GET",
        "/api/v1/registry-key",
        MockResponse::Json {
            status: 200,
            body: serde_json::json!({
                "key_id": "custom-key-42",
                "public_key": "Y3VzdG9tLWtleQ==",
                "algorithm": "ed25519",
            })
            .to_string(),
        },
    );

    let client = RegistryClient::with_url(mock.api_url());
    let key = client.get_registry_key().unwrap();
    assert_eq!(key.key_id, "custom-key-42");
}

// ── Isolation test ──────────────────────────────────────────────────────────

#[test]
fn each_mock_server_is_isolated() {
    let mock1 = MockRegistry::start();
    let mock2 = MockRegistry::start();

    mock1.seed_package("pkg1", &[sample_entry("pkg1", "1.0.0")]);
    mock2.seed_package("pkg2", &[sample_entry("pkg2", "2.0.0")]);

    let client1 = RegistryClient::with_url(mock1.api_url());
    let client2 = RegistryClient::with_url(mock2.api_url());

    // client1 can see pkg1 but not pkg2.
    assert!(client1.get_package("pkg1").is_ok());
    assert!(matches!(
        client1.get_package("pkg2"),
        Err(ApiError::Server { status: 404, .. })
    ));

    // client2 can see pkg2 but not pkg1.
    assert!(client2.get_package("pkg2").is_ok());
    assert!(matches!(
        client2.get_package("pkg1"),
        Err(ApiError::Server { status: 404, .. })
    ));
}

#[test]
fn servers_use_different_ports() {
    let mock1 = MockRegistry::start();
    let mock2 = MockRegistry::start();
    assert_ne!(mock1.api_url(), mock2.api_url());
}
