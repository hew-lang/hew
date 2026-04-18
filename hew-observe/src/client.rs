//! HTTP client for fetching runtime profiler data.

use std::io::{self, BufRead, Read, Write};
#[cfg(unix)]
use std::os::unix::net::UnixStream;
use std::path::{Path, PathBuf};
use std::time::Duration;

use serde::Deserialize;

/// Typed failure categories from a profiler client request.
///
/// Available via [`ProfilerClient::last_error`] after any `fetch_*` call
/// returns `None`, letting callers distinguish transient I/O failures from
/// socket misconfiguration or server-side issues.
#[derive(Debug)]
pub enum ClientError {
    /// Could not connect to the unix socket (stale path, permissions, etc.).
    Connect(io::Error),
    /// Failed to configure socket read/write timeouts.
    Timeout(io::Error),
    /// Failed to write the HTTP request.
    Write(io::Error),
    /// Failed to read the HTTP response (status line, headers, or body).
    Read(io::Error),
    /// Server returned a non-200 status line.
    BadStatus(String),
    /// Response body was not valid JSON for the expected type.
    Parse(serde_json::Error),
}

impl std::fmt::Display for ClientError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Connect(e) => write!(f, "connect failed: {e}"),
            Self::Timeout(e) => write!(f, "timeout configure failed: {e}"),
            Self::Write(e) => write!(f, "write failed: {e}"),
            Self::Read(e) => write!(f, "read failed: {e}"),
            Self::BadStatus(s) => write!(f, "bad status: {s}"),
            Self::Parse(e) => write!(f, "parse failed: {e}"),
        }
    }
}

impl From<serde_json::Error> for ClientError {
    fn from(e: serde_json::Error) -> Self {
        Self::Parse(e)
    }
}

/// Metrics snapshot from `/api/metrics`.
#[derive(Debug, Clone, Default, Deserialize)]
pub struct Metrics {
    #[serde(default)]
    pub timestamp_secs: f64,
    #[serde(default)]
    pub tasks_spawned: u64,
    #[serde(default)]
    pub tasks_completed: u64,
    #[serde(default)]
    pub steals: u64,
    #[serde(default)]
    pub messages_sent: u64,
    #[serde(default)]
    pub messages_received: u64,
    #[serde(default)]
    pub active_workers: u64,
    #[serde(default)]
    pub alloc_count: u64,
    #[serde(default)]
    pub dealloc_count: u64,
    #[serde(default)]
    pub bytes_allocated: u64,
    #[serde(default)]
    pub bytes_freed: u64,
    #[serde(default)]
    pub bytes_live: u64,
    #[serde(default)]
    pub peak_bytes_live: u64,
}

/// Per-actor stats from `/api/actors`.
#[derive(Debug, Clone, Default, Deserialize)]
pub struct ActorInfo {
    pub id: u64,
    #[serde(default)]
    pub pid: u64,
    /// Hew actor type name, e.g. `"Counter"`.  Absent from older profiler
    /// versions; defaults to an empty string in that case.
    #[serde(default)]
    pub actor_type: String,
    #[serde(default)]
    pub state: String,
    #[serde(default)]
    pub msgs: u64,
    #[serde(default)]
    pub time_ns: u64,
    #[serde(default)]
    pub mbox_depth: i64,
    #[serde(default)]
    pub mbox_hwm: i64,
}

impl ActorInfo {
    /// Human-readable state name (capitalized for display).
    pub fn state_name(&self) -> &str {
        if self.state.is_empty() {
            "Unknown"
        } else {
            &self.state
        }
    }

    /// Display label for the actor type.
    ///
    /// Returns the registered Hew type name when available, or `"Actor"` as a
    /// fallback for older profiler versions that do not emit `actor_type`.
    pub fn type_label(&self) -> &str {
        if self.actor_type.is_empty() {
            "Actor"
        } else {
            &self.actor_type
        }
    }
}

/// History entry from `/api/metrics/history` (abbreviated keys).
#[derive(Debug, Clone, Default, Deserialize)]
#[expect(
    dead_code,
    reason = "Fields deserialized from JSON; only a subset used for sparklines currently"
)]
pub struct HistoryEntry {
    #[serde(default)]
    pub t: f64,
    #[serde(default, rename = "ts")]
    pub tasks_spawned: u64,
    #[serde(default, rename = "tc")]
    pub tasks_completed: u64,
    #[serde(default, rename = "st")]
    pub steals: u64,
    #[serde(default, rename = "ms")]
    pub messages_sent: u64,
    #[serde(default, rename = "mr")]
    pub messages_received: u64,
    #[serde(default, rename = "aw")]
    pub active_workers: u64,
    #[serde(default, rename = "ac")]
    pub alloc_count: u64,
    #[serde(default, rename = "dc")]
    pub dealloc_count: u64,
    #[serde(default, rename = "ba")]
    pub bytes_allocated: u64,
    #[serde(default, rename = "bf")]
    pub bytes_freed: u64,
    #[serde(default, rename = "bl")]
    pub bytes_live: u64,
    #[serde(default, rename = "pb")]
    pub peak_bytes_live: u64,
}

/// Cluster member from `/api/cluster/members`.
#[derive(Debug, Clone, Default, Deserialize)]
pub struct ClusterMember {
    #[serde(default)]
    pub node_id: u16,
    #[serde(default)]
    pub state: String,
    #[serde(default)]
    pub incarnation: u64,
    #[serde(default)]
    pub addr: String,
    #[serde(default)]
    pub last_seen_ms: u64,
}

/// Connection info from `/api/connections`.
#[derive(Debug, Clone, Default, Deserialize)]
pub struct ConnectionInfo {
    #[serde(default)]
    pub conn_id: i32,
    #[serde(default)]
    pub peer_node_id: u16,
    #[serde(default)]
    pub state: String,
    #[serde(default)]
    pub last_activity_ms: u64,
}

/// Routing table snapshot from `/api/routing/table`.
#[derive(Debug, Clone, Default, Deserialize)]
pub struct RoutingSnapshot {
    #[serde(default)]
    pub local_node_id: u16,
    #[serde(default)]
    pub routes: Vec<RouteEntry>,
}

#[derive(Debug, Clone, Default, Deserialize)]
pub struct RouteEntry {
    #[serde(default)]
    pub node_id: u16,
    #[serde(default)]
    pub conn_id: i32,
}

/// Trace event from `/api/traces`.
#[derive(Debug, Clone, Default, Deserialize)]
#[expect(
    dead_code,
    reason = "Fields deserialized from JSON; used for future display features"
)]
pub struct TraceEvent {
    #[serde(default)]
    pub trace_id: String,
    #[serde(default)]
    pub span_id: u64,
    #[serde(default)]
    pub parent_span_id: u64,
    #[serde(default)]
    pub actor_id: u64,
    /// Dispatch function pointer cast to `u64` — the de-facto actor type
    /// identifier.  `0` when the actor was freed before the drain ran, or when
    /// the profiler actor registry has not been populated.  Use together with
    /// `actor_type` for display; use the raw value to group or compare actor
    /// types programmatically.
    #[serde(default)]
    pub actor_type_id: u64,
    /// Registered Hew type name for this actor (e.g. `"Counter"`), or `None`
    /// when the dispatch function has not been registered via
    /// `hew_actor_register_type` (requires MLIR codegen emission — see #1258).
    #[serde(default)]
    pub actor_type: Option<String>,
    #[serde(default)]
    pub event_type: String,
    #[serde(default)]
    pub msg_type: i32,
    #[serde(default)]
    pub timestamp_ns: u64,
    /// Fully-qualified handler name (`"ActorType::handler_name"`), or `None`
    /// when the runtime's metadata registry has not been populated for this
    /// `(actor_type, msg_type)` pair.  Populated on native builds once
    /// `hew_register_handler_name` codegen emission lands (see #1259).
    #[serde(default)]
    pub handler_name: Option<String>,
}

impl TraceEvent {
    /// Returns `true` for event types that drive the actionable trace UI views
    /// (timeline, actor drill-down).  This is the single source of truth for
    /// the "actionable event" predicate used throughout hew-observe.
    pub fn is_actionable(&self) -> bool {
        matches!(
            self.event_type.as_str(),
            "send" | "spawn" | "crash" | "stop"
        )
    }
}

#[derive(Debug, Clone, Default, Deserialize)]
pub struct SupervisorRow {
    #[serde(default)]
    pub depth: u16,
    #[serde(default)]
    pub label: String,
    #[serde(default)]
    pub state: String,
}

#[derive(Debug, Clone, Default, Deserialize)]
pub struct CrashEntry {
    #[serde(default)]
    pub time_s: f64,
    #[serde(default)]
    pub actor_id: u64,
    #[serde(default)]
    pub signal: i32,
    #[serde(default)]
    pub msg_type: i32,
    #[serde(default)]
    pub fault_addr: u64,
}

/// Connection status.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ConnectionStatus {
    Connected,
    Disconnected,
    Connecting,
}

/// Transport backend for the profiler client.
#[derive(Debug)]
enum Transport {
    /// TCP via reqwest (for `--addr host:port` mode).
    Tcp {
        base_url: String,
        http: reqwest::blocking::Client,
    },
    /// Unix domain socket (for auto-discovered profilers).
    #[cfg(unix)]
    Unix { socket_path: PathBuf },
}

/// HTTP client for the profiler endpoint.
///
/// Connection status is determined exclusively by the metrics probe
/// (`fetch_metrics`). Other fetch methods do not affect connection
/// status — they may fail independently (e.g. a tab-specific endpoint
/// returning bad JSON) without poisoning the overall indicator.
///
/// When any `fetch_*` call returns `None`, inspect [`Self::last_error`]
/// to see the categorized failure reason.
#[derive(Debug)]
pub struct ProfilerClient {
    transport: Transport,
    pub status: ConnectionStatus,
    /// Last error from the most recent `fetch_*` call, if it failed.
    /// Cleared to `None` on a successful fetch.
    pub last_error: Option<ClientError>,
}

impl ProfilerClient {
    /// Create a client that connects over TCP.
    pub fn new_tcp(base_url: &str) -> Self {
        let http = reqwest::blocking::Client::builder()
            .timeout(Duration::from_millis(500))
            .build()
            .unwrap_or_default();
        Self {
            transport: Transport::Tcp {
                base_url: base_url.to_owned(),
                http,
            },
            status: ConnectionStatus::Connecting,
            last_error: None,
        }
    }

    /// Create a client that connects over a unix domain socket.
    #[cfg(unix)]
    pub fn new_unix(socket_path: &Path) -> Self {
        Self {
            transport: Transport::Unix {
                socket_path: socket_path.to_owned(),
            },
            status: ConnectionStatus::Connecting,
            last_error: None,
        }
    }

    /// Fetch current metrics and update connection status.
    ///
    /// This is the PRIMARY health-check endpoint. Connection status is
    /// set to `Connected` or `Disconnected` based solely on this call.
    pub fn fetch_metrics(&mut self) -> Option<Metrics> {
        if let Some(m) = self.get_json("/api/metrics") {
            self.status = ConnectionStatus::Connected;
            Some(m)
        } else {
            self.status = ConnectionStatus::Disconnected;
            None
        }
    }

    pub fn fetch_actors(&mut self) -> Option<Vec<ActorInfo>> {
        self.get_json("/api/actors")
    }

    pub fn fetch_history(&mut self) -> Option<Vec<HistoryEntry>> {
        self.get_json("/api/metrics/history")
    }

    pub fn fetch_cluster_members(&mut self) -> Option<Vec<ClusterMember>> {
        self.get_json("/api/cluster/members")
    }

    pub fn fetch_connections(&mut self) -> Option<Vec<ConnectionInfo>> {
        self.get_json("/api/connections")
    }

    pub fn fetch_routing(&mut self) -> Option<RoutingSnapshot> {
        self.get_json("/api/routing/table")
    }

    pub fn fetch_traces(&mut self) -> Option<Vec<TraceEvent>> {
        self.get_json("/api/traces")
    }

    pub fn fetch_supervisors(&mut self) -> Option<Vec<SupervisorRow>> {
        self.get_json("/api/supervisors")
    }

    pub fn fetch_crashes(&mut self) -> Option<Vec<CrashEntry>> {
        self.get_json("/api/crashes")
    }

    /// Fetch JSON from an endpoint. Does NOT affect connection status.
    fn get_json<T: serde::de::DeserializeOwned>(&mut self, path: &str) -> Option<T> {
        let body = self.get_bytes(path)?;
        match serde_json::from_slice(&body) {
            Ok(v) => Some(v),
            Err(e) => {
                self.last_error = Some(ClientError::Parse(e));
                None
            }
        }
    }

    /// Raw GET request returning the response body bytes.
    ///
    /// Uses a two-phase approach: [`Self::fetch_bytes_inner`] borrows
    /// `self` immutably to perform the I/O, then this method stores any
    /// error in `self.last_error` before returning `None`.
    fn get_bytes(&mut self, path: &str) -> Option<Vec<u8>> {
        match self.fetch_bytes_inner(path) {
            Ok(body) => {
                self.last_error = None;
                Some(body)
            }
            Err(e) => {
                self.last_error = Some(e);
                None
            }
        }
    }

    /// Inner fetch that borrows `self` immutably and returns a typed `Result`.
    /// Called exclusively by [`Self::get_bytes`].
    fn fetch_bytes_inner(&self, path: &str) -> Result<Vec<u8>, ClientError> {
        match &self.transport {
            Transport::Tcp { base_url, http } => {
                let url = format!("{base_url}{path}");
                let resp = http
                    .get(&url)
                    .send()
                    .map_err(|e| ClientError::Connect(io::Error::other(e)))?;
                if resp.status().is_success() {
                    resp.bytes()
                        .map_err(|e| ClientError::Read(io::Error::other(e)))
                        .map(|b| b.to_vec())
                } else {
                    Err(ClientError::BadStatus(resp.status().to_string()))
                }
            }
            #[cfg(unix)]
            Transport::Unix { socket_path } => unix_get(socket_path, path),
        }
    }
}

#[cfg(unix)]
/// Blocking HTTP/1.1 GET over a unix domain socket.
///
/// Opens a fresh connection per request (profiler responses are small
/// JSON payloads, so connection reuse isn't worth the complexity).
///
/// Returns a typed [`ClientError`] on failure so callers can distinguish
/// connect errors (stale/missing socket path) from I/O failures and
/// unexpected server responses.
fn unix_get(socket_path: &Path, path: &str) -> Result<Vec<u8>, ClientError> {
    let mut stream = UnixStream::connect(socket_path).map_err(ClientError::Connect)?;
    stream
        .set_read_timeout(Some(Duration::from_millis(500)))
        .map_err(ClientError::Timeout)?;
    stream
        .set_write_timeout(Some(Duration::from_millis(500)))
        .map_err(ClientError::Timeout)?;

    let request = format!("GET {path} HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n");
    stream
        .write_all(request.as_bytes())
        .map_err(ClientError::Write)?;

    // Read the HTTP response: status line, headers, blank line, body.
    let mut reader = std::io::BufReader::new(stream);

    // Status line: "HTTP/1.1 200 OK\r\n"
    let mut status_line = String::new();
    reader
        .read_line(&mut status_line)
        .map_err(ClientError::Read)?;
    if !status_line.contains(" 200 ") {
        return Err(ClientError::BadStatus(status_line.trim_end().to_owned()));
    }

    // Headers — look for Content-Length, skip until blank line.
    let mut content_length: Option<usize> = None;
    loop {
        let mut line = String::new();
        reader.read_line(&mut line).map_err(ClientError::Read)?;
        let trimmed = line.trim();
        if trimmed.is_empty() {
            break; // End of headers.
        }
        if let Some(val) = trimmed.strip_prefix("content-length:") {
            content_length = val.trim().parse().ok();
        } else if let Some(val) = trimmed.strip_prefix("Content-Length:") {
            content_length = val.trim().parse().ok();
        }
    }

    // Read body.
    if let Some(len) = content_length {
        let mut body = vec![0u8; len];
        reader.read_exact(&mut body).map_err(ClientError::Read)?;
        Ok(body)
    } else {
        // No Content-Length — read until EOF (Connection: close).
        let mut body = Vec::new();
        reader.read_to_end(&mut body).map_err(ClientError::Read)?;
        Ok(body)
    }
}

/// Multi-node client wrapping one `ProfilerClient` per node.
#[derive(Debug)]
pub struct ClusterClient {
    pub nodes: Vec<NodeClient>,
}

#[derive(Debug)]
#[expect(
    dead_code,
    reason = "Fields used for future multi-node display features"
)]
pub struct NodeClient {
    pub client: ProfilerClient,
    pub addr: String,
    pub node_id: Option<u16>,
}

impl ClusterClient {
    /// Create a cluster client from TCP addresses.
    pub fn new(addrs: &[String]) -> Self {
        let nodes = addrs
            .iter()
            .map(|addr| {
                let base_url = format!("http://{addr}");
                NodeClient {
                    client: ProfilerClient::new_tcp(&base_url),
                    addr: addr.clone(),
                    node_id: None,
                }
            })
            .collect();
        Self { nodes }
    }

    /// Create a cluster client from a single unix domain socket.
    #[cfg(unix)]
    pub fn from_unix(socket_path: &Path, label: &str) -> Self {
        let nodes = vec![NodeClient {
            client: ProfilerClient::new_unix(socket_path),
            addr: label.to_owned(),
            node_id: None,
        }];
        Self { nodes }
    }

    pub fn status(&self) -> ConnectionStatus {
        if self
            .nodes
            .iter()
            .any(|n| n.client.status == ConnectionStatus::Connected)
        {
            ConnectionStatus::Connected
        } else if self
            .nodes
            .iter()
            .any(|n| n.client.status == ConnectionStatus::Connecting)
        {
            ConnectionStatus::Connecting
        } else {
            ConnectionStatus::Disconnected
        }
    }
}

#[cfg(all(test, unix))]
mod tests {
    use super::*;
    use std::os::unix::net::UnixListener;

    /// Bind a listener on `path`, accept one connection, send `response`,
    /// then close.  Spawned on a background thread so the test's
    /// `ProfilerClient` can connect from the main thread.
    fn serve_once(path: &Path, response: &'static str) {
        let listener = UnixListener::bind(path).expect("bind");
        std::thread::spawn(move || {
            if let Ok((mut conn, _)) = listener.accept() {
                // Drain the request (ignore it).
                let mut buf = [0u8; 512];
                let _ = conn.read(&mut buf);
                let _ = conn.write_all(response.as_bytes());
                // conn drops here, closing the socket → EOF for reader.
            }
        });
    }

    // ── connect error ─────────────────────────────────────────────────────

    #[test]
    fn connect_error_surfaces_in_last_error() {
        let tmp = tempfile::tempdir().unwrap();
        let sock = tmp.path().join("nonexistent.sock");

        let mut client = ProfilerClient::new_unix(&sock);
        let result = client.fetch_metrics();

        assert!(result.is_none(), "expected None on missing socket");
        assert!(
            matches!(client.last_error, Some(ClientError::Connect(_))),
            "expected ClientError::Connect, got {:?}",
            client.last_error
        );
    }

    // ── bad status ────────────────────────────────────────────────────────

    #[test]
    fn bad_status_surfaces_in_last_error() {
        let tmp = tempfile::tempdir().unwrap();
        let sock = tmp.path().join("bad_status.sock");

        serve_once(&sock, "HTTP/1.1 404 Not Found\r\nContent-Length: 0\r\n\r\n");
        // Give the thread a moment to bind.
        std::thread::sleep(Duration::from_millis(50));

        let mut client = ProfilerClient::new_unix(&sock);
        let result = client.fetch_metrics();

        assert!(result.is_none(), "expected None on 404");
        assert!(
            matches!(client.last_error, Some(ClientError::BadStatus(ref s)) if s.contains("404")),
            "expected ClientError::BadStatus with '404', got {:?}",
            client.last_error
        );
    }

    // ── parse error ───────────────────────────────────────────────────────

    #[test]
    fn parse_error_surfaces_in_last_error() {
        let tmp = tempfile::tempdir().unwrap();
        let sock = tmp.path().join("bad_json.sock");

        serve_once(
            &sock,
            "HTTP/1.1 200 OK\r\nContent-Length: 8\r\n\r\nnot json",
        );
        std::thread::sleep(Duration::from_millis(50));

        let mut client = ProfilerClient::new_unix(&sock);
        let result = client.fetch_metrics();

        assert!(result.is_none(), "expected None on bad JSON");
        assert!(
            matches!(client.last_error, Some(ClientError::Parse(_))),
            "expected ClientError::Parse, got {:?}",
            client.last_error
        );
    }

    // ── success clears last_error ─────────────────────────────────────────

    #[test]
    fn success_clears_last_error() {
        let tmp = tempfile::tempdir().unwrap();
        let sock_fail = tmp.path().join("fail.sock");
        let sock_ok = tmp.path().join("ok.sock");

        // First call: missing socket → sets last_error.
        let mut client = ProfilerClient::new_unix(&sock_fail);
        client.fetch_metrics();
        assert!(client.last_error.is_some());

        // Swap to a socket that serves valid metrics JSON.
        let metrics_json = r#"{"timestamp_secs":1.0,"tasks_spawned":0,"tasks_completed":0,"steals":0,"messages_sent":0,"messages_received":0,"active_workers":0,"alloc_count":0,"dealloc_count":0,"bytes_allocated":0,"bytes_freed":0,"bytes_live":0,"peak_bytes_live":0}"#;
        let body_len = metrics_json.len();
        let response =
            format!("HTTP/1.1 200 OK\r\nContent-Length: {body_len}\r\n\r\n{metrics_json}");
        let response: &'static str = Box::leak(response.into_boxed_str());
        serve_once(&sock_ok, response);
        std::thread::sleep(Duration::from_millis(50));

        client.transport = Transport::Unix {
            socket_path: sock_ok.clone(),
        };
        let result = client.fetch_metrics();

        assert!(result.is_some(), "expected Some on valid response");
        assert!(
            client.last_error.is_none(),
            "expected last_error cleared after success, got {:?}",
            client.last_error
        );
    }

    // ── ClientError::Display ──────────────────────────────────────────────

    #[test]
    fn client_error_display_includes_category() {
        let e = ClientError::Connect(io::Error::new(io::ErrorKind::NotFound, "no such file"));
        assert!(e.to_string().starts_with("connect failed:"), "{e}");

        let e = ClientError::BadStatus("HTTP/1.1 503 Service Unavailable".to_owned());
        assert!(e.to_string().starts_with("bad status:"), "{e}");

        let e: ClientError = serde_json::from_str::<serde_json::Value>("bad")
            .unwrap_err()
            .into();
        assert!(e.to_string().starts_with("parse failed:"), "{e}");
    }
}
