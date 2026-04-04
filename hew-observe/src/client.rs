//! HTTP client for fetching runtime profiler data.

use std::io::{BufRead, Read, Write};
#[cfg(unix)]
use std::os::unix::net::UnixStream;
use std::path::{Path, PathBuf};
use std::time::Duration;

use serde::Deserialize;

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
    #[serde(default)]
    pub event_type: String,
    #[serde(default)]
    pub msg_type: i32,
    #[serde(default)]
    pub timestamp_ns: u64,
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
#[derive(Debug)]
pub struct ProfilerClient {
    transport: Transport,
    pub status: ConnectionStatus,
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
    fn get_json<T: serde::de::DeserializeOwned>(&self, path: &str) -> Option<T> {
        let body = self.get_bytes(path)?;
        serde_json::from_slice(&body).ok()
    }

    /// Raw GET request returning the response body bytes.
    fn get_bytes(&self, path: &str) -> Option<Vec<u8>> {
        match &self.transport {
            Transport::Tcp { base_url, http } => {
                let url = format!("{base_url}{path}");
                let resp = http.get(&url).send().ok()?;
                if resp.status().is_success() {
                    resp.bytes().ok().map(|b| b.to_vec())
                } else {
                    None
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
fn unix_get(socket_path: &Path, path: &str) -> Option<Vec<u8>> {
    let mut stream = UnixStream::connect(socket_path).ok()?;
    stream
        .set_read_timeout(Some(Duration::from_millis(500)))
        .ok()?;
    stream
        .set_write_timeout(Some(Duration::from_millis(500)))
        .ok()?;

    let request = format!("GET {path} HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n");
    stream.write_all(request.as_bytes()).ok()?;

    // Read the HTTP response: status line, headers, blank line, body.
    let mut reader = std::io::BufReader::new(stream);

    // Status line: "HTTP/1.1 200 OK\r\n"
    let mut status_line = String::new();
    reader.read_line(&mut status_line).ok()?;
    if !status_line.contains(" 200 ") {
        return None;
    }

    // Headers — look for Content-Length, skip until blank line.
    let mut content_length: Option<usize> = None;
    loop {
        let mut line = String::new();
        reader.read_line(&mut line).ok()?;
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
        reader.read_exact(&mut body).ok()?;
        Some(body)
    } else {
        // No Content-Length — read until EOF (Connection: close).
        let mut body = Vec::new();
        reader.read_to_end(&mut body).ok()?;
        Some(body)
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
