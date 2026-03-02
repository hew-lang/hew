//! HTTP client for fetching runtime profiler data.

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
#[expect(
    dead_code,
    reason = "Fields deserialized from JSON; used for future display features"
)]
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
#[expect(
    dead_code,
    reason = "Fields deserialized from JSON; used for future display features"
)]
pub struct RoutingSnapshot {
    #[serde(default)]
    pub local_node_id: u16,
    #[serde(default)]
    pub routes: Vec<RouteEntry>,
}

#[derive(Debug, Clone, Default, Deserialize)]
#[expect(
    dead_code,
    reason = "Fields deserialized from JSON; used for future display features"
)]
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

/// Connection status.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ConnectionStatus {
    Connected,
    Disconnected,
    Connecting,
}

/// HTTP client for the profiler endpoint.
#[derive(Debug)]
pub struct ProfilerClient {
    base_url: String,
    http: reqwest::blocking::Client,
    pub status: ConnectionStatus,
}

impl ProfilerClient {
    pub fn new(base_url: &str) -> Self {
        let http = reqwest::blocking::Client::builder()
            .timeout(Duration::from_millis(500))
            .build()
            .unwrap_or_default();
        Self {
            base_url: base_url.to_owned(),
            http,
            status: ConnectionStatus::Connecting,
        }
    }

    pub fn fetch_metrics(&mut self) -> Option<Metrics> {
        match self
            .http
            .get(format!("{}/api/metrics", self.base_url))
            .send()
        {
            Ok(resp) if resp.status().is_success() => {
                self.status = ConnectionStatus::Connected;
                resp.json().ok()
            }
            _ => {
                self.status = ConnectionStatus::Disconnected;
                None
            }
        }
    }

    pub fn fetch_actors(&mut self) -> Option<Vec<ActorInfo>> {
        match self
            .http
            .get(format!("{}/api/actors", self.base_url))
            .send()
        {
            Ok(resp) if resp.status().is_success() => {
                self.status = ConnectionStatus::Connected;
                resp.json().ok()
            }
            _ => {
                self.status = ConnectionStatus::Disconnected;
                None
            }
        }
    }

    pub fn fetch_history(&mut self) -> Option<Vec<HistoryEntry>> {
        match self
            .http
            .get(format!("{}/api/metrics/history", self.base_url))
            .send()
        {
            Ok(resp) if resp.status().is_success() => {
                self.status = ConnectionStatus::Connected;
                resp.json().ok()
            }
            _ => {
                self.status = ConnectionStatus::Disconnected;
                None
            }
        }
    }

    pub fn fetch_cluster_members(&mut self) -> Option<Vec<ClusterMember>> {
        match self
            .http
            .get(format!("{}/api/cluster/members", self.base_url))
            .send()
        {
            Ok(resp) if resp.status().is_success() => {
                self.status = ConnectionStatus::Connected;
                resp.json().ok()
            }
            _ => {
                self.status = ConnectionStatus::Disconnected;
                None
            }
        }
    }

    pub fn fetch_connections(&mut self) -> Option<Vec<ConnectionInfo>> {
        match self
            .http
            .get(format!("{}/api/connections", self.base_url))
            .send()
        {
            Ok(resp) if resp.status().is_success() => {
                self.status = ConnectionStatus::Connected;
                resp.json().ok()
            }
            _ => {
                self.status = ConnectionStatus::Disconnected;
                None
            }
        }
    }

    pub fn fetch_routing(&mut self) -> Option<RoutingSnapshot> {
        match self
            .http
            .get(format!("{}/api/routing/table", self.base_url))
            .send()
        {
            Ok(resp) if resp.status().is_success() => {
                self.status = ConnectionStatus::Connected;
                resp.json().ok()
            }
            _ => {
                self.status = ConnectionStatus::Disconnected;
                None
            }
        }
    }

    pub fn fetch_traces(&mut self) -> Option<Vec<TraceEvent>> {
        match self
            .http
            .get(format!("{}/api/traces", self.base_url))
            .send()
        {
            Ok(resp) if resp.status().is_success() => {
                self.status = ConnectionStatus::Connected;
                resp.json().ok()
            }
            _ => {
                self.status = ConnectionStatus::Disconnected;
                None
            }
        }
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
    pub fn new(addrs: &[String]) -> Self {
        let nodes = addrs
            .iter()
            .map(|addr| {
                let base_url = format!("http://{addr}");
                NodeClient {
                    client: ProfilerClient::new(&base_url),
                    addr: addr.clone(),
                    node_id: None,
                }
            })
            .collect();
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
