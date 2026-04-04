//! Application state for the TUI observer.

use std::path::Path;
use std::time::{Duration, Instant};

use crate::client::{
    ActorInfo, ClusterClient, ClusterMember, ConnectionInfo, ConnectionStatus,
    CrashEntry as ClientCrashEntry, HistoryEntry, Metrics, RouteEntry, RoutingSnapshot,
    SupervisorRow as ClientSupervisorRow, TraceEvent,
};
#[cfg(unix)]
use crate::discovery;

/// Active tab.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Tab {
    Overview,
    Actors,
    Supervisors,
    Crashes,
    Cluster,
    Messages,
    Timeline,
}

const TABS: [Tab; 7] = [
    Tab::Overview,
    Tab::Actors,
    Tab::Supervisors,
    Tab::Crashes,
    Tab::Cluster,
    Tab::Messages,
    Tab::Timeline,
];

const ZOOM_LEVELS_NS: [u64; 6] = [
    1_000_000_000,
    2_000_000_000,
    5_000_000_000,
    10_000_000_000,
    30_000_000_000,
    60_000_000_000,
];

/// Sort column for actor list.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SortColumn {
    Id,
    State,
    Messages,
    MailboxDepth,
    ProcessingTime,
}

const SORT_COLUMNS: [SortColumn; 5] = [
    SortColumn::Id,
    SortColumn::State,
    SortColumn::Messages,
    SortColumn::MailboxDepth,
    SortColumn::ProcessingTime,
];

#[derive(Debug, Clone)]
pub struct CrashEntry {
    pub time_s: f64,
    pub actor_id: u64,
    pub signal: i32,
    pub msg_type: i32,
    pub fault_addr: u64,
}

/// Demo supervision tree node.
#[derive(Debug, Clone)]
pub struct SupervisorNode {
    pub name: String,
    pub strategy: String,
    pub children: Vec<TreeChild>,
}

#[derive(Debug, Clone)]
pub struct TreeChild {
    pub name: String,
    pub state: &'static str,
    pub restarts: u32,
}

/// Flattened tree row for display.
#[derive(Debug, Clone)]
pub struct TreeRow {
    pub depth: u16,
    pub label: String,
    pub state: String,
}

impl From<ClientCrashEntry> for CrashEntry {
    fn from(entry: ClientCrashEntry) -> Self {
        Self {
            time_s: entry.time_s,
            actor_id: entry.actor_id,
            signal: entry.signal,
            msg_type: entry.msg_type,
            fault_addr: entry.fault_addr,
        }
    }
}

impl From<ClientSupervisorRow> for TreeRow {
    fn from(row: ClientSupervisorRow) -> Self {
        Self {
            depth: row.depth,
            label: row.label,
            state: row.state,
        }
    }
}

/// Main application state.
#[derive(Debug)]
#[expect(
    clippy::struct_excessive_bools,
    reason = "TUI app state naturally has many boolean flags"
)]
pub struct App {
    pub active_tab: Tab,
    pub show_help: bool,
    pub filter_active: bool,
    pub filter_text: String,

    // Data
    pub metrics: Metrics,
    pub actors: Vec<ActorInfo>,
    pub history: Vec<HistoryEntry>,
    pub crashes: Vec<CrashEntry>,
    pub tree_rows: Vec<TreeRow>,

    // Derived
    pub msg_rate: f64,
    pub sparkline_msgs: Vec<u64>,
    pub sparkline_actors: Vec<u64>,
    pub sparkline_memory: Vec<u64>,

    // Selection state
    pub actor_selected: usize,
    pub sort_column: SortColumn,
    pub crash_selected: usize,
    pub tree_selected: usize,

    // Cluster data
    pub cluster_members: Vec<ClusterMember>,
    pub cluster_connections: Vec<ConnectionInfo>,
    pub cluster_routing: RoutingSnapshot,

    // Message flow data
    pub trace_events: Vec<TraceEvent>,
    pub trace_paused: bool,
    pub trace_scroll: usize,
    pub trace_filter_actor: Option<u64>,

    // Timeline data
    pub timeline_offset_ns: i64,
    pub timeline_window_ns: u64,
    pub timeline_paused: bool,
    pub timeline_zoom_level: usize,

    // Connection
    cluster: Option<ClusterClient>,
    pub connection_status: ConnectionStatus,
    pub demo_mode: bool,
    pub base_url: String,

    /// When true, periodically re-scan the discovery directory and
    /// reconnect if the current profiler is gone or a new one appears.
    auto_discover: bool,
    last_discovery_scan: Instant,

    prev_messages_sent: u64,
    prev_timestamp: f64,
}

impl App {
    /// Connect to profilers over TCP.
    pub fn new_tcp(node_addrs: &[String]) -> Self {
        let base_url = node_addrs
            .first()
            .map_or_else(|| "localhost:6060".to_owned(), Clone::clone);
        Self::build(Some(ClusterClient::new(node_addrs)), base_url, false, false)
    }

    /// Connect to a profiler over a unix domain socket.
    #[cfg(unix)]
    pub fn new_unix(socket_path: &Path, label: &str) -> Self {
        let cluster = ClusterClient::from_unix(socket_path, label);
        Self::build(Some(cluster), label.to_owned(), false, false)
    }

    /// Connect via auto-discovery (re-scans periodically).
    #[cfg(unix)]
    pub fn new_discovered(socket_path: &Path, label: &str) -> Self {
        let cluster = ClusterClient::from_unix(socket_path, label);
        Self::build(Some(cluster), label.to_owned(), false, true)
    }

    /// Auto-discover mode with no initial connection.
    #[cfg(unix)]
    pub fn new_waiting() -> Self {
        Self::build(None, String::new(), false, true)
    }

    /// Demo mode with synthetic data.
    pub fn new_demo() -> Self {
        let mut app = Self::build(None, "demo".to_owned(), true, false);
        app.load_demo_data();
        app
    }

    fn build(
        cluster: Option<ClusterClient>,
        base_url: String,
        demo: bool,
        auto_discover: bool,
    ) -> Self {
        Self {
            active_tab: Tab::Overview,
            show_help: false,
            filter_active: false,
            filter_text: String::new(),
            metrics: Metrics::default(),
            actors: Vec::new(),
            history: Vec::new(),
            crashes: Vec::new(),
            tree_rows: Vec::new(),
            msg_rate: 0.0,
            sparkline_msgs: Vec::new(),
            sparkline_actors: Vec::new(),
            sparkline_memory: Vec::new(),
            actor_selected: 0,
            sort_column: SortColumn::Id,
            crash_selected: 0,
            tree_selected: 0,
            cluster_members: Vec::new(),
            cluster_connections: Vec::new(),
            cluster_routing: RoutingSnapshot::default(),
            trace_events: Vec::new(),
            trace_paused: false,
            trace_scroll: 0,
            trace_filter_actor: None,
            timeline_offset_ns: 0,
            timeline_window_ns: ZOOM_LEVELS_NS[2], // default 5s
            timeline_paused: false,
            timeline_zoom_level: 2,
            cluster,
            connection_status: if demo {
                ConnectionStatus::Connected
            } else {
                ConnectionStatus::Connecting
            },
            demo_mode: demo,
            base_url,
            auto_discover,
            last_discovery_scan: Instant::now(),
            prev_messages_sent: 0,
            prev_timestamp: 0.0,
        }
    }

    /// Clamp all selection indices to valid ranges so they never exceed
    /// the current list lengths (e.g. after a refresh shrinks a list or
    /// a filter narrows the visible set).
    pub fn clamp_selections(&mut self) {
        let actor_len = self.filtered_actors().len();
        if actor_len == 0 {
            self.actor_selected = 0;
        } else {
            self.actor_selected = self.actor_selected.min(actor_len - 1);
        }

        if self.crashes.is_empty() {
            self.crash_selected = 0;
        } else {
            self.crash_selected = self.crash_selected.min(self.crashes.len() - 1);
        }

        if self.tree_rows.is_empty() {
            self.tree_selected = 0;
        } else {
            self.tree_selected = self.tree_selected.min(self.tree_rows.len() - 1);
        }

        if self.trace_events.is_empty() {
            self.trace_scroll = 0;
        } else {
            self.trace_scroll = self.trace_scroll.min(self.trace_events.len() - 1);
        }
    }

    pub fn next_tab(&mut self) {
        let idx = TABS.iter().position(|t| *t == self.active_tab).unwrap_or(0);
        self.active_tab = TABS[(idx + 1) % TABS.len()];
    }

    pub fn prev_tab(&mut self) {
        let idx = TABS.iter().position(|t| *t == self.active_tab).unwrap_or(0);
        self.active_tab = TABS[(idx + TABS.len() - 1) % TABS.len()];
    }

    pub fn cycle_sort(&mut self) {
        let idx = SORT_COLUMNS
            .iter()
            .position(|s| *s == self.sort_column)
            .unwrap_or(0);
        self.sort_column = SORT_COLUMNS[(idx + 1) % SORT_COLUMNS.len()];
        self.sort_actors();
    }

    pub fn actor_list_next(&mut self) {
        let len = self.filtered_actors().len();
        if len > 0 {
            self.actor_selected = (self.actor_selected + 1).min(len - 1);
        }
    }

    pub fn actor_list_prev(&mut self) {
        self.actor_selected = self.actor_selected.saturating_sub(1);
    }

    pub fn crash_list_next(&mut self) {
        if !self.crashes.is_empty() {
            self.crash_selected = (self.crash_selected + 1).min(self.crashes.len() - 1);
        }
    }

    pub fn crash_list_prev(&mut self) {
        self.crash_selected = self.crash_selected.saturating_sub(1);
    }

    pub fn tree_next(&mut self) {
        if !self.tree_rows.is_empty() {
            self.tree_selected = (self.tree_selected + 1).min(self.tree_rows.len() - 1);
        }
    }

    pub fn tree_prev(&mut self) {
        self.tree_selected = self.tree_selected.saturating_sub(1);
    }

    // -- Messages tab navigation --

    pub fn messages_scroll_up(&mut self) {
        self.trace_scroll = self.trace_scroll.saturating_sub(1);
    }

    pub fn messages_scroll_down(&mut self) {
        if !self.trace_events.is_empty() {
            self.trace_scroll = (self.trace_scroll + 1).min(self.trace_events.len() - 1);
        }
    }

    pub fn messages_toggle_pause(&mut self) {
        self.trace_paused = !self.trace_paused;
    }

    pub fn messages_set_filter(&mut self, actor_id: u64) {
        self.trace_filter_actor = Some(actor_id);
    }

    /// Filter messages by the `actor_id` of the currently scrolled-to event.
    pub fn messages_filter_selected(&mut self) {
        let events: Vec<&TraceEvent> = self
            .trace_events
            .iter()
            .filter(|e| {
                if let Some(filter_id) = self.trace_filter_actor {
                    e.actor_id == filter_id
                } else {
                    true
                }
            })
            .filter(|e| e.is_actionable())
            .collect();
        if let Some(evt) = events.get(self.trace_scroll) {
            self.messages_set_filter(evt.actor_id);
        }
    }

    pub fn messages_clear_filter(&mut self) {
        self.trace_filter_actor = None;
    }

    // -- Timeline tab navigation --

    #[expect(
        clippy::cast_possible_wrap,
        reason = "timeline window is always small enough to fit in i64"
    )]
    pub fn timeline_scroll_left(&mut self) {
        self.timeline_offset_ns -= self.timeline_window_ns as i64 / 4;
    }

    #[expect(
        clippy::cast_possible_wrap,
        reason = "timeline window is always small enough to fit in i64"
    )]
    pub fn timeline_scroll_right(&mut self) {
        self.timeline_offset_ns += self.timeline_window_ns as i64 / 4;
    }

    pub fn timeline_zoom_in(&mut self) {
        if self.timeline_zoom_level > 0 {
            self.timeline_zoom_level -= 1;
            self.timeline_window_ns = ZOOM_LEVELS_NS[self.timeline_zoom_level];
        }
    }

    pub fn timeline_zoom_out(&mut self) {
        if self.timeline_zoom_level < ZOOM_LEVELS_NS.len() - 1 {
            self.timeline_zoom_level += 1;
            self.timeline_window_ns = ZOOM_LEVELS_NS[self.timeline_zoom_level];
        }
    }

    pub fn timeline_toggle_pause(&mut self) {
        self.timeline_paused = !self.timeline_paused;
    }

    pub fn timeline_snap_to_now(&mut self) {
        self.timeline_offset_ns = 0;
    }

    pub fn filtered_actors(&self) -> Vec<&ActorInfo> {
        if self.filter_text.is_empty() {
            self.actors.iter().collect()
        } else {
            self.actors
                .iter()
                .filter(|a| {
                    let id_str = a.id.to_string();
                    let state = a.state_name().to_lowercase();
                    let filter = self.filter_text.to_lowercase();
                    id_str.contains(&filter) || state.contains(&filter)
                })
                .collect()
        }
    }

    #[allow(
        clippy::too_many_lines,
        reason = "TUI refresh reads multiple sources in a single pass"
    )]
    pub fn refresh(&mut self) {
        if self.demo_mode {
            return;
        }

        // In auto-discover mode, periodically re-scan for profilers.
        #[cfg(unix)]
        if self.auto_discover {
            self.try_rediscover();
        }

        let Some(cluster) = &mut self.cluster else {
            return;
        };

        // Use first node for single-node data (metrics, actors, history)
        let Some(first) = cluster.nodes.first_mut() else {
            return;
        };

        // Only fetch data needed for the current tab to avoid unnecessary blocking
        let metrics = first.client.fetch_metrics();
        let status = first.client.status;
        if status == ConnectionStatus::Disconnected {
            self.connection_status = status;
            return;
        }

        let actors = if self.active_tab == Tab::Actors || self.active_tab == Tab::Overview {
            first.client.fetch_actors()
        } else {
            None
        };

        let history = if self.active_tab == Tab::Overview {
            first.client.fetch_history()
        } else {
            None
        };

        let supervisors = if self.active_tab == Tab::Supervisors {
            first.client.fetch_supervisors()
        } else {
            None
        };

        let crashes = if self.active_tab == Tab::Crashes {
            first.client.fetch_crashes()
        } else {
            None
        };

        // Cluster tab: fetch cluster data from first connected node
        let (cluster_members, connections, routing) =
            if self.active_tab == Tab::Cluster || self.active_tab == Tab::Timeline {
                (
                    first.client.fetch_cluster_members(),
                    first.client.fetch_connections(),
                    first.client.fetch_routing(),
                )
            } else {
                (None, None, None)
            };

        // Messages/Timeline tab: fetch traces from all nodes
        let traces = if (self.active_tab == Tab::Messages && !self.trace_paused)
            || self.active_tab == Tab::Timeline
        {
            let cluster_ref = self.cluster.as_mut().unwrap();
            let mut all_traces = Vec::new();
            for node in &mut cluster_ref.nodes {
                if let Some(mut t) = node.client.fetch_traces() {
                    all_traces.append(&mut t);
                }
            }
            if all_traces.is_empty() {
                None
            } else {
                Some(all_traces)
            }
        } else {
            None
        };

        let cluster_status = self.cluster.as_ref().unwrap().status();

        if let Some(m) = metrics {
            if self.prev_timestamp > 0.0 && m.timestamp_secs > self.prev_timestamp {
                let dt = m.timestamp_secs - self.prev_timestamp;
                let dm = m.messages_sent.saturating_sub(self.prev_messages_sent);
                #[expect(
                    clippy::cast_precision_loss,
                    reason = "message count rate doesn't need full u64 precision"
                )]
                let rate = dm as f64 / dt;
                self.msg_rate = rate;
            }
            self.prev_timestamp = m.timestamp_secs;
            self.prev_messages_sent = m.messages_sent;
            self.metrics = m;
        }

        if let Some(a) = actors {
            self.actors = a;
            self.sort_actors();
        }

        if let Some(h) = history {
            self.update_sparklines(&h);
            self.history = h;
        }

        if let Some(rows) = supervisors {
            self.tree_rows = rows.into_iter().map(Into::into).collect();
        }

        if let Some(entries) = crashes {
            self.crashes = entries.into_iter().map(Into::into).collect();
        }

        if let Some(members) = cluster_members {
            self.cluster_members = members;
        }
        if let Some(conns) = connections {
            self.cluster_connections = conns;
        }
        if let Some(r) = routing {
            self.cluster_routing = r;
        }

        if let Some(t) = traces {
            // Filter out begin/end span events at ingestion — they carry
            // dispatch timing data but are not actionable in the swimlane
            // display, and including them inflates the event counter.
            self.trace_events.extend(
                t.into_iter()
                    .filter(|e| e.event_type != "begin" && e.event_type != "end"),
            );
            // Cap at 1000 entries
            if self.trace_events.len() > 1000 {
                let drain = self.trace_events.len() - 1000;
                self.trace_events.drain(..drain);
            }
        }

        self.connection_status = cluster_status;
    }

    #[cfg(unix)]
    /// Re-scan the discovery directory and reconnect if needed.
    ///
    /// Scans every 3 seconds. When disconnected (or no cluster), picks
    /// up a newly started profiler. When connected, does nothing — we
    /// don't disrupt a live session.
    fn try_rediscover(&mut self) {
        const SCAN_INTERVAL: Duration = Duration::from_secs(3);

        if self.last_discovery_scan.elapsed() < SCAN_INTERVAL {
            return;
        }
        self.last_discovery_scan = Instant::now();

        // Only reconnect when we have no connection.
        let needs_connect =
            self.cluster.is_none() || self.connection_status == ConnectionStatus::Disconnected;
        if !needs_connect {
            return;
        }

        let profilers = discovery::scan_profilers();
        if let Some(p) = profilers.first() {
            self.cluster = Some(ClusterClient::from_unix(&p.socket_path, &p.program));
            self.base_url.clone_from(&p.program);
            self.connection_status = ConnectionStatus::Connecting;
        }
    }

    fn sort_actors(&mut self) {
        match self.sort_column {
            SortColumn::Id => self.actors.sort_by_key(|a| a.id),
            SortColumn::State => self.actors.sort_by(|a, b| a.state.cmp(&b.state)),
            SortColumn::Messages => self.actors.sort_by(|a, b| b.msgs.cmp(&a.msgs)),
            SortColumn::MailboxDepth => {
                self.actors.sort_by(|a, b| b.mbox_depth.cmp(&a.mbox_depth));
            }
            SortColumn::ProcessingTime => {
                self.actors.sort_by(|a, b| b.time_ns.cmp(&a.time_ns));
            }
        }
    }

    fn update_sparklines(&mut self, history: &[HistoryEntry]) {
        // Message rate sparkline: compute deltas
        self.sparkline_msgs = history
            .windows(2)
            .map(|w| w[1].messages_sent.saturating_sub(w[0].messages_sent))
            .collect();

        // Active workers sparkline
        self.sparkline_actors = history.iter().map(|h| h.active_workers).collect();

        // Memory sparkline (bytes_live over time)
        self.sparkline_memory = history.iter().map(|h| h.bytes_live).collect();
    }

    #[expect(
        clippy::too_many_lines,
        reason = "demo data initialization is inherently verbose"
    )]
    fn load_demo_data(&mut self) {
        self.metrics = Metrics {
            timestamp_secs: 42.5,
            tasks_spawned: 128,
            tasks_completed: 115,
            steals: 47,
            messages_sent: 9823,
            messages_received: 9801,
            active_workers: 4,
            alloc_count: 2048,
            dealloc_count: 1920,
            bytes_allocated: 524_288,
            bytes_freed: 491_520,
            bytes_live: 32_768,
            peak_bytes_live: 65_536,
        };
        self.msg_rate = 231.4;

        self.actors = vec![
            ActorInfo {
                id: 1,
                pid: 100,
                state: "running".to_owned(),
                msgs: 4201,
                time_ns: 1_200_000_000,
                mbox_depth: 3,
                mbox_hwm: 12,
            },
            ActorInfo {
                id: 2,
                pid: 101,
                state: "idle".to_owned(),
                msgs: 1580,
                time_ns: 800_000_000,
                mbox_depth: 0,
                mbox_hwm: 8,
            },
            ActorInfo {
                id: 3,
                pid: 102,
                state: "blocked".to_owned(),
                msgs: 920,
                time_ns: 450_000_000,
                mbox_depth: 15,
                mbox_hwm: 15,
            },
            ActorInfo {
                id: 4,
                pid: 103,
                state: "crashed".to_owned(),
                msgs: 312,
                time_ns: 100_000_000,
                mbox_depth: 0,
                mbox_hwm: 4,
            },
            ActorInfo {
                id: 5,
                pid: 104,
                state: "runnable".to_owned(),
                msgs: 2105,
                time_ns: 600_000_000,
                mbox_depth: 7,
                mbox_hwm: 20,
            },
            ActorInfo {
                id: 6,
                pid: 105,
                state: "idle".to_owned(),
                msgs: 88,
                time_ns: 30_000_000,
                mbox_depth: 0,
                mbox_hwm: 2,
            },
        ];

        self.crashes = vec![
            CrashEntry {
                time_s: 41.2,
                actor_id: 4,
                signal: 11,
                msg_type: 3,
                fault_addr: 0x0000_0000_DEAD_BEEF,
            },
            CrashEntry {
                time_s: 38.7,
                actor_id: 4,
                signal: 11,
                msg_type: 1,
                fault_addr: 0x0000_0000_0000_0000,
            },
            CrashEntry {
                time_s: 22.1,
                actor_id: 7,
                signal: 6,
                msg_type: 2,
                fault_addr: 0x0000_0000_CAFE_BABE,
            },
        ];

        // Build demo supervision tree
        let supervisors = vec![
            SupervisorNode {
                name: "root_sup".into(),
                strategy: "one_for_one".into(),
                children: vec![
                    TreeChild {
                        name: "worker_pool_sup".into(),
                        state: "Running",
                        restarts: 0,
                    },
                    TreeChild {
                        name: "logger".into(),
                        state: "Idle",
                        restarts: 0,
                    },
                ],
            },
            SupervisorNode {
                name: "worker_pool_sup".into(),
                strategy: "one_for_all".into(),
                children: vec![
                    TreeChild {
                        name: "worker_1".into(),
                        state: "Running",
                        restarts: 1,
                    },
                    TreeChild {
                        name: "worker_2".into(),
                        state: "Running",
                        restarts: 0,
                    },
                    TreeChild {
                        name: "worker_3".into(),
                        state: "Crashed",
                        restarts: 3,
                    },
                ],
            },
        ];
        self.tree_rows = flatten_tree(&supervisors);

        // Demo sparklines
        self.sparkline_msgs = vec![12, 45, 30, 67, 23, 89, 54, 32, 78, 41, 55, 90, 44, 61, 37];
        self.sparkline_actors = vec![4, 4, 4, 3, 4, 4, 4, 4, 3, 4, 4, 4, 4, 4, 4];
        self.sparkline_memory = vec![
            28_672, 30_720, 31_744, 32_768, 30_720, 32_768, 34_816, 32_768, 31_744, 32_768, 34_816,
            36_864, 34_816, 32_768, 32_768,
        ];

        // Demo cluster members (3 nodes)
        self.cluster_members = vec![
            ClusterMember {
                node_id: 1,
                state: "alive".into(),
                incarnation: 2,
                addr: "127.0.0.1:9000".into(),
                last_seen_ms: 0,
            },
            ClusterMember {
                node_id: 2,
                state: "alive".into(),
                incarnation: 1,
                addr: "192.168.1.11:9000".into(),
                last_seen_ms: 500,
            },
            ClusterMember {
                node_id: 3,
                state: "suspect".into(),
                incarnation: 3,
                addr: "192.168.1.12:9000".into(),
                last_seen_ms: 8000,
            },
        ];

        // Demo connections
        self.cluster_connections = vec![
            ConnectionInfo {
                conn_id: 0,
                peer_node_id: 2,
                state: "active".into(),
                last_activity_ms: 500,
            },
            ConnectionInfo {
                conn_id: 1,
                peer_node_id: 3,
                state: "active".into(),
                last_activity_ms: 8000,
            },
        ];

        // Demo routing
        self.cluster_routing = RoutingSnapshot {
            local_node_id: 1,
            routes: vec![
                RouteEntry {
                    node_id: 2,
                    conn_id: 0,
                },
                RouteEntry {
                    node_id: 3,
                    conn_id: 1,
                },
            ],
        };

        // Demo trace events (50 events over 10 seconds)
        let base_ns: u64 = 40_000_000_000;
        let mut traces = Vec::new();
        // Spawns
        traces.push(TraceEvent {
            trace_id: "0001".into(),
            span_id: 1,
            parent_span_id: 0,
            actor_id: (1u64 << 48) | 1,
            event_type: "spawn".into(),
            msg_type: 0,
            timestamp_ns: base_ns,
        });
        traces.push(TraceEvent {
            trace_id: "0002".into(),
            span_id: 2,
            parent_span_id: 0,
            actor_id: (2u64 << 48) | 0x65,
            event_type: "spawn".into(),
            msg_type: 0,
            timestamp_ns: base_ns + 100_000_000,
        });
        traces.push(TraceEvent {
            trace_id: "0003".into(),
            span_id: 3,
            parent_span_id: 0,
            actor_id: (3u64 << 48) | 0xC9,
            event_type: "spawn".into(),
            msg_type: 0,
            timestamp_ns: base_ns + 200_000_000,
        });
        // Message sends between nodes
        for i in 0..20u64 {
            traces.push(TraceEvent {
                trace_id: format!("t{:04}", 10 + i),
                span_id: 100 + i,
                parent_span_id: 0,
                actor_id: (1u64 << 48) | 1,
                event_type: "send".into(),
                msg_type: 3,
                timestamp_ns: base_ns + 1_000_000_000 + i * 200_000_000,
            });
        }
        for i in 0..15u64 {
            traces.push(TraceEvent {
                trace_id: format!("t{:04}", 30 + i),
                span_id: 200 + i,
                parent_span_id: 0,
                actor_id: (2u64 << 48) | 0x65,
                event_type: "send".into(),
                msg_type: 7,
                timestamp_ns: base_ns + 1_100_000_000 + i * 300_000_000,
            });
        }
        for i in 0..5u64 {
            traces.push(TraceEvent {
                trace_id: format!("t{:04}", 50 + i),
                span_id: 300 + i,
                parent_span_id: 0,
                actor_id: (3u64 << 48) | 0xC9,
                event_type: "send".into(),
                msg_type: 5,
                timestamp_ns: base_ns + 500_000_000 + i * 500_000_000,
            });
        }
        // Crash on node 3
        traces.push(TraceEvent {
            trace_id: "crash1".into(),
            span_id: 400,
            parent_span_id: 0,
            actor_id: (3u64 << 48) | 0xC9,
            event_type: "crash".into(),
            msg_type: 0,
            timestamp_ns: base_ns + 8_000_000_000,
        });
        // Sort by timestamp
        traces.sort_by_key(|t| t.timestamp_ns);
        self.trace_events = traces;
    }
}

fn flatten_tree(supervisors: &[SupervisorNode]) -> Vec<TreeRow> {
    let mut rows = Vec::new();
    // Find root (first supervisor)
    if let Some(root) = supervisors.first() {
        flatten_node(root, supervisors, 0, &mut rows);
    }
    rows
}

fn flatten_node(
    node: &SupervisorNode,
    all: &[SupervisorNode],
    depth: u16,
    rows: &mut Vec<TreeRow>,
) {
    rows.push(TreeRow {
        depth,
        label: format!("⊞ {} [{}]", node.name, node.strategy),
        state: "Supervisor".to_owned(),
    });
    for child in &node.children {
        // Check if child is also a supervisor
        if let Some(sub) = all.iter().find(|s| s.name == child.name) {
            flatten_node(sub, all, depth + 1, rows);
        } else {
            rows.push(TreeRow {
                depth: depth + 1,
                label: format!("  {} (restarts: {})", child.name, child.restarts),
                state: child.state.to_owned(),
            });
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn demo_app() -> App {
        App::new_demo()
    }

    /// Pressing `/` to re-activate filter mode must NOT clear an existing filter.
    #[test]
    fn filter_text_survives_reactivation() {
        let mut app = demo_app();
        app.filter_text = "running".to_owned();
        app.filter_active = false;

        // Simulate pressing `/` — only set filter_active, do NOT clear filter_text.
        app.filter_active = true;

        assert_eq!(
            app.filter_text, "running",
            "filter text must survive re-activation"
        );
    }

    /// Pressing Esc (explicit clear) must reset both `filter_active` and `filter_text`.
    #[test]
    fn esc_clears_filter_explicitly() {
        let mut app = demo_app();
        app.filter_text = "running".to_owned();
        app.filter_active = true;

        // Simulate Esc handler.
        app.filter_active = false;
        app.filter_text.clear();
        app.clamp_selections();

        assert!(!app.filter_active);
        assert!(app.filter_text.is_empty(), "Esc must clear filter text");
    }

    /// `filter_text` and `sort_column` must survive `next_tab` / `prev_tab` round-trips.
    #[test]
    fn filter_and_sort_persist_across_tab_switch() {
        let mut app = demo_app();
        app.filter_text = "actor42".to_owned();
        app.sort_column = SortColumn::Messages;
        let start_tab = app.active_tab;

        app.next_tab();
        app.next_tab();
        app.prev_tab();
        app.prev_tab();

        assert_eq!(app.active_tab, start_tab);
        assert_eq!(app.filter_text, "actor42");
        assert_eq!(app.sort_column, SortColumn::Messages);
    }

    /// `cycle_sort` advances through all columns and wraps around.
    #[test]
    fn cycle_sort_advances_and_wraps() {
        let mut app = demo_app();
        app.sort_column = SortColumn::Id;
        let total = SORT_COLUMNS.len();
        for _ in 0..total {
            app.cycle_sort();
        }
        assert_eq!(app.sort_column, SortColumn::Id, "sort must wrap back to Id");
    }
}
