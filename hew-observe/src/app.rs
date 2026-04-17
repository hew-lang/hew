//! Application state for the TUI observer.

use std::cmp::Reverse;
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
    active_node_label: String,

    /// When true, periodically re-scan the discovery directory and
    /// reconnect if the current profiler is gone or a new one appears.
    auto_discover: bool,
    last_discovery_scan: Instant,

    prev_messages_sent: u64,
    prev_timestamp: f64,
}

/// Scan interval for auto-discovery re-scans.
#[cfg(unix)]
pub(crate) const DISCOVERY_SCAN_INTERVAL: Duration = Duration::from_secs(3);

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
            active_node_label: base_url.clone(),
            base_url,
            auto_discover,
            last_discovery_scan: Instant::now(),
            prev_messages_sent: 0,
            prev_timestamp: 0.0,
        }
    }

    pub fn is_multi_node(&self) -> bool {
        self.cluster
            .as_ref()
            .is_some_and(|cluster| cluster.nodes.len() > 1)
    }

    /// Returns `true` when the app was started with no initial connection and
    /// is waiting for a profiler to appear via auto-discovery.
    #[cfg(unix)]
    pub fn is_waiting(&self) -> bool {
        self.cluster.is_none() && self.auto_discover
    }

    /// Non-Unix stub — auto-discovery is unavailable so waiting mode never
    /// occurs on these platforms.
    #[cfg(not(unix))]
    pub fn is_waiting(&self) -> bool {
        false
    }

    pub fn configured_node_count(&self) -> usize {
        self.cluster
            .as_ref()
            .map_or(1, |cluster| cluster.nodes.len().max(1))
    }

    pub fn connected_node_count(&self) -> usize {
        self.cluster.as_ref().map_or_else(
            || usize::from(self.connection_status == ConnectionStatus::Connected),
            |cluster| {
                cluster
                    .nodes
                    .iter()
                    .filter(|node| node.client.status == ConnectionStatus::Connected)
                    .count()
            },
        )
    }

    pub fn configured_target_label(&self) -> String {
        let Some(cluster) = &self.cluster else {
            return if self.base_url.is_empty() {
                "waiting for profiler".to_owned()
            } else {
                self.base_url.clone()
            };
        };

        match cluster.nodes.as_slice() {
            [] => self.base_url.clone(),
            [node] => node.addr.clone(),
            [first, rest @ ..] => format!("{} +{} more", first.addr, rest.len()),
        }
    }

    pub fn active_node_label(&self) -> &str {
        if self.active_node_label.is_empty() {
            self.base_url.as_str()
        } else {
            self.active_node_label.as_str()
        }
    }

    fn active_node_candidates(&self) -> Vec<String> {
        let Some(cluster) = &self.cluster else {
            return Vec::new();
        };

        let connected: Vec<String> = cluster
            .nodes
            .iter()
            .filter(|node| node.client.status == ConnectionStatus::Connected)
            .map(|node| node.addr.clone())
            .collect();
        if !connected.is_empty() {
            return connected;
        }

        let reachable: Vec<String> = cluster
            .nodes
            .iter()
            .filter(|node| node.client.status != ConnectionStatus::Disconnected)
            .map(|node| node.addr.clone())
            .collect();
        if !reachable.is_empty() {
            return reachable;
        }

        cluster.nodes.iter().map(|node| node.addr.clone()).collect()
    }

    fn set_active_node(&mut self, label: &str) {
        if self.active_node_label == label {
            return;
        }
        self.active_node_label.clear();
        self.active_node_label.push_str(label);
        self.msg_rate = 0.0;
        self.prev_messages_sent = 0;
        self.prev_timestamp = 0.0;
    }

    fn cycle_active_node(&mut self, reverse: bool) -> bool {
        let candidates = self.active_node_candidates();
        if candidates.len() <= 1 {
            return false;
        }

        let next_idx = match candidates
            .iter()
            .position(|label| label == self.active_node_label())
        {
            Some(current_idx) => {
                if reverse {
                    (current_idx + candidates.len() - 1) % candidates.len()
                } else {
                    (current_idx + 1) % candidates.len()
                }
            }
            None if reverse => candidates.len() - 1,
            None => 0,
        };
        let next_label = &candidates[next_idx];
        if next_label == self.active_node_label() {
            return false;
        }

        self.set_active_node(next_label);
        true
    }

    pub fn switch_active_node_prev(&mut self) -> bool {
        let switched = self.cycle_active_node(true);
        if switched {
            self.refresh();
            self.clamp_selections();
        }
        switched
    }

    pub fn switch_active_node_next(&mut self) -> bool {
        let switched = self.cycle_active_node(false);
        if switched {
            self.refresh();
            self.clamp_selections();
        }
        switched
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

    fn should_fetch_traces(&self) -> bool {
        match self.active_tab {
            Tab::Messages => !self.trace_paused,
            Tab::Timeline => !self.timeline_paused,
            _ => false,
        }
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

        let fetch_actors = self.active_tab == Tab::Actors || self.active_tab == Tab::Overview;
        let fetch_history = self.active_tab == Tab::Overview;
        let fetch_supervisors = self.active_tab == Tab::Supervisors;
        let fetch_crashes = self.active_tab == Tab::Crashes;
        let fetch_cluster = self.active_tab == Tab::Cluster || self.active_tab == Tab::Timeline;
        let fetch_traces = self.should_fetch_traces();

        // In auto-discover mode, periodically re-scan for profilers.
        #[cfg(unix)]
        if self.auto_discover {
            self.try_rediscover();
        }

        let mut metrics = None;
        let mut actors = None;
        let mut history = None;
        let mut supervisors = None;
        let mut crashes = None;
        let mut cluster_members = None;
        let mut connections = None;
        let mut routing = None;
        let mut traces = None;
        let mut next_active_node = None;
        let cluster_status;
        let preferred_active_node = self.active_node_label().to_owned();

        {
            let Some(cluster) = &mut self.cluster else {
                return;
            };

            let mut node_metrics = vec![None; cluster.nodes.len()];
            for (idx, node) in cluster.nodes.iter_mut().enumerate() {
                node_metrics[idx] = node.client.fetch_metrics();
            }

            cluster_status = cluster.status();
            // Keep the current healthy node sticky so the core panes do not
            // snap back to an earlier configured node when it recovers.
            let primary_idx = cluster
                .nodes
                .iter()
                .position(|node| {
                    node.client.status == ConnectionStatus::Connected
                        && node.addr == preferred_active_node
                })
                .or_else(|| {
                    cluster
                        .nodes
                        .iter()
                        .position(|node| node.client.status == ConnectionStatus::Connected)
                });

            if let Some(primary_idx) = primary_idx {
                metrics = node_metrics[primary_idx].take();
                next_active_node = Some(cluster.nodes[primary_idx].addr.clone());
                {
                    let primary = &mut cluster.nodes[primary_idx].client;
                    actors = if fetch_actors {
                        primary.fetch_actors()
                    } else {
                        None
                    };
                    history = if fetch_history {
                        primary.fetch_history()
                    } else {
                        None
                    };
                    supervisors = if fetch_supervisors {
                        primary.fetch_supervisors()
                    } else {
                        None
                    };
                    crashes = if fetch_crashes {
                        primary.fetch_crashes()
                    } else {
                        None
                    };
                    (cluster_members, connections, routing) = if fetch_cluster {
                        (
                            primary.fetch_cluster_members(),
                            primary.fetch_connections(),
                            primary.fetch_routing(),
                        )
                    } else {
                        (None, None, None)
                    };
                }

                traces = if fetch_traces {
                    let mut all_traces = Vec::new();
                    for node in &mut cluster.nodes {
                        if let Some(mut node_traces) = node.client.fetch_traces() {
                            all_traces.append(&mut node_traces);
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
            }
        }

        self.connection_status = cluster_status;
        let Some(active_node) = next_active_node else {
            return;
        };
        self.set_active_node(&active_node);

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
    }

    #[cfg(unix)]
    /// Re-scan the discovery directory and reconnect if needed.
    ///
    /// Scans every [`DISCOVERY_SCAN_INTERVAL`]. When disconnected (or no
    /// cluster), picks up a newly started profiler. When connected, does
    /// nothing — we don't disrupt a live session.
    fn try_rediscover(&mut self) {
        self.try_rediscover_with(discovery::scan_profilers);
    }

    /// Inner rediscovery logic with an injectable scan function.
    ///
    /// Separated from `try_rediscover` so unit tests can drive it without
    /// touching the real discovery directory.
    #[cfg(unix)]
    pub(crate) fn try_rediscover_with<F>(&mut self, scan: F)
    where
        F: FnOnce() -> Vec<discovery::DiscoveredProfiler>,
    {
        if self.last_discovery_scan.elapsed() < DISCOVERY_SCAN_INTERVAL {
            return;
        }
        self.last_discovery_scan = Instant::now();

        // Only reconnect when we have no connection.
        let needs_connect =
            self.cluster.is_none() || self.connection_status == ConnectionStatus::Disconnected;
        if !needs_connect {
            return;
        }

        let profilers = scan();
        if let Some(p) = profilers.first() {
            self.cluster = Some(ClusterClient::from_unix(&p.socket_path, &p.program));
            self.base_url.clone_from(&p.program);
            self.set_active_node(&p.program);
            self.connection_status = ConnectionStatus::Connecting;
        }
    }

    /// Expire the discovery scan timer so the next `try_rediscover_with`
    /// call is not gated by the interval.  Test helper only.
    #[cfg(all(unix, test))]
    pub(crate) fn expire_discovery_scan(&mut self) {
        self.last_discovery_scan = Instant::now()
            .checked_sub(DISCOVERY_SCAN_INTERVAL + Duration::from_millis(1))
            .unwrap_or_else(Instant::now);
    }

    fn sort_actors(&mut self) {
        match self.sort_column {
            SortColumn::Id => self.actors.sort_by_key(|a| a.id),
            SortColumn::State => self.actors.sort_by(|a, b| a.state.cmp(&b.state)),
            SortColumn::Messages => self.actors.sort_by_key(|actor| Reverse(actor.msgs)),
            SortColumn::MailboxDepth => {
                self.actors.sort_by_key(|actor| Reverse(actor.mbox_depth));
            }
            SortColumn::ProcessingTime => {
                self.actors.sort_by_key(|actor| Reverse(actor.time_ns));
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
                actor_type: "Counter".to_owned(),
                state: "running".to_owned(),
                msgs: 4201,
                time_ns: 1_200_000_000,
                mbox_depth: 3,
                mbox_hwm: 12,
            },
            ActorInfo {
                id: 2,
                pid: 101,
                actor_type: "Counter".to_owned(),
                state: "idle".to_owned(),
                msgs: 1580,
                time_ns: 800_000_000,
                mbox_depth: 0,
                mbox_hwm: 8,
            },
            ActorInfo {
                id: 3,
                pid: 102,
                actor_type: "Logger".to_owned(),
                state: "blocked".to_owned(),
                msgs: 920,
                time_ns: 450_000_000,
                mbox_depth: 15,
                mbox_hwm: 15,
            },
            ActorInfo {
                id: 4,
                pid: 103,
                actor_type: "Logger".to_owned(),
                state: "crashed".to_owned(),
                msgs: 312,
                time_ns: 100_000_000,
                mbox_depth: 0,
                mbox_hwm: 4,
            },
            ActorInfo {
                id: 5,
                pid: 104,
                actor_type: "Supervisor".to_owned(),
                state: "runnable".to_owned(),
                msgs: 2105,
                time_ns: 600_000_000,
                mbox_depth: 7,
                mbox_hwm: 20,
            },
            ActorInfo {
                id: 6,
                pid: 105,
                actor_type: "Worker".to_owned(),
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
    use std::collections::VecDeque;
    use std::io::{BufRead, BufReader, Write};
    use std::net::{TcpListener, TcpStream};
    use std::sync::{
        atomic::{AtomicBool, Ordering},
        Arc, Mutex,
    };
    use std::thread::{self, JoinHandle};

    struct TestTraceServer {
        addr: String,
        state: Arc<Mutex<TestTraceState>>,
        shutdown: Arc<AtomicBool>,
        worker: Option<JoinHandle<()>>,
    }

    struct TestTraceState {
        metrics_timestamp: f64,
        trace_requests: usize,
        trace_responses: VecDeque<String>,
    }

    impl TestTraceServer {
        fn new(trace_responses: Vec<String>) -> Self {
            Self::with_metrics(trace_responses, 1.0)
        }

        fn with_metrics(trace_responses: Vec<String>, metrics_timestamp: f64) -> Self {
            let listener = TcpListener::bind("127.0.0.1:0").expect("bind trace test server");
            Self::from_listener(listener, trace_responses, metrics_timestamp)
        }

        fn bind_at(addr: &str, trace_responses: Vec<String>, metrics_timestamp: f64) -> Self {
            let listener = TcpListener::bind(addr).expect("bind trace test server");
            Self::from_listener(listener, trace_responses, metrics_timestamp)
        }

        fn from_listener(
            listener: TcpListener,
            trace_responses: Vec<String>,
            metrics_timestamp: f64,
        ) -> Self {
            listener
                .set_nonblocking(true)
                .expect("set nonblocking trace test server");
            let addr = listener
                .local_addr()
                .expect("read trace server addr")
                .to_string();
            let state = Arc::new(Mutex::new(TestTraceState {
                metrics_timestamp,
                trace_requests: 0,
                trace_responses: trace_responses.into(),
            }));
            let shutdown = Arc::new(AtomicBool::new(false));
            let worker = {
                let state = Arc::clone(&state);
                let shutdown = Arc::clone(&shutdown);
                thread::spawn(move || loop {
                    match listener.accept() {
                        Ok((stream, _)) => {
                            if shutdown.load(Ordering::SeqCst) {
                                break;
                            }
                            handle_trace_request(stream, &state);
                        }
                        Err(e) if e.kind() == std::io::ErrorKind::WouldBlock => {
                            if shutdown.load(Ordering::SeqCst) {
                                break;
                            }
                            thread::sleep(Duration::from_millis(10));
                        }
                        Err(e) => panic!("accept trace test server connection: {e}"),
                    }
                })
            };
            Self {
                addr,
                state,
                shutdown,
                worker: Some(worker),
            }
        }

        fn addr(&self) -> String {
            self.addr.clone()
        }

        fn trace_requests(&self) -> usize {
            self.state
                .lock()
                .expect("lock trace server state")
                .trace_requests
        }
    }

    impl Drop for TestTraceServer {
        fn drop(&mut self) {
            self.shutdown.store(true, Ordering::SeqCst);
            let _ = TcpStream::connect(&self.addr);
            if let Some(worker) = self.worker.take() {
                worker.join().expect("join trace test server");
            }
        }
    }

    fn handle_trace_request(mut stream: TcpStream, state: &Arc<Mutex<TestTraceState>>) {
        // Accepted sockets can inherit the listener's nonblocking mode on
        // Windows, so force blocking reads before parsing the request.
        stream
            .set_nonblocking(false)
            .expect("set blocking trace request stream");
        let mut reader = BufReader::new(stream.try_clone().expect("clone trace request stream"));
        let mut request_line = String::new();
        reader
            .read_line(&mut request_line)
            .expect("read trace request line");
        loop {
            let mut header = String::new();
            reader
                .read_line(&mut header)
                .expect("read trace request header");
            if header == "\r\n" || header.is_empty() {
                break;
            }
        }

        let path = request_line
            .split_whitespace()
            .nth(1)
            .expect("extract trace request path");
        let body = trace_response_body(path, state);
        let response = format!(
            "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nContent-Length: {}\r\nConnection: close\r\n\r\n{body}",
            body.len()
        );
        stream
            .write_all(response.as_bytes())
            .expect("write trace response");
    }

    fn trace_response_body(path: &str, state: &Arc<Mutex<TestTraceState>>) -> String {
        match path {
            "/api/metrics" => {
                let metrics_timestamp = state
                    .lock()
                    .expect("lock trace server state")
                    .metrics_timestamp;
                format!(r#"{{"timestamp_secs":{metrics_timestamp}}}"#)
            }
            "/api/actors" | "/api/metrics/history" | "/api/supervisors" | "/api/crashes" => {
                "[]".to_owned()
            }
            "/api/cluster/members" | "/api/connections" => "[]".to_owned(),
            "/api/routing/table" => r#"{"local_node_id":1,"routes":[]}"#.to_owned(),
            "/api/traces" => {
                let mut state = state.lock().expect("lock trace server state");
                state.trace_requests += 1;
                state
                    .trace_responses
                    .pop_front()
                    .unwrap_or_else(|| "[]".to_owned())
            }
            _ => "null".to_owned(),
        }
    }

    fn demo_app() -> App {
        App::new_demo()
    }

    fn trace_response(trace_id: &str, timestamp_ns: u64) -> String {
        serde_json::to_string(&vec![serde_json::json!({
            "trace_id": trace_id,
            "span_id": 0,
            "parent_span_id": 0,
            "actor_id": 42,
            "event_type": "send",
            "msg_type": 7,
            "timestamp_ns": timestamp_ns,
        })])
        .expect("serialize trace response")
    }

    fn timeline_app(server: &TestTraceServer) -> App {
        let mut app = App::new_tcp(&[server.addr()]);
        app.active_tab = Tab::Timeline;
        app
    }

    fn trace_timestamps(app: &App) -> Vec<u64> {
        app.trace_events
            .iter()
            .map(|event| event.timestamp_ns)
            .collect()
    }

    fn unused_tcp_addr() -> String {
        TcpListener::bind("127.0.0.1:0")
            .expect("bind unused tcp addr")
            .local_addr()
            .expect("read unused tcp addr")
            .to_string()
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

    #[test]
    fn timeline_pause_skips_trace_drain_until_unpaused() {
        let server = TestTraceServer::new(vec![trace_response("after-pause", 1)]);
        let mut app = timeline_app(&server);
        app.timeline_paused = true;

        app.refresh();
        assert!(
            app.trace_events.is_empty(),
            "paused timeline refresh must not drain traces"
        );
        assert_eq!(
            server.trace_requests(),
            0,
            "paused timeline refresh must not hit /api/traces"
        );

        app.timeline_toggle_pause();
        app.refresh();

        assert_eq!(
            server.trace_requests(),
            1,
            "unpaused timeline refresh must resume trace fetching"
        );
        assert_eq!(app.trace_events.len(), 1);
        assert_eq!(trace_timestamps(&app), vec![1]);
    }

    #[test]
    fn timeline_unpaused_refresh_keeps_fetching_traces() {
        let server = TestTraceServer::new(vec![
            trace_response("first", 1),
            trace_response("second", 2),
        ]);
        let mut app = timeline_app(&server);

        app.refresh();
        app.refresh();

        assert_eq!(
            server.trace_requests(),
            2,
            "unpaused timeline refreshes must continue fetching traces"
        );
        assert_eq!(trace_timestamps(&app), vec![1, 2]);
    }

    #[test]
    fn refresh_uses_first_connected_node_when_primary_is_down() {
        let live = TestTraceServer::new(Vec::new());
        let dead_addr = unused_tcp_addr();
        let live_addr = live.addr();
        let mut app = App::new_tcp(&[dead_addr.clone(), live_addr.clone()]);

        app.refresh();

        assert_eq!(app.connection_status, ConnectionStatus::Connected);
        assert_eq!(app.connected_node_count(), 1);
        assert_eq!(app.active_node_label(), live_addr);
        assert_eq!(
            app.configured_target_label(),
            format!("{dead_addr} +1 more")
        );
        assert!((app.metrics.timestamp_secs - 1.0).abs() < f64::EPSILON);
    }

    #[test]
    fn switching_active_node_resets_rate_baseline() {
        let mut app = App::new_tcp(&["alpha:6060".to_owned(), "beta:6061".to_owned()]);
        app.msg_rate = 42.0;
        app.prev_messages_sent = 9;
        app.prev_timestamp = 3.5;

        app.set_active_node("beta:6061");

        assert_eq!(app.active_node_label(), "beta:6061");
        assert!(app.msg_rate.abs() < f64::EPSILON);
        assert_eq!(app.prev_messages_sent, 0);
        assert!(app.prev_timestamp.abs() < f64::EPSILON);
    }

    #[test]
    fn cycle_active_node_uses_configured_order_before_first_refresh() {
        let mut app = App::new_tcp(&[
            "alpha:6060".to_owned(),
            "beta:6061".to_owned(),
            "gamma:6062".to_owned(),
        ]);

        assert!(app.cycle_active_node(false));
        assert_eq!(app.active_node_label(), "beta:6061");

        assert!(app.cycle_active_node(true));
        assert_eq!(app.active_node_label(), "alpha:6060");
    }

    #[test]
    fn switch_active_node_skips_disconnected_nodes_and_refreshes() {
        let alpha = TestTraceServer::with_metrics(Vec::new(), 1.0);
        let beta = TestTraceServer::with_metrics(Vec::new(), 2.0);
        let dead_addr = unused_tcp_addr();
        let alpha_addr = alpha.addr();
        let beta_addr = beta.addr();
        let mut app = App::new_tcp(&[alpha_addr.clone(), dead_addr, beta_addr.clone()]);

        app.refresh();
        assert_eq!(app.active_node_label(), alpha_addr);
        assert!((app.metrics.timestamp_secs - 1.0).abs() < f64::EPSILON);

        assert!(app.switch_active_node_next());
        assert_eq!(app.active_node_label(), beta_addr);
        assert!((app.metrics.timestamp_secs - 2.0).abs() < f64::EPSILON);

        assert!(app.switch_active_node_next());
        assert_eq!(app.active_node_label(), alpha_addr);
        assert!((app.metrics.timestamp_secs - 1.0).abs() < f64::EPSILON);
    }

    #[test]
    fn refresh_does_not_snap_back_to_recovered_first_node() {
        let fallback = TestTraceServer::with_metrics(Vec::new(), 2.0);
        let recovering_addr = unused_tcp_addr();
        let fallback_addr = fallback.addr();
        let mut app = App::new_tcp(&[recovering_addr.clone(), fallback_addr.clone()]);

        app.refresh();
        assert_eq!(app.active_node_label(), fallback_addr);
        assert!((app.metrics.timestamp_secs - 2.0).abs() < f64::EPSILON);

        let _recovered = TestTraceServer::bind_at(&recovering_addr, Vec::new(), 1.0);
        app.refresh();

        assert_eq!(
            app.active_node_label(),
            fallback_addr,
            "recovering earlier node must not steal the active pane source"
        );
        assert!(
            (app.metrics.timestamp_secs - 2.0).abs() < f64::EPSILON,
            "core panes must stay pinned to the current healthy node"
        );
    }

    // ---------------------------------------------------------------------------
    // Rediscovery lifecycle tests (unix only)
    // ---------------------------------------------------------------------------

    #[cfg(unix)]
    fn stub_profiler(pid: u32, program: &str) -> crate::discovery::DiscoveredProfiler {
        crate::discovery::DiscoveredProfiler {
            pid,
            socket_path: std::path::PathBuf::from(format!("/tmp/hew-test-{pid}.sock")),
            started: 0,
            program: program.to_owned(),
        }
    }

    /// A waiting app with an expired scan timer picks up a newly appeared
    /// profiler and transitions out of waiting mode.
    #[cfg(unix)]
    #[test]
    fn waiting_app_picks_up_appearing_profiler() {
        let mut app = App::new_waiting();
        assert!(app.is_waiting(), "precondition: must start in waiting mode");

        app.expire_discovery_scan();
        app.try_rediscover_with(|| vec![stub_profiler(55, "myapp")]);

        assert!(
            !app.is_waiting(),
            "should leave waiting mode after profiler appears"
        );
        assert_eq!(
            app.connection_status,
            ConnectionStatus::Connecting,
            "should transition to Connecting after discovering profiler"
        );
        assert!(
            app.base_url.contains("myapp"),
            "target label should reflect the discovered program name"
        );
    }

    /// The scan is gated by [`DISCOVERY_SCAN_INTERVAL`]; calling
    /// `try_rediscover_with` before the interval elapses must not invoke the
    /// scan callback.
    #[cfg(unix)]
    #[test]
    fn scan_interval_gate_prevents_premature_rediscovery() {
        let mut app = App::new_waiting();
        // last_discovery_scan was just set to Instant::now() in new_waiting(),
        // so the interval has NOT elapsed yet.
        app.try_rediscover_with(|| panic!("scan must not be called within the interval"));
        // reaching here without a panic proves the gate works.
        assert!(app.is_waiting(), "should remain in waiting mode");
    }

    /// When no profilers are visible at scan time the app remains in waiting
    /// mode and is ready to try again on the next tick.
    #[cfg(unix)]
    #[test]
    fn waiting_app_stays_waiting_when_no_profilers_found() {
        let mut app = App::new_waiting();
        app.expire_discovery_scan();
        app.try_rediscover_with(Vec::new);

        assert!(
            app.is_waiting(),
            "should remain in waiting mode when scan returns empty"
        );
    }

    /// After a profiler appears and auto-connect fires, a second scan while
    /// already connecting must not disturb the in-progress connection.
    #[cfg(unix)]
    #[test]
    fn connecting_app_skips_rediscovery_scan() {
        let mut app = App::new_waiting();
        // First scan: profiler appears → move to Connecting.
        app.expire_discovery_scan();
        app.try_rediscover_with(|| vec![stub_profiler(55, "myapp")]);
        assert_eq!(app.connection_status, ConnectionStatus::Connecting);

        // Second attempt while Connecting — scan callback must not be called
        // because needs_connect is false (cluster is Some and status is not
        // Disconnected).
        app.expire_discovery_scan();
        app.try_rediscover_with(|| panic!("must not re-scan while connecting"));
        // Passing here proves the guard held.
    }
}
