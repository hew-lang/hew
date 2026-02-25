//! Application state for the TUI observer.

use crate::client::{
    ActorInfo, ClusterClient, ClusterMember, ConnectionInfo, ConnectionStatus, HistoryEntry,
    Metrics, RouteEntry, RoutingSnapshot, TraceEvent,
};

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

/// Demo crash entry (runtime doesn't expose crash data via HTTP yet).
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
    pub state: &'static str,
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

    prev_messages_sent: u64,
    prev_timestamp: f64,
}

impl App {
    pub fn new(node_addrs: &[String], demo: bool) -> Self {
        let cluster = if demo {
            None
        } else {
            Some(ClusterClient::new(node_addrs))
        };

        let base_url = node_addrs
            .first()
            .map_or_else(|| "localhost:6060".to_owned(), Clone::clone);

        let mut app = Self {
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
            prev_messages_sent: 0,
            prev_timestamp: 0.0,
        };

        if demo {
            app.load_demo_data();
        }

        app
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

    #[expect(
        dead_code,
        reason = "Will be wired to key bindings in Messages tab UI task"
    )]
    pub fn messages_set_filter(&mut self, actor_id: u64) {
        self.trace_filter_actor = Some(actor_id);
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

    pub fn refresh(&mut self) {
        if self.demo_mode {
            return;
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

        if let Some(members) = cluster_members {
            self.cluster_members = members;
        }
        if let Some(conns) = connections {
            self.cluster_connections = conns;
        }
        if let Some(r) = routing {
            self.cluster_routing = r;
        }

        if let Some(mut t) = traces {
            self.trace_events.append(&mut t);
            // Cap at 1000 entries
            if self.trace_events.len() > 1000 {
                let drain = self.trace_events.len() - 1000;
                self.trace_events.drain(..drain);
            }
        }

        self.connection_status = cluster_status;
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
            actor_id: (2u64 << 48) | 101,
            event_type: "spawn".into(),
            msg_type: 0,
            timestamp_ns: base_ns + 100_000_000,
        });
        traces.push(TraceEvent {
            trace_id: "0003".into(),
            span_id: 3,
            parent_span_id: 0,
            actor_id: (3u64 << 48) | 201,
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
                actor_id: (2u64 << 48) | 101,
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
                actor_id: (3u64 << 48) | 201,
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
            actor_id: (3u64 << 48) | 201,
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
        state: "Supervisor",
    });
    for child in &node.children {
        // Check if child is also a supervisor
        if let Some(sub) = all.iter().find(|s| s.name == child.name) {
            flatten_node(sub, all, depth + 1, rows);
        } else {
            rows.push(TreeRow {
                depth: depth + 1,
                label: format!("  {} (restarts: {})", child.name, child.restarts),
                state: child.state,
            });
        }
    }
}
