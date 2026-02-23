//! Application state for the TUI observer.

use crate::client::{ActorInfo, ConnectionStatus, HistoryEntry, Metrics, ProfilerClient};

/// Active tab.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Tab {
    Overview,
    Actors,
    Supervisors,
    Crashes,
}

const TABS: [Tab; 4] = [Tab::Overview, Tab::Actors, Tab::Supervisors, Tab::Crashes];

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

    // Connection
    client: Option<ProfilerClient>,
    pub connection_status: ConnectionStatus,
    pub demo_mode: bool,
    pub base_url: String,

    prev_messages_sent: u64,
    prev_timestamp: f64,
}

impl App {
    pub fn new(base_url: &str, demo: bool) -> Self {
        let client = if demo {
            None
        } else {
            Some(ProfilerClient::new(base_url))
        };

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
            client,
            connection_status: if demo {
                ConnectionStatus::Connected
            } else {
                ConnectionStatus::Connecting
            },
            demo_mode: demo,
            base_url: base_url.to_owned(),
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

        let Some(client) = &mut self.client else {
            return;
        };

        // Only fetch data needed for the current tab to avoid unnecessary blocking
        let metrics = client.fetch_metrics();
        let status = client.status;
        if status == ConnectionStatus::Disconnected {
            self.connection_status = status;
            return;
        }

        let actors = if self.active_tab == Tab::Actors || self.active_tab == Tab::Overview {
            client.fetch_actors()
        } else {
            None
        };

        let history = if self.active_tab == Tab::Overview {
            client.fetch_history()
        } else {
            None
        };

        let status = client.status;

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

        self.connection_status = status;
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
