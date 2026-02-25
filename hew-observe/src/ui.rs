//! TUI rendering for the observer.

use ratatui::layout::{Alignment, Constraint, Direction, Layout, Rect};
use ratatui::style::{Color, Modifier, Style};
use ratatui::text::{Line, Span};
use ratatui::widgets::{
    Bar, BarChart, BarGroup, Block, Borders, Cell, Clear, Paragraph, Row, Sparkline, Table, Tabs,
    Wrap,
};
use ratatui::Frame;

use crate::app::{App, SortColumn, Tab};
use crate::client::ConnectionStatus;

pub fn draw(f: &mut Frame, app: &mut App) {
    let chunks = Layout::default()
        .direction(Direction::Vertical)
        .constraints([
            Constraint::Length(3), // header + tabs
            Constraint::Min(0),    // body
            Constraint::Length(1), // status bar
        ])
        .split(f.area());

    draw_header(f, app, chunks[0]);
    draw_body(f, app, chunks[1]);
    draw_status_bar(f, app, chunks[2]);

    if app.show_help {
        draw_help_popup(f);
    }
}

fn draw_header(f: &mut Frame, app: &App, area: Rect) {
    let chunks = Layout::default()
        .direction(Direction::Horizontal)
        .constraints([Constraint::Min(0), Constraint::Length(30)])
        .split(area);

    let titles: Vec<Line> = [
        "① Overview",
        "② Actors",
        "③ Supervisors",
        "④ Crashes",
        "⑤ Cluster",
        "⑥ Messages",
        "⑦ Timeline",
    ]
    .iter()
    .map(|t| Line::from(*t))
    .collect();

    let tab_idx = match app.active_tab {
        Tab::Overview => 0,
        Tab::Actors => 1,
        Tab::Supervisors => 2,
        Tab::Crashes => 3,
        Tab::Cluster => 4,
        Tab::Messages => 5,
        Tab::Timeline => 6,
    };

    let tabs = Tabs::new(titles)
        .block(
            Block::default()
                .borders(Borders::ALL)
                .title(" hew-observe "),
        )
        .select(tab_idx)
        .style(Style::default().fg(Color::White))
        .highlight_style(
            Style::default()
                .fg(Color::Cyan)
                .add_modifier(Modifier::BOLD),
        );
    f.render_widget(tabs, chunks[0]);

    // Connection indicator
    let (status_text, status_color) = match app.connection_status {
        ConnectionStatus::Connected => ("● Connected", Color::Green),
        ConnectionStatus::Disconnected => ("● Disconnected", Color::Red),
        ConnectionStatus::Connecting => ("● Connecting…", Color::Yellow),
    };
    let mode = if app.demo_mode { " [DEMO]" } else { "" };
    let conn = Paragraph::new(format!("{status_text}{mode}"))
        .style(Style::default().fg(status_color))
        .alignment(Alignment::Right)
        .block(Block::default().borders(Borders::ALL));
    f.render_widget(conn, chunks[1]);
}

fn draw_body(f: &mut Frame, app: &mut App, area: Rect) {
    match app.active_tab {
        Tab::Overview => draw_overview(f, app, area),
        Tab::Actors => draw_actors(f, app, area),
        Tab::Supervisors => draw_supervisors(f, app, area),
        Tab::Crashes => draw_crashes(f, app, area),
        Tab::Cluster => draw_cluster(f, app, area),
        Tab::Messages => draw_messages(f, app, area),
        Tab::Timeline => draw_timeline(f, app, area),
    }
}

// ---------------------------------------------------------------------------
// Cluster tab
// ---------------------------------------------------------------------------

fn draw_cluster(f: &mut Frame, app: &App, area: Rect) {
    let chunks = Layout::default()
        .direction(Direction::Vertical)
        .constraints([Constraint::Percentage(50), Constraint::Percentage(50)])
        .split(area);
    draw_cluster_topology(f, app, chunks[0]);
    draw_cluster_members(f, app, chunks[1]);
}

fn member_state_color(state: &str) -> Color {
    match state {
        "alive" => Color::Green,
        "suspect" => Color::Yellow,
        "dead" => Color::Red,
        "left" => Color::DarkGray,
        _ => Color::White,
    }
}

fn draw_cluster_topology(f: &mut Frame, app: &App, area: Rect) {
    let block = Block::default()
        .borders(Borders::ALL)
        .title(" Cluster Topology ");
    let inner = block.inner(area);
    f.render_widget(block, area);

    if app.cluster_members.is_empty() {
        let msg = Paragraph::new("No cluster data")
            .style(Style::default().fg(Color::DarkGray))
            .alignment(Alignment::Center);
        f.render_widget(msg, inner);
        return;
    }

    // Arrange nodes in a 2-column grid
    let members = &app.cluster_members;
    let num_rows = members.len().div_ceil(2);
    let row_constraints: Vec<Constraint> = (0..num_rows)
        .map(|_| Constraint::Length(6))
        .chain(std::iter::once(Constraint::Min(0)))
        .collect();
    let rows = Layout::default()
        .direction(Direction::Vertical)
        .constraints(row_constraints)
        .split(inner);

    for (row_idx, chunk) in rows.iter().enumerate() {
        if row_idx >= num_rows {
            break;
        }
        let cols = Layout::default()
            .direction(Direction::Horizontal)
            .constraints([Constraint::Percentage(50), Constraint::Percentage(50)])
            .split(*chunk);

        for col in 0..2 {
            let member_idx = row_idx * 2 + col;
            if member_idx >= members.len() {
                break;
            }
            let m = &members[member_idx];
            let is_self = m.node_id == app.cluster_routing.local_node_id;
            let title = if is_self {
                format!(" node:{} (self) ", m.node_id)
            } else {
                format!(" node:{} ", m.node_id)
            };

            let color = member_state_color(&m.state);
            let state_bullet = Span::styled(format!("● {}", m.state), Style::default().fg(color));

            // Check if there is a connection to this node
            let conn_status = if is_self {
                Span::raw("")
            } else if app
                .cluster_connections
                .iter()
                .any(|c| c.peer_node_id == m.node_id)
            {
                Span::styled(" ↔ connected", Style::default().fg(Color::Green))
            } else {
                Span::styled(" ✕ no conn", Style::default().fg(Color::DarkGray))
            };

            let lines = vec![
                Line::from(Span::styled(&m.addr, Style::default().fg(Color::White))),
                Line::from(vec![state_bullet, conn_status]),
                Line::from(Span::styled(
                    "msg/s: \u{2014}",
                    Style::default().fg(Color::DarkGray),
                )),
            ];

            let node_block = Block::default()
                .borders(Borders::ALL)
                .title(title)
                .border_style(Style::default().fg(color));
            let para = Paragraph::new(lines).block(node_block);
            f.render_widget(para, cols[col]);
        }
    }
}

#[expect(
    clippy::cast_precision_loss,
    reason = "last_seen_ms values are small enough for display"
)]
fn draw_cluster_members(f: &mut Frame, app: &App, area: Rect) {
    let header = Row::new(vec![
        Cell::from("Node"),
        Cell::from("State"),
        Cell::from("Incarnation"),
        Cell::from("Address"),
        Cell::from("Last Seen"),
    ])
    .style(
        Style::default()
            .fg(Color::Yellow)
            .add_modifier(Modifier::BOLD),
    );

    let rows: Vec<Row> = app
        .cluster_members
        .iter()
        .map(|m| {
            let color = member_state_color(&m.state);
            let last_seen = if m.last_seen_ms == 0 {
                "\u{2014}".to_owned() // em-dash
            } else if m.last_seen_ms >= 1000 {
                format!("{:.1}s ago", m.last_seen_ms as f64 / 1000.0)
            } else {
                format!("{}ms ago", m.last_seen_ms)
            };
            Row::new(vec![
                Cell::from(format!("node:{}", m.node_id)),
                Cell::from(m.state.as_str()).style(Style::default().fg(color)),
                Cell::from(format!("{}", m.incarnation)),
                Cell::from(m.addr.as_str()),
                Cell::from(last_seen),
            ])
        })
        .collect();

    let table = Table::new(
        rows,
        [
            Constraint::Length(10),
            Constraint::Length(10),
            Constraint::Length(12),
            Constraint::Length(22),
            Constraint::Min(12),
        ],
    )
    .header(header)
    .block(
        Block::default()
            .borders(Borders::ALL)
            .title(" Cluster Members "),
    );
    f.render_widget(table, area);
}

// ---------------------------------------------------------------------------
// Messages tab (sequence / swimlane diagram)
// ---------------------------------------------------------------------------

fn draw_messages(f: &mut Frame, app: &App, area: Rect) {
    let chunks = Layout::default()
        .direction(Direction::Vertical)
        .constraints([Constraint::Min(0), Constraint::Length(3)])
        .split(area);
    draw_message_swimlanes(f, app, chunks[0]);
    draw_message_controls(f, app, chunks[1]);
}

#[expect(
    clippy::cast_possible_truncation,
    clippy::cast_precision_loss,
    clippy::too_many_lines,
    reason = "swimlane rendering involves safe small-range conversions and is inherently verbose"
)]
fn draw_message_swimlanes(f: &mut Frame, app: &App, area: Rect) {
    let subtitle = if let Some(actor) = app.trace_filter_actor {
        format!(" Messages  [filtered: actor {actor}] ")
    } else {
        " Messages ".to_owned()
    };
    let block = Block::default().borders(Borders::ALL).title(subtitle);
    let inner = block.inner(area);
    f.render_widget(block, area);

    // Filter events
    let events: Vec<&crate::client::TraceEvent> = app
        .trace_events
        .iter()
        .filter(|e| {
            if let Some(filter_id) = app.trace_filter_actor {
                e.actor_id == filter_id
            } else {
                true
            }
        })
        .filter(|e| {
            e.event_type == "send"
                || e.event_type == "spawn"
                || e.event_type == "crash"
                || e.event_type == "stop"
        })
        .collect();

    if events.is_empty() {
        let msg = Paragraph::new("No trace events. Enable tracing: HEW_TRACE=1")
            .style(Style::default().fg(Color::DarkGray))
            .alignment(Alignment::Center);
        f.render_widget(msg, inner);
        return;
    }

    // Collect unique node IDs
    let mut node_ids: Vec<u16> = events.iter().map(|e| (e.actor_id >> 48) as u16).collect();
    for m in &app.cluster_members {
        node_ids.push(m.node_id);
    }
    node_ids.sort_unstable();
    node_ids.dedup();

    let lane_colors = [Color::Cyan, Color::Green, Color::Yellow, Color::Magenta];
    let num_lanes = node_ids.len();
    if num_lanes == 0 || inner.width < 4 || inner.height < 3 {
        return;
    }

    let available_height = inner.height as usize;
    // Reserve 2 lines for header
    let event_rows = available_height.saturating_sub(2);
    if event_rows == 0 {
        return;
    }

    // Determine visible events slice
    let visible_events = if app.trace_paused {
        let start = app
            .trace_scroll
            .min(events.len().saturating_sub(event_rows));
        let end = (start + event_rows).min(events.len());
        &events[start..end]
    } else {
        let start = events.len().saturating_sub(event_rows);
        &events[start..]
    };

    // Compute lane x positions
    let lane_width = inner.width / num_lanes as u16;
    // Build lines: header first
    let mut lines: Vec<Line> = Vec::new();

    // Header line with node labels
    let mut header_spans = Vec::new();
    for (i, &nid) in node_ids.iter().enumerate() {
        let label = format!("node:{nid}");
        let color = lane_colors[i % lane_colors.len()];
        let pad = lane_width as usize;
        let formatted = format!("{label:^pad$}");
        header_spans.push(Span::styled(
            formatted,
            Style::default().fg(color).add_modifier(Modifier::BOLD),
        ));
    }
    lines.push(Line::from(header_spans));

    // Separator
    let sep = "\u{2500}".repeat(inner.width as usize);
    lines.push(Line::from(Span::styled(
        sep,
        Style::default().fg(Color::DarkGray),
    )));

    // Get base timestamp for relative display
    let base_ns = events.first().map_or(0, |e| e.timestamp_ns);

    // Event rows
    for evt in visible_events {
        let node_id = (evt.actor_id >> 48) as u16;
        let lane_idx = node_ids.iter().position(|&n| n == node_id).unwrap_or(0);
        let color = lane_colors[lane_idx % lane_colors.len()];

        // Build the row: show vertical bars at each lane, with event info at the source lane
        let relative_s = (evt.timestamp_ns.saturating_sub(base_ns)) as f64 / 1_000_000_000.0;
        let time_str = format!("{relative_s:>6.2}s");

        let event_label = match evt.event_type.as_str() {
            "send" => format!("──▶ send({})", evt.msg_type),
            "spawn" => "◆ spawn".to_owned(),
            "crash" => "✕ crash".to_owned(),
            "stop" => "◇ stop".to_owned(),
            other => other.to_owned(),
        };

        let mut row_spans = Vec::new();
        row_spans.push(Span::styled(
            format!("{time_str} "),
            Style::default().fg(Color::DarkGray),
        ));

        let remaining_width = inner.width.saturating_sub(8) as usize;
        for (i, _) in node_ids.iter().enumerate() {
            let segment_width = remaining_width / num_lanes;
            if i == lane_idx {
                let label_display = if event_label.len() > segment_width {
                    event_label[..segment_width].to_owned()
                } else {
                    format!("{event_label:<segment_width$}")
                };
                let event_color = match evt.event_type.as_str() {
                    "spawn" => Color::Green,
                    "crash" => Color::Red,
                    "stop" => Color::DarkGray,
                    _ => color,
                };
                row_spans.push(Span::styled(
                    label_display,
                    Style::default().fg(event_color),
                ));
            } else {
                let bar = format!("{:\u{2502}^width$}", "", width = segment_width);
                row_spans.push(Span::styled(bar, Style::default().fg(Color::DarkGray)));
            }
        }
        lines.push(Line::from(row_spans));
    }

    let para = Paragraph::new(lines);
    f.render_widget(para, inner);
}

fn draw_message_controls(f: &mut Frame, app: &App, area: Rect) {
    let paused_str = if app.trace_paused { "yes" } else { "no" };
    let filter_str = if let Some(actor) = app.trace_filter_actor {
        format!("  │  Filter: actor {actor}")
    } else {
        String::new()
    };
    let text = format!(
        " [space/p] pause  [↑↓] scroll  [c] clear filter │ Events: {} │ Paused: {paused_str}{filter_str}",
        app.trace_events.len()
    );
    let bar = Paragraph::new(text).block(Block::default().borders(Borders::ALL));
    f.render_widget(bar, area);
}

// ---------------------------------------------------------------------------
// Timeline tab
// ---------------------------------------------------------------------------

fn draw_timeline(f: &mut Frame, app: &App, area: Rect) {
    let chunks = Layout::default()
        .direction(Direction::Vertical)
        .constraints([
            Constraint::Min(0),
            Constraint::Length(2),
            Constraint::Length(3),
        ])
        .split(area);
    draw_timeline_chart(f, app, chunks[0]);
    draw_timeline_legend(f, chunks[1]);
    draw_timeline_controls(f, app, chunks[2]);
}

#[expect(
    clippy::cast_possible_truncation,
    clippy::cast_precision_loss,
    clippy::cast_sign_loss,
    clippy::cast_possible_wrap,
    clippy::similar_names,
    clippy::too_many_lines,
    reason = "timeline pixel math involves safe small-range conversions; inherently verbose"
)]
fn draw_timeline_chart(f: &mut Frame, app: &App, area: Rect) {
    let block = Block::default().borders(Borders::ALL).title(" Timeline ");
    let inner = block.inner(area);
    f.render_widget(block, area);

    if inner.width < 10 || inner.height < 4 {
        return;
    }

    // Collect node IDs from cluster members and trace events
    let mut node_ids: Vec<u16> = app.cluster_members.iter().map(|m| m.node_id).collect();
    for evt in &app.trace_events {
        let nid = (evt.actor_id >> 48) as u16;
        if !node_ids.contains(&nid) {
            node_ids.push(nid);
        }
    }
    node_ids.sort_unstable();
    node_ids.dedup();

    if node_ids.is_empty() || app.trace_events.is_empty() {
        let msg = Paragraph::new("No events in window")
            .style(Style::default().fg(Color::DarkGray))
            .alignment(Alignment::Center);
        f.render_widget(msg, inner);
        return;
    }

    // Determine visible time window
    let latest_ns = app
        .trace_events
        .iter()
        .map(|e| e.timestamp_ns)
        .max()
        .unwrap_or(0);
    let window_ns = app.timeline_window_ns;
    let center_ns = (latest_ns as i64 + app.timeline_offset_ns).max(0) as u64;
    let window_start = center_ns.saturating_sub(window_ns / 2);
    let window_end = window_start + window_ns;

    // Left margin for node labels
    let label_width: u16 = 10;
    let chart_width = inner.width.saturating_sub(label_width) as usize;
    if chart_width < 4 {
        return;
    }

    let mut lines: Vec<Line> = Vec::new();

    // Time axis header
    let mut time_header_spans = Vec::new();
    time_header_spans.push(Span::styled(
        format!("{:<width$}", "", width = label_width as usize),
        Style::default(),
    ));
    let num_ticks = 5.min(chart_width / 8);
    if num_ticks > 0 {
        let tick_spacing = chart_width / num_ticks;
        for i in 0..num_ticks {
            let x = i * tick_spacing;
            let t_ns = window_start as f64 + (x as f64 / chart_width as f64) * window_ns as f64;
            let t_s = t_ns / 1_000_000_000.0;
            let label = format!("{t_s:.1}s");
            let padded = format!("{label:<tick_spacing$}");
            time_header_spans.push(Span::styled(padded, Style::default().fg(Color::DarkGray)));
        }
    }
    lines.push(Line::from(time_header_spans));

    // One row per node
    let lane_colors = [Color::Cyan, Color::Green, Color::Yellow, Color::Magenta];

    for (node_idx, &nid) in node_ids.iter().enumerate() {
        // Node label
        let label = format!("node:{nid} \u{2502}");
        let padded_label = format!("{label:>width$}", width = label_width as usize);

        // Build the chart row
        let mut row_chars: Vec<(char, Color)> = vec![(' ', Color::DarkGray); chart_width];

        // Place events
        let node_events: Vec<&crate::client::TraceEvent> = app
            .trace_events
            .iter()
            .filter(|e| {
                let enid = (e.actor_id >> 48) as u16;
                enid == nid && e.timestamp_ns >= window_start && e.timestamp_ns < window_end
            })
            .collect();

        for evt in &node_events {
            let x = ((evt.timestamp_ns - window_start) as f64 / window_ns as f64
                * chart_width as f64) as usize;
            if x < chart_width {
                let (glyph, color) = match evt.event_type.as_str() {
                    "spawn" => ('\u{25C6}', Color::Green),   // ◆
                    "crash" => ('\u{2715}', Color::Red),     // ✕
                    "send" => ('\u{25CF}', Color::Cyan),     // ●
                    "stop" => ('\u{25C7}', Color::DarkGray), // ◇
                    _ => ('\u{00B7}', Color::White),         // ·
                };
                // If there's already something at this position (density), keep the higher-priority one
                let existing = row_chars[x].0;
                if existing == ' ' || existing == '\u{25CF}' {
                    row_chars[x] = (glyph, color);
                }
            }
        }

        let node_color = lane_colors[node_idx % lane_colors.len()];
        let mut spans = Vec::new();
        spans.push(Span::styled(
            padded_label,
            Style::default().fg(node_color).add_modifier(Modifier::BOLD),
        ));
        for (ch, color) in &row_chars {
            spans.push(Span::styled(ch.to_string(), Style::default().fg(*color)));
        }
        lines.push(Line::from(spans));

        // Separator between rows (if not last)
        if node_idx < node_ids.len() - 1 {
            let sep = format!(
                "{:>width$}{}",
                "",
                "\u{2500}".repeat(chart_width),
                width = label_width as usize
            );
            lines.push(Line::from(Span::styled(
                sep,
                Style::default().fg(Color::DarkGray),
            )));
        }
    }

    let para = Paragraph::new(lines);
    f.render_widget(para, inner);
}

fn draw_timeline_legend(f: &mut Frame, area: Rect) {
    let legend = Line::from(vec![
        Span::styled(" \u{25C6} ", Style::default().fg(Color::Green)),
        Span::raw("spawn  "),
        Span::styled("\u{2715} ", Style::default().fg(Color::Red)),
        Span::raw("crash  "),
        Span::styled("\u{25CF} ", Style::default().fg(Color::Cyan)),
        Span::raw("message  "),
        Span::styled("\u{25C7} ", Style::default().fg(Color::DarkGray)),
        Span::raw("stop  "),
        Span::styled("\u{25B2} ", Style::default().fg(Color::Yellow)),
        Span::raw("suspect  "),
        Span::styled("\u{2605} ", Style::default().fg(Color::Red)),
        Span::raw("dead"),
    ]);
    let para = Paragraph::new(legend).alignment(Alignment::Center);
    f.render_widget(para, area);
}

#[expect(
    clippy::cast_precision_loss,
    reason = "timeline offset/window values are small enough for display"
)]
fn draw_timeline_controls(f: &mut Frame, app: &App, area: Rect) {
    let paused_str = if app.timeline_paused { "yes" } else { "no" };
    let window_s = app.timeline_window_ns as f64 / 1_000_000_000.0;
    let offset_s = app.timeline_offset_ns as f64 / 1_000_000_000.0;
    let text = format!(
        " [←→] scroll  [+/-] zoom  [p] pause  [n] snap to now │ Window: {window_s:.0}s │ Offset: {offset_s:+.1}s │ Events: {} │ Paused: {paused_str}",
        app.trace_events.len()
    );
    let bar = Paragraph::new(text).block(Block::default().borders(Borders::ALL));
    f.render_widget(bar, area);
}

fn draw_overview(f: &mut Frame, app: &App, area: Rect) {
    let chunks = Layout::default()
        .direction(Direction::Vertical)
        .constraints([
            Constraint::Length(8),  // stats
            Constraint::Length(10), // sparklines
            Constraint::Min(0),     // top actors bar chart
        ])
        .split(area);

    draw_overview_stats(f, app, chunks[0]);
    draw_overview_sparklines(f, app, chunks[1]);
    draw_overview_top_actors(f, app, chunks[2]);
}

fn draw_overview_stats(f: &mut Frame, app: &App, area: Rect) {
    let m = &app.metrics;
    let stats_text = vec![
        Line::from(vec![
            Span::styled("Actors Spawned: ", Style::default().fg(Color::Gray)),
            Span::styled(
                format!("{}", m.tasks_spawned),
                Style::default().fg(Color::Cyan),
            ),
            Span::raw("    "),
            Span::styled("Completed: ", Style::default().fg(Color::Gray)),
            Span::styled(
                format!("{}", m.tasks_completed),
                Style::default().fg(Color::Cyan),
            ),
            Span::raw("    "),
            Span::styled("Active: ", Style::default().fg(Color::Gray)),
            Span::styled(
                format!("{}", m.tasks_spawned.saturating_sub(m.tasks_completed)),
                Style::default()
                    .fg(Color::Green)
                    .add_modifier(Modifier::BOLD),
            ),
        ]),
        Line::from(vec![
            Span::styled("Messages Sent: ", Style::default().fg(Color::Gray)),
            Span::styled(
                format!("{}", m.messages_sent),
                Style::default().fg(Color::Cyan),
            ),
            Span::raw("    "),
            Span::styled("Received: ", Style::default().fg(Color::Gray)),
            Span::styled(
                format!("{}", m.messages_received),
                Style::default().fg(Color::Cyan),
            ),
            Span::raw("    "),
            Span::styled("Rate: ", Style::default().fg(Color::Gray)),
            Span::styled(
                format!("{:.1} msg/s", app.msg_rate),
                Style::default()
                    .fg(Color::Yellow)
                    .add_modifier(Modifier::BOLD),
            ),
        ]),
        Line::from(vec![
            Span::styled("Workers: ", Style::default().fg(Color::Gray)),
            Span::styled(
                format!("{}", m.active_workers),
                Style::default().fg(Color::Cyan),
            ),
            Span::raw("    "),
            Span::styled("Work Steals: ", Style::default().fg(Color::Gray)),
            Span::styled(format!("{}", m.steals), Style::default().fg(Color::Cyan)),
        ]),
        Line::from(vec![
            Span::styled("Memory Live: ", Style::default().fg(Color::Gray)),
            Span::styled(
                format_bytes(m.bytes_live),
                Style::default().fg(Color::Magenta),
            ),
            Span::raw("    "),
            Span::styled("Peak: ", Style::default().fg(Color::Gray)),
            Span::styled(
                format_bytes(m.peak_bytes_live),
                Style::default().fg(Color::Magenta),
            ),
            Span::raw("    "),
            Span::styled("Allocs: ", Style::default().fg(Color::Gray)),
            Span::styled(
                format!("{}", m.alloc_count),
                Style::default().fg(Color::Cyan),
            ),
        ]),
        Line::from(vec![
            Span::styled("Uptime: ", Style::default().fg(Color::Gray)),
            Span::styled(
                format_duration(m.timestamp_secs),
                Style::default().fg(Color::White),
            ),
        ]),
    ];
    let stats = Paragraph::new(stats_text).block(
        Block::default()
            .borders(Borders::ALL)
            .title(" System Overview "),
    );
    f.render_widget(stats, area);
}

fn draw_overview_sparklines(f: &mut Frame, app: &App, area: Rect) {
    let spark_chunks = Layout::default()
        .direction(Direction::Horizontal)
        .constraints([Constraint::Percentage(50), Constraint::Percentage(50)])
        .split(area);

    let msg_sparkline = Sparkline::default()
        .block(
            Block::default()
                .borders(Borders::ALL)
                .title(" Messages/sec "),
        )
        .data(&app.sparkline_msgs)
        .style(Style::default().fg(Color::Cyan));
    f.render_widget(msg_sparkline, spark_chunks[0]);

    let actor_sparkline = Sparkline::default()
        .block(
            Block::default()
                .borders(Borders::ALL)
                .title(" Active Workers "),
        )
        .data(&app.sparkline_actors)
        .style(Style::default().fg(Color::Green));
    f.render_widget(actor_sparkline, spark_chunks[1]);
}

fn draw_overview_top_actors(f: &mut Frame, app: &App, area: Rect) {
    let mut sorted = app.actors.clone();
    sorted.sort_by(|a, b| b.msgs.cmp(&a.msgs));
    sorted.truncate(5);

    let bars: Vec<Bar> = sorted
        .iter()
        .map(|a| {
            Bar::default()
                .label(Line::from(format!("actor:{}", a.id)))
                .value(a.msgs)
                .style(Style::default().fg(Color::Cyan))
        })
        .collect();

    let chart = BarChart::default()
        .block(
            Block::default()
                .borders(Borders::ALL)
                .title(" Top 5 Actors by Messages "),
        )
        .data(BarGroup::default().bars(&bars))
        .bar_width(12)
        .bar_gap(2)
        .value_style(Style::default().fg(Color::White).bg(Color::Cyan));
    f.render_widget(chart, area);
}

fn draw_actors(f: &mut Frame, app: &mut App, area: Rect) {
    let chunks = Layout::default()
        .direction(Direction::Vertical)
        .constraints([Constraint::Length(3), Constraint::Min(0)])
        .split(area);

    // Filter bar
    let filter_text = if app.filter_active {
        format!("Filter: {}▏", app.filter_text)
    } else if app.filter_text.is_empty() {
        "Press / to filter, s to sort".into()
    } else {
        format!("Filter: {} (press / to edit)", app.filter_text)
    };
    let sort_label = match app.sort_column {
        SortColumn::Id => "ID",
        SortColumn::State => "State",
        SortColumn::Messages => "Messages",
        SortColumn::MailboxDepth => "Mailbox",
        SortColumn::ProcessingTime => "Time",
    };
    let filter_bar = Paragraph::new(format!("{filter_text}  │  Sort: {sort_label}"))
        .block(Block::default().borders(Borders::ALL).title(" Actor List "));
    f.render_widget(filter_bar, chunks[0]);

    // Actor table
    let header = Row::new(vec![
        Cell::from("ID"),
        Cell::from("PID"),
        Cell::from("State"),
        Cell::from("Messages"),
        Cell::from("Mailbox"),
        Cell::from("HWM"),
        Cell::from("CPU Time"),
    ])
    .style(
        Style::default()
            .fg(Color::Yellow)
            .add_modifier(Modifier::BOLD),
    );

    let filtered = app.filtered_actors();
    let rows: Vec<Row> = filtered
        .iter()
        .enumerate()
        .map(|(i, a)| {
            let state_color = state_color(&a.state);
            let style = if i == app.actor_selected {
                Style::default()
                    .bg(Color::DarkGray)
                    .add_modifier(Modifier::BOLD)
            } else {
                Style::default()
            };
            Row::new(vec![
                Cell::from(format!("{}", a.id)),
                Cell::from(format!("{}", a.pid)),
                Cell::from(a.state_name()).style(Style::default().fg(state_color)),
                Cell::from(format!("{}", a.msgs)),
                Cell::from(format!("{}", a.mbox_depth)),
                Cell::from(format!("{}", a.mbox_hwm)),
                Cell::from(format_ns(a.time_ns)),
            ])
            .style(style)
        })
        .collect();

    let table = Table::new(
        rows,
        [
            Constraint::Length(8),
            Constraint::Length(8),
            Constraint::Length(12),
            Constraint::Length(12),
            Constraint::Length(10),
            Constraint::Length(8),
            Constraint::Min(12),
        ],
    )
    .header(header)
    .block(Block::default().borders(Borders::ALL));
    f.render_widget(table, chunks[1]);
}

fn draw_supervisors(f: &mut Frame, app: &App, area: Rect) {
    if app.tree_rows.is_empty() && !app.demo_mode {
        let msg = Paragraph::new(
            "Supervision tree data is not yet available via the runtime HTTP endpoint.\n\n\
             The runtime tracks supervisor trees internally but does not expose them\n\
             over the profiler API. A future /api/supervisors endpoint is needed.\n\n\
             Use --demo flag to see a sample supervision tree.",
        )
        .block(
            Block::default()
                .borders(Borders::ALL)
                .title(" Supervision Tree "),
        )
        .style(Style::default().fg(Color::Yellow))
        .wrap(Wrap { trim: false });
        f.render_widget(msg, area);
        return;
    }

    let rows: Vec<Row> = app
        .tree_rows
        .iter()
        .enumerate()
        .map(|(i, row)| {
            let indent = "  ".repeat(row.depth as usize);
            let color = match row.state {
                "Running" | "Supervisor" => Color::Green,
                "Idle" => Color::Gray,
                "Crashed" => Color::Red,
                _ => Color::Yellow,
            };
            let style = if i == app.tree_selected {
                Style::default()
                    .bg(Color::DarkGray)
                    .add_modifier(Modifier::BOLD)
            } else {
                Style::default()
            };
            Row::new(vec![
                Cell::from(format!("{indent}{}", row.label)).style(Style::default().fg(color)),
                Cell::from(row.state).style(Style::default().fg(color)),
            ])
            .style(style)
        })
        .collect();

    let table = Table::new(rows, [Constraint::Min(40), Constraint::Length(15)])
        .header(
            Row::new(vec!["Tree", "State"]).style(
                Style::default()
                    .fg(Color::Yellow)
                    .add_modifier(Modifier::BOLD),
            ),
        )
        .block(
            Block::default()
                .borders(Borders::ALL)
                .title(" Supervision Tree "),
        );
    f.render_widget(table, area);
}

fn draw_crashes(f: &mut Frame, app: &App, area: Rect) {
    if app.crashes.is_empty() && !app.demo_mode {
        let msg = Paragraph::new(
            "Crash log data is not yet available via the runtime HTTP endpoint.\n\n\
             The runtime maintains a 64-entry crash ring buffer internally but does\n\
             not expose it over the profiler API. A future /api/crashes endpoint is needed.\n\n\
             Use --demo flag to see sample crash data.",
        )
        .block(Block::default().borders(Borders::ALL).title(" Crash Log "))
        .style(Style::default().fg(Color::Yellow))
        .wrap(Wrap { trim: false });
        f.render_widget(msg, area);
        return;
    }

    let header = Row::new(vec![
        Cell::from("Time"),
        Cell::from("Actor"),
        Cell::from("Signal"),
        Cell::from("Msg Type"),
        Cell::from("Fault Address"),
    ])
    .style(
        Style::default()
            .fg(Color::Yellow)
            .add_modifier(Modifier::BOLD),
    );

    let rows: Vec<Row> = app
        .crashes
        .iter()
        .enumerate()
        .map(|(i, c)| {
            let style = if i == app.crash_selected {
                Style::default()
                    .bg(Color::DarkGray)
                    .add_modifier(Modifier::BOLD)
            } else {
                Style::default()
            };
            let sig_name = match c.signal {
                6 => "SIGABRT".to_owned(),
                11 => "SIGSEGV".to_owned(),
                s => format!("SIG({s})"),
            };
            Row::new(vec![
                Cell::from(format!("{:.1}s", c.time_s)),
                Cell::from(format!("actor:{}", c.actor_id)),
                Cell::from(sig_name).style(Style::default().fg(Color::Red)),
                Cell::from(format!("{}", c.msg_type)),
                Cell::from(format!("{:#018x}", c.fault_addr)),
            ])
            .style(style)
        })
        .collect();

    let table = Table::new(
        rows,
        [
            Constraint::Length(10),
            Constraint::Length(12),
            Constraint::Length(10),
            Constraint::Length(10),
            Constraint::Min(20),
        ],
    )
    .header(header)
    .block(
        Block::default()
            .borders(Borders::ALL)
            .title(" Crash Log (most recent first) "),
    );
    f.render_widget(table, area);
}

fn draw_status_bar(f: &mut Frame, app: &App, area: Rect) {
    let mode = if app.demo_mode { "DEMO" } else { "LIVE" };
    let node_count = if app.cluster_members.is_empty() {
        1
    } else {
        app.cluster_members.len()
    };
    let text = format!(
        " [{mode}] {} │ {node_count} node(s) │ Tab: switch │ ?: help │ r: refresh │ q: quit",
        app.base_url
    );
    let bar = Paragraph::new(text).style(Style::default().bg(Color::DarkGray).fg(Color::White));
    f.render_widget(bar, area);
}

fn draw_help_popup(f: &mut Frame) {
    let area = f.area();
    let popup_width = 55;
    let popup_height = 23;
    let x = area.width.saturating_sub(popup_width) / 2;
    let y = area.height.saturating_sub(popup_height) / 2;
    let popup = Rect::new(
        x,
        y,
        popup_width.min(area.width),
        popup_height.min(area.height),
    );

    f.render_widget(Clear, popup);

    let help_text = vec![
        Line::from(""),
        Line::from(vec![
            Span::styled("  Tab/Shift+Tab  ", style_key()),
            Span::raw("Switch tabs"),
        ]),
        Line::from(vec![
            Span::styled("  q              ", style_key()),
            Span::raw("Quit"),
        ]),
        Line::from(vec![
            Span::styled("  r              ", style_key()),
            Span::raw("Force refresh"),
        ]),
        Line::from(vec![
            Span::styled("  ?              ", style_key()),
            Span::raw("Toggle help"),
        ]),
        Line::from(vec![
            Span::styled("  ↑/↓            ", style_key()),
            Span::raw("Navigate lists"),
        ]),
        Line::from(vec![
            Span::styled("  /              ", style_key()),
            Span::raw("Filter actors"),
        ]),
        Line::from(vec![
            Span::styled("  s              ", style_key()),
            Span::raw("Cycle sort column"),
        ]),
        Line::from(vec![
            Span::styled("  Esc            ", style_key()),
            Span::raw("Cancel filter"),
        ]),
        Line::from(vec![
            Span::styled("  Ctrl+C         ", style_key()),
            Span::raw("Force quit"),
        ]),
        Line::from(""),
        Line::from(Span::styled(
            "  Distributed tabs:",
            Style::default()
                .fg(Color::Yellow)
                .add_modifier(Modifier::BOLD),
        )),
        Line::from(vec![
            Span::styled("  space/p        ", style_key()),
            Span::raw("Pause (Messages/Timeline)"),
        ]),
        Line::from(vec![
            Span::styled("  ←/→            ", style_key()),
            Span::raw("Scroll time (Timeline)"),
        ]),
        Line::from(vec![
            Span::styled("  +/-            ", style_key()),
            Span::raw("Zoom in/out (Timeline)"),
        ]),
        Line::from(vec![
            Span::styled("  n              ", style_key()),
            Span::raw("Snap to now (Timeline)"),
        ]),
        Line::from(vec![
            Span::styled("  c              ", style_key()),
            Span::raw("Clear filter (Messages)"),
        ]),
        Line::from(""),
    ];

    let help = Paragraph::new(help_text).block(
        Block::default()
            .borders(Borders::ALL)
            .title(" Key Bindings ")
            .style(Style::default().bg(Color::Black)),
    );
    f.render_widget(help, popup);
}

fn style_key() -> Style {
    Style::default()
        .fg(Color::Cyan)
        .add_modifier(Modifier::BOLD)
}

fn state_color(state: &str) -> Color {
    match state {
        "idle" => Color::Gray,
        "runnable" => Color::Yellow,
        "running" => Color::Green,
        "blocked" => Color::Blue,
        "stopping" => Color::Magenta,
        "crashed" => Color::Red,
        "stopped" => Color::DarkGray,
        _ => Color::White,
    }
}

#[expect(
    clippy::cast_precision_loss,
    reason = "display formatting doesn't need full u64 precision"
)]
fn format_bytes(bytes: u64) -> String {
    if bytes >= 1_073_741_824 {
        format!("{:.1} GiB", bytes as f64 / 1_073_741_824.0)
    } else if bytes >= 1_048_576 {
        format!("{:.1} MiB", bytes as f64 / 1_048_576.0)
    } else if bytes >= 1024 {
        format!("{:.1} KiB", bytes as f64 / 1024.0)
    } else {
        format!("{bytes} B")
    }
}

#[expect(
    clippy::cast_possible_truncation,
    clippy::cast_sign_loss,
    reason = "duration seconds are always small positive values"
)]
fn format_duration(secs: f64) -> String {
    let total = secs as u64;
    let h = total / 3600;
    let m = (total % 3600) / 60;
    let s = total % 60;
    if h > 0 {
        format!("{h}h {m}m {s}s")
    } else if m > 0 {
        format!("{m}m {s}s")
    } else {
        format!("{s}s")
    }
}

#[expect(
    clippy::cast_precision_loss,
    reason = "display formatting doesn't need full u64 precision"
)]
fn format_ns(ns: u64) -> String {
    if ns >= 1_000_000_000 {
        format!("{:.2}s", ns as f64 / 1_000_000_000.0)
    } else if ns >= 1_000_000 {
        format!("{:.1}ms", ns as f64 / 1_000_000.0)
    } else if ns >= 1000 {
        format!("{:.1}µs", ns as f64 / 1000.0)
    } else {
        format!("{ns}ns")
    }
}
