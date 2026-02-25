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
        Tab::Cluster => draw_cluster_placeholder(f, area),
        Tab::Messages => draw_messages_placeholder(f, area),
        Tab::Timeline => draw_timeline_placeholder(f, area),
    }
}

fn draw_cluster_placeholder(f: &mut Frame, area: Rect) {
    let msg = Paragraph::new("Cluster view — coming soon")
        .block(Block::default().borders(Borders::ALL).title(" Cluster "))
        .style(Style::default().fg(Color::Yellow));
    f.render_widget(msg, area);
}

fn draw_messages_placeholder(f: &mut Frame, area: Rect) {
    let msg = Paragraph::new("Message flow view — coming soon")
        .block(Block::default().borders(Borders::ALL).title(" Messages "))
        .style(Style::default().fg(Color::Yellow));
    f.render_widget(msg, area);
}

fn draw_timeline_placeholder(f: &mut Frame, area: Rect) {
    let msg = Paragraph::new("Timeline view — coming soon")
        .block(Block::default().borders(Borders::ALL).title(" Timeline "))
        .style(Style::default().fg(Color::Yellow));
    f.render_widget(msg, area);
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
    let text = format!(
        " [{mode}] {} │ Tab: switch │ ?: help │ r: refresh │ q: quit",
        app.base_url
    );
    let bar = Paragraph::new(text).style(Style::default().bg(Color::DarkGray).fg(Color::White));
    f.render_widget(bar, area);
}

fn draw_help_popup(f: &mut Frame) {
    let area = f.area();
    let popup_width = 50;
    let popup_height = 16;
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
