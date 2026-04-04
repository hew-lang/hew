//! Centralized colour palette and reusable styles for the observer TUI.
//!
//! Every colour literal lives here. UI code references named constants and
//! style helper functions so the palette can be adjusted in one place
//! without touching rendering logic.

use ratatui::style::{Color, Modifier, Style};

// ---------------------------------------------------------------------------
// Named colour constants
// ---------------------------------------------------------------------------

/// Accent colour used for highlighted values, sparklines, bar charts, etc.
pub const ACCENT: Color = Color::Cyan;

/// Healthy / running / connected state.
pub const STATE_HEALTHY: Color = Color::Green;

/// Error / crash / disconnected state.
pub const STATE_ERROR: Color = Color::Red;

/// Warning / suspect / runnable state.
pub const STATE_WARNING: Color = Color::Yellow;

/// Idle / stopped / inactive element.
pub const STATE_IDLE: Color = Color::Gray;

/// Blocking state indicator.
pub const STATE_BLOCKED: Color = Color::Blue;

/// Stopping-in-progress state.
pub const STATE_STOPPING: Color = Color::Magenta;

/// Stopped / dead / left — very faint.
pub const STATE_STOPPED: Color = Color::DarkGray;

/// Primary body text.
pub const TEXT_PRIMARY: Color = Color::White;

/// Muted / secondary text (labels, separators).
pub const TEXT_MUTED: Color = Color::Gray;

/// Dim text (empty-state messages, disabled elements).
pub const TEXT_DIM: Color = Color::DarkGray;

/// Memory-related metric values.
pub const METRIC_MEMORY: Color = Color::Magenta;

/// Border / separator lines.
#[expect(
    dead_code,
    reason = "Available for future use in custom border styling"
)]
pub const BORDER: Color = Color::DarkGray;

/// Table / section header background.
pub const HEADER_BG: Color = Color::Black;

/// Table / section header foreground.
pub const HEADER_FG: Color = Color::Yellow;

/// Selected-row background.
pub const SELECTED_BG: Color = Color::DarkGray;

/// Selected-row foreground.
#[expect(
    dead_code,
    reason = "Available for future use in selected-row text styling"
)]
pub const SELECTED_FG: Color = Color::White;

/// Keyboard shortcut foreground.
pub const KEY_FG: Color = Color::Cyan;

/// Status bar background.
pub const STATUS_BAR_BG: Color = Color::DarkGray;

/// Status bar foreground.
pub const STATUS_BAR_FG: Color = Color::White;

/// Bar chart value text background.
pub const BAR_VALUE_BG: Color = Color::Cyan;

/// Bar chart value text foreground.
pub const BAR_VALUE_FG: Color = Color::White;

/// Connection status: connected.
pub const CONN_CONNECTED: Color = Color::Green;

/// Connection status: disconnected.
pub const CONN_DISCONNECTED: Color = Color::Red;

/// Connection status: connecting.
pub const CONN_CONNECTING: Color = Color::Yellow;

// ---------------------------------------------------------------------------
// Lane colours for swimlane / timeline views
// ---------------------------------------------------------------------------

/// Rotating palette for per-node swimlane colouring.
pub const LANE_COLOURS: [Color; 4] = [Color::Cyan, Color::Green, Color::Yellow, Color::Magenta];

// ---------------------------------------------------------------------------
// Reusable style functions
// ---------------------------------------------------------------------------

/// Style for table/section headers: bold, yellow foreground.
pub fn header_style() -> Style {
    Style::default().fg(HEADER_FG).add_modifier(Modifier::BOLD)
}

/// Style for the currently selected row.
pub fn selected_style() -> Style {
    Style::default()
        .bg(SELECTED_BG)
        .add_modifier(Modifier::BOLD)
}

/// Style for keyboard shortcut labels.
pub fn key_style() -> Style {
    Style::default().fg(KEY_FG).add_modifier(Modifier::BOLD)
}

/// Style for muted / secondary text.
pub fn muted_style() -> Style {
    Style::default().fg(TEXT_MUTED)
}

/// Style for dim / empty-state text.
pub fn dim_style() -> Style {
    Style::default().fg(TEXT_DIM)
}

/// Map an actor state string to its display colour.
pub fn actor_state_colour(state: &str) -> Color {
    match state {
        "idle" => STATE_IDLE,
        "runnable" => STATE_WARNING,
        "running" => STATE_HEALTHY,
        "blocked" => STATE_BLOCKED,
        "stopping" => STATE_STOPPING,
        "crashed" => STATE_ERROR,
        "stopped" => STATE_STOPPED,
        _ => TEXT_PRIMARY,
    }
}

/// Map a cluster member state string to its display colour.
pub fn member_state_colour(state: &str) -> Color {
    match state {
        "alive" => STATE_HEALTHY,
        "suspect" => STATE_WARNING,
        "dead" => STATE_ERROR,
        "left" => STATE_STOPPED,
        _ => TEXT_PRIMARY,
    }
}

/// Map a connection state string to its display colour.
pub fn connection_state_colour(state: &str) -> Color {
    match state {
        "active" => STATE_HEALTHY,
        "connecting" | "draining" => STATE_WARNING,
        "closed" => STATE_ERROR,
        _ => TEXT_PRIMARY,
    }
}

/// Map a trace event type to its (glyph, colour) pair for the timeline chart.
pub fn timeline_event_glyph(event_type: &str) -> (char, Color) {
    match event_type {
        "spawn" => ('\u{25C6}', STATE_HEALTHY), // diamond
        "crash" => ('\u{2715}', STATE_ERROR),   // multiply sign
        "send" => ('\u{25CF}', ACCENT),         // filled circle
        "stop" => ('\u{25C7}', STATE_STOPPED),  // open diamond
        _ => ('\u{00B7}', TEXT_PRIMARY),        // middle dot
    }
}
