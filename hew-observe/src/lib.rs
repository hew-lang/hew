//! TUI actor observer for debugging Hew actor systems.
//!
//! Connects to a running Hew program's profiler HTTP endpoint
//! (enabled via `HEW_PPROF=:port` or `HEW_PPROF=auto`) and displays
//! live metrics, actor lists, supervision trees, and crash logs.

mod app;
mod client;
#[cfg(unix)]
mod discovery;
mod events;
mod theme;
mod ui;

use std::fmt;
use std::io;
use std::io::IsTerminal as _;
use std::time::{Duration, Instant};
#[cfg(unix)]
use std::time::{SystemTime, UNIX_EPOCH};

use app::{App, Tab};
use client::ClientError;
use crossterm::event::{self, Event, KeyCode, KeyModifiers};
use crossterm::terminal::{
    disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen,
};
use crossterm::ExecutableCommand as _;
use ratatui::backend::CrosstermBackend;
use ratatui::Terminal;

/// TUI actor observer for Hew — inspect actors, messages, and crashes in real time.
#[derive(clap::Args, Debug)]
pub struct Options {
    /// Address of the Hew profiler endpoint (host:port).
    #[arg(long)]
    addr: Option<String>,

    /// Connect to processes by PID (via unix socket discovery; repeatable).
    #[arg(long, value_delimiter = ',')]
    pid: Vec<u32>,

    /// List discovered profiler processes and exit.
    #[arg(long)]
    list: bool,

    /// Additional node endpoints for multi-node observation (repeatable).
    #[arg(long = "node")]
    node: Vec<String>,

    /// Refresh interval in milliseconds
    #[arg(long, default_value_t = 1000)]
    refresh_ms: u64,

    /// Start in demo mode with sample data (no HTTP connection)
    #[arg(long)]
    demo: bool,
}

#[derive(Debug)]
enum ConnectError {
    #[cfg_attr(
        not(unix),
        expect(dead_code, reason = "built only by the unix profiler discovery")
    )]
    NoProfilerForPid(u32),
    #[cfg(unix)]
    MultipleProfilers(Vec<AmbiguousProfiler>),
    ClientInit(ClientError),
}

impl PartialEq for ConnectError {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::NoProfilerForPid(lhs), Self::NoProfilerForPid(rhs)) => lhs == rhs,
            #[cfg(unix)]
            (Self::MultipleProfilers(lhs), Self::MultipleProfilers(rhs)) => lhs == rhs,
            (Self::ClientInit(lhs), Self::ClientInit(rhs)) => lhs.to_string() == rhs.to_string(),
            _ => false,
        }
    }
}

impl Eq for ConnectError {}

#[cfg(unix)]
#[derive(Debug, Clone, PartialEq, Eq)]
struct AmbiguousProfiler {
    pid: u32,
    program: String,
    uptime_secs: u64,
}

impl fmt::Display for ConnectError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::NoProfilerForPid(pid) => write!(
                f,
                "No profiler found for PID {pid} — \
                 run 'hew observe --list' to see active profilers"
            ),
            #[cfg(unix)]
            Self::MultipleProfilers(profilers) => {
                write!(
                    f,
                    "Multiple profilers running — \
                     use --pid to select one (try 'hew observe --list' for details):"
                )?;
                for profiler in profilers {
                    write!(
                        f,
                        "\n  --pid {}  {}  (up {}s)",
                        profiler.pid, profiler.program, profiler.uptime_secs,
                    )?;
                }
                Ok(())
            }
            Self::ClientInit(error) => {
                write!(f, "Failed to initialize profiler client: {error}")
            }
        }
    }
}

impl std::error::Error for ConnectError {}

/// Resolve which profiler(s) to connect to, returning the App.
fn connect(cli: &Options) -> Result<App, ConnectError> {
    if cli.demo {
        return Ok(App::new_demo());
    }

    // Explicit --addr: TCP mode (backward compatible).
    if let Some(addr) = &cli.addr {
        let mut addrs = vec![addr.clone()];
        for n in &cli.node {
            if !addrs.contains(n) {
                addrs.push(n.clone());
            }
        }
        return App::new_tcp(&addrs).map_err(ConnectError::ClientInit);
    }

    // Unix socket discovery (--pid, auto-discover).
    #[cfg(unix)]
    {
        connect_with(
            cli,
            discovery::find_by_pid,
            discovery::scan_profilers,
            unix_now_secs(),
        )
    }

    // Non-unix fallback: no discovery available, use TCP default.
    #[cfg(not(unix))]
    {
        let _ = &cli.pid; // suppress unused warning
        eprintln!(
            "note: automatic discovery is not available on this platform — \
             use --addr host:port to specify the profiler address (falling back to localhost:6060)"
        );
        App::new_tcp(&["localhost:6060".to_owned()]).map_err(ConnectError::ClientInit)
    }
}

#[cfg(unix)]
fn connect_with<FindByPid, ScanProfilers>(
    cli: &Options,
    find_by_pid: FindByPid,
    scan_profilers: ScanProfilers,
    now_secs: u64,
) -> Result<App, ConnectError>
where
    FindByPid: FnMut(u32) -> Option<discovery::DiscoveredProfiler>,
    ScanProfilers: FnOnce() -> Vec<discovery::DiscoveredProfiler>,
{
    if !cli.pid.is_empty() {
        let mut find_by_pid = find_by_pid;
        let mut profilers = Vec::new();
        for &pid in &cli.pid {
            if profilers
                .iter()
                .any(|profiler: &discovery::DiscoveredProfiler| profiler.pid == pid)
            {
                continue;
            }
            profilers.push(find_by_pid(pid).ok_or(ConnectError::NoProfilerForPid(pid))?);
        }
        if profilers.len() == 1 {
            let profiler = &profilers[0];
            return Ok(App::new_unix(&profiler.socket_path, &profiler.program));
        }
        let targets = profilers
            .into_iter()
            .map(|profiler| {
                let label = format!("{} (PID {})", profiler.program, profiler.pid);
                (profiler.socket_path, label)
            })
            .collect::<Vec<_>>();
        return Ok(App::new_unix_multi(&targets));
    }

    let profilers = scan_profilers();
    match profilers.len() {
        0 => {
            // No profilers yet — start in waiting mode, re-discover will
            // pick one up when it appears.
            Ok(App::new_waiting())
        }
        1 => {
            let profiler = &profilers[0];
            Ok(App::new_discovered(
                &profiler.socket_path,
                &profiler.program,
            ))
        }
        _ => Err(ConnectError::MultipleProfilers(
            profilers
                .into_iter()
                .map(|profiler| AmbiguousProfiler {
                    pid: profiler.pid,
                    program: profiler.program,
                    uptime_secs: now_secs.saturating_sub(profiler.started),
                })
                .collect(),
        )),
    }
}

#[cfg(unix)]
fn unix_now_secs() -> u64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map_or(0, |duration| duration.as_secs())
}

#[cfg(unix)]
fn list_profilers() {
    let profilers = discovery::scan_profilers();
    if profilers.is_empty() {
        println!("No Hew profilers discovered.");
        return;
    }

    let now = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map_or(0, |d| d.as_secs());

    println!("{:<8} {:<20} {:<12} SOCKET", "PID", "PROGRAM", "UPTIME");
    for p in &profilers {
        let uptime = now.saturating_sub(p.started);
        let uptime_str = if uptime >= 3600 {
            format!("{}h {}m", uptime / 3600, (uptime % 3600) / 60)
        } else if uptime >= 60 {
            format!("{}m {}s", uptime / 60, uptime % 60)
        } else {
            format!("{uptime}s")
        };
        println!(
            "{:<8} {:<20} {:<12} {}",
            p.pid,
            p.program,
            uptime_str,
            p.socket_path.display(),
        );
    }
}

fn run_app(cli: &Options, mut app: App) -> Result<(), Box<dyn std::error::Error>> {
    let backend = CrosstermBackend::new(io::stdout());
    let mut terminal = Terminal::new(backend)?;
    let refresh = Duration::from_millis(cli.refresh_ms);
    let mut last_refresh = Instant::now()
        .checked_sub(refresh)
        .unwrap_or_else(Instant::now);

    loop {
        // Periodic data fetch
        if last_refresh.elapsed() >= refresh {
            app.refresh();
            app.clamp_selections();
            last_refresh = Instant::now();
        }

        terminal.draw(|f| ui::draw(f, &mut app))?;

        // Poll for events with a short timeout so we can refresh
        if event::poll(Duration::from_millis(50))? {
            if let Event::Key(key) = event::read()? {
                if key.code == KeyCode::Char('c') && key.modifiers.contains(KeyModifiers::CONTROL) {
                    break;
                }
                match key.code {
                    KeyCode::Char('q') if !app.filter_active => break,
                    KeyCode::Tab => app.next_tab(),
                    KeyCode::BackTab => app.prev_tab(),
                    KeyCode::Char('r') if !app.filter_active => {
                        app.refresh();
                        app.clamp_selections();
                        last_refresh = Instant::now();
                    }
                    KeyCode::Char('?') if !app.filter_active => {
                        app.show_help = !app.show_help;
                    }
                    _ => {
                        if handle_tab_keys(&mut app, key.code) {
                            last_refresh = Instant::now();
                        }
                    }
                }
            }
        }
    }

    Ok(())
}

/// Run the actor observer, or list discovered processes without a terminal.
///
/// # Errors
/// Returns connection or terminal errors after restoring the terminal state.
pub fn run(cli: &Options) -> Result<(), Box<dyn std::error::Error>> {
    if cli.list {
        #[cfg(unix)]
        list_profilers();
        #[cfg(not(unix))]
        println!("Discovery is not available on this platform. Use --addr host:port.");
        return Ok(());
    }

    let app = connect(cli)?;
    if !std::io::stdout().is_terminal() {
        return Err(io::Error::other(
            "hew observe requires a terminal (stdout is not a TTY); \
             use the HTTP endpoints directly in non-interactive contexts.",
        )
        .into());
    }

    enable_raw_mode()?;
    let _terminal = TerminalSession;
    io::stdout().execute(EnterAlternateScreen)?;
    run_app(cli, app)
}

/// Restore the terminal on both successful exit and I/O failure.
struct TerminalSession;

impl Drop for TerminalSession {
    fn drop(&mut self) {
        let _ = disable_raw_mode();
        let _ = io::stdout().execute(LeaveAlternateScreen);
    }
}

fn handle_tab_keys(app: &mut App, key: KeyCode) -> bool {
    if app.filter_active {
        match key {
            KeyCode::Esc => {
                app.filter_active = false;
                app.filter_text.clear();
                app.clamp_selections();
            }
            KeyCode::Enter => app.filter_active = false,
            KeyCode::Backspace => {
                app.filter_text.pop();
                app.clamp_selections();
            }
            KeyCode::Char(c) => {
                app.filter_text.push(c);
                app.clamp_selections();
            }
            _ => {}
        }
        return false;
    }

    match key {
        KeyCode::Char('[') => return app.switch_active_node_prev(),
        KeyCode::Char(']') => return app.switch_active_node_next(),
        _ => {}
    }

    match app.active_tab {
        Tab::Actors => match key {
            KeyCode::Up => app.actor_list_prev(),
            KeyCode::Down => app.actor_list_next(),
            KeyCode::Char('s') => app.cycle_sort(),
            KeyCode::Char('/') => {
                app.filter_active = true;
            }
            _ => {}
        },
        Tab::Supervisors => match key {
            KeyCode::Up => app.tree_prev(),
            KeyCode::Down => app.tree_next(),
            KeyCode::Enter => app.supervisor_show_events(),
            _ => {}
        },
        Tab::Crashes => match key {
            KeyCode::Up => app.crash_list_prev(),
            KeyCode::Down => app.crash_list_next(),
            _ => {}
        },
        Tab::Messages => match key {
            KeyCode::Up => app.messages_scroll_up(),
            KeyCode::Down => app.messages_scroll_down(),
            KeyCode::Char('p' | ' ') => app.messages_toggle_pause(),
            KeyCode::Char('f') => app.messages_filter_selected(),
            KeyCode::Char('c') => app.messages_clear_filter(),
            _ => {}
        },
        Tab::Timeline => match key {
            KeyCode::Left => app.timeline_scroll_left(),
            KeyCode::Right => app.timeline_scroll_right(),
            KeyCode::Char('+') => app.timeline_zoom_in(),
            KeyCode::Char('-') => app.timeline_zoom_out(),
            KeyCode::Char('p' | ' ') => app.timeline_toggle_pause(),
            KeyCode::Char('n') => app.timeline_snap_to_now(),
            _ => {}
        },
        Tab::Overview | Tab::Cluster => {}
    }

    false
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn pid_option_accepts_repeated_and_comma_separated_values() {
        use clap::Parser as _;

        #[derive(clap::Parser)]
        struct Cli {
            #[command(flatten)]
            observe: Options,
        }

        let cli = Cli::try_parse_from(["hew-observe", "--pid", "101", "--pid", "202,303"])
            .expect("parse repeated PIDs");
        assert_eq!(cli.observe.pid, vec![101, 202, 303]);
    }

    #[cfg(unix)]
    fn cli(pid: &[u32]) -> Options {
        Options {
            addr: None,
            pid: pid.to_vec(),
            list: false,
            node: Vec::new(),
            refresh_ms: 1_000,
            demo: false,
        }
    }

    #[cfg(unix)]
    fn profiler(pid: u32, program: &str, started: u64) -> discovery::DiscoveredProfiler {
        discovery::DiscoveredProfiler {
            pid,
            socket_path: std::path::PathBuf::from(format!("/tmp/hew-profilers-{pid}.sock")),
            started,
            program: program.to_owned(),
        }
    }

    #[cfg(unix)]
    #[test]
    fn invalid_pid_returns_error() {
        let error = connect_with(&cli(&[42]), |_| None, || panic!("unexpected scan"), 1_000)
            .expect_err("invalid --pid should fail");

        assert_eq!(error, ConnectError::NoProfilerForPid(42));
        let msg = error.to_string();
        assert!(
            msg.contains("No profiler found for PID 42"),
            "unexpected message: {msg}"
        );
        assert!(msg.contains("--list"), "error should suggest --list: {msg}");
    }

    #[cfg(unix)]
    #[test]
    fn multiple_pids_connect_in_requested_order_and_deduplicate() {
        let mut looked_up = Vec::new();
        let app = connect_with(
            &cli(&[202, 101, 202]),
            |pid| {
                looked_up.push(pid);
                Some(profiler(pid, "worker", 900))
            },
            || panic!("unexpected scan"),
            1_000,
        )
        .expect("multiple PIDs should connect");

        assert_eq!(looked_up, vec![202, 101]);
        assert!(app.is_multi_node());
        assert_eq!(app.configured_node_count(), 2);
        assert_eq!(app.active_node_label(), "worker (PID 202)");
        assert_eq!(app.configured_target_label(), "worker (PID 202) +1 more");
    }

    #[cfg(unix)]
    #[test]
    fn missing_pid_in_selection_reports_that_pid() {
        let error = connect_with(
            &cli(&[101, 202]),
            |pid| (pid == 101).then(|| profiler(pid, "worker", 900)),
            || panic!("unexpected scan"),
            1_000,
        )
        .expect_err("a missing PID should fail the selection");

        assert_eq!(error, ConnectError::NoProfilerForPid(202));
    }

    #[cfg(unix)]
    #[test]
    fn ambiguous_profilers_return_error() {
        let error = connect_with(
            &cli(&[]),
            |_| panic!("unexpected pid lookup"),
            || vec![profiler(101, "alpha", 900), profiler(202, "beta", 980)],
            1_000,
        )
        .expect_err("ambiguous auto-discovery should fail");

        assert_eq!(
            error,
            ConnectError::MultipleProfilers(vec![
                AmbiguousProfiler {
                    pid: 101,
                    program: "alpha".to_owned(),
                    uptime_secs: 100,
                },
                AmbiguousProfiler {
                    pid: 202,
                    program: "beta".to_owned(),
                    uptime_secs: 20,
                },
            ]),
        );
        let msg = error.to_string();
        assert!(
            msg.contains("--pid 101") && msg.contains("alpha"),
            "should list first profiler: {msg}"
        );
        assert!(
            msg.contains("--pid 202") && msg.contains("beta"),
            "should list second profiler: {msg}"
        );
        assert!(msg.contains("--list"), "error should suggest --list: {msg}");
        assert!(msg.contains("--pid"), "error should suggest --pid: {msg}");
    }

    #[cfg(unix)]
    #[test]
    fn no_profilers_returns_waiting_app() {
        // Zero discovered profilers → observer starts in waiting mode.
        let app = connect_with(
            &cli(&[]),
            |_| panic!("unexpected pid lookup"),
            Vec::new,
            1_000,
        )
        .expect("zero profilers should not fail — waiting mode");

        assert!(
            app.is_waiting(),
            "should be in waiting mode when no profilers found"
        );
    }

    #[cfg(unix)]
    #[test]
    fn single_profiler_auto_connects() {
        let app = connect_with(
            &cli(&[]),
            |_| panic!("unexpected pid lookup"),
            || vec![profiler(55, "myapp", 900)],
            1_000,
        )
        .expect("single profiler should auto-connect");

        assert!(
            !app.is_waiting(),
            "should not be in waiting mode with one profiler"
        );
    }

    #[test]
    fn explicit_addr_deduplicates_multi_node_targets() {
        let app = connect(&Options {
            addr: Some("alpha:6060".to_owned()),
            pid: Vec::new(),
            list: false,
            node: vec![
                "beta:6061".to_owned(),
                "alpha:6060".to_owned(),
                "gamma:6062".to_owned(),
                "beta:6061".to_owned(),
            ],
            refresh_ms: 1_000,
            demo: false,
        })
        .expect("explicit --addr should connect");

        assert!(app.is_multi_node());
        assert_eq!(app.configured_node_count(), 3);
        assert_eq!(app.configured_target_label(), "alpha:6060 +2 more");
        assert_eq!(app.active_node_label(), "alpha:6060");
    }
}
