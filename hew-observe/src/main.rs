//! TUI actor observer for debugging Hew actor systems.
//!
//! Connects to a running Hew program's profiler HTTP endpoint
//! (enabled via `HEW_PPROF=:port` or `HEW_PPROF=auto`) and displays
//! live metrics, actor lists, supervision trees, and crash logs.

mod app;
mod client;
#[cfg(unix)]
mod discovery;
mod theme;
mod ui;

use std::fmt;
use std::io;
use std::time::{Duration, Instant};
#[cfg(unix)]
use std::time::{SystemTime, UNIX_EPOCH};

use app::{App, Tab};
use clap::Parser;
use crossterm::event::{self, Event, KeyCode, KeyModifiers};
use crossterm::terminal::{
    disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen,
};
use crossterm::ExecutableCommand as _;
use ratatui::backend::CrosstermBackend;
use ratatui::Terminal;

/// TUI actor observer for Hew — inspect actors, messages, and crashes in real time.
#[derive(Parser, Debug)]
#[command(name = "hew-observe", version, about)]
struct Cli {
    /// Address of the Hew profiler endpoint (host:port).
    #[arg(long)]
    addr: Option<String>,

    /// Connect to a specific process by PID (via unix socket discovery).
    #[arg(long)]
    pid: Option<u32>,

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

#[derive(Debug, Clone, PartialEq, Eq)]
enum ConnectError {
    NoProfilerForPid(u32),
    #[cfg(unix)]
    MultipleProfilers(Vec<AmbiguousProfiler>),
}

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
                 run 'hew-observe --list' to see active profilers"
            ),
            #[cfg(unix)]
            Self::MultipleProfilers(profilers) => {
                write!(
                    f,
                    "Multiple profilers running — \
                     use --pid to select one (try 'hew-observe --list' for details):"
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
        }
    }
}

impl std::error::Error for ConnectError {}

/// Resolve which profiler(s) to connect to, returning the App.
fn connect(cli: &Cli) -> Result<App, ConnectError> {
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
        return Ok(App::new_tcp(&addrs));
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
        let _ = cli.pid; // suppress unused warning
        eprintln!(
            "note: automatic discovery is not available on this platform — \
             use --addr host:port to specify the profiler address (falling back to localhost:6060)"
        );
        Ok(App::new_tcp(&["localhost:6060".to_owned()]))
    }
}

#[cfg(unix)]
fn connect_with<FindByPid, ScanProfilers>(
    cli: &Cli,
    find_by_pid: FindByPid,
    scan_profilers: ScanProfilers,
    now_secs: u64,
) -> Result<App, ConnectError>
where
    FindByPid: FnOnce(u32) -> Option<discovery::DiscoveredProfiler>,
    ScanProfilers: FnOnce() -> Vec<discovery::DiscoveredProfiler>,
{
    if let Some(pid) = cli.pid {
        let profiler = find_by_pid(pid).ok_or(ConnectError::NoProfilerForPid(pid))?;
        return Ok(App::new_unix(&profiler.socket_path, &profiler.program));
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

fn run_app(cli: &Cli, mut app: App) -> Result<(), Box<dyn std::error::Error>> {
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

fn main() {
    let cli = Cli::parse();

    if cli.list {
        #[cfg(unix)]
        list_profilers();
        #[cfg(not(unix))]
        println!("Discovery is not available on this platform. Use --addr host:port.");
        return;
    }

    let app = match connect(&cli) {
        Ok(app) => app,
        Err(error) => {
            eprintln!("Error: {error}");
            std::process::exit(1);
        }
    };

    enable_raw_mode().expect("failed to enable raw mode");
    io::stdout()
        .execute(EnterAlternateScreen)
        .expect("failed to enter alternate screen");

    let result = run_app(&cli, app);

    // Always restore terminal, even on error
    let _ = disable_raw_mode();
    let _ = io::stdout().execute(LeaveAlternateScreen);

    if let Err(e) = result {
        eprintln!("Error: {e}");
        std::process::exit(1);
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

    #[cfg(unix)]
    fn cli(pid: Option<u32>) -> Cli {
        Cli {
            addr: None,
            pid,
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
        let error = connect_with(
            &cli(Some(42)),
            |_| None,
            || panic!("unexpected scan"),
            1_000,
        )
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
    fn ambiguous_profilers_return_error() {
        let error = connect_with(
            &cli(None),
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
            &cli(None),
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
            &cli(None),
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
        let app = connect(&Cli {
            addr: Some("alpha:6060".to_owned()),
            pid: None,
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
