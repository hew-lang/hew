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

use std::io;
use std::time::{Duration, Instant};

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

/// Resolve which profiler(s) to connect to, returning the App.
fn connect(cli: &Cli) -> App {
    if cli.demo {
        return App::new_demo();
    }

    // Explicit --addr: TCP mode (backward compatible).
    if let Some(addr) = &cli.addr {
        let mut addrs = vec![addr.clone()];
        for n in &cli.node {
            if !addrs.contains(n) {
                addrs.push(n.clone());
            }
        }
        return App::new_tcp(&addrs);
    }

    // Unix socket discovery (--pid, auto-discover).
    #[cfg(unix)]
    {
        // Explicit --pid: look up via discovery.
        if let Some(pid) = cli.pid {
            if let Some(profiler) = discovery::find_by_pid(pid) {
                return App::new_unix(&profiler.socket_path, &profiler.program);
            }
            eprintln!("No profiler found for PID {pid}");
            std::process::exit(1);
        }

        // Auto-discovery: scan for running profilers.
        // The app will re-scan periodically if nothing is found yet.
        let profilers = discovery::scan_profilers();
        match profilers.len() {
            0 => {
                // No profilers yet — start in waiting mode, re-discover will
                // pick one up when it appears.
                App::new_waiting()
            }
            1 => {
                let p = &profilers[0];
                App::new_discovered(&p.socket_path, &p.program)
            }
            _ => {
                eprintln!("Multiple profilers discovered — specify --pid:");
                for p in &profilers {
                    let uptime = std::time::SystemTime::now()
                        .duration_since(std::time::UNIX_EPOCH)
                        .map_or(0, |d| d.as_secs())
                        .saturating_sub(p.started);
                    eprintln!("  --pid {}  {}  (up {}s)", p.pid, p.program, uptime,);
                }
                std::process::exit(1);
            }
        }
    }

    // Non-unix fallback: no discovery available, use TCP default.
    #[cfg(not(unix))]
    {
        let _ = cli.pid; // suppress unused warning
        eprintln!("No profilers discovered, falling back to localhost:6060");
        App::new_tcp(&["localhost:6060".to_owned()])
    }
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

fn run_app(cli: &Cli) -> Result<(), Box<dyn std::error::Error>> {
    let backend = CrosstermBackend::new(io::stdout());
    let mut terminal = Terminal::new(backend)?;

    let mut app = connect(cli);
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
                    _ => handle_tab_keys(&mut app, key.code),
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

    enable_raw_mode().expect("failed to enable raw mode");
    io::stdout()
        .execute(EnterAlternateScreen)
        .expect("failed to enter alternate screen");

    let result = run_app(&cli);

    // Always restore terminal, even on error
    let _ = disable_raw_mode();
    let _ = io::stdout().execute(LeaveAlternateScreen);

    if let Err(e) = result {
        eprintln!("Error: {e}");
        std::process::exit(1);
    }
}

fn handle_tab_keys(app: &mut App, key: KeyCode) {
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
        return;
    }

    match app.active_tab {
        Tab::Actors => match key {
            KeyCode::Up => app.actor_list_prev(),
            KeyCode::Down => app.actor_list_next(),
            KeyCode::Char('s') => app.cycle_sort(),
            KeyCode::Char('/') => {
                app.filter_active = true;
                app.filter_text.clear();
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
}
