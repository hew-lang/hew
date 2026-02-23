//! TUI actor observer for debugging Hew actor systems.
//!
//! Connects to a running Hew program's profiler HTTP endpoint
//! (enabled via `HEW_PPROF=:port`) and displays live metrics,
//! actor lists, supervision trees, and crash logs.

mod app;
mod client;
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
    /// Address of the Hew profiler endpoint (host:port)
    #[arg(long, default_value = "localhost:6060")]
    addr: String,

    /// Refresh interval in milliseconds
    #[arg(long, default_value_t = 1000)]
    refresh_ms: u64,

    /// Start in demo mode with sample data (no HTTP connection)
    #[arg(long)]
    demo: bool,
}

fn run_app(cli: &Cli) -> Result<(), Box<dyn std::error::Error>> {
    let backend = CrosstermBackend::new(io::stdout());
    let mut terminal = Terminal::new(backend)?;

    let base_url = format!("http://{}", cli.addr);
    let mut app = App::new(&base_url, cli.demo);
    let refresh = Duration::from_millis(cli.refresh_ms);
    let mut last_refresh = Instant::now()
        .checked_sub(refresh)
        .unwrap_or_else(Instant::now);

    loop {
        // Periodic data fetch
        if last_refresh.elapsed() >= refresh {
            app.refresh();
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
            }
            KeyCode::Enter => app.filter_active = false,
            KeyCode::Backspace => {
                app.filter_text.pop();
            }
            KeyCode::Char(c) => app.filter_text.push(c),
            _ => {}
        }
        return;
    }

    match app.active_tab {
        Tab::Overview => {}
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
    }
}
