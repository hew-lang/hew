//! `hew watch` — watch for file changes and re-run type checking automatically.

use std::path::{Path, PathBuf};
use std::sync::mpsc;
use std::time::{Duration, Instant};

use notify::{EventKind, RecursiveMode, Watcher};

use crate::{compile, diagnostic};

const RED: &str = "\x1b[31m";
const GREEN: &str = "\x1b[32m";
const YELLOW: &str = "\x1b[33m";
const BOLD: &str = "\x1b[1m";
const DIM: &str = "\x1b[2m";
const RESET: &str = "\x1b[0m";
const CLEAR_SCREEN: &str = "\x1b[2J\x1b[H";

struct WatchPalette {
    red: &'static str,
    green: &'static str,
    yellow: &'static str,
    bold: &'static str,
    dim: &'static str,
    reset: &'static str,
    clear_screen: &'static str,
}

fn watch_palette_for(use_ansi: bool) -> WatchPalette {
    if use_ansi {
        WatchPalette {
            red: RED,
            green: GREEN,
            yellow: YELLOW,
            bold: BOLD,
            dim: DIM,
            reset: RESET,
            clear_screen: CLEAR_SCREEN,
        }
    } else {
        WatchPalette {
            red: "",
            green: "",
            yellow: "",
            bold: "",
            dim: "",
            reset: "",
            clear_screen: "",
        }
    }
}

fn watch_palette() -> WatchPalette {
    watch_palette_for(diagnostic::use_ansi_diagnostics())
}

struct WatchTarget {
    is_dir: bool,
    watch_path: PathBuf,
    initial_file: Option<String>,
    watched_file: Option<PathBuf>,
}

pub fn cmd_watch(args: &crate::args::WatchArgs) {
    let input = args.input.display().to_string();
    let options = args.to_compile_options();
    watch_loop(&input, args.run, args.clear, args.debounce, &options);
}

fn watch_loop(
    input: &str,
    run: bool,
    clear: bool,
    debounce_ms: u64,
    options: &compile::CompileOptions,
) {
    let palette = watch_palette();
    let target = resolve_watch_target(input);

    // Initial check
    do_check(
        target.initial_file.as_deref(),
        input,
        target.is_dir,
        run,
        clear,
        options,
        &palette,
    );
    emit_watch_ready(&palette);

    // Set up file watcher
    let (tx, rx) = mpsc::channel();
    let mut watcher = notify::recommended_watcher(move |res: notify::Result<notify::Event>| {
        if let Ok(event) = res {
            let _ = tx.send(event);
        }
    })
    .unwrap_or_else(|e| {
        eprintln!("Error: cannot create file watcher: {e}");
        std::process::exit(1);
    });

    let recursive_mode = if target.is_dir {
        RecursiveMode::Recursive
    } else {
        RecursiveMode::NonRecursive
    };
    watcher
        .watch(&target.watch_path, recursive_mode)
        .unwrap_or_else(|e| {
            eprintln!("Error: cannot watch '{}': {e}", target.watch_path.display());
            std::process::exit(1);
        });

    while let Ok(event) = rx.recv() {
        if !is_relevant_event(&event, target.is_dir, target.watched_file.as_deref()) {
            continue;
        }

        debounce_relevant_events(
            &rx,
            debounce_ms,
            target.is_dir,
            target.watched_file.as_deref(),
        );

        let best_path = best_event_path(&event);
        let changed_file = changed_file_name(best_path);
        let check_file =
            check_file_for_event(target.is_dir, target.initial_file.as_ref(), best_path);

        if let Some(changed) = &changed_file {
            emit_changed_file(changed, &palette);
        }

        do_check(
            check_file.as_deref(),
            input,
            target.is_dir,
            run,
            clear,
            options,
            &palette,
        );
        emit_watch_ready(&palette);
    }
}

fn resolve_watch_target(input: &str) -> WatchTarget {
    let path = Path::new(input);
    if !path.exists() {
        eprintln!("Error: '{input}' does not exist");
        std::process::exit(1);
    }

    if path.is_dir() {
        WatchTarget {
            is_dir: true,
            watch_path: path.to_path_buf(),
            initial_file: None,
            watched_file: None,
        }
    } else {
        let parent = path.parent().unwrap_or(Path::new("."));
        let canonical = std::fs::canonicalize(path).unwrap_or_else(|_| path.to_path_buf());
        WatchTarget {
            is_dir: false,
            watch_path: parent.to_path_buf(),
            initial_file: Some(input.to_string()),
            watched_file: Some(canonical),
        }
    }
}

fn emit_watch_ready(palette: &WatchPalette) {
    eprintln!(
        "{dim}\nWatching for changes...{reset}",
        dim = palette.dim,
        reset = palette.reset
    );
}

fn emit_changed_file(changed: &str, palette: &WatchPalette) {
    eprintln!(
        "\n{dim}Changed: {changed}{reset}",
        dim = palette.dim,
        reset = palette.reset
    );
}

fn debounce_relevant_events(
    rx: &mpsc::Receiver<notify::Event>,
    debounce_ms: u64,
    is_dir: bool,
    watched_file: Option<&Path>,
) {
    let mut last_event_time = Instant::now();
    loop {
        match rx.recv_timeout(Duration::from_millis(debounce_ms)) {
            Ok(ev) if is_relevant_event(&ev, is_dir, watched_file) => {
                last_event_time = Instant::now();
            }
            _ => {
                if last_event_time.elapsed() >= Duration::from_millis(debounce_ms) {
                    break;
                }
            }
        }
    }
}

fn best_event_path(event: &notify::Event) -> Option<&Path> {
    if matches!(
        event.kind,
        EventKind::Modify(notify::event::ModifyKind::Name(_))
    ) && event.paths.len() > 1
    {
        event
            .paths
            .iter()
            .rev()
            .find(|p| {
                p.exists()
                    && p.extension()
                        .is_some_and(|ext| ext.eq_ignore_ascii_case("hew"))
            })
            .or_else(|| event.paths.iter().rev().find(|p| p.exists()))
            .or(event.paths.last())
            .map(PathBuf::as_path)
    } else {
        event.paths.first().map(PathBuf::as_path)
    }
}

fn changed_file_name(path: Option<&Path>) -> Option<String> {
    path.and_then(|p| p.file_name())
        .and_then(|n| n.to_str())
        .map(String::from)
}

fn check_file_for_event(
    is_dir: bool,
    initial_file: Option<&String>,
    best_path: Option<&Path>,
) -> Option<String> {
    if is_dir {
        best_path.and_then(|p| p.to_str()).map(String::from)
    } else {
        initial_file.cloned()
    }
}

fn is_relevant_event(event: &notify::Event, is_dir: bool, watched_file: Option<&Path>) -> bool {
    // Only react to content modifications and file creation
    match event.kind {
        EventKind::Modify(
            notify::event::ModifyKind::Data(_) | notify::event::ModifyKind::Name(_),
        )
        | EventKind::Create(_) => {}
        _ => return false,
    }

    // Ignore changes in target/ and .git/ directories
    let in_ignored_dir = event.paths.iter().all(|p| {
        p.components().any(|c| {
            let s = c.as_os_str();
            s == "target" || s == ".git"
        })
    });
    if in_ignored_dir {
        return false;
    }

    if is_dir {
        // In directory mode, only react to .hew file changes
        let has_hew_file = event.paths.iter().any(|p| {
            p.extension()
                .is_some_and(|ext| ext.eq_ignore_ascii_case("hew"))
        });
        if !has_hew_file {
            return false;
        }
    } else if let Some(target) = watched_file {
        // In single-file mode, only react to changes to the watched file
        let matches_target = event.paths.iter().any(|p| p == target);
        if !matches_target {
            return false;
        }
    }

    true
}

fn do_check(
    file: Option<&str>,
    original_input: &str,
    is_dir: bool,
    run: bool,
    clear: bool,
    options: &compile::CompileOptions,
    palette: &WatchPalette,
) {
    if clear {
        eprint!("{}", palette.clear_screen);
    }

    let Some(check_target) = resolve_check_target(file, original_input, is_dir) else {
        return;
    };

    let start = Instant::now();
    emit_check_start(check_target, palette);

    if run {
        run_check_and_program(check_target, options, palette, start);
    } else {
        run_check_only(check_target, options, palette, start);
    }
}

fn resolve_check_target<'a>(
    file: Option<&'a str>,
    original_input: &'a str,
    is_dir: bool,
) -> Option<&'a str> {
    if !is_dir {
        return Some(original_input);
    }

    match file {
        Some(f) => Some(f),
        None => find_first_hew_file(original_input)
            .map(|entry| Box::leak(entry.into_boxed_str()) as &str)
            .or_else(|| {
                eprintln!("No .hew files found in '{original_input}'");
                None
            }),
    }
}

fn emit_check_start(check_target: &str, palette: &WatchPalette) {
    let now = chrono_like_timestamp();
    eprintln!(
        "{bold}[{now}] Checking {check_target}...{reset}",
        bold = palette.bold,
        reset = palette.reset
    );
}

fn run_check_and_program(
    check_target: &str,
    options: &compile::CompileOptions,
    palette: &WatchPalette,
    start: Instant,
) {
    let Some((tmp_path, tmp_bin)) = create_temp_binary(palette) else {
        return;
    };

    let result = compile::compile(check_target, Some(&tmp_bin), false, options);
    let elapsed = start.elapsed();

    match result {
        Ok(_) => {
            emit_watch_status("✓ Build succeeded", palette.green, elapsed, palette);
            eprintln!(
                "{dim}--- program output ---{reset}",
                dim = palette.dim,
                reset = palette.reset
            );
            run_compiled_binary(&tmp_bin, palette);
        }
        Err(_) => emit_watch_status("✗ Build failed", palette.red, elapsed, palette),
    }

    drop(tmp_path);
}

fn create_temp_binary(palette: &WatchPalette) -> Option<(tempfile::TempPath, String)> {
    tempfile::Builder::new()
        .prefix("hew_watch_")
        .suffix(crate::platform::exe_suffix())
        .tempfile()
        .map(|file| {
            let tmp_path = file.into_temp_path();
            let tmp_bin = tmp_path.display().to_string();
            (tmp_path, tmp_bin)
        })
        .map_err(|e| {
            eprintln!(
                "{red}✗ Cannot create temp file: {e}{reset}",
                red = palette.red,
                reset = palette.reset
            );
        })
        .ok()
}

fn run_compiled_binary(tmp_bin: &str, palette: &WatchPalette) {
    match std::process::Command::new(tmp_bin).status() {
        Ok(s) if s.success() => {}
        Ok(s) => {
            eprintln!(
                "{yellow}⚠ Process exited with code {}{reset}",
                s.code().unwrap_or(-1),
                yellow = palette.yellow,
                reset = palette.reset
            );
        }
        Err(e) => {
            eprintln!(
                "{red}✗ Cannot run compiled binary: {e}{reset}",
                red = palette.red,
                reset = palette.reset
            );
        }
    }
}

fn run_check_only(
    check_target: &str,
    options: &compile::CompileOptions,
    palette: &WatchPalette,
    start: Instant,
) {
    let result = compile::compile(check_target, None, true, options);
    let elapsed = start.elapsed();

    match result {
        Ok(_) => emit_watch_status("✓ No errors", palette.green, elapsed, palette),
        Err(_) => emit_watch_status("✗ Check failed", palette.red, elapsed, palette),
    }
}

fn emit_watch_status(status: &str, color: &str, elapsed: Duration, palette: &WatchPalette) {
    eprintln!(
        "{color}{status}{reset} {dim}({elapsed:.0?}){reset}",
        color = color,
        reset = palette.reset,
        dim = palette.dim
    );
}

fn find_first_hew_file(dir: &str) -> Option<String> {
    let path = Path::new(dir);
    for entry in std::fs::read_dir(path).ok()?.flatten() {
        let p = entry.path();
        if p.is_file()
            && p.extension()
                .is_some_and(|ext| ext.eq_ignore_ascii_case("hew"))
        {
            return p.to_str().map(String::from);
        }
    }
    None
}

fn chrono_like_timestamp() -> String {
    use std::time::SystemTime;

    let now = SystemTime::now()
        .duration_since(SystemTime::UNIX_EPOCH)
        .unwrap_or_default();
    let secs = now.as_secs();
    // Simple HH:MM:SS from UTC seconds
    let hours = (secs % 86400) / 3600;
    let minutes = (secs % 3600) / 60;
    let seconds = secs % 60;
    format!("{hours:02}:{minutes:02}:{seconds:02}")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn plain_watch_palette_removes_escape_sequences() {
        let palette = watch_palette_for(false);

        assert!(palette.clear_screen.is_empty());

        let checking = format!(
            "{bold}[12:34:56] Checking main.hew...{reset}",
            bold = palette.bold,
            reset = palette.reset
        );
        assert_eq!(checking, "[12:34:56] Checking main.hew...");
        assert!(!checking.contains("\u{1b}["));

        let changed = format!(
            "\n{dim}Changed: main.hew{reset}",
            dim = palette.dim,
            reset = palette.reset
        );
        assert_eq!(changed, "\nChanged: main.hew");
        assert!(!changed.contains("\u{1b}["));
    }

    #[test]
    fn ansi_watch_palette_keeps_terminal_sequences() {
        let palette = watch_palette_for(true);

        assert_eq!(palette.clear_screen, CLEAR_SCREEN);
        assert_eq!(palette.bold, BOLD);
        assert_eq!(palette.dim, DIM);
        assert_eq!(palette.reset, RESET);
    }
}
