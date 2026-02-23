//! `hew watch` — watch for file changes and re-run type checking automatically.

use std::path::Path;
use std::sync::mpsc;
use std::time::{Duration, Instant};

use notify::{EventKind, RecursiveMode, Watcher};

use crate::compile;

pub fn cmd_watch(args: &[String]) {
    let mut input = None;
    let mut run = false;
    let mut clear = false;
    let mut debounce_ms: u64 = 300;
    let mut options = compile::CompileOptions::default();
    let mut i = 0;

    while i < args.len() {
        match args[i].as_str() {
            "--run" => run = true,
            "--clear" => clear = true,
            "--debounce" => {
                i += 1;
                if i >= args.len() {
                    eprintln!("Error: --debounce requires an argument");
                    std::process::exit(1);
                }
                debounce_ms = args[i].parse().unwrap_or_else(|_| {
                    eprintln!("Error: --debounce requires a numeric value");
                    std::process::exit(1);
                });
            }
            "--Werror" => options.werror = true,
            "--no-typecheck" => options.no_typecheck = true,
            "--pkg-path" => {
                i += 1;
                if i >= args.len() {
                    eprintln!("Error: --pkg-path requires an argument");
                    std::process::exit(1);
                }
                options.pkg_path = Some(std::path::PathBuf::from(&args[i]));
            }
            s if s.starts_with("--pkg-path=") => {
                options.pkg_path = Some(std::path::PathBuf::from(
                    s.strip_prefix("--pkg-path=").unwrap(),
                ));
            }
            s if s.starts_with('-') => {
                eprintln!("Unknown option: {s}");
                std::process::exit(1);
            }
            _ => {
                if input.is_none() {
                    input = Some(args[i].clone());
                } else {
                    eprintln!("Error: unexpected argument '{}'", args[i]);
                    std::process::exit(1);
                }
            }
        }
        i += 1;
    }

    let Some(input) = input else {
        eprintln!("Usage: hew watch <file.hew | directory> [--run] [--clear] [--debounce <ms>]");
        std::process::exit(1);
    };

    watch_loop(&input, run, clear, debounce_ms, &options);
}

fn watch_loop(
    input: &str,
    run: bool,
    clear: bool,
    debounce_ms: u64,
    options: &compile::CompileOptions,
) {
    let path = Path::new(input);
    if !path.exists() {
        eprintln!("Error: '{}' does not exist", input);
        std::process::exit(1);
    }

    let is_dir = path.is_dir();

    // Determine the file(s) to check and the directory to watch.
    let (watch_path, initial_file, watched_file) = if is_dir {
        (path.to_path_buf(), None, None)
    } else {
        let parent = path.parent().unwrap_or(Path::new("."));
        let canonical = std::fs::canonicalize(path).unwrap_or_else(|_| path.to_path_buf());
        (
            parent.to_path_buf(),
            Some(input.to_string()),
            Some(canonical),
        )
    };

    // Initial check
    do_check(initial_file.as_deref(), input, is_dir, run, clear, options);
    eprintln!("\x1b[2m\nWatching for changes...\x1b[0m");

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

    let recursive_mode = if is_dir {
        RecursiveMode::Recursive
    } else {
        RecursiveMode::NonRecursive
    };
    watcher
        .watch(&watch_path, recursive_mode)
        .unwrap_or_else(|e| {
            eprintln!("Error: cannot watch '{}': {e}", watch_path.display());
            std::process::exit(1);
        });

    loop {
        // Block until we get an event
        let event = match rx.recv() {
            Ok(e) => e,
            Err(_) => break, // channel closed
        };

        if !is_relevant_event(&event, is_dir, watched_file.as_deref()) {
            continue;
        }

        // Trailing-edge debounce: wait until no new relevant events arrive
        // for the full debounce period.
        let mut last_event_time = Instant::now();
        loop {
            match rx.recv_timeout(Duration::from_millis(debounce_ms)) {
                Ok(ev) if is_relevant_event(&ev, is_dir, watched_file.as_deref()) => {
                    last_event_time = Instant::now();
                }
                _ => {
                    if last_event_time.elapsed() >= Duration::from_millis(debounce_ms) {
                        break;
                    }
                }
            }
        }

        // For rename events, prefer the path that exists and has a .hew extension,
        // falling back to the last path (destination) if multiple exist.
        let best_path = if matches!(
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
        } else {
            event.paths.first()
        };

        let changed_file = best_path
            .and_then(|p| p.file_name())
            .and_then(|n| n.to_str())
            .map(String::from);

        let check_file = if is_dir {
            // For directory mode, find the .hew file that changed
            best_path.and_then(|p| p.to_str()).map(String::from)
        } else {
            initial_file.clone()
        };

        if let Some(changed) = &changed_file {
            eprintln!("\n\x1b[2mChanged: {changed}\x1b[0m");
        }

        do_check(check_file.as_deref(), input, is_dir, run, clear, options);
        eprintln!("\x1b[2m\nWatching for changes...\x1b[0m");
    }
}

fn is_relevant_event(event: &notify::Event, is_dir: bool, watched_file: Option<&Path>) -> bool {
    // Only react to content modifications and file creation
    match event.kind {
        EventKind::Modify(notify::event::ModifyKind::Data(_))
        | EventKind::Create(_)
        | EventKind::Modify(notify::event::ModifyKind::Name(_)) => {}
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
) {
    if clear {
        eprint!("\x1b[2J\x1b[H");
    }

    let check_target = if is_dir {
        match file {
            Some(f) => f,
            None => {
                // On initial check of a directory, find the first .hew file
                if let Some(entry) = find_first_hew_file(original_input) {
                    // Leak is acceptable here — this only runs once at startup.
                    #[expect(
                        clippy::leaked_box,
                        reason = "one-time allocation for the initial directory scan target"
                    )]
                    Box::leak(entry.into_boxed_str())
                } else {
                    eprintln!("No .hew files found in '{original_input}'");
                    return;
                }
            }
        }
    } else {
        original_input
    };

    let now = chrono_like_timestamp();
    eprintln!("\x1b[1m[{now}] Checking {check_target}...\x1b[0m");

    let start = Instant::now();

    if run {
        // Compile to a temp binary and execute it.
        let tmp_path = match tempfile::Builder::new().prefix("hew_watch_").tempfile() {
            Ok(f) => f.into_temp_path(),
            Err(e) => {
                eprintln!("\x1b[31m✗ Cannot create temp file: {e}\x1b[0m");
                return;
            }
        };
        let tmp_bin = tmp_path.display().to_string();

        let result = compile::compile(check_target, Some(&tmp_bin), false, options);
        let elapsed = start.elapsed();

        match result {
            Ok(_) => {
                eprintln!(
                    "\x1b[32m✓ Build succeeded\x1b[0m \x1b[2m({:.0?})\x1b[0m",
                    elapsed
                );
                eprintln!("\x1b[2m--- program output ---\x1b[0m");
                let status = std::process::Command::new(&tmp_bin).status();
                match status {
                    Ok(s) if s.success() => {}
                    Ok(s) => {
                        eprintln!(
                            "\x1b[33m⚠ Process exited with code {}\x1b[0m",
                            s.code().unwrap_or(-1)
                        );
                    }
                    Err(e) => {
                        eprintln!("\x1b[31m✗ Cannot run compiled binary: {e}\x1b[0m");
                    }
                }
            }
            Err(_) => {
                eprintln!(
                    "\x1b[31m✗ Build failed\x1b[0m \x1b[2m({:.0?})\x1b[0m",
                    elapsed
                );
            }
        }
        drop(tmp_path);
    } else {
        let result = compile::compile(check_target, None, true, options);
        let elapsed = start.elapsed();

        match result {
            Ok(_) => {
                eprintln!("\x1b[32m✓ No errors\x1b[0m \x1b[2m({:.0?})\x1b[0m", elapsed);
            }
            Err(_) => {
                eprintln!(
                    "\x1b[31m✗ Check failed\x1b[0m \x1b[2m({:.0?})\x1b[0m",
                    elapsed
                );
            }
        }
    }
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
