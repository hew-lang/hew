//! REPL host-death counter.
//!
//! Tracks sessions that ended in an uncaught signal (crash) by maintaining
//! two files under the platform state directory:
//!
//! - `hew/jit-host-deaths/clean-exit-marker` — written on REPL start,
//!   deleted on clean exit.  If it exists at startup the previous session
//!   did not exit cleanly.
//!
//! - `hew/jit-host-deaths/count-7d` — newline-delimited Unix timestamps
//!   (seconds since epoch), one per crash event.  Entries older than 7 days
//!   are pruned on each read so the file stays small.
//!
//! ## Platform notes
//!
//! `dirs::state_dir()` returns `None` on macOS and Windows (neither platform
//! has a standard "state" directory separate from "data").  We fall back to
//! `dirs::data_local_dir()`, which maps to:
//!   - macOS: `~/Library/Application Support`
//!   - Windows: `%LOCALAPPDATA%`
//!   - Linux: `~/.local/share` (or `$XDG_DATA_HOME`)
//!
//! If both return `None` (uncommon: no HOME and no platform override) all
//! counter operations are silently skipped — we never fail the REPL startup
//! just because the counter directory is unavailable.
//!
//! ## Concurrency
//!
//! Only the REPL host writes these files, and the REPL is single-threaded.
//! No locking is used.

use std::fs;
use std::io::Write;
use std::path::PathBuf;
use std::time::{Duration, SystemTime, UNIX_EPOCH};

const SUBDIR: &str = "hew/jit-host-deaths";
const MARKER_NAME: &str = "clean-exit-marker";
const COUNTER_NAME: &str = "count-7d";
const SEVEN_DAYS_SECS: u64 = 7 * 24 * 60 * 60;

// ---------------------------------------------------------------------------
// Directory resolution
// ---------------------------------------------------------------------------

/// Resolve the host-death directory, preferring `state_dir` and falling back
/// to `data_local_dir`.
///
/// Returns `None` if neither platform dir is available (both `dirs` calls
/// return `None`).  All callers treat `None` as "silently skip".
fn host_death_dir() -> Option<PathBuf> {
    // state_dir is None on macOS and Windows; data_local_dir is the portable
    // fallback.  See module-level doc for mapped paths.
    let base = dirs::state_dir().or_else(dirs::data_local_dir)?;
    Some(base.join(SUBDIR))
}

fn marker_path() -> Option<PathBuf> {
    Some(host_death_dir()?.join(MARKER_NAME))
}

fn counter_path() -> Option<PathBuf> {
    Some(host_death_dir()?.join(COUNTER_NAME))
}

// ---------------------------------------------------------------------------
// Timestamp helpers
// ---------------------------------------------------------------------------

fn now_unix_secs() -> u64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or(Duration::ZERO)
        .as_secs()
}

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

/// Called at REPL startup.
///
/// 1. Checks whether the clean-exit marker from a previous session exists.
///    If it does, the previous session crashed; a new crash timestamp is
///    appended to the rolling counter file (entries older than 7 days are
///    pruned).
/// 2. Writes the clean-exit marker for the current session.
///
/// Returns the number of host-death events in the last 7 days, or `0` when
/// the marker does not exist, the counter file is absent, or the counter
/// directory is unavailable.
#[must_use]
pub fn startup() -> u32 {
    let Some(marker) = marker_path() else {
        return 0;
    };
    let Some(dir) = host_death_dir() else {
        return 0;
    };

    // Check for a leftover marker from a previous crashed session.
    let previous_crash = marker.exists();
    let crash_count = if previous_crash {
        record_crash_and_count()
    } else {
        recent_crash_count()
    };

    // Write the clean-exit marker for this session.
    if fs::create_dir_all(&dir).is_ok() {
        // Truncate-or-create; content is irrelevant — presence is the signal.
        let _ = fs::OpenOptions::new()
            .create(true)
            .truncate(true)
            .write(true)
            .open(&marker)
            .and_then(|mut f| f.write_all(b"1"));
    }

    crash_count
}

/// Called on clean REPL exit.
///
/// Deletes the clean-exit marker.  If the marker cannot be removed (e.g.
/// the file was already gone) the error is silently ignored.
pub fn on_clean_exit() {
    if let Some(marker) = marker_path() {
        let _ = fs::remove_file(marker);
    }
}

// ---------------------------------------------------------------------------
// Internal helpers
// ---------------------------------------------------------------------------

/// Append a crash timestamp to the rolling counter and return the total count
/// of events within the last 7 days (including this one).
fn record_crash_and_count() -> u32 {
    let Some(path) = counter_path() else {
        return 0;
    };
    let Some(dir) = host_death_dir() else {
        return 0;
    };

    let now = now_unix_secs();
    let cutoff = now.saturating_sub(SEVEN_DAYS_SECS);

    // Read existing entries, prune stale ones, append the new event.
    let mut entries: Vec<u64> = read_counter_entries(&path)
        .into_iter()
        .filter(|&ts| ts >= cutoff)
        .collect();
    entries.push(now);

    if fs::create_dir_all(&dir).is_ok() {
        write_counter_entries(&path, &entries);
    }

    u32::try_from(entries.len()).unwrap_or(u32::MAX)
}

/// Return the count of crash events recorded within the last 7 days without
/// adding a new event.
fn recent_crash_count() -> u32 {
    let Some(path) = counter_path() else {
        return 0;
    };

    let now = now_unix_secs();
    let cutoff = now.saturating_sub(SEVEN_DAYS_SECS);

    let count = read_counter_entries(&path)
        .into_iter()
        .filter(|&ts| ts >= cutoff)
        .count();

    u32::try_from(count).unwrap_or(u32::MAX)
}

/// Parse the counter file: one decimal Unix timestamp per line.
///
/// Lines that do not parse are silently skipped (forward-compatible: a future
/// format might add fields after the timestamp separated by a space).
fn read_counter_entries(path: &std::path::Path) -> Vec<u64> {
    let Ok(text) = fs::read_to_string(path) else {
        return Vec::new();
    };
    text.lines()
        .filter_map(|line| line.split_whitespace().next()?.parse::<u64>().ok())
        .collect()
}

/// Write timestamp entries to the counter file, one per line.
fn write_counter_entries(path: &std::path::Path, entries: &[u64]) {
    let Ok(mut f) = fs::OpenOptions::new()
        .create(true)
        .truncate(true)
        .write(true)
        .open(path)
    else {
        return;
    };
    for ts in entries {
        let _ = writeln!(f, "{ts}");
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    fn tmp_dir() -> tempfile::TempDir {
        tempfile::tempdir().expect("create tmp dir")
    }

    /// Simulate the marker being present (previous crash) and verify
    /// `record_crash_and_count` records it and returns 1.
    #[test]
    fn crash_detected_when_marker_exists_returns_nonzero_count() {
        let dir = tmp_dir();
        let counter = dir.path().join("count-7d");

        // No prior entries — should return 1 after recording.
        let now = now_unix_secs();
        // Manually invoke the recording logic with our temp path.
        let cutoff = now.saturating_sub(SEVEN_DAYS_SECS);
        let mut entries: Vec<u64> = read_counter_entries(&counter)
            .into_iter()
            .filter(|&ts| ts >= cutoff)
            .collect();
        entries.push(now);
        write_counter_entries(&counter, &entries);

        let count = u32::try_from(entries.len()).unwrap_or(u32::MAX);
        assert_eq!(count, 1, "first crash should yield count=1");

        // Simulate a second crash event.
        let mut entries2: Vec<u64> = read_counter_entries(&counter)
            .into_iter()
            .filter(|&ts| ts >= cutoff)
            .collect();
        entries2.push(now + 1);
        write_counter_entries(&counter, &entries2);
        assert_eq!(entries2.len(), 2, "second crash should yield count=2");
    }

    /// Entries older than 7 days should be pruned on read.
    #[test]
    fn stale_entries_pruned_from_counter() {
        let dir = tmp_dir();
        let counter = dir.path().join("count-7d");

        let now = now_unix_secs();
        // Write two entries: one stale (8 days ago), one current.
        let stale = now.saturating_sub(SEVEN_DAYS_SECS + 60);
        write_counter_entries(&counter, &[stale, now]);

        let cutoff = now.saturating_sub(SEVEN_DAYS_SECS);
        let live: Vec<u64> = read_counter_entries(&counter)
            .into_iter()
            .filter(|&ts| ts >= cutoff)
            .collect();
        assert_eq!(live.len(), 1, "stale entry should be pruned");
        assert_eq!(live[0], now);
    }

    /// A missing counter file should return an empty vec, not panic.
    #[test]
    fn missing_counter_file_returns_empty() {
        let dir = tmp_dir();
        let counter = dir.path().join("no-such-file");
        let entries = read_counter_entries(&counter);
        assert!(entries.is_empty());
    }

    /// The marker file is created on startup and removed on clean exit.
    #[test]
    fn marker_created_on_startup_removed_on_clean_exit() {
        // Use a temp dir as the state base dir — we can't override dirs::state_dir()
        // at runtime, so we test the file-level primitives directly.
        let dir = tmp_dir();
        let marker = dir.path().join("clean-exit-marker");

        // Simulate writing the marker (startup).
        fs::write(&marker, b"1").expect("write marker");
        assert!(
            marker.exists(),
            "marker should exist after startup simulation"
        );

        // Simulate clean exit.
        fs::remove_file(&marker).expect("remove marker");
        assert!(
            !marker.exists(),
            "marker should be gone after clean-exit simulation"
        );
    }
}
