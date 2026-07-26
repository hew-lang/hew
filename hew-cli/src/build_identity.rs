//! Fail-closed pairing check between the compiler driver and `libhew.a`.
//!
//! `cargo build -p hew-cli` produces the driver alone. The link step for a
//! compiled Hew program additionally needs `hew-lib`'s staticlib next to that
//! driver (`libhew.a` on Unix, `hew.lib` on Windows), and nothing in Cargo ties
//! their lifetimes together. A fresh driver beside a month-old archive used to
//! fail as a wall of undefined `hew_*` symbols — `hew_hashmap_get_clone_layout`,
//! `hew_stream_last_error_kind`, `hew_runtime_cleanup_after_main` and friends —
//! which reads like a compiler bug and is not.
//!
//! Both halves now carry a digest of the `hew-lib` / `hew-runtime` / `hew-std`
//! source set (see the `hew-build-identity` crate). This module reads the
//! archive's copy back at link time and refuses to proceed unless it matches
//! the driver's. The check is deliberately at the point of use, so it also
//! covers the paths no Makefile edge can reach: a direct `cargo build`, an IDE
//! build, a shared `CARGO_TARGET_DIR` carrying an older archive, or an archive
//! copied in by hand.
//!
//! Fail closed means fail closed: a missing, truncated or unreadable stamp is
//! refused, never accepted as a match.
//!
//! # Cost
//!
//! Establishing the answer means reading the whole archive: refusing an archive
//! that carries two different identities requires knowing whether a second one
//! exists, so there is no early exit. On a 160 MB debug `libhew.a` that is
//! roughly a quarter of a second, and the vertical slice and the ratchets link
//! thousands of programs.
//!
//! The answer, though, is a property of the archive *file*. It cannot change
//! between two links unless the file changes. So the scan is memoized on a
//! fingerprint of the file itself (see [`ArchiveFingerprint`]) at two levels:
//! an in-process map, so one driver process linking N programs scans once, and
//! a marker file beside the archive, so the next `hew` process does not scan at
//! all. Both are keyed strictly enough that a rebuilt or replaced archive
//! misses and is scanned again, and both only ever record a *success* — the
//! memo can assert "this exact file matched this exact driver" and nothing
//! else. Anything the memo cannot vouch for falls through to the full scan.

use std::collections::BTreeSet;
use std::collections::HashMap;
use std::io::{Read, Write};
use std::path::{Path, PathBuf};
use std::sync::{Mutex, OnceLock};

use hew_build_identity::{digest_from_stamp, STAMP_LEN, STAMP_PREFIX};

/// Identity of the runtime + stdlib sources this driver was built from.
pub(crate) const DRIVER_IDENTITY: &str = env!("HEW_BUILD_IDENTITY");

/// Bytes read per filesystem round trip while scanning for the stamp.
const CHUNK_BYTES: usize = 1 << 20;

/// First line of the marker file, and the only version of it this driver reads.
///
/// Bump this whenever the scan's verdict for a fixed archive could change —
/// a new stamp format, a change to what counts as a conflict — so markers
/// written by the old rules are misses rather than silently honoured answers.
const MARKER_VERSION: &str = "HEW_IDENTITY_MEMO_V1";

/// Why an archive's build identity could not be established.
#[derive(Debug)]
enum IdentityReadError {
    /// The archive could not be opened or read.
    Unreadable(std::io::Error),
    /// The archive carries no usable stamp.
    Missing,
    /// The archive carries more than one distinct identity.
    Conflicting(Vec<String>),
}

/// Refuses to link `archive` unless it was built from this driver's sources.
///
/// Returns the caller-facing error text on refusal so the resolution path can
/// surface it exactly like any other link failure.
///
/// The scan runs at most once per distinct archive file. A memo hit is only
/// ever "this file, byte-for-byte as it is right now, was scanned and matched
/// *this* driver"; every other outcome — including a refusal — is recomputed.
/// Refusals are deliberately not cached: they are terminal and rare, so their
/// cost does not matter, and not caching them removes any way for the memo to
/// manufacture a verdict it did not observe.
pub(crate) fn verify_archive(archive: &Path) -> Result<(), String> {
    let before = ArchiveFingerprint::of(archive);

    if let Some(fingerprint) = &before {
        if in_process_memo_confirms(fingerprint) || marker_confirms(archive, fingerprint) {
            remember_in_process(fingerprint);
            return Ok(());
        }
    }

    let outcome = verdict(archive, read_archive_identity(archive));

    if outcome.is_ok() {
        if let Some(fingerprint) = before {
            // Only publish an answer for a file that held still for the whole
            // scan. If the archive was rewritten underneath the read, the
            // bytes the scan saw are not the bytes now on disk, so there is
            // nothing to memoize — the next link scans again.
            if ArchiveFingerprint::of(archive).as_ref() == Some(&fingerprint) {
                remember_in_process(&fingerprint);
                write_marker(archive, &fingerprint);
            }
        }
    }

    outcome
}

/// The filesystem facts that pin down *which bytes* live at a path.
///
/// This is the memo key, so what it must guarantee is narrow and absolute: if
/// the contents at `path` differ from the contents that were scanned, at least
/// one field differs.
///
/// * `len` and the modification time catch every ordinary rebuild — Cargo
///   writes a new archive, `cp` writes a new archive, both move the mtime.
/// * `dev` and `ino` catch replacement by rename or copy-onto-a-new-file, which
///   is how Cargo and every install step actually put an archive in place: the
///   path is the same and the file behind it is not.
/// * `ctime` catches the one case the others could in principle be talked out
///   of — an in-place rewrite that restores the old length and back-dates the
///   mtime with `touch`. `ctime` is stamped by the kernel on any change to the
///   inode or its contents and userspace cannot move it backwards, so a forged
///   mtime does not buy a memo hit.
///
/// Off Unix there is no `ctime`; creation time stands in for it. That is a
/// weaker key against deliberate back-dating and a fully adequate one against
/// the failure this whole check exists for, which is an ordinary stale build
/// artefact.
#[derive(Clone, PartialEq, Eq, Debug)]
struct ArchiveFingerprint {
    /// Absolute path the fingerprint was taken from.
    path: String,
    /// The filesystem facts above, rendered as one comparable line.
    stat: String,
}

impl ArchiveFingerprint {
    /// Fingerprints the file at `archive`, or `None` if it cannot be pinned
    /// down.
    ///
    /// Every `None` here costs a rescan and nothing else, so anything even
    /// slightly ambiguous — an unstattable file, a path that does not survive a
    /// round trip through the marker's line-oriented format — returns `None`
    /// rather than a key that might collide.
    fn of(archive: &Path) -> Option<Self> {
        let metadata = std::fs::metadata(archive).ok()?;
        if !metadata.is_file() {
            return None;
        }

        let path = std::path::absolute(archive).ok()?.to_str()?.to_owned();
        if path.contains('\n') {
            return None;
        }

        Some(Self {
            path,
            stat: stat_key(&metadata)?,
        })
    }
}

#[cfg(unix)]
#[allow(
    clippy::unnecessary_wraps,
    reason = "shares one signature with the non-Unix arm, which genuinely has no key when the \
              platform cannot report a modification or creation time"
)]
fn stat_key(metadata: &std::fs::Metadata) -> Option<String> {
    use std::os::unix::fs::MetadataExt;

    Some(format!(
        "len={} mtime={}.{:09} ctime={}.{:09} dev={} ino={}",
        metadata.len(),
        metadata.mtime(),
        metadata.mtime_nsec(),
        metadata.ctime(),
        metadata.ctime_nsec(),
        metadata.dev(),
        metadata.ino(),
    ))
}

#[cfg(not(unix))]
fn stat_key(metadata: &std::fs::Metadata) -> Option<String> {
    let modified = metadata.modified().ok()?;
    let created = metadata.created().ok()?;
    Some(format!(
        "len={} mtime={modified:?} created={created:?}",
        metadata.len(),
    ))
}

/// Archives this process has already scanned and matched, keyed by path.
///
/// The stored fingerprint is checked in full on every hit, so an entry for a
/// path that has since been rebuilt does not answer for the new file.
fn in_process_memo() -> &'static Mutex<HashMap<String, String>> {
    static MEMO: OnceLock<Mutex<HashMap<String, String>>> = OnceLock::new();
    MEMO.get_or_init(|| Mutex::new(HashMap::new()))
}

fn in_process_memo_confirms(fingerprint: &ArchiveFingerprint) -> bool {
    // A poisoned lock means some other thread panicked mid-update, so the map
    // is not something to take an answer from: rescan.
    in_process_memo()
        .lock()
        .is_ok_and(|memo| memo.get(&fingerprint.path) == Some(&fingerprint.stat))
}

fn remember_in_process(fingerprint: &ArchiveFingerprint) {
    if let Ok(mut memo) = in_process_memo().lock() {
        memo.insert(fingerprint.path.clone(), fingerprint.stat.clone());
    }
}

/// Where the cross-process marker for `archive` lives.
///
/// Beside the archive, because that is the one place whose lifetime is tied to
/// it: a target directory that gets wiped, or a checkout that gets deleted,
/// takes the marker with it. A marker in a user-wide cache would outlive the
/// archive it describes, which is exactly the property that must not hold.
fn marker_path(archive: &Path) -> Option<PathBuf> {
    let name = archive.file_name()?.to_str()?;
    Some(archive.with_file_name(format!(".{name}.hew-identity")))
}

/// Reports whether a marker vouches for exactly this file and this driver.
///
/// Every line must match. The version pins the scan rules, the path and stat
/// pin the file, and the digest pins the driver — a driver rebuilt from
/// different runtime sources carries a different [`DRIVER_IDENTITY`] and so
/// finds no marker it can use, even though the archive never moved.
fn marker_confirms(archive: &Path, fingerprint: &ArchiveFingerprint) -> bool {
    let Some(path) = marker_path(archive) else {
        return false;
    };
    let Ok(text) = std::fs::read_to_string(path) else {
        return false;
    };

    let mut lines = text.lines();
    lines.next() == Some(MARKER_VERSION)
        && lines.next() == Some(fingerprint.path.as_str())
        && lines.next() == Some(fingerprint.stat.as_str())
        && lines.next() == Some(DRIVER_IDENTITY)
        && lines.next().is_none()
}

/// Records a verified archive for the next process, best effort.
///
/// Every failure path here is silent and harmless: no marker means the next
/// link scans, which is the behaviour this whole change is optimising away and
/// never the behaviour it is weakening. Read-only target directories and
/// concurrent linkers therefore need no special handling beyond not crashing.
///
/// The write is a create-then-rename so a reader never observes a half-written
/// marker, and the temporary name carries the pid so two linkers racing on the
/// same archive cannot truncate each other's file.
fn write_marker(archive: &Path, fingerprint: &ArchiveFingerprint) {
    let Some(path) = marker_path(archive) else {
        return;
    };
    let temporary = path.with_extension(format!("tmp{}", std::process::id()));

    let body = format!(
        "{MARKER_VERSION}\n{}\n{}\n{DRIVER_IDENTITY}\n",
        fingerprint.path, fingerprint.stat
    );

    let written = std::fs::File::create(&temporary).and_then(|mut file| {
        file.write_all(body.as_bytes())?;
        file.sync_all()
    });

    if written.is_err() || std::fs::rename(&temporary, &path).is_err() {
        let _ = std::fs::remove_file(&temporary);
    }
}

/// Turns a read result into the caller-facing verdict for `archive`.
fn verdict(archive: &Path, read: Result<String, IdentityReadError>) -> Result<(), String> {
    match read {
        Ok(found) if found == DRIVER_IDENTITY => Ok(()),
        Ok(found) => Err(refusal(
            archive,
            &found,
            "The driver and the archive were built from different hew-lib / hew-runtime /\n\
             hew-std sources.",
        )),
        Err(IdentityReadError::Missing) => Err(refusal(
            archive,
            &format!("missing — no `{STAMP_PREFIX}` stamp"),
            "The archive carries no build identity at all, so it cannot be shown to match\n\
             this driver. It predates the stamp or was not produced by this workspace.",
        )),
        Err(IdentityReadError::Conflicting(found)) => Err(refusal(
            archive,
            &format!(
                "ambiguous — {} different identities: {}",
                found.len(),
                found.join(", ")
            ),
            "The archive carries more than one build identity, so its members were not all\n\
             compiled from the same sources — a partial rebuild left stale objects behind, or\n\
             archives from two checkouts were merged. Which identity counts as the archive's\n\
             would come down to which stamp the scan happened to reach first, so no answer is\n\
             trustworthy and this is refused outright, including when one of them matches.",
        )),
        Err(IdentityReadError::Unreadable(error)) => Err(refusal(
            archive,
            &format!("unreadable — {error}"),
            "The archive's build identity could not be read, so it cannot be shown to match\n\
             this driver.",
        )),
    }
}

fn read_archive_identity(archive: &Path) -> Result<String, IdentityReadError> {
    let file = std::fs::File::open(archive).map_err(IdentityReadError::Unreadable)?;
    scan_reader(std::io::BufReader::new(file))
}

/// Streams `reader` collecting every distinct well-formed stamp it carries.
///
/// The whole archive is read, not just the run-up to the first stamp. Stopping
/// at the first one made the verdict depend on layout: an archive holding a
/// stale member and a fresh one passed when the fresh stamp happened to come
/// first and was refused when it came second, for the same set of objects. An
/// archive has one identity or it has no usable identity at all.
///
/// The archive is large (a debug `libhew.a` runs to hundreds of megabytes), so
/// this never loads the whole file: it keeps one chunk plus a `STAMP_LEN - 1`
/// byte tail, which is exactly enough for a stamp straddling a chunk boundary.
/// Re-scanning that tail can rediscover a stamp already seen; identical
/// findings collapse, so only genuinely different identities conflict.
fn scan_reader<R: Read>(mut reader: R) -> Result<String, IdentityReadError> {
    let overlap = STAMP_LEN - 1;
    let mut chunk = vec![0u8; CHUNK_BYTES];
    let mut window: Vec<u8> = Vec::with_capacity(CHUNK_BYTES + overlap);
    let mut found = BTreeSet::new();

    loop {
        let read = fill(&mut reader, &mut chunk).map_err(IdentityReadError::Unreadable)?;
        if read == 0 {
            break;
        }
        window.extend_from_slice(&chunk[..read]);
        collect_stamps(&window, &mut found);
        if window.len() > overlap {
            let stale = window.len() - overlap;
            window.drain(..stale);
        }
    }

    let mut found: Vec<String> = found.into_iter().collect();
    match found.len() {
        0 => Err(IdentityReadError::Missing),
        1 => Ok(found.remove(0)),
        _ => Err(IdentityReadError::Conflicting(found)),
    }
}

/// Reads until `buffer` is full or the reader is exhausted.
fn fill<R: Read>(reader: &mut R, buffer: &mut [u8]) -> std::io::Result<usize> {
    let mut filled = 0;
    while filled < buffer.len() {
        match reader.read(&mut buffer[filled..]) {
            Ok(0) => break,
            Ok(read) => filled += read,
            Err(error) if error.kind() == std::io::ErrorKind::Interrupted => {}
            Err(error) => return Err(error),
        }
    }
    Ok(filled)
}

/// Adds every complete, well-formed stamp in `window` to `found`.
///
/// A prefix hit that is not followed by a valid digest is skipped rather than
/// treated as an identity: the archive also contains the bare symbol name and
/// may contain it inside debug info, and neither is the payload.
fn collect_stamps(window: &[u8], found: &mut BTreeSet<String>) {
    let prefix = STAMP_PREFIX.as_bytes();
    let mut from = 0;
    while let Some(offset) = find_subslice(&window[from..], prefix) {
        let at = from + offset;
        if let Some(stamp) = window.get(at..at + STAMP_LEN) {
            if let Some(digest) = digest_from_stamp(stamp) {
                found.insert(digest.to_string());
            }
        }
        from = at + 1;
    }
}

fn find_subslice(haystack: &[u8], needle: &[u8]) -> Option<usize> {
    if needle.is_empty() || haystack.len() < needle.len() {
        return None;
    }
    let first = needle[0];
    let last_start = haystack.len() - needle.len();
    let mut cursor = 0;
    while cursor <= last_start {
        let offset = haystack[cursor..=last_start]
            .iter()
            .position(|byte| *byte == first)?;
        let at = cursor + offset;
        if &haystack[at..at + needle.len()] == needle {
            return Some(at);
        }
        cursor = at + 1;
    }
    None
}

fn refusal(archive: &Path, found: &str, explanation: &str) -> String {
    let name = archive.file_name().map_or_else(
        || "libhew.a".to_string(),
        |name| name.to_string_lossy().into_owned(),
    );
    format!(
        "Error: {name} does not match this compiler — refusing to link.\n\
         \x20 archive               : {archive}\n\
         \x20 archive build identity: {found}\n\
         \x20 driver  build identity: {driver}\n\
         \n\
         {explanation}\n\
         Linking would fail with undefined runtime symbols such as\n\
         `hew_runtime_cleanup_after_main` — a stale build artifact, not a compiler bug.\n\
         \n\
         {fix}",
        archive = archive.display(),
        driver = DRIVER_IDENTITY,
        fix = fix_hint(&name),
    )
}

/// The rebuild instruction that actually rebuilds *this* archive.
///
/// The WASM support archives are built by their own targets; telling someone
/// staring at a refused `libhew_runtime.a` to run `make hew-native` sends them
/// to rebuild a different file and watch the refusal repeat.
fn fix_hint(name: &str) -> String {
    let (make_target, cargo_line) = match name {
        "libhew_runtime.a" => (
            "make wasm-runtime",
            "cargo build -p hew-cli && \
             cargo build -p hew-runtime --target wasm32-wasip1 --no-default-features",
        ),
        "libhew_std.a" => (
            "make wasm-runtime",
            "cargo build -p hew-cli && cargo build -p hew-std --target wasm32-wasip1",
        ),
        _ => ("make hew-native", "cargo build -p hew-cli -p hew-lib"),
    };
    format!(
        "Fix: rebuild both halves from this checkout —\n\
         \x20 {make_target}\n\
         or, without make:\n\
         \x20 {cargo_line}"
    )
}

#[cfg(test)]
mod tests {
    use super::{
        find_subslice, refusal, scan_reader, IdentityReadError, DRIVER_IDENTITY, STAMP_LEN,
        STAMP_PREFIX,
    };
    use hew_build_identity::DIGEST_HEX_LEN;

    fn stamp(digest: &str) -> Vec<u8> {
        format!("{STAMP_PREFIX}{digest}\0").into_bytes()
    }

    /// Runs the caller-facing verdict over archive bytes, so the refusal text a
    /// user would see is what gets asserted.
    fn verify_archive_bytes(bytes: &[u8]) -> Result<(), String> {
        let archive = std::path::Path::new("/repo/target/debug/libhew.a");
        super::verdict(archive, scan_reader(bytes))
    }

    #[test]
    fn driver_identity_is_a_real_digest() {
        assert!(
            hew_build_identity::is_valid_digest(DRIVER_IDENTITY),
            "driver identity should be a 64-char lower-case hex digest, got {DRIVER_IDENTITY:?}"
        );
    }

    #[test]
    fn a_stamp_is_found_in_a_small_buffer() {
        let digest = "a".repeat(DIGEST_HEX_LEN);
        let mut bytes = vec![0u8; 4096];
        bytes.extend_from_slice(&stamp(&digest));
        bytes.extend_from_slice(&[0u8; 4096]);
        assert_eq!(scan_reader(bytes.as_slice()).expect("stamp found"), digest);
    }

    #[test]
    fn a_stamp_straddling_a_chunk_boundary_is_found() {
        // Place the stamp so it starts a few bytes before the 1 MiB chunk edge:
        // the reader must carry the partial match into the next chunk.
        let digest = "b".repeat(DIGEST_HEX_LEN);
        let mut bytes = vec![b'.'; super::CHUNK_BYTES - 5];
        bytes.extend_from_slice(&stamp(&digest));
        bytes.extend_from_slice(&[b'.'; 128]);
        assert_eq!(scan_reader(bytes.as_slice()).expect("stamp found"), digest);
    }

    #[test]
    fn the_bare_symbol_name_is_not_mistaken_for_a_stamp() {
        let digest = "c".repeat(DIGEST_HEX_LEN);
        let mut bytes = b"HEW_BUILD_IDENTITY_V1\0some other symbol\0".to_vec();
        bytes.extend_from_slice(&stamp(&digest));
        assert_eq!(scan_reader(bytes.as_slice()).expect("stamp found"), digest);
    }

    #[test]
    fn a_malformed_stamp_does_not_shadow_the_real_one() {
        let digest = "d".repeat(DIGEST_HEX_LEN);
        let mut bytes = format!("{STAMP_PREFIX}not-a-digest").into_bytes();
        bytes.extend_from_slice(&stamp(&digest));
        assert_eq!(scan_reader(bytes.as_slice()).expect("stamp found"), digest);
    }

    #[test]
    fn an_unstamped_archive_fails_closed() {
        let bytes = vec![0u8; 4096];
        assert!(matches!(
            scan_reader(bytes.as_slice()),
            Err(IdentityReadError::Missing)
        ));
    }

    #[test]
    fn a_truncated_stamp_at_end_of_file_fails_closed() {
        let mut bytes = vec![0u8; 16];
        bytes.extend_from_slice(format!("{STAMP_PREFIX}{}", "e".repeat(10)).as_bytes());
        assert!(matches!(
            scan_reader(bytes.as_slice()),
            Err(IdentityReadError::Missing)
        ));
    }

    #[test]
    fn two_different_identities_are_refused_whichever_comes_first() {
        let stale = "1".repeat(DIGEST_HEX_LEN);
        let fresh = "2".repeat(DIGEST_HEX_LEN);

        for (first, second) in [(&stale, &fresh), (&fresh, &stale)] {
            let mut bytes = vec![b'.'; 512];
            bytes.extend_from_slice(&stamp(first));
            bytes.extend_from_slice(&[b'.'; 512]);
            bytes.extend_from_slice(&stamp(second));

            let Err(IdentityReadError::Conflicting(found)) = scan_reader(bytes.as_slice()) else {
                panic!("an archive carrying two identities must be refused, not resolved");
            };
            assert_eq!(found, vec![stale.clone(), fresh.clone()]);
        }
    }

    /// The driver's own identity appearing in the archive does not license the
    /// other one. A partially rebuilt archive is refused even when the half the
    /// scan reaches first is the matching half.
    #[test]
    fn a_matching_stamp_does_not_excuse_a_second_one() {
        let other = "3".repeat(DIGEST_HEX_LEN);
        let mut bytes = stamp(DRIVER_IDENTITY);
        bytes.extend_from_slice(&[b'.'; 64]);
        bytes.extend_from_slice(&stamp(&other));

        let message = verify_archive_bytes(&bytes).expect_err("must refuse");
        assert!(message.contains("ambiguous"), "{message}");
        assert!(message.contains(DRIVER_IDENTITY), "{message}");
        assert!(message.contains(&other), "{message}");
    }

    /// The umbrella archive legitimately carries the same stamp several times —
    /// once per staticlib member that embeds it — and the chunk overlap can
    /// rediscover one. Repeats of a single identity are not a conflict.
    #[test]
    fn the_same_identity_repeated_is_not_a_conflict() {
        let digest = "4".repeat(DIGEST_HEX_LEN);
        let mut bytes = vec![b'.'; 128];
        for _ in 0..3 {
            bytes.extend_from_slice(&stamp(&digest));
            bytes.extend_from_slice(&[b'.'; 128]);
        }
        assert_eq!(scan_reader(bytes.as_slice()).expect("stamp found"), digest);
    }

    /// A stamp landing on the chunk boundary is seen by two consecutive windows.
    /// Deduplication, not scan order, is what keeps that from reading as two
    /// identities.
    #[test]
    fn a_boundary_straddling_stamp_is_not_counted_twice() {
        let digest = "5".repeat(DIGEST_HEX_LEN);
        let mut bytes = vec![b'.'; super::CHUNK_BYTES - 5];
        bytes.extend_from_slice(&stamp(&digest));
        bytes.extend_from_slice(&[b'.'; 128]);
        assert_eq!(scan_reader(bytes.as_slice()).expect("stamp found"), digest);
    }

    #[test]
    fn subslice_search_handles_repeated_first_bytes() {
        assert_eq!(find_subslice(b"HHHHHEW", b"HEW"), Some(4));
        assert_eq!(find_subslice(b"abc", b"abcd"), None);
        assert_eq!(find_subslice(b"", b"a"), None);
    }

    #[test]
    fn the_refusal_names_the_stale_artifact_and_the_fix() {
        let message = refusal(
            std::path::Path::new("/repo/target/debug/libhew.a"),
            "missing — no stamp",
            "The archive carries no build identity at all.",
        );
        assert!(message.contains("/repo/target/debug/libhew.a"), "{message}");
        assert!(message.contains("make hew-native"), "{message}");
        assert!(
            message.contains("cargo build -p hew-cli -p hew-lib"),
            "{message}"
        );
        assert!(message.contains(DRIVER_IDENTITY), "{message}");
        assert!(
            !message.contains("undefined symbol:"),
            "the refusal replaces the undefined-symbol wall: {message}"
        );
    }

    /// A refused wasm archive must point at the target that rebuilds *it*.
    #[test]
    fn the_refusal_names_the_target_that_rebuilds_this_archive() {
        for (archive, expected) in [
            ("/repo/target/debug/libhew.a", "make hew-native"),
            (
                "/repo/target/wasm32-wasip1/debug/libhew_runtime.a",
                "make wasm-runtime",
            ),
            (
                "/repo/target/wasm32-wasip1/debug/libhew_std.a",
                "make wasm-runtime",
            ),
        ] {
            let message = refusal(std::path::Path::new(archive), "missing", "Explanation.");
            assert!(message.contains(expected), "{archive}: {message}");
        }
    }

    #[test]
    fn stamp_len_matches_the_shared_format() {
        assert_eq!(STAMP_LEN, STAMP_PREFIX.len() + DIGEST_HEX_LEN);
    }

    // ── memoization ───────────────────────────────────────────────────
    //
    // The memo may only ever turn a scan that would have said "matches this
    // driver" into no scan at all. These cover the two things that would make
    // it something else: answering for a file it did not scan, and answering
    // for a driver it did not match.

    use super::{marker_confirms, marker_path, verify_archive, ArchiveFingerprint, MARKER_VERSION};
    use std::path::Path;

    /// Writes a plausible archive: filler, a stamp, more filler.
    fn write_archive(path: &Path, digest: &str, filler: usize) {
        let mut bytes = vec![b'.'; filler];
        bytes.extend_from_slice(&stamp(digest));
        bytes.extend_from_slice(&[b'.'; 64]);
        std::fs::write(path, bytes).expect("archive written");
    }

    fn scratch() -> tempfile::TempDir {
        tempfile::tempdir().expect("tempdir")
    }

    #[test]
    fn a_matching_archive_is_verified_once_and_then_remembered() {
        let dir = scratch();
        let archive = dir.path().join("libhew.a");
        write_archive(&archive, DRIVER_IDENTITY, 512);

        verify_archive(&archive).expect("a matching archive links");

        let marker = marker_path(&archive).expect("marker path");
        assert!(marker.exists(), "a verified archive leaves a marker");

        let fingerprint = ArchiveFingerprint::of(&archive).expect("fingerprint");
        assert!(
            marker_confirms(&archive, &fingerprint),
            "the marker vouches for the file it was written for"
        );

        verify_archive(&archive).expect("the remembered archive still links");
    }

    /// The counterfactual that the memo has to survive: verify an archive,
    /// swap a mismatched one into its place, link again. The stale marker is
    /// still sitting there and must not be able to speak for the new file.
    #[test]
    fn a_replaced_archive_is_re_verified_and_refused() {
        let dir = scratch();
        let archive = dir.path().join("libhew.a");

        write_archive(&archive, DRIVER_IDENTITY, 512);
        verify_archive(&archive).expect("the matching archive links");
        let marker = marker_path(&archive).expect("marker path");
        assert!(marker.exists(), "precondition: a marker was written");

        let other = "7".repeat(DIGEST_HEX_LEN);
        write_archive(&archive, &other, 4096);

        let message = verify_archive(&archive)
            .expect_err("a replaced, mismatched archive must be refused, not served from cache");
        assert!(message.contains(&other), "{message}");
        assert!(message.contains(DRIVER_IDENTITY), "{message}");
        assert!(message.contains("refusing to link"), "{message}");

        let fingerprint = ArchiveFingerprint::of(&archive).expect("fingerprint");
        assert!(
            !marker_confirms(&archive, &fingerprint),
            "the marker must not vouch for a file it never described"
        );
    }

    /// Rebuilding in place can land the same byte count at the same path. The
    /// key still has to move, or the memo answers for bytes it never read.
    #[test]
    fn an_in_place_rewrite_of_the_same_size_changes_the_key() {
        let dir = scratch();
        let archive = dir.path().join("libhew.a");

        write_archive(&archive, &"a".repeat(DIGEST_HEX_LEN), 512);
        let before = ArchiveFingerprint::of(&archive).expect("fingerprint");

        write_archive(&archive, &"b".repeat(DIGEST_HEX_LEN), 512);
        let after = ArchiveFingerprint::of(&archive).expect("fingerprint");

        assert_eq!(
            std::fs::metadata(&archive).expect("metadata").len(),
            std::fs::metadata(&archive).expect("metadata").len(),
        );
        assert_ne!(
            before, after,
            "a same-size in-place rewrite must still miss the memo"
        );
    }

    /// A conflicted archive is refused on every link, not once. Caching only
    /// successes is what makes that automatic.
    #[test]
    fn a_conflicted_archive_is_refused_every_time_and_leaves_no_marker() {
        let dir = scratch();
        let archive = dir.path().join("libhew.a");

        let mut bytes = stamp(DRIVER_IDENTITY);
        bytes.extend_from_slice(&[b'.'; 64]);
        bytes.extend_from_slice(&stamp(&"8".repeat(DIGEST_HEX_LEN)));
        std::fs::write(&archive, bytes).expect("archive written");

        for _ in 0..2 {
            let message = verify_archive(&archive).expect_err("must refuse");
            assert!(message.contains("ambiguous"), "{message}");
        }

        assert!(
            !marker_path(&archive).expect("marker path").exists(),
            "a refusal is never memoized"
        );
    }

    #[test]
    fn a_marker_naming_another_driver_is_not_honoured() {
        let dir = scratch();
        let archive = dir.path().join("libhew.a");
        write_archive(&archive, DRIVER_IDENTITY, 512);

        let fingerprint = ArchiveFingerprint::of(&archive).expect("fingerprint");
        let marker = marker_path(&archive).expect("marker path");
        std::fs::write(
            &marker,
            format!(
                "{MARKER_VERSION}\n{}\n{}\n{}\n",
                fingerprint.path,
                fingerprint.stat,
                "9".repeat(DIGEST_HEX_LEN)
            ),
        )
        .expect("marker written");

        assert!(
            !marker_confirms(&archive, &fingerprint),
            "a marker written by a differently-built driver is a miss"
        );
    }

    /// Each line of the marker is load-bearing: a marker that agrees on
    /// everything but one field is a miss, not a near-enough hit.
    #[test]
    fn a_marker_disagreeing_on_any_line_is_not_honoured() {
        let dir = scratch();
        let archive = dir.path().join("libhew.a");
        write_archive(&archive, DRIVER_IDENTITY, 512);

        let fingerprint = ArchiveFingerprint::of(&archive).expect("fingerprint");
        let marker = marker_path(&archive).expect("marker path");
        let good = [
            MARKER_VERSION.to_string(),
            fingerprint.path.clone(),
            fingerprint.stat.clone(),
            DRIVER_IDENTITY.to_string(),
        ];

        for corrupt in 0..good.len() {
            let mut lines = good.clone();
            lines[corrupt] = format!("{}-tampered", lines[corrupt]);
            std::fs::write(&marker, lines.join("\n") + "\n").expect("marker written");
            assert!(
                !marker_confirms(&archive, &fingerprint),
                "line {corrupt} must be load-bearing"
            );
        }

        // Truncated and over-long markers are misses too.
        std::fs::write(&marker, good[..3].join("\n") + "\n").expect("marker written");
        assert!(!marker_confirms(&archive, &fingerprint));

        std::fs::write(&marker, good.join("\n") + "\nextra\n").expect("marker written");
        assert!(!marker_confirms(&archive, &fingerprint));

        std::fs::write(&marker, good.join("\n") + "\n").expect("marker written");
        assert!(
            marker_confirms(&archive, &fingerprint),
            "the intact marker is a hit"
        );
    }

    /// A marker carried over to a different archive describes a path that is
    /// not the one being linked, so it cannot vouch for it.
    #[test]
    fn a_marker_copied_beside_another_archive_is_not_honoured() {
        let dir = scratch();
        let original = dir.path().join("libhew.a");
        write_archive(&original, DRIVER_IDENTITY, 512);
        verify_archive(&original).expect("the matching archive links");

        let elsewhere = scratch();
        let copy = elsewhere.path().join("libhew.a");
        std::fs::copy(&original, &copy).expect("archive copied");
        std::fs::copy(
            marker_path(&original).expect("marker path"),
            marker_path(&copy).expect("marker path"),
        )
        .expect("marker copied");

        let fingerprint = ArchiveFingerprint::of(&copy).expect("fingerprint");
        assert!(
            !marker_confirms(&copy, &fingerprint),
            "a marker names the one file it was written for"
        );
    }

    #[test]
    fn an_unstamped_archive_is_refused_and_not_memoized() {
        let dir = scratch();
        let archive = dir.path().join("libhew.a");
        std::fs::write(&archive, vec![0u8; 4096]).expect("archive written");

        let message = verify_archive(&archive).expect_err("must refuse");
        assert!(message.contains("missing"), "{message}");
        assert!(!marker_path(&archive).expect("marker path").exists());
    }

    #[test]
    fn a_directory_is_never_fingerprinted() {
        let dir = scratch();
        assert!(ArchiveFingerprint::of(dir.path()).is_none());
        assert!(ArchiveFingerprint::of(&dir.path().join("absent.a")).is_none());
    }

    /// The in-process tier is what makes one driver process linking N programs
    /// scan once. It answers for a path only while the file behind that path is
    /// still the one that was scanned.
    #[test]
    fn the_in_process_memo_answers_for_one_file_and_not_its_successor() {
        let dir = scratch();
        let archive = dir.path().join("libhew.a");
        write_archive(&archive, DRIVER_IDENTITY, 512);

        let first = ArchiveFingerprint::of(&archive).expect("fingerprint");
        assert!(!super::in_process_memo_confirms(&first));

        super::remember_in_process(&first);
        assert!(super::in_process_memo_confirms(&first));

        // Same path, rebuilt file: the remembered entry must not answer.
        write_archive(&archive, DRIVER_IDENTITY, 8192);
        let second = ArchiveFingerprint::of(&archive).expect("fingerprint");
        assert_ne!(first.stat, second.stat);
        assert!(!super::in_process_memo_confirms(&second));
    }
}
