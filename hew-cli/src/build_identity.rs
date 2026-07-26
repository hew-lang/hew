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

use std::io::Read;
use std::path::Path;

use hew_build_identity::{digest_from_stamp, STAMP_LEN, STAMP_PREFIX};

/// Identity of the runtime + stdlib sources this driver was built from.
pub(crate) const DRIVER_IDENTITY: &str = env!("HEW_BUILD_IDENTITY");

/// Bytes read per filesystem round trip while scanning for the stamp.
const CHUNK_BYTES: usize = 1 << 20;

/// Why an archive's build identity could not be established.
#[derive(Debug)]
enum IdentityReadError {
    /// The archive could not be opened or read.
    Unreadable(std::io::Error),
    /// The archive carries no usable stamp.
    Missing,
}

/// Refuses to link `archive` unless it was built from this driver's sources.
///
/// Returns the caller-facing error text on refusal so the resolution path can
/// surface it exactly like any other link failure.
pub(crate) fn verify_archive(archive: &Path) -> Result<(), String> {
    match read_archive_identity(archive) {
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

/// Streams `reader` looking for the first well-formed stamp.
///
/// The archive is large (a debug `libhew.a` runs to hundreds of megabytes), so
/// this never loads the whole file: it keeps one chunk plus a `STAMP_LEN - 1`
/// byte tail, which is exactly enough for a stamp straddling a chunk boundary.
fn scan_reader<R: Read>(mut reader: R) -> Result<String, IdentityReadError> {
    let overlap = STAMP_LEN - 1;
    let mut chunk = vec![0u8; CHUNK_BYTES];
    let mut window: Vec<u8> = Vec::with_capacity(CHUNK_BYTES + overlap);

    loop {
        let read = fill(&mut reader, &mut chunk).map_err(IdentityReadError::Unreadable)?;
        if read == 0 {
            break;
        }
        window.extend_from_slice(&chunk[..read]);
        if let Some(digest) = scan_window(&window) {
            return Ok(digest);
        }
        if window.len() > overlap {
            let stale = window.len() - overlap;
            window.drain(..stale);
        }
    }

    scan_window(&window).ok_or(IdentityReadError::Missing)
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

/// Finds the first complete, well-formed stamp in `window`.
///
/// A prefix hit that is not followed by a valid digest is skipped rather than
/// treated as the answer: the archive also contains the bare symbol name and
/// may contain it inside debug info, and neither is the payload.
fn scan_window(window: &[u8]) -> Option<String> {
    let prefix = STAMP_PREFIX.as_bytes();
    let mut from = 0;
    while let Some(offset) = find_subslice(&window[from..], prefix) {
        let at = from + offset;
        if let Some(stamp) = window.get(at..at + STAMP_LEN) {
            if let Some(digest) = digest_from_stamp(stamp) {
                return Some(digest.to_string());
            }
        }
        from = at + 1;
    }
    None
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
         Fix: rebuild both halves from this checkout —\n\
         \x20 make hew-native\n\
         or, without make:\n\
         \x20 cargo build -p hew-cli -p hew-lib",
        archive = archive.display(),
        driver = DRIVER_IDENTITY,
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

    #[test]
    fn stamp_len_matches_the_shared_format() {
        assert_eq!(STAMP_LEN, STAMP_PREFIX.len() + DIGEST_HEX_LEN);
    }
}
