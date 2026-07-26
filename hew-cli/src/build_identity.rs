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

use std::collections::BTreeSet;
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
    /// The archive carries more than one distinct identity.
    Conflicting(Vec<String>),
}

/// Refuses to link `archive` unless it was built from this driver's sources.
///
/// Returns the caller-facing error text on refusal so the resolution path can
/// surface it exactly like any other link failure.
pub(crate) fn verify_archive(archive: &Path) -> Result<(), String> {
    verdict(archive, read_archive_identity(archive))
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
}
