//! Content-addressed build identity for the Hew runtime + standard library.
//!
//! `cargo build -p hew-cli` produces the compiler driver only. Linking a
//! compiled Hew program additionally needs `hew-lib`'s staticlib
//! (`target/debug/libhew.a`, `target/debug/hew.lib` on Windows) sitting next to
//! that driver. Nothing in Cargo ties the two together, so a fresh driver can
//! end up beside a month-old archive — and the failure surfaces as a wall of
//! undefined `hew_*` symbols that reads like a compiler bug.
//!
//! This crate defines the single stamp format that closes that gap:
//!
//! * every crate whose staticlib the driver can link — `hew-lib`, and
//!   `hew-runtime` / `hew-std` for WASM — computes [`scan::compute`] over the
//!   same input set in its build script and emits a `#[no_mangle]` static
//!   holding the stamp, so the digest is physically present inside the archive.
//! * `hew-cli`'s build script computes the same digest and bakes it into the
//!   driver.
//! * At link time the driver reads the archive's stamp back and refuses to link
//!   when the two disagree.
//!
//! The digest covers source *content*, not the package version string. A
//! version string does not move between commits, which is exactly why a stale
//! archive looked plausible; a content hash changes on every source edit.

/// ASCII marker that immediately precedes the digest inside the archive.
///
/// The trailing `=` matters. The archive's symbol table carries the bare symbol
/// name, and debug info can carry `HEW_BUILD_IDENTITY_V1:` with a type
/// annotation after it; neither spelling uses `=`, so the separator keeps the
/// scanner off everything but the payload itself.
pub const STAMP_PREFIX: &str = "HEW_BUILD_IDENTITY_V1=";

/// Number of lower-case hex characters in the digest that follows the prefix.
pub const DIGEST_HEX_LEN: usize = 64;

/// Total stamp length: [`STAMP_PREFIX`] followed by [`DIGEST_HEX_LEN`] hex chars.
pub const STAMP_LEN: usize = STAMP_PREFIX.len() + DIGEST_HEX_LEN;

/// Name of the `#[no_mangle]` static that `hew-lib` exports to carry the stamp.
pub const STAMP_SYMBOL: &str = "HEW_BUILD_IDENTITY_V1";

/// The package whose staticlib *is* the archive.
///
/// Its non-dev path-dependency closure — resolved from the manifests, never
/// listed by hand — is the set of crates whose sources define the identity.
/// `hew-lib` is the umbrella staticlib; it links `hew-runtime`, the
/// consolidated `hew-std` archive and everything those pull in, and an edit
/// anywhere in that closure changes the bytes of `libhew.a`.
pub const ROOT_INPUT_CRATE: &str = "hew-lib";

/// File names and extensions that participate in the digest.
pub const INPUT_FILE_EXTENSIONS: [&str; 1] = ["rs"];

/// Exact file names that participate in the digest regardless of extension.
pub const INPUT_FILE_NAMES: [&str; 2] = ["Cargo.toml", "build.rs"];

/// Workspace-root files that participate in the digest.
///
/// The lockfile pins every external dependency the archive compiles against, so
/// `cargo update` changes the linked code while every crate source stays
/// byte-identical; the workspace manifest carries the shared dependency
/// versions, the profiles and the lint configuration. Without both, two
/// non-equivalent archives can carry the same digest.
pub const WORKSPACE_INPUT_FILES: [&str; 2] = ["Cargo.toml", "Cargo.lock"];

/// Returns `true` when `candidate` is a well-formed identity digest.
///
/// Fail-closed callers use this to reject a stamp they managed to locate but
/// cannot interpret, rather than treating an unparsable stamp as a match.
#[must_use]
pub fn is_valid_digest(candidate: &str) -> bool {
    candidate.len() == DIGEST_HEX_LEN
        && candidate
            .bytes()
            .all(|b| b.is_ascii_digit() || (b'a'..=b'f').contains(&b))
}

/// Extracts the digest from a byte window that starts with [`STAMP_PREFIX`].
///
/// Returns `None` when the window is too short, does not start with the prefix,
/// or the trailing characters are not a valid digest.
#[must_use]
pub fn digest_from_stamp(window: &[u8]) -> Option<&str> {
    let rest = window.strip_prefix(STAMP_PREFIX.as_bytes())?;
    let digest = rest.get(..DIGEST_HEX_LEN)?;
    let digest = core::str::from_utf8(digest).ok()?;
    is_valid_digest(digest).then_some(digest)
}

#[cfg(feature = "scan")]
pub mod scan;

#[cfg(test)]
mod tests {
    use super::{digest_from_stamp, is_valid_digest, DIGEST_HEX_LEN, STAMP_LEN, STAMP_PREFIX};

    #[test]
    fn stamp_len_is_prefix_plus_digest() {
        assert_eq!(STAMP_LEN, STAMP_PREFIX.len() + DIGEST_HEX_LEN);
    }

    #[test]
    fn digest_validation_rejects_wrong_shape() {
        assert!(is_valid_digest(&"a".repeat(DIGEST_HEX_LEN)));
        assert!(!is_valid_digest(&"a".repeat(DIGEST_HEX_LEN - 1)));
        assert!(!is_valid_digest(&"A".repeat(DIGEST_HEX_LEN)));
        assert!(!is_valid_digest(&"g".repeat(DIGEST_HEX_LEN)));
    }

    #[test]
    fn digest_is_extracted_from_a_well_formed_stamp() {
        let stamp = format!("{STAMP_PREFIX}{}", "0".repeat(DIGEST_HEX_LEN));
        assert_eq!(
            digest_from_stamp(stamp.as_bytes()),
            Some("0".repeat(DIGEST_HEX_LEN).as_str())
        );
    }

    #[test]
    fn digest_extraction_fails_closed_on_a_truncated_stamp() {
        let stamp = format!("{STAMP_PREFIX}{}", "0".repeat(DIGEST_HEX_LEN - 1));
        assert_eq!(digest_from_stamp(stamp.as_bytes()), None);
    }

    #[test]
    fn digest_extraction_rejects_the_bare_symbol_name() {
        // The archive symbol table carries `HEW_BUILD_IDENTITY_V1` with no
        // separator, and debug info can carry it followed by `:`; neither may
        // be mistaken for the payload.
        assert_eq!(digest_from_stamp(b"HEW_BUILD_IDENTITY_V1"), None);
        let annotated = format!("HEW_BUILD_IDENTITY_V1: [u8; {}]", STAMP_LEN + 1);
        assert_eq!(digest_from_stamp(annotated.as_bytes()), None);
    }
}
