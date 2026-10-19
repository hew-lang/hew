//! Package-name validation shared by registry-facing code paths.

const PACKAGE_NAME_RULES: &str = "only lowercase alphanumeric, `_`, and dotted segments allowed";

/// Encode an authored identity only after checking its complete grammar.
pub(crate) fn to_wire(name: &str, mode: crate::config::WireNames) -> Result<String, String> {
    if !is_valid(name) {
        return Err(invalid_message(name));
    }
    Ok(match mode {
        crate::config::WireNames::Dotted => name.to_string(),
        crate::config::WireNames::Slash => name.replace('.', "/"),
    })
}

/// Decode the exact configured wire grammar without accepting mixed identities.
pub(crate) fn from_wire(name: &str, mode: crate::config::WireNames) -> Result<String, String> {
    match mode {
        crate::config::WireNames::Dotted => to_wire(name, mode),
        crate::config::WireNames::Slash => {
            if name.contains('.') {
                return Err(format!("invalid slash package identity `{name}`"));
            }
            let logical = name.replace('/', ".");
            if !is_valid(&logical) {
                return Err(format!("invalid slash package identity `{name}`"));
            }
            Ok(logical)
        }
    }
}

/// Registry publishers retain authored dependency names; other publishers may
/// submit slash names. Decode either complete grammar, never a mixture.
pub(crate) fn dependency_from_wire(name: &str) -> Result<String, String> {
    let mode = if name.contains('/') {
        crate::config::WireNames::Slash
    } else {
        crate::config::WireNames::Dotted
    };
    from_wire(name, mode)
}

/// Validate a package name in dotted notation.
///
/// Names must be non-empty, at most 128 bytes, and composed of non-empty
/// lowercase ASCII alphanumeric/underscore segments separated by dots.
#[must_use]
pub(crate) fn is_valid(name: &str) -> bool {
    if name.is_empty() || name.len() > 128 {
        return false;
    }
    if name.starts_with('.') || name.ends_with('.') {
        return false;
    }
    if name.contains('/') || name.contains('\\') {
        return false;
    }

    for segment in name.split('.') {
        if segment.is_empty() {
            return false;
        }
        if !segment
            .chars()
            .all(|c| c.is_ascii_lowercase() || c.is_ascii_digit() || c == '_')
        {
            return false;
        }
    }
    true
}

/// The first segment of a valid name that an import path cannot spell, if
/// any. A published package must be importable by its whole name, so publish
/// and `hew check` refuse such a name; [`is_valid`] alone keeps governing wire
/// and lockfile identity, so names already on a registry still resolve.
///
/// The parser's import-segment predicate is the authority: `acme.type` is
/// importable, `acme.3d` is not.
#[must_use]
pub(crate) fn unimportable_segment(name: &str) -> Option<&str> {
    name.split('.')
        .find(|segment| !hew_parser::is_import_path_segment(segment))
}

/// Check that `name` is a valid package name that an import can spell.
///
/// # Errors
///
/// Returns the diagnostic for the first rule `name` breaks.
pub(crate) fn check_importable(name: &str) -> Result<(), String> {
    if !is_valid(name) {
        return Err(invalid_message(name));
    }
    match unimportable_segment(name) {
        Some(segment) => Err(format!(
            "invalid package name `{name}`: segment `{segment}` cannot be written in an import \
             path (`import {name};` does not parse); each segment must start with a letter or `_`"
        )),
        None => Ok(()),
    }
}

#[must_use]
pub(crate) fn invalid_message(name: &str) -> String {
    format!("invalid package name `{name}`: {PACKAGE_NAME_RULES}")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn accepts_canonical_names() {
        assert!(is_valid("mypackage"));
        assert!(is_valid("my_package"));
        assert!(is_valid("std.net.http"));
        assert!(is_valid("pkg123.v2"));
    }

    #[test]
    fn wire_identity_rejects_mixed_or_escaping_names() {
        use crate::config::WireNames;
        assert_eq!(
            to_wire("hew.math.stats", WireNames::Slash).unwrap(),
            "hew/math/stats"
        );
        assert_eq!(
            from_wire("hew/math/stats", WireNames::Slash).unwrap(),
            "hew.math.stats"
        );
        assert_eq!(
            dependency_from_wire("hew.math.stats").unwrap(),
            "hew.math.stats"
        );
        assert_eq!(
            dependency_from_wire("hew/math/stats").unwrap(),
            "hew.math.stats"
        );
        for name in [
            "hew.math/stats",
            "hew//stats",
            "../stats",
            "/stats",
            "hew/%2e%2e/stats",
            "hew::stats",
        ] {
            assert!(dependency_from_wire(name).is_err(), "{name}");
        }
        assert!(from_wire("hew.math.stats", WireNames::Slash).is_err());
        assert!(from_wire("hew/math/stats", WireNames::Dotted).is_err());
    }

    #[test]
    fn importable_names_follow_the_parser() {
        for name in ["acme.three_d", "acme.type", "acme.actor", "mypackage"] {
            assert!(check_importable(name).is_ok(), "{name}");
        }
        for name in ["acme.3d", "3d", "pkg123.v2.9x"] {
            let error = check_importable(name).unwrap_err();
            assert!(
                error.contains("cannot be written in an import path"),
                "{error}"
            );
        }
        assert!(check_importable("Acme.x")
            .unwrap_err()
            .contains("only lowercase"));
    }

    #[test]
    fn every_ecosystem_package_name_is_importable() {
        for name in [
            "hew.auth.oauth",
            "hew.dag",
            "hew.db.mongodb",
            "hew.db.mysql",
            "hew.db.postgres",
            "hew.db.redis",
            "hew.db.sql",
            "hew.db.sqlite",
            "hew.image.magick",
            "hew.math.stats",
            "hew.metrics",
            "hew.net.http",
            "hew.queue.mqtt",
            "hew.queue.nats",
            "hew.storage.s3",
            "hew.template",
        ] {
            assert!(check_importable(name).is_ok(), "{name}");
        }
    }

    #[test]
    fn rejects_invalid_names() {
        for name in [
            "",
            "my package",
            "my@package",
            "my:package",
            "std..http",
            ".std",
            "std.",
            "std/net/http",
            "std\\net",
            "..",
            "evil..etc",
            "evil.../../../tmp/pwned",
            "evil./tmp/pwned",
        ] {
            assert!(!is_valid(name), "{name} should be invalid");
        }
    }
}
