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
