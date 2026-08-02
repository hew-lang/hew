//! Package-name validation shared by registry-facing code paths.

const PACKAGE_NAME_RULES: &str = "only lowercase alphanumeric, `_`, and `::` allowed";

/// Validate a package name in canonical `::` notation.
///
/// Names must be non-empty, at most 128 bytes, and composed of non-empty
/// lowercase ASCII alphanumeric/underscore segments separated by `::`.
#[must_use]
pub(crate) fn is_valid(name: &str) -> bool {
    if name.is_empty() || name.len() > 128 {
        return false;
    }
    if name.starts_with("::") || name.ends_with("::") {
        return false;
    }
    if name.contains('/') || name.contains('\\') {
        return false;
    }

    for segment in name.split("::") {
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
        assert!(is_valid("std::net::http"));
        assert!(is_valid("pkg123::v2"));
    }

    #[test]
    fn rejects_invalid_names() {
        for name in [
            "",
            "my package",
            "my@package",
            "my:package",
            "std::::http",
            "::std",
            "std::",
            "std/net/http",
            "std\\net",
            "evil::..::etc",
            "evil::../../../tmp/pwned",
            "evil::/tmp/pwned",
        ] {
            assert!(!is_valid(name), "{name} should be invalid");
        }
    }
}
