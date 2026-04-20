use std::path::PathBuf;
use std::time::Duration;

/// Locate the `hew` binary for subprocess-based CLI features.
///
/// When running as `hew <subcommand>`, `current_exe()` is the hew binary itself.
/// When running unit tests, the test binary is in `target/debug/deps/` and the
/// hew binary is usually at `target/debug/hew`.
pub(crate) fn find_hew_binary() -> Result<PathBuf, String> {
    let exe = std::env::current_exe().map_err(|e| format!("cannot locate self: {e}"))?;

    if exe
        .file_name()
        .is_some_and(|name| name == "hew" || name == "hew.exe")
    {
        return Ok(exe);
    }

    let exe_dir = exe.parent().expect("exe should have a parent directory");
    let hew_name = format!("hew{}", crate::platform::exe_suffix());
    let candidates = [
        exe_dir.join(format!("../{hew_name}")),
        exe_dir.join(&hew_name),
        exe_dir.join(format!("../../debug/{hew_name}")),
    ];

    for candidate in &candidates {
        if candidate.exists() {
            return candidate
                .canonicalize()
                .map_err(|e| format!("cannot resolve hew binary path: {e}"));
        }
    }

    Err(format!(
        "cannot find hew binary (searched relative to {})",
        exe_dir.display()
    ))
}

pub(crate) fn html_escape(input: &str) -> String {
    let mut escaped = String::with_capacity(input.len());
    for ch in input.chars() {
        match ch {
            '&' => escaped.push_str("&amp;"),
            '<' => escaped.push_str("&lt;"),
            '>' => escaped.push_str("&gt;"),
            '"' => escaped.push_str("&quot;"),
            _ => escaped.push(ch),
        }
    }
    escaped
}

pub(crate) fn parse_timeout(raw: &str) -> Result<Duration, String> {
    use std::num::IntErrorKind;

    const INVALID_TIMEOUT_MESSAGE: &str =
        "invalid --timeout; expected an integer with optional unit suffix (ms, s, m)";

    let value = raw.trim();
    if value.is_empty() {
        return Err(INVALID_TIMEOUT_MESSAGE.to_string());
    }

    let (amount, unit) = if let Some(ms) = value.strip_suffix("ms") {
        (ms, "ms")
    } else if let Some(seconds) = value.strip_suffix('s') {
        (seconds, "s")
    } else if let Some(minutes) = value.strip_suffix('m') {
        (minutes, "m")
    } else {
        (value, "s")
    };

    if amount.is_empty() {
        return Err(INVALID_TIMEOUT_MESSAGE.to_string());
    }

    let parsed = amount.parse::<u64>().map_err(|error| match error.kind() {
        IntErrorKind::PosOverflow => format!("invalid --timeout '{raw}': value is too large"),
        _ => INVALID_TIMEOUT_MESSAGE.to_string(),
    })?;

    if parsed == 0 {
        return Err("--timeout must be at least 1 second".to_string());
    }

    match unit {
        "ms" => Ok(Duration::from_millis(parsed)),
        "s" => Ok(Duration::from_secs(parsed)),
        "m" => parsed
            .checked_mul(60)
            .map(Duration::from_secs)
            .ok_or_else(|| format!("invalid --timeout '{raw}': value is too large")),
        _ => unreachable!("unsupported timeout unit"),
    }
}

#[cfg(test)]
mod tests {
    use super::{html_escape, parse_timeout};
    use std::time::Duration;

    #[test]
    fn html_escape_escapes_special_characters() {
        assert_eq!(html_escape(r#"a<b>c&d"e'f"#), "a&lt;b&gt;c&amp;d&quot;e'f");
    }

    #[test]
    fn html_escape_handles_empty_and_existing_entities() {
        assert_eq!(html_escape(""), "");
        assert_eq!(html_escape("&lt;tag&gt;"), "&amp;lt;tag&amp;gt;");
    }

    #[test]
    fn parse_timeout_accepts_supported_units() {
        assert_eq!(parse_timeout("30").unwrap(), Duration::from_secs(30));
        assert_eq!(parse_timeout("30s").unwrap(), Duration::from_secs(30));
        assert_eq!(parse_timeout("500ms").unwrap(), Duration::from_millis(500));
        assert_eq!(parse_timeout("1m").unwrap(), Duration::from_mins(1));
    }

    #[test]
    fn parse_timeout_rejects_empty_and_malformed_values() {
        assert_eq!(
            parse_timeout("").unwrap_err(),
            "invalid --timeout; expected an integer with optional unit suffix (ms, s, m)"
        );
        assert_eq!(
            parse_timeout("abc").unwrap_err(),
            "invalid --timeout; expected an integer with optional unit suffix (ms, s, m)"
        );
        assert_eq!(
            parse_timeout("5h").unwrap_err(),
            "invalid --timeout; expected an integer with optional unit suffix (ms, s, m)"
        );
    }

    #[test]
    fn parse_timeout_rejects_zero_and_overflow() {
        assert_eq!(
            parse_timeout("0").unwrap_err(),
            "--timeout must be at least 1 second"
        );
        assert_eq!(
            parse_timeout("18446744073709551616s").unwrap_err(),
            "invalid --timeout '18446744073709551616s': value is too large"
        );
        assert_eq!(
            parse_timeout("307445734561825861m").unwrap_err(),
            "invalid --timeout '307445734561825861m': value is too large"
        );
    }
}
