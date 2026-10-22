//! Ordinary YAML Value parsing, distinct from the strict typed wire codec.
//! Native and sandbox values share the same input limits and parser diagnostics.

/// Reject YAML inputs larger than 1 MiB before parsing to avoid memory abuse.
pub const YAML_PARSE_SIZE_LIMIT_BYTES: usize = 1024 * 1024;
/// Reject YAML inputs declaring more than 200 anchors before parsing.
///
/// 32 was too low: a YAML config with 33+ URL values containing `&param=val`
/// query-string fragments, or plain scalars like `AT&T`, triggered false
/// positives. The alias guard (`YAML_PARSE_ALIAS_LIMIT` = 1024) is the
/// primary billion-laughs defence; this constant guards the anchor fan-out
/// surface only.
pub const YAML_PARSE_ANCHOR_LIMIT: usize = 200;
/// Reject YAML inputs referencing more than 1024 aliases before parsing.
pub const YAML_PARSE_ALIAS_LIMIT: usize = 1024;

/// Parse an ordinary YAML value with the native resource limits.
///
/// # Errors
///
/// Returns the input-limit or serde YAML diagnostic without rewriting it.
pub fn parse(input: &str) -> Result<serde_yaml::Value, String> {
    validate_yaml_input_limits(input)?;
    serde_yaml::from_str(input).map_err(|error| error.to_string())
}

fn validate_yaml_input_limits(input: &str) -> Result<(), String> {
    if input.len() > YAML_PARSE_SIZE_LIMIT_BYTES {
        return Err(format!(
            "invalid YAML input: size limit exceeded ({} bytes > {} byte cap)",
            input.len(),
            YAML_PARSE_SIZE_LIMIT_BYTES
        ));
    }

    if !input
        .as_bytes()
        .iter()
        .any(|byte| matches!(byte, b'&' | b'*'))
    {
        return Ok(());
    }

    let bytes = input.as_bytes();
    let mut anchors = 0usize;
    let mut aliases = 0usize;
    let mut idx = 0usize;
    let mut in_single_quote = false;
    let mut in_double_quote = false;
    // Set when the scanner is inside a YAML line comment (`# …`). Reset on
    // both LF (`\n`) and bare CR (`\r`). Bare CR is the line terminator for
    // CR-only (old Mac) line endings. Without the CR reset, a `#` on a
    // CR-terminated comment line leaves `in_comment` set for the remainder of
    // the document, hiding all subsequent anchors and aliases from the guard.
    // The `\r` byte of a CRLF pair is also harmless to reset on early — the
    // following `\n` resets again, which is idempotent.
    let mut in_comment = false;

    while idx < bytes.len() {
        match bytes[idx] {
            b'\n' | b'\r' => {
                // End of line (LF, bare CR, or CRLF CR byte) resets comment state.
                in_comment = false;
            }
            b'#' if !in_single_quote && !in_double_quote && !in_comment => {
                // Start of a YAML line comment. Anchor/alias tokens in
                // comments are semantically inert; skip them.
                in_comment = true;
            }
            b'\'' if !in_double_quote && !in_comment => {
                if in_single_quote && bytes.get(idx + 1) == Some(&b'\'') {
                    idx += 2;
                    continue;
                }
                in_single_quote = !in_single_quote;
            }
            b'"' if !in_single_quote && !in_comment => {
                in_double_quote = !in_double_quote;
            }
            b'\\' if in_double_quote => {
                idx += 2;
                continue;
            }
            b'&' | b'*'
                if !in_single_quote
                    && !in_double_quote
                    && !in_comment
                    && bytes
                        .get(idx + 1)
                        .is_some_and(|next| is_yaml_anchor_alias_name_byte(*next)) =>
            {
                if bytes[idx] == b'&' {
                    anchors += 1;
                    if anchors > YAML_PARSE_ANCHOR_LIMIT {
                        return Err(format!(
                            "invalid YAML input: anchor limit exceeded ({anchors} > {YAML_PARSE_ANCHOR_LIMIT})"
                        ));
                    }
                } else {
                    aliases += 1;
                    if aliases > YAML_PARSE_ALIAS_LIMIT {
                        return Err(format!(
                            "invalid YAML input: alias limit exceeded ({aliases} > {YAML_PARSE_ALIAS_LIMIT})"
                        ));
                    }
                }
            }
            _ => {}
        }

        idx += 1;
    }

    Ok(())
}

fn is_yaml_anchor_alias_name_byte(byte: u8) -> bool {
    matches!(
        byte,
        b'a'..=b'z'
            | b'A'..=b'Z'
            | b'0'..=b'9'
            | b'_'
            | b'-'
            | b'.'
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fmt::Write as _;

    #[test]
    fn validate_cr_only_comment_cannot_hide_over_limit_anchors() {
        // With CR-only line endings (`\r` without `\n`), a YAML comment `#…\r`
        // must reset `in_comment` on the `\r` so that subsequent lines are
        // scanned normally. Before the fix, `in_comment` was only reset on `\n`,
        // leaving the scanner "inside a comment" for the rest of the document,
        // which allowed any number of real `&anchor` tokens to bypass the guard.
        //
        // This test calls `validate_yaml_input_limits` directly to verify the
        // byte-scanner behaviour regardless of how serde_yaml handles CR line
        // endings.
        let mut doc = String::from("# comment line\r");
        for i in 0..=200usize {
            // 201 real anchor tokens — one beyond YAML_PARSE_ANCHOR_LIMIT.
            write!(doc, "key_{i}: &anchor_{i} value_{i}\r").unwrap();
        }
        let result = validate_yaml_input_limits(&doc);
        assert!(
            result.is_err(),
            "CR-only comment line must not hide over-limit anchors from the guard"
        );
        let msg = result.unwrap_err();
        assert!(
            msg.contains("anchor limit"),
            "expected 'anchor limit' in error, got: {msg}"
        );
    }

    #[test]
    fn validate_cr_only_comment_cannot_hide_over_limit_aliases() {
        // Mirrors the anchor test above for alias tokens (`*alias`). With the
        // pre-fix scanner, a `#` comment on a CR-terminated line disables
        // alias counting for the whole document — YAML_PARSE_ALIAS_LIMIT (1024)
        // becomes bypassable with CR-only input.
        let mut doc = String::from("# comment line\r");
        for i in 0..=1024usize {
            // 1025 alias tokens — one beyond YAML_PARSE_ALIAS_LIMIT.
            write!(doc, "key_{i}: *alias_{i}\r").unwrap();
        }
        let result = validate_yaml_input_limits(&doc);
        assert!(
            result.is_err(),
            "CR-only comment line must not hide over-limit aliases from the guard"
        );
        let msg = result.unwrap_err();
        assert!(
            msg.contains("alias limit"),
            "expected 'alias limit' in error, got: {msg}"
        );
    }
}
