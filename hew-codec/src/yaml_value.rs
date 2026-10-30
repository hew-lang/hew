//! Ordinary YAML Value parsing, distinct from the strict typed wire codec.
//! Native and sandbox values share the same input limits and parser diagnostics.

use core::cell::Cell;
use core::fmt;

use serde::de::{self, DeserializeSeed, EnumAccess, MapAccess, SeqAccess, VariantAccess, Visitor};

/// Reject YAML inputs larger than 1 MiB before parsing to avoid memory abuse.
pub const YAML_PARSE_SIZE_LIMIT_BYTES: usize = 1024 * 1024;
/// Reject YAML inputs declaring more than 200 anchors before parsing.
///
/// 32 was too low: a YAML config with 33+ URL values containing `&param=val`
/// query-string fragments, or plain scalars like `AT&T`, triggered false
/// positives. [`check_alias_expansion`] is the billion-laughs defence; this
/// constant and [`YAML_PARSE_ALIAS_LIMIT`] are cheap textual pre-checks.
pub const YAML_PARSE_ANCHOR_LIMIT: usize = 200;
/// Reject YAML inputs referencing more than 1024 aliases before parsing.
pub const YAML_PARSE_ALIAS_LIMIT: usize = 1024;

/// Parse an ordinary YAML value with the native resource limits.
///
/// # Errors
///
/// Returns the input-limit, expansion-limit or serde YAML diagnostic without
/// rewriting it.
pub fn parse(input: &str) -> Result<serde_yaml::Value, String> {
    validate_yaml_input_limits(input)?;
    check_alias_expansion(input).map_err(|error| error.to_string())?;
    serde_yaml::from_str(input).map_err(|error| error.to_string())
}

/// Expansion budget units each input byte buys. A node costs
/// [`YAML_NODE_COST`] units plus one per scalar byte, so a document without
/// aliases always fits; aliases may at most multiply the document's size by
/// this factor over [`YAML_NODE_COST`].
pub const YAML_EXPANSION_UNITS_PER_BYTE: usize = 32;
/// Expansion units every document may use regardless of its length.
pub const YAML_EXPANSION_FLOOR_UNITS: usize = 64 * 1024;
/// Units one node costs before its scalar text.
const YAML_NODE_COST: usize = 16;

/// Refuse a document whose aliases expand it beyond its expansion budget.
///
/// Counting anchors and aliases does not bound the parsed tree: an anchor
/// over a large node, or one that itself holds aliases, is copied in full at
/// every alias, so a few hundred aliases grow a few kilobytes into gigabytes.
/// This pass walks the document as `serde_yaml` delivers it, aliases
/// replayed, keeps nothing, and stops as soon as the walk exceeds the
/// budget, before any tree is built. Errors other than the budget are left
/// to the real parse.
///
/// # Errors
///
/// Returns the expansion-limit diagnostic, at the position the walk stopped.
pub fn check_alias_expansion(input: &str) -> Result<(), serde_yaml::Error> {
    let left = Cell::new(
        YAML_EXPANSION_UNITS_PER_BYTE
            .saturating_mul(input.len())
            .max(YAML_EXPANSION_FLOOR_UNITS),
    );
    let over = Cell::new(false);
    let walk = Walk {
        left: &left,
        over: &over,
    };
    match walk.deserialize(serde_yaml::Deserializer::from_str(input)) {
        Err(error) if over.get() => Err(error),
        _ => Ok(()),
    }
}

/// A visitor that keeps nothing and charges every node it is shown.
#[derive(Clone, Copy)]
struct Walk<'b> {
    left: &'b Cell<usize>,
    over: &'b Cell<bool>,
}

impl Walk<'_> {
    fn charge<E: de::Error>(self, scalar_len: usize) -> Result<(), E> {
        let cost = YAML_NODE_COST.saturating_add(scalar_len);
        if let Some(left) = self.left.get().checked_sub(cost) {
            self.left.set(left);
            Ok(())
        } else {
            self.over.set(true);
            Err(E::custom(
                "invalid YAML input: alias expansion limit exceeded",
            ))
        }
    }
}

impl<'de> DeserializeSeed<'de> for Walk<'_> {
    type Value = ();

    fn deserialize<D: de::Deserializer<'de>>(self, deserializer: D) -> Result<(), D::Error> {
        deserializer.deserialize_any(self)
    }
}

impl<'de> Visitor<'de> for Walk<'_> {
    type Value = ();

    fn expecting(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("any YAML value")
    }

    fn visit_bool<E: de::Error>(self, _: bool) -> Result<(), E> {
        self.charge(0)
    }

    fn visit_i64<E: de::Error>(self, _: i64) -> Result<(), E> {
        self.charge(0)
    }

    fn visit_u64<E: de::Error>(self, _: u64) -> Result<(), E> {
        self.charge(0)
    }

    fn visit_f64<E: de::Error>(self, _: f64) -> Result<(), E> {
        self.charge(0)
    }

    fn visit_str<E: de::Error>(self, value: &str) -> Result<(), E> {
        self.charge(value.len())
    }

    fn visit_unit<E: de::Error>(self) -> Result<(), E> {
        self.charge(0)
    }

    fn visit_none<E: de::Error>(self) -> Result<(), E> {
        self.charge(0)
    }

    fn visit_some<D: de::Deserializer<'de>>(self, deserializer: D) -> Result<(), D::Error> {
        self.deserialize(deserializer)
    }

    fn visit_seq<A: SeqAccess<'de>>(self, mut seq: A) -> Result<(), A::Error> {
        self.charge(0)?;
        while seq.next_element_seed(self)?.is_some() {}
        Ok(())
    }

    fn visit_map<A: MapAccess<'de>>(self, mut map: A) -> Result<(), A::Error> {
        self.charge(0)?;
        while map.next_key_seed(self)?.is_some() {
            map.next_value_seed(self)?;
        }
        Ok(())
    }

    /// A tagged node: the tag, then its value.
    fn visit_enum<A: EnumAccess<'de>>(self, data: A) -> Result<(), A::Error> {
        let ((), variant) = data.variant_seed(self)?;
        variant.newtype_variant_seed(self)
    }
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

    /// `c` holds `refs` copies of `b`, which holds `refs` copies of the
    /// `items`-element anchor `a`: about `items * refs * refs` nodes from a
    /// document of a few kilobytes and well under the alias count limit.
    fn nested_fan_out(items: usize, refs: usize) -> String {
        let big = vec!["x"; items].join(", ");
        let mid = vec!["*a"; refs].join(", ");
        let top = vec!["*b"; refs].join(", ");
        format!("a: &a [{big}]\nb: &b [{mid}]\nc: [{top}]\n")
    }

    #[test]
    fn nested_alias_fan_out_is_refused_before_expansion() {
        // 64 million nodes from 4.4 KB; this used to exhaust memory.
        let doc = nested_fan_out(400, 400);
        assert!(
            validate_yaml_input_limits(&doc).is_ok(),
            "under the textual limits"
        );
        let err = parse(&doc).expect_err("expansion is bounded");
        assert!(err.contains("alias expansion limit"), "got: {err}");
    }

    #[test]
    fn large_anchor_repeated_is_refused() {
        let doc = format!(
            "a: &a \"{}\"\nb: [{}]\n",
            "x".repeat(16 * 1024),
            vec!["*a"; 1000].join(", ")
        );
        let err = parse(&doc).expect_err("16 MB of copies from 20 KB");
        assert!(err.contains("alias expansion limit"), "got: {err}");
    }

    #[test]
    fn ordinary_alias_reuse_still_parses() {
        let mut doc = String::from("base: &base {retries: 3, timeout: 30, region: ca-central}\n");
        for i in 0..200 {
            writeln!(doc, "svc{i}: {{<<: *base, name: svc{i}}}").unwrap();
        }
        let value = parse(&doc).expect("modest reuse is within budget");
        assert_eq!(value["svc7"]["name"], serde_yaml::Value::from("svc7"));
    }

    #[test]
    fn alias_free_documents_always_fit_the_budget() {
        // Densest node-per-byte shapes: empty flow sequences and implicit nulls.
        for doc in ["[[], [], [], []]", "a:\nb:\nc:\n", "- \n- \n- \n"] {
            assert!(check_alias_expansion(doc).is_ok(), "{doc:?}");
        }
        let wide = format!("[{}]", vec!["[]"; 200_000].join(","));
        assert!(check_alias_expansion(&wide).is_ok());
    }

    #[test]
    fn wire_yaml_decode_shares_the_expansion_bound() {
        let doc = nested_fan_out(400, 400);
        let err = crate::Source::new(crate::Format::Yaml, doc.as_bytes())
            .expect_err("the typed codec refuses it too");
        assert!(
            format!("{err:?}").contains("alias expansion limit"),
            "got: {err:?}"
        );
    }

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
