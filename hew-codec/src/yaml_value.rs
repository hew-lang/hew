//! Ordinary YAML Value parsing, distinct from the strict typed wire codec.
//! Native and sandbox values share the same input limits and parser diagnostics.

use core::cell::Cell;
use core::fmt;

use serde::de::{self, DeserializeSeed, EnumAccess, MapAccess, SeqAccess, VariantAccess, Visitor};

/// Reject YAML inputs larger than 1 MiB before parsing to avoid memory abuse.
pub const YAML_PARSE_SIZE_LIMIT_BYTES: usize = 1024 * 1024;

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

/// Expansion units any one document may use. A node costs
/// [`YAML_NODE_COST`] units plus one per scalar byte, so every alias-free
/// document within [`YAML_PARSE_SIZE_LIMIT_BYTES`] fits, and no document,
/// aliases replayed, builds more than about a million nodes.
pub const YAML_EXPANSION_BUDGET_UNITS: usize = 16 * YAML_PARSE_SIZE_LIMIT_BYTES;
/// Units one node costs before its scalar text.
const YAML_NODE_COST: usize = 16;

/// Refuse a document whose aliases expand it beyond the expansion budget.
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
    let left = Cell::new(YAML_EXPANSION_BUDGET_UNITS);
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
    Ok(())
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
            "under the size limit"
        );
        let err = parse(&doc).expect_err("expansion is bounded");
        assert!(err.contains("alias expansion limit"), "got: {err}");
    }

    #[test]
    fn large_anchor_repeated_is_refused() {
        let doc = format!(
            "a: &a \"{}\"\nb: [{}]\n",
            "x".repeat(16 * 1024),
            vec!["*a"; 2000].join(", ")
        );
        let err = parse(&doc).expect_err("32 MB of copies from 24 KB");
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
    fn merged_defaults_across_many_services_parse() {
        // A compose-style file: 100 shared settings merged into 200 services
        // expands about 1,500-fold over its 13 KB of text.
        let mut doc = String::from("defaults: &defaults\n");
        for k in 0..100 {
            writeln!(doc, "  key{k}: value{k}").unwrap();
        }
        doc.push_str("services:\n");
        for i in 0..200 {
            writeln!(doc, "  svc{i}:\n    <<: *defaults\n    name: svc{i}").unwrap();
        }
        let value = parse(&doc).expect("ordinary merge-key reuse is within budget");
        assert_eq!(
            value["services"]["svc199"]["name"],
            serde_yaml::Value::from("svc199")
        );
    }

    #[test]
    fn alias_free_documents_always_fit_the_budget() {
        // Densest node-per-byte shapes: empty flow sequences and implicit nulls.
        for doc in ["[[], [], [], []]", "a:\nb:\nc:\n", "- \n- \n- \n"] {
            assert!(check_alias_expansion(doc).is_ok(), "{doc:?}");
        }
        // The same shapes at the 1 MiB input limit.
        let wide = format!(
            "[{}]",
            vec!["[]"; YAML_PARSE_SIZE_LIMIT_BYTES / 3 - 1].join(",")
        );
        assert!(wide.len() <= YAML_PARSE_SIZE_LIMIT_BYTES);
        assert!(check_alias_expansion(&wide).is_ok());
        let nulls = "- \n".repeat(YAML_PARSE_SIZE_LIMIT_BYTES / 3);
        assert!(check_alias_expansion(&nulls).is_ok());
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
}
