//! Ordinary YAML Value parsing, distinct from the strict typed wire codec.
//! Native and sandbox values share the same input limits and parser diagnostics.

mod tree;
pub use ayaml::{Error, Number};
pub use tree::{Mapping, TaggedValue, Value};

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
pub fn parse(input: &str) -> Result<Value, String> {
    validate_yaml_input_limits(input)?;
    decode::<ayaml::Value>(input)
        .and_then(Value::try_from)
        .map_err(|error| error.to_string())
}

/// Encode an ordinary YAML value.
///
/// # Errors
///
/// Returns the emitter diagnostic for an unsupported value or size limit.
pub fn to_string(value: &Value) -> Result<String, Error> {
    encode(&ayaml::Value::try_from(value)?)
}

pub(crate) fn encode(value: &ayaml::Value) -> Result<String, Error> {
    let scalar = match value {
        ayaml::Value::Tagged(tagged) => &tagged.value,
        value => value,
    };
    // ayaml's root block indent is relative to column -1. The libyaml reader
    // counts from column 0, so an explicit indent would eat a content column.
    match scalar {
        ayaml::Value::String(text)
            if text.contains('\n') && text.trim_start_matches('\n').starts_with([' ', '\t']) =>
        {
            let options = ayaml::EmitterOptions::default()
                .with_scalar_style(Some(ayaml::ScalarStyle::DoubleQuoted));
            ayaml::to_string_value_with_options(value, &options)
        }
        _ => ayaml::to_string_value(value),
    }
}

/// Expansion units any one document may use. Each expanded node costs at
/// least sixteen units in addition to its scalar or tag text.
pub const YAML_EXPANSION_BUDGET_UNITS: usize = 16 * YAML_PARSE_SIZE_LIMIT_BYTES;
const YAML_NODE_COST: usize = 16;

fn limits() -> ayaml::Limits {
    ayaml::Limits::default()
        .with_node_cost_bytes(YAML_NODE_COST)
        .with_max_events(3 * YAML_PARSE_SIZE_LIMIT_BYTES + 4)
        .with_max_alias_expansion_bytes(YAML_EXPANSION_BUDGET_UNITS)
        .with_alias_bytes_per_input_byte(0)
        .with_max_collection_entries(YAML_EXPANSION_BUDGET_UNITS / YAML_NODE_COST)
        .with_max_alias_dereferences(YAML_EXPANSION_BUDGET_UNITS / YAML_NODE_COST)
}

pub(crate) fn decode<'de, T: serde::Deserialize<'de>>(input: &'de str) -> Result<T, Error> {
    // A document inside the input cap fits the expansion budget on its own.
    // Every YAML alias starts with a literal asterisk.
    if input.len() > YAML_PARSE_SIZE_LIMIT_BYTES || input.as_bytes().contains(&b'*') {
        check_alias_expansion(input)?;
    }
    ayaml::compat::from_str_with_limits(input, &limits()).map_err(expansion_error)
}

fn expansion_error(error: Error) -> Error {
    if error
        .limit()
        .is_some_and(|limit| limit.kind() == ayaml::LimitKind::AliasExpansion)
    {
        let normalized = Error::new(
            ayaml::ErrorKind::Limit,
            "invalid YAML input: alias expansion limit exceeded",
        );
        return match error.span() {
            Some(span) => normalized.with_span(span),
            None => normalized,
        };
    }
    error
}

/// Validate the syntax and alias expansion budget of a single document.
///
/// # Errors
///
/// Returns the parser diagnostic or the exceeded resource limit.
pub fn check_alias_expansion(input: &str) -> Result<(), Error> {
    let left = Cell::new(YAML_EXPANSION_BUDGET_UNITS);
    ayaml::compat::from_str_seed_with_limits(input, &limits(), Walk { left: &left })
        .map_err(expansion_error)
}

/// A visitor that keeps nothing and charges every node it is shown.
#[derive(Clone, Copy)]
struct Walk<'b> {
    left: &'b Cell<usize>,
}

impl Walk<'_> {
    fn charge<E: de::Error>(self, scalar_len: usize) -> Result<(), E> {
        let cost = YAML_NODE_COST.saturating_add(scalar_len);
        if let Some(left) = self.left.get().checked_sub(cost) {
            self.left.set(left);
            Ok(())
        } else {
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

    fn visit_i128<E: de::Error>(self, _: i128) -> Result<(), E> {
        self.charge(0)
    }

    fn visit_u128<E: de::Error>(self, _: u128) -> Result<(), E> {
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

    #[test]
    fn encoding_constructed_values_checks_depth_before_copying() {
        let bound = ayaml::Limits::default().max_depth;
        let at_limit = (0..bound).fold(Value::Null, |child, _| Value::Sequence(vec![child]));
        assert!(ayaml::Value::try_from(&at_limit).is_ok());
        let above_limit = Value::Sequence(vec![at_limit]);
        let error =
            to_string(&above_limit).expect_err("constructed value exceeds the emitter depth");
        assert_eq!(error.limit().unwrap().kind(), ayaml::LimitKind::Depth);
    }

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
        assert_eq!(value["svc7"]["name"], Value::from("svc7"));
    }

    #[test]
    fn recursive_aliases_fail_the_walk() {
        for doc in [
            "&loop [*loop]",
            "&loop {again: *loop}",
            "&loop !Node {again: *loop}",
        ] {
            assert!(check_alias_expansion(doc).is_err(), "{doc:?}");
            assert!(parse(doc).is_err(), "{doc:?}");
        }
    }

    #[test]
    fn parser_errors_fail_the_walk() {
        for doc in ["[unfinished", "*missing", "---\nfirst\n---\nsecond\n"] {
            assert!(check_alias_expansion(doc).is_err(), "{doc:?}");
        }
    }

    #[test]
    fn scalar_and_tagged_aliases_still_parse() {
        for doc in [
            "[&scalar hello, *scalar]",
            "[&scalar !Name hello, *scalar]",
            "[&list !Items [1, null, true], *list]",
            "[&map !Settings {retries: 3}, *map]",
        ] {
            assert_eq!(
                parse(doc).expect("ordinary aliases remain supported"),
                Value::try_from(ayaml::compat::from_str::<ayaml::Value>(doc).unwrap()).unwrap(),
                "{doc:?}"
            );
        }
    }

    #[test]
    fn wide_integer_aliases_still_parse_in_the_wire_codec() {
        for integer in [i128::MIN, i128::MAX] {
            let doc = format!("[&wide {integer}, *wide]");
            let value = crate::Source::new(crate::Format::Yaml, doc.as_bytes())
                .expect("the wire codec supports wide integers")
                .read_any()
                .unwrap();
            assert_eq!(
                value,
                crate::Value::Seq(vec![crate::Value::Int(integer); 2])
            );
        }
        // The walk counts all scalar types, even ones a later reader rejects.
        assert!(check_alias_expansion(&u128::MAX.to_string()).is_ok());
    }

    #[test]
    fn wide_integers_do_not_bypass_expansion_accounting() {
        let doc = format!("wide: {}\n{}", i128::MAX, nested_fan_out(400, 400));
        let err = check_alias_expansion(&doc).expect_err("the entire document is checked");
        assert!(err.to_string().contains("alias expansion limit"), "{err}");
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
        assert_eq!(value["services"]["svc199"]["name"], Value::from("svc199"));
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
