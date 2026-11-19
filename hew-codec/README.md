# hew-codec

The one serialization engine behind Hew's `json`, `yaml`, `toml`, `msgpack`
and `cbor` modules. A compiled per-type walk drives a `Sink` with structural
events and pulls from a `Source`; each format only spells those events.
Map and set output is in canonical order, duplicate keys are refused on every
input, and every `DecodeError` carries the path of the failing value.

Ordinary YAML values for `std.encoding.yaml` are available through
`hew_codec::yaml_value::{parse, to_string, Value}`. Their mappings retain
insertion order and report the reserved capacity used by Hew's resource
accounting. Rust consumers moving from `serde_yaml::Value` can convert the
result to `ayaml::Value` with `ayaml::Value::from(&value)`.
Tag strings are stored at their exact size on construction. Callers can remove
calls to the previous `managed_tree::normalize_yaml_tags` helper.
