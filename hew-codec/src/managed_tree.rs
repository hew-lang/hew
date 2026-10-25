//! Canonical managed-payload facts shared by native and browser encoding values.

/// Canonical width of a retained string or collection handle.
pub const HANDLE_BYTES: u64 = 8;
/// Four-byte discriminant plus an eight-byte scalar or backing handle.
pub const JSON_NODE_BYTES: u64 = 12;
/// Four-byte discriminant plus an eight-byte scalar or backing handle.
pub const YAML_NODE_BYTES: u64 = 12;
/// Date presence, time-field presence and offset kind (three bytes), year
/// (u16), month/day/hour/minute/second (five u8s), nanoseconds (u32), and
/// offset minutes (i16). Missing fields retain their canonical slot.
pub const TOML_DATETIME_BYTES: u64 = 16;
/// Four-byte discriminant plus the largest typed payload: a datetime.
pub const TOML_NODE_BYTES: u64 = 4 + TOML_DATETIME_BYTES;

/// Provider facts projected into sandbox managed-type rows.
#[derive(Debug, Clone, Copy)]
pub struct ManagedTreeLayout {
    /// Width of one inline format value, including its discriminant.
    pub node_bytes: u64,
    /// Width of a canonical backing handle.
    pub handle_bytes: u64,
    /// Inline width of one map key (a string handle or a YAML value).
    pub map_key_bytes: u64,
    /// Whether reserved map entry capacity is exposed by this provider.
    pub map_capacity: bool,
    /// Inline payload of a boxed tagged node, excluding tag UTF-8.
    pub tagged_bytes: u64,
    /// Whether semantic copies retain the same immutable whole tree.
    pub shared_copy: bool,
}

/// JSON trees own their nodes and B-tree entries independently.
pub const JSON_LAYOUT: ManagedTreeLayout = ManagedTreeLayout {
    node_bytes: JSON_NODE_BYTES,
    handle_bytes: HANDLE_BYTES,
    map_key_bytes: HANDLE_BYTES,
    map_capacity: false,
    tagged_bytes: 0,
    shared_copy: false,
};

/// YAML trees support arbitrary typed keys, retained map capacity and tags.
pub const YAML_LAYOUT: ManagedTreeLayout = ManagedTreeLayout {
    node_bytes: YAML_NODE_BYTES,
    handle_bytes: HANDLE_BYTES,
    map_key_bytes: YAML_NODE_BYTES,
    map_capacity: true,
    tagged_bytes: HANDLE_BYTES + YAML_NODE_BYTES,
    shared_copy: true,
};

/// TOML trees include typed datetime slots and string-keyed B-tree entries.
pub const TOML_LAYOUT: ManagedTreeLayout = ManagedTreeLayout {
    node_bytes: TOML_NODE_BYTES,
    handle_bytes: HANDLE_BYTES,
    map_key_bytes: HANDLE_BYTES,
    map_capacity: false,
    tagged_bytes: 0,
    shared_copy: false,
};

fn slots(count: usize, width: u64) -> Option<u64> {
    u64::try_from(count).ok()?.checked_mul(width)
}

fn text(value: &String, copy: bool) -> Option<u64> {
    slots(if copy { value.len() } else { value.capacity() }, 1)
}

/// Measure a JSON tree, including its root and retained typed capacities.
/// A copy plan measures the capacities produced by its independent clone.
#[must_use]
pub fn json_payload_bytes(value: &serde_json::Value, copy: bool) -> Option<u64> {
    fn descendants(value: &serde_json::Value, copy: bool) -> Option<u64> {
        match value {
            serde_json::Value::String(value) => text(value, copy),
            serde_json::Value::Array(values) => {
                let mut bytes = slots(
                    if copy {
                        values.len()
                    } else {
                        values.capacity()
                    },
                    JSON_NODE_BYTES,
                )?;
                for value in values {
                    bytes = bytes.checked_add(descendants(value, copy)?)?;
                }
                Some(bytes)
            }
            serde_json::Value::Object(values) => {
                let mut bytes = slots(values.len(), HANDLE_BYTES + JSON_NODE_BYTES)?;
                for (key, value) in values {
                    bytes = bytes.checked_add(text(key, copy)?)?;
                    bytes = bytes.checked_add(descendants(value, copy)?)?;
                }
                Some(bytes)
            }
            serde_json::Value::Null | serde_json::Value::Bool(_) | serde_json::Value::Number(_) => {
                Some(0)
            }
        }
    }
    JSON_NODE_BYTES.checked_add(descendants(value, copy)?)
}

/// Measure a TOML tree, including typed datetime slots and retained capacities.
/// A copy plan measures the capacities produced by its independent clone.
#[must_use]
pub fn toml_payload_bytes(value: &toml::Value, copy: bool) -> Option<u64> {
    fn descendants(value: &toml::Value, copy: bool) -> Option<u64> {
        match value {
            toml::Value::String(value) => text(value, copy),
            toml::Value::Array(values) => {
                let mut bytes = slots(
                    if copy {
                        values.len()
                    } else {
                        values.capacity()
                    },
                    TOML_NODE_BYTES,
                )?;
                for value in values {
                    bytes = bytes.checked_add(descendants(value, copy)?)?;
                }
                Some(bytes)
            }
            toml::Value::Table(values) => {
                let mut bytes = slots(values.len(), HANDLE_BYTES + TOML_NODE_BYTES)?;
                for (key, value) in values {
                    bytes = bytes.checked_add(text(key, copy)?)?;
                    bytes = bytes.checked_add(descendants(value, copy)?)?;
                }
                Some(bytes)
            }
            toml::Value::Integer(_)
            | toml::Value::Float(_)
            | toml::Value::Boolean(_)
            | toml::Value::Datetime(_) => Some(0),
        }
    }
    TOML_NODE_BYTES.checked_add(descendants(value, copy)?)
}

/// Normalize private YAML tag strings to exact-capacity canonical text once
/// at parsing. Copies and subtree extraction preserve this invariant.
pub fn normalize_yaml_tags(value: &mut serde_yaml::Value) {
    use serde_yaml::Value;
    match value {
        Value::Tagged(tagged) => {
            tagged.tag =
                serde_yaml::value::Tag::new(tagged.tag.to_string().into_boxed_str().into_string());
            normalize_yaml_tags(&mut tagged.value);
        }
        Value::Sequence(values) => values.iter_mut().for_each(normalize_yaml_tags),
        Value::Mapping(values) => {
            if values.keys().any(contains_tag) {
                let mut normalized = serde_yaml::Mapping::with_capacity(values.capacity());
                for (mut key, mut value) in std::mem::take(values) {
                    normalize_yaml_tags(&mut key);
                    normalize_yaml_tags(&mut value);
                    normalized.insert(key, value);
                }
                *values = normalized;
            } else {
                values.values_mut().for_each(normalize_yaml_tags);
            }
        }
        Value::Null | Value::Bool(_) | Value::Number(_) | Value::String(_) => {}
    }
}

fn contains_tag(value: &serde_yaml::Value) -> bool {
    match value {
        serde_yaml::Value::Tagged(_) => true,
        serde_yaml::Value::Sequence(values) => values.iter().any(contains_tag),
        serde_yaml::Value::Mapping(values) => values
            .iter()
            .any(|(key, value)| contains_tag(key) || contains_tag(value)),
        _ => false,
    }
}

fn tag_bytes(tag: &serde_yaml::value::Tag) -> Option<u64> {
    use std::fmt::Write as _;
    struct Counter(u64);
    impl std::fmt::Write for Counter {
        fn write_str(&mut self, value: &str) -> std::fmt::Result {
            self.0 = self
                .0
                .checked_add(value.len() as u64)
                .ok_or(std::fmt::Error)?;
            Ok(())
        }
    }
    let mut counter = Counter(0);
    write!(counter, "{tag}").ok()?;
    Some(counter.0)
}

/// Measure a normalized YAML tree, including its root and retained capacities.
/// Logical YAML copies share this whole tree until mutation detaches it.
#[must_use]
pub fn yaml_payload_bytes(value: &serde_yaml::Value) -> Option<u64> {
    fn descendants(value: &serde_yaml::Value) -> Option<u64> {
        match value {
            serde_yaml::Value::String(value) => text(value, false),
            serde_yaml::Value::Sequence(values) => {
                let mut bytes = slots(values.capacity(), YAML_NODE_BYTES)?;
                for value in values {
                    bytes = bytes.checked_add(descendants(value)?)?;
                }
                Some(bytes)
            }
            serde_yaml::Value::Mapping(values) => {
                let mut bytes = slots(values.capacity(), 2 * YAML_NODE_BYTES)?;
                for (key, value) in values {
                    bytes = bytes.checked_add(descendants(key)?)?;
                    bytes = bytes.checked_add(descendants(value)?)?;
                }
                Some(bytes)
            }
            serde_yaml::Value::Tagged(tagged) => YAML_LAYOUT
                .tagged_bytes
                .checked_add(tag_bytes(&tagged.tag)?)?
                .checked_add(descendants(&tagged.value)?),
            serde_yaml::Value::Null | serde_yaml::Value::Bool(_) | serde_yaml::Value::Number(_) => {
                Some(0)
            }
        }
    }
    YAML_NODE_BYTES.checked_add(descendants(value)?)
}
