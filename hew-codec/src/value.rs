//! The in-memory document tree shared by every format.

use core::cmp::Ordering;
use core::fmt;

use serde::de::{self, DeserializeSeed, MapAccess, SeqAccess, Visitor};
use serde::ser::{SerializeMap, SerializeSeq};

use crate::MAX_DEPTH;

/// A parsed or about-to-be-written document. Map entries keep their input
/// order; the sink orders them canonically before writing.
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Null,
    Bool(bool),
    Int(i128),
    Float(f64),
    Str(String),
    Bytes(Vec<u8>),
    Seq(Vec<Value>),
    Map(Vec<(Value, Value)>),
}

impl Value {
    /// The kind name used in `Type` errors.
    #[must_use]
    pub fn kind(&self) -> &'static str {
        match self {
            Self::Null => "null",
            Self::Bool(_) => "bool",
            Self::Int(_) => "integer",
            Self::Float(_) => "float",
            Self::Str(_) => "string",
            Self::Bytes(_) => "bytes",
            Self::Seq(_) => "sequence",
            Self::Map(_) => "map",
        }
    }

    fn rank(&self) -> u8 {
        match self {
            Self::Null => 0,
            Self::Bool(_) => 1,
            Self::Int(_) => 2,
            Self::Float(_) => 3,
            Self::Str(_) => 4,
            Self::Bytes(_) => 5,
            Self::Seq(_) => 6,
            Self::Map(_) => 7,
        }
    }

    /// A total order over values, used to find duplicate keys.
    pub(crate) fn total_cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (Self::Bool(a), Self::Bool(b)) => a.cmp(b),
            (Self::Int(a), Self::Int(b)) => a.cmp(b),
            (Self::Float(a), Self::Float(b)) => a.total_cmp(b),
            (Self::Str(a), Self::Str(b)) => a.cmp(b),
            (Self::Bytes(a), Self::Bytes(b)) => a.cmp(b),
            (Self::Seq(a), Self::Seq(b)) => cmp_iter(a.iter(), b.iter(), |x, y| x.total_cmp(y)),
            (Self::Map(a), Self::Map(b)) => cmp_iter(a.iter(), b.iter(), |(ak, av), (bk, bv)| {
                ak.total_cmp(bk).then_with(|| av.total_cmp(bv))
            }),
            _ => self.rank().cmp(&other.rank()),
        }
    }

    /// Short rendering of a key for paths and messages.
    pub(crate) fn render_key(&self) -> String {
        match self {
            Self::Str(text) => format!("{text:?}"),
            Self::Int(value) => value.to_string(),
            Self::Bool(value) => value.to_string(),
            other => other.kind().to_string(),
        }
    }
}

fn cmp_iter<T>(
    mut a: impl Iterator<Item = T>,
    mut b: impl Iterator<Item = T>,
    cmp: impl Fn(&T, &T) -> Ordering,
) -> Ordering {
    loop {
        match (a.next(), b.next()) {
            (Some(x), Some(y)) => match cmp(&x, &y) {
                Ordering::Equal => {}
                other => return other,
            },
            (None, None) => return Ordering::Equal,
            (None, Some(_)) => return Ordering::Less,
            (Some(_), None) => return Ordering::Greater,
        }
    }
}

/// The first key that occurs twice in `entries`, if any.
pub(crate) fn duplicate_key(entries: &[(Value, Value)]) -> Option<&Value> {
    let mut order: Vec<usize> = (0..entries.len()).collect();
    order.sort_by(|&a, &b| entries[a].0.total_cmp(&entries[b].0));
    order
        .windows(2)
        .find(|pair| entries[pair[0]].0.total_cmp(&entries[pair[1]].0) == Ordering::Equal)
        .map(|pair| &entries[pair[0]].0)
}

impl serde::Serialize for Value {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        match self {
            Self::Null => serializer.serialize_unit(),
            Self::Bool(value) => serializer.serialize_bool(*value),
            Self::Int(value) => {
                if let Ok(small) = i64::try_from(*value) {
                    serializer.serialize_i64(small)
                } else if let Ok(unsigned) = u64::try_from(*value) {
                    serializer.serialize_u64(unsigned)
                } else {
                    serializer.serialize_i128(*value)
                }
            }
            Self::Float(value) => serializer.serialize_f64(*value),
            Self::Str(value) => serializer.serialize_str(value),
            Self::Bytes(value) => serializer.serialize_bytes(value),
            Self::Seq(items) => {
                let mut seq = serializer.serialize_seq(Some(items.len()))?;
                for item in items {
                    seq.serialize_element(item)?;
                }
                seq.end()
            }
            Self::Map(entries) => {
                let mut map = serializer.serialize_map(Some(entries.len()))?;
                for (key, value) in entries {
                    map.serialize_entry(key, value)?;
                }
                map.end()
            }
        }
    }
}

/// Deserializes one value from a serde-based parser, refusing duplicate keys
/// and nesting deeper than [`MAX_DEPTH`]. `depth` counts enclosing containers.
#[derive(Clone, Copy)]
pub(crate) struct ValueSeed {
    pub depth: usize,
}

impl ValueSeed {
    fn enter<E: de::Error>(self) -> Result<Self, E> {
        if self.depth >= MAX_DEPTH {
            return Err(E::custom(format_args!(
                "nesting exceeds the limit of {MAX_DEPTH}"
            )));
        }
        Ok(Self {
            depth: self.depth + 1,
        })
    }
}

impl<'de> DeserializeSeed<'de> for ValueSeed {
    type Value = Value;

    fn deserialize<D: de::Deserializer<'de>>(self, deserializer: D) -> Result<Value, D::Error> {
        deserializer.deserialize_any(self)
    }
}

impl<'de> Visitor<'de> for ValueSeed {
    type Value = Value;

    fn expecting(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str("a data value")
    }

    fn visit_unit<E>(self) -> Result<Value, E> {
        Ok(Value::Null)
    }

    fn visit_none<E>(self) -> Result<Value, E> {
        Ok(Value::Null)
    }

    fn visit_some<D: de::Deserializer<'de>>(self, deserializer: D) -> Result<Value, D::Error> {
        self.deserialize(deserializer)
    }

    fn visit_newtype_struct<D: de::Deserializer<'de>>(
        self,
        deserializer: D,
    ) -> Result<Value, D::Error> {
        self.deserialize(deserializer)
    }

    fn visit_bool<E>(self, value: bool) -> Result<Value, E> {
        Ok(Value::Bool(value))
    }

    fn visit_i64<E>(self, value: i64) -> Result<Value, E> {
        Ok(Value::Int(value.into()))
    }

    fn visit_u64<E>(self, value: u64) -> Result<Value, E> {
        Ok(Value::Int(value.into()))
    }

    fn visit_i128<E>(self, value: i128) -> Result<Value, E> {
        Ok(Value::Int(value))
    }

    fn visit_u128<E: de::Error>(self, value: u128) -> Result<Value, E> {
        i128::try_from(value)
            .map(Value::Int)
            .map_err(|_| E::custom("integer exceeds 128 bits"))
    }

    fn visit_f64<E>(self, value: f64) -> Result<Value, E> {
        Ok(Value::Float(value))
    }

    fn visit_char<E>(self, value: char) -> Result<Value, E> {
        Ok(Value::Str(value.to_string()))
    }

    fn visit_str<E>(self, value: &str) -> Result<Value, E> {
        Ok(Value::Str(value.to_owned()))
    }

    fn visit_string<E>(self, value: String) -> Result<Value, E> {
        Ok(Value::Str(value))
    }

    fn visit_bytes<E>(self, value: &[u8]) -> Result<Value, E> {
        Ok(Value::Bytes(value.to_vec()))
    }

    fn visit_byte_buf<E>(self, value: Vec<u8>) -> Result<Value, E> {
        Ok(Value::Bytes(value))
    }

    fn visit_seq<A: SeqAccess<'de>>(self, mut seq: A) -> Result<Value, A::Error> {
        let inner = self.enter()?;
        let mut items = Vec::with_capacity(seq.size_hint().unwrap_or(0).min(4096));
        while let Some(item) = seq.next_element_seed(inner)? {
            items.push(item);
        }
        Ok(Value::Seq(items))
    }

    fn visit_map<A: MapAccess<'de>>(self, mut map: A) -> Result<Value, A::Error> {
        let inner = self.enter()?;
        let mut entries = Vec::with_capacity(map.size_hint().unwrap_or(0).min(4096));
        while let Some(key) = map.next_key_seed(inner)? {
            let value = map.next_value_seed(inner)?;
            entries.push((key, value));
        }
        if let Some(key) = duplicate_key(&entries) {
            return Err(de::Error::custom(format_args!(
                "duplicate key {}",
                key.render_key()
            )));
        }
        Ok(Value::Map(entries))
    }
}
