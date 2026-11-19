//! Owned YAML nodes shared by the native and browser value providers.
//! Hew measures retained map capacity before admitting a mutation. The parser
//! exposes its map entries, so transfer them to an insertion-ordered map that
//! also exposes the allocation. Tags own exactly their retained UTF-8 bytes.

use std::hash::{BuildHasher, Hash, Hasher, RandomState};
use std::ops::Index;
use std::sync::OnceLock;

use ayaml::{Error, ErrorKind, Number, Value as Parsed};

/// A mapping preserves the insertion order of arbitrary YAML keys.
pub type Mapping = indexmap::IndexMap<Value, Value>;

/// A tagged node retains the resolved YAML tag and its inner value.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct TaggedValue {
    pub tag: Box<str>,
    pub value: Value,
}

/// A dynamically typed YAML value.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Value {
    Null,
    Bool(bool),
    Number(Number),
    String(String),
    Sequence(Vec<Self>),
    Mapping(Mapping),
    Tagged(Box<TaggedValue>),
}

impl Hash for Value {
    fn hash<H: Hasher>(&self, state: &mut H) {
        std::mem::discriminant(self).hash(state);
        match self {
            Self::Null => {}
            Self::Bool(v) => v.hash(state),
            Self::Number(v) => v.hash(state),
            Self::String(v) => v.hash(state),
            Self::Sequence(v) => v.hash(state),
            Self::Tagged(v) => v.hash(state),
            Self::Mapping(entries) => {
                // Map equality ignores insertion order, including in a mapping key.
                static ENTRY_HASH: OnceLock<RandomState> = OnceLock::new();
                let entry_hash = ENTRY_HASH.get_or_init(RandomState::new);
                let mut combined = 0u64;
                for entry in entries {
                    combined = combined.wrapping_add(entry_hash.hash_one(entry));
                }
                entries.len().hash(state);
                combined.hash(state);
            }
        }
    }
}

impl Value {
    fn untagged(&self) -> &Self {
        let mut value = self;
        while let Self::Tagged(tagged) = value {
            value = &tagged.value;
        }
        value
    }

    #[must_use]
    pub fn as_bool(&self) -> Option<bool> {
        if let Self::Bool(v) = self.untagged() {
            Some(*v)
        } else {
            None
        }
    }
    #[must_use]
    pub fn as_i64(&self) -> Option<i64> {
        if let Self::Number(v) = self.untagged() {
            v.as_i64()
        } else {
            None
        }
    }
    #[must_use]
    pub fn as_f64(&self) -> Option<f64> {
        if let Self::Number(v) = self.untagged() {
            v.as_f64()
        } else {
            None
        }
    }
    #[must_use]
    pub fn as_str(&self) -> Option<&str> {
        if let Self::String(v) = self.untagged() {
            Some(v)
        } else {
            None
        }
    }
    #[must_use]
    pub fn as_mapping(&self) -> Option<&Mapping> {
        if let Self::Mapping(v) = self.untagged() {
            Some(v)
        } else {
            None
        }
    }
    pub fn as_mapping_mut(&mut self) -> Option<&mut Mapping> {
        let mut value = self;
        while let Self::Tagged(tagged) = value {
            value = &mut tagged.value;
        }
        if let Self::Mapping(v) = value {
            Some(v)
        } else {
            None
        }
    }
    #[must_use]
    pub fn as_sequence(&self) -> Option<&[Self]> {
        if let Self::Sequence(v) = self.untagged() {
            Some(v)
        } else {
            None
        }
    }
}

impl From<i64> for Value {
    fn from(v: i64) -> Self {
        Self::Number(v.into())
    }
}
impl From<f64> for Value {
    fn from(v: f64) -> Self {
        Self::Number(v.into())
    }
}
impl From<&str> for Value {
    fn from(v: &str) -> Self {
        Self::String(v.to_owned())
    }
}

impl Index<&str> for Value {
    type Output = Self;
    fn index(&self, key: &str) -> &Self {
        self.as_mapping()
            .and_then(|entries| entries.get(&Self::from(key)))
            .unwrap_or(&Self::Null)
    }
}

impl TryFrom<Parsed> for Value {
    type Error = Error;

    fn try_from(value: Parsed) -> Result<Self, Error> {
        Ok(match value {
            Parsed::Null => Self::Null,
            Parsed::Bool(v) => Self::Bool(v),
            Parsed::Number(v) if v.is_i64() || v.is_u64() || v.is_f64() => Self::Number(v),
            Parsed::Number(_) => {
                return Err(Error::new(
                    ErrorKind::Serde,
                    "YAML value integer is outside the i64 and u64 ranges",
                ))
            }
            Parsed::String(v) => Self::String(v),
            Parsed::Sequence(v) => Self::Sequence(
                v.into_iter()
                    .map(Self::try_from)
                    .collect::<Result<_, _>>()?,
            ),
            Parsed::Mapping(v) => {
                let mut fields = Mapping::with_capacity(v.len());
                for (key, value) in v {
                    if fields
                        .insert(Self::try_from(key)?, Self::try_from(value)?)
                        .is_some()
                    {
                        return Err(Error::new(ErrorKind::Serde, "duplicate YAML mapping key"));
                    }
                }
                Self::Mapping(fields)
            }
            Parsed::Tagged(v) => {
                let value = Self::try_from(v.value)?;
                if v.tag.handle() == "!" {
                    Self::Tagged(Box::new(TaggedValue {
                        tag: v.tag.to_string().into_boxed_str(),
                        value,
                    }))
                } else {
                    value
                }
            }
        })
    }
}

impl TryFrom<&Value> for Parsed {
    type Error = Error;

    fn try_from(value: &Value) -> Result<Self, Error> {
        value.for_emitter(&ayaml::Limits::default(), 0)
    }
}

impl Value {
    // Values built with array.push or object.set can exceed the parser's depth.
    // Check before descending or allocating the emitter's representation.
    fn for_emitter(&self, limits: &ayaml::Limits, depth: usize) -> Result<Parsed, Error> {
        limits.check_depth(depth)?;
        Ok(match self {
            Value::Null => Parsed::Null,
            Value::Bool(v) => Parsed::Bool(*v),
            Value::Number(v) => Parsed::Number(*v),
            Value::String(v) => Parsed::String(v.clone()),
            Value::Sequence(v) => Parsed::Sequence(
                v.iter()
                    .map(|node| node.for_emitter(limits, depth + 1))
                    .collect::<Result<_, _>>()?,
            ),
            Value::Mapping(v) => Parsed::Mapping(
                v.iter()
                    .map(|(key, value)| {
                        Ok((
                            key.for_emitter(limits, depth + 1)?,
                            value.for_emitter(limits, depth + 1)?,
                        ))
                    })
                    .collect::<Result<_, Error>>()?,
            ),
            Value::Tagged(v) => Parsed::Tagged(Box::new(ayaml::TaggedValue {
                tag: ayaml::Tag::new("", v.tag.as_ref()),
                value: v.value.for_emitter(limits, depth + 1)?,
            })),
        })
    }
}
