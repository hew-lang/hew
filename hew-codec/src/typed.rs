//! Execute a checked portable shape through the ordinary format event ABI.
//!
//! The bridge value is positional JSON. Integers and floats travel as strings,
//! bytes as octets, and variants as `[declaration_index, payload]`. This is an
//! internal value transport, not a serialization format.

use serde_json::{json, Value as Bridge};

use crate::property::{Key, Node, Shape};
use crate::{DecodeError, Format, Member, Sink, Source, Table};

fn duplicate(source: &Source<'_>) -> Bridge {
    let DecodeError::Duplicate { path, key } = source.duplicate() else {
        unreachable!()
    };
    json!([path, key])
}

#[expect(
    clippy::cast_possible_truncation,
    reason = "f32 codec values narrow to the declared IEEE width"
)]
fn float(bits: u8, value: f64) -> Result<f64, DecodeError> {
    match bits {
        32 => Ok(f64::from(value as f32)),
        64 => Ok(value),
        _ => Err(invalid("invalid float width")),
    }
}

fn invalid(reason: impl Into<String>) -> DecodeError {
    DecodeError::Invalid {
        path: String::new(),
        reason: reason.into(),
    }
}

struct Walk<'a> {
    shape: &'a Shape,
    tables: Vec<Vec<Member<'a>>>,
}

impl<'a> Walk<'a> {
    fn new(shape: &'a Shape) -> Result<Self, DecodeError> {
        let count = shape.nodes.len();
        if shape.root >= count
            || shape.names.len() != count
            || shape.aggregates.len() != count
            || shape.variants.len() != count
            || shape.grouped_payloads.len() != count
        {
            return Err(invalid("typed shape metadata is incomplete"));
        }
        for node in &shape.nodes {
            let children: Vec<usize> = match node {
                Node::Tuple(children) => children.clone(),
                Node::Array { element, .. }
                | Node::Sequence(element)
                | Node::Set(element)
                | Node::Option(element) => vec![*element],
                Node::Map { key, value } => vec![*key, *value],
                Node::Record(fields) => fields.iter().map(|(_, child)| *child).collect(),
                Node::Variant(cases) => {
                    if cases
                        .iter()
                        .any(|(key, child)| (key.flags & Member::PAYLOAD != 0) != child.is_some())
                    {
                        return Err(invalid("typed variant payload disagrees with its table"));
                    }
                    cases.iter().filter_map(|(_, child)| *child).collect()
                }
                _ => Vec::new(),
            };
            if children.iter().any(|child| *child >= count) {
                return Err(invalid("typed shape child is absent"));
            }
        }
        let tables = shape
            .nodes
            .iter()
            .map(|node| {
                let keys: Vec<&Key> = match node {
                    Node::Record(fields) => fields.iter().map(|(key, _)| key).collect(),
                    Node::Variant(cases) => cases.iter().map(|(key, _)| key).collect(),
                    _ => Vec::new(),
                };
                keys.into_iter()
                    .map(|key| Member::new(&key.name, key.tag.unwrap_or(0), key.flags))
                    .collect()
            })
            .collect();
        Ok(Self { shape, tables })
    }

    fn node(&self, id: usize, depth: usize) -> Result<&Node, DecodeError> {
        if depth > crate::MAX_DEPTH {
            return Err(invalid("typed value exceeds nesting limit"));
        }
        self.shape
            .nodes
            .get(id)
            .ok_or_else(|| invalid("typed shape child is absent"))
    }

    fn table(&self, id: usize) -> Table<'_> {
        let tagged = match &self.shape.nodes[id] {
            Node::Record(fields) => fields.first().is_some_and(|(key, _)| key.tag.is_some()),
            Node::Variant(cases) => cases.first().is_some_and(|(key, _)| key.tag.is_some()),
            _ => false,
        };
        Table::new(&self.tables[id], tagged)
    }

    #[expect(
        clippy::too_many_lines,
        reason = "one exhaustive walk keeps format events aligned with every checked shape"
    )]
    fn encode<'w>(
        &'w self,
        sink: &mut Sink<'w>,
        id: usize,
        value: &Bridge,
        depth: usize,
    ) -> Result<(), DecodeError> {
        let array = || {
            value
                .as_array()
                .ok_or_else(|| invalid("typed value is not positional"))
        };
        let text = || {
            value
                .as_str()
                .ok_or_else(|| invalid("typed scalar is not text"))
        };
        match self.node(id, depth)? {
            Node::Unit => sink.null(),
            Node::Bool => sink.bool(
                value
                    .as_bool()
                    .ok_or_else(|| invalid("typed bool is absent"))?,
            ),
            Node::Signed { min, max } => {
                let n: i64 = text()?
                    .parse()
                    .map_err(|_| invalid("invalid signed integer"))?;
                if n < *min || n > *max {
                    return Err(invalid("typed integer is out of range"));
                }
                sink.i64(n);
            }
            Node::Unsigned { max } => {
                let n: u64 = text()?
                    .parse()
                    .map_err(|_| invalid("invalid unsigned integer"))?;
                if n > *max {
                    return Err(invalid("typed integer is out of range"));
                }
                sink.u64(n);
            }
            Node::Float { bits } => sink.f64(float(
                *bits,
                text()?.parse().map_err(|_| invalid("invalid float"))?,
            )?),
            Node::Char | Node::String => sink.str(text()?),
            Node::Bytes => {
                let bytes: Result<Vec<u8>, _> = array()?
                    .iter()
                    .map(|byte| {
                        byte.as_u64()
                            .and_then(|n| u8::try_from(n).ok())
                            .ok_or_else(|| invalid("invalid octet"))
                    })
                    .collect();
                sink.bytes(&bytes?);
            }
            Node::Tuple(children) => {
                let values = array()?;
                if values.len() != children.len() {
                    return Err(invalid("typed tuple length mismatch"));
                }
                sink.seq_begin(values.len());
                for (child, value) in children.iter().zip(values) {
                    self.encode(sink, *child, value, depth + 1)?;
                }
                sink.seq_end();
            }
            Node::Array { element, len } => {
                let values = array()?;
                if values.len() != *len {
                    return Err(invalid("typed array length mismatch"));
                }
                sink.seq_begin(values.len());
                for value in values {
                    self.encode(sink, *element, value, depth + 1)?;
                }
                sink.seq_end();
            }
            Node::Sequence(element) | Node::Set(element) => {
                let values = array()?;
                if matches!(self.shape.nodes[id], Node::Set(_)) {
                    sink.set_begin(values.len());
                } else {
                    sink.seq_begin(values.len());
                }
                for value in values {
                    let value = if matches!(self.shape.nodes[id], Node::Set(_)) {
                        value
                            .as_array()
                            .and_then(|pair| pair.first())
                            .ok_or_else(|| invalid("invalid set entry"))?
                    } else {
                        value
                    };
                    self.encode(sink, *element, value, depth + 1)?;
                }
                sink.seq_end();
            }
            Node::Map { key, value: item } => {
                let entries = array()?;
                sink.map_begin(
                    entries.len(),
                    matches!(self.shape.nodes[*key], Node::String),
                );
                for entry in entries {
                    let pair = entry
                        .as_array()
                        .filter(|p| p.len() >= 2)
                        .ok_or_else(|| invalid("invalid map pair"))?;
                    self.encode(sink, *key, &pair[0], depth + 1)?;
                    self.encode(sink, *item, &pair[1], depth + 1)?;
                }
                sink.map_end();
            }
            Node::Record(fields) => {
                let values = array()?;
                if values.len() != fields.len() {
                    return Err(invalid("typed record length mismatch"));
                }
                sink.record_begin(self.table(id));
                for (index, ((key, child), value)) in fields.iter().zip(values).enumerate() {
                    if key.flags & Member::SKIP != 0 {
                        continue;
                    }
                    sink.field(index);
                    self.encode(sink, *child, value, depth + 1)?;
                }
                sink.record_end();
            }
            Node::Variant(cases) => {
                let pair = array()?;
                let tag = pair
                    .first()
                    .and_then(Bridge::as_u64)
                    .and_then(|tag| usize::try_from(tag).ok())
                    .ok_or_else(|| invalid("invalid variant index"))?;
                let (_, child) = cases
                    .get(tag)
                    .ok_or_else(|| invalid("variant index out of range"))?;
                sink.variant(self.table(id), tag);
                if let Some(child) = child {
                    self.encode(
                        sink,
                        *child,
                        pair.get(1)
                            .ok_or_else(|| invalid("variant payload absent"))?,
                        depth + 1,
                    )?;
                }
                sink.variant_end();
            }
            Node::Option(child) => {
                if value.is_null() {
                    sink.null();
                } else {
                    let values = array()?;
                    self.encode(
                        sink,
                        *child,
                        values
                            .first()
                            .ok_or_else(|| invalid("Option payload absent"))?,
                        depth + 1,
                    )?;
                }
            }
        }
        Ok(())
    }

    fn decode<'w>(
        &'w self,
        source: &mut Source<'w>,
        id: usize,
        depth: usize,
        error: &mut Option<DecodeError>,
    ) -> Bridge {
        match self.decode_inner(source, id, depth, error) {
            Ok(value) => value,
            Err(failure) => {
                *error = Some(failure);
                json!({"decode_error": true})
            }
        }
    }

    #[expect(
        clippy::too_many_lines,
        reason = "the ordered walk retains successful prefixes before the first decode error"
    )]
    fn decode_inner<'w>(
        &'w self,
        source: &mut Source<'w>,
        id: usize,
        depth: usize,
        error: &mut Option<DecodeError>,
    ) -> Result<Bridge, DecodeError> {
        Ok(match self.node(id, depth)? {
            Node::Unit => {
                source.read_unit()?;
                Bridge::Null
            }
            Node::Bool => json!(source.read_bool()?),
            Node::Signed { min, max } => json!(source.read_i64(*min, *max)?.to_string()),
            Node::Unsigned { max } => json!(source.read_u64(*max)?.to_string()),
            Node::Float { bits } => json!(float(*bits, source.read_f64()?)?.to_string()),
            Node::Char => json!(source.read_char()?.to_string()),
            Node::String => json!(source.read_str()?),
            Node::Bytes => json!(source.read_bytes()?),
            Node::Tuple(children) => {
                source.tuple_begin(children.len())?;
                let mut values = Vec::with_capacity(children.len());
                for child in children {
                    source.seq_next()?;
                    values.push(self.decode(source, *child, depth + 1, error));
                    if error.is_some() {
                        break;
                    }
                }
                if error.is_none() {
                    source.seq_next()?;
                }
                Bridge::Array(values)
            }
            Node::Array { element, len } => {
                source.tuple_begin(*len)?;
                let mut values = Vec::with_capacity(*len);
                while error.is_none() && source.seq_next()? {
                    values.push(self.decode(source, *element, depth + 1, error));
                }
                Bridge::Array(values)
            }
            Node::Sequence(element) | Node::Set(element) => {
                let set = matches!(self.shape.nodes[id], Node::Set(_));
                if set {
                    source.set_begin()?;
                } else {
                    source.seq_begin()?;
                }
                let mut values = Vec::new();
                while error.is_none() && source.seq_next()? {
                    let value = self.decode(source, *element, depth + 1, error);
                    values.push(if set {
                        json!([value, duplicate(source)])
                    } else {
                        value
                    });
                }
                Bridge::Array(values)
            }
            Node::Map { key, value } => {
                source.map_begin(matches!(self.shape.nodes[*key], Node::String))?;
                let mut pairs = Vec::new();
                while error.is_none() && source.map_next()? {
                    let key = self.decode(source, *key, depth + 1, error);
                    let value = if error.is_none() {
                        self.decode(source, *value, depth + 1, error)
                    } else {
                        Bridge::Null
                    };
                    pairs.push(json!([key, value, duplicate(source)]));
                }
                Bridge::Array(pairs)
            }
            Node::Record(fields) => {
                source.record_begin(self.table(id))?;
                let mut values = Vec::with_capacity(fields.len());
                for (_, child) in fields {
                    if let Err(failure) = source.record_next() {
                        *error = Some(failure);
                        values.push(json!({"decode_error": true}));
                        break;
                    }
                    values.push(self.decode(source, *child, depth + 1, error));
                    if error.is_some() {
                        break;
                    }
                }
                if error.is_none() {
                    source.record_next()?;
                }
                Bridge::Array(values)
            }
            Node::Variant(cases) => {
                let tag = source.variant(self.table(id))?;
                let payload = if let Some(child) = cases[tag].1 {
                    self.decode(source, child, depth + 1, error)
                } else {
                    Bridge::Null
                };
                if error.is_none() {
                    source.variant_end()?;
                }
                json!([tag, payload])
            }
            Node::Option(child) => {
                if source.is_null()? {
                    Bridge::Null
                } else {
                    json!([self.decode(source, *child, depth + 1, error)])
                }
            }
        })
    }
}

/// Encode a positional bridge value through the same sink as native code.
///
/// # Errors
/// Returns an invalid-value error for a malformed bridge value.
pub fn encode(shape: &Shape, format: Format, value: &Bridge) -> Result<Vec<u8>, DecodeError> {
    let walk = Walk::new(shape)?;
    let mut sink = Sink::new(format);
    walk.encode(&mut sink, shape.root, value, 0)?;
    Ok(sink.finish())
}

/// An ordered decoded prefix. A `decode_error` marker occupies the first
/// failing value; the VM can execute earlier selected collection callbacks
/// before reporting `error`, exactly as the native walk does.
#[derive(Debug)]
pub struct Decoded {
    pub value: Bridge,
    pub error: Option<DecodeError>,
}

/// Decode bytes through the same source and field tables as native code.
///
/// # Errors
/// Returns an invalid shape or format syntax error. Typed failures are retained
/// in the ordered result, after any successfully decoded prefix.
pub fn decode(shape: &Shape, format: Format, bytes: &[u8]) -> Result<Decoded, DecodeError> {
    let walk = Walk::new(shape)?;
    let mut error = None;
    let value = walk.decode(&mut Source::new(format, bytes)?, shape.root, 0, &mut error);
    Ok(Decoded { value, error })
}

/// Decode an already parsed source-language value, including codec errors.
///
/// # Errors
/// Returns an invalid shape error. Typed failures are retained in the result.
pub fn decode_value(
    shape: &Shape,
    format: Format,
    value: crate::Value,
) -> Result<Decoded, DecodeError> {
    let walk = Walk::new(shape)?;
    let mut error = None;
    let value = walk.decode(
        &mut Source::from_value(format, value),
        shape.root,
        0,
        &mut error,
    );
    Ok(Decoded { value, error })
}
