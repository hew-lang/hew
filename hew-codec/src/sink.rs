//! The encode half of the event ABI.

use base64::Engine as _;

use crate::formats::{canonical_cmp, sort_key, write};
use crate::table::{Member, Table};
use crate::value::Value;
use crate::Format;

/// Receives one value's structural events and writes it in one format.
///
/// A value is one scalar event, or a container: `seq_begin`/`set_begin` …
/// `seq_end`, `map_begin` (key, value)* `map_end`, `record_begin` (`field`
/// value)* `record_end`, or `variant` [payload] `variant_end`. A walk that
/// breaks this grammar is a compiler defect; the sink panics rather than
/// write a malformed document.
#[derive(Debug)]
pub struct Sink<'t> {
    format: Format,
    stack: Vec<Frame<'t>>,
    root: Option<Value>,
}

#[derive(Debug)]
enum Frame<'t> {
    Seq {
        items: Vec<Value>,
        set: bool,
    },
    Map {
        entries: Vec<(Value, Value)>,
        key: Option<Value>,
        pairs: bool,
    },
    Record {
        table: Table<'t>,
        fields: Vec<(usize, Value)>,
        field: Option<usize>,
    },
    Variant {
        table: Table<'t>,
        index: usize,
        payload: Option<Value>,
    },
}

impl<'t> Sink<'t> {
    #[must_use]
    pub fn new(format: Format) -> Self {
        Self {
            format,
            stack: Vec::new(),
            root: None,
        }
    }

    fn emit(&mut self, value: Value) {
        match self.stack.last_mut() {
            None => {
                assert!(self.root.is_none(), "hew-codec: a second root value");
                self.root = Some(value);
            }
            Some(Frame::Seq { items, .. }) => items.push(value),
            Some(Frame::Map { entries, key, .. }) => match key.take() {
                Some(key) => entries.push((key, value)),
                None => *key = Some(value),
            },
            Some(Frame::Record { fields, field, .. }) => {
                let index = field
                    .take()
                    .expect("hew-codec: a record value without `field`");
                fields.push((index, value));
            }
            Some(Frame::Variant { payload, .. }) => {
                assert!(payload.is_none(), "hew-codec: a second variant payload");
                *payload = Some(value);
            }
        }
    }

    /// The key a record field or variant is written under.
    fn member_key(&self, table: &Table<'_>, member: &Member<'_>) -> Value {
        if table.tagged && !self.format.is_text() {
            Value::Int(member.tag.into())
        } else {
            Value::Str(member.key.to_owned())
        }
    }

    fn sorted<T>(&self, items: Vec<T>, key: impl Fn(&T) -> &Value) -> Vec<T> {
        let mut keyed: Vec<(Vec<u8>, T)> = items
            .into_iter()
            .map(|item| (sort_key(self.format, key(&item)), item))
            .collect();
        keyed.sort_by(|(a, _), (b, _)| canonical_cmp(self.format, a, b));
        keyed.into_iter().map(|(_, item)| item).collect()
    }

    pub fn null(&mut self) {
        self.emit(Value::Null);
    }

    pub fn bool(&mut self, value: bool) {
        self.emit(Value::Bool(value));
    }

    pub fn i64(&mut self, value: i64) {
        self.emit(Value::Int(value.into()));
    }

    pub fn u64(&mut self, value: u64) {
        self.emit(Value::Int(value.into()));
    }

    /// JSON has no non-finite numbers: they are written as the strings
    /// `"NaN"`, `"Infinity"` and `"-Infinity"`.
    pub fn f64(&mut self, value: f64) {
        let value = if self.format == Format::Json && !value.is_finite() {
            Value::Str(
                if value.is_nan() {
                    "NaN"
                } else if value > 0.0 {
                    "Infinity"
                } else {
                    "-Infinity"
                }
                .to_owned(),
            )
        } else {
            Value::Float(value)
        };
        self.emit(value);
    }

    pub fn str(&mut self, value: &str) {
        self.emit(Value::Str(value.to_owned()));
    }

    /// Text formats write bytes as padded base64 (RFC 4648).
    pub fn bytes(&mut self, value: &[u8]) {
        let value = if self.format.is_text() {
            Value::Str(base64::engine::general_purpose::STANDARD.encode(value))
        } else {
            Value::Bytes(value.to_vec())
        };
        self.emit(value);
    }

    /// A sequence of `len` values (tuple, array, `Vec`).
    pub fn seq_begin(&mut self, len: usize) {
        self.stack.push(Frame::Seq {
            items: Vec::with_capacity(len),
            set: false,
        });
    }

    /// A set: a sequence written in canonical order.
    pub fn set_begin(&mut self, len: usize) {
        self.stack.push(Frame::Seq {
            items: Vec::with_capacity(len),
            set: true,
        });
    }

    pub fn seq_end(&mut self) {
        let Some(Frame::Seq { items, set }) = self.stack.pop() else {
            panic!("hew-codec: `seq_end` without an open sequence");
        };
        let items = if set {
            self.sorted(items, |item| item)
        } else {
            items
        };
        self.emit(Value::Seq(items));
    }

    /// A map of `len` entries, each a key value then a value. Text formats
    /// write a map with `string` keys as an object and any other map as a
    /// sequence of `[key, value]` pairs.
    pub fn map_begin(&mut self, len: usize, string_keys: bool) {
        self.stack.push(Frame::Map {
            entries: Vec::with_capacity(len),
            key: None,
            pairs: self.format.is_text() && !string_keys,
        });
    }

    pub fn map_end(&mut self) {
        let Some(Frame::Map {
            entries,
            key: None,
            pairs,
        }) = self.stack.pop()
        else {
            panic!("hew-codec: `map_end` without an open map or with a dangling key");
        };
        let entries = self.sorted(entries, |(key, _)| key);
        let value = if pairs {
            Value::Seq(
                entries
                    .into_iter()
                    .map(|(key, value)| Value::Seq(vec![key, value]))
                    .collect(),
            )
        } else {
            Value::Map(entries)
        };
        self.emit(value);
    }

    pub fn record_begin(&mut self, table: Table<'t>) {
        self.stack.push(Frame::Record {
            table,
            fields: Vec::with_capacity(table.members.len()),
            field: None,
        });
    }

    /// The next value is field `index` of the open record.
    pub fn field(&mut self, index: usize) {
        match self.stack.last_mut() {
            Some(Frame::Record { table, field, .. })
                if field.is_none() && index < table.members.len() =>
            {
                *field = Some(index);
            }
            _ => panic!("hew-codec: `field` outside a record or out of range"),
        }
    }

    /// Fields are written in the order the walk emitted them (declaration
    /// order), except a tagged record in a binary format, which is written in
    /// ascending tag order. A `null` is omitted for an `OMIT_NULL` field, and
    /// always in TOML, which has no null.
    pub fn record_end(&mut self) {
        let Some(Frame::Record {
            table,
            mut fields,
            field: None,
        }) = self.stack.pop()
        else {
            panic!("hew-codec: `record_end` without an open record or with a dangling field");
        };
        fields.retain(|(index, value)| {
            !(matches!(value, Value::Null)
                && (self.format == Format::Toml || table.members[*index].has(Member::OMIT_NULL)))
        });
        if table.tagged && !self.format.is_text() {
            fields.sort_by_key(|(index, _)| table.members[*index].tag);
        }
        let entries = fields
            .into_iter()
            .map(|(index, value)| (self.member_key(&table, &table.members[index]), value))
            .collect();
        self.emit(Value::Map(entries));
    }

    /// Variant `index` of `table`. A `PAYLOAD` variant is followed by exactly
    /// one payload value (a sequence for several, a record for named fields).
    pub fn variant(&mut self, table: Table<'t>, index: usize) {
        assert!(
            index < table.members.len(),
            "hew-codec: variant index out of range"
        );
        self.stack.push(Frame::Variant {
            table,
            index,
            payload: None,
        });
    }

    /// A unit variant is its key alone; a payload variant is a map of one
    /// entry from its key to the payload.
    pub fn variant_end(&mut self) {
        let Some(Frame::Variant {
            table,
            index,
            payload,
        }) = self.stack.pop()
        else {
            panic!("hew-codec: `variant_end` without an open variant");
        };
        let key = self.member_key(&table, &table.members[index]);
        self.emit(match payload {
            Some(payload) => Value::Map(vec![(key, payload)]),
            None => key,
        });
    }

    /// Replay a dynamic value (`json.Value` and its peers) as events, so it
    /// follows the same format rules as typed data.
    pub fn value(&mut self, value: &Value) {
        match value {
            Value::Null => self.null(),
            Value::Bool(v) => self.bool(*v),
            Value::Int(v) => self.emit(Value::Int(*v)),
            Value::Float(v) => self.f64(*v),
            Value::Str(v) => self.str(v),
            Value::Bytes(v) => self.bytes(v),
            Value::Seq(items) => {
                self.seq_begin(items.len());
                for item in items {
                    self.value(item);
                }
                self.seq_end();
            }
            Value::Map(entries) => {
                let string_keys = entries.iter().all(|(key, _)| matches!(key, Value::Str(_)));
                self.map_begin(entries.len(), string_keys);
                for (key, item) in entries {
                    self.value(key);
                    self.value(item);
                }
                self.map_end();
            }
        }
    }

    /// The written document. Text formats produce UTF-8.
    #[must_use]
    pub fn finish(self) -> Vec<u8> {
        assert!(
            self.stack.is_empty(),
            "hew-codec: `finish` with an open container"
        );
        let root = self.root.expect("hew-codec: `finish` before any value");
        write(self.format, &root)
    }
}
