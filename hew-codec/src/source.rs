//! The decode half of the event ABI.

use base64::Engine as _;

use crate::formats::parse;
use crate::table::{Member, Table};
use crate::value::{duplicate_key, Value};
use crate::{DecodeError, Format};

/// Hands a parsed document to a decode walk one pull at a time, tracking the
/// path of the value being read so every error names it.
///
/// Pulls follow the same grammar as [`crate::Sink`] events. A record hands
/// out every member exactly once, in declaration order: a present key, or
/// `null` for an absent `ACCEPT_ABSENT` or `SKIP` member, so the walk never
/// tracks presence itself. Unknown keys are skipped. After any error the walk
/// abandons the source.
#[derive(Debug)]
pub struct Source<'t> {
    format: Format,
    stack: Vec<Frame<'t>>,
    staged: Option<Value>,
    path: Vec<String>,
}

#[derive(Debug)]
enum Frame<'t> {
    Seq {
        items: std::vec::IntoIter<Value>,
        index: usize,
        open: bool,
    },
    Map {
        entries: std::vec::IntoIter<(Value, Value)>,
        state: MapState,
    },
    Record {
        table: Table<'t>,
        values: Vec<Option<Value>>,
        next: usize,
        open: bool,
    },
    Variant {
        payload: bool,
    },
}

#[derive(Debug)]
enum MapState {
    Idle,
    Key { value: Value, segment: String },
    Value,
}

fn int_target(min: i128, max: i128) -> String {
    let named = [
        (i128::from(i8::MIN), i128::from(i8::MAX), "i8"),
        (i128::from(i16::MIN), i128::from(i16::MAX), "i16"),
        (i128::from(i32::MIN), i128::from(i32::MAX), "i32"),
        (i128::from(i64::MIN), i128::from(i64::MAX), "i64"),
        (0, i128::from(u8::MAX), "u8"),
        (0, i128::from(u16::MAX), "u16"),
        (0, i128::from(u32::MAX), "u32"),
        (0, i128::from(u64::MAX), "u64"),
    ];
    named
        .iter()
        .find(|(lo, hi, _)| *lo == min && *hi == max)
        .map_or_else(
            || format!("integer in {min}..={max}"),
            |(_, _, name)| (*name).to_owned(),
        )
}

impl<'t> Source<'t> {
    /// Parse `input` as one complete document.
    ///
    /// # Errors
    /// `DecodeError::Syntax` when the input is malformed, has a duplicate key,
    /// or nests deeper than [`crate::MAX_DEPTH`].
    pub fn new(format: Format, input: &[u8]) -> Result<Self, DecodeError> {
        Ok(Self::from_value(format, parse(format, input)?))
    }

    /// Decode an already-parsed document (a dynamic value) as `format` would.
    #[must_use]
    pub fn from_value(format: Format, value: Value) -> Self {
        Self {
            format,
            stack: Vec::new(),
            staged: Some(value),
            path: Vec::new(),
        }
    }

    /// The path of the value being read: `.field`, `[index]`, `["key"]`,
    /// `.Variant`; empty at the root.
    #[must_use]
    pub fn path(&self) -> String {
        self.path.concat()
    }

    fn take(&mut self) -> Value {
        self.staged
            .take()
            .expect("hew-codec: a read with no value staged")
    }

    fn type_error(&self, expected: impl Into<String>, found: impl Into<String>) -> DecodeError {
        DecodeError::Type {
            path: self.path(),
            expected: expected.into(),
            found: found.into(),
        }
    }

    fn mismatch(&self, expected: &str, found: &Value) -> DecodeError {
        self.type_error(expected, found.kind())
    }

    /// A value was fully read: advance the enclosing container.
    fn complete(&mut self) {
        match self.stack.last_mut() {
            Some(Frame::Seq { open, .. } | Frame::Record { open, .. }) => {
                if *open {
                    *open = false;
                    self.path.pop();
                }
            }
            Some(Frame::Map { state, .. }) => match core::mem::replace(state, MapState::Idle) {
                MapState::Key { value, segment } => {
                    *state = MapState::Value;
                    self.staged = Some(value);
                    self.path.push(segment);
                }
                MapState::Value => {
                    self.path.pop();
                }
                MapState::Idle => panic!("hew-codec: a map value read without `map_next`"),
            },
            Some(Frame::Variant { .. }) | None => {}
        }
    }

    #[expect(
        clippy::unnecessary_wraps,
        reason = "every read arm returns through it"
    )]
    fn finish_read<T>(&mut self, value: T) -> Result<T, DecodeError> {
        self.complete();
        Ok(value)
    }

    /// `null` is consumed and reported; any other value stays staged.
    ///
    /// # Errors
    /// Never; the `Result` keeps every pull uniform.
    pub fn is_null(&mut self) -> Result<bool, DecodeError> {
        if matches!(self.staged, Some(Value::Null)) {
            self.staged = None;
            self.complete();
            Ok(true)
        } else {
            Ok(false)
        }
    }

    /// # Errors
    /// `Type` when the value is not a boolean.
    pub fn read_bool(&mut self) -> Result<bool, DecodeError> {
        match self.take() {
            Value::Bool(value) => self.finish_read(value),
            other => Err(self.mismatch("bool", &other)),
        }
    }

    fn read_int(&mut self, min: i128, max: i128) -> Result<i128, DecodeError> {
        match self.take() {
            Value::Int(value) if (min..=max).contains(&value) => self.finish_read(value),
            Value::Int(value) => Err(DecodeError::Range {
                path: self.path(),
                value: value.to_string(),
                target: int_target(min, max),
            }),
            other => Err(self.mismatch("integer", &other)),
        }
    }

    /// An integer within `min..=max`.
    ///
    /// # Errors
    /// `Type` for a non-integer (a fraction included), `Range` when it does
    /// not fit.
    pub fn read_i64(&mut self, min: i64, max: i64) -> Result<i64, DecodeError> {
        self.read_int(min.into(), max.into())
            .map(|value| i64::try_from(value).unwrap_or_default())
    }

    /// An integer within `0..=max`.
    ///
    /// # Errors
    /// `Type` for a non-integer, `Range` when it does not fit.
    pub fn read_u64(&mut self, max: u64) -> Result<u64, DecodeError> {
        self.read_int(0, max.into())
            .map(|value| u64::try_from(value).unwrap_or_default())
    }

    /// A number. JSON also accepts `"NaN"`, `"Infinity"` and `"-Infinity"`.
    ///
    /// # Errors
    /// `Type` when the value is not a number.
    #[expect(
        clippy::cast_precision_loss,
        reason = "an integer written for a float field reads as the nearest float"
    )]
    pub fn read_f64(&mut self) -> Result<f64, DecodeError> {
        let value = self.take();
        let number = match &value {
            Value::Float(number) => *number,
            Value::Int(number) => *number as f64,
            Value::Str(text) if self.format == Format::Json => match text.as_str() {
                "NaN" => f64::NAN,
                "Infinity" => f64::INFINITY,
                "-Infinity" => f64::NEG_INFINITY,
                _ => return Err(self.mismatch("float", &value)),
            },
            _ => return Err(self.mismatch("float", &value)),
        };
        self.finish_read(number)
    }

    /// # Errors
    /// `Type` when the value is not a string.
    pub fn read_str(&mut self) -> Result<String, DecodeError> {
        match self.take() {
            Value::Str(text) => self.finish_read(text),
            other => Err(self.mismatch("string", &other)),
        }
    }

    /// A string of exactly one Unicode scalar.
    ///
    /// # Errors
    /// `Type` for anything else.
    pub fn read_char(&mut self) -> Result<char, DecodeError> {
        let value = self.take();
        if let Value::Str(text) = &value {
            let mut chars = text.chars();
            if let (Some(ch), None) = (chars.next(), chars.next()) {
                return self.finish_read(ch);
            }
        }
        Err(self.mismatch("char", &value))
    }

    /// Binary formats read a byte string; text formats read padded base64.
    ///
    /// # Errors
    /// `Type` for anything else.
    pub fn read_bytes(&mut self) -> Result<Vec<u8>, DecodeError> {
        let value = self.take();
        let bytes = match value {
            Value::Bytes(bytes) if !self.format.is_text() => bytes,
            Value::Str(text) if self.format.is_text() => {
                match base64::engine::general_purpose::STANDARD.decode(text.as_bytes()) {
                    Ok(bytes) => bytes,
                    Err(_) => return Err(self.type_error("base64 string", "string")),
                }
            }
            other => {
                let expected = if self.format.is_text() {
                    "base64 string"
                } else {
                    "bytes"
                };
                return Err(self.mismatch(expected, &other));
            }
        };
        self.finish_read(bytes)
    }

    /// Any value, for a dynamic target (`json.Value` and its peers).
    ///
    /// # Errors
    /// Never.
    pub fn read_any(&mut self) -> Result<Value, DecodeError> {
        let value = self.take();
        self.finish_read(value)
    }

    fn open_seq(&mut self) -> Result<Vec<Value>, DecodeError> {
        match self.take() {
            Value::Seq(items) => Ok(items),
            other => Err(self.mismatch("sequence", &other)),
        }
    }

    fn push_seq(&mut self, items: Vec<Value>) -> usize {
        let len = items.len();
        self.stack.push(Frame::Seq {
            items: items.into_iter(),
            index: 0,
            open: false,
        });
        len
    }

    /// A sequence of any length (`Vec`); returns its length.
    ///
    /// # Errors
    /// `Type` when the value is not a sequence.
    pub fn seq_begin(&mut self) -> Result<usize, DecodeError> {
        let items = self.open_seq()?;
        Ok(self.push_seq(items))
    }

    /// A sequence of exactly `len` values (a tuple or fixed array).
    ///
    /// # Errors
    /// `Type` when the value is not a sequence of that length.
    pub fn tuple_begin(&mut self, len: usize) -> Result<(), DecodeError> {
        let items = self.open_seq()?;
        if items.len() != len {
            return Err(self.type_error(
                format!("sequence of {len}"),
                format!("sequence of {}", items.len()),
            ));
        }
        self.push_seq(items);
        Ok(())
    }

    /// A set; returns its length.
    ///
    /// # Errors
    /// `Type` when the value is not a sequence, `Duplicate` when an element
    /// repeats.
    pub fn set_begin(&mut self) -> Result<usize, DecodeError> {
        let items = self.open_seq()?;
        let pairs: Vec<(Value, Value)> =
            items.into_iter().map(|item| (item, Value::Null)).collect();
        if let Some(element) = duplicate_key(&pairs) {
            return Err(DecodeError::Duplicate {
                path: self.path(),
                key: element.render_key(),
            });
        }
        Ok(self.push_seq(pairs.into_iter().map(|(item, _)| item).collect()))
    }

    /// Stage the next element; `false` closes the sequence.
    ///
    /// # Errors
    /// Never.
    pub fn seq_next(&mut self) -> Result<bool, DecodeError> {
        let Some(Frame::Seq {
            items,
            index,
            open: open @ false,
        }) = self.stack.last_mut()
        else {
            panic!("hew-codec: `seq_next` outside a sequence or before its element was read");
        };
        if let Some(item) = items.next() {
            let segment = format!("[{index}]");
            *index += 1;
            *open = true;
            self.staged = Some(item);
            self.path.push(segment);
            Ok(true)
        } else {
            self.stack.pop();
            self.complete();
            Ok(false)
        }
    }

    /// A map; returns its entry count. In text formats a map without
    /// `string` keys is the `[key, value]` pair sequence the sink writes.
    ///
    /// # Errors
    /// `Type` when the value is not the map form its keys select, `Duplicate`
    /// for a repeated key in the pair form.
    pub fn map_begin(&mut self, string_keys: bool) -> Result<usize, DecodeError> {
        let pairs = self.format.is_text() && !string_keys;
        let entries = match self.take() {
            Value::Map(entries) if !pairs => entries,
            Value::Seq(pairs) if self.format.is_text() && !string_keys => {
                let mut entries = Vec::with_capacity(pairs.len());
                for (index, pair) in pairs.into_iter().enumerate() {
                    match pair {
                        Value::Seq(pair) if pair.len() == 2 => {
                            let mut pair = pair.into_iter();
                            if let (Some(key), Some(value)) = (pair.next(), pair.next()) {
                                entries.push((key, value));
                            }
                        }
                        other => {
                            self.path.push(format!("[{index}]"));
                            return Err(self.mismatch("[key, value] pair", &other));
                        }
                    }
                }
                if let Some(key) = duplicate_key(&entries) {
                    return Err(DecodeError::Duplicate {
                        path: self.path(),
                        key: key.render_key(),
                    });
                }
                entries
            }
            other => {
                let expected = if pairs {
                    "sequence of [key, value] pairs"
                } else {
                    "map"
                };
                return Err(self.mismatch(expected, &other));
            }
        };
        let len = entries.len();
        self.stack.push(Frame::Map {
            entries: entries.into_iter(),
            state: MapState::Idle,
        });
        Ok(len)
    }

    /// Stage the next key; once it is read, its value is staged. `false`
    /// closes the map.
    ///
    /// # Errors
    /// Never.
    pub fn map_next(&mut self) -> Result<bool, DecodeError> {
        let Some(Frame::Map {
            entries,
            state: state @ MapState::Idle,
        }) = self.stack.last_mut()
        else {
            panic!("hew-codec: `map_next` outside a map or before its entry was read");
        };
        if let Some((key, value)) = entries.next() {
            let segment = format!("[{}]", key.render_key());
            *state = MapState::Key { value, segment };
            self.staged = Some(key);
            Ok(true)
        } else {
            self.stack.pop();
            self.complete();
            Ok(false)
        }
    }

    /// The key a record field or variant is read under matches `member`.
    fn is_key(&self, table: &Table<'_>, member: &Member<'_>, key: &Value) -> bool {
        match key {
            Value::Int(tag) if table.tagged && !self.format.is_text() => {
                *tag == i128::from(member.tag)
            }
            Value::Str(text) if !table.tagged || self.format.is_text() => text == member.key,
            _ => false,
        }
    }

    /// # Errors
    /// `Type` when the value is not a map.
    pub fn record_begin(&mut self, table: Table<'t>) -> Result<(), DecodeError> {
        let entries = match self.take() {
            Value::Map(entries) => entries,
            other => return Err(self.mismatch("map", &other)),
        };
        let mut values: Vec<Option<Value>> = vec![None; table.members.len()];
        for (key, value) in entries {
            if let Some(index) = table
                .members
                .iter()
                .position(|member| !member.has(Member::SKIP) && self.is_key(&table, member, &key))
            {
                values[index] = Some(value);
            }
        }
        self.stack.push(Frame::Record {
            table,
            values,
            next: 0,
            open: false,
        });
        Ok(())
    }

    /// Stage the next field and return its index; `None` closes the record.
    ///
    /// # Errors
    /// `Missing` for an absent member without `ACCEPT_ABSENT` or `SKIP`.
    pub fn record_next(&mut self) -> Result<Option<usize>, DecodeError> {
        let Some(Frame::Record {
            table,
            values,
            next,
            open: open @ false,
        }) = self.stack.last_mut()
        else {
            panic!("hew-codec: `record_next` outside a record or before its field was read");
        };
        let index = *next;
        let Some(member) = table.members.get(index) else {
            self.stack.pop();
            self.complete();
            return Ok(None);
        };
        *next += 1;
        let segment = format!(".{}", member.key);
        let value = match values[index].take() {
            Some(value) => value,
            None if member.has(Member::ACCEPT_ABSENT) || member.has(Member::SKIP) => Value::Null,
            None => {
                return Err(DecodeError::Missing {
                    path: format!("{}{segment}", self.path()),
                })
            }
        };
        *open = true;
        self.staged = Some(value);
        self.path.push(segment);
        Ok(Some(index))
    }

    /// Read a variant of `table` and return its index. A `PAYLOAD` variant
    /// stages its payload for the walk; `variant_end` closes it either way.
    ///
    /// # Errors
    /// `UnknownVariant` for a name or tag that matches nothing, `Type` for a
    /// value of the wrong form or a payload mismatch.
    pub fn variant(&mut self, table: Table<'t>) -> Result<usize, DecodeError> {
        let value = self.take();
        let (key, payload) = match value {
            Value::Map(entries) if entries.len() == 1 => {
                let mut entries = entries.into_iter();
                let (key, payload) = entries.next().unwrap_or((Value::Null, Value::Null));
                (key, Some(payload))
            }
            Value::Map(entries) => {
                return Err(self.type_error("map of one entry", format!("map of {}", entries.len())))
            }
            Value::Str(_) | Value::Int(_) => (value, None),
            other => return Err(self.mismatch("variant", &other)),
        };
        let Some(index) = table
            .members
            .iter()
            .position(|member| self.is_key(&table, member, &key))
        else {
            return Err(DecodeError::UnknownVariant {
                path: self.path(),
                name: match key {
                    Value::Str(name) => name,
                    other => other.render_key(),
                },
            });
        };
        let member = &table.members[index];
        match (member.has(Member::PAYLOAD), payload) {
            (true, Some(payload)) => {
                self.path.push(format!(".{}", member.key));
                self.staged = Some(payload);
                self.stack.push(Frame::Variant { payload: true });
            }
            (false, None) => self.stack.push(Frame::Variant { payload: false }),
            (true, None) => {
                return Err(self.type_error(format!("{} with a payload", member.key), key.kind()))
            }
            (false, Some(_)) => {
                return Err(self.type_error(format!("unit variant {}", member.key), "map"))
            }
        }
        Ok(index)
    }

    /// # Errors
    /// Never.
    pub fn variant_end(&mut self) -> Result<(), DecodeError> {
        let Some(Frame::Variant { payload }) = self.stack.pop() else {
            panic!("hew-codec: `variant_end` without an open variant");
        };
        if payload {
            self.path.pop();
        }
        self.complete();
        Ok(())
    }
}
