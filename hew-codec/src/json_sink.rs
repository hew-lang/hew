//! JSON events written directly into the document buffer.

use std::io::Write as _;
use std::ops::Range;

use crate::{Member, Table};

/// A typed JSON writer. Only canonical maps and sets buffer encoded ranges;
/// records, sequences and borrowed scalars write directly to the document.
#[derive(Debug, Default)]
pub struct JsonSink<'t> {
    out: Vec<u8>,
    stack: Vec<Frame<'t>>,
    started: bool,
}

#[derive(Debug)]
enum Frame<'t> {
    Seq {
        count: usize,
    },
    Set {
        start: usize,
        item: usize,
        items: Vec<Range<usize>>,
    },
    Map {
        start: usize,
        item: usize,
        key: Option<Range<usize>>,
        entries: Vec<(Range<usize>, Range<usize>)>,
        pairs: bool,
    },
    Record {
        table: Table<'t>,
        field: Option<usize>,
        count: usize,
    },
    Variant {
        payload: bool,
        count: usize,
    },
}

impl<'t> JsonSink<'t> {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    // Prefix a value only once its nullness is known, so omitted fields never
    // need a temporary value or a repair pass over the record.
    fn begin(&mut self, null: bool) -> bool {
        match self.stack.last_mut() {
            None => {
                assert!(!self.started, "hew-codec: a second root value");
                self.started = true;
            }
            Some(Frame::Seq { count }) => {
                if *count != 0 {
                    self.out.push(b',');
                }
                *count += 1;
            }
            Some(Frame::Set { item, .. } | Frame::Map { item, .. }) => *item = self.out.len(),
            Some(Frame::Record {
                table,
                field,
                count,
            }) => {
                let member =
                    &table.members()[field.take().expect("hew-codec: record value without field")];
                if null && member.has(Member::OMIT_NULL) {
                    return false;
                }
                if *count != 0 {
                    self.out.push(b',');
                }
                *count += 1;
                serde_json::to_writer(&mut self.out, member.key()).expect("JSON string");
                self.out.push(b':');
            }
            Some(Frame::Variant { payload, count }) => {
                assert!(
                    *payload && *count == 0,
                    "hew-codec: invalid variant payload"
                );
                *count += 1;
            }
        }
        true
    }

    fn end(&mut self) {
        match self.stack.last_mut() {
            Some(Frame::Set { item, items, .. }) => items.push(*item..self.out.len()),
            Some(Frame::Map {
                item, key, entries, ..
            }) => {
                let value = *item..self.out.len();
                if let Some(key) = key.take() {
                    entries.push((key, value));
                } else {
                    *key = Some(value);
                }
            }
            _ => {}
        }
    }

    fn scalar(&mut self, value: impl serde::Serialize) {
        if self.begin(false) {
            serde_json::to_writer(&mut self.out, &value).expect("JSON scalar");
            self.end();
        }
    }

    pub fn null(&mut self) {
        if self.begin(true) {
            self.out.extend_from_slice(b"null");
            self.end();
        }
    }
    pub fn bool(&mut self, value: bool) {
        self.scalar(value);
    }
    pub fn i64(&mut self, value: i64) {
        self.scalar(value);
    }
    pub fn u64(&mut self, value: u64) {
        self.scalar(value);
    }
    pub(crate) fn i128(&mut self, value: i128) {
        self.scalar(value);
    }
    pub fn f64(&mut self, value: f64) {
        if value.is_finite() {
            self.scalar(value);
        } else {
            self.str(if value.is_nan() {
                "NaN"
            } else if value > 0.0 {
                "Infinity"
            } else {
                "-Infinity"
            });
        }
    }
    pub fn str(&mut self, value: &str) {
        self.scalar(value);
    }
    pub fn bytes(&mut self, value: &[u8]) {
        self.begin(false);
        self.out.push(b'"');
        let mut encoder = base64::write::EncoderWriter::new(
            &mut self.out,
            &base64::engine::general_purpose::STANDARD,
        );
        encoder.write_all(value).expect("base64 buffer");
        encoder.finish().expect("base64 buffer");
        drop(encoder);
        self.out.push(b'"');
        self.end();
    }
    pub fn seq_begin(&mut self, _len: usize) {
        self.begin(false);
        self.out.push(b'[');
        self.stack.push(Frame::Seq { count: 0 });
    }
    pub fn set_begin(&mut self, len: usize) {
        self.begin(false);
        self.out.push(b'[');
        self.stack.push(Frame::Set {
            start: self.out.len(),
            item: 0,
            items: Vec::with_capacity(len),
        });
    }
    pub fn seq_end(&mut self) {
        match self
            .stack
            .pop()
            .expect("hew-codec: sequence end without sequence")
        {
            Frame::Seq { .. } => {}
            Frame::Set {
                start, mut items, ..
            } => {
                items.sort_by(|a, b| self.out[a.clone()].cmp(&self.out[b.clone()]));
                let sorted = items.iter().enumerate().fold(
                    Vec::with_capacity(self.out.len() - start + items.len()),
                    |mut out, (i, item)| {
                        if i != 0 {
                            out.push(b',');
                        }
                        out.extend_from_slice(&self.out[item.clone()]);
                        out
                    },
                );
                self.out.truncate(start);
                self.out.extend_from_slice(&sorted);
            }
            _ => panic!("hew-codec: sequence end without sequence"),
        }
        self.out.push(b']');
        self.end();
    }
    pub fn map_begin(&mut self, len: usize, string_keys: bool) {
        self.begin(false);
        self.out.push(if string_keys { b'{' } else { b'[' });
        self.stack.push(Frame::Map {
            start: self.out.len(),
            item: 0,
            key: None,
            entries: Vec::with_capacity(len),
            pairs: !string_keys,
        });
    }
    pub fn map_end(&mut self) {
        let Some(Frame::Map {
            start,
            key: None,
            mut entries,
            pairs,
            ..
        }) = self.stack.pop()
        else {
            panic!("hew-codec: invalid map end");
        };
        entries.sort_by(|(a, _), (b, _)| self.out[a.clone()].cmp(&self.out[b.clone()]));
        let mut sorted = Vec::with_capacity(self.out.len() - start + entries.len() * 4);
        for (i, (key, value)) in entries.iter().enumerate() {
            if i != 0 {
                sorted.push(b',');
            }
            if pairs {
                sorted.push(b'[');
            }
            sorted.extend_from_slice(&self.out[key.clone()]);
            sorted.push(if pairs { b',' } else { b':' });
            sorted.extend_from_slice(&self.out[value.clone()]);
            if pairs {
                sorted.push(b']');
            }
        }
        self.out.truncate(start);
        self.out.extend_from_slice(&sorted);
        self.out.push(if pairs { b']' } else { b'}' });
        self.end();
    }
    pub fn record_begin(&mut self, table: Table<'t>) {
        self.begin(false);
        self.out.push(b'{');
        self.stack.push(Frame::Record {
            table,
            field: None,
            count: 0,
        });
    }
    pub fn field(&mut self, index: usize) {
        let Some(Frame::Record { table, field, .. }) = self.stack.last_mut() else {
            panic!("hew-codec: field outside record");
        };
        assert!(
            field.is_none() && index < table.members().len(),
            "hew-codec: invalid record field"
        );
        *field = Some(index);
    }
    pub fn record_end(&mut self) {
        assert!(
            matches!(self.stack.pop(), Some(Frame::Record { field: None, .. })),
            "hew-codec: invalid record end"
        );
        self.out.push(b'}');
        self.end();
    }
    pub fn variant(&mut self, table: Table<'t>, index: usize) {
        self.begin(false);
        let member = &table.members()[index];
        let payload = member.has(Member::PAYLOAD);
        if payload {
            self.out.push(b'{');
        }
        serde_json::to_writer(&mut self.out, member.key()).expect("JSON string");
        if payload {
            self.out.push(b':');
        }
        self.stack.push(Frame::Variant { payload, count: 0 });
    }
    pub fn variant_end(&mut self) {
        let Some(Frame::Variant { payload, count }) = self.stack.pop() else {
            panic!("hew-codec: variant end without variant");
        };
        assert_eq!(
            count,
            usize::from(payload),
            "hew-codec: missing variant payload"
        );
        if payload {
            self.out.push(b'}');
        }
        self.end();
    }
    #[must_use]
    pub fn finish(self) -> Vec<u8> {
        assert!(
            self.started && self.stack.is_empty(),
            "hew-codec: incomplete JSON document"
        );
        self.out
    }
}
