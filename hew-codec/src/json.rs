//! A strict RFC 8259 JSON reader.
//!
//! `serde_json` normalizes an integer above 64 bits to a float and duplicate
//! object keys to the last one before a visitor sees them. This reader keeps
//! integers exact up to 128 bits, refuses a duplicate key at its own offset,
//! and reports every syntax error with a byte offset, line and column.

use crate::value::Value;
use crate::{DecodeError, MAX_DEPTH};

pub(crate) fn parse(text: &[u8]) -> Result<Value, DecodeError> {
    let mut reader = Reader { text, at: 0 };
    reader.space();
    let value = reader.value(0)?;
    reader.space();
    if reader.at < text.len() {
        return Err(reader.error("trailing characters after the value"));
    }
    Ok(value)
}

struct Reader<'a> {
    text: &'a [u8],
    at: usize,
}

impl Reader<'_> {
    fn error(&self, reason: impl Into<String>) -> DecodeError {
        self.error_at(self.at, reason)
    }

    fn error_at(&self, offset: usize, reason: impl Into<String>) -> DecodeError {
        DecodeError::syntax_at(Some(self.text), offset, reason)
    }

    fn peek(&self) -> Option<u8> {
        self.text.get(self.at).copied()
    }

    fn space(&mut self) {
        while let Some(b' ' | b'\t' | b'\n' | b'\r') = self.peek() {
            self.at += 1;
        }
    }

    fn expect(&mut self, byte: u8) -> Result<(), DecodeError> {
        if self.peek() == Some(byte) {
            self.at += 1;
            Ok(())
        } else {
            Err(self.error(format!("expected `{}`", char::from(byte))))
        }
    }

    fn value(&mut self, depth: usize) -> Result<Value, DecodeError> {
        match self.peek() {
            Some(b'{') => self.object(depth + 1),
            Some(b'[') => self.array(depth + 1),
            Some(b'"') => self.string().map(Value::Str),
            Some(b'-' | b'0'..=b'9') => self.number(),
            Some(b't') => self.word("true", Value::Bool(true)),
            Some(b'f') => self.word("false", Value::Bool(false)),
            Some(b'n') => self.word("null", Value::Null),
            Some(_) => Err(self.error("expected a value")),
            None => Err(self.error("unexpected end of input")),
        }
    }

    fn word(&mut self, word: &str, value: Value) -> Result<Value, DecodeError> {
        if self.text[self.at..].starts_with(word.as_bytes()) {
            self.at += word.len();
            Ok(value)
        } else {
            Err(self.error("expected a value"))
        }
    }

    fn enter(&self, depth: usize) -> Result<(), DecodeError> {
        if depth > MAX_DEPTH {
            Err(self.error(format!("nesting exceeds the limit of {MAX_DEPTH}")))
        } else {
            Ok(())
        }
    }

    fn array(&mut self, depth: usize) -> Result<Value, DecodeError> {
        self.enter(depth)?;
        self.at += 1;
        let mut items = Vec::new();
        self.space();
        if self.peek() == Some(b']') {
            self.at += 1;
            return Ok(Value::Seq(items));
        }
        loop {
            self.space();
            items.push(self.value(depth)?);
            self.space();
            match self.peek() {
                Some(b',') => self.at += 1,
                Some(b']') => {
                    self.at += 1;
                    return Ok(Value::Seq(items));
                }
                _ => return Err(self.error("expected `,` or `]`")),
            }
        }
    }

    fn object(&mut self, depth: usize) -> Result<Value, DecodeError> {
        self.enter(depth)?;
        self.at += 1;
        let mut entries: Vec<(Value, Value)> = Vec::new();
        let mut key_offsets = Vec::new();
        self.space();
        if self.peek() == Some(b'}') {
            self.at += 1;
            return Ok(Value::Map(entries));
        }
        loop {
            self.space();
            match self.peek() {
                Some(b'"') => {}
                Some(_) => return Err(self.error("expected a string key")),
                None => return Err(self.error("unexpected end of input")),
            }
            key_offsets.push(self.at);
            let key = self.string()?;
            self.space();
            self.expect(b':')?;
            self.space();
            let value = self.value(depth)?;
            entries.push((Value::Str(key), value));
            self.space();
            match self.peek() {
                Some(b',') => self.at += 1,
                Some(b'}') => {
                    self.at += 1;
                    break;
                }
                _ => return Err(self.error("expected `,` or `}`")),
            }
        }
        if let Some(key) = crate::value::duplicate_key(&entries) {
            // Report the second occurrence: the first is legitimate.
            let second = entries
                .iter()
                .enumerate()
                .filter(|(_, (k, _))| k == key)
                .nth(1)
                .map_or(self.at, |(index, _)| key_offsets[index]);
            return Err(self.error_at(second, format!("duplicate key {}", key.render_key())));
        }
        Ok(Value::Map(entries))
    }

    fn string(&mut self) -> Result<String, DecodeError> {
        self.at += 1;
        let mut out = String::new();
        loop {
            let start = self.at;
            while let Some(byte) = self.peek() {
                if byte == b'"' || byte == b'\\' || byte < 0x20 {
                    break;
                }
                self.at += 1;
            }
            let run = core::str::from_utf8(&self.text[start..self.at])
                .map_err(|e| self.error_at(start + e.valid_up_to(), "invalid UTF-8"))?;
            out.push_str(run);
            match self.peek() {
                Some(b'"') => {
                    self.at += 1;
                    return Ok(out);
                }
                Some(b'\\') => {
                    self.at += 1;
                    self.escape(&mut out)?;
                }
                Some(_) => return Err(self.error("control character in string")),
                None => return Err(self.error("unterminated string")),
            }
        }
    }

    fn escape(&mut self, out: &mut String) -> Result<(), DecodeError> {
        let Some(byte) = self.peek() else {
            return Err(self.error("unterminated string"));
        };
        self.at += 1;
        let ch = match byte {
            b'"' => '"',
            b'\\' => '\\',
            b'/' => '/',
            b'b' => '\u{8}',
            b'f' => '\u{c}',
            b'n' => '\n',
            b'r' => '\r',
            b't' => '\t',
            b'u' => {
                let start = self.at - 2;
                let high = self.hex4()?;
                let code = if (0xD800..0xDC00).contains(&high) {
                    if !self.text[self.at..].starts_with(b"\\u") {
                        return Err(self.error_at(start, "unpaired surrogate"));
                    }
                    self.at += 2;
                    let low = self.hex4()?;
                    if !(0xDC00..0xE000).contains(&low) {
                        return Err(self.error_at(start, "unpaired surrogate"));
                    }
                    0x10000 + ((high - 0xD800) << 10) + (low - 0xDC00)
                } else {
                    high
                };
                char::from_u32(code).ok_or_else(|| self.error_at(start, "unpaired surrogate"))?
            }
            _ => return Err(self.error_at(self.at - 1, "invalid escape")),
        };
        out.push(ch);
        Ok(())
    }

    fn hex4(&mut self) -> Result<u32, DecodeError> {
        let digits = self
            .text
            .get(self.at..self.at + 4)
            .and_then(|d| core::str::from_utf8(d).ok())
            .and_then(|d| {
                u32::from_str_radix(d, 16)
                    .ok()
                    .filter(|_| d.bytes().all(|b| b.is_ascii_hexdigit()))
            })
            .ok_or_else(|| self.error("invalid unicode escape"))?;
        self.at += 4;
        Ok(digits)
    }

    fn digits(&mut self) -> usize {
        let start = self.at;
        while let Some(b'0'..=b'9') = self.peek() {
            self.at += 1;
        }
        self.at - start
    }

    fn number(&mut self) -> Result<Value, DecodeError> {
        let start = self.at;
        if self.peek() == Some(b'-') {
            self.at += 1;
        }
        match self.peek() {
            Some(b'0') => self.at += 1,
            Some(b'1'..=b'9') => {
                self.digits();
            }
            _ => return Err(self.error("invalid number")),
        }
        let mut integral = true;
        if self.peek() == Some(b'.') {
            integral = false;
            self.at += 1;
            if self.digits() == 0 {
                return Err(self.error("invalid number"));
            }
        }
        if let Some(b'e' | b'E') = self.peek() {
            integral = false;
            self.at += 1;
            if let Some(b'+' | b'-') = self.peek() {
                self.at += 1;
            }
            if self.digits() == 0 {
                return Err(self.error("invalid number"));
            }
        }
        // The lexeme is ASCII by construction.
        let lexeme = core::str::from_utf8(&self.text[start..self.at]).unwrap_or_default();
        if integral {
            if let Ok(value) = lexeme.parse::<i128>() {
                return Ok(Value::Int(value));
            }
        }
        lexeme
            .parse::<f64>()
            .map(Value::Float)
            .map_err(|_| self.error_at(start, "invalid number"))
    }
}
