//! AST equality helpers for round-trip testing.
//!
//! `Spanned<T>` is a `(T, Range<usize>)` tuple — the span's byte offsets depend
//! on the exact formatting of the source text. For round-trip property tests
//! we want `parse(format(parse(src)))` to equal `parse(src)` *as programs*
//! even though span offsets will differ between the two parses.
//!
//! [`program_eq_ignoring_spans`] serialises both programs into a compact byte
//! fingerprint that leaves out every `Range` and compares the bytes. The
//! fingerprint is one flat buffer, so a large program costs a few bytes per
//! node rather than a tree of JSON values.

use crate::ast::Program;
use serde::ser::{self, Serialize};

/// Return `true` iff two programs are structurally equal once all span fields
/// are left out. Use for round-trip property assertions where byte offsets
/// naturally differ between parses of differently-formatted sources.
#[must_use]
pub fn program_eq_ignoring_spans(a: &Program, b: &Program) -> bool {
    fingerprint(a) == fingerprint(b)
}

/// The span, in `a`, of the first top-level item that differs from `b` once
/// spans are left out; the end of the last item of `a` when one program has
/// items the other lacks; `None` when the items are equal.
#[must_use]
pub fn first_item_difference(a: &Program, b: &Program) -> Option<std::ops::Range<usize>> {
    if let Some((item, _)) = a
        .items
        .iter()
        .zip(&b.items)
        .find(|(left, right)| fingerprint(&left.0) != fingerprint(&right.0))
    {
        return Some(item.1.clone());
    }
    (a.items.len() != b.items.len()).then(|| {
        let end = a.items.last().map_or(0, |item| item.1.end);
        end..end
    })
}

/// The span-blind encoding of `value`.
fn fingerprint(value: &(impl Serialize + ?Sized)) -> Vec<u8> {
    let mut out = Vec::new();
    // Serialising into a byte buffer cannot fail.
    let _ = value.serialize(&mut Fingerprint { out: &mut out });
    out
}

/// A serialiser writing a tagged, length-prefixed byte stream. Every serde
/// shape gets its own tag and a variant is written by index, so two values
/// encode equally exactly when they are equal; a struct named `Range` (a
/// span) writes nothing.
struct Fingerprint<'a> {
    out: &'a mut Vec<u8>,
}

#[derive(Debug)]
struct Never;

impl std::fmt::Display for Never {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("fingerprinting cannot fail")
    }
}

impl std::error::Error for Never {}

impl ser::Error for Never {
    fn custom<T: std::fmt::Display>(_: T) -> Self {
        Self
    }
}

impl Fingerprint<'_> {
    fn tag(&mut self, tag: u8) {
        self.out.push(tag);
    }

    fn bytes(&mut self, tag: u8, bytes: &[u8]) {
        self.tag(tag);
        self.out
            .extend_from_slice(&(bytes.len() as u64).to_le_bytes());
        self.out.extend_from_slice(bytes);
    }

    fn variant(&mut self, tag: u8, index: u32) {
        self.tag(tag);
        self.out.extend_from_slice(&index.to_le_bytes());
    }
}

/// A compound value being written; `skip` drops a span's fields.
struct Compound<'a, 'b> {
    writer: &'a mut Fingerprint<'b>,
    skip: bool,
}

impl<'a, 'b> ser::Serializer for &'a mut Fingerprint<'b> {
    type Ok = ();
    type Error = Never;
    type SerializeSeq = Compound<'a, 'b>;
    type SerializeTuple = Compound<'a, 'b>;
    type SerializeTupleStruct = Compound<'a, 'b>;
    type SerializeTupleVariant = Compound<'a, 'b>;
    type SerializeMap = Map<'a, 'b>;
    type SerializeStruct = Compound<'a, 'b>;
    type SerializeStructVariant = Compound<'a, 'b>;

    fn serialize_bool(self, v: bool) -> Result<(), Never> {
        self.bytes(0, &[u8::from(v)]);
        Ok(())
    }
    fn serialize_i8(self, v: i8) -> Result<(), Never> {
        self.serialize_i128(i128::from(v))
    }
    fn serialize_i16(self, v: i16) -> Result<(), Never> {
        self.serialize_i128(i128::from(v))
    }
    fn serialize_i32(self, v: i32) -> Result<(), Never> {
        self.serialize_i128(i128::from(v))
    }
    fn serialize_i64(self, v: i64) -> Result<(), Never> {
        self.serialize_i128(i128::from(v))
    }
    fn serialize_i128(self, v: i128) -> Result<(), Never> {
        self.bytes(1, &v.to_le_bytes());
        Ok(())
    }
    fn serialize_u8(self, v: u8) -> Result<(), Never> {
        self.serialize_u128(u128::from(v))
    }
    fn serialize_u16(self, v: u16) -> Result<(), Never> {
        self.serialize_u128(u128::from(v))
    }
    fn serialize_u32(self, v: u32) -> Result<(), Never> {
        self.serialize_u128(u128::from(v))
    }
    fn serialize_u64(self, v: u64) -> Result<(), Never> {
        self.serialize_u128(u128::from(v))
    }
    fn serialize_u128(self, v: u128) -> Result<(), Never> {
        self.bytes(2, &v.to_le_bytes());
        Ok(())
    }
    fn serialize_f32(self, v: f32) -> Result<(), Never> {
        self.serialize_f64(f64::from(v))
    }
    fn serialize_f64(self, v: f64) -> Result<(), Never> {
        self.bytes(3, &v.to_bits().to_le_bytes());
        Ok(())
    }
    fn serialize_char(self, v: char) -> Result<(), Never> {
        self.bytes(4, &u32::from(v).to_le_bytes());
        Ok(())
    }
    fn serialize_str(self, v: &str) -> Result<(), Never> {
        self.bytes(5, v.as_bytes());
        Ok(())
    }
    fn serialize_bytes(self, v: &[u8]) -> Result<(), Never> {
        self.bytes(6, v);
        Ok(())
    }
    fn serialize_none(self) -> Result<(), Never> {
        self.tag(7);
        Ok(())
    }
    fn serialize_some<T: ?Sized + Serialize>(self, value: &T) -> Result<(), Never> {
        self.tag(8);
        value.serialize(self)
    }
    fn serialize_unit(self) -> Result<(), Never> {
        self.tag(9);
        Ok(())
    }
    fn serialize_unit_struct(self, _: &'static str) -> Result<(), Never> {
        self.serialize_unit()
    }
    fn serialize_unit_variant(
        self,
        _: &'static str,
        index: u32,
        _: &'static str,
    ) -> Result<(), Never> {
        self.variant(10, index);
        Ok(())
    }
    fn serialize_newtype_struct<T: ?Sized + Serialize>(
        self,
        _: &'static str,
        value: &T,
    ) -> Result<(), Never> {
        value.serialize(self)
    }
    fn serialize_newtype_variant<T: ?Sized + Serialize>(
        self,
        _: &'static str,
        index: u32,
        _: &'static str,
        value: &T,
    ) -> Result<(), Never> {
        self.variant(11, index);
        value.serialize(self)
    }
    fn serialize_seq(self, _: Option<usize>) -> Result<Compound<'a, 'b>, Never> {
        self.tag(12);
        Ok(Compound {
            writer: self,
            skip: false,
        })
    }
    fn serialize_tuple(self, _: usize) -> Result<Compound<'a, 'b>, Never> {
        self.tag(13);
        Ok(Compound {
            writer: self,
            skip: false,
        })
    }
    fn serialize_tuple_struct(self, _: &'static str, _: usize) -> Result<Compound<'a, 'b>, Never> {
        self.tag(13);
        Ok(Compound {
            writer: self,
            skip: false,
        })
    }
    fn serialize_tuple_variant(
        self,
        _: &'static str,
        index: u32,
        _: &'static str,
        _: usize,
    ) -> Result<Compound<'a, 'b>, Never> {
        self.variant(14, index);
        Ok(Compound {
            writer: self,
            skip: false,
        })
    }
    fn serialize_map(self, _: Option<usize>) -> Result<Map<'a, 'b>, Never> {
        self.tag(15);
        Ok(Map {
            writer: self,
            entries: Vec::new(),
            key: None,
        })
    }
    fn serialize_struct(self, name: &'static str, _: usize) -> Result<Compound<'a, 'b>, Never> {
        let skip = name == "Range";
        if !skip {
            self.tag(16);
        }
        Ok(Compound { writer: self, skip })
    }
    fn serialize_struct_variant(
        self,
        _: &'static str,
        index: u32,
        _: &'static str,
        _: usize,
    ) -> Result<Compound<'a, 'b>, Never> {
        self.variant(17, index);
        Ok(Compound {
            writer: self,
            skip: false,
        })
    }
}

impl Compound<'_, '_> {
    fn element<T: ?Sized + Serialize>(&mut self, value: &T) -> Result<(), Never> {
        if self.skip {
            return Ok(());
        }
        value.serialize(&mut *self.writer)
    }

    fn end(self) {
        if !self.skip {
            self.writer.tag(18);
        }
    }
}

impl ser::SerializeSeq for Compound<'_, '_> {
    type Ok = ();
    type Error = Never;
    fn serialize_element<T: ?Sized + Serialize>(&mut self, value: &T) -> Result<(), Never> {
        self.element(value)
    }
    fn end(self) -> Result<(), Never> {
        Compound::end(self);
        Ok(())
    }
}

impl ser::SerializeTuple for Compound<'_, '_> {
    type Ok = ();
    type Error = Never;
    fn serialize_element<T: ?Sized + Serialize>(&mut self, value: &T) -> Result<(), Never> {
        self.element(value)
    }
    fn end(self) -> Result<(), Never> {
        Compound::end(self);
        Ok(())
    }
}

impl ser::SerializeTupleStruct for Compound<'_, '_> {
    type Ok = ();
    type Error = Never;
    fn serialize_field<T: ?Sized + Serialize>(&mut self, value: &T) -> Result<(), Never> {
        self.element(value)
    }
    fn end(self) -> Result<(), Never> {
        Compound::end(self);
        Ok(())
    }
}

impl ser::SerializeTupleVariant for Compound<'_, '_> {
    type Ok = ();
    type Error = Never;
    fn serialize_field<T: ?Sized + Serialize>(&mut self, value: &T) -> Result<(), Never> {
        self.element(value)
    }
    fn end(self) -> Result<(), Never> {
        Compound::end(self);
        Ok(())
    }
}

/// Maps compare independently of iteration order, including module source maps.
struct Map<'a, 'b> {
    writer: &'a mut Fingerprint<'b>,
    entries: Vec<(Vec<u8>, Vec<u8>)>,
    key: Option<Vec<u8>>,
}

impl ser::SerializeMap for Map<'_, '_> {
    type Ok = ();
    type Error = Never;
    fn serialize_key<T: ?Sized + Serialize>(&mut self, key: &T) -> Result<(), Never> {
        self.key = Some(fingerprint(key));
        Ok(())
    }
    fn serialize_value<T: ?Sized + Serialize>(&mut self, value: &T) -> Result<(), Never> {
        let key = self.key.take().expect("map values follow their keys");
        self.entries.push((key, fingerprint(value)));
        Ok(())
    }
    fn end(mut self) -> Result<(), Never> {
        self.entries.sort_unstable();
        for (key, value) in self.entries {
            self.writer.out.extend(key);
            self.writer.out.extend(value);
        }
        self.writer.tag(18);
        Ok(())
    }
}

impl ser::SerializeStruct for Compound<'_, '_> {
    type Ok = ();
    type Error = Never;
    fn serialize_field<T: ?Sized + Serialize>(
        &mut self,
        _: &'static str,
        value: &T,
    ) -> Result<(), Never> {
        self.element(value)
    }
    fn skip_field(&mut self, _: &'static str) -> Result<(), Never> {
        if !self.skip {
            self.writer.tag(19);
        }
        Ok(())
    }
    fn end(self) -> Result<(), Never> {
        Compound::end(self);
        Ok(())
    }
}

impl ser::SerializeStructVariant for Compound<'_, '_> {
    type Ok = ();
    type Error = Never;
    fn serialize_field<T: ?Sized + Serialize>(
        &mut self,
        _: &'static str,
        value: &T,
    ) -> Result<(), Never> {
        self.element(value)
    }
    fn skip_field(&mut self, _: &'static str) -> Result<(), Never> {
        self.writer.tag(19);
        Ok(())
    }
    fn end(self) -> Result<(), Never> {
        Compound::end(self);
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse;

    #[test]
    fn map_iteration_order_does_not_change_equality() {
        let ordered = (0..16)
            .map(|n| (n.to_string(), n))
            .collect::<std::collections::BTreeMap<_, _>>();
        let unordered = ordered
            .clone()
            .into_iter()
            .collect::<std::collections::HashMap<_, _>>();
        assert_eq!(fingerprint(&ordered), fingerprint(&unordered));
    }

    #[test]
    fn identical_sources_compare_equal() {
        let src = "fn main() { let x = 1; }";
        let a = parse(src).program;
        let b = parse(src).program;
        assert!(program_eq_ignoring_spans(&a, &b));
    }

    #[test]
    fn semantically_equal_sources_with_different_layout_compare_equal() {
        // Same program, different whitespace ⇒ different spans, same AST.
        let a = parse("fn main() { let x = 1; }").program;
        let b = parse("fn main() {\n    let x = 1;\n}\n").program;
        assert!(
            program_eq_ignoring_spans(&a, &b),
            "span-stripped ASTs should match across whitespace variants"
        );
    }

    #[test]
    fn different_programs_compare_not_equal() {
        let a = parse("fn main() { let x = 1; }").program;
        let b = parse("fn main() { let x = 2; }").program;
        assert!(!program_eq_ignoring_spans(&a, &b));
    }

    #[test]
    fn first_item_difference_names_the_changed_item_in_the_first_program() {
        let source = "fn keep() {}\nfn change() { let x = 1; }\n";
        let a = parse(source).program;
        let b = parse("fn keep() {}\nfn change() { let x = 2; }\n").program;
        let span = first_item_difference(&a, &b).expect("the second item differs");
        assert!(source[span].starts_with("fn change()"));
        assert_eq!(first_item_difference(&a, &a), None);
        let shorter = parse("fn keep() {}\n").program;
        let end = source.trim_end().len();
        assert_eq!(first_item_difference(&a, &shorter), Some(end..end));
    }
}
