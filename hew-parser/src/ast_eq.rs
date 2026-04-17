//! AST equality helpers for round-trip testing.
//!
//! `Spanned<T>` is a `(T, Range<usize>)` tuple — the span's byte offsets depend
//! on the exact formatting of the source text. For round-trip property tests
//! we want `parse(format(parse(src)))` to equal `parse(src)` *as programs*
//! even though span offsets will differ between the two parses.
//!
//! This module provides [`program_eq_ignoring_spans`] which serialises both
//! programs via `serde_json`, walks the resulting tree, and normalises every
//! span (represented as `{"start": N, "end": M}`) to a sentinel before
//! comparing. Simple, correct, and fast enough for the ~1000-file corpus.

use crate::ast::Program;
use serde_json::Value;

/// Return `true` iff two programs are structurally equal after all span fields
/// are normalised away. Use for round-trip property assertions where byte
/// offsets naturally differ between parses of differently-formatted sources.
///
/// # Panics
///
/// Panics if `Program` fails to serialise to JSON — this is effectively
/// impossible for the AST types defined in this crate (all fields derive
/// `Serialize`) and indicates a bug in the AST definition.
#[must_use]
pub fn program_eq_ignoring_spans(a: &Program, b: &Program) -> bool {
    let mut va = serde_json::to_value(a).expect("Program serialises to JSON");
    let mut vb = serde_json::to_value(b).expect("Program serialises to JSON");
    strip_spans(&mut va);
    strip_spans(&mut vb);
    va == vb
}

/// Produce a pretty-printed, span-stripped JSON rendering of `program`.
/// Useful as a diff substrate when a round-trip assertion fails.
///
/// # Panics
///
/// Panics if `Program` fails to serialise to JSON (see
/// [`program_eq_ignoring_spans`] — same invariant).
#[must_use]
pub fn program_debug_json(program: &Program) -> String {
    let mut v = serde_json::to_value(program).expect("Program serialises to JSON");
    strip_spans(&mut v);
    serde_json::to_string_pretty(&v).expect("Value serialises to String")
}

/// Recursively walk a JSON value and replace any span object
/// (`{"start": int, "end": int}` with exactly those two keys) with `Null`.
/// `Spanned<T>` is serialised by `serde` as a two-element array `[value,
/// {"start": N, "end": M}]`; normalising the span half of that tuple is
/// sufficient to make byte offsets irrelevant to equality.
fn strip_spans(v: &mut Value) {
    match v {
        Value::Object(map) => {
            if is_span_object(map) {
                *v = Value::Null;
                return;
            }
            for child in map.values_mut() {
                strip_spans(child);
            }
        }
        Value::Array(items) => {
            for child in items {
                strip_spans(child);
            }
        }
        Value::Null | Value::Bool(_) | Value::Number(_) | Value::String(_) => {}
    }
}

fn is_span_object(map: &serde_json::Map<String, Value>) -> bool {
    if map.len() != 2 {
        return false;
    }
    matches!(map.get("start"), Some(Value::Number(n)) if n.is_u64())
        && matches!(map.get("end"), Some(Value::Number(n)) if n.is_u64())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse;

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
    fn is_span_object_rejects_non_span_maps() {
        let not_span: serde_json::Map<String, Value> =
            serde_json::from_str(r#"{"start": 0, "end": 10, "extra": 1}"#).unwrap();
        assert!(!is_span_object(&not_span));

        let also_not: serde_json::Map<String, Value> =
            serde_json::from_str(r#"{"start": -1, "end": 10}"#).unwrap();
        // -1 is signed, not u64 — treat as not-a-span.
        assert!(!is_span_object(&also_not));
    }
}
