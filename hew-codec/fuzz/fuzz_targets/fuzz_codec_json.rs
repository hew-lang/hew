//! The strict JSON reader against `serde_json` as a reference.
//!
//! Properties: the reader never panics and stays within the heap bound; it
//! accepts exactly the documents `serde_json` accepts, except that it refuses
//! duplicate object keys and reads a number beyond the f64 range as an
//! infinity (which the JSON writer spells `"Infinity"`) where `serde_json`
//! refuses it; and an accepted document has the same structure, strings and
//! numbers under both readers.
#![no_main]

use hew_codec::{DecodeError, Source, Value};
use libfuzzer_sys::fuzz_target;
use serde_json::Value as Reference;

mod support;

fn same(ours: &Value, theirs: &Reference) -> bool {
    match (ours, theirs) {
        (Value::Null, Reference::Null) => true,
        (Value::Bool(a), Reference::Bool(b)) => a == b,
        (Value::Str(a), Reference::String(b)) => a == b,
        (Value::Int(a), Reference::Number(b)) => match (b.as_i64(), b.as_u64()) {
            (Some(b), _) => *a == i128::from(b),
            (_, Some(b)) => *a == i128::from(b),
            // serde_json reads an integer beyond 64 bits as the nearest f64.
            #[expect(clippy::cast_precision_loss, reason = "mirrors serde_json's rounding")]
            _ => b.as_f64() == Some(*a as f64),
        },
        (Value::Float(a), Reference::Number(b)) => b.as_f64() == Some(*a),
        (Value::Seq(a), Reference::Array(b)) => {
            a.len() == b.len() && a.iter().zip(b).all(|(x, y)| same(x, y))
        }
        (Value::Map(a), Reference::Object(b)) => {
            a.len() == b.len()
                && a.iter().all(|(key, value)| match key {
                    Value::Str(key) => b.get(key).is_some_and(|other| same(value, other)),
                    _ => false,
                })
        }
        _ => false,
    }
}

fn has_infinity(value: &Value) -> bool {
    match value {
        Value::Float(f) => f.is_infinite(),
        Value::Seq(items) => items.iter().any(has_infinity),
        Value::Map(entries) => entries.iter().any(|(_, v)| has_infinity(v)),
        _ => false,
    }
}

fuzz_target!(|data: &[u8]| {
    let bound = support::heap_bound(data.len());
    let ours = support::bounded("json", bound, || {
        Source::json(data).map(|mut source| source.read_any().expect("parsed document"))
    });
    let theirs = serde_json::from_slice::<Reference>(data);
    match (&ours, &theirs) {
        (Ok(ours), Ok(theirs)) => assert!(
            same(ours, theirs),
            "readers disagree: ours {ours:?}, serde_json {theirs:?}"
        ),
        (Err(DecodeError::Syntax { reason, .. }), Ok(_)) if reason.starts_with("duplicate key") => {
        }
        (Ok(ours), Err(err)) if err.to_string().starts_with("number out of range") => {
            assert!(has_infinity(ours), "{err} without an infinity: {ours:?}");
        }
        (Ok(ours), Err(err)) => panic!("accepted what serde_json refuses ({err}): {ours:?}"),
        (Err(err), Ok(theirs)) => panic!("refused what serde_json accepts ({err:?}): {theirs:?}"),
        (Err(_), Err(_)) => {}
    }
});
