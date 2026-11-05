//! Structure-aware valid documents for every supported wire format.
//!
//! Mutation-only parser targets spend most iterations on rejection paths.
//! This target maps every byte string to a bounded value, serializes it first,
//! and then checks parser acceptance and canonical fixed-point behaviour.
#![no_main]

use arbitrary::Unstructured;
use hew_codec::{Format, Sink, Source, Value};
use libfuzzer_sys::fuzz_target;

#[allow(
    dead_code,
    reason = "this target only needs the shared format selector"
)]
mod support;

const MAX_GENERATED_DEPTH: usize = 6;
const MAX_WIDTH: usize = 6;

fn choice(u: &mut Unstructured<'_>, max: u8) -> u8 {
    u.int_in_range(0..=max).unwrap_or(0)
}

fn width(u: &mut Unstructured<'_>) -> usize {
    usize::from(choice(u, MAX_WIDTH as u8))
}

fn text(u: &mut Unstructured<'_>) -> String {
    let len = width(u) * 2;
    let bytes = u.bytes(len).unwrap_or(&[]);
    bytes
        .iter()
        .map(|byte| char::from(b'a' + byte % 26))
        .collect()
}

fn scalar(u: &mut Unstructured<'_>, allow_null: bool) -> Value {
    match choice(u, if allow_null { 4 } else { 3 }) {
        0 => Value::Bool(choice(u, 1) == 1),
        1 => Value::Int(i128::from(u.arbitrary::<i64>().unwrap_or(0))),
        2 => {
            let raw = u.arbitrary::<i32>().unwrap_or(0);
            Value::Float(f64::from(raw) / 17.0)
        }
        3 => Value::Str(text(u)),
        _ => Value::Null,
    }
}

fn general_value(u: &mut Unstructured<'_>, depth: usize) -> Value {
    if depth >= MAX_GENERATED_DEPTH {
        return scalar(u, true);
    }
    match choice(u, 7) {
        0..=4 => scalar(u, true),
        5 => {
            let len = width(u) * 4;
            Value::Bytes(u.bytes(len).unwrap_or(&[]).to_vec())
        }
        6 => Value::Seq((0..width(u)).map(|_| general_value(u, depth + 1)).collect()),
        _ => Value::Map(
            (0..width(u))
                .map(|index| {
                    let key = Value::Str(format!("k{index}_{}", text(u)));
                    (key, general_value(u, depth + 1))
                })
                .collect(),
        ),
    }
}

fn toml_value(u: &mut Unstructured<'_>, depth: usize) -> Value {
    if depth >= MAX_GENERATED_DEPTH {
        return scalar(u, false);
    }
    match choice(u, 5) {
        0..=3 => scalar(u, false),
        4 => Value::Seq(
            (0..width(u))
                .map(|_| Value::Int(i128::from(u.arbitrary::<i64>().unwrap_or(0))))
                .collect(),
        ),
        _ => toml_table(u, depth + 1),
    }
}

fn toml_table(u: &mut Unstructured<'_>, depth: usize) -> Value {
    Value::Map(
        (0..width(u))
            .map(|index| {
                (
                    Value::Str(format!("k{index}_{}", text(u))),
                    toml_value(u, depth),
                )
            })
            .collect(),
    )
}

fn write(format: Format, value: &Value) -> Vec<u8> {
    let mut sink = Sink::new(format);
    sink.value(value);
    sink.finish()
}

fn read(format: Format, bytes: &[u8]) -> Option<Value> {
    let mut source = Source::new(format, bytes).ok()?;
    Some(source.read_any().expect("parsed document has one root"))
}

fuzz_target!(|data: &[u8]| {
    let Some((&selector, bytes)) = data.split_first() else {
        return;
    };
    let format = support::format(selector);
    let mut u = Unstructured::new(bytes);
    let value = if format == Format::Toml {
        toml_table(&mut u, 0)
    } else {
        general_value(&mut u, 0)
    };
    let first = write(format, &value);
    let decoded = read(format, &first).unwrap_or_else(|| {
        panic!(
            "{format:?} refused generated valid output: {:?}",
            String::from_utf8_lossy(&first)
        )
    });
    let second = write(format, &decoded);
    assert_eq!(
        first, second,
        "{format:?} valid encoding is not a fixed point"
    );
});
