use hew_codec::{DecodeError, Format, Sink, Source, Value};

fn encode(format: Format, value: &Value) -> Vec<u8> {
    let mut bytes = Vec::new();
    match format {
        Format::Cbor => ciborium::ser::into_writer(value, &mut bytes).unwrap(),
        Format::Msgpack => bytes = rmp_serde::to_vec(value).unwrap(),
        Format::Yaml => bytes = serde_yaml::to_string(value).unwrap().into_bytes(),
        _ => unreachable!(),
    }
    bytes
}

fn key(reverse: bool, value: i128) -> Value {
    let mut entries = vec![
        (Value::Int(1), Value::Int(value)),
        (Value::Int(2), Value::Int(20)),
    ];
    if reverse {
        entries.reverse();
    }
    Value::Map(entries)
}

fn assert_duplicate(format: Format, input: &[u8]) {
    let Err(error) = Source::new(format, input) else {
        panic!("duplicate keys must be rejected");
    };
    assert!(
        matches!(error, DecodeError::Syntax { ref reason, .. } if reason.contains("duplicate key")),
        "{error:?}"
    );
}

#[test]
fn map_keys_ignore_entry_order_recursively() {
    for format in [Format::Cbor, Format::Msgpack, Format::Yaml] {
        for wrap in [false, true] {
            let wrap_key = |key| if wrap { Value::Seq(vec![key]) } else { key };
            let value = Value::Map(vec![
                (wrap_key(key(false, 10)), Value::Int(1)),
                (wrap_key(key(true, 10)), Value::Int(2)),
            ]);
            assert_duplicate(format, &encode(format, &value));
        }
    }
}

#[test]
fn distinct_map_keys_roundtrip_and_preserve_input_order() {
    for format in [Format::Cbor, Format::Msgpack] {
        let value = Value::Map(vec![
            (key(true, 10), Value::Int(1)),
            (key(false, 11), Value::Int(2)),
        ]);
        let input = encode(format, &value);
        let parsed = Source::new(format, &input).unwrap().read_any().unwrap();
        assert_eq!(parsed, value);
        let mut sink = Sink::new(format);
        sink.value(&parsed);
        let first = sink.finish();
        let parsed = Source::new(format, &first).unwrap().read_any().unwrap();
        let mut sink = Sink::new(format);
        sink.value(&parsed);
        assert_eq!(first, sink.finish());
    }
}

#[test]
fn cbor_nested_map_key_fuzz_regression() {
    assert_duplicate(
        Format::Cbor,
        include_bytes!("fixtures/cbor-nested-map-keys.cbor"),
    );
}

fn balanced_key(depth: usize, value: i128, reverse: bool) -> Value {
    if depth == 0 {
        return Value::Int(value);
    }
    let mut entries = vec![
        (balanced_key(depth - 1, 0, reverse), Value::Int(0)),
        (balanced_key(depth - 1, 1, reverse), Value::Int(value)),
    ];
    if reverse {
        entries.reverse();
    }
    Value::Map(entries)
}

#[test]
fn balanced_nested_map_keys_preserve_order_and_detect_duplicates() {
    // Both child keys differ only at their final value. Re-sorting maps
    // recursively inside each comparison makes this small tree quadratic.
    let value = balanced_key(14, 0, true);
    for format in [Format::Cbor, Format::Msgpack] {
        let input = encode(format, &value);
        let parsed = Source::new(format, &input).unwrap().read_any().unwrap();
        assert_eq!(parsed, value);

        let duplicate = Value::Map(vec![
            (balanced_key(8, 0, false), Value::Int(0)),
            (balanced_key(8, 0, true), Value::Int(1)),
        ]);
        assert_duplicate(format, &encode(format, &duplicate));
    }
}

#[test]
fn maps_in_map_key_values_ignore_entry_order() {
    for format in [Format::Cbor, Format::Msgpack, Format::Yaml] {
        let value = Value::Map(vec![
            (
                Value::Map(vec![(Value::Int(0), key(false, 10))]),
                Value::Int(0),
            ),
            (
                Value::Map(vec![(Value::Int(0), key(true, 10))]),
                Value::Int(1),
            ),
        ]);
        assert_duplicate(format, &encode(format, &value));
    }
}
