use hew_codec::{yaml_value, Format, Sink, Source, Value, MAX_DEPTH};

fn assert_writer_fixed_point(input: &[u8]) -> Value {
    let value = Source::new(Format::Yaml, input)
        .unwrap()
        .read_any()
        .unwrap();
    let mut sink = Sink::new(Format::Yaml);
    sink.value(&value);
    let first = sink.finish();
    let decoded = Source::new(Format::Yaml, &first)
        .unwrap()
        .read_any()
        .unwrap();
    let mut sink = Sink::new(Format::Yaml);
    sink.value(&decoded);
    assert_eq!(sink.finish(), first);
    decoded
}

#[test]
fn indented_root_strings_keep_their_leading_space() {
    for (document, text) in [
        ("' 7\n\n'", " 7\n"),
        ("\"\n@\n\n\"", " @\n"),
        ("\"\\n\\tÉtage\\n\\n\"", "\n\tÉtage\n\n"),
    ] {
        assert_eq!(
            assert_writer_fixed_point(document.as_bytes()),
            Value::Str(text.to_owned()),
            "{document:?}"
        );

        for tag in ["", "!Label "] {
            let input = format!("{tag}{document}");
            let value = yaml_value::parse(&input).unwrap();
            let output = yaml_value::to_string(&value).unwrap();
            assert_eq!(yaml_value::parse(&output).unwrap(), value, "{input:?}");
        }
    }
}

#[test]
fn deeply_nested_native_keys_are_refused_before_canonical_expansion() {
    let input = include_bytes!("fixtures/yaml-nested-keys.yaml");
    assert!(Source::new(Format::Yaml, input).is_err());
}

#[test]
fn non_string_maps_count_both_pair_containers() {
    let mut input = "0".to_owned();
    for _ in 0..MAX_DEPTH / 2 {
        input = format!("{{0: {input}}}");
    }
    assert_writer_fixed_point(input.as_bytes());
    input = format!("{{0: {input}}}");
    assert!(Source::new(Format::Yaml, input.as_bytes()).is_err());
}

#[test]
fn ordinary_sequences_keep_the_full_depth_budget() {
    let input = format!("{}0{}", "[".repeat(MAX_DEPTH), "]".repeat(MAX_DEPTH));
    assert_writer_fixed_point(input.as_bytes());
    let too_deep = format!("[{input}]");
    assert!(Source::new(Format::Yaml, too_deep.as_bytes()).is_err());
}
