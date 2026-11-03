use hew_codec::{Format, Sink, Source, MAX_DEPTH};

fn assert_writer_fixed_point(input: &[u8]) {
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
