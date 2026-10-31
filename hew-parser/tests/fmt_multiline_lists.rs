use hew_parser::fmt::{fidelity, format_source};

fn assert_layout(source: &str) {
    let parsed = hew_parser::parse(source);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let formatted = format_source(source, &parsed.program);
    assert_eq!(formatted, source);
    fidelity::check(source, &formatted).expect("layout preserves tokens and comments");
    let reparsed = hew_parser::parse(&formatted);
    assert_eq!(format_source(&formatted, &reparsed.program), formatted);
}

#[test]
fn multiline_record_arrays_keep_one_item_per_line() {
    assert_layout(
        "fn main() {\n    let invalid = [\n        restart.Backoff { initial: 0ns, maximum: 1s },\n        restart.Backoff { initial: 0ns - 1ns, maximum: 1s },\n        restart.Backoff { initial: 2s, maximum: 1s },\n    ];\n}\n",
    );
}

#[test]
fn compact_lists_stay_compact() {
    assert_layout("fn main() {\n    let values = [1, 2, 3];\n    println(values.len());\n}\n");
}
