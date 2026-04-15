use hew_parser::parse;

#[test]
fn duplicate_wire_struct_field_numbers_report_diagnostic() {
    let result = parse(
        r"#[wire]
struct Message {
    first: i32 @1,
    second: i32 @1,
}",
    );

    assert!(
        result
            .errors
            .iter()
            .any(|error| error.message.contains("duplicate wire field number @1")),
        "expected duplicate field-number diagnostic, got: {:?}",
        result.errors,
    );
}

#[test]
fn reserved_wire_struct_field_numbers_report_diagnostic() {
    let result = parse(
        r"#[wire]
struct Message {
    reserved @3;
    field: i32 @3,
}",
    );

    assert!(
        result
            .errors
            .iter()
            .any(|error| error.message.contains("wire field number @3 is reserved")),
        "expected reserved field-number diagnostic, got: {:?}",
        result.errors,
    );
}
