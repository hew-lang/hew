use hew_parser::{fmt::format_source, parse};

#[test]
fn comma_data_members_preserve_statement_and_signature_semicolons() {
    let source = r#"
        type Point { x: i64, y: i64, fn total(point: Point) -> i64 { point.x + point.y } }
        enum Reply { Ready, Value { label: string, count: i64 }, Failed(string), }
        actor Counter {
            var count: i64 = 0,
            let label: string = "counter",
            mailbox 64 overflow drop_new,
            receive fn bump() { count += 1; }
        }
        trait Readable { fn read(self) -> i64; }
        extern "C" { fn foreign_value(value: i64) -> i64; }
        fn main() { let point = Point { x: 1, y: 2 }; var n = point.x; n += 1; }
    "#;
    let parsed = parse(source);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let formatted = format_source(source, &parsed.program);
    assert!(formatted.contains("x: i64,"), "{formatted}");
    assert!(formatted.contains("Ready,"), "{formatted}");
    assert!(formatted.contains("count: i64 = 0,"), "{formatted}");
    assert!(formatted.contains("count += 1;"), "{formatted}");
    assert!(formatted.contains("fn read(self) -> i64;"), "{formatted}");
    let reparsed = parse(&formatted);
    assert!(reparsed.errors.is_empty(), "{:?}", reparsed.errors);
    assert_eq!(format_source(&formatted, &reparsed.program), formatted);
}

#[test]
fn machine_data_lists_and_routes_use_commas_but_blocks_do_not() {
    let source = r"
        machine Flow {
            events { Start, Data { bytes: bytes, code: i64 }, }
            emits { Start, Data, }
            state Idle,
            state Busy { count: i64, entry { let n = 1; } exit { let n = 2; } },
            on Start: Idle => Busy { count: 1 }
            on Data(payload): Busy => Idle,
            default { state }
        }
    ";
    let parsed = parse(source);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let formatted = format_source(source, &parsed.program);
    assert!(formatted.contains("state Idle,"), "{formatted}");
    assert!(formatted.contains("=> Idle,"), "{formatted}");
    assert!(formatted.contains("let n = 1;"), "{formatted}");
    let reparsed = parse(&formatted);
    assert!(
        reparsed.errors.is_empty(),
        "{:?}\n{formatted}",
        reparsed.errors
    );
    assert_eq!(format_source(&formatted, &reparsed.program), formatted);
}

#[test]
fn supervisor_config_and_wire_fields_use_structural_commas() {
    let source = r"
        #[wire] type Packet { label: string @1, count: i64 @2, }
        actor Worker { receive fn work() {} }
        supervisor App {
            strategy: one_for_one,
            intensity: 5 within 60s,
            child worker: Worker() restart: permanent shutdown: 5s,
        }
    ";
    let parsed = parse(source);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let formatted = format_source(source, &parsed.program);
    assert!(formatted.contains("strategy: one_for_one,"), "{formatted}");
    assert!(
        formatted.contains("intensity: 5 within 60s,"),
        "{formatted}"
    );
    assert!(formatted.contains("shutdown: 5s,"), "{formatted}");
    let reparsed = parse(&formatted);
    assert!(reparsed.errors.is_empty(), "{:?}", reparsed.errors);
    assert_eq!(format_source(&formatted, &reparsed.program), formatted);
}

#[test]
fn obsolete_semicolon_members_are_rejected_at_the_delimiter() {
    for source in [
        "type Point { x: i64; y: i64 }",
        "enum Reply { Ready; Failed(string) }",
        "enum Reply { Value { count: i64; } }",
        "actor Counter { var count: i64 = 0; receive fn bump() {} }",
        "actor Counter { mailbox 64; }",
        "machine Flow { events { Start; } state Idle }",
        "machine Flow { events { Start } emits { Start; } state Idle }",
        "machine Flow { state Idle; }",
        "machine Flow { state Busy { count: i64; } }",
        "machine Flow { events { Start } state Idle, on Start: Idle => Idle; }",
        "#[wire] type Packet { count: i64 @1; }",
        "supervisor App { strategy: one_for_one; }",
        "supervisor App { intensity: 5 within 60s; }",
        "supervisor App { child worker: Worker(); }",
    ] {
        let parsed = parse(source);
        let delimiter = source.find(';').unwrap();
        assert!(
            parsed
                .errors
                .iter()
                .any(|e| e.span == (delimiter..delimiter + 1)
                    && e.message
                        .contains("expected `,` between structural members")),
            "{source}: {:?}",
            parsed.errors
        );
    }
}

#[test]
fn omitted_separator_is_only_allowed_before_the_closing_brace() {
    for source in [
        "type Point { x: i64 y: i64 }",
        "enum Reply { Ready Failed }",
        "actor Counter { var count: i64 = 0 receive fn bump() {} }",
        "machine Flow { state Idle state Busy }",
    ] {
        let parsed = parse(source);
        assert!(
            parsed.errors.iter().any(|e| e
                .message
                .contains("expected `,` between structural members")),
            "{source}: {:?}",
            parsed.errors
        );
    }
    for source in [
        "type Point { x: i64 }",
        "enum Reply { Ready }",
        "actor Counter { var count: i64 = 0 }",
        "machine Flow { state Idle }",
    ] {
        let parsed = parse(source);
        assert!(parsed.errors.is_empty(), "{source}: {:?}", parsed.errors);
    }
}
