use hew_parser::{
    fmt::{format_source, migrate_punctuation},
    parse, ParseDiagnosticKind, Severity,
};

const CURRENT: &str = r#"
#[wire] type Packet { label: string @1; count: i64 @2; reserved @4, @5; }
type Point { x: i64; y: i64; fn total(point: Point) -> i64 { point.x + point.y } }
enum Reply { Ready; Value { label: string; count: i64; } Failed(string); }
actor Counter {
    var count: i64 = 0;
    let label: string = "counter";
    mailbox 64 overflow drop_new;
    receive fn bump() { count += 1; }
}
machine Flow {
    events { Start; Data { bytes: bytes; code: i64; } }
    emits { Start; Data; }
    state Idle;
    state Busy { count: i64; entry { let n = 1; } exit { let n = 2; } }
    on Start: Idle => Busy { count: 1 }
    on Data(payload): Busy => Idle;
    default { state }
}
actor Worker { receive fn work() {} }
supervisor App {
    strategy: one_for_one;
    intensity: 5 within 60s;
    child worker: Worker() restart: permanent shutdown: 5s;
}
fn main() {
    let point = Point { x: 1, y: 2 };
    match Reply.Ready {
        .Ready => { println("ready"); }
        .Failed(reason) => println(reason),
        _ => println("other"),
    }
}
"#;

#[test]
fn declaration_members_and_arms_round_trip() {
    let parsed = parse(CURRENT);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let formatted = format_source(CURRENT, &parsed.program);
    let reparsed = parse(&formatted);
    assert!(
        reparsed.errors.is_empty(),
        "{:?}\n{formatted}",
        reparsed.errors
    );
    assert_eq!(format_source(&formatted, &reparsed.program), formatted);
    assert!(formatted.contains("count: i64 = 0;"));
    assert!(formatted.contains(".Ready => {"));
    assert!(!formatted.contains("},\n"));
}

#[test]
fn retired_marks_report_recoverable_kinds_at_exact_sites() {
    let cases = [
        (
            "type P { x: i64, }",
            ",",
            ParseDiagnosticKind::MemberTerminator,
        ),
        (
            "type P { x: i64 }",
            "",
            ParseDiagnosticKind::MemberTerminator,
        ),
        (
            "enum E { Value { x: i64; }, }",
            ",",
            ParseDiagnosticKind::SeparatorAfterBody,
        ),
        (
            "actor A { name: string; }",
            "name",
            ParseDiagnosticKind::ActorFieldBinding,
        ),
        (
            "fn main() { let p = P { x: 1; y: 2 }; }",
            ";",
            ParseDiagnosticKind::ListSeparator,
        ),
        (
            "fn main() { match 1 { 1 => { 2 }, _ => 3, } }",
            ",",
            ParseDiagnosticKind::SeparatorAfterBody,
        ),
    ];
    for (source, mark, kind) in cases {
        let parsed = parse(source);
        let error = parsed
            .errors
            .iter()
            .find(|error| error.kind == kind)
            .unwrap_or_else(|| panic!("{source}: {:?}", parsed.errors));
        assert_eq!(error.severity, Severity::Error);
        assert!(error.hint.is_some());
        if !mark.is_empty() {
            assert_eq!(
                &source[error.span.clone()],
                if kind == ParseDiagnosticKind::ActorFieldBinding {
                    ""
                } else {
                    mark
                }
            );
        }
    }
}

#[test]
fn punctuation_migration_preserves_comments_and_rejects_other_errors() {
    let old = "type P { x: i64, // field\n y: i64, }\nactor A { name: string, }\nfn main() { let p = P { x: 1, y: 2 }; }\n";
    let migrated = migrate_punctuation(old).expect("old separators migrate");
    assert!(migrated.contains("x: i64; // field"), "{migrated}");
    assert!(migrated.contains("let name: string;"), "{migrated}");
    assert_eq!(migrate_punctuation(&migrated).unwrap(), migrated);
    let bad = "type P { x: i64, } fn main( {";
    assert!(!migrate_punctuation(bad).unwrap_err().refusals.is_empty());
}
