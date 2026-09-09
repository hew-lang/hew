//! Negative controls for the pattern conditions of spec §12.5: `if let`,
//! `while let` and `let … else`. The positive behaviour is proved end to end by
//! the `pattern-if-let-coverage`, `pattern-let-chains` and `pattern-let-else`
//! core-acceptance cases; these tests pin the refusals that keep the surface
//! honest.

#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
pub(super) use super::*;

#[test]
fn bare_variant_pattern_in_if_let_is_refused() {
    let (errors, _) = parse_and_check(
        r#"
enum Slot { Present(i64), Absent }
fn main() {
    let slot: Slot = .Present(1);
    if let Present(n) = slot { println(f"{n}"); }
}
"#,
    );
    assert!(
        errors
            .iter()
            .any(|error| matches!(error.kind, TypeErrorKind::BareVariantPattern)),
        "a bare variant pattern stays E_BARE_VARIANT_PATTERN inside `if let`: {errors:#?}"
    );
}

#[test]
fn every_match_pattern_is_admitted_in_if_let() {
    // The positive control for the refusals below: the shapes the old
    // payload-only gate rejected now type-check in `if let` and `while let`.
    let (errors, _) = parse_and_check(
        r#"
type Point { x: i64, y: i64 }
enum Item { Text(string), Pair(string, i64), Blank }
fn main() {
    let point = Point { x: 1, y: 2 };
    if let Point { x, y } = point { println(f"{x} {y}"); }
    let entry = ("a", 1);
    if let (label, count) = entry { println(f"{label} {count}"); }
    let item: Item = .Blank;
    if let .Text(owned) | .Pair(owned, _) = item { println(owned); }
    if let .Blank = item { println("blank"); }
    var pending: Item = .Blank;
    while let .Text(owned) | .Pair(owned, _) = pending {
        println(owned);
        pending = .Blank;
    }
}
"#,
    );
    assert!(
        errors.is_empty(),
        "every `match` pattern shape must type-check in a pattern condition: {errors:#?}"
    );
}

#[test]
fn condition_binding_is_not_visible_in_the_else_arm() {
    let (errors, _) = parse_and_check(
        r#"
fn main() {
    let first: Option<i64> = .Some(1);
    if let .Some(n) = first {
        println(f"{n}");
    } else {
        println(f"{n}");
    }
}
"#,
    );
    assert!(
        errors
            .iter()
            .any(|error| matches!(error.kind, TypeErrorKind::UndefinedVariable)),
        "a condition binding must not reach the else arm: {errors:#?}"
    );
}

#[test]
fn let_else_block_must_diverge() {
    let (errors, _) = parse_and_check(
        r#"
fn main() {
    let value: Option<i64> = .Some(1);
    let .Some(n) = value else {
        println("no value");
    };
    println(f"{n}");
}
"#,
    );
    assert!(
        errors
            .iter()
            .any(|error| matches!(error.kind, TypeErrorKind::LetElseDoesNotDiverge)),
        "a fall-through `else` is E_LET_ELSE_FALLTHROUGH: {errors:#?}"
    );
}

#[test]
fn refutable_plain_let_is_refused() {
    let (errors, _) = parse_and_check(
        r#"
fn main() {
    let value: Option<i64> = .Some(1);
    let .Some(n) = value;
    println(f"{n}");
}
"#,
    );
    assert!(
        errors
            .iter()
            .any(|error| matches!(error.kind, TypeErrorKind::RefutableLetPattern { .. })),
        "a refutable pattern in a plain `let` is E_REFUTABLE_LET: {errors:#?}"
    );
}

#[test]
fn pattern_condition_diagnostic_codes_match_the_spec() {
    assert_eq!(
        TypeErrorKind::RefutableLetPattern {
            kind_label: "enum variant".to_string(),
        }
        .as_kind_str(),
        "E_REFUTABLE_LET"
    );
    assert_eq!(
        TypeErrorKind::LetElseDoesNotDiverge.as_kind_str(),
        "E_LET_ELSE_FALLTHROUGH"
    );
}
