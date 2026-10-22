//! Consume-receiver metadata plumbing for issue #1295.
//!
//! Consuming receiver metadata requests an owned argument. Ordinary values
//! remain usable through copying; affine receivers move. The per-call-site
//! flag preserves the acquisition contract for lowering.

use crate::common;

use hew_types::check::SpanKey;
use hew_types::error::TypeErrorKind;
use hew_types::module_registry::ModuleRegistry;
use hew_types::Checker;

use common::parse_program;

const TRAIT_AND_IMPL: &str = r#"trait Sink {
    fn drain(self);
}

type Bucket {
    tag: string;
}

impl Sink for Bucket {
    fn drain(self) {}
}

fn main() {
    let b = Bucket { tag: "x" };
    b.drain();
    b.drain();
}
"#;

fn run_checker(source: &str, register_consume: bool) -> hew_types::TypeCheckOutput {
    let program = parse_program(source);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    if register_consume {
        checker.register_consume_receiver_method("Sink::drain");
    }
    checker.check_program(&program)
}

#[test]
fn consume_receiver_preserves_ordinary_values() {
    let output = run_checker(TRAIT_AND_IMPL, true);
    assert!(output.errors.is_empty(), "{:?}", output.errors);
}

#[test]
fn consume_receiver_moves_affine_values() {
    let source = TRAIT_AND_IMPL.replace("type Bucket", "#[resource]\ntype Bucket");
    let output = run_checker(&source, true);
    assert!(
        output
            .errors
            .iter()
            .any(|error| error.kind == TypeErrorKind::UseAfterMove),
        "affine consuming receivers must still move: {:?}",
        output.errors
    );
}

/// Borrowing receivers leave the caller usable without acquiring an owner.
#[test]
fn no_consume_receiver_does_not_mark_moved() {
    let output = run_checker(TRAIT_AND_IMPL, /*register_consume=*/ false);
    assert!(
        !output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::UseAfterMove),
        "did not expect UseAfterMove when consume set is empty; got errors: {:?}",
        output.errors,
    );
}

/// Consuming calls retain the ownership-acquisition metadata for lowering.
#[test]
fn consume_receiver_records_per_call_site_flag() {
    let output = run_checker(TRAIT_AND_IMPL, /*register_consume=*/ true);
    assert!(
        output
            .method_call_consumes_receiver
            .iter()
            .filter(|key| key.module_idx == 0)
            .count()
            == 2,
        "both root consuming calls must retain their acquisition metadata: {:?}",
        output.method_call_consumes_receiver,
    );
}

/// Sanity: the per-call-site flag in the checker output is keyed by the
/// span of the consuming call, so codegen can look it up by call site.
#[test]
fn flag_span_matches_consuming_call_site() {
    let source = r#"trait Sink {
    fn drain(self);
}

type Bucket {
    tag: string;
}

impl Sink for Bucket {
    fn drain(self) {}
}

fn main() {
    let b = Bucket { tag: "x" };
    b.drain();
}
"#;

    let program = parse_program(source);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.register_consume_receiver_method("Sink::drain");
    let output = checker.check_program(&program);

    let root_call_sites: Vec<&SpanKey> = output
        .method_call_consumes_receiver
        .iter()
        .filter(|key| key.module_idx == 0)
        .collect();
    assert_eq!(
        root_call_sites.len(),
        1,
        "expected exactly one root consuming call site; got {root_call_sites:?}",
    );

    let call_start = source.find("b.drain()").expect("root call is in source");
    let expected_call_end = call_start + "b.drain()".len();
    let key = root_call_sites[0];
    assert!(
        key.start <= call_start && expected_call_end <= key.end,
        "expected root span to cover `b.drain()` at {call_start}..{expected_call_end}; got {key:?}",
    );
    let snippet = source
        .get(key.start..key.end)
        .expect("root call span must be a valid source range");
    assert!(
        snippet.contains("b.drain"),
        "expected span to cover `b.drain(...)`; got {snippet:?} at {key:?}",
    );
}
