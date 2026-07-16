//! `while let` fail-closed refusal for a skipped OWNED enum payload field.
//!
//! A `while let` header re-evaluates the scrutinee every iteration, and a
//! payload field left unbound (by `..` rest or a bare `_` sibling) has no
//! per-iteration release site — a reassigned scrutinee leaks the prior
//! iteration's value. Unlike `match` / `if let` / `let ... else`, which
//! discharge the skipped owned sibling through the enum composite drop, the
//! loop position cannot, so MIR lowering refuses the shape rather than emit a
//! leak. `BitCopy` skips (no heap) and fully-bound payloads are unaffected.

use hew_hir::{lower_program, ResolutionCtx};
use hew_mir::{lower_hir_module, IrPipeline, MirDiagnosticKind};
use hew_types::{module_registry::ModuleRegistry, Checker};

fn pipeline_allow_diags(source: &str) -> IrPipeline {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    assert!(
        tc_output.errors.is_empty(),
        "type errors: {:#?}",
        tc_output.errors
    );
    let output = lower_program(
        &parsed.program,
        &tc_output,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    assert!(
        output.diagnostics.is_empty(),
        "HIR diagnostics: {:#?}",
        output.diagnostics
    );
    lower_hir_module(&output.module)
}

fn skipped_owned_refusals(pipeline: &IrPipeline) -> usize {
    pipeline
        .diagnostics
        .iter()
        .filter(|d| {
            matches!(
                &d.kind,
                MirDiagnosticKind::NotYetImplemented { construct, .. }
                    if construct == "while-let skipped owned enum payload field"
            )
        })
        .count()
}

#[test]
fn while_let_skipped_owned_payload_field_rejected_rest() {
    // `..` leaves the owned `b` field unbound.
    let source = r#"
enum Packet {
    Data { a: string, b: string };
    Empty;
}

fn make() -> Packet {
    Packet::Data { a: "a".to_upper(), b: "b".to_upper() }
}

fn main() {
    var p = make();
    var total = 0;
    while let Packet::Data { a, .. } = p {
        total = total + a.len();
        p = Packet::Empty;
    }
    if total > 0 {
        print("w");
    }
}"#;
    let pipeline = pipeline_allow_diags(source);
    assert_eq!(
        skipped_owned_refusals(&pipeline),
        1,
        "a `while let` rest that skips an owned payload field must fail closed: {:#?}",
        pipeline.diagnostics
    );
}

#[test]
fn while_let_skipped_owned_payload_field_rejected_wildcard() {
    // Explicit `b: _` skips the owned `b` field — same refusal as `..`.
    let source = r#"
enum Packet {
    Data { a: string, b: string };
    Empty;
}

fn make() -> Packet {
    Packet::Data { a: "a".to_upper(), b: "b".to_upper() }
}

fn main() {
    var p = make();
    var total = 0;
    while let Packet::Data { a, b: _ } = p {
        total = total + a.len();
        p = Packet::Empty;
    }
    if total > 0 {
        print("w");
    }
}"#;
    let pipeline = pipeline_allow_diags(source);
    assert_eq!(
        skipped_owned_refusals(&pipeline),
        1,
        "an explicit `_` on an owned payload sibling must fail closed: {:#?}",
        pipeline.diagnostics
    );
}

#[test]
fn while_let_bitcopy_skip_is_admitted() {
    // A skipped BitCopy field owns no heap — nothing to leak, so it lowers.
    let source = r#"
enum Pairs {
    Pair { x: i64, y: i64 };
    Empty;
}

fn make() -> Pairs {
    Pairs::Pair { x: 1, y: 2 }
}

fn main() {
    var p = make();
    var total = 0;
    while let Pairs::Pair { x, .. } = p {
        total = total + x;
        p = Pairs::Empty;
    }
    if total > 0 {
        print("ok");
    }
}"#;
    let pipeline = pipeline_allow_diags(source);
    assert_eq!(
        skipped_owned_refusals(&pipeline),
        0,
        "a BitCopy skip owns no heap and must not be refused: {:#?}",
        pipeline.diagnostics
    );
}

#[test]
fn while_let_fully_bound_owned_payload_is_admitted() {
    // Every owned payload field is bound — rides the composite/back-edge drop.
    let source = r#"
enum Packet {
    Data { a: string, b: string };
    Empty;
}

fn make() -> Packet {
    Packet::Data { a: "a".to_upper(), b: "b".to_upper() }
}

fn main() {
    var p = make();
    var total = 0;
    while let Packet::Data { a, b } = p {
        total = total + a.len() + b.len();
        p = Packet::Empty;
    }
    if total > 0 {
        print("w");
    }
}"#;
    let pipeline = pipeline_allow_diags(source);
    assert_eq!(
        skipped_owned_refusals(&pipeline),
        0,
        "a fully-bound owned payload must lower: {:#?}",
        pipeline.diagnostics
    );
}
