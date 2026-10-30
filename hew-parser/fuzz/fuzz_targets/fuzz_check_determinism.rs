//! Checker determinism and declaration-order independence.
//!
//! Two fresh checkers over the same parse-clean program must publish the same
//! diagnostics, in the same order, and the same resolved type facts. Raw
//! expression types are not compared: their inference variables number from
//! a process-wide counter. The
//! specification makes constant references independent of declaration order
//! (§3.2), so reordering the module's `const` items among their own slots must
//! not change whether the program checks.

#![no_main]

mod support;

use hew_parser::Severity;
use hew_parser::ast::Item;
use hew_types::TypeCheckOutput;
use hew_types::error::Severity as CheckSeverity;
use hew_types::error::TypeErrorKind;
use libfuzzer_sys::fuzz_target;

/// The observable content of one diagnostic, in emission order.
type DiagnosticView<'a> = (
    &'a hew_types::error::TypeErrorKind,
    CheckSeverity,
    &'a std::ops::Range<usize>,
    &'a str,
    &'a [hew_types::error::TypeErrorNote],
    &'a [String],
    Option<&'a str>,
);

fn diagnostics(errors: &[hew_types::TypeError]) -> Vec<DiagnosticView<'_>> {
    errors
        .iter()
        .map(|e| {
            (
                &e.kind,
                e.severity,
                &e.span,
                e.message.as_str(),
                e.notes.as_slice(),
                e.suggestions.as_slice(),
                e.source_module.as_deref(),
            )
        })
        .collect()
}

/// Warnings in emission order, with the unused-binding warnings set apart
/// and sorted by span.
///
/// Deviation: the checker emits unused-variable and unused-`var` warnings in
/// hash order, which
/// is a known defect being fixed separately. Until it lands they are compared
/// as a set so the target can reach other differences. Compare the whole list
/// in order once the emission order is fixed.
fn warnings(
    warnings: &[hew_types::TypeError],
) -> (Vec<DiagnosticView<'_>>, Vec<DiagnosticView<'_>>) {
    let unused = |e: &&DiagnosticView<'_>| {
        matches!(
            e.0,
            TypeErrorKind::UnusedVariable | TypeErrorKind::UnusedMut
        )
    };
    let all = diagnostics(warnings);
    let mut unused_set: Vec<_> = all.iter().filter(unused).cloned().collect();
    unused_set.sort_by_key(|e| (e.2.start, e.2.end));
    let rest = all.iter().filter(|e| !unused(e)).cloned().collect();
    (rest, unused_set)
}

fn assert_same(source: &str, first: &TypeCheckOutput, second: &TypeCheckOutput) {
    assert_eq!(
        diagnostics(&first.errors),
        diagnostics(&second.errors),
        "errors differ between two checks\n--- source ---\n{source}"
    );
    assert_eq!(
        warnings(&first.warnings),
        warnings(&second.warnings),
        "warnings differ between two checks\n--- source ---\n{source}"
    );
    assert!(
        first.resolved_expr_types == second.resolved_expr_types,
        "resolved expression types differ between two checks\n--- source ---\n{source}"
    );
    assert!(
        first.resolved_binding_types == second.resolved_binding_types,
        "binding types differ between two checks\n--- source ---\n{source}"
    );
    assert!(
        first.resolved_annotation_types == second.resolved_annotation_types,
        "annotation types differ between two checks\n--- source ---\n{source}"
    );
}

/// `source` with its top-level `const` items rotated one slot: each const's
/// text moves into the next const's slot. `None` when there are fewer than two.
fn rotate_consts(source: &str, program: &hew_parser::ast::Program) -> Option<String> {
    let slots: Vec<std::ops::Range<usize>> = program
        .items
        .iter()
        .filter(|(item, _)| matches!(item, Item::Const(_)))
        .map(|(_, span)| span.clone())
        .collect();
    if slots.len() < 2 || slots.windows(2).any(|w| w[0].end > w[1].start) {
        return None;
    }
    let mut out = String::with_capacity(source.len());
    let mut at = 0;
    for (index, slot) in slots.iter().enumerate() {
        let moved = &slots[(index + slots.len() - 1) % slots.len()];
        out.push_str(&source[at..slot.start]);
        out.push_str(&source[moved.clone()]);
        at = slot.end;
    }
    out.push_str(&source[at..]);
    Some(out)
}

fn check(program: &hew_parser::ast::Program) -> TypeCheckOutput {
    support::checker().check_program(program)
}

fuzz_target!(|data: &[u8]| {
    let Ok(source) = std::str::from_utf8(data) else {
        return;
    };
    let parsed = hew_parser::parse(source);
    if parsed.errors.iter().any(|e| e.severity == Severity::Error) {
        return;
    }
    let first = check(&parsed.program);
    let second = check(&parsed.program);
    assert_same(source, &first, &second);

    let Some(rotated) = rotate_consts(source, &parsed.program) else {
        return;
    };
    let reparsed = hew_parser::parse(&rotated);
    if reparsed
        .errors
        .iter()
        .any(|e| e.severity == Severity::Error)
    {
        return;
    }
    let reordered = check(&reparsed.program);
    assert_eq!(
        first.errors.is_empty(),
        reordered.errors.is_empty(),
        "reordering const declarations changed the verdict\n--- source ---\n{source}\n--- reordered ---\n{rotated}\n--- errors ---\n{:?}\n--- reordered errors ---\n{:?}",
        first.errors.iter().map(|e| &e.message).collect::<Vec<_>>(),
        reordered
            .errors
            .iter()
            .map(|e| &e.message)
            .collect::<Vec<_>>(),
    );
});
