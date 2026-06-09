//! FC-P1-D binary-operator HIR pre-pass gates.
//!
//! These tests pin the three fail-closed gates that close MIR audit sites
//! `:5336`, `:5564`, `:5696`. The gates run unconditionally on all targets
//! until MIR gains `TargetSpec` threading. LESSONS `boundary-fail-closed`.

use hew_hir::{lower_program, HirDiagnosticKind, ResolutionCtx, TargetArch};
use hew_types::{module_registry::ModuleRegistry, Checker};

fn typecheck_and_lower(source: &str) -> hew_hir::LowerOutput {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tco = checker.check_program(&parsed.program);
    // Some negative-control tests intentionally leave type errors out of scope
    // (e.g. range-in-for: the for/Iterator surface isn't fully checker-clean
    // in the in-tree harness), so we DON'T assert tco.errors empty here. The
    // gates we care about are still evaluated against expr_types.
    lower_program(&parsed.program, &tco, &ResolutionCtx, TargetArch::host())
}

fn has_diag<F>(output: &hew_hir::LowerOutput, pred: F) -> bool
where
    F: Fn(&HirDiagnosticKind) -> bool,
{
    output.diagnostics.iter().any(|d| pred(&d.kind))
}

// ── Gate 1: Range / RangeInclusive in value position ─────────────────────────

#[test]
fn range_in_value_position_rejected() {
    let output = typecheck_and_lower(
        r"
        fn main() {
            let r = 1..10;
        }
        ",
    );
    assert!(
        has_diag(&output, |k| matches!(
            k,
            HirDiagnosticKind::BinaryOperatorUnsupportedInMir { op } if op == ".."
        )),
        "expected BinaryOperatorUnsupportedInMir for `..`, got: {:?}",
        output.diagnostics
    );
    assert!(
        output.into_result().is_err(),
        "into_result() must fail on gated range"
    );
}

#[test]
fn range_inclusive_in_value_position_rejected() {
    let output = typecheck_and_lower(
        r"
        fn main() {
            let r = 1..=10;
        }
        ",
    );
    assert!(
        has_diag(&output, |k| matches!(
            k,
            HirDiagnosticKind::BinaryOperatorUnsupportedInMir { op } if op == "..="
        )),
        "expected BinaryOperatorUnsupportedInMir for `..=`, got: {:?}",
        output.diagnostics
    );
    assert!(output.into_result().is_err());
}

#[test]
fn range_in_for_loop_accepted() {
    let output = typecheck_and_lower(
        r"
        fn main() {
            for i in 1..10 {
            }
        }
        ",
    );
    assert!(
        !has_diag(&output, |k| matches!(
            k,
            HirDiagnosticKind::BinaryOperatorUnsupportedInMir { .. }
        )),
        "for-iterable range must be exempt; diagnostics: {:?}",
        output.diagnostics
    );
}

// ── Gate 2: div / mod on isize ───────────────────────────────────────────────

#[test]
fn isize_divide_rejected() {
    let output = typecheck_and_lower(
        r"
        fn op(x: isize, y: isize) -> isize {
            x / y
        }
        ",
    );
    assert!(
        has_diag(&output, |k| matches!(
            k,
            HirDiagnosticKind::PlatformSizedDivRemUnsupported { op } if op == "/"
        )),
        "expected PlatformSizedDivRemUnsupported `/`, got: {:?}",
        output.diagnostics
    );
    assert!(output.into_result().is_err());
}

#[test]
fn isize_modulo_rejected() {
    let output = typecheck_and_lower(
        r"
        fn op(x: isize, y: isize) -> isize {
            x % y
        }
        ",
    );
    assert!(
        has_diag(&output, |k| matches!(
            k,
            HirDiagnosticKind::PlatformSizedDivRemUnsupported { op } if op == "%"
        )),
        "expected PlatformSizedDivRemUnsupported `%`, got: {:?}",
        output.diagnostics
    );
    assert!(output.into_result().is_err());
}

#[test]
fn i64_divide_accepted() {
    let output = typecheck_and_lower(
        r"
        fn op(x: i64, y: i64) -> i64 {
            x / y
        }
        ",
    );
    assert!(
        !has_diag(&output, |k| matches!(
            k,
            HirDiagnosticKind::PlatformSizedDivRemUnsupported { .. }
        )),
        "i64 division must not be gated; diagnostics: {:?}",
        output.diagnostics
    );
}

// ── Gate 3: shift on isize / usize ───────────────────────────────────────────

#[test]
fn isize_shift_left_rejected() {
    let output = typecheck_and_lower(
        r"
        fn op(x: isize, n: i32) -> isize {
            x << n
        }
        ",
    );
    assert!(
        has_diag(&output, |k| matches!(
            k,
            HirDiagnosticKind::PlatformSizedShiftUnsupported { op } if op == "<<"
        )),
        "expected PlatformSizedShiftUnsupported `<<`, got: {:?}",
        output.diagnostics
    );
    assert!(output.into_result().is_err());
}

#[test]
fn usize_shift_right_rejected() {
    let output = typecheck_and_lower(
        r"
        fn op(x: usize, n: i32) -> usize {
            x >> n
        }
        ",
    );
    assert!(
        has_diag(&output, |k| matches!(
            k,
            HirDiagnosticKind::PlatformSizedShiftUnsupported { op } if op == ">>"
        )),
        "expected PlatformSizedShiftUnsupported `>>`, got: {:?}",
        output.diagnostics
    );
    assert!(output.into_result().is_err());
}

#[test]
fn i32_shift_accepted() {
    let output = typecheck_and_lower(
        r"
        fn op(x: i32, n: i32) -> i32 {
            x << n
        }
        ",
    );
    assert!(
        !has_diag(&output, |k| matches!(
            k,
            HirDiagnosticKind::PlatformSizedShiftUnsupported { .. }
        )),
        "i32 shift must not be gated; diagnostics: {:?}",
        output.diagnostics
    );
}

#[test]
fn comparison_on_isize_accepted() {
    let output = typecheck_and_lower(
        r"
        fn op(x: isize, y: isize) -> bool {
            x < y
        }
        ",
    );
    assert!(
        !has_diag(&output, |k| matches!(
            k,
            HirDiagnosticKind::PlatformSizedDivRemUnsupported { .. }
                | HirDiagnosticKind::PlatformSizedShiftUnsupported { .. }
        )),
        "comparison on isize must not trip div/shift gates; diagnostics: {:?}",
        output.diagnostics
    );
}

// Comparison-op coverage: `<=`, `>`, `>=` fall through `apply_binop_gates`
// the same way `<` does. These are not in the gated set; the assertion is
// "no gate diagnostic fires on the comparison itself". One per op.

#[test]
fn comparison_le_on_isize_accepted() {
    let output = typecheck_and_lower(
        r"
        fn op(x: isize, y: isize) -> bool {
            x <= y
        }
        ",
    );
    assert!(
        !has_diag(&output, |k| matches!(
            k,
            HirDiagnosticKind::PlatformSizedDivRemUnsupported { .. }
                | HirDiagnosticKind::PlatformSizedShiftUnsupported { .. }
                | HirDiagnosticKind::BinaryOperatorUnsupportedInMir { .. }
        )),
        "<= on isize must not trip any binop gate; diagnostics: {:?}",
        output.diagnostics
    );
}

#[test]
fn comparison_gt_on_isize_accepted() {
    let output = typecheck_and_lower(
        r"
        fn op(x: isize, y: isize) -> bool {
            x > y
        }
        ",
    );
    assert!(
        !has_diag(&output, |k| matches!(
            k,
            HirDiagnosticKind::PlatformSizedDivRemUnsupported { .. }
                | HirDiagnosticKind::PlatformSizedShiftUnsupported { .. }
                | HirDiagnosticKind::BinaryOperatorUnsupportedInMir { .. }
        )),
        "> on isize must not trip any binop gate; diagnostics: {:?}",
        output.diagnostics
    );
}

#[test]
fn comparison_ge_on_isize_accepted() {
    let output = typecheck_and_lower(
        r"
        fn op(x: isize, y: isize) -> bool {
            x >= y
        }
        ",
    );
    assert!(
        !has_diag(&output, |k| matches!(
            k,
            HirDiagnosticKind::PlatformSizedDivRemUnsupported { .. }
                | HirDiagnosticKind::PlatformSizedShiftUnsupported { .. }
                | HirDiagnosticKind::BinaryOperatorUnsupportedInMir { .. }
        )),
        ">= on isize must not trip any binop gate; diagnostics: {:?}",
        output.diagnostics
    );
}

// ── Machine bodies: walker must reach state entry/exit + transition guard/body

#[test]
fn binop_in_machine_transition_body_rejected() {
    // `x / y` on isize inside a transition body must be caught by the
    // FC-P1-D walker. The transition body is lowered via
    // `lower_machine_expr_filtered` (lower.rs ~:4201) so a missed scan
    // would surface at the MIR producer. `ident` returns isize so the
    // checker types the operands as isize (machine fields use `this.field`,
    // which is checker-supported in transition bodies but not exercised
    // here — we want operand typing independent of state-binding mechanics).
    let output = typecheck_and_lower(
        r"
        fn ident(x: isize) -> isize { x }

        machine M {
            events {
                Go;
            }

            state A { n: i32; }
            state B { n: i32; }


            on Go: A => B {
                let _ = ident(0) / ident(0);
                B { n: 0 }
            }
            on Go: B => A {
                A { n: 0 }
            }
        }
        ",
    );
    assert!(
        has_diag(&output, |k| matches!(
            k,
            HirDiagnosticKind::PlatformSizedDivRemUnsupported { op } if op == "/"
        )),
        "expected PlatformSizedDivRemUnsupported `/` from machine transition \
         body; diagnostics: {:?}",
        output.diagnostics
    );
}

#[test]
fn binop_in_machine_state_entry_rejected() {
    // `entry { ... }` block is lowered via `lower_machine_block_filtered`
    // (lower.rs ~:4137). A binop in entry must be caught. Use Gate 1
    // (Range in value position) — it fires unconditionally without needing
    // operand-type info, which is the cleanest probe that the walker visits
    // entry blocks at all.
    let output = typecheck_and_lower(
        r"
        machine M {
            events {
                Go;
            }

            state A {
                n: i32;
                entry {
                    let _ = 1..10;
                    A { n: 0 }
                }
            }
            state B { n: i32; }


            on Go: A => B { B { n: 0 } }
            on Go: B => A { A { n: 0 } }
        }
        ",
    );
    assert!(
        has_diag(&output, |k| matches!(
            k,
            HirDiagnosticKind::BinaryOperatorUnsupportedInMir { op } if op == ".."
        )),
        "expected BinaryOperatorUnsupportedInMir `..` from machine state \
         entry block; diagnostics: {:?}",
        output.diagnostics
    );
}

#[test]
fn binop_in_machine_state_exit_rejected() {
    // Same probe shape as entry, using `..=` to differentiate.
    let output = typecheck_and_lower(
        r"
        machine M {
            events {
                Go;
            }

            state A {
                n: i32;
                exit {
                    let _ = 1..=10;
                    A { n: 0 }
                }
            }
            state B { n: i32; }


            on Go: A => B { B { n: 0 } }
            on Go: B => A { A { n: 0 } }
        }
        ",
    );
    assert!(
        has_diag(&output, |k| matches!(
            k,
            HirDiagnosticKind::BinaryOperatorUnsupportedInMir { op } if op == "..="
        )),
        "expected BinaryOperatorUnsupportedInMir `..=` from machine state \
         exit block; diagnostics: {:?}",
        output.diagnostics
    );
}

#[test]
fn binop_in_machine_transition_guard_rejected() {
    // `when` guard expression is lowered through the same expr surface as
    // the body. A binop here must be caught.
    let output = typecheck_and_lower(
        r"
        fn ident(x: isize) -> isize { x }

        machine M {
            events {
                Go;
            }

            state A { n: i32; }
            state B { n: i32; }


            on Go: A => B when (ident(0) >> 2) == ident(0) {
                B { n: 0 }
            }
            on Go: A => B { B { n: 0 } }
            on Go: B => A { A { n: 0 } }
        }
        ",
    );
    assert!(
        has_diag(&output, |k| matches!(
            k,
            HirDiagnosticKind::PlatformSizedShiftUnsupported { op } if op == ">>"
        )),
        "expected PlatformSizedShiftUnsupported `>>` from machine transition \
         guard; diagnostics: {:?}",
        output.diagnostics
    );
}
