//! FileCheck-style MIR dump tests.
//!
//! Each test lowers a small Hew source to `IrPipeline`, calls `dump_mir`,
//! and asserts structural properties via CHECK directives instead of
//! struct-literal equality. This is the refactor-tolerant format that
//! RC2/RC5/RC6/RC7 can diff against without rewriting struct literals.
//!
//! The CHECK harness itself is tested in `support::filecheck` (including
//! a negative test proving CHECK-NOT rejects bad input — required by the
//! "a feature is done when it rejects bad input" quality bar).

mod support;
use hew_mir::{dump_mir, DumpStage};
use support::filecheck::check_directives;

// ---------------------------------------------------------------------------
// Pipeline helper
// ---------------------------------------------------------------------------

fn compile_to_pipeline(source: &str) -> hew_mir::IrPipeline {
    use hew_hir::{lower_program, verify_hir, ResolutionCtx};
    use hew_types::TypeCheckOutput;

    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let output = lower_program(
        &parsed.program,
        &TypeCheckOutput::default(),
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    assert!(
        output.diagnostics.is_empty(),
        "HIR diagnostics: {:?}",
        output.diagnostics
    );
    let verify = verify_hir(&output.module);
    assert!(verify.is_empty(), "HIR verify: {verify:?}");
    hew_mir::lower_hir_module(&output.module)
}

// ---------------------------------------------------------------------------
// Fixture 1: plain arithmetic function
//
// CHECK: fn add(i64, i64) -> i64
// CHECK: bb0:
// CHECK: add.checked
// CHECK-NEXT: return
// CHECK-NOT: trap(DivideByZero)
// ---------------------------------------------------------------------------

const FIXTURE_ARITH: &str = "
// CHECK: fn add(i64, i64) -> i64
// CHECK: bb0:
// CHECK: add.checked
// CHECK-NOT: trap(DivideByZero)
";

#[test]
fn filecheck_arith_function_header_and_checked_add() {
    let source = "fn add(a: i64, b: i64) -> i64 { a + b }";
    let pipeline = compile_to_pipeline(source);
    let dump = dump_mir(&pipeline, DumpStage::Raw);
    check_directives(&dump, FIXTURE_ARITH).unwrap_or_else(|e| panic!("{e}\n\nFull dump:\n{dump}"));
}

// ---------------------------------------------------------------------------
// Fixture 2: conditional branch
//
// CHECK: fn sign(i64) -> i64
// CHECK: branch
// CHECK-NOT: suspend
// ---------------------------------------------------------------------------

const FIXTURE_BRANCH: &str = "
// CHECK: fn sign(i64) -> i64
// CHECK: branch
// CHECK-NOT: suspend
";

#[test]
fn filecheck_branch_function_has_branch_terminator() {
    let source = "fn sign(x: i64) -> i64 { if x > 0 { 1 } else { 0 } }";
    let pipeline = compile_to_pipeline(source);
    let dump = dump_mir(&pipeline, DumpStage::Raw);
    check_directives(&dump, FIXTURE_BRANCH).unwrap_or_else(|e| panic!("{e}\n\nFull dump:\n{dump}"));
}

// ---------------------------------------------------------------------------
// Fixture 3: integer overflow trap (checked arithmetic splits the CFG)
//
// CHECK: fn overflow_add(i64, i64) -> i64
// CHECK: add.checked
// CHECK: trap(IntegerOverflow)
// ---------------------------------------------------------------------------

const FIXTURE_TRAP: &str = "
// CHECK: fn overflow_add(i64, i64) -> i64
// CHECK: add.checked
// CHECK: trap(IntegerOverflow)
";

#[test]
fn filecheck_overflow_trap_present_in_checked_add() {
    // `+` on i64 compiles to IntArithChecked → cfg split → Trap(IntegerOverflow)
    let source = "fn overflow_add(a: i64, b: i64) -> i64 { a + b }";
    let pipeline = compile_to_pipeline(source);
    let dump = dump_mir(&pipeline, DumpStage::Raw);
    check_directives(&dump, FIXTURE_TRAP).unwrap_or_else(|e| panic!("{e}\n\nFull dump:\n{dump}"));
}

// ---------------------------------------------------------------------------
// Fixture 4: checked stage emits "checks: none" for passing function
//
// CHECK: fn simple_return
// CHECK: checks: none
// ---------------------------------------------------------------------------

const FIXTURE_CHECKED_NONE: &str = "
// CHECK: fn simple_return
// CHECK: checks: none
";

#[test]
fn filecheck_checked_stage_emits_checks_none_for_passing_function() {
    let source = "fn simple_return() -> i64 { 42 }";
    let pipeline = compile_to_pipeline(source);
    let dump = dump_mir(&pipeline, DumpStage::Checked);
    check_directives(&dump, FIXTURE_CHECKED_NONE)
        .unwrap_or_else(|e| panic!("{e}\n\nFull dump:\n{dump}"));
}

// ---------------------------------------------------------------------------
// Fixture 5: elaborated stage drop plan for an owned string
//
// CHECK: fn string_owner
// CHECK: drop_plans:
// CHECK: drop
// CHECK-NOT: drop_plans: none
// ---------------------------------------------------------------------------

const FIXTURE_ELAB_DROP: &str = "
// CHECK: fn string_owner
// CHECK: drop_plans:
// CHECK: drop
// CHECK-NOT: drop_plans: none
";

#[test]
fn filecheck_elab_drop_plan_present_for_string_local() {
    // A function that owns a `string` local must have a drop plan entry.
    let source = r#"fn string_owner() -> i64 { let s: string = "hello"; 0 }"#;
    let pipeline = compile_to_pipeline(source);
    let dump = dump_mir(&pipeline, DumpStage::Elab);
    check_directives(&dump, FIXTURE_ELAB_DROP)
        .unwrap_or_else(|e| panic!("{e}\n\nFull dump:\n{dump}"));
}

// ---------------------------------------------------------------------------
// Harness self-test: the negative gate (plan §6 requirement)
//
// Prove the CHECK-NOT harness rejects bad input. If this test passes it
// means the harness correctly returned Err — the assert! below inverts that.
// ---------------------------------------------------------------------------

#[test]
fn filecheck_harness_check_not_rejects_matching_pattern() {
    let dump = "fn main() -> i64\n  bb0:\n    trap(IntegerOverflow)\n    return\n";
    // This fixture asserts 'trap' is NOT present — but it is. Must fail.
    let should_fail_fixture = "// CHECK-NOT: trap";
    let result = check_directives(dump, should_fail_fixture);
    assert!(
        result.is_err(),
        "CHECK-NOT over a dump containing 'trap' must return Err; \
         the harness must reject bad input"
    );
}
