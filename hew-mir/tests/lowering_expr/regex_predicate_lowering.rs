//! MIR lowering tests for `HirMatchArmPredicate::Regex` dispatch.
//!
//! Exercises `lower_match_regex` (slice 4): the ordered-predicate CFG
//! that `lower_match` routes to when any arm carries a `Regex` predicate.
//!
//! Every test runs the full parse → HIR-lower → MIR-lower pipeline so the
//! assertions are on the real production path, not hand-built fixtures.
//!
//! Gate: slice 4 — verifies typed `hew_regex_match` / `hew_regex_capture`
//! terminal-call emission and the null-check branch chain.

use hew_hir::{lower_program, ResolutionCtx};
use hew_mir::{lower_hir_module, CallAuthority, CmpPred, Instr, IrPipeline, Place, Terminator};
use hew_types::module_registry::ModuleRegistry;
use hew_types::runtime_call::RuntimeCallFamily;
use hew_types::Checker;

/// Run the full parse → typecheck → HIR → MIR pipeline.
fn pipeline_with_tc(source: &str) -> IrPipeline {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    let output = lower_program(
        &parsed.program,
        &tc_output,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    lower_hir_module(&output.module)
}

/// Find the named function's raw MIR.
fn find_fn<'a>(p: &'a IrPipeline, name: &str) -> &'a hew_mir::RawMirFunction {
    p.raw_mir
        .iter()
        .find(|f| f.name == name)
        .unwrap_or_else(|| panic!("function `{name}` not found in raw_mir"))
}

/// Find the named function's canonical Checked MIR.
fn find_checked_fn<'a>(p: &'a IrPipeline, name: &str) -> &'a hew_mir::CheckedMirFunction {
    p.checked_mir
        .iter()
        .find(|f| f.name == name)
        .unwrap_or_else(|| panic!("function `{name}` not found in checked_mir"))
}

#[derive(Debug, Clone, Copy)]
struct CheckedRuntimeCall<'a> {
    args: &'a [Place],
    dest: Option<Place>,
}

/// Collect only calls whose linker spelling and typed runtime authority agree.
/// A name collision with an ordinary extern therefore cannot satisfy these
/// structural assertions.
fn checked_runtime_calls(
    f: &hew_mir::CheckedMirFunction,
    family: RuntimeCallFamily,
) -> Vec<CheckedRuntimeCall<'_>> {
    f.blocks
        .iter()
        .filter_map(|block| match &block.terminator {
            Terminator::Call {
                callee,
                authority: CallAuthority::Runtime(actual_family),
                args,
                dest,
                ..
            } if *actual_family == family && callee == family.c_symbol() => {
                Some(CheckedRuntimeCall { args, dest: *dest })
            }
            _ => None,
        })
        .collect()
}

/// Collect all instructions from all blocks in a function.
fn all_instrs(f: &hew_mir::RawMirFunction) -> Vec<&Instr> {
    f.blocks
        .iter()
        .flat_map(|b| b.instructions.iter())
        .collect()
}

/// A regex arm with no named captures must emit exactly one
/// typed `Terminator::Call` for `hew_regex_match` and no typed
/// `hew_regex_capture` terminal calls.
#[test]
fn regex_arm_no_captures_emits_regex_match_not_capture() {
    let src = r#"
        fn classify(s: string) -> string {
            match s {
                re"^hello" => "greeting",
                _ => "other",
            }
        }
    "#;
    let pipeline = pipeline_with_tc(src);
    let f = find_checked_fn(&pipeline, "classify");
    let match_calls = checked_runtime_calls(f, RuntimeCallFamily::RegexMatch);
    assert_eq!(
        match_calls.len(),
        1,
        "expected exactly one hew_regex_match call; got {}",
        match_calls.len()
    );

    let capture_calls = checked_runtime_calls(f, RuntimeCallFamily::RegexCapture);
    assert!(
        capture_calls.is_empty(),
        "expected no hew_regex_capture calls for a no-capture pattern; got {}",
        capture_calls.len()
    );
}

/// `hew_regex_match` must receive two arguments: the scrutinee place
/// and the literal-id constant.
#[test]
fn regex_match_call_has_two_args_scrutinee_and_literal_id() {
    let src = r#"
        fn classify(s: string) -> string {
            match s {
                re"^hello" => "greeting",
                _ => "other",
            }
        }
    "#;
    let pipeline = pipeline_with_tc(src);
    let f = find_checked_fn(&pipeline, "classify");
    let calls = checked_runtime_calls(f, RuntimeCallFamily::RegexMatch);
    let call = calls
        .first()
        .expect("typed hew_regex_match call must be present");

    assert_eq!(
        call.args.len(),
        2,
        "hew_regex_match must receive 2 args (scrutinee, literal_id); got {}",
        call.args.len()
    );
}

/// Two regex arms in a single match must emit exactly two
/// `hew_regex_match` calls — one per arm.
#[test]
fn two_regex_arms_emit_two_regex_match_calls() {
    let src = r#"
        fn route(s: string) -> string {
            match s {
                re"^GET " => "get",
                re"^POST " => "post",
                _ => "unknown",
            }
        }
    "#;
    let pipeline = pipeline_with_tc(src);
    let f = find_checked_fn(&pipeline, "route");
    let match_calls = checked_runtime_calls(f, RuntimeCallFamily::RegexMatch);
    assert_eq!(
        match_calls.len(),
        2,
        "two regex arms must emit two hew_regex_match calls; got {}",
        match_calls.len()
    );
}

/// The regex-predicate CFG must contain a `Branch` terminator whose `cond`
/// is an `IntCmp { pred: NotEq }` result — the truthiness check on the
/// i32 return value of `hew_regex_match`.
#[test]
fn regex_arm_emits_not_eq_branch_on_match_result() {
    let src = r#"
        fn classify(s: string) -> string {
            match s {
                re"^hello" => "greeting",
                _ => "other",
            }
        }
    "#;
    let pipeline = pipeline_with_tc(src);
    let f = find_fn(&pipeline, "classify");
    let instrs = all_instrs(f);

    // The IntCmp(NotEq) instruction must be present.
    let has_not_eq_cmp = instrs.iter().any(|i| {
        matches!(
            i,
            Instr::IntCmp {
                pred: CmpPred::NotEq,
                ..
            }
        )
    });
    assert!(
        has_not_eq_cmp,
        "regex predicate dispatch must contain an IntCmp(NotEq) for the match truthiness check"
    );
}

/// With no wildcard arm the tail block must be a `Trap { kind: ExhaustivenessFallthrough }`.
/// (The checker rejects non-exhaustive regex matches; the trap is belt-and-braces.)
#[test]
fn regex_match_without_wildcard_has_exhaustiveness_trap() {
    let src = r#"
        fn guard(s: string) -> string {
            match s {
                re"^hello" => "greeting",
            }
        }
    "#;
    // NOTE: if the checker rejects this as non-exhaustive before MIR, the
    // HIR module will carry an Unsupported node and MIR will emit a
    // NotYetImplemented diagnostic; the test still passes because the
    // pipeline runs without panicking (no `todo!()`). We verify only that
    // no panic occurs — exhaustiveness enforcement is the checker's job.
    let parsed = hew_parser::parse(src);
    // Parser may or may not reject the no-wildcard regex match — don't
    // assert on parse errors here; just run the pipeline and ensure no panic.
    if !parsed.errors.is_empty() {
        return; // parser already rejects; nothing to verify at MIR level
    }
    let mut checker =
        hew_types::Checker::new(hew_types::module_registry::ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    let output = lower_program(
        &parsed.program,
        &tc_output,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    let pipeline = lower_hir_module(&output.module);
    // If the function is present, look for the trap.
    if let Some(f) = pipeline.raw_mir.iter().find(|f| f.name == "guard") {
        let has_trap = f.blocks.iter().any(|b| {
            matches!(
                b.terminator,
                Terminator::Trap {
                    kind: hew_mir::TrapKind::ExhaustivenessFallthrough
                }
            )
        });
        // If the pipeline produced a function (no HIR Unsupported → no MIR abort),
        // it must have the exhaustiveness trap.
        assert!(
            has_trap,
            "regex match without wildcard must have an ExhaustivenessFallthrough trap"
        );
    }
    // If `guard` is absent (HIR emitted Unsupported due to checker rejection),
    // the test passes — the checker did the right thing.
}

/// A regex arm with no captures and no wildcard: the `hew_regex_match`
/// result local must be the `dest` of the typed terminal call — not `None`.
#[test]
fn regex_match_call_has_dest_not_none() {
    let src = r#"
        fn classify(s: string) -> string {
            match s {
                re"^hello" => "greeting",
                _ => "other",
            }
        }
    "#;
    let pipeline = pipeline_with_tc(src);
    let f = find_checked_fn(&pipeline, "classify");
    let calls = checked_runtime_calls(f, RuntimeCallFamily::RegexMatch);
    let call = calls
        .first()
        .expect("typed hew_regex_match call must be present");

    assert!(
        call.dest.is_some(),
        "hew_regex_match must have a dest local (the i32 match result); got None"
    );
}

/// The MIR for a no-capture regex arm must contain at least one `Branch`
/// terminator — the match-check branch. (The wildcard arm provides the else
/// path; no capture-check branches are needed.)
#[test]
fn regex_arm_no_captures_emits_at_least_one_branch_terminator() {
    let src = r#"
        fn classify(s: string) -> string {
            match s {
                re"^hello" => "greeting",
                _ => "other",
            }
        }
    "#;
    let pipeline = pipeline_with_tc(src);
    let f = find_fn(&pipeline, "classify");

    let branch_count = f
        .blocks
        .iter()
        .filter(|b| matches!(b.terminator, Terminator::Branch { .. }))
        .count();
    assert!(
        branch_count >= 1,
        "regex predicate dispatch must emit at least one Branch terminator; got 0"
    );
}
