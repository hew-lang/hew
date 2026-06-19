//! End-to-end emission tests for the per-kind `Terminator::Trap`
//! lowering. The MIR carries a `TrapKind` discriminant; codegen must
//! route each kind through `hew_trap_with_code(<code>)` before the
//! fallback `llvm.trap` so the supervisor can distinguish overflow
//! from OOB from divide-by-zero (instead of collapsing every cause
//! to `Signal(4)` / `Signal(5)`).
//!
//! The exit-code constants here MUST stay in lock-step with
//! `HEW_TRAP_*` in `hew-runtime/src/internal/types.rs`; native
//! `supervisor.rs` re-exports those constants.
//!
//! LESSONS applied:
//! - `boundary-fail-closed` (P0): the trap path must contain a real
//!   call to `hew_trap_with_code` with the correct per-kind code AND
//!   still emit `llvm.trap` + `unreachable` as the verifier-required
//!   block terminator / non-actor fallback.
//! - `exhaustive-coverage` (P0): one assertion per `TrapKind` value
//!   reachable from the producer-side MIR slices that exist today
//!   (B-2 IntegerOverflow, B-5 DivideByZero / ShiftOutOfRange,
//!   C-2 IndexOutOfBounds via Vec OOB). `SignedMinDivNegOne` shares
//!   the same divide trap surface as `DivideByZero`; we exercise it
//!   through an explicit MIR fixture rather than a source snippet
//!   because the producer fires on the dividend-side check too.

use std::path::Path;

use hew_codegen_rs::{emit_module, EmitOptions};
use hew_hir::{lower_program, verify_hir, ResolutionCtx};
use hew_mir::{
    BasicBlock, BlockKind, CheckedMirFunction, DropPlan, ElabBlock, ElaboratedMirFunction,
    ExitPath, IrPipeline, RawMirFunction, Terminator, TrapKind,
};
use hew_types::TypeCheckOutput;

fn emit_ll(source: &str, module_name: &str) -> String {
    let parsed = hew_parser::parse(source);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let output = lower_program(
        &parsed.program,
        &TypeCheckOutput::default(),
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    assert!(
        output.diagnostics.is_empty() && verify_hir(&output.module).is_empty(),
        "hir: {:?} verify: {:?}",
        output.diagnostics,
        verify_hir(&output.module)
    );
    let pipeline = hew_mir::lower_hir_module(&output.module);
    assert!(
        pipeline.diagnostics.is_empty(),
        "mir diagnostics: {:?}",
        pipeline.diagnostics
    );
    let tmp = std::env::temp_dir().join(format!("hew-trap-kind-{module_name}"));
    std::fs::create_dir_all(&tmp).expect("create out_dir");
    let options = EmitOptions {
        module_name,
        out_dir: &tmp,
        native: false,
        wasm: false,
        target_triple: None,
        debug: false,
        source_path: None,
    };
    let artefacts =
        emit_module(&pipeline, &options).expect("trap-kind pipeline must emit successfully");
    let ll_path: &Path = artefacts
        .ll_path
        .as_deref()
        .expect("emit_module must populate ll_path");
    std::fs::read_to_string(ll_path).expect("read emitted .ll")
}

fn emit_trap_kind_ll(kind: TrapKind, module_name: &str) -> String {
    let raw_blocks = vec![BasicBlock {
        id: 0,
        statements: vec![],
        instructions: vec![],
        terminator: Terminator::Trap { kind },
    }];
    let pipeline = IrPipeline {
        thir: vec![],
        raw_mir: vec![RawMirFunction {
            name: "trap_probe".to_string(),
            return_ty: hew_types::ResolvedTy::Unit,
            call_conv: hew_mir::FunctionCallConv::Default,
            params: vec![],
            locals: vec![],
            blocks: raw_blocks.clone(),
            decisions: vec![],
            intrinsic_id: None,
            await_deadline_ns: std::collections::HashMap::new(),

            lambda_actor_user_param_locals: Vec::new(),
            span: None,
            instr_spans: ::std::collections::BTreeMap::new(),
        }],
        checked_mir: vec![CheckedMirFunction {
            name: "trap_probe".to_string(),
            return_ty: hew_types::ResolvedTy::Unit,
            blocks: raw_blocks,
            decisions: vec![],
            checks: vec![],
            cooperate_sites: vec![],
        }],
        elaborated_mir: vec![ElaboratedMirFunction {
            name: "trap_probe".to_string(),
            return_ty: hew_types::ResolvedTy::Unit,
            statements: vec![],
            decisions: vec![],
            blocks: vec![ElabBlock {
                id: 0,
                kind: BlockKind::Normal,
                drops: vec![],
                successor: None,
            }],
            drop_plans: vec![(ExitPath::Return { block: 0 }, DropPlan::default())],
            coroutine: None,
            lambda_captures: vec![],
        }],
        diagnostics: vec![],
        wire_layouts: std::sync::Arc::default(),
        opaque_handle_names: vec![],
        record_layouts: vec![],
        actor_layouts: vec![],
        supervisor_layouts: vec![],
        machine_layouts: vec![],
        enum_layouts: vec![],
        regex_literals: vec![],
        user_consts: Vec::new(),
        extern_decls: vec![],
        dyn_vtable_registry: vec![],
        hashmap_lowering_facts: vec![],
        hashset_lowering_facts: vec![],
        actor_send_aliasing: std::collections::HashMap::new(),
        polymorphic_mir: Vec::new(),
        user_clone_record_seeds: vec![],
    };
    let tmp = std::env::temp_dir().join(format!("hew-trap-kind-{module_name}"));
    std::fs::create_dir_all(&tmp).expect("create out_dir");
    let options = EmitOptions {
        module_name,
        out_dir: &tmp,
        native: false,
        wasm: false,
        target_triple: None,
        debug: false,
        source_path: None,
    };
    let artefacts = emit_module(&pipeline, &options).expect("trap-kind MIR fixture must emit");
    let ll_path: &Path = artefacts
        .ll_path
        .as_deref()
        .expect("emit_module must populate ll_path");
    std::fs::read_to_string(ll_path).expect("read emitted .ll")
}

/// Assert the emitted IR contains a call to `hew_trap_with_code` with
/// the given i32 argument. inkwell's textual IR formats integer
/// constants as `i32 N` so we match on a substring that pins both the
/// callee symbol and the constant.
fn assert_trap_with_code(ll: &str, code: i32) {
    let needle = format!("@hew_trap_with_code(i32 {code})");
    assert!(
        ll.contains(&needle),
        "expected `{needle}` in emitted IR; got:\n{ll}"
    );
    // The fallback / verifier-required terminator must still follow.
    let call_idx = ll
        .find(&needle)
        .unwrap_or_else(|| panic!("expected `{needle}` in emitted IR; got:\n{ll}"));
    let fallback = &ll[call_idx..];
    assert!(
        fallback.contains("@llvm.trap"),
        "trap block must still emit `llvm.trap` after `{needle}` as the fallback terminator; got:\n{ll}"
    );
    assert!(
        fallback.contains("unreachable"),
        "trap block must end with `unreachable` after `{needle}`; got:\n{ll}"
    );
}

/// `hew_trap_with_code` must be declared exactly once per module even
/// if multiple trap sites reference it.
fn assert_trap_decl_unique(ll: &str) {
    // The textual IR has one `declare void @hew_trap_with_code` line
    // when the symbol is used externally; assert we didn't accidentally
    // emit duplicate declarations.
    let decl_count = ll.matches("declare void @hew_trap_with_code").count();
    assert_eq!(
        decl_count, 1,
        "expected exactly one `declare void @hew_trap_with_code` line, got {decl_count}:\n{ll}"
    );
}

// ---------------------------------------------------------------------------
// IntegerOverflow — code 201
// ---------------------------------------------------------------------------

#[test]
fn integer_overflow_emits_hew_trap_with_code_201() {
    let ll = emit_ll("fn main() -> i64 { 1 + 2 }", "integer_overflow");
    assert_trap_with_code(&ll, 201);
    assert_trap_decl_unique(&ll);
}

// ---------------------------------------------------------------------------
// DivideByZero — code 202
// ---------------------------------------------------------------------------

#[test]
fn divide_by_zero_emits_hew_trap_with_code_202() {
    // The divide-by-zero trap surface is the LHS check in `/` / `%`.
    // The shape mirrors B-5's existing div_shift_trap_emission test.
    let ll = emit_ll(
        "fn main() -> i64 { let a: i64 = 10; let b: i64 = 2; a / b }",
        "divide_by_zero",
    );
    assert_trap_with_code(&ll, 202);
}

// ---------------------------------------------------------------------------
// SignedMinDivNegOne — code 203
// ---------------------------------------------------------------------------

#[test]
fn signed_min_div_neg_one_emits_hew_trap_with_code_203() {
    let ll = emit_trap_kind_ll(TrapKind::SignedMinDivNegOne, "signed_min_div_neg_one");
    assert_trap_with_code(&ll, 203);
}

// ---------------------------------------------------------------------------
// ShiftOutOfRange — code 204
// ---------------------------------------------------------------------------

#[test]
fn shift_out_of_range_emits_hew_trap_with_code_204() {
    let ll = emit_ll(
        "fn main() -> i64 { let a: i64 = 1; let b: i64 = 3; a << b }",
        "shift_out_of_range",
    );
    assert_trap_with_code(&ll, 204);
}

// ---------------------------------------------------------------------------
// IndexOutOfBounds — code 205
// ---------------------------------------------------------------------------

#[test]
fn index_out_of_bounds_emits_hew_trap_with_code_205() {
    let ll = emit_trap_kind_ll(TrapKind::IndexOutOfBounds, "index_out_of_bounds");
    assert_trap_with_code(&ll, 205);
}

// ---------------------------------------------------------------------------
// SupervisorChildUnavailable / actor-send discriminator — code 206
// ---------------------------------------------------------------------------

#[test]
fn supervisor_child_unavailable_emits_hew_trap_with_code_206() {
    let ll = emit_trap_kind_ll(
        TrapKind::SupervisorChildUnavailable,
        "supervisor_child_unavailable",
    );
    assert_trap_with_code(&ll, 206);
}

// ---------------------------------------------------------------------------
// Multiple trap sites in one module reuse the same declaration.
// ---------------------------------------------------------------------------

#[test]
fn multiple_traps_share_one_declaration() {
    // Two arithmetic ops → two trap blocks → still one declare line.
    let ll = emit_ll("fn main() -> i64 { (1 + 2) * (3 + 4) }", "multiple_traps");
    assert_trap_decl_unique(&ll);
    // Both call sites present (same code for both overflow traps).
    let call_count = ll.matches("@hew_trap_with_code(i32 201)").count();
    assert!(
        call_count >= 2,
        "expected at least two `hew_trap_with_code(i32 201)` call sites, got {call_count}:\n{ll}"
    );
}

// ---------------------------------------------------------------------------
// G2 — trap-code cross-crate parity (codegen literal ↔ runtime constant)
//
// The codegen-emitted code MUST equal the runtime `HEW_TRAP_*` constant. The
// production code already single-sources these via `use
// hew_runtime::internal::types::HEW_TRAP_*`, so a renumber on either side is a
// build error. This test is the behavioural backstop: it pins the *emitted IR*
// to the runtime constant (not to a hand-copied literal), so the proof lives in
// the compiled artefact, not a convention. A runtime renumber that somehow
// compiled would still go red here. Closes TRAP-1 (`codegen-offset-mirror-drift`
// family extended from offsets to discriminant codes).
// ---------------------------------------------------------------------------

#[test]
fn emitted_trap_codes_equal_runtime_constants() {
    use hew_runtime::internal::types::{
        HEW_TRAP_ACTOR_SEND_FAILED, HEW_TRAP_DIVIDE_BY_ZERO, HEW_TRAP_INDEX_OUT_OF_BOUNDS,
        HEW_TRAP_INTEGER_OVERFLOW, HEW_TRAP_SHIFT_OUT_OF_RANGE, HEW_TRAP_SIGNED_MIN_DIV_NEG_ONE,
    };

    // Each TrapKind whose lowering routes through the `Terminator::Trap` arm,
    // paired with the runtime constant it must emit. Derived from the runtime
    // side — NOT a literal copy — so a renumber moves the expectation in lock
    // step with the producer.
    let cases = [
        (
            TrapKind::SignedMinDivNegOne,
            HEW_TRAP_SIGNED_MIN_DIV_NEG_ONE,
        ),
        (TrapKind::IndexOutOfBounds, HEW_TRAP_INDEX_OUT_OF_BOUNDS),
        (
            TrapKind::SupervisorChildUnavailable,
            HEW_TRAP_ACTOR_SEND_FAILED,
        ),
    ];
    for (kind, runtime_code) in cases {
        let module = format!("g2_parity_{}", runtime_code);
        let ll = emit_trap_kind_ll(kind, &module);
        assert_trap_with_code(&ll, runtime_code);
    }

    // Source-driven kinds (the producer fires these from real expressions).
    assert_trap_with_code(
        &emit_ll("fn main() -> i64 { 1 + 2 }", "g2_parity_overflow"),
        HEW_TRAP_INTEGER_OVERFLOW,
    );
    assert_trap_with_code(
        &emit_ll(
            "fn main() -> i64 { let a: i64 = 10; let b: i64 = 2; a / b }",
            "g2_parity_divzero",
        ),
        HEW_TRAP_DIVIDE_BY_ZERO,
    );
    assert_trap_with_code(
        &emit_ll(
            "fn main() -> i64 { let a: i64 = 1; let b: i64 = 3; a << b }",
            "g2_parity_shift",
        ),
        HEW_TRAP_SHIFT_OUT_OF_RANGE,
    );
    // `MachineDispatchUnreachable` (207) and `ExhaustivenessFallthrough` (208)
    // are intentionally not asserted here: the standalone `Terminator::Trap`
    // fixture lowers `MachineDispatchUnreachable` to a bare `unreachable` (the
    // synthesised `<Name>__step` dispatch is the real producer), and 208 only
    // fires inside the enum-tag-OOB / match-fallthrough synthesised helpers.
    // Both still single-source their code from the runtime constant at the
    // emit site (the build-level G2 guard covers them).
}

// ---------------------------------------------------------------------------
// Source self-scan guard: no raw integer literal in emit_trap_with_code calls.
//
// The value-driven `emitted_trap_codes_equal_runtime_constants` test cannot
// exercise the synthesised enum-tag-OOB / ser/de-tag-OOB helpers that call
// `emit_trap_with_code_raw` directly (they fire only inside synthesised bodies
// that the MIR fixture pipeline never emits). This structural guard fills the
// gap: it scans `llvm.rs` at compile time and fails if ANY call to
// `emit_trap_with_code` or `emit_trap_with_code_raw` passes a bare integer
// literal as the code argument, rather than a `HEW_TRAP_*` named constant.
//
// Algorithm:
//  1. Load the source via `include_str!` so the check is always in sync.
//  2. Strip line-comment tails (`//` to end of line) so doc-comment mentions
//     of numeric codes do not false-positive. String contents in this file
//     are only label strings ("some_label"), never integer-looking values, so
//     simple `//`-stripping is sufficient.
//  3. For each `emit_trap_with_code` token (either variant) that is NOT a `fn`
//     definition, locate the argument list (up to 8 lines ahead) and assert
//     that no argument is a bare decimal integer (i.e., a token that is only
//     digits, possibly surrounded by whitespace and followed by a comma or `)`,
//     WITHOUT a preceding `HEW_TRAP_` prefix or a following `as u64` cast).
// ---------------------------------------------------------------------------

/// Fail if any `emit_trap_with_code[_raw]` call in llvm.rs passes a bare
/// integer literal as the code argument instead of a `HEW_TRAP_*` constant.
///
/// Works for both single-line and multi-line call forms: for each call site
/// (not a `fn` definition), the argument list is extracted by scanning forward
/// until the parens balance, then each comma-separated argument is inspected.
/// An argument that is solely decimal digits (no leading identifier, no
/// trailing `as`) is a bare integer — a violation.
#[test]
fn no_raw_integer_in_emit_trap_with_code_calls() {
    // Include llvm.rs at compile time so the path is always resolved against
    // the actual source tree, regardless of test working directory.
    let source = include_str!("../src/llvm.rs");

    // Strip single-line comment tails so doc-comment mentions like
    // `hew_trap_with_code(HEW_TRAP_ACTOR_SEND_FAILED=206)` in `///` lines
    // do not produce false positives. String literal contents in llvm.rs are
    // only label strings (e.g. "enum_clone_tag_oob"), never integer-looking,
    // so `//`-stripping without full string-awareness is sufficient here.
    let stripped: String = source
        .lines()
        .map(|line| {
            if let Some(idx) = line.find("//") {
                &line[..idx]
            } else {
                line
            }
        })
        .collect::<Vec<_>>()
        .join("\n");

    let needle = "emit_trap_with_code";
    let mut violations: Vec<String> = Vec::new();
    let mut search_start = 0usize;

    while let Some(rel) = stripped[search_start..].find(needle) {
        let call_start = search_start + rel;

        // Determine the line number for diagnostics.
        let line_num = stripped[..call_start].lines().count() + 1;

        // Skip function definitions: `fn emit_trap_with_code` — look back a
        // few chars for `fn `.
        let look_back = call_start.saturating_sub(4);
        if stripped[look_back..call_start].contains("fn ") {
            search_start = call_start + needle.len();
            continue;
        }

        // Advance past the needle to find the opening `(`.
        let after_needle = call_start + needle.len();
        // The name may be `emit_trap_with_code` or `emit_trap_with_code_raw`;
        // skip any `_raw` suffix and whitespace to reach `(`.
        let paren_pos = match stripped[after_needle..].find('(') {
            Some(p) => after_needle + p,
            None => {
                search_start = after_needle;
                continue;
            }
        };

        // Extract balanced argument list by scanning for matching `)`.
        let args_start = paren_pos + 1;
        let mut depth = 1usize;
        let mut args_end = args_start;
        for (i, ch) in stripped[args_start..].char_indices() {
            match ch {
                '(' => depth += 1,
                ')' => {
                    depth -= 1;
                    if depth == 0 {
                        args_end = args_start + i;
                        break;
                    }
                }
                _ => {}
            }
        }
        let args_text = &stripped[args_start..args_end];

        // Split arguments on top-level commas (no nested paren splitting needed
        // for these calls; the args are simple expressions, not closures).
        let args: Vec<&str> = args_text.split(',').map(str::trim).collect();

        // `emit_trap_with_code(fn_ctx, CODE, label)` → code is arg index 1
        // `emit_trap_with_code_raw(ctx, mod, builder, CODE, label)` → index 3
        // We check EVERY argument for the bare-integer smell rather than
        // hard-coding the index, so the guard works even if call signatures evolve.
        for arg in &args {
            let tok = arg.trim();
            // A bare integer: all digits, length >= 3 (trap codes are 3-digit),
            // no alphabetic prefix (would indicate a named constant), no `as`
            // following (would indicate a cast expression on a named constant).
            let is_bare_integer =
                !tok.is_empty() && tok.chars().all(|c| c.is_ascii_digit()) && tok.len() >= 3;

            if is_bare_integer {
                violations.push(format!(
                    "llvm.rs line {line_num}: call `emit_trap_with_code[_raw]` passes raw integer `{tok}` as code arg — use a HEW_TRAP_* constant instead"
                ));
            }
        }

        search_start = after_needle;
    }

    assert!(
        violations.is_empty(),
        "emit_trap_with_code[_raw] calls with raw integer literal code args found in llvm.rs:\n{}",
        violations.join("\n")
    );
}
