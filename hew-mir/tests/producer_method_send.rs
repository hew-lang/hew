//! Contract tests for the `hew_duplex_send` producer arm in `lower_duplex_send`.
//!
//! The send producer (`lower_duplex_send`) emits:
//!   `ConstI64 { dest: len, value: 8 }` followed by
//!   `CallRuntimeAbi { symbol: "hew_duplex_send", args: [recv, msg, len], dest: None }`.
//!
//! These tests exercise the full pipeline: `duplex_pair<int,int>(16)` binds
//! two `DuplexHandle` locals, and `a.send(msg)` produces the correct
//! `CallRuntimeAbi` instruction sequence.  Handle-typed bindings must never
//! appear as `Move` sources — the `stmt()` handler stores them directly into
//! `binding_locals` to preserve the `DuplexHandle` kind for `drop_kind_for`.
//!
//! ## Retired hand-built HIR test
//!
//! A prior version of this file contained a hand-built `HirModule` test
//! (`send_producer_emits_const_i64_then_call_runtime_abi`) that exercised
//! `lower_duplex_send` directly by bypassing the parser and type-checker.
//! That workaround was necessary because the type-info threading gap
//! (HIR `lower_identifier` falling through to `Unresolved` for checker-
//! registered builtins like `duplex_pair`) prevented the full pipeline from
//! recording a `MethodCallRewrite` for `.send()`, so no `CallRuntimeAbi` was
//! ever produced end-to-end.
//!
//! The gap is now closed: HIR threads `expr_types` from `TypeCheckOutput` into
//! `lower_identifier`, so `duplex_pair`'s call-result type propagates through
//! the checker → HIR → MIR pipeline correctly.  The real-pipeline tests
//! (`one_send_emits_call_runtime_abi_with_three_args`,
//! `two_sends_emit_two_call_runtime_abi_instructions`, and siblings) now cover
//! the same `lower_duplex_send` structural shape — receiver as `DuplexHandle`,
//! three args, no `dest`, preceded by `ConstI64 { value: 8 }` — that the
//! hand-built test was approximating.  The hand-built scaffolding is retired.

use hew_hir::{lower_program, ResolutionCtx};
use hew_mir::{lower_hir_module, DropKind, ExitPath, Instr, IrPipeline, Place};
use hew_types::module_registry::ModuleRegistry;
use hew_types::Checker;

/// Run the full pipeline with type-checking.
fn pipeline_with_tc(source: &str) -> IrPipeline {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    let output = lower_program(&parsed.program, &tc_output, &ResolutionCtx);
    lower_hir_module(&output.module)
}

/// Flatten all instructions from all blocks of the named function's raw MIR.
fn all_instrs(p: &IrPipeline, fn_name: &str) -> Vec<Instr> {
    p.raw_mir
        .iter()
        .find(|f| f.name == fn_name)
        .expect("function must be present in raw_mir")
        .blocks
        .iter()
        .flat_map(|b| b.instructions.iter().cloned())
        .collect()
}

/// Flatten all drops from the named function's elaborated MIR exit plans.
fn all_drops(p: &IrPipeline, fn_name: &str) -> Vec<hew_mir::ElabDrop> {
    p.elaborated_mir
        .iter()
        .find(|f| f.name == fn_name)
        .expect("function must be present in elaborated_mir")
        .drop_plans
        .iter()
        .filter(|(exit, _)| matches!(exit, ExitPath::Return { .. }))
        .flat_map(|(_, plan)| plan.drops.iter().cloned())
        .collect()
}

/// No `Instr::Move { src: DuplexHandle(_), .. }` may appear anywhere in
/// the instruction stream for a function that constructs a duplex pair and
/// calls `.send()`.  Handle-typed places are stored directly in
/// `binding_locals` without a Move — the handle-typed branch in `stmt()`
/// enforces this invariant for `drop_kind_for` correctness.
#[test]
fn duplex_pair_plus_send_no_move_of_duplex_handle() {
    let source = r"
        fn main() -> int {
            let (a, b) = duplex_pair<int, int>(16);
            a.send(42);
            return 0;
        }
    ";
    let pipeline = pipeline_with_tc(source);
    let instrs = all_instrs(&pipeline, "main");

    let bad_moves: Vec<_> = instrs
        .iter()
        .filter(|i| {
            matches!(
                i,
                Instr::Move {
                    src: Place::DuplexHandle(_),
                    ..
                }
            )
        })
        .collect();

    assert!(
        bad_moves.is_empty(),
        "DuplexHandle must never appear as Move src; found: {bad_moves:?}"
    );
}

/// `a.send(42)` must produce exactly one `CallRuntimeAbi { symbol:
/// "hew_duplex_send", args: [DuplexHandle, Local(msg), Local(len)], dest:
/// None }` in the instruction stream, preceded immediately by
/// `ConstI64 { value: 8 }`.
#[test]
fn one_send_emits_call_runtime_abi_with_three_args() {
    let source = r"
        fn main() -> int {
            let (a, b) = duplex_pair<int, int>(16);
            a.send(42);
            return 0;
        }
    ";
    let pipeline = pipeline_with_tc(source);
    let instrs = all_instrs(&pipeline, "main");

    let send_pos = instrs
        .iter()
        .position(
            |i| matches!(i, Instr::CallRuntimeAbi(call) if call.symbol() == "hew_duplex_send"),
        )
        .expect("must find CallRuntimeAbi for hew_duplex_send");

    let Instr::CallRuntimeAbi(call) = &instrs[send_pos] else {
        unreachable!()
    };

    assert_eq!(call.symbol(), "hew_duplex_send");
    let args = call.args();
    assert_eq!(args.len(), 3, "3 args: recv, msg, len; got: {args:?}");
    assert!(
        matches!(args[0], Place::DuplexHandle(_)),
        "args[0] (receiver) must be DuplexHandle; got: {:?}",
        args[0]
    );
    assert!(
        matches!(args[1], Place::Local(_)),
        "args[1] (msg) must be Local"
    );
    assert!(
        matches!(args[2], Place::Local(_)),
        "args[2] (len) must be Local"
    );
    assert!(
        call.dest().is_none(),
        "send discards result; dest must be None"
    );

    assert!(send_pos > 0, "ConstI64 len must precede the send call");
    assert!(
        matches!(instrs[send_pos - 1], Instr::ConstI64 { value: 8, .. }),
        "instruction before hew_duplex_send must be ConstI64 {{value:8}}; got: {:?}",
        instrs[send_pos - 1]
    );
}

/// Two consecutive `a.send(N)` calls produce two `CallRuntimeAbi` instructions,
/// each preceded by `ConstI64 { value: 8 }`.
#[test]
fn two_sends_emit_two_call_runtime_abi_instructions() {
    let source = r"
        fn main() -> int {
            let (a, b) = duplex_pair<int, int>(16);
            a.send(42);
            a.send(43);
            return 0;
        }
    ";
    let pipeline = pipeline_with_tc(source);
    let instrs = all_instrs(&pipeline, "main");

    let send_positions: Vec<usize> = instrs
        .iter()
        .enumerate()
        .filter_map(|(i, instr)| {
            matches!(instr, Instr::CallRuntimeAbi(call) if call.symbol() == "hew_duplex_send")
                .then_some(i)
        })
        .collect();

    assert_eq!(send_positions.len(), 2, "two sends → two CallRuntimeAbi");

    for &pos in &send_positions {
        assert!(pos > 0);
        assert!(
            matches!(instrs[pos - 1], Instr::ConstI64 { value: 8, .. }),
            "instruction before send at {pos} must be ConstI64{{8}}; got {:?}",
            instrs[pos - 1]
        );
    }
}

/// After two sends, no `Move { src: DuplexHandle }` appears (non-consuming
/// borrow semantics).
#[test]
fn two_sends_do_not_move_duplex_handle_full_pipeline() {
    let source = r"
        fn main() -> int {
            let (a, b) = duplex_pair<int, int>(16);
            a.send(42);
            a.send(43);
            return 0;
        }
    ";
    let pipeline = pipeline_with_tc(source);
    let instrs = all_instrs(&pipeline, "main");

    let bad: Vec<_> = instrs
        .iter()
        .filter(|i| {
            matches!(
                i,
                Instr::Move {
                    src: Place::DuplexHandle(_),
                    ..
                }
            )
        })
        .collect();
    assert!(bad.is_empty(), "no Move of DuplexHandle; found: {bad:?}");
}

/// After two sends the Duplex handles still appear in the drop plan.
#[test]
fn sender_handle_remains_in_drop_plan_after_two_sends() {
    let source = r"
        fn main() -> int {
            let (a, b) = duplex_pair<int, int>(16);
            a.send(42);
            a.send(43);
            return 0;
        }
    ";
    let pipeline = pipeline_with_tc(source);
    let drops = all_drops(&pipeline, "main");

    let duplex_close: Vec<_> = drops
        .iter()
        .filter(|d| d.kind == DropKind::DuplexClose)
        .collect();

    assert_eq!(duplex_close.len(), 2, "both handles must be in drop plan");
    for d in &duplex_close {
        assert!(matches!(d.place, Place::DuplexHandle(_)));
    }
}

/// Sends on `a` and `b` reference distinct `DuplexHandle` locals.
#[test]
fn two_sends_on_different_handles_use_distinct_receiver_places() {
    let source = r"
        fn main() -> int {
            let (a, b) = duplex_pair<int, int>(16);
            a.send(1);
            b.send(2);
            return 0;
        }
    ";
    let pipeline = pipeline_with_tc(source);
    let instrs = all_instrs(&pipeline, "main");

    let receivers: Vec<u32> = instrs
        .iter()
        .filter_map(|i| {
            if let Instr::CallRuntimeAbi(call) = i {
                if call.symbol() == "hew_duplex_send" {
                    if let Some(Place::DuplexHandle(n)) = call.args().first() {
                        return Some(*n);
                    }
                }
            }
            None
        })
        .collect();

    assert_eq!(receivers.len(), 2);
    assert_ne!(
        receivers[0], receivers[1],
        "sends on a and b must use distinct handles"
    );
}
