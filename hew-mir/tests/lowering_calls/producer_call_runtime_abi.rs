//! Contract tests for the HIR→MIR producer for `Instr::CallRuntimeAbi`.
//!
//! Slice E2-B wires the producer side: `HirExprKind::Call` with a
//! `BindingRef { name: "hew_duplex_pair" | "hew_duplex_send", resolved:
//! Unresolved }` callee routes to `lower_runtime_call`, which emits
//! `Instr::CallRuntimeAbi` and registers `DuplexHandle` Places in
//! `tuple_decomp` / `binding_locals`.
//!
//! These tests run the full pipeline:
//!   parse → typecheck → HIR lower → MIR lower
//! to exercise the real production path rather than hand-built fixtures.
//! `duplex_pair` is a checker-registered builtin; `register_builtins()`
//! fires inside `Checker::check_program`.
//!
//! WHEN-OBSOLETE: when the bridge pipeline gains end-to-end codegen
//! coverage that checks MIR-level instruction shapes automatically;
//! at that point these structural assertion tests become redundant
//! with the E2E test suite.

use hew_hir::{lower_program, ResolutionCtx};
use hew_mir::{lower_hir_module, Instr, IrPipeline, Place};
use hew_types::module_registry::ModuleRegistry;
use hew_types::Checker;

/// Run the full pipeline with type-checking so that `duplex_pair` (a
/// checker-registered builtin) resolves correctly in HIR.
fn pipeline_with_tc(source: &str) -> IrPipeline {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    // HIR may emit diagnostics for constructs outside the E1 bridge subset
    // (e.g. NotYetImplemented for features not yet wired). Filter-assert
    // that only the expected kinds appear rather than asserting empty.
    let output = lower_program(
        &parsed.program,
        &tc_output,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    lower_hir_module(&output.module)
}

/// Extract the `main` function's raw MIR, panicking with a clear message
/// if it is missing.
fn main_raw(p: &IrPipeline) -> &hew_mir::RawMirFunction {
    p.raw_mir
        .iter()
        .find(|f| f.name == "main")
        .expect("main must be present in raw_mir")
}

// ---------------------------------------------------------------------------
// duplex_pair lowering — CallRuntimeAbi structural shape
// ---------------------------------------------------------------------------

/// `let (a, b) = duplex_pair<i64, i64>(16);` must produce exactly one
/// `Instr::CallRuntimeAbi { symbol: "hew_duplex_pair", .. }` in the
/// instruction stream with four args: `cap`, `r_cap` (same `Place` in the
/// one-arg form), `dh0`, `dh1`.
///
/// The one-arg E1 form (capacity: i64) duplicates the single capacity
/// local for both `s_cap` and `r_cap` slots, so `args[0] == args[1]`.
/// When E1 expands to a two-arg form, this equality assertion is the
/// pivot to update.
#[test]
fn duplex_pair_emits_call_runtime_abi_with_four_args() {
    let source = r"
        fn main() -> i64 {
            let (a, b) = duplex_pair<i64, i64>(16);
            return 0;
        }
    ";
    let pipeline = pipeline_with_tc(source);
    let raw = main_raw(&pipeline);

    let instr = raw
        .blocks
        .iter()
        .flat_map(|b| b.instructions.iter())
        .find(|i| matches!(i, Instr::CallRuntimeAbi(c) if c.symbol() == "hew_duplex_pair"))
        .expect("must find exactly one CallRuntimeAbi for hew_duplex_pair");

    let Instr::CallRuntimeAbi(call) = instr else {
        unreachable!("already matched above");
    };

    assert_eq!(call.symbol(), "hew_duplex_pair");
    let args = call.args();
    assert_eq!(
        args.len(),
        4,
        "hew_duplex_pair takes (s_cap, r_cap, *mut dh0, *mut dh1); expected 4 args, got: {args:?}"
    );

    // args[0] and args[1] are the capacity locals (Local(_) — not handles).
    assert!(
        matches!(args[0], Place::Local(_)),
        "args[0] (s_cap) must be a Local; got: {:?}",
        args[0]
    );
    assert!(
        matches!(args[1], Place::Local(_)),
        "args[1] (r_cap) must be a Local; got: {:?}",
        args[1]
    );
    // One-arg E1 form: symmetric capacity, so both slots get the same local.
    assert_eq!(
        args[0], args[1],
        "one-arg E1 form duplicates the single capacity local for both directions; \
         args[0]={:?} args[1]={:?}",
        args[0], args[1]
    );

    // args[2] and args[3] are the DuplexHandle out-param Places.
    assert!(
        matches!(args[2], Place::DuplexHandle(_)),
        "args[2] must be a DuplexHandle; got: {:?}",
        args[2]
    );
    assert!(
        matches!(args[3], Place::DuplexHandle(_)),
        "args[3] must be a DuplexHandle; got: {:?}",
        args[3]
    );

    // The two handles must be distinct (different local indices).
    let Place::DuplexHandle(n2) = args[2] else {
        unreachable!()
    };
    let Place::DuplexHandle(n3) = args[3] else {
        unreachable!()
    };
    assert_ne!(
        n2, n3,
        "the two DuplexHandle out-params must have distinct local indices"
    );

    // i32 result (error code) is discarded — dest must be None.
    assert!(
        call.dest().is_none(),
        "hew_duplex_pair discards its i32 result; dest must be None"
    );
}

/// The raw MIR `locals` vector at each `DuplexHandle` index must carry
/// `ResolvedTy::Named { name: "Duplex", .. }` — confirming that
/// `lower_duplex_pair` allocated type-tagged locals for the two handles.
///
/// SHIM NOTE: Drop plan population (`owned_locals` → `ElaboratedMirFunction
/// ::drop_plans`) depends on `binding.ty` being non-`BitCopy` at the
/// `let a = __tuple_N.0` stmt.  Because `duplex_pair` is a
/// checker-registered builtin whose call result type is not yet threaded
/// into HIR (`lower_identifier` returns `ResolvedTy::Unit` for unresolved
/// callee, so the tuple element types are also `Unit`), `ValueClass::of_ty`
/// sees `Unit` → `BitCopy` and the `owned_locals.push` does not fire.
/// The drop plan therefore contains zero `DuplexClose` entries in this
/// pipeline configuration.
///
/// The `locals` assertion here is the available regression guard: it pins
/// that `lower_duplex_pair` correctly allocates `Duplex`-typed locals even
/// though the drop elaboration gap exists.  When HIR threads `expr_types`
/// from `TypeCheckOutput` (the post-E2 bridge work), the `binding.ty` for
/// `a` and `b` will become `Duplex<i64, i64>`, `ValueClass::of_ty` will
/// return `AffineResource`, and the `owned_locals.push` will fire — at
/// that point this test should be extended to also assert the drop plan.
#[test]
fn duplex_pair_locals_carry_duplex_type_at_handle_indices() {
    let source = r"
        fn main() -> i64 {
            let (a, b) = duplex_pair<i64, i64>(16);
            return 0;
        }
    ";
    let pipeline = pipeline_with_tc(source);
    let raw = main_raw(&pipeline);

    // Locate the two DuplexHandle args from the CallRuntimeAbi instruction.
    let pair_instr = raw
        .blocks
        .iter()
        .flat_map(|b| b.instructions.iter())
        .find(|i| matches!(i, hew_mir::Instr::CallRuntimeAbi(c) if c.symbol() == "hew_duplex_pair"))
        .expect("hew_duplex_pair CallRuntimeAbi must be present");

    let hew_mir::Instr::CallRuntimeAbi(ref pair_call) = pair_instr else {
        unreachable!()
    };
    let args = pair_call.args();
    let Place::DuplexHandle(n0) = args[2] else {
        panic!("args[2] must be DuplexHandle; got {:?}", args[2])
    };
    let Place::DuplexHandle(n1) = args[3] else {
        panic!("args[3] must be DuplexHandle; got {:?}", args[3])
    };

    // Both local slots must be typed as Named { "Duplex" }.
    let ty0 = raw
        .locals
        .get(n0 as usize)
        .expect("handle local 0 must have a type entry");
    let ty1 = raw
        .locals
        .get(n1 as usize)
        .expect("handle local 1 must have a type entry");

    assert!(
        matches!(ty0, hew_types::ResolvedTy::Named { name, .. } if name == "Duplex"),
        "local[{n0}] must be Named{{Duplex}}; got {ty0:?}"
    );
    assert!(
        matches!(ty1, hew_types::ResolvedTy::Named { name, .. } if name == "Duplex"),
        "local[{n1}] must be Named{{Duplex}}; got {ty1:?}"
    );
}

/// No `Instr::Move { src: DuplexHandle(_), .. }` may appear anywhere in
/// the instruction stream for a function that only constructs a duplex
/// pair.  Handle-typed places are stored directly in `binding_locals`
/// without a Move to preserve `drop_kind_for` semantics.
#[test]
fn duplex_pair_no_move_of_duplex_handle() {
    let source = r"
        fn main() -> i64 {
            let (a, b) = duplex_pair<i64, i64>(16);
            return 0;
        }
    ";
    let pipeline = pipeline_with_tc(source);
    let raw = main_raw(&pipeline);

    let bad_moves: Vec<_> = raw
        .blocks
        .iter()
        .flat_map(|b| b.instructions.iter())
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

// ---------------------------------------------------------------------------
// Allowlist gate: unknown symbol is rejected at the MIR layer
// ---------------------------------------------------------------------------

/// `hew_duplex_pair` is in the allowlist, so it must not produce a
/// `NotYetImplemented` MIR diagnostic about an unrecognised runtime symbol.
/// This is distinct from the construction-side allowlist test in
/// `runtime_abi_instr.rs` — here we exercise the allowlist via the
/// full producer path.
#[test]
fn duplex_pair_symbol_is_on_allowlist_no_unsupported_diagnostic() {
    let source = r"
        fn main() -> i64 {
            let (a, b) = duplex_pair<i64, i64>(16);
            return 0;
        }
    ";
    let pipeline = pipeline_with_tc(source);

    // No diagnostic about an unrecognised runtime symbol.
    let bad: Vec<_> = pipeline
        .diagnostics
        .iter()
        .filter(|d| {
            if let hew_mir::MirDiagnosticKind::NotYetImplemented { construct, .. } = &d.kind {
                construct.contains("unrecognised") || construct.contains("runtime symbol")
            } else {
                false
            }
        })
        .collect();

    assert!(
        bad.is_empty(),
        "no NotYetImplemented for unknown runtime symbol expected; got: {bad:?}"
    );
}

// ---------------------------------------------------------------------------
// actor link/monitor lowering — discarded calls only
// ---------------------------------------------------------------------------

fn link_monitor_source(body: &str) -> String {
    format!(
        r"
        actor Probe {{
            receive fn crash() {{
                exit(1)
            }}
        }}

        fn main() -> i64 {{
            {body}
        }}
        "
    )
}

fn calls_for<'a>(raw: &'a hew_mir::RawMirFunction, symbol: &str) -> Vec<&'a hew_mir::RuntimeCall> {
    raw.blocks
        .iter()
        .flat_map(|b| b.instructions.iter())
        .filter_map(|instr| match instr {
            Instr::CallRuntimeAbi(call) if call.symbol() == symbol => Some(call),
            _ => None,
        })
        .collect()
}

#[test]
fn discarded_link_and_monitor_emit_call_runtime_abi_without_dest() {
    // 1-arg user surface: `link(target)` / `monitor(target)`. The producer
    // synthesizes the implicit `self` subject via `hew_actor_self()` as ABI
    // arg0 and threads the user target as arg1.
    let source = link_monitor_source(
        r"
        let p = spawn Probe;
        let q = spawn Probe;
        link(p);
        monitor(q);
        return 0;
        ",
    );
    let pipeline = pipeline_with_tc(&source);
    let raw = main_raw(&pipeline);

    // Each link/monitor must be preceded by exactly one self-handle synthesis.
    let self_calls = calls_for(raw, "hew_actor_self");
    assert_eq!(
        self_calls.len(),
        2,
        "expected one hew_actor_self synthesis per link/monitor, got {self_calls:?}"
    );
    for call in &self_calls {
        assert!(
            call.args().is_empty(),
            "hew_actor_self takes no args; got {:?}",
            call.args()
        );
        assert!(
            matches!(call.dest(), Some(Place::Local(_))),
            "hew_actor_self must write its borrowed self handle into a Local; got {:?}",
            call.dest()
        );
    }

    for symbol in ["hew_actor_link", "hew_actor_monitor"] {
        let calls = calls_for(raw, symbol);
        assert_eq!(
            calls.len(),
            1,
            "expected exactly one CallRuntimeAbi for {symbol}, got {calls:?}"
        );
        let call = calls[0];
        assert_eq!(
            call.args().len(),
            2,
            "{symbol} must carry two actor handles (synthesized self, user target)"
        );
        // arg0: the synthesized `hew_actor_self()` result — a Local, NOT an
        // ActorHandle place. Its source must be a preceding hew_actor_self
        // call writing into that same Local.
        let Place::Local(self_idx) = call.args()[0] else {
            panic!(
                "{symbol} arg0 must be the synthesized self Local; got {:?}",
                call.args()[0]
            );
        };
        assert!(
            self_calls
                .iter()
                .any(|c| c.dest() == Some(Place::Local(self_idx))),
            "{symbol} arg0 Local({self_idx}) must be produced by a hew_actor_self call"
        );
        // arg1: the user-provided target, an ActorHandle from `spawn Probe`.
        assert!(
            matches!(call.args()[1], Place::ActorHandle(_)),
            "{symbol} arg1 must be the user target ActorHandle; got {:?}",
            call.args()[1]
        );
        assert!(
            call.dest().is_none(),
            "{symbol} discarded statement-position call must use dest=None"
        );
    }
}

#[test]
fn value_needed_link_emits_call_runtime_abi_with_result_dest() {
    // link() in value position: the MIR producer must emit hew_actor_link
    // with dest=Some(Place::Local(N)) of type Result<(),LinkError>, with no
    // NYI diagnostic.
    let source = link_monitor_source(
        r"
        let p = spawn Probe;
        let r = link(p);
        let _ = r;
        return 0;
        ",
    );
    let pipeline = pipeline_with_tc(&source);
    let raw = main_raw(&pipeline);

    // No NYI diagnostics: the feature is now wired.
    assert!(
        pipeline.diagnostics.is_empty(),
        "value-needed link() must produce no diagnostics; got: {:?}",
        pipeline.diagnostics
    );

    let calls = calls_for(raw, "hew_actor_link");
    assert_eq!(
        calls.len(),
        1,
        "expected exactly one hew_actor_link CallRuntimeAbi; got {calls:?}"
    );
    let call = calls[0];
    assert_eq!(
        call.args().len(),
        2,
        "hew_actor_link must carry two handles"
    );
    // Value-needed: dest must be Some(Place::Local(_)) — the Result<(),LinkError>
    // local the codegen handler writes Ok(()) into.
    assert!(
        matches!(call.dest(), Some(Place::Local(_))),
        "value-needed hew_actor_link must use dest=Some(Place::Local(_)); got {:?}",
        call.dest()
    );
}

#[test]
fn value_needed_monitor_emits_call_runtime_abi_with_i64_dest_and_record_init() {
    // monitor() in value position: the MIR producer must emit hew_actor_monitor
    // with dest=Some(Place::Local(N: i64)) for the raw ref_id, then a RecordInit
    // that assembles MonitorRef{ref_id} from that local. No NYI diagnostic.
    //
    // The MonitorRef is consumed with `let _ = m;` (move-to-wildcard), which
    // routes through the real auto-drop path (RuntimeDropDescriptor::MonitorRefClose
    // → lower_drop_runtime). It is deliberately NOT consumed via `m.close()`:
    // `.close()` does not resolve as a method on `MonitorRef` from real source,
    // so a `.close()` here would exercise a path no user program can reach.
    let source = link_monitor_source(
        r"
        let p = spawn Probe;
        let m = monitor(p);
        let _ = m;
        return 0;
        ",
    );
    let pipeline = pipeline_with_tc(&source);
    let raw = main_raw(&pipeline);

    // No NYI diagnostics.
    assert!(
        pipeline.diagnostics.is_empty(),
        "value-needed monitor() must produce no diagnostics; got: {:?}",
        pipeline.diagnostics
    );

    let calls = calls_for(raw, "hew_actor_monitor");
    assert_eq!(
        calls.len(),
        1,
        "expected exactly one hew_actor_monitor CallRuntimeAbi; got {calls:?}"
    );
    let call = calls[0];
    assert_eq!(
        call.args().len(),
        2,
        "hew_actor_monitor must carry two handles"
    );
    // Value-needed: dest must be Some(Place::Local(_)) for the raw i64 ref_id.
    assert!(
        matches!(call.dest(), Some(Place::Local(_))),
        "value-needed hew_actor_monitor must use dest=Some(Place::Local(_)); got {:?}",
        call.dest()
    );

    // A RecordInit must follow to assemble MonitorRef{ref_id} from the raw i64.
    let has_record_init = raw
        .blocks
        .iter()
        .flat_map(|b| b.instructions.iter())
        .any(|i| matches!(i, Instr::RecordInit { .. }));
    assert!(
        has_record_init,
        "value-needed monitor() must produce a RecordInit to assemble MonitorRef{{ref_id}}"
    );
}

// ---------------------------------------------------------------------------
// actor unlink lowering — mirroring link/monitor
// ---------------------------------------------------------------------------

#[test]
fn discarded_unlink_emits_call_runtime_abi_without_dest() {
    // 1-arg user surface: `unlink(target)`. The producer synthesizes the
    // implicit `self` subject via `hew_actor_self()` as ABI arg0 and
    // threads the user target as arg1, matching `hew_actor_unlink(a, b)`.
    let source = link_monitor_source(
        r"
        let p = spawn Probe;
        unlink(p);
        return 0;
        ",
    );
    let pipeline = pipeline_with_tc(&source);
    assert!(
        pipeline.diagnostics.is_empty(),
        "unlink() in statement position must lower without diagnostics; got: {:?}",
        pipeline.diagnostics
    );

    let raw = main_raw(&pipeline);

    let self_calls = calls_for(raw, "hew_actor_self");
    // One `hew_actor_self` synthesis for the single `unlink(p)` call.
    assert_eq!(
        self_calls.len(),
        1,
        "expected exactly one hew_actor_self synthesis for unlink(p); got {self_calls:?}"
    );

    let unlink_calls = calls_for(raw, "hew_actor_unlink");
    assert_eq!(
        unlink_calls.len(),
        1,
        "expected exactly one CallRuntimeAbi for hew_actor_unlink; got {unlink_calls:?}"
    );
    let call = unlink_calls[0];
    assert_eq!(
        call.args().len(),
        2,
        "hew_actor_unlink must carry two actor handles (synthesized self, user target)"
    );
    // arg0: the synthesized `hew_actor_self()` result.
    let Place::Local(self_idx) = call.args()[0] else {
        panic!(
            "hew_actor_unlink arg0 must be the synthesized self Local; got {:?}",
            call.args()[0]
        );
    };
    assert!(
        self_calls
            .iter()
            .any(|c| c.dest() == Some(Place::Local(self_idx))),
        "hew_actor_unlink arg0 Local({self_idx}) must be produced by a hew_actor_self call"
    );
    // arg1: the user-provided target ActorHandle from `spawn Probe`.
    assert!(
        matches!(call.args()[1], Place::ActorHandle(_)),
        "hew_actor_unlink arg1 must be the user target ActorHandle; got {:?}",
        call.args()[1]
    );
    assert!(
        call.dest().is_none(),
        "discarded unlink() call must use dest=None; got {:?}",
        call.dest()
    );
}
