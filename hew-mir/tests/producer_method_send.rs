//! Contract tests for the `hew_duplex_send` producer arm in `lower_duplex_send`.
//!
//! The send producer (`lower_duplex_send`) emits:
//!   `ConstI64 { dest: len, value: 8 }` followed by
//!   `CallRuntimeAbi { symbol: "hew_duplex_send", args: [recv, msg, len], dest: None }`.
//!
//! ## Pipeline gap (current state)
//!
//! The full HIR pipeline (`duplex_pair<int,int>(16)` → `a.send(msg)`) cannot
//! produce `CallRuntimeAbi { symbol: "hew_duplex_send" }` end-to-end because
//! the type-info threading gap prevents the checker from recording a
//! `MethodCallRewrite` for `.send()`:
//!
//!   1. `duplex_pair` is a checker-registered builtin, not an AST `fn` item.
//!   2. HIR's `lower_identifier` falls through to `Unresolved` for `duplex_pair`,
//!      so the call result type is `ResolvedTy::Unit` (not the actual tuple type).
//!   3. The tuple-let elements (`a`, `b`) therefore have type `Unit`.
//!   4. The type checker sees `a: Unit` → no `Duplex<S,R>::send` method found
//!      → no `MethodCallRewrite` entry → E1 bridge emits `MethodCallNoRewrite`
//!      → the method call falls through to `Unsupported` in HIR → MIR sees
//!      no `Call { callee: BindingRef("hew_duplex_send") }` to lower.
//!
//! The gap will close when HIR threads `expr_types` from `TypeCheckOutput`
//! into `lower_identifier` so checker-registered builtins return the
//! checker-inferred call-result type.
//!
//! ## What this file tests instead
//!
//! - `lower_duplex_send` structural shape: tested via a hand-built `HirModule`
//!   that bypasses the type-info gap by constructing HIR nodes directly.  The
//!   hand-built path wires `BindingRef { resolved: Binding(id) }` for the send
//!   receiver so `lower_value` finds a `DuplexHandle` in `binding_locals`
//!   without going through the checker's `MethodCallRewrite` table.  See
//!   `send_producer_emits_const_i64_then_call_runtime_abi` below.
//! - The absence of spurious `Move { src: DuplexHandle }` in source programs
//!   that construct a duplex pair (regression guard against handle-typed binding
//!   regression in `stmt()`).
//! - When the pipeline gap closes: the aspirational tests below (marked
//!   `#[ignore]`) will pass without changes to their assertion bodies.
//!
//! WHEN-OBSOLETE: when the HIR `expr_types` threading is in place, remove
//! `#[ignore]` from the aspirational tests and delete this comment block.

use hew_hir::{
    lower_program, BindingId, HirBinding, HirBlock, HirExpr, HirExprKind, HirFn, HirItem,
    HirLiteral, HirModule, HirNodeId, HirStmt, HirStmtKind, IntentKind, ItemId, ResolutionCtx,
    ResolvedRef, ScopeId, SiteId, ValueClass,
};
use hew_mir::{lower_hir_module, DropKind, Instr, IrPipeline, Place};
use hew_types::module_registry::ModuleRegistry;
use hew_types::{Checker, ResolvedTy};

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
        .flat_map(|(_, plan)| plan.drops.iter().cloned())
        .collect()
}

// ---------------------------------------------------------------------------
// Regression guards — currently testable with the pipeline as-is
// ---------------------------------------------------------------------------

/// No `Instr::Move { src: DuplexHandle(_), .. }` may appear anywhere in
/// the instruction stream for a function that constructs a duplex pair and
/// calls `.send()`.  Handle-typed places are stored directly in
/// `binding_locals` without a Move — the handle-typed branch in `stmt()`
/// enforces this.
///
/// This test is meaningful even though `.send()` does not produce a
/// `CallRuntimeAbi` today: it guards against a regression where `stmt()`
/// loses the `DuplexHandle` kind by emitting a `Move` to a generic `Local`,
/// which would silently break `drop_kind_for` when the pipeline gap closes.
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

// ---------------------------------------------------------------------------
// Hand-built HIR test — exercises lower_duplex_send directly
// ---------------------------------------------------------------------------

/// Build a minimal `HirModule` that calls `duplex_pair` (to obtain a
/// `DuplexHandle` in `binding_locals`) then calls `hew_duplex_send`
/// with that handle as the receiver.
///
/// This bypasses the HIR type-info gap: instead of going through the
/// parser + type-checker, we hand-construct `BindingRef { resolved:
/// Binding(id) }` nodes where `id` is the tuple-element binding we
/// created for `a`.  `lower_hir_module` then finds `DuplexHandle(N)` in
/// `binding_locals[a_id]` and routes to `lower_duplex_send`.
///
/// The function body in HIR:
/// ```text
/// fn main() -> int {
///     let a = duplex_pair(16).0;           // Let(a, TupleIndex(Call(BindingRef("duplex_pair", Unresolved), [16]), 0))
///     hew_duplex_send(a, 42);              // Expr(Call(BindingRef("hew_duplex_send", Unresolved), [BindingRef(a_id), Literal(42)]))
///     return 0;
/// }
/// ```
///
/// The `TupleIndex` wraps the `Call` directly (no intermediate binding)
/// so that `lower_value(TupleIndex)` receives the proxy `Local(M)` returned
/// by `lower_duplex_pair` and looks it up in `tuple_decomp[M]` — obtaining
/// `DuplexHandle(N0)` without a `Move { src: proxy, dest: new_local }` hop
/// that would break the `tuple_decomp` lookup.
///
/// Satisfies the `raii-null-after-move` `LESSONS` trigger: the send
/// producer must not emit `Move { src: DuplexHandle }`, and the receiver
/// place in `CallRuntimeAbi` must be `DuplexHandle`, not a generic
/// `Local`.
#[test]
#[allow(
    clippy::too_many_lines,
    reason = "hand-built HIR module requires explicit construction of every node; \
              splitting would obscure the narrative flow of pair → bind → send"
)]
fn send_producer_emits_const_i64_then_call_runtime_abi() {
    // --- ID allocation -------------------------------------------------
    // Use small fixed IDs; each must be distinct.
    let fn_node = HirNodeId(0);
    let block_node = HirNodeId(1);
    let scope = ScopeId(0);
    let fn_item = ItemId(0);

    // Binding ID — only `a` needs one (no intermediate _tup binding)
    let a_binding_id = BindingId(0);

    // Site IDs (unique per expression site)
    let site_cap = SiteId(0);
    let site_pair_call = SiteId(1);
    let site_tup_idx = SiteId(2);
    let site_a_ref = SiteId(3);
    let site_msg = SiteId(4);
    let site_send_callee = SiteId(5);
    let site_send_call = SiteId(6);
    let site_ret_val = SiteId(7);

    // Node IDs for each sub-expression (unique)
    let node_cap = HirNodeId(10);
    let node_pair_callee = HirNodeId(11);
    let node_pair_call = HirNodeId(12);
    let node_tup_idx = HirNodeId(13);
    let node_a_ref = HirNodeId(14);
    let node_msg = HirNodeId(15);
    let node_send_callee = HirNodeId(16);
    let node_send_call = HirNodeId(17);
    let node_ret_val = HirNodeId(18);
    let node_stmt0 = HirNodeId(20); // let a = duplex_pair(16).0
    let node_stmt1 = HirNodeId(21); // hew_duplex_send(...)
    let node_stmt2 = HirNodeId(22); // return 0

    let dummy_span = 0..0;

    // --- Capacity literal (16) -----------------------------------------
    let cap_expr = HirExpr {
        node: node_cap,
        site: site_cap,
        ty: ResolvedTy::I64,
        value_class: ValueClass::BitCopy,
        intent: IntentKind::Read,
        kind: HirExprKind::Literal(HirLiteral::Integer(16)),
        span: dummy_span.clone(),
    };

    // --- duplex_pair callee BindingRef ----------------------------------
    let pair_callee = HirExpr {
        node: node_pair_callee,
        site: site_pair_call,
        ty: ResolvedTy::Unit,
        value_class: ValueClass::BitCopy,
        intent: IntentKind::Read,
        kind: HirExprKind::BindingRef {
            name: "duplex_pair".to_string(),
            resolved: ResolvedRef::Unresolved,
        },
        span: dummy_span.clone(),
    };

    // --- duplex_pair Call expression ------------------------------------
    let pair_call_expr = HirExpr {
        node: node_pair_call,
        site: site_pair_call,
        ty: ResolvedTy::Unit, // type-info gap: would be Tuple(Duplex, Duplex) when fixed
        value_class: ValueClass::BitCopy,
        intent: IntentKind::Read,
        kind: HirExprKind::Call {
            callee: Box::new(pair_callee),
            args: vec![cap_expr],
        },
        span: dummy_span.clone(),
    };

    // --- TupleIndex .0 wrapping the call directly ----------------------
    // Wrapping the Call directly (no Let for the tuple result) means that
    // `lower_value(TupleIndex)` receives the proxy Local returned by
    // `lower_duplex_pair` directly and looks up `tuple_decomp[proxy_idx]`,
    // yielding `DuplexHandle(N0)` without an intermediate Move hop.
    let tuple_idx_expr = HirExpr {
        node: node_tup_idx,
        site: site_tup_idx,
        ty: ResolvedTy::Named {
            name: "Duplex".to_string(),
            args: vec![ResolvedTy::I64, ResolvedTy::I64],
        },
        value_class: ValueClass::BitCopy,
        intent: IntentKind::Read,
        kind: HirExprKind::TupleIndex {
            tuple: Box::new(pair_call_expr),
            index: 0,
        },
        span: dummy_span.clone(),
    };

    // --- Stmt 0: let a = duplex_pair(16).0 -----------------------------
    let a_binding = HirBinding {
        id: a_binding_id,
        name: "a".to_string(),
        ty: ResolvedTy::Named {
            name: "Duplex".to_string(),
            args: vec![ResolvedTy::I64, ResolvedTy::I64],
        },
        mutable: false,
        span: dummy_span.clone(),
    };
    let stmt_let_a = HirStmt {
        node: node_stmt0,
        kind: HirStmtKind::Let(a_binding, Some(tuple_idx_expr)),
        span: dummy_span.clone(),
    };

    // --- Send receiver BindingRef (a) ----------------------------------
    let a_ref_expr = HirExpr {
        node: node_a_ref,
        site: site_a_ref,
        // Duplex type so ValueClass lookup produces AffineResource (or
        // Unknown with no type_classes entry) — either way, NOT BitCopy,
        // so the send is treated as a non-consuming borrow (IntentKind::Read).
        ty: ResolvedTy::Named {
            name: "Duplex".to_string(),
            args: vec![ResolvedTy::I64, ResolvedTy::I64],
        },
        value_class: ValueClass::BitCopy, // hand-built: doesn't matter for lower_value
        intent: IntentKind::Read,         // non-consuming send
        kind: HirExprKind::BindingRef {
            name: "a".to_string(),
            resolved: ResolvedRef::Binding(a_binding_id),
        },
        span: dummy_span.clone(),
    };

    // --- Message literal (42) ------------------------------------------
    let msg_expr = HirExpr {
        node: node_msg,
        site: site_msg,
        ty: ResolvedTy::I64,
        value_class: ValueClass::BitCopy,
        intent: IntentKind::Read,
        kind: HirExprKind::Literal(HirLiteral::Integer(42)),
        span: dummy_span.clone(),
    };

    // --- hew_duplex_send callee BindingRef -----------------------------
    let send_callee = HirExpr {
        node: node_send_callee,
        site: site_send_callee,
        ty: ResolvedTy::Unit,
        value_class: ValueClass::BitCopy,
        intent: IntentKind::Read,
        kind: HirExprKind::BindingRef {
            name: "hew_duplex_send".to_string(),
            resolved: ResolvedRef::Unresolved,
        },
        span: dummy_span.clone(),
    };

    // --- hew_duplex_send Call expression --------------------------------
    let send_call_expr = HirExpr {
        node: node_send_call,
        site: site_send_call,
        ty: ResolvedTy::Unit,
        value_class: ValueClass::BitCopy,
        intent: IntentKind::Read,
        kind: HirExprKind::Call {
            callee: Box::new(send_callee),
            args: vec![a_ref_expr, msg_expr],
        },
        span: dummy_span.clone(),
    };

    // --- Stmt 1: Expr(hew_duplex_send(a, 42)) --------------------------
    let stmt_send = HirStmt {
        node: node_stmt1,
        kind: HirStmtKind::Expr(send_call_expr),
        span: dummy_span.clone(),
    };

    // --- Return literal (0) --------------------------------------------
    let ret_val = HirExpr {
        node: node_ret_val,
        site: site_ret_val,
        ty: ResolvedTy::I64,
        value_class: ValueClass::BitCopy,
        intent: IntentKind::Read,
        kind: HirExprKind::Literal(HirLiteral::Integer(0)),
        span: dummy_span.clone(),
    };

    // --- Stmt 2: return 0 ----------------------------------------------
    let stmt_return = HirStmt {
        node: node_stmt2,
        kind: HirStmtKind::Return(Some(ret_val)),
        span: dummy_span.clone(),
    };

    // --- Assemble the function body ------------------------------------
    let body = HirBlock {
        node: block_node,
        scope,
        statements: vec![stmt_let_a, stmt_send, stmt_return],
        tail: None,
        ty: ResolvedTy::Unit,
        span: dummy_span.clone(),
    };

    let hir_fn = HirFn {
        id: fn_item,
        node: fn_node,
        name: "main".to_string(),
        type_params: vec![],
        params: vec![],
        return_ty: ResolvedTy::I64,
        body,
        span: dummy_span.clone(),
    };

    let module = HirModule {
        items: vec![HirItem::Function(hir_fn)],
        type_classes: std::collections::HashMap::default(),
    };

    // --- Run MIR lowering ----------------------------------------------
    let pipeline = lower_hir_module(&module);

    let instrs: Vec<Instr> = pipeline
        .raw_mir
        .iter()
        .find(|f| f.name == "main")
        .expect("main must be present")
        .blocks
        .iter()
        .flat_map(|b| b.instructions.iter().cloned())
        .collect();

    // --- Assert: exactly one CallRuntimeAbi for hew_duplex_send --------
    let send_positions: Vec<usize> = instrs
        .iter()
        .enumerate()
        .filter_map(|(i, instr)| {
            matches!(instr, Instr::CallRuntimeAbi(c) if c.symbol() == "hew_duplex_send")
                .then_some(i)
        })
        .collect();

    assert_eq!(
        send_positions.len(),
        1,
        "expected exactly one CallRuntimeAbi for hew_duplex_send; got: {send_positions:?}\n\
         instructions: {instrs:?}"
    );

    let pos = send_positions[0];
    let Instr::CallRuntimeAbi(ref send_call) = instrs[pos] else {
        unreachable!()
    };

    assert_eq!(send_call.symbol(), "hew_duplex_send");
    let args = send_call.args();
    assert_eq!(args.len(), 3, "send takes (recv, msg, len); got: {args:?}");
    assert!(
        matches!(args[0], Place::DuplexHandle(_)),
        "args[0] (receiver) must be DuplexHandle — not a generic Local; got: {:?}",
        args[0]
    );
    assert!(
        matches!(args[1], Place::Local(_)),
        "args[1] (message) must be a Local; got: {:?}",
        args[1]
    );
    assert!(
        matches!(args[2], Place::Local(_)),
        "args[2] (len) must be a Local; got: {:?}",
        args[2]
    );
    assert!(
        send_call.dest().is_none(),
        "send discards result; dest must be None"
    );

    // --- Assert: ConstI64 { value: 8 } immediately before the send ----
    assert!(
        pos > 0,
        "there must be a ConstI64 instruction before hew_duplex_send"
    );
    assert!(
        matches!(instrs[pos - 1], Instr::ConstI64 { value: 8, .. }),
        "instruction immediately before hew_duplex_send must be ConstI64{{value: 8}}; \
         got: {:?}",
        instrs[pos - 1]
    );

    // --- Assert: no Move with DuplexHandle as source (raii-null-after-move) ---
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
        "DuplexHandle must never appear as Move src (raii-null-after-move); found: {bad_moves:?}"
    );
}

// ---------------------------------------------------------------------------
// Aspirational tests — blocked on HIR type-info threading
// ---------------------------------------------------------------------------
//
// These tests encode the correct assertions for when the pipeline gap closes.
// They are `#[ignore]`d to keep the test suite green while documenting the
// intended contract.  Remove `#[ignore]` when the HIR `expr_types` threading
// is in place and `duplex_pair`'s call result type propagates correctly.

/// `a.send(42)` must produce exactly one `CallRuntimeAbi { symbol:
/// "hew_duplex_send", args: [DuplexHandle, Local(msg), Local(len)], dest:
/// None }` in the instruction stream, preceded immediately by
/// `ConstI64 { value: 8 }`.
///
/// BLOCKED: requires HIR `expr_types` threading so `.send()` gets a
/// `MethodCallRewrite` entry with `a: Duplex<int,int>`.
#[test]
#[ignore = "blocked on HIR expr_types threading for duplex_pair call-result type"]
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
        .position(|i| matches!(i, Instr::CallRuntimeAbi(c) if c.symbol() == "hew_duplex_send"))
        .expect("must find CallRuntimeAbi for hew_duplex_send");

    let Instr::CallRuntimeAbi(ref send_call) = instrs[send_pos] else {
        unreachable!()
    };

    assert_eq!(send_call.symbol(), "hew_duplex_send");
    let args = send_call.args();
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
        send_call.dest().is_none(),
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
///
/// BLOCKED: same HIR type-info threading gap.
#[test]
#[ignore = "blocked on HIR expr_types threading for duplex_pair call-result type"]
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
            matches!(instr, Instr::CallRuntimeAbi(c) if c.symbol() == "hew_duplex_send")
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
///
/// BLOCKED: same HIR type-info threading gap.  The current pipeline emits
/// no send calls at all, so this test is aspirational in the "two sends"
/// framing; the non-`#[ignore]`d version above covers the
/// currently-reachable subset of this invariant.
#[test]
#[ignore = "blocked on HIR expr_types threading for duplex_pair call-result type"]
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
///
/// BLOCKED: same HIR type-info threading gap.
#[test]
#[ignore = "blocked on HIR expr_types threading for duplex_pair call-result type"]
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
///
/// BLOCKED: same HIR type-info threading gap.
#[test]
#[ignore = "blocked on HIR expr_types threading for duplex_pair call-result type"]
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
            if let Instr::CallRuntimeAbi(c) = i {
                if c.symbol() == "hew_duplex_send" {
                    if let Some(Place::DuplexHandle(n)) = c.args().first() {
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
