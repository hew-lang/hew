//! W5.011 P3 — owned-`string` temporary/drop substrate canaries.
//!
//! A fresh-owned `string` result used in a non-consuming (borrowing) context
//! must be released EXACTLY ONCE — never zero (leak), never twice (refcount
//! over-decrement / premature free). `string` is refcounted (`hew_string_drop`
//! decrements and frees at zero); every `is_fresh_owned_string_producer`
//! (`hew_string_concat`, `_to_uppercase`, the `Vec<string>` getter
//! `hew_vec_get_str`, …) hands the caller exactly one drop obligation, and a
//! borrowing use (`hew_string_length`, …) reads the buffer without consuming the
//! refcount (verified in `hew-runtime/src/string.rs`).
//!
//! Two release paths cooperate, on disjoint shapes:
//!   * BOUND (`let y = <producer>; y.len()`) — `derive_cow_fresh_borrowed_owner`
//!     admits `y` and the elaborator emits a scope-exit `CowHeap` drop per exit.
//!   * NESTED / DISCARD (`(<producer>).len()`, `<producer>;`) — the bare temp
//!     has no binding, so `apply_nested_fresh_string_temp_drops` splices an
//!     inline `Instr::Drop` after the borrowing use / producer.
//!
//! Fail-closed throughout: a value that escapes (return, store, user-fn call,
//! container-move insert) earns NO drop — it leaks, never double-frees.
//!
//! INDEX FORM (`vec-generic-index` lane — LANDED): `xs[i]` over `Vec<string>`
//! now lowers to the same `hew_vec_get_str` retained owner as `.get(i)`, so this
//! producer-agnostic substrate handles every shape with zero further work —
//! `let y = xs[i]; y.len()` via the BOUND path, `xs[i].len()` via the NESTED
//! path, `xs[i];` via the DISCARD path. The `index_*` canaries below pin each on
//! the INDEX spelling specifically. The one shape unique to `xs[i]` (vs the
//! unconditional `.get(i)` call) is the OOB bounds-check trap that PRECEDES the
//! getter: the bound owner `y` is `Uninit` on that trap edge, so its scope-exit
//! drop must NOT fire there (`index_bound_oob_trap_drops_nothing`) — otherwise a
//! clean OOB trap (SIGTRAP, code 205) degrades into a use-of-uninitialised-
//! pointer SIGSEGV. `enumerate_exits`' Trap arm filters the scope-exit LIFO by
//! the same init-aware predicate the Return arm uses, which closes that gap.
//!
//! LESSONS: boundary-fail-closed (P0), cleanup-all-exits, raii-null-after-move.

use hew_hir::{lower_program, ResolutionCtx};
use hew_mir::{lower_hir_module, DropKind, ExitPath, Instr, IrPipeline};
use hew_types::module_registry::ModuleRegistry;
use hew_types::{Checker, ResolvedTy};

/// Run the full pipeline with type-checking so checker-registered builtins and
/// string producers resolve to their inferred call-result types.
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

/// Inline `hew_string_drop` `Instr::Drop`s in one function's raw MIR — the
/// NESTED/DISCARD (sub-problem B) release path.
fn inline_string_drops(pl: &IrPipeline, fn_name: &str) -> usize {
    pl.raw_mir
        .iter()
        .filter(|f| f.name == fn_name)
        .flat_map(|f| f.blocks.iter())
        .flat_map(|b| b.instructions.iter())
        .filter(|i| {
            matches!(
                i,
                Instr::Drop {
                    ty: ResolvedTy::String,
                    drop_fn: Some(s),
                    ..
                } if s == "hew_string_drop"
            )
        })
        .count()
}

/// Per-Return-path elaborated `hew_string_drop` `CowHeap` drops in one
/// function — the BOUND (sub-problem A) scope-exit release path. Return exits
/// are mutually exclusive, so the per-path obligation is the max over Return
/// exits (a single straight-line function has exactly one Return exit).
fn return_exit_string_drops(pl: &IrPipeline, fn_name: &str) -> usize {
    let f = pl
        .elaborated_mir
        .iter()
        .find(|f| f.name == fn_name)
        .expect("function must be present in elaborated_mir");
    f.drop_plans
        .iter()
        .filter(|(exit, _)| matches!(exit, ExitPath::Return { .. }))
        .map(|(_, plan)| {
            plan.drops
                .iter()
                .filter(|d| {
                    matches!(
                        &d.kind,
                        DropKind::CowHeap { drop_fn } if *drop_fn == "hew_string_drop"
                    )
                })
                .count()
        })
        .max()
        .unwrap_or(0)
}

/// Total `hew_string_drop` obligations a single normal-return execution incurs:
/// the inline (nested/discard) drops plus the per-Return-path scope-exit drops.
fn total_string_drops(pl: &IrPipeline, fn_name: &str) -> usize {
    inline_string_drops(pl, fn_name) + return_exit_string_drops(pl, fn_name)
}

/// Per-Panic-path (bounds-check / OOB trap) elaborated `hew_string_drop`
/// `CowHeap` drops in one function — the max over panic exits. A binding that is
/// `Uninit` at the trap edge (e.g. `let y = xs[i];` traps in the bounds check
/// BEFORE the getter binds `y`) must contribute ZERO here: `enumerate_exits`'
/// Trap arm filters the scope-exit LIFO by the same init-aware `drops_for_exit`
/// predicate the Return arm uses, so an unbound slot is never dropped on the
/// panic path (otherwise `hew_string_drop` would dereference uninitialised stack
/// memory and turn a clean trap into a SIGSEGV).
fn panic_exit_string_drops(pl: &IrPipeline, fn_name: &str) -> usize {
    let f = pl
        .elaborated_mir
        .iter()
        .find(|f| f.name == fn_name)
        .expect("function must be present in elaborated_mir");
    f.drop_plans
        .iter()
        .filter(|(exit, _)| matches!(exit, ExitPath::Panic { .. }))
        .map(|(_, plan)| {
            plan.drops
                .iter()
                .filter(|d| {
                    matches!(
                        &d.kind,
                        DropKind::CowHeap { drop_fn } if *drop_fn == "hew_string_drop"
                    )
                })
                .count()
        })
        .max()
        .unwrap_or(0)
}

fn assert_no_nyi(pl: &IrPipeline) {
    assert!(
        !pl.diagnostics.iter().any(|d| matches!(
            &d.kind,
            hew_mir::MirDiagnosticKind::NotYetImplemented { .. }
        )),
        "unexpected NotYetImplemented gate; diagnostics: {:?}",
        pl.diagnostics
    );
}

// ---------------------------------------------------------------------------
// Canary 1 — BOUND Vec<string> getter: `let y = xs.get(i); y.len()` → one drop.
// ---------------------------------------------------------------------------

#[test]
fn canary1_bound_vec_get_releases_exactly_once() {
    let pl = pipeline_with_tc(
        "fn c1(xs: Vec<string>) -> i64 {\n    let y = xs.get(0);\n    y.len() as i64\n}\n",
    );
    assert_no_nyi(&pl);
    // `y` is a fresh retained owner read only by `len` (a borrow): the BOUND
    // path drops it once at scope exit; the NESTED path adds nothing.
    assert_eq!(
        return_exit_string_drops(&pl, "c1"),
        1,
        "bound Vec<string> getter must drop once at the Return exit"
    );
    assert_eq!(
        inline_string_drops(&pl, "c1"),
        0,
        "bound case is handled by the scope-exit path; no inline drop"
    );
    assert_eq!(total_string_drops(&pl, "c1"), 1);
}

// ---------------------------------------------------------------------------
// Canary 2 — NESTED Vec<string> getter in a loop: `xs.get(i).len()` balances
// the retained owner with exactly one inline drop per iteration.
// ---------------------------------------------------------------------------

#[test]
fn canary2_nested_vec_get_in_loop_balances() {
    let pl = pipeline_with_tc(
        "fn c2(xs: Vec<string>, n: i64) -> i64 {\n    for i in 0..n {\n        xs.get(0).len();\n    }\n    0\n}\n",
    );
    assert_no_nyi(&pl);
    // The bare `hew_vec_get_str` temp is borrowed by `len` then dead: the NESTED
    // path splices exactly one inline drop in the loop body (per-iteration
    // balance of the retain). No binding ⇒ no scope-exit drop.
    assert_eq!(
        inline_string_drops(&pl, "c2"),
        1,
        "nested Vec<string> getter must place exactly one inline hew_string_drop"
    );
    assert_eq!(return_exit_string_drops(&pl, "c2"), 0);
}

// ---------------------------------------------------------------------------
// Canary 3 — BOUND string producers: `let y = s.to_uppercase(); y.len()` and
// `let y = a + b; y.len()` each release exactly once.
// ---------------------------------------------------------------------------

#[test]
fn canary3_bound_string_producers_release_once() {
    let pl = pipeline_with_tc(
        "fn upper(s: string) -> i64 {\n    let y = s.to_uppercase();\n    y.len() as i64\n}\nfn concat(a: string, b: string) -> i64 {\n    let y = a + b;\n    y.len() as i64\n}\n",
    );
    assert_no_nyi(&pl);
    assert_eq!(
        return_exit_string_drops(&pl, "upper"),
        1,
        "bound to_uppercase result must drop once at the Return exit"
    );
    assert_eq!(inline_string_drops(&pl, "upper"), 0);
    assert_eq!(total_string_drops(&pl, "upper"), 1);

    assert_eq!(
        return_exit_string_drops(&pl, "concat"),
        1,
        "bound concat result must drop once at the Return exit"
    );
    assert_eq!(inline_string_drops(&pl, "concat"), 0);
    assert_eq!(total_string_drops(&pl, "concat"), 1);
}

// ---------------------------------------------------------------------------
// Canary 3b — NESTED string producers: `(a + b).len()` and
// `s.to_uppercase().len()` each release the bare temp once, inline.
// ---------------------------------------------------------------------------

#[test]
fn canary3b_nested_string_producers_release_once() {
    let pl = pipeline_with_tc(
        "fn nconcat(a: string, b: string) -> i64 {\n    (a + b).len() as i64\n}\nfn nupper(s: string) -> i64 {\n    s.to_uppercase().len() as i64\n}\n",
    );
    assert_no_nyi(&pl);
    assert_eq!(
        inline_string_drops(&pl, "nconcat"),
        1,
        "nested concat temp must get exactly one inline hew_string_drop"
    );
    assert_eq!(return_exit_string_drops(&pl, "nconcat"), 0);
    assert_eq!(total_string_drops(&pl, "nconcat"), 1);

    assert_eq!(
        inline_string_drops(&pl, "nupper"),
        1,
        "nested to_uppercase temp must get exactly one inline hew_string_drop"
    );
    assert_eq!(return_exit_string_drops(&pl, "nupper"), 0);
    assert_eq!(total_string_drops(&pl, "nupper"), 1);
}

// ---------------------------------------------------------------------------
// Canary 4 — escape / consuming shapes earn NO drop (fail-closed; the owner is
// transferred, so a drop here would double-free).
// ---------------------------------------------------------------------------

#[test]
fn canary4_escaping_and_consuming_shapes_do_not_drop() {
    let pl = pipeline_with_tc(
        "fn ret_escape(a: string, b: string) -> string {\n    a + b\n}\nfn consume(s: string) -> i64 {\n    s.len() as i64\n}\nfn userfn_escape(a: string, b: string) -> i64 {\n    let y = a + b;\n    consume(y)\n}\n",
    );
    assert_no_nyi(&pl);
    // Returned concat → moved to the ReturnSlot (caller owns); no drop here.
    assert_eq!(
        total_string_drops(&pl, "ret_escape"),
        0,
        "a returned fresh string is owned by the caller; the callee must not drop it"
    );
    // `consume(y)` moves `y` into a user fn — an unproven ownership posture; the
    // bound owner must NOT be dropped at the call site (fail-closed leak).
    assert_eq!(
        total_string_drops(&pl, "userfn_escape"),
        0,
        "a string moved into a user-fn call must not also drop at the caller (no double-free)"
    );
}

// ---------------------------------------------------------------------------
// Canary 5 — DISCARD compatibility: a discarded fresh producer (`a + b;`) is
// released by exactly one inline drop, producer-agnostically (this folds the
// vec-branch's Vec-specific discard fix into the general substrate).
// ---------------------------------------------------------------------------

#[test]
fn canary5_discarded_producer_releases_once() {
    let pl = pipeline_with_tc(
        "fn dconcat(a: string, b: string) {\n    a + b;\n}\nfn dvecget(xs: Vec<string>) {\n    xs.get(0);\n}\n",
    );
    assert_no_nyi(&pl);
    assert_eq!(
        inline_string_drops(&pl, "dconcat"),
        1,
        "a discarded concat must be released by one inline hew_string_drop"
    );
    assert_eq!(
        inline_string_drops(&pl, "dvecget"),
        1,
        "a discarded Vec<string> getter (retained owner) must be released by one inline drop"
    );
}

// ---------------------------------------------------------------------------
// Index-form canaries (`vec-generic-index` lane). `xs[i]` over `Vec<string>`
// lowers to the same `hew_vec_get_str` retained owner as `.get(i)`, so the
// producer-agnostic substrate releases each shape with no Vec-specific code.
// These pin every release shape on the INDEX spelling, plus the OOB
// bounds-check trap edge that is unique to `xs[i]`.
// ---------------------------------------------------------------------------

/// BOUND index: `let y = xs[i]; y.len()` releases the retained owner exactly
/// once at the (single) Return exit via the substrate BOUND path — the same
/// proof as `canary1`, on the index spelling. This is the original lane request
/// (`let y = xs[1]`).
#[test]
fn index_bound_releases_exactly_once() {
    let pl = pipeline_with_tc(
        "fn ib(xs: Vec<string>, i: i64) -> i64 {\n    let y = xs[i];\n    y.len() as i64\n}\n",
    );
    assert_no_nyi(&pl);
    assert_eq!(
        return_exit_string_drops(&pl, "ib"),
        1,
        "bound Vec<string> index must drop the retained owner once at the Return exit"
    );
    assert_eq!(
        inline_string_drops(&pl, "ib"),
        0,
        "the bound case releases via the scope-exit path, not an inline drop"
    );
    assert_eq!(
        total_string_drops(&pl, "ib"),
        1,
        "exactly one drop obligation per normal-return execution"
    );
}

/// NESTED index in a loop: `xs[i].len();` borrows the retained temp then drops
/// it inline via the substrate NESTED path — the same proof as `canary2`, on
/// the index spelling. Exactly one inline drop SITE (fires once per iteration).
#[test]
fn index_nested_in_loop_balances() {
    let pl = pipeline_with_tc(
        "fn inl(xs: Vec<string>, n: i64) -> i64 {\n    for i in 0..n {\n        xs[i].len();\n    }\n    0\n}\n",
    );
    assert_no_nyi(&pl);
    assert_eq!(
        inline_string_drops(&pl, "inl"),
        1,
        "nested Vec<string> index must place exactly one inline hew_string_drop"
    );
    assert_eq!(
        return_exit_string_drops(&pl, "inl"),
        0,
        "a nested temp has no binding, so it earns no scope-exit drop"
    );
}

/// DISCARD index: `xs[i];` releases the unused retained owner with exactly one
/// inline drop via the substrate DISCARD path. This is the path that SUBSUMED
/// the Vec-specific `release_discarded_vec_string_index` helper the lane removed
/// after rebasing onto the general owned-string temp substrate.
#[test]
fn index_discard_releases_once() {
    let pl = pipeline_with_tc("fn idc(xs: Vec<string>, i: i64) {\n    xs[i];\n}\n");
    assert_no_nyi(&pl);
    assert_eq!(
        inline_string_drops(&pl, "idc"),
        1,
        "a discarded Vec<string> index must be released by one inline hew_string_drop"
    );
    assert_eq!(
        return_exit_string_drops(&pl, "idc"),
        0,
        "a discarded temp has no binding, so it earns no scope-exit drop"
    );
}

/// OOB-TRAP edge — the shape unique to `xs[i]` vs the unconditional `.get(i)`
/// call. `let y = xs[i]; y.len()` emits an explicit bounds-check trap BEFORE the
/// getter binds `y`. On that trap edge `y` is `Uninit`, so NO scope-exit drop
/// may fire: dropping the unbound slot would `hew_string_drop` an uninitialised
/// pointer and degrade a clean OOB trap (SIGTRAP, code 205) into a SIGSEGV. The
/// in-bounds Return path still releases the owner exactly once; only the panic
/// edge is empty of string drops.
#[test]
fn index_bound_oob_trap_drops_nothing() {
    let pl = pipeline_with_tc(
        "fn ot(xs: Vec<string>, i: i64) -> i64 {\n    let y = xs[i];\n    y.len() as i64\n}\n",
    );
    assert_no_nyi(&pl);
    assert_eq!(
        return_exit_string_drops(&pl, "ot"),
        1,
        "the in-bounds path must still release the bound owner once",
    );
    assert_eq!(
        panic_exit_string_drops(&pl, "ot"),
        0,
        "the OOB bounds-check trap precedes the getter; the Uninit binding must \
         not be dropped on the trap edge (else a clean trap degrades to SIGSEGV)",
    );
}
