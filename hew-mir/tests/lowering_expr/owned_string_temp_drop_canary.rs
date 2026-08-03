//! W5.011 P3 — owned-`string` temporary/drop substrate canaries.
//!
//! A fresh-owned `string` result used in a non-consuming (borrowing) context
//! must be released EXACTLY ONCE — never zero (leak), never twice (refcount
//! over-decrement / premature free). `string` is refcounted (`hew_string_drop`
//! decrements and frees at zero); the `CalleeOwnershipContract` result
//! projection marks fresh-owned-string producers (`hew_string_concat`,
//! `_to_uppercase`, the `Vec<string>` getter `hew_vec_get_str`, …) as handing
//! the caller exactly one drop obligation, and a
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
//! Returns and consuming runtime/container sinks transfer ownership. Ordinary
//! Hew calls borrow, so their caller-owned string keeps its drop.
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
    assert!(
        tc_output.errors.is_empty(),
        "type-check errors: {:#?}",
        tc_output.errors
    );
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
                } if *s == hew_mir::DropFnSpec::Release("hew_string_drop")
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
                        DropKind::CowHeap { release } if release.release_symbol() == "hew_string_drop"
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

/// Callee-side `+1` mints for a returned borrowed string parameter.
fn string_retains(pl: &IrPipeline, fn_name: &str) -> usize {
    pl.raw_mir
        .iter()
        .filter(|f| f.name == fn_name)
        .flat_map(|f| f.blocks.iter())
        .flat_map(|b| b.instructions.iter())
        .filter(|i| matches!(i, Instr::StringRetain { .. }))
        .count()
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
                        DropKind::CowHeap { release } if release.release_symbol() == "hew_string_drop"
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

#[test]
fn borrowed_projection_from_owned_index_uses_the_parent_owner() {
    let pl = pipeline_with_tc(
        r"
        fn borrow_len(value: string) -> i64 { value.len() }

        fn projected(values: Vec<(string, string)>) -> i64 {
            borrow_len(values[0].0)
        }
        ",
    );
    assert_no_nyi(&pl);
}

// ---------------------------------------------------------------------------
// Canary 1 — BOUND Vec<string> getter: `let y = xs[i]; y.len()` → one drop.
// ---------------------------------------------------------------------------

#[test]
fn canary1_bound_vec_get_releases_exactly_once() {
    let pl = pipeline_with_tc(
        "fn c1(xs: Vec<string>) -> i64 {\n    let y = xs[0];\n    y.len() as i64\n}\n",
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
// Canary 2 — NESTED Vec<string> getter in a loop: `xs[i].len()` balances
// the retained owner with exactly one inline drop per iteration.
// ---------------------------------------------------------------------------

#[test]
fn canary2_nested_vec_get_in_loop_balances() {
    let pl = pipeline_with_tc(
        "fn c2(xs: Vec<string>, n: i64) -> i64 {\n    for i in 0..n {\n        xs[0].len();\n    }\n    0\n}\n",
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
// Canary 3 — BOUND string producers: `let y = s.to_upper(); y.len()` and
// `let y = a + b; y.len()` each release exactly once.
// ---------------------------------------------------------------------------

#[test]
fn canary3_bound_string_producers_release_once() {
    let pl = pipeline_with_tc(
        "fn upper(s: string) -> i64 {\n    let y = s.to_upper();\n    y.len() as i64\n}\nfn concat(a: string, b: string) -> i64 {\n    let y = a + b;\n    y.len() as i64\n}\n",
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
// `s.to_upper().len()` each release the bare temp once, inline.
// ---------------------------------------------------------------------------

#[test]
fn canary3b_nested_string_producers_release_once() {
    let pl = pipeline_with_tc(
        "fn nconcat(a: string, b: string) -> i64 {\n    (a + b).len() as i64\n}\nfn nupper(s: string) -> i64 {\n    s.to_upper().len() as i64\n}\n",
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
// Canary 4 — returns transfer ownership, while ordinary by-value calls borrow.
// ---------------------------------------------------------------------------

#[test]
fn canary4_return_transfers_and_user_call_borrows() {
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
    // `consume(y)` borrows `y`; the caller keeps and releases its owner.
    assert_eq!(
        total_string_drops(&pl, "userfn_escape"),
        1,
        "a string passed to a user function remains caller-owned"
    );
}

// ---------------------------------------------------------------------------
// Canary 4b — TEMP-arg caller-side mint (#2428 residual). A fresh string-CALL
// (`s.to_upper()`) or f-string (`f"n={n}"`) result passed BY VALUE as a
// temporary argument to a BORROWING user function has no `let`, so the
// #2743/#2745 caller-side mint gives its otherwise-unowned temp exactly one
// scope-exit drop. Before this fix the classifier recognised only the
// `Binary`/`Unary` concat producer (`arg_concat`, the baseline here); a
// string-returning `Call` hit the catch-all, skipped the mint, and leaked
// 32 B/call. Each producer now earns exactly one scope-exit release.
// ---------------------------------------------------------------------------

#[test]
fn canary4b_string_call_temp_arg_releases_once() {
    let pl = pipeline_with_tc(
        "fn borrow_len(s: string) -> i64 {\n    s.len() as i64\n}\n\
         fn arg_concat(a: string, b: string) -> i64 {\n    borrow_len(a + b)\n}\n\
         fn arg_upper(s: string) -> i64 {\n    borrow_len(s.to_upper())\n}\n\
         fn arg_fstring(n: i64) -> i64 {\n    borrow_len(f\"n={n}\")\n}\n",
    );
    assert_no_nyi(&pl);
    // The by-value temp arg has no binding, so the release is the caller-side
    // synthetic-owner scope-exit drop (never an inline nested drop): exactly one
    // per producer. `arg_fstring` additionally carries one inline drop for its
    // `to_string_i64` intermediate (a nested fresh temp borrowed by the concat),
    // which is orthogonal to the arg-temp mint under test here.
    for f in ["arg_concat", "arg_upper", "arg_fstring"] {
        assert_eq!(
            return_exit_string_drops(&pl, f),
            1,
            "{f}: the fresh string temp arg to a borrowing fn must earn exactly one scope-exit drop"
        );
    }
    // The concat baseline and the string-call producer have no intermediate, so
    // their ONLY string drop is the arg-temp scope-exit release.
    assert_eq!(total_string_drops(&pl, "arg_concat"), 1);
    assert_eq!(total_string_drops(&pl, "arg_upper"), 1);
}

// ---------------------------------------------------------------------------
// Return carriers — pointer aliasing does not imply a borrowed return.
//
// A whole by-value string parameter is retained before the return-slot move.
// A string projection is retained by the field load. Both therefore hand the
// caller exactly one independently releasable share even though the returned
// pointer can alias input storage. A direct borrowing consumer must give that
// anonymous call-result carrier exactly one caller-side release.
// ---------------------------------------------------------------------------

#[test]
fn parameter_and_projection_return_carriers_release_once_at_direct_consumer() {
    let pl = pipeline_with_tc(
        "record Holder { value: string }\n\
         fn passthru(value: string) -> string { value }\n\
         fn choose(holder: Holder, fallback: string, project: bool) -> string {\n\
         \x20   if project { holder.value } else { fallback }\n\
         }\n\
         fn repeated(value: string, first: bool) -> string {\n\
         \x20   if first { value } else { value }\n\
         }\n\
         fn nested(value: string, through_call: bool) -> string {\n\
         \x20   if through_call { passthru(value) } else { value }\n\
         }\n\
         fn early(value: string, return_value: bool) -> string {\n\
         \x20   if return_value { return value; }\n\
         \x20   \"static-control\"\n\
         }\n\
         fn return_join_twice(holder: Holder, fallback: string, project: bool, early: bool) -> string {\n\
         \x20   let joined = if project { holder.value } else { fallback };\n\
         \x20   if early { return joined; }\n\
         \x20   joined\n\
         }\n\
         fn identity<T>(value: T) -> T { value }\n\
         fn borrow_len(value: string) -> i64 { value.len() }\n\
         fn direct(value: string) -> i64 { borrow_len(passthru(value)) }\n\
         fn mixed(holder: Holder, fallback: string, project: bool) -> i64 {\n\
         \x20   borrow_len(choose(holder, fallback, project))\n\
         }\n\
         fn repeat_call(value: string, first: bool) -> i64 {\n\
         \x20   borrow_len(repeated(value, first))\n\
         }\n\
         fn nested_call(value: string, through_call: bool) -> i64 {\n\
         \x20   borrow_len(nested(value, through_call))\n\
         }\n\
         fn early_call(value: string, return_value: bool) -> i64 {\n\
         \x20   borrow_len(early(value, return_value))\n\
         }\n\
         fn return_join_twice_call(holder: Holder, fallback: string, project: bool, early: bool) -> i64 {\n\
         \x20   borrow_len(return_join_twice(holder, fallback, project, early))\n\
         }\n\
         fn generic_call(value: string) -> i64 {\n\
         \x20   borrow_len(identity<string>(value))\n\
         }\n\
         fn return_again(value: string) -> string {\n\
         \x20   passthru(value)\n\
         }\n",
    );
    assert_no_nyi(&pl);

    assert_eq!(
        string_retains(&pl, "passthru"),
        1,
        "the forwarded parameter must gain exactly one return share"
    );
    assert_eq!(
        string_retains(&pl, "choose"),
        1,
        "only the forwarded branch needs an explicit retain; the projection \
         branch is retained by its field load"
    );
    assert_eq!(
        string_retains(&pl, "return_join_twice"),
        1,
        "multiple return slots for one mixed join must not duplicate its \
         path-specific retain"
    );
    assert_eq!(
        string_retains(&pl, "nested"),
        1,
        "a nested carrier already owns its share; only the directly forwarded \
         sibling arm needs a retain"
    );
    for caller in [
        "direct",
        "mixed",
        "repeat_call",
        "nested_call",
        "early_call",
        "return_join_twice_call",
        "generic_call",
    ] {
        assert_eq!(
            return_exit_string_drops(&pl, caller),
            1,
            "{caller}: the anonymous returned carrier borrowed by the consumer \
             must have one caller-side scope-exit release"
        );
        assert_eq!(
            inline_string_drops(&pl, caller),
            0,
            "{caller}: the carrier is owned by the synthetic binding path, not \
             by the nested runtime-temp path"
        );
    }
    assert_eq!(
        total_string_drops(&pl, "return_again"),
        0,
        "a returned carrier transferred onward is not a borrowing consumer and \
         must not gain a caller-side drop"
    );
}

#[test]
fn bound_return_carrier_keeps_one_release_without_a_second_temp_owner() {
    let pl = pipeline_with_tc(
        "fn passthru(value: string) -> string { value }\n\
         fn bound(value: string) -> i64 {\n\
         \x20   let returned = passthru(value);\n\
         \x20   returned.len()\n\
         }\n",
    );
    assert_no_nyi(&pl);
    assert_eq!(
        total_string_drops(&pl, "bound"),
        1,
        "binding the returned carrier must preserve the existing exactly-once \
         release path"
    );
}

const CLOSURE_STRING_CARRIER_SOURCE: &str = r#"
        fn invoke(make: fn() -> string) -> string {
            make()
        }

        fn borrow_len(value: string) -> i64 {
            value.len()
        }

        fn captured(seed: string) -> i64 {
            let make = || seed;
            borrow_len(make())
        }

        fn parameter() -> i64 {
            let identity = |value: string| value;
            borrow_len(identity("parameter-owner".to_upper()))
        }

        fn fresh() -> i64 {
            let make = || "x".to_upper();
            borrow_len(make())
        }

        fn explicit_return_only() -> i64 {
            let make = || -> string {
                return "explicit-owner".to_upper();
            };
            borrow_len(make())
        }

        fn wrapped(seed: string) -> i64 {
            let make = || seed;
            borrow_len(invoke(make))
        }

        fn nested_runtime(seed: string) -> i64 {
            let make = || seed;
            make().len()
        }

        fn discarded() {
            let make = || "discarded".to_upper();
            make();
        }

        "#;

#[test]
fn closure_invoke_string_carriers_release_once_without_widening_opaque_externs() {
    let pl = pipeline_with_tc(CLOSURE_STRING_CARRIER_SOURCE);
    assert_no_nyi(&pl);
    assert_eq!(
        string_retains(&pl, "__hew_closure_invoke_parameter_0"),
        1,
        "the identity closure shim must retain its borrowed string parameter \
         before returning an independently releasable share"
    );
    assert_eq!(
        string_retains(&pl, "__hew_closure_invoke_fresh_0"),
        0,
        "a closure shim returning a fresh string producer must not retain its \
         already-owned result a second time"
    );
    for caller in ["captured", "fresh", "wrapped", "explicit_return_only"] {
        assert_eq!(
            total_string_drops(&pl, caller),
            1,
            "{caller}: every closure-invoke string result carries exactly one \
             caller-owned share, including through a Hew wrapper"
        );
    }
    assert_eq!(
        total_string_drops(&pl, "parameter"),
        2,
        "the heap-producing closure argument keeps its original caller drop \
         obligation while the identity result carries the shim-retained share"
    );
    assert_eq!(
        inline_string_drops(&pl, "parameter"),
        1,
        "the fresh argument share must be released immediately after the \
         borrowing CallClosure"
    );
    assert_eq!(
        return_exit_string_drops(&pl, "parameter"),
        1,
        "the closure result retains a distinct share balanced by the existing \
         caller-side result owner"
    );
    for caller in ["nested_runtime", "discarded"] {
        assert_eq!(
            inline_string_drops(&pl, caller),
            1,
            "{caller}: a bare CallClosure string temp must receive one inline \
             release after its borrowing runtime use or discard"
        );
        assert_eq!(
            return_exit_string_drops(&pl, caller),
            0,
            "{caller}: the bare CallClosure temp must not also acquire a \
             binding-scoped owner"
        );
    }
}

#[test]
fn direct_opaque_extern_string_scrutinee_fails_closed() {
    let pl = pipeline_with_tc(
        r#"
        extern "C" { fn host_opaque_string() -> string; }
        fn opaque_extern_wrapper() -> string { unsafe { host_opaque_string() } }
        fn borrow_len(value: string) -> i64 { value.len() }
        fn main() -> i64 { borrow_len(opaque_extern_wrapper()) }
        "#,
    );
    assert!(
        pl.diagnostics.iter().any(|diagnostic| matches!(
            &diagnostic.kind,
            hew_mir::MirDiagnosticKind::NotYetImplemented { construct, .. }
                if construct == "call-scrutinee ownership is unresolved"
        )),
        "an ownership-opaque result must stop before codegen: {:#?}",
        pl.diagnostics
    );
    assert_eq!(total_string_drops(&pl, "main"), 0);
}

#[test]
fn opaque_extern_string_return_cannot_be_laundered_through_function_value() {
    let pl = pipeline_with_tc(
        r#"
        type FactoryBox {
            make: fn() -> string;
        }

        extern "C" {
            fn host_factory() -> fn() -> string;
            fn host_factory_box() -> FactoryBox;
            fn host_opaque_string() -> string;
        }

        fn borrow_len(value: string) -> i64 {
            value.len()
        }

        fn direct_extern_factory() -> i64 {
            let make = unsafe { host_factory() };
            borrow_len(make())
        }

        fn aggregate_extern_factory() -> i64 {
            let factory = unsafe { host_factory_box() };
            borrow_len((factory.make)())
        }

        fn opaque_wrapper() -> string {
            unsafe { host_opaque_string() }
        }

        fn closure_wrapped_extern() -> i64 {
            let make = || opaque_wrapper();
            borrow_len(make())
        }

        fn closure_explicit_return_extern() -> i64 {
            let make = || -> string {
                return opaque_wrapper();
            };
            borrow_len(make())
        }

        fn domestic_factory() -> fn() -> string {
            || "domestic".to_upper()
        }

        fn domestic_factory_is_preserved() -> i64 {
            let make = domestic_factory();
            borrow_len(make())
        }
        "#,
    );

    let foreign_factory_refusals = pl
        .diagnostics
        .iter()
        .filter(|diagnostic| {
            matches!(
                &diagnostic.kind,
                hew_mir::MirDiagnosticKind::NotYetImplemented { construct, .. }
                    if construct.contains(
                        "returning a string-returning callable value"
                    )
            )
        })
        .count();
    assert_eq!(
        foreign_factory_refusals, 2,
        "both direct and record-contained ownership-opaque extern factories \
         must fail closed before their callable pairs can acquire the \
         ClosureInvoke +1 return contract; \
         diagnostics: {:#?}",
        pl.diagnostics
    );
    let closure_refusals = pl
        .diagnostics
        .iter()
        .filter(|diagnostic| {
            matches!(
                &diagnostic.kind,
                hew_mir::MirDiagnosticKind::NotYetImplemented { construct, .. }
                    if construct
                        == "closure string return without an owned-return contract"
            )
        })
        .count();
    assert_eq!(
        closure_refusals, 2,
        "both tail and tail-less explicit-return closure paths forwarding an opaque \
         string extern wrapper must remain fail-closed; \
         diagnostics: {:#?}",
        pl.diagnostics
    );
    assert_eq!(
        total_string_drops(&pl, "domestic_factory_is_preserved"),
        1,
        "a Hew-produced callable remains admitted and its string result carries \
         exactly one caller release"
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
        "fn dconcat(a: string, b: string) {\n    a + b;\n}\nfn dvecget(xs: Vec<string>) {\n    xs[0];\n}\n",
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

#[test]
fn discarded_audited_runtime_string_result_releases_once() {
    let pl = pipeline_with_tc(
        r#"
extern "C" {
    fn hew_stream_last_error() -> string;
}

fn drain_error() {
    unsafe {
        let _ = hew_stream_last_error();
    }
}
"#,
    );
    assert_no_nyi(&pl);
    assert_eq!(
        inline_string_drops(&pl, "drain_error"),
        1,
        "an audited runtime extern with a measured transferred string result \
         still needs one caller-side drop when discarded"
    );
    assert_eq!(return_exit_string_drops(&pl, "drain_error"), 0);
}

#[test]
fn audited_xml_string_result_and_forwarder_release_once() {
    let pl = pipeline_with_tc(
        r#"
extern "C" {
    fn hew_xml_to_string(node: i64) -> string;
}

fn xml_text(node: i64) -> string {
    unsafe { hew_xml_to_string(node) }
}

fn borrow_len(value: string) -> i64 {
    value.len()
}

fn forwarded(node: i64) -> i64 {
    borrow_len(xml_text(node))
}
"#,
    );
    assert_no_nyi(&pl);
    assert_eq!(
        total_string_drops(&pl, "forwarded"),
        1,
        "the measured XML string transfer must carry exactly one caller-side \
         release through its Hew forwarder"
    );
    assert_eq!(
        total_string_drops(&pl, "xml_text"),
        0,
        "the forwarding function transfers the XML string owner to its caller"
    );
}

#[test]
#[allow(
    clippy::too_many_lines,
    reason = "one ownership-boundary matrix keeps positive wrapper forms and their fail-closed controls in the same checked module"
)]
fn audited_extern_string_temp_through_unsafe_tail_releases_once() {
    let pl = pipeline_with_tc(
        r#"
extern "C" {
    fn hew_xml_to_string(node: i64) -> string;
}

fn xml_text(node: i64) -> string {
    unsafe { hew_xml_to_string(node) }
}

fn borrow_len(value: string) -> i64 {
    value.len()
}

fn domestic(node: i64) -> string {
    f"node={node}"
}

fn direct(node: i64) -> i64 {
    borrow_len(unsafe { hew_xml_to_string(node) })
}

fn nested(node: i64) -> i64 {
    borrow_len(unsafe { unsafe { hew_xml_to_string(node) } })
}

fn plain(node: i64) -> i64 {
    borrow_len(domestic(node))
}

fn forwarded(node: i64) -> i64 {
    borrow_len(xml_text(node))
}

fn statement_tail(node: i64) -> i64 {
    borrow_len(unsafe {
        let marker = 0;
        hew_xml_to_string(node + marker)
    })
}

fn immutable_alias(node: i64) -> i64 {
    borrow_len(unsafe {
        let value = hew_xml_to_string(node);
        value
    })
}

fn all_fresh_if(node: i64, take_first: bool) -> i64 {
    borrow_len(unsafe {
        if take_first {
            hew_xml_to_string(node)
        } else {
            hew_xml_to_string(node + 1)
        }
    })
}

fn all_fresh_match(node: i64, choice: i64) -> i64 {
    borrow_len(unsafe {
        match choice {
            0 => hew_xml_to_string(node),
            _ => hew_xml_to_string(node + 1),
        }
    })
}

fn mixed_if(node: i64, fallback: string, take_fresh: bool) -> i64 {
    borrow_len(unsafe {
        if take_fresh {
            hew_xml_to_string(node)
        } else {
            fallback
        }
    })
}

fn mutable_alias(node: i64) -> i64 {
    borrow_len(unsafe {
        var value = hew_xml_to_string(node);
        value
    })
}

fn static_literal() -> i64 {
    borrow_len(unsafe { "static" })
}

fn borrowed(value: string) -> i64 {
    borrow_len(unsafe { value })
}

fn opaque(make: fn() -> string) -> string {
    make()
}

fn opaque_wrapped(make: fn() -> string) -> i64 {
    borrow_len(unsafe { opaque(make) })
}
"#,
    );
    assert_no_nyi(&pl);
    for caller in [
        "direct",
        "nested",
        "statement_tail",
        "immutable_alias",
        "mutable_alias",
        "all_fresh_if",
        "all_fresh_match",
        "plain",
        "forwarded",
    ] {
        assert_eq!(
            total_string_drops(&pl, caller),
            1,
            "`{caller}` must balance the measured transferred string with \
             exactly one caller-side release"
        );
    }
    assert_eq!(
        total_string_drops(&pl, "mixed_if"),
        1,
        "the fresh-or-borrowed join retains one independent result share"
    );
    for caller in ["static_literal", "borrowed"] {
        assert_eq!(
            total_string_drops(&pl, caller),
            0,
            "`{caller}` has no audited fresh-producer tail and must not acquire \
             a synthetic owner"
        );
    }
    assert_eq!(
        total_string_drops(&pl, "opaque_wrapped"),
        1,
        "the indirect closure ABI returns one independently releasable string \
         share, so its Hew wrapper must propagate that carrier authority"
    );
}

#[test]
fn measured_markdown_result_releases_once_direct_and_through_wrapper() {
    let pl = pipeline_with_tc(
        r#"
extern "C" {
    fn hew_markdown_to_html(markdown: string) -> string;
}

fn to_html(markdown: string) -> string {
    unsafe { hew_markdown_to_html(markdown) }
}

fn borrow_len(value: string) -> i64 {
    value.len()
}

fn direct(markdown: string) -> i64 {
    let html = unsafe { hew_markdown_to_html(markdown) };
    html.len()
}

fn forwarded(markdown: string) -> i64 {
    to_html(markdown).len()
}
"#,
    );
    assert_no_nyi(&pl);
    for caller in ["direct", "forwarded"] {
        assert_eq!(
            total_string_drops(&pl, caller),
            1,
            "{caller}: a measured Markdown result borrowed by the caller must \
             earn exactly one caller-side string drop"
        );
    }
    assert_eq!(
        total_string_drops(&pl, "to_html"),
        0,
        "the shipped-wrapper shape forwards its owner instead of releasing it"
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

// ---------------------------------------------------------------------------
// f-string interpolation temp release (the rc1 drop-safety completion
// pass). `f"item-{i}"` over a non-string value desugars
// (`hew-hir/src/lower.rs::lower_interpolated_string`) to a chain of
// `stdlib_catalog` presentation-name calls: `to_string_i64(i)` (a fresh
// conversion temp) then `string_concat(lit, temp)` (the join). Both reach MIR
// as `Terminator::Call` to the CATALOG name, not the `hew_*` c-symbol; unlike
// its `to_string_i64`/`println_str` siblings, `string_concat` had no
// `callee_ownership_contract` row (only `hew_string_concat` did), so it fell
// through to `FAIL_CLOSED` and neither the conversion temp (unrecognised
// borrowing use) nor the concat's own result (unrecognised fresh producer)
// was ever admitted by this file's NESTED/DISCARD substrate — both leaked.
// ---------------------------------------------------------------------------

/// NESTED statement position: `println(f"item-{i}")` — neither temp is bound
/// to a `let`, so both are the substrate's NESTED shape. Exactly one inline
/// drop each: the conversion temp (borrowed by the concat) and the concat
/// result (borrowed by `println_str`, a covered print sink).
#[test]
fn canary6_fstring_interpolation_statement_position_releases_both_temps() {
    let pl = pipeline_with_tc("fn f6(i: i64) {\n    println(f\"item-{i}\");\n}\n");
    assert_no_nyi(&pl);
    assert_eq!(
        inline_string_drops(&pl, "f6"),
        2,
        "f-string interpolation of a non-string value must release both the \
         Display::fmt conversion temp (hew_i64_to_string) and the \
         hew_string_concat join result -- one inline drop each"
    );
    assert_eq!(return_exit_string_drops(&pl, "f6"), 0);
}

/// Gen-body shape: a standalone `gen fn` yields `f"item-{i}"` per iteration.
/// The concat result is published through the yield-transport (a MOVE,
/// correctly excluded from this substrate), so only the conversion temp is at
/// risk here -- but `lower_gen_block` builds the coroutine ramp's
/// `RawMirFunction` through its own hand-rolled pipeline instead of
/// `lower_function`, so it never called `apply_nested_fresh_string_temp_drops`
/// at all (a SEPARATE gap from the catalog-name contract row above; every
/// ordinary function gets the splice for free via `lower_function`'s shared
/// post-`finalize_blocks` step). The gen-body ramp is emitted as its own MIR
/// function named `__hew_gen_body_<owner>_<id>`.
#[test]
fn canary7_fstring_interpolation_gen_yield_releases_conversion_temp() {
    let pl = pipeline_with_tc(
        "gen fn g7(n: i64) -> string {\n    var i: i64 = 0;\n    while i < n {\n        yield f\"item-{i}\";\n        i = i + 1;\n    }\n}\n",
    );
    assert_no_nyi(&pl);
    assert_eq!(
        inline_string_drops(&pl, "__hew_gen_body_g7_0"),
        1,
        "the hew_i64_to_string conversion temp feeding the yielded f-string \
         must release exactly once inside the generator body ramp"
    );
    assert_eq!(
        return_exit_string_drops(&pl, "__hew_gen_body_g7_0"),
        0,
        "the conversion temp is a NESTED (unbound) shape, not a scope-exit-drop binding"
    );
}

#[test]
fn terminator_produced_nested_concat_operand_releases_after_borrowing_concat() {
    let pl = pipeline_with_tc(
        r#"
fn compose() -> i64 {
    let full = "left-" + "middle".to_upper() + "-right";
    full.len()
}
"#,
    );
    assert_no_nyi(&pl);
    assert_eq!(
        inline_string_drops(&pl, "compose"),
        2,
        "the function-returned operand and the first concat intermediate each need one inline drop"
    );
    assert_eq!(
        return_exit_string_drops(&pl, "compose"),
        1,
        "the final bound concat result remains owned by the ordinary scope-exit path"
    );
}
