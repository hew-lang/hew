//! For-await / generator consumer loop-variable release on early-`return`
//! bodies.
//!
//! The consuming body of `for await v in stream` (and `for x in gen()`)
//! releases its fresh, solely-owned yielded value on every path out of the
//! body: the fall-through body-end drop, the `break`/`continue` edge drops,
//! and — pinned here — the early-`return` edge. A `Terminator::Return` on
//! some body path must not be treated as an ownership escape of the loop
//! variable: doing so suppressed the body-end drop for the WHOLE binding, so
//! every iteration leaked its received value (one `alloc_cstring_data` node
//! per yield), not just the returning one.
//!
//! Exactly-once walls (the wrong fix is a double-free):
//!   * the return edge and the body-end drop are mutually exclusive in the
//!     CFG — each runtime path releases once;
//!   * `return v` (the loop variable itself) moves ownership to the caller:
//!     BOTH releases must stay suppressed (leak-not-double-free posture; the
//!     `ReturnSlot` `Instr::Move` is the escape the body scan catches).
//!
//! LESSONS: cleanup-all-exits (P1), raii-null-after-move (P0),
//! drop-allowset-from-value-flow (P0).

use hew_hir::{lower_program, ResolutionCtx};
use hew_mir::{lower_hir_module, Instr, IrPipeline, SuspendKind, Terminator};
use hew_types::module_registry::ModuleRegistry;
use hew_types::{Checker, ResolvedTy};

/// Full front-half pipeline with type-checking (actors, streams, and
/// generators need checker side-tables to lower).
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

/// Inline `hew_string_drop` releases in `fn_name`, split by whether the
/// carrying block ends in `Terminator::Return` (the early-return edge) or not
/// (the body-end / fall-through release).
///
/// This sees only the releases lowering writes into the instruction stream.
/// An edge whose release is scheduled by drop elaboration instead does not
/// appear here; ask [`string_releases_on_return_exits`] for that edge.
fn string_drops_by_edge(pl: &IrPipeline, fn_name: &str) -> (usize, usize) {
    let f = pl
        .raw_mir
        .iter()
        .find(|f| f.name == fn_name)
        .expect("function must be present in raw_mir");
    let mut on_return_edge = 0;
    let mut on_fall_through = 0;
    for block in &f.blocks {
        let drops = block
            .instructions
            .iter()
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
            .count();
        if matches!(block.terminator, Terminator::Return) {
            on_return_edge += drops;
        } else {
            on_fall_through += drops;
        }
    }
    (on_return_edge, on_fall_through)
}

/// String releases scheduled on every `Return` exit plan of `fn_name`.
///
/// The elaborated drop plan is the release authority for an exit edge: an
/// owner whose generation is live when control leaves the function is
/// released by its plan entry, not by an instruction the body wrote. A
/// released frame therefore shows up here even when the instruction stream
/// carries no `Instr::Drop` for it.
fn string_releases_on_return_exits(pl: &IrPipeline, fn_name: &str) -> usize {
    let f = pl
        .elaborated_mir
        .iter()
        .find(|f| f.name == fn_name)
        .expect("function must be present in elaborated_mir");
    f.drop_plans
        .iter()
        .filter(|(exit, _)| matches!(exit, hew_mir::ExitPath::Return { .. }))
        .flat_map(|(_, plan)| plan.drops.iter())
        .filter(|drop| drop.ty == ResolvedTy::String)
        .count()
}

fn assert_no_nyi(pl: &IrPipeline) {
    let nyi: Vec<_> = pl
        .diagnostics
        .iter()
        .filter(|d| matches!(d.kind, hew_mir::MirDiagnosticKind::NotYetImplemented { .. }))
        .collect();
    assert!(nyi.is_empty(), "unexpected NYI diagnostics: {nyi:#?}");
}

fn string_drops_for_call_arg(
    pl: &IrPipeline,
    fn_name: &str,
    target_callee: &str,
    arg_index: usize,
) -> usize {
    let function = pl
        .raw_mir
        .iter()
        .find(|function| function.name == fn_name)
        .expect("function must be present in raw MIR");
    let arg_place = function
        .blocks
        .iter()
        .find_map(|block| match &block.terminator {
            Terminator::Call {
                callee: actual_callee,
                args,
                ..
            } if actual_callee == target_callee => args.get(arg_index).copied(),
            _ => None,
        })
        .expect("named call argument must be present in raw MIR");
    function
        .blocks
        .iter()
        .flat_map(|block| block.instructions.iter())
        .filter(|instruction| {
            matches!(
                instruction,
                Instr::Drop {
                    place,
                    ty: ResolvedTy::String,
                    drop_fn: Some(spec),
                    ..
                } if *place == arg_place
                    && *spec == hew_mir::DropFnSpec::Release("hew_string_drop")
            )
        })
        .count()
}

/// A stream can be created before an unrelated await and transferred into its
/// `for await` cursor only after that suspension resumes.  Destroying the
/// coroutine while parked must close the original stream; after the transfer,
/// only the cursor may own that close.  This pins both sides of the hand-off so
/// a global LIFO re-add cannot hide a double-close behind the leak fix.
#[test]
#[allow(
    clippy::too_many_lines,
    reason = "the regression pins both sides of one ownership hand-off in one readable fixture"
)]
fn forawait_stream_source_is_closed_on_pretransfer_suspend_only() {
    let pl = pipeline_with_tc(
        r#"
        actor Maker {
            receive gen fn items() -> string {
                yield "one";
            }

            receive fn tick() -> i64 {
                1
            }
        }

        actor Drain {
            receive fn run(m: LocalPid<Maker>) {
                let input = m.items();
                let _ready = await m.tick();
                for await item in input {
                    println(item);
                }
            }
        }
        "#,
    );
    assert_no_nyi(&pl);
    assert!(
        !pl.diagnostics.iter().any(|diag| matches!(
            diag.kind,
            hew_mir::MirDiagnosticKind::ObligationUnderReleased { ref function, .. }
                if function == "Drain__recv__run"
        )),
        "pre-transfer stream ownership must balance on every suspend-abandon edge: {:#?}",
        pl.diagnostics
    );
    assert!(
        !pl.diagnostics.iter().any(|diag| matches!(
            diag.kind,
            hew_mir::MirDiagnosticKind::ObligationOverReleased { ref function, .. }
                if function == "Drain__recv__run"
        )),
        "the source/cursor hand-off must not add a second stream close: {:#?}",
        pl.diagnostics
    );

    let raw = pl
        .raw_mir
        .iter()
        .find(|function| function.name == "Drain__recv__run")
        .expect("drain handler must lower");
    let pretransfer_block = raw
        .blocks
        .iter()
        .find(|block| {
            matches!(
                raw.suspend_kinds.get(&block.id),
                Some(SuspendKind::Ask { .. })
            )
        })
        .map(|block| block.id)
        .expect("await tick must suspend before the for-await transfer");
    let posttransfer_block = raw
        .blocks
        .iter()
        .find(|block| {
            matches!(
                raw.suspend_kinds.get(&block.id),
                Some(SuspendKind::StreamNext { .. })
            )
        })
        .map(|block| block.id)
        .expect("for-await must suspend on stream-next after the transfer");

    let elaborated = pl
        .elaborated_mir
        .iter()
        .find(|function| function.name == "Drain__recv__run")
        .expect("drain handler must elaborate");
    let stream_close_places = |block| {
        elaborated
            .drop_plans
            .iter()
            .find_map(|(exit, plan)| match exit {
                hew_mir::ExitPath::Suspend { block: actual, .. } if *actual == block => Some(
                    plan.drops
                        .iter()
                        .filter(|drop| {
                            matches!(
                                drop.drop_fn,
                                Some(hew_mir::DropFnSpec::Runtime(
                                    hew_types::runtime_call::RuntimeDropDescriptor::StreamClose
                                ))
                            )
                        })
                        .map(|drop| drop.place)
                        .collect::<Vec<_>>(),
                ),
                _ => None,
            })
            .expect("suspend-abandon plan must have a drop plan")
    };
    let pretransfer_closes = stream_close_places(pretransfer_block);
    assert_eq!(
        pretransfer_closes.len(),
        1,
        "pre-transfer abandonment closes exactly the original source"
    );
    let source_place = pretransfer_closes[0];
    let posttransfer_closes = stream_close_places(posttransfer_block);
    assert_eq!(
        posttransfer_closes.len(),
        1,
        "post-transfer abandonment closes exactly the cursor"
    );
    assert!(
        !posttransfer_closes.contains(&source_place),
        "after transfer the consumed source local must not receive a second close"
    );
}

/// A guard can return before the later `for await` move. That terminal plan
/// must close the original source once; normal post-handoff cleanup remains
/// owned by the cursor.
#[test]
fn forawait_stream_source_is_closed_on_pretransfer_return() {
    let pl = pipeline_with_tc(
        r#"
        actor Maker {
            receive gen fn items() -> string {
                yield "one";
            }

            receive fn tick() -> i64 {
                1
            }
        }

        actor Drain {
            receive fn run(m: LocalPid<Maker>, stop: bool) {
                let input = m.items();
                if stop {
                    return;
                }
                let _ready = await m.tick();
                for await item in input {
                    println(item);
                }
            }
        }
        "#,
    );
    assert_no_nyi(&pl);
    assert!(
        !pl.diagnostics.iter().any(|diag| matches!(
            diag.kind,
            hew_mir::MirDiagnosticKind::ObligationUnderReleased { ref function, .. }
                | hew_mir::MirDiagnosticKind::ObligationOverReleased { ref function, .. }
                if function == "Drain__recv__run"
        )),
        "pre-transfer return must balance the original stream exactly once: {:#?}",
        pl.diagnostics
    );

    let raw = pl
        .raw_mir
        .iter()
        .find(|function| function.name == "Drain__recv__run")
        .expect("drain handler must lower");
    let ask_block = raw
        .blocks
        .iter()
        .find(|block| {
            matches!(
                raw.suspend_kinds.get(&block.id),
                Some(SuspendKind::Ask { .. })
            )
        })
        .map(|block| block.id)
        .expect("the non-returning path must reach the setup await");
    let elaborated = pl
        .elaborated_mir
        .iter()
        .find(|function| function.name == "Drain__recv__run")
        .expect("drain handler must elaborate");
    let source_place = elaborated
        .drop_plans
        .iter()
        .find_map(|(exit, plan)| match exit {
            hew_mir::ExitPath::Suspend { block, .. } if *block == ask_block => plan
                .drops
                .iter()
                .find(|drop| {
                    matches!(
                        drop.drop_fn,
                        Some(hew_mir::DropFnSpec::Runtime(
                            hew_types::runtime_call::RuntimeDropDescriptor::StreamClose
                        ))
                    )
                })
                .map(|drop| drop.place),
            _ => None,
        })
        .expect("the setup await must close the pre-transfer source on abandonment");
    let early_return_closes: Vec<_> = elaborated
        .drop_plans
        .iter()
        .filter_map(|(exit, plan)| matches!(exit, hew_mir::ExitPath::Return { .. }).then_some(plan))
        .flat_map(|plan| {
            plan.drops.iter().filter(move |drop| {
                drop.place == source_place
                    && matches!(
                        drop.drop_fn,
                        Some(hew_mir::DropFnSpec::Runtime(
                            hew_types::runtime_call::RuntimeDropDescriptor::StreamClose
                        ))
                    )
            })
        })
        .collect();
    assert_eq!(
        early_return_closes.len(),
        1,
        "the sole pre-transfer return plan closes the original source exactly once"
    );
}

/// Multiple hand-offs can be at different stages on the same abandonment
/// edge. Source/cursor admission is proved independently, then closes still
/// follow the shared declaration-LIFO order.
#[test]
#[allow(
    clippy::too_many_lines,
    reason = "the regression keeps its multi-stage ownership and LIFO assertions together"
)]
fn forawait_stream_handoff_mixed_owners_preserve_lifo_order() {
    let pl = pipeline_with_tc(
        r#"
        actor Maker {
            receive gen fn items() -> string {
                yield "one";
            }

            receive fn tick() -> i64 {
                1
            }
        }

        actor Drain {
            receive fn run(m: LocalPid<Maker>) {
                let old = m.items();
                let a = m.items();
                let b = m.items();
                let newest = m.items();
                let _ready = await m.tick();
                for await outer in a {
                    for await inner in b {
                        println(inner);
                        break;
                    }
                    println(outer);
                    break;
                }
            }
        }
        "#,
    );
    assert_no_nyi(&pl);
    assert!(
        !pl.diagnostics.iter().any(|diag| matches!(
            diag.kind,
            hew_mir::MirDiagnosticKind::ObligationUnderReleased { ref function, .. }
                | hew_mir::MirDiagnosticKind::ObligationOverReleased { ref function, .. }
                if function == "Drain__recv__run"
        )),
        "mixed hand-off ownership must remain exactly-once: {:#?}",
        pl.diagnostics
    );

    let raw = pl
        .raw_mir
        .iter()
        .find(|function| function.name == "Drain__recv__run")
        .expect("drain handler must lower");
    let setup_suspend = raw
        .blocks
        .iter()
        .find(|block| {
            matches!(
                raw.suspend_kinds.get(&block.id),
                Some(SuspendKind::Ask { .. })
            )
        })
        .map(|block| block.id)
        .expect("setup await must suspend before either hand-off");
    let stream_suspends: Vec<_> = raw
        .blocks
        .iter()
        .filter_map(|block| match raw.suspend_kinds.get(&block.id) {
            Some(SuspendKind::StreamNext { stream, .. }) => Some((block.id, *stream)),
            _ => None,
        })
        .collect();
    assert!(
        stream_suspends.len() >= 2,
        "nested for-await must lower two stream-next suspensions"
    );
    let (outer_block, outer_cursor) = stream_suspends[0];
    let (inner_block, inner_cursor) = stream_suspends[1];

    let elaborated = pl
        .elaborated_mir
        .iter()
        .find(|function| function.name == "Drain__recv__run")
        .expect("drain handler must elaborate");
    let closes_for_suspend = |block| {
        elaborated
            .drop_plans
            .iter()
            .find_map(|(exit, plan)| match exit {
                hew_mir::ExitPath::Suspend { block: actual, .. } if *actual == block => Some(
                    plan.drops
                        .iter()
                        .filter(|drop| {
                            matches!(
                                drop.drop_fn,
                                Some(hew_mir::DropFnSpec::Runtime(
                                    hew_types::runtime_call::RuntimeDropDescriptor::StreamClose
                                ))
                            )
                        })
                        .map(|drop| drop.place)
                        .collect::<Vec<_>>(),
                ),
                _ => None,
            })
            .expect("suspend-abandon plan must exist")
    };
    let setup_closes = closes_for_suspend(setup_suspend);
    assert_eq!(
        setup_closes.len(),
        4,
        "setup suspension owns both hand-off sources and ordinary neighbors"
    );
    let newest_ordinary = setup_closes[0];
    let source_b = setup_closes[1];
    let source_a = setup_closes[2];
    let old_ordinary = setup_closes[3];
    assert_ne!(source_a, source_b, "the two source handles are distinct");

    let outer_closes = closes_for_suspend(outer_block);
    assert_eq!(
        outer_closes,
        vec![outer_cursor, newest_ordinary, source_b, old_ordinary],
        "cursor and deferred source insert among newer/older ordinary owners in declaration-LIFO order"
    );
    let inner_closes = closes_for_suspend(inner_block);
    assert_eq!(
        inner_closes,
        vec![inner_cursor, outer_cursor, newest_ordinary, old_ordinary],
        "inner next preserves nested cursor and ordinary-owner declaration-LIFO order"
    );
}

/// An early `return` on one body path must not suppress the fall-through
/// body-end release: iterations that do NOT return still free their received
/// string. Pre-fix this was the 1-leak-per-received-value shape — the
/// `Terminator::Return` arm of the body walk answered "unsafe", poisoning the
/// whole binding.
#[test]
fn forawait_early_return_body_keeps_body_end_release() {
    let pl = pipeline_with_tc(
        r#"
        actor Maker {
            receive gen fn items() -> string {
                yield "one";
                yield "two";
            }
        }
        fn main() -> i64 {
            let m = spawn Maker;
            for await v in m.items() {
                if v.len() > 3 {
                    return 1;
                }
            }
            0
        }
        "#,
    );
    assert_no_nyi(&pl);
    let (_, fall_through) = string_drops_by_edge(&pl, "main");
    assert!(
        fall_through >= 1,
        "the fall-through body-end release must survive an early-return path \
         (its absence leaks every non-returning iteration's received string)"
    );
}

/// The returning iteration's received value is freed ON the return edge —
/// the body-end drop sits past the `return` and never runs on that path.
#[test]
fn forawait_early_return_edge_releases_current_iteration() {
    let pl = pipeline_with_tc(
        r#"
        actor Maker {
            receive gen fn items() -> string {
                yield "one";
                yield "two";
            }
        }
        fn main() -> i64 {
            let m = spawn Maker;
            for await v in m.items() {
                if v.len() > 3 {
                    return 1;
                }
            }
            0
        }
        "#,
    );
    assert_no_nyi(&pl);
    assert_eq!(
        string_releases_on_return_exits(&pl, "main"),
        1,
        "the early-return edge must release exactly the current iteration's \
         received string (0 = the returning iteration leaks; >1 = a second \
         holder was dropped on the same edge)"
    );
}

/// `return v` moves the loop variable to the caller: neither the body-end
/// drop nor the return-edge drop may fire (the caller owns the release).
/// Leak-not-double-free posture — over-emitting here frees the value the
/// caller still reads.
#[test]
fn forawait_returned_loop_var_escapes_without_release() {
    let pl = pipeline_with_tc(
        r#"
        actor Maker {
            receive gen fn items() -> string {
                yield "one";
                yield "two";
            }
        }
        fn first(m: LocalPid<Maker>) -> string {
            for await v in m.items() {
                return v;
            }
            ""
        }
        fn main() {
            let m = spawn Maker;
            let s = first(m);
            println(s);
        }
        "#,
    );
    assert_no_nyi(&pl);
    let (return_edge, fall_through) = string_drops_by_edge(&pl, "first");
    assert_eq!(
        (return_edge, fall_through),
        (0, 0),
        "a loop variable moved out by `return v` is owned by the caller; any \
         emitted release double-frees the returned string"
    );
}

/// `return wrap(v)`, where `wrap` is an identity pass-through
/// (`fn wrap(v: string) -> string { return v; }`), forwards the loop
/// variable's buffer through a `Terminator::Call` before it reaches the
/// return slot. Neither the body-end drop nor the return-edge drop may
/// fire: `wrap` is not a verified borrowing callee (it is not on the
/// runtime ownership-contract table's closed borrow list), so its `v`
/// argument must be treated as an escape — exactly like `return v`
/// itself, just one call-hop removed. Before the fix the escape scan
/// blanket-treated every `Terminator::Call` argument as a borrow, so the
/// return-edge ledger fired a release AFTER `wrap` had already threaded
/// the same buffer into its own return value: a use-after-free the
/// caller reads as an emptied string (issue #2412 / #2463).
#[test]
fn forawait_return_forwarded_via_call_escapes_without_release() {
    let pl = pipeline_with_tc(
        r#"
        actor Maker {
            receive gen fn items() -> string {
                yield "one";
                yield "two";
            }
        }
        fn wrap(v: string) -> string {
            return v;
        }
        fn first(m: LocalPid<Maker>) -> string {
            for await v in m.items() {
                return wrap(v);
            }
            ""
        }
        fn main() {
            let m = spawn Maker;
            let s = first(m);
            println(s);
        }
        "#,
    );
    assert_no_nyi(&pl);
    let (return_edge, fall_through) = string_drops_by_edge(&pl, "first");
    assert_eq!(
        (return_edge, fall_through),
        (0, 0),
        "a loop variable forwarded through an identity callee (`wrap`) is not \
         on the verified-borrow list; any emitted release double-frees the \
         buffer `wrap`'s return value shares with it"
    );
}

/// `break`-edge analogue of the identity-forwarding shape. The frame binder
/// carries its own owner generation, and the identity callee retains on
/// return (`fn wrap` lowers to `string.retain` before its return move), so
/// `carry` and the binder hold one count each. The break edge releases the
/// binder's count exactly once; withholding it leaks one count per iteration,
/// and a second release underflows the buffer `carry` still reads.
#[test]
fn forawait_break_forwarded_via_call_releases_the_binder_count_once() {
    let pl = pipeline_with_tc(
        r#"
        actor Maker {
            receive gen fn items() -> string {
                yield "one";
                yield "two";
            }
        }
        fn wrap(v: string) -> string {
            return v;
        }
        fn main() -> i64 {
            let m = spawn Maker;
            var carry = "init";
            for await v in m.items() {
                carry = wrap(v);
                break;
            }
            println(carry);
            0
        }
        "#,
    );
    assert_no_nyi(&pl);
    // Scoping the drop count to `wrap`'s exact argument place keeps this
    // assertion independent of `carry`'s expected overwrite release.
    let v_place_drops = string_drops_for_call_arg(&pl, "main", "wrap", 0);
    assert_eq!(
        v_place_drops, 1,
        "the frame binder forwarded through an identity callee before `break` \
         owns one count of its own; the break edge must release it exactly \
         once (0 leaks a count per iteration, >1 underflows the buffer \
         `carry` reads)"
    );
}

/// NEGATIVE: `ParamsOnly` identifies a parameter-derived result, but does not
/// identify WHICH parameter. With two heap arguments, lowering must not guess
/// that the result carries the active yield binder and suppress its break-edge
/// cleanup.
#[test]
fn forawait_break_ambiguous_forward_keeps_yield_cleanup() {
    let pl = pipeline_with_tc(
        r#"
        actor Maker {
            receive gen fn items() -> string {
                yield "one";
            }
        }
        fn choose(a: string, b: string, first: bool) -> string {
            if first { return a; }
            b
        }
        fn main() -> i64 {
            let m = spawn Maker;
            var carry = "init";
            for await v in m.items() {
                carry = choose(v, "fallback", true);
                break;
            }
            println(carry);
            0
        }
        "#,
    );
    assert_no_nyi(&pl);
    assert_eq!(
        string_drops_for_call_arg(&pl, "main", "choose", 0),
        1,
        "two heap arguments leave the forwarded parameter ambiguous, so the active yield binder must retain its break-edge cleanup",
    );
}

/// The sync-generator consumer (`for x in gen()`) shares the same body walk:
/// an early `return` must not poison its per-iteration release either.
#[test]
fn sync_generator_early_return_keeps_body_end_release() {
    let pl = pipeline_with_tc(
        r#"
        gen fn names() -> string {
            yield "alpha";
            yield "beta";
        }
        fn pick() -> i64 {
            for x in names() {
                if x.len() > 4 {
                    return 1;
                }
            }
            0
        }
        fn main() {
            let n = pick();
            println(f"{n}");
        }
        "#,
    );
    assert_no_nyi(&pl);
    let (return_edge, fall_through) = string_drops_by_edge(&pl, "pick");
    assert!(
        fall_through >= 1,
        "the sync-generator body-end release must survive an early-return path"
    );
    assert_eq!(
        return_edge, 1,
        "the sync-generator early-return edge must release the current \
         iteration's yielded string exactly once"
    );
}

/// A `break` that follows a `return`-carrying path in the same body keeps its
/// break-edge release: the break-edge emitter re-runs the same body walk, so
/// a `Return`-poisoned walk would silently suppress this edge too.
#[test]
fn forawait_break_edge_release_survives_sibling_return_path() {
    let pl = pipeline_with_tc(
        r#"
        actor Maker {
            receive gen fn items() -> string {
                yield "one";
                yield "two";
            }
        }
        fn main() -> i64 {
            let m = spawn Maker;
            for await v in m.items() {
                if v.len() > 30 {
                    return 1;
                }
                if v.len() > 2 {
                    break;
                }
            }
            0
        }
        "#,
    );
    assert_no_nyi(&pl);
    assert_eq!(
        string_releases_on_return_exits(&pl, "main"),
        1,
        "the return edge releases the current iteration's received string"
    );
    // Body-end release + break-edge release both live in non-Return blocks;
    // they are CFG-mutually-exclusive per iteration.
    let (_, fall_through) = string_drops_by_edge(&pl, "main");
    assert!(
        fall_through >= 2,
        "both the body-end release and the break-edge release must survive a \
         sibling return path (got {fall_through})"
    );
}
