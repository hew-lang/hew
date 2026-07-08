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
use hew_mir::{lower_hir_module, Instr, IrPipeline, Terminator};
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

fn assert_no_nyi(pl: &IrPipeline) {
    let nyi: Vec<_> = pl
        .diagnostics
        .iter()
        .filter(|d| matches!(d.kind, hew_mir::MirDiagnosticKind::NotYetImplemented { .. }))
        .collect();
    assert!(nyi.is_empty(), "unexpected NYI diagnostics: {nyi:#?}");
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
    let (return_edge, _) = string_drops_by_edge(&pl, "main");
    assert_eq!(
        return_edge, 1,
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
    let (return_edge, fall_through) = string_drops_by_edge(&pl, "main");
    assert_eq!(
        return_edge, 1,
        "the return edge releases the current iteration's received string"
    );
    // Body-end release + break-edge release both live in non-Return blocks;
    // they are CFG-mutually-exclusive per iteration.
    assert!(
        fall_through >= 2,
        "both the body-end release and the break-edge release must survive a \
         sibling return path (got {fall_through})"
    );
}
