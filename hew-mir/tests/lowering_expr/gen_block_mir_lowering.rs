//! Producer-bridge tests for MIR `Terminator::Yield` lowering.
//!
//! S3a scope: `HirExprKind::GenBlock` and `HirExprKind::Yield` lower to
//! `Terminator::Yield { value, next }` inside a synthetic gen-body function
//! registered in the `IrPipeline`. The enclosing function receives a
//! `Place::Local` typed as `Generator<Y, R>`.
//!
//! These tests are the structural contract that S3b (cross-yield liveness
//! synthesis) and S4 (LLVM state-machine codegen) consume. A regression here
//! means the downstream slices are looking at malformed CFG.
//!
//! LESSONS row `end-to-end-before-layer-thickening` (P0): every cross-stage
//! producer-emit must carry a structural assertion test before the consumer
//! ships. This file is that gate for the generator MIR lane.

use hew_hir::{lower_program, ResolutionCtx};
use hew_mir::{
    terminator_source_places, GeneratorEnvFieldPlan, IrPipeline, MirDiagnosticKind, Place,
    RawMirFunction, Terminator,
};
use hew_types::{module_registry::ModuleRegistry, Checker};

fn lower_checked(source: &str) -> IrPipeline {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    assert!(
        tc_output.errors.is_empty(),
        "type-check errors: {:?}",
        tc_output.errors
    );
    let hir = lower_program(
        &parsed.program,
        &tc_output,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    assert!(
        hir.diagnostics.is_empty(),
        "HIR diagnostics: {:?}",
        hir.diagnostics
    );
    hew_mir::lower_hir_module(&hir.module)
}

fn find_gen_body<'a>(pipeline: &'a IrPipeline, owner: &str) -> &'a RawMirFunction {
    let prefix = format!("__hew_gen_body_{owner}_");
    pipeline
        .raw_mir
        .iter()
        .find(|f| f.name.starts_with(&prefix))
        .unwrap_or_else(|| {
            panic!(
                "expected a gen-body function with prefix `{prefix}`; \
                 available raw MIR functions: {:?}",
                pipeline.raw_mir.iter().map(|f| &f.name).collect::<Vec<_>>()
            )
        })
}

/// For each `Terminator::Yield` in the function, return `(block_id, next_block_id)`.
fn yield_sites(func: &RawMirFunction) -> Vec<(u32, u32)> {
    func.blocks
        .iter()
        .filter_map(|b| {
            if let Terminator::Yield { next, .. } = &b.terminator {
                Some((b.id, *next))
            } else {
                None
            }
        })
        .collect()
}

/// Positive case: a gen block with two yield expressions produces a gen-body
/// function containing exactly two `Terminator::Yield` terminators.
///
/// CFG shape expected:
///   block 0: (stmts) → Yield { value: Local(N), next: 1 }
///   block 1: (stmts) → Yield { value: Local(M), next: 2 }
///   block 2: (stmts) → Return
///
/// This is the structural contract S3b reads when it traverses the CFG to
/// compute cross-yield live sets.
#[test]
fn gen_block_with_two_yields_emits_two_terminator_yield() {
    let pipeline = lower_checked(
        r"
        fn main() {
            let g = gen { yield 1; yield 2; };
        }
        ",
    );

    assert!(
        pipeline.diagnostics.is_empty(),
        "unexpected MIR diagnostics: {:?}",
        pipeline.diagnostics
    );

    let body = find_gen_body(&pipeline, "main");
    let sites = yield_sites(body);
    assert_eq!(
        sites.len(),
        2,
        "expected 2 Terminator::Yield in gen body; got {sites:?}"
    );

    // The resume blocks must be distinct and must exist in the block list.
    let block_ids: Vec<u32> = body.blocks.iter().map(|b| b.id).collect();
    for (yield_block, resume_block) in &sites {
        assert!(
            block_ids.contains(resume_block),
            "resume block {resume_block} from yield at block {yield_block} \
             is not in the gen-body's block list: {block_ids:?}"
        );
    }

    // The two resume blocks must be different.
    assert_ne!(
        sites[0].1, sites[1].1,
        "both yields resume to the same block — \
         resume-block allocation must be monotone"
    );

    // The last block must be Return (generator body ends when exhausted).
    let last_terminator = &body
        .blocks
        .iter()
        .max_by_key(|b| b.id)
        .expect("gen body must have at least one block")
        .terminator;
    assert!(
        matches!(last_terminator, Terminator::Return),
        "gen-body's last block must terminate with Return; got {last_terminator:?}"
    );
}

/// Regression: an actor `receive gen fn` lowers its `yield`s through the
/// same `GenBlock` → `Terminator::Yield` state-machine path a standalone
/// `gen fn` uses, rather than bailing out of MIR lowering. The handler's
/// gen-body function (`__hew_gen_body_<mangled_handler>_<id>`) must be present
/// with one `Terminator::Yield` per source `yield`, and MIR lowering must emit
/// no diagnostics. Before the fix, `lower_actor_receive_handlers` pushed an
/// `UnsupportedNode` diagnostic ("generator MIR lowering is a separate lane")
/// and skipped the handler entirely.
#[test]
fn actor_receive_gen_fn_emits_gen_body_with_terminator_yield() {
    let pipeline = lower_checked(
        r"
        actor Seq {
            init() {}
            receive gen fn count_up() -> i64 { yield 1; yield 2; }
        }
        fn main() { let _s = spawn Seq(); }
        ",
    );

    assert!(
        pipeline.diagnostics.is_empty(),
        "unexpected MIR diagnostics: {:?}",
        pipeline.diagnostics
    );

    // The handler's emit symbol is `Seq__recv__count_up`; the gen-body owner is
    // that mangled name, so the gen-body fn is `__hew_gen_body_Seq__recv__count_up_<id>`.
    let body = find_gen_body(&pipeline, "Seq__recv__count_up");
    let sites = yield_sites(body);
    assert_eq!(
        sites.len(),
        2,
        "expected 2 Terminator::Yield in the actor generator body; got {sites:?}"
    );

    // The handler function itself must exist and materialise the generator via
    // `Terminator::MakeGenerator` (the thin shell that returns the handle).
    let handler = pipeline
        .raw_mir
        .iter()
        .find(|f| f.name == "Seq__recv__count_up")
        .unwrap_or_else(|| {
            panic!(
                "expected actor generator handler `Seq__recv__count_up`; available: {:?}",
                pipeline.raw_mir.iter().map(|f| &f.name).collect::<Vec<_>>()
            )
        });
    assert!(
        handler
            .blocks
            .iter()
            .any(|b| matches!(b.terminator, Terminator::MakeGenerator { .. })),
        "actor generator handler must emit Terminator::MakeGenerator to build the generator handle"
    );
}

/// Structural assertion: each `Terminator::Yield` carries a `value` Place
/// that is a `Place::Local(N)` bound to the lowered yield value.
/// This test also confirms that the gen-body function is registered in the
/// `IrPipeline.raw_mir` list (i.e. `generated_functions` plumbing works).
#[test]
fn terminator_yield_emitted_at_yield_site_with_local_value() {
    let pipeline = lower_checked(
        r"
        fn main() {
            let g = gen { yield 42; };
        }
        ",
    );

    assert!(
        pipeline.diagnostics.is_empty(),
        "unexpected MIR diagnostics: {:?}",
        pipeline.diagnostics
    );

    let body = find_gen_body(&pipeline, "main");

    // Exactly one Yield terminator.
    let sites = yield_sites(body);
    assert_eq!(
        sites.len(),
        1,
        "expected 1 Terminator::Yield; got {sites:?}"
    );

    // The yield value must be a Place::Local (not ReturnSlot or a handle kind).
    let yield_block_id = sites[0].0;
    let yield_block = body
        .blocks
        .iter()
        .find(|b| b.id == yield_block_id)
        .expect("yield block must exist");
    let Terminator::Yield { value, next } = &yield_block.terminator else {
        panic!("expected Terminator::Yield");
    };
    assert!(
        matches!(value, hew_mir::Place::Local(_)),
        "Yield value must be a Place::Local; got {value:?}"
    );

    // The resume block must exist.
    let block_ids: Vec<u32> = body.blocks.iter().map(|b| b.id).collect();
    assert!(
        block_ids.contains(next),
        "resume block {next} is not in the block list: {block_ids:?}"
    );

    // Gen body is registered alongside `main` in the raw_mir list.
    assert!(
        pipeline.raw_mir.len() >= 2,
        "expected at least main + gen-body in raw_mir; got {} functions: {:?}",
        pipeline.raw_mir.len(),
        pipeline.raw_mir.iter().map(|f| &f.name).collect::<Vec<_>>()
    );
}

/// The gen-body function is registered in `IrPipeline.raw_mir` alongside
/// the enclosing function. This test exercises the `generated_functions`
/// plumbing that surfaces nested functions through the pipeline.
///
/// Note on the `generator_borrow_across_yield_detected` test from plan §6:
/// that validation candidate requires a live borrow at a yield site. The
/// v0.5 spine has no MIR borrow-op construction surface (see `lower.rs`
/// line ~326 comment: "Aliasing, `GeneratorBorrowAcrossYield`… have no
/// construction surface"). `MirCheck::GeneratorBorrowAcrossYield` is
/// declared in model.rs:1993 but no pass constructs it today. That test
/// will land alongside the borrow-op surface in a later slice; this note
/// documents the gap so S3a is not mis-counted as missing a required gate.
#[test]
fn gen_body_function_registered_in_raw_mir_pipeline() {
    let parsed = hew_parser::parse(
        r"
        fn main() {
            let g = gen { yield 1; };
        }
        ",
    );
    // Lower via the typecheck-free path to confirm the plumbing is
    // independent of whether the checker ran.
    let hir = hew_hir::lower_program(
        &parsed.program,
        &hew_types::TypeCheckOutput::default(),
        &hew_hir::ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    let pipeline = hew_mir::lower_hir_module(&hir.module);
    let gen_body = pipeline
        .raw_mir
        .iter()
        .find(|f| f.name.contains("__hew_gen_body_"));
    assert!(
        gen_body.is_some(),
        "gen-body function must be registered in raw_mir; found: {:?}",
        pipeline.raw_mir.iter().map(|f| &f.name).collect::<Vec<_>>()
    );
}

/// CFG completeness: a gen block with multiple yields produces blocks in
/// a connected chain. Every resume block must be reachable from the entry
/// block by following terminator edges.
#[test]
fn gen_block_body_cfg_is_connected() {
    let pipeline = lower_checked(
        r"
        fn main() {
            let g = gen { yield 1; yield 2; yield 3; };
        }
        ",
    );

    assert!(
        pipeline.diagnostics.is_empty(),
        "unexpected MIR diagnostics: {:?}",
        pipeline.diagnostics
    );

    let body = find_gen_body(&pipeline, "main");
    let sites = yield_sites(body);
    assert_eq!(sites.len(), 3, "expected 3 yields");

    // Verify that the resume block for each yield is the source block of
    // the following yield (i.e. the chain is: 0→yield→1→yield→2→yield→3→return).
    // Block ids are monotone by construction so we can sort by yield_block_id.
    let mut sorted = sites.clone();
    sorted.sort_by_key(|(bid, _)| *bid);

    for window in sorted.windows(2) {
        let (_, resume_of_first) = window[0];
        let (source_of_second, _) = window[1];
        assert_eq!(
            resume_of_first, source_of_second,
            "resume block of first yield ({resume_of_first}) must be the \
             source block of the second yield ({source_of_second})"
        );
    }
}

/// Security gate (UAF / FFI-ownership): a `gen { }` block that captures an
/// `#[opaque]` runtime handle as a free variable must FAIL CLOSED at MIR
/// lowering, not shallow-copy the handle into the generator's env.
///
/// An `#[opaque]`-only type classifies as `ValueClass::BitCopy` (pointer-width,
/// no implicit drop), so the capture gate's `BitCopy` admission check ALONE
/// would admit it. But the generator's env is a flat `memcpy` of the capture
/// record into the coro companion's heap env block (freed with the companion
/// at generator teardown): shallow-copying an opaque handle aliases the
/// caller's handle, a use-after-free / double-free at generator teardown. The
/// gate additionally rejects any value transitively containing an opaque
/// handle, surfacing a `NotYetImplemented` diagnostic instead of
/// materialising an env field.
#[test]
fn gen_block_capturing_opaque_handle_fails_closed() {
    let pipeline = lower_checked(
        r#"
        #[opaque]
        type Dq {}

        extern "C" {
            fn hew_deque_new() -> Dq;
            fn hew_deque_len(dq: Dq) -> i64;
        }

        fn main() {
            let dq = unsafe { hew_deque_new() };
            let g = gen { yield unsafe { hew_deque_len(dq) }; };
        }
        "#,
    );

    let opaque_capture_rejected = pipeline.diagnostics.iter().any(|d| {
        matches!(
            &d.kind,
            MirDiagnosticKind::NotYetImplemented { construct, .. }
                if construct.contains("opaque/owned value")
        )
    });
    assert!(
        opaque_capture_rejected,
        "a generator capture of an `#[opaque]` handle must fail closed with a \
         NotYetImplemented diagnostic (shallow-copying it into the generator's \
         env is a use-after-free); got: {:#?}",
        pipeline.diagnostics
    );
}

/// The single-diagnostic guarantee for an inadmissible generator capture: the
/// root `NotYetImplemented` is the ONLY diagnostic. The synthetic body still
/// names the rejected capture as a free variable, but `lower_gen_block` poisons
/// its binding id, so the body sub-builder suppresses the two cascade
/// secondaries it would otherwise stack on the root — a `MirStatement::Use` of
/// an un-`Bind`-ed binding (→ dataflow `InitialisedBeforeUse`) and an
/// `UnresolvedPlace` (no backend slot). Only the actionable rejection surfaces.
#[test]
fn gen_block_inadmissible_capture_emits_single_diagnostic() {
    let pipeline = lower_checked(
        r#"
        #[opaque]
        type Dq {}

        extern "C" {
            fn hew_deque_new() -> Dq;
            fn hew_deque_len(dq: Dq) -> i64;
        }

        fn main() {
            let dq = unsafe { hew_deque_new() };
            let g = gen { yield unsafe { hew_deque_len(dq) }; };
        }
        "#,
    );

    assert_eq!(
        pipeline.diagnostics.len(),
        1,
        "an inadmissible gen capture must emit exactly one diagnostic (the root \
         NotYetImplemented); the InitialisedBeforeUse/UnresolvedPlace cascade must \
         be suppressed; got: {:#?}",
        pipeline.diagnostics
    );
    assert!(
        matches!(
            &pipeline.diagnostics[0].kind,
            MirDiagnosticKind::NotYetImplemented { construct, .. }
                if construct.contains("opaque/owned value")
        ),
        "the single diagnostic must be the root NotYetImplemented capture \
         rejection; got: {:#?}",
        pipeline.diagnostics[0]
    );
    // Defence in depth: neither cascade secondary may appear anywhere in the
    // pipeline diagnostics, even if the count assertion above is later relaxed.
    let cascade = pipeline.diagnostics.iter().find(|d| {
        matches!(
            &d.kind,
            MirDiagnosticKind::InitialisedBeforeUse { .. }
                | MirDiagnosticKind::UnresolvedPlace { .. }
        )
    });
    assert!(
        cascade.is_none(),
        "no InitialisedBeforeUse/UnresolvedPlace cascade may follow the root \
         capture rejection; got: {cascade:#?}",
    );
}

/// Regression companion to the opaque-capture gate: a plain `BitCopy` scalar
/// capture (`i64`) must STILL be admitted — the tightened predicate
/// (`gen_env_capture_admissible`) must not over-reject the supported path. No
/// `NotYetImplemented` capture diagnostic, and a gen body is produced.
#[test]
fn gen_block_capturing_scalar_is_admitted() {
    let pipeline = lower_checked(
        r"
        fn main() {
            let base = 10;
            let g = gen { yield base; };
        }
        ",
    );

    let capture_rejected = pipeline.diagnostics.iter().any(|d| {
        matches!(
            &d.kind,
            MirDiagnosticKind::NotYetImplemented { construct, .. }
                if construct.contains("into a generator")
        )
    });
    assert!(
        !capture_rejected,
        "a BitCopy scalar generator capture must be admitted, not fail closed; \
         got: {:#?}",
        pipeline.diagnostics
    );
    // The gen body must still be produced (capture admitted, env materialised).
    let _body = find_gen_body(&pipeline, "main");
    let main = pipeline
        .raw_mir
        .iter()
        .find(|function| function.name == "main")
        .expect("main MIR must exist");
    let make_term = main
        .blocks
        .iter()
        .find_map(|block| match &block.terminator {
            term @ Terminator::MakeGenerator { .. } => Some(term),
            _ => None,
        })
        .expect("main must construct the generator");
    let Terminator::MakeGenerator { env: Some(env), .. } = make_term else {
        panic!("scalar capture must carry a typed generator env plan");
    };
    assert!(matches!(env.place, Place::Local(_)));
    assert!(
        matches!(&env.ty, hew_types::ResolvedTy::Named { name, .. } if name.starts_with("__hew_gen_env_main_"))
    );
    assert_eq!(env.fields, vec![GeneratorEnvFieldPlan::TrivialCopy]);
    assert_eq!(terminator_source_places(make_term, None), vec![env.place]);
}

/// A `gen fn` whose parameter is a bare named-function reference (`fn(i64)->i64`,
/// `ResolvedTy::Function`) must be admitted into the generator env.
///
/// A `Function` type's runtime representation is a two-word `{code_ptr, env_ptr}`
/// fat pointer where `env_ptr` is null by construction (no captures). Both words
/// are non-owning addresses; flat-copying them into the generator's captured env
/// is safe (no heap ownership to alias). Before this fix the gate returned `false`
/// for `PersistentShare` values, emitting `NotYetImplemented` for the fn param and
/// cascading `InitialisedBeforeUse`/`UnresolvedPlace` onto every sibling scalar
/// param via the `all_materialisable = false` all-or-nothing gate.
#[test]
fn gen_block_capturing_fn_typed_param_is_admitted() {
    let pipeline = lower_checked(
        r"
        gen fn mapped(f: fn(i64) -> i64) -> i64 { yield f(1); }
        fn dbl(x: i64) -> i64 { x * 2 }
        fn main() { for v in mapped(dbl) { println(v); } }
        ",
    );
    let rejected = pipeline.diagnostics.iter().any(|d| {
        matches!(
            &d.kind,
            MirDiagnosticKind::NotYetImplemented { construct, .. }
                if construct.contains("into a generator")
        )
    });
    assert!(
        !rejected,
        "a capture-free fn-typed (null-env) generator capture must be admitted; \
         got: {:#?}",
        pipeline.diagnostics
    );
    // The gen body must be produced — env materialised, body lowered.
    let _body = find_gen_body(&pipeline, "mapped");
}

/// CAP-11 P0: a plain function may not forward an unproven fn-typed parameter
/// into a generator constructor. The caller can supply a capturing closure, so
/// the parameter's env word is not provably null at the crossing.
#[test]
fn gen_fn_forwarded_fn_parameter_fails_closed() {
    let pipeline = lower_checked(
        r"
        gen fn mapped(f: fn(i64) -> i64) -> i64 { yield f(1); }
        fn forward(f: fn(i64) -> i64) -> Generator<i64, ()> { mapped(f) }
        fn dbl(x: i64) -> i64 { x * 2 }
        fn main() { for v in forward(dbl) { println(v); } }
        ",
    );

    let rejected = pipeline.diagnostics.iter().any(|d| {
        matches!(
            &d.kind,
            MirDiagnosticKind::NotYetImplemented { construct, .. }
                if construct.contains("capturing closure as a generator")
        )
    });
    assert!(
        rejected,
        "a forwarded fn parameter must fail closed because its env provenance is \
         not provably null; got: {:#?}",
        pipeline.diagnostics
    );
}

/// CAP-11 P0: a fn-valued call result is also unproven. This is the direct
/// laundering shape: the capturing closure is hidden behind `identity(...)`
/// before it reaches the generator constructor.
#[test]
fn gen_fn_fn_valued_call_result_fails_closed() {
    let pipeline = lower_checked(
        r#"
        gen fn mapped(f: fn(i64) -> i64) -> i64 { yield f(1); }
        fn identity(f: fn(i64) -> i64) -> fn(i64) -> i64 { f }
        fn main() {
            let suffix = "owned";
            for v in mapped(identity(|x: i64| x + suffix.len())) { println(v); }
        }
        "#,
    );

    let rejected = pipeline.diagnostics.iter().any(|d| {
        matches!(
            &d.kind,
            MirDiagnosticKind::NotYetImplemented { construct, .. }
                if construct.contains("capturing closure as a generator")
        )
    });
    assert!(
        rejected,
        "a fn-valued call result must fail closed because it may hide a capturing \
         closure env; got: {:#?}",
        pipeline.diagnostics
    );
}

/// The call-result provenance must survive a local binding. Otherwise
/// `let f = producer(); mapped(f)` would erase the producer shape before the
/// generator gate examines the argument.
#[test]
fn gen_fn_bound_fn_valued_call_result_fails_closed() {
    let pipeline = lower_checked(
        r#"
        gen fn mapped(f: fn(i64) -> i64) -> i64 { yield f(1); }
        fn identity(f: fn(i64) -> i64) -> fn(i64) -> i64 { f }
        fn main() {
            let suffix = "owned";
            let f = identity(|x: i64| x + suffix.len());
            for v in mapped(f) { println(v); }
        }
        "#,
    );

    let rejected = pipeline.diagnostics.iter().any(|d| {
        matches!(
            &d.kind,
            MirDiagnosticKind::NotYetImplemented { construct, .. }
                if construct.contains("capturing closure as a generator")
        )
    });
    assert!(
        rejected,
        "a bound fn-valued call result must retain its unproven env provenance; \
         got: {:#?}",
        pipeline.diagnostics
    );
}

/// Owning aggregates can safely hold capturing closures because their own drop
/// machinery releases the env. Reading that fn value into a generator is not
/// safe: the generator flat-copies the pair but cannot release the inner env.
#[test]
fn gen_fn_fn_valued_aggregate_read_fails_closed() {
    let pipeline = lower_checked(
        r"
        type Handler { action: fn(i64) -> i64; }
        gen fn mapped(f: fn(i64) -> i64) -> i64 { yield f(1); }
        fn make_adder(n: i64) -> fn(i64) -> i64 { |x: i64| x + n }
        fn main() {
            let h = Handler { action: make_adder(2) };
            for v in mapped(h.action) { println(v); }
        }
        ",
    );

    let rejected = pipeline.diagnostics.iter().any(|d| {
        matches!(
            &d.kind,
            MirDiagnosticKind::NotYetImplemented { construct, .. }
                if construct.contains("capturing closure as a generator")
        )
    });
    assert!(
        rejected,
        "a fn-valued aggregate read must fail closed because the field may own a \
         capturing closure env; got: {:#?}",
        pipeline.diagnostics
    );
}

/// Anonymous `gen {}` construction bypasses `lower_direct_call`, so its env
/// materialiser must independently reject a local fn-valued call result whose
/// closure env is not provably null.
#[test]
fn gen_block_fn_valued_call_result_capture_fails_closed() {
    let pipeline = lower_checked(
        r"
        fn make_adder(n: i64) -> fn(i64) -> i64 { |x: i64| x + n }
        fn main() {
            let f = make_adder(2);
            let g = gen { yield f(1); };
        }
        ",
    );

    let rejected = pipeline.diagnostics.iter().any(|d| {
        matches!(
            &d.kind,
            MirDiagnosticKind::NotYetImplemented { construct, .. }
                if construct.contains("unproven env provenance")
        )
    });
    assert!(
        rejected,
        "an anonymous gen block must reject an unproven fn-valued capture; got: {:#?}",
        pipeline.diagnostics
    );
}

/// Negative control for the anonymous-gen capture-site gate: a local binding
/// sourced from a named function has a provably-null env word.
#[test]
fn gen_block_named_fn_capture_is_admitted() {
    let pipeline = lower_checked(
        r"
        fn dbl(x: i64) -> i64 { x * 2 }
        fn main() {
            let f = dbl;
            let g = gen { yield f(1); };
        }
        ",
    );

    let rejected = pipeline.diagnostics.iter().any(|d| {
        matches!(
            &d.kind,
            MirDiagnosticKind::NotYetImplemented { construct, .. }
                if construct.contains("into a generator")
        )
    });
    assert!(
        !rejected,
        "an anonymous gen block must admit a named-function capture; got: {:#?}",
        pipeline.diagnostics
    );
}

/// Provenance collection must reach a fixed point. The outer binding is
/// examined before the nested block's `g = make_adder(...)` producer is
/// discovered on the first pass; a second pass must propagate the taint to
/// `f` before `mapped(f)` lowers.
#[test]
fn gen_fn_nested_binding_provenance_reaches_fixed_point() {
    let pipeline = lower_checked(
        r"
        gen fn mapped(f: fn(i64) -> i64) -> i64 { yield f(1); }
        fn make_adder(n: i64) -> fn(i64) -> i64 { |x: i64| x + n }
        fn main() {
            let f = {
                let g = make_adder(2);
                g
            };
            for v in mapped(f) { println(v); }
        }
        ",
    );

    let rejected = pipeline.diagnostics.iter().any(|d| {
        matches!(
            &d.kind,
            MirDiagnosticKind::NotYetImplemented { construct, .. }
                if construct.contains("capturing closure as a generator")
        )
    });
    assert!(
        rejected,
        "nested producer provenance must propagate before generator calls lower; \
         got: {:#?}",
        pipeline.diagnostics
    );
}

/// The same fixed point must cover a loop back-edge: on the second iteration
/// `f = g` receives the capturing closure produced at the end of the first.
#[test]
fn gen_fn_loop_backedge_provenance_reaches_fixed_point() {
    let pipeline = lower_checked(
        r"
        gen fn mapped(f: fn(i64) -> i64) -> i64 { yield f(1); }
        fn dbl(x: i64) -> i64 { x * 2 }
        fn make_adder(n: i64) -> fn(i64) -> i64 { |x: i64| x + n }
        fn main() {
            var f = dbl;
            var g = dbl;
            var i = 0;
            while i < 2 {
                f = g;
                g = make_adder(i);
                i = i + 1;
            }
            for v in mapped(f) { println(v); }
        }
        ",
    );

    let rejected = pipeline.diagnostics.iter().any(|d| {
        matches!(
            &d.kind,
            MirDiagnosticKind::NotYetImplemented { construct, .. }
                if construct.contains("capturing closure as a generator")
        )
    });
    assert!(
        rejected,
        "loop back-edge provenance must propagate before generator calls lower; \
         got: {:#?}",
        pipeline.diagnostics
    );
}

/// Generator bodies use a fresh child builder, so they need the same fixed-point
/// pre-pass as top-level functions before lowering nested generator calls.
#[test]
fn nested_generator_body_provenance_reaches_fixed_point() {
    let pipeline = lower_checked(
        r"
        gen fn mapped(f: fn(i64) -> i64) -> i64 { yield f(1); }
        fn make_adder(n: i64) -> fn(i64) -> i64 { |x: i64| x + n }
        fn main() {
            let outer = gen {
                let f = {
                    let g = make_adder(2);
                    g
                };
                for v in mapped(f) { yield v; }
            };
        }
        ",
    );

    let rejected = pipeline.diagnostics.iter().any(|d| {
        matches!(
            &d.kind,
            MirDiagnosticKind::NotYetImplemented { construct, .. }
                if construct.contains("capturing closure as a generator")
        )
    });
    assert!(
        rejected,
        "generator body provenance must reach a fixed point before nested generator \
         calls lower; got: {:#?}",
        pipeline.diagnostics
    );
}

/// Pattern payloads come from owning enum storage and may contain capturing
/// closures. The payload binder must be marked unproven before an anonymous
/// generator captures it.
#[test]
fn gen_block_fn_valued_pattern_payload_fails_closed() {
    let pipeline = lower_checked(
        r"
        fn make_opt(n: i64) -> Option<fn(i64) -> i64> {
            Some(|x: i64| x + n)
        }
        fn main() {
            if let Some(f) = make_opt(2) {
                let g = gen { yield f(1); };
            }
        }
        ",
    );

    let rejected = pipeline.diagnostics.iter().any(|d| {
        matches!(
            &d.kind,
            MirDiagnosticKind::NotYetImplemented { construct, .. }
                if construct.contains("unproven env provenance")
        )
    });
    assert!(
        rejected,
        "a fn-valued pattern payload must not enter an anonymous generator env; \
         got: {:#?}",
        pipeline.diagnostics
    );
}

/// `receive gen fn` calls use the actor-stream lowering path rather than
/// `lower_direct_call`; that boundary must reject the same fn-valued call
/// results before the handler shell copies its parameter into the generator env.
#[test]
fn actor_gen_stream_fn_valued_call_result_fails_closed() {
    let pipeline = lower_checked(
        r"
        actor Mapper {
            init() {}
            receive gen fn mapped(f: fn(i64) -> i64) -> i64 { yield f(1); }
        }
        fn make_adder(n: i64) -> fn(i64) -> i64 { |x: i64| x + n }
        fn main() {
            let mapper = spawn Mapper();
            let stream = mapper.mapped(make_adder(2));
        }
        ",
    );

    let rejected = pipeline.diagnostics.iter().any(|d| {
        matches!(
            &d.kind,
            MirDiagnosticKind::NotYetImplemented { construct, .. }
                if construct.contains("capturing closure as a generator")
        )
    });
    assert!(
        rejected,
        "an actor generator stream call must reject an unproven fn-valued argument; \
         got: {:#?}",
        pipeline.diagnostics
    );
}

/// Negative control for the actor-stream gate: a named function has a null env
/// word and remains a supported direct argument.
#[test]
fn actor_gen_stream_named_fn_is_admitted() {
    let pipeline = lower_checked(
        r"
        actor Mapper {
            init() {}
            receive gen fn mapped(f: fn(i64) -> i64) -> i64 { yield f(1); }
        }
        fn dbl(x: i64) -> i64 { x * 2 }
        fn main() {
            let mapper = spawn Mapper();
            let stream = mapper.mapped(dbl);
        }
        ",
    );

    let rejected = pipeline.diagnostics.iter().any(|d| {
        matches!(
            &d.kind,
            MirDiagnosticKind::NotYetImplemented { construct, .. }
                if construct.contains("capturing closure as a generator")
        )
    });
    assert!(
        !rejected,
        "an actor generator stream must admit a direct named function; got: {:#?}",
        pipeline.diagnostics
    );
}

/// Closure invoke shims use a child builder. Their fn-typed parameters must be
/// seeded as unproven before a closure body can forward one into a generator.
#[test]
fn closure_body_forwarded_fn_parameter_fails_closed() {
    let pipeline = lower_checked(
        r"
        gen fn mapped(f: fn(i64) -> i64) -> i64 { yield f(1); }
        fn main() {
            let forward = |f: fn(i64) -> i64| mapped(f);
            let base = 2;
            let g = forward(|x: i64| x + base);
        }
        ",
    );

    let rejected = pipeline.diagnostics.iter().any(|d| {
        matches!(
            &d.kind,
            MirDiagnosticKind::NotYetImplemented { construct, .. }
                if construct.contains("capturing closure as a generator")
        )
    });
    assert!(
        rejected,
        "a closure body must reject a forwarded fn parameter with unproven env \
         provenance; got: {:#?}",
        pipeline.diagnostics
    );
}

/// The child closure builder must also inherit provenance for fn-valued
/// captures from its enclosing function.
#[test]
fn closure_body_captured_fn_call_result_fails_closed() {
    let pipeline = lower_checked(
        r"
        gen fn mapped(f: fn(i64) -> i64) -> i64 { yield f(1); }
        fn make_adder(n: i64) -> fn(i64) -> i64 { |x: i64| x + n }
        fn main() {
            let f = make_adder(2);
            let forward = || mapped(f);
            let g = forward();
        }
        ",
    );

    let rejected = pipeline.diagnostics.iter().any(|d| {
        matches!(
            &d.kind,
            MirDiagnosticKind::NotYetImplemented { construct, .. }
                if construct.contains("capturing closure as a generator")
        )
    });
    assert!(
        rejected,
        "a closure body must retain an enclosing fn capture's unproven env \
         provenance; got: {:#?}",
        pipeline.diagnostics
    );
}

/// Lambda-actor bodies are another child-builder path with user parameters.
/// A fn-typed message parameter may carry a capturing closure and cannot be
/// forwarded into a generator env.
#[test]
fn lambda_actor_body_forwarded_fn_parameter_fails_closed() {
    let pipeline = lower_checked(
        r"
        gen fn mapped(f: fn(i64) -> i64) -> i64 { yield f(1); }
        fn main() {
            let worker = actor |f: fn(i64) -> i64| {
                let g = mapped(f);
            };
            let base = 2;
            let _sent = worker.send(|x: i64| x + base);
        }
        ",
    );

    let rejected = pipeline.diagnostics.iter().any(|d| {
        matches!(
            &d.kind,
            MirDiagnosticKind::NotYetImplemented { construct, .. }
                if construct.contains("capturing closure as a generator")
        )
    });
    assert!(
        rejected,
        "a lambda-actor body must reject a forwarded fn message parameter; got: {:#?}",
        pipeline.diagnostics
    );
}

/// A named generator function can itself be used as a first-class fn value.
/// Calls through that binding lower via `CallClosure`, so the indirect path must
/// enforce the same fn-argument provenance gate as `lower_direct_call`.
#[test]
fn indirect_gen_fn_call_result_fails_closed() {
    let pipeline = lower_checked(
        r"
        gen fn mapped(f: fn(i64) -> i64) -> i64 { yield f(1); }
        fn main() {
            let make = mapped;
            let base = 2;
            let g = make(|x: i64| x + base);
        }
        ",
    );

    let rejected = pipeline.diagnostics.iter().any(|d| {
        matches!(
            &d.kind,
            MirDiagnosticKind::NotYetImplemented { construct, .. }
                if construct.contains("capturing closure as a generator")
        )
    });
    assert!(
        rejected,
        "an indirect generator call must reject a capturing closure argument; got: {:#?}",
        pipeline.diagnostics
    );
}

/// Negative control for the indirect-call gate: named functions and
/// capture-free closures still carry null env words.
#[test]
fn indirect_gen_fn_null_env_args_are_admitted() {
    let pipeline = lower_checked(
        r"
        gen fn mapped(f: fn(i64) -> i64) -> i64 { yield f(1); }
        fn dbl(x: i64) -> i64 { x * 2 }
        fn main() {
            let make = mapped;
            let g1 = make(dbl);
            let g2 = make(|x: i64| x + 1);
        }
        ",
    );

    let rejected = pipeline.diagnostics.iter().any(|d| {
        matches!(
            &d.kind,
            MirDiagnosticKind::NotYetImplemented { construct, .. }
                if construct.contains("capturing closure as a generator")
        )
    });
    assert!(
        !rejected,
        "an indirect generator call must admit null-env function arguments; got: {:#?}",
        pipeline.diagnostics
    );
}

/// A whole-scrutinee match binding (`f => ...`) is another pattern-introduced
/// fn value and must be treated as unproven.
#[test]
fn match_binding_fn_provenance_fails_closed() {
    let pipeline = lower_checked(
        r"
        gen fn mapped(f: fn(i64) -> i64) -> i64 { yield f(1); }
        fn make_adder(n: i64) -> fn(i64) -> i64 { |x: i64| x + n }
        fn main() {
            match make_adder(2) {
                f => { let g = mapped(f); },
            }
        }
        ",
    );

    let rejected = pipeline.diagnostics.iter().any(|d| {
        matches!(
            &d.kind,
            MirDiagnosticKind::NotYetImplemented { construct, .. }
                if construct.contains("capturing closure as a generator")
        )
    });
    assert!(
        rejected,
        "a fn-valued match binding must carry unproven env provenance; got: {:#?}",
        pipeline.diagnostics
    );
}

/// Negative control for the widened gate: direct generator calls with a named
/// function or capture-free closure have provably-null env words and remain
/// supported.
#[test]
fn gen_fn_direct_null_env_function_values_are_admitted() {
    let pipeline = lower_checked(
        r"
        gen fn mapped(f: fn(i64) -> i64) -> i64 { yield f(1); }
        fn dbl(x: i64) -> i64 { x * 2 }
        fn main() {
            for v in mapped(dbl) { println(v); }
            for v in mapped(|x: i64| x + 1) { println(v); }
        }
        ",
    );

    let rejected = pipeline.diagnostics.iter().any(|d| {
        matches!(
            &d.kind,
            MirDiagnosticKind::NotYetImplemented { construct, .. }
                if construct.contains("capturing closure as a generator")
        )
    });
    assert!(
        !rejected,
        "direct named functions and capture-free closures must remain admitted; \
         got: {:#?}",
        pipeline.diagnostics
    );
}

/// A generator that captures a closure-with-env must STILL fail closed after the
/// fn-typed-param admission gate is widened.
///
/// A closure literal that captures outer bindings carries a heap-boxed env pointer.
/// Flat-copying the two-word `{code_ptr, env_ptr}` into the generator's captured
/// env shallow-aliases the caller's heap env → double-free / UAF at generator
/// teardown. This test is the boundary guard that proves the lane did NOT over-widen
/// the gate to all `PersistentShare` values.
///
/// Note: the closure literal `|x: i64| -> i64 { x + base }` is expected to type as
/// `ResolvedTy::Closure { captures: [i64], .. }` with a non-empty capture list at
/// the MIR env-build site. If a future optimisation elides the env (empty captures
/// in a trivially-inlineable closure) adjust the fixture to use a runtime-unknown
/// captured binding.
#[test]
fn gen_block_capturing_closure_with_env_fails_closed() {
    let pipeline = lower_checked(
        r"
        fn main() {
            let base = 10;
            let f = |x: i64| -> i64 { x + base };
            let g = gen { yield f(1); };
        }
        ",
    );
    let rejected = pipeline.diagnostics.iter().any(|d| {
        matches!(
            &d.kind,
            MirDiagnosticKind::NotYetImplemented { construct, .. }
                if construct.contains("into a generator")
        )
    });
    assert!(
        rejected,
        "a closure carrying a heap env must STILL fail closed (its env would be \
         shallow-aliased); got: {:#?}",
        pipeline.diagnostics
    );
}
