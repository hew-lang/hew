//! A metamorphic release-count sweep over binder shapes and value provenance.
//!
//! # Why this exists
//!
//! Five separate regressions in this area had the same shape: the ownership
//! walk asked "is this value a fresh `+1` owner?" at a site where one input to
//! the answer was not reachable, defaulted fail-closed, and the consumer
//! withheld a release that was legitimate. Every one of them was found by a
//! hand-written leak oracle for one concrete program, and every one of them was
//! invisible to the other oracles — because an oracle pins ONE point of the
//! space, and the gaps are cells of a product.
//!
//! Each single-program oracle asserts an ABSOLUTE fact ("this program leaks
//! nothing"), which is only checkable by running it under `leaks(1)`. That is
//! expensive, macOS-only, and — because it is expensive — sparse. This sweep
//! asserts a RELATIVE fact instead:
//!
//! > **Perturbing a program along an axis that does not change what it owns
//! > must not change how many releases it runs.**
//!
//! A relative fact needs no allocator, so it can be checked densely, in-process,
//! over a product of axes rather than at isolated points. It cannot tell you
//! that the absolute count is right — the oracles do that — but it can tell you
//! that the count does not depend on something it must not depend on, which is
//! exactly the failure mode all five regressions had.
//!
//! # The axes
//!
//! Every axis is **ownership-neutral by construction**: moving along it changes
//! neither the set of heap values the program creates nor the scopes they die
//! in. Only the ROUTE by which the ownership walk must discover that changes.
//!
//! * **Binder shape** — how the `Option<Row>` is consumed: `let`, `if let`,
//!   `if let/else`, `match`, a projection out of the payload, a closure capture
//!   of the payload, a discarded result, `let/else`, `while let`.
//!
//! * **Scalar provenance** — the record's `i64` field: a literal, a by-value
//!   parameter, an expression over one, a local bound from one. An `i64` owns
//!   no heap and can alias nothing, so a walk that lets it reach a
//!   may-alias-a-heap-parameter verdict is reading a scalar into a heap
//!   question.
//!
//! * **Heap producer** — how the record's `string` field is produced: `+`, a
//!   Hew `fn`, and seven different stdlib string shims. All are `+1` producers.
//!   A bare `string` literal is deliberately EXCLUDED — a static literal may
//!   legitimately need no release, so it is not ownership-neutral against the
//!   others.
//!
//! * **Producer route** — how the `Option<Row>` itself is obtained: a direct
//!   `fn`, a `fn` wrapping another, a concrete `Vec<Row>::get`, a generic
//!   `fn first<T>(v: Vec<T>) -> Option<T>`, and a `HashMap::get`. Each hands
//!   back a fresh `Option<Row>`; none borrows caller storage.
//!
//! # What is counted
//!
//! The release count is read from the ELABORATED MIR (`drop_plans`), not from an
//! allocator. That is the same authority the runtime free calls are generated
//! from, so an under-count here is the leak and an over-count is the double
//! free — without needing to run anything.

use hew_mir::IrPipeline;
use hew_types::module_registry::ModuleRegistry;
use hew_types::Checker;
use std::collections::BTreeMap;

fn pipeline_with_tc(source: &str) -> IrPipeline {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    let output = hew_hir::lower_program(
        &parsed.program,
        &tc_output,
        &hew_hir::ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    hew_mir::lower_hir_module(&output.module)
}

/// Total number of releases the elaborator planted in `main`.
///
/// A source that does not lower at all must never read as "zero releases" — a
/// silently-failing cell would make the whole sweep vacuously green, which is
/// the one way an instrument like this fails without saying so. Absence of the
/// function is therefore a panic, not a count.
fn release_count(p: &IrPipeline) -> usize {
    p.elaborated_mir
        .iter()
        .find(|f| f.name == "main")
        .expect("`main` must lower — a cell that does not compile must fail loudly, never count 0")
        .drop_plans
        .iter()
        .map(|(_, plan)| plan.drops.len())
        .sum()
}

// ── the axes ──────────────────────────────────────────────────────────────

/// Binder shape → the loop-body statement that consumes `PRODUCE`.
const BINDERS: &[(&str, &str)] = &[
    ("let_binder", "        let o = PRODUCE;\n        total = total + 1;\n"),
    ("if_let", "        if let Some(r) = PRODUCE { total = total + r.id; }\n"),
    (
        "if_let_else",
        "        if let Some(r) = PRODUCE { total = total + r.id; } else { total = total + 1; }\n",
    ),
    (
        "match_arm",
        "        match PRODUCE {\n            Some(r) => { total = total + r.id; }\n            \
         None => { total = total + 1; }\n        }\n",
    ),
    (
        "nested_proj",
        "        if let Some(r) = PRODUCE { let s = r.name; total = total + s.len(); }\n",
    ),
    (
        "capture",
        "        if let Some(r) = PRODUCE { let g = || r.id; total = total + g(); }\n",
    ),
    ("discard", "        PRODUCE;\n        total = total + 1;\n"),
    (
        "let_else",
        "        let Some(r) = PRODUCE else { total = total + 1; i = i + 1; continue; };\n        \
         total = total + r.id;\n",
    ),
    (
        "while_let",
        "        var c = PRODUCE;\n        while let Some(r) = c { total = total + r.id; c = None; }\n",
    ),
];

/// Heap producer for `Row.name`. Every one is a fresh `+1` `string`.
const HEAP: &[(&str, &str)] = &[
    ("concat", "\"g2429-\" + \"row\""),
    ("hew_fn", "mk()"),
    ("to_upper", "\"g2429-row\".to_upper()"),
    ("repeat", "\"g\".repeat(3)"),
    ("trim", "\"  g2429  \".trim()"),
    ("replace", "\"a,b\".replace(\"a\", \"z\")"),
    ("slice", "\"g2429-row\".slice(0, 3)"),
    ("fstring", "f\"g2429-{n}\""),
    ("chain", "\"g2429\".to_upper().to_lower()"),
];

/// Scalar provenance for `Row.id`. Every one is an `i64`.
const SCALAR: &[(&str, &str)] = &[
    ("lit", "1"),
    ("param", "n"),
    ("param_expr", "n + 1"),
    ("local", "m"),
];

/// Producer route → the extra declarations it needs, and the body of
/// `fn produce(n: i64) -> Option<Row>`. `ROW` expands to the record literal.
const ROUTES: &[(&str, &str, &str)] = &[
    ("direct_fn", "", "    Some(ROW)\n"),
    (
        "wrapper_fn",
        "fn inner(n: i64) -> Option<Row> { Some(ROW) }\n",
        "    inner(n)\n",
    ),
    (
        "vec_get",
        "",
        "    let v: Vec<Row> = Vec::new();\n    v.push(ROW);\n    v.get(0)\n",
    ),
    (
        "generic_get",
        "fn first<T>(v: Vec<T>) -> Option<T> { v.get(0) }\n",
        "    let v: Vec<Row> = Vec::new();\n    v.push(ROW);\n    first(v)\n",
    ),
    (
        "map_get",
        "",
        "    let m: HashMap<i64, Row> = HashMap::new();\n    m.insert(0, ROW);\n    m.get(0)\n",
    ),
];

fn program(binder: &str, route_decls: &str, route_body: &str, heap: &str, scalar: &str) -> String {
    let local = if scalar == "m" {
        "    let m = n;\n"
    } else {
        ""
    };
    let row = format!("Row {{ name: {heap}, id: {scalar} }}");
    let body = route_body.replace("ROW", &row);
    let decls = route_decls.replace("ROW", &row);
    format!(
        "fn mk() -> string {{ \"g2429-\" + \"row\" }}\n\n\
         type Row {{\n    name: string;\n    id: i64;\n}}\n\n\
         {decls}\n\
         fn produce(n: i64) -> Option<Row> {{\n{local}{body}}}\n\n\
         fn main() -> i64 {{\n    var total = 0;\n    var i = 0;\n    while i < 3 {{\n\
         {}        i = i + 1;\n    }}\n    if total >= 0 {{ 0 }} else {{ 1 }}\n}}\n",
        binder.replace("PRODUCE", "produce(i)")
    )
}

/// Run one plane of the product and assert the release count is constant within
/// each binder shape. `cells` yields `(label, source)` per binder.
fn assert_release_count_invariant(
    plane: &str,
    cells: impl Fn(&str, &str) -> Vec<(String, String)>,
) {
    let mut failures: Vec<String> = Vec::new();
    for (binder_name, binder) in BINDERS {
        let mut counts: BTreeMap<usize, Vec<String>> = BTreeMap::new();
        for (label, source) in cells(binder_name, binder) {
            counts
                .entry(release_count(&pipeline_with_tc(&source)))
                .or_default()
                .push(label);
        }
        if counts.len() > 1 {
            let detail = counts
                .iter()
                .map(|(count, labels)| format!("{count} releases: {}", labels.join(", ")))
                .collect::<Vec<_>>()
                .join("  |  ");
            failures.push(format!("  {binder_name}: {detail}"));
        }
    }
    assert!(
        failures.is_empty(),
        "{plane}: the release count must not depend on an ownership-neutral perturbation. \
         A cell that releases FEWER times than its siblings leaks; one that releases MORE \
         double-frees. Diverging binder shapes:\n{}",
        failures.join("\n")
    );
}

// ── the planes ────────────────────────────────────────────────────────────

/// Plane 1: binder shape × heap producer × scalar provenance, over the direct
/// `fn` route. 9 × 9 × 4 = 324 programs.
///
/// This is the plane the `to_upper`-in-a-record regression lived in: the shim
/// producer and the by-value scalar parameter each independently starved one
/// disjunct of the freshness verdict, and only their CONJUNCTION withheld the
/// release — so neither single-axis perturbation would have exposed it.
#[test]
fn release_count_is_invariant_across_heap_producer_and_scalar_provenance() {
    assert_release_count_invariant("heap-producer × scalar-provenance", |_, binder| {
        let mut out = Vec::new();
        for (heap_name, heap) in HEAP {
            for (scalar_name, scalar) in SCALAR {
                out.push((
                    format!("{heap_name}/{scalar_name}"),
                    program(binder, "", "    Some(ROW)\n", heap, scalar),
                ));
            }
        }
        out
    });
}

/// Plane 2: binder shape × producer route × heap producer × scalar provenance.
/// 9 × 5 × 3 × 2 = 270 programs.
///
/// This is the plane the generic-`Vec::get` regression lived in: the route, not
/// the value, decided whether the release ran. `generic_get` and `map_get` reach
/// the ownership walk through a HIR placeholder symbol rather than the symbol
/// the site emits, so their freshness has to be resolved rather than read.
#[test]
fn release_count_is_invariant_across_producer_route() {
    assert_release_count_invariant("producer-route", |_, binder| {
        let mut out = Vec::new();
        for (route_name, decls, body) in ROUTES {
            for (heap_name, heap) in &HEAP[..3] {
                for (scalar_name, scalar) in &SCALAR[..2] {
                    out.push((
                        format!("{route_name}/{heap_name}/{scalar_name}"),
                        program(binder, decls, body, heap, scalar),
                    ));
                }
            }
        }
        out
    });
}

/// The sweep is only meaningful if a withheld release actually moves the number
/// it reads. Pin that the instrument has resolution: the `discard` shape, whose
/// result is never bound, must still plan strictly fewer releases than the
/// `if_let` shape that binds and projects the payload.
///
/// Without this, a `release_count` that returned a constant — the elaborated
/// function missing, the drop plans empty — would make both planes vacuously
/// green.
#[test]
fn the_sweep_can_observe_a_difference_in_release_count() {
    let discard = release_count(&pipeline_with_tc(&program(
        BINDERS.iter().find(|(n, _)| *n == "discard").unwrap().1,
        "",
        "    Some(ROW)\n",
        HEAP[0].1,
        SCALAR[0].1,
    )));
    let if_let = release_count(&pipeline_with_tc(&program(
        BINDERS.iter().find(|(n, _)| *n == "if_let").unwrap().1,
        "",
        "    Some(ROW)\n",
        HEAP[0].1,
        SCALAR[0].1,
    )));
    assert!(
        discard > 0 && if_let > discard,
        "the release counter must have resolution — binding and projecting the payload \
         (`if_let` = {if_let}) must plan strictly more releases than discarding the call \
         result (`discard` = {discard}), and neither may be zero"
    );
}
