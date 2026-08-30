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

/// What the sweep observes in one cell: how many releases the elaborator
/// planted, and whether the program was ACCEPTED at all.
///
/// # Why acceptance had to join the observable
///
/// Round 9 read the release count alone. That is blind to the entire class this
/// round found, because an over-restriction does not change a count — it
/// REFUSES the program. Re-running the extended sweep against the defect
/// confirmed the blindness directly: with the fix reverted, every carrier and
/// every extern-participation cell still agreed on its release count while
/// `hew check` was rejecting the standard library.
///
/// A count is a relative fact and needs no allocator. Acceptance is likewise
/// relative and likewise free: a perturbation that changes neither what a
/// program owns nor what it means must not change whether the compiler accepts
/// it. That is the property the `ProcessError` and `CronNextResult` seams
/// violated, and it is the one an axis sweep can actually see.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
struct CellVerdict {
    releases: usize,
    refusals: usize,
}

impl std::fmt::Display for CellVerdict {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.refusals == 0 {
            write!(f, "{} releases", self.releases)
        } else {
            write!(f, "{} releases + {} REFUSED", self.releases, self.refusals)
        }
    }
}

fn cell_verdict(p: &IrPipeline) -> CellVerdict {
    CellVerdict {
        releases: release_count(p),
        refusals: p.diagnostics.len(),
    }
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
    ("if_let", "        if let HIT = PRODUCE { total = total + USEID; }\n"),
    (
        "if_let_else",
        "        if let HIT = PRODUCE { total = total + USEID; } else { total = total + 1; }\n",
    ),
    (
        "match_arm",
        "        match PRODUCE {\n            HIT => { total = total + USEID; }\n            \
         MISS => { total = total + 1; }\n        }\n",
    ),
    (
        "nested_proj",
        "        if let HIT = PRODUCE { let s = USESTR; total = total + s.len(); }\n",
    ),
    (
        "capture",
        "        if let HIT = PRODUCE { let g = || USEID; total = total + g(); }\n",
    ),
    ("discard", "        PRODUCE;\n        total = total + 1;\n"),
    (
        "let_else",
        "        let HIT = PRODUCE else { total = total + 1; i = i + 1; continue; };\n        \
         total = total + USEID;\n",
    ),
    (
        "while_let",
        "        var c = PRODUCE;\n        while let HIT = c { total = total + USEID; c = MISSEXPR; }\n",
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
        "    let v: Vec<Row> = Vec.new();\n    v.push(ROW);\n    v.get(0)\n",
    ),
    (
        "generic_get",
        "fn first<T>(v: Vec<T>) -> Option<T> { v.get(0) }\n",
        "    let v: Vec<Row> = Vec.new();\n    v.push(ROW);\n    first(v)\n",
    ),
    (
        "map_get",
        "",
        "    let m: HashMap<i64, Row> = HashMap.new();\n    m.insert(0, ROW);\n    m.get(0)\n",
    ),
];

/// A carrier class: the shape the binder unwraps, plus everything a binder
/// template needs in order to be written once and instantiated for all of them.
///
/// Round 9's sweep hard-coded exactly one carrier, `Option<Record{string,i64}>`,
/// and said so. That is why it found `HashMap::get` and did NOT find the
/// `ProcessError`/`CronError` seam: the standard library's dominant error
/// carrier is a `Result` whose `Err` payload is an ENUM of `string`s, and no
/// cell of the round-9 product had that shape.
struct Carrier {
    name: &'static str,
    /// Extra declarations this carrier needs, appended to the fixed preamble.
    decls: &'static str,
    /// `produce`'s declared return type.
    ret_ty: &'static str,
    /// Expression building the payload-carrying case. `ROW` expands to the
    /// record literal built from the heap and scalar axes.
    hit_expr: &'static str,
    /// Expression building the empty case.
    miss_expr: &'static str,
    /// Pattern binding the payload as `r`.
    hit_pat: &'static str,
    /// Pattern for the empty case.
    miss_pat: &'static str,
    /// An `i64`-valued expression over the bound payload `r`.
    use_id: &'static str,
    /// A `string`-valued expression over the bound payload `r`.
    use_str: &'static str,
    /// Binder shapes that are NOT ownership-neutral for this carrier, and are
    /// therefore not cells of the product at all.
    ///
    /// This is a statement about the FIXTURE, not about the compiler: a cell
    /// listed here is an invalid program that the language correctly rejects,
    /// so comparing it against its siblings would be comparing a program that
    /// does something different. Genuine refusals belong in [`KNOWN_GAPS`].
    skip_binders: &'static [&'static str],
}

/// The carrier classes the standard library actually uses.
///
/// `result_enum_msg` is `std/process.hew`'s `Result<Output, ProcessError>` and
/// `std/time/cron/cron.hew`'s `Result<i64, CronError>` reduced to their common
/// shape: an error path whose payload is an enum of owned strings, consumed by
/// a callee-owned parameter (`process_error_message(err)`). That is the exact
/// program this round found the compiler refusing.
const CARRIERS: &[Carrier] = &[
    Carrier {
        name: "option_record",
        decls: "",
        ret_ty: "Option<Row>",
        hit_expr: "Some(ROW)",
        miss_expr: "None",
        hit_pat: "Some(r)",
        miss_pat: "None",
        use_id: "r.id",
        use_str: "r.name",
        skip_binders: &[],
    },
    Carrier {
        name: "result_record",
        decls: "",
        ret_ty: "Result<Row, Failure>",
        hit_expr: "Ok(ROW)",
        miss_expr: "Err(Failure.Bad(\"miss\" + \"ed\"))",
        hit_pat: "Ok(r)",
        miss_pat: "Err(_)",
        use_id: "r.id",
        use_str: "r.name",
        skip_binders: &[],
    },
    Carrier {
        name: "result_enum_msg",
        decls: "",
        ret_ty: "Result<i64, Failure>",
        hit_expr: "Err(Failure.Bad(ROWNAME))",
        miss_expr: "Ok(0)",
        hit_pat: "Err(r)",
        miss_pat: "Ok(_)",
        use_id: "failure_len(r)",
        use_str: "failure_msg(r)",
        // `|| failure_len(r)` moves the captured enum OUT of the closure
        // environment, which the language forbids outright ("whole-value move
        // of captured closure value"). A record payload can be read through a
        // field projection instead; an enum payload cannot be read without
        // consuming it, so there is no ownership-neutral capture cell here.
        skip_binders: &["capture"],
    },
    Carrier {
        name: "option_tuple",
        decls: "",
        ret_ty: "Option<(string, i64)>",
        hit_expr: "Some(ROWTUPLE)",
        miss_expr: "None",
        hit_pat: "Some(r)",
        miss_pat: "None",
        use_id: "r.1",
        use_str: "r.0",
        skip_binders: &[],
    },
];

/// Cells the extended sweep REFUSES, with the authority that refuses them.
///
/// This is a ratchet, not a suppression. A listed cell that starts compiling
/// fails the sweep and must be deleted from this table; a refusal that is NOT
/// listed fails the sweep as a new seam. Either way the table cannot silently
/// drift away from what the compiler does.
///
/// The remaining entry is not this round's class: it is a refusal of a DOMESTIC
/// value, with no `extern` anywhere in the program, and is not reachable from
/// the standard library as it is written today. It remains recorded here
/// because the sweep is the instrument that found it and deleting the cell
/// would lose it.
const KNOWN_GAPS: &[(&str, &str, &str)] = &[(
    "result_record",
    "while_let",
    "E_NOT_YET_IMPLEMENTED `while-let scrutinee reassigned from non-fresh value`: \
         reassigning the scrutinee to `Err(Failure::Bad(\"a\" + \"b\"))` — an enum literal \
         over a fresh concat, which is fresh by construction — is not recognised as \
         proven-fresh. The `Option` carrier's `c = None` reassignment carries no heap and \
         so never asks the question.",
)];

fn known_gap(carrier: &str, binder: &str) -> Option<&'static str> {
    KNOWN_GAPS
        .iter()
        .find(|(c, b, _)| *c == carrier && *b == binder)
        .map(|(_, _, why)| *why)
}

/// Declarations every cell carries, whether or not the carrier under test uses
/// them. Keeping the preamble fixed means a carrier cannot change the release
/// count merely by changing what is in scope.
///
/// `failure_len` and `failure_msg` are the `process_error_message(err)` shape:
/// a callee-owned parameter that consumes an enum of owned strings.
const PREAMBLE: &str = "fn mk() -> string { \"g2429-\" + \"row\" }\n\n\
     type Row {\n    name: string;\n    id: i64;\n}\n\n\
     enum Failure { Bad(string); Worse(string) }\n\n\
     fn failure_msg(f: Failure) -> string { \
     match f { Failure.Bad(m) => m, Failure.Worse(m) => m } }\n\
     fn failure_len(f: Failure) -> i64 { failure_msg(f).len() }\n\n";

/// Expand a binder template and a route body for one carrier.
fn instantiate(
    carrier: &Carrier,
    binder: &str,
    route_body: &str,
    heap: &str,
    scalar: &str,
) -> String {
    let row = format!("Row {{ name: {heap}, id: {scalar} }}");
    let hit = carrier
        .hit_expr
        .replace("ROWTUPLE", &format!("({heap}, {scalar})"))
        .replace("ROWNAME", heap)
        .replace("ROW", &row);
    let local = if scalar == "m" {
        "    let m = n;\n"
    } else {
        ""
    };
    let body = route_body
        .replace("HITEXPR", &hit)
        .replace("MISSEXPR", carrier.miss_expr)
        .replace("ROW", &row);
    let stmt = binder
        .replace("PRODUCE", "produce(i)")
        .replace("HIT", carrier.hit_pat)
        .replace("MISSEXPR", carrier.miss_expr)
        .replace("MISS", carrier.miss_pat)
        .replace("USEID", carrier.use_id)
        .replace("USESTR", carrier.use_str);
    format!(
        "{PREAMBLE}{}\n\
         fn produce(n: i64) -> {} {{\n{local}{body}}}\n\n\
         fn main() -> i64 {{\n    var total = 0;\n    var i = 0;\n    while i < 3 {{\n\
         {stmt}        i = i + 1;\n    }}\n    if total >= 0 {{ 0 }} else {{ 1 }}\n}}\n",
        carrier.decls, carrier.ret_ty
    )
}

/// The round-9 signature, preserved: the `option_record` carrier over an
/// explicit route.
fn program(binder: &str, route_decls: &str, route_body: &str, heap: &str, scalar: &str) -> String {
    let row = format!("Row {{ name: {heap}, id: {scalar} }}");
    let local = if scalar == "m" {
        "    let m = n;\n"
    } else {
        ""
    };
    let body = route_body.replace("ROW", &row);
    let decls = route_decls.replace("ROW", &row);
    let stmt = binder
        .replace("PRODUCE", "produce(i)")
        .replace("HIT", "Some(r)")
        .replace("MISSEXPR", "None")
        .replace("MISS", "None")
        .replace("USEID", "r.id")
        .replace("USESTR", "r.name");
    format!(
        "{PREAMBLE}{decls}\n\
         fn produce(n: i64) -> Option<Row> {{\n{local}{body}}}\n\n\
         fn main() -> i64 {{\n    var total = 0;\n    var i = 0;\n    while i < 3 {{\n\
         {stmt}        i = i + 1;\n    }}\n    if total >= 0 {{ 0 }} else {{ 1 }}\n}}\n"
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
        let mut counts: BTreeMap<CellVerdict, Vec<String>> = BTreeMap::new();
        for (label, source) in cells(binder_name, binder) {
            counts
                .entry(cell_verdict(&pipeline_with_tc(&source)))
                .or_default()
                .push(label);
        }
        let refused = counts.keys().any(|verdict| verdict.refusals > 0);
        if counts.len() > 1 || refused {
            let detail = counts
                .iter()
                .map(|(verdict, labels)| format!("{verdict}: {}", labels.join(", ")))
                .collect::<Vec<_>>()
                .join("  |  ");
            failures.push(format!("  {binder_name}: {detail}"));
        }
    }
    assert!(
        failures.is_empty(),
        "{plane}: neither the release count nor ACCEPTANCE may depend on an \
         ownership-neutral perturbation. A cell that releases FEWER times than its siblings \
         leaks; one that releases MORE double-frees; one that is REFUSED rejects a program \
         its siblings accept. Every cell here is a valid program by construction, so any \
         refusal at all is a defect. Diverging binder shapes:\n{}",
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
/// result is discharged immediately, plans fewer delayed releases than the
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
        discard == 0 && if_let > discard,
        "the release counter must have resolution — binding and projecting the payload \
         (`if_let` = {if_let}) must plan strictly more releases than discarding the call \
         result (`discard` = {discard}), which is discharged inline"
    );
}

// ── the carrier plane ─────────────────────────────────────────────────────

/// Plane 3: **carrier class** × binder shape × heap producer × scalar
/// provenance, over the direct `fn` route.
///
/// Round 9's stated limit was that it swept only the `Option<Record>` carrier.
/// That limit is exactly why `ProcessError` got through: the standard library's
/// error carrier is a `Result` whose `Err` payload is an enum of owned strings,
/// consumed by a callee-owned parameter — a shape with no cell in the round-9
/// product.
///
/// Invariance is asserted WITHIN a carrier, not across carriers: a `Result`
/// legitimately owns more than an `Option`, so a cross-carrier comparison would
/// be comparing programs that own different things. What must hold is that
/// inside each carrier the count still does not depend on how the payload's
/// string was produced or where its scalar came from.
///
/// Each carrier is an independent comparison plane. Running those planes as
/// deterministic shards keeps every cell in the product while allowing the
/// test runner to execute the four expensive planes concurrently.
const CARRIER_CLASS_SHARDS: usize = 4;

fn assert_carrier_class_invariant(shard: usize) {
    let mut failures: Vec<String> = Vec::new();
    let mut closed_gaps: Vec<String> = Vec::new();
    let mut carriers_checked = 0;
    for carrier in CARRIERS
        .iter()
        .enumerate()
        .filter(|(index, _)| index % CARRIER_CLASS_SHARDS == shard)
        .map(|(_, carrier)| carrier)
    {
        carriers_checked += 1;
        for (binder_name, binder) in BINDERS {
            if carrier.skip_binders.contains(binder_name) {
                continue;
            }
            let mut counts: BTreeMap<CellVerdict, Vec<String>> = BTreeMap::new();
            for (heap_name, heap) in HEAP {
                for (scalar_name, scalar) in SCALAR {
                    let source = instantiate(carrier, binder, "    HITEXPR\n", heap, scalar);
                    counts
                        .entry(cell_verdict(&pipeline_with_tc(&source)))
                        .or_default()
                        .push(format!("{heap_name}/{scalar_name}"));
                }
            }
            let refused = counts.keys().any(|verdict| verdict.refusals > 0);
            match known_gap(carrier.name, binder_name) {
                // A recorded gap that still refuses is the expected state.
                Some(_) if refused => {}
                Some(why) => closed_gaps.push(format!("  {}/{binder_name}: {why}", carrier.name)),
                None if counts.len() > 1 || refused => {
                    let detail = counts
                        .iter()
                        .map(|(verdict, labels)| format!("{verdict}: {}", labels.join(", ")))
                        .collect::<Vec<_>>()
                        .join("  |  ");
                    failures.push(format!("  {}/{binder_name}: {detail}", carrier.name));
                }
                None => {}
            }
        }
    }
    assert!(
        carriers_checked > 0,
        "carrier-class shard {shard} must cover at least one carrier"
    );
    assert!(
        closed_gaps.is_empty(),
        "these cells are listed in KNOWN_GAPS but now compile — delete the entries so the \
         table cannot drift away from what the compiler does:\n{}",
        closed_gaps.join("\n")
    );
    assert!(
        failures.is_empty(),
        "carrier class: neither the release count nor ACCEPTANCE may depend on an \
         ownership-neutral perturbation. A cell that releases FEWER times than its \
         siblings leaks; one that releases MORE double-frees; one that is REFUSED rejects \
         a program its siblings accept. Every cell here is a valid program by \
         construction, so any refusal not in KNOWN_GAPS is a new seam:\n{}",
        failures.join("\n")
    );
}

#[test]
fn release_count_is_invariant_across_carrier_class_shard_1_of_4() {
    assert_carrier_class_invariant(0);
}

#[test]
fn release_count_is_invariant_across_carrier_class_shard_2_of_4() {
    assert_carrier_class_invariant(1);
}

#[test]
fn release_count_is_invariant_across_carrier_class_shard_3_of_4() {
    assert_carrier_class_invariant(2);
}

#[test]
fn release_count_is_invariant_across_carrier_class_shard_4_of_4() {
    assert_carrier_class_invariant(3);
}

// ── the extern-provenance plane ───────────────────────────────────────────

/// How the carrier's owned `string` payload is PRODUCED. Every variant yields
/// exactly one fresh `+1` string and nothing else, so all three own the same
/// thing and must be accepted alike.
///
/// The point is that the extern must be the payload's PRODUCER, not a
/// bystander. Both defects this round found travel that way: the extern's
/// result flows INTO the value the error carrier is built from, and the
/// provenance verdict travels out with it through the producing function's
/// return, so it is the carrier's binder — not the extern call — that gets
/// refused several frames later.
///
/// * `audited_extern` is `std/process.hew`'s
///   `hew_process_last_error() -> string`: an audited `result = "fresh"` row
///   with `release-symbol = "hew_string_drop"`. It hands the caller a newly
///   owned allocation, exactly as `mk()` does. It was a DEFERRED cell until
///   #2828 settled the retention question — the row now also carries
///   `result-retention = "transferred"`, measured at the runtime's own
///   alloc/free sites, and the mint side reads that rather than inferring it
///   from "audited fresh". Before that answer existed this cell planned ZERO
///   releases against the domestic one's; folding it back in is what pins the
///   leak closed.
/// * `pod_extern` is `std/time/cron/cron.hew`'s
///   `hew_cron_next_hew(...) -> CronNextResult`, consumed the way
///   `Err(cron_error_from_result(result))` consumes it: the POD is passed into
///   a Hew function that builds the payload. A record of `i32` and `i64` has
///   nowhere to put a pointer, so it contributes no ownership at all.
///
/// Both fixtures are ROOT compilation units carrying their own `extern "C"`
/// block, which is precisely the condition — `ExternProvenance::Root` — under
/// which `hew check std/process.hew` read the compiler's own runtime ABI as a
/// foreign host.
const PAYLOAD_SOURCES: &[(&str, &str, &str)] = &[
    ("domestic", "", "mk()"),
    (
        "audited_extern",
        "extern \"C\" {\n    fn hew_process_last_error() -> string;\n}\n",
        "unsafe { hew_process_last_error() }",
    ),
    (
        "pod_extern",
        "extern \"C\" {\n    fn host_pod() -> Pod;\n}\n\
         type Pod { status: i32; timestamp: i64 }\n\
         fn from_pod(p: Pod) -> string { f\"g2429-{p.timestamp}\" }\n",
        "from_pod(unsafe { host_pod() })",
    ),
];

/// Plane 5: **payload provenance** × carrier class × binder shape.
///
/// This is the plane THIS round's defect lived in, stated as a metamorphic
/// property rather than as two files: a payload produced by an audited `+1`
/// `extern` and a payload produced by a Hew `+1` function own the same thing,
/// and a payload routed through a pointer-free POD returned by an `extern` owns
/// no more than one built from a literal. None of the three may change either
/// the release count or ACCEPTANCE.
///
/// Round 9's sweep could not have caught this even with these axes added,
/// because it observed the release count alone — and an over-restriction does
/// not change a count, it refuses the program. Acceptance had to join the
/// observable first; see [`CellVerdict`].
#[test]
fn release_count_and_acceptance_are_invariant_across_payload_provenance() {
    let mut failures: Vec<String> = Vec::new();
    for carrier in CARRIERS {
        for (binder_name, binder) in BINDERS {
            if carrier.skip_binders.contains(binder_name)
                || known_gap(carrier.name, binder_name).is_some()
            {
                // Not a cell of this product: either the fixture is not
                // ownership-neutral for this carrier, or the shape is a
                // recorded gap whose refusal plane 3 already ratchets.
                continue;
            }
            let mut counts: BTreeMap<CellVerdict, Vec<String>> = BTreeMap::new();
            for (source_name, decls, heap) in PAYLOAD_SOURCES {
                let source = instantiate(carrier, binder, "    HITEXPR\n", heap, SCALAR[0].1);
                let source = format!("{decls}{source}");
                counts
                    .entry(cell_verdict(&pipeline_with_tc(&source)))
                    .or_default()
                    .push((*source_name).to_string());
            }
            if counts.len() > 1 || counts.keys().any(|verdict| verdict.refusals > 0) {
                let detail = counts
                    .iter()
                    .map(|(verdict, labels)| format!("{verdict}: {}", labels.join(", ")))
                    .collect::<Vec<_>>()
                    .join("  |  ");
                failures.push(format!("  {}/{binder_name}: {detail}", carrier.name));
            }
        }
    }
    assert!(
        failures.is_empty(),
        "payload provenance: a payload routed through a pointer-free POD returned by an \
         `extern` owns exactly what a domestic one owns. Neither the release count nor \
         acceptance may depend on which produced it. A REFUSED cell is this round's \
         class — the compiler rejecting shipped code because an `extern` in its own \
         runtime ABI was read as a foreign host:\n{}",
        failures.join("\n")
    );
}
