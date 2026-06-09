//! W4.001 Stage C2 — ResolvedImplCall HashMap/HashSet kernel-call
//! materialisation gate.
//!
//! The C1 substrate added `HirExprKind::ResolvedImplCall` as the unified
//! consumer of `TypeCheckOutput::resolved_calls` for HashMap/HashSet
//! method dispatch, but the variant was gated dormant (the MIR consumer
//! `unreachable!`'d on emission). C2 flips the gate
//! (`RESOLVED_IMPL_CALL_ACTIVATED = true`) and lowers the variant to a
//! direct `Terminator::Call` against the kernel symbol recorded by the
//! resolver verbatim in `MethodTarget.symbol_name` (now carried on the
//! HIR variant as `target_symbol`).
//!
//! ## What this test asserts
//!
//! 1. **Kernel symbol fidelity end-to-end.** Driving a Hew source
//!    program through parse → type-check → HIR (`lower_program`) →
//!    MIR (`lower_hir_module`), every HashMap / HashSet method call on
//!    a checker-accepted handle shape lowers to a `Terminator::Call`
//!    against the canonical kernel symbol (`hew_hashmap_<op>_layout` /
//!    `hew_hashset_<op>_layout`) with the exact receiver-inclusive
//!    arity demanded by the C0a runtime ABI.
//!
//! 2. **Catalog-presence exhaustiveness (LayoutDescriptorSymbol load-bearing).**
//!    The K × V matrix the resolver may admit is bounded above by the
//!    `BuiltinLinkage::LayoutDescriptorSymbol` rows in the stdlib
//!    catalog. The kernel ABI is `(handle, key_ptr, val_ptr)` for
//!    insert (descriptors are snapshotted into the map at construction
//!    time, not passed per-op), so this is a *catalog presence* check
//!    rather than a call-arg materialisation — but it is the
//!    load-bearing invariant that locks the catalog against silent
//!    shrinkage during Stage C3 retirements.
//!
//! ## Scope honesty
//!
//! At v0.5 the checker admits only **Named-record K** for the
//! layout-keyed HashMap/HashSet substrate (see
//! `examples/v05/hashmap_run_pass.hew`). Primitive K (e.g. `i64`) is
//! intentionally rejected by the typechecker today with the structured
//! "HashMap currently requires string keys" diagnostic — Stage C3 is the
//! lane that expands the accepted-K set. Therefore the end-to-end
//! kernel-symbol matrix walks **Named-record K × multiple V classes ×
//! every kernel op** (the accept set that C2 actually activates). The
//! catalog-presence sub-test (which is purely catalog-side and does not
//! drive the checker) independently walks the full 10×11 primitive
//! K × V matrix to guard the descriptor inventory that C3 will later
//! activate from the checker side.
//!
//! ## LESSONS walked
//!
//! - `checker-authority` (P0): `target_symbol` is read VERBATIM from
//!   the HIR variant — never re-derived from `method_name`.
//! - `exhaustive-coverage` (P0): every kernel op (HashMap insert / get /
//!   contains_key / remove / len; HashSet insert / contains / remove /
//!   len) is exercised; the catalog presence sub-test walks the closed
//!   10×11 K × V grid.
//! - `boundary-fail-closed` (P0): kernel-symbol strings match the
//!   runtime exports VERBATIM; the test compares against literal
//!   `hew_hashmap_*_layout` / `hew_hashset_*_layout` strings rather
//!   than reconstructing them.
//! - `parity-or-tracked-gap` (P0): WASM parity is covered by the
//!   existing `wasm_fail_closed_for_every_layout_op_symbol` test in
//!   `hashmap_layout_ops.rs`; this lane changes nothing in
//!   `uses_wasm_excluded_symbol`.

use std::collections::BTreeSet;

use hew_hir::{
    lower_program,
    stdlib_catalog::{entries, BuiltinLinkage, LayoutDescriptorRole},
    ResolutionCtx,
};
use hew_mir::{lower_hir_module, Terminator};
use hew_types::{module_registry::ModuleRegistry, Checker};

// ---------------------------------------------------------------------------
// Pipeline helpers
// ---------------------------------------------------------------------------

fn lower_module(source: &str) -> hew_mir::IrPipeline {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors for source\n----\n{source}\n----\n{:#?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    assert!(
        tc_output.errors.is_empty(),
        "type errors for source\n----\n{source}\n----\n{:#?}",
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
        "HIR diagnostics for source\n----\n{source}\n----\n{:#?}",
        hir.diagnostics
    );
    lower_hir_module(&hir.module)
}

fn collect_call_terminators(pipeline: &hew_mir::IrPipeline) -> Vec<(String, usize)> {
    let mut found = Vec::new();
    for func in &pipeline.raw_mir {
        if func.name != "main" {
            continue;
        }
        for block in &func.blocks {
            if let Terminator::Call { callee, args, .. } = &block.terminator {
                found.push((callee.clone(), args.len()));
            }
        }
    }
    found
}

fn has_call(pipeline: &hew_mir::IrPipeline, sym: &str, arity: usize) -> bool {
    collect_call_terminators(pipeline)
        .iter()
        .any(|(c, a)| c == sym && *a == arity)
}

// ---------------------------------------------------------------------------
// (1) Anchor: HashMap<Point, i64>.insert → hew_hashmap_insert_layout / arity 3
// ---------------------------------------------------------------------------

/// Smoke test for the gate flip: the simplest accepted handle shape
/// lowers `m.insert(k, v)` through `ResolvedImplCall` into the kernel
/// `Terminator::Call`. If this fails, either the gate (`RESOLVED_IMPL_CALL_ACTIVATED`)
/// or the MIR arm regressed and end-to-end HashMap dispatch is broken.
#[test]
fn hashmap_point_i64_insert_lowers_to_hew_hashmap_insert_layout_with_arity_3() {
    let src = r"
record Point { x: i64, y: i64 }

fn main() {
    let m: HashMap<Point, i64> = HashMap::new();
    m.insert(Point { x: 1, y: 2 }, 42);
}
";
    let pipeline = lower_module(src);
    assert!(
        has_call(&pipeline, "hew_hashmap_insert_layout", 3),
        "HashMap<Point, i64>.insert(K, V) must lower to Terminator::Call \
         {{ callee: \"hew_hashmap_insert_layout\", args.len() = 3 }} \
         (receiver, key, value); got: {:#?}",
        collect_call_terminators(&pipeline)
    );
}

// ---------------------------------------------------------------------------
// (2) HashMap kernel-symbol matrix: every op against the accepted shape
// ---------------------------------------------------------------------------

/// V classes the checker admits today as a HashMap value: scalar /
/// string per the active validator. (`i64`, `string`, `bool`, `char`
/// cover the breadth without piling on f32/f64 — those exist in the
/// catalog but the resolver gates them out at use-site.)
const ACCEPTED_VALUE_TYPES: &[(&str, &str)] = &[
    // (type-syntax, literal-expression-for-V)
    ("i64", "10"),
    ("string", "\"v\""),
    ("bool", "true"),
    ("char", "'v'"),
];

/// Walk every kernel op against `HashMap<Point, V>` for each accepted
/// V class. Asserts both the kernel symbol AND the receiver-inclusive
/// arity (any drift surfaces as a test failure with the full
/// `Terminator::Call` inventory attached).
#[test]
fn hashmap_kernel_symbol_matrix_named_record_k_by_accepted_v_classes() {
    let ops: &[(&str, &str, &str, usize)] = &[
        // (op-name, source-expr-template, kernel-symbol, receiver-inclusive arity)
        (
            "insert",
            "insert(Point { x: 1, y: 2 }, {v})",
            "hew_hashmap_insert_layout",
            3,
        ),
        (
            "get",
            "get(Point { x: 1, y: 2 })",
            "hew_hashmap_get_layout",
            2,
        ),
        (
            "contains_key",
            "contains_key(Point { x: 1, y: 2 })",
            "hew_hashmap_contains_key_layout",
            2,
        ),
        (
            "remove",
            "remove(Point { x: 1, y: 2 })",
            "hew_hashmap_remove_layout",
            2,
        ),
        ("len", "len()", "hew_hashmap_len_layout", 1),
    ];

    let mut failures: Vec<String> = Vec::new();
    for (v_ty, v_lit) in ACCEPTED_VALUE_TYPES {
        for (op_name, expr_tmpl, kernel_symbol, arity) in ops {
            let expr = expr_tmpl.replace("{v}", v_lit);
            let src = format!(
                r"
record Point {{ x: i64, y: i64 }}

fn main() {{
    let m: HashMap<Point, {v_ty}> = HashMap::new();
    let _r = m.{expr};
}}
"
            );
            let pipeline = lower_module(&src);
            if !has_call(&pipeline, kernel_symbol, *arity) {
                failures.push(format!(
                    "HashMap<Point, {v_ty}>.{op_name} → expected Terminator::Call \
                     {{ callee: {kernel_symbol:?}, arity: {arity} }}; \
                     got: {:#?}",
                    collect_call_terminators(&pipeline)
                ));
            }
        }
    }
    assert!(
        failures.is_empty(),
        "HashMap kernel-symbol matrix failures ({} of {}):\n{}",
        failures.len(),
        ACCEPTED_VALUE_TYPES.len() * ops.len(),
        failures.join("\n---\n")
    );
}

// ---------------------------------------------------------------------------
// (3) HashSet kernel-symbol matrix: every op against the accepted shape
// ---------------------------------------------------------------------------

#[test]
fn hashset_kernel_symbol_matrix_named_record_t_across_every_op() {
    let ops: &[(&str, &str, &str, usize)] = &[
        (
            "insert",
            "insert(Point { x: 1, y: 2 })",
            "hew_hashset_insert_layout",
            2,
        ),
        (
            "contains",
            "contains(Point { x: 1, y: 2 })",
            "hew_hashset_contains_layout",
            2,
        ),
        (
            "remove",
            "remove(Point { x: 1, y: 2 })",
            "hew_hashset_remove_layout",
            2,
        ),
        ("len", "len()", "hew_hashset_len_layout", 1),
    ];

    let mut failures: Vec<String> = Vec::new();
    for (op_name, expr, kernel_symbol, arity) in ops {
        let src = format!(
            r"
record Point {{ x: i64, y: i64 }}

fn main() {{
    let s: HashSet<Point> = HashSet::new();
    let _r = s.{expr};
}}
"
        );
        let pipeline = lower_module(&src);
        if !has_call(&pipeline, kernel_symbol, *arity) {
            failures.push(format!(
                "HashSet<Point>.{op_name} → expected Terminator::Call \
                 {{ callee: {kernel_symbol:?}, arity: {arity} }}; \
                 got: {:#?}",
                collect_call_terminators(&pipeline)
            ));
        }
    }
    assert!(
        failures.is_empty(),
        "HashSet kernel-symbol matrix failures ({} of {}):\n{}",
        failures.len(),
        ops.len(),
        failures.join("\n---\n")
    );
}

// ---------------------------------------------------------------------------
// (4) End-to-end: every kernel op surfaces in the run-pass-fixture shape
// ---------------------------------------------------------------------------

/// Mirrors the call topology of `examples/v05/hashmap_run_pass.hew`:
/// a single `main` exercises every HashMap and HashSet op against
/// `Point`-keyed handles, and the MIR must contain a `Terminator::Call`
/// for each kernel symbol with the exact ABI arity. This pins the
/// run-pass fixture's MIR shape against silent drift.
#[test]
fn hashmap_and_hashset_run_pass_topology_pins_every_kernel_symbol() {
    let src = r"
record Point { x: i64, y: i64 }

fn main() {
    let m: HashMap<Point, i64> = HashMap::new();
    m.insert(Point { x: 1, y: 2 }, 10);
    let _g = m.get(Point { x: 1, y: 2 });
    let _c = m.contains_key(Point { x: 1, y: 2 });
    let _r = m.remove(Point { x: 1, y: 2 });
    let _n = m.len();

    let s: HashSet<Point> = HashSet::new();
    s.insert(Point { x: 1, y: 2 });
    let _sc = s.contains(Point { x: 1, y: 2 });
    let _sr = s.remove(Point { x: 1, y: 2 });
    let _sn = s.len();
}
";
    let pipeline = lower_module(src);
    let calls = collect_call_terminators(&pipeline);
    let expected: &[(&str, usize)] = &[
        ("hew_hashmap_insert_layout", 3),
        ("hew_hashmap_get_layout", 2),
        ("hew_hashmap_contains_key_layout", 2),
        ("hew_hashmap_remove_layout", 2),
        ("hew_hashmap_len_layout", 1),
        ("hew_hashset_insert_layout", 2),
        ("hew_hashset_contains_layout", 2),
        ("hew_hashset_remove_layout", 2),
        ("hew_hashset_len_layout", 1),
    ];
    let mut missing: Vec<String> = Vec::new();
    for (sym, arity) in expected {
        if !calls.iter().any(|(c, a)| c == sym && a == arity) {
            missing.push(format!("expected Terminator::Call {sym:?} / arity {arity}"));
        }
    }
    assert!(
        missing.is_empty(),
        "run-pass topology missing {} kernel-call shapes:\n{}\n\nactual calls: {calls:#?}",
        missing.len(),
        missing.join("\n")
    );
}

// ---------------------------------------------------------------------------
// (5) Catalog-presence exhaustive coverage (LayoutDescriptorSymbol)
// ---------------------------------------------------------------------------

/// K classes the C0b descriptor catalog reaches: 10 primitive classes
/// (including f32/f64 — these descriptors ship `hash_fn = None` /
/// `eq_fn = None` belt-and-suspenders per DI-003; the resolver rejects
/// them at use-site via `BoundNotSatisfied(Hash, f64)`, so a presence
/// row in the catalog is the C0b invariant while resolver acceptance is
/// the C1+C3 invariant).
const REQUIRED_KEY_TYPES: &[&str] = &[
    "i32", "i64", "u32", "u64", "f32", "f64", "bool", "char", "string", "bytes",
];

/// V classes the C0b descriptor catalog reaches: 10 scalar classes plus
/// `unit` for the HashSet<T> = HashMap<T, ()> ZST equivalence.
const REQUIRED_VAL_TYPES: &[&str] = &[
    "i32", "i64", "u32", "u64", "f32", "f64", "bool", "char", "string", "bytes", "unit",
];

fn descriptor_symbol_set(role: LayoutDescriptorRole) -> BTreeSet<String> {
    entries()
        .iter()
        .filter_map(|e| match e.linkage {
            BuiltinLinkage::LayoutDescriptorSymbol {
                symbol,
                role: entry_role,
            } if entry_role == role => Some(symbol.to_string()),
            _ => None,
        })
        .collect()
}

/// Walks every (K, V) cell in the closed coverage matrix and asserts
/// both a `LayoutDescriptorRole::Key` row for K and a
/// `LayoutDescriptorRole::Value` row for V exist in the stdlib catalog.
///
/// Although the runtime ABI does not pass descriptors per-op (they are
/// snapshotted into the map at construction time), this catalog is the
/// load-bearing inventory: Stage C3 will widen the resolver's accepted
/// K set to the full primitive grid below, and any catalog row that
/// silently disappears between C2 and C3 must surface here first.
#[test]
fn k_by_v_coverage_matrix_every_pair_has_both_descriptor_rows() {
    let key_descriptors = descriptor_symbol_set(LayoutDescriptorRole::Key);
    let val_descriptors = descriptor_symbol_set(LayoutDescriptorRole::Value);
    let mut missing: Vec<String> = Vec::new();
    for k in REQUIRED_KEY_TYPES {
        let expected_key = format!("hew_layout_key_{k}");
        for v in REQUIRED_VAL_TYPES {
            let expected_val = format!("hew_layout_val_{v}");
            if !key_descriptors.contains(&expected_key) {
                missing.push(format!(
                    "HashMap<{k}, {v}>: missing K descriptor `{expected_key}` \
                     (LayoutDescriptorRole::Key)"
                ));
            }
            if !val_descriptors.contains(&expected_val) {
                missing.push(format!(
                    "HashMap<{k}, {v}>: missing V descriptor `{expected_val}` \
                     (LayoutDescriptorRole::Value)"
                ));
            }
        }
    }
    assert!(
        missing.is_empty(),
        "K × V coverage matrix has {} gaps:\n{}",
        missing.len(),
        missing.join("\n")
    );
    // Coverage matrix size sanity: 10 × 11 = 110 cells; if the catalog
    // scope shifts under our feet (e.g. a new V class added), the brief
    // and the catalog gate must move together.
    assert_eq!(
        REQUIRED_KEY_TYPES.len() * REQUIRED_VAL_TYPES.len(),
        110,
        "C2 brief cites 10 K × 11 V = 110 cells; the catalog scope has \
         shifted — update the brief and the catalog gate together."
    );
}
