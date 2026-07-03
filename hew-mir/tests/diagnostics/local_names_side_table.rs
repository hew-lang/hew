//! gdb `-g` variable DIEs — the per-local source-name side-table
//! ([`RawMirFunction::local_names`]) must carry the user's binding names for
//! function parameters and `let` bindings through to codegen, which emits them
//! as `DW_TAG_formal_parameter` / `DW_TAG_variable` DIEs so `info locals` /
//! `print <var>` show `x`, not `local_0`. A regression that dropped the names
//! would still emit a (nameless) `-g` binary, so pin the names at the MIR
//! boundary where it is cheap to assert. Fail-closed: an unrecoverable name is
//! `None` (no DIE), never a fabricated `local_N`.

use hew_hir::{lower_program, ResolutionCtx};
use hew_mir::{lower_hir_module, IrPipeline, RawMirFunction};
use hew_types::module_registry::ModuleRegistry;
use hew_types::Checker;

/// Parse → typecheck → HIR-lower → raw/elaborated MIR.
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

fn raw_fn<'a>(p: &'a IrPipeline, name: &str) -> &'a RawMirFunction {
    p.raw_mir
        .iter()
        .find(|f| f.name == name)
        .expect("function must be present in raw_mir")
}

/// `true` when some local slot carries the exact name `want`.
fn has_name(f: &RawMirFunction, want: &str) -> bool {
    f.local_names.iter().any(|n| n.as_deref() == Some(want))
}

#[test]
fn param_names_are_recorded_in_declaration_order() {
    let source = "\
fn add(a: i64, b: i64) -> i64 {
    a + b
}
";
    let pipeline = pipeline_with_tc(source);
    let f = raw_fn(&pipeline, "add");

    // The side-table is parallel to `locals`, so it is at least as long as the
    // parameter prefix.
    assert!(
        f.local_names.len() >= f.params.len(),
        "local_names must cover the parameter slots: names={:?} params={}",
        f.local_names,
        f.params.len()
    );
    // Parameters occupy locals[0..params.len()] and are named from `HirBinding`.
    assert_eq!(
        f.local_names.first().and_then(|n| n.as_deref()),
        Some("a"),
        "param 0 must be named `a`: {:?}",
        f.local_names
    );
    assert_eq!(
        f.local_names.get(1).and_then(|n| n.as_deref()),
        Some("b"),
        "param 1 must be named `b`: {:?}",
        f.local_names
    );
}

#[test]
fn let_binding_names_are_recorded() {
    let source = "\
fn compute() -> i64 {
    let x = 42;
    let y = x + 1;
    y
}
";
    let pipeline = pipeline_with_tc(source);
    let f = raw_fn(&pipeline, "compute");

    assert!(
        has_name(f, "x"),
        "the `let x` binding must be named in local_names: {:?}",
        f.local_names
    );
    assert!(
        has_name(f, "y"),
        "the `let y` binding must be named in local_names: {:?}",
        f.local_names
    );
}

#[test]
fn anonymous_temporaries_keep_none() {
    // A bare expression statement allocates temporaries that carry no source
    // binding name — they must stay `None` (fail-closed: codegen emits no DIE)
    // rather than being fabricated. The named bindings still appear.
    let source = "\
fn mix(seed: i64) -> i64 {
    let total = seed + seed + seed;
    total
}
";
    let pipeline = pipeline_with_tc(source);
    let f = raw_fn(&pipeline, "mix");

    // `seed` (param) and `total` (let) are named.
    assert!(
        has_name(f, "seed"),
        "param `seed` named: {:?}",
        f.local_names
    );
    assert!(
        has_name(f, "total"),
        "let `total` named: {:?}",
        f.local_names
    );
    // The intermediate `seed + seed` temporary has no binding, so at least one
    // slot is `None` — the table is best-effort, not exhaustive.
    assert!(
        f.local_names.iter().any(Option::is_none),
        "anonymous temporaries must remain unnamed (None): {:?}",
        f.local_names
    );
}
