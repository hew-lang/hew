//! #2320: an imported function's source span must be dropped before codegen.
//!
//! `RawMirFunction.span` is a byte offset into the function's own module
//! source, but codegen's debug line-index and the CLI's fail-closed codegen
//! diagnostic renderer only ever resolve spans against the ROOT input. An
//! imported function's span resolved against the root mislabels the location
//! (a false `root.hew:line:col` caret pointing at another file). The MIR
//! producer therefore drops the span of every function whose item is tagged in
//! `HirModule::diagnostic_source_modules` (the imported-provenance signal), so
//! a fail-closed codegen error in an imported module degrades to the bare,
//! locationless line instead of mislabelling — while root functions keep the
//! source-attributed carets #2091 added.

use hew_hir::{lower_program, HirItem, ResolutionCtx};
use hew_mir::lower_hir_module;
use hew_types::module_registry::ModuleRegistry;
use hew_types::Checker;

/// Lower `src`, stamp `imported` as originating from a `dep` module (as a file
/// import would populate `diagnostic_source_modules`) before MIR lowering, and
/// return each raw-MIR function's `(name, span)`.
fn raw_spans_with_imported(src: &str, imported: &str) -> Vec<(String, Option<(u32, u32)>)> {
    let parsed = hew_parser::parse(src);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc = checker.check_program(&parsed.program);
    let mut output = lower_program(
        &parsed.program,
        &tc,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    let id = output
        .module
        .items
        .iter()
        .find_map(|item| match item {
            HirItem::Function(f) if f.name == imported => Some(f.id),
            _ => None,
        })
        .expect("imported fn must exist in the lowered module");
    output
        .module
        .diagnostic_source_modules
        .insert(id, "dep".to_string());
    lower_hir_module(&output.module)
        .raw_mir
        .iter()
        .map(|r| (r.name.clone(), r.span))
        .collect()
}

#[test]
fn imported_function_span_is_dropped_before_codegen() {
    let src = r"
        fn dep_fn(a: i64) -> i64 {
            a + 1
        }
        fn root_fn() -> i64 {
            dep_fn(2)
        }
        fn main() -> i64 {
            root_fn()
        }
    ";
    let spans = raw_spans_with_imported(src, "dep_fn");

    let dep_span = spans
        .iter()
        .find(|(n, _)| n == "dep_fn")
        .map(|(_, s)| *s)
        .expect("dep_fn must be lowered to raw MIR");
    assert_eq!(
        dep_span, None,
        "an imported function's source span must be dropped so a codegen \
         fail-closed error degrades instead of mislabelling against the root \
         source (#2320); got {dep_span:?}"
    );

    // A root-module function keeps its span — the fix is scoped to imported
    // functions and must not regress #2091's source-attributed carets.
    let root_span = spans
        .iter()
        .find(|(n, _)| n == "root_fn")
        .map(|(_, s)| *s)
        .expect("root_fn must be lowered to raw MIR");
    assert!(
        root_span.is_some(),
        "a root-module function's span must be retained (#2091); got None"
    );
}

#[test]
fn imported_generic_monomorphisation_span_is_dropped() {
    // A monomorphisation of an imported generic re-lowers the imported origin's
    // body (so it carries the origin's imported span) under a mangled name that
    // never appears in `module.items`. Tagging only the origin must still drop
    // the mono's span — otherwise an imported generic's codegen failure would
    // mislabel against the root source.
    let src = r"
        fn dep_id<T>(x: T) -> T {
            x
        }
        fn main() -> i64 {
            dep_id(7)
        }
    ";
    let spans = raw_spans_with_imported(src, "dep_id");

    // Every non-root function (the imported generic's monomorphisation) must
    // have its span dropped; only the root `main` retains one.
    for (name, span) in &spans {
        if name != "main" {
            assert_eq!(
                *span, None,
                "imported monomorphisation `{name}` must have its span dropped (#2320); \
                 got {span:?}"
            );
        }
    }
    assert!(
        spans.iter().any(|(n, s)| n == "main" && s.is_some()),
        "the root `main` must retain its span; raw MIR was {spans:?}"
    );
}
