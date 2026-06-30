//! #2320: the CLI gates the fail-closed codegen source caret on root-module
//! membership at the render boundary, keyed by `imported_function_symbol_names`.
//!
//! `RawMirFunction.span` is a byte offset into the function's own module source,
//! but codegen's debug line-index and the CLI's fail-closed codegen diagnostic
//! renderer resolve spans against the ROOT input. Rather than drop imported
//! spans in the MIR producer (which would blind the codegen attach point that
//! records the failing function's identity), the producer keeps them and the
//! CLI render boundary degrades an imported function's error to a bare line.
//! This test locks the membership set that gate consumes: it must name every
//! imported function — and the mangled name of an imported generic's
//! monomorphisation — while excluding root functions, whose carets still render.

use hew_hir::{lower_program, HirItem, ResolutionCtx};
use hew_mir::{imported_function_symbol_names, lower_hir_module};
use hew_types::module_registry::ModuleRegistry;
use hew_types::Checker;
use std::collections::HashSet;

/// `(function name, raw-MIR span)` for each lowered function, used to assert the
/// MIR producer no longer drops imported spans (render-boundary gating).
type RawSpans = Vec<(String, Option<(u32, u32)>)>;

/// Lower `src`, stamp `imported` as originating from a `dep` module (as a file
/// import would populate `diagnostic_source_modules`), and return both the
/// imported-symbol set the CLI gate consumes and each raw-MIR function's
/// `(name, span)` — so the test can also assert spans are NO LONGER dropped.
fn imported_set_and_raw_spans(src: &str, imported: &str) -> (HashSet<String>, RawSpans) {
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
    let imported_set = imported_function_symbol_names(&output.module);
    let raw_spans = lower_hir_module(&output.module)
        .raw_mir
        .iter()
        .map(|r| (r.name.clone(), r.span))
        .collect();
    (imported_set, raw_spans)
}

#[test]
fn imported_function_is_named_in_membership_set() {
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
    let (imported_set, raw_spans) = imported_set_and_raw_spans(src, "dep_fn");

    assert!(
        imported_set.contains("dep_fn"),
        "the imported function must be named in the membership set the CLI gate \
         consumes (#2320); got {imported_set:?}"
    );
    assert!(
        !imported_set.contains("root_fn") && !imported_set.contains("main"),
        "root-module functions must be excluded so their source carets still \
         render (#2091); got {imported_set:?}"
    );

    // Pure render-boundary gating: the MIR producer no longer nulls imported
    // spans. Both the imported and the root function retain their spans; the CLI
    // decides whether to draw a caret using the membership set above.
    let dep_span = raw_spans
        .iter()
        .find(|(n, _)| n == "dep_fn")
        .map(|(_, s)| *s);
    assert!(
        matches!(dep_span, Some(Some(_))),
        "the imported function's span must be RETAINED (render-boundary gating, \
         not producer suppression); got {dep_span:?}"
    );
    let root_span = raw_spans
        .iter()
        .find(|(n, _)| n == "root_fn")
        .map(|(_, s)| *s);
    assert!(
        matches!(root_span, Some(Some(_))),
        "a root-module function's span must be retained (#2091); got {root_span:?}"
    );
}

#[test]
fn imported_generic_monomorphisation_is_named_in_membership_set() {
    // A monomorphisation of an imported generic re-lowers the imported origin's
    // body under a mangled name that never appears in `module.items`. Tagging
    // only the origin must still place the mono's mangled name in the set —
    // otherwise an imported generic's codegen failure would mislabel the root.
    let src = r"
        fn dep_id<T>(x: T) -> T {
            x
        }
        fn main() -> i64 {
            dep_id(7)
        }
    ";
    let (imported_set, raw_spans) = imported_set_and_raw_spans(src, "dep_id");

    // Every non-root raw-MIR function here is the imported generic's
    // monomorphisation; each mangled name must be in the membership set.
    for (name, _) in &raw_spans {
        if name != "main" {
            assert!(
                imported_set.contains(name),
                "imported monomorphisation `{name}` must be named in the membership \
                 set (#2320); got {imported_set:?}"
            );
        }
    }
    assert!(
        !imported_set.contains("main"),
        "the root `main` must be excluded from the imported set; got {imported_set:?}"
    );
}
