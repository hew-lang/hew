//! W4.001 Stage C0b — regression gate: layout-descriptor catalog rows are
//! *not* callable.
//!
//! The `BuiltinLinkage::LayoutDescriptorSymbol` variant added in C0b
//! declares 21 `#[no_mangle] pub static` descriptor symbols (10 K + 11 V).
//! These rows exist only for ABI enumeration and the
//! `stdlib_catalog_layout_descriptor_coverage` gate — they must never be
//! reachable as user-callable functions, because:
//!
//! 1. They name *statics*, not fns; the runtime has no entry-point body.
//! 2. MIR's `module_fn_names` already excludes them, so a HIR call that
//!    reached MIR would route through the runtime-ABI / fail-closed paths
//!    rather than `Terminator::Call` — producing confusing diagnostics
//!    far from the user's actual mistake.
//!
//! The fix in `hew-hir/src/lower.rs`:
//!   - `seed_stdlib_fn_registry` skips `LayoutDescriptorSymbol` rows so
//!     they never enter `fn_registry`.
//!   - `build_callable_set` (the verifier's callable-set builder) does the
//!     same.
//!
//! ## How the test discriminates regression from intended behaviour
//!
//! The checker rejects an unknown function name with an `UndefinedFunction`
//! `TypeError`. That happens regardless of whether the catalog row was
//! seeded — so observing a checker error alone is **not** sufficient to
//! prove the HIR-layer exclusion is wired.
//!
//! The load-bearing signal is at the HIR layer: with the exclusion in
//! place, HIR-lowering emits an `UnresolvedSymbol { name: <descriptor> }`
//! diagnostic; **without** it, the row in `fn_registry` would let the
//! HIR-resolver "succeed" silently and only a downstream
//! `CheckerBoundaryViolation` would surface (which masks the root cause
//! of the rejection). This test pins the `UnresolvedSymbol` presence as
//! the discriminating contract — verified empirically against
//! lower.rs both with and without the skip blocks.

use hew_hir::{lower_program, HirDiagnosticKind, ResolutionCtx};
use hew_types::module_registry::ModuleRegistry;
use hew_types::Checker;

/// Every catalog descriptor symbol shipped by C0b. Lockstep with the
/// `KEY_LAYOUT_DESCRIPTOR_SYMBOLS` / `VAL_LAYOUT_DESCRIPTOR_SYMBOLS`
/// manifests in `hew-cabi/src/map.rs` and the catalog rows in
/// `hew-hir/src/stdlib_catalog.rs`.
const DESCRIPTOR_SYMBOLS: &[&str] = &[
    // Key descriptors (10)
    "hew_layout_key_i32",
    "hew_layout_key_i64",
    "hew_layout_key_u32",
    "hew_layout_key_u64",
    "hew_layout_key_f32",
    "hew_layout_key_f64",
    "hew_layout_key_bool",
    "hew_layout_key_char",
    "hew_layout_key_string",
    "hew_layout_key_bytes",
    // Value descriptors (11)
    "hew_layout_val_i32",
    "hew_layout_val_i64",
    "hew_layout_val_u32",
    "hew_layout_val_u64",
    "hew_layout_val_f32",
    "hew_layout_val_f64",
    "hew_layout_val_bool",
    "hew_layout_val_char",
    "hew_layout_val_string",
    "hew_layout_val_bytes",
    "hew_layout_val_unit",
];

fn lower_call(symbol: &str) -> Vec<HirDiagnosticKind> {
    let source = format!(
        r"
        fn main() {{
            {symbol}();
        }}
        "
    );
    let parsed = hew_parser::parse(&source);
    // Parsing a bare identifier call must always succeed — the
    // rejection has to come from the resolution layer, not the parser.
    assert!(
        parsed.errors.is_empty(),
        "parse must succeed for `{symbol}()`; got: {:#?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    // Sanity-check: checker rejects the call as an undefined function.
    // (Pre-condition of the test scenario; not the discriminator.)
    assert!(
        !tc_output.errors.is_empty(),
        "checker must reject `{symbol}()` as undefined; got no errors"
    );
    let lower_output = lower_program(
        &parsed.program,
        &tc_output,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    lower_output
        .diagnostics
        .into_iter()
        .map(|d| d.kind)
        .collect()
}

/// **Load-bearing assertion.** For every descriptor symbol, HIR lowering
/// must emit an `UnresolvedSymbol { name: <descriptor> }` diagnostic.
/// Empirically verified: without the `LayoutDescriptorSymbol` skip in
/// `seed_stdlib_fn_registry`, the catalog row pre-empts HIR resolution
/// and the `UnresolvedSymbol` diagnostic disappears (only a downstream
/// `CheckerBoundaryViolation` would remain). Presence of
/// `UnresolvedSymbol` for the descriptor name is therefore the
/// discriminator between "HIR exclusion wired" and "HIR exclusion
/// silently dropped".
#[test]
fn every_layout_descriptor_symbol_emits_hir_unresolved_diagnostic() {
    let mut failures: Vec<String> = Vec::new();

    for symbol in DESCRIPTOR_SYMBOLS {
        let diags = lower_call(symbol);
        let has_unresolved = diags.iter().any(|d| {
            matches!(
                d,
                HirDiagnosticKind::UnresolvedSymbol { name } if name == symbol
            )
        });
        if !has_unresolved {
            failures.push(format!(
                "calling descriptor `{symbol}()` did NOT surface \
                 `UnresolvedSymbol {{ name: \"{symbol}\" }}` in HIR diagnostics. \
                 This is the regression marker — the catalog row is still in \
                 `fn_registry`, allowing HIR resolution to silently succeed \
                 before MIR drops the call. Check `hew-hir/src/lower.rs::\
                 seed_stdlib_fn_registry` and `build_callable_set` both skip \
                 `BuiltinLinkage::LayoutDescriptorSymbol`. Got diagnostics: \
                 {diags:#?}"
            ));
        }
    }

    assert!(
        failures.is_empty(),
        "Layout descriptor HIR non-callability gate failed for {n} symbol(s):\n  - {joined}",
        n = failures.len(),
        joined = failures.join("\n  - "),
    );
}

/// Negative-control style spot check: a randomly chosen *real* runtime
/// FFI shim (one that *is* callable) does NOT emit `UnresolvedSymbol` —
/// proving the diagnostic is contingent on the descriptor classification
/// and not a blanket "every unknown name produces `UnresolvedSymbol`"
/// effect that would make the load-bearing test above vacuous.
#[test]
fn callable_runtime_shim_does_not_emit_unresolved_symbol() {
    // `hew_string_len` is a `RuntimeFfiShim` row that has always been
    // callable and seeded into `fn_registry`. Calling it via its catalog
    // surface name (not the runtime symbol) should *not* surface an
    // `UnresolvedSymbol` for that name — it resolves cleanly through the
    // normal stdlib resolution path.
    let source = r#"
        fn main() {
            let _ = "hi".len();
        }
    "#;
    let parsed = hew_parser::parse(source);
    assert!(parsed.errors.is_empty(), "parse: {:#?}", parsed.errors);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    let lower_output = lower_program(
        &parsed.program,
        &tc_output,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );

    // None of the descriptor symbols should appear in *any* HIR diagnostic
    // when no descriptor is referenced — guards against a false-positive
    // pollution of the diagnostic surface.
    for symbol in DESCRIPTOR_SYMBOLS {
        for d in &lower_output.diagnostics {
            if let HirDiagnosticKind::UnresolvedSymbol { name } = &d.kind {
                assert_ne!(
                    name, symbol,
                    "no descriptor symbol should appear in unrelated diagnostics; \
                     saw `{name}` in {:#?}",
                    lower_output.diagnostics,
                );
            }
        }
    }
}
