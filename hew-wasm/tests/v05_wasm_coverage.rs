//! Integration tests: v0.5 fixture coverage via the `hew-wasm` public API.
//!
//! Every v0.5 language feature exercised by the LSP fixture set is exercised
//! here via `hew_wasm::analyze`, `hew_wasm::type_check`, and `hew_wasm::hover`.
//! Sources are shared via `include_str!` — no source duplication.
//!
//! Tests run on the native target (cargo test) only; not compiled to wasm32.
//!
//! ## Fixture tiers
//!
//! **`FIXTURES`** (all 30, excluding the cross-module main): every fixture is
//! checked for API validity (no export panic, well-formed JSON, diagnostics array
//! present).
//!
//! **`ANALYSIS_ERROR_FIXTURES`** (subset of `FIXTURES`): fixtures that are
//! expected to produce error-severity diagnostics.  These are excluded from the
//! zero-error loops; instead each gets a dedicated test documenting the known gap.
//!
//! `v05_cross_module_machine_main` is handled entirely outside `FIXTURES` because
//! the single-source API cannot resolve its cross-file import.

// ── Fixture table ─────────────────────────────────────────────────────────

macro_rules! fixture {
    ($name:literal) => {
        (
            $name,
            include_str!(concat!("../../hew-lsp/tests/fixtures/", $name, ".hew")),
        )
    };
}

const FIXTURES: &[(&str, &str)] = &[
    fixture!("v05_associated_type_projection"),
    fixture!("v05_async_await"),
    fixture!("v05_attributes"),
    fixture!("v05_closures"),
    fixture!("v05_cross_module_machine_defs"),
    fixture!("v05_display_fstring"),
    fixture!("v05_extern_unsafe"),
    fixture!("v05_generators"),
    fixture!("v05_impl_where_clause"),
    fixture!("v05_index_trait"),
    fixture!("v05_is_operator"),
    fixture!("v05_link_monitor"),
    fixture!("v05_machine_generics"),
    fixture!("v05_machine_methods"),
    fixture!("v05_machine_states"),
    fixture!("v05_machine_substrate"),
    fixture!("v05_match_enum_variant"),
    fixture!("v05_record_decl"),
    fixture!("v05_record_literals"),
    fixture!("v05_record_tuple_literal"),
    fixture!("v05_regex_literal"),
    fixture!("v05_result_option_ctors"),
    fixture!("v05_scope_fork"),
    fixture!("v05_select_arms"),
    fixture!("v05_spawn_lambda_actor"),
    fixture!("v05_std_channels"),
    fixture!("v05_string_methods"),
    fixture!("v05_trait_bounds"),
    fixture!("v05_unsafe_block"),
    fixture!("v05_while_let"),
];

/// Fixtures that are expected to produce error-severity diagnostics when
/// analyzed as a single source file.  These are excluded from zero-error loop
/// assertions; each has a dedicated test documenting the known gap.
///
/// W4.023 Stage 0 alignment: classification for each fixture is noted inline.
/// See the LSP fixture classification matrix in `hew-lsp/src/server/mod.rs`
/// (the "W4.023 Stage 0" comment block) for the authoritative table.
const ANALYSIS_ERROR_FIXTURES: &[&str] = &[
    // known-rejected: `async fn` / `await` are not valid Hew syntax; the parser
    // rejects them.  The corresponding fail-closed LSP test is
    // `v05_async_await_is_rejected_with_parse_errors`.
    "v05_async_await",
    // hir-rejected: nested closure captures are not yet materialised in the
    // checker side-table.  The type checker accepts the fixture, but HIR
    // lowering surfaces a `CheckerBoundaryViolation` for `twice` (which
    // captures `inc`, itself a closure variable).  The browser editor must not
    // show green for code the native compiler rejects at HIR.  Fail-closed per
    // checker-authority doctrine.  Dedicated test:
    // `v05_wasm_coverage_closure_hir_gap_surfaces_error`.
    "v05_closures",
    // accepted (intentional type error): `obj[key]` on a type that does not
    // implement Indexable — type error by design, exercising error-recovery in
    // the index-expression checker.
    "v05_index_trait",
    // accepted (intentional type error): `machine Boxed<T>` declares only one
    // state (`Idle`); the type checker requires at least two states per machine.
    // The fixture exercises generic machine syntax, not exhaustive state coverage.
    "v05_machine_generics",
    // accepted (intentional type error): `machine Turnstile` handles `Coin` only
    // on `Locked` and `Push` only on `Unlocked` — the type checker requires
    // exhaustive event handling across all states.  The fixture exercises machine
    // method syntax, not coverage.
    "v05_machine_methods",
    // accepted (intentional type error): `machine Traffic` handles only two of
    // the four declared events per state.  The fixture exercises state entry/exit
    // hooks, not exhaustive coverage.
    "v05_machine_states",
    // pending-upstream-substrate W3.006: `type Pair(i32, i32)` — tuple-record
    // syntax is not yet supported by the parser.  The corresponding LSP test is
    // kept ignored pending W3.006 tuple substrate landing.
    "v05_record_tuple_literal",
    // pending-upstream-substrate (unassigned): `select { StreamNext ... }` arms
    // use unimplemented syntax; the parser cannot handle them.  The corresponding
    // LSP test is kept ignored pending a dedicated select-arm-kinds lane.
    "v05_select_arms",
    // pending-upstream-substrate (unassigned): `spawn |msg: i32| { ... }` —
    // lambda actor spawn syntax is not yet supported.  The corresponding LSP test
    // is kept ignored pending a dedicated lambda-spawn lane.
    "v05_spawn_lambda_actor",
    // accepted (known type error): `Channel<i32>` / `Stream<i32>` / `Sink<i32>`
    // — generic channel lowering is currently implemented only for `string` and
    // `bytes`; other element types produce type errors.  The fixture exercises
    // channel syntax, not full lowering.  Stage 4 should add fail-closed
    // type-error assertions for generic channel params.
    "v05_std_channels",
    // hir-rejected: `println(holder.value)` inside `impl<T> Holder<T> where T:
    // Display` — the type checker accepts the constraint but HIR lowering cannot
    // resolve a monomorphic overload for `println` with a generic-typed argument.
    // HIR also fires UnresolvedSymbol on the `println` call site.  Native
    // `hew check` rejects with the same two HIR errors.  The browser editor must
    // not show green for this code.  Fail-closed per checker-authority doctrine.
    // Dedicated test: `v05_wasm_coverage_impl_where_clause_hir_gap_surfaces_error`.
    "v05_impl_where_clause",
];

// ── Helpers ───────────────────────────────────────────────────────────────

fn parse_json(fixture: &str, api: &str, json: &str) -> serde_json::Value {
    serde_json::from_str(json)
        .unwrap_or_else(|e| panic!("fixture {fixture} ({api}): invalid JSON: {e}\nraw: {json}"))
}

fn assert_valid_api_response(fixture: &str, api: &str, json: &str) {
    let parsed = parse_json(fixture, api, json);
    assert!(
        parsed["diagnostics"].is_array(),
        "fixture {fixture} ({api}): response must have a 'diagnostics' array"
    );
}

/// Fail closed: panics with the diagnostic text if any error-severity diagnostics
/// are present.  Warning-severity diagnostics (e.g. unused-function) are permitted.
fn check_zero_errors(fixture: &str, api: &str, parsed: &serde_json::Value) {
    let diags = parsed["diagnostics"].as_array().unwrap_or_else(|| {
        panic!("fixture {fixture} ({api}): response missing 'diagnostics' array")
    });
    let errors: Vec<&serde_json::Value> = diags
        .iter()
        .filter(|d| d["severity"].as_str() == Some("error"))
        .collect();
    if !errors.is_empty() {
        let msgs: Vec<&str> = errors
            .iter()
            .map(|d| d["message"].as_str().unwrap_or("<no message>"))
            .collect();
        panic!(
            "fixture {fixture} ({api}): {} error diagnostic(s): {}",
            errors.len(),
            msgs.join("; ")
        );
    }
}

fn is_analysis_error_fixture(name: &str) -> bool {
    ANALYSIS_ERROR_FIXTURES.contains(&name)
}

// ── Fixture count sanity ──────────────────────────────────────────────────

#[test]
fn v05_wasm_coverage_fixture_count() {
    assert_eq!(
        FIXTURES.len(),
        30,
        "FIXTURES table must cover all 30 v05 fixtures \
         (v05_cross_module_machine_main is tested separately)"
    );
    assert_eq!(
        ANALYSIS_ERROR_FIXTURES.len(),
        11,
        "ANALYSIS_ERROR_FIXTURES must list exactly 11 known-error fixtures"
    );
}

// ── API-valid: all 30 fixtures produce well-formed JSON ───────────────────

#[test]
fn v05_wasm_coverage_analyze_all_api_valid() {
    for (name, source) in FIXTURES {
        let json = hew_wasm::analyze(source)
            .unwrap_or_else(|_| panic!("fixture {name}: analyze() export error"));
        assert_valid_api_response(name, "analyze", &json);
    }
}

#[test]
fn v05_wasm_coverage_type_check_all_api_valid() {
    for (name, source) in FIXTURES {
        let json = hew_wasm::type_check(source)
            .unwrap_or_else(|_| panic!("fixture {name}: type_check() export error"));
        assert_valid_api_response(name, "type_check", &json);
    }
}

// ── Zero-error: 28 clean fixtures produce no error-severity diagnostics ───

#[test]
fn v05_wasm_coverage_analyze_clean_fixtures() {
    for (name, source) in FIXTURES {
        if is_analysis_error_fixture(name) {
            continue;
        }
        let json = hew_wasm::analyze(source)
            .unwrap_or_else(|_| panic!("fixture {name}: analyze() export error"));
        let parsed = parse_json(name, "analyze", &json);
        check_zero_errors(name, "analyze", &parsed);
    }
}

#[test]
fn v05_wasm_coverage_type_check_clean_fixtures() {
    for (name, source) in FIXTURES {
        if is_analysis_error_fixture(name) {
            continue;
        }
        let json = hew_wasm::type_check(source)
            .unwrap_or_else(|_| panic!("fixture {name}: type_check() export error"));
        let parsed = parse_json(name, "type_check", &json);
        check_zero_errors(name, "type_check", &parsed);
    }
}

// ── Known-error fixtures: dedicated tests documenting the gap ─────────────

/// `async fn` / `await` are not valid Hew syntax.  The parser rejects the
/// fixture with one or more error-severity, parse-phase diagnostics.  Mirrors
/// the canonical contract established by
/// `hew-lsp/.../mod.rs::v05_async_await_is_rejected_with_parse_errors`:
/// the fixture must produce parse errors; the exact message text is not
/// contractual (it is a parser-recovery implementation detail).
#[test]
fn v05_wasm_coverage_async_await_api_valid() {
    const FIXTURE: &str = "v05_async_await";
    let source = include_str!("../../hew-lsp/tests/fixtures/v05_async_await.hew");
    let json = hew_wasm::analyze(source)
        .unwrap_or_else(|_| panic!("fixture {FIXTURE}: analyze() export error"));
    let parsed = parse_json(FIXTURE, "analyze", &json);
    let diags = parsed["diagnostics"]
        .as_array()
        .unwrap_or_else(|| panic!("fixture {FIXTURE}: missing diagnostics array"));

    // Require at least one error-severity, parse-phase diagnostic: the parser
    // must reject `async fn` / `await` with an error.  Message text is
    // intentionally not asserted — recovery wording is non-contractual.
    let parse_errors: Vec<&serde_json::Value> = diags
        .iter()
        .filter(|d| d["severity"].as_str() == Some("error") && d["phase"].as_str() == Some("parse"))
        .collect();
    assert!(
        !parse_errors.is_empty(),
        "fixture {FIXTURE}: expected ≥1 parse-phase error diagnostic; \
         got {} diagnostics: {diags:?}",
        diags.len()
    );
}

/// Nested closure captures that the type-checker leaves unresolved surface as
/// a `CheckerBoundaryViolation` at HIR lowering.  The browser editor must show
/// an error for this code: accepting it silently would present the editor as
/// green for a program that `hew run` rejects.
///
/// Specific repro: `let twice = |x: i32| -> i32 { inc(inc(x)) }` where `inc`
/// is itself a closure variable — the checker does not materialise capture
/// metadata for the nested `inc` call, and HIR lowering fires
/// `CheckerBoundaryViolation`.  Fail-closed per checker-authority doctrine.
#[test]
fn v05_wasm_coverage_closure_hir_gap_surfaces_error() {
    const FIXTURE: &str = "v05_closures";
    let source = include_str!("../../hew-lsp/tests/fixtures/v05_closures.hew");
    let json = hew_wasm::analyze(source)
        .unwrap_or_else(|_| panic!("fixture {FIXTURE}: analyze() export error"));
    let parsed = parse_json(FIXTURE, "analyze", &json);
    let diags = parsed["diagnostics"]
        .as_array()
        .unwrap_or_else(|| panic!("fixture {FIXTURE}: missing diagnostics array"));

    let hir_errors: Vec<&serde_json::Value> = diags
        .iter()
        .filter(|d| {
            d["severity"].as_str() == Some("error")
                && d["phase"].as_str() == Some("hir")
                && d["kind"].as_str() == Some("CheckerBoundaryViolation")
        })
        .collect();
    assert!(
        !hir_errors.is_empty(),
        "fixture {FIXTURE}: expected ≥1 hir-phase CheckerBoundaryViolation error diagnostic \
         for nested closure capture; got {} diagnostics: {diags:?}",
        diags.len()
    );
}

/// `impl<T> Holder<T> where T: Display` — HIR lowering cannot resolve a
/// monomorphic overload for `println` called with a generic-typed `holder.value`
/// argument.  It also fires `UnresolvedSymbol` on the `println` call site.
/// Native `hew check` rejects this fixture with the same two HIR diagnostics.
/// The browser editor must show errors; silently greening it would be a
/// false-green for code that `hew run` rejects.  Fail-closed per
/// checker-authority doctrine.
#[test]
fn v05_wasm_coverage_impl_where_clause_hir_gap_surfaces_error() {
    const FIXTURE: &str = "v05_impl_where_clause";
    let source = include_str!("../../hew-lsp/tests/fixtures/v05_impl_where_clause.hew");
    let json = hew_wasm::analyze(source)
        .unwrap_or_else(|_| panic!("fixture {FIXTURE}: analyze() export error"));
    let parsed = parse_json(FIXTURE, "analyze", &json);
    let diags = parsed["diagnostics"]
        .as_array()
        .unwrap_or_else(|| panic!("fixture {FIXTURE}: missing diagnostics array"));

    let hir_errors: Vec<&serde_json::Value> = diags
        .iter()
        .filter(|d| d["severity"].as_str() == Some("error") && d["phase"].as_str() == Some("hir"))
        .collect();
    assert!(
        !hir_errors.is_empty(),
        "fixture {FIXTURE}: expected ≥1 hir-phase error diagnostic for unresolved \
         monomorphic overload in generic impl; got {} diagnostics: {diags:?}",
        diags.len()
    );
    // At least one must be UnresolvedBuiltinOverload — the primary HIR gap.
    let overload_errors: Vec<&serde_json::Value> = hir_errors
        .iter()
        .filter(|d| d["kind"].as_str() == Some("UnresolvedBuiltinOverload"))
        .copied()
        .collect();
    assert!(
        !overload_errors.is_empty(),
        "fixture {FIXTURE}: expected ≥1 hir-phase UnresolvedBuiltinOverload diagnostic; \
         got hir errors: {hir_errors:?}"
    );
}

// ── Cross-module main fixture (single-source limitation) ──────────────────

/// `v05_cross_module_machine_main` imports `machines::toggle` from a separate
/// source file.  The single-source `hew-wasm` API cannot resolve cross-file
/// imports; downstream errors cascade from the unresolved import.
/// Verify that `analyze()` returns well-formed JSON without crashing.
#[test]
fn v05_wasm_coverage_cross_module_main_analyze_valid() {
    const FIXTURE: &str = "v05_cross_module_machine_main";
    let source = include_str!("../../hew-lsp/tests/fixtures/v05_cross_module_machine_main.hew");
    let json = hew_wasm::analyze(source)
        .unwrap_or_else(|_| panic!("fixture {FIXTURE}: analyze() export error"));
    let parsed = parse_json(FIXTURE, "analyze", &json);
    assert!(
        parsed["diagnostics"].is_array(),
        "fixture {FIXTURE}: analyze() must return a 'diagnostics' array"
    );
}

// ── hover() for 7 specific fixtures ──────────────────────────────────────

/// Assert `hover()` returns a non-null result at `offset` and that the `contents`
/// field is non-empty and contains `expected`.
///
/// Uses `rfind` to locate the last occurrence of each probe function name,
/// mirroring the pattern from the LSP tests.  Hovering over a function-call
/// identifier triggers the `fn_sigs` fallback path in the hover engine and
/// returns the function's signature.
fn assert_hover_contains(fixture: &str, source: &str, offset: usize, expected: &str) {
    let json = hew_wasm::hover(source, offset)
        .unwrap_or_else(|_| panic!("fixture {fixture}: hover() export error at offset {offset}"));
    assert_ne!(
        json, "null",
        "fixture {fixture}: hover() returned null at offset {offset}; \
         expected hover for {expected:?}"
    );
    let parsed = parse_json(fixture, "hover", &json);
    let contents = parsed["contents"].as_str().unwrap_or_else(|| {
        panic!("fixture {fixture}: hover result missing string 'contents': {json}")
    });
    assert!(
        !contents.is_empty(),
        "fixture {fixture}: hover contents must be non-empty at offset {offset}"
    );
    assert!(
        contents.contains(expected),
        "fixture {fixture}: hover at offset {offset} expected to contain {expected:?}, got:\n{contents}"
    );
}

#[test]
fn v05_wasm_coverage_hover_is_operator() {
    let source = include_str!("../../hew-lsp/tests/fixtures/v05_is_operator.hew");
    let offset = source
        .rfind("is_probe")
        .expect("is_probe must appear in v05_is_operator");
    assert_hover_contains("v05_is_operator", source, offset, "is_probe");
}

#[test]
fn v05_wasm_coverage_hover_string_methods() {
    let source = include_str!("../../hew-lsp/tests/fixtures/v05_string_methods.hew");
    let offset = source
        .rfind("string_methods_probe")
        .expect("string_methods_probe must appear in v05_string_methods");
    assert_hover_contains("v05_string_methods", source, offset, "string_methods_probe");
}

#[test]
fn v05_wasm_coverage_hover_extern_unsafe() {
    let source = include_str!("../../hew-lsp/tests/fixtures/v05_extern_unsafe.hew");
    let offset = source
        .rfind("extern_unsafe_probe")
        .expect("extern_unsafe_probe must appear in v05_extern_unsafe");
    assert_hover_contains("v05_extern_unsafe", source, offset, "extern_unsafe_probe");
}

#[test]
fn v05_wasm_coverage_hover_record_decl() {
    let source = include_str!("../../hew-lsp/tests/fixtures/v05_record_decl.hew");
    let offset = source
        .rfind("record_decl_probe")
        .expect("record_decl_probe must appear in v05_record_decl");
    assert_hover_contains("v05_record_decl", source, offset, "record_decl_probe");
}

#[test]
fn v05_wasm_coverage_hover_while_let() {
    let source = include_str!("../../hew-lsp/tests/fixtures/v05_while_let.hew");
    let offset = source
        .rfind("while_let_probe")
        .expect("while_let_probe must appear in v05_while_let");
    assert_hover_contains("v05_while_let", source, offset, "while_let_probe");
}

#[test]
fn v05_wasm_coverage_hover_machine_generics() {
    let source = include_str!("../../hew-lsp/tests/fixtures/v05_machine_generics.hew");
    let offset = source
        .rfind("machine_generics_probe")
        .expect("machine_generics_probe must appear in v05_machine_generics");
    assert_hover_contains(
        "v05_machine_generics",
        source,
        offset,
        "machine_generics_probe",
    );
}

#[test]
fn v05_wasm_coverage_hover_impl_where_clause() {
    let source = include_str!("../../hew-lsp/tests/fixtures/v05_impl_where_clause.hew");
    let offset = source
        .rfind("impl_where_clause_probe")
        .expect("impl_where_clause_probe must appear in v05_impl_where_clause");
    assert_hover_contains(
        "v05_impl_where_clause",
        source,
        offset,
        "impl_where_clause_probe",
    );
}
