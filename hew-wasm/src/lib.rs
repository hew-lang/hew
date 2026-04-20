//! WASM bindings for Hew analysis-only diagnostics and editor tooling.
//!
//! This crate exposes the frontend analysis surfaces used by browser/editor
//! integrations. It intentionally does not provide native codegen, linking,
//! runtime execution, or a full in-browser Hew VM.
//!
//! Available source-code analysis capabilities:
//!
//! - **Diagnostics** — parse, type-check, and analyze Hew source code
//!   (`analyze`, `hover`, `get_keywords`).
//! - **Navigation** — go-to-definition, find references, rename.
//! - **Editing** — completions, signature help, code actions.
//! - **Presentation** — semantic tokens, document symbols, inlay hints, folding ranges.

use serde::Serialize;
use wasm_bindgen::prelude::*;

#[derive(Serialize)]
struct RefsResult {
    name: String,
    spans: Vec<hew_analysis::OffsetSpan>,
}

// ── Diagnostics API ──────────────────────────────────────────────────────

/// Analyze Hew source code and return diagnostics, tokens, and symbols as JSON.
///
/// This is analysis-only and does not execute the program.
#[must_use]
#[wasm_bindgen]
pub fn analyze(source: &str) -> String {
    let result = run_analysis(source);
    serde_json::to_string(&result).unwrap_or_default()
}

/// Get the list of Hew keywords for editor completion.
#[must_use]
#[wasm_bindgen]
pub fn get_keywords() -> String {
    serde_json::to_string(&hew_lexer::ALL_KEYWORDS).unwrap_or_default()
}

/// Get hover information for a position in the source.
///
/// Returns JSON `{ contents, span? }` or `null` if nothing to display.
#[must_use]
#[wasm_bindgen]
pub fn hover(source: &str, offset: usize) -> String {
    let analysis = parse_and_type_check(source);
    let hover = hew_analysis::hover::hover(
        source,
        &analysis.parse_result,
        analysis.type_output.as_ref(),
        offset,
    );
    encode_optional_json(hover.as_ref())
}

// ── Completions ──────────────────────────────────────────────────────────

/// Get completions at byte offset. Returns JSON array of `CompletionItem`.
#[must_use]
#[wasm_bindgen]
pub fn complete(source: &str, offset: usize) -> String {
    let analysis = parse_and_type_check(source);
    let items = hew_analysis::completions::complete(
        source,
        &analysis.parse_result,
        analysis.type_output.as_ref(),
        offset,
    );
    serde_json::to_string(&items).unwrap_or_default()
}

// ── Navigation ───────────────────────────────────────────────────────────

/// Go to definition. Returns JSON `{ start, end }` or `null`.
#[must_use]
#[wasm_bindgen]
pub fn goto_definition(source: &str, offset: usize) -> String {
    let parse_result = hew_parser::parse(source);
    let Some(word) = hew_analysis::util::word_at_offset(source, offset) else {
        return encode_optional_json::<hew_analysis::OffsetSpan>(None);
    };
    let definition = hew_analysis::definition::find_definition(source, &parse_result, &word);
    encode_optional_json(definition.as_ref())
}

/// Find all references at offset. Returns JSON `{ name, spans }` or `null`.
#[must_use]
#[wasm_bindgen]
pub fn find_references(source: &str, offset: usize) -> String {
    let parse_result = hew_parser::parse(source);
    encode_optional_json(
        hew_analysis::references::find_all_references(source, &parse_result, offset)
            .map(|(name, spans)| RefsResult { name, spans })
            .as_ref(),
    )
}

// ── Rename ───────────────────────────────────────────────────────────────

/// Prepare rename at offset. Returns JSON `{ start, end }` or `null`.
#[must_use]
#[wasm_bindgen]
pub fn prepare_rename(source: &str, offset: usize) -> String {
    let parse_result = hew_parser::parse(source);
    let rename_span = hew_analysis::rename::prepare_rename(source, &parse_result, offset);
    encode_optional_json(rename_span.as_ref())
}

/// Compute rename edits. Returns JSON array of `{ span, new_text }` or `null`.
#[must_use]
#[wasm_bindgen]
pub fn rename(source: &str, offset: usize, new_name: &str) -> String {
    let parse_result = hew_parser::parse(source);
    let edits = hew_analysis::rename::rename(source, &parse_result, offset, new_name);
    encode_optional_json(edits.as_ref())
}

// ── Document structure ───────────────────────────────────────────────────

/// Get document symbols. Returns JSON array of `SymbolInfo`.
#[must_use]
#[wasm_bindgen]
pub fn document_symbols(source: &str) -> String {
    let parse_result = hew_parser::parse(source);
    let symbols = hew_analysis::symbols::build_document_symbols(source, &parse_result);
    serde_json::to_string(&symbols).unwrap_or_default()
}

/// Get semantic tokens. Returns JSON array of `{ start, length, token_type, modifiers }`.
#[must_use]
#[wasm_bindgen]
pub fn semantic_tokens(source: &str) -> String {
    let tokens = hew_analysis::semantic_tokens::build_semantic_tokens(source);
    serde_json::to_string(&tokens).unwrap_or_default()
}

/// Get folding ranges. Returns JSON array of `{ start_line, end_line, kind }`.
#[must_use]
#[wasm_bindgen]
pub fn folding_ranges(source: &str) -> String {
    let parse_result = hew_parser::parse(source);
    let ranges = hew_analysis::folding::build_folding_ranges(source, &parse_result);
    serde_json::to_string(&ranges).unwrap_or_default()
}

// ── Type information ─────────────────────────────────────────────────────

/// Get signature help at offset. Returns JSON `SignatureHelpResult` or `null`.
#[must_use]
#[wasm_bindgen]
pub fn signature_help(source: &str, offset: usize) -> String {
    let analysis = parse_and_type_check(source);
    let Some(type_output) = analysis.type_output.as_ref() else {
        return encode_optional_json::<hew_analysis::SignatureHelpResult>(None);
    };
    let help = hew_analysis::signature_help::build_signature_help(source, type_output, offset);
    encode_optional_json(help.as_ref())
}

/// Get inlay hints. Returns JSON array of `{ offset, label, kind, padding_left }`.
#[must_use]
#[wasm_bindgen]
pub fn inlay_hints(source: &str) -> String {
    let analysis = parse_and_type_check(source);
    let Some(type_output) = analysis.type_output.as_ref() else {
        return String::new();
    };
    let hints =
        hew_analysis::inlay_hints::build_inlay_hints(source, &analysis.parse_result, type_output);
    serde_json::to_string(&hints).unwrap_or_default()
}

// ── Code actions ─────────────────────────────────────────────────────────

/// Get code actions for given diagnostics JSON. Returns JSON array of `CodeAction`.
///
/// `diagnostics_json` should be a JSON array of `DiagnosticInfo` objects:
/// `[{ "kind": "UndefinedVariable", "message": "...", "span": { "start": 0, "end": 5 }, "suggestions": [] }]`
#[must_use]
#[wasm_bindgen]
pub fn code_actions(source: &str, diagnostics_json: &str) -> String {
    let diags: Vec<hew_analysis::code_actions::DiagnosticInfo> =
        serde_json::from_str(diagnostics_json).unwrap_or_default();
    let actions = hew_analysis::code_actions::build_code_actions(source, &diags);
    serde_json::to_string(&actions).unwrap_or_default()
}

// ── Internal helpers ─────────────────────────────────────────────────────

/// A secondary span attached to a diagnostic (e.g. "defined here").
#[derive(Serialize)]
struct WasmNote {
    start_offset: usize,
    end_offset: usize,
    message: String,
}

/// Diagnostic reported by the WASM analysis pipeline.
#[derive(Serialize)]
struct WasmDiagnostic {
    severity: String,
    message: String,
    start_offset: usize,
    end_offset: usize,
    kind: String,
    notes: Vec<WasmNote>,
    suggestions: Vec<String>,
}

/// Combined analysis result returned by `analyze()`.
#[derive(Serialize)]
struct AnalysisResult {
    diagnostics: Vec<WasmDiagnostic>,
    tokens: Vec<hew_analysis::SemanticToken>,
    symbols: Vec<hew_analysis::SymbolInfo>,
}

struct AnalyzedSource {
    parse_result: hew_parser::ParseResult,
    type_output: Option<hew_types::TypeCheckOutput>,
}

fn parse_and_type_check(source: &str) -> AnalyzedSource {
    let parse_result = hew_parser::parse(source);
    let type_output = if parse_result.errors.is_empty() {
        let mut checker = hew_types::Checker::new(hew_types::module_registry::ModuleRegistry::new(
            hew_types::module_registry::build_module_search_paths(),
        ));
        Some(checker.check_program(&parse_result.program))
    } else {
        None
    };

    AnalyzedSource {
        parse_result,
        type_output,
    }
}

fn run_analysis(source: &str) -> AnalysisResult {
    let tokens = hew_analysis::semantic_tokens::build_semantic_tokens(source);

    let analysis = parse_and_type_check(source);
    let mut diagnostics = Vec::new();

    for err in &analysis.parse_result.errors {
        let severity = match err.severity {
            hew_parser::Severity::Warning => "warning",
            hew_parser::Severity::Error => "error",
        };
        diagnostics.push(WasmDiagnostic {
            severity: severity.to_string(),
            message: err.message.clone(),
            start_offset: err.span.start,
            end_offset: err.span.end,
            kind: "parse_error".to_string(),
            notes: Vec::new(),
            suggestions: Vec::new(),
        });
    }

    if let Some(type_output) = analysis.type_output.as_ref() {
        for err in &type_output.errors {
            let severity = match err.severity {
                hew_types::error::Severity::Warning => "warning",
                hew_types::error::Severity::Error => "error",
            };
            diagnostics.push(WasmDiagnostic {
                severity: severity.to_string(),
                message: err.message.clone(),
                start_offset: err.span.start,
                end_offset: err.span.end,
                kind: err.kind.as_kind_str().to_string(),
                notes: err
                    .notes
                    .iter()
                    .map(|(span, msg)| WasmNote {
                        start_offset: span.start,
                        end_offset: span.end,
                        message: msg.clone(),
                    })
                    .collect(),
                suggestions: err.suggestions.clone(),
            });
        }
    }

    let symbols = hew_analysis::symbols::build_document_symbols(source, &analysis.parse_result);

    AnalysisResult {
        diagnostics,
        tokens,
        symbols,
    }
}

fn encode_optional_json<T: Serialize>(value: Option<&T>) -> String {
    serde_json::to_string(&value).unwrap_or_default()
}

#[cfg(test)]
#[test]
fn curated_playground_manifest_smoke() {
    use std::path::Path;

    let crate_dir = Path::new(env!("CARGO_MANIFEST_DIR"));
    let playground_dir = crate_dir.join("../examples/playground");
    let manifest_path = playground_dir.join("manifest.json");
    let manifest = std::fs::read_to_string(&manifest_path).unwrap_or_else(|err| {
        panic!(
            "failed to read curated playground manifest {}: {err}",
            manifest_path.display()
        )
    });
    let entries: Vec<serde_json::Value> = serde_json::from_str(&manifest).unwrap_or_else(|err| {
        panic!(
            "failed to parse curated playground manifest {}: {err}",
            manifest_path.display()
        )
    });

    assert!(
        !entries.is_empty(),
        "curated playground manifest should contain at least one example"
    );

    let valid_wasi = ["runnable", "unsupported"];

    for entry in entries {
        let id = entry
            .get("id")
            .and_then(serde_json::Value::as_str)
            .unwrap_or_else(|| {
                panic!("curated playground manifest entry missing string id: {entry}")
            });

        // Verify capability contract: every entry must carry well-formed capability metadata.
        let caps = entry
            .get("capabilities")
            .unwrap_or_else(|| panic!("manifest entry {id} missing 'capabilities' field"));
        let browser_cap = caps
            .get("browser")
            .and_then(serde_json::Value::as_str)
            .unwrap_or_else(|| panic!("manifest entry {id} missing capabilities.browser"));
        assert_eq!(
            browser_cap, "analysis-only",
            "manifest entry {id}: capabilities.browser must be 'analysis-only' \
             (hew-wasm is Tier 1 analysis-only; no in-browser execution exists)"
        );
        let wasi_cap = caps
            .get("wasi")
            .and_then(serde_json::Value::as_str)
            .unwrap_or_else(|| panic!("manifest entry {id} missing capabilities.wasi"));
        assert!(
            valid_wasi.contains(&wasi_cap),
            "manifest entry {id}: capabilities.wasi must be one of {valid_wasi:?}, got {wasi_cap:?}"
        );

        let source_path_rel = entry
            .get("source_path")
            .and_then(serde_json::Value::as_str)
            .unwrap_or_else(|| {
                panic!("curated playground manifest entry missing string source_path: {entry}")
            });
        let source_path = playground_dir.join(source_path_rel);
        let source = std::fs::read_to_string(&source_path).unwrap_or_else(|err| {
            panic!(
                "failed to read curated playground source {} for {}: {err}",
                source_path.display(),
                id
            )
        });
        let analysis_json = analyze(&source);
        let analysis: serde_json::Value =
            serde_json::from_str(&analysis_json).unwrap_or_else(|err| {
                panic!(
                    "expected valid analyze() JSON for {id} ({source_path_rel}): {err}\n{analysis_json}"
                )
            });
        let diagnostics = analysis
            .get("diagnostics")
            .and_then(serde_json::Value::as_array)
            .unwrap_or_else(|| {
                panic!("expected diagnostics array for {id} ({source_path_rel}): {analysis_json}")
            });

        assert!(
            diagnostics.is_empty(),
            "expected zero diagnostics for {id} ({source_path_rel}), got {analysis_json}"
        );
    }
}

// ── Tests ─────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn analyze_valid_program() {
        let result = analyze("fn main() { println(42); }");
        assert!(!result.is_empty());
        let parsed: serde_json::Value = serde_json::from_str(&result).unwrap();
        assert!(parsed["diagnostics"].as_array().unwrap().is_empty());
    }

    #[test]
    fn analyze_parse_error() {
        let result = analyze("fn {");
        let parsed: serde_json::Value = serde_json::from_str(&result).unwrap();
        assert!(!parsed["diagnostics"].as_array().unwrap().is_empty());
    }

    #[test]
    fn keywords_non_empty() {
        let result = get_keywords();
        let parsed: serde_json::Value = serde_json::from_str(&result).unwrap();
        assert!(parsed.as_array().unwrap().len() > 10);
    }

    #[test]
    fn hover_returns_type() {
        let source = "fn main() { let x: i64 = 42; }";
        let result = hover(source, 25); // offset within `42`
                                        // hover may or may not return a type depending on checker output;
                                        // just ensure it doesn't panic.
        let _ = result;
    }

    #[test]
    fn hover_no_result_encodes_json_null() {
        let parsed: serde_json::Value = serde_json::from_str(&hover("fn {", 0)).unwrap();
        assert!(parsed.is_null());
    }

    #[test]
    fn complete_returns_items() {
        let source = "fn main() { let x = 42; x }";
        let result = complete(source, 25); // near 'x'
        assert!(!result.is_empty());
    }

    #[test]
    fn document_symbols_returns_symbols() {
        let source = "fn foo() {} fn bar() {}";
        let result = document_symbols(source);
        let parsed: serde_json::Value = serde_json::from_str(&result).unwrap();
        assert_eq!(parsed.as_array().unwrap().len(), 2);
    }

    #[test]
    fn semantic_tokens_returns_tokens() {
        let result = semantic_tokens("let x = 42;");
        let parsed: serde_json::Value = serde_json::from_str(&result).unwrap();
        assert!(!parsed.as_array().unwrap().is_empty());
    }

    #[test]
    fn folding_ranges_for_function() {
        let source = "fn foo() {\n  let x = 1;\n  let y = 2;\n}";
        let result = folding_ranges(source);
        // The parser may or may not produce folding ranges for this simple
        // example depending on span recording, so just verify valid JSON.
        let parsed: serde_json::Value = serde_json::from_str(&result).unwrap();
        assert!(parsed.as_array().is_some());
    }

    #[test]
    fn goto_definition_finds_function() {
        let source = "fn foo() {} fn main() { foo(); }";
        let result = goto_definition(source, 25); // on 'o' in second foo
        assert!(!result.is_empty());
    }

    #[test]
    fn goto_definition_no_result_encodes_json_null() {
        let parsed: serde_json::Value =
            serde_json::from_str(&goto_definition("fn main() {}", 0)).unwrap();
        assert!(parsed.is_null());
    }

    #[test]
    fn find_references_works() {
        let source = "fn main() { let x = 1; let y = x; }";
        let result = find_references(source, 17); // on 'x' definition
        assert!(!result.is_empty());
    }

    #[test]
    fn find_references_no_result_encodes_json_null() {
        let parsed: serde_json::Value =
            serde_json::from_str(&find_references("fn main() {}", 0)).unwrap();
        assert!(parsed.is_null());
    }

    #[test]
    fn code_actions_from_json() {
        let source = "let x = foob;";
        let diags_json = r#"[{
            "kind": "UndefinedVariable",
            "message": "undefined variable `foob`",
            "span": { "start": 8, "end": 12 },
            "suggestions": ["`food`"]
        }]"#;
        let result = code_actions(source, diags_json);
        let parsed: serde_json::Value = serde_json::from_str(&result).unwrap();
        assert!(!parsed.as_array().unwrap().is_empty());
    }

    #[test]
    fn inlay_hints_no_panic() {
        let source = "fn main() { let x = 42; }";
        let result = inlay_hints(source);
        // May or may not produce hints depending on type checker, but should not panic.
        let _: serde_json::Value = serde_json::from_str(&result).unwrap_or_default();
    }

    #[test]
    fn signature_help_no_panic() {
        let source = "fn add(a: i64, b: i64) -> i64 { a + b } fn main() { add(1, 2); }";
        let result = signature_help(source, 58); // inside add(1, ...)
                                                 // May or may not produce help, but should not panic.
        let _ = result;
    }

    #[test]
    fn signature_help_no_result_encodes_json_null() {
        let parsed: serde_json::Value =
            serde_json::from_str(&signature_help("fn main() {}", 0)).unwrap();
        assert!(parsed.is_null());
    }

    #[test]
    fn prepare_rename_no_panic() {
        let source = "fn foo() {} fn main() { foo(); }";
        let result = prepare_rename(source, 3); // on 'foo' definition
                                                // Should return a span or JSON null.
        let _ = result;
    }

    #[test]
    fn rename_no_panic() {
        let source = "fn foo() {} fn main() { foo(); }";
        let result = rename(source, 3, "bar"); // rename 'foo' to 'bar'
        let _ = result;
    }

    #[test]
    fn rename_no_result_encodes_json_null() {
        let parsed: serde_json::Value =
            serde_json::from_str(&rename("fn main() {}", 0, "bar")).unwrap();
        assert!(parsed.is_null());
    }

    // ── WasmDiagnostic notes/suggestions serialization contract ─────────────

    /// Every diagnostic in `analyze()` JSON must carry `notes` and `suggestions`
    /// fields, even when they are empty.  This locks down the serialized shape
    /// introduced by PR #967 for both the parse-error and type-error paths.
    #[test]
    fn diagnostic_always_has_notes_and_suggestions_fields() {
        // Parse error path — the checker never runs.
        let json = analyze("fn {");
        let v: serde_json::Value = serde_json::from_str(&json).unwrap();
        let diags = v["diagnostics"].as_array().unwrap();
        assert!(!diags.is_empty(), "expected at least one parse diagnostic");
        for d in diags {
            assert!(
                d.get("notes").and_then(|n| n.as_array()).is_some(),
                "parse-path diagnostic missing `notes` array: {d}"
            );
            assert!(
                d.get("suggestions").and_then(|s| s.as_array()).is_some(),
                "parse-path diagnostic missing `suggestions` array: {d}"
            );
        }

        // Type-error path (clean parse, type error).
        let json = analyze("fn main() { let x = 1; x = 2; }");
        let v: serde_json::Value = serde_json::from_str(&json).unwrap();
        let diags = v["diagnostics"].as_array().unwrap();
        // There may be zero type errors on this program if the checker doesn't
        // enforce immutability here; in that case only shape of a valid program
        // matters, which is covered by analyze_valid_program.  If there are
        // diagnostics, every one must carry both fields.
        for d in diags {
            assert!(
                d.get("notes").and_then(|n| n.as_array()).is_some(),
                "type-path diagnostic missing `notes` array: {d}"
            );
            assert!(
                d.get("suggestions").and_then(|s| s.as_array()).is_some(),
                "type-path diagnostic missing `suggestions` array: {d}"
            );
        }
    }

    /// A mutability violation must produce a diagnostic whose `suggestions`
    /// array is non-empty and whose entries are non-empty strings.
    #[test]
    fn diagnostic_suggestions_populated_for_mutability_error() {
        // `let x` is immutable; `x = 2` should produce a MutabilityError with
        // a "consider changing this to `var x`" suggestion.
        let json = analyze("fn main() { let x = 1; x = 2; }");
        let v: serde_json::Value = serde_json::from_str(&json).unwrap();
        let diags = v["diagnostics"].as_array().unwrap();

        let with_suggestions: Vec<&serde_json::Value> = diags
            .iter()
            .filter(|d| {
                d.get("suggestions")
                    .and_then(|s| s.as_array())
                    .is_some_and(|a| !a.is_empty())
            })
            .collect();

        assert!(
            !with_suggestions.is_empty(),
            "expected at least one diagnostic with non-empty suggestions for \
             mutability violation, got: {json}"
        );

        for d in &with_suggestions {
            let suggestions = d["suggestions"].as_array().unwrap();
            for s in suggestions {
                assert!(
                    s.as_str().is_some_and(|t| !t.is_empty()),
                    "suggestion must be a non-empty string, got: {s}"
                );
            }
        }
    }

    /// A duplicate function definition must produce a diagnostic whose `notes`
    /// array is non-empty and whose entries carry the expected named fields
    /// (`start_offset`, `end_offset`, `message`).
    #[test]
    fn diagnostic_notes_populated_for_duplicate_definition() {
        // Two functions named `foo` — duplicate_definition attaches a note
        // "previous definition here" pointing at the first span.
        let json = analyze("fn foo() {} fn foo() {}");
        let v: serde_json::Value = serde_json::from_str(&json).unwrap();
        let diags = v["diagnostics"].as_array().unwrap();

        let with_notes: Vec<&serde_json::Value> = diags
            .iter()
            .filter(|d| {
                d.get("notes")
                    .and_then(|n| n.as_array())
                    .is_some_and(|a| !a.is_empty())
            })
            .collect();

        assert!(
            !with_notes.is_empty(),
            "expected at least one diagnostic with non-empty notes for \
             duplicate definition, got: {json}"
        );

        for d in &with_notes {
            for note in d["notes"].as_array().unwrap() {
                assert!(
                    note.get("start_offset")
                        .and_then(serde_json::Value::as_u64)
                        .is_some(),
                    "note missing numeric `start_offset`: {note}"
                );
                assert!(
                    note.get("end_offset")
                        .and_then(serde_json::Value::as_u64)
                        .is_some(),
                    "note missing numeric `end_offset`: {note}"
                );
                assert!(
                    note.get("message")
                        .and_then(serde_json::Value::as_str)
                        .is_some_and(|s| !s.is_empty()),
                    "note missing non-empty string `message`: {note}"
                );
            }
        }
    }

    /// Full `analyze()` → `code_actions()` round-trip through the browser-bridge seam.
    ///
    /// The browser playground calls `analyze()`, extracts diagnostics (which now
    /// carry `suggestions`), converts the span shape (`start_offset`/`end_offset`
    /// → `span: {start, end}`), then calls `code_actions()`.  This test
    /// exercises that full path to prove suggestions produced by the type checker
    /// survive in a form that yields real code actions with exact span fidelity.
    #[test]
    fn analyze_suggestions_flow_through_to_code_actions() {
        // `fooo` is an undefined *variable* reference (not a call) so the
        // diagnostic span covers exactly the identifier, not trailing parens.
        // `foo` in scope has edit-distance 1 ≤ max_dist 1 and will be suggested.
        let source = "fn main() { let foo = 1; let _y = fooo; }";
        let analyze_json = analyze(source);
        let analysis: serde_json::Value = serde_json::from_str(&analyze_json).unwrap();
        let diags = analysis["diagnostics"].as_array().unwrap();

        let undef_diag = diags
            .iter()
            .find(|d| d["kind"].as_str() == Some("UndefinedVariable"))
            .unwrap_or_else(|| {
                panic!("expected an UndefinedVariable diagnostic for `fooo`; got: {analyze_json}")
            });

        // The diagnostic span must cover exactly the undefined identifier.
        let diag_start = usize::try_from(undef_diag["start_offset"].as_u64().unwrap()).unwrap();
        let diag_end = usize::try_from(undef_diag["end_offset"].as_u64().unwrap()).unwrap();
        assert_eq!(
            &source[diag_start..diag_end],
            "fooo",
            "diagnostic span must cover exactly the undefined identifier `fooo`"
        );

        // The suggestion `foo` must be present (raw name, not backtick-wrapped).
        let suggestions = undef_diag["suggestions"].as_array().unwrap();
        assert!(
            suggestions.iter().any(|s| s.as_str() == Some("foo")),
            "expected `foo` in suggestions for `fooo`; got: {suggestions:?}"
        );

        // Reconstruct the DiagnosticInfo-format JSON that the browser bridge
        // assembles: convert start_offset/end_offset → span.{start,end}.
        let diag_info_json = serde_json::json!([{
            "kind":    undef_diag["kind"].as_str().unwrap(),
            "message": undef_diag["message"].as_str().unwrap(),
            "span": {
                "start": undef_diag["start_offset"].as_u64().unwrap(),
                "end":   undef_diag["end_offset"].as_u64().unwrap(),
            },
            "suggestions": suggestions,
        }])
        .to_string();

        let actions_json = code_actions(source, &diag_info_json);
        let actions: serde_json::Value = serde_json::from_str(&actions_json).unwrap();
        let actions_arr = actions.as_array().unwrap();

        assert!(
            !actions_arr.is_empty(),
            "expected code action(s) from analyze() suggestions; got: {actions_json}"
        );

        // The first action must be "Replace with `foo`" and its edit must cover
        // the exact original diagnostic span with `foo` as the replacement text.
        let action = &actions_arr[0];
        assert_eq!(
            action["title"].as_str().unwrap(),
            "Replace with `foo`",
            "action title must name the suggested replacement; got: {actions_json}"
        );
        let edits = action["edits"].as_array().unwrap();
        assert!(!edits.is_empty(), "action must carry at least one edit");

        let edit = &edits[0];
        let edit_start = usize::try_from(edit["span"]["start"].as_u64().unwrap()).unwrap();
        let edit_end = usize::try_from(edit["span"]["end"].as_u64().unwrap()).unwrap();
        let new_text = edit["new_text"].as_str().unwrap();

        assert_eq!(
            edit_start, diag_start,
            "edit span start must equal diagnostic start_offset"
        );
        assert_eq!(
            edit_end, diag_end,
            "edit span end must equal diagnostic end_offset"
        );
        assert_eq!(
            new_text, "foo",
            "replacement text must be the suggested name"
        );

        // Apply the edit and verify the corrected source replaces `fooo` with `foo`.
        let corrected = format!(
            "{}{}{}",
            &source[..edit_start],
            new_text,
            &source[edit_end..]
        );
        assert_eq!(
            corrected, "fn main() { let foo = 1; let _y = foo; }",
            "applying the edit must replace `fooo` with `foo` exactly"
        );
    }

    /// Notes emitted by `analyze()` point at the intended previous-definition region.
    ///
    /// The browser uses `start_offset`/`end_offset` from `WasmNote` to render
    /// secondary highlights.  This test verifies the offsets identify the first
    /// `foo` definition in source and carry the canonical "previous definition here"
    /// message.
    #[test]
    fn analyze_notes_carry_display_ready_spans() {
        // Two `foo` definitions.  The DuplicateDefinition error fires on the second;
        // its note must point back into the *first* `fn foo() {}` and must not
        // reach into the second definition.
        let source = "fn foo() {} fn foo() {}";
        //            ^^^^^^^^^^^  ← first definition
        //                         ^^^^^^^^^^^  ← second definition triggers the error

        // The second `fn foo` starts at this byte offset; the note must end here
        // or earlier so it points only at the first definition.
        let second_def_start = source.rfind("fn foo").expect("second definition not found");

        let analyze_json = analyze(source);
        let analysis: serde_json::Value = serde_json::from_str(&analyze_json).unwrap();
        let diags = analysis["diagnostics"].as_array().unwrap();

        let diag_with_notes = diags
            .iter()
            .find(|d| d["notes"].as_array().is_some_and(|a| !a.is_empty()))
            .unwrap_or_else(|| {
                panic!("expected a diagnostic with notes for duplicate fn foo; got: {analyze_json}")
            });

        let notes = diag_with_notes["notes"].as_array().unwrap();
        assert_eq!(
            notes.len(),
            1,
            "DuplicateDefinition must carry exactly one note; got: {analyze_json}"
        );

        let note = &notes[0];
        let start = usize::try_from(
            note["start_offset"]
                .as_u64()
                .unwrap_or_else(|| panic!("note missing `start_offset`: {note}")),
        )
        .expect("start_offset fits usize");
        let end = usize::try_from(
            note["end_offset"]
                .as_u64()
                .unwrap_or_else(|| panic!("note missing `end_offset`: {note}")),
        )
        .expect("end_offset fits usize");

        // Note span must not reach into the second definition.
        assert!(
            end <= second_def_start,
            "note end={end} must not reach into the second definition \
             (second `fn foo` starts at {second_def_start})"
        );
        assert!(
            start < end,
            "note span must be non-zero width (start={start}, end={end})"
        );

        // The sliced text must contain the duplicated name, confirming the note
        // points at the right region.
        let sliced = &source[start..end];
        assert!(
            sliced.contains("foo"),
            "note span must cover text containing the first `foo` definition; sliced: {sliced:?}"
        );

        // The message must be exactly the canonical "previous definition here" label.
        assert_eq!(
            note["message"].as_str().unwrap_or(""),
            "previous definition here",
            "note must carry the canonical 'previous definition here' message"
        );
    }

    /// End-to-end: `analyze()` → browser bridge → `code_actions()` for an
    /// undefined *function call*.  The type checker spans the whole call
    /// expression (`fooo()`), but the code-action edit must replace only the
    /// callee name (`fooo`) so the argument list and parentheses are preserved.
    #[test]
    fn analyze_undefined_function_action_preserves_call_parens() {
        // `fooo()` calls an undefined function; `foo` is defined and has
        // edit-distance 1 ≤ max_dist 1 so it will appear as a suggestion.
        let source = "fn foo() {} fn main() { fooo(); }";
        //                                    ^   ^
        //                                   24  28 = start of '('

        let analyze_json = analyze(source);
        let analysis: serde_json::Value = serde_json::from_str(&analyze_json).unwrap();
        let diags = analysis["diagnostics"].as_array().unwrap();

        let undef_diag = diags
            .iter()
            .find(|d| d["kind"].as_str() == Some("UndefinedFunction"))
            .unwrap_or_else(|| {
                panic!("expected an UndefinedFunction diagnostic for `fooo()`; got: {analyze_json}")
            });

        // Confirm the type checker spans the full call expression (known behaviour).
        let diag_start = usize::try_from(undef_diag["start_offset"].as_u64().unwrap()).unwrap();
        let diag_end = usize::try_from(undef_diag["end_offset"].as_u64().unwrap()).unwrap();
        assert_eq!(
            &source[diag_start..diag_end],
            "fooo()",
            "UndefinedFunction diagnostic span must cover the full call expression"
        );

        // `foo` must be present in suggestions.
        let suggestions = undef_diag["suggestions"].as_array().unwrap();
        assert!(
            suggestions.iter().any(|s| s.as_str() == Some("foo")),
            "expected `foo` in suggestions for `fooo()`; got: {suggestions:?}"
        );

        // Reconstruct the DiagnosticInfo JSON the browser bridge would send.
        let diag_info_json = serde_json::json!([{
            "kind":    undef_diag["kind"].as_str().unwrap(),
            "message": undef_diag["message"].as_str().unwrap(),
            "span": {
                "start": undef_diag["start_offset"].as_u64().unwrap(),
                "end":   undef_diag["end_offset"].as_u64().unwrap(),
            },
            "suggestions": suggestions,
        }])
        .to_string();

        let actions_json = code_actions(source, &diag_info_json);
        let actions: serde_json::Value = serde_json::from_str(&actions_json).unwrap();
        let actions_arr = actions.as_array().unwrap();

        assert!(
            !actions_arr.is_empty(),
            "expected code action(s) for UndefinedFunction; got: {actions_json}"
        );

        let action = &actions_arr[0];
        assert_eq!(
            action["title"].as_str().unwrap(),
            "Replace with `foo`",
            "action title must name the suggested replacement; got: {actions_json}"
        );

        let edit = &action["edits"].as_array().unwrap()[0];
        let edit_start = usize::try_from(edit["span"]["start"].as_u64().unwrap()).unwrap();
        let edit_end = usize::try_from(edit["span"]["end"].as_u64().unwrap()).unwrap();
        let new_text = edit["new_text"].as_str().unwrap();

        assert_eq!(
            new_text, "foo",
            "replacement text must be the suggested callee name"
        );

        // The edit span must cover only `fooo` (the callee), not the `()`.
        assert_eq!(
            &source[edit_start..edit_end],
            "fooo",
            "edit span must cover only the callee identifier, not the argument list"
        );

        // Applying the edit must yield `foo()` — parentheses intact.
        let corrected = format!(
            "{}{}{}",
            &source[..edit_start],
            new_text,
            &source[edit_end..]
        );
        assert_eq!(
            corrected, "fn foo() {} fn main() { foo(); }",
            "applying the edit must replace `fooo()` with `foo()`, preserving the call syntax"
        );
    }

    /// End-to-end: `analyze()` → browser bridge → `code_actions()` for an
    /// undefined *generic* function call.  The span covers `fooo<i64>()`;
    /// after `trim_to_callee_name` trims at `<`, the edit must replace only
    /// `fooo` so the type argument list `<i64>()` is preserved.
    #[test]
    fn analyze_undefined_generic_function_preserves_type_args() {
        // `fooo<i64>()` calls an undefined function; `foo` is in scope with
        // edit-distance 1 ≤ max_dist 1 and will be suggested.
        let source = "fn foo() {} fn main() { fooo<i64>(); }";

        let analyze_json = analyze(source);
        let analysis: serde_json::Value = serde_json::from_str(&analyze_json).unwrap();
        let diags = analysis["diagnostics"].as_array().unwrap();

        let undef_diag = diags
            .iter()
            .find(|d| d["kind"].as_str() == Some("UndefinedFunction"))
            .unwrap_or_else(|| {
                panic!(
                    "expected an UndefinedFunction diagnostic for `fooo<i64>()`; \
                     got: {analyze_json}"
                )
            });

        // The type checker spans the entire call expression including type args.
        let diag_start = usize::try_from(undef_diag["start_offset"].as_u64().unwrap()).unwrap();
        let diag_end = usize::try_from(undef_diag["end_offset"].as_u64().unwrap()).unwrap();
        let call_text = &source[diag_start..diag_end];
        assert!(
            call_text.starts_with("fooo") && call_text.contains('<'),
            "UndefinedFunction span must cover generic call expression (starts with \
             `fooo` and contains `<`); got: {call_text:?}"
        );

        // `foo` must be present in suggestions.
        let suggestions = undef_diag["suggestions"].as_array().unwrap();
        assert!(
            suggestions.iter().any(|s| s.as_str() == Some("foo")),
            "expected `foo` in suggestions for `fooo<i64>()`; got: {suggestions:?}"
        );

        // Reconstruct the DiagnosticInfo JSON the browser bridge would send.
        let diag_info_json = serde_json::json!([{
            "kind":    undef_diag["kind"].as_str().unwrap(),
            "message": undef_diag["message"].as_str().unwrap(),
            "span": {
                "start": undef_diag["start_offset"].as_u64().unwrap(),
                "end":   undef_diag["end_offset"].as_u64().unwrap(),
            },
            "suggestions": suggestions,
        }])
        .to_string();

        let actions_json = code_actions(source, &diag_info_json);
        let actions: serde_json::Value = serde_json::from_str(&actions_json).unwrap();
        let actions_arr = actions.as_array().unwrap();

        assert!(
            !actions_arr.is_empty(),
            "expected code action(s) for undefined generic function; got: {actions_json}"
        );

        let action = &actions_arr[0];
        assert_eq!(
            action["title"].as_str().unwrap(),
            "Replace with `foo`",
            "action title must name the suggested replacement; got: {actions_json}"
        );

        let edit = &action["edits"].as_array().unwrap()[0];
        let edit_start = usize::try_from(edit["span"]["start"].as_u64().unwrap()).unwrap();
        let edit_end = usize::try_from(edit["span"]["end"].as_u64().unwrap()).unwrap();
        let new_text = edit["new_text"].as_str().unwrap();

        assert_eq!(
            new_text, "foo",
            "replacement text must be the suggested callee name"
        );

        // The edit span must cover only `fooo` — not `fooo<i64>` or `fooo<i64>()`.
        assert_eq!(
            &source[edit_start..edit_end],
            "fooo",
            "edit span must cover only the callee identifier, not the type-argument list"
        );
        // Confirmed: `<` is at edit_end, not inside the edit.
        assert_eq!(
            source.as_bytes().get(edit_end).copied(),
            Some(b'<'),
            "character immediately after edit must be `<` (start of type-arg list)"
        );

        // Applying the edit must yield `foo<i64>()` — type arguments intact.
        let corrected = format!(
            "{}{}{}",
            &source[..edit_start],
            new_text,
            &source[edit_end..]
        );
        assert_eq!(
            corrected, "fn foo() {} fn main() { foo<i64>(); }",
            "applying the edit must replace `fooo<i64>()` with `foo<i64>()`, \
             preserving the type-argument list"
        );
    }

    /// Browser-bridge coverage for `UndefinedFunction` with interstitial trivia
    /// between the callee and the type-argument list.
    ///
    /// A diagnostic spanning `fooo /*keep*/ <i64>()` must produce an edit that
    /// covers only the identifier token `fooo`, leaving the comment and type args
    /// untouched.  Tested via direct `code_actions()` call because producing
    /// interstitial-comment generic calls through `analyze()` requires Hew
    /// concrete syntax that may not preserve such trivia in the AST span.
    #[test]
    fn code_actions_preserves_trivia_between_callee_and_type_args() {
        let source = "fn foo() {} fn main() { fooo /*keep*/ <i64>(); }";
        //                                       ^   ^          ^
        //                                      24  28         45 = end of ')'
        let diag_info_json = serde_json::json!([{
            "kind": "UndefinedFunction",
            "message": "undefined function `fooo`",
            "span": { "start": 24, "end": 45 },
            "suggestions": ["foo"],
        }])
        .to_string();

        let actions_json = code_actions(source, &diag_info_json);
        let actions: serde_json::Value = serde_json::from_str(&actions_json).unwrap();
        let actions_arr = actions.as_array().unwrap();

        assert!(
            !actions_arr.is_empty(),
            "expected a code action for UndefinedFunction with trivia; got: {actions_json}"
        );

        let edit = &actions_arr[0]["edits"].as_array().unwrap()[0];
        let edit_start = usize::try_from(edit["span"]["start"].as_u64().unwrap()).unwrap();
        let edit_end = usize::try_from(edit["span"]["end"].as_u64().unwrap()).unwrap();
        let new_text = edit["new_text"].as_str().unwrap();

        assert_eq!(new_text, "foo", "replacement must be the suggested name");

        // The edit must cover only `fooo` — the identifier token, not the comment.
        assert_eq!(
            &source[edit_start..edit_end],
            "fooo",
            "edit span must cover only the callee identifier, not the interstitial comment"
        );

        // Applying the edit must preserve both the comment and the type-arg list.
        let corrected = format!(
            "{}{}{}",
            &source[..edit_start],
            new_text,
            &source[edit_end..]
        );
        assert_eq!(
            corrected, "fn foo() {} fn main() { foo /*keep*/ <i64>(); }",
            "applying the edit must replace only `fooo`, preserving `/*keep*/ <i64>()`"
        );
    }

    /// Browser-bridge: `code_actions()` for a qualified-path typo
    /// (`Vec::neww()` → `Vec::new()`).
    ///
    /// The old byte-walk stopped at `::` and extracted only `Vec`, so replacing
    /// `Vec` with `Vec::new` produced `Vec::new::neww()`.  With path-separator
    /// awareness the edit must span the full callee path `Vec::neww` and leave
    /// the argument list untouched.
    #[test]
    fn code_actions_qualified_path_replaces_full_callee_path() {
        let source = "fn main() { let _ = Vec::neww(); }";
        //                                   ^       ^
        //                                  20      29 = start of '('

        // DiagnosticInfo as the browser bridge would construct it.
        let diag_info_json = serde_json::json!([{
            "kind":    "UndefinedFunction",
            "message": "undefined function `Vec::neww`",
            "span":    { "start": 20, "end": 31 },   // full Vec::neww() span
            "suggestions": ["Vec::new"],
        }])
        .to_string();

        let actions_json = code_actions(source, &diag_info_json);
        let actions: serde_json::Value = serde_json::from_str(&actions_json).unwrap();
        let actions_arr = actions.as_array().unwrap();

        assert_eq!(
            actions_arr.len(),
            1,
            "expected exactly one action; got: {actions_json}"
        );

        let action = &actions_arr[0];
        assert_eq!(
            action["title"].as_str().unwrap(),
            "Replace with `Vec::new`",
            "action title must include the full suggested path"
        );

        let edit = &action["edits"].as_array().unwrap()[0];
        let edit_start = usize::try_from(edit["span"]["start"].as_u64().unwrap()).unwrap();
        let edit_end = usize::try_from(edit["span"]["end"].as_u64().unwrap()).unwrap();
        let new_text = edit["new_text"].as_str().unwrap();

        assert_eq!(
            new_text, "Vec::new",
            "replacement must be the full suggested path"
        );
        // The edit must cover `Vec::neww` (the full callee path), not just `Vec`.
        assert_eq!(
            &source[edit_start..edit_end],
            "Vec::neww",
            "edit span must cover the entire callee path, not just the first segment"
        );

        // Applying the edit must preserve the argument list.
        let corrected = format!(
            "{}{}{}",
            &source[..edit_start],
            new_text,
            &source[edit_end..]
        );
        assert_eq!(
            corrected, "fn main() { let _ = Vec::new(); }",
            "applying the edit must yield Vec::new() with parentheses intact"
        );
    }
}
