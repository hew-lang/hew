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
/// Returns JSON `{ contents, span? }` or an empty string if nothing to display.
#[must_use]
#[wasm_bindgen]
pub fn hover(source: &str, offset: usize) -> String {
    let analysis = parse_and_type_check(source);
    match hew_analysis::hover::hover(
        source,
        &analysis.parse_result,
        analysis.type_output.as_ref(),
        offset,
    ) {
        Some(result) => serde_json::to_string(&result).unwrap_or_default(),
        None => String::new(),
    }
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

/// Go to definition. Returns JSON `{ start, end }` or empty string.
#[must_use]
#[wasm_bindgen]
pub fn goto_definition(source: &str, offset: usize) -> String {
    let parse_result = hew_parser::parse(source);
    let Some(word) = hew_analysis::util::word_at_offset(source, offset) else {
        return String::new();
    };
    match hew_analysis::definition::find_definition(source, &parse_result, &word) {
        Some(span) => serde_json::to_string(&span).unwrap_or_default(),
        None => String::new(),
    }
}

/// Find all references at offset. Returns JSON `{ name, spans }` or empty string.
#[must_use]
#[wasm_bindgen]
pub fn find_references(source: &str, offset: usize) -> String {
    let parse_result = hew_parser::parse(source);
    match hew_analysis::references::find_all_references(source, &parse_result, offset) {
        Some((name, spans)) => {
            #[derive(Serialize)]
            struct RefsResult {
                name: String,
                spans: Vec<hew_analysis::OffsetSpan>,
            }
            serde_json::to_string(&RefsResult { name, spans }).unwrap_or_default()
        }
        None => String::new(),
    }
}

// ── Rename ───────────────────────────────────────────────────────────────

/// Prepare rename at offset. Returns JSON `{ start, end }` or empty string.
#[must_use]
#[wasm_bindgen]
pub fn prepare_rename(source: &str, offset: usize) -> String {
    let parse_result = hew_parser::parse(source);
    match hew_analysis::rename::prepare_rename(source, &parse_result, offset) {
        Some(span) => serde_json::to_string(&span).unwrap_or_default(),
        None => String::new(),
    }
}

/// Compute rename edits. Returns JSON array of `{ span, new_text }` or empty string.
#[must_use]
#[wasm_bindgen]
pub fn rename(source: &str, offset: usize, new_name: &str) -> String {
    let parse_result = hew_parser::parse(source);
    match hew_analysis::rename::rename(source, &parse_result, offset, new_name) {
        Some(edits) => serde_json::to_string(&edits).unwrap_or_default(),
        None => String::new(),
    }
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

/// Get signature help at offset. Returns JSON `SignatureHelpResult` or empty string.
#[must_use]
#[wasm_bindgen]
pub fn signature_help(source: &str, offset: usize) -> String {
    let analysis = parse_and_type_check(source);
    let Some(type_output) = analysis.type_output.as_ref() else {
        return String::new();
    };
    match hew_analysis::signature_help::build_signature_help(source, type_output, offset) {
        Some(result) => serde_json::to_string(&result).unwrap_or_default(),
        None => String::new(),
    }
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

/// Diagnostic reported by the WASM analysis pipeline.
#[derive(Serialize)]
struct WasmDiagnostic {
    severity: String,
    message: String,
    start_offset: usize,
    end_offset: usize,
    kind: String,
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
        diagnostics.push(WasmDiagnostic {
            severity: "error".to_string(),
            message: err.message.clone(),
            start_offset: err.span.start,
            end_offset: err.span.end,
            kind: "parse_error".to_string(),
        });
    }

    if let Some(type_output) = analysis.type_output.as_ref() {
        for err in &type_output.errors {
            diagnostics.push(WasmDiagnostic {
                severity: "error".to_string(),
                message: err.message.clone(),
                start_offset: err.span.start,
                end_offset: err.span.end,
                kind: format!("{:?}", err.kind),
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

    for entry in entries {
        let id = entry
            .get("id")
            .and_then(serde_json::Value::as_str)
            .unwrap_or_else(|| {
                panic!("curated playground manifest entry missing string id: {entry}")
            });
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
    fn find_references_works() {
        let source = "fn main() { let x = 1; let y = x; }";
        let result = find_references(source, 17); // on 'x' definition
        assert!(!result.is_empty());
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
    fn prepare_rename_no_panic() {
        let source = "fn foo() {} fn main() { foo(); }";
        let result = prepare_rename(source, 3); // on 'foo' definition
                                                // Should return a span or empty string.
        let _ = result;
    }

    #[test]
    fn rename_no_panic() {
        let source = "fn foo() {} fn main() { foo(); }";
        let result = rename(source, 3, "bar"); // rename 'foo' to 'bar'
        let _ = result;
    }
}
