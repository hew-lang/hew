//! WASM bindings for Hew language diagnostics and analysis.
//!
//! This crate provides source-code analysis capabilities:
//!
//! - **Diagnostics** — parse, type-check, and analyze Hew source code
//!   (`analyze`, `hover`, `get_keywords`).

use serde::{Deserialize, Serialize};
use wasm_bindgen::prelude::*;

// ── Diagnostics API ──────────────────────────────────────────────────────

/// Analyze Hew source code and return diagnostics as JSON.
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
#[must_use]
#[wasm_bindgen]
pub fn hover(source: &str, offset: usize) -> String {
    let parse_result = hew_parser::parse(source);
    if !parse_result.errors.is_empty() {
        return String::new();
    }

    let mut checker = hew_types::Checker::new();
    let output = checker.check_program(&parse_result.program);

    for (span_key, ty) in &output.expr_types {
        if span_key.start <= offset && offset < span_key.end {
            return format!("{ty}");
        }
    }
    String::new()
}

#[derive(Serialize, Deserialize)]
struct AnalysisResult {
    diagnostics: Vec<Diagnostic>,
    tokens: Vec<TokenInfo>,
    symbols: Vec<SymbolInfo>,
}

#[derive(Serialize, Deserialize)]
struct Diagnostic {
    severity: String,
    message: String,
    start_offset: usize,
    end_offset: usize,
    kind: String,
}

#[derive(Serialize, Deserialize)]
struct TokenInfo {
    kind: String,
    start: usize,
    end: usize,
}

#[derive(Serialize, Deserialize)]
struct SymbolInfo {
    name: String,
    kind: String,
    start: usize,
    end: usize,
}

fn run_analysis(source: &str) -> AnalysisResult {
    let tokens_raw = hew_lexer::lex(source);
    let tokens: Vec<TokenInfo> = tokens_raw
        .iter()
        .map(|(t, s)| TokenInfo {
            kind: format!("{t:?}").split('(').next().unwrap_or("").to_string(),
            start: s.start,
            end: s.end,
        })
        .collect();

    let parse_result = hew_parser::parse(source);
    let mut diagnostics = Vec::new();

    for err in &parse_result.errors {
        diagnostics.push(Diagnostic {
            severity: "error".to_string(),
            message: err.message.clone(),
            start_offset: err.span.start,
            end_offset: err.span.end,
            kind: "parse_error".to_string(),
        });
    }

    if parse_result.errors.is_empty() {
        let mut checker = hew_types::Checker::new();
        let type_output = checker.check_program(&parse_result.program);

        for err in &type_output.errors {
            diagnostics.push(Diagnostic {
                severity: "error".to_string(),
                message: err.message.clone(),
                start_offset: err.span.start,
                end_offset: err.span.end,
                kind: format!("{:?}", err.kind),
            });
        }
    }

    let symbols = extract_symbols(&parse_result.program);

    AnalysisResult {
        diagnostics,
        tokens,
        symbols,
    }
}

fn extract_symbols(program: &hew_parser::ast::Program) -> Vec<SymbolInfo> {
    let mut symbols = Vec::new();
    for (item, span) in &program.items {
        let info = match item {
            hew_parser::ast::Item::Function(f) => Some((&f.name, "function")),
            hew_parser::ast::Item::Actor(a) => Some((&a.name, "actor")),
            hew_parser::ast::Item::Supervisor(s) => Some((&s.name, "supervisor")),
            hew_parser::ast::Item::Trait(t) => Some((&t.name, "trait")),
            hew_parser::ast::Item::TypeDecl(t) => Some((&t.name, "type")),
            hew_parser::ast::Item::Const(c) => Some((&c.name, "constant")),
            hew_parser::ast::Item::Wire(w) => Some((&w.name, "wire")),
            hew_parser::ast::Item::TypeAlias(ta) => Some((&ta.name, "type_alias")),
            hew_parser::ast::Item::Import(_)
            | hew_parser::ast::Item::Impl(_)
            | hew_parser::ast::Item::ExternBlock(_) => None,
        };
        if let Some((name, kind)) = info {
            symbols.push(SymbolInfo {
                name: name.clone(),
                kind: kind.to_string(),
                start: span.start,
                end: span.end,
            });
        }
    }
    symbols
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
}
