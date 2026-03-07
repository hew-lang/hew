//! Analysis module for code actions.

use crate::{CodeAction, OffsetSpan, RenameEdit};

/// A diagnostic description needed for code action generation.
///
/// This is a protocol-independent representation of a diagnostic that
/// carries just enough information to compute code actions.
#[derive(Debug, serde::Serialize, serde::Deserialize)]
pub struct DiagnosticInfo {
    pub kind: Option<String>,
    pub message: String,
    pub span: OffsetSpan,
    pub suggestions: Vec<String>,
}

/// Build code actions from a set of diagnostics.
#[must_use]
pub fn build_code_actions(source: &str, diagnostics: &[DiagnosticInfo]) -> Vec<CodeAction> {
    let mut actions = Vec::new();
    for diag in diagnostics {
        let kind = diag.kind.as_deref();
        match kind {
            Some("UndefinedVariable" | "UndefinedFunction") => {
                for suggestion in &diag.suggestions {
                    let name = extract_suggestion_name(suggestion);
                    actions.push(CodeAction {
                        title: format!("Replace with `{name}`"),
                        edits: vec![RenameEdit {
                            span: diag.span,
                            new_text: name,
                        }],
                    });
                }
            }
            Some("MutabilityError") => {
                // Extract the variable name from "cannot assign to immutable variable `name`"
                let var_name = diag.message.split('`').nth(1);
                if let Some(let_span) = find_let_keyword(source, diag.span.start, var_name) {
                    actions.push(CodeAction {
                        title: "Change `let` to `var`".to_string(),
                        edits: vec![RenameEdit {
                            span: let_span,
                            new_text: "var".to_string(),
                        }],
                    });
                }
            }
            Some("UnusedVariable") => {
                // Extract the variable name from the message (format: "unused variable: `name`")
                if let Some(name) = diag.message.split('`').nth(1) {
                    if !name.starts_with('_') {
                        let start_offset = diag.span.start;
                        let end_offset = diag.span.end;
                        let region = &source[start_offset..end_offset];
                        if let Some(rel) = region.find(name) {
                            let abs_start = start_offset + rel;
                            let abs_end = abs_start + name.len();
                            actions.push(CodeAction {
                                title: "Prefix with `_`".to_string(),
                                edits: vec![RenameEdit {
                                    span: OffsetSpan {
                                        start: abs_start,
                                        end: abs_end,
                                    },
                                    new_text: format!("_{name}"),
                                }],
                            });
                        }
                    }
                }
            }
            _ => {}
        }
    }
    actions
}

/// Extract a suggested name from ``Did you mean \`foo\`?`` style strings.
fn extract_suggestion_name(suggestion: &str) -> String {
    if let Some(start) = suggestion.find('`') {
        if let Some(end) = suggestion[start + 1..].find('`') {
            return suggestion[start + 1..start + 1 + end].to_string();
        }
    }
    suggestion.to_string()
}

/// Search backwards from `diag_offset` for a `let ` keyword, verifying it
/// declares `var_name` if provided.
fn find_let_keyword(
    source: &str,
    diag_offset: usize,
    var_name: Option<&str>,
) -> Option<OffsetSpan> {
    let search_start = diag_offset.saturating_sub(200);
    let search_region = &source[search_start..diag_offset];
    // Search backwards for all `let ` occurrences, closest first
    for (rel_pos, _) in search_region.rmatch_indices("let ") {
        let abs_pos = search_start + rel_pos;
        // Verify this `let` declares the expected variable
        if let Some(name) = var_name {
            let after_let = &source[abs_pos + 4..];
            let trimmed = after_let.trim_start();
            if !trimmed.starts_with(name) {
                continue;
            }
        }
        return Some(OffsetSpan {
            start: abs_pos,
            end: abs_pos + 3,
        });
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn extract_suggestion_name_backticks() {
        assert_eq!(extract_suggestion_name("Did you mean `foo`?"), "foo");
    }

    #[test]
    fn extract_suggestion_name_plain() {
        assert_eq!(extract_suggestion_name("foo"), "foo");
    }

    #[test]
    fn undefined_variable_actions() {
        let source = "let x = foob;";
        let diag = DiagnosticInfo {
            kind: Some("UndefinedVariable".to_string()),
            message: "undefined variable `foob`".to_string(),
            span: OffsetSpan { start: 8, end: 12 },
            suggestions: vec!["`food`".to_string(), "`foobar`".to_string()],
        };
        let actions = build_code_actions(source, &[diag]);
        assert_eq!(actions.len(), 2);
        assert_eq!(actions[0].title, "Replace with `food`");
        assert_eq!(actions[0].edits[0].new_text, "food");
        assert_eq!(actions[1].title, "Replace with `foobar`");
    }

    #[test]
    fn mutability_error_action() {
        let source = "let x = 1\nx = 2";
        let diag = DiagnosticInfo {
            kind: Some("MutabilityError".to_string()),
            message: "cannot assign to immutable variable `x`".to_string(),
            span: OffsetSpan { start: 10, end: 15 },
            suggestions: vec![],
        };
        let actions = build_code_actions(source, &[diag]);
        assert_eq!(actions.len(), 1);
        assert_eq!(actions[0].title, "Change `let` to `var`");
        assert_eq!(actions[0].edits[0].span, OffsetSpan { start: 0, end: 3 });
        assert_eq!(actions[0].edits[0].new_text, "var");
    }

    #[test]
    fn unused_variable_action() {
        let source = "let abc = 1";
        let diag = DiagnosticInfo {
            kind: Some("UnusedVariable".to_string()),
            message: "unused variable: `abc`".to_string(),
            span: OffsetSpan { start: 4, end: 7 },
            suggestions: vec![],
        };
        let actions = build_code_actions(source, &[diag]);
        assert_eq!(actions.len(), 1);
        assert_eq!(actions[0].title, "Prefix with `_`");
        assert_eq!(actions[0].edits[0].span, OffsetSpan { start: 4, end: 7 });
        assert_eq!(actions[0].edits[0].new_text, "_abc");
    }

    #[test]
    fn unused_variable_already_prefixed() {
        let source = "let _abc = 1";
        let diag = DiagnosticInfo {
            kind: Some("UnusedVariable".to_string()),
            message: "unused variable: `_abc`".to_string(),
            span: OffsetSpan { start: 4, end: 8 },
            suggestions: vec![],
        };
        let actions = build_code_actions(source, &[diag]);
        assert!(actions.is_empty());
    }

    #[test]
    fn no_actions_for_unknown_kind() {
        let source = "let x = 1";
        let diag = DiagnosticInfo {
            kind: Some("SomethingElse".to_string()),
            message: "something".to_string(),
            span: OffsetSpan { start: 0, end: 9 },
            suggestions: vec![],
        };
        let actions = build_code_actions(source, &[diag]);
        assert!(actions.is_empty());
    }

    #[test]
    fn find_let_keyword_basic() {
        let source = "let x = 1\nx = 2";
        let span = find_let_keyword(source, 10, Some("x"));
        assert_eq!(span, Some(OffsetSpan { start: 0, end: 3 }));
    }

    #[test]
    fn find_let_keyword_wrong_name() {
        let source = "let y = 1\nx = 2";
        let span = find_let_keyword(source, 10, Some("x"));
        assert!(span.is_none());
    }
}
