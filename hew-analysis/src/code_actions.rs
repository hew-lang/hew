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
            // UndefinedFunction: the type checker spans the entire call expression
            // (e.g. `fooo()`).  Trim to the callee identifier only so the edit
            // rewrites the name and leaves the argument list intact.
            Some("UndefinedFunction") => {
                let callee_span = trim_to_callee_name(source, diag.span);
                for suggestion in &diag.suggestions {
                    let name = extract_suggestion_name(suggestion);
                    actions.push(CodeAction {
                        title: format!("Replace with `{name}`"),
                        edits: vec![RenameEdit {
                            span: callee_span,
                            new_text: name,
                        }],
                    });
                }
            }

            // Suggestion-based rename for spans that already cover exactly the
            // erroneous identifier (variable, type, field, method).
            Some("UndefinedVariable" | "UndefinedType" | "UndefinedField" | "UndefinedMethod") => {
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

            // Assigning to an immutable `let` binding — offer to change it to `var`.
            Some("MutabilityError") => {
                let var_name = diag.message.split('`').nth(1);
                if let Some(let_span) = find_keyword(source, diag.span.start, "let", var_name) {
                    actions.push(CodeAction {
                        title: "Change `let` to `var`".to_string(),
                        edits: vec![RenameEdit {
                            span: let_span,
                            new_text: "var".to_string(),
                        }],
                    });
                }
            }

            // `var` binding that is never reassigned — offer to demote it to `let`.
            // The diagnostic span covers the whole declaration, so search backward
            // from the span *end* (not start) to find the `var` keyword.
            Some("UnusedMut") => {
                let var_name = diag.message.split('`').nth(1);
                if let Some(var_span) = find_keyword(source, diag.span.end, "var", var_name) {
                    actions.push(CodeAction {
                        title: "Change `var` to `let`".to_string(),
                        edits: vec![RenameEdit {
                            span: var_span,
                            new_text: "let".to_string(),
                        }],
                    });
                }
            }

            // Unused variable — prefix with `_` to silence the warning.
            Some("UnusedVariable") => {
                if let Some(name) = diag.message.split('`').nth(1) {
                    if let Some(edit) = prefix_with_underscore(source, diag.span, name) {
                        actions.push(CodeAction {
                            title: "Prefix with `_`".to_string(),
                            edits: vec![edit],
                        });
                    }
                }
            }

            // Shadowing — offer to prefix the shadowing identifier with `_`.
            Some("Shadowing") => {
                if let Some(name) = diag.message.split('`').nth(1) {
                    if let Some(edit) = prefix_with_underscore(source, diag.span, name) {
                        actions.push(CodeAction {
                            title: format!("Prefix `{name}` with `_`"),
                            edits: vec![edit],
                        });
                    }
                }
            }

            // Unused import — remove the entire import statement.
            Some("UnusedImport") => {
                actions.push(CodeAction {
                    title: "Remove unused import".to_string(),
                    edits: vec![RenameEdit {
                        span: diag.span,
                        new_text: String::new(),
                    }],
                });
            }

            // All other diagnostic kinds have no mechanical fix available.
            _ => {}
        }
    }
    actions
}

// ── Private helpers ──────────────────────────────────────────────────

/// Extract a suggested name from `` Did you mean `foo`? `` style strings.
fn extract_suggestion_name(suggestion: &str) -> String {
    if let Some(start) = suggestion.find('`') {
        if let Some(end) = suggestion[start + 1..].find('`') {
            return suggestion[start + 1..start + 1 + end].to_string();
        }
    }
    suggestion.to_string()
}

/// For an `UndefinedFunction` diagnostic whose span covers the full call
/// expression (e.g. `fooo(args)`), return a span that covers only the callee
/// name (`fooo`) by trimming at the first `(`.
///
/// Falls back to `span` unchanged when `(` is not present (e.g. bare method
/// reference or malformed input).
fn trim_to_callee_name(source: &str, span: OffsetSpan) -> OffsetSpan {
    let region = source.get(span.start..span.end).unwrap_or("");
    let name_len = region.find('(').unwrap_or(region.len());
    OffsetSpan {
        start: span.start,
        end: span.start + name_len,
    }
}

/// Search backwards from `diag_offset` for `keyword` (either `"let"` or
/// `"var"`), verifying that it declares `var_name` if provided. Returns the
/// byte span covering the keyword itself.
fn find_keyword(
    source: &str,
    diag_offset: usize,
    keyword: &str,
    var_name: Option<&str>,
) -> Option<OffsetSpan> {
    let search_start = diag_offset.saturating_sub(200);
    let search_region = &source[search_start..diag_offset];
    let needle = format!("{keyword} ");
    for (rel_pos, _) in search_region.rmatch_indices(needle.as_str()) {
        let abs_pos = search_start + rel_pos;
        if let Some(name) = var_name {
            let after_kw = &source[abs_pos + keyword.len() + 1..];
            let trimmed = after_kw.trim_start();
            if !trimmed.starts_with(name) {
                continue;
            }
            // Word-boundary: char immediately after `name` must not be ident.
            let is_ident = |b: u8| b.is_ascii_alphanumeric() || b == b'_';
            let after_name = &trimmed[name.len()..];
            if after_name.as_bytes().first().is_some_and(|&b| is_ident(b)) {
                continue;
            }
        }
        return Some(OffsetSpan {
            start: abs_pos,
            end: abs_pos + keyword.len(),
        });
    }
    None
}

/// Prefix the identifier `name` within `diag_span` with `_`. Returns `None`
/// if `name` already starts with `_` or cannot be located in the span.
fn prefix_with_underscore(source: &str, diag_span: OffsetSpan, name: &str) -> Option<RenameEdit> {
    if name.starts_with('_') {
        return None;
    }
    let region = source.get(diag_span.start..diag_span.end)?;
    let rel = region.find(name)?;
    let abs_start = diag_span.start + rel;
    let abs_end = abs_start + name.len();
    Some(RenameEdit {
        span: OffsetSpan {
            start: abs_start,
            end: abs_end,
        },
        new_text: format!("_{name}"),
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn diag(kind: &str, message: &str, start: usize, end: usize) -> DiagnosticInfo {
        DiagnosticInfo {
            kind: Some(kind.to_string()),
            message: message.to_string(),
            span: OffsetSpan { start, end },
            suggestions: vec![],
        }
    }

    fn diag_with_suggestions(
        kind: &str,
        message: &str,
        start: usize,
        end: usize,
        suggestions: Vec<&str>,
    ) -> DiagnosticInfo {
        DiagnosticInfo {
            kind: Some(kind.to_string()),
            message: message.to_string(),
            span: OffsetSpan { start, end },
            suggestions: suggestions.into_iter().map(String::from).collect(),
        }
    }

    // ── extract_suggestion_name ──────────────────────────────────────

    #[test]
    fn extract_suggestion_name_backticks() {
        assert_eq!(extract_suggestion_name("Did you mean `foo`?"), "foo");
    }

    #[test]
    fn extract_suggestion_name_plain() {
        assert_eq!(extract_suggestion_name("foo"), "foo");
    }

    // ── UndefinedVariable / UndefinedFunction ────────────────────────

    #[test]
    fn undefined_variable_actions() {
        let source = "let x = foob;";
        let d = diag_with_suggestions(
            "UndefinedVariable",
            "undefined variable `foob`",
            8,
            12,
            vec!["`food`", "`foobar`"],
        );
        let actions = build_code_actions(source, &[d]);
        assert_eq!(actions.len(), 2);
        assert_eq!(actions[0].title, "Replace with `food`");
        assert_eq!(actions[0].edits[0].new_text, "food");
        assert_eq!(actions[1].title, "Replace with `foobar`");
    }

    #[test]
    fn undefined_function_action_trims_call_expr_to_callee() {
        // The type checker spans the whole call expression `fooo()` (bytes 24–30).
        // The edit must replace only `fooo` (bytes 24–28) so `()` is preserved.
        let source = "fn foo() {} fn main() { fooo(); }";
        //                                       ^   ^
        //                                      24  28 = start of '('
        let d = diag_with_suggestions(
            "UndefinedFunction",
            "undefined function `fooo`",
            24, // start of `fooo()`
            30, // end of `fooo()` — past ')'
            vec!["foo"],
        );
        let actions = build_code_actions(source, &[d]);
        assert_eq!(actions.len(), 1);
        assert_eq!(actions[0].title, "Replace with `foo`");
        let edit = &actions[0].edits[0];
        assert_eq!(edit.new_text, "foo");
        // Edit must cover only the callee name, not the parentheses.
        assert_eq!(edit.span, OffsetSpan { start: 24, end: 28 });
        // Applying the edit must yield valid, call-preserving source.
        let corrected = format!("{}{}{}", &source[..24], "foo", &source[28..]);
        assert_eq!(corrected, "fn foo() {} fn main() { foo(); }");
    }

    // ── UndefinedType / UndefinedField / UndefinedMethod ─────────────

    #[test]
    fn undefined_type_action_uses_suggestion() {
        let source = "let x: Foo = 1;";
        let d = diag_with_suggestions(
            "UndefinedType",
            "undefined type `Foo`",
            7,
            10,
            vec!["`Bar`"],
        );
        let actions = build_code_actions(source, &[d]);
        assert_eq!(actions.len(), 1);
        assert_eq!(actions[0].title, "Replace with `Bar`");
        assert_eq!(actions[0].edits[0].new_text, "Bar");
    }

    #[test]
    fn undefined_field_action() {
        let source = "let _ = x.coutn;";
        let d = diag_with_suggestions(
            "UndefinedField",
            "undefined field `coutn`",
            10,
            15,
            vec!["`count`"],
        );
        let actions = build_code_actions(source, &[d]);
        assert_eq!(actions.len(), 1);
        assert_eq!(actions[0].edits[0].new_text, "count");
    }

    #[test]
    fn undefined_method_action() {
        let source = "x.incrment();";
        let d = diag_with_suggestions(
            "UndefinedMethod",
            "undefined method `incrment`",
            2,
            9,
            vec!["`increment`"],
        );
        let actions = build_code_actions(source, &[d]);
        assert_eq!(actions.len(), 1);
        assert_eq!(actions[0].edits[0].new_text, "increment");
    }

    // ── MutabilityError ──────────────────────────────────────────────

    #[test]
    fn mutability_error_action() {
        let source = "let x = 1\nx = 2";
        let d = diag(
            "MutabilityError",
            "cannot assign to immutable variable `x`",
            10,
            15,
        );
        let actions = build_code_actions(source, &[d]);
        assert_eq!(actions.len(), 1);
        assert_eq!(actions[0].title, "Change `let` to `var`");
        assert_eq!(actions[0].edits[0].span, OffsetSpan { start: 0, end: 3 });
        assert_eq!(actions[0].edits[0].new_text, "var");
    }

    // ── UnusedMut ────────────────────────────────────────────────────

    #[test]
    fn unused_mut_action() {
        let source = "fn f() { var abc = 1; }";
        let d = diag(
            "UnusedMut",
            "variable `abc` is declared mutable but never reassigned",
            9,
            22,
        );
        let actions = build_code_actions(source, &[d]);
        assert_eq!(actions.len(), 1);
        assert_eq!(actions[0].title, "Change `var` to `let`");
        assert_eq!(actions[0].edits[0].new_text, "let");
        let edit_span = actions[0].edits[0].span;
        assert_eq!(&source[edit_span.start..edit_span.end], "var");
    }

    // ── UnusedVariable ───────────────────────────────────────────────

    #[test]
    fn unused_variable_action() {
        let source = "let abc = 1";
        let d = diag("UnusedVariable", "unused variable: `abc`", 4, 7);
        let actions = build_code_actions(source, &[d]);
        assert_eq!(actions.len(), 1);
        assert_eq!(actions[0].title, "Prefix with `_`");
        assert_eq!(actions[0].edits[0].span, OffsetSpan { start: 4, end: 7 });
        assert_eq!(actions[0].edits[0].new_text, "_abc");
    }

    #[test]
    fn unused_variable_already_prefixed() {
        let source = "let _abc = 1";
        let d = diag("UnusedVariable", "unused variable: `_abc`", 4, 8);
        let actions = build_code_actions(source, &[d]);
        assert!(actions.is_empty());
    }

    // ── Shadowing ────────────────────────────────────────────────────

    #[test]
    fn shadowing_action() {
        let source = "let x = 1;\nlet x = 2;";
        let second_x = source.rfind('x').unwrap();
        let d = diag(
            "Shadowing",
            "variable `x` shadows a binding in an outer scope",
            second_x,
            second_x + 1,
        );
        let actions = build_code_actions(source, &[d]);
        assert_eq!(actions.len(), 1);
        assert_eq!(actions[0].title, "Prefix `x` with `_`");
        assert_eq!(actions[0].edits[0].new_text, "_x");
    }

    #[test]
    fn shadowing_already_prefixed_no_action() {
        let source = "let x = 1;\nlet _x = 2;";
        let second_x = source.rfind("_x").unwrap();
        let d = diag(
            "Shadowing",
            "variable `_x` shadows a binding in an outer scope",
            second_x,
            second_x + 2,
        );
        let actions = build_code_actions(source, &[d]);
        assert!(actions.is_empty());
    }

    // ── UnusedImport ─────────────────────────────────────────────────

    #[test]
    fn unused_import_action() {
        let source = "import std::os;\nfn main() {}";
        let d = diag("UnusedImport", "unused import `std::os`", 0, 15);
        let actions = build_code_actions(source, &[d]);
        assert_eq!(actions.len(), 1);
        assert_eq!(actions[0].title, "Remove unused import");
        assert_eq!(actions[0].edits[0].new_text, "");
        assert_eq!(actions[0].edits[0].span, OffsetSpan { start: 0, end: 15 });
    }

    // ── Group-E: no mechanical fix ───────────────────────────────────

    #[test]
    fn no_action_for_group_e_kinds() {
        let source = "let x: i32 = 1;";
        for kind in &[
            "Mismatch",
            "ArityMismatch",
            "InferenceFailed",
            "NonExhaustiveMatch",
            "ReturnTypeMismatch",
            "UseAfterMove",
            "BlockingCallInReceiveFn",
            "BorrowedParamReturn",
        ] {
            let d = diag(kind, "some message", 0, 5);
            let actions = build_code_actions(source, &[d]);
            assert!(
                actions.is_empty(),
                "expected no actions for kind {kind}, got {actions:?}"
            );
        }
    }

    // ── find_keyword ─────────────────────────────────────────────────

    #[test]
    fn find_let_keyword_basic() {
        let source = "let x = 1\nx = 2";
        let span = find_keyword(source, 10, "let", Some("x"));
        assert_eq!(span, Some(OffsetSpan { start: 0, end: 3 }));
    }

    #[test]
    fn find_let_keyword_wrong_name() {
        let source = "let y = 1\nx = 2";
        let span = find_keyword(source, 10, "let", Some("x"));
        assert!(span.is_none());
    }

    #[test]
    fn find_var_keyword_basic() {
        let source = "var abc = 1\nabc = 2";
        let span = find_keyword(source, 12, "var", Some("abc"));
        assert_eq!(span, Some(OffsetSpan { start: 0, end: 3 }));
    }

    #[test]
    fn find_keyword_no_false_positive_on_prefix_match() {
        // `var abcde` must NOT match when searching for name `abc`.
        let source = "var abcde = 1\nabc = 2";
        let span = find_keyword(source, 14, "var", Some("abc"));
        assert!(span.is_none(), "prefix 'abcde' should not match name 'abc'");
    }
}
