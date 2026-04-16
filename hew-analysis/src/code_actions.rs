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
#[allow(
    clippy::too_many_lines,
    reason = "diagnostic-to-fix mapping stays clearer as one exhaustive match"
)]
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

            Some("StyleSuggestion")
                if diag.message.contains("while true")
                    && diag
                        .suggestions
                        .iter()
                        .any(|suggestion| suggestion.contains("loop")) =>
            {
                if let Some(edit) = replace_while_true_with_loop(source, diag.span) {
                    actions.push(CodeAction {
                        title: "Replace `while true` with `loop`".to_string(),
                        edits: vec![edit],
                    });
                }
            }

            Some("UnreachableCode") => {
                actions.push(CodeAction {
                    title: "Remove unreachable code".to_string(),
                    edits: vec![RenameEdit {
                        span: diag.span,
                        new_text: String::new(),
                    }],
                });
            }

            Some("NonExhaustiveMatch") => {
                let missing_arms: Vec<_> = diag
                    .suggestions
                    .iter()
                    .filter(|suggestion| !suggestion.is_empty())
                    .cloned()
                    .collect();
                if let Some(edit) = add_missing_match_arms(source, diag.span, &missing_arms) {
                    actions.push(CodeAction {
                        title: "Add missing match arms".to_string(),
                        edits: vec![edit],
                    });
                }
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
/// expression, return a span covering only the callee path token.
///
/// Walks identifier bytes (`[A-Za-z0-9_]`) and `::` path separators, stopping
/// at the first byte that is neither part of an identifier nor a `::` prefix.
/// This correctly handles all call forms:
/// - plain calls:                 `fooo()`              → `fooo`
/// - qualified paths:             `Vec::neww()`         → `Vec::neww`
/// - generic calls:               `fooo<T>()`           → `fooo`
/// - trivia before type args:     `fooo /*c*/ <T>()`    → `fooo`
/// - qualified generic calls:     `Vec::neww<T>()`      → `Vec::neww`
fn trim_to_callee_name(source: &str, span: OffsetSpan) -> OffsetSpan {
    let region = source.get(span.start..span.end).unwrap_or("").as_bytes();
    let mut pos = 0;
    loop {
        // Walk one identifier segment.
        while pos < region.len() && (region[pos].is_ascii_alphanumeric() || region[pos] == b'_') {
            pos += 1;
        }
        // Continue through `::` only when the next byte after `::` is also a
        // valid identifier start — this prevents walking into `::` trailing
        // separators or `::=` and similar tokens.
        if pos + 2 < region.len()
            && region[pos] == b':'
            && region[pos + 1] == b':'
            && (region[pos + 2].is_ascii_alphanumeric() || region[pos + 2] == b'_')
        {
            pos += 2;
            continue;
        }
        break;
    }
    OffsetSpan {
        start: span.start,
        end: span.start + pos,
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

fn replace_while_true_with_loop(source: &str, diag_span: OffsetSpan) -> Option<RenameEdit> {
    let region = source.get(diag_span.start..diag_span.end)?;
    let while_rel = region.find("while")?;
    let after_while = &region[while_rel + "while".len()..];
    let trimmed = after_while.trim_start_matches(char::is_whitespace);
    if !trimmed.starts_with("true") {
        return None;
    }
    let ws_len = after_while.len() - trimmed.len();
    Some(RenameEdit {
        span: OffsetSpan {
            start: diag_span.start + while_rel,
            end: diag_span.start + while_rel + "while".len() + ws_len + "true".len(),
        },
        new_text: "loop".to_string(),
    })
}

fn add_missing_match_arms(
    source: &str,
    diag_span: OffsetSpan,
    missing_arms: &[String],
) -> Option<RenameEdit> {
    if missing_arms.is_empty() {
        return None;
    }

    let region = source.get(diag_span.start..diag_span.end)?;
    let open_rel = region.find('{')?;
    let close_rel = region.rfind('}')?;
    if open_rel >= close_rel {
        return None;
    }

    let insert_at = diag_span.start + close_rel;
    let body = &region[open_rel + 1..close_rel];
    let insert_text = if body.contains('\n') {
        let closing_indent = line_indent_at(source, insert_at);
        let inner_indent =
            match_body_indent(body).unwrap_or_else(|| format!("{closing_indent}    "));
        let mut text = String::new();
        for arm in missing_arms {
            if !text.is_empty() || !body.ends_with('\n') {
                text.push('\n');
            }
            text.push_str(&inner_indent);
            text.push_str(arm);
            text.push_str(" => {},");
        }
        text.push('\n');
        text.push_str(&closing_indent);
        text
    } else {
        format!(" {} ", inline_missing_match_arms(missing_arms))
    };

    Some(RenameEdit {
        span: OffsetSpan {
            start: insert_at,
            end: insert_at,
        },
        new_text: insert_text,
    })
}

fn inline_missing_match_arms(missing_arms: &[String]) -> String {
    missing_arms
        .iter()
        .map(|arm| format!("{arm} => {{}},"))
        .collect::<Vec<_>>()
        .join(" ")
}

fn line_indent_at(source: &str, offset: usize) -> String {
    let line_start = source[..offset].rfind('\n').map_or(0, |idx| idx + 1);
    source[line_start..offset]
        .chars()
        .take_while(|ch| matches!(ch, ' ' | '\t'))
        .collect()
}

fn match_body_indent(body: &str) -> Option<String> {
    body.lines()
        .find(|line| !line.trim().is_empty())
        .map(|line| {
            line.chars()
                .take_while(|ch| matches!(ch, ' ' | '\t'))
                .collect()
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

    #[test]
    fn undefined_function_generic_call_trims_to_callee_only() {
        // Generic call `fooo<i64>()`: span covers `fooo<i64>()` (bytes 24–35).
        // The edit must replace only `fooo` (bytes 24–28), leaving `<i64>()`
        // intact so the corrected source becomes `foo<i64>()`.
        let source = "fn foo() {} fn main() { fooo<i64>(); }";
        //                                       ^   ^    ^
        //                                      24  28   35 = end of ')'
        let d = diag_with_suggestions(
            "UndefinedFunction",
            "undefined function `fooo`",
            24, // start of `fooo<i64>()`
            35, // end of `fooo<i64>()` — past ')'
            vec!["foo"],
        );
        let actions = build_code_actions(source, &[d]);
        assert_eq!(actions.len(), 1);
        assert_eq!(actions[0].title, "Replace with `foo`");
        let edit = &actions[0].edits[0];
        assert_eq!(edit.new_text, "foo");
        // Edit must cover only the callee, not `<i64>()`.
        assert_eq!(edit.span, OffsetSpan { start: 24, end: 28 });
        // Applying the edit must preserve the type-argument list.
        let corrected = format!("{}{}{}", &source[..24], "foo", &source[28..]);
        assert_eq!(corrected, "fn foo() {} fn main() { foo<i64>(); }");
    }

    #[test]
    fn undefined_function_trivia_between_callee_and_type_args() {
        // `fooo /*keep*/ <i64>()`: interstitial comment between callee and `<`.
        // Trimming at `<` would produce `fooo /*keep*/ ` as the edit span;
        // trimming by identifier bytes must produce only `fooo` (bytes 24–28).
        let source = "fn foo() {} fn main() { fooo /*keep*/ <i64>(); }";
        //                                       ^   ^          ^
        //                                      24  28         45 = end of ')'
        let d = diag_with_suggestions(
            "UndefinedFunction",
            "undefined function `fooo`",
            24, // start of `fooo /*keep*/ <i64>()`
            45, // end past ')'
            vec!["foo"],
        );
        let actions = build_code_actions(source, &[d]);
        assert_eq!(actions.len(), 1);
        let edit = &actions[0].edits[0];
        assert_eq!(edit.new_text, "foo");
        // Edit must cover only the identifier token `fooo`, not the comment
        // or type-argument list.
        assert_eq!(edit.span, OffsetSpan { start: 24, end: 28 });
        // Applying the edit must preserve the comment and type-argument list.
        let corrected = format!("{}{}{}", &source[..24], "foo", &source[28..]);
        assert_eq!(corrected, "fn foo() {} fn main() { foo /*keep*/ <i64>(); }");
    }

    #[test]
    fn undefined_function_qualified_path_replaces_full_callee_path() {
        // `Vec::neww()`: the callee is the qualified path `Vec::neww`.
        // Trimming at the first `::` separator would leave only `Vec`; replacing
        // `Vec` with the suggestion `Vec::new` would produce `Vec::new::neww()`.
        // The correct edit span must cover the entire path `Vec::neww` (bytes 20–29).
        let source = "fn main() { let _ = Vec::neww(); }";
        //                                   ^       ^
        //                                  20      29 = start of '('
        let d = diag_with_suggestions(
            "UndefinedFunction",
            "undefined function `Vec::neww`",
            20, // start of `Vec::neww()`
            31, // end past ')'
            vec!["Vec::new"],
        );
        let actions = build_code_actions(source, &[d]);
        assert_eq!(actions.len(), 1);
        assert_eq!(actions[0].title, "Replace with `Vec::new`");
        let edit = &actions[0].edits[0];
        assert_eq!(edit.new_text, "Vec::new");
        // Edit span must cover the full callee path, not just the first segment.
        assert_eq!(edit.span, OffsetSpan { start: 20, end: 29 });
        // Applying the edit must yield the corrected call.
        let corrected = format!("{}{}{}", &source[..20], "Vec::new", &source[29..]);
        assert_eq!(corrected, "fn main() { let _ = Vec::new(); }");
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

    fn apply_edit(source: &str, edit: &RenameEdit) -> String {
        format!(
            "{}{}{}",
            &source[..edit.span.start],
            edit.new_text,
            &source[edit.span.end..]
        )
    }

    // ── StyleSuggestion / UnreachableCode ─────────────────────────────

    #[test]
    fn style_suggestion_while_true_action_rewrites_header_to_loop() {
        let source = "fn main() {
    while true {
        break;
    }
}";
        let start = source.find("while true").expect("while true start");
        let end = source[start..]
            .find(
                "
    }",
            )
            .map(|idx| start + idx + 6)
            .expect("while block end");
        let d = diag_with_suggestions(
            "StyleSuggestion",
            "`while true` can be simplified",
            start,
            end,
            vec!["use `loop { ... }` instead of `while true { ... }`"],
        );
        let actions = build_code_actions(source, &[d]);
        assert_eq!(actions.len(), 1);
        assert_eq!(actions[0].title, "Replace `while true` with `loop`");
        assert_eq!(
            apply_edit(source, &actions[0].edits[0]),
            "fn main() {
    loop {
        break;
    }
}"
        );
    }

    #[test]
    fn style_suggestion_without_loop_rewrite_has_no_action() {
        let source = "type User { id: int }";
        let d = diag(
            "StyleSuggestion",
            "wire `User.id`: field has `since 2` but struct has no version",
            0,
            source.len(),
        );
        let actions = build_code_actions(source, &[d]);
        assert!(actions.is_empty());
    }

    #[test]
    fn unreachable_code_action_removes_dead_statement() {
        let source = "fn main() {
    return;
    log(1);
}";
        let start = source.find("    log(1);").expect("dead statement start");
        let end = start
            + "    log(1);
"
            .len();
        let d = diag_with_suggestions(
            "UnreachableCode",
            "unreachable code",
            start,
            end,
            vec!["remove this code or restructure the control flow"],
        );
        let actions = build_code_actions(source, &[d]);
        assert_eq!(actions.len(), 1);
        assert_eq!(actions[0].title, "Remove unreachable code");
        assert_eq!(
            apply_edit(source, &actions[0].edits[0]),
            "fn main() {
    return;
}"
        );
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

    #[test]
    fn non_exhaustive_match_action_adds_missing_arms_multiline() {
        let source =
            "fn label(opt: Option<int>) -> int {\n    match opt {\n        None => 0,\n    }\n}\n";
        let match_start = source.find("match").unwrap();
        let span = OffsetSpan {
            start: match_start,
            end: match_start + source[match_start..].find("\n    }\n").unwrap() + 6,
        };
        let d = diag_with_suggestions(
            "NonExhaustiveMatch",
            "non-exhaustive match: missing Some",
            span.start,
            span.end,
            vec!["Some(_)"],
        );
        let actions = build_code_actions(source, &[d]);
        assert_eq!(actions.len(), 1);
        assert_eq!(actions[0].title, "Add missing match arms");
        assert_eq!(
            actions[0].edits[0].new_text,
            "\n        Some(_) => {},\n    "
        );
    }

    #[test]
    fn non_exhaustive_match_action_adds_missing_arms_inline() {
        let source = "fn label(opt: Option<int>) -> int { match opt { None => 0, } }";
        let match_start = source.find("match").unwrap();
        let span = OffsetSpan {
            start: match_start,
            end: match_start + source[match_start..].find(", }").unwrap() + 3,
        };
        let d = diag_with_suggestions(
            "NonExhaustiveMatch",
            "non-exhaustive match: missing Some",
            span.start,
            span.end,
            vec!["Some(_)"],
        );
        let actions = build_code_actions(source, &[d]);
        assert_eq!(actions.len(), 1);
        assert_eq!(actions[0].edits[0].new_text, " Some(_) => {}, ");
    }

    // ── Group-E: no mechanical fix ───────────────────────────────────

    #[test]
    fn no_action_for_group_e_kinds() {
        let source = "let x: i32 = 1;";
        for kind in &[
            "Mismatch",
            "ArityMismatch",
            "InferenceFailed",
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
