//! Rename analysis: validate rename sites and compute text edits.

use hew_parser::ParseResult;

use crate::definition::find_definition;
use crate::references::find_all_references;
use crate::util::{simple_word_at_offset, word_at_offset};
use crate::{OffsetSpan, RenameEdit};

/// Check whether rename is valid at `offset`. Returns the word span if yes.
///
/// Returns `None` if the cursor is not on an identifier, the identifier contains
/// a dot or `::` qualifier, or neither a definition nor any references exist for the name.
#[must_use]
pub fn prepare_rename(
    source: &str,
    parse_result: &ParseResult,
    offset: usize,
) -> Option<OffsetSpan> {
    let word = word_at_offset(source, offset)?;
    if word.contains('.') || word.contains("::") {
        return None;
    }
    if find_all_references(source, parse_result, offset).is_none()
        && find_definition(source, parse_result, &word).is_none()
    {
        return None;
    }
    let (_word, span) = simple_word_at_offset(source, offset)?;
    Some(span)
}

/// Compute rename edits: replace every reference (and the definition site) with `new_name`.
///
/// Returns `None` if no identifier is found at `offset` or the symbol has neither
/// a definition nor any references.
#[must_use]
pub fn rename(
    source: &str,
    parse_result: &ParseResult,
    offset: usize,
    new_name: &str,
) -> Option<Vec<RenameEdit>> {
    let (name, _) = simple_word_at_offset(source, offset)?;
    let spans = find_all_references(source, parse_result, offset)
        .map(|(_, spans)| spans)
        .unwrap_or_default();

    let mut edits: Vec<RenameEdit> = spans
        .iter()
        .map(|span| RenameEdit {
            span: *span,
            new_text: new_name.to_string(),
        })
        .collect();

    // Also include the definition site if not already covered by references.
    if let Some(def_span) = find_definition(source, parse_result, &name) {
        if !edits
            .iter()
            .any(|e| e.span.start == def_span.start && e.span.end == def_span.end)
        {
            edits.push(RenameEdit {
                span: def_span,
                new_text: new_name.to_string(),
            });
        }
    }

    if edits.is_empty() {
        return None;
    }

    edits.sort_by_key(|e| (e.span.start, e.span.end));
    edits.dedup_by(|a, b| a.span.start == b.span.start && a.span.end == b.span.end);

    Some(edits)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse(source: &str) -> hew_parser::ParseResult {
        hew_parser::parse(source)
    }

    #[test]
    fn rename_local_variable() {
        let source = "fn main() {\n    let x = 1;\n    let y = x + 2;\n}";
        let pr = parse(source);
        let offset = source.find("let x").unwrap() + 4;
        let result = rename(source, &pr, offset, "z");
        assert!(result.is_some(), "should produce rename edits");
        let edits = result.unwrap();
        assert!(
            edits.len() >= 2,
            "should rename definition and usage, got {}",
            edits.len()
        );
        for edit in &edits {
            assert_eq!(edit.new_text, "z");
        }
    }

    #[test]
    fn prepare_rename_at_whitespace() {
        let source = "fn main() { }";
        let pr = parse(source);
        let offset = source.find("{ }").unwrap() + 1;
        let result = prepare_rename(source, &pr, offset);
        assert!(result.is_none(), "cannot rename at whitespace");
    }

    #[test]
    fn prepare_rename_on_definition_without_local_references() {
        let source = "fn main() { }";
        let pr = parse(source);
        let offset = source.find("main").unwrap();
        let result = prepare_rename(source, &pr, offset);
        assert!(
            result.is_some(),
            "definition-only symbols should still be renameable"
        );
    }

    #[test]
    fn prepare_rename_rejects_qualified_name() {
        // Place cursor on "bar" (after the dot) so word_at_offset returns "foo.bar"
        let source = "fn main() {\n    foo.bar();\n}";
        let pr = parse(source);
        let offset = source.find("bar").unwrap();
        let result = prepare_rename(source, &pr, offset);
        assert!(result.is_none(), "cannot rename qualified name");
    }

    #[test]
    fn rename_function_name() {
        let source = "fn greet() {}\nfn main() {\n    greet()\n}";
        let pr = parse(source);
        let offset = source.find("greet").unwrap();
        let result = rename(source, &pr, offset, "hello");
        assert!(result.is_some(), "should produce rename edits for function");
        let edits = result.unwrap();
        for edit in &edits {
            assert_eq!(edit.new_text, "hello");
        }
        // Should rename at both definition and call sites
        assert!(
            edits.len() >= 2,
            "should rename at definition and call site, got {}",
            edits.len()
        );
    }

    #[test]
    fn prepare_rename_returns_span() {
        let source = "fn main() {\n    let x = 1;\n    let y = x + 2;\n}";
        let pr = parse(source);
        let offset = source.find("let x").unwrap() + 4;
        let result = prepare_rename(source, &pr, offset);
        assert!(result.is_some(), "prepare_rename should return a span");
        let span = result.unwrap();
        assert_eq!(&source[span.start..span.end], "x");
    }
}
