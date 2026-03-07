//! Rename analysis: validate rename sites and compute text edits.

use hew_parser::ParseResult;

use crate::definition::find_definition;
use crate::references::find_all_references;
use crate::util::{simple_word_at_offset, word_at_offset};
use crate::{OffsetSpan, RenameEdit};

/// Check whether rename is valid at `offset`. Returns the word span if yes.
///
/// Returns `None` if the cursor is not on an identifier, the identifier contains
/// a dot or `::` qualifier, or no references exist for the name.
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
    // Verify that references exist for this name.
    find_all_references(source, parse_result, offset)?;
    let (_word, span) = simple_word_at_offset(source, offset)?;
    Some(span)
}

/// Compute rename edits: replace every reference (and the definition site) with `new_name`.
///
/// Returns `None` if no identifier is found at `offset` or no references exist.
#[must_use]
pub fn rename(
    source: &str,
    parse_result: &ParseResult,
    offset: usize,
    new_name: &str,
) -> Option<Vec<RenameEdit>> {
    let (name, spans) = find_all_references(source, parse_result, offset)?;

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
