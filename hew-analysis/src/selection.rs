//! Nested source selections from tokens and the shared syntax traversal.

use crate::ast_visit::{self, AstVisitor, BindingInfo, ScopeFrame, VisitContext};
use crate::OffsetSpan;
use hew_parser::ast::{Expr, Item, Span, Stmt};
use hew_parser::ParseResult;

struct Selections {
    offset: usize,
    spans: Vec<OffsetSpan>,
}
impl Selections {
    fn add(&mut self, span: &Span) {
        if span.start <= self.offset && self.offset <= span.end {
            self.spans.push(OffsetSpan {
                start: span.start,
                end: span.end,
            });
        }
    }
}
impl<'a> AstVisitor<'a> for Selections {
    fn visit_scope(&mut self, span: &Span, _: &[ScopeFrame<'a>]) {
        self.add(span);
    }
    fn visit_item(&mut self, _: &'a Item, span: &'a Span, _: VisitContext<'a>) {
        self.add(span);
    }
    fn visit_stmt(&mut self, _: &'a Stmt, span: &'a Span, _: VisitContext<'a>) {
        self.add(span);
    }
    fn visit_expr(&mut self, _: &'a Expr, span: &'a Span, _: VisitContext<'a>) {
        self.add(span);
    }
    fn visit_binding(&mut self, binding: BindingInfo<'a>, _: VisitContext<'a>) {
        self.add(&binding.span);
    }
}

/// Smallest selection first; every following range strictly contains its child.
#[must_use]
pub fn selection_ranges(source: &str, parsed: &ParseResult, offset: usize) -> Vec<OffsetSpan> {
    let offset = offset.min(source.len());
    let mut selections = Selections {
        offset,
        spans: Vec::new(),
    };
    let tokens = hew_lexer::lex(source);
    // At a token's first character, choose that token. Only use the token
    // on the left when the cursor lies at its end and no next token begins.
    if let Some((_, span)) = tokens
        .iter()
        .find(|(_, span)| span.start <= offset && offset < span.end)
        .or_else(|| tokens.iter().find(|(_, span)| span.end == offset))
    {
        selections.add(&(span.start..span.end));
    }
    ast_visit::walk_parse_result(Some(source), parsed, &mut selections);
    selections.spans.push(OffsetSpan {
        start: 0,
        end: source.len(),
    });
    selections
        .spans
        .retain(|span| span.start <= span.end && span.end <= source.len());
    selections
        .spans
        .sort_by_key(|span| (span.end - span.start, span.start, span.end));
    selections.spans.dedup();
    let mut nested: Vec<OffsetSpan> = Vec::new();
    for span in selections.spans {
        if nested
            .last()
            .is_none_or(|child| span.start <= child.start && child.end <= span.end)
        {
            nested.push(span);
        }
    }
    nested
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn selections_remain_nested_for_incomplete_source_and_empty_files() {
        for source in [
            "",
            "fn main() {\n let value = (1 +",
            "// text 🦊\n",
            "fn main() {}\n",
        ] {
            let parsed = hew_parser::parse(source);
            for offset in (0..=source.len()).filter(|offset| source.is_char_boundary(*offset)) {
                let selections = selection_ranges(source, &parsed, offset);
                assert_eq!(
                    selections.last(),
                    Some(&OffsetSpan {
                        start: 0,
                        end: source.len()
                    })
                );
                for span in &selections {
                    assert!(span.start <= offset && offset <= span.end);
                    assert!(
                        source.is_char_boundary(span.start) && source.is_char_boundary(span.end)
                    );
                }
                for pair in selections.windows(2) {
                    assert!(pair[1].start <= pair[0].start && pair[0].end <= pair[1].end);
                    assert_ne!(pair[0], pair[1]);
                }
            }
        }
    }
}
