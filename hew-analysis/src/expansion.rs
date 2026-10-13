//! Readable macro expansions shared by the CLI, language server and browser.

use crate::ast_visit::{walk_program, AstVisitor, VisitContext};
use hew_parser::ast::{Expr, Item, Program, Span};
use hew_parser::macro_syntax::Origin;
use std::path::Path;

/// Render the expanded program, or the innermost expansion at a source offset.
#[must_use]
pub fn render(program: &Program, source: &Path, offset: Option<usize>) -> Option<String> {
    let Some(offset) = offset else {
        return Some(hew_parser::fmt::format_expanded_program(program));
    };
    let expansion = program
        .expansions
        .expansions
        .iter()
        .filter_map(|expansion| {
            let (Origin::Caller { file, span } | Origin::Template { file, span }) =
                &expansion.invocation;
            let (file, span) = program
                .expansions
                .source_origin(span)
                .unwrap_or_else(|| (file.clone(), span.clone()));
            (file == source && span.contains(&offset)).then_some((span.len(), expansion))
        })
        .min_by_key(|(length, _)| *length)?
        .1;
    let mut view = ExpansionView {
        range: &expansion.synthetic,
        items: Vec::new(),
        expression: None,
    };
    walk_program(None, program, &mut view);
    if !view.items.is_empty() {
        return Some(
            view.items
                .iter()
                .map(|(item, span)| hew_parser::fmt::format_expanded_item(item, span))
                .collect::<Vec<_>>()
                .join("\n"),
        );
    }
    view.expression
        .map(|(expr, span)| hew_parser::fmt::format_expanded_expression(expr, span))
}

struct ExpansionView<'ast> {
    range: &'ast Span,
    items: Vec<(&'ast Item, &'ast Span)>,
    expression: Option<(&'ast Expr, &'ast Span)>,
}

impl<'ast> AstVisitor<'ast> for ExpansionView<'ast> {
    fn visit_item(&mut self, item: &'ast Item, span: &'ast Span, _ctx: VisitContext<'ast>) {
        if self.range.contains(&span.start) {
            self.items.push((item, span));
        }
    }

    fn visit_expr(&mut self, expr: &'ast Expr, span: &'ast Span, _ctx: VisitContext<'ast>) {
        if self.range.contains(&span.start)
            && self
                .expression
                .is_none_or(|(_, previous)| span.len() > previous.len())
        {
            self.expression = Some((expr, span));
        }
    }
}
