//! Readable macro expansions shared by the CLI, language server and browser.

use crate::ast_visit::{walk_program, AstVisitor, VisitContext};
use hew_parser::ast::{Expr, FnDecl, Item, Program, Span, TraitItem};
use hew_parser::macro_syntax::Origin;
use std::path::Path;

/// Render the expanded program, or the innermost expansion at a source offset.
#[must_use]
pub fn render(program: &Program, source: &Path, offset: Option<usize>) -> Option<String> {
    let Some(offset) = offset else {
        return Some(hew_parser::fmt::format_expanded_program(program));
    };
    let selected = program
        .expansions
        .expansions
        .iter()
        .enumerate()
        .filter_map(|(index, expansion)| {
            let (Origin::Caller { file, span } | Origin::Template { file, span }) =
                &expansion.invocation;
            let (file, span) = program
                .expansions
                .source_origin(span)
                .unwrap_or_else(|| (file.clone(), span.clone()));
            (file == source && span.contains(&offset)).then_some((span.len(), index))
        })
        .min_by_key(|(length, _)| *length)?
        .1;
    let mut selected_expansions = vec![false; program.expansions.expansions.len()];
    let mut ranges = Vec::new();
    for (index, expansion) in program.expansions.expansions.iter().enumerate() {
        if index == selected
            || expansion
                .parent
                .is_some_and(|parent| selected_expansions[parent.0 as usize])
        {
            selected_expansions[index] = true;
            ranges.push(&expansion.synthetic);
        }
    }
    let mut view = ExpansionView {
        ranges,
        items: Vec::new(),
        expression: None,
    };
    walk_program(None, program, &mut view);
    if !view.items.is_empty() {
        return Some(view.items.join("\n"));
    }
    view.expression
        .map(|(expr, span)| hew_parser::fmt::format_expanded_expression(expr, span))
}

struct ExpansionView<'ast> {
    ranges: Vec<&'ast Span>,
    items: Vec<String>,
    expression: Option<(&'ast Expr, &'ast Span)>,
}

impl ExpansionView<'_> {
    fn includes(&self, span: &Span) -> bool {
        self.ranges.iter().any(|range| range.contains(&span.start))
    }

    fn method(&mut self, method: &FnDecl) {
        if self.includes(&method.fn_span) {
            self.items.push(hew_parser::fmt::format_expanded_item(
                &Item::Function(method.clone()),
                &method.fn_span,
            ));
        }
    }
}

impl<'ast> AstVisitor<'ast> for ExpansionView<'ast> {
    fn visit_item(&mut self, item: &'ast Item, span: &'ast Span, _ctx: VisitContext<'ast>) {
        if self.includes(span) {
            self.items
                .push(hew_parser::fmt::format_expanded_item(item, span));
            return;
        }
        match item {
            Item::Impl(block) => {
                for method in &block.methods {
                    self.method(method);
                }
            }
            Item::Actor(actor) => {
                for method in &actor.methods {
                    self.method(method);
                }
            }
            Item::Trait(declaration) => {
                for item in &declaration.items {
                    if let TraitItem::Method(method) = item {
                        if self.includes(&method.span) {
                            self.items
                                .push(hew_parser::fmt::format_expanded_trait_method(method));
                        }
                    }
                }
            }
            _ => {}
        }
    }

    fn visit_expr(&mut self, expr: &'ast Expr, span: &'ast Span, _ctx: VisitContext<'ast>) {
        // The walker is pre-order: the first surviving expression is the
        // outer expression, even when a nested expansion has a longer span.
        if self.includes(span) && self.expression.is_none() {
            self.expression = Some((expr, span));
        }
    }
}
