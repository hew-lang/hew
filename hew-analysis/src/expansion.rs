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
    let selected = selected_expansions(program, source, offset)?;
    let ranges = selected
        .iter()
        .enumerate()
        .filter_map(|(index, selected)| {
            selected.then_some(&program.expansions.expansions[index].synthetic)
        })
        .collect();
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

/// Select the invocation and descendants once for both text and provenance.
fn selected_expansions(program: &Program, source: &Path, offset: usize) -> Option<Vec<bool>> {
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
                .source_origin(file, span)
                .unwrap_or_else(|| (file.clone(), span.clone()));
            (file == source && span.contains(&offset)).then_some((span.len(), index))
        })
        .min_by_key(|(length, _)| *length)?
        .1;
    let mut selected_expansions = vec![false; program.expansions.expansions.len()];
    for (index, expansion) in program.expansions.expansions.iter().enumerate() {
        if index == selected
            || expansion
                .parent
                .is_some_and(|parent| selected_expansions[parent.0 as usize])
        {
            selected_expansions[index] = true;
        }
    }
    Some(selected_expansions)
}

/// Structured inspection response shared by command-line and editor clients.
#[derive(Debug, serde::Serialize)]
#[serde(rename_all = "camelCase")]
pub struct Inspection {
    pub text: String,
    #[expect(
        clippy::struct_field_names,
        reason = "Matches the inspectionOnly marker in the editor protocol."
    )]
    inspection_only: bool,
    pub origins: Vec<ExpansionOrigin>,
}

/// Render expanded syntax with original-source provenance. Hygiene remains
/// compiler metadata, so the result is always labelled as inspection-only.
#[must_use]
pub fn inspect(program: &Program, source: &Path, offset: Option<usize>) -> Option<Inspection> {
    Some(Inspection {
        text: render(program, source, offset)?,
        inspection_only: true,
        origins: origins(program, source, offset),
    })
}

/// Source provenance for an invocation in an inspection view. Offsets are UTF-8
/// byte offsets in the original files, never offsets into the rendered text.
#[derive(Debug, serde::Serialize)]
pub struct ExpansionOrigin {
    pub name: String,
    pub invocation: SourceOrigin,
    pub definition: SourceOrigin,
}

/// An original source file and half-open UTF-8 byte span.
#[derive(Debug, serde::Serialize)]
pub struct SourceOrigin {
    pub file: std::path::PathBuf,
    pub span: Span,
}

/// Return source provenance using the compiler's existing expansion map.
fn origins(program: &Program, source: &Path, offset: Option<usize>) -> Vec<ExpansionOrigin> {
    let resolve = |origin: &Origin| {
        let (Origin::Caller { file, span } | Origin::Template { file, span }) = origin;
        let (file, span) = program
            .expansions
            .source_origin(file, span)
            .unwrap_or_else(|| (file.clone(), span.clone()));
        SourceOrigin { file, span }
    };
    let selected =
        offset.map(|offset| selected_expansions(program, source, offset).unwrap_or_default());
    program
        .expansions
        .expansions
        .iter()
        .enumerate()
        .filter_map(|(index, expansion)| {
            let invocation = resolve(&expansion.invocation);
            if selected
                .as_ref()
                .map_or(invocation.file != source, |indices| {
                    !indices.get(index).copied().unwrap_or(false)
                })
            {
                return None;
            }
            Some(ExpansionOrigin {
                name: expansion.name.clone(),
                invocation,
                definition: resolve(&expansion.definition),
            })
        })
        .collect()
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
