use std::collections::HashSet;
use std::sync::Arc;

use hew_parser::ast::{Span, SyntaxContext};
use hew_parser::macro_syntax::{FragmentInputs, FragmentKind, TokenContexts, TokenTree};

use crate::matcher::{capture_name, repetition_suffix};
use crate::tokens::{delimiters, spelling};
use crate::{Captures, ExpansionError, Pattern};

#[derive(Debug, Clone, Copy)]
pub enum OriginKind {
    Template,
    Capture,
}

#[derive(Debug, Clone)]
pub struct TokenOrigin {
    pub generated: Span,
    pub original: Span,
    pub kind: OriginKind,
}

#[derive(Debug)]
pub struct Generated {
    pub source: String,
    pub offset: usize,
    pub contexts: Arc<TokenContexts>,
    pub fragments: FragmentInputs,
    pub origins: Vec<TokenOrigin>,
}

/// Transcribe captures as parsed syntax with fresh spans and retained contexts.
///
/// # Errors
///
/// Returns an error for invalid template captures or repetitions, or when the
/// outer invocation's generated-node allowance is exhausted.
pub fn transcribe(
    pattern: &Pattern,
    template: &TokenTree,
    captures: &Captures,
    context: SyntaxContext,
    offset: usize,
    remaining_nodes: &mut usize,
) -> Result<Generated, ExpansionError> {
    let TokenTree::Group { children, .. } = template else {
        return Err(error(
            template,
            "a macro template must be a delimited group",
        ));
    };
    let mut writer = Writer {
        pattern,
        captures,
        context,
        remaining_nodes,
        source: String::new(),
        offset,
        contexts: TokenContexts::default(),
        fragments: FragmentInputs::default(),
        origins: Vec::new(),
    };
    writer.template_sequence(children, None)?;
    Ok(Generated {
        source: writer.source,
        offset,
        contexts: Arc::new(writer.contexts),
        fragments: writer.fragments,
        origins: writer.origins,
    })
}

struct Writer<'a> {
    pattern: &'a Pattern,
    captures: &'a Captures,
    context: SyntaxContext,
    remaining_nodes: &'a mut usize,
    source: String,
    offset: usize,
    contexts: TokenContexts,
    fragments: FragmentInputs,
    origins: Vec<TokenOrigin>,
}

impl Writer<'_> {
    fn template_sequence(
        &mut self,
        trees: &[TokenTree],
        iteration: Option<(usize, usize)>,
    ) -> Result<(), ExpansionError> {
        let mut cursor = 0;
        while let Some(tree) = trees.get(cursor) {
            if spelling(tree) != Some("$") {
                self.template_tree(tree, iteration)?;
                cursor += 1;
                continue;
            }
            let next = trees
                .get(cursor + 1)
                .ok_or_else(|| error(tree, "expected a capture after `$`"))?;
            if let TokenTree::Group {
                delimiter: hew_parser::macro_syntax::Delimiter::Parenthesis,
                children,
                ..
            } = next
            {
                if iteration.is_some() {
                    return Err(error(next, "nested macro repetitions are not supported"));
                }
                let (separator, kind, consumed) = repetition_suffix(&trees[cursor + 2..], next)?;
                let group = self.repetition_group(children)?;
                let count = self.captures.repetitions.get(&group).copied().unwrap_or(0);
                if !kind.permits(count) {
                    return Err(error(
                        next,
                        "template repetition does not match the number of captured fragments",
                    ));
                }
                self.leading(tree.leading());
                for index in 0..count {
                    if index > 0 && separator.is_some() {
                        self.template_tree(&trees[cursor + 2], None)?;
                    }
                    self.template_sequence(children, Some((group, index)))?;
                }
                cursor += 2 + consumed;
                continue;
            }
            let name = capture_name(next)?;
            let spec = self
                .pattern
                .captures
                .get(&name)
                .ok_or_else(|| error(next, "unknown macro capture"))?;
            let index = match (spec.repetition, iteration) {
                (None, _) => 0,
                (Some(group), Some((current, index))) if group == current => index,
                _ => {
                    return Err(error(
                        next,
                        "a repeated capture must appear in its template repetition",
                    ))
                }
            };
            let capture = self
                .captures
                .get(name)
                .get(index)
                .ok_or_else(|| error(next, "macro capture is missing from this repetition"))?;
            let tokens = Arc::clone(&capture.tokens);
            let kind = spec.kind;
            self.leading(tree.leading());
            let start = self.source.len();
            for (index, token) in tokens.iter().enumerate() {
                self.capture_tree(token, index == 0)?;
            }
            let end = self.source.len();
            if kind != FragmentKind::Ident {
                let parsed = hew_parser::parse_generated_fragment(
                    &self.source[start..end],
                    self.offset + start,
                    Arc::new(self.contexts.within(self.offset + start..self.offset + end)),
                    self.fragments
                        .within(self.offset + start..self.offset + end),
                    kind,
                );
                if !parsed.errors.is_empty() || parsed.consumed != end - start {
                    return Err(error(
                        next,
                        "captured syntax could not be inserted into the template",
                    ));
                }
                self.fragments.insert(
                    parsed
                        .fragment
                        .ok_or_else(|| error(next, "missing captured syntax"))?,
                );
            }
            cursor += 2;
        }
        Ok(())
    }

    fn template_tree(
        &mut self,
        tree: &TokenTree,
        iteration: Option<(usize, usize)>,
    ) -> Result<(), ExpansionError> {
        self.charge(tree)?;
        self.leading(tree.leading());
        match tree {
            TokenTree::Fragment { .. } => {
                return Err(error(
                    tree,
                    "a parsed capture cannot become template syntax",
                ));
            }
            TokenTree::Token { spelling, span, .. } => {
                self.token(spelling, span.clone(), self.context, OriginKind::Template);
            }
            TokenTree::Group {
                delimiter,
                span,
                children,
                closing,
                ..
            } => {
                let (open, close) = delimiters(*delimiter);
                self.token(
                    open,
                    span.start..span.start + open.len(),
                    self.context,
                    OriginKind::Template,
                );
                self.template_sequence(children, iteration)?;
                self.leading(closing);
                self.token(
                    close,
                    span.end - close.len()..span.end,
                    self.context,
                    OriginKind::Template,
                );
            }
        }
        Ok(())
    }

    fn capture_tree(&mut self, tree: &TokenTree, first: bool) -> Result<(), ExpansionError> {
        self.charge(tree)?;
        if !first {
            self.leading(tree.leading());
        }
        match tree {
            TokenTree::Fragment { syntax, tokens } => {
                let start = self.source.len();
                for (index, token) in tokens.iter().enumerate() {
                    self.capture_tree(token, index == 0)?;
                }
                let parsed = hew_parser::parse_generated_fragment(
                    &self.source[start..],
                    self.offset + start,
                    Arc::new(
                        self.contexts
                            .within(self.offset + start..self.offset + self.source.len()),
                    ),
                    self.fragments
                        .within(self.offset + start..self.offset + self.source.len()),
                    syntax.kind(),
                );
                if !parsed.errors.is_empty() || parsed.consumed != self.source.len() - start {
                    return Err(error(
                        tree,
                        "captured syntax could not retain its fragment boundary",
                    ));
                }
                self.fragments.insert(
                    parsed
                        .fragment
                        .ok_or_else(|| error(tree, "missing captured syntax"))?,
                );
            }
            TokenTree::Token {
                spelling,
                span,
                context,
                ..
            } => {
                self.token(spelling, span.clone(), *context, OriginKind::Capture);
            }
            TokenTree::Group {
                delimiter,
                span,
                children,
                closing,
                ..
            } => {
                let (open, close) = delimiters(*delimiter);
                self.token(
                    open,
                    span.start..span.start + open.len(),
                    SyntaxContext::ROOT,
                    OriginKind::Capture,
                );
                for child in children {
                    self.capture_tree(child, false)?;
                }
                self.leading(closing);
                self.token(
                    close,
                    span.end - close.len()..span.end,
                    SyntaxContext::ROOT,
                    OriginKind::Capture,
                );
            }
        }
        Ok(())
    }

    fn repetition_group(&self, trees: &[TokenTree]) -> Result<usize, ExpansionError> {
        let mut groups = HashSet::new();
        self.collect_repetition_groups(trees, &mut groups)?;
        if groups.len() != 1 {
            return Err(ExpansionError {
                message: "a template repetition must use captures from one pattern repetition"
                    .to_string(),
                span: crate::tokens::span(trees),
            });
        }
        Ok(*groups.iter().next().expect("exactly one repetition group"))
    }

    fn collect_repetition_groups(
        &self,
        trees: &[TokenTree],
        groups: &mut HashSet<usize>,
    ) -> Result<(), ExpansionError> {
        let mut cursor = 0;
        while let Some(tree) = trees.get(cursor) {
            if spelling(tree) == Some("$") {
                let next = trees
                    .get(cursor + 1)
                    .ok_or_else(|| error(tree, "expected a capture after `$`"))?;
                let name = capture_name(next)?;
                let spec = self
                    .pattern
                    .captures
                    .get(&name)
                    .ok_or_else(|| error(next, "unknown macro capture"))?;
                if let Some(group) = spec.repetition {
                    groups.insert(group);
                }
                cursor += 2;
            } else {
                if let TokenTree::Group { children, .. } = tree {
                    self.collect_repetition_groups(children, groups)?;
                }
                cursor += 1;
            }
        }
        Ok(())
    }

    fn charge(&mut self, tree: &TokenTree) -> Result<(), ExpansionError> {
        let Some(remaining) = self.remaining_nodes.checked_sub(1) else {
            return Err(error(
                tree,
                "macro expansion exceeded one million generated nodes",
            ));
        };
        *self.remaining_nodes = remaining;
        Ok(())
    }

    fn leading(&mut self, trivia: &str) {
        if trivia.is_empty() {
            if self
                .source
                .as_bytes()
                .last()
                .is_some_and(|byte| !byte.is_ascii_whitespace())
            {
                self.source.push(' ');
            }
        } else {
            self.source.push_str(trivia);
        }
    }

    fn token(&mut self, spelling: &str, original: Span, context: SyntaxContext, kind: OriginKind) {
        let start = self.offset + self.source.len();
        self.source.push_str(spelling);
        let generated = start..self.offset + self.source.len();
        self.contexts.insert(generated.clone(), context);
        self.origins.push(TokenOrigin {
            generated,
            original,
            kind,
        });
    }
}

fn error(tree: &TokenTree, message: &str) -> ExpansionError {
    ExpansionError {
        message: message.to_string(),
        span: tree.span().clone(),
    }
}
