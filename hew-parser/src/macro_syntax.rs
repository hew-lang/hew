use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::path::PathBuf;

use crate::ast::{
    Block, Expr, Ident, Path, Pattern, Span, Spanned, SyntaxContext, TypeExpr, Visibility,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Delimiter {
    Parenthesis,
    Bracket,
    Brace,
    Attribute,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum TokenTree {
    Token {
        spelling: String,
        span: Span,
        context: SyntaxContext,
        leading: String,
    },
    Group {
        delimiter: Delimiter,
        span: Span,
        children: Vec<TokenTree>,
        leading: String,
        closing: String,
    },
}

impl TokenTree {
    #[must_use]
    pub fn leading(&self) -> &str {
        match self {
            Self::Token { leading, .. } | Self::Group { leading, .. } => leading,
        }
    }

    #[must_use]
    pub fn span(&self) -> &Span {
        match self {
            Self::Token { span, .. } | Self::Group { span, .. } => span,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum FragmentKind {
    Ident,
    Expr,
    Ty,
    Pat,
    Block,
    Literal,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Fragment {
    Ident(Spanned<Ident>),
    Expr(Spanned<Expr>),
    Ty(Spanned<TypeExpr>),
    Pat(Spanned<Pattern>),
    Block(Spanned<Block>),
    Literal(Spanned<Expr>),
}

impl Fragment {
    #[must_use]
    pub fn span(&self) -> &Span {
        match self {
            Self::Ident((_, span))
            | Self::Expr((_, span))
            | Self::Ty((_, span))
            | Self::Pat((_, span))
            | Self::Block((_, span))
            | Self::Literal((_, span)) => span,
        }
    }
}

/// Contexts on generated tokens, including identifiers inside string interpolation.
#[derive(Debug, Clone, Default)]
pub struct TokenContexts {
    spans: BTreeMap<usize, (usize, SyntaxContext)>,
}

impl TokenContexts {
    pub fn insert(&mut self, span: Span, context: SyntaxContext) {
        self.spans.insert(span.start, (span.end, context));
    }

    #[must_use]
    pub fn at(&self, span: &Span) -> SyntaxContext {
        self.spans
            .range(..=span.start)
            .next_back()
            .filter(|(_, (end, _))| span.end <= *end)
            .map_or(SyntaxContext::ROOT, |(_, (_, context))| *context)
    }
}

/// Parsed captures occupy one grammar node when inserted into a template.
#[derive(Debug, Clone, Default)]
pub struct FragmentInputs {
    fragments: BTreeMap<usize, Fragment>,
}

impl FragmentInputs {
    pub fn insert(&mut self, fragment: Fragment) {
        self.fragments.insert(fragment.span().start, fragment);
    }

    pub(crate) fn at(&self, start: usize) -> Option<&Fragment> {
        self.fragments.get(&start)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum ExpansionCategory {
    Expr,
    Items,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct MacroRule {
    pub pattern: TokenTree,
    pub template: TokenTree,
    pub category: ExpansionCategory,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct MacroDecl {
    pub visibility: Visibility,
    pub name: Ident,
    pub rules: Vec<MacroRule>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct MacroCall {
    pub path: Path,
    pub input: TokenTree,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct ExpansionId(pub u32);

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ContextRow {
    pub parent: SyntaxContext,
    pub expansion: ExpansionId,
    pub definition_source: PathBuf,
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct SyntaxContexts {
    pub rows: Vec<ContextRow>,
}

impl SyntaxContexts {
    /// Mint a fresh template context anchored to its defining source file.
    ///
    /// # Panics
    ///
    /// Panics if one compilation contains more than `u32::MAX` contexts.
    pub fn mint(
        &mut self,
        parent: SyntaxContext,
        expansion: ExpansionId,
        definition_source: PathBuf,
    ) -> SyntaxContext {
        let context = SyntaxContext::from_row(
            u32::try_from(self.rows.len() + 1).expect("more than u32::MAX syntax contexts"),
        );
        self.rows.push(ContextRow {
            parent,
            expansion,
            definition_source,
        });
        context
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum Origin {
    Caller { file: PathBuf, span: Span },
    Template { file: PathBuf, span: Span },
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Expansion {
    pub file: PathBuf,
    pub synthetic: Span,
    pub invocation: Origin,
    pub definition: Origin,
    pub parent: Option<ExpansionId>,
    pub origins: Vec<(Span, Origin)>,
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct ExpansionMap {
    pub contexts: SyntaxContexts,
    pub expansions: Vec<Expansion>,
    pub sources: BTreeMap<PathBuf, String>,
}

impl ExpansionMap {
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.contexts.rows.is_empty() && self.expansions.is_empty() && self.sources.is_empty()
    }
}
