use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::path::{Path as FilePath, PathBuf};

use crate::ast::{
    Block, Expr, Ident, Path, Pattern, Span, Spanned, SyntaxContext, TypeExpr, Visibility,
};

/// Built-ins share the macro call spelling but remain compiler-owned.
#[must_use]
pub fn is_builtin_macro(name: &str) -> bool {
    matches!(
        name,
        "stringify" | "location" | "include_str" | "include_bytes" | "embed"
    )
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Delimiter {
    Parenthesis,
    Bracket,
    Brace,
    Attribute,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum TokenTree {
    Interpolated {
        spelling: String,
        span: Span,
        leading: String,
        expressions: Vec<(Span, Vec<TokenTree>)>,
    },
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
    Fragment {
        syntax: Box<Fragment>,
        tokens: Vec<TokenTree>,
    },
}

impl TokenTree {
    /// Write the token tree without depending on a source buffer.
    pub fn write_source(&self, output: &mut String) {
        match self {
            Self::Token {
                spelling, leading, ..
            }
            | Self::Interpolated {
                spelling, leading, ..
            } => {
                output.push_str(leading);
                output.push_str(spelling);
            }
            Self::Group {
                delimiter,
                children,
                leading,
                closing,
                ..
            } => {
                output.push_str(leading);
                let (open, close) = match delimiter {
                    Delimiter::Parenthesis => ("(", ")"),
                    Delimiter::Bracket => ("[", "]"),
                    Delimiter::Brace => ("{", "}"),
                    Delimiter::Attribute => ("#[", "]"),
                };
                output.push_str(open);
                for child in children {
                    child.write_source(output);
                }
                output.push_str(closing);
                output.push_str(close);
            }
            Self::Fragment { tokens, .. } => {
                for token in tokens {
                    token.write_source(output);
                }
            }
        }
    }

    #[must_use]
    pub fn leading(&self) -> &str {
        match self {
            Self::Token { leading, .. }
            | Self::Group { leading, .. }
            | Self::Interpolated { leading, .. } => leading,
            Self::Fragment { tokens, .. } => tokens.first().map_or("", Self::leading),
        }
    }

    #[must_use]
    pub fn span(&self) -> &Span {
        match self {
            Self::Token { span, .. }
            | Self::Group { span, .. }
            | Self::Interpolated { span, .. } => span,
            Self::Fragment { syntax, .. } => syntax.span(),
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

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
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
    pub fn kind(&self) -> FragmentKind {
        match self {
            Self::Ident(_) => FragmentKind::Ident,
            Self::Expr(_) => FragmentKind::Expr,
            Self::Ty(_) => FragmentKind::Ty,
            Self::Pat(_) => FragmentKind::Pat,
            Self::Block(_) => FragmentKind::Block,
            Self::Literal(_) => FragmentKind::Literal,
        }
    }

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
    #[must_use]
    pub fn within(&self, span: Span) -> Self {
        Self {
            spans: self
                .spans
                .range(span)
                .map(|(&start, &value)| (start, value))
                .collect(),
        }
    }

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
    #[must_use]
    pub fn within(&self, span: Span) -> Self {
        Self {
            fragments: self
                .fragments
                .range(span)
                .map(|(&start, value)| (start, value.clone()))
                .collect(),
        }
    }

    pub(crate) fn remove(&mut self, start: usize) {
        self.fragments.remove(&start);
    }

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
pub struct AttributePattern {
    pub name: Ident,
    pub method: bool,
    pub fails: bool,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct FunctionInput {
    pub function: crate::ast::FnDecl,
    pub tokens: Vec<TokenTree>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct MacroRule {
    pub attribute: Option<AttributePattern>,
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

#[derive(Debug)]
pub struct ExpansionInput {
    pub source: String,
    pub offset: usize,
    pub contexts: std::sync::Arc<TokenContexts>,
    pub fragments: FragmentInputs,
}

pub trait ExpansionProvider: std::fmt::Debug {
    /// Begin expanding a macro invocation in the requested grammar category.
    ///
    /// # Errors
    /// Returns a diagnostic if resolution, matching or expansion fails.
    fn begin(
        &mut self,
        call: &MacroCall,
        category: ExpansionCategory,
        source: &std::path::Path,
    ) -> Result<ExpansionInput, String>;

    /// Begin expanding an attribute macro around its captured function.
    ///
    /// # Errors
    /// Returns a diagnostic if resolution, matching or expansion fails.
    fn begin_attribute(
        &mut self,
        call: &MacroCall,
        function: &FunctionInput,
        span: Span,
        source: &std::path::Path,
    ) -> Result<ExpansionInput, String>;

    /// Resolve an attribute macro before parsing its carrier.
    ///
    /// # Errors
    /// Returns a diagnostic if the path does not resolve to an attribute macro.
    fn resolve_attribute(&self, call: &MacroCall, source: &std::path::Path) -> Result<(), String>;

    fn complete(&mut self);

    /// Register a macro declaration encountered during expansion.
    ///
    /// # Errors
    /// Returns a diagnostic if the declaration is invalid or conflicts with a name.
    fn declare(
        &mut self,
        declaration: &MacroDecl,
        span: Span,
        source: &std::path::Path,
    ) -> Result<(), String>;
}

pub type SharedExpander = std::rc::Rc<std::cell::RefCell<dyn ExpansionProvider>>;

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
    pub captured_bodies: Vec<Span>,
    pub name: String,
    pub file: PathBuf,
    pub synthetic: Span,
    pub invocation: Origin,
    pub callee: Origin,
    pub definition: Origin,
    pub parent: Option<ExpansionId>,
    pub origins: Vec<(Span, Origin)>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum TemplateBody {
    Expr(Spanned<Expr>),
    Items(Vec<Spanned<crate::ast::Item>>),
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct MacroTemplate {
    pub source: PathBuf,
    /// The rule's original extent in its defining source.
    pub definition_span: Span,
    pub visibility: Visibility,
    pub body: TemplateBody,
    pub origins: Vec<(Span, Origin)>,
}

impl MacroTemplate {
    #[must_use]
    pub fn origin(&self, span: &Span) -> Option<&Origin> {
        self.origins
            .iter()
            .find(|(generated, _)| generated.contains(&span.start))
            .map(|(_, original)| original)
    }
}

#[derive(Debug, Clone, Default, PartialEq, Serialize, Deserialize)]
pub struct ExpansionMap {
    pub templates: Vec<MacroTemplate>,
    pub used_imports: Vec<(PathBuf, Span)>,
    pub contexts: SyntaxContexts,
    pub expansions: Vec<Expansion>,
    pub sources: BTreeMap<PathBuf, String>,
}

impl ExpansionMap {
    /// Resolve a generated position in its owning file to the token that wrote it.
    #[must_use]
    pub fn source_origin(&self, source: &FilePath, span: &Span) -> Option<(PathBuf, Span)> {
        let expansion = self.expansions.iter().find(|expansion| {
            expansion.file == source && expansion.synthetic.contains(&span.start)
        })?;
        let origin = expansion
            .origins
            .iter()
            .find(|(generated, _)| generated.contains(&span.start))
            .map_or(&expansion.invocation, |(_, origin)| origin);
        let (Origin::Caller { file, span } | Origin::Template { file, span }) = origin;
        self.source_origin(file, span)
            .or_else(|| Some((file.clone(), span.clone())))
    }

    /// The outermost invocation containing generated syntax in its owning file.
    #[must_use]
    pub fn invocation_origin(&self, source: &FilePath, span: &Span) -> Option<(PathBuf, Span)> {
        let expansion = self.expansions.iter().find(|expansion| {
            expansion.file == source && expansion.synthetic.contains(&span.start)
        })?;
        let (Origin::Caller { file, span } | Origin::Template { file, span }) =
            &expansion.invocation;
        self.invocation_origin(file, span)
            .or_else(|| Some((file.clone(), span.clone())))
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.templates.is_empty()
            && self.contexts.rows.is_empty()
            && self.expansions.is_empty()
            && self.sources.is_empty()
    }
}
