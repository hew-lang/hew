use std::collections::HashMap;
use std::sync::Arc;

use hew_parser::ast::{Pattern as SyntaxPattern, Spanned, Symbol};
use hew_parser::macro_syntax::{Delimiter, Fragment, FragmentKind, TokenTree};
use hew_parser::parser::Parser;

use crate::tokens::{delimiters, source, spelling};
use crate::ExpansionError;

#[derive(Debug, Clone)]
pub struct Capture {
    pub syntax: Arc<Fragment>,
    pub tokens: Arc<[TokenTree]>,
}

#[derive(Debug, Clone, Default)]
pub struct Captures {
    pub(crate) function: Option<hew_parser::macro_syntax::FunctionInput>,
    pub(crate) values: HashMap<Symbol, Vec<Capture>>,
    pub(crate) repetitions: HashMap<usize, usize>,
}

#[derive(Debug, Default)]
struct MatchState {
    captures: Vec<(Symbol, Capture)>,
    repetitions: Vec<(usize, usize)>,
}

#[derive(Debug, Clone, Copy)]
struct Checkpoint {
    captures: usize,
    repetitions: usize,
}

impl MatchState {
    fn checkpoint(&self) -> Checkpoint {
        Checkpoint {
            captures: self.captures.len(),
            repetitions: self.repetitions.len(),
        }
    }

    fn rewind(&mut self, checkpoint: Checkpoint) {
        self.captures.truncate(checkpoint.captures);
        self.repetitions.truncate(checkpoint.repetitions);
    }

    fn finish(self) -> Captures {
        let mut result = Captures::default();
        for (name, capture) in self.captures {
            result.values.entry(name).or_default().push(capture);
        }
        result.repetitions.extend(self.repetitions);
        result
    }
}

impl Captures {
    #[must_use]
    pub fn get(&self, name: Symbol) -> &[Capture] {
        self.values.get(&name).map_or(&[], Vec::as_slice)
    }
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct CaptureSpec {
    pub kind: FragmentKind,
    pub repetition: Option<usize>,
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum RepeatKind {
    ZeroOrMore,
    OneOrMore,
    Optional,
}

impl RepeatKind {
    pub(crate) fn from_spelling(spelling: &str) -> Option<Self> {
        match spelling {
            "*" => Some(Self::ZeroOrMore),
            "+" => Some(Self::OneOrMore),
            "?" => Some(Self::Optional),
            _ => None,
        }
    }

    pub(crate) fn permits(self, count: usize) -> bool {
        match self {
            Self::ZeroOrMore => true,
            Self::OneOrMore => count > 0,
            Self::Optional => count <= 1,
        }
    }
}

#[derive(Debug, Clone)]
enum Element {
    Token(String),
    Group(Delimiter, Vec<Self>),
    Capture(Symbol, FragmentKind),
    Repeat {
        id: usize,
        body: Vec<Self>,
        separator: Option<String>,
        kind: RepeatKind,
    },
}

#[derive(Debug, Clone)]
pub struct Pattern {
    pub(crate) attribute: Option<hew_parser::macro_syntax::AttributePattern>,
    elements: Vec<Element>,
    pub(crate) captures: HashMap<Symbol, CaptureSpec>,
    repetitions: usize,
}

impl Pattern {
    /// Validate and compile a parenthesized macro pattern.
    ///
    /// # Errors
    ///
    /// Returns an error for invalid captures, follow sets or repetitions.
    pub fn compile(tree: &TokenTree) -> Result<Self, ExpansionError> {
        let TokenTree::Group {
            delimiter: Delimiter::Parenthesis,
            children,
            ..
        } = tree
        else {
            return Err(error(tree, "a macro pattern must use parentheses"));
        };
        let mut pattern = Self {
            attribute: None,
            elements: Vec::new(),
            captures: HashMap::new(),
            repetitions: 0,
        };
        pattern.elements = pattern.compile_sequence(children, None, ")")?;
        Ok(pattern)
    }

    #[must_use]
    pub fn matches(&self, tree: &TokenTree) -> Option<Captures> {
        let TokenTree::Group {
            delimiter: Delimiter::Parenthesis,
            children,
            ..
        } = tree
        else {
            return None;
        };
        let (text, offset, contexts, fragments) = source(children);
        let mut parser =
            Parser::with_contexts(&text, offset, Arc::new(contexts)).with_fragments(fragments);
        let mut state = MatchState::default();
        let end = match_sequence(&self.elements, children, 0, &mut state, &mut parser)?;
        (end == children.len()).then(|| state.finish())
    }

    pub(crate) fn placeholders(&self) -> Captures {
        let mut captures = Captures {
            function: self.attribute.as_ref().map(crate::attributes::placeholder),
            ..Captures::default()
        };
        for (&name, spec) in &self.captures {
            let text = match spec.kind {
                FragmentKind::Ident => format!("__capture_{name}"),
                FragmentKind::Expr | FragmentKind::Literal => "0".into(),
                FragmentKind::Ty => "i32".into(),
                FragmentKind::Pat => "_".into(),
                FragmentKind::Block => "{}".into(),
            };
            let parsed = hew_parser::parse_fragment(&text, 0, Arc::default(), spec.kind);
            captures.values.insert(
                name,
                vec![Capture {
                    syntax: Arc::new(parsed.fragment.expect("placeholder is valid syntax")),
                    tokens: Arc::from([TokenTree::Token {
                        spelling: text.clone(),
                        span: 0..text.len(),
                        context: hew_parser::ast::SyntaxContext::ROOT,
                        leading: String::new(),
                    }]),
                }],
            );
            if let Some(group) = spec.repetition {
                captures.repetitions.insert(group, 1);
            }
        }
        captures
    }

    #[allow(clippy::too_many_lines, reason = "one cursor-driven pattern grammar")]
    fn compile_sequence(
        &mut self,
        trees: &[TokenTree],
        repetition: Option<usize>,
        closing: &str,
    ) -> Result<Vec<Element>, ExpansionError> {
        let mut elements = Vec::new();
        let mut cursor = 0;
        while let Some(tree) = trees.get(cursor) {
            if spelling(tree) != Some("$") {
                elements.push(match tree {
                    TokenTree::Fragment { .. } => {
                        return Err(error(
                            tree,
                            "a macro pattern cannot inspect a parsed capture",
                        ));
                    }
                    TokenTree::Token { spelling, .. }
                    | TokenTree::Interpolated { spelling, .. } => Element::Token(spelling.clone()),
                    TokenTree::Group {
                        delimiter,
                        children,
                        ..
                    } => Element::Group(
                        *delimiter,
                        self.compile_sequence(children, repetition, delimiters(*delimiter).1)?,
                    ),
                });
                cursor += 1;
                continue;
            }
            let next = trees
                .get(cursor + 1)
                .ok_or_else(|| error(tree, "expected a capture after `$`"))?;
            if let TokenTree::Group {
                delimiter: Delimiter::Parenthesis,
                children,
                ..
            } = next
            {
                if repetition.is_some() {
                    return Err(error(next, "nested macro repetitions are not supported"));
                }
                let (separator, kind, consumed) = repetition_suffix(&trees[cursor + 2..], next)?;
                let id = self.repetitions;
                self.repetitions += 1;
                let body = self.compile_sequence(
                    children,
                    Some(id),
                    separator.as_deref().unwrap_or(closing),
                )?;
                if body.is_empty() {
                    return Err(error(next, "a repeated macro pattern must consume input"));
                }
                elements.push(Element::Repeat {
                    id,
                    body,
                    separator,
                    kind,
                });
                cursor += 2 + consumed;
                continue;
            }
            let name = capture_name(next)?;
            if trees.get(cursor + 2).and_then(spelling) != Some(":") {
                return Err(error(
                    next,
                    "a pattern capture needs a fragment kind: `$name:expr`",
                ));
            }
            let kind_tree = trees
                .get(cursor + 3)
                .ok_or_else(|| error(next, "expected a fragment kind"))?;
            let kind = match spelling(kind_tree) {
                Some("ident") => FragmentKind::Ident,
                Some("expr") => FragmentKind::Expr,
                Some("ty") => FragmentKind::Ty,
                Some("pat") => FragmentKind::Pat,
                Some("block") => FragmentKind::Block,
                Some("literal") => FragmentKind::Literal,
                _ => {
                    return Err(error(
                        kind_tree,
                        "expected `ident`, `expr`, `ty`, `pat`, `block` or `literal`",
                    ))
                }
            };
            let follow = trees.get(cursor + 4).map_or(Some(closing), spelling);
            if !valid_follow(kind, follow) {
                return Err(error(
                    kind_tree,
                    "this token cannot follow the captured fragment",
                ));
            }
            if self
                .captures
                .insert(name, CaptureSpec { kind, repetition })
                .is_some()
            {
                return Err(error(
                    next,
                    "a capture name may occur only once in a macro pattern",
                ));
            }
            elements.push(Element::Capture(name, kind));
            cursor += 4;
        }
        Ok(elements)
    }
}

fn valid_follow(kind: FragmentKind, follow: Option<&str>) -> bool {
    match kind {
        FragmentKind::Expr => matches!(follow, Some("," | ";" | ")" | "]" | "}")),
        FragmentKind::Ty => matches!(follow, Some("," | ";" | ")" | "]" | "}" | "=")),
        FragmentKind::Pat => matches!(follow, Some("=>" | "," | "|" | "if" | ")" | "]" | "}")),
        FragmentKind::Ident | FragmentKind::Block | FragmentKind::Literal => true,
    }
}

pub(crate) fn capture_name(tree: &TokenTree) -> Result<Symbol, ExpansionError> {
    let Some(name) = spelling(tree) else {
        return Err(error(tree, "expected a capture name"));
    };
    let mut chars = name.chars();
    if !chars
        .next()
        .is_some_and(|c| c.is_ascii_alphabetic() || c == '_')
        || !chars.all(|c| c.is_ascii_alphanumeric() || c == '_')
    {
        return Err(error(tree, "expected a capture name"));
    }
    Ok(Symbol::intern(name))
}

pub(crate) fn repetition_suffix(
    trees: &[TokenTree],
    group: &TokenTree,
) -> Result<(Option<String>, RepeatKind, usize), ExpansionError> {
    if let Some(kind) = trees
        .first()
        .and_then(spelling)
        .and_then(RepeatKind::from_spelling)
    {
        return Ok((None, kind, 1));
    }
    let separator = trees.first().and_then(spelling);
    let kind = trees
        .get(1)
        .and_then(spelling)
        .and_then(RepeatKind::from_spelling);
    match (separator, kind) {
        (Some(separator @ ("," | ";")), Some(kind)) if !matches!(kind, RepeatKind::Optional) => {
            Ok((Some(separator.to_string()), kind, 2))
        }
        _ => Err(error(
            group,
            "a repetition needs `*`, `+` or `?`, with an optional comma or semicolon separator",
        )),
    }
}

fn match_sequence(
    elements: &[Element],
    input: &[TokenTree],
    cursor: usize,
    state: &mut MatchState,
    parser: &mut Parser<'_>,
) -> Option<usize> {
    let checkpoint = state.checkpoint();
    let result = match_sequence_inner(elements, input, cursor, state, parser);
    if result.is_none() {
        state.rewind(checkpoint);
    }
    result
}

#[allow(
    clippy::too_many_lines,
    reason = "backtracking shares one capture checkpoint state"
)]
fn match_sequence_inner(
    mut elements: &[Element],
    input: &[TokenTree],
    mut cursor: usize,
    state: &mut MatchState,
    parser: &mut Parser<'_>,
) -> Option<usize> {
    while let Some((first, rest)) = elements.split_first() {
        match first {
            Element::Token(expected) => {
                if input.get(cursor).and_then(spelling) != Some(expected.as_str()) {
                    return None;
                }
                cursor += 1;
            }
            Element::Group(expected, body) => {
                let TokenTree::Group {
                    delimiter,
                    children,
                    ..
                } = input.get(cursor)?
                else {
                    return None;
                };
                if delimiter != expected
                    || match_sequence(body, children, 0, state, parser)? != children.len()
                {
                    return None;
                }
                cursor += 1;
            }
            Element::Capture(name, kind) => {
                let remaining = input.get(cursor..)?;
                let (consumed, capture) = parse_capture(*kind, remaining, parser)?;
                let prefixes = match rest.first() {
                    Some(Element::Token(follow)) if *kind == FragmentKind::Pat && follow == "|" => {
                        pattern_prefixes(&capture, remaining)
                    }
                    _ => Vec::new(),
                };
                if prefixes.is_empty() {
                    state.captures.push((*name, capture));
                    cursor += consumed;
                } else {
                    let checkpoint = state.checkpoint();
                    state.captures.push((*name, capture.clone()));
                    if let Some(end) = match_sequence(rest, input, cursor + consumed, state, parser)
                    {
                        return Some(end);
                    }
                    state.rewind(checkpoint);
                    for (consumed, pattern) in prefixes {
                        let checkpoint = state.checkpoint();
                        state.captures.push((
                            *name,
                            Capture {
                                syntax: Arc::new(Fragment::Pat(pattern.clone())),
                                tokens: Arc::from(&remaining[..consumed]),
                            },
                        ));
                        if let Some(end) =
                            match_sequence(rest, input, cursor + consumed, state, parser)
                        {
                            return Some(end);
                        }
                        state.rewind(checkpoint);
                    }
                    return None;
                }
            }
            Element::Repeat {
                id,
                body,
                separator,
                kind,
            } => {
                let mut states = vec![(cursor, state.checkpoint())];
                loop {
                    let (mut next, _) = *states.last()?;
                    if states.len() > 1 {
                        if matches!(kind, RepeatKind::Optional) {
                            break;
                        }
                        if let Some(separator) = separator {
                            if input.get(next).and_then(spelling) != Some(separator.as_str()) {
                                break;
                            }
                            next += 1;
                        }
                    }
                    let Some(end) = match_sequence(body, input, next, state, parser) else {
                        break;
                    };
                    if end == next {
                        break;
                    }
                    states.push((end, state.checkpoint()));
                }
                for (count, (end, checkpoint)) in states.into_iter().enumerate().rev() {
                    state.rewind(checkpoint);
                    if kind.permits(count) {
                        state.repetitions.push((*id, count));
                        if let Some(end) = match_sequence(rest, input, end, state, parser) {
                            return Some(end);
                        }
                    }
                }
                return None;
            }
        }
        elements = rest;
    }
    Some(cursor)
}

fn parse_capture(
    kind: FragmentKind,
    trees: &[TokenTree],
    parser: &mut Parser<'_>,
) -> Option<(usize, Capture)> {
    if let TokenTree::Fragment { syntax, .. } = trees.first()? {
        if syntax.kind() != kind {
            return None;
        }
        return Some((
            1,
            Capture {
                syntax: Arc::new((**syntax).clone()),
                tokens: Arc::from(&trees[..1]),
            },
        ));
    }
    let start = trees.first()?.span().start;
    let parsed = parser.parse_fragment_at(start, kind);
    if !parsed.errors.is_empty() {
        return None;
    }
    let syntax = parsed.fragment?;
    let end = start + parsed.consumed;
    let count = trees.partition_point(|tree| tree.span().end <= end);
    if count == 0 || trees[count - 1].span().end != end {
        return None;
    }
    Some((
        count,
        Capture {
            syntax: Arc::new(syntax),
            tokens: Arc::from(&trees[..count]),
        },
    ))
}

fn pattern_prefixes<'a>(
    capture: &'a Capture,
    trees: &[TokenTree],
) -> Vec<(usize, &'a Spanned<SyntaxPattern>)> {
    let Fragment::Pat(pattern) = capture.syntax.as_ref() else {
        return Vec::new();
    };
    let mut pattern = pattern;
    let mut prefixes = Vec::new();
    while let SyntaxPattern::Or(left, _) = &pattern.0 {
        pattern = left;
        let count = trees.partition_point(|tree| tree.span().end <= pattern.1.end);
        if count > 0 && trees.get(count).and_then(spelling) == Some("|") {
            prefixes.push((count, pattern));
        }
    }
    prefixes
}

fn error(tree: &TokenTree, message: &str) -> ExpansionError {
    ExpansionError {
        message: message.to_string(),
        span: tree.span().clone(),
    }
}
