use std::collections::HashMap;
use std::sync::Arc;

use hew_parser::ast::Symbol;
use hew_parser::macro_syntax::{Delimiter, Fragment, FragmentKind, TokenTree};

use crate::tokens::{delimiters, source, spelling};
use crate::ExpansionError;

#[derive(Debug, Clone)]
pub struct Capture {
    pub syntax: Arc<Fragment>,
    pub tokens: Arc<[TokenTree]>,
}

#[derive(Debug, Clone, Default)]
pub struct Captures {
    pub(crate) values: HashMap<Symbol, Vec<Capture>>,
    pub(crate) repetitions: HashMap<usize, usize>,
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
        let (end, captures) = match_sequence(&self.elements, children, 0, Captures::default())?;
        (end == children.len()).then_some(captures)
    }

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
                    TokenTree::Token { spelling, .. } => Element::Token(spelling.clone()),
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
    captures: Captures,
) -> Option<(usize, Captures)> {
    let Some((first, rest)) = elements.split_first() else {
        return Some((cursor, captures));
    };
    match first {
        Element::Token(expected) => (input.get(cursor).and_then(spelling)
            == Some(expected.as_str()))
        .then(|| match_sequence(rest, input, cursor + 1, captures))?,
        Element::Group(expected, elements) => {
            let TokenTree::Group {
                delimiter,
                children,
                ..
            } = input.get(cursor)?
            else {
                return None;
            };
            if delimiter != expected {
                return None;
            }
            let (end, captures) = match_sequence(elements, children, 0, captures)?;
            (end == children.len()).then(|| match_sequence(rest, input, cursor + 1, captures))?
        }
        Element::Capture(name, kind) => {
            let remaining = input.get(cursor..)?;
            let (consumed, capture) = parse_capture(*kind, remaining)?;
            let mut candidates = vec![(consumed, capture)];
            if let Some(Element::Token(follow)) = rest.first() {
                if remaining.get(consumed).and_then(spelling) != Some(follow.as_str()) {
                    for end in (1..consumed).rev() {
                        if remaining.get(end).and_then(spelling) == Some(follow.as_str()) {
                            if let Some(candidate) = parse_capture(*kind, &remaining[..end]) {
                                if candidate.0 == end {
                                    candidates.push(candidate);
                                }
                            }
                        }
                    }
                }
            }
            for (consumed, capture) in candidates {
                let mut trial = captures.clone();
                trial.values.entry(*name).or_default().push(capture);
                if let Some(result) = match_sequence(rest, input, cursor + consumed, trial) {
                    return Some(result);
                }
            }
            None
        }
        Element::Repeat {
            id,
            body,
            separator,
            kind,
        } => {
            let mut states = vec![(cursor, captures)];
            loop {
                let (mut next, captures) = states.last()?.clone();
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
                let Some((end, captures)) = match_sequence(body, input, next, captures) else {
                    break;
                };
                if end == next {
                    break;
                }
                states.push((end, captures));
            }
            for (count, (end, mut captures)) in states.into_iter().enumerate().rev() {
                if kind.permits(count) {
                    captures.repetitions.insert(*id, count);
                    if let Some(result) = match_sequence(rest, input, end, captures) {
                        return Some(result);
                    }
                }
            }
            None
        }
    }
}

fn parse_capture(kind: FragmentKind, trees: &[TokenTree]) -> Option<(usize, Capture)> {
    if trees.is_empty() {
        return None;
    }
    let (text, offset, contexts) = source(trees);
    let parsed = hew_parser::parse_fragment(&text, offset, Arc::new(contexts), kind);
    if !parsed.errors.is_empty() {
        return None;
    }
    let syntax = parsed.fragment?;
    let end = offset + parsed.consumed;
    let count = trees
        .iter()
        .take_while(|tree| tree.span().end <= end)
        .count();
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

fn error(tree: &TokenTree, message: &str) -> ExpansionError {
    ExpansionError {
        message: message.to_string(),
        span: tree.span().clone(),
    }
}
