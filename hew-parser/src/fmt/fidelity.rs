//! Source-fidelity oracle for the formatter.
//!
//! `hew fmt` may change whitespace and punctuation, and nothing else.
//! [`check`] proves that for one input by comparing:
//!
//! - the parsed programs, ignoring spans;
//! - the trivia trace: every significant token and every comment in source
//!   order, with each comment marked as trailing (code before it on its line)
//!   or own-line. Punctuation other than braces is left out: choosing it is
//!   the formatter's job, and the AST comparison covers its meaning.
//!
//! The AST comparison proves the program means the same thing. The trace
//! proves that declarations and members kept their order and that each comment
//! stayed between the same two tokens with the same attachment.

use std::fmt::Write as _;

use hew_lexer::Token;

use super::extract_comments;
use crate::ast_eq::program_eq_ignoring_spans;

/// One element of a source's trivia trace.
#[derive(Debug, Clone, PartialEq, Eq)]
enum TraceItem {
    Token(String),
    Comment { text: String, trailing: bool },
}

impl std::fmt::Display for TraceItem {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TraceItem::Token(t) => f.write_str(t),
            TraceItem::Comment {
                text,
                trailing: true,
            } => write!(f, "trailing comment {text:?}"),
            TraceItem::Comment {
                text,
                trailing: false,
            } => write!(f, "own-line comment {text:?}"),
        }
    }
}

/// Why a formatted text does not faithfully reprint its source.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FidelityError {
    /// The formatted text no longer parses.
    Reparse(String),
    /// The formatted text parses to a different program.
    AstChanged,
    /// A token or comment moved, appeared or vanished.
    TraceChanged {
        /// Byte offset in the formatted text nearest the first divergence.
        formatted_offset: usize,
        /// What the source has at the divergence.
        expected: String,
        /// What the formatted text has at the divergence.
        found: String,
    },
}

impl std::fmt::Display for FidelityError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FidelityError::Reparse(e) => write!(f, "formatted output does not parse: {e}"),
            FidelityError::AstChanged => f.write_str("formatted output parses to a different program"),
            FidelityError::TraceChanged {
                formatted_offset,
                expected,
                found,
            } => write!(
                f,
                "formatted output diverges at byte {formatted_offset}: expected {expected}, found {found}"
            ),
        }
    }
}

impl std::error::Error for FidelityError {}

/// Prove that `formatted` reprints `source` without changing its program,
/// its token order or the placement of any comment.
///
/// # Errors
///
/// Returns the first [`FidelityError`] found.
pub fn check(source: &str, formatted: &str) -> Result<(), FidelityError> {
    let original = crate::parse(source);
    let reparsed = crate::parse(formatted);
    if let Some(e) = reparsed.errors.first() {
        return Err(FidelityError::Reparse(format!("{e:?}")));
    }
    if !program_eq_ignoring_spans(&original.program, &reparsed.program) {
        return Err(FidelityError::AstChanged);
    }

    let expected = trace(source);
    let found = trace(formatted);
    let mismatch = expected
        .iter()
        .zip(&found)
        .position(|(e, f)| e.0 != f.0)
        .or_else(|| (expected.len() != found.len()).then(|| expected.len().min(found.len())));
    let Some(index) = mismatch else {
        return Ok(());
    };
    let describe = |items: &[(TraceItem, usize)]| {
        let mut out = String::new();
        for (item, _) in items.iter().skip(index).take(3) {
            if !out.is_empty() {
                out.push(' ');
            }
            let _ = write!(out, "{item}");
        }
        if out.is_empty() {
            out.push_str("end of file");
        }
        out
    };
    Err(FidelityError::TraceChanged {
        formatted_offset: found.get(index).map_or(formatted.len(), |(_, at)| *at),
        expected: describe(&expected),
        found: describe(&found),
    })
}

/// Whether a token takes part in the trace: words, literals and braces.
/// Other punctuation is the formatter's to choose, and the AST comparison
/// already proves it kept the program's meaning. Braces stay so a comment
/// cannot silently cross a block boundary.
fn is_traced(text: &str) -> bool {
    text.starts_with(|c: char| c.is_alphanumeric() || matches!(c, '_' | '"' | '\'' | '{' | '}'))
}

/// A token or comment of the source, in position order.
struct Lexeme<'s> {
    token: Option<Token<'s>>,
    item: Option<TraceItem>,
    at: usize,
}

/// Tokens and comments of `source` in order, each with its byte offset.
fn trace(source: &str) -> Vec<(TraceItem, usize)> {
    let mut lexemes: Vec<Lexeme<'_>> = hew_lexer::lex(source)
        .into_iter()
        .map(|(token, span)| {
            let text = match token {
                Token::DocComment(t) | Token::InnerDocComment(t) => Some(t.trim_end()),
                _ => Some(&source[span.start..span.end]).filter(|t| is_traced(t)),
            };
            Lexeme {
                item: text.map(|t| TraceItem::Token(t.to_string())),
                token: Some(token),
                at: span.start,
            }
        })
        .collect();
    lexemes.extend(extract_comments(source, false).into_iter().map(|c| Lexeme {
        token: None,
        item: Some(TraceItem::Comment {
            text: c.text.trim_end().to_string(),
            trailing: super::is_trailing_comment(source, c.span.start),
        }),
        at: c.span.start,
    }));
    lexemes.sort_by_key(|l| l.at);
    hoist_record_bases(&mut lexemes);
    lexemes
        .into_iter()
        .filter_map(|l| l.item.map(|item| (item, l.at)))
        .collect()
}

/// Move each record literal's `..base` to the front of its braces.
///
/// D488 makes base-first the one written spelling of a record literal, and
/// the formatter moves a trailing base there. Applying the same move to the
/// trace keeps that sanctioned reorder from reading as a moved token, while
/// every other token keeps its exact position. A pattern's `..` rest moves
/// nothing traced.
fn hoist_record_bases(lexemes: &mut [Lexeme<'_>]) {
    while let Some((open, base, end)) = misplaced_record_base(lexemes) {
        lexemes[open + 1..end].rotate_left(base - open - 1);
    }
}

/// The first `..base` that is not the first entry of its braces, as
/// (index of `{`, index of `..`, index one past the base).
fn misplaced_record_base(lexemes: &[Lexeme<'_>]) -> Option<(usize, usize, usize)> {
    let mut openers: Vec<(usize, bool)> = Vec::new();
    let mut previous: Option<&Token<'_>> = None;
    for (index, lexeme) in lexemes.iter().enumerate() {
        let Some(token) = &lexeme.token else {
            continue;
        };
        match token {
            Token::LeftBrace | Token::LeftParen | Token::LeftBracket | Token::HashBracket => {
                openers.push((index, matches!(token, Token::LeftBrace)));
            }
            Token::RightBrace | Token::RightParen | Token::RightBracket => {
                openers.pop();
            }
            Token::DotDot if matches!(previous, Some(Token::Comma)) => {
                if let Some(&(open, true)) = openers.last() {
                    let end = base_end(lexemes, index);
                    return Some((open, index, end));
                }
            }
            _ => {}
        }
        previous = Some(token);
    }
    None
}

/// Index one past the base expression that starts with the `..` at `start`:
/// the next `,` or closing delimiter at the same nesting depth.
fn base_end(lexemes: &[Lexeme<'_>], start: usize) -> usize {
    let mut depth = 0usize;
    for (index, lexeme) in lexemes.iter().enumerate().skip(start + 1) {
        match lexeme.token {
            Some(Token::LeftBrace | Token::LeftParen | Token::LeftBracket) => depth += 1,
            Some(Token::RightBrace | Token::RightParen | Token::RightBracket) if depth == 0 => {
                return index;
            }
            Some(Token::RightBrace | Token::RightParen | Token::RightBracket) => depth -= 1,
            Some(Token::Comma) if depth == 0 => return index,
            _ => {}
        }
    }
    lexemes.len()
}
