//! Source-fidelity oracle for the formatter.
//!
//! `hew fmt` may change layout and separators, and nothing else. [`check`]
//! proves that for one input by comparing:
//!
//! - the parsed programs, ignoring spans;
//! - the trace: every word, literal, bracket and comment in source order,
//!   with each comment marked as trailing (code before it on its line) or
//!   own-line, and anchored by the punctuation on either side of it;
//! - the line endings, which must all follow the source's first one.
//!
//! The AST comparison proves the program means the same thing. The trace
//! proves that declarations and members kept their order and that each
//! comment stayed where it was: a comment may gain or lose an adjacent `,` or
//! `;` that the formatter adds or drops, but it may not cross one, an
//! operator or any other token.

use std::fmt::Write as _;

use hew_lexer::Token;

use super::extract_comments;
use crate::ast_eq::program_eq_ignoring_spans;

/// One element of a source's trace.
#[derive(Debug, Clone, PartialEq, Eq)]
enum TraceItem {
    Token(String),
    Comment {
        text: String,
        trailing: bool,
        /// Untraced punctuation between the previous traced item and the comment.
        before: Vec<String>,
        /// Untraced punctuation between the comment and the next traced item.
        after: Vec<String>,
    },
}

impl TraceItem {
    /// Whether `found` reprints `self` in the same place.
    fn same_place(&self, found: &TraceItem) -> bool {
        match (self, found) {
            (TraceItem::Token(a), TraceItem::Token(b)) => a == b,
            (
                TraceItem::Comment {
                    text,
                    trailing,
                    before,
                    after,
                },
                TraceItem::Comment {
                    text: found_text,
                    trailing: found_trailing,
                    before: found_before,
                    after: found_after,
                },
            ) => {
                let is_separator = |t: &&String| matches!(t.as_str(), "," | ";");
                let operators = |side: &[String]| -> Vec<String> {
                    side.iter().filter(|t| !is_separator(t)).cloned().collect()
                };
                let has_separator = |side: &[String]| side.iter().any(|t| is_separator(&t));
                let crossed = (has_separator(before)
                    && !has_separator(found_before)
                    && has_separator(found_after))
                    || (has_separator(after)
                        && !has_separator(found_after)
                        && has_separator(found_before));
                text == found_text
                    && trailing == found_trailing
                    && operators(before) == operators(found_before)
                    && operators(after) == operators(found_after)
                    && !crossed
            }
            _ => false,
        }
    }
}

impl std::fmt::Display for TraceItem {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TraceItem::Token(t) => f.write_str(t),
            TraceItem::Comment {
                text,
                trailing,
                before,
                after,
            } => write!(
                f,
                "{} comment {text:?} after `{}` before `{}`",
                if *trailing { "trailing" } else { "own-line" },
                before.join(" "),
                after.join(" "),
            ),
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
    /// A line break in the formatted text does not use the source's style.
    LineEnding {
        /// Byte offset of the first line break in the wrong style.
        formatted_offset: usize,
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
            FidelityError::LineEnding { formatted_offset } => write!(
                f,
                "formatted output mixes line endings at byte {formatted_offset}"
            ),
        }
    }
}

impl std::error::Error for FidelityError {}

/// Whether `source` ends its lines with `\r\n`, judged by its first line.
#[must_use]
pub fn uses_crlf(source: &str) -> bool {
    source
        .find('\n')
        .is_some_and(|i| source[..i].ends_with('\r'))
}

/// Prove that `formatted` reprints `source` without changing its program,
/// its token order, the placement of any comment or its line-ending style.
///
/// # Errors
///
/// Returns the first [`FidelityError`] found.
pub fn check(source: &str, formatted: &str) -> Result<(), FidelityError> {
    let original = crate::parse(source);
    let reparsed = crate::parse(formatted);
    if let Some(e) = reparsed
        .errors
        .iter()
        .find(|e| matches!(e.severity, crate::Severity::Error))
    {
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
        .position(|(e, f)| !e.0.same_place(&f.0))
        .or_else(|| (expected.len() != found.len()).then(|| expected.len().min(found.len())));
    if let Some(index) = mismatch {
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
        return Err(FidelityError::TraceChanged {
            formatted_offset: found.get(index).map_or(formatted.len(), |(_, at)| *at),
            expected: describe(&expected),
            found: describe(&found),
        });
    }
    check_line_endings(formatted, uses_crlf(source))
}

/// Every line break between tokens of `formatted` must be `\r\n` when `crlf`
/// and `\n` otherwise. Breaks inside literals are the program's content.
fn check_line_endings(formatted: &str, crlf: bool) -> Result<(), FidelityError> {
    let mut gap_start = 0;
    let spans = hew_lexer::Lexer::new(formatted)
        .map(|(_, span)| (span.start, span.end))
        .chain(std::iter::once((formatted.len(), formatted.len())));
    for (start, end) in spans {
        let gap = &formatted.as_bytes()[gap_start..start];
        for (i, &b) in gap.iter().enumerate() {
            let preceded_by_cr = i > 0 && gap[i - 1] == b'\r';
            let wrong = match b {
                b'\n' => crlf != preceded_by_cr,
                b'\r' => !crlf || gap.get(i + 1) != Some(&b'\n'),
                _ => false,
            };
            if wrong {
                return Err(FidelityError::LineEnding {
                    formatted_offset: gap_start + i,
                });
            }
        }
        gap_start = end;
    }
    Ok(())
}

/// Whether a token takes part in the trace: words, literals and brackets.
/// Separators and operators are anchors of the comments beside them.
fn is_traced(text: &str) -> bool {
    text.starts_with(|c: char| {
        c.is_alphanumeric()
            || matches!(
                c,
                '_' | '"' | '\'' | '{' | '}' | '(' | ')' | '[' | ']' | '#'
            )
    })
}

/// A token or comment of the source, in position order.
struct Lexeme<'s> {
    token: Option<Token<'s>>,
    item: Option<TraceItem>,
    /// The token's text when it is punctuation outside the trace.
    punctuation: Option<String>,
    at: usize,
}

/// Tokens and comments of `source` in order, each with its byte offset.
fn trace(source: &str) -> Vec<(TraceItem, usize)> {
    let mut lexemes: Vec<Lexeme<'_>> = hew_lexer::Lexer::new(source)
        .map(|(token, span)| {
            let text = &source[span.start..span.end];
            let traced = match token {
                Token::DocComment(t) | Token::InnerDocComment(t) => Some(t.trim_end()),
                _ => Some(text).filter(|t| is_traced(t)),
            };
            Lexeme {
                punctuation: traced.is_none().then(|| text.to_string()),
                item: traced.map(|t| TraceItem::Token(t.to_string())),
                token: Some(token),
                at: span.start,
            }
        })
        .collect();
    lexemes.extend(extract_comments(source, false).into_iter().map(|c| Lexeme {
        token: None,
        punctuation: None,
        item: Some(TraceItem::Comment {
            text: c.text.trim_end().replace("\r\n", "\n"),
            trailing: super::is_trailing_comment(source, c.span.start),
            before: Vec::new(),
            after: Vec::new(),
        }),
        at: c.span.start,
    }));
    lexemes.sort_by_key(|l| l.at);
    hoist_record_bases(&mut lexemes);

    let mut items: Vec<(TraceItem, usize)> = Vec::new();
    let mut punctuation: Vec<String> = Vec::new();
    // Comments still collecting the punctuation that follows them.
    let mut open_comments: Vec<usize> = Vec::new();
    for lexeme in lexemes {
        if let Some(text) = lexeme.punctuation {
            for &index in &open_comments {
                if let TraceItem::Comment { after, .. } = &mut items[index].0 {
                    after.push(text.clone());
                }
            }
            punctuation.push(text);
            continue;
        }
        let Some(mut item) = lexeme.item else {
            continue;
        };
        if let TraceItem::Comment { before, .. } = &mut item {
            *before = std::mem::take(&mut punctuation);
            // A comment ends the punctuation run of the comments before it.
            open_comments.clear();
            open_comments.push(items.len());
        } else {
            punctuation.clear();
            open_comments.clear();
        }
        items.push((item, lexeme.at));
    }
    items
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
