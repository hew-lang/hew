//! Pretty-printer that converts an AST back to canonical Hew source text.

pub mod fidelity;

use std::fmt::Write as _;
use std::ops::Range;

use finl_unicode::categories::CharacterCategories;

use crate::ast::{
    sym, ActorDecl, ActorInit, Attribute, AttributeArg, BinaryOp, Block, CallArg, ChildSpec,
    CompoundAssignOp, ConditionItem, ConstDecl, ElseBlock, Expr, ExternBlock, ExternFnDecl,
    FieldDecl, FnDecl, Ident, ImplDecl, ImportDecl, ImportSpec, IntRadix, Item, LambdaParam,
    Literal, MachineDecl, MachineState, MachineTransition, MachineTransitionBodyForm, MatchArm,
    NamingCase, NominalPatternPayload, OverflowPolicy, Param, Path, Pattern, PatternField, Program,
    ReceiveFnDecl, RecordDecl, RecordKind, RestartPolicy, SelectArm, ShutdownDirective, Span,
    Spanned, Stmt, StringPart, SupervisorDecl, SupervisorStrategy, TimeoutClause, TraitBound,
    TraitDecl, TraitItem, TraitMethod, TypeAliasDecl, TypeBodyItem, TypeDecl, TypeDeclKind,
    TypeExpr, TypeParam, UnaryOp, VariantDecl, VariantKind, Visibility, WhereClause, WireMetadata,
};

/// Format a duration in nanoseconds to the most natural unit suffix.
fn format_duration_ns(ns: i64) -> String {
    if ns == 0 {
        return "0ns".to_string();
    }
    if ns % 3_600_000_000_000 == 0 {
        format!("{}h", ns / 3_600_000_000_000)
    } else if ns % 60_000_000_000 == 0 {
        format!("{}m", ns / 60_000_000_000)
    } else if ns % 1_000_000_000 == 0 {
        format!("{}s", ns / 1_000_000_000)
    } else if ns % 1_000_000 == 0 {
        format!("{}ms", ns / 1_000_000)
    } else if ns % 1_000 == 0 {
        format!("{}us", ns / 1_000)
    } else {
        format!("{ns}ns")
    }
}

/// Format an AST [`Program`] as canonical Hew source text (comments are not preserved).
#[must_use]
pub fn format_program(program: &Program) -> String {
    let mut f = Formatter::new("", Vec::new());
    f.format_program(program);
    f.output
}

/// Format one expression as canonical Hew source text: the text a failed
/// `assert` reports for its condition.
#[must_use]
pub fn format_expression(expr: &Spanned<Expr>) -> String {
    let mut f = Formatter::new("", Vec::new());
    f.format_expr(expr);
    f.output
}

/// Format an AST [`Program`] as canonical Hew source text, preserving comments from `source`.
#[must_use]
pub fn format_source(source: &str, program: &Program) -> String {
    // Doc comments travel with the other comments, so they keep their place
    // among attributes and declarations exactly as written.
    let comments = extract_comments(source, true);
    let mut f = Formatter::new(source, comments);
    f.format_program(program);
    f.flush_comments_before(usize::MAX);
    with_line_endings(&f.output, fidelity::uses_crlf(source))
}

/// `text` with every line break between its tokens written as `\r\n` when
/// `crlf`, else `\n`. Breaks inside literals are program content and stay.
fn with_line_endings(text: &str, crlf: bool) -> String {
    let mut out = String::with_capacity(text.len());
    let mut gap_start = 0;
    let spans = hew_lexer::Lexer::new(text)
        .map(|(_, span)| (span.start, span.end))
        .chain(std::iter::once((text.len(), text.len())));
    for (start, end) in spans {
        let gap = text[gap_start..start].replace("\r\n", "\n");
        if crlf {
            out.push_str(&gap.replace('\n', "\r\n"));
        } else {
            out.push_str(&gap);
        }
        out.push_str(&text[start..end]);
        gap_start = end;
    }
    out
}

/// Format `program` with the comments of `source`, refusing any output that
/// would not reprint `source` faithfully.
///
/// # Errors
///
/// Returns the [`fidelity::FidelityError`] describing how the formatted text
/// would differ from `source` beyond layout.
pub fn format_checked(source: &str, program: &Program) -> Result<String, fidelity::FidelityError> {
    let formatted = format_source(source, program);
    fidelity::check(source, &formatted)?;
    Ok(formatted)
}

/// A checker-approved replacement for a legacy bare enum variant.
///
/// The formatter owns the byte edit, while the caller supplies the semantic
/// decision.  Keeping that split prevents a token rewrite from guessing whether
/// an identifier denotes a variant or an ordinary binding.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VariantMigration {
    pub span: Range<usize>,
    pub name: String,
    pub replacement: String,
}

/// A source location the legacy-syntax migrator deliberately declined to edit.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MigrationRefusal {
    pub span: Range<usize>,
    pub reason: String,
}

/// Failure returned when the migrator cannot prove a requested source edit.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MigrationError {
    pub refusals: Vec<MigrationRefusal>,
}

impl std::fmt::Display for MigrationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "legacy syntax migration refused {} site(s)",
            self.refusals.len()
        )
    }
}

impl std::error::Error for MigrationError {}

/// Rewrite legacy path separators and checker-approved bare variants.
///
/// Every edit is anchored to lexer tokens.  Comments and string literals never
/// produce a `DoubleColon` token, and a caller-provided variant span must still
/// point at the named identifier token before it is changed.
///
/// # Errors
///
/// Returns [`MigrationError`] when a checker-selected variant no longer points
/// at its expected identifier token, or when requested edits overlap.
pub fn migrate_legacy_syntax(
    source: &str,
    variants: &[VariantMigration],
) -> Result<String, MigrationError> {
    let tokens = hew_lexer::lex(source);
    let mut edits: Vec<(Range<usize>, String)> = Vec::new();

    for (index, (token, span)) in tokens.iter().enumerate() {
        if !matches!(token, hew_lexer::Token::DoubleColon) {
            continue;
        }
        let is_turbofish = tokens
            .get(index + 1)
            .is_some_and(|(next, _)| matches!(next, hew_lexer::Token::Less));
        let replacement = if is_turbofish { "" } else { "." };
        edits.push((span.start..span.end, replacement.to_string()));
    }

    let mut refusals = Vec::new();
    for variant in variants {
        let valid_token = tokens.iter().any(|(token, span)| {
            span.start == variant.span.start
                && span.end == variant.span.end
                && matches!(token, hew_lexer::Token::Identifier(name) if *name == variant.name)
        });
        if !valid_token {
            refusals.push(MigrationRefusal {
                span: variant.span.clone(),
                reason: format!(
                    "expected identifier `{}` selected by the checker",
                    variant.name
                ),
            });
            continue;
        }
        edits.push((variant.span.clone(), variant.replacement.clone()));
    }

    edits.sort_by_key(|(span, _)| (span.start, span.end));
    for pair in edits.windows(2) {
        if pair[0].0.end > pair[1].0.start {
            refusals.push(MigrationRefusal {
                span: pair[1].0.clone(),
                reason: "migration edits overlap".to_string(),
            });
        }
    }
    if !refusals.is_empty() {
        return Err(MigrationError { refusals });
    }

    let mut migrated = source.to_string();
    for (span, replacement) in edits.into_iter().rev() {
        migrated.replace_range(span, &replacement);
    }
    Ok(migrated)
}

struct Formatter<'a> {
    output: String,
    indent: usize,
    source: &'a str,
    /// The source's tokens, from the lexer that also yields its comments.
    tokens: Vec<(hew_lexer::Token<'a>, Range<usize>)>,
    /// For each opening bracket token, the index of its closing token.
    closers: std::collections::HashMap<usize, usize>,
    /// For each closing bracket token, the index of its opening token.
    openers: std::collections::HashMap<usize, usize>,
    /// Opening parentheses already printed, or owned as syntax by the
    /// expression's parent (a call's parentheses around a sole argument).
    printed_parens: std::collections::HashSet<usize>,
    /// Depth of f-string interpolations being printed; their expressions sit
    /// inside one string token, so the formatter chooses their parentheses.
    interpolation_depth: usize,
    comments: Vec<Comment>,
    next_comment: usize,
    prev_source_pos: usize,
}

impl<'a> Formatter<'a> {
    fn new(source: &'a str, comments: Vec<Comment>) -> Self {
        let tokens: Vec<_> = hew_lexer::Lexer::new(source)
            .map(|(token, span)| (token, span.start..span.end))
            .collect();
        let mut closers = std::collections::HashMap::new();
        let mut open = Vec::new();
        for (index, (token, _)) in tokens.iter().enumerate() {
            match token {
                hew_lexer::Token::LeftParen
                | hew_lexer::Token::LeftBracket
                | hew_lexer::Token::LeftBrace
                | hew_lexer::Token::HashBracket => open.push(index),
                hew_lexer::Token::RightParen
                | hew_lexer::Token::RightBracket
                | hew_lexer::Token::RightBrace => {
                    if let Some(opener) = open.pop() {
                        closers.insert(opener, index);
                    }
                }
                _ => {}
            }
        }
        let openers = closers
            .iter()
            .map(|(&open, &close)| (close, open))
            .collect();
        Self {
            output: String::new(),
            indent: 0,
            source,
            tokens,
            closers,
            openers,
            printed_parens: std::collections::HashSet::new(),
            interpolation_depth: 0,
            comments,
            next_comment: 0,
            prev_source_pos: 0,
        }
    }

    /// Index of the first source token starting at or after `pos`.
    fn token_at_or_after(&self, pos: usize) -> usize {
        self.tokens.partition_point(|(_, span)| span.start < pos)
    }

    /// Start of the first source token at or after `from` and before `to`
    /// that satisfies `wanted`.
    fn find_token(
        &self,
        from: usize,
        to: usize,
        wanted: impl Fn(&hew_lexer::Token<'_>) -> bool,
    ) -> Option<usize> {
        self.tokens[self.token_at_or_after(from)..]
            .iter()
            .take_while(|(_, span)| span.start < to)
            .find(|(token, _)| wanted(token))
            .map(|(_, span)| span.start)
    }

    /// The grouping parentheses the source wraps around the expression at
    /// `span`, outermost first, as (open token, open, close) positions. The
    /// AST drops them, so the formatter reads them back from the tokens. A
    /// span may stop short of the `)` that closes a parenthesized last
    /// operand, so a layer's `)` may follow further `)` opened inside it.
    fn source_parens(&self, span: &Span) -> Vec<(usize, usize, usize)> {
        let mut layers = Vec::new();
        if self.source.is_empty() || span.start >= span.end || self.interpolation_depth > 0 {
            return layers;
        }
        let first = self.token_at_or_after(span.start);
        let end = self.token_at_or_after(span.end);
        if first == 0 || first >= end {
            return layers;
        }
        // The expression's last token; a span may reach just past a `)` it
        // does not own, or stop short of one it does.
        let mut last = end - 1;
        let mut open = first - 1;
        loop {
            if !matches!(self.tokens[open].0, hew_lexer::Token::LeftParen)
                || self.printed_parens.contains(&open)
                || self.is_call_paren(open)
            {
                break;
            }
            let Some(&close) = self.closers.get(&open) else {
                break;
            };
            if close < last {
                // A span can start inside its first operand's parentheses:
                // this group belongs to that operand, so look further out.
                if open == 0 {
                    break;
                }
                open -= 1;
                continue;
            }
            // Between the expression's last token and this `)`, only the
            // closers of parentheses opened inside the expression.
            let encloses = close >= last
                && (last + 1..close).all(|i| {
                    matches!(self.tokens[i].0, hew_lexer::Token::RightParen)
                        && self.openers.get(&i).is_some_and(|&o| o > open)
                });
            if !encloses {
                break;
            }
            layers.push((open, self.tokens[open].1.start, self.tokens[close].1.start));
            last = close;
            if open == 0 {
                break;
            }
            open -= 1;
        }
        layers.reverse();
        layers
    }

    /// Whether the source already parenthesizes the expression at `span`,
    /// so the formatter must not add its own layer.
    fn source_parenthesizes(&self, span: &Span) -> bool {
        !self.source_parens(span).is_empty()
    }

    /// Whether the formatter chooses parentheses for the expression at
    /// `span` from precedence. An expression read from source keeps exactly
    /// the parentheses written around it, since they parsed to this very
    /// tree; only a synthesized expression needs the formatter to decide.
    fn chooses_parens(&self, span: &Span) -> bool {
        self.source.is_empty() || span.start >= span.end || self.interpolation_depth > 0
    }

    /// Mark the argument parentheses of the call at `span`, its last `)`,
    /// as the call's own syntax rather than grouping.
    fn own_call_parens(&mut self, span: &Span) {
        let end = self.token_at_or_after(span.end);
        if end == 0 || !matches!(self.tokens[end - 1].0, hew_lexer::Token::RightParen) {
            return;
        }
        if let Some(&open) = self.openers.get(&(end - 1)) {
            self.printed_parens.insert(open);
        }
    }

    /// Whether the `(` token at `open` opens a call's argument list: it
    /// directly follows a name or a closing bracket, where a grouping
    /// parenthesis cannot stand.
    fn is_call_paren(&self, open: usize) -> bool {
        let Some((previous, _)) = open.checked_sub(1).map(|i| &self.tokens[i]) else {
            return false;
        };
        let after_dot = open >= 2 && matches!(self.tokens[open - 2].0, hew_lexer::Token::Dot);
        crate::parser::Parser::is_ident_token(previous)
            || matches!(
                previous,
                hew_lexer::Token::RightParen
                    | hew_lexer::Token::RightBracket
                    | hew_lexer::Token::Question
            )
            || (after_dot
                && self.source[self.tokens[open - 1].1.clone()].starts_with(char::is_alphabetic))
    }

    /// Whether the last source token inside `span` is `)`.
    fn span_ends_with_paren(&self, span: &Span) -> bool {
        let after = self.token_at_or_after(span.end);
        after > 0
            && self.tokens[after - 1].1.start >= span.start
            && matches!(self.tokens[after - 1].0, hew_lexer::Token::RightParen)
    }

    /// The first `{` at or after `from`, before `to`.
    fn find_open_brace(&self, from: usize, to: usize) -> Option<usize> {
        self.find_token(from, to, |t| matches!(t, hew_lexer::Token::LeftBrace))
    }

    /// The first `}` at or after `from`, before `before`; `before` when none.
    fn find_block_close(&self, from: usize, before: usize) -> usize {
        let end = before.min(self.source.len());
        self.find_token(from, end, |t| matches!(t, hew_lexer::Token::RightBrace))
            .unwrap_or(end)
    }

    fn has_comments(&self) -> bool {
        !self.comments.is_empty()
    }

    // ------------------------------------------------------------------
    // Helpers
    // ------------------------------------------------------------------

    /// Append `s`. Each token of `s` that is the formatter's copy of the
    /// next source token first emits the comments written before that
    /// token, so a comment keeps its place between the same two tokens.
    fn write_ident(&mut self, ident: Ident) {
        self.write(ident.name.as_str());
    }

    fn write(&mut self, s: &str) {
        if self.source.is_empty() || !s.contains(|c: char| !c.is_whitespace()) {
            self.output.push_str(s);
            return;
        }
        let mut written = 0;
        for (_, span) in hew_lexer::Lexer::new(s) {
            let index = self.token_at_or_after(self.prev_source_pos);
            let Some(source_span) = self.tokens.get(index).map(|(_, span)| span.clone()) else {
                break;
            };
            if self.source[source_span.clone()] != s[span.start..span.end] {
                continue;
            }
            let pending = self
                .comments
                .get(self.next_comment)
                .is_some_and(|c| c.span.start < source_span.start);
            if pending {
                self.output.push_str(&s[written..span.start]);
                self.flush_inline_comments(source_span.start);
                written = if self.output.ends_with(char::is_whitespace) {
                    span.start
                } else {
                    // Keep the space the fragment put before this token.
                    s[written..span.start].trim_end().len() + written
                };
                if self.output.ends_with(char::is_whitespace) {
                    written = span.start;
                }
            }
            self.prev_source_pos = source_span.end;
        }
        self.output.push_str(&s[written..]);
    }

    fn writeln(&mut self, s: &str) {
        self.write_indent();
        self.output.push_str(s);
        self.output.push('\n');
    }

    fn newline(&mut self) {
        self.output.push('\n');
    }

    fn write_indent(&mut self) {
        for _ in 0..self.indent {
            self.output.push_str("    ");
        }
    }

    /// Write a comma-separated list using a per-item formatting closure.
    fn comma_sep<T>(&mut self, items: &[T], mut fmt_item: impl FnMut(&mut Self, &T)) {
        for (i, item) in items.iter().enumerate() {
            if i > 0 {
                self.write_token(", ", |t| matches!(t, hew_lexer::Token::Comma));
            }
            fmt_item(self, item);
        }
    }

    /// Write `open`, the items separated by commas, then `close`. When the
    /// source list between the bracket tokens `bounds` holds comments, the
    /// list breaks one item per line so every comment stays beside the item
    /// it annotates; `angles` counts `<…>` as nesting (type lists). The
    /// broken layout ends the last item with a comma when `last_comma`.
    #[expect(
        clippy::too_many_arguments,
        reason = "one list printer for every bracket kind"
    )]
    fn delimited_list<T>(
        &mut self,
        open: &str,
        close: &str,
        items: &[T],
        bounds: Option<(usize, usize)>,
        angles: bool,
        last_comma: bool,
        mut fmt_item: impl FnMut(&mut Self, &T),
    ) {
        let layout = bounds.and_then(|(open_token, close_token)| {
            let commented = self.list_has_line_comment(open_token, close_token);
            let starts = self.list_item_starts(open_token, close_token, angles);
            (commented && starts.len() == items.len()).then_some((starts, open_token, close_token))
        });
        let Some((starts, open_token, close_token)) = layout else {
            self.write(open);
            self.comma_sep(items, fmt_item);
            if let Some((_, close_token)) = bounds {
                // The cursor stops at the closer; writing it moves past.
                let close_pos = self.tokens[close_token].1.start;
                self.flush_inline_comments(close_pos);
                self.prev_source_pos = self.prev_source_pos.max(close_pos);
            }
            self.write(close);
            return;
        };
        self.write(open.trim_end());
        self.newline();
        self.indent += 1;
        self.prev_source_pos = self.tokens[open_token].1.end;
        let last = items.len().saturating_sub(1);
        for (i, (item, start)) in items.iter().zip(starts).enumerate() {
            self.begin_member(start, false);
            self.write_indent();
            fmt_item(self, item);
            if i < last || last_comma {
                self.write(",");
            }
            self.newline();
        }
        let close_pos = self.tokens[close_token].1.start;
        self.flush_comments_before(close_pos);
        self.indent -= 1;
        self.write_indent();
        self.prev_source_pos = self.prev_source_pos.max(close_pos);
        // Every item already ends with its comma.
        self.write(close.trim_start().trim_start_matches(','));
    }

    /// Whether a line comment (or a block comment spanning lines) sits
    /// directly in the list between the bracket tokens `open` and `close`,
    /// outside any nested bracket group, which lays out its own comments.
    /// A block comment on one line stays inline beside its item.
    fn list_has_line_comment(&self, open: usize, close: usize) -> bool {
        let breaks_line = |c: &Comment| c.text.starts_with("//") || c.text.contains('\n');
        let mut index = open;
        while index < close {
            // `index` is a token at the list's own level; the next one
            // follows its nested group, when it opens one.
            let after = self
                .closers
                .get(&index)
                .filter(|_| index != open)
                .copied()
                .unwrap_or(index);
            let gap = self.tokens[after].1.end..self.tokens[after + 1].1.start;
            let commented = self.comments[self.next_comment..]
                .iter()
                .take_while(|c| c.span.start < gap.end)
                .any(|c| gap.contains(&c.span.start) && breaks_line(c));
            if commented {
                return true;
            }
            index = after + 1;
        }
        false
    }

    /// Start of each comma-separated item between the bracket tokens `open`
    /// and `close`, skipping nested brackets, lambda parameter lists and,
    /// with `angles`, type argument lists. A trailing comma adds no item.
    fn list_item_starts(&self, open: usize, close: usize, angles: bool) -> Vec<usize> {
        let mut starts = Vec::new();
        let mut index = open + 1;
        let mut at_item_start = true;
        let mut angle_depth = 0usize;
        while index < close {
            let token = &self.tokens[index].0;
            if at_item_start {
                starts.push(self.tokens[index].1.start);
                at_item_start = false;
                // A lambda's `|a, b|` parameters hold commas of their own.
                let mut lambda = index;
                if matches!(token, hew_lexer::Token::Move) {
                    lambda += 1;
                }
                if lambda < close && matches!(self.tokens[lambda].0, hew_lexer::Token::Pipe) {
                    index = lambda + 1;
                    while index < close && !matches!(self.tokens[index].0, hew_lexer::Token::Pipe) {
                        index += 1;
                    }
                    index += 1;
                    continue;
                }
            }
            match token {
                hew_lexer::Token::Comma if angle_depth == 0 => at_item_start = true,
                hew_lexer::Token::Less if angles => angle_depth += 1,
                hew_lexer::Token::Greater if angles && angle_depth > 0 => angle_depth -= 1,
                hew_lexer::Token::GreaterGreater if angles => {
                    angle_depth = angle_depth.saturating_sub(2);
                }
                _ => {
                    if let Some(&closer) = self.closers.get(&index) {
                        index = closer;
                    }
                }
            }
            index += 1;
        }
        starts
    }

    /// The bracket tokens of the list that ends the expression at `span`
    /// (a call's `(…)`, an array's `[…]`, a record literal's `{…}`).
    fn trailing_list(&self, span: &Span, closer: &str) -> Option<(usize, usize)> {
        let first = self.token_at_or_after(span.start);
        let end = self.token_at_or_after(span.end);
        // A span may stop just before its closer or run just past it.
        [end.checked_sub(1), Some(end)]
            .into_iter()
            .flatten()
            .filter(|&close| {
                close < self.tokens.len() && self.source[self.tokens[close].1.clone()] == *closer
            })
            .find_map(|close| {
                self.openers
                    .get(&close)
                    .filter(|&&open| open >= first)
                    .map(|&open| (open, close))
            })
    }

    /// The bracket tokens of the first `(…)` at or after `from`, outside any
    /// `<…>` generic parameter list: a declaration's parameter list.
    fn params_list(&self, from: usize) -> Option<(usize, usize)> {
        if self.source.is_empty() {
            return None;
        }
        let mut angle_depth = 0usize;
        for index in self.token_at_or_after(from)..self.tokens.len() {
            match self.tokens[index].0 {
                hew_lexer::Token::Less => angle_depth += 1,
                hew_lexer::Token::Greater => angle_depth = angle_depth.saturating_sub(1),
                hew_lexer::Token::GreaterGreater => angle_depth = angle_depth.saturating_sub(2),
                hew_lexer::Token::LeftParen if angle_depth == 0 => {
                    return self.closers.get(&index).map(|&close| (index, close));
                }
                hew_lexer::Token::LeftBrace | hew_lexer::Token::Semicolon => return None,
                _ => {}
            }
        }
        None
    }

    /// `{ ..base, name: value, ... }` — the one record-literal body spelling.
    /// The base comes first: it supplies every field the literal does not name,
    /// so reading it first reads the value in the order it is built.
    fn format_record_literal_body(
        &mut self,
        fields: &[(Ident, Spanned<Expr>)],
        base: Option<&Spanned<Expr>>,
        bounds: Option<(usize, usize)>,
    ) {
        let write_field = |f: &mut Self, (name, value): &(Ident, Spanned<Expr>)| {
            f.write_ident(*name);
            f.write(": ");
            f.format_expr(value);
        };
        let Some(base) = base else {
            self.delimited_list(" { ", " }", fields, bounds, false, true, write_field);
            return;
        };
        self.write(" { ..");
        self.format_expr(base);
        // Comments written after a trailing base belong with it.
        self.flush_comments_before_token_after(base.1.end);
        if !fields.is_empty() {
            self.trim_trailing_spaces();
            self.write(", ");
        }
        self.comma_sep(fields, write_field);
        self.write(" }");
    }

    fn format_path(&mut self, path: &Path) {
        self.write(&path.to_string());
    }

    fn write_visibility(&mut self, vis: Visibility) {
        match vis {
            Visibility::Private => {}
            Visibility::Pub => self.write("pub "),
            Visibility::Package => self.write("package "),
        }
    }

    /// Emit an outer (`///`) or inner (`//!`) doc-comment block, one line per
    /// `\n` in the stored content. The parser strips the prefix and one
    /// leading space; we add them back. Empty lines emit the prefix alone,
    /// matching parser input.
    fn write_doc_comment(&mut self, doc: &str, prefix: &str) {
        for line in doc.split('\n') {
            self.write_indent();
            if line.is_empty() {
                self.write(prefix);
                self.write("\n");
            } else {
                self.write(prefix);
                self.write(" ");
                self.write(line);
                self.write("\n");
            }
        }
    }

    /// Emit a declaration's doc comment from the AST. With source, doc
    /// comments are flushed in place like any other comment instead.
    fn write_outer_doc(&mut self, doc: Option<&String>) {
        if !self.source.is_empty() {
            return;
        }
        if let Some(d) = doc {
            self.write_doc_comment(d, "///");
        }
    }

    // ------------------------------------------------------------------
    // Comment flushing
    // ------------------------------------------------------------------

    fn flush_comments_before(&mut self, pos: usize) {
        while self.next_comment < self.comments.len()
            && self.comments[self.next_comment].span.start < pos
        {
            self.flush_one_comment();
        }
    }

    fn flush_block_end_comments(&mut self, scope_end: usize) {
        if self.has_comments() {
            let brace = self.find_block_close(self.prev_source_pos, scope_end);
            // Comments in `[prev_source_pos, brace)` whose source column is at
            // or before the closing `}`'s column are typed at outer indent and
            // logically document the next branch in an `if/else if/else` chain
            // (or a similar trailing construct), e.g.
            //     if cond {
            //         body
            //     // documents next branch
            //     } else if other {
            // Emit those at the outer indent so they round-trip stably; emit
            // anything at deeper column with the inner indent (the default).
            let brace_col = source_column(self.source, brace);
            while self.next_comment < self.comments.len()
                && self.comments[self.next_comment].span.start < brace
            {
                let c_start = self.comments[self.next_comment].span.start;
                let outer = !is_trailing_comment(self.source, c_start)
                    && source_column(self.source, c_start) <= brace_col
                    && self.indent > 0;
                if outer {
                    self.indent -= 1;
                    self.flush_one_comment();
                    self.indent += 1;
                } else {
                    self.flush_one_comment();
                }
            }
            // The cursor stops at the `}` itself; writing it moves past.
            if brace < self.source.len() {
                self.prev_source_pos = brace;
            }
        }
    }

    /// Emit exactly one pending comment (the next one) using the comment-flush
    /// rules that `flush_comments_before` applies. Caller is responsible for
    /// any indent adjustment around the call.
    fn flush_one_comment(&mut self) {
        let idx = self.next_comment;
        if is_trailing_comment(self.source, self.comments[idx].span.start) {
            if self.output.ends_with('\n') {
                self.output.pop();
            }
            self.write(" ");
        } else {
            let gap_start = self.prev_source_pos.min(self.source.len());
            let gap_end = self.comments[idx].span.start.min(self.source.len());
            if gap_start < gap_end {
                let newlines = self.source[gap_start..gap_end]
                    .chars()
                    .filter(|&c| c == '\n')
                    .count();
                if self.prev_source_pos > 0 && newlines > 1 && !self.output.ends_with("\n\n") {
                    self.newline();
                }
            }
            self.write_indent();
        }

        let text = self.comments[idx].text.clone();
        self.write(&text);
        self.newline();
        self.prev_source_pos = self.comments[idx].span.end;
        self.next_comment += 1;
    }

    fn flush_comments_and_separate(&mut self, pos: usize, needs_blank_line: bool) {
        let had_comments = self.next_comment;
        self.flush_comments_before(pos);
        let flushed_comments = self.next_comment > had_comments;
        if needs_blank_line && !flushed_comments && !self.output.ends_with("\n\n") {
            self.newline();
        }
        if flushed_comments {
            self.keep_blank_line_before(pos);
        }
    }

    /// After flushing comments, keep a blank line the author left between
    /// the last of them and the declaration at `pos`.
    fn keep_blank_line_before(&mut self, pos: usize) {
        let gap = self.source.get(self.prev_source_pos..pos).unwrap_or("");
        if gap.matches('\n').count() > 1 && !self.output.ends_with("\n\n") {
            self.newline();
        }
    }

    /// Emit the comments before `pos` from inside an expression. The
    /// expression continues after them on a continuation line, so a comment
    /// keeps its place between the same two tokens: a trailing comment stays
    /// on the line it trails and an own-line comment gets its own line.
    fn flush_inline_comments(&mut self, pos: usize) {
        if self
            .comments
            .get(self.next_comment)
            .is_none_or(|c| c.span.start >= pos)
        {
            return;
        }
        while let Some(comment) = self.comments.get(self.next_comment) {
            if comment.span.start >= pos {
                break;
            }
            let text = comment.text.clone();
            let trailing = is_trailing_comment(self.source, comment.span.start);
            let line_comment = text.starts_with("//") || text.contains('\n');
            self.prev_source_pos = comment.span.end;
            self.next_comment += 1;
            while self.output.ends_with(' ') {
                self.output.pop();
            }
            if trailing && !self.output.ends_with('\n') {
                self.write(" ");
            } else {
                if !self.output.ends_with('\n') {
                    self.newline();
                }
                self.indent += 1;
                self.write_indent();
                self.indent -= 1;
            }
            self.write(&text);
            if line_comment {
                self.newline();
            } else {
                self.write(" ");
            }
        }
        if self.output.ends_with('\n') {
            self.indent += 1;
            self.write_indent();
            self.indent -= 1;
        }
    }

    /// Emit, inside an expression, the comments before the first token at
    /// or after `pos`: those between a receiver and its `.member`.
    fn flush_comments_before_token_after(&mut self, pos: usize) {
        if let Some(rest) = self.source.get(pos..) {
            if let Some((_, span)) = hew_lexer::Lexer::new(rest).next() {
                self.flush_inline_comments(pos + span.start);
            }
        }
    }

    /// When the next source token after the cursor satisfies `wanted`, emit
    /// the comments before it inside the current expression and move the
    /// cursor past it, so the formatter's copy of that token keeps its place.
    fn flush_before_next_token(&mut self, wanted: impl Fn(&hew_lexer::Token<'_>) -> bool) {
        let index = self.token_at_or_after(self.prev_source_pos);
        if let Some((token, span)) = self.tokens.get(index) {
            if wanted(token) {
                let start = span.start;
                self.flush_inline_comments(start);
                // The cursor stays at the token; writing it moves past.
                self.prev_source_pos = start;
            }
        }
    }

    /// Drop spaces at the end of the output, unless they are the indent of
    /// an otherwise empty line.
    fn trim_trailing_spaces(&mut self) {
        let line_start = self.output.rfind('\n').map_or(0, |i| i + 1);
        if !self.output[line_start..].trim().is_empty() {
            let kept = self.output.trim_end_matches(' ').len();
            self.output.truncate(kept);
        }
    }

    /// Write ` else ` for the next `else`. A comment the author put between
    /// the closing `}` and `else` keeps its line, so `else` starts its own.
    fn write_else(&mut self) {
        let index = self.token_at_or_after(self.prev_source_pos);
        let Some((hew_lexer::Token::Else, span)) = self.tokens.get(index) else {
            self.write(" else ");
            return;
        };
        let (start, end) = (span.start, span.end);
        if self
            .comments
            .get(self.next_comment)
            .is_some_and(|c| c.span.start < start)
        {
            self.newline();
            self.flush_comments_before(start);
            self.write_indent();
            self.write("else ");
        } else {
            self.write(" else ");
        }
        self.prev_source_pos = end;
    }

    /// Write `text`, the formatter's copy of the next source token when that
    /// token satisfies `wanted`, after the comments that precede the token.
    fn write_token(&mut self, text: &str, wanted: impl Fn(&hew_lexer::Token<'_>) -> bool) {
        self.flush_before_next_token(wanted);
        // After a flushed comment the output already ends in a space.
        let text = if self.output.ends_with(' ') {
            text.trim_start()
        } else {
            text
        };
        if text.starts_with([';', ',']) {
            self.trim_trailing_spaces();
        }
        self.write(text);
    }

    /// Start a member of a declaration body whose first source token is at
    /// `start`, emitting the comments before it. With source, the member is
    /// preceded by a blank line exactly when the author left one before it
    /// (or before its leading comments); a synthesized program uses
    /// `canonical_blank_line` instead.
    fn begin_member(&mut self, start: usize, canonical_blank_line: bool) {
        if self.source.is_empty() {
            if canonical_blank_line && !self.output.ends_with("\n\n") {
                self.newline();
            }
            return;
        }
        // A trailing comment stays on the line of the member before.
        while self
            .comments
            .get(self.next_comment)
            .is_some_and(|c| c.span.start < start && is_trailing_comment(self.source, c.span.start))
        {
            self.flush_one_comment();
        }
        let lead = self
            .comments
            .get(self.next_comment)
            .map_or(start, |c| c.span.start.min(start));
        if blank_line_before(self.source, lead)
            && !self.output.ends_with("\n\n")
            && !self.output.ends_with("{\n")
        {
            self.newline();
        }
        if lead < start {
            self.flush_comments_before(start);
            self.keep_blank_line_before(start);
        }
        self.prev_source_pos = self.prev_source_pos.max(start);
    }

    /// Finish a member that ends at `end` without a block of its own, so a
    /// comment gap after it is measured from its last token.
    fn end_member(&mut self, end: usize) {
        self.prev_source_pos = self.prev_source_pos.max(end);
    }

    /// The source spelling of the literal at `span`: exactly one literal
    /// token, optionally negated. `None` when there is no source or the span
    /// does not cover a literal token (a node the parser synthesized).
    fn literal_spelling(&self, span: &Span) -> Option<String> {
        let text = self.source.get(span.clone())?;
        let tokens = hew_lexer::lex(text);
        let (negated, literal) = match tokens.as_slice() {
            [(literal, _)] => (false, literal),
            [(hew_lexer::Token::Minus, _), (literal, _)] => (true, literal),
            _ => return None,
        };
        let spelling = match literal {
            hew_lexer::Token::Integer(t)
            | hew_lexer::Token::Float(t)
            | hew_lexer::Token::Duration(t)
            | hew_lexer::Token::StringLit(t)
            | hew_lexer::Token::RawString(t)
            | hew_lexer::Token::ByteStringLit(t)
            | hew_lexer::Token::RegexLiteral(t)
            | hew_lexer::Token::InterpolatedString(t)
            | hew_lexer::Token::CharLit(t) => *t,
            hew_lexer::Token::True => "true",
            hew_lexer::Token::False => "false",
            _ => return None,
        };
        Some(if negated {
            format!("-{spelling}")
        } else {
            spelling.to_string()
        })
    }

    /// Reprint the attributes that open the item starting at `item_start`,
    /// in source order and spelling. Declarations that fold their attributes
    /// into flags use this so `#[resource] #[opaque]` and `#[max_heap(64 kb)]`
    /// survive as written. Returns `false` when there is no source to read.
    fn format_item_attributes(&mut self, item_start: usize) -> bool {
        let Some(rest) = self
            .source
            .get(item_start..)
            .filter(|_| !self.source.is_empty())
        else {
            return false;
        };
        let mut tokens = hew_lexer::Lexer::new(rest).peekable();
        loop {
            match tokens.peek() {
                // A doc comment prints in place with the other comments.
                Some((hew_lexer::Token::DocComment(_), _)) => {
                    tokens.next();
                }
                Some((hew_lexer::Token::HashBracket, span)) => {
                    let start = item_start + span.start;
                    let mut depth = 0usize;
                    let mut end = start;
                    for (token, span) in tokens.by_ref() {
                        match token {
                            hew_lexer::Token::HashBracket | hew_lexer::Token::LeftBracket => {
                                depth += 1;
                            }
                            hew_lexer::Token::RightBracket => depth -= 1,
                            _ => {}
                        }
                        end = item_start + span.end;
                        if depth == 0 {
                            break;
                        }
                    }
                    self.flush_comments_before(start);
                    let text = self.attribute_text(&(start..end));
                    self.write_indent();
                    self.write(&text);
                    self.newline();
                }
                Some((_, span)) => {
                    // Doc comments between the attributes and the keyword.
                    self.flush_comments_before(item_start + span.start);
                    return true;
                }
                None => return true,
            }
        }
    }

    /// The attribute at `span` with canonical spacing and its source
    /// spelling (`#[timeout(1000ms)]` stays in milliseconds).
    fn attribute_text(&self, span: &Span) -> String {
        let mut out = String::new();
        let mut previous: Option<&str> = None;
        for (_, token) in hew_lexer::lex(&self.source[span.clone()]) {
            let text = &self.source[span.start + token.start..span.start + token.end];
            if let Some(previous) = previous {
                let tight = matches!(previous, "#[" | "(" | ".")
                    || matches!(text, ")" | "]" | "," | "(" | ".");
                if previous == "," || !tight {
                    out.push(' ');
                }
            }
            out.push_str(text);
            previous = Some(text);
        }
        out
    }

    /// Whether the expression at `span` is a `break` or `continue` written
    /// without braces in expression position.
    fn is_bare_control_flow(&self, span: &Span) -> bool {
        self.source.get(span.clone()).is_some_and(|text| {
            matches!(
                hew_lexer::Lexer::new(text).next(),
                Some((hew_lexer::Token::Break | hew_lexer::Token::Continue, _))
            )
        })
    }

    /// The source spelling of each element of the `bytes [..]` literal at
    /// `span`, when the source holds exactly `len` integer elements.
    fn byte_array_spellings(&self, span: &Span, len: usize) -> Option<Vec<String>> {
        let text = self.source.get(span.clone())?;
        let elements: Vec<String> = hew_lexer::lex(text)
            .into_iter()
            .filter_map(|(token, _)| match token {
                hew_lexer::Token::Integer(t) => Some(t.to_string()),
                _ => None,
            })
            .collect();
        (elements.len() == len).then_some(elements)
    }

    // ------------------------------------------------------------------
    // Program
    // ------------------------------------------------------------------

    fn format_program(&mut self, program: &Program) {
        if let Some(doc) = program
            .module_doc
            .as_ref()
            .filter(|_| self.source.is_empty())
        {
            for line in doc.split('\n') {
                if line.is_empty() {
                    self.write("//!\n");
                } else {
                    self.write("//! ");
                    self.write(line);
                    self.write("\n");
                }
            }
            if !program.items.is_empty() {
                self.write("\n");
            }
        }
        for (i, item) in program.items.iter().enumerate() {
            self.flush_comments_and_separate(item.1.start, i > 0);
            self.prev_source_pos = item.1.start;
            self.format_item(&item.0, item.1.start, item.1.end);
            // Only advance if format_item didn't already advance past the item
            // (block-containing items advance via flush_block_end_comments).
            if self.prev_source_pos < item.1.start {
                self.prev_source_pos = item.1.end;
            }
        }
    }

    // ------------------------------------------------------------------
    // Items
    // ------------------------------------------------------------------

    fn format_item(&mut self, item: &Item, span_start: usize, span_end: usize) {
        match item {
            Item::Import(decl) => self.format_import(decl, span_start..span_end),
            Item::Const(decl) => self.format_const(decl),
            Item::TypeDecl(decl) => self.format_type_decl(decl, span_start, span_end),
            Item::TypeAlias(decl) => self.format_type_alias(decl),
            Item::Trait(decl) => self.format_trait(decl, span_start, span_end),
            Item::Impl(decl) => self.format_impl(decl, span_end),
            Item::Function(decl) => self.format_fn(decl, span_end),
            Item::ExternBlock(decl) => self.format_extern_block(decl, span_end),
            Item::Actor(decl) => self.format_actor(decl, span_start, span_end),
            Item::Supervisor(decl) => self.format_supervisor(decl, span_start, span_end),
            Item::Machine(decl) => self.format_machine(decl, span_start, span_end),
            Item::Record(decl) => self.format_record(decl),
        }
    }

    fn format_import(&mut self, decl: &ImportDecl, span: Span) {
        self.write_indent();
        self.write("import ");
        if let Some(file_path) = &decl.file_path {
            self.write("\"");
            self.write(file_path);
            self.write("\"");
        } else {
            self.format_path(&decl.path);
            if let Some(spec) = &decl.spec {
                self.write(".");
                match spec {
                    ImportSpec::Names(names) => {
                        let bounds = self
                            .find_open_brace(span.start, span.end)
                            .map(|open| self.token_at_or_after(open))
                            .and_then(|open| self.closers.get(&open).map(|&close| (open, close)));
                        let close = if decl.selection_trailing_comma {
                            ",}"
                        } else {
                            "}"
                        };
                        self.delimited_list(
                            "{",
                            close,
                            names,
                            bounds,
                            false,
                            decl.selection_trailing_comma,
                            |f, n| {
                                f.write_ident(n.name);
                                if let Some(alias) = &n.alias {
                                    f.write(" as ");
                                    f.write_ident(*alias);
                                }
                            },
                        );
                    }
                }
            }
            // Whole-module alias (`import path as alias;`). The parser only
            // accepts this for the whole-module form, so it never coexists
            // with a `::{ }` / `::*` spec.
            if let Some(alias) = &decl.module_alias {
                self.write(" as ");
                self.write_ident(*alias);
            }
        }
        self.write_token(";\n", |t| matches!(t, hew_lexer::Token::Semicolon));
    }

    fn format_const(&mut self, decl: &ConstDecl) {
        self.write_outer_doc(decl.doc_comment.as_ref());
        self.write_indent();
        self.write_visibility(decl.visibility);
        self.write("const ");
        self.write_ident(decl.name);
        self.write(": ");
        self.format_type_expr(&decl.ty.0);
        self.write(" = ");
        self.format_expr(&decl.value);
        self.write_token(";\n", |t| matches!(t, hew_lexer::Token::Semicolon));
    }

    fn format_type_alias(&mut self, decl: &TypeAliasDecl) {
        self.write_outer_doc(decl.doc_comment.as_ref());
        self.write_indent();
        self.write_visibility(decl.visibility);
        self.write("type ");
        self.write_ident(decl.name);
        self.format_opt_type_params(decl.type_params.as_ref());
        self.write(" = ");
        self.format_type_expr(&decl.ty.0);
        self.write_token(";\n", |t| matches!(t, hew_lexer::Token::Semicolon));
    }

    fn format_record(&mut self, decl: &RecordDecl) {
        self.write_outer_doc(decl.doc_comment.as_ref());
        self.write_indent();
        self.write_visibility(decl.visibility);
        self.write("type ");
        self.write_ident(decl.name);
        self.format_opt_type_params(decl.type_params.as_ref());
        self.format_opt_where_clause(decl.where_clause.as_ref());
        match &decl.kind {
            RecordKind::Named(fields) => {
                self.write(" {\n");
                self.indent += 1;
                for (i, field) in fields.iter().enumerate() {
                    self.write_outer_doc(field.doc_comment.as_ref());
                    self.write_indent();
                    self.write_ident(field.name);
                    self.write(": ");
                    self.format_type_expr(&field.ty.0);
                    if i + 1 < fields.len() {
                        self.write(",");
                    }
                    self.write("\n");
                }
                self.indent -= 1;
                self.write_indent();
                self.write("}\n");
            }
            RecordKind::Tuple(field_types) => {
                self.write("(");
                for (i, (ty, _)) in field_types.iter().enumerate() {
                    self.format_type_expr(ty);
                    if i + 1 < field_types.len() {
                        self.write(", ");
                    }
                }
                self.write(");\n");
            }
        }
    }

    fn format_type_decl(&mut self, decl: &TypeDecl, span_start: usize, span_end: usize) {
        if let Some(wire) = &decl.wire {
            self.format_wire_type_decl(decl, wire, span_start, span_end);
            return;
        }
        self.write_outer_doc(decl.doc_comment.as_ref());
        if !self.format_item_attributes(span_start) {
            self.format_type_decl_attributes(decl);
        }
        self.write_indent();
        self.write_visibility(decl.visibility);
        if decl.is_indirect {
            self.write("indirect ");
        }
        match decl.kind {
            TypeDeclKind::Struct => self.write("type "),
            TypeDeclKind::Enum => self.write("enum "),
        }
        self.write_ident(decl.name);
        self.format_opt_type_params(decl.type_params.as_ref());
        self.format_opt_where_clause(decl.where_clause.as_ref());
        self.write(" {\n");
        self.indent += 1;
        for item in &decl.body {
            match item {
                TypeBodyItem::Field {
                    name,
                    ty,
                    attributes,
                    doc_comment,
                    span,
                } => {
                    // flush inline comments that appear before this field
                    self.flush_comments_before(span.start);
                    // position at item start so the blank-line heuristic in
                    // the trailing-comment flush below counts only newlines
                    // between the field name and any trailing comment
                    self.prev_source_pos = span.start;
                    self.write_outer_doc(doc_comment.as_ref());
                    self.format_attributes(attributes);
                    self.write_indent();
                    self.write_ident(*name);
                    self.write(": ");
                    self.format_type_expr(&ty.0);
                    self.write(",");
                    self.newline();
                    // flush any trailing comment on this line; span.end is the
                    // first token of the next item (or closing brace), so any
                    // comment between content and span.end is captured here
                    self.flush_comments_before(span.end);
                    self.prev_source_pos = self.prev_source_pos.max(span.end);
                }
                TypeBodyItem::Variant(v) => {
                    // flush inline comments that appear before this variant
                    self.flush_comments_before(v.span.start);
                    // position at item start so the blank-line heuristic in
                    // the trailing-comment flush below counts only newlines
                    // between the variant name and any trailing comment
                    self.prev_source_pos = v.span.start;
                    self.format_variant(v, true);
                    // flush any trailing comment on this line; v.span.end is
                    // the first token of the next item (or closing brace), so
                    // any comment between content and v.span.end is captured
                    self.flush_comments_before(v.span.end);
                    self.prev_source_pos = self.prev_source_pos.max(v.span.end);
                }
                TypeBodyItem::Method(f) => {
                    self.begin_member(member_start(&f.attributes, f.fn_span.start), false);
                    let has_consuming_self =
                        decl.consuming_methods.iter().any(|name| name == &f.name);
                    self.format_type_body_method(f, f.fn_span.end, has_consuming_self);
                }
            }
        }
        if self.has_comments() {
            self.flush_block_end_comments(span_end);
        }
        self.indent -= 1;
        self.writeln("}");
    }

    /// The attributes a type declaration folds into flags, for a program
    /// with no source to reprint them from.
    fn format_type_decl_attributes(&mut self, decl: &TypeDecl) {
        match decl.resource_marker {
            crate::ast::ResourceMarker::None => {}
            crate::ast::ResourceMarker::Resource => {
                self.write_indent();
                self.write("#[resource]\n");
            }
            crate::ast::ResourceMarker::Linear => {
                self.write_indent();
                self.write("#[linear]\n");
            }
        }
        if decl.is_opaque {
            self.write_indent();
            self.write("#[opaque]\n");
        }
        if let Some(lang_item) = &decl.lang_item {
            self.write_indent();
            self.write("#[lang_item(\"");
            self.write(lang_item);
            self.write("\")]\n");
        }
    }

    fn format_type_body_method(
        &mut self,
        decl: &FnDecl,
        span_end: usize,
        has_consuming_self: bool,
    ) {
        self.write_outer_doc(decl.doc_comment.as_ref());
        self.format_attributes(&decl.attributes);
        self.write_indent();
        if decl.is_generator {
            self.write("gen ");
        }
        self.write("fn ");
        self.write_ident(decl.name);
        self.format_opt_type_params(decl.type_params.as_ref());
        self.write("(");
        if has_consuming_self {
            self.write("consume self");
            if !decl.params.is_empty() {
                self.write(", ");
            }
        }
        self.format_params(&decl.params);
        self.write(")");
        if let Some(ret) = &decl.return_type {
            self.write(" -> ");
            self.format_type_expr(&ret.0);
        }
        self.format_opt_where_clause(decl.where_clause.as_ref());
        self.write(" ");
        self.format_block(&decl.body, span_end);
        self.newline();
    }

    fn format_wire_type_decl(
        &mut self,
        decl: &TypeDecl,
        wire: &WireMetadata,
        span_start: usize,
        span_end: usize,
    ) {
        if !self.format_item_attributes(span_start) {
            self.format_wire_attributes(decl, wire);
        }
        self.write_indent();
        self.write_visibility(decl.visibility);
        match decl.kind {
            TypeDeclKind::Struct => self.write("type "),
            TypeDeclKind::Enum => self.write("enum "),
        }
        self.write_ident(decl.name);
        self.write(" {\n");
        self.indent += 1;
        match decl.kind {
            TypeDeclKind::Struct => {
                for (i, item) in decl.body.iter().enumerate() {
                    if let TypeBodyItem::Field { name, ty, span, .. } = item {
                        self.flush_comments_before(span.start);
                        self.prev_source_pos = span.start;
                        self.write_indent();
                        self.write_ident(*name);
                        self.write(": ");
                        self.format_type_expr(&ty.0);
                        // Emit wire field metadata
                        if let Some(meta) = wire.field_meta.get(i) {
                            self.write(" @");
                            self.write(&meta.field_number.to_string());
                            self.format_wire_field_modifiers(
                                meta.is_optional,
                                meta.is_deprecated,
                                meta.is_repeated,
                                meta.since,
                                meta.json_name.as_deref(),
                                meta.yaml_name.as_deref(),
                            );
                        }
                        self.write(",");
                        self.newline();
                        self.flush_comments_before(span.end);
                        self.prev_source_pos = self.prev_source_pos.max(span.end);
                    }
                }
                // Emit reserved field numbers
                if !wire.reserved_numbers.is_empty() {
                    self.write_indent();
                    self.write("reserved ");
                    for (i, n) in wire.reserved_numbers.iter().enumerate() {
                        if i > 0 {
                            self.write(", ");
                        }
                        self.write("@");
                        self.write(&n.to_string());
                    }
                    self.write_token(";\n", |t| matches!(t, hew_lexer::Token::Semicolon));
                }
            }
            TypeDeclKind::Enum => {
                // Variant bodies are tagged by variant index, not by per-field
                // `@N`; reserved tags do not apply.  Delegate to the regular
                // variant formatter to handle unit / tuple / struct payloads.
                for item in &decl.body {
                    if let TypeBodyItem::Variant(v) = item {
                        self.flush_comments_before(v.span.start);
                        self.prev_source_pos = v.span.start;
                        self.format_variant(v, true);
                        self.flush_comments_before(v.span.end);
                        self.prev_source_pos = self.prev_source_pos.max(v.span.end);
                    }
                }
            }
        }
        if self.has_comments() {
            self.flush_block_end_comments(span_end);
        }
        self.indent -= 1;
        self.writeln("}");
    }

    /// The attributes a wire declaration folds into its metadata, for a
    /// program with no source to reprint them from.
    fn format_wire_attributes(&mut self, decl: &TypeDecl, wire: &WireMetadata) {
        if let Some(lang_item) = &decl.lang_item {
            self.write_indent();
            self.write("#[lang_item(\"");
            self.write(lang_item);
            self.write("\")]\n");
        }
        // Emit type-level naming attributes
        self.format_naming_attr("json", wire.json_case);
        self.format_naming_attr("yaml", wire.yaml_case);
        self.write_indent();
        if wire.version.is_some() || wire.min_version.is_some() {
            self.write("#[wire(");
            let mut first = true;
            if let Some(v) = wire.version {
                write!(self.output, "version = {v}").unwrap();
                first = false;
            }
            if let Some(v) = wire.min_version {
                if !first {
                    self.write(", ");
                }
                write!(self.output, "min_version = {v}").unwrap();
            }
            self.write(")]\n");
        } else {
            self.write("#[wire]\n");
        }
    }

    fn format_naming_attr(&mut self, attr_name: &str, case: Option<NamingCase>) {
        if let Some(case) = case {
            self.write_indent();
            let s = case.as_str();
            let needs_quotes = s.contains('-');
            self.write("#[");
            self.write(attr_name);
            self.write("(");
            if needs_quotes {
                self.write("\"");
            }
            self.write(s);
            if needs_quotes {
                self.write("\"");
            }
            self.write(")]\n");
        }
    }

    fn format_wire_field_modifiers(
        &mut self,
        is_optional: bool,
        is_deprecated: bool,
        is_repeated: bool,
        since: Option<u32>,
        json_name: Option<&str>,
        yaml_name: Option<&str>,
    ) {
        if is_optional {
            self.write(" optional");
        }
        if is_deprecated {
            self.write(" deprecated");
        }
        if is_repeated {
            self.write(" repeated");
        }
        if let Some(version) = since {
            self.write(" since ");
            self.write(&version.to_string());
        }
        if let Some(name) = json_name {
            self.write(" json(\"");
            self.write(name);
            self.write("\")");
        }
        if let Some(name) = yaml_name {
            self.write(" yaml(\"");
            self.write(name);
            self.write("\")");
        }
    }

    fn format_variant(&mut self, v: &VariantDecl, trailing_comma: bool) {
        self.write_outer_doc(v.doc_comment.as_ref());
        self.write_indent();
        self.write_ident(v.name);
        match &v.kind {
            VariantKind::Unit => {}
            VariantKind::Tuple(fields) => {
                if !fields.is_empty() {
                    self.write("(");
                    self.comma_sep(fields, |f, ty| f.format_type_expr(&ty.0));
                    self.write(")");
                }
            }
            VariantKind::Struct(fields) => {
                self.write(" { ");
                self.comma_sep(fields, |f, (name, ty)| {
                    f.write_ident(*name);
                    f.write(": ");
                    f.format_type_expr(&ty.0);
                });
                self.write(" }");
            }
        }
        if trailing_comma {
            self.write(",");
        }
        self.newline();
    }

    fn format_trait(&mut self, decl: &TraitDecl, span_start: usize, span_end: usize) {
        self.write_outer_doc(decl.doc_comment.as_ref());
        if !self.format_item_attributes(span_start) {
            if let Some(key) = &decl.lang_item {
                self.write_indent();
                self.write(&format!("#[lang_item(\"{key}\")]\n"));
            }
        }
        self.write_indent();
        self.write_visibility(decl.visibility);
        self.write("trait ");
        self.write_ident(decl.name);
        self.format_opt_type_params(decl.type_params.as_ref());
        if let Some(supers) = &decl.super_traits {
            self.write(": ");
            self.format_trait_bound_list(supers);
        }
        self.write(" {\n");
        self.indent += 1;
        for (i, item) in decl.items.iter().enumerate() {
            match item {
                TraitItem::Method(m) => {
                    self.begin_member(member_start(&m.attributes, m.span.start), i > 0);
                    self.format_trait_method(m);
                    self.end_member(m.span.end);
                }
                TraitItem::AssociatedType {
                    name,
                    bounds,
                    default,
                    span,
                } => {
                    self.begin_member(span.start, i > 0);
                    self.write_indent();
                    self.write("type ");
                    self.write_ident(*name);
                    if !bounds.is_empty() {
                        self.write(": ");
                        self.format_trait_bound_list(bounds);
                    }
                    if let Some(def) = default {
                        self.write(" = ");
                        self.format_type_expr(&def.0);
                    }
                    self.write_token(";\n", |t| matches!(t, hew_lexer::Token::Semicolon));
                    self.end_member(span.end);
                }
            }
        }
        if self.has_comments() {
            self.flush_block_end_comments(span_end);
        }
        self.indent -= 1;
        self.writeln("}");
    }

    fn format_trait_method(&mut self, m: &TraitMethod) {
        self.write_outer_doc(m.doc_comment.as_ref());
        if m.attributes.is_empty() {
            if let Some(key) = &m.lang_item {
                self.write_indent();
                self.write(&format!("#[lang_item(\"{key}\")]\n"));
            }
        } else {
            self.format_attributes(&m.attributes);
        }
        self.flush_after_attributes(&m.attributes);
        self.write_indent();
        self.write("fn ");
        self.write_ident(m.name);
        if m.consumes_self {
            self.format_opt_type_params(m.type_params.as_ref());
            self.write("(consume self");
            let rest = m.params.get(1..).unwrap_or(&[]);
            if !rest.is_empty() {
                self.write(", ");
            }
            self.format_params(rest);
            self.write(")");
            if let Some(ret) = m.return_type.as_ref() {
                self.write(" -> ");
                self.format_type_expr(&ret.0);
            }
            self.format_opt_where_clause(m.where_clause.as_ref());
        } else {
            self.format_fn_signature(
                m.span.start,
                m.type_params.as_ref(),
                &m.params,
                m.return_type.as_ref(),
                m.where_clause.as_ref(),
            );
        }
        if let Some(body) = &m.body {
            self.write(" ");
            self.format_block(body, m.span.end);
            self.newline();
        } else {
            self.write_token(";\n", |t| matches!(t, hew_lexer::Token::Semicolon));
        }
    }

    fn format_impl(&mut self, decl: &ImplDecl, span_end: usize) {
        self.write_outer_doc(decl.doc_comment.as_ref());
        self.write_indent();
        self.write("impl");
        self.format_opt_type_params(decl.type_params.as_ref());
        self.write(" ");
        if let Some(bound) = &decl.trait_bound {
            self.format_trait_bound(bound);
            self.write(" for ");
        }
        self.format_type_expr(&decl.target_type.0);
        self.format_opt_where_clause(decl.where_clause.as_ref());
        self.write(" {\n");
        self.indent += 1;
        // Aliases and methods print in source order; the AST keeps them in
        // separate lists.
        let mut members: Vec<(usize, Result<&crate::ast::ImplTypeAlias, &FnDecl>)> = decl
            .type_aliases
            .iter()
            .map(|alias| (alias.span.start, Ok(alias)))
            .chain(
                decl.methods
                    .iter()
                    .map(|m| (member_start(&m.attributes, m.fn_span.start), Err(m))),
            )
            .collect();
        members.sort_by_key(|(start, _)| *start);
        for (i, (start, member)) in members.iter().enumerate() {
            self.begin_member(*start, i > 0);
            match member {
                Ok(alias) => {
                    self.write_indent();
                    self.write("type ");
                    self.write_ident(alias.name);
                    self.write(" = ");
                    self.format_type_expr(&alias.ty.0);
                    self.write_token(";\n", |t| matches!(t, hew_lexer::Token::Semicolon));
                    self.end_member(alias.span.end);
                }
                Err(method) => self.format_fn(method, span_end),
            }
        }
        if self.has_comments() {
            self.flush_block_end_comments(span_end);
        }
        self.indent -= 1;
        self.writeln("}");
    }

    fn format_extern_block(&mut self, decl: &ExternBlock, span_end: usize) {
        self.write_indent();
        self.write("extern \"");
        self.write(&decl.abi);
        self.write("\" {\n");
        self.indent += 1;
        for f in &decl.functions {
            // flush inline comments that appear before this fn
            self.flush_comments_before(f.span.start);
            // position at fn start so the blank-line heuristic in the
            // trailing-comment flush below counts only newlines between
            // the fn declaration and any trailing comment on its line
            self.prev_source_pos = f.span.start;
            self.format_extern_fn(f);
            // flush any trailing comment on this fn's line; f.span.end
            // is the first byte after the trailing `;`, so any same-line
            // comment falls in the range [f.span.start, f.span.end)
            self.flush_comments_before(f.span.end);
            self.prev_source_pos = self.prev_source_pos.max(f.span.end);
        }
        if self.has_comments() {
            self.flush_block_end_comments(span_end);
        }
        self.indent -= 1;
        self.writeln("}");
    }

    fn format_extern_fn(&mut self, f: &ExternFnDecl) {
        self.format_attributes(&f.attributes);
        self.write_indent();
        self.write("fn ");
        self.write_ident(f.name);
        self.write("(");
        self.format_params(&f.params);
        if f.is_variadic {
            if !f.params.is_empty() {
                self.write(", ");
            }
            self.write("...");
        }
        self.write(")");
        if let Some(ret) = &f.return_type {
            self.write(" -> ");
            self.format_type_expr(&ret.0);
        }
        self.write_token(";\n", |t| matches!(t, hew_lexer::Token::Semicolon));
    }

    fn format_actor(&mut self, decl: &ActorDecl, span_start: usize, span_end: usize) {
        self.write_outer_doc(decl.doc_comment.as_ref());
        if !self.format_item_attributes(span_start) {
            if let Some(bytes) = decl.max_heap_bytes {
                self.write_indent();
                self.write(&format!("#[max_heap({bytes})]\n"));
            }
        }
        self.write_indent();
        self.write_visibility(decl.visibility);
        self.write("actor ");
        self.write_ident(decl.name);
        if !decl.type_params.is_empty() {
            self.write("<");
            let mut first = true;
            for param in &decl.type_params {
                if !first {
                    self.write(", ");
                }
                first = false;
                self.write_ident(param.name);
                if !param.bounds.is_empty() {
                    self.write(": ");
                    self.format_trait_bound_list(&param.bounds);
                }
            }
            self.write(">");
        }
        if let Some(supers) = &decl.super_traits {
            self.write(": ");
            self.format_trait_bound_list(supers);
        }
        self.write(" {\n");
        self.indent += 1;
        // Members print in source order. The AST groups them by kind, so the
        // spans recover the order the author wrote; with no source (a
        // synthesized program) the stable sort keeps the grouping.
        let mut members: Vec<(usize, ActorMember<'_>)> = Vec::new();
        members.extend(
            decl.fields
                .iter()
                .map(|f| (f.span.start, ActorMember::Field(f))),
        );
        if let Some(span) = &decl.mailbox_span {
            members.push((span.start, ActorMember::Mailbox));
        } else if decl.mailbox_capacity.is_some() {
            members.push((0, ActorMember::Mailbox));
        }
        if let Some(init) = &decl.init {
            members.push((init.span.start, ActorMember::Init(init)));
        }
        members.extend(decl.receive_fns.iter().map(|r| {
            (
                member_start(&r.attributes, r.span.start),
                ActorMember::Receive(r),
            )
        }));
        members.extend(decl.methods.iter().map(|m| {
            (
                member_start(&m.attributes, m.fn_span.start),
                ActorMember::Method(m),
            )
        }));
        members.sort_by_key(|(start, _)| *start);

        let mut previous_was_field = false;
        for (i, (start, member)) in members.iter().enumerate() {
            let is_field = matches!(member, ActorMember::Field(_));
            self.begin_member(*start, i > 0 && !(is_field && previous_was_field));
            previous_was_field = is_field;
            match member {
                ActorMember::Field(field) => {
                    self.format_field_decl(field);
                    self.end_member(field.span.end);
                }
                ActorMember::Mailbox => {
                    self.format_actor_mailbox(decl);
                    if let Some(span) = &decl.mailbox_span {
                        self.end_member(span.end);
                    }
                }
                ActorMember::Init(init) => self.format_actor_init(init, span_end),
                ActorMember::Receive(recv) => self.format_receive_fn(recv, span_end),
                ActorMember::Method(method) => self.format_fn(method, span_end),
            }
        }

        if self.has_comments() {
            self.flush_block_end_comments(span_end);
        }
        self.indent -= 1;
        self.writeln("}");
    }

    fn format_actor_mailbox(&mut self, decl: &ActorDecl) {
        let Some(cap) = &decl.mailbox_capacity else {
            return;
        };
        self.write_indent();
        self.write("mailbox ");
        self.write(&cap.to_string());
        if let Some(policy) = &decl.overflow_policy {
            self.write(" overflow ");
            match policy {
                OverflowPolicy::DropNew => self.write("drop_new"),
                OverflowPolicy::DropOld => self.write("drop_old"),
                OverflowPolicy::Block => self.write("block"),
                OverflowPolicy::Fail => self.write("fail"),
                OverflowPolicy::Coalesce {
                    key_field,
                    fallback,
                } => {
                    self.write("coalesce(");
                    self.write_ident(*key_field);
                    self.write(")");
                    if let Some(fb) = fallback {
                        self.write(" fallback ");
                        match fb {
                            crate::ast::OverflowFallback::DropNew => self.write("drop_new"),
                            crate::ast::OverflowFallback::DropOld => self.write("drop_old"),
                            crate::ast::OverflowFallback::Block => self.write("block"),
                            crate::ast::OverflowFallback::Fail => self.write("fail"),
                        }
                    }
                }
            }
        }
        self.write(",\n");
    }

    #[expect(clippy::too_many_lines, reason = "machine formatting has many clauses")]
    fn format_machine(&mut self, decl: &MachineDecl, span_start: usize, span_end: usize) {
        self.write_indent();
        self.write_visibility(decl.visibility);
        self.write("machine ");
        self.write_ident(decl.name);
        if !decl.type_params.is_empty() || !decl.const_params.is_empty() {
            self.write("<");
            let mut first = true;
            for param in &decl.type_params {
                if !first {
                    self.write(", ");
                }
                first = false;
                self.write_ident(param.name);
                if !param.bounds.is_empty() {
                    self.write(": ");
                    self.format_trait_bound_list(&param.bounds);
                }
            }
            for param in &decl.const_params {
                if !first {
                    self.write(", ");
                }
                first = false;
                self.write("const ");
                self.write_ident(param.name);
                self.write(": ");
                match param.ty {
                    crate::ast::ConstParamTy::Usize => self.write("usize"),
                }
                if let Some(default) = param.default {
                    self.write(" = ");
                    self.write(&default.to_string());
                }
            }
            self.write(">");
        }
        self.format_opt_where_clause(decl.where_clause.as_ref());
        self.write(" {\n");
        self.indent += 1;

        // Composite-group membership: substates owned by a composite are
        // re-emitted inside their `state Composite { … }` block (driven by the
        // side-table), not as flat top-level states.
        let composite_members: std::collections::HashSet<Ident> = decl
            .composite_groups
            .iter()
            .flat_map(|g| g.members.iter().copied())
            .collect();

        // Top-level transitions, excluding those that belong to a composite's
        // parent-rule block (those are re-emitted inside the composite).
        let parent_rule_keys: std::collections::HashSet<(Ident, Ident, Ident)> = decl
            .composite_groups
            .iter()
            .flat_map(|g| {
                g.members.iter().flat_map(move |m| {
                    g.parent_transitions
                        .iter()
                        .map(move |pt| (*m, pt.event_name, pt.target_state))
                })
            })
            .collect();

        // Sections and members print in source order; a synthesized machine
        // keeps the canonical order the list is built in.
        let mut members: Vec<(usize, MachineMember<'_>)> = Vec::new();
        if !decl.events.is_empty() {
            let at = self.machine_section(span_start, span_end, "events");
            members.push((at, MachineMember::Events));
        }
        if !decl.emits.is_empty() {
            let at = self.machine_section(span_start, span_end, "emits");
            members.push((at, MachineMember::Emits));
        }
        members.extend(
            decl.states
                .iter()
                .filter(|state| !composite_members.contains(&state.name))
                .map(|state| (state.span.start, MachineMember::State(state))),
        );
        members.extend(
            decl.composite_groups
                .iter()
                .map(|group| (group.span.start, MachineMember::Composite(group))),
        );
        members.extend(
            decl.transitions
                .iter()
                .filter(|t| {
                    !parent_rule_keys.contains(&(t.source_state, t.event_name, t.target_state))
                })
                .map(|t| (t.span.start, MachineMember::Transition(t))),
        );
        if decl.has_default {
            let at = self.machine_section(span_start, span_end, "default");
            members.push((at, MachineMember::Default));
        }
        // Without source the section keywords have no position, so keep the
        // canonical order the list was built in.
        if !self.source.is_empty() {
            members.sort_by_key(|(start, _)| *start);
        }

        let mut previous: Option<std::mem::Discriminant<MachineMember<'_>>> = None;
        for (start, member) in &members {
            let kind = std::mem::discriminant(member);
            // Canonically, a blank line separates sections and composites.
            let canonical = previous
                .is_some_and(|p| p != kind || matches!(member, MachineMember::Composite(_)));
            previous = Some(kind);
            self.begin_member(*start, canonical);
            match member {
                MachineMember::Events => {
                    self.format_machine_events("events", &decl.events, span_end);
                }
                MachineMember::Emits => self.format_machine_events("emits", &decl.emits, span_end),
                MachineMember::State(state) => {
                    self.format_machine_leaf_state(state);
                    self.end_member(state.span.end);
                }
                MachineMember::Composite(group) => {
                    self.format_machine_composite(decl, group, span_end);
                    self.end_member(group.span.end);
                }
                MachineMember::Transition(transition) => {
                    self.write_indent();
                    self.format_machine_transition(transition, span_end);
                    self.newline();
                    self.end_member(transition.span.end);
                }
                MachineMember::Default => {
                    self.write_indent();
                    self.write("default { state }\n");
                    let close = self.find_block_close(*start, span_end);
                    self.end_member(close + 1);
                }
            }
        }
        if self.has_comments() {
            self.flush_block_end_comments(span_end);
        }

        self.indent -= 1;
        self.writeln("}");
    }

    /// Start of the machine-body section keyword `word` (`events`, `emits`,
    /// `default`) directly inside the machine's braces; 0 without source.
    fn machine_section(&self, start: usize, end: usize, word: &str) -> usize {
        let mut depth = 0usize;
        for (token, span) in &self.tokens[self.token_at_or_after(start)..] {
            if span.start >= end {
                break;
            }
            match token {
                hew_lexer::Token::LeftBrace => depth += 1,
                hew_lexer::Token::RightBrace => depth = depth.saturating_sub(1),
                _ if depth == 1 && self.source[span.clone()] == *word => return span.start,
                _ => {}
            }
        }
        0
    }

    /// Emit an `events { … }` or `emits { … }` header, placing each
    /// comment before the declaration it precedes.
    fn format_machine_events(
        &mut self,
        keyword: &str,
        events: &[crate::ast::MachineEvent],
        span_end: usize,
    ) {
        let open = self.find_open_brace(self.prev_source_pos, span_end);
        self.write_indent();
        self.write(keyword);
        self.write(" {\n");
        if let Some(open) = open {
            self.prev_source_pos = open + 1;
        }
        self.indent += 1;
        for event in events {
            self.begin_member(event.span.start, false);
            self.write_indent();
            self.write_ident(event.name);
            self.format_machine_field_list(&event.fields);
            self.write("\n");
            self.end_member(event.span.end);
        }
        if self.has_comments() {
            self.flush_block_end_comments(span_end);
        }
        self.indent -= 1;
        self.write_indent();
        self.write("}\n");
    }

    /// Emit `{ name: Type, … }` after an event/state name, or `,` when empty.
    fn format_machine_field_list(&mut self, fields: &[(Ident, Spanned<TypeExpr>)]) {
        if fields.is_empty() {
            self.write(",");
        } else {
            self.write(" { ");
            for (i, (name, ty)) in fields.iter().enumerate() {
                if i > 0 {
                    self.write(" ");
                }
                self.write_ident(*name);
                self.write(": ");
                self.format_type_expr(&ty.0);
                self.write(",");
            }
            self.write(" },");
        }
    }

    /// Emit one leaf `state` declaration (fields + entry/exit).
    fn format_machine_leaf_state(&mut self, state: &MachineState) {
        self.write_indent();
        self.write("state ");
        self.write_ident(state.name);
        let has_entry_exit = state.entry.is_some() || state.exit.is_some();
        if has_entry_exit {
            self.write(" {\n");
            self.indent += 1;
            for (name, ty) in &state.fields {
                self.write_indent();
                self.write_ident(*name);
                self.write(": ");
                self.format_type_expr(&ty.0);
                self.write(",\n");
            }
            if let Some(entry) = &state.entry {
                self.write_indent();
                self.write("entry ");
                self.format_block(entry, self.source.len());
                self.newline();
            }
            if let Some(exit) = &state.exit {
                self.write_indent();
                self.write("exit ");
                self.format_block(exit, self.source.len());
                self.newline();
            }
            self.indent -= 1;
            self.write_indent();
            self.write("},\n");
        } else if !state.fields.is_empty() {
            self.write(" {");
            for (name, ty) in &state.fields {
                self.write(" ");
                self.write_ident(*name);
                self.write(": ");
                self.format_type_expr(&ty.0);
                self.write(",");
            }
            self.write(" },\n");
        } else {
            self.write(",\n");
        }
    }

    /// Emit one machine transition head + body in the `=>` / `reenter` surface.
    /// Caller writes the leading indent.
    fn format_machine_transition(&mut self, transition: &MachineTransition, span_end: usize) {
        self.write("on ");
        self.write_ident(transition.event_name);
        // Re-emit the `on E(a, b):` head binding from the side-list. The
        // parser splices a `let a = event.a;` prelude into the body for
        // lowering; we strip that prelude below so it does not double up.
        if !transition.event_bindings.is_empty() {
            self.write("(");
            for (i, name) in transition.event_bindings.iter().enumerate() {
                if i > 0 {
                    self.write(", ");
                }
                self.write_ident(*name);
            }
            self.write(")");
        }
        self.write(": ");
        // Source states are patterns and have no contextual form, so they are
        // always emitted bare. The target's leading `.` comes from the AST's
        // authored-spelling flag — never inferred from the body expression,
        // which is not the authority for it and does not exist in the
        // block/payload-shorthand forms.
        self.write_ident(transition.source_state);
        self.write_token(" => ", |t| matches!(t, hew_lexer::Token::FatArrow));
        if transition.target_is_contextual {
            self.write(".");
        }
        self.write_ident(
            transition
                .target_composite
                .unwrap_or(transition.target_state),
        );
        if transition.reenter {
            self.write(" reenter");
        }
        if let Some(guard) = &transition.guard {
            self.write(" when ");
            self.format_expr(guard);
        }
        // Re-emit the AUTHORED body: strip the composite entry/exit hook
        // prelude (D2/D3 splices, counted by `composite_prelude_len`) first,
        // then the head-binding `let a = event.a;` prelude. Both are parser
        // desugar artifacts the formatter must not echo, or re-parsing would
        // double-apply them.
        let hook_stripped;
        let after_hooks = if transition.composite_prelude_len == 0 {
            &transition.body.0
        } else {
            hook_stripped = Self::strip_leading_block_stmts(
                &transition.body.0,
                transition.composite_prelude_len,
            );
            &hook_stripped
        };
        let stripped_body;
        let body_expr = if transition.event_bindings.is_empty() {
            after_hooks
        } else {
            stripped_body =
                Self::strip_event_binding_prelude(after_hooks, &transition.event_bindings);
            &stripped_body
        };
        match transition.body_form {
            MachineTransitionBodyForm::Implicit => self.write(","),
            MachineTransitionBodyForm::PayloadShorthand => {
                // The head already wrote the target name (with its authored
                // dot); the shorthand re-emits only the payload field list.
                // Bare targets carry a `StructInit`, contextual ones a
                // `ContextVariant` record — both name the target state.
                let payload = match body_expr {
                    Expr::StructInit {
                        path, fields, base, ..
                    } if path.as_single() == Some(transition.target_state) => {
                        Some((fields, base.as_deref()))
                    }
                    Expr::ContextVariant(context) if context.name == transition.target_state => {
                        context
                            .record
                            .as_ref()
                            .map(|record| (&record.fields, record.base.as_deref()))
                    }
                    _ => None,
                };
                if let Some((fields, base)) = payload {
                    self.write(" { ");
                    // The base comes first, so the fields that override it read
                    // after the value they override (D488).
                    if let Some(base) = base {
                        self.write("..");
                        self.format_expr(base);
                        if !fields.is_empty() {
                            self.write(", ");
                        }
                    }
                    for (i, (fname, fval)) in fields.iter().enumerate() {
                        if i > 0 {
                            self.write(", ");
                        }
                        self.write_ident(*fname);
                        self.write(": ");
                        self.format_expr(fval);
                    }
                    self.write(" }");
                } else {
                    self.write(" { ");
                    self.format_expr(&(body_expr.clone(), transition.body.1.clone()));
                    self.write(" }");
                }
            }
            MachineTransitionBodyForm::Block => {
                self.write(" ");
                if let Expr::Block(block) = body_expr {
                    self.format_block(block, span_end);
                } else {
                    self.write("{ ");
                    self.format_expr(&(body_expr.clone(), transition.body.1.clone()));
                    self.write(" }");
                }
            }
        }
    }

    /// Drop the first `count` statements of a block body (the composite
    /// entry/exit hook splice prelude). If only a tail remains, surface it
    /// directly so the body collapses back to its authored shorthand.
    fn strip_leading_block_stmts(body: &Expr, count: usize) -> Expr {
        let Expr::Block(block) = body else {
            return body.clone();
        };
        if count == 0 || count > block.stmts.len() {
            return body.clone();
        }
        let remaining: Vec<Spanned<Stmt>> = block.stmts[count..].to_vec();
        if remaining.is_empty() {
            if let Some(tail) = &block.trailing_expr {
                return tail.0.clone();
            }
        }
        Expr::Block(Block {
            stmts: remaining,
            trailing_expr: block.trailing_expr.clone(),
        })
    }

    /// Strip the leading `let <binding> = event.<binding>;` prelude statements
    /// the parser splices in for an `on E(bindings): …` head binding, so the
    /// formatter can re-emit the head form without the desugar. If, after
    /// stripping, only a tail expression remains, that tail becomes the body
    /// (collapsing a one-line `on E(x): S => T { Body }` back to its head form).
    fn strip_event_binding_prelude(body: &Expr, bindings: &[Ident]) -> Expr {
        let Expr::Block(block) = body else {
            return body.clone();
        };
        // Count how many leading statements are the synthesized prelude lets.
        let mut skip = 0;
        for (stmt, _) in &block.stmts {
            if skip >= bindings.len() {
                break;
            }
            let Stmt::Let {
                pattern: (Pattern::Identifier(name), _),
                value: Some((Expr::FieldAccess { object, field }, _)),
                ..
            } = stmt
            else {
                break;
            };
            let is_event_field = matches!(&object.0, Expr::Ident(o) if o.name.as_str() == "event");
            if is_event_field && *name == field.0 && bindings.contains(name) {
                skip += 1;
            } else {
                break;
            }
        }
        if skip == 0 {
            return body.clone();
        }
        let remaining: Vec<Spanned<Stmt>> = block.stmts[skip..].to_vec();
        // If only a tail expression remains, surface it directly so the body
        // collapses to the canonical struct-init / identifier shorthand.
        if remaining.is_empty() {
            if let Some(tail) = &block.trailing_expr {
                return tail.0.clone();
            }
        }
        Expr::Block(Block {
            stmts: remaining,
            trailing_expr: block.trailing_expr.clone(),
        })
    }

    /// Re-emit a composite `state Composite { … }` block from the grouping
    /// side-table: composite fields, entry/exit, `initial`-marked substates,
    /// then the parent-level transitions authored inside the block.
    fn format_machine_composite(
        &mut self,
        decl: &MachineDecl,
        group: &crate::ast::CompositeGroup,
        span_end: usize,
    ) {
        self.write_indent();
        self.write("state ");
        self.write_ident(group.name);
        self.write(" {\n");
        self.indent += 1;

        for (name, ty) in &group.fields {
            self.write_indent();
            self.write_ident(*name);
            self.write(": ");
            self.format_type_expr(&ty.0);
            self.write(",\n");
        }
        if let Some(entry) = &group.entry {
            self.write_indent();
            self.write("entry ");
            self.format_block(entry, self.source.len());
            self.newline();
        }
        if let Some(exit) = &group.exit {
            self.write_indent();
            self.write("exit ");
            self.format_block(exit, self.source.len());
            self.newline();
        }

        let mut inner: Vec<(usize, Result<&MachineState, &MachineTransition>)> = group
            .members
            .iter()
            .filter_map(|member| decl.states.iter().find(|s| &s.name == member))
            .map(|state| (state.span.start, Ok(state)))
            .chain(
                group
                    .parent_transitions
                    .iter()
                    .map(|pt| (pt.span.start, Err(pt))),
            )
            .collect();
        inner.sort_by_key(|(start, _)| *start);
        for (start, item) in inner {
            let state = match item {
                Ok(state) => state,
                Err(pt) => {
                    self.begin_member(start, false);
                    self.write_indent();
                    self.format_machine_transition(pt, span_end);
                    self.newline();
                    self.end_member(pt.span.end);
                    continue;
                }
            };
            let member_name = &state.name;
            self.begin_member(start, false);
            self.write_indent();
            if &group.initial == member_name {
                self.write("initial ");
            }
            // Substate fields exclude the composite-owned shared fields (which
            // are emitted on the composite, not stamped here).
            let own_fields: Vec<&(Ident, Spanned<TypeExpr>)> = state
                .fields
                .iter()
                .filter(|(fname, _)| !group.fields.iter().any(|(gn, _)| gn == fname))
                .collect();
            self.format_machine_substate(state.name, &own_fields, state, span_end);
            self.end_member(state.span.end);
        }
        if self.has_comments() {
            self.flush_block_end_comments(group.span.end);
        }

        self.indent -= 1;
        self.write_indent();
        self.write("},\n");
    }

    /// Emit a substate declaration inside a composite block. The `initial`
    /// modifier (when present) is already written by the caller.
    fn format_machine_substate(
        &mut self,
        name: Ident,
        own_fields: &[&(Ident, Spanned<TypeExpr>)],
        state: &MachineState,
        _span_end: usize,
    ) {
        self.write("state ");
        self.write_ident(name);
        let has_entry_exit = state.entry.is_some() || state.exit.is_some();
        if has_entry_exit {
            self.write(" {\n");
            self.indent += 1;
            for (fname, ty) in own_fields {
                self.write_indent();
                self.write_ident(*fname);
                self.write(": ");
                self.format_type_expr(&ty.0);
                self.write(",\n");
            }
            if let Some(entry) = &state.entry {
                self.write_indent();
                self.write("entry ");
                self.format_block(entry, self.source.len());
                self.newline();
            }
            if let Some(exit) = &state.exit {
                self.write_indent();
                self.write("exit ");
                self.format_block(exit, self.source.len());
                self.newline();
            }
            self.indent -= 1;
            self.write_indent();
            self.write("},\n");
        } else if !own_fields.is_empty() {
            self.write(" {");
            for (fname, ty) in own_fields {
                self.write(" ");
                self.write_ident(*fname);
                self.write(": ");
                self.format_type_expr(&ty.0);
                self.write(",");
            }
            self.write(" },\n");
        } else {
            self.write(",\n");
        }
    }

    fn format_field_decl(&mut self, f: &FieldDecl) {
        self.write_outer_doc(f.doc_comment.as_ref());
        self.write_indent();
        // An immutable field may be written without `let`; keep the author's
        // spelling.
        let bare = self.source.get(f.span.clone()).is_some_and(|text| {
            matches!(
                hew_lexer::Lexer::new(text).next(),
                Some((hew_lexer::Token::Identifier(_), _))
            )
        });
        if f.is_mutable {
            self.write("var ");
        } else if !bare {
            self.write("let ");
        }
        self.write_ident(f.name);
        self.write(": ");
        self.format_type_expr(&f.ty.0);
        if let Some(default) = &f.default {
            self.write(" = ");
            self.format_expr(default);
        }
        self.write(",\n");
    }

    fn format_actor_init(&mut self, init: &ActorInit, scope_end: usize) {
        self.write_indent();
        self.write("init(");
        self.format_params(&init.params);
        self.write(") ");
        self.format_block(&init.body, scope_end);
        self.newline();
    }

    /// Emit, each on its own line, the comments between a declaration's
    /// attributes and its first keyword.
    fn flush_after_attributes(&mut self, attrs: &[Attribute]) {
        if let Some(end) = attrs.iter().map(|a| a.span.end).max() {
            let keyword = self.find_token(end, self.source.len(), |t| {
                !matches!(
                    t,
                    hew_lexer::Token::DocComment(_) | hew_lexer::Token::InnerDocComment(_)
                )
            });
            if let Some(start) = keyword {
                self.flush_comments_before(start);
            }
        }
    }

    fn format_attributes(&mut self, attrs: &[Attribute]) {
        for attr in attrs {
            self.flush_comments_before(attr.span.start);
            if self
                .source
                .get(attr.span.clone())
                .is_some_and(|t| t.starts_with("#["))
            {
                let text = self.attribute_text(&attr.span);
                self.write_indent();
                self.write(&text);
                self.newline();
                continue;
            }
            self.write_indent();
            self.write("#[");
            self.write(&attr.name);
            if !attr.args.is_empty() {
                self.write("(");
                for (i, arg) in attr.args.iter().enumerate() {
                    if i > 0 {
                        self.write(", ");
                    }
                    match arg {
                        AttributeArg::Positional(s) => {
                            // If the value contains characters that are not valid
                            // in a bare identifier (e.g. `.` in `"math.sqrt"`),
                            // re-quote it as a string literal so the output round-trips.
                            let needs_quotes = s.chars().any(|c| !c.is_alphanumeric() && c != '_');
                            if needs_quotes {
                                self.write("\"");
                                self.write(s);
                                self.write("\"");
                            } else {
                                self.write(s);
                            }
                        }
                        AttributeArg::KeyValue { key, value } => {
                            self.write(key);
                            self.write(" = ");
                            // Attribute values retain literal contents in the
                            // AST. Emit them bare only when lexing preserves a
                            // single identifier or the same integer value.
                            let tokens = hew_lexer::lex(value);
                            let bare = match tokens.as_slice() {
                                [(hew_lexer::Token::Identifier(name), _)] => *name == value,
                                [(hew_lexer::Token::Integer(integer), _)] => *integer == value,
                                _ => false,
                            };
                            if bare {
                                self.write(value);
                            } else {
                                self.write("\"");
                                self.write(value);
                                self.write("\"");
                            }
                        }
                        AttributeArg::Duration(ns) => self.write(&format_duration_ns(*ns)),
                    }
                }
                self.write(")");
            }
            self.write("]\n");
        }
    }

    fn format_receive_fn(&mut self, recv: &ReceiveFnDecl, scope_end: usize) {
        self.write_outer_doc(recv.doc_comment.as_ref());
        self.format_attributes(&recv.attributes);
        self.flush_after_attributes(&recv.attributes);
        self.write_indent();

        if recv.is_generator {
            self.write("receive gen fn ");
        } else {
            self.write("receive fn ");
        }
        self.write_ident(recv.name);
        self.format_fn_signature(
            recv.span.start,
            recv.type_params.as_ref(),
            &recv.params,
            recv.return_type.as_ref(),
            recv.where_clause.as_ref(),
        );
        self.write(" ");
        self.format_block(&recv.body, scope_end);
        self.newline();
    }

    fn format_supervisor(&mut self, decl: &SupervisorDecl, span_start: usize, span_end: usize) {
        self.write_indent();
        self.write_visibility(decl.visibility);
        self.write("supervisor ");
        self.write_ident(decl.name);
        if !decl.type_params.is_empty() {
            self.format_opt_type_params(Some(&decl.type_params));
        }
        // Emit the config-param clause when present: `supervisor App(config: T)`.
        // Without this, `hew fmt` silently drops the param and breaks all
        // config.field references in the body — a fail-open on the dev-tool surface.
        if !decl.params.is_empty() {
            self.write("(");
            self.format_params(&decl.params);
            self.write(")");
        }
        self.write(" {\n");
        self.indent += 1;

        // Clauses and children print in source order. Write `strategy:` only
        // when the declaration carries one: materializing the default would
        // rewrite the program instead of formatting it.
        let mut clauses: Vec<(usize, Option<&ChildSpec>, &str)> = Vec::new();
        if decl.strategy.is_some() {
            clauses.push((
                self.machine_section(span_start, span_end, "strategy"),
                None,
                "strategy",
            ));
        }
        if decl.intensity.is_some() {
            clauses.push((
                self.machine_section(span_start, span_end, "intensity"),
                None,
                "intensity",
            ));
        }
        clauses.extend(
            decl.children
                .iter()
                .map(|c| (c.span.start, Some(c), "child")),
        );
        clauses.sort_by_key(|(start, _, _)| *start);

        let mut previous_was_child = false;
        for (i, (start, child, clause)) in clauses.into_iter().enumerate() {
            let canonical = child.is_some() && (i == 0 || !previous_was_child);
            previous_was_child = child.is_some();
            self.begin_member(start, canonical);
            if let Some(child) = child {
                self.format_child_spec(child);
                self.end_member(child.span.end);
                continue;
            }
            self.write_indent();
            if clause == "strategy" {
                self.write("strategy: ");
                match decl.strategy {
                    Some(SupervisorStrategy::OneForOne) => self.write("one_for_one"),
                    Some(SupervisorStrategy::OneForAll) => self.write("one_for_all"),
                    Some(SupervisorStrategy::RestForOne) => self.write("rest_for_one"),
                    Some(SupervisorStrategy::SimpleOneForOne) => self.write("simple_one_for_one"),
                    None => {}
                }
            } else if let Some(intensity) = &decl.intensity {
                self.write("intensity: ");
                self.write(&intensity.restarts.to_string());
                self.write(" within ");
                self.write(&intensity.window);
            }
            self.write(",\n");
        }
        if self.has_comments() {
            self.flush_block_end_comments(span_end);
        }

        self.indent -= 1;
        self.writeln("}");
    }

    /// Whether the source writes `child name: Actor()` with an empty
    /// argument list, which means the same as leaving it out.
    fn child_writes_parens(&self, spec: &ChildSpec) -> bool {
        let mut index = self.token_at_or_after(spec.span.start);
        let end = self.token_at_or_after(spec.span.end);
        while index < end
            && !matches!(&self.tokens[index].0, hew_lexer::Token::Identifier(name) if spec.actor_type.as_single().is_some_and(|actor| *name == actor.name.as_str()))
        {
            index += 1;
        }
        index += 1;
        let mut depth = 0usize;
        while index < end {
            match self.tokens[index].0 {
                hew_lexer::Token::Less => depth += 1,
                hew_lexer::Token::Greater if depth > 0 => depth -= 1,
                hew_lexer::Token::LeftParen if depth == 0 => return true,
                _ if depth == 0 => return false,
                _ => {}
            }
            index += 1;
        }
        false
    }

    fn format_child_spec(&mut self, spec: &ChildSpec) {
        self.write_indent();
        // `pool` vs `child` is load-bearing — a pool is a dynamic
        // simple_one_for_one child, not a static one. The old formatter always
        // wrote `child`, silently dropping pool-ness; preserve it here.
        self.write(if spec.is_pool { "pool " } else { "child " });
        self.write_ident(spec.name);
        self.write(": ");
        self.format_path(&spec.actor_type);
        if !spec.type_args.is_empty() {
            self.write("<");
            self.comma_sep(&spec.type_args, |formatter, ty| {
                formatter.format_type_expr(&ty.0);
            });
            self.write(">");
        }
        if !spec.args.is_empty() || self.child_writes_parens(spec) {
            self.write("(");
            self.comma_sep(&spec.args, |f, (field_name, arg)| {
                f.write_ident(*field_name);
                f.write(": ");
                f.format_expr(arg);
            });
            self.write(")");
        }
        // Pool arity is a clause, so it prints outside the parentheses. It
        // comes first among the clauses because arity binds to the template the
        // parentheses just described.
        if let Some(count) = &spec.count {
            self.write(" count: ");
            self.format_expr(count);
        }
        if let Some(restart) = &spec.restart {
            self.write(" restart: ");
            match restart {
                RestartPolicy::Permanent => self.write("permanent"),
                RestartPolicy::Transient => self.write("transient"),
                RestartPolicy::Temporary => self.write("temporary"),
            }
        }
        if let Some(shutdown) = &spec.shutdown {
            self.write(" shutdown: ");
            match shutdown {
                ShutdownDirective::Timeout(d) => self.write(d),
                ShutdownDirective::BrutalKill => self.write("brutal_kill"),
                ShutdownDirective::Infinity => self.write("infinity"),
            }
        }
        // `wired_to:` was silently dropped by the old formatter — preserve it.
        // HashMap iteration order is non-deterministic, so emit keys sorted to
        // keep the round-trip stable and idempotent.
        if let Some(wired_to) = &spec.wired_to {
            if !wired_to.is_empty() {
                let mut entries: Vec<(&String, &String)> = wired_to.iter().collect();
                entries.sort_by(|a, b| a.0.cmp(b.0));
                self.write(" wired_to: { ");
                self.comma_sep(&entries, |f, (key, sibling)| {
                    f.write(key);
                    f.write(": ");
                    f.write(sibling);
                });
                self.write(" }");
            }
        }
        self.write(",\n");
    }

    fn format_fn(&mut self, decl: &FnDecl, span_end: usize) {
        self.write_outer_doc(decl.doc_comment.as_ref());
        self.format_attributes(&decl.attributes);
        self.flush_after_attributes(&decl.attributes);
        self.write_indent();
        self.write_visibility(decl.visibility);

        if decl.is_generator {
            self.write("gen ");
        }
        self.write("fn ");
        self.write_ident(decl.name);
        // An inherent-impl `consume self` receiver is materialised as the
        // leading `self: Self` parameter; emit the `consume self` spelling and
        // skip that synthetic first parameter, mirroring the type-body formatter.
        if decl.consumes_self {
            self.format_opt_type_params(decl.type_params.as_ref());
            self.write("(consume self");
            let rest = decl.params.get(1..).unwrap_or(&[]);
            if !rest.is_empty() {
                self.write(", ");
            }
            self.format_params(rest);
            self.write(")");
            if let Some(ret) = decl.return_type.as_ref() {
                self.write(" -> ");
                self.format_type_expr(&ret.0);
            }
            self.format_opt_where_clause(decl.where_clause.as_ref());
        } else {
            self.format_fn_signature(
                decl.fn_span.start,
                decl.type_params.as_ref(),
                &decl.params,
                decl.return_type.as_ref(),
                decl.where_clause.as_ref(),
            );
        }
        self.write(" ");
        self.format_block(&decl.body, span_end);
        self.newline();
    }

    // ------------------------------------------------------------------
    // Types
    // ------------------------------------------------------------------

    /// One parameter list and optional reply, shared by `fn` and `actor` types.
    fn format_callable_type(
        &mut self,
        head: &str,
        params: &[Spanned<TypeExpr>],
        return_type: &Spanned<TypeExpr>,
    ) {
        self.write(head);
        self.write("(");
        self.comma_sep(params, |f, p| f.format_type_expr(&p.0));
        self.write(")");
        // An omitted return type is unit; keep a unit the author wrote out.
        let written_unit = self
            .source
            .get(return_type.1.clone())
            .is_some_and(|t| t.split_whitespace().collect::<String>() == "()");
        if written_unit || !matches!(return_type.0, TypeExpr::Tuple(ref elems) if elems.is_empty())
        {
            self.write(" -> ");
            self.format_type_expr(&return_type.0);
        }
    }

    fn format_type_expr(&mut self, ty: &TypeExpr) {
        match ty {
            TypeExpr::Named { path, type_args } => {
                self.format_path(path);
                if let Some(args) = type_args {
                    self.write("<");
                    self.comma_sep(args, |f, arg| f.format_type_expr(&arg.0));
                    self.write(">");
                }
            }
            TypeExpr::QualifiedAssocPath(assoc) => {
                self.write("<");
                self.format_type_expr(&assoc.base.0);
                self.write(" as ");
                self.format_path(&assoc.trait_path);
                self.write(">");
                for member in &assoc.members {
                    self.write(".");
                    self.write_ident(*member);
                }
            }
            TypeExpr::Result { ok, err } => {
                self.write("Result<");
                self.format_type_expr(&ok.0);
                self.write(", ");
                self.format_type_expr(&err.0);
                self.write(">");
            }
            TypeExpr::Fallible { success, error } => {
                self.format_type_expr(&success.0);
                self.write(" fails ");
                self.format_type_expr(&error.0);
            }
            TypeExpr::Option(inner) => {
                self.write("Option<");
                self.format_type_expr(&inner.0);
                self.write(">");
            }
            TypeExpr::Tuple(elems) => {
                self.write("(");
                self.comma_sep(elems, |f, elem| f.format_type_expr(&elem.0));
                self.write(")");
            }
            TypeExpr::Array { element, size } => {
                self.write("[");
                self.format_type_expr(&element.0);
                self.write("; ");
                self.write(&size.to_string());
                self.write("]");
            }
            TypeExpr::Slice(inner) => {
                self.write("[");
                self.format_type_expr(&inner.0);
                self.write("]");
            }
            TypeExpr::Function {
                capabilities,
                params,
                return_type,
            } => self.format_callable_type(&format!("fn{capabilities}"), params, return_type),
            TypeExpr::ActorFn {
                params,
                return_type,
            } => self.format_callable_type("actor", params, return_type),
            TypeExpr::Pointer {
                is_mutable,
                pointee,
            } => {
                self.write(if *is_mutable { "*mut " } else { "*const " });
                self.format_type_expr(&pointee.0);
            }
            TypeExpr::Borrow(inner) => {
                self.write("&");
                self.format_type_expr(&inner.0);
            }
            TypeExpr::TraitObject(bounds) => {
                self.write("dyn ");
                if bounds.len() == 1 {
                    self.format_trait_bound(&bounds[0]);
                } else {
                    self.write("(");
                    for (i, bound) in bounds.iter().enumerate() {
                        if i > 0 {
                            self.write(" + ");
                        }
                        self.format_trait_bound(bound);
                    }
                    self.write(")");
                }
            }
            TypeExpr::Infer => {
                self.write("_");
            }
        }
    }

    /// Format `<type_params>(params) -> return_type where clause`.
    /// Write a signature whose declaration starts at `from` in the source.
    fn format_fn_signature(
        &mut self,
        from: usize,
        type_params: Option<&Vec<TypeParam>>,
        params: &[Param],
        return_type: Option<&Spanned<TypeExpr>>,
        where_clause: Option<&WhereClause>,
    ) {
        self.format_opt_type_params(type_params);
        let bounds = self.params_list(from);
        self.delimited_list("(", ")", params, bounds, true, true, Self::format_param);
        if let Some(ret) = return_type {
            self.write_token(" -> ", |t| matches!(t, hew_lexer::Token::Arrow));
            self.format_type_expr(&ret.0);
            if ret.1.start < ret.1.end {
                self.prev_source_pos = self.prev_source_pos.max(ret.1.end);
            }
        }
        self.format_opt_where_clause(where_clause);
    }

    fn format_opt_type_params(&mut self, params: Option<&Vec<TypeParam>>) {
        if let Some(params) = params {
            self.write("<");
            self.comma_sep(params, |f, p| {
                f.write_ident(p.name);
                if !p.bounds.is_empty() {
                    f.write(": ");
                    f.format_trait_bound_list(&p.bounds);
                }
            });
            self.write(">");
        }
    }

    fn format_trait_bound(&mut self, bound: &TraitBound) {
        self.format_path(&bound.path);
        if bound.type_args.is_some() || !bound.assoc_type_bindings.is_empty() {
            self.write("<");
            let mut needs_comma = false;
            if let Some(args) = &bound.type_args {
                for arg in args {
                    if needs_comma {
                        self.write(", ");
                    }
                    self.format_type_expr(&arg.0);
                    needs_comma = true;
                }
            }
            for binding in &bound.assoc_type_bindings {
                if needs_comma {
                    self.write(", ");
                }
                self.write_ident(binding.name);
                self.write(" = ");
                self.format_type_expr(&binding.ty.0);
                needs_comma = true;
            }
            self.write(">");
        }
    }

    fn format_trait_bound_list(&mut self, bounds: &[TraitBound]) {
        for (i, b) in bounds.iter().enumerate() {
            if i > 0 {
                self.write(" + ");
            }
            self.format_trait_bound(b);
        }
    }

    fn format_opt_where_clause(&mut self, clause: Option<&WhereClause>) {
        if let Some(clause) = clause {
            self.write_token(" where ", |t| matches!(t, hew_lexer::Token::Where));
            self.comma_sep(&clause.predicates, |f, pred| {
                f.flush_inline_comments(pred.ty.1.start);
                f.format_type_expr(&pred.ty.0);
                f.write(": ");
                f.format_trait_bound_list(&pred.bounds);
            });
        }
    }

    fn format_params(&mut self, params: &[Param]) {
        self.comma_sep(params, Self::format_param);
    }

    fn format_param(&mut self, p: &Param) {
        if p.name.name == sym::SELF_VALUE
            && matches!(
                &p.ty.0,
                TypeExpr::Named {
                    path,
                    type_args: None,
                } if path.as_single().is_some_and(|ident| ident.name == sym::SELF_TYPE)
            )
        {
            if p.is_mutable {
                self.write("var ");
            }
            self.write("self");
            return;
        }
        // `consume` precedes `var` in the surface grammar
        // (`fn sink(consume var c: Conn)`); emit it first so the
        // formatted output reparses to the same ownership disposition.
        // The `self` fast-path above never reaches here, and `consume
        // self` is rejected by the parser, so an affine receiver is
        // never mis-printed with a `consume` modifier.
        if p.is_consume {
            self.write("consume ");
        }
        if p.is_mutable {
            self.write("var ");
        }
        self.write_ident(p.name);
        self.write(": ");
        self.format_type_expr(&p.ty.0);
    }

    // ------------------------------------------------------------------
    // Blocks & statements
    // ------------------------------------------------------------------

    fn format_block(&mut self, block: &Block, scope_end: usize) {
        // A comment between a header and its `{` stays before the brace.
        let open = self.find_open_brace(self.prev_source_pos, scope_end);
        if let Some(open) = open {
            self.flush_inline_comments(open);
        }
        // Empty-block fast path: render `{}` on a single line when the block
        // has no statements, no trailing expression, and no comments fall
        // inside the block's source range. Multi-line `{\n}` is semantically
        // identical but stretches absolute byte offsets of every later
        // expression; that has surfaced a latent cross-module span-collision
        // bug in the type-checker → codegen `(start, end)` lookup tables
        // (see PR feat/actor-edges-phase-alpha-cow-envelopes / quic_service
        // smoke regression). Keeping empty blocks single-line also matches
        // common formatter conventions (rustfmt, gofmt) and is round-trip
        // stable.
        if block.stmts.is_empty() && block.trailing_expr.is_none() {
            let bytes = self.source.as_bytes();
            let from = self.prev_source_pos.min(bytes.len());
            // Some callers pass `self.source.len()` (no tighter bound), and
            // the parser has been observed to produce inverted spans for
            // empty function bodies (item.span.end < item.span.start). Be
            // defensive: clamp to source length and fall back to source end
            // when the supplied scope is degenerate.
            let to_raw = scope_end.min(bytes.len());
            let to = if to_raw > from { to_raw } else { bytes.len() };
            if from < to {
                // Locate this block's opening `{` and matching `}` in source.
                if let Some(open_idx) = self.find_open_brace(from, to) {
                    let close_idx = self.find_block_close(open_idx + 1, to);
                    // find_block_close returns `to` when no `}` was found in
                    // range; only collapse when we actually located the brace.
                    if close_idx < to {
                        // Check whether ANY comment falls inside the block's
                        // source range. We must scan forward from
                        // `next_comment` because some comments may have been
                        // logically classified earlier even though they sit
                        // inside this block in source (e.g. doc comments
                        // attached to earlier siblings can land here).
                        let mut comment_inside = false;
                        let mut i = self.next_comment;
                        while i < self.comments.len() {
                            let cs = self.comments[i].span.start;
                            if cs >= close_idx {
                                break;
                            }
                            if cs >= open_idx {
                                comment_inside = true;
                                break;
                            }
                            i += 1;
                        }
                        if !comment_inside {
                            self.write("{}");
                            self.prev_source_pos = close_idx + 1;
                            return;
                        }
                    }
                }
            }
        }
        self.write("{\n");
        self.indent += 1;
        if let Some(open) = open {
            self.prev_source_pos = open + 1;
        }
        for stmt in &block.stmts {
            self.flush_comments_before(stmt.1.start);
            self.format_stmt(&stmt.0);
            self.prev_source_pos = self.prev_source_pos.max(stmt.1.end);
        }
        if let Some(trailing) = &block.trailing_expr {
            self.flush_comments_before(trailing.1.start);
            self.write_indent();
            self.format_expr(trailing);
            self.newline();
            self.prev_source_pos = self.prev_source_pos.max(trailing.1.end);
        }
        self.flush_block_end_comments(scope_end);
        self.indent -= 1;
        self.write_indent();
        self.write("}");
    }

    fn format_gen_block(&mut self, body: &Block) {
        self.write("gen ");
        if self.can_format_gen_block_inline(body) {
            self.format_gen_block_inline(body);
        } else {
            self.format_block(body, self.source.len());
        }
    }

    fn can_format_gen_block_inline(&self, body: &Block) -> bool {
        let item_count = body.stmts.len() + usize::from(body.trailing_expr.is_some());
        item_count <= 2
            && !self.next_block_has_comments()
            && body
                .stmts
                .iter()
                .all(|(stmt, _)| Self::can_format_stmt_inline(stmt))
            && body
                .trailing_expr
                .as_deref()
                .is_none_or(|(expr, _)| Self::can_format_expr_inline(expr))
    }

    fn next_block_has_comments(&self) -> bool {
        if self.comments.is_empty() {
            return false;
        }
        let Some((open, close)) = self.next_block_bounds() else {
            return false;
        };
        self.comments[self.next_comment..]
            .iter()
            .any(|comment| comment.span.start > open && comment.span.start < close)
    }

    fn next_block_bounds(&self) -> Option<(usize, usize)> {
        let from = self.prev_source_pos.min(self.source.len());
        let open = self.find_open_brace(from, self.source.len())?;
        let close = self.find_block_close(open + 1, self.source.len());
        (close < self.source.len()).then_some((open, close))
    }

    fn can_format_stmt_inline(stmt: &Stmt) -> bool {
        match stmt {
            Stmt::Let { value, .. } | Stmt::Var { value, .. } => value
                .as_ref()
                .is_none_or(|(expr, _)| Self::can_format_expr_inline(expr)),
            Stmt::Assign { target, value, .. } => {
                Self::can_format_expr_inline(&target.0) && Self::can_format_expr_inline(&value.0)
            }
            Stmt::Break { value, .. } | Stmt::Return(value) => value
                .as_ref()
                .is_none_or(|(expr, _)| Self::can_format_expr_inline(expr)),
            Stmt::Continue { .. } => true,
            Stmt::Defer(expr) => Self::can_format_expr_inline(&expr.0),
            Stmt::Expression(expr) => Self::can_format_expr_inline(&expr.0),
            Stmt::If { .. }
            | Stmt::IfLet { .. }
            | Stmt::Match { .. }
            | Stmt::Loop { .. }
            | Stmt::For { .. }
            | Stmt::While { .. }
            | Stmt::WhileLet { .. } => false,
        }
    }

    #[expect(
        clippy::too_many_lines,
        reason = "the exhaustive expression-shape classifier stays readable as one match"
    )]
    fn can_format_expr_inline(expr: &Expr) -> bool {
        match expr {
            Expr::Literal(_)
            | Expr::Ident(_)
            | Expr::QualifiedAssoc(_)
            | Expr::RegexLiteral(_)
            | Expr::ByteStringLiteral(_)
            | Expr::ByteArrayLiteral(_)
            | Expr::Yield(None)
            | Expr::Return(None) => true,
            Expr::ContextVariant(context) => context.record.as_ref().is_none_or(|record| {
                record
                    .fields
                    .iter()
                    .all(|(_, expr)| Self::can_format_expr_inline(&expr.0))
                    && record
                        .base
                        .as_ref()
                        .is_none_or(|expr| Self::can_format_expr_inline(&expr.0))
            }),
            Expr::Tuple(exprs) => exprs
                .iter()
                .all(|(expr, _)| Self::can_format_expr_inline(expr)),
            Expr::Array(elements) => elements
                .iter()
                .all(|element| Self::can_format_expr_inline(&element.expr().0)),
            Expr::MapLiteral { entries } => entries.iter().all(|(key, value)| {
                Self::can_format_expr_inline(&key.0) && Self::can_format_expr_inline(&value.0)
            }),
            Expr::ArrayRepeat { value, count } => {
                Self::can_format_expr_inline(&value.0) && Self::can_format_expr_inline(&count.0)
            }
            Expr::Unary { operand, .. }
            | Expr::ReturnError(operand)
            | Expr::Clone(operand)
            | Expr::PostfixTry(operand)
            | Expr::Await(operand)
            | Expr::AwaitRestart(operand)
            | Expr::Yield(Some(operand))
            | Expr::Return(Some(operand)) => Self::can_format_expr_inline(&operand.0),
            Expr::Binary { left, right, .. }
            | Expr::Coalesce { left, right }
            | Expr::Handle {
                operand: left,
                body: right,
                ..
            }
            | Expr::Is {
                lhs: left,
                rhs: right,
            } => Self::can_format_expr_inline(&left.0) && Self::can_format_expr_inline(&right.0),
            Expr::Call { function, args, .. } => {
                Self::can_format_expr_inline(&function.0)
                    && args
                        .iter()
                        .all(|arg| Self::can_format_expr_inline(&arg.expr().0))
            }
            Expr::MethodCall { receiver, args, .. } => {
                Self::can_format_expr_inline(&receiver.0)
                    && args
                        .iter()
                        .all(|arg| Self::can_format_expr_inline(&arg.expr().0))
            }
            Expr::FieldAccess { object, .. } | Expr::GenericApplySuffix { target: object, .. } => {
                Self::can_format_expr_inline(&object.0)
            }
            Expr::Index { object, index } => {
                Self::can_format_expr_inline(&object.0) && Self::can_format_expr_inline(&index.0)
            }
            Expr::Cast { expr, .. } => Self::can_format_expr_inline(&expr.0),
            Expr::Range { start, end, .. } => {
                start
                    .as_ref()
                    .is_none_or(|expr| Self::can_format_expr_inline(&expr.0))
                    && end
                        .as_ref()
                        .is_none_or(|expr| Self::can_format_expr_inline(&expr.0))
            }
            Expr::StructInit { fields, base, .. } => {
                fields
                    .iter()
                    .all(|(_, expr)| Self::can_format_expr_inline(&expr.0))
                    && base
                        .as_ref()
                        .is_none_or(|expr| Self::can_format_expr_inline(&expr.0))
            }
            Expr::RecordInitSuffix {
                target,
                fields,
                base,
            } => {
                Self::can_format_expr_inline(&target.0)
                    && fields
                        .iter()
                        .all(|(_, expr)| Self::can_format_expr_inline(&expr.0))
                    && base
                        .as_ref()
                        .is_none_or(|expr| Self::can_format_expr_inline(&expr.0))
            }
            Expr::InterpolatedString(parts) => parts.iter().all(|part| match part {
                StringPart::Literal(_) => true,
                StringPart::Expr((expr, _)) | StringPart::StructuralExpr((expr, _)) => {
                    Self::can_format_expr_inline(expr)
                }
            }),
            Expr::Block(_)
            | Expr::If { .. }
            | Expr::IfLet { .. }
            | Expr::Match { .. }
            | Expr::Lambda { .. }
            | Expr::Spawn { .. }
            | Expr::SpawnLambdaActor { .. }
            | Expr::Scope { .. }
            | Expr::ForkChild { .. }
            | Expr::ForkBlock { .. }
            | Expr::ScopeDeadline { .. }
            | Expr::Select { .. }
            | Expr::Race(_)
            | Expr::UnsafeBlock(_)
            | Expr::MachineEmit { .. }
            | Expr::GenBlock { .. } => false,
        }
    }

    fn format_gen_block_inline(&mut self, body: &Block) {
        self.write("{");
        if !body.stmts.is_empty() || body.trailing_expr.is_some() {
            self.write(" ");
            for (stmt, _) in &body.stmts {
                self.format_stmt_inline(stmt);
                self.write(" ");
            }
            if let Some(expr) = body.trailing_expr.as_deref() {
                self.format_expr(expr);
                self.write(" ");
            }
        }
        self.write("}");
    }

    fn format_stmt_inline(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Let {
                pattern,
                ty,
                value,
                else_block,
            } => {
                self.write("let ");
                self.format_pattern(pattern);
                if let Some(ty) = ty {
                    self.write(": ");
                    self.format_type_expr(&ty.0);
                }
                if let Some(expr) = value {
                    self.write(" = ");
                    self.format_expr(expr);
                }
                if let Some(else_block) = else_block {
                    self.write_else();
                    self.format_block(else_block, self.source.len());
                }
                self.write(";");
            }
            Stmt::Var { name, ty, value } => {
                self.write("var ");
                self.write_ident(*name);
                if let Some(ty) = ty {
                    self.write(": ");
                    self.format_type_expr(&ty.0);
                }
                if let Some(expr) = value {
                    self.write(" = ");
                    self.format_expr(expr);
                }
                self.write(";");
            }
            Stmt::Assign { target, op, value } => {
                self.format_expr(target);
                if let Some(op) = op {
                    self.write(" ");
                    self.write(compound_assign_op_str(*op));
                    self.write(" ");
                } else {
                    self.write(" = ");
                }
                self.format_expr(value);
                self.write(";");
            }
            Stmt::Break { label, value } => {
                self.write("break");
                if let Some(label) = label {
                    self.write(" @");
                    self.write_ident(*label);
                }
                if let Some(expr) = value {
                    self.write(" ");
                    self.format_expr(expr);
                }
                self.write(";");
            }
            Stmt::Continue { label } => {
                self.write("continue");
                if let Some(label) = label {
                    self.write(" @");
                    self.write_ident(*label);
                }
                self.write(";");
            }
            Stmt::Return(value) => {
                self.write("return");
                if let Some(expr) = value {
                    self.write(" ");
                    self.format_expr(expr);
                }
                self.write(";");
            }
            Stmt::Defer(expr) => {
                self.write("defer ");
                self.format_expr(expr);
                self.write(";");
            }
            Stmt::Expression(expr) => {
                self.format_expr(expr);
                self.write(";");
            }
            Stmt::If { .. }
            | Stmt::IfLet { .. }
            | Stmt::Match { .. }
            | Stmt::Loop { .. }
            | Stmt::For { .. }
            | Stmt::While { .. }
            | Stmt::WhileLet { .. } => {
                unreachable!("non-inline statement reached gen block inline formatter")
            }
        }
    }

    #[expect(clippy::too_many_lines, reason = "match on all Stmt variants")]
    fn format_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Let {
                pattern,
                ty,
                value,
                else_block,
            } => {
                self.write_indent();
                self.write("let ");
                self.format_pattern(pattern);
                if let Some(ty) = ty {
                    self.write(": ");
                    self.format_type_expr(&ty.0);
                }
                if let Some(val) = value {
                    self.write(" = ");
                    self.format_expr(val);
                }
                if let Some(else_block) = else_block {
                    self.write_else();
                    self.format_block(else_block, self.source.len());
                }
                self.write_token(";\n", |t| matches!(t, hew_lexer::Token::Semicolon));
            }
            Stmt::Var { name, ty, value } => {
                self.write_indent();
                self.write("var ");
                self.write_ident(*name);
                if let Some(ty) = ty {
                    self.write(": ");
                    self.format_type_expr(&ty.0);
                }
                if let Some(val) = value {
                    self.write(" = ");
                    self.format_expr(val);
                }
                self.write_token(";\n", |t| matches!(t, hew_lexer::Token::Semicolon));
            }
            Stmt::Assign { target, op, value } => {
                self.write_indent();
                self.format_expr(target);
                if let Some(op) = op {
                    self.write(" ");
                    self.write(compound_assign_op_str(*op));
                    self.write(" ");
                } else {
                    self.write(" = ");
                }
                self.format_expr(value);
                self.write_token(";\n", |t| matches!(t, hew_lexer::Token::Semicolon));
            }
            Stmt::If {
                condition,
                then_block,
                else_block,
            } => {
                self.write_indent();
                self.write("if ");
                self.format_cond_expr(condition);
                self.write(" ");
                self.format_block(then_block, self.source.len());
                if let Some(eb) = else_block {
                    self.format_else_block(eb);
                }
                self.newline();
            }
            Stmt::IfLet {
                conditions,
                body,
                else_body,
            } => {
                self.write_indent();
                self.write("if ");
                self.format_condition(conditions);
                self.write(" ");
                self.format_block(body, self.source.len());
                if let Some(else_block) = else_body {
                    self.write_else();
                    self.format_expr(else_block);
                }
                self.newline();
            }
            Stmt::Match { scrutinee, arms } => {
                self.write_indent();
                self.write("match ");
                self.format_cond_expr(scrutinee);
                self.write(" {\n");
                self.indent += 1;
                // advance past the opening `{` so the blank-line heuristic in
                // flush_comments_before counts only lines within the block;
                // search from the scrutinee's end to the first arm's pattern start
                if self.has_comments() {
                    let search_from = scrutinee.1.end.min(self.source.len());
                    let search_to = arms
                        .first()
                        .map_or(self.source.len(), |a| a.pattern.1.start);
                    if let Some(open) = self.find_open_brace(search_from, search_to) {
                        self.prev_source_pos = open + 1;
                    }
                }
                for arm in arms {
                    self.flush_comments_before(arm.pattern.1.start);
                    self.format_match_arm(arm);
                    self.prev_source_pos = self.prev_source_pos.max(arm.body.1.end);
                }
                if self.has_comments() {
                    let close = self.find_block_close(self.prev_source_pos, self.source.len());
                    self.flush_comments_before(close);
                    if close < self.source.len() {
                        self.prev_source_pos = close;
                    }
                }
                self.indent -= 1;
                self.write_indent();
                self.write("}\n");
            }
            Stmt::Loop { label, body } => {
                self.write_indent();
                if let Some(label) = label {
                    self.write("@");
                    self.write_ident(*label);
                    self.write(": ");
                }
                self.write("loop ");
                self.format_block(body, self.source.len());
                self.newline();
            }
            Stmt::For {
                label,
                pattern,
                iterable,
                body,
            } => {
                self.write_indent();
                if let Some(label) = label {
                    self.write("@");
                    self.write_ident(*label);
                    self.write(": ");
                }
                self.write("for ");
                self.format_pattern(pattern);
                self.write(" in ");
                self.format_expr(iterable);
                self.write(" ");
                self.format_block(body, self.source.len());
                self.newline();
            }
            Stmt::While {
                label,
                condition,
                body,
            } => {
                self.write_indent();
                if let Some(label) = label {
                    self.write("@");
                    self.write_ident(*label);
                    self.write(": ");
                }
                self.write("while ");
                self.format_cond_expr(condition);
                self.write(" ");
                self.format_block(body, self.source.len());
                self.newline();
            }
            Stmt::WhileLet {
                label,
                conditions,
                body,
            } => {
                self.write_indent();
                if let Some(label) = label {
                    self.write("@");
                    self.write_ident(*label);
                    self.write(": ");
                }
                self.write("while ");
                self.format_condition(conditions);
                self.write(" ");
                self.format_block(body, self.source.len());
                self.newline();
            }
            Stmt::Break { label, value } => {
                self.write_indent();
                self.write("break");
                if let Some(label) = label {
                    self.write(" @");
                    self.write_ident(*label);
                }
                if let Some(val) = value {
                    self.write(" ");
                    self.format_expr(val);
                }
                self.write_token(";\n", |t| matches!(t, hew_lexer::Token::Semicolon));
            }
            Stmt::Continue { label } => {
                self.write_indent();
                self.write("continue");
                if let Some(label) = label {
                    self.write(" @");
                    self.write_ident(*label);
                }
                self.write_token(";\n", |t| matches!(t, hew_lexer::Token::Semicolon));
            }
            Stmt::Return(val) => {
                self.write_indent();
                self.write("return");
                if let Some(val) = val {
                    self.write(" ");
                    self.format_expr(val);
                }
                self.write_token(";\n", |t| matches!(t, hew_lexer::Token::Semicolon));
            }
            Stmt::Expression(expr) => {
                self.write_indent();
                self.format_expr(expr);
                self.write_token(";\n", |t| matches!(t, hew_lexer::Token::Semicolon));
            }
            Stmt::Defer(expr) => {
                self.write_indent();
                self.write("defer ");
                self.format_expr(expr);
                // Block expressions already end with `}`, no semicolon.
                if matches!(expr.0, Expr::Block(_)) {
                    self.write("\n");
                } else {
                    self.write_token(";\n", |t| matches!(t, hew_lexer::Token::Semicolon));
                }
            }
        }
    }

    fn format_else_block(&mut self, eb: &ElseBlock) {
        if eb.is_if {
            if let Some(if_stmt) = &eb.if_stmt {
                self.write_else();
                // Print the inner `if` without leading indent (it's on the same line).
                // Only `Stmt::If` and `Stmt::IfLet` are valid here by parser construction
                // (see parser.rs: `else if`/`else if let` branches). Every other `Stmt`
                // variant is enumerated explicitly so that adding a new control-flow
                // statement forces a design decision at this dispatch site instead of
                // silently falling through to `format_stmt` and re-indenting.
                match &if_stmt.0 {
                    Stmt::If {
                        condition,
                        then_block,
                        else_block,
                    } => {
                        self.write("if ");
                        self.format_expr(condition);
                        self.write(" ");
                        self.format_block(then_block, self.source.len());
                        if let Some(eb) = else_block {
                            self.format_else_block(eb);
                        }
                    }
                    Stmt::IfLet {
                        conditions,
                        body,
                        else_body,
                    } => {
                        self.write("if ");
                        self.format_condition(conditions);
                        self.write(" ");
                        self.format_block(body, self.source.len());
                        if let Some(else_block) = else_body {
                            self.write_else();
                            self.format_expr(else_block);
                        }
                    }
                    Stmt::Let { .. }
                    | Stmt::Var { .. }
                    | Stmt::Assign { .. }
                    | Stmt::Match { .. }
                    | Stmt::Loop { .. }
                    | Stmt::For { .. }
                    | Stmt::While { .. }
                    | Stmt::WhileLet { .. }
                    | Stmt::Break { .. }
                    | Stmt::Continue { .. }
                    | Stmt::Return(_)
                    | Stmt::Defer(_)
                    | Stmt::Expression(_) => {
                        // Parser invariant: `else if`/`else if let` are the only shapes
                        // that populate `ElseBlock::if_stmt`. Reaching this arm means the
                        // AST was hand-built or corrupted; fall back to `format_stmt` so
                        // we still produce valid source, and log via `debug_assert!` so
                        // tests surface the invariant break.
                        debug_assert!(
                            false,
                            "format_else_block: non-if stmt in else-if position: {:?}",
                            &if_stmt.0
                        );
                        self.format_stmt(&if_stmt.0);
                    }
                }
            }
        } else if let Some(block) = &eb.block {
            self.write_else();
            self.format_block(block, self.source.len());
        }
    }

    fn format_match_arm(&mut self, arm: &MatchArm) {
        self.write_indent();
        self.format_pattern(&arm.pattern);
        if let Some(guard) = &arm.guard {
            self.write(" if ");
            self.format_expr(guard);
        }
        self.write_token(" => ", |t| matches!(t, hew_lexer::Token::FatArrow));
        self.format_expr(&arm.body);
        self.write(",");
        self.newline();
    }

    // ------------------------------------------------------------------
    // Expressions
    // ------------------------------------------------------------------

    /// Return `true` when `expr` must be parenthesised before a postfix operator
    /// (`.field`, `.method()`, `[index]`, `?`).
    ///
    /// Postfix operators bind at the highest precedence in the Pratt loop.  Any
    /// expression at a strictly lower precedence level — binary ops, prefix unary
    /// ops, range, `is`, `await`, `clone` — must be wrapped in parens so that
    /// re-parsing produces the same AST.  Delimited forms (literals, identifiers,
    /// tuples, arrays, blocks, calls, other postfix) are already unambiguous.
    ///
    /// A `StructInit` receiver also needs parens: in `if`/`while` condition or
    /// `match` scrutinee position a bare struct literal is suppressed (the `{`
    /// opens the block), so `(Foo { a: 1 }).b` must keep its parens to re-parse
    /// as a field access. Emitting them unconditionally is always correct — in
    /// non-condition position the parens are merely redundant, not wrong.
    fn needs_receiver_parens(expr: &Expr) -> bool {
        matches!(
            expr,
            Expr::Binary { .. }
                | Expr::Coalesce { .. }
                | Expr::Handle { .. }
                | Expr::ReturnError(_)
                | Expr::Unary { .. }
                | Expr::Clone(_)
                | Expr::Range { .. }
                | Expr::Is { .. }
                | Expr::Await(_)
                | Expr::AwaitRestart(_)
                | Expr::StructInit { .. }
        )
    }

    /// Format an expression that appears as the object/receiver of a postfix
    /// operation (`.`, `[]`, `?`), adding parentheses when required for correct
    /// re-parsing.
    fn format_receiver(&mut self, expr: &Spanned<Expr>) {
        if Self::needs_receiver_parens(&expr.0) && self.chooses_parens(&expr.1) {
            self.write("(");
            self.format_expr(expr);
            self.write(")");
        } else {
            self.format_expr(expr);
        }
    }

    /// Format an `if`/`while` condition or `match` scrutinee, wrapping a direct
    /// struct literal in parens so it re-parses correctly.
    ///
    /// In condition/scrutinee position the parser suppresses bare struct
    /// literals (the `{` opens the block / loop body / match arms), so a struct
    /// literal that is the direct condition must keep its parens: `if (Foo {
    /// a: 1 }) {}` and `match (Foo { a: 1 }) { … }` would otherwise format to
    /// `if Foo { a: 1 } {}` / `match Foo { a: 1 } { … }`, which re-parse with
    /// the struct body swallowing the block. Only a direct `StructInit` needs
    /// this — every other condition form (binary ops, calls, blocks, nested
    /// `if`/`match`) re-parses unchanged.
    /// Print an `if` / `while` condition (§12.5) as written: `&&`-joined
    /// operands, each either `let PATTERN = expr` or a boolean expression.
    fn format_condition(&mut self, conditions: &[ConditionItem]) {
        for (index, item) in conditions.iter().enumerate() {
            if index > 0 {
                self.write(" && ");
            }
            match item {
                ConditionItem::Let { pattern, expr } => {
                    self.write("let ");
                    self.format_pattern(pattern);
                    self.write(" = ");
                    self.format_expr(expr);
                }
                ConditionItem::Expr(expr) => self.format_cond_expr(expr),
            }
        }
    }

    fn format_cond_expr(&mut self, expr: &Spanned<Expr>) {
        if matches!(expr.0, Expr::StructInit { .. }) && self.chooses_parens(&expr.1) {
            self.write("(");
            self.format_expr(expr);
            self.write(")");
        } else {
            self.format_expr(expr);
        }
    }

    /// Format an expression with precedence tracking for correct parenthesization.
    ///
    /// `parent_prec` is the precedence of the enclosing binary operator (0 at top level).
    /// `is_right` indicates this expression is the right operand of its parent binary op.
    fn format_expr_prec(&mut self, expr: &Spanned<Expr>, parent_prec: u8, is_right: bool) {
        if self.source_parenthesizes(&expr.1) {
            self.format_expr(expr);
        } else {
            self.format_expr_prec_bare(expr, parent_prec, is_right);
        }
    }

    /// [`Self::format_expr_prec`] for an expression the source does not
    /// parenthesize.
    fn format_expr_prec_bare(&mut self, expr: &Spanned<Expr>, parent_prec: u8, is_right: bool) {
        if let Expr::Binary { left, op, right } = &expr.0 {
            let prec = binop_precedence(*op);
            // Need parens when:
            // 1. Our precedence is lower than the parent (tighter parent binds first)
            // 2. Same precedence AND we're on the right side (handles non-associative like a-b-c)
            let needs_parens = (prec < parent_prec || (prec == parent_prec && is_right))
                && self.chooses_parens(&expr.1);
            if needs_parens {
                self.write("(");
            }
            self.format_expr_prec(left, prec, false);
            self.write(" ");
            self.write(binary_op_str(*op));
            self.write(" ");
            self.format_expr_prec(right, prec, true);
            if needs_parens {
                self.write(")");
            }
        } else if let Expr::Is { lhs, rhs } = &expr.0 {
            let prec = binop_precedence(BinaryOp::Equal);
            let needs_parens = (prec < parent_prec || (prec == parent_prec && is_right))
                && self.chooses_parens(&expr.1);
            if needs_parens {
                self.write("(");
            }
            self.format_expr_prec(lhs, prec, false);
            self.write(" is ");
            self.format_expr_prec(rhs, prec, true);
            if needs_parens {
                self.write(")");
            }
        } else {
            let needs_parens = matches!(
                expr.0,
                Expr::Coalesce { .. } | Expr::Handle { .. } | Expr::ReturnError(_)
            ) && parent_prec > 0
                && self.chooses_parens(&expr.1);
            if needs_parens {
                self.write("(");
            }
            self.format_expr(expr);
            if needs_parens {
                self.write(")");
            }
        }
    }

    fn format_expr(&mut self, expr: &Spanned<Expr>) {
        let parens = self.source_parens(&expr.1);
        for &(token, open, _) in &parens {
            self.printed_parens.insert(token);
            self.flush_inline_comments(open);
            self.write("(");
        }
        self.format_expr_unparenthesized(expr);
        for &(_, _, close) in parens.iter().rev() {
            self.flush_inline_comments(close);
            self.write(")");
            self.prev_source_pos = self.prev_source_pos.max(close + 1);
        }
        if expr.1.start < expr.1.end {
            self.prev_source_pos = self.prev_source_pos.max(expr.1.end);
        }
    }

    #[expect(clippy::too_many_lines, reason = "match on all Expr variants")]
    fn format_expr_unparenthesized(&mut self, expr: &Spanned<Expr>) {
        // A block flushes its own comments statement by statement, and its
        // span can start inside the braces.
        if !matches!(expr.0, Expr::Block(_)) {
            self.flush_inline_comments(expr.1.start);
        }
        match &expr.0 {
            Expr::Binary { left, op, right } => {
                let prec = binop_precedence(*op);
                self.format_expr_prec(left, prec, false);
                self.write(" ");
                self.write(binary_op_str(*op));
                self.write(" ");
                self.format_expr_prec(right, prec, true);
            }
            Expr::Unary { op, operand } => {
                match op {
                    UnaryOp::Not => self.write("!"),
                    UnaryOp::Negate => self.write("-"),
                    UnaryOp::BitNot => self.write("~"),
                    UnaryOp::RawDeref => self.write("*"),
                }
                let needs_parens = matches!(
                    operand.0,
                    Expr::Binary { .. }
                        | Expr::Is { .. }
                        | Expr::Coalesce { .. }
                        | Expr::Handle { .. }
                        | Expr::ReturnError(_)
                ) && self.chooses_parens(&operand.1);
                if needs_parens {
                    self.write("(");
                }
                self.format_expr(operand);
                if needs_parens {
                    self.write(")");
                }
            }
            Expr::Literal(lit) => self.format_literal(lit, &expr.1),
            Expr::Ident(name) => self.write_ident(*name),
            Expr::ContextVariant(context) => {
                self.write(".");
                self.write_ident(context.name);
                if let Some(record) = &context.record {
                    self.format_record_literal_body(&record.fields, record.base.as_deref(), None);
                }
            }
            Expr::GenericApplySuffix { target, type_args } => {
                if matches!(
                    &target.0,
                    Expr::Ident(_)
                        | Expr::FieldAccess { .. }
                        | Expr::QualifiedAssoc(_)
                        | Expr::GenericApplySuffix { .. }
                ) {
                    self.format_expr(target);
                } else {
                    self.format_receiver(target);
                }
                self.write("<");
                self.comma_sep(type_args, |f, ty| f.format_type_expr(&ty.0));
                self.write(">");
            }
            Expr::RecordInitSuffix {
                target,
                fields,
                base,
            } => {
                self.format_receiver(target);
                self.format_record_literal_body(
                    fields,
                    base.as_deref(),
                    self.trailing_list(&expr.1, "}"),
                );
            }
            Expr::QualifiedAssoc(assoc) => {
                self.write("<");
                self.format_type_expr(&assoc.base.0);
                self.write(" as ");
                self.format_path(&assoc.trait_path);
                self.write(">");
                for member in &assoc.members {
                    self.write(".");
                    self.write_ident(*member);
                }
            }
            Expr::Clone(operand) => {
                self.write("clone ");
                self.format_expr(operand);
            }
            Expr::Tuple(elems) => {
                let bounds = self.trailing_list(&expr.1, ")");
                self.delimited_list("(", ")", elems, bounds, false, true, Formatter::format_expr);
            }
            Expr::Array(elements) => {
                let bounds = self.trailing_list(&expr.1, "]");
                self.delimited_list("[", "]", elements, bounds, false, true, |f, element| {
                    if element.is_spread() {
                        f.write("..");
                    }
                    f.format_expr(element.expr());
                });
            }
            Expr::ArrayRepeat { value, count } => {
                self.write("[");
                self.format_expr(value);
                self.write("; ");
                self.format_expr(count);
                self.write("]");
            }
            Expr::Block(block) if self.is_bare_control_flow(&expr.1) => {
                // `=> break` desugars to a one-statement block; the source
                // wrote the keyword alone, so print it that way.
                match block.stmts.first().map(|(stmt, _)| stmt) {
                    Some(Stmt::Break { label, .. }) => {
                        self.write("break");
                        if let Some(label) = label {
                            self.write(" @");
                            self.write_ident(*label);
                        }
                    }
                    Some(Stmt::Continue { label }) => {
                        self.write("continue");
                        if let Some(label) = label {
                            self.write(" @");
                            self.write_ident(*label);
                        }
                    }
                    _ => self.format_block(block, self.source.len()),
                }
            }
            Expr::Block(block) => {
                self.format_block(block, self.source.len());
            }
            Expr::If {
                condition,
                then_block,
                else_block,
            } => {
                self.write("if ");
                self.format_cond_expr(condition);
                self.write(" ");
                self.format_expr(then_block);
                if let Some(eb) = else_block {
                    self.write_else();
                    self.format_expr(eb);
                }
            }
            Expr::IfLet {
                conditions,
                body,
                else_body,
            } => {
                self.write("if ");
                self.format_condition(conditions);
                self.write(" ");
                self.format_block(body, self.source.len());
                if let Some(else_block) = else_body {
                    self.write_else();
                    self.format_expr(else_block);
                }
            }
            Expr::Match { scrutinee, arms } => {
                self.write("match ");
                self.format_cond_expr(scrutinee);
                self.write(" {\n");
                self.indent += 1;
                // advance past the opening `{` so the blank-line heuristic in
                // flush_comments_before counts only lines within the block;
                // search from the scrutinee's end to the first arm's pattern start
                if self.has_comments() {
                    let search_from = scrutinee.1.end.min(self.source.len());
                    let search_to = arms
                        .first()
                        .map_or(self.source.len(), |a| a.pattern.1.start);
                    if let Some(open) = self.find_open_brace(search_from, search_to) {
                        self.prev_source_pos = open + 1;
                    }
                }
                for arm in arms {
                    self.flush_comments_before(arm.pattern.1.start);
                    self.format_match_arm(arm);
                    self.prev_source_pos = self.prev_source_pos.max(arm.body.1.end);
                }
                if self.has_comments() {
                    let close = self.find_block_close(self.prev_source_pos, self.source.len());
                    self.flush_comments_before(close);
                    if close < self.source.len() {
                        self.prev_source_pos = close;
                    }
                }
                self.indent -= 1;
                self.write_indent();
                self.write("}");
            }
            Expr::Lambda {
                is_move,
                private_captures,
                type_params,
                params,
                return_type,
                body,
            } => {
                if *is_move {
                    self.write("move ");
                }
                if !private_captures.is_empty() {
                    self.write("capture(");
                    self.comma_sep(private_captures, |f, (name, _)| {
                        f.write("var ");
                        f.write_ident(*name);
                    });
                    self.write(") ");
                }
                if type_params.is_some() {
                    self.format_opt_type_params(type_params.as_ref());
                    self.write("(");
                    self.format_lambda_params(params);
                    self.write(")");
                    if let Some(ret) = return_type {
                        self.write(" -> ");
                        self.format_type_expr(&ret.0);
                    }
                    self.write_token(" => ", |t| matches!(t, hew_lexer::Token::FatArrow));
                    self.format_expr(body);
                } else {
                    self.write("|");
                    self.format_lambda_params(params);
                    self.write("|");
                    if let Some(ret) = return_type {
                        self.write(" -> ");
                        self.format_type_expr(&ret.0);
                        self.write(" ");
                        if matches!(body.0, Expr::Block(_)) {
                            self.format_expr(body);
                        } else {
                            self.write("{ ");
                            self.format_expr(body);
                            self.write(" }");
                        }
                    } else {
                        self.write(" ");
                        self.format_expr(body);
                    }
                }
            }
            Expr::Spawn {
                target,
                type_args,
                args,
            } => {
                self.write("spawn ");
                self.format_expr(target);
                if !type_args.is_empty() {
                    self.write("<");
                    self.comma_sep(type_args, |f, (te, _)| {
                        f.format_type_expr(te);
                    });
                    self.write(">");
                }
                // `spawn Worker()` and `spawn Worker` are the same spawn;
                // keep the empty argument list when the author wrote one.
                if !args.is_empty() || self.span_ends_with_paren(&expr.1) {
                    self.write("(");
                    self.comma_sep(args, |f, (name, value)| {
                        f.write_ident(*name);
                        f.write(": ");
                        f.format_expr(value);
                    });
                    self.flush_inline_comments(expr.1.end);
                    self.write(")");
                }
            }
            Expr::SpawnLambdaActor {
                is_move,
                params,
                return_type,
                body,
            } => {
                self.write("actor ");
                if *is_move {
                    self.write("move ");
                }
                self.write("|");
                self.format_lambda_params(params);
                self.write("|");
                if let Some(ret) = return_type {
                    self.write(" -> ");
                    self.format_type_expr(&ret.0);
                }
                self.write(" ");
                self.format_expr(body);
            }
            Expr::Scope { body } => {
                self.write("scope ");
                self.format_block(body, self.source.len());
            }
            Expr::ForkChild { expr } => {
                self.write("fork ");
                self.format_expr(expr);
            }
            Expr::ForkBlock { body } => {
                self.write("fork ");
                self.format_block(body, self.source.len());
            }
            Expr::ScopeDeadline { duration, body } => {
                self.write("scope within ");
                self.format_expr(duration);
                self.write(" ");
                self.format_block(body, self.source.len());
            }
            Expr::InterpolatedString(parts) => {
                // The source spelling keeps escapes and interpolation layout.
                if let Some(spelling) = self.literal_spelling(&expr.1) {
                    self.write(&spelling);
                    return;
                }
                self.write("f\"");
                for part in parts {
                    match part {
                        StringPart::Literal(s) => {
                            self.write(&escape_fstring_literal(s));
                        }
                        StringPart::Expr(expr) => {
                            self.write("{");
                            self.interpolation_depth += 1;
                            self.format_expr(expr);
                            self.interpolation_depth -= 1;
                            self.write("}");
                        }
                        StringPart::StructuralExpr(expr) => {
                            self.write("{");
                            self.interpolation_depth += 1;
                            self.format_expr(expr);
                            self.interpolation_depth -= 1;
                            self.write(":?}");
                        }
                    }
                }
                self.write("\"");
            }
            Expr::Call {
                function,
                type_args,
                args,
                ..
            } => {
                // Add parens around a FieldAccess callee so that `(rec.f)(args)` — a
                // function call through a fn-typed field — formats as `(rec.f)(args)` and
                // re-parses as `Call { FieldAccess }`, not as a `MethodCall`. The two forms
                // are syntactically distinct (different AST nodes, different checker paths);
                // normalising them would break the round-trip property.
                let needs_callee_parens = (matches!(function.0, Expr::Lambda { .. })
                    || (type_args.is_none() && matches!(function.0, Expr::FieldAccess { .. })))
                    && self.chooses_parens(&function.1);
                if needs_callee_parens {
                    self.write("(");
                }
                self.format_expr(function);
                if needs_callee_parens {
                    self.write(")");
                }
                if let Some(type_args) = type_args {
                    self.write("<");
                    self.comma_sep(type_args, |f, ta| f.format_type_expr(&ta.0));
                    self.write(">");
                }
                self.own_call_parens(&expr.1);
                let bounds = self.trailing_list(&expr.1, ")");
                self.format_call_args(args, bounds);
            }
            Expr::MethodCall {
                receiver,
                method,
                args,
            } => {
                self.format_receiver(receiver);
                self.flush_comments_before_token_after(receiver.1.end);
                self.write(".");
                self.write_ident(method.0);
                self.own_call_parens(&expr.1);
                let bounds = self.trailing_list(&expr.1, ")");
                self.format_call_args(args, bounds);
            }
            Expr::StructInit {
                path,
                fields,
                type_args,
                base,
            } => {
                self.format_path(path);
                if let Some(type_args) = type_args {
                    self.write("<");
                    self.comma_sep(type_args, |f, ta| f.format_type_expr(&ta.0));
                    self.write(">");
                }
                self.format_record_literal_body(
                    fields,
                    base.as_deref(),
                    self.trailing_list(&expr.1, "}"),
                );
            }
            Expr::Select { arms, timeout } => {
                self.write("select {\n");
                self.indent += 1;
                for arm in arms {
                    self.format_select_arm(arm);
                }
                if let Some(t) = timeout {
                    self.format_timeout(t);
                }
                self.indent -= 1;
                self.write_indent();
                self.write("}");
            }
            Expr::Race(exprs) => {
                self.write("race {\n");
                self.indent += 1;
                for e in exprs {
                    self.write_indent();
                    self.format_expr(e);
                    self.write(",\n");
                }
                self.indent -= 1;
                self.write_indent();
                self.write("}");
            }
            Expr::UnsafeBlock(block) => {
                self.write("unsafe ");
                self.format_block(block, self.source.len());
            }
            Expr::Yield(val) => {
                self.write("yield");
                if let Some(val) = val {
                    self.write(" ");
                    self.format_expr(val);
                }
            }
            Expr::Return(val) => {
                self.write("return");
                if let Some(val) = val {
                    self.write(" ");
                    self.format_expr(val);
                }
            }
            Expr::ReturnError(value) => {
                self.write("return error ");
                self.format_expr(value);
            }
            Expr::FieldAccess { object, field } => {
                // Consecutive numeric fields otherwise merge into a float
                // token: `(outer.0).1` must not become `outer.0.1`.
                let numeric_receiver = match &object.0 {
                    Expr::FieldAccess { field, .. } => field
                        .0
                        .name
                        .as_str()
                        .starts_with(|c: char| c.is_ascii_digit()),
                    Expr::Literal(Literal::Integer { .. }) => true,
                    _ => false,
                };
                if numeric_receiver
                    && field
                        .0
                        .name
                        .as_str()
                        .starts_with(|c: char| c.is_ascii_digit())
                    && self.chooses_parens(&object.1)
                {
                    self.write("(");
                    self.format_expr(object);
                    self.write(")");
                } else {
                    self.format_receiver(object);
                }
                self.flush_comments_before_token_after(object.1.end);
                self.write(".");
                self.write_ident(field.0);
            }
            Expr::Index { object, index } => {
                self.format_receiver(object);
                self.write("[");
                self.format_expr(index);
                self.write("]");
            }
            Expr::Cast { expr, ty } => {
                self.format_receiver(expr);
                self.write(" as ");
                self.format_type_expr(&ty.0);
            }
            Expr::PostfixTry(expr) => {
                self.format_receiver(expr);
                self.write("?");
            }
            Expr::Coalesce { left, right } => {
                self.format_expr_prec(left, 1, false);
                self.write(" ?? ");
                if matches!(right.0, Expr::Handle { .. }) {
                    self.write("(");
                    self.format_expr(right);
                    self.write(")");
                } else {
                    self.format_expr(right);
                }
            }
            Expr::Handle {
                operand,
                error,
                body,
            } => {
                self.format_expr_prec(operand, 1, false);
                self.write(" handle ");
                self.write_ident(error.0);
                self.write(" ");
                self.format_expr(body);
            }
            Expr::Range {
                start,
                end,
                inclusive,
            } => {
                if let Some(s) = start {
                    self.format_expr(s);
                }
                if *inclusive {
                    self.write("..=");
                } else {
                    self.write("..");
                }
                if let Some(e) = end {
                    self.format_expr(e);
                }
            }
            Expr::Await(inner) => {
                self.write("await ");
                self.format_expr_prec(inner, 25, false);
            }
            Expr::AwaitRestart(inner) => {
                self.write("await_restart ");
                self.format_expr(inner);
            }
            Expr::RegexLiteral(_) | Expr::ByteStringLiteral(_)
                if self.literal_spelling(&expr.1).is_some() =>
            {
                let spelling = self.literal_spelling(&expr.1).unwrap_or_default();
                self.write(&spelling);
            }
            Expr::RegexLiteral(pattern) => {
                self.write("re\"");
                self.write(&escape_regex_pattern(pattern));
                self.write("\"");
            }
            Expr::ByteStringLiteral(data) => {
                self.write("b\"");
                self.write(&escape_byte_string(data));
                self.write("\"");
            }
            Expr::ByteArrayLiteral(data) => {
                // Each element prints as the source spelled it (`4`, `0x04`).
                let spelled = self.byte_array_spellings(&expr.1, data.len());
                self.write("bytes [");
                for (i, &b) in data.iter().enumerate() {
                    if i > 0 {
                        self.write(", ");
                    }
                    match &spelled {
                        Some(elements) => self.write(&elements[i]),
                        None => self.write(&format!("0x{b:02x}")),
                    }
                }
                self.write("]");
            }
            Expr::MapLiteral { entries } => {
                self.write("{");
                self.comma_sep(entries, |f, (key, value)| {
                    f.format_expr(key);
                    f.write(": ");
                    f.format_expr(value);
                });
                self.write("}");
            }
            Expr::Is { .. } => self.format_expr_prec_bare(expr, 0, false),
            Expr::MachineEmit { event_name, fields } => {
                self.write("emit ");
                self.write_ident(*event_name);
                if fields.is_empty() {
                    self.write(" {}");
                } else {
                    self.write(" { ");
                    for (i, (name, val)) in fields.iter().enumerate() {
                        if i > 0 {
                            self.write(", ");
                        }
                        self.write_ident(*name);
                        self.write(": ");
                        self.format_expr(val);
                    }
                    self.write(" }");
                }
            }
            Expr::GenBlock { body } => {
                self.format_gen_block(body);
            }
        }
    }

    fn format_select_arm(&mut self, arm: &SelectArm) {
        self.flush_comments_before(arm.binding.1.start);
        self.write_indent();
        self.format_pattern(&arm.binding);
        self.write(" from ");
        self.format_expr(&arm.source);
        self.write_token(" => ", |t| matches!(t, hew_lexer::Token::FatArrow));
        self.format_expr(&arm.body);
        self.write(",");
        self.newline();
    }

    fn format_timeout(&mut self, tc: &TimeoutClause) {
        if let Some((hew_lexer::Token::After, span)) = self
            .tokens
            .get(self.token_at_or_after(self.prev_source_pos))
        {
            let start = span.start;
            self.flush_comments_before(start);
        }
        self.write_indent();
        self.write("after ");
        self.format_expr(&tc.duration);
        self.write_token(" => ", |t| matches!(t, hew_lexer::Token::FatArrow));
        self.format_expr(&tc.body);
        self.write(",");
        self.newline();
    }

    /// Write a call's parenthesized arguments; `bounds` are the source's
    /// `(` and `)` tokens.
    fn format_call_args(&mut self, args: &[CallArg], bounds: Option<(usize, usize)>) {
        self.delimited_list("(", ")", args, bounds, false, true, |f, arg| match arg {
            CallArg::Named { name, value } => {
                f.write_ident(*name);
                f.write(": ");
                f.format_expr(value);
            }
            CallArg::Positional(e) => f.format_expr(e),
        });
    }

    fn format_lambda_params(&mut self, params: &[LambdaParam]) {
        self.comma_sep(params, |f, p| {
            f.write_ident(p.name);
            if let Some(ty) = &p.ty {
                f.write(": ");
                f.format_type_expr(&ty.0);
            }
        });
    }

    /// Print a literal as the source spelled it (`0x04`, `1_000`, `3000ms`,
    /// `"\u{3000}"`); the AST holds only its value. A synthesized literal
    /// with no source token prints its canonical spelling.
    fn format_literal(&mut self, lit: &Literal, span: &Span) {
        use std::fmt::Write;
        if let Some(spelling) = self.literal_spelling(span) {
            self.write(&spelling);
            return;
        }
        match lit {
            Literal::Integer { value, radix } => {
                // Radix forms print the magnitude with an explicit sign: the
                // `i128` carrier's two's-complement rendering ("{:X}" of -1) is
                // 32 hex digits and would not round trip through the parser.
                let sign = if *value < 0 { "-" } else { "" };
                let magnitude = value.unsigned_abs();
                match radix {
                    IntRadix::Hex => {
                        let _ = write!(self.output, "{sign}0x{magnitude:X}");
                    }
                    IntRadix::Octal => {
                        let _ = write!(self.output, "{sign}0o{magnitude:o}");
                    }
                    IntRadix::Binary => {
                        let _ = write!(self.output, "{sign}0b{magnitude:b}");
                    }
                    IntRadix::Decimal => {
                        let _ = write!(self.output, "{value}");
                    }
                }
            }
            Literal::Float(f) => {
                let s = f.to_string();
                self.write(&s);
                // Ensure there's always a decimal point for clarity.
                if !s.contains('.') {
                    self.write(".0");
                }
            }
            Literal::String(s) => {
                self.write("\"");
                self.write(&escape_string(s));
                self.write("\"");
            }
            Literal::Bool(b) => self.write(if *b { "true" } else { "false" }),
            Literal::Char(c) => {
                let mut buf = String::new();
                escape_char_literal(*c, &mut buf);
                self.write("'");
                self.write(&buf);
                self.write("'");
            }
            Literal::Duration(ns) => {
                if *ns >= 3_600_000_000_000 && *ns % 3_600_000_000_000 == 0 {
                    let _ = write!(self.output, "{}h", *ns / 3_600_000_000_000);
                } else if *ns >= 60_000_000_000 && *ns % 60_000_000_000 == 0 {
                    let _ = write!(self.output, "{}m", *ns / 60_000_000_000);
                } else if *ns >= 1_000_000_000 && *ns % 1_000_000_000 == 0 {
                    let _ = write!(self.output, "{}s", *ns / 1_000_000_000);
                } else if *ns >= 1_000_000 && *ns % 1_000_000 == 0 {
                    let _ = write!(self.output, "{}ms", *ns / 1_000_000);
                } else if *ns >= 1_000 && *ns % 1_000 == 0 {
                    let _ = write!(self.output, "{}us", *ns / 1_000);
                } else {
                    let _ = write!(self.output, "{ns}ns");
                }
            }
        }
    }

    // ------------------------------------------------------------------
    // Patterns
    // ------------------------------------------------------------------

    fn format_pattern(&mut self, pat: &Spanned<Pattern>) {
        self.flush_inline_comments(pat.1.start);
        self.format_pattern_kind(pat);
        if pat.1.start < pat.1.end {
            self.prev_source_pos = self.prev_source_pos.max(pat.1.end);
        }
    }

    fn format_pattern_kind(&mut self, pat: &Spanned<Pattern>) {
        match &pat.0 {
            Pattern::Wildcard => self.write("_"),
            Pattern::Literal(lit) => self.format_literal(lit, &pat.1),
            Pattern::Identifier(name) => self.write_ident(*name),
            // A one-segment constructor with no operands is written bare.
            Pattern::NominalPath {
                path,
                payload: Some(NominalPatternPayload::Tuple(patterns)),
            } if path.segments.len() == 1 && patterns.is_empty() => self.format_path(path),
            Pattern::NominalPath { path, payload } => {
                self.format_path(path);
                self.format_nominal_pattern_payload(payload.as_ref());
            }
            Pattern::ContextVariant(context) => {
                self.write(".");
                self.write_ident(context.name);
                self.format_nominal_pattern_payload(context.payload.as_ref());
            }
            Pattern::RecordShorthand { fields, rest } => {
                self.format_record_pattern_fields(fields, rest.as_ref());
            }
            Pattern::Tuple(patterns) => {
                self.write("(");
                self.comma_sep(patterns, Formatter::format_pattern);
                self.write(")");
            }
            Pattern::Or(left, right) => {
                self.format_pattern(left);
                self.write_token(" | ", |t| matches!(t, hew_lexer::Token::Pipe));
                self.format_pattern(right);
            }
            Pattern::Regex { pattern, .. } => {
                self.write("re\"");
                self.write(&escape_regex_pattern(pattern));
                self.write("\"");
            }
        }
    }

    fn format_nominal_pattern_payload(&mut self, payload: Option<&NominalPatternPayload>) {
        match payload {
            None => {}
            Some(NominalPatternPayload::Tuple(patterns)) => {
                self.write("(");
                self.comma_sep(patterns, Formatter::format_pattern);
                self.write(")");
            }
            Some(NominalPatternPayload::Record { fields, rest }) => {
                self.write(" ");
                self.format_record_pattern_fields(fields, rest.as_ref());
            }
        }
    }

    fn format_pattern_field(&mut self, f: &PatternField) {
        self.write_ident(f.name);
        if let Some(pat) = &f.pattern {
            self.write(": ");
            self.format_pattern(pat);
        }
    }

    fn format_record_pattern_fields(
        &mut self,
        fields: &[PatternField],
        rest: Option<&Range<usize>>,
    ) {
        self.write("{ ");
        self.comma_sep(fields, Self::format_pattern_field);
        if rest.is_some() {
            if !fields.is_empty() {
                self.write(", ");
            }
            self.write("..");
        }
        self.write(" }");
    }
}

// ---------------------------------------------------------------------------
// String helpers
// ---------------------------------------------------------------------------

fn binary_op_str(op: BinaryOp) -> &'static str {
    match op {
        BinaryOp::Add => "+",
        BinaryOp::Subtract => "-",
        BinaryOp::Multiply => "*",
        BinaryOp::Divide => "/",
        BinaryOp::Modulo => "%",
        BinaryOp::Equal => "==",
        BinaryOp::NotEqual => "!=",
        BinaryOp::Less => "<",
        BinaryOp::LessEqual => "<=",
        BinaryOp::Greater => ">",
        BinaryOp::GreaterEqual => ">=",
        BinaryOp::And => "&&",
        BinaryOp::Or => "||",
        BinaryOp::BitAnd => "&",
        BinaryOp::BitOr => "|",
        BinaryOp::BitXor => "^",
        BinaryOp::Shl => "<<",
        BinaryOp::Shr => ">>",
        BinaryOp::Range => "..",
        BinaryOp::RangeInclusive => "..=",
        BinaryOp::WrappingAdd => "&+",
        BinaryOp::WrappingSub => "&-",
        BinaryOp::WrappingMul => "&*",
    }
}

/// Use the parser's binding powers so formatting cannot change operator grouping.
fn binop_precedence(op: BinaryOp) -> u8 {
    use hew_lexer::Token;

    let token = match op {
        BinaryOp::Or => Token::PipePipe,
        BinaryOp::BitOr => Token::Pipe,
        BinaryOp::BitXor => Token::Caret,
        BinaryOp::BitAnd => Token::Ampersand,
        BinaryOp::And => Token::AmpAmp,
        BinaryOp::Equal => Token::EqualEqual,
        BinaryOp::NotEqual => Token::NotEqual,
        BinaryOp::Less => Token::Less,
        BinaryOp::LessEqual => Token::LessEqual,
        BinaryOp::Greater => Token::Greater,
        BinaryOp::GreaterEqual => Token::GreaterEqual,
        BinaryOp::Range => Token::DotDot,
        BinaryOp::RangeInclusive => Token::DotDotEqual,
        BinaryOp::Shl => Token::LessLess,
        BinaryOp::Shr => Token::GreaterGreater,
        BinaryOp::Add => Token::Plus,
        BinaryOp::Subtract => Token::Minus,
        BinaryOp::WrappingAdd => Token::AmpPlus,
        BinaryOp::WrappingSub => Token::AmpMinus,
        BinaryOp::Multiply => Token::Star,
        BinaryOp::Divide => Token::Slash,
        BinaryOp::Modulo => Token::Percent,
        BinaryOp::WrappingMul => Token::AmpStar,
    };
    crate::parser::infix_bp(&token)
        .expect("binary operator has an infix binding power")
        .0
}

fn compound_assign_op_str(op: CompoundAssignOp) -> &'static str {
    match op {
        CompoundAssignOp::Add => "+=",
        CompoundAssignOp::Subtract => "-=",
        CompoundAssignOp::Multiply => "*=",
        CompoundAssignOp::Divide => "/=",
        CompoundAssignOp::Modulo => "%=",
        CompoundAssignOp::BitAnd => "&=",
        CompoundAssignOp::BitOr => "|=",
        CompoundAssignOp::BitXor => "^=",
        CompoundAssignOp::Shl => "<<=",
        CompoundAssignOp::Shr => ">>=",
    }
}

/// Returns `true` for non-ASCII Unicode scalars that should be emitted raw
/// (readable, unambiguous in source), following the rustfmt/gofmt convention.
///
/// The escape-vs-preserve decision is derived from the authoritative Unicode
/// `General_Category` so the predicate stays correct as new codepoints are
/// assigned and avoids the drift-prone hand-maintained range list it replaces.
///
/// Escapes (returns `false`) when the character belongs to any of:
/// - **Cc** (Control): U+0080–U+009F C1 controls and the ASCII controls
///   (the caller already gates `is_control()` before reaching here, but
///   `finl_unicode::is_control()` is the normative source for the Cc set).
/// - **Cf** (Format): soft hyphen, zero-width spaces, all `BiDi` controls
///   (U+202A–U+202E, U+2066–U+2069), deprecated format/shaping controls
///   (U+206A–U+206F), Mongolian vowel separator (U+180E), word joiners,
///   interlinear annotations, BOM, tag block — the Trojan-Source/confusable
///   class.
/// - **Co** (Private Use): U+E000–U+F8FF BMP PUA; supplementary PUA planes.
/// - **Cn** (Unassigned): no defined rendering — fail-closed.
/// - **Zl** (Line Separator): U+2028.
/// - **Zp** (Paragraph Separator): U+2029.
///
/// Also escapes (returns `false`) scalars that `General_Category` alone does
/// not catch but which still render as nothing or as a blank in source:
/// - **`Default_Ignorable_Code_Point`**: variation selectors (U+FE00–FE0F,
///   U+E0100–E01EF), the combining grapheme joiner (U+034F), Hangul fillers
///   (U+115F–1160), the tag block (U+E0000–E0FFF), and the rest of the
///   property. Many of these have a *readable* `General_Category` (Mn/Lo/So)
///   yet emit nothing, so a category-only predicate leaks them raw.
/// - **Blank-looking scalars**: BRAILLE PATTERN BLANK (U+2800) and the Hangul
///   fillers, which occupy width but show nothing — a confusable/space-spoof
///   vector.
///
/// Preserves (returns `true`) everything else: letters (L*), marks (M*),
/// numbers (N*), punctuation (P*), symbols (S*), and space separators (Zs) —
/// including accented letters (`é`), CJK (`世`), arrows (`→`), em dashes
/// (`—`), and similar readable Unicode.
///
/// Note: Cs (Surrogate) codepoints are not valid Rust `char` values and
/// therefore never reach this function.
#[must_use]
pub fn is_printable_non_ascii(c: char) -> bool {
    debug_assert!(!c.is_ascii(), "only call for non-ASCII chars");

    // Escape the C categories that are invisible, confusable, or unrendered.
    if c.is_control() // Cc — C1 controls (U+0080–U+009F) and ASCII controls
        || c.is_format() // Cf — all Format characters (BiDi, soft-hyphen, ZWJ, shaping, tags, …)
        || c.is_private_use() // Co — Private Use Areas
        || c.is_unassigned() // Cn — no assigned rendering, fail-closed
        || c.is_separator_line() // Zl — U+2028
        || c.is_separator_paragraph() // Zp — U+2029
        || is_default_ignorable(c) // renders as nothing despite a readable category
        || is_blank_looking(c)
    // occupies width but shows nothing (Braille blank, Hangul fillers)
    {
        return false;
    }

    true
}

/// Unicode `Default_Ignorable_Code_Point` property (from
/// `DerivedCoreProperties.txt`).  `finl_unicode` 1.4 exposes only
/// `General_Category`, so this property is encoded explicitly.  These scalars
/// are rendered as nothing by conforming renderers; several (e.g. U+FE0F
/// VARIATION SELECTOR-16, U+034F COMBINING GRAPHEME JOINER) sit in *readable*
/// categories (Mn/Lo/So) and so escape the `General_Category` gate, yet still
/// emit raw and act as confusable/Trojan-Source vectors.  Escaping them keeps
/// the formatted source visually faithful.
fn is_default_ignorable(c: char) -> bool {
    matches!(c as u32,
        0x00AD                              // SOFT HYPHEN
        | 0x034F                            // COMBINING GRAPHEME JOINER
        | 0x061C                            // ARABIC LETTER MARK
        | 0x115F..=0x1160                   // HANGUL CHOSEONG/JUNGSEONG FILLER
        | 0x17B4..=0x17B5                   // KHMER VOWEL INHERENT AQ/AA
        | 0x180B..=0x180F                   // MONGOLIAN FVS1-4 + VOWEL SEPARATOR
        | 0x200B..=0x200F                   // ZERO WIDTH SPACE … RLM
        | 0x202A..=0x202E                   // BiDi embedding/override controls
        | 0x2060..=0x206F                   // WORD JOINER … deprecated format/shaping
        | 0x3164                            // HANGUL FILLER
        | 0xFE00..=0xFE0F                   // VARIATION SELECTOR-1 … -16
        | 0xFEFF                            // ZERO WIDTH NO-BREAK SPACE / BOM
        | 0xFFA0                            // HALFWIDTH HANGUL FILLER
        | 0xFFF0..=0xFFF8                   // unassigned specials (ignorable)
        | 0x1BCA0..=0x1BCA3                 // SHORTHAND FORMAT CONTROLS
        | 0x1D173..=0x1D17A                 // MUSICAL SYMBOL BEGIN/END controls
        | 0xE0000..=0xE0FFF                 // TAGS + VARIATION SELECTORS SUPPLEMENT
    )
}

/// Scalars that occupy advance width but render as a blank box of nothing and
/// are **not** already caught by `is_default_ignorable`.  The Hangul fillers
/// (U+115F/U+1160/U+3164/U+FFA0) are blank-looking too but carry the
/// `Default_Ignorable` property, so they are handled there; the only addition
/// here is the Braille blank, whose `General_Category` is the readable `So`.
fn is_blank_looking(c: char) -> bool {
    c == '\u{2800}' // BRAILLE PATTERN BLANK
}

/// Escape a single character for use inside a double-quoted string or
/// f-string literal part.
///
/// Covers the full escape set the parser resolves:
///   `\n \t \r \\ \" \0 \xNN \u{...}`
///
/// • Named escapes for the six ASCII control chars the parser recognises by
///   name (`\n`, `\t`, `\r`, `\\`, `\"`, `\0`).
/// • `\xNN` (two lowercase hex digits) for any other non-printable ASCII byte
///   (code point < 0x20 or == 0x7F).
/// • Printable ASCII and readable non-ASCII Unicode pass through verbatim.
///   "Readable" means: NOT a control character, NOT a Format/Separator
///   Unicode category, NOT a private-use codepoint — per `is_printable_non_ascii`.
/// • `\u{HHHH}` (uppercase hex, no leading zeros beyond the minimum) for
///   invisible/confusable non-ASCII (zero-width spaces, `BiDi` overrides, C1
///   controls, etc.).
///
/// This follows the rustfmt/gofmt convention: preserve readable Unicode
/// (`é`, `→`, `世`, `—`) verbatim; escape only what is genuinely invisible
/// or confusable in source text.
///
/// `fstring_braces`: when `true`, also escape `{` and `}` as `\{` / `\}` so
/// they survive round-trip inside an f-string interpolation boundary.
fn escape_str_char(c: char, out: &mut String, fstring_braces: bool) {
    match c {
        '\\' => out.push_str("\\\\"),
        '"' => out.push_str("\\\""),
        '\n' => out.push_str("\\n"),
        '\t' => out.push_str("\\t"),
        '\r' => out.push_str("\\r"),
        '\0' => out.push_str("\\0"),
        '{' if fstring_braces => out.push_str("\\{"),
        '}' if fstring_braces => out.push_str("\\}"),
        c if c.is_ascii() => {
            let b = c as u8;
            if b < 0x20 || b == 0x7f {
                // Non-printable ASCII not covered by a named escape above.
                let _ = write!(out, "\\x{b:02x}");
            } else {
                out.push(c);
            }
        }
        c if c.is_control() => {
            // Non-ASCII control chars (C1 range U+0080–U+009F, NEL U+0085).
            let cp = c as u32;
            let _ = write!(out, "\\u{{{cp:X}}}");
        }
        c if is_printable_non_ascii(c) => {
            // Readable non-ASCII: preserve verbatim (é, →, 世, —, …).
            out.push(c);
        }
        c => {
            // Invisible or confusable non-ASCII (zero-width, BiDi, private-use, …).
            let cp = c as u32;
            let _ = write!(out, "\\u{{{cp:X}}}");
        }
    }
}

fn escape_string(s: &str) -> String {
    let mut out = String::with_capacity(s.len() + 4);
    for c in s.chars() {
        escape_str_char(c, &mut out, false);
    }
    out
}

/// Escape the literal-text parts of an f-string (`f"…"`).
///
/// The lexer has already resolved all escape sequences to their byte values
/// (e.g. `\n` → a real newline) before storing them in `StringPart::Literal`.
/// The formatter must re-escape those bytes so that re-parsing yields the
/// same value.  An f-string literal part has the same escape set as a plain
/// string *plus* `{` and `}`, which delimit interpolation holes and must be
/// written as `\{` / `\}` to survive round-trip.
fn escape_fstring_literal(s: &str) -> String {
    let mut out = String::with_capacity(s.len() + 4);
    for c in s.chars() {
        escape_str_char(c, &mut out, true);
    }
    out
}

/// Escape a regex literal pattern (`re"…"`) for re-emission.
///
/// Regex backslash sequences (`\s`, `\d`, `\u{…}`, …) are passed through
/// verbatim because the pattern reaches the engine unchanged save for the
/// delimiter escape `\"` — see `normalize_regex_literal`. A raw invisible or
/// confusable scalar that appears literally in the pattern is rewritten to a
/// `\u{…}` regex escape: the `regex` engine treats `\u{202E}` as the U+202E
/// codepoint, so the match semantics are identical while the source stays
/// visually faithful. This routes regex literals through the same
/// invisible/confusable policy as string, f-string, byte-string, and char
/// literals, closing the bypass where a raw RLO inside `re"…"` would be emitted
/// verbatim.
fn escape_regex_pattern(pattern: &str) -> String {
    let mut out = String::with_capacity(pattern.len());
    let mut chars = pattern.chars();
    while let Some(c) = chars.next() {
        if c == '\\' {
            // Preserve the regex escape (both chars) verbatim.
            out.push('\\');
            if let Some(next) = chars.next() {
                out.push(next);
            } else {
                out.push('\\');
            }
        } else if c == '"' {
            out.push_str("\\\"");
        } else if c.is_ascii() {
            out.push(c);
        } else if c.is_control() || !is_printable_non_ascii(c) {
            // Non-ASCII control or invisible/confusable scalar: emit a \u{…}
            // regex escape, which the engine maps to the same codepoint.
            let cp = c as u32;
            let _ = write!(out, "\\u{{{cp:X}}}");
        } else {
            // Readable non-ASCII (é, →, 世, …): preserve verbatim.
            out.push(c);
        }
    }
    out
}

fn escape_byte_string(data: &[u8]) -> String {
    if let Ok(s) = std::str::from_utf8(data) {
        return escape_string(s);
    }

    let mut out = String::with_capacity(data.len());
    for &b in data {
        match b {
            b'\\' => out.push_str("\\\\"),
            b'"' => out.push_str("\\\""),
            b'\n' => out.push_str("\\n"),
            b'\t' => out.push_str("\\t"),
            b'\r' => out.push_str("\\r"),
            b'\0' => out.push_str("\\0"),
            b' '..=b'!' | b'#'..=b'[' | b']'..=b'~' => out.push(b as char),
            _ => {
                let _ = write!(out, "\\x{b:02x}");
            }
        }
    }
    out
}

/// Escape a single character for use inside a single-quoted char literal
/// (`'…'`).
///
/// Shares the invisible/confusable escape policy with [`escape_str_char`]: the
/// only differences are that `'` (not `"`) is the delimiter that must be
/// backslash-escaped, and `{`/`}` need no escaping (a char literal has no
/// interpolation). Routing char literals through the same policy closes the
/// bypass where a raw U+202E (RLO) or U+FE0F (VS-16) char literal would
/// otherwise be emitted verbatim.
fn escape_char_literal(c: char, out: &mut String) {
    match c {
        '\\' => out.push_str("\\\\"),
        '\'' => out.push_str("\\'"),
        '\n' => out.push_str("\\n"),
        '\t' => out.push_str("\\t"),
        '\r' => out.push_str("\\r"),
        '\0' => out.push_str("\\0"),
        c if c.is_ascii() => {
            let b = c as u8;
            if b < 0x20 || b == 0x7f {
                let _ = write!(out, "\\x{b:02x}");
            } else {
                out.push(c);
            }
        }
        c if c.is_control() => {
            // Non-ASCII control chars (C1 range U+0080–U+009F, NEL U+0085).
            let cp = c as u32;
            let _ = write!(out, "\\u{{{cp:X}}}");
        }
        c if is_printable_non_ascii(c) => {
            // Readable non-ASCII: preserve verbatim (é, →, 世, —, …).
            out.push(c);
        }
        c => {
            // Invisible or confusable non-ASCII (zero-width, BiDi, variation
            // selectors, Default_Ignorable, …).
            let cp = c as u32;
            let _ = write!(out, "\\u{{{cp:X}}}");
        }
    }
}

// ---------------------------------------------------------------------------
// Comment extraction
// ---------------------------------------------------------------------------

/// One member of a machine body, in the order the source declares it.
enum MachineMember<'a> {
    Events,
    Emits,
    State(&'a MachineState),
    Composite(&'a crate::ast::CompositeGroup),
    Transition(&'a MachineTransition),
    Default,
}

/// One member of an actor body, in the order the source declares it.
enum ActorMember<'a> {
    Field(&'a FieldDecl),
    Mailbox,
    Init(&'a ActorInit),
    Receive(&'a ReceiveFnDecl),
    Method(&'a FnDecl),
}

/// First source byte of a declaration whose keyword starts at `keyword`,
/// counting any attributes written before it.
fn member_start(attributes: &[Attribute], keyword: usize) -> usize {
    attributes
        .iter()
        .map(|a| a.span.start)
        .chain(std::iter::once(keyword))
        .min()
        .unwrap_or(keyword)
}

#[derive(Debug, Clone)]
pub struct Comment {
    pub text: String,
    pub span: Range<usize>,
}

/// The comments of `source` with their byte positions, taken from the
/// lexer: the trivia between its tokens, plus doc-comment tokens when
/// `include_doc_comments` is set. Strings, raw strings, characters and
/// f-string interpolations can therefore never be mistaken for comments.
#[must_use]
pub fn extract_comments(source: &str, include_doc_comments: bool) -> Vec<Comment> {
    let mut comments = Vec::new();
    let mut gap_start = 0;
    for (token, span) in hew_lexer::Lexer::new(source) {
        trivia_comments(source, gap_start, span.start, &mut comments);
        if include_doc_comments
            && matches!(
                token,
                hew_lexer::Token::DocComment(_) | hew_lexer::Token::InnerDocComment(_)
            )
        {
            let text = source[span.start..span.end].trim_end_matches('\r');
            comments.push(Comment {
                text: text.to_string(),
                span: span.start..span.start + text.len(),
            });
        }
        gap_start = span.end;
    }
    trivia_comments(source, gap_start, source.len(), &mut comments);
    comments
}

/// The comments in `source[start..end]`, a gap the lexer skipped, which
/// holds only whitespace and comments.
fn trivia_comments(source: &str, start: usize, end: usize, out: &mut Vec<Comment>) {
    let gap = &source.as_bytes()[..end];
    let mut i = start;
    while i < end {
        if gap[i..].starts_with(b"//") {
            let len = gap[i..].iter().position(|&b| b == b'\n').unwrap_or(end - i);
            let text = source[i..i + len].trim_end_matches('\r');
            out.push(Comment {
                text: text.to_string(),
                span: i..i + text.len(),
            });
            i += len;
        } else if gap[i..].starts_with(b"/*") {
            let comment_start = i;
            let mut depth = 0usize;
            while i < end {
                if gap[i..].starts_with(b"/*") {
                    depth += 1;
                    i += 2;
                } else if gap[i..].starts_with(b"*/") {
                    depth -= 1;
                    i += 2;
                    if depth == 0 {
                        break;
                    }
                } else {
                    i += 1;
                }
            }
            out.push(Comment {
                text: source[comment_start..i].to_string(),
                span: comment_start..i,
            });
        } else {
            i += 1;
        }
    }
}

/// Whether the author left a blank line directly before `pos`, or before
/// the doc comment that opens the declaration at `pos`.
fn blank_line_before(source: &str, pos: usize) -> bool {
    let mut before = source[..pos.min(source.len())].trim_end();
    loop {
        let line_start = before.rfind('\n').map_or(0, |i| i + 1);
        if !before[line_start..].trim_start().starts_with("///") {
            break;
        }
        before = before[..line_start].trim_end();
    }
    let end = before.len();
    let gap = &source[end..pos.min(source.len())];
    // The gap runs to the declaration; only its leading whitespace counts.
    let leading = &gap[..gap.len() - gap.trim_start().len()];
    leading.matches('\n').count() > 1
}

fn is_trailing_comment(source: &str, comment_start: usize) -> bool {
    let bytes = source.as_bytes();
    let mut i = comment_start;
    while i > 0 && bytes[i - 1] != b'\n' {
        i -= 1;
    }
    source[i..comment_start].chars().any(|c| !c.is_whitespace())
}

/// Source column (0-based) of `pos` within its line. Used by the block-end
/// comment dedent heuristic to decide whether a comment is at outer or inner
/// indent — a comment at or before the closing `}`'s column documents the
/// next branch in an if/else-if chain (or similar) and must be re-emitted at
/// outer indent so the chain idiom round-trips.
fn source_column(source: &str, pos: usize) -> usize {
    let bytes = source.as_bytes();
    let end = pos.min(bytes.len());
    let mut i = end;
    while i > 0 && bytes[i - 1] != b'\n' {
        i -= 1;
    }
    end - i
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse;

    #[test]
    fn migrates_legacy_paths_turbofish_and_checker_selected_variants() {
        let source = concat!(
            "import a.b.{C};\n",
            "fn main() {\n",
            "    let value = Some(42);\n",
            "    f::<T>();\n",
            "    HashMap::<string, i64>::new();\n",
            "    Vec::new::<i64>();\n",
            "    // keep a::b and f::<T>() in comments\n",
            "    println(\"a::b and f::<T>()\");\n",
            "}\n"
        );
        let start = source.find("Some(42)").unwrap();
        let migrated = migrate_legacy_syntax(
            source,
            &[VariantMigration {
                span: start..start + "Some".len(),
                name: "Some".to_string(),
                replacement: "Option.Some".to_string(),
            }],
        )
        .unwrap();

        assert_eq!(
            migrated,
            concat!(
                "import a.b.{C};\n",
                "fn main() {\n",
                "    let value = Option.Some(42);\n",
                "    f<T>();\n",
                "    HashMap<string, i64>.new();\n",
                "    Vec.new<i64>();\n",
                "    // keep a::b and f::<T>() in comments\n",
                "    println(\"a::b and f::<T>()\");\n",
                "}\n"
            )
        );
        assert_eq!(migrate_legacy_syntax(&migrated, &[]).unwrap(), migrated);
    }

    #[test]
    fn refuses_variant_rewrite_without_the_checker_selected_token() {
        let error = migrate_legacy_syntax(
            "fn main() { println(\"Some\"); }\n",
            &[VariantMigration {
                span: 21..25,
                name: "Some".to_string(),
                replacement: ".Some".to_string(),
            }],
        )
        .unwrap_err();

        assert_eq!(error.refusals.len(), 1);
        assert!(error.refusals[0]
            .reason
            .contains("expected identifier `Some` selected by the checker"));
    }

    fn roundtrip(src: &str) -> String {
        let result = parse(src);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        format_program(&result.program)
    }

    #[test]
    fn runtime_attribute_values_survive_formatting() {
        let source = r#"
            impl Connection {
                #[runtime(family = TcpAttachLocal, symbol = hew_tcp_attach_native,
                    lowering = actor_ingress, target = native,
                    classification = "non-declarable-stdlib", receiver = connection,
                    data = on_data, close = on_close, result = status_result,
                    error_type = "std.net.AttachError", error_variant = Refused)]
                fn attach() {}
            }
        "#;
        let parsed = parse(source);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let formatted = format_program(&parsed.program);
        let reparsed = parse(&formatted);
        assert!(
            reparsed.errors.is_empty(),
            "{:?}\n{formatted}",
            reparsed.errors
        );
        let Item::Impl(before) = &parsed.program.items[0].0 else {
            panic!("expected original implementation");
        };
        let Item::Impl(after) = &reparsed.program.items[0].0 else {
            panic!("expected formatted implementation");
        };
        assert_eq!(
            before.methods[0].attributes[0].args,
            after.methods[0].attributes[0].args
        );
        assert_eq!(format_program(&reparsed.program), formatted);
    }

    #[test]
    fn simple_function() {
        let src = "fn main() -> i32 {\n    0\n}\n";
        let formatted = roundtrip(src);
        assert_eq!(formatted, src);
    }

    #[test]
    fn doc_comment_before_impl_block_is_preserved() {
        // Regression test for #3238: the formatter's item model attached
        // `doc_comment` to every other item kind (fn/struct/enum/trait) but
        // not to `impl`, so a `///` comment immediately preceding an `impl`
        // block was collected by the parser and then dropped on the floor.
        let src = "\
enum Foo {
    A,
}

/// Doc comment for Foo Display.
impl Display for Foo {
    fn fmt(f: Foo) -> string {
        match f {
            .A => \"a\",
        }
    }
}
";
        let formatted = roundtrip(src);
        assert_eq!(formatted, src);
    }

    #[test]
    fn fibonacci() {
        let src = "\
fn fibonacci(n: i32) -> i32 {
    if n <= 1 {
        n
    } else {
        fibonacci(n - 1) + fibonacci(n - 2)
    }
}

fn main() -> i32 {
    let result = fibonacci(10);
    println(result);
    0
}
";
        let formatted = roundtrip(src);
        assert_eq!(formatted, src);
    }

    #[test]
    fn actor_declaration() {
        let src = "\
actor Counter {
    let count: i32,

    receive fn increment() {
        self.count = self.count + 1;
    }

    receive fn get_count() -> i32 {
        self.count
    }
}
";
        let formatted = roundtrip(src);
        assert_eq!(formatted, src);
    }

    #[test]
    fn enum_declaration() {
        let src = "\
enum Colour {
    Red,
    Green,
    Blue,
}
";
        let formatted = roundtrip(src);
        assert_eq!(formatted, src);
    }

    #[test]
    fn extern_block() {
        let src = "\
extern \"C\" {
    fn puts(s: string) -> i32;
    fn exit(code: i32);
}
";
        let formatted = roundtrip(src);
        assert_eq!(formatted, src);
    }

    #[test]
    fn extern_rt_block_roundtrip() {
        let src = "\
extern \"rt\" {
    fn println(s: string);
    fn print(s: string);
    fn assert(cond: bool);
}
";
        let formatted = roundtrip(src);
        assert_eq!(formatted, src);
    }

    #[test]
    fn pub_type_alias_roundtrip() {
        // `format_type_alias` emits `write_visibility` before `type`; a `pub`
        // top-level alias must survive format -> reparse rather than
        // silently losing its visibility modifier.
        let src = "\
pub type Label = string;

fn main() {
}
";
        let formatted = roundtrip(src);
        assert_eq!(formatted, src);
    }

    #[test]
    fn pub_supervisor_roundtrip() {
        // `format_supervisor` must emit the visibility modifier: a file import
        // publishes only `pub` declarations, so dropping it here turns a
        // formatted supervisor invisible to its importer.
        let src = "\
pub actor Worker {
    let id: i64,

    receive fn identify() -> i64 {
        id
    }
}

pub supervisor Inner {
    strategy: one_for_one,

    child worker: Worker(id: 23) restart: temporary,
}
";
        let formatted = roundtrip(src);
        assert_eq!(formatted, src);
    }

    #[test]
    fn parameterized_type_alias_roundtrip() {
        // `format_type_alias` calls `format_opt_type_params` between the
        // alias name and `=`; a generic alias's type-parameter list must
        // round-trip through format -> reparse.
        let src = "\
type Pair<T> = (T, T);

fn main() {
}
";
        let formatted = roundtrip(src);
        assert_eq!(formatted, src);
    }

    #[test]
    fn preserves_consume_modifier_on_extern_param() {
        // `consume` pins by-move ownership on an affine boundary parameter;
        // the formatter must emit it so the surface ownership disposition
        // survives a format → reparse round-trip. Regression: the modifier
        // was silently dropped, turning a boundary `consume` into an
        // inferred borrow and breaking the corpus round-trip (RAII-2 #1295).
        let src = "\
extern \"C\" {
    fn sink(consume c: Conn) -> i32;
}
";
        let formatted = roundtrip(src);
        assert_eq!(formatted, src);
    }

    #[test]
    fn preserves_consume_before_var_param_order() {
        // Surface grammar is `consume var name: T` — `consume` precedes
        // `var`. The formatter must reproduce that order so the reparse
        // recovers both the move disposition and the mutable binding.
        let src = "\
fn drain(consume var c: Conn) -> i32 {
    0
}
";
        let formatted = roundtrip(src);
        assert_eq!(formatted, src);
    }

    #[test]
    fn preserves_trait_receiver_identity_and_consuming_self() {
        let src = "\
trait Fluent {
    #[returns_receiver]
    fn with(consume self, consume child: Child) -> Self;
}
";
        let formatted = roundtrip(src);
        assert_eq!(formatted, src);
    }

    fn roundtrip_source(src: &str) -> String {
        let result = parse(src);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        format_source(src, &result.program)
    }

    #[test]
    fn full_range_and_negative_literals_round_trip_in_every_radix() {
        // The `i128` carrier renders negatives as their two's-complement form
        // under `{:X}`, so radix output prints an explicit sign and magnitude.
        // Each spelling must survive format-and-reparse unchanged.
        for source in [
            "fn main() {\n    let a: u64 = 18446744073709551615;\n}\n",
            "fn main() {\n    let a: u64 = 0xFFFFFFFFFFFFFFFF;\n}\n",
            "fn main() {\n    let a: u64 = 0o1777777777777777777777;\n}\n",
            "fn main() {\n    let a: u8 = 0b11111111;\n}\n",
            "fn main() {\n    let a: i64 = -9223372036854775808;\n}\n",
            "fn main() {\n    match x {\n        -0x10 => 0,\n        _ => 1,\n    }\n}\n",
        ] {
            let once = roundtrip_source(source);
            assert_eq!(once, source, "first format changed the source");
            assert_eq!(
                roundtrip_source(&once),
                once,
                "format is not idempotent for {source:?}"
            );
        }
    }

    #[test]
    fn preserves_leading_comment() {
        let src = "\
// A greeting function
fn greet() {
    println(\"hello\");
}
";
        assert_eq!(roundtrip_source(src), src);
    }

    #[test]
    fn preserves_trailing_comment() {
        let src = "\
fn main() -> i32 {
    let x = 5; // init value
    println(x);
    0
}
";
        assert_eq!(roundtrip_source(src), src);
    }

    #[test]
    fn preserves_comment_between_items() {
        let src = "\
fn foo() -> i32 {
    1
}

// Bar does something else
fn bar() -> i32 {
    2
}
";
        assert_eq!(roundtrip_source(src), src);
    }

    #[test]
    fn preserves_comment_inside_block() {
        let src = "\
fn main() -> i32 {
    // compute result
    let x = 5;
    0
}
";
        assert_eq!(roundtrip_source(src), src);
    }

    #[test]
    fn hex_literal_preserved() {
        let src = "fn main() {\n    let x = 0xFF;\n}\n";
        assert_eq!(roundtrip(src), src);
    }

    #[test]
    fn binary_literal_preserved() {
        let src = "fn main() {\n    let x = 0b1010;\n}\n";
        assert_eq!(roundtrip(src), src);
    }

    #[test]
    fn octal_literal_preserved() {
        let src = "fn main() {\n    let x = 0o77;\n}\n";
        assert_eq!(roundtrip(src), src);
    }

    #[test]
    fn await_prefix_syntax() {
        // `await` expressions round-trip independently of the function modifier.
        let src = "fn main() {\n    let x = await foo();\n}\n";
        assert_eq!(roundtrip(src), src);
    }

    #[test]
    fn async_gen_fn_roundtrip() {
        let src = "gen fn stream() -> i32 {\n    yield 1;\n}\n";
        assert_eq!(roundtrip(src), src);
    }

    #[test]
    fn pub_const_preserved() {
        let src = "pub const MAX: i32 = 100;\n";
        assert_eq!(roundtrip(src), src);
    }

    #[test]
    fn named_call_args_preserved() {
        let src = "\
fn main() {
    foo(name: 42, value: 10);
}
";
        assert_eq!(roundtrip(src), src);
    }

    #[test]
    fn operator_precedence_parens() {
        // Lower-precedence operand on the right of higher-precedence op needs parens
        let src = "fn main() {\n    let x = a * (b + c);\n}\n";
        assert_eq!(roundtrip(src), src);
    }

    #[test]
    fn optional_recovery_preserves_grouping_under_unary_and_binary_operators() {
        for source in [
            "fn main() {\n    let value = (a ?? b) ?? c;\n}\n",
            "fn main() {\n    let value = a ?? b ?? c;\n}\n",
            "fn main() {\n    let value = -(a ?? b);\n}\n",
            "fn main() {\n    let value = (a ?? b) + c;\n}\n",
        ] {
            assert_eq!(roundtrip(source), source);
        }
    }

    #[test]
    fn identity_preserves_operator_grouping() {
        for expression in [
            "!(left is right)",
            "(left is right) + offset",
            "left is (middle is right)",
            "left is (a && b)",
            "(a == b) & mask",
            "a || (b .. c)",
        ] {
            let source = format!("fn main() {{\n    let value = {expression};\n}}\n");
            assert_eq!(roundtrip(&source), source);
        }
    }

    #[test]
    fn operator_same_precedence_right_assoc() {
        // a - (b - c) must keep parens (same precedence, right operand)
        let src = "fn main() {\n    let x = a - (b - c);\n}\n";
        assert_eq!(roundtrip(src), src);
    }

    #[test]
    fn enum_all_variants_comma() {
        let src = "\
enum Colour {
    Red,
    Green,
    Blue,
}
";
        assert_eq!(roundtrip(src), src);
    }

    #[test]
    fn wire_enum_roundtrips() {
        let src = "\
#[wire]
enum Status {
    Pending,
    Active,
    Completed,
}
";
        assert_eq!(roundtrip(src), src);
    }

    #[test]
    fn wire_enum_with_json_case_roundtrips() {
        let src = "\
#[json(camelCase)]
#[wire]
enum Status {
    PendingReview,
    ActiveNow,
    Completed,
}
";
        assert_eq!(roundtrip(src), src);
    }

    #[test]
    fn wire_type_since_roundtrips() {
        let src = "\
#[wire]
type Msg {
    added: String @2 repeated since 3 yaml(\"added\"),
}
";
        assert_eq!(roundtrip(src), src);
    }

    #[test]
    fn wire_type_roundtrips_byte_stably() {
        let src = "\
#[wire]
type Msg {
    id: i64 @1,
    name: Option<String> @2 optional since 2,
    tags: Vec<String> @3 repeated,
    reserved @4;
}
";
        let formatted = roundtrip(src);
        assert!(
            !formatted.contains("struct"),
            "formatted wire type should not contain struct: {formatted}"
        );
        assert_eq!(formatted, src);
        assert_eq!(roundtrip(&formatted), formatted);
    }

    #[test]
    fn enum_inline_comments_preserved() {
        let src = "\
pub enum IoError {
    // The target path does not exist.
    NotFound(int),
    // The process lacks permission for the operation.
    PermissionDenied(int),
    Other(int), // catch-all
}
";
        assert_eq!(roundtrip_source(src), src);
    }

    #[test]
    fn struct_field_comments_preserved() {
        let src = "\
type Config {
    // The server port to bind on.
    port: int,
    host: string, // hostname or IP
}
";
        assert_eq!(roundtrip_source(src), src);
    }

    #[test]
    fn match_arm_comments_preserved() {
        let src = "\
fn classify(e: IoError) -> string {
    match e {
        // file not found
        IoError.NotFound(_) => \"missing\",
        IoError.PermissionDenied(_) => \"denied\", // access error
        _ => \"other\",
    }
}
";
        assert_eq!(roundtrip_source(src), src);
    }

    #[test]
    fn match_arm_trailing_on_last_arm() {
        let src = "\
fn classify(e: IoError) -> string {
    match e {
        IoError.NotFound(_) => \"missing\",
        IoError.Other(_) => \"error\", // catch-all trailing
    }
}
";
        assert_eq!(roundtrip_source(src), src);
    }

    #[test]
    fn match_expr_arm_comments_preserved() {
        let src = "\
fn main() -> string {
    let msg = match get_error() {
        // success path
        IoError.NotFound(_) => \"not found\",
        _ => \"error\",
    };
    msg
}
";
        assert_eq!(roundtrip_source(src), src);
    }

    #[test]
    fn empty_block_collapses_to_single_line() {
        // Regression: multi-line empty blocks (e.g. `.Ok(_) => {\n        },`)
        // shift absolute byte offsets of every later expression and have
        // surfaced cross-module span-collision crashes in codegen. Empty
        // blocks must be rendered as `{}` on one line, matching the rustfmt
        // / gofmt convention.
        let src = "\
fn assert_ok(result: Result<(), int>) {
    match result {
        Ok(_) => {
        },
        Err(e) => panic(\"op failed\"),
    }
}

fn empty_fn() {
}
";
        let expected = "\
fn assert_ok(result: Result<(), int>) {
    match result {
        Ok(_) => {},
        Err(e) => panic(\"op failed\"),
    }
}

fn empty_fn() {}
";
        assert_eq!(roundtrip_source(src), expected);
        // Idempotence on the collapsed form.
        assert_eq!(roundtrip_source(expected), expected);
    }

    #[test]
    fn combined_comments_idempotent() {
        let src = "\
// module-level comment
enum Status {
    // ok variant
    Ok,
    // error variant
    Err(int), // with payload
}

type Cfg {
    // host to connect to
    host: string,
    port: int, // default 8080
}

fn handle(s: Status) -> int {
    match s {
        // success
        Status.Ok => 0,
        // failure
        Status.Err(code) => code,
    }
}
";
        let once = roundtrip_source(src);
        assert_eq!(once, src, "first pass must preserve comments");
        let twice = roundtrip_source(&once);
        assert_eq!(twice, once, "second pass must be idempotent");
    }

    #[test]
    fn preserves_comment_above_else_if_branch_header() {
        let src = "\
fn classify(x: int) -> int {
    if x == 0 {
        0
    // documents the next branch
    } else if x == 1 {
        1
    } else {
        -1
    }
}
";
        let once = roundtrip_source(src);
        assert_eq!(once, src, "first pass must preserve comment");
        let twice = roundtrip_source(&once);
        assert_eq!(twice, once, "second pass must be idempotent");
    }

    #[test]
    fn preserves_comment_above_else_branch_header() {
        let src = "\
fn classify(x: int) -> int {
    if x == 0 {
        0
    // fallback case
    } else {
        -1
    }
}
";
        let once = roundtrip_source(src);
        assert_eq!(once, src, "first pass must preserve comment");
        let twice = roundtrip_source(&once);
        assert_eq!(twice, once, "second pass must be idempotent");
    }

    #[test]
    fn preserves_long_else_if_chain_comments() {
        let src = "\
fn lookup(errno: int) -> int {
    // ENOENT
    if errno == 2 {
        1
    // EACCES
    } else if errno == 13 {
        2
    // EEXIST
    } else if errno == 17 {
        3
    } else {
        0
    }
}
";
        let once = roundtrip_source(src);
        assert_eq!(once, src, "first pass must preserve comments");
        let twice = roundtrip_source(&once);
        assert_eq!(twice, once, "second pass must be idempotent");
    }

    #[test]
    fn preserves_comment_above_match_arm_block_close() {
        let src = "\
fn handle(x: int) -> int {
    match x {
        0 => {
            1
        // documents next arm
        },
        _ => 2,
    }
}
";
        let once = roundtrip_source(src);
        assert_eq!(once, src, "first pass must preserve comment");
        let twice = roundtrip_source(&once);
        assert_eq!(twice, once, "second pass must be idempotent");
    }

    #[test]
    fn preserves_trailing_comment_on_extern_fn() {
        let src = "\
extern \"C\" {
    fn foo(x: i32) -> i32; // returns the value untouched
    fn bar(y: i32);
}
";
        let once = roundtrip_source(src);
        assert_eq!(once, src, "first pass must preserve trailing comment");
        let twice = roundtrip_source(&once);
        assert_eq!(twice, once, "second pass must be idempotent");
    }

    #[test]
    fn preserves_trailing_comment_on_last_extern_fn() {
        let src = "\
extern \"C\" {
    fn foo(x: i32) -> i32;
    fn bar(y: i32) -> i32; // last fn trailing comment
}
";
        let once = roundtrip_source(src);
        assert_eq!(
            once, src,
            "first pass must preserve trailing comment on last fn"
        );
        let twice = roundtrip_source(&once);
        assert_eq!(twice, once, "second pass must be idempotent");
    }

    #[test]
    fn preserves_leading_comment_above_extern_fn() {
        let src = "\
extern \"C\" {
    fn first(x: i32) -> i32;
    // groups the read variants
    fn read_u8(buf: i32) -> i32;
    fn read_u16(buf: i32) -> i32;
}
";
        let once = roundtrip_source(src);
        assert_eq!(once, src, "first pass must preserve leading comment");
        let twice = roundtrip_source(&once);
        assert_eq!(twice, once, "second pass must be idempotent");
    }

    #[test]
    fn preserves_internal_abi_marker_on_extern_fn() {
        let src = "\
extern \"C\" {
    fn hew_stream_last_error() -> string;
    fn hew_stream_last_errno() -> i32; // INTERNAL-ABI: OS errno from thread-local; 0 when none recorded
}
";
        let once = roundtrip_source(src);
        assert_eq!(
            once, src,
            "INTERNAL-ABI marker must remain on the fn it documents"
        );
        let twice = roundtrip_source(&once);
        assert_eq!(twice, once, "second pass must be idempotent");
    }

    #[test]
    fn scope_block_roundtrips() {
        let src = "\
fn main() {
    scope {
        1
    };
}
";
        assert_eq!(roundtrip(src), src);
    }

    #[test]
    fn fork_child_forms_roundtrip() {
        let src = "\
fn main() {
    let child = fork run();
    fork run_other();
}
";
        assert_eq!(roundtrip(src), src);
    }

    #[test]
    fn nested_scope_roundtrips() {
        let src = "\
fn main() {
    scope {
        let worker = fork run();
        fork audit();
        worker
    };
}
";
        assert_eq!(roundtrip(src), src);
    }

    // ── StructInit with explicit type args (F1 formatter coverage) ────────

    #[test]
    fn struct_init_explicit_type_arg_formats_correctly() {
        // The formatter must emit `Name<T> { ... }` when type_args is Some.
        let src = "\
type Wrapper<T> {
    value: T,
}

fn main() {
    let w = Wrapper<String> { value: \"hello\" };
}
";
        assert_eq!(roundtrip(src), src);
    }

    #[test]
    fn struct_init_without_type_arg_formats_unchanged() {
        // The formatter must emit `Name { ... }` (no `<>`) when type_args is None.
        let src = "\
type Wrapper<T> {
    value: T,
}

fn main() {
    let w = Wrapper { value: \"hello\" };
}
";
        assert_eq!(roundtrip(src), src);
    }

    #[test]
    fn extern_symbol_attribute_round_trips_in_extern_block() {
        // `#[extern_symbol("…")]` on an `extern "C"` fn
        // must survive a parse/format round-trip. The string contains `{T}`
        // which is non-identifier-shaped, so the formatter re-quotes it.
        let src = "\
extern \"C\" {
    #[extern_symbol(\"hew_vec_push_{T}\")]
    fn hew_vec_push(v: ptr, x: ptr);
}
";
        assert_eq!(roundtrip(src), src);
    }

    #[test]
    fn extern_symbol_attribute_round_trips_on_impl_method() {
        let src = "\
impl<T> Vec<T> {
    #[extern_symbol(\"hew_vec_push_{T}\")]
    fn push(self, x: T) {
    }
}
";
        assert_eq!(roundtrip(src), src);
    }

    // ── escape_str_char: escape-sequence faithfulness (P0) ──────────────────

    /// Named escape sequences that the lexer resolved to their byte values must
    /// re-emit as the named escape, NOT as a raw byte.
    #[test]
    fn escape_string_roundtrips_named_escapes() {
        // The lexer stores a literal newline byte; escape_string must emit \n.
        assert_eq!(escape_string("\n"), "\\n");
        assert_eq!(escape_string("\t"), "\\t");
        assert_eq!(escape_string("\r"), "\\r");
        assert_eq!(escape_string("\\"), "\\\\");
        assert_eq!(escape_string("\""), "\\\"");
        assert_eq!(escape_string("\0"), "\\0");
    }

    /// \xNN escape for non-printable ASCII outside the named set.
    #[test]
    fn escape_string_roundtrips_xnn_control() {
        // U+0001 SOH — non-printable ASCII, not in named set.
        assert_eq!(escape_string("\x01"), "\\x01");
        // U+001F US — highest C0 below space, not in named set.
        assert_eq!(escape_string("\x1f"), "\\x1f");
        // U+007F DEL.
        assert_eq!(escape_string("\x7f"), "\\x7f");
    }

    /// \u{...} escape for non-ASCII invisible/confusable characters.
    #[test]
    fn escape_string_escapes_invisible_unicode() {
        // U+200B zero-width space (Format Cf) — must be escaped.
        assert_eq!(escape_string("\u{200B}"), "\\u{200B}");
        // U+202E right-to-left override (BiDi Cf) — must be escaped.
        assert_eq!(escape_string("\u{202E}"), "\\u{202E}");
        // U+FEFF byte order mark (Cf) — must be escaped.
        assert_eq!(escape_string("\u{FEFF}"), "\\u{FEFF}");
        // U+2028 line separator (Zl) — must be escaped.
        assert_eq!(escape_string("\u{2028}"), "\\u{2028}");
        // U+2029 paragraph separator (Zp) — must be escaped.
        assert_eq!(escape_string("\u{2029}"), "\\u{2029}");
        // U+00AD soft hyphen (Cf) — must be escaped.
        assert_eq!(escape_string("\u{AD}"), "\\u{AD}");
    }

    // ── escape_str_char: preserve-readable-Unicode (A176) ───────────────────

    /// Readable non-ASCII Unicode must pass through verbatim.
    #[test]
    fn escape_string_preserves_readable_unicode() {
        // Accented letter.
        assert_eq!(escape_string("é"), "é");
        // Arrow symbol.
        assert_eq!(escape_string("→"), "→");
        // CJK ideograph.
        assert_eq!(escape_string("世"), "世");
        // Em dash.
        assert_eq!(escape_string("—"), "—");
        // Mixed: readable Unicode adjacent to ASCII.
        assert_eq!(escape_string("café"), "café");
        assert_eq!(escape_string("résumé"), "résumé");
    }

    /// `is_printable_non_ascii` classifies readable chars as printable.
    #[test]
    fn is_printable_non_ascii_true_for_readable() {
        assert!(is_printable_non_ascii('é'), "accented letter");
        assert!(is_printable_non_ascii('→'), "arrow");
        assert!(is_printable_non_ascii('世'), "CJK");
        assert!(is_printable_non_ascii('—'), "em dash");
        assert!(is_printable_non_ascii('π'), "greek letter");
        assert!(is_printable_non_ascii('£'), "pound sign");
        assert!(is_printable_non_ascii('©'), "copyright sign");
    }

    /// `is_printable_non_ascii` classifies invisible/confusable chars as non-printable.
    #[test]
    fn is_printable_non_ascii_false_for_invisible() {
        assert!(!is_printable_non_ascii('\u{200B}'), "zero-width space");
        assert!(!is_printable_non_ascii('\u{200D}'), "zero-width joiner");
        assert!(!is_printable_non_ascii('\u{202E}'), "rtl override");
        assert!(!is_printable_non_ascii('\u{FEFF}'), "BOM");
        assert!(!is_printable_non_ascii('\u{2028}'), "line separator");
        assert!(!is_printable_non_ascii('\u{2029}'), "paragraph separator");
        assert!(!is_printable_non_ascii('\u{00AD}'), "soft hyphen");
        assert!(!is_printable_non_ascii('\u{E000}'), "private use start");
        assert!(!is_printable_non_ascii('\u{FFF0}'), "specials block");
        // Previously-missed Cf chars (deprecated format/shaping controls).
        assert!(
            !is_printable_non_ascii('\u{206A}'),
            "U+206A INHIBIT SYMMETRIC SWAPPING (Cf)"
        );
        assert!(
            !is_printable_non_ascii('\u{206B}'),
            "U+206B ACTIVATE SYMMETRIC SWAPPING (Cf)"
        );
        assert!(
            !is_printable_non_ascii('\u{206C}'),
            "U+206C INHIBIT ARABIC FORM SHAPING (Cf)"
        );
        assert!(
            !is_printable_non_ascii('\u{206D}'),
            "U+206D ACTIVATE ARABIC FORM SHAPING (Cf)"
        );
        assert!(
            !is_printable_non_ascii('\u{206E}'),
            "U+206E NATIONAL DIGIT SHAPES (Cf)"
        );
        assert!(
            !is_printable_non_ascii('\u{206F}'),
            "U+206F NOMINAL DIGIT SHAPES (Cf)"
        );
        assert!(
            !is_printable_non_ascii('\u{180E}'),
            "U+180E MONGOLIAN VOWEL SEPARATOR (Cf)"
        );
    }

    // ── property tests: category-derived escape/preserve classification ──────

    /// Every non-ASCII char in `General_Category` Cf (Format) must be escaped.
    /// Cf contains the Trojan-Source / confusable class — all must be escaped.
    #[test]
    fn property_all_cf_chars_escape() {
        let leaked: Vec<char> = (0x80u32..=0x0010_FFFFu32)
            .filter_map(char::from_u32)
            .filter(|c| c.is_format() && is_printable_non_ascii(*c))
            .collect();
        assert!(
            leaked.is_empty(),
            "Cf chars incorrectly marked printable: {:?}",
            leaked
                .iter()
                .map(|c| format!("U+{:04X}", *c as u32))
                .collect::<Vec<_>>()
        );
    }

    /// Every non-ASCII char in `General_Category` Cc (Control) must be escaped.
    #[test]
    fn property_all_cc_chars_escape() {
        let leaked: Vec<char> = (0x80u32..=0x0010_FFFFu32)
            .filter_map(char::from_u32)
            .filter(|c| c.is_control() && is_printable_non_ascii(*c))
            .collect();
        assert!(
            leaked.is_empty(),
            "Cc chars incorrectly marked printable: {:?}",
            leaked
                .iter()
                .map(|c| format!("U+{:04X}", *c as u32))
                .collect::<Vec<_>>()
        );
    }

    /// Every non-ASCII char in `General_Category` Co (Private Use) must be escaped.
    #[test]
    fn property_all_co_chars_escape() {
        let leaked: Vec<char> = (0x80u32..=0x0010_FFFFu32)
            .filter_map(char::from_u32)
            .filter(|c| c.is_private_use() && is_printable_non_ascii(*c))
            .collect();
        assert!(
            leaked.is_empty(),
            "Co (Private Use) chars incorrectly marked printable: {:?}",
            leaked
                .iter()
                .map(|c| format!("U+{:04X}", *c as u32))
                .collect::<Vec<_>>()
        );
    }

    /// Zl (U+2028) and Zp (U+2029) must be escaped.
    #[test]
    fn property_zl_zp_escape() {
        assert!(
            !is_printable_non_ascii('\u{2028}'),
            "U+2028 LINE SEPARATOR (Zl) must be escaped"
        );
        assert!(
            !is_printable_non_ascii('\u{2029}'),
            "U+2029 PARAGRAPH SEPARATOR (Zp) must be escaped"
        );
    }

    /// A sample of L* / N* / P* / S* / M* chars must be preserved raw.
    /// These are the readable categories that idiomatic source may contain.
    #[test]
    fn property_readable_categories_preserved() {
        let readable_samples: &[(char, &str)] = &[
            // Letters (L*)
            ('é', "U+00E9 LATIN SMALL LETTER E WITH ACUTE (Ll)"),
            ('π', "U+03C0 GREEK SMALL LETTER PI (Ll)"),
            ('世', "U+4E16 CJK UNIFIED IDEOGRAPH-4E16 (Lo)"),
            ('Ñ', "U+00D1 LATIN CAPITAL LETTER N WITH TILDE (Lu)"),
            (
                'ǅ',
                "U+01C5 LATIN CAPITAL LETTER D WITH SMALL LETTER Z WITH CARON (Lt)",
            ),
            ('ˈ', "U+02C8 MODIFIER LETTER VERTICAL LINE (Lm)"),
            // Numbers (N*)
            ('²', "U+00B2 SUPERSCRIPT TWO (No)"),
            ('Ⅲ', "U+2162 ROMAN NUMERAL THREE (Nl)"),
            ('١', "U+0661 ARABIC-INDIC DIGIT ONE (Nd)"),
            // Punctuation (P*)
            ('«', "U+00AB LEFT-POINTING DOUBLE ANGLE QUOTATION MARK (Pi)"),
            (
                '»',
                "U+00BB RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK (Pf)",
            ),
            ('—', "U+2014 EM DASH (Pd)"),
            ('·', "U+00B7 MIDDLE DOT (Po)"),
            // Symbols (S*)
            ('©', "U+00A9 COPYRIGHT SIGN (So)"),
            ('→', "U+2192 RIGHTWARDS ARROW (Sm)"),
            ('£', "U+00A3 POUND SIGN (Sc)"),
            // Marks (M*)
            ('\u{0300}', "U+0300 COMBINING GRAVE ACCENT (Mn)"),
        ];
        for &(c, label) in readable_samples {
            assert!(
                is_printable_non_ascii(c),
                "{label} must be preserved raw (got escaped)"
            );
        }
    }

    // ── Default_Ignorable + blank-looking scalars (completeness) ─────────────

    /// Representatives of every `Default_Ignorable` range — plus the
    /// blank-looking Braille pattern — must escape, even the ones whose
    /// `General_Category` is readable (Mn/Lo/So) and would otherwise slip past
    /// the category gate.
    #[test]
    fn property_default_ignorable_and_blank_escape() {
        let must_escape: &[(char, &str)] = &[
            ('\u{00AD}', "U+00AD SOFT HYPHEN (Cf)"),
            (
                '\u{034F}',
                "U+034F COMBINING GRAPHEME JOINER (Mn, readable cat)",
            ),
            ('\u{061C}', "U+061C ARABIC LETTER MARK (Cf)"),
            ('\u{115F}', "U+115F HANGUL CHOSEONG FILLER (Lo, blank)"),
            ('\u{1160}', "U+1160 HANGUL JUNGSEONG FILLER (Lo, blank)"),
            ('\u{17B4}', "U+17B4 KHMER VOWEL INHERENT AQ (Cf)"),
            (
                '\u{180B}',
                "U+180B MONGOLIAN FREE VARIATION SELECTOR ONE (Mn)",
            ),
            ('\u{180E}', "U+180E MONGOLIAN VOWEL SEPARATOR (Cf)"),
            ('\u{200B}', "U+200B ZERO WIDTH SPACE (Cf)"),
            ('\u{200D}', "U+200D ZERO WIDTH JOINER (Cf)"),
            ('\u{202E}', "U+202E RIGHT-TO-LEFT OVERRIDE (Cf)"),
            ('\u{2060}', "U+2060 WORD JOINER (Cf)"),
            (
                '\u{2800}',
                "U+2800 BRAILLE PATTERN BLANK (So, blank-looking)",
            ),
            ('\u{3164}', "U+3164 HANGUL FILLER (Lo, blank)"),
            (
                '\u{FE0F}',
                "U+FE0F VARIATION SELECTOR-16 (Mn, readable cat)",
            ),
            ('\u{FE00}', "U+FE00 VARIATION SELECTOR-1 (Mn)"),
            ('\u{FEFF}', "U+FEFF ZERO WIDTH NO-BREAK SPACE / BOM (Cf)"),
            ('\u{FFA0}', "U+FFA0 HALFWIDTH HANGUL FILLER (Lo, blank)"),
            (
                '\u{1D173}',
                "U+1D173 MUSICAL SYMBOL BEGIN BEAM (Cf, supplementary)",
            ),
            (
                '\u{E0100}',
                "U+E0100 VARIATION SELECTOR-17 (Mn, supplement)",
            ),
            ('\u{E0001}', "U+E0001 LANGUAGE TAG (Cf, tag block)"),
        ];
        for &(c, label) in must_escape {
            assert!(
                !is_printable_non_ascii(c),
                "{label} must be escaped (got preserved)"
            );
            // The string-escape path must emit a \u{…} escape, never the raw char.
            let escaped = escape_string(&c.to_string());
            assert_eq!(
                escaped,
                format!("\\u{{{:X}}}", c as u32),
                "{label} must escape via escape_string"
            );
        }
    }

    /// Exhaustive: every scalar in the explicit `Default_Ignorable` ranges escapes.
    #[test]
    fn property_all_default_ignorable_ranges_escape() {
        let ranges: &[(u32, u32)] = &[
            (0x00AD, 0x00AD),
            (0x034F, 0x034F),
            (0x061C, 0x061C),
            (0x115F, 0x1160),
            (0x17B4, 0x17B5),
            (0x180B, 0x180F),
            (0x200B, 0x200F),
            (0x202A, 0x202E),
            (0x2060, 0x206F),
            (0x3164, 0x3164),
            (0xFE00, 0xFE0F),
            (0xFEFF, 0xFEFF),
            (0xFFA0, 0xFFA0),
            (0xFFF0, 0xFFF8),
            (0x1BCA0, 0x1BCA3),
            (0x1D173, 0x1D17A),
            (0xE0000, 0xE0FFF),
        ];
        let leaked: Vec<char> = ranges
            .iter()
            .flat_map(|&(lo, hi)| lo..=hi)
            .filter_map(char::from_u32)
            .filter(|c| !c.is_ascii() && is_printable_non_ascii(*c))
            .collect();
        assert!(
            leaked.is_empty(),
            "Default_Ignorable scalars incorrectly preserved: {:?}",
            leaked
                .iter()
                .map(|c| format!("U+{:04X}", *c as u32))
                .collect::<Vec<_>>()
        );
    }

    // ── char + regex literal escape parity (FIX 3: bypass surfaces) ──────────

    /// A char literal containing an invisible/confusable scalar must escape via
    /// the shared policy, not pass through raw.
    #[test]
    fn char_literal_escapes_invisible() {
        let mut out = String::new();
        escape_char_literal('\u{202E}', &mut out);
        assert_eq!(out, "\\u{202E}", "RLO in char literal must escape");

        out.clear();
        escape_char_literal('\u{FE0F}', &mut out);
        assert_eq!(out, "\\u{FE0F}", "VS-16 in char literal must escape");

        out.clear();
        escape_char_literal('\u{200B}', &mut out);
        assert_eq!(out, "\\u{200B}", "ZWSP in char literal must escape");
    }

    /// Char literals with readable Unicode and named escapes are preserved.
    #[test]
    fn char_literal_preserves_readable_and_named() {
        let cases: &[(char, &str)] = &[
            ('é', "é"),
            ('世', "世"),
            ('→', "→"),
            ('\n', "\\n"),
            ('\'', "\\'"),
            ('\\', "\\\\"),
            ('a', "a"),
        ];
        for &(c, expected) in cases {
            let mut out = String::new();
            escape_char_literal(c, &mut out);
            assert_eq!(out, expected, "char {c:?} formatted wrong");
        }
    }

    /// A char literal round-trips through parse → format with the invisible
    /// scalar escaped, and is idempotent.
    #[test]
    fn char_literal_invisible_round_trips_escaped() {
        let raw_src = "fn f() -> char {\n    '\u{202E}'\n}\n";
        let formatted = roundtrip(raw_src);
        assert!(
            formatted.contains("'\\u{202E}'"),
            "RLO char literal must format escaped; got: {formatted:?}"
        );
        let twice = roundtrip(&formatted);
        assert_eq!(twice, formatted, "char-literal escape must be idempotent");
    }

    /// A regex literal containing an invisible scalar must escape it to a
    /// `\u{…}` regex escape (semantically identical to the engine), while
    /// preserving readable Unicode and regex backslash escapes verbatim.
    #[test]
    fn regex_literal_escapes_invisible_preserves_escapes() {
        // Raw RLO inside the pattern → \u{202E}.
        assert_eq!(
            escape_regex_pattern("a\u{202E}b"),
            "a\\u{202E}b",
            "RLO in regex must escape to \\u{{202E}}"
        );
        // Readable Unicode preserved verbatim.
        assert_eq!(
            escape_regex_pattern("café→世"),
            "café→世",
            "readable Unicode in regex preserved"
        );
        // Regex backslash escapes preserved verbatim.
        assert_eq!(
            escape_regex_pattern("\\s+\\d*"),
            "\\s+\\d*",
            "regex backslash escapes preserved"
        );
        // Quote escaped, VS-16 escaped.
        assert_eq!(
            escape_regex_pattern("x\u{FE0F}\"y"),
            "x\\u{FE0F}\\\"y",
            "VS-16 escaped and quote escaped in regex"
        );
    }

    /// A regex literal with an invisible scalar round-trips through parse →
    /// format with the scalar escaped, and is idempotent.
    #[test]
    fn regex_literal_invisible_round_trips_escaped() {
        let raw_src = "fn f() {\n    let r = re\"a\u{202E}b\";\n}\n";
        let formatted = roundtrip(raw_src);
        assert!(
            formatted.contains("re\"a\\u{202E}b\""),
            "RLO regex literal must format escaped; got: {formatted:?}"
        );
        let twice = roundtrip(&formatted);
        assert_eq!(twice, formatted, "regex-literal escape must be idempotent");
    }

    // ── full parse/format round-trip for Unicode string literals ────────────

    /// A string literal containing readable Unicode must survive a parse/format
    /// cycle unchanged (formatter preserves raw Unicode rather than escaping it).
    #[test]
    fn string_literal_readable_unicode_round_trips() {
        let src = "fn greet() -> string {\n    \"café → résumé\"\n}\n";
        assert_eq!(roundtrip(src), src);
    }

    /// Idempotency: running the formatter twice on a source with readable Unicode
    /// must produce the same output both times.
    #[test]
    fn string_literal_readable_unicode_idempotent() {
        let src = "fn greet() -> string {\n    \"世界 — hello → world\"\n}\n";
        let once = roundtrip(src);
        assert_eq!(
            once, src,
            "first pass must preserve readable Unicode verbatim"
        );
        let twice = roundtrip(&once);
        assert_eq!(twice, once, "second pass must be idempotent");
    }

    /// A string literal containing an invisible Unicode character must have that
    /// character escaped after formatting, and remain stable on a second pass.
    #[test]
    fn string_literal_invisible_unicode_escaped_and_idempotent() {
        // Source contains a literal zero-width space (U+200B) inside the string.
        // After formatting, it must appear as \u{200B}.
        let raw_src = "fn test() -> string {\n    \"hello\u{200B}world\"\n}\n";
        let formatted = roundtrip(raw_src);
        assert!(
            formatted.contains("\\u{200B}"),
            "zero-width space must be escaped; got: {formatted:?}"
        );
        // Second pass must not change anything.
        let twice = roundtrip(&formatted);
        assert_eq!(twice, formatted, "must be idempotent after escaping");
    }

    /// f-string literals with readable Unicode survive round-trip.
    #[test]
    fn fstring_literal_readable_unicode_round_trips() {
        let src = "fn greet(name: string) -> string {\n    f\"bonjour {name} — café\"\n}\n";
        assert_eq!(roundtrip(src), src);
    }

    #[test]
    fn generic_supervisor_and_child_arguments_roundtrip() {
        let source = "supervisor Group<T: Send>(seed: Vec<T>) { child worker: module.Worker<Vec<T>>(value: seed), }";
        let formatted = roundtrip(source);
        let parsed = parse(&formatted);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let Item::Supervisor(supervisor) = &parsed.program.items[0].0 else {
            panic!("supervisor lost by formatting")
        };
        assert_eq!(supervisor.type_params[0].name, Ident::new("T"));
        assert_eq!(supervisor.type_params[0].bounds[0].path.to_string(), "Send");
        assert_eq!(
            supervisor.children[0].actor_type.to_string(),
            "module.Worker"
        );
        assert!(
            matches!(&supervisor.children[0].type_args[0].0, TypeExpr::Named { path, type_args: Some(args) } if path.to_string() == "Vec" && args.len() == 1)
        );
        assert_eq!(roundtrip(&formatted), formatted);
    }

    // ── supervisor config-param round-trip ───────────────────────────────────

    /// A supervisor with a config param must preserve the param and all
    /// config.field references in the body through `hew fmt`. The formatter
    /// previously dropped the `(config: T)` clause, silently breaking the body.
    #[test]
    fn supervisor_config_param_roundtrips() {
        let src = "\
type AppConfig { size: i64, label: string }

actor Cache {
    var capacity: i64,
    var name: string,
    receive fn get_cap(_n: i64) -> i64 {
        capacity;
        name.len()
    }
}

supervisor App(config: AppConfig) {
    strategy: one_for_one,
    intensity: 3 within 60s,

    child cache: Cache(capacity: config.size, name: config.label),
}

fn main() -> i64 {
    let cfg = AppConfig { size: 5, label: \"hi\" };
    let sup = spawn App(config: cfg);
    let _c = sup.cache;
    supervisor_stop(sup);
    0
}
";
        let formatted = roundtrip(src);
        assert!(
            formatted.contains("supervisor App(config: AppConfig)"),
            "hew fmt must preserve the supervisor config param; got:\n{formatted}"
        );
        assert!(
            formatted.contains("config.size"),
            "hew fmt must preserve config.size reference in body; got:\n{formatted}"
        );
        assert!(
            formatted.contains("config.label"),
            "hew fmt must preserve config.label reference in body; got:\n{formatted}"
        );
        // Idempotency: a second pass must not change the output.
        let twice = roundtrip(&formatted);
        assert_eq!(
            twice, formatted,
            "supervisor config-param format must be idempotent"
        );
    }

    /// A supervisor WITHOUT a config param must not emit an empty `()` clause —
    /// the formatter must be silent when there are no params.
    #[test]
    fn supervisor_without_config_param_no_parens() {
        let src = "\
supervisor Simple {
    strategy: one_for_one,
}
";
        let formatted = roundtrip(src);
        assert!(
            !formatted.contains("supervisor Simple("),
            "supervisor with no config params must not emit parens; got:\n{formatted}"
        );
    }

    #[test]
    fn transition_body_form_survives_head_binding_desugaring() {
        let src = "\
machine Socket {
    events {
        Connect { fd: i64, }
    }

    state Idle,
    state Active { h: Handle, },

    on Connect(fd): Idle => _ { Socket.Active { h: Handle { fd: fd } } }
    on Connect(fd): Active => Active { h: Handle { fd: fd } }
    on Connect(fd): Active => Active reenter { h: Handle { fd: fd }, ..state }
    on Connect(fd): Active => Idle,
}
";
        let formatted = roundtrip(src);
        assert!(
            formatted
                .contains("on Connect(fd): Idle => _ { Socket.Active { h: Handle { fd: fd } } }"),
            "computed-target block must retain its outer braces; got:\n{formatted}"
        );
        assert!(
            formatted.contains("on Connect(fd): Active => Active { h: Handle { fd: fd } }"),
            "payload shorthand must remain shorthand; got:\n{formatted}"
        );
        assert!(
            formatted.contains(
                "on Connect(fd): Active => Active reenter { ..state, h: Handle { fd: fd } }"
            ),
            "a transition field list writes its spread base first; got:\n{formatted}"
        );
        assert!(
            formatted.contains("on Connect(fd): Active => Idle,"),
            "implicit transition must remain implicit; got:\n{formatted}"
        );
        assert_eq!(roundtrip(&formatted), formatted);
    }

    #[test]
    fn impl_block_doc_comment_is_preserved() {
        let src = "\
enum Foo {
    A,
}

/// Doc comment for Foo Display.
impl Display for Foo {
    fn fmt(f: Foo) -> string {
        match f {
            .A => \"a\",
        }
    }
}
";
        let formatted = roundtrip(src);
        assert!(
            formatted.contains("/// Doc comment for Foo Display."),
            "doc comment on an impl block must survive formatting; got:\n{formatted}"
        );
        assert_eq!(roundtrip(&formatted), formatted);
    }
}
