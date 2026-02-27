//! Hand-written recursive-descent parser with Pratt precedence for operator expressions.

use crate::ast::{
    ActorDecl, ActorInit, Attribute, BinaryOp, Block, CallArg, ChildSpec, CompoundAssignOp,
    ConstDecl, ElseBlock, Expr, ExternBlock, ExternFnDecl, FieldDecl, FnDecl, ImplDecl, ImportDecl,
    ImportName, ImportSpec, IntRadix, Item, LambdaParam, Literal, MatchArm, NamingCase,
    OverflowFallback, OverflowPolicy, Param, Pattern, PatternField, Program, ReceiveFnDecl,
    RestartPolicy, SelectArm, Span, Spanned, Stmt, StringPart, SupervisorDecl, SupervisorStrategy,
    TimeoutClause, TraitBound, TraitDecl, TraitItem, TraitMethod, TypeAliasDecl, TypeBodyItem,
    TypeDecl, TypeDeclKind, TypeExpr, TypeParam, UnaryOp, VariantDecl, WhereClause, WherePredicate,
    WireDecl, WireDeclKind, WireFieldDecl,
};
use hew_lexer::Token;
use std::cell::Cell;

/// Parse an integer literal string, handling hex (`0x`), octal (`0o`), binary (`0b`)
/// prefixes and underscore separators.
fn parse_int_literal(s: &str) -> Result<i64, std::num::ParseIntError> {
    let cleaned: String = s.chars().filter(|c| *c != '_').collect();
    if let Some(hex) = cleaned
        .strip_prefix("0x")
        .or_else(|| cleaned.strip_prefix("0X"))
    {
        i64::from_str_radix(hex, 16)
    } else if let Some(oct) = cleaned
        .strip_prefix("0o")
        .or_else(|| cleaned.strip_prefix("0O"))
    {
        i64::from_str_radix(oct, 8)
    } else if let Some(bin) = cleaned
        .strip_prefix("0b")
        .or_else(|| cleaned.strip_prefix("0B"))
    {
        i64::from_str_radix(bin, 2)
    } else {
        cleaned.parse::<i64>()
    }
}

/// Detect the radix of an integer literal string from its prefix.
fn detect_int_radix(s: &str) -> IntRadix {
    let cleaned: String = s.chars().filter(|c| *c != '_').collect();
    if cleaned.starts_with("0x") || cleaned.starts_with("0X") {
        IntRadix::Hex
    } else if cleaned.starts_with("0o") || cleaned.starts_with("0O") {
        IntRadix::Octal
    } else if cleaned.starts_with("0b") || cleaned.starts_with("0B") {
        IntRadix::Binary
    } else {
        IntRadix::Decimal
    }
}

/// Parse a duration literal string (e.g. "100ns", "5s") into nanoseconds.
fn parse_duration_literal(s: &str) -> Option<i64> {
    let s = &s.replace('_', "");
    if let Some(num) = s.strip_suffix("ns") {
        num.parse::<i64>().ok()
    } else if let Some(num) = s.strip_suffix("us") {
        num.parse::<i64>().ok().and_then(|v| v.checked_mul(1_000))
    } else if let Some(num) = s.strip_suffix("ms") {
        num.parse::<i64>()
            .ok()
            .and_then(|v| v.checked_mul(1_000_000))
    } else if let Some(num) = s.strip_suffix('h') {
        num.parse::<i64>()
            .ok()
            .and_then(|v| v.checked_mul(3_600_000_000_000))
    } else if let Some(num) = s.strip_suffix('m') {
        num.parse::<i64>()
            .ok()
            .and_then(|v| v.checked_mul(60_000_000_000))
    } else if let Some(num) = s.strip_suffix('s') {
        num.parse::<i64>()
            .ok()
            .and_then(|v| v.checked_mul(1_000_000_000))
    } else {
        None
    }
}

/// Process escape sequences in a string literal, converting `\n`, `\t`, `\r`,
/// `\\`, `\"`, and `\0` to their corresponding characters.
fn unescape_string(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    let mut chars = s.chars();
    while let Some(c) = chars.next() {
        if c == '\\' {
            match chars.next() {
                Some('n') => out.push('\n'),
                Some('t') => out.push('\t'),
                Some('r') => out.push('\r'),
                Some('"') => out.push('"'),
                Some('0') => out.push('\0'),
                Some('x') => {
                    // \xNN hex escape
                    let hi = chars.next();
                    let lo = chars.next();
                    if let (Some(h), Some(l)) = (hi, lo) {
                        if let Ok(byte) = u8::from_str_radix(&format!("{}{}", h, l), 16) {
                            out.push(byte as char);
                        } else {
                            out.push('\\');
                            out.push('x');
                            out.push(h);
                            out.push(l);
                        }
                    } else {
                        out.push('\\');
                        out.push('x');
                    }
                }
                Some('\\') | None => out.push('\\'),
                Some(other) => {
                    // Unknown escape: preserve as-is
                    out.push('\\');
                    out.push(other);
                }
            }
        } else {
            out.push(c);
        }
    }
    out
}

/// Split an interpolated string (f-string or template literal) into literal
/// segments and parsed expression segments.
///
/// * `raw` — the full token text including delimiters (e.g. `f"hello {x}"`)
/// * `prefix_len` — bytes to strip from the front (2 for `f"`, 1 for `` ` ``)
/// * `suffix_len` — bytes to strip from the end (1 for `"` or `` ` ``)
/// * `expr_open` — the marker that opens an expression (`"{"` or `"${"`)
/// * `span_start` — byte offset of the token in the original source
fn parse_string_parts(
    raw: &str,
    prefix_len: usize,
    suffix_len: usize,
    expr_open: &str,
    span_start: usize,
) -> Vec<StringPart> {
    let inner = &raw[prefix_len..raw.len() - suffix_len];
    let inner_offset = span_start + prefix_len;
    let mut parts = Vec::new();
    let mut literal_buf = String::new();
    let chars: Vec<(usize, char)> = inner.char_indices().collect();
    let mut idx = 0;

    while idx < chars.len() {
        let (byte_pos, c) = chars[idx];

        // Handle escape sequences — prevents `\{` or `\$` from opening an expr
        if c == '\\' && idx + 1 < chars.len() {
            let (_, next) = chars[idx + 1];
            match next {
                'n' => literal_buf.push('\n'),
                't' => literal_buf.push('\t'),
                'r' => literal_buf.push('\r'),
                '"' => literal_buf.push('"'),
                '0' => literal_buf.push('\0'),
                'x' if idx + 3 < chars.len() => {
                    let (_, h) = chars[idx + 2];
                    let (_, l) = chars[idx + 3];
                    if let Ok(byte) = u8::from_str_radix(&format!("{}{}", h, l), 16) {
                        literal_buf.push(byte as char);
                    } else {
                        literal_buf.push('\\');
                        literal_buf.push('x');
                        literal_buf.push(h);
                        literal_buf.push(l);
                    }
                    idx += 4;
                    continue;
                }
                '\\' => literal_buf.push('\\'),
                '{' => literal_buf.push('{'),
                '$' => literal_buf.push('$'),
                '`' => literal_buf.push('`'),
                other => {
                    literal_buf.push('\\');
                    literal_buf.push(other);
                }
            }
            idx += 2;
            continue;
        }

        // Check for expression opening marker
        if inner[byte_pos..].starts_with(expr_open) {
            // Flush accumulated literal text
            if !literal_buf.is_empty() {
                parts.push(StringPart::Literal(std::mem::take(&mut literal_buf)));
            }

            let open_char_len = expr_open.chars().count();
            idx += open_char_len;

            let expr_start_byte = if idx < chars.len() {
                chars[idx].0
            } else {
                inner.len()
            };

            // Scan for matching `}`, respecting nested braces and string literals
            let mut depth: u32 = 1;
            while idx < chars.len() {
                let ch = chars[idx].1;
                match ch {
                    '{' => depth += 1,
                    '}' => {
                        depth -= 1;
                        if depth == 0 {
                            break;
                        }
                    }
                    '"' => {
                        // Skip over string literals inside the expression
                        idx += 1;
                        while idx < chars.len() && chars[idx].1 != '"' {
                            if chars[idx].1 == '\\' {
                                idx += 1;
                            }
                            idx += 1;
                        }
                    }
                    _ => {}
                }
                idx += 1;
            }

            let expr_end_byte = if idx < chars.len() {
                chars[idx].0
            } else {
                inner.len()
            };

            // Skip the closing `}`
            if idx < chars.len() {
                idx += 1;
            }

            let expr_text = &inner[expr_start_byte..expr_end_byte];
            if !expr_text.is_empty() {
                let mut sub_parser = Parser::new(expr_text);
                if let Some((expr, sub_span)) = sub_parser.parse_expr() {
                    let adjusted_start = inner_offset + expr_start_byte + sub_span.start;
                    let adjusted_end = inner_offset + expr_start_byte + sub_span.end;
                    parts.push(StringPart::Expr((expr, adjusted_start..adjusted_end)));
                }
            }
            continue;
        }

        literal_buf.push(c);
        idx += 1;
    }

    if !literal_buf.is_empty() {
        parts.push(StringPart::Literal(literal_buf));
    }

    parts
}

/// Maximum nesting depth for recursive parse functions.
const MAX_DEPTH: usize = 256;

/// RAII guard that decrements the parser recursion depth on drop.
///
/// Uses a raw pointer to avoid holding a borrow on the `Parser` struct,
/// which would conflict with the mutable borrows needed by parse methods.
#[derive(Debug)]
struct RecursionGuard(*const Cell<usize>);

impl Drop for RecursionGuard {
    fn drop(&mut self) {
        // SAFETY: The pointer targets `Parser::depth`, which lives at least as long
        // as any `RecursionGuard` created from it (guards are local variables in
        // parser methods that take `&mut self`).
        let cell = unsafe { &*self.0 };
        cell.set(cell.get() - 1);
    }
}

/// Snapshot of parser position for speculative (backtracking) parses.
struct SavedPos {
    pos: usize,
    error_count: usize,
    angle_mutation_count: usize,
}

/// Parser state wrapping a token stream.
#[derive(Debug)]
pub struct Parser<'src> {
    tokens: Vec<(Token<'src>, Span)>,
    pos: usize,
    errors: Vec<ParseError>,
    depth: Cell<usize>,
    /// When inside a `scope |s| { ... }` block, this holds the binding name "s"
    /// so that `s.launch`, `s.cancel()` can be desugared
    /// to the corresponding AST nodes.
    scope_binding: Option<String>,
    /// Stack of token mutations performed by `eat_closing_angle`, so they can
    /// be rolled back on speculative-parse backtrack.
    angle_mutations: Vec<(usize, (Token<'src>, Span))>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Severity {
    Error,
    Warning,
}

#[derive(Debug, Clone)]
pub struct ParseError {
    pub message: String,
    pub span: Span,
    /// Optional actionable suggestion for fixing the error.
    pub hint: Option<String>,
    pub severity: Severity,
}

#[derive(Debug)]
pub struct ParseResult {
    pub program: Program,
    pub errors: Vec<ParseError>,
}

impl<'src> Parser<'src> {
    #[must_use]
    pub fn new(source: &'src str) -> Self {
        let raw_tokens = hew_lexer::lex(source);
        let mut errors = Vec::new();
        let mut tokens = Vec::new();
        for (t, s) in raw_tokens {
            let span = s.start..s.end;
            if matches!(t, Token::Error) {
                errors.push(ParseError {
                    message: "unexpected character".to_string(),
                    span,
                    hint: None,
                    severity: Severity::Error,
                });
            } else {
                tokens.push((t, span));
            }
        }
        Self {
            tokens,
            pos: 0,
            errors,
            depth: Cell::new(0),
            scope_binding: None,
            angle_mutations: Vec::new(),
        }
    }

    // ── Helpers ──
    fn peek(&self) -> Option<&Token<'src>> {
        self.tokens.get(self.pos).map(|(t, _)| t)
    }

    fn peek_span(&self) -> Span {
        self.tokens.get(self.pos).map_or(0..0, |(_, s)| s.clone())
    }

    fn advance(&mut self) -> Option<(Token<'src>, Span)> {
        if self.pos < self.tokens.len() {
            let tok = self.tokens[self.pos].clone();
            self.pos += 1;
            Some(tok)
        } else {
            None
        }
    }

    fn expect(&mut self, expected: &Token<'_>) -> Option<Span> {
        if let Some(tok) = self.peek() {
            if std::mem::discriminant(tok) == std::mem::discriminant(expected) {
                let (_, span) = self.advance().unwrap();
                return Some(span);
            }
        }
        let found = match self.peek() {
            Some(tok) => format!("{tok}"),
            None => "end of file".to_string(),
        };
        // Add a hint when a semicolon is expected but a statement keyword follows
        if matches!(expected, Token::Semicolon) && self.peek_starts_stmt() {
            self.error_with_hint(
                format!("expected {expected}, found {found}"),
                "add `;` at the end of the previous statement",
            );
        } else {
            self.error(format!("expected {expected}, found {found}"));
        }
        None
    }

    fn eat(&mut self, expected: &Token<'_>) -> bool {
        if let Some(tok) = self.peek() {
            if std::mem::discriminant(tok) == std::mem::discriminant(expected) {
                self.advance();
                return true;
            }
        }
        false
    }

    /// Returns true if the current token could start a new statement.
    fn peek_starts_stmt(&self) -> bool {
        matches!(
            self.peek(),
            Some(
                Token::Let
                    | Token::Var
                    | Token::If
                    | Token::For
                    | Token::While
                    | Token::Loop
                    | Token::Return
                    | Token::Break
                    | Token::Continue
                    | Token::Defer
                    | Token::Spawn
                    | Token::Import
                    | Token::Fn
            )
        )
    }

    fn at_end(&self) -> bool {
        self.pos >= self.tokens.len()
    }

    /// Peek at the token at an absolute position in the token stream.
    fn peek_at(&self, index: usize) -> Option<&Token<'src>> {
        self.tokens.get(index).map(|(t, _)| t)
    }

    /// Check whether the current token starts with `>` (i.e. is `>`, `>>`, `>=`, or `>>=`).
    /// Used in type-argument / type-parameter parsing so that `Vec<Vec<i32>>`
    /// works without requiring a space before `>>`.
    fn at_closing_angle(&self) -> bool {
        matches!(
            self.peek(),
            Some(
                Token::Greater
                    | Token::GreaterGreater
                    | Token::GreaterEqual
                    | Token::GreaterGreaterEqual
            )
        )
    }

    /// Consume a single `>` from the current token, splitting compound tokens
    /// (`>>`, `>=`, `>>=`) as needed.  Returns `true` if a `>` was consumed.
    fn eat_closing_angle(&mut self) -> bool {
        let Some((tok, span)) = self.tokens.get(self.pos) else {
            return false;
        };
        match tok {
            Token::Greater => {
                self.pos += 1;
                true
            }
            Token::GreaterGreater => {
                // `>>` → consume first `>`, leave `>` for the outer context
                self.angle_mutations
                    .push((self.pos, self.tokens[self.pos].clone()));
                let mid = span.start + 1;
                let remaining_span = mid..span.end;
                self.tokens[self.pos] = (Token::Greater, remaining_span);
                true
            }
            Token::GreaterEqual => {
                // `>=` → consume `>`, leave `=`
                self.angle_mutations
                    .push((self.pos, self.tokens[self.pos].clone()));
                let mid = span.start + 1;
                let remaining_span = mid..span.end;
                self.tokens[self.pos] = (Token::Equal, remaining_span);
                true
            }
            Token::GreaterGreaterEqual => {
                // `>>=` → consume first `>`, leave `>=`
                self.angle_mutations
                    .push((self.pos, self.tokens[self.pos].clone()));
                let mid = span.start + 1;
                let remaining_span = mid..span.end;
                self.tokens[self.pos] = (Token::GreaterEqual, remaining_span);
                true
            }
            _ => false,
        }
    }

    fn error(&mut self, message: String) {
        let span = self.peek_span();
        self.errors.push(ParseError {
            message,
            span,
            hint: None,
            severity: Severity::Error,
        });
    }

    fn error_with_hint(&mut self, message: String, hint: impl Into<String>) {
        let span = self.peek_span();
        self.errors.push(ParseError {
            message,
            span,
            hint: Some(hint.into()),
            severity: Severity::Error,
        });
    }

    fn warning_at(&mut self, message: String, span: Span) {
        self.errors.push(ParseError {
            message,
            span,
            hint: None,
            severity: Severity::Warning,
        });
    }

    /// Increment recursion depth and return a guard that decrements on drop.
    /// Returns `None` (after recording an error) if `MAX_DEPTH` is exceeded.
    fn enter_recursion(&mut self) -> Option<RecursionGuard> {
        let d = self.depth.get() + 1;
        self.depth.set(d);
        if d > MAX_DEPTH {
            self.error("maximum nesting depth exceeded".to_string());
            self.depth.set(d - 1);
            return None;
        }
        Some(RecursionGuard(std::ptr::from_ref(&self.depth)))
    }

    /// If the token is a contextual keyword, return its identifier name.
    fn contextual_keyword_name(tok: &Token<'_>) -> Option<&'static str> {
        match tok {
            Token::After => Some("after"),
            Token::From => Some("from"),
            Token::Init => Some("init"),
            Token::Child => Some("child"),
            Token::Restart => Some("restart"),
            Token::Budget => Some("budget"),
            Token::Strategy => Some("strategy"),
            Token::Permanent => Some("permanent"),
            Token::Transient => Some("transient"),
            Token::Temporary => Some("temporary"),
            Token::OneForOne => Some("one_for_one"),
            Token::OneForAll => Some("one_for_all"),
            Token::RestForOne => Some("rest_for_one"),
            Token::Wire => Some("wire"),
            Token::Optional => Some("optional"),
            Token::Deprecated => Some("deprecated"),
            Token::Reserved => Some("reserved"),
            _ => None,
        }
    }

    /// Returns true if the token can be used as an identifier (regular or contextual keyword).
    fn is_ident_token(tok: &Token<'_>) -> bool {
        matches!(tok, Token::Identifier(_)) || Self::contextual_keyword_name(tok).is_some()
    }

    fn expect_ident(&mut self) -> Option<String> {
        match self.peek() {
            Some(Token::Identifier(name)) => {
                let name = name.to_string();
                self.advance();
                Some(name)
            }
            Some(tok) if Self::contextual_keyword_name(tok).is_some() => {
                let name = Self::contextual_keyword_name(self.peek().unwrap()).unwrap();
                self.advance();
                Some(name.to_string())
            }
            _ => {
                let found = match self.peek() {
                    Some(tok) => format!("{tok}"),
                    None => "end of file".to_string(),
                };
                self.error(format!("expected identifier, found {found}"));
                None
            }
        }
    }

    fn save_pos(&self) -> SavedPos {
        SavedPos {
            pos: self.pos,
            error_count: self.errors.len(),
            angle_mutation_count: self.angle_mutations.len(),
        }
    }

    fn restore_pos(&mut self, saved: SavedPos) {
        self.pos = saved.pos;
        self.errors.truncate(saved.error_count);
        // Undo any token mutations made by eat_closing_angle since this save point
        while self.angle_mutations.len() > saved.angle_mutation_count {
            let (idx, tok) = self.angle_mutations.pop().unwrap();
            self.tokens[idx] = tok;
        }
    }

    /// Collect consecutive doc comment tokens with the given prefix and return
    /// the concatenated content, or `None` if no matching comments are present.
    fn collect_doc_comments_with_prefix(
        &mut self,
        prefix: &str,
        is_match: fn(&Token<'src>) -> Option<&'src str>,
    ) -> Option<String> {
        let mut lines = Vec::new();
        while let Some(s) = self.peek().and_then(is_match) {
            let content = s.strip_prefix(prefix).unwrap_or(s);
            // Strip one leading space if present (conventional formatting)
            let content = content.strip_prefix(' ').unwrap_or(content);
            lines.push(content.to_string());
            self.advance();
        }
        if lines.is_empty() {
            None
        } else {
            Some(lines.join("\n"))
        }
    }

    /// Collect consecutive outer doc comment (`///`) tokens and return
    /// the concatenated content, or `None` if no doc comments are present.
    fn collect_doc_comments(&mut self) -> Option<String> {
        self.collect_doc_comments_with_prefix("///", |t| match t {
            Token::DocComment(s) => Some(s),
            _ => None,
        })
    }

    /// Collect consecutive inner doc comment (`//!`) tokens at the start of
    /// the file and return the concatenated content.
    fn collect_inner_doc_comments(&mut self) -> Option<String> {
        self.collect_doc_comments_with_prefix("//!", |t| match t {
            Token::InnerDocComment(s) => Some(s),
            _ => None,
        })
    }

    // ── Program and Items ──
    pub fn parse_program(&mut self) -> Program {
        let mut items = Vec::new();

        // Collect inner doc comments (`//!`) at the start of the file
        let module_doc = self.collect_inner_doc_comments();

        while !self.at_end() {
            // Skip any inner doc comments that appear between items
            while matches!(self.peek(), Some(Token::InnerDocComment(_))) {
                self.advance();
            }
            if self.at_end() {
                break;
            }
            if let Some(item) = self.parse_item() {
                items.push(item);
            } else {
                // Skip to next item on error
                self.advance();
            }
        }

        Program {
            items,
            module_doc,
            module_graph: None,
        }
    }

    /// Parse zero or more `#[name]` or `#[name(arg1, arg2)]` attributes.
    fn parse_attributes(&mut self) -> Vec<Attribute> {
        let mut attrs = Vec::new();
        while self.peek() == Some(&Token::HashBracket) {
            let start = self.peek_span().start;
            self.advance(); // consume `#[`
            let Some(name) = self.expect_ident() else {
                break;
            };
            let mut args = Vec::new();
            if self.eat(&Token::LeftParen) {
                while self.peek() != Some(&Token::RightParen) && !self.at_end() {
                    if self.peek().is_some_and(|tok| Self::is_ident_token(tok)) {
                        // Safe to call: we know the token is identifier-like
                        args.push(self.expect_ident().unwrap_or_default());
                    } else if let Some(Token::StringLit(s) | Token::RawString(s)) = self.peek() {
                        let val = s
                            .strip_prefix("r\"")
                            .or_else(|| s.strip_prefix('"'))
                            .and_then(|s| s.strip_suffix('"'))
                            .unwrap_or(s)
                            .to_string();
                        self.advance();
                        args.push(val);
                    } else {
                        break;
                    }
                    if !self.eat(&Token::Comma) {
                        break;
                    }
                }
                let _ = self.expect(&Token::RightParen);
            }
            let end = self.peek_span().start;
            let _ = self.expect(&Token::RightBracket);
            let end = self.peek_span().start.max(end);
            attrs.push(Attribute {
                name,
                args,
                span: start..end,
            });
        }
        attrs
    }

    #[expect(clippy::too_many_lines, reason = "parser function with many branches")]
    fn parse_item(&mut self) -> Option<Spanned<Item>> {
        // Collect any outer doc comments (`///`) and attributes before this item.
        // Support both orderings: `/// docs #[attr]` and `#[attr] /// docs`.
        let mut doc_comment = self.collect_doc_comments();
        let attrs = self.parse_attributes();
        if doc_comment.is_none() {
            doc_comment = self.collect_doc_comments();
        }
        let start = self.peek_span().start;
        // Pre-compute attribute span before attrs is moved into the item.
        let attr_start = attrs.first().map(|a| a.span.start);

        let item = match self.peek() {
            Some(Token::Import) => {
                self.advance();
                Item::Import(self.parse_import()?)
            }
            Some(Token::Const) => {
                self.advance();
                Item::Const(self.parse_const_decl(false)?)
            }
            Some(Token::Pub) => {
                self.advance();
                let is_pure = self.eat(&Token::Pure);
                match self.peek() {
                    Some(Token::Fn) => {
                        self.advance();
                        let mut f = self.parse_function(false, false, true, is_pure, attrs)?;
                        f.doc_comment.clone_from(&doc_comment);
                        Item::Function(f)
                    }
                    Some(Token::Async) => {
                        self.advance();
                        if self.eat(&Token::Gen) {
                            if self.eat(&Token::Fn) {
                                let mut f =
                                    self.parse_function(true, true, true, is_pure, attrs)?;
                                f.doc_comment.clone_from(&doc_comment);
                                Item::Function(f)
                            } else {
                                self.error("expected 'fn' after 'async gen'".to_string());
                                return None;
                            }
                        } else if self.eat(&Token::Fn) {
                            let mut f = self.parse_function(true, false, true, is_pure, attrs)?;
                            f.doc_comment.clone_from(&doc_comment);
                            Item::Function(f)
                        } else {
                            self.error("expected 'fn' or 'gen fn' after 'async'".to_string());
                            return None;
                        }
                    }
                    Some(Token::Gen) => {
                        self.advance();
                        if self.eat(&Token::Fn) {
                            let mut f = self.parse_function(false, true, true, is_pure, attrs)?;
                            f.doc_comment.clone_from(&doc_comment);
                            Item::Function(f)
                        } else {
                            self.error("expected 'fn' after 'gen'".to_string());
                            return None;
                        }
                    }
                    Some(Token::Struct) => {
                        self.error("use 'type' instead of 'struct' to declare types".to_string());
                        return None;
                    }
                    Some(Token::Enum) => {
                        let mut t = self.parse_struct_or_enum(true)?;
                        t.doc_comment = doc_comment;
                        Item::TypeDecl(t)
                    }
                    Some(Token::Type) => {
                        if self.is_type_alias_lookahead() {
                            Item::TypeAlias(self.parse_type_alias(true)?)
                        } else {
                            let mut t = self.parse_struct_or_enum(true)?;
                            t.doc_comment = doc_comment;
                            Item::TypeDecl(t)
                        }
                    }
                    Some(Token::Trait) => {
                        self.advance();
                        let mut t = self.parse_trait_decl(true)?;
                        t.doc_comment = doc_comment;
                        Item::Trait(t)
                    }
                    Some(Token::Actor) => {
                        self.advance();
                        let mut a = self.parse_actor_decl(true)?;
                        a.doc_comment = doc_comment;
                        Item::Actor(a)
                    }
                    Some(Token::Supervisor) => {
                        self.advance();
                        Item::Supervisor(self.parse_supervisor_decl(true)?)
                    }
                    Some(Token::Wire) => {
                        self.advance();
                        Item::Wire(self.parse_wire_decl(&attrs, true)?)
                    }
                    Some(Token::Const) => {
                        self.advance();
                        Item::Const(self.parse_const_decl(true)?)
                    }
                    _ => {
                        if is_pure {
                            self.error(
                                "'pure' can only be applied to function declarations".to_string(),
                            );
                        } else {
                            self.error("invalid item after 'pub'".to_string());
                        }
                        return None;
                    }
                }
            }
            Some(Token::Fn) => {
                self.advance();
                let mut f = self.parse_function(false, false, false, false, attrs)?;
                f.doc_comment.clone_from(&doc_comment);
                Item::Function(f)
            }
            Some(Token::Pure) => {
                self.advance();
                match self.peek() {
                    Some(Token::Fn) => {
                        self.advance();
                        let mut f = self.parse_function(false, false, false, true, attrs)?;
                        f.doc_comment.clone_from(&doc_comment);
                        Item::Function(f)
                    }
                    Some(Token::Gen) => {
                        self.advance();
                        if self.eat(&Token::Fn) {
                            let mut f = self.parse_function(false, true, false, true, attrs)?;
                            f.doc_comment.clone_from(&doc_comment);
                            Item::Function(f)
                        } else {
                            self.error("expected 'fn' after 'pure gen'".to_string());
                            return None;
                        }
                    }
                    _ => {
                        self.error(
                            "'pure' can only be applied to function declarations".to_string(),
                        );
                        return None;
                    }
                }
            }
            Some(Token::Async) => {
                self.advance();
                if self.eat(&Token::Gen) {
                    if self.eat(&Token::Fn) {
                        let mut f = self.parse_function(true, true, false, false, attrs)?;
                        f.doc_comment.clone_from(&doc_comment);
                        Item::Function(f)
                    } else {
                        self.error("expected 'fn' after 'async gen'".to_string());
                        return None;
                    }
                } else if self.eat(&Token::Fn) {
                    let mut f = self.parse_function(true, false, false, false, attrs)?;
                    f.doc_comment.clone_from(&doc_comment);
                    Item::Function(f)
                } else {
                    self.error("expected 'fn' or 'gen fn' after 'async'".to_string());
                    return None;
                }
            }
            Some(Token::Gen) => {
                self.advance();
                if self.eat(&Token::Fn) {
                    let mut f = self.parse_function(false, true, false, false, attrs)?;
                    f.doc_comment.clone_from(&doc_comment);
                    Item::Function(f)
                } else {
                    self.error("expected 'fn' after 'gen'".to_string());
                    return None;
                }
            }
            Some(Token::Struct) => {
                self.error("use 'type' instead of 'struct' to declare types".to_string());
                return None;
            }
            Some(Token::Enum) => {
                let mut t = self.parse_struct_or_enum(false)?;
                t.doc_comment = doc_comment;
                Item::TypeDecl(t)
            }
            Some(Token::Type) => {
                if self.is_type_alias_lookahead() {
                    Item::TypeAlias(self.parse_type_alias(false)?)
                } else {
                    let mut t = self.parse_struct_or_enum(false)?;
                    t.doc_comment = doc_comment;
                    Item::TypeDecl(t)
                }
            }
            Some(Token::Trait) => {
                self.advance();
                let mut t = self.parse_trait_decl(false)?;
                t.doc_comment = doc_comment;
                Item::Trait(t)
            }
            Some(Token::Impl) => {
                self.advance();
                Item::Impl(self.parse_impl_decl()?)
            }
            Some(Token::Wire) => {
                self.advance();
                Item::Wire(self.parse_wire_decl(&attrs, false)?)
            }
            Some(Token::Actor) => {
                self.advance();
                let mut a = self.parse_actor_decl(false)?;
                a.doc_comment = doc_comment;
                Item::Actor(a)
            }
            Some(Token::Supervisor) => {
                self.advance();
                Item::Supervisor(self.parse_supervisor_decl(false)?)
            }
            Some(Token::Extern) => {
                self.advance();
                Item::ExternBlock(self.parse_extern_block()?)
            }
            Some(Token::Foreign) => {
                self.error_with_hint(
                    "unexpected 'foreign'".to_string(),
                    "use 'extern' instead of 'foreign'",
                );
                return None;
            }
            _ => {
                let found = match self.peek() {
                    Some(tok) => format!("{tok}"),
                    None => "end of file".to_string(),
                };
                // Detect common keywords from other languages
                if let Some(Token::Identifier(id)) = self.peek() {
                    match *id {
                        "struct" | "record" => {
                            self.error_with_hint(
                                format!("unexpected '{id}'"),
                                "Hew uses 'type' to declare structs: type Name { ... }",
                            );
                            return None;
                        }
                        "class" | "object" => {
                            self.error_with_hint(
                                format!("unexpected '{id}'"),
                                "Hew uses 'actor' for stateful objects: actor Name { ... }",
                            );
                            return None;
                        }
                        "func" | "function" | "def" | "sub" | "proc" | "method" => {
                            self.error_with_hint(
                                format!("unexpected '{id}'"),
                                "Hew uses 'fn' to declare functions: fn name() { ... }",
                            );
                            return None;
                        }
                        "interface" | "protocol" => {
                            self.error_with_hint(
                                format!("unexpected '{id}'"),
                                "Hew uses 'trait' to declare interfaces: trait Name { ... }",
                            );
                            return None;
                        }
                        _ => {}
                    }
                }
                self.error(format!(
                    "expected item (fn, actor, type, import, ...), found {found}"
                ));
                return None;
            }
        };

        let end = self.peek_span().start;
        // Extend span to cover leading attributes if present.
        let item_start = attr_start.unwrap_or(start);
        Some((item, item_start..end))
    }

    fn parse_function(
        &mut self,
        is_async: bool,
        is_gen: bool,
        is_pub: bool,
        is_pure: bool,
        attributes: Vec<Attribute>,
    ) -> Option<FnDecl> {
        let name = self.expect_ident()?;

        let type_params = if self.eat(&Token::Less) {
            Some(self.parse_type_params()?)
        } else {
            None
        };

        self.expect(&Token::LeftParen)?;
        let params = self.parse_params();
        self.expect(&Token::RightParen)?;

        let return_type = if self.eat(&Token::Arrow) {
            Some(self.parse_type()?)
        } else {
            None
        };

        let where_clause = if self.peek() == Some(&Token::Where) {
            self.advance();
            Some(self.parse_where_clause()?)
        } else {
            None
        };

        let body = self.parse_block()?;

        Some(FnDecl {
            attributes,
            is_async,
            is_generator: is_gen,
            is_pub,
            is_pure,
            name,
            type_params,
            params,
            return_type,
            where_clause,
            body,
            doc_comment: None,
        })
    }

    fn is_type_alias_lookahead(&self) -> bool {
        // Check for "type Name =" pattern (Name can be a contextual keyword)
        matches!(self.tokens.get(self.pos), Some((Token::Type, _)))
            && self
                .tokens
                .get(self.pos + 1)
                .is_some_and(|(tok, _)| Self::is_ident_token(tok))
            && matches!(self.tokens.get(self.pos + 2), Some((Token::Equal, _)))
    }

    fn parse_type_alias(&mut self, is_pub: bool) -> Option<TypeAliasDecl> {
        self.expect(&Token::Type)?;
        let name = self.expect_ident()?;
        self.expect(&Token::Equal)?;
        let ty = self.parse_type()?;
        self.expect(&Token::Semicolon)?;
        Some(TypeAliasDecl { is_pub, name, ty })
    }

    fn parse_struct_or_enum(&mut self, is_pub: bool) -> Option<TypeDecl> {
        let kind = match self.peek() {
            Some(Token::Type) => {
                self.advance();
                TypeDeclKind::Struct
            }
            Some(Token::Enum) => {
                self.advance();
                TypeDeclKind::Enum
            }
            _ => return None,
        };

        let name = self.expect_ident()?;

        let type_params = if self.eat(&Token::Less) {
            Some(self.parse_type_params()?)
        } else {
            None
        };

        let where_clause = if self.peek() == Some(&Token::Where) {
            self.advance();
            Some(self.parse_where_clause()?)
        } else {
            None
        };

        self.expect(&Token::LeftBrace)?;

        let mut body = Vec::new();
        while !self.at_end() && self.peek() != Some(&Token::RightBrace) {
            if let Some(item) = self.parse_type_body_item(kind) {
                body.push(item);
            } else {
                let found = match self.peek() {
                    Some(tok) => format!("{tok}"),
                    None => "end of file".to_string(),
                };
                self.error(format!("unexpected {found} in type body"));
                self.advance();
            }
        }

        self.expect(&Token::RightBrace)?;

        Some(TypeDecl {
            is_pub,
            kind,
            name,
            type_params,
            where_clause,
            body,
            doc_comment: None,
        })
    }

    fn parse_type_body_item(&mut self, kind: TypeDeclKind) -> Option<TypeBodyItem> {
        match kind {
            TypeDeclKind::Struct => {
                if self.peek() == Some(&Token::Fn) {
                    self.advance();
                    Some(TypeBodyItem::Method(self.parse_function(
                        false,
                        false,
                        false,
                        false,
                        Vec::new(),
                    )?))
                } else {
                    // Field
                    let name = self.expect_ident()?;
                    self.expect(&Token::Colon)?;
                    let ty = self.parse_type()?;
                    if !self.eat(&Token::Semicolon) && self.peek() == Some(&Token::Comma) {
                        self.error("use `;` instead of `,` to separate fields".to_string());
                        self.advance();
                    }
                    Some(TypeBodyItem::Field { name, ty })
                }
            }
            TypeDeclKind::Enum => {
                // Enum variant
                let name = self.expect_ident()?;
                let mut fields = Vec::new();

                if self.eat(&Token::LeftParen) {
                    while !self.at_end() && self.peek() != Some(&Token::RightParen) {
                        fields.push(self.parse_type()?);
                        if !self.eat(&Token::Comma) {
                            break;
                        }
                    }
                    self.expect(&Token::RightParen)?;
                }

                if !self.eat(&Token::Semicolon) && self.peek() == Some(&Token::Comma) {
                    self.error("use `;` instead of `,` to separate variants".to_string());
                    self.advance();
                }
                Some(TypeBodyItem::Variant(VariantDecl { name, fields }))
            }
        }
    }

    fn parse_trait_decl(&mut self, is_pub: bool) -> Option<TraitDecl> {
        let name = self.expect_ident()?;

        let type_params = if self.eat(&Token::Less) {
            Some(self.parse_type_params()?)
        } else {
            None
        };

        let super_traits = if self.eat(&Token::Colon) {
            let mut bounds = Vec::new();
            loop {
                bounds.push(self.parse_trait_bound()?);
                if !self.eat(&Token::Plus) {
                    break;
                }
            }
            Some(bounds)
        } else {
            None
        };

        self.expect(&Token::LeftBrace)?;

        let mut items = Vec::new();
        while !self.at_end() && self.peek() != Some(&Token::RightBrace) {
            if let Some(item) = self.parse_trait_item() {
                items.push(item);
            } else {
                self.advance();
            }
        }

        self.expect(&Token::RightBrace)?;

        Some(TraitDecl {
            is_pub,
            name,
            type_params,
            super_traits,
            items,
            doc_comment: None,
        })
    }

    fn parse_trait_item(&mut self) -> Option<TraitItem> {
        // Skip doc comments before trait items
        self.collect_doc_comments();
        let is_pure = self.eat(&Token::Pure);
        match self.peek() {
            Some(Token::Fn) => {
                self.advance();
                let name = self.expect_ident()?;

                let type_params = if self.eat(&Token::Less) {
                    Some(self.parse_type_params()?)
                } else {
                    None
                };

                self.expect(&Token::LeftParen)?;
                let params = self.parse_params();
                self.expect(&Token::RightParen)?;

                let return_type = if self.eat(&Token::Arrow) {
                    Some(self.parse_type()?)
                } else {
                    None
                };

                let where_clause = if self.peek() == Some(&Token::Where) {
                    self.advance();
                    Some(self.parse_where_clause()?)
                } else {
                    None
                };

                let body = if self.peek() == Some(&Token::LeftBrace) {
                    Some(self.parse_block()?)
                } else {
                    self.expect(&Token::Semicolon)?;
                    None
                };

                Some(TraitItem::Method(TraitMethod {
                    name,
                    is_pure,
                    type_params,
                    params,
                    return_type,
                    where_clause,
                    body,
                }))
            }
            Some(Token::Type) => {
                self.advance();
                let name = self.expect_ident()?;

                let bounds = if self.eat(&Token::Colon) {
                    let mut bounds = Vec::new();
                    loop {
                        bounds.push(self.parse_trait_bound()?);
                        if !self.eat(&Token::Plus) {
                            break;
                        }
                    }
                    bounds
                } else {
                    Vec::new()
                };

                self.expect(&Token::Semicolon)?;
                Some(TraitItem::AssociatedType { name, bounds })
            }
            _ => {
                let found = match self.peek() {
                    Some(tok) => format!("{tok}"),
                    None => "end of file".to_string(),
                };
                self.error_with_hint(
                    format!("expected trait item, found {found}"),
                    "trait items must be 'fn' signatures or 'type' declarations",
                );
                None
            }
        }
    }

    fn parse_impl_decl(&mut self) -> Option<ImplDecl> {
        let type_params = if self.eat(&Token::Less) {
            Some(self.parse_type_params()?)
        } else {
            None
        };

        // Try to parse trait bound first
        let saved_pos = self.save_pos();
        let trait_bound = if let Some(bound) = self.parse_trait_bound() {
            if self.eat(&Token::For) {
                Some(bound)
            } else {
                self.restore_pos(saved_pos);
                None
            }
        } else {
            self.restore_pos(saved_pos);
            None
        };

        let target_type = self.parse_type()?;

        let where_clause = if self.peek() == Some(&Token::Where) {
            self.advance();
            Some(self.parse_where_clause()?)
        } else {
            None
        };

        self.expect(&Token::LeftBrace)?;

        let mut methods = Vec::new();
        while !self.at_end() && self.peek() != Some(&Token::RightBrace) {
            let is_pure = self.eat(&Token::Pure);
            if self.peek() == Some(&Token::Fn) {
                self.advance();
                if let Some(method) = self.parse_function(false, false, false, is_pure, Vec::new())
                {
                    methods.push(method);
                }
            } else {
                self.advance();
            }
        }

        self.expect(&Token::RightBrace)?;

        Some(ImplDecl {
            type_params,
            trait_bound,
            target_type,
            where_clause,
            methods,
        })
    }

    /// Checks if the current position looks like a field declaration (ident: type).
    fn peek_is_field_decl(&mut self) -> bool {
        let saved = self.save_pos();
        let result = if self.expect_ident().is_some() {
            self.peek() == Some(&Token::Colon)
        } else {
            false
        };
        self.restore_pos(saved);
        result
    }

    #[expect(
        clippy::too_many_lines,
        reason = "actor decl parsing has many fields and sections"
    )]
    fn parse_actor_decl(&mut self, is_pub: bool) -> Option<ActorDecl> {
        let name = self.expect_ident()?;

        let super_traits = if self.eat(&Token::Colon) {
            let mut bounds = Vec::new();
            loop {
                bounds.push(self.parse_trait_bound()?);
                if !self.eat(&Token::Plus) {
                    break;
                }
            }
            Some(bounds)
        } else {
            None
        };

        self.expect(&Token::LeftBrace)?;

        let mut init = None;
        let mut fields = Vec::new();
        let mut receive_fns = Vec::new();
        let mut methods = Vec::new();
        let mut mailbox_capacity = None;
        let mut overflow_policy = None;

        while !self.at_end() && self.peek() != Some(&Token::RightBrace) {
            if self.peek() == Some(&Token::Init) {
                self.advance();
                self.expect(&Token::LeftParen)?;
                let params = self.parse_params();
                self.expect(&Token::RightParen)?;
                let body = self.parse_block()?;
                init = Some(ActorInit { params, body });
            } else if self.peek() == Some(&Token::Pure) || self.peek() == Some(&Token::Receive) {
                let is_pure = self.eat(&Token::Pure);
                if self.peek() == Some(&Token::Receive) {
                    let recv_start = self.peek_span().start;
                    self.advance();
                    let is_generator = if self.eat(&Token::Gen) {
                        if !self.eat(&Token::Fn) {
                            self.error("expected 'fn' after 'receive gen'".to_string());
                            return None;
                        }
                        true
                    } else {
                        if !self.eat(&Token::Fn) {
                            self.error("expected 'fn' after 'receive'".to_string());
                            return None;
                        }
                        false
                    };
                    let handler_name = self.expect_ident()?;
                    let type_params = if self.eat(&Token::Less) {
                        Some(self.parse_type_params()?)
                    } else {
                        None
                    };
                    self.expect(&Token::LeftParen)?;
                    let params = self.parse_params();
                    self.expect(&Token::RightParen)?;

                    let return_type = if self.eat(&Token::Arrow) {
                        Some(self.parse_type()?)
                    } else {
                        None
                    };

                    let where_clause = if self.peek() == Some(&Token::Where) {
                        self.advance();
                        Some(self.parse_where_clause()?)
                    } else {
                        None
                    };

                    let body = self.parse_block()?;
                    let recv_end = self.peek_span().start;
                    receive_fns.push(ReceiveFnDecl {
                        is_generator,
                        is_pure,
                        name: handler_name,
                        type_params,
                        params,
                        return_type,
                        where_clause,
                        body,
                        span: recv_start..recv_end,
                    });
                } else if self.peek() == Some(&Token::Fn) {
                    self.advance();
                    if let Some(method) =
                        self.parse_function(false, false, false, is_pure, Vec::new())
                    {
                        methods.push(method);
                    }
                } else {
                    self.error("'pure' can only be applied to function declarations".to_string());
                    return None;
                }
            } else if self.peek() == Some(&Token::Fn) {
                self.advance();
                if let Some(method) = self.parse_function(false, false, false, false, Vec::new()) {
                    methods.push(method);
                }
            } else if self.peek() == Some(&Token::Let) {
                self.advance();
                let field_name = self.expect_ident()?;
                self.expect(&Token::Colon)?;
                let ty = self.parse_type()?;
                if !self.eat(&Token::Semicolon) && self.peek() == Some(&Token::Comma) {
                    self.error("use `;` instead of `,` to separate fields".to_string());
                    self.advance();
                }
                fields.push(FieldDecl {
                    name: field_name,
                    ty,
                });
            } else if self.peek() == Some(&Token::Var) {
                self.advance();
                let field_name = self.expect_ident()?;
                self.expect(&Token::Colon)?;
                let ty = self.parse_type()?;
                // Skip optional `= expr` initializer
                if self.eat(&Token::Equal) {
                    let _ = self.parse_expr();
                }
                if !self.eat(&Token::Semicolon) && self.peek() == Some(&Token::Comma) {
                    self.error("use `;` instead of `,` to separate fields".to_string());
                    self.advance();
                }
                fields.push(FieldDecl {
                    name: field_name,
                    ty,
                });
            } else if matches!(self.peek(), Some(Token::Identifier(s)) if *s == "mailbox") {
                self.advance();
                if let Some(Token::Integer(n)) = self.peek() {
                    if let Some(cap) = parse_int_literal(n)
                        .ok()
                        .and_then(|v| u32::try_from(v).ok())
                    {
                        mailbox_capacity = Some(cap);
                    }
                    self.advance();
                }
                // Parse optional `overflow policy`
                if matches!(self.peek(), Some(Token::Identifier(s)) if *s == "overflow") {
                    self.advance();
                    overflow_policy = self.parse_overflow_policy();
                }
                self.eat(&Token::Semicolon);
            } else if self.peek_is_field_decl() {
                let field_name = self.expect_ident()?;
                self.expect(&Token::Colon)?;
                let ty = self.parse_type()?;
                if !self.eat(&Token::Semicolon) && self.peek() == Some(&Token::Comma) {
                    self.error("use `;` instead of `,` to separate fields".to_string());
                    self.advance();
                }
                fields.push(FieldDecl {
                    name: field_name,
                    ty,
                });
            } else {
                self.advance();
            }
        }

        self.expect(&Token::RightBrace)?;

        Some(ActorDecl {
            is_pub,
            name,
            super_traits,
            init,
            fields,
            receive_fns,
            methods,
            mailbox_capacity,
            overflow_policy,
            is_isolated: false,
            doc_comment: None,
        })
    }

    fn parse_overflow_policy(&mut self) -> Option<OverflowPolicy> {
        match self.peek() {
            Some(Token::Identifier(s)) => {
                let policy_name = (*s).to_owned();
                match &*policy_name {
                    "drop_new" => {
                        self.advance();
                        Some(OverflowPolicy::DropNew)
                    }
                    "drop_old" => {
                        self.advance();
                        Some(OverflowPolicy::DropOld)
                    }
                    "block" => {
                        self.advance();
                        Some(OverflowPolicy::Block)
                    }
                    "fail" => {
                        self.advance();
                        Some(OverflowPolicy::Fail)
                    }
                    "coalesce" => {
                        self.advance();
                        self.expect(&Token::LeftParen)?;
                        let key_field = self.expect_ident()?;
                        self.expect(&Token::RightParen)?;
                        let fallback = if matches!(self.peek(), Some(Token::Identifier(s)) if *s == "fallback")
                        {
                            self.advance();
                            match self.peek() {
                                Some(Token::Identifier(s)) => {
                                    let fb = (*s).to_owned();
                                    self.advance();
                                    match &*fb {
                                        "drop_new" => Some(OverflowFallback::DropNew),
                                        "drop_old" => Some(OverflowFallback::DropOld),
                                        "block" => Some(OverflowFallback::Block),
                                        "fail" => Some(OverflowFallback::Fail),
                                        _ => {
                                            self.error_with_hint(
                                                format!("unknown fallback policy '{fb}'"),
                                                "valid fallbacks: drop_new, drop_old, block, fail",
                                            );
                                            None
                                        }
                                    }
                                }
                                _ => None,
                            }
                        } else {
                            None
                        };
                        Some(OverflowPolicy::Coalesce {
                            key_field,
                            fallback,
                        })
                    }
                    _ => {
                        self.error_with_hint(
                            format!("unknown overflow policy '{policy_name}'"),
                            "valid policies: drop_new, drop_old, block, fail, coalesce(key)",
                        );
                        None
                    }
                }
            }
            _ => None,
        }
    }

    #[expect(
        clippy::too_many_lines,
        reason = "supervisor parsing requires sequential field handling"
    )]
    fn parse_supervisor_decl(&mut self, is_pub: bool) -> Option<SupervisorDecl> {
        let name = self.expect_ident()?;

        self.expect(&Token::LeftBrace)?;

        let mut strategy = None;
        let mut max_restarts = None;
        let mut window = None;
        let mut children = Vec::new();

        while !self.at_end() && self.peek() != Some(&Token::RightBrace) {
            match self.peek() {
                Some(Token::Strategy) => {
                    self.advance();
                    self.expect(&Token::Colon)?;
                    strategy = match self.peek() {
                        Some(Token::OneForOne) => {
                            self.advance();
                            Some(SupervisorStrategy::OneForOne)
                        }
                        Some(Token::OneForAll) => {
                            self.advance();
                            Some(SupervisorStrategy::OneForAll)
                        }
                        Some(Token::RestForOne) => {
                            self.advance();
                            Some(SupervisorStrategy::RestForOne)
                        }
                        _ => None,
                    };
                    if !self.eat(&Token::Semicolon) {
                        self.eat(&Token::Comma);
                    }
                }
                Some(Token::Identifier(s)) if *s == "max_restarts" => {
                    self.advance();
                    self.expect(&Token::Colon)?;
                    if let Some(Token::Integer(num_str)) = self.peek() {
                        max_restarts = parse_int_literal(num_str).ok();
                        self.advance();
                    }
                    if !self.eat(&Token::Semicolon) {
                        self.eat(&Token::Comma);
                    }
                }
                Some(Token::Identifier(s)) if *s == "window" => {
                    self.advance();
                    self.expect(&Token::Colon)?;
                    let mut val = String::new();
                    if let Some(Token::Integer(num_str)) = self.peek() {
                        val.push_str(num_str);
                        self.advance();
                    }
                    // Accept optional 's' suffix for seconds (e.g. `10s`)
                    if let Some(Token::Identifier(s)) = self.peek() {
                        if *s == "s" {
                            val.push('s');
                            self.advance();
                        }
                    }
                    if !val.is_empty() {
                        window = Some(val);
                    }
                    if !self.eat(&Token::Semicolon) {
                        self.eat(&Token::Comma);
                    }
                }
                Some(Token::Child) => {
                    self.advance();
                    let child_name = self.expect_ident()?;
                    self.expect(&Token::Colon)?;
                    let actor_type = self.expect_ident()?;

                    let mut args = Vec::new();
                    if self.eat(&Token::LeftParen) {
                        while !self.at_end() && self.peek() != Some(&Token::RightParen) {
                            args.push(self.parse_expr()?);
                            if !self.eat(&Token::Comma) {
                                break;
                            }
                        }
                        self.expect(&Token::RightParen)?;
                    }

                    let restart = match self.peek() {
                        Some(Token::Permanent) => {
                            self.advance();
                            Some(RestartPolicy::Permanent)
                        }
                        Some(Token::Transient) => {
                            self.advance();
                            Some(RestartPolicy::Transient)
                        }
                        Some(Token::Temporary) => {
                            self.advance();
                            Some(RestartPolicy::Temporary)
                        }
                        _ => None,
                    };

                    // Skip inline modifiers like restart(...) budget(...) strategy(...)
                    while matches!(
                        self.peek(),
                        Some(
                            Token::Identifier(_) | Token::Strategy | Token::Restart | Token::Budget
                        )
                    ) {
                        self.advance();
                        if self.eat(&Token::LeftParen) {
                            let mut depth = 1u32;
                            while !self.at_end() && depth > 0 {
                                match self.peek() {
                                    Some(Token::LeftParen) => {
                                        depth += 1;
                                        self.advance();
                                    }
                                    Some(Token::RightParen) => {
                                        depth -= 1;
                                        self.advance();
                                    }
                                    _ => {
                                        self.advance();
                                    }
                                }
                            }
                        }
                    }

                    if !self.eat(&Token::Semicolon) {
                        self.eat(&Token::Comma);
                    }
                    children.push(ChildSpec {
                        name: child_name,
                        actor_type,
                        args,
                        restart,
                    });
                }
                _ => {
                    // Skip unknown fields (e.g. max_restarts: 5, window: 10, children: [...])
                    self.advance();
                    if self.eat(&Token::Colon) {
                        // Skip the value — may be an integer, identifier, or bracket list
                        while !self.at_end()
                            && self.peek() != Some(&Token::Semicolon)
                            && self.peek() != Some(&Token::Comma)
                            && self.peek() != Some(&Token::RightBrace)
                        {
                            self.advance();
                        }
                        if !self.eat(&Token::Semicolon) {
                            self.eat(&Token::Comma);
                        }
                    }
                }
            }
        }

        self.expect(&Token::RightBrace)?;

        Some(SupervisorDecl {
            is_pub,
            name,
            strategy,
            max_restarts,
            window,
            children,
        })
    }

    fn parse_extern_block(&mut self) -> Option<ExternBlock> {
        let abi = if let Some(Token::StringLit(s) | Token::RawString(s)) = self.peek() {
            let abi = s
                .strip_prefix("r\"")
                .or_else(|| s.strip_prefix('"'))
                .and_then(|s| s.strip_suffix('"'))
                .unwrap_or(s)
                .to_string();
            self.advance();
            abi
        } else {
            "C".to_string()
        };

        self.expect(&Token::LeftBrace)?;

        let mut functions = Vec::new();
        while !self.at_end() && self.peek() != Some(&Token::RightBrace) {
            if self.peek() == Some(&Token::Fn) {
                self.advance();
                let name = self.expect_ident()?;

                self.expect(&Token::LeftParen)?;
                let params = self.parse_params();

                let is_variadic = self.eat(&Token::DotDot);
                self.expect(&Token::RightParen)?;

                let return_type = if self.eat(&Token::Arrow) {
                    Some(self.parse_type()?)
                } else {
                    None
                };

                self.expect(&Token::Semicolon)?;

                functions.push(ExternFnDecl {
                    name,
                    params,
                    return_type,
                    is_variadic,
                });
            } else {
                self.advance();
            }
        }

        self.expect(&Token::RightBrace)?;

        Some(ExternBlock { abi, functions })
    }

    #[expect(
        clippy::too_many_lines,
        reason = "wire decl parsing has many fields and variants"
    )]
    fn parse_wire_decl(&mut self, attrs: &[Attribute], is_pub: bool) -> Option<WireDecl> {
        let kind = match self.peek() {
            Some(Token::Type) => {
                self.advance();
                WireDeclKind::Struct
            }
            Some(Token::Enum) => {
                self.advance();
                WireDeclKind::Enum
            }
            _ => return None,
        };

        let name = self.expect_ident()?;

        self.expect(&Token::LeftBrace)?;

        let mut fields = Vec::new();
        let mut variants = Vec::new();

        while !self.at_end() && self.peek() != Some(&Token::RightBrace) {
            match kind {
                WireDeclKind::Struct => {
                    // Handle reserved(...) directive
                    if self.peek() == Some(&Token::Reserved) {
                        self.advance();
                        if self.eat(&Token::LeftParen) {
                            while !self.at_end() && self.peek() != Some(&Token::RightParen) {
                                self.advance();
                                self.eat(&Token::Comma);
                            }
                            self.expect(&Token::RightParen)?;
                        }
                        if !self.eat(&Token::Semicolon) {
                            self.eat(&Token::Comma);
                        }
                        continue;
                    }

                    // Parse wire field
                    let field_name = self.expect_ident()?;
                    self.expect(&Token::Colon)?;
                    let raw_ty = self.expect_ident()?;
                    // Normalize legacy lowercase aliases to canonical type names
                    let ty = match raw_ty.as_str() {
                        "string" | "str" => "String".to_string(),
                        _ => raw_ty,
                    };

                    // Field number: `field: type = N`, `field: type @N`, or auto-assigned
                    let field_number = if self.eat(&Token::Equal) || self.eat(&Token::At) {
                        if let Some(Token::Integer(num_str)) = self.peek() {
                            let raw = (*num_str).to_string();
                            self.advance();
                            if let Some(n) = parse_int_literal(&raw)
                                .ok()
                                .and_then(|v| u32::try_from(v).ok())
                            {
                                n
                            } else {
                                self.error(format!("invalid wire field number: {raw}"));
                                return None;
                            }
                        } else {
                            self.error("expected integer for wire field number".to_string());
                            return None;
                        }
                    } else {
                        // Auto-assign field number
                        #[expect(
                            clippy::cast_possible_truncation,
                            reason = "wire field numbers won't exceed u32::MAX"
                        )]
                        {
                            fields.len() as u32 + 1
                        }
                    };

                    // Parse optional modifiers: optional, deprecated, repeated, json("name"), yaml("name")
                    let mut is_optional = false;
                    let mut is_deprecated = false;
                    let mut is_repeated = false;
                    let mut json_name: Option<String> = None;
                    let mut yaml_name: Option<String> = None;
                    loop {
                        match self.peek() {
                            Some(Token::Optional) => {
                                is_optional = true;
                                self.advance();
                            }
                            Some(Token::Deprecated) => {
                                is_deprecated = true;
                                self.advance();
                            }
                            Some(Token::Identifier(s)) if *s == "repeated" => {
                                is_repeated = true;
                                self.advance();
                            }
                            Some(Token::Identifier(s)) if *s == "json" => {
                                self.advance();
                                if self.eat(&Token::LeftParen) {
                                    if let Some(Token::StringLit(s) | Token::RawString(s)) =
                                        self.peek()
                                    {
                                        let unquoted = s
                                            .strip_prefix("r\"")
                                            .or_else(|| s.strip_prefix('"'))
                                            .and_then(|s| s.strip_suffix('"'))
                                            .unwrap_or(s);
                                        json_name = Some(unquoted.to_string());
                                        self.advance();
                                    }
                                    let _ = self.expect(&Token::RightParen);
                                }
                            }
                            Some(Token::Identifier(s)) if *s == "yaml" => {
                                self.advance();
                                if self.eat(&Token::LeftParen) {
                                    if let Some(Token::StringLit(s) | Token::RawString(s)) =
                                        self.peek()
                                    {
                                        let unquoted = s
                                            .strip_prefix("r\"")
                                            .or_else(|| s.strip_prefix('"'))
                                            .and_then(|s| s.strip_suffix('"'))
                                            .unwrap_or(s);
                                        yaml_name = Some(unquoted.to_string());
                                        self.advance();
                                    }
                                    let _ = self.expect(&Token::RightParen);
                                }
                            }
                            _ => break,
                        }
                    }

                    fields.push(WireFieldDecl {
                        name: field_name,
                        ty,
                        field_number,
                        is_optional,
                        is_repeated,
                        is_reserved: false,
                        is_deprecated,
                        json_name,
                        yaml_name,
                    });

                    if !self.eat(&Token::Semicolon) {
                        self.eat(&Token::Comma);
                    }
                }
                WireDeclKind::Enum => {
                    // Parse enum variant
                    let variant_name = self.expect_ident()?;
                    let mut variant_fields = Vec::new();

                    if self.eat(&Token::LeftParen) {
                        while !self.at_end() && self.peek() != Some(&Token::RightParen) {
                            variant_fields.push(self.parse_type()?);
                            if !self.eat(&Token::Comma) {
                                break;
                            }
                        }
                        self.expect(&Token::RightParen)?;
                    }

                    if !self.eat(&Token::Comma) {
                        self.eat(&Token::Semicolon);
                    }
                    variants.push(VariantDecl {
                        name: variant_name,
                        fields: variant_fields,
                    });
                }
            }
        }

        self.expect(&Token::RightBrace)?;

        // Extract struct-level JSON/YAML naming conventions from outer attributes.
        let json_case = attrs
            .iter()
            .find(|a| a.name == "json")
            .and_then(|a| a.args.first().and_then(|s| NamingCase::from_attr(s)));
        let yaml_case = attrs
            .iter()
            .find(|a| a.name == "yaml")
            .and_then(|a| a.args.first().and_then(|s| NamingCase::from_attr(s)));

        Some(WireDecl {
            is_pub,
            kind,
            name,
            fields,
            variants,
            json_case,
            yaml_case,
        })
    }

    fn parse_import(&mut self) -> Option<ImportDecl> {
        // File-path import: import "path/to/file.hew";
        if let Some(Token::StringLit(s) | Token::RawString(s)) = self.peek() {
            let raw = *s;
            self.advance();
            self.expect(&Token::Semicolon)?;
            let file_path = raw
                .strip_prefix("r\"")
                .or_else(|| raw.strip_prefix('"'))
                .and_then(|s| s.strip_suffix('"'))
                .unwrap_or(raw)
                .to_owned();
            return Some(ImportDecl {
                path: Vec::new(),
                spec: None,
                file_path: Some(file_path),
                resolved_items: None,
            });
        }

        let mut path = Vec::new();

        loop {
            path.push(self.expect_ident()?);
            // Only continue path if :: is followed by an identifier
            if self.peek() == Some(&Token::DoubleColon) {
                let saved = self.save_pos();
                self.advance(); // consume ::
                if !matches!(
                    self.peek(),
                    Some(
                        Token::Identifier(_)
                            | Token::After
                            | Token::From
                            | Token::Init
                            | Token::Child
                            | Token::Restart
                            | Token::Budget
                            | Token::Strategy
                            | Token::Permanent
                            | Token::Transient
                            | Token::Temporary
                            | Token::OneForOne
                            | Token::OneForAll
                            | Token::RestForOne
                            | Token::Wire
                            | Token::Optional
                            | Token::Deprecated
                            | Token::Reserved
                    )
                ) {
                    // :: followed by *, {, etc. — restore and let spec parsing handle it
                    self.restore_pos(saved);
                    break;
                }
            } else {
                break;
            }
        }

        let spec = if self.eat(&Token::DoubleColon) {
            if self.eat(&Token::Star) {
                Some(ImportSpec::Glob)
            } else if self.eat(&Token::LeftBrace) {
                let mut names = Vec::new();
                while !self.at_end() && self.peek() != Some(&Token::RightBrace) {
                    let name = self.expect_ident()?;
                    let alias = if self.eat(&Token::As) {
                        Some(self.expect_ident()?)
                    } else {
                        None
                    };
                    names.push(ImportName { name, alias });
                    if !self.eat(&Token::Comma) {
                        break;
                    }
                }
                self.expect(&Token::RightBrace)?;
                Some(ImportSpec::Names(names))
            } else {
                let found = self
                    .peek()
                    .map_or_else(|| "end of input".to_string(), |t| format!("{t}"));
                self.error(format!("expected `*` or `{{` after `::`, found {found}"));
                return None;
            }
        } else {
            None
        };

        self.expect(&Token::Semicolon)?;

        Some(ImportDecl {
            path,
            spec,
            file_path: None,
            resolved_items: None,
        })
    }

    fn parse_const_decl(&mut self, is_pub: bool) -> Option<ConstDecl> {
        let name = self.expect_ident()?;
        self.expect(&Token::Colon)?;
        let ty = self.parse_type()?;
        self.expect(&Token::Equal)?;
        let value = self.parse_expr()?;
        self.expect(&Token::Semicolon)?;

        Some(ConstDecl {
            is_pub,
            name,
            ty,
            value,
        })
    }

    // ── Types ──
    #[expect(
        clippy::too_many_lines,
        reason = "recursive descent parser requires sequential case handling"
    )]
    fn parse_type(&mut self) -> Option<Spanned<TypeExpr>> {
        let _guard = self.enter_recursion()?;
        let start = self.peek_span().start;

        let ty = match self.peek() {
            Some(Token::LeftParen) => {
                self.advance();
                if self.eat(&Token::RightParen) {
                    // Unit type represented as empty tuple
                    TypeExpr::Tuple(Vec::new())
                } else {
                    let mut types = vec![self.parse_type()?];
                    while self.eat(&Token::Comma) {
                        if self.peek() == Some(&Token::RightParen) {
                            break;
                        }
                        types.push(self.parse_type()?);
                    }
                    self.expect(&Token::RightParen)?;

                    if types.len() == 1 {
                        return Some(types.into_iter().next().unwrap());
                    }
                    TypeExpr::Tuple(types)
                }
            }
            Some(Token::LeftBracket) => {
                self.advance();
                let element_type = self.parse_type()?;

                if self.eat(&Token::Semicolon) {
                    // Array: [T; N] - but AST expects u64, not expr for size
                    if let Some(Token::Integer(num_str)) = self.peek() {
                        if let Some(size) = parse_int_literal(num_str)
                            .ok()
                            .and_then(|v| u64::try_from(v).ok())
                        {
                            self.advance();
                            self.expect(&Token::RightBracket)?;
                            TypeExpr::Array {
                                element: Box::new(element_type),
                                size,
                            }
                        } else {
                            self.error("array size must be integer literal".to_string());
                            return None;
                        }
                    } else {
                        self.error("expected array size".to_string());
                        return None;
                    }
                } else {
                    // Slice: [T]
                    self.expect(&Token::RightBracket)?;
                    TypeExpr::Slice(Box::new(element_type))
                }
            }
            Some(Token::Star) => {
                self.advance();
                let is_mutable = self.eat(&Token::Var);
                let pointee = self.parse_type()?;
                TypeExpr::Pointer {
                    is_mutable,
                    pointee: Box::new(pointee),
                }
            }
            Some(Token::Dyn) => {
                self.advance();
                // dyn TraitName or dyn TraitName<TypeArgs>
                let name = self.expect_ident()?;
                let type_args = if self.eat(&Token::Less) {
                    Some(self.parse_type_args()?)
                } else {
                    None
                };
                TypeExpr::TraitObject(TraitBound { name, type_args })
            }
            Some(Token::Fn) => {
                self.advance();
                self.expect(&Token::LeftParen)?;

                let mut params = Vec::new();
                while !self.at_end() && self.peek() != Some(&Token::RightParen) {
                    params.push(self.parse_type()?);
                    if !self.eat(&Token::Comma) {
                        break;
                    }
                }
                self.expect(&Token::RightParen)?;

                let return_type = if self.eat(&Token::Arrow) {
                    Box::new(self.parse_type()?)
                } else {
                    // Default to unit type
                    Box::new((TypeExpr::Tuple(Vec::new()), 0..0))
                };

                TypeExpr::Function {
                    params,
                    return_type,
                }
            }
            _ => {
                // Named type: identifier or contextual keyword, with optional module prefix
                let name = self.expect_ident()?;
                // `_` in type position means infer the type
                if name == "_" {
                    TypeExpr::Infer
                } else {
                    // Check for module-qualified type: module.TypeName
                    let name = if self.eat(&Token::Dot) {
                        let type_name = self.expect_ident()?;
                        format!("{name}.{type_name}")
                    } else {
                        name
                    };
                    let type_args = if self.eat(&Token::Less) {
                        Some(self.parse_type_args()?)
                    } else {
                        None
                    };
                    TypeExpr::Named { name, type_args }
                }
            }
        };

        let end = self.peek_span().start;
        Some((ty, start..end))
    }

    fn parse_type_params(&mut self) -> Option<Vec<TypeParam>> {
        let mut params = Vec::new();

        while !self.at_end() && !self.at_closing_angle() {
            let name = self.expect_ident()?;

            let bounds = if self.eat(&Token::Colon) {
                let mut bounds = Vec::new();
                loop {
                    bounds.push(self.parse_trait_bound()?);
                    if !self.eat(&Token::Plus) {
                        break;
                    }
                }
                bounds
            } else {
                Vec::new()
            };

            params.push(TypeParam { name, bounds });

            if !self.eat(&Token::Comma) {
                break;
            }
        }

        if !self.eat_closing_angle() {
            self.error("expected '>'".to_string());
            return None;
        }
        Some(params)
    }

    fn parse_type_args(&mut self) -> Option<Vec<Spanned<TypeExpr>>> {
        let mut args = Vec::new();

        while !self.at_end() && !self.at_closing_angle() {
            args.push(self.parse_type()?);
            if !self.eat(&Token::Comma) {
                break;
            }
        }

        if !self.eat_closing_angle() {
            self.error("expected '>'".to_string());
            return None;
        }
        Some(args)
    }

    fn parse_trait_bound(&mut self) -> Option<TraitBound> {
        let name = self.expect_ident()?;

        let type_args = if self.eat(&Token::Less) {
            Some(self.parse_type_args()?)
        } else {
            None
        };

        Some(TraitBound { name, type_args })
    }

    fn parse_where_clause(&mut self) -> Option<WhereClause> {
        let mut predicates = Vec::new();

        loop {
            let ty = self.parse_type()?;
            self.expect(&Token::Colon)?;

            let mut bounds = Vec::new();
            loop {
                bounds.push(self.parse_trait_bound()?);
                if !self.eat(&Token::Plus) {
                    break;
                }
            }

            predicates.push(WherePredicate { ty, bounds });

            if !self.eat(&Token::Comma) {
                break;
            }
        }

        Some(WhereClause { predicates })
    }

    fn parse_params(&mut self) -> Vec<Param> {
        let mut params = Vec::new();

        while !self.at_end() && self.peek() != Some(&Token::RightParen) {
            let is_mutable = self.eat(&Token::Var);
            let Some(name) = self.expect_ident() else {
                break;
            };

            if !self.eat(&Token::Colon) {
                break;
            }

            if let Some(ty) = self.parse_type() {
                params.push(Param {
                    name,
                    ty,
                    is_mutable,
                });
            }

            if !self.eat(&Token::Comma) {
                break;
            }
        }

        params
    }

    /// Returns true if the expression is a block-like construct that doesn't need a trailing semicolon.
    fn is_block_expr(expr: &Expr) -> bool {
        matches!(
            expr,
            Expr::Block(_)
                | Expr::If { .. }
                | Expr::Match { .. }
                | Expr::Scope { .. }
                | Expr::ScopeLaunch(_)
                | Expr::Unsafe(_)
                | Expr::Select { .. }
        )
    }

    // ── Statements ──
    fn parse_block(&mut self) -> Option<Block> {
        self.expect(&Token::LeftBrace)?;

        let mut stmts = Vec::new();
        let mut trailing_expr = None;

        while !self.at_end() && self.peek() != Some(&Token::RightBrace) {
            // Try to parse as statement first
            if let Some(stmt) = self.parse_stmt() {
                stmts.push(stmt);
                while self.peek() == Some(&Token::Semicolon) {
                    let span = self.peek_span();
                    self.advance();
                    self.warning_at("unnecessary semicolon".to_string(), span);
                }
                continue;
            }

            // Try as expression
            if let Some(expr) = self.parse_expr() {
                // Check for assignment
                if let Some(op) = self.parse_compound_assign_op() {
                    let value = self.parse_expr()?;
                    self.expect(&Token::Semicolon)?;
                    let span = expr.1.start..value.1.end;
                    stmts.push((
                        Stmt::Assign {
                            target: expr,
                            op: Some(op),
                            value,
                        },
                        span,
                    ));
                } else if self.eat(&Token::Equal) {
                    let value = self.parse_expr()?;
                    self.expect(&Token::Semicolon)?;
                    let span = expr.1.start..value.1.end;
                    stmts.push((
                        Stmt::Assign {
                            target: expr,
                            op: None,
                            value,
                        },
                        span,
                    ));
                } else if self.eat(&Token::Semicolon) {
                    // Expression statement
                    while self.peek() == Some(&Token::Semicolon) {
                        let semi_span = self.peek_span();
                        self.advance();
                        self.warning_at("unnecessary semicolon".to_string(), semi_span);
                    }
                    let span = expr.1.clone();
                    stmts.push((Stmt::Expression(expr), span));
                } else if self.peek() != Some(&Token::RightBrace) && Self::is_block_expr(&expr.0) {
                    // Block-like expressions (if, match, blocks, loops) don't need semicolons
                    let span = expr.1.clone();
                    stmts.push((Stmt::Expression(expr), span));
                } else {
                    // Trailing expression (no semicolon)
                    trailing_expr = Some(Box::new(expr));
                    break;
                }
            } else {
                let found = match self.peek() {
                    Some(tok) => format!("{tok}"),
                    None => "end of file".to_string(),
                };
                self.error(format!("unexpected {found} in block"));
                self.advance();
            }
        }

        self.expect(&Token::RightBrace)?;

        Some(Block {
            stmts,
            trailing_expr,
        })
    }

    #[expect(clippy::too_many_lines, reason = "parser function with many branches")]
    fn parse_stmt(&mut self) -> Option<Spanned<Stmt>> {
        let _guard = self.enter_recursion()?;
        let start = self.peek_span().start;

        // Check for labeled loop/while: 'label: loop/while
        if let Some(Token::Label(_)) = self.peek() {
            return self.parse_labeled_stmt(start);
        }

        let stmt = match self.peek() {
            Some(Token::Let) => {
                self.advance();
                let pattern = self.parse_pattern()?;

                let ty = if self.eat(&Token::Colon) {
                    Some(self.parse_type()?)
                } else {
                    None
                };

                let value = if self.eat(&Token::Equal) {
                    Some(self.parse_expr()?)
                } else {
                    None
                };

                self.expect(&Token::Semicolon)?;

                Stmt::Let { pattern, ty, value }
            }
            Some(Token::Var) => {
                self.advance();
                let name = self.expect_ident()?;

                let ty = if self.eat(&Token::Colon) {
                    Some(self.parse_type()?)
                } else {
                    None
                };

                let value = if self.eat(&Token::Equal) {
                    Some(self.parse_expr()?)
                } else {
                    None
                };

                self.expect(&Token::Semicolon)?;

                Stmt::Var { name, ty, value }
            }
            // These don't need semicolons (they have blocks)
            Some(Token::If) => {
                self.advance();
                let condition = self.parse_expr()?;
                let then_block = self.parse_block()?;

                let else_block = if self.eat(&Token::Else) {
                    if self.peek() == Some(&Token::If) {
                        // else if
                        let if_stmt = Box::new(self.parse_stmt()?);
                        Some(ElseBlock {
                            is_if: true,
                            if_stmt: Some(if_stmt),
                            block: None,
                        })
                    } else {
                        // else block
                        let block = self.parse_block()?;
                        Some(ElseBlock {
                            is_if: false,
                            if_stmt: None,
                            block: Some(block),
                        })
                    }
                } else {
                    None
                };

                Stmt::If {
                    condition,
                    then_block,
                    else_block,
                }
            }
            Some(Token::Match) => {
                self.advance();
                let scrutinee = self.parse_expr()?;
                self.expect(&Token::LeftBrace)?;

                let mut arms = Vec::new();
                while !self.at_end() && self.peek() != Some(&Token::RightBrace) {
                    arms.push(self.parse_match_arm()?);
                }

                self.expect(&Token::RightBrace)?;

                Stmt::Match { scrutinee, arms }
            }
            Some(Token::Loop) => {
                self.advance();
                let body = self.parse_block()?;
                Stmt::Loop { label: None, body }
            }
            Some(Token::While) => {
                self.advance();
                let condition = self.parse_expr()?;
                let body = self.parse_block()?;
                Stmt::While {
                    label: None,
                    condition,
                    body,
                }
            }
            Some(Token::For) => {
                self.advance();
                let is_await = self.eat(&Token::Await);
                let pattern = self.parse_pattern()?;
                self.expect(&Token::In)?;
                let iterable = self.parse_expr()?;
                let body = self.parse_block()?;
                Stmt::For {
                    is_await,
                    pattern,
                    iterable,
                    body,
                }
            }
            Some(Token::Break) => {
                self.advance();
                let label = if let Some(Token::Label(l)) = self.peek() {
                    let name = l[1..].to_string();
                    self.advance();
                    Some(name)
                } else {
                    None
                };
                let value = if self.peek() == Some(&Token::Semicolon) {
                    None
                } else {
                    Some(self.parse_expr()?)
                };
                self.expect(&Token::Semicolon)?;
                Stmt::Break { label, value }
            }
            Some(Token::Continue) => {
                self.advance();
                let label = if let Some(Token::Label(l)) = self.peek() {
                    let name = l[1..].to_string();
                    self.advance();
                    Some(name)
                } else {
                    None
                };
                self.expect(&Token::Semicolon)?;
                Stmt::Continue { label }
            }
            Some(Token::Return) => {
                self.advance();
                let value = if self.peek() == Some(&Token::Semicolon) {
                    None
                } else {
                    Some(self.parse_expr()?)
                };
                self.expect(&Token::Semicolon)?;
                Stmt::Return(value)
            }
            Some(Token::Defer) => {
                self.advance();
                let expr = self.parse_expr()?;
                self.expect(&Token::Semicolon)?;
                Stmt::Defer(Box::new(expr))
            }
            _ => {
                // Not a recognized statement keyword - this will be handled by the caller
                return None;
            }
        };

        let end = self.peek_span().start;
        Some((stmt, start..end))
    }

    /// Parse a labeled statement: `'label: while ...` or `'label: loop ...`
    fn parse_labeled_stmt(&mut self, start: usize) -> Option<Spanned<Stmt>> {
        let label_tok = self.advance()?;
        let label = if let (Token::Label(l), _) = label_tok {
            l[1..].to_string()
        } else {
            return None;
        };
        self.expect(&Token::Colon)?;

        let stmt = match self.peek() {
            Some(Token::While) => {
                self.advance();
                let condition = self.parse_expr()?;
                let body = self.parse_block()?;
                Stmt::While {
                    label: Some(label),
                    condition,
                    body,
                }
            }
            Some(Token::Loop) => {
                self.advance();
                let body = self.parse_block()?;
                Stmt::Loop {
                    label: Some(label),
                    body,
                }
            }
            _ => {
                self.error("expected `while` or `loop` after label".to_string());
                return None;
            }
        };

        let end = self.peek_span().start;
        Some((stmt, start..end))
    }

    fn parse_compound_assign_op(&mut self) -> Option<CompoundAssignOp> {
        match self.peek() {
            Some(Token::PlusEqual) => {
                self.advance();
                Some(CompoundAssignOp::Add)
            }
            Some(Token::MinusEqual) => {
                self.advance();
                Some(CompoundAssignOp::Subtract)
            }
            Some(Token::StarEqual) => {
                self.advance();
                Some(CompoundAssignOp::Multiply)
            }
            Some(Token::SlashEqual) => {
                self.advance();
                Some(CompoundAssignOp::Divide)
            }
            Some(Token::PercentEqual) => {
                self.advance();
                Some(CompoundAssignOp::Modulo)
            }
            Some(Token::AmpEqual) => {
                self.advance();
                Some(CompoundAssignOp::BitAnd)
            }
            Some(Token::PipeEqual) => {
                self.advance();
                Some(CompoundAssignOp::BitOr)
            }
            Some(Token::CaretEqual) => {
                self.advance();
                Some(CompoundAssignOp::BitXor)
            }
            Some(Token::LessLessEqual) => {
                self.advance();
                Some(CompoundAssignOp::Shl)
            }
            Some(Token::GreaterGreaterEqual) => {
                self.advance();
                Some(CompoundAssignOp::Shr)
            }
            _ => None,
        }
    }

    // ── Expressions (Pratt Precedence) ──
    fn parse_expr(&mut self) -> Option<Spanned<Expr>> {
        let _guard = self.enter_recursion()?;
        self.parse_expr_bp(0)
    }

    #[expect(
        clippy::too_many_lines,
        reason = "Pratt parser covers many expression forms"
    )]
    fn parse_expr_bp(&mut self, min_bp: u8) -> Option<Spanned<Expr>> {
        let start = self.peek_span().start;

        // Prefix operators
        let mut lhs = if let Some(rbp) = self.peek().and_then(prefix_bp) {
            let (op_tok, _) = self.advance()?;
            match op_tok {
                Token::Bang => {
                    let operand = self.parse_expr_bp(rbp)?;
                    let end = operand.1.end;
                    (
                        Expr::Unary {
                            op: UnaryOp::Not,
                            operand: Box::new(operand),
                        },
                        start..end,
                    )
                }
                Token::Minus => {
                    let operand = self.parse_expr_bp(rbp)?;
                    let end = operand.1.end;
                    (
                        Expr::Unary {
                            op: UnaryOp::Negate,
                            operand: Box::new(operand),
                        },
                        start..end,
                    )
                }
                Token::Tilde => {
                    let operand = self.parse_expr_bp(rbp)?;
                    let end = operand.1.end;
                    (
                        Expr::Unary {
                            op: UnaryOp::BitNot,
                            operand: Box::new(operand),
                        },
                        start..end,
                    )
                }
                Token::Await => {
                    let operand = self.parse_expr_bp(rbp)?;
                    let end = operand.1.end;
                    (Expr::Await(Box::new(operand)), start..end)
                }
                _ => unreachable!(),
            }
        } else {
            self.parse_primary()?
        };

        // Infix + postfix
        loop {
            // Try postfix first (highest precedence)
            match self.peek() {
                Some(Token::Dot) => {
                    lhs = self.parse_dot_postfix(lhs)?;
                    continue;
                }
                Some(Token::LeftParen) => {
                    lhs = self.parse_call_postfix(lhs)?;
                    continue;
                }
                Some(Token::LeftBracket) => {
                    lhs = self.parse_index_postfix(lhs)?;
                    continue;
                }
                Some(Token::Question) => {
                    self.advance();
                    let end = self.peek_span().start;
                    lhs = (Expr::PostfixTry(Box::new(lhs)), start..end);
                    continue;
                }
                _ => {}
            }

            // Generic call: ident<Type1, Type2>(args)
            // Must check BEFORE infix operators consume '<' as less-than.
            if self.peek() == Some(&Token::Less) {
                if let Expr::Identifier(_) = &lhs.0 {
                    let saved = self.save_pos();
                    self.advance(); // consume '<'
                    if let Some(type_args) = self.parse_type_args() {
                        if self.peek() == Some(&Token::LeftParen) {
                            self.advance(); // consume '('
                            let args = self.parse_call_args()?;
                            self.expect(&Token::RightParen)?;
                            let end = self.peek_span().start;
                            lhs = (
                                Expr::Call {
                                    function: Box::new(lhs),
                                    type_args: Some(type_args),
                                    args,
                                    is_tail_call: false,
                                },
                                start..end,
                            );
                            continue;
                        }
                    }
                    // Not a generic call — backtrack
                    self.restore_pos(saved);
                }
            }

            // Timeout combinator: expr | after duration
            // Checked before infix so `| after` is not consumed as bitwise OR.
            if self.peek() == Some(&Token::Pipe) {
                let saved = self.save_pos();
                self.advance(); // consume |
                if self.peek() == Some(&Token::After) {
                    // Binding power 5 (same as bitwise OR left bp)
                    if 5 >= min_bp {
                        self.advance(); // consume after
                        let duration = self.parse_expr_bp(6)?;
                        let end = duration.1.end;
                        lhs = (
                            Expr::Timeout {
                                expr: Box::new(lhs),
                                duration: Box::new(duration),
                            },
                            start..end,
                        );
                        continue;
                    }
                }
                self.restore_pos(saved);
            }

            // Then try infix
            let Some((lbp, rbp)) = self.peek().and_then(infix_bp) else {
                break;
            };
            if lbp < min_bp {
                break;
            }

            let (op_tok, _) = self.advance()?;
            let Some(op) = token_to_binop(&op_tok) else {
                self.error(format!("invalid binary operator token: {op_tok:?}"));
                return None;
            };
            let rhs = self.parse_expr_bp(rbp)?;
            let end = rhs.1.end;

            lhs = (
                Expr::Binary {
                    left: Box::new(lhs),
                    op,
                    right: Box::new(rhs),
                },
                start..end,
            );
        }

        Some(lhs)
    }

    #[expect(
        clippy::too_many_lines,
        reason = "expression parser with many branches"
    )]
    fn parse_primary(&mut self) -> Option<Spanned<Expr>> {
        let start = self.peek_span().start;

        let expr = match self.peek()? {
            Token::Duration(s) => {
                if let Some(nanos) = parse_duration_literal(s) {
                    self.advance();
                    Expr::Literal(Literal::Duration(nanos))
                } else {
                    self.error_with_hint(
                        "invalid duration literal".to_string(),
                        "valid formats: 100ms, 5s, 2m, 1h, 500us, 10ns",
                    );
                    return None;
                }
            }
            Token::Integer(s) => {
                let radix = detect_int_radix(s);
                if let Ok(val) = parse_int_literal(s) {
                    self.advance();
                    Expr::Literal(Literal::Integer { value: val, radix })
                } else {
                    self.error_with_hint(
                        format!("invalid integer literal '{s}'"),
                        "integer literals support decimal, 0x hex, 0o octal, and 0b binary",
                    );
                    return None;
                }
            }
            Token::Float(s) => {
                let cleaned: String = s.chars().filter(|c| *c != '_').collect();
                if let Ok(val) = cleaned.parse::<f64>() {
                    self.advance();
                    Expr::Literal(Literal::Float(val))
                } else {
                    self.error(format!("invalid float literal '{s}'"));
                    return None;
                }
            }
            Token::StringLit(s) => {
                // Strip surrounding quotes and process escape sequences
                let s = s
                    .strip_prefix('"')
                    .and_then(|s| s.strip_suffix('"'))
                    .unwrap_or(s);
                let s = unescape_string(s);
                self.advance();
                Expr::Literal(Literal::String(s))
            }
            Token::RawString(s) => {
                let s = s
                    .strip_prefix("r\"")
                    .and_then(|s| s.strip_suffix('"'))
                    .unwrap_or(s)
                    .to_string();
                self.advance();
                Expr::Literal(Literal::String(s))
            }
            Token::InterpolatedString(s) => {
                let s = s.to_string();
                self.advance();
                let parts = parse_string_parts(&s, 2, 1, "{", start);
                Expr::InterpolatedString(parts)
            }
            Token::RegexLiteral(s) => {
                // Strip `re"` prefix and `"` suffix to get the pattern.
                let pattern = s
                    .strip_prefix("re\"")
                    .and_then(|s| s.strip_suffix('"'))
                    .unwrap_or(s);
                let pattern = pattern.to_string();
                self.advance();
                Expr::RegexLiteral(pattern)
            }
            Token::True => {
                self.advance();
                Expr::Literal(Literal::Bool(true))
            }
            Token::False => {
                self.advance();
                Expr::Literal(Literal::Bool(false))
            }
            Token::Identifier(name) => {
                let mut name = name.to_string();
                self.advance();

                // Handle path expressions like Vec::new, HashMap::new
                while self.eat(&Token::DoubleColon) {
                    if let Some(segment) = self.expect_ident() {
                        name = format!("{name}::{segment}");
                    } else {
                        break;
                    }
                }

                // Desugar scope handle method calls: s.launch { ... }, s.cancel(), s.is_cancelled()
                if self.scope_binding.as_deref() == Some(&name) && self.peek() == Some(&Token::Dot)
                {
                    let saved_pos = self.save_pos();
                    self.advance(); // consume .
                    if let Some(Token::Identifier(method)) = self.peek() {
                        let method = method.to_string();
                        match method.as_str() {
                            "launch" => {
                                self.advance(); // consume "launch"
                                let body = self.parse_block()?;
                                let end = self.peek_span().start;
                                return Some((Expr::ScopeLaunch(body), start..end));
                            }
                            "cancel" => {
                                self.advance(); // consume "cancel"
                                self.expect(&Token::LeftParen)?;
                                self.expect(&Token::RightParen)?;
                                let end = self.peek_span().start;
                                return Some((Expr::ScopeCancel, start..end));
                            }

                            _ => {
                                // Not a scope method, restore and fall through
                                self.restore_pos(saved_pos);
                            }
                        }
                    } else {
                        self.restore_pos(saved_pos);
                    }
                }

                // Check for struct initialization
                if self.peek() == Some(&Token::LeftBrace) {
                    // Look ahead to disambiguate struct init vs block
                    let saved_pos = self.save_pos();
                    self.advance(); // consume {
                    let is_struct_init = if self.peek().is_some_and(|tok| Self::is_ident_token(tok))
                    {
                        self.advance();
                        self.peek() == Some(&Token::Colon)
                    } else {
                        false
                    };
                    self.restore_pos(saved_pos);

                    if is_struct_init {
                        self.advance(); // consume {
                        let mut fields = Vec::new();
                        while !self.at_end() && self.peek() != Some(&Token::RightBrace) {
                            let field_name = self.expect_ident()?;
                            self.expect(&Token::Colon)?;
                            let value = self.parse_expr()?;
                            fields.push((field_name, value));

                            if !self.eat(&Token::Comma) {
                                break;
                            }
                        }
                        self.expect(&Token::RightBrace)?;
                        Expr::StructInit { name, fields }
                    } else {
                        Expr::Identifier(name)
                    }
                } else {
                    Expr::Identifier(name)
                }
            }
            Token::LeftParen => {
                self.advance();

                // Try parsing as lambda first
                let saved_pos = self.save_pos();
                let is_lambda = if self.try_parse_lambda_params().is_some() {
                    if self.expect(&Token::RightParen).is_some() {
                        // Check for optional return type
                        if self.eat(&Token::Arrow) {
                            self.parse_type().is_some() && self.peek() == Some(&Token::FatArrow)
                        } else {
                            self.peek() == Some(&Token::FatArrow)
                        }
                    } else {
                        false
                    }
                } else {
                    false
                };
                self.restore_pos(saved_pos);

                if is_lambda {
                    let is_move = false;
                    let params = self.try_parse_lambda_params()?;
                    self.expect(&Token::RightParen)?;

                    let return_type = if self.eat(&Token::Arrow) {
                        Some(self.parse_type()?)
                    } else {
                        None
                    };

                    self.expect(&Token::FatArrow)?;
                    let body = Box::new(self.parse_expr()?);

                    Expr::Lambda {
                        is_move,
                        params,
                        return_type,
                        body,
                    }
                } else if self.eat(&Token::RightParen) {
                    // Unit tuple
                    Expr::Tuple(Vec::new())
                } else {
                    // Parenthesized expression or tuple
                    let mut exprs = vec![self.parse_expr()?];
                    while self.eat(&Token::Comma) {
                        if self.peek() == Some(&Token::RightParen) {
                            break;
                        }
                        exprs.push(self.parse_expr()?);
                    }
                    self.expect(&Token::RightParen)?;

                    if exprs.len() == 1 {
                        return Some(exprs.into_iter().next().unwrap());
                    }
                    Expr::Tuple(exprs)
                }
            }
            Token::LeftBracket => {
                self.advance();
                let mut elements = Vec::new();

                while !self.at_end() && self.peek() != Some(&Token::RightBracket) {
                    elements.push(self.parse_expr()?);
                    if !self.eat(&Token::Comma) {
                        break;
                    }
                }

                self.expect(&Token::RightBracket)?;
                Expr::Array(elements)
            }
            Token::LeftBrace => Expr::Block(self.parse_block()?),
            Token::If => {
                self.advance();
                let condition = Box::new(self.parse_expr()?);
                let then_block = Box::new(self.parse_expr()?);
                let else_block = if self.eat(&Token::Else) {
                    Some(Box::new(self.parse_expr()?))
                } else {
                    None
                };
                Expr::If {
                    condition,
                    then_block,
                    else_block,
                }
            }
            Token::Match => {
                self.advance();
                let scrutinee = Box::new(self.parse_expr()?);
                self.expect(&Token::LeftBrace)?;

                let mut arms = Vec::new();
                while !self.at_end() && self.peek() != Some(&Token::RightBrace) {
                    arms.push(self.parse_match_arm()?);
                }

                self.expect(&Token::RightBrace)?;
                Expr::Match { scrutinee, arms }
            }
            Token::Spawn => {
                self.advance();

                // Check for optional `move` keyword before lambda actor
                let is_move = self.eat(&Token::Move);

                // Check for lambda actor: spawn [move] (params) => body
                if self.peek() == Some(&Token::LeftParen) {
                    let saved_pos = self.save_pos();
                    self.advance();
                    let is_lambda_actor = self.try_parse_lambda_params().is_some() && {
                        self.expect(&Token::RightParen).is_some()
                            && self.peek() == Some(&Token::FatArrow)
                    };
                    self.restore_pos(saved_pos);

                    if is_lambda_actor {
                        self.advance(); // consume (
                        let params = self.try_parse_lambda_params()?;
                        self.expect(&Token::RightParen)?;

                        let return_type = if self.eat(&Token::Arrow) {
                            Some(self.parse_type()?)
                        } else {
                            None
                        };

                        self.expect(&Token::FatArrow)?;
                        let body = Box::new(self.parse_expr()?);

                        return Some((
                            Expr::SpawnLambdaActor {
                                is_move,
                                params,
                                return_type,
                                body,
                            },
                            start..self.peek_span().start,
                        ));
                    }
                }

                // Regular spawn: spawn ActorName(field: value, ...)
                let name = self.expect_ident()?;
                let name_end = self.peek_span().start;
                let target = Box::new((Expr::Identifier(name), start..name_end));
                let args = if self.eat(&Token::LeftParen) {
                    let mut args = Vec::new();
                    while !self.at_end() && self.peek() != Some(&Token::RightParen) {
                        let field_name = self.expect_ident()?;
                        self.expect(&Token::Colon)?;
                        let value = self.parse_expr()?;
                        args.push((field_name, value));
                        if !self.eat(&Token::Comma) {
                            break;
                        }
                    }
                    self.expect(&Token::RightParen)?;
                    args
                } else {
                    Vec::new()
                };

                Expr::Spawn { target, args }
            }
            Token::Move => {
                self.advance();
                if self.eat(&Token::LeftParen) {
                    // Move lambda
                    let params = self.try_parse_lambda_params()?;
                    self.expect(&Token::RightParen)?;

                    let return_type = if self.eat(&Token::Arrow) {
                        Some(self.parse_type()?)
                    } else {
                        None
                    };

                    self.expect(&Token::FatArrow)?;
                    let body = Box::new(self.parse_expr()?);

                    Expr::Lambda {
                        is_move: true,
                        params,
                        return_type,
                        body,
                    }
                } else {
                    self.error("expected '(' after 'move'".to_string());
                    return None;
                }
            }
            Token::Return => {
                self.advance();
                // Return expressions are not typically parsed in expression context
                // This might be a parsing context issue - skip for now
                self.error("return statement in expression context".to_string());
                return None;
            }
            Token::Scope => {
                self.advance();
                // Reject old scope.method() syntax
                if self.eat(&Token::Dot) {
                    self.error(
                        "'scope.method()' syntax has been removed; use 'scope |s| { s.method() }' instead"
                            .to_string(),
                    );
                    return None;
                }
                // Parse optional binding: scope |s| { ... }
                let binding = if self.eat(&Token::Pipe) {
                    let name = self.expect_ident()?;
                    self.expect(&Token::Pipe)?;
                    Some(name)
                } else {
                    None
                };
                // Parse the scope body with the binding active
                let prev_binding = self.scope_binding.take();
                self.scope_binding.clone_from(&binding);
                let body = self.parse_block()?;
                self.scope_binding = prev_binding;
                Expr::Scope { binding, body }
            }
            Token::Try => {
                self.error(
                    "'try'/'catch' blocks have been removed; use the '?' operator instead"
                        .to_string(),
                );
                return None;
            }
            Token::Unsafe => {
                self.advance();
                Expr::Unsafe(self.parse_block()?)
            }
            Token::Select => {
                self.advance();
                self.expect(&Token::LeftBrace)?;

                let mut arms = Vec::new();
                let mut timeout = None;
                while !self.at_end() && self.peek() != Some(&Token::RightBrace) {
                    if self.peek() == Some(&Token::After)
                        || matches!(self.peek(), Some(Token::Identifier(s)) if *s == "after")
                    {
                        self.advance();
                        let duration = self.parse_expr()?;
                        self.expect(&Token::FatArrow)?;
                        let body = self.parse_expr()?;
                        self.eat(&Token::Comma);
                        timeout = Some(Box::new(TimeoutClause {
                            duration: Box::new(duration),
                            body: Box::new(body),
                        }));
                        break;
                    }
                    arms.push(self.parse_select_arm()?);
                }

                self.expect(&Token::RightBrace)?;

                Expr::Select { arms, timeout }
            }
            Token::Race => {
                self.error("'race' blocks have been removed; use 'select' instead".to_string());
                return None;
            }
            Token::Join => {
                self.advance();
                // Accept either parentheses or braces for join
                let (open, close) = if self.peek() == Some(&Token::LeftBrace) {
                    (Token::LeftBrace, Token::RightBrace)
                } else {
                    (Token::LeftParen, Token::RightParen)
                };
                self.expect(&open)?;

                let mut exprs = Vec::new();
                while !self.at_end() && self.peek() != Some(&close) {
                    exprs.push(self.parse_expr()?);
                    if !self.eat(&Token::Comma) {
                        break;
                    }
                }

                self.expect(&close)?;
                Expr::Join(exprs)
            }
            Token::Yield => {
                self.advance();
                let value = if matches!(self.peek(), Some(Token::Semicolon | Token::RightBrace)) {
                    None
                } else {
                    Some(Box::new(self.parse_expr()?))
                };
                Expr::Yield(value)
            }
            Token::Cooperate => {
                self.advance();
                Expr::Cooperate
            }
            // Contextual keywords that can be used as identifiers in expressions
            tok if Self::contextual_keyword_name(&tok).is_some() => {
                let name = Self::contextual_keyword_name(&tok).unwrap();
                self.advance();
                Expr::Identifier(name.to_string())
            }
            _ => {
                let found = match self.peek() {
                    Some(tok) => format!("{tok}"),
                    None => "end of file".to_string(),
                };
                self.error(format!("expected expression, found {found}"));
                return None;
            }
        };

        let end = self.peek_span().start;
        Some((expr, start..end))
    }

    fn try_parse_lambda_params(&mut self) -> Option<Vec<LambdaParam>> {
        let mut params = Vec::new();

        while !self.at_end() && self.peek() != Some(&Token::RightParen) {
            let name = self.expect_ident()?;

            let ty = if self.eat(&Token::Colon) {
                Some(self.parse_type()?)
            } else {
                None
            };

            params.push(LambdaParam { name, ty });

            if !self.eat(&Token::Comma) {
                break;
            }
        }

        Some(params)
    }

    fn parse_dot_postfix(&mut self, lhs: Spanned<Expr>) -> Option<Spanned<Expr>> {
        let start = lhs.1.start;
        self.advance(); // consume .

        // Handle tuple index: t.0, t.1, etc.
        if let Some(Token::Integer(n)) = self.peek() {
            let field = n.to_string();
            self.advance();
            let end = self.peek_span().start;
            return Some((
                Expr::FieldAccess {
                    object: Box::new(lhs),
                    field,
                },
                start..end,
            ));
        }

        let field = self.expect_ident()?;

        // Check for method call
        if self.peek() == Some(&Token::LeftParen) {
            self.advance();
            let args = self.parse_call_args()?;

            self.expect(&Token::RightParen)?;
            let end = self.peek_span().start;

            Some((
                Expr::MethodCall {
                    receiver: Box::new(lhs),
                    method: field,
                    args,
                },
                start..end,
            ))
        } else {
            // Field access
            let end = self.peek_span().start;
            Some((
                Expr::FieldAccess {
                    object: Box::new(lhs),
                    field,
                },
                start..end,
            ))
        }
    }

    /// Parse a comma-separated list of call arguments, supporting both
    /// positional (`expr`) and named (`name: expr`) forms.
    ///
    /// Named args must come after all positional args.
    fn parse_call_args(&mut self) -> Option<Vec<CallArg>> {
        let mut args = Vec::new();
        let mut seen_named = false;

        while !self.at_end() && self.peek() != Some(&Token::RightParen) {
            // Check for named arg: identifier followed by colon.
            // We peek at the current token AND the next one to distinguish
            // `name: expr` from a plain expression that starts with an identifier.
            let is_named_arg = self.peek().is_some_and(|t| Self::is_ident_token(t))
                && self.peek_at(self.pos + 1) == Some(&Token::Colon);

            if is_named_arg {
                let name = self.expect_ident()?;
                self.advance(); // consume ':'
                let value = self.parse_expr()?;
                args.push(CallArg::Named { name, value });
                seen_named = true;
                if !self.eat(&Token::Comma) {
                    break;
                }
                continue;
            }

            // Positional argument
            if seen_named {
                self.error("positional arguments must come before named arguments".to_string());
            }
            match self.parse_expr() {
                Some(expr) => args.push(CallArg::Positional(expr)),
                None => break,
            }
            if !self.eat(&Token::Comma) {
                break;
            }
        }

        Some(args)
    }

    fn parse_call_postfix(&mut self, lhs: Spanned<Expr>) -> Option<Spanned<Expr>> {
        let start = lhs.1.start;
        self.advance(); // consume (

        let args = self.parse_call_args()?;

        self.expect(&Token::RightParen)?;
        let end = self.peek_span().start;

        Some((
            Expr::Call {
                function: Box::new(lhs),
                type_args: None, // postfix calls don't parse type args (ambiguous with <)
                args,
                is_tail_call: false,
            },
            start..end,
        ))
    }

    fn parse_index_postfix(&mut self, lhs: Spanned<Expr>) -> Option<Spanned<Expr>> {
        let start = lhs.1.start;
        self.advance(); // consume [

        let index = self.parse_expr()?;
        self.expect(&Token::RightBracket)?;
        let end = self.peek_span().start;

        Some((
            Expr::Index {
                object: Box::new(lhs),
                index: Box::new(index),
            },
            start..end,
        ))
    }

    // ── Patterns ──
    fn parse_pattern(&mut self) -> Option<Spanned<Pattern>> {
        let _guard = self.enter_recursion()?;
        let mut result = self.parse_base_pattern()?;

        // Handle OR patterns: `1 | 2 | 3` becomes Or(Or(1, 2), 3)
        while self.peek() == Some(&Token::Pipe) {
            self.advance();
            let right = self.parse_base_pattern()?;
            let span = result.1.start..right.1.end;
            result = (Pattern::Or(Box::new(result), Box::new(right)), span);
        }

        Some(result)
    }

    #[expect(
        clippy::too_many_lines,
        reason = "recursive descent parser requires sequential case handling"
    )]
    fn parse_base_pattern(&mut self) -> Option<Spanned<Pattern>> {
        let _guard = self.enter_recursion()?;
        let start = self.peek_span().start;

        let pattern = match self.peek() {
            Some(Token::Identifier(name)) => {
                let mut name = name.to_string();
                self.advance();
                // Wildcard pattern
                if name == "_" {
                    Pattern::Wildcard
                } else {
                    // Handle qualified names like Color::Red
                    while self.eat(&Token::DoubleColon) {
                        if let Some(segment) = self.expect_ident() {
                            name = format!("{name}::{segment}");
                        } else {
                            break;
                        }
                    }

                    if self.eat(&Token::LeftParen) {
                        // Constructor pattern
                        let mut patterns = Vec::new();
                        while !self.at_end() && self.peek() != Some(&Token::RightParen) {
                            patterns.push(self.parse_pattern()?);
                            if !self.eat(&Token::Comma) {
                                break;
                            }
                        }
                        self.expect(&Token::RightParen)?;
                        Pattern::Constructor { name, patterns }
                    } else if self.eat(&Token::LeftBrace) {
                        // Struct pattern
                        let mut fields = Vec::new();
                        while !self.at_end() && self.peek() != Some(&Token::RightBrace) {
                            let field_name = self.expect_ident()?;
                            let pattern = if self.eat(&Token::Colon) {
                                Some(self.parse_pattern()?)
                            } else {
                                None
                            };
                            fields.push(PatternField {
                                name: field_name,
                                pattern,
                            });

                            if !self.eat(&Token::Comma) {
                                break;
                            }
                        }
                        self.expect(&Token::RightBrace)?;
                        Pattern::Struct { name, fields }
                    } else {
                        Pattern::Identifier(name)
                    }
                }
            }
            Some(Token::LeftParen) => {
                self.advance();
                if self.eat(&Token::RightParen) {
                    // Unit pattern - represented as empty tuple
                    Pattern::Tuple(Vec::new())
                } else {
                    let mut patterns = vec![self.parse_pattern()?];
                    while self.eat(&Token::Comma) {
                        if self.peek() == Some(&Token::RightParen) {
                            break;
                        }
                        patterns.push(self.parse_pattern()?);
                    }
                    self.expect(&Token::RightParen)?;

                    if patterns.len() == 1 {
                        return Some(patterns.into_iter().next().unwrap());
                    }
                    Pattern::Tuple(patterns)
                }
            }
            Some(Token::Integer(s)) => {
                let radix = detect_int_radix(s);
                if let Ok(val) = parse_int_literal(s) {
                    self.advance();
                    Pattern::Literal(Literal::Integer { value: val, radix })
                } else {
                    self.error_with_hint(
                        format!("invalid integer literal '{s}'"),
                        "integer literals support decimal, 0x hex, 0o octal, and 0b binary",
                    );
                    return None;
                }
            }
            Some(Token::StringLit(s)) => {
                let s = s
                    .strip_prefix('"')
                    .and_then(|s| s.strip_suffix('"'))
                    .unwrap_or(s);
                let s = unescape_string(s);
                self.advance();
                Pattern::Literal(Literal::String(s))
            }
            Some(Token::RawString(s)) => {
                let s = s
                    .strip_prefix("r\"")
                    .and_then(|s| s.strip_suffix('"'))
                    .unwrap_or(s)
                    .to_string();
                self.advance();
                Pattern::Literal(Literal::String(s))
            }
            Some(Token::True) => {
                self.advance();
                Pattern::Literal(Literal::Bool(true))
            }
            Some(Token::False) => {
                self.advance();
                Pattern::Literal(Literal::Bool(false))
            }
            // Contextual keywords used as identifiers in patterns
            Some(
                Token::After
                | Token::From
                | Token::Init
                | Token::Child
                | Token::Restart
                | Token::Budget
                | Token::Strategy
                | Token::Permanent
                | Token::Transient
                | Token::Temporary
                | Token::OneForOne
                | Token::OneForAll
                | Token::RestForOne
                | Token::Wire
                | Token::Optional
                | Token::Deprecated
                | Token::Reserved,
            ) => {
                let name = match self.peek().unwrap() {
                    Token::After => "after",
                    Token::From => "from",
                    Token::Init => "init",
                    Token::Child => "child",
                    Token::Restart => "restart",
                    Token::Budget => "budget",
                    Token::Strategy => "strategy",
                    Token::Permanent => "permanent",
                    Token::Transient => "transient",
                    Token::Temporary => "temporary",
                    Token::OneForOne => "one_for_one",
                    Token::OneForAll => "one_for_all",
                    Token::RestForOne => "rest_for_one",
                    Token::Wire => "wire",
                    Token::Optional => "optional",
                    Token::Deprecated => "deprecated",
                    Token::Reserved => "reserved",
                    _ => unreachable!(),
                }
                .to_string();
                self.advance();
                Pattern::Identifier(name)
            }
            _ => {
                let found = match self.peek() {
                    Some(tok) => format!("{tok}"),
                    None => "end of file".to_string(),
                };
                self.error(format!("expected pattern, found {found}"));
                return None;
            }
        };

        let end = self.peek_span().start;
        Some((pattern, start..end))
    }

    fn parse_match_arm(&mut self) -> Option<MatchArm> {
        let pattern = self.parse_pattern()?;

        let guard = if self.eat(&Token::If) {
            Some(self.parse_expr()?)
        } else {
            None
        };

        if self.peek() == Some(&Token::Equal) {
            self.error_with_hint(
                "expected '=>' in match arm, found '='".to_string(),
                "use '=>' (fat arrow) to separate pattern from body",
            );
            self.advance();
        } else {
            self.expect(&Token::FatArrow)?;
        }
        let body = self.parse_expr()?;
        if self.peek() == Some(&Token::RightBrace) {
            self.eat(&Token::Comma); // trailing comma optional on last arm
        } else {
            self.expect(&Token::Comma)?;
        }

        Some(MatchArm {
            pattern,
            guard,
            body,
        })
    }

    fn parse_select_arm(&mut self) -> Option<SelectArm> {
        let binding = self.parse_pattern()?;
        // Accept either `<-` or `from` for select arms
        if !self.eat(&Token::LeftArrow) {
            self.expect(&Token::From)?;
        }
        let source = self.parse_expr()?;
        self.expect(&Token::FatArrow)?;
        let body = self.parse_expr()?;
        self.eat(&Token::Comma);

        Some(SelectArm {
            binding,
            source,
            body,
        })
    }
}

// ── Precedence Functions ──

/// Get binding power for infix operators (left, right).
/// Higher numbers = tighter binding.
fn infix_bp(op: &Token) -> Option<(u8, u8)> {
    match op {
        // Send: lowest
        Token::LeftArrow => Some((1, 2)), // <- (right-assoc)
        // Logical OR
        Token::PipePipe => Some((3, 4)),
        // Bitwise OR
        Token::Pipe => Some((5, 6)),
        // Bitwise XOR
        Token::Caret => Some((7, 8)),
        // Bitwise AND
        Token::Ampersand => Some((9, 10)),
        // Logical AND
        Token::AmpAmp => Some((11, 12)),
        // Equality / regex match
        Token::EqualEqual | Token::NotEqual | Token::MatchOp | Token::NotMatchOp => Some((13, 14)),
        // Relational
        Token::Less | Token::LessEqual | Token::Greater | Token::GreaterEqual => Some((15, 16)),
        // Range
        Token::DotDot | Token::DotDotEqual => Some((17, 18)),
        // Shift
        Token::LessLess | Token::GreaterGreater => Some((19, 20)),
        // Additive
        Token::Plus | Token::Minus => Some((21, 22)),
        // Multiplicative
        Token::Star | Token::Slash | Token::Percent => Some((23, 24)),
        _ => None,
    }
}

fn prefix_bp(op: &Token) -> Option<u8> {
    match op {
        Token::Bang | Token::Minus | Token::Tilde | Token::Await => Some(25),
        _ => None,
    }
}

fn token_to_binop(token: &Token) -> Option<BinaryOp> {
    match token {
        Token::Plus => Some(BinaryOp::Add),
        Token::Minus => Some(BinaryOp::Subtract),
        Token::Star => Some(BinaryOp::Multiply),
        Token::Slash => Some(BinaryOp::Divide),
        Token::Percent => Some(BinaryOp::Modulo),
        Token::EqualEqual => Some(BinaryOp::Equal),
        Token::NotEqual => Some(BinaryOp::NotEqual),
        Token::Less => Some(BinaryOp::Less),
        Token::LessEqual => Some(BinaryOp::LessEqual),
        Token::Greater => Some(BinaryOp::Greater),
        Token::GreaterEqual => Some(BinaryOp::GreaterEqual),
        Token::AmpAmp => Some(BinaryOp::And),
        Token::PipePipe => Some(BinaryOp::Or),
        Token::Ampersand => Some(BinaryOp::BitAnd),
        Token::Pipe => Some(BinaryOp::BitOr),
        Token::Caret => Some(BinaryOp::BitXor),
        Token::LessLess => Some(BinaryOp::Shl),
        Token::GreaterGreater => Some(BinaryOp::Shr),
        Token::LeftArrow => Some(BinaryOp::Send),
        Token::DotDot => Some(BinaryOp::Range),
        Token::DotDotEqual => Some(BinaryOp::RangeInclusive),
        Token::MatchOp => Some(BinaryOp::RegexMatch),
        Token::NotMatchOp => Some(BinaryOp::RegexNotMatch),
        _ => None,
    }
}

// ── Public API ──

/// Parse a Hew source file into an AST with error reporting.
#[must_use]
pub fn parse(source: &str) -> ParseResult {
    let mut parser = Parser::new(source);
    let program = parser.parse_program();
    ParseResult {
        program,
        errors: parser.errors,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_simple_function() {
        let source = "fn main() { let x = 1 + 2; }";
        let result = parse(source);
        assert!(result.errors.is_empty());
        assert_eq!(result.program.items.len(), 1);
    }

    #[test]
    fn parse_doc_comment_on_function() {
        let source = "/// Adds numbers.\nfn add(a: i32, b: i32) -> i32 { a + b }";
        let result = parse(source);
        assert!(result.errors.is_empty());
        assert_eq!(result.program.items.len(), 1);
        if let Item::Function(f) = &result.program.items[0].0 {
            assert_eq!(f.doc_comment.as_deref(), Some("Adds numbers."));
        } else {
            panic!("expected function");
        }
    }

    #[test]
    fn parse_module_doc_comment() {
        let source = "//! Module docs.\n//! Line two.\nfn foo() {}";
        let result = parse(source);
        assert!(result.errors.is_empty());
        assert_eq!(
            result.program.module_doc.as_deref(),
            Some("Module docs.\nLine two.")
        );
    }

    #[test]
    fn parse_no_doc_comment() {
        let source = "fn bare() {}";
        let result = parse(source);
        assert!(result.errors.is_empty());
        if let Item::Function(f) = &result.program.items[0].0 {
            assert!(f.doc_comment.is_none());
        } else {
            panic!("expected function");
        }
    }

    #[test]
    fn parse_struct_decl() {
        let source = "type Point { x: i32; y: i32; }";
        let result = parse(source);
        assert!(result.errors.is_empty());
        assert_eq!(result.program.items.len(), 1);
    }

    #[test]
    fn parse_actor_decl() {
        let source =
            "actor Counter { count: i32; receive fn increment() { self.count = self.count + 1; } }";
        let result = parse(source);
        assert!(result.errors.is_empty());
        assert_eq!(result.program.items.len(), 1);
    }

    #[test]
    fn parse_receive_gen_fn() {
        let source = "actor NumberStream { receive gen fn numbers() -> i32 { yield 1; } }";
        let result = parse(source);
        assert!(result.errors.is_empty());
        if let Item::Actor(actor) = &result.program.items[0].0 {
            assert_eq!(actor.receive_fns.len(), 1);
            assert!(actor.receive_fns[0].is_generator);
        } else {
            panic!("expected actor item");
        }
    }

    #[test]
    fn parse_receive_fn_type_params_and_where_clause() {
        let source = "actor Foo { receive fn bar<T>(x: T) -> T where T: Display { x } }";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        if let Item::Actor(actor) = &result.program.items[0].0 {
            assert_eq!(actor.receive_fns.len(), 1);
            let rf = &actor.receive_fns[0];
            assert_eq!(rf.name, "bar");
            let tps = rf.type_params.as_ref().expect("expected type_params");
            assert_eq!(tps.len(), 1);
            assert_eq!(tps[0].name, "T");
            let wc = rf.where_clause.as_ref().expect("expected where_clause");
            assert_eq!(wc.predicates.len(), 1);
            assert_eq!(wc.predicates[0].bounds[0].name, "Display");
        } else {
            panic!("expected actor item");
        }
    }

    #[test]
    fn parse_if_expression() {
        let source = "fn main() { let result = if x > 0 { x } else { -x }; }";
        let result = parse(source);
        if !result.errors.is_empty() {
            for error in &result.errors {
                eprintln!("Error: {} at {:?}", error.message, error.span);
            }
        }
        assert!(result.errors.is_empty());
    }

    #[test]
    fn parse_match_expression() {
        let source = "fn main() { match opt { Some(x) => x, None => 0, } }";
        let result = parse(source);
        assert!(result.errors.is_empty());
    }

    #[test]
    fn parse_lambda() {
        let source = "fn main() { let f = (x: i32) => x * 2; }";
        let result = parse(source);
        if !result.errors.is_empty() {
            for error in &result.errors {
                eprintln!("Error: {} at {:?}", error.message, error.span);
            }
        }
        assert!(result.errors.is_empty());
    }

    #[test]
    fn parse_fibonacci_example() {
        let source = include_str!("../../examples/fibonacci.hew");
        let result = parse(source);
        if !result.errors.is_empty() {
            for error in &result.errors {
                eprintln!("Error: {} at {:?}", error.message, error.span);
            }
        }
        assert!(result.errors.is_empty());
    }

    #[test]
    fn parse_match_or_pattern() {
        let source = "fn classify(n: i32) -> i32 { match n { 1 | 2 | 3 => 1, _ => 0, } }";
        let result = parse(source);
        if !result.errors.is_empty() {
            for error in &result.errors {
                eprintln!("Error: {} at {:?}", error.message, error.span);
            }
        }
        assert!(result.errors.is_empty());
    }

    #[test]
    fn parse_labeled_while_break_continue() {
        let source = r#"fn main() -> i32 {
            var i = 0;
            @outer: while i < 5 {
                var j = 0;
                while j < 5 {
                    if j == 3 { break @outer; }
                    j = j + 1;
                }
                i = i + 1;
            }
            0
        }"#;
        let result = parse(source);
        for e in &result.errors {
            eprintln!("Error: {} at {:?}", e.message, e.span);
        }
        assert!(result.errors.is_empty());

        // Verify the label was parsed
        if let Item::Function(ref f) = result.program.items[0].0 {
            // Third statement should be the labeled while
            if let Stmt::While { ref label, .. } = f.body.stmts[1].0 {
                assert_eq!(label.as_deref(), Some("outer"));
            } else {
                panic!("expected While statement");
            }
        } else {
            panic!("expected Function item");
        }
    }

    #[test]
    fn parse_labeled_loop() {
        let source = r#"fn main() -> i32 {
            @top: loop {
                break @top;
            }
            0
        }"#;
        let result = parse(source);
        assert!(result.errors.is_empty());
        if let Item::Function(ref f) = result.program.items[0].0 {
            if let Stmt::Loop { ref label, .. } = f.body.stmts[0].0 {
                assert_eq!(label.as_deref(), Some("top"));
            } else {
                panic!("expected Loop statement");
            }
        }
    }

    #[test]
    fn parse_labeled_continue() {
        let source = r#"fn main() -> i32 {
            var i = 0;
            @outer: while i < 5 {
                i = i + 1;
                continue @outer;
            }
            0
        }"#;
        let result = parse(source);
        for e in &result.errors {
            eprintln!("Error: {} at {:?}", e.message, e.span);
        }
        assert!(result.errors.is_empty());
    }

    #[test]
    fn parse_for_await_loop() {
        let source = r#"fn main() {
            for await item in stream {
                println(item);
            }
        }"#;
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);

        if let Item::Function(ref f) = result.program.items[0].0 {
            if let Stmt::For { is_await, .. } = &f.body.stmts[0].0 {
                assert!(*is_await);
            } else {
                panic!("expected For statement");
            }
        } else {
            panic!("expected Function item");
        }
    }

    #[test]
    fn parse_async_gen_fn() {
        let source = "async gen fn count_up() -> i32 { yield 1; yield 2; }";
        let result = parse(source);
        for _e in &result.errors {}
        match &result.program.items[0].0 {
            Item::Function(_f) => {}
            _ => panic!("expected Function item"),
        }
    }

    #[test]
    fn parse_pub_async_gen_fn() {
        let source = "pub async gen fn numbers() -> i32 { yield 42; }";
        let result = parse(source);
        match &result.program.items[0].0 {
            Item::Function(_f) => {}
            _ => panic!("expected Function item"),
        }
    }

    #[test]
    fn parse_pattern_underscore_integer() {
        let source = "fn main() { match x { 1_000 => 1, _ => 0, } }";
        let result = parse(source);
        assert!(result.errors.is_empty());
    }

    #[test]
    fn parse_lexer_error_reported() {
        // The backtick is not a valid token; it should produce a parse error
        let source = "fn main() { let x = `; }";
        let result = parse(source);
        assert!(result
            .errors
            .iter()
            .any(|e| e.message.contains("unexpected character")));
    }

    #[test]
    fn parse_string_escape_sequences() {
        let source = r#"fn main() -> i32 { let a = "hello\nworld"; let b = "tab\there"; let c = "quote\"end"; let d = "back\\slash"; let e = "null\0byte"; 0 }"#;
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        let body = match &result.program.items[0].0 {
            Item::Function(f) => &f.body,
            _ => panic!("expected Function item"),
        };
        let stmts = &body.stmts;
        let get_str = |idx: usize| -> &str {
            if let (
                Stmt::Let {
                    value: Some(val), ..
                },
                _,
            ) = &stmts[idx]
            {
                if let (Expr::Literal(Literal::String(s)), _) = val {
                    return s.as_str();
                }
            }
            panic!("expected let with string literal at index {}", idx);
        };
        assert_eq!(get_str(0), "hello\nworld");
        assert_eq!(get_str(1), "tab\there");
        assert_eq!(get_str(2), "quote\"end");
        assert_eq!(get_str(3), "back\\slash");
        assert_eq!(get_str(4), "null\0byte");
    }

    #[test]
    fn parse_interpolated_string_contains_expr_part() {
        let result = parse(r#"fn main() { let s = f"hello {name}"; }"#);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        let Item::Function(f) = &result.program.items[0].0 else {
            panic!("expected function");
        };
        let Stmt::Let {
            value: Some((Expr::InterpolatedString(parts), _)),
            ..
        } = &f.body.stmts[0].0
        else {
            panic!("expected interpolated string");
        };
        assert!(parts.iter().any(|p| matches!(p, StringPart::Expr(_))));
    }

    #[test]
    #[ignore = "known gap: interpolation parse errors silently dropped (sub-parser errors not propagated)"]
    fn parse_interpolated_string_empty_expr_reports_error() {
        let result = parse(r#"fn main() { let s = f"hello {}"; }"#);
        assert!(
            !result.errors.is_empty(),
            "expected parse errors for malformed interpolation"
        );
    }

    #[test]
    fn parse_deeply_nested_expr_produces_error() {
        // 300 levels of parenthesized nesting exceeds MAX_DEPTH (256).
        // Use a child thread with an explicit stack size to avoid the test
        // runner's own stack limit being hit before our guard triggers.
        let result = std::thread::Builder::new()
            .stack_size(16 * 1024 * 1024)
            .spawn(|| {
                let open: String = "(".repeat(300);
                let close: String = ")".repeat(300);
                let source = format!("fn main() -> i32 {{ {open}1{close} }}");
                parse(&source)
            })
            .expect("failed to spawn thread")
            .join()
            .expect("thread panicked");

        assert!(
            result
                .errors
                .iter()
                .any(|e| e.message.contains("maximum nesting depth exceeded")),
            "expected nesting depth error, got: {:?}",
            result.errors
        );
    }

    #[test]
    fn parse_error_missing_brace() {
        let source = "fn main() -> i32 {\n    let x = 42;\n    0\n";
        let result = parse(source);
        assert!(
            result.errors.iter().any(|e| e.message.contains("`}`")),
            "expected `}}` error, got: {:?}",
            result.errors
        );
    }

    #[test]
    fn parse_error_unexpected_token() {
        let source = "fn main() {\n    let x = 42 + + + ;\n}";
        let result = parse(source);
        assert!(
            !result.errors.is_empty(),
            "expected parse errors for unexpected tokens"
        );
    }

    #[test]
    fn parse_error_unclosed_string() {
        let source = "fn main() {\n    println(\"hello world);\n}";
        let result = parse(source);
        assert!(
            !result.errors.is_empty(),
            "expected parse errors for unclosed string"
        );
    }

    #[test]
    fn parse_error_missing_expr() {
        let source = "fn main() {\n    let x = ;\n}";
        let result = parse(source);
        assert!(
            result
                .errors
                .iter()
                .any(|e| e.message.contains("expected expression")),
            "expected 'expected expression' error, got: {:?}",
            result.errors
        );
    }

    #[test]
    fn parse_error_missing_semicolon() {
        let source = "fn main() {\n    let x = 42\n    let y = 10;\n}";
        let result = parse(source);
        assert!(
            result.errors.iter().any(|e| e.message.contains("expected")),
            "expected error about missing semicolon, got: {:?}",
            result.errors
        );
    }

    // -----------------------------------------------------------------------
    // Edge case tests: nested generics, chained methods, complex expressions
    // -----------------------------------------------------------------------

    #[test]
    fn parse_nested_generic_types() {
        let source = "fn main() { let v: Vec<Vec<i32>> = Vec::new(); }";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    }

    #[test]
    fn parse_deeply_nested_generics() {
        let source = "fn main() { let v: HashMap<string, Vec<Vec<i32>>> = HashMap::new(); }";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    }

    #[test]
    fn parse_chained_method_calls() {
        let source = "fn main() { a.b().c().d(); }";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    }

    #[test]
    fn parse_chained_methods_with_args() {
        let source = "fn main() { x.filter(1).map(2).collect(); }";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    }

    #[test]
    fn parse_operator_precedence_complex() {
        let source = "fn main() -> i32 { x + y * z - w / v }";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        // Verify the trailing expression exists (the complex expression)
        if let Item::Function(f) = &result.program.items[0].0 {
            assert!(f.body.trailing_expr.is_some());
        } else {
            panic!("expected function");
        }
    }

    #[test]
    fn parse_mixed_precedence_with_parens() {
        let source = "fn main() -> i32 { (a + b) * (c - d) / e }";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    }

    #[test]
    fn parse_empty_function_body() {
        let source = "fn noop() {}";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        if let Item::Function(f) = &result.program.items[0].0 {
            assert!(f.body.stmts.is_empty());
            assert!(f.body.trailing_expr.is_none());
        } else {
            panic!("expected function");
        }
    }

    #[test]
    fn parse_empty_actor_body() {
        let source = "actor Empty {}";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        if let Item::Actor(a) = &result.program.items[0].0 {
            assert!(a.fields.is_empty());
            assert!(a.receive_fns.is_empty());
        } else {
            panic!("expected actor");
        }
    }

    #[test]
    fn parse_unicode_in_string_literal() {
        let source = r#"fn main() { let s = "Hello, 世界! 🦀"; }"#;
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        if let Item::Function(f) = &result.program.items[0].0 {
            if let (
                Stmt::Let {
                    value: Some(val), ..
                },
                _,
            ) = &f.body.stmts[0]
            {
                if let (Expr::Literal(Literal::String(s)), _) = val {
                    assert!(s.contains("世界"));
                    assert!(s.contains("🦀"));
                } else {
                    panic!("expected string literal");
                }
            }
        }
    }

    #[test]
    fn parse_empty_string_literal() {
        let source = r#"fn main() { let s = ""; }"#;
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    }

    #[test]
    fn parse_large_integer_literal() {
        let source = "fn main() -> i64 { 9_223_372_036_854_775_807 }";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        if let Item::Function(f) = &result.program.items[0].0 {
            if let Some(boxed) = &f.body.trailing_expr {
                if let (Expr::Literal(Literal::Integer { value: n, .. }), _) = boxed.as_ref() {
                    assert_eq!(*n, i64::MAX);
                } else {
                    panic!("expected integer literal");
                }
            }
        }
    }

    #[test]
    fn parse_hex_integer_literal() {
        let source = "fn main() -> i64 { 0xFF }";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        if let Item::Function(f) = &result.program.items[0].0 {
            if let Some(boxed) = &f.body.trailing_expr {
                if let (Expr::Literal(Literal::Integer { value: n, radix }), _) = boxed.as_ref() {
                    assert_eq!(*n, 255);
                    assert_eq!(*radix, IntRadix::Hex);
                } else {
                    panic!("expected integer literal");
                }
            }
        }
    }

    #[test]
    fn parse_binary_integer_literal() {
        let source = "fn main() -> i64 { 0b1010 }";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        if let Item::Function(f) = &result.program.items[0].0 {
            if let Some(boxed) = &f.body.trailing_expr {
                if let (Expr::Literal(Literal::Integer { value: n, radix }), _) = boxed.as_ref() {
                    assert_eq!(*n, 10);
                    assert_eq!(*radix, IntRadix::Binary);
                } else {
                    panic!("expected integer literal");
                }
            }
        }
    }

    #[test]
    fn parse_octal_integer_literal() {
        let source = "fn main() -> i64 { 0o77 }";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        if let Item::Function(f) = &result.program.items[0].0 {
            if let Some(boxed) = &f.body.trailing_expr {
                if let (Expr::Literal(Literal::Integer { value: n, radix }), _) = boxed.as_ref() {
                    assert_eq!(*n, 63);
                    assert_eq!(*radix, IntRadix::Octal);
                } else {
                    panic!("expected integer literal");
                }
            }
        }
    }

    #[test]
    fn parse_multiple_items() {
        let source = "fn foo() {} fn bar() {} type Baz {}";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        assert_eq!(result.program.items.len(), 3);
    }

    #[test]
    fn parse_empty_program() {
        let result = parse("");
        assert!(result.errors.is_empty());
        assert!(result.program.items.is_empty());
    }

    #[test]
    fn parse_nested_if_else() {
        let source = "fn main() -> i32 { if a > 0 { if b > 0 { 1 } else { 2 } } else { 3 } }";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    }

    #[test]
    fn parse_match_with_struct_pattern() {
        let source = "fn main() { match p { Point { x, y } => x + y, _ => 0, } }";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    }

    #[test]
    fn parse_tuple_expression() {
        let source = "fn main() { let t = (1, 2, 3); }";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    }

    #[test]
    fn parse_array_expression() {
        let source = "fn main() { let a = [1, 2, 3]; }";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    }

    #[test]
    fn parse_unary_operators() {
        let source = "fn main() { let a = -x; let b = !flag; }";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    }

    #[test]
    fn parse_comparison_chain() {
        let source = "fn main() -> bool { a < b && b > c || d == e && f != g }";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    }

    #[test]
    fn parse_import_statement() {
        let source = "import std::fs;";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        if let Item::Import(imp) = &result.program.items[0].0 {
            assert_eq!(imp.path, vec!["std", "fs"]);
        } else {
            panic!("expected import");
        }
    }

    #[test]
    fn parse_trait_declaration() {
        let source = "trait Printable { fn print(self); }";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    }

    #[test]
    fn parse_error_duplicate_keyword() {
        let source = "fn fn main() {}";
        let result = parse(source);
        assert!(
            !result.errors.is_empty(),
            "expected parse error for duplicate fn keyword"
        );
    }

    // Duration literal parsing
    #[test]
    fn parse_duration_literals() {
        let source = "fn main() { let a = 100ms; let b = 5s; let c = 1m; let d = 2h; }";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        if let Item::Function(f) = &result.program.items[0].0 {
            let stmts = &f.body.stmts;
            assert_eq!(stmts.len(), 4);
            // 100ms → 100_000_000
            if let Stmt::Let {
                value: Some((Expr::Literal(Literal::Duration(ns)), _)),
                ..
            } = &stmts[0].0
            {
                assert_eq!(*ns, 100_000_000);
            } else {
                panic!("expected Duration literal for 100ms");
            }
            // 5s → 5_000_000_000
            if let Stmt::Let {
                value: Some((Expr::Literal(Literal::Duration(ns)), _)),
                ..
            } = &stmts[1].0
            {
                assert_eq!(*ns, 5_000_000_000);
            } else {
                panic!("expected Duration literal for 5s");
            }
            // 1m → 60_000_000_000
            if let Stmt::Let {
                value: Some((Expr::Literal(Literal::Duration(ns)), _)),
                ..
            } = &stmts[2].0
            {
                assert_eq!(*ns, 60_000_000_000);
            } else {
                panic!("expected Duration literal for 1m");
            }
            // 2h → 7_200_000_000_000
            if let Stmt::Let {
                value: Some((Expr::Literal(Literal::Duration(ns)), _)),
                ..
            } = &stmts[3].0
            {
                assert_eq!(*ns, 7_200_000_000_000);
            } else {
                panic!("expected Duration literal for 2h");
            }
        } else {
            panic!("expected function");
        }
    }

    #[test]
    fn parse_foreign_block() {
        // `foreign` is no longer accepted — only `extern` is valid
        let source = "foreign { fn ext_add(a: i32, b: i32) -> i32; fn ext_print(msg: string); }";
        let result = parse(source);
        assert!(
            result
                .errors
                .iter()
                .any(|e| e.message.contains("unexpected 'foreign'")
                    && e.hint
                        .as_deref()
                        .is_some_and(|h| h.contains("use 'extern'"))),
            "expected foreign rejection error, got: {:?}",
            result.errors
        );

        // Verify that `extern` still works for the same purpose
        let source =
            "extern \"C\" { fn ext_add(a: i32, b: i32) -> i32; fn ext_print(msg: string); }";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        assert_eq!(result.program.items.len(), 1);
    }

    #[test]
    fn parse_timeout_combinator() {
        let source = "fn main() { let r = foo() | after 5000; }";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        // Check the expression is Timeout wrapping a call
        let stmt = &result.program.items[0];
        if let Item::Function(f) = &stmt.0 {
            if let (
                Stmt::Let {
                    value: Some(val), ..
                },
                _,
            ) = &f.body.stmts[0]
            {
                assert!(
                    matches!(val.0, Expr::Timeout { .. }),
                    "expected Timeout, got {:?}",
                    val.0
                );
            } else {
                panic!("expected let binding");
            }
        } else {
            panic!("expected FnDecl");
        }
    }
    // -- Import aliasing --

    #[test]
    fn parse_import_alias() {
        let source = r#"import std::net::{http as h, websocket as ws};"#;
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        if let Item::Import(imp) = &result.program.items[0].0 {
            assert_eq!(imp.path, vec!["std", "net"]);
            if let Some(ImportSpec::Names(names)) = &imp.spec {
                assert_eq!(names.len(), 2);
                assert_eq!(names[0].name, "http");
                assert_eq!(names[0].alias.as_deref(), Some("h"));
                assert_eq!(names[1].name, "websocket");
                assert_eq!(names[1].alias.as_deref(), Some("ws"));
            } else {
                panic!("expected ImportSpec::Names, got {:?}", imp.spec);
            }
        } else {
            panic!("expected import item");
        }
    }

    #[test]
    fn parse_import_alias_single() {
        let source = r#"import mymod::{foo as bar};"#;
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        if let Item::Import(imp) = &result.program.items[0].0 {
            if let Some(ImportSpec::Names(names)) = &imp.spec {
                assert_eq!(names.len(), 1);
                assert_eq!(names[0].name, "foo");
                assert_eq!(names[0].alias.as_deref(), Some("bar"));
            } else {
                panic!("expected ImportSpec::Names");
            }
        } else {
            panic!("expected import item");
        }
    }

    #[test]
    fn parse_import_no_alias_preserves_name() {
        // Names without `as` should have alias = None
        let source = r#"import mymod::{foo, bar};"#;
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        if let Item::Import(imp) = &result.program.items[0].0 {
            if let Some(ImportSpec::Names(names)) = &imp.spec {
                assert_eq!(names.len(), 2);
                assert_eq!(names[0].name, "foo");
                assert!(names[0].alias.is_none());
                assert_eq!(names[1].name, "bar");
                assert!(names[1].alias.is_none());
            } else {
                panic!("expected ImportSpec::Names");
            }
        } else {
            panic!("expected import item");
        }
    }

    #[test]
    fn parse_import_bare_colons_rejected() {
        // `import foo::;` is syntactically invalid — `::` must be followed by `*` or `{`
        let source = r#"import foo::;"#;
        let result = parse(source);
        assert!(
            !result.errors.is_empty(),
            "expected parse error for `import foo::;`"
        );
    }

    #[test]
    fn parse_import_glob() {
        let source = r#"import utils::*;"#;
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        if let Item::Import(imp) = &result.program.items[0].0 {
            assert_eq!(imp.spec, Some(ImportSpec::Glob));
        } else {
            panic!("expected import item");
        }
    }

    #[test]
    fn parse_float_with_underscore_separators() {
        let source = "fn main() { let x = 1_000.5; }";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    }

    #[test]
    fn parse_raw_string_literal() {
        let source = r#"fn main() { let x = r"hello\nworld"; }"#;
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    }
    #[test]
    fn test_hex_escape_in_string() {
        // \x41 = 'A', \x42 = 'B'
        assert_eq!(unescape_string(r"\x41\x42"), "AB");
        // Mixed with normal text and other escapes
        assert_eq!(unescape_string(r"hi\x21\n"), "hi!\n");
        // Invalid hex digits preserved as-is
        assert_eq!(unescape_string(r"\xZZ"), "\\xZZ");
        // Truncated hex escape (only one char after \x)
        assert_eq!(unescape_string("\\x4"), "\\x");
    }
} // mod tests
