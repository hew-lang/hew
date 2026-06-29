//! Shared cursor, token-advance, lookahead, error-recording, and recovery
//! primitives — the `impl Parser` methods every grammar area calls.

#[allow(
    clippy::wildcard_imports,
    reason = "grammar-area submodules share the parent parser namespace via the split"
)]
use super::*;

impl<'src> Parser<'src> {
    #[must_use]
    pub fn new(source: &'src str) -> Self {
        Self::new_with_offset(source, 0)
    }

    /// Build a parser whose every token span is shifted by `offset` bytes.
    ///
    /// Used when parsing a sub-expression extracted from an interpolated string:
    /// the lexer produces 0-based spans over the extracted text, but all spans
    /// must refer to the original source.  Shifting at construction time means
    /// that every AST node span — including deeply nested sub-nodes — and every
    /// parse error emitted by this parser is already absolute on return.
    pub(crate) fn new_with_offset(source: &'src str, offset: usize) -> Self {
        let raw_tokens = hew_lexer::lex(source);
        let mut errors = Vec::new();
        let mut tokens = Vec::new();
        for (t, s) in raw_tokens {
            let span = (s.start + offset)..(s.end + offset);
            if matches!(t, Token::Error) {
                errors.push(ParseError {
                    message: "unexpected character".to_string(),
                    span,
                    hint: None,
                    severity: Severity::Error,
                    kind: ParseDiagnosticKind::InvalidLiteral,
                });
            } else {
                tokens.push((t, span));
            }
        }
        Self {
            tokens,
            pos: 0,
            errors,
            depth: Rc::new(Cell::new(0)),
            angle_mutations: Vec::new(),
            allow_implicit_self_params: false,
            scope_expr_depth: 0,
            fork_block_depth: 0,
            no_struct_literal: Rc::new(Cell::new(false)),
            last_token_end: offset,
        }
    }

    // ── Helpers ──
    pub(crate) fn peek(&self) -> Option<&Token<'src>> {
        self.tokens.get(self.pos).map(|(t, _)| t)
    }

    pub(crate) fn peek_span(&self) -> Span {
        self.tokens
            .get(self.pos)
            .map_or(self.last_token_end..self.last_token_end, |(_, s)| s.clone())
    }

    pub(crate) fn advance(&mut self) -> Option<(Token<'src>, Span)> {
        if self.pos < self.tokens.len() {
            let tok = self.tokens[self.pos].clone();
            self.last_token_end = tok.1.end;
            self.pos += 1;
            Some(tok)
        } else {
            None
        }
    }

    pub(crate) fn expect(&mut self, expected: &Token<'_>) -> Option<Span> {
        if let Some(tok) = self.peek() {
            if std::mem::discriminant(tok) == std::mem::discriminant(expected) {
                let Some((_, span)) = self.advance() else {
                    self.error_unexpected_eof(expected);
                    return None;
                };
                return Some(span);
            }
        }
        let found = match self.peek() {
            Some(tok) => format!("{tok}"),
            None => "end of file".to_string(),
        };
        // Add a hint when a semicolon is expected but a statement keyword follows
        if matches!(expected, Token::Semicolon) && self.peek_starts_stmt() {
            self.error_unexpected_token_with_hint(
                format!("{expected}"),
                &found,
                "add `;` at the end of the previous statement",
            );
        } else {
            self.error_unexpected_token(format!("{expected}"), &found);
        }
        None
    }

    pub(crate) fn eat(&mut self, expected: &Token<'_>) -> bool {
        if let Some(tok) = self.peek() {
            if std::mem::discriminant(tok) == std::mem::discriminant(expected) {
                self.advance();
                return true;
            }
        }
        false
    }

    /// Returns true if the current token could start a new statement.
    pub(crate) fn peek_starts_stmt(&self) -> bool {
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

    pub(crate) fn at_end(&self) -> bool {
        self.pos >= self.tokens.len()
    }

    /// Peek at the token at an absolute position in the token stream.
    pub(crate) fn peek_at(&self, index: usize) -> Option<&Token<'src>> {
        self.tokens.get(index).map(|(t, _)| t)
    }

    /// Whether the current position is a contextual `clone <operand>` prefix.
    ///
    /// True only when the current token is the identifier `clone` AND the next
    /// token begins an operand (`token_begins_clone_operand`). When `clone` is
    /// followed by a continuation token (`.`, `(`, `[`, `?`, an infix operator,
    /// or a terminator) it stays an ordinary identifier — so `x.clone()`,
    /// `fn clone(...)`, and `clone(args)` are unaffected. The adjacency check
    /// is precedence-free: `clone x` was always a parse error before (two
    /// adjacent primaries), so repurposing it cannot change the meaning of any
    /// previously valid program.
    pub(crate) fn peek_is_clone_prefix(&self) -> bool {
        matches!(self.peek(), Some(Token::Identifier(name)) if *name == "clone")
            && self
                .peek_at(self.pos + 1)
                .is_some_and(token_begins_clone_operand)
    }

    /// Whether the current position begins a `consume <param>` modifier inside a
    /// parameter list. `consume` is a contextual keyword (lexed as an ordinary
    /// identifier, like `clone`): it carries the by-move modifier meaning only
    /// when it *leads* a parameter declaration — i.e. it is immediately followed
    /// by `var`, the receiver `self`, or the parameter name (another
    /// identifier). A bare `consume:` / `consume,` / `consume)` is an ordinary
    /// parameter *named* `consume`, so `fn f(consume: T)`, `x.consume()`, and
    /// `fn consume(...)` are all unaffected. The adjacency check is
    /// precedence-free: two adjacent identifiers (`consume name`) were never a
    /// valid single parameter before, so repurposing the lead identifier cannot
    /// change the meaning of any previously valid program.
    pub(crate) fn peek_is_consume_param_modifier(&self) -> bool {
        matches!(self.peek(), Some(Token::Identifier(name)) if *name == "consume")
            && matches!(
                self.peek_at(self.pos + 1),
                Some(Token::Var | Token::Identifier(_))
            )
    }

    /// Check whether the current token starts with `>` (i.e. is `>`, `>>`, `>=`, or `>>=`).
    /// Used in type-argument / type-parameter parsing so that `Vec<Vec<i32>>`
    /// works without requiring a space before `>>`.
    pub(crate) fn at_closing_angle(&self) -> bool {
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
    pub(crate) fn eat_closing_angle(&mut self) -> bool {
        let Some((tok, span)) = self.tokens.get(self.pos) else {
            return false;
        };
        match tok {
            Token::Greater => {
                // Update last_token_end before advancing so peek_span() at EOF
                // returns the position immediately after the `>` we just consumed.
                // This is the same invariant that advance() maintains; but
                // eat_closing_angle bypasses advance() to avoid re-cloning.
                self.last_token_end = span.end;
                self.pos += 1;
                true
            }
            Token::GreaterGreater => {
                // `>>` → consume first `>`, leave `>` for the outer context.
                // mid is the byte just past the first `>`.
                self.angle_mutations
                    .push((self.pos, self.tokens[self.pos].clone()));
                let mid = span.start + 1;
                let remaining_span = mid..span.end;
                // last use of `span`; NLL ends the borrow here.
                self.last_token_end = mid;
                self.tokens[self.pos] = (Token::Greater, remaining_span);
                true
            }
            Token::GreaterEqual => {
                // `>=` → consume `>`, leave `=`
                self.angle_mutations
                    .push((self.pos, self.tokens[self.pos].clone()));
                let mid = span.start + 1;
                let remaining_span = mid..span.end;
                self.last_token_end = mid;
                self.tokens[self.pos] = (Token::Equal, remaining_span);
                true
            }
            Token::GreaterGreaterEqual => {
                // `>>=` → consume first `>`, leave `>=`
                self.angle_mutations
                    .push((self.pos, self.tokens[self.pos].clone()));
                let mid = span.start + 1;
                let remaining_span = mid..span.end;
                self.last_token_end = mid;
                self.tokens[self.pos] = (Token::GreaterEqual, remaining_span);
                true
            }
            _ => false,
        }
    }

    pub(crate) fn error(&mut self, message: String) {
        let span = self.peek_span();
        self.error_at(message, span);
    }

    pub(crate) fn error_at(&mut self, message: String, span: Span) {
        self.errors.push(ParseError {
            message,
            span,
            hint: None,
            severity: Severity::Error,
            kind: ParseDiagnosticKind::Other,
        });
    }

    pub(crate) fn error_with_hint(&mut self, message: String, hint: impl Into<String>) {
        let span = self.peek_span();
        self.error_at_with_hint(message, span, hint);
    }

    pub(crate) fn error_at_with_hint(
        &mut self,
        message: String,
        span: Span,
        hint: impl Into<String>,
    ) {
        self.errors.push(ParseError {
            message,
            span,
            hint: Some(hint.into()),
            severity: Severity::Error,
            kind: ParseDiagnosticKind::Other,
        });
    }

    pub(crate) fn error_closure_pipe_syntax(
        &mut self,
        message: impl Into<String>,
        span: Span,
        hint: impl Into<String>,
    ) {
        self.errors.push(ParseError {
            message: message.into(),
            span,
            hint: Some(hint.into()),
            severity: Severity::Error,
            kind: ParseDiagnosticKind::ClosurePipeSyntax,
        });
    }

    /// Record an `UnexpectedToken` diagnostic at the current position.
    pub(crate) fn error_unexpected_token(
        &mut self,
        expected: impl Into<String>,
        got: impl Into<String>,
    ) {
        let span = self.peek_span();
        self.error_unexpected_token_at(expected, got, span);
    }

    pub(crate) fn error_unexpected_token_at(
        &mut self,
        expected: impl Into<String>,
        got: impl Into<String>,
        span: Span,
    ) {
        let expected = expected.into();
        let got = got.into();
        let message = format!("expected {expected}, found {got}");
        self.errors.push(ParseError {
            message,
            span,
            hint: None,
            severity: Severity::Error,
            kind: ParseDiagnosticKind::UnexpectedToken { expected, got },
        });
    }

    pub(crate) fn error_unexpected_token_with_hint(
        &mut self,
        expected: impl Into<String>,
        got: impl Into<String>,
        hint: impl Into<String>,
    ) {
        let span = self.peek_span();
        let expected = expected.into();
        let got = got.into();
        let message = format!("expected {expected}, found {got}");
        self.errors.push(ParseError {
            message,
            span,
            hint: Some(hint.into()),
            severity: Severity::Error,
            kind: ParseDiagnosticKind::UnexpectedToken { expected, got },
        });
    }

    pub(crate) fn error_unexpected_eof(&mut self, context: impl std::fmt::Display) {
        let span = self.peek_span();
        self.errors.push(ParseError {
            message: format!("unexpected end of input, expected {context}"),
            span,
            hint: None,
            severity: Severity::Error,
            kind: ParseDiagnosticKind::UnexpectedEof,
        });
    }

    pub(crate) fn error_invalid_literal(&mut self, message: String) {
        let span = self.peek_span();
        self.errors.push(ParseError {
            message,
            span,
            hint: None,
            severity: Severity::Error,
            kind: ParseDiagnosticKind::InvalidLiteral,
        });
    }

    pub(crate) fn error_missing_expression(&mut self, got: impl Into<String>) {
        let got = got.into();
        let span = self.peek_span();
        self.errors.push(ParseError {
            message: format!("expected expression, found {got}"),
            span,
            hint: None,
            severity: Severity::Error,
            kind: ParseDiagnosticKind::MissingExpression { got },
        });
    }

    pub(crate) fn error_invalid_pattern(&mut self, got: impl Into<String>) {
        let got = got.into();
        let span = self.peek_span();
        self.errors.push(ParseError {
            message: format!("expected pattern, found {got}"),
            span,
            hint: None,
            severity: Severity::Error,
            kind: ParseDiagnosticKind::InvalidPattern { got },
        });
    }

    pub(crate) fn error_invalid_literal_with_hint(
        &mut self,
        message: String,
        hint: impl Into<String>,
    ) {
        let span = self.peek_span();
        self.errors.push(ParseError {
            message,
            span,
            hint: Some(hint.into()),
            severity: Severity::Error,
            kind: ParseDiagnosticKind::InvalidLiteral,
        });
    }

    pub(crate) fn parse_char_escape(&mut self, s: &str) -> Option<char> {
        let mut chars = s.chars();
        let Some(c) = chars.next() else {
            self.error_invalid_literal("invalid char literal".to_string());
            return None;
        };
        let result = if c == '\\' {
            let Some(escaped) = chars.next() else {
                self.error_invalid_literal("invalid escape sequence".to_string());
                return None;
            };
            match escaped {
                'n' => '\n',
                't' => '\t',
                'r' => '\r',
                '0' => '\0',
                '\\' => '\\',
                '\'' => '\'',
                'x' => {
                    // \xHH — exactly two hex digits → a byte (ASCII scalar).
                    let hi = chars.next();
                    let lo = chars.next();
                    let (Some(hi), Some(lo)) = (hi, lo) else {
                        self.error_invalid_literal("invalid escape sequence".to_string());
                        return None;
                    };
                    let Ok(byte) = u8::from_str_radix(&format!("{hi}{lo}"), 16) else {
                        self.error_invalid_literal("invalid escape sequence".to_string());
                        return None;
                    };
                    byte as char
                }
                'u' => {
                    // \u{H...} — 1–6 hex digits forming a Unicode scalar value.
                    if chars.next() != Some('{') {
                        self.error_invalid_literal("invalid escape sequence".to_string());
                        return None;
                    }
                    let mut hex = String::new();
                    let mut closed = false;
                    for ch in chars.by_ref() {
                        if ch == '}' {
                            closed = true;
                            break;
                        }
                        hex.push(ch);
                    }
                    if !closed || hex.is_empty() || hex.len() > 6 {
                        self.error_invalid_literal("invalid Unicode escape".to_string());
                        return None;
                    }
                    let Ok(codepoint) = u32::from_str_radix(&hex, 16) else {
                        self.error_invalid_literal("invalid Unicode escape".to_string());
                        return None;
                    };
                    let Some(decoded) = char::from_u32(codepoint) else {
                        self.error_invalid_literal(
                            "invalid Unicode escape: not a valid scalar value".to_string(),
                        );
                        return None;
                    };
                    decoded
                }
                _ => {
                    self.error_invalid_literal("invalid escape sequence".to_string());
                    return None;
                }
            }
        } else {
            c
        };
        if chars.next().is_some() {
            self.error_invalid_literal("invalid char literal".to_string());
            return None;
        }
        Some(result)
    }

    pub(crate) fn warning_at(&mut self, message: String, span: Span) {
        self.errors.push(ParseError {
            message,
            span,
            hint: None,
            severity: Severity::Warning,
            kind: ParseDiagnosticKind::Other,
        });
    }

    /// Increment recursion depth and return a guard that decrements on drop.
    /// Returns `None` (after recording an error) if `MAX_DEPTH` is exceeded.
    pub(crate) fn enter_recursion(&mut self) -> Option<RecursionGuard> {
        let d = self.depth.get() + 1;
        self.depth.set(d);
        if d > MAX_DEPTH {
            self.error("maximum nesting depth exceeded".to_string());
            self.depth.set(d - 1);
            return None;
        }
        Some(RecursionGuard(Rc::clone(&self.depth)))
    }

    /// If the token is a contextual keyword, return its identifier name.
    pub(crate) fn contextual_keyword_name(tok: &Token<'_>) -> Option<&'static str> {
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
            Token::Optional => Some("optional"),
            Token::Deprecated => Some("deprecated"),
            Token::Reserved => Some("reserved"),
            Token::State => Some("state"),
            Token::Event => Some("event"),
            Token::On => Some("on"),
            Token::When => Some("when"),
            Token::Join => Some("join"),
            // Machine-block keywords that can also appear as external function names
            // or identifiers in other positions.
            Token::Entry => Some("entry"),
            Token::Exit => Some("exit"),
            Token::Emit => Some("emit"),
            _ => None,
        }
    }

    pub(crate) fn looks_like_scope_deadline(&self) -> bool {
        if !matches!(self.peek(), Some(Token::After)) {
            return false;
        }
        if !matches!(
            self.tokens.get(self.pos + 1).map(|(token, _)| token),
            Some(Token::LeftParen)
        ) {
            return false;
        }

        let mut paren_depth = 0usize;
        for idx in (self.pos + 1)..self.tokens.len() {
            match &self.tokens[idx].0 {
                Token::LeftParen => paren_depth += 1,
                Token::RightParen => {
                    paren_depth = paren_depth.saturating_sub(1);
                    if paren_depth == 0 {
                        return matches!(
                            self.tokens.get(idx + 1).map(|(token, _)| token),
                            Some(Token::LeftBrace)
                        );
                    }
                }
                _ => {}
            }
        }
        false
    }

    /// Returns true if the token can be used as an identifier (regular or contextual keyword).
    pub(crate) fn is_ident_token(tok: &Token<'_>) -> bool {
        matches!(tok, Token::Identifier(_)) || Self::contextual_keyword_name(tok).is_some()
    }

    /// Returns true when the current token is a bare `Identifier` matching `kw`.
    ///
    /// Used for machine-body contextual keywords (`events`, `emits`, `reenter`,
    /// `initial`) that are NOT lexer keywords — they tokenize as ordinary
    /// identifiers and only carry keyword meaning inside the machine body, so
    /// they cost nothing in the global identifier namespace.
    pub(crate) fn peek_machine_kw(&self, kw: &str) -> bool {
        matches!(self.peek(), Some(Token::Identifier(name)) if *name == kw)
    }

    /// Consumes the current token iff it is a bare `Identifier` matching `kw`.
    /// See `peek_machine_kw` for the contextual-keyword rationale.
    pub(crate) fn eat_machine_kw(&mut self, kw: &str) -> bool {
        if self.peek_machine_kw(kw) {
            self.advance();
            true
        } else {
            false
        }
    }

    pub(crate) fn expect_ident(&mut self) -> Option<String> {
        match self.peek() {
            Some(Token::Identifier(name)) => {
                let name = name.to_string();
                self.advance();
                Some(name)
            }
            Some(tok) => {
                if let Some(name) = Self::contextual_keyword_name(tok) {
                    self.advance();
                    Some(name.to_string())
                } else if let Some(kw) = tok.keyword_str() {
                    // Reserved keyword in a name position — emit a targeted
                    // diagnostic so the user knows the word is off-limits.
                    self.error_at_with_hint(
                        format!("`{kw}` is a reserved word and cannot be used as a name"),
                        self.peek_span(),
                        format!("rename this item to something other than `{kw}`"),
                    );
                    None
                } else {
                    self.error(format!("expected identifier, found {tok}"));
                    None
                }
            }
            None => {
                self.error("expected identifier, found end of file".to_string());
                None
            }
        }
    }

    pub(crate) fn is_import_path_segment_token(tok: &Token<'_>) -> bool {
        matches!(tok, Token::Identifier(_) | Token::Actor)
            || Self::contextual_keyword_name(tok).is_some()
    }

    pub(crate) fn expect_import_path_segment(&mut self) -> Option<String> {
        match self.peek() {
            Some(Token::Actor) => {
                self.advance();
                Some("actor".to_string())
            }
            Some(tok) if Self::contextual_keyword_name(tok).is_some() => {
                let name = Self::contextual_keyword_name(tok).expect("checked above");
                self.advance();
                Some(name.to_string())
            }
            _ => self.expect_ident(),
        }
    }

    /// Skip tokens until the next actor-body item boundary (`receive`, `fn`,
    /// `let`, `var`, or the actor's closing `}`) or end-of-file.  Nested
    /// brace groups (`{…}`) are consumed wholesale so we stop only at the
    /// actor-level boundary, not inside a method body.
    ///
    /// Used for error recovery after a bad item name inside an actor body so
    /// a single bad declaration does not cascade into dozens of errors.
    pub(crate) fn skip_to_actor_item_boundary(&mut self) {
        let mut depth: usize = 0;
        while !self.at_end() {
            match self.peek() {
                Some(Token::LeftBrace) => {
                    depth += 1;
                    self.advance();
                }
                Some(Token::RightBrace) => {
                    if depth == 0 {
                        // This is the actor-body closing brace — stop here.
                        break;
                    }
                    depth -= 1;
                    self.advance();
                }
                Some(Token::Receive | Token::Fn | Token::Let | Token::Var) if depth == 0 => {
                    break;
                }
                _ => {
                    self.advance();
                }
            }
        }
    }

    pub(crate) fn is_match_arm_pattern_start(&self) -> bool {
        match self.peek() {
            Some(Token::Minus) => matches!(
                self.peek_at(self.pos + 1),
                Some(Token::Integer(_) | Token::Float(_))
            ),
            Some(Token::Dot) => matches!(self.peek_at(self.pos + 1), Some(Token::Identifier(_))),
            Some(
                Token::Identifier(_)
                | Token::LeftParen
                | Token::Integer(_)
                | Token::StringLit(_)
                | Token::CharLit(_)
                | Token::RawString(_)
                | Token::RegexLiteral(_)
                | Token::True
                | Token::False,
            ) => true,
            Some(tok) => Self::contextual_keyword_name(tok).is_some(),
            None => false,
        }
    }

    /// Recover after a malformed match arm by consuming the bad arm, then
    /// stopping before the closing `}` or the next arm's pattern.
    pub(crate) fn skip_to_match_arm_boundary(&mut self) {
        let mut depth: usize = 0;
        let mut closed_top_level_block = false;
        while !self.at_end() {
            match self.peek() {
                Some(Token::RightBrace) if depth == 0 => break,
                Some(Token::Comma) if depth == 0 => {
                    self.advance();
                    break;
                }
                Some(Token::LeftBrace) => {
                    depth += 1;
                    closed_top_level_block = false;
                    self.advance();
                }
                Some(Token::RightBrace) => {
                    depth -= 1;
                    self.advance();
                    closed_top_level_block = depth == 0;
                }
                Some(_)
                    if depth == 0
                        && closed_top_level_block
                        && self.is_match_arm_pattern_start() =>
                {
                    break;
                }
                _ => {
                    closed_top_level_block = false;
                    self.advance();
                }
            }
        }
    }

    /// Returns true when the token can start a top-level item.
    pub(crate) fn is_top_level_item_start(tok: &Token<'_>) -> bool {
        matches!(
            tok,
            Token::Fn
                | Token::Pub
                | Token::Package
                | Token::Actor
                | Token::Type
                | Token::Trait
                | Token::Impl
                | Token::Import
                | Token::Record
                | Token::Enum
                | Token::Machine
                | Token::Supervisor
                | Token::Const
                | Token::Indirect
                | Token::Async
                | Token::Gen
                | Token::Extern
                | Token::HashBracket
                | Token::DocComment(_)
        )
    }

    /// Skip tokens until the next top-level item start or end-of-file.
    /// Always advances at least one token, then skips nested brace groups
    /// wholesale.  Used for error recovery in `parse_program` so a single
    /// bad declaration does not cascade into one error per remaining token.
    pub(crate) fn skip_to_top_level_item_boundary(&mut self) {
        // Consume at least the current (failed) token so we always make
        // forward progress even if it looks like an item-start keyword.
        if !self.at_end() {
            self.advance();
        }
        let mut depth: usize = 0;
        while !self.at_end() {
            match self.peek() {
                Some(Token::LeftBrace) => {
                    depth += 1;
                    self.advance();
                }
                Some(Token::RightBrace) => {
                    if depth == 0 {
                        self.advance(); // consume stray `}` and stop
                        break;
                    }
                    depth -= 1;
                    self.advance();
                }
                Some(tok) if depth == 0 && Self::is_top_level_item_start(tok) => {
                    break;
                }
                _ => {
                    self.advance();
                }
            }
        }
    }

    pub(crate) fn save_pos(&self) -> SavedPos {
        SavedPos {
            pos: self.pos,
            error_count: self.errors.len(),
            angle_mutation_count: self.angle_mutations.len(),
            last_token_end: self.last_token_end,
        }
    }

    #[expect(
        clippy::needless_pass_by_value,
        reason = "SavedPos is consumed to restore parser state"
    )]
    pub(crate) fn restore_pos(&mut self, saved: SavedPos) {
        self.pos = saved.pos;
        self.errors.truncate(saved.error_count);
        self.last_token_end = saved.last_token_end;
        // Undo any token mutations made by eat_closing_angle since this save point
        while self.angle_mutations.len() > saved.angle_mutation_count {
            let (idx, tok) = self.angle_mutations.pop().unwrap();
            self.tokens[idx] = tok;
        }
    }

    /// Collect consecutive doc comment tokens with the given prefix and return
    /// the concatenated content, or `None` if no matching comments are present.
    pub(crate) fn collect_doc_comments_with_prefix(
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
    pub(crate) fn collect_doc_comments(&mut self) -> Option<String> {
        self.collect_doc_comments_with_prefix("///", |t| match t {
            Token::DocComment(s) => Some(s),
            _ => None,
        })
    }

    /// Collect consecutive inner doc comment (`//!`) tokens at the start of
    /// the file and return the concatenated content.
    pub(crate) fn collect_inner_doc_comments(&mut self) -> Option<String> {
        self.collect_doc_comments_with_prefix("//!", |t| match t {
            Token::InnerDocComment(s) => Some(s),
            _ => None,
        })
    }
}
