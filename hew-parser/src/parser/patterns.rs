//! Pattern parsing, match/select arms, and struct-literal disambiguation.

#[allow(
    clippy::wildcard_imports,
    reason = "grammar-area submodules share the parent parser namespace via the split"
)]
use super::*;

impl Parser<'_> {
    // ── Patterns ──
    pub(crate) fn parse_pattern(&mut self) -> Option<Spanned<Pattern>> {
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
    pub(crate) fn parse_base_pattern(&mut self) -> Option<Spanned<Pattern>> {
        let _guard = self.enter_recursion()?;
        let start = self.peek_span().start;

        let pattern = match self.peek() {
            Some(Token::Minus)
                if matches!(
                    self.peek_at(self.pos + 1),
                    Some(Token::Integer(_) | Token::Float(_))
                ) =>
            {
                self.advance(); // consume '-'
                let (next, _) = self.advance()?;
                match next {
                    Token::Integer(s) => {
                        if let Ok((value, radix)) = parse_negated_int_literal(s) {
                            Pattern::Literal(Literal::Integer { value, radix })
                        } else {
                            self.error_invalid_literal_with_hint(
                                format!("invalid integer literal '-{s}'"),
                                "integer literals support decimal, 0x hex, 0o octal, and 0b binary",
                            );
                            return None;
                        }
                    }
                    Token::Float(s) => {
                        let cleaned: String = s.chars().filter(|c| *c != '_').collect();
                        if let Ok(val) = cleaned.parse::<f64>() {
                            Pattern::Literal(Literal::Float(-val))
                        } else {
                            self.error_invalid_literal(format!("invalid float literal '-{s}'"));
                            return None;
                        }
                    }
                    _ => unreachable!(),
                }
            }
            // Leading-dot variant pattern: `.Variant`, `.Variant(p, ..)`, or
            // `.Variant { f: p }`. The enum type is left implicit — the
            // type-checker resolves the bare short name against the match
            // scrutinee's type (`resolve_variant_match`), exactly as it does for
            // a leading-dot constructor expression. Fires only when the dot is
            // immediately followed by an identifier, so `..` range patterns
            // (a distinct `Token::DotDot`) are untouched. Emits the SAME
            // `Pattern::Constructor` / `Pattern::Struct` / `Pattern::Identifier`
            // a bare (unqualified) name would, so downstream resolution and HIR
            // lowering need no new pattern variant.
            Some(Token::Dot)
                if matches!(self.peek_at(self.pos + 1), Some(Token::Identifier(_))) =>
            {
                self.advance(); // consume '.'
                let name = self.expect_ident()?;
                if self.eat(&Token::LeftParen) {
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
            Some(Token::Identifier(name)) => {
                let mut name = name.to_string();
                self.advance();
                // Wildcard pattern
                if name == "_" {
                    Pattern::Wildcard
                } else {
                    // Handle qualified names like Colour::Red
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
                        // Safe: len == 1 guarantees next() yields one element.
                        return Some(patterns.into_iter().next().unwrap());
                    }
                    Pattern::Tuple(patterns)
                }
            }
            // Shorthand record destructure: `{ a, b }` with no type name.
            // The checker infers the record type from the scrutinee type and
            // delegates to the same field-binding path as `Pattern::Struct`.
            Some(Token::LeftBrace) => {
                self.advance(); // consume '{'
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
                Pattern::RecordShorthand { fields }
            }
            Some(Token::Integer(s)) => {
                if let Ok((val, radix)) = parse_int_literal(s) {
                    self.advance();
                    Pattern::Literal(Literal::Integer { value: val, radix })
                } else {
                    self.error_invalid_literal_with_hint(
                        format!("invalid integer literal '{s}'"),
                        "integer literals support decimal, 0x hex, 0o octal, and 0b binary",
                    );
                    return None;
                }
            }
            Some(Token::Float(s)) => {
                let cleaned: String = s.chars().filter(|c| *c != '_').collect();
                if let Ok(val) = cleaned.parse::<f64>() {
                    self.advance();
                    Pattern::Literal(Literal::Float(val))
                } else {
                    self.error_invalid_literal(format!("invalid float literal '{s}'"));
                    return None;
                }
            }
            Some(Token::StringLit(s)) => {
                let inner = unquote_str(s);
                let tok_start = self.peek_span().start;
                let (unescaped, unescape_errs) = unescape_string(inner);
                for (off, msg) in unescape_errs {
                    let err_start = tok_start + 1 + off;
                    self.errors.push(ParseError {
                        message: msg.to_string(),
                        span: err_start..err_start + 2,
                        hint: None,
                        severity: Severity::Error,
                        kind: ParseDiagnosticKind::InvalidLiteral,
                    });
                }
                self.advance();
                Pattern::Literal(Literal::String(unescaped))
            }
            Some(Token::CharLit(s)) => {
                let inner = s
                    .strip_prefix('\'')
                    .and_then(|s| s.strip_suffix('\''))
                    .unwrap_or(s);
                if let Some(c) = self.parse_char_escape(inner) {
                    self.advance();
                    Pattern::Literal(Literal::Char(c))
                } else {
                    return None;
                }
            }
            Some(Token::RawString(s)) => {
                let s = unquote_str(s).to_string();
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
            // Regex literal pattern: re"pattern" in a match arm.
            // `captures` is empty here; the checker populates it from the
            // regex engine's named-capture list after validating the pattern.
            Some(Token::RegexLiteral(s)) => {
                let s = *s;
                let pattern = normalize_regex_literal(s);
                self.advance();
                Pattern::Regex {
                    pattern,
                    captures: vec![],
                }
            }
            // Contextual keywords used as identifiers in patterns
            Some(tok) if Self::contextual_keyword_name(tok).is_some() => {
                let name = Self::contextual_keyword_name(self.peek().unwrap()).unwrap();
                self.advance();
                Pattern::Identifier(name.to_string())
            }
            _ => {
                let found = match self.peek() {
                    Some(tok) => format!("{tok}"),
                    None => "end of file".to_string(),
                };
                self.error_invalid_pattern(found);
                return None;
            }
        };

        let end = self.peek_span().start;
        Some((pattern, start..end))
    }

    pub(crate) fn parse_match_arm(&mut self) -> Option<MatchArm> {
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
        } else if Self::is_block_expr(&body.0) {
            self.eat(&Token::Comma);
        } else {
            self.expect(&Token::Comma)?;
        }

        Some(MatchArm {
            pattern,
            guard,
            body,
        })
    }

    pub(crate) fn parse_select_arm(&mut self) -> Option<SelectArm> {
        let binding = self.parse_pattern()?;
        self.expect(&Token::From)?;
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

    // ── Struct-literal disambiguation helpers ──────────────────────────────

    /// Speculatively probe whether the `{` at the current position begins a
    /// struct literal rather than a block statement.
    ///
    /// Contract: caller must have peeked `Token::LeftBrace` but NOT yet
    /// consumed it. This method saves, consumes `{`, inspects one token of
    /// lookahead, then restores position — so the caller's position is
    /// unchanged on return.
    ///
    /// Returns `true` when:
    ///   - the brace is immediately followed by `}` (empty struct literal)
    ///   - the brace is followed by `..` (functional-update-only form)
    ///   - the brace is followed by `ident :` (named field)
    ///
    /// Returns `false` otherwise (block beginning with a statement, expression,
    /// keyword, etc.).
    pub(crate) fn probe_struct_init_brace(&mut self) -> bool {
        let saved_pos = self.save_pos();
        self.advance(); // consume {
        let probe = if self.peek() == Some(&Token::RightBrace) {
            // Empty struct literal: Foo {}
            true
        } else if self.peek() == Some(&Token::DotDot) {
            // Functional-update-only form: `Foo { ..base }`.
            true
        } else if self.peek().is_some_and(|tok| Self::is_ident_token(tok)) {
            self.advance();
            self.peek() == Some(&Token::Colon)
        } else {
            false
        };
        self.restore_pos(saved_pos);
        probe
    }

    /// Parse the body of a struct literal after the opening `{` has been
    /// consumed.  Handles named fields, an optional trailing comma, and the
    /// functional-update `..base` tail.
    ///
    /// Returns `(fields, base)` where `fields` is a vec of `(name, expr)`
    /// pairs and `base` is `Some(expr)` when a `..base` suffix was present.
    ///
    /// Returns `None` if parsing fails (error is recorded on `self`).
    #[allow(
        clippy::type_complexity,
        reason = "return tuple encodes (fields, base) for struct literal body; extracting a named type would require a public struct in a private-helper context"
    )]
    pub(crate) fn parse_struct_init_body(
        &mut self,
    ) -> Option<(Vec<(String, Spanned<Expr>)>, Option<Box<Spanned<Expr>>>)> {
        let mut fields = Vec::new();
        let mut base: Option<Box<Spanned<Expr>>> = None;

        while !self.at_end() && self.peek() != Some(&Token::RightBrace) {
            if self.peek() == Some(&Token::DotDot) {
                // `..base_expr` — must be the last item in the list.
                self.advance(); // consume `..`
                let base_expr = self.parse_expr()?;
                base = Some(Box::new(base_expr));
                // Allow an optional trailing comma before `}`.
                self.eat(&Token::Comma);
                if self.peek() != Some(&Token::RightBrace) {
                    self.error(
                        "functional-update `..base` must be the last item in the field list"
                            .to_string(),
                    );
                }
                break;
            }
            let field_name = self.expect_ident()?;
            self.expect(&Token::Colon)?;
            let value = self.parse_expr()?;
            fields.push((field_name, value));

            if !self.eat(&Token::Comma) {
                break;
            }
        }
        self.expect(&Token::RightBrace)?;

        Some((fields, base))
    }
}
