//! Expression parsing (Pratt precedence), postfix chains, lambdas, and
//! struct-literal initialisation.

#[allow(
    clippy::wildcard_imports,
    reason = "grammar-area submodules share the parent parser namespace via the split"
)]
use super::*;

impl Parser<'_> {
    // ── Expressions (Pratt Precedence) ──
    pub(crate) fn parse_expr(&mut self) -> Option<Spanned<Expr>> {
        let _guard = self.enter_recursion()?;
        self.parse_expr_bp(0)
    }

    /// True while a bare identifier directly followed by `{` must be read as an
    /// identifier (block opener) rather than a struct literal — i.e. inside an
    /// `if`/`while` condition or `match` scrutinee at the top level.
    pub(crate) fn no_struct_literal(&self) -> bool {
        self.no_struct_literal.get()
    }

    /// Set `no_struct_literal` to `value` and return a guard that restores the
    /// previous value on drop. Survives every early `return`/`?` path.
    pub(crate) fn set_no_struct_literal(&self, value: bool) -> NoStructLiteralGuard {
        let prev = self.no_struct_literal.get();
        self.no_struct_literal.set(value);
        NoStructLiteralGuard {
            cell: Rc::clone(&self.no_struct_literal),
            prev,
        }
    }

    /// Parse an `if`/`while` condition or `match` scrutinee. In this position a
    /// bare identifier directly followed by `{` opens the block, never a struct
    /// literal, so the `no_struct_literal` restriction is set for the duration
    /// of the parse. The restriction is lifted again inside any bracketing
    /// delimiter (see the `(...)`, `[...]`, call-args, index, and struct-body
    /// parse sites), so `if (Foo { a: 1 }).b {…}` and other delimited struct
    /// literals in the condition still parse.
    pub(crate) fn parse_cond_expr(&mut self) -> Option<Spanned<Expr>> {
        let _guard = self.set_no_struct_literal(true);
        self.parse_expr()
    }

    /// Run `f` with the `no_struct_literal` restriction lifted. Used when the
    /// parser descends through an unambiguous delimiter (`(...)`, `[...]`, call
    /// args, index, struct body): the enclosing `{` is no longer the condition's
    /// block, so a struct literal there is no longer ambiguous.
    pub(crate) fn with_struct_literals_allowed<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> T {
        let _guard = self.set_no_struct_literal(false);
        f(self)
    }

    #[expect(
        clippy::too_many_lines,
        reason = "Pratt parser covers many expression forms"
    )]
    pub(crate) fn parse_expr_bp(&mut self, min_bp: u8) -> Option<Spanned<Expr>> {
        let start = self.peek_span().start;

        // `&expr` is not an expression in Hew. `&` is infix bitwise-and and the
        // type-level borrow marker (`&T`, ABI substrate); there is no
        // prefix-reference/borrow expression. Duplication is spelled `clone x`.
        // Catch a leading `&` in operand position with a targeted diagnostic so
        // the reader is pointed at `clone` instead of a generic parse error,
        // then recover by parsing the operand as if the `&` were absent (one
        // diagnostic, no cascade).
        if self.peek() == Some(&Token::Ampersand) {
            self.error_with_hint(
                "`&` is not a prefix operator; Hew has no reference or borrow \
                 expression"
                    .to_string(),
                "to duplicate a value, write `clone x`",
            );
            self.advance()?; // consume `&` and recover on the operand
            return self.parse_expr_bp(min_bp);
        }

        // Prefix operators
        let mut lhs = if self.peek_is_clone_prefix() {
            // Contextual `clone <operand>` duplication prefix. `clone` is not a
            // reserved word — it is also a method/free-fn name — so it only acts
            // as the prefix when it sits in operator position immediately
            // followed by an operand token (`peek_is_clone_prefix`). Binds at
            // unary precedence so `clone a + b` is `(clone a) + b` and
            // `clone x.field` / `clone foo()` clone the whole postfix chain.
            self.advance()?; // consume `clone`
            let operand = self.parse_expr_bp(CLONE_PREFIX_BP)?;
            let end = operand.1.end;
            (Expr::Clone(Box::new(operand)), start..end)
        } else if let Some(rbp) = self.peek().and_then(prefix_bp) {
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
                Token::AwaitRestart => {
                    let operand = self.parse_expr_bp(rbp)?;
                    let end = operand.1.end;
                    (Expr::AwaitRestart(Box::new(operand)), start..end)
                }
                Token::Star => {
                    // Raw pointer dereference (`*expr`).  v0.5 parses but
                    // the type checker rejects with either
                    // `UnsafeOperationRequiresBlock` (outside unsafe) or a
                    // "not lowered in v0.5" diagnostic (inside unsafe).
                    let operand = self.parse_expr_bp(rbp)?;
                    let end = operand.1.end;
                    (
                        Expr::Unary {
                            op: UnaryOp::RawDeref,
                            operand: Box::new(operand),
                        },
                        start..end,
                    )
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
                Some(Token::As) => {
                    self.advance();
                    let ty = self.parse_type()?;
                    let end = ty.1.end;
                    let expr_start = lhs.1.start;
                    lhs = (
                        Expr::Cast {
                            expr: Box::new(lhs),
                            ty,
                        },
                        expr_start..end,
                    );
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
                    // Binding power 13 (same as bitwise OR left bp)
                    if 13 >= min_bp {
                        self.advance(); // consume after
                        let duration = self.parse_expr_bp(14)?;
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

            // Detect removed `=~` and `!~` regex operators.  The lexer never
            // produced `EqTilde`/`BangTilde` tokens, so the character sequences
            // tokenise as adjacent `=`+`~` or `!`+`~`.  Neither `=` nor `!` has
            // infix binding power, so the loop would break and leave a confusing
            // "expected `;`" error — check here before the infix break.
            {
                let next = self.peek();
                let is_eq_tilde = next == Some(&Token::Equal)
                    && self.peek_at(self.pos + 1) == Some(&Token::Tilde)
                    && {
                        let eq_end = self.peek_span().end;
                        self.tokens
                            .get(self.pos + 1)
                            .is_some_and(|(_, s)| s.start == eq_end)
                    };
                let is_bang_tilde = next == Some(&Token::Bang)
                    && self.peek_at(self.pos + 1) == Some(&Token::Tilde)
                    && {
                        let bang_end = self.peek_span().end;
                        self.tokens
                            .get(self.pos + 1)
                            .is_some_and(|(_, s)| s.start == bang_end)
                    };
                if is_eq_tilde || is_bang_tilde {
                    let op_str = if is_eq_tilde { "=~" } else { "!~" };
                    let op_start = self.peek_span().start;
                    self.advance(); // consume `=` or `!`
                    let op_end = self.peek_span().end;
                    self.advance(); // consume `~`
                    let op_span = op_start..op_end;
                    self.error_at_with_hint(
                        format!(
                            "E_REGEX_OP_REMOVED: the `{op_str}` regex operator has been removed; \
                             use a match arm or `Pattern.is_match()` instead (HEW-SPEC-2026 §5)"
                        ),
                        op_span.clone(),
                        format!(
                            "replace `expr {op_str} pattern` with `match expr {{ re\"...\" => true, _ => false }}`"
                        ),
                    );
                    while !matches!(
                        self.peek(),
                        Some(&Token::Semicolon | &Token::RightBrace) | None
                    ) {
                        self.advance();
                    }
                    if self.peek() == Some(&Token::Semicolon) {
                        self.advance();
                    }
                    lhs = (Expr::Tuple(vec![]), op_span);
                    break;
                }
            }

            // Then try infix
            let Some((lbp, rbp)) = self.peek().and_then(infix_bp) else {
                break;
            };
            if lbp < min_bp {
                break;
            }

            // Detect the removed `<-` send operator: lexer now produces two tokens
            // `<` (at pos) and `-` (at pos+1) adjacently.  Emit E_OPERATOR_REMOVED,
            // then skip to the statement boundary (`;` or `}`) so that the caller
            // does not produce cascading "unexpected token" diagnostics for the
            // right-hand side tokens.
            if self.peek() == Some(&Token::Less) {
                let less_end = self.peek_span().end;
                if self.peek_at(self.pos + 1) == Some(&Token::Minus) {
                    let minus_start = self
                        .tokens
                        .get(self.pos + 1)
                        .map_or(usize::MAX, |(_, s)| s.start);
                    if less_end == minus_start {
                        let op_span = self.peek_span().start..minus_start + 1;
                        self.advance(); // consume `<`
                        self.advance(); // consume `-`
                        self.error_at_with_hint(
                            "E_OPERATOR_REMOVED: the `<-` send operator has been removed; \
                             use `handle(msg)` call syntax instead (HEW-SPEC-2026 §4.x)"
                                .to_string(),
                            op_span.clone(),
                            "replace `target <- msg` with `target(msg)`".to_string(),
                        );
                        // Skip tokens through the end of the statement to suppress
                        // cascading "unexpected token" diagnostics on the RHS.
                        while !matches!(
                            self.peek(),
                            Some(&Token::Semicolon | &Token::RightBrace) | None
                        ) {
                            self.advance();
                        }
                        // Consume the `;` now so that parse_block treats this as a
                        // fully consumed expression statement rather than seeing the
                        // semicolon as unexpected.
                        if self.peek() == Some(&Token::Semicolon) {
                            self.advance();
                        }
                        // Return a synthetic unit expression so the block parser
                        // completes the statement without entering the error-recovery path.
                        lhs = (Expr::Tuple(vec![]), op_span);
                        break;
                    }
                }
            }

            // `is` is a keyword token (not a symbol), handled as a special infix form.
            if self.peek() == Some(&Token::Is) {
                self.advance(); // consume `is`
                let rhs = self.parse_expr_bp(rbp)?;
                let end = rhs.1.end;
                let lhs_start = lhs.1.start;
                lhs = (
                    Expr::Is {
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    },
                    lhs_start..end,
                );
                continue;
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
    pub(crate) fn parse_primary(&mut self) -> Option<Spanned<Expr>> {
        let start = self.peek_span().start;

        let expr = match self.peek()? {
            Token::Duration(s) => {
                if let Some(nanos) = parse_duration_literal(s) {
                    self.advance();
                    Expr::Literal(Literal::Duration(nanos))
                } else {
                    self.error_invalid_literal_with_hint(
                        "invalid duration literal".to_string(),
                        "valid formats: 100ms, 5s, 2m, 1h, 500us, 10ns",
                    );
                    return None;
                }
            }
            Token::Integer(s) => {
                if let Ok((val, radix)) = parse_int_literal(s) {
                    self.advance();
                    Expr::Literal(Literal::Integer { value: val, radix })
                } else {
                    self.error_invalid_literal_with_hint(
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
                    self.error_invalid_literal(format!("invalid float literal '{s}'"));
                    return None;
                }
            }
            Token::StringLit(s) => {
                let inner = unquote_str(s);
                let tok_start = start;
                let (unescaped, unescape_errs) = unescape_string(inner);
                if unescaped.contains('\0') {
                    self.errors
                        .push(embedded_nul_string_error(start..self.peek_span().end));
                }
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
                Expr::Literal(Literal::String(unescaped))
            }
            Token::CharLit(s) => {
                let inner = s
                    .strip_prefix('\'')
                    .and_then(|s| s.strip_suffix('\''))
                    .unwrap_or(s);
                if let Some(c) = self.parse_char_escape(inner) {
                    self.advance();
                    Expr::Literal(Literal::Char(c))
                } else {
                    return None;
                }
            }
            Token::RawString(s) => {
                let s = unquote_str(s).to_string();
                if s.contains('\0') {
                    self.errors
                        .push(embedded_nul_string_error(start..self.peek_span().end));
                }
                self.advance();
                Expr::Literal(Literal::String(s))
            }
            Token::ByteStringLit(s) => {
                // Strip b"..." wrapper and unescape.
                let inner = s
                    .strip_prefix("b\"")
                    .and_then(|s| s.strip_suffix('"'))
                    .unwrap_or(s);
                let tok_start = start;
                let (unescaped, unescape_errs) = unescape_string(inner);
                for (off, msg) in unescape_errs {
                    let err_start = tok_start + 2 + off;
                    self.errors.push(ParseError {
                        message: msg.to_string(),
                        span: err_start..err_start + 2,
                        hint: None,
                        severity: Severity::Error,
                        kind: ParseDiagnosticKind::InvalidLiteral,
                    });
                }
                self.advance();
                Expr::ByteStringLiteral(unescaped.into_bytes())
            }
            Token::InterpolatedString(s) => {
                let s = s.to_string();
                self.advance();
                let parts = parse_string_parts(&s, 2, 1, "{", start, &mut self.errors);
                Expr::InterpolatedString(parts)
            }
            Token::RegexLiteral(s) => {
                // Normalise the token: strip delimiters and decode only the
                // delimiter escape `\"`. Regex backslashes are passed through
                // verbatim so `re"\s+"` reaches the engine as `\s+`.
                let pattern = normalize_regex_literal(s);
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
            Token::Label(label) => {
                let name = (*label).to_string();
                self.advance();
                Expr::Identifier(name)
            }
            Token::Identifier(name)
                if *name == "bytes" && self.peek_at(self.pos + 1) == Some(&Token::LeftBracket) =>
            {
                self.advance(); // consume "bytes"
                self.advance(); // consume "["

                let mut values: Vec<u8> = Vec::new();
                while self.peek() != Some(&Token::RightBracket) {
                    let elem_expr = self.parse_expr()?;
                    if let Expr::Literal(Literal::Integer { value, .. }) = &elem_expr.0 {
                        if *value < 0 || *value > 255 {
                            self.error_invalid_literal(format!(
                                "byte value {value} out of range (must be 0..255)"
                            ));
                            return None;
                        }
                        #[expect(
                            clippy::cast_possible_truncation,
                            clippy::cast_sign_loss,
                            reason = "Checked to be 0..=255 above"
                        )]
                        values.push(*value as u8);
                    } else {
                        self.error(
                            "byte array literal elements must be integer literals".to_string(),
                        );
                        return None;
                    }
                    if !self.eat(&Token::Comma) {
                        break;
                    }
                }
                self.expect(&Token::RightBracket)?;
                Expr::ByteArrayLiteral(values)
            }
            Token::Identifier(name) => {
                let mut name = name.to_string();
                self.advance();

                // Handle path expressions like Vec::new, HashMap::new
                // Optionally accept Rust-style turbofish on a path segment:
                // `Type::<T, U>::method` — the `<...>` are stashed and surface
                // as the call's `type_args` when the path is invoked with `(`.
                let mut turbofish: Option<Vec<Spanned<TypeExpr>>> = None;
                while self.eat(&Token::DoubleColon) {
                    if self.peek() == Some(&Token::Less) {
                        let saved = self.save_pos();
                        self.advance(); // consume '<'
                        if let Some(args) = self.parse_type_args() {
                            if turbofish.is_some() {
                                self.error(
                                    "turbofish `::<...>` may appear at most once in a path"
                                        .to_string(),
                                );
                            }
                            turbofish = Some(args);
                            // A turbofish must be followed by another `::ident`
                            // segment or the call site `(`. If we don't see
                            // `::` next we fall out of the loop and let the
                            // surrounding logic (`(args)`) consume the call.
                            continue;
                        }
                        self.restore_pos(saved);
                        break;
                    }
                    if let Some(segment) = self.expect_ident() {
                        name = format!("{name}::{segment}");
                    } else {
                        break;
                    }
                }

                // If turbofish was present, the path must be invoked as a call.
                // Build the `Expr::Call` here so the explicit type args reach the
                // checker via `Call.type_args` exactly as for the bare-call form
                // `Type::method<T>(args)`. The struct-init / generic-call paths
                // below are skipped because turbofish is unambiguously a call.
                if let Some(type_args) = turbofish {
                    if self.peek() != Some(&Token::LeftParen) {
                        self.error(
                            "turbofish `::<...>` must be followed by a function call `(...)`"
                                .to_string(),
                        );
                        return None;
                    }
                    self.advance(); // consume '('
                    let args = self.parse_call_args()?;
                    self.expect(&Token::RightParen)?;
                    let end = self.peek_span().start;
                    let func_span = start..end;
                    let func = (Expr::Identifier(name), func_span.clone());
                    return Some((
                        Expr::Call {
                            function: Box::new(func),
                            type_args: Some(type_args),
                            args,
                            is_tail_call: false,
                        },
                        start..end,
                    ));
                }

                // Check for struct initialization — including the explicit-type-arg form
                // `Name<T, ...> { field: expr, ... }`.  We need a speculative parse
                // because `<` is also a comparison operator: only commit when we see
                // a closing `>` **immediately** followed by `{`.
                let explicit_type_args: Option<Vec<Spanned<TypeExpr>>> =
                    if self.peek() == Some(&Token::Less) {
                        let saved_pos = self.save_pos();
                        self.advance(); // consume '<'
                        if let Some(type_args) = self.parse_type_args() {
                            // Accept only if `{` follows (this is a struct init, not a
                            // comparison expression like `if x < y { ... }`).
                            if self.peek() == Some(&Token::LeftBrace) {
                                Some(type_args)
                            } else {
                                self.restore_pos(saved_pos);
                                None
                            }
                        } else {
                            self.restore_pos(saved_pos);
                            None
                        }
                    } else {
                        None
                    };

                if explicit_type_args.is_some() || self.peek() == Some(&Token::LeftBrace) {
                    // In `if`/`while` condition or `match` scrutinee position a
                    // bare identifier followed by `{` opens the block, not a
                    // struct literal — so the probe is suppressed there. The
                    // restriction only applies to the bare-identifier form; the
                    // explicit-type-arg form (`Name<T> { ... }`) is already gated
                    // on `>{` and is unambiguous, so it is left intact.
                    let is_struct_init = explicit_type_args.is_some()
                        || (!self.no_struct_literal() && self.probe_struct_init_brace());

                    if is_struct_init {
                        self.advance(); // consume {
                                        // Inside the struct body the `{` is consumed, so any
                                        // nested bare-ident struct literal is unambiguous again.
                        let (fields, base) =
                            self.with_struct_literals_allowed(Self::parse_struct_init_body)?;
                        Expr::StructInit {
                            name,
                            fields,
                            type_args: explicit_type_args,
                            base,
                        }
                    } else {
                        Expr::Identifier(name)
                    }
                } else {
                    Expr::Identifier(name)
                }
            }
            Token::Less => {
                // Speculative parse to detect old generic lambda: <T>(x: T) => expr.
                // This form was removed in v0.5; type-parameterized closures are not
                // supported. Detect the form and emit a typed migration diagnostic.
                let saved_pos = self.save_pos();
                self.advance(); // consume '<'

                let is_old_generic_lambda = if let Some(_type_params) = self.parse_type_params() {
                    if self.peek() == Some(&Token::LeftParen) {
                        self.advance(); // consume '('
                        if self.try_parse_lambda_params().is_some() {
                            if self.expect(&Token::RightParen).is_some() {
                                // Check for optional return type then `=>`
                                if self.eat(&Token::Arrow) {
                                    self.parse_type().is_some()
                                        && self.peek() == Some(&Token::FatArrow)
                                } else {
                                    self.peek() == Some(&Token::FatArrow)
                                }
                            } else {
                                false
                            }
                        } else {
                            false
                        }
                    } else {
                        false
                    }
                } else {
                    false
                };

                self.restore_pos(saved_pos);

                if is_old_generic_lambda {
                    // Consume through the form for error recovery, then emit typed error.
                    self.advance(); // consume '<'
                    self.parse_type_params();
                    self.expect(&Token::LeftParen);
                    self.try_parse_lambda_params();
                    self.expect(&Token::RightParen);
                    self.parse_opt_return_type();
                    if self.eat(&Token::FatArrow) {
                        self.parse_expr();
                    }
                    self.error_closure_pipe_syntax(
                        "E_CLOSURE_PIPE_SYNTAX: `<T>(params) => body` has been removed; \
                         type-parameterized closures are not supported in v0.5"
                            .to_string(),
                        start..self.peek_span().start,
                        "use `|params| body` — omit type parameters on closure expressions",
                    );
                    return None;
                }
                self.error("unexpected '<' at start of expression".to_string());
                return None;
            }
            Token::LeftParen => {
                self.advance();
                // Inside `(...)` the enclosing `{` is no longer the condition's
                // block, so a struct literal here is unambiguous — lift the
                // restriction for the parenthesised sub-expression (e.g.
                // `if (Foo { a: 1 }).b {…}`). Restored on arm exit.
                let _allow_struct = self.set_no_struct_literal(false);

                // Detect and reject old `(params) => body` parenthesized lambda syntax.
                // This form was removed in v0.5; the current form is `|params| body`.
                // Keep the detection here so we can surface a typed migration diagnostic
                // rather than a cryptic parse error when `=>` is encountered later.
                let saved_pos = self.save_pos();
                let is_old_paren_lambda = if self.try_parse_lambda_params().is_some() {
                    if self.expect(&Token::RightParen).is_some() {
                        // Check for optional return type annotation then `=>`
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

                if is_old_paren_lambda {
                    // Consume through the entire form for error recovery continuity.
                    self.try_parse_lambda_params();
                    self.expect(&Token::RightParen);
                    self.parse_opt_return_type();
                    if self.eat(&Token::FatArrow) {
                        self.parse_expr();
                    }
                    self.error_closure_pipe_syntax(
                        "E_CLOSURE_PIPE_SYNTAX: `(params) => body` has been removed; \
                         use `|params| body` instead"
                            .to_string(),
                        start..self.peek_span().start,
                        "replace `(params) => body` with `|params| body`",
                    );
                    return None;
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
                        // Safe: len == 1 guarantees next() yields one element.
                        return Some(exprs.into_iter().next().unwrap());
                    }
                    Expr::Tuple(exprs)
                }
            }
            Token::LeftBracket => {
                self.advance();
                // Inside `[...]` the enclosing `{` is no longer the condition's
                // block, so struct literals in array elements are unambiguous.
                let _allow_struct = self.set_no_struct_literal(false);
                if self.eat(&Token::RightBracket) {
                    return Some((Expr::Array(Vec::new()), start..self.peek_span().start));
                }

                let first = self.parse_expr()?;
                if self.eat(&Token::Semicolon) {
                    let count = self.parse_expr()?;
                    self.expect(&Token::RightBracket)?;
                    return Some((
                        Expr::ArrayRepeat {
                            value: Box::new(first),
                            count: Box::new(count),
                        },
                        start..self.peek_span().start,
                    ));
                }

                let mut elements = vec![first];
                while self.eat(&Token::Comma) {
                    if self.peek() == Some(&Token::RightBracket) {
                        break;
                    }
                    elements.push(self.parse_expr()?);
                }

                self.expect(&Token::RightBracket)?;
                Expr::Array(elements)
            }
            Token::Pipe | Token::PipePipe => self.parse_pipe_lambda(false, start)?,
            Token::LeftBrace => {
                // Disambiguate: {"str": expr, ...} → MapLiteral, else → Block
                // Note: bare {} remains a Block — empty HashMap coercion is
                // handled in the type checker when expected type is HashMap.
                // Use direct lookahead (no save/restore) for the common block path.
                //
                // A `{` reaching this arm is unambiguously a block (or map)
                // expression — even in `if`/`while` condition or `match`
                // scrutinee position (`if { let x = Foo { a: 1 }; x.a } {…}`).
                // The `{` opens the block's body, so the enclosing condition's
                // block is no longer ambiguous: lift `no_struct_literal` so a
                // struct literal INSIDE this block parses. Restored on arm exit.
                let _allow_struct = self.set_no_struct_literal(false);
                if matches!(self.peek_at(self.pos + 1), Some(Token::StringLit(_)))
                    && self.peek_at(self.pos + 2) == Some(&Token::Colon)
                {
                    self.advance(); // consume '{'
                    self.parse_map_literal_entries()?
                } else {
                    Expr::Block(self.parse_block()?)
                }
            }
            Token::If => {
                self.advance();
                if self.eat(&Token::Let) {
                    let pattern = Box::new(self.parse_pattern()?);
                    self.expect(&Token::Equal)?;
                    let expr = Box::new(self.parse_expr()?);
                    let body = self.parse_block()?;
                    let else_body = if self.eat(&Token::Else) {
                        Some(self.parse_block()?)
                    } else {
                        None
                    };
                    Expr::IfLet {
                        pattern,
                        expr,
                        body,
                        else_body,
                    }
                } else {
                    let condition = Box::new(self.parse_cond_expr()?);
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
            }
            Token::Match => {
                self.advance();
                let scrutinee = Box::new(self.parse_cond_expr()?);
                self.expect(&Token::LeftBrace)?;

                let mut arms = Vec::new();
                while !self.at_end() && self.peek() != Some(&Token::RightBrace) {
                    let before = self.pos;
                    if let Some(arm) = self.parse_match_arm() {
                        arms.push(arm);
                    } else {
                        self.skip_to_match_arm_boundary();
                        if self.pos == before && !self.at_end() {
                            self.advance();
                        }
                    }
                }

                self.expect(&Token::RightBrace)?;
                Expr::Match { scrutinee, arms }
            }
            // actor [move] |params| [-> Ret] { body }
            // Lambda actor literal (replaces `spawn (...) => ...`).
            Token::Actor => {
                self.advance();

                let is_move = self.eat(&Token::Move);

                if self.peek() != Some(&Token::Pipe) {
                    self.error_with_hint(
                        "E_SPAWN_LAMBDA_SYNTAX_REMOVED: expected `|` to begin actor parameter list"
                            .to_string(),
                        "use `actor |params| { body }` to declare a lambda actor".to_string(),
                    );
                    return None;
                }
                self.advance(); // consume `|`

                let params = self.try_parse_lambda_params().unwrap_or_default();

                self.expect(&Token::Pipe)?;

                let return_type = self.parse_opt_return_type()?;

                // Lambda actor body must be a braced block; parse_block consumes the `{`.
                if self.peek() != Some(&Token::LeftBrace) {
                    self.error_with_hint(
                        "E_SPAWN_LAMBDA_SYNTAX_REMOVED: expected `{` to begin actor body"
                            .to_string(),
                        "use `actor |params| { body }` — the body must be a braced block"
                            .to_string(),
                    );
                    return None;
                }
                let body_block = self.parse_block()?;
                let body_end = self.peek_span().start;
                let body = Box::new((Expr::Block(body_block), start..body_end));

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
            Token::Spawn => {
                self.advance();

                // Check whether the user wrote the legacy `spawn (params) => body` form.
                // This form was removed in favour of `actor |params| { body }`.
                // Consume an optional `move` keyword only to detect legacy syntax; it is not
                // used in the regular `spawn ActorName(...)` form.
                let _is_move_legacy = self.eat(&Token::Move);

                if self.peek() == Some(&Token::LeftParen) {
                    let saved_pos = self.save_pos();
                    self.advance();
                    let is_legacy_lambda = self.try_parse_lambda_params().is_some() && {
                        self.expect(&Token::RightParen).is_some()
                            && (self.peek() == Some(&Token::FatArrow)
                                || self.peek() == Some(&Token::Arrow))
                    };
                    self.restore_pos(saved_pos);

                    if is_legacy_lambda {
                        // Consume through the entire legacy form so recovery can continue.
                        self.advance(); // (
                        self.try_parse_lambda_params();
                        self.expect(&Token::RightParen);
                        self.parse_opt_return_type();
                        if self.eat(&Token::FatArrow) {
                            self.parse_expr();
                        }
                        self.error_at_with_hint(
                            "E_SPAWN_LAMBDA_SYNTAX_REMOVED: `spawn (...) => ...` has been removed; \
                             use `actor |...| { ... }` instead (HEW-SPEC-2026 §4.x)"
                                .to_string(),
                            start..self.peek_span().start,
                            "replace `spawn (params) => body` with `actor |params| { body }`"
                                .to_string(),
                        );
                        return None;
                    }
                }

                // Regular spawn: spawn ActorName(...) or spawn module.ActorName(...)
                // or spawn ActorName<T>(...) with explicit turbofish type args.
                let name = self.expect_ident()?;
                let name_end = self.peek_span().start;
                let target = if self.eat(&Token::Dot) {
                    let actor_name = self.expect_ident()?;
                    let actor_end = self.peek_span().start;
                    Box::new((
                        Expr::FieldAccess {
                            object: Box::new((Expr::Identifier(name), start..name_end)),
                            field: actor_name,
                        },
                        start..actor_end,
                    ))
                } else {
                    Box::new((Expr::Identifier(name), start..name_end))
                };

                // Optional turbofish type-argument list `<T, U>` before `(`.
                // A bare `<` here is unambiguous: spawn does not admit
                // comparison in this position (the target is a name, not an
                // expression), so we eagerly parse the angle-bracket list.
                let type_args = if self.eat(&Token::Less) {
                    self.parse_type_args().unwrap_or_default()
                } else {
                    vec![]
                };

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

                Expr::Spawn {
                    target,
                    type_args,
                    args,
                }
            }
            Token::Move => {
                self.advance();
                if matches!(self.peek(), Some(Token::Pipe | Token::PipePipe)) {
                    self.parse_pipe_lambda(true, start)?
                } else if self.peek() == Some(&Token::LeftParen) {
                    // Old `move (params) => body` form — detect and diagnose.
                    // Consume through the form for recovery, then emit a typed error.
                    let saved_pos = self.save_pos();
                    self.advance(); // consume '('
                    let is_old_paren_lambda = self.try_parse_lambda_params().is_some()
                        && self.expect(&Token::RightParen).is_some()
                        && (self.peek() == Some(&Token::FatArrow)
                            || self.peek() == Some(&Token::Arrow));
                    self.restore_pos(saved_pos);

                    if is_old_paren_lambda {
                        self.advance(); // consume '('
                        self.try_parse_lambda_params();
                        self.expect(&Token::RightParen);
                        self.parse_opt_return_type();
                        if self.eat(&Token::FatArrow) {
                            self.parse_expr();
                        }
                        self.error_closure_pipe_syntax(
                            "E_CLOSURE_PIPE_SYNTAX: `move (params) => body` has been removed; \
                             use `move |params| expr` instead"
                                .to_string(),
                            start..self.peek_span().start,
                            "replace `move (params) => body` with `move |params| expr`",
                        );
                        return None;
                    }
                    self.error_closure_pipe_syntax(
                        "E_CLOSURE_PIPE_SYNTAX: expected `|` after `move` to begin a closure"
                            .to_string(),
                        self.peek_span(),
                        "write `move |params| expr`",
                    );
                    return None;
                } else {
                    self.error_closure_pipe_syntax(
                        "E_CLOSURE_PIPE_SYNTAX: expected `|` after `move` to begin a closure"
                            .to_string(),
                        self.peek_span(),
                        "write `move |params| expr`",
                    );
                    return None;
                }
            }
            // gen { yield ...; } — lazy generator block expression.
            // Must be followed immediately by a braced block; `gen fn` (item-level
            // generator functions) is parsed separately and does not reach here.
            // Bare `gen` without a block emits a typed diagnostic.
            Token::Gen if self.peek_at(self.pos + 1) == Some(&Token::LeftBrace) => {
                self.advance(); // consume `gen`
                let body = self.parse_block()?;
                Expr::GenBlock { body }
            }
            Token::Gen => {
                self.advance(); // consume `gen`
                let found = match self.peek() {
                    Some(tok) => format!("`{tok}`"),
                    None => "end of file".to_string(),
                };
                self.error_with_hint(
                    format!("E_GEN_BLOCK_SYNTAX: `gen` must be followed by a block; found {found}"),
                    "write `gen { yield expr; }` to create a generator block".to_string(),
                );
                return None;
            }
            Token::Return => {
                self.advance();
                // `return [expr]` in expression position. Unlike `Stmt::Return`
                // there is NO trailing `;` here; the operand ends where the
                // surrounding expression ends. Stop on any token that cannot
                // begin an expression (`;` / `}` / `)` / `]` / `,`) or `else`
                // (so a `let Pat = e else { ... }` clause and an
                // `if cond { return } else { ... }` branch are not swallowed),
                // yielding `return` with no value. Mirrors the `Yield` shape.
                let value = if matches!(
                    self.peek(),
                    Some(
                        Token::Semicolon
                            | Token::RightBrace
                            | Token::RightParen
                            | Token::RightBracket
                            | Token::Comma
                            | Token::Else
                    ) | None
                ) {
                    None
                } else {
                    Some(Box::new(self.parse_expr()?))
                };
                Expr::Return(value)
            }
            Token::Scope => {
                self.advance();
                // Reject obsolete surfaces: `scope.method()` and `scope |s| { ... }`.
                if self.eat(&Token::Dot) {
                    self.error(
                        "'scope.method()' syntax has been removed; use 'scope { ... }' with `fork name = expr;` bindings instead"
                            .to_string(),
                    );
                    return None;
                }
                if self.peek() == Some(&Token::Pipe) {
                    self.error(
                        "'scope |s| { s.launch / s.spawn / s.cancel }' has been removed; use 'scope { fork name = call(...); }' instead"
                            .to_string(),
                    );
                    return None;
                }
                self.scope_expr_depth += 1;
                let body = self.parse_block()?;
                self.scope_expr_depth -= 1;
                Expr::Scope { body }
            }
            Token::Fork => {
                let fork_span = self.peek_span();
                self.advance();
                // `fork` is now exclusively the child-start verb inside a scope block:
                // `fork name = call(...);` or bare `fork call(...);`.
                if self.peek() == Some(&Token::LeftBrace) {
                    if self.scope_expr_depth == 0 {
                        self.error_at(
                            "`fork { ... }` child-task blocks are only valid inside `scope { ... }`"
                                .to_string(),
                            fork_span,
                        );
                        return None;
                    }
                    if self.fork_block_depth > 0 {
                        self.error_at(
                            "nested `fork { ... }` blocks are not a CT-2 surface; use an inner `scope { ... }`"
                                .to_string(),
                            fork_span,
                        );
                        return None;
                    }
                    self.fork_block_depth += 1;
                    let body = self.parse_block()?;
                    self.fork_block_depth -= 1;
                    Expr::ForkBlock { body }
                } else {
                    let binding = if self.fork_starts_child_binding() {
                        let name = self.expect_ident()?;
                        self.expect(&Token::Equal)?;
                        Some(name)
                    } else {
                        None
                    };
                    let expr = self.parse_expr()?;
                    Expr::ForkChild {
                        binding,
                        expr: Box::new(expr),
                    }
                }
            }
            Token::After if self.looks_like_scope_deadline() => {
                let after_span = self.peek_span();
                if self.scope_expr_depth == 0 {
                    self.error_at(
                        "`after(duration) { ... }` deadline clauses are only valid inside `scope { ... }`"
                            .to_string(),
                        after_span,
                    );
                    return None;
                }
                self.advance();
                self.expect(&Token::LeftParen)?;
                let duration = self.parse_expr()?;
                self.expect(&Token::RightParen)?;
                let body = self.parse_block()?;
                Expr::ScopeDeadline {
                    duration: Box::new(duration),
                    body,
                }
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
                if self.peek() != Some(&Token::LeftBrace) {
                    self.error(
                        "expected `{` after `unsafe`; `unsafe` must be followed by a block"
                            .to_string(),
                    );
                    return None;
                }
                Expr::UnsafeBlock(Box::new(self.parse_block()?))
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
                self.error(
                    "'cooperate' is compiler-internal; explicit cooperate expressions are not supported"
                        .to_string(),
                );
                return None;
            }
            Token::This => {
                self.advance();
                Expr::This
            }
            Token::Emit => {
                self.advance();
                let event_name = self.expect_ident()?;
                let fields = if self.eat(&Token::LeftBrace) {
                    let mut fields = Vec::new();
                    while !self.at_end() && self.peek() != Some(&Token::RightBrace) {
                        let field_name = self.expect_ident()?;
                        self.expect(&Token::Colon)?;
                        let field_val = self.parse_expr()?;
                        fields.push((field_name, field_val));
                        if !self.eat(&Token::Comma) {
                            break;
                        }
                    }
                    self.expect(&Token::RightBrace)?;
                    fields
                } else {
                    Vec::new()
                };
                Expr::MachineEmit { event_name, fields }
            }
            // Contextual keywords that can be used as identifiers in expressions
            tok if Self::contextual_keyword_name(tok).is_some() => {
                let name = Self::contextual_keyword_name(tok).unwrap();
                self.advance();
                Expr::Identifier(name.to_string())
            }
            _ => {
                let found = match self.peek() {
                    Some(tok) => format!("{tok}"),
                    None => "end of file".to_string(),
                };
                self.error_missing_expression(found);
                return None;
            }
        };

        let end = self.peek_span().start;
        Some((expr, start..end))
    }

    pub(crate) fn parse_pipe_lambda(&mut self, is_move: bool, start: usize) -> Option<Expr> {
        let params = if self.eat(&Token::PipePipe) {
            Vec::new()
        } else {
            self.expect(&Token::Pipe)?;
            let params = self.try_parse_pipe_lambda_params().or_else(|| {
                self.error_closure_pipe_syntax(
                    "E_CLOSURE_PIPE_SYNTAX: malformed closure parameter list".to_string(),
                    start..self.peek_span().start,
                    "write parameters as `|name|` or `|name: Type|`",
                );
                None
            })?;
            self.expect(&Token::Pipe).or_else(|| {
                self.error_closure_pipe_syntax(
                    "E_CLOSURE_PIPE_SYNTAX: expected `|` to close closure parameters".to_string(),
                    start..self.peek_span().start,
                    "write `|params| expr`",
                );
                None
            })?;
            params
        };

        let return_type = self.parse_opt_return_type()?;
        let body = if return_type.is_some() {
            if self.peek() != Some(&Token::LeftBrace) {
                self.error_closure_pipe_syntax(
                    "E_CLOSURE_PIPE_SYNTAX: typed pipe closures require a braced body".to_string(),
                    self.peek_span(),
                    "write `|params| -> Type { expr }`",
                );
                return None;
            }
            let body_start = self.peek_span().start;
            let body_block = self.parse_block()?;
            let body_end = self.peek_span().start;
            Box::new((Expr::Block(body_block), body_start..body_end))
        } else {
            if matches!(
                self.peek(),
                Some(Token::Semicolon | Token::RightBrace) | None
            ) {
                self.error_closure_pipe_syntax(
                    "E_CLOSURE_PIPE_SYNTAX: closure body is required".to_string(),
                    self.peek_span(),
                    "write `|| expr` or `|| { ... }`",
                );
                return None;
            }
            Box::new(self.parse_expr()?)
        };

        Some(Expr::Lambda {
            is_move,
            type_params: None,
            params,
            return_type,
            body,
        })
    }

    /// Parse map literal entries after the opening `{` has already been consumed.
    /// Expects at least one `key: value` pair, followed by optional comma-separated pairs.
    pub(crate) fn parse_map_literal_entries(&mut self) -> Option<Expr> {
        let mut entries = Vec::new();
        loop {
            let key = self.parse_expr()?;
            self.expect(&Token::Colon)?;
            let value = self.parse_expr()?;
            entries.push((key, value));

            if !self.eat(&Token::Comma) {
                break;
            }
            if self.peek() == Some(&Token::RightBrace) {
                break; // trailing comma
            }
        }
        self.expect(&Token::RightBrace)?;
        Some(Expr::MapLiteral { entries })
    }

    pub(crate) fn try_parse_pipe_lambda_params(&mut self) -> Option<Vec<LambdaParam>> {
        let mut params = Vec::new();

        while !self.at_end() && self.peek() != Some(&Token::Pipe) {
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

    pub(crate) fn try_parse_lambda_params(&mut self) -> Option<Vec<LambdaParam>> {
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

    #[allow(
        clippy::too_many_lines,
        reason = "single dispatch over all postfix-dot syntactic forms; splitting fragments shared start/cursor state"
    )]
    pub(crate) fn parse_dot_postfix(&mut self, lhs: Spanned<Expr>) -> Option<Spanned<Expr>> {
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

        // Accumulate optional `::Segment` pairs after the first identifier.
        // This handles cross-module enum variant construction: `fs.IoError::TimedOut(0)`.
        // Mirrors the DoubleColon accumulation loop in parse_primary's Identifier branch.
        let mut method = field;
        while self.eat(&Token::DoubleColon) {
            if let Some(segment) = self.expect_ident() {
                method = format!("{method}::{segment}");
            } else {
                break;
            }
        }

        if method.contains("::") {
            if let Some(mut name) = Self::dotted_expr_name(&lhs.0) {
                name.push('.');
                name.push_str(&method);
                // A `{` here begins a struct literal only when struct literals
                // are allowed in this position and the brace actually opens a
                // field list. In `if`/`while` condition or `match` scrutinee
                // position the `no_struct_literal` restriction is active, so
                // `if m.E::V { } else { }` opens the then-block — the `{` must
                // NOT be consumed as an empty struct literal here (which would
                // orphan the `else`). The probe also guards `{ stmt; }` blocks.
                if self.peek() == Some(&Token::LeftBrace)
                    && !self.no_struct_literal()
                    && self.probe_struct_init_brace()
                {
                    self.advance(); // consume {
                                    // Inside the struct body the `{` is consumed, so any nested
                                    // bare-ident struct literal is unambiguous again.
                    let (fields, base) =
                        self.with_struct_literals_allowed(Self::parse_struct_init_fields)?;
                    let end = self.peek_span().start;
                    return Some((
                        Expr::StructInit {
                            name,
                            fields,
                            type_args: None,
                            base,
                        },
                        start..end,
                    ));
                }
                let end = self.peek_span().start;
                return Some((Expr::Identifier(name), start..end));
            }
        }

        // Check for method call
        if self.peek() == Some(&Token::LeftParen) {
            self.advance();
            let args = self.parse_call_args()?;

            self.expect(&Token::RightParen)?;
            let end = self.peek_span().start;

            Some((
                Expr::MethodCall {
                    receiver: Box::new(lhs),
                    method,
                    args,
                },
                start..end,
            ))
        } else if self.peek() == Some(&Token::LeftBrace) && method.contains("::") {
            // Module-qualified struct literal: `module.Type::Variant { fields }`.
            //
            // Gate: `::` must have been consumed above — `method` carries the
            // full `Type::Variant` path segment.  A plain field access like
            // `obj.field { ... }` does NOT enter this arm (no `::` in field),
            // preserving the existing parse error / block interpretation for that
            // shape.
            //
            // The receiver (`lhs`) must be a bare `Ident` (single-level module
            // alias).  Nested-module paths (`a.b.Type::Variant`) fall through to
            // FieldAccess; that limit is deferred to v0.5.1.
            //
            // Disambiguate via the shared probe: `{ field: val }` → struct init;
            // `{ stmt; }` or `{ expr }` → not a struct literal, fall through.
            //
            // In `if`/`while` condition or `match` scrutinee position the
            // `no_struct_literal` restriction is active, so `if m.E::V { }` opens
            // the block rather than starting a struct literal — consistent with
            // the bare-identifier form above.
            if !self.no_struct_literal() && self.probe_struct_init_brace() {
                // Build the qualified type name: `module.Type::Variant`.
                // `lhs` is the module identifier; `method` is `Type::Variant`.
                // Non-identifier receivers (e.g. chained `a.b.C::D { }`) fall
                // through to FieldAccess — nested-module paths are out of scope
                // for v0.5; the checker surfaces an error via the field-access
                // path rather than a misleading struct-literal attempt.
                let module_name = if let Expr::Identifier(n) = &lhs.0 {
                    n.clone()
                } else {
                    let end = self.peek_span().start;
                    return Some((
                        Expr::FieldAccess {
                            object: Box::new(lhs),
                            field: method,
                        },
                        start..end,
                    ));
                };
                let qualified_name = format!("{module_name}.{method}");
                self.advance(); // consume {
                let (fields, base) =
                    self.with_struct_literals_allowed(Self::parse_struct_init_body)?;
                let end = self.peek_span().start;
                Some((
                    Expr::StructInit {
                        name: qualified_name,
                        fields,
                        type_args: None,
                        base,
                    },
                    start..end,
                ))
            } else {
                // Brace begins a block, not a struct literal — fall through to
                // FieldAccess.  The brace will be parsed as the next statement.
                let end = self.peek_span().start;
                Some((
                    Expr::FieldAccess {
                        object: Box::new(lhs),
                        field: method,
                    },
                    start..end,
                ))
            }
        } else {
            // Field access (method == field when no :: was consumed; otherwise a
            // unit-variant or bare type-path reference — preserved as FieldAccess).
            let end = self.peek_span().start;
            Some((
                Expr::FieldAccess {
                    object: Box::new(lhs),
                    field: method,
                },
                start..end,
            ))
        }
    }

    pub(crate) fn parse_struct_init_fields(&mut self) -> Option<StructInitFields> {
        let mut fields = Vec::new();
        let mut base: Option<Box<Spanned<Expr>>> = None;
        while !self.at_end() && self.peek() != Some(&Token::RightBrace) {
            if self.peek() == Some(&Token::DotDot) {
                self.advance(); // consume `..`
                let base_expr = self.parse_expr()?;
                base = Some(Box::new(base_expr));
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

    pub(crate) fn dotted_expr_name(expr: &Expr) -> Option<String> {
        match expr {
            Expr::Identifier(name) => Some(name.clone()),
            Expr::FieldAccess { object, field } => {
                let mut name = Self::dotted_expr_name(&object.0)?;
                name.push('.');
                name.push_str(field);
                Some(name)
            }
            _ => None,
        }
    }

    /// Parse a comma-separated list of call arguments, supporting both
    /// positional (`expr`) and named (`name: expr`) forms.
    ///
    /// Named args must come after all positional args.
    pub(crate) fn parse_call_args(&mut self) -> Option<Vec<CallArg>> {
        // Call arguments sit inside `(...)`, so a struct literal here is
        // unambiguous even in condition position (`if f(Foo { a: 1 }) {…}`).
        let _allow_struct = self.set_no_struct_literal(false);
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
                if self.parse_expr().is_none() {
                    break;
                }
                if !self.eat(&Token::Comma) {
                    break;
                }
                continue;
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

    pub(crate) fn parse_call_postfix(&mut self, lhs: Spanned<Expr>) -> Option<Spanned<Expr>> {
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

    pub(crate) fn parse_index_postfix(&mut self, lhs: Spanned<Expr>) -> Option<Spanned<Expr>> {
        let start = lhs.1.start;
        self.advance(); // consume [
                        // Index contents sit inside `[...]`, so a struct literal here is
                        // unambiguous even in condition position (`if xs[Foo { a: 1 }.k] {…}`).
        let _allow_struct = self.set_no_struct_literal(false);

        // C-3 range-slice support: detect the five range forms in index
        // position and emit `Expr::Range` (with the inclusive flag) so the
        // checker/HIR can route to slice lowering. Forms:
        //   xs[..]      — both endpoints open
        //   xs[..b]     — open start, closed end (also `..=b`)
        //   xs[a..]     — closed start, open end
        //   xs[a..b]    — both endpoints closed
        //   xs[a..=b]   — closed start, inclusive end
        // All other contents (e.g. `xs[i]`, `xs[f()]`) remain `Expr::Index`.

        let bracket_start_span = self.peek_span();

        // Form 1: leading `..` or `..=` — open start.
        if matches!(self.peek(), Some(Token::DotDot | Token::DotDotEqual)) {
            let inclusive = matches!(self.peek(), Some(Token::DotDotEqual));
            let dotdot_span = self.peek_span();
            self.advance(); // consume `..` or `..=`
                            // After the `..`, either `]` (xs[..] / xs[..=] — the latter is
                            // ill-formed but we accept it as `..` with a closed inclusive flag
                            // and let the checker complain about the missing endpoint via
                            // type inference) or an expression for the closed end.
            let end_expr = if self.peek() == Some(&Token::RightBracket) {
                None
            } else {
                Some(Box::new(self.parse_expr()?))
            };
            self.expect(&Token::RightBracket)?;
            let end_pos = self.peek_span().start;
            let range_span = dotdot_span.start..end_pos;
            let range_expr = (
                Expr::Range {
                    start: None,
                    end: end_expr,
                    inclusive,
                },
                range_span,
            );
            return Some((
                Expr::Index {
                    object: Box::new(lhs),
                    index: Box::new(range_expr),
                },
                start..end_pos,
            ));
        }

        // Parse the start sub-expression with `min_bp = 5` — above the
        // range precedence (3, 4) — so the Pratt loop does NOT fold a
        // trailing `..` / `..=` into a binary range. We then inspect the
        // next token to discriminate `xs[a]` (single-element index) from
        // `xs[a..]` (open-end slice) and `xs[a..b]` / `xs[a..=b]` (closed
        // slice). This lets `xs[a..]` succeed even though the normal
        // Pratt loop would demand a RHS after `..` and bail.
        let first = self.parse_expr_bp(5)?;

        let (inclusive, has_range_op) = match self.peek() {
            Some(Token::DotDot) => (false, true),
            Some(Token::DotDotEqual) => (true, true),
            _ => (false, false),
        };

        if has_range_op {
            // `xs[a..]` / `xs[a..b]` / `xs[a..=b]`.
            let range_start_pos = first.1.start;
            self.advance(); // consume `..` or `..=`
            let end_expr = if self.peek() == Some(&Token::RightBracket) {
                None
            } else {
                // Parse the upper bound at full `parse_expr` precedence
                // so nested expressions (`xs[a..b+1]`) work.
                Some(Box::new(self.parse_expr()?))
            };
            self.expect(&Token::RightBracket)?;
            let end_pos = self.peek_span().start;
            let range_span = range_start_pos..end_pos;
            let range_expr = (
                Expr::Range {
                    start: Some(Box::new(first)),
                    end: end_expr,
                    inclusive,
                },
                range_span,
            );
            return Some((
                Expr::Index {
                    object: Box::new(lhs),
                    index: Box::new(range_expr),
                },
                start..end_pos,
            ));
        }

        // No range operator after the start expression: this is a single-
        // element index (`xs[i]`, `xs[a + b]`, etc.). `parse_expr_bp(5)`
        // already absorbed every infix operator above range precedence,
        // which is the right closure for single-element indexing — range
        // operators in index position go through the branch above.
        let index = first;
        // Silence unused-binding warning: `bracket_start_span` is consulted
        // only by the leading-`..` branch above.
        let _ = bracket_start_span;

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
}
