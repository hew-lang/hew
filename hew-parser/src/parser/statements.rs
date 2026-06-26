//! Block and statement parsing.

#[allow(
    clippy::wildcard_imports,
    reason = "grammar-area submodules share the parent parser namespace via the split"
)]
use super::*;

impl Parser<'_> {
    // ── Statements ──
    pub(crate) fn parse_block(&mut self) -> Option<Block> {
        self.expect(&Token::LeftBrace)?;

        let mut stmts = Vec::new();
        let mut trailing_expr = None;

        while !self.at_end() && self.peek() != Some(&Token::RightBrace) {
            // Try to parse as statement first.
            if let Some(stmt) = self.parse_stmt() {
                // If this is the last item in the block (`}` follows immediately, no
                // semicolon consumed), and the statement is a value-bearing form (`if`,
                // `if let`, `match`), promote it to the block's trailing expression.
                // These forms produce a value — the HIR/MIR pipeline reads
                // `block.trailing_expr` as the return value of the block; a
                // `Stmt::If`/`Stmt::Match` leaves the return slot uninitialised.
                let at_tail = self.peek() == Some(&Token::RightBrace);
                let is_value_bearing = matches!(
                    stmt.0,
                    Stmt::If { .. } | Stmt::IfLet { .. } | Stmt::Match { .. }
                );
                if at_tail && is_value_bearing {
                    // SAFETY: `is_value_bearing` ensures promote_stmt_to_trailing_expr
                    // will always return Some for these exact variants.
                    let expr = Self::promote_stmt_to_trailing_expr(stmt)
                        .expect("promote_stmt_to_trailing_expr must succeed for If/IfLet/Match");
                    trailing_expr = Some(Box::new(expr));
                    break;
                }
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
                } else if Self::is_block_expr(&expr.0)
                    && (self.peek() != Some(&Token::RightBrace)
                        || matches!(expr.0, Expr::ForkBlock { .. } | Expr::ScopeDeadline { .. }))
                {
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

    /// Convert a value-bearing statement (`Stmt::If`, `Stmt::IfLet`,
    /// `Stmt::Match`) that appears in tail position into the equivalent
    /// expression form, so the block's `trailing_expr` slot can be populated.
    ///
    /// Returns `None` for statement forms that are not value-bearing (i.e.
    /// statements that never reach this function in practice because the caller
    /// guards on `is_value_bearing` first).
    ///
    /// Span notes: `Stmt::If` does not store individual spans for `then_block`
    /// and `else_block`.  We wrap their `Block` values inside `Expr::Block`
    /// nodes and assign each one a span that starts at the end of the
    /// preceding element and ends at the close of the outer statement.  This is
    /// a conservative approximation; diagnostic positions within the blocks are
    /// still anchored by the expressions and statements inside them, so the
    /// wrapper span matters only for coarse messages like "this block has type
    /// T" — where "the if statement" is equally informative.
    pub(crate) fn promote_stmt_to_trailing_expr(stmt: Spanned<Stmt>) -> Option<Spanned<Expr>> {
        let (stmt_kind, stmt_span) = stmt;
        match stmt_kind {
            Stmt::If {
                condition,
                then_block,
                else_block,
            } => {
                // Wrap `then_block: Block` as `Expr::Block` with an empty
                // (zero-length) span.
                //
                // WHY empty span: `Stmt::If` does not store individual block
                // spans; they are consumed inside `parse_block` and not
                // propagated.  Recovering them without modifying `Stmt::If`
                // (which would cascade through HIR/MIR and serialisers) is out
                // of scope for this slice.
                //
                // CONSERVATIVE CORRECTNESS: `span_contains_offset` in
                // completions.rs returns `true` for any empty span, so LSP
                // completions walk both branches unconditionally — matching the
                // pre-promotion behaviour where `collect_locals_from_stmt` for
                // `Stmt::If` also walks both branches without a span guard.
                //
                // WHEN obsolete: once `Stmt::If` carries `then_block_span` and
                // `else_block_span` fields, thread them through here and drop
                // the empty-span approximation.
                //
                // WHAT the real fix looks like: capture `self.peek_span()` at
                // each `parse_block()` call site for `then_block` and `else_block`
                // inside `parse_stmt`, store those ranges on `Stmt::If`, then
                // use them here.
                let block_start = condition.1.end;
                let then_expr = Box::new((Expr::Block(then_block), block_start..block_start));
                let else_expr = else_block
                    .map(|eb| Box::new(Self::convert_else_block_to_expr(eb, stmt_span.end)));
                Some((
                    Expr::If {
                        condition: Box::new(condition),
                        then_block: then_expr,
                        else_block: else_expr,
                    },
                    stmt_span,
                ))
            }
            Stmt::IfLet {
                pattern,
                expr,
                body,
                else_body,
            } => {
                // Stmt::IfLet and Expr::IfLet share the same `else_body: Option<Block>` type.
                Some((
                    Expr::IfLet {
                        pattern,
                        expr,
                        body,
                        else_body,
                    },
                    stmt_span,
                ))
            }
            Stmt::Match { scrutinee, arms } => Some((
                Expr::Match {
                    scrutinee: Box::new(scrutinee),
                    arms,
                },
                stmt_span,
            )),
            _ => None,
        }
    }

    /// Convert an `ElseBlock` (from `Stmt::If`) into a `Spanned<Expr>` usable
    /// as the `else_block` field of `Expr::If`.
    pub(crate) fn convert_else_block_to_expr(
        else_block: ElseBlock,
        parent_end: usize,
    ) -> Spanned<Expr> {
        if let Some(if_stmt) = else_block.if_stmt {
            // `else if ...` — recursively promote the nested `Stmt::If`.
            let span = if_stmt.1.clone();
            if let Some(promoted) = Self::promote_stmt_to_trailing_expr(*if_stmt) {
                return promoted;
            }
            // Fallback: should be unreachable if the nested stmt is Stmt::If.
            (
                Expr::Block(Block {
                    stmts: vec![],
                    trailing_expr: None,
                }),
                span,
            )
        } else if let Some(block) = else_block.block {
            // `else { ... }` — Block has no own span; use an empty span so that
            // span_contains_offset always returns true (empty spans are treated as
            // "universally contained").  This is conservative but correct: callers
            // still consult inner statement spans for finer positioning.
            // See the WHY/WHEN/WHAT on the Stmt::If arm in
            // `promote_stmt_to_trailing_expr` for the full rationale.
            (Expr::Block(block), parent_end..parent_end)
        } else {
            // Malformed ElseBlock — produce an empty block as a safe default.
            (
                Expr::Block(Block {
                    stmts: vec![],
                    trailing_expr: None,
                }),
                parent_end..parent_end,
            )
        }
    }

    #[expect(clippy::too_many_lines, reason = "parser function with many branches")]
    pub(crate) fn parse_stmt(&mut self) -> Option<Spanned<Stmt>> {
        let _guard = self.enter_recursion()?;
        let start = self.peek_span().start;

        // Check for labeled loop/while: @label: loop/while. A bare @name in
        // expression position is a context-reader candidate.
        if matches!(self.peek(), Some(Token::Label(_)))
            && self.peek_at(self.pos + 1) == Some(&Token::Colon)
        {
            return self.parse_labeled_stmt(start);
        }

        let stmt = match self.peek() {
            Some(Token::Let) => {
                self.advance();
                let pattern = self.parse_pattern()?;

                // `let r? = expr;` is syntactic sugar for `let r = expr?;`.
                // The `?` must immediately follow a simple identifier pattern;
                // complex patterns (tuples, constructors) cannot carry the
                // propagation suffix — the binding site is ambiguous without a
                // single name to anchor the unwrapped value to.
                let propagate = if self.peek() == Some(&Token::Question) {
                    let q_span = self.peek_span();
                    if !matches!(pattern.0, Pattern::Identifier(_)) {
                        self.error_at(
                            "`?` propagation suffix requires a simple identifier pattern"
                                .to_string(),
                            q_span,
                        );
                        return None;
                    }
                    self.advance();
                    true
                } else {
                    false
                };

                let ty = if self.eat(&Token::Colon) {
                    Some(self.parse_type()?)
                } else {
                    None
                };

                let value = if self.eat(&Token::Equal) {
                    let (expr, expr_span) = self.parse_expr()?;
                    if propagate {
                        // Desugar: wrap RHS in PostfixTry so `let r? = e;`
                        // is exactly `let r = e?;` from the type-checker onward.
                        // The span covers the full RHS so diagnostics from the
                        // `?` type-check land on the expression, not on `r?`.
                        let end = expr_span.end;
                        Some((
                            Expr::PostfixTry(Box::new((expr, expr_span))),
                            pattern.1.start..end,
                        ))
                    } else {
                        Some((expr, expr_span))
                    }
                } else if propagate {
                    self.error(
                        "`let r? = expr;` requires an initialiser; `let r?;` is not valid"
                            .to_string(),
                    );
                    return None;
                } else {
                    None
                };

                // `let Pat = expr else { <diverging block> };` — the let-else
                // fallback clause, parsed AFTER the value and BEFORE the
                // terminating `;`. The else block is carried structurally so
                // the checker can enforce that it diverges; it is NOT desugared
                // away here. `let r? = e else {…}` is rejected: the `?`
                // propagation suffix already supplies a fallback path, so an
                // `else` clause would be contradictory. An `else` with no
                // initialiser (`let x else {…}`) has nothing to bind, so it is
                // also rejected.
                let else_block = if self.eat(&Token::Else) {
                    if propagate {
                        self.error(
                            "`?` propagation suffix and an `else` clause cannot both \
                             appear on a `let`; use one or the other"
                                .to_string(),
                        );
                        return None;
                    }
                    if value.is_none() {
                        self.error(
                            "`let … else { … }` requires an initialiser before the \
                             `else` clause"
                                .to_string(),
                        );
                        return None;
                    }
                    Some(self.parse_block()?)
                } else {
                    None
                };

                self.expect(&Token::Semicolon)?;

                Stmt::Let {
                    pattern,
                    ty,
                    value,
                    else_block,
                }
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
                    Stmt::IfLet {
                        pattern,
                        expr,
                        body,
                        else_body,
                    }
                } else {
                    let condition = self.parse_cond_expr()?;
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
            }
            Some(Token::Match) => {
                self.advance();
                let scrutinee = self.parse_cond_expr()?;
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

                Stmt::Match { scrutinee, arms }
            }
            Some(Token::Loop) => {
                self.advance();
                let body = self.parse_block()?;
                Stmt::Loop { label: None, body }
            }
            Some(Token::While) => {
                self.advance();
                if self.eat(&Token::Let) {
                    let pattern = Box::new(self.parse_pattern()?);
                    self.expect(&Token::Equal)?;
                    let expr = Box::new(self.parse_expr()?);
                    let body = self.parse_block()?;
                    Stmt::WhileLet {
                        label: None,
                        pattern,
                        expr,
                        body,
                    }
                } else {
                    let condition = self.parse_cond_expr()?;
                    let body = self.parse_block()?;
                    Stmt::While {
                        label: None,
                        condition,
                        body,
                    }
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
                    label: None,
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
                let value = if matches!(self.peek(), Some(Token::Semicolon | Token::RightBrace)) {
                    None
                } else {
                    Some(self.parse_expr()?)
                };
                // A statement-position `return` normally ends with `;`. When it
                // is the last item in a block (`}` follows, no `;`), it is the
                // block's trailing `return` — `parse_block` promotes the
                // resulting `Stmt::Return` to a trailing `Expr::Return`, so do
                // not demand a `;` here. This is the `else { return … }`
                // let-else body and the `if c { return … }`-tail shape.
                if self.peek() != Some(&Token::RightBrace) {
                    self.expect(&Token::Semicolon)?;
                }
                Stmt::Return(value)
            }
            Some(Token::Defer) => {
                self.advance();
                let expr = self.parse_expr()?;
                // Block expressions don't need a trailing semicolon
                // (consistent with if/while/for).
                if !matches!(expr.0, Expr::Block(_)) {
                    self.expect(&Token::Semicolon)?;
                }
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
    pub(crate) fn parse_labeled_stmt(&mut self, start: usize) -> Option<Spanned<Stmt>> {
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
                if self.eat(&Token::Let) {
                    let pattern = Box::new(self.parse_pattern()?);
                    self.expect(&Token::Equal)?;
                    let expr = Box::new(self.parse_expr()?);
                    let body = self.parse_block()?;
                    Stmt::WhileLet {
                        label: Some(label),
                        pattern,
                        expr,
                        body,
                    }
                } else {
                    let condition = self.parse_cond_expr()?;
                    let body = self.parse_block()?;
                    Stmt::While {
                        label: Some(label),
                        condition,
                        body,
                    }
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
            Some(Token::For) => {
                self.advance();
                let is_await = self.eat(&Token::Await);
                let pattern = self.parse_pattern()?;
                self.expect(&Token::In)?;
                let iterable = self.parse_expr()?;
                let body = self.parse_block()?;
                Stmt::For {
                    label: Some(label),
                    is_await,
                    pattern,
                    iterable,
                    body,
                }
            }
            _ => {
                self.error("expected `while`, `loop`, or `for` after label".to_string());
                return None;
            }
        };

        let end = self.peek_span().start;
        Some((stmt, start..end))
    }

    pub(crate) fn parse_compound_assign_op(&mut self) -> Option<CompoundAssignOp> {
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
}
