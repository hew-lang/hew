use super::*;

/// A parsed grammar fragment and the byte boundary it consumed.
#[derive(Debug)]
pub struct FragmentParseResult {
    pub fragment: Option<Fragment>,
    pub consumed: usize,
    pub errors: Vec<ParseError>,
}

/// Parse one macro argument with the ordinary Hew grammar.
#[must_use]
pub fn parse_fragment(
    source: &str,
    offset: usize,
    contexts: Arc<TokenContexts>,
    kind: FragmentKind,
) -> FragmentParseResult {
    parse_generated_fragment(source, offset, contexts, FragmentInputs::default(), kind)
}

/// Parse one generated fragment with structural captures.
#[must_use]
pub fn parse_generated_fragment(
    source: &str,
    offset: usize,
    contexts: Arc<TokenContexts>,
    fragments: FragmentInputs,
    kind: FragmentKind,
) -> FragmentParseResult {
    let mut parser = Parser::with_contexts(source, offset, contexts);
    parser.fragments = fragments;
    let fragment = parser.parse_fragment_kind(kind);
    FragmentParseResult {
        fragment,
        consumed: parser.last_token_end - offset,
        errors: parser.errors,
    }
}

/// Parse a template, treating captured syntax as atomic grammar nodes.
#[must_use]
pub fn parse_generated(
    source: &str,
    offset: usize,
    contexts: Arc<TokenContexts>,
    fragments: FragmentInputs,
) -> ParseResult {
    let mut parser = Parser::with_contexts(source, offset, contexts);
    parser.fragments = fragments;
    let program = parser.parse_program();
    ParseResult {
        program,
        errors: parser.errors,
    }
}

impl Parser<'_> {
    #[must_use]
    pub fn with_fragments(mut self, fragments: FragmentInputs) -> Self {
        self.fragments = fragments;
        self
    }

    /// Parse a fragment at a token boundary without re-lexing or changing the cursor.
    #[must_use]
    pub fn parse_fragment_at(&mut self, start: usize, kind: FragmentKind) -> FragmentParseResult {
        let saved = self.save_pos();
        self.pos = self.tokens.partition_point(|(_, span)| span.start < start);
        self.last_token_end = start;
        let error_start = self.errors.len();
        let fragment = self.parse_fragment_kind(kind);
        let result = FragmentParseResult {
            fragment,
            consumed: self.last_token_end - start,
            errors: self.errors[error_start..].to_vec(),
        };
        self.restore_pos(saved);
        result
    }

    fn parse_fragment_kind(&mut self, kind: FragmentKind) -> Option<Fragment> {
        match kind {
            FragmentKind::Ident => self.expect_ident_spanned().map(Fragment::Ident),
            FragmentKind::Expr => self.parse_expr().map(Fragment::Expr),
            FragmentKind::Ty => self.parse_type().map(Fragment::Ty),
            FragmentKind::Pat => self.parse_pattern().map(Fragment::Pat),
            FragmentKind::Block => {
                let start = self.peek_span().start;
                self.parse_block()
                    .map(|block| Fragment::Block((block, start..self.last_token_end)))
            }
            FragmentKind::Literal => self.parse_literal_fragment().map(Fragment::Literal),
        }
    }

    pub(crate) fn peek_fragment(&self) -> Option<Fragment> {
        self.fragments.at(self.peek_span().start).cloned()
    }

    pub(crate) fn consume_fragment(&mut self, span: &Span) {
        while !self.at_end() && self.peek_span().start < span.end {
            self.advance();
        }
    }

    pub(crate) fn take_expression_fragment(&mut self) -> Result<Option<Spanned<Expr>>, ()> {
        let Some(fragment) = self.peek_fragment() else {
            return Ok(None);
        };
        let expression = match fragment {
            Fragment::Expr(expression) | Fragment::Literal(expression) => expression,
            Fragment::Block((block, span)) => (Expr::Block(block), span),
            Fragment::Ident(_) => return Ok(None),
            _ => return Err(self.invalid_fragment_position(&fragment, "an expression")),
        };
        self.consume_fragment(&expression.1);
        if let Some(expander) = self.expander.as_ref() {
            let span = expression.1.clone();
            if let Some(source) = self
                .source
                .get(span.start - self.source_offset..span.end - self.source_offset)
                .filter(|source| source.contains('!'))
            {
                let mut fragments = self.fragments.within(span.clone());
                fragments.remove(span.start);
                let mut parser =
                    Parser::with_contexts(source, span.start, Arc::clone(&self.contexts))
                        .with_fragments(fragments)
                        .with_expander(Rc::clone(expander), self.macro_source.clone());
                let expanded = parser.parse_expr();
                self.errors.extend(parser.errors);
                return expanded.map(Some).ok_or(());
            }
        }
        Ok(Some(expression))
    }

    pub(crate) fn take_pattern_fragment(&mut self) -> Result<Option<Spanned<Pattern>>, ()> {
        let Some(fragment) = self.peek_fragment() else {
            return Ok(None);
        };
        let pattern = match fragment {
            Fragment::Pat(pattern) => pattern,
            Fragment::Ident(_) | Fragment::Literal(_) => return Ok(None),
            _ => return Err(self.invalid_fragment_position(&fragment, "a pattern")),
        };
        self.consume_fragment(&pattern.1);
        Ok(Some(pattern))
    }

    pub(crate) fn take_type_fragment(&mut self) -> Result<Option<Spanned<TypeExpr>>, ()> {
        let Some(fragment) = self.peek_fragment() else {
            return Ok(None);
        };
        let ty = match fragment {
            Fragment::Ty(ty) => ty,
            Fragment::Ident(_) => return Ok(None),
            _ => return Err(self.invalid_fragment_position(&fragment, "a type")),
        };
        self.consume_fragment(&ty.1);
        Ok(Some(ty))
    }

    pub(crate) fn invalid_fragment_position(&mut self, fragment: &Fragment, expected: &str) {
        self.error_at(
            format!("captured macro fragment cannot be used as {expected}"),
            fragment.span().clone(),
        );
        self.consume_fragment(fragment.span());
    }

    fn parse_literal_fragment(&mut self) -> Option<Spanned<Expr>> {
        let start = self.peek_span().start;
        let negative = self.eat(&Token::Minus);
        let expression = self.parse_primary()?;
        let numeric = matches!(
            expression.0,
            Expr::Literal(Literal::Integer { .. } | Literal::Float(_))
        );
        let literal = matches!(
            expression.0,
            Expr::Literal(_)
                | Expr::ByteStringLiteral(_)
                | Expr::ByteArrayLiteral(_)
                | Expr::RegexLiteral(_)
        );
        if !literal || negative && !numeric {
            self.error_at("expected a literal macro argument".into(), expression.1);
            return None;
        }
        if negative {
            let end = expression.1.end;
            Some((
                Expr::Unary {
                    op: UnaryOp::Negate,
                    operand: Box::new(expression),
                },
                start..end,
            ))
        } else {
            Some(expression)
        }
    }
}
