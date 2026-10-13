#[allow(clippy::wildcard_imports, reason = "shared parser grammar namespace")]
use super::*;
use crate::macro_syntax::{
    Delimiter, ExpansionCategory, MacroCall, MacroDecl, MacroRule, SharedExpander, TokenTree,
};

impl Parser<'_> {
    #[must_use]
    pub fn with_expander(mut self, expander: SharedExpander, source: std::path::PathBuf) -> Self {
        self.expander = Some(expander);
        self.macro_source = source;
        self
    }

    pub fn finish_expanded(mut self) -> crate::ParseResult {
        let program = self.parse_program();
        crate::ParseResult {
            program,
            errors: self.errors,
        }
    }

    pub(crate) fn is_macro_declaration(&self) -> bool {
        matches!(self.peek(), Some(Token::Identifier("macro")))
            && matches!(self.peek_at(self.pos + 1), Some(Token::Identifier(_)))
            && self.peek_at(self.pos + 2) == Some(&Token::LeftBrace)
    }

    pub(crate) fn is_macro_call(&self) -> bool {
        let mut cursor = self.pos;
        loop {
            if !matches!(self.peek_at(cursor), Some(Token::Identifier(_))) {
                return false;
            }
            cursor += 1;
            match self.peek_at(cursor) {
                Some(Token::Bang) => return true,
                Some(Token::Dot) => cursor += 1,
                _ => return false,
            }
        }
    }

    pub(crate) fn parse_macro_call(&mut self) -> Option<MacroCall> {
        let mut segments = vec![self.expect_ident_spanned()?];
        while self.eat(&Token::Dot) {
            segments.push(self.expect_ident_spanned()?);
        }
        self.expect(&Token::Bang)?;
        let input = self.parse_token_tree(Delimiter::Parenthesis)?;
        Some(MacroCall {
            path: Path { segments },
            input,
        })
    }

    pub(crate) fn expand_macro_expression(&mut self, call: &MacroCall) -> Option<Spanned<Expr>> {
        self.with_macro_expansion(call, ExpansionCategory::Expr, |parser| {
            let expression = parser.parse_expr();
            if !parser.at_end() {
                parser.error("an expression macro must produce exactly one expression".into());
            }
            expression
        })
    }

    pub(crate) fn expand_macro_items(&mut self, call: &MacroCall) -> Option<Vec<Spanned<Item>>> {
        self.with_macro_expansion(call, ExpansionCategory::Items, |parser| {
            Some(parser.parse_program().items)
        })
    }

    fn with_macro_expansion<T>(
        &mut self,
        call: &MacroCall,
        category: ExpansionCategory,
        parse: impl FnOnce(&mut Parser<'_>) -> Option<T>,
    ) -> Option<T> {
        let expander = Rc::clone(self.expander.as_ref()?);
        let generated = match expander
            .borrow_mut()
            .begin(call, category, &self.macro_source)
        {
            Ok(generated) => generated,
            Err(message) => {
                self.error_at(
                    message,
                    call.path.segments.first()?.1.start..call.input.span().end,
                );
                return None;
            }
        };
        let mut parser =
            Parser::with_contexts(&generated.source, generated.offset, generated.contexts)
                .with_fragments(generated.fragments)
                .with_expander(Rc::clone(&expander), self.macro_source.clone());
        let result = parse(&mut parser);
        self.errors.extend(parser.errors);
        expander.borrow_mut().complete();
        result
    }

    /// Parse one contextual `macro` declaration.
    pub fn parse_macro_decl(&mut self, visibility: Visibility) -> Option<MacroDecl> {
        self.advance();
        let name = self.expect_ident()?;
        self.expect(&Token::LeftBrace)?;
        let mut rules = Vec::new();
        while !self.at_end() && self.peek() != Some(&Token::RightBrace) {
            let start = self.peek_span().start;
            let pattern = self.parse_token_tree(Delimiter::Parenthesis)?;
            self.expect(&Token::FatArrow)?;
            let category = match self.expect_ident()?.name.as_str() {
                "expr" => ExpansionCategory::Expr,
                "items" => ExpansionCategory::Items,
                _ => {
                    self.error("macro rule category must be `expr` or `items`".into());
                    return None;
                }
            };
            let template = self.parse_token_tree(Delimiter::Brace)?;
            let end = template.span().end;
            rules.push(MacroRule {
                pattern,
                template,
                category,
                span: start..end,
            });
            self.refuse_mark_after_body();
        }
        self.expect(&Token::RightBrace)?;
        if rules.is_empty() {
            self.error("a macro must declare at least one rule".into());
            return None;
        }
        Some(MacroDecl {
            visibility,
            name,
            rules,
        })
    }

    pub(crate) fn parse_token_tree(&mut self, delimiter: Delimiter) -> Option<TokenTree> {
        let _guard = self.enter_recursion()?;
        let (open, close) = match delimiter {
            Delimiter::Parenthesis => (Token::LeftParen, Token::RightParen),
            Delimiter::Bracket => (Token::LeftBracket, Token::RightBracket),
            Delimiter::Brace => (Token::LeftBrace, Token::RightBrace),
            Delimiter::Attribute => (Token::HashBracket, Token::RightBracket),
        };
        let leading = self.token_tree_trivia();
        let start = self.expect(&open)?.start;
        let mut children = Vec::new();
        while !self.at_end() && self.peek() != Some(&close) {
            if let Some(syntax) = self.peek_fragment() {
                let mut tokens = Vec::new();
                while !self.at_end() && self.peek_span().start < syntax.span().end {
                    tokens.push(self.parse_raw_token_tree_atom()?);
                }
                children.push(TokenTree::Fragment {
                    syntax: Box::new(syntax),
                    tokens,
                });
                continue;
            }
            children.push(self.parse_raw_token_tree_atom()?);
        }
        let closing = self.token_tree_trivia();
        let end = self.expect(&close)?.end;
        Some(TokenTree::Group {
            delimiter,
            span: start..end,
            children,
            leading,
            closing,
        })
    }

    fn parse_raw_token_tree_atom(&mut self) -> Option<TokenTree> {
        let nested = match self.peek()? {
            Token::LeftParen => Some(Delimiter::Parenthesis),
            Token::LeftBracket => Some(Delimiter::Bracket),
            Token::LeftBrace => Some(Delimiter::Brace),
            Token::HashBracket => Some(Delimiter::Attribute),
            Token::RightParen | Token::RightBracket | Token::RightBrace => {
                self.error("mismatched delimiter in macro syntax".into());
                return None;
            }
            _ => None,
        };
        if let Some(nested) = nested {
            self.parse_token_tree(nested)
        } else {
            let leading = self.token_tree_trivia();
            let (_, span) = self.advance()?;
            let spelling = self
                .source
                .get(span.start - self.source_offset..span.end - self.source_offset)?
                .to_string();
            let context = self.contexts.at(&span);
            Some(TokenTree::Token {
                spelling,
                span,
                context,
                leading,
            })
        }
    }

    fn token_tree_trivia(&self) -> String {
        self.source
            .get(
                self.last_token_end - self.source_offset
                    ..self.peek_span().start - self.source_offset,
            )
            .unwrap_or_default()
            .to_string()
    }
}
