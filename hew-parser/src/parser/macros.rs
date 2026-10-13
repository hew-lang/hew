#[allow(clippy::wildcard_imports, reason = "shared parser grammar namespace")]
use super::*;
use crate::macro_syntax::{Delimiter, ExpansionCategory, MacroDecl, MacroRule, TokenTree};

impl Parser<'_> {
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
                children.push(self.parse_token_tree(nested)?);
            } else {
                let leading = self.token_tree_trivia();
                let (_, span) = self.advance()?;
                let spelling = self
                    .source
                    .get(span.start - self.source_offset..span.end - self.source_offset)?
                    .to_string();
                let context = self.contexts.at(&span);
                children.push(TokenTree::Token {
                    spelling,
                    span,
                    context,
                    leading,
                });
            }
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
