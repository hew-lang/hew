//! Semantic token analysis for Hew source code.
//!
//! Produces absolute byte-offset tokens from lexer output, independent of any
//! LSP encoding.  The caller (e.g. `hew-lsp`) is responsible for converting
//! these into whatever delta-encoded, UTF-16 form the protocol requires.

use hew_lexer::Token;

use crate::{token_modifiers, token_types, SemanticToken};

/// Classify a single lexer token into a semantic token type index.
fn classify_token(token: &Token<'_>) -> Option<u32> {
    if token.is_keyword() {
        return Some(token_types::KEYWORD);
    }
    if token.is_operator() {
        return Some(token_types::OPERATOR);
    }
    match token {
        // Literals
        Token::Integer(_) | Token::Float(_) | Token::Duration(_) => Some(token_types::NUMBER),
        Token::StringLit(_)
        | Token::RawString(_)
        | Token::InterpolatedString(_)
        | Token::RegexLiteral(_) => Some(token_types::STRING),

        // Identifiers
        Token::Identifier(_) | Token::Label(_) => Some(token_types::VARIABLE),

        // Comments (doc comments are emitted as tokens, unlike regular comments)
        Token::DocComment(_) | Token::InnerDocComment(_) => Some(token_types::COMMENT),

        // Skip delimiters, punctuation, errors, block comments
        _ => None,
    }
}

/// Keywords that introduce a named declaration — the identifier immediately
/// following one of these tokens is a declaration site.
fn is_function_decl_context(prev: Option<&Token<'_>>) -> bool {
    matches!(prev, Some(Token::Fn | Token::Receive))
}

fn is_type_decl_context(prev: Option<&Token<'_>>) -> bool {
    prev.is_some_and(Token::is_type_decl_keyword)
}

/// Build semantic tokens from Hew source code.
///
/// Returns a list of [`SemanticToken`] values with **absolute byte offsets**
/// (`start` is a byte offset into `source`, `length` is in bytes).  The token
/// type and modifier fields use the constants from [`crate::token_types`] and
/// [`crate::token_modifiers`].
#[must_use]
pub fn build_semantic_tokens(source: &str) -> Vec<SemanticToken> {
    let lexer_tokens = hew_lexer::lex(source);
    let mut result = Vec::new();

    for (i, (token, span)) in lexer_tokens.iter().enumerate() {
        let Some(mut token_type) = classify_token(token) else {
            continue;
        };
        if matches!(token, Token::Identifier(_)) {
            let prev = i
                .checked_sub(1)
                .and_then(|idx| lexer_tokens.get(idx).map(|(prev, _)| prev));
            if is_function_decl_context(prev) {
                token_type = token_types::FUNCTION;
            } else if is_type_decl_context(prev) {
                token_type = token_types::TYPE;
            }
        }

        let length = span.end - span.start;

        // Compute modifier bitset from lexer context.
        let mut modifiers: u32 = 0;

        // Mark identifiers after declaration keywords as DECLARATION.
        if matches!(token, Token::Identifier(_)) {
            if i > 0 && lexer_tokens[i - 1].0.is_decl_keyword() {
                modifiers |= token_modifiers::DECLARATION;
            }
            // Identifiers after `const` are also READONLY.
            if i > 0 && matches!(lexer_tokens[i - 1].0, Token::Const) {
                modifiers |= token_modifiers::READONLY;
            }
        }

        // Mark `async` keyword with ASYNC modifier.
        if matches!(token, Token::Async) {
            modifiers |= token_modifiers::ASYNC;
        }

        // Mark labels at definition sites ('label:) with DECLARATION modifier.
        if matches!(token, Token::Label(_)) {
            let next = lexer_tokens.get(i + 1).map(|(t, _)| t);
            if matches!(next, Some(Token::Colon)) {
                modifiers |= token_modifiers::DECLARATION;
            }
        }

        result.push(SemanticToken {
            start: span.start,
            length,
            token_type,
            modifiers,
        });
    }

    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn let_statement_tokens() {
        let tokens = build_semantic_tokens("let x = 42;");
        assert!(!tokens.is_empty());
        // First token: `let` keyword
        assert_eq!(tokens[0].token_type, token_types::KEYWORD);
        assert_eq!(tokens[0].start, 0);
        assert_eq!(tokens[0].length, 3);
        // Second token: `x` variable with DECLARATION modifier
        assert_eq!(tokens[1].token_type, token_types::VARIABLE);
        assert_eq!(tokens[1].start, 4);
        assert_eq!(tokens[1].length, 1);
        assert_ne!(
            tokens[1].modifiers & token_modifiers::DECLARATION,
            0,
            "x should have DECLARATION modifier"
        );
    }

    #[test]
    fn const_gets_readonly_modifier() {
        let tokens = build_semantic_tokens("const Y = 1;");
        // Y should have both DECLARATION and READONLY
        let y = &tokens[1];
        assert_eq!(y.token_type, token_types::VARIABLE);
        assert_ne!(y.modifiers & token_modifiers::DECLARATION, 0);
        assert_ne!(y.modifiers & token_modifiers::READONLY, 0);
    }

    #[test]
    fn async_keyword_gets_async_modifier() {
        let tokens = build_semantic_tokens("async fn foo() {}");
        let async_tok = &tokens[0];
        assert_eq!(async_tok.token_type, token_types::KEYWORD);
        assert_ne!(async_tok.modifiers & token_modifiers::ASYNC, 0);
    }

    #[test]
    fn function_and_type_declarations() {
        let tokens = build_semantic_tokens("fn calc(v: i32) -> i32 { v }");
        // `calc` should be typed as FUNCTION
        let calc = tokens.iter().find(|t| t.start == 3).unwrap();
        assert_eq!(calc.token_type, token_types::FUNCTION);
    }

    #[test]
    fn label_definition_gets_declaration_modifier() {
        // @outer: loop { break @outer; }
        let tokens = build_semantic_tokens("@outer: loop { break @outer; }");
        let labels: Vec<_> = tokens
            .iter()
            .filter(|t| t.token_type == token_types::VARIABLE)
            .collect();
        assert!(
            labels.len() >= 2,
            "expected at least two label tokens, got {}",
            labels.len()
        );
        // Definition site (@outer:) should have DECLARATION modifier.
        let def_label = &labels[0];
        assert_ne!(
            def_label.modifiers & token_modifiers::DECLARATION,
            0,
            "label at definition site should have DECLARATION modifier"
        );
        // Usage site (break @outer) should NOT have DECLARATION modifier.
        let use_label = &labels[1];
        assert_eq!(
            use_label.modifiers & token_modifiers::DECLARATION,
            0,
            "label at usage site should not have DECLARATION modifier"
        );
    }
}
