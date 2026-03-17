//! Semantic token analysis for Hew source code.
//!
//! Produces absolute byte-offset tokens from lexer output, independent of any
//! LSP encoding.  The caller (e.g. `hew-lsp`) is responsible for converting
//! these into whatever delta-encoded, UTF-16 form the protocol requires.

use hew_lexer::Token;

use crate::{token_modifiers, token_types, SemanticToken};

/// Primitive type names that the lexer emits as `Identifier` but should be
/// highlighted as types.
const PRIMITIVE_TYPE_NAMES: &[&str] = &[
    "i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64", "isize", "usize", "f32", "f64", "bool",
    "char", "string", "bytes", "void", "never", "duration",
];

/// Well-known generic/collection/concurrency type names from the standard
/// library.  These are always PascalCase, so they'd be caught by the
/// `starts_with(uppercase)` heuristic below, but listing them explicitly
/// makes the classification deterministic across source positions.
const BUILTIN_TYPE_NAMES: &[&str] = &[
    "Result",
    "Option",
    "Ok",
    "Err",
    "Some",
    "None",
    "Vec",
    "HashMap",
    "Arc",
    "Rc",
    "Weak",
    "ActorRef",
    "Task",
    "Scope",
    "Generator",
    "AsyncGenerator",
    "Stream",
    "Sink",
    "Send",
    "Frozen",
    "Copy",
    "Range",
    "ActorStream",
];

/// Returns `true` if `name` is a known type name (primitive or builtin).
fn is_type_name(name: &str) -> bool {
    PRIMITIVE_TYPE_NAMES.contains(&name) || BUILTIN_TYPE_NAMES.contains(&name)
}

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

/// Returns `true` if the previous token introduces a type annotation context
/// (`:` or `->`), meaning the next identifier is likely a type name.
fn is_type_annotation_context(prev: Option<&Token<'_>>) -> bool {
    matches!(prev, Some(Token::Colon | Token::Arrow))
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
        if let Token::Identifier(name) = token {
            let prev = i
                .checked_sub(1)
                .and_then(|idx| lexer_tokens.get(idx).map(|(prev, _)| prev));
            if is_function_decl_context(prev) {
                token_type = token_types::FUNCTION;
            } else if is_type_decl_context(prev) {
                token_type = token_types::TYPE;
            } else if is_type_name(name) {
                // Known primitive/builtin type name — always a type regardless
                // of position.
                token_type = token_types::TYPE;
            } else if is_type_annotation_context(prev)
                && name.starts_with(|c: char| c.is_ascii_uppercase())
            {
                // PascalCase identifier after `:` or `->` — user-defined type.
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
    fn param_type_annotation_classified_as_type() {
        let tokens = build_semantic_tokens("fn f(x: i32) {}");
        // `i32` at byte offset 8 should be TYPE, not VARIABLE
        let i32_tok = tokens.iter().find(|t| t.start == 8).unwrap();
        assert_eq!(
            i32_tok.token_type,
            token_types::TYPE,
            "i32 in parameter type should be TYPE"
        );
    }

    #[test]
    fn return_type_classified_as_type() {
        let tokens = build_semantic_tokens("fn f() -> f64 { 0.0 }");
        // `f64` after `->` should be TYPE
        let f64_tok = tokens
            .iter()
            .find(|t| {
                t.token_type == token_types::TYPE
                    && &"fn f() -> f64 { 0.0 }"[t.start..t.start + t.length] == "f64"
            })
            .expect("f64 return type should be classified as TYPE");
        assert_eq!(f64_tok.token_type, token_types::TYPE);
    }

    #[test]
    fn let_type_annotation_classified_as_type() {
        let tokens = build_semantic_tokens("let x: u8 = 0;");
        let u8_tok = tokens
            .iter()
            .find(|t| {
                let text = &"let x: u8 = 0;"[t.start..t.start + t.length];
                text == "u8"
            })
            .expect("u8 type annotation should produce a token");
        assert_eq!(
            u8_tok.token_type,
            token_types::TYPE,
            "u8 in let type annotation should be TYPE"
        );
    }

    #[test]
    fn user_type_after_colon_classified_as_type() {
        let tokens = build_semantic_tokens("fn f(p: Point) {}");
        let point_tok = tokens
            .iter()
            .find(|t| {
                let text = &"fn f(p: Point) {}"[t.start..t.start + t.length];
                text == "Point"
            })
            .expect("Point type should produce a token");
        assert_eq!(
            point_tok.token_type,
            token_types::TYPE,
            "PascalCase identifier after `:` should be TYPE"
        );
    }

    #[test]
    fn lowercase_identifier_after_colon_stays_variable() {
        // In struct init or named args, `name: value` — value stays VARIABLE
        let source = "fn f() { let x = Foo { name: val }; }";
        let tokens = build_semantic_tokens(source);
        let val_tok = tokens
            .iter()
            .find(|t| &source[t.start..t.start + t.length] == "val")
            .expect("val should produce a token");
        assert_eq!(
            val_tok.token_type,
            token_types::VARIABLE,
            "lowercase identifier after `:` in struct init should stay VARIABLE"
        );
    }

    #[test]
    fn all_primitive_types_classified() {
        for ty in super::PRIMITIVE_TYPE_NAMES {
            let source = format!("fn f(x: {ty}) {{}}");
            let tokens = build_semantic_tokens(&source);
            let ty_tok = tokens
                .iter()
                .find(|t| &source[t.start..t.start + t.length] == *ty)
                .unwrap_or_else(|| panic!("{ty} should produce a token"));
            assert_eq!(
                ty_tok.token_type,
                token_types::TYPE,
                "{ty} should be classified as TYPE"
            );
        }
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
