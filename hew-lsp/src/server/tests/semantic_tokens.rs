use super::*;

fn semantic_token_data(source: &str, tokens: &[SemanticToken]) -> Vec<(String, u32, u32)> {
    let lo = compute_line_offsets(source);
    let mut line = 0u32;
    let mut col = 0u32;
    let mut data = Vec::new();
    for token in tokens {
        line += token.delta_line;
        if token.delta_line == 0 {
            col += token.delta_start;
        } else {
            col = token.delta_start;
        }
        let start = position_to_offset(source, &lo, Position::new(line, col));
        let end = position_to_offset(source, &lo, Position::new(line, col + token.length));
        data.push((
            source[start..end].to_string(),
            token.token_type,
            token.token_modifiers_bitset,
        ));
    }
    data
}

#[test]
fn semantic_tokens_for_let() {
    let source = "let x = 42;";
    let lo = compute_line_offsets(source);
    let analysis_tokens = hew_analysis::semantic_tokens::build_semantic_tokens(source);
    let tokens = analysis_tokens_to_lsp(source, &lo, &analysis_tokens)
        .expect("semantic token encoding should succeed");
    assert!(!tokens.is_empty());
    // First token should be `let` keyword
    assert_eq!(tokens[0].token_type, hew_analysis::token_types::KEYWORD);
    // Second token is `x` identifier — should have DECLARATION modifier (bit 0)
    assert_eq!(tokens[1].token_type, hew_analysis::token_types::VARIABLE);
    let decl = modifier_bit(&SemanticTokenModifier::DECLARATION);
    assert_eq!(
        tokens[1].token_modifiers_bitset & decl,
        decl,
        "x should be DECLARATION"
    );
}

#[test]
fn semantic_token_modifiers_const() {
    let source = "const Y = 1;";
    let lo = compute_line_offsets(source);
    let analysis_tokens = hew_analysis::semantic_tokens::build_semantic_tokens(source);
    let tokens = analysis_tokens_to_lsp(source, &lo, &analysis_tokens)
        .expect("semantic token encoding should succeed");
    let decl = modifier_bit(&SemanticTokenModifier::DECLARATION);
    let readonly = modifier_bit(&SemanticTokenModifier::READONLY);
    // Find the `const` keyword and the identifier after it.
    let const_tok = &tokens[0]; // `const`
    assert_eq!(const_tok.token_type, hew_analysis::token_types::KEYWORD);
    let y_tok = &tokens[1]; // `Y`
    assert_eq!(y_tok.token_type, hew_analysis::token_types::VARIABLE);
    assert_eq!(
        y_tok.token_modifiers_bitset & decl,
        decl,
        "Y should be DECLARATION"
    );
    assert_eq!(
        y_tok.token_modifiers_bitset & readonly,
        readonly,
        "Y should be READONLY"
    );
}

#[test]
fn semantic_tokens_include_bitwise_and_shift_operators() {
    let source = "fn main() { let a = 1; let b = 2; let x = ~a << b >> 1 & a | b ^ a; x <<= 1; x >>= 1; x &= 1; x |= 1; x ^= 1; }";
    let lo = compute_line_offsets(source);
    let analysis_tokens = hew_analysis::semantic_tokens::build_semantic_tokens(source);
    let tokens = analysis_tokens_to_lsp(source, &lo, &analysis_tokens)
        .expect("semantic token encoding should succeed");
    let operator_token_type = hew_analysis::token_types::OPERATOR;
    let token_data = semantic_token_data(source, &tokens);
    let operator_texts: Vec<String> = token_data
        .into_iter()
        .filter(|(_, token_type, _)| *token_type == operator_token_type)
        .map(|(text, _, _)| text)
        .collect();
    for op in [
        "~", "<<", ">>", "<<=", ">>=", "&=", "|=", "^=", "&", "|", "^",
    ] {
        assert!(
            operator_texts.iter().any(|text| text == op),
            "expected operator token '{op}', got {operator_texts:?}"
        );
    }
}

#[test]
fn semantic_tokens_mark_function_and_type_declarations() {
    let source = "type Point {\n    x: i32;\n}\n\ntrait Stream {\n    type Item;\n    fn next() -> i32;\n}\n\nfn calc(v: i32) -> i32 {\n    v\n}\n";
    let lo = compute_line_offsets(source);
    let analysis_tokens = hew_analysis::semantic_tokens::build_semantic_tokens(source);
    let tokens = analysis_tokens_to_lsp(source, &lo, &analysis_tokens)
        .expect("semantic token encoding should succeed");
    let token_data = semantic_token_data(source, &tokens);
    let function_token_type = hew_analysis::token_types::FUNCTION;
    let type_token_type = hew_analysis::token_types::TYPE;
    let decl = modifier_bit(&SemanticTokenModifier::DECLARATION);

    assert!(
        token_data
            .iter()
            .any(|(text, token_type, modifiers)| text == "calc"
                && *token_type == function_token_type
                && (modifiers & decl) == decl),
        "expected function declaration token for calc"
    );
    assert!(
        token_data
            .iter()
            .any(|(text, token_type, _)| text == "next" && *token_type == function_token_type),
        "expected function token for trait method next"
    );
    assert!(
        token_data
            .iter()
            .any(|(text, token_type, _)| text == "Point" && *token_type == type_token_type),
        "expected type token for Point"
    );
    assert!(
        token_data
            .iter()
            .any(|(text, token_type, _)| text == "Item" && *token_type == type_token_type),
        "expected type token for associated type Item"
    );
}

#[test]
fn semantic_tokens_reject_delta_underflow() {
    let source = "let alpha = 1; let beta = 2;";
    let lo = compute_line_offsets(source);
    let tokens = vec![
        hew_analysis::SemanticToken {
            start: source.find("beta").expect("beta token"),
            length: "beta".len(),
            token_type: hew_analysis::token_types::VARIABLE,
            modifiers: 0,
        },
        hew_analysis::SemanticToken {
            start: source.find("alpha").expect("alpha token"),
            length: "alpha".len(),
            token_type: hew_analysis::token_types::VARIABLE,
            modifiers: 0,
        },
    ];
    let err = analysis_tokens_to_lsp(source, &lo, &tokens)
        .expect_err("out-of-order tokens must be rejected");
    assert!(
        err.message().contains("semantic token delta underflow"),
        "expected underflow error, got: {err:?}"
    );
}
