use hew_parser::ast::{Expr, Literal, Stmt};

fn parse_expr(src: &str) -> Expr {
    let result = hew_parser::parse(src);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let item = &result.program.items[0].0;
    match item {
        hew_parser::ast::Item::Function(f) => {
            let stmt = &f.body.stmts[0].0;
            match stmt {
                Stmt::Let {
                    value: Some(val), ..
                } => val.0.clone(),
                _ => panic!("expected let statement"),
            }
        }
        _ => panic!("expected function item"),
    }
}

#[test]
fn char_literal_basic() {
    let expr = parse_expr("fn main() { let c = 'a'; }");
    assert_eq!(expr, Expr::Literal(Literal::Char('a')));
}

#[test]
fn char_literal_newline_escape() {
    let expr = parse_expr(r"fn main() { let c = '\n'; }");
    assert_eq!(expr, Expr::Literal(Literal::Char('\n')));
}

#[test]
fn char_literal_tab_escape() {
    let expr = parse_expr(r"fn main() { let c = '\t'; }");
    assert_eq!(expr, Expr::Literal(Literal::Char('\t')));
}

#[test]
fn char_literal_null_escape() {
    let expr = parse_expr(r"fn main() { let c = '\0'; }");
    assert_eq!(expr, Expr::Literal(Literal::Char('\0')));
}

#[test]
fn char_literal_backslash_escape() {
    let expr = parse_expr(r"fn main() { let c = '\\'; }");
    assert_eq!(expr, Expr::Literal(Literal::Char('\\')));
}

#[test]
fn char_literal_single_quote_escape() {
    let expr = parse_expr(r"fn main() { let c = '\''; }");
    assert_eq!(expr, Expr::Literal(Literal::Char('\'')));
}

#[test]
fn char_literal_unicode_escape_bmp() {
    // \u{...} now decodes to the scalar — required so the formatter can write
    // an invisible char as an escape rather than a raw codepoint.
    let expr = parse_expr(r"fn main() { let c = '\u{202E}'; }");
    assert_eq!(expr, Expr::Literal(Literal::Char('\u{202E}')));
}

#[test]
fn char_literal_unicode_escape_supplementary() {
    let expr = parse_expr(r"fn main() { let c = '\u{1F600}'; }");
    assert_eq!(expr, Expr::Literal(Literal::Char('\u{1F600}')));
}

#[test]
fn char_literal_unicode_escape_min_digits() {
    let expr = parse_expr(r"fn main() { let c = '\u{9}'; }");
    assert_eq!(expr, Expr::Literal(Literal::Char('\u{9}')));
}

#[test]
fn char_literal_hex_byte_escape() {
    let expr = parse_expr(r"fn main() { let c = '\x41'; }");
    assert_eq!(expr, Expr::Literal(Literal::Char('A')));
}

#[test]
fn char_literal_rejects_unterminated_unicode_escape() {
    // The lexer regex requires a closing `}`; an unterminated escape must not
    // silently parse — fail closed.
    let result = hew_parser::parse(r"fn main() { let c = '\u{202E'; }");
    assert!(
        !result.errors.is_empty(),
        "unterminated \\u{{ escape must be rejected"
    );
}
