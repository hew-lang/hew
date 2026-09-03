//! Input classification for the REPL.
//!
//! Determines whether user input is a top-level item, a statement,
//! a REPL command, or a bare expression.

use hew_parser::ast::Item;

/// The kind of input entered at the REPL prompt.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InputKind {
    /// A top-level item (`fn`, `struct`, `enum`, `actor`, `trait`, `impl`, `pub`).
    Item,
    /// A statement (`let`, `var`, `if`, `for`, `while`, `return`).
    Statement,
    /// A REPL meta-command (starts with `:`).
    Command(ReplCommand),
    /// A bare expression to evaluate and auto-print.
    Expression,
}

/// Whether buffered REPL input should be evaluated, extended, or rejected.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InputCompleteness {
    /// The current buffer is a complete input and can be evaluated now.
    Complete,
    /// The current buffer is a valid prefix of a larger input and should keep buffering.
    Incomplete,
    /// The current buffer is syntactically invalid and should surface diagnostics now.
    Invalid,
}

/// A REPL meta-command.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ReplCommand {
    /// `:help` — show available commands.
    Help,
    /// `:quit` or `:q` — exit the REPL.
    Quit,
    /// `:session` or `:show` — summarize remembered state.
    Session,
    /// `:items` — list remembered top-level items.
    Items,
    /// `:clear` — reset session state.
    Clear,
    /// `:type <expr>` — show the inferred type of an expression.
    Type(String),
    /// `:load <file>` — load a `.hew` file into the session.
    Load(String),
    /// Unknown command.
    Unknown(String),
}

/// Classify a line of REPL input.
#[must_use]
pub fn classify(input: &str) -> InputKind {
    let trimmed = input.trim();
    if trimmed.is_empty() {
        return InputKind::Expression;
    }

    if let Some(cmd) = trimmed.strip_prefix(':') {
        return InputKind::Command(parse_command(cmd));
    }

    if parses_as_item(trimmed) {
        InputKind::Item
    } else if parses_as_statement(trimmed) {
        InputKind::Statement
    } else {
        InputKind::Expression
    }
}

/// Classify whether a buffered REPL input is complete enough to evaluate.
#[must_use]
pub fn input_completeness(input: &str) -> InputCompleteness {
    let trimmed = input.trim();
    if trimmed.is_empty() || trimmed.starts_with(':') {
        return InputCompleteness::Complete;
    }

    if parses_in_any_context(trimmed) {
        return InputCompleteness::Complete;
    }

    if has_unclosed_delimiters(trimmed)
        || parses_with_continuation(trimmed)
        || ends_with_bare_attribute(trimmed)
    {
        return InputCompleteness::Incomplete;
    }

    InputCompleteness::Invalid
}

/// True if `input` is nothing but one or more syntactically complete
/// `#[name]` / `#[name(args)]` attribute groups (`#[wire]`, stacked
/// `#[a]\n#[b]`, …) with no item yet attached to them.
///
/// An attribute alone has balanced delimiters (so `has_unclosed_delimiters`
/// says "complete") and does not parse in any standalone context (so
/// `parses_in_any_context` says "no"), which without this check falls
/// through to `Invalid` — surfacing a parse error on the attribute line
/// instead of buffering for the item it decorates.
///
/// This is a token-level check, deliberately independent of
/// HEW-SPEC-2026 §12.6's closed attribute table: whether `name` turns out to
/// be a recognised attribute (and legal in whatever position the item the
/// user is about to type would put it in) is the parser's job once the item
/// arrives. The REPL only needs to know "is this shape still waiting for an
/// item", and a misspelled or invented attribute name is exactly the input a
/// user is mid-way through typing — not evidence the buffer is unrecoverable.
fn ends_with_bare_attribute(input: &str) -> bool {
    let tokens = hew_lexer::lex(input);
    let mut pos = 0usize;
    let mut saw_group = false;
    while pos < tokens.len() {
        if !matches!(tokens[pos].0, hew_lexer::Token::HashBracket) {
            return false;
        }
        pos += 1;
        // Attribute name: any identifier-like token (including contextual
        // keywords used as attribute names, e.g. `#[on(..)]`).
        let Some((name_tok, _)) = tokens.get(pos) else {
            return false;
        };
        if !matches!(
            name_tok,
            hew_lexer::Token::Identifier(_) | hew_lexer::Token::On
        ) {
            return false;
        }
        pos += 1;
        if matches!(
            tokens.get(pos).map(|(t, _)| t),
            Some(hew_lexer::Token::LeftParen)
        ) {
            pos += 1;
            let mut depth = 1usize;
            while depth > 0 {
                match tokens.get(pos).map(|(t, _)| t) {
                    Some(hew_lexer::Token::LeftParen) => depth += 1,
                    Some(hew_lexer::Token::RightParen) => depth -= 1,
                    Some(_) => {}
                    None => return false,
                }
                pos += 1;
            }
        }
        if !matches!(
            tokens.get(pos).map(|(t, _)| t),
            Some(hew_lexer::Token::RightBracket)
        ) {
            return false;
        }
        pos += 1;
        saw_group = true;
    }
    saw_group
}

/// Parse a REPL command string (after the leading `:`).
fn parse_command(cmd: &str) -> ReplCommand {
    let parts: Vec<&str> = cmd.trim().splitn(2, char::is_whitespace).collect();
    let name = parts[0];
    let arg = parts.get(1).map(|s| s.trim().to_string());

    match name {
        "help" | "h" => ReplCommand::Help,
        "quit" | "q" | "exit" => ReplCommand::Quit,
        "session" | "show" => ReplCommand::Session,
        "items" => ReplCommand::Items,
        "clear" | "reset" => ReplCommand::Clear,
        "type" | "t" => ReplCommand::Type(arg.unwrap_or_default()),
        "load" | "l" => ReplCommand::Load(arg.unwrap_or_default()),
        other => ReplCommand::Unknown(other.to_string()),
    }
}

fn parses_as_item(input: &str) -> bool {
    let parse_result = hew_parser::parse(input);
    parse_result.errors.is_empty() && !parse_result.program.items.is_empty()
}

fn parses_as_statement(input: &str) -> bool {
    let source = format!("fn main() {{\n{input}\n}}\n");
    let parse_result = hew_parser::parse(&source);
    if !parse_result.errors.is_empty() || parse_result.program.items.len() != 1 {
        return false;
    }

    let Some((Item::Function(function), _)) = parse_result.program.items.first() else {
        return false;
    };

    function.body.trailing_expr.is_none() && !function.body.stmts.is_empty()
}

fn parses_as_expression(input: &str) -> bool {
    let source = format!("fn main() {{\n    let __hew_eval_probe = {input};\n}}\n");
    let parse_result = hew_parser::parse(&source);
    parse_result.errors.is_empty() && parse_result.program.items.len() == 1
}

fn parses_in_any_context(input: &str) -> bool {
    parses_as_item(input) || parses_as_statement(input) || parses_as_expression(input)
}

fn parses_with_continuation(input: &str) -> bool {
    const CONTINUATION_SUFFIXES: &[&str] = &[
        "\"",
        " __hew_repl_probe__",
        "\n__hew_repl_probe__",
        " 0",
        "\n0",
        " 0;",
        "\n0;",
        " i64;",
        "\ni64;",
        " {\n}\n",
        "\n{\n}\n",
        " i64 {\n}\n",
        "\ni64 {\n}\n",
    ];

    CONTINUATION_SUFFIXES.iter().any(|suffix| {
        let mut candidate = String::with_capacity(input.len() + suffix.len());
        candidate.push_str(input);
        candidate.push_str(suffix);
        parses_in_any_context(&candidate)
    })
}

/// Check whether input has unclosed delimiters (for multi-line input).
#[must_use]
pub fn has_unclosed_delimiters(input: &str) -> bool {
    let mut depth: i32 = 0;
    let mut in_string = false;
    let mut prev = '\0';

    for ch in input.chars() {
        if in_string {
            if ch == '"' && prev != '\\' {
                in_string = false;
            }
        } else {
            match ch {
                '"' => in_string = true,
                '{' | '(' | '[' => depth += 1,
                '}' | ')' | ']' => depth -= 1,
                _ => {}
            }
        }
        prev = ch;
    }

    depth > 0 || in_string
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn classify_items() {
        assert_eq!(classify("fn foo() {}"), InputKind::Item);
        assert_eq!(classify("const LIMIT: i64 = 10;"), InputKind::Item);
        assert_eq!(classify("type UserId = i64;"), InputKind::Item);
        assert_eq!(classify("enum Colour { Red; Green; }"), InputKind::Item);
        assert_eq!(
            classify("actor Counter { receive fn increment() {} }"),
            InputKind::Item
        );
        assert_eq!(
            classify("trait Printable { fn print(val: Self); }"),
            InputKind::Item
        );
        assert_eq!(classify("pub fn bar() {}"), InputKind::Item);
        assert_eq!(classify("/// Adds numbers.\nfn add() {}"), InputKind::Item);
        // `#[export(..)]` (not `#[memo]`, which HEW-SPEC-2026 §12.6's closed
        // attribute table does not recognise) exercises classification of an
        // attribute-decorated item.
        assert_eq!(
            classify("#[export(\"cached\")]\nfn cached() -> i64 { 42 }"),
            InputKind::Item
        );
    }

    #[test]
    fn classify_statements() {
        assert_eq!(classify("let x = 42;"), InputKind::Statement);
        assert_eq!(classify("var y = 10;"), InputKind::Statement);
        assert_eq!(classify("value = value + 1;"), InputKind::Statement);
    }

    #[test]
    fn classify_expressions() {
        assert_eq!(classify("1 + 2"), InputKind::Expression);
        assert_eq!(classify("foo(42)"), InputKind::Expression);
        assert_eq!(classify("x * y + z"), InputKind::Expression);
        assert_eq!(classify("{ let x = 1; x + 2 }"), InputKind::Expression);
    }

    #[test]
    fn classify_commands() {
        assert_eq!(classify(":help"), InputKind::Command(ReplCommand::Help));
        assert_eq!(classify(":quit"), InputKind::Command(ReplCommand::Quit));
        assert_eq!(classify(":q"), InputKind::Command(ReplCommand::Quit));
        assert_eq!(
            classify(":session"),
            InputKind::Command(ReplCommand::Session)
        );
        assert_eq!(classify(":show"), InputKind::Command(ReplCommand::Session));
        assert_eq!(classify(":items"), InputKind::Command(ReplCommand::Items));
        // :bindings is no longer a command; routes through Unknown.
        assert_eq!(
            classify(":bindings"),
            InputKind::Command(ReplCommand::Unknown("bindings".to_string()))
        );
        assert_eq!(classify(":clear"), InputKind::Command(ReplCommand::Clear));
        assert_eq!(classify(":reset"), InputKind::Command(ReplCommand::Clear));
        assert_eq!(
            classify(":type x + 1"),
            InputKind::Command(ReplCommand::Type("x + 1".to_string()))
        );
        assert_eq!(
            classify(":load foo.hew"),
            InputKind::Command(ReplCommand::Load("foo.hew".to_string()))
        );
        assert_eq!(
            classify(":unknown"),
            InputKind::Command(ReplCommand::Unknown("unknown".to_string()))
        );
    }

    #[test]
    fn unclosed_delimiters() {
        assert!(has_unclosed_delimiters("fn foo() {"));
        assert!(has_unclosed_delimiters("let x = (1 +"));
        assert!(has_unclosed_delimiters(r#"let s = "hello"#));
        assert!(!has_unclosed_delimiters("fn foo() {}"));
        assert!(!has_unclosed_delimiters("let x = (1 + 2);"));
        assert!(!has_unclosed_delimiters(r#"let s = "hello {world";"#));
    }

    #[test]
    fn empty_input() {
        assert_eq!(classify(""), InputKind::Expression);
        assert_eq!(classify("   "), InputKind::Expression);
    }

    #[test]
    fn expression_probe_accepts_block_expressions() {
        assert!(parses_as_expression("{ let x = 1; x + 2 }"));
    }

    #[test]
    fn input_completeness_recognizes_complete_inputs() {
        assert_eq!(input_completeness("1 + 2"), InputCompleteness::Complete);
        assert_eq!(
            input_completeness("let answer = 42;"),
            InputCompleteness::Complete
        );
        assert_eq!(input_completeness(":type 1 +"), InputCompleteness::Complete);
    }

    #[test]
    fn input_completeness_recognizes_incomplete_inputs() {
        assert_eq!(input_completeness("1 +"), InputCompleteness::Incomplete);
        assert_eq!(
            input_completeness("let answer ="),
            InputCompleteness::Incomplete
        );
        assert_eq!(input_completeness("if true"), InputCompleteness::Incomplete);
        assert_eq!(
            input_completeness("fn add(a: i64, b: i64) ->"),
            InputCompleteness::Incomplete
        );
        assert_eq!(
            input_completeness(r#""hello"#),
            InputCompleteness::Incomplete
        );
    }

    #[test]
    fn input_completeness_buffers_bare_attribute_awaiting_its_item() {
        // A standalone attribute line has balanced delimiters and parses in
        // no context on its own — it must be buffered for the item that
        // follows, not surfaced as a parse error.
        assert_eq!(input_completeness("#[wire]"), InputCompleteness::Incomplete);
        assert_eq!(
            input_completeness("#[wire]\n#[other]"),
            InputCompleteness::Incomplete
        );
        // Once the decorated item is appended, the buffer is complete.
        assert_eq!(
            input_completeness("#[wire]\ntype UserMessage { name: string @1, }"),
            InputCompleteness::Complete
        );
    }

    #[test]
    fn input_completeness_recognizes_invalid_inputs() {
        // A trailing closing paren with no matching opener cannot be
        // completed by more input.
        assert_eq!(input_completeness("1 + )"), InputCompleteness::Invalid);
        assert_eq!(input_completeness("let = 1;"), InputCompleteness::Invalid);
    }
}
