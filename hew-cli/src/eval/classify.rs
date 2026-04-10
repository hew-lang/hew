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
    /// `:bindings` — list persistent bindings.
    Bindings,
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

    if has_unclosed_delimiters(trimmed) || parses_with_continuation(trimmed) {
        return InputCompleteness::Incomplete;
    }

    InputCompleteness::Invalid
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
        "bindings" => ReplCommand::Bindings,
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
        assert_eq!(
            classify("#[memo]\nfn cached() -> i64 { 42 }"),
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
        assert_eq!(
            classify(":bindings"),
            InputKind::Command(ReplCommand::Bindings)
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
    fn input_completeness_recognizes_invalid_inputs() {
        assert_eq!(input_completeness("1 + *"), InputCompleteness::Invalid);
        assert_eq!(input_completeness("let = 1;"), InputCompleteness::Invalid);
    }
}
