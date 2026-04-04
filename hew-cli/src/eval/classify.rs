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

/// A REPL meta-command.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ReplCommand {
    /// `:help` — show available commands.
    Help,
    /// `:quit` or `:q` — exit the REPL.
    Quit,
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

/// Parse a REPL command string (after the leading `:`).
fn parse_command(cmd: &str) -> ReplCommand {
    let parts: Vec<&str> = cmd.trim().splitn(2, char::is_whitespace).collect();
    let name = parts[0];
    let arg = parts.get(1).map(|s| s.trim().to_string());

    match name {
        "help" | "h" => ReplCommand::Help,
        "quit" | "q" | "exit" => ReplCommand::Quit,
        "clear" => ReplCommand::Clear,
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

#[cfg(test)]
fn parses_as_expression(input: &str) -> bool {
    let source = format!("fn main() {{\n    let __hew_eval_probe = {input};\n}}\n");
    let parse_result = hew_parser::parse(&source);
    parse_result.errors.is_empty() && parse_result.program.items.len() == 1
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

    depth > 0
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
        assert_eq!(classify(":clear"), InputKind::Command(ReplCommand::Clear));
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
}
