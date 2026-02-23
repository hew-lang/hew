//! Input classification for the REPL.
//!
//! Determines whether user input is a top-level item, a statement,
//! a REPL command, or a bare expression.

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

    let first_word = trimmed.split_whitespace().next().unwrap_or("");
    match first_word {
        "fn" | "struct" | "enum" | "actor" | "trait" | "impl" | "pub" | "supervisor" | "const"
        | "wire" | "type" | "import" | "extern" => InputKind::Item,
        "let" | "var" | "if" | "for" | "while" | "loop" | "return" | "match" => {
            InputKind::Statement
        }
        _ => InputKind::Expression,
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
        assert_eq!(classify("struct Point { x: i32; }"), InputKind::Item);
        assert_eq!(classify("enum Color { Red, Green }"), InputKind::Item);
        assert_eq!(classify("actor Counter { }"), InputKind::Item);
        assert_eq!(classify("trait Printable { }"), InputKind::Item);
        assert_eq!(classify("impl Printable for Point { }"), InputKind::Item);
        assert_eq!(classify("pub fn bar() {}"), InputKind::Item);
    }

    #[test]
    fn classify_statements() {
        assert_eq!(classify("let x = 42;"), InputKind::Statement);
        assert_eq!(classify("var y = 10;"), InputKind::Statement);
    }

    #[test]
    fn classify_expressions() {
        assert_eq!(classify("1 + 2"), InputKind::Expression);
        assert_eq!(classify("foo(42)"), InputKind::Expression);
        assert_eq!(classify("x * y + z"), InputKind::Expression);
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
}
