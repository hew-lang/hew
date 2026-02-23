//! REPL session state — accumulates items and bindings across evaluations.

use std::fmt::Write;

use super::classify::{self, InputKind};

/// Persistent state for a REPL session.
#[derive(Debug, Clone)]
pub struct Session {
    /// Prior top-level items (structs, fns, actors, enums, etc.).
    items: Vec<String>,
    /// Prior let/var bindings from the main body.
    bindings: Vec<String>,
}

impl Default for Session {
    fn default() -> Self {
        Self::new()
    }
}

impl Session {
    /// Create a new empty session.
    #[must_use]
    pub fn new() -> Self {
        Self {
            items: Vec::new(),
            bindings: Vec::new(),
        }
    }

    /// Reset all accumulated state.
    pub fn clear(&mut self) {
        self.items.clear();
        self.bindings.clear();
    }

    /// Record a successfully-evaluated item.
    pub fn add_item(&mut self, source: &str) {
        self.items.push(source.to_string());
    }

    /// Record a successfully-evaluated binding (let/var).
    pub fn add_binding(&mut self, source: &str) {
        self.bindings.push(source.to_string());
    }

    /// Build a complete Hew program from session state plus new input.
    ///
    /// The `input` is classified and placed appropriately:
    /// - Items go before `fn main()`.
    /// - Bindings go inside `main()` before the new input.
    /// - Expressions are wrapped in `println()` for auto-printing.
    #[must_use]
    pub fn build_program(&self, input: &str) -> SyntheticProgram {
        let kind = classify::classify(input);
        let mut source = String::new();

        // Emit prior items.
        for item in &self.items {
            source.push_str(item);
            source.push('\n');
        }

        match &kind {
            InputKind::Item => {
                // The new item goes at the top level; we still need a main.
                source.push_str(input);
                source.push('\n');
                source.push_str("fn main() {\n");
                for binding in &self.bindings {
                    source.push_str("    ");
                    source.push_str(binding);
                    source.push('\n');
                }
                source.push_str("}\n");
            }
            InputKind::Statement => {
                source.push_str("fn main() {\n");
                for binding in &self.bindings {
                    source.push_str("    ");
                    source.push_str(binding);
                    source.push('\n');
                }
                source.push_str("    ");
                let trimmed = input.trim();
                source.push_str(trimmed);
                if !trimmed.ends_with(';') {
                    source.push(';');
                }
                source.push_str("\n}\n");
            }
            InputKind::Expression => {
                source.push_str("fn main() {\n");
                for binding in &self.bindings {
                    source.push_str("    ");
                    source.push_str(binding);
                    source.push('\n');
                }
                let trimmed = input.trim();
                let trimmed = trimmed.strip_suffix(';').unwrap_or(trimmed);
                // Wrap the expression for auto-printing via the
                // type-generic `println()` builtin.
                let _ = write!(source, "    println({trimmed});\n}}");
            }
            InputKind::Command(_) => {
                // Commands are handled before we get here; return a no-op program.
                source.push_str("fn main() {}\n");
            }
        }

        SyntheticProgram { source, kind }
    }

    /// Build a program that just type-checks an expression and returns its type.
    #[must_use]
    pub fn build_type_query(&self, expr: &str) -> String {
        let mut source = String::new();
        for item in &self.items {
            source.push_str(item);
            source.push('\n');
        }
        source.push_str("fn main() {\n");
        for binding in &self.bindings {
            source.push_str("    ");
            source.push_str(binding);
            source.push('\n');
        }
        let trimmed = expr.trim().strip_suffix(';').unwrap_or(expr.trim());
        let _ = writeln!(source, "    let __repl_type_query = {trimmed};");
        source.push_str("}\n");
        source
    }
}

/// A synthetic program assembled from session state and new input.
#[derive(Debug)]
pub struct SyntheticProgram {
    /// The full Hew source code.
    pub source: String,
    /// What kind of input produced this program.
    pub kind: InputKind,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty_session_expression() {
        let session = Session::new();
        let prog = session.build_program("1 + 2");
        assert!(prog.source.contains("println(1 + 2)"));
        assert!(prog.source.contains("fn main()"));
    }

    #[test]
    fn session_accumulates_items() {
        let mut session = Session::new();
        session.add_item("fn helper() -> i32 { 42 }");
        let prog = session.build_program("helper()");
        assert!(prog.source.contains("fn helper() -> i32 { 42 }"));
        assert!(prog.source.contains("println(helper())"));
    }

    #[test]
    fn session_accumulates_bindings() {
        let mut session = Session::new();
        session.add_binding("let x = 10;");
        let prog = session.build_program("x + 5");
        assert!(prog.source.contains("let x = 10;"));
        assert!(prog.source.contains("println(x + 5)"));
    }

    #[test]
    fn item_input() {
        let session = Session::new();
        let prog = session.build_program("fn foo() -> i32 { 42 }");
        assert_eq!(prog.kind, InputKind::Item);
        assert!(prog.source.contains("fn foo() -> i32 { 42 }"));
        assert!(prog.source.contains("fn main()"));
    }

    #[test]
    fn statement_input() {
        let session = Session::new();
        let prog = session.build_program("let x = 42;");
        assert_eq!(prog.kind, InputKind::Statement);
        assert!(prog.source.contains("let x = 42;"));
    }

    #[test]
    fn clear_resets() {
        let mut session = Session::new();
        session.add_item("fn foo() {}");
        session.add_binding("let x = 1;");
        session.clear();
        let prog = session.build_program("42");
        assert!(!prog.source.contains("fn foo()"));
        assert!(!prog.source.contains("let x = 1"));
    }

    #[test]
    fn type_query() {
        let mut session = Session::new();
        session.add_binding("let x = 10;");
        let source = session.build_type_query("x + 5");
        assert!(source.contains("let __repl_type_query = x + 5;"));
        assert!(source.contains("let x = 10;"));
    }
}
