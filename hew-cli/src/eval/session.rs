//! REPL session state — accumulates items and bindings across evaluations.

use std::fmt::Write;
use std::ops::Range;

use hew_parser::ast::{Item, Stmt};

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

    /// Record any explicit bindings from a successfully-evaluated statement input.
    pub fn add_persistent_bindings_from_statement(&mut self, input: &str) {
        for binding in persistent_binding_sources(input) {
            self.add_binding(&binding);
        }
    }

    /// Build a complete Hew program from session state plus new input.
    ///
    /// The `input` is classified and placed appropriately:
    /// - Items go before `fn main()`.
    /// - Bindings go inside `main()` before the new input.
    /// - Expressions are wrapped in `println()` for auto-printing.
    #[must_use]
    #[cfg_attr(not(test), allow(dead_code))]
    pub fn build_program(&self, input: &str) -> SyntheticProgram {
        let kind = classify::classify(input);
        self.build_program_with_kind(input, kind)
    }

    /// Build a complete Hew program from session state plus a known input kind.
    #[must_use]
    pub fn build_program_with_kind(&self, input: &str, kind: InputKind) -> SyntheticProgram {
        let mut source = String::new();
        let mut diagnostic_view = None;

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
                let input_start = source.len();
                source.push_str(trimmed);
                let input_end = source.len();
                if !trimmed.ends_with(';') {
                    source.push(';');
                }
                source.push_str("\n}\n");
                diagnostic_view = Some(SyntheticDiagnosticView {
                    source: trimmed.to_string(),
                    input_span: input_start..input_end,
                });
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
                source.push_str("    println(");
                let input_start = source.len();
                source.push_str(trimmed);
                let input_end = source.len();
                source.push_str(");\n}\n");
                diagnostic_view = Some(SyntheticDiagnosticView {
                    source: trimmed.to_string(),
                    input_span: input_start..input_end,
                });
            }
            InputKind::Command(_) => {
                // Commands are handled before we get here; return a no-op program.
                source.push_str("fn main() {}\n");
            }
        }

        SyntheticProgram {
            source,
            kind,
            diagnostic_view,
        }
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

fn persistent_binding_sources(input: &str) -> Vec<String> {
    const MAIN_PREFIX: &str = "fn main() {\n";

    let source = format!("{MAIN_PREFIX}{input}\n}}\n");
    let parse_result = hew_parser::parse(&source);
    if !parse_result.errors.is_empty() || parse_result.program.items.len() != 1 {
        debug_assert!(
            false,
            "statement persistence expected parseable statement input: {input:?}"
        );
        return Vec::new();
    }

    let Some((Item::Function(function), _)) = parse_result.program.items.first() else {
        debug_assert!(false, "statement persistence expected synthetic main");
        return Vec::new();
    };

    let prefix_len = MAIN_PREFIX.len();
    function
        .body
        .stmts
        .iter()
        .filter_map(|(stmt, span)| match stmt {
            Stmt::Let { .. } | Stmt::Var { .. } => {
                let start = span.start.checked_sub(prefix_len)?;
                let end = span.end.checked_sub(prefix_len)?.min(input.len());
                let binding = input.get(start..end)?.trim();
                Some(if binding.ends_with(';') {
                    binding.to_string()
                } else {
                    format!("{binding};")
                })
            }
            _ => None,
        })
        .collect()
}

/// A synthetic program assembled from session state and new input.
#[derive(Debug)]
pub struct SyntheticProgram {
    /// The full Hew source code.
    pub source: String,
    /// What kind of input produced this program.
    #[cfg_attr(not(test), allow(dead_code))]
    pub kind: InputKind,
    /// Optional user-facing view for remapping diagnostics away from wrappers.
    pub diagnostic_view: Option<SyntheticDiagnosticView>,
}

/// Mapping metadata for wrapper-stripped diagnostics.
#[derive(Debug, Clone)]
pub struct SyntheticDiagnosticView {
    /// The user-facing input source that should appear in diagnostics.
    pub source: String,
    /// Byte range of the user input within the synthetic program source.
    pub input_span: Range<usize>,
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
    fn statement_replay_keeps_only_explicit_bindings() {
        let mut session = Session::new();
        session.add_persistent_bindings_from_statement("let x = 10;\nvar y = x + 1;");
        let prog = session.build_program("x + y");
        assert!(
            prog.source.contains("let x = 10;"),
            "source: {}",
            prog.source
        );
        assert!(
            prog.source.contains("var y = x + 1;"),
            "source: {}",
            prog.source
        );
    }

    #[test]
    fn statement_replay_ignores_one_shot_statement() {
        let mut session = Session::new();
        session.add_persistent_bindings_from_statement("println(\"once\");");
        let prog = session.build_program("1 + 1");
        assert!(
            !prog.source.contains("println(\"once\")"),
            "source: {}",
            prog.source
        );
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
    fn doc_commented_item_input() {
        let session = Session::new();
        let prog = session.build_program("/// Docs\nfn foo() -> i32 { 42 }");
        assert_eq!(prog.kind, InputKind::Item);
        assert!(prog.source.contains("/// Docs\nfn foo() -> i32 { 42 }"));
    }

    #[test]
    fn statement_input() {
        let session = Session::new();
        let prog = session.build_program("let x = 42;");
        assert_eq!(prog.kind, InputKind::Statement);
        assert!(prog.source.contains("let x = 42;"));
    }

    #[test]
    fn assignment_input_is_statement() {
        let session = Session::new();
        let prog = session.build_program("value = value + 1;");
        assert_eq!(prog.kind, InputKind::Statement);
        assert!(prog.source.contains("value = value + 1;"));
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
