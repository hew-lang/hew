//! REPL session state — accumulates items and bindings across evaluations.

use std::fmt::Write;
use std::ops::Range;

use hew_parser::ast::{Item, Pattern, Stmt, TypeDeclKind, WireDeclKind};

use super::classify::{self, InputKind};

/// Persistent state for a REPL session.
#[derive(Debug, Clone)]
pub struct Session {
    /// Prior top-level items (structs, fns, actors, enums, etc.).
    items: Vec<SessionItem>,
    /// Prior let/var bindings from the main body.
    bindings: Vec<SessionBinding>,
}

/// Counts of persistent state remembered by a REPL session.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct SessionCounts {
    pub items: usize,
    pub bindings: usize,
}

#[derive(Debug, Clone)]
struct SessionItem {
    source: String,
    summary: String,
}

#[derive(Debug, Clone)]
struct SessionBinding {
    source: String,
    summary: String,
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
        let items = session_items_from_source(source);
        if items.is_empty() {
            self.items.push(SessionItem::fallback(source));
        } else {
            self.items.extend(items);
        }
    }

    /// Record a successfully-evaluated binding (let/var).
    #[cfg_attr(
        not(test),
        allow(
            dead_code,
            reason = "alternate REPL session paths exercise this in tests"
        )
    )]
    pub fn add_binding(&mut self, source: &str) {
        let bindings = persistent_bindings(source);
        if bindings.is_empty() {
            self.bindings.push(SessionBinding::fallback(source));
        } else {
            self.bindings.extend(bindings);
        }
    }

    /// Record any explicit bindings from a successfully-evaluated statement input.
    pub fn add_persistent_bindings_from_statement(&mut self, input: &str) {
        self.bindings.extend(persistent_bindings(input));
    }

    /// Count remembered items and bindings.
    #[must_use]
    pub fn counts(&self) -> SessionCounts {
        SessionCounts {
            items: self.items.len(),
            bindings: self.bindings.len(),
        }
    }

    /// Render a short session-state summary.
    #[must_use]
    pub fn render_overview(&self) -> String {
        let counts = self.counts();
        let mut out = String::new();
        let _ = writeln!(out, "Session state:");
        let _ = writeln!(out, "  {}", count_phrase(counts.items, "remembered item"));
        let _ = writeln!(
            out,
            "  {}",
            count_phrase(counts.bindings, "persistent binding")
        );
        if counts.items == 0 && counts.bindings == 0 {
            out.push_str("  Use let/var bindings or top-level items to build up the session.\n");
        } else {
            out.push_str("  Use :items or :bindings to inspect remembered definitions.\n");
        }
        out
    }

    /// Render the remembered top-level item list.
    #[must_use]
    pub fn render_items(&self) -> String {
        render_entry_list(
            "Remembered items",
            "No remembered items.",
            self.items.iter().map(|item| item.summary.as_str()),
        )
    }

    /// Render the persistent binding list.
    #[must_use]
    pub fn render_bindings(&self) -> String {
        render_entry_list(
            "Persistent bindings",
            "No persistent bindings.",
            self.bindings.iter().map(|binding| binding.summary.as_str()),
        )
    }

    /// Build a complete Hew program from session state plus new input.
    ///
    /// The `input` is classified and placed appropriately:
    /// - Items go before `fn main()`.
    /// - Bindings go inside `main()` before the new input.
    /// - Expressions are wrapped in `println()` for auto-printing.
    #[must_use]
    #[cfg_attr(
        not(test),
        allow(dead_code, reason = "unit tests cover the convenience builder path")
    )]
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
            source.push_str(&item.source);
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
                    source.push_str(&binding.source);
                    source.push('\n');
                }
                source.push_str("}\n");
            }
            InputKind::Statement => {
                source.push_str("fn main() {\n");
                for binding in &self.bindings {
                    source.push_str("    ");
                    source.push_str(&binding.source);
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
                    source.push_str(&binding.source);
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
            source.push_str(&item.source);
            source.push('\n');
        }
        source.push_str("fn main() {\n");
        for binding in &self.bindings {
            source.push_str("    ");
            source.push_str(&binding.source);
            source.push('\n');
        }
        let trimmed = expr.trim().strip_suffix(';').unwrap_or(expr.trim());
        let _ = writeln!(source, "    let __repl_type_query = {trimmed};");
        source.push_str("}\n");
        source
    }
}

impl SessionItem {
    fn fallback(source: &str) -> Self {
        Self {
            source: source.trim().to_string(),
            summary: source_preview(source),
        }
    }
}

impl SessionBinding {
    #[cfg_attr(
        not(test),
        allow(
            dead_code,
            reason = "fallback only applies when parsing cannot classify bindings"
        )
    )]
    fn fallback(source: &str) -> Self {
        Self {
            source: ensure_trailing_semicolon(source),
            summary: fallback_binding_summary(source),
        }
    }
}

fn session_items_from_source(source: &str) -> Vec<SessionItem> {
    let parse_result = hew_parser::parse(source);
    if !parse_result.errors.is_empty() {
        return Vec::new();
    }

    if parse_result.program.items.len() == 1 {
        let (item, _) = &parse_result.program.items[0];
        return vec![SessionItem {
            source: source.trim().to_string(),
            summary: summarize_item(item),
        }];
    }

    parse_result
        .program
        .items
        .iter()
        .map(|(item, span)| SessionItem {
            source: slice_source(source, span),
            summary: summarize_item(item),
        })
        .collect()
}

fn persistent_bindings(input: &str) -> Vec<SessionBinding> {
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
            Stmt::Let { pattern, .. } => {
                let start = span.start.checked_sub(prefix_len)?;
                let end = span.end.checked_sub(prefix_len)?.min(input.len());
                let binding = input.get(start..end)?.trim();
                Some(SessionBinding {
                    source: ensure_trailing_semicolon(binding),
                    summary: summarize_let_binding(&pattern.0, binding),
                })
            }
            Stmt::Var { name, .. } => {
                let start = span.start.checked_sub(prefix_len)?;
                let end = span.end.checked_sub(prefix_len)?.min(input.len());
                let binding = input.get(start..end)?.trim();
                Some(SessionBinding {
                    source: ensure_trailing_semicolon(binding),
                    summary: format!("var {name}"),
                })
            }
            _ => None,
        })
        .collect()
}

fn slice_source(source: &str, span: &Range<usize>) -> String {
    source
        .get(span.clone())
        .unwrap_or(source)
        .trim()
        .to_string()
}

fn summarize_item(item: &Item) -> String {
    match item {
        Item::Import(import) => summarize_import(import),
        Item::Const(decl) => format!("const {}", decl.name),
        Item::TypeDecl(decl) => format!(
            "{} {}",
            match decl.kind {
                TypeDeclKind::Struct => "struct",
                TypeDeclKind::Enum => "enum",
            },
            decl.name
        ),
        Item::TypeAlias(decl) => format!("type {}", decl.name),
        Item::Trait(decl) => format!("trait {}", decl.name),
        Item::Impl(decl) => {
            if decl.trait_bound.is_some() {
                "trait impl block".to_string()
            } else {
                "impl block".to_string()
            }
        }
        Item::Wire(decl) => format!(
            "wire {} {}",
            match decl.kind {
                WireDeclKind::Struct => "struct",
                WireDeclKind::Enum => "enum",
            },
            decl.name
        ),
        Item::Function(decl) => summarize_function(decl),
        Item::ExternBlock(block) => format!("extern \"{}\" block", block.abi),
        Item::Actor(decl) => format!("actor {}", decl.name),
        Item::Supervisor(decl) => format!("supervisor {}", decl.name),
        Item::Machine(decl) => format!("machine {}", decl.name),
    }
}

fn summarize_import(import: &hew_parser::ast::ImportDecl) -> String {
    if let Some(path) = &import.file_path {
        return format!("import {path}");
    }
    if import.path.is_empty() {
        "import <module>".to_string()
    } else {
        format!("import {}", import.path.join("::"))
    }
}

fn summarize_function(function: &hew_parser::ast::FnDecl) -> String {
    let mut summary = String::new();
    if function.is_async {
        summary.push_str("async ");
    }
    if function.is_generator {
        summary.push_str("gen ");
    }
    summary.push_str("fn ");
    summary.push_str(&function.name);
    summary
}

fn summarize_let_binding(pattern: &Pattern, source: &str) -> String {
    let mut names = Vec::new();
    collect_pattern_identifiers(pattern, &mut names);
    if names.is_empty() {
        format!(
            "let {}",
            binding_head(source, "let").unwrap_or_else(|| "<pattern>".to_string())
        )
    } else {
        format!("let {}", names.join(", "))
    }
}

fn collect_pattern_identifiers(pattern: &Pattern, names: &mut Vec<String>) {
    match pattern {
        Pattern::Wildcard | Pattern::Literal(_) => {}
        Pattern::Identifier(name) => push_unique(names, name),
        Pattern::Constructor { patterns, .. } | Pattern::Tuple(patterns) => {
            for (pattern, _) in patterns {
                collect_pattern_identifiers(pattern, names);
            }
        }
        Pattern::Struct { fields, .. } => {
            for field in fields {
                if let Some((pattern, _)) = &field.pattern {
                    collect_pattern_identifiers(pattern, names);
                } else {
                    push_unique(names, &field.name);
                }
            }
        }
        Pattern::Or(left, right) => {
            collect_pattern_identifiers(&left.0, names);
            collect_pattern_identifiers(&right.0, names);
        }
    }
}

fn push_unique(names: &mut Vec<String>, name: &str) {
    if !names.iter().any(|existing| existing == name) {
        names.push(name.to_string());
    }
}

fn render_entry_list<'a, I>(title: &str, empty_message: &str, entries: I) -> String
where
    I: IntoIterator<Item = &'a str>,
{
    let entries: Vec<&str> = entries.into_iter().collect();
    if entries.is_empty() {
        return format!("{empty_message}\n");
    }

    let mut out = String::new();
    let _ = writeln!(out, "{title} ({}):", entries.len());
    for entry in entries {
        let _ = writeln!(out, "  - {entry}");
    }
    out
}

fn count_phrase(count: usize, singular: &str) -> String {
    if count == 1 {
        format!("1 {singular}")
    } else {
        format!("{count} {singular}s")
    }
}

fn ensure_trailing_semicolon(source: &str) -> String {
    let trimmed = source.trim();
    if trimmed.ends_with(';') {
        trimmed.to_string()
    } else {
        format!("{trimmed};")
    }
}

#[cfg_attr(
    not(test),
    allow(
        dead_code,
        reason = "fallback summaries are only consumed in test-only paths"
    )
)]
fn fallback_binding_summary(source: &str) -> String {
    let trimmed = source.trim();
    if trimmed.starts_with("let ") {
        format!(
            "let {}",
            binding_head(trimmed, "let").unwrap_or_else(|| "<pattern>".to_string())
        )
    } else if trimmed.starts_with("var ") {
        format!(
            "var {}",
            binding_head(trimmed, "var").unwrap_or_else(|| "<binding>".to_string())
        )
    } else {
        source_preview(source)
    }
}

fn binding_head(source: &str, keyword: &str) -> Option<String> {
    let trimmed = source.trim();
    let rest = trimmed.strip_prefix(keyword)?.trim_start();
    let head = rest
        .split_once('=')
        .map_or(rest, |(before, _)| before)
        .trim()
        .trim_end_matches(';')
        .trim();
    if head.is_empty() {
        None
    } else {
        Some(head.to_string())
    }
}

fn source_preview(source: &str) -> String {
    let mut lines = source
        .lines()
        .map(str::trim)
        .filter(|line| !line.is_empty());
    let Some(first) = lines.next() else {
        return "<input>".to_string();
    };
    if lines.next().is_some() {
        format!("{first} …")
    } else {
        first.to_string()
    }
}

/// A synthetic program assembled from session state and new input.
#[derive(Debug)]
pub struct SyntheticProgram {
    /// The full Hew source code.
    pub source: String,
    /// What kind of input produced this program.
    #[cfg_attr(
        not(test),
        allow(
            dead_code,
            reason = "kind is inspected by tests and debug-only REPL flows"
        )
    )]
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
        session.add_item("fn helper() -> i64 { 42 }");
        let prog = session.build_program("helper()");
        assert!(prog.source.contains("fn helper() -> i64 { 42 }"));
        assert!(prog.source.contains("println(helper())"));
        assert_eq!(session.counts().items, 1);
    }

    #[test]
    fn session_accumulates_bindings() {
        let mut session = Session::new();
        session.add_binding("let x = 10;");
        let prog = session.build_program("x + 5");
        assert!(prog.source.contains("let x = 10;"));
        assert!(prog.source.contains("println(x + 5)"));
        assert_eq!(session.counts().bindings, 1);
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

    #[test]
    fn render_overview_reports_empty_session() {
        let session = Session::new();
        assert_eq!(
            session.render_overview(),
            "Session state:\n  0 remembered items\n  0 persistent bindings\n  Use let/var bindings or top-level items to build up the session.\n"
        );
    }

    #[test]
    fn render_items_lists_structured_item_summaries() {
        let mut session = Session::new();
        session.add_item("fn compute() -> i64 { 42 }");
        session.add_item("const LIMIT: i64 = 10;");

        let output = session.render_items();
        assert!(output.contains("Remembered items (2):"), "output: {output}");
        assert!(output.contains("fn compute"), "output: {output}");
        assert!(output.contains("const LIMIT"), "output: {output}");
    }

    #[test]
    fn render_bindings_lists_destructured_names() {
        let mut session = Session::new();
        session.add_binding("let (left, right) = pair();\nvar total = 0;");

        let output = session.render_bindings();
        assert!(
            output.contains("Persistent bindings (2):"),
            "output: {output}"
        );
        assert!(output.contains("let left, right"), "output: {output}");
        assert!(output.contains("var total"), "output: {output}");
    }
}
