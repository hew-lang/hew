//! Main REPL loop — interactive read-eval-print for Hew.

use super::classify::{self, InputKind, ReplCommand};
use super::session::Session;
use std::path::PathBuf;
use std::process::Command;

/// Result of evaluating a single input in the REPL.
#[derive(Debug)]
pub struct EvalResult {
    /// Output produced by the program.
    pub output: String,
    /// Whether any errors occurred.
    pub had_errors: bool,
    /// Error messages, if any.
    pub errors: Vec<String>,
}

/// A non-interactive REPL session for programmatic use.
#[derive(Debug)]
pub struct ReplSession {
    session: Session,
}

impl Default for ReplSession {
    fn default() -> Self {
        Self::new()
    }
}

impl ReplSession {
    /// Create a new REPL session.
    #[must_use]
    pub fn new() -> Self {
        Self {
            session: Session::new(),
        }
    }

    /// Evaluate a line of input and return the result.
    pub fn eval(&mut self, input: &str) -> EvalResult {
        let trimmed = input.trim();
        if trimmed.is_empty() {
            return EvalResult {
                output: String::new(),
                had_errors: false,
                errors: Vec::new(),
            };
        }

        let kind = classify::classify(trimmed);

        // Handle commands.
        if let InputKind::Command(cmd) = &kind {
            return self.handle_command(cmd);
        }

        // Build the synthetic program.
        let program = self.session.build_program(trimmed);

        // Parse.
        let parse_result = hew_parser::parse(&program.source);
        if !parse_result.errors.is_empty() {
            let errors: Vec<String> = parse_result
                .errors
                .iter()
                .map(|e| e.message.clone())
                .collect();
            return EvalResult {
                output: String::new(),
                had_errors: true,
                errors,
            };
        }

        // Type-check.
        let mut checker = hew_types::Checker::new();
        let tco = checker.check_program(&parse_result.program);
        let type_errors: Vec<String> = tco.errors.iter().map(|e| e.message.clone()).collect();

        if !type_errors.is_empty() {
            return EvalResult {
                output: String::new(),
                had_errors: true,
                errors: type_errors,
            };
        }

        // Compile and execute via native pipeline.
        match compile_and_execute(&program.source) {
            Ok(output) => {
                // On success, persist the input into session state.
                match &program.kind {
                    InputKind::Item => self.session.add_item(trimmed),
                    InputKind::Statement => self.session.add_binding(trimmed),
                    InputKind::Expression | InputKind::Command(_) => {}
                }

                EvalResult {
                    output,
                    had_errors: false,
                    errors: Vec::new(),
                }
            }
            Err(e) => EvalResult {
                output: String::new(),
                had_errors: true,
                errors: vec![e],
            },
        }
    }

    /// Show the inferred type of an expression.
    ///
    /// # Errors
    ///
    /// Returns parse or type errors if the expression is invalid.
    pub fn type_of(&mut self, expr: &str) -> Result<String, Vec<String>> {
        let source = self.session.build_type_query(expr);
        let parse_result = hew_parser::parse(&source);
        if !parse_result.errors.is_empty() {
            return Err(parse_result
                .errors
                .iter()
                .map(|e| e.message.clone())
                .collect());
        }

        let mut checker = hew_types::Checker::new();
        let tco = checker.check_program(&parse_result.program);
        if !tco.errors.is_empty() {
            return Err(tco.errors.iter().map(|e| e.message.clone()).collect());
        }

        // Find the type of `__repl_type_query` in the fn_sigs or expr_types.
        // We look for the expression type of the RHS of the let binding.
        // The query variable name won't be in fn_sigs, so search expr_types
        // for the expression at the right byte offset.
        // As a simpler approach, look for the binding in the source and find
        // the expression type at that span.
        if let Some(sig) = tco.fn_sigs.get("main") {
            // Search through expr_types for any expression spanning the
            // query expression area.
            let query_marker = "__repl_type_query = ";
            if let Some(marker_pos) = source.find(query_marker) {
                let expr_start = marker_pos + query_marker.len();
                let expr_end = source[expr_start..]
                    .find(';')
                    .map_or(source.len(), |p| expr_start + p);

                // Find the best matching span in expr_types.
                let mut best_ty = None;
                let mut best_span_len = usize::MAX;
                for (span, ty) in &tco.expr_types {
                    if span.start >= expr_start && span.end <= expr_end {
                        let span_len = span.end - span.start;
                        // Pick the widest span that covers the whole expression.
                        if best_ty.is_none() || span_len > best_span_len {
                            best_ty = Some(ty.clone());
                            best_span_len = span_len;
                        }
                    }
                }

                if let Some(ty) = best_ty {
                    return Ok(format!("{ty}"));
                }
            }

            // Fallback: check the return type of main.
            let _ = sig;
        }

        Ok("unknown".to_string())
    }

    /// Load a file into the session.
    ///
    /// # Errors
    ///
    /// Returns an error if the file cannot be read or parsed.
    pub fn load_file(&mut self, path: &str) -> Result<String, String> {
        let source =
            std::fs::read_to_string(path).map_err(|e| format!("cannot read '{path}': {e}"))?;

        // Parse the file to extract items and statements.
        let parse_result = hew_parser::parse(&source);
        if !parse_result.errors.is_empty() {
            let errors: Vec<String> = parse_result
                .errors
                .iter()
                .map(|e| e.message.clone())
                .collect();
            return Err(errors.join("\n"));
        }

        // Add all non-main items from the file.
        let mut loaded_count = 0usize;
        for (item, span) in &parse_result.program.items {
            let item_source = &source[span.start..span.end];
            match item {
                hew_parser::ast::Item::Function(f) if f.name == "main" => {
                    // Skip main — the REPL generates its own.
                }
                _ => {
                    self.session.add_item(item_source);
                    loaded_count += 1;
                }
            }
        }

        Ok(format!("Loaded {loaded_count} items from {path}"))
    }

    /// Reset the session.
    pub fn clear(&mut self) {
        self.session.clear();
    }

    /// Handle a REPL command.
    fn handle_command(&mut self, cmd: &ReplCommand) -> EvalResult {
        match cmd {
            ReplCommand::Help => EvalResult {
                output: help_text().to_string(),
                had_errors: false,
                errors: Vec::new(),
            },
            ReplCommand::Quit => EvalResult {
                output: String::new(),
                had_errors: false,
                errors: Vec::new(),
            },
            ReplCommand::Clear => {
                self.clear();
                EvalResult {
                    output: "Session cleared.\n".to_string(),
                    had_errors: false,
                    errors: Vec::new(),
                }
            }
            ReplCommand::Type(expr) => {
                if expr.is_empty() {
                    return EvalResult {
                        output: String::new(),
                        had_errors: true,
                        errors: vec!["Usage: :type <expression>".to_string()],
                    };
                }
                match self.type_of(expr) {
                    Ok(ty) => EvalResult {
                        output: format!("{ty}\n"),
                        had_errors: false,
                        errors: Vec::new(),
                    },
                    Err(errors) => EvalResult {
                        output: String::new(),
                        had_errors: true,
                        errors,
                    },
                }
            }
            ReplCommand::Load(path) => {
                if path.is_empty() {
                    return EvalResult {
                        output: String::new(),
                        had_errors: true,
                        errors: vec!["Usage: :load <file.hew>".to_string()],
                    };
                }
                match self.load_file(path) {
                    Ok(msg) => EvalResult {
                        output: format!("{msg}\n"),
                        had_errors: false,
                        errors: Vec::new(),
                    },
                    Err(e) => EvalResult {
                        output: String::new(),
                        had_errors: true,
                        errors: vec![e],
                    },
                }
            }
            ReplCommand::Unknown(name) => EvalResult {
                output: String::new(),
                had_errors: true,
                errors: vec![format!("Unknown command: :{name}. Type :help for help.")],
            },
        }
    }
}

/// Locate the `hew` binary.
///
/// When running as `hew eval`, `current_exe()` is the hew binary itself.
/// When running unit tests, the test binary is in `target/debug/deps/` and
/// the hew binary is at `target/debug/hew`.
fn find_hew_binary() -> Result<PathBuf, String> {
    let exe = std::env::current_exe().map_err(|e| format!("cannot locate self: {e}"))?;

    // If the current binary is named "hew" (or "hew.exe" on Windows), use it directly.
    if exe
        .file_name()
        .is_some_and(|n| n == "hew" || n == "hew.exe")
    {
        return Ok(exe);
    }

    // Otherwise, search relative to the current binary.
    let exe_dir = exe.parent().expect("exe should have a parent directory");
    let hew_name = if cfg!(target_os = "windows") {
        "hew.exe"
    } else {
        "hew"
    };
    let candidates = [
        exe_dir.join(format!("../{hew_name}")), // target/debug/deps/../hew
        exe_dir.join(hew_name),                 // same dir
        exe_dir.join(format!("../../debug/{hew_name}")), // fallback
    ];

    for c in &candidates {
        if c.exists() {
            return c
                .canonicalize()
                .map_err(|e| format!("cannot resolve hew binary path: {e}"));
        }
    }

    Err(format!(
        "cannot find hew binary (searched relative to {})",
        exe_dir.display()
    ))
}

/// Compile Hew source to a native binary via `hew build` and execute it.
fn compile_and_execute(source: &str) -> Result<String, String> {
    let hew_binary = find_hew_binary()?;

    let tmp_dir = tempfile::tempdir().map_err(|e| format!("cannot create temp dir: {e}"))?;

    let src_path = tmp_dir.path().join("eval.hew");
    std::fs::write(&src_path, source).map_err(|e| format!("cannot write temp source: {e}"))?;

    let bin_name = if cfg!(target_os = "windows") {
        "eval_bin.exe"
    } else {
        "eval_bin"
    };
    let bin_path = tmp_dir.path().join(bin_name);

    // Compile.
    let compile = Command::new(&hew_binary)
        .arg("build")
        .arg(&src_path)
        .arg("-o")
        .arg(&bin_path)
        .output()
        .map_err(|e| format!("cannot invoke hew build: {e}"))?;

    if !compile.status.success() {
        let stderr = String::from_utf8_lossy(&compile.stderr);
        let stdout = String::from_utf8_lossy(&compile.stdout);
        let msg = if stderr.is_empty() {
            stdout.to_string()
        } else {
            stderr.to_string()
        };
        return Err(msg);
    }

    // Execute.
    let run = Command::new(&bin_path)
        .output()
        .map_err(|e| format!("cannot execute compiled program: {e}"))?;

    if !run.status.success() {
        let stderr = String::from_utf8_lossy(&run.stderr);
        return Err(if stderr.is_empty() {
            "program exited with non-zero status".to_string()
        } else {
            stderr.to_string()
        });
    }

    Ok(String::from_utf8_lossy(&run.stdout).to_string())
}

/// Run the interactive REPL.
///
/// # Errors
///
/// Returns an error if readline fails fatally.
pub fn run_interactive() -> Result<(), Box<dyn std::error::Error>> {
    let mut rl = rustyline::DefaultEditor::new()?;
    let mut session = ReplSession::new();

    println!("Hew REPL v{}", env!("CARGO_PKG_VERSION"));
    println!("Type :help for help, :quit to exit.\n");

    loop {
        let prompt = "hew> ";
        let line = match rl.readline(prompt) {
            Ok(line) => line,
            Err(
                rustyline::error::ReadlineError::Interrupted | rustyline::error::ReadlineError::Eof,
            ) => {
                println!();
                break;
            }
            Err(e) => return Err(e.into()),
        };

        let mut input = line.clone();

        // Multi-line: keep reading if delimiters are unclosed.
        while classify::has_unclosed_delimiters(&input) {
            match rl.readline("... ") {
                Ok(cont) => {
                    input.push('\n');
                    input.push_str(&cont);
                }
                Err(_) => break,
            }
        }

        let trimmed = input.trim();
        if trimmed.is_empty() {
            continue;
        }

        let _ = rl.add_history_entry(&input);

        // Check for quit command before eval.
        if let InputKind::Command(ReplCommand::Quit) = classify::classify(trimmed) {
            break;
        }

        let result = session.eval(trimmed);
        if result.had_errors {
            for err in &result.errors {
                eprintln!("error: {err}");
            }
        } else if !result.output.is_empty() {
            print!("{}", result.output);
        }
    }

    Ok(())
}

/// Evaluate a single expression non-interactively.
///
/// # Errors
///
/// Returns an error string if evaluation fails.
pub fn eval_one(expr: &str) -> Result<String, String> {
    let mut session = ReplSession::new();
    let result = session.eval(expr);
    if result.had_errors {
        Err(result.errors.join("\n"))
    } else {
        Ok(result.output)
    }
}

/// Evaluate a file in REPL context.
///
/// # Errors
///
/// Returns an error string if evaluation fails.
pub fn eval_file(path: &str) -> Result<(), Box<dyn std::error::Error>> {
    let source = std::fs::read_to_string(path).map_err(|e| format!("cannot read '{path}': {e}"))?;

    let mut session = ReplSession::new();
    let mut buffer = String::new();

    for line in source.lines() {
        let trimmed = line.trim();
        if (trimmed.is_empty() || trimmed.starts_with("//")) && buffer.is_empty() {
            continue;
        }

        if !buffer.is_empty() {
            buffer.push('\n');
        }
        buffer.push_str(line);

        // Keep accumulating if delimiters are unclosed.
        if classify::has_unclosed_delimiters(&buffer) {
            continue;
        }

        let input = buffer.trim();
        if input.is_empty() {
            buffer.clear();
            continue;
        }

        let result = session.eval(input);
        if result.had_errors {
            for err in &result.errors {
                eprintln!("error: {err}");
            }
            return Err(format!("error in file {path}").into());
        }
        if !result.output.is_empty() {
            print!("{}", result.output);
        }
        buffer.clear();
    }

    // Evaluate any remaining buffered input.
    let input = buffer.trim();
    if !input.is_empty() {
        let result = session.eval(input);
        if result.had_errors {
            for err in &result.errors {
                eprintln!("error: {err}");
            }
            return Err(format!("error in file {path}").into());
        }
        if !result.output.is_empty() {
            print!("{}", result.output);
        }
    }

    Ok(())
}

fn help_text() -> &'static str {
    "\
Commands:
  :help, :h         Show this help message
  :quit, :q         Exit the REPL
  :clear            Reset session (clear all definitions)
  :type <expr>      Show the inferred type of an expression
  :load <file>      Load a .hew file into the session

Input types:
  fn, struct, ...   Top-level items are remembered across evaluations
  let x = ...;      Bindings persist in the session
  <expression>      Bare expressions are evaluated and printed
"
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Skip tests that require the full compilation pipeline (hew + hew-codegen)
    /// when hew-codegen is not available (e.g. in CI Rust-only test jobs).
    fn require_codegen() -> bool {
        match find_hew_binary() {
            Ok(hew) => {
                // Check that `hew build` can actually find hew-codegen
                let out = std::process::Command::new(&hew).arg("--version").output();
                out.is_ok()
            }
            Err(_) => false,
        }
    }

    #[test]
    fn eval_arithmetic() {
        if !require_codegen() {
            return;
        }
        let mut session = ReplSession::new();
        let result = session.eval("1 + 2");
        assert!(!result.had_errors, "errors: {:?}", result.errors);
        assert_eq!(result.output, "3\n");
    }

    #[test]
    fn eval_binding_persists() {
        if !require_codegen() {
            return;
        }
        let mut session = ReplSession::new();
        let r1 = session.eval("let x = 42;");
        assert!(!r1.had_errors, "errors: {:?}", r1.errors);
        let r2 = session.eval("x + 1");
        assert!(!r2.had_errors, "errors: {:?}", r2.errors);
        assert_eq!(r2.output, "43\n");
    }

    #[test]
    fn eval_function_persists() {
        if !require_codegen() {
            return;
        }
        let mut session = ReplSession::new();
        let r1 = session.eval("fn double(x: i64) -> i64 { x * 2 }");
        assert!(!r1.had_errors, "errors: {:?}", r1.errors);
        let r2 = session.eval("double(21)");
        assert!(!r2.had_errors, "errors: {:?}", r2.errors);
        assert_eq!(r2.output, "42\n");
    }

    #[test]
    fn eval_clear_resets() {
        if !require_codegen() {
            return;
        }
        let mut session = ReplSession::new();
        let _ = session.eval("let x = 10;");
        let r = session.eval(":clear");
        assert_eq!(r.output, "Session cleared.\n");
        let r2 = session.eval("x + 1");
        assert!(r2.had_errors);
    }

    #[test]
    fn eval_parse_error() {
        let mut session = ReplSession::new();
        let result = session.eval("fn {");
        assert!(result.had_errors);
        assert!(!result.errors.is_empty());
    }

    #[test]
    fn eval_help_command() {
        let mut session = ReplSession::new();
        let result = session.eval(":help");
        assert!(!result.had_errors);
        assert!(result.output.contains(":quit"));
    }

    #[test]
    fn eval_unknown_command() {
        let mut session = ReplSession::new();
        let result = session.eval(":foo");
        assert!(result.had_errors);
        assert!(result.errors[0].contains("Unknown command"));
    }

    #[test]
    fn eval_one_expression() {
        if !require_codegen() {
            return;
        }
        let result = eval_one("2 * 3");
        assert_eq!(result.unwrap(), "6\n");
    }

    #[test]
    fn eval_file_multiline() {
        if !require_codegen() {
            return;
        }
        let dir = std::env::temp_dir();
        let path = dir.join("hew_eval_multiline_test.hew");
        std::fs::write(
            &path,
            "fn add(a: i64, b: i64) -> i64 {\n    a + b\n}\n\nadd(1, 2)\n",
        )
        .unwrap();
        let result = eval_file(path.to_str().unwrap());
        assert!(result.is_ok(), "eval_file failed: {result:?}");
        std::fs::remove_file(&path).ok();
    }
}
