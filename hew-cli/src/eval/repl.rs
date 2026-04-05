//! Main REPL loop — interactive read-eval-print for Hew.

use super::classify::{self, InputKind, ReplCommand};
use super::session::Session;
use std::fmt;
use std::io::Read;
use std::path::Path;
use std::process::{Child, Command, Stdio};
use std::thread::JoinHandle;
use std::time::{Duration, Instant};

const DEFAULT_EVAL_TIMEOUT: Duration = Duration::from_secs(30);

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
    execution_timeout: Duration,
}

#[derive(Debug)]
pub enum CliEvalError {
    DiagnosticsRendered,
    Message(String),
}

#[derive(Debug)]
enum LoadFileError {
    Message(String),
    Parse {
        source: String,
        errors: Vec<hew_parser::ParseError>,
    },
}

impl fmt::Display for CliEvalError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::DiagnosticsRendered => write!(f, "diagnostics already rendered"),
            Self::Message(message) => f.write_str(message),
        }
    }
}

impl std::error::Error for CliEvalError {}

impl fmt::Display for LoadFileError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Message(message) => f.write_str(message),
            Self::Parse { errors, .. } => f.write_str(
                &errors
                    .iter()
                    .map(|error| error.message.as_str())
                    .collect::<Vec<_>>()
                    .join("\n"),
            ),
        }
    }
}

struct CheckedProgram {
    kind: InputKind,
    program: hew_parser::ast::Program,
    source: String,
}

enum EvalCheckFailure {
    Parse {
        source: String,
        errors: Vec<hew_parser::ParseError>,
    },
    Type {
        source: String,
        errors: Vec<hew_types::TypeError>,
    },
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
        Self::with_timeout(DEFAULT_EVAL_TIMEOUT)
    }

    /// Create a REPL session with a custom execution timeout.
    #[must_use]
    pub fn with_timeout(execution_timeout: Duration) -> Self {
        Self {
            session: Session::new(),
            execution_timeout,
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
        let checked_program = match self.prepare_program(trimmed, kind) {
            Ok(program) => program,
            Err(EvalCheckFailure::Parse { errors, .. }) => {
                return EvalResult {
                    output: String::new(),
                    had_errors: true,
                    errors: errors.into_iter().map(|error| error.message).collect(),
                };
            }
            Err(EvalCheckFailure::Type { errors, .. }) => {
                return EvalResult {
                    output: String::new(),
                    had_errors: true,
                    errors: errors.into_iter().map(|error| error.message).collect(),
                };
            }
        };

        // Compile and execute in-process.  Import resolution and typecheck are
        // re-run inside compile_from_source_checked with correct stage ordering
        // (resolve imports BEFORE typecheck) so that stdlib type metadata is
        // available to the enrichment and codegen passes.
        match run_inprocess_compiled(
            checked_program.program,
            &checked_program.source,
            self.execution_timeout,
        ) {
            Ok(output) => {
                // On success, persist the input into session state.
                self.record_success(trimmed, &checked_program.kind);

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

    fn eval_cli(&mut self, input: &str, input_name: &str) -> Result<String, CliEvalError> {
        let trimmed = input.trim();
        if trimmed.is_empty() {
            return Ok(String::new());
        }

        let kind = classify::classify(trimmed);

        if let InputKind::Command(cmd) = &kind {
            return self.handle_cli_command(cmd, input_name);
        }

        let checked_program = match self.prepare_program(trimmed, kind) {
            Ok(program) => program,
            Err(EvalCheckFailure::Parse { source, errors }) => {
                crate::diagnostic::render_parse_diagnostics(&source, input_name, &errors);
                return Err(CliEvalError::DiagnosticsRendered);
            }
            Err(EvalCheckFailure::Type { source, errors }) => {
                crate::diagnostic::render_type_diagnostics(&source, input_name, &errors);
                return Err(CliEvalError::DiagnosticsRendered);
            }
        };

        match run_inprocess_compiled(
            checked_program.program,
            &checked_program.source,
            self.execution_timeout,
        ) {
            Ok(output) => {
                self.record_success(trimmed, &checked_program.kind);
                Ok(output)
            }
            Err(error) => Err(CliEvalError::Message(error)),
        }
    }

    /// Show the inferred type of an expression.
    ///
    /// # Errors
    ///
    /// Returns parse or type errors if the expression is invalid.
    pub fn type_of(&mut self, expr: &str) -> Result<String, Vec<String>> {
        match self.type_of_checked(expr) {
            Ok(ty) => Ok(ty),
            Err(EvalCheckFailure::Parse { errors, .. }) => {
                Err(errors.into_iter().map(|error| error.message).collect())
            }
            Err(EvalCheckFailure::Type { errors, .. }) => {
                Err(errors.into_iter().map(|error| error.message).collect())
            }
        }
    }

    fn type_of_checked(&mut self, expr: &str) -> Result<String, EvalCheckFailure> {
        let source = self.session.build_type_query(expr);
        let parse_result = hew_parser::parse(&source);
        if !parse_result.errors.is_empty() {
            return Err(EvalCheckFailure::Parse {
                source,
                errors: parse_result.errors,
            });
        }

        let mut checker = hew_types::Checker::new(hew_types::module_registry::ModuleRegistry::new(
            hew_types::module_registry::build_module_search_paths(),
        ));
        let tco = checker.check_program(&parse_result.program);
        if !tco.errors.is_empty() {
            return Err(EvalCheckFailure::Type {
                source,
                errors: tco.errors,
            });
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
                    return Ok(ty.user_facing().to_string());
                }
            }

            // Fallback: check the return type of main.
            let _ = sig;
        }

        Ok("unknown".to_string())
    }

    fn eval_type_command_cli(
        &mut self,
        expr: &str,
        input_name: &str,
    ) -> Result<String, CliEvalError> {
        if expr.is_empty() {
            return Err(CliEvalError::Message(
                "Usage: :type <expression>".to_string(),
            ));
        }

        match self.type_of_checked(expr) {
            Ok(ty) => Ok(format!("{ty}\n")),
            Err(EvalCheckFailure::Parse { source, errors }) => {
                crate::diagnostic::render_parse_diagnostics(&source, input_name, &errors);
                Err(CliEvalError::DiagnosticsRendered)
            }
            Err(EvalCheckFailure::Type { source, errors }) => {
                crate::diagnostic::render_type_diagnostics(&source, input_name, &errors);
                Err(CliEvalError::DiagnosticsRendered)
            }
        }
    }

    /// Load a file into the session.
    ///
    /// # Errors
    ///
    /// Returns an error if the file cannot be read or parsed.
    fn load_file(&mut self, path: &str) -> Result<String, LoadFileError> {
        let source = std::fs::read_to_string(path)
            .map_err(|e| LoadFileError::Message(format!("cannot read '{path}': {e}")))?;

        // Parse the file to extract items and statements.
        let parse_result = hew_parser::parse(&source);
        if !parse_result.errors.is_empty() {
            return Err(LoadFileError::Parse {
                source,
                errors: parse_result.errors,
            });
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

    fn handle_cli_command(
        &mut self,
        cmd: &ReplCommand,
        input_name: &str,
    ) -> Result<String, CliEvalError> {
        match cmd {
            ReplCommand::Type(expr) => self.eval_type_command_cli(expr, input_name),
            ReplCommand::Load(path) if !path.is_empty() => match self.load_file(path) {
                Ok(message) => Ok(format!("{message}\n")),
                Err(LoadFileError::Parse { source, errors }) => {
                    crate::diagnostic::render_parse_diagnostics(&source, path, &errors);
                    Err(CliEvalError::DiagnosticsRendered)
                }
                Err(error) => Err(CliEvalError::Message(error.to_string())),
            },
            _ => {
                let result = self.handle_command(cmd);
                if result.had_errors {
                    Err(CliEvalError::Message(result.errors.join("\n")))
                } else {
                    Ok(result.output)
                }
            }
        }
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
                        errors: vec![e.to_string()],
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

    fn prepare_program(
        &self,
        input: &str,
        kind: InputKind,
    ) -> Result<CheckedProgram, EvalCheckFailure> {
        let synthetic_program = self.session.build_program_with_kind(input, kind.clone());

        let parse_result = hew_parser::parse(&synthetic_program.source);
        if !parse_result.errors.is_empty() {
            return Err(EvalCheckFailure::Parse {
                source: synthetic_program.source,
                errors: parse_result.errors,
            });
        }

        let mut checker = hew_types::Checker::new(hew_types::module_registry::ModuleRegistry::new(
            hew_types::module_registry::build_module_search_paths(),
        ));
        let tco = checker.check_program(&parse_result.program);

        if !tco.errors.is_empty() {
            return Err(EvalCheckFailure::Type {
                source: synthetic_program.source,
                errors: tco.errors,
            });
        }

        Ok(CheckedProgram {
            kind,
            program: parse_result.program,
            source: synthetic_program.source,
        })
    }

    fn record_success(&mut self, input: &str, kind: &InputKind) {
        match kind {
            InputKind::Item => self.session.add_item(input),
            InputKind::Statement => self.session.add_binding(input),
            InputKind::Expression | InputKind::Command(_) => {}
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
enum InteractiveEvalOutcome {
    Continue,
    Output(String),
    RenderedDiagnostics,
    MessageError(String),
    Quit,
}

fn handle_interactive_input(session: &mut ReplSession, input: &str) -> InteractiveEvalOutcome {
    if matches!(
        classify::classify(input),
        InputKind::Command(ReplCommand::Quit)
    ) {
        return InteractiveEvalOutcome::Quit;
    }

    match session.eval_cli(input, "<repl>") {
        Ok(output) if output.is_empty() => InteractiveEvalOutcome::Continue,
        Ok(output) => InteractiveEvalOutcome::Output(output),
        Err(CliEvalError::DiagnosticsRendered) => InteractiveEvalOutcome::RenderedDiagnostics,
        Err(CliEvalError::Message(message)) => InteractiveEvalOutcome::MessageError(message),
    }
}

/// Compile the given already-parsed program to a native binary in a temporary
/// directory and execute it, returning its stdout.
///
/// Import resolution and typecheck are performed here (not by the caller)
/// so that the codegen pipeline sees stdlib type information in the same order
/// as the normal `compile()` path.  The REPL's fast in-process typecheck is
/// kept for user-facing error reporting only; this function runs the full
/// correctly-ordered pipeline for codegen.
fn run_inprocess_compiled(
    program: hew_parser::ast::Program,
    source: &str,
    timeout: Duration,
) -> Result<String, String> {
    let tmp_dir = tempfile::tempdir().map_err(|e| format!("cannot create temp dir: {e}"))?;

    let bin_name = format!("eval_bin{}", crate::platform::exe_suffix());
    let bin_path = tmp_dir.path().join(bin_name);
    let bin_path_str = bin_path
        .to_str()
        .ok_or_else(|| "temp binary path is not valid UTF-8".to_string())?;

    crate::compile::compile_from_source_checked(
        program,
        source,
        "<repl>",
        bin_path_str,
        &crate::compile::CompileOptions::default(),
    )?;

    match run_eval_binary_with_timeout(&bin_path, timeout) {
        Ok(crate::process::BinaryRunOutcome::Success { stdout }) => {
            // Normalize Windows \r\n line endings to \n for consistent output.
            Ok(stdout.replace("\r\n", "\n"))
        }
        Ok(crate::process::BinaryRunOutcome::Failed { stderr, .. }) => Err(if stderr.is_empty() {
            "program exited with non-zero status".to_string()
        } else {
            stderr
        }),
        Ok(crate::process::BinaryRunOutcome::Timeout) => Err(format!(
            "evaluation timed out after {}",
            crate::process::format_timeout(timeout)
        )),
        Err(e) => Err(format!("cannot execute compiled program: {e}")),
    }
}

/// Execute an eval binary with bounded wall-clock time while draining both
/// output pipes. Phase-1 keeps this local to eval so `hew test` behavior stays
/// unchanged in this lane.
fn run_eval_binary_with_timeout(
    binary: &Path,
    timeout: Duration,
) -> Result<crate::process::BinaryRunOutcome, String> {
    let mut command = Command::new(binary);
    command.stdout(Stdio::piped()).stderr(Stdio::piped());

    let mut child = spawn_eval_child(&mut command)?;
    let output = EvalChildOutput::spawn(&mut child)?;
    let start = Instant::now();

    loop {
        match child.try_wait() {
            Ok(Some(status)) => {
                let (stdout, stderr) = output.finish()?;
                if status.success() {
                    return Ok(crate::process::BinaryRunOutcome::Success { stdout });
                }
                return Ok(crate::process::BinaryRunOutcome::Failed { stdout, stderr });
            }
            Ok(None) => {
                if start.elapsed() > timeout {
                    terminate_timed_out_eval_child(&mut child)?;
                    let _ = output.finish()?;
                    return Ok(crate::process::BinaryRunOutcome::Timeout);
                }
                std::thread::sleep(Duration::from_millis(10));
            }
            Err(e) => return Err(format!("cannot poll child process: {e}")),
        }
    }
}

struct EvalChildOutput {
    stdout: EvalPipeReader,
    stderr: EvalPipeReader,
}

impl EvalChildOutput {
    fn spawn(child: &mut Child) -> Result<Self, String> {
        let stdout = child
            .stdout
            .take()
            .ok_or_else(|| "child stdout pipe missing".to_string())?;
        let stderr = child
            .stderr
            .take()
            .ok_or_else(|| "child stderr pipe missing".to_string())?;

        Ok(Self {
            stdout: EvalPipeReader::spawn(stdout, "stdout"),
            stderr: EvalPipeReader::spawn(stderr, "stderr"),
        })
    }

    fn finish(self) -> Result<(String, String), String> {
        Ok((self.stdout.finish()?, self.stderr.finish()?))
    }
}

struct EvalPipeReader {
    name: &'static str,
    handle: JoinHandle<Result<String, String>>,
}

impl EvalPipeReader {
    fn spawn<T>(stream: T, name: &'static str) -> Self
    where
        T: Read + Send + 'static,
    {
        Self {
            name,
            handle: std::thread::spawn(move || read_eval_pipe(stream, name)),
        }
    }

    fn finish(self) -> Result<String, String> {
        self.handle
            .join()
            .map_err(|_| format!("child {} reader panicked", self.name))?
    }
}

fn read_eval_pipe<T: Read>(mut stream: T, name: &str) -> Result<String, String> {
    let mut bytes = Vec::new();
    stream
        .read_to_end(&mut bytes)
        .map_err(|e| format!("cannot read child {name}: {e}"))?;
    Ok(String::from_utf8_lossy(&bytes).into_owned())
}

fn terminate_timed_out_eval_child(child: &mut Child) -> Result<(), String> {
    kill_timed_out_eval_child(child)?;
    child
        .wait()
        .map_err(|e| format!("cannot reap timed-out child process: {e}"))?;
    Ok(())
}

#[cfg(unix)]
fn spawn_eval_child(command: &mut Command) -> Result<Child, String> {
    use std::os::unix::process::CommandExt;

    // SAFETY: `pre_exec` runs in the child process after `fork` and before
    // `exec`. `setpgid(0, 0)` only mutates the child's own process-group
    // membership so timed-out executions can be terminated as a group.
    unsafe {
        command.pre_exec(|| {
            if libc::setpgid(0, 0) == 0 {
                Ok(())
            } else {
                Err(std::io::Error::last_os_error())
            }
        });
    }

    command
        .spawn()
        .map_err(|e| format!("cannot spawn child process: {e}"))
}

#[cfg(not(unix))]
fn spawn_eval_child(command: &mut Command) -> Result<Child, String> {
    command
        .spawn()
        .map_err(|e| format!("cannot spawn child process: {e}"))
}

#[cfg(unix)]
#[allow(
    clippy::cast_possible_wrap,
    reason = "PIDs fit in i32 on all supported Unix platforms"
)]
fn kill_timed_out_eval_child(child: &mut Child) -> Result<(), String> {
    let process_group = child.id() as i32;
    // SAFETY: `killpg` targets the child-created process group. If the group is
    // already gone, `ESRCH` is treated as success and `wait()` reaps the child.
    let result = unsafe { libc::killpg(process_group, libc::SIGKILL) };
    if result == 0 {
        return Ok(());
    }

    let group_error = std::io::Error::last_os_error();
    if group_error.raw_os_error() == Some(libc::ESRCH) {
        return Ok(());
    }

    child.kill().map_err(|kill_error| {
        format!(
            "cannot kill timed-out child process group: {group_error}; \
             fallback child kill failed: {kill_error}"
        )
    })
}

#[cfg(not(unix))]
fn kill_timed_out_eval_child(child: &mut Child) -> Result<(), String> {
    child
        .kill()
        .map_err(|e| format!("cannot kill timed-out child process: {e}"))
}

/// Run the interactive REPL with a custom execution timeout.
///
/// # Errors
///
/// Returns an error if readline fails fatally.
pub fn run_interactive(timeout: Duration) -> Result<(), Box<dyn std::error::Error>> {
    let mut rl = rustyline::DefaultEditor::new()?;
    let mut session = ReplSession::with_timeout(timeout);

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

        match handle_interactive_input(&mut session, trimmed) {
            InteractiveEvalOutcome::Continue | InteractiveEvalOutcome::RenderedDiagnostics => {}
            InteractiveEvalOutcome::Output(output) => print!("{output}"),
            InteractiveEvalOutcome::MessageError(message) => eprintln!("error: {message}"),
            InteractiveEvalOutcome::Quit => break,
        }
    }

    Ok(())
}

/// Evaluate a single expression non-interactively.
///
/// # Errors
///
/// Returns an error string if evaluation fails.
pub fn eval_one(expr: &str, timeout: Duration) -> Result<String, CliEvalError> {
    let mut session = ReplSession::with_timeout(timeout);
    session.eval_cli(expr, "<eval>")
}

/// Evaluate a file in REPL context.
///
/// # Errors
///
/// Returns an error string if evaluation fails.
pub fn eval_file(path: &str, timeout: Duration) -> Result<(), CliEvalError> {
    let (source, input_name) = if path == "-" {
        let mut source = String::new();
        std::io::stdin()
            .read_to_string(&mut source)
            .map_err(|e| CliEvalError::Message(format!("cannot read stdin: {e}")))?;
        (source, String::from("<stdin>"))
    } else {
        let source = std::fs::read_to_string(path)
            .map_err(|e| CliEvalError::Message(format!("cannot read '{path}': {e}")))?;
        (source, path.to_string())
    };

    let mut session = ReplSession::with_timeout(timeout);
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

        let output = session.eval_cli(input, &input_name)?;
        if !output.is_empty() {
            print!("{output}");
        }
        buffer.clear();
    }

    // Evaluate any remaining buffered input.
    let input = buffer.trim();
    if !input.is_empty() {
        let output = session.eval_cli(input, &input_name)?;
        if !output.is_empty() {
            print!("{output}");
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

    /// Verifies the in-process codegen pipeline is available by compiling a
    /// trivial program.  Returns false (and prints a skip message) when the
    /// embedded `hew-codegen` backend or `libhew_runtime.a` aren't built yet.
    fn require_toolchain() -> bool {
        static OK: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
        *OK.get_or_init(|| {
            let dir = tempfile::tempdir().expect("temp dir");
            let bin_name = format!("probe{}", crate::platform::exe_suffix());
            let bin_path = dir.path().join(bin_name);
            let source = "fn main() { println(\"ok\"); }\n";
            let parse_result = hew_parser::parse(source);
            if !parse_result.errors.is_empty() {
                eprintln!("REPL integration tests skipped: probe parse failed");
                return false;
            }
            let ok = crate::compile::compile_from_source_checked(
                parse_result.program,
                source,
                "<repl-probe>",
                bin_path.to_str().unwrap_or("probe"),
                &crate::compile::CompileOptions::default(),
            )
            .is_ok();
            if !ok {
                eprintln!(
                    "REPL integration tests skipped: \
                     in-process compile failed (codegen/runtime not available)"
                );
            }
            ok
        })
    }

    #[test]
    fn eval_arithmetic() {
        if !require_toolchain() {
            return;
        }
        let mut session = ReplSession::new();
        let result = session.eval("1 + 2");
        assert!(!result.had_errors, "errors: {:?}", result.errors);
        assert_eq!(result.output, "3\n");
    }

    #[test]
    fn eval_binding_persists() {
        if !require_toolchain() {
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
        if !require_toolchain() {
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
    fn eval_doc_commented_function_persists() {
        if !require_toolchain() {
            return;
        }
        let mut session = ReplSession::new();
        let r1 = session.eval("/// Doubles the input.\nfn double(x: i64) -> i64 { x * 2 }");
        assert!(!r1.had_errors, "errors: {:?}", r1.errors);
        let r2 = session.eval("double(21)");
        assert!(!r2.had_errors, "errors: {:?}", r2.errors);
        assert_eq!(r2.output, "42\n");
    }

    /// Regression test: regex literals must produce valid MLIR via the
    /// in-process codegen path.  The old implementation passed a stale `tco`
    /// (computed before import resolution) to `enrich_program_ast`, causing a
    /// call-site / declaration type mismatch for `hew_regex_new` in the
    /// generated MLIR.
    #[test]
    fn eval_regex_literal() {
        if !require_toolchain() {
            return;
        }
        let mut session = ReplSession::new();
        let result = session.eval(r#""hello" =~ re"h.*o""#);
        assert!(!result.had_errors, "errors: {:?}", result.errors);
        assert_eq!(result.output, "true\n");
    }

    #[test]
    fn eval_clear_resets() {
        if !require_toolchain() {
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
        if !require_toolchain() {
            return;
        }
        let result = eval_one("2 * 3", DEFAULT_EVAL_TIMEOUT);
        assert_eq!(result.unwrap(), "6\n");
    }

    #[test]
    fn eval_timeout_is_reported() {
        if !require_toolchain() {
            return;
        }
        let mut session = ReplSession::with_timeout(Duration::from_millis(100));
        let define =
            session.eval("fn spin_forever() {\n    loop {\n        println(\"spin\");\n    }\n}");
        assert!(!define.had_errors, "errors: {:?}", define.errors);

        let result = session.eval("spin_forever()");
        assert!(result.had_errors);
        assert!(result.errors[0].contains("evaluation timed out after 100ms"));
    }

    #[test]
    fn interactive_parse_errors_use_shared_diagnostics_path() {
        let mut session = ReplSession::new();
        assert_eq!(
            handle_interactive_input(&mut session, "1 +"),
            InteractiveEvalOutcome::RenderedDiagnostics
        );
    }

    #[test]
    fn interactive_type_errors_use_shared_diagnostics_path() {
        let mut session = ReplSession::new();
        assert_eq!(
            handle_interactive_input(&mut session, "let answer: i64 = \"oops\";"),
            InteractiveEvalOutcome::RenderedDiagnostics
        );
    }

    #[test]
    fn type_command_parse_errors_use_shared_diagnostics_path() {
        let mut session = ReplSession::new();
        assert!(matches!(
            session.eval_cli(":type 1 +", "<repl>"),
            Err(CliEvalError::DiagnosticsRendered)
        ));
    }

    #[test]
    fn type_command_type_errors_use_shared_diagnostics_path() {
        let mut session = ReplSession::new();
        assert!(matches!(
            session.eval_cli(":type 1 + \"x\"", "<repl>"),
            Err(CliEvalError::DiagnosticsRendered)
        ));
    }

    #[test]
    fn eval_file_multiline() {
        if !require_toolchain() {
            return;
        }
        let dir = tempfile::tempdir().expect("temp dir");
        let path = dir.path().join("hew_eval_multiline_test.hew");
        std::fs::write(
            &path,
            "fn add(a: i64, b: i64) -> i64 {\n    a + b\n}\n\nadd(1, 2)\n",
        )
        .unwrap();
        let result = eval_file(path.to_str().unwrap(), DEFAULT_EVAL_TIMEOUT);
        assert!(result.is_ok(), "eval_file failed: {result:?}");
    }
}
