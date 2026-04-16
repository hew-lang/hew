//! Main REPL loop — interactive read-eval-print for Hew.

use super::classify::{self, InputCompleteness, InputKind, ReplCommand};
use super::session::{Session, SessionCounts, SyntheticDiagnosticView};
use std::fmt;
use std::io::Read;
use std::path::PathBuf;
use std::time::Duration;

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
    /// Project directory used to resolve manifest deps and local imports for
    /// in-memory compile, matching the behaviour of `compile_file`.
    project_dir: Option<PathBuf>,
    /// Target triple for compilation (e.g. `wasm32-wasi`). When `None` the
    /// host native target is used.
    eval_target: Option<String>,
}

#[derive(Debug)]
pub enum CliEvalError {
    DiagnosticsRendered,
    Message(String),
    /// The compiled program exited non-zero. Any stdout produced before the
    /// failure is preserved so callers can surface it to the user, and runtime
    /// stderr is captured so JSON callers can include it in the contract.
    RuntimeFailure {
        stdout: String,
        stderr: String,
        exit_code: i32,
    },
}

#[derive(Debug)]
enum LoadFileError {
    Message(String),
    DiagnosticsRendered,
}

impl fmt::Display for CliEvalError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::DiagnosticsRendered => write!(f, "diagnostics already rendered"),
            Self::Message(message) => f.write_str(message),
            Self::RuntimeFailure { exit_code, .. } => {
                write!(f, "program exited with status {exit_code}")
            }
        }
    }
}

impl std::error::Error for CliEvalError {}

impl fmt::Display for LoadFileError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Message(message) => f.write_str(message),
            Self::DiagnosticsRendered => f.write_str("diagnostics already rendered"),
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
        diagnostic_view: Option<SyntheticDiagnosticView>,
        errors: Vec<hew_parser::ParseError>,
    },
    Type {
        source: String,
        diagnostic_view: Option<SyntheticDiagnosticView>,
        errors: Vec<hew_types::TypeError>,
        module_source_map: Box<crate::diagnostic::ModuleSourceMap>,
    },
}

enum CompiledEvalError {
    DiagnosticsRendered,
    Message(String),
    RuntimeFailure {
        stdout: String,
        stderr: String,
        exit_code: i32,
    },
}

impl From<crate::compile::CompileFromSourceError> for CompiledEvalError {
    fn from(error: crate::compile::CompileFromSourceError) -> Self {
        match error {
            crate::compile::CompileFromSourceError::DiagnosticsRendered => {
                Self::DiagnosticsRendered
            }
            crate::compile::CompileFromSourceError::Message(message) => Self::Message(message),
        }
    }
}

pub(crate) fn emit_runtime_failure_output(stdout: &str, stderr: &str) {
    if !stdout.is_empty() {
        print!("{stdout}");
    }
    if !stderr.is_empty() {
        eprint!("{stderr}");
    }
}

fn normalize_captured_output(output: &str) -> String {
    output.replace("\r\n", "\n")
}

impl Default for ReplSession {
    fn default() -> Self {
        Self::new()
    }
}

fn typecheck_program(
    program: &hew_parser::ast::Program,
    enable_wasm: bool,
) -> hew_types::check::TypeCheckOutput {
    let mut checker = hew_types::Checker::new(hew_types::module_registry::ModuleRegistry::new(
        hew_types::module_registry::build_module_search_paths(),
    ));
    if enable_wasm {
        checker.enable_wasm_target();
    }
    checker.check_program(program)
}

fn program_has_imports(program: &hew_parser::ast::Program) -> bool {
    program
        .items
        .iter()
        .any(|(item, _)| matches!(item, hew_parser::ast::Item::Import(_)))
}

fn render_eval_parse_diagnostics(
    source: &str,
    input_name: &str,
    diagnostic_view: Option<&SyntheticDiagnosticView>,
    errors: &[hew_parser::ParseError],
) {
    if let Some(diagnostic_view) = diagnostic_view {
        if let Some(errors) = remap_parse_errors(diagnostic_view, errors) {
            crate::diagnostic::render_parse_diagnostics(
                &diagnostic_view.source,
                input_name,
                &errors,
            );
            return;
        }
    }

    crate::diagnostic::render_parse_diagnostics(source, input_name, errors);
}

fn render_eval_type_diagnostics(
    source: &str,
    input_name: &str,
    diagnostic_view: Option<&SyntheticDiagnosticView>,
    diagnostics: &[hew_types::TypeError],
    module_source_map: &crate::diagnostic::ModuleSourceMap,
) {
    let (remapped, original) = split_eval_type_diagnostics(diagnostic_view, diagnostics);

    if let Some(diagnostic_view) = diagnostic_view {
        if !remapped.is_empty() {
            crate::diagnostic::render_type_diagnostics(
                &diagnostic_view.source,
                input_name,
                &remapped,
            );
        }
    }

    if !original.is_empty() {
        crate::diagnostic::render_type_diagnostics_with_sources(
            source,
            input_name,
            &original,
            module_source_map,
        );
    }
}

fn remap_parse_errors(
    diagnostic_view: &SyntheticDiagnosticView,
    errors: &[hew_parser::ParseError],
) -> Option<Vec<hew_parser::ParseError>> {
    let mut remapped = Vec::new();
    let mut fallback_suffix_error = None;

    for error in errors {
        match remap_parse_span(diagnostic_view, &error.span)? {
            RemappedParseSpan::InInput(span) => remapped.push(hew_parser::ParseError {
                span,
                ..error.clone()
            }),
            RemappedParseSpan::AtInputEnd(span) => {
                fallback_suffix_error.get_or_insert_with(|| hew_parser::ParseError {
                    span,
                    ..error.clone()
                });
            }
        }
    }

    if remapped.is_empty() {
        remapped.push(fallback_suffix_error?);
    }

    Some(remapped)
}

fn remap_type_diagnostic(
    diagnostic_view: &SyntheticDiagnosticView,
    diagnostic: &hew_types::TypeError,
) -> Option<hew_types::TypeError> {
    let span = remap_type_span(diagnostic_view, &diagnostic.span)?;
    let notes = diagnostic
        .notes
        .iter()
        .map(|(span, message)| Some((remap_type_span(diagnostic_view, span)?, message.clone())))
        .collect::<Option<Vec<_>>>()?;

    Some(hew_types::TypeError {
        span,
        notes,
        ..diagnostic.clone()
    })
}

fn split_eval_type_diagnostics(
    diagnostic_view: Option<&SyntheticDiagnosticView>,
    diagnostics: &[hew_types::TypeError],
) -> (Vec<hew_types::TypeError>, Vec<hew_types::TypeError>) {
    let mut remapped = Vec::new();
    let mut original = Vec::new();

    for diagnostic in diagnostics {
        if diagnostic.source_module.is_none() {
            if let Some(diagnostic_view) = diagnostic_view {
                if let Some(remapped_diagnostic) =
                    remap_type_diagnostic(diagnostic_view, diagnostic)
                {
                    remapped.push(remapped_diagnostic);
                    continue;
                }
            }
        }

        original.push(diagnostic.clone());
    }

    (remapped, original)
}

enum RemappedParseSpan {
    InInput(std::ops::Range<usize>),
    AtInputEnd(std::ops::Range<usize>),
}

fn remap_parse_span(
    diagnostic_view: &SyntheticDiagnosticView,
    span: &std::ops::Range<usize>,
) -> Option<RemappedParseSpan> {
    if span.start < diagnostic_view.input_span.start {
        return None;
    }

    let input_len = diagnostic_view.source.len();
    if span.start >= diagnostic_view.input_span.end {
        return Some(RemappedParseSpan::AtInputEnd(input_len..input_len));
    }

    let start = span.start - diagnostic_view.input_span.start;
    let end = span
        .end
        .saturating_sub(diagnostic_view.input_span.start)
        .min(input_len);
    Some(RemappedParseSpan::InInput(start..end.max(start)))
}

fn remap_type_span(
    diagnostic_view: &SyntheticDiagnosticView,
    span: &std::ops::Range<usize>,
) -> Option<std::ops::Range<usize>> {
    if span.start < diagnostic_view.input_span.start || span.start >= diagnostic_view.input_span.end
    {
        return None;
    }

    let input_len = diagnostic_view.source.len();
    let start = span.start.saturating_sub(diagnostic_view.input_span.start);
    let end = span
        .end
        .saturating_sub(diagnostic_view.input_span.start)
        .min(input_len);
    Some(start..end.max(start))
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
            project_dir: None,
            eval_target: None,
        }
    }

    /// Create a REPL session anchored to the project containing `file_path`.
    ///
    /// The parent directory of `file_path` is used as the project root so that
    /// manifest deps, local `src/` imports, and lockfile resolution behave
    /// identically to a `compile_file` invocation on that file.
    #[must_use]
    pub fn for_path(file_path: &str, execution_timeout: Duration) -> Self {
        let project_dir = std::path::Path::new(file_path)
            .parent()
            .map(std::path::Path::to_path_buf);
        Self {
            session: Session::new(),
            execution_timeout,
            project_dir,
            eval_target: None,
        }
    }

    /// Create a REPL session anchored to the project containing `file_path`
    /// and configured for the given compilation target.
    #[must_use]
    pub fn for_path_with_target(
        file_path: &str,
        execution_timeout: Duration,
        target: Option<&str>,
    ) -> Self {
        let mut session = Self::for_path(file_path, execution_timeout);
        session.eval_target = target.map(str::to_owned);
        session
    }

    /// Create a REPL session with a custom execution timeout and target.
    #[must_use]
    pub fn with_timeout_and_target(execution_timeout: Duration, target: Option<&str>) -> Self {
        let mut session = Self::with_timeout(execution_timeout);
        session.eval_target = target.map(str::to_owned);
        session
    }

    fn is_wasm_target(&self) -> bool {
        self.eval_target.as_deref().is_some_and(|t| {
            crate::target::TargetSpec::from_requested(Some(t)).is_ok_and(|spec| spec.is_wasm())
        })
    }

    /// Evaluate a line of input and return the result.
    #[cfg_attr(
        not(test),
        allow(
            dead_code,
            reason = "CLI entry points currently route through eval_cli"
        )
    )]
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
        match run_eval_compiled(
            checked_program.program,
            &checked_program.source,
            "<repl>",
            self.execution_timeout,
            self.project_dir.clone(),
            self.eval_target.as_deref(),
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
            Err(CompiledEvalError::DiagnosticsRendered) => EvalResult {
                output: String::new(),
                had_errors: true,
                errors: vec!["diagnostics already rendered".to_string()],
            },
            Err(CompiledEvalError::Message(error)) => EvalResult {
                output: String::new(),
                had_errors: true,
                errors: vec![error],
            },
            Err(CompiledEvalError::RuntimeFailure {
                stdout,
                stderr,
                exit_code,
            }) => {
                if !stderr.is_empty() {
                    eprint!("{stderr}");
                }
                EvalResult {
                    output: stdout,
                    had_errors: true,
                    errors: vec![format!("program exited with status {exit_code}")],
                }
            }
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
            Err(EvalCheckFailure::Parse {
                source,
                diagnostic_view,
                errors,
            }) => {
                render_eval_parse_diagnostics(
                    &source,
                    input_name,
                    diagnostic_view.as_ref(),
                    &errors,
                );
                return Err(CliEvalError::DiagnosticsRendered);
            }
            Err(EvalCheckFailure::Type {
                source,
                diagnostic_view,
                errors,
                module_source_map,
            }) => {
                render_eval_type_diagnostics(
                    &source,
                    input_name,
                    diagnostic_view.as_ref(),
                    &errors,
                    &module_source_map,
                );
                return Err(CliEvalError::DiagnosticsRendered);
            }
        };

        match run_eval_compiled(
            checked_program.program,
            &checked_program.source,
            "<repl>",
            self.execution_timeout,
            self.project_dir.clone(),
            self.eval_target.as_deref(),
        ) {
            Ok(output) => {
                self.record_success(trimmed, &checked_program.kind);
                Ok(output)
            }
            Err(CompiledEvalError::DiagnosticsRendered) => Err(CliEvalError::DiagnosticsRendered),
            Err(CompiledEvalError::Message(error)) => Err(CliEvalError::Message(error)),
            Err(CompiledEvalError::RuntimeFailure {
                stdout,
                stderr,
                exit_code,
            }) => Err(CliEvalError::RuntimeFailure {
                stdout,
                stderr,
                exit_code,
            }),
        }
    }

    fn eval_file_cli(
        &mut self,
        input: &str,
        input_name: &str,
        source_label: &str,
    ) -> Result<String, CliEvalError> {
        let trimmed = input.trim();
        if trimmed.is_empty() {
            return Ok(String::new());
        }

        let kind = classify::classify(trimmed);

        if let InputKind::Command(cmd) = &kind {
            return self.handle_cli_command(cmd, input_name);
        }

        let synthetic_program = self.session.build_program_with_kind(trimmed, kind.clone());
        let parse_result = hew_parser::parse(&synthetic_program.source);
        if !parse_result.errors.is_empty() {
            render_eval_parse_diagnostics(
                &synthetic_program.source,
                input_name,
                synthetic_program.diagnostic_view.as_ref(),
                &parse_result.errors,
            );
            return Err(CliEvalError::DiagnosticsRendered);
        }

        if matches!(kind, InputKind::Expression | InputKind::Statement)
            && !program_has_imports(&parse_result.program)
        {
            let tco = typecheck_program(&parse_result.program, self.is_wasm_target());
            let module_source_map =
                crate::diagnostic::build_module_source_map(&parse_result.program);
            if !tco.errors.is_empty() {
                render_eval_type_diagnostics(
                    &synthetic_program.source,
                    input_name,
                    synthetic_program.diagnostic_view.as_ref(),
                    &tco.errors,
                    &module_source_map,
                );
                return Err(CliEvalError::DiagnosticsRendered);
            }
        }

        match run_eval_compiled(
            parse_result.program,
            &synthetic_program.source,
            source_label,
            self.execution_timeout,
            self.project_dir.clone(),
            self.eval_target.as_deref(),
        ) {
            Ok(output) => {
                self.record_success(trimmed, &kind);
                Ok(output)
            }
            Err(CompiledEvalError::DiagnosticsRendered) => Err(CliEvalError::DiagnosticsRendered),
            Err(CompiledEvalError::Message(error)) => Err(CliEvalError::Message(error)),
            Err(CompiledEvalError::RuntimeFailure {
                stdout,
                stderr,
                exit_code,
            }) => Err(CliEvalError::RuntimeFailure {
                stdout,
                stderr,
                exit_code,
            }),
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
                diagnostic_view: None,
                errors: parse_result.errors,
            });
        }

        let tco = typecheck_program(&parse_result.program, self.is_wasm_target());
        let module_source_map = crate::diagnostic::build_module_source_map(&parse_result.program);
        if !tco.errors.is_empty() {
            return Err(EvalCheckFailure::Type {
                source,
                diagnostic_view: None,
                errors: tco.errors,
                module_source_map: Box::new(module_source_map),
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
            Err(EvalCheckFailure::Parse {
                source,
                diagnostic_view,
                errors,
            }) => {
                render_eval_parse_diagnostics(
                    &source,
                    input_name,
                    diagnostic_view.as_ref(),
                    &errors,
                );
                Err(CliEvalError::DiagnosticsRendered)
            }
            Err(EvalCheckFailure::Type {
                source,
                diagnostic_view,
                errors,
                module_source_map,
            }) => {
                render_eval_type_diagnostics(
                    &source,
                    input_name,
                    diagnostic_view.as_ref(),
                    &errors,
                    &module_source_map,
                );
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
        let before = self.session.counts();

        let output =
            self.eval_source_file_cli(&source, path, path)
                .map_err(|error| match error {
                    CliEvalError::DiagnosticsRendered => LoadFileError::DiagnosticsRendered,
                    CliEvalError::Message(message) => LoadFileError::Message(message),
                    CliEvalError::RuntimeFailure {
                        stdout,
                        stderr,
                        exit_code,
                    } => {
                        emit_runtime_failure_output(&stdout, &stderr);
                        LoadFileError::Message(format!("program exited with status {exit_code}"))
                    }
                })?;

        if !output.is_empty() {
            print!("{output}");
        }

        let added = session_count_delta(before, self.session.counts());
        Ok(format!("Loaded {path} ({})", describe_load_result(added)))
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
                Err(LoadFileError::DiagnosticsRendered) => Err(CliEvalError::DiagnosticsRendered),
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
            ReplCommand::Session => EvalResult {
                output: self.session.render_overview(),
                had_errors: false,
                errors: Vec::new(),
            },
            ReplCommand::Items => EvalResult {
                output: self.session.render_items(),
                had_errors: false,
                errors: Vec::new(),
            },
            ReplCommand::Bindings => EvalResult {
                output: self.session.render_bindings(),
                had_errors: false,
                errors: Vec::new(),
            },
            ReplCommand::Clear => {
                let removed = self.session.counts();
                self.clear();
                EvalResult {
                    output: describe_clear_result(removed),
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
        let diagnostic_view = synthetic_program.diagnostic_view.clone();

        let parse_result = hew_parser::parse(&synthetic_program.source);
        if !parse_result.errors.is_empty() {
            return Err(EvalCheckFailure::Parse {
                source: synthetic_program.source,
                diagnostic_view,
                errors: parse_result.errors,
            });
        }

        let tco = typecheck_program(&parse_result.program, self.is_wasm_target());
        let module_source_map = crate::diagnostic::build_module_source_map(&parse_result.program);

        if !tco.errors.is_empty() {
            return Err(EvalCheckFailure::Type {
                source: synthetic_program.source,
                diagnostic_view,
                errors: tco.errors,
                module_source_map: Box::new(module_source_map),
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
            InputKind::Statement => self.session.add_persistent_bindings_from_statement(input),
            InputKind::Expression | InputKind::Command(_) => {}
        }
    }

    fn eval_source_file_cli(
        &mut self,
        source: &str,
        input_name: &str,
        source_label: &str,
    ) -> Result<String, CliEvalError> {
        let mut buffer = String::new();
        let mut collected = String::new();

        for line in source.lines() {
            let trimmed = line.trim();
            if (trimmed.is_empty() || trimmed.starts_with("//")) && buffer.is_empty() {
                continue;
            }

            if !buffer.is_empty() {
                buffer.push('\n');
            }
            buffer.push_str(line);

            if matches!(
                classify::input_completeness(&buffer),
                InputCompleteness::Incomplete
            ) {
                continue;
            }

            let input = buffer.trim();
            if input.is_empty() {
                buffer.clear();
                continue;
            }

            match self.eval_file_cli(input, input_name, source_label) {
                Ok(output) => collected.push_str(&output),
                Err(CliEvalError::RuntimeFailure {
                    stdout,
                    stderr,
                    exit_code,
                }) => {
                    // Prepend output collected from successful earlier chunks so
                    // callers (non-JSON print path, JSON stdout field, :load) all
                    // see the full pre-failure output rather than just the partial
                    // output of the failing chunk.
                    if !collected.is_empty() {
                        collected.push_str(&stdout);
                        return Err(CliEvalError::RuntimeFailure {
                            stdout: collected,
                            stderr,
                            exit_code,
                        });
                    }
                    return Err(CliEvalError::RuntimeFailure {
                        stdout,
                        stderr,
                        exit_code,
                    });
                }
                Err(e) => return Err(e),
            }
            buffer.clear();
        }

        let input = buffer.trim();
        if !input.is_empty() {
            match self.eval_file_cli(input, input_name, source_label) {
                Ok(output) => collected.push_str(&output),
                Err(CliEvalError::RuntimeFailure {
                    stdout,
                    stderr,
                    exit_code,
                }) => {
                    if !collected.is_empty() {
                        collected.push_str(&stdout);
                        return Err(CliEvalError::RuntimeFailure {
                            stdout: collected,
                            stderr,
                            exit_code,
                        });
                    }
                    return Err(CliEvalError::RuntimeFailure {
                        stdout,
                        stderr,
                        exit_code,
                    });
                }
                Err(e) => return Err(e),
            }
        }

        Ok(collected)
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
        Err(CliEvalError::RuntimeFailure {
            stdout,
            stderr,
            exit_code,
        }) => {
            emit_runtime_failure_output(&stdout, &stderr);
            InteractiveEvalOutcome::MessageError(format!("program exited with status {exit_code}"))
        }
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
    source_label: &str,
    timeout: Duration,
    project_dir: Option<PathBuf>,
    target: Option<&str>,
) -> Result<String, CompiledEvalError> {
    let tmp_dir = tempfile::tempdir()
        .map_err(|e| CompiledEvalError::Message(format!("cannot create temp dir: {e}")))?;

    let bin_name = format!("eval_bin{}", crate::platform::exe_suffix());
    let bin_path = tmp_dir.path().join(bin_name);
    let bin_path_str = bin_path
        .to_str()
        .ok_or_else(|| CompiledEvalError::Message("temp binary path is not valid UTF-8".into()))?;

    crate::compile::compile_from_source_checked(
        program,
        source,
        source_label,
        bin_path_str,
        &crate::compile::CompileOptions {
            project_dir,
            target: target.map(str::to_owned),
            ..crate::compile::CompileOptions::default()
        },
    )
    .map_err(CompiledEvalError::from)?;

    match crate::process::run_binary_with_timeout(&bin_path, timeout) {
        Ok(crate::process::BinaryRunOutcome::Success { stdout }) => {
            Ok(normalize_captured_output(&stdout))
        }
        Ok(crate::process::BinaryRunOutcome::Failed {
            stdout,
            stderr,
            exit_code,
        }) => Err(CompiledEvalError::RuntimeFailure {
            stdout: normalize_captured_output(&stdout),
            stderr: normalize_captured_output(&stderr),
            exit_code,
        }),
        Ok(crate::process::BinaryRunOutcome::Timeout) => Err(CompiledEvalError::Message(format!(
            "evaluation timed out after {}",
            crate::process::format_timeout(timeout)
        ))),
        Err(e) => Err(CompiledEvalError::Message(format!(
            "cannot execute compiled program: {e}"
        ))),
    }
}

/// Dispatch to native or WASM execution depending on the requested target.
///
/// When `target` is `None` (or a non-WASM triple), falls through to the
/// existing native `run_inprocess_compiled` path.  When `target` resolves to a
/// WASM target, the program is compiled to a `.wasm` module and executed via
/// `wasmtime` with captured output.
fn run_eval_compiled(
    program: hew_parser::ast::Program,
    source: &str,
    source_label: &str,
    timeout: Duration,
    project_dir: Option<PathBuf>,
    target: Option<&str>,
) -> Result<String, CompiledEvalError> {
    let is_wasm = target.is_some_and(|t| {
        crate::target::TargetSpec::from_requested(Some(t)).is_ok_and(|spec| spec.is_wasm())
    });

    if is_wasm {
        run_wasm_eval_compiled(program, source, source_label, timeout, project_dir, target)
    } else {
        run_inprocess_compiled(program, source, source_label, timeout, project_dir, target)
    }
}

/// Compile to a WASM module and execute it via wasmtime, capturing stdout.
fn run_wasm_eval_compiled(
    program: hew_parser::ast::Program,
    source: &str,
    source_label: &str,
    timeout: Duration,
    project_dir: Option<PathBuf>,
    target: Option<&str>,
) -> Result<String, CompiledEvalError> {
    let tmp_dir = tempfile::tempdir()
        .map_err(|e| CompiledEvalError::Message(format!("cannot create temp dir: {e}")))?;

    let module_path = tmp_dir.path().join("eval_module.wasm");
    let module_path_str = module_path
        .to_str()
        .ok_or_else(|| CompiledEvalError::Message("temp module path is not valid UTF-8".into()))?;

    crate::compile::compile_from_source_checked(
        program,
        source,
        source_label,
        module_path_str,
        &crate::compile::CompileOptions {
            project_dir,
            target: target.map(str::to_owned),
            ..crate::compile::CompileOptions::default()
        },
    )
    .map_err(CompiledEvalError::from)?;

    match crate::wasi_runner::run_module_captured(&module_path, timeout) {
        Ok(crate::wasi_runner::WasiCapturedOutcome::Success { stdout }) => Ok(stdout),
        Ok(crate::wasi_runner::WasiCapturedOutcome::Failed {
            stdout,
            stderr,
            exit_code,
        }) => Err(CompiledEvalError::RuntimeFailure {
            stdout,
            stderr,
            exit_code,
        }),
        Ok(crate::wasi_runner::WasiCapturedOutcome::Timeout) => {
            Err(CompiledEvalError::Message(format!(
                "evaluation timed out after {}",
                crate::process::format_timeout(timeout)
            )))
        }
        Err(e) => Err(CompiledEvalError::Message(format!(
            "cannot execute WASM module: {e}"
        ))),
    }
}

/// Run the interactive REPL with a custom execution timeout.
///
/// # Errors
///
/// Returns an error if readline fails fatally.
pub fn run_interactive(
    timeout: Duration,
    target: Option<&str>,
) -> Result<(), Box<dyn std::error::Error>> {
    let mut rl = rustyline::DefaultEditor::new()?;
    let mut session = ReplSession::with_timeout_and_target(timeout, target);

    println!("Hew REPL v{}", env!("CARGO_PKG_VERSION"));
    println!("Type :help for commands, :session to inspect state, :quit to exit.\n");

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

        // Multi-line: keep reading while the parser says the input is incomplete.
        while matches!(
            classify::input_completeness(&input),
            InputCompleteness::Incomplete
        ) {
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
pub fn eval_one(
    expr: &str,
    timeout: Duration,
    target: Option<&str>,
) -> Result<String, CliEvalError> {
    let mut session = ReplSession::with_timeout_and_target(timeout, target);
    session.eval_cli(expr, "<eval>")
}

/// Evaluate a file in REPL context.
///
/// # Errors
///
/// Returns an error string if evaluation fails.
pub fn eval_file(
    path: &str,
    timeout: Duration,
    target: Option<&str>,
) -> Result<String, CliEvalError> {
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

    let mut session = if path == "-" {
        ReplSession::with_timeout_and_target(timeout, target)
    } else {
        ReplSession::for_path_with_target(path, timeout, target)
    };
    session.eval_source_file_cli(&source, &input_name, &input_name)
}

/// Evaluate a file in REPL context, returning its stdout as a `String`.
///
/// Used by `hew playground verify` to compare program output against a
/// checked-in `.expected` file without mixing verification output with
/// program output.  Unlike [`eval_file`] this does not accept `"-"` for
/// stdin — playground entries are always on-disk files.
///
/// # Errors
///
/// Returns a [`CliEvalError`] if the file cannot be read, fails to compile,
/// or exits with a non-zero status.
pub fn eval_file_captured(
    path: &str,
    timeout: Duration,
    target: Option<&str>,
) -> Result<String, CliEvalError> {
    let source = std::fs::read_to_string(path)
        .map_err(|e| CliEvalError::Message(format!("cannot read '{path}': {e}")))?;
    let input_name = path.to_string();

    let mut session = ReplSession::for_path_with_target(path, timeout, target);
    session.eval_source_file_cli(&source, &input_name, &input_name)
}

fn help_text() -> &'static str {
    "\
Commands:
  :help, :h         Show this help message
  :session, :show   Summarize remembered session state
  :items            List remembered top-level items
  :bindings         List persistent let/var bindings
  :quit, :q         Exit the REPL
  :clear, :reset    Reset session (clear all definitions)
  :type <expr>      Show the inferred type of an expression
  :load <file>      Load a .hew file into the session

Input types:
  fn, struct, ...   Top-level items are remembered across evaluations
  let x = ...;      Bindings persist in the session
  <expression>      Bare expressions are evaluated and printed
"
}

fn session_count_delta(before: SessionCounts, after: SessionCounts) -> SessionCounts {
    SessionCounts {
        items: after.items.saturating_sub(before.items),
        bindings: after.bindings.saturating_sub(before.bindings),
    }
}

fn describe_load_result(added: SessionCounts) -> String {
    if added.items == 0 && added.bindings == 0 {
        "no persistent session changes".to_string()
    } else {
        format!("added {}", describe_session_entries(added))
    }
}

fn describe_clear_result(removed: SessionCounts) -> String {
    if removed.items == 0 && removed.bindings == 0 {
        "Session cleared.\n".to_string()
    } else {
        format!(
            "Session cleared.\nRemoved {}.\n",
            describe_session_entries(removed)
        )
    }
}

fn describe_session_entries(counts: SessionCounts) -> String {
    let mut parts = Vec::new();
    if counts.items > 0 {
        parts.push(pluralize_session_entry(counts.items, "item"));
    }
    if counts.bindings > 0 {
        parts.push(pluralize_session_entry(counts.bindings, "binding"));
    }

    if parts.is_empty() {
        "no session entries".to_string()
    } else {
        parts.join(", ")
    }
}

fn pluralize_session_entry(count: usize, singular: &str) -> String {
    if count == 1 {
        format!("1 {singular}")
    } else {
        format!("{count} {singular}s")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[cfg(unix)]
    fn capture_stderr<T>(f: impl FnOnce() -> T) -> (T, String) {
        use std::fs::File;
        use std::io::Write;
        use std::os::fd::{AsRawFd, FromRawFd};

        // SAFETY: We temporarily redirect the process stderr fd to a pipe,
        // restore it before returning, and only use valid file descriptors
        // created by `pipe`/`dup`.
        unsafe {
            let mut pipe_fds = [0; 2];
            assert_eq!(libc::pipe(pipe_fds.as_mut_ptr()), 0, "pipe failed");

            let stderr_fd = std::io::stderr().as_raw_fd();
            let saved_stderr = libc::dup(stderr_fd);
            assert!(saved_stderr >= 0, "dup failed");
            assert_eq!(libc::dup2(pipe_fds[1], stderr_fd), stderr_fd, "dup2 failed");
            libc::close(pipe_fds[1]);

            let result = f();

            std::io::stderr().flush().expect("flush stderr");
            assert_eq!(
                libc::dup2(saved_stderr, stderr_fd),
                stderr_fd,
                "restore dup2 failed"
            );
            libc::close(saved_stderr);

            let mut reader = File::from_raw_fd(pipe_fds[0]);
            let mut captured = String::new();
            reader
                .read_to_string(&mut captured)
                .expect("read captured stderr");

            (result, captured)
        }
    }

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
        assert_eq!(r.output, "Session cleared.\nRemoved 1 binding.\n");
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
        assert!(result.output.contains(":session"));
        assert!(result.output.contains(":bindings"));
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
        let result = eval_one("2 * 3", DEFAULT_EVAL_TIMEOUT, None);
        assert_eq!(result.unwrap(), "6\n");
    }

    #[test]
    fn eval_timeout_is_reported() {
        if !require_toolchain() {
            return;
        }
        let mut session = ReplSession::with_timeout(Duration::from_millis(100));
        let define = session
            .eval("fn spin_forever() {\n    var i = 0;\n    loop {\n        i = i + 1;\n    }\n}");
        assert!(!define.had_errors, "errors: {:?}", define.errors);

        let result = session.eval("spin_forever()");
        assert!(result.had_errors);
        assert!(result.errors[0].contains("evaluation timed out after 100ms"));
    }

    #[test]
    fn native_runtime_failure_normalizes_captured_stderr() {
        assert_eq!(
            normalize_captured_output("line1\r\nline2\r\n"),
            "line1\nline2\n"
        );
    }

    #[cfg(unix)]
    #[test]
    fn eval_runtime_failure_still_surfaces_stderr() {
        if !require_toolchain() {
            return;
        }

        let mut session = ReplSession::new();
        let (result, stderr) = capture_stderr(|| session.eval(r#"panic("eval stderr")"#));

        assert!(result.had_errors);
        assert_eq!(result.output, "");
        assert!(
            result.errors[0].contains("program exited with status 101"),
            "unexpected errors: {:?}",
            result.errors
        );
        assert!(
            stderr.contains("eval stderr"),
            "expected runtime stderr to remain visible, got: {stderr:?}"
        );
    }

    #[test]
    fn interactive_invalid_input_uses_shared_diagnostics_path() {
        let mut session = ReplSession::new();
        assert_eq!(
            handle_interactive_input(&mut session, "1 + *"),
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
    fn remap_type_span_excludes_end_boundary() {
        let diagnostic_view = SyntheticDiagnosticView {
            source: "1 + \"x\"".to_string(),
            input_span: 12..19,
        };

        assert_eq!(remap_type_span(&diagnostic_view, &(18..19)), Some(6..7));
        assert_eq!(remap_type_span(&diagnostic_view, &(19..20)), None);
    }

    #[test]
    fn split_eval_type_diagnostics_keeps_non_root_attribution() {
        let diagnostic_view = SyntheticDiagnosticView {
            source: "1 + \"x\"".to_string(),
            input_span: 12..19,
        };
        let root_diagnostic = hew_types::TypeError::new(
            hew_types::error::TypeErrorKind::InvalidOperation,
            12..19,
            "cannot apply `+` to `int` and `String`",
        );
        let dep_diagnostic = hew_types::TypeError::new(
            hew_types::error::TypeErrorKind::ReturnTypeMismatch,
            0..4,
            "return type mismatch: expected `int`, found `bool`",
        )
        .with_source_module("dep");

        let (remapped, original) = split_eval_type_diagnostics(
            Some(&diagnostic_view),
            &[root_diagnostic, dep_diagnostic.clone()],
        );

        assert_eq!(remapped.len(), 1);
        assert_eq!(remapped[0].span, 0..7);
        assert_eq!(original.len(), 1);
        assert_eq!(original[0].span, dep_diagnostic.span);
        assert_eq!(
            original[0].message.as_str(),
            dep_diagnostic.message.as_str()
        );
        assert_eq!(
            original[0].source_module.as_deref(),
            dep_diagnostic.source_module.as_deref()
        );
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
        let result = eval_file(path.to_str().unwrap(), DEFAULT_EVAL_TIMEOUT, None);
        assert!(result.is_ok(), "eval_file failed: {result:?}");
    }

    #[test]
    fn eval_file_balanced_incomplete_expression() {
        if !require_toolchain() {
            return;
        }
        let dir = tempfile::tempdir().expect("temp dir");
        let path = dir.path().join("hew_eval_balanced_incomplete_expr.hew");
        std::fs::write(&path, "1 +\n2\n").unwrap();

        let result = eval_file(path.to_str().unwrap(), DEFAULT_EVAL_TIMEOUT, None);
        assert!(result.is_ok(), "eval_file failed: {result:?}");
    }

    #[test]
    fn eval_session_command_reports_counts() {
        let mut session = ReplSession::new();
        let result = session.eval(":session");
        assert!(!result.had_errors);
        assert!(
            result.output.contains("Session state:"),
            "output: {}",
            result.output
        );
        assert!(
            result.output.contains("0 remembered items"),
            "output: {}",
            result.output
        );
        assert!(
            result.output.contains("0 persistent bindings"),
            "output: {}",
            result.output
        );
    }

    #[test]
    fn eval_items_and_bindings_commands_list_entries() {
        let mut session = ReplSession::new();
        session.session.add_item("async fn answer() -> i64 { 42 }");
        session
            .session
            .add_binding("let (left, right) = pair();\nvar total = 0;");

        let items = session.eval(":items");
        assert!(!items.had_errors);
        assert!(items.output.contains("Remembered items (1):"));
        assert!(items.output.contains("async fn answer"));

        let bindings = session.eval(":bindings");
        assert!(!bindings.had_errors);
        assert!(bindings.output.contains("Persistent bindings (2):"));
        assert!(bindings.output.contains("let left, right"));
        assert!(bindings.output.contains("var total"));
    }

    #[test]
    fn eval_reset_alias_clears_session() {
        let mut session = ReplSession::new();
        session.session.add_binding("let value = 1;");

        let cleared = session.eval(":reset");
        assert_eq!(cleared.output, "Session cleared.\nRemoved 1 binding.\n");
        let overview = session.eval(":session");
        assert!(overview.output.contains("0 persistent bindings"));
    }

    // -----------------------------------------------------------------------
    // Project-context parity tests
    //
    // These verify that eval flows pick up project dir / manifest context the
    // same way `compile_file` does.  Each test creates a temporary project
    // directory tree and exercises the path that previously used
    // `project_context_for_program` with no manifest.
    // -----------------------------------------------------------------------

    /// `eval_file` anchored to a real path resolves local `src/` imports that
    /// would fail when `project_dir` defaults to an unrelated cwd.
    #[test]
    fn eval_file_resolves_local_src_import() {
        if !require_toolchain() {
            return;
        }

        // Build a minimal project tree:
        //   <tmp>/
        //     src/
        //       greet.hew   -- module with a pub fn
        //     main.hew      -- imports from greet and calls the fn
        let dir = tempfile::tempdir().expect("create temp dir");
        let src_dir = dir.path().join("src");
        std::fs::create_dir_all(&src_dir).expect("create src dir");

        std::fs::write(
            src_dir.join("greet.hew"),
            "pub fn hello() -> str { \"hi from greet\" }\n",
        )
        .expect("write greet.hew");

        let main_path = dir.path().join("main.hew");
        std::fs::write(
            &main_path,
            "import local \"greet\";\nfn main() { println(local.hello()); }\n",
        )
        .expect("write main.hew");

        let result = eval_file(
            main_path.to_str().expect("main path is valid UTF-8"),
            DEFAULT_EVAL_TIMEOUT,
            None,
        );
        assert!(
            result.is_ok(),
            "eval_file with local import failed: {result:?}"
        );
    }

    /// `ReplSession::for_path` carries the project directory so that
    /// `eval_source_file_cli` resolves imports relative to that project.
    #[test]
    fn repl_session_for_path_carries_project_dir() {
        if !require_toolchain() {
            return;
        }

        let dir = tempfile::tempdir().expect("create temp dir");
        let src_dir = dir.path().join("src");
        std::fs::create_dir_all(&src_dir).expect("create src dir");

        std::fs::write(
            src_dir.join("math.hew"),
            "pub fn square(n: i64) -> i64 { n * n }\n",
        )
        .expect("write math.hew");

        let main_path = dir.path().join("prog.hew");
        std::fs::write(
            &main_path,
            "import local \"math\";\nfn main() { println(local.square(4)); }\n",
        )
        .expect("write prog.hew");

        let main_path_str = main_path.to_str().expect("path is valid UTF-8");
        let timeout = DEFAULT_EVAL_TIMEOUT;
        let source = std::fs::read_to_string(main_path_str).expect("read prog.hew");

        let mut session = ReplSession::for_path(main_path_str, timeout);
        let result = session.eval_source_file_cli(&source, main_path_str, main_path_str);
        assert!(
            result.is_ok(),
            "eval via for_path session failed: {result:?}"
        );
    }

    /// Verifies that `FrontendOptions::project_dir` is forwarded through the
    /// in-memory compile pipeline.  A `compile_program` call with `project_dir`
    /// set to a real project tree must see manifest deps; previously it would
    /// always get `manifest_deps: None`.
    #[test]
    fn compile_program_inherits_project_dir_manifest_deps() {
        use std::io::Write;

        let dir = tempfile::tempdir().expect("create temp dir");
        let manifest = "[package]\nname = \"myapp\"\n\n[dependencies]\nstdlib = \"*\"\n";
        let mut f = std::fs::File::create(dir.path().join("hew.toml")).expect("create hew.toml");
        f.write_all(manifest.as_bytes()).expect("write hew.toml");

        let options = hew_compile::FrontendOptions {
            project_dir: Some(dir.path().to_path_buf()),
            ..Default::default()
        };

        // A trivial program with no imports — it just needs to compile cleanly
        // so we can confirm the project context is wired in without an import
        // resolution error.
        let source = "fn main() { println(\"ok\"); }\n";
        let parse_result = hew_parser::parse(source);
        assert!(
            parse_result.errors.is_empty(),
            "parse errors: {:?}",
            parse_result.errors
        );
        // compile_program must not error; previously it would ignore project_dir.
        // With the fix, manifest_deps is loaded from the temp dir.
        let result = hew_compile::compile_program(parse_result.program, source, "<test>", &options);
        assert!(
            result.is_ok(),
            "compile_program with project_dir failed: {result:?}"
        );
    }

    // ── WASI target test helpers ─────────────────────────────────────────────

    /// Returns `true` if the WASI toolchain (codegen + wasmtime) is available
    /// and produces correct output.  Skips WASI tests gracefully when either
    /// wasmtime is missing or the WASM runtime library is not built.
    ///
    /// Uses `wasi_runner::find_wasmtime()` — the same lookup used at runtime —
    /// so that tests skip iff the feature would also fail at runtime (not just
    /// when `wasmtime` happens to be absent from `PATH` while still reachable
    /// via the `~/.wasmtime/bin/` fallback).
    fn require_wasi_toolchain() -> bool {
        static OK: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
        *OK.get_or_init(|| {
            // Use the same wasmtime locator as production code so the test skip
            // decision is consistent with whether eval would actually succeed.
            if crate::wasi_runner::find_wasmtime().is_none() {
                eprintln!(
                    "WASI eval tests skipped: wasmtime not found \
                     (checked PATH and ~/.wasmtime/bin/)"
                );
                return false;
            }

            // Compile a probe and run it — this validates that libhew_runtime.a
            // for wasm32-wasip1 is built and linked (--allow-undefined means a
            // link without the runtime still succeeds but produces silent output).
            let dir = tempfile::tempdir().expect("temp dir");
            let wasm_path = dir.path().join("probe.wasm");
            let source = "fn main() { println(\"wasi-probe-ok\"); }\n";
            let parse_result = hew_parser::parse(source);
            if !parse_result.errors.is_empty() {
                eprintln!("WASI eval tests skipped: probe parse failed");
                return false;
            }
            let compiled = crate::compile::compile_from_source_checked(
                parse_result.program,
                source,
                "<wasi-probe>",
                wasm_path.to_str().unwrap_or("probe.wasm"),
                &crate::compile::CompileOptions {
                    target: Some("wasm32-wasi".to_owned()),
                    ..crate::compile::CompileOptions::default()
                },
            );
            if compiled.is_err() {
                eprintln!(
                    "WASI eval tests skipped: wasm32-wasi compile failed \
                     (codegen/wasm toolchain not available)"
                );
                return false;
            }

            // Run the probe and verify it prints the expected output.
            match crate::wasi_runner::run_module_captured(&wasm_path, DEFAULT_EVAL_TIMEOUT) {
                Ok(crate::wasi_runner::WasiCapturedOutcome::Success { stdout })
                    if stdout.trim() == "wasi-probe-ok" =>
                {
                    true
                }
                other => {
                    eprintln!(
                        "WASI eval tests skipped: probe run produced unexpected output \
                         (libhew_runtime.a for wasm32-wasip1 may not be built): {other:?}"
                    );
                    false
                }
            }
        })
    }

    // ── WASI inline expression ───────────────────────────────────────────────

    #[test]
    fn wasi_eval_arithmetic() {
        if !require_wasi_toolchain() {
            return;
        }
        let result = eval_one("1 + 2", DEFAULT_EVAL_TIMEOUT, Some("wasm32-wasi"));
        assert_eq!(result.unwrap(), "3\n");
    }

    #[test]
    fn wasi_eval_string_output() {
        if !require_wasi_toolchain() {
            return;
        }
        let result = eval_one(
            r#"println("hello from wasi")"#,
            DEFAULT_EVAL_TIMEOUT,
            Some("wasm32-wasi"),
        );
        assert_eq!(result.unwrap(), "hello from wasi\n");
    }

    // ── WASI parse / type error surfaces as DiagnosticsRendered ─────────────

    #[test]
    fn wasi_eval_parse_error_surfaces() {
        let mut session =
            ReplSession::with_timeout_and_target(DEFAULT_EVAL_TIMEOUT, Some("wasm32-wasi"));
        let result = session.eval_cli("fn {", "<eval>");
        assert!(
            matches!(result, Err(CliEvalError::DiagnosticsRendered)),
            "expected DiagnosticsRendered, got {result:?}"
        );
    }

    #[test]
    fn wasi_eval_type_error_surfaces() {
        let mut session =
            ReplSession::with_timeout_and_target(DEFAULT_EVAL_TIMEOUT, Some("wasm32-wasi"));
        let result = session.eval_cli("let x: i64 = \"oops\";", "<eval>");
        assert!(
            matches!(result, Err(CliEvalError::DiagnosticsRendered)),
            "expected DiagnosticsRendered, got {result:?}"
        );
    }

    // ── WASI file eval ───────────────────────────────────────────────────────

    #[test]
    fn wasi_eval_file_function_and_call() {
        if !require_wasi_toolchain() {
            return;
        }
        let dir = tempfile::tempdir().expect("temp dir");
        let path = dir.path().join("wasi_eval_test.hew");
        std::fs::write(
            &path,
            "fn add(a: i64, b: i64) -> i64 {\n    a + b\n}\n\nadd(10, 32)\n",
        )
        .unwrap();
        let result = eval_file(
            path.to_str().unwrap(),
            DEFAULT_EVAL_TIMEOUT,
            Some("wasm32-wasi"),
        );
        assert!(result.is_ok(), "wasi eval_file failed: {result:?}");
    }
}
