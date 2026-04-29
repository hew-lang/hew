//! Interactive REPL for the Hew programming language.
//!
//! Provides `hew eval` — an interactive read-eval-print loop that compiles
//! each input via the native compilation pipeline. Users can type Hew
//! expressions, statements, and definitions interactively, with results
//! printed immediately.
//!
//! # Architecture
//!
//! Each evaluation builds a synthetic Hew program from accumulated session
//! state (prior items and bindings) plus the new input, runs the in-process
//! compile pipeline to produce a native binary, and captures its stdout output.

pub mod classify;
pub mod repl;
pub mod session;

#[cfg(test)]
mod type_tests;

// ---------------------------------------------------------------------------
// JSON run contract
// ---------------------------------------------------------------------------

/// Machine-readable result of a single `hew eval --json` invocation.
///
/// Emitted as a single JSON object on stdout when `--json` is passed.
/// The schema is stable within the `0.x` series; new optional fields may be
/// added without bumping the major version.
///
/// # Fields
///
/// - `status`      — `"ok"`, `"compile_error"`, or `"runtime_failure"`
/// - `stdout`      — program output captured from the child process (empty
///   string when the program produced no output or when a compile error
///   prevented execution)
/// - `stderr`      — runtime stderr captured from the child process (empty
///   string unless `status == "runtime_failure"`)
/// - `exit_code`   — child exit code; `0` on success or compile error,
///   non-zero on runtime failure
/// - `diagnostics` — compiler diagnostic text (non-empty only when
///   `status == "compile_error"`; empty string otherwise)
#[derive(Debug, serde::Serialize)]
pub struct EvalJsonOutput {
    pub status: EvalStatus,
    pub stdout: String,
    pub stderr: String,
    pub exit_code: i32,
    pub diagnostics: String,
}

/// Outcome category for [`EvalJsonOutput`].
#[derive(Debug, serde::Serialize)]
#[serde(rename_all = "snake_case")]
pub enum EvalStatus {
    Ok,
    CompileError,
    RuntimeFailure,
}

/// Resolve and validate an optional `--target` string for `hew eval`.
///
/// Returns `Ok(None)` when no target was supplied (native path).
/// Returns `Ok(Some(spec))` for the exact supported WASI targets
/// (`wasm32-wasi` and `wasm32-wasip1` both normalize to `wasm32-wasip1`).
/// Returns `Err(message)` for non-WASM explicit targets **and** wasi-shaped
/// triples that do not normalize to `wasm32-wasip1` (e.g.
/// `wasm32-unknown-unknown-wasi`).
fn resolve_eval_target(
    triple: Option<&str>,
) -> Result<Option<crate::target::ExecutionTarget>, String> {
    let target = match triple {
        None => return Ok(None),
        Some(t) => crate::target::ExecutionTarget::from_requested(Some(t))?,
    };
    // Only the canonical wasm32-wasip1 target (which wasm32-wasi also
    // normalizes to) is supported for eval.
    // Other wasi-shaped triples like `wasm32-unknown-unknown-wasi` parse as
    // WASM but do not normalize to wasm32-wasip1 and must be rejected.
    if target.is_wasi() && target.normalized_triple() == "wasm32-wasip1" {
        Ok(Some(target))
    } else {
        Err(format!(
            "`hew eval --target {}` is not supported. \
             Only `--target wasm32-wasi` is accepted; omit --target for native eval.",
            target.normalized_triple()
        ))
    }
}

/// Run the `hew eval` subcommand.
pub fn cmd_eval(args: &crate::args::EvalArgs) {
    let timeout = resolve_eval_timeout(&args.timeout);
    let target_spec = resolve_eval_target(args.target.as_deref()).unwrap_or_else(|e| {
        eprintln!("Error: {e}");
        std::process::exit(1);
    });
    let request = EvalRequest::from_args(args);
    let target = eval_target_arg(args.target.as_deref(), target_spec.as_ref());

    validate_eval_request(&request, args.json, target);

    if args.json {
        emit_json_eval(&request, timeout, target, args.jit);
        return;
    }

    execute_eval_request(&request, timeout, target, args.jit);
}

fn resolve_eval_timeout(raw: &str) -> std::time::Duration {
    crate::util::parse_timeout(raw).unwrap_or_else(|e| {
        eprintln!("Error: {e}");
        std::process::exit(1);
    })
}

enum EvalRequest {
    File(String),
    Expr(String),
    Repl,
}

impl EvalRequest {
    fn from_args(args: &crate::args::EvalArgs) -> Self {
        if let Some(file) = &args.file {
            Self::File(file.display().to_string())
        } else if args.expr.is_empty() {
            Self::Repl
        } else {
            Self::Expr(args.expr.join(" "))
        }
    }

    fn is_repl(&self) -> bool {
        matches!(self, Self::Repl)
    }
}

fn eval_target_arg<'a>(
    requested_target: Option<&'a str>,
    target_spec: Option<&crate::target::ExecutionTarget>,
) -> Option<&'a str> {
    target_spec.map(|_| requested_target.unwrap_or("wasm32-wasi"))
}

fn validate_eval_request(request: &EvalRequest, json: bool, target: Option<&str>) {
    if json && request.is_repl() {
        eprintln!(
            "Error: --json requires -f <file> or an inline expression; it cannot be used with the interactive REPL."
        );
        std::process::exit(1);
    }

    if let Some(target) = target.filter(|_| request.is_repl()) {
        eprintln!(
            "Error: interactive REPL is not supported for --target {target}. \
             Provide an inline expression or use -f <file>.",
        );
        std::process::exit(1);
    }
}

fn emit_json_eval(
    request: &EvalRequest,
    timeout: std::time::Duration,
    target: Option<&str>,
    jit: Option<crate::args::JitMode>,
) {
    let result = match request {
        EvalRequest::File(path) => {
            capture_json_eval(|| repl::eval_file(path, timeout, target, jit))
        }
        EvalRequest::Expr(expr) => capture_json_eval(|| repl::eval_one(expr, timeout, target, jit)),
        EvalRequest::Repl => unreachable!("validated before JSON evaluation"),
    };

    // Always exit 0 when --json is active: the structured `status` field
    // carries the outcome; callers must not rely on the process exit code.
    println!(
        "{}",
        serde_json::to_string(&result).expect("JSON serialization is infallible")
    );
}

fn capture_json_eval<F>(run: F) -> EvalJsonOutput
where
    F: FnOnce() -> Result<String, repl::CliEvalError>,
{
    crate::diagnostic::start_diagnostic_capture();
    let result = run();
    let diagnostics = crate::diagnostic::finish_diagnostic_capture();
    eval_result_to_json(result, diagnostics)
}

fn execute_eval_request(
    request: &EvalRequest,
    timeout: std::time::Duration,
    target: Option<&str>,
    jit: Option<crate::args::JitMode>,
) {
    match request {
        EvalRequest::File(path) => emit_eval_output(repl::eval_file(path, timeout, target, jit)),
        EvalRequest::Expr(expr) => emit_eval_output(repl::eval_one(expr, timeout, target, jit)),
        EvalRequest::Repl => {
            if let Err(e) = repl::run_interactive(timeout, target) {
                eprintln!("Error: {e}");
                std::process::exit(1);
            }
        }
    }
}

fn emit_eval_output(result: Result<String, repl::CliEvalError>) {
    match result {
        Ok(output) => {
            if !output.is_empty() {
                print!("{output}");
            }
        }
        Err(e) => exit_eval_error(e),
    }
}

/// Convert a raw eval result into an [`EvalJsonOutput`].
///
/// `diagnostics` is the text captured via
/// [`crate::diagnostic::start_diagnostic_capture`] during the eval call; it
/// is non-empty only when a compile error was rendered.
fn eval_result_to_json(
    result: Result<String, repl::CliEvalError>,
    diagnostics: String,
) -> EvalJsonOutput {
    match result {
        Ok(stdout) => EvalJsonOutput {
            status: EvalStatus::Ok,
            stdout,
            stderr: String::new(),
            exit_code: 0,
            diagnostics: String::new(),
        },
        Err(repl::CliEvalError::RuntimeFailure {
            stdout,
            stderr,
            exit_code,
        }) => EvalJsonOutput {
            status: EvalStatus::RuntimeFailure,
            stdout,
            stderr,
            exit_code,
            diagnostics: String::new(),
        },
        Err(repl::CliEvalError::DiagnosticsRendered) => EvalJsonOutput {
            status: EvalStatus::CompileError,
            stdout: String::new(),
            stderr: String::new(),
            exit_code: 0,
            diagnostics,
        },
        Err(repl::CliEvalError::Message(message)) => EvalJsonOutput {
            status: EvalStatus::CompileError,
            stdout: String::new(),
            stderr: String::new(),
            exit_code: 0,
            diagnostics: message,
        },
    }
}

fn exit_eval_error(error: repl::CliEvalError) -> ! {
    match error {
        repl::CliEvalError::Message(message) => {
            eprintln!("Error: {message}");
            std::process::exit(1);
        }
        repl::CliEvalError::RuntimeFailure {
            stdout,
            stderr,
            exit_code,
        } => {
            // Surface any output the program produced before it failed, then
            // exit with the child's own exit code so callers can observe it.
            repl::emit_runtime_failure_output(&stdout, &stderr);
            std::process::exit(exit_code);
        }
        repl::CliEvalError::DiagnosticsRendered => {
            std::process::exit(1);
        }
    }
}

#[cfg(test)]
mod target_validation_tests {
    use super::resolve_eval_target;

    #[test]
    fn no_target_is_native() {
        assert!(resolve_eval_target(None).unwrap().is_none());
    }

    #[test]
    fn wasm32_wasi_is_accepted() {
        let spec = resolve_eval_target(Some("wasm32-wasi"))
            .expect("wasm32-wasi should be accepted")
            .expect("should return Some");
        assert!(spec.is_wasi());
    }

    #[test]
    fn wasm32_wasip1_is_accepted() {
        let spec = resolve_eval_target(Some("wasm32-wasip1"))
            .expect("wasm32-wasip1 should be accepted")
            .expect("should return Some");
        assert!(spec.is_wasi());
    }

    #[test]
    fn native_cross_target_is_rejected() {
        let err = resolve_eval_target(Some("x86_64-unknown-linux-gnu"))
            .expect_err("native cross-target should be rejected");
        assert!(
            err.contains("not supported"),
            "expected 'not supported' in error: {err}"
        );
        assert!(
            err.contains("x86_64"),
            "expected target triple in error: {err}"
        );
    }

    #[test]
    fn aarch64_apple_darwin_is_rejected() {
        let err = resolve_eval_target(Some("aarch64-apple-darwin"))
            .expect_err("native target should be rejected");
        assert!(
            err.contains("not supported"),
            "expected 'not supported' in error: {err}"
        );
    }

    #[test]
    fn wasi_like_but_unsupported_triple_is_rejected() {
        // wasm32-unknown-unknown-wasi parses as WASM (contains "wasi") but
        // does not normalize to wasm32-wasip1, so it must be rejected up front
        // rather than silently falling through to a broken eval path.
        let err = resolve_eval_target(Some("wasm32-unknown-unknown-wasi"))
            .expect_err("unsupported wasi-like triple should be rejected");
        assert!(
            err.contains("not supported"),
            "expected 'not supported' in error: {err}"
        );
    }
}
