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
fn resolve_eval_target(triple: Option<&str>) -> Result<Option<crate::target::TargetSpec>, String> {
    let spec = match triple {
        None => return Ok(None),
        Some(t) => crate::target::TargetSpec::from_requested(Some(t))?,
    };
    // Only the canonical wasm32-wasip1 target (which wasm32-wasi also
    // normalizes to) is supported for eval.
    // Other wasi-shaped triples like `wasm32-unknown-unknown-wasi` parse as
    // WASM but do not normalize to wasm32-wasip1 and must be rejected.
    if spec.is_wasm() && spec.normalized_triple() == "wasm32-wasip1" {
        Ok(Some(spec))
    } else {
        Err(format!(
            "`hew eval --target {}` is not supported. \
             Only `--target wasm32-wasi` is accepted; omit --target for native eval.",
            spec.normalized_triple()
        ))
    }
}

/// Run the `hew eval` subcommand.
pub fn cmd_eval(args: &crate::args::EvalArgs) {
    let timeout = crate::process::timeout_from_seconds(args.timeout).unwrap_or_else(|e| {
        eprintln!("Error: {e}");
        std::process::exit(1);
    });

    // Validate the target up front.  Only wasm32-wasi / wasm32-wasip1 are
    // accepted as explicit targets; any other explicit triple is rejected with
    // a clear diagnostic.
    let target_spec = resolve_eval_target(args.target.as_deref()).unwrap_or_else(|e| {
        eprintln!("Error: {e}");
        std::process::exit(1);
    });
    let target = target_spec
        .as_ref()
        .map(|_| args.target.as_deref().unwrap_or("wasm32-wasi"));

    // --json requires a non-interactive invocation.
    if args.json && args.file.is_none() && args.expr.is_empty() {
        eprintln!("Error: --json requires -f <file> or an inline expression; it cannot be used with the interactive REPL.");
        std::process::exit(1);
    }

    // Interactive REPL is not supported for explicit WASI targets.
    // Each WASI execution is compile-per-input via wasmtime; a persistent
    // session loop is intentionally out of scope for this bounded lane.
    if target_spec.is_some() && args.file.is_none() && args.expr.is_empty() {
        eprintln!(
            "Error: interactive REPL is not supported for --target {}. \
             Provide an inline expression or use -f <file>.",
            args.target.as_deref().unwrap_or("wasm32-wasi")
        );
        std::process::exit(1);
    }

    // --json mode: collect result into a structured contract and emit as JSON.
    if args.json {
        let result = if let Some(ref file) = args.file {
            let path = file.display().to_string();
            crate::diagnostic::start_diagnostic_capture();
            let outcome = repl::eval_file(&path, timeout, target);
            let diagnostics = crate::diagnostic::finish_diagnostic_capture();
            eval_result_to_json(outcome, diagnostics)
        } else {
            let expr = args.expr.join(" ");
            crate::diagnostic::start_diagnostic_capture();
            let outcome = repl::eval_one(&expr, timeout, target);
            let diagnostics = crate::diagnostic::finish_diagnostic_capture();
            eval_result_to_json(outcome, diagnostics)
        };
        // Always exit 0 when --json is active: the structured `status` field
        // carries the outcome; callers must not rely on the process exit code.
        println!(
            "{}",
            serde_json::to_string(&result).expect("JSON serialization is infallible")
        );
        return;
    }

    // Check for `-f <file>` flag first.
    if let Some(ref file) = args.file {
        let path = file.display().to_string();
        match repl::eval_file(&path, timeout, target) {
            Ok(output) => {
                if !output.is_empty() {
                    print!("{output}");
                }
            }
            Err(e) => exit_eval_error(e),
        }
        return;
    }

    if args.expr.is_empty() {
        // Interactive REPL.
        if let Err(e) = repl::run_interactive(timeout, target) {
            eprintln!("Error: {e}");
            std::process::exit(1);
        }
        return;
    }

    // Evaluate inline expression.
    let expr = args.expr.join(" ");
    match repl::eval_one(&expr, timeout, target) {
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
        assert!(spec.is_wasm());
    }

    #[test]
    fn wasm32_wasip1_is_accepted() {
        let spec = resolve_eval_target(Some("wasm32-wasip1"))
            .expect("wasm32-wasip1 should be accepted")
            .expect("should return Some");
        assert!(spec.is_wasm());
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
