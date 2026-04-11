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

    // Check for `-f <file>` flag first.
    if let Some(ref file) = args.file {
        let path = file.display().to_string();
        if let Err(e) = repl::eval_file(&path, timeout, target) {
            exit_eval_error(e);
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

fn exit_eval_error(error: repl::CliEvalError) -> ! {
    match error {
        repl::CliEvalError::Message(message) => {
            eprintln!("Error: {message}");
            std::process::exit(1);
        }
        repl::CliEvalError::RuntimeFailure { stdout, exit_code } => {
            // Surface any output the program produced before it failed, then
            // exit with the child's own exit code so callers can observe it.
            if !stdout.is_empty() {
                print!("{stdout}");
            }
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
