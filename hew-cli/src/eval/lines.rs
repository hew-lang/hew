//! A compile-once line harness using ordinary Hew I/O and ownership semantics.

use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::time::Duration;

use super::repl::{self, CliEvalError};
use super::session::SyntheticDiagnosticView;

pub(super) fn eval(
    args: &crate::args::EvalArgs,
    timeout: Duration,
    target: Option<&str>,
) -> Result<String, CliEvalError> {
    let (_directory, binary) = compile(args, target)?;
    run(&binary, args.json, timeout, target.is_some())
}

fn compile(
    args: &crate::args::EvalArgs,
    target: Option<&str>,
) -> Result<(tempfile::TempDir, PathBuf), CliEvalError> {
    let (input, label, project) = input(args).map_err(CliEvalError::Message)?;
    let prefix = "import std.io;\nfn main() {\nwhile let .Some(line) = io.read_line().expect(\"standard input\") {\n";
    let source = format!("{prefix}{input}\n}}\n}}\n");
    let view = SyntheticDiagnosticView {
        source: input.clone(),
        input_span: prefix.len()..prefix.len() + input.len(),
    };
    if args.show_wrapper {
        eprintln!("{source}");
    }
    let target_spec =
        crate::target::TargetSpec::from_requested(target).map_err(CliEvalError::Message)?;
    let options = repl::eval_compile_options(Some(project), target);
    let frontend = crate::compile::frontend_options(&target_spec, &options);
    let mut state = hew_compile::run_source_frontend(&source, &label, &frontend);
    let parsed = state.parse_result.take().expect("source frontend parses");
    repl::render_eval_parse_diagnostics(&source, &label, Some(&view), &parsed.errors);
    if repl::parse_errors_are_fatal(&parsed.errors) {
        return Err(CliEvalError::DiagnosticsRendered);
    }
    if let Some(errors) = repl::frontend_type_errors(&state) {
        repl::render_eval_type_diagnostics(
            &source,
            &label,
            Some(&view),
            errors,
            state
                .typecheck_result
                .as_ref()
                .expect("checked frontend")
                .sources
                .modules(),
        );
        if errors
            .iter()
            .any(|error| error.severity == hew_types::error::Severity::Error)
        {
            return Err(CliEvalError::DiagnosticsRendered);
        }
    }
    let directory =
        tempfile::tempdir().map_err(|error| CliEvalError::Message(error.to_string()))?;
    let binary = directory.path().join(if target.is_some() {
        "lines.wasm".into()
    } else {
        format!("lines{}", crate::platform::exe_suffix())
    });
    crate::compile_native_from_program(parsed.program, &source, &label, &binary, &options)
        .map_err(|_| CliEvalError::DiagnosticsRendered)?;
    Ok((directory, binary))
}

fn run(
    binary: &Path,
    capture: bool,
    timeout: Duration,
    wasi: bool,
) -> Result<String, CliEvalError> {
    let mut command = if wasi {
        let runner = crate::wasi_runner::find_wasmtime().ok_or_else(|| {
            CliEvalError::Message("cannot find wasmtime for WASI line processing".into())
        })?;
        let mut command = Command::new(runner);
        command.arg("run").arg(binary);
        command
    } else {
        Command::new(binary)
    };
    command.stdin(Stdio::inherit());
    let outcome = if capture {
        crate::process::run_command_captured(&mut command, timeout)
    } else {
        crate::process::run_command_interactive(&mut command, Some(timeout), true).map(|outcome| {
            match outcome {
                crate::process::ChildWaitOutcome::Timeout => {
                    crate::process::BinaryRunOutcome::Timeout
                }
                crate::process::ChildWaitOutcome::Exited(status) if status.success() => {
                    crate::process::BinaryRunOutcome::Success {
                        stdout: String::new(),
                    }
                }
                crate::process::ChildWaitOutcome::Exited(status) => {
                    crate::process::BinaryRunOutcome::Failed {
                        stdout: String::new(),
                        stderr: String::new(),
                        exit_code: status.code().unwrap_or(1),
                        signal: None,
                    }
                }
            }
        })
    }
    .map_err(CliEvalError::Message)?;
    match outcome {
        crate::process::BinaryRunOutcome::Success { stdout } => Ok(stdout),
        crate::process::BinaryRunOutcome::Failed {
            stdout,
            stderr,
            exit_code,
            signal,
        } => Err(CliEvalError::RuntimeFailure {
            stdout,
            stderr,
            exit_code,
            signal,
        }),
        crate::process::BinaryRunOutcome::Timeout => Err(CliEvalError::Message(format!(
            "line processing timed out after {}",
            crate::process::format_timeout(timeout)
        ))),
    }
}

fn input(args: &crate::args::EvalArgs) -> Result<(String, String, PathBuf), String> {
    let current = std::env::current_dir().map_err(|error| error.to_string())?;
    if let Some(path) = &args.file {
        if path.as_os_str() == "-" {
            return Err(
                "--lines reserves stdin for data; supply the body inline or with -f <file>".into(),
            );
        }
        if !args.expr.is_empty() {
            return Err("supply the --lines body either inline or with -f <file>".into());
        }
        let path = current.join(path);
        let source = std::fs::read_to_string(&path)
            .map_err(|error| format!("cannot read '{}': {error}", path.display()))?;
        Ok((
            source,
            path.display().to_string(),
            path.parent().expect("absolute path has parent").to_owned(),
        ))
    } else if args.expr.is_empty() {
        Err("--lines requires an inline body or -f <file>".into())
    } else {
        Ok((args.expr.join(" "), "<eval-lines>".into(), current))
    }
}
