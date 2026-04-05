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

/// Run the `hew eval` subcommand.
pub fn cmd_eval(args: &crate::args::EvalArgs) {
    let timeout = crate::process::timeout_from_seconds(args.timeout).unwrap_or_else(|e| {
        eprintln!("Error: {e}");
        std::process::exit(1);
    });

    // Check for `-f <file>` flag first.
    if let Some(ref file) = args.file {
        let path = file.display().to_string();
        if let Err(e) = repl::eval_file(&path, timeout) {
            exit_eval_error(e);
        }
        return;
    }

    if args.expr.is_empty() {
        // Interactive REPL.
        if let Err(e) = repl::run_interactive(timeout) {
            eprintln!("Error: {e}");
            std::process::exit(1);
        }
        return;
    }

    // Evaluate inline expression.
    let expr = args.expr.join(" ");
    match repl::eval_one(&expr, timeout) {
        Ok(output) => {
            if !output.is_empty() {
                print!("{output}");
            }
        }
        Err(e) => exit_eval_error(e),
    }
}

fn exit_eval_error(error: repl::CliEvalError) -> ! {
    if let repl::CliEvalError::Message(message) = error {
        eprintln!("Error: {message}");
    }
    std::process::exit(1);
}
