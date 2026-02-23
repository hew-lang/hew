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
//! state (prior items and bindings) plus the new input, compiles it to a
//! native binary via `hew build`, and captures its stdout output.

pub mod classify;
pub mod repl;
pub mod session;

pub use repl::{eval_file, eval_one, run_interactive};

/// Run the `hew eval` subcommand.
pub fn cmd_eval(args: &[String]) {
    if args.is_empty() {
        // Interactive REPL.
        if let Err(e) = run_interactive() {
            eprintln!("Error: {e}");
            std::process::exit(1);
        }
        return;
    }

    // Check for `-f <file>` flag.
    if args[0] == "-f" {
        if args.len() < 2 {
            eprintln!("Error: -f requires a file argument");
            std::process::exit(1);
        }
        if let Err(e) = eval_file(&args[1]) {
            eprintln!("Error: {e}");
            std::process::exit(1);
        }
        return;
    }

    // Evaluate inline expression.
    let expr = args.join(" ");
    match eval_one(&expr) {
        Ok(output) => {
            if !output.is_empty() {
                print!("{output}");
            }
        }
        Err(e) => {
            eprintln!("Error: {e}");
            std::process::exit(1);
        }
    }
}
