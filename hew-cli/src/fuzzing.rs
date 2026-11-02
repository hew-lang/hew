//! In-process `hew check` for the cargo-fuzz targets in `hew-cli/fuzz`.

use clap::Parser;

/// What one `hew check` run reported.
#[derive(Debug)]
pub struct Checked {
    /// The exit code `hew check` would return.
    pub exit_code: i32,
    /// Diagnostics the text format printed.
    pub text: String,
    /// The array the JSON format printed (`[]` in text mode).
    pub json: String,
}

/// Run `hew check` with `arguments` (everything after `check`) on the calling
/// thread, capturing its diagnostics instead of printing them.
///
/// # Panics
///
/// Panics when `arguments` do not parse as `hew check` arguments.
#[must_use]
pub fn check(arguments: &[&str]) -> Checked {
    let cli = crate::args::Cli::try_parse_from(["hew", "check"].iter().chain(arguments))
        .expect("fuzz harness passes valid `hew check` arguments");
    let Some(crate::args::Command::Check(args)) = cli.command else {
        unreachable!("`hew check` parses as the check command");
    };
    crate::diagnostic_json::set_output_format(args.format.into());
    crate::diagnostic::start_diagnostic_capture();
    let exit_code = crate::cmd_check_run(&args);
    let text = crate::diagnostic::finish_diagnostic_capture();
    let json = crate::diagnostic_json::take_json_diagnostics();
    crate::diagnostic_json::set_output_format(crate::diagnostic_json::OutputFormat::Text);
    Checked {
        exit_code,
        text,
        json,
    }
}
