//! The audience-grouped `hew --help` listing.
//!
//! clap renders every subcommand under one flat `Commands:` heading, which put
//! the seventeen package and registry commands beside `hew build`. The command
//! set is grouped here instead: the listing is generated from clap's own
//! metadata - names, visibility, and the `about` line each subcommand already
//! carries - and installed as the literal part of a help template, so a
//! description is written in exactly one place and the help cannot drift from
//! the parser.

use std::collections::BTreeMap;
use std::fmt::Write as _;
use std::io::IsTerminal as _;

use clap::Command;

/// Fallback wrapping width when stdout is not a terminal (piped invocations,
/// CI, and help tests) or when the terminal width cannot be queried. Matches
/// clap's own non-TTY default so captured help output remains deterministic.
const DEFAULT_LISTING_WIDTH: usize = 100;

/// Indent before a command name.
const INDENT: &str = "  ";

/// Minimum gap between the longest command name and the descriptions.
const GAP: usize = 2;

/// Heading commands land under when no group claims them.
///
/// A command is never dropped from the help because someone forgot to file it;
/// it surfaces here instead, and `unfiled_commands_are_empty` fails.
const UNFILED_HEADING: &str = "Other";

/// Audience groups, in the order they are listed, and the commands in each.
///
/// Membership is stated once here rather than on the variants because clap 4.6
/// has no per-subcommand `help_heading`: `Command::help_heading` groups
/// arguments only, and subcommands render under a single heading.
const COMMAND_GROUPS: &[(&str, &[&str])] = &[
    (
        "Build and run",
        &[
            "run", "build", "generate", "debug", "watch", "eval", "init", "new",
        ],
    ),
    (
        "Code quality",
        &["check", "test", "bench", "fmt", "doc", "wire", "machine"],
    ),
    (
        "Packages",
        &[
            "add", "remove", "install", "update", "outdated", "tree", "list", "search", "info",
            "publish", "native",
        ],
    ),
    (
        "Registry account",
        &[
            "login",
            "logout",
            "key",
            "namespace",
            "yank",
            "deprecate",
            "index",
        ],
    ),
    (
        "Tooling",
        &[
            "tool",
            "lsp",
            "observe",
            "symbolize",
            "completions",
            "version",
            "env",
            "node",
        ],
    ),
];

/// The `hew` command with the grouped help installed.
///
/// Parsing goes through this rather than `Cli::command()` so `--help` and the
/// parser can never describe different command sets.
pub(crate) fn hew_command() -> Command {
    hew_command_with_width(listing_width())
}

fn hew_command_with_width(width: usize) -> Command {
    let command = <crate::args::Cli as clap::CommandFactory>::command()
        .bin_name("hew")
        .term_width(width);
    let template = help_template(&command, width);
    command.help_template(template)
}

/// Live wrapping width for the help listing.
///
/// Queries the terminal size when stdout is a TTY (falling back to `COLUMNS`
/// when the terminal driver reports no column count, matching clap's own
/// `dimensions()` precedence) and returns [`DEFAULT_LISTING_WIDTH`] for all
/// non-TTY invocations so piped help output and snapshot tests are
/// deterministic.
fn listing_width() -> usize {
    let stdout_is_terminal = std::io::stdout().is_terminal();
    if !stdout_is_terminal {
        return DEFAULT_LISTING_WIDTH;
    }
    let terminal_width = stdout_terminal_width();
    let columns = std::env::var("COLUMNS").ok();
    resolve_listing_width(true, terminal_width, columns.as_deref())
}

fn resolve_listing_width(
    stdout_is_terminal: bool,
    terminal_width: Option<usize>,
    columns_env: Option<&str>,
) -> usize {
    if !stdout_is_terminal {
        return DEFAULT_LISTING_WIDTH;
    }
    terminal_width
        .filter(|&width| width > 0)
        .or_else(|| columns_env.and_then(parse_positive_width))
        .unwrap_or(DEFAULT_LISTING_WIDTH)
}

fn parse_positive_width(raw: &str) -> Option<usize> {
    raw.trim().parse::<usize>().ok().filter(|&width| width > 0)
}

#[cfg(unix)]
fn stdout_terminal_width() -> Option<usize> {
    let mut winsize = libc::winsize {
        ws_row: 0,
        ws_col: 0,
        ws_xpixel: 0,
        ws_ypixel: 0,
    };
    // SAFETY: `TIOCGWINSZ` writes a `libc::winsize` struct to the valid
    // mutable pointer for `STDOUT_FILENO`; returns 0 on success.
    let rc = unsafe { libc::ioctl(libc::STDOUT_FILENO, libc::TIOCGWINSZ, &raw mut winsize) };
    if rc == 0 && winsize.ws_col > 0 {
        Some(usize::from(winsize.ws_col))
    } else {
        None
    }
}

#[cfg(windows)]
fn stdout_terminal_width() -> Option<usize> {
    use windows_sys::Win32::Foundation::INVALID_HANDLE_VALUE;
    use windows_sys::Win32::System::Console::{
        GetConsoleScreenBufferInfo, GetStdHandle, CONSOLE_SCREEN_BUFFER_INFO, STD_OUTPUT_HANDLE,
    };
    // SAFETY: `GetStdHandle` and `GetConsoleScreenBufferInfo` are standard
    // Win32 console queries; `info` is zero-initialized and valid for writes.
    unsafe {
        let handle = GetStdHandle(STD_OUTPUT_HANDLE);
        if handle.is_null() || handle == INVALID_HANDLE_VALUE {
            return None;
        }
        let mut info: CONSOLE_SCREEN_BUFFER_INFO = std::mem::zeroed();
        if GetConsoleScreenBufferInfo(handle, &raw mut info) == 0 {
            return None;
        }
        let width = i32::from(info.srWindow.Right) - i32::from(info.srWindow.Left) + 1;
        usize::try_from(width).ok().filter(|&width| width > 0)
    }
}

#[cfg(not(any(unix, windows)))]
fn stdout_terminal_width() -> Option<usize> {
    None
}

/// Build the help template: clap's own layout with the flat `{all-args}`
/// command list replaced by the grouped listing as literal text.
///
/// Literal template text is emitted verbatim, so the two-column alignment
/// survives; `{before-help}` would be re-wrapped by clap and lose it.
fn help_template(command: &Command, width: usize) -> String {
    let styles = command.get_styles();
    let header = styles.get_header();

    let mut ungrouped: BTreeMap<&str, &Command> = command
        .get_subcommands()
        .filter(|sub| !sub.is_hide_set())
        .map(|sub| (sub.get_name(), sub))
        .collect();

    let mut groups: Vec<(&str, Vec<&Command>)> = Vec::new();
    for (heading, names) in COMMAND_GROUPS {
        let members: Vec<&Command> = names
            .iter()
            .filter_map(|name| ungrouped.remove(name))
            .collect();
        if !members.is_empty() {
            groups.push((heading, members));
        }
    }
    if !ungrouped.is_empty() {
        groups.push((UNFILED_HEADING, ungrouped.into_values().collect()));
    }

    let name_column = groups
        .iter()
        .flat_map(|(_, members)| members.iter())
        .map(|sub| sub.get_name().chars().count())
        .max()
        .unwrap_or(0)
        + GAP;

    let mut template = String::from("{before-help}{about-with-newline}\n{usage-heading} {usage}\n");
    for (heading, members) in &groups {
        let _ = write!(template, "\n{header}{heading}:{header:#}\n");
        for sub in members {
            push_entry(&mut template, sub, name_column, width, styles);
        }
    }
    let _ = write!(template, "\n{header}Options:{header:#}\n");
    template.push_str("{options}{after-help}");
    template
}

/// Write one `  name    description` row, wrapped and hanging-indented.
fn push_entry(
    out: &mut String,
    sub: &Command,
    name_column: usize,
    width: usize,
    styles: &clap::builder::Styles,
) {
    let literal = styles.get_literal();
    let name = sub.get_name();
    let about = sub.get_about().map(ToString::to_string).unwrap_or_default();

    let indent = INDENT.len() + name_column;
    out.push_str(INDENT);
    let _ = write!(out, "{literal}{name}{literal:#}");
    if about.is_empty() {
        out.push('\n');
        return;
    }
    out.push_str(&" ".repeat(name_column - name.chars().count()));

    for (index, line) in wrap(&about, width.saturating_sub(indent))
        .into_iter()
        .enumerate()
    {
        if index > 0 {
            out.push_str(&" ".repeat(indent));
        }
        out.push_str(&line);
        out.push('\n');
    }
}

/// Greedy word wrap. Descriptions are single-line prose, so word boundaries are
/// the only break points needed.
fn wrap(text: &str, width: usize) -> Vec<String> {
    let mut lines = Vec::new();
    let mut current = String::new();
    for word in text.split_whitespace() {
        if current.is_empty() {
            current.push_str(word);
        } else if current.chars().count() + 1 + word.chars().count() <= width {
            current.push(' ');
            current.push_str(word);
        } else {
            lines.push(std::mem::take(&mut current));
            current.push_str(word);
        }
    }
    if !current.is_empty() {
        lines.push(current);
    }
    lines
}

#[cfg(test)]
mod tests {
    use super::{
        hew_command, hew_command_with_width, listing_width, resolve_listing_width, COMMAND_GROUPS,
        DEFAULT_LISTING_WIDTH, UNFILED_HEADING,
    };

    #[test]
    fn tool_usage_keeps_the_public_command_name() {
        for executable in ["hew", "hew.exe", r"C:\Tools\hew.exe"] {
            for tool in ["lsp", "observe"] {
                let help = hew_command()
                    .try_get_matches_from([executable, tool, "--help"])
                    .unwrap_err();
                assert_eq!(help.kind(), clap::error::ErrorKind::DisplayHelp);
                assert!(
                    help.to_string().contains(&format!("Usage: hew {tool}")),
                    "{executable}: {help}"
                );
            }
        }
    }

    fn rendered_help() -> String {
        hew_command().render_help().to_string()
    }

    fn visible_command_names() -> Vec<String> {
        hew_command()
            .get_subcommands()
            .filter(|sub| !sub.is_hide_set())
            .map(|sub| sub.get_name().to_string())
            .collect()
    }

    /// The generated listing only - clap owns the usage and options sections.
    fn listing_lines(help: &str) -> Vec<&str> {
        let start = help
            .find("\nBuild and run:\n")
            .expect("grouped listing should be present");
        let end = start
            + help[start..]
                .find("\nOptions:\n")
                .expect("options section should follow the listing");
        help[start..end].lines().collect()
    }

    /// The point of the change: a reader scanning `hew --help` sees five
    /// audiences instead of one flat list.
    #[test]
    fn help_lists_commands_under_audience_headings() {
        let help = rendered_help();
        for (heading, _) in COMMAND_GROUPS {
            assert!(
                help.contains(&format!("\n{heading}:\n")),
                "`{heading}:` heading missing from help:\n{help}"
            );
        }
        assert!(
            !help.contains("\nCommands:\n"),
            "the flat command list should be gone:\n{help}"
        );
    }

    /// Fail-closed control for the group table: a command added to the parser
    /// and forgotten here still reaches the help, and this test says so.
    #[test]
    fn every_visible_command_is_filed_under_exactly_one_group() {
        let visible = visible_command_names();
        assert!(!visible.is_empty(), "no visible subcommands to file");

        for name in &visible {
            let groups: Vec<&str> = COMMAND_GROUPS
                .iter()
                .filter(|(_, names)| names.contains(&name.as_str()))
                .map(|(heading, _)| *heading)
                .collect();
            assert_eq!(
                groups.len(),
                1,
                "`{name}` is filed under {groups:?}, expected exactly one group"
            );
        }

        let help = rendered_help();
        assert!(
            !help.contains(&format!("\n{UNFILED_HEADING}:\n")),
            "unfiled commands reached the help:\n{help}"
        );
    }

    /// A long description wraps under its own column rather than running off
    /// the page or breaking the two-column alignment.
    #[test]
    fn help_listing_wraps_within_the_listing_width() {
        let deterministic = hew_command_with_width(DEFAULT_LISTING_WIDTH)
            .render_help()
            .to_string();
        for line in listing_lines(&deterministic) {
            assert!(
                line.chars().count() <= DEFAULT_LISTING_WIDTH,
                "deterministic non-TTY line exceeds {DEFAULT_LISTING_WIDTH} columns: {line:?}"
            );
        }

        // `libtest` captures Rust print macros without redirecting OS stdout,
        // so `hew_command()` honours the live terminal width when tests run in
        // an interactive terminal wider than `DEFAULT_LISTING_WIDTH`.
        let live_width = listing_width();
        let live = rendered_help();
        for line in listing_lines(&live) {
            assert!(
                line.chars().count() <= live_width,
                "line exceeds live listing width ({live_width}): {line:?}"
            );
        }
    }

    #[test]
    fn resolve_listing_width_uses_fallback_when_not_a_terminal() {
        assert_eq!(
            resolve_listing_width(false, Some(160), Some("72")),
            DEFAULT_LISTING_WIDTH
        );
        assert_eq!(
            resolve_listing_width(false, None, Some("60")),
            DEFAULT_LISTING_WIDTH
        );
    }

    #[test]
    fn resolve_listing_width_prefers_terminal_size_then_columns_then_default() {
        assert_eq!(resolve_listing_width(true, Some(132), Some("80")), 132);
        assert_eq!(resolve_listing_width(true, None, Some("72")), 72);
        assert_eq!(resolve_listing_width(true, Some(0), Some(" 88 ")), 88);
        assert_eq!(
            resolve_listing_width(true, None, Some("0")),
            DEFAULT_LISTING_WIDTH
        );
        assert_eq!(
            resolve_listing_width(true, None, Some("not-a-number")),
            DEFAULT_LISTING_WIDTH
        );
        assert_eq!(
            resolve_listing_width(true, None, None),
            DEFAULT_LISTING_WIDTH
        );
    }

    #[test]
    fn hew_command_with_width_wraps_to_requested_width() {
        let narrow = hew_command_with_width(60).render_help().to_string();
        let narrow_lines = listing_lines(&narrow);
        for line in &narrow_lines {
            assert!(
                line.chars().count() <= 60,
                "narrow line exceeds 60 columns: {line:?}"
            );
        }

        let wide = hew_command_with_width(140).render_help().to_string();
        let wide_lines = listing_lines(&wide);
        assert!(
            narrow_lines.len() >= wide_lines.len(),
            "narrow width should wrap at least as many lines as wide width"
        );

        // Very narrow width (smaller than hanging indent) must not panic.
        let tiny = hew_command_with_width(8).render_help().to_string();
        assert!(!listing_lines(&tiny).is_empty());
    }
}
