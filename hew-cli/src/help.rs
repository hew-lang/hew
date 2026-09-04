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

use clap::Command;

/// Width the generated listing wraps to.
///
/// WHY: clap's own default when stdout is not a terminal, which is what the
/// help tests and every piped invocation see. WHEN: replace this with the live
/// terminal width once the listing is rendered at print time rather than baked
/// into the template string. WHAT: a real solution asks clap for `term_w`,
/// which is private to its help renderer today.
const LISTING_WIDTH: usize = 100;

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
        &["run", "build", "debug", "watch", "eval", "init"],
    ),
    (
        "Code quality",
        &["check", "test", "fmt", "doc", "wire", "machine"],
    ),
    (
        "Packages",
        &[
            "add", "remove", "install", "update", "outdated", "tree", "list", "search", "info",
            "publish",
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
        &["tool", "lsp", "observe", "completions", "version", "env"],
    ),
];

/// The `hew` command with the grouped help installed.
///
/// Parsing goes through this rather than `Cli::command()` so `--help` and the
/// parser can never describe different command sets.
pub(crate) fn hew_command() -> Command {
    let command = <crate::args::Cli as clap::CommandFactory>::command();
    let template = help_template(&command);
    command.help_template(template)
}

/// Build the help template: clap's own layout with the flat `{all-args}`
/// command list replaced by the grouped listing as literal text.
///
/// Literal template text is emitted verbatim, so the two-column alignment
/// survives; `{before-help}` would be re-wrapped by clap and lose it.
fn help_template(command: &Command) -> String {
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
            push_entry(&mut template, sub, name_column, styles);
        }
    }
    let _ = write!(template, "\n{header}Options:{header:#}\n");
    template.push_str("{options}{after-help}");
    template
}

/// Write one `  name    description` row, wrapped and hanging-indented.
fn push_entry(out: &mut String, sub: &Command, name_column: usize, styles: &clap::builder::Styles) {
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

    for (index, line) in wrap(&about, LISTING_WIDTH.saturating_sub(indent))
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
    use super::{hew_command, COMMAND_GROUPS, LISTING_WIDTH, UNFILED_HEADING};

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
        let help = rendered_help();
        for line in listing_lines(&help) {
            assert!(
                line.chars().count() <= LISTING_WIDTH,
                "line exceeds the listing width: {line:?}"
            );
        }
    }
}
