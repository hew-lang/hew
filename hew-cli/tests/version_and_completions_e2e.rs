mod support;

use std::process::{Command, Output};

use support::hew_binary;

fn run_hew(args: &[&str]) -> Output {
    Command::new(hew_binary()).args(args).output().unwrap()
}

fn assert_version_shape(stdout: &str) {
    let line = stdout.trim_end();
    let prefix = format!("hew {} (", env!("CARGO_PKG_VERSION"));

    assert!(line.starts_with(&prefix), "stdout: {stdout}");
    assert!(line.ends_with(')'), "stdout: {stdout}");

    let details = &line[prefix.len()..line.len() - 1];
    let mut parts = details.split(", ");
    let profile = parts
        .next()
        .expect("version detail should contain a profile");
    assert!(matches!(profile, "debug" | "release"), "stdout: {stdout}");

    if let Some(git) = parts.next() {
        let git = git.strip_suffix("-dirty").unwrap_or(git);
        assert!(
            !git.is_empty() && git.chars().all(|ch| ch.is_ascii_hexdigit()),
            "stdout: {stdout}"
        );
    }

    assert!(parts.next().is_none(), "stdout: {stdout}");
}

#[test]
fn hew_version_subcommand_exits_zero_and_emits_version_on_stdout() {
    let output = run_hew(&["version"]);

    assert!(
        output.status.success(),
        "hew version failed:
stdout: {}
stderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    assert!(
        output.stderr.is_empty(),
        "unexpected stderr: {}",
        String::from_utf8_lossy(&output.stderr),
    );

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert_version_shape(&stdout);
}

fn assert_completions_output(shell: &str, marker: &str) {
    let output = run_hew(&["completions", shell]);

    assert!(
        output.status.success(),
        "hew completions {shell} failed:
stdout: {}
stderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    assert!(
        output.stderr.is_empty(),
        "unexpected stderr for {shell}: {}",
        String::from_utf8_lossy(&output.stderr),
    );

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("hew"), "stdout for {shell}: {stdout}");
    assert!(stdout.contains(marker), "stdout for {shell}: {stdout}");
}

#[test]
fn completions_bash_exits_zero_and_emits_script_to_stdout() {
    assert_completions_output("bash", "complete -F _hew");
}

#[test]
fn completions_zsh_exits_zero_and_emits_script_to_stdout() {
    assert_completions_output("zsh", "#compdef hew");
}

#[test]
fn completions_fish_exits_zero_and_emits_script_to_stdout() {
    assert_completions_output("fish", "complete -c hew");
}

#[test]
fn completions_powershell_exits_zero_and_emits_script_to_stdout() {
    assert_completions_output(
        "powershell",
        "Register-ArgumentCompleter -Native -CommandName 'hew'",
    );
}
