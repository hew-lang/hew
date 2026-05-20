mod support;

use std::process::Command;

use support::{describe_output, hew_binary, repo_root};

#[test]
fn werror_flag_is_accepted_by_build_style_commands() {
    // --Werror must stay accepted by the build-style commands even when the
    // input itself is otherwise invalid.
    for command in ["check", "run", "debug"] {
        let output = Command::new(hew_binary())
            .args([command, "--Werror", "placeholder.hew"])
            .current_dir(repo_root())
            .output()
            .unwrap();

        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(
            !stderr.contains("Unknown option: --Werror") && !stderr.contains("unexpected argument"),
            "{command} rejected --Werror flag: {stderr}",
        );
    }
}

#[test]
fn build_subcommand_is_rejected_as_unrecognised() {
    let output = Command::new(hew_binary())
        .args(["build", "placeholder.hew"])
        .current_dir(repo_root())
        .output()
        .expect("run hew build");

    assert!(
        !output.status.success(),
        "hew build subcommand must be rejected as unrecognised\n{}",
        describe_output(&output),
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("unrecognized subcommand 'build'")
            || stderr.contains("unrecognised subcommand 'build'"),
        "expected clap to reject build; got:\n{stderr}"
    );
}

#[test]
fn net_parameter_surface_fixture_checks_cleanly() {
    let source = repo_root().join("hew-types/tests/fixtures/net_parameter_surfaces_typecheck.hew");
    let source_arg = source.to_str().expect("source path should be valid UTF-8");

    let output = Command::new(hew_binary())
        .args(["check", source_arg])
        .current_dir(repo_root())
        .output()
        .expect("run hew check");

    assert!(
        output.status.success(),
        "hew check {} failed\n{}",
        source.display(),
        describe_output(&output),
    );
}
