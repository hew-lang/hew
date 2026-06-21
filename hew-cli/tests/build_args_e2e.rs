mod support;

use std::process::Command;

use support::{describe_output, hew_binary, repo_root};

/// `--link-lib` must accept values that begin with a dash, such as `-lMagickWand-7.Q16`.
///
/// Before the fix, clap interpreted the leading `-` as an unknown flag and
/// rejected the argument before the file was even opened. The test uses a
/// non-existent input path so it never reaches the linker, but the error must
/// be about the missing file, not about an unexpected argument or unknown flag.
#[test]
fn link_lib_hyphen_value_is_not_rejected_as_unknown_flag() {
    for command in ["run", "debug", "build"] {
        let output = Command::new(hew_binary())
            .args([
                command,
                "--link-lib",
                "-lMagickWand-7.Q16",
                "placeholder.hew",
            ])
            .current_dir(repo_root())
            .output()
            .unwrap();

        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(
            !stderr.contains("unexpected argument")
                && !stderr.contains("unknown flag")
                && !stderr.contains("invalid value")
                && !stderr.contains("unrecognized"),
            "`hew {command} --link-lib -lMagickWand-7.Q16` was rejected by clap as a flag: {stderr}",
        );
    }
}

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
fn net_parameter_surface_fixture_reaches_deep_gate() {
    let source = repo_root().join("hew-types/tests/fixtures/net_parameter_surfaces_typecheck.hew");
    let source_arg = source.to_str().expect("source path should be valid UTF-8");

    let output = Command::new(hew_binary())
        .args(["check", source_arg])
        .current_dir(repo_root())
        .output()
        .expect("run hew check");
    let stderr = String::from_utf8_lossy(&output.stderr);

    assert!(
        !output.status.success(),
        "Stage 2 check should reject {} after frontend typechecking\n{}",
        source.display(),
        describe_output(&output),
    );
    assert!(
        stderr.contains("E_HIR") || stderr.contains("E_NOT_YET_IMPLEMENTED"),
        "fixture should reach the HIR/MIR deep gates, not fail in argument parsing or file loading:\n{stderr}",
    );
    assert!(
        !stderr.contains("type errors found"),
        "net parameter surface fixture should still pass the frontend typecheck before deep gates:\n{stderr}",
    );
}
