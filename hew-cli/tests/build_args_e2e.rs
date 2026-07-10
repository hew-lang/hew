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
fn net_parameter_surface_fixture_checks_clean_after_resource_qualification() {
    let source = repo_root().join("hew-types/tests/fixtures/net_parameter_surfaces_typecheck.hew");
    let source_arg = source.to_str().expect("source path should be valid UTF-8");

    let output = Command::new(hew_binary())
        .args(["check", source_arg])
        .current_dir(repo_root())
        .output()
        .expect("run hew check");
    let stderr = String::from_utf8_lossy(&output.stderr);

    // This fixture exercises a wide net parameter surface (tls, smtp,
    // websocket, quic, net.connect_timeout). It is valid Hew — it typechecks,
    // compiles, and runs. Historically `hew check` still rejected it at a deep
    // gate: smtp and websocket each define a distinct `Conn` type with a
    // `close` method, and once both became close resources those two impls
    // lowered to the same unqualified `Conn::close` symbol, tripping the #2400
    // duplicate-unqualified-symbol fail-closed guard at codegen-front. That was
    // a compiler limitation, not a defect in this program.
    //
    // The free->close migration qualifies colliding resource wrappers, so the
    // two `Conn::close` impls now lower to distinct symbols and the fixture
    // checks clean. The resource-wrapper case of #2400 is closed by that
    // qualification; the guard itself still fences other same-name collisions.
    assert!(
        output.status.success(),
        "net parameter surface fixture should now check clean after resource \
         wrappers are qualified\n{}",
        describe_output(&output),
    );
    // Teeth: if the qualification regresses, the #2400 collision comes back and
    // the check fails on the duplicate symbol again — guard against that here so
    // the failure names the actual cause instead of a bare non-zero exit.
    assert!(
        !stderr.contains("duplicate function symbol"),
        "resource wrapper qualification regressed — smtp/websocket `Conn::close` \
         collided at codegen-front again:\n{stderr}",
    );
    assert!(
        !stderr.contains("type errors found"),
        "net parameter surface fixture should pass the frontend typecheck:\n{stderr}",
    );
}
