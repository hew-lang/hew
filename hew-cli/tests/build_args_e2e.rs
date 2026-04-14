mod support;

use std::process::Command;

use support::{describe_output, hew_binary, repo_root, require_codegen};

#[test]
fn werror_flag_is_accepted_as_noop_by_build_style_commands() {
    // --Werror is accepted for spec compatibility but is a no-op.
    // It should not cause an "unknown option" error.
    for command in ["build", "check", "run", "debug"] {
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
fn directory_module_demo_can_be_checked_built_and_run() {
    require_codegen();

    let source = repo_root().join("examples/directory_module_demo/main.hew");
    let workspace = tempfile::Builder::new()
        .prefix("directory-module-demo-")
        .tempdir_in(repo_root())
        .expect("create smoke workspace in repo root");
    let output_path = workspace.path().join(format!(
        "directory_module_demo{}",
        std::env::consts::EXE_SUFFIX
    ));
    let source_arg = source.to_str().expect("source path should be valid UTF-8");
    let output_arg = output_path
        .to_str()
        .expect("output path should be valid UTF-8");

    let check_output = Command::new(hew_binary())
        .args(["check", source_arg])
        .current_dir(repo_root())
        .output()
        .expect("run hew check");
    assert!(
        check_output.status.success(),
        "hew check {} failed\n{}",
        source.display(),
        describe_output(&check_output),
    );

    let build_output = Command::new(hew_binary())
        .args(["build", source_arg, "-o", output_arg])
        .current_dir(repo_root())
        .output()
        .expect("run hew build");
    assert!(
        build_output.status.success(),
        "hew build {} failed\n{}",
        source.display(),
        describe_output(&build_output),
    );
    assert!(
        output_path.exists(),
        "hew build did not create {}",
        output_path.display(),
    );

    let built_binary_output = Command::new(&output_path)
        .current_dir(repo_root())
        .output()
        .expect("run built demo binary");
    assert!(
        built_binary_output.status.success(),
        "built demo binary failed\n{}",
        describe_output(&built_binary_output),
    );

    let run_output = Command::new(hew_binary())
        .args(["run", source_arg])
        .current_dir(repo_root())
        .output()
        .expect("run hew run");
    assert!(
        run_output.status.success(),
        "hew run {} failed\n{}",
        source.display(),
        describe_output(&run_output),
    );

    for (label, output) in [
        ("built demo binary", &built_binary_output),
        ("hew run", &run_output),
    ] {
        assert_eq!(
            String::from_utf8_lossy(&output.stdout).replace("\r\n", "\n"),
            "Hello from a merged directory module!\n",
            "{label} produced unexpected stdout\n{}",
            describe_output(output),
        );
    }
}
